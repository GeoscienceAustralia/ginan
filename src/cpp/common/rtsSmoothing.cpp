#include "common/rtsSmoothing.hpp"
#include <boost/log/trivial.hpp>
#include <map>
#include <memory>
#include <thread>
#include "architectureDocs.hpp"
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/algebraTrace.hpp"
#include "common/constants.hpp"
#include "common/eigenIncluder.hpp"
#include "common/interactiveTerminal.hpp"
#include "common/metaData.hpp"
#include "common/mongoWrite.hpp"
#include "common/navigation.hpp"
#include "common/receiver.hpp"
#include "pea/inputsOutputs.hpp"

using std::make_shared;
using std::map;
using std::shared_ptr;
using std::this_thread::sleep_for;

// #pragma GCC optimize ("O0")

//================================================================================
// RtsConfiguration Implementation
//================================================================================

/** Create configuration from global acsConfig */
RtsConfiguration RtsConfiguration::fromAcsConfig()
{
    RtsConfiguration config;
    config.queue_rts_outputs   = acsConfig.pppOpts.queue_rts_outputs;
    config.rts_inverter        = acsConfig.pppOpts.rts_inverter;
    config.rts_only            = acsConfig.rts_only;
    config.output_residuals    = acsConfig.output_residuals;
    config.retain_rts_files    = acsConfig.retain_rts_files;
    config.output_measurements = acsConfig.mongoOpts.output_measurements;
    config.queue_mongo_outputs = acsConfig.mongoOpts.queue_outputs;
    config.sleep_milliseconds  = acsConfig.sleep_milliseconds;
    return config;
}

//================================================================================
// RtsFileReader Implementation
//================================================================================

/** Constructor */
RtsFileReader::RtsFileReader(const string& filename) : inputFile(filename)
{
    checkValidFile(inputFile, "RTS file (rts_forward)");
    resetEpochFlags();
}

/** Read the next object from file and update FilterData */
E_SerialObject RtsFileReader::readNextObject(FilterData& filterData)
{
    E_SerialObject type = getFilterTypeFromFile(currentPosition, inputFile);

    BOOST_LOG_TRIVIAL(debug) << "Found " << type._to_string() << "\n";

    if (type == +E_SerialObject::NONE)
    {
        return type;
    }

    bool success = false;

    switch (type)
    {
        case E_SerialObject::METADATA:
            success = processMetadata(filterData);
            break;

        case E_SerialObject::MEASUREMENT:
            success = processMeasurement(filterData);
            break;

        case E_SerialObject::TRANSITION_MATRIX:
            success = processTransitionMatrix(filterData);
            break;

        case E_SerialObject::FILTER_MINUS:
            success = processFilterMinus(filterData);
            break;

        case E_SerialObject::FILTER_PLUS:
            success = processFilterPlus(filterData);
            break;

        default:
            BOOST_LOG_TRIVIAL(error) << "Unknown rts type" << "\n";
            return E_SerialObject::NONE;
    }

    if (!success)
    {
        return E_SerialObject::NONE;
    }

    return type;
}

/** Check if we have all required data for RTS processing */
bool RtsFileReader::isReadyForProcessing() const
{
    // Processing is ready when we have FILTER_PLUS (which is the trigger)
    // and the FilterData has been properly initialized
    return hasFilterPlus;
}

/** Reset data presence flags for new epoch */
void RtsFileReader::resetEpochFlags()
{
    hasMetadata         = false;
    hasMeasurements     = false;
    hasTransitionMatrix = false;
    hasFilterMinus      = false;
    hasFilterPlus       = false;
}

/** Process metadata object */
bool RtsFileReader::processMetadata(FilterData& filterData)
{
    filterData.skipNextRts = (filterData.smoothedKF.metaDataMap["SKIP_PREV_RTS"] == "TRUE");

    bool success = getFilterObjectFromFile(
        E_SerialObject::METADATA,
        filterData.metaDataMap,
        currentPosition,
        inputFile
    );
    if (success)
    {
        hasMetadata = true;
    }
    else
    {
        BOOST_LOG_TRIVIAL(debug) << "Failed to read metadata" << "\n";
    }

    return success;
}

/** Process measurement object */
bool RtsFileReader::processMeasurement(FilterData& filterData)
{
    bool success = getFilterObjectFromFile(
        E_SerialObject::MEASUREMENT,
        filterData.measurements,
        currentPosition,
        inputFile
    );
    if (success)
    {
        hasMeasurements = true;
    }

    return success;
}

/** Process transition matrix object */
bool RtsFileReader::processTransitionMatrix(FilterData& filterData)
{
    bool success = getFilterObjectFromFile(
        E_SerialObject::TRANSITION_MATRIX,
        filterData.transitionMatrixObject,
        currentPosition,
        inputFile
    );
    if (success)
    {
        MatrixXd transition = filterData.transitionMatrixObject.asMatrix();
        filterData.updateTransitionMatrix(transition);
        hasTransitionMatrix = true;
    }

    return success;
}

/** Process filter minus object */
bool RtsFileReader::processFilterMinus(FilterData& filterData)
{
    bool success = getFilterObjectFromFile(
        E_SerialObject::FILTER_MINUS,
        filterData.kalmanMinus,
        currentPosition,
        inputFile
    );
    if (success)
    {
        if (filterData.smoothedXready == false)
        {
            filterData.smoothedXready = true;
        }

        // assume a trivial transition matrix unless overridden
        filterData.resetTransitionMatrix(filterData.kalmanMinus.x.rows());
        hasFilterMinus = true;
    }

    return success;
}

/** Process filter plus object */
bool RtsFileReader::processFilterPlus(FilterData& filterData)
{
    bool success = getFilterObjectFromFile(
        E_SerialObject::FILTER_PLUS,
        filterData.kalmanPlus,
        currentPosition,
        inputFile
    );
    if (success)
    {
        hasFilterPlus = true;
    }

    return success;
}

//================================================================================
// FilterData Implementation
//================================================================================

/** Reset transition matrix to identity */
void FilterData::resetTransitionMatrix(int size)
{
    transitionMatrix = MatrixXd::Identity(size, size);
}

/** Update transition matrix by multiplying with new transition */
void FilterData::updateTransitionMatrix(const MatrixXd& newTransition)
{
    if (transitionMatrix.rows() == 0)
        transitionMatrix = newTransition;
    else
        transitionMatrix = (transitionMatrix * newTransition).eval();
}

/** Initialize smoothed filter from kalman plus state */
void FilterData::initializeSmoothedFilter(const KFState& kfState)
{
    smoothedKF     = kfState;
    smoothedPready = true;
}

//================================================================================
// RtsProcessor Implementation
//================================================================================

/** Constructor with dependency injection */
RtsProcessor::RtsProcessor(
    const string&           outputFilename,
    bool                    write,
    const RtsConfiguration& configuration
)
    : outputFile(outputFilename), writeOutput(write), config(configuration)
{
}

/** Initialize first epoch smoothed filter */
bool RtsProcessor::initializeFirstEpoch(FilterData& filterData, KFState& kfState)
{
    filterData.kalmanPlus.metaDataMap = kfState.metaDataMap;
    filterData.initializeSmoothedFilter(filterData.kalmanPlus);

    if (writeOutput)
    {
        spitFilterToFile(
            filterData.smoothedKF,
            E_SerialObject::FILTER_SMOOTHED,
            outputFile,
            config.queue_rts_outputs
        );
        spitFilterToFile(
            filterData.measurements,
            E_SerialObject::MEASUREMENT,
            outputFile,
            config.queue_rts_outputs
        );
    }

    return true;
}

/** Perform RTS computation and output for current epoch */
bool RtsProcessor::performRtsComputationAndOutput(
    FilterData& filterData,
    KFState&    kfState,
    GTime&      epochStartTime
)
{
    // Perform the mathematical RTS computation
    bool rtsSuccess = filterData.performRtsComputation(kfState, config);
    if (!rtsSuccess)
    {
        return false;
    }

    // Handle output operations
    handleOutput(filterData);

    // Handle timing and logging
    GTime epochStopTime = timeGet();
    handleTimingAndLogging(filterData, epochStartTime, epochStopTime);

    return true;
}

/** Process complete epoch data (computation + output) */
bool RtsProcessor::processEpoch(
    FilterData& filterData,
    KFState&    kfState,
    GTime&      epochStartTime,
    double&     lag
)
{
    if (filterData.smoothedPready == false)
    {
        return initializeFirstEpoch(filterData, kfState);
    }

    // Update lag calculation
    lag = (kfState.time - filterData.kalmanPlus.time).to_double();

    return performRtsComputationAndOutput(filterData, kfState, epochStartTime);
}

/** Write metadata to output file */
void RtsProcessor::writeMetadata(FilterData& filterData)
{
    if (writeOutput)
    {
        spitFilterToFile(
            filterData.metaDataMap,
            E_SerialObject::METADATA,
            outputFile,
            config.queue_rts_outputs
        );
    }
}

/** Handle output operations */
void RtsProcessor::handleOutput(FilterData& filterData)
{
    if (writeOutput)
    {
        InteractiveTerminal::setMode(E_InteractiveMode::Outputs);

        spitFilterToFile(
            filterData.smoothedKF,
            E_SerialObject::FILTER_SMOOTHED,
            outputFile,
            config.queue_rts_outputs
        );
        spitFilterToFile(
            filterData.measurements,
            E_SerialObject::MEASUREMENT,
            outputFile,
            config.queue_rts_outputs
        );

        if (filterData.skipNextRts)
        {
            filterData.skipNextRts                               = false;
            filterData.smoothedKF.metaDataMap["SKIP_RTS_OUTPUT"] = "TRUE";
            spitFilterToFile(
                filterData.smoothedKF.metaDataMap,
                E_SerialObject::METADATA,
                outputFile,
                config.queue_rts_outputs
            );
        }
    }
    else
    {
        // Handle non-write case if needed
        if (filterData.smoothedKF.metaDataMap.find(TRACE_FILENAME_STR + SMOOTHED_SUFFIX) !=
            filterData.smoothedKF.metaDataMap.end())
        {
            GTime         dummyTime;
            Network       dummyNet;
            std::ofstream trace(
                filterData.smoothedKF.metaDataMap.at(TRACE_FILENAME_STR + SMOOTHED_SUFFIX),
                std::ofstream::out | std::ofstream::app
            );
        }
    }
}

/** Handle timing and logging */
void RtsProcessor::handleTimingAndLogging(
    const FilterData& filterData,
    GTime&            epochStartTime,
    GTime             epochStopTime
)
{
    RtsTimingLogger::logEpochTiming(filterData, epochStartTime, epochStopTime);
}

//================================================================================
// RtsTimingLogger Implementation
//================================================================================

/** Log epoch processing timing and update progress */
void RtsTimingLogger::logEpochTiming(
    const FilterData& filterData,
    GTime&            epochStartTime,
    GTime             epochStopTime
)
{
    auto boostTime = formatTimeForLogging(filterData.kalmanPlus.time);

    BOOST_LOG_TRIVIAL(info) << "Processed epoch" << " - " << boostTime << " (took "
                            << (epochStopTime - epochStartTime) << ")";

    updateTerminalProgress(filterData, epochStartTime, epochStopTime);

    epochStartTime = timeGet();
}

/** Format time for logging output */
boost::posix_time::ptime RtsTimingLogger::formatTimeForLogging(const GTime& time)
{
    int fractionalMilliseconds = calculateFractionalMilliseconds(time);

    return boost::posix_time::from_time_t((time_t)((PTime)time).bigTime) +
           boost::posix_time::millisec(fractionalMilliseconds);
}

/** Update interactive terminal with progress information */
void RtsTimingLogger::updateTerminalProgress(
    const FilterData& filterData,
    GTime             epochStartTime,
    GTime             epochStopTime
)
{
    InteractiveTerminal::clearModes(
        (string) " Processing epoch " + filterData.kalmanPlus.time.to_string(),
        (string) " Last Epoch took " +
            std::to_string((epochStopTime - epochStartTime).to_double()) + "s"
    );
    InteractiveTerminal::setMode(E_InteractiveMode::Syncing);
}

/** Calculate fractional milliseconds from time */
int RtsTimingLogger::calculateFractionalMilliseconds(const GTime& time)
{
    return (time.bigTime - (long int)time.bigTime) * 1000;
}

//================================================================================
// FilterData Mathematical Computation
//================================================================================

/** Perform RTS smoothing computation for current epoch */
bool FilterData::performRtsComputation(KFState& kfState, const RtsConfiguration& config)
{
    if (smoothedXready == false)
    {
        return false;
    }

    if (config.rts_only && kfState.time == GTime::noTime())
    {
        kfState.time = kalmanPlus.time;
    }

    double lag = (kfState.time - kalmanPlus.time).to_double();

    BOOST_LOG_TRIVIAL(info) << "RTS lag: " << lag;

    InteractiveTerminal::setMode(E_InteractiveMode::Filtering);

    smoothedKF.time = kalmanPlus.time;

    smoothedKF.P = (smoothedKF.P + smoothedKF.P.transpose()).eval() / 2;

    // get process noise and dynamics
    auto& F = transitionMatrix;

    if (F.rows() == 0 && F.cols() == 0)
    {
        // assume identity state transition if none was performed/required
        F = MatrixXd::Identity(kalmanPlus.P.rows(), kalmanPlus.P.rows());
    }

    MatrixXd FP = F * kalmanPlus.P;

    E_Inverter inverter = config.rts_inverter;

    auto failInversion = [&]()
    {
        auto oldInverter = inverter;
        inverter         = E_Inverter::_from_integral(((int)inverter) + 1);

        BOOST_LOG_TRIVIAL(warning) << "Inverter type " << oldInverter._to_string()
                                   << " failed, trying " << inverter._to_string();
    };
    VectorXd deltaX = VectorXd::Zero(kalmanPlus.x.rows());
    MatrixXd deltaP = MatrixXd::Zero(kalmanPlus.P.rows(), kalmanPlus.P.cols());

    map<string, bool> filterChunks;
    for (auto& [id, fcP] : kalmanPlus.filterChunkMap)
        filterChunks[id] = true;
    for (auto& [id, fcM] : kalmanMinus.filterChunkMap)
        filterChunks[id] = true;

    for (auto& [id, dummy] : filterChunks)
    {
        auto& fcP = kalmanPlus.filterChunkMap[id];
        auto& fcM = kalmanMinus.filterChunkMap[id];

        if (fcP.numX == 0 || fcM.numX == 0)
        {
            BOOST_LOG_TRIVIAL(debug) << "Ignoring  chunk " << id;
            continue;
        }

        BOOST_LOG_TRIVIAL(debug) << "Filtering chunk " << id;
        auto Q = kalmanMinus.P.block(fcM.begX, fcM.begX, fcM.numX, fcM.numX)
                     .triangularView<Eigen::Upper>()
                     .transpose();
        auto FP_ = FP.block(fcM.begX, fcP.begX, fcM.numX, fcP.numX);

        MatrixXd Ck;

        int pass = false;

        auto solve = [&]<typename SOLVER>(SOLVER solver) -> bool
        {
            solver.compute(Q);
            if (solver.info())
            {
                pass = false;
                return pass;
            }

            Ck = solver.solve(FP_).transpose();

            if (solver.info())
            {
                pass = false;
                return pass;
            }

            pass = true;
            return pass;
        };

        while (inverter != +E_Inverter::FIRST_UNSUPPORTED && (pass == false))
            switch (inverter)
            {
                default:
                {
                    BOOST_LOG_TRIVIAL(warning)
                        << "Inverter type " << config.rts_inverter._to_string()
                        << " not supported, reverting to LDLT";

                    // Note: We cannot modify the config here as it's const
                    // Fall through to LDLT case
                    inverter = E_Inverter::LDLT;

                    continue;
                }
                case E_Inverter::FULLPIVLU:
                case E_Inverter::INV:
                {
                    Eigen::FullPivLU<MatrixXd> solver(
                        kalmanMinus.P.block(fcM.begX, fcM.begX, fcM.numX, fcM.numX)
                    );

                    if (solver.isInvertible() == false)
                    {
                        failInversion();

                        break;
                    }

                    MatrixXd Pinv = solver.inverse();

                    Pinv = (Pinv + Pinv.transpose()).eval() / 2;

                    Ck = (FP_.transpose() * Pinv);

                    pass = true;

                    break;
                }
                case E_Inverter::LLT:
                {
                    solve(Eigen::LLT<MatrixXd>());
                    if (pass == false)
                        failInversion();
                    break;
                }
                case E_Inverter::LDLT:
                {
                    solve(Eigen::LDLT<MatrixXd>());
                    if (pass == false)
                        failInversion();
                    break;
                }
                case E_Inverter::COLPIVHQR:
                {
                    solve(Eigen::ColPivHouseholderQR<MatrixXd>());
                    if (pass == false)
                        failInversion();
                    break;
                }
                case E_Inverter::BDCSVD:
                {
                    solve(Eigen::BDCSVD<MatrixXd>());
                    if (pass == false)
                        failInversion();
                    break;
                }
                case E_Inverter::JACOBISVD:
                {
                    solve(Eigen::JacobiSVD<MatrixXd>());
                    if (pass == false)
                        failInversion();
                    break;
                }
            }

        if (pass == false)
        {
            BOOST_LOG_TRIVIAL(warning) << "RTS failed to find solution to invert system of "
                                          "equations, smoothed values may be bad";

            BOOST_LOG_TRIVIAL(debug) << "P-det: " << kalmanMinus.P.determinant();

            kalmanMinus.outputConditionNumber(std::cout);

            BOOST_LOG_TRIVIAL(debug) << "P:\n" << kalmanMinus.P.format(heavyFmt);
            kalmanMinus.outputCorrelations(std::cout);
            std::cout << "\n";

            return false;  // Signal failure to break out of the loop
        }

        auto deltaX_   = deltaX.segment(fcP.begX, fcP.numX);
        auto smoothedX = smoothedKF.x.segment(fcM.begX, fcM.numX);
        auto xMinus    = kalmanMinus.x.segment(fcM.begX, fcM.numX);
        auto deltaP_   = deltaP.block(fcP.begX, fcP.begX, fcP.numX, fcP.numX);
        auto smoothedP = smoothedKF.P.block(fcM.begX, fcM.begX, fcM.numX, fcM.numX);
        auto minuxP    = kalmanMinus.P.block(fcM.begX, fcM.begX, fcM.numX, fcM.numX);

        deltaX_ = Ck * (smoothedX - xMinus);
        deltaP_ = Ck * (smoothedP - minuxP) * Ck.transpose();
    }

    smoothedKF.dx = deltaX;
    smoothedKF.x  = deltaX + kalmanPlus.x;
    smoothedKF.P  = deltaP + kalmanPlus.P;

    if (measurements.H.rows())
        if (measurements.H.cols() == deltaX.rows())
        {
            measurements.VV -= measurements.H * deltaX;
        }
        else
        {
            BOOST_LOG_TRIVIAL(error) << "RTScrewy" << "\n";
        }

    smoothedKF.kfIndexMap = kalmanPlus.kfIndexMap;

    return true;
}

//================================================================================
// RtsOutputFileReader Implementation
//================================================================================

/** Constructor */
RtsOutputFileReader::RtsOutputFileReader(const string& filename) : reversedStatesFilename(filename)
{
}

/** Read the next object from reversed file */
E_SerialObject RtsOutputFileReader::readNextObject(FilterData& filterData)
{
    E_SerialObject type = getFilterTypeFromFile(currentPosition, reversedStatesFilename);

    BOOST_LOG_TRIVIAL(debug) << "Outputting " << type._to_string() << " from file position "
                             << currentPosition << "\n";

    if (type == +E_SerialObject::NONE)
    {
        return type;
    }

    bool success = false;

    switch (type)
    {
        case E_SerialObject::METADATA:
            success = processMetadataForOutput(filterData);
            break;

        case E_SerialObject::MEASUREMENT:
            success = processMeasurementForOutput(filterData);
            break;

        case E_SerialObject::FILTER_SMOOTHED:
            success = processSmoothedFilterForOutput(filterData);
            break;

        default:
            BOOST_LOG_TRIVIAL(error) << "UNEXPECTED RTS OUTPUT TYPE";
            return E_SerialObject::NONE;
    }

    if (!success)
    {
        return E_SerialObject::NONE;
    }

    return type;
}

/** Process metadata object for output */
bool RtsOutputFileReader::processMetadataForOutput(FilterData& filterData)
{
    bool success = getFilterObjectFromFile(
        E_SerialObject::METADATA,
        filterData.metaDataMap,
        currentPosition,
        reversedStatesFilename
    );
    if (!success)
    {
        BOOST_LOG_TRIVIAL(error) << "BAD RTS OUTPUT read";
    }
    return success;
}

/** Process measurement object for output */
bool RtsOutputFileReader::processMeasurementForOutput(FilterData& filterData)
{
    bool success = getFilterObjectFromFile(
        E_SerialObject::MEASUREMENT,
        filterData.measurements,
        currentPosition,
        reversedStatesFilename
    );
    if (!success)
    {
        BOOST_LOG_TRIVIAL(error) << "BAD RTS OUTPUT read";
    }
    return success;
}

/** Process smoothed filter object for output */
bool RtsOutputFileReader::processSmoothedFilterForOutput(FilterData& filterData)
{
    bool success = getFilterObjectFromFile(
        E_SerialObject::FILTER_SMOOTHED,
        filterData.smoothedKF,
        currentPosition,
        reversedStatesFilename
    );
    if (!success)
    {
        BOOST_LOG_TRIVIAL(error) << "BAD RTS OUTPUT READ";
    }
    else
    {
        filterData.smoothedKF.metaDataMap = filterData.metaDataMap;
    }
    return success;
}

//================================================================================
// RtsOutputProcessor Implementation
//================================================================================

/** Constructor with dependency injection */
RtsOutputProcessor::RtsOutputProcessor(const RtsConfiguration& configuration)
    : config(configuration)
{
}

/** Process measurement output */
void RtsOutputProcessor::processMeasurementOutput(FilterData& filterData)
{
    outputResidualsToFile(filterData);
    outputMeasurementsToMongo(filterData);
}

/** Process smoothed filter output */
void RtsOutputProcessor::processSmoothedFilterOutput(
    FilterData&  filterData,
    ReceiverMap& receiverMap
)
{
    performEpochPostProcessing(filterData, receiverMap);
    firstEpoch = false;
}

/** Output residuals to file */
void RtsOutputProcessor::outputResidualsToFile(FilterData& filterData)
{
    if (filterData.metaDataMap.find(TRACE_FILENAME_STR + SMOOTHED_SUFFIX) ==
        filterData.metaDataMap.end())
    {
        return;
    }

    string        filename = filterData.metaDataMap.at(TRACE_FILENAME_STR + SMOOTHED_SUFFIX);
    std::ofstream ofs(filename, std::ofstream::out | std::ofstream::app);

    if (ofs && config.output_residuals)
    {
        outputResiduals(ofs, filterData.measurements, "/RTS");
    }
}

/** Output measurements to MongoDB */
void RtsOutputProcessor::outputMeasurementsToMongo(FilterData& filterData)
{
    if (config.output_measurements)
    {
        mongoMeasResiduals(
            filterData.measurements.time,
            filterData.measurements,
            config.queue_mongo_outputs,
            "/PPP_RTS"
        );
    }
}

/** Perform epoch post-processing and outputs */
void RtsOutputProcessor::performEpochPostProcessing(
    FilterData&  filterData,
    ReceiverMap& receiverMap
)
{
    if (filterData.smoothedKF.metaDataMap.find(TRACE_FILENAME_STR + SMOOTHED_SUFFIX) ==
        filterData.smoothedKF.metaDataMap.end())
    {
        return;
    }

    GTime         dummyTime;
    Network       dummyNet;
    std::ofstream trace(
        filterData.smoothedKF.metaDataMap.at(TRACE_FILENAME_STR + SMOOTHED_SUFFIX),
        std::ofstream::out | std::ofstream::app
    );

    perEpochPostProcessingAndOutputs(
        trace,
        dummyTime,
        dummyNet,
        receiverMap,
        filterData.smoothedKF,
        false,
        true,
        firstEpoch
    );
}

/** Rauch-Tung-Striebel Smoothing.
 * Combine estimations using filtered data from before and after each epoch.
 * Complete filter vectors and matrices are stored in a binary file that is able to be read in
 * reverse.
 */
Architecture RTS_Smoothing__()
{
    DOCS_REFERENCE(Binary_Archive__);
}

/** Output filter states in chronological order from a reversed binary trace file
 */
void rtsOutput(
    KFState&                kfState,      ///< State to get filter traces from
    ReceiverMap&            receiverMap,  ///< map of receivers
    const RtsConfiguration* config        ///< Configuration for dependency injection
)
{
    InteractiveTerminal::setMode(E_InteractiveMode::Outputs);

    string reversedStatesFilename = kfState.rts_basename + BACKWARD_SUFFIX;

    BOOST_LOG_TRIVIAL(info) << "Outputting RTS products..." << "\n";

    // Use provided config or create default from acsConfig
    RtsConfiguration        defaultConfig = RtsConfiguration::fromAcsConfig();
    const RtsConfiguration& rtsConfig     = config ? *config : defaultConfig;

    // Initialize the output reader and processor using SOLID principles
    RtsOutputFileReader outputReader(reversedStatesFilename);
    RtsOutputProcessor  outputProcessor(rtsConfig);
    FilterData          filterData;
    filterData.metaDataMap = kfState.metaDataMap;

    while (true)
    {
        E_SerialObject type = outputReader.readNextObject(filterData);

        if (type == +E_SerialObject::NONE)
        {
            break;
        }

        // Process different object types using the processor
        switch (type)
        {
            case E_SerialObject::METADATA:
                // Metadata is read but no additional processing needed
                break;

            case E_SerialObject::MEASUREMENT:
                outputProcessor.processMeasurementOutput(filterData);
                break;

            case E_SerialObject::FILTER_SMOOTHED:
                outputProcessor.processSmoothedFilterOutput(filterData, receiverMap);
                break;

            default:
                // Error handling is already done in the reader
                break;
        }

        if (outputReader.isAtBeginning())
        {
            return;
        }

        if (outputReader.isInvalidPosition())
        {
            BOOST_LOG_TRIVIAL(error) << "Invalid file position reached during RTS output" << "\n";
            return;
        }
    }
}

/** Iterate over stored filter states in reverse and perform filtering.
 * Saves filtered states to a secondary binary file, which is in reverse-chronological order due to
 * the save sequence. Most serial objects that are processed are merely stored or accumulated as
 * prerequisites for the FILTER_PLUS object, which contains the state of the filter immediately
 * after the update step. At that stage, the previously smoothed (next chronologically) filter state
 * is combined with the next filter minus state (immediately before the next chronological update
 * step), any state transitions, and the filter plus state, using the standard rts algorithm. The
 * filtered state and a measurements object which has updated residuals are then stored in a binary
 * file. If intermediate outputs are enabled (rare) it performs some outputs using each filter
 * state, but typically outputs all states chronologically after the reverse running rts procedure
 * has reached the first epoch and all data is available for output in the correct sequence.
 */
void rtsSmoothing(
    KFState&                kfState,
    ReceiverMap&            receiverMap,
    bool                    write,
    const RtsConfiguration* config
)
{
    DOCS_REFERENCE(RTS_Smoothing__);

    if (kfState.rts_lag == 0)
    {
        return;
    }

    BOOST_LOG_TRIVIAL(info) << "\n"
                            << "---------------PROCESSING WITH RTS--------------------- " << "\n";

    for (auto& [id, rec] : receiverMap)
        rec.obsList.clear();

    for (auto& [dummy, satNav] : nav.satNavMap)
        satNav.attStatus = {};

    // Use provided config or create default from acsConfig
    RtsConfiguration        defaultConfig = RtsConfiguration::fromAcsConfig();
    const RtsConfiguration& rtsConfig     = config ? *config : defaultConfig;

    // Initialize the reader and processor using SOLID principles
    string inputFile  = kfState.rts_basename + FORWARD_SUFFIX;
    string outputFile = kfState.rts_basename + BACKWARD_SUFFIX;

    RtsFileReader reader(inputFile);
    RtsProcessor  processor(outputFile, write, rtsConfig);
    FilterData    filterData;

    if (write)
    {
        std::ofstream ofs(outputFile, std::ofstream::out | std::ofstream::trunc);
    }

    double lag            = 0;
    GTime  epochStartTime = timeGet();

    while (lag != kfState.rts_lag)
    {
        E_SerialObject type = reader.readNextObject(filterData);

        if (type == +E_SerialObject::NONE)
        {
            break;
        }

        // Handle metadata writing
        if (type == +E_SerialObject::METADATA)
        {
            processor.writeMetadata(filterData);
        }

        // Check if we're ready for RTS processing (FILTER_PLUS received)
        if (reader.isReadyForProcessing())
        {
            bool success = processor.processEpoch(filterData, kfState, epochStartTime, lag);
            if (!success)
            {
                // RTS computation failed, break out of the loop
                lag = kfState.rts_lag;
                break;
            }

            // Reset flags for next epoch
            reader.resetEpochFlags();
        }

        if (reader.isAtBeginning())
        {
            break;
        }
    }

    if (write)
    {
        while (spitQueueRunning)
        {
            sleep_for(std::chrono::milliseconds(rtsConfig.sleep_milliseconds));
        }

        rtsOutput(kfState, receiverMap, &rtsConfig);
    }

    if (lag == kfState.rts_lag)
    {
        // delete the beginning of the history file
        string tempFile = kfState.rts_basename + FORWARD_SUFFIX + "_temp";
        {
            std::ofstream tempStream(
                tempFile,
                std::ifstream::binary | std::ofstream::out | std::ofstream::trunc
            );
            std::fstream inputStream(inputFile, std::ifstream::binary | std::ifstream::in);

            inputStream.seekg(0, inputStream.end);
            long int lengthPos  = inputStream.tellg();
            long int currentPos = reader.getCurrentPosition();

            vector<char> fileContents(lengthPos - currentPos);

            inputStream.seekg(currentPos, inputStream.beg);

            inputStream.read(&fileContents[0], lengthPos - currentPos);
            tempStream.write(&fileContents[0], lengthPos - currentPos);
        }

        std::remove(inputFile.c_str());
        std::rename(tempFile.c_str(), inputFile.c_str());
    }

    if (kfState.rts_lag <= 0 && rtsConfig.retain_rts_files == false)
    {
        BOOST_LOG_TRIVIAL(info) << "Removing RTS file: " << inputFile;

        std::remove(inputFile.c_str());

        BOOST_LOG_TRIVIAL(info) << "Removing RTS file: " << outputFile;

        std::remove(outputFile.c_str());
    }
}
