#pragma once

#include <map>
#include <string>
#include "common/algebra.hpp"
#include "common/algebraTrace.hpp"
#include "common/eigenIncluder.hpp"

using std::map;
using std::string;

struct ReceiverMap;

/** Configuration interface for RTS processing to support dependency injection */
struct RtsConfiguration
{
    // PPP options
    bool       queue_rts_outputs = false;
    E_Inverter rts_inverter      = E_Inverter::LDLT;

    // RTS specific options
    bool rts_only = false;

    // Output options
    bool output_residuals = false;
    bool retain_rts_files = true;

    // MongoDB options
    bool output_measurements = false;
    bool queue_mongo_outputs = false;

    // Sleep timing
    int sleep_milliseconds = 10;

    /** Create configuration from global acsConfig */
    static RtsConfiguration fromAcsConfig();
};

/** Structure to encapsulate all filter data objects read from binary files */
struct FilterData
{
    KFState                kalmanMinus;
    KFState                kalmanPlus;
    KFState                smoothedKF;
    KFMeas                 measurements;
    MatrixXd               transitionMatrix;
    TransitionMatrixObject transitionMatrixObject;
    map<string, string>    metaDataMap;

    // State tracking flags
    bool smoothedXready = false;
    bool smoothedPready = false;
    bool skipNextRts    = false;

    /** Reset transition matrix to identity */
    void resetTransitionMatrix(int size);

    /** Update transition matrix by multiplying with new transition */
    void updateTransitionMatrix(const MatrixXd& newTransition);

    /** Initialize smoothed filter from kalman plus state */
    void initializeSmoothedFilter(const KFState& kfState);

    /** Perform RTS smoothing computation for current epoch */
    bool performRtsComputation(KFState& kfState, const RtsConfiguration& config);
};

/** Reader class for RTS binary file data following SOLID principles */
class RtsFileReader
{
   private:
    string   inputFile;
    long int currentPosition = -1;

    // Data presence flags for current epoch
    bool hasMetadata         = false;
    bool hasMeasurements     = false;
    bool hasTransitionMatrix = false;
    bool hasFilterMinus      = false;
    bool hasFilterPlus       = false;

   public:
    /** Constructor */
    explicit RtsFileReader(const string& filename);

    /** Read the next object from file and update FilterData */
    E_SerialObject readNextObject(FilterData& filterData);

    /** Check if we have all required data for RTS processing */
    bool isReadyForProcessing() const;

    /** Reset data presence flags for new epoch */
    void resetEpochFlags();

    /** Get current file position */
    long int getCurrentPosition() const { return currentPosition; }

    /** Check if we've reached the beginning of file */
    bool isAtBeginning() const { return currentPosition == 0; }

   private:
    /** Process metadata object */
    bool processMetadata(FilterData& filterData);

    /** Process measurement object */
    bool processMeasurement(FilterData& filterData);

    /** Process transition matrix object */
    bool processTransitionMatrix(FilterData& filterData);

    /** Process filter minus object */
    bool processFilterMinus(FilterData& filterData);

    /** Process filter plus object */
    bool processFilterPlus(FilterData& filterData);
};

/** Processor class for RTS computation and output following SOLID principles */
class RtsProcessor
{
   private:
    string                  outputFile;
    bool                    writeOutput;
    const RtsConfiguration& config;

   public:
    /** Constructor with dependency injection */
    RtsProcessor(const string& outputFilename, bool write, const RtsConfiguration& configuration);

    /** Initialize first epoch smoothed filter */
    bool initializeFirstEpoch(FilterData& filterData, KFState& kfState);

    /** Process complete epoch data (computation + output) */
    bool processEpoch(FilterData& filterData, KFState& kfState, GTime& epochStartTime, double& lag);

    /** Write metadata to output file */
    void writeMetadata(FilterData& filterData);

   private:
    /** Perform RTS computation and output for current epoch */
    bool performRtsComputationAndOutput(
        FilterData& filterData,
        KFState&    kfState,
        GTime&      epochStartTime
        /* lag removed: computed where needed */
    );

    /** Handle output operations */
    void handleOutput(FilterData& filterData);

    /** Handle timing and logging */
    void handleTimingAndLogging(
        const FilterData& filterData,
        GTime&            epochStartTime,
        GTime             epochStopTime
    );
};

/** Timing and logging handler for RTS processing */
class RtsTimingLogger
{
   public:
    /** Log epoch processing timing and update progress */
    static void
    logEpochTiming(const FilterData& filterData, GTime& epochStartTime, GTime epochStopTime);

    /** Format time for logging output */
    static boost::posix_time::ptime formatTimeForLogging(const GTime& time);

    /** Update interactive terminal with progress information */
    static void
    updateTerminalProgress(const FilterData& filterData, GTime epochStartTime, GTime epochStopTime);

    /** Calculate fractional milliseconds from time */
    static int calculateFractionalMilliseconds(const GTime& time);
};

/** Output reader class for reading smoothed results from backward file */
class RtsOutputFileReader
{
   private:
    string   reversedStatesFilename;
    long int currentPosition = -1;

   public:
    /** Constructor */
    explicit RtsOutputFileReader(const string& filename);

    /** Read the next object from reversed file */
    E_SerialObject readNextObject(FilterData& filterData);

    /** Check if we've reached the beginning of file */
    bool isAtBeginning() const { return currentPosition == 0; }

    /** Check if position is invalid */
    bool isInvalidPosition() const { return currentPosition < 0; }

   private:
    /** Process metadata object for output */
    bool processMetadataForOutput(FilterData& filterData);

    /** Process measurement object for output */
    bool processMeasurementForOutput(FilterData& filterData);

    /** Process smoothed filter object for output */
    bool processSmoothedFilterForOutput(FilterData& filterData);
};

/** Output processor class for handling RTS output operations */
class RtsOutputProcessor
{
   private:
    bool                    firstEpoch = true;
    const RtsConfiguration& config;

   public:
    /** Constructor with dependency injection */
    explicit RtsOutputProcessor(const RtsConfiguration& configuration);

    /** Process measurement output */
    void processMeasurementOutput(FilterData& filterData);

    /** Process smoothed filter output */
    void processSmoothedFilterOutput(FilterData& filterData, ReceiverMap& receiverMap);

    /** Reset first epoch flag */
    void resetFirstEpoch() { firstEpoch = true; }

   private:
    /** Output residuals to file */
    void outputResidualsToFile(FilterData& filterData);

    /** Output measurements to MongoDB */
    void outputMeasurementsToMongo(FilterData& filterData);

    /** Perform epoch post-processing and outputs */
    void performEpochPostProcessing(FilterData& filterData, ReceiverMap& receiverMap);
};

void rtsSmoothing(
    KFState&                kfState,
    ReceiverMap&            receiverMap,
    bool                    write  = false,
    const RtsConfiguration* config = nullptr
);

void rtsOutput(
    KFState&                kfState,
    ReceiverMap&            receiverMap,
    const RtsConfiguration* config = nullptr
);
