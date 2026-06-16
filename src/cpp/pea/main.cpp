// #pragma GCC optimize ("O0")

#include <algorithm>
#include <boost/chrono/chrono_io.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/utility/setup/console.hpp>
#include <boost/system/error_code.hpp>
#include <chrono>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <set>
#include <signal.h>
#include <sstream>
#include <string>
#include <thread>
#include <vector>
#include "architectureDocs.hpp"
#include "common/algebraTrace.hpp"
#include "common/api.hpp"
#include "common/debug.hpp"
#include "common/fileLog.hpp"
#include "common/gTime.hpp"
#include "common/mongoRead.hpp"
#include "common/mongoWrite.hpp"
#include "common/navigation.hpp"
#include "common/ntripBroadcast.hpp"
#include "common/observations.hpp"
#include "common/receiver.hpp"
#include "common/rinexClkWrite.hpp"
#include "common/rinexNavWrite.hpp"
#include "common/rinexObsWrite.hpp"
#include "common/rtsSmoothing.hpp"
#include "common/sinex.hpp"
#include "common/streamNtrip.hpp"
#include "common/streamObs.hpp"
#include "common/summary.hpp"
#include "common/tcpSocket.hpp"
#include "common/testUtils.hpp"
#include "inertial/posProp.hpp"
#include "iono/ionoModel.hpp"
#if defined(ENABLE_PARALLELISATION) || defined(_OPENMP)
#include "omp.h"
#endif
#include "orbprop/coordinates.hpp"
#include "orbprop/orbitProp.hpp"
#include "pea/inputsOutputs.hpp"
#include "pea/minimumConstraints.hpp"
#include "pea/peaCommitStrings.hpp"
#include "pea/preprocessor.hpp"
#include "slr/slr.hpp"

using namespace std::literals::chrono_literals;
using std::make_shared;
using std::make_unique;
using std::string;
using std::chrono::system_clock;
using std::chrono::time_point;
using std::this_thread::sleep_for;

/** Retrieve data from streams and process together once per epoch.
 *
 * In order to be capable of real-time and post-processed execution, the pea treats all observation
 * inputs as streams. These streams are created with a stream source type, and a parser, with the
 * separation of these strategies allowing for far more reusable code, and immediate extension of
 * inputs from simply files, to HTTP, TCP, Serial, Pipes etc.
 *
 * The fundamental tick of the pea is the `epoch_interval`, which drives all major processing steps.
 * Once an initial epoch has been designated the pea requests data for the next epoch from each
 * stream. The streams either respond with the data, or with a flag indicating the data may be
 * available later, or will never be available.
 *
 * Various configuration parameters define the exact flow through the synchronisation code, however
 * once all data is available, or no more is expected, or a timeout has occurred, the epoch is
 * considered synchronised and processing is performed.
 *
 * Unlike observation streams, which provide data up until the requested epoch time, other streams
 * such as navigation streams parse all data available to them, and output any values to global
 * maps, which may be later accessed as required by other components.
 *
 * The synchronisation process parses all streams sequentially, without multithreading, so map
 * collisions are not expected.
 */
Architecture Streams_And_Synchronisation__() {}

#ifdef ENABLE_PARALLELISATION
#endif

bool                  doDocs = false;
Navigation            nav    = {};
int                   epoch  = 0;
GTime                 tsync  = GTime::noTime();
map<int, SatIdentity> satIdMap;
map<string, string>   latestMissingObsStatusByReceiver;

struct ConfiguredStreamState
{
    std::set<string> configuredSources;
    std::set<string> configuredReceivers;
};

std::vector<StreamParserPtr> retiredStreamParsers;

string streamDisplayName(const string& sourceString)
{
    URL url(sourceString);

    if (url.host.empty() == false && url.path.empty() == false)
    {
        if (url.path.front() == '/')
        {
            return url.path.substr(1);
        }

        return url.path;
    }

    return sourceString;
}

void retireStreamParser(StreamParserPtr streamParser_ptr)
{
    if (streamParser_ptr == nullptr)
    {
        return;
    }

    try
    {
        if (auto* tcpSocket = dynamic_cast<TcpSocket*>(&streamParser_ptr->stream))
        {
            tcpSocket->shutdown();
        }
    }
    catch (...)
    {
    }

    retiredStreamParsers.push_back(std::move(streamParser_ptr));
}

ConfiguredStreamState getConfiguredStreamState()
{
    ConfiguredStreamState state;

    auto addSources = [&](const vector<string>& inputNames)
    { state.configuredSources.insert(inputNames.begin(), inputNames.end()); };

    auto addReceiverSources = [&](const auto& inputMap)
    {
        for (const auto& [id, inputNames] : inputMap)
        {
            state.configuredReceivers.insert(id);
            addSources(inputNames);
        }
    };

    for (const auto& [id, slrInputs] : slrObsFiles)
    {
        state.configuredReceivers.insert(id);
        addSources(slrInputs);
    }

    addReceiverSources(acsConfig.ubx_inputs);
    addReceiverSources(acsConfig.sbf_inputs);
    addReceiverSources(acsConfig.custom_inputs);
    addReceiverSources(acsConfig.rnx_inputs);
    addReceiverSources(acsConfig.obs_rtcm_inputs);
    addReceiverSources(acsConfig.pseudo_sp3_inputs);
    addReceiverSources(acsConfig.pseudo_snx_inputs);

    addSources(acsConfig.nav_rtcm_inputs);
    addSources(acsConfig.qzs_rtcm_inputs);
    addSources(acsConfig.sisnet_inputs);

    return state;
}

void avoidCollisions(ReceiverMap& receiverMap)
{
    for (auto& [id, rec] : receiverMap)
    {
        auto trace = getTraceFile(rec);
    }

    for (auto& [id, rec] : receiverMap)
    {
        // create sinex estimate maps
        theSinex.estimatesMap[id];
        theSinex.solEpochMap[id];

        auto& recOpts = acsConfig.getRecOpts(id);

        if (recOpts.sat_id.empty() == false)
        {
            auto& satNav = nav.satNavMap[recOpts.sat_id.c_str()];
        }
    }
}

/** Perform operations for each station
 * This function occurs in parallel with other stations - ensure that any operations on global maps
 * do not create new entries, as that will destroy the map for other processes. Variables within the
 * rec object are ok to use, but be aware that pointers from the within the receiver often point to
 * global variables. Prepare global maps by accessing the desired elements before calling this
 * function.
 */
void mainOncePerEpochPerStation(Receiver& rec, Network& net, bool& emptyEpoch, KFState& remoteState)
{
    if (rec.isPseudoRec)
    {
        return;
    }

    auto trace = getTraceFile(rec);

    if (rec.ready == false)
    {
        string missingStatus;
        if (auto it = latestMissingObsStatusByReceiver.find(rec.id);
            it != latestMissingObsStatusByReceiver.end())
        {
            missingStatus = it->second;
        }

        trace << "\n"
              << "Receiver " << rec.id << " has no data for this epoch";
        if (missingStatus.empty() == false)
        {
            trace << " (" << missingStatus << ")";
        }

        BOOST_LOG_TRIVIAL(info) << "Receiver " << rec.id << " has no data for this epoch"
                                << (missingStatus.empty() ? "" : " (" + missingStatus + ")");
        return;
    }

    sinexPerEpochPerStation(trace, tsync, rec);

    preprocessor(trace, rec, true, &net.kfState, &remoteState);

    if (acsConfig.process_preprocessor == false)
    {
        // Update observation variances if not calculated in preprocessor
        obsVariances(rec.obsList);
    }

    if (acsConfig.process_spp)
    {
        spp(trace, rec, &net.kfState, &remoteState);
    }

    if (rec.ready == false || rec.invalid)
    {
        return;
    }

    auto missingWarnInvalidate = [&](string thing, bool failureBool, bool requiredConfig) -> bool
    {
        if (failureBool == false)
        {
            return false;
        }

        if (requiredConfig)
        {
            trace << "\n"
                  << "Warning: Receiver " << rec.id << " rejected due to lack of " << thing;
            BOOST_LOG_TRIVIAL(warning)
                << "Receiver " << rec.id << " rejected due to lack of " << thing;

            rec.invalid = true;

            return true;
        }

        BOOST_LOG_TRIVIAL(warning) << thing << " not found for " << rec.id;

        return false;
    };

    bool sppUsed;
    selectAprioriSource(trace, rec, tsync, sppUsed, net.kfState, &remoteState);

    if (sppUsed)
    {
        string message =
            "fixed receiver apriori position not found for " + rec.id
            + "; using SPP fallback. Check receiver_options." + rec.id
            + ".apriori_position, receiver_options." + rec.id
            + ".models.pos.sources, and loaded SINEX station coordinates.";

        if (acsConfig.require_apriori_positions)
        {
            trace << "\n"
                  << "Warning: Receiver " << rec.id << " rejected because " << message;
            BOOST_LOG_TRIVIAL(warning) << "Receiver " << rec.id << " rejected because " << message;

            rec.invalid = true;

            return;
        }

        BOOST_LOG_TRIVIAL(warning) << message;
    }

    if (missingWarnInvalidate(
            "Receiver metadata apriori position",
            rec.failureAprioriPos,
            acsConfig.require_apriori_positions
        ))
        return;
    if (missingWarnInvalidate(
            "Antenna details",
            rec.antennaId.empty(),
            acsConfig.require_antenna_details
        ))
        return;
    if (missingWarnInvalidate(
            "Site eccentricity",
            rec.failureEccentricity,
            acsConfig.require_site_eccentricity
        ))
        return;
    if (missingWarnInvalidate("Sinex information", rec.failureSinex, acsConfig.require_sinex_data))
        return;

    emptyEpoch = false;

    BOOST_LOG_TRIVIAL(trace) << "Read " << rec.obsList.size() << " observations for station "
                             << rec.id;

    // calculate statistics
    if (rec.obsList.empty() == false)
    {
        if ((GTime)rec.firstEpoch == GTime::noTime())
        {
            rec.firstEpoch = rec.obsList.front()->time;
        }
        rec.lastEpoch = rec.obsList.front()->time;
        rec.epochCount++;
        rec.obsCount += rec.obsList.size();

        for (auto& obs : only<GObs>(rec.obsList))
            for (auto& [ft, sigList] : obs.sigsLists)
                for (auto& sig : sigList)
                {
                    rec.codeCount[sig.code]++;
                }

        for (auto& obs : only<GObs>(rec.obsList))
        {
            rec.satCount[obs.Sat]++;
        }
    }

    if (acsConfig.process_ionosphere)
    {
        obsIonoData(trace, rec);
    }

    auto& recOpts = acsConfig.getRecOpts(rec.id);

    rec.antBoresight = recOpts.antenna_boresight;
    rec.antAzimuth   = recOpts.antenna_azimuth;

    recAtt(rec, tsync, recOpts.attitudeModel.sources);
    testEclipse(rec.obsList);

    if (acsConfig.output_rinex_obs)
    {
        writeRinexObs(rec.id, rec, tsync, rec.obsList, acsConfig.rinex_obs_version);
    }
}

void mainOncePerEpochPerSatellite(
    Trace&   trace,
    GTime    time,
    SatSys   Sat,
    KFState& kfState,
    KFState& remoteKF
)
{
    auto& satNav  = nav.satNavMap[Sat];
    auto& satOpts = acsConfig.getSatOpts(Sat);

    if (satOpts.exclude)
    {
        return;
    }

    // get svn and block type if possible
    if (Sat.svn().empty())
    {
        auto it = nav.svnMap[Sat].lower_bound(time);
        if (it == nav.svnMap[Sat].end())
        {
            BOOST_LOG_TRIVIAL(warning) << "SVN not found for " << Sat.id();

            Sat.setSvn("UNKNOWN");
        }
        else
        {
            Sat.setSvn(it->second);
        }

        // reinitialise the options with the updated values
        satOpts._initialised = false;
    }

    if (Sat.blockType().empty())
    {
        auto it = nav.blocktypeMap.find(Sat.svn());
        if (it == nav.blocktypeMap.end())
        {
            BOOST_LOG_TRIVIAL(warning)
                << "Block type not found for " << Sat.id()
                << ", attitude modelling etc may be affected, check sinex file";

            Sat.setBlockType("UNKNOWN");
        }
        else
        {
            Sat.setBlockType(it->second);
        }

        // reinitialise the options with the updated values
        satOpts._initialised =
            false;  // todo? this is insufficient since the opts are inherited from the other
                    // initialised ones per file which are not reset
    }

    satOpts = acsConfig.getSatOpts(Sat);

    satNav.antBoresight = satOpts.antenna_boresight;
    satNav.antAzimuth   = satOpts.antenna_azimuth;

    selectAprioriSource(Sat, time, kfState, &remoteKF);
}

void cullData(GTime time)
{
    cullOldEphs(time);
    cullOldSSRs(time);
    cullOldBiases(time);

    mongoCull(time);
}

void mainOncePerEpoch(Network& pppNet, Network& ionNet, ReceiverMap& receiverMap, GTime time)
{
    avoidCollisions(receiverMap);

    // reload any new or modified files
    reloadInputFiles();

    addDefaultBias();

    createTracefiles(receiverMap, pppNet, ionNet);

    auto pppTrace = getTraceFile(pppNet);

    // initialise mongo if not already done
    mongoooo();

    // integrate accelerations and early prepare satPos0 with propagated values
    predictOrbits(pppTrace, pppNet.kfState, time);
    // predictInertials(pppTrace, pppNet.kfState, time);

    BOOST_LOG_TRIVIAL(info) << " ------- PREPROCESSING STATIONS       --------" << "\n";

    KFState remoteState;
    if (acsConfig.mongoOpts.use_predictions != E_Mongo::NONE)
    {
        mongoReadFilter(remoteState, time, acsConfig.mongoOpts.used_predictions);

        BOOST_LOG_TRIVIAL(info) << "Remote state was updated at " << remoteState.time.to_string();

        remoteState.outputStates(pppTrace, "/REMOTE");

        mongoStates(
            pppNet.kfState,
            {.suffix    = "/REMOTE",
             .instances = acsConfig.mongoOpts.output_states,
             .queue     = acsConfig.mongoOpts.queue_outputs}
        );

        pppNet.kfState.alternate_ptr = &remoteState;

        nav.erp.filterValues = getErpFromFilter(pppNet.kfState);
    }

    const int DT = 1;
    for (int dt : {DT, 0})
    {
        ERPValues erpv = getErp(nav.erp, time + dt);

        FrameSwapper frameSwapper(time + dt, erpv);

        frameSwapper.setCache(dt);
    }

    loadSBASdata(pppTrace, time, nav);

    // try to get svns & block types of all used satellites
    for (auto& [Sat, satNav] : nav.satNavMap)
    {
        if (acsConfig.process_sys[Sat.sys] == false)
            continue;

        mainOncePerEpochPerSatellite(pppTrace, time, Sat, pppNet.kfState, remoteState);
    }

    // do per-station pre processing
    bool emptyEpoch = true;
#ifdef ENABLE_PARALLELISATION
    Eigen::setNbThreads(1);
#pragma omp parallel for
#endif
    for (int i = 0; i < receiverMap.size(); i++)
    {
        auto rec_ptr_iterator = receiverMap.begin();
        std::advance(rec_ptr_iterator, i);

        auto& [id, rec] = *rec_ptr_iterator;
        mainOncePerEpochPerStation(rec, pppNet, emptyEpoch, remoteState);
    }
    Eigen::setNbThreads(0);

    if (emptyEpoch)
    {
        BOOST_LOG_TRIVIAL(warning) << "Epoch " << epoch << " has no observations";
    }

    if (acsConfig.process_ppp)
    {
        ppp(pppTrace, receiverMap, pppNet.kfState, remoteState);

        for (auto& [Sat, satNav] : nav.satNavMap)
        {
            // prevent the memoised orbital positions being used in per epoch post processing and
            // outputs
            satNav.satPos0.posTime = GTime::noTime();
        }
    }

    perEpochPostProcessingAndOutputs(
        pppTrace,
        time,
        ionNet,
        receiverMap,
        pppNet.kfState,
        emptyEpoch
    );

    if (acsConfig.delete_old_ephemerides)
    {
        cullData(time);
    }

    if (acsConfig.check_plumbing)
    {
        plumber();
    }

    callbacksOncePerEpoch();
}

/** Perform any post-final epoch calculations and outputs, then begin reverse smoothing and tidy up
 */
void mainPostProcessing(Network& pppNet, Network& ionNet, ReceiverMap& receiverMap)
{
    BOOST_LOG_TRIVIAL(info) << "Post processing... ";

    auto pppTrace = getTraceFile(pppNet);

    if (acsConfig.process_ppp && acsConfig.ambrOpts.mode != E_ARmode::OFF &&
        acsConfig.ambrOpts.once_per_epoch == false && acsConfig.ambrOpts.fix_and_hold)
    {
        BOOST_LOG_TRIVIAL(info) << "\n"
                                << "---------------PERFORMING AMBIGUITY RESOLUTION ON NETWORK WITH "
                                   "FIX AND HOLD ------------- "
                                << "\n";

        fixAndHoldAmbiguities(pppTrace, pppNet.kfState);

        mongoStates(
            pppNet.kfState,
            {.suffix    = "/AR",
             .instances = acsConfig.mongoOpts.output_states,
             .queue     = acsConfig.mongoOpts.queue_outputs}
        );
    }

    if (acsConfig.process_ppp && acsConfig.process_minimum_constraints &&
        acsConfig.minconOpts.once_per_epoch == false)
    {
        BOOST_LOG_TRIVIAL(info) << " ------- PERFORMING MIN-CONSTRAINTS   --------" << "\n";

        for (auto& [id, rec] : receiverMap)
        {
            rec.minconApriori = rec.aprioriPos;
        }

        {
            ERPValues erpv = getErp(nav.erp, pppNet.kfState.time);

            FrameSwapper frameSwapper(pppNet.kfState.time, erpv);

            for (auto& [Sat, satNav] : nav.satNavMap)
            {
                SatPos satPos;

                satPos.Sat        = Sat;
                satPos.satNav_ptr = &satNav;

                bool pass = satpos(
                    nullStream,
                    pppNet.kfState.time,
                    pppNet.kfState.time,
                    satPos,
                    {E_Source::PRECISE, E_Source::BROADCAST},
                    E_OffsetType::COM,
                    nav
                );
                if (pass == false)
                {
                    BOOST_LOG_TRIVIAL(warning) << "No sat pos found for " << satPos.Sat.id() << ".";
                    continue;
                }

                satNav.aprioriPos = frameSwapper(satPos.rSatCom);
            }
        }

        MinconStatistics minconStatistics;

        mincon(pppTrace, pppNet.kfState, &minconStatistics);

        pppNet.kfState.outputStates(pppTrace, "/CONSTRAINED");

        mongoStates(
            pppNet.kfState,
            {.suffix    = "/CONSTRAINED",
             .instances = acsConfig.mongoOpts.output_states,
             .queue     = acsConfig.mongoOpts.queue_outputs}
        );

        outputMinconStatistics(pppTrace, minconStatistics);
    }

    if (acsConfig.output_sinex)
    {
        sinexPostProcessing(tsync, receiverMap, pppNet.kfState);
    }

    outputPredictedStates(pppTrace, pppNet.kfState);

    if (acsConfig.process_rts)
    {
        while (spitQueueRunning)
        {
            sleep_for(std::chrono::milliseconds(acsConfig.sleep_milliseconds));
        }

        for (auto& [Sat, satNav] : nav.satNavMap)
        {
            // prevent the memoised orbital positions being used in rts
            satNav.satPos0.posTime = GTime::noTime();
        }

        if (acsConfig.process_ppp)
        {
            rtsSmoothing(pppNet.kfState, receiverMap, true);
        }

        if (acsConfig.process_ionosphere)
        {
            rtsSmoothing(ionNet.kfState, receiverMap, true);
        }
    }

    outputSummaries(pppTrace, receiverMap);

    outputStatistics(pppTrace, pppNet.kfState.statisticsMapSum, pppNet.kfState.statisticsMapSum);
}

void tryExitGracefully(int signum)
{
    static bool closeRequest = false;

    if (closeRequest)
    {
        // second time this is called, dont worry too much about gracefullness
        abort();
    }

    BOOST_LOG_TRIVIAL(info) << "SIGINT detected - tidying up and exiting...";

    closeRequest = true;

    acsConfig.max_epochs = 1;
}

int main(int argc, char** argv)
{
    traceLevel = 5;

    // Register the sink in the logging core
    boost::log::core::get()->add_sink(boost::make_shared<sinks::synchronous_sink<ConsoleLog>>());
    boost::log::core::get()->set_filter(boost::log::trivial::severity >= boost::log::trivial::info);
    acsSeverity = boost::log::trivial::info;

    BOOST_LOG_TRIVIAL(info) << "PEA starting... (" << ginanBranchName() << " "
                            << ginanCommitVersion() << " from " << ginanCommitDate() << ")" << "\n";

    GTime peaStartTime       = timeGet();
    auto  peaStartTimeChrono = system_clock::now();

    exitOnErrors();

    // Define the function to be called when ctrl-c (SIGINT) is sent to process
    signal(SIGINT, tryExitGracefully);

    bool pass = configure(argc, argv);
    if (pass == false)
    {
        if (acsConfig.dry_run)
        {
            BOOST_LOG_TRIVIAL(error) << "Dry-run failed: configuration is invalid";
        }
        else
        {
            BOOST_LOG_TRIVIAL(error) << "Incorrect configuration";
        }

        BOOST_LOG_TRIVIAL(info) << "PEA finished";
        TcpSocket::ioContext.stop();
        return EXIT_FAILURE;
    }

    if (acsConfig.dry_run)
    {
        BOOST_LOG_TRIVIAL(info) << "Dry-run successful: configuration and sanity checks passed";
        BOOST_LOG_TRIVIAL(info) << "PEA finished";
        TcpSocket::ioContext.stop();
        return EXIT_SUCCESS;
    }

    if (acsConfig.output_log)
    {
        addFileLog(acsConfig.log_json);
    }

    BOOST_LOG_TRIVIAL(info) << "Compilation details:";
    BOOST_LOG_TRIVIAL(info) << "====================";
    BOOST_LOG_TRIVIAL(info) << "Ginan branch:     " << ginanBranchName();
    BOOST_LOG_TRIVIAL(info) << "Ginan version:    " << ginanCommitVersion();
    BOOST_LOG_TRIVIAL(info) << "Commit date:      " << ginanCommitDate();
    BOOST_LOG_TRIVIAL(info) << "Operating system: " << ginanOsName();
    BOOST_LOG_TRIVIAL(info) << "Compiler version: " << ginanCompilerVersion();
    BOOST_LOG_TRIVIAL(info) << "Boost version:    " << ginanBoostVersion();
    BOOST_LOG_TRIVIAL(info) << "Eigen version:    " << ginanEigenVersion();
    BOOST_LOG_TRIVIAL(info) << "Mongocxx version: " << ginanMongoVersion();
    BOOST_LOG_TRIVIAL(info) << "\n";

    BOOST_LOG_TRIVIAL(info) << "Runtime details:";
    BOOST_LOG_TRIVIAL(info) << "================";
    BOOST_LOG_TRIVIAL(info) << "Logging at trace level" << traceLevel;
    BOOST_LOG_TRIVIAL(info) << "Threading with max " << Eigen::nbThreads() << " eigen threads";
#ifdef ENABLE_PARALLELISATION
    BOOST_LOG_TRIVIAL(info) << "Threading with max " << omp_get_max_threads() << " omp threads";
#endif
    BOOST_LOG_TRIVIAL(info) << "\n";
    BOOST_LOG_TRIVIAL(info) << "\n";

    // prepare the satNavMap so that it at least has entries for everything
    for (auto [sys, max] :
         {tuple<E_Sys, int>{E_Sys::GPS, NSATGPS},
          tuple<E_Sys, int>{E_Sys::GLO, NSATGLO},
          tuple<E_Sys, int>{E_Sys::GAL, NSATGAL},
          tuple<E_Sys, int>{E_Sys::QZS, NSATQZS},
          tuple<E_Sys, int>{E_Sys::LEO, NSATLEO},
          tuple<E_Sys, int>{E_Sys::BDS, NSATBDS},
          tuple<E_Sys, int>{E_Sys::SBS, NSATSBS}})
        for (int prn = 1; prn <= max; prn++)
        {
            SatSys Sat(sys, prn);

            if (acsConfig.process_sys[Sat.sys] == false)
                continue;

            auto& satOpts = acsConfig.getSatOpts(Sat);

            if (satOpts.exclude)
                continue;

            nav.satNavMap[Sat].id = Sat.id();
        }

    nav.leaps = acsConfig.leap_seconds;

    Network pppNet;
    {
        pppNet.kfState.FilterOptions::operator=(acsConfig.pppOpts);
        pppNet.kfState.id               = "Net";
        pppNet.kfState.output_residuals = acsConfig.output_residuals;
        pppNet.kfState.outputMongoMeasurements =
            (acsConfig.mongoOpts.output_measurements != E_Mongo::NONE);

        pppNet.kfState.measRejectCallbacks.push_back(incrementPhaseSignalError);
        pppNet.kfState.measRejectCallbacks.push_back(incrementSatelliteErrors);
        pppNet.kfState.measRejectCallbacks.push_back(incrementReceiverErrors);
        pppNet.kfState.measRejectCallbacks.push_back(pseudoMeasTest);
        pppNet.kfState.measRejectCallbacks.push_back(deweightMeas);

        pppNet.kfState.stateRejectCallbacks.push_back(incrementStateErrors);
        pppNet.kfState.stateRejectCallbacks.push_back(
            rejectWorstMeasByState
        );  // Assume the state error is caused by a single measurement error and try removing it
        // first
        pppNet.kfState.stateRejectCallbacks.push_back(relaxState);
        pppNet.kfState.stateRejectCallbacks.push_back(rejectAllMeasByState);
    }

    Network ionNet;
    if (acsConfig.process_ionosphere)
    {
        ionNet.kfState.FilterOptions::operator=(acsConfig.ionModelOpts);
        ionNet.kfState.id               = "ION";
        ionNet.kfState.output_residuals = acsConfig.output_residuals;
        ionNet.kfState.outputMongoMeasurements =
            (acsConfig.mongoOpts.output_measurements != E_Mongo::NONE);
        ionNet.kfState.rts_basename = "IONEX_RTS";

        ionNet.kfState.measRejectCallbacks.push_back(deweightMeas);

        ionNet.kfState.stateRejectCallbacks.push_back(incrementStateErrors);
        ionNet.kfState.stateRejectCallbacks.push_back(
            rejectWorstMeasByState
        );  // Assume the state error is caused by a single measurement error and try removing it
        // first
        ionNet.kfState.stateRejectCallbacks.push_back(relaxState);
        ionNet.kfState.stateRejectCallbacks.push_back(rejectAllMeasByState);
    }

    // initialise mongo
    mongoooo();

    if (acsConfig.rts_only)
    {
        replaceString(acsConfig.pppOpts.rts_filename, "<RECEIVER>", pppNet.id);

        pppNet.kfState.rts_basename = acsConfig.pppOpts.rts_filename;

        rtsSmoothing(pppNet.kfState, receiverMap);

        exit(0);
    }

    initialiseBias();

    boost::posix_time::ptime logptime = currentLogptime();
    createDirectories(logptime);

    reloadInputFiles();

    auto hasRealtimeObservationInput = []()
    {
        for (auto& [id, streamParser_ptr] : streamParserMultimap)
        {
            auto* obsStream = dynamic_cast<ObsStream*>(streamParser_ptr.get());
            if (obsStream != nullptr && dynamic_cast<TcpSocket*>(&obsStream->stream) != nullptr)
            {
                return true;
            }
        }

        return false;
    };

    bool hasRealtimeObsInput = hasRealtimeObservationInput();

    addDefaultBias();

    TcpSocket::startClients();

    if (acsConfig.start_epoch.is_not_a_date_time() == false)
    {
        GTime startGTime = GTime(acsConfig.start_epoch);
        tsync            = startGTime.floorTime(acsConfig.epoch_interval);

        if (tsync != startGTime)
        {
            BOOST_LOG_TRIVIAL(warning)
                << "Start epoch " << startGTime << " is not aligned to the epoch interval "
                << acsConfig.epoch_interval << ", rounding down to " << tsync;
        }

        acsConfig.start_epoch = tsync.to_posixTime();
    }

    createTracefiles(receiverMap, pppNet, ionNet);

    for (auto yaml : acsConfig.yamls)
    {
        YAML::Emitter emitter;
        emitter << YAML::DoubleQuoted << YAML::Flow << YAML::BeginSeq << yaml;

        string config(emitter.c_str() + 1);

        mongoOutputConfig(config);
    }

    configureUploadingStreams();

    configAtmosRegions(std::cout, receiverMap);

    if (acsConfig.mincon_only)
    {
        minconOnly(std::cout, receiverMap);
    }

    doDebugs();

    BOOST_LOG_TRIVIAL(info) << "\n";
    BOOST_LOG_TRIVIAL(info) << "Starting to process epochs...";

    //============================================================================
    // MAIN PROCESSING LOOP														//
    //============================================================================

    GTime lastEpochStartTime;
    GTime lastProcStartTime;
    GTime lastEpochStopTime;

    auto atLastEpoch = [&](bool processed = false) -> bool
    {
        // Check number of epochs
        if (acsConfig.max_epochs > 0 && epoch >= acsConfig.max_epochs)
        {
            BOOST_LOG_TRIVIAL(info) << "\n"
                                    << "Exiting at epoch " << epoch << " as epoch count "
                                    << acsConfig.max_epochs << " has been reached";

            return true;
        }

        if (tsync == GTime::noTime())
        {
            return false;
        }

        GWeek week = tsync;
        GTow  tow  = tsync;

        if (processed)
        {
            BOOST_LOG_TRIVIAL(info)
                << "Processed epoch #" << epoch << " - " << "GPS time: " << week << " "
                << std::setw(6) << tow << " - " << tsync << " (data handling took "
                << (lastProcStartTime - lastEpochStartTime) << ", processing took "
                << (lastEpochStopTime - lastProcStartTime) << ", epoch took "
                << (lastEpochStopTime - lastEpochStartTime) << ")";
        }

        // Check end epoch
        if (acsConfig.end_epoch.is_not_a_date_time() == false &&
            tsync.to_posixTime() >= acsConfig.end_epoch)
        {
            BOOST_LOG_TRIVIAL(info)
                << "Exiting at epoch " << epoch << " (" << tsync << ") as end epoch "
                << acsConfig.end_epoch << " has been reached";

            return true;
        }

        return false;
    };

    // Read the observations for each station and do stuff

    bool waitMessage = false;
    bool nextEpoch   = true;
    bool complete    = false;  // When all input files are empty the processing is deemed complete -
                               // run until then, or until something else breaks the loop
    int loopEpochs = 0;  // A count of how many loops of epoch_interval this loop used up (usually
                         // one, but may be more if skipping epochs)
    auto nominalLoopStartTime =
        system_clock::now();  // The time the next loop is expected to start - if it doesnt start
                              // until after this, it may be skipped
    GTime firstObsTime              = GTime::noTime();
    bool  holdingReconnectOutage    = false;
    bool  hasProcessedRealtimeEpoch = false;
    auto  reconnectOutageStart      = system_clock::time_point{};
    auto  lastOutageStatusLog       = system_clock::time_point{};

    while (complete == false)
    {
        // Increment epoch and tsync
        if (nextEpoch)
        {
            nextEpoch = false;
            epoch++;

            BOOST_LOG_TRIVIAL(info) << "Starting epoch #" << epoch;

            nominalLoopStartTime +=
                std::chrono::milliseconds((int)(acsConfig.wait_next_epoch * 1000));

            lastEpochStartTime = timeGet();

            if (tsync != GTime::noTime())
            {
                // dont obliterate the freshly configured tsync before the first epoch
                if (epoch != 1)
                {
                    tsync.bigTime += acsConfig.epoch_interval;
                }

                // Eugene: This will try forcing align tsync to integeral epochs, which can make
                // processing epochs something like: 0.0, 0.3, 0.6, 1.0, ...
                // if (fabs(tsync.bigTime - round(tsync.bigTime)) < acsConfig.epoch_tolerance)
                // {
                //     tsync.bigTime = round(tsync.bigTime);
                // }
            }

            for (auto& [id, rec] : receiverMap)
            {
                rec.ready = false;

                auto trace = getTraceFile(rec);

                trace << "\n";
                trace << "\n"
                      << "------=============== Epoch " << epoch << " =============-----------"
                      << "\n";
                trace << "\n"
                      << "------=============== Time  " << tsync << " =============-----------"
                      << "\n";
            }

            {
                auto pppTrace = getTraceFile(pppNet);

                pppTrace << "\n";
                pppTrace << "\n"
                         << "------=============== Epoch " << epoch << " =============-----------"
                         << "\n";
                pppTrace << "\n"
                         << "------=============== Time  " << tsync << " =============-----------"
                         << "\n";
            }
        }

        // Calculate the time at which we will stop waiting for data to come in for this epoch
        auto breakTime = nominalLoopStartTime +
                         std::chrono::milliseconds((int)(acsConfig.max_rec_latency * 1000));

        if (loopEpochs)  // Eugene: This doesn't do anything at all?
        {
            BOOST_LOG_TRIVIAL(info) << "Starting epoch #" << epoch;

            waitMessage = false;
        }

        if (system_clock::now() > breakTime)
        {
            BOOST_LOG_TRIVIAL(warning)
                << "Excessive time elapsed, skipping epoch " << epoch
                << ". Configuration 'wait_next_epoch' is " << acsConfig.wait_next_epoch;

            nextEpoch = true;

            if (atLastEpoch())
            {
                break;
            }

            continue;
        }

        // get observations from streams (allow some delay between stations, and retry, to ensure
        // all messages for the epoch have arrived)
        map<string, bool>   dataAvailableMap;
        map<string, string> missingObsReasons;
        map<string, string> missingObsSources;
        map<string, string> missingObsStates;
        vector<double>      readyLatencySeconds;
        bool                repeat  = true;
        int                 attempt = 0;
        while (repeat && system_clock::now() < breakTime)
        {
            attempt++;

            repeat = false;

            // load any changes from the config
            bool newConfig = acsConfig.parse();

            // make any changes to streams.
            ConfiguredStreamState configuredStreamState;
            if (newConfig)
            {
                reloadInputFiles();
                configureUploadingStreams();
                configuredStreamState = getConfiguredStreamState();
            }

            // remove any dead streams
            for (auto iter = streamParserMultimap.begin(); iter != streamParserMultimap.end();)
            {
                auto& [id, streamParser_ptr] = *iter;
                auto&  stream                = streamParser_ptr->stream;
                string recId                 = id;

                auto& recOpts = acsConfig.getRecOpts(id);

                auto removeReceiver = [&]()
                {
                    for (auto& [key, index] : pppNet.kfState.kfIndexMap)
                    {
                        if (key.str == recId)
                        {
                            pppNet.kfState.removeState(key);

                            auto nodeHandler          = pppNet.kfState.kfIndexMap.extract(key);
                            nodeHandler.key().rec_ptr = nullptr;
                            pppNet.kfState.kfIndexMap.insert(std::move(nodeHandler));
                        }
                    }

                    receiverMap.erase(recId);
                };

                if (newConfig &&
                    configuredStreamState.configuredSources.find(stream.sourceString) ==
                        configuredStreamState.configuredSources.end())
                {
                    auto retiredStreamParser = streamParser_ptr;

                    BOOST_LOG_TRIVIAL(info) << "Removing " << stream.sourceString
                                            << " because it is no longer configured";

                    streamDOAMap.erase(stream.sourceString);

                    if (configuredStreamState.configuredReceivers.find(recId) ==
                        configuredStreamState.configuredReceivers.end())
                    {
                        removeReceiver();
                    }

                    iter = streamParserMultimap.erase(iter);
                    retireStreamParser(std::move(retiredStreamParser));

                    continue;
                }

                if (recOpts.kill)
                {
                    BOOST_LOG_TRIVIAL(info)
                        << "Removing " << stream.sourceString << " due to kill config";
                    auto retiredStreamParser = streamParser_ptr;

                    removeReceiver();

                    iter = streamParserMultimap.erase(iter);
                    retireStreamParser(std::move(retiredStreamParser));

                    continue;
                }

                try
                {
                    auto& obsStream = dynamic_cast<ObsStream&>(*streamParser_ptr);

                    if (obsStream.hasObs())
                    {
                        iter++;
                        continue;
                    }
                }
                catch (...)
                {
                }

                if (stream.isAvailable() && stream.isDead())
                {
                    BOOST_LOG_TRIVIAL(info) << "No more data available on " << stream.sourceString;

                    // record as dead and erase
                    streamDOAMap[stream.sourceString] = true;

                    iter = streamParserMultimap.erase(iter);

                    continue;
                }

                iter++;
            }

            if (newConfig)
            {
                hasRealtimeObsInput = hasRealtimeObservationInput();
            }

            // Check if all streams are dead
            if (streamParserMultimap.empty())
            {
                static bool once = true;
                if (once)
                {
                    once = false;

                    BOOST_LOG_TRIVIAL(info) << "\n";
                    BOOST_LOG_TRIVIAL(info) << "Inputs finished at epoch #" << epoch;
                }

                if (acsConfig.require_obs)
                    complete = true;

                break;
            }

            BOOST_LOG_TRIVIAL(debug) << "\n";
            BOOST_LOG_TRIVIAL(debug) << "tsync: " << tsync.to_string(6) << " - attempt " << attempt;
            BOOST_LOG_TRIVIAL(debug) << "Parsing non-observation streams ...";

            // parse all non-observation streams
            for (auto& [id, streamParser_ptr] : streamParserMultimap)
                try
                {
                    auto& obsStream = dynamic_cast<ObsStream&>(*streamParser_ptr);
                }
                catch (std::bad_cast& e)
                {
                    streamParser_ptr->parse();
                }

            BOOST_LOG_TRIVIAL(debug) << "Parsing observation streams ...";

            // Parse observation streams (including pseudo observations) for all receivers
            for (auto& [id, rec] : receiverMap)
            {
                auto& recOpts = acsConfig.getRecOpts(id);

                if (recOpts.exclude)
                {
                    continue;
                }

                if (rec.ready)
                {
                    BOOST_LOG_TRIVIAL(debug) << "Receiver " << id << " is ready, skipping ...";
                    continue;
                }

                missingObsReasons[id] = "no_obs_stream";

                auto trace = getTraceFile(rec);

                // Search suitable data from all files from this receiver (for real-time data, there
                // should be only one unique stream per receiver)
                auto [begin, end] = streamParserMultimap.equal_range(id);
                for (auto it = begin; it != end; it++)
                {
                    auto& streamParser_ptr = it->second;

                    ObsStream* obsStream_ptr;

                    try
                    {
                        obsStream_ptr = &dynamic_cast<ObsStream&>(*streamParser_ptr);
                    }
                    catch (std::bad_cast& e)
                    {
                        continue;
                    }

                    auto& obsStream = *obsStream_ptr;

                    missingObsSources[id] = streamDisplayName(obsStream.stream.sourceString);

                    auto describeStreamState = [&](const string& reason)
                    {
                        std::ostringstream state;
                        state << "reason=" << reason
                              << ", source=" << streamDisplayName(obsStream.stream.sourceString)
                              << ", available=" << (obsStream.stream.isAvailable() ? "yes" : "no")
                              << ", dead=" << (obsStream.stream.isDead() ? "yes" : "no");

                        if (auto* tcpSocket = dynamic_cast<TcpSocket*>(&obsStream.stream))
                        {
                            long long nextReconnectUnixTime = tcpSocket->nextReconnectUnixTime();
                            if (nextReconnectUnixTime > 0)
                            {
                                auto nowUnixTime =
                                    std::chrono::system_clock::to_time_t(system_clock::now());
                                long long reconnectInSeconds =
                                    std::max(0LL, nextReconnectUnixTime - (long long)nowUnixTime);
                                state << ", reconnect_in_s=" << reconnectInSeconds;
                            }
                            else
                            {
                                state << ", reconnect_in_s=0";
                            }
                        }
                        else
                        {
                            state << ", reconnect_in_s=n/a";
                        }

                        if (obsStream.obsAgeCode != E_ObsAgeCode::UNKNOWN)
                        {
                            state << ", obs_age=" << obsStream.obsAgeCode;
                        }

                        return state.str();
                    };

                    BOOST_LOG_TRIVIAL(debug) << "Reading stream '" << obsStream.stream.sourceString
                                             << "', isAvailable=" << obsStream.stream.isAvailable()
                                             << ", isDead=" << obsStream.stream.isDead();

                    if (obsStream.stream.isAvailable() == false)
                    {
                        missingObsReasons[id] = "stream_unavailable";
                        missingObsStates[id]  = describeStreamState("stream_unavailable");
                        BOOST_LOG_TRIVIAL(debug) << "Skipping unavailable stream '"
                                                 << obsStream.stream.sourceString << "'";
                        continue;
                    }

                    if (auto* tcpSocket = dynamic_cast<TcpSocket*>(&obsStream.stream))
                    {
                        if (tcpSocket->reconnectDueAfter(breakTime))
                        {
                            missingObsReasons[id] = "reconnect_after_breaktime";
                            missingObsStates[id] = describeStreamState("reconnect_after_breaktime");
                            BOOST_LOG_TRIVIAL(
                                debug
                            ) << "Skipping stream '"
                              << obsStream.stream.sourceString
                              << "' for this epoch because reconnect is scheduled after breakTime";
                            continue;
                        }
                    }

                    double epochTolerance =
                        DTTOL;  // Use a very small tolerance if data interval is unknown (0) yet,
                                // otherwise can take in multiple-epoch data

                    // try to get some data (again)
                    bool moreData = true;
                    while (moreData)
                    {
                        if (acsConfig.assign_closest_epoch)
                        {
                            BOOST_LOG_TRIVIAL(warning)
                                << "'assign_closest_epoch' is on, observations that fall between "
                                   "processing epochs will be grouped to nearest epochs despite "
                                   "'epoch_tolerance'";

                            epochTolerance = acsConfig.epoch_interval / 2;
                        }
                        else if (obsStream.interval > 0)
                        {
                            epochTolerance =
                                std::min(obsStream.interval / 2, acsConfig.epoch_tolerance);
                        }

                        BOOST_LOG_TRIVIAL(debug)
                            << "Getting obs, id=" << id << ", targetTime=" << tsync.to_string(6)
                            << ", epochTolerance=" << epochTolerance;

                        GTime obsTime = tsync;
                        rec.obsList   = obsStream.getObs(obsTime, epochTolerance);

                        cleanSignals(rec.obsList);

                        switch (obsStream.obsAgeCode)
                        {
                            case E_ObsAgeCode::UNKNOWN:
                                moreData = false;
                                if (firstObsTime == GTime::noTime() || obsTime < firstObsTime)
                                {
                                    firstObsTime = obsTime;
                                }
                                break;
                            case E_ObsAgeCode::NO_OBS:
                                moreData = false;
                                break;
                            case E_ObsAgeCode::PAST_OBS:
                                preprocessor(trace, rec);
                                break;
                            case E_ObsAgeCode::CURRENT_OBS:
                                moreData = false;  // Eugene to fix: May have more current obs from
                                                   // next file
                                preprocessor(trace, rec);
                                break;
                            case E_ObsAgeCode::FUTURE_OBS:
                                moreData = false;
                                break;
                        }

                        BOOST_LOG_TRIVIAL(debug)
                            << "Checking obs age and preprocessing data if needed"
                            << ", obsAgeCode=" << obsStream.obsAgeCode
                            << ", moreData=" << (moreData ? "true" : "false");
                    }

                    if (rec.obsList.empty())
                    {
                        // Can only be NO_OBS or FUTURE_OBS
                        if (obsStream.obsAgeCode == E_ObsAgeCode::NO_OBS)
                        {
                            missingObsReasons[id] = "no_obs";
                            missingObsStates[id]  = describeStreamState("no_obs");
                            // Failed to get observations, try again later
                            repeat = true;  // Only need to retry on NO_OBS, for FUTURE_OBS, just
                                            // move to next receiver

                            BOOST_LOG_TRIVIAL(debug)
                                << "Failed to get observations, try again later ...";
                        }
                        else if (obsStream.obsAgeCode == E_ObsAgeCode::FUTURE_OBS)
                        {
                            missingObsReasons[id] = "future_obs";
                            missingObsStates[id]  = describeStreamState("future_obs");
                        }
                        else if (obsStream.obsAgeCode == E_ObsAgeCode::UNKNOWN)
                        {
                            missingObsReasons[id] = "unknown_obs_age";
                            missingObsStates[id]  = describeStreamState("unknown_obs_age");
                        }

                        break;
                    }

                    dataAvailableMap[rec.id] = true;
                    missingObsReasons.erase(rec.id);
                    missingObsSources.erase(rec.id);
                    missingObsStates.erase(rec.id);
                    rec.ready  = true;
                    rec.source = obsStream.stream.sourceString;

                    extractReceiverMetadata(rec, obsStream.parser, &rec.obsList);
                    extractTrackedSignals(rec, obsStream.parser, &rec.obsList);

                    GTime readyTime = timeGet();
                    if (tsync != GTime::noTime())
                    {
                        double readyLatencySecondsValue = (readyTime - tsync).to_double();
                        readyLatencySeconds.push_back(readyLatencySecondsValue);

                        BOOST_LOG_TRIVIAL(debug)
                            << "Receiver " << rec.id << " ready diagnostic: attempt=" << attempt
                            << ", ready_time=" << readyTime << ", ready_latency_s=" << std::fixed
                            << std::setprecision(2) << readyLatencySecondsValue
                            << ", obs_count=" << rec.obsList.size()
                            << ", source=" << streamDisplayName(rec.source);
                    }
                    else
                    {
                        BOOST_LOG_TRIVIAL(debug)
                            << "Receiver " << rec.id << " ready diagnostic: attempt=" << attempt
                            << ", ready_time=" << readyTime
                            << ", ready_latency_s=startup_pending_tsync"
                            << ", obs_count=" << rec.obsList.size()
                            << ", source=" << streamDisplayName(rec.source);
                    }

                    auto now = system_clock::now();

                    if (now >= nominalLoopStartTime)
                    {
                        auto nominalLatency = now - nominalLoopStartTime;

                        trace << "\n"
                              << std::chrono::duration_cast<std::chrono::milliseconds>(
                                     nominalLoopStartTime - peaStartTimeChrono
                                 )
                                     .count()
                              << "ms" << " "
                              << std::chrono::duration_cast<std::chrono::milliseconds>(
                                     now - peaStartTimeChrono
                                 )
                                     .count()
                              << "ms" << " " << rec.id << " nominal latency :  "
                              << std::chrono::duration_cast<std::chrono::milliseconds>(
                                     nominalLatency
                                 )
                                     .count()
                              << "ms";
                    }
                    else
                    {
                        // this observation is earlier than expected
                        // only shorten waiting periods, never extend

                        auto nominalLatency = nominalLoopStartTime - now;

                        trace << "\n"
                              << std::chrono::duration_cast<std::chrono::milliseconds>(
                                     nominalLoopStartTime - peaStartTimeChrono
                                 )
                                     .count()
                              << "ms" << " "
                              << std::chrono::duration_cast<std::chrono::milliseconds>(
                                     now - peaStartTimeChrono
                                 )
                                     .count()
                              << "ms" << " " << rec.id << " nominal latency : -"
                              << std::chrono::duration_cast<std::chrono::milliseconds>(
                                     nominalLatency
                                 )
                                     .count()
                              << "ms" << " Advancing start time";

                        auto alternateBreakTime =
                            now +
                            std::chrono::milliseconds((int)(acsConfig.max_rec_latency * 1000));
                        auto alternateStartTime = now;

                        if (alternateBreakTime < breakTime)
                        {
                            breakTime = alternateBreakTime;
                        }
                        if (alternateStartTime < nominalLoopStartTime)
                        {
                            nominalLoopStartTime = alternateStartTime;
                        }
                    }

                    break;
                }
            }
            // Determine start time by earliest obs time and try getting obs again
            if (tsync == GTime::noTime())
            {
                tsync = firstObsTime.floorTime(acsConfig.epoch_interval);

                acsConfig.start_epoch = tsync.to_posixTime();

                BOOST_LOG_TRIVIAL(warning)
                    << "Start epoch not configured, rounding down first obs time " << firstObsTime
                    << ", epoch_interval=" << acsConfig.epoch_interval
                    << ", start_epoch=" << tsync.to_string(6);

                repeat = true;
            }

            if (repeat)
            {
                sleep_for(std::chrono::milliseconds(acsConfig.sleep_milliseconds));
            }
        }

        BOOST_LOG_TRIVIAL(debug) << "Epoch data handling done with " << attempt << " attempt(s)\n";

        if (complete)
        {
            break;
        }

        if (acsConfig.allow_missing_inputs && dataAvailableMap.empty())
        {
            if (waitMessage == false)
            {
                waitMessage = true;

                BOOST_LOG_TRIVIAL(info) << "No more data available, waiting for further inputs...";
            }

            continue;
        }

        if (dataAvailableMap.empty() && acsConfig.require_obs && hasProcessedRealtimeEpoch &&
            epoch > 1)
        {
            bool      sawRealtimeObsStream      = false;
            long long earliestReconnectUnixTime = 0;

            for (auto& [id, streamParser_ptr] : streamParserMultimap)
            {
                ObsStream* obsStream_ptr = nullptr;

                try
                {
                    obsStream_ptr = &dynamic_cast<ObsStream&>(*streamParser_ptr);
                }
                catch (std::bad_cast&)
                {
                    continue;
                }

                auto& obsStream = *obsStream_ptr;

                if (obsStream.isPseudoRec)
                {
                    continue;
                }

                auto* tcpSocket = dynamic_cast<TcpSocket*>(&obsStream.stream);
                if (tcpSocket == nullptr)
                {
                    continue;
                }

                sawRealtimeObsStream = true;

                long long nextReconnectUnixTime = tcpSocket->nextReconnectUnixTime();
                if (nextReconnectUnixTime > 0 &&
                    (earliestReconnectUnixTime == 0 ||
                     nextReconnectUnixTime < earliestReconnectUnixTime))
                {
                    earliestReconnectUnixTime = nextReconnectUnixTime;
                }
            }

            if (sawRealtimeObsStream)
            {
                if (holdingReconnectOutage == false)
                {
                    holdingReconnectOutage = true;
                    reconnectOutageStart   = system_clock::now();
                    lastOutageStatusLog    = reconnectOutageStart - 1s;
                    BOOST_LOG_TRIVIAL(info) << "Realtime outage hold active, keeping tsync at "
                                            << tsync << " until realtime observations resume";
                }

                auto now = system_clock::now();

                if (now - lastOutageStatusLog >= 1s)
                {
                    auto outageElapsed = std::chrono::duration_cast<std::chrono::seconds>(
                        now - reconnectOutageStart
                    );
                    BOOST_LOG_TRIVIAL(info) << "Realtime outage hold ongoing, tsync=" << tsync
                                            << ", outage_elapsed=" << outageElapsed.count() << "s";
                    lastOutageStatusLog = now;
                }

                auto holdSleep = std::chrono::milliseconds(acsConfig.sleep_milliseconds);

                if (earliestReconnectUnixTime > 0)
                {
                    auto earliestReconnectTime =
                        system_clock::from_time_t((time_t)earliestReconnectUnixTime);

                    if (earliestReconnectTime > now)
                    {
                        holdSleep = std::min(
                            std::chrono::duration_cast<std::chrono::milliseconds>(
                                earliestReconnectTime - now
                            ),
                            std::chrono::milliseconds(1000)
                        );
                    }
                }

                sleep_for(holdSleep);
                nominalLoopStartTime = system_clock::now();
                lastEpochStartTime   = timeGet();

                continue;
            }
        }
        else if (holdingReconnectOutage)
        {
            holdingReconnectOutage = false;
            auto outageElapsed     = std::chrono::duration_cast<std::chrono::seconds>(
                system_clock::now() - reconnectOutageStart
            );
            BOOST_LOG_TRIVIAL(info) << "Realtime outage hold released at tsync " << tsync
                                    << " after " << outageElapsed.count() << "s";
        }

        if (tsync == GTime::noTime())
        {
            if (acsConfig.require_obs)
                continue;

            tsync = timeGet().floorTime(acsConfig.epoch_interval);

            acsConfig.start_epoch = tsync.to_posixTime();
        }

        lastProcStartTime = timeGet();

        bool realtimeProcessing = acsConfig.simulate_real_time || hasRealtimeObsInput;

        std::ostringstream syncSummary;
        syncSummary << "Synced " << dataAvailableMap.size() << "/" << receiverMap.size()
                    << " receivers after " << attempt << " attempt(s)";

        if (realtimeProcessing)
        {
            syncSummary << ", sync_now=" << lastProcStartTime
                        << ", sync_latency=" << (lastProcStartTime - tsync);

            if (readyLatencySeconds.empty() == false)
            {
                std::sort(readyLatencySeconds.begin(), readyLatencySeconds.end());

                auto percentile = [&](double p)
                {
                    size_t index = (size_t)std::floor(p * (readyLatencySeconds.size() - 1));
                    return readyLatencySeconds[index];
                };

                syncSummary << ", ready_latency_s[min/p25/p50/p75/max]=" << std::fixed
                            << std::setprecision(2) << readyLatencySeconds.front() << "/"
                            << percentile(0.25) << "/" << percentile(0.50) << "/"
                            << percentile(0.75) << "/" << readyLatencySeconds.back();
            }

            BOOST_LOG_TRIVIAL(info) << syncSummary.str();
        }
        else if (attempt > 1 || dataAvailableMap.size() < receiverMap.size())
        {
            BOOST_LOG_TRIVIAL(info) << syncSummary.str();
        }
        else
        {
            BOOST_LOG_TRIVIAL(debug) << syncSummary.str();
        }

        for (auto& [id, rec] : receiverMap)
        {
            if (dataAvailableMap.find(id) != dataAvailableMap.end())
            {
                latestMissingObsStatusByReceiver.erase(id);
                continue;
            }

            auto it = missingObsReasons.find(id);
            if (it == missingObsReasons.end())
            {
                latestMissingObsStatusByReceiver.erase(id);
                continue;
            }

            std::ostringstream missingDiagnostic;
            missingDiagnostic << "Receiver " << id << " not-ready diagnostic: attempts=" << attempt
                              << ", sync_reason=" << it->second
                              << ", sync_latency=" << (lastProcStartTime - tsync);

            auto sourceIt = missingObsSources.find(id);
            if (sourceIt != missingObsSources.end() && sourceIt->second.empty() == false)
            {
                missingDiagnostic << ", source=" << sourceIt->second;
            }

            auto stateIt = missingObsStates.find(id);
            if (stateIt != missingObsStates.end() && stateIt->second.empty() == false)
            {
                latestMissingObsStatusByReceiver[id] = stateIt->second;
                missingDiagnostic << ", state={" << stateIt->second << "}";
            }
            else
            {
                latestMissingObsStatusByReceiver.erase(id);
            }

            BOOST_LOG_TRIVIAL(debug) << missingDiagnostic.str();
        }

        if (acsConfig.require_obs == false || dataAvailableMap.empty() == false)
        {
            mainOncePerEpoch(pppNet, ionNet, receiverMap, tsync);

            if (acsConfig.require_obs && dataAvailableMap.empty() == false)
            {
                hasProcessedRealtimeEpoch = true;
            }
        }
        lastEpochStopTime = timeGet();

        if (atLastEpoch(true))
        {
            break;
        }

        nextEpoch = true;
    }

    // Disconnect the downloading clients and stop the io_service for clean shutdown.
    for (auto& [id, ntripStream] : only<NtripStream>(streamParserMultimap))
    {
        ntripStream.disconnect();
    }

    ntripBroadcaster.stopBroadcast();
    TcpSocket::ioContext.stop();

    GTime peaInterTime = timeGet();
    BOOST_LOG_TRIVIAL(info) << "\n"
                            << "PEA started  processing at : " << peaStartTime << "\n"
                            << "and finished processing at : " << peaInterTime << "\n"
                            << "Total processing duration  : " << (peaInterTime - peaStartTime)
                            << "\n"
                            << "\n";

    BOOST_LOG_TRIVIAL(info) << "\n"
                            << "Finalising streams and post processing...";

    mainPostProcessing(pppNet, ionNet, receiverMap);

    GTime peaStopTime = timeGet();
    BOOST_LOG_TRIVIAL(info) << "\n"
                            << "PEA started  processing at : " << peaStartTime << "\n"
                            << "and finished processing at : " << peaStopTime << "\n"
                            << "Total processing duration  : " << (peaStopTime - peaStartTime)
                            << "\n"
                            << "\n";

    BOOST_LOG_TRIVIAL(info) << "PEA finished";

    return EXIT_SUCCESS;
}
