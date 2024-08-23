
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

/** Retrieve data from streams and process together once per epoch.
 *
 * In order to be capable of real-time and post-processed execution, the pea treats all observation inputs as streams.
 * These streams are created with a stream source type, and a parser, with the separation of these strategies allowing for far more reusable code,
 * and immediate extension of inputs from simply files, to HTTP, TCP, Serial, Pipes etc.
 *
 * The fundamental tick of the pea is the `epoch_interval`, which drives all major processing steps.
 * Once an initial epoch has been designated the pea requests data for the next epoch from each stream.
 * The streams either respond with the data, or with a flag indicating the data may be available later, or will never be available.
 *
 * Various configuration parameters define the exact flow through the synchronisation code, however once all data is available, or no more is expected,
 * or a timeout has occurred, the epoch is considered synchronised and processing is performed.
 *
 * Unlike observation streams, which provide data up until the requested epoch time, other streams such as navigation streams parse all data available to them,
 * and output any values to global maps, which may be later accessed as required by other components.
 *
 * The synchronisation process parses all streams sequentially, without multithreading, so map collisions are not expected.
 */
Architecture Streams_And_Synchronisation__()
{

}

#include <sys/time.h>
#include <filesystem>
#include <algorithm>
#include <iostream>
#include <signal.h>
#include <fstream>
#include <memory>
#include <chrono>
#include <thread>
#include <string>

#ifdef ENABLE_PARALLELISATION
	#include "omp.h"
#endif

using namespace std::literals::chrono_literals;
using std::this_thread::sleep_for;
using std::chrono::system_clock;
using std::chrono::time_point;
using std::make_unique;
using std::make_shared;
using std::string;

#include <boost/log/utility/setup/console.hpp>
#include <boost/system/error_code.hpp>
#include <boost/log/trivial.hpp>

#include "interactiveTerminal.hpp"
#include "minimumConstraints.hpp"
#include "peaCommitStrings.hpp"
#include "ntripBroadcast.hpp"
#include "inputsOutputs.hpp"
#include "rinexNavWrite.hpp"
#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "algebraTrace.hpp"
#include "rtsSmoothing.hpp"
#include "observations.hpp"
#include "preprocessor.hpp"
#include "streamNtrip.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "mongoWrite.hpp"
#include "streamObs.hpp"
#include "tcpSocket.hpp"
#include "mongoRead.hpp"
#include "testUtils.hpp"
#include "orbitProp.hpp"
#include "ionoModel.hpp"
#include "receiver.hpp"
#include "posProp.hpp"
#include "summary.hpp"
#include "fileLog.hpp"
#include "sinex.hpp"
#include "gTime.hpp"
#include "debug.hpp"
#include "api.hpp"

bool					doDocs	= false;
Navigation				nav		= {};
int						epoch	= 0;
GTime					tsync	= GTime::noTime();
map<int, SatIdentity>	satIdMap;

void avoidCollisions(
	ReceiverMap&		receiverMap)
{
	for (auto& [id, rec] : receiverMap)
	{
		auto trace = getTraceFile(rec);
	}

	for (auto& [id, rec] : receiverMap)
	{
		//create sinex estimate maps
		theSinex.estimatesMap	[id];
		theSinex.solEpochMap	[id];

		auto& recOpts = acsConfig.getRecOpts(id);

		if (recOpts.sat_id.empty() == false)
		{
			auto& satNav = nav.satNavMap[recOpts.sat_id.c_str()];
		}
	}
}

/** Perform operations for each station
* This function occurs in parallel with other stations - ensure that any operations on global maps do not create new entries, as that will destroy the map for other processes.
* Variables within the rec object are ok to use, but be aware that pointers from the within the receiver often point to global variables.
* Prepare global maps by accessing the desired elements before calling this function.
*/
void mainOncePerEpochPerStation(
	Receiver&	rec,
	Network&	net,
	bool&		emptyEpoch,
	KFState&	remoteState)
{
	if (rec.isPseudoRec)
	{
		return;
	}

	auto trace = getTraceFile(rec);

	if (rec.ready == false)
	{
		trace << "\n"			<< "Receiver " << rec.id << " has no data for this epoch";
		BOOST_LOG_TRIVIAL(info)	<< "Receiver " << rec.id << " has no data for this epoch";
		return;
	}

	sinexPerEpochPerStation(trace, tsync, rec);

	preprocessor(trace, rec, true);

	//recalculate variances now that elevations are known due to satellite postions calculation above
	obsVariances(rec.obsList);

	if (acsConfig.process_spp)
	{
		spp(trace, rec.obsList, rec.sol, rec.id, &net.kfState, &remoteState);
	}

	if	(  rec.ready == false
		|| rec.invalid)
	{
		return;
	}

	auto missingWarnInvalidate = [&](
			string	thing,
			bool	failureBool,
			bool	requiredConfig) -> bool
	{
		if (failureBool == false)
		{
			return false;
		}

		if (requiredConfig)
		{
			trace << "\n"				<< "Warning: Receiver " << rec.id << " rejected due to lack of " << thing;
			BOOST_LOG_TRIVIAL(warning)	<< "Warning: Receiver " << rec.id << " rejected due to lack of " << thing;

			rec.invalid = true;

			return true;
		}

		BOOST_LOG_TRIVIAL(warning)	<< "Warning: " << thing << " not found for " << rec.id;

		return false;
	};

	bool sppUsed;
	selectAprioriSource(trace, rec, tsync, sppUsed, net.kfState, &remoteState);

	if (missingWarnInvalidate("Apriori position1",		sppUsed,					acsConfig.require_apriori_positions))		return;
	if (missingWarnInvalidate("Apriori position2",		rec.failureAprioriPos,		acsConfig.require_apriori_positions))		return;
	if (missingWarnInvalidate("Antenna details",		rec.antennaId.empty(),		acsConfig.require_antenna_details))			return;
	if (missingWarnInvalidate("Site eccentricity",		rec.failureEccentricity,	acsConfig.require_site_eccentricity))		return;
	if (missingWarnInvalidate("Sinex information",		rec.failureSinex,			acsConfig.require_sinex_data))				return;

	emptyEpoch = false;

	BOOST_LOG_TRIVIAL(trace)
	<< "Read " << rec.obsList.size()
	<< " observations for station " << rec.id;

	for (auto& obs : only<GObs>(rec.obsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		if (obs.satStat_ptr == nullptr)
		{
			continue;
		}

		auto& satStat = *obs.satStat_ptr;
		auto& recOpts = acsConfig.getRecOpts(rec.id, {obs.Sat.sysName()});

		if (satStat.el < recOpts.elevation_mask_deg * D2R)
		{
			obs.excludeElevation = true;
		}
	}

	//calculate statistics
	{
		if ((GTime) rec.firstEpoch	== GTime::noTime())		{	rec.firstEpoch	= rec.obsList.front()->time;		}
																rec.lastEpoch	= rec.obsList.front()->time;
		rec.epochCount++;
		rec.obsCount += rec.obsList.size();

		for (auto& obs				: only<GObs>(rec.obsList))
		for (auto& [ft, sigList]	: obs.sigsLists)
		for (auto& sig				: sigList)
		{
			rec.codeCount[sig.code]++;
		}

		for (auto& obs				: only<GObs>(rec.obsList))
		{
			rec.satCount[obs.Sat]++;
		}
	}

	if (acsConfig.process_ionosphere)
	{
		obsIonoData(trace, rec);
	}

	auto& recOpts = acsConfig.getRecOpts(rec.id);

	rec.antBoresight	= recOpts.antenna_boresight;
	rec.antAzimuth		= recOpts.antenna_azimuth;

	recAtt(rec, tsync, recOpts.attitudeModel.sources);

	testEclipse(rec.obsList);

	if (acsConfig.output_rinex_obs)
	{
		writeRinexObs(rec.id, rec.snx, tsync, rec.obsList, acsConfig.rinex_obs_version);
	}
}

void mainOncePerEpochPerSatellite(
	Trace&		trace,
	GTime		time,
	SatSys		Sat,
	KFState&	kfState,
	KFState&	remoteKF)
{
	auto& satNav 	= nav.satNavMap[Sat];
	auto& satOpts	= acsConfig.getSatOpts(Sat);

	if (satOpts.exclude)
	{
		return;
	}

	//get svn and block type if possible
	if (Sat.svn().empty())
	{
		auto it = nav.svnMap[Sat].lower_bound(time);
		if (it == nav.svnMap[Sat].end())
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: SVN not found for " << Sat.id();

			Sat.setSvn("UNKNOWN");
		}
		else
		{
			Sat.setSvn(it->second);
		}

		//reinitialise the options with the updated values
		satOpts._initialised = false;
	}

	if (Sat.blockType().empty())
	{
		auto it = nav.blocktypeMap.find(Sat.svn());
		if (it == nav.blocktypeMap.end())
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: Block type not found for " << Sat.id() << ", attitude modelling etc may be affected, check sinex file";

			Sat.setBlockType("UNKNOWN");
		}
		else
		{
			Sat.setBlockType(it->second);
		}

		//reinitialise the options with the updated values
		satOpts._initialised = false;		//todo aaron, this is insufficient since the opts are inherited from the other initialised ones per file which are not reset
	}

	satOpts = acsConfig.getSatOpts(Sat);

	satNav.antBoresight	= satOpts.antenna_boresight;
	satNav.antAzimuth	= satOpts.antenna_azimuth;

	selectAprioriSource(Sat, time, kfState, &remoteKF);
}

void cullData(
	GTime time)
{
	cullOldEphs		(time);
	cullOldSSRs		(time);
	cullOldBiases	(time);

	mongoCull(time);
}

void mainOncePerEpoch(
	Network&		pppNet,
	Network&		ionNet,
	ReceiverMap&	receiverMap,
	GTime			time)
{
	avoidCollisions(receiverMap);

	//reload any new or modified files
	reloadInputFiles();

	addDefaultBias();

	createTracefiles(receiverMap, pppNet, ionNet);

	auto pppTrace = getTraceFile(pppNet);

	//initialise mongo if not already done
	mongoooo();

	//integrate accelerations and early prepare satPos0 with propagated values
	predictOrbits	(pppTrace, pppNet.kfState, time);
	// predictInertials(pppTrace, pppNet.kfState, time);

	InteractiveTerminal::setMode(E_InteractiveMode::Preprocessing);
	BOOST_LOG_TRIVIAL(info) << " ------- PREPROCESSING STATIONS       --------" << "\n";

	KFState remoteState;
	if (acsConfig.mongoOpts.use_predictions)
	{
		mongoReadFilter(remoteState, time, acsConfig.mongoOpts.used_predictions);

		BOOST_LOG_TRIVIAL(info) << "Remote state was updated at " << remoteState.time.to_string();

		remoteState.outputStates(pppTrace, "/REMOTE");

		mongoStates(pppNet.kfState,
					{
						.suffix		= "/REMOTE",
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});

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

	//try to get svns & block types of all used satellites
	for (auto& [Sat, satNav] : nav.satNavMap)
	{
		if (acsConfig.process_sys[Sat.sys] == false)
			continue;

		mainOncePerEpochPerSatellite(pppTrace, time, Sat, pppNet.kfState, remoteState);
	}

	//do per-station pre processing
	bool emptyEpoch = true;
#	ifdef ENABLE_PARALLELISATION
		Eigen::setNbThreads(1);
#		pragma omp parallel for
#	endif
	for (int i = 0; i < receiverMap.size(); i++)
	{
		auto rec_ptr_iterator = receiverMap.begin();
		std::advance(rec_ptr_iterator, i);

		auto& [id, rec] = *rec_ptr_iterator;
		mainOncePerEpochPerStation(rec, pppNet, emptyEpoch, remoteState);
	}
	Eigen::setNbThreads(0);


	if	(emptyEpoch)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Epoch " << epoch << " has no observations";
	}

	if (acsConfig.process_ppp)
	{
		ppp(pppTrace, receiverMap, pppNet.kfState, remoteState);
	}

	KFState* kfState_ptr;

	KFState tempKfState;

	if (acsConfig.ambrOpts.fix_and_hold)	{									kfState_ptr = &pppNet.kfState;		}
	else									{	tempKfState = pppNet.kfState;	kfState_ptr = &tempKfState;		}

	auto& kfState = *kfState_ptr;

	perEpochPostProcessingAndOutputs(pppTrace, pppNet, ionNet, receiverMap, kfState, ionNet.kfState, time, emptyEpoch);

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
void mainPostProcessing(
	Network&		pppNet,
	Network&		ionNet,
	ReceiverMap&	receiverMap)
{
	BOOST_LOG_TRIVIAL(info)
	<< "Post processing... ";

	auto pppTrace = getTraceFile(pppNet);

	if	( 	acsConfig.process_ppp
		&&	acsConfig.ambrOpts.mode != +E_ARmode::OFF
		&&	acsConfig.ambrOpts.once_per_epoch == false
		&&	acsConfig.ambrOpts.fix_and_hold)
	{
		BOOST_LOG_TRIVIAL(info)
		<< "\n"
		<< "---------------PERFORMING AMBIGUITY RESOLUTION ON NETWORK WITH FIX AND HOLD ------------- " << "\n";

		fixAndHoldAmbiguities(pppTrace, pppNet.kfState);

		mongoStates(pppNet.kfState,
					{
						.suffix		= "/AR",
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});
	}

	if	(	acsConfig.process_ppp
		&&	acsConfig.process_minimum_constraints
		&&	acsConfig.minconOpts.once_per_epoch == false)
	{
		InteractiveTerminal::setMode(E_InteractiveMode::MinimumConstraints);
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

				satPos.Sat			= Sat;
				satPos.satNav_ptr	= &satNav;

				bool pass =	satpos(nullStream, pppNet.kfState.time, pppNet.kfState.time, satPos, {E_Source::PRECISE, E_Source::BROADCAST}, E_OffsetType::COM, nav);
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(warning) << "Warning: No sat pos found for " << satPos.Sat.id() << ".";
					continue;
				}

				satNav.aprioriPos = frameSwapper(satPos.rSatCom);
			}
		}

		MinconStatistics minconStatistics;


		InteractiveTerminal minconTrace("MinimumConstraints", pppTrace);

		mincon(minconTrace, pppNet.kfState, &minconStatistics);

		pppNet.kfState.outputStates(minconTrace, "/CONSTRAINED");

		mongoStates(pppNet.kfState,
					{
						.suffix		= "/CONSTRAINED",
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});

		outputMinconStatistics(minconTrace, minconStatistics);
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
			//prevent the memoised orbital positions being used in rts
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

void tryExitGracefully(
	int signum)
{
	static bool closeRequest = false;

	if (closeRequest)
	{
		//second time this is called, dont worry too much about gracefullness
		interactiveTerminaldestructor.~InteractiveTerminalDestructor();

		abort();
	}

	BOOST_LOG_TRIVIAL(info) << "SIGINT detected - tidying up and exiting...";

	closeRequest = true;

	acsConfig.max_epochs = 1;
}


int main(
	int		argc,
	char**	argv)
{
	traceLevel = 5;

	// Register the sink in the logging core
	boost::log::core::get()->add_sink(boost::make_shared<sinks::synchronous_sink<ConsoleLog>>());
	boost::log::core::get()->set_filter(boost::log::trivial::severity >= boost::log::trivial::info);
	acsSeverity = boost::log::trivial::info;

	BOOST_LOG_TRIVIAL(info)
	<< "PEA starting... (" << ginanBranchName() << " " << ginanCommitVersion() << " from " << ginanCommitDate() << ")" << "\n";

	GTime	peaStartTime		= timeGet();
	auto	peaStartTimeChrono	= system_clock::now();

	exitOnErrors();

	// Define the function to be called when ctrl-c (SIGINT) is sent to process
	signal(SIGINT, tryExitGracefully);

	bool pass = configure(argc, argv);
	if (pass == false)
	{
		BOOST_LOG_TRIVIAL(error) 	<< "Error: Incorrect configuration";
		BOOST_LOG_TRIVIAL(info) 	<< "PEA finished";
		TcpSocket::ioService.stop();
		return EXIT_FAILURE;
	}

	if (acsConfig.output_log)
	{
		addFileLog();
	}

	BOOST_LOG_TRIVIAL(info)	<< "Compilation details:";
	BOOST_LOG_TRIVIAL(info)	<< "====================";
	BOOST_LOG_TRIVIAL(info)	<< "Ginan branch:     " << ginanBranchName();
	BOOST_LOG_TRIVIAL(info)	<< "Ginan version:    " << ginanCommitVersion();
	BOOST_LOG_TRIVIAL(info)	<< "Commit date:      " << ginanCommitDate();
	BOOST_LOG_TRIVIAL(info)	<< "Operating system: " << ginanOsName();
	BOOST_LOG_TRIVIAL(info)	<< "Compiler version: " << ginanCompilerVersion();
	BOOST_LOG_TRIVIAL(info)	<< "Boost version:    " << ginanBoostVersion();
	BOOST_LOG_TRIVIAL(info)	<< "Eigen version:    " << ginanEigenVersion();
	BOOST_LOG_TRIVIAL(info)	<< "Mongocxx version: " << ginanMongoVersion();
	BOOST_LOG_TRIVIAL(info) << "\n";



	BOOST_LOG_TRIVIAL(info)	<< "Runtime details:";
	BOOST_LOG_TRIVIAL(info)	<< "================";
	BOOST_LOG_TRIVIAL(info)	<< "Logging at trace level" << traceLevel;
	BOOST_LOG_TRIVIAL(info)	<< "Threading with max " << Eigen::nbThreads()		<< " eigen threads";
#ifdef ENABLE_PARALLELISATION
	BOOST_LOG_TRIVIAL(info)	<< "Threading with max " << omp_get_max_threads()	<< " omp threads";
#endif
	BOOST_LOG_TRIVIAL(info) << "\n";
	BOOST_LOG_TRIVIAL(info) << "\n";

	//prepare the satNavMap so that it at least has entries for everything
	for (auto [sys, max] :	{	tuple<E_Sys, int>{E_Sys::GPS, NSATGPS},
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

		nav.satNavMap[Sat].id			= Sat.id();
	}

	Network pppNet;
	{
		pppNet.kfState.FilterOptions::operator=(acsConfig.pppOpts);
		pppNet.kfState.id						= "Net";
		pppNet.kfState.output_residuals			= acsConfig.output_residuals;
		pppNet.kfState.outputMongoMeasurements	= acsConfig.mongoOpts.output_measurements;

		pppNet.kfState.measRejectCallbacks	.push_back(deweightMeas);
		pppNet.kfState.measRejectCallbacks	.push_back(incrementPhaseSignalError);
		pppNet.kfState.measRejectCallbacks	.push_back(incrementReceiverError);
		pppNet.kfState.measRejectCallbacks	.push_back(pseudoMeasTest);

		pppNet.kfState.stateRejectCallbacks	.push_back(orbitGlitchReaction);	//this goes before reject by state
		pppNet.kfState.stateRejectCallbacks	.push_back(rejectByState);
	}

	Network ionNet;
	if (acsConfig.process_ionosphere)
	{
		ionNet.kfState.FilterOptions::operator=(acsConfig.ionModelOpts);
		ionNet.kfState.id						= "ION";
		ionNet.kfState.output_residuals			= acsConfig.output_residuals;
		ionNet.kfState.outputMongoMeasurements	= acsConfig.mongoOpts.output_measurements;
		ionNet.kfState.rts_basename				= "IONEX_RTS";

		ionNet.kfState.measRejectCallbacks	.push_back(deweightMeas);

		ionNet.kfState.stateRejectCallbacks	.push_back(rejectByState);
	}

	//initialise mongo
	mongoooo();

	if (acsConfig.rts_only)
	{
		pppNet.kfState.rts_basename = acsConfig.pppOpts.rts_filename;

		rtsSmoothing(pppNet.kfState, receiverMap);

		exit(0);
	}

	initialiseBias();

	boost::posix_time::ptime logptime = currentLogptime();
	createDirectories(logptime);

	reloadInputFiles();

	addDefaultBias();

	TcpSocket::startClients();

	if (acsConfig.start_epoch.is_not_a_date_time() == false)
	{
		PTime startTime;
		startTime.bigTime = boost::posix_time::to_time_t(acsConfig.start_epoch);

		tsync = startTime;
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

	BOOST_LOG_TRIVIAL(info)
	<< "\n";
	BOOST_LOG_TRIVIAL(info)
	<< "Starting to process epochs...";

	//============================================================================
	// MAIN PROCESSING LOOP														//
	//============================================================================

	GTime lastEpochStartTime;
	GTime lastEpochStopTime;

	auto atLastEpoch = [&](bool processed = false) -> bool
	{
		// Check number of epochs
		if	(  acsConfig.max_epochs	> 0
			&& epoch				>= acsConfig.max_epochs)
		{
			BOOST_LOG_TRIVIAL(info)
			<< "\n"
			<< "Exiting at epoch " << epoch
			<< " as epoch count " << acsConfig.max_epochs
			<< " has been reached";

			return true;
		}

		if (tsync == GTime::noTime())
		{
			return false;
		}

		int fractionalMilliseconds = (tsync.bigTime - (long int) tsync.bigTime) * 1000;
		auto boostTime = boost::posix_time::from_time_t((time_t)((PTime)tsync).bigTime) + boost::posix_time::millisec(fractionalMilliseconds);

		GWeek	week	= tsync;
		GTow	tow		= tsync;

		if (processed)
		{
			BOOST_LOG_TRIVIAL(info)
			<< "Processed epoch #" << epoch
			<< " - " << "GPS time: " << week << " " << std::setw(6) << tow << " - " << boostTime
			<< " (took " << (lastEpochStopTime - lastEpochStartTime) << ")";
		}

		// Check end epoch
		if	(  acsConfig.end_epoch.is_not_a_date_time() == false
			&& boostTime >= acsConfig.end_epoch)
		{
			BOOST_LOG_TRIVIAL(info)
			<< "Exiting at epoch " << epoch << " (" << boostTime
			<< ") as end epoch " << acsConfig.end_epoch
			<< " has been reached";

			return true;
		}

		return false;
	};

	// Read the observations for each station and do stuff

	bool	nextEpoch					= true;
	bool	complete					= false;					// When all input files are empty the processing is deemed complete - run until then, or until something else breaks the loop
	int		loopEpochs					= 0;						// A count of how many loops of epoch_interval this loop used up (usually one, but may be more if skipping epochs)
	auto	nominalLoopStartTime		= system_clock::now();		// The time the next loop is expected to start - if it doesnt start until after this, it may be skipped
	while (complete == false)
	{
		if (nextEpoch)
		{
			nextEpoch = false;
			epoch++;

			BOOST_LOG_TRIVIAL(info) << "\n"
			<< "Starting epoch #" << epoch;

			nominalLoopStartTime += std::chrono::milliseconds((int)(acsConfig.wait_next_epoch * 1000));

			if (tsync != GTime::noTime())
			{
				//dont obliterate the freshly configured tsync before the first epoch
				if (epoch != 1)
				{
					tsync.bigTime += acsConfig.epoch_interval;
				}

				if (fabs(tsync.bigTime - round(tsync.bigTime)) < acsConfig.epoch_tolerance)
				{
					tsync.bigTime = round(tsync.bigTime);
				}
			}

			InteractiveTerminal::clearModes(
											(string)" Processing epoch "	+ std::to_string(epoch) + "    " + tsync.to_string(),
											(string)" Last Epoch took "		+ std::to_string((lastEpochStopTime - lastEpochStartTime).to_double()) + "s");
			InteractiveTerminal::setMode(E_InteractiveMode::Syncing);

			for (auto& [id, rec] : receiverMap)
			{
				rec.ready = false;

				auto trace		= getTraceFile(rec);

				trace		<< "\n" << "------=============== Epoch " << epoch	<< " =============-----------" << "\n";
				trace		<< "\n" << "------=============== Time  " << tsync	<< " =============-----------" << "\n";
			}

			{
				auto pppTrace	= getTraceFile(pppNet);

				pppTrace	<< "\n" << "------=============== Epoch " << epoch	<< " =============-----------" << "\n";
				pppTrace	<< "\n" << "------=============== Time  " << tsync	<< " =============-----------" << "\n";
			}
		}

		// Calculate the time at which we will stop waiting for data to come in for this epoch
		auto breakTime	= nominalLoopStartTime
						+ std::chrono::milliseconds((int)(acsConfig.max_rec_latency	* 1000));

		if (loopEpochs)
		{
			BOOST_LOG_TRIVIAL(info) << "\n"
			<< "Starting epoch #" << epoch;
		}

		if (system_clock::now() > breakTime)
		{
			BOOST_LOG_TRIVIAL(warning) << "\n"
			<< "Warning: Excessive time elapsed, skipping epoch " << epoch
			<< ". Configuration 'wait_next_epoch' is " << acsConfig.wait_next_epoch;

			nextEpoch = true;

			if (atLastEpoch())
			{
				break;
			}

			continue;
		}

		//get observations from streams (allow some delay between stations, and retry, to ensure all messages for the epoch have arrived)
		map<string, bool>	dataAvailableMap;
		bool				repeat		= true;
		while	( repeat
				&&system_clock::now() < breakTime)
		{
			repeat = false;

			//load any changes from the config
			bool newConfig = acsConfig.parse();

			//make any changes to streams.
			if (newConfig)
			{
				configureUploadingStreams();
			}

			//remove any dead streams
			for (auto iter = streamParserMultimap.begin(); iter != streamParserMultimap.end(); )
			{
				auto& [id, streamParser_ptr]	= *iter;
				auto& stream					= streamParser_ptr->stream;

				auto& recOpts = acsConfig.getRecOpts(id);

				if (recOpts.kill)
				{
					BOOST_LOG_TRIVIAL(info)
					<< "Removing " << stream.sourceString << " due to kill config" << "\n";

					for (auto& [key, index] : pppNet.kfState.kfIndexMap)
					{
						if (key.str == id)
						{
							pppNet.kfState.removeState(key);

							auto nodeHandler = pppNet.kfState.kfIndexMap.extract(key);
							nodeHandler.key().rec_ptr = nullptr;
							pppNet.kfState.kfIndexMap.insert(std::move(nodeHandler));
						}
					}

					receiverMap.erase(id);

					iter = streamParserMultimap.erase(iter);

					continue;
				}

				try
				{
					auto& obsStream	= dynamic_cast<ObsStream&>(*streamParser_ptr);

					if (obsStream.hasObs())
					{
						iter++;
						continue;
					}
				}
				catch(...){}

				if (stream.isDead())
				{
					BOOST_LOG_TRIVIAL(info)
					<< "No more data available on " << stream.sourceString << "\n";

					//record as dead and erase
					streamDOAMap[stream.sourceString] = true;

					receiverMap[id].obsList.clear();

					iter = streamParserMultimap.erase(iter);

					continue;
				}

				iter++;
			}

			if (streamParserMultimap.empty())
			{
				static bool once = true;
				if (once)
				{
					once = false;

					BOOST_LOG_TRIVIAL(info)
					<< "\n";
					BOOST_LOG_TRIVIAL(info)
					<< "Inputs finished at epoch #" << epoch;
				}

				if (acsConfig.require_obs)
					complete = true;

				break;
			}

			//parse all non-observation streams
			for (auto& [id, streamParser_ptr] : streamParserMultimap)
			try
			{
				auto& obsStream = dynamic_cast<ObsStream&>(*streamParser_ptr);
			}
			catch (std::bad_cast& e)
			{
				streamParser_ptr->parse();
			}



			for (auto& [id, streamParser_ptr] : streamParserMultimap)
			{
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

				auto& recOpts = acsConfig.getRecOpts(id);

				if (recOpts.exclude)
				{
					continue;
				}

				auto& rec = receiverMap[id];

				auto trace = getTraceFile(rec);

				if (obsStream.isPseudoRec)
				{
					rec.isPseudoRec = true;
				}

				//try to get some data (again)
				if (rec.ready)
				{
					continue;
				}

				bool moreData = true;
				while (moreData)
				{
					if (acsConfig.assign_closest_epoch)	rec.obsList = obsStream.getObs(tsync, acsConfig.epoch_interval / 2);
					else								rec.obsList = obsStream.getObs(tsync, acsConfig.epoch_tolerance);

					switch (obsStream.obsWaitCode)
					{
						case E_ObsWaitCode::EARLY_DATA:								preprocessor(trace, rec);	break;
						case E_ObsWaitCode::OK:					moreData = false;	preprocessor(trace, rec);	break;
						case E_ObsWaitCode::NO_DATA_WAIT:		moreData = false;								break;
						case E_ObsWaitCode::NO_DATA_EVER:		moreData = false;								break;
					}
				}

				if (rec.obsList.empty())
				{
					//failed to get observations
					if (obsStream.obsWaitCode == +E_ObsWaitCode::NO_DATA_WAIT)
					{
						// try again later
						repeat = true;
						sleep_for(std::chrono::milliseconds(acsConfig.sleep_milliseconds));
					}

					continue;
				}

				if (tsync == GTime::noTime())
				{
					tsync = rec.obsList.front()->time.floorTime(acsConfig.epoch_interval);

					acsConfig.start_epoch	= boost::posix_time::from_time_t((time_t)((PTime)tsync).bigTime);

					if (tsync + acsConfig.epoch_tolerance < rec.obsList.front()->time)
					{
						repeat = true;
						continue;
					}
				}

				dataAvailableMap[rec.id]	= true;
				rec.ready					= true;

				auto now = system_clock::now();

				if (now >= nominalLoopStartTime)
				{
					auto nominalLatency	= now - nominalLoopStartTime;

					BOOST_LOG_TRIVIAL(debug)
					<< std::chrono::duration_cast<std::chrono::milliseconds>(nominalLoopStartTime	- peaStartTimeChrono).count() << "ms"	<< " "
					<< std::chrono::duration_cast<std::chrono::milliseconds>(now					- peaStartTimeChrono).count() << "ms"	<< " "
					<< rec.id << " nominal latency :  "
					<< std::chrono::duration_cast<std::chrono::milliseconds>(nominalLatency								).count() << "ms";
				}
				else
				{
					//this observation is earlier than expected
					//only shorten waiting periods, never extend

					auto nominalLatency	= nominalLoopStartTime - now;

					BOOST_LOG_TRIVIAL(debug)
					<< std::chrono::duration_cast<std::chrono::milliseconds>(nominalLoopStartTime	- peaStartTimeChrono).count() << "ms"	<< " "
					<< std::chrono::duration_cast<std::chrono::milliseconds>(now					- peaStartTimeChrono).count() << "ms"	<< " "
					<< rec.id << " nominal latency : -"
					<< std::chrono::duration_cast<std::chrono::milliseconds>(nominalLatency								).count() << "ms"
					<< " Advancing start time";

					auto alternateBreakTime = now + std::chrono::milliseconds((int)(acsConfig.max_rec_latency * 1000));
					auto alternateStartTime = now;

					if (alternateBreakTime < breakTime)					{	breakTime				= alternateBreakTime;	}
					if (alternateStartTime < nominalLoopStartTime)		{	nominalLoopStartTime	= alternateStartTime;	}
				}
			}
		}

		if (complete)
		{
			break;
		}

		if (tsync == GTime::noTime())
		{
			if (acsConfig.require_obs)
				continue;

			tsync = timeGet();
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Synced " << dataAvailableMap.size() << " receivers...";

		lastEpochStartTime	= timeGet();
		if	( acsConfig.require_obs		== false
			||dataAvailableMap.empty()	== false)
		{
			mainOncePerEpoch(pppNet, ionNet, receiverMap, tsync);
		}
		lastEpochStopTime	= timeGet();


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
	TcpSocket::ioService.stop();

	GTime peaInterTime = timeGet();
	BOOST_LOG_TRIVIAL(info)
	<< "\n"
	<< "PEA started  processing at : " << peaStartTime << "\n"
	<< "and finished processing at : " << peaInterTime << "\n"
	<< "Total processing duration  : " << (peaInterTime - peaStartTime) << "\n" << "\n";

	BOOST_LOG_TRIVIAL(info)
	<< "\n"
	<< "Finalising streams and post processing...";

	mainPostProcessing(pppNet, ionNet, receiverMap);

	GTime peaStopTime = timeGet();
	BOOST_LOG_TRIVIAL(info)
	<< "\n"
	<< "PEA started  processing at : " << peaStartTime	<< "\n"
	<< "and finished processing at : " << peaStopTime	<< "\n"
	<< "Total processing duration  : " << (peaStopTime - peaStartTime) << "\n" << "\n";

	InteractiveTerminal::clearModes(
									(string)" Processing complete at epoch "	+ std::to_string(epoch) + "    " + tsync.to_string(),
									(string)" Processing took "					+ std::to_string((peaStopTime - peaStartTime).to_double()) + "s");
	InteractiveTerminal::setMode(E_InteractiveMode::Complete);

	BOOST_LOG_TRIVIAL(info) << "PEA finished";

	while (InteractiveTerminal::enabled)
	{
		sleep_for(std::chrono::seconds(10));
	}

	return EXIT_SUCCESS;
}
