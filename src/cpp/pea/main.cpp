#include <unordered_map>
#include <sys/time.h>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <memory>
#include <chrono>
#include <thread>
#include <string>

#include "omp.h"

using namespace std::literals::chrono_literals;
using std::chrono::system_clock;
using std::chrono::time_point;
using std::this_thread::sleep_for;
using std::string;

#include <boost/log/utility/setup/console.hpp>
#include <boost/log/trivial.hpp>
#include <boost/filesystem.hpp>


#include "ntripCasterService.hpp"
#include "acsNtripBroadcast.hpp"
#include "minimumConstraints.hpp"
#include "networkEstimator.hpp"
#include "peaCommitVersion.h"
#include "algebraTrace.hpp"
#include "rtsSmoothing.hpp"
#include "corrections.hpp"
#include "streamTrace.hpp"
#include "writeClock.hpp"
#include "GNSSambres.hpp"
#include "acsConfig.hpp"
#include "acsStream.hpp"
#include "ntripSocket.hpp"
#include "testUtils.hpp"
#include "biasSINEX.hpp"
#include "ionoModel.hpp"
#include "station.hpp"
#include "summary.hpp"
#include "antenna.hpp"
#include "preceph.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "orbits.hpp"
#include "gTime.hpp"
#include "mongo.hpp"
#include "debug.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "snx.hpp"
#include "trop.h"
#include "vmf3.h"

nav_t		nav		= {};
int			epoch	= 0;
GTime		tsync	= GTime::noTime();

// Used for stream specific tracing.
std::multimap	<string, std::shared_ptr<NtripRtcmStream>>	ntripRtcmMultimap;
std::multimap	<string, ACSObsStreamPtr>	obsStreamMultimap;
std::multimap	<string, ACSNavStreamPtr>	navStreamMultimap;
std::map		<string, bool>				streamDOAMap;
NtripBroadcaster outStreamManager;

#ifdef ENABLE_MONGODB
	Mongo*	mongo_ptr = nullptr;
#endif

void removeUnprocessed(
	ObsList&	obsList)
{
	for (auto& obs : obsList)
	{
		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			obs.excludeSystem = true;
		}
	}
}

void dumpConfig(Trace& trace)
{
	trace << "+ RAW CONFIG" << std::endl;

	std::ifstream config(acsConfig.configFilename);

	string str;
	while (std::getline(config, str))
	{
		trace << str << std::endl;
	}

	trace << "- RAW CONFIG" << std::endl;
}

bool fileChanged(string filename)
{
	bool valid = checkValidFile(filename);
	if (valid == false)
	{
		return false;
	}
	
	auto writeTime = boost::filesystem::last_write_time(filename);
	auto& readTime = acsConfig.configModifyTimeMap[filename];
	if (readTime == writeTime)
	{
		return false;
	}
	readTime = writeTime;
	return true;
}	

void removeInvalidFiles(
	vector<string>& files)
{
	for (auto it = files.begin(); it != files.end(); )
	{
		auto& filename = *it;
		bool valid = checkValidFile(filename);
		if (valid == false)
		{
			it = files.erase(it);
		}
		else
		{
			it++;
		}
	}
}

void recordNetworkStatistics(std::multimap<std::string, std::shared_ptr<NtripRtcmStream>> downloadStreamMap )
{
	string netStreamFilename = acsConfig.trace_directory + "NetworkStatistics.json";
	std::ofstream netStream(netStreamFilename, std::ofstream::out | std::ofstream::ate);
	
	if (!netStream)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not open trace file for network statistics at " << netStreamFilename;
		return;
	}	
	
	std::vector<std::string> dataJSON;
	std::string epochData = "\"epochData\": [";
	std::string hourData = "\"hourData\": [";
	std::string runData = "\"runData\": [";
	std::string connData = "\"epochConnData\": [";
				
	bool isFirstEntry = true;
	for (auto& [id, s] : downloadStreamMap )
	{
		NtripRtcmStream& downStream = *s;

		if( isFirstEntry )
			isFirstEntry = false;
		else
		{
			epochData += ",";
			hourData += ",";
			runData += ",";
			connData += ",";
		}
		dataJSON = downStream.getJsonNetworkStatistics(tsync);
		epochData += dataJSON[0];
		hourData += dataJSON[1];
		runData +=  dataJSON[2];
		connData +=  dataJSON[3];
	}
	epochData += "]";
	hourData += "]";
	runData += "]";
	connData += "]";
	netStream << "{" << epochData << "," << hourData << "," 
						<< runData << "," << connData << "}";	
}

void reloadInputFiles()
{
	removeInvalidFiles(acsConfig.atxfiles);
	for (auto& atxfile : acsConfig.atxfiles)
	{
		if (fileChanged(atxfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading ATX file " << atxfile;

		bool pass = readantexf(atxfile, nav);
		if (pass == false)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Unable to load ATX from file " << atxfile;

			continue;
		}
	}

	removeInvalidFiles(acsConfig.erpfiles);
	for (auto& erpfile : acsConfig.erpfiles)
	{
		if (fileChanged(erpfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading ERP file " << erpfile;

		readerp(erpfile, &nav.erp);
	}

	removeInvalidFiles(acsConfig.orbfiles);
	removeInvalidFiles(acsConfig.sp3files);
	if (acsConfig.orbfiles.size() > 0)
	{
		bool updated = false;

		/* orbit info from orbit file */
		for (auto& orbfile : acsConfig.orbfiles)
		{
			if (fileChanged(orbfile) == false)
			{
				continue;
			}

			updated = true;

			BOOST_LOG_TRIVIAL(info)
			<< "Reading in the orbit file" << orbfile << std::endl;

			readorbit(orbfile, nav.orbpod);
		}

		if (updated)
		{
			orb2sp3(nav);
		}
	}
	else
	{
		for (auto& sp3file : acsConfig.sp3files)
		{
			if (fileChanged(sp3file) == false)
			{
				continue;
			}

			BOOST_LOG_TRIVIAL(info)
			<< "Loading SP3 file " << sp3file << std::endl;

			nav.pephMap.clear();
			readsp3(sp3file, &nav, 0);
		}
	}

	removeInvalidFiles(acsConfig.navfiles);
	for (auto& navfile : acsConfig.navfiles)
	{
		if (fileChanged(navfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading NAV file " << navfile;

		FileRinexStream rinexStream(navfile);

		rinexStream.parse();
	}

	removeInvalidFiles(acsConfig.clkfiles);
	for (auto& clkfile : acsConfig.clkfiles)
	{
		if (fileChanged(clkfile) == false)
		{
			continue;
		}

		/* CLK file - RINEX 3 */
		BOOST_LOG_TRIVIAL(info)
		<< "Loading CLK file " << clkfile;

		FileRinexStream rinexStream(clkfile);

		rinexStream.parse();
	}

	removeInvalidFiles(acsConfig.dcbfiles);
	for (auto& dcbfile : acsConfig.dcbfiles)
	{
		if (fileChanged(dcbfile) == false)
		{
			continue;
		}

		/* DCB file */
		BOOST_LOG_TRIVIAL(info)
		<< "Loading DCB file " << dcbfile;
		
		readdcb(dcbfile, &nav); 
	}

	removeInvalidFiles(acsConfig.bsxfiles);
	for (auto& bsxfile : acsConfig.bsxfiles)
	{
		if (fileChanged(bsxfile) == false)
		{
			continue;
		}

		/* BSX file*/
		BOOST_LOG_TRIVIAL(info)
		<< "Loading BSX file " << bsxfile;

		read_bias_SINEX(bsxfile);
	}

	removeInvalidFiles(acsConfig.ionfiles);
	for (auto& ionfile : acsConfig.ionfiles)
	{
		if (fileChanged(ionfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading ION file " << ionfile;

		readtec(ionfile, &nav, 0, nullptr);
	}

	static bool once = true;
	removeInvalidFiles(acsConfig.snxfiles);
	for (auto& snxfile : acsConfig.snxfiles)
	{
		if (fileChanged(snxfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading SNX file " <<  snxfile << std::endl;

		bool fail = read_sinex(snxfile, once);
		if (fail)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Unable to load SINEX file " << snxfile;

			continue;
		}
		
		once = false;
	}
}

template<typename T>
std::ofstream getTraceFile(
	T& thing)
{
	if (thing.traceFilename.empty())
	{
		return std::ofstream();
	}
	
	std::ofstream trace(thing.traceFilename, std::ios::app);
	if (!trace)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not open trace file for " << thing.id << " at " << thing.traceFilename;
	}
	
	return trace;
}

void createNewTraceFile(
	const	string& id,
			string& logtime,
			string  new_path_trace,
			string& old_path_trace,
			string  suffix,
			bool	outputHeader,
			bool	outputConfig)
{
	replaceString(new_path_trace, "<STATION>", id);
	replaceString(new_path_trace, "<LOGTIME>", logtime);

	// Create the trace file if its a new filename, otherwise, keep the old one
	if (new_path_trace == old_path_trace)
	{
		//the filename is the same, keep using the old ones
		return;
	}
			
	old_path_trace = new_path_trace;
	
	string suffixedPath = old_path_trace + suffix;
	
	BOOST_LOG_TRIVIAL(debug)
	<< "\tCreating new file for " << id << " at " << suffixedPath;
	
	ofstream trace(suffixedPath);
	if (!trace)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not create file for " << id << " at " << suffixedPath;
		
		return;
	}
	
	// Trace file head
	if (outputHeader)
	{
		trace << "station    : " << id << std::endl;
		trace << "start_epoch: " << acsConfig.start_epoch		<< std::endl;
		trace << "end_epoch  : " << acsConfig.end_epoch			<< std::endl;
		trace << "trace_level: " << acsConfig.trace_level		<< std::endl;
		trace << "pea_version: " << PEA_COMMIT_VERSION			<< std::endl;
		trace << "rts_lag    : " << acsConfig.pppOpts.rts_lag	<< std::endl;
	}
	
	if (outputConfig)
	{
		dumpConfig(trace);
	}
}

void createTracefiles(
	map<string, Station>&	stationMap,
	Network&				net)
{
	GTime traceTime = tsync;
	//round to nearest chunk by integer arithmetic
	long int roundTime = traceTime.time;
	roundTime /= acsConfig.trace_rotate_period;
	roundTime *= acsConfig.trace_rotate_period;
	traceTime.time = roundTime;
	string logtime = traceTime.to_string(0);
	std::replace( logtime.begin(), logtime.end(), '/', '-');
	
	if (acsConfig.output_trace)
	for (auto& [id, rec] : stationMap)
	{
		createNewTraceFile(id,		logtime, acsConfig.trace_filename,						rec.traceFilename,				"",					true, acsConfig.output_config);
	}
	
	if	( acsConfig.process_rts
		&&acsConfig.pppOpts.rts_lag)
	for (auto& [id, rec] : stationMap)
	{
		createNewTraceFile(id,		logtime, acsConfig.pppOpts.rts_filename,				rec.rtk.pppState.rts_filename,	SMOOTHED_SUFFIX,	true, acsConfig.output_config);
	}
	
	if (acsConfig.output_summary)
	{
		createNewTraceFile(net.id,	logtime, acsConfig.summary_filename,					net.traceFilename,				"",					true, acsConfig.output_config);
	}
	
	if (acsConfig.output_clocks)
	{
		createNewTraceFile(net.id,	logtime, acsConfig.clocks_filename,						net.clockFilename,				"",					false, false);
	}
	
	if	( acsConfig.process_rts
		&&acsConfig.netwOpts.rts_lag)
	if (acsConfig.output_clocks)
	{
		createNewTraceFile(net.id,	logtime, acsConfig.clocks_filename + SMOOTHED_SUFFIX,	net.rtsClockFilename,			"",					false, false);
	}
	
	for (auto& [id, upStream_ptr] : outStreamManager.ntripUploadStreams)
	{ 
		NtripBroadcaster::NtripUploadClient& upStream = *upStream_ptr;
		upStream.ntripTrace.level_trace = level_trace;
		string path_trace = acsConfig.trace_filename;
		replaceString(path_trace, "<STATION>", id + "-UPLOAD");
		replaceString(path_trace, "<LOGTIME>", logtime);
		BOOST_LOG_TRIVIAL(debug)
		<< "\tCreating trace file for stream " << id;		
		upStream.traceFilename	= path_trace;
		auto trace = getTraceFile(upStream);

		// Trace file head
		trace << "station    : " << id << std::endl;
		trace << "start_epoch: " << acsConfig.start_epoch << std::endl;
		trace << "end_epoch  : " << acsConfig.end_epoch   << std::endl;
		trace << "trace_level: " << acsConfig.trace_level << std::endl;
		trace << "pea_version: " << PEA_COMMIT_VERSION    << std::endl;

		if (acsConfig.output_config)
		{
			dumpConfig(trace);
		}
	}
}

void mainOncePerEpochPerStation(
	Station&	rec,
	double*		orog,
	gptgrid_t&	gptg)
{
	TestStack ts(rec.id);

	auto trace = getTraceFile(rec);

	trace << std::endl << "################# Starting Epoch " << epoch << " << ############" << std::endl;

	BOOST_LOG_TRIVIAL(debug)
	<< "\tRead " << rec.obsList.size()
	<< " observations for station " << rec.id;

	removeUnprocessed(rec.obsList);

	sinexPerEpochPerStation(tsync, rec);

	/* linear combinations */
	for (auto& obs : rec.obsList)
	{
		obs.satStat_ptr->lc_pre = obs.satStat_ptr->lc_new;
		obs.satStat_ptr->lc_new = {};
	}
	obs2lcs		(trace,	rec.obsList);

	/* cycle slip/clock jump detection and repair */
	detectslips	(trace,	rec.obsList);
	detectjump	(trace,	rec.obsList, acsConfig.elevation_mask, rec.cj);

	for (auto& obs			: rec.obsList)
	for (auto& [ft, Sig]	: obs.Sigs)
	{
		if (obs.satStat_ptr->sigStatMap[ft].slip.any)
		{
			rec.slipCount++;
			break;
		}
	}

	GTime prevTime = rec.rtk.sol.time;

	//do a spp on the observations
	sppos(trace, rec.obsList, rec.rtk.sol);
	if (prevTime.time != 0)
	{
		rec.rtk.tt = timediff(rec.rtk.sol.time, prevTime);
	}
	
	if (acsConfig.process_ionosphere)
	{
		bool sppUsed;
		selectAprioriSource(rec,sppUsed);
		if(rec.aprioriPos.norm()) update_receivr_measr(trace, rec);
	}

	/* exclude measurements of eclipsing satellite (block IIA) */
	if (acsConfig.reject_eclipse)
	{
		testeclipse(rec.obsList);
	}

	if (acsConfig.process_user)
	{
		pppos(trace, rec.rtk, rec.obsList, rec);
		
		if	(  rec.rtk.sol.stat 			!= SOLQ_NONE 
			&& acsConfig.ambrOpts.WLmode	!= E_ARmode::OFF)
		{
			KFState kfARcopy = rec.rtk.pppState;
			
			int nfixed = enduserAmbigResl(trace, rec.obsList, kfARcopy);
			if (nfixed>0)
			{
				trace << std::endl << "-------------- AR PPP solution ----------------------" << std::endl;
				pppoutstat(trace, kfARcopy,false,SOLQ_FIX,rec.rtk.sol.numSats);
				trace << std::endl << "------------ AR PPP solution end --------------------" << std::endl;
			}
		}

		// Compare estimated station position with benchmark in SINEX file
		if (acsConfig.output_ppp_sol)
		{
			Vector3d snxPos = rec.snx.pos;
			Vector3d estPos = rec.rtk.sol.pppRRec;
			Vector3d diffEcef = snxPos - estPos;
			double latLonHt[3];
			ecef2pos(snxPos, latLonHt); // rad,rad,m
			double diffEcefArr[3];
			Vector3d::Map(diffEcefArr, diffEcef.rows())	= diffEcef; // equiv. to diffEcef = diff
			double diffEnuArr[3];
			ecef2enu(latLonHt, diffEcefArr, diffEnuArr);
			Vector3d diffEnu;
			diffEnu = Vector3d::Map(diffEnuArr, diffEnu.rows());

			std::ofstream fout(acsConfig.ppp_sol_filename, std::ios::out | std::ios::app);
			if (!fout)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Could not open trace file for PPP solution at " << acsConfig.ppp_sol_filename;
			}
			else
			{
				fout << epoch << " ";
				fout << rec.id << " ";
				fout << snxPos.transpose() << " ";
				fout << estPos.transpose() << " ";
				fout << diffEcef.transpose() << " ";
				fout << diffEnu.transpose() << " ";
				fout << std::endl;
			}
		}
	}

	if (acsConfig.process_network)
	for (auto once : {1})
	{
		// If there is no antenna information skip processing this station
		if (rec.snx.anttype.empty()) 
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "\tNo Antenna Information for " << rec.id
			<< " skipping this station";

			break;
		}

		// if the vmf3 option has been selected, use it, otherwise send null
		vmf3_t*	rtkVmf3_ptr = nullptr;
		double*	rtkOrog_ptr = nullptr;
		if	(!acsConfig.tropOpts.vmf3dir.empty())
		{
			rtkVmf3_ptr = &rec.vmf3;
			rtkOrog_ptr = orog;
		}

		/* observed minus computed for each satellites */
		pppomc(trace, rec.rtk, rec.obsList, gptg, rec.cj, rec, rtkVmf3_ptr, rtkOrog_ptr);
	}

	if	( (acsConfig.process_rts)
		&&(acsConfig.pppOpts.rts_lag > 0)
		&&(epoch > acsConfig.pppOpts.rts_lag))
	{
		KFState rts = RTS_Process(rec.rtk.pppState);
		std::ofstream rtsTrace(rec.rtk.pppState.rts_filename + SMOOTHED_SUFFIX, std::ofstream::out | std::ofstream::app);
		pppoutstat(rtsTrace, rts);

#		ifdef ENABLE_MONGODB
		if	( acsConfig.process_user
			&&acsConfig.output_mongo_states)
		{
			mongoStates(rts, "RTS_");
		}
#		endif
	}

#	ifdef ENABLE_MONGODB
	if (acsConfig.output_mongo_measurements)
	{
		mongoMeasSatStat(rec.obsList);
	}

	if	( acsConfig.process_user
		&&acsConfig.output_mongo_states)
	{
		mongoStates(rec.rtk.pppState);
	}
#	endif
}

void mainOncePerEpoch(
	Network&		net,
	StationList&	epochStations,
	GTime			tsync)
{
	TestStack ts("1/Ep");
	double ep[6] = {};
	time2epoch(tsync, ep);		//todo aaron, change to string function too

	auto netTrace = getTraceFile(net);
	
	if (acsConfig.ssrOpts.calculate_ssr)
	{
		initSsrOut();
	}
	
	if (acsConfig.process_network)
	{
		netTrace << std::endl << "------=============== " << epoch << " =============-----------" << std::endl;

		networkEstimator(netTrace, epochStations, net.kfState, tsync);

		if	( acsConfig.output_AR_clocks == false 
			||ARsol_ready() == false) 
		{
			net.kfState.outputStates(netTrace);
		}
		
#		ifdef ENABLE_MONGODB
		if (acsConfig.output_mongo_states)
		{
			mongoStates(net.kfState);
		}
#		endif

		// Output clock product body part
		if	(acsConfig.output_clocks)
		{
			outputClockfileHeader(net.clockFilename, net.kfState, ep);
			
			if	(acsConfig.output_AR_clocks == false 
				||ARsol_ready() == false)
			{
				outputReceiverClocks (net.clockFilename, net.kfState, ep);
				outputSatelliteClocks(net.clockFilename, net.kfState, ep);
			}
		}

		if	(  acsConfig.ambrOpts.WLmode!=E_ARmode::OFF
			|| acsConfig.ambrOpts.NLmode!=E_ARmode::OFF)
		{
			/* Instantaneous AR */
			KF_ARcopy = net.kfState;
		
			networkAmbigResl(netTrace, epochStations, KF_ARcopy);

			if (acsConfig.output_AR_clocks  && ARsol_ready())
			{
				KF_ARcopy.outputStates(netTrace);
			}
			
#			ifdef ENABLE_MONGODB
			if (acsConfig.output_mongo_states)
				mongoStates(KF_ARcopy, "AR_");
#			endif
			
			if (acsConfig.output_AR_clocks && ARsol_ready())
			{
				outputReceiverClocks (net.clockFilename, KF_ARcopy, ep);
				outputSatelliteClocks(net.clockFilename, KF_ARcopy, ep);
			}
		}

		if	( (acsConfig.process_rts)
			&&(acsConfig.netwOpts.rts_lag > 0)
			&&(epoch > acsConfig.netwOpts.rts_lag))
		{
			KFState rts = RTS_Process(net.kfState, false);
			
			std::ofstream rtsTrace(net.kfState.rts_filename + SMOOTHED_SUFFIX, std::ofstream::out | std::ofstream::app);
			
			if (rts.time != GTime::noTime())
			{
				pppoutstat(rtsTrace, rts);
			}
			
			if	(acsConfig.output_clocks)
			{
				outputClockfileHeader(net.rtsClockFilename, rts, ep);
				outputReceiverClocks (net.rtsClockFilename, rts, ep);
				outputSatelliteClocks(net.rtsClockFilename, rts, ep);
			}

#			ifdef ENABLE_MONGODB
			if	( acsConfig.output_mongo_states)
			{
				mongoStates(rts, "RTS_");
			}
#			endif
		}

		if (acsConfig.ssrOpts.calculate_ssr)
		{
			std::set<SatSys> sats;	// List of satellites visible in this epoch
			for (auto& rec_ptr	: epochStations)
			for (auto& obs	: rec_ptr->obsList)
				sats.insert(obs.Sat);
			
			calcSsrCorrections(netTrace, sats, tsync);
		
			if (acsConfig.ssrOpts.upload_to_caster)
				outStreamManager.sendMessages(true);
			else
				rtcmEncodeToFile(epoch);
			
			if (acsConfig.ssrOpts.sync_epochs)
				exportSyncFile(epoch);
		}
	}

	if (acsConfig.process_ionosphere)
	{
		update_ionosph_model(netTrace, epochStations, tsync);
	}

	if (acsConfig.output_persistance)
	{
		outputPersistanceNav();
	}

	TestStack::saveData();
	TestStack::testStatus();
}

void mainPostProcessing(
	Network&				net,
	map<string, Station>&	stationMap)
{
	auto netTrace = getTraceFile(net);
	
	if	(  acsConfig.process_ionosphere 
		&& acsConfig.output_ionex)
	{
		ionex_file_write(netTrace, tsync, true);
	}

	if	( acsConfig.process_network 
		&&acsConfig.process_minimum_constraints)
	{
		BOOST_LOG_TRIVIAL(info)
		<< std::endl
		<< "---------------PROCESSING NETWORK WITH MINIMUM CONSTRAINTS ------------- " << std::endl;

		minimum(netTrace, net.kfState);
		net.kfState.outputStates(netTrace);
		
		if (acsConfig.output_AR_clocks)
		{
			minimum(netTrace, KF_ARcopy);
			KF_ARcopy.outputStates(netTrace);
		}
	}

	if (acsConfig.output_sinex)
	{
		sinexPostProcessing(tsync, stationMap, net.kfState);
	}

	if (acsConfig.output_persistance)
	{
		BOOST_LOG_TRIVIAL(info)
		<< "Storing persistant states to continue processing...";

		outputPersistanceStates(stationMap, net.kfState);
	}

	if (acsConfig.process_network)
	{
		if	(acsConfig.output_AR_clocks)	outputOrbit(KF_ARcopy);
		else								outputOrbit(net.kfState);
	}
	
	if	(acsConfig.process_rts)
	{
		if (acsConfig.pppOpts.rts_lag < 0)
		for (auto& [id, rec] : stationMap)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------PROCESSING PPP WITH RTS------------------------- " << std::endl;

			RTS_Process(rec.rtk.pppState, true);

			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------OUTPUTTING PPP RTS RESULTS---------------------- " << std::endl;

			RTS_Output(rec.rtk.pppState);
		}

		if (acsConfig.netwOpts.rts_lag < 0)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------PROCESSING NETWORK WITH RTS--------------------- " << std::endl;

			RTS_Process(net.kfState, true);

			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------OUTPUTTING NETWORK RTS RESULTS------------------ " << std::endl;

			RTS_Output(net.kfState, net.rtsClockFilename);
		}

		if (acsConfig.ionFilterOpts.rts_lag < 0)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------PROCESSING IONOSPHERE WITH RTS------------------ " << std::endl;

			RTS_Process(iono_KFState, true);

			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------OUTPUTTING IONOSPHERE RTS RESULTS--------------- " << std::endl;

			RTS_Output(iono_KFState);
		}
	}

	if (acsConfig.output_biasSINEX)
	{
		bias_io_opt biaopt;
		biaopt.OSB_biases = acsConfig.ambrOpts.writeOSB;
		biaopt.DSB_biases = acsConfig.ambrOpts.writeDSB;
		biaopt.SSR_biases = false;
		biaopt.SAT_biases = acsConfig.ambrOpts.writeSATbias;
		biaopt.REC_biases = acsConfig.ambrOpts.writeRecBias;
		biaopt.COD_biases = true;
		biaopt.PHS_biases = true;	
	
		write_bias_SINEX(netTrace, tsync, acsConfig.biasSINEX_filename, biaopt);
	}
	
	if 	(  acsConfig.ambrOpts.WLmode != E_ARmode::OFF 
		|| acsConfig.ambrOpts.NLmode != E_ARmode::OFF) 
	{
		net_sect_out(netTrace);
	}
	
	TestStack::printStatus(true);

	if (acsConfig.output_summary)
	{
		//todo aaron # what processing options we enabled
		outputSummaries(stationMap);
	}
}

void testRtcmEncodeDecode();

int main(int argc, char **argv)
{
	tracelevel(5);

	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::info);
	boost::log::add_console_log(std::cout, boost::log::keywords::format = "%Message%");

	BOOST_LOG_TRIVIAL(info)
	<< "PEA starting... (" << PEA_BRANCH_NAME << " v" PEA_COMMIT_VERSION " from " << PEA_COMMIT_DATE << ")" << std::endl;

	auto peaStartTime = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));

	if (!configure(argc, argv))
	{
		BOOST_LOG_TRIVIAL(error) 	<< "Incorrect configuration";
		BOOST_LOG_TRIVIAL(info) 	<< "PEA finished";
#ifndef	ENABLE_UNIT_TESTS
		NtripSocket::io_service.stop();
#endif
		return EXIT_FAILURE;
	}

	doDebugs();

	#	ifdef ENABLE_MONGODB
	{
		mongoooo();
	}
	#	endif

	TestStack::openData();


	BOOST_LOG_TRIVIAL(info)
	<< "Threading with " << Eigen::nbThreads()
	<< " threads" << std::endl;

	// Ensure the output directories exist
	if (!acsConfig.summary_directory		.empty())	boost::filesystem::create_directories(acsConfig.summary_directory);
	if (!acsConfig.trace_directory			.empty())	boost::filesystem::create_directories(acsConfig.trace_directory);
	if (!acsConfig.clocks_directory			.empty())	boost::filesystem::create_directories(acsConfig.clocks_directory);
	if (!acsConfig.ionex_directory			.empty())	boost::filesystem::create_directories(acsConfig.ionex_directory);
	if (!acsConfig.ionstec_directory		.empty())	boost::filesystem::create_directories(acsConfig.ionstec_directory);
	if (!acsConfig.biasSINEX_directory		.empty())	boost::filesystem::create_directories(acsConfig.biasSINEX_directory);
	if (!acsConfig.sinex_directory			.empty())	boost::filesystem::create_directories(acsConfig.sinex_directory);
	if (!acsConfig.testOpts.directory		.empty())	boost::filesystem::create_directories(acsConfig.testOpts.directory);
	if (!acsConfig.ssrOpts.rtcm_directory	.empty())	boost::filesystem::create_directories(acsConfig.ssrOpts.rtcm_directory);
	if (!acsConfig.ppp_sol_directory		.empty())	boost::filesystem::create_directories(acsConfig.ppp_sol_directory);
	if (!acsConfig.persistance_directory	.empty())	boost::filesystem::create_directories(acsConfig.persistance_directory);

	BOOST_LOG_TRIVIAL(info)
	<< "Logging with trace level:" << acsConfig.trace_level << std::endl << std::endl;

	tracelevel(acsConfig.trace_level);
	
	if ( acsConfig.caster_test )
	{
		// This struct stops normal operation of the PEA and puts it into
		// caster testing mode and sets up the stream performance web service.

		
		// Existing streams from the configuration are not required. To remove them
		// the networking is started and shutdown as work for the network thread gets
		// queued when the stream is created.
#ifndef	ENABLE_UNIT_TESTS
		NtripSocket::startClients();
#endif
		for (auto& [id, s] : ntripRtcmMultimap)
		{
			NtripStream& downStream = *s;
			downStream.disconnect();
		}
		outStreamManager.stopBroadcast();
		
		ntripRtcmMultimap.clear();
		obsStreamMultimap.clear();
		outStreamManager.ntripUploadStreams.clear();
		

		NtripCasterService casterService;   
		casterService.caster_stream_root = acsConfig.caster_stream_root;
		casterService.startPerformanceMonitoring();

#ifndef	ENABLE_UNIT_TESTS
		NtripSocket::io_service.stop();
#endif
		exit(0);
	}
	
	for (auto& [id, s] : ntripRtcmMultimap )
	{
		NtripRtcmStream& downStream = *s;
		downStream.ntripTrace.level_trace = acsConfig.trace_level;
		downStream.print_stream_statistics = acsConfig.print_stream_statistics;
	}

	for (auto& [id, s] : outStreamManager.ntripUploadStreams)
	{
		NtripBroadcaster::NtripUploadClient& uploadStream = *s;
		uploadStream.ntripTrace.level_trace = acsConfig.trace_level;
		uploadStream.print_stream_statistics = acsConfig.print_stream_statistics;
	}
	
	double		orog[NGRID] = {};	 		/* vmf3 orography information, config->orography */
	gptgrid_t	gptg		= {};			/* gpt grid information */


	//read orography file for VMF3
	{
		int orog_sucess = false;
		if 	( acsConfig.tropOpts.orography	.empty() == false
			&&acsConfig.tropOpts.vmf3dir	.empty() == false)
		{
			orog_sucess = readorog(acsConfig.tropOpts.orography, orog);
		}

		if (orog_sucess == false)
		{
			// read gpt2 grid file
			readgrid(acsConfig.tropOpts.gpt2grid, &gptg);
		}
	}

	if (acsConfig.input_persistance)
	{
		BOOST_LOG_TRIVIAL(info)
		<< "Loading persistant navigation object to continue processing...";

		inputPersistanceNav();
	}

	if (acsConfig.process_ionosphere)
	{
		bool pass = config_ionosph_model();
		if	(pass == false)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error in Ionosphere Model configuration";

			return EXIT_FAILURE;
		}
	}

	map<string, Station> stationMap;

	BOOST_LOG_TRIVIAL(debug)
	<< "\tCreating trace files ";

	if (acsConfig.output_biasSINEX)
	{
		std::ofstream(acsConfig.biasSINEX_filename);
	}
	if (acsConfig.output_ppp_sol)
	{
		std::ofstream(acsConfig.ppp_sol_filename);
	}

	Network net;
	{
		net.kfState.max_filter_iter		= acsConfig.netwOpts.max_filter_iter;
		net.kfState.max_prefit_remv		= acsConfig.netwOpts.max_prefit_remv;
		net.kfState.inverter			= acsConfig.netwOpts.inverter;
		net.kfState.output_residuals	= acsConfig.output_residuals;
		net.kfState.rejectCallbacks.push_back(deweightMeas);
		net.kfState.rejectCallbacks.push_back(incrementPhaseSignalError);
	}

	if (acsConfig.process_rts)
	{
		if (acsConfig.netwOpts.rts_lag)
		{
			initFilterTrace(net.kfState,		acsConfig.netwOpts.rts_filename,		"Network",		acsConfig.netwOpts.rts_lag);
		}

		if (acsConfig.ionFilterOpts.rts_lag)
		{
			initFilterTrace(iono_KFState,		acsConfig.ionFilterOpts.rts_filename,	"Ionosphere",	acsConfig.ionFilterOpts.rts_lag);
		}
	}

	if (acsConfig.input_persistance)
	{
		BOOST_LOG_TRIVIAL(info)
		<< "Loading persistant states to continue processing...";

		inputPersistanceStates(stationMap, net.kfState);
	}

	acsConfig.parse();
	reloadInputFiles();
	
	if (acsConfig.start_epoch.is_not_a_date_time() == false)
	{
		tsync.time = boost::posix_time::to_time_t(acsConfig.start_epoch);
	}

	BOOST_LOG_TRIVIAL(info)
	<< std::endl;
	BOOST_LOG_TRIVIAL(info)
	<< "Starting to process epochs...";

	TestStack ts(acsConfig.config_description);

#ifndef	ENABLE_UNIT_TESTS
	NtripSocket::startClients();
#endif

	//============================================================================
	// MAIN PROCESSING LOOP														//
	//============================================================================

	// Read the observations for each station and do stuff
	bool complete = false;
	int loopEpochs = 1;
	auto nextNominalLoopStartTime = system_clock::now();
	while (complete == false)
	{
		if (tsync != GTime::noTime())
		{
			tsync.time				+= loopEpochs * acsConfig.epoch_interval;
		}
		
		epoch						+= loopEpochs;
		nextNominalLoopStartTime	+= loopEpochs * std::chrono::milliseconds((int)(acsConfig.wait_next_epoch * 1000));
		
		auto breakTime	= nextNominalLoopStartTime
						+ std::chrono::milliseconds((int)(acsConfig.wait_all_stations	* 1000));
						
		BOOST_LOG_TRIVIAL(info)
		<< std::endl;
		BOOST_LOG_TRIVIAL(info)
		<< "Starting epoch #" << epoch;

		TestStack ts("Epoch " + std::to_string(epoch));

		StationList epochStations;
		
		//get observations from streams (allow some delay between stations, and retry, to ensure all messages for the epoch have arrived)
		bool foundFirst	= false;
		bool repeat		= true;
		while	( repeat
				&&system_clock::now() < breakTime)
		{
			repeat = false;

			for (auto& [id, s] : navStreamMultimap)
			{
				NavStream& navStream = *s;	
				
				navStream.getNav();
			}
			
			//remove any dead streams
			for (auto iter = obsStreamMultimap.begin(); iter != obsStreamMultimap.end(); )
			{
				ObsStream& obsStream = *iter->second;

				if (obsStream.isDead())
				{
					BOOST_LOG_TRIVIAL(info)
					<< "No more data available on " << obsStream.sourceString << std::endl;
					
					//record as dead and erase
					streamDOAMap[obsStream.sourceString] = true;
					
					iter = obsStreamMultimap.erase(iter);
				}
				else
				{
					iter++;
				}
			}

			if (obsStreamMultimap.empty())
			{
				BOOST_LOG_TRIVIAL(info)
				<< std::endl;
				BOOST_LOG_TRIVIAL(info)
				<< "Inputs finished at epoch #" << epoch;

				complete = true;

				break;
			}

			for (auto& [id, s] : obsStreamMultimap)
			{
				ObsStream&	obsStream	= *s;				
				
				auto& recOpts = acsConfig.getRecOpts(id);

				if (recOpts.exclude)
				{
					continue;
				}
				
				auto& rec = stationMap[id];

				if	( (rec.obsList.size() > 0)
					&&(rec.obsList.front().time == tsync))
				{
					//already have observations for this epoch.
					continue;
				}
				
				//try to get some data
				rec.obsList = obsStream.getObs(tsync);

				if (rec.obsList.empty())
				{
					//failed to get observations, try again later
					repeat = true;
					sleep_for(1ms);
					continue;
				}

				if (foundFirst == false)
				{
					foundFirst = true;
					
					//first observation found for this epoch, give any other stations some time to get their observations too
					//only shorten waiting periods, never extend
					auto now = system_clock::now();
					
					auto alternateBreakTime = now + std::chrono::milliseconds((int)(acsConfig.wait_all_stations	* 1000));
					auto alternateStartTime = now + std::chrono::milliseconds((int)(acsConfig.wait_next_epoch	* 1000));
					
					if (alternateBreakTime < breakTime)						{	breakTime					= alternateBreakTime;	}
					if (alternateStartTime < nextNominalLoopStartTime)		{	nextNominalLoopStartTime	= alternateStartTime;	}
				}

				//initialise the station if required
				if (rec.id.empty())
				{
					BOOST_LOG_TRIVIAL(debug)
					<< "Initialising station " << id;

					rec.id				= id;

					// Read the BLQ file
					bool found = false;
					for (auto& blqfile : acsConfig.blqfiles)
					{
						found = readblq(blqfile.c_str(), id.c_str(), rec.rtk.opt.odisp[0]);

						if (found)
						{
							break;
						}
					}

					if (found == false)
					{
						BOOST_LOG_TRIVIAL(warning)
						<< "No BLQ for " << id;
					}

					if (acsConfig.process_user)
					{
						rec.rtk.pppState.max_filter_iter	= acsConfig.pppOpts.max_filter_iter;
						rec.rtk.pppState.max_prefit_remv	= acsConfig.pppOpts.max_prefit_remv;
						rec.rtk.pppState.inverter			= acsConfig.pppOpts.inverter;
						rec.rtk.pppState.output_residuals	= acsConfig.output_residuals;

						rec.rtk.pppState.rejectCallbacks.push_back(countSignalErrors);
						rec.rtk.pppState.rejectCallbacks.push_back(deweightMeas);
					}
					
					if	( acsConfig.process_rts
						&&acsConfig.pppOpts.rts_lag)
					{
						string rts_filename = acsConfig.pppOpts.rts_filename;

						replaceString(rts_filename, "<STATION>", id);

						initFilterTrace(rec.rtk.pppState, rts_filename, id, acsConfig.pppOpts.rts_lag);
					}
				}
				
				//calculate statistics
				{
					if (rec.firstEpoch	== GTime::noTime())		{	rec.firstEpoch	= rec.obsList.front().time;		}
					if (tsync			== GTime::noTime())		{	tsync			= rec.obsList.front().time;		}
																	rec.lastEpoch	= rec.obsList.front().time;
					rec.epochCount++;
					rec.obsCount += rec.obsList.size();
					
					for (auto& obs				: rec.obsList)
					for (auto& [ft, sigList]	: obs.SigsLists)
					for (auto& sig				: sigList) 
					{
						rec.codeCount[sig.code]++;
					}

					for (auto& obs				: rec.obsList)
					{
						rec.satCount[obs.Sat.id()]++;
					}
				}

				//prepare and connect navigation objects to the observations
				for (auto& obs : rec.obsList)
				{
					obs.satNav_ptr					= &nav.satNavMap[obs.Sat];
					obs.satNav_ptr->eph_ptr 		= seleph	(tsync, obs.Sat, -1, nav);
					obs.satNav_ptr->geph_ptr 		= selgeph	(tsync, obs.Sat, -1, nav);
					obs.satNav_ptr->pephList_ptr	= &nav.pephMap[obs.Sat];
					obs.mount 						= id;
					updatenav(obs);

					obs.satStat_ptr					= &rec.rtk.satStatMap[obs.Sat];
					obs.satOrb_ptr					= &nav.orbpod.satOrbitMap[obs.Sat];
					
					auto& satOpts = acsConfig.getSatOpts(obs.Sat);
				}

				obsVariances(rec.obsList);

				//add this station to the list of stations with data for this epoch
				epochStations.push_back(&rec);
			}
		}
		
		if (acsConfig.process_user)
		{
			if (acsConfig.ssrOpts.sync_epochs)
				waitForSyncFile(epoch + acsConfig.ssrOpts.sync_epoch_offset);
			
			if (acsConfig.ssrOpts.read_from_files)
				rtcmDecodeFromFile(epoch + acsConfig.ssrOpts.sync_epoch_offset); // add an offset if starting later than 00:00:00
		}
		
		// Observations or not provide trace information on the downloading station stream.

		
		for (auto& [id, rec] : stationMap )
		{
			auto down_it = ntripRtcmMultimap.find(rec.id);
			if( down_it != ntripRtcmMultimap.end() )
			{
				std::ofstream stecfile(acsConfig.ionstec_filename, std::ofstream::app);
				auto trace = getTraceFile(rec);
				trace << std::endl << "<<<<<<<<<<< Network Trace : Epoch " << epoch << " >>>>>>>>>>>" << std::endl;      
				NtripRtcmStream& downStream = *down_it->second;
				downStream.traceWriteEpoch(trace);             
			} 
		}

		for (auto& [id, s] : outStreamManager.ntripUploadStreams)
		{
			NtripBroadcaster::NtripUploadClient& uploadStream = *s;
			auto trace = getTraceFile(uploadStream);
            trace << std::endl << "<<<<<<<<<<< Network Trace : Epoch " << epoch << " >>>>>>>>>>>" << std::endl;
            uploadStream.traceWriteEpoch(trace);
        }
        
        recordNetworkStatistics(ntripRtcmMultimap);
			
		if	(complete)
		{
			break;
		}

		if	(tsync == GTime::noTime())
		{
			continue;
		}

		if	(epochStations.empty())
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Epoch " << epoch << " finished with no observations";
			
			//eat one epoch before next loop
			loopEpochs = 1;
			continue;
		}

		double ep[6];
		time2epoch(tsync, ep);
		
		createTracefiles(stationMap, net);
		
		//try to get svns of all used satellites
		for (auto& [satId, satNav] : nav.satNavMap)
		{
			SatSys Sat;
			Sat.fromHash(satId);
			pcvacs_t* pcvsat = findAntenna(Sat.id(), ep, nav);
			if (pcvsat)
			{
				Sat.setSvn(pcvsat->svn);
			}
		}

		// if the vmf3 option has been selected:
		if	(!acsConfig.tropOpts.vmf3dir.empty())
		{
			BOOST_LOG_TRIVIAL(debug)
			<< "VMF3 option chosen";

			for (auto& rec_ptr : epochStations)
			{
				auto& rec = *rec_ptr;
				double jd = ymdhms2jd(ep);

				// read vmf3 grid information
				rec.vmf3.m = 1;
				udgrid(acsConfig.tropOpts.vmf3dir.c_str(), rec.vmf3.vmf3g, jd, rec.mjd0, rec.vmf3.m);
			}
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Synced " << epochStations.size() << " stations...";

		auto epochStartTime = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));

		acsConfig.parse();
		reloadInputFiles();
		
#		ifdef ENABLE_PARALLELISATION
#		ifndef ENABLE_UNIT_TESTS
		Eigen::setNbThreads(1);
#			pragma omp parallel for
#		endif
#		endif
		for (int i = 0; i < epochStations.size(); i++)
		{
			auto rec_ptr_iterator = epochStations.begin();
			std::advance(rec_ptr_iterator, i);

			Station& rec = **rec_ptr_iterator;
			mainOncePerEpochPerStation(rec, orog, gptg);
		}	

		Eigen::setNbThreads(0);

		mainOncePerEpoch(net, epochStations, tsync);

		auto epochStopTime = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));

		int week;
		double sec = time2gpst(tsync, &week);
		auto boostTime = boost::posix_time::from_time_t(tsync.time);

		BOOST_LOG_TRIVIAL(info)
		<< "Processed epoch #" << epoch
		<< " - " << "GPS time: " << week << " " << std::setw(6) << sec << " - " << boostTime << " (took " << (epochStopTime-epochStartTime) << ")";

		// Check end epoch
		if	( !acsConfig.end_epoch.is_not_a_date_time()
			&&boostTime >= acsConfig.end_epoch)
		{
			BOOST_LOG_TRIVIAL(info)
			<< "Exiting at epoch " << epoch << " (" << boostTime
			<< ") as end epoch " << acsConfig.end_epoch
			<< " has been reached";

			break;
		}

		// Check number of epochs
		if	( acsConfig.max_epochs	> 0
			&&epoch					>= acsConfig.max_epochs)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "Exiting at epoch " << epoch << " (" << boostTime
			<< ") as epoch count " << acsConfig.max_epochs
			<< " has been reached";

			break;
		}

		auto loopStopTime		= system_clock::now();
		auto loopExcessDuration = loopStopTime - (nextNominalLoopStartTime + std::chrono::milliseconds((int)(acsConfig.wait_all_stations * 1000)));
		int excessLoops			= loopExcessDuration / std::chrono::milliseconds((int)(acsConfig.wait_next_epoch * 1000));
		
		if (excessLoops < 0)		{	excessLoops = 0;																						}	
		if (excessLoops > 0)		{	BOOST_LOG_TRIVIAL(warning) << std::endl << "Excessive time elapsed, skipping " << excessLoops << " epochs";		}
		
		loopEpochs = 1 + excessLoops;
	}


#ifndef	ENABLE_UNIT_TESTS
	// Disconnect the downloading clients and stop the io_service for clean shutdown.
	for (auto& [id, s] : ntripRtcmMultimap)
	{
		NtripStream& downStream = *s;
		downStream.disconnect();
	}
	outStreamManager.stopBroadcast();  
	NtripSocket::io_service.stop();
#endif
		
	auto peaInterTime = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
	BOOST_LOG_TRIVIAL(info)
	<< std::endl
	<< "PEA started  processing at : " << peaStartTime << std::endl
	<< "and finished processing at : " << peaInterTime << std::endl
	<< "Total processing duration  : " << (peaInterTime - peaStartTime) << std::endl << std::endl;

	BOOST_LOG_TRIVIAL(info)
	<< std::endl
	<< "Finalising streams and post processing...";

	mainPostProcessing(net, stationMap);

	auto peaStopTime = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));

	BOOST_LOG_TRIVIAL(info)
	<< std::endl
	<< "PEA started  processing at : " << peaStartTime	<< std::endl
	<< "and finished processing at : " << peaStopTime	<< std::endl
	<< "Total processing duration  : " << (peaStopTime - peaStartTime) << std::endl << std::endl;

	BOOST_LOG_TRIVIAL(info)
	<< "PEA finished";

	return(EXIT_SUCCESS);
}
