
// #pragma GCC optimize ("O0")

#include <sys/time.h>
#include <algorithm>
#include <iostream>
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
using std::string;

#include <boost/log/utility/setup/console.hpp>
#include <boost/log/trivial.hpp>
#include <boost/filesystem.hpp>


#include "minimumConstraints.hpp"
#include "ntripSourceTable.hpp"
#include "networkEstimator.hpp"
#include "peaCommitVersion.h"
#include "ntripBroadcast.hpp"
#include "rinexNavWrite.hpp"
#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "algebraTrace.hpp"
#include "rtsSmoothing.hpp"
#include "ntripSocket.hpp"
#include "rtcmEncoder.hpp"
#include "corrections.hpp"
#include "forceModels.hpp"
#include "streamTrace.hpp"
#include "mongoWrite.hpp"
#include "GNSSambres.hpp"
#include "instrument.hpp"
#include "acsConfig.hpp"
#include "acsStream.hpp"
#include "testUtils.hpp"
#include "biasSINEX.hpp"
#include "ionoModel.hpp"
#include "ephemeris.hpp"
#include "sp3Write.hpp"
#include "station.hpp"
#include "summary.hpp"
#include "antenna.hpp"
#include "satStat.hpp"
#include "fileLog.hpp"
#include "jpl_eph.hpp"
#include "common.hpp"
#include "orbits.hpp"
#include "acsQC.hpp"
#include "gTime.hpp"
#include "debug.hpp"
#include "sinex.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "trop.h"
#include "vmf3.h"


nav_t			nav		= {};
int				epoch	= 1;
GTime			tsync	= GTime::noTime();

void excludeUnprocessed(
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

bool fileChanged(
	string filename)
{
	bool valid = checkValidFile(filename);
	if (valid == false)
	{
		return false;
	}

	auto modifyTime = boost::filesystem::last_write_time(filename);
	
	auto it = acsConfig.configModifyTimeMap.find(filename);
	if (it == acsConfig.configModifyTimeMap.end())
	{
		//the first time this file has been read, 
		//update then return true
		acsConfig.configModifyTimeMap[filename] = modifyTime;
		
		return true;
	}
	
	auto& [dummy, readTime] = *it;
	if (readTime != modifyTime)
	{
		//has a different modification time, update then return true
		readTime = modifyTime;
	
		return true;
	}
	
	//has been read with this time before
	return false;
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

/** Create a station object from a file
*/
void addStationDataFile(
	string fileName,			///< Filename to create station from
	string fileType,			///< Type of data in file
	string dataType)			///< Type of data
{
	if (streamDOAMap.find(fileName) != streamDOAMap.end())
	{
		//this stream was already added, dont re-add
		return;
	}

	boost::filesystem::path filePath(fileName);

	if (checkValidFile(fileName, dataType) == false)
	{
		return;
	}

	if (!boost::filesystem::is_regular_file(filePath))
	{
		return;
	}

	auto filename = filePath.filename();
	string extension = filename.extension().string();

	string stationId = filename.string().substr(0,4);
	boost::algorithm::to_upper(stationId);

	auto& recOpts = acsConfig.getRecOpts(stationId);

	if (recOpts.exclude)
	{
		return;
	}

	if (fileType == "RINEX")
	{
		auto rinexStream_ptr = std::make_shared<FileRinexStream>(filePath.string());

		if		(dataType == "NAV")		navStreamMultimap.insert({stationId, std::move(rinexStream_ptr)});
		else if	(dataType == "OBS")		obsStreamMultimap.insert({stationId, std::move(rinexStream_ptr)});

		streamDOAMap[fileName] = false;

		acsConfig.rinexFiles.push_back(filename.string());
	}

	if (fileType == "RTCM")
	{
		auto rtcmStream_ptr = std::make_shared<FileRtcmStream>(filePath.string());

		if		(dataType == "NAV")		navStreamMultimap.insert({stationId, std::move(rtcmStream_ptr)});
		else if	(dataType == "OBS")		obsStreamMultimap.insert({stationId, std::move(rtcmStream_ptr)});

		streamDOAMap[fileName] = false;
	}
	
	if (fileType == "SP3")
	{
		auto pseudoObsStream_ptr = std::make_shared<FileSp3Stream>(filePath.string());

		if		(dataType == "PSEUDO")	pseudoObsStreamMultimap.insert({stationId, std::move(pseudoObsStream_ptr)});

		streamDOAMap[fileName] = false;
	}
}

void reloadInputFiles()
{
	boost::filesystem::path root_stations_path(acsConfig.root_stations_dir);

	if (boost::filesystem::is_directory(root_stations_path))
	{
		for (auto& rnxfile			: acsConfig.rnxfiles)			{	addStationDataFile(rnxfile,			"RINEX",	"OBS");			}	
		for (auto& rtcmfile			: acsConfig.obs_rtcmfiles)		{	addStationDataFile(rtcmfile,		"RTCM",		"OBS");			}	
		for (auto& rtcmfile			: acsConfig.nav_rtcmfiles)		{	addStationDataFile(rtcmfile,		"RTCM",		"NAV");			}
		for (auto& pseudoobsfile	: acsConfig.pseudoobs_files)	{	addStationDataFile(pseudoobsfile,	"SP3",		"PSEUDO");		}

		if	( obsStreamMultimap.empty()
			&&acsConfig.require_obs)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "No station observation (rnx) files found in "
			<< acsConfig.root_stations_dir;
		}
	}

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

		readerp(erpfile, nav.erp);
	}

	removeInvalidFiles(acsConfig.orbfiles);
	removeInvalidFiles(acsConfig.sp3files);
	if (acsConfig.orbfiles.empty() == false)
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

			readorbit(orbfile);
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

			readSp3ToNav(sp3file, &nav, 0);
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

		readBiasSinex(bsxfile);
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

		readtec(ionfile, &nav);
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

	removeInvalidFiles(acsConfig.egmfiles);
	for (auto& egmfile : acsConfig.egmfiles)
	{
		if (fileChanged(egmfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading EGMCoef file " << egmfile;

		readegm(egmfile, nav.egm);
	}	

	removeInvalidFiles(acsConfig.jplfiles);
	for (auto& jplfile : acsConfig.jplfiles)
	{
		if (fileChanged(jplfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading JPL file " << jplfile;
		
		nav.jplEph_ptr = (struct jpl_eph_data*) jpl_init_ephemeris(jplfile.c_str(), nullptr, nullptr);          ///< a Pointer to The jpl_eph_data Structure
	}		
}

void configureUploadingStreams()
{
	for (auto& [outLabel, outStreamData] : acsConfig.uploadingStreamData)
	{
		auto it = ntripBroadcaster.ntripUploadStreams.find(outLabel);

		// Create stream if it does not already exist.
		if (it == ntripBroadcaster.ntripUploadStreams.end())
		{
			auto outStream_ptr = std::make_shared<NtripUploader>(outStreamData.target_url);
			auto& outStream = *outStream_ptr.get();
			ntripBroadcaster.ntripUploadStreams[outLabel] = std::move(outStream_ptr);

			it = ntripBroadcaster.ntripUploadStreams.find(outLabel);
		}

		auto& [label, outStream_ptr]	= *it;
		auto& outStream					= *outStream_ptr;
		
		outStream.streamConfig.rtcmMessagesTypes	= outStreamData.rtcmMessagesTypes;
		outStream.streamConfig.itrf_datum 			= outStreamData.itrf_datum;
		outStream.streamConfig.provider_id 			= outStreamData.provider_id;
		outStream.streamConfig.solution_id 			= outStreamData.solution_id;
		outStream.streamConfig.update_interval	 	= outStreamData.update_interval;
		outStream.streamConfig.master_iod 			= outStreamData.master_iod;
	}

	for (auto it = ntripBroadcaster.ntripUploadStreams.begin(); it != ntripBroadcaster.ntripUploadStreams.end();)
	{
		if (acsConfig.uploadingStreamData.find(it->first) == acsConfig.uploadingStreamData.end())
		{
			auto& [label, outStream_ptr]	= *it;
			auto& outStream					= *outStream_ptr;
			outStream.disconnect();
			it = ntripBroadcaster.ntripUploadStreams.erase(it);
		}
		else
		{
			it++;
		}
	}
}

void configureDownloadingStreams()
{
// 	std::multimap<string, vector<string>> hostMap;
// 	
// 	for (auto host : acsConfig.netwOpts.download_hosts)
// 	{
// 		NtripSourceTable sourceTable(host);
// 
// 		sourceTable.getSourceTable();
// 
// 		hostMap.insert({host, sourceTable.getStreamMounts()});
// 	}

	for (auto nav : {false, true})
	{
		set<string> streamUrls;
		if (nav == false)	streamUrls = acsConfig.netwOpts.obs_mount_url;
		else				streamUrls = acsConfig.netwOpts.nav_mount_url;

		map<string, string> ntripStreams;
		for (auto fullUrl : streamUrls)
		{
			std::size_t slashPos = fullUrl.find_last_of("/");	// find first 4 characters after last '/'
			string id		= fullUrl.substr(slashPos + 1,	4);					// e.g. http://user:pass@auscors.ga.gov.au:2101/BCEP00BKG0 --> BCEP
			ntripStreams[id] = fullUrl;
		}

		// Check for additional streams to add.
		for (auto& [id, fullUrl] : ntripStreams)
		{
			if (streamDOAMap.find(fullUrl) != streamDOAMap.end())
			{
				//this stream was already added, dont re-add
				continue;
			}

			// Do not insert stream twice.
			if (ntripRtcmMultimap.find(id) != ntripRtcmMultimap.end())
				continue;

			std::size_t slashPos = fullUrl.find_last_of("/");	// find first 4 characters after last '/'
			string hostname = fullUrl.substr(0,				slashPos + 1);
			string mount	= fullUrl.substr(slashPos + 1,	fullUrl.length() - slashPos+1);
			
// 			auto host = hostMap.find(hostname);
// 			if (host != hostMap.end())
// 			{
// 				auto mounts = host->second;
// 				if (std::find(mounts.begin(), mounts.end(), mount) == mounts.end())
// 				{
// 					URL url = URL::parse(fullUrl);
// 					BOOST_LOG_TRIVIAL(warning)
// 					<< "Stream, " << url.sanitised() << " not found in sourcetable, invalid host/mount combination.\n";
// 				}
// 			}
			
			auto ntripStream_ptr = std::make_shared<NtripRtcmStream>(fullUrl);
			ntripRtcmMultimap.insert({id, ntripStream_ptr}); // for network (internet) tracing

			//prepare to download streams if enabled
			{
				NtripRtcmStream& downloadStream = *ntripStream_ptr;
				string filename;
				if (nav)		filename = acsConfig.rtcm_nav_filename;
				else			filename = acsConfig.rtcm_obs_filename;

				bool createFile = false;
				if (nav == true		&& acsConfig.record_rtcm_nav)		createFile = true;
				if (nav == false	&& acsConfig.record_rtcm_obs)		createFile = true;

				if (createFile)
				{
					replaceString(filename, "<STATION>",	id);
					replaceString(filename, "<STREAM>",		mount);
					
					downloadStream.rtcm_filename	= filename;
					downloadStream.record_rtcm		= true;
					downloadStream.createRtcmFile();
				}
				
				if (acsConfig.output_decoded_rtcm_json)
				{
					filename = acsConfig.decoded_rtcm_json_filename;
					replaceString(filename, "<STATION>",	id);
					replaceString(filename, "<STREAM>",		mount);
					downloadStream.rtcmTraceFilename = filename;
				}
			}

			if (nav == false)		{	obsStreamMultimap.insert({id, std::move(ntripStream_ptr)});		}
			else					{	navStreamMultimap.insert({id, std::move(ntripStream_ptr)});		}
			streamDOAMap[fullUrl] = false;
		}

		// Check for any streams to remove.
		for (auto [id, fullUrl] : ntripStreams)
		{
			auto it = ntripRtcmMultimap.find(id);
			if (it == ntripRtcmMultimap.end())
			{
				NtripRtcmStream& downloadStream = *it->second;
				downloadStream.disconnect();	
				BOOST_LOG_TRIVIAL(warning) << "Removing downloading mount point : " << id << std::endl;
				if (nav == false)		{	obsStreamMultimap.erase(id);	}
				else					{	navStreamMultimap.erase(id);	}
			}
		}
	}

	for (auto& [id, s] : ntripRtcmMultimap)
	{
		auto& downStream = *s;
		downStream.print_stream_statistics		= acsConfig.print_stream_statistics;
	}
}

void createNewTraceFile(
	const string				id,
	boost::posix_time::ptime	logptime,
	string  					new_path_trace,
	string& 					old_path_trace,
	bool						outputHeader,
	bool						outputConfig)
{	
	replaceString(new_path_trace, "<STATION>", id);
	replaceTimes (new_path_trace, logptime);

	// Create the trace file if its a new filename, otherwise, keep the old one
	if (new_path_trace == old_path_trace)
	{
		//the filename is the same, keep using the old ones
		return;
	}

	old_path_trace = new_path_trace;

	BOOST_LOG_TRIVIAL(debug)
	<< "\tCreating new file for " << id << " at " << old_path_trace;

	std::ofstream trace(old_path_trace);
	if (!trace)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not create file for " << id << " at " << old_path_trace;

		return;
	}

	// Trace file head
	if (outputHeader)
	{
		trace << "station    : " << id << std::endl;
		trace << "start_epoch: " << acsConfig.start_epoch			<< std::endl;
		trace << "end_epoch  : " << acsConfig.end_epoch				<< std::endl;
		trace << "trace_level: " << acsConfig.trace_level			<< std::endl;
		trace << "pea_version: " << GINAN_COMMIT_VERSION			<< std::endl;
// 		trace << "rts_lag    : " << acsConfig.pppOpts.rts_lag		<< std::endl;
	}

	if (outputConfig)
	{
		dumpConfig(trace);
	}
}

map<string, string> fileNames;

/** Create new empty trace files only when required when the filename is changed
 */
void createTracefiles(
	map<string, Station>&	stationMap,
	Network&				net)
{
	string dummyStr;
	GTime logtime = tsync.roundTime(acsConfig.trace_rotate_period);
	
	boost::posix_time::ptime	logptime	= boost::posix_time::from_time_t(logtime.time);
	
	// Ensure the output directories exist
	for (auto directory : {
								acsConfig.erp_directory,
								acsConfig.log_directory,
								acsConfig.ionex_directory,
								acsConfig.sinex_directory,
								acsConfig.trace_directory,
								acsConfig.orbits_directory,
								acsConfig.clocks_directory,
								acsConfig.ionstec_directory,
								acsConfig.ppp_sol_directory,
								acsConfig.summary_directory,
								acsConfig.testOpts.directory,
								acsConfig.rtcm_nav_directory,
								acsConfig.rtcm_obs_directory,
								acsConfig.rinex_obs_directory,
								acsConfig.rinex_nav_directory,
								acsConfig.trop_sinex_directory,
								acsConfig.bias_sinex_directory,
								acsConfig.persistance_directory,
								acsConfig.pppOpts.rts_directory,
								acsConfig.netwOpts.rts_directory,
								acsConfig.ssrOpts.rtcm_directory,
								acsConfig.ionFilterOpts.rts_directory,
								acsConfig.decoded_rtcm_json_directory,
								acsConfig.encoded_rtcm_json_directory,
								acsConfig.network_statistics_json_directory
							})
	{
		if (directory == "./")	continue;
		if (directory.empty())	continue;
		
		replaceTimes(directory, logptime);
		boost::filesystem::create_directories(directory);
	}
	
	if (acsConfig.output_trace)
	{
		for (auto& [id, rec] : stationMap)
		{
			createNewTraceFile(id,			logptime,	acsConfig.trace_filename,							rec.traceFilename,						true,	acsConfig.output_config);
		}
	}
	
	if	(	acsConfig.output_trop_sinex
		&&	acsConfig.process_user)
	{
		for (auto& [id, rec] : stationMap)
		{
			createNewTraceFile(id,			logptime,	acsConfig.trop_sinex_filename,						rec.tropFilename,						false,	false);
		
			if	(  acsConfig.process_rts
				&& acsConfig.pppOpts.rts_lag)
			{
				createNewTraceFile(id,		logptime,	acsConfig.trop_sinex_filename + SMOOTHED_SUFFIX,	rec.rtsTropFilename,					false,	false);
			}
		}
	}
	
	if	(  acsConfig.process_rts
		&& acsConfig.pppOpts.rts_lag)
	{
		for (auto& [id, rec] : stationMap)
		{
			createNewTraceFile(id,			logptime,	acsConfig.pppOpts.rts_filename + FORWARD_SUFFIX,	rec.rtk.pppState.rts_forward_filename,	false,	false);
			createNewTraceFile(id,			logptime,	acsConfig.pppOpts.rts_filename,						rec.rtk.pppState.rts_filename,			true,	acsConfig.output_config);
		}
	}
	
	if	(  acsConfig.process_rts
		&& acsConfig.netwOpts.rts_lag)
	{
		createNewTraceFile(net.id,		logptime,	acsConfig.netwOpts.rts_filename + FORWARD_SUFFIX,	net.kfState.rts_forward_filename,		false,	false);
		createNewTraceFile(net.id,		logptime,	acsConfig.netwOpts.rts_filename,					net.kfState.rts_filename,				true,	acsConfig.output_config);
	}

	if (acsConfig.output_summary)
	{
		createNewTraceFile(net.id,		logptime,	acsConfig.summary_filename,							net.traceFilename,						true,	acsConfig.output_config);
	}

	if (acsConfig.output_log)
	{
		createNewTraceFile("",			logptime,	acsConfig.log_filename,								FileLog::path_log,						false,	false);
	}
	
	if	( acsConfig.output_trop_sinex
		&&acsConfig.process_network)
	{	
		createNewTraceFile(net.id,		logptime,	acsConfig.trop_sinex_filename,						net.tropFilename,						false,	false);
		
		if	( acsConfig.process_rts
			&&acsConfig.netwOpts.rts_lag)
		{
			createNewTraceFile(net.id,	logptime,	acsConfig.trop_sinex_filename + SMOOTHED_SUFFIX,	net.rtsTropFilename,					false,	false);
		}
	}

	if	( acsConfig.output_erp
		&&acsConfig.process_network)
	{	
		createNewTraceFile(net.id,		logptime,	acsConfig.erp_filename,								net.erpFilename,						false,	false);
		
		if	( acsConfig.process_rts
			&&acsConfig.netwOpts.rts_lag)
		{
			createNewTraceFile(net.id,	logptime,	acsConfig.erp_filename + SMOOTHED_SUFFIX,			net.rtsErpFilename,						false,	false);
		}
	}

	if	(acsConfig.output_decoded_rtcm_json)
	{
		createNewTraceFile(net.id,		logptime,	acsConfig.decoded_rtcm_json_filename,				acsConfig.decoded_rtcm_json_filename,	false,	false);
	}

	if	(acsConfig.output_encoded_rtcm_json)
	{
		createNewTraceFile(net.id,		logptime,	acsConfig.encoded_rtcm_json_filename,				acsConfig.encoded_rtcm_json_filename,	false,	false);
	}

	if (acsConfig.output_ppp_sol)
	{
		for (auto& [id, rec] : stationMap)
		{
			string old_filename = rec.solutFilename;
			createNewTraceFile(id,			logptime,	acsConfig.ppp_sol_filename,							rec.solutFilename,						true,	acsConfig.output_config);
			if(old_filename != rec.solutFilename) rec.sol_header = true;
		}
	}

	if (acsConfig.output_rinex_obs)
	{
		for (auto& [id, rec] : stationMap)
		{
			auto filenameMap = getSysOutputFilenames(acsConfig.rinex_obs_filename,	tsync, id);
			for (auto& [filename, dummy] : filenameMap)
			{
				createNewTraceFile(id,		logptime,	filename,											fileNames[filename],					false,	false);
			}
		}
	}
	
	if (acsConfig.output_rinex_nav)
	{
		auto filenameMap = getSysOutputFilenames(acsConfig.rinex_nav_filename,	tsync);
		for (auto& [filename, dummy] : filenameMap)
		{
			createNewTraceFile("",		logptime,	filename,											fileNames[filename],					false,	false);
		}
	}

	if (acsConfig.output_orbits)
	{
		auto filenameMap = getSysOutputFilenames(acsConfig.orbits_filename,		tsync);
		for (auto& [filename, dummy] : filenameMap)
		{
			createNewTraceFile(net.id,	logptime,	filename,											fileNames[filename],					false,	false);
		}
	}

	if (acsConfig.output_clocks)
	{
		auto filenameMap = getSysOutputFilenames(acsConfig.clocks_filename,		tsync);
		for (auto& [filename, dummy] : filenameMap)
		{
			createNewTraceFile(net.id,	logptime,	filename,											fileNames[filename],					false,	false);
		}

		if	(  acsConfig.process_rts
			&& acsConfig.netwOpts.rts_lag)
		for (auto& [filename, dummy] : filenameMap)
		{
			createNewTraceFile(net.id, 	logptime,	filename + SMOOTHED_SUFFIX, 						fileNames[filename + SMOOTHED_SUFFIX],	false,	false);
		}
	}
	
	if (acsConfig.output_bias_sinex)
	{
		createNewTraceFile("", 	logptime,acsConfig.bias_sinex_filename,fileNames[acsConfig.bias_sinex_filename],	false,	false);
	}	
}

void avoidCollisions(
	StationMap&		stationMap)
{
	for (auto& [id, rec] : stationMap)
	{
		auto trace = getTraceFile(rec);
		
		acsConfig.getRecOpts(id);
		
		//prepare and connect navigation objects to the observations
		for (auto& obs : rec.obsList)
		{
			obs.satNav_ptr					= &nav.satNavMap[obs.Sat];
			obs.satNav_ptr->eph_ptr 		= seleph<Eph>	(trace, tsync, obs.Sat, -1, nav);
			obs.satNav_ptr->geph_ptr 		= seleph<Geph>	(trace, tsync, obs.Sat, -1, nav);
			obs.mount 						= rec.id;
			updatenav(obs);

			obs.satStat_ptr					= &rec.rtk.satStatMap[obs.Sat];

			acsConfig.getSatOpts(obs.Sat);
			
			//ar stuff
			{
				if (acsConfig.process_network)	ARstations["NETWORK"].ID	= "NETWORK";
				else							ARstations[rec.id].ID		= rec.id;
				
				sys_activ[rec.id];
			
				ARsatellites[obs.Sat];
				
				for (int i = 0; i < E_AmbTyp::_size(); i++)
				{
					E_AmbTyp	ambType			= E_AmbTyp::_values()[i];
					
					elev_archive[{KF::AMBIGUITY, obs.Sat, obs.mount,i}];
					slip_archive[{KF::AMBIGUITY, obs.Sat, obs.mount,i}];
				}
			} 
		}
	}
	
	// if the vmf3 option has been selected:
	if	(acsConfig.tropOpts.vmf3dir.empty() == false)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "VMF3 option chosen";
		
		double ep[6];
		time2epoch(tsync, ep);
		
		double jd = ymdhms2jd(ep);

		// read vmf3 grid information
		nav.vmf3.m = 1;
		udgrid(acsConfig.tropOpts.vmf3dir.c_str(), nav.vmf3, jd);
	}
}

void initialiseStation(
	string		id,
	Station&	rec)
{
	BOOST_LOG_TRIVIAL(debug)
	<< "Initialising station " << id;

	rec.id = id;

	// Read the BLQ file
	bool found = false;
	for (auto& blqfile : acsConfig.blqfiles)
	{
		found = readblq(blqfile, id.c_str(), rec.rtk.opt.otlDisplacement[0]);

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
		rec.rtk.pppState.id					= id;
		rec.rtk.pppState.max_filter_iter	= acsConfig.pppOpts.max_filter_iter;
		rec.rtk.pppState.max_prefit_remv	= acsConfig.pppOpts.max_prefit_remv;
		rec.rtk.pppState.inverter			= acsConfig.pppOpts.inverter;
		rec.rtk.pppState.sigma_threshold	= acsConfig.pppOpts.sigma_threshold;
		rec.rtk.pppState.sigma_check		= acsConfig.pppOpts.sigma_check;
		rec.rtk.pppState.w_test				= acsConfig.pppOpts.w_test;
		rec.rtk.pppState.chi_square_test	= acsConfig.pppOpts.chi_square_test;
		rec.rtk.pppState.chi_square_mode	= acsConfig.pppOpts.chi_square_mode;
		rec.rtk.pppState.output_residuals	= acsConfig.output_residuals;

		rec.rtk.pppState.measRejectCallbacks	.push_back(countSignalErrors);
		rec.rtk.pppState.measRejectCallbacks	.push_back(deweightMeas);
		rec.rtk.pppState.stateRejectCallbacks	.push_back(deweightByState);
		rec.rtk.pppState.stateRejectCallbacks	.push_back(clockGlitchReaction);
	}

	if	( acsConfig.process_rts
		&&acsConfig.pppOpts.rts_lag)
	{
		rec.rtk.pppState.rts_lag = acsConfig.pppOpts.rts_lag;
	}
}

/** Perform operations for each station
 * This function occurs in parallel with other stations - ensure that any operations on global maps do not create new entries, as that will destroy the map for other processes.
 * Variables within the rec object are ok to use, but be aware that pointers from the within the receiver often point to global variables.
 * Prepare global maps by accessing the desired elements before calling this function.
 */
void mainOncePerEpochPerStation(
	Station&	rec,
	double*		orog,
	gptgrid_t&	gptg,
	bool&		emptyEpoch)
{
	TestStack ts(rec.id);
	
	if (rec.ready == false)
	{
		return;
	}
	
	emptyEpoch = false;

	auto trace = getTraceFile(rec);

	trace << std::endl << "------=============== Epoch " << epoch << " =============-----------" << std::endl;

	BOOST_LOG_TRIVIAL(debug)
	<< "\tRead " << rec.obsList.size()
	<< " observations for station " << rec.id;

	//calculate statistics
	{
		if (rec.firstEpoch	== GTime::noTime())		{	rec.firstEpoch	= rec.obsList.front().time;		}
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

	if (acsConfig.process_preprocessor)
	{
		excludeUnprocessed(rec.obsList);
		
		obsVariances(rec.obsList);
	
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
// 		detectjump	(trace,	rec.obsList, acsConfig.elevation_mask, rec.cj);

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
	}

	if (acsConfig.process_ionosphere)
	{
		bool sppUsed;
		selectAprioriSource(rec, sppUsed);

		if (rec.aprioriPos.norm())
			update_receivr_measr(trace, rec);
	}

	/* exclude measurements of eclipsing satellite (block IIA) */
	if (acsConfig.reject_eclipse)
	{
		testeclipse(rec.obsList);
	}

	if (acsConfig.process_user)
	{
		pppos(trace, rec.rtk, rec.obsList, rec);
		
		if (rec.rtk.sol.stat != SOLQ_NONE)
		{
			int nfixed = 0;
			if ( acsConfig.ambrOpts.NLmode	!= +E_ARmode::OFF)
			{
				nfixed = enduserAmbigResl(trace, rec.obsList, rec.rtk.pppState, rec.snx.pos, rec.rtk.sol.dop[2], rec.solutFilename, rec.sol_header);
				
			}
			else
			{
				rec.rtk.pppState.outputStates(trace);
			}
			
			trace << std::endl << "Solution with " << nfixed << " ambigities resolved";

		}
		
		if (acsConfig.output_ppp_sol)
		{
			outputPPPSolution(rec);
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
			rtkVmf3_ptr = &nav.vmf3;
			rtkOrog_ptr = orog;
		}

		/* observed minus computed for each satellites */
		pppomc(trace, rec.rtk, rec.obsList, gptg, rec, rtkVmf3_ptr, rtkOrog_ptr);
	}

	if	( (acsConfig.process_rts)
		&&(acsConfig.pppOpts.rts_lag > 0)
		&&(epoch >= acsConfig.pppOpts.rts_lag))
	{
		KFState rts = RTS_Process(rec.rtk.pppState);
	}

	if	( acsConfig.process_user
		&&acsConfig.output_mongo_states)
	{
		mongoStates(rec.rtk.pppState);
	}

	if (acsConfig.output_rinex_obs)
	{
		writeRinexObs(rec.id, rec.snx, tsync, rec.obsList);
	}
}

void mainPerEpochPostProcessingAndOutputs(
	Network&		net,
	StationMap&		stationMap)
{
	TestStack ts("post");
	
	auto netTrace = getTraceFile(net);

	if	(acsConfig.process_user)
	{
		if (acsConfig.output_trop_sinex)
		for (auto& [id, rec]	: stationMap)
		{
			for (auto& [id, rec]	: stationMap)
			{
				KFState dummy;
				outputTropSinex(rec.tropFilename, tsync, stationMap,	dummy, id);
			}
		}
		
		if (acsConfig.output_clocks)
		{
			auto filenameSysMap = getSysOutputFilenames(acsConfig.clocks_filename, tsync);

			for (auto [filename, sysMap] : filenameSysMap)
			{
				outputClocks(filename, acsConfig.clocks_receiver_source, acsConfig.clocks_satellite_source, tsync, sysMap, net.kfState, &stationMap);
			}
		}
	}
	
	if	( acsConfig.process_ppp
		||acsConfig.process_network)
	{
		if (acsConfig.output_mongo_states)
		{
			mongoStates(net.kfState);
		}
		
		KFState KF_ARcopy = net.kfState;
		
		if (acsConfig.ambrOpts.NLmode != +E_ARmode::OFF)
		{
			networkAmbigResl(netTrace, stationMap, KF_ARcopy);

			if (ARsol_ready())
			{
				KF_ARcopy.outputStates(netTrace, " AR");
			}

			if (acsConfig.output_mongo_states)
			{
				mongoStates(KF_ARcopy, "_AR");
			}
		}
		
		// Output clock product body part
		if	( acsConfig.output_AR_clocks == false
			||ARsol_ready() == false)
		{
			if	(acsConfig.output_clocks)
			{
				auto filenameSysMap = getSysOutputFilenames(acsConfig.clocks_filename, tsync);

				for (auto [filename, sysMap] : filenameSysMap)
				{
					outputClocks(filename, acsConfig.clocks_receiver_source, acsConfig.clocks_satellite_source, tsync, sysMap, net.kfState, &stationMap);
				}
			}
			
 			net.kfState.outputStates(netTrace);			
		}
		else
		{
			auto filenameSysMap = getSysOutputFilenames(acsConfig.clocks_filename, tsync);

			for (auto [filename, sysMap] : filenameSysMap)
			{
				outputClocks(filename, E_Ephemeris::KALMAN, E_Ephemeris::KALMAN, tsync, sysMap, KF_ARcopy, &stationMap);
			}
		}
		
		if (acsConfig.output_erp)
		{
			writeERPFromNetwork(net.erpFilename, net.kfState);
		}

		if	( (acsConfig.process_rts)
			&&(acsConfig.netwOpts.rts_lag > 0)
			&&(epoch >= acsConfig.netwOpts.rts_lag))
		{
			KFState rts = RTS_Process(KF_ARcopy, false, &stationMap);

			if (ARsol_ready())
			{
				auto filenameSysMap = getSysOutputFilenames(acsConfig.clocks_filename, tsync);

				for (auto [filename, sysMap] : filenameSysMap)
				{
					outputClocks(filename, E_Ephemeris::KALMAN, E_Ephemeris::KALMAN, tsync, sysMap, KF_ARcopy, &stationMap);
				}
			}
		}

		if	(  acsConfig.process_rts
			&& acsConfig.netwOpts.rts_lag > 0 
			&& epoch >= acsConfig.netwOpts.rts_lag)
		{
			KFState rts = RTS_Process(net.kfState, false, &stationMap, net.clockFilename, net.tropFilename);
		}
	}
	
	if (acsConfig.ssrOpts.calculate_ssr)
	{
		BOOST_LOG_TRIVIAL(info)
		<< "Preparing mongo data\n";
		
		if (ARsol_ready() 
		&& acsConfig.output_AR_clocks)
		{
			KFState KF_ARcopy = retrieve_last_ARcopy ();
			prepareSsrStates(netTrace, KF_ARcopy, tsync);
		}
		else
		{
			prepareSsrStates(netTrace, net.kfState, tsync);
		}
		//else
		{
// 				rtcmEncodeToFile();
		}
	}
	
	if (acsConfig.output_orbits)
	{
		outputSp3(tsync, acsConfig.orbits_data_source, &net.kfState);
	}

	if (acsConfig.output_persistance)
	{
		outputPersistanceNav();
	}
	
	if (acsConfig.output_mongo_states)
	{
		outputApriori		(stationMap);
		outputDeltaClocks	(stationMap);
	}
	
	for (auto& [id, s] : ntripBroadcaster.ntripUploadStreams)
	{
// 		auto& uploadStream = *s;
// 		
// 		auto trace = getTraceFile(uploadStream);
// 		trace << std::endl << "<<<<<<<<<<< Network Trace : Epoch " << epoch << " >>>>>>>>>>>" << std::endl;
	}	
	
	writeNetworkTraces(stationMap);
}

void mainOncePerEpoch(
	Network&		net,
	StationMap&		stationMap,
	GTime			tsync)
{
	Instrument	instrument	("1/Ep");
	TestStack	ts			("1/Ep");
	
	//load any changes from the config
	bool newConfig = acsConfig.parse();
	
	avoidCollisions(stationMap);
	
	//reload any new or modified files
	reloadInputFiles();

	addDefaultBiasSinex();
	
	createTracefiles(stationMap, net);
	
	//make any changes to streams.
	if (newConfig)
	{
		configureDownloadingStreams();
		configureUploadingStreams();
	}
	
	auto netTrace = getTraceFile(net);
	
	netTrace << std::endl << "------=============== Epoch " << epoch << " =============-----------" << std::endl;
	
	//initialise mongo if not already done
	mongoooo();

	//try to get svns of all used satellites
	for (auto& [satId, satNav] : nav.satNavMap)
	{
		SatSys Sat;
		Sat.fromHash(satId);
		
		PhaseCenterData* pcvsat_ptr;
		findAntenna(Sat.id(), tsync, nav, F1, &pcvsat_ptr);
		if (pcvsat_ptr)
		{
			Sat.setSvn(pcvsat_ptr->svn);
		}
	}
		
	//do per-station pre processing
	bool emptyEpoch = true;
#	ifdef ENABLE_PARALLELISATION
#	ifndef ENABLE_UNIT_TESTS
		Eigen::setNbThreads(1);
#		pragma omp parallel for
#	endif
#	endif
	for (int i = 0; i < stationMap.size(); i++)
	{
		auto rec_ptr_iterator = stationMap.begin();
		std::advance(rec_ptr_iterator, i);

		auto& [id, rec] = *rec_ptr_iterator;
		mainOncePerEpochPerStation(rec,nav.orography, nav.gptg, emptyEpoch);
	}
	Eigen::setNbThreads(0);

	if	(emptyEpoch)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Epoch " << epoch << " has no observations";
	}

	if (acsConfig.output_mongo_measurements)
	{
		mongoMeasSatStat_all(stationMap);
	}

	if (acsConfig.process_ppp)
	{
		PPP(netTrace, stationMap, net.kfState, nav.gptg, nav.orography);
	}

	if (acsConfig.output_rinex_nav)
	{
		writeRinexNav();
	}

	if (acsConfig.process_network)
	{
		networkEstimator(netTrace, stationMap, net.kfState, tsync);
	}

	if (1)	//acsConfig.ambiguityResolution
	{
		//ambiguityResolution(netTrace, net.kfState);
	}
	
	if (acsConfig.delete_old_ephemerides)
	{
		cullOldEphs(tsync);
		cullOldSSRs(tsync);
	}

	if (acsConfig.check_plumbing)
	{
		plumber();
	}
	
	mainPerEpochPostProcessingAndOutputs(net, stationMap);
	
	if (acsConfig.process_ionosphere)
	{
		update_ionosph_model(netTrace, stationMap, tsync);
	}
	
	TestStack::saveData();
	TestStack::testStatus();
}

void mainPostProcessing(
	Network&	net,
	StationMap&	stationMap)
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
		net.kfState.outputStates(netTrace, " Constrained");
		
		if (  acsConfig.ambrOpts.NLmode    != +E_ARmode::OFF 
		   && acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO )
		{
			KFState KF_ARcopy = retrieve_last_ARcopy();
			minimum(netTrace, KF_ARcopy);
		}

	}
	
	if (  acsConfig.ambrOpts.NLmode    != +E_ARmode::OFF 
	   && acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO )
		   dump__WLambg( netTrace );

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
		if (acsConfig.output_AR_clocks)
		{
			KFState KF_ARcopy = retrieve_last_ARcopy ();
			outputOrbit(KF_ARcopy);
		}	
		else
		{
			outputOrbit(net.kfState);
		}
	}

	if	(acsConfig.process_rts)
	{
		if (acsConfig.pppOpts.rts_lag < 0)
		{
			for (auto& [id, rec] : stationMap)
			{
				BOOST_LOG_TRIVIAL(info)
				<< std::endl
				<< "---------------PROCESSING PPP WITH RTS------------------------- " << std::endl;

				RTS_Process(rec.rtk.pppState,	true, &stationMap, "",					rec.rtsTropFilename);
			}
		}

		if (acsConfig.netwOpts.rts_lag < 0)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------PROCESSING NETWORK WITH RTS--------------------- " << std::endl;

			RTS_Process(net.kfState,		true, &stationMap, net.rtsClockFilename, net.rtsTropFilename, net.rtsErpFilename);
		}

		if (acsConfig.ionFilterOpts.rts_lag < 0)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------PROCESSING IONOSPHERE WITH RTS------------------ " << std::endl;

			RTS_Process(iono_KFState,		true, &stationMap);
		}
	}

	if (acsConfig.output_bias_sinex)
	{
		writeBiasSinex(netTrace, tsync, fileNames[acsConfig.bias_sinex_filename]);
	}

	TestStack::printStatus(true);
	Instrument::printStatus();

	outputSummaries(netTrace, stationMap);
}

int main(
	int		argc, 
	char**	argv)
{
	
	tracelevel(5);

	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::info);
	boost::log::add_console_log(std::cout, boost::log::keywords::format = "%Message%");

	BOOST_LOG_TRIVIAL(info)
	<< "PEA starting... (" << GINAN_BRANCH_NAME << " " GINAN_COMMIT_VERSION " from " << GINAN_COMMIT_DATE << ")" << std::endl;

	auto peaStartTime = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));

	bool pass = configure(argc, argv);
	if (pass == false)
	{
		BOOST_LOG_TRIVIAL(error) 	<< "Incorrect configuration";
		BOOST_LOG_TRIVIAL(info) 	<< "PEA finished";
		NtripSocket::io_service.stop();
		return EXIT_FAILURE;
	}
	
	if (acsConfig.output_log)
	{
		addFileLog();
	}
	
	exitOnErrors();

	TestStack::openData();

	BOOST_LOG_TRIVIAL(info)
	<< "Threading with " << Eigen::nbThreads()
	<< " threads" << std::endl;

	

	BOOST_LOG_TRIVIAL(info)
	<< "Logging with trace level:" << acsConfig.trace_level << std::endl << std::endl;

	tracelevel(acsConfig.trace_level);

	//read orography file for VMF3
	{
		int orog_sucess = false;
		if 	( acsConfig.tropOpts.orography	.empty() == false
			&&acsConfig.tropOpts.vmf3dir	.empty() == false)
		{
			orog_sucess = readorog(acsConfig.tropOpts.orography, nav.orography);
		}

		if (orog_sucess == false)
		{
			// read gpt2 grid file
			readgrid(acsConfig.tropOpts.gpt2grid, &nav.gptg);
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
	
	if	( acsConfig.ambrOpts.NLmode != +E_ARmode::OFF )
		config_AmbigResl();

	map<string, Station> stationMap;
	
	//prepare the satNavMap so that it at least has entries for everything
	{
		for (int prn = MINPRNGPS; prn <= MAXPRNGPS; prn++)	{	SatSys s(E_Sys::GPS, prn);	nav.satNavMap[s];	}
		for (int prn = MINPRNGLO; prn <= MAXPRNGLO; prn++)	{	SatSys s(E_Sys::GLO, prn);	nav.satNavMap[s];	}
		for (int prn = MINPRNGAL; prn <= MAXPRNGAL; prn++)	{	SatSys s(E_Sys::GAL, prn);	nav.satNavMap[s];	}
		for (int prn = MINPRNBDS; prn <= MAXPRNBDS; prn++)	{	SatSys s(E_Sys::BDS, prn);	nav.satNavMap[s];	}	
	}

	BOOST_LOG_TRIVIAL(debug)
	<< "\tCreating trace files ";

	Network net;
	{
		net.kfState.id					= "Net";
		net.kfState.max_filter_iter		= acsConfig.netwOpts.max_filter_iter;
		net.kfState.max_prefit_remv		= acsConfig.netwOpts.max_prefit_remv;
		net.kfState.inverter			= acsConfig.netwOpts.inverter;
		net.kfState.sigma_check			= acsConfig.netwOpts.sigma_check;
		net.kfState.sigma_threshold		= acsConfig.netwOpts.sigma_threshold;
		net.kfState.w_test				= acsConfig.netwOpts.w_test;
		net.kfState.chi_square_test		= acsConfig.netwOpts.chi_square_test;
		net.kfState.chi_square_mode		= acsConfig.netwOpts.chi_square_mode;
		net.kfState.output_residuals	= acsConfig.output_residuals;
		net.kfState.measRejectCallbacks	.push_back(deweightMeas);
		net.kfState.measRejectCallbacks	.push_back(incrementPhaseSignalError);
		net.kfState.stateRejectCallbacks.push_back(deweightByState);
	}

	if (acsConfig.process_rts)
	{
		if (acsConfig.netwOpts.rts_lag)
		{
			net.kfState.rts_lag					= acsConfig.netwOpts.rts_lag;
		}

		if (acsConfig.ionFilterOpts.rts_lag)
		{
			iono_KFState.rts_lag					= acsConfig.ionFilterOpts.rts_lag;	
		}
	}

	if (acsConfig.input_persistance)
	{
		BOOST_LOG_TRIVIAL(info)
		<< "Loading persistant states to continue processing...";

		inputPersistanceStates(stationMap, net.kfState);
	}

	acsConfig.parse();
	
	initialiseBiasSinex();

	reloadInputFiles();

	addDefaultBiasSinex();
	
	doDebugs();

	if (acsConfig.start_epoch.is_not_a_date_time() == false)
	{
		tsync.time = boost::posix_time::to_time_t(acsConfig.start_epoch);
	}

	BOOST_LOG_TRIVIAL(info)
	<< std::endl;
	BOOST_LOG_TRIVIAL(info)
	<< "Starting to process epochs...";

	TestStack ts(acsConfig.config_description);

	NtripSocket::startClients();
	
	//initialise mongo
	mongoooo();

	configureDownloadingStreams();
	configureUploadingStreams();
	
	//============================================================================
	// MAIN PROCESSING LOOP														//
	//============================================================================

	// Read the observations for each station and do stuff
	bool	complete					= false;							// When all input files are empty the processing is deemed complete - run until then, or until something else breaks the loop
	int		loopEpochs					= 0;								// A count of how many loops of epoch_interval this loop used up (usually one, but may be more if skipping epochs)
	auto	nextNominalLoopStartTime	= system_clock::now() + 10s;		// The time the next loop is expected to start - if it doesnt start until after this, it may be skipped
	while (complete == false)
	{
		if (tsync != GTime::noTime())
		{
			tsync.time				+= loopEpochs * acsConfig.epoch_interval;
		}

		epoch						+= loopEpochs;
		nextNominalLoopStartTime	+= loopEpochs * std::chrono::milliseconds((int)(acsConfig.wait_next_epoch * 1000));

		// Calculate the time at which we will stop waiting for data to come in for this epoch
		auto breakTime	= nextNominalLoopStartTime
						+ std::chrono::milliseconds((int)(acsConfig.wait_all_stations	* 1000));

		BOOST_LOG_TRIVIAL(info) << std::endl
		<< "Starting epoch #" << epoch;

		TestStack ts("Epoch " + std::to_string(epoch));
		
		for (auto& [id, rec] : stationMap)
		{
			rec.ready = false;
		}

		//get observations from streams (allow some delay between stations, and retry, to ensure all messages for the epoch have arrived)
		bool 	foundFirst		= false;
		bool	repeat			= true;
		int		stationCount	= 0;
		while	( repeat
				&&system_clock::now() < breakTime)
		{
			if (acsConfig.require_obs)
			{
				repeat = false;
			}

			for (auto& [id, s] : navStreamMultimap)
			{
				auto& navStream = *s;

				navStream.getNav();
			}

			//remove any dead streams
			for (auto iter = obsStreamMultimap.begin(); iter != obsStreamMultimap.end(); )
			{
				auto& [dummy, obsStream_ptr]	= *iter;
				auto& obsStream					= *obsStream_ptr;
				
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

			if	(  obsStreamMultimap		.empty()
				&& pseudoObsStreamMultimap	.empty()
				&& acsConfig.require_obs)
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

				if	( (rec.obsList.empty()		== false)
					&&(rec.obsList.front().time	== tsync))
				{
					//already have observations for this epoch.
					continue;
				}

				//try to get some data
				rec.obsList = obsStream.getObs(tsync);

				if (rec.obsList.empty())
				{
					//failed to get observations
					if (obsStream.obsWaitCode == +E_ObsWaitCode::NO_DATA_WAIT)
					{
						// try again later
						repeat = true;
						sleep_for(1ms);
					}
					
					continue;
				}
				
				if (tsync == GTime::noTime())
				{
					tsync.time				= ((int) (rec.obsList.front().time / acsConfig.epoch_interval)) * acsConfig.epoch_interval;
					acsConfig.start_epoch	= boost::posix_time::from_time_t(tsync);
					
					if (tsync + 0.5 < rec.obsList.front().time)
					{
						repeat = true;
						continue;	
					}
				}

				stationCount++;
				
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
					initialiseStation(id, rec);
				}
				
				rec.ready = true;
			}
			
			for (auto& [id, s] : pseudoObsStreamMultimap)
			{
				PseudoObsStream&	pseudoObsStream	= *s;
	
				auto& pseudoRec = stationMap[id];
				
				if	( (pseudoRec.pseudoObsList.empty()		== false)
					&&(pseudoRec.pseudoObsList.front().time	== tsync))
				{
					//already have observations for this epoch.
					continue;
				}
				
				//try to get some data
				pseudoRec.pseudoObsList = pseudoObsStream.getObs(tsync);

				if (pseudoRec.pseudoObsList.empty())
				{
					continue;
				}
				
				if (tsync == GTime::noTime())
				{
					tsync.time				= ((int)(pseudoRec.pseudoObsList.front().time / acsConfig.epoch_interval)) * acsConfig.epoch_interval;
					acsConfig.start_epoch	= boost::posix_time::from_time_t(tsync);
					
					if (tsync + 0.5 < pseudoRec.pseudoObsList.front().time)
					{
						repeat = true;
						continue;	
					}
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
			
			tsync = utc2gpst(timeget());
		}
		
		BOOST_LOG_TRIVIAL(info)
		<< "Synced " << stationCount << " stations...";
		
		auto epochStartTime	= boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
		{
			mainOncePerEpoch(net, stationMap, tsync);
		}
		auto epochStopTime	= boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));

		
		
		int week;
		double sec = time2gpst(tsync, &week);
		auto boostTime = boost::posix_time::from_time_t(tsync.time);

		BOOST_LOG_TRIVIAL(info)
		<< "Processed epoch #" << epoch
		<< " - " << "GPS time: " << week << " " << std::setw(6) << sec << " - " << boostTime 
		<< " (took " << (epochStopTime-epochStartTime) << ")";

		// Check end epoch
		if	(  acsConfig.end_epoch.is_not_a_date_time() == false
			&& boostTime >= acsConfig.end_epoch)
		{
			BOOST_LOG_TRIVIAL(info)
			<< "Exiting at epoch " << epoch << " (" << boostTime
			<< ") as end epoch " << acsConfig.end_epoch
			<< " has been reached";

			break;
		}

		// Check number of epochs
		if	(  acsConfig.max_epochs	> 0
			&& epoch					>= acsConfig.max_epochs)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "Exiting at epoch " << epoch << " (" << boostTime
			<< ") as epoch count " << acsConfig.max_epochs
			<< " has been reached";

			break;
		}

		// Calculate how many loops need to be skipped based on when the next loop was supposed to begin
		auto loopStopTime		= system_clock::now();
		auto loopExcessDuration = loopStopTime - (nextNominalLoopStartTime + std::chrono::milliseconds((int)(acsConfig.wait_all_stations * 1000)));
		int excessLoops			= loopExcessDuration / std::chrono::milliseconds((int)(acsConfig.wait_next_epoch * 1000));

		if (excessLoops < 0)		{	excessLoops = 0;	}
		if (excessLoops > 0)	
		{
			BOOST_LOG_TRIVIAL(warning) << std::endl 
			<< "Excessive time elapsed, skipping " << excessLoops 
			<< " epochs. Configuration 'wait_next_epoch' is " << acsConfig.wait_next_epoch;	
		}

		loopEpochs = 1 + excessLoops;
	}

	// Disconnect the downloading clients and stop the io_service for clean shutdown.
	for (auto& [id, s] : ntripRtcmMultimap)
	{
		auto& downStream = *s;
		downStream.disconnect();
	}
	ntripBroadcaster.stopBroadcast();
	NtripSocket::io_service.stop();

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

	return EXIT_SUCCESS;
}
