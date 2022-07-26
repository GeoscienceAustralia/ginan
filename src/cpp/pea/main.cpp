
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
#include "preprocessor.hpp"
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
#include "metaData.hpp"
#include "station.hpp"
#include "preceph.hpp"
#include "summary.hpp"
#include "antenna.hpp"
#include "satStat.hpp"
#include "fileLog.hpp"
#include "jpl_eph.hpp"
#include "common.hpp"
#include "orbits.hpp"
#include "gTime.hpp"
#include "debug.hpp"
#include "sinex.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "gpx.hpp"
#include "trop.h"
#include "vmf3.h"


Navigation		nav		= {};
int				epoch	= 1;
GTime			tsync	= GTime::noTime();

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
		for (auto& rnxfile			: acsConfig.rnx_files)			{	addStationDataFile(rnxfile,			"RINEX",	"OBS");			}	
		for (auto& rtcmfile			: acsConfig.obs_rtcm_files)		{	addStationDataFile(rtcmfile,		"RTCM",		"OBS");			}	
		for (auto& rtcmfile			: acsConfig.nav_rtcm_files)		{	addStationDataFile(rtcmfile,		"RTCM",		"NAV");			}
		for (auto& pseudoobsfile	: acsConfig.pseudoobs_files)	{	addStationDataFile(pseudoobsfile,	"SP3",		"PSEUDO");		}

		if	( obsStreamMultimap.empty()
			&&acsConfig.require_obs)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: No station observation (rnx) files found in "
			<< acsConfig.root_stations_dir;
		}
	}

	removeInvalidFiles(acsConfig.atx_files);
	for (auto& atxfile : acsConfig.atx_files)
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
			<< "Error: Unable to load ATX from file " << atxfile;

			continue;
		}
	}

	removeInvalidFiles(acsConfig.erp_files);
	for (auto& erpfile : acsConfig.erp_files)
	{
		if (fileChanged(erpfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading ERP file " << erpfile;

		readerp(erpfile, nav.erp);
	}

	removeInvalidFiles(acsConfig.orb_files);
	removeInvalidFiles(acsConfig.sp3_files);
	if (acsConfig.orb_files.empty() == false)
	{
		bool updated = false;

		/* orbit info from orbit file */
		for (auto& orbfile : acsConfig.orb_files)
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
		for (auto& sp3file : acsConfig.sp3_files)
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

	removeInvalidFiles(acsConfig.nav_files);
	for (auto& navfile : acsConfig.nav_files)
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

	removeInvalidFiles(acsConfig.clk_files);
	for (auto& clkfile : acsConfig.clk_files)
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

	removeInvalidFiles(acsConfig.dcb_files);
	for (auto& dcbfile : acsConfig.dcb_files)
	{
		if (fileChanged(dcbfile) == false)
		{
			continue;
		}

		/* DCB file */
		BOOST_LOG_TRIVIAL(info)
		<< "Loading DCB file " << dcbfile;

		readdcb(dcbfile);
	}

	removeInvalidFiles(acsConfig.bsx_files);
	for (auto& bsxfile : acsConfig.bsx_files)
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

	removeInvalidFiles(acsConfig.ion_files);
	for (auto& ionfile : acsConfig.ion_files)
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
	removeInvalidFiles(acsConfig.snx_files);
	for (auto& snxfile : acsConfig.snx_files)
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
			<< "Error: Unable to load SINEX file " << snxfile;

			continue;
		}

		once = false;
	}

	removeInvalidFiles(acsConfig.egm_files);
	for (auto& egmfile : acsConfig.egm_files)
	{
		if (fileChanged(egmfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading EGMCoef file " << egmfile;

		readegm(egmfile, nav.egm);
	}	

	removeInvalidFiles(acsConfig.jpl_files);
	for (auto& jplfile : acsConfig.jpl_files)
	{
		if (fileChanged(jplfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading JPL file " << jplfile;
		
		nav.jplEph_ptr = (struct jpl_eph_data*) jpl_init_ephemeris(jplfile.c_str(), nullptr, nullptr);          // a Pointer to The jpl_eph_data Structure
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
			auto outStream_ptr = std::make_shared<NtripUploader>(outStreamData.url);
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
		set<string>* streamUrls_ptr;
		if (nav == false)	streamUrls_ptr = &acsConfig.pppOpts.obs_mount_url;
		else				streamUrls_ptr = &acsConfig.pppOpts.nav_mount_url;

		auto& streamUrls = *streamUrls_ptr;

		map<string, string> ntripStreams;
		for (auto fullUrl : streamUrls)
		{
			std::size_t slashPos = fullUrl.find_last_of("/");	// find first 4 characters after last '/'			//todo aaron, lots of redundancy here, same code in config file, and below
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
				BOOST_LOG_TRIVIAL(warning) << "Warning: Removing downloading mount point : " << id << std::endl;
				if (nav == false)		{	obsStreamMultimap.erase(id);	}
				else					{	navStreamMultimap.erase(id);	}
			}
		}
	}
}

bool createNewTraceFile(
	const string				id,
	boost::posix_time::ptime	logptime,
	string  					new_path_trace,
	string& 					old_path_trace,
	bool						outputHeader = false,
	bool						outputConfig = false)
{	
	replaceString(new_path_trace, "<STATION>", id);
	replaceTimes (new_path_trace, logptime);

	// Create the trace file if its a new filename, otherwise, keep the old one
	if (new_path_trace == old_path_trace)
	{
		//the filename is the same, keep using the old ones
		return false;
	}

	old_path_trace = new_path_trace;

	BOOST_LOG_TRIVIAL(debug)
	<< "Creating new file for " << id << " at " << old_path_trace;

	std::ofstream trace(old_path_trace);
	if (!trace)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error: Could not create file for " << id << " at " << old_path_trace;

		return false;
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
	
	return true;
}

map<string, string> fileNames;

/** Create new empty trace files only when required when the filename is changed
 */
void createTracefiles(
	map<string, Station>&	stationMap,
	Network&				net)
{
	string dummyStr;
	GTime logtime = tsync.roundTime(acsConfig.rotate_period);
	
	boost::posix_time::ptime	logptime	= boost::posix_time::from_time_t(logtime.time);
	
	if (logtime.time == GTime::noTime())
	{
		logptime = boost::posix_time::not_a_date_time;
	}
	
	// Ensure the output directories exist
	for (auto directory : {
								acsConfig.erp_directory,
								acsConfig.gpx_directory,
								acsConfig.log_directory,
								acsConfig.test_directory,
								acsConfig.ionex_directory,
								acsConfig.sinex_directory,
								acsConfig.trace_directory,
								acsConfig.orbits_directory,
								acsConfig.clocks_directory,
								acsConfig.ionstec_directory,
								acsConfig.ppp_sol_directory,
								acsConfig.rtcm_nav_directory,
								acsConfig.rtcm_obs_directory,
								acsConfig.rinex_obs_directory,
								acsConfig.rinex_nav_directory,
								acsConfig.trop_sinex_directory,
								acsConfig.bias_sinex_directory,
								acsConfig.persistance_directory,
								acsConfig.pppOpts.rts_directory,
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
	
	for (auto rts : {false, true})
	{
		if	(	rts 
			&&(	acsConfig.process_rts		== false
			  ||acsConfig.pppOpts.rts_lag	== 0))
		{
			continue;
		}

		
		string suff = "";
		
		if (rts)
		{
			suff = SMOOTHED_SUFFIX;
			
			if (acsConfig.process_network)
			{
				bool newTraceFile = createNewTraceFile(net.id,		boost::posix_time::not_a_date_time,	acsConfig.pppOpts.rts_filename,		net.kfState.rts_basename);
			
				if (newTraceFile)
				{
	// 				std::cout << std::endl << "new trace file";
					std::remove((net.kfState.rts_basename					).c_str());
					std::remove((net.kfState.rts_basename + FORWARD_SUFFIX	).c_str());
					std::remove((net.kfState.rts_basename + BACKWARD_SUFFIX	).c_str());
				}
			}
			
			if (acsConfig.process_user)
			for (auto& [id, rec] : stationMap)
			{
				bool newTraceFile = createNewTraceFile(id,			boost::posix_time::not_a_date_time,	acsConfig.pppOpts.rts_filename,		rec.pppState.rts_basename);
				
				if (newTraceFile)
				{
// 					std::cout << std::endl << "new trace file";
					std::remove((rec.pppState.rts_basename					).c_str());
					std::remove((rec.pppState.rts_basename + FORWARD_SUFFIX	).c_str());
					std::remove((rec.pppState.rts_basename + BACKWARD_SUFFIX).c_str());
				}
			}
		}
		
		for (auto& [id, rec] : stationMap)
		{
			bool newTraceFile = false;
			
			if (acsConfig.output_station_trace)
			{
				newTraceFile |= createNewTraceFile(id,			logptime,	acsConfig.station_trace_filename	+ suff,	rec.pppState.metaDataMap[TRACE_FILENAME_STR		+ suff],	true,	acsConfig.output_config);
				
				if (suff.empty())
				{
					rec.traceFilename = rec.pppState.metaDataMap[TRACE_FILENAME_STR];
				}
			}
			
			if	(	acsConfig.output_trop_sinex
				&&	acsConfig.process_user)
			{
				newTraceFile |= createNewTraceFile(id,			logptime,	acsConfig.trop_sinex_filename		+ suff,	rec.pppState.metaDataMap[TROP_FILENAME_STR		+ suff]);
			}
			
			if (acsConfig.output_ppp_sol)
			{
				newTraceFile |= createNewTraceFile(id,			logptime,	acsConfig.ppp_sol_filename			+ suff,	rec.pppState.metaDataMap[SOLUTION_FILENAME_STR	+ suff],	true,	acsConfig.output_config);
			}
			
			if (acsConfig.output_gpx)
			{
				newTraceFile |= createNewTraceFile(id,			logptime,	acsConfig.gpx_filename				+ suff,	rec.pppState.metaDataMap[GPX_FILENAME_STR		+ suff]);
			}
			
					
			if	(  rts 
				&& newTraceFile)
			{
				spitFilterToFile(rec.pppState.metaDataMap,	E_SerialObject::METADATA, rec.pppState.rts_basename + FORWARD_SUFFIX); 
			}
		}
		
		bool newTraceFile = false;
		
		if (acsConfig.output_network_trace)
		{
			newTraceFile |= createNewTraceFile(net.id,	logptime,	acsConfig.network_trace_filename	+ suff,	net.kfState.metaDataMap[TRACE_FILENAME_STR		+ suff],	true,	acsConfig.output_config);
			
			if (suff.empty())
			{
				net.traceFilename = net.kfState.metaDataMap[TRACE_FILENAME_STR];
			}
		}

		if (acsConfig.output_ionex)
		{
			newTraceFile |= createNewTraceFile("",		logptime,	acsConfig.ionex_filename			+ suff,	net.kfState.metaDataMap[IONEX_FILENAME_STR		+ suff]);
		}

		if (acsConfig.output_ionstec)
		{
			newTraceFile |= createNewTraceFile("",		logptime,	acsConfig.ionstec_filename			+ suff,	net.kfState.metaDataMap[IONSTEC_FILENAME_STR	+ suff]);
		}
		
	
		if	( acsConfig.output_trop_sinex
			&&acsConfig.process_network)
		{	
			newTraceFile |= createNewTraceFile(net.id,	logptime,	acsConfig.trop_sinex_filename		+ suff,	net.kfState.metaDataMap[TROP_FILENAME_STR		+ suff]);
		}

		if	( acsConfig.output_erp
			&&acsConfig.process_network)
		{	
			newTraceFile |= createNewTraceFile(net.id,	logptime,	acsConfig.erp_filename				+ suff,	net.kfState.metaDataMap[ERP_FILENAME_STR		+ suff]);
		}
		
		if (acsConfig.output_clocks)
		{
			auto filenameMap = getSysOutputFilenames(acsConfig.clocks_filename,		tsync);
			for (auto& [filename, dummy] : filenameMap)
			{
				newTraceFile |= createNewTraceFile(net.id,	logptime,	filename + suff,						fileNames[filename + suff]);
			}
		}	
		
		if	(  rts 
			&& newTraceFile)
		{
			spitFilterToFile(net.kfState.metaDataMap,	E_SerialObject::METADATA, net.kfState.rts_basename + FORWARD_SUFFIX); 
		}
	}
	
	if (acsConfig.output_log)
	{
		createNewTraceFile("",			logptime,	acsConfig.log_filename,								FileLog::path_log);
	}
	
	if	(acsConfig.output_decoded_rtcm_json)
	{
		createNewTraceFile(net.id,		logptime,	acsConfig.decoded_rtcm_json_filename,				acsConfig.decoded_rtcm_json_filename);	//todo aaron, these will collide after first iteratio?
	}

	if	(acsConfig.output_encoded_rtcm_json)
	{
		createNewTraceFile(net.id,		logptime,	acsConfig.encoded_rtcm_json_filename,				acsConfig.encoded_rtcm_json_filename);
	}

	if (acsConfig.output_rinex_obs)
	{
		for (auto& [id, rec] : stationMap)
		{
			auto filenameMap = getSysOutputFilenames(acsConfig.rinex_obs_filename,	tsync, id);
			for (auto& [filename, dummy] : filenameMap)
			{
				createNewTraceFile(id,		logptime,	filename,										fileNames[filename]);
			}
		}
	}
	
	if (acsConfig.output_rinex_nav)
	{
		auto filenameMap = getSysOutputFilenames(acsConfig.rinex_nav_filename,	tsync);
		for (auto& [filename, dummy] : filenameMap)
		{
			createNewTraceFile("",		logptime,	filename,											fileNames[filename]);
		}
	}

	if (acsConfig.output_orbits)
	{
		auto filenameMap = getSysOutputFilenames(acsConfig.orbits_filename,		tsync);
		for (auto& [filename, dummy] : filenameMap)
		{
			createNewTraceFile(net.id,	logptime,	filename,											fileNames[filename]);
		}
	}
		
	if (acsConfig.output_bias_sinex)
	{
		createNewTraceFile("", 		logptime,	acsConfig.bias_sinex_filename,	fileNames[acsConfig.bias_sinex_filename]);
	}	
}

void avoidCollisions(
	StationMap&		stationMap)
{
	for (auto& [id, rec] : stationMap)
	{
		auto trace = getTraceFile(rec);
	}
	
	// if the vmf3 option has been selected:
	if	(acsConfig.model.trop.vmf3dir.empty() == false)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "VMF3 option chosen";
		
		double ep[6];
		time2epoch(tsync, ep);
		
		double jd = ymdhms2jd(ep);

		// read vmf3 grid information
		nav.vmf3.m = 1;
		udgrid(acsConfig.model.trop.vmf3dir.c_str(), nav.vmf3, jd);
	}
}

void initialiseStation(
	string		id,
	Station&	rec)
{
	BOOST_LOG_TRIVIAL(debug)
	<< "Initialising station " << id;

	Instrument	instrument(__FUNCTION__);
	
	rec.id = id;

	// Read the BLQ file
	bool found = false;
	for (auto& blqfile : acsConfig.blq_files)
	{
		found = readblq(blqfile, id.c_str(), rec.otlDisplacement);

		if (found)
		{
			break;
		}
	}

	if (found == false)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: No BLQ for " << id;
	}

	if (acsConfig.process_user)
	{
		rec.pppState.id					= id;
		rec.pppState.max_filter_iter	= acsConfig.pppOpts.max_filter_iter;
		rec.pppState.max_prefit_remv	= acsConfig.pppOpts.max_prefit_remv;
		rec.pppState.inverter			= acsConfig.pppOpts.inverter;
		rec.pppState.sigma_threshold	= acsConfig.pppOpts.sigma_threshold;
		rec.pppState.sigma_check		= acsConfig.pppOpts.sigma_check;
		rec.pppState.w_test				= acsConfig.pppOpts.w_test;
		rec.pppState.chi_square_test	= acsConfig.pppOpts.chi_square_test;
		rec.pppState.chi_square_mode	= acsConfig.pppOpts.chi_square_mode;
		rec.pppState.output_residuals	= acsConfig.output_residuals;

		rec.pppState.measRejectCallbacks	.push_back(countSignalErrors);
		rec.pppState.measRejectCallbacks	.push_back(deweightMeas);
		rec.pppState.stateRejectCallbacks	.push_back(deweightByState);
		rec.pppState.stateRejectCallbacks	.push_back(clockGlitchReaction);
	}

	if	( acsConfig.process_rts
		&&acsConfig.pppOpts.rts_lag)
	{
		rec.pppState.rts_lag = acsConfig.pppOpts.rts_lag;
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
	Instrument instrument(__FUNCTION__);
	
	if (rec.ready == false)
	{
		return;
	}
	
	sinexPerEpochPerStation(tsync, rec);		//todo aaron, move out of preprocessor?
	
	emptyEpoch = false;

	auto trace = getTraceFile(rec);

	trace << std::endl << "------=============== Epoch " << epoch << " =============-----------" << std::endl;
	trace << std::endl << "------=============== Time  " << boost::posix_time::from_time_t(tsync.time) << " =============-----------" << std::endl;

	BOOST_LOG_TRIVIAL(trace)
	<< "Read " << rec.obsList.size()
	<< " observations for station " << rec.id;

	//calculate statistics
	{
		Instrument instrument("Statistics");
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
		Instrument instrument("ppp");
		pppos(trace, rec.obsList, rec);
		
		if (rec.sol.stat != SOLQ_NONE)
		{
			int nfixed = 0;
			
			rec.pppState.outputStates(trace);
			pppoutstat(trace, rec.pppState);
			
			if (acsConfig.ambrOpts.NLmode != +E_ARmode::OFF)
			{
				TempDisabler td(acsConfig.output_mongo_measurements);
				
				nfixed = enduserAmbigResl(trace, rec.obsList, rec.pppState, rec.snx.pos, rec.sol.dop[2], rec.pppState.metaDataMap[SOLUTION_FILENAME_STR]);
			}
			
			trace << std::endl << "Solution with " << nfixed << " ambigities resolved";
			
			if (acsConfig.output_ppp_sol)
			{
				outputPPPSolution(rec.pppState.metaDataMap[SOLUTION_FILENAME_STR], rec);
			}
		}
	}

	if (acsConfig.process_network)
	for (auto once : {1})
	{
		Instrument instrument("net");
		// If there is no antenna information skip processing this station
		if (rec.snx.anttype.empty())
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: \tNo Antenna Information for " << rec.id
			<< " skipping this station";

			break;
		}

		// if the vmf3 option has been selected, use it, otherwise send null
		vmf3_t*	rtkVmf3_ptr = nullptr;
		double*	rtkOrog_ptr = nullptr;
		if	(acsConfig.model.trop.vmf3dir.empty() == false)		//todo aaron, use the option rather than the filename
		{
			rtkVmf3_ptr = &nav.vmf3;
			rtkOrog_ptr = orog;
		}

		/* observed minus computed for each satellites */
		pppomc(trace, rec.obsList, gptg, rec, rtkVmf3_ptr, rtkOrog_ptr);
	}

	if	(  acsConfig.process_rts
		&& acsConfig.pppOpts.rts_lag > 0
		&& epoch >= acsConfig.pppOpts.rts_lag)
	{
		KFState rts = RTS_Process(rec.pppState);
	}

	if	(  acsConfig.process_user
		&& acsConfig.output_mongo_states)
	{
		mongoStates(rec.pppState);
	}

	if (acsConfig.output_rinex_obs)
	{
		writeRinexObs(rec.id, rec.snx, tsync, rec.obsList, acsConfig.rinex_obs_version);
	}

	if (acsConfig.output_gpx)
	{
		writeGPX(rec.pppState.metaDataMap[GPX_FILENAME_STR], rec.id, rec.pppState);
	}
}

void mainPerEpochPostProcessingAndOutputs(
	Network&		net,
	StationMap&		stationMap)
{
	TestStack	ts(__FUNCTION__);
	Instrument	instrument(__FUNCTION__);
	
	auto netTrace = getTraceFile(net);

	if (acsConfig.process_user)
	{
		if (acsConfig.output_trop_sinex)
		for (auto& [id, rec] : stationMap)
		{
			outputTropSinex(rec.pppState.metaDataMap["tropFilename"], tsync, stationMap, id);
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
			TempDisabler td(acsConfig.output_mongo_measurements);
				
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
		
		net.kfState.outputStates(netTrace);
		
		if (acsConfig.output_clocks)
		{
			auto filenameSysMap = getSysOutputFilenames(acsConfig.clocks_filename, tsync);
			
			if	( acsConfig.output_ar_clocks == false
				||ARsol_ready() == false)
			{
				for (auto [filename, sysMap] : filenameSysMap)
				{
					outputClocks(filename, acsConfig.clocks_receiver_source, acsConfig.clocks_satellite_source, tsync, sysMap, net.kfState, &stationMap);
				}
			}
			else
			{
				for (auto [filename, sysMap] : filenameSysMap)
				{
					outputClocks(filename, E_Ephemeris::KALMAN, E_Ephemeris::KALMAN, tsync, sysMap, KF_ARcopy, &stationMap);
				}
			}
		}
		
		if (acsConfig.output_erp)
		{
			writeERPFromNetwork(net.kfState.metaDataMap[ERP_FILENAME_STR], net.kfState);
		}

		if	(  acsConfig.process_rts
			&& acsConfig.pppOpts.rts_lag > 0
			&& epoch >= acsConfig.pppOpts.rts_lag)
		{
			RTS_Process(net.kfState,	false, &stationMap);
			RTS_Process(KF_ARcopy,		false, &stationMap);

			if (ARsol_ready())
			{
				auto filenameSysMap = getSysOutputFilenames(acsConfig.clocks_filename, tsync);

				for (auto [filename, sysMap] : filenameSysMap)
				{
					outputClocks(filename, E_Ephemeris::KALMAN, E_Ephemeris::KALMAN, tsync, sysMap, KF_ARcopy, &stationMap);
				}
			}
		}
	}
	
	if (acsConfig.ssrOpts.calculate_ssr)
	{
		BOOST_LOG_TRIVIAL(info)
		<< "Calculating SSR message precursors\n";
		
		if	(  ARsol_ready() 
			&& acsConfig.output_ar_clocks)
		{
			KFState KF_ARcopy = retrieve_last_ARcopy();
			prepareSsrStates(netTrace, KF_ARcopy,	tsync);
		}
		else
		{
			prepareSsrStates(netTrace, net.kfState,	tsync);
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
}

void mainOncePerEpoch(
	Network&		net,
	StationMap&		stationMap,
	GTime			time)
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
	netTrace << std::endl << "------=============== Time  " << boost::posix_time::from_time_t(time.time) << " =============-----------" << std::endl;
	
	//initialise mongo if not already done
	mongoooo();

	//try to get svns of all used satellites
	for (auto& [satId, satNav] : nav.satNavMap)
	{
		SatSys Sat;
		Sat.fromHash(satId);
		
		PhaseCenterData* pcvsat_ptr = nullptr;
		findAntenna(Sat.id(), time, nav, F1, &pcvsat_ptr);
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
		<< "Warning: Epoch " << epoch << " has no observations";
	}

	if (acsConfig.output_mongo_measurements)
	{
		mongoMeasSatStat(stationMap);
	}

	if (acsConfig.process_ppp)
	{
		PPP(netTrace, stationMap, net.kfState, nav.gptg, nav.orography);
	}

	if (acsConfig.output_rinex_nav)
	{
		writeRinexNav(acsConfig.rinex_nav_version);
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
		updateIonosphereModel(netTrace, net.kfState.metaDataMap[IONSTEC_FILENAME_STR], net.kfState.metaDataMap[IONEX_FILENAME_STR], stationMap, time);
	}
	
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
		ionexFileWrite(netTrace, net.kfState.metaDataMap[IONEX_FILENAME_STR], tsync, true);
	}

	if	( acsConfig.process_network
		&&acsConfig.process_minimum_constraints)
	{
		BOOST_LOG_TRIVIAL(info)
		<< std::endl
		<< "---------------PROCESSING NETWORK WITH MINIMUM CONSTRAINTS ------------- " << std::endl;
		
		mincon(netTrace, net.kfState);
	}
	
	if	(  acsConfig.ambrOpts.NLmode	!= +E_ARmode::OFF 
		&& acsConfig.ionoOpts.corr_mode	== +E_IonoMode::IONO_FREE_LINEAR_COMBO)
	{
		dump_WLambg(netTrace);
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
		outputOrbit(net.kfState);
	}

	if	(acsConfig.process_rts)
	{
		if	(  acsConfig.process_user
			&& acsConfig.pppOpts.rts_lag < 0)
		{
			for (auto& [id, rec] : stationMap)
			{
				BOOST_LOG_TRIVIAL(info)
				<< std::endl
				<< "---------------PROCESSING PPP WITH RTS------------------------- " << std::endl;

				RTS_Process(rec.pppState,	true, &stationMap);
			}
		}

		if	( acsConfig.process_network
			&&acsConfig.pppOpts.rts_lag < 0)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------PROCESSING NETWORK WITH RTS--------------------- " << std::endl;

			RTS_Process(net.kfState,		true, &stationMap);
		}

		if	( acsConfig.process_ionosphere
			&&acsConfig.pppOpts.rts_lag < 0)
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

	if (acsConfig.testOpts.enable)
	{
		TestStack::printStatus(true);
		TestStack::saveData();
		Instrument::printStatus();
	}

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

	exitOnErrors();
	
	bool pass = configure(argc, argv);
	if (pass == false)
	{
		BOOST_LOG_TRIVIAL(error) 	<< "Error: Incorrect configuration";
		BOOST_LOG_TRIVIAL(info) 	<< "PEA finished";
		NtripSocket::io_service.stop();
		return EXIT_FAILURE;
	}
	
	if (acsConfig.output_log)
	{
		addFileLog();
	}
	

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
		if 	( acsConfig.model.trop.orography.empty() == false		//todo aaron, check the option, not the filename, or just read anyway
			&&acsConfig.model.trop.vmf3dir	.empty() == false)
		{
			orog_sucess = readorog(acsConfig.model.trop.orography, nav.orography);
		}

		if (orog_sucess == false)
		{
			// read gpt2 grid file
			readgrid(acsConfig.model.trop.gpt2grid, &nav.gptg);
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

	Network net;
	{
		net.kfState.id					= "Net";
		net.kfState.max_filter_iter		= acsConfig.pppOpts.max_filter_iter;
		net.kfState.max_prefit_remv		= acsConfig.pppOpts.max_prefit_remv;
		net.kfState.inverter			= acsConfig.pppOpts.inverter;
		net.kfState.sigma_check			= acsConfig.pppOpts.sigma_check;
		net.kfState.sigma_threshold		= acsConfig.pppOpts.sigma_threshold;
		net.kfState.w_test				= acsConfig.pppOpts.w_test;
		net.kfState.chi_square_test		= acsConfig.pppOpts.chi_square_test;
		net.kfState.chi_square_mode		= acsConfig.pppOpts.chi_square_mode;
		net.kfState.output_residuals	= acsConfig.output_residuals;
		net.kfState.measRejectCallbacks	.push_back(deweightMeas);
		net.kfState.measRejectCallbacks	.push_back(incrementPhaseSignalError);
		net.kfState.stateRejectCallbacks.push_back(deweightByState);
	}

	if	(  acsConfig.process_rts
		&& acsConfig.pppOpts.rts_lag)
	{
		net.kfState.rts_lag		= acsConfig.pppOpts.rts_lag;
		iono_KFState.rts_lag	= acsConfig.pppOpts.rts_lag;
	}

	//initialise mongo
	mongoooo();
	
	if (acsConfig.rts_only)
	{
		net.kfState.rts_lag = 4000;
		net.kfState.rts_basename = acsConfig.pppOpts.rts_filename;
		
		RTS_Process(net.kfState);
		
		exit(0);
	}
	
	if (acsConfig.input_persistance)
	{
		BOOST_LOG_TRIVIAL(info)
		<< "Loading persistant states to continue processing...";

		inputPersistanceStates(stationMap, net.kfState);
	}
	
	initialiseBiasSinex();

	reloadInputFiles();

	addDefaultBiasSinex();

	NtripSocket::startClients();

	configureDownloadingStreams();
	configureUploadingStreams();
	
	for (auto& [id, s] : obsStreamMultimap)
	{
		auto& rec = stationMap[id];
		
		initialiseStation(id, rec);
	}
	
	createTracefiles(stationMap, net);
	
	//initialise mongo
	mongoooo();
	
	if (acsConfig.mincon_only)
	{
		minconOnly(std::cout, stationMap);
	}
	
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
			clearSlips(rec.obsList);
		}

		//get observations from streams (allow some delay between stations, and retry, to ensure all messages for the epoch have arrived)
		map<string, bool>	dataAvailableMap;
		bool 				foundFirst	= false;
		bool				repeat		= true;
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

				//try to get some data (again)
				if (rec.ready == false)
				{
					bool moreData = true;
					while (moreData)
					{
						rec.obsList = obsStream.getObs(tsync);

						switch (obsStream.obsWaitCode)
						{
							case E_ObsWaitCode::EARLY_DATA:							preprocessor(rec);	obsStream.eatObs();	break;
							case E_ObsWaitCode::OK:				moreData = false;	preprocessor(rec);	obsStream.eatObs();	break;
							case E_ObsWaitCode::NO_DATA_WAIT:	moreData = false;											break;
							case E_ObsWaitCode::NO_DATA_EVER:	moreData = false;											break;
						}
					}
				}
				
				if (rec.obsList.empty())
				{
					//failed to get observations
					if (obsStream.obsWaitCode == +E_ObsWaitCode::NO_DATA_WAIT)
					{
						// try again later
						repeat = true;
						sleep_for(2ms);
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

				dataAvailableMap[rec.id] = true;
				
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
		<< "Synced " << dataAvailableMap.size() << " stations...";
		
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
			<< "Warning: Excessive time elapsed, skipping " << excessLoops 
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
