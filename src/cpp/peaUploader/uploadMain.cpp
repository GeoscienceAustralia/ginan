#include <sys/time.h>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <memory>
#include <chrono>
#include <thread>
#include <string>

using namespace std::literals::chrono_literals;
using std::chrono::system_clock;
using std::chrono::time_point;
using std::this_thread::sleep_for;
using std::string;

#include <boost/log/utility/setup/console.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/log/trivial.hpp>
#include <boost/filesystem.hpp>


#include "peaCommitVersion.h"
#include "ntripBroadcast.hpp"
#include "streamTrace.hpp"
#include "acsConfig.hpp"
#include "mongoRead.hpp"
#include "acsStream.hpp"
#include "fileLog.hpp"
#include "gTime.hpp"

nav_t		nav		= {};
int			epoch	= 0;
GTime		tsync	= GTime::noTime();


// Function included for code separation 
Matrix3d ecef2rac(
	Vector3d& rSat,				// Sat position (ECEF)
	Vector3d& satVel)			// Sat velocity (ECEF)
{
	// Ref: RTCM c10403.3, equation (3.12-5), p188 (this rotation matrix performs RAC->ECEF, so ECEF->RAC is simply the transpose of this)
											Vector3d ea = satVel.normalized();
	Vector3d rv = rSat.cross(satVel);		Vector3d ec = rv.normalized();
											Vector3d er = ea.cross(ec);

	Matrix3d Rt;
	Rt.row(0) = er;
	Rt.row(1) = ea;
	Rt.row(2) = ec;

	return Rt;
}

void configureUploadingStreams()
{
	for (auto& [outLabel,outStreamData] : acsConfig.uploadingStreamData)
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
		outStream.streamConfig.itrf_datum			= outStreamData.itrf_datum;
		outStream.streamConfig.provider_id			= outStreamData.provider_id;
		outStream.streamConfig.solution_id			= outStreamData.solution_id;
		outStream.streamConfig.update_interval		= outStreamData.update_interval;
		outStream.streamConfig.master_iod			= outStreamData.master_iod;
	}

	for (auto it = ntripBroadcaster.ntripUploadStreams.cbegin(); it != ntripBroadcaster.ntripUploadStreams.cend();)
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

void createNewTraceFile(
	const string&				id,
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

/** Create new empty trace files only when required when the filename is changed
 */
void createTracefiles()
{
	GTime logtime = tsync.roundTime(acsConfig.trace_rotate_period);
	
	boost::posix_time::ptime	logptime	= boost::posix_time::from_time_t(logtime.time);
// 	boost::posix_time::ptime	noptime		= boost::posix_time::not_a_date_time;

	if (acsConfig.output_log )
	{	
		createNewTraceFile("",			logptime,	acsConfig.log_filename,								FileLog::path_log,						false,	false);
	}

	for (auto& [id, upStream_ptr] : ntripBroadcaster.ntripUploadStreams)
	{
		auto& upStream = *upStream_ptr;
		
// 		createNewTraceFile(id + "-UP",	logptime,	acsConfig.trace_filename,							upStream.traceFilename,					true,	acsConfig.output_config);
	}
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
	
	for (auto& directory : 	{ 
								acsConfig.log_directory,
								acsConfig.trace_directory
							})
	{						
		if (directory.empty() == false)
		{
			boost::filesystem::create_directories(directory);
		}
	}

	for (auto& [id, s] : ntripBroadcaster.ntripUploadStreams)
	{
		auto& uploadStream = *s;
		
		uploadStream.print_stream_statistics = acsConfig.print_stream_statistics;
	}

	acsConfig.delete_mongo_history = false;
	mongoooo();
	if (mongo_ptr == nullptr)
	{
		BOOST_LOG_TRIVIAL(error) << "Failed to connect to Mongo DB exiting.";
		exit(1);
	}
	BOOST_LOG_TRIVIAL(info) << "Connected to Mongo DB : " << acsConfig.mongo_uri;

	tsync = timeget();
	tsync.sec = 0;

	ntripBroadcaster.startBroadcast();

	// Loop that keeps the logging updated on a time delay.
	// Sending of RTCM messages have their own timing loops.
	while (true)
	{
		BOOST_LOG_TRIVIAL(info) << "Main Logging loop, epoch : " << epoch << std::endl;
		std::this_thread::sleep_until(std::chrono::system_clock::from_time_t(tsync.time));
		tsync.time += 30;
		epoch++;

		//load any changes from the config
		acsConfig.parse();
		createTracefiles();
		configureUploadingStreams();
		
		for (auto& [id, s] : ntripBroadcaster.ntripUploadStreams)
		{
// 			auto& uploadStream = *s;
// 			
// 			auto trace = getTraceFile(uploadStream);
// 			trace << std::endl << "<<<<<<<<<<< Network Trace : Epoch " << epoch << " >>>>>>>>>>>" << std::endl;
		}	
	}

// 	// Disconnect the downloading clients and stop the io_service for clean shutdown.
// 	for (auto& [id, s] : ntripRtcmMultimap)
// 	{
// 		auto& downStream = *s;
// 		downStream.disconnect();
// 	}
// 	outStreamManager.stopBroadcast();
// 	NtripSocket::io_service.stop();
}
