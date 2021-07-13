#include "ntripCasterService.hpp"


void NtripCasterService::startPerformanceMonitoring()
{
	NtripSourceTable sourceTable(caster_stream_root);
	sourceTable.getSourceTable();
	sourceTableData = sourceTable.sourceTableData;
	
	for (auto& sourceEntry : sourceTableData )
	{
		std::string streamUrl = caster_stream_root + sourceEntry.mountPoint;
		auto ntripStream_ptr = std::make_shared<NtripRtcmStream>(streamUrl);
		
		downloadStreamMap.insert({sourceEntry.mountPoint,ntripStream_ptr});
	}

	if (acsConfig.start_epoch.is_not_a_date_time() == false)
	{
		tsync.time = boost::posix_time::to_time_t(acsConfig.start_epoch);
	}
	else
	{
		tsync.time = system_clock::to_time_t(system_clock::now());
	}
	double tgap = acsConfig.epoch_interval;    
	
	makeTraceFiles();
	
	epoch = 0;
	while(true)
	{
		bool repeat		= true;
		auto maxDelay	= system_clock::now() + std::chrono::milliseconds((int)(acsConfig.epoch_interval * 1000));
		auto breakTime	= maxDelay;
		while	( repeat
				&&system_clock::now() < breakTime)
		{
			for (auto& [id, s] : downloadStreamMap)
			{    
				ObsStream&	obsStream	= *s;
				obsStream.getObs(tsync);
			}
			std::this_thread::sleep_for(std::chrono::milliseconds(1));
		}

		BOOST_LOG_TRIVIAL(debug) << std::endl << "<<<<<<<<<<< Network Trace : Epoch " << epoch << " >>>>>>>>>>>" << std::endl;
		BOOST_LOG_TRIVIAL(debug) << "Date / Time : " << boost::posix_time::from_time_t(std::chrono::system_clock::to_time_t(breakTime)) << std::endl;

		for (auto once : {1})
		{
			string netStreamFilename = acsConfig.trace_directory + "NetworkStatistics.json";
			std::ofstream netStream(netStreamFilename, std::ofstream::out | std::ofstream::ate);
			if (!netStream)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Could not open trace file for network statistics at " << netStreamFilename;
				break;
			}
			int streamCnt = 0;
			netStream << "[";
			for (auto& [id, s] : downloadStreamMap )
			{
				NtripRtcmStream& downStream = *s;
				
				auto t = traceFiles.find(id);
				if ( t == traceFiles.end() )
					continue;
				std::ofstream trace(t->second,std::ofstream::out | std::ofstream::app);
				trace << std::endl << "<<<<<<<<<<< Network Trace : Epoch " << epoch << " >>>>>>>>>>>" << std::endl;
				trace << "Date / Time : " << boost::posix_time::from_time_t(std::chrono::system_clock::to_time_t(breakTime)) << std::endl;
				downStream.traceWriteEpoch(trace);
				trace.close();

				std::string jsonNetworkStatistics = downStream.getJsonNetworkStatistics(breakTime);
				netStream.write(jsonNetworkStatistics.c_str(),jsonNetworkStatistics.length());
				streamCnt++;
				if( downloadStreamMap.size() != streamCnt )
				netStream << ",";
				netStream << std::endl;
			}
			netStream << "]";
		}
		
		epoch++;
		tsync.time += tgap;
	}    
	
}

void NtripCasterService::makeTraceFiles()
{
	GTime traceTime = tsync;
	//round to nearest chunk by integer arithmetic
	long int roundTime = traceTime.time;
	roundTime /= acsConfig.trace_rotate_period;
	roundTime *= acsConfig.trace_rotate_period;
	traceTime.time = roundTime;    
	
	string logtime = traceTime.to_string(0);
	std::replace( logtime.begin(), logtime.end(), '/', '-');      
	
	
	for (auto& [id, s] : downloadStreamMap)
	{     
		string path_trace = acsConfig.trace_filename;
		replaceString(path_trace, "<STATION>", id);
		replaceString(path_trace, "<LOGTIME>", logtime);

		BOOST_LOG_TRIVIAL(debug)
		<< "\tCreating trace file for stream " << id;
		std::ofstream trace(path_trace,std::ofstream::out | std::ofstream::ate);
		
		
		if (!trace)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Could not create trace file for " << id << " at " << path_trace;
		}
		else
		{
			// Trace file head
			trace << "station    : " << id << std::endl;
			trace << "start_epoch: " << acsConfig.start_epoch << std::endl;
			trace << "end_epoch  : " << acsConfig.end_epoch   << std::endl;
			trace << "trace_level: " << acsConfig.trace_level << std::endl;
			trace << "pea_version: " << PEA_COMMIT_VERSION    << std::endl;
		} 
		trace.close();
		traceFiles.insert({id,path_trace});
	}
}
