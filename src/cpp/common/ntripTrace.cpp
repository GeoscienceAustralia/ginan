
#ifdef ENABLE_MONGODB
#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/json.hpp>

using bsoncxx::builder::basic::kvp;

#endif

#include <string>

using std::string;

#include "acsStream.hpp"
#include "acsConfig.hpp"
#include "common.hpp"

void NetworkStatistics::onErrorStatistics(
	const boost::system::error_code& 	err,
	string 								operation)
{
	if (networkTraceFilename.empty())
	{
		return;
	}
	
#	ifdef ENABLE_MONGODB

	std::ofstream fout(networkTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << networkTraceFilename << " in " << __FUNCTION__ << std::endl;
		return;
	}
	
	connectCount++;
	
	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("StreamName",				streamName													));
	doc.append(kvp("MessageType",				"Error"														));
	doc.append(kvp("BoostSysErrCode",			err.value()													));
	doc.append(kvp("Error",						err.message()												));
	doc.append(kvp("SocketOperation",			operation													));
	doc.append(kvp("Time",						bsoncxx::types::b_date {std::chrono::system_clock::now()}	));
	
	fout << bsoncxx::to_json(doc) << std::endl;
	
#	else
	BOOST_LOG_TRIVIAL(warning) << "Warning: exporting to JSON requires compilation with MongoDB.";
#	endif
}

void NetworkStatistics::onConnectedStatistics()
{
	if (networkTraceFilename.empty())
	{
		return;
	}
	
#	ifdef ENABLE_MONGODB

	std::ofstream fout(networkTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << networkTraceFilename << " in " << __FUNCTION__ << std::endl;
		return;
	}
	
	connectCount++;
	
	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("StreamName",				streamName													));
	doc.append(kvp("MessageType",				"Connected"													));
	doc.append(kvp("Time",						bsoncxx::types::b_date {std::chrono::system_clock::now()}	));
	doc.append(kvp("ConnectCount",				connectCount												));
	doc.append(kvp("DisonnectCount",			disconnectCount												));
	
	fout << bsoncxx::to_json(doc) << std::endl;
	
#	else
	BOOST_LOG_TRIVIAL(warning) << "Warning: exporting to JSON requires compilation with MongoDB.";
#	endif
}

void NetworkStatistics::onDisconnectedStatistics()
{
	if (networkTraceFilename.empty())
	{
		return;
	}
	
#	ifdef ENABLE_MONGODB

	std::ofstream fout(networkTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << networkTraceFilename << " in " << __FUNCTION__ << std::endl;
		return;
	}
	
	disconnectCount++;
	
	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("StreamName",				streamName													));
	doc.append(kvp("MessageType",				"Disconnected"												));
	doc.append(kvp("Time",						bsoncxx::types::b_date {std::chrono::system_clock::now()}	));
	doc.append(kvp("ConnectCount",				connectCount												));
	doc.append(kvp("DisonnectCount",			disconnectCount												));
	
	fout << bsoncxx::to_json(doc) << std::endl;
	
#	else
	BOOST_LOG_TRIVIAL(warning) << "Warning: exporting to JSON requires compilation with MongoDB.";
#	endif
}

void NetworkStatistics::onChunkSentStatistics()
{
	if (networkTraceFilename.empty())
	{
		return;
	}
	
#	ifdef ENABLE_MONGODB

	std::ofstream fout(networkTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << networkTraceFilename << " in " << __FUNCTION__ << std::endl;
		return;
	}
	
	chunksSent++;
	
	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("StreamName",				streamName													));
	doc.append(kvp("MessageType",				"ChunkSent"													));
	doc.append(kvp("Time",						bsoncxx::types::b_date {std::chrono::system_clock::now()}	));
	doc.append(kvp("ChunksSent",				chunksSent													));
	
	fout << bsoncxx::to_json(doc) << std::endl;
	
#	else
	BOOST_LOG_TRIVIAL(warning) << "Warning: exporting to JSON requires compilation with MongoDB.";
#	endif
}

void NetworkStatistics::onChunkReceivedStatistics()
{
	if (networkTraceFilename.empty())
	{
		return;
	}
	
#	ifdef ENABLE_MONGODB
	if (trace_level < 5)
		return;
	
	std::ofstream fout(networkTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << networkTraceFilename << " in " << __FUNCTION__ << std::endl;
		return;
	}
	
	chunksReceived++;
	
	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("StreamName",				streamName													));
	doc.append(kvp("MessageType",				"ChunkReceived"												));
	doc.append(kvp("Time",						bsoncxx::types::b_date {std::chrono::system_clock::now()}	));
	doc.append(kvp("ChunksReceived",				chunksReceived													));
	
	fout << bsoncxx::to_json(doc) << std::endl;
	
#	else
	BOOST_LOG_TRIVIAL(warning) << "Warning: exporting to JSON requires compilation with MongoDB.";
#	endif
}

void RtcmTrace::outputSsrEphToJson(
	const SSREph&	ssrEph,
	const SatSys&	Sat)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}
	
#	ifdef ENABLE_MONGODB

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << std::endl;
		return;
	}
	
	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("MountPoint",				rtcmMountPoint						));
	doc.append(kvp("MessageType",				"Orbit Correction"					));
	doc.append(kvp("EpochTime1s",				ssrEph.ssrMeta.epochTime1s			));
	doc.append(kvp("SSRUpdateIntervalInd",		ssrEph.ssrMeta.ssrUpdateIntIndex	));
	doc.append(kvp("SSRUpdateIntervalSec",		ssrEph.udi							));
	doc.append(kvp("MultipleMessageIndicator",	ssrEph.ssrMeta.multipleMessage		));
	doc.append(kvp("SatelliteReferenceDatum",	(int) ssrEph.ssrMeta.referenceDatum	));	// 0 = ITRF, 1 = Regional // bsoncxx doesn't like uints
	doc.append(kvp("IODSSR",					ssrEph.iod							));
	doc.append(kvp("SSRProviderID",				(int) ssrEph.ssrMeta.provider		));
	doc.append(kvp("SSRSolutionID",				(int) ssrEph.ssrMeta.solution		));
	doc.append(kvp("SatelliteID",				Sat.id()							));
	doc.append(kvp("IODE",						ssrEph.iode							));
	doc.append(kvp("DeltaRadial",				ssrEph.deph[0]						));
	doc.append(kvp("DeltaAlongTrack",			ssrEph.deph[1]						));
	doc.append(kvp("DeltaCrossTrack",			ssrEph.deph[2]						));
	doc.append(kvp("DotDeltaRadial",			ssrEph.ddeph[0]						));
	doc.append(kvp("DotDeltaAlongTrack",		ssrEph.ddeph[1]						));
	doc.append(kvp("DotDeltaCrossTrack",		ssrEph.ddeph[2]						));
	
	fout << bsoncxx::to_json(doc) << std::endl;
	
#	else
	BOOST_LOG_TRIVIAL(warning) << "Warning: exporting to JSON requires compilation with MongoDB.";
#	endif
}


void RtcmTrace::outputSsrClkToJson(
	const SSRClk&	ssrClk,
	const SatSys&	Sat)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}
	
#	ifdef ENABLE_MONGODB

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << std::endl;
		return;
	}
	
	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("MountPoint",				rtcmMountPoint						));
	doc.append(kvp("MessageType",				"Clock Correction"					));
	doc.append(kvp("EpochTime1s",				ssrClk.ssrMeta.epochTime1s			));
	doc.append(kvp("SSRUpdateIntervalInd",		ssrClk.ssrMeta.ssrUpdateIntIndex	));
	doc.append(kvp("SSRUpdateIntervalSec",		ssrClk.udi							));
	doc.append(kvp("MultipleMessageIndicator",	ssrClk.ssrMeta.multipleMessage		));
	doc.append(kvp("SatelliteReferenceDatum",	(int) ssrClk.ssrMeta.referenceDatum	));	// 0 = ITRF, 1 = Regional
	doc.append(kvp("IODSSR",					ssrClk.iod							));
	doc.append(kvp("SSRProviderID",				(int) ssrClk.ssrMeta.provider		));
	doc.append(kvp("SSRSolutionID",				(int) ssrClk.ssrMeta.solution		));
	doc.append(kvp("SatelliteID",				Sat.id()							));
	doc.append(kvp("DeltaClockC0",				ssrClk.dclk[0]						));
	doc.append(kvp("DeltaClockC1",				ssrClk.dclk[1]						));
	doc.append(kvp("DeltaClockC2",				ssrClk.dclk[2]						));
	
	fout << bsoncxx::to_json(doc) << std::endl;
	
#	else
	BOOST_LOG_TRIVIAL(warning) << "Warning: exporting to JSON requires compilation with MongoDB.";
#	endif
}


void RtcmTrace::traceSsrCodeB(
	SatSys		Sat,
	E_ObsCode	code,
	SSRBias&	ssrBias)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}
	
#	ifdef ENABLE_MONGODB

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << std::endl;
		return;
	}
	
	double ep[6];
	int yds[3];

	time2epoch(ssrBias.t0, ep);
	epoch2yds(ep, yds);
	
	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("MountPoint",				rtcmMountPoint						));
	doc.append(kvp("MessageType",				"CodeBias"							));
	doc.append(kvp("Sat",						Sat.id()							));
	doc.append(kvp("Code",						code._to_string()					));
	doc.append(kvp("Year",						yds[0]								));
	doc.append(kvp("Day",						yds[1]								));
	doc.append(kvp("Second",					yds[2]								));
	doc.append(kvp("Bias",						ssrBias.obsCodeBiasMap[code].bias	));
	
	fout << bsoncxx::to_json(doc) << std::endl;
	
#	else
	BOOST_LOG_TRIVIAL(warning) << "Warning: exporting to JSON requires compilation with MongoDB.";
#	endif
}

void RtcmTrace::traceSsrPhasB(
	SatSys		Sat,
	E_ObsCode	code,
	SSRBias&	ssrBias)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}
	
#	ifdef ENABLE_MONGODB

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << std::endl;
		return;
	}
	
	double ep[6];
	int yds[3];

	time2epoch(ssrBias.t0, ep);
	epoch2yds(ep, yds);
	
	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("MountPoint",				rtcmMountPoint						));
	doc.append(kvp("MessageType",				"PhaseBias"							));
	doc.append(kvp("Sat",						Sat.id()							));
	doc.append(kvp("Code",						code._to_string()					));
	doc.append(kvp("Year",						yds[0]								));
	doc.append(kvp("Day",						yds[1]								));
	doc.append(kvp("Second",					yds[2]								));
	doc.append(kvp("Bias",						ssrBias.obsCodeBiasMap[code].bias	));
	
	fout << bsoncxx::to_json(doc) << std::endl;
	
#	else
	BOOST_LOG_TRIVIAL(warning) << "Warning: exporting to JSON requires compilation with MongoDB.";
#	endif
}

void RtcmTrace::traceBroEph(
	Eph&		eph,
	E_Sys		sys)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}
	
#	ifdef ENABLE_MONGODB

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << std::endl;
		return;
	}
	
	double ep[6];
	time2epoch(eph.toc, ep);

	bsoncxx::builder::basic::document doc = {};
	//Note the Satellite id is not set in rinex correctly as we a mixing GNSS systems.
	doc.append(kvp("Sat",		eph.Sat.id()	));
	doc.append(kvp("ep[0]",		(int)ep[0]		));
	doc.append(kvp("ep[0]",		(int)ep[0]		));
	doc.append(kvp("ep[1]",		(int)ep[1]		));
	doc.append(kvp("ep[2]",		(int)ep[2]		));
	doc.append(kvp("ep[3]",		(int)ep[3]		));
	doc.append(kvp("ep[4]",		(int)ep[4]		));
	doc.append(kvp("ep[5]",		(int)ep[5]		));
	doc.append(kvp("f0",		eph.f0			));
	doc.append(kvp("f1",		eph.f1			));
	doc.append(kvp("f2",		eph.f2			));

	doc.append(kvp("",			eph.iode		));
	doc.append(kvp("",			eph.crs			));
	doc.append(kvp("",			eph.deln		));
	doc.append(kvp("",			eph.M0			));
	doc.append(kvp("cuc",		eph.cuc			));
	doc.append(kvp("e",			eph.e			));
	doc.append(kvp("cus",		eph.cus			));
	doc.append(kvp("A",			SQRT(eph.A)		));	
	doc.append(kvp("toes",		eph.toes		));
	doc.append(kvp("cic",		eph.cic			));
	doc.append(kvp("OMG0",		eph.OMG0		));
	doc.append(kvp("cis",		eph.cis			));
	doc.append(kvp("i0",		eph.i0			));
	doc.append(kvp("crc",		eph.crc			));
	doc.append(kvp("omg",		eph.omg			));
	doc.append(kvp("OMGd",		eph.OMGd		));
	doc.append(kvp("idot",		eph.idot		));
	doc.append(kvp("code",		eph.code		));
	doc.append(kvp("week",		eph.week		));
	doc.append(kvp("flag",		eph.flag		));
	
	if (sys == +E_Sys::GPS )
	{
		// https://cddis.nasa.gov/archive/gnss/data/daily/2021/102/21n/
		/*
			GLOBAL POSITIONING SYSTEM
			STANDARD POSITIONING SERVICE
			SIGNAL SPECIFICATION
			2nd Ed, June 2,1995
			see section - 2.5.3 User Range Accuracy
		*/
		double ura = 0;
		if		(eph.sva <= 6)
		{
			ura = 10*pow(2, 1+((double)eph.sva/2.0));
			ura = round(ura)/10.0;
		}
		else if	( eph.sva != 15 )
			ura = pow(2,(double)eph.sva-2.0);
		else
			ura = -1;

		doc.append(kvp("ura",		ura							));
		doc.append(kvp("svh",		eph.svh						));
		doc.append(kvp("tgd[0]",	eph.tgd[0]					));
		doc.append(kvp("iodc",		eph.iodc					));
		doc.append(kvp("tow",		time2gpst(eph.ttr,&eph.week)));
		doc.append(kvp("fit",		eph.fit						));
	}
	else if (sys == +E_Sys::GAL)
	{
		// https://cddis.nasa.gov/archive/gnss/data/daily/2021/102/21l/

		/*
			EUROPEAN GNSS (GALILEO) OPEN SERVICE
			SIGNAL-IN-SPACE
			INTERFACE CONTROL
			DOCUMENT
			Issue 2.0, January 2021
			See Section, 5.1.12. Signal – In – Space Accuracy (SISA)
		*/
		double SISA;
		if		( eph.sva <= 49 )			SISA = 0.0+(eph.sva-0)	* 0.01;
		else if ( eph.sva <= 74 )			SISA = 0.5+(eph.sva-50)	* 0.02;
		else if ( eph.sva <= 99 )			SISA = 1.0+(eph.sva-75)	* 0.04;
		else if ( eph.sva <= 125 )			SISA = 2.0+(eph.sva-100)* 0.16;
		else								SISA = -1;

		doc.append(kvp("SISA",		SISA						));
		doc.append(kvp("svh",		eph.svh						));
		doc.append(kvp("tgd[0]",	eph.tgd[0]					));
		doc.append(kvp("tgd[1]",	eph.tgd[1]					));
		doc.append(kvp("tow",		time2gpst(eph.ttr,&eph.week)));
	}
	
	fout << bsoncxx::to_json(doc) << std::endl;
	
#	else
	BOOST_LOG_TRIVIAL(warning) << "Warning: exporting to JSON requires compilation with MongoDB.";
#	endif
}


string NetworkStatistics::getNetworkStatistics(
	GTime	now,
	string	label)
{
	bsoncxx::builder::basic::document doc = {};

	doc.append(kvp("label", 			label));
	doc.append(kvp("Stream", 			streamName));
// 	doc.append(kvp("Epoch", 			std::put_time(std::localtime(&now.time),		"%F %X")));
// 	doc.append(kvp("Start", 			std::put_time(std::localtime(&startTime.time),	"%F %X")));
// 	doc.append(kvp("Finish", 			std::put_time(std::localtime(&endTime.time),	"%F %X")));
	doc.append(kvp("Network", 			acsConfig.analysis_agency));
	doc.append(kvp("Downloading", 		true));

	auto timeNow = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
	boost::posix_time::time_duration totalTime = timeNow - boost::posix_time::from_time_t(startTime.time);

// 	double connRatio;
// 	if (disconnectionCount == 0
// 		&& numberChunks > 0)
// 	{
// 		connRatio = 1;
// 	}
// 	else
// 	{
// 		if (totalTime.total_milliseconds() == 0)
// 			connRatio = 0;
// 		else
// 			connRatio = (double)connectedDuration.total_milliseconds() / (double)totalTime.total_milliseconds();
// 	}
// 
// 
// 	double meanReconn = 0;
// 	if (disconnectionCount != 0)
// 		meanReconn = (double)disconnectedDuration.total_milliseconds() / (60.0 * 1000.0 * disconnectionCount);
// 
// 	doc.append(kvp("Disconnects", 		disconnectionCount));
// 	doc.append(kvp("MeanDowntime", 		meanReconn));
// 	doc.append(kvp("ConnectedRatio", 	connRatio));

// 	double chunkRatio = 0;
// 
// 	if (numberChunks != 0)
// 		chunkRatio = (double)numberErroredChunks / (double)(numberChunks);
// 
// 	doc.append(kvp("Chunks", 			numberChunks));
// 	doc.append(kvp("ChunkErrors", 		numberErroredChunks));
// 	doc.append(kvp("ChunkErrorRatio",	chunkRatio));
	
	return bsoncxx::to_json(doc);
}

// void NetworkStatistics::printNetworkStatistics(
// 	Trace& trace)
// {
// 	std::stringstream traceStr;
// 	traceStr << "Start           : " << std::put_time(std::localtime(&startTime.time),	"%F %X")	<< std::endl;
// 	traceStr << "Finish          : " << std::put_time(std::localtime(&endTime.time),	"%F %X")	<< std::endl;
// 
// 	auto timeNow = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
// 	boost::posix_time::time_duration totalTime = timeNow - boost::posix_time::from_time_t(startTime.time);
// 
// 
// 	double connRatio;
// 	if (disconnectionCount == 0
// 		&& numberChunks > 0)
// 	{
// 		connRatio = 1;
// 	}
// 	else
// 	{
// 		if (totalTime.total_milliseconds() == 0)
// 			connRatio = 0;
// 		else
// 			connRatio = (double) connectedDuration.total_milliseconds() / (double)totalTime.total_milliseconds();
// 	}
// 
// 	double meanReconn = 0;
// 	if (disconnectionCount != 0)
// 		meanReconn = (double)disconnectedDuration.total_milliseconds() / (60.0 * 1000.0 * disconnectionCount);
// 
// 	traceStr << "Disconnects     : " << disconnectionCount 	<< std::endl;
// 	traceStr << "MeanDowntime    : " << meanReconn 			<< std::endl;
// 	traceStr << "ConnectedRatio  : " << connRatio 			<< std::endl;
// 
// 	double chunkRatio = 0;
// 
// 	if (numberChunks != 0)
// 		chunkRatio = (double)numberErroredChunks / (double)(numberChunks);
// 
// 	traceStr << "Chunks          : " << numberChunks		<< std::endl;
// 	traceStr << "ChunkErrors     : " << numberErroredChunks	<< std::endl;
// 	traceStr << "ChunkErrorRatio : " << chunkRatio			<< std::endl;
// 
// 	bool printToTerminal = false;
// 	if	( chunkRatio			> 0.01
// 		||connRatio				< 0.99)
// 	{
// 		printToTerminal = true;
// 	}
// 
// 	if	(  printToTerminal
// 		&& acsConfig.print_stream_statistics)
// 	{
// 		BOOST_LOG_TRIVIAL(debug) << traceStr.str();
// 	}
// 
// 	string messLine;
// 	while (std::getline(traceStr, messLine))
// 		tracepde(0, trace, (messLine + "\n").c_str());
// }

void RtcmStatistics::printRtcmStatistics(
	Trace& trace)
{
	std::stringstream traceStr;
// 	traceStr << "Start           : " << std::put_time(std::localtime(&startTime.time),	"%F %X")	<< std::endl;
// 	traceStr << "Finish          : " << std::put_time(std::localtime(&endTime.time),	"%F %X")	<< std::endl;

// 	auto timeNow = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
// 	boost::posix_time::time_duration totalTime = timeNow - boost::posix_time::from_time_t(startTime.time);


	traceStr << "RtcmExtraBytes  : " << numNonMessBytes		<< std::endl;
	traceStr << "RtcmFailCrc     : " << numFramesFailedCRC 	<< std::endl;
	traceStr << "RtcmPassedCrc   : " << numFramesPassCRC	<< std::endl;
	traceStr << "RtcmDecoded     : " << numFramesDecoded	<< std::endl;
	traceStr << "RtcmPreamble    : " << numPreambleFound	<< std::endl;

	double FailedToPreambleRatio = 0;
	if (numPreambleFound != 0)
		FailedToPreambleRatio = (double)numFramesFailedCRC / (double)numPreambleFound;
	traceStr << "RtcmFailedCrcToPreambleRatio : " << FailedToPreambleRatio << std::endl;

	if (numMessagesLatency != 0)
	{
		double meanLatency = totalLatency / numMessagesLatency;	
		traceStr << "meanLatency     : " << meanLatency	<< std::endl;
	}
	else
	{
		traceStr << "meanLatency     : " << 0.0			<< std::endl;
	}

	bool printToTerminal = false;
	if	(FailedToPreambleRatio	> 0.01)
	{
		printToTerminal = true;
	}

	if	(  printToTerminal
		&& acsConfig.print_stream_statistics)
	{
		BOOST_LOG_TRIVIAL(debug) << traceStr.str();
	}

	string messLine;
	while (std::getline(traceStr, messLine))
		tracepde(0, trace, (messLine + "\n").c_str());
}
