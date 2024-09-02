
// #pragma GCC optimize ("O0")

#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/json.hpp>

using bsoncxx::builder::basic::kvp;
using bsoncxx::types::b_date;


#include <chrono>
#include <string>

using std::chrono::system_clock;
using std::chrono::time_point;
using std::string;

#include "ntripTrace.hpp"
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


	std::ofstream fout(networkTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << networkTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	connectCount++;

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("StreamName",				streamName									));
	doc.append(kvp("MessageType",				"Error"										));
	doc.append(kvp("BoostSysErrCode",			err.value()									));
	doc.append(kvp("Error",						err.message()								));
	doc.append(kvp("SocketOperation",			operation									));
	doc.append(kvp("Time",						b_date {std::chrono::system_clock::now()}	));

	fout << bsoncxx::to_json(doc) << "\n";
}

void NetworkStatistics::onConnectedStatistics()
{
	if (networkTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(networkTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << networkTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	connectCount++;

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("StreamName",				streamName									));
	doc.append(kvp("MessageType",				"Connected"									));
	doc.append(kvp("Time",						b_date {std::chrono::system_clock::now()}	));
	doc.append(kvp("ConnectCount",				connectCount								));
	doc.append(kvp("DisonnectCount",			disconnectCount								));

	fout << bsoncxx::to_json(doc) << "\n";
}

void NetworkStatistics::onDisconnectedStatistics()
{
	if (networkTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(networkTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << networkTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	disconnectCount++;

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("StreamName",				streamName									));
	doc.append(kvp("MessageType",				"Disconnected"								));
	doc.append(kvp("Time",						b_date {std::chrono::system_clock::now()}	));
	doc.append(kvp("ConnectCount",				connectCount								));
	doc.append(kvp("DisonnectCount",			disconnectCount								));

	fout << bsoncxx::to_json(doc) << "\n";
}

void NetworkStatistics::onChunkSentStatistics()
{
	if (networkTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(networkTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << networkTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	chunksSent++;

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("StreamName",				streamName									));
	doc.append(kvp("MessageType",				"ChunkSent"									));
	doc.append(kvp("Time",						b_date {std::chrono::system_clock::now()}	));
	doc.append(kvp("ChunksSent",				chunksSent									));

	fout << bsoncxx::to_json(doc) << "\n";
}

void NetworkStatistics::onChunkReceivedStatistics()
{
	if (networkTraceFilename.empty())
	{
		return;
	}

	if (traceLevel < 5)
		return;

	std::ofstream fout(networkTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << networkTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	chunksReceived++;

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("StreamName",				streamName									));
	doc.append(kvp("MessageType",				"ChunkReceived"								));
	doc.append(kvp("Time",						b_date {std::chrono::system_clock::now()}	));
	doc.append(kvp("ChunksReceived",			chunksReceived								));

	fout << bsoncxx::to_json(doc) << "\n";
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

	double totalTime = (timeGet() - startTime).to_double();

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
// 	traceStr << "Start           : " << std::put_time(std::localtime(&startTime.time),	"%F %X")	<< "\n";
// 	traceStr << "Finish          : " << std::put_time(std::localtime(&endTime.time),	"%F %X")	<< "\n";
//
// 	double totalTime = timeGet() - startTime;
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
// 	traceStr << "Disconnects     : " << disconnectionCount 	<< "\n";
// 	traceStr << "MeanDowntime    : " << meanReconn 			<< "\n";
// 	traceStr << "ConnectedRatio  : " << connRatio 			<< "\n";
//
// 	double chunkRatio = 0;
//
// 	if (numberChunks != 0)
// 		chunkRatio = (double)numberErroredChunks / (double)(numberChunks);
//
// 	traceStr << "Chunks          : " << numberChunks		<< "\n";
// 	traceStr << "ChunkErrors     : " << numberErroredChunks	<< "\n";
// 	traceStr << "ChunkErrorRatio : " << chunkRatio			<< "\n";
//
// 	bool printToTerminal = false;
// 	if	( chunkRatio			> 0.01
// 		||connRatio				< 0.99)
// 	{
// 		printToTerminal = true;
// 	}
//
// 	string messLine;
// 	while (std::getline(traceStr, messLine))
// 		tracepde(0, trace, (messLine + "\n").c_str());
// }


