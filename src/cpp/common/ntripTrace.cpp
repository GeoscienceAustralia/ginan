// #pragma GCC optimize ("O0")

#include "common/ntripTrace.hpp"
#include <boost/json.hpp>
#include <chrono>
#include <string>
#include "common/acsConfig.hpp"
#include "common/common.hpp"

using std::string;
using std::chrono::system_clock;
using std::chrono::time_point;

void NetworkStatistics::onErrorStatistics(const boost::system::error_code& err, string operation)
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

    boost::json::object doc = {};
    doc["StreamName"]       = streamName;
    doc["MessageType"]      = "Error";
    doc["BoostSysErrCode"]  = err.value();
    doc["Error"]            = err.message();
    doc["SocketOperation"]  = operation;
    doc["Time"]             = timeGet().to_string();

    fout << boost::json::serialize(doc) << "\n";
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

    boost::json::object doc = {};
    doc["StreamName"]       = streamName;
    doc["MessageType"]      = "Connected";
    doc["Time"]             = timeGet().to_string();
    doc["ConnectCount"]     = connectCount;
    doc["DisonnectCount"]   = disconnectCount;

    fout << boost::json::serialize(doc) << "\n";
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

    boost::json::object doc = {};
    doc["StreamName"]       = streamName;
    doc["MessageType"]      = "Disconnected";
    doc["Time"]             = timeGet().to_string();
    doc["ConnectCount"]     = connectCount;
    doc["DisonnectCount"]   = disconnectCount;

    fout << boost::json::serialize(doc) << "\n";
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

    boost::json::object doc = {};
    doc["StreamName"]       = streamName;
    doc["MessageType"]      = "ChunkSent";
    doc["Time"]             = timeGet().to_string();
    doc["ChunksSent"]       = chunksSent;

    fout << boost::json::serialize(doc) << "\n";
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

    boost::json::object doc = {};
    doc["StreamName"]       = streamName;
    doc["MessageType"]      = "ChunkReceived";
    doc["Time"]             = timeGet().to_string();
    doc["ChunksReceived"]   = chunksReceived;

    fout << boost::json::serialize(doc) << "\n";
}

string NetworkStatistics::getNetworkStatistics(GTime now, string label)
{
    boost::json::object doc = {};

    doc["label"]  = label;
    doc["Stream"] = streamName;
    // 	doc.append(kvp("Epoch", 			std::put_time(std::localtime(&now.time),		"%F
    // %X"))); 	doc.append(kvp("Start", 			std::put_time(std::localtime(&startTime.time),
    // "%F %X"))); 	doc.append(kvp("Finish", 			std::put_time(std::localtime(&endTime.time),
    // "%F %X")));
    doc["Network"]     = acsConfig.analysis_agency;
    doc["Downloading"] = true;

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
    // 			connRatio = (double)connectedDuration.total_milliseconds() /
    // (double)totalTime.total_milliseconds();
    // 	}
    //
    //
    // 	double meanReconn = 0;
    // 	if (disconnectionCount != 0)
    // 		meanReconn = (double)disconnectedDuration.total_milliseconds() / (60.0 * 1000.0 *
    // disconnectionCount);
    //
    // 	doc["Disconnects"] = 		disconnectionCount;
    // 	doc["MeanDowntime"] = 		meanReconn;
    // 	doc["ConnectedRatio"] = 	connRatio;

    // 	double chunkRatio = 0;
    //
    // 	if (numberChunks != 0)
    // 		chunkRatio = (double)numberErroredChunks / (double)(numberChunks);
    //
    // 	doc["Chunks"] = 			numberChunks;
    // 	doc["ChunkErrors"] = 		numberErroredChunks;
    // 	doc.append(kvp("ChunkErrorRatio",	chunkRatio));

    return boost::json::serialize(doc);
}

// void NetworkStatistics::printNetworkStatistics(
// 	Trace& trace)
// {
// 	std::stringstream traceStr;
// 	traceStr << "Start           : " << std::put_time(std::localtime(&startTime.time),	"%F %X")
// << "\n"; 	traceStr << "Finish          : " << std::put_time(std::localtime(&endTime.time),
// "%F %X")	<< "\n";
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
// 			connRatio = (double) connectedDuration.total_milliseconds() /
// (double)totalTime.total_milliseconds();
// 	}
//
// 	double meanReconn = 0;
// 	if (disconnectionCount != 0)
// 		meanReconn = (double)disconnectedDuration.total_milliseconds() / (60.0 * 1000.0 *
// disconnectionCount);
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
