#pragma once

#include <boost/asio.hpp>
#include <boost/asio/buffer.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <iostream>
#include <string>
#include <vector>
#include "common/enums.h"
#include "common/gTime.hpp"
#include "common/satSys.hpp"

using std::string;
using std::vector;

struct NetworkStatistics
{
    string streamName;
    GTime  startTime;
    GTime  endTime;

    string networkTraceFilename;

    int connectCount    = 0;
    int disconnectCount = 0;
    int chunksSent      = 0;
    int chunksReceived  = 0;

    boost::posix_time::time_duration connectedDuration    = boost::posix_time::hours(0);
    boost::posix_time::time_duration disconnectedDuration = boost::posix_time::hours(0);

    string getNetworkStatistics(GTime now, string label);

    void onConnectedStatistics();

    void onDisconnectedStatistics();

    void onChunkSentStatistics();

    void onChunkReceivedStatistics();

    void onErrorStatistics(const boost::system::error_code& err, string operation);
};
