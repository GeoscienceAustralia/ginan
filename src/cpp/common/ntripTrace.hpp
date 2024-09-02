
#pragma once

#include "satSys.hpp"
#include "gTime.hpp"
#include "enums.h"

#include <iostream>
#include <string>
#include <vector>
#include <boost/date_time/posix_time/posix_time.hpp>

using std::string;
using std::vector;

#include <boost/asio/buffer.hpp>
#include <boost/asio.hpp>

struct NetworkStatistics
{
	string	streamName;
	GTime	startTime;
	GTime	endTime;

	string	networkTraceFilename;

	int		connectCount			= 0;
	int		disconnectCount 		= 0;
	int 	chunksSent				= 0;
	int		chunksReceived			= 0;

	boost::posix_time::time_duration connectedDuration		= boost::posix_time::hours(0);
	boost::posix_time::time_duration disconnectedDuration	= boost::posix_time::hours(0);

	string	getNetworkStatistics(
		GTime	now,
		string	label);

	void onConnectedStatistics();

	void onDisconnectedStatistics();

	void onChunkSentStatistics();

	void onChunkReceivedStatistics();

	void onErrorStatistics(
		const boost::system::error_code& 	err,
		string 								operation);
};

