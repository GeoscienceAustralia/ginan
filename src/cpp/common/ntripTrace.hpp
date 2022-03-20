#ifndef ACS_RTCM_TRACE
#define ACS_RTCM_TRACE

#include "navigation.hpp"
#include "satSys.hpp"
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
	
	string	networkTraceFilename = "";
	
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

struct RtcmStatistics
{
	long int	numPreambleFound	= 0;
	long int	numFramesFailedCRC	= 0;
	long int	numFramesPassCRC	= 0;
	long int	numFramesDecoded	= 0;
	long int	numNonMessBytes		= 0;
	long int	numMessagesLatency	= 0;
	double		totalLatency		= 0;
	
	void printRtcmStatistics(
		Trace& trace);
};

struct RtcmTrace 
{
	string	rtcmTraceFilename	= "";
	string	rtcmMountPoint;	
	
	RtcmTrace(
		string mountpoint	= "",
		string filename		= "") : rtcmTraceFilename{filename}, rtcmMountPoint{mountpoint}
	{
	}

	void networkLog(
		string message)
	{
		std::ofstream outStream(rtcmTraceFilename, std::ios::app);
		if (!outStream)
		{
			std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << std::endl;
			return;
		}
	
		outStream << boost::posix_time::from_time_t(std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()));
		outStream << " networkLog" << message << std::endl;
	}

	void messageChunkLog(
		string message)
	{
		std::ofstream outStream(rtcmTraceFilename, std::ios::app);
		if (!outStream)
		{
			std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << std::endl;
			return;
		}
	
		outStream << boost::posix_time::from_time_t(std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()));
		outStream << " messageChunkLog" << message << std::endl;
	}

	void messageRtcmLog(
		string message)
	{
		std::ofstream outStream(rtcmTraceFilename, std::ios::app);
		if (!outStream)
		{
			std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << std::endl;
			return;
		}
	
		outStream << boost::posix_time::from_time_t(std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()));
		outStream << " messageRtcmLog" << message << std::endl;
	}

	void messageRtcmByteLog(
		string message)
	{
		std::ofstream outStream(rtcmTraceFilename, std::ios::app);
		if (!outStream)
		{
			std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << std::endl;
			return;
		}
	
		outStream << boost::posix_time::from_time_t(std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()));
		outStream << " messageRtcmByteLog" << message << std::endl;
	}   

	void outputSsrEphToJson(
		const SSREph&	ssrEph,
		const SatSys&	Sat);
	
	void outputSsrClkToJson(
		const SSRClk&	ssrClk,
		const SatSys&	Sat);
	
	void traceSsrEph(
		SatSys		Sat,
		SSREph&		ssrEph);

	void traceSsrClk(
		SatSys		Sat,
		SSRClk&		ssrClk);

	void traceSsrCodeB(
		SatSys		Sat,
		E_ObsCode	mode,
		SSRBias&	ssrBias);

	void traceSsrPhasB(
		SatSys		Sat,
		E_ObsCode	mode,
		SSRBias&	ssrBias);

	void traceBroEph(
		Eph&		eph,
		E_Sys		sys);
};

#endif
