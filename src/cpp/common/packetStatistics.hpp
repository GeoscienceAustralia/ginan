
#pragma once

#include "trace.hpp"

struct PacketStatistics
{
	long int	numPreambleFound	= 0;
	long int	numFramesFailedCRC	= 0;
	long int	numFramesPassCRC	= 0;
	long int	numFramesDecoded	= 0;
	long int	numNonMessBytes		= 0;
	long int	numMessagesLatency	= 0;
	double		totalLatency		= 0;
	
	void printPacketStatistics(
		Trace& trace)
	{
		std::stringstream traceStr;
	// 	traceStr << "Start           : " << std::put_time(std::localtime(&startTime.time),	"%F %X")	<< std::endl;
	// 	traceStr << "Finish          : " << std::put_time(std::localtime(&endTime.time),	"%F %X")	<< std::endl;

//	 	double totalTime = timeGet() - startTime;

		traceStr << "ExtraBytes  : " << numNonMessBytes		<< std::endl;
		traceStr << "FailCrc     : " << numFramesFailedCRC 	<< std::endl;
		traceStr << "PassedCrc   : " << numFramesPassCRC	<< std::endl;
		traceStr << "Decoded     : " << numFramesDecoded	<< std::endl;
		traceStr << "Preamble    : " << numPreambleFound	<< std::endl;

		double FailedToPreambleRatio = 0;
		if (numPreambleFound != 0)
			FailedToPreambleRatio = (double)numFramesFailedCRC / (double)numPreambleFound;
		traceStr << "FailedCrcToPreambleRatio : " << FailedToPreambleRatio << std::endl;

		if (numMessagesLatency != 0)
		{
			double meanLatency = totalLatency / numMessagesLatency;	
			traceStr << "meanLatency     : " << meanLatency	<< std::endl;
		}
		else
		{
			traceStr << "meanLatency     : " << 0.0			<< std::endl;
		}

		string messLine;
		while (std::getline(traceStr, messLine))
			tracepdeex(0, trace, (messLine + "\n").c_str());
	}
	
	void checksumFailure(
		string id = "")
	{
		numFramesFailedCRC++;
	
		std::stringstream message;
		message << "\n CRC Failure - "			<< id;
		message << "\n Number Fail CRC   : "	<< numFramesFailedCRC;
		message << "\n Number Passed CRC : "	<< numFramesPassCRC;
		message << "\n Number Decoded    : "	<< numFramesDecoded;
		message << "\n Number Preamble   : "	<< numPreambleFound;
		message << "\n Number Unknown    : "	<< numNonMessBytes;
		message << "\n";
		
		std::cout << message.str();
// 		messageRtcmLog(message.str());	//todo aaron
	}
	
	
// 	bool trigger = false;
	void checksumSuccess(
		unsigned int crcRead = 0)
	{
// 		std::cout << "Pass\n";
		numFramesPassCRC++;
		
// 		printf("\n CRC pass: %02x %02x %02x", 
// 			((char*)&crcRead)[2], 
// 			((char*)&crcRead)[1], 
// 			((char*)&crcRead)[0]);
	}
	
	void nonFrameByteFound(
		unsigned char c)
	{
// 		printf(".%02x", c);
		
		numNonMessBytes++;
// 		trigger = true;
	}
	
	void preambleFound()
	{
// 		if (trigger)
// 		{
// 			std::cout << std::endl << "premable";
// 			trigger = false;
// 		}
		numPreambleFound++;	
	
		if (numNonMessBytes)
		{
			std::stringstream message;
			message << "Extra Bytes, size : " << numNonMessBytes;
// 			messageRtcmLog(message.str());//todo aaron
		}
		
		numNonMessBytes = 0;
	}
	
	void frameDecoded()
	{
		numFramesDecoded++;
	}
};
