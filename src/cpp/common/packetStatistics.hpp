
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
	// 	traceStr << "Start           : " << std::put_time(std::localtime(&startTime.time),	"%F %X")	<< "\n";
	// 	traceStr << "Finish          : " << std::put_time(std::localtime(&endTime.time),	"%F %X")	<< "\n";

//	 	double totalTime = timeGet() - startTime;

		traceStr << "ExtraBytes  : " << numNonMessBytes		<< "\n";
		traceStr << "FailCrc     : " << numFramesFailedCRC 	<< "\n";
		traceStr << "PassedCrc   : " << numFramesPassCRC	<< "\n";
		traceStr << "Decoded     : " << numFramesDecoded	<< "\n";
		traceStr << "Preamble    : " << numPreambleFound	<< "\n";

		double FailedToPreambleRatio = 0;
		if (numPreambleFound != 0)
			FailedToPreambleRatio = (double)numFramesFailedCRC / (double)numPreambleFound;
		traceStr << "FailedCrcToPreambleRatio : " << FailedToPreambleRatio << "\n";

		if (numMessagesLatency != 0)
		{
			double meanLatency = totalLatency / numMessagesLatency;
			traceStr << "meanLatency     : " << meanLatency	<< "\n";
		}
		else
		{
			traceStr << "meanLatency     : " << 0.0			<< "\n";
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
// 		messageRtcmLog(message.str());
	}


	void checksumSuccess(
		unsigned int crcRead = 0)
	{
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
	}

	void preambleFound()
	{
		numPreambleFound++;

		if (numNonMessBytes)
		{
			std::stringstream message;
			message << "Extra Bytes, size : " << numNonMessBytes;
// 			messageRtcmLog(message.str());
		}

		numNonMessBytes = 0;
	}

	void frameDecoded()
	{
		numFramesDecoded++;
	}
};
