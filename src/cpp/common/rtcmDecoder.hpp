
#pragma once

#include "packetStatistics.hpp"
#include "observations.hpp"
#include "rtcmEncoder.hpp"
#include "rtcmTrace.hpp"
#include "streamObs.hpp"
#include "acsConfig.hpp"
#include "gTime.hpp"
#include "enums.h"

struct SignalInfo
{
	uint8_t		signal_id;
	E_FType		ftype;
	E_ObsCode	obsCode;
};

struct RtcmDecoder : RtcmTrace, ObsLister, PacketStatistics
{
	static double			rtcmDeltaTime;		///< Common time used among all rtcmDecoders for delaying decoding when realtime is enabled
	static map<GTime, int>	receivedTimeMap;

	GTime			lastTimeStamp;

	GTime			receivedTime;		///< Recent internal time from decoded rtcm messages

	ObsList			superObsList;		///< List to accumulate observations from smaller lists which share a common time

	static uint16_t message_length(
		char header[2]);

	static RtcmMessageType message_type(
		const uint8_t message[]);

	int adjGpsWeek(
		int week);

	int adjGstWeek(
		int week);

	int adjBdtWeek(
		int week);

	void traceLatency(GTime gpsTime);

	constexpr static int updateInterval[16] =
	{
		1, 2, 5, 10, 15, 30, 60, 120, 240, 300, 600, 900, 1800, 3600, 7200, 10800
	};

	GTime		rtcmTimestampTime;
	GWeek		rtcmWeek = -1;

	E_ObsCode signal_to_code(
		E_Sys	sys,
		uint8_t	signal);

	GTime rtcmTime();

	void decodeEphemeris(
		vector<unsigned char>&	message);

	void decodeSSR(
		vector<unsigned char>&	message);

	GTime decodeCustomTimestamp(
		vector<unsigned char>&	message);

	E_RTCMSubmessage decodeCustomId(
		vector<unsigned char>&	message);

	ObsList decodeMSM(
		vector<unsigned char>&	message);

	string recordFilename;

	void recordFrame(
		vector<unsigned char>&	data,
		unsigned int			crcRead)
	{
		if (recordFilename.empty())
		{
			return;
		}

		std::ofstream ofs(recordFilename, std::ofstream::app);

		if (!ofs)
		{
			return;
		}

		//Write the custom time stamp message.
		RtcmEncoder encoder;
		encoder.rtcmTraceFilename = rtcmTraceFilename;

		auto buffer	= encoder.encodeTimeStampRTCM();
		bool write	= encoder.encodeWriteMessageToBuffer(buffer);

		if (write)
		{
			encoder.encodeWriteMessages(ofs);
		}

		//copy the message to the output file too
		ofs.write((char *)data.data(),		data.size());
		ofs.write((char *)&crcRead,	3);
	}



	E_ReturnType decodeCustom(
		vector<unsigned char>& message)
	{
		E_RTCMSubmessage submessage = decodeCustomId(message);

		switch (submessage)
		{
			case (E_RTCMSubmessage::TIMESTAMP):
			{
				GTime timeStamp = decodeCustomTimestamp(message);

				rtcmTimestampTime = timeStamp;

				if (acsConfig.simulate_real_time)
				{
					//get the current time and compare it with the timestamp in the message
					GTime now = timeGet();

					//find the delay between creation of the timestamp, and now
					double thisDeltaTime = (now - timeStamp).to_double();

					//initialise the global rtcm delay if needed
					if (rtcmDeltaTime == 0)
					{
						rtcmDeltaTime = thisDeltaTime;
					}

					//if the delay is shorter than the global, go back and wait until it is longer
					if (thisDeltaTime < rtcmDeltaTime)
					{
						return E_ReturnType::WAIT;
					}

					if (acsConfig.output_decoded_rtcm_json)
						traceTimestamp(timeStamp);

					break;
				}

				if (1)
				{
					int& waitingStreams = receivedTimeMap[timeStamp];

					if (lastTimeStamp == GTime::noTime())
					{
						lastTimeStamp = timeStamp;
						waitingStreams++;

						return E_ReturnType::WAIT;
					}

					if (timeStamp != lastTimeStamp)
					{
						lastTimeStamp = timeStamp;
						waitingStreams++;
					}


					auto& [firstTime, count] = *receivedTimeMap.begin();

					if (timeStamp > firstTime)
					{
						return E_ReturnType::WAIT;
					}

					if (timeStamp < firstTime)
					{
						std::cout << "unexpected time here" << "\n";
						exit(1);
					}

					//we are the head of the pack, decrement/remove, and process
					waitingStreams--;

					if (waitingStreams <= 0)
					{
						receivedTimeMap.erase(timeStamp);
					}
				}

				if (acsConfig.output_decoded_rtcm_json)
					traceTimestamp(timeStamp);

				break;
			}
			default:
			{
				if (acsConfig.output_decoded_rtcm_json)
					traceUnknown();

				break;
			}
		}

		return E_ReturnType::OK;
	}

	E_ReturnType decode(
		vector<unsigned char>&	message);

};


unsigned	int		getbitu			(const unsigned char *buff,	int		pos, int len);
			int		getbits			(const unsigned char *buff, int		pos, int len,	bool* failure_ptr = nullptr);
unsigned	int		getbituInc		(const unsigned char *buff, int&	pos, int len);
			int		getbitsInc		(const unsigned char *buff, int&	pos, int len,	bool* failure_ptr = nullptr);


int          getbitg	(const unsigned char *buff, int  pos, int len);
int          getbitgInc	(const unsigned char *buff, int& pos, int len);

int getbitgInc(
	vector<unsigned char>&	buff,
	int&					pos,
	int						len);

unsigned int	getbitu(
	vector<unsigned char>&	buff,
	int						pos,
	int						len);

unsigned int	getbituInc(
	vector<unsigned char>&	buff,
	int&					pos,
	int						len);

int				getbitsInc(
	vector<unsigned char>&	buff,
	int&					pos,
	int						len,
	bool*					failure_ptr = nullptr);

double			getbituIncScale(
	vector<unsigned char>&	buff,
	int&					pos,
	int						len,
	double					scale);

double			getbitsIncScale(
	vector<unsigned char>&	buff,
	int&					pos,
	int						len,
	double					scale,
	bool*					failure_ptr = nullptr);

