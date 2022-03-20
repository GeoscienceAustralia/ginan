
#ifndef __RTCM_DECODER__
#define __RTCM_DECODER__

#include "ntripTrace.hpp"

struct SignalInfo
{
	uint8_t		signal_id;
	E_FType		ftype;
	E_ObsCode	rinex_observation_code;
};

struct RtcmDecoder : RtcmTrace, RtcmStatistics
{
	map<SatSys, map<E_ObsCode, int>> MSM7_lock_time;
	
	static uint16_t message_length(
		char header[2]);
	
	static RtcmMessageType message_type(
		const uint8_t message[]);
	
	int adjgpsweek(
		int week);
	
	GTime tow2Time(
		double tow);
	
	GTime getGpst();
	
	void traceLatency(GTime gpsTime);
	
	constexpr static int updateInterval[16] =
	{
		1, 2, 5, 10, 15, 30, 60, 120, 240, 300, 600, 900, 1800, 3600, 7200, 10800
	};
	
	E_FType code_to_ftype(
		E_Sys		sys, 
		E_ObsCode	code);
	
	GTime rtcm_UTC;
	
	
	boost::optional<SignalInfo> get_signal_info(
		E_Sys	sys, 
		uint8_t	signal);
	
	E_ObsCode signal_to_code(
		E_Sys	sys, 
		uint8_t	signal);
	
	GTime nowTime();

	void decodeEphemeris(
		uint8_t*			data, 
		unsigned int		message_length);
	
	void decodeSSR(
		uint8_t*			data, 
		unsigned int		message_length);
	
	GTime decodeCustomTimestamp(
		uint8_t*			data, 
		unsigned int		message_length);
	
	E_RTCMSubmessage decodeCustomId(
		uint8_t*			data, 
		unsigned int		message_length);
	
	ObsList decodeMSM7(
		uint8_t*			data, 
		unsigned int		message_length);
};


#endif
