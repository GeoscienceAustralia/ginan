
#pragma once

#include "rtcmTrace.hpp"
#include "satSys.hpp"
#include "enums.h"
#include "ssr.hpp"


using std::pair;

typedef map<SatSys, SSREph>					SsrEphMap;
typedef map<SatSys, SSRClk>	 				SsrClkMap;
typedef map<SatSys, SSRUra>					SsrUraMap;
typedef map<SatSys, SSRHRClk>				SsrHRClkMap;
typedef map<SatSys, SSRCodeBias>			SsrCBMap;
typedef map<SatSys, SSRPhasBias>			SsrPBMap;

typedef map<SatSys, SSROut> 				SsrOutMap;

void calculateSsrComb(
	GTime 			referenceTime,
	int 			udi,
	SSRMeta& 		ssrMeta,
	int 			masterIod,
	SsrOutMap&		ssrOutMap);

struct RtcmEncoder : RtcmTrace
{
	RtcmEncoder(
		string rtcmMountpoint		= "",
		string rtcmTraceFilename	= "") 
	:	RtcmTrace {rtcmMountpoint, rtcmTraceFilename}
	{
		
	}
	
	constexpr static int updateInterval[16] =
	{
		1, 2, 5, 10, 15, 30, 60, 120, 240, 300, 600, 900, 1800, 3600, 7200, 10800
	};
	
	vector<uint8_t> data;

	int masterIod 		= 1;
	
	SsrEphMap	lastRegSsrEphMap;	///< last SSR orbit corrections uploaded, used to check IODE's
	SsrClkMap	lastRegSsrClkMap;	///< last regular SSR clock corrections uploaded, used to calculate high rate SSR clock corrections

	static int getUdiIndex(
		int udi);
	
	void encodeWriteMessages(
		std::ostream&	outputStream);
	
	bool encodeWriteMessageToBuffer(
		vector<uint8_t>& buffer);
	
	vector<uint8_t> encodeTimeStampRTCM();
	
	int encodeSsrHeader(
		unsigned char*	buf,
		E_Sys			sys,
		RtcmMessageType	messCode,
		SSRMeta&		ssrMeta,
		int				iod,
		int				dispBiasConistInd	= -1,
		int				MWConistInd			= -1);

	vector<uint8_t> encodeSsrOrbClk(
		SsrOutMap&		ssrOutMap,
		RtcmMessageType	messCode);
	
	vector<uint8_t> encodeSsrUra(
		SsrOutMap&		ssrOutMap,
		RtcmMessageType	messCode);
	
	vector<uint8_t> encodeSsrCode(
		SsrCBMap&		ssrCBMap,
		RtcmMessageType	messCode);
	
	vector<uint8_t> encodeSsrPhase(
		SsrPBMap&		ssrPBMap,
		RtcmMessageType	messCode);
	
	vector<uint8_t> encodeEphemeris(
		Eph&			eph,
		RtcmMessageType	messCode);
	
	vector<uint8_t> encodeEphemeris(
		Geph&			geph,
		RtcmMessageType	messCode);
};

int setbitsInc(unsigned char *buff, int pos, int len, const				int value);
int setbituInc(unsigned char *buff, int pos, int len, const unsigned	int value);
int setbitgInc(unsigned char *buff, int pos, int len, const				int value);
