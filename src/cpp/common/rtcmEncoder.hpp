#ifndef RTCMENCODER_H
#define RTCMENCODER_H

#include "observations.hpp"
#include "navigation.hpp"
#include "ntripTrace.hpp"
#include "common.hpp"
#include "satSys.hpp"
#include "enums.h"


using std::pair;

typedef map<SatSys, pair<SSREph, SSRClk>> 	SsrCombMap;
typedef map<SatSys, SSRPhasBias> 			SsrPBMap;
typedef map<SatSys, SSRCodeBias>			SsrCBMap;
typedef map<SatSys, SSROut> 				SsrClkOutMap;
typedef map<SatSys, SSRClk>	 				SsrClkMap;
typedef map<SatSys, SSREph>					SsrEphMap;


void calculateSsrComb(
	GTime 						curTime,
	int 						udi,
	SSRMeta 					ssrMeta,
	int 						masterIod,
	map<SatSys, SSROut>&		ssrOutMap);

struct RtcmEncoder : RtcmTrace
{
	RtcmEncoder(
		string rtcmMountpoint		= "",
		string rtcmTraceFilename	= "") : RtcmTrace{rtcmMountpoint, rtcmTraceFilename}
	{
		
	}
	
	constexpr static int updateInterval[16] =
	{
		1, 2, 5, 10, 15, 30, 60, 120, 240, 300, 600, 900, 1800, 3600, 7200, 10800
	};
	
	vector<uint8_t> data;
	
	static int getUdiIndex(
		int udi);
	
	void encodeWriteMessages(
		std::ostream&	outputStream);
	
	bool encodeWriteMessageToBuffer(
		vector<uint8_t>& buffer);
	
	vector<uint8_t> encodeSsrComb(
		map<SatSys, SSROut>&	ssrCombMap);
	
	vector<uint8_t> encodeSsrPhase(
		SsrPBMap&	ssrPBMap);
	
	vector<uint8_t> encodeSsrCode(
		SsrCBMap&	ssrCBMap);
	
	vector<uint8_t> encodeTimeStampRTCM();
};

void	rtcmEncodeToFile();

#endif
