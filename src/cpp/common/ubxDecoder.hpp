
#pragma once

// #pragma GCC optimize ("O0")

#include <vector>
#include <map>

using std::vector;
using std::map;

#include "icdDecoder.hpp"
#include "streamObs.hpp"
#include "gTime.hpp"
#include "enums.h"

extern map<int,				E_Sys>		ubxSysMap;
extern map<E_Sys, map<int,	E_ObsCode>>	ubxSysObsCodeMap;

struct UbxDecoder : ObsLister, IcdDecoder
{
	static 			map<string, map<GTime, Vector3d,	std::greater<GTime>>>	gyroDataMaps;
	static 			map<string, map<GTime, Vector3d,	std::greater<GTime>>>	acclDataMaps;
	static 			map<string, map<GTime, double,		std::greater<GTime>>>	tempDataMaps;
	
	unsigned int	lastTimeTag = 0;
	GTime			lastTime;
	
	string			recId;
	
	string raw_ubx_filename;
	
	void decodeEphFrames(
		SatSys	Sat);
	
	void decodeRAWX(
		vector<unsigned char>& payload);
	
	void decodeSFRBX(
		vector<unsigned char>& payload);
	
	void decodeMEAS(
		vector<unsigned char>& payload);
	
	void decodeRXM(
		vector<unsigned char>&	payload,
		unsigned char			id)
	{
// 		printf("\nReceived RXM-0x%02x message", id);
		switch (id)
		{
			default:
			{
				break;
			}
			case E_RXMId::RAWX:		{	decodeRAWX	(payload);	break;	}
			case E_RXMId::SFRBX:	{	decodeSFRBX	(payload);	break;	}
		}
	}
	
	void decodeESF(
		vector<unsigned char>&	payload,
		unsigned char			id)
	{
// 		printf("\nReceived ESF-0x%02x message", id);
		switch (id)
		{
			default:
			{
				break;
			}
			case E_ESFId::MEAS:		{	decodeMEAS	(payload);	break;	}
		}
	}
	
	void decode(
		unsigned char			ubxClass,
		unsigned char			id,
		vector<unsigned char>&	payload)
	{
// 		printf("\nReceived ubx: 0x%02x : 0x%02x > %ld bytes", ubxClass, id, payload.size());
		
		switch (ubxClass)
		{
			default:				{								break;	}
			case E_UBXClass::RXM:	{	decodeRXM(payload, id);		break;	}
			case E_UBXClass::ESF:	{	decodeESF(payload, id);		break;	}
		}
	}
	
	void recordFrame(
		unsigned char			ubxClass,
		unsigned char			id,
		vector<unsigned char>&	data,
		unsigned short int		crcRead)	
	{
		if (raw_ubx_filename.empty())
		{
			return;
		}
		
		std::ofstream ofs(raw_ubx_filename, std::ofstream::app);

		if (!ofs)
		{
			return;
		}
		
// 		//Write the custom time stamp message.
// 		RtcmEncoder encoder;
// 		encoder.rtcmTraceFilename = rtcmTraceFilename;
// 		
// 		auto buffer	= encoder.encodeTimeStampRTCM();	
// 		bool write	= encoder.encodeWriteMessageToBuffer(buffer);
// 
// 		if (write)
// 		{
// 			encoder.encodeWriteMessages(ofs);
// 		}
		
		//copy the message to the output file too
		unsigned short int payloadLength = data.size();
		
		ofs.write((char *)&ubxClass,		1);
		ofs.write((char *)&id,				1);
		ofs.write((char *)&payloadLength,	2);
		ofs.write((char *)data.data(),		data.size());
		ofs.write((char *)&crcRead,			3);
	}
};
