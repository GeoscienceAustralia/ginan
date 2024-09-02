

// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

FileType UBX__()
{

}

#include "observations.hpp"
#include "navigation.hpp"
#include "ubxDecoder.hpp"
#include "icdDecoder.hpp"
#include "streamUbx.hpp"
#include "constants.hpp"
#include "gTime.hpp"
#include "enums.h"


map<int, E_Sys>	ubxSysMap =
{
	{0, E_Sys::GPS},
	{1, E_Sys::SBS},
	{2, E_Sys::GAL},
	{3, E_Sys::BDS},
	{4, E_Sys::IMS},
	{5, E_Sys::QZS},
	{6, E_Sys::GLO}
};

map<E_Sys, map<int, E_ObsCode>>	ubxSysObsCodeMap =
{
	{	E_Sys::GPS,
		{
			{0,	E_ObsCode::L1C},
			{3,	E_ObsCode::L2C},
			{4,	E_ObsCode::L2C},
			{6,	E_ObsCode::L5I},
			{7,	E_ObsCode::L5Q}
		}
	}
};

map<string, map<GTime, Vector3d,	std::greater<GTime>>>	UbxDecoder::gyroDataMaps;
map<string, map<GTime, Vector3d,	std::greater<GTime>>>	UbxDecoder::acclDataMaps;
map<string, map<GTime, double,		std::greater<GTime>>>	UbxDecoder::tempDataMaps;

void UbxDecoder::decodeRAWX(
	vector<unsigned char>& payload)
{
// 	std::cout << "Recieved RAWX message" << "\n";

	double				rcvTow	= *((double*)				&payload[0]);
	short unsigned	int	week	= *((short unsigned	int*)	&payload[8]);
	char				leapS	= *((char*)					&payload[10]);
	unsigned char		numMeas = 							 payload[11];

	if (payload.size() != 16 + 32 * numMeas)
	{
		return;
	}

// 	std::cout << "\n" << "Recieved RAWX message has " << numMeas << " measurements" << "\n";

	map<SatSys, GObs> obsMap;

	for (int i = 0; i < numMeas; i++)
	{
		unsigned char* measPayload = &payload[i*32];	//below offsets dont start at zero, this matches spec

		double	pr		= *((double*)	&measPayload[16]);
		double	cp		= *((double*)	&measPayload[24]);
		float	dop		= *((float*)	&measPayload[32]);
		int		gnssId	=				 measPayload[36];
		int		satId	=				 measPayload[37];
		int		sigId	=				 measPayload[38];

		E_Sys sys = ubxSysMap[gnssId];

		if (sys == +E_Sys::NONE)
			continue;

		E_ObsCode obsCode = ubxSysObsCodeMap[sys][sigId];

		if (obsCode == +E_ObsCode::NONE)
			continue;

		Sig sig;
		sig.code	= obsCode;
		sig.L	= cp;
		sig.P	= pr;
		sig.D	= dop;

		SatSys Sat(sys, satId);
		auto& obs = obsMap[Sat];
		obs.Sat		= Sat;
		obs.time	= gpst2time(week, rcvTow);

		printf("meas %s %s %s %14.3lf %14.3lf\n", obs.time.to_string().c_str(), Sat.id().c_str(), obsCode._to_string(), pr, cp);
		auto ft = code2Freq[sys][obsCode];
		obs.sigsLists[ft].push_back(sig);
	}

	ObsList obsList;

	for (auto& [Sat, obs] : obsMap)
	{
		obsList.push_back((shared_ptr<GObs>)obs);
	}

	obsListList.push_back(obsList);

	lastTimeTag	= 0;
	lastTime	= gpst2time(week, rcvTow);
}


void UbxDecoder::decodeMEAS(
	vector<unsigned char>& payload)
{
			unsigned int	timeTag	= *((unsigned int*)			&payload[0]);
	short	unsigned int	flags	= *((short unsigned int*)	&payload[4]);
	short	unsigned int	id		= *((short unsigned int*)	&payload[6]);

	int numMeas = flags >> 11;

	//adjust time tags
	if (lastTimeTag == 0)
	{
		lastTimeTag = timeTag;
	}

	double timeOffset = ((signed int)(timeTag - lastTimeTag)) * 1e-3;

// 	std::cout << "\n" << "Recieved MEAS message has " << numMeas << " measurements at " << timeOffset << "\n";

	for (int i = 0; i < numMeas; i++)
	{
		unsigned int data			= *((unsigned int*)			&payload[8 + 4 * i]);

		data &= 0x3fffffff;

		unsigned int dataType	= data >> 24;
		int dataField			= data &= 0x00ffffff;

		dataField <<= 8;	//get leading ones
		dataField >>= 8;

		E_MEASDataType measDataType = E_MEASDataType::_from_integral(dataType);

		switch (measDataType)
		{
			default:
			{
// 				std::cout << "\n" << measDataType._to_string();
				break;
			}
			case E_MEASDataType::GYRO_X:
			case E_MEASDataType::GYRO_Y:
			case E_MEASDataType::GYRO_Z:
			{
				double gyro = dataField * P2_12;
// 				std::cout << "\n" << measDataType._to_string() << " : " << gyro;

				int index = 0;
				if		(measDataType == +E_MEASDataType::GYRO_X)	index = 0;		//ubx indices are dumb and not ordered
				else if	(measDataType == +E_MEASDataType::GYRO_Y)	index = 1;
				else if (measDataType == +E_MEASDataType::GYRO_Z)	index = 2;

				gyroDataMaps[recId][lastTime + timeOffset][index] = gyro;

				break;
			}
			case E_MEASDataType::ACCL_X:
			case E_MEASDataType::ACCL_Y:
			case E_MEASDataType::ACCL_Z:
			{
				double accl = dataField * P2_10;
// 				std::cout << "\n" << measDataType._to_string() << " : " << accl;

				int index = 0;
				if		(measDataType == +E_MEASDataType::ACCL_X)	index = 0;
				else if	(measDataType == +E_MEASDataType::ACCL_Y)	index = 1;
				else if (measDataType == +E_MEASDataType::ACCL_Z)	index = 2;

				acclDataMaps[recId][lastTime + timeOffset][index] = accl;

				break;
			}
			case E_MEASDataType::GYRO_TEMP:
			{
				double temp = dataField * 1e-2;
// 				std::cout << "\n" << measDataType._to_string() << " : " << temp;

				tempDataMaps[recId][lastTime + timeOffset] = temp;

				break;
			}
		}
	}
}

signed int gpsBitSFromWord(
	vector<int>&	words,
	int				wordNum,
	int				offset,
	int				len)
{
	signed int word = words[wordNum-1];
	offset -= 1;		//icd counts from 1
	offset %= 30;		//icd words have indices like 31..
	word <<= offset;
	word >>= 32 - len;

	return word;
}

unsigned int gpsBitUFromWord(
	vector<int>&	words,
	int				wordNum,
	int				offset,
	int				len)
{
	unsigned int word = words[wordNum-1];
	offset -= 1;		//icd counts from 1
	offset %= 30;		//icd words have indices like 31..
	word <<= offset;
	word >>= 32 - len;

	return word;
}

#include <bsoncxx/json.hpp>

void UbxDecoder::decodeEphFrames(
	SatSys	Sat)
{
	Eph eph;
	bool pass = true;

	pass &= decodeGpsSubframe(subframeMap[Sat][1], eph);
	pass &= decodeGpsSubframe(subframeMap[Sat][2], eph);
	pass &= decodeGpsSubframe(subframeMap[Sat][3], eph);

	if (pass)
	{
		std::cout << "\n" << "*";
		eph.Sat		= Sat;
		eph.type	= E_NavMsgType::LNAV;
		nav.ephMap[eph.Sat][eph.type][eph.toe] = eph;


		bsoncxx::builder::basic::document doc = {};

		traceBrdcEphBody(doc, eph);

		std::cout << bsoncxx::to_json(doc) << "\n";
//
// 		if (acsConfig.output_decoded_rtcm_json)
// 			traceBrdcEph(RtcmMessageType::GPS_EPHEMERIS, eph);
//
// 		if (acsConfig.localMongo.output_rtcm_messages) 11, iode27
// 			mongoBrdcEph(eph);
	}
}

void UbxDecoder::decodeSFRBX(
	vector<unsigned char>& payload)
{
// 	std::cout << "Recieved SFRBX message" << "\n";
	if (payload.size() < 5)
		return;

	int gnssId		= payload[0];
	int satId		= payload[1];
	int frameLen	= payload[4];

	if (frameLen != (payload.size() - 8) / 4.0)
		return;

	E_Sys sys = ubxSysMap[gnssId];

	if (sys == +E_Sys::NONE)
		return;

	if (sys != +E_Sys::GPS)
		return;

	SatSys Sat(sys, satId);

// 	printf("\n %s ", Sat.id().c_str());

	for (int b = 0; b < 8; b++)
	{
		auto byte = payload[b];
// 		if (b % 4 == 0)
// 			printf("--- ");
// 		printf("%02x ", byte);
	}

	vector<int> frameWords;

	int* words = (int*) &payload.data()[8];
	for (int f = 0; f < frameLen; f++)
	{
		int word = words[f] << 2;

		frameWords.push_back(word);

// 		printf("%08x ", word);
	}

	int preamble	= gpsBitUFromWord(frameWords, 1, 1,		8);
	int subFrameId	= gpsBitUFromWord(frameWords, 2, 20,	3);

	if (preamble != 0x8b)
		return;

// 	printf("\n preamble : %02x - subFrameId : %02x  - ", preamble, subFrameId);

	if	( subFrameId <= 0
		&&subFrameId >= 4)
	{
		return;
	}

// 	vector<unsigned char> subFrame;
// 	int byteBits = 0;
// 	unsigned char byte;
// 	for (auto& word : frameWords)
// 	for (int j = 23; j >= 0; j--)
// 	{
// 		byte <<= 1;
// 		byte += (word >> j) & 1;
//
// 		byteBits++;
// 		if (byteBits == 8)
// 		{
// 			byteBits = 0;
// 			subFrame.push_back(byte);
// 		}
// 	}

	if (1)
	{
		subframeMap[Sat][subFrameId] = frameWords;
	}

	switch (subFrameId)
	{
		default:							break;
		case 2:
		case 3:		decodeEphFrames(Sat);	break;
	}
}

