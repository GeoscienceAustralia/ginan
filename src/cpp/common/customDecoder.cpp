

// #pragma GCC optimize ("O0")

#include "customDecoder.hpp"
#include "observations.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "gTime.hpp"
#include "enums.h"


map<string, map<GTime, Vector3d,	std::greater<GTime>>>	CustomDecoder::gyroDataMaps;
map<string, map<GTime, Vector3d,	std::greater<GTime>>>	CustomDecoder::acclDataMaps;
map<string, map<GTime, double,		std::greater<GTime>>>	CustomDecoder::tempDataMaps;

void CustomDecoder::decodeRAWX(
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


void CustomDecoder::decodeMEAS(
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
#include <bsoncxx/json.hpp>

void CustomDecoder::decodeEphFrames(
	SatSys	Sat)
{
	Eph eph;
	bool pass = true;

	if (pass)
	{
		std::cout << "\n" << "*";
		eph.Sat		= Sat;
		eph.type	= E_NavMsgType::LNAV;
		nav.ephMap[eph.Sat][eph.type][eph.toe] = eph;


// 		if (acsConfig.output_decoded_rtcm_json)
// 			traceBrdcEph(RtcmMessageType::GPS_EPHEMERIS, eph);
//
// 		if (acsConfig.localMongo.output_rtcm_messages) 11, iode27
// 			mongoBrdcEph(eph);
	}
}

void CustomDecoder::decodeSFRBX(
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

}

