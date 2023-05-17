

// #pragma GCC optimize ("O0")

#include <memory>

using std::make_shared;

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

void UbxDecoder::decodeRAWX(
	vector<unsigned char>& payload)
{
	std::cout << "Recieved RAWX message" << std::endl;
	
	double				rcvTow	= *((double*)				&payload[0]);
	short unsigned	int	week	= *((short unsigned	int*)	&payload[8]);
	short			int	leapS	= *((short			int*)	&payload[10]);
					int	numMeas = 							 payload[11];
	
	std::cout << "has " << numMeas << " measurements" << std::endl;
	
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
		
// 		printf("meas %s %s %s %14.3lf %14.3lf\n", obs.time.to_string(4).c_str(), Sat.id().c_str(), obsCode._to_string(), pr, cp);
		auto ft = code2Freq[sys][obsCode];
		obs.SigsLists[ft].push_back(sig);
	}
	
	ObsList obsList;
	
	for (auto& [Sat, obs] : obsMap)
	{
		obsList.push_back((shared_ptr<GObs>)obs);
	}
	
	obsListList.push_back(obsList);
}

int gpsBitSFromWord(
	int	word,
	int	offset,
	int	len)
{
	word <<= (offset - 1);
	word >>= (32 - len);
	
	return word;
}

unsigned int gpsBitUFromWord(
	unsigned int	word,
	int				offset,
	int				len)
{
	word <<= (offset - 1);
	word >>= (32 - len);
	
	return word;
}

void UbxDecoder::decodeEphFrames(
	SatSys	Sat)
{
	auto& frame1 = subframeMap[Sat][1];
	auto& frame2 = subframeMap[Sat][2];
	auto& frame3 = subframeMap[Sat][3];
	
	
	Eph eph;
	bool pass = true;
	
	pass &= decodeGpsSubframe(frame1, eph);
	pass &= decodeGpsSubframe(frame2, eph);
	pass &= decodeGpsSubframe(frame3, eph);
	
}
// int save_subfrm(int sat, raw_t* raw)
// {
// 	unsigned char* p = raw->buff + 6, *q;
// 	int i, j, n, id = (U4(p + 6) >> 2) & 0x7;
// 
// 	trace(4, "save_subfrm: sat=%2d id=%d\n", sat, id);
// 
// 	if (id < 1 || 5 < id) return 0;
// 
// 	q = raw->subfrm[sat - 1] + (id - 1) * 30;
// 
// 	for (i = n = 0, p += 2; i < 10; i++, p += 4)
// 	{
// 		for (j = 23; j >= 0; j--)
// 		{
// 			*q = (*q << 1) + ((U4(p) >> j) & 1); if (++n % 8 == 0) q++;
// 		}
// 	}
// 	return id;
// }
void UbxDecoder::decodeSFRBX(
	vector<unsigned char>& payload)
{
// 	std::cout << "Recieved SFRBX message" << std::endl;
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
	
	if (satId != 6)
		return;
	
	SatSys Sat(sys, satId);
	
	printf("\n %s ", Sat.id().c_str());
	
	for (int b = 0; b < 8; b++)
	{
		auto byte = payload[b];
		if (b % 4 == 0)
			printf("--- ");
		printf("%02x ", byte);
	}
	
	vector<int> frameWords;
	
	int* words = (int*) &payload.data()[8];
	for (int f = 0; f < frameLen; f++)
	{
		int word = words[f] << 2;
		
		frameWords.push_back(word);
		
		printf("%08x ", word);
	}
	
	
	int preamble	= gpsBitUFromWord(frameWords[0],	1,	8);
	int subFrameId	= gpsBitUFromWord(frameWords[1],	20,	3);
	
	if (preamble != 0x8b)
		return;
	
	printf("\n preamble : %02x - subFrameId : %02x  - ", preamble, subFrameId);
	
	if	( subFrameId <= 0
		&&subFrameId >= 4)
	{
		return;
	}
	
// 	subframeMap[Sat][subFrameId] = frameWords;
	
	switch (subFrameId)
	{
		default:							break;
		case 2:
		case 3:		decodeEphFrames(Sat);	break;
	}
}
	
