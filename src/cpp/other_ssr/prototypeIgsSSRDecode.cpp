
#include <utility>

using std::pair;

#include "rtcmDecoder.hpp"
#include "navigation.hpp"
#include "otherSSR.hpp"
#include "biases.hpp"
#include "gTime.hpp"
#include "enums.h"
#include "ssr.hpp"

#define IGSSSRTRCLVL 4

struct SSRHeader
{
	GTime time;
	int updateInterval	= 1;
	int iod				= -1;
	int numSats;
	int dispBiasConsis;
	int mwConsis;
	int numLayers;
	double vtecQuality;

	SSRMeta ssrMeta;
};

map<E_Sys, map<int, E_ObsCode>> IGSSSRIndex2Code =
{
	{	E_Sys::GPS,
		{
			{ 0, E_ObsCode::L1C},
			{ 1, E_ObsCode::L1P},
			{ 2, E_ObsCode::L1W},
			{ 3, E_ObsCode::L1S},
			{ 4, E_ObsCode::L1L},
			{ 5, E_ObsCode::L2C},
			{ 6, E_ObsCode::L2D},
			{ 7, E_ObsCode::L2S},
			{ 8, E_ObsCode::L2L},
			{10, E_ObsCode::L2P},
			{11, E_ObsCode::L2W},
			{14, E_ObsCode::L5I},
			{15, E_ObsCode::L5Q}
		}
	},
	{	E_Sys::GLO,
		{
			{ 0, E_ObsCode::L1C},
			{ 1, E_ObsCode::L1P},
			{ 2, E_ObsCode::L2C},
			{ 3, E_ObsCode::L2P},
			{ 4, E_ObsCode::L4A},
			{ 5, E_ObsCode::L4B},
			{ 6, E_ObsCode::L6A},
			{ 7, E_ObsCode::L6B},
			{ 8, E_ObsCode::L3I},
			{ 9, E_ObsCode::L3Q}
		}
	},
	{	E_Sys::GAL,
		{
			{ 0, E_ObsCode::L1A},
			{ 1, E_ObsCode::L1B},
			{ 2, E_ObsCode::L1C},
			{ 5, E_ObsCode::L5I},
			{ 6, E_ObsCode::L5Q},
			{ 8, E_ObsCode::L7I},
			{ 9, E_ObsCode::L7Q},
			{14, E_ObsCode::L6A},
			{15, E_ObsCode::L6B},
			{16, E_ObsCode::L6C}
		}
	},
	{	E_Sys::QZS,
		{
			{ 0, E_ObsCode::L1C},
			{ 1, E_ObsCode::L1S},
			{ 2, E_ObsCode::L1L},
			{ 3, E_ObsCode::L2S},
			{ 4, E_ObsCode::L2L},
			{ 6, E_ObsCode::L5I},
			{ 7, E_ObsCode::L5Q},
			{ 9, E_ObsCode::L6S},
			{10, E_ObsCode::L6L},
			{17, E_ObsCode::L6E}
		}
	},
	{	E_Sys::BDS,
		{
			{ 0, E_ObsCode::L2I},
			{ 1, E_ObsCode::L2Q},
			{ 3, E_ObsCode::L6I},
			{ 4, E_ObsCode::L6Q},
			{ 6, E_ObsCode::L7I},
			{ 7, E_ObsCode::L7Q},
			{ 9, E_ObsCode::L1D},
			{10, E_ObsCode::L1P},
			{12, E_ObsCode::L5D},
			{13, E_ObsCode::L5P},
			{15, E_ObsCode::L1A},
			{18, E_ObsCode::L6A}
		}
	}
};

map<E_Sys, map<E_ObsCode, int>> igsSSRCode2Index =
{
	{	E_Sys::GPS,
		{
			{E_ObsCode::L1C, 0},
			{E_ObsCode::L1P, 1},
			{E_ObsCode::L1W, 2},
			{E_ObsCode::L1S, 3},
			{E_ObsCode::L1L, 4},
			{E_ObsCode::L2C, 5},
			{E_ObsCode::L2D, 6},
			{E_ObsCode::L2S, 7},
			{E_ObsCode::L2L, 8},
			{E_ObsCode::L2P,10},
			{E_ObsCode::L2W, 11},
			{E_ObsCode::L5I, 14},
			{E_ObsCode::L5Q, 15}
		}
	},
	{	E_Sys::GLO,
		{
			{ E_ObsCode::L1C, 0},
			{ E_ObsCode::L1P, 1},
			{ E_ObsCode::L2C, 2},
			{ E_ObsCode::L2P, 3},
			{ E_ObsCode::L4A, 4},
			{ E_ObsCode::L4B, 5},
			{ E_ObsCode::L6A, 6},
			{ E_ObsCode::L6B, 7},
			{ E_ObsCode::L3I, 8},
			{ E_ObsCode::L3Q, 9}
		}
	},
	{	E_Sys::GAL,
		{
			{E_ObsCode::L1A, 0},
			{E_ObsCode::L1B, 1},
			{E_ObsCode::L1C, 2},
			{E_ObsCode::L5I, 5},
			{E_ObsCode::L5Q, 6},
			{E_ObsCode::L7I, 8},
			{E_ObsCode::L7Q, 9},
			{E_ObsCode::L6A, 14},
			{E_ObsCode::L6B, 15},
			{E_ObsCode::L6C, 16}
		}
	},
	{	E_Sys::QZS,
		{
			{E_ObsCode::L1C, 0},
			{E_ObsCode::L1S, 1},
			{E_ObsCode::L1L, 2},
			{E_ObsCode::L2S, 3},
			{E_ObsCode::L2L, 4},
			{E_ObsCode::L5I, 6},
			{E_ObsCode::L5Q, 7},
			{E_ObsCode::L6S, 9},
			{E_ObsCode::L6L, 10},
			{E_ObsCode::L6E, 17}
		}
	},
	{	E_Sys::BDS,
		{
			{E_ObsCode::L2I, 0},
			{E_ObsCode::L2Q, 1},
			{E_ObsCode::L6I, 3},
			{E_ObsCode::L6Q, 4},
			{E_ObsCode::L7I, 6},
			{E_ObsCode::L7Q, 7},
			{E_ObsCode::L1D, 9},
			{E_ObsCode::L1P, 10},
			{E_ObsCode::L5D, 12},
			{E_ObsCode::L5P, 13},
			{E_ObsCode::L1A, 15},
			{E_ObsCode::L6A, 18}
		}
	}
};

map <SatSys, SSROut> igsSSRStorage;

GTime igsSSRlastTime;

void updateNavSSR()
{
	for (auto& [Sat, ssrBlock] : igsSSRStorage)
	{
		auto& ssr = nav.satNavMap[Sat].receivedSSR;

		if (ssrBlock.ephUpdated)
		if (ssr.ssrEph_map.find(ssrBlock.ssrEph.t0) == ssr.ssrEph_map.end())
		{
			ssr.ssrEph_map[ssrBlock.ssrEph.t0] = ssrBlock.ssrEph;
			tracepdeex(IGSSSRTRCLVL,std::cout, "\n#IGS_SSR ORBITS %s %s %4d %10.4f %10.4f %10.4f %d ",	Sat.id().c_str(),ssrBlock.ssrEph.t0.to_string().c_str(), ssrBlock.ssrEph.iode,ssrBlock.ssrEph.deph[0],ssrBlock.ssrEph.deph[1],ssrBlock.ssrEph.deph[2], ssrBlock.ssrEph.iod);
		}

		if (ssrBlock.clkUpdated)
		if ( ssr.ssrClk_map.find(ssrBlock.ssrClk.t0) == ssr.ssrClk_map.end())
		{
			ssr.ssrClk_map[ssrBlock.ssrClk.t0] = ssrBlock.ssrClk;
			tracepdeex(IGSSSRTRCLVL,std::cout, "\n#IGS_SSR CLOCKS %s %s      %10.4f %10.4f %10.4f %d ",	Sat.id().c_str(),ssrBlock.ssrClk.t0.to_string().c_str(), ssrBlock.ssrClk.dclk[0], ssrBlock.ssrClk.dclk[1],ssrBlock.ssrClk.dclk[2], ssrBlock.ssrClk.iod);
		}

		if (ssrBlock.hrclkUpdated)
		if (ssr.ssrHRClk_map.find(ssrBlock.ssrHRClk.t0) == ssr.ssrHRClk_map.end())
		{
			ssr.ssrHRClk_map[ssrBlock.ssrHRClk.t0] = ssrBlock.ssrHRClk;
		}

		if (ssrBlock.codeUpdated)
		{
			BiasEntry	entry;
			string		id  = Sat.id() + ":" + Sat.sysChar();
			entry.measType	= CODE;
			entry.Sat		= Sat;
			entry.tini		= ssrBlock.ssrCodeBias.t0 - ssrBlock.ssrCodeBias.udi / 2.0;
			entry.tfin		= entry.tini + acsConfig.ssrInOpts.code_bias_valid_time;
			entry.source	= "ssr";

			tracepdeex(IGSSSRTRCLVL,std::cout, "\n#IGS_SSR CODBIA %s %s: ",
					Sat.id().c_str(),ssrBlock.ssrCodeBias.t0.to_string().c_str());

			for (auto& [code,biasSSR] : ssrBlock.ssrCodeBias.obsCodeBiasMap)
			{
				entry.cod1	= code;
				entry.cod2	= E_ObsCode::NONE;
				entry.bias	= -biasSSR.bias;
				entry.var	=  0;
				entry.slop	=  0;
				entry.slpv	=  0;

				pushBiasEntry(id, entry);
				tracepdeex(IGSSSRTRCLVL,std::cout, "%s %9.4f; ", code._to_string(), biasSSR.bias);
			}

			if (ssr.ssrCodeBias_map.find(ssrBlock.ssrCodeBias.t0) == ssr.ssrCodeBias_map.end())
			{
				ssr.ssrCodeBias_map[ssrBlock.ssrCodeBias.t0] = ssrBlock.ssrCodeBias;
			}
		}

		if (ssrBlock.phaseUpdated)
		{
			BiasEntry	entry;
			string		id  = Sat.id() + ":" + string(1, Sat.sysChar());
			entry.measType	= PHAS;
			entry.Sat		= Sat;
			entry.tini		= ssrBlock.ssrPhasBias.t0 - ssrBlock.ssrPhasBias.udi/2.0;
			entry.tfin		= entry.tini + acsConfig.ssrInOpts.code_bias_valid_time;
			entry.source	= "ssr";

			tracepdeex(IGSSSRTRCLVL,std::cout, "\n#IGS_SSR PHSBIA %s %s: ",	Sat.id().c_str(),ssrBlock.ssrPhasBias.t0.to_string().c_str());

			for (auto& [code,biasSSR] : ssrBlock.ssrPhasBias.obsCodeBiasMap)
			{
				entry.cod1	= code;
				entry.cod2	= E_ObsCode::NONE;
				entry.bias	= -biasSSR.bias;
				entry.var	=  0;
				entry.slop	=  0;
				entry.slpv	=  0;

				pushBiasEntry(id, entry);
				tracepdeex(IGSSSRTRCLVL,std::cout, "%s %9.4f; ", code._to_string(), biasSSR.bias);
			}

			if (ssr.ssrPhasBias_map.find(ssrBlock.ssrPhasBias.t0) == ssr.ssrPhasBias_map.end())
			{
				ssr.ssrPhasBias_map[ssrBlock.ssrPhasBias.t0] = ssrBlock.ssrPhasBias;
			}
		}

		if (ssrBlock.uraUpdated)
		if (ssr.ssrUra_map.find(ssrBlock.ssrUra.t0) == ssr.ssrUra_map.end())
		{
			ssr.ssrUra_map[ssrBlock.ssrUra.t0] = ssrBlock.ssrUra;
		}
	}

	igsSSRStorage.clear();
}

int decodeigsSSR_header(
	vector<unsigned char>&	data,
	GTime					now,
	int 					opt,
	SSRHeader&				ssrHead)
{
	SSRMeta& ssrMeta = ssrHead.ssrMeta;

	int i = 23;
	ssrMeta.epochTime1s			= getbituInc(data, i, 20);
	ssrMeta.updateIntIndex		= getbituInc(data, i, 4);

	ssrHead.updateInterval		= ssrUdi[ssrMeta.updateIntIndex];
	ssrMeta.multipleMessage		= getbituInc(data, i, 1);
	ssrHead.iod					= getbituInc(data, i, 4);
	ssrMeta.provider			= getbituInc(data, i, 16);
	ssrMeta.solution			= getbituInc(data, i, 4);

	if (opt == 1)
		ssrMeta.referenceDatum	= getbituInc(data, i, 1);

	if (opt == 2)
	{
		ssrHead.dispBiasConsis	= getbituInc(data, i, 1);
		ssrHead.mwConsis		= getbituInc(data, i, 1);
	}

	if (opt == 3)
	{
		ssrHead.vtecQuality		= getbituInc(data, i, 9)*0.05;
		ssrHead.numLayers		= getbituInc(data, i, 2)+1;
	}
	else
		ssrHead.numSats			= getbituInc(data, i, 6);

	double tow = ssrMeta.epochTime1s;
	ssrMeta.receivedTime		= GTime(GTow(tow), now);

	if (ssrHead.updateInterval > 1)
		tow+= 0.5 * ssrHead.updateInterval;

	ssrHead.time				= GTime(GTow(tow), now);
	return i;
}

/* orbit */
void decodeigsSSR_type1(
	vector<unsigned char>&	data,
	GTime					now,
	E_Sys					sys)
{
	SSRHeader ssrHead;
	int i = decodeigsSSR_header(data, now, 1, ssrHead);

	if (ssrHead.ssrMeta.receivedTime > igsSSRlastTime)
		igsSSRlastTime = ssrHead.ssrMeta.receivedTime;
	if (now < igsSSRlastTime)
		return;

	if (i==0)
		return;

	for (int sat = 0; sat < ssrHead.numSats && i + 135 <= data.size() * 8; sat++)
	{
		SSREph ssrEph;
		int	satId			= getbituInc(data, i, 6);
		ssrEph.iode			= getbituInc(data, i, 8);
		ssrEph.deph[0]		= getbitsInc(data, i, 22) * 0.1e-3; // Position, radial, along track, cross track.
		ssrEph.deph[1]		= getbitsInc(data, i, 20) * 0.4e-3;
		ssrEph.deph[2]		= getbitsInc(data, i, 20) * 0.4e-3;
		ssrEph.ddeph[0]		= getbitsInc(data, i, 21) * 0.001e-3; // Velocity
		ssrEph.ddeph[1]		= getbitsInc(data, i, 19) * 0.004e-3;
		ssrEph.ddeph[2]		= getbitsInc(data, i, 19) * 0.004e-3;

		ssrEph.ssrMeta		= ssrHead.ssrMeta;
		ssrEph.t0			= ssrHead.time;
		ssrEph.udi			= ssrHead.updateInterval;
		ssrEph.iod 			= ssrHead.iod;

		SatSys Sat(sys, satId);
		igsSSRStorage[Sat].ssrEph		= ssrEph;
		igsSSRStorage[Sat].ephUpdated	= true;
	}

	if (ssrHead.ssrMeta.multipleMessage == 0)
		updateNavSSR();

	return;
}

/* clock */
void decodeigsSSR_type2(
	vector<unsigned char>&	data,
	GTime					now,
	E_Sys					sys)
{
	SSRHeader ssrHead;
	int i = decodeigsSSR_header(data, now, 0, ssrHead);

	if (ssrHead.ssrMeta.receivedTime > igsSSRlastTime)
		igsSSRlastTime = ssrHead.ssrMeta.receivedTime;
	if (now < igsSSRlastTime)
		return;

	for (int sat = 0; sat < ssrHead.numSats && i + 76 <= data.size() * 8; sat++)
	{
		SSRClk ssrClk;
		int	satId			= getbituInc(data, i, 6);
		ssrClk.dclk[0]		= getbitsInc(data, i, 22) * 0.1e-3;
		ssrClk.dclk[1]		= getbitsInc(data, i, 21) * 0.001e-3;
		ssrClk.dclk[2]		= getbitsInc(data, i, 27) * 0.00002e-3;

		ssrClk.ssrMeta		= ssrHead.ssrMeta;
		ssrClk.t0			= ssrHead.time;
		ssrClk.udi			= ssrHead.updateInterval;
		ssrClk.iod 			= ssrHead.iod;

		SatSys Sat(sys, satId);
		igsSSRStorage[Sat].ssrClk		= ssrClk;
		igsSSRStorage[Sat].clkUpdated	= true;
	}

	if (ssrHead.ssrMeta.multipleMessage == 0)
		updateNavSSR();

	return;
}

/* combined */
void decodeigsSSR_type3(
	vector<unsigned char>&	data,
	GTime					now,
	E_Sys					sys)
{
	SSRHeader ssrHead;
	int i = decodeigsSSR_header(data, now, 1, ssrHead);

	if (ssrHead.ssrMeta.receivedTime > igsSSRlastTime)
		igsSSRlastTime = ssrHead.ssrMeta.receivedTime;
	if (now < igsSSRlastTime)
		return;

	for (int sat = 0; sat < ssrHead.numSats && i + 205 <= data.size() * 8; sat++)
	{
		SSREph ssrEph;
		SSRClk ssrClk;

		int	satId			= getbituInc(data, i, 6);
		ssrEph.iode			= getbituInc(data, i, 8);
		ssrEph.deph[0]		= getbitsInc(data, i, 22) * 0.1e-3; // Position, radial, along track, cross track.
		ssrEph.deph[1]		= getbitsInc(data, i, 20) * 0.4e-3;
		ssrEph.deph[2]		= getbitsInc(data, i, 20) * 0.4e-3;
		ssrEph.ddeph[0]		= getbitsInc(data, i, 21) * 0.001e-3; // Velocity
		ssrEph.ddeph[1]		= getbitsInc(data, i, 19) * 0.004e-3;
		ssrEph.ddeph[2]		= getbitsInc(data, i, 19) * 0.004e-3;
		ssrClk.dclk[0]		= getbitsInc(data, i, 22) * 0.1e-3;
		ssrClk.dclk[1]		= getbitsInc(data, i, 21) * 0.001e-3;
		ssrClk.dclk[2]		= getbitsInc(data, i, 27) * 0.00002e-3;

		ssrEph.ssrMeta		= ssrHead.ssrMeta;
		ssrEph.t0			= ssrHead.time;
		ssrEph.udi			= ssrHead.updateInterval;
		ssrEph.iod 			= ssrHead.iod;

		ssrClk.ssrMeta		= ssrHead.ssrMeta;
		ssrClk.t0			= ssrHead.time;
		ssrClk.udi			= ssrHead.updateInterval;
		ssrClk.iod 			= ssrHead.iod;

		SatSys Sat(sys, satId);
		igsSSRStorage[Sat].ssrEph		= ssrEph;
		igsSSRStorage[Sat].ephUpdated	= true;
		igsSSRStorage[Sat].ssrClk		= ssrClk;
		igsSSRStorage[Sat].clkUpdated	= true;
	}

	if (ssrHead.ssrMeta.multipleMessage == 0)
		updateNavSSR();

	return;
}

/* HR clocks */
void decodeigsSSR_type4(
	vector<unsigned char>&	data,
	GTime					now,
	E_Sys					sys)
{
	SSRHeader ssrHead;
	int i = decodeigsSSR_header(data, now, 0, ssrHead);

	if (ssrHead.ssrMeta.receivedTime > igsSSRlastTime)
		igsSSRlastTime = ssrHead.ssrMeta.receivedTime;
	if (now < igsSSRlastTime)
		return;

	for (int sat = 0; sat < ssrHead.numSats && i + 28 <= data.size() * 8; sat++)
	{
		SSRHRClk ssrHRClk;
		int	satId			= getbituInc(data, i, 6);
		ssrHRClk.hrclk		= getbitsInc(data, i, 22) * 0.1e-3;

		ssrHRClk.ssrMeta	= ssrHead.ssrMeta;
		ssrHRClk.t0			= ssrHead.time;
		ssrHRClk.udi		= ssrHead.updateInterval;
		ssrHRClk.iod 		= ssrHead.iod;

		SatSys Sat(sys, satId);
		igsSSRStorage[Sat].ssrHRClk		= ssrHRClk;
		igsSSRStorage[Sat].hrclkUpdated	= true;
	}

	if (ssrHead.ssrMeta.multipleMessage == 0)
		updateNavSSR();

	return;
}

/* Code Bias */
void decodeigsSSR_type5(
	vector<unsigned char>&	data,
	GTime					now,
	E_Sys					sys)
{
	SSRHeader ssrHead;
	int i = decodeigsSSR_header(data, now, 0, ssrHead);

	if (ssrHead.ssrMeta.receivedTime > igsSSRlastTime)
		igsSSRlastTime = ssrHead.ssrMeta.receivedTime;
	if (now < igsSSRlastTime)
		return;

	for (int sat = 0; sat < ssrHead.numSats && i + 11 <= data.size() * 8; sat++)
	{
		int	satId			= getbituInc(data, i, 6);
		int nbias			= getbituInc(data, i, 5);

		SSRCodeBias ssrBiasCode;
		ssrBiasCode.ssrMeta	= ssrHead.ssrMeta;
		ssrBiasCode.t0		= ssrHead.time;
		ssrBiasCode.udi		= ssrHead.updateInterval;
		ssrBiasCode.iod 	= ssrHead.iod;

		for (int k = 0; k < nbias && i + 19 <= data.size() * 8; k++)
		{
			int	rtcm_code	= getbituInc(data, i, 5);
			double bias		= getbitsInc(data, i, 14) * 0.01;
			if (IGSSSRIndex2Code[sys].find(rtcm_code) == IGSSSRIndex2Code[sys].end())
				continue;
			E_ObsCode code = IGSSSRIndex2Code[sys][rtcm_code];
			ssrBiasCode.obsCodeBiasMap[code].bias = bias;
		}

		SatSys Sat(sys, satId);
		igsSSRStorage[Sat].ssrCodeBias	= ssrBiasCode;
		igsSSRStorage[Sat].codeUpdated	= true;
	}

	if (ssrHead.ssrMeta.multipleMessage == 0)
		updateNavSSR();

	return;
}

/* Phase Bias */
void decodeigsSSR_type6(
	vector<unsigned char>&	data,
	GTime					now,
	E_Sys					sys)
{
	SSRHeader ssrHead;
	int i = decodeigsSSR_header(data, now, 2, ssrHead);

	if (ssrHead.ssrMeta.receivedTime > igsSSRlastTime)
		igsSSRlastTime = ssrHead.ssrMeta.receivedTime;
	if (now < igsSSRlastTime)
		return;

	for (int sat = 0; sat < ssrHead.numSats && i + 28 <= data.size() * 8; sat++)
	{
		int	satId					= getbituInc(data, i, 6);
		SSRPhase ssrPhase;
		ssrPhase.dispBiasConistInd	= ssrHead.dispBiasConsis;
		ssrPhase.MWConistInd		= ssrHead.mwConsis;

		int nbias					= getbituInc(data, i, 5);
		ssrPhase.yawAngle			= getbituInc(data, i, 9)/256.0	*PI;
		ssrPhase.yawRate			= getbitsInc(data, i, 8)/8192.0	*PI;

		SSRPhasBias ssrBiasPhas;
		ssrBiasPhas.ssrMeta			= ssrHead.ssrMeta;
		ssrBiasPhas.t0				= ssrHead.time;
		ssrBiasPhas.udi				= ssrHead.updateInterval;
		ssrBiasPhas.iod 			= ssrHead.iod;
		ssrBiasPhas.ssrPhase		= ssrPhase;

		for (int k = 0; k < nbias && i + 32 <= data.size() * 8; k++)
		{
			int	rtcm_code				= getbituInc(data, i, 5);

			SSRPhaseCh ssrPhaseCh;
			ssrPhaseCh.signalIntInd		= getbituInc(data, i, 1);
			ssrPhaseCh.signalWLIntInd	= getbituInc(data, i, 2);
			ssrPhaseCh.signalDisconCnt	= getbituInc(data, i, 4);
			double phaseBias			= getbitsInc(data, i, 20) * 0.0001;

			if (IGSSSRIndex2Code[sys].find(rtcm_code) == IGSSSRIndex2Code[sys].end())
				continue;

			E_ObsCode code = IGSSSRIndex2Code[sys][rtcm_code];

			ssrBiasPhas.obsCodeBiasMap[code].bias	= phaseBias;
			ssrBiasPhas.ssrPhaseChs	[code]			= ssrPhaseCh;
		}

		SatSys Sat(sys, satId);
		igsSSRStorage[Sat].ssrPhasBias	= ssrBiasPhas;
		igsSSRStorage[Sat].phaseUpdated	= true;
	}

	if (ssrHead.ssrMeta.multipleMessage == 0)
		updateNavSSR();

	return;
}

/* URA message */
void decodeigsSSR_type7(
	vector<unsigned char>&	data,
	GTime					now,
	E_Sys					sys)
{
	SSRHeader ssrHead;
	int i = decodeigsSSR_header(data, now, 0, ssrHead);

	if (ssrHead.ssrMeta.receivedTime > igsSSRlastTime)
		igsSSRlastTime = ssrHead.ssrMeta.receivedTime;
	if (now < igsSSRlastTime)
		return;

	for (int sat = 0; sat < ssrHead.numSats && i + 12 <= data.size() * 8; sat++)
	{
		SSRUra ssrUra;
		int	satId			= getbituInc(data, i, 6);
		int	uraInd			= getbitsInc(data, i, 6);

		ssrUra.t0			= ssrHead.time;
		ssrUra.udi			= ssrHead.updateInterval;
		ssrUra.iod 			= ssrHead.iod;
		ssrUra.ura			= uraInd;

		SatSys Sat(sys, satId);
		igsSSRStorage[Sat].ssrUra		= ssrUra;
		igsSSRStorage[Sat].uraUpdated	= true;
	}

	if (ssrHead.ssrMeta.multipleMessage == 0)
		updateNavSSR();

	return;
}

/* Iono VTEC */
void decodeigsSSR_type8(
	vector<unsigned char>&	data,
	GTime					now)
{
	SSRHeader ssrHead;

	int i = decodeigsSSR_header(data, now, 3, ssrHead);

	if (ssrHead.ssrMeta.receivedTime > igsSSRlastTime)
		igsSSRlastTime = ssrHead.ssrMeta.receivedTime;
	if (now < igsSSRlastTime)
		return;

	SSRAtmGlobal ssrAtmGlobal;
	ssrAtmGlobal.numberLayers  = ssrHead.numLayers;	//todo aaron, can these be the same thing?
	ssrAtmGlobal.vtecQuality   = ssrHead.vtecQuality;
	ssrAtmGlobal.time			 = ssrHead.time;

	for (int layerNum = 0; layerNum < ssrHead.numLayers && i + 16 <= data.size() * 8; layerNum++)
	{
		auto& layer = ssrAtmGlobal.layers[layerNum];

		layer.height	= getbituInc(data, i, 8) * 10;
		layer.maxDegree	= getbituInc(data, i, 4) + 1;
		layer.maxOrder	= getbituInc(data, i, 4) + 1;

		int nind = 0;
		for (int ord = 0;	ord < layer.maxOrder;								ord++)
		for (int deg = ord;	deg < layer.maxDegree && i + 16 <= data.size() * 8;	deg++)		//todo aaron duplicate size checks redundant?
		{
			layer.sphHarmonic[nind].layer = layerNum;

			auto& sphComp		= layer.sphHarmonic[nind];
			sphComp.order		= ord;
			sphComp.degree		= deg;
			sphComp.trigType	= E_TrigType::SIN;
			sphComp.value		= getbitsInc(data, i, 16) * 0.005;

			if ((i+16)>(data.size()*8))
				return;

			nind++;
		}
		for (int ord = 1;	ord < layer.maxOrder;								ord++)
		for (int deg = ord;	deg < layer.maxDegree && i + 16 <= data.size() * 8;	deg++)
		{
			layer.sphHarmonic[nind].layer = layerNum;

			auto& sphComp		= layer.sphHarmonic[nind];
			sphComp.order		= ord;
			sphComp.degree		= deg;
			sphComp.trigType	= E_TrigType::COS;
			sphComp.value		= getbitsInc(data, i, 16) * 0.005;

			if ((i+16)>(data.size()*8))
				return;

			nind++;
		}
	}

	if (ssrHead.ssrMeta.multipleMessage == 0)
		updateNavSSR();

	nav.ssrAtm.atmosGlobalMap[ssrAtmGlobal.time] = ssrAtmGlobal;

	return;
}

E_ReturnType decodeigsSSR(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (data.size() < 7)
		return E_ReturnType::BAD_LENGTH;

	if (now < igsSSRlastTime)
		return E_ReturnType::WAIT;

	int stype = getbitu(data,15,8);
	IgsSSRSubtype subType = IgsSSRSubtype::_from_integral(stype);

	E_Sys sys;
	IgsSSRSubtype group = IGS_SSR_group(subType, sys);

	switch (group)
	{
		case IgsSSRSubtype::GROUP_ORB:		decodeigsSSR_type1(data, now, sys);		break;
		case IgsSSRSubtype::GROUP_CLK:		decodeigsSSR_type2(data, now, sys);		break;
		case IgsSSRSubtype::GROUP_CMB:		decodeigsSSR_type3(data, now, sys);		break;
		case IgsSSRSubtype::GROUP_HRC:		decodeigsSSR_type4(data, now, sys);		break;
		case IgsSSRSubtype::GROUP_COD:		decodeigsSSR_type5(data, now, sys);		break;
		case IgsSSRSubtype::GROUP_PHS:		decodeigsSSR_type6(data, now, sys);		break;
		case IgsSSRSubtype::GROUP_URA:		decodeigsSSR_type7(data, now, sys);		break;
		case IgsSSRSubtype::GROUP_ION:		decodeigsSSR_type8(data, now);			break;
	}

	if (now < igsSSRlastTime)
		return E_ReturnType::WAIT;

	return E_ReturnType::OK;
}
