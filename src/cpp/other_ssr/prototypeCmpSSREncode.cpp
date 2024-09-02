#include <tuple>

using std::tuple;

#include "rtcmEncoder.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "otherSSR.hpp"
#include "satSys.hpp"
#include "ssr.hpp"

int currentIOD = -1;
map<int,	SatSys>							currentSatMap;
map<E_Sys,	map <int,E_ObsCode>>			currentBiasMap;
map<int,	tuple<SatSys,E_ObsCode>>		currentCellMap;
map<SatSys,int>								currentSatIodes;

map<SatSys, map<E_ObsCode ,double>>			phaseBiasOffset;
map<SatSys, map<E_ObsCode, int>>			phaseDiscCountr;

map<int,vector<uint8_t>>					cmpSSRServiceMessages;

map <E_Sys,int> compactSSRSysMap =
{
	{E_Sys::GPS, 0},
	{E_Sys::GLO, 1},
	{E_Sys::GAL, 2},
	{E_Sys::BDS, 3},
	{E_Sys::QZS, 4},
	{E_Sys::SBS, 5}
};

map<E_Sys, map<int, E_ObsCode>> cmpSSRIndex2Code =
{
	{	E_Sys::GPS,
		{
			{ 0, E_ObsCode::L1C},
			{ 1, E_ObsCode::L1P},
			{ 2, E_ObsCode::L1W},
			{ 3, E_ObsCode::L1S},
			{ 4, E_ObsCode::L1L},
			{ 5, E_ObsCode::L1X},
			{ 6, E_ObsCode::L2S},
			{ 7, E_ObsCode::L2L},
			{ 8, E_ObsCode::L2X},
			{ 9, E_ObsCode::L2P},
			{10, E_ObsCode::L2W},
			{11, E_ObsCode::L5I},
			{12, E_ObsCode::L5Q},
			{13, E_ObsCode::L5X}
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
			{ 6, E_ObsCode::L4X},
			{ 7, E_ObsCode::L6A},
			{ 8, E_ObsCode::L6B},
			{ 9, E_ObsCode::L6X},
			{10, E_ObsCode::L3I},
			{11, E_ObsCode::L3Q},
			{12, E_ObsCode::L3X}
		}
	},
	{	E_Sys::GAL,
		{
			{ 0, E_ObsCode::L1B},
			{ 1, E_ObsCode::L1C},
			{ 2, E_ObsCode::L1X},
			{ 3, E_ObsCode::L5I},
			{ 4, E_ObsCode::L5Q},
			{ 5, E_ObsCode::L5X},
			{ 6, E_ObsCode::L7I},
			{ 7, E_ObsCode::L7Q},
			{ 8, E_ObsCode::L7X},
			{ 9, E_ObsCode::L8I},
			{10, E_ObsCode::L8Q},
			{11, E_ObsCode::L8X}
		}
	},
	{	E_Sys::QZS,
		{
			{ 0, E_ObsCode::L1C},
			{ 1, E_ObsCode::L1S},
			{ 2, E_ObsCode::L1L},
			{ 3, E_ObsCode::L1X},
			{ 4, E_ObsCode::L2S},
			{ 5, E_ObsCode::L2L},
			{ 6, E_ObsCode::L2X},
			{ 7, E_ObsCode::L5I},
			{ 8, E_ObsCode::L5Q},
			{ 9, E_ObsCode::L5X}
		}
	},
	{	E_Sys::BDS,
		{
			{ 0, E_ObsCode::L2I},
			{ 1, E_ObsCode::L2Q},
			{ 2, E_ObsCode::L2X},
			{ 3, E_ObsCode::L6I},
			{ 4, E_ObsCode::L6Q},
			{ 5, E_ObsCode::L6X},
			{ 6, E_ObsCode::L7I},
			{ 7, E_ObsCode::L7Q},
			{ 8, E_ObsCode::L7X}
		}
	}
};

vector<uint8_t>  encodecompactMSK(
	map<SatSys, SSROut>&		orbClkMap,
	map<SatSys, SSRCodeBias>&	codeBiasMap,
	map<SatSys, SSRPhasBias>&	phaseBiasMap,
	SSRAtm						ssrAtm,
	int							updateIntIndex)
{
	vector<uint8_t> buffer;

	currentSatMap	.clear();
	currentBiasMap	.clear();
	currentCellMap	.clear();
	currentSatIodes	.clear();

	int epochTime1s		= -1;

	map <E_Sys, int> sysNsat;
	map <SatSys,int> availSat;

	/* get available sat from OrbClkMaps */
	for (auto& [Sat, data] : orbClkMap)
	if (availSat.find(Sat) == availSat.end())
	{
		availSat[Sat] = sysNsat[Sat.sys];
		sysNsat[Sat.sys]++;

		currentSatIodes[Sat] = data.ssrEph.iode;

		if (epochTime1s < 0)
		{
			epochTime1s		= data.ssrEph.ssrMeta.epochTime1s;
		}
	}

	map<E_Sys,int>					sysNbias;
	map<E_Sys,map <E_ObsCode,int>>	availBias;

	/* get available sat and biases from CodBiaMaps */
	for (auto& [Sat, data] : codeBiasMap)
	for (auto& [code, bias] : data.obsCodeBiasMap)
	{
		if (availSat.find(Sat) == availSat.end())
		{
			availSat[Sat] = sysNsat[Sat.sys];
			sysNsat[Sat.sys]++;

			if (epochTime1s<0)
			{
				epochTime1s		= data.ssrMeta.epochTime1s;
			}
		}

		if (availBias[Sat.sys].find(code) == availBias[Sat.sys].end())
		{
			availBias[Sat.sys][code] = sysNbias[Sat.sys];
			sysNbias[Sat.sys]++;
		}
	}

	for (auto& [Sat,data] : phaseBiasMap)
	for (auto& [code,bia] : data.obsCodeBiasMap)
	{
		if (availSat.find(Sat) == availSat.end())
		{
			availSat[Sat] = sysNsat[Sat.sys]++;
			if (epochTime1s<0)
			{
				epochTime1s = data.ssrMeta.epochTime1s;
			}
		}

		if (availBias[Sat.sys].find(code) == availBias[Sat.sys].end())
			availBias[Sat.sys][code] = sysNbias[Sat.sys]++;
	}

	for (auto& [regID, regData]	: ssrAtm.atmosRegionsMap)
	for (auto& [sat, stecData]	: regData.stecData)
	{
		if (availSat.find(sat) == availSat.end())
		{
			availSat[sat] = sysNsat[sat.sys]++;
			if (epochTime1s<0)
			{
				epochTime1s		= ssrAtm.ssrMeta.epochTime1s;
			}
		}
	}

	int nSat_all = 0;
	int nSig_all = 0;
	int bitLen   = 49;

	for (auto& [Sat, ind] : availSat)
		ind = -1;

	for (auto& [sys, nsat] : sysNsat)
	{
		sysNbias[sys] = 0;
		nsat = 0;
		bitLen += 61;
		for (auto& [code,ind] : availBias[sys])
			ind=-1;

		for (auto& [ind,code] : cmpSSRIndex2Code[sys])
		if (availBias[sys].find(code) != availBias[sys].end())
		{
			availBias[sys][code] = sysNbias[sys];
			currentBiasMap[sys][sysNbias[sys]++] = code;
		}

		for (int s = 1; s < 41; s++)
		{
			SatSys Sat;
			Sat.sys = sys;
			Sat.prn = s;
			if (availSat.find(Sat) != availSat.end())
			{
				currentSatMap[nSat_all++] = Sat;
				availSat[Sat] = nsat++;
			}
			else
				continue;

			if (acsConfig.ssrOpts.cmpssr_cell_mask)
			{
				bitLen += sysNbias[sys];
				for (auto& [ind,code] : currentBiasMap[sys])
				{
					bool noCodes =	(  codeBiasMap						.find(Sat)	== codeBiasMap						.end()
									|| codeBiasMap[Sat].obsCodeBiasMap	.find(code)	== codeBiasMap[Sat].obsCodeBiasMap	.end());

					bool noPhase =	(  phaseBiasMap						.find(Sat)	== phaseBiasMap						.end()
									|| phaseBiasMap[Sat].obsCodeBiasMap	.find(code)	== phaseBiasMap[Sat].obsCodeBiasMap	.end());

					if (noCodes && noPhase)
						continue;

					currentCellMap[nSig_all] = {Sat, code};
					nSig_all++;
				}
			}
		}
	}

	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	if (++currentIOD > 15)
		currentIOD = 0;

	int i = 0;
	i = setbituInc(buf,i,12,	4073);
	i = setbituInc(buf,i,4,		1);
	i = setbituInc(buf,i,20,	epochTime1s);
	i = setbituInc(buf,i,4,		updateIntIndex);
	i = setbituInc(buf,i,1,		0);
	i = setbituInc(buf,i,4,		currentIOD);
	i = setbituInc(buf,i,4,		sysNsat.size());

	for (auto& [sys,nbia] : sysNsat)
	{
		i = setbituInc(buf,i,4,		compactSSRSysMap[sys]);
		int isys = i;

		for (auto& [sat,ind] : availSat)
		if (sat.sys == sys)
			setbituInc(buf,isys+sat.prn,1,1);
		isys += 40;

		for (auto& [ind,code] : cmpSSRIndex2Code[sys])
		if (availBias[sys].find(code) != availBias[sys].end())
			setbituInc(buf,isys+ind,1,1);
		isys += 16;

		i=isys;
		if (!acsConfig.ssrOpts.cmpssr_cell_mask)
			i = setbituInc(buf,i,1,0);
		else
		{
			i = setbituInc(buf,i,1,1);
			isys = i;

			for (auto& [sat,satInd] : availSat)
			{
				if (satInd	<	0)					continue;
				if (sat.sys	!=	sys)				continue;

				for (auto& [code, codInd] : availBias[sys])
				{
					if (codInd < 0)
						continue;

					int ind = satInd * nbia + codInd;
					setbituInc(buf,isys+ind,1,1);
				}
			}
			i = isys + nbia * sysNsat[sys];
		}
	}

	return buffer;
}

vector<uint8_t>  encodecompactORB(
	map<SatSys, SSROut>&		orbClkMap,
	int							updateIntIndex,
	bool						last)
{
	vector<uint8_t> buffer;

	int bitLen   = 37;
	for (auto& [ind, sat] : currentSatMap)
	{
		bitLen +=49;
		if (sat.sys == +E_Sys::GAL)
			bitLen +=2;
	}
	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	auto& [Sat, ssrOut1]	= *orbClkMap.begin();
	auto& ssrEph			= ssrOut1.ssrEph;
	auto& ssrMeta			= ssrEph.ssrMeta;
	int timeOfHour			= ssrMeta.epochTime1s % 3600;
	int multipleMessage 	= last?0:1;

	int i = 0;
	i = setbituInc(buf,i,12, 4073);
	i = setbituInc(buf,i,4,	 2);
	i = setbituInc(buf,i,12, timeOfHour);
	i = setbituInc(buf,i,4,	 updateIntIndex);
	i = setbituInc(buf,i,1,	 multipleMessage);
	i = setbituInc(buf,i,4,	 currentIOD);

	for (auto& [ind,sat] : currentSatMap)
	{
		int iode = 0;
		int dRad = -16384;
		int dAlg = - 4096;
		int dCrs = - 4096;

		if (orbClkMap.find(sat) != orbClkMap.end())
		{
			iode = orbClkMap[sat].ssrEph.iode;
			if (iode == currentSatIodes[sat])
			{
				dRad = (int)round(orbClkMap[sat].ssrEph.deph[0]*625   );
				dAlg = (int)round(orbClkMap[sat].ssrEph.deph[1]*156.25);
				dCrs = (int)round(orbClkMap[sat].ssrEph.deph[2]*156.25);

				if (abs(dRad) > 16383
				|| abs(dRad) >  4095
				|| abs(dRad) >  4095)
				{
					dRad = -16384;
					dAlg = - 4096;
					dCrs = - 4096;
				}
			}
		}

		int ns = 8;
		if (sat.sys == +E_Sys::GAL)
			ns=10;

		i = setbituInc(buf,i,ns, iode);
		i = setbitsInc(buf,i,15, dRad);
		i = setbitsInc(buf,i,13, dAlg);
		i = setbitsInc(buf,i,13, dCrs);
	}

	return buffer;
}

vector<uint8_t>  encodecompactCLK(
	map<SatSys, SSROut>&		orbClkMap,
	int							updateIntIndex,
	bool						last)
{
	vector<uint8_t> buffer;

	int bitLen   = 37 + 15*currentSatMap.size();
	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	auto& [Sat, ssrOut1]	= *orbClkMap.begin();
	auto& ssrEph			= ssrOut1.ssrEph;
	auto& ssrMeta			= ssrEph.ssrMeta;
	int timeOfHour			= ssrMeta.epochTime1s % 3600;
	int multipleMessage 	= last?0:1;

	int i = 0;
	i = setbituInc(buf,i,12, 4073);
	i = setbituInc(buf,i,4,	 3);
	i = setbituInc(buf,i,12, timeOfHour);
	i = setbituInc(buf,i,4,	 updateIntIndex);
	i = setbituInc(buf,i,1,	 multipleMessage);
	i = setbituInc(buf,i,4,	 currentIOD);

	for (auto& [ind,sat] : currentSatMap)
	{
		int dClk = -16384;

		if (orbClkMap.find(sat) != orbClkMap.end()
		&& orbClkMap[sat].ssrEph.iode == currentSatIodes[sat])
		{
			dClk = (int)round(orbClkMap[sat].ssrClk.dclk[0]*625   );
			if (abs(dClk) > 16383)
				dClk = -16384;
		}

		i = setbitsInc(buf,i,15, dClk);
	}
	return buffer;
}

vector<uint8_t>  encodecompactURA(
	map<SatSys, SSROut>&		orbClkMap,
	int							updateIntIndex,
	bool						last)
{
	vector<uint8_t> buffer;

	/* not implemented yet... */
	/*

	int bitLen   = 37 + 6*currentSatMap.size();
	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	auto& [Sat, ssrOut1]	= *orbClkMap.begin();
	auto& ssrEph			= ssrOut1.ssrEph;
	auto& ssrMeta			= ssrEph.ssrMeta;
	int timeOfHour			= ssrMeta.epochTime1s % 3600;
	int multipleMessage 	= last?0:1;

	int i = 0;
	i = setbituInc(buf,i,12, 4073);
	i = setbituInc(buf,i,4,	 7);
	i = setbituInc(buf,i,12, timeOfHour);
	i = setbituInc(buf,i,4,	 updateIntIndex);
	i = setbituInc(buf,i,1,	 multipleMessage);
	i = setbituInc(buf,i,4,	 currentIOD);

	for (auto& [ind,sat] : currentSatMap)
	{
		double ura = xxxx;
		int uraClass = uraToClassValue(ura);
		i = setbituInc(buf,i, 6, uraClass);
	}
	*/
	return buffer;
}

/**
	Encode hybrid orbit and clock message
	warning: Only global mode supported
	warning: Requires both orbit and clocks use subtype 2 and 3 for orbit-only or clock-only messages
*/
vector<uint8_t>  encodecompactCMB(
	map<SatSys, SSROut>&		orbClkMap,
	int							updateIntIndex,
	bool						last)
{
	vector<uint8_t> buffer;

	int bitLen   = 40;
	for (auto& [ind,sat] : currentSatMap)
	{
		bitLen +=64;
		if (sat.sys == +E_Sys::GAL)
			bitLen +=2;
	}
	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	auto& [Sat, ssrOut1]	= *orbClkMap.begin();
	auto& ssrEph			= ssrOut1.ssrEph;
	auto& ssrMeta			= ssrEph.ssrMeta;
	int timeOfHour			= ssrMeta.epochTime1s % 3600;
	int multipleMessage 	= last?0:1;

	int i = 0;
	i = setbituInc(buf,i,12, 4073);
	i = setbituInc(buf,i,4,	 11);
	i = setbituInc(buf,i,12, timeOfHour);
	i = setbituInc(buf,i,4,	 updateIntIndex);
	i = setbituInc(buf,i,1,	 multipleMessage);
	i = setbituInc(buf,i,4,	 currentIOD);
	i = setbituInc(buf,i,1,	 1);				// Orbit corrections required
	i = setbituInc(buf,i,1,	 1);				// Clock corrections required
	i = setbituInc(buf,i,1,	 0);				// Regional corrections not supported

	for (auto& [ind,sat] : currentSatMap)
	{
		int iode = 0;
		int dRad = -16384;
		int dAlg = - 4096;
		int dCrs = - 4096;
		int dClk = -16384;

		if (orbClkMap.find(sat) != orbClkMap.end())
		{
			iode = orbClkMap[sat].ssrEph.iode;
			if (iode == currentSatIodes[sat])
			{
				dRad = (int)round(orbClkMap[sat].ssrEph.deph[0]*625   );
				dAlg = (int)round(orbClkMap[sat].ssrEph.deph[1]*156.25);
				dCrs = (int)round(orbClkMap[sat].ssrEph.deph[2]*156.25);
				dClk = (int)round(orbClkMap[sat].ssrClk.dclk[0]*625   );

				if (abs(dRad) > 16383
				|| abs(dRad) >  4095
				|| abs(dRad) >  4095
				|| abs(dClk) > 16383)
				{
					dRad = -16384;
					dAlg = - 4096;
					dCrs = - 4096;
					dClk = -16384;
				}
			}
		}

		int ns = 8;
		if (sat.sys == +E_Sys::GAL)
			ns=10;

		i = setbituInc(buf,i,ns, iode);
		i = setbitsInc(buf,i,15, dRad);
		i = setbitsInc(buf,i,13, dAlg);
		i = setbitsInc(buf,i,13, dCrs);
		i = setbitsInc(buf,i,15, dClk);
	}

	return buffer;
}

vector<uint8_t>  encodecompactCOD(
	map<SatSys, SSRCodeBias>&	codeBiasMap,
	int							updateIntIndex,
	bool						last)
{
	vector<uint8_t> buffer;

	int bitLen  = 37;
	if (acsConfig.ssrOpts.cmpssr_cell_mask)
		bitLen += 11*currentCellMap.size();
	else
	for (auto& [ind,sat] : currentSatMap)
	{
		bitLen += 11*currentBiasMap[sat.sys].size();
	}
	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	auto& [Sat, ssrCod]		= *codeBiasMap.begin();
	auto& ssrMeta			= ssrCod.ssrMeta;
	int timeOfHour			= ssrMeta.epochTime1s % 3600;
	int multipleMessage 	= last?0:1;

	int i = 0;
	i = setbituInc(buf,i,12, 4073);
	i = setbituInc(buf,i,4,	 4);
	i = setbituInc(buf,i,12, timeOfHour);
	i = setbituInc(buf,i,4,	 updateIntIndex);
	i = setbituInc(buf,i,1,	 multipleMessage);
	i = setbituInc(buf,i,4,	 currentIOD);

	if (acsConfig.ssrOpts.cmpssr_cell_mask)
	{
		for (auto& [ind, sig] : currentCellMap)
		{
			auto& [sat, code] = sig;

			int cBias = -1024;
			if (codeBiasMap.find(sat) != codeBiasMap.end()
			&& codeBiasMap[sat].obsCodeBiasMap.find(code) != codeBiasMap[sat].obsCodeBiasMap.end())
				cBias = (int)round(codeBiasMap[sat].obsCodeBiasMap[code].bias * 50);

			if (abs(cBias)>1023)
				cBias = -1024;

			i = setbitsInc(buf,i,11, cBias);
		}
		return buffer;
	}

	for (auto& [ind,sat] : currentSatMap)
	{
		for (auto& [ind2, code] : currentBiasMap[sat.sys])
		{
			int cBias = -1024;
			if (codeBiasMap.find(sat) != codeBiasMap.end()
			&& codeBiasMap[sat].obsCodeBiasMap.find(code) != codeBiasMap[sat].obsCodeBiasMap.end())
				cBias = (int)round(codeBiasMap[sat].obsCodeBiasMap[code].bias * 50);

			if (abs(cBias)>1023)
				cBias = -1024;

			i = setbitsInc(buf,i,11, cBias);
		}
	}

	return buffer;
}

int phaseBiasDiscChk(
	map<E_ObsCode, BiasVar> biasMap,
	SatSys					sat,
	E_ObsCode				code)
{
	double bias = 0;
	bool found	= false;
	E_FType	frq	= code2Freq[sat.sys][code];
	
	if (biasMap.find(code) != biasMap.end())
	{
		bias = biasMap[code].bias;
		found = true;
	}
	else
	{
		E_ObsCode 	hax = freq2CodeHax(sat.sys,frq);
		if (biasMap.find(hax) != biasMap.end())
		{
			bias = biasMap[hax].bias;
			found = true;
		}
	}
	
	if (!found)
		return -16384;
	
	int pBias = (int)round(1000*(bias-phaseBiasOffset[sat][code]));
	
	if (abs(pBias)>16383)
	{
		double lamb = genericWavelength[frq];
		phaseBiasOffset[sat][code] = lamb*round(bias/lamb);
		pBias = (int)round(1000*(bias-phaseBiasOffset[sat][code]));
		if(++phaseDiscCountr[sat][code] > 3)
			phaseDiscCountr[sat][code]=0;
	}
	
	return pBias;
}

vector<uint8_t>  encodecompactPHS(
	map<SatSys, SSRPhasBias>&	phaseBiasMap,
	int							updateIntIndex,
	bool						last)
{
	vector<uint8_t> buffer;

	int bitLen  = 37;
	if (acsConfig.ssrOpts.cmpssr_cell_mask)
		bitLen += 17*currentCellMap.size();
	else
	for (auto& [ind,sat] : currentSatMap)
	{
		bitLen += 17*currentBiasMap[sat.sys].size();
	}
	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	auto& [Sat, ssrPhs]		= *phaseBiasMap.begin();
	auto& ssrMeta			= ssrPhs.ssrMeta;
	int timeOfHour			= ssrMeta.epochTime1s % 3600;
	int multipleMessage 	= last?0:1;

	int i = 0;
	i = setbituInc(buf,i,12, 4073);
	i = setbituInc(buf,i,4,	 5);
	i = setbituInc(buf,i,12, timeOfHour);
	i = setbituInc(buf,i,4,	 updateIntIndex);
	i = setbituInc(buf,i,1,	 multipleMessage);
	i = setbituInc(buf,i,4,	 currentIOD);

	if (acsConfig.ssrOpts.cmpssr_cell_mask)
	{
		for (auto& [ind, sig] : currentCellMap)
		{
			auto& [sat, code] = sig;

			int pBias = -16384;
			if (phaseBiasMap.find(sat) != phaseBiasMap.end())
				pBias = phaseBiasDiscChk(phaseBiasMap[sat].obsCodeBiasMap, sat, code);

			i = setbitsInc(buf,i,15, pBias);
			i = setbitsInc(buf,i,2,  phaseDiscCountr[sat][code]);
		}
		return buffer;
	}

	for (auto& [ind,sat] : currentSatMap)
	for (auto& [ind2, code] : currentBiasMap[sat.sys])
	{
		int pBias = -16384;
		if (phaseBiasMap.find(sat) != phaseBiasMap.end())
			pBias = phaseBiasDiscChk(phaseBiasMap[sat].obsCodeBiasMap, sat, code);
		
		i = setbitsInc(buf,i,15, pBias);
		i = setbitsInc(buf,i,2,  phaseDiscCountr[sat][code]);
	}

	return buffer;
}

/**
	Encode hybrid code and phase bias message
	warning 1: Currently only global mode supported
*/
vector<uint8_t>  encodecompactBIA(
	map<SatSys, SSRCodeBias>&	codeBiasMap,
	map<SatSys, SSRPhasBias>&	phaseBiasMap,
	int							updateIntIndex,
	bool						last)
{
	vector<uint8_t> buffer;

	int bitLen  = 40;

	int inclCodBia = codeBiasMap.empty()?0:1;		// add config mask here?
	int inclPhsBia = phaseBiasMap.empty()?0:1;		// add config mask here?

	int nbit = 11*inclCodBia + 17*inclPhsBia;
	if (nbit == 0)
		return buffer;

	if (acsConfig.ssrOpts.cmpssr_cell_mask)
		bitLen += nbit*currentCellMap.size();

	for (auto& [ind, sat] : currentSatMap)
	{
		bitLen += nbit*currentBiasMap[sat.sys].size();
	}

	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	int timeOfHour=0;
	if (inclCodBia>0)
	{
		auto& [Sat, ssrCod]	= *codeBiasMap.begin();
		auto& ssrMeta		= ssrCod.ssrMeta;
		timeOfHour			= ssrMeta.epochTime1s % 3600;
	}
	else if (inclPhsBia>0)
	{
		auto& [Sat, ssrPhs]	= *phaseBiasMap.begin();
		auto& ssrMeta		= ssrPhs.ssrMeta;
		timeOfHour			= ssrMeta.epochTime1s % 3600;
	}

	int multipleMessage 	= last?0:1;

	int i = 0;
	i = setbituInc(buf,i,12, 4073);
	i = setbituInc(buf,i,4,	 6);
	i = setbituInc(buf,i,12, timeOfHour);
	i = setbituInc(buf,i,4,	 updateIntIndex);
	i = setbituInc(buf,i,1,	 multipleMessage);
	i = setbituInc(buf,i,4,	 currentIOD);

	i = setbituInc(buf,i,1,	 inclCodBia);
	i = setbituInc(buf,i,1,	 inclPhsBia);
	i = setbituInc(buf,i,1,	 0);		// Todo: Regional bias correction not supported at the moment

	if (acsConfig.ssrOpts.cmpssr_cell_mask)
	{
		for (auto& [ind,sig] : currentCellMap)
		{
			auto& [sat, code] = sig;

			if (inclCodBia>0)
			{
				int cBias = -1024;
				if (codeBiasMap.find(sat) != codeBiasMap.end()
				&& codeBiasMap[sat].obsCodeBiasMap.find(code) != codeBiasMap[sat].obsCodeBiasMap.end())
					cBias = (int)round(codeBiasMap[sat].obsCodeBiasMap[code].bias * 50);

				if (abs(cBias)>1023)
					cBias = -1024;

				i = setbitsInc(buf,i,11, cBias);
			}

			if (inclPhsBia>0)
			{
				int pBias = -16384;
				if (phaseBiasMap.find(sat) != phaseBiasMap.end())
					pBias = phaseBiasDiscChk(phaseBiasMap[sat].obsCodeBiasMap, sat, code);
				i = setbitsInc(buf,i,15, pBias);
				i = setbitsInc(buf,i,2,  phaseDiscCountr[sat][code]);
			}
		}
		return buffer;
	}

	for (auto& [ind,sat] : currentSatMap)
	{
		for (auto& [ind2, code] : currentBiasMap[sat.sys])
		{
			if (inclCodBia>0)
			{
				int cBias = -1024;
				if (codeBiasMap.find(sat) != codeBiasMap.end()
				&& codeBiasMap[sat].obsCodeBiasMap.find(code) != codeBiasMap[sat].obsCodeBiasMap.end())
					cBias = (int)round(codeBiasMap[sat].obsCodeBiasMap[code].bias * 50);

				if (abs(cBias)>1023)
					cBias = -1024;

				i = setbitsInc(buf,i,11, cBias);
			}

			if (inclPhsBia>0)
			{
				int pBias = -16384;
				if (phaseBiasMap.find(sat) != phaseBiasMap.end())
					pBias = phaseBiasDiscChk(phaseBiasMap[sat].obsCodeBiasMap, sat, code);
				
				i = setbitsInc(buf,i,15, pBias);
				i = setbitsInc(buf,i,2,  phaseDiscCountr[sat][code]);
			}
		}
	}

	return buffer;
}

map<int,map<int,double>> stecPolyCommonMode;
vector<uint8_t>  encodecompactTEC(
	SSRMeta&					ssrMeta,
	int							regId,
	SSRAtmRegion&				ssrAtmReg,
	int							updateIntIndex,
	bool						last)
{
	vector<uint8_t> buffer;

	if (ssrAtmReg.ionoPolySize<=0)
		return buffer;

	int nBitTec  = 0;
	int polyType = -1;
	switch(ssrAtmReg.ionoPolySize)
	{
		case 1:	polyType = 0; nBitTec = 20; break;
		case 3:	polyType = 1; nBitTec = 44; break;
		case 4:	polyType = 2; nBitTec = 54; break;
		case 6:	polyType = 3; nBitTec = 70; break;
	}

	if (nBitTec ==0)
		return buffer;

	int bitLen   = 44 + currentSatMap.size();

	map<int,SatSys>	regSat;
	for (auto& [ind,sat] : currentSatMap)
	if (ssrAtmReg.stecData.find(sat) != ssrAtmReg.stecData.end())
	{
		regSat[ind]=sat;
		bitLen+=nBitTec;
	}
	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	int timeOfHour			= ssrMeta.epochTime1s % 3600;
	int multipleMessage 	= last?0:1;

	int i = 0;
	i = setbituInc(buf,i,12, 4073);
	i = setbituInc(buf,i,4,	 3);
	i = setbituInc(buf,i,12, timeOfHour);
	i = setbituInc(buf,i,4,	 updateIntIndex);
	i = setbituInc(buf,i,1,	 multipleMessage);
	i = setbituInc(buf,i,4,	 currentIOD);
	i = setbituInc(buf,i,2,	 polyType);
	i = setbituInc(buf,i,5,	 regId);

	int isys =i;
	for (auto& [ind,sat] : regSat)
		setbituInc(buf,isys+ind,1,1);
	i = isys + currentSatMap.size();
	
	// Remove common mode
	for (int iPoly = 0; iPoly<ssrAtmReg.ionoPolySize; iPoly++)
	{
		double valPoly	= 0;
		double nsamp	= 0; 
		for (auto& [ind,sat] : regSat)
		{
			auto& [time, stecData]	= *ssrAtmReg.stecData[sat].begin();
			double val = stecData.poly[iPoly];
			if (val<-9990)
				continue;
			valPoly += val;
			nsamp	+= 1.0;
		}
		if (nsamp==0)
			continue;
			
		valPoly/=nsamp;
		
		if (stecPolyCommonMode[regId].find(iPoly) == stecPolyCommonMode[regId].end())
			stecPolyCommonMode[regId][iPoly] = valPoly;
		
		if (stecPolyCommonMode[regId].find(iPoly) == stecPolyCommonMode[regId].end())
			stecPolyCommonMode[regId][iPoly] += 0.01*(valPoly-stecPolyCommonMode[regId][iPoly]);
	}
	
	for (auto& [ind,sat] : regSat)
	{
		auto& [time, stecData]	= *ssrAtmReg.stecData[sat].begin();

		int uraClass = uraToClassValue(stecData.sigma);
		i = setbituInc(buf,i, 6, uraClass);

		bool inRange = true;
		int C00 = -8192, C00_tmp = 0;
		int C01 = -2048, C01_tmp = 0;
		int C10 = -2048, C10_tmp = 0;
		int C11 = - 512, C11_tmp = 0;
		int C02 = - 128, C02_tmp = 0;
		int C20 = - 128, C20_tmp = 0;

		switch(polyType)
		{
			case 3:	C02_tmp = (int)round( (stecData.poly[4]-stecPolyCommonMode[regId][4]) * 200); 	if (abs(C02_tmp) >  127) {inRange = false; break;}
					C20_tmp = (int)round( (stecData.poly[5]-stecPolyCommonMode[regId][5]) * 200); 	if (abs(C20_tmp) >  127) {inRange = false; break;}
			case 2:	C11_tmp = (int)round( (stecData.poly[3]-stecPolyCommonMode[regId][3]) *  50); 	if (abs(C11_tmp) >  511) {inRange = false; break;}
			case 1:	C01_tmp = (int)round( (stecData.poly[1]-stecPolyCommonMode[regId][1]) *  50); 	if (abs(C01_tmp) > 2047) {inRange = false; break;}
					C10_tmp = (int)round( (stecData.poly[2]-stecPolyCommonMode[regId][2]) *  50); 	if (abs(C10_tmp) > 2047) {inRange = false; break;}
			case 0:	C00_tmp = (int)round( (stecData.poly[0]-stecPolyCommonMode[regId][0]) *  20); 	if (abs(C00_tmp) > 8191) {inRange = false; break;}
		}

		if (inRange)
		{
			C00 = C00_tmp;
			C10 = C10_tmp;
			C01 = C01_tmp;
			C11 = C11_tmp;
			C20 = C20_tmp;
			C02 = C02_tmp;
		}

		i = setbitsInc(buf,i, 14, C00);
		if (polyType==0)
			continue;
		i = setbitsInc(buf,i, 12, C01);
		i = setbitsInc(buf,i, 12, C10);
		if (polyType==1)
			continue;
		i = setbitsInc(buf,i, 10, C11);
		if (polyType==2)
			continue;
		i = setbitsInc(buf,i,  8, C02);
		i = setbitsInc(buf,i,  8, C20);
	}

	return buffer;
}

vector<uint8_t>  encodecompactGRD(
	SSRMeta&					ssrMeta,
	int							regId,
	SSRAtmRegion&				ssrAtmReg,
	int							updateIntIndex,
	bool						last)
{
	vector<uint8_t> buffer;

	if (ssrAtmReg.ionoGrid
	&& ssrAtmReg.tropGrid)
		return buffer;

	int bitLen   = 57 + currentSatMap.size();

	int nGrid = ssrAtmReg.gridLatDeg.size();

	int tropType	= ssrAtmReg.tropGrid ? 1	: 0;
	int nBitTrop	= ssrAtmReg.tropGrid ? 17	: 0;
	double offsetDry = 2.3;
	double offsetWet = 0.252;
	bitLen+= nGrid*nBitTrop;

	int ionoType	= acsConfig.ssrOpts.cmpssr_stec_format < 4 ? 0 : 1;
	int nBitIono	= acsConfig.ssrOpts.cmpssr_stec_format < 4 ? 7 : 16;
	map<int,SatSys>	regSat;
	for (auto& [ind, sat] : currentSatMap)
	if (ssrAtmReg.stecData.find(sat) != ssrAtmReg.stecData.end())
	{
		regSat[ind] = sat;
		bitLen += nGrid * nBitIono;
	}

	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	int timeOfHour			= ssrMeta.epochTime1s % 3600;
	int multipleMessage 	= last?0:1;


	int i = 0;
	i = setbituInc(buf,i,12, 4073);
	i = setbituInc(buf,i,4,	 9);
	i = setbituInc(buf,i,12, timeOfHour);
	i = setbituInc(buf,i,4,	 updateIntIndex);
	i = setbituInc(buf,i,1,	 multipleMessage);
	i = setbituInc(buf,i,4,	 currentIOD);
	i = setbituInc(buf,i,2,	 tropType);
	i = setbituInc(buf,i,1,	 ionoType);
	i = setbituInc(buf,i,5,	 regId);

	int isys =i;
	for (auto& [ind,sat] : regSat)
		setbituInc(buf,isys+ind,1,1);
	i = isys + currentSatMap.size();

	auto& [tim1, tropData]	= *ssrAtmReg.tropData.begin();
	int uraClass = uraToClassValue(tropData.sigma);
	i = setbituInc(buf,i, 6, uraClass);
	i = setbituInc(buf,i, 6, nGrid);

	for (auto& [iGrid, latDeg] : ssrAtmReg.gridLatDeg)
	{
		if (tropType==1)
		{
			int dTDry = (int)round((tropData.gridDry[iGrid]-offsetDry)*250);
			if (abs(dTDry)>255)
				dTDry=-256;
			int dTWet = (int)round((tropData.gridWet[iGrid]-offsetWet)*250);
			if (abs(dTWet)>127)
				dTWet=-128;
			i = setbituInc(buf,i, 9, dTDry);
			i = setbituInc(buf,i, 8, dTWet);
		}

		int stecMax = 32767;
		if (acsConfig.ssrOpts.cmpssr_stec_format>0)
			stecMax = 63;

		for (auto& [iSat,sat] : regSat)
		{
			auto& [tim2, stecData]	= *ssrAtmReg.stecData[sat].begin();
			int dSTEC = (int)round(stecData.grid[iGrid]*25);
			if (abs(dSTEC)>stecMax)
				dSTEC=-stecMax-1;
			i = setbituInc(buf,i,nBitIono, dSTEC);
		}
	}
	return buffer;
}

vector<uint8_t>  encodecompactATM(
	SSRMeta&				ssrMeta,
	int						regId,
	SSRAtmRegion&			ssrAtmReg,
	int						updateIntIndex,
	bool					last)
{
	vector<uint8_t> buffer;


	int tropType = 0;
	int nBitTrpGrid = 0;
	if (ssrAtmReg.tropGrid)
	{
		tropType++;
		nBitTrpGrid = acsConfig.ssrOpts.cmpssr_trop_format==0?6:8;
	}
	int nBitTrpPoly = 0;
	int tropPoly = -1;
	switch (ssrAtmReg.tropPolySize)
	{
		case 1: tropPoly =0; nBitTrpPoly =  9; tropType+=2; break;
		case 3: tropPoly =1; nBitTrpPoly = 23; tropType+=2; break;
		case 4: tropPoly =2; nBitTrpPoly = 30; tropType+=2; break;
	}

	int ionoType = 0;
	int nBitIonGrid = 0;
	double ionGridLSB = 0;
	if (ssrAtmReg.ionoGrid)
	{
		ionoType++;
		switch (acsConfig.ssrOpts.cmpssr_stec_format)
		{
			case 0:	nBitIonGrid = 4; ionGridLSB = 0.04; break;
			case 1:	nBitIonGrid = 4; ionGridLSB = 0.12; break;
			case 2:	nBitIonGrid = 5; ionGridLSB = 0.16; break;
			case 3:	nBitIonGrid = 7; ionGridLSB = 0.24; break;
		}
	}
	int nBitIonPoly = 0;
	int ionoPoly = -1;
	switch (ssrAtmReg.ionoPolySize)
	{
		case 1: ionoPoly =0; nBitIonPoly = 14; ionoType+=2; break;
		case 3: ionoPoly =1; nBitIonPoly = 38; ionoType+=2; break;
		case 4: ionoPoly =2; nBitIonPoly = 48; ionoType+=2; break;
		case 6: ionoPoly =3; nBitIonPoly = 64; ionoType+=2; break;
	}


	if (tropType == 0
	&& ionoType == 0)
		return buffer;

	int nGrid = ssrAtmReg.gridLatDeg.size();

	int bitLen  = 52;
	if (tropType > 0)
	{
		bitLen += 6;
		if (nBitTrpPoly>0)
			bitLen += 2 + nBitTrpPoly;
		if (nBitTrpGrid>0)
			bitLen += 1 + nGrid*nBitTrpGrid;
	}

	map<int,SatSys>	regSat;
	if (ionoType>0)
	{
		bitLen += currentSatMap.size();

		for (auto& [ind,sat] : currentSatMap)
		if (ssrAtmReg.stecData.find(sat) != ssrAtmReg.stecData.end())
		{
			regSat[ind]=sat;
			bitLen+= 6;
			if (nBitIonPoly>0)
				bitLen+= 2 + nBitIonPoly;
			if (nBitIonGrid>0)
				bitLen+= 2 + nGrid*nBitIonGrid;
		}
	}
	int byteLen = ceil(bitLen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();


	int timeOfHour			= ssrMeta.epochTime1s % 3600;
	int multipleMessage 	= last?0:1;

	int i = 0;
	i = setbituInc(buf,i,12, 4073);
	i = setbituInc(buf,i,4,	 12);
	i = setbituInc(buf,i,12, timeOfHour);
	i = setbituInc(buf,i,4,	 updateIntIndex);
	i = setbituInc(buf,i,1,	 multipleMessage);
	i = setbituInc(buf,i,4,	 currentIOD);
	i = setbituInc(buf,i,2,	 tropType);
	i = setbituInc(buf,i,2,	 ionoType);
	i = setbituInc(buf,i,5,	 regId);
	i = setbituInc(buf,i,6,	 nGrid);

	if (tropType > 0)
	{
		auto& [tim1, tropData]	= *ssrAtmReg.tropData.begin();
		int uraClass = uraToClassValue(tropData.sigma);
		i = setbituInc(buf,i, 6, uraClass);

		if (nBitTrpPoly>0)
		{
			bool inRange = true;
			int T00 = -256, T00_tmp = 0;
			int T01 = - 64, T01_tmp = 0;
			int T10 = - 64, T10_tmp = 0;
			int T11 = - 64, T11_tmp = 0;

			switch(tropPoly)
			{
				case 2:	T11_tmp = (int)round( tropData.polyDry[3] *1000); 	if (abs(T11_tmp) > 63 ) {inRange = false; break;}
				case 1:	T01_tmp = (int)round( tropData.polyDry[1] * 500); 	if (abs(T01_tmp) > 63 ) {inRange = false; break;}
						T10_tmp = (int)round( tropData.polyDry[2] * 500); 	if (abs(T10_tmp) > 63 ) {inRange = false; break;}
				case 0:	T00_tmp = (int)round( tropData.polyDry[0] * 250); 	if (abs(T00_tmp) > 255) {inRange = false; break;}
			}

			if (inRange)
			{
				T00 = T00_tmp;
				T10 = T10_tmp;
				T01 = T01_tmp;
				T11 = T11_tmp;
			}

			i = setbitsInc(buf,i, 9, T00);
			if (tropPoly>0)
			{
				i = setbitsInc(buf,i, 12, T01);
				i = setbitsInc(buf,i, 12, T10);
			}
			if (tropPoly>1)
				i = setbitsInc(buf,i, 10, T11);
		}

		if (nBitTrpGrid > 0)
		{
			double	acumGrid 	= 0;
			int		nOkGrid		= 0;
			for ( auto& [ind, zwd] : tropData.gridWet)
			if (zwd>-9999)
			{
				acumGrid += zwd;
				nOkGrid++;
			}

			int intZWDOffset = (int)round(acumGrid * 50 / nOkGrid);
			double offsetWet = 0.02 * intZWDOffset;
			int tropGridFormat = acsConfig.ssrOpts.cmpssr_trop_format;

			i = setbituInc(buf,i, 1, tropGridFormat);
			i = setbituInc(buf,i, 4, intZWDOffset);

			for ( auto& [ind, lat] : ssrAtmReg.gridLatDeg)
			{
				int maxZWD = pow(2, nBitTrpGrid - 1) - 1;
				int preZWD = -maxZWD - 1;

				if (tropData.gridWet.find(ind) != tropData.gridWet.end())
					preZWD = (int)round((tropData.gridWet[ind] - offsetWet) * 250);

				if (abs(preZWD)>maxZWD)
					preZWD = -maxZWD - 1;

				i = setbitsInc(buf,i, nBitTrpGrid, preZWD);
			}
		}
	}

	if (ionoType)
	{
		for (auto& [j, sat] : regSat)
			setbituInc(buf, i+j, 1, 1);

		i += currentSatMap.size();

		for (auto& [j,sat] : regSat)
		{
			auto& [time, stecData]	= *ssrAtmReg.stecData[sat].begin();

			int uraClass = uraToClassValue(stecData.sigma);
			i = setbituInc(buf,i, 6, uraClass);

			if (nBitIonPoly>0)
			{
				i = setbituInc(buf,i, 2, ionoPoly);

				bool inRange = true;
				int C00 = -8192, C00_tmp = 0;
				int C01 = -2048, C01_tmp = 0;
				int C10 = -2048, C10_tmp = 0;
				int C11 = - 512, C11_tmp = 0;
				int C02 = - 128, C02_tmp = 0;
				int C20 = - 128, C20_tmp = 0;

				switch(ionoPoly)
				{
					case 3:	C02_tmp = (int)round( stecData.poly[4] * 200); 	if (abs(C02_tmp) >  127) {inRange = false; break;}
							C20_tmp = (int)round( stecData.poly[5] * 200); 	if (abs(C20_tmp) >  127) {inRange = false; break;}
					case 2:	C11_tmp = (int)round( stecData.poly[3] *  50); 	if (abs(C11_tmp) >  511) {inRange = false; break;}
					case 1:	C01_tmp = (int)round( stecData.poly[1] *  50); 	if (abs(C01_tmp) > 2047) {inRange = false; break;}
							C10_tmp = (int)round( stecData.poly[2] *  50); 	if (abs(C10_tmp) > 2047) {inRange = false; break;}
					case 0:	C00_tmp = (int)round( stecData.poly[0] *  20); 	if (abs(C00_tmp) > 8191) {inRange = false; break;}
				}

				if (inRange)
				{
					C00 = C00_tmp;
					C10 = C10_tmp;
					C01 = C01_tmp;
					C11 = C11_tmp;
					C20 = C20_tmp;
					C02 = C02_tmp;
				}

				i = setbitsInc(buf,i, 14, C00);
				if (ionoPoly==0)
					continue;
				i = setbitsInc(buf,i, 12, C01);
				i = setbitsInc(buf,i, 12, C10);
				if (ionoPoly==1)
					continue;
				i = setbitsInc(buf,i, 10, C11);
				if (ionoPoly==2)
					continue;
				i = setbitsInc(buf,i,  8, C02);
				i = setbitsInc(buf,i,  8, C20);
			}

			if (nBitIonGrid>0)
			for (auto& [ind, lat] : ssrAtmReg.gridLatDeg)
			{
				int maxSTEC = pow(2,nBitIonGrid-1)-1;
				int preSTEC = -maxSTEC - 1;

				if (stecData.grid.find(ind) != stecData.grid.end())
					preSTEC = (int)round(stecData.grid[ind] / ionGridLSB);

				if (abs(preSTEC) > maxSTEC)
					preSTEC = -maxSTEC - 1;

				i = setbitsInc(buf,i, nBitIonGrid, preSTEC);
			}
		}
	}
	return buffer;
}

vector<uint8_t> encodeGridInfo(
	SSRAtm&						ssrAtm)
{
	vector<uint8_t> buffer;

	int bitlen = 17;
	for (auto& [regID,regData] : ssrAtm.atmosRegionsMap)
	{
		bitlen+=39;
		switch (regData.gridType)
		{
			case 0: bitlen+= 6 + 21 * regData.gridLatDeg.size(); break;
			case 1: bitlen+= 31; break;
			case 2: bitlen+= 31; break;
			default:
				std::cout << "Unknown gridtype for region: " << regID << "\n";
				regData.gridType = -1;
				continue;
		}
	}

	int byteLen = ceil(bitlen/8.0);
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();

	int i=0;
	i = setbituInc(buf,i, 4, 3);
	int regIOD = 0;
	auto& [regID,regData] = *ssrAtm.atmosRegionsMap.begin();

	i = setbituInc(buf,i, 3, regIOD);
	i = setbituInc(buf,i, 4, 0);
	i = setbituInc(buf,i, 6, ssrAtm.atmosRegionsMap.size()-1);

	for (auto& [regID,regData] : ssrAtm.atmosRegionsMap)
	{
		if (regData.gridType<0)
			continue;
		i = setbituInc(buf,i, 5, regID);
		i = setbituInc(buf,i, 1, 0);					/** Warning: network/region parts are not enabled */
		int tmp = 0;
		switch (regData.gridType)
		{
			case 0:
			{
				double thisLatDeg = regData.gridLatDeg[0];
				double thisLonDeg = regData.gridLonDeg[0];
				tmp =(int) round(regData.gridLatDeg[0]*16284/PI * D2R);		i = setbitsInc(buf,i,15, tmp);		//todo aaron, check scaling facotr
				tmp =(int) round(regData.gridLonDeg[0]*16284/PI * D2R);		i = setbitsInc(buf,i,16, tmp);
				tmp = regData.gridLatDeg.size();							i = setbituInc(buf,i, 6, tmp);

				for (auto& [ind, lat] : regData.gridLatDeg)
				{
					double dLat = regData.gridLatDeg[ind] - thisLatDeg;
					tmp =(int) round(dLat * 100 * D2R);						i = setbitsInc(buf,i,10, tmp);
					thisLatDeg += 0.01 * tmp * R2D;

					double dLon = regData.gridLonDeg[ind] - thisLonDeg;
					tmp =(int) round(dLon * 100 * D2R);						i = setbitsInc(buf,i,11, tmp);
					thisLonDeg += 0.01 * tmp * R2D;
				}
				break;
			}
			case 1:
			case 2:
			{
				if	(  regData.intLatDeg <= 0
					|| regData.intLonDeg <= 0)
				{
					continue;
				}

				int nGridLat = ceil((regData.maxLatDeg - regData.minLatDeg) / regData.intLatDeg);
				int nGridLon = ceil((regData.maxLonDeg - regData.minLonDeg) / regData.intLonDeg);
				tmp =(int) round(regData.gridLatDeg[0]*D2R*16284/PI);	i = setbitsInc(buf,i,15, tmp);
				tmp =(int) round(regData.gridLonDeg[0]*D2R*16284/PI);	i = setbitsInc(buf,i,16, tmp);
																		i = setbituInc(buf,i, 6, nGridLat);
																		i = setbituInc(buf,i, 6, nGridLon);
				tmp =(int) round(regData.intLatDeg*D2R*100);			i = setbituInc(buf,i, 9, tmp);
				tmp =(int) round(regData.intLonDeg*D2R*100);			i = setbituInc(buf,i,10, tmp);
				break;
			}
			default:
			{
				vector<uint8_t> dummy;
				return dummy;
			}
		}
	}

	return buffer;
}

vector<uint8_t>  encodecompactSRV(
	SSRAtm&						ssrAtm)
{
	/* Send archived messages */
	if (cmpSSRServiceMessages.empty() == false)
	{
		auto [ind, message]	= *cmpSSRServiceMessages.begin();

		vector<uint8_t> buffer;

		buffer.resize(message.size());

		copy(message.begin(), message.end(), buffer.begin());

		cmpSSRServiceMessages.erase(ind);

		return buffer;
	}

	/* service packets to be implemented here */
	for (auto& [regID,regData] : ssrAtm.atmosRegionsMap)
	{
		vector<uint8_t> buffer = encodeGridInfo(ssrAtm);

		int buffSize = buffer.size();
		int ibuff=0;
		int indMess=0;
		while (ibuff < buffSize)
		{
			int nBlocks = ceil(0.2*(buffSize-ibuff));

			if (nBlocks > 4)
				nBlocks = 4;

			int bitLen	= 22 + 40*nBlocks;
			int byteLen	= ceil(bitLen / 8.0);

			vector<uint8_t> message;
			message.resize(byteLen);

			unsigned char* buf = message.data();

			int multipleMessage = 1;

			if ((buffSize - ibuff) <= 20)
				multipleMessage = 0;

			int i=0;
			i = setbituInc(buf,i,12, 4073);
			i = setbituInc(buf,i,4,	 10);
			i = setbituInc(buf,i,1,  multipleMessage);
			i = setbituInc(buf,i,3,	 indMess);
			i = setbituInc(buf,i,2,	 nBlocks);

			for (int ibyte = 0; ibyte<(5*nBlocks) && ibuff<buffSize; ibyte++)
				i = setbituInc(buf,i,8,	 buffer[ibuff++]);

			cmpSSRServiceMessages[indMess++] = message;
		}
	}

	if (cmpSSRServiceMessages.empty() == false)
	{
		auto& [ind, message] = *cmpSSRServiceMessages.begin();

		vector<uint8_t> quickout;

		quickout.resize(message.size());

		copy(message.begin(), message.end(), quickout.begin());

		cmpSSRServiceMessages.erase(ind);

		return quickout;
	}

	vector<uint8_t> dummy;
	return dummy;
}
