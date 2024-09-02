
#include "rtcmDecoder.hpp"
#include "ionoModel.hpp"
#include "otherSSR.hpp"

#define CMPSSRTRCLVL 2

struct PhaseDiscControl
{
	double  bias		= 0;
	int 	disc		= -1;
	int		intlevel	= 0;
};

map<int, map<SatSys, map<E_ObsCode, PhaseDiscControl>>>	compactSsrPhaseDisc;

map<int, map<int, SatSys>>								compactSsrSatelliteIndex;
map<int, map<int, pair<SatSys, E_ObsCode>>>				compactSsrSignalIndex;
map<int, map<int, SatSys>>								compactSsrIonoIndex;

map<int, map<SatSys, SSROut>>							compactSsrStorage;
SSRAtm													compactSsrAtmStorage;

vector<unsigned char>									compactSsrServiceMessage;

bool ssrAtmUpdated = false;

int lastServiceMessage = -1;

map<E_Sys, map<int, E_ObsCode>> compactSSRIndex2Code
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
			{11, E_ObsCode::L8X},
			{12, E_ObsCode::L6B},
			{13, E_ObsCode::L6C},
			{14, E_ObsCode::L6X}
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
			{ 9, E_ObsCode::L5X},
			{10, E_ObsCode::L6S},
			{11, E_ObsCode::L6E},
			{12, E_ObsCode::L6X}
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

int compactSSRIod = -1;
GTime compactSsrMaskTime;
GTime compactSsrLastTime;

double decodeCmpSsrField(
	vector<unsigned char>&	data,
	int&					i,
	int						bitLen,
	double					scale,
	double					offset)
{
	int tmp = getbitsInc(data, i,bitLen);

	if (tmp == -pow(2,bitLen-1))
		return SSR_UNAVAILABLE;

	return scale * tmp + offset;
}

double checkDisc(
	SatSys		Sat,
	E_ObsCode	code,
	double		bias,
	int			disc,
	int 		regionID = -1)
{
	bool newBias = false;

	if		(compactSsrPhaseDisc.find(regionID)				== compactSsrPhaseDisc.end())						newBias = true;
	else if (compactSsrPhaseDisc[regionID].find(Sat)		== compactSsrPhaseDisc[regionID].end())				newBias = true;
	else if (compactSsrPhaseDisc[regionID][Sat].find(code)	== compactSsrPhaseDisc[regionID][Sat].end())		newBias = true;

	auto& storedDisc = compactSsrPhaseDisc[regionID][Sat][code];

	if (newBias)
	{
		storedDisc.bias = bias;
		storedDisc.disc = disc;
		storedDisc.intlevel = 0;
		return bias;
	}

	double lam = genericWavelength[code2Freq[Sat.sys][code]];

	if (storedDisc.disc != disc)
	{
		int dlam = (int) round((bias - storedDisc.bias) / lam);
		storedDisc.intlevel += dlam;
		storedDisc.disc = disc;
	}

	storedDisc.bias = bias;

	return bias + lam * storedDisc.intlevel;
}

void copySSRBlock (
	SatSys Sat,
	SSROut ssrBlock)
{
	auto& ssr = nav.satNavMap[Sat].receivedSSR;

	if	(ssrBlock.ephUpdated)
	{
		GTime tEph = ssrBlock.ssrEph.ssrMeta.receivedTime;
		if	(ssr.ssrEph_map.find(tEph) == ssr.ssrEph_map.end())
		{
			tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n#CMP_SSR ORBITS %s %s %4d  %2d %10.4f %10.4f %10.4f",
						Sat.id().c_str(),
						tEph.to_string().c_str(),
						ssrBlock.ssrEph.iode,
						ssrBlock.ssrEph.iod,
						ssrBlock.ssrEph.deph[0],
						ssrBlock.ssrEph.deph[1],
						ssrBlock.ssrEph.deph[2]);

			if	(  ssrBlock.ssrEph.deph[0] == SSR_UNAVAILABLE
				|| ssrBlock.ssrEph.deph[1] == SSR_UNAVAILABLE
				|| ssrBlock.ssrEph.deph[2] == SSR_UNAVAILABLE)
			{
				ssr.ssrEph_map.clear();
			}
			else
				ssr.ssrEph_map[tEph] = ssrBlock.ssrEph;

			ssrBlock.ephUpdated = false;
		}
	}

	if	(ssrBlock.clkUpdated)
	{
		GTime tClk = ssrBlock.ssrClk.ssrMeta.receivedTime;
		if	(ssr.ssrClk_map.find(tClk) == ssr.ssrClk_map.end())
		{
			tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n#CMP_SSR CLOCKS %s %s       %2d %10.4f",
							Sat.id().c_str(),
							tClk.to_string().c_str(),
							ssrBlock.ssrClk.iod,
							ssrBlock.ssrClk.dclk[0]);

			if (fabs(ssrBlock.ssrClk.dclk[0]) > 9000)
				ssr.ssrClk_map.clear();
			else
				ssr.ssrClk_map[tClk] = ssrBlock.ssrClk;

			ssrBlock.clkUpdated = false;
		}
	}
	if (ssrBlock.codeUpdated)
	{
		BiasEntry	entry;
		string		id  = Sat.id() + ":" + Sat.sysChar();
		entry.measType	= CODE;
		entry.Sat		= Sat;
		entry.tini		= ssrBlock.ssrCodeBias.t0 - 0.5*ssrBlock.ssrCodeBias.udi;
		entry.tfin		= entry.tini + acsConfig.ssrInOpts.code_bias_valid_time;
		entry.source	= "ssr";

		tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n#CMP_SSR CODBIA %s %s: ", Sat.id().c_str(),entry.tini.to_string().c_str());

		for (auto& [code,biasSSR] : ssrBlock.ssrCodeBias.obsCodeBiasMap)
		{
			entry.cod1	= code;
			entry.cod2	= E_ObsCode::NONE;
			entry.bias	= -biasSSR.bias;
			entry.var	=  0;
			entry.slop	=  0;
			entry.slpv	=  0;

			pushBiasEntry(id, entry);
			tracepdeex(CMPSSRTRCLVL+1,std::cout, "%s %9.4f; ", code._to_string(), biasSSR.bias);
		}

		if  ( ssr.ssrCodeBias_map.find(entry.tini) == ssr.ssrCodeBias_map.end())
		{
			ssr.ssrCodeBias_map[entry.tini] = ssrBlock.ssrCodeBias;
		}
		ssrBlock.codeUpdated = false;
	}

	if (ssrBlock.phaseUpdated)
	{
		BiasEntry	entry;
		string		id  = Sat.id() + ":" + Sat.sysChar();
		entry.measType	= PHAS;
		entry.Sat		= Sat;
		entry.tini		= ssrBlock.ssrPhasBias.t0 - 0.5*ssrBlock.ssrPhasBias.udi;
		entry.tfin		= entry.tini + acsConfig.ssrInOpts.phase_bias_valid_time;
		entry.source	= "ssr";

		tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n#CMP_SSR PHSBIA %s %s: ", Sat.id().c_str(),entry.tini.to_string().c_str());

		for(auto& [code,biasSSR] : ssrBlock.ssrPhasBias.obsCodeBiasMap)
		{
			entry.cod1	= code;
			entry.cod2	= E_ObsCode::NONE;
			entry.bias	= -biasSSR.bias;
			entry.var	=  0;
			entry.slop	=  0;
			entry.slpv	=  0;

			tracepdeex(CMPSSRTRCLVL+1,std::cout, "%s %9.4f; ", code._to_string(), biasSSR.bias);
			pushBiasEntry(id, entry);
		}

		if  ( ssr.ssrPhasBias_map.find(entry.tini) == ssr.ssrPhasBias_map.end())
		{
			ssr.ssrPhasBias_map[entry.tini] = ssrBlock.ssrPhasBias;
		}
		ssrBlock.phaseUpdated = false;
	}

	if	(ssrBlock.uraUpdated)
	if  ( ssr.ssrUra_map.find(ssrBlock.ssrUra.t0) == ssr.ssrUra_map.end() )
	{
		ssr.ssrUra_map[ssrBlock.ssrUra.t0] = ssrBlock.ssrUra;

		tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n#CMP_SSR URA    %s %s  %.4f",
			Sat.id().c_str(),
			ssrBlock.ssrUra.t0.to_string().c_str(),
			uraSsr[ssrBlock.ssrUra.ura] / 1000);

		ssrBlock.uraUpdated = false;
	}
}

void copySSRCorrections(
	int regID)
{
	if (regID == -1
	 || acsConfig.ssrOpts.region_id == regID)
	for (auto& [Sat, ssrBlock] : compactSsrStorage[regID])
		copySSRBlock (Sat, ssrBlock);

	if (ssrAtmUpdated)
	{
		GTime tAtm = compactSsrAtmStorage.ssrMeta.receivedTime;
		for (auto& [regInd,regData] : compactSsrAtmStorage.atmosRegionsMap)
		{
			auto& navAtm = nav.ssrAtm.atmosRegionsMap[regInd];

			if (regData.regionDefIOD > 0
			 && navAtm.regionDefIOD != regData.regionDefIOD)
			{
				navAtm.regionDefIOD = regData.regionDefIOD;
				navAtm.minLatDeg	= regData.minLatDeg;
				navAtm.maxLatDeg	= regData.maxLatDeg;
				navAtm.intLatDeg	= regData.intLatDeg;
				navAtm.minLonDeg	= regData.minLonDeg;
				navAtm.maxLonDeg	= regData.maxLonDeg;
				navAtm.intLonDeg	= regData.intLonDeg;
				navAtm.gridType		= regData.gridType;
				navAtm.ionoGrid		= regData.ionoGrid;
				navAtm.tropGrid		= regData.tropGrid;
				navAtm.tropPolySize = regData.tropPolySize;
				navAtm.ionoPolySize = regData.ionoPolySize;
				navAtm.gridLatDeg.clear();
				navAtm.gridLonDeg.clear();

				for (auto& [ind, latDeg] : regData.gridLatDeg)
				{
					navAtm.gridLatDeg[ind] = latDeg;
					navAtm.gridLonDeg[ind] = regData.gridLonDeg[ind];
				}
			}

			if (regData.tropData.find(tAtm) != regData.tropData.end())
			{
				navAtm.tropData[tAtm] = regData.tropData[tAtm];
				if (regInd == acsConfig.ssrOpts.region_id)
				{
																			tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n#CMP_SSR TRP     %s  %.4f\n    Poly:",	tAtm.to_string().c_str(), regData.tropData[tAtm].sigma);
					for (auto& [ind,val] : regData.tropData[tAtm].polyDry)	tracepdeex(CMPSSRTRCLVL+1,std::cout, " %.4f", val);
																			tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n    Grid:");
					for (auto& [ind,val] : regData.tropData[tAtm].gridWet)	tracepdeex(CMPSSRTRCLVL+1,std::cout, " %.4f", val);
																			tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n");
				}
			}

			for (auto& [Sat, stecMap] : regData.stecData)
			if (stecMap.find(tAtm) != stecMap.end())
			{
				navAtm.stecData[Sat][tAtm] = stecMap[tAtm];
				if (regInd == acsConfig.ssrOpts.region_id)
				{
																			tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n#CMP_SSR ION %s %s  %.4f\n    Poly:",Sat.id().c_str(), tAtm.to_string().c_str(), stecMap[tAtm].sigma);
					for (auto& [ind,val] : stecMap[tAtm].poly)				tracepdeex(CMPSSRTRCLVL+1,std::cout, " %.4f", val);
																			tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n    Grid:");
					for (auto& [ind,val] : stecMap[tAtm].grid)				tracepdeex(CMPSSRTRCLVL+1,std::cout, " %.4f", val);
																			tracepdeex(CMPSSRTRCLVL+1,std::cout, "\n");
				}

			}
		}
		ssrAtmUpdated = false;
	}

	compactSsrStorage.clear();
}

GTime toh2time(
	GTime close,
	double toh)
{
	double tow = GTow(close);
	double dtoh = toh - (tow - 3600.0 * floor(tow/3600));

	if		(dtoh < -1800)	dtoh += 3600;
	else if	(dtoh >  1800)	dtoh -= 3600;

	return close + dtoh;
}

void decodeGridInfo(
	vector<unsigned char>&	data)
{
	int i=4;
	int servIOD		= getbituInc(data,i,3);
	int areaIOD		= getbituInc(data,i,4);
	int numNet		= getbituInc(data,i,6)+1;

	for (int n = 0; n < numNet; n++)
	{
		int		netw_ID		= getbituInc(data,i,5);
		bool	part		= getbituInc(data,i,1);
		int		partID		= netw_ID;
		if (part)
			partID			+= 32*getbituInc(data,i,4);
		int		gridType	= getbituInc(data,i,2);

		SSRAtmRegion& atmRegion = nav.ssrAtm.atmosRegionsMap[partID];

		if (atmRegion.regionDefIOD != servIOD)
		{
			atmRegion.gridLatDeg	.clear();
			atmRegion.gridLonDeg	.clear();
			atmRegion.tropData		.clear();
			atmRegion.stecData		.clear();

			atmRegion.regionDefIOD = servIOD;
		}
		double radscale = PI * P2_15;
		switch (gridType)
		{
			case 0:
			{
				double thisLatDeg		= getbitsInc(data,i,15) * radscale * R2D;
				double thisLonDeg		= getbitsInc(data,i,16) * radscale * R2D;

				atmRegion.gridLatDeg[0]= thisLatDeg;
				atmRegion.gridLonDeg[0]= thisLonDeg;
				int ngrid			= getbituInc(data,i, 6);

				atmRegion.minLatDeg = thisLatDeg - 0.1;
				atmRegion.minLonDeg = thisLonDeg - 0.1;
				atmRegion.maxLatDeg = thisLatDeg + 0.1;
				atmRegion.maxLonDeg = thisLonDeg + 0.1;

				for (int grd = 0; grd < ngrid; grd++)
				{
					thisLatDeg += getbitsInc(data,i,10)*0.01 * R2D;
					thisLatDeg += getbitsInc(data,i,11)*0.01 * R2D;

					if (atmRegion.minLatDeg > (thisLatDeg))		atmRegion.minLatDeg = thisLatDeg;
					if (atmRegion.maxLatDeg < (thisLatDeg))		atmRegion.maxLatDeg = thisLatDeg;
					if (atmRegion.minLonDeg > (thisLonDeg))		atmRegion.minLonDeg = thisLonDeg;
					if (atmRegion.maxLonDeg < (thisLonDeg))		atmRegion.maxLonDeg = thisLonDeg;

					atmRegion.gridLatDeg[grd] = thisLatDeg < -180 ? (thisLatDeg + 360) : (thisLatDeg > 180 ? (thisLatDeg - 360) : thisLatDeg);
					atmRegion.gridLonDeg[grd] = thisLonDeg < -180 ? (thisLonDeg + 360) : (thisLonDeg > 180 ? (thisLonDeg - 360) : thisLonDeg);
				}
				double dLat = atmRegion.maxLatDeg - atmRegion.minLatDeg;
				double dLon = atmRegion.maxLonDeg - atmRegion.minLonDeg;

				double nLat = sqrt(dLat * ngrid / dLon);
				double nLon = ngrid / nLat;

				atmRegion.intLatDeg = dLat / nLat;
				atmRegion.intLonDeg = dLon / nLon;

				break;
			}
			case 1:
			{
				atmRegion.gridLatDeg[0]		= getbitsInc(data,i,15) * radscale * R2D;
				atmRegion.gridLonDeg[0]		= getbitsInc(data,i,16) * radscale * R2D;
				int ngridLat				= getbituInc(data,i, 6);
				int ngridLon				= getbituInc(data,i, 6);


				atmRegion.intLatDeg			= getbituInc(data,i, 9) * 0.01 * R2D;
				atmRegion.intLonDeg			= getbituInc(data,i,10) * 0.01 * R2D;
				atmRegion.minLatDeg			= atmRegion.gridLatDeg[0] - ngridLat * atmRegion.intLatDeg;
				atmRegion.maxLatDeg			= atmRegion.gridLatDeg[0];
				atmRegion.minLonDeg			= atmRegion.gridLonDeg[0];
				atmRegion.maxLatDeg			= atmRegion.gridLonDeg[0] + ngridLon * atmRegion.intLonDeg;

				for (int nlat = 0, grd = 0; nlat < ngridLat; nlat++)
				for (int nlon = 0;          nlon < ngridLon; nlon++)
				{
					atmRegion.gridLatDeg[grd]	= atmRegion.gridLatDeg[0] - nlat * atmRegion.intLatDeg;
					atmRegion.gridLonDeg[grd]	= atmRegion.gridLonDeg[0] + nlon * atmRegion.intLonDeg;
					grd++;
				}
				break;
			}
			case 2:
			{
				atmRegion.gridLatDeg[0]		= getbitsInc(data,i,15) * radscale * R2D;
				atmRegion.gridLonDeg[0]		= getbitsInc(data,i,16) * radscale * R2D;
				int ngridLat				= getbituInc(data,i, 6);
				int ngridLon				= getbituInc(data,i, 6);

				atmRegion.intLatDeg			= getbituInc(data,i, 9) * 0.01 * R2D;
				atmRegion.intLonDeg			= getbituInc(data,i,10) * 0.01 * R2D;
				atmRegion.minLatDeg			= atmRegion.gridLatDeg[0];
				atmRegion.maxLatDeg			= atmRegion.gridLatDeg[0] + ngridLat * atmRegion.intLatDeg;
				atmRegion.minLonDeg			= atmRegion.gridLonDeg[0];
				atmRegion.maxLatDeg			= atmRegion.gridLonDeg[0] + ngridLon * atmRegion.intLonDeg;

				for (int nlon = 0, grd = 0; nlon < ngridLon; nlon++)
				for (int nlat = 0;          nlat < ngridLat; nlat++)
				{
					atmRegion.gridLatDeg[grd]	= atmRegion.gridLatDeg[0] + nlat * atmRegion.intLatDeg;
					atmRegion.gridLonDeg[grd]	= atmRegion.gridLonDeg[0] + nlon * atmRegion.intLonDeg;
					grd++;
				}
				break;
			}
		}

	}
}

void processServiceData(
	vector<unsigned char>&	data)
{
	int srvMessageType = getbitu(data,0,4);
	switch (srvMessageType)
	{
		case 3:	decodeGridInfo(data);		break;
		default: tracepdeex(4, std::cout,"Unsupported compact SSR service message\n");
	}
}


int decodeSSR_header(
	vector<unsigned char>&	data,
	SSRMeta&		ssrMeta,
	bool			mask)
{
	int i = 16;
	if (mask)
		ssrMeta.epochTime1s		= getbituInc(data,i,20);
	else
	{
		ssrMeta.epochTime1s		= getbituInc(data,i,12);
		ssrMeta.receivedTime	= toh2time(compactSsrMaskTime, ssrMeta.epochTime1s);
	}
	ssrMeta.updateIntIndex		= getbituInc(data,i, 4);
	ssrMeta.multipleMessage		= getbituInc(data,i, 1);
	int ssrIod					= getbituInc(data,i, 4);

	if (mask)
		compactSSRIod		= ssrIod;
	else if (ssrIod != compactSSRIod)
		return E_ReturnType::BAD_LENGTH;

	return i;
}

/**
 * Compact SSR message type 1: satellite/code mask
 */
int decodeSSR_mask(
	vector<unsigned char>&	data,
	GTime					now)
{
	compactSsrStorage		.clear();
	compactSsrSatelliteIndex.clear();
	compactSsrSignalIndex	.clear();

	for (auto& [regId, regData] : compactSsrAtmStorage.atmosRegionsMap)
	{
		regData.tropData.clear();
		regData.stecData.clear();
	}

	int bitLen = data.size() * 8;


	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, true);

	compactSsrLastTime = GTime(GTow(ssrMeta.epochTime1s), now);

	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	compactSsrMaskTime = compactSsrLastTime;

	int nsys = getbituInc(data,i,4);

	for (int s = 0, satind = 0, cellind = 0; s < nsys; s++)
	{
		if ((i + 61) > bitLen)
		{
			compactSSRIod = -1;
			return E_ReturnType::BAD_LENGTH;
		}

		int sysID = getbituInc(data,i,4);
		E_Sys sys;
		switch (sysID)
		{
			case 0: sys = E_Sys::GPS; break;
			case 1: sys = E_Sys::GLO; break;
			case 2: sys = E_Sys::GAL; break;
			case 3: sys = E_Sys::BDS; break;
			case 4: sys = E_Sys::QZS; break;
			case 5: sys = E_Sys::SBS; break;
			default:
				tracepdeex(2, std::cout,"Warning unsupported GNSS for compact SSR\n");
				compactSSRIod = -1;
				return 0;
		}

		int nsat = 0;
		map<int,SatSys> sysSatIndex;
		for (int n=1; n<41; n++)
		{
			SatSys Sat(sys,n);
			if (getbituInc(data,i,1)==1)
			{
				compactSsrSatelliteIndex[-1][satind++] = Sat;
				sysSatIndex[nsat++] = Sat;
			}
		}

		int ncod = 0;
		map<int,E_ObsCode> sysCodeIndex;
		for (int n=0; n<16; n++)
		{
			if  (getbituInc(data,i,1) == 1
				&& compactSSRIndex2Code[sys].find(n) != compactSSRIndex2Code[sys].end())
			{
				sysCodeIndex[ncod++] = compactSSRIndex2Code[sys][n];
			}
		}

		bool cellField = false;
		if (getbituInc(data,i,1) == 1)
		{
			if (nsat*ncod >128)
			{
				tracepdeex(2, std::cout,"Warning cell size for compact SSR execeds maximum size\n");
				compactSSRIod = -1;
				return 0;
			}
			cellField = true;
			if ((i+nsat*ncod) > bitLen)
				return E_ReturnType::BAD_LENGTH;
		}

		for (auto& [sind,Sat] : sysSatIndex)
		for (auto& [cind,code] : sysCodeIndex)
		{
			if	(  cellField
				&& getbituInc(data,i,1)==0)
			{
				continue;
			}

			compactSsrSignalIndex[-1][cellind].first	= Sat;
			compactSsrSignalIndex[-1][cellind].second	= code;
			cellind++;
		}
	}

	tracepdeex(CMPSSRTRCLVL, std::cout, "\n#CMPSSR_MSK %s  iod: %2d  nsat: %2d  nsig: %2d ",
					compactSsrMaskTime.to_string().c_str(),
					compactSSRIod,
					compactSsrSatelliteIndex[-1].size(),
					compactSsrSignalIndex[-1].size());

	return i;
}

/**
 * Compact SSR message type 2: global satellite orbits
 */
int decodeSSR_orbit(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return 0;
	int bitLen = data.size()*8;

	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);

	if (ssrMeta.receivedTime > compactSsrLastTime)
		compactSsrLastTime = ssrMeta.receivedTime;

	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	if (i < 0)
	{
		tracepdeex(4, std::cout,"Orbit message inconsistent with mask IOD\n");
		return 0;
	}

	SSREph ssrEph;
	ssrEph.ssrMeta	= ssrMeta;
	ssrEph.t0		= ssrMeta.receivedTime;
	ssrEph.udi		= ssrUdi[ssrMeta.updateIntIndex];
	if (ssrEph.udi > 1)
		ssrEph.t0 += 0.5*ssrEph.udi;

	ssrEph.iod		= compactSSRIod;

	for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
	{
		int ni = (Sat.sys == +E_Sys::GAL)?10:8;
		if ((i+ni+41) > bitLen)
			return E_ReturnType::BAD_LENGTH;

		ssrEph.iode 	= getbituInc(data, i, ni);
		ssrEph.deph[0]	= decodeCmpSsrField (data,i,15,0.0016,0);
		ssrEph.deph[1]	= decodeCmpSsrField (data,i,13,0.0064,0);
		ssrEph.deph[2]	= decodeCmpSsrField (data,i,13,0.0064,0);

		compactSsrStorage[-1][Sat].ssrEph		= ssrEph;
		compactSsrStorage[-1][Sat].ephUpdated	= true;
	}

	tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_ORB %s  iod: %2d  nsat: %2d           udi: %2d", ssrMeta.receivedTime.to_string().c_str(), compactSSRIod, compactSsrSatelliteIndex[-1].size(), ssrEph.udi);

	if (ssrMeta.multipleMessage == 0)
		copySSRCorrections(-1);

	return i;
}

/**
 * Compact SSR message type 3: global satellite clock offsets
 */
int decodeSSR_clock(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return 0;
	int bitLen = data.size()*8;

	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);

	if (ssrMeta.receivedTime > compactSsrLastTime)
		compactSsrLastTime = ssrMeta.receivedTime;		//todo aaron, this is all copy-paste in every function

	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	if (i<0)
	{
		tracepdeex(4, std::cout,"Clock message inconsistent with mask IOD\n");
		return 0;
	}

	SSRClk ssrClk;
	ssrClk.ssrMeta	= ssrMeta;
	ssrClk.t0		= ssrMeta.receivedTime;
	ssrClk.udi		= ssrUdi[ssrMeta.updateIntIndex];
	if (ssrClk.udi > 1)
		ssrClk.t0 += 0.5*ssrClk.udi;
	ssrClk.iod		= compactSSRIod;

	for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
	{
		if ((i+15) > bitLen)
			return E_ReturnType::BAD_LENGTH;

		ssrClk.dclk[0]	= decodeCmpSsrField (data,i,15,0.0016,0);

		compactSsrStorage[-1][Sat].ssrClk		= ssrClk;
		compactSsrStorage[-1][Sat].clkUpdated	= true;
	}

	tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_CLK %s  iod: %2d  nsat: %2d            udi: %2d", ssrMeta.receivedTime.to_string().c_str(), compactSSRIod, compactSsrSatelliteIndex[-1].size(), ssrClk.udi);

	if (ssrMeta.multipleMessage == 0)
		copySSRCorrections(-1);

	return i;
}

/**
 * Compact SSR message type 11: regional satellite orbits and clock offsets
 */
int decodeSSR_combined(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return 0;

	int bitLen = data.size() * 8;

	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);

	if (ssrMeta.receivedTime > compactSsrLastTime)
		compactSsrLastTime = ssrMeta.receivedTime;

	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	if (i<0)
	{
		tracepdeex(4, std::cout,"Orbit/Clock message inconsistent with mask IOD\n");
		return 0;
	}

	if ((i+3) > bitLen)
		return E_ReturnType::BAD_LENGTH;

	bool orbitAvailable = getbituInc(data,i,1);
	bool clockAvailable = getbituInc(data,i,1);
	int  regionID = -1;
	if (getbituInc(data,i,1))
	{
		if ((i+5+compactSsrSatelliteIndex[-1].size()) > bitLen)
			return E_ReturnType::BAD_LENGTH;

		regionID	= getbituInc(data,i,5);
		int n=0;
		for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
		if (getbituInc(data,i,1)==1)
			compactSsrSatelliteIndex[regionID][n++] = Sat;
	}

	SSREph ssrEph;
	ssrEph.ssrMeta	= ssrMeta;
	ssrEph.t0		= ssrMeta.receivedTime;
	ssrEph.udi		= ssrUdi[ssrMeta.updateIntIndex];
	if (ssrEph.udi > 1)
		ssrEph.t0 += 0.5*ssrEph.udi;
	ssrEph.iod		= compactSSRIod;

	SSRClk ssrClk;
	ssrClk.ssrMeta	= ssrMeta;
	ssrClk.t0		= ssrEph.t0;
	ssrClk.udi		= ssrEph.udi;
	ssrClk.iod		= compactSSRIod;

	for (auto& [indx, Sat] : compactSsrSatelliteIndex[regionID])
	{
		int ni = (Sat.sys == +E_Sys::GAL)?10:8;

		if (orbitAvailable)
		{
			if ((i+ni+41)> bitLen)
				return E_ReturnType::BAD_LENGTH;

			ssrEph.iode 	= getbituInc(data, i, ni);
			ssrEph.deph[0]	= decodeCmpSsrField (data,i,15,0.0016,0);
			ssrEph.deph[1]	= decodeCmpSsrField (data,i,13,0.0064,0);
			ssrEph.deph[2]	= decodeCmpSsrField (data,i,13,0.0064,0);

			compactSsrStorage[regionID][Sat].ssrEph		= ssrEph;
			compactSsrStorage[regionID][Sat].ephUpdated	= true;
		}
		if (clockAvailable)
		{
			if ((i+15) > bitLen)
				return E_ReturnType::BAD_LENGTH;

			ssrClk.dclk[0]	= decodeCmpSsrField (data,i,15,0.0016,0);

			compactSsrStorage[regionID][Sat].ssrClk		= ssrClk;
			compactSsrStorage[regionID][Sat].clkUpdated	= true;
		}
	}

	if (regionID == -1)
		tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_CMB %s %s %s global   , nsat:%d", ssrMeta.receivedTime.to_string().c_str(), orbitAvailable?"orb":"   ", clockAvailable?"clk":"   ",           compactSsrSatelliteIndex[regionID].size());
	else
		tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_CMB %s %s %s region %2d, nsat:%d",ssrMeta.receivedTime.to_string().c_str(), orbitAvailable?"orb":"   ", clockAvailable?"clk":"   ", regionID, compactSsrSatelliteIndex[regionID].size());

	if (ssrMeta.multipleMessage == 0)
		copySSRCorrections(regionID);

	return i;
}

/**
 * Compact SSR message type 4: global satellite code biases
*/
int decodeSSR_code_bias(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return 0;
	int bitLen = data.size()*8;

	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);

	if (ssrMeta.receivedTime > compactSsrLastTime)
		compactSsrLastTime = ssrMeta.receivedTime;

	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	if (i<0)
	{
		tracepdeex(4, std::cout,"Code bias message inconsistent with mask IOD\n");
		return 0;
	}

	SSRCodeBias ssrCodeBias;
	ssrCodeBias.ssrMeta	= ssrMeta;
	ssrCodeBias.t0		= ssrMeta.receivedTime;
	ssrCodeBias.udi		= ssrUdi[ssrMeta.updateIntIndex];
	if (ssrCodeBias.udi > 1)
		ssrCodeBias.t0 += 0.5*ssrCodeBias.udi;
	ssrCodeBias.iod 	= compactSSRIod;

	for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
		compactSsrStorage[-1][Sat].ssrCodeBias	= ssrCodeBias;

	for (auto& [indx, SatnCode] : compactSsrSignalIndex[-1])
	{
		SatSys Sat		= SatnCode.first;
		E_ObsCode code	= SatnCode.second;

		if ((i+11) > bitLen)
			return E_ReturnType::BAD_LENGTH;

		auto& biasEntry = compactSsrStorage[-1][Sat].ssrCodeBias.obsCodeBiasMap[code];
		biasEntry.bias	= decodeCmpSsrField (data,i,11,0.02,0);
	}
	tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_COD %s iod: %2d udi: %2d nsig:%d", ssrMeta.receivedTime.to_string().c_str(), compactSSRIod, ssrCodeBias.udi, compactSsrSignalIndex[-1].size());

	for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
		compactSsrStorage[-1][Sat].codeUpdated	= true;

	if (ssrMeta.multipleMessage == 0)
		copySSRCorrections(-1);

	return i;
}

/**
 * Compact SSR message type 5: global satellite phase biases
*/
int decodeSSR_phas_bias(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return 0;
	int bitLen = data.size()*8;

	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);

	if (ssrMeta.receivedTime > compactSsrLastTime)
		compactSsrLastTime = ssrMeta.receivedTime;

	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	if (i<0)
	{
		tracepdeex(4, std::cout,"Phase bias message inconsistent with mask IOD\n");
		return 0;
	}

	SSRPhasBias ssrPhasBias;
	ssrPhasBias.ssrMeta	= ssrMeta;
	ssrPhasBias.t0		= ssrMeta.receivedTime;
	ssrPhasBias.udi		= ssrUdi[ssrMeta.updateIntIndex];
	if (ssrPhasBias.udi > 1)
		ssrPhasBias.t0 += 0.5*ssrPhasBias.udi;
	ssrPhasBias.iod 	= compactSSRIod;

	for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
		compactSsrStorage[-1][Sat].ssrPhasBias	= ssrPhasBias;


	for (auto& [indx, SatnCode] : compactSsrSignalIndex[-1])
	{
		SatSys Sat		= SatnCode.first;
		E_ObsCode code	= SatnCode.second;

		if ((i+17) > bitLen)
			return E_ReturnType::BAD_LENGTH;

		double bias 									= decodeCmpSsrField (data,i,15,0.001,0);
		int disc										= getbituInc(data, i, 2);

		auto& biasEntry 			= compactSsrStorage[-1][Sat].ssrPhasBias.obsCodeBiasMap[code];
		biasEntry.bias				= checkDisc(Sat, code, bias, disc, -1);

		auto& ssrPhaseChs			= compactSsrStorage[-1][Sat].ssrPhasBias.ssrPhaseChs[code];
		ssrPhaseChs.signalDisconCnt	= disc;
	}

	for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
		compactSsrStorage[-1][Sat].phaseUpdated	= true;

	tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_PHS %s  iod: %2d  nsig:%d  udi: %2d", ssrMeta.receivedTime.to_string().c_str(), compactSSRIod, compactSsrSignalIndex[-1].size(), ssrPhasBias.udi);

	if (ssrMeta.multipleMessage == 0)
		copySSRCorrections(-1);

	return i;
}

/**
 * Combined code and phase bias (with potential regional biases)
*/
int decodeSSR_comb_bias(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return 0;
	int bitLen = data.size()*8;

	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);

	if (ssrMeta.receivedTime > compactSsrLastTime)
		compactSsrLastTime = ssrMeta.receivedTime;

	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	if (i<0)
	{
		tracepdeex(4, std::cout,"Combined bias message inconsistent with mask IOD\n");
		return 0;
	}

	if ((i+3) > bitLen)
		return E_ReturnType::BAD_LENGTH;

	bool code_Available = getbituInc(data,i,1);
	bool phaseAvailable = getbituInc(data,i,1);
	int  regionID		= -1;
	if (getbituInc(data,i,1)==1)
	{
		if ((i+5 + compactSsrSignalIndex[-1].size()) > bitLen)
			return E_ReturnType::BAD_LENGTH;

		regionID		= getbituInc(data,i,5);
		int n=0;
		map<SatSys,int> regSats;
		for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
		if (getbituInc(data,i,1)==1)
		{
			compactSsrSatelliteIndex[regionID][n]=Sat;
			regSats[Sat] = n++;
		}
		n=0;
		for (auto& [indx, SatnCode] : compactSsrSignalIndex[-1])
		if (regSats.find(SatnCode.first) != regSats.end())
		{
			compactSsrSignalIndex[regionID][n++] = SatnCode;
		}
	}

	SSRCodeBias ssrCodeBias;
	ssrCodeBias.ssrMeta	= ssrMeta;
	ssrCodeBias.t0		= ssrMeta.receivedTime;
	ssrCodeBias.udi		= ssrUdi[ssrMeta.updateIntIndex];
	if (ssrCodeBias.udi > 1)
		ssrCodeBias.t0 += 0.5*ssrCodeBias.udi;
	ssrCodeBias.iod 	= compactSSRIod;

	SSRPhasBias ssrPhasBias;
	ssrPhasBias.ssrMeta	= ssrMeta;
	ssrPhasBias.t0		= ssrCodeBias.t0;
	ssrPhasBias.udi		= ssrCodeBias.udi;
	ssrPhasBias.iod 	= compactSSRIod;

	for (auto& [indx, Sat] : compactSsrSatelliteIndex[regionID])
	{
		if (code_Available)			compactSsrStorage[regionID][Sat].ssrCodeBias	= ssrCodeBias;
		if (phaseAvailable)			compactSsrStorage[regionID][Sat].ssrPhasBias	= ssrPhasBias;
	}

	int nCode = 0;
	int nPhase = 0;

	for (auto& [indx, SatnCode] : compactSsrSignalIndex[regionID])
	{
		SatSys Sat		= SatnCode.first;
		E_ObsCode code	= SatnCode.second;


		if (code_Available)
		{
			if ((i+11) > bitLen)
				return E_ReturnType::BAD_LENGTH;

			auto& biasEntry 		= compactSsrStorage[regionID][Sat].ssrCodeBias.obsCodeBiasMap[code];
			biasEntry.bias			= decodeCmpSsrField (data,i,11,0.02,0);
			if (regionID>=0)
				biasEntry.bias	   += compactSsrStorage[-1][Sat].ssrCodeBias.obsCodeBiasMap[code].bias;
			nCode++;
		}

		if (phaseAvailable)
		{
			if ((i+17)> bitLen)
				return E_ReturnType::BAD_LENGTH;

			double bias 				 = decodeCmpSsrField (data,i,15,0.001,0);
			int disc					 = getbituInc(data, i, 2);

			auto& biasEntry 			 = compactSsrStorage[regionID][Sat].ssrPhasBias.obsCodeBiasMap[code];
			biasEntry.bias				 = checkDisc(Sat, code, bias, disc, -1);
			if (regionID>=0)
				biasEntry.bias			+= compactSsrStorage[-1][Sat].ssrPhasBias.obsCodeBiasMap[code].bias;

			auto& ssrPhaseChs			 = compactSsrStorage[regionID][Sat].ssrPhasBias.ssrPhaseChs[code];
			ssrPhaseChs.signalDisconCnt	 = disc;

			nPhase++;
		}
	}

	if (regionID == -1)
		tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_BIA %s %s %s global   , nsig:%d", ssrMeta.receivedTime.to_string().c_str(), code_Available?"cod":"   ", phaseAvailable?"phs":"   ",           compactSsrSignalIndex[regionID].size());
	else
		tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_BIA %s %s %s region %2d, nsig:%d",ssrMeta.receivedTime.to_string().c_str(), code_Available?"cod":"   ", phaseAvailable?"phs":"   ", regionID, compactSsrSignalIndex[regionID].size());


	for (auto& [indx, Sat] : compactSsrSatelliteIndex[regionID])
	{
		if (nCode > 0) compactSsrStorage[regionID][Sat].codeUpdated	 = true;
		if (nPhase> 0) compactSsrStorage[regionID][Sat].phaseUpdated = true;
	}

	if (ssrMeta.multipleMessage == 0)
		copySSRCorrections(regionID);

	return i;
}

/**
 * User Range Accuracy
*/
int decodeSSR_URA(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return 0;

	int bitLen = data.size()*8;

	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);

	if (ssrMeta.receivedTime > compactSsrLastTime)
		compactSsrLastTime = ssrMeta.receivedTime;

	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	if (i<0)
	{
		tracepdeex(4, std::cout,"URA message inconsistent with mask IOD\n");
		return 0;
	}

	SSRUra ssrUra;
	ssrUra.t0		= ssrMeta.receivedTime;
	ssrUra.udi		= ssrUdi[ssrMeta.updateIntIndex];
	if (ssrUra.udi > 1)
		ssrUra.t0 += 0.5*ssrUra.udi;
	ssrUra.iod 		= compactSSRIod;

	for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
	{
		if ((i+6) > bitLen)
			return E_ReturnType::BAD_LENGTH;

		ssrUra.ura	= getbituInc(data, i, 6);

		compactSsrStorage[-1][Sat].ssrUra		= ssrUra;
		compactSsrStorage[-1][Sat].uraUpdated	= true;
	}

	tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_URA %s  iod: %2d  nsat: %2d  udi: %2d", ssrMeta.receivedTime.to_string().c_str(), compactSSRIod, compactSsrSatelliteIndex[-1].size(), ssrUra.udi);

	if (ssrMeta.multipleMessage == 0)
		copySSRCorrections(-1);

	return i;
}

/**
 * Compact (polynomial based) salnt TEC maps
*/
int decodeSSR_slant_TEC(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return 0;

	int bitLen = data.size() * 8;

	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);

	if (ssrMeta.receivedTime > compactSsrLastTime)
		compactSsrLastTime = ssrMeta.receivedTime;

	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	if (i<0)
	{
		tracepdeex(4, std::cout,"STEC message inconsistent with mask IOD\n");
		return 0;
	}

	if ((i+7) > bitLen)
		return E_ReturnType::BAD_LENGTH;

	compactSsrAtmStorage.ssrMeta = ssrMeta;

	int STECType		= getbituInc(data,i,2);
	int regionID		= getbituInc(data,i,5);
	int n=0;
	compactSsrIonoIndex[regionID].clear();
	for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
	if (getbituInc(data,i,1)==1)
		compactSsrIonoIndex[regionID][n++] = Sat;

	SSRAtmRegion& atmRegion = compactSsrAtmStorage.atmosRegionsMap[regionID];

	GTime tAtm		= ssrMeta.receivedTime;

	for (auto& [indx, Sat] : compactSsrIonoIndex[regionID])
	{
		if ((i+20) > bitLen)
			return E_ReturnType::BAD_LENGTH;

		atmRegion.stecData[Sat][tAtm].sigma		= uraSsr[getbituInc(data, i, 6)]/1000;
		atmRegion.stecData[Sat][tAtm].iod		= compactSSRIod;


		atmRegion.stecData[Sat][tAtm].poly[0]	= decodeCmpSsrField (data,i,14,0.05,0);

		if (STECType==0)
			continue;

		if ((i+24) > bitLen)
			return E_ReturnType::BAD_LENGTH;

		atmRegion.stecData[Sat][tAtm].poly[1]	= decodeCmpSsrField (data,i,12,0.02,0);
		atmRegion.stecData[Sat][tAtm].poly[2]	= decodeCmpSsrField (data,i,12,0.02,0);

		if (STECType == 1)
			continue;

		if ((i+10)  > bitLen)
			return E_ReturnType::BAD_LENGTH;

		atmRegion.stecData[Sat][tAtm].poly[3]	= decodeCmpSsrField (data,i,10,0.02,0);

		if (STECType == 2)
			continue;

		if ((i+16) > bitLen)
			return E_ReturnType::BAD_LENGTH;

		atmRegion.stecData[Sat][tAtm].poly[4]	= decodeCmpSsrField (data,i, 8,0.005,0);
		atmRegion.stecData[Sat][tAtm].poly[5]	= decodeCmpSsrField (data,i, 8,0.005,0);
	}

	ssrAtmUpdated = true;
	tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_TEC %s %d region %2d, nsat: %d",tAtm.to_string().c_str(), STECType, regionID, compactSsrIonoIndex[regionID].size());

	if (ssrMeta.multipleMessage == 0)
		copySSRCorrections(-2);

	return i;
}

/**
 * Grid based Ionosphere and troposphere corrections
*/
int decodeSSR_grid_ATM(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return 0;

	int bitLen = data.size() * 8;

	SSRMeta ssrMeta;

	int i = decodeSSR_header(data, ssrMeta, false);

	if (ssrMeta.receivedTime > compactSsrLastTime)
		compactSsrLastTime = ssrMeta.receivedTime;

	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	if (i<0)
	{
		tracepdeex(4, std::cout,"Grid message inconsistent with mask IOD\n");
		return 0;
	}

	if ((i+20) > bitLen)
		return E_ReturnType::BAD_LENGTH;

	compactSsrAtmStorage.ssrMeta = ssrMeta;

	int tropType		= getbituInc(data,i,2);
	int STECtype		= getbituInc(data,i,1);
	int regionID		= getbituInc(data,i,5);
	GTime tAtm			= ssrMeta.receivedTime;

	int n=0;
	compactSsrIonoIndex[regionID].clear();
	for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
	if (getbituInc(data,i,1)==1)
		compactSsrIonoIndex[regionID][n++] = Sat;

	SSRAtmRegion& atmRegion = compactSsrAtmStorage.atmosRegionsMap[regionID];

	if (tropType > 0)
		atmRegion.tropData[tAtm].sigma	= uraSsr[getbituInc(data,i,6)]/1000;

	int numGrid		= getbituInc(data,i,6);
	int tmp;
	for (int grd=0; grd < numGrid; grd++)
	{
		if (tropType==1)
		{
			if ((i+17) > bitLen)
				return E_ReturnType::BAD_LENGTH;

			atmRegion.tropData[tAtm].gridDry[grd]	= decodeCmpSsrField (data,i, 9,0.004,2.3);
			atmRegion.tropData[tAtm].gridWet[grd]	= decodeCmpSsrField (data,i, 8,0.004,0.252);
		}

		int ni = STECtype==0?7:16;
		for (auto& [indx, Sat]  : compactSsrIonoIndex[regionID])
		{
			if ((i+ni) > bitLen)
				return E_ReturnType::BAD_LENGTH;


			atmRegion.stecData[Sat][tAtm].grid[grd] = decodeCmpSsrField (data,i,ni,0.02,0);
		}
	}

	ssrAtmUpdated = true;
	tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_GRD %s %d %d region %2d, nsig:%d", tAtm.to_string().c_str(), tropType, STECtype, regionID, compactSsrIonoIndex[regionID].size());

	if (ssrMeta.multipleMessage == 0)
		copySSRCorrections(-2);

	return i;
}

/**
 * Compact (polynomial based) atmospheric corrections
*/
int decodeSSR_comp_ATM(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return 0;

	int bitLen = data.size()*8;

	SSRMeta ssrMeta;

	int i = decodeSSR_header(data, ssrMeta, false);

	if (ssrMeta.receivedTime > compactSsrLastTime)
		compactSsrLastTime = ssrMeta.receivedTime;
	if (now < compactSsrLastTime)
		return E_ReturnType::WAIT;

	if (i < 0)
	{
		tracepdeex(4, std::cout,"Compact atmosphere message inconsistent with mask IOD\n");
		return 1;
	}

	if ((i+21) > bitLen)
		return E_ReturnType::BAD_LENGTH;

	compactSsrAtmStorage.ssrMeta = ssrMeta;

	int tropType	= getbituInc(data,i,2);
	int stecType	= getbituInc(data,i,2);
	int regionID	= getbituInc(data,i,5);
	int numbGrid	= getbituInc(data,i,6);

	SSRAtmRegion& atmRegion = compactSsrAtmStorage.atmosRegionsMap[regionID];
	GTime tAtm				= ssrMeta.receivedTime;

	if (tropType > 0)
	{
		atmRegion.tropData[tAtm].sigma				= uraSsr[getbituInc(data,i,6)]/1000;
		tracepdeex(CMPSSRTRCLVL+2,std::cout, "\n    Trop (%.3f)", atmRegion.tropData[tAtm].sigma);

		if (tropType&1)
		{
			if ((i+11) > bitLen)
				return E_ReturnType::BAD_LENGTH;

			int dry_Type								= getbituInc(data,i,2);
			atmRegion.tropData[tAtm].polyDry[0]			= decodeCmpSsrField (data,i, 9,0.004,2.3);
			tracepdeex(CMPSSRTRCLVL+2,std::cout, "  dry: %.3f", atmRegion.tropData[tAtm].polyDry[0]);

			if (dry_Type>0)
			{
				if ((i+14) > bitLen)
					return E_ReturnType::BAD_LENGTH;

				atmRegion.tropData[tAtm].polyDry[1]		= decodeCmpSsrField (data,i, 7,0.002,0);
				atmRegion.tropData[tAtm].polyDry[2]		= decodeCmpSsrField (data,i, 7,0.002,0);

				tracepdeex(CMPSSRTRCLVL+2,std::cout, " %.3f %.3f", atmRegion.tropData[tAtm].polyDry[1], atmRegion.tropData[tAtm].polyDry[2]);
			}
			if (dry_Type>1)
			{
				if ((i+14) > bitLen)
					return E_ReturnType::BAD_LENGTH;

				atmRegion.tropData[tAtm].polyDry[3]		= decodeCmpSsrField (data,i, 7,0.001,0);
				tracepdeex(CMPSSRTRCLVL+2,std::cout, " %.3f", atmRegion.tropData[tAtm].polyDry[3]);
			}
		}
		if (tropType>1)
		{
			if ((i+5) > bitLen)
				return E_ReturnType::BAD_LENGTH;

			int wet_Type								= getbituInc(data,i,1);
			int nw = wet_Type==0?6:8;

			tracepdeex(CMPSSRTRCLVL+2,std::cout, "  wet: ");

			double wet_offset							= getbituInc(data,i,4) * 0.02;
			for (int grd=0; grd<numbGrid; grd++)
			{
				if ((i+nw) > bitLen)
					return E_ReturnType::BAD_LENGTH;

				atmRegion.tropData[tAtm].gridWet[grd]	= decodeCmpSsrField (data,i,nw,0.004,wet_offset);
				tracepdeex(CMPSSRTRCLVL+2,std::cout, "  %.3f", atmRegion.tropData[tAtm].gridWet[grd]);
			}
		}
		ssrAtmUpdated = true;
	}

	if (stecType == 0)
	{
		tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_ATM %s %s     region %2d, nsig:%d",ssrMeta.receivedTime.to_string().c_str(), (tropType>0)?"trp":"   ", regionID, compactSsrSignalIndex[regionID].size());
		if (ssrMeta.multipleMessage == 0)
			copySSRCorrections(-2);
		return i;
	}

	int n=0;
	compactSsrIonoIndex[regionID].clear();
	for (auto& [indx, Sat] : compactSsrSatelliteIndex[-1])
	if (getbituInc(data,i,1)==1)
		compactSsrIonoIndex[regionID][n++] = Sat;

	tracepdeex(CMPSSRTRCLVL+2,std::cout, "\n    Ionosphere: ");

	for (auto& [indx, Sat]  : compactSsrIonoIndex[regionID])
	{
		tracepdeex(CMPSSRTRCLVL+2,std::cout, "\n       %s (%.3f)", Sat.id().c_str(), atmRegion.tropData[tAtm].sigma);
		atmRegion.stecData[Sat][tAtm].sigma	= uraSsr[getbituInc(data,i,6)]/1000;
		if (stecType & 1)
		{
			if ((i+16) > bitLen)
				return E_ReturnType::BAD_LENGTH;

			int polyType								= getbituInc(data,i, 2);
			atmRegion.stecData[Sat][tAtm].poly[0]		= decodeCmpSsrField (data,i,14,0.05,0);
			tracepdeex(CMPSSRTRCLVL+2,std::cout, "  poly: %.3f", atmRegion.stecData[Sat][tAtm].poly[0]);

			if (polyType>0)
			{
				if ((i+24) > bitLen)
					return E_ReturnType::BAD_LENGTH;

				atmRegion.stecData[Sat][tAtm].poly[1]	= decodeCmpSsrField (data,i,12,0.02,0);
				atmRegion.stecData[Sat][tAtm].poly[2]	= decodeCmpSsrField (data,i,12,0.02,0);

				tracepdeex(CMPSSRTRCLVL+2,std::cout, " %.3f %.3f", atmRegion.stecData[Sat][tAtm].poly[1], atmRegion.stecData[Sat][tAtm].poly[2]);
			}

			if (polyType>1)
			{
				if ((i+10) > bitLen)
					return E_ReturnType::BAD_LENGTH;

				atmRegion.stecData[Sat][tAtm].poly[3]	= decodeCmpSsrField (data,i,10,0.02,0);

				tracepdeex(CMPSSRTRCLVL+2,std::cout, " %.3f", atmRegion.stecData[Sat][tAtm].poly[3]);
			}

			if (polyType==3)
			{
				if ((i+16) > bitLen)
					return E_ReturnType::BAD_LENGTH;

				atmRegion.stecData[Sat][tAtm].poly[4]	= decodeCmpSsrField (data,i, 8,0.005,0);
				atmRegion.stecData[Sat][tAtm].poly[5]	= decodeCmpSsrField (data,i, 8,0.005,0);
				tracepdeex(CMPSSRTRCLVL+2,std::cout, " %.3f %.3f", atmRegion.stecData[Sat][tAtm].poly[4], atmRegion.stecData[Sat][tAtm].poly[5]);
			}
		}

		if (stecType > 1)
		{
			int gridType								= getbituInc(data,i, 2);
			int ni = 4;
			double scale = 0;
			switch (gridType)
			{
				case 0:	ni=4; scale=0.04;	break;
				case 1:	ni=4; scale=0.12;	break;
				case 2:	ni=5; scale=0.16;	break;
				case 3:	ni=7; scale=0.24;	break;
			}

			tracepdeex(CMPSSRTRCLVL+2,std::cout, "  grid: %d", gridType);

			for (int grd=0; grd<numbGrid; grd++)
			{
				if ((i+ni) > bitLen)
					return -1;
				atmRegion.stecData[Sat][tAtm].grid[grd]	= decodeCmpSsrField (data,i,ni,scale,0);
			}
		}
	}

	ssrAtmUpdated = true;
	tracepdeex(CMPSSRTRCLVL, std::cout,"\n#CMPSSR_ATM %s %s ion region %2d, nsig:%d",ssrMeta.receivedTime.to_string().c_str(), (tropType>0)?"trp":"   ", regionID, compactSsrSignalIndex[regionID].size());

	if (ssrMeta.multipleMessage == 0)
		copySSRCorrections(-2);

	return i;
}

int decodeSSR_service(
	vector<unsigned char>&	data)
{
	int bitLen = data.size()*8;

	int i=16;
	bool multimessage	= getbituInc(data,i,1);
	int dataPacks		= getbituInc(data,i,2)+1;
	int messageCount	= getbituInc(data,i,3);

	if ((i+40*dataPacks) > bitLen)
		return E_ReturnType::BAD_LENGTH;

	if	(  lastServiceMessage >=0
		&& lastServiceMessage != (messageCount-1))
	{
		tracepdeex(2,std::cout,"Missing packets in compact SSR service messages %d -> %d\n",
			lastServiceMessage,messageCount );

		compactSsrServiceMessage.clear();
		lastServiceMessage = -1;
	}

	if 	(  lastServiceMessage < 0
		&& messageCount != 0)
	{
		tracepdeex(2,std::cout,"Missing packets in compact SSR service messages %d -> %d\n",
			lastServiceMessage,messageCount);
		return i;
	}

	lastServiceMessage = messageCount;

	for (int i = 0; i < 40 * dataPacks; i++)
	{
		unsigned char newbyte = getbituInc(data,i,8);
		compactSsrServiceMessage.push_back(newbyte);
	}

	if (multimessage)
		processServiceData(compactSsrServiceMessage);

	return i;
}

int decodecompactSSR(
	vector<unsigned char>&	data,
	GTime					now)
{
	if	(  compactSsrLastTime != GTime::noTime()
		&& now < compactSsrLastTime)
	{
		return E_ReturnType::WAIT;
	}

	if (data.size() < 7)
		return E_ReturnType::BAD_LENGTH;

	int stype = getbitu(data, 12, 4);

	CompactSSRSubtype subtype = CompactSSRSubtype::_from_integral(stype);

	switch (subtype)
	{
		case CompactSSRSubtype::MSK:		return decodeSSR_mask		(data, now);
		case CompactSSRSubtype::ORB:		return decodeSSR_orbit		(data, now);
		case CompactSSRSubtype::CLK:		return decodeSSR_clock		(data, now);
		case CompactSSRSubtype::COD:		return decodeSSR_code_bias	(data, now);
		case CompactSSRSubtype::PHS:		return decodeSSR_phas_bias	(data, now);
		case CompactSSRSubtype::BIA:		return decodeSSR_comb_bias	(data, now);
		case CompactSSRSubtype::URA:		return decodeSSR_URA		(data, now);
		case CompactSSRSubtype::TEC:		return decodeSSR_slant_TEC	(data, now);
		case CompactSSRSubtype::GRD:		return decodeSSR_grid_ATM	(data, now);
		case CompactSSRSubtype::SRV:		return decodeSSR_service	(data);
		case CompactSSRSubtype::CMB:		return decodeSSR_combined	(data, now);
		case CompactSSRSubtype::ATM:		return decodeSSR_comp_ATM	(data, now);

		default: tracepdeex(2, std::cout,"Submessage type %d not supported\n", stype);

		return E_ReturnType::OK;
	}
}
