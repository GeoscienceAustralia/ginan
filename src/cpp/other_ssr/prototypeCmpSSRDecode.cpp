
#include "otherSSR.hpp"
#include "ionoModel.hpp"

int compactSSRIod = -1;
GTime maskTime;

map<int, map<int,SatSys>>					compactSSRSatlIndex;
map<int, map<int,pair<SatSys,E_ObsCode>>>	compactSSRSignlIndex;
map<int, map<int,SatSys>>					compactSSRIonoIndex;

map<int, map<SatSys, SSROut>> compactSSRStorage;
SSRAtm compactSSRAtmStorage;

vector<unsigned char> compactSSRServiceMessage;
int lastServiceMessage=-1;

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

int cmpSSRUpdateInt[16] =
{
	1, 2, 5, 10, 15, 30, 60, 120, 240, 300, 600, 900, 1800, 3600, 7200, 10800
};

double cmpSSRaccuracy[64] =
{
	0.03000, 0.00025, 0.00050, 0.00075, 0.00100, 0.00125, 0.00150, 0.00175,
	0.00200, 0.00275, 0.00350, 0.00425, 0.00500, 0.00575, 0.00650, 0.00725,
	0.00800, 0.01025, 0.01250, 0.01475, 0.01700, 0.01925, 0.02150, 0.02375,
	0.02600, 0.03275, 0.03950, 0.04625, 0.05300, 0.05975, 0.06650, 0.07325,
	0.08000, 0.10025, 0.12050, 0.14075, 0.16100, 0.18125, 0.20150, 0.22175,
	0.24200, 0.30275, 0.36350, 0.42425, 0.48500, 0.54575, 0.60650, 0.66725,
	0.72800, 0.91025, 1.09250, 1.27475, 1.45700, 1.63925, 1.82150, 2.00375,
	2.18600, 2.73275, 3.27950, 3.82625, 4.37300, 4.91975, 5.46650, 6.01325
};

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

void decode_grid_info(
	vector<unsigned char>&	data)
{
	int i=4;
	int servIOD		= getbituInc(data,i,3);
	int areaIOD		= getbituInc(data,i,4);
	int numNet		= getbituInc(data,i,6)+1;
	
	bool updated = false;
	
	for (int n = 0; n < numNet; n++)
	{
		int		netw_ID		= getbituInc(data,i,5);
		bool	part		= getbituInc(data,i,1);
		int		partID		= 16*netw_ID;
		if (part)
			partID			= getbituInc(data,i,4)+16*netw_ID;
		int		gridType	= getbituInc(data,i,2);
		
		SSRAtmRegion& atmRegion = nav.ssrAtm.atmosRegionsMap[partID];               
		
		if (atmRegion.regionDefIOD != servIOD)
		{
			updated = true;
			atmRegion.regionDefIOD = servIOD;
		}
		
		switch (gridType)
		{
			case 0:
			{
				double thisLat		= getbitsInc(data,i,15) * 0.0054931640625;
				double thisLon		= getbitsInc(data,i,16) * 0.0054931640625;
				
				atmRegion.gridLat[0]= thisLat;
				atmRegion.gridLon[0]= thisLon;
				int ngrid			= getbituInc(data,i, 6);
				
				double maxdLat[2]	= {0};
				double maxdLon[2]	= {0};
				
				for (int grd=0; grd<ngrid; grd++)
				{
					thisLat		+= getbitsInc(data,i,10)*0.01;
					thisLon		+= getbitsInc(data,i,11)*0.01;
					
					if (atmRegion.minLat > thisLat)	atmRegion.minLat = thisLat;
					if (atmRegion.maxLat < thisLat)	atmRegion.maxLat = thisLat;
					if (atmRegion.minLon > thisLon)	atmRegion.minLon = thisLon;
					if (atmRegion.maxLon < thisLon)	atmRegion.maxLon = thisLon;
					
					atmRegion.gridLat[grd] = thisLat<-180?(thisLat+360):(thisLat>180?(thisLat-360):thisLat);
					atmRegion.gridLon[grd] = thisLon<-180?(thisLon+360):(thisLon>180?(thisLon-360):thisLon);
				}
				break;
			}
			case 1:
			{
				atmRegion.gridLat[0]		= getbitsInc(data,i,15) * 0.0054931640625;
				atmRegion.gridLon[0]		= getbitsInc(data,i,16) * 0.0054931640625;
				int ngridLat				= getbituInc(data,i, 6);
				int ngridLon				= getbituInc(data,i, 6);
				
				
				atmRegion.intLat			= getbituInc(data,i, 9) * 0.01;
				atmRegion.intLon			= getbituInc(data,i,10) * 0.01;
				atmRegion.minLat			= atmRegion.gridLat[0] - ngridLat*atmRegion.intLat;
				atmRegion.maxLat			= atmRegion.gridLat[0];
				atmRegion.minLon			= atmRegion.gridLon[0];
				atmRegion.maxLat			= atmRegion.gridLon[0] + ngridLon*atmRegion.intLon;
				
				for (int nlat=0, grd=0; nlat<ngridLat; nlat++)
				for (int nlon=0;        nlon<ngridLon; nlon++)
				{
					atmRegion.gridLat[grd]	= atmRegion.gridLat[0] - nlat * atmRegion.intLat;
					atmRegion.gridLon[grd]	= atmRegion.gridLon[0] + nlon * atmRegion.intLon;
					grd++;
				}
				break;   
			}
			case 2:
			{
				atmRegion.gridLat[0]		= getbitsInc(data,i,15) * 0.0054931640625;
				atmRegion.gridLon[0]		= getbitsInc(data,i,16) * 0.0054931640625;
				int ngridLat				= getbituInc(data,i, 6);
				int ngridLon				= getbituInc(data,i, 6);
				
				atmRegion.intLat			= getbituInc(data,i, 9) * 0.01;
				atmRegion.intLon			= getbituInc(data,i,10) * 0.01;
				atmRegion.minLat			= atmRegion.gridLat[0];
				atmRegion.maxLat			= atmRegion.gridLat[0] + ngridLat * atmRegion.intLat;
				atmRegion.minLon			= atmRegion.gridLon[0];
				atmRegion.maxLat			= atmRegion.gridLon[0] + ngridLon * atmRegion.intLon;
				
				for (int nlon=0, grd=0; nlon<ngridLon; nlon++)
				for (int nlat=0;        nlat<ngridLat; nlat++)
				{
					atmRegion.gridLat[grd]	= atmRegion.gridLat[0] + nlat * atmRegion.intLat;
					atmRegion.gridLon[grd]	= atmRegion.gridLon[0] + nlon * atmRegion.intLon;
					grd++;
				}
				break;
			}
		}
		
	}
	if (updated)
		defineLocalIonoBasis();
}

void processServiceData(
	vector<unsigned char>&	data)
{
	int srvMessageType = getbitu(data,0,4);
	switch (srvMessageType)
	{
		case 3:	decode_grid_info(data);		break;
		default: tracepdeex(4, std::cout,"Unsupported compact SSR service message\n");
	}
}


int decodeSSR_header(
	vector<unsigned char>&	data,
	SSRMeta& ssrMeta,
	bool mask)
{
	int i=16;
	if (mask)
		ssrMeta.epochTime1s	= getbituInc(data,i,20);
	else
		ssrMeta.epochTime1s	= getbituInc(data,i,20);	
	ssrMeta.updateIntIndex	= getbituInc(data,i, 4);
	ssrMeta.multipleMessage	= getbituInc(data,i, 1);
	int ssrIod				= getbituInc(data,i, 4);
	if (mask)
		compactSSRIod		= ssrIod;
	else if (ssrIod != compactSSRIod)
		return -1;
	
	return i;
}	

/**
 * Compact SSR message type 1: satellite/code mask
 */
void decodeSSR_mask(
	vector<unsigned char>&	data,
	GTime					now)
{
	compactSSRSatlIndex.clear();
	compactSSRSignlIndex.clear();
	
	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, true);
	// maskTime = tow_to_time(now, ssrMeta.epochTime1s);
	maskTime = GTime(GTow(ssrMeta.epochTime1s),now);
	
	int nsys = getbituInc(data,i,4);
	for (int s = 0, satind = 0, cellind = 0; s < nsys; s++)
	{
		if ((i+61)<data.size())
		{
			compactSSRIod = -1;
			return;
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
			default: tracepdeex(2, std::cout,"Warning unsupported GNSS for compact SSR\n");
			compactSSRIod = -1;
			return;
		}
		
		int nsat = 0;
		map<int,SatSys> sysSatIndex;
		for (int n=0; n<40; n++)
		{
			SatSys Sat(sys,n);
			if (getbituInc(data,i,1)==1)
				compactSSRSatlIndex[-1][satind++] = Sat;
			sysSatIndex[nsat++] = Sat;
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
				tracepdeex(2, std::cout,"Warning cell size for compact SSR exeeds maximum size\n");
			cellField = true;
		}
		
		int ncell = 0;
		for (auto& [sind,Sat] : sysSatIndex)
		for (auto& [cind,code] : sysCodeIndex)
		{
			if	(  cellField
				&& ncell++ < 128
				&& getbituInc(data,i,1)==0)
			{
				continue;
			}
			
			compactSSRSignlIndex[-1][cellind].first		= Sat;
			compactSSRSignlIndex[-1][cellind].second	= code;
			cellind++;
		}
	}
}

/**
 * Compact SSR message type 2: global satellite orbits
 */
void decodeSSR_orbit(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return;
	
	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);
	if (i < 0)
	{
		tracepdeex(4, std::cout,"Orbit frame inconsistent with mask IOD\n");
		return;
	}
	
	SSREph ssrEph;
	ssrEph.ssrMeta	= ssrMeta;
	ssrEph.t0		= toh2time(now, 1.0*ssrMeta.epochTime1s); 
	ssrEph.udi		= cmpSSRUpdateInt[ssrMeta.updateIntIndex];
	ssrEph.iod		= compactSSRIod;
	
	for (auto& [indx, Sat] : compactSSRSatlIndex[-1])
	{
		int ni = (Sat.sys == +E_Sys::GAL)?10:8;
		if ((i+ni+41)<data.size()*8)
			return;
		ssrEph.iode 	= getbituInc(data, i, ni);
		ssrEph.deph[0]	= getbitsInc(data, i, 15) * 0.0016;
		ssrEph.deph[1]	= getbitsInc(data, i, 13) * 0.0064;
		ssrEph.deph[2]	= getbitsInc(data, i, 13) * 0.0064;
		
		compactSSRStorage[-1][Sat].ssrEph		= ssrEph;
		compactSSRStorage[-1][Sat].ephUpdated	= true;
	}
	
}

/**
 * Compact SSR message type 3: global satellite clock offsets
 */
void decodeSSR_clock(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return;
	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);
	if (i<0)
	{
		tracepdeex(4, std::cout,"Clock frame inconsistent with mask IOD\n");
		return;
	}
	SSRClk ssrClk;
	ssrClk.ssrMeta	= ssrMeta;
	ssrClk.t0		= toh2time(now, 1.0*ssrMeta.epochTime1s); 
	ssrClk.udi		= cmpSSRUpdateInt[ssrMeta.updateIntIndex];
	ssrClk.iod		= compactSSRIod;
	
	for (auto& [indx, Sat] : compactSSRSatlIndex[-1])
	{
		if ((i+15)<data.size()*8)
			return;
		ssrClk.dclk[0]	= getbitsInc(data, i, 15) * 0.0016;	
		
		compactSSRStorage[-1][Sat].ssrClk		= ssrClk;
		compactSSRStorage[-1][Sat].clkUpdated	= true;
	}
	
}

/**
 * Compact SSR message type 11: regional satellite orbits and clock offsets
 */
void decodeSSR_combined(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return;
	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);
	if (i<0)
	{
		tracepdeex(4, std::cout,"Orbit/Clock frame inconsistent with mask IOD\n");
		return;
	}
	
	bool orbitAvailable = getbituInc(data,i,1);
	bool clockAvailable = getbituInc(data,i,1);
	int  regionID = -1;
	if (getbituInc(data,i,1))
	{
		regionID	= getbituInc(data,i,5)*16;
		int n=0;
		for (auto& [indx, Sat] : compactSSRSatlIndex[-1])
		if (getbituInc(data,i,1)==1)
			compactSSRSatlIndex[regionID][n++] = Sat;
	}
	
	SSREph ssrEph;
	ssrEph.ssrMeta	= ssrMeta;
	ssrEph.t0		= toh2time(now, 1.0*ssrMeta.epochTime1s); 
	ssrEph.udi		= cmpSSRUpdateInt[ssrMeta.updateIntIndex];
	ssrEph.iod		= compactSSRIod;
	
	SSRClk ssrClk;
	ssrClk.ssrMeta	= ssrMeta;
	ssrClk.t0		= ssrEph.t0; 
	ssrClk.udi		= ssrEph.udi;
	ssrClk.iod		= compactSSRIod;
	
	for (auto& [indx, Sat] : compactSSRSatlIndex[regionID])
	{
		int ni = (Sat.sys == +E_Sys::GAL)?10:8;
		if (orbitAvailable)
		{
			if ((i+ni+41)<data.size()*8)
				return;
		
			ssrEph.iode 	= getbituInc(data, i, ni);
			ssrEph.deph[0]	= getbitsInc(data, i, 15) * 0.0016;
			ssrEph.deph[1]	= getbitsInc(data, i, 13) * 0.0064;
			ssrEph.deph[2]	= getbitsInc(data, i, 13) * 0.0064;
		
			compactSSRStorage[regionID][Sat].ssrEph		= ssrEph;
			compactSSRStorage[regionID][Sat].ephUpdated	= true;
		}
		if (clockAvailable)
		{
			if ((i+15)<data.size()*8)
				return;
			ssrClk.dclk[0]	= getbitsInc(data, i, 15) * 0.0016;	
		
			compactSSRStorage[regionID][Sat].ssrClk		= ssrClk;
			compactSSRStorage[regionID][Sat].clkUpdated	= true;
		}
	}
}

/**
 * Compact SSR message type 4: global satellite code biases
*/
void decodeSSR_code_bias(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return;
	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);
	if (i<0)
	{
		tracepdeex(4, std::cout,"Code bias frame inconsistent with mask IOD\n");
		return;
	}
	
	SSRCodeBias ssrCodeBias;
	ssrCodeBias.ssrMeta	= ssrMeta;
	ssrCodeBias.t0		= toh2time(now, 1.0*ssrMeta.epochTime1s);
	ssrCodeBias.udi		= cmpSSRUpdateInt[ssrMeta.updateIntIndex];
	ssrCodeBias.iod 	= compactSSRIod;
		
	for (auto& [indx, SatnCode] : compactSSRSignlIndex[-1])
	{
		SatSys Sat		= SatnCode.first;
		E_ObsCode code	= SatnCode.second;
		
		if ((i+11)<data.size()*8)
			return;
		
		ssrCodeBias.obsCodeBiasMap[code].bias	= getbitsInc(data, i, 11) * 0.02;
		compactSSRStorage[-1][Sat].ssrCodeBias	= ssrCodeBias;
		compactSSRStorage[-1][Sat].codeUpdated	= true;
	}
}

/**
 * Compact SSR message type 5: global satellite phase biases
*/
void decodeSSR_phas_bias(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return;
	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);
	if (i<0)
	{
		tracepdeex(4, std::cout,"Phase bias frame inconsistent with mask IOD\n");
		return;
	}
	
	SSRPhasBias ssrPhasBias;
	ssrPhasBias.ssrMeta	= ssrMeta;
	ssrPhasBias.t0		= toh2time(now, 1.0*ssrMeta.epochTime1s);
	ssrPhasBias.udi		= cmpSSRUpdateInt[ssrMeta.updateIntIndex];
	ssrPhasBias.iod 	= compactSSRIod;
	
	for (auto& [indx, SatnCode] : compactSSRSignlIndex[-1])
	{
		SatSys Sat		= SatnCode.first;
		E_ObsCode code	= SatnCode.second;
		
		if ((i+17)<data.size()*8)
			return;
		
		
		ssrPhasBias.obsCodeBiasMap[code].bias	= getbitsInc(data, i, 15) * 0.001;
		
		SSRPhaseCh ssrPhaseCh;
		ssrPhaseCh.signalDisconCnt				= getbituInc(data, i, 2);
		ssrPhasBias.ssrPhaseChs[code]			= ssrPhaseCh;
			
		compactSSRStorage[-1][Sat].ssrPhasBias	= ssrPhasBias;
		compactSSRStorage[-1][Sat].phaseUpdated	= true;
	}	
}

/**
 * Combined code and phase bias (with potential regional biases)
*/
void decodeSSR_comb_bias(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return;
	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);
	if (i<0)
	{
		tracepdeex(4, std::cout,"Combined bias frame inconsistent with mask IOD\n");
		return;
	}
	
	bool code_Available = getbituInc(data,i,1);
	bool phaseAvailable = getbituInc(data,i,1);
	int  regionID		= -1;
	if (getbituInc(data,i,1)==0)
	{
		regionID		= getbituInc(data,i,5)*16;
		int n=0;
		for (auto& [indx, SatnCode] : compactSSRSignlIndex[-1])
		if (getbituInc(data,i,1)==1)
			compactSSRSignlIndex[regionID][n++] = SatnCode;
	}
	
	SSRCodeBias ssrCodeBias;
	ssrCodeBias.ssrMeta	= ssrMeta;
	ssrCodeBias.t0		= toh2time(now, 1.0*ssrMeta.epochTime1s);
	ssrCodeBias.udi		= cmpSSRUpdateInt[ssrMeta.updateIntIndex];
	ssrCodeBias.iod 	= compactSSRIod;
	
	SSRPhasBias ssrPhasBias;
	ssrPhasBias.ssrMeta	= ssrMeta;
	ssrPhasBias.t0		= ssrCodeBias.t0;
	ssrPhasBias.udi		= ssrCodeBias.udi;
	ssrPhasBias.iod 	= compactSSRIod;
	
	for (auto& [indx, SatnCode] : compactSSRSignlIndex[regionID])
	{
		SatSys Sat		= SatnCode.first;
		E_ObsCode code	= SatnCode.second;
		
		if (code_Available)
		{
			if ((i+11)<data.size()*8)
			return;
		
			ssrCodeBias.obsCodeBiasMap[code].bias			= getbitsInc(data, i, 11) * 0.02;
			compactSSRStorage[regionID][Sat].ssrCodeBias	= ssrCodeBias;
			compactSSRStorage[regionID][Sat].codeUpdated	= true;
		}
		
		if (phaseAvailable)
		{
			if ((i+17)<data.size()*8)
				return;
		
			ssrPhasBias.obsCodeBiasMap[code].bias			= getbitsInc(data, i, 15) * 0.001;
			ssrPhasBias.ssrPhaseChs[code].signalDisconCnt	= getbituInc(data, i, 2);
			compactSSRStorage[regionID][Sat].ssrPhasBias	= ssrPhasBias;
			compactSSRStorage[regionID][Sat].phaseUpdated	= true;
		}
	}
}

/**
 * User Range Accuracy
*/
void decodeSSR_URA(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return;
	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);
	if (i<0)
	{
		tracepdeex(4, std::cout,"URA frame inconsistent with mask IOD\n");
		return;
	}
	
	SSRUra ssrUra;
	ssrUra.t0		= toh2time(now, 1.0*ssrMeta.epochTime1s);
	ssrUra.udi		= cmpSSRUpdateInt[ssrMeta.updateIntIndex];
	ssrUra.iod 		= compactSSRIod;
	
	for (auto& [indx, Sat] : compactSSRSatlIndex[-1])
	{
		if ((i+6)<data.size()*8)
			return;
		
		ssrUra.ura	= getbituInc(data, i, 6);	
		
		compactSSRStorage[-1][Sat].ssrUra		= ssrUra;
		compactSSRStorage[-1][Sat].uraUpdated	= true;
	}	
}

/**
 * Compact (polynomial based) salnt TEC maps
*/
void decodeSSR_slant_TEC(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return;
	SSRMeta ssrMeta;
	int i = decodeSSR_header(data, ssrMeta, false);
	if (i<0)
	{
		tracepdeex(4, std::cout,"URA frame inconsistent with mask IOD\n");
		return;
	}
	
	int STECType		= getbituInc(data,i,2);
	int regionID		= getbituInc(data,i,5)*16;
	int n=0;
	for (auto& [indx, Sat] : compactSSRSatlIndex[-1])
		compactSSRIonoIndex[regionID][n++] = Sat;
	
	
	SSRAtmRegion& atmRegion = compactSSRAtmStorage.atmosRegionsMap[regionID];
	
	GTime tAtm		= toh2time(now, ssrMeta.epochTime1s);
	
	for (auto& [indx, Sat] : compactSSRIonoIndex[regionID])
	{
		if ((i+20)<data.size()*8)
			return;
		
		atmRegion.stecData[Sat][tAtm].accr		= cmpSSRaccuracy[getbituInc(data, i, 6)];
		atmRegion.stecData[Sat][tAtm].iod		= compactSSRIod;
		
		atmRegion.stecData[Sat][tAtm].poly[0]	= getbitsInc(data, i,14) * 0.05;
		if (STECType==0)
			continue;
		
		if ((i+24)<data.size()*8)
				return;
		
		atmRegion.stecData[Sat][tAtm].poly[1]	= getbitsInc(data, i,12) * 0.02;
		atmRegion.stecData[Sat][tAtm].poly[2]	= getbitsInc(data, i,12) * 0.02;
		
		if (STECType == 1)
			continue;
		
		if ((i+10)<data.size()*8)
				return;
		
		
		atmRegion.stecData[Sat][tAtm].poly[3]	= getbitsInc(data, i,10) * 0.02;
		
		if (STECType == 2)
			continue;
		
		if ((i+16)<data.size()*8)
				return;
		
		atmRegion.stecData[Sat][tAtm].poly[4]	= getbitsInc(data, i, 8) * 0.005;
		atmRegion.stecData[Sat][tAtm].poly[5]	= getbitsInc(data, i, 8) * 0.005;
	}
}

/**
 * Grid based Ionosphere and troposphere corrections
*/
void decodeSSR_grid_ATM(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return;
	
	SSRMeta ssrMeta;
	
	int i = decodeSSR_header(data, ssrMeta, false);
	if (i<0)
	{
		tracepdeex(4, std::cout,"URA frame inconsistent with mask IOD\n");
		return;
	}
	
	int tropType		= getbituInc(data,i,2);
	int STECtype		= getbituInc(data,i,1);
	int regionID		= getbituInc(data,i,5)*16;
	
	int n=0;
	for (auto& [indx, Sat] : compactSSRSatlIndex[-1])
		compactSSRIonoIndex[regionID][n++] = Sat;
	
	SSRAtmRegion& atmRegion = compactSSRAtmStorage.atmosRegionsMap[regionID];
	
	if (tropType > 0)
		atmRegion.tropData[now].acc	= cmpSSRaccuracy[getbituInc(data,i,6)];
	
	int numGrid		= getbituInc(data,i,6);
	
	for (int grd=0; grd < numGrid; grd++)
	{
		if (tropType==1)
		{
			if ((i+17)<data.size()*8)
				return;
			atmRegion.tropData[now].gridDry[grd] = getbitsInc(data,i,9) * 0.004 + 2.3;
			atmRegion.tropData[now].gridWet[grd] = getbitsInc(data,i,8) * 0.004 + 0.252;
		}
		
		int ni = STECtype==0?7:16;
		for (auto& [indx, Sat]  : compactSSRIonoIndex[regionID])
		{
			if ((i+ni)<data.size()*8)
				return;
			atmRegion.stecData[Sat][now].grid[grd] = getbitsInc(data,i,ni) * 0.02;
		}
	}
}

/**
 * Compact (polynomial based) atmospheric corrections
*/
void decodeSSR_comp_ATM(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (compactSSRIod < 0)
		return;
	
	SSRMeta ssrMeta;
	
	int i = decodeSSR_header(data, ssrMeta, false);
	if (i < 0)
	{
		tracepdeex(4, std::cout,"URA frame inconsistent with mask IOD\n");
		return;
	}
	
	if ((i+21)<data.size()*8)
		return;
	
	int tropType	= getbituInc(data,i,2);
	int stecType	= getbituInc(data,i,2);
	int regionID	= getbituInc(data,i,5)+16;
	int numbGrid	= getbituInc(data,i,6);
	
	SSRAtmRegion& atmRegion = compactSSRAtmStorage.atmosRegionsMap[regionID];
	
	atmRegion.tropData[now].acc						= cmpSSRaccuracy[getbituInc(data,i,6)];
	if (tropType&1)
	{
		if ((i+11)<data.size()*8)
			return;
		
		int dry_Type								= getbituInc(data,i,2);
		atmRegion.tropData[now].poly[0]				= getbitsInc(data,i,9) * 0.004 + 2.3;
		if (dry_Type>0)
		{
			if ((i+14)<data.size())
				return;
			
			atmRegion.tropData[now].poly[1]			= getbitsInc(data,i,7) * 0.002;
			atmRegion.tropData[now].poly[2]			= getbitsInc(data,i,7) * 0.002;
		}
		if (dry_Type>1)
			atmRegion.tropData[now].poly[3]			= getbitsInc(data,i,7) * 0.001;
	}
	if (tropType>1)
	{
		if ((i+5)<data.size()*8)
			return;
			
		int wet_Type								= getbituInc(data,i,1);
		int nw = wet_Type==0?6:8;
		
		double wet_offset							= getbituInc(data,i,4) * 0.02;
		for (int grd=0; grd<numbGrid && (i+nw)<data.size()*8; grd++)
			atmRegion.tropData[now].gridWet[grd]	= getbituInc(data,i,nw) * 0.004 + wet_offset;
		
	}
	
	int n=0;
	for (auto& [indx, Sat] : compactSSRSatlIndex[-1])
		compactSSRIonoIndex[regionID][n++] = Sat;
	
	for (auto& [indx, Sat]  : compactSSRIonoIndex[regionID])
	{
		atmRegion.stecData[Sat][now].accr	= cmpSSRaccuracy[getbituInc(data,i,6)];
		if (stecType & 1)
		{
			if ((i+16)<data.size()*8)
				return;
			
			int polyType								= getbituInc(data,i, 2);
			atmRegion.stecData[Sat][now].poly[0]		= getbitsInc(data,i,14) * 0.05;
			
			if (polyType>0)
			{
				if ((i+24)<data.size()*8)
					return;
			
				atmRegion.stecData[Sat][now].poly[1]	= getbitsInc(data,i,12) * 0.02;
				atmRegion.stecData[Sat][now].poly[2]	= getbitsInc(data,i,12) * 0.02;
			}
			
			if (polyType>1)
			{
				if ((i+10)<data.size()*8)
					return;
				
				atmRegion.stecData[Sat][now].poly[3]	= getbitsInc(data,i,10) * 0.02;
			}
			
			if (polyType==3)
			{
				if ((i+16)<data.size()*8)
					return;
				atmRegion.stecData[Sat][now].poly[4]	= getbitsInc(data,i, 8) * 0.005;
				atmRegion.stecData[Sat][now].poly[5]	= getbitsInc(data,i, 8) * 0.005;
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
				case 3:	ni=6; scale=0.24;	break;
			}
			
			for (int grd=0; grd<numbGrid&& (i+ni)<data.size()*8; grd++)
				atmRegion.stecData[Sat][now].grid[grd]	= getbitsInc(data,i, ni) * scale;
		}
	}
}

void decodeSSR_service(
	vector<unsigned char>&	data,
	GTime					now)
{
	int i=16;
	bool multimessage	= getbituInc(data,i,1);
	int dataPacks		= getbituInc(data,i,2)+1;
	int messageCount	= getbituInc(data,i,3);
	
	if ((i+40*dataPacks) < data.size() * 8)
		return;
	
	if	(  lastServiceMessage >=0
		&& lastServiceMessage != (messageCount-1))
	{
		tracepdeex(2,std::cout,"Missing packets in compact SSR service messages %d -> %d\n",
			lastServiceMessage,messageCount );
		
		compactSSRServiceMessage.clear();
		lastServiceMessage = -1;
	}
	
	if 	(  lastServiceMessage < 0
		&& messageCount != 0)
	{
		tracepdeex(2,std::cout,"Missing packets in compact SSR service messages %d -> %d\n",
			lastServiceMessage,messageCount);
		return;
	}
	
	lastServiceMessage = messageCount;
	
	for (int i=0; i<40*dataPacks; i++)
	{
		unsigned char newbyte = getbituInc(data,i,8);
		compactSSRServiceMessage.push_back(newbyte);
	}
	
	if (multimessage)
		processServiceData(compactSSRServiceMessage);
}


void decodecompactSSR(
	vector<unsigned char>&	data,
	GTime					now)
{
	if (data.size()<7)
		return;
	
	int stype = getbitu(data,12,4);
	CompactSSRSubtype subtype = CompactSSRSubtype::_from_integral(stype);
	
	switch (subtype)
	{
		case CompactSSRSubtype::CMP_SSR_MSK: return decodeSSR_mask		(data, now);
		case CompactSSRSubtype::CMP_SSR_ORB: return decodeSSR_orbit		(data, now);
		case CompactSSRSubtype::CMP_SSR_CLK: return decodeSSR_clock		(data, now);
		case CompactSSRSubtype::CMP_SSR_COD: return decodeSSR_code_bias	(data, now);
		case CompactSSRSubtype::CMP_SSR_PHS: return decodeSSR_phas_bias	(data, now);
		case CompactSSRSubtype::CMP_SSR_BIA: return decodeSSR_comb_bias	(data, now);
		case CompactSSRSubtype::CMP_SSR_URA: return decodeSSR_URA		(data, now);
		case CompactSSRSubtype::CMP_SSR_TEC: return decodeSSR_slant_TEC	(data, now);
		case CompactSSRSubtype::CMP_SSR_GRD: return decodeSSR_grid_ATM	(data, now);
		case CompactSSRSubtype::CMP_SSR_SRV: return decodeSSR_service	(data, now);
		case CompactSSRSubtype::CMP_SSR_CMB: return decodeSSR_combined	(data, now);
		case CompactSSRSubtype::CMP_SSR_ATM: return decodeSSR_comp_ATM	(data, now);
		
		default: tracepdeex(4, std::cout,"Submessage type %d not supported\n", stype);
	}
}
