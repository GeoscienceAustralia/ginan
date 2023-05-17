
#include "otherSSR.hpp"

map<E_Sys, map<E_ObsCode, int>> igsSSRCode2Index
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

map<SatSys,SSRClk>	last_clock;

unsigned short IGS_SSR_subtype(
	int		typ, 
	E_Sys	sys)
{
	if (typ == 8)           return 201;
	switch (sys)
	{
		case E_Sys::GPS:    return typ+20;
		case E_Sys::GLO:    return typ+40;
		case E_Sys::GAL:    return typ+60;
		case E_Sys::QZS:    return typ+80;
		case E_Sys::BDS:    return typ+100;
		case E_Sys::SBS:    return typ+120;
	}
	return 0;
}

int IGS_SSR_group(
	IgsSSRSubtype	subTyp, 
	E_Sys&			sys)
{
	sys=E_Sys::NONE;
	if (subTyp == +IgsSSRSubtype::IGS_SSR_IONVTEC) return 8;
	
	int iSubTyp = subTyp;
	
	switch (iSubTyp/20)
	{
		case 1:	sys = E_Sys::GPS;	return iSubTyp%20;
		case 2:	sys = E_Sys::GLO;	return iSubTyp%20;
		case 3:	sys = E_Sys::GAL;	return iSubTyp%20;
		case 4:	sys = E_Sys::QZS;	return iSubTyp%20;
		case 5:	sys = E_Sys::BDS;	return iSubTyp%20;
		case 6:	sys = E_Sys::SBS; 	return iSubTyp%20;
	}
	return 0;
}

vector<uint8_t>  encodeIGS_ORB( 
	map<SatSys, SSROut>&		orbClkMap,
	E_Sys						sys,
	bool						last)
{
	int numSat = orbClkMap.size();
	if (numSat == 0)
	{
		return vector<uint8_t>();
	}
	
	auto& [Sat, ssrOut1]	= *orbClkMap.begin();
	auto& ssrEph			= ssrOut1.ssrEph;
	auto& ssrMeta			= ssrEph.ssrMeta;
	
	int bitLen	= 78 + numSat * 135;
	int byteLen = ceil(bitLen/8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();
	
	unsigned int submessCode     = IGS_SSR_subtype (1, sys);
	unsigned int multipleMessage = last?0:1;
	
	int i = 0;
	i = setbituInc(buf,i,12,	4076);
	i = setbituInc(buf,i,3,	    1);
	i = setbituInc(buf,i,8,	    submessCode);
	i = setbituInc(buf,i,20,	ssrMeta.epochTime1s);
	i = setbituInc(buf,i,4,		ssrMeta.updateIntIndex);
	i = setbituInc(buf,i,1,		multipleMessage);
	i = setbituInc(buf,i,4,		ssrEph.iod);
	i = setbituInc(buf,i,16,	ssrMeta.provider);
	i = setbituInc(buf,i,4,		ssrMeta.solution);
	i = setbituInc(buf,i,1,		ssrMeta.referenceDatum);
	i = setbituInc(buf,i,6,		numSat);
	
	for (auto& [Sat, ssrOut] : orbClkMap)
	{
		auto& ssrEph = ssrOut.ssrEph;
			
		i = setbituInc(buf,i, 6,	Sat.prn);
		i = setbituInc(buf,i, 8,	ssrEph.iode);
		
		int d;
		d = (int)round(ssrEph.deph[0]		/ 0.1e-3);				i = setbitsInc(buf,i,22,d);
		d = (int)round(ssrEph.deph[1]		/ 0.4e-3);				i = setbitsInc(buf,i,20,d);
		d = (int)round(ssrEph.deph[2]		/ 0.4e-3);				i = setbitsInc(buf,i,20,d);
		d = (int)round(ssrEph.ddeph[0]		/ 0.001e-3);			i = setbitsInc(buf,i,21,d); 
		d = (int)round(ssrEph.ddeph[1]		/ 0.004e-3);			i = setbitsInc(buf,i,19,d);    
		d = (int)round(ssrEph.ddeph[2]		/ 0.004e-3);			i = setbitsInc(buf,i,19,d);
	}
	int bitl = byteLen*8-i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding orbit.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
	}
	i = setbituInc(buf, i, bitl, 0); 
	
	return buffer;
}

vector<uint8_t>  encodeIGS_CLK( 
	map<SatSys, SSROut>&		orbClkMap,
	E_Sys						sys,
	bool						last)
{
	int numSat = orbClkMap.size();
	if (numSat == 0)
	{
		return vector<uint8_t>();
	}
	
	auto& [Sat, ssrOut1]	= *orbClkMap.begin();
	auto& ssrEph			= ssrOut1.ssrEph;
	auto& ssrMeta			= ssrEph.ssrMeta;
	
	int bitLen	= 78 + numSat * 76;
	int byteLen = ceil(bitLen/8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();
	
	unsigned int submessCode     = IGS_SSR_subtype (2, sys);
	unsigned int multipleMessage = last?0:1;
	
	int i = 0;
	i = setbituInc(buf,i,12,	4076);
	i = setbituInc(buf,i,3,	    1);
	i = setbituInc(buf,i,8,	    submessCode);
	i = setbituInc(buf,i,20,	ssrMeta.epochTime1s);
	i = setbituInc(buf,i,4,		ssrMeta.updateIntIndex);
	i = setbituInc(buf,i,1,		multipleMessage);
	i = setbituInc(buf,i,4,		ssrEph.iod);
	i = setbituInc(buf,i,16,	ssrMeta.provider);
	i = setbituInc(buf,i,4,		ssrMeta.solution);
	i = setbituInc(buf,i,6,		numSat);
	
	for (auto& [Sat, ssrOut] : orbClkMap)
	{
		auto& ssrClk = ssrOut.ssrClk;
			
		i = setbituInc(buf,i, 6,	Sat.prn);
		
		int d;
		d = (int)round(ssrClk.dclk[0]		/ 0.1e-3);				i = setbitsInc(buf,i,22,d); 
		d = (int)round(ssrClk.dclk[1]		/ 0.001e-3);			i = setbitsInc(buf,i,21,d);  
		d = (int)round(ssrClk.dclk[2]		/ 0.00002e-3);			i = setbitsInc(buf,i,27,d);  
		
		last_clock[Sat] = ssrClk;
	}
	int bitl = byteLen*8-i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding clock.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
	}
	i = setbituInc(buf, i, bitl, 0); 
	
	return buffer;
}

vector<uint8_t>  encodeIGS_CMB( 
	map<SatSys, SSROut>&		orbClkMap,
	E_Sys						sys,
	bool						last)
{
	int numSat = orbClkMap.size();
	if (numSat == 0)
	{
		return vector<uint8_t>();
	}
	
	auto& [Sat, ssrOut1]	= *orbClkMap.begin();
	auto& ssrEph			= ssrOut1.ssrEph;
	auto& ssrMeta			= ssrEph.ssrMeta;
	
	int bitLen	= 79 + numSat * 205;
	int byteLen = ceil(bitLen/8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();
	
	unsigned int submessCode     = IGS_SSR_subtype (3, sys);
	unsigned int multipleMessage = last?0:1;
	
	int i = 0;
	i = setbituInc(buf,i,12,	4076);
	i = setbituInc(buf,i,3,	    1);
	i = setbituInc(buf,i,8,	    submessCode);
	i = setbituInc(buf,i,20,	ssrMeta.epochTime1s);
	i = setbituInc(buf,i,4,		ssrMeta.updateIntIndex);
	i = setbituInc(buf,i,1,		multipleMessage);
	i = setbituInc(buf,i,4,		ssrEph.iod);
	i = setbituInc(buf,i,16,	ssrMeta.provider);
	i = setbituInc(buf,i,4,		ssrMeta.solution);
	i = setbituInc(buf,i,1,		ssrMeta.referenceDatum);
	i = setbituInc(buf,i,6,		numSat);
	
	for (auto& [Sat, ssrOut] : orbClkMap)
	{
		auto& ssrEph = ssrOut.ssrEph;
		auto& ssrClk = ssrOut.ssrClk;
			
		i = setbituInc(buf,i, 6,	Sat.prn);
		i = setbituInc(buf,i, 8,	ssrEph.iode);
		
		int d;
		d = (int)round(ssrEph.deph[0]		/ 0.1e-3);				i = setbitsInc(buf,i,22,d);
		d = (int)round(ssrEph.deph[1]		/ 0.4e-3);				i = setbitsInc(buf,i,20,d);
		d = (int)round(ssrEph.deph[2]		/ 0.4e-3);				i = setbitsInc(buf,i,20,d);
		d = (int)round(ssrEph.ddeph[0]		/ 0.001e-3);			i = setbitsInc(buf,i,21,d); 
		d = (int)round(ssrEph.ddeph[1]		/ 0.004e-3);			i = setbitsInc(buf,i,19,d);    
		d = (int)round(ssrEph.ddeph[2]		/ 0.004e-3);			i = setbitsInc(buf,i,19,d);
		
		d = (int)round(ssrClk.dclk[0]		/ 0.1e-3);				i = setbitsInc(buf,i,22,d); 
		d = (int)round(ssrClk.dclk[1]		/ 0.001e-3);			i = setbitsInc(buf,i,21,d);  
		d = (int)round(ssrClk.dclk[2]		/ 0.00002e-3);			i = setbitsInc(buf,i,27,d);  
		
		last_clock[Sat] = ssrClk;
	}
	int bitl = byteLen*8-i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding combined.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
	}
	i = setbituInc(buf, i, bitl, 0); 
	
	return buffer;
}

vector<uint8_t>  encodeIGS_HRC( 
	map<SatSys, SSROut>&		orbClkMap,
	E_Sys						sys,
	bool						last)
{
	map<SatSys,double> hrClocks;
	for (auto& [Sat, ssrOut] : orbClkMap)
	{
		auto& ssrClk = ssrOut.ssrClk;
		
		if (last_clock.find(Sat) == last_clock.end())							continue;
		
		if (last_clock[Sat].iod != ssrOut.ssrClk.iod)						continue;
		
		double dt = (ssrClk.t0 - last_clock[Sat].t0).to_double();
		if (fabs(dt) > igsUpdateInterval[ssrClk.ssrMeta.updateIntIndex])		continue;
		
		hrClocks[Sat]	=  ssrClk.dclk[0]
						-(last_clock[Sat].dclk[0]
						+ last_clock[Sat].dclk[1]*dt
						+ last_clock[Sat].dclk[2]*dt*dt);	
	}
	
	int numSat = hrClocks.size();
	if (numSat == 0)
		return vector<uint8_t>();

	auto& [Sat, ssrOut1]	= *orbClkMap.begin();
	auto& ssrClk			= ssrOut1.ssrClk;
	auto& ssrMeta			= ssrClk.ssrMeta;
	
	int bitLen	= 78 + numSat * 28;
	int byteLen = ceil(bitLen/8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();
	
	unsigned int submessCode     = IGS_SSR_subtype (4, sys);
	unsigned int multipleMessage = last?0:1;
	
	int i = 0;
	i = setbituInc(buf,i,12,	4076);
	i = setbituInc(buf,i,3,	    1);
	i = setbituInc(buf,i,8,	    submessCode);
	i = setbituInc(buf,i,20,	ssrMeta.epochTime1s);
	i = setbituInc(buf,i,4,		ssrMeta.updateIntIndex);
	i = setbituInc(buf,i,1,		multipleMessage);
	i = setbituInc(buf,i,4,		ssrClk.iod);
	i = setbituInc(buf,i,16,	ssrMeta.provider);
	i = setbituInc(buf,i,4,		ssrMeta.solution);
	i = setbituInc(buf,i,6,		numSat);
	
	for (auto& [Sat, hrClk] : hrClocks)
	{
		i = setbituInc(buf,i, 6, Sat.prn);
		
		int d = (int)round(hrClk / 0.1e-3);
		i = setbitsInc(buf,i,22, d); 
	}
	int bitl = byteLen*8-i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding HR clock.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
	}
	i = setbituInc(buf, i, bitl, 0); 
	
	return buffer;
}


vector<uint8_t>  encodeIGS_COD( 
	map<SatSys, SSRCodeBias>&	codBiasMap,
	E_Sys						sys,
	bool						last)
{
	if (igsSSRCode2Index.find(sys) == igsSSRCode2Index.end())
		return vector<uint8_t>();
	
	int numSat = codBiasMap.size();
	
	if (numSat <= 0)
		return vector<uint8_t>(); 
	
	int totalNbias = 0;
	for (auto& [Sat,ssrCodeBias] : codBiasMap)
	for (auto it = ssrCodeBias.obsCodeBiasMap.begin(); it != ssrCodeBias.obsCodeBiasMap.end();)
	{
		auto obsCode = it->first;
		if (igsSSRCode2Index[sys].find(obsCode) == igsSSRCode2Index[sys].end())
		{
			it = ssrCodeBias.obsCodeBiasMap.erase(it);
		}
		else
		{
			it++;
			totalNbias++;
		}
	}

	if (totalNbias == 0)
		return vector<uint8_t>();  
	
	// Write the header information.
	auto s_it = codBiasMap.begin();
	auto& [Sat, ssrCodeBias] = *s_it;
	SSRMeta& ssrMeta = ssrCodeBias.ssrMeta;
	
	int bitLen = 78+numSat*11+totalNbias*19;
	int byteLen = ceil(bitLen/8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();
	
	unsigned int submessCode     = IGS_SSR_subtype (5, sys);
	unsigned int multipleMessage = last?0:1;
	
	int i = 0;
	i = setbituInc(buf,i,12,    4076);
	i = setbituInc(buf,i,3,     1);
	i = setbituInc(buf,i,8,     submessCode);
	i = setbituInc(buf,i,20,    ssrMeta.epochTime1s);
	i = setbituInc(buf,i,4,	    ssrMeta.updateIntIndex);
	i = setbituInc(buf,i,1,	    multipleMessage);
	i = setbituInc(buf,i,4,	    ssrCodeBias.iod);
	i = setbituInc(buf,i,16,    ssrMeta.provider);
	i = setbituInc(buf,i,4,	    ssrMeta.solution);
	i = setbituInc(buf,i,6,     numSat);
	
	for (auto& [sat, ssrCodeBias] : codBiasMap)
	{
		int nbias= ssrCodeBias.obsCodeBiasMap.size();
		
		i = setbituInc(buf,i,6, sat.prn);
		i = setbituInc(buf,i,5,	nbias);
		
		for (auto& [obsCode, entry] : ssrCodeBias.obsCodeBiasMap)
		{
			int rtcm_code = igsSSRCode2Index[sys][obsCode];
			int bia = (int)round(entry.bias / 0.01); 
			
			i = setbituInc(buf,i,5,	rtcm_code);
			i = setbitsInc(buf,i,14,bia);
		}
	} 
	
	int bitl = byteLen*8-i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding SSR Phase.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
	}
	
	i = setbituInc(buf,i,bitl,0);
	
	return buffer;
}

vector<uint8_t>  encodeIGS_PHS(
	map<SatSys, SSRPhasBias>&	ssrPBMap,
	E_Sys						sys,
	bool						last)
{
	if (igsSSRCode2Index.find(sys) == igsSSRCode2Index.end())
		return vector<uint8_t>();
	
	int numSat = ssrPBMap.size();
	
	if (numSat <= 0)
		return vector<uint8_t>(); 
	
	int totalNbias = 0;
	for (auto& [Sat,ssrPhasBias] : ssrPBMap)
	for (auto it = ssrPhasBias.obsCodeBiasMap.begin(); it != ssrPhasBias.obsCodeBiasMap.end();)
	{
		auto obsCode = it->first;
		if (igsSSRCode2Index[sys].find(obsCode) == igsSSRCode2Index[sys].end())
		{
			it = ssrPhasBias.obsCodeBiasMap.erase(it);
		}
		else
		{
			it++;
			totalNbias++;
		}
	}
	
	if (totalNbias == 0)
		return vector<uint8_t>();  
	
	// Write the header information.
	auto s_it = ssrPBMap.begin();
	auto& [Sat, ssrPhasBias] = *s_it;
	SSRMeta& ssrMeta = ssrPhasBias.ssrMeta;
	
	int bitLen = 80+numSat*28+totalNbias*32;
	int byteLen = ceil(bitLen/8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();
	
	unsigned int submessCode     = IGS_SSR_subtype (6, sys);
	unsigned int multipleMessage = last?0:1;
	
	int i = 0;
	i = setbituInc(buf,i,12,    4076);
	i = setbituInc(buf,i,3,     1);
	i = setbituInc(buf,i,8,     submessCode);
	i = setbituInc(buf,i,20,    ssrMeta.epochTime1s);
	i = setbituInc(buf,i,4,	    ssrMeta.updateIntIndex);
	i = setbituInc(buf,i,1,	    multipleMessage);
	i = setbituInc(buf,i,4,	    ssrPhasBias.iod);
	i = setbituInc(buf,i,16,    ssrMeta.provider);
	i = setbituInc(buf,i,4,	    ssrMeta.solution);
	i = setbituInc(buf,i,1,	    ssrPhasBias.ssrPhase.dispBiasConistInd);
	i = setbituInc(buf,i,1,	    ssrPhasBias.ssrPhase.MWConistInd);
	i = setbituInc(buf,i,6,     numSat);
	
	for (auto& [sat, ssrPhasBias] : ssrPBMap)
	{
		SSRPhase ssrPhase = ssrPhasBias.ssrPhase;
		int nbias = ssrPhasBias.obsCodeBiasMap.size();
		int yaw   = (int)round(ssrPhase.yawAngle* 256/PI);
		int rate  = (int)round(ssrPhase.yawRate	*8192/PI);
		
		i = setbituInc(buf,i,6,     sat.prn);
		i = setbituInc(buf,i,5,	    nbias);
		i = setbituInc(buf,i,9,	    yaw);
		i = setbitsInc(buf,i,8,	    rate);
		
		for (auto& [obsCode, entry] : ssrPhasBias.obsCodeBiasMap)
		{
			int rtcm_code = igsSSRCode2Index[sys][obsCode];
			int bias      = (int)round(entry.bias / 0.0001);
			SSRPhaseCh ssrPhaseCh = ssrPhasBias.ssrPhaseChs[obsCode];
			
			i = setbituInc(buf,i,5,	    rtcm_code);
			i = setbituInc(buf,i,1,	    ssrPhaseCh.signalIntInd);
			i = setbituInc(buf,i,2,	    ssrPhaseCh.signalWLIntInd);
			i = setbituInc(buf,i,4,	    ssrPhaseCh.signalDisconCnt);
			i = setbitsInc(buf,i,20,    bias);
		}
	} 
	
	int bitl = byteLen*8-i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding SSR Phase.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
	}
	
	i = setbituInc(buf,i,bitl,0);
	
	return buffer;
}

vector<uint8_t>  encodeIGS_URA( 
	map<SatSys, SSRUra>&		uraMap,
	E_Sys						sys,
	bool						last)
{
	int numSat = uraMap.size();
	if (numSat == 0)
	{
		return vector<uint8_t>();
	}
	
	auto& [Sat, ssrUra]	= *uraMap.begin();
	auto& ssrMeta		= ssrUra.ssrMeta;
	
	int bitLen	= 78 + numSat * 12;
	int byteLen = ceil(bitLen/8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();
	
	unsigned int submessCode     = IGS_SSR_subtype (7, sys);
	unsigned int multipleMessage = last?0:1;
	
	int i = 0;
	i = setbituInc(buf,i,12,	4076);
	i = setbituInc(buf,i,3,	    1);
	i = setbituInc(buf,i,8,	    submessCode);
	i = setbituInc(buf,i,20,	ssrMeta.epochTime1s);
	i = setbituInc(buf,i,4,		ssrMeta.updateIntIndex);
	i = setbituInc(buf,i,1,		multipleMessage);
	i = setbituInc(buf,i,4,		ssrUra.iod);
	i = setbituInc(buf,i,16,	ssrMeta.provider);
	i = setbituInc(buf,i,4,		ssrMeta.solution);
	i = setbituInc(buf,i,6,		numSat);
	
	for (auto& [Sat, satUra] : uraMap)
	{
		i = setbituInc(buf,i, 6, Sat.prn);
		
		int uraClass = uraToClassValue(satUra.ura);
		i = setbituInc(buf,i, 6, uraClass);
	}
	int bitl = byteLen*8-i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding URA.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
	}
	i = setbituInc(buf, i, bitl, 0); 
	
	return buffer;
}


vector<uint8_t>  encodeIGS_ATM(
	SSRAtm						ssrAtm,
	bool						last)
{
	vector<uint8_t> buffer;
	
	auto it = nav.ssrAtm.atmosGlobalMap.begin();
	if (it == nav.ssrAtm.atmosGlobalMap.end())
		return buffer;
	
	auto& [time, ssrGlobAtm] = *it;	
		
	if (ssrGlobAtm.numberLayers <= 0)
		return buffer;
	
	if	(  ssrGlobAtm.layers.size() > 4
		|| ssrGlobAtm.layers.size() != ssrGlobAtm.numberLayers)
	{
		return buffer;
	}
	int bitLen = 83+ssrGlobAtm.numberLayers*16;
	map<int,map<int,map<int,map<bool,double>>>> basisMaps;  // basis coefficients indexed by layer, degree, order and parity 
	for (auto& [ilay,vteclay] : ssrGlobAtm.layers)
	for (auto& [ibas,b] : vteclay.sphHarmonic)
	{
		basisMaps[ilay][b.degree][b.order][b.parity] = b.coeffc;
		bitLen += 16*(SQR(b.degree+1) - (b.degree-b.order)*(b.degree-b.order+1));	//todo aaron, check should be +1,0?
	}
	
	int byteLen = ceil(bitLen/8.0);
	
	buffer.resize(byteLen);
	unsigned char* buf = buffer.data();
	
	int multipleMessage = last?0:1;
	int VTECQuality = (int)round(ssrGlobAtm.vtecQuality/0.05);
	if (VTECQuality > 511)
		VTECQuality = 511;
	
	int i = 0;
	i = setbituInc(buf,i,12,	4076);
	i = setbituInc(buf,i,3,		1);
	i = setbituInc(buf,i,8,		201);
	i = setbituInc(buf,i,20,	ssrAtm.ssrMeta.epochTime1s);
	i = setbituInc(buf,i,4,		ssrAtm.ssrMeta.updateIntIndex);
	i = setbituInc(buf,i,1,		multipleMessage);
	i = setbituInc(buf,i,4,		ssrGlobAtm.iod);
	i = setbituInc(buf,i,16,	ssrAtm.ssrMeta.provider);
	i = setbituInc(buf,i,4,		ssrAtm.ssrMeta.solution);
	i = setbituInc(buf,i,9,		VTECQuality);
	i = setbituInc(buf,i,2,		ssrGlobAtm.layers.size()-1);
	
	for (auto& [ilay,vteclay] : ssrGlobAtm.layers)
	{
		int height = (int)round(vteclay.height/10);
		int ndegre = vteclay.maxDegree;
		int norder = vteclay.maxOrder;
		
		i = setbituInc(buf,i,20,height);
		i = setbituInc(buf,i,4,	ndegre-1);
		i = setbituInc(buf,i,4,	norder-1);
		
		for (int m = 0; m < norder; m++)
		for (int n = m; n < ndegre; n++)
		{
			int coefC = (int)round(basisMaps[ilay][n][m][true]/0.005);
			i = setbitsInc(buf,i,16, coefC);
		}
		
		for (int m = 1; m < norder; m++)
		for (int n = m; n < ndegre; n++)
		{
			int coefS = (int)round(basisMaps[ilay][n][m][false]/0.005);
			i = setbitsInc(buf,i,16, coefS);
		}
	}
	
	int bitl = byteLen*8-i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding SSR Ionosphere.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
	}
	
	i = setbituInc(buf,i,bitl,0);
	
	return buffer;
}
