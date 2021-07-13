#include <boost/log/trivial.hpp>

#include "rtcmEncoder.hpp"

using std::pair;

void RtcmEncoder::Encoder::encodeWriteMessages(std::ostream& outputStream)
{
	if ( outputStream.good() )
	{
		outputStream.write((const char*)&data[0],data.size());
		data.clear();
	}
}


void RtcmEncoder::Encoder::encodeWriteMessageToBuffer(
	unsigned char*	buf, 
	int				messLength)
{
	int i = 0;
	unsigned char nbuf[messLength+6];
	
	i = setbituInc(nbuf,i,8,	RTCM_PREAMBLE);
	i = setbituInc(nbuf,i,6,	0);
	i = setbituInc(nbuf,i,10,	messLength);

	memcpy(nbuf+3,buf,sizeof(uint8_t)*messLength);
	i = i + messLength*8;

	const unsigned char* bCrcBuf = (const unsigned char *)nbuf;
	unsigned int crcCalc = crc24q(bCrcBuf, sizeof(char)*(messLength+3));
	
	unsigned char* bCrcCalc = (unsigned char*)&crcCalc;
	unsigned int b1 = 0;
	unsigned int b2 = 0;
	unsigned int b3 = 0;
	setbituInc((unsigned char *)&b1,0,8,*(bCrcCalc+0));
	setbituInc((unsigned char *)&b2,0,8,*(bCrcCalc+1));
	setbituInc((unsigned char *)&b3,0,8,*(bCrcCalc+2));
	i = setbituInc(nbuf,i,8,b3);
	i = setbituInc(nbuf,i,8,b2);
	i = setbituInc(nbuf,i,8,b1);
	
	data.insert(data.end(), &nbuf[0], &nbuf[messLength+6]);
}

void RtcmEncoder::SSREncoder::encodeSsr(bool useSSROut)
{
	encodeSsrComb	(E_Sys::GPS, useSSROut);
	encodeSsrComb	(E_Sys::GAL, useSSROut);    
	encodeSsrPhase	(E_Sys::GPS, useSSROut);
	encodeSsrPhase	(E_Sys::GAL, useSSROut);    
	encodeSsrCode	(E_Sys::GPS, useSSROut);
	encodeSsrCode	(E_Sys::GAL, useSSROut);    
}


void RtcmEncoder::SSREncoder::encodeSsrComb(
	E_Sys	targetSys,
	bool	useSSROut)
{
	map<GTime, map<SatSys, SSREph>> ssrOrbMap;
	
	for (auto& [satId, satNav] : nav.satNavMap)
	{
		SatSys Sat(satId);
		
		if (Sat.sys != targetSys)
			continue;
		
		SSREph ssrEph;
		if (useSSROut)
		{
			if (!satNav.ssrOut.ssrEph.canExport)
				continue;
			ssrEph = satNav.ssrOut.ssrEph;
			satNav.ssrOut.ssrEph.canExport = false;
		}
		else
		{
			if (satNav.ssr.ssrEph_map.size() == 0)
			{
				continue;
			}
			ssrEph = satNav.ssr.ssrEph_map.begin()->second;
		}
		
		if (ssrEph.iod == -1)
			continue;
		
		ssrOrbMap[ssrEph.t0][Sat] = ssrEph;
	}

	map<GTime, map<SatSys, SSRClk>> ssrClkMap;
	
	for (auto& [satId, satNav] : nav.satNavMap)
	{
		SatSys Sat(satId);
		
		if (Sat.sys != targetSys)
			continue;
	
		SSRClk ssrClk;
		
		if (useSSROut)
		{
			SSROut& ssrOut = satNav.ssrOut;
			
			if (ssrOut.ssrClk.canExport == false)
				continue;
			
			ssrClk = ssrOut.ssrClk;
			ssrOut.ssrClk.canExport = false;
		}
		else
		{
			ssrClk = satNav.ssr.ssrClk_map.begin()->second;
		}
		
		if (ssrClk.iod == -1)
			continue;
		
		ssrClkMap[ssrClk.t0][Sat] = ssrClk;
	}
	
	map<GTime, map<SatSys, pair<SSREph, SSRClk>>> ssrCombMap;

	auto it = ssrOrbMap.begin();
	while(it != ssrOrbMap.end())
	{
		auto t0			= it->first;
		auto s_EphMap	= it->second;
		bool foundComb = false;

		if (ssrClkMap.find(t0) != ssrClkMap.end())
		{  
			auto s_ClkMap = ssrClkMap[t0];
			auto s_Comb = ssrCombMap[t0];
			vector<SatSys> obsToRem;
			for (auto&[Sat,ssrEph] : s_EphMap )
			{
				if (s_ClkMap.find(Sat) != s_ClkMap.end())
				{  
					// Copy objects back into map structure.
					pair<SSREph,SSRClk> obPair(ssrEph,s_ClkMap[Sat]);
					s_Comb[Sat] = obPair;
					ssrCombMap[t0] = s_Comb;
	
					obsToRem.push_back(Sat);
				}
			}
			for (auto Sat : obsToRem)
			{
				s_EphMap.erase(Sat);
				s_ClkMap.erase(Sat);
			}
			
			if ( s_EphMap.size() != 0 )
			{
				ssrOrbMap[t0] = s_EphMap;
			}
			else
			{
				ssrOrbMap.erase(t0);
			}
			
			if( s_ClkMap.size() != 0 )
			{
				ssrClkMap[t0] = s_ClkMap;
			}
			else
			{
				ssrClkMap.erase(t0);
			}
			
			foundComb = true;
			break;
		}
		
		if ( foundComb )
			it = ssrOrbMap.begin();
		else
			it++;
	}

	/* At this point ssrOrbMap, has the singular non-matched SSREph.
	* ssrClkMap, has the singular non-match SSRClk.
	* ssrCombMap, has the match pairs of SSREph and SSRClk.
	* 
	* Currently the only messages encoded are the matched pairs.
	*/

	for (auto& [t0, s_Comb] : ssrCombMap)
	{		
		int numSat = s_Comb.size();
		
		int i = 0;
		int bitLen = 0;

		// Write the header information.

		int np = 0;
		int ni = 0;
		int nj = 0;
		int offp = 0;
		unsigned int messCode = 0;
		if (targetSys == +E_Sys::GPS) 
		{
			messCode = +RtcmMessageType::GPS_SSR_COMB_CORR;
			np=6; ni= 8; nj= 0; offp=  0;
			bitLen = 68+numSat*205;
		}
		if (targetSys == +E_Sys::GAL)
		{
			messCode = +RtcmMessageType::GAL_SSR_COMB_CORR;
			np=6; ni=10; nj= 0; offp=  0;
			bitLen = 68+numSat*207;
		}
		
		int byteLen = ceil(bitLen/8.0);
		unsigned char buf[byteLen];

		auto s_it = s_Comb.begin();
		auto CombPair = s_it->second;
		SSREph ssrEph = CombPair.first;
		
		SSRMeta ssrMeta = ssrEph.ssrMeta;
		
		i = setbituInc(buf,i,12,	messCode);
		i = setbituInc(buf,i,20,	ssrMeta.epochTime1s);
		i = setbituInc(buf,i,4,		ssrMeta.ssrUpdateIntIndex);
		i = setbituInc(buf,i,1,		ssrMeta.multipleMessage);
		i = setbituInc(buf,i,1,		ssrMeta.referenceDatum);
		i = setbituInc(buf,i,4,		ssrEph.iod);
		i = setbituInc(buf,i,16,	ssrMeta.provider);
		i = setbituInc(buf,i,4,		ssrMeta.solution);
		i = setbituInc(buf,i,6,		numSat);
		
		for(auto& [Sat, comb] : s_Comb)
		{
			auto& [ssrEph, ssrClk] = comb;
				
			i = setbituInc(buf,i,np,Sat.prn);
			i = setbituInc(buf,i,ni,ssrEph.iode);
			
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

			traceSsrEph(Sat,ssrEph);
			traceSsrClk(Sat,ssrClk);
		}
		int bitl = byteLen*8-i;
		if (bitl > 7 )
		{
			BOOST_LOG_TRIVIAL(error) << "Error encoding combined.\n";
			BOOST_LOG_TRIVIAL(error) << "bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
		}
		i = setbituInc(buf,i,bitl,0); 
		
		encodeWriteMessageToBuffer(buf, byteLen);
	}
}

void RtcmEncoder::SSREncoder::encodeSsrPhase(
	E_Sys	targetSys, 
	bool	useSSROut)
{
	map<GTime, map<SatSys, SSRPhasBias>> ssrPBMap;
	
	for (auto& [satId, satNav] : nav.satNavMap)
	{
		SatSys Sat(satId);
		if (Sat.sys != targetSys)
			continue;
			
		SSRPhasBias ssrPhasBias;
		if (useSSROut)
		{
			SSROut& ssrOut = satNav.ssrOut;
			
			if (ssrOut.ssrPhasBias.canExport == false)
				continue;
			
			ssrOut.ssrPhasBias.canExport = false;
			
			ssrPhasBias = ssrOut.ssrPhasBias;
		}
		else
		{
			ssrPhasBias = satNav.ssr.ssrPhasBias_map.begin()->second;
		}
		
		if	(  ssrPhasBias.t0.time	== 0 
			|| ssrPhasBias.iod		== -1 )
		{
			continue;
		}
		
		GTime t0 = ssrPhasBias.t0;
		
		ssrPBMap[t0][Sat] = ssrPhasBias;
	}
	
	for (auto& [t0, s_PBMap] : ssrPBMap)
	{
		int numSat = s_PBMap.size();
		
		int i = 0;
		int bitLen = 0;

		int totalNbias = 0;
		for (auto& [Sat,ssrPhasBias] : s_PBMap)
		{
			totalNbias += ssrPhasBias.bias.size();
		}
		
		if (totalNbias == 0)
			break;
		
		// Write the header information.

		int np = 0;
		int ni = 0;
		int nj = 0;
		int offp = 0;
		unsigned int messCode = 0;
		if (targetSys == +E_Sys::GPS) 
		{
			messCode = +RtcmMessageType::GPS_SSR_PHASE_BIAS;
			np=6; ni= 8; nj= 0; offp=  0;
			bitLen = 69+numSat*28+totalNbias*32;
		}
		if (targetSys == +E_Sys::GAL)
		{
			messCode = +RtcmMessageType::GAL_SSR_PHASE_BIAS;
			np=6; ni=10; nj= 0; offp=  0;
			bitLen = 69+numSat*28+totalNbias*32;
		}
		
		int byteLen = ceil(bitLen/8.0);
		unsigned char buf[byteLen];

		auto s_it = s_PBMap.begin();
		auto ssrPhasBias = s_it->second;
		SSRMeta ssrMeta = ssrPhasBias.ssrMeta;
		
		i = setbituInc(buf,i,12,messCode);
		i = setbituInc(buf,i,20,ssrMeta.epochTime1s);
		i = setbituInc(buf,i,4,	ssrMeta.ssrUpdateIntIndex);
		i = setbituInc(buf,i,1,	ssrMeta.multipleMessage);
		
		i = setbituInc(buf,i,4,	ssrPhasBias.iod);
		i = setbituInc(buf,i,16,ssrMeta.provider);
		i = setbituInc(buf,i,4,	ssrMeta.solution);
	
		i = setbituInc(buf,i,1,	ssrPhasBias.ssrPhase.dispBiasConistInd);
		i = setbituInc(buf,i,1,	ssrPhasBias.ssrPhase.MWConistInd); 
		
		i = setbituInc(buf,i,6,numSat);
		
		for (auto& [Sat, ssrPhasBias] : s_PBMap)
		{
			SSRPhase ssrPhase = ssrPhasBias.ssrPhase;
			
			int d;
														i = setbituInc(buf,i,np,Sat.prn);
			d = ssrPhasBias.bias.size();				i = setbituInc(buf,i,5,	d);
			d = (int)round(ssrPhase.yawAngle*256);		i = setbituInc(buf,i,9,	d);
			d = (int)round(ssrPhase.yawRate*8192);		i = setbitsInc(buf,i,8,	d);
			
			for (auto& [obCode, bias] : ssrPhasBias.bias)
			{
				SSRPhaseCh ssrPhaseCh = ssrPhasBias.ssrPhaseChs[obCode];
				
				//BOOST_LOG_TRIVIAL(debug) << "Phase, obCode : " << obCode << std::endl;
				//BOOST_LOG_TRIVIAL(debug) << "E_sys         : " << sys << std::endl;
				//BOOST_LOG_TRIVIAL(debug) << "mCodes_gps.size() : " << mCodes_gps.size() << std::endl;
				//print_map( mCodes_gps.left, " E_ObsCode <--> RTCM ", BOOST_LOG_TRIVIAL(debug) );
				
				int rtcm_code = 0;
				if ( targetSys == +E_Sys::GPS )
					rtcm_code = mCodes_gps.left.at(obCode);
				else if ( targetSys == +E_Sys::GAL )
					rtcm_code = mCodes_gal.left.at(obCode); 
				
				//BOOST_LOG_TRIVIAL(debug) << "rtcm_code      : " << rtcm_code << std::endl;
				
														i = setbituInc(buf,i,5,	rtcm_code);
														i = setbituInc(buf,i,1,	ssrPhaseCh.signalIntInd);
														i = setbituInc(buf,i,2,	ssrPhaseCh.signalWidIntInd);
														i = setbituInc(buf,i,4,	ssrPhaseCh.signalDisconCnt);
				d = (int)round(bias/0.0001);			i = setbitsInc(buf,i,20,d);
				
				traceSsrPhasB(Sat, obCode, ssrPhasBias);   
			}
		} 
		
		int bitl = byteLen*8-i;
		if (bitl > 7 )
		{
			BOOST_LOG_TRIVIAL(error) << "Error encoding SSR Phase.\n";
			BOOST_LOG_TRIVIAL(error) << "bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
		}
		
		i = setbituInc(buf,i,bitl,0);
		
		encodeWriteMessageToBuffer(buf,byteLen);
	}
}


void RtcmEncoder::SSREncoder::encodeSsrCode(
	E_Sys	targetSys, 
	bool	useSSROut)
{	
	map<GTime, map<SatSys, SSRCodeBias>> ssrCBMap;
	
	for (auto& [satId, satNav] : nav.satNavMap)
	{
		SatSys Sat(satId);
		if	(Sat.sys != targetSys)
			continue;
		
		SSRCodeBias ssrCodeBias;
		if (useSSROut)
		{
			SSROut& ssrOut = satNav.ssrOut;
			
			if (ssrOut.ssrCodeBias.canExport == false)
				continue;
			
			ssrCodeBias = ssrOut.ssrCodeBias;
			ssrOut.ssrCodeBias.canExport = false;
		}
		else
		{
			ssrCodeBias = satNav.ssr.ssrCodeBias_map.begin()->second; 
		}
		
		if	(  ssrCodeBias.t0.time	== 0  
			|| ssrCodeBias.iod		== -1)
		{
			continue;
		}
		
		GTime t0 = ssrCodeBias.t0;
		
		ssrCBMap[t0][Sat] = ssrCodeBias;
	}
	
	for(auto& [t0, s_CBMap] : ssrCBMap)
	{
		int numSat = s_CBMap.size();
		
		int i = 0;
		int bitLen = 0;

		int totalNbias = 0;
		for (auto s_it = s_CBMap.begin(); s_it != s_CBMap.end(); s_it++)
		{
			SSRCodeBias ssrCodeBias = s_it->second;
			totalNbias += ssrCodeBias.bias.size();
		}
		
		if (totalNbias == 0)
			break;
		
		// Write the header information.

		int np = 0;
		int ni = 0;
		int nj = 0;
		int offp = 0;
		unsigned int messCode = 0;
		if (targetSys == +E_Sys::GPS) 
		{
			messCode = +RtcmMessageType::GPS_SSR_CODE_BIAS;
			np=6; ni= 8; nj= 0; offp=  0;
			bitLen = 67+numSat*11+totalNbias*19;
		}
		if (targetSys == +E_Sys::GAL)
		{
			messCode = +RtcmMessageType::GAL_SSR_CODE_BIAS;
			np=6; ni=10; nj= 0; offp=  0;
			bitLen = 67+numSat*11+totalNbias*19;
		}
		
		int byteLen = ceil(bitLen/8.0);
		unsigned char buf[byteLen];

		auto s_it = s_CBMap.begin();
		auto& ssrCodeBias = s_it->second;
		
		SSRMeta& ssrMeta = ssrCodeBias.ssrMeta;
		
		i = setbituInc(buf,i,12,messCode);
		i = setbituInc(buf,i,20,ssrMeta.epochTime1s);
		i = setbituInc(buf,i,4,	ssrMeta.ssrUpdateIntIndex);
		i = setbituInc(buf,i,1,	ssrMeta.multipleMessage);
		
		i = setbituInc(buf,i,4,	ssrCodeBias.iod);
		i = setbituInc(buf,i,16,ssrMeta.provider);
		i = setbituInc(buf,i,4,	ssrMeta.solution);
		i = setbituInc(buf,i,6,	numSat);
	
		for (auto& [Sat, ssrCodeBias] : s_CBMap)
		{
			i = setbituInc(buf, i, np, Sat.prn);
			unsigned int nbias = ssrCodeBias.bias.size();

			i = setbituInc(buf,i,5,nbias);
			
			for (auto& [obCode, bias] : ssrCodeBias.bias)
			{
				int rtcm_code = 0;
				if		( targetSys == +E_Sys::GPS )	{	rtcm_code = mCodes_gps.left.at(obCode);		}
				else if ( targetSys == +E_Sys::GAL )	{	rtcm_code = mCodes_gal.left.at(obCode);		}
				

														i = setbituInc(buf, i, 5,		rtcm_code);
				int d = (int)round(bias / 0.01);		i = setbitsInc(buf, i, 14,	d);

				traceSsrCodeB(Sat, obCode, ssrCodeBias);                  
			}
		}
		
		int bitl = byteLen*8-i;
		if (bitl > 7 )
		{
			BOOST_LOG_TRIVIAL(error) << "Error encoding SSR Code.\n";
			BOOST_LOG_TRIVIAL(error) << "bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << std::endl;
		}
		i = setbituInc(buf,i,bitl,0);
		
		encodeWriteMessageToBuffer(buf,byteLen);
	}
}
