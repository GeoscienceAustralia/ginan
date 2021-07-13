#include "acsStream.hpp"



void NtripTrace::traceSsrEph(SatSys Sat,SSREph ssrEph)
{
	std::ostream oSsrEph(&ssrEphBuf);
	
	char str[32];
	time2str(ssrEph.t0, str, 2); 

	tracepdeex(5,oSsrEph,"%s %s ",Sat.id(),str);
	tracepdeex(5,oSsrEph,"% 14d ",ssrEph.iod);
	tracepdeex(5,oSsrEph,"% 14d ",ssrEph.iode);      
	tracepdeex(5,oSsrEph,"% 14.6f ",ssrEph.deph[0]);
	tracepdeex(5,oSsrEph,"% 14.6f ",ssrEph.deph[1]);
	tracepdeex(5,oSsrEph,"% 14.6f ",ssrEph.deph[2]);
	tracepdeex(5,oSsrEph,"% 14.6f ",ssrEph.ddeph[0]);
	tracepdeex(5,oSsrEph,"% 14.6f ",ssrEph.ddeph[1]);
	tracepdeex(5,oSsrEph,"% 14.6f",ssrEph.ddeph[2]);
	oSsrEph << str << std::endl;
}

void NtripTrace::traceSsrClk(SatSys Sat,SSRClk ssrClk)
{
	std::ostream oSsrClk(&ssrClkBuf);
	
	char str[32];
	time2str(ssrClk.t0, str, 2); 
	
	tracepdeex(5,oSsrClk,"%s %s ",Sat.id(),str);
	tracepdeex(5,oSsrClk,"% 14d ",ssrClk.iod);
	tracepdeex(5,oSsrClk,"% 14.6f ",ssrClk.dclk[0]);
	tracepdeex(5,oSsrClk,"% 14.6f ",ssrClk.dclk[1]);
	tracepdeex(5,oSsrClk,"% 14.6f\n",ssrClk.dclk[2]);
}

void NtripTrace::traceSsrCodeB(SatSys Sat, E_ObsCode mode, SSRCodeBias ssrBias)
{
std::ostream oSsrCoB(&ssrCoBBuf);
	
	double ep[6];
	int yds[3];
	
	time2epoch(ssrBias.t0, ep);
	epoch2yds(ep, yds);            
	tracepdeex(5,oSsrCoB,"%s\t%s\t%d:%d:%d\t", Sat.id(), mode, yds[0], yds[1], yds[2]);
	tracepdeex(5,oSsrCoB,"% 8.4f\n", ssrBias.bias[mode]);
}

void NtripTrace::traceSsrPhasB(SatSys Sat,E_ObsCode mode, SSRPhasBias ssrBias)
{
	std::ostream oSsrPhB(&ssrPhBBuf);
	
	double ep[6];
	int yds[3];
	
	time2epoch(ssrBias.t0, ep);
	epoch2yds(ep, yds);
	tracepdeex(5,oSsrPhB,"%s\t%s\t%d:%d:%d\t", Sat.id(), mode, yds[0], yds[1], yds[2]);
	tracepdeex(5,oSsrPhB,"% 8.4f\n", ssrBias.bias[mode]);
}

void NtripTrace::traceBroEph(Eph eph,E_Sys sys)
{
	std::ostream oBroEph(&broEphBuf);
	
	double ep[6];
	time2epoch(eph.toc, ep);
	
	//Note the Satellite id is not set in rinex correctly as we a mixing GNSS systems.
	oBroEph << eph.Sat.id();
	tracepdeex(5,oBroEph,"%5i",(int)ep[0]);
	tracepdeex(5,oBroEph,"%5i",(int)ep[0]);
	tracepdeex(5,oBroEph,"%3i",(int)ep[1]);
	tracepdeex(5,oBroEph,"%3i",(int)ep[2]);
	tracepdeex(5,oBroEph,"%3i",(int)ep[3]);
	tracepdeex(5,oBroEph,"%3i",(int)ep[4]);
	tracepdeex(5,oBroEph,"%3i",(int)ep[5]);
	tracepdeex(5,oBroEph,"% 19.12e",eph.f0);
	tracepdeex(5,oBroEph,"% 19.12e",eph.f1);
	tracepdeex(5,oBroEph,"% 19.12e",eph.f2);
	oBroEph << std::endl; 
		
	if (sys==+E_Sys::GPS )
	{
		// https://cddis.nasa.gov/archive/gnss/data/daily/2021/102/21n/
		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",(double)eph.iode);
		tracepdeex(5,oBroEph,"% 19.12e",eph.crs);
		tracepdeex(5,oBroEph,"% 19.12e",eph.deln);
		tracepdeex(5,oBroEph,"% 19.12e",eph.M0);
		oBroEph << std::endl;
		
		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",eph.cuc);
		tracepdeex(5,oBroEph,"% 19.12e",eph.e);
		tracepdeex(5,oBroEph,"% 19.12e",eph.cus);
		tracepdeex(5,oBroEph,"% 19.12e",SQRT(eph.A));
		oBroEph << std::endl;            

		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",eph.toes);
		tracepdeex(5,oBroEph,"% 19.12e",eph.cic);
		tracepdeex(5,oBroEph,"% 19.12e",eph.OMG0);
		tracepdeex(5,oBroEph,"% 19.12e",eph.cis);
		oBroEph << std::endl;         
		
		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",eph.i0);
		tracepdeex(5,oBroEph,"% 19.12e",eph.crc);
		tracepdeex(5,oBroEph,"% 19.12e",eph.omg);
		tracepdeex(5,oBroEph,"% 19.12e",eph.OMGd);
		oBroEph << std::endl;             

		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",eph.idot);
		tracepdeex(5,oBroEph,"% 19.12e",(double)eph.code);
		tracepdeex(5,oBroEph,"% 19.12e",(double)eph.week);
		tracepdeex(5,oBroEph,"% 19.12e",(double)eph.flag);
		oBroEph << std::endl;            
	
		/*
			GLOBAL POSITIONING SYSTEM 
			STANDARD POSITIONING SERVICE
			SIGNAL SPECIFICATION
			2nd Ed, June 2,1995
			see section - 2.5.3 User Range Accuracy
		*/
		double ura = 0;
		if (eph.sva <= 6)
		{
			ura = 10*pow(2, 1+((double)eph.sva/2.0));
			ura = round(ura)/10.0;
		}
		else if ( eph.sva != 15 )
			ura = pow(2,(double)eph.sva-2.0);
		else
			ura = -1;
		
		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",ura);
		tracepdeex(5,oBroEph,"% 19.12e", (double)eph.svh);
		tracepdeex(5,oBroEph,"% 19.12e",eph.tgd[0]);
		tracepdeex(5,oBroEph,"% 19.12e",(double)eph.iodc);
		oBroEph << std::endl;

		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",time2gpst(eph.ttr,&eph.week));
		tracepdeex(5,oBroEph,"% 19.12e",eph.fit);
		oBroEph << std::endl;
	}
	else if (sys==+E_Sys::GAL) 
	{
		// https://cddis.nasa.gov/archive/gnss/data/daily/2021/102/21l/
		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",(double)eph.iode);
		tracepdeex(5,oBroEph,"% 19.12e",eph.crs);
		tracepdeex(5,oBroEph,"% 19.12e",eph.deln);
		tracepdeex(5,oBroEph,"% 19.12e",eph.M0);
		oBroEph << std::endl;

		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",eph.cuc);
		tracepdeex(5,oBroEph,"% 19.12e",eph.e);
		tracepdeex(5,oBroEph,"% 19.12e",eph.cus);
		tracepdeex(5,oBroEph,"% 19.12e",SQRT(eph.A));
		oBroEph << std::endl;  
		
		oBroEph << "    ";            
		tracepdeex(5,oBroEph,"% 19.12e",eph.toes);      
		tracepdeex(5,oBroEph,"% 19.12e",eph.cic);
		tracepdeex(5,oBroEph,"% 19.12e",eph.OMG0);
		tracepdeex(5,oBroEph,"% 19.12e",eph.cis);
		oBroEph << std::endl; 
		
		
		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",eph.i0);
		tracepdeex(5,oBroEph,"% 19.12e",eph.crc);
		tracepdeex(5,oBroEph,"% 19.12e",eph.omg);
		tracepdeex(5,oBroEph,"% 19.12e",eph.OMGd);
		oBroEph << std::endl; 
		
		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",eph.idot);
		tracepdeex(5,oBroEph,"% 19.12e",(double)eph.code);
		tracepdeex(5,oBroEph,"% 19.12e",(double)eph.week);
		oBroEph << std::endl;

		/*
			EUROPEAN GNSS (GALILEO) OPEN SERVICE
			SIGNAL-IN-SPACE
			INTERFACE CONTROL
			DOCUMENT
			Issue 2.0, January 2021
			See Section, 5.1.12. Signal – In – Space Accuracy (SISA)
		*/
		double SISA;
		if		( eph.sva <= 49 )			SISA = 0.0+(eph.sva-0)	* 0.01;
		else if ( eph.sva <= 74 )			SISA = 0.5+(eph.sva-50)	* 0.02;
		else if ( eph.sva <= 99 )			SISA = 1.0+(eph.sva-75)	* 0.04;
		else if ( eph.sva <= 125 )			SISA = 2.0+(eph.sva-100)* 0.16;
		else								SISA = -1;
		
		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",SISA);
		tracepdeex(5,oBroEph,"% 19.12e",(double)eph.svh);
		tracepdeex(5,oBroEph,"% 19.12e",eph.tgd[0]);
		tracepdeex(5,oBroEph,"% 19.12e",eph.tgd[1]);
		oBroEph << std::endl;
		
		oBroEph << "    ";
		tracepdeex(5,oBroEph,"% 19.12e",time2gpst(eph.ttr,&eph.week));
	}
}

void NtripTrace::traceWriteEpoch(
	Trace& trace)
{
	if (netConnBuf.size() != 0)
	{
		std::istream oNetConn(&netConnBuf);
		tracepde(3,trace, "NTRIP Stream Connection Data.\n");
		
		std::string messLine;
		while(oNetConn)
		{
			std::getline(oNetConn,messLine);
			if ( messLine.length() != 0 )
			{
				tracepde(3,trace,messLine+"\n");
				//BOOST_LOG_TRIVIAL(debug) << mountPoint << " - " << messLine << std::endl;
			}
		}
	}

	if (messErrChunkBuf.size() != 0)
	{
		std::istream oErrChunk(&messErrChunkBuf);
		tracepde(3,trace, "NTRIP V2 Data Chunk Errors.\n");
		
		std::string messLine;
		std::string lastLine;
		while (oErrChunk)
		{
			std::getline(oErrChunk,messLine);
			if ( messLine.length() != 0 )
			{
				tracepde(5,trace,messLine+"\n");
				lastLine = messLine;
			}
		}
		//BOOST_LOG_TRIVIAL(debug) << mountPoint << " - " << lastLine << std::endl;
		if ( level_trace < 5 )
		tracepde(3,trace,lastLine+"\n"); 
	} 
	
	if (messErrRtcmBuf.size() != 0)
	{
		std::istream oErrRTCM(&messErrRtcmBuf);
		tracepde(3,trace, "RTCM Dataframe Errors.\n");
		
		std::string messLine;
		std::string lastLine;
		while(oErrRTCM)
		{
			std::getline(oErrRTCM,messLine);
			if ( messLine.length() != 0 )
			{
				tracepde(5,trace,messLine+"\n");
				lastLine = messLine;
			}
		}
		//BOOST_LOG_TRIVIAL(debug) << mountPoint << " - " << lastLine << std::endl;
		if ( level_trace < 5 )
		tracepde(3,trace,lastLine+"\n"); 
	}

	if (messErrRtcmByteBuf.size() != 0)
	{
		std::istream oErrRtcmBtye(&messErrRtcmByteBuf);
		tracepde(3,trace, "RTCM Extra Byte Dataframe Errors.\n");
		
		std::string messLine;
		std::string lastLine;
		while(oErrRtcmBtye)
		{
			std::getline(oErrRtcmBtye,messLine);
			if ( messLine.length() != 0 )
			{
				tracepde(5,trace,messLine+"\n");
				lastLine = messLine;
			}
		}
		//BOOST_LOG_TRIVIAL(debug) << mountPoint << " - " << lastLine << std::endl;
		if ( level_trace < 5 )
		tracepde(3,trace,lastLine+"\n"); 
	}     



	if (broEphBuf.size() != 0)
	{
		std::istream oBroEph(&broEphBuf);
		tracepde(5,trace, "NTRIP RTCM Broadcast Ephemeris Messages, in order of arrival/output.\n");
		
		std::string messLine;
		while(oBroEph)
		{
			std::getline(oBroEph,messLine);
			tracepde(5,trace,messLine+"\n"); 
		}
	}
	
	if (ssrEphBuf.size() != 0)
	{
		std::istream oSsrEph(&ssrEphBuf);
		tracepde(5,trace, "NTRIP RTCM SSR Ephemeris Messages, in order of arrival/output.\n");
		
		std::string messLine;
		while(oSsrEph)
		{
			std::getline(oSsrEph,messLine);
			tracepde(5,trace,messLine+"\n"); 
		}
	}  
	
	if (ssrClkBuf.size() != 0)
	{
		std::istream oSsrClk(&ssrClkBuf);
		tracepde(5,trace, "NTRIP RTCM SSR Clock Bias Messages, in order of arrival/output.\n");
		
		std::string messLine;
		while(oSsrClk)
		{
			std::getline(oSsrClk,messLine);
			tracepde(5,trace,messLine+"\n"); 
		}
	}
	
	if (ssrCoBBuf.size() != 0)
	{
		std::istream oSsrCoB(&ssrCoBBuf);
		tracepde(5,trace, "NTRIP RTCM SSR Code Bias Messages, in order of arrival/output.\n");
		
		std::string messLine;
		while(oSsrCoB)
		{
			std::getline(oSsrCoB,messLine);
			tracepde(5,trace,messLine+"\n"); 
		}
	} 
	
	if (ssrPhBBuf.size() != 0)
	{
		std::istream oSsrPhB(&ssrPhBBuf);
		tracepde(5,trace, "NTRIP RTCM SSR Phase Bias Messages, in order of arrival/output.\n");
		
		std::string messLine;
		while(oSsrPhB)
		{
			std::getline(oSsrPhB,messLine);
			tracepde(5,trace,messLine+"\n"); 
		}
	}
}
