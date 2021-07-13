
#include "navigation.hpp"
#include "acsStream.hpp"
#include "acsConfig.hpp"
#include "enums.h"
#include "rtcmEncoder.hpp"
#include <map>
#include <boost/utility/binary.hpp>
#include <boost/filesystem.hpp>


static int rtcmdeblvl = 3;

ObsList ObsStream::getObs()
{
	if (obsListList.size() > 0)
	{
		ObsList& obsList = obsListList.front();

		std::sort(obsList.begin(), obsList.end(), [](Obs& a, Obs& b)
			{
				if (a.Sat.sys < b.Sat.sys)		return true;
				if (a.Sat.sys > b.Sat.sys)		return false;
				if (a.Sat.prn < b.Sat.prn)		return true;
				else							return false;
			});

		for (auto& obs					: obsList)
		for (auto& [ftype, sigsList]	: obs.SigsLists)
		{
			sigsList.sort([](RawSig& a, RawSig& b)
				{
					auto iterA = std::find(acsConfig.code_priorities.begin(), acsConfig.code_priorities.end(), a.code);
					auto iterB = std::find(acsConfig.code_priorities.begin(), acsConfig.code_priorities.end(), b.code);

					if (a.L == 0)		return false;
					if (b.L == 0)		return true;
					if (a.P == 0)		return false;
					if (b.P == 0)		return true;
					if (iterA < iterB)	return true;
					else				return false;
				});

			RawSig firstOfType = sigsList.front();

			//use first of type as representative if its in the priority list
			auto iter = std::find(acsConfig.code_priorities.begin(), acsConfig.code_priorities.end(), firstOfType.code);
			if (iter != acsConfig.code_priorities.end())
			{
				obs.Sigs[ftype] = Sig(firstOfType);
			}
		}

		return obsList;
	}
	else
	{
		return ObsList();
	}
}









const int DefGLOChnl [24] =
{ 1, -4, 5, 6, 1, -4, 5, 6, -2, -7, 0, -1, -2, -7, 0, -1, 4, -3, 3, 2, 4, -3, 3, 2 };

// sys -> rtcm signal enum -> siginfo (sig enum,
map<E_Sys, map<uint8_t, SignalInfo>> signal_id_mapping =
{
	{	E_Sys::GPS,
		{
			{2,		{2,		L1,		E_ObsCode::L1C}},
			{3,		{3,		L1,		E_ObsCode::L1P}},
			{4,		{4,		L1,		E_ObsCode::L1W}},

			{8,		{8,		L2,		E_ObsCode::L2C}},
			{9,		{9,		L2,		E_ObsCode::L2P}},
			{10,	{10,	L2,		E_ObsCode::L2W}},
			{15,	{15,	L2,		E_ObsCode::L2S}},
			{16,	{16,	L2,		E_ObsCode::L2L}},
			{17,	{17,	L2,		E_ObsCode::L2X}},

			{22,	{22,	L5,		E_ObsCode::L5I}},
			{23,	{23,	L5,		E_ObsCode::L5Q}},
			{24,	{24,	L5,		E_ObsCode::L5X}},

			{30,	{2,		L1,		E_ObsCode::L1S}},
			{31,	{2,		L1,		E_ObsCode::L1L}},
			{32,	{2,		L1,		E_ObsCode::L1X}}
		}
	},

	{	E_Sys::GLO,
		{
			{2,		{2,		G1,		E_ObsCode::L1C}},
			{3,		{3,		G1,		E_ObsCode::L1P}},

			{8,		{8,		G2,		E_ObsCode::L2C}},
			{9,		{9,		G2,		E_ObsCode::L2P}}
		}
	},

	{	E_Sys::GAL,
		{
			{2,		{2,		E1,		E_ObsCode::L1C}},
			{3,		{3,		E1,		E_ObsCode::L1A}},
			{4,		{4,		E1,		E_ObsCode::L1B}},
			{5,		{5,		E1,		E_ObsCode::L1X}},
			{6,		{6,		E1,		E_ObsCode::L1Z}},

			{8,		{8,		E6,		E_ObsCode::L6C}},
			{9,		{9,		E6,		E_ObsCode::L6A}},
			{10,	{10,	E6,		E_ObsCode::L6B}},
			{11,	{11,	E6,		E_ObsCode::L6X}},
			{12,	{12,	E6,		E_ObsCode::L6Z}},

			{14,	{14,	E5B,	E_ObsCode::L7I}},
			{15,	{15,	E5B,	E_ObsCode::L7Q}},
			{16,	{16,	E5B,	E_ObsCode::L7X}},

			{18,	{18,	E5AB,	E_ObsCode::L8I}},
			{19,	{19,	E5AB,	E_ObsCode::L8Q}},
			{20,	{20,	E5AB,	E_ObsCode::L8X}},

			{22,	{22,	E5A,	E_ObsCode::L5I}},
			{23,	{23,	E5A,	E_ObsCode::L5Q}},
			{24,	{24,	E5A,	E_ObsCode::L5X}}
		}
	},

	{	E_Sys::QZS,
		{
			{2,		{2,		L1,		E_ObsCode::L1C}},

			{9,		{9,		LEX,	E_ObsCode::L6S}},
			{10,	{10,	LEX,	E_ObsCode::L6L}},
			{11,	{11,	LEX,	E_ObsCode::L6X}},

			{15,	{15,	L2,		E_ObsCode::L2S}},
			{16,	{16,	L2,		E_ObsCode::L2L}},
			{17,	{17,	L2,		E_ObsCode::L2X}},

			{22,	{22,	L5,		E_ObsCode::L5I}},
			{23,	{23,	L5,		E_ObsCode::L5Q}},
			{24,	{24,	L5,		E_ObsCode::L5X}},

			{30,	{30,	L1,		E_ObsCode::L1S}},
			{31,	{31,	L1,		E_ObsCode::L1L}},
			{32,	{32,	L1,		E_ObsCode::L1X}}
		}
	},

	{	E_Sys::CMP,
		{
			{2,		{2,		B1,		E_ObsCode::L2I}},
			{3,		{3,		B1,		E_ObsCode::L2Q}},
			{4,		{4,		B1,		E_ObsCode::L2X}},

			{8,		{8,		B3,		E_ObsCode::L6I}},
			{9,		{9,		B3,		E_ObsCode::L6Q}},
			{10,	{10,	B3,		E_ObsCode::L6X}},

			{14,	{14,	B2,		E_ObsCode::L7I}},
			{15,	{15,	B2,		E_ObsCode::L7Q}},
			{16,	{16,	B2,		E_ObsCode::L7X}}
		}
	}
};

// sys -> rtcm signal enum -> siginfo (sig enum,
map<E_Sys, map<E_ObsCode, E_FType>> codeTypeMap =
{
	{	E_Sys::GPS,
		{
			{E_ObsCode::L1C,	L1	},
			{E_ObsCode::L1P,	L1	},
			{E_ObsCode::L1W,	L1	},

			{E_ObsCode::L2C,	L2	},
			{E_ObsCode::L2P,	L2	},
			{E_ObsCode::L2W,	L2	},
			{E_ObsCode::L2S,	L2	},
			{E_ObsCode::L2L,	L2	},
			{E_ObsCode::L2X,	L2	},

			{E_ObsCode::L5I,	L5	},
			{E_ObsCode::L5Q,	L5	},
			{E_ObsCode::L5X,	L5	},

			{E_ObsCode::L1S,	L1	},
			{E_ObsCode::L1L,	L1	},
			{E_ObsCode::L1X,	L1	}
		}
	},

	{	E_Sys::GLO,
		{
			{E_ObsCode::L1C,	G1	},
			{E_ObsCode::L1P,	G1	},
			
			{E_ObsCode::L2C,	G2	},
			{E_ObsCode::L2P,	G2	}
		}
	},

	{	E_Sys::GAL,
		{
			{E_ObsCode::L1C,	E1	},
			{E_ObsCode::L1A,	E1	},
			{E_ObsCode::L1B,	E1	},
			{E_ObsCode::L1X,	E1	},
			{E_ObsCode::L1Z,	E1	},

			{E_ObsCode::L6C,	E6	},
			{E_ObsCode::L6A,	E6	},
			{E_ObsCode::L6B,	E6	},
			{E_ObsCode::L6X,	E6	},
			{E_ObsCode::L6Z,	E6	},

			{E_ObsCode::L7I,	E5B	},
			{E_ObsCode::L7Q,	E5B	},
			{E_ObsCode::L7X,	E5B	},

			{E_ObsCode::L8I,	E5AB},
			{E_ObsCode::L8Q,	E5AB},
			{E_ObsCode::L8X,	E5AB},

			{E_ObsCode::L5I,	E5A	},
			{E_ObsCode::L5Q,	E5A	},
			{E_ObsCode::L5X,	E5A	}
		}
	},

	{	E_Sys::QZS,
		{
			{E_ObsCode::L1C,	L1	},

			{E_ObsCode::L6S,	LEX	},
			{E_ObsCode::L6L,	LEX	},
			{E_ObsCode::L6X,	LEX	},

			{E_ObsCode::L2S,	L2	},
			{E_ObsCode::L2L,	L2	},
			{E_ObsCode::L2X,	L2	},

			{E_ObsCode::L5I,	L5	},
			{E_ObsCode::L5Q,	L5	},
			{E_ObsCode::L5X,	L5	},

			{E_ObsCode::L1S,	L1	},
			{E_ObsCode::L1L,	L1	},
			{E_ObsCode::L1X,	L1	}
		}
	},

	{	E_Sys::CMP,
		{
			{E_ObsCode::L2I,	B1	},
			{E_ObsCode::L2Q,	B1	},
			{E_ObsCode::L2X,	B1	},

			{E_ObsCode::L6I,	B3	},
			{E_ObsCode::L6Q,	B3	},
			{E_ObsCode::L6X,	B3	},

			{E_ObsCode::L7I,	B2	},
			{E_ObsCode::L7Q,	B2	},
			{E_ObsCode::L7X,	B2	}
		}
	}
};

// From the RTCM spec table 3.5-73

map<E_Sys, map<E_FType, double>> signal_phase_alignment =
{
	{	E_Sys::GPS,
		{
			{L1,		1.57542E9},
			{L2,		1.22760E9},
			{L5,		1.17645E9}
		}
	},

	// TODO

	//    {SatelliteSystem::GLONASS, {
	//        {FrequencyBand::G1, 1.57542E9},
	//        {FrequencyBand::G2, 1.22760E9},
	//    }},

	{	E_Sys::GAL,
		{
			{E1,		1.57542E9},
			{E5A,		1.17645E9},
			{E5B,		1.20714E9},
			{E5AB,		1.1191795E9},
			{E6,		1.27875E9},
		}
	},

	{	E_Sys::QZS,
		{
			{L1,		1.57542E9},
			{L2,		1.22760E9},
			{L5,		1.17645E9}
		}
	},

	{	E_Sys::CMP,
		{
			{B1,		1.561098E9},
			{B2,		1.207140E9},
			{B3,		1.26852E9}
		}
	}
};












void RtcmDecoder::setTime(GTime& time, double tow)
{
	auto now = utc2gpst(timeget());

	int week;
	double tow_p = time2gpst(now, &week);		//todo aaron, cant use now all the time
	if(acsConfig.ssrOpts.settime_week_override >= 0) // manually set GPS week when post-processing
		week = acsConfig.ssrOpts.settime_week_override;

	int sPerWeek = 60*60*24*7;
	if      (tow < tow_p - sPerWeek/2)				tow += sPerWeek;
	else if (tow > tow_p + sPerWeek/2)				tow -= sPerWeek;

	time = gpst2time(week, tow);
}


int RtcmDecoder::SSRDecoder::getUdiIndex(
	int udi)
{
	int index = -1;
	for (int i = 0; i < 16; ++i) // 16 from updateInterval[16] above
	{
		if (updateInterval[i] == udi)
		{
			index = i;
			break;
		}
	}
	return index;
}

void RtcmDecoder::SSRDecoder::decodeSSR(
	uint8_t*		data, 
	unsigned int	message_length)
{
	int i = 0;
	int message_number		= getbituInc(data, i, 12);
	int	epochTime1s			= getbituInc(data, i, 20);
	int	ssrUpdateIntIndex	= getbituInc(data, i, 4);
	int	multipleMessage		= getbituInc(data, i, 1);

	int ssrUpdateInterval	= updateInterval[ssrUpdateIntIndex];
	double epochTime		= epochTime1s + ssrUpdateInterval / 2.0;
	BOOST_LOG_TRIVIAL(debug) << "SSR message received: " << message_number;

	E_Sys::_enumerated sys = E_Sys::NONE;
	if 	( message_number == +RtcmMessageType::GPS_SSR_ORB_CORR
		||message_number == +RtcmMessageType::GPS_SSR_CLK_CORR
		||message_number == +RtcmMessageType::GPS_SSR_COMB_CORR
		||message_number == +RtcmMessageType::GPS_SSR_CODE_BIAS
		||message_number == +RtcmMessageType::GPS_SSR_PHASE_BIAS						
		||message_number == +RtcmMessageType::GPS_SSR_URA)
	{
		sys = E_Sys::GPS;
	}
	else if ( message_number == +RtcmMessageType::GAL_SSR_ORB_CORR
			||message_number == +RtcmMessageType::GAL_SSR_CLK_CORR
			||message_number == +RtcmMessageType::GAL_SSR_CODE_BIAS
			||message_number == +RtcmMessageType::GAL_SSR_PHASE_BIAS
			||message_number == +RtcmMessageType::GAL_SSR_COMB_CORR)
	{
		sys = E_Sys::GAL;
	}
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message in SSRDecoder::decode()";
	}

	int np = 0;
	int ni = 0;
	int nj = 0;
	int offp = 0;
	switch (sys) 
	{
		case E_Sys::GPS: np=6; ni= 8; nj= 0; offp=  0; break;
		case E_Sys::GLO: np=5; ni= 8; nj= 0; offp=  0; break;
		case E_Sys::GAL: np=6; ni=10; nj= 0; offp=  0; break;
		case E_Sys::QZS: np=4; ni= 8; nj= 0; offp=192; break;
		case E_Sys::CMP: np=6; ni=10; nj=24; offp=  1; break;
		case E_Sys::SBS: np=6; ni= 9; nj=24; offp=120; break;
		default: BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system in SSRDecoder::decode()";
	}

	unsigned int referenceDatum = 0;
	if 	( message_number == +RtcmMessageType::GPS_SSR_ORB_CORR
		||message_number == +RtcmMessageType::GPS_SSR_COMB_CORR
		||message_number == +RtcmMessageType::GAL_SSR_ORB_CORR
		||message_number == +RtcmMessageType::GAL_SSR_COMB_CORR)
	{
		referenceDatum	= getbituInc(data, i, 1);
	}
	unsigned int	iod					= getbituInc(data, i, 4);
	unsigned int	provider			= getbituInc(data, i, 16);
	unsigned int	solution			= getbituInc(data, i, 4);
	
	unsigned int dispBiasConistInd;
	unsigned int MWConistInd;
	if  ( message_number == +RtcmMessageType::GPS_SSR_PHASE_BIAS 
		||message_number == +RtcmMessageType::GAL_SSR_PHASE_BIAS )
	{
		dispBiasConistInd  = getbituInc(data, i, 1);
		MWConistInd        = getbituInc(data, i, 1);
	}
	
	unsigned int	numSats				= getbituInc(data, i, 6);

	// SRR variables for encoding and decoding.
	SSRMeta ssrMeta;
	ssrMeta.epochTime1s			= epochTime1s;
	ssrMeta.ssrUpdateIntIndex	= ssrUpdateIntIndex;
	ssrMeta.multipleMessage		= multipleMessage;
	ssrMeta.referenceDatum		= referenceDatum;
	ssrMeta.provider			= provider;
	ssrMeta.solution			= solution;
	
	for (int sat = 0; sat < numSats; sat++)
	{
		unsigned int	satId			= getbituInc(data, i, np)+offp;

		SatSys Sat(sys, satId);
		
		auto& ssr = nav.satNavMap[Sat].ssr;
			
		if 	( message_number == +RtcmMessageType::GPS_SSR_ORB_CORR
			||message_number == +RtcmMessageType::GPS_SSR_COMB_CORR
			||message_number == +RtcmMessageType::GAL_SSR_ORB_CORR
			||message_number == +RtcmMessageType::GAL_SSR_COMB_CORR)
		{
			SSREph ssrEph;
			ssrEph.ssrMeta = ssrMeta;

			setTime(ssrEph.t0, epochTime);

			ssrEph.iod 			= iod;
			ssrEph.iode			= getbituInc(data, i, ni);
			// ??ssrEph.iodcrc		= getbituInc(data, i, nj);
			ssrEph.deph[0]		= getbitsInc(data, i, 22) * 0.1e-3; // Position, radial, along track, cross track.
			ssrEph.deph[1]		= getbitsInc(data, i, 20) * 0.4e-3;
			ssrEph.deph[2]		= getbitsInc(data, i, 20) * 0.4e-3;
			ssrEph.ddeph[0]		= getbitsInc(data, i, 21) * 0.001e-3; // Velocity
			ssrEph.ddeph[1]		= getbitsInc(data, i, 19) * 0.004e-3;
			ssrEph.ddeph[2]		= getbitsInc(data, i, 19) * 0.004e-3;

								
			//tracepdeex(rtcmdeblvl,std::cout, "\n#RTCM_DEC SSRORB %s %s %4d %10.3f %10.3f %10.3f ", Sat.id(),ssrEph.t0.to_string(2), ssrEph.iode,ssrEph.deph[0],ssrEph.deph[1],ssrEph.deph[2]);
			if	( ssr.ssrEph_map.size() == 0
				||ssrEph.iod		!= ssr.ssrEph_map.begin()->second.iod
				||ssrEph.t0			!= ssr.ssrEph_map.begin()->second.t0)
			{
				ssr.ssrEph_map[ssrEph.t0] = ssrEph;
			}
			
			traceSsrEph(Sat,ssrEph);
		}

		if 	( message_number == +RtcmMessageType::GPS_SSR_CLK_CORR
			||message_number == +RtcmMessageType::GPS_SSR_COMB_CORR
			||message_number == +RtcmMessageType::GAL_SSR_CLK_CORR
			||message_number == +RtcmMessageType::GAL_SSR_COMB_CORR)
		{
			SSRClk ssrClk;
			ssrClk.ssrMeta = ssrMeta;

			setTime(ssrClk.t0, epochTime);

			ssrClk.iod 			= iod;
			
			// C = C_0 + C_1(t-t_0)+C_2(t-t_0)^2 where C is a correction in meters.
			// C gets converted into a time correction for futher calculations.
			ssrClk.dclk[0]		= getbitsInc(data, i, 22) * 0.1e-3;
			ssrClk.dclk[1]		= getbitsInc(data, i, 21) * 0.001e-3;
			ssrClk.dclk[2]		= getbitsInc(data, i, 27) * 0.00002e-3;
			
			//tracepdeex(rtcmdeblvl,std::cout, "\n#RTCM_DEC SSRCLK %s %s      %10.3f %10.3f %10.3f", Sat.id(),ssrClk.t0.to_string(2), ssrClk.dclk[0],ssrClk.dclk[1],ssrClk.dclk[2]);
			if	( ssr.ssrClk_map.size() == 0
				||ssrClk.iod		!= ssr.ssrClk_map.begin()->second.iod
				||ssrClk.t0			!= ssr.ssrClk_map.begin()->second.t0)
			{
				ssr.ssrClk_map[ssrClk.t0] = ssrClk;
			}
			
			traceSsrClk(Sat,ssrClk);
		}

		if 	(message_number == +RtcmMessageType::GPS_SSR_URA)
		{
			SSRUra ssrUra;

			setTime(ssrUra.t0, epochTime);

			ssrUra.iod 			= iod;
			ssrUra.ura			= getbituInc(data, i, 6);
			
			if	( ssr.ssrUra_map.size() == 0
				||ssrUra.iod		!= ssr.ssrUra_map.begin()->second.iod
				||ssrUra.t0			!= ssr.ssrUra_map.begin()->second.t0)
			{
				// This is the total User Range Accuracy calculated from all the SSR.
				// TODO: Check implementation, RTCM manual DF389.                     
				ssr.ssrUra_map[ssrUra.t0] = ssrUra;
			}
		}
		
		if  ( message_number == +RtcmMessageType::GPS_SSR_CODE_BIAS
			||message_number == +RtcmMessageType::GAL_SSR_CODE_BIAS)
		{
			SSRCodeBias ssrBiasCode;
			
			setTime(ssrBiasCode.t0, epochTime);
			
			ssrBiasCode.iod				= iod;
			ssrBiasCode.ssrMeta			= ssrMeta;
			
			unsigned int nbias			= getbituInc(data, i, 5);
			
			for (int k = 0; k < nbias && i + 19 <= message_length * 8; k++) 
			{
				int		rtcm_code		= getbituInc(data, i, 5);
				double bias				= getbitsInc(data, i, 14) * 0.01;
				
				try
				{
					E_ObsCode mode;
					if		(sys == E_Sys::GPS)		{	mode = mCodes_gps.right.at(rtcm_code);	}
					else if (sys == E_Sys::GAL)		{	mode = mCodes_gal.right.at(rtcm_code);	}
					else
					{
						BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system in SSRDecoder::decode()";
					}
					ssrBiasCode.bias[mode] = bias;
					
					traceSsrCodeB(Sat, mode, ssrBiasCode);
				}
				catch (std::exception& e)
				{
					BOOST_LOG_TRIVIAL(error) << "Error, Decoding SSR Message unknown RTCM code : " << rtcm_code;
				}
			}
			
			if	( ssr.ssrCodeBias_map.size() == 0
				||ssrBiasCode.iod		!= ssr.ssrCodeBias_map.begin()->second.iod
				||ssrBiasCode.t0		!= ssr.ssrCodeBias_map.begin()->second.t0)
			{
				ssr.ssrCodeBias_map[ssrBiasCode.t0] = ssrBiasCode;
			}
		}
		
		if  ( message_number == +RtcmMessageType::GPS_SSR_PHASE_BIAS
			||message_number == +RtcmMessageType::GAL_SSR_PHASE_BIAS )
		{
			SSRPhasBias ssrBiasPhas;
			SSRPhase ssrPhase;
			
			setTime(ssrBiasPhas.t0, epochTime);
			
			ssrBiasPhas.ssrMeta			= ssrMeta;
			ssrBiasPhas.iod 			= iod;
			
			ssrPhase.dispBiasConistInd	= dispBiasConistInd;
			ssrPhase.MWConistInd		= dispBiasConistInd;
			ssrPhase.nbias				= getbituInc(data, i, 5);
			ssrPhase.yawAngle			= getbituInc(data, i, 9)/256;
			ssrPhase.yawRate			= getbitsInc(data, i, 8)/8192;
			
			ssrBiasPhas.ssrPhase = ssrPhase;
			
			for (int k = 0; k < ssrPhase.nbias && i + 32 <= message_length * 8; k++) 
			{
				SSRPhaseCh ssrPhaseCh;
				unsigned int rtcm_code		= getbituInc(data, i, 5);
				ssrPhaseCh.signalIntInd		= getbituInc(data, i, 1);
				ssrPhaseCh.signalWidIntInd	= getbituInc(data, i, 2);
				ssrPhaseCh.signalDisconCnt	= getbituInc(data, i, 4);
				double PhaseBias			= getbitsInc(data, i, 20) * 0.0001;
				
				try
				{
					E_ObsCode mode;
					if ( sys == E_Sys::GPS )
					{
						mode = mCodes_gps.right.at(rtcm_code);
					}
					else if ( sys == E_Sys::GAL )
					{
						mode = mCodes_gal.right.at(rtcm_code);
					}
					else
						BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system in SSRDecoder::decode()"; 
					
					ssrBiasPhas.bias		[mode] = PhaseBias; // offset meters due to satellite rotation.
					ssrBiasPhas.ssrPhaseChs	[mode] = ssrPhaseCh;
					
					traceSsrPhasB(Sat, mode, ssrBiasPhas);
				}
				catch (std::exception& e)
				{
					BOOST_LOG_TRIVIAL(error) << "Error, Decoding SSR Message unknown RTCM code : " << rtcm_code;
				}
			}
			
			if	( ssr.ssrPhasBias_map.size() == 0
				||ssrBiasPhas.iod		!= ssr.ssrPhasBias_map.begin()->second.iod
				||ssrBiasPhas.t0		!= ssr.ssrPhasBias_map.begin()->second.t0)
			{
				ssr.ssrPhasBias_map[ssrBiasPhas.t0] = ssrBiasPhas;
			}
		}
	}
}


		
void RtcmDecoder::EphemerisDecoder::decodeEphemeris(uint8_t* data, unsigned int message_length)
{
	Eph eph = {};
	int i = 0;
	int message_number		= getbituInc(data, i, 12);

	E_Sys::_enumerated sys = E_Sys::NONE;
	if 		( message_number == RtcmMessageType::GPS_EPHEMERIS )
	{
		sys = E_Sys::GPS;
	}
	else if ( message_number == RtcmMessageType::GAL_FNAV_EPHEMERIS
			||message_number == RtcmMessageType::GAL_INAV_EPHEMERIS)
	{
		sys = E_Sys::GAL;
	}
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message in EphemerisDecoder::decode()";
	}

	if (sys == E_Sys::GPS)
	{
		if (i + 476 > message_length * 8)
		{
			BOOST_LOG_TRIVIAL(error) << "rtcm3 1019 length error: len=%d\n" << message_length;
			return;
		}

		int prn			= getbituInc(data, i,  6);
		eph.week		= adjgpsweek(getbituInc(data, i, 10));
		eph.sva   		= getbituInc(data, i,  4);
		eph.code  		= getbituInc(data, i,  2);
		eph.idot  		= getbitsInc(data, i, 14)*P2_43*SC2RAD;
		eph.iode  		= getbituInc(data, i,  8);
		double toc     	= getbituInc(data, i, 16)*16.0;
		eph.f2    		= getbitsInc(data, i,  8)*P2_55;
		eph.f1    		= getbitsInc(data, i, 16)*P2_43;
		eph.f0    		= getbitsInc(data, i, 22)*P2_31;
		eph.iodc  		= getbituInc(data, i, 10);
		eph.crs   		= getbitsInc(data, i, 16)*P2_5;
		eph.deln  		= getbitsInc(data, i, 16)*P2_43*SC2RAD;
		eph.M0    		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cuc   		= getbitsInc(data, i, 16)*P2_29;
		eph.e     		= getbituInc(data, i, 32)*P2_33;
		eph.cus   		= getbitsInc(data, i, 16)*P2_29;
		double sqrtA    = getbituInc(data, i, 32)*P2_19;
		eph.toes  		= getbituInc(data, i, 16)*16.0;
		eph.cic   		= getbitsInc(data, i, 16)*P2_29;
		eph.OMG0  		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cis   		= getbitsInc(data, i, 16)*P2_29;
		eph.i0    		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.crc   		= getbitsInc(data, i, 16)*P2_5;
		eph.omg   		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.OMGd  		= getbitsInc(data, i, 24)*P2_43*SC2RAD;
		eph.tgd[0]		= getbitsInc(data, i,  8)*P2_31;
		eph.svh   		= getbituInc(data, i,  6);
		eph.flag  		= getbituInc(data, i,  1);
		eph.fit   		= getbituInc(data, i,  1)?0.0:4.0; /* 0:4hr,1:>4hr */

		if (prn >= 40)
		{
			sys = E_Sys::SBS;
			prn += 80;
		}
		
		if (1)	//todo aaron, janky?
		{
			int week;
			double tow	= time2gpst(utc2gpst(timeget()), &week);
			eph.ttr		= gpst2time(week, floor(tow));
		}
		eph.Sat		= SatSys(sys, prn);
		eph.toe		= gpst2time(eph.week,eph.toes);
		eph.toc		= gpst2time(eph.week,toc);
		eph.A		= SQR(sqrtA);
	}
	else if (sys == E_Sys::GAL)
	{
		if ( message_number == RtcmMessageType::GAL_FNAV_EPHEMERIS )
		{
			if (i + 496-12 > message_length * 8)
			{
				BOOST_LOG_TRIVIAL(error) << "rtcm3 1045 length error: len=%d\n" << message_length;
				return;
			}
		}
		else if (message_number == RtcmMessageType::GAL_INAV_EPHEMERIS)
		{
			if (i + 504-12 > message_length * 8)
			{
				BOOST_LOG_TRIVIAL(error) << "rtcm3 1046 length error: len=%d\n" << message_length;
				return;
			}                    
		}
		else
		{
			BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message for GAL in EphemerisDecoder::decode()";                    
		}
		
		int prn			= getbituInc(data, i, 6);
		eph.week		= adjgpsweek(getbituInc(data, i, 12));
		eph.iode  		= getbituInc(data, i, 10); // Documented as IODnav
		eph.sva   		= getbituInc(data, i,  8); // Documented SISA
		
		eph.idot  		= getbitsInc(data, i, 14)*P2_43*SC2RAD;
		double toc     	= getbituInc(data, i, 14)*60.0;
		eph.f2    		= getbitsInc(data, i,  6)*P2_59;
		eph.f1    		= getbitsInc(data, i, 21)*P2_46;
		eph.f0    		= getbitsInc(data, i, 31)*P2_34;
		eph.crs   		= getbitsInc(data, i, 16)*P2_5;
		eph.deln  		= getbitsInc(data, i, 16)*P2_43*SC2RAD;
		eph.M0    		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cuc   		= getbitsInc(data, i, 16)*P2_29;
		eph.e     		= getbituInc(data, i, 32)*P2_33;
		eph.cus   		= getbitsInc(data, i, 16)*P2_29;
		double sqrtA    = getbituInc(data, i, 32)*P2_19;
		eph.toes  		= getbituInc(data, i, 14)*60.0;
		eph.cic   		= getbitsInc(data, i, 16)*P2_29;
		eph.OMG0  		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cis   		= getbitsInc(data, i, 16)*P2_29;
		eph.i0    		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.crc   		= getbitsInc(data, i, 16)*P2_5;
		eph.omg   		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.OMGd  		= getbitsInc(data, i, 24)*P2_43*SC2RAD;
		eph.tgd[0]		= getbitsInc(data, i, 10)*P2_32;

		int e5a_hs	= 0;
		int e5a_dvs	= 0;
		int rsv		= 0;
		int e5b_hs	= 0;
		int e5b_dvs	= 0;
		int e1_hs	= 0;
		int e1_dvs	= 0;
		if (message_number == RtcmMessageType::GAL_FNAV_EPHEMERIS )
		{
			e5a_hs		= getbituInc(data, i,  2);		// OSHS
			e5a_dvs		= getbituInc(data, i,  1);		// OSDVS
			rsv			= getbituInc(data, i,  7);
		}
		else if (message_number == RtcmMessageType::GAL_INAV_EPHEMERIS)
		{
			eph.tgd[1]	= getbitsInc(data, i,10)*P2_32;	// E5b/E1
			e5b_hs		= getbituInc(data, i, 2);		// E5b OSHS
			e5b_dvs		= getbituInc(data, i, 1);		// E5b OSDVS
			e1_hs		= getbituInc(data, i, 2);		// E1 OSHS
			e1_dvs		= getbituInc(data, i, 1);		// E1 OSDVS
		}

		if (1)	//todo aaron, janky?
		{
			int week;
			double tow	= time2gpst(utc2gpst(timeget()), &week);
			eph.ttr		= gpst2time(week, floor(tow));
		}
		eph.Sat		= SatSys(sys, prn);
		eph.toe		= gpst2time(eph.week,eph.toes);
		eph.toc		= gpst2time(eph.week,toc);                
		
		eph.A		= SQR(sqrtA);
		if (message_number == RtcmMessageType::GAL_FNAV_EPHEMERIS )
		{
			eph.svh=(e5a_hs<<4)+(e5a_dvs<<3);
			eph.code=(1<<1)+(1<<8); // data source = F/NAV+E5a
			eph.iodc=eph.iode;
		}
		else if (message_number == RtcmMessageType::GAL_INAV_EPHEMERIS)
		{
			eph.svh=(e5b_hs<<7)+(e5b_dvs<<6)+(e1_hs<<1)+(e1_dvs<<0);
			eph.code=(1<<0)+(1<<2)+(1<<9); // data source = I/NAV+E1+E5b
			eph.iodc=eph.iode;
		}           
	}
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error: unrecognised sys in EphemerisDecoder::decode()";
	}

	//check for iode, add if not found.
	for (auto& eph_ : nav.ephMap[eph.Sat])
	{
		if	( eph_.iode == eph.iode
			&&fabs(timediff(eph.toe,eph_.toe))<6*60*60) //current iode is guaranteed to be different from other iode's transmitted in the last 6 hours, but then it begins to repeat
		{
			return;
		}
	}		
	//tracepdeex(rtcmdeblvl,std::cout, "\n#RTCM_DEC BRCEPH %s %s %4d %16.9e %13.6e %10.3e ", eph.Sat.id(),eph.toc.to_string(2), eph.iode, eph.f0,eph.f1,eph.f2);
	//std::cout << "Adding ephemeris for " << eph.Sat.id() << std::endl;
	nav.ephMap[eph.Sat].push_back(eph);
	
	traceBroEph(eph,sys);
}



E_FType RtcmDecoder::MSM7Decoder::code_to_ftype(E_Sys sys, E_ObsCode code)
{
	if	( codeTypeMap		.count(sys)		> 0
		&&codeTypeMap[sys]	.count(code)	> 0)
	{
		return codeTypeMap.at(sys).at(code);
	}

	return FTYPE_NONE;
}

boost::optional<SignalInfo> RtcmDecoder::MSM7Decoder::get_signal_info(E_Sys sys, uint8_t signal)
{
	if	( signal_id_mapping		.count(sys)		> 0
		&&signal_id_mapping[sys].count(signal)	> 0)
	{
		return signal_id_mapping.at(sys).at(signal);
	}

	return boost::optional<SignalInfo>();
}

E_ObsCode RtcmDecoder::MSM7Decoder::signal_to_code(E_Sys sys, uint8_t signal)
{
	boost::optional<SignalInfo> info = get_signal_info(sys, signal);

	if (info)
	{
		return info->rinex_observation_code;
	}

	return E_ObsCode::NONE;
}


ObsList RtcmDecoder::MSM7Decoder::decodeMSM7(uint8_t* data, unsigned int message_length,
											map<SatSys,map<E_ObsCode,int>> MSM7_lock_time)
{
	ObsList obsList;
	int i = 0;

	int message_number				= getbituInc(data, i,	12);
	int reference_station_id		= getbituInc(data, i,	12);
	int epoch_time_					= getbituInc(data, i,	30);
	int multiple_message			= getbituInc(data, i,	1);
	int issue_of_data_station		= getbituInc(data, i,	3);
	int reserved					= getbituInc(data, i,	7);
	int clock_steering_indicator	= getbituInc(data, i,	2);
	int external_clock_indicator	= getbituInc(data, i,	2);
	int smoothing_indicator			= getbituInc(data, i,	1);
	int smoothing_interval			= getbituInc(data, i,	3);

	int msmtyp = message_number%10;
	bool extrainfo=false;
	if(msmtyp==5 || msmtyp==7) extrainfo=true;
	int nbcd=15, nbph=22, nblk=4, nbcn=6;
	double sccd=P2_24, scph=P2_29, scsn=1.0; 
	if(msmtyp==6 || msmtyp==7){
		nbcd=20; sccd=P2_29;
		nbph=24; scph=P2_31;
		nblk=10;
		nbcn=10; scsn=0.0625;
	}
	
	int sysind = message_number/10; // integer division is intentional
	E_Sys rtcmsys=E_Sys::NONE;
	double tow = epoch_time_ * 0.001;
	switch(sysind)
	{
		case 107:	rtcmsys = E_Sys::GPS; break;
		case 108:	rtcmsys = E_Sys::GLO; break;
		case 109:	rtcmsys = E_Sys::GAL; break;
		case 111:	rtcmsys = E_Sys::QZS; break;
		case 112:	rtcmsys = E_Sys::CMP; tow+=14.0; break;
	}
	
	GTime tobs;
	if(rtcmsys == +E_Sys::GLO)
	{
		int dowi=(epoch_time_  >> 27);
		int todi=(epoch_time_ & 0x7FFFFFF);
		tow = 86400.0*dowi + 0.001*todi - 10800.0;
		GTime tglo;
		setTime(tglo, tow);
		tobs=utc2gpst(tglo);
	} 
	else setTime(tobs, tow);

	//create observations for satelllites according to the mask
	for (int sat = 0; sat < 64; sat++)
	{
		bool mask 					= getbituInc(data, i,	1);
		if (mask)
		{
			Obs obs;
			obs.Sat.sys = rtcmsys;
			obs.Sat.prn = sat + 1;
			obs.time=tobs;
			obsList.push_back(obs);
		}
	}

	//create a temporary list of signals
	list<E_ObsCode> signalMaskList;
	for (int sig = 0; sig < 32; sig++)
	{
		bool mask 					= getbituInc(data, i,	1);
		if (mask)
		{
			int code = signal_to_code(rtcmsys, sig + 1);
			signalMaskList.push_back(E_ObsCode::_from_integral(code));
		}
	}

	//create a temporary list of signal pointers for simpler iteration later
	map<int,RawSig*> signalPointermap;
	map<int,SatSys>  cellSatellitemap;
	int indx=0;
	//create signals for observations according to existing observations, the list of signals, and the cell mask
	for (auto& obs		: obsList)
	for (auto& sigNum	: signalMaskList)
	{
		bool mask 					= getbituInc(data, i,	1);
		if (mask)
		{
			RawSig sig;

			sig.code = sigNum;
			E_FType ft = code_to_ftype(rtcmsys, sig.code);

			obs.SigsLists[ft].push_back(sig);

			RawSig* pointer = &obs.SigsLists[ft].back();
			signalPointermap [indx] = pointer;
			cellSatellitemap [indx] = obs.Sat;
			indx++;
		}
	}

	//get satellite specific data - needs to be in chunks
	map<SatSys,bool> SatelliteDatainvalid;
	for (auto& obs : obsList)
	{
		int ms_rough_range			= getbituInc(data, i,	8);
		if (ms_rough_range == 255)
		{
			SatelliteDatainvalid[obs.Sat]=true;
			continue;
		}
		else SatelliteDatainvalid[obs.Sat]=false;
		
		for (auto& [ft, sigList]	: obs.SigsLists)
		for (auto& sig				: sigList)
		{
			sig.P = ms_rough_range;
			sig.L = ms_rough_range;
		}
	}

	map<SatSys,map<E_FType,double>>  GLOFreqShift;
	if(extrainfo)
	{
		for (auto& obs : obsList)
		{
			int extended_sat_info		= getbituInc(data, i,	4);


			if(rtcmsys == +E_Sys::GLO)
			{
				GLOFreqShift[obs.Sat][G1] = 562500.0*(extended_sat_info-7);
				GLOFreqShift[obs.Sat][G2] = 437500.0*(extended_sat_info-7);
			}
		}
	}
	else if(rtcmsys == +E_Sys::GLO)
	{
		for (auto& obs : obsList)
		{
			short int prn=obs.Sat.prn;
			if(prn>24 || prn<1)
			{
				SatelliteDatainvalid[obs.Sat]=true;
				GLOFreqShift[obs.Sat][G1] = 0.0;
				GLOFreqShift[obs.Sat][G2] = 0.0;
				
			}
			else
			{
				GLOFreqShift[obs.Sat][G1] = 562500.0 * DefGLOChnl[prn-1];
				GLOFreqShift[obs.Sat][G2] = 437500.0 * DefGLOChnl[prn-1];
			}
		}
	}

	for (auto& obs : obsList)
	{
		int rough_range_modulo		= getbituInc(data, i,	10);

		for (auto& [ft, sigList]	: obs.SigsLists)
		for (auto& sig				: sigList)
		{
			sig.P += rough_range_modulo * P2_10;
			sig.L += rough_range_modulo * P2_10;
		}
	}

	if(extrainfo)
	for (auto& obs : obsList)
	{
		int rough_doppler			= getbituInc(data, i,	14);
		if (rough_doppler == 0x2000)
			continue;

		for (auto& [ft, sigList]	: obs.SigsLists)
		for (auto& sig				: sigList)
		{
			sig.D = rough_doppler;
		}
	}

	//get signal specific data
	for (auto& [indx, signalPointer] : signalPointermap)
	{
		int fine_pseudorange		= getbitsInc(data, i,	nbcd);
		if (fine_pseudorange == 0x80000)
		{
			SatelliteDatainvalid[cellSatellitemap[indx]]=true;
			continue;
		}
		
		RawSig& sig = *signalPointer;
		sig.P += fine_pseudorange * sccd;
	}

	for (auto& [indx, signalPointer] : signalPointermap)
	{
		int fine_phase_range		= getbitsInc(data, i,	nbph);
		if (fine_phase_range == 0x800000)
		{
			SatelliteDatainvalid[cellSatellitemap[indx]]=true;
			continue;
		}
		
		RawSig& sig = *signalPointer;
		sig.L += fine_phase_range * scph;
	}

	for (auto& [indx, signalPointer] : signalPointermap)
	{
		int lock_time_indicator	= getbituInc(data, i,	nblk);
		
		RawSig& sig = *signalPointer;
		sig.LLI=0;
		
		SatSys sat = cellSatellitemap [indx];
		if	( MSM7_lock_time.find(sat)!=MSM7_lock_time.end()
			&&MSM7_lock_time[sat].find(sig.code)!=MSM7_lock_time[sat].end())
		{
			int past_time = MSM7_lock_time[sat][sig.code];
			if(lock_time_indicator<past_time) sig.LLI=1;
		}
		MSM7_lock_time[sat][sig.code]=lock_time_indicator;
	}

	for (auto& [indx, signalPointer] : signalPointermap)
	{
		int half_cycle_ambiguity	= getbituInc(data, i,	1);

		RawSig& sig = *signalPointer;

	}

	for (auto& [indx, signalPointer] : signalPointermap)
	{
		int carrier_noise_ratio		= getbituInc(data, i,	nbcn);

		RawSig& sig = *signalPointer;
		sig.snr = carrier_noise_ratio * scsn;
	}

	if(extrainfo)
	for (auto& [indx, signalPointer] : signalPointermap)
	{
		int fine_doppler		= getbitsInc(data, i,	15);
		if (fine_doppler == 0x4000)
			continue;
		RawSig& sig = *signalPointer;
		sig.D += fine_doppler			* 0.0001;
	}


	//convert millisecond measurements to meters or cycles
	for (auto& obs				: obsList)
	for (auto& [ft, sigList]	: obs.SigsLists)
	for (auto& sig				: sigList)
	{
		double freqcy = signal_phase_alignment[rtcmsys][ft];
		if(rtcmsys == +E_Sys::GLO) freqcy += GLOFreqShift[obs.Sat][ft];
		
		sig.P *= CLIGHT	/ 1000;
		sig.L *= freqcy	/ 1000;
		
		//tracepdeex(rtcmdeblvl,std::cout, "\n#RTCM_DEC MSMOBS %s %s %d %s %.4f %.4f", obs.time.to_string(2), obs.Sat.id(), ft, sig.code._to_string(),sig.P, sig.L );
	}

	return obsList;
}


uint16_t RtcmDecoder::message_length(char header[2])
{
	// Message length is 10 bits starting at bit 6
	return getbitu((uint8_t*)header, 6,	10);
}

RtcmMessageType RtcmDecoder::message_type(const uint8_t message[])
{
	auto id = getbitu(message, 0,	12);

	if (!RtcmMessageType::_is_valid(id))
	{
		return RtcmMessageType::NONE;
	}

	return RtcmMessageType::_from_integral(id);
}

void RtcmStream::parseRTCM(std::istream& inputStream)
{
	while (inputStream)
	{
		
		int byteCnt = 0;
		int pos;
		while (true)
		{
			// Skip to the start of the frame - marked by preamble character 0xD3
			pos = inputStream.tellg();
			
			char c;
			inputStream.read(&c,1);

			if (inputStream)
			{
				if (c == RTCM_PREAMBLE)
				{
					break;
				}
			}
			else
			{                
				return;
			}
			byteCnt++;
		}
		if ( numPreambleFound == 0 )
			byteCnt = 0;
		
		numPreambleFound++;
		numNonMessBytes += byteCnt;
		
		if( byteCnt != 0 )
		{
			std::stringstream message;
			message << "RTCM Extra Bytes, size : " << byteCnt;
			message << ", Total extra bytes : " << numNonMessBytes;
			messageRtcmLog(message.str());
		}

		// Read the frame length - 2 bytes big endian only want 10 bits
		char buf[2];
		inputStream.read((char*)buf, 2);
		int pos2=inputStream.tellg();
		if (inputStream.fail())
		{
			inputStream.clear();
			inputStream.seekg(pos);
			return;
		}
		
		auto message_length = RtcmDecoder::message_length(buf);

		// Read the frame data (include the header)
		unsigned char data[message_length + 3]; 
		data[0] = RTCM_PREAMBLE;
		data[1] = buf[0];
		data[2] = buf[1];
		unsigned char* message = data + 3;
		inputStream.read((char*)message, message_length);
		if (inputStream.fail())
		{
			//tracepdeex(rtcmdeblvl+1,std::cout, "\nSTR_%s bytes in buffer", station.name);
			inputStream.clear();
			inputStream.seekg(pos);
			return;
		}
		// Read the frame CRC
		unsigned int crcRead = 0;
		inputStream.read((char*)&crcRead, 3);		
		if (inputStream.fail())
		{
			//tracepdeex(rtcmdeblvl+1,std::cout, "\nSTR_%s leaving %d bytes in buffer", station.name, message_length);
			inputStream.clear();
			inputStream.seekg(pos);
			return;
		}

		unsigned int crcCalc = crc24q(data, sizeof(data));
		int nmeass = 0;
		if(message_length>8) nmeass = getbitu(message, 0,	12);
		
		if	( (((char*)&crcCalc)[0] != ((char*)&crcRead)[2])
			||(((char*)&crcCalc)[1] != ((char*)&crcRead)[1])
			||(((char*)&crcCalc)[2] != ((char*)&crcRead)[0]))
		{
			numFramesFailedCRC++;
			std::stringstream message;
			message << "RTCM CRC Failure, Number Fail CRC : " << numFramesFailedCRC;
			message << ", Number Passed CRC : " << numFramesPassCRC;
			message << ", Number Decoded : " << numFramesDecoded;
			message << ", Number Preamble : " << numPreambleFound;
			messageRtcmLog(message.str());
			inputStream.seekg(pos2);
			continue;
		}
		numFramesPassCRC++;
		
		

		//tracepdeex(rtcmdeblvl+1,std::cout, "STR_%s : RTCM Message %4d\n", station.name, nmeass);
		
		
		auto message_type = RtcmDecoder::message_type((unsigned char*) message);
		if 		( message_type == +RtcmMessageType::GPS_EPHEMERIS
				||message_type == +RtcmMessageType::GAL_FNAV_EPHEMERIS
				/*||message_type == +RtcmMessageType::GAL_INAV_EPHEMERIS*/)
		{
			numFramesDecoded++;
			decodeEphemeris((uint8_t*) message,message_length);
		}
		else if ( message_type == +RtcmMessageType::GPS_SSR_COMB_CORR
				||message_type == +RtcmMessageType::GPS_SSR_ORB_CORR
				||message_type == +RtcmMessageType::GPS_SSR_CLK_CORR
				||message_type == +RtcmMessageType::GPS_SSR_URA
				||message_type == +RtcmMessageType::GAL_SSR_COMB_CORR
				||message_type == +RtcmMessageType::GAL_SSR_ORB_CORR
				||message_type == +RtcmMessageType::GAL_SSR_CLK_CORR
				||message_type == +RtcmMessageType::GPS_SSR_CODE_BIAS
				||message_type == +RtcmMessageType::GAL_SSR_CODE_BIAS
				||message_type == +RtcmMessageType::GPS_SSR_PHASE_BIAS
				||message_type == +RtcmMessageType::GAL_SSR_PHASE_BIAS)
		{
			numFramesDecoded++;
			decodeSSR((uint8_t*) message,message_length);
		}
		else if ( message_type == +RtcmMessageType::MSM4_GPS
				||message_type == +RtcmMessageType::MSM4_GLONASS
				||message_type == +RtcmMessageType::MSM4_GALILEO
				||message_type == +RtcmMessageType::MSM4_QZSS
				||message_type == +RtcmMessageType::MSM4_BEIDOU
				||message_type == +RtcmMessageType::MSM5_GPS
				||message_type == +RtcmMessageType::MSM5_GLONASS
				||message_type == +RtcmMessageType::MSM5_GALILEO
				||message_type == +RtcmMessageType::MSM5_QZSS
				||message_type == +RtcmMessageType::MSM5_BEIDOU
				||message_type == +RtcmMessageType::MSM6_GPS
				||message_type == +RtcmMessageType::MSM6_GLONASS
				||message_type == +RtcmMessageType::MSM6_GALILEO
				||message_type == +RtcmMessageType::MSM6_QZSS
				||message_type == +RtcmMessageType::MSM6_BEIDOU
				||message_type == +RtcmMessageType::MSM7_GPS
				||message_type == +RtcmMessageType::MSM7_GLONASS
				||message_type == +RtcmMessageType::MSM7_GALILEO
				||message_type == +RtcmMessageType::MSM7_QZSS
				||message_type == +RtcmMessageType::MSM7_BEIDOU)
		{
			numFramesDecoded++;
			ObsList obsList = decodeMSM7((uint8_t*) message,message_length,MSM7_lock_time);
			
			//SuperList.insert(SuperList.end(),obsList.begin(),obsList.end());
			int i=54;
			int multimessage = getbituInc(message, i,	1);
			
			//tracepdeex(rtcmdeblvl,std::cout, "\n%s %4d %s %2d %1d", station.name, message_type._to_integral(), obsList.front().time.to_string(0), obsList.size(), multimessage);
			
			if(multimessage==0)
			{
				SuperList.insert(SuperList.end(),obsList.begin(),obsList.end());
				obsListList.push_back(SuperList);
				SuperList.clear();
			}
			else if	( SuperList.size()>0
					&&obsList.size()>0	
					&&fabs(timediff(SuperList.front().time,obsList.front().time))>0.5)
			{
				obsListList.push_back(SuperList);
				SuperList.clear();
				SuperList.insert(SuperList.end(),obsList.begin(),obsList.end());
			}
			else
				SuperList.insert(SuperList.end(),obsList.begin(),obsList.end());
				
			if(SuperList.size()>1000) SuperList.clear();
		}
	} 	
}

/** Initialises SSROut struct. To be called at the start of every epoch
*/
void initSsrOut(
	double tgap)
{
	int udi			= acsConfig.ssrOpts.update_interval;
	int udiIndex	= RtcmDecoder::SSRDecoder::getUdiIndex(udi);
	int tgapInt		= (int)round(tgap);
	
	if (udi < acsConfig.epoch_interval)		BOOST_LOG_TRIVIAL(error) << "Error: Config ssrOpts.update_interval < acsConfig.epoch_interval.";
	if (udiIndex == -1)						BOOST_LOG_TRIVIAL(error) << "Error: Config ssrOpts.update_interval is not valid (" << acsConfig.ssrOpts.update_interval << ").";
	if (abs(tgap - tgapInt) > 0.000001)		BOOST_LOG_TRIVIAL(error) << "Error: tgap is not an int.";
	if (udi % tgapInt != 0)					BOOST_LOG_TRIVIAL(error) << "Error: Config ssrOpts.update_interval (" << acsConfig.ssrOpts.update_interval << ") is not a multiple of processing_options.epoch_interval (" << tgap << ").";

	int udiNumEpochs = udi / tgap;
	for (auto &[key, entry] : nav.satNavMap)
	{
		// Reset sat count to zero
		entry.ssrOut.numObs = 0;

		// Set UDI
		entry.ssrOut.udiNumEpochs		= udiNumEpochs;
		entry.ssrOut.ssrEph.udi			= udi;
		entry.ssrOut.ssrClk.udi			= udi;
		entry.ssrOut.ssrCodeBias.udi	= udi;
		entry.ssrOut.ssrPhasBias.udi	= udi;

		entry.ssrOut.ssrEph		.ssrMeta.ssrUpdateIntIndex	= udiIndex;
		entry.ssrOut.ssrClk		.ssrMeta.ssrUpdateIntIndex	= udiIndex;
		entry.ssrOut.ssrCodeBias.ssrMeta.ssrUpdateIntIndex	= udiIndex;
		entry.ssrOut.ssrPhasBias.ssrMeta.ssrUpdateIntIndex	= udiIndex;
	}
}

/** Calculates average of given vector
*/
double	calcAve(vector<double> vec)
{
	if (vec.size() == 0)
	{
		return 0;
	}
	double accum = 0;
	for (auto val : vec)
	{
		accum += val;
	}
	return accum / vec.size();
}

/** Calculates SSR corrections and prepares SSROut for exporting.
* To be called at the end of every epoch, before rtcmEncodeToFile() is called;
* SSR corrections: clocks, orbits, code + phase
*/
void	calcSsrCorrections(
	Trace&				trace,			///< Trace to output to
	std::set<SatSys>&	sats,			///< List of satellites visible in this epoch
	GTime 			tsync)			///< Time of current epoch
{
	// Calculate orbits + clocks at tsync & tsync+udi
	for (auto& sat : sats)
	{
		// Create a dummy observation
		Obs obs;
		obs.Sat = sat;
		obs.satNav_ptr = &nav.satNavMap[obs.Sat]; // for satpos_ssr()
		
		GTime time = tsync;
		GTime teph = tsync;
		bool pass;

		char id[5];
		obs.Sat.getId(id);
		// Excerpt from satposs():
		/* grep satellite antenna information */
		PcoMapType* pcoMap_ptr = NULL;
		{
			double ep[6];
			time2epoch(time, ep);
			pcvacs_t* pcsat = findAntenna(id, ep, nav);

			if (pcsat == NULL)
			{
				if (obs.Sat < MINPRNSBS)
				{
					tracepde(1, trace,	"Warning: no satellite (%s) pco information\n", id);
					printf(				"Warning: no satellite (%s) pco information\n", id);
				}
			}
			else
			{
				pcoMap_ptr = &pcsat->pcoMap;
			}
		}

		// Calculate broadcast & precise orb & clock at tsync
		pass = satpos(trace, time, teph, obs, E_Ephemeris::BROADCAST, nav, pcoMap_ptr);
		
		SSROut& ssrOut = nav.satNavMap[sat].ssrOut; // for readability
		
		double relativisticAdj;
		if (pass == true)
		{
			Eph* eph = seleph(teph, sat, -1, nav);
			double tk = timediff(time, eph->toc);
			ssrOut.ssrClk.broadcast	= (eph->f0 + eph->f1 * tk + eph->f2 * tk * tk) * CLIGHT;
			
			//ssrOut.ssrClk.broadcast		= obs.dtSat[0] * CLIGHT; // units: m
			//relativisticAdj				= 2 * obs.rSat.dot(obs.satVel) / CLIGHT;
			//ssrOut.ssrClk.broadcast	   += relativisticAdj; // 'undo' relativistic adjustment performed in satpos(BROADCAST)
			ssrOut.ssrClk.isBroadcastSet	= true;
			ssrOut.ssrEph.broadcast			= obs.rSat;
			ssrOut.ssrEph.broadcastVel		= obs.satVel;
			ssrOut.ssrEph.isBroadcastSet	= true;
			ssrOut.ssrEph.iode				= obs.iode;
		}
		
		pass = satpos(trace, time, teph, obs, acsConfig.ssrOpts.ssr_ephemeris, 	nav, pcoMap_ptr);
		if (pass == true)
		{
			ssrOut.ssrEph.precise			= obs.rSat;
			ssrOut.ssrEph.isPreciseSet		= true;
			// (Note - precise clock is set in outputSatelliteClocks())
		}

		// Calculate broadcast & precise orb's at tsync+udi
		time = tsync + ssrOut.ssrEph.udi;
		
		pass = satpos(trace, time, teph, obs, E_Ephemeris::BROADCAST, nav, pcoMap_ptr);
		if (pass == true)
		{
			ssrOut.ssrEph.nextBroadcast		= obs.rSat;
			ssrOut.ssrEph.nextBroadcastVel	= obs.satVel;
			ssrOut.ssrEph.isNextBroadcastSet= true;
			
			assert(ssrOut.ssrEph.iode == obs.iode);
		}
		
		pass = satpos(trace, time, teph, obs, acsConfig.ssrOpts.ssr_ephemeris, 	nav, pcoMap_ptr);
		if (pass == true)
		{
			ssrOut.ssrEph.nextPrecise		= obs.rSat;
			ssrOut.ssrEph.isNextPreciseSet	= true;
		}
	}

	// Form master metadata struct for all SSR messages
	SSRMeta masterSsrMeta = {};
	masterSsrMeta.epochTime1s		= time2gpst(tsync, nullptr);
	masterSsrMeta.ssrUpdateIntIndex	= -1;// to be set individually
	masterSsrMeta.multipleMessage	= 0; //note: change when broadcasting multi-gnss
	masterSsrMeta.referenceDatum	= 0; // 0: global (ITRF); 1: local
	masterSsrMeta.provider			= 0; // provided by RTCM on request
	masterSsrMeta.solution			= 0; // one for each SSR service of the SSR provider

	// Prepare SSR message data
	int masterIod					= 0; // constant value for all messages belonging to the same set
	for (auto& [key, entry] : nav.satNavMap)
	{
		SSROut& ssrOut = entry.ssrOut;
		if (ssrOut.epochsSinceUpdate >= ssrOut.udiNumEpochs)
		{
			// Prepare SSR clock corrections
			SSRClkOut& ssrClk = ssrOut.ssrClk;
			
			if	( ssrClk.isPreciseSet
				&&ssrClk.isBroadcastSet
				&&ssrOut.numObs > 0)
			{
				ssrClk.dclk[0]					= ssrClk.precise - ssrClk.broadcast;
				ssrClk.dclk[1]					= 0;	// set to zero (not used)
				ssrClk.dclk[2]					= 0;	// set to zero (not used)

				int udiIndex					= RtcmDecoder::SSRDecoder::getUdiIndex((int)round(ssrClk.udi));
				if (udiIndex == -1)	
					BOOST_LOG_TRIVIAL(error) << "Error: ssrClk.udi is not valid (" << ssrClk.udi << ").";
				
				ssrClk.t0						= tsync + ssrClk.udi / 2.0;
				ssrClk.iod						= masterIod;
				ssrClk.ssrMeta					= masterSsrMeta;
				ssrClk.ssrMeta.ssrUpdateIntIndex= udiIndex;
				ssrClk.canExport				= true;
			}
			ssrClk.isPreciseSet 	= false;
			ssrClk.isBroadcastSet 	= false;


			// Prepare SSR orbit corrections
			SSREphOut& ssrEph = ssrOut.ssrEph;
			if	( ssrEph.isPreciseSet
				&&ssrEph.isBroadcastSet
				&&ssrEph.isNextPreciseSet
				&&ssrEph.isNextBroadcastSet
				&&ssrOut.numObs > 0)
			{
				Vector3d	diffEcef			= ssrEph.broadcast		- ssrEph.precise;
				Vector3d	nextDiffEcef		= ssrEph.nextBroadcast	- ssrEph.nextPrecise;
				Vector3d	diffRac				= ecef2rac(diffEcef, 		ssrEph.broadcast, 		ssrEph.broadcastVel);
				Vector3d	nextDiffRac			= ecef2rac(nextDiffEcef, 	ssrEph.nextBroadcast, 	ssrEph.nextBroadcastVel);
				Vector3d	aveDiffRac			= (diffRac + nextDiffRac) / 2.0;
				Vector3d	dDiffRac			= (nextDiffRac - diffRac) / ssrEph.udi;
				Vector3d::Map(ssrEph.deph, aveDiffRac.rows())	= aveDiffRac; // Copy from Vector3d to C array
				Vector3d::Map(ssrEph.ddeph,dDiffRac.rows())		= dDiffRac;

				int udiIndex					= RtcmDecoder::SSRDecoder::getUdiIndex((int)round(ssrEph.udi));
				if (udiIndex == -1)	
					BOOST_LOG_TRIVIAL(error) << "Error: ssrEph.udi is not valid (" << ssrEph.udi << ").";
				
				ssrEph.t0						= tsync + ssrEph.udi / 2.0;
				ssrEph.iod						= masterIod;
				ssrEph.ssrMeta					= masterSsrMeta;
				ssrEph.ssrMeta.ssrUpdateIntIndex= udiIndex;
				ssrEph.canExport				= true;
			}
			ssrEph.isPreciseSet 	= false;
			ssrEph.isBroadcastSet 	= false;


			// Prepare SSR code bias corrections
			SSRCodeBiasOut& ssrCodeBias = ssrOut.ssrCodeBias;
			if (ssrCodeBias.isSet)
			{
				ssrCodeBias.isSet = false;
				
				int udiIndex						= RtcmDecoder::SSRDecoder::getUdiIndex((int)round(ssrCodeBias.udi));
				if (udiIndex == -1)	
					BOOST_LOG_TRIVIAL(error) << "Error: ssrCodeBias.udi is not valid (" << ssrCodeBias.udi << ").";
				
				ssrCodeBias.t0							= tsync + ssrCodeBias.udi / 2.0;
				ssrCodeBias.iod							= masterIod;
				ssrCodeBias.ssrMeta						= masterSsrMeta;
				ssrCodeBias.ssrMeta.ssrUpdateIntIndex	= udiIndex;
				ssrCodeBias.canExport					= true;
				
// 				assert(ssrCodeBias.udi_code == ssrBias.udi_phas); // note - if these need to be different, modify SSRBias to have two ssrMeta's - one for phase & one for code
			}


			// Prepare SSR phase bias corrections
			SSRPhasBiasOut& ssrPhasBias = ssrOut.ssrPhasBias;
			if (ssrPhasBias.isSet)
			{
				ssrPhasBias.isSet = false;
				
				int udiIndex						= RtcmDecoder::SSRDecoder::getUdiIndex((int)round(ssrPhasBias.udi));
				if (udiIndex == -1)
					BOOST_LOG_TRIVIAL(error) << "Error: ssrPhasBias.udi is not valid (" << ssrPhasBias.udi << ").";
				
				ssrPhasBias.t0							= tsync + ssrPhasBias.udi / 2.0;
				ssrPhasBias.iod							= masterIod;
				
				ssrPhasBias.ssrMeta						= masterSsrMeta;
				ssrPhasBias.ssrMeta.ssrUpdateIntIndex	= udiIndex;
				ssrPhasBias.ssrPhase.dispBiasConistInd	= 0;
				ssrPhasBias.ssrPhase.MWConistInd		= 0;
				
				int nbias 		 						= ssrPhasBias.bias.size();
				
				ssrPhasBias.ssrPhase.nbias				= nbias;
				ssrPhasBias.ssrPhase.yawAngle			= 0;
				ssrPhasBias.ssrPhase.yawRate			= 0;
				ssrPhasBias.canExport					 = true;
				
// 				assert(ssrBias.udi_code == ssrBias.udi_phas); // note - if these need to be different, modify SSRBias to have two ssrMeta's - one for phase & one for code
				assert(nbias == ssrPhasBias.var.size());
				
				for (auto& [pkey, value] : ssrPhasBias.bias)
				{
					ssrPhasBias.ssrPhaseChs[pkey].signalIntInd		= 0; // to implement later - set when bias moves beyond upper/lower limit of value
					ssrPhasBias.ssrPhaseChs[pkey].signalWidIntInd	= 0; // ^see note above
					ssrPhasBias.ssrPhaseChs[pkey].signalDisconCnt	= 0;
					
					if (abs(value) > 52.4287) // 52.4287 from ssr_2_phase_bias_v06
					{
						ssrPhasBias.canExport = false;			// ^ In the interim, don't transmit phase bias when out of bounds
					}
				}
			}

			// Copy data into nav
			ssrOut.epochsSinceUpdate = 0;
		}
		ssrOut.epochsSinceUpdate++;
	}
}

/** Sets filename for RTCM files
*/
string	getRtcmFilename(int epochNum)
{
	return acsConfig.ssrOpts.rtcm_directory + "rtcmDataEpoch" + std::to_string(epochNum) + ".dat";
}

/** Encodes nav.satNavMap[].ssrOut to file containing RTCM messages
*/
void	rtcmEncodeToFile(int epochNum)
{
	string filename = getRtcmFilename(epochNum);
	std::ofstream ofRtcmSsr(filename, std::ios::out | std::ios::binary);
	if (!ofRtcmSsr)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not open file for exporting SSR corrections at " << filename;
		return;
	}
	RtcmEncoder::SSREncoder rtcmSsrEnc;
	rtcmSsrEnc.encodeSsr(true);
	rtcmSsrEnc.encodeWriteMessages(ofRtcmSsr);
}

/** Decodes file containing RTCM messages to nav.satNavMap[].ssr
*/
void	rtcmDecodeFromFile(int epochNum)
{
	string filename = getRtcmFilename(epochNum);
	std::ifstream ifRtcmSsr(filename, std::ios::in | std::ios::binary);
	if (!ifRtcmSsr)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not open file for importing SSR corrections at " << filename;
		return;
	}
	RtcmStream absStream;
	absStream.parseRTCM(ifRtcmSsr);
}

/** Creates a file containing the current epoch number
*/
void	exportSyncFile(int epochNum)
{
	string filename = acsConfig.ssrOpts.rtcm_directory + "sync.tmp";
	std::ofstream fout(filename);
	if (!fout)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not open sync file at " << filename;
		return;
	}
	fout << epochNum << std::endl;
}

/** Creates a file containing the current epoch number
*/
void	waitForSyncFile(int epochNum)
{
	string filename = acsConfig.ssrOpts.rtcm_directory + "sync.tmp";
	using namespace std::literals::chrono_literals;
	bool wait = true;
	while (wait)
	{
		std::ifstream fin(filename);
		while (!fin.is_open())
		{
			std::this_thread::sleep_for(0.1s);
			fin.open(filename);
		}
		int numIn = 0;
		fin >> numIn;
		if (numIn >= epochNum)
		{
			wait = false;
		}
		else
		{
			std::this_thread::sleep_for(0.1s);
		}
	}
}


/** Writes nav.satNavMap[].ssrOut to a human-readable file
*/
void	writeSsrOutToFile(
	int					epochNum, 
	std::set<SatSys>	sats)
{
	string filename = "ssrOut.dbg";
	std::ofstream out(filename, std::ios::app);
	
	if (!out)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not open trace file for SSR messages at " << filename;
		return;
	}
	out.precision(17);
	
	// Header
	out << "epochNum" 			<< "\t";
	out << "satId" 				<< "\t";
	out << "SSREph.canExport"	<< "\t";
	out << "SSREph.t0"			<< "\t";
	out << "SSREph.udi"			<< "\t";
	out << "SSREph.iod"			<< "\t";
	out << "SSREph.iode"		<< "\t";
	out << "SSREph.deph[0]"		<< "\t";
	out << "SSREph.deph[1]"		<< "\t";
	out << "SSREph.deph[2]"		<< "\t";
	out << "SSREph.ddeph[0]"	<< "\t";
	out << "SSREph.ddeph[1]"	<< "\t";
	out << "SSREph.ddeph[2]"	<< "\t";

	out << "SSRClk.canExport"	<< "\t";
	out << "SSRClk.t0"			<< "\t";
	out << "SSRClk.udi"			<< "\t";
	out << "SSRClk.iod"			<< "\t";
	out << "SSRClk.dclk[0]"		<< "\t";
	out << "SSRClk.dclk[1]"		<< "\t";
	out << "SSRClk.dclk[2]"		<< "\t";

	out << "SSRBias.canExportPhase"	<< "\t";
	out << "SSRBias.canExportCode"	<< "\t";
	out << "SSRBias.t0_code"	<< "\t";
	out << "SSRBias.t0_phas"	<< "\t";
	out << "SSRBias.udi_code"	<< "\t";
	out << "SSRBias.udi_phas"	<< "\t";
	out << "SSRBias.iod_code"	<< "\t";
	out << "SSRBias.iod_phas"	<< "\t";
	for (int i=0; i<2; ++i)	out << "ssrBias.cbias_"<< i << "\t";
	for (int i=0; i<2; ++i)	out << "ssrBias.cvari_"<< i << "\t";
	for (int i=0; i<2; ++i)	out << "ssrBias.pbias_"<< i << "\t";
	for (int i=0; i<2; ++i)	out << "ssrBias.pvari_"<< i << "\t";
	for (int i=0; i<2; ++i)
	{
		out << "ssrBias.ssrPhaseCh.signalIntInd_"		<< i <<	"\t";
		out << "ssrBias.ssrPhaseCh.signalWidIntInd_"	<< i <<	"\t";
		out << "ssrBias.ssrPhaseCh.signalDisconCnt_"	<< i << "\t";
	}
	out << std::endl;

	// Body
	for (auto& sat : sats)
	{
		auto& entry = nav.satNavMap[sat];
		
		out << epochNum << "\t";
		out << sat.id() << "\t";
		out << entry.ssrOut.ssrEph.canExport<< "\t";
		out << entry.ssrOut.ssrEph.t0		<< "\t";
		out << entry.ssrOut.ssrEph.udi		<< "\t";
		out << entry.ssrOut.ssrEph.iod		<< "\t";
		out << entry.ssrOut.ssrEph.iode		<< "\t";
		out << entry.ssrOut.ssrEph.deph[0]	<< "\t";
		out << entry.ssrOut.ssrEph.deph[1]	<< "\t";
		out << entry.ssrOut.ssrEph.deph[2]	<< "\t";
		out << entry.ssrOut.ssrEph.ddeph[0]	<< "\t";
		out << entry.ssrOut.ssrEph.ddeph[1]	<< "\t";
		out << entry.ssrOut.ssrEph.ddeph[2]	<< "\t";

		out << entry.ssrOut.ssrClk.canExport<< "\t";
		out << entry.ssrOut.ssrClk.t0		<< "\t";
		out << entry.ssrOut.ssrClk.udi		<< "\t";
		out << entry.ssrOut.ssrClk.iod		<< "\t";
		out << entry.ssrOut.ssrClk.dclk[0]	<< "\t";
		out << entry.ssrOut.ssrClk.dclk[1]	<< "\t";
		out << entry.ssrOut.ssrClk.dclk[2]	<< "\t";

		out << entry.ssrOut.ssrPhasBias.canExport	<< "\t";
		out << entry.ssrOut.ssrCodeBias.canExport	<< "\t";
		out << entry.ssrOut.ssrCodeBias.t0			<< "\t";
		out << entry.ssrOut.ssrPhasBias.t0			<< "\t";
		out << entry.ssrOut.ssrCodeBias.udi			<< "\t";
		out << entry.ssrOut.ssrPhasBias.udi			<< "\t";
		out << entry.ssrOut.ssrCodeBias.iod			<< "\t";
		out << entry.ssrOut.ssrPhasBias.iod			<< "\t";
		for (auto& [key, val] : entry.ssrOut.ssrCodeBias.bias)	out << val<< "\t";
		for (auto& [key, val] : entry.ssrOut.ssrCodeBias.var)	out << val<< "\t";
		for (auto& [key, val] : entry.ssrOut.ssrPhasBias.bias)	out << val<< "\t";
		for (auto& [key, val] : entry.ssrOut.ssrPhasBias.var)	out << val<< "\t";
		
		for (auto& [key, ssrPhaseCh] : entry.ssrOut.ssrPhasBias.ssrPhaseChs)
		{
			out << ssrPhaseCh.signalIntInd		<< "\t";
			out << ssrPhaseCh.signalWidIntInd	<< "\t";
			out << ssrPhaseCh.signalDisconCnt	<< "\t";
		}
		out << std::endl;
	}
	out << std::endl;
}


std::string NtripRtcmStream::getJsonNetworkStatistics(system_clock::time_point epochTime)
{
	auto time = std::chrono::system_clock::to_time_t(epochTime);
	
	std::stringstream networkJson;
	networkJson << "{" << std::endl;
	networkJson << "\"Station\": \"" << url.path.substr(1,url.path.length()) << "\",";
	networkJson << "\"Epoch\": \"" << std::put_time( std::localtime( &time ), "%F %X" ) << "\","; 
	networkJson << "\"Network\": \"" <<  acsConfig.analysis_agency << "\",";
	networkJson << "\"Downloading\": \"true\",";
	
	auto timeNow = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
	boost::posix_time::time_duration totalTime = timeNow - startTime;
	double connRatio = (double)connectedDuration.total_milliseconds() /(double)totalTime.total_milliseconds();
	
	double meanReconn = 0;
	if ( disconnectionCount != 0 )
		meanReconn = (double)disconnectedDuration.total_milliseconds()/(60.0*1000.0*disconnectionCount);

	networkJson << "\"Disconnects\": " << disconnectionCount << ",";
	networkJson << "\"MeanDowntime\": " << meanReconn << ",";
	networkJson << "\"Connected ratio\": " << connRatio << ",";

	double chunkRatio = 0;
	int numberChunks = numberErroredChunks+numberValidChunks;
	if ( numberChunks != 0 )
		chunkRatio = (double)numberErroredChunks/(double)(numberChunks);

	networkJson << "\"Chunks\": " << numberChunks << ",";
	networkJson << "\"ChunkErrors\": " <<  numberErroredChunks << ",";
	networkJson << "\"Chunk error ratio\": " << chunkRatio << ",";

	networkJson << "\"RtcmExtraBytes\": " << numNonMessBytes << ",";
	networkJson << "\"RtcmFailCrc\": " << numFramesFailedCRC << ",";
	networkJson << "\"RtcmPassedCrc\": " << numFramesPassCRC << ",";
	networkJson << "\"RtcmDecoded\": " << numFramesDecoded << ",";
	networkJson << "\"RtcmPreamble\": " << numPreambleFound << ",";
	
	double FailedToPreambleRatio = 0;
	if ( numPreambleFound != 0 )
		FailedToPreambleRatio = (double)numFramesFailedCRC/(double)numPreambleFound;
	networkJson << "\"RtcmFailedCrcToPreambleRatio\": " << FailedToPreambleRatio;
	networkJson << "}";
	
	return networkJson.str();
}

void NtripRtcmStream::traceMakeNetworkOverview(Trace& trace)
{
	bool printToTerminal = false;
	
	
	std::stringstream message;
	message << url.path;
	if	( !isConnected
		&& disconnectionCount == 0
		&& (numberValidChunks+disconnectionCount) == 0 )
	{
		message << ", Network, Has Not Connected.";
		message << std::endl;
		message << std::endl;            
	}
	else
	{
		
		message << ", Network, Number Disconnects : " << disconnectionCount;
		double connRatio = 1;
		if( disconnectionCount != 0 )
		{
			auto timeNow = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
			boost::posix_time::time_duration totalTime = timeNow - startTime;
			double connRatio = (double)connectedDuration.total_milliseconds() /(double)totalTime.total_milliseconds();
			double meanReconn = (double)disconnectedDuration.total_milliseconds()/(60.0*1000.0*disconnectionCount);
			
			message << ", Mean Re-connection Time (min) : " << meanReconn;
			message << ", Ratio Connectioned Time : " << connRatio;
		}
		if( connRatio < 0.99 )
			printToTerminal = true;
		
		message << std::endl;
		message << url.path;
		message << ", Number Chunks : " << numberValidChunks+numberErroredChunks;
		message << ", Number Errored Chunks : " << numberErroredChunks;
		
		double chunkRatio = 0;
		if ( numberErroredChunks+numberValidChunks != 0 )
		{
			chunkRatio = (double)numberErroredChunks/(double)(numberErroredChunks+numberValidChunks);
			message << ", Ratio Error Chunks : " << chunkRatio;
		}
		if( chunkRatio > 0.01 )
			printToTerminal = true;
		
		message << std::endl;
		message << url.path;        
		message << ", RTCM Total extra bytes : " << numNonMessBytes;
		message << ", RTCM Fail CRC : " << numFramesFailedCRC;
		message << ", RTCM Passed CRC : " << numFramesPassCRC;
		message << ", RTCM Decoded : " << numFramesDecoded;
		message << ", RTCM Preamble : " << numPreambleFound;
		
		double crcRatio = 0;
		if ( numPreambleFound != 0 )
		{
			crcRatio = (double)numFramesFailedCRC/(double)numPreambleFound;
			message << ", RTCM Ratio, Fail/Preamble : " << crcRatio;
		}
		if( crcRatio > 0.01 )
			printToTerminal = true;
		
		message << std::endl;
		message << std::endl;
	}
	
	// The variable reduces the amount of printing to the terminal to only
	// streams with high error.
	if	( printToTerminal
		&& print_stream_statistics)
		BOOST_LOG_TRIVIAL(debug) << message.str();
	
	
	std::string messLine;
	while(std::getline(message, messLine)) 
		tracepde(3,trace,messLine+"\n"); 
}
