

// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

FileType RTCM__()
{

}

FileType SSR__()
{

}

#include "rtcmEncoder.hpp"
#include "rtcmDecoder.hpp"
#include "streamRtcm.hpp"
#include "mongoWrite.hpp"
#include "acsConfig.hpp"
#include "otherSSR.hpp"
#include "biases.hpp"
#include "gTime.hpp"
#include "enums.h"

double			RtcmDecoder::rtcmDeltaTime = 0;

map<GTime, int>	RtcmDecoder::receivedTimeMap;

map<E_FType, double> carrierFrequency =
{
	{F1, FREQ1},
	{F2, FREQ2},
	{F5, FREQ5},
	{F6, FREQ6},
	{F7, FREQ7},
	{F8, FREQ8},
	{G1, FREQ1_GLO},
	{G2, FREQ2_GLO},
	{G3, FREQ3_GLO},
	{G4, FREQ4_GLO},
	{G6, FREQ6_GLO},
	{B1, FREQ1_CMP},
	{B3, FREQ3_CMP}
};


GTime RtcmDecoder::rtcmTime()
{
	GTime time;

	if		(rtcmTimestampTime	!= GTime::noTime())		time = rtcmTimestampTime;
	else if (tsync				!= GTime::noTime())		time = tsync;
	// todo Eugene: gps nav
	else 												time = timeGet();

	return time;
}

/** adjust GPS week number according to RTCM time
 */
int RtcmDecoder::adjGpsWeek(
	int week)			///< not-adjusted GPS week number
{
	GWeek nowWeek = rtcmTime();

	int dWeeks		= nowWeek - week;
	int roundDWeeks	= (dWeeks + 512) / 1024 * 1024;

	return (week + roundDWeeks);
}

/** adjust GST week number according to RTCM time
 */
int RtcmDecoder::adjGstWeek(
	int week)			///< not-adjusted GST week number
{
	GWeek nowWeek = rtcmTime();

	int dWeeks		= nowWeek - week;
	int roundDWeeks	= (dWeeks + 2048) / 4096 * 4096;

	return (week + roundDWeeks);
}

/** adjust BDT week number according to RTCM time
 */
int RtcmDecoder::adjBdtWeek(
	int week)			///< not-adjusted BDT week number
{
	BWeek nowWeek = rtcmTime();

	int dWeeks		= nowWeek - week;
	int roundDWeeks	= (dWeeks + 4096) / 8192 * 8192;

	return (week + roundDWeeks);
}

E_RTCMSubmessage RtcmDecoder::decodeCustomId(
	vector<unsigned char>& message)
{
	int i = 0;

	int messageNumber		= getbituInc(message, i, 12);
	int customMessageNumber	= getbituInc(message, i, 8);

	E_RTCMSubmessage customType = E_RTCMSubmessage::_from_integral(customMessageNumber);

	return customType;
}

GTime RtcmDecoder::decodeCustomTimestamp(
	vector<unsigned char>& data)
{
	int i = 0;

	int messageNumber		= getbituInc(data, i, 12);
	int customMessageNumber	= getbituInc(data, i, 8);

	E_RTCMSubmessage customType = E_RTCMSubmessage::_from_integral(customMessageNumber);

	GTime time;

	int reserved 			= getbituInc(data, i, 4);

	unsigned int chunk1		= getbituInc(data, i, 32);
	unsigned int chunk2		= getbituInc(data, i, 32);

	long int milliseconds	=  ((unsigned long int) chunk1)
							+ (((unsigned long int) chunk2) << 32);

	time.bigTime = milliseconds / 1000.0;

	return time;
}


/** decode RTCM SSR messages
 */
void RtcmDecoder::decodeSSR(
	vector<unsigned char>& data)	///< stream data
{
	int i = 0;

	int messageNumber			= getbituInc(data, i, 12);

// 	std::cout << "SSR message received: " << messageNumber << "\n";

	RtcmMessageType messCode;
	try
	{
		messCode	= RtcmMessageType::_from_integral(messageNumber);
	}
	catch (...)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message in " << __FUNCTION__;
		return;
	}

	string	messCodeStr	= messCode._to_string();
	string	messTypeStr	= messCodeStr.substr(8);

	E_Sys sys = rtcmMessageSystemMap[messCode];

	if (sys == +E_Sys::NONE)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: invalid message code system :" << messCode;
		return;
	}

	if	( sys != +E_Sys::GPS
		&&sys != +E_Sys::GLO
		&&sys != +E_Sys::GAL
		&&sys != +E_Sys::QZS
		&&sys != +E_Sys::SBS
		&&sys != +E_Sys::BDS)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message in " << __FUNCTION__;
	}

	// if (sys == +E_Sys::BDS)
	// {
	// 	printf ("\nRTCM %4d: ", messageNumber);
	// 	for (auto& dat : data) printf("%02X",dat);
	// 	printf ("\n");
	// }

	int ne = 0;
	int ns = 0;
	int np = 0;
	int ni = 0;
	int nj = 0;
	switch (sys)
	{
		case E_Sys::GPS:	ne = 20;	ns = 6;		np = 6;		ni =  8;	nj =  0;	break;
		case E_Sys::GLO:	ne = 17;	ns = 6;		np = 5;		ni =  8;	nj =  0;	break;
		case E_Sys::GAL:	ne = 20;	ns = 6;		np = 6;		ni = 10;	nj =  0;	break;
		case E_Sys::QZS:	ne = 20;	ns = 4;		np = 4;		ni =  8;	nj =  0;	break;
		case E_Sys::BDS:	ne = 20;	ns = 6;		np = 6;		ni =  8;	nj = 10;	break;
		case E_Sys::SBS:	ne = 20;	ns = 6;		np = 6;		ni =  9;	nj =  0;	break;
		default: BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system in " << __FUNCTION__;	return;
	}

	int	epochTime1s 			= getbituInc(data, i, ne);
	int	updateIntIndex			= getbituInc(data, i, 4);
	int	multipleMessage			= getbituInc(data, i, 1);

	int ssrUpdateInterval		= updateInterval[updateIntIndex];

	double	referenceTime;
	if (updateIntIndex == 0)	referenceTime	= epochTime1s;
	else						referenceTime	= epochTime1s + ssrUpdateInterval / 2.0;

	GTime	nearTime		= rtcmTime();

	GTime	receivedTime;
	GTime	t0;
	if		(sys == +E_Sys::GLO)
	{
		receivedTime	= GTime(RTod(epochTime1s), 		nearTime);
		t0				= GTime(RTod(referenceTime),	nearTime);
	}
	else if (sys == +E_Sys::BDS)
	{
		receivedTime	= GTime(BTow(epochTime1s), 		nearTime);
		t0				= GTime(BTow(referenceTime),	nearTime);
	}
	else
	{
		receivedTime	= GTime(GTow(epochTime1s), 		nearTime);
		t0				= GTime(GTow(referenceTime),	nearTime);
	}

//     std::cout << "SSR message received: " << messCode << "\n";

	unsigned int referenceDatum = 0;
	if	( messTypeStr == "ORB_CORR"
		||messTypeStr == "COMB_CORR")
	{
		referenceDatum			= getbituInc(data, i, 1);
	}

	unsigned int	iod			= getbituInc(data, i, 4);
	unsigned int	provider	= getbituInc(data, i, 16);
	unsigned int	solution	= getbituInc(data, i, 4);

	unsigned int dispBiasConistInd	= 0;
	unsigned int MWConistInd		= 0;
	if  ( messTypeStr == "PHASE_BIAS")
	{
		dispBiasConistInd		= getbituInc(data, i, 1);
		MWConistInd				= getbituInc(data, i, 1);
	}

	unsigned int	numSats		= getbituInc(data, i, ns);

	// SRR variables for encoding and decoding.
	SSRMeta ssrMeta;
	ssrMeta.epochTime1s			= epochTime1s;
	ssrMeta.receivedTime		= receivedTime;
	ssrMeta.updateIntIndex		= updateIntIndex;
	ssrMeta.multipleMessage		= multipleMessage;
	ssrMeta.referenceDatum		= referenceDatum;
	ssrMeta.provider			= provider;
	ssrMeta.solution			= solution;
	ssrMeta.numSats				= numSats;

	bool failure = false;

	for (int sat = 0; sat < numSats; sat++)
	{
		unsigned int	satId	= getbituInc(data, i, np);

		SatSys Sat(sys, satId);

		auto& ssr = nav.satNavMap[Sat].receivedSSR;

		if	( messTypeStr == "ORB_CORR"
			||messTypeStr == "COMB_CORR")
		{
			SSREph ssrEph;
			ssrEph.ssrMeta		= ssrMeta;
			ssrEph.t0			= t0;
			ssrEph.udi			= ssrUpdateInterval;
			ssrEph.iod 			= iod;

			ssrEph.iodcrc		= getbituInc(data, i, nj);
			ssrEph.iode			= getbituInc(data, i, ni);
			ssrEph.deph[0]		= getbitsInc(data, i, 22) * 0.1e-3;		// Position, radial, along track, cross track.
			ssrEph.deph[1]		= getbitsInc(data, i, 20) * 0.4e-3;
			ssrEph.deph[2]		= getbitsInc(data, i, 20) * 0.4e-3;
			ssrEph.ddeph[0]		= getbitsInc(data, i, 21) * 0.001e-3;	// Velocity
			ssrEph.ddeph[1]		= getbitsInc(data, i, 19) * 0.004e-3;
			ssrEph.ddeph[2]		= getbitsInc(data, i, 19) * 0.004e-3;

			tracepdeex(5, std::cout, "\n#RTCM_SSR ORBITS %s %s %4d %10.3f %10.3f %10.3f %d ", Sat.id().c_str(), ssrEph.t0.to_string().c_str(), ssrEph.iode, ssrEph.deph[0], ssrEph.deph[1], ssrEph.deph[2], iod);

			ssr.ssrEph_map[receivedTime] = ssrEph;

			if (acsConfig.output_decoded_rtcm_json)
				traceSsrEph(messCode, Sat, ssrEph);
		}

		if	( messTypeStr == "CLK_CORR"
			||messTypeStr == "COMB_CORR")
		{
			SSRClk ssrClk;
			ssrClk.ssrMeta		= ssrMeta;
			ssrClk.t0			= t0;
			ssrClk.udi			= ssrUpdateInterval;
			ssrClk.iod 			= iod;

			// C = C_0 + C_1(t-t_0)+C_2(t-t_0)^2 where C is a correction in meters.
			// C gets converted into a time correction for futher calculations.

			ssrClk.dclk[0]		= getbitsInc(data, i, 22) * 0.1e-3;
			ssrClk.dclk[1]		= getbitsInc(data, i, 21) * 0.001e-3;
			ssrClk.dclk[2]		= getbitsInc(data, i, 27) * 0.00002e-3;

			tracepdeex(5, std::cout, "\n#RTCM_SSR CLOCKS %s %s      %10.3f %10.3f %10.3f %d", Sat.id().c_str(), ssrClk.t0.to_string().c_str(), ssrClk.dclk[0], ssrClk.dclk[1], ssrClk.dclk[2], iod);

			ssr.ssrClk_map[receivedTime] = ssrClk;

			if (acsConfig.output_decoded_rtcm_json)
				traceSsrClk(messCode, Sat, ssrClk);
		}

		if (messTypeStr == "URA")
		{
			//std::cout << "Received SSR URA Message.\n";

			SSRUra ssrUra;
			ssrUra.ssrMeta		= ssrMeta;
			ssrUra.t0			= t0;
			ssrUra.udi 			= ssrUpdateInterval;
			ssrUra.iod 			= iod;

			int uraClassValue	= getbituInc(data, i, 6);
			ssrUra.ura			= uraSsr[uraClassValue];

			// This is the total User Range Accuracy calculated from all the SSR.
			// TODO: Check implementation, RTCM manual DF389.

			ssr.ssrUra_map[receivedTime] = ssrUra;

			if (acsConfig.output_decoded_rtcm_json)
				traceSsrUra(messCode, Sat, ssrUra);
		}

		if (messTypeStr == "HR_CLK_CORR")
		{
			SSRHRClk ssrHRClk;
			ssrHRClk.ssrMeta	= ssrMeta;
			ssrHRClk.t0			= t0;
			ssrHRClk.udi 		= ssrUpdateInterval;
			ssrHRClk.iod 		= iod;

			ssrHRClk.hrclk		= getbitsInc(data, i, 22) * 0.1e-3;

			ssr.ssrHRClk_map[receivedTime] = ssrHRClk;

			if (acsConfig.output_decoded_rtcm_json)
				traceSsrHRClk(messCode, Sat, ssrHRClk);
		}

		if (messTypeStr == "CODE_BIAS")
		{
			SSRCodeBias ssrBiasCode;
			ssrBiasCode.ssrMeta				= ssrMeta;
			ssrBiasCode.t0					= t0;
			ssrBiasCode.udi					= ssrUpdateInterval;
			ssrBiasCode.iod					= iod;

			ssrBiasCode.nbias				= getbituInc(data, i, 5);

			BiasEntry	entry;
			string		id;
			if (Sat.sys == +E_Sys::GLO)	id = Sat.id() + ":" + Sat.id();
			else						id = Sat.id() + ":" + Sat.sysChar();

			entry.measType	= CODE;
			entry.Sat		= Sat;
			entry.tini		= t0;
			entry.tfin		= entry.tini + acsConfig.ssrInOpts.code_bias_valid_time;
			entry.source	= "ssr";

			for (int k = 0; k < ssrBiasCode.nbias && i + 19 <= data.size() * 8; k++)
			{
				int		rtcmCode		= getbituInc(data, i, 5);
				double	bias			= getbitsIncScale(data, i, 14, 0.01,	&failure);

				try
				{
					E_ObsCode obsCode;
					if		(sys == +E_Sys::GPS)	{	obsCode = mCodes_gps.right.at(rtcmCode);	}
					else if (sys == +E_Sys::GLO)	{	obsCode = mCodes_glo.right.at(rtcmCode);	}
					else if (sys == +E_Sys::GAL)	{	obsCode = mCodes_gal.right.at(rtcmCode);	}
					else if (sys == +E_Sys::QZS)	{	obsCode = mCodes_qzs.right.at(rtcmCode);	}
					else if (sys == +E_Sys::BDS)	{	obsCode = mCodes_bds.right.at(rtcmCode);	}
					else if (sys == +E_Sys::SBS)	{	obsCode = mCodes_sbs.right.at(rtcmCode);	}
					else
					{
						BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system in " << __FUNCTION__;
						continue;
					}

					ssrBiasCode.obsCodeBiasMap[obsCode].bias = bias;	//todo aaron missing var

					if (acsConfig.output_decoded_rtcm_json)
						traceSsrCodeBias(messCode, Sat, obsCode, ssrBiasCode);

					entry.cod1	= obsCode;
					entry.cod2	= E_ObsCode::NONE;
					entry.bias	= bias;
					entry.var	= 0;
					entry.slop	= 0;
					entry.slpv	= 0;

					pushBiasEntry(id, entry);
					tracepdeex(5, std::cout, "\n#RTCM_SSR CODBIA for %s %s: %.4f", Sat.id().c_str(), obsCode._to_string(), bias);
				}

				catch (std::exception& e)
				{
					// BOOST_LOG_TRIVIAL(error) << "Error, Decoding SSR Message unknown RTCM code : " << rtcmCode << " for " << rtcmMountPoint << " : " << messCode;
					// BOOST_LOG_TRIVIAL(error) << "Code bias for " << Sat.id() << " rtcmCode: " << rtcmCode << ": " << bias;
					continue;
				}
			}

			ssr.ssrCodeBias_map[receivedTime] = ssrBiasCode;
		}

		if  ( messTypeStr == "PHASE_BIAS")
		{
			SSRPhasBias ssrBiasPhas;
			SSRPhase ssrPhase;
			ssrBiasPhas.ssrMeta				= ssrMeta;
			ssrBiasPhas.t0					= t0;
			ssrBiasPhas.udi 				= ssrUpdateInterval;
			ssrBiasPhas.iod 				= iod;

			ssrPhase.dispBiasConistInd		= dispBiasConistInd;
			ssrPhase.MWConistInd			= MWConistInd;

			ssrBiasPhas.nbias				= getbituInc		(data, i, 5);
			ssrPhase.yawAngle				= getbituIncScale	(data, i, 9,	1/256.0		*SC2RAD);
			ssrPhase.yawRate				= getbitsIncScale	(data, i, 8,	1/8192.0	*SC2RAD,	&failure);

			ssrBiasPhas.ssrPhase = ssrPhase;

			BiasEntry	entry;
			string		id;
			if (Sat.sys == +E_Sys::GLO)	id = Sat.id() + ":" + Sat.id();
			else						id = Sat.id() + ":" + Sat.sysChar();

			entry.measType	= PHAS;
			entry.Sat		= Sat;
			entry.tini		= t0;
			entry.tfin		= entry.tini + acsConfig.ssrInOpts.phase_bias_valid_time;
			entry.source	= "ssr";

			for (int k = 0; k < ssrBiasPhas.nbias && i + 32 <= data.size() * 8; k++)
			{
				SSRPhaseCh ssrPhaseCh;
				unsigned int rtcmCode		= getbituInc(data, i, 5);
				ssrPhaseCh.signalIntInd		= getbituInc(data, i, 1);
				ssrPhaseCh.signalWLIntInd	= getbituInc(data, i, 2);
				ssrPhaseCh.signalDisconCnt	= getbituInc(data, i, 4);
				double bias					= getbitsIncScale(data, i, 20, 0.0001,	&failure);

				try
				{
					E_ObsCode obsCode;
					if		(sys == +E_Sys::GPS)	{	obsCode = mCodes_gps.right.at(rtcmCode);	}
					else if (sys == +E_Sys::GLO)	{	obsCode = mCodes_glo.right.at(rtcmCode);	}
					else if (sys == +E_Sys::GAL)	{	obsCode = mCodes_gal.right.at(rtcmCode);	}
					else if (sys == +E_Sys::QZS)	{	obsCode = mCodes_qzs.right.at(rtcmCode);	}
					else if (sys == +E_Sys::BDS)	{	obsCode = mCodes_bds.right.at(rtcmCode);	}
					else if (sys == +E_Sys::SBS)	{	obsCode = mCodes_sbs.right.at(rtcmCode);	}
					else
					{
						BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system in " << __FUNCTION__;
						continue;
					}

					ssrBiasPhas.obsCodeBiasMap	[obsCode].bias	= bias; // offset meters due to satellite rotation.	//todo aaron missing var
					ssrBiasPhas.ssrPhaseChs		[obsCode]		= ssrPhaseCh;

					if (acsConfig.output_decoded_rtcm_json)
						traceSsrPhasBias(messCode, Sat, obsCode, ssrBiasPhas);

					entry.cod1	= obsCode;
					entry.cod2	= E_ObsCode::NONE;
					entry.bias	= bias;
					entry.var	= 0;
					entry.slop	= 0;
					entry.slpv	= 0;

					pushBiasEntry(id, entry);
					tracepdeex(5, std::cout, "\n#RTCM_SSR PHSBIA for %s %s: %.4f", Sat.id().c_str(), obsCode._to_string(), bias);
				}
				catch (std::exception& e)
				{
					// BOOST_LOG_TRIVIAL(error) << "Error, Decoding SSR Message unknown RTCM code : " << rtcmCode << " for " << rtcmMountPoint << " : " << messCode;
					continue;
				}
			}

			ssr.ssrPhasBias_map[receivedTime] = ssrBiasPhas;
		}
	}
}


/** decode RTCM navigation messages
 */
void RtcmDecoder::decodeEphemeris(
	vector<unsigned char>& data)	///< stream data
{
	Eph		eph		= {};
	Geph	geph	= {};
	int i = 0;

	int messageNumber			= getbituInc(data, i, 12);

	RtcmMessageType messCode;
	try
	{
		messCode	= RtcmMessageType::_from_integral(messageNumber);
	}
	catch (...)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message in " << __FUNCTION__;
		return;
	}

	E_Sys sys = E_Sys::NONE;
	switch (messageNumber)
	{
		case RtcmMessageType::GPS_EPHEMERIS:		sys = E_Sys::GPS;	break;
		case RtcmMessageType::GLO_EPHEMERIS:		sys = E_Sys::GLO;	break;
		case RtcmMessageType::BDS_EPHEMERIS:		sys = E_Sys::BDS;	break;
		case RtcmMessageType::QZS_EPHEMERIS:		sys = E_Sys::QZS;	break;
		case RtcmMessageType::GAL_FNAV_EPHEMERIS:	//fallthrough
		case RtcmMessageType::GAL_INAV_EPHEMERIS:	sys = E_Sys::GAL;	break;
		default:
		{
			BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message in " << __FUNCTION__;
			return;
		}
	}

	bool failure = false;

	if (sys == +E_Sys::GPS)
	{
		if (i + 488-12 > data.size() * 8)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: rtcm3 1019 length error: len=" << data.size();
			return;
		}

		eph.type	= E_NavMsgType::LNAV;

		int prn			= getbituInc(data, i,  6);
		eph.weekRollOver= getbituInc(data, i, 10);				eph.week	= adjGpsWeek(eph.weekRollOver);		// rolled-over week -> full week number
		eph.sva			= getbituInc(data, i,  4);				eph.ura[0]	= svaToUra(eph.sva);
		eph.code		= getbituInc(data, i,  2);
		eph.idot		= getbitsInc(data, i, 14)*P2_43*SC2RAD;
		eph.iode		= getbituInc(data, i,  8);
		eph.tocs		= getbituInc(data, i, 16)*16.0;
		eph.f2			= getbitsInc(data, i,  8)*P2_55;
		eph.f1			= getbitsInc(data, i, 16)*P2_43;
		eph.f0			= getbitsInc(data, i, 22)*P2_31;
		eph.iodc		= getbituInc(data, i, 10);
		eph.crs			= getbitsInc(data, i, 16)*P2_5;
		eph.deln		= getbitsInc(data, i, 16)*P2_43*SC2RAD;
		eph.M0			= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cuc			= getbitsInc(data, i, 16)*P2_29;
		eph.e			= getbituInc(data, i, 32)*P2_33;
		eph.cus			= getbitsInc(data, i, 16)*P2_29;
		eph.sqrtA		= getbituInc(data, i, 32)*P2_19;		eph.A		= SQR(eph.sqrtA);
		eph.toes		= getbituInc(data, i, 16)*16.0;
		eph.cic			= getbitsInc(data, i, 16)*P2_29;
		eph.OMG0		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cis			= getbitsInc(data, i, 16)*P2_29;
		eph.i0			= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.crc			= getbitsInc(data, i, 16)*P2_5;
		eph.omg			= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.OMGd		= getbitsInc(data, i, 24)*P2_43*SC2RAD;
		eph.tgd[0]		= getbitsInc(data, i,  8)*P2_31;
		int svh			= getbituInc(data, i,  6);				eph.svh   	= (E_Svh)svh;
		eph.flag		= getbituInc(data, i,  1);
		eph.fitFlag		= getbituInc(data, i,  1);				eph.fit		= eph.fitFlag?0.0:4.0;	// 0:4hr,1:>4hr

		if (prn >= 40)
		{
			sys = E_Sys::SBS;
			prn += 80;
		}
		eph.Sat		= SatSys(sys, prn);

		GTime nearTime	= rtcmTime();

		eph.ttm		= nearTime;
		eph.toe		= GTime(GTow(eph.toes), nearTime);
		eph.toc		= GTime(GTow(eph.tocs), nearTime);

		if (acsConfig.use_tgd_bias)
			decomposeTGDBias(eph.Sat, eph.tgd[0]);
	}
	else if (sys == +E_Sys::GLO)
	{
		if (i + 360-12 > data.size() * 8)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: rtcm3 1020 length error: len=" << data.size();
			return;
		}

		geph.type	= E_NavMsgType::FDMA;

		int prn			= getbituInc(data, i,  6);
		geph.frq		= getbituInc(data, i,  5)-7;			i +=  4;	// skip DF104, 105, 106 (almanac health, P1)
        geph.tk_hour	= getbituInc(data, i,  5);
        geph.tk_min		= getbituInc(data, i,  6);
        geph.tk_sec		= getbituInc(data, i,  1)*30;
		int svh			= getbituInc(data, i,  1);				geph.svh	= (E_Svh)svh;

		int dummy		= getbituInc(data, i,  1);				// skip DF109 (P2)
		geph.tb			= getbituInc(data, i,  7);				geph.iode	= geph.tb;
		geph.vel[0]		= getbitgInc(data, i, 24)*P2_20*1E3;
		geph.pos[0]		= getbitgInc(data, i, 27)*P2_11*1E3;
		geph.acc[0]		= getbitgInc(data, i,  5)*P2_30*1E3;
		geph.vel[1]		= getbitgInc(data, i, 24)*P2_20*1E3;
		geph.pos[1]		= getbitgInc(data, i, 27)*P2_11*1E3;
		geph.acc[1]		= getbitgInc(data, i,  5)*P2_30*1E3;
		geph.vel[2]		= getbitgInc(data, i, 24)*P2_20*1E3;
		geph.pos[2]		= getbitgInc(data, i, 27)*P2_11*1E3;
		geph.acc[2]		= getbitgInc(data, i,  5)*P2_30*1E3;
		dummy			= getbituInc(data, i,  1);					// skip DF120 (P3)
		geph.gammaN		= getbitgInc(data, i, 11)*P2_40;
		dummy			= getbituInc(data, i,  3);					// skip DF122, 123 (P, ln)
		geph.taun		= getbitgInc(data, i, 22)*P2_30;
		geph.dtaun		= getbitgInc(data, i,  5)*P2_30;
		geph.age		= getbituInc(data, i,  5);
		dummy			= getbituInc(data, i,  5);							// skip DF127, 128 (P4, FT)
		geph.NT			= getbituInc(data, i, 11);							// GLONASS-M only, may be arbitrary value
		geph.glonassM	= getbituInc(data, i,  2);							// if GLONASS-M data feilds valid
		geph.moreData	= getbituInc(data, i,  1);							// availability of additional data
																i += 43;	// skip DF132, 133 (NA, tauc)
		geph.N4			= getbituInc(data, i,  5);							// additional data and GLONASS-M only, may be arbitrary value

		geph.Sat	= SatSys(sys, prn);

		RTod	toes	= geph.tb		* 15 * 60;

		RTod	tofs	= geph.tk_hour	* 60 * 60
						+ geph.tk_min	* 60
						+ geph.tk_sec;

		GTime nearTime = rtcmTime();
		geph.toe = GTime(toes, nearTime);
		geph.tof = GTime(tofs, nearTime);
	}
	else if (sys == +E_Sys::BDS)
	{
		if (i + 511-12 > data.size() * 8)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: rtcm3 1042 length error: len=" << data.size();
			return;
		}

		eph.type	= E_NavMsgType::D1;

		int prn			= getbituInc(data, i,  6);
		eph.weekRollOver= getbituInc(data, i, 13);				eph.week	= adjBdtWeek(eph.weekRollOver);		// rolled-over week -> full week number
		eph.sva   		= getbituInc(data, i,  4);				eph.ura[0]	= svaToUra(eph.sva);
		eph.idot  		= getbitsInc(data, i, 14)*P2_43*SC2RAD;
		eph.aode  		= getbituInc(data, i,  5);
		eph.tocs     	= getbituInc(data, i, 17)*8.0;
		eph.f2    		= getbitsInc(data, i, 11)*P2_66;
		eph.f1    		= getbitsInc(data, i, 22)*P2_50;
		eph.f0    		= getbitsInc(data, i, 24)*P2_33;
		eph.aodc  		= getbituInc(data, i,  5);
		eph.crs   		= getbitsInc(data, i, 18)*P2_6;
		eph.deln  		= getbitsInc(data, i, 16)*P2_43*SC2RAD;
		eph.M0    		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cuc   		= getbitsInc(data, i, 18)*P2_31;
		eph.e     		= getbituInc(data, i, 32)*P2_33;
		eph.cus   		= getbitsInc(data, i, 18)*P2_31;
		eph.sqrtA   	= getbituInc(data, i, 32)*P2_19;		eph.A		= SQR(eph.sqrtA);
		eph.toes  		= getbituInc(data, i, 17)*8.0;
		eph.cic   		= getbitsInc(data, i, 18)*P2_31;
		eph.OMG0  		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cis   		= getbitsInc(data, i, 18)*P2_31;
		eph.i0    		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.crc   		= getbitsInc(data, i, 18)*P2_6;
		eph.omg   		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.OMGd  		= getbitsInc(data, i, 24)*P2_43*SC2RAD;
		eph.tgd[0]		= getbitsInc(data, i, 10)*1E-10;
		eph.tgd[1]		= getbitsInc(data, i, 10)*1E-10;
		int svh			= getbituInc(data, i,  1);				eph.svh		= (E_Svh)svh;

		eph.Sat		= SatSys(sys, prn);

		eph.iode	= int(eph.tocs / 720) % 240;
		eph.iodc	= eph.iode + 256 * int(eph.tocs / 172800) % 4;

		GTime nearTime	= rtcmTime();

		eph.ttm		= nearTime;
		eph.toe		= GTime(BTow(eph.toes), nearTime);
		eph.toc		= GTime(BTow(eph.tocs), nearTime);
	}
	else if (sys == +E_Sys::QZS)
	{
		if (i + 485-12 > data.size() * 8)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: rtcm3 1044 length error: len=" << data.size();
			return;
		}

		eph.type	= E_NavMsgType::LNAV;

		int prn			= getbituInc(data, i,  4);
		eph.tocs     	= getbituInc(data, i, 16)*16.0;
		eph.f2    		= getbitsInc(data, i,  8)*P2_55;
		eph.f1    		= getbitsInc(data, i, 16)*P2_43;
		eph.f0    		= getbitsInc(data, i, 22)*P2_31;
		eph.iode  		= getbituInc(data, i,  8);
		eph.crs   		= getbitsInc(data, i, 16)*P2_5;
		eph.deln  		= getbitsInc(data, i, 16)*P2_43*SC2RAD;
		eph.M0    		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cuc   		= getbitsInc(data, i, 16)*P2_29;
		eph.e     		= getbituInc(data, i, 32)*P2_33;
		eph.cus   		= getbitsInc(data, i, 16)*P2_29;
		eph.sqrtA		= getbituInc(data, i, 32)*P2_19;		eph.A		= SQR(eph.sqrtA);
		eph.toes  		= getbituInc(data, i, 16)*16.0;
		eph.cic   		= getbitsInc(data, i, 16)*P2_29;
		eph.OMG0  		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cis   		= getbitsInc(data, i, 16)*P2_29;
		eph.i0    		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.crc   		= getbitsInc(data, i, 16)*P2_5;
		eph.omg   		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.OMGd  		= getbitsInc(data, i, 24)*P2_43*SC2RAD;
		eph.idot  		= getbitsInc(data, i, 14)*P2_43*SC2RAD;
		eph.code  		= getbituInc(data, i,  2);
		eph.weekRollOver= getbituInc(data, i, 10);				eph.week	= adjGpsWeek(eph.weekRollOver);		// rolled-over week -> full week number
		eph.sva   		= getbituInc(data, i,  4);				eph.ura[0]	= svaToUra(eph.sva);
		int svh			= getbituInc(data, i,  6);				eph.svh		= (E_Svh)svh;
		eph.tgd[0]		= getbitsInc(data, i,  8)*P2_31;
		eph.iodc  		= getbituInc(data, i, 10);
		eph.fitFlag		= getbituInc(data, i,  1);				eph.fit		= eph.fitFlag?0.0:2.0; // 0:2hr,1:>2hr

		eph.Sat		= SatSys(sys, prn);

		GTime nearTime	= rtcmTime();

		eph.ttm		= nearTime;
		eph.toe		= GTime(GTow(eph.toes), nearTime);
		eph.toc		= GTime(GTow(eph.tocs), nearTime);
		if (acsConfig.use_tgd_bias)
			decomposeTGDBias(eph.Sat, eph.tgd[0]);
	}
	else if (sys == +E_Sys::GAL)
	{
		if		(messageNumber == RtcmMessageType::GAL_FNAV_EPHEMERIS )
		{
			if (i + 496-12 > data.size() * 8)
			{
				BOOST_LOG_TRIVIAL(error) << "Error: rtcm3 1045 length error: len=" << data.size();
				return;
			}

			eph.type	= E_NavMsgType::FNAV;
		}
		else if (messageNumber == RtcmMessageType::GAL_INAV_EPHEMERIS)
		{
			if (i + 504-12 > data.size() * 8)
			{
				BOOST_LOG_TRIVIAL(error) << "Error: rtcm3 1046 length error: len=" << data.size();
				return;
			}

			eph.type	= E_NavMsgType::INAV;
		}
		else
		{
			BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message for GAL in " << __FUNCTION__;
		}

		int prn			= getbituInc(data, i, 6);
		eph.weekRollOver= getbituInc(data, i, 12);				eph.week	= adjGstWeek(eph.weekRollOver) + 1024;	// rolled-over week -> full week number and align to GPST
		eph.iode  		= getbituInc(data, i, 10);				eph.iodc	= eph.iode;						// Documented as IODnav
		eph.sva   		= getbituInc(data, i,  8);				eph.ura[0]	= svaToSisa(eph.sva);			// Documented SISA
		eph.idot  		= getbitsInc(data, i, 14)*P2_43*SC2RAD;
		eph.tocs     	= getbituInc(data, i, 14)*60.0;
		eph.f2    		= getbitsInc(data, i,  6)*P2_59;
		eph.f1    		= getbitsInc(data, i, 21)*P2_46;
		eph.f0    		= getbitsInc(data, i, 31)*P2_34;
		eph.crs   		= getbitsInc(data, i, 16)*P2_5;
		eph.deln  		= getbitsInc(data, i, 16)*P2_43*SC2RAD;
		eph.M0    		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cuc   		= getbitsInc(data, i, 16)*P2_29;
		eph.e     		= getbituInc(data, i, 32)*P2_33;
		eph.cus   		= getbitsInc(data, i, 16)*P2_29;
		eph.sqrtA    	= getbituInc(data, i, 32)*P2_19;		eph.A		= SQR(eph.sqrtA);
		eph.toes  		= getbituInc(data, i, 14)*60.0;
		eph.cic   		= getbitsInc(data, i, 16)*P2_29;
		eph.OMG0  		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.cis   		= getbitsInc(data, i, 16)*P2_29;
		eph.i0    		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.crc   		= getbitsInc(data, i, 16)*P2_5;
		eph.omg   		= getbitsInc(data, i, 32)*P2_31*SC2RAD;
		eph.OMGd  		= getbitsInc(data, i, 24)*P2_43*SC2RAD;
		eph.tgd[0]		= getbitsInc(data, i, 10)*P2_32;

		if		(messageNumber == RtcmMessageType::GAL_FNAV_EPHEMERIS)
		{
			eph.e5a_hs		= getbituInc(data, i,  2);			// OSHS
			eph.e5a_dvs		= getbituInc(data, i,  1);			// OSDVS
			// eph.rsv			= getbituInc(data, i,  7);

			int svh		= (eph.e5a_hs	<< 4)
						+ (eph.e5a_dvs	<< 3);
			eph.svh		= (E_Svh)svh;
			eph.code	= (1<<1)+(1<<8); // data source = F/NAV+E5a
		}
		else if (messageNumber == RtcmMessageType::GAL_INAV_EPHEMERIS)
		{
			eph.tgd[1]		= getbitsInc(data, i, 10)*P2_32;	// E5b/E1
			eph.e5b_hs		= getbituInc(data, i,  2);			// E5b OSHS
			eph.e5b_dvs		= getbituInc(data, i,  1);			// E5b OSDVS
			eph.e1_hs		= getbituInc(data, i,  2);			// E1 OSHS
			eph.e1_dvs		= getbituInc(data, i,  1);			// E1 OSDVS

			int svh		= (eph.e5b_hs	<< 7)
						+ (eph.e5b_dvs	<< 6)
						+ (eph.e1_hs	<< 1)
						+ (eph.e1_dvs	<< 0);
			eph.svh		= (E_Svh)svh;
			eph.code	= (1<<0)+(1<<2)+(1<<9); // data source = I/NAV+E1+E5b

			if (acsConfig.use_tgd_bias)
				decomposeBGDBias(eph.Sat, eph.tgd[0], eph.tgd[1]);
		}

		eph.Sat		= SatSys(sys, prn);

		GTime nearTime	= rtcmTime();

		eph.ttm		= nearTime;
		eph.toe		= GTime(GTow(eph.toes), nearTime);
		eph.toc		= GTime(GTow(eph.tocs), nearTime);
	}
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system in " << __FUNCTION__;
		return;
	}

	if	( sys == +E_Sys::GPS
		||sys == +E_Sys::GAL
		||sys == +E_Sys::BDS
		||sys == +E_Sys::QZS)
	{
		nav.ephMap[eph.Sat][eph.type][eph.toe] = eph;

		traceTrivialDebug("#RTCM_BRD EPHEMR %s %s %d", eph.Sat.id().c_str(), eph.toe.to_string().c_str(), eph.iode);

		if (acsConfig.output_decoded_rtcm_json)
			traceBrdcEph(messCode, eph);
	}
	else if (sys == +E_Sys::GLO)
	{
		nav.gephMap[geph.Sat][geph.type][geph.toe] = geph;

		traceTrivialDebug("#RTCM_BRD EPHEMR %s %s %d", geph.Sat.id().c_str(), geph.toe.to_string().c_str(), geph.iode);

		if (acsConfig.output_decoded_rtcm_json)
			traceBrdcEph(messCode, geph);
	}
	else
	{
		if (acsConfig.output_decoded_rtcm_json)
			traceUnknown();
	}
}


// sys -> rtcm signal enum -> siginfo (sig enum,// From the RTCM spec...
//  - table 3.5-91 (GPS)
//  - table 3.5-96 (GLONASS)
//  - table 3.5-99 (GALILEO)
//  - table 3.5-105 (QZSS)
//  - table 3.5-108 (BEIDOU)
map<E_Sys, map<uint8_t, SignalInfo>> sysIdSignalMapMap =
{
	{	E_Sys::GPS,
		{
			{2,		{2,		F1,		E_ObsCode::L1C}},
			{3,		{3,		F1,		E_ObsCode::L1P}},
			{4,		{4,		F1,		E_ObsCode::L1W}},

			{8,		{8,		F2,		E_ObsCode::L2C}},
			{9,		{9,		F2,		E_ObsCode::L2P}},
			{10,	{10,	F2,		E_ObsCode::L2W}},
			{15,	{15,	F2,		E_ObsCode::L2S}},
			{16,	{16,	F2,		E_ObsCode::L2L}},
			{17,	{17,	F2,		E_ObsCode::L2X}},

			{22,	{22,	F5,		E_ObsCode::L5I}},
			{23,	{23,	F5,		E_ObsCode::L5Q}},
			{24,	{24,	F5,		E_ObsCode::L5X}},

			{30,	{2,		F1,		E_ObsCode::L1S}},
			{31,	{2,		F1,		E_ObsCode::L1L}},
			{32,	{2,		F1,		E_ObsCode::L1X}}
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
			{2,		{2,		F1,		E_ObsCode::L1C}},
			{3,		{3,		F1,		E_ObsCode::L1A}},
			{4,		{4,		F1,		E_ObsCode::L1B}},
			{5,		{5,		F1,		E_ObsCode::L1X}},
			{6,		{6,		F1,		E_ObsCode::L1Z}},

			{8,		{8,		F6,		E_ObsCode::L6C}},
			{9,		{9,		F6,		E_ObsCode::L6A}},
			{10,	{10,	F6,		E_ObsCode::L6B}},
			{11,	{11,	F6,		E_ObsCode::L6X}},
			{12,	{12,	F6,		E_ObsCode::L6Z}},

			{14,	{14,	F7,		E_ObsCode::L7I}},
			{15,	{15,	F7,		E_ObsCode::L7Q}},
			{16,	{16,	F7,		E_ObsCode::L7X}},

			{18,	{18,	F8,		E_ObsCode::L8I}},
			{19,	{19,	F8,		E_ObsCode::L8Q}},
			{20,	{20,	F8,		E_ObsCode::L8X}},

			{22,	{22,	F5,		E_ObsCode::L5I}},
			{23,	{23,	F5,		E_ObsCode::L5Q}},
			{24,	{24,	F5,		E_ObsCode::L5X}}
		}
	},

	{	E_Sys::QZS,
		{
			{2,		{2,		F1,		E_ObsCode::L1C}},

			{9,		{9,		F6,		E_ObsCode::L6S}},
			{10,	{10,	F6,		E_ObsCode::L6L}},
			{11,	{11,	F6,		E_ObsCode::L6X}},

			{15,	{15,	F2,		E_ObsCode::L2S}},
			{16,	{16,	F2,		E_ObsCode::L2L}},
			{17,	{17,	F2,		E_ObsCode::L2X}},

			{22,	{22,	F5,		E_ObsCode::L5I}},
			{23,	{23,	F5,		E_ObsCode::L5Q}},
			{24,	{24,	F5,		E_ObsCode::L5X}},

			{30,	{30,	F1,		E_ObsCode::L1S}},
			{31,	{31,	F1,		E_ObsCode::L1L}},
			{32,	{32,	F1,		E_ObsCode::L1X}}
		}
	},

	{	E_Sys::BDS,
		{
			{2,		{2,		B1,		E_ObsCode::L2I}},
			{3,		{3,		B1,		E_ObsCode::L2Q}},
			{4,		{4,		B1,		E_ObsCode::L2X}},

			{8,		{8,		B3,		E_ObsCode::L6I}},
			{9,		{9,		B3,		E_ObsCode::L6Q}},
			{10,	{10,	B3,		E_ObsCode::L6X}},

			{14,	{14,	F7,		E_ObsCode::L7I}},
			{15,	{15,	F7,		E_ObsCode::L7Q}},
			{16,	{16,	F7,		E_ObsCode::L7X}},

			{22,	{22,	F5,		E_ObsCode::L5D}},
			{23,	{23,	F5,		E_ObsCode::L5P}},
			{24,	{24,	F5,		E_ObsCode::L5X}},
			{25,	{25,	F7,		E_ObsCode::L7D}},

			{30,	{30,	F1,		E_ObsCode::L1D}},
			{31,	{31,	F1,		E_ObsCode::L1P}},
			{32,	{32,	F1,		E_ObsCode::L1X}}
		}
	}
};

E_ObsCode RtcmDecoder::signal_to_code(E_Sys sys, uint8_t signal)
{
	auto it1 = sysIdSignalMapMap.find(sys);
	if (it1 == sysIdSignalMapMap.end())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: unrecognised system in " << __FUNCTION__ << ": mountpoint=" << rtcmMountpoint << " sys=" << sys;

		return E_ObsCode::NONE;
	}

	auto& [dummy1, idSignalMap] = *it1;

	auto it2 = idSignalMap.find(signal);
	if (it2 == idSignalMap.end())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: unrecognised signal in " << __FUNCTION__ << ": mountpoint=" << rtcmMountpoint << " sys=" << sys << " signal=" << (int)signal;

		return E_ObsCode::NONE;
	}

	auto& [dummy2, sigInfo] = *it2;

	return sigInfo.obsCode;
}

const int defaultGloChannel[] = { 0, 1, -4, 5, 6, 1, -4, 5, 6, -2, -7, 0, -1, -2, -7, 0, -1, 4, -3, 3, 2, 4, -3, 3, 2 };

double lockTimeFromIndicator(
	int indicator)
{
	if (indicator == 0)
	{
		return 0;
	}

	double lockTime = pow(2, 4 + indicator) / 1000.0;

	return lockTime;
}

double highResLockTimeFromIndicator(
	int indicator)
{
	int		i = indicator;
	double	lockTime = 0;
	if		(i < 64)		lockTime = 1		* i - 0;
	else if (i < 96)		lockTime = 2		* i - 64;
	else if (i < 128)		lockTime = 4		* i - 256;
	else if (i < 160)		lockTime = 8		* i - 768;
	else if (i < 192)		lockTime = 16		* i - 2048;
	else if (i < 224)		lockTime = 32		* i - 5120;
	else if (i < 256)		lockTime = 64		* i - 12288;
	else if (i < 288)		lockTime = 128		* i - 28672;
	else if (i < 320)		lockTime = 256		* i - 65536;
	else if (i < 352)		lockTime = 512		* i - 147456;
	else if (i < 384)		lockTime = 1024		* i - 327680;
	else if (i < 416)		lockTime = 2048		* i - 720896;
	else if (i < 448)		lockTime = 4096		* i - 1572864;
	else if (i < 480)		lockTime = 8192		* i - 3407872;
	else if (i < 512)		lockTime = 16384	* i - 7340032;
	else if (i < 544)		lockTime = 32768	* i - 15728640;
	else if (i < 576)		lockTime = 65536	* i - 33554432;
	else if (i < 608)		lockTime = 131072	* i - 71303168;
	else if (i < 640)		lockTime = 262144	* i - 150994944;
	else if (i < 672)		lockTime = 524288	* i - 318767104;
	else if (i < 704)		lockTime = 1048576	* i - 671088640;
	else if (i== 704)		lockTime = 2097152	* i - 1409286144;
	else
		BOOST_LOG_TRIVIAL(warning) << "Warning: High res lock time out of bounds: " << lockTime;

	lockTime /= 1000;

	return lockTime;
}

ObsList RtcmDecoder::decodeMSM(
	vector<unsigned char>& data)
{
	ObsList obsList;
	int i = 0;

	int messageNumber				= getbituInc(data, i,	12);
	int reference_station_id		= getbituInc(data, i,	12);
	int epoch_time_					= getbituInc(data, i,	30);
	int multiple_message			= getbituInc(data, i,	1);
	int issue_of_data_station		= getbituInc(data, i,	3);
	int reserved					= getbituInc(data, i,	7);
	int clock_steering_indicator	= getbituInc(data, i,	2);
	int external_clock_indicator	= getbituInc(data, i,	2);
	int smoothing_indicator			= getbituInc(data, i,	1);
	int smoothing_interval			= getbituInc(data, i,	3);

	RtcmMessageType messCode;
	try
	{
		messCode	= RtcmMessageType::_from_integral(messageNumber);
	}
	catch (...)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message in " << __FUNCTION__;
		return obsList;
	}

	int msmtyp = messageNumber % 10;
	int lsat = 0;
	int lcell = 0;
	switch (msmtyp)
	{
		case 4:		lsat = 18;	lcell = 48;	break;
		case 5:		lsat = 36;	lcell = 63;	break;
		case 6:		lsat = 18;	lcell = 65;	break;
		case 7:		lsat = 36;	lcell = 80;	break;
		default:	BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message in " << __FUNCTION__;	return obsList;
	}

	bool extrainfo = false;
	if 	(  msmtyp == 5
		|| msmtyp == 7)
	{
		extrainfo = true;
	}

	int nbcd = 15;
	int nbph = 22;
	int nblk = 4;
	int nbcn = 6;
	double sccd = P2_24;
	double scph = P2_29;
	double scsn = 1;
	if	(  msmtyp == 6
		|| msmtyp == 7)
	{
		nbcd = 20; sccd = P2_29;
		nbph = 24; scph = P2_31;
		nblk = 10;
		nbcn = 10; scsn = 0.0625;
	}

	int sysind = messageNumber / 10; // integer division is intentional
	E_Sys rtcmsys = E_Sys::NONE;


	GTime nearTime	= rtcmTime();

	double tow = epoch_time_ * 0.001;
	GTime tobs;

	switch (sysind)
	{
		case 107:	rtcmsys = E_Sys::GPS;		tobs = GTime(GTow(tow), nearTime);		break;
		case 108:	rtcmsys = E_Sys::GLO;		/*see below*/							break;
		case 109:	rtcmsys = E_Sys::GAL;		tobs = GTime(GTow(tow), nearTime);		break;
		case 111:	rtcmsys = E_Sys::QZS;		tobs = GTime(GTow(tow), nearTime);		break;
		case 112:	rtcmsys = E_Sys::BDS;		tobs = GTime(BTow(tow), nearTime);		break;
		default:	BOOST_LOG_TRIVIAL(error) << "Error: unrecognised message in " << __FUNCTION__;	return obsList;
	}

	if (rtcmsys == +E_Sys::GLO)
	{
		int dowi = (epoch_time_ >> 27);
		int todi = (epoch_time_ & 0x7FFFFFF);

		RTod tk	= 0.001 * todi;
		tobs = GTime(tk, nearTime);
	}

	traceLatency(tobs);

	//create observations for satellites according to the mask
	int nsat = 0;
	for (int sat = 0; sat < 64; sat++)
	{
		bool mask 					= getbituInc(data, i,	1);
		if (mask == false)
		{
			continue;
		}

		GObs obs;
		obs.Sat = SatSys(rtcmsys, sat + 1);
		obs.time	= tobs;

		obsList.push_back((shared_ptr<GObs>)obs);

// 		std::cout << obs.time << " " << obs.Sat.id() << "\n";

		nsat++;
	}

	//create a temporary list of signals
	vector<E_ObsCode> signalMaskList;
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
	map<int, Sig*>		signalPointermap;
	map<int, SatSys>	cellSatellitemap;
	int ncell = 0;
	//create signals for observations according to existing observations, the list of signals, and the cell mask
	for (auto& obs		: only<GObs>(obsList))
	for (auto& sigNum	: signalMaskList)
	{
		bool mask 					= getbituInc(data, i,	1);
		if (mask == false)
		{
			continue;
		}

		Sig sig;
		sig.code = sigNum;

		E_FType ft = FTYPE_NONE;
		if (code2Freq.find(rtcmsys) != code2Freq.end())
		{
			if (code2Freq[rtcmsys].find(sig.code) != code2Freq[rtcmsys].end())	// must not skip unknwon/unsupported systems or signals in the list of signals -- unknown != no observation
			{
				ft = code2Freq[rtcmsys][sig.code];
			}
			else
			{
				BOOST_LOG_TRIVIAL(warning) << "Warning: unrecognised signal in " << __FUNCTION__ << ": mountpoint=" << rtcmMountpoint << " messageNumber=" << messageNumber << " signal=" << sig.code;
			}
		}
		else
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: unrecognised system in " << __FUNCTION__ << ": mountpoint=" << rtcmMountpoint << "messageNumber=" << messageNumber;
		}

		obs.sigsLists[ft].push_back(sig);

		Sig* pointer = &obs.sigsLists[ft].back();
		signalPointermap[ncell] = pointer;
		cellSatellitemap[ncell] = obs.Sat;
		ncell++;
	}

	if (i + nsat * lsat + ncell * lcell > data.size() * 8)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: rtcm3 " << messageNumber << " length error: len=" << data.size();
		return obsList;
	}

	//get satellite specific data - needs to be in chunks
	for (auto& obs : only<GObs>(obsList))
	{
		int ms_rough_range			= getbituInc(data, i,	8);
		if (ms_rough_range == 255)
		{
			obs.excludeBadRange = true;

			continue;
		}

		for (auto& [ft, sigList]	: obs.sigsLists)
		for (auto& sig				: sigList)
		{
			sig.P = ms_rough_range;
			sig.L = ms_rough_range;
		}
	}

	map<SatSys, map<E_FType, double>>	GLOFreqShift;
	if (extrainfo)
	{
		for (auto& obs : only<GObs>(obsList))
		{
			int extended_sat_info		= getbituInc(data, i,	4);

			if (rtcmsys == +E_Sys::GLO)
			{
				GLOFreqShift[obs.Sat][G1] = DFRQ1_GLO * (extended_sat_info-7);
				GLOFreqShift[obs.Sat][G2] = DFRQ2_GLO * (extended_sat_info-7);
			}
		}
	}
	else if(rtcmsys == +E_Sys::GLO)
	{
		for (auto& obs : only<GObs>(obsList))
		{
			short int prn = obs.Sat.prn;

			if	(  prn > 24
				|| prn < 1)
			{
				GLOFreqShift[obs.Sat][G1] = 0;
				GLOFreqShift[obs.Sat][G2] = 0;
			}
			else
			{
				GLOFreqShift[obs.Sat][G1] = DFRQ1_GLO * defaultGloChannel[prn];
				GLOFreqShift[obs.Sat][G2] = DFRQ2_GLO * defaultGloChannel[prn];
			}
		}
	}

	for (auto& obs : only<GObs>(obsList))
	{
		int rough_range_modulo		= getbituInc(data, i,	10);

		for (auto& [ft, sigList]	: obs.sigsLists)
		for (auto& sig				: sigList)
		{
			sig.P += rough_range_modulo * P2_10;
			sig.L += rough_range_modulo * P2_10;
		}
	}

	if (extrainfo)
	for (auto& obs : only<GObs>(obsList))
	{
		bool failure = false;
		double rough_doppler			= getbitsIncScale(data, i,	14, 1, &failure);
		if (failure)
		{
			continue;
		}

		for (auto& [ft, sigList]	: obs.sigsLists)
		for (auto& sig				: sigList)
		{
			sig.D = rough_doppler;
		}
	}

	//get signal specific data
	for (auto& [indx, signalPointer] : signalPointermap)
	{
		Sig& sig = *signalPointer;

		bool failure = false;
		double fine_pseudorange		= getbitsIncScale(data, i,	nbcd, sccd, &failure);
		if (failure)
		{
			sig.invalid = true;
			continue;
		}

		sig.P += fine_pseudorange;
	}

	for (auto& [indx, signalPointer] : signalPointermap)
	{
		Sig& sig = *signalPointer;

		bool failure = false;
		double fine_phase_range		= getbitsIncScale(data, i,	nbph, scph, &failure);
		if (failure)
		{
			sig.invalid = true;

			continue;
		}

		sig.L += fine_phase_range;
	}

	for (auto& [indx, signalPointer] : signalPointermap)
	{
		int lockTimeIndicator		= getbituInc(data, i,	nblk);

		double lockTime = 0;
		if		(  msmtyp <= 5)		lockTime = lockTimeFromIndicator(lockTimeIndicator);
		else if	(  msmtyp == 6
				|| msmtyp == 7)		lockTime = highResLockTimeFromIndicator(lockTimeIndicator);

		Sig&	sig = *signalPointer;
		SatSys&	Sat = cellSatellitemap[indx];

		if (lockTime < acsConfig.epoch_interval)	sig.LLI = true;
		else 										sig.LLI = false;
	}

	for (auto& [indx, signalPointer] : signalPointermap)
	{
		int half_cycle_ambiguity	= getbituInc(data, i,	1);

		Sig& sig = *signalPointer;

		if (half_cycle_ambiguity > 0)
			sig.LLI = true;
	}

	for (auto& [indx, signalPointer] : signalPointermap)
	{
		double carrier_noise_ratio		= getbituIncScale(data, i,	nbcn, scsn);

		Sig& sig = *signalPointer;
		sig.snr = carrier_noise_ratio;
	}

	if (extrainfo)
	for (auto& [indx, signalPointer] : signalPointermap)
	{
		bool failure = false;
		double fine_doppler			= getbitsIncScale(data, i,	15, 0.0001, &failure);

		if (failure)
			continue;

		Sig& sig = *signalPointer;

		sig.D += fine_doppler;
	}


	//convert millisecond or m/s measurements to meters or cycles or Hz
	for (auto& obs				: only<GObs>(obsList))
	for (auto& [ft, sigList]	: obs.sigsLists)
	for (auto& sig				: sigList)
	{
		double freqcy = carrierFrequency[ft];
		if (rtcmsys == +E_Sys::GLO)
			freqcy += GLOFreqShift[obs.Sat][ft];

		sig.P *=  CLIGHT	/ 1000;		// ms  -> metre
		sig.L *=  freqcy	/ 1000;		// ms  -> cycle
		sig.D *= -freqcy	/ CLIGHT;	// m/s -> Hz


		traceTrivialDebug("#RTCM_MSM OBSERV %s %s %d %s %.4f %.4f", obs.time.to_string().c_str(), obs.Sat.id().c_str(), ft, sig.code._to_string(), sig.P, sig.L);

		if (acsConfig.output_decoded_rtcm_json)
			traceMSM(messCode, obs.time, obs.Sat, sig);
	}


	return obsList;
}


void RtcmDecoder::traceLatency(GTime tobs)
{
	GTime now = timeGet();

	double latency = (now - tobs).to_double();

	//std::cout << "traceLatency : " << latency << " seconds.\n";
	totalLatency += latency;
	numMessagesLatency++;
}

E_ReturnType RtcmDecoder::decode(
	vector<unsigned char>&	message)
{
	E_ReturnType retVal = E_ReturnType::OK;

	int messageNumber			= getbitu(message, 0, 12);

// 	std::cout << "\n" << "Received " << RtcmMessageType::_from_integral(messageNumber)._to_string();

	switch (messageNumber)
	{
		default:											retVal =	E_ReturnType::UNSUPPORTED;	break;

		case +RtcmMessageType::CUSTOM:						retVal =	decodeCustom	(message);	break;
		case +RtcmMessageType::GPS_EPHEMERIS:			//fallthrough
		case +RtcmMessageType::GLO_EPHEMERIS:			//fallthrough
		case +RtcmMessageType::BDS_EPHEMERIS:			//fallthrough
		case +RtcmMessageType::QZS_EPHEMERIS:			//fallthrough
		case +RtcmMessageType::GAL_INAV_EPHEMERIS:		//fallthrough
		case +RtcmMessageType::GAL_FNAV_EPHEMERIS:						decodeEphemeris	(message);	break;

		case +RtcmMessageType::GPS_SSR_ORB_CORR:		//fallthrough
		case +RtcmMessageType::GPS_SSR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::GPS_SSR_COMB_CORR:		//fallthrough
		case +RtcmMessageType::GPS_SSR_CODE_BIAS:		//fallthrough
		case +RtcmMessageType::GPS_SSR_PHASE_BIAS:		//fallthrough
		case +RtcmMessageType::GPS_SSR_URA:				//fallthrough
		case +RtcmMessageType::GPS_SSR_HR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::GLO_SSR_ORB_CORR:		//fallthrough
		case +RtcmMessageType::GLO_SSR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::GLO_SSR_COMB_CORR:		//fallthrough
		case +RtcmMessageType::GLO_SSR_CODE_BIAS:		//fallthrough
		case +RtcmMessageType::GLO_SSR_PHASE_BIAS:		//fallthrough
		case +RtcmMessageType::GLO_SSR_URA:				//fallthrough
		case +RtcmMessageType::GLO_SSR_HR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::GAL_SSR_ORB_CORR:		//fallthrough
		case +RtcmMessageType::GAL_SSR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::GAL_SSR_COMB_CORR:		//fallthrough
		case +RtcmMessageType::GAL_SSR_CODE_BIAS:		//fallthrough
		case +RtcmMessageType::GAL_SSR_PHASE_BIAS:		//fallthrough
		case +RtcmMessageType::GAL_SSR_URA:				//fallthrough
		case +RtcmMessageType::GAL_SSR_HR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::QZS_SSR_ORB_CORR:		//fallthrough
		case +RtcmMessageType::QZS_SSR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::QZS_SSR_COMB_CORR:		//fallthrough
		case +RtcmMessageType::QZS_SSR_CODE_BIAS:		//fallthrough
		case +RtcmMessageType::QZS_SSR_PHASE_BIAS:		//fallthrough
		case +RtcmMessageType::QZS_SSR_URA:				//fallthrough
		case +RtcmMessageType::QZS_SSR_HR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::BDS_SSR_ORB_CORR:		//fallthrough
		case +RtcmMessageType::BDS_SSR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::BDS_SSR_COMB_CORR:		//fallthrough
		case +RtcmMessageType::BDS_SSR_CODE_BIAS:		//fallthrough
		case +RtcmMessageType::BDS_SSR_PHASE_BIAS:		//fallthrough
		case +RtcmMessageType::BDS_SSR_URA:				//fallthrough
		case +RtcmMessageType::BDS_SSR_HR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::SBS_SSR_ORB_CORR:		//fallthrough
		case +RtcmMessageType::SBS_SSR_CLK_CORR:		//fallthrough
		case +RtcmMessageType::SBS_SSR_COMB_CORR:		//fallthrough
		case +RtcmMessageType::SBS_SSR_CODE_BIAS:		//fallthrough
		case +RtcmMessageType::SBS_SSR_PHASE_BIAS:		//fallthrough
		case +RtcmMessageType::SBS_SSR_URA:				//fallthrough
		case +RtcmMessageType::SBS_SSR_HR_CLK_CORR:						decodeSSR		(message);	break;

		case +RtcmMessageType::MSM4_GPS:				//fallthrough
		case +RtcmMessageType::MSM4_GLONASS:			//fallthrough
		case +RtcmMessageType::MSM4_GALILEO:			//fallthrough
		case +RtcmMessageType::MSM4_QZSS:				//fallthrough
		case +RtcmMessageType::MSM4_BEIDOU:				//fallthrough
		case +RtcmMessageType::MSM5_GPS:				//fallthrough
		case +RtcmMessageType::MSM5_GLONASS:			//fallthrough
		case +RtcmMessageType::MSM5_GALILEO:			//fallthrough
		case +RtcmMessageType::MSM5_QZSS:				//fallthrough
		case +RtcmMessageType::MSM5_BEIDOU:				//fallthrough
		case +RtcmMessageType::MSM6_GPS:				//fallthrough
		case +RtcmMessageType::MSM6_GLONASS:			//fallthrough
		case +RtcmMessageType::MSM6_GALILEO:			//fallthrough
		case +RtcmMessageType::MSM6_QZSS:				//fallthrough
		case +RtcmMessageType::MSM6_BEIDOU:				//fallthrough
		case +RtcmMessageType::MSM7_GPS:				//fallthrough
		case +RtcmMessageType::MSM7_GLONASS:			//fallthrough
		case +RtcmMessageType::MSM7_GALILEO:			//fallthrough
		case +RtcmMessageType::MSM7_QZSS:				//fallthrough
		case +RtcmMessageType::MSM7_BEIDOU:				//fallthrough
		{
			ObsList obsList = decodeMSM(message);

			int i = 54;
			int multimessage = getbituInc(message, i,	1);

// 			tracepdeex(0, std::cout, "\n%2d %s %2d %2d ", messageId, obsList.front()->time.to_string().c_str(), obsList.size(), multimessage);

			if	(  superObsList	.empty() == false
				&& obsList		.empty() == false
				&& fabs((superObsList.front()->time - obsList.front()->time).to_double()) > DTTOL)	//todo aaron ew, fix
			{
				//time delta, push the old list and start a new one
				obsListList.push_back(std::move(superObsList));
				superObsList.clear();

				retVal = E_ReturnType::GOT_OBS;
			}

			//copy the new data into the new list
			superObsList.insert(superObsList.end(), obsList.begin(), obsList.end());

			if (multimessage == 0)
			{
				obsListList.push_back(std::move(superObsList));
				superObsList.clear();

				retVal = E_ReturnType::GOT_OBS;
			}

			if (superObsList.size() > 1000)
			{
				superObsList.clear();
			}

			break;
		}

		case +RtcmMessageType::IGS_SSR:						if (decodeigsSSR	(message, rtcmTime()) == E_ReturnType::WAIT) 	retVal = E_ReturnType::WAIT;	break;
		case +RtcmMessageType::COMPACT_SSR:					if (decodecompactSSR(message, rtcmTime()) == E_ReturnType::WAIT) 	retVal = E_ReturnType::WAIT;	break;
	}


	if		(  retVal == E_ReturnType::OK
			|| retVal == E_ReturnType::GOT_OBS)
	{
		frameDecoded();
	}
	else if (  retVal == E_ReturnType::UNSUPPORTED)
	{
		if (acsConfig.output_decoded_rtcm_json)
			traceUnknown();
	}

	return retVal;
}


/** extract unsigned bits from byte data
*/
unsigned int getbitu(
	const unsigned char*	buff,	///< byte data
	int						pos,	///< bit position from start of data (bits)
	int						len)	///< bit length (bits) (len<=32)
{
	unsigned int bits = 0;
	for (int i = pos; i < pos+len; i++)
		bits = (bits<<1) + ((buff[i/8]>>(7-i%8))&1u);

	return bits;
}

/** extract unsigned bits from RTCM messages
*/
unsigned int getbitu(
	vector<unsigned char>&	buff,	///< RTCM messages
	int						pos,	///< bit position from start of data (bits)
	int						len)	///< bit length (bits) (len<=32)
{
	return getbitu(buff.data(), pos, len);
}

/** extract signed bits from byte data
*/
int getbits(
	const unsigned char*	buff,			///< byte data
	int						pos,			///< bit position from start of data (bits)
	int						len,			///< bit length (bits) (len<=32)
	bool*					failure_ptr)	///< pointer for failure flag
{
	unsigned int bits = getbitu(buff, pos, len);

	long int invalid = (1ul<<(len-1));

	if (bits == invalid)
	{
// 		std::cout << "warning: invalid number received on " << __FUNCTION__ << " " << invalid << " " << len << "\n";
		if (failure_ptr)
		{
			*failure_ptr = true;
		}
	}

	if	( len <= 0
		||len >= 32
		||!(bits&(1u<<(len-1))))
	{
		return (int)bits;
	}
	return (int)(bits|(~0u<<len)); /* extend sign */
}

/** increasingly extract unsigned bits from byte data
*/
unsigned int getbituInc(
	const unsigned char*	buff,	///< byte data
	int&					pos,	///< bit position from start of data (bits)
	int						len)	///< bit length (bits) (len<=32)
{
	unsigned int ans = getbitu(buff, pos, len);
	pos += len;
	return ans;
}

/** increasingly extract unsigned bits from RTCM messages
*/
unsigned int getbituInc(
	vector<unsigned char>&	buff,	///< byte data
	int&					pos,	///< bit position from start of data (bits)
	int						len)	///< bit length (bits) (len<=32)
{
	return getbituInc(buff.data(), pos, len);
}

/** increasingly extract signed bits from byte data
*/
int getbitsInc(
	const unsigned char*	buff,			///< byte data
	int&					pos,			///< bit position from start of data (bits)
	int						len,			///< bit length (bits) (len<=32)
	bool*					failure_ptr)	///< pointer for failure flag
{
	int ans = getbits(buff, pos, len, failure_ptr);
	pos += len;
	return ans;
}

/** increasingly extract signed bits from RTCM messages
*/
int getbitsInc(
	vector<unsigned char>&	buff,			///< byte data
	int&					pos,			///< bit position from start of data (bits)
	int						len,			///< bit length (bits) (len<=32)
	bool*					failure_ptr)	///< pointer for failure flag
{
	return getbitsInc(buff.data(), pos, len, failure_ptr);
}

/** increasingly extract signed bits from RTCM messages with scale factor/resolution applied
*/
double getbitsIncScale(
	vector<unsigned char>&	buff,			///< byte data
	int&					pos,			///< bit position from start of data (bits)
	int						len,			///< bit length (bits) (len<=32)
	double					scale,			///< scale factor/resolution
	bool*					failure_ptr)	///< pointer for failure flag
{
	return scale * getbitsInc(buff.data(), pos, len, failure_ptr);
}

/** increasingly extract unsigned bits from RTCM messages with scale factor/resolution applied
*/
double getbituIncScale(
	vector<unsigned char>&	buff,	///< byte data
	int&					pos,	///< bit position from start of data (bits)
	int						len,	///< bit length (bits) (len<=32)
	double					scale)	///< scale factor/resolution
{
	return scale * getbituInc(buff.data(), pos, len);
}

/** extract sign-magnitude bits applied in GLO nav messages from byte data
*/
int getbitg(
	const unsigned char*	buff,	///< byte data
	int						pos,	///< bit position from start of data (bits)
	int						len)	///< bit length (bits) (len<=32)
{
    int value = getbitu(buff, pos+1, len-1);
    return getbitu(buff, pos, 1) ? -value : value;
}

/** increasingly extract sign-magnitude bits applied in GLO nav messages from byte data
*/
int getbitgInc(
	const unsigned char*	buff,	///< byte data
	int&					pos,	///< bit position from start of data (bits)
	int						len)	///< bit length (bits) (len<=32)
{
	int ans = getbitg(buff, pos, len);
	pos += len;
	return ans;
}

/** increasingly extract sign-magnitude bits applied in GLO nav messages from RTCM messages
*/
int getbitgInc(
	vector<unsigned char>&	buff,	///< byte data
	int&					pos,	///< bit position from start of data (bits)
	int						len)	///< bit length (bits) (len<=32)
{
	return getbitgInc(buff.data(), pos, len);
}
