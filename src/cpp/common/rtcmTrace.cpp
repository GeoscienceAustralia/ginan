
// #pragma GCC optimize ("O0")

#include <bsoncxx/json.hpp>

#include "rtcmTrace.hpp"
#include "ephemeris.hpp"
#include "common.hpp"
#include "ssr.hpp"

using bsoncxx::builder::basic::kvp;

map<RtcmMessageType, E_Sys> rtcmMessageSystemMap =
{
	{RtcmMessageType::GPS_EPHEMERIS			, E_Sys::GPS},
	{RtcmMessageType::GLO_EPHEMERIS			, E_Sys::GLO},
	{RtcmMessageType::BDS_EPHEMERIS 		, E_Sys::BDS},
 	{RtcmMessageType::QZS_EPHEMERIS			, E_Sys::QZS},
	{RtcmMessageType::GAL_FNAV_EPHEMERIS	, E_Sys::GAL},
	{RtcmMessageType::GAL_INAV_EPHEMERIS	, E_Sys::GAL},
	{RtcmMessageType::GPS_SSR_ORB_CORR		, E_Sys::GPS},
	{RtcmMessageType::GPS_SSR_CLK_CORR		, E_Sys::GPS},
	{RtcmMessageType::GPS_SSR_CODE_BIAS		, E_Sys::GPS},
	{RtcmMessageType::GPS_SSR_COMB_CORR		, E_Sys::GPS},
	{RtcmMessageType::GPS_SSR_URA			, E_Sys::GPS},
	{RtcmMessageType::GPS_SSR_HR_CLK_CORR	, E_Sys::GPS},
	{RtcmMessageType::GLO_SSR_ORB_CORR		, E_Sys::GLO},
	{RtcmMessageType::GLO_SSR_CLK_CORR		, E_Sys::GLO},
	{RtcmMessageType::GLO_SSR_CODE_BIAS		, E_Sys::GLO},
	{RtcmMessageType::GLO_SSR_COMB_CORR		, E_Sys::GLO},
	{RtcmMessageType::GLO_SSR_URA			, E_Sys::GLO},
	{RtcmMessageType::GLO_SSR_HR_CLK_CORR	, E_Sys::GLO},
	{RtcmMessageType::MSM4_GPS 				, E_Sys::GPS},
	{RtcmMessageType::MSM5_GPS 				, E_Sys::GPS},
	{RtcmMessageType::MSM6_GPS 				, E_Sys::GPS},
	{RtcmMessageType::MSM7_GPS 				, E_Sys::GPS},
	{RtcmMessageType::MSM4_GLONASS 			, E_Sys::GLO},
	{RtcmMessageType::MSM5_GLONASS 			, E_Sys::GLO},
	{RtcmMessageType::MSM6_GLONASS 			, E_Sys::GLO},
	{RtcmMessageType::MSM7_GLONASS 			, E_Sys::GLO},
	{RtcmMessageType::MSM4_GALILEO 			, E_Sys::GAL},
	{RtcmMessageType::MSM5_GALILEO 			, E_Sys::GAL},
	{RtcmMessageType::MSM6_GALILEO 			, E_Sys::GAL},
	{RtcmMessageType::MSM7_GALILEO 			, E_Sys::GAL},
	{RtcmMessageType::MSM4_QZSS 			, E_Sys::QZS},
	{RtcmMessageType::MSM5_QZSS 			, E_Sys::QZS},
	{RtcmMessageType::MSM6_QZSS 			, E_Sys::QZS},
	{RtcmMessageType::MSM7_QZSS 			, E_Sys::QZS},
	{RtcmMessageType::MSM4_BEIDOU 			, E_Sys::BDS},
	{RtcmMessageType::MSM5_BEIDOU 			, E_Sys::BDS},
	{RtcmMessageType::MSM6_BEIDOU 			, E_Sys::BDS},
	{RtcmMessageType::MSM7_BEIDOU 			, E_Sys::BDS},
	{RtcmMessageType::GAL_SSR_ORB_CORR		, E_Sys::GAL},
	{RtcmMessageType::GAL_SSR_CLK_CORR		, E_Sys::GAL},
	{RtcmMessageType::GAL_SSR_CODE_BIAS		, E_Sys::GAL},
	{RtcmMessageType::GAL_SSR_COMB_CORR		, E_Sys::GAL},
	{RtcmMessageType::GAL_SSR_URA			, E_Sys::GAL},
	{RtcmMessageType::GAL_SSR_HR_CLK_CORR	, E_Sys::GAL},
	{RtcmMessageType::QZS_SSR_ORB_CORR		, E_Sys::QZS},
	{RtcmMessageType::QZS_SSR_CLK_CORR		, E_Sys::QZS},
	{RtcmMessageType::QZS_SSR_CODE_BIAS		, E_Sys::QZS},
	{RtcmMessageType::QZS_SSR_COMB_CORR		, E_Sys::QZS},
	{RtcmMessageType::QZS_SSR_URA			, E_Sys::QZS},
	{RtcmMessageType::QZS_SSR_HR_CLK_CORR	, E_Sys::QZS},
	{RtcmMessageType::SBS_SSR_ORB_CORR		, E_Sys::SBS},
	{RtcmMessageType::SBS_SSR_CLK_CORR		, E_Sys::SBS},
	{RtcmMessageType::SBS_SSR_CODE_BIAS		, E_Sys::SBS},
	{RtcmMessageType::SBS_SSR_COMB_CORR		, E_Sys::SBS},
	{RtcmMessageType::SBS_SSR_URA			, E_Sys::SBS},
	{RtcmMessageType::SBS_SSR_HR_CLK_CORR	, E_Sys::SBS},
	{RtcmMessageType::BDS_SSR_ORB_CORR		, E_Sys::BDS},
	{RtcmMessageType::BDS_SSR_CLK_CORR		, E_Sys::BDS},
	{RtcmMessageType::BDS_SSR_CODE_BIAS		, E_Sys::BDS},
	{RtcmMessageType::BDS_SSR_COMB_CORR		, E_Sys::BDS},
	{RtcmMessageType::BDS_SSR_URA			, E_Sys::BDS},
	{RtcmMessageType::BDS_SSR_HR_CLK_CORR	, E_Sys::BDS},
	{RtcmMessageType::GPS_SSR_PHASE_BIAS	, E_Sys::GPS},
	{RtcmMessageType::GLO_SSR_PHASE_BIAS	, E_Sys::GLO},
	{RtcmMessageType::GAL_SSR_PHASE_BIAS	, E_Sys::GAL},
	{RtcmMessageType::QZS_SSR_PHASE_BIAS	, E_Sys::QZS},
	{RtcmMessageType::SBS_SSR_PHASE_BIAS	, E_Sys::SBS},
	{RtcmMessageType::BDS_SSR_PHASE_BIAS	, E_Sys::BDS},
	{RtcmMessageType::COMPACT_SSR			, E_Sys::SUPPORTED},
	{RtcmMessageType::IGS_SSR				, E_Sys::SUPPORTED}
};


void RtcmTrace::traceSsrEph(
	RtcmMessageType	messCode,
	SatSys			Sat,
	SSREph&			ssrEph)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	GTime	nearTime		= timeGet();

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("type",						"ssrEph"));
	doc.append(kvp("Mountpoint",				rtcmMountpoint									));
	doc.append(kvp("MessageNumber",				messCode._to_integral()							));
	doc.append(kvp("MessageType",				messCode._to_string()							));
	doc.append(kvp("ReceivedSentTimeGPST",		nearTime.to_string()							));
	doc.append(kvp("EpochTimeGPST",				ssrEph.ssrMeta.receivedTime.to_string()			));
	doc.append(kvp("ReferenceTimeGPST",			ssrEph.t0.to_string()							));
	doc.append(kvp("EpochTime1s",				ssrEph.ssrMeta.epochTime1s						));
	doc.append(kvp("SSRUpdateIntervalSec",		ssrEph.udi										));
	doc.append(kvp("SSRUpdateIntervalIndex",	ssrEph.ssrMeta.updateIntIndex					));
	doc.append(kvp("MultipleMessageIndicator",	ssrEph.ssrMeta.multipleMessage					));
	doc.append(kvp("SatReferenceDatum",			(int)ssrEph.ssrMeta.referenceDatum				));	// 0 = ITRF, 1 = Regional // bsoncxx doesn't like uints
	doc.append(kvp("IODSSR",					ssrEph.iod										));
	doc.append(kvp("SSRProviderID",				(int)ssrEph.ssrMeta.provider					));
	doc.append(kvp("SSRSolutionID",				(int)ssrEph.ssrMeta.solution					));
	doc.append(kvp("Sat",						Sat.id()										));
	doc.append(kvp("IODE",						ssrEph.iode										));
	doc.append(kvp("IODCRC",					ssrEph.iodcrc									));
	doc.append(kvp("DeltaRadial",				ssrEph.deph[0]									));
	doc.append(kvp("DeltaAlongTrack",			ssrEph.deph[1]									));
	doc.append(kvp("DeltaCrossTrack",			ssrEph.deph[2]									));
	doc.append(kvp("DotDeltaRadial",			ssrEph.ddeph[0]									));
	doc.append(kvp("DotDeltaAlongTrack",		ssrEph.ddeph[1]									));
	doc.append(kvp("DotDeltaCrossTrack",		ssrEph.ddeph[2]									));

	fout << bsoncxx::to_json(doc) << "\n";
}

void RtcmTrace::traceSsrClk(
	RtcmMessageType	messCode,
	SatSys			Sat,
	SSRClk&			ssrClk)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	GTime	nearTime		= timeGet();

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("type",						"ssrClk"));
	doc.append(kvp("Mountpoint",				rtcmMountpoint									));
	doc.append(kvp("MessageNumber",				messCode._to_integral()							));
	doc.append(kvp("MessageType",				messCode._to_string()							));
	doc.append(kvp("ReceivedSentTimeGPST",		nearTime.to_string()							));
	doc.append(kvp("EpochTimeGPST",				ssrClk.ssrMeta.receivedTime.to_string()			));
	doc.append(kvp("ReferenceTimeGPST",			ssrClk.t0.to_string()							));
	doc.append(kvp("EpochTime1s",				ssrClk.ssrMeta.epochTime1s						));
	doc.append(kvp("SSRUpdateIntervalSec",		ssrClk.udi										));
	doc.append(kvp("SSRUpdateIntervalIndex",	ssrClk.ssrMeta.updateIntIndex					));
	doc.append(kvp("MultipleMessageIndicator",	ssrClk.ssrMeta.multipleMessage					));
	doc.append(kvp("SatReferenceDatum",			(int)ssrClk.ssrMeta.referenceDatum				));	// 0 = ITRF, 1 = Regional	// could be combined corrections
	doc.append(kvp("IODSSR",					ssrClk.iod										));
	doc.append(kvp("SSRProviderID",				(int)ssrClk.ssrMeta.provider					));
	doc.append(kvp("SSRSolutionID",				(int)ssrClk.ssrMeta.solution					));
	doc.append(kvp("Sat",						Sat.id()										));
	doc.append(kvp("DeltaClockC0",				ssrClk.dclk[0]									));
	doc.append(kvp("DeltaClockC1",				ssrClk.dclk[1]									));
	doc.append(kvp("DeltaClockC2",				ssrClk.dclk[2]									));

	fout << bsoncxx::to_json(doc) << "\n";
}

void RtcmTrace::traceSsrUra(
	RtcmMessageType	messCode,
	SatSys			Sat,
	SSRUra&			ssrUra)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	GTime	nearTime		= timeGet();

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("type",						"ssrURA"));
	doc.append(kvp("Mountpoint",				rtcmMountpoint									));
	doc.append(kvp("MessageNumber",				messCode._to_integral()							));
	doc.append(kvp("MessageType",				messCode._to_string()							));
	doc.append(kvp("ReceivedSentTimeGPST",		nearTime.to_string()							));
	doc.append(kvp("EpochTimeGPST",				ssrUra.ssrMeta.receivedTime.to_string()			));
	doc.append(kvp("ReferenceTimeGPST",			ssrUra.t0.to_string()							));
	doc.append(kvp("EpochTime1s",				ssrUra.ssrMeta.epochTime1s						));
	doc.append(kvp("SSRUpdateIntervalSec",		ssrUra.udi										));
	doc.append(kvp("SSRUpdateIntervalIndex",	ssrUra.ssrMeta.updateIntIndex					));
	doc.append(kvp("MultipleMessageIndicator",	ssrUra.ssrMeta.multipleMessage					));
	doc.append(kvp("IODSSR",					ssrUra.iod										));
	doc.append(kvp("SSRProviderID",				(int)ssrUra.ssrMeta.provider					));
	doc.append(kvp("SSRSolutionID",				(int)ssrUra.ssrMeta.solution					));
	doc.append(kvp("Sat",						Sat.id()										));
	doc.append(kvp("SSRURA",					ssrUra.ura										));

	fout << bsoncxx::to_json(doc) << "\n";
}

void RtcmTrace::traceSsrHRClk(
	RtcmMessageType	messCode,
	SatSys			Sat,
	SSRHRClk&		SsrHRClk)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	GTime	nearTime		= timeGet();

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("type",						"ssrHRClk"));
	doc.append(kvp("Mountpoint",				rtcmMountpoint									));
	doc.append(kvp("MessageNumber",				messCode._to_integral()							));
	doc.append(kvp("MessageType",				messCode._to_string()							));
	doc.append(kvp("ReceivedSentTimeGPST",		nearTime.to_string()							));
	doc.append(kvp("EpochTimeGPST",				SsrHRClk.ssrMeta.receivedTime.to_string()		));
	doc.append(kvp("ReferenceTimeGPST",			SsrHRClk.t0.to_string()							));
	doc.append(kvp("EpochTime1s",				SsrHRClk.ssrMeta.epochTime1s					));
	doc.append(kvp("SSRUpdateIntervalSec",		SsrHRClk.udi									));
	doc.append(kvp("SSRUpdateIntervalIndex",	SsrHRClk.ssrMeta.updateIntIndex					));
	doc.append(kvp("MultipleMessageIndicator",	SsrHRClk.ssrMeta.multipleMessage				));
	doc.append(kvp("IODSSR",					SsrHRClk.iod									));
	doc.append(kvp("SSRProviderID",				(int)SsrHRClk.ssrMeta.provider					));
	doc.append(kvp("SSRSolutionID",				(int)SsrHRClk.ssrMeta.solution					));
	doc.append(kvp("Sat",						Sat.id()										));
	doc.append(kvp("HighRateClockCorr",			SsrHRClk.hrclk									));

	fout << bsoncxx::to_json(doc) << "\n";
}

void RtcmTrace::traceSsrCodeBias(
	RtcmMessageType	messCode,
	SatSys			Sat,
	E_ObsCode		code,
	SSRCodeBias&	ssrBias)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	GTime	nearTime		= timeGet();

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("type",						"ssrCodeBias"));
	doc.append(kvp("Mountpoint",				rtcmMountpoint									));
	doc.append(kvp("MessageNumber",				messCode._to_integral()							));
	doc.append(kvp("MessageType",				messCode._to_string()							));
	doc.append(kvp("ReceivedSentTimeGPST",		nearTime.to_string()							));
	doc.append(kvp("EpochTimeGPST",				ssrBias.ssrMeta.receivedTime.to_string()		));
	doc.append(kvp("ReferenceTimeGPST",			ssrBias.t0.to_string()							));
	doc.append(kvp("EpochTime1s",				ssrBias.ssrMeta.epochTime1s						));
	doc.append(kvp("SSRUpdateIntervalSec",		ssrBias.udi										));
	doc.append(kvp("SSRUpdateIntervalIndex",	ssrBias.ssrMeta.updateIntIndex					));
	doc.append(kvp("MultipleMessageIndicator",	ssrBias.ssrMeta.multipleMessage					));
	doc.append(kvp("IODSSR",					ssrBias.iod										));
	doc.append(kvp("SSRProviderID",				(int)ssrBias.ssrMeta.provider					));
	doc.append(kvp("SSRSolutionID",				(int)ssrBias.ssrMeta.solution					));
	doc.append(kvp("Sat",						Sat.id()										));
	doc.append(kvp("Code",						code._to_string()								));
	doc.append(kvp("Bias",						ssrBias.obsCodeBiasMap[code].bias				));

	fout << bsoncxx::to_json(doc) << "\n";
}

void RtcmTrace::traceSsrPhasBias(
	RtcmMessageType	messCode,
	SatSys			Sat,
	E_ObsCode		code,
	SSRPhasBias&	ssrBias)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	GTime	nearTime		= timeGet();

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("type",						"ssrPhasBias"));
	doc.append(kvp("Mountpoint",				rtcmMountpoint									));
	doc.append(kvp("MessageNumber",				messCode._to_integral()							));
	doc.append(kvp("MessageType",				messCode._to_string()							));
	doc.append(kvp("ReceivedSentTimeGPST",		nearTime.to_string()							));
	doc.append(kvp("EpochTimeGPST",				ssrBias.ssrMeta.receivedTime.to_string()		));
	doc.append(kvp("ReferenceTimeGPST",			ssrBias.t0.to_string()							));
	doc.append(kvp("EpochTime1s",				ssrBias.ssrMeta.epochTime1s						));
	doc.append(kvp("SSRUpdateIntervalSec",		ssrBias.udi										));
	doc.append(kvp("SSRUpdateIntervalIndex",	ssrBias.ssrMeta.updateIntIndex					));
	doc.append(kvp("MultipleMessageIndicator",	ssrBias.ssrMeta.multipleMessage					));
	doc.append(kvp("IODSSR",					ssrBias.iod										));
	doc.append(kvp("SSRProviderID",				(int)ssrBias.ssrMeta.provider					));
	doc.append(kvp("SSRSolutionID",				(int)ssrBias.ssrMeta.solution					));
	doc.append(kvp("DisperBiasConsisIndicator",	ssrBias.ssrPhase.dispBiasConistInd				));
	doc.append(kvp("MWConsistencyIndicator",	ssrBias.ssrPhase.MWConistInd					));
	doc.append(kvp("Sat",						Sat.id()										));
	doc.append(kvp("YawAngle",					ssrBias.ssrPhase.yawAngle						));
	doc.append(kvp("YawRate",					ssrBias.ssrPhase.yawRate						));
	doc.append(kvp("Code",						code._to_string()								));
	doc.append(kvp("SignalIntegerIndicator",	(int)ssrBias.ssrPhaseChs[code].signalIntInd		));
	doc.append(kvp("SignalsWLIntegerIndicator",	(int)ssrBias.ssrPhaseChs[code].signalWLIntInd	));
	doc.append(kvp("SignalDiscontinuityCount",	(int)ssrBias.ssrPhaseChs[code].signalDisconCnt	));
	doc.append(kvp("Bias",						ssrBias.obsCodeBiasMap[code].bias				));

	fout << bsoncxx::to_json(doc) << "\n";
}

void RtcmTrace::traceTimestamp(
	GTime time)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("type",			"timestamp"					));
	doc.append(kvp("Mountpoint",	rtcmMountpoint				));
	doc.append(kvp("time",			(string)time				));
	doc.append(kvp("ticks",			(double)time.bigTime		));

	fout << bsoncxx::to_json(doc) << "\n";
}

/** Write decoded/encoded GPS/GAL/BDS/QZS ephemeris messages to a json file
*/
void RtcmTrace::traceBrdcEph(	//todo aaron, template this for gps/glo?
	RtcmMessageType	messCode,
	Eph&			eph)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	bsoncxx::builder::basic::document doc = {};

	GTime	nearTime		= timeGet();

	// Note the Satellite id is not set in rinex correctly as we a mixing GNSS systems.
	doc.append(kvp("type",					"brdcEph"				));
	doc.append(kvp("Mountpoint",			rtcmMountpoint			));
	doc.append(kvp("MessageNumber",			messCode._to_integral()	));
	doc.append(kvp("MessageType",			messCode._to_string()	));
	doc.append(kvp("ReceivedSentTimeGPST",	nearTime.to_string()	));
	doc.append(kvp("Type",					eph.type._to_string()	));

	doc.append(kvp("ToeGPST",				eph.toe.to_string()	));
	doc.append(kvp("TocGPST",				eph.toc.to_string()	));

	traceBrdcEphBody(doc, eph);

	fout << bsoncxx::to_json(doc) << "\n";
}

/** Write decoded/encoded GAL ephemeris messages to a json file
*/
void RtcmTrace::traceBrdcEph(
	RtcmMessageType	messCode,
	Geph&			geph)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	bsoncxx::builder::basic::document doc = {};

	GTime	nearTime		= timeGet();

	// Note the Satellite id is not set in rinex correctly as we a mixing GNSS systems.
	doc.append(kvp("type",					"brdcEph"				));
	doc.append(kvp("Mountpoint",			rtcmMountpoint			));
	doc.append(kvp("MessageNumber",			messCode._to_integral()	));
	doc.append(kvp("MessageType",			messCode._to_string()	));
	doc.append(kvp("ReceivedSentTimeGPST",	nearTime.to_string()	));
	doc.append(kvp("Sat",					geph.Sat.id()			));
	doc.append(kvp("Type",					geph.type._to_string()	));

	doc.append(kvp("ToeGPST",				geph.toe.to_string()	));
	doc.append(kvp("TofGPST",				geph.tof.to_string()	));

	traceBrdcEphBody(doc, geph);

	fout << bsoncxx::to_json(doc) << "\n";
}

void	traceBrdcEphBody(
	bsoncxx::builder::basic::document&	doc,
	Eph&								eph)
{
	doc.append(kvp("Sat",			eph.Sat.id()			));
	doc.append(kvp("weekRollOver",	eph.weekRollOver		));
	doc.append(kvp("week",			eph.week				));
	doc.append(kvp("toes",			eph.toes				));
	doc.append(kvp("tocs",			eph.tocs				));
	doc.append(kvp("howTow",		eph.howTow				));
	doc.append(kvp("toe",			eph.toe.to_string()		));
	doc.append(kvp("toc",			eph.toc.to_string()		));
	doc.append(kvp("ttm",			eph.ttm.to_string()		));

	doc.append(kvp("aode",			eph.aode	));
	doc.append(kvp("aodc",			eph.aodc	));
	doc.append(kvp("iode",			eph.iode	));
	doc.append(kvp("iodc",			eph.iodc	));

	doc.append(kvp("f0",			eph.f0		));
	doc.append(kvp("f1",			eph.f1		));
	doc.append(kvp("f2",			eph.f2		));

	doc.append(kvp("sqrtA",			eph.sqrtA	));
	doc.append(kvp("A",				eph.A		));
	doc.append(kvp("e",				eph.e		));
	doc.append(kvp("i0",			eph.i0		));
	doc.append(kvp("idot",			eph.idot	));
	doc.append(kvp("omg",			eph.omg		));
	doc.append(kvp("OMG0",			eph.OMG0	));
	doc.append(kvp("OMGd",			eph.OMGd	));
	doc.append(kvp("M0",			eph.M0		));
	doc.append(kvp("deln",			eph.deln	));
	doc.append(kvp("crc",			eph.crc		));
	doc.append(kvp("crs",			eph.crs		));
	doc.append(kvp("cic",			eph.cic		));
	doc.append(kvp("cis",			eph.cis		));
	doc.append(kvp("cuc",			eph.cuc		));
	doc.append(kvp("cus",			eph.cus		));

	doc.append(kvp("tgd0",			eph.tgd[0]	));
	doc.append(kvp("tgd1",			eph.tgd[1]	));	// GPS/QZS no tgd[1]
	doc.append(kvp("sva",			eph.sva		));

	if	( eph.Sat.sys == +E_Sys::GPS
		||eph.Sat.sys == +E_Sys::QZS)
	{
		doc.append(kvp("ura",				eph.ura[0]	));
		doc.append(kvp("svh",				eph.svh		));
		doc.append(kvp("code",				eph.code	));
		doc.append(kvp("flag",				eph.flag	));	// QZS no flag
		doc.append(kvp("fitFlag",			eph.fitFlag	));
		doc.append(kvp("fit",				eph.fit		));
	}
	else if (eph.Sat.sys == +E_Sys::GAL)
	{
		doc.append(kvp("SISA",				eph.ura[0]	));
		doc.append(kvp("SVHealth",			eph.svh		));
		doc.append(kvp("E5aHealth",			eph.e5a_hs	));
		doc.append(kvp("E5aDataValidity",	eph.e5a_dvs	));
		doc.append(kvp("E5bHealth",			eph.e5b_hs	));
		doc.append(kvp("E5bDataValidity",	eph.e5b_dvs	));
		doc.append(kvp("E1Health",			eph.e1_hs	));
		doc.append(kvp("E1DataValidity",	eph.e1_dvs	));
		doc.append(kvp("DataSource",		eph.code	));
	}
	else if (eph.Sat.sys == +E_Sys::BDS)
	{
		doc.append(kvp("URA",				eph.ura[0]	));
		doc.append(kvp("SVHealth",			eph.svh		));
	}
}

void	traceBrdcEphBody(
	bsoncxx::builder::basic::document&	doc,
	Geph&								geph)
{
	doc.append(kvp("ToeSecOfDay",			geph.tb					));
	doc.append(kvp("TofHour",				geph.tk_hour			));
	doc.append(kvp("TofMin",				geph.tk_min				));
	doc.append(kvp("TofSec",				geph.tk_sec				));

	doc.append(kvp("IODE",					geph.iode		));

	doc.append(kvp("TauN",					geph.taun		));
	doc.append(kvp("GammaN",				geph.gammaN		));
	doc.append(kvp("DeltaTauN",				geph.dtaun		));

	doc.append(kvp("PosX",					geph.pos[0]		));
	doc.append(kvp("PosY",					geph.pos[1]		));
	doc.append(kvp("PosZ",					geph.pos[2]		));
	doc.append(kvp("VelX",					geph.vel[0]		));
	doc.append(kvp("VelY",					geph.vel[1]		));
	doc.append(kvp("VelZ",					geph.vel[2]		));
	doc.append(kvp("AccX",					geph.acc[0]		));
	doc.append(kvp("AccY",					geph.acc[1]		));
	doc.append(kvp("AccZ",					geph.acc[2]		));

	doc.append(kvp("FrquencyNumber",		geph.frq		));
	doc.append(kvp("SVHealth",				geph.svh		));
	doc.append(kvp("Age",					geph.age		));

	doc.append(kvp("GLONASSM",				geph.glonassM	));
	doc.append(kvp("NumberOfDayIn4Year",	geph.NT			));
	doc.append(kvp("AdditionalData",		geph.moreData	));
	doc.append(kvp("4YearIntervalNumber",	geph.N4			));
}

/** Writes nav.satNavMap[].ssrOut to a human-readable file
*/
void writeSsrOutToFile(
	int						epochNum,
	map<SatSys, SSROut>&	ssrOutMap)
{
	string filename = "ssrOut.dbg";
	std::ofstream out(filename, std::ios::app);

	if (!out)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error: Could not open trace file for SSR messages at " << filename;
		return;
	}
	out.precision(17);

	// Header
	out << "epochNum" 			<< "\t";
	out << "satId" 				<< "\t";

// 	out << "SSREph.canExport"	<< "\t";
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

// 	out << "SSRClk.canExport"	<< "\t";
	out << "SSRClk.t0"			<< "\t";
	out << "SSRClk.udi"			<< "\t";
	out << "SSRClk.iod"			<< "\t";
	out << "SSRClk.dclk[0]"		<< "\t";
	out << "SSRClk.dclk[1]"		<< "\t";
	out << "SSRClk.dclk[2]"		<< "\t";

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
		out << "ssrBias.ssrPhaseCh.signalWLIntInd_"		<< i <<	"\t";
		out << "ssrBias.ssrPhaseCh.signalDisconCnt_"	<< i << "\t";
	}
	out << "\n";

	// Body
	for (auto& [Sat, ssrOut] : ssrOutMap)
	{
		out << epochNum << "\t";
		out << Sat.id() << "\t";
// 		out << ssrOut.ssrEph.canExport<< "\t";
		out << ssrOut.ssrEph.t0			<< "\t";
		out << ssrOut.ssrEph.udi		<< "\t";
		out << ssrOut.ssrEph.iod		<< "\t";
		out << ssrOut.ssrEph.iode		<< "\t";
		out << ssrOut.ssrEph.deph[0]	<< "\t";
		out << ssrOut.ssrEph.deph[1]	<< "\t";
		out << ssrOut.ssrEph.deph[2]	<< "\t";
		out << ssrOut.ssrEph.ddeph[0]	<< "\t";
		out << ssrOut.ssrEph.ddeph[1]	<< "\t";
		out << ssrOut.ssrEph.ddeph[2]	<< "\t";

// 		out << ssrOut.ssrClk.canExport<< "\t";
		out << ssrOut.ssrClk.t0			<< "\t";
		out << ssrOut.ssrClk.udi		<< "\t";
		out << ssrOut.ssrClk.iod		<< "\t";
		out << ssrOut.ssrClk.dclk[0]	<< "\t";
		out << ssrOut.ssrClk.dclk[1]	<< "\t";
		out << ssrOut.ssrClk.dclk[2]	<< "\t";

		out << ssrOut.ssrCodeBias.t0			<< "\t";
		out << ssrOut.ssrPhasBias.t0			<< "\t";
		out << ssrOut.ssrCodeBias.udi			<< "\t";
		out << ssrOut.ssrPhasBias.udi			<< "\t";
		out << ssrOut.ssrCodeBias.iod			<< "\t";
		out << ssrOut.ssrPhasBias.iod			<< "\t";
		for (auto& [key, val] : ssrOut.ssrCodeBias.obsCodeBiasMap)	out << val.bias << "\t" << val.var << "\t";
		for (auto& [key, val] : ssrOut.ssrPhasBias.obsCodeBiasMap)	out << val.bias << "\t" << val.var << "\t";

		for (auto& [key, ssrPhaseCh] : ssrOut.ssrPhasBias.ssrPhaseChs)
		{
			out << ssrPhaseCh.signalIntInd		<< "\t";
			out << ssrPhaseCh.signalWLIntInd	<< "\t";
			out << ssrPhaseCh.signalDisconCnt	<< "\t";
		}
		out << "\n";
	}
	out << "\n";
}


/** Write msm message to a json file
*/
void RtcmTrace::traceMSM(
	RtcmMessageType	messCode,
	GTime			time,
	SatSys			Sat,
	Sig&			sig)
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	GTime	nearTime		= timeGet();

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("type",						"MSM"											));
	doc.append(kvp("Mountpoint",				rtcmMountpoint									));
	doc.append(kvp("MessageNumber",				messCode._to_integral()							));
	doc.append(kvp("MessageType",				messCode._to_string()							));
	doc.append(kvp("ReceivedSentTimeGPST",		nearTime.to_string()							));
	doc.append(kvp("EpochTimeGPST",				time.to_string()								));
	doc.append(kvp("Sat",						Sat.id()										));
	doc.append(kvp("Code",						sig.code._to_string()							));
	doc.append(kvp("Pseudorange",				sig.P											));
	doc.append(kvp("CarrierPhase",				sig.L											));
	doc.append(kvp("Doppler",					sig.D											));
	doc.append(kvp("SNR",						sig.snr											));
	doc.append(kvp("LLI",						sig.LLI											));
	doc.append(kvp("IsInvalid",					sig.invalid										));

	fout << bsoncxx::to_json(doc) << "\n";
}


/** Write unknown message to a json file
*/
void RtcmTrace::traceUnknown()
{
	if (rtcmTraceFilename.empty())
	{
		return;
	}

	std::ofstream fout(rtcmTraceFilename, std::ios::app);
	if (!fout)
	{
		std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
		return;
	}

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("type",					"?"			));

	fout << bsoncxx::to_json(doc) << "\n";
}
