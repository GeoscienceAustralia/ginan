
// #pragma GCC optimize ("O0")

#include <fstream>

#include <boost/log/trivial.hpp>

#include "rinexNavWrite.hpp"
#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "common.hpp"
#include "rinex.hpp"
#include "trace.hpp"



struct RinexNavFileOutput
{
	map<E_Sys,	bool>	sysMap;
	map<SatSys, int>	last_iode;  ///< Used to track last ephemeris.
};

map<string, RinexNavFileOutput> filenameNavFileDataMap;


map<E_UtcId, string> utcIdStr =
{
	{E_UtcId::NONE,		""			},
	{E_UtcId::UTC_USNO,	"UTC(USNO)"	},
	{E_UtcId::UTC_SU,	"UTC(SU)"	},
	{E_UtcId::UTCGAL,	"UTCGAL"	},
	{E_UtcId::UTC_NTSC,	"UTC(NTSC)"	},
	{E_UtcId::UTC_NICT,	"UTC(NICT)"	},
	{E_UtcId::UTC_NPLI,	"UTC(NPLI)"	},
	{E_UtcId::UTCIRN,	"UTCIRN"	},
	{E_UtcId::UTC_OP,	"UTC(OP)"	},
	{E_UtcId::UTC_NIST,	"UTC(NIST)"	},
};

void outputNavRinexEph(
	Eph&			eph,
	Trace&			trace,
	const double	rnxver)
{
	string formatStr = "% 15.12fE%+03d";

	if (rnxver >= 4.0)
	{
		tracepdeex(0, trace, "> EPH %-3s %-4s", eph.Sat.id().c_str(), eph.type);
		trace << "\n";
	}

	auto sys = eph.Sat.sys;

	E_TimeSys tsys = E_TimeSys::GPST;
	switch (sys)
	{
		case E_Sys::GPS:	tsys = E_TimeSys::GPST;		break;
		case E_Sys::GAL:	tsys = E_TimeSys::GST;		break;
		case E_Sys::BDS:	tsys = E_TimeSys::BDT;		break;
		case E_Sys::QZS:	tsys = E_TimeSys::QZSST;	break;
		case E_Sys::SBS:	tsys = E_TimeSys::GPST;		break;
		default:			tsys = E_TimeSys::GPST;		break;
	}

	double ep[6] = {0};
	time2epoch(eph.toc, ep, tsys);

	tracepdeex(0, trace, "%-3s %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", eph.Sat.id().c_str(), ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	traceFormatedFloat(trace, eph.f0,		formatStr);
	traceFormatedFloat(trace, eph.f1,		formatStr);
	traceFormatedFloat(trace, eph.f2,		formatStr);
	trace << "\n";

	trace << "    ";
	if (sys != +E_Sys::BDS)		{	traceFormatedFloat(trace, eph.iode,		formatStr);	}	/* GPS/QZS: IODE, GAL: IODnav */
	else 						{	traceFormatedFloat(trace, eph.aode,		formatStr);	}	/* BDS: AODE */

	traceFormatedFloat(trace, eph.crs,		formatStr);
	traceFormatedFloat(trace, eph.deln,		formatStr);
	traceFormatedFloat(trace, eph.M0,		formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, eph.cuc,		formatStr);
	traceFormatedFloat(trace, eph.e,		formatStr);
	traceFormatedFloat(trace, eph.cus,		formatStr);
	traceFormatedFloat(trace, sqrt(eph.A),	formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, eph.toes,		formatStr);
	traceFormatedFloat(trace, eph.cic,		formatStr);
	traceFormatedFloat(trace, eph.OMG0,		formatStr);
	traceFormatedFloat(trace, eph.cis,		formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, eph.i0,		formatStr);
	traceFormatedFloat(trace, eph.crc,		formatStr);
	traceFormatedFloat(trace, eph.omg,		formatStr);
	traceFormatedFloat(trace, eph.OMGd,		formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, eph.idot,		formatStr);
	traceFormatedFloat(trace, eph.code,		formatStr);
	traceFormatedFloat(trace, eph.week,		formatStr);	/* GPS/QZS: GPS week, GAL: GAL week, BDS: BDT week */
	traceFormatedFloat(trace, eph.flag,		formatStr);
	trace << "\n";

	trace << "    ";
	if (sys == +E_Sys::GAL)		{	traceFormatedFloat(trace, svaToSisa	(eph.sva),	formatStr);	}
	else						{	traceFormatedFloat(trace, svaToUra	(eph.sva),	formatStr);	}

	traceFormatedFloat(trace, eph.svh,		formatStr);
	traceFormatedFloat(trace, eph.tgd[0],	formatStr);	/* GPS/QZS:TGD, GAL:BGD E5a/E1, BDS: TGD1 B1/B3 */

	if	(  sys == +E_Sys::GAL
		|| sys == +E_Sys::BDS)	{	traceFormatedFloat(trace, eph.tgd[1],	formatStr);	}	/* GAL:BGD E5b/E1, BDS: TGD2 B2/B3 */
	else						{	traceFormatedFloat(trace, eph.iodc,		formatStr);	}	/* GPS/QZS:IODC */
	trace << "\n";

	trace << "    ";
	if	(sys != +E_Sys::BDS)	{	traceFormatedFloat(trace, GTow(eph.ttm), formatStr);	}
	else						{	traceFormatedFloat(trace, BTow(eph.ttm), formatStr);	}

	if		(sys == +E_Sys::GPS)	{		traceFormatedFloat(trace, eph.fit,		formatStr);	}	/* fit interval in hours for GPS */
	else if (sys == +E_Sys::QZS)	{		traceFormatedFloat(trace, eph.fitFlag,	formatStr);	}	/* fit interval flag for QZS */
	else if (sys == +E_Sys::BDS)	{		traceFormatedFloat(trace, eph.aodc,		formatStr);	}	/* BDS: AODC */
	else							{		traceFormatedFloat(trace, 0,			formatStr);	}	/* spare */
	trace << "\n";
}


void outputNavRinexGeph(
	Geph&			geph,
	Trace&			trace,
	const double	rnxver)
{
	string formatStr = "% 15.12fE%+03d";

	if (rnxver >= 4.0)
	{
		tracepdeex(0, trace, "> EPH %-3s %-4s", geph.Sat.id().c_str(), geph.type);
		trace << "\n";
	}

	if (geph.Sat.sys != +E_Sys::GLO)
	{
		BOOST_LOG_TRIVIAL(error) << "Error output Geph to RINEX incorrect system type";
		return;
	}

	UtcTime	utcTime;
	GTime fakeGTime;

	utcTime = geph.tof;
	fakeGTime.bigTime = utcTime.bigTime;
	double tof = GTow(fakeGTime);

	double ep[6] = {0};
	time2epoch(geph.toe, ep, E_TimeSys::UTC);

	tracepdeex(0, trace, "%-3s %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", geph.Sat.id().c_str(), ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);
	traceFormatedFloat(trace,-geph.taun,	formatStr);   	// -taun
	traceFormatedFloat(trace, geph.gammaN,	formatStr);
	traceFormatedFloat(trace, tof, formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, geph.pos[0] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.vel[0] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.acc[0] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.svh, formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, geph.pos[1] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.vel[1] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.acc[1] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.frq, formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, geph.pos[2] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.vel[2] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.acc[2] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.age, formatStr);
	trace << "\n";

	if (rnxver >= 3.05)
	{
		// todo Eugene: additional records from version 3.05 and on
	}
}


void outputNavRinexCeph(
	Ceph&			ceph,
	Trace&			trace,
	const double	rnxver)
{
	string formatStr = "% 15.12fE%+03d";

	if (rnxver < 4.0)
	{
		BOOST_LOG_TRIVIAL(error) << "Error output Ceph to RINEX incorrect RINEX version=" << rnxver;
		return;
	}

	tracepdeex(0, trace, "> EPH %-3s %-4s", ceph.Sat.id().c_str(), ceph.type);
	trace << "\n";

	auto sys	= ceph.Sat.sys;
	auto type	= ceph.type;

	E_TimeSys tsys = E_TimeSys::GPST;
	switch (sys)
	{
		case E_Sys::GPS:	tsys = E_TimeSys::GPST;		break;
		case E_Sys::GAL:	tsys = E_TimeSys::GST;		break;
		case E_Sys::BDS:	tsys = E_TimeSys::BDT;		break;
		case E_Sys::QZS:	tsys = E_TimeSys::QZSST;	break;
		case E_Sys::SBS:	tsys = E_TimeSys::GPST;		break;
		default:			tsys = E_TimeSys::GPST;		break;
	}

	double ep[6] = {0};
	time2epoch(ceph.toc, ep, tsys);

	tracepdeex(0, trace, "%-3s %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", ceph.Sat.id().c_str(), ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	traceFormatedFloat(trace, ceph.f0,		formatStr);
	traceFormatedFloat(trace, ceph.f1,		formatStr);
	traceFormatedFloat(trace, ceph.f2,		formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, ceph.Adot,	formatStr);
	traceFormatedFloat(trace, ceph.crs,		formatStr);
	traceFormatedFloat(trace, ceph.deln,	formatStr);
	traceFormatedFloat(trace, ceph.M0,		formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, ceph.cuc,		formatStr);
	traceFormatedFloat(trace, ceph.e,		formatStr);
	traceFormatedFloat(trace, ceph.cus,		formatStr);
	traceFormatedFloat(trace, sqrt(ceph.A),	formatStr);
	trace << "\n";

	trace << "    ";
	if	(sys != +E_Sys::BDS)	{	traceFormatedFloat(trace, ceph.tops,	formatStr);	}
	else						{	traceFormatedFloat(trace, ceph.toes,	formatStr);	}

	traceFormatedFloat(trace, ceph.cic,		formatStr);
	traceFormatedFloat(trace, ceph.OMG0,	formatStr);
	traceFormatedFloat(trace, ceph.cis,		formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, ceph.i0,		formatStr);
	traceFormatedFloat(trace, ceph.crc,		formatStr);
	traceFormatedFloat(trace, ceph.omg,		formatStr);
	traceFormatedFloat(trace, ceph.OMGd,	formatStr);
	trace << "\n";

	trace << "    ";
	traceFormatedFloat(trace, ceph.idot,	formatStr);
	traceFormatedFloat(trace, ceph.dn0d,	formatStr);

	if	(sys != +E_Sys::BDS)
	{
		traceFormatedFloat(trace, ceph.ura[0],	formatStr);
		traceFormatedFloat(trace, ceph.ura[1],	formatStr);
		trace << "\n";

		trace << "    ";
		traceFormatedFloat(trace, ceph.ura[3],	formatStr);
		traceFormatedFloat(trace, ceph.svh,		formatStr);
		traceFormatedFloat(trace, ceph.tgd[0],	formatStr);
		traceFormatedFloat(trace, ceph.ura[2],	formatStr);
		trace << "\n";

		trace << "    ";
		traceFormatedFloat(trace, ceph.isc[0],	formatStr);
		traceFormatedFloat(trace, ceph.isc[1],	formatStr);
		traceFormatedFloat(trace, ceph.isc[2],	formatStr);
		traceFormatedFloat(trace, ceph.isc[3],	formatStr);
		trace << "\n";


		if (type==+E_NavMsgType::CNAV)
		{
			trace << "    ";
			traceFormatedFloat(trace, GTow(ceph.ttm),	formatStr);
			traceFormatedFloat(trace, ceph.wnop,		formatStr);
			trace << "                   ";
			trace << "                   ";
			trace << "\n";
		}
		else if (type==+E_NavMsgType::CNV2)
		{
			trace << "    ";
			traceFormatedFloat(trace, ceph.isc[4],	formatStr);
			traceFormatedFloat(trace, ceph.isc[5],	formatStr);
			trace << "                   ";
			trace << "                   ";
			trace << "\n";

			trace << "    ";
			traceFormatedFloat(trace, GTow(ceph.ttm),	formatStr);
			traceFormatedFloat(trace, ceph.wnop,		formatStr);
			trace << "                   ";
			trace << "                   ";
			trace << "\n";
		}
	}
	else
	{
		traceFormatedFloat(trace, ceph.orb,		formatStr);
		traceFormatedFloat(trace, ceph.tops,	formatStr);
		trace << "\n";

		trace << "    ";
		traceFormatedFloat(trace, ceph.sis[0],	formatStr);
		traceFormatedFloat(trace, ceph.sis[1],	formatStr);
		traceFormatedFloat(trace, ceph.sis[2],	formatStr);
		traceFormatedFloat(trace, ceph.sis[3],	formatStr);
		trace << "\n";

		if  ( type==+E_NavMsgType::CNV1
			||type==+E_NavMsgType::CNV2)
		{
			trace << "    ";
			if		(type==+E_NavMsgType::CNV1)
			{
				traceFormatedFloat(trace, ceph.isc[0],	formatStr);
				trace << "                   ";
			}
			else if	(type==+E_NavMsgType::CNV2)
			{
				trace << "                   ";
				traceFormatedFloat(trace, ceph.isc[1],	formatStr);
			}
			traceFormatedFloat(trace, ceph.tgd[0],	formatStr);
			traceFormatedFloat(trace, ceph.tgd[1],	formatStr);
			trace << "\n";

			trace << "    ";
			traceFormatedFloat(trace, ceph.sis[4],	formatStr);
			traceFormatedFloat(trace, ceph.svh,		formatStr);
			traceFormatedFloat(trace, ceph.flag,	formatStr);
			traceFormatedFloat(trace, ceph.iodc,	formatStr);
			trace << "\n";

			trace << "    ";
			traceFormatedFloat(trace, BTow(ceph.ttm),	formatStr);
			trace << "                   ";
			trace << "                   ";
			traceFormatedFloat(trace, ceph.iode,		formatStr);
			trace << "\n";
		}
		else if (type==+E_NavMsgType::CNV3)
		{
			trace << "    ";
			traceFormatedFloat(trace, ceph.sis[4],	formatStr);
			traceFormatedFloat(trace, ceph.svh,		formatStr);
			traceFormatedFloat(trace, ceph.flag,	formatStr);
			traceFormatedFloat(trace, ceph.tgd[2],	formatStr);
			trace << "\n";

			trace << "    ";
			traceFormatedFloat(trace, BTow(ceph.ttm),	formatStr);
			trace << "                   ";
			trace << "                   ";
			trace << "                   ";
			trace << "\n";
		}
	}
}


void outputNavRinexSTO(
	STO&			sto,
	Trace&			trace,
	const double	rnxver)
{
	string formatStr = "% 15.12fE%+03d";

	if (rnxver < 4.0)
	{
		BOOST_LOG_TRIVIAL(error) << "Error output STO to RINEX incorrect RINEX version=" << rnxver;
		return;
	}

	if (sto.Sat.prn > 0)	tracepdeex(0, trace, "> STO %-3s %-4s", sto.Sat.id().c_str(), sto.type);
	else					tracepdeex(0, trace, "> STO %-3c %-4s", sto.Sat.sysChar(),    sto.type);
	trace << "\n";

	auto sys = sto.Sat.sys;

	E_TimeSys tsys = E_TimeSys::GPST;
	switch (sys)
	{
		case E_Sys::GPS:	tsys = E_TimeSys::GPST;		break;
		case E_Sys::GLO:	tsys = E_TimeSys::UTC;		break;
		case E_Sys::GAL:	tsys = E_TimeSys::GST;		break;
		case E_Sys::BDS:	tsys = E_TimeSys::BDT;		break;
		case E_Sys::QZS:	tsys = E_TimeSys::QZSST;	break;
		case E_Sys::SBS:	tsys = E_TimeSys::GPST;		break;
		default:			tsys = E_TimeSys::GPST;		break;
	}

	double ep[6] = {0};
	time2epoch(sto.tot, ep, tsys);

	tracepdeex(0, trace, "    %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	string sbasId;
	if (sto.sid != +E_SbasId::NONE)
	{
		sbasId = sto.sid._to_string();
		std::replace(sbasId.begin(), sbasId.end(), '_', '-');
	}
	tracepdeex(0, trace, " %-18s %-18s %-18s", sto.code._to_string(), sbasId.c_str(), utcIdStr[sto.uid]);
	trace << "\n";

	double	ttm		= 0;
	int		week	= 0;
	int		weekRef	= 0;
	if (sys != +E_Sys::BDS)		{	ttm = GTow(sto.ttm);	week = GWeek(sto.ttm);	weekRef = GWeek(sto.tot);	}
	else 						{	ttm = BTow(sto.ttm);	week = BWeek(sto.ttm);	weekRef = BWeek(sto.tot);	}

	trace << "    ";
	traceFormatedFloat(trace, ttm + (week - weekRef) * 604800.0, formatStr);				/* align ttm to tot week */
	traceFormatedFloat(trace, sto.A0,	formatStr);
	traceFormatedFloat(trace, sto.A1,	formatStr);
	traceFormatedFloat(trace, sto.A2,	formatStr);
	trace << "\n";
}


void outputNavRinexEOP(
	EOP&			eop,
	Trace&			trace,
	const double	rnxver)
{
	string formatStr = "% 15.12fE%+03d";

	if (rnxver < 4.0)
	{
		BOOST_LOG_TRIVIAL(error) << "Error output EOP to RINEX incorrect RINEX version=" << rnxver;
		return;
	}

	if (eop.Sat.prn > 0)	tracepdeex(0, trace, "> EOP %-3s %-4s", eop.Sat.id().c_str(), eop.type);
	else					tracepdeex(0, trace, "> EOP %-3c %-4s", eop.Sat.sysChar(),    eop.type);
	trace << "\n";

	auto sys = eop.Sat.sys;

	E_TimeSys tsys = E_TimeSys::GPST;
	switch (sys)
	{
		case E_Sys::GPS:	tsys = E_TimeSys::GPST;		break;
		case E_Sys::GLO:	tsys = E_TimeSys::UTC;		break;
		case E_Sys::GAL:	tsys = E_TimeSys::GST;		break;
		case E_Sys::BDS:	tsys = E_TimeSys::BDT;		break;
		case E_Sys::QZS:	tsys = E_TimeSys::QZSST;	break;
		case E_Sys::SBS:	tsys = E_TimeSys::GPST;		break;
		default:			tsys = E_TimeSys::GPST;		break;
	}

	double ep[6] = {0};
	time2epoch(eop.teop, ep, tsys);

	tracepdeex(0, trace, "    %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	traceFormatedFloat(trace, eop.xp	* R2AS,	formatStr);
	traceFormatedFloat(trace, eop.xpr	* R2AS,	formatStr);
	traceFormatedFloat(trace, eop.xprr	* R2AS,	formatStr);
	trace << "\n";

	trace << "    ";
	trace << "                   ";
	traceFormatedFloat(trace, eop.yp	* R2AS,	formatStr);
	traceFormatedFloat(trace, eop.ypr	* R2AS,	formatStr);
	traceFormatedFloat(trace, eop.yprr	* R2AS,	formatStr);
	trace << "\n";

	double	ttm		= 0;
	int		week	= 0;
	int		weekRef	= 0;
	if (sys != +E_Sys::BDS)		{	ttm = GTow(eop.ttm);	week = GWeek(eop.ttm);	weekRef = GWeek(eop.teop);	}
	else 						{	ttm = BTow(eop.ttm);	week = BWeek(eop.ttm);	weekRef = BWeek(eop.teop);	}

	trace << "    ";
	traceFormatedFloat(trace, ttm + (week - weekRef) * 604800.0, formatStr);				/* align ttm to teop week */
	traceFormatedFloat(trace, eop.dut1,			formatStr);
	traceFormatedFloat(trace, eop.dur,			formatStr);
	traceFormatedFloat(trace, eop.durr,			formatStr);
	trace << "\n";
}


void outputNavRinexION(
	ION&			ion,
	Trace&			trace,
	const double	rnxver)
{
	string formatStr = "% 15.12fE%+03d";

	if (rnxver < 4.0)
	{
		BOOST_LOG_TRIVIAL(error) << "Error output ION to RINEX incorrect RINEX version=" << rnxver;
		return;
	}

	if (ion.Sat.prn > 0)	tracepdeex(0, trace, "> ION %-3s %-4s", ion.Sat.id().c_str(), ion.type);
	else					tracepdeex(0, trace, "> ION %-3c %-4s", ion.Sat.sysChar(),    ion.type);
	trace << "\n";

	auto sys	= ion.Sat.sys;
	auto type	= ion.type;

	E_TimeSys tsys = E_TimeSys::GPST;
	switch (sys)
	{
		case E_Sys::GPS:	tsys = E_TimeSys::GPST;		break;
		case E_Sys::GLO:	tsys = E_TimeSys::UTC;		break;
		case E_Sys::GAL:	tsys = E_TimeSys::GST;		break;
		case E_Sys::BDS:	tsys = E_TimeSys::BDT;		break;
		case E_Sys::QZS:	tsys = E_TimeSys::QZSST;	break;
		case E_Sys::SBS:	tsys = E_TimeSys::GPST;		break;
		default:			tsys = E_TimeSys::GPST;		break;
	}

	double ep[6] = {0};
	time2epoch(ion.ttm, ep, tsys);

	tracepdeex(0, trace, "    %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	if	(sys==+E_Sys::GAL)
	{
		traceFormatedFloat(trace, ion.ai0,	formatStr);
		traceFormatedFloat(trace, ion.ai1,	formatStr);
		traceFormatedFloat(trace, ion.ai2,	formatStr);
		trace << "\n";

		trace << "    ";
		traceFormatedFloat(trace, ion.flag,	formatStr);
		trace << "                   ";
		trace << "                   ";
		trace << "                   ";
		trace << "\n";
	}
	else if	( sys  == +E_Sys::BDS
			&&type == +E_NavMsgType::CNVX)
	{
		traceFormatedFloat(trace, ion.alpha1,	formatStr);
		traceFormatedFloat(trace, ion.alpha2,	formatStr);
		traceFormatedFloat(trace, ion.alpha3,	formatStr);
		trace << "\n";

		trace << "    ";
		traceFormatedFloat(trace, ion.alpha4,	formatStr);
		traceFormatedFloat(trace, ion.alpha5,	formatStr);
		traceFormatedFloat(trace, ion.alpha6,	formatStr);
		traceFormatedFloat(trace, ion.alpha7,	formatStr);
		trace << "\n";

		trace << "    ";
		traceFormatedFloat(trace, ion.alpha8,	formatStr);
		traceFormatedFloat(trace, ion.alpha9,	formatStr);
		trace << "                   ";
		trace << "                   ";
		trace << "\n";
	}
	else if	( type==+E_NavMsgType::LNAV
			||type==+E_NavMsgType::D1D2
			||type==+E_NavMsgType::CNVX)
	{
		traceFormatedFloat(trace, ion.a0,	formatStr);
		traceFormatedFloat(trace, ion.a1,	formatStr);
		traceFormatedFloat(trace, ion.a2,	formatStr);
		trace << "\n";

		trace << "    ";
		traceFormatedFloat(trace, ion.a3,	formatStr);
		traceFormatedFloat(trace, ion.b0,	formatStr);
		traceFormatedFloat(trace, ion.b1,	formatStr);
		traceFormatedFloat(trace, ion.b2,	formatStr);
		trace << "\n";

		trace << "    ";
		traceFormatedFloat(trace, ion.b3,	formatStr);
		traceFormatedFloat(trace, ion.code,	formatStr);
		trace << "                   ";
		trace << "                   ";
		trace << "\n";
	}
}


void outputNavRinexBody(
	RinexNavFileOutput&	outFileData,
	Trace&				rinexStream,
	const double		rnxver)
{
	for (auto& [sys, output] : outFileData.sysMap)
	{
		if (output == false)
			continue;

		// EOP/ION messages
		// STO messages ignored
		if (rnxver >= 4)
		{
			E_NavMsgType type = defNavMsgType[sys];

			auto* ion_ptr = seleph<ION>(std::cout, tsync, sys, type, nav);	if (ion_ptr != nullptr)	outputNavRinexION(*ion_ptr, rinexStream, rnxver);
			auto* eop_ptr = seleph<EOP>(std::cout, tsync, sys, type, nav);	if (eop_ptr != nullptr)	outputNavRinexEOP(*eop_ptr, rinexStream, rnxver);
		}

		for (auto& [Sat, satNav] : nav.satNavMap)
		{
			if (Sat.sys != sys)
				continue;

			E_NavMsgType nvtyp = acsConfig.used_nav_types[Sat.sys];

			if (sys == +E_Sys::GLO)
			{
				auto geph_ptr = seleph<Geph>(std::cout, tsync, Sat, nvtyp, ANY_IODE, nav);

				if (geph_ptr == nullptr)
					continue;

				auto& geph = *geph_ptr;

				if	( outFileData.last_iode.find(Sat)	== outFileData.last_iode.end()
					||geph.iode							!= outFileData.last_iode[Sat])
				{
					outFileData.last_iode[Sat] = geph.iode;
					outputNavRinexGeph(geph, rinexStream, rnxver);
				}

				continue;
			}
			else if (sys == +E_Sys::SBS)
			{
				//optional to do (probably not useful): Seph writing
				// Seph* seph_ptr = seleph<Seph>(std::cout, tsync, sat, ANY_IODE, nav);

				// if (seph_ptr == nullptr)
				// 	continue;

				// if	( outFileData.last_iode.find(sat)	== outFileData.last_iode.end()
				// 	||seph_ptr->iode					!= outFileData.last_iode[sat])
				// {
				// 	outFileData.last_iode[sat] = seph_ptr->iode;
				// 	outputNavRinexSeph(*seph_ptr, rinexStream, rnxver);
				// }

				continue;
			}
			else
			{
				auto eph_ptr = seleph<Eph>(std::cout, tsync, Sat, nvtyp, ANY_IODE, nav);

				if (eph_ptr == nullptr)
					continue;

				// Note iode can be zero, checking the map makes a zero entry as well.
				if	(  outFileData.last_iode.find(Sat)	== outFileData.last_iode.end()
					|| eph_ptr->iode					!= outFileData.last_iode[Sat])
				{
					outFileData.last_iode[Sat] = eph_ptr->iode;
					outputNavRinexEph(*eph_ptr, rinexStream, rnxver);
				}

				continue;
			}
		}
	}
}


void rinexNavHeader(
	map<E_Sys, bool>&	sysMap,
	Trace&				rinexStream,
	const double		rnxver)
{
	string prog		= "PEA v2";
	string runby	= "Geoscience Australia";

	UtcTime now = timeGet();

	string timeDate = now.to_string(0);
	boost::replace_all(timeDate, "-", "");
	boost::replace_all(timeDate, ":", "");
	timeDate += " UTC";

	string sysDesc;
	if (sysMap.size() == 1)	sysDesc = rinexSysDesc(sysMap.begin()->first);
	else					sysDesc = rinexSysDesc(E_Sys::COMB);

	tracepdeex(0, rinexStream, "%9.2f%-11s%-20s%-20s%-20s\n",
		rnxver,
		"",
		"N: GNSS NAV DATA",
		sysDesc.c_str(),
		"RINEX VERSION / TYPE");

	tracepdeex(0, rinexStream, "%-20.20s%-20.20s%-20.20s%-20s\n",
		prog.c_str(),
		runby.c_str(),
		timeDate.c_str(),
		"PGM / RUN BY / DATE");

	if (int(rnxver) == 3)
	{
		for (auto& [sys, output] : sysMap)
		{
			if (output == false)
			{
				continue;
			}

			double* ion_arr1 = nullptr;
			double* ion_arr2 = nullptr;
			string	ion_str1;
			string	ion_str2;

			E_NavMsgType type = defNavMsgType[sys];

			auto& ionList = nav.ionMap[sys][type];

			for (auto& [dummy, ion] : ionList)
			{
				switch (sys)
				{
					case E_Sys::GPS:
					case E_Sys::QZS:
					case E_Sys::BDS:	ion_arr1 = &ion.vals[0];	ion_str1 = sys._to_string();	ion_str1 += 'A';
										ion_arr2 = &ion.vals[4];	ion_str2 = sys._to_string();	ion_str2 += 'B';	break;
					case E_Sys::GAL:	ion_arr1 = &ion.vals[0];	ion_str1 = sys._to_string();	ion_str1 += ' ';	break;
					default:
						continue;
				}

				if (ion_arr1)
					tracepdeex(0, rinexStream, "%s %12.4E%12.4E%12.4E%12.4E%7s%-20s\n",
						ion_str1.c_str(),
						ion_arr1[0],
						ion_arr1[1],
						ion_arr1[2],
						ion_arr1[3], "", "IONOSPHERIC CORR");

				if (ion_arr2)
					tracepdeex(0, rinexStream, "%s %12.4E%12.4E%12.4E%12.4E%7s%-20s\n",
						ion_str2.c_str(),
						ion_arr2[0],
						ion_arr2[1],
						ion_arr2[2],
						ion_arr2[3], "", "IONOSPHERIC CORR");
			}
		}

		for (auto& [sys, output] : sysMap)
		{
			if (output == false)
				continue;

			E_StoCode		code1 = E_StoCode::NONE;
			E_StoCode		code2 = E_StoCode::NONE;
			E_NavMsgType	type = defNavMsgType[sys];

			switch (sys)
			{
				case E_Sys::GPS:	code1 = E_StoCode::GPUT;								break;
				case E_Sys::GLO:	code1 = E_StoCode::GLUT;	code2 = E_StoCode::GLGP;	break;
				case E_Sys::GAL:	code1 = E_StoCode::GAUT;	code2 = E_StoCode::GAGP;	break;
				case E_Sys::QZS:	code1 = E_StoCode::QZUT;	code2 = E_StoCode::QZGP;	break;
				case E_Sys::BDS:	code1 = E_StoCode::BDUT;								break;

				default:
					continue;
			}

			for (auto& code : {code1, code2})
			{
				auto& stoList = nav.stoMap[code][type];

				for (auto& [dummy, sto] : stoList)
				{
					int		week;
					double	tow;
					if (sys != +E_Sys::BDS)	{	tow = GTow(sto.tot);	week = GWeek(sto.tot);	}
					else 					{	tow = BTow(sto.tot);	week = BWeek(sto.tot);	}

					tracepdeex(0, rinexStream, "%s %17.10E%16.9E%7.0f%5.0f %-5s %-2s %-20s\n",
						code._to_string(),
						sto.A0,
						sto.A1,
						tow,
						week, "", "", "TIME SYSTEM CORR");
				}
			}
		}
	}

	tracepdeex(0, rinexStream, "%6d%54s%-20s\n", nav.leaps, "", 	"LEAP SECONDS");
	tracepdeex(0, rinexStream, "%60s%-20s\n", "",					"END OF HEADER");
}

void writeRinexNav(const double rnxver)
{
	auto filenameSysMap = getSysOutputFilenames(acsConfig.rinex_nav_filename, tsync);

	for (auto [filename, sysMap] : filenameSysMap)
	{
		auto& fileData = filenameNavFileDataMap[filename];

		std::ofstream rinexStream(filename, std::ofstream::app);
		if (!rinexStream)
		{
			BOOST_LOG_TRIVIAL(error) << "Error opening " << filename << " for writing rinex nav";
			return;
		}

		if (rinexStream.tellp() == 0)
			rinexNavHeader(sysMap, rinexStream, rnxver);

		fileData.sysMap = sysMap;

		outputNavRinexBody(fileData, rinexStream, rnxver);
	}
}

void outputNavRinexBodyAll(
	Trace&				rinexStream,
	const double		rnxver)
{
	if (rnxver >= 4)
	{
		for (auto& [code,	stoCodeMap]	: nav.stoMap)
		for (auto& [type,	stoList]	: stoCodeMap)
		for (auto it = stoList.rbegin(); it != stoList.rend(); it++)
		{
			auto& [key, value] = *it;

			outputNavRinexSTO(value, rinexStream, rnxver);
		}

		for (auto& [sys,	eopSysMap]	: nav.eopMap)
		for (auto& [type,	eopList]	: eopSysMap)
		for (auto it = eopList.rbegin(); it != eopList.rend(); it++)
		{
			auto& [key, value] = *it;

			outputNavRinexEOP(value, rinexStream, rnxver);
		}

		for (auto& [sys,	ionSysMap]	: nav.ionMap)
		for (auto& [type,	ionList]	: ionSysMap)
		for (auto it = ionList.rbegin(); it != ionList.rend(); it++)
		{
			auto& [key, value] = *it;

			outputNavRinexION(value, rinexStream, rnxver);
		}
	}

	for (auto& [satId,	navList]	: nav.ephMap)
	for (auto& [nvtyp,	ephList]	: navList)
	for (auto it = ephList.rbegin(); it != ephList.rend(); it++)
	{
		auto& [key, value] = *it;

		outputNavRinexEph(value, rinexStream, rnxver);
	}

	for (auto& [satId,	navList]	: nav.gephMap)
	for (auto& [nvtyp,	gephList]	: navList)
	for (auto it = gephList.rbegin(); it != gephList.rend(); it++)
	{
		auto& [key, value] = *it;

		outputNavRinexGeph(value, rinexStream, rnxver);
	}

	// for (auto& [satId,	sephList]	: nav.sephMap)
	// for (auto it = sephList.rbegin(); it != sephList.rend(); it++)
	// {
// 			auto& [key, value] = *it;

	// 	outputNavRinexSeph(ritSeph->second, rinexStream, rnxver);
	// }

	for (auto& [satId,	cephSatMap]	: nav.cephMap)
	for (auto& [type,	cephList]	: cephSatMap)
	for (auto it = cephList.rbegin(); it != cephList.rend(); it++)
	{
		auto& [key, value] = *it;

		outputNavRinexCeph(value, rinexStream, rnxver);
	}
}

void writeRinexNavAll(string filename, const double rnxver)
{
	map<E_Sys, bool>	sysMap =
	{
		{E_Sys::GPS, true},
		{E_Sys::GLO, true},
		{E_Sys::GAL, true},
		{E_Sys::BDS, true},
		{E_Sys::QZS, true},
		{E_Sys::SBS, true}
	};

	std::ofstream rinexStream(filename, std::ofstream::app);
	if (!rinexStream)
	{
		BOOST_LOG_TRIVIAL(error) << "Error opening " << filename << " for writing rinex nav";
		return;
	}

	if (rinexStream.tellp() == 0)
		rinexNavHeader(sysMap, rinexStream, rnxver);


	outputNavRinexBodyAll(rinexStream, rnxver);
}
