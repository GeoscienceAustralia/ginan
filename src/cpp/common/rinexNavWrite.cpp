
// #pragma GCC optimize ("O0")

#include <fstream>

#include <boost/log/trivial.hpp>

#include "rinexNavWrite.hpp"
#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "streamTrace.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "common.hpp"
#include "rinex.hpp"



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
		trace << std::endl;
	}

	auto sys = eph.Sat.sys;

	double ep[6];
	if (sys != +E_Sys::BDS)	{		time2epoch(eph.toc,				ep);	}
	else					{		time2epoch(gpst2bdt(eph.toc),	ep); 	}	/* gpst -> bdt */

	tracepdeex(0, trace, "%-3s %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", eph.Sat.id().c_str(), ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	traceFormatedFloat(trace, eph.f0,		formatStr);
	traceFormatedFloat(trace, eph.f1,		formatStr);
	traceFormatedFloat(trace, eph.f2,		formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, eph.iode,		formatStr); /* GPS/QZS: IODE, GAL: IODnav, BDS: AODE */
	traceFormatedFloat(trace, eph.crs,		formatStr);
	traceFormatedFloat(trace, eph.deln,		formatStr);
	traceFormatedFloat(trace, eph.M0,		formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, eph.cuc,		formatStr);
	traceFormatedFloat(trace, eph.e,		formatStr);
	traceFormatedFloat(trace, eph.cus,		formatStr);
	traceFormatedFloat(trace, sqrt(eph.A),	formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, eph.toes,		formatStr);
	traceFormatedFloat(trace, eph.cic,		formatStr);
	traceFormatedFloat(trace, eph.OMG0,		formatStr);
	traceFormatedFloat(trace, eph.cis,		formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, eph.i0,		formatStr);
	traceFormatedFloat(trace, eph.crc,		formatStr);
	traceFormatedFloat(trace, eph.omg,		formatStr);
	traceFormatedFloat(trace, eph.OMGd,		formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, eph.idot,		formatStr);
	traceFormatedFloat(trace, eph.code,		formatStr);
	traceFormatedFloat(trace, eph.week,		formatStr); /* GPS/QZS: GPS week, GAL: GAL week, BDS: BDT week */
	traceFormatedFloat(trace, eph.flag,		formatStr);
	trace << std::endl;

	trace << "    ";
	if (sys != +E_Sys::GAL)		{	traceFormatedFloat(trace, svaToUra(eph.sva),	formatStr);	}
	else						{	traceFormatedFloat(trace, svaToSisa(eph.sva),	formatStr);	}
	
	traceFormatedFloat(trace, eph.svh,		formatStr);
	traceFormatedFloat(trace, eph.tgd[0],	formatStr); /* GPS/QZS:TGD, GAL:BGD E5a/E1, BDS: TGD1 B1/B3 */

	if	(  sys == +E_Sys::GAL
		|| sys == +E_Sys::BDS)	{	traceFormatedFloat(trace, eph.tgd[1],	formatStr);	}	/* GAL:BGD E5b/E1, BDS: TGD2 B2/B3 */	
	else						{	traceFormatedFloat(trace, eph.iodc,		formatStr);	}	/* GPS/QZS:IODC */	
	trace << std::endl;

	trace << "    ";
	double ttm;
	int week;
	if (sys != +E_Sys::BDS)		{	ttm = time2gpst(eph.ttm,			&week);	}	
	else 						{	ttm = time2bdt(gpst2bdt(eph.ttm),	&week);	} /* gpst -> bdt */
	traceFormatedFloat(trace, ttm + (week - eph.week) * 604800.0, formatStr);

	if		(  sys == +E_Sys::GPS 
			|| sys == +E_Sys::QZS)	{		traceFormatedFloat(trace, eph.fit,	formatStr);	}	
	else if (  sys == +E_Sys::BDS)	{		traceFormatedFloat(trace, eph.iodc,	formatStr);	}	/* AODC */
	else							{		traceFormatedFloat(trace, 0,		formatStr);	}	/* spare */
	trace << std::endl;
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
		trace << std::endl;
	}

	if (geph.Sat.sys != +E_Sys::GLO)
	{
		BOOST_LOG_TRIVIAL(error) << "Error output Geph to RINEX incorrect system type";
		return;
	}

	double tof = time2gpst(gpst2utc(geph.tof), nullptr);      /* v.3: tow in utc */

	double ep[6];
	GTime toe = gpst2utc(geph.toe); /* gpst -> utc */
	time2epoch(toe, ep);

	tracepdeex(0, trace, "%-3s %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", geph.Sat.id().c_str(), ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);
	traceFormatedFloat(trace, geph.taun,	formatStr);
	traceFormatedFloat(trace, geph.gamn,	formatStr);
	traceFormatedFloat(trace, tof, formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, geph.pos[0] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.vel[0] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.acc[0] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.svh, formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, geph.pos[1] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.vel[1] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.acc[1] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.frq, formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, geph.pos[2] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.vel[2] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.acc[2] / 1E3, formatStr);
	traceFormatedFloat(trace, geph.age, formatStr);
	trace << std::endl;
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
	trace << std::endl;

	auto sys	= ceph.Sat.sys;
	auto type	= ceph.type;

	double ep[6];
	if (sys != +E_Sys::BDS)	{		time2epoch(ceph.toc,			ep);	}
	else					{		time2epoch(gpst2bdt(ceph.toc),	ep); 	}	/* gpst -> bdt */

	tracepdeex(0, trace, "%-3s %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", ceph.Sat.id().c_str(), ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	traceFormatedFloat(trace, ceph.f0,		formatStr);
	traceFormatedFloat(trace, ceph.f1,		formatStr);
	traceFormatedFloat(trace, ceph.f2,		formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, ceph.Adot,	formatStr);
	traceFormatedFloat(trace, ceph.crs,		formatStr);
	traceFormatedFloat(trace, ceph.deln,	formatStr);
	traceFormatedFloat(trace, ceph.M0,		formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, ceph.cuc,		formatStr);
	traceFormatedFloat(trace, ceph.e,		formatStr);
	traceFormatedFloat(trace, ceph.cus,		formatStr);
	traceFormatedFloat(trace, sqrt(ceph.A),	formatStr);
	trace << std::endl;

	trace << "    ";
	if	(sys != +E_Sys::BDS)	{	traceFormatedFloat(trace, ceph.tops,	formatStr);	}
	else						{	traceFormatedFloat(trace, ceph.toes,	formatStr);	}

	traceFormatedFloat(trace, ceph.cic,		formatStr);
	traceFormatedFloat(trace, ceph.OMG0,	formatStr);
	traceFormatedFloat(trace, ceph.cis,		formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, ceph.i0,		formatStr);
	traceFormatedFloat(trace, ceph.crc,		formatStr);
	traceFormatedFloat(trace, ceph.omg,		formatStr);
	traceFormatedFloat(trace, ceph.OMGd,	formatStr);
	trace << std::endl;

	trace << "    ";
	traceFormatedFloat(trace, ceph.idot,	formatStr);
	traceFormatedFloat(trace, ceph.dn0d,	formatStr);

	if	(sys != +E_Sys::BDS)
	{
		traceFormatedFloat(trace, ceph.ura[0],	formatStr);
		traceFormatedFloat(trace, ceph.ura[1],	formatStr);
		trace << std::endl;

		trace << "    ";
		traceFormatedFloat(trace, ceph.ura[3],	formatStr);
		traceFormatedFloat(trace, ceph.svh,		formatStr);
		traceFormatedFloat(trace, ceph.tgd[0],	formatStr);
		traceFormatedFloat(trace, ceph.ura[2],	formatStr);
		trace << std::endl;

		trace << "    ";
		traceFormatedFloat(trace, ceph.isc[0],	formatStr);
		traceFormatedFloat(trace, ceph.isc[1],	formatStr);
		traceFormatedFloat(trace, ceph.isc[2],	formatStr);
		traceFormatedFloat(trace, ceph.isc[3],	formatStr);
		trace << std::endl;

		if (type==+E_NavMsgType::CNAV)
		{
			trace << "    ";
			double ttm;
			int week;
			ttm = time2gpst(ceph.ttm,	&week);	
			traceFormatedFloat(trace, ttm, 			formatStr);
			traceFormatedFloat(trace, ceph.wnop,	formatStr);
			trace << "                   ";
			trace << "                   ";
			trace << std::endl;
		}
		else if (type==+E_NavMsgType::CNV2)
		{
			trace << "    ";
			traceFormatedFloat(trace, ceph.isc[4],	formatStr);
			traceFormatedFloat(trace, ceph.isc[5],	formatStr);
			trace << "                   ";
			trace << "                   ";
			trace << std::endl;

			trace << "    ";
			double ttm;
			int week;
			ttm = time2gpst(ceph.ttm,	&week);	
			traceFormatedFloat(trace, ttm, 			formatStr);
			traceFormatedFloat(trace, ceph.wnop,	formatStr);
			trace << "                   ";
			trace << "                   ";
			trace << std::endl;
		}
	}
	else
	{
		traceFormatedFloat(trace, ceph.orb,		formatStr);
		traceFormatedFloat(trace, ceph.tops,	formatStr);
		trace << std::endl;

		trace << "    ";
		traceFormatedFloat(trace, ceph.sis[0],	formatStr);
		traceFormatedFloat(trace, ceph.sis[1],	formatStr);
		traceFormatedFloat(trace, ceph.sis[2],	formatStr);
		traceFormatedFloat(trace, ceph.sis[3],	formatStr);
		trace << std::endl;

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
			trace << std::endl;

			trace << "    ";
			traceFormatedFloat(trace, ceph.sis[4],	formatStr);
			traceFormatedFloat(trace, ceph.svh,		formatStr);
			traceFormatedFloat(trace, ceph.flag,	formatStr);
			traceFormatedFloat(trace, ceph.iodc,	formatStr);
			trace << std::endl;

			trace << "    ";
			double ttm;
			int week;
			ttm = time2bdt(gpst2bdt(ceph.ttm),	&week);
			traceFormatedFloat(trace, ttm, 			formatStr);
			trace << "                   ";
			traceFormatedFloat(trace, ceph.iode,	formatStr);
			trace << "                   ";
			trace << std::endl;
		}
		else if (type==+E_NavMsgType::CNV3)
		{
			trace << "    ";
			traceFormatedFloat(trace, ceph.sis[4],	formatStr);
			traceFormatedFloat(trace, ceph.svh,		formatStr);
			traceFormatedFloat(trace, ceph.flag,	formatStr);
			traceFormatedFloat(trace, ceph.tgd[2],	formatStr);
			trace << std::endl;

			trace << "    ";
			double ttm;
			int week;
			ttm = time2bdt(gpst2bdt(ceph.ttm),	&week);
			traceFormatedFloat(trace, ttm,			formatStr);
			trace << "                   ";
			trace << "                   ";
			trace << "                   ";
			trace << std::endl;
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
	trace << std::endl;

	auto sys = sto.Sat.sys;

	double ep[6];
	if (sys != +E_Sys::BDS)	{		time2epoch(sto.tot,				ep);	}
	else					{		time2epoch(gpst2bdt(sto.tot),	ep); 	}

	tracepdeex(0, trace, "    %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	string sbasId;
	if (sto.sid != +E_SbasId::NONE)
	{
		sbasId = sto.sid._to_string();
		std::replace(sbasId.begin(), sbasId.end(), '_', '-');
	}
	tracepdeex(0, trace, " %-18s %-18s %-18s", sto.code._to_string(), sbasId.c_str(), utcIdStr[sto.uid]);
	trace << std::endl;

	trace << "    ";
	double ttm;
	int week, weekRef;
	if (sys != +E_Sys::BDS)		{	time2gpst(sto.tot,				&weekRef);	ttm = time2gpst(sto.ttm,			&week);	}
	else 						{	time2bdt(gpst2bdt(sto.tot),		&weekRef);	ttm = time2bdt(gpst2bdt(sto.ttm),	&week);	} /* gpst -> bdt */
	traceFormatedFloat(trace, ttm + (week - weekRef) * 604800.0, formatStr);
	traceFormatedFloat(trace, sto.A0,	formatStr);
	traceFormatedFloat(trace, sto.A1,	formatStr);
	traceFormatedFloat(trace, sto.A2,	formatStr);
	trace << std::endl;
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
	trace << std::endl;

	auto sys = eop.Sat.sys;

	double ep[6];
	if (sys != +E_Sys::BDS)	{		time2epoch(eop.teop,			ep);	}
	else					{		time2epoch(gpst2bdt(eop.teop),	ep); 	}

	tracepdeex(0, trace, "    %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	traceFormatedFloat(trace, eop.xp	* R2AS,	formatStr);
	traceFormatedFloat(trace, eop.xpr	* R2AS,	formatStr);
	traceFormatedFloat(trace, eop.xprr	* R2AS,	formatStr);
	trace << std::endl;

	trace << "    ";
	trace << "                   ";
	traceFormatedFloat(trace, eop.yp	* R2AS,	formatStr);
	traceFormatedFloat(trace, eop.ypr	* R2AS,	formatStr);
	traceFormatedFloat(trace, eop.yprr	* R2AS,	formatStr);
	trace << std::endl;

	trace << "    ";
	double ttm;
	int week, weekRef;
	if (sys != +E_Sys::BDS)		{	time2gpst(eop.teop,				&weekRef);	ttm = time2gpst(eop.ttm,			&week);	}
	else 						{	time2bdt(gpst2bdt(eop.teop),	&weekRef);	ttm = time2bdt(gpst2bdt(eop.ttm),	&week);	} /* gpst -> bdt */
	traceFormatedFloat(trace, ttm + (week - weekRef) * 604800.0, formatStr);
	traceFormatedFloat(trace, eop.dut1,			formatStr);
	traceFormatedFloat(trace, eop.dur,			formatStr);
	traceFormatedFloat(trace, eop.durr,			formatStr);
	trace << std::endl;
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
	trace << std::endl;

	auto sys	= ion.Sat.sys;
	auto type	= ion.type;

	double ep[6];
	if (sys != +E_Sys::BDS)	{		time2epoch(ion.ttm,				ep);	}
	else					{		time2epoch(gpst2bdt(ion.ttm),	ep); 	}

	tracepdeex(0, trace, "    %04.0f %02.0f %02.0f %02.0f %02.0f %02.0f", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	if	(sys==+E_Sys::GAL)
	{
		traceFormatedFloat(trace, ion.ai0,	formatStr);
		traceFormatedFloat(trace, ion.ai1,	formatStr);
		traceFormatedFloat(trace, ion.ai2,	formatStr);
		trace << std::endl;

		trace << "    ";
		traceFormatedFloat(trace, ion.flag,	formatStr);
		trace << "                   ";
		trace << "                   ";
		trace << "                   ";
		trace << std::endl;
	}
	else if	( sys==+E_Sys::BDS
			&&type==+E_NavMsgType::CNVX)
	{
		traceFormatedFloat(trace, ion.alpha1,	formatStr);
		traceFormatedFloat(trace, ion.alpha2,	formatStr);
		traceFormatedFloat(trace, ion.alpha3,	formatStr);
		trace << std::endl;

		trace << "    ";
		traceFormatedFloat(trace, ion.alpha4,	formatStr);
		traceFormatedFloat(trace, ion.alpha5,	formatStr);
		traceFormatedFloat(trace, ion.alpha6,	formatStr);
		traceFormatedFloat(trace, ion.alpha7,	formatStr);
		trace << std::endl;

		trace << "    ";
		traceFormatedFloat(trace, ion.alpha8,	formatStr);
		traceFormatedFloat(trace, ion.alpha9,	formatStr);
		trace << "                   ";
		trace << "                   ";
		trace << std::endl;
	}
	else if	( type==+E_NavMsgType::LNAV
			||type==+E_NavMsgType::D1D2
			||type==+E_NavMsgType::CNVX)
	{
		traceFormatedFloat(trace, ion.a0,	formatStr);
		traceFormatedFloat(trace, ion.a1,	formatStr);
		traceFormatedFloat(trace, ion.a2,	formatStr);
		trace << std::endl;
		
		trace << "    ";
		traceFormatedFloat(trace, ion.a3,	formatStr);
		traceFormatedFloat(trace, ion.b0,	formatStr);
		traceFormatedFloat(trace, ion.b1,	formatStr);
		traceFormatedFloat(trace, ion.b2,	formatStr);
		trace << std::endl;

		trace << "    ";
		traceFormatedFloat(trace, ion.b3,	formatStr);
		traceFormatedFloat(trace, ion.code,	formatStr);
		trace << "                   ";
		trace << "                   ";
		trace << std::endl;
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

			ION* ion_ptr = seleph<ION>(std::cout, tsync, sys, type, nav);	if (ion_ptr != nullptr)	outputNavRinexION(*ion_ptr, rinexStream, rnxver);
			EOP* eop_ptr = seleph<EOP>(std::cout, tsync, sys, type, nav);	if (eop_ptr != nullptr)	outputNavRinexEOP(*eop_ptr, rinexStream, rnxver);
		}

		int idode = -1;
		for (auto& [satId, satNav] : nav.satNavMap)
		{
			SatSys sat;
			sat.fromHash(satId);

			if (sat.sys != sys)
				continue;

			if (sys == +E_Sys::GLO)
			{
				Geph* geph_ptr = seleph<Geph>(std::cout, tsync, sat, -1, nav);

				if (geph_ptr == nullptr)
					continue;

				if	( outFileData.last_iode.find(sat)	== outFileData.last_iode.end()
					||geph_ptr->iode					!= outFileData.last_iode[sat])
				{
					outFileData.last_iode[sat] = geph_ptr->iode;
					outputNavRinexGeph(*geph_ptr, rinexStream, rnxver);
				}
				
				continue;
			}
			else if (sys == +E_Sys::SBS)
			{
				//optional to do (probably not useful): Seph writing
				// Seph* seph_ptr = seleph<Seph>(std::cout, tsync, sat, -1, nav);

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
				Eph* eph_ptr = seleph<Eph>(std::cout, tsync, sat, -1, nav);
			
				if (eph_ptr == nullptr)
					continue;

				// Note iode can be zero, checking the map makes a zero entry as well.
				if	(  outFileData.last_iode.find(sat)	== outFileData.last_iode.end() 
					|| eph_ptr->iode					!= outFileData.last_iode[sat])
				{
					outFileData.last_iode[sat] = eph_ptr->iode;
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
	string prog		= "PEA v1";
	string runby	= "Geoscience Australia";

	GTime now = utc2gpst(timeget());

	char tempStr[20];
	time2str(now, tempStr, 0);

	string timeDate(tempStr);
	boost::replace_all(timeDate, "/", "");
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
					int week;
					double tow;
					if (sys == +E_Sys::BDS)	{	tow = time2bdt(gpst2bdt(sto.tot),	&week);	} /* gpst -> bdt */
					else 					{	tow = time2gpst(sto.tot,			&week);	}	

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
		for (auto& [time,	sto]		: stoList)
		{
			outputNavRinexSTO(sto, rinexStream, rnxver);
		}

		for (auto& [sys,	eopSysMap]	: nav.eopMap)
		for (auto& [type,	eopList]	: eopSysMap)
		for (auto& [time,	eop]		: eopList)
		{
			outputNavRinexEOP(eop, rinexStream, rnxver);
		}

		for (auto& [sys,	ionSysMap]	: nav.ionMap)
		for (auto& [type,	ionList]	: ionSysMap)
		for (auto& [time,	ion]		: ionList)
		{
			outputNavRinexION(ion, rinexStream, rnxver);
		}
	}

	for (auto& [satId,	ephList]	: nav.ephMap)
	for (auto& [time,	eph]		: ephList)
	{
		outputNavRinexEph(eph, rinexStream, rnxver);
	}

	for (auto& [satId,	gephList]	: nav.gephMap)
	for (auto& [time,	geph]		: gephList)
	{
		outputNavRinexGeph(geph, rinexStream, rnxver);
	}

	// for (auto& [satId,	sephList]	: nav.sephMap)
	// for (auto& [time,	seph]		: sephList)
	// {
	// 	outputNavRinexSeph(seph, rinexStream, rnxver);
	// }

	for (auto& [satId,	cephSatMap]	: nav.cephMap)
	for (auto& [type,	cephList]	: cephSatMap)
	for (auto& [time,	ceph]		: cephList)
	{
		outputNavRinexCeph(ceph, rinexStream, rnxver);
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
	}
	
	if (rinexStream.tellp() == 0)
		rinexNavHeader(sysMap, rinexStream, rnxver);
	
	outputNavRinexBodyAll(rinexStream, rnxver);
}
