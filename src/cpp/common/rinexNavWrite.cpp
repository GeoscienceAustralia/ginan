
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



void outputNavRinexEph(
	Eph&	eph, 
	Trace&	trace)
{
	string formatStr = "% 15.12fE%+03d";

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
	double ttr;
	int week;
	if (sys != +E_Sys::BDS)		{	ttr = time2gpst(eph.ttr,			&week);	}	
	else 						{	ttr = time2bdt(gpst2bdt(eph.ttr),	&week);	} /* gpst -> bdt */
	traceFormatedFloat(trace, ttr + (week - eph.week) * 604800.0, formatStr);

	if		(  sys == +E_Sys::GPS 
			|| sys == +E_Sys::QZS)	{		traceFormatedFloat(trace, eph.fit,	formatStr);	}	
	else if (  sys == +E_Sys::BDS)	{		traceFormatedFloat(trace, eph.iodc,	formatStr);	}	/* AODC */
	else							{		traceFormatedFloat(trace, 0,		formatStr);	}	/* spare */
	trace << std::endl;
}


void outputNavRinexGeph(
	Geph&	geph, 
	Trace&	trace)
{
	string formatStr = "% 15.12fE%+03d";

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


void outputNavRinexBody(
	RinexNavFileOutput&	outFileData,
	Trace&				rinexStream)
{
	for (auto& [sys, output] : outFileData.sysMap)
	{
		if (output == false)
			continue;

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
					outputNavRinexGeph(*geph_ptr, rinexStream);
				}
				
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
					outputNavRinexEph(*eph_ptr, rinexStream);
				}
				
				continue;
			}
		}
	}
}


void rinexNavHeader( 
	map<E_Sys, bool>&	sysMap, 
	Trace&				rinexStream)
{
	double rnxver	= 3.05;
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
		switch (sys)
		{
			case E_Sys::GPS:	ion_arr1 = &nav.ion_gps[0];		ion_str1 = "GPSA";
								ion_arr2 = &nav.ion_gps[4];		ion_str2 = "GPSB";	break;

			case E_Sys::GAL:	ion_arr1 = &nav.ion_gal[0];		ion_str1 = "GAL ";	break;
			
			case E_Sys::QZS:	ion_arr1 = &nav.ion_qzs[0];		ion_str1 = "QZSA";
								ion_arr2 = &nav.ion_gps[4];		ion_str2 = "QZSB";	break;
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

	for (auto& [sys, output] : sysMap)
	{
		if (output == false)
			continue;

		double*	tsys_arr = nullptr;
		string	tsys_str;
		
		switch (sys)
		{
			case E_Sys::GPS:	tsys_arr = nav.utc_gps;		tsys_str = "GPUT";			break;
			case E_Sys::GAL:	tsys_arr = nav.utc_gal;		tsys_str = "GAUT";			break;
			case E_Sys::QZS:	tsys_arr = nav.utc_qzs;		tsys_str = "QZUT";			break;
			case E_Sys::BDS:	tsys_arr = nav.utc_cmp;		tsys_str = "BDUT";			break;

			default:
				continue;
		}
		
		tracepdeex(0, rinexStream, "%s %17.10E%16.9E%7.0f%5.0f %-5s %-2s %-20s\n",
			tsys_str.c_str(),
			tsys_arr[0], 
			tsys_arr[1], 
			tsys_arr[2],
			tsys_arr[3], "", "", "TIME SYSTEM CORR");
	}
	
	tracepdeex(0, rinexStream, "%6d%54s%-20s\n", nav.leaps, "", 	"LEAP SECONDS");
	tracepdeex(0, rinexStream, "%60s%-20s\n", "",					"END OF HEADER");
}

void writeRinexNav()
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
			rinexNavHeader(sysMap, rinexStream);
		
		fileData.sysMap = sysMap;
		
		outputNavRinexBody(fileData, rinexStream);
	}
}
