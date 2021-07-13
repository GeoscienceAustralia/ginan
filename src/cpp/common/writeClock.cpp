
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

#include "streamTrace.hpp"
#include "acsConfig.hpp"
#include "constants.h"
#include "station.hpp"
#include "algebra.hpp"
#include "enums.h"
#include "GNSSambres.hpp"


/* macro defintions */
#define             VERSION             3.00

/** Output receiver clocks
*/
void outputReceiverClocks(
	string&		filename,	///< Path of file to output to
	KFState&	kfState,	///< Kalman filter to pull clocks from
	double*		epoch)		///< Epoch time
{
	std::ofstream clockFile(filename, std::ofstream::app);

	/* output receiver clock value */
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	( key.type	== KF::REC_SYS_BIAS
			&&key.num	== SatSys(E_Sys::GPS).biasGroup())
		{
			KFKey rkey = key;
			double rclk		= 0;
			double stddev	= 0;
			kfState.getKFValue(rkey, rclk, &stddev);

			/* clock value */
			tracepdeex(0, clockFile, "AR %.4s %4d%3d%3d%3d%3d%10.6f%3d   %19.12E %19.12E\n",
						key.str.c_str(),
						(int)	epoch[0],
						(int)	epoch[1],
						(int)	epoch[2],
						(int)	epoch[3],
						(int)	epoch[4],
								epoch[5],
						1,
						rclk/CLIGHT,
						stddev);
		}
	}
	clockFile.flush();
	return;
}

/** Output satellite clocks
*/
void outputSatelliteClocks(
	string&		filename,	///< Path of file to output to
	KFState&	kfState,	///< Kalman filter to pull clocks from
	double*		epoch)		///< Epoch time
{
	std::ofstream clockFile(filename, std::ofstream::app);

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type == KF::SAT_CLOCK)
		{
			KFKey skey = key;
			char id[5];
			key.Sat.getId(id);
			double clk		= 0;
			double sigma	= 0;
			kfState.getKFValue(skey, clk, &sigma);

			tracepdeex(0, clockFile,"AS %-4s %4d%3d%3d%3d%3d%10.6f%3d   %19.12E %19.12E\n",
					id,
					(int)epoch[0],
					(int)epoch[1],
					(int)epoch[2],
					(int)epoch[3],
					(int)epoch[4],
						epoch[5],
					2,
					clk/CLIGHT,
					sigma);
			
			if (acsConfig.ssrOpts.calculate_ssr
			&& (!acsConfig.output_AR_clocks || ARsol_ready())) // Always records non-AR clocks; records AR-clocks if AR solution is stable
			{
				nav.satNavMap[key.Sat].ssrOut.ssrClk.precise = clk; // units: m
				nav.satNavMap[key.Sat].ssrOut.ssrClk.isPreciseSet = true;
			}
		}
	}
	clockFile.flush();
	return;
}

/** Output clock file header if necessary
*/
void outputClockfileHeader(
	string&		filename,	///< Path of tile to output to
	KFState&	kfState,	///< Kalman filter to pull clocks from
	double*		epoch)		///< Epoch time
{
	std::ofstream clockFile(filename, std::ofstream::app);
	
	auto pos = clockFile.tellp();
	if (pos != 0)
	{
		return;
	}

	/* determine satellite system type */
	char syschar		= 0;
	int firstBiasGroup	= 0;
	int num_sats		= 0;
	for (int i = 0; i < E_Sys::NUM_SYS; i++)
	{
		if (acsConfig.process_sys[i])
		{
			SatSys Sat = SatSys(E_Sys::_from_integral(i));
			if (syschar == 0)	{	syschar = Sat.sysChar();	}
			else				{	syschar = 'M';				}

			if (firstBiasGroup == 0)
			{
				firstBiasGroup = Sat.biasGroup();
			}

			switch (i)
			{
				case E_Sys::GPS:	num_sats += NSATGPS;	break;
				case E_Sys::GLO:	num_sats += NSATGLO;	break;
				case E_Sys::GAL:	num_sats += NSATGAL;	break;
				case E_Sys::CMP:	num_sats += NSATCMP;	break;
			}
		}
	}

	KFKey refKey = {};
	int num_recs = 0;
	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		if	( (kfKey.type == KF::REC_SYS_BIAS	&&	kfKey.num == firstBiasGroup)
			||(kfKey.type == KF::REF_SYS_BIAS	&&	kfKey.num == firstBiasGroup))
		{
			num_recs++;
		}

		if	(kfKey.type == KF::REF_SYS_BIAS)
		{
			refKey = kfKey;
		}
	}

	if (refKey.str.empty())
	{
		BOOST_LOG_TRIVIAL(error) << "error, should have found reference receiver";
// 		return;
	}


	tracepdeex(0, clockFile, "%9.2f           C                   %c                   %s\n", VERSION, syschar,	"RINEX VERSION / TYPE");
	tracepdeex(0, clockFile, "%-20s%-20s%4d%02d%02d %02d%02d%02d %4s%s\n", acsConfig.analysis_program.c_str(),acsConfig.analysis_agency.c_str(),(int)epoch[0],(int)epoch[1],(int)epoch[2],(int)epoch[3],(int)epoch[4],(int)epoch[5],"LCL","PGM / RUN BY / DATE");
	tracepdeex(0, clockFile, "%-60s%s\n","", 																	"SYS / # / OBS TYPES");
	tracepdeex(0, clockFile, "%-60s%s\n","",																	"TIME SYSTEM ID");
	tracepdeex(0, clockFile, "%6d    %2s    %2s%-42s%s\n",            2,"AS","AR","",							"# / TYPES OF DATA");
	tracepdeex(0, clockFile, "%-60s%s\n","",																	"STATION NAME / NUM");
	tracepdeex(0, clockFile, "%-60s%s\n","",																	"STATION CLK REF");
	tracepdeex(0, clockFile, "%-3s  %-55s%s\n", acsConfig.analysis_agency.c_str(), acsConfig.analysis_center.c_str(),			"ANALYSIS CENTER");
	tracepdeex(0, clockFile, "%6d%54s%s\n",1,"",																"# OF CLK REF");
	tracepdeex(0, clockFile, "%-4s %-20s%35s%s\n", "", refKey.str.substr(0, 4).c_str(),"",						"ANALYSIS CLK REF");
	tracepdeex(0, clockFile, "%6d    %-50s%s\n", num_recs,"IGS14",												"# OF SOLN STA / TRF");
	// MM This line causes the clock combination software to crash to removing
	//tracepdeex(0, clockFile, "%-60s%s\n",acsConfig.rinex_comment,												"COMMENT");

	/* output receiver prn and coordinates */
	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		if	(   kfKey.num	!= firstBiasGroup
			||( kfKey.type != KF::REC_SYS_BIAS
			&&kfKey.type != KF::REF_SYS_BIAS))
		{
			continue;
		}

		if (kfKey.station_ptr)
		{
			tracepdeex(0, clockFile, "%-4s ", kfKey.station_ptr->id			.substr(0, 4).c_str());
			tracepdeex(0, clockFile, "%-20s", kfKey.station_ptr->snx.monuid	.substr(0,20).c_str());
			tracepdeex(0, clockFile, "%11.0f %11.0f %11.0f%s\n",
					kfKey.station_ptr->snx.pos(0) * 1000,
					kfKey.station_ptr->snx.pos(1) * 1000,
					kfKey.station_ptr->snx.pos(2) * 1000,
					"SOLN STA NAME / NUM");
		}
	}

	/* output satellite PRN*/
	int k = 0;
	tracepdeex(0, clockFile, "%6d%54s%s\n",num_sats,"","# OF SOLN SATS");
	if (acsConfig.process_sys[E_Sys::GPS])	for (int prn = 1; prn <= NSATGPS; prn++)	{k++;	SatSys s(E_Sys::GPS, prn);	tracepdeex(0, clockFile, "%3s ",	s.id().c_str());	if (k % 15 == 0) tracepdeex(0, clockFile, "%s\n","PRN LIST");}
	if (acsConfig.process_sys[E_Sys::GLO])	for (int prn = 1; prn <= NSATGLO; prn++)	{k++;	SatSys s(E_Sys::GLO, prn);	tracepdeex(0, clockFile, "%3s ",	s.id().c_str());	if (k % 15 == 0) tracepdeex(0, clockFile, "%s\n","PRN LIST");}
	if (acsConfig.process_sys[E_Sys::GAL])	for (int prn = 1; prn <= NSATGAL; prn++)	{k++;	SatSys s(E_Sys::GAL, prn);	tracepdeex(0, clockFile, "%3s ",	s.id().c_str());	if (k % 15 == 0) tracepdeex(0, clockFile, "%s\n","PRN LIST");}
	if (acsConfig.process_sys[E_Sys::CMP])	for (int prn = 1; prn <= NSATCMP; prn++)	{k++;	SatSys s(E_Sys::CMP, prn);	tracepdeex(0, clockFile, "%3s ",	s.id().c_str());	if (k % 15 == 0) tracepdeex(0, clockFile, "%s\n","PRN LIST");}
	/*finish the line*/						while (k % 15 != 0)						{k++;								tracepdeex(0, clockFile, "    ");						if (k % 15 == 0) tracepdeex(0, clockFile, "%s\n","PRN LIST");}


	tracepdeex(0, clockFile, "%-60s%s\n","","END OF HEADER");

	return;
}
