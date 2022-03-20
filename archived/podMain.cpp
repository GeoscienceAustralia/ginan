#include "forceModels.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "station.hpp"
#include "jplEph.hpp"

#include <iostream>
#include <fstream>

void podMain(
	Network&	net,
	Station&	pseudoRec)
{
	/* initial condition for differential equation*/
	/* Option 1: ECEF ICs from POD */
	// // Initial value in ECEF, to be transformed to ECI
	// Vector6d rvECEF;
	// rvECEF = {-12053.996853e3,  21412.163227e3,  -9849.315798e3, -1032.8190896,   707.2236495,  2889.2728872};

	// double erpv[4] = {};
	// geterp(&nav.erp, tsync, erpv);
	
	// double mjdUTC = gpst2mjd(tsync);
	// double leapSec	= nav.leaps;
	// double dUT1_UTC = erpv[2];
	// double dUTC_TAI	= -(19 + leapSec);
	// double xp = erpv[0];
	// double yp = erpv[1];
	// double lod = erpv[3];
	// clIERS iersInstance;
	// iersInstance.Set(dUT1_UTC, dUTC_TAI, xp, yp, lod);
	// double mjdTT = mjdUTC + iersInstance.TT_UTC(mjdUTC) / 86400;

	// Matrix3d mECI2ECEF = Matrix3d::Identity();
	// Matrix3d mdECI2ECEF = Matrix3d::Identity();
	// Vector6d rvECI;
	// ecef2eciVec_sofa(mjdUTC, iersInstance, rvECEF, rvECI);	

	/* Option 2: ECI ICs from POD */
	// Initial value in ECI
	// Vector6d rvECI = {14226165.4977822, 20021922.7850642, -9875597.15080248, -1254.27669098652, 2274.30031195604, 2891.66233001166};

	/* Option 3: ECEF position from SP3, ECEF velocity from trigonometric interpolation */
	// // allocate memory for an array of the character string
	// char* source_eph = new char[acsConfig.sp3files[0].size()];
	// // for the first string, allocate memory in the character array and copy
	// strncpy(source_eph, acsConfig.sp3files[0].c_str(), acsConfig.sp3files[0].size());
	
	// char* control_eph = source_eph;
	// int num_data	= NUM_DAT_ORB_INTP;
	// int num_terms	= NUM_ORD_ORB_INTP;
	// int prn = pseudoRec.pseudoObsList.front().Sat.prn;
	// char which_strategy = '2';
	// double tinc = 1; //time increment, seconds
	// double ep[6] = {0};
	// time2epoch(tsync - tinc, ep);
	// double mjdTb1t	= ymdhms2jd(ep) - JD2MJD;
	// Vector3d posTb1t = trigPosInterp(source_eph, mjdTb1t, num_data, num_terms, prn, which_strategy); //position before the epoch
	// time2epoch(tsync + tinc, ep);
	// double mjdTa1t	= ymdhms2jd(ep) - JD2MJD;
	// Vector3d posTa1t = trigPosInterp(source_eph, mjdTa1t, num_data, num_terms, prn, which_strategy); //position after the epoch					
	// // time2epoch(tsync - 2 * tinc, ep);
	// // double mjdTb2t	= ymdhms2jd(ep) - JD2MJD;
	// // Vector3d posTb2t = trigPosInterp(source_eph, mjdTb2t, num_data, num_terms, prn, which_strategy); //position before the epoch
	// // time2epoch(tsync + 2 * tinc, ep);
	// // double mjdTa2t	= ymdhms2jd(ep) - JD2MJD;
	// // Vector3d posTa2t = trigPosInterp(source_eph, mjdTa2t, num_data, num_terms, prn, which_strategy); //position after the epoch					
	// Vector3d velT0 = (posTa1t - posTb1t) / (2 * tinc); //second-order central difference to obtain the velocity components
	// // Vector3d velT02 = (-posTa2t + 8 * posTa1t - 8 * posTb1t + posTb2t) / (12 * tinc); //fourth-order central difference to obtain the velocity components

	// Vector6d rvECEF;
	// rvECEF.head(3) = pseudoRec.pseudoObsList.front().pos;
	// rvECEF.tail(3) = velT0 * 1000;
	// std::cout 
	// << "Calculated velocity components at the first epoch:" << std::endl
	// << std::setprecision(10) << rvECEF.tail(3).transpose() << std::endl;
	
	// double erpv[4] = {};
	// geterp(&nav.erp, tsync, erpv);
	
	double mjdUTC	= gpst2mjd(tsync);
	double leapSec	= nav.leaps;
	// double dUTC_TAI	= -(19 + leapSec);
	// double xp		= erpv[0];
	// double yp		= erpv[1];
	// double dUT1_UTC	= erpv[2];
	// double lod		= erpv[3];
	// IERS iersInstance;
	// iersInstance.Set(dUT1_UTC, dUTC_TAI, xp, yp, lod);

	// Matrix3d mECI2ECEF	= Matrix3d::Identity();
	// Matrix3d mdECI2ECEF	= Matrix3d::Identity();
	
	// Vector6d rvECI;
	// ecef2eciVec_sofa(mjdUTC, iersInstance, rvECEF, rvECI);	
	// std::cout << "ECI coordinates at the first epoch:" << std::endl
	// 		  << std::setprecision(10) << rvECI.transpose() << std::endl;



	/* initial condition for variational equation*/

	Vector6d rvECI = Vector6d::Zero();

	SatSys Sat = SatSys(E_Sys::GPS, 1);
	
	auto& orbitPropagator	= orbitPropagatorMap[Sat];
	
    for (auto& [satId, orbitPropagator] : orbitPropagatorMap)
	{
		orbitPropagator.setPropOption(acsConfig.forceModels);
		orbitPropagator.init(rvECI, mjdUTC, leapSec, &nav.erp, nav.egm);
	}

	std::ofstream fileOPResults("./ex01/OPResults.txt");
	if (!fileOPResults)
	{
		std::cout << "Error openinng results file!\n";
		return;
	}
	
	fileOPResults << "MJD(UTC)			x(ECEF)			y(ECEF)			z(ECEF)			vx(ECEF)			vy(ECEF)			vz(ECEF)\n";
}
