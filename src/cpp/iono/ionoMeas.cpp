
// #pragma GCC optimize ("O0")

#include "observations.hpp"
#include "coordinates.hpp"
#include "corrections.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "satStat.hpp"
#include "station.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "enums.h"

#define PHASE_BIAS_STD 0.05

bool ippInRange(GTime time, VectorPos& Ion_pp)
{
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::MEAS_OUT:				return true;
		case E_IonoModel::SPHERICAL_HARMONICS:	return ippCheckSphhar(time,Ion_pp);
		case E_IonoModel::SPHERICAL_CAPS:		return ippCheckSphcap(time,Ion_pp);
		case E_IonoModel::BSPLINE:				return ippCheckBsplin(time,Ion_pp);
		case E_IonoModel::LOCAL:				return ippCheckLocal (time,Ion_pp);
		case E_IonoModel::NONE:					return false;
	}
	return false;
}

/*----------------------------------------------------------------------------------------------*/
/* Update station measurement data and geometric information  									*/
/* The following variables needs to be set to valid numbers in order for this function to work  */
/* properly:																					*/
/*	- rec.recpos					RECEIVER POSITION											*/
/*  - rec.obsList					OBSERVATIONS												*/
/*		- obs.exclude				excluded satellites											*/
/*		- obs.satStat_ptr->vs		satellite validity											*/
/*		- obs.satStat_ptr->el		elevation of satellite seen from station					*/
/*  	- obs.satStat_ptr->azel		alevation and azimut of satellite							*/
/*		- obs.satStat_ptr->lc_new	Current linear combinations									*/
/*		- obs.satStat_ptr->lc_pre	Previous linear combinations								*/
/*		- obs.rSat					Satellite position											*/
/*																								*/
/* The function will set the following variables within the observation structure:				*/
/*  - obs.ionExclude				Flag to indicate unsuitable measurments						*/
/*  - obs.STECtoDELAY				Factor between Slant TEC and Ionospheric delay for FREQ1	*/
/*  - obs.angIPP[j]					Inclination factor for layer "j"							*/
/*  - obs.latIPP[j]					Latitude of Piercing point for layer "j"					*/
/*  - obs.lonIPP[j]					Latitude of Piercing point for layer "j"					*/
/*  - obs.STECsmth					Smoothed Ionosphere measurement								*/
/*  - obs.STECsmvr					Variance of Smoothed Ionosphere measurement					*/

void update_receivr_measr(
	Trace&		trace, 
	Station&	rec)
{
	
	tracepdeex(4,trace,"\n---------------------- Ionospheric delay measurments -----------------------------\n");
	tracepdeex(4,trace,"ION_MEAS sat    tow     RawGF meas  RawGF std.  GF_code_mea  GF_phas_mea  GF to TECu\n");

	ObsList& obsList = rec.obsList;
	for (auto& obs : only<GObs>(obsList))
	if (obs.satNav_ptr)
	if (obs.satStat_ptr)
	{
		SatNav& 	satNav	= *obs.satNav_ptr;
		SatStat&	satStat	= *obs.satStat_ptr;
		
		obs.STECtype = 0;
		
		if	( satStat.el < acsConfig.elevation_mask
			||obs.exclude)
		{
			obs.ionExcludeElevation = 1;
			continue;
		}

		E_FType frq1;
		E_FType frq2;
		E_FType frq3;
		if (!satFreqs(obs.Sat.sys,frq1,frq2,frq3))
			continue;
		
		S_LC lc		= getLC(obs, obs.satStat_ptr->lc_new, frq1, frq2);
		S_LC lc_pre	= getLC(obs, obs.satStat_ptr->lc_pre, frq1, frq2);

		if (lc.valid == false)
		{
			obs.ionExcludeLC = 1;
			continue;
		}

		/* Setting STEC to Delay factor */
		obs.STECtoDELAY = STEC2DELAY * (satNav.lamMap[frq2] * satNav.lamMap[frq2] - satNav.lamMap[frq1] * satNav.lamMap[frq1]);

		/* setting ionospheric piercing point data */
		VectorPos pos = ecef2pos(rec.aprioriPos);
		
		VectorPos posp;
		
		for (int j = 0; j < acsConfig.ionModelOpts.layer_heights.size(); j++)
		{
			obs.ippMap[j].ang = ionppp(pos, satStat.azel, RE_WGS84 / 1000, acsConfig.ionModelOpts.layer_heights[j] / 1000, posp);
			
			tracepdeex(5,trace,"IPP_verif  %s   %8.3f %8.3f   %8.3f %8.3f ", obs.Sat.id().c_str(),
					pos. latDeg(), 
					pos. lonDeg(), 
					posp.latDeg(), 
					posp.lonDeg());
			
			if (ippInRange(obs.time, posp) == false)
			{
				obs.ionExcludeRange = 1;
				break;
			}
			
			obs.ippMap[j].lat = posp[0];
			obs.ippMap[j].lon = posp[1];
			tracepdeex(5,trace,"  %8.3f %8.3f\n", posp[0]*R2D, posp[1]*R2D);
		}
		
		if (obs.ionExclude) 
			continue;

		if (fabs((satStat.lastObsTime - obs.time).to_double()) > 300)
		{
			satStat.ambvar	= 0;
		}
		satStat.lastObsTime = obs.time;

		double varL = obs.Sigs.begin()->second.phasVar;
		double varP = obs.Sigs.begin()->second.codeVar;

		double amb = - (lc.GF_Phas_m + lc.GF_Code_m);
		double oldSTEC = satStat.prevSTEC;
		
		if	( fabs(lc.GF_Phas_m - lc_pre.GF_Phas_m) > 0.05		/* Basic cycle slip detection */
			||fabs(lc.GF_Phas_m - oldSTEC) > 0.05
			||satStat.ambvar <= 0)
		{
			satStat.gf_amb = amb;
			satStat.ambvar = varP;     /* 1.0001*varP; */
		}
		else
		{
			double SmtG = satStat.ambvar / (varP + satStat.ambvar);
			satStat.gf_amb	+= SmtG * (amb - satStat.gf_amb);
			satStat.ambvar	 = SmtG * (varP);
		}
		obs.STECtype = 1;
		obs.STECsmth = (satStat.gf_amb + lc.GF_Phas_m)/obs.STECtoDELAY;
		obs.STECsmvr =((satStat.ambvar + 2*varL) + SQR(PHASE_BIAS_STD))/obs.STECtoDELAY/obs.STECtoDELAY;
		obs.STECcodeComb = obs.Sigs[frq1].code._to_integral() * 100 + obs.Sigs[frq2].code._to_integral();
		
		satStat.prevSTEC = lc.GF_Phas_m;

		GTow	tow		= obs.time;
		tracepdeex(4,trace,"ION_MEAS %s %8.0f  %10.4f  %10.3e  %10.4f  %10.4f  %10.4f\n",
					obs.Sat.id().c_str(),
					tow,
					obs.STECsmth,
					obs.STECsmvr,
					obs.Sigs[frq2].P - obs.Sigs[frq1].P,
					obs.Sigs[frq1].L * satNav.lamMap[frq1] - obs.Sigs[frq2].L * satNav.lamMap[frq2],
					obs.STECtoDELAY);
	}
}

void writeReceiverMeasurements(
	Trace&					trace, 
	string					filename,
	map<string, Station>&	stations, 
	GTime					time)
{
	GWeek	week	= time;
	GTow	tow		= time;
	
	int nlayer=acsConfig.ionModelOpts.layer_heights.size();
	
	tracepdeex(2, trace, "Writing Ionosphere measurements %5d %12.3f  %4d %2d ", week, tow, stations.size(), nlayer);

	std::ofstream stecfile(filename, std::ofstream::app);

	tracepdeex(0,stecfile,"#TYP_MEA,week,       tow,site,sat,  ion_meas,   ion_var,s,l");

	if (nlayer<=0) 
		tracepdeex(0,stecfile,"  Sta. ECEF X    Sta. ECEF Y    Sta. ECEF Z    Sat. ECEF X    Sat. ECEF Y    Sat. ECEF Z\n");
	else 
	{
		for (int j=0; j<nlayer; j++) 
			tracepdeex(0,stecfile,",     hei,  ipplat,  ipplon, slant_f");
		
		tracepdeex(0,stecfile,"\n");
	}

	int i = 0;
	for (auto& [id, rec] : stations)
	{
		if (rec.obsList.size() < MIN_NSAT_STA) 
			continue;

		for	(auto& obs : only<GObs>(rec.obsList))
		{
			if (obs.ionExclude) 
				continue;
			
			if (!acsConfig.use_for_iono_model[obs.Sat.sys])
				continue;
			
			tracepdeex(0,stecfile,"IONO_MEA,%4d,%10.3f,%s,%s,%10.4f,%10.4e,%d,%d",week, tow,
				rec.id.c_str(), obs.Sat.id().c_str(), obs.STECsmth, obs.STECsmvr, obs.STECtype, nlayer);
			
			if (nlayer <= 0)
			{
				tracepdeex(0,stecfile,", %13.3f,%13.3f,%13.3f,%13.3f,%13.3f,%13.3f, %4d\n\n",
					rec.aprioriPos[0],
					rec.aprioriPos[1], 
					rec.aprioriPos[2],
					obs.rSat[0], 
					obs.rSat[1], 
					obs.rSat[2],
					obs.STECcodeComb);
			
				continue;
			}
			for (int j = 0; j < nlayer; j++)
			{
				tracepdeex(0,stecfile,",%8.0f,%8.3f,%8.3f,%8.3f", acsConfig.ionModelOpts.layer_heights[j]/1000,
					obs.ippMap[j].lat*R2D, 
					obs.ippMap[j].lon*R2D,
					obs.ippMap[j].ang);
			}
			tracepdeex(0, stecfile,"\n");
			i++;
		}
	}
	
	tracepdeex(0, stecfile,"#---------------------------------------------------------------------------------------------------------\n");
	tracepdeex(3, trace, "... %d entries written\n", i);
}

int  ginan2IonoMeas(
	Trace& trace,			///< debug trace
	StationMap& stationMap, ///< List of stations containing observations for this epoch
	KFState& measKFstate)	///< Kalman filter object containing the ionosphere estimates
{
	tracepdeex(3,trace,"\n Ginan 2.0 Ionosphere Function %s\n", measKFstate.time.to_string().c_str());
	
	int found = 0;
	
	for (auto& [id, rec] : stationMap)
	for (auto& obs	: only<GObs>(rec.obsList))
	{
		KFKey kfKey;
		kfKey.type	= KF::IONO_STEC;
		kfKey.str	= obs.mount;
		kfKey.Sat	= obs.Sat;
	
		double stecVal = 0;
		double stecVar = 0;
		bool pass = measKFstate.getKFValue(kfKey, stecVal, &stecVar);
		
		if (obs.ionExclude)
			continue;
		
		if (pass)
		{
			tracepdeex(4,trace,"    sTEC for %s %s found: %.4f -> %.4f\n",obs.Sat.id().c_str(), obs.mount.c_str(), obs.STECsmth, obs.STECtoDELAY*stecVal);
			obs.STECsmth = stecVal;
			obs.STECsmvr = stecVar;
			obs.STECtype = 3;
			found++;
		}
		else
		{
			obs.ionExclude = 1;
		}
		
	}
	return found;
}
