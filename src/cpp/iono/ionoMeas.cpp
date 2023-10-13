
// #pragma GCC optimize ("O0")

#include "observations.hpp"
#include "coordinates.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "ionModels.hpp"
#include "satStat.hpp"
#include "station.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "enums.h"

#define PHASE_BIAS_STD 0.05

bool ippInRange(
	GTime		time, 
	VectorPos&	ionPP)
{
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::MEAS_OUT:				return true;
		case E_IonoModel::SPHERICAL_HARMONICS:	return ippCheckSphhar(time, ionPP);
		case E_IonoModel::SPHERICAL_CAPS:		return ippCheckSphcap(time, ionPP);
		case E_IonoModel::BSPLINE:				return ippCheckBsplin(time, ionPP);
		case E_IonoModel::LOCAL:				return ippCheckLocal (time, ionPP);
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

void obsIonoData(
	Trace&		trace, 
	Station&	rec)
{
	if (rec.aprioriPos.isZero())
		return;
	
	tracepdeex(4,trace,"\n---------------------- Ionospheric delay measurments -----------------------------\n");
	tracepdeex(4,trace,"ION_MEAS sat    tow     RawGF meas  RawGF std.  GF_code_mea  GF_phas_mea  GF to TECu\n");

	for (auto& obs : only<GObs>(rec.obsList))
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
		if (!satFreqs(obs.Sat.sys, frq1, frq2, frq3))
			continue;
		
		S_LC lc		= getLC(obs, obs.satStat_ptr->lc_new, frq1, frq2);
		S_LC lc_pre	= getLC(obs, obs.satStat_ptr->lc_pre, frq1, frq2);

		if (lc.valid == false)
		{
			obs.ionExcludeLC = 1;
			continue;
		}

		/* Setting STEC to Delay factor */
		obs.stecToDelay = STEC2DELAY * (SQR(satNav.lamMap[frq2]) - SQR(satNav.lamMap[frq1]));

		/* setting ionospheric piercing point data */
		VectorPos pos = ecef2pos(rec.aprioriPos);
		
		VectorPos posp;
		
		for (int j = 0; j < acsConfig.ionModelOpts.layer_heights.size(); j++)
		{
			obs.ippMap[j].slantFactor = ionppp(pos, satStat.azel, RE_WGS84 / 1000, acsConfig.ionModelOpts.layer_heights[j] / 1000, posp);
			
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
			tracepdeex(5,trace,"  %8.3f %8.3f\n", posp[0] * R2D, posp[1] * R2D);
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
		
		if	( fabs(lc.GF_Phas_m - lc_pre.GF_Phas_m)	> 0.05		/* Basic cycle slip detection */
			||fabs(lc.GF_Phas_m - oldSTEC)			> 0.05
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
		obs.STECtype = 1;		//todo aaron magic numbers
		obs.stecVal = (satStat.gf_amb + lc.GF_Phas_m)					/		obs.stecToDelay;
		obs.stecVar =((satStat.ambvar + 2*varL) + SQR(PHASE_BIAS_STD))	/ SQR(	obs.stecToDelay);
		
		obs.stecCodeCombo	= obs.Sigs[frq1].code._to_integral() * 100
							+ obs.Sigs[frq2].code._to_integral();
		
		satStat.prevSTEC = lc.GF_Phas_m;

		GTow	tow		= obs.time;
		tracepdeex(4,trace,"ION_MEAS %s %8.0f  %10.4f  %10.3e  %10.4f  %10.4f  %10.4f\n",
					obs.Sat.id().c_str(),
					tow,
					obs.stecVal,
					obs.stecVar,
					obs.Sigs[frq2].P						- obs.Sigs[frq1].P,
					obs.Sigs[frq1].L * satNav.lamMap[frq1]	- obs.Sigs[frq2].L * satNav.lamMap[frq2],
					obs.stecToDelay);
	}
}

void writeSTECfromRTS(		//todo aaron why is this special?
	string			filename,
	KFState&		kfState)
{
	GWeek	week	= kfState.time;
	GTow	tow		= kfState.time;
	
	std::ofstream stecfile(filename, std::ofstream::app);
	if (!stecfile)
	{
		return;
	}
	
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::IONO_STEC)
			continue;
	
		double stecVal = 0;
		double stecVar = 0;
		
		bool pass = kfState.getKFValue(key, stecVal, &stecVar);
		
		if (pass == false)
			continue;
		
		if (stecVar > SQR(acsConfig.ionoOpts.iono_sigma_limit))
			continue;
		
		GObs* obs_ptr = nullptr;
		
		if (key.rec_ptr)
		{
			auto& rec = *key.rec_ptr;
			
			for	(auto& obs : only<GObs>(rec.obsList))
			{
				if (key.Sat != obs.Sat)
				{
					continue;
				}
				
				//found the observation for this ionosphere
				obs_ptr = &obs;
			}
		}
		
		tracepdeex(0 ,stecfile, "IONO_MEA,%4d,%10.3f,%s,%s,%10.4f,%10.4e,4",
				week, 
				tow,
				key.str.c_str(), 
				key.Sat.id().c_str(), 
				stecVal, 
				stecVar);
		
		if (obs_ptr)
		{
			//use obs things here
		}
		
		if (key.rec_ptr)
		{
			auto& rec = *key.rec_ptr;
			VectorPos pos = ecef2pos(rec.aprioriPos);
			
			tracepdeex(0,stecfile, ",1,0,%8.4f,%8.4f,1.0",
				pos.latDeg(), 
				pos.lonDeg());
		}
		
		tracepdeex(0, stecfile, "\n");
	}
}

void writeIONStec(
	Trace&						trace, 
	string						filename,
	map<string, Station>&		stations, 
	GTime						time)
{
	GWeek	week	= time;
	GTow	tow		= time;
	
	int nlayer = acsConfig.ionModelOpts.layer_heights.size();
	
	tracepdeex(2, trace, "Writing Ionosphere measurements %5d %12.3f  %4d %2d ", week, tow, stations.size(), nlayer);

	std::ofstream stecfile(filename, std::ofstream::app);
	if (!stecfile)
	{
		return;
	}

	tracepdeex(0, stecfile, "#TYP_MEA,week,       tow,site,sat,  ion_meas,   ion_var,s,l");

	if (nlayer <= 0) 
	{
		tracepdeex(0, stecfile,"  Sta. ECEF X    Sta. ECEF Y    Sta. ECEF Z    Sat. ECEF X    Sat. ECEF Y    Sat. ECEF Z\n");
	}
	else 
	{
		for (int j=  0; j < nlayer; j++) 
			tracepdeex(0, stecfile,",     hei,  ipplat,  ipplon, slant_f");
		
		tracepdeex(0, stecfile,"\n");
	}

	int i = 0;
	for (auto& [id, rec]	: stations)
	for	(auto& obs			: only<GObs>(rec.obsList))
	{
		if (rec.obsList.size() < MIN_NSAT_STA) 
			continue;

		if (obs.ionExclude) 
			continue;
		
		if (acsConfig.use_for_iono_model[obs.Sat.sys] == false)
			continue;
		
		tracepdeex(0,stecfile,"IONO_MEA,%4d,%10.3f,%s,%s,%10.4f,%10.4e,%d,%d",
			week, 
			tow,
			rec.id.c_str(), 
			obs.Sat.id().c_str(), 
			obs.stecVal, 
			obs.stecVar, 
			obs.STECtype, 
			nlayer);
		
		if (nlayer <= 0)
		{
			tracepdeex(0,stecfile,", %13.3f,%13.3f,%13.3f,%13.3f,%13.3f,%13.3f, %4d\n\n",
				rec.aprioriPos[0],
				rec.aprioriPos[1], 
				rec.aprioriPos[2],
				obs.rSatCom[0], 
				obs.rSatCom[1], 
				obs.rSatCom[2],
				obs.stecCodeCombo);
		
			continue;
		}
		
		for (int j = 0; j < nlayer; j++)
		{
			tracepdeex(0, stecfile, ",%8.0f,%8.3f,%8.3f,%8.3f",
				acsConfig.ionModelOpts.layer_heights[j] / 1000,
				obs.ippMap[j].lat * R2D, 
				obs.ippMap[j].lon * R2D,
				obs.ippMap[j].slantFactor);
		}
		
		tracepdeex(0, stecfile, "\n");
		i++;
	}
	
	tracepdeex(0, stecfile,"#---------------------------------------------------------------------------------------------------------\n");
	tracepdeex(3, trace, "... %d entries written\n", i);
}

void  obsIonoDataFromFilter(
	Trace&			trace,			///< debug trace
	StationMap&		stationMap, 	///< List of stations containing observations for this epoch
	KFState&		measKFstate)	///< Kalman filter object containing the ionosphere estimates
{
	tracepdeex(3, trace,"\n%s %s\n", __FUNCTION__, measKFstate.time.to_string().c_str());
	
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
		
		if	(  pass
			&& stecVar < SQR(acsConfig.ionoOpts.iono_sigma_limit))
		{
			tracepdeex(4, trace, "    sTEC for %s %s found: %.4f -> %.4f\n", obs.Sat.id().c_str(), obs.mount.c_str(), obs.stecVal, obs.stecToDelay * stecVal);
			obs.stecVal = stecVal;
			obs.stecVar = stecVar;
			obs.STECtype = 3;
		}
		else
		{
			obs.ionExclude = 1;
		}
	}
}
