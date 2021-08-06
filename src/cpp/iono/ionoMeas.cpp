
#include "observations.hpp"
#include "streamTrace.hpp"
#include "corrections.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "constants.h"
#include "satStat.hpp"
#include "station.hpp"
#include "common.hpp"
#include "enums.h"

#define PHASE_BIAS_STD 0.05

extern int Ipp_in_range(GTime time, double *Ion_pp)
{
	switch (acsConfig.ionFilterOpts.model)
	{
		case E_IonoModel::MEAS_OUT:				return 1;
		case E_IonoModel::SPHERICAL_HARMONICS:	return Ipp_check_sphhar(time,Ion_pp);
		case E_IonoModel::SPHERICAL_CAPS:		return Ipp_check_sphcap(time,Ion_pp);
		case E_IonoModel::BSPLINE:				return Ipp_check_bsplin(time,Ion_pp);
	}
	return 0;
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

int update_receivr_measr(
	Trace&		trace, 
	Station&	rec)
{
	E_FType f2 = F2;

	tracepde(4,trace,"\n---------------------- Ionospheric delay measurments -----------------------------\n");
	tracepde(4,trace,"ION_MEAS sat    tow     RawGF meas  RawGF std.  GF_code_mea  GF_phas_mea  GF to TECu\n");

	ObsList& obsList = rec.obsList;
	for (auto& obs : obsList)
	{
		SatNav& 	satNav	= *obs.satNav_ptr;
		SatStat&	satStat	= *obs.satStat_ptr;
		string satidstr = obs.Sat.id();
		obs.STECtype=0;
		
		if	( (satStat.el < acsConfig.elevation_mask)
			||(obs.exclude))
		{
			obs.ionExclude = 1;
			continue;
		}

		if( obs.Sat.sys == +E_Sys::GAL ) f2=F5; 

		S_LC lc		= getLC(obs, obs.satStat_ptr->lc_new, L1, f2);
		S_LC lc_pre	= getLC(obs, obs.satStat_ptr->lc_pre, L1, f2);

		if (lc.valid == false)
		{
			obs.ionExclude = 1;
			continue;
		}

		/* Setting STEC to Delay factor */
		obs.STECtoDELAY = STEC2DELAY * (satNav.lamMap[f2] * satNav.lamMap[f2] - satNav.lamMap[F1] * satNav.lamMap[F1]);

		/* setting ionospheric piercing point data */
		double pos[3];
		double posp[3] = {0};
		ecef2pos(rec.aprioriPos, pos);
		for (int j = 0; j < acsConfig.ionFilterOpts.layer_heights.size(); j++)
		{
			obs.angIPP[j] = ionppp(pos, satStat.azel, RE_WGS84/1000.0, acsConfig.ionFilterOpts.layer_heights[j]/1000.0, posp);
			tracepde(5,trace,"IPP_verif  %s   %8.3f %8.3f   %8.3f %8.3f ", satidstr.c_str(),
					pos[0]*R2D, 
					pos[1]*R2D, 
					posp[0]*R2D, 
					posp[1]*R2D);
			
			if(!Ipp_in_range(obs.time,posp))
			{
				obs.ionExclude =1;
				break;
			}
			
			obs.latIPP[j] = posp[0];
			obs.lonIPP[j] = posp[1];
			tracepde(5,trace,"  %8.3f %8.3f\n", posp[0]*R2D, posp[1]*R2D);
		}
		
		if (obs.ionExclude) 
			continue;

		if (fabs(timediff(satStat.lastObsTime, obs.time)) > 300)
		{
			satStat.ambvar	= 0;
		}
		satStat.lastObsTime = obs.time;

		double varL = obs.Sigs.begin()->second.phasVar;
		double varP = obs.Sigs.begin()->second.codeVar;

		double amb = - (lc.GF_Phas_m + lc.GF_Code_m);	//todo aaron, the signs of these come out a bit weird

		if	( fabs(lc.GF_Phas_m - lc_pre.GF_Phas_m) > 0.05		/* Basic cycle slip detection */
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
		obs.STECsmth = (satStat.gf_amb + lc.GF_Phas_m);
		obs.STECsmvr = (satStat.ambvar + 2*varL) + SQR(PHASE_BIAS_STD);


		int obstweek;
		double obstsec = time2gpst(obs.time, &obstweek);
		tracepde(4,trace,"ION_MEAS %s %8.0f  %10.4f  %10.3e  %10.4f  %10.4f  %10.4f\n",
					satidstr.c_str(),
					obstsec,
					obs.STECsmth,
					obs.STECsmvr,
					obs.Sigs[f2].P - obs.Sigs[L1].P,
					obs.Sigs[L1].L * satNav.lamMap[L1] - obs.Sigs[f2].L * satNav.lamMap[f2],
					obs.STECtoDELAY);
	}
	
	return 1;
}

void write_receivr_measr(
	Trace&				trace, 
	std::list<Station*>	stations, 
	GTime				time)
{
	int week;
	double tow = time2gpst(time, &week);
	int nlayer=acsConfig.ionFilterOpts.layer_heights.size();
	
	tracepdeex(2, trace, "Writing Ionosphere measurements %5d %12.3f  %4d %2d ", week, tow, stations.size(), nlayer);

	std::ofstream stecfile(acsConfig.ionstec_filename, std::ofstream::app);

	tracepdeex(2,stecfile,"\n#IONO_MEAS  week       tow        sta  sat  Iono. meas  Iono. var.  state  # layers");

	if (nlayer<=0) 
		tracepdeex(2,stecfile,"  Sta. ECEF X    Sta. ECEF Y    Sta. ECEF Z    Sat. ECEF X    Sat. ECEF Y    Sat. ECEF Z\n");
	else 
	{
		for (int j=0; j<nlayer; j++) 
			tracepdeex(2,stecfile,"   height   IPP lat.  IPP lon.  Slant F.");
		
		tracepdeex(2,stecfile,"\n");
	}

	int i = 0;
	for (auto& rec_ptr : stations)
	{
		auto& rec = *rec_ptr;
		//Trace& rectrc = *rec.trace.get();
		
		if (rec.obsList.size() < MIN_NSAT_STA) 
			continue;

		for	(auto& obs : rec.obsList)
		{
			if (obs.ionExclude) 
				continue;
			
			tracepdeex(2,stecfile,"#IONO_MEA, %5d, %12.3f, %s, %s, %10.4f, %10.4e,   %d,     %2d   ",week, tow, 
				rec.id, obs.Sat.id().c_str(), obs.STECsmth, obs.STECsmvr, obs.STECtype, nlayer);
			
			if (nlayer<=0)
			{
				tracepdeex(2,stecfile,", %13.3f, %13.3f, %13.3f, %13.3f, %13.3f, %13.3f\n",
					rec.aprioriPos[0],
					rec.aprioriPos[1], 
					rec.aprioriPos[2],
					obs.rSat[0], 
					obs.rSat[1], 
					obs.rSat[2]);
			
				continue;
			}
			for (int j=0; j<nlayer; j++)
			{
				tracepdeex(2,stecfile,", %8.0f, %8.2f, %8.3f, %8.3f", acsConfig.ionFilterOpts.layer_heights[j]/1000,
					obs.latIPP[j]*R2D, 
					obs.lonIPP[j]*R2D,
					obs.angIPP[j]);
			}
			tracepdeex(2, stecfile,"\n");
			i++;
		}
	}
	tracepdeex(2, stecfile,"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n");
	tracepdeex(2, trace, "... %d entries written\n", i);
}



