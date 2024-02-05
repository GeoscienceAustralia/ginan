
// #pragma GCC optimize ("O0")

#include "observations.hpp"
#include "coordinates.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "ionModels.hpp"
#include "receiver.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "enums.h"

#define PHASE_BIAS_STD 0.05
bool ionoConfigured = false;

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
	Receiver&	rec)
{
	if (ionoConfigured == false)
		ionoConfigured = configIonModel(trace);

	if (ionoConfigured == false)
		return;

	if (rec.aprioriPos.isZero())
		return;

	tracepdeex(4, trace, "\n---------------------- Ionospheric delay measurments -----------------------------\n");
	tracepdeex(4, trace, "ION_MEAS sat    tow     RawGF meas  RawGF std.  GF_code_mea  GF_phas_mea  GF to TECu\n");

	auto& recOpts = acsConfig.getRecOpts(rec.id);

	for (auto& obs : only<GObs>(rec.obsList))
	if (obs.satNav_ptr)
	if (obs.satStat_ptr)
	{
		SatNav& 	satNav	= *obs.satNav_ptr;
		SatStat&	satStat	= *obs.satStat_ptr;

		obs.stecVar  = SQR(1e5);

		if	( satStat.el < recOpts.elevation_mask_deg * D2R
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
			obs.ippMap[j].slantFactor = ionppp(pos, satStat, RE_WGS84 / 1000, acsConfig.ionModelOpts.layer_heights[j] / 1000, posp);

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

			obs.ippMap[j].latDeg = posp.latDeg();
			obs.ippMap[j].lonDeg = posp.lonDeg();
			tracepdeex(5,trace,"  %8.3f %8.3f\n", posp.latDeg(), posp.lonDeg());
		}

		if (obs.ionExclude)
			continue;

		if (fabs((satStat.lastObsTime - obs.time).to_double()) > 300)
		{
			satStat.ambvar	= 0;
		}
		satStat.lastObsTime = obs.time;

		double varL = obs.sigs.begin()->second.phasVar;
		double varP = obs.sigs.begin()->second.codeVar;

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
		obs.stecType = 1;		//todo aaron magic numbers
		obs.stecVal = (satStat.gf_amb + lc.GF_Phas_m)					/		obs.stecToDelay;
		obs.stecVar =((satStat.ambvar + 2*varL) + SQR(PHASE_BIAS_STD))	/ SQR(	obs.stecToDelay);

		obs.stecCodeCombo	= obs.sigs[frq1].code._to_integral() * 100
							+ obs.sigs[frq2].code._to_integral();

		satStat.prevSTEC = lc.GF_Phas_m;

		GTow	tow		= obs.time;
		tracepdeex(4,trace,"ION_MEAS %s %8.0f  %10.4f  %10.3e  %10.4f  %10.4f  %10.4f\n",
					obs.Sat.id().c_str(),
					tow,
					obs.stecVal,
					obs.stecVar,
					obs.sigs[frq2].P						- obs.sigs[frq1].P,
					obs.sigs[frq1].L * satNav.lamMap[frq1]	- obs.sigs[frq2].L * satNav.lamMap[frq2],
					obs.stecToDelay);
	}
}

void writeIonStec(
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

	tracepdeex(0, stecfile, "#TYP_MEA,%4s,%10s,%4s,%3s,%10s,%10s,%1s,%1s,%13s,%13s,%13s,%4s,%13s,%13s,%13s",
				"week",
				"tow",
				"site",
				"sat",
				"ion_meas",
				"ion_var",
				"s",
				"l",
				"Sat ECEF X",
				"Sat ECEF Y",
				"Sat ECEF Z",
				"com",
				"Rec ECEF X",
				"Rec ECEF Y",
				"Rec ECEF Z");

	int nlayer = acsConfig.ionModelOpts.layer_heights.size();

	for (int i = 0; i < nlayer; i++)
	{
		tracepdeex(0, stecfile,",%8s,%8s,%8s,%8s",
				"height",
				"ipplat",
				"ipplon",
				"slant_f");
	}

	tracepdeex(0, stecfile,"\n");

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::IONO_STEC)
			continue;

		double stecVal = 0;
		double stecVar = 0;

		bool pass = kfState.getKFValue(key, stecVal, &stecVar);

		if (pass == false)
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

		tracepdeex(0 ,stecfile, "IONO_MEA,%4d,%10.3f,%4s,%3s,%10.4f,%10.4e",
				week,
				tow,
				key.str.c_str(),
				key.Sat.id().c_str(),
				stecVal,
				stecVar);

		if (obs_ptr)
		{
			auto& obs = *obs_ptr;

			tracepdeex(0, stecfile,",%1d,%1d,%13.3f,%13.3f,%13.3f,%4d",
				obs.stecType,
				nlayer,
				obs.rSatCom[0],
				obs.rSatCom[1],
				obs.rSatCom[2],
				obs.stecCodeCombo);
		}
		else
		{
			tracepdeex(0, stecfile,",%1s,%1s,%13s,%13s,%13s,%4s",
				"",
				"",
				"",
				"",
				"",
				"");
		}

		if (key.rec_ptr)
		{
			auto& rec = *key.rec_ptr;

			tracepdeex(0, stecfile,",%13.3f,%13.3f,%13.3f",
				rec.aprioriPos[0],
				rec.aprioriPos[1],
				rec.aprioriPos[2]);
		}
		else
		{
			tracepdeex(0, stecfile,",%13s,%13s,%13s",
				"",
				"",
				"");
		}

		if (obs_ptr)
		for (int j = 0; j < nlayer; j++)
		{
			auto& obs = *obs_ptr;

			tracepdeex(0, stecfile, ",%8.0f,%8.3f,%8.3f,%8.3f",
				acsConfig.ionModelOpts.layer_heights[j] / 1000,
				obs.ippMap[j].latDeg,
				obs.ippMap[j].lonDeg,
				obs.ippMap[j].slantFactor);
		}

		tracepdeex(0, stecfile, "\n");
	}
}

void obsIonoDataFromFilter(
	Trace&			trace,			///< debug trace
	ReceiverMap&	receiverMap, 	///< List of stations containing observations for this epoch
	KFState&		measKFstate)	///< Kalman filter object containing the ionosphere estimates
{
	tracepdeex(3, trace,"\n%s %s\n", __FUNCTION__, measKFstate.time.to_string().c_str());

	for (auto& [id, rec] : receiverMap)
	for (auto& obs	: only<GObs>(rec.obsList))
	{
		if (obs.ionExclude)
			continue;

		KFKey kfKey;
		kfKey.type	= KF::IONO_STEC;
		kfKey.str	= obs.mount;
		kfKey.Sat	= obs.Sat;

		double stecVal = 0;
		double stecVar = 0;
		bool pass = measKFstate.getKFValue(kfKey, stecVal, &stecVar);

		if (pass == false)
		{
			obs.ionExclude = 1;
			continue;
		}

		tracepdeex(4, trace, "    sTEC for %s %s found: %.4f -> %.4f\n", obs.Sat.id().c_str(), obs.mount.c_str(), obs.stecVal, stecVal);
		obs.stecVal		= stecVal;
		obs.stecVar		= stecVar;
		obs.stecType	= 3;
	}
}
