
#include <map>
#include <string>

using std::string;
using std::map;

#include "ionoModel.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "enums.h"


/* Global parameters */
ionomodel_t iono_modl;
KFState iono_KFState;
map<E_Sys,string>  IonRefSta;

#define		INIT_VAR_RDCB	100.0
#define		INIT_VAR_SDCB	100.0
#define		INIT_VAR_SCHP	100.0

FILE* fp_iondebug;

extern int config_ionosph_model()
{
	fp_iondebug = nullptr;
	
	if (acsConfig.output_ionstec) std::ofstream(acsConfig.ionstec_filename);
	if (acsConfig.output_ionex)   std::ofstream(acsConfig.ionex_filename);
	
	iono_KFState.max_filter_iter	= acsConfig.ionFilterOpts.max_filter_iter;
	iono_KFState.max_prefit_remv	= acsConfig.ionFilterOpts.max_prefit_remv;
	iono_KFState.inverter			= acsConfig.ionFilterOpts.inverter;
		
	//fp_iondebug = fopen("iono_debug_trace.txt", "w");
	switch (acsConfig.ionFilterOpts.model)
	{
		case E_IonoModel::MEAS_OUT:				return 1;
		case E_IonoModel::SPHERICAL_HARMONICS:	return configure_iono_model_sphhar();
		case E_IonoModel::SPHERICAL_CAPS:	    return configure_iono_model_sphcap();
		case E_IonoModel::BSPLINE:			    return configure_iono_model_bsplin();
	}
	return 0;
}

static double ion_coef(int ind, Obs& obs, bool slant)
{
	switch(acsConfig.ionFilterOpts.model)
	{
		case E_IonoModel::SPHERICAL_HARMONICS:  return ion_coef_sphhar(ind, obs, slant);
		case E_IonoModel::SPHERICAL_CAPS:   	return ion_coef_sphcap(ind, obs, slant);
		case E_IonoModel::BSPLINE:      	  	return ion_coef_bsplin(ind, obs, slant);
	}
	return 0;
}

/*****************************************************************************************/
/* Updating the ionosphere model parameters                                              */
/* The ionosphere model should be initialized by calling 'config_ionosph_model'          */
/* Ionosphere measurments from stations should be loaded using 'update_station_measr'    */
/*****************************************************************************************/
void update_ionosph_model(
	Trace&			trace,			///< Trace to output to
	StationList&	stations,       ///< List of pointers to stations to use
	GTime 			iontime,		///< Time of this epoch
	double			tgap)        	///< Time elapsed since last update
{
	TestStack ts(__FUNCTION__);
	
	iono_KFState.initFilterEpoch();
	iono_KFState.time = iontime;
	
	if (acsConfig.ionFilterOpts.model== +E_IonoModel::NONE) return;
	if (acsConfig.output_ionstec) write_receivr_measr(trace, stations, iono_KFState.time);
	if (acsConfig.ionFilterOpts.model== +E_IonoModel::MEAS_OUT) return; 

	tracepde(3, trace,"UPDATE IONO MODEL ...\n");
	//count valid measurements for each station
	map<string, map<E_Sys,int>> stationlist;
	map<SatSys, int> satelltlist;
	map<E_Sys, string> maxCountSta;
	map<E_Sys,int> satCount;
	int NmeaTot=0;
	
	for (auto& rec_ptr 	: stations)
	{
		map<E_Sys,int> satcnt;
		for (auto& obs 		: rec_ptr->obsList)
		{
			if (obs.ionExclude) 
				continue;
			
			satcnt[obs.Sat.sys]++;
			satelltlist[obs.Sat]++;
		}
		for(auto& [sys,nsat] : satcnt)
		{
			if(nsat<MIN_NSAT_STA) 
				continue;
			stationlist[rec_ptr->id][sys]+=nsat;
			
			if (rec_ptr->id==acsConfig.pivot_station ) nsat=999;
			if (rec_ptr->id==IonRefSta[sys])		   nsat=9999;
			
			if (satCount[sys]<nsat)
			{
				satCount[sys]=nsat;
				maxCountSta[sys]=rec_ptr->id;
			}
		}
	}
	int NSatTot=0;
	int NStaTot=0;
	int NMeaTot=0;
	
	for(auto& [rec,list] : stationlist) 
		NStaTot+=list.size();
	
	for(auto& [sat,nrec] : satelltlist)
	{
		NSatTot++;
		NMeaTot += nrec;
	} 
	
	if(NMeaTot < (acsConfig.ionFilterOpts.NBasis + NSatTot + NStaTot))
	{
		tracepde(2, trace,"#IONO_MOD Not enough Measurements %5d < %4d + %4d + %3d\n", NMeaTot, acsConfig.ionFilterOpts.NBasis, NSatTot, NStaTot);
		return;
	}
	
	map<E_Sys,bool> reset_DCBs;
	for(auto& [sys,nsat] : satCount)
	{
		reset_DCBs[sys]=false;
		if(nsat<MIN_NSAT_STA) continue;
		if(maxCountSta[sys]!=IonRefSta[sys])
		{
			tracepde(2, trace,"#IONO_MOD WARNING change in reference station for %s: %s\n", sys._to_string(), maxCountSta[sys]);
			reset_DCBs[sys]=false;
		}
		IonRefSta[sys]=maxCountSta[sys];
		tracepde(4, trace,"#IONO_MOD REF STATION for %s: %s\n", sys._to_string(), maxCountSta[sys]);
	} 
	
	//add measurements and create design matrix entries
	KFMeasEntryList kfMeasEntryList;

	for (auto& rec_ptr	: stations)
	{
		auto& rec = *rec_ptr;
		string sta= rec.id;
		for (auto& obs 		: rec_ptr->obsList)
		{
			E_Sys sys=obs.Sat.sys;
			
			if(obs.ionExclude)						continue;
			if(stationlist[sta][sys]<MIN_NSAT_STA) 	continue;
			
			/************ Ionosphere Measurements ************/
			ObsKey obsKey;
			obsKey.Sat = obs.Sat;
			obsKey.str = sta;
			KFMeasEntry meas(&iono_KFState, obsKey);
			meas.setValue(obs.STECsmth);
			meas.setNoise(obs.STECsmvr);
			
			/************ receiver DCB ************/        /* We may need to change this for multi-code solutions */
			if(rec.id != IonRefSta[sys])
			{
				KFKey recDCBKey;
				recDCBKey.type	= KF::DCB;
				recDCBKey.str	= rec.id;
				recDCBKey.num	= sys._to_integral();
			
				InitialState recDCBInit;
				recDCBInit.x = 0;
				recDCBInit.P = 0;
				recDCBInit.Q = 0;
				
				meas.addDsgnEntry(recDCBKey, 1, recDCBInit);
			}
			
			/************ receiver DCB ************/        /* We may need to change this for multi-code solutions */
			KFKey satDCBKey;
			satDCBKey.type	= KF::DCB;
			satDCBKey.Sat	= obs.Sat;

			InitialState satDCBInit;
			satDCBInit.x = 0;
			satDCBInit.P = 0;
			satDCBInit.Q = 0;

			meas.addDsgnEntry(satDCBKey, 1, satDCBInit);
			
			for (int i = 0; i < acsConfig.ionFilterOpts.NBasis; i++)
			{
				double coef = ion_coef(i, obs, true);
				
				KFKey ionModelKey;
				ionModelKey.type	= KF::IONOSPHERIC;
				ionModelKey.num		= i;

				InitialState ionModelInit;
				ionModelInit.x = 0;
				ionModelInit.P = 0;
				ionModelInit.Q = SQR(acsConfig.ionFilterOpts.model_noise);
				
				meas.addDsgnEntry(ionModelKey, coef, ionModelInit);
				
				tracepde(4, trace,"#IONO_MOD %s %4d %9.5f %10.5f %8.5f %8.5f %12.5e %9.5f %12.5e\n",
					((string)meas.obsKey).c_str(), i, obs.latIPP[0]*R2D, obs.lonIPP[0]*R2D, obs.angIPP[0], 
					obs.STECtoDELAY, coef, obs.STECsmth, obs.STECsmvr);
			}
			
			kfMeasEntryList.push_back(meas);
		}
	}
	
	//add process noise to existing states as per their initialisations.
	iono_KFState.stateTransition(trace, tgap);

	//combine the measurement list into a single design matrix, measurement vector, and measurement noise vector
	KFMeas combinedMeas = iono_KFState.combineKFMeasList(kfMeasEntryList);

	//if there are uninitialised state values, estimate them using least squares
	if (iono_KFState.lsqRequired)
	{
		iono_KFState.lsqRequired = false;
		trace << std::endl << " -------INITIALISING IONO USING LEAST SQUARES--------" << std::endl;

		iono_KFState.leastSquareInitStates(std::cout, combinedMeas, true);
	}
	else
	{
		//perform kalman filtering
		trace << std::endl << " ------- DOING IONO KALMAN FILTER --------" << std::endl;

		iono_KFState.filterKalman(trace, combinedMeas, false);
// 		trace << std::endl << " ------- AFTER IONO KALMAN FILTER --------" << std::endl;
	}

	
	MatrixXd atran = combinedMeas.A.transpose();
	TestStack::testMat("v", combinedMeas.V);
	TestStack::testMat("y", combinedMeas.Y);
	TestStack::testMat("x", iono_KFState.x, 0, &iono_KFState.P);
	TestStack::testMat("H", atran);
	TestStack::testMat("var", combinedMeas.R);
	
	if (acsConfig.output_ionex)
	{
		ionex_file_write(trace, iontime);
	}
	
	if (acsConfig.output_biasSINEX) for (auto& [dcbKey, index] : iono_KFState.kfIndexMap)
	{
		if (dcbKey.type != KF::DCB) 
			continue;
		
		double bias		= 0;
		double variance	= 0;
		iono_KFState.getKFValue(dcbKey, bias, &variance);
		
		if (acsConfig.process_sys[dcbKey.Sat.sys])
		{
			outp_bias(trace, tsync, E_BiasType::DSB, "",			dcbKey.Sat,	E_ObsCode::L1C, E_ObsCode::L2W, bias, variance, 86400, CODE);
		}
		else if (dcbKey.str != "")
		{
			SatSys sat0 = {};
			outp_bias(trace, tsync, E_BiasType::DSB, dcbKey.str,	sat0,		E_ObsCode::L1C, E_ObsCode::L2W, bias, variance, 86400, CODE);
		}
	}
}
