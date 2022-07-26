
// #pragma GCC optimize ("O0")

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
IonModel			ionModel;
KFState				iono_KFState;
map<E_Sys,string>	ionRefRec;

#define		INIT_VAR_RDCB	100.0
#define		INIT_VAR_SDCB	100.0
#define		INIT_VAR_SCHP	100.0


int config_ionosph_model()
{
	iono_KFState.max_filter_iter	= acsConfig.pppOpts.max_filter_iter;
	iono_KFState.max_prefit_remv	= acsConfig.pppOpts.max_prefit_remv;
	iono_KFState.inverter			= acsConfig.pppOpts.inverter;
	iono_KFState.output_residuals	= acsConfig.output_residuals;
		
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::MEAS_OUT:				return 1;
		case E_IonoModel::SPHERICAL_HARMONICS:	return configure_iono_model_sphhar();
		case E_IonoModel::SPHERICAL_CAPS:		return configure_iono_model_sphcap();
		case E_IonoModel::BSPLINE:				return configure_iono_model_bsplin();
		case E_IonoModel::NONE:					return 0;
	}
	return 0;
}

double ion_coef(
	int		ind,
	Obs&	obs, 
	bool	slant)
{
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::SPHERICAL_HARMONICS:	return ion_coef_sphhar(ind, obs, slant);
		case E_IonoModel::SPHERICAL_CAPS:		return ion_coef_sphcap(ind, obs, slant);
		case E_IonoModel::BSPLINE:				return ion_coef_bsplin(ind, obs, slant);
		default:								return 0;
	}
}

/** Updating the ionosphere model parameters     
 * The ionosphere model should be initialized by calling 'config_ionosph_model'        
 * Ionosphere measurments from stations should be loaded using 'update_station_measr' 
 */
void updateIonosphereModel(
	Trace&			trace,				///< Trace to output to
	string			ionstecFilename,	///< Filename for ionstec outputs
	string			ionexFilename,		///< Filename for ionex outputs
	StationMap&		stations,			///< List of pointers to stations to use
	GTime 			time)				///< Time of this epoch
{
	TestStack ts(__FUNCTION__);
	
	if (acsConfig.ionModelOpts.model== +E_IonoModel::NONE) 
		return;
	
	if (acsConfig.output_ionstec)
		writeReceiverMeasurements(trace, ionstecFilename, stations, time);
	
	if (acsConfig.ionModelOpts.model== +E_IonoModel::MEAS_OUT) 
		return; 

	tracepdeex(2, trace,"UPDATE IONO MODEL ... %s\n", time.to_string(0).c_str());
	//count valid measurements for each station
	map<string, map<E_Sys,int>>		stationlist;
	map<SatSys, int>				satelltlist;
	map<E_Sys, string>				maxCountSta;
	map<E_Sys,int>					satCount;
	
	for (auto& [id, rec] : stations)
	{
		map<E_Sys, int> satcnt;
		for (auto& obs 		: rec.obsList)
		{
			if (obs.ionExclude) 
				continue;
			
			satcnt[obs.Sat.sys]++;
			satelltlist[obs.Sat]++;
		}
		
		for (auto& [sys, nsat] : satcnt)
		{
			if (nsat < MIN_NSAT_STA) 
				continue;
			
			stationlist[rec.id][sys] += nsat;
			
			if (rec.id == acsConfig.pivot_station)		nsat=999;
			if (rec.id == ionRefRec[sys])				nsat=9999;
			
			if (satCount[sys] < nsat)
			{
				satCount	[sys] = nsat;
				maxCountSta	[sys] = rec.id;
			}
		}
	}
	
	int NSatTot = 0;
	int NStaTot = 0;
	int NMeaTot = 0;
	
	for (auto& [rec,list] : stationlist) 
		NStaTot += list.size();
	
	for (auto& [sat,nrec] : satelltlist)
	{
		NSatTot++;
		NMeaTot += nrec;
	} 
	
	if (NMeaTot < (acsConfig.ionModelOpts.NBasis + NSatTot + NStaTot))
	{
		tracepdeex(2, trace,"#IONO_MOD Not enough Measurements %5d < %4d + %4d + %3d\n", NMeaTot, acsConfig.ionModelOpts.NBasis, NSatTot, NStaTot);
		return;
	}
	
	map<E_Sys, bool> reset_DCBs;
	for (auto& [sys, nsat] : satCount)
	{
		reset_DCBs[sys] = false;
		
		if (nsat < MIN_NSAT_STA) 
			continue;
		
		if (maxCountSta[sys] != ionRefRec[sys])
		{
			tracepdeex(2, trace,"#IONO_MOD WARNING change in reference station for %s: %s\n", sys._to_string(), maxCountSta[sys]);
			reset_DCBs[sys] = false;
		}
		ionRefRec[sys] = maxCountSta[sys];
		tracepdeex(4, trace,"#IONO_MOD REF STATION for %s: %s\n", sys._to_string(), maxCountSta[sys]);
	} 
	
	//add measurements and create design matrix entries
	KFMeasEntryList kfMeasEntryList;

	for (auto& [id, rec]	: stations)
	for (auto& obs			: rec.obsList)
	{
		E_Sys sys = obs.Sat.sys;
		
		if (obs.ionExclude)								{	continue;	}
		if (stationlist[rec.id][sys] < MIN_NSAT_STA)	{	continue;	}
		
		/************ Ionosphere Measurements ************/
		ObsKey obsKey;
		obsKey.Sat = obs.Sat;
		obsKey.str = rec.id;
		
		KFMeasEntry meas(&iono_KFState, obsKey);
		meas.setValue(obs.STECsmth);
		meas.setNoise(obs.STECsmvr);
		
		/************ receiver DCB ************/        /* We may need to change this for multi-code solutions */
		if (rec.id != ionRefRec[sys])
		{
			KFKey recDCBKey;
			recDCBKey.type	= KF::DCB;
			recDCBKey.str	= rec.id;
			recDCBKey.num	= sys._to_integral();
			
			meas.addDsgnEntry(recDCBKey, 1);
		}
		
		/************ satellite DCB ************/        /* We may need to change this for multi-code solutions */
		KFKey satDCBKey;
		satDCBKey.type	= KF::DCB;
		satDCBKey.Sat	= obs.Sat;

		meas.addDsgnEntry(satDCBKey, 1);
		
		for (int i = 0; i < acsConfig.ionModelOpts.NBasis; i++)
		{
			double coef = ion_coef(i, obs, true);
			
			KFKey ionModelKey;
			ionModelKey.type	= KF::IONOSPHERIC;
			ionModelKey.num		= i;

			InitialState ionModelInit;
			ionModelInit.Q = SQR(acsConfig.ionModelOpts.model_noise);
			
			meas.addDsgnEntry(ionModelKey, coef, ionModelInit);
			
			tracepdeex(5, trace,"#IONO_MOD %s %4d %9.5f %10.5f %8.5f %8.5f %12.5e %9.5f %12.5e\n",
					((string)meas.obsKey).c_str(), i,
					obs.latIPP[0]*R2D, 
					obs.lonIPP[0]*R2D, 
					obs.angIPP[0], 
					obs.STECtoDELAY, 
					coef, 
					obs.STECsmth,
					obs.STECsmvr);
		}
		
		kfMeasEntryList.push_back(meas);
	}
	
	//add process noise to existing states as per their initialisations.
	iono_KFState.stateTransition(trace, time);

	//combine the measurement list into a single design matrix, measurement vector, and measurement noise vector
	KFMeas combinedMeas = iono_KFState.combineKFMeasList(kfMeasEntryList);

	//if there are uninitialised state values, estimate them using least squares
	if (iono_KFState.lsqRequired)
	{
		iono_KFState.lsqRequired = false;
		trace << std::endl << "-------INITIALISING IONO USING LEAST SQUARES--------" << std::endl;

		iono_KFState.leastSquareInitStates(std::cout, combinedMeas, true);
	}
	else
	{
		//perform kalman filtering
		trace << std::endl << "------- DOING IONO KALMAN FILTER --------" << std::endl;

		iono_KFState.filterKalman(trace, combinedMeas, false);
// 		trace << std::endl << " ------- AFTER IONO KALMAN FILTER --------" << std::endl;
	}

	iono_KFState.outputStates(trace, " Ion");
	
	MatrixXd atran = combinedMeas.H.transpose();
	TestStack::testMat("v", combinedMeas.V);
	TestStack::testMat("y", combinedMeas.Y);
	TestStack::testMat("x", iono_KFState.x, 0, &iono_KFState.P);
	TestStack::testMat("H", atran);
	TestStack::testMat("var", combinedMeas.R);
	
	if (acsConfig.output_ionex)
	{
		ionexFileWrite(trace, ionexFilename, time);
	}
	
	if (acsConfig.output_bias_sinex)
	for (auto& [dcbKey, index] : iono_KFState.kfIndexMap)
	{
		if (dcbKey.type != KF::DCB) 
			continue;
		
		double bias		= 0;
		double variance	= 0;
		iono_KFState.getKFValue(dcbKey, bias, &variance);
		
		if (acsConfig.process_sys[dcbKey.Sat.sys])
		{
			outp_bias(trace, time, "",			dcbKey.Sat,	E_ObsCode::L1C, E_ObsCode::L2W, bias, variance, acsConfig.ambrOpts.biasOutrate, CODE);
		}
		else if (dcbKey.str != "")
		{
			SatSys sat0 = {};
			outp_bias(trace, time, dcbKey.str,	sat0,		E_ObsCode::L1C, E_ObsCode::L2W, bias, variance, acsConfig.ambrOpts.biasOutrate, CODE);
		}
	}
}
