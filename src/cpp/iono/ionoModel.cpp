
// #pragma GCC optimize ("O0")

#include <string>
#include <map>

using std::string;
using std::map;

#include "ionoModel.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "enums.h"


/* Global parameters */
map<E_Sys,string>	ionRefRec;

#define		INIT_VAR_RDCB	100.0
#define		INIT_VAR_SDCB	100.0
#define		INIT_VAR_SCHP	100.0

void ionosphereSsrUpdate(
	Trace&		trace,
	KFState&	kfState)
{
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::SPHERICAL_HARMONICS:	return ionOutputSphcal(trace, kfState);
		case E_IonoModel::LOCAL:				return ionOutputLocal (trace, kfState);
		case E_IonoModel::MEAS_OUT:				return;
		case E_IonoModel::NONE:					return;
	}
	
	tracepdeex (5,trace, "Unsupported system for SSR output\n");
}

int configIonModel()
{
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::MEAS_OUT:				return 1;
		case E_IonoModel::SPHERICAL_HARMONICS:	return configIonModelSphhar();
		case E_IonoModel::SPHERICAL_CAPS:		return configIonModelSphcap();
		case E_IonoModel::BSPLINE:				return configIonModelBsplin();
		case E_IonoModel::NONE:					return 0;
	}
	
	return 0;
}

double ionModelCoef(
	Trace&		trace,
	int			ind,
	IonoObs&	obs, 
	bool		slant)
{
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::SPHERICAL_HARMONICS:	return ionCoefSphhar(		ind, obs, slant);
		case E_IonoModel::SPHERICAL_CAPS:		return ionCoefSphcap(		ind, obs, slant);
		case E_IonoModel::BSPLINE:				return ionCoefBsplin(		ind, obs, slant);
		case E_IonoModel::LOCAL:				return ionCoefLocal (trace,	ind, obs);
		default:								return 0;
	}
}

/** Updating the ionosphere model parameters     
 * The ionosphere model should be initialized by calling 'config_ionosph_model'        
 * Ionosphere measurments from stations should be loaded using 'update_station_measr' 
 */
void filterIonosphere(
	Trace&			trace,				///< Trace to output to
	KFState&		kfState,			///< Filter state
	StationMap&		stations,			///< List of pointers to stations to use
	GTime 			time)				///< Time of this epoch
{
	if (acsConfig.ionModelOpts.model== +E_IonoModel::NONE) 
		return;
	
	if (acsConfig.ionModelOpts.model== +E_IonoModel::MEAS_OUT) 
		return; 

	tracepdeex(2, trace,"UPDATE IONO MODEL ... %s\n", time.to_string().c_str());
	//count valid measurements for each station
	map<string, map<E_Sys,int>>		stationlist;
	map<SatSys, int>				satelltlist;
	map<E_Sys, string>				maxCountSta;
	map<E_Sys,int>					satCount;
	
	for (auto& [id, rec] : stations)
	{
		map<E_Sys, int> satcnt;
		for (auto& obs 		: only<GObs>(rec.obsList))
		{
			if (obs.ionExclude) 
				continue;
			
			if (!acsConfig.use_for_iono_model[obs.Sat.sys])
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
	
	if (NMeaTot < (acsConfig.ionModelOpts.numBasis + NSatTot + NStaTot))
	{
		tracepdeex(2, trace,"#IONO_MOD Not enough Measurements %5d < %4d + %4d + %3d\n", NMeaTot, acsConfig.ionModelOpts.numBasis, NSatTot, NStaTot);
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
	for (auto& obs			: only<GObs>(rec.obsList))
	{
		E_Sys sys = obs.Sat.sys;
		
		if (obs.ionExclude)								{	continue;	}
		if (stationlist[rec.id][sys] < MIN_NSAT_STA)	{	continue;	}
	
		auto& recOpts = acsConfig.getRecOpts(id);
		auto& satOpts = acsConfig.getSatOpts(obs.Sat);
	
		/************ Ionosphere Measurements ************/
		KFKey obsKey;
		obsKey.Sat = obs.Sat;
		obsKey.str = rec.id;
		
		KFMeasEntry meas(&kfState, obsKey);
		meas.setValue(obs.stecVal);
		meas.setNoise(obs.stecVar);
		
		/************ receiver DCB ************/        /* We may need to change this for multi-code solutions */
		if (rec.id != ionRefRec[sys])
		{
			InitialState init = initialStateFromConfig(recOpts.code_bias);
			
			if (init.estimate)
			{
				SatSys sat0;
				sat0.sys = sys;
				sat0.prn = 0;
				
				KFKey recDCBKey;
				recDCBKey.type	= KF::CODE_BIAS;
				recDCBKey.str	= rec.id;
				recDCBKey.Sat	= sat0;
				recDCBKey.num	= obs.stecCodeCombo;
			
				meas.addDsgnEntry(recDCBKey, 1, init);
			}
		}
		
		/************ satellite DCB ************/        /* We may need to change this for multi-code solutions */
		{
			InitialState init = initialStateFromConfig(satOpts.code_bias);
			
			if (init.estimate)
			{
				KFKey satDCBKey;
				satDCBKey.type	= KF::CODE_BIAS;
				satDCBKey.str	= "";
				satDCBKey.Sat	= obs.Sat;
				satDCBKey.num	= obs.stecCodeCombo;
			
				meas.addDsgnEntry(satDCBKey, 1, init);
			}
		}
		
		for (int i = 0; i < acsConfig.ionModelOpts.numBasis; i++)
		{
			double coef = ionModelCoef(trace,i, obs, true);
			
			if (coef == 0)
				continue;
			
			KFKey ionModelKey;
			ionModelKey.type	= KF::IONOSPHERIC;
			ionModelKey.num		= i;

			InitialState ionModelInit = initialStateFromConfig(acsConfig.ionModelOpts.ion);
			
			meas.addDsgnEntry(ionModelKey, coef, ionModelInit);
			
			tracepdeex(5, trace,"#IONO_MOD %s %4d %9.5f %10.5f %8.5f %8.5f %12.5e %9.5f %12.5e\n",
					((string)meas.obsKey).c_str(), 
					i,
					obs.ippMap[0].lat * R2D, 
					obs.ippMap[0].lon * R2D, 
					obs.ippMap[0].slantFactor, 
					obs.stecToDelay, 
					coef, 
					obs.stecVal,
					obs.stecVar);
		}
		
		kfMeasEntryList.push_back(meas);
	}
	
	//add process noise to existing states as per their initialisations.
	kfState.stateTransition(trace, time);

	//combine the measurement list into a single design matrix, measurement vector, and measurement noise vector
	KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

	//if there are uninitialised state values, estimate them using least squares
	if (kfState.lsqRequired)
	{
		kfState.lsqRequired = false;
		trace << std::endl << "-------INITIALISING IONO USING LEAST SQUARES--------" << std::endl;

		kfState.leastSquareInitStates(std::cout, combinedMeas, true);
	}
	else
	{
		trace << std::endl << "------- DOING IONO KALMAN FILTER --------" << std::endl;

		kfState.filterKalman(trace, combinedMeas, false);
	}

	kfState.outputStates(trace, "/ION");
}


double getSSRIono(
	GTime		time,		///< time of ionosphere correction
	Vector3d&	rRec,		///< receiver position 
	Vector3d&	rSat,		///< Satellite position
	double& 	variance,	///< Ionosphere variance
	SatSys&	 	Sat)		///< Satellite
{
	double ionoDelay = 0;

	if (getCmpSSRIono (time, nav.ssrAtm,		rRec,	ionoDelay, variance, Sat))		return ionoDelay;
	if (getIGSSSRIono (time, nav.ssrAtm, rSat,	rRec,	ionoDelay, variance))			return ionoDelay;
	
	variance = -1;
	
	return 0;	
}

/** Estimate biases from Ionosphere modelling DCBs */ 
bool queryBiasDCB(
	Trace&		trace,		///< debug trace
	KFState&	kfState,	///< Kalman filter to take biases from
	SatSys		Sat,		///< GNSS Satellite
	string		Rec,		///< Receiver id
	E_FType		freq,		///< GNSS frequency index
	double&		bias,		///< Output bias value
	double&		var)		///< Output bias variance
{
	double lamb = nav.satNavMap[Sat].lamMap[freq];
	if (lamb == 0)
		return false;

	double dcbVal;
	double dcbVar;

	bool pass = false;
	for (auto& [key, index] : kfState.kfIndexMap)
	if	(  key.type == +KF::CODE_BIAS
		&& key.Sat  == Sat
		&& key.str  == Rec)
	{
		/* We need a way to select between GAL L1X-L5X and L1C-L5Q DCBs... */
		pass = kfState.getKFValue(key, dcbVal, &dcbVar);

		break;
	}
	
	if (pass == false)
		return false;
	
	double coef = TEC_CONSTANT * SQR(lamb / CLIGHT);
	bias	= 	  coef	* dcbVal;
	var		= SQR(coef) * dcbVar;
	
	return true;
}
