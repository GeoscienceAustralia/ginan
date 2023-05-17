
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
KFState				iono_KFState;
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
		case E_IonoModel::SPHERICAL_HARMONICS:	return ionOutputSphcal(trace,kfState);
		case E_IonoModel::LOCAL:				return ionOutputLocal (trace,kfState);
		case E_IonoModel::MEAS_OUT:				return;
		case E_IonoModel::NONE:					return;
	}
	
	tracepdeex (5,trace, "Unsupported system for SSR output\n");
}

int configIonModel()
{
	iono_KFState.max_filter_iter			= acsConfig.pppOpts.max_filter_iter;
	iono_KFState.max_prefit_remv			= acsConfig.pppOpts.max_prefit_remv;
	iono_KFState.inverter					= acsConfig.pppOpts.inverter;
	iono_KFState.output_residuals			= acsConfig.output_residuals;
	iono_KFState.outputMongoMeasurements	= acsConfig.localMongo.output_measurements;
	iono_KFState.rts_basename				= "IONEX_RTS";
		
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
	int		ind,
	GObs&	obs, 
	bool	slant)
{
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::SPHERICAL_HARMONICS:	return ionCoefSphhar(ind, obs, slant);
		case E_IonoModel::SPHERICAL_CAPS:		return ionCoefSphcap(ind, obs, slant);
		case E_IonoModel::BSPLINE:				return ionCoefBsplin(ind, obs, slant);
		case E_IonoModel::LOCAL:				return ionCoefLocal (ind, obs);
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
	if (acsConfig.ionModelOpts.model== +E_IonoModel::NONE) 
		return;
	
	if (acsConfig.output_ionstec)
		writeReceiverMeasurements(trace, ionstecFilename, stations, time);
	
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
		
		KFMeasEntry meas(&iono_KFState, obsKey);
		meas.setValue(obs.STECsmth);
		meas.setNoise(obs.STECsmvr);
		
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
				recDCBKey.type	= KF::DCB;
				recDCBKey.str	= rec.id;
				recDCBKey.Sat	= sat0;
				recDCBKey.num	= obs.STECcodeComb;
			
				meas.addDsgnEntry(recDCBKey, 1, init);
			}
		}
		
		/************ satellite DCB ************/        /* We may need to change this for multi-code solutions */
		{
			InitialState init = initialStateFromConfig(satOpts.code_bias);
			
			if (init.estimate)
			{
				KFKey satDCBKey;
				satDCBKey.type	= KF::DCB;
				satDCBKey.str	= "";
				satDCBKey.Sat	= obs.Sat;
				satDCBKey.num	= obs.STECcodeComb;
			
				meas.addDsgnEntry(satDCBKey, 1, init);
			}
		}
		
		for (int i = 0; i < acsConfig.ionModelOpts.NBasis; i++)
		{
			double coef = ionModelCoef(i, obs, true);
			
			if (coef==0)
				continue;
			
			KFKey ionModelKey;
			ionModelKey.type	= KF::IONOSPHERIC;
			ionModelKey.num		= i;

			InitialState ionModelInit = initialStateFromConfig(acsConfig.ionModelOpts.ion);
			
			meas.addDsgnEntry(ionModelKey, coef, ionModelInit);
			
			tracepdeex(5, trace,"#IONO_MOD %s %4d %9.5f %10.5f %8.5f %8.5f %12.5e %9.5f %12.5e\n",
					((string)meas.obsKey).c_str(), i,
					obs.ippMap[0].lat*R2D, 
					obs.ippMap[0].lon*R2D, 
					obs.ippMap[0].ang, 
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

	iono_KFState.outputStates(trace, "/ION");
	
	MatrixXd atran = combinedMeas.H.transpose();
	
	if (acsConfig.ssrOpts.ionosphere_sources.front() == +E_Source::KALMAN)
		ionosphereSsrUpdate(trace, iono_KFState);
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
	Trace&	trace,	///< debug trace
	SatSys	Sat,	///< GNSS Satellite
	string	Rec,	///< Receiver id
	E_FType	freq,	///< GNSS frequency index
	double&	bias,	///< Output bias value
	double&	var)	///< Output bias variance
{
	E_FType frq1 = F1;
	E_FType frq2 = F2;
	
	if (Sat.sys == +E_Sys::GAL)
		frq2 = F5;

	if (Sat.sys == +E_Sys::GLO)
	{
		frq1 = G1;
		frq2 = G2;
	}

	if	(  freq != frq1
		&& freq != frq2)
	{
		return false;
	}

	double lamb = nav.satNavMap[Sat].lamMap[freq];
	if (lamb == 0)
		return false;

	KFKey kfKey;
	kfKey.type	= KF::DCB;
	kfKey.str	= Rec;
	kfKey.Sat	= Sat;

	double dcbVal;
	double dcbVar;

	bool pass = iono_KFState.getKFValue(kfKey, dcbVal, &dcbVar);
	if (pass == false)
		return false;

	double coef = SQR(CLIGHT / lamb) / 40.3e16;
	bias	= 	  coef	* dcbVal;
	var		= SQR(coef) * dcbVar;
	
	return true;
}
