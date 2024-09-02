
// #pragma GCC optimize ("O0")

#include <string>
#include <map>

using std::string;
using std::map;

#include "ionoModel.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "enums.h"


/* Global parameters */
map<E_Sys,string>	ionRefRec;
map<int,int>		ionStateOutage;

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

bool configIonModel(
	Trace&		trace)
{
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::MEAS_OUT:				return true;
		case E_IonoModel::SPHERICAL_HARMONICS:	return configIonModelSphhar(trace);
		case E_IonoModel::SPHERICAL_CAPS:		return configIonModelSphcap(trace);
		case E_IonoModel::BSPLINE:				return configIonModelBsplin(trace);
		case E_IonoModel::LOCAL:				return configIonModelLocal_(trace);
		default:								return false;
	}
}

double ionModelCoef(
	Trace&		trace,
	int			ind,
	IonoObs&	obs,
	bool		slant)
{
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::SPHERICAL_HARMONICS:	return ionCoefSphhar(trace,	ind, obs, slant);
		case E_IonoModel::SPHERICAL_CAPS:		return ionCoefSphcap(trace,	ind, obs, slant);
		case E_IonoModel::BSPLINE:				return ionCoefBsplin(trace,	ind, obs, slant);
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
	ReceiverMap&	receiverMap,		///< List of pointers to stations to use
	GTime 			time)				///< Time of this epoch
{
	if (!ionoConfigured)
		ionoConfigured = configIonModel(trace);

	if (!ionoConfigured)
		return;

	if (acsConfig.ionModelOpts.model == +E_IonoModel::NONE)
		return;

	if (acsConfig.ionModelOpts.model == +E_IonoModel::MEAS_OUT)
		return;

	tracepdeex(2, trace,"UPDATE IONO MODEL ... %s\n", time.to_string().c_str());
	//count valid measurements for each station
	map<string, map<E_Sys, int>>	stationList;
	map<SatSys, int>				satelliteList;
	map<E_Sys, string>				maxCountRec;
	map<E_Sys, int>					satCount;

	for (auto& [id, rec] : receiverMap)
	{
		map<E_Sys, int> satcnt;
		for (auto& obs 		: only<GObs>(rec.obsList))
		{
			if (obs.ionExclude)
				continue;

			if (acsConfig.use_for_iono_model[obs.Sat.sys] == false)
				continue;

			satcnt[obs.Sat.sys]++;
			satelliteList[obs.Sat]++;
		}

		for (auto& [sys, nsat] : satcnt)
		{
			if (nsat < MIN_NSAT_REC)
				continue;

			stationList[rec.id][sys] += nsat;

			if (rec.id == acsConfig.pivot_receiver)		nsat = 999;
			if (rec.id == ionRefRec[sys])				nsat = 9999;

			if (satCount[sys] < nsat)
			{
				satCount	[sys] = nsat;
				maxCountRec	[sys] = rec.id;
			}
		}
	}

	int nSatTot		= 0;
	int nRecTot		= 0;
	int nMeasTot	= 0;

	for (auto& [rec, list] : stationList)
		nRecTot += list.size();

	for (auto& [sat, nRec] : satelliteList)
	{
		nSatTot++;
		nMeasTot += nRec;
	}

	int nStateTot = acsConfig.ionModelOpts.numBasis + nSatTot + nRecTot;
	if (acsConfig.ionModelOpts.model == +E_IonoModel::LOCAL)
	{
		nStateTot = 0;
		for (auto& [regId,regData] : nav.ssrAtm.atmosRegionsMap)
		{
			if		(regData.ionoGrid)				nStateTot += nSatTot * (regData.gridLatDeg.size()	- 1);
			else if (regData.ionoPolySize > 0)		nStateTot += nSatTot * (regData.ionoPolySize		- 1);
		}
	}

	if (nMeasTot < nStateTot)
	{
		tracepdeex(2, trace,"#IONO_MOD Not enough Measurements %5d < %4d;  %3d, %3d\n", nMeasTot, nStateTot, nSatTot, nRecTot);
		return;
	}

	map<E_Sys, bool> reset_DCBs;
	for (auto& [sys, nsat] : satCount)
	{
		reset_DCBs[sys] = false;

		if (nsat < MIN_NSAT_REC)
			continue;

		if (maxCountRec[sys] != ionRefRec[sys])
		{
			tracepdeex(2, trace,"#IONO_MOD WARNING change in reference station for %s: %s\n", sys._to_string(), maxCountRec[sys]);
			reset_DCBs[sys] = true;
		}

		ionRefRec[sys] = maxCountRec[sys];
		tracepdeex(4, trace,"#IONO_MOD REF STATION for %s: %s\n", sys._to_string(), maxCountRec[sys]);
	}

	map <E_Sys,int> mainObsCombo;
	for (auto& [sys,pivt] : ionRefRec)
	for (auto& obs		  : only<GObs>(receiverMap[pivt].obsList))
	{
		if (obs.Sat.sys != sys) continue;

		mainObsCombo[sys] = obs.stecCodeCombo;
		break;
	}

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::IONOSPHERIC)
			continue;

		if (ionStateOutage[index]++ > 3)
			kfState.removeState(key);
	}

	//add measurements and create design matrix entries
	KFMeasEntryList kfMeasEntryList;

	for (auto& [id, rec]	: receiverMap)
	for (auto& obs			: only<GObs>(rec.obsList))
	{
		E_Sys sys = obs.Sat.sys;

		auto& recOpts = acsConfig.getRecOpts(id);
		auto& satOpts = acsConfig.getSatOpts(obs.Sat);

		if (obs.ionExclude)											{	continue;	}
		if (obs.stecType <= 0)										{	continue;	}
		if (stationList[rec.id][sys] < MIN_NSAT_REC)				{	continue;	}
		if (obs.stecVar > SQR(recOpts.iono_sigma_limit)) 			{	continue;	}

		/************ Ionosphere Measurements ************/
		KFKey obsKey;
		obsKey.Sat = obs.Sat;
		obsKey.str = rec.id;

		KFMeasEntry meas(&kfState, obsKey);
		meas.setValue(obs.stecVal);
		meas.setNoise(obs.stecVar);

		/************ receiver DCB ************/        /* We may need to change this for multi-code solutions */
		SatSys sat0;
		sat0.sys = sys;
		sat0.prn = 0;

		KFKey recDCBKey;
		recDCBKey.type	= KF::CODE_BIAS;
		recDCBKey.str	= rec.id;
		recDCBKey.Sat	= sat0;
		recDCBKey.num	= obs.stecCodeCombo;

		if (reset_DCBs[sys])
			kfState.removeState(recDCBKey);

		InitialState init = initialStateFromConfig(recOpts.code_bias);
		if (rec.id != ionRefRec[sys])
			meas.addDsgnEntry(recDCBKey, 1, init);

		/************ satellite DCB ************/        /* We may need to change this for multi-code solutions */
		if (acsConfig.ionModelOpts.estimate_sat_dcb		///todo aaron, ew..
		|| mainObsCombo[sys] != obs.stecCodeCombo)
		{
			InitialState init = initialStateFromConfig(satOpts.code_bias);

			KFKey satDCBKey;
			satDCBKey.type	= KF::CODE_BIAS;
			satDCBKey.Sat	= obs.Sat;
			satDCBKey.num	= obs.stecCodeCombo;

			meas.addDsgnEntry(satDCBKey, 1, init);
		}

		/************ Ionosphere basis ************/
		obs.ionoSat = obs.Sat;
		for (int i = 0; i < acsConfig.ionModelOpts.numBasis; i++)
		{
			double coef = ionModelCoef(trace,i, obs, true);

			if (coef == 0)
				continue;

			ionStateOutage[i] = 0;

			KFKey ionModelKey;
			ionModelKey.type	= KF::IONOSPHERIC;
			ionModelKey.num		= i;

			InitialState ionModelInit = initialStateFromConfig(acsConfig.ionModelOpts.ion);

			meas.addDsgnEntry(ionModelKey, coef, ionModelInit);

			tracepdeex(4, trace,"#IONO_MOD %s %4d %9.5f %10.5f %8.5f %8.5f %12.5e %9.5f %12.5e\n",
					((string)meas.obsKey).c_str(),
					i,
					obs.ippMap[0].latDeg,
					obs.ippMap[0].lonDeg,
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
	KFMeas kfMeas(kfState, kfMeasEntryList);

	//if there are uninitialised state values, estimate them using least squares
	if (kfState.lsqRequired)
	{
		trace << "\n" << "-------INITIALISING IONO USING LEAST SQUARES--------" << "\n";

		kfState.leastSquareInitStates(std::cout, kfMeas, true);
	}
	else
	{
		trace << "\n" << "------- DOING IONO KALMAN FILTER --------" << "\n";

		kfState.filterKalman(trace, kfMeas, "/IONO", false);
	}

	kfState.outputStates(trace, "/ION");
}


double getSSRIono(
	Trace&		trace,		///< Debug trace
	GTime		time,		///< time of ionosphere correction
	Vector3d&	rRec,		///< receiver position
	AzEl&	 	azel,		///< satellite azimut/elevation
	double& 	variance,	///< Ionosphere variance
	SatSys&	 	Sat)		///< Satellite
{
	double ionoDelay = 0;

	if (getCmpSSRIono(trace, time, nav.ssrAtm, rRec,		ionoDelay, variance, Sat))		return ionoDelay;
	if (getIGSSSRIono(trace, time, nav.ssrAtm, rRec, azel,	ionoDelay, variance))			return ionoDelay;

	variance = -1;

	return 0;
}


map<E_ObsCode,int> galSigGroups =
{
	{E_ObsCode::L1C, 1},
	{E_ObsCode::L1X, 2},
	{E_ObsCode::L5Q, 1},
	{E_ObsCode::L5X, 2},
	{E_ObsCode::L7Q, 1},
	{E_ObsCode::L7X, 2},
	{E_ObsCode::L8Q, 1},
	{E_ObsCode::L8X, 2},
	{E_ObsCode::L6C, 1},
	{E_ObsCode::L6X, 2}
};

bool galCodeMatch(
	int			keyNum,		///< key number for DCB state
	E_ObsCode	code)		///< signal code
{
	if (galSigGroups.find(code) == galSigGroups.end())
		return false;

	E_ObsCode code2 = E_ObsCode::_from_integral(keyNum/100);
	if (galSigGroups.find(code2) == galSigGroups.end())
		return false;

	return galSigGroups[code] == galSigGroups[code2];
}

/** Estimate biases from Ionosphere modelling DCBs */
bool queryBiasDCB(
	Trace&		trace,		///< debug trace
	KFState&	kfState,	///< Kalman filter to take biases from
	SatSys		Sat,		///< GNSS Satellite
	string		Rec,		///< Receiver id
	E_ObsCode	code,		///< GNSS signal code
	double&		bias,		///< Output bias value
	double&		var)		///< Output bias variance
{
	E_FType ftyp = code2Freq[Sat.sys][code];
	double lamb = nav.satNavMap[Sat].lamMap[ftyp];
	if (lamb == 0)
		return false;

	double dcbVal;
	double dcbVar;

	bool pass = false;
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != +KF::CODE_BIAS)		continue;
		if (key.Sat  != Sat)				continue;
		if (key.str  != Rec)				continue;
		if (Sat.sys  == +E_Sys::GAL
		 && !galCodeMatch(key.num,code))	continue;

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
