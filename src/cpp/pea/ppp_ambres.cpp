
// #pragma GCC optimize ("O0")

/**------------------------------------------------------------------------------
* reference :
*     [1] P.J.G.Teunissen, The least-square ambiguity decorrelation adjustment:
*         a method for fast GPS ambiguity estimation, J.Geodesy, Vol.70, 65-82,
*         1995
*     [2] X.-W.Chang, X.Yang, T.Zhou, MLAMBDA: A modified LAMBDA method for
*         integer least-squares estimation, J.Geodesy, Vol.79, 552-565, 2005
*-----------------------------------------------------------------------------*/

#include <iostream>
#include <math.h>

#include "eigenIncluder.hpp"
#include "GNSSambres.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "biases.hpp"
#include "common.hpp"
#include "trace.hpp"

static bool filterError = false;

bool recordFilterError(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index,
	bool		postFit)
{
	filterError = true;

	return true;
}

bool applyBestIntegerAmbiguity(
	Trace&		trace,		///< Debug trace
	KFState&	kfState)	///< Reference to Kalman filter containing float solutions
{
	KFKey	bestKey;
	double	smallestVar = 1e10;

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::AMBIGUITY)
		{
			continue;
		}

		double var = kfState.P(index, index);

		if	( var > smallestVar
			||var < FIXED_AMB_VAR * 5)
		{
			continue;
		}

		smallestVar	= var;
		bestKey		= key;
	}

	if (bestKey.type == KF::NONE)
	{
		return false;
	}

	KFMeasEntryList kfMeasEntryList;

	int index = kfState.kfIndexMap[bestKey];

	double closest = round(kfState.x(index));

	KFMeasEntry measEntry(&kfState);

	measEntry.obsKey = bestKey;

	measEntry.addDsgnEntry(bestKey, 1);

	measEntry.setValue(closest);
	measEntry.setNoise(FIXED_AMB_VAR);

	kfMeasEntryList.push_back(measEntry);

	KFMeas kfMeas(kfState, kfMeasEntryList, kfState.time);

	filterError = false;
	kfState.measRejectCallbacks.push_back(recordFilterError);
	{
		kfState.filterKalman(trace, kfMeas);
	}
	kfState.measRejectCallbacks.pop_back();

	if (filterError)
	{
		return false;
	}

	kfState.outputStates(trace, "/AR1");

	return true;
}


void applyUCAmbiguities(
	Trace&		trace,		///< Debug trace
	KFState&	kfState,	///< Reference to Kalman filter containing float solutions
	GinAR_mtx&	mtrx)		///< Reference to structure containing fixed ambiguities and Z transformations
{
	int nz = mtrx.zfix.size();
	int nx = mtrx.ambmap.size();

	tracepdeex(1, trace, "   %d out of %d ambiguities resolved, applying...\n", nz, nx);

	MatrixXd Z		= mtrx.Ztrs;
	VectorXd zfix	= mtrx.zfix;

	if (AR_VERBO)
	{
		trace << "\n" << "zfix =" << "\n" << zfix.transpose()	<< "\n";
		trace << "\n" << "Ztrs =" << "\n" << Z   			 	<< "\n";
	}

	KFMeasEntryList kfMeasEntryList;

	for (int i = 0; i < nz; i++)
	{
		double residual = zfix(i);

		KFMeasEntry measEntry(&kfState);

		measEntry.obsKey.type		= KF::Z_AMB;
		measEntry.obsKey.comment	= "Ambiguity Psueodobs";

		measEntry.addNoiseEntry(measEntry.obsKey, 1, FIXED_AMB_VAR);

		tracepdeex(4, trace, "      Applying:  ");

		for (int j = 0; j < nx; j++)
		{
			if (Z(i,j) == 0)
			{
				continue;
			}

			double ambiguity = 0;

			KFKey key = mtrx.ambmap[j];
			kfState.getKFValue(key, ambiguity);

			residual -= Z(i,j) * ambiguity;

			tracepdeex(4,trace,"%+3.0f A(%s,%s,%2d) ",Z(i,j),key.str.c_str(),key.Sat.id().c_str(),key.num);

			InitialState init;
			init.x = ambiguity;
			init.P = 3600;

			measEntry.addDsgnEntry(mtrx.ambmap[j], Z(i,j), init);
		}

		tracepdeex(4, trace, "= %+10.5f\n", zfix(i));

		measEntry.setInnov(residual);

		kfMeasEntryList.push_back(measEntry);
	}

	KFMeas kfMeas(kfState, kfMeasEntryList, kfState.time);

	kfState.filterKalman(trace, kfMeas, "/AR", true);

	kfState.outputStates(trace, "/AR");
}

void fixAndHoldAmbiguities(
	Trace&		trace,		///< Debug trace
	KFState&	kfState)	///< Filter state
{
	tracepdeex(3, trace, "%s: %s\n", __FUNCTION__, kfState.time.to_string().c_str());

	if (acsConfig.ambrOpts.mode == +E_ARmode::OFF)
	{
		return;
	}

	GinAR_mtx ARmtx;
	map<string, int> nsat;		// number of satellites visible by station
	map<SatSys, int> nsta;		// number of stations visible by satellite

	int ind = 0;
	vector<int> indices;
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::AMBIGUITY)
		{
			continue;
		}

		if (acsConfig.solve_amb_for[key.Sat.sys] == false)
		{
			continue;
		}

		indices.push_back(index);

		ARmtx.ambmap[ind] = key;
		ind++;
	}

	if (ind == 0)
		return;

	ARmtx.aflt	= kfState.x(indices);
	ARmtx.Paflt	= kfState.P(indices, indices);

	GinAR_opt ARopt;
	ARopt.mode		= acsConfig.ambrOpts.mode;
	ARopt.sucthr	= acsConfig.ambrOpts.succsThres;
	ARopt.ratthr	= acsConfig.ambrOpts.ratioThres;
	ARopt.nset		= acsConfig.ambrOpts.lambda_set;
	ARopt.nitr		= acsConfig.ambrOpts.AR_max_itr;

	if (traceLevel > 4)
		AR_VERBO = true;

	// Resolve and apply ambiguities
	int nfix = GNSS_AR(trace, ARmtx, ARopt);
	if (nfix > 0)
	{
		applyUCAmbiguities(trace, kfState, ARmtx);
	}

	while (0)
	{
		bool applied = applyBestIntegerAmbiguity(trace, kfState);

		if (applied == false)
		{
			break;
		}
	}
}



bool queryBiasUC(
	Trace&		trace,		///< debug stream
	GTime		time,		///< time of biases
	KFState&	kfState,	///< filter state to take biases from
	SatSys		Sat,		///< satellite (for receiver biases, sat.sys needs to be set to the appropriate system, and sat.prn must be 0)
	string		rec,		///< receiver  (for satellite biases nees to be "")
	E_ObsCode	code,		///< signal code
	double&		bias,		///< bias value
	double&		var,		///< bias variance
	E_MeasType	type)		///< measurement type
{
	KFKey kfKey;
	kfKey.str	= rec;
	kfKey.Sat	= Sat;
	kfKey.num	= code;

	if (Sat.prn == 0)		//todo aaron, check if needed and reverse logic
	{
		auto& recOpts = acsConfig.getRecOpts(rec, {Sat.sys._to_string(), code._to_string()});

		if (type == CODE)
		{
			if (recOpts.codeBiasModel.enable == false)
				return true;

			InitialState init = initialStateFromConfig(recOpts.code_bias);
			if (init.estimate == false)
			{
				getBias(trace, time, rec, Sat, code, CODE, bias, var);
				return true;
			}

			kfKey.type	= KF::CODE_BIAS;

			return kfState.getKFValue(kfKey, bias, &var);
		}

		if (type == PHAS)
		{
			if (recOpts.phaseBiasModel.enable == false)
				return true;

			InitialState init = initialStateFromConfig(recOpts.phase_bias);
			if (init.estimate == false)
			{
				getBias(trace, time, rec, Sat, code, PHAS, bias, var);

				return true;
			}

			kfKey.type	= KF::PHASE_BIAS;

			return kfState.getKFValue(kfKey, bias, &var);
		}
	}
	else if (rec.empty())
	{
		auto& satOpts = acsConfig.getSatOpts(Sat);

		if (type == CODE)
		{
			if (!satOpts.codeBiasModel.enable)
				return true;

			InitialState init = initialStateFromConfig(satOpts.code_bias);
			if (init.estimate == false)
			{
				getBias(trace, time, Sat.id(), Sat, code, CODE, bias, var);
				return true;
			}

			kfKey.type	= KF::CODE_BIAS;
			bool pass = kfState.getKFValue(kfKey, bias, &var);

			tracepdeex(5,trace,"\n Searching UC %s - %s", ((string)kfKey).c_str(), pass?"found":"not found");

			return pass;
		}

		if (type == PHAS)
		{
			if (satOpts.phaseBiasModel.enable == false)
				return true;

			InitialState init = initialStateFromConfig(satOpts.phase_bias);
			if (init.estimate == false)
			{
				getBias(trace, time, Sat.id(), Sat, code, PHAS, bias, var);
				return true;
			}

			kfKey.type	= KF::PHASE_BIAS;
			bool pass = kfState.getKFValue(kfKey, bias, &var);

			tracepdeex(5,trace,"\n Searching UC %s - %s", ((string)kfKey).c_str(), pass?"found":"not found");

			return pass;
		}
	}

	return false;
}
