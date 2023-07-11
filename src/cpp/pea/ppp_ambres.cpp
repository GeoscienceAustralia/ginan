
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
#include "instrument.hpp"
#include "GNSSambres.hpp"
#include "biasSINEX.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "trace.hpp"

KFState kfState_forAR;

bool	fixKFReady = false;

void applyUCAmbiguities(
	Trace& trace,		///< Debug trace
	KFState& kfState,	///< Reference to Kalman filter containing float solutions
	GinAR_mtx& mtrx)	///< Reference to structure containing fixed ambiguities and Z transformations
{
	int nz = mtrx.zfix.size();
	int nx = mtrx.ambmap.size();
	
	tracepdeex(1,trace,"   %d out of %d ambiguities resolved, applying...\n",nz,nx);
	
	MatrixXd Z    = mtrx.Ztrs;
	VectorXd zfix = mtrx.zfix;
	
	if (AR_VERBO)
	{
		trace << std::endl << "zfix =" << std::endl << zfix.transpose()	<< std::endl;
		trace << std::endl << "Ztrs =" << std::endl << Z   			 	<< std::endl;
	}
	
	KFMeasEntryList kfMeasEntryList;

	for (int i=0; i<nz; i++)
	{
		double residual = zfix(i);
		
		KFMeasEntry measEntry(&kfState);
		measEntry.obsKey.type = KF::Z_AMB;
		measEntry.obsKey.num  = i;
		measEntry.addNoiseEntry(measEntry.obsKey, 1, FIXED_AMB_VAR);
		
		tracepdeex(4,trace,"      Applying:  ");
		
		for (int j=0; j<nx; j++)
		if (Z(i,j)!=0)
		{
			double ambiguity=0;
			KFKey key = mtrx.ambmap[j];
			kfState.getKFValue(key, ambiguity);
			
			residual -= Z(i,j) * ambiguity;
			
			tracepdeex(4,trace,"%+3.0f A(%s,%s,%2d) ",Z(i,j),key.str.c_str(),key.Sat.id().c_str(),key.num);
			
			InitialState init;
			init.x = ambiguity;
			init.P = 3600;
			
			measEntry.addDsgnEntry(mtrx.ambmap[j], Z(i,j), init);
		}
		
		tracepdeex(4,trace,"= %+10.5f\n", zfix(i));
		
		measEntry.setInnov(residual);
		
		kfMeasEntryList.push_back(measEntry);
	}

	kfState.noiseElementStateTransition();
	KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList, kfState.time);
	kfState.filterKalman(trace, combinedMeas, true);
	kfState.outputStates(trace);
}

int PPP_AR(
	Trace&		trace,		///< Debug trace
	KFState&	kfState)	///< Filter state
{
	Instrument	instrument(__FUNCTION__);
		
	tracepdeex(3,trace,"Ginan 2.0 AR: %s\n",kfState.time.to_string(2));
	fixKFReady=false;
	kfState_forAR = kfState;
	
	if (acsConfig.ambrOpts.mode == +E_ARmode::OFF)
	{
		return 0;
	}
	
	GinAR_mtx ARmtx;
	map<string,int> nsat;		// number of satellites visible by station
	map<SatSys,int> nsta;		// number of stations visible by satellite
	
	int ind=0;
	vector<int> indices;
	for (auto& [key, index] : kfState.kfIndexMap)
	if (key.type == KF::AMBIGUITY)
	{
		if (!acsConfig.solve_amb_for[key.Sat.sys])
			continue;
			
		indices.push_back(index);
		ARmtx.ambmap[ind++]=key;
	}
	
	if (ind<=0)
		return 0;
	
	ARmtx.aflt  = kfState.x(indices);
	ARmtx.Paflt = kfState.P(indices, indices);
	
	GinAR_opt ARopt;
	ARopt.mode = acsConfig.ambrOpts.mode;
	if (ARopt.mode == +E_ARmode::ROUND
	 || ARopt.mode == +E_ARmode::ITER_RND)
		ARopt.mode = E_ARmode::BOOTST;
	ARopt.sucthr = acsConfig.ambrOpts.succsThres;
	ARopt.ratthr = acsConfig.ambrOpts.ratioThres;
	ARopt.nset   = acsConfig.ambrOpts.lambda_set;
	ARopt.nitr   = acsConfig.ambrOpts.AR_max_itr;
	if (acsConfig.trace_level > 4) 
		AR_VERBO=true;
	
	/* Resolve ambiguities */
	int nfix = GNSS_AR (trace, ARmtx, ARopt);
	if (nfix>0)
	{
		applyUCAmbiguities (trace, kfState, ARmtx);
		fixKFReady = true;
	}
	
	kfState_forAR = kfState;
	kfState_forAR.rts_basename = kfState.rts_basename + "_AR";
	
	return nfix;
}

bool copyFixedKF(KFState& fixed)
{
	fixed = kfState_forAR;
	
	/* add additional readiness check here if needed */
	
	return fixKFReady;
}

void overwriteFixedKF(
	KFState& kfState)
{
	kfState_forAR = kfState;
}

bool queryBiasUC(
	Trace&		trace,		///< debug stream 
	GTime		time,		///< time of biases
	SatSys		Sat,		///< satellite (for receiver biases, sat.sys needs to be set to the appropriate system, and sat.prn must be 0)
	string		rec,		///< receiver  (for satellite biases nees to be "")
	E_ObsCode	code,		///< signal code
	double&		bias,		///< bias value
	double&		var,		///< bias variance	
	E_MeasType	type)		///< measurement type
{
	auto& sysCodes = acsConfig.code_priorities[Sat.sys];
	
	auto prioritityIt = std::find(sysCodes.begin(), sysCodes.end(), code);
	if (prioritityIt == sysCodes.end())
		return false;
	
	KFKey kfKey;
	kfKey.str	= rec;
	kfKey.Sat	= Sat;
	kfKey.num	= code;
	
	if (Sat.prn == 0)		//todo aaron, check if needed and reverse logic
	{
		auto& recOpts = acsConfig.getRecOpts(rec, {Sat.sys._to_string(), code._to_string()});
		
		if (type == CODE)
		{
			if (!recOpts.rec_code_bias.enable)
				return true;
			
			InitialState init = initialStateFromConfig(recOpts.code_bias);
			if (init.estimate == false)
			{
				getBiasSinex(trace, time, rec, Sat, code, CODE, bias, var);
				return true;
			}
			
			kfKey.type	= KF::CODE_BIAS;
			
			return kfState_forAR.getKFValue(kfKey,bias, &var);
		}
		
		if (type == PHAS)
		{
			if (!recOpts.rec_phase_bias.enable)
				return true;
			
			InitialState init = initialStateFromConfig(recOpts.phase_bias);
			if (init.estimate == false)
			{
				getBiasSinex(trace, time, rec, Sat, code, PHAS, bias, var);
				return true;
			}
			
			kfKey.type	= KF::PHASE_BIAS;
			
			return kfState_forAR.getKFValue(kfKey, bias, &var);
		}
	}
	else if (rec.empty())
	{
		auto& satOpts = acsConfig.getSatOpts(Sat);
		
		if (type == CODE)
		{
			if (!satOpts.sat_code_bias.enable)
				return true;
			
			InitialState init = initialStateFromConfig(satOpts.code_bias);
			if (init.estimate == false)
			{
				getBiasSinex(trace, time, Sat.id(), Sat, code, CODE, bias, var);
				return true;
			}
			
			kfKey.type	= KF::CODE_BIAS;
			string keyStr = kfKey;
			bool pass = kfState_forAR.getKFValue(kfKey, bias, &var);
			tracepdeex(5,trace,"\n Searching UC %s - %s", keyStr.c_str(), pass?"found":"not found");
			
			return pass;
		}
		
		if (type == PHAS)
		{
			if (!satOpts.sat_phase_bias.enable)
				return true;
			
			InitialState init = initialStateFromConfig(satOpts.phase_bias);
			if (init.estimate == false)
			{
				getBiasSinex(trace, time, Sat.id(), Sat, code, PHAS, bias, var);
				return true;
			}
			
			kfKey.type	= KF::PHASE_BIAS;
			string keyStr = kfKey;
			bool pass = kfState_forAR.getKFValue(kfKey, bias, &var);
			tracepdeex(5,trace,"\n Searching UC %s - %s", keyStr.c_str(), pass?"found":"not found");
			
			return pass;
		}
	}
	
	return false;
}
