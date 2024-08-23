
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

Architecture SPP__()
{

}

#include <algorithm>
#include <sstream>
#include <string>
#include <math.h>

using std::ostringstream;

#include "interactiveTerminal.hpp"
#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "ephPrecise.hpp"
#include "navigation.hpp"
#include "tropModels.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "ionModels.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "biases.hpp"
#include "trace.hpp"
#include "enums.h"
#include "ppp.hpp"

#define ERR_ION		7.0			///< ionospheric delay std (m)
#define ERR_BRDCI	0.5			///< broadcast iono model error factor
#define ERR_CBIAS	0.3			///< code bias error std (m)

/** Calculate pseudorange with code bias correction
*/
bool prange(
	Trace&		trace,			///< Trace file to output to
	GObs&		obs,			///< Observation to calculate pseudorange for
	int			ionomode,		///< Ionospheric correction mode
	double&		range,			///< Pseudorange value output
	double&		measVar,		///< Pseudorange variance output
	double&		biasVar,		///< Bias variance output
	KFState*	kfState_ptr)	///< Optional kfstate to retrieve biases from
{
	SatNav&		satNav	= *obs.satNav_ptr;
	auto&		lam		= satNav.lamMap;

	range	= 0;
	measVar	= 0;
	biasVar	= SQR(ERR_CBIAS);

	E_Sys sys = obs.Sat.sys;
	if (sys == +E_Sys::NONE)
	{
		return false;
	}

	E_FType f_1;
	E_FType f_2;
	E_FType f_3;
	if (!satFreqs(sys,f_1,f_2,f_3))
		return false;

	if	( obs.sigs[f_1].P	== 0
		|| lam[f_1]			== 0)
	{
		return false;
	}

	//get a bias if the default invalid value is still present
	double& B1		= obs.sigs[f_1].biases	[CODE];
	double& var1	= obs.sigs[f_1].biasVars[CODE];
	if (isnan(B1))
	{
		B1		= 0;
		var1	= 0;
		getBias(trace, obs.time, obs.Sat.id(), obs.Sat, obs.sigs[f_1].code, CODE, B1, var1, kfState_ptr);
	}

	double P1	= obs.sigs[f_1].P - B1;

	double PC = 0;
	if (ionomode == E_IonoMode::IONO_FREE_LINEAR_COMBO) /* dual-frequency */
	{
		if	(  obs.sigs[f_2].P	== 0
			|| lam[f_2]			== 0)
		{
			return false;
		}

		double gamma= SQR(lam[f_2]) / SQR(lam[f_1]); /* f1^2/f2^2 */

		double& B2		= obs.sigs[f_2].biases	[CODE];
		double& var2	= obs.sigs[f_2].biasVars[CODE];

		if (isnan(B2))
		{
			B2		= 0;
			var2	= 0;
			getBias(trace, obs.time, obs.Sat.id(), obs.Sat, obs.sigs[f_2].code, CODE, B2, var2, kfState_ptr);
		}

		double P2	= obs.sigs[f_2].P - B2;

		/* iono-free combination */
		PC = (gamma * P1 - P2) / (gamma - 1);
	}
	else   /* single-frequency */
	{
		if (P1 == 0)
		{
			return false;
		}

		double varP1 = 0;

		//get a bias if the default invalid value is still present
		if (isnan(obs.sigs[f_1].biases[CODE]))
		{
			bool pass = getBias(trace, obs.time, obs.Sat.id(), obs.Sat, obs.sigs[f_1].code, CODE, obs.sigs[f_1].biases[CODE], varP1, kfState_ptr);
			if (pass == false)
			{
				BOOST_LOG_TRIVIAL(warning)
				<< "Warning: Bias not found in " << __FUNCTION__ << " for " << obs.Sat.id();
			}
		}

		PC = P1 - obs.sigs[f_1].biases[CODE];
	}

	range	= PC;
	measVar	= obs.sigs[f_1].codeVar;	//todo aaron, use combo?

	if (var1)
		biasVar = var1;

	return true;
}

/** Compute ionospheric corrections
*/
bool ionocorr(
	GTime		time,		///< Time
	VectorPos&	pos,		///< Receiver position in LLH
	AzEl&		azel,		///< Azimuth and elevation
	E_IonoMode	ionoMode,	///< Ionospheric correction model
	double&		dion,		///< Ionospheric delay (L1) value output
	double&		var)		///< Ionospheric delay (L1) variance output
{
// 	trace(4, "ionocorr: time=%s opt=%d sat=%s pos=%.3f %.3f azel=%.3f %.3f\n",
// 			time.to_string(3).c_str(),
// 			ionoopt,
// 			id,
// 			pos.latDeg(),
// 			pos.lonDeg(),
// 			azel[0]	*R2D,
// 			azel[1]	*R2D);

	if (ionoMode == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
	{
		dion	= 0;
		var		= 0;

		return true;
	}

	/* broadcast model */
	if (ionoMode == +E_IonoMode::BROADCAST)
	{
		E_Sys			sys		= E_Sys::GPS;
		E_NavMsgType	type	= defNavMsgType[sys];

		auto ion_ptr = seleph<ION>(std::cout, time, sys, type, nav);

		double* vals = nullptr;
		if (ion_ptr != nullptr)
			vals = ion_ptr->vals;

		dion	= ionmodel(time, vals, pos, azel);
		var		= SQR(dion * ERR_BRDCI);

		return true;
	}


	dion	= 0;
	var		= SQR(ERR_ION);

	/* tmp fix : KH
	if (ionoopt ==  E_IonoMode::TOTAL_ELECTRON_CONTENT)
	{
		int res = iontec(time, &nav, pos, azel, 1, dion, var);
		if (!res)
		{
			dion = 0;
			var =	SQR(ERR_ION);
		}
		return res;
	}
	*/
	//if (ionoopt != E_IonoMode::OFF)	fprintf(stderr,"SPP not unsupporting ionosphere mode %s", ionoopt._to_string());

	return true;
}



/** Validate Dilution of Precision of solution
*/
bool validateDOP(
	Trace&		trace,				///< Trace file to output to
	ObsList&	obsList,			///< List of observations for this epoch
	double		elevationMaskDeg,	///< Elevation mask
	Dops*     	dops_ptr = nullptr)	///< Optional pointer to output for DOP
{
	vector<AzEl> azels;
	azels.reserve(8);
	double dop[4] = {};
// 	tracepde(3, trace, "valsol  : n=%d nv=%d\n", obsList.size(), nv);

	// large gdop check
	for (auto& obs : only<GObs>(obsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		if (obs.sppValid == false)
			continue;

		auto& satStat = *obs.satStat_ptr;

		if (satStat.el < elevationMaskDeg * D2R)
		{
			continue;
		}

		azels.push_back(satStat);
	}

	Dops dops = dopCalc(azels);

	if (dops_ptr != nullptr)
	{
		*dops_ptr = dops;
	}

	if	( dops.gdop <= 0
		||dops.gdop >  acsConfig.sppOpts.max_gdop)
	{
		BOOST_LOG_TRIVIAL(info) << "DOP Validation failed with gdop = " << dops.gdop << " on " << obsList.front()->mount;

		return false;
	}

	return true;
}

void printFailures(
	const	string&		id,
			ObsList&	obsList)
{
	InteractiveTerminal ss(string("Failures/") + id, nullStream);

	tracepdeex(4, ss, "\nFailures:");
	tracepdeex(4, ss, "\n%20s ",""						);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%c", obs.Sat.sysChar()			);
	tracepdeex(4, ss, "\n%20s ",""						);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", obs.Sat.prn/10%10			);
	tracepdeex(4, ss, "\n%20s ",""						);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", obs.Sat.prn%10				);

	tracepdeex(4, ss, "\n%20s:","failExclude"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureExclude			);
	tracepdeex(4, ss, "\n%20s:","failNoSatPos"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureNoSatPos		);
	tracepdeex(4, ss, "\n%20s:","failNoSatClock"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureNoSatClock		);
	tracepdeex(4, ss, "\n%20s:","failNoPseudorange"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureNoPseudorange	);
	tracepdeex(4, ss, "\n%20s:","failIodeConsistency"	);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureIodeConsistency	);
	tracepdeex(4, ss, "\n%20s:","failBroadcastEph"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureBroadcastEph	);
	tracepdeex(4, ss, "\n%20s:","failRSat"				);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureRSat			);
	tracepdeex(4, ss, "\n%20s:","failSSRFail"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureSSRFail			);
	tracepdeex(4, ss, "\n%20s:","failSsrPosEmpty"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureSsrPosEmpty		);
	tracepdeex(4, ss, "\n%20s:","failSsrClkEmpty"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureSsrClkEmpty		);
	tracepdeex(4, ss, "\n%20s:","failSsrPosTime"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureSsrPosTime		);
	tracepdeex(4, ss, "\n%20s:","failSsrClkTime"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureSsrClkTime		);
	tracepdeex(4, ss, "\n%20s:","failSsrPosMag"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureSsrPosMag		);
	tracepdeex(4, ss, "\n%20s:","failSsrClkMag"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureSsrClkMag		);
	tracepdeex(4, ss, "\n%20s:","failSsrPosUdi"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureSsrPosUdi		);
	tracepdeex(4, ss, "\n%20s:","failSsrClkUdi"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureSsrClkUdi		);
	tracepdeex(4, ss, "\n%20s:","failGeodist"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureGeodist			);
	tracepdeex(4, ss, "\n%20s:","failElevation"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureElevation		);
	tracepdeex(4, ss, "\n%20s:","failPrange"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failurePrange			);
	tracepdeex(4, ss, "\n%20s:","failIonocorr"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.failureIonocorr		);
	tracepdeex(4, ss, "\n%20s:","excludeElevation"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.excludeElevation		);
	tracepdeex(4, ss, "\n%20s:","excludeEclipse"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.excludeEclipse			);
	tracepdeex(4, ss, "\n%20s:","excludeSystem"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.excludeSystem			);
	tracepdeex(4, ss, "\n%20s:","excludeOutlier"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.excludeOutlier			);
	tracepdeex(4, ss, "\n%20s:","excludeBadSPP"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.excludeBadSPP			);
	tracepdeex(4, ss, "\n%20s:","excludeConfig"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.excludeConfig			);
	tracepdeex(4, ss, "\n%20s:","excludeSVH"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.excludeSVH				);
	tracepdeex(4, ss, "\n%20s:","excludeBadRange"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, ss, "%d", (bool)obs.excludeBadRange		);

	tracepdeex(4, ss, "\n\n");

	BOOST_LOG_TRIVIAL(debug) << ss.str();
}












void removeUnmeasuredStates(
	Trace&				trace,				///< Trace to output to
	KFState&			kfState, 			///< Filter to remove states from
	KFMeasEntryList&	kfMeasEntryList)	///< List of measurements for this filter iteration
{
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type == +KF::ONE)
		{
			continue;
		}

		bool found = false;

		for (auto& measEntry : kfMeasEntryList)
		{
			auto it = measEntry.designEntryMap.find(key);
			if (it != measEntry.designEntryMap.end())
			{
				found = true;
				break;
			}
		}

		if (found)
		{
			continue;
		}

		kfState.removeState(key);
	}
}

/** Estimate receiver position and biases using code measurements
*/
E_Solution estpos(
	Trace&		trace,					///< Trace file to output to
	ObsList&	obsList,				///< List of observations for this epoch
	Solution&	sol,					///< Solution object containing initial conditions and results
	string		id,						///< Id of receiver
	KFState*	kfState_ptr = nullptr,	///< Optional kfstate pointer to retrieve ppp values from
	string		description = "SPP")	///< Description to prepend to clarify outputs
{
	int numMeas = 0;

	if (obsList.empty())
	{
		return E_Solution::NONE;
	}

	auto& kfState = sol.sppState;
	if (acsConfig.sppOpts.always_reinitialise)
	{
		kfState = KFState();		//reset to zero to prevent lock-in of bad positions
	}

	kfState.FilterOptions::operator=(acsConfig.sppOpts);

	int iter;
	int removals = 0;
	double adjustment = 10000;

	auto& recOpts = acsConfig.getRecOpts(id);

	tracepdeex(5, trace, "\n ---- STARTING SPP LSQ ----");
	for (iter = 0; iter < acsConfig.sppOpts.max_lsq_iterations; iter++)
	{
		tracepdeex(5, trace, "\nSPP It: %d", iter);
		kfState.initFilterEpoch();

		Vector3d	rRec	= Vector3d::Zero();
		double		dtRec	= 0;

		KFKey recPosKeys[3];
		KFKey recSysBiasKey	= {KF::REC_SYS_BIAS};

		for (short i = 0; i < 3; i++)
		{
			recPosKeys[i].type	= KF::REC_POS;
			recPosKeys[i].num	= i;
			recPosKeys[i].str	= id;

			kfState.getKFValue(recPosKeys[i],	rRec(i));
		}

		VectorPos pos = ecef2pos(rRec);

		if (pos.hgt() > 60'000'000)
		{
			tracepdeex(5, trace, "\nSPP found unfeasible position with height: %f", pos.hgt());
			return E_Solution::NONE;
		}

		KFMeasEntryList kfMeasEntryList;

		for (auto& obs : only<GObs>(obsList))
		{
			if (obs.exclude)
			{
				obs.failureExclude = true;

				tracepdeex(5, trace, "\n%s spp exclusion : %x", obs.Sat.id().c_str(), obs.exclude);
				continue;
			}

			recSysBiasKey.Sat	= SatSys(obs.Sat.sys);
			recSysBiasKey.str	= id;

			SatStat& satStat = *obs.satStat_ptr;

			KFMeasEntry codeMeas(&kfState);

			kfState.getKFValue(recSysBiasKey, dtRec);

			obs.sppValid = false;

			Vector3d rSat = obs.rSatApc;
			if (rSat.isZero())
			{
				obs.failureRSat = true;

				continue;
			}


// 			rSat += dtRec / CLIGHT * obs.satVel;

			double r = geodist(rSat, rRec, satStat.e);
			int debuglvl = 2;

			// psudorange with code bias correction
			double range;
			double vMeas;
			double vBias;
			int pass = prange(trace, obs, acsConfig.sppOpts.iono_mode, range, vMeas, vBias, kfState_ptr);
			if (pass == false)
			{
				obs.failurePrange = true;

				continue;
			}

			if (r <= 0)
			{
				obs.failureGeodist = true;

				tracepdeex(debuglvl, trace, "\n %s Geodist fail", obs.Sat.id().c_str());
				continue;
			}
			else
			{
				tracepdeex(debuglvl, trace, "\nSPP sat, %s, %14.3f, %12.3f", obs.Sat.id().c_str(), r, -CLIGHT * obs.satClk);
			}

			if (obs.ephPosValid == false)
			{
				obs.failureNoSatPos = true;

				tracepdeex(debuglvl, trace, " ... Eph Pos fail");
				continue;
			}

// 			if (obs.ephClkValid == false)
// 			{
// 				obs.failureNoSatClock = true;
//
// 				tracepdeex(debuglvl, trace, " ... Eph clk fail");
// 				continue;
// 			}

			tracepdeex(debuglvl, trace, ", %14.3f", range);

			satazel(pos, satStat.e, satStat);

			if	(  satStat.el < recOpts.elevation_mask_deg * D2R
				&& adjustment < 1000)
			{
				obs.failureElevation = true;

				tracepdeex(debuglvl, trace, " ... Elevation_mask fail");

				continue;
			}

			tracepdeex(debuglvl, trace, ", %6.2f", satStat.el * R2D);

			// ionospheric corrections
			double dion;
			double vion;
			pass = ionocorr(obs.time, pos, satStat, acsConfig.sppOpts.iono_mode, dion, vion);
			if (pass == false)
			{
				obs.failureIonocorr = true;

				tracepdeex(debuglvl, trace, " ... Ion fail");
				continue;
			}

			// GPS-L1 -> L1/B1
			if (obs.Sat.sys == +E_Sys::GLO)		{	double lamG1 = obs.satNav_ptr->lamMap[G1];		if (lamG1 > 0)		dion *= SQR(lamG1 * FREQ1 / CLIGHT);	}
			if (obs.Sat.sys == +E_Sys::BDS)		{	double lamB1 = obs.satNav_ptr->lamMap[B1];		if (lamB1 > 0)		dion *= SQR(lamB1 * FREQ1 / CLIGHT);	}

			tracepdeex(debuglvl, trace, ", %9.5f", dion);

			// tropospheric corrections
			double vtrp;
			double dryZTD;
			double dryMap;
			double wetZTD;
			double wetMap;

			double dtrp = tropSAAS(trace, obs.time, pos, satStat.el, dryZTD, dryMap, wetZTD, wetMap, vtrp);

			tracepdeex(debuglvl, trace, ", %9.5f", dtrp);

			// pseudorange residual
			double expected	= r
							+ dtRec
							- obs.satClk * CLIGHT
							+ dion
							+ dtrp;

			double res = range - expected;

			for (short i = 0; i < 3; i++)
			{
				codeMeas.addDsgnEntry(recPosKeys[i],	-satStat.e[i]);
			}
			{
// 				codeMeas.addDsgnEntry(recClockKey,		1);
			}
// 			if (recSysBiasKey.num != recClockKey.num)
			{
				codeMeas.addDsgnEntry(recSysBiasKey,	1);
			}

			// error variance
			if (acsConfig.sppOpts.iono_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
				vMeas *= 3;

			double var	= vMeas
						+ obs.satClkVar
						+ obs.posVar
						+ vBias
						+ vion
						+ vtrp;

			var *= SQR(acsConfig.sppOpts.sigma_scaling);

			codeMeas.obsKey.Sat = obs.Sat;

			codeMeas.setValue(res);
			codeMeas.setNoise(var);

			codeMeas.metaDataMap["obs_ptr"] = (void*) &obs;

			kfMeasEntryList.push_back(codeMeas);

			obs.sppValid		= true;			//todo aaron, this is messy, lots of excludes dont work if spp not run, harmonise the spp/ppp exclusion methods.
			obs.sppCodeResidual	= res;
		}

		//force reinitialisation of everything by least squares
		kfState.P.setIdentity();
		kfState.P *= -1;

		removeUnmeasuredStates(trace, kfState, kfMeasEntryList);

		//use state transition to initialise states
		kfState.stateTransition(trace, obsList.front()->time);

		//combine the measurement list into a single matrix
		numMeas = kfMeasEntryList.size();
		KFMeas kfMeas(kfState, kfMeasEntryList);

		if	( numMeas < kfMeas.H.cols() - 1
			||numMeas == 0)
		{
			BOOST_LOG_TRIVIAL(info)
			<< __FUNCTION__ << ": lack of valid measurements ns=" << numMeas
			<< " on " << id << " after " << iter << " iterations."
			<< " (has " << obsList.size() << " total observations)";

			printFailures(id, obsList);

			tracepdeex(5, trace, "\nlack of valid measurements, END OF SPP LSQ");
			return E_Solution::NONE;
		}

		if	( rRec.isZero()
			&&iter == 0)
		{
			kfMeas.Y /= 2;
		}

		VectorXd dx;
		kfState.leastSquareInitStates(trace, kfMeas, true, &dx);

		if (traceLevel >= 4)
		{
			kfMeas.V = kfMeas.Y;
			outputResiduals(trace, kfMeas, iter, (string)"/" + description, 0, kfMeas.H.rows());
		}
		adjustment = dx.norm();
		tracepdeex(4, trace, "\nSPP dx: %15.4f\n", adjustment);


		if	( adjustment < 4000
			&&acsConfig.sppOpts.postfitOpts.sigma_check
			&&removals < acsConfig.sppOpts.postfitOpts.max_iterations)
		{
			//use 'array' for component-wise calculations
			auto		measVariations		= kfMeas.Y.array().square();	//delta squared

			auto		measVariances		= kfMeas.R.diagonal().array().max(SQR(adjustment));

			// trace << measVariances.sqrt();

			ArrayXd		measRatios			= measVariations	/ measVariances;
						measRatios			= measRatios.isFinite()	.select(measRatios,		0);

			//if any are outside the expected values, flag an error

			Eigen::ArrayXd::Index measIndex;

// 			std::cout << "\nmeasRatios\n" << measRatios;

			double maxMeasRatio		= measRatios	.maxCoeff(&measIndex);

			if	(maxMeasRatio > SQR(acsConfig.sppOpts.postfitOpts.meas_sigma_threshold))
			{
				trace << "\n" << "LARGE MEAS  ERROR OF " << maxMeasRatio << " AT " << measIndex << " : " << kfMeas.obsKeys[measIndex];

				GObs& badObs = *(GObs*) kfMeas.metaDataMaps[measIndex]["obs_ptr"];

				badObs.excludeOutlier = true;
				continue;
			}
		}

		if (adjustment < 1E-4)
		{
			double dtRec_m = 0;
			kfState.getKFValue({KF::REC_SYS_BIAS, SatSys(E_Sys::GPS), id}, dtRec_m);

			sol.numMeas	= numMeas;
			sol.sppTime	= obsList.front()->time - dtRec_m / CLIGHT;

			if (traceLevel >= 4)
			{
				kfState.outputStates(trace, (string)"/" + description);
			}

			if (kfState.chiSquareTest.enable)
			{
				double a = sqrt( kfState.P(1,1) + kfState.P(2,2) + kfState.P(3,3)	) * kfState.chi / kfState.dof;
				double b = sqrt( kfState.P(4,4)										) * kfState.chi / kfState.dof;

				tracepdeex(4, trace, "chi2stats: chi = %10f\n",							kfState.chi);
				tracepdeex(4, trace, "chi2stats: dof = %10f\n",							kfState.dof);
				tracepdeex(5, trace, "chi2stats: sqrt(var_pos) * chi^2/dof = %10f\n",	a);
				tracepdeex(5, trace, "chi2stats: sqrt(var_dclk)* chi^2/dof = %10f\n",	b);

				if (kfState.chiQCPass == false)
				{
					tracepdeex(4, trace, " - Bad chiQC");

					return E_Solution::SINGLE_X;
				}
			}

			bool dopPass = validateDOP(trace, obsList, recOpts.elevation_mask_deg, &sol.dops);
			if (dopPass == false)
			{
				tracepdeex(4, trace, " - Bad DOP %f", sol.dops.gdop);

				return E_Solution::SINGLE_X;
			}

			return E_Solution::SINGLE;
		}
	}
	tracepdeex(5, trace, "\n ---- END OF SPP LSQ, iterations = %d ----", iter);

	if (iter >= acsConfig.sppOpts.max_lsq_iterations)
	{
		BOOST_LOG_TRIVIAL(debug) << "SPP failed to converge after " << iter << " iterations for " << id << " - " << description;
	}

	return E_Solution::NONE;
}

/** Receiver autonomous integrity monitoring (RAIM) failure detection and exclution
*/
bool raim(
	Trace&		trace,		///< Trace file to output to
	ObsList&	obsList,	///< List of observations for this epoch
	Solution&	sol,		///< Solution object containing initial conditions and results
	string		id,			///< Id of receiver
	KFState*	kfState_ptr = nullptr)
{
	trace << "\n" << "Performing RAIM.";

	SatSys exSat;
	double bestRms = 100;

	map<SatSys, SatStat> satStatBak;
	map<SatSys, SatStat> satStatBest;

	auto backupSatStats = [&](map<SatSys, SatStat>& dest, bool backup)
	{
		for (auto& obs : only<GObs>(obsList))
		{
			if (obs.exclude)
			{
				continue;
			}

			if (backup)		dest[obs.Sat]		= *obs.satStat_ptr;
			else			*obs.satStat_ptr	= dest[obs.Sat];
		}
	};

	backupSatStats(satStatBak, true);

	for (auto& testObs : only<GObs>(obsList))
	{
		if (testObs.exclude)
		{
			continue;
		}

		map<SatSys, SatStat> satStatBak;

		backupSatStats(satStatBak, false);

		ObsList testList;

		//push a list of everything thats not the test observation
		for (auto& obs : only<GObs>(obsList))
		{
			if (&obs == &testObs)		{	continue;	}
			if (obs.exclude)			{	continue;	}

			testList.push_back((shared_ptr<GObs>)obs);
		}

		Solution sol_e = sol;

		//avoid lock-in for raim despite config
		sol_e.sppState = KFState();

		//try to get position using test subset of all observations
		E_Solution status = estpos(trace, testList, sol_e, id, kfState_ptr, (string)"RAIM/" + id + "/" + testObs.Sat.id());
		if (status != +E_Solution::SINGLE)
		{
			continue;
		}

		int		numSat	= 0;
		double	testRms	= 0;

		for (auto& obs : only<GObs>(testList))
		{
			if (obs.sppValid == false)
				continue;

			testRms += SQR(obs.sppCodeResidual);
			numSat++;
		}

		testRms = sqrt(testRms / numSat);

		if (numSat < 5)
		{
			tracepdeex(3, trace, "%s: exsat=%s lack of satellites numSat=%2d\n", __FUNCTION__, testObs.Sat.id().c_str(), numSat);
			continue;
		}

		tracepdeex(3, trace, "%s: exsat=%s rms=%8.3f\n", __FUNCTION__, testObs.Sat.id().c_str(), testRms);

		if (testRms > bestRms)
		{
			//this solution is worse
			continue;
		}

		// copy test obs to real result
		for (auto& testObs : only<GObs>(testList))
		for (auto& origObs : only<GObs>(obsList))
		{
			if (testObs.Sat != origObs.Sat)
			{
				//only use the equivalent obs in the real list according to the test list
				continue;
			}

			origObs.sppValid		= testObs.sppValid;
			origObs.sppCodeResidual	= testObs.sppCodeResidual;
		}

		backupSatStats(satStatBest, true);

		sol					= sol_e;
		sol.status			= E_Solution::SINGLE;
		exSat				= testObs.Sat;
		bestRms				= testRms;
		testObs.sppValid	= false;
	}

	if ((int) exSat)
	{
		backupSatStats(satStatBest, false);

		tracepdeex(3, trace, "\n%s: %s excluded by RAIM", obsList.front()->time.to_string().c_str(), exSat.id().c_str());
		BOOST_LOG_TRIVIAL(debug) << "SPP converged after " << exSat.id() << " was excluded for " << obsList.front()->mount;

		return true;
	}

	return false;
}

/** Compute receiver position, velocity, clock bias by single-point positioning with pseudorange observables
*/
void spp(
	Trace&		trace,			///< Trace file to output to
	ObsList&	obsList,		///< List of observations for this epoch
	Solution&	sol,			///< Solution object containing initial state and results
	string		id,				///< Id of receiver
	KFState*	kfState_ptr,	///< Optional pointer to filter to take ephemerides from
	KFState*	remote_ptr)		///< Optional pointer to filter to take ephemerides from
{
	if (obsList.empty())
	{
		BOOST_LOG_TRIVIAL(info) <<  "SPP failed due to no observation data on " << id;

		sol.status = E_Solution::NONE;

		return;
	}

	for (auto& obs : only<GObs>(obsList))
	{
		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			continue;
		}

		auto& satOpts = acsConfig.getSatOpts(obs.Sat);

		satPosClk(trace, obs.time, obs, nav, satOpts.posModel.sources, satOpts.clockModel.sources, kfState_ptr, remote_ptr, E_OffsetType::APC);
	}

	tracepdeex(3,trace,	"\n%s  : tobs=%s n=%zu", __FUNCTION__, obsList.front()->time.to_string().c_str(), obsList.size());

	//estimate receiver position with pseudorange
	sol.status = estpos(trace, obsList, sol, id, kfState_ptr, (string) "SPP/" + id);	//todo aaron, remote too?

	if (sol.status != +E_Solution::SINGLE)
	{
		trace << "\n" << "Spp error with " << sol.numMeas << " measurements.";

		//Receiver Autonomous Integrity Monitoring
		if	( sol.numMeas >= 6		//need 6 so that 6-1 is still overconstrained, otherwise they all pass equally.
			&&acsConfig.sppOpts.raim)
		{
			raim(trace, obsList, sol, id);
		}
	}

	if	(sol.status != +E_Solution::SINGLE)
	{
		BOOST_LOG_TRIVIAL(debug)	<< "SPP failed for " << id;
		trace						<< "SPP failed for " << id;
	}

	//set observations that were valid
	for (auto& obs : only<GObs>(obsList))
	{
		if	( sol.status != +E_Solution::SINGLE
			&&sol.status != +E_Solution::SINGLE_X)
		{
			//all measurements are bad if we cant get spp
			// printf("\n all spp bad");
			obs.excludeBadSPP = true;
			continue;
		}

		if	( obs.exclude
			||obs.sppValid)
		{
			continue;
		}

		// printf("\n %s spp bad", obs.Sat.id().c_str());
		obs.excludeBadSPP = true;
	}

	sol.sppState.outputStates(trace, "/SPP/" + id);

	//copy states to often-used vectors
	for (short i = 0; i	< 3; i++)							{										sol.sppState.getKFValue({KF::REC_POS,		{},		id, i},	sol.sppRRec[i]);	}
	for (short i = E_Sys::GPS; i < +E_Sys::SUPPORTED; i++)	{	E_Sys sys = E_Sys::_values()[i];	sol.sppState.getKFValue({KF::REC_SYS_BIAS,	{sys},	id},	sol.dtRec_m[sys]);	}

	tracepdeex(3, trace, "\n%s  sol: %f %f %f",	__FUNCTION__, sol.sppRRec[0], sol.sppRRec[1], sol.sppRRec[2]);
	tracepdeex(3, trace, "\n%s  clk: %f\n", 	__FUNCTION__, sol.dtRec_m[E_Sys::GPS]);
}

