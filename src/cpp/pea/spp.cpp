
// #pragma GCC optimize ("O0")

#include <algorithm>
#include <string>
#include <math.h>

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
int validateDOP(
	Trace&		trace,					///< Trace file to output to
	ObsList&	obsList,				///< List of observations for this epoch
	double		elevationMask,
	double*     dopout = nullptr)		///< Optional pointer to output for DOP
{
	vector<double> azels;
	azels.reserve(16);
	double dop[4] = {};
// 	tracepde(3, trace, "valsol  : n=%d nv=%d\n", obsList.size(), nv);

	// large gdop check
	int ns = 0;
	for (auto& obs : only<GObs>(obsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		if (obs.vsat == false)
			continue;

		azels.push_back(obs.satStat_ptr->az);
		azels.push_back(obs.satStat_ptr->el);
		ns++;
	}

	dops(ns, azels.data(), elevationMask, dop);

	if (dopout != nullptr)
	{
		for(int i=0; i<4; i++)
			dopout[i] = dop[i];
	}

	if	( dop[0] <= 0
		||dop[0] > acsConfig.sppOpts.max_gdop)
	{
		BOOST_LOG_TRIVIAL(info) << "DOP Validation failed with gdop = " << dop[0] << " on " << obsList.front()->mount;
		return 0;
	}
	return 1;
}

void printFailures(
	ObsList&	obsList)
{
	tracepdeex(4, std::cout, "\nFailures:");
	tracepdeex(4, std::cout, "\n%20s ",""					);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%c", obs.Sat.sysChar()			);
	tracepdeex(4, std::cout, "\n%20s ",""					);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", obs.Sat.prn/10%10			);
	tracepdeex(4, std::cout, "\n%20s ",""					);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", obs.Sat.prn%10				);

	tracepdeex(4, std::cout, "\n%20s:","failExclude"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureExclude			);
	tracepdeex(4, std::cout, "\n%20s:","failNoSatPos"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureNoSatPos		);
	tracepdeex(4, std::cout, "\n%20s:","failNoSatClock"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureNoSatClock		);
	tracepdeex(4, std::cout, "\n%20s:","failNoPseudorange"	);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureNoPseudorange	);
	tracepdeex(4, std::cout, "\n%20s:","failIodeConsistency");		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureIodeConsistency	);
	tracepdeex(4, std::cout, "\n%20s:","failBroadcastEph"	);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureBroadcastEph	);
	tracepdeex(4, std::cout, "\n%20s:","failRSat"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureRSat			);
	tracepdeex(4, std::cout, "\n%20s:","failSSRFail"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSSRFail			);
	tracepdeex(4, std::cout, "\n%20s:","failSsrPosEmpty"	);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosEmpty		);
	tracepdeex(4, std::cout, "\n%20s:","failSsrClkEmpty"	);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkEmpty		);
	tracepdeex(4, std::cout, "\n%20s:","failSsrPosTime"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosTime		);
	tracepdeex(4, std::cout, "\n%20s:","failSsrClkTime"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkTime		);
	tracepdeex(4, std::cout, "\n%20s:","failSsrPosMag"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosMag		);
	tracepdeex(4, std::cout, "\n%20s:","failSsrClkMag"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkMag		);
	tracepdeex(4, std::cout, "\n%20s:","failSsrPosUdi"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosUdi		);
	tracepdeex(4, std::cout, "\n%20s:","failSsrClkUdi"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkUdi		);
	tracepdeex(4, std::cout, "\n%20s:","failGeodist"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureGeodist			);
	tracepdeex(4, std::cout, "\n%20s:","failElevation"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureElevation		);
	tracepdeex(4, std::cout, "\n%20s:","failPrange"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failurePrange			);
	tracepdeex(4, std::cout, "\n%20s:","failIonocorr"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureIonocorr		);
	tracepdeex(4, std::cout, "\n%20s:","excludeElevation"	);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.excludeElevation		);
	tracepdeex(4, std::cout, "\n%20s:","excludeEclipse"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.excludeEclipse			);
	tracepdeex(4, std::cout, "\n%20s:","excludeSystem"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.excludeSystem			);
	tracepdeex(4, std::cout, "\n%20s:","excludeOutlier"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.excludeOutlier			);
	tracepdeex(4, std::cout, "\n%20s:","excludeBadSPP"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.excludeBadSPP			);
	tracepdeex(4, std::cout, "\n%20s:","excludeConfig"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.excludeConfig			);
	tracepdeex(4, std::cout, "\n%20s:","excludeSVH"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.excludeSVH				);
	tracepdeex(4, std::cout, "\n%20s:","excludeBadRange"	);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.excludeBadRange		);

	tracepdeex(4, std::cout, "\n\n");
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
	string		description = "SPP-")	///< Description to prepend to clarify outputs
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

	auto& recOpts = acsConfig.getRecOpts(id);

	int iter;
	int removals = 0;
	double adjustment = 10000;

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

			obs.vsat = false;

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

			double el = satazel(pos, satStat.e, *obs.satStat_ptr);
			if	(  el < recOpts.elevation_mask_deg * D2R
				&& adjustment < 10000)
			{
				obs.failureElevation = true;

				tracepdeex(debuglvl, trace, " ... Elevation_mask fail");

				continue;
			}

			tracepdeex(debuglvl, trace, ", %6.2f", el * R2D);

			// ionospheric corrections
			double dion;
			double vion;
			pass = ionocorr(obs.time, pos, *obs.satStat_ptr, acsConfig.sppOpts.iono_mode, dion, vion);
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

			double dtrp = tropSAAS(trace, obs.time, pos, obs.satStat_ptr->el, dryZTD, dryMap, wetZTD, wetMap, vtrp);

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

			obs.vsat		= true;			//todo aaron, this is messy, lots of excludes dont work if spp not run, harmonise the spp/ppp exclusion methods.
			obs.rescode_v	= res;
		}

		//force reinitialisation of everything by least squares
		kfState.P.setIdentity();
		kfState.P *= -1;

		removeUnmeasuredStates(trace, kfState, kfMeasEntryList);

		//use state transition to initialise states
		kfState.stateTransition(trace, obsList.front()->time);

		//combine the measurement list into a single matrix
		numMeas = kfMeasEntryList.size();
		KFMeas kfMeas = kfState.combineKFMeasList(kfMeasEntryList);

		if	( numMeas < kfMeas.H.cols() - 1
			||numMeas == 0)
		{
			BOOST_LOG_TRIVIAL(info)
			<< __FUNCTION__ << ": lack of valid measurements ns=" << numMeas
			<< " on " << id << " after " << iter << " iterations."
			<< " (has " << obsList.size() << " total observations)";

			printFailures(obsList);

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

		if (trace_level >= 4)
		{
			kfMeas.V = kfMeas.Y;
			outputResiduals(trace, kfMeas, iter, (string)"/" + description, 0, kfMeas.H.rows());
		}
		adjustment = dx.norm();
		tracepdeex(4, trace, "\nSPP dx: %15.4f\n", adjustment);
		if (adjustment < 1E-4)
		{
			if	( acsConfig.sppOpts.postfitOpts.sigma_check
				&&removals < acsConfig.sppOpts.postfitOpts.max_iterations)
			{
				//use 'array' for component-wise calculations
				auto		measVariations		= kfMeas.Y.array().square();	//delta squared

				auto		measVariances		= kfMeas.R.diagonal().array();

				ArrayXd		measRatios			= measVariations	/ measVariances;
							measRatios			= measRatios.isFinite()	.select(measRatios,		0);

				//if any are outside the expected values, flag an error

				Eigen::ArrayXd::Index measIndex;

	// 			std::cout << "\nmeasRatios\n" << measRatios;

				double maxMeasRatio		= measRatios	.maxCoeff(&measIndex);

				if	(maxMeasRatio > SQR(acsConfig.sppOpts.postfitOpts.sigma_threshold))
				{
					trace << std::endl << "LARGE MEAS  ERROR OF " << maxMeasRatio << " AT " << measIndex << " : " << kfMeas.obsKeys[measIndex];

					GObs& badObs = *(GObs*) kfMeas.metaDataMaps[measIndex]["obs_ptr"];

					badObs.excludeOutlier = true;
					continue;
				}
			}


			double dtRec_m = 0;
			kfState.getKFValue({KF::REC_SYS_BIAS, SatSys(E_Sys::GPS), id}, dtRec_m);

			sol.numMeas	= numMeas;
			sol.sppTime	= obsList.front()->time - dtRec_m / CLIGHT;

			double a = sqrt( kfState.P(1,1) + kfState.P(2,2) + kfState.P(3,3)	) * kfState.chi / kfState.dof;
			double b = sqrt( kfState.P(4,4)										) * kfState.chi / kfState.dof;

			tracepdeex(5, trace, "chi2stats: sqrt(var_pos) * chi^2/dof = %10f\n", a);
			tracepdeex(5, trace, "chi2stats: sqrt(var_dclk)* chi^2/dof = %10f\n", b);

			if (trace_level >= 4)
			{
				kfState.outputStates(trace, (string)"/" + description);
			}

			if (kfState.chiQCPass == false)
			{
				tracepdeex(4, trace, " - Bad chiQC");

				//too many (false) positives, announce but ignore
				return E_Solution::SINGLE_X;
			}

			if (validateDOP(trace, obsList, recOpts.elevation_mask_deg, sol.dop) == false)
			{
				tracepdeex(4, trace, " - Bad DOP %f", sol.dop[0]);
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
bool raim_fde(
	Trace&		trace,		///< Trace file to output to
	ObsList&	obsList,	///< List of observations for this epoch
	Solution&	sol,		///< Solution object containing initial conditions and results
	string		id,			///< Id of receiver
	KFState*	kfState_ptr = nullptr)
{
	double	rms_min	= 100;

	SatSys exSat = {};

	for (auto& testObs : only<GObs>(obsList))
	{
		if (testObs.exclude)
		{
			continue;
		}

		ObsList testList;

		//push a list of everything thats not the test observation
		for (auto& obs : only<GObs>(obsList))
		{
			if (&obs == &testObs)		{	continue;	}
			if (obs.exclude)			{	continue;	}

			testList.push_back((shared_ptr<GObs>)obs);
		}

		Solution sol_e = sol;

		//try to get position using test subset of all observations
		E_Solution status = estpos(trace, testList, sol_e, id, kfState_ptr, (string)"RAIM-" + testObs.Sat.id());
		if (status != +E_Solution::SINGLE)
		{
			continue;
		}

		int		nvsat = 0;
		double	rms_e = 0;

		for (auto& obs : only<GObs>(testList))
		{
			if (obs.vsat == false)
				continue;

			rms_e += SQR(obs.rescode_v);
			nvsat++;
		}

		if (nvsat < 5)
		{
			tracepdeex(3, std::cout, "%s: exsat=%s lack of satellites nvsat=%2d\n", __FUNCTION__, testObs.Sat.id().c_str(), nvsat);
			continue;
		}

		rms_e = sqrt(rms_e / nvsat);

		tracepdeex(3, trace, "%s: exsat=%s rms=%8.3f\n", __FUNCTION__, testObs.Sat.id().c_str(), rms_e);

		if (rms_e > rms_min)
			continue;

		/* save result */
		for (auto& testObs : only<GObs>(testList))
		for (auto& origObs : only<GObs>(obsList))
		{
			if (testObs.Sat != origObs.Sat)
			{
				continue;
			}

			origObs.vsat		= testObs.vsat;
			origObs.rescode_v	= testObs.rescode_v;
		}

		sol				= sol_e;
		sol.status		= E_Solution::SINGLE;
		exSat			= testObs.Sat;
		rms_min			= rms_e;
		testObs.vsat	= 0;
	}

	if ((int) exSat)
	{
		tracepdeex(3, trace, "%s: %s excluded by raim", obsList.front()->time.to_string(2).c_str(), exSat.id().c_str());
		BOOST_LOG_TRIVIAL(debug) << "SPP converged after " << exSat.id() << " was excluded for " << obsList.front()->mount;
		return true;
	}

	return false;
}

/** Compute receiver position, velocity, clock bias by single-point positioning with pseudorange observables
*/
void SPP(
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

	tracepdeex(3,trace,	"\n%s  : tobs=%s n=%zu", __FUNCTION__, obsList.front()->time.to_string(3).c_str(), obsList.size());

	//estimate receiver position with pseudorange
	sol.status = estpos(trace, obsList, sol, id, kfState_ptr);	//todo aaron, remote too?

	//Receiver Autonomous Integrity Monitoring
	if (sol.status != +E_Solution::SINGLE)
	{
		trace << std::endl << "Spp error with " << sol.numMeas << " measurements.";

		if	( sol.numMeas >= 6		//need 6 so that 6-1 is still overconstrained, otherwise they all pass equally.
			&&acsConfig.sppOpts.raim)
		{
			trace << " Performing RAIM." << std::endl;
			raim_fde(trace, obsList, sol, id);
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
// 			printf("\n all spp bad");
			obs.excludeBadSPP = true;
			continue;
		}

		if	( obs.exclude
			||obs.vsat)
		{
			continue;
		}

// 		printf("\n %s spp bad", obs.Sat.id().c_str());
		obs.excludeBadSPP = true;
	}

	sol.sppState.outputStates(trace, "/SPP");

	//copy states to often-used vectors
	for (short i = 0; i	< 3; i++)							{										sol.sppState.getKFValue({KF::REC_POS,		{},		id, i},	sol.sppRRec[i]);	}
	for (short i = E_Sys::GPS; i < +E_Sys::SUPPORTED; i++)	{	E_Sys sys = E_Sys::_values()[i];	sol.sppState.getKFValue({KF::REC_SYS_BIAS,	{sys},	id},	sol.dtRec_m[sys]);	}

	tracepdeex(3, trace,    "\n%s  sol: %f %f %f",	__FUNCTION__, sol.sppRRec[0], sol.sppRRec[1], sol.sppRRec[2]);
	tracepdeex(3, trace,    "\n%s  clk: %f\n", 		__FUNCTION__, sol.dtRec_m[E_Sys::GPS]);
}

