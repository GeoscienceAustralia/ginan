
// #pragma GCC optimize ("O0")

#include <algorithm>
#include <string>
#include <math.h>    

#include "eigenIncluder.hpp"
#include "streamTrace.hpp"
#include "corrections.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "testUtils.hpp"
#include "constants.hpp"
#include "biasSINEX.hpp"
#include "algebra.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "trop.h"


#define MAXITR      10          /* max number of iteration for point pos */
#define ERR_ION     7.0         /* ionospheric delay std (m) */
#define ERR_TROP    3.0         /* tropspheric delay std (m) */
#define ERR_SAAS    0.3         /* saastamoinen model error std (m) */
#define ERR_BRDCI   0.5         /* broadcast iono model error factor */
#define ERR_CBIAS   0.3         /* code bias error std (m) */
#define REL_HUMI    0.7         /* relative humidity for saastamoinen model */


/** get tgd parameter (m)
*/
double gettgd(
	SatNav&		satNav)		///< Satellite navigation object
{
	if (satNav.eph_ptr == nullptr)
	{
		return 0;
	}
	return CLIGHT * satNav.eph_ptr->tgd[0];
}

/** Calculate pseudorange with code bias correction
*/
bool	prange(
	Trace&		trace,		///< Trace file to output to
	Obs&		obs,		///< Observation to calculate pseudorange for
	int			iter,		///< Iteration number (allows extra tests when elevation values are initialised)
	int			ionomode,	///< Ionospheric correction mode
	double&		range,		///< Pseudorange value output
	double&		var)		///< Pseudorange variance output
{
	SatNav&		satNav	= *obs.satNav_ptr;
	auto&		lam		= satNav.lamMap;

	range	= 0;
	var		= 0;

	E_Sys sys = obs.Sat.sys;
	if (sys == +E_Sys::NONE)
	{
		return false;
	}

	E_FType f_1 = F1;
	E_FType f_2 = F2;

	/* L1-L2 for GPS/GLO/QZS, L1-L5 for GAL/SBS */
	if  ( sys == +E_Sys::GAL
		||sys == +E_Sys::SBS
		||(sys == +E_Sys::GPS && acsConfig.ionoOpts.iflc_freqs==+E_LinearCombo::L1L5_ONLY))
	{
		f_2 = F5;
	}

	if	( lam[f_1] == 0
		||lam[f_2] == 0)
	{
		return false;
	}

	double gamma	= SQR(lam[f_2]) / SQR(lam[f_1]); /* f1^2/f2^2 */
	double P1		= obs.Sigs[f_1].P;
	double P2		= obs.Sigs[f_2].P;

	double PC = 0;
	if (ionomode == E_IonoMode::IONO_FREE_LINEAR_COMBO) /* dual-frequency */
	{
		if 	( P1 == 0
			||P2 == 0)
		{
			return false;
		}
// 		if (obs.code[i] == CODE_L1C) P1 += P1_C1; /* C1->P1 */
// 		if (obs.code[j] == CODE_L2C) P2 += P2_C2; /* C2->P2 */

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
		if (isnan(obs.Sigs[f_1].biases[CODE]))
		{
			bool pass = getBiasSinex(trace, obs.time, obs.Sat.id(), obs.Sat, obs.Sigs[f_1].code, E_ObsCode::NONE, CODE, obs.Sigs[f_1].biases[CODE], varP1);
			if (pass == false)
			{
				BOOST_LOG_TRIVIAL(warning)
				<< "Warning: Bias not found in " << __FUNCTION__ << " for " << obs.Sat.id();
			}
		}
		
// 		if (obs.code[i] == CODE_L1C)
// 			P1 += P1_C1; /* C1->P1 */
// 		PC = P1 - P1_P2 / (1 - gamma);
		PC = P1 - obs.Sigs[f_1].biases[CODE];
	}

	range	= PC;
	var		= SQR(ERR_CBIAS);

	return true;
}

/** Compute ionospheric corrections
*/
int ionocorr(
	GTime	time,		///< Time
	double*	pos,		///< Receiver position in LLH
	double*	azel,		///< Azimuth and elevation
	int		ionoopt,	///< Ionospheric correction model
	double&	dion,		///< Ionospheric delay (L1) value output
	double&	var)		///< Ionospheric delay (L1) variance output
{
// 	trace(4, "ionocorr: time=%s opt=%d sat=%s pos=%.3f %.3f azel=%.3f %.3f\n",
// 			time.to_string(3).c_str(),
// 			ionoopt,
// 			id,
// 			pos[0]	*R2D,
// 			pos[1]	*R2D,
// 			azel[0]	*R2D,
// 			azel[1]	*R2D);

	if (ionoopt == E_IonoMode::IONO_FREE_LINEAR_COMBO)
	{
		dion = 0;
		var = 0;
		return 1;
	}
	/* broadcast model */
	if (ionoopt == E_IonoMode::BROADCAST)
	{
		E_Sys sys = E_Sys::GPS;
		E_NavMsgType type = defNavMsgType[sys];
		
		ION* ion_ptr = seleph<ION>(std::cout, time, sys, type, nav);
		double* vals = nullptr;
		if (ion_ptr != nullptr)
			vals = ion_ptr->vals;
		dion = ionmodel(time, vals, pos, azel);
		var = SQR(dion * ERR_BRDCI);
		return 1;
	}

	
	dion = 0;
	var = SQR(ERR_ION);
	
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
	
	return 1;
}

/** Compute tropospheric corrections
*/
int tropcorr(
	GTime	time,		///< Time
	double*	pos,		///< Receiver position in LLH
	double*	azel,		///< Azimuth and elevation
	double&	trp,		///< Tropospheric value output
	double&	var)		///< Tropospheric variance output
{
// 	trace(4, "tropcorr: time=%s opt=%d pos=%.3f %.3f azel=%.3f %.3f\n",
// 			time.to_string(3).c_str(),
// 			tropopt,
// 			pos[0]*R2D,
// 			pos[1]*R2D,
// 			azel[0]*R2D,
// 			azel[1]*R2D);
	trp = tropmodel(time, pos, azel, REL_HUMI);
	var = SQR(ERR_SAAS / (sin(azel[1]) + 0.1));
	return 1;
}

/** Validate Dilution of Precision of solution
*/
int validateDOP(
	Trace&		trace,					///< Trace file to output to
	ObsList&	obsList,				///< List of observations for this epoch
	double*     dopout = nullptr)		///< Optional pointer to output for DOP
{
	vector<double> azels;
	azels.reserve(16);
	double dop[4]={0};
// 	tracepde(3, trace, "valsol  : n=%d nv=%d\n", obsList.size(), nv);

	// large gdop check
	int ns = 0;
	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		if (!obs.vsat)
			continue;
		azels.push_back(obs.satStat_ptr->az);
		azels.push_back(obs.satStat_ptr->el);
		ns++;
	}

	dops(ns, azels.data(), acsConfig.elevation_mask, dop);

	if(dopout != nullptr)
	{
		for(int i=0; i<4; i++)
			dopout[i] = dop[i];
	}
	
	if	( dop[0] <= 0
		||dop[0] > acsConfig.max_gdop)
	{
		BOOST_LOG_TRIVIAL(info) << "valsol(): gdop error gdop=" << dop[0];
		return 0;
	}
	return 1;
}

/** Estimate receiver position and biases using code measurements
*/
int estpos(
	Trace&		trace,			///< Trace file to output to
	ObsList&	obsList,		///< List of observations for this epoch
	Solution&	sol,			///< Solution object containing initial conditions and results
	bool		warn = true)	///< Option to warn about divergent solutions
{
	TestStack ts(__FUNCTION__);

	int numMeas = 0;
	
	string id = obsList.front().mount;

	KFState& kfState = sol.sppState;
	kfState = KFState();		//reset to zero to prevent lock-in of bad positions
	int iter;
	for (iter = 0; iter < MAXITR; iter++)
	{
		tracepdeex(5, trace, "\nSPP It: %d", iter);
		kfState.initFilterEpoch();

		Vector3d	rRec	= Vector3d::Zero();
		double		dtRec	= 0;

		KFKey recPosKeys[3];
// 		KFKey recClockKey	= {KF::REC_SYS_BIAS};
		KFKey recSysBiasKey	= {KF::REC_SYS_BIAS};

		for (short i = 0; i < 3; i++)
		{
			recPosKeys[i].type	= KF::REC_POS;
			recPosKeys[i].num	= i;
			recPosKeys[i].str	= id;

			kfState.getKFValue(recPosKeys[i],	rRec(i));
		}

		double pos[3];
		ecef2pos(rRec, pos);

		KFMeasEntryList kfMeasEntryList;

		for (auto& obs : obsList)
		{
			if (obs.exclude)
			{
// 				tracepdeex(5, trace, "\n%s spp exclusion : %x", obs.Sat.id().c_str(), obs.exclude); 
				continue;
			}
			
// 			recClockKey		.num = E_BiasGroup::GPS;
			recSysBiasKey	.num	= obs.Sat.biasGroup();
			recSysBiasKey	.str	= id;

			SatStat& satStat = *obs.satStat_ptr;

			KFMeasEntry codeMeas(&kfState);

			kfState.getKFValue(recSysBiasKey, dtRec);
			
			obs.vsat = false;

			// geometric distance/azimuth/elevation angle
			double r = geodist(obs.rSat, rRec, satStat.e);
			if (r <= 0)
			{
				tracepdeex(5, trace, "\n %s Geodist fail", obs.Sat.id().c_str());
				continue;
			}

			double el = satazel(pos, satStat.e.data(), obs.satStat_ptr->azel);
			if (el < acsConfig.elevation_mask)
			{
				tracepdeex(5, trace, "\n %s Elevation_mask fail", obs.Sat.id().c_str());
				continue;
			}

			if (satexclude(obs.Sat, obs.svh))
			{
				tracepdeex(5, trace, "\n %s SVH fail", obs.Sat.id().c_str());
				continue;
			}

			// psudorange with code bias correction
			double range;
			double vmeas;
			int pass = prange(trace, obs, iter, acsConfig.ionoOpts.corr_mode, range, vmeas);
// 			int pass = prange(trace, obs, iter, E_IonoMode::BROADCAST, range, vmeas);
			if (pass == false)
			{
				continue;
			}

			// ionospheric corrections
			double dion;
			double vion;
			pass = ionocorr(obs.time, pos, obs.satStat_ptr->azel, acsConfig.ionoOpts.corr_mode, dion, vion);
			if (pass == false)
			{
				tracepdeex(5, trace, "\n %s Ion fail", obs.Sat.id().c_str());
				continue;
			}

			// GPS-L1 -> L1/B1
			double lam_L1 = obs.satNav_ptr->lamMap[F1];
			if (lam_L1 > 0)
			{
				dion *= SQR(lam_L1 / lam_carr[F1]);
			}

			// tropospheric corrections
			double dtrp;
			double vtrp;
			pass = tropcorr(obs.time, pos, obs.satStat_ptr->azel, dtrp, vtrp);
			if (pass == false)
			{
				tracepdeex(5, trace, "\n %s Trop fail", obs.Sat.id().c_str());
				continue;
			}

			// pseudorange residual
			double expected	= r
							+ dtRec
							- CLIGHT * obs.dtSat[0]
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
			double varEl = obs.Sigs.begin()->second.codeVar;
			if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO) 
				varEl *= 3; 
			
			double var	= varEl
						+ obs.ephVar
						+ vmeas
						+ vion
						+ vtrp;

			codeMeas.setValue(res);
			codeMeas.setNoise(var);

			kfMeasEntryList.push_back(codeMeas);

			obs.vsat		= true;
			obs.rescode_v	= res;
		}

		//force reinitialisation of everything by least squares (by setting P to zero)
		kfState.P.setZero();

		//use state transition to initialise states
		kfState.stateTransition(trace, obsList.front().time);

		//combine the measurement list into a single matrix
		numMeas = kfMeasEntryList.size();
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		if	( numMeas < combinedMeas.H.cols() - 1
			||numMeas == 0)
		{
			BOOST_LOG_TRIVIAL(info)
			<< __FUNCTION__ << ": lack of valid measurements ns=" << combinedMeas.H.rows()
			<< " on " << id 
			<< " (has " << obsList.size() << " total observations)";
			
			return numMeas;
		}

		VectorXd dx;
		kfState.leastSquareInitStates(trace, combinedMeas, true, &dx);

		tracepdeex(5, trace, "\nSPP dx: %f\n", dx.norm());
		if (dx.norm() < 1E-4)
		{
			if (kfState.chiQCPass == false)
			{
				tracepdeex(5, trace, "\nBad chiQC");
				
				//too many (false) positives, announce but ignore
// 				return numMeas;
			}

			if (validateDOP(trace, obsList, sol.dop) == false)
			{
				tracepdeex(5, trace, "\nBad DOP %f", sol.dop[0]);
				return numMeas;
			}

			double dtRec_m = 0;
			kfState.getKFValue({KF::REC_SYS_BIAS,	{}, id, E_BiasGroup::GPS}, dtRec_m);
			
			sol.numSats	= numMeas;
			sol.stat	= SOLQ_SINGLE;
			sol.time	= obsList.front().time - dtRec_m / CLIGHT;

			return 0;
		}
	}

	if	(  iter >= MAXITR
		&& warn)
	{
		BOOST_LOG_TRIVIAL(debug) << "SPP failed to converge after " << iter << " iterations for " << id;
	}

	return numMeas;
}

/** Receiver autonomous integrity monitoring (RAIM) failure detection and exclution
*/
bool raim_fde(
	Trace&		trace,		///< Trace file to output to
	ObsList&	obsList,	///< List of observations for this epoch
	Solution&	sol)		///< Solution object containing initial conditions and results
{
	double	rms_min	= 100;

	SatSys exSat = {};

	for (auto& testObs : obsList)
	{
		if (testObs.exclude)
		{
			continue;
		}

		ObsList testList;

		//push a list of everything thats not the test observation
		for (auto& obs : obsList)
		{
			if (&obs == &testObs)		{	continue;	}
			if (obs.exclude)			{	continue;	}

			testList.push_back(obs);
		}

		Solution sol_e = sol;

		//try to get position using test subset of all observations
		int error = estpos(trace, testList, sol_e, false);
		if (error)
		{
			continue;
		}

		int		nvsat = 0;
		double	rms_e = 0;

		for (auto& obs : testList)
		{
			if (obs.vsat == false)
				continue;

			rms_e += SQR(obs.rescode_v);
			nvsat++;
		}
		
		if (nvsat < 5)
		{
			tracepdeex(3, std::cout, "raim_fde: exsat=%s lack of satellites nvsat=%2d\n", testObs.Sat.id().c_str(), nvsat);
			continue;
		}
		
		rms_e = sqrt(rms_e / nvsat);

		tracepdeex(3, trace, "raim_fde: exsat=%s rms=%8.3f\n", testObs.Sat.id().c_str(), rms_e);

		if (rms_e > rms_min)
			continue;

		/* save result */
		for (auto& obs : testList)
		{
			auto originalObs = std::find_if(obsList.begin(), obsList.end(), [&](Obs& origObs){return origObs.Sat == obs.Sat;});
			originalObs->vsat		= obs.vsat;
			originalObs->rescode_v	= obs.rescode_v;
		}

		sol				= sol_e;
		exSat			= testObs.Sat;
		rms_min			= rms_e;
		testObs.vsat	= 0;
	}

	if ((int) exSat)
	{
		char tstr[32];
		time2str(obsList.front().time, tstr, 2);
		tracepdeex(3, trace, "%s: %s excluded by raim\n", tstr + 11, exSat.id().c_str());
		BOOST_LOG_TRIVIAL(debug) << "SPP converged after " << exSat.id() << " was excluded for " << obsList.front().mount;
		return true;
	}
	
	return false;
}

/** Compute receiver position, velocity, clock bias by single-point positioning with pseudorange observables
*/
void sppos(
	Trace&		trace,			///< Trace file to output to
	ObsList&	obsList,		///< List of observations for this epoch
	Solution&	sol)			///< Solution object containing initial state and results
{
	TestStack ts(__FUNCTION__);

	sol.stat = SOLQ_NONE;
	int n_obs = obsList.size();
	if (n_obs <= 0)
	{
		BOOST_LOG_TRIVIAL(info) <<  "SPP failed due to no observation data";
		return;
	}

	tracepdeex(3,trace,	"\n%s  : tobs=%s n=%zu\n", __FUNCTION__, obsList.front().time.to_string(3).c_str(), obsList.size());

	//estimate receiver position with pseudorange
	int numMeas = estpos(trace, obsList, sol);
	int error = numMeas;

	//Receiver Autonomous Integrity Monitoring
	if (error)
	{
		trace << std::endl << "Spp error with " << numMeas << " measurements.";
		
		if	( numMeas >= 6		//need 6 so that 6-1 is still overconstrained, otherwise they all pass equally.
			&&acsConfig.raim)
		{
			trace << " Performing RAIM." << std::endl ;
			raim_fde(trace, obsList, sol);
		}
	}
	
	if	(sol.stat != SOLQ_SINGLE)
	{
		BOOST_LOG_TRIVIAL(debug) << "SPP failed for " << obsList.front().mount;
		trace << "SPP failed for " << obsList.front().mount;
	}

	//set observations that were valid
	for (auto& obs : obsList)
	{
		if (sol.stat != SOLQ_SINGLE)
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
	
	string id = obsList.front().mount;
	
	//copy states to often-used vectors
	for (short i = 0; i < 3;				i++)	{	sol.sppState.getKFValue({KF::REC_POS,		{}, id, i}, sol.sppRRec[i]);	}
	for (short i = 0; i < E_BiasGroup::NUM;	i++)	{	sol.sppState.getKFValue({KF::REC_SYS_BIAS,	{}, id, i}, sol.dtRec_m[i]);	}

	TestStack::testMat("sol.sppRRec", sol.sppRRec);
	tracepdeex(3, trace,    "\n%s  sol: %f %f %f",	__FUNCTION__, sol.sppRRec[0], sol.sppRRec[1], sol.sppRRec[2]);
	tracepdeex(3, trace,    "\n%s  clk: %f\n", 		__FUNCTION__, sol.dtRec_m[0]);
}

