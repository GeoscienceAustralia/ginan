
#include <unordered_map>
#include <algorithm>

#include "eigenIncluder.hpp"

#include "streamTrace.hpp"
#include "corrections.hpp"
#include "acsConfig.hpp"
#include "testUtils.hpp"
#include "constants.h"
#include "algebra.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "trop.h"


#define MAXITR      10          /* max number of iteration for point pos */
#define ERR_ION     5.0         /* ionospheric delay std (m) */
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
	if (satNav.eph_ptr == NULL)
	{
		return 0;
	}
	return CLIGHT * satNav.eph_ptr->tgd[0];
}

/** Calculate pseudorange with code bias correction
*/
bool	prange(
	Obs&		obs,		///< Observation to calculate pseudorange for
	int			iter,		///< Iteration number (allows extra tests when elevation values are initialised)
	int			ionomode,	///< Ionospheric correction mode
	double&		range,		///< Pseudorange value output
	double&		var)		///< Pseudorange variance output
{
	SatNav&		satNav	= *obs.satNav_ptr;
	SatStat&	satStat	= *obs.satStat_ptr;
	auto&		lam		= satNav.lamMap;

	range	= 0;
	var		= 0;

	int sys = obs.Sat.sys;
	if (sys == E_Sys::NONE)
	{
		return false;
	}

	E_FType f_1 = F1;
	E_FType f_2 = F2;

	/* L1-L2 for GPS/GLO/QZS, L1-L5 for GAL/SBS */
	if  ( sys == E_Sys::GAL
		||sys == E_Sys::SBS)
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
	double P1_P2	= satNav.cBias_P1_P2;
	double P1_C1	= satNav.cBiasMap[F1];
	double P2_C2	= satNav.cBiasMap[F2];

	/* if no P1-P2 DCB, use TGD instead */
	if (P1_P2 == 0
		&&(sys == E_Sys::GPS
		|| sys == E_Sys::GAL
		|| sys == E_Sys::QZS))
	{
		P1_P2 = (1 - gamma) * gettgd(satNav);
	}

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
// 		if (obs.code[i] == CODE_L1C)
// 			P1 += P1_C1; /* C1->P1 */
		PC = P1 - P1_P2 / (1 - gamma);
	}

	if (acsConfig.ppp_ephemeris == +E_Ephemeris::SBAS)
	{

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
	double&	ion,		///< Ionospheric delay (L1) value output
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

	/* broadcast model */
	if (ionoopt == E_IonoMode::BROADCAST)
	{
		ion = ionmodel(time, nav.ion_gps, pos, azel);
		var = SQR(ion * ERR_BRDCI);
		return 1;
	}

	ion = 0;

	if (ionoopt == E_IonoMode::OFF)	var = SQR(ERR_ION);
	else							var = 0;

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
	Trace&		trace,			///< Trace file to output to
	ObsList&	obsList)		///< List of observations for this epoch
{
	vector<double> azels;
	azels.reserve(16);
	double dop[4];

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
	Solution&	sol)			///< Solution object containing initial conditions and results
{
	TestStack ts(__FUNCTION__);

	int numMeas = 0;

	KFState& kfState = sol.sppState;
	kfState = KFState();		//reset to zero to prevent lock-in of bad positions

	int iter;
	for (iter = 0; iter < MAXITR; iter++)
	{
		kfState.initFilterEpoch();

		Vector3d	rRec	= Vector3d::Zero();
		double		dtRec	= 0;

		KFKey recPosKeys[3];
		KFKey recClockKey;
		KFKey recSysBiasKey;

		for (short i = 0; i < 3; i++)
		{
			recPosKeys[i].type	= KF::REC_POS;
			recPosKeys[i].num	= i;

			kfState.getKFValue(recPosKeys[i],	rRec(i));
		}

		double pos[3];
		ecef2pos(rRec, pos);

		KFMeasEntryList kfMeasEntryList;

		for (auto& obs : obsList)
		{
			if (obs.exclude)
			{
				continue;
			}

			SatStat& satStat = *obs.satStat_ptr;

			KFMeasEntry codeMeas(&kfState);

			{
				recClockKey.type	= KF::REC_SYS_BIAS;
				recClockKey.num		= obs.Sat.biasGroup();

				kfState.getKFValue(recClockKey,		dtRec);
			}
			obs.vsat = false;

			// geometric distance/azimuth/elevation angle
			double r = geodist(obs.rSat, rRec, satStat.e);
			if (r <= 0)
			{
				continue;
			}

			double el = satazel(pos, satStat.e.data(), obs.satStat_ptr->azel);
			if (el < acsConfig.elevation_mask)
			{
				continue;
			}

			if (satexclude(obs.Sat, obs.svh))
			{
				continue;
			}

			// psudorange with code bias correction
			double range;
			double vmeas;
			int pass = prange(obs, iter, acsConfig.ionoOpts.corr_mode, range, vmeas);
// 			int pass = prange(obs, iter, E_IonoMode::BROADCAST, range, vmeas);
			if (pass == false)
			{
				continue;
			}

			// ionospheric corrections
			int ionopt;
			double dion;
			double vion;
			if (rRec.norm() > 0)	ionopt = acsConfig.ionoOpts.corr_mode;
			else					ionopt = E_IonoMode::BROADCAST;
			pass = ionocorr(obs.time, pos, obs.satStat_ptr->azel, ionopt, dion, vion);
			if (pass == false)
			{
				printf("Ion Fail\n");
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
				printf("Trop Fail\n");
				continue;
			}

			// pseudorange residual
			double expected	= r
							+ dtRec
							- CLIGHT * obs.dtSat[0]
							+ dion
							+ dtrp;

			double res = range - expected;

			{
				recSysBiasKey.type	= KF::REC_SYS_BIAS;
				recSysBiasKey.num	= obs.Sat.biasGroup();
			}

			for (short i = 0; i < 3; i++)
			{
				codeMeas.addDsgnEntry(recPosKeys[i],	-satStat.e[i]);
			}
			{
				codeMeas.addDsgnEntry(recClockKey,		1);
				codeMeas.addDsgnEntry(recSysBiasKey,	1);
			}

			// error variance
			double varEl = obs.Sigs.begin()->second.codeVar;
			double var	= varEl
						+ obs.var
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
		kfState.stateTransition(trace, GTime::noTime());

		//combine the measurement list into a single matrix
		numMeas = kfMeasEntryList.size();
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		if	( (numMeas < combinedMeas.A.cols() - 1)
			||(numMeas == 0))

		{
			BOOST_LOG_TRIVIAL(info)
			<< "estpos(): lack of valid measurements ns=" << combinedMeas.A.rows()
			<< " on " << obsList.front().mount 
			<< " (has " << obsList.size() << " total observations)";
			return numMeas;
		}

		VectorXd dx;
		kfState.leastSquareInitStates(trace, combinedMeas, true, &dx);

		if (dx.norm() < 1E-4)
		{
			if (kfState.chiQCPass == false)
			{
				return numMeas;
			}

			if (validateDOP(trace, obsList) == false)
			{
				return numMeas;
			}

			//copy states to often-used vectors
			for (short i = 0; i < 3;				i++)	{	kfState.getKFValue({KF::REC_POS,		{}, "", i}, sol.sppRRec[i]);	}
			for (short i = 0; i < BiasGroup::NUM;	i++)	{	kfState.getKFValue({KF::REC_SYS_BIAS,	{}, "", i}, sol.dtRec_m[i]);	}

			sol.numSats	= numMeas;
			sol.stat	= SOLQ_SINGLE;
			sol.time	= timeadd(obsList.front().time, -sol.dtRec_m[0] / CLIGHT);

			return 0;
		}
	}

	if (iter >= MAXITR)
	{
		BOOST_LOG_TRIVIAL(info) << "estpos(): iteration divergent iter=" << iter << " on " << obsList.front().mount;
	}

	return numMeas;
}

/** Receiver autonomous integrity monitoring (RAIM) failure detection and exclution
*/
void raim_fde(
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
			if (&obs == &testObs)
			{
				continue;
			}

			if (obs.exclude)
			{
				continue;
			}

			testList.push_back(obs);
		}

		Solution sol_e = sol;

		//try to get position using test subset of all observations
		int error = estpos(trace, testList, sol_e);
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
			tracepde(3, std::cout, "raim_fde: exsat=%2d lack of satellites nvsat=%2d\n", testObs.Sat, nvsat);
			continue;
		}
		rms_e = sqrt(rms_e / nvsat);

		tracepde(3, trace, "raim_fde: exsat=%2d rms=%8.3f\n", testObs.Sat, rms_e);

		if (rms_e > rms_min)
			continue;

		/* save result */
		for (auto& obs : testList)
		{
			auto originalObs = std::find_if(obsList.begin(), obsList.end(), [&](Obs& origObs){return origObs.Sat == obs.Sat;});
			originalObs->vsat		= obs.vsat;
			originalObs->rescode_v	= obs.rescode_v;
		}

		sol		= sol_e;
		exSat	= testObs.Sat;
		rms_min	= rms_e;
		testObs.vsat = 0;
	}

	if ((int) exSat)
	{
		char tstr[32];
		time2str(obsList.front().time, tstr, 2);
		tracepde(3, trace, "%s: %s excluded by raim\n", tstr + 11, exSat.id().c_str());
	}
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
		BOOST_LOG_TRIVIAL(info) <<  "sppos(): no observation data";
	}

	tracepde(3,trace,	"sppos  : tobs=%s n=%zu\n", obsList.front().time.to_string(3).c_str(), obsList.size());

	sol.time = obsList.front().time;

	//satellite positons, velocities and clocks
	satposs(trace, sol.time, obsList, nav, E_Ephemeris::BROADCAST);

	//estimate receiver position with pseudorange
	int numMeas = estpos(trace, obsList, sol);
	int error = numMeas;

	//Receiver Autonomous Integrity Monitoring
	if	( error
		&&numMeas >= 6
		&&acsConfig.raim)
	{
		raim_fde(trace, obsList, sol);
	}

	//set observations that were valid
	for (auto& obs : obsList)
	{
		if (sol.stat != SOLQ_SINGLE)
		{
			//all measurements are bad if we cant get spp
			obs.excludeBadSPP = true;
		}
		
		if	( obs.exclude
			||obs.vsat)
		{
			continue;
		}

		obs.excludeBadSPP = true;
	}

	TestStack::testMat("sol.sppRRec", sol.sppRRec);
	tracepde(3, trace,    "\nsppos  sol: %f %f %f", sol.sppRRec[0], sol.sppRRec[1], sol.sppRRec[2]);
	tracepde(3, trace,    "\nsppos  clk: %f\n", 	sol.dtRec_m[0]);
}

