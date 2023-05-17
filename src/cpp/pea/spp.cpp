
// #pragma GCC optimize ("O0")

#include <algorithm>
#include <string>
#include <math.h>    

#include "eigenIncluder.hpp"
#include "corrections.hpp"
#include "coordinates.hpp"
#include "ephPrecise.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "testUtils.hpp"
#include "constants.hpp"
#include "biasSINEX.hpp"
#include "algebra.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "trop.h"


#define MAXITR		10			///< max number of iteration for point pos 
#define ERR_ION		7.0			///< ionospheric delay std (m) 
#define ERR_TROP	3.0			///< tropspheric delay std (m) 
#define ERR_SAAS	0.3			///< saastamoinen model error std (m) 
#define ERR_BRDCI	0.5			///< broadcast iono model error factor 
#define ERR_CBIAS	0.3			///< code bias error std (m) 
#define REL_HUMI	0.7			///< relative humidity for saastamoinen model 


/** Calculate pseudorange with code bias correction
*/
bool	prange(
	Trace&		trace,		///< Trace file to output to
	GObs&		obs,		///< Observation to calculate pseudorange for
	int			ionomode,	///< Ionospheric correction mode
	double&		range,		///< Pseudorange value output
	double&		measVar,	///< Pseudorange variance output
	double&		biasVar)	///< Bias variance output
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

	E_FType f_1 = E_FType::FTYPE_NONE;
	E_FType f_2;
	
	for (auto& code : acsConfig.code_priorities[sys])
	{
		if (f_1 == E_FType::FTYPE_NONE)
		{
			f_1 = code2Freq[sys][code];
			continue;
		}
		E_FType ft = code2Freq[sys][code]; //todo aaron this reproduces something that happens in acsQC, store and reuse.
		if (ft != f_1)
		{
			f_2 = ft;
			break;
		}
	}

	if (acsConfig.ionoOpts.iflc_freqs==+E_LinearCombo::L1L5_ONLY) 
	{
		f_1 = F1;
		f_2 = F5;
	}

	if	( obs.Sigs[f_1].P	== 0
		|| lam[f_1]			== 0)
	{
		return false;
	}
	
	double B1	= 0; 
	double var1	= 0;	 
	getBiasSinex(trace, obs.time, obs.Sat.id(), obs.Sat, obs.Sigs[f_1].code, CODE, B1, var1);
	double P1	= obs.Sigs[f_1].P - B1;

	double PC = 0;
	if (ionomode == E_IonoMode::IONO_FREE_LINEAR_COMBO) /* dual-frequency */
	{
		if	(  obs.Sigs[f_2].P	== 0
			|| lam[f_2]			== 0)
		{
			return false;
		}

		double gamma= SQR(lam[f_2]) / SQR(lam[f_1]); /* f1^2/f2^2 */
		double B2	= 0;
		double var2 = 0;	
		getBiasSinex(trace, obs.time, obs.Sat.id(), obs.Sat, obs.Sigs[f_2].code, CODE, B2, var2);
		double P2	= obs.Sigs[f_2].P - B2;

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
		
		PC = P1 - obs.Sigs[f_1].biases[CODE];
	}

	range	= PC;
	measVar	= obs.Sigs[f_1].codeVar;	//todo aaron, use combo?

	if (var1)
		biasVar = var1;
	
	return true;
}

/** Compute ionospheric corrections
*/
bool ionocorr(
	GTime		time,		///< Time
	VectorPos&	pos,		///< Receiver position in LLH
	double*		azel,		///< Azimuth and elevation
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

/** Compute tropospheric corrections
*/
int tropcorr(
	GTime		time,		///< Time
	VectorPos&	pos,		///< Receiver position in LLH
	double*		azel,		///< Azimuth and elevation
	double&		trp,		///< Tropospheric value output
	double&		var)		///< Tropospheric variance output
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

		if (!obs.vsat)
			continue;
		azels.push_back(obs.satStat_ptr->az);
		azels.push_back(obs.satStat_ptr->el);
		ns++;
	}

	dops(ns, azels.data(), acsConfig.elevation_mask, dop);

	if (dopout != nullptr)
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

void printFailures(
	ObsList&	obsList)
{
	tracepdeex(4, std::cout, "\nFailures:");
	tracepdeex(4, std::cout, "\n%20s ",""					);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%c", obs.Sat.sysChar()			);
	tracepdeex(4, std::cout, "\n%20s ",""					);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", obs.Sat.prn/10%10			);
	tracepdeex(4, std::cout, "\n%20s ",""					);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", obs.Sat.prn%10				);
	                                                                                                                                 
	tracepdeex(4, std::cout, "\n%20s:","Exclude"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureExclude			);
	tracepdeex(4, std::cout, "\n%20s:","NoSatPos"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureNoSatPos		);
	tracepdeex(4, std::cout, "\n%20s:","NoSatClock"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureNoSatClock		);
	tracepdeex(4, std::cout, "\n%20s:","NoPseudorange"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureNoPseudorange	);
	tracepdeex(4, std::cout, "\n%20s:","IodeConsistency"	);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureIodeConsistency	);
	tracepdeex(4, std::cout, "\n%20s:","BroadcastEph"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureBroadcastEph	);
	tracepdeex(4, std::cout, "\n%20s:","Seleph"				);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSeleph			);
	tracepdeex(4, std::cout, "\n%20s:","SSRFail"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSSRFail			);
	tracepdeex(4, std::cout, "\n%20s:","SsrPosEmpty"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosEmpty		);
	tracepdeex(4, std::cout, "\n%20s:","SsrClkEmpty"		);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkEmpty		);
	tracepdeex(4, std::cout, "\n%20s:","SsrPosTime"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosTime		);
	tracepdeex(4, std::cout, "\n%20s:","SsrClkTime"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkTime		);
	tracepdeex(4, std::cout, "\n%20s:","SsrPosMag"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosMag		);
	tracepdeex(4, std::cout, "\n%20s:","SsrClkMag"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkMag		);
	tracepdeex(4, std::cout, "\n%20s:","SsrPosUdi"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosUdi		);
	tracepdeex(4, std::cout, "\n%20s:","SsrClkUdi"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkUdi		);
	tracepdeex(4, std::cout, "\n%20s:","Geodist"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureGeodist			);
	tracepdeex(4, std::cout, "\n%20s:","Elevation"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureElevation		);
	tracepdeex(4, std::cout, "\n%20s:","Satexclude"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureSatexclude		);
	tracepdeex(4, std::cout, "\n%20s:","Prange"				);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failurePrange			);
	tracepdeex(4, std::cout, "\n%20s:","Ionocorr"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureIonocorr		);
	tracepdeex(4, std::cout, "\n%20s:","Tropcorr"			);		for (auto& obs : only<GObs>(obsList))	tracepdeex(4, std::cout, "%d", (bool)obs.failureTropcorr		);
    
	tracepdeex(4, std::cout, "\n\n");
}


void removeUnmeasuredStates(
	Trace&				trace,				///< Trace to output to
	KFState&			kfState, 			///< Filter to remove states from
	KFMeasEntryList&	kfMeasEntryList)
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
	string		description = "SPP-")
{
	int numMeas = 0;

	if (obsList.empty())
	{
		return E_Solution::NONE;
	}
	
	auto& kfState = sol.sppState;
	kfState = KFState();		//reset to zero to prevent lock-in of bad positions
	
	int iter;
    tracepdeex (5, trace, "\n ---- STARTING SPP LSQ ----");
	for (iter = 0; iter < MAXITR; iter++)
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
				tracepdeex(5, trace, "\n%s spp exclusion : %x", obs.Sat.id().c_str(), obs.exclude); 
				continue;
			}
			
			recSysBiasKey.Sat	= SatSys(obs.Sat.sys);
			recSysBiasKey.str	= id;

			SatStat& satStat = *obs.satStat_ptr;

			KFMeasEntry codeMeas(&kfState);

			kfState.getKFValue(recSysBiasKey, dtRec);
			
			obs.vsat = false;

			Vector3d rSat = obs.rSat;
			
// 			rSat += dtRec / CLIGHT * obs.satVel;
			
			// geometric distance/azimuth/elevation angle
			double r = geodist(rSat, rRec, satStat.e);
			int debuglvl = 2;
			
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
			
			if	( obs.ephPosValid == false
// 				||obs.ephClkValid == false
				)
			{
				obs.failureSatexclude = true;
				
				tracepdeex(debuglvl, trace, " ... SVH fail");
				continue;
			}

			// psudorange with code bias correction
			double range;
			double vMeas;
			double vBias;
			int pass = prange(trace, obs, acsConfig.ionoOpts.corr_mode, range, vMeas, vBias);
			if (pass == false)
			{
				obs.failurePrange = true;
				
				continue;
			}

			tracepdeex(debuglvl, trace, ", %14.3f", range);
			
			double el = satazel(pos, satStat.e, obs.satStat_ptr->azel);
			if (el < acsConfig.elevation_mask)
			{
				obs.failureElevation = true;
				
				tracepdeex(debuglvl, trace, " ... Elevation_mask fail");
				
				continue;
			}

			tracepdeex(debuglvl, trace, ", %6.2f", el * R2D);
			
			// ionospheric corrections
			double dion;
			double vion;
			pass = ionocorr(obs.time, pos, obs.satStat_ptr->azel, acsConfig.ionoOpts.corr_mode, dion, vion);
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
			double dtrp;
			double vtrp;
			pass = tropcorr(obs.time, pos, obs.satStat_ptr->azel, dtrp, vtrp);
			if (pass == false)
			{
				obs.failureTropcorr = true;
				
				tracepdeex(debuglvl, trace, " ... Trop fail");
				continue;
			}

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
			if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO) 
				vMeas *= 3; 
			
			double var	= vMeas
						+ obs.satClkVar
						+ obs.posVar
						+ vBias
						+ vion
						+ vtrp;
						
			auto& recOpts = acsConfig.getRecOpts(obs.mount);
			
			var *= SQR(recOpts.spp_sigma_scaling);

			codeMeas.obsKey.Sat = obs.Sat;
			
			codeMeas.setValue(res);
			codeMeas.setNoise(var);

			kfMeasEntryList.push_back(codeMeas);

			obs.vsat		= true;
			obs.rescode_v	= res;
		}

		//force reinitialisation of everything by least squares (by setting P to zero)
		kfState.P.setZero();

		removeUnmeasuredStates(trace, kfState, kfMeasEntryList);
		
		//use state transition to initialise states
		kfState.stateTransition(trace, obsList.front()->time);

		//combine the measurement list into a single matrix
		numMeas = kfMeasEntryList.size();
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		if	( numMeas < combinedMeas.H.cols() - 1
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
		
		
		VectorXd dx;
		kfState.leastSquareInitStates(trace, combinedMeas, true, &dx);

		if (trace_level >= 4)
		{
			combinedMeas.V = combinedMeas.Y;
			outputResiduals(trace, combinedMeas, iter, (string)"/" + description, 0, combinedMeas.H.rows());
		}
		
		tracepdeex(4, trace, "\nSPP dx: %15.4f\n", dx.norm());
		if (dx.norm() < 1E-4)
		{
			double dtRec_m = 0;
			kfState.getKFValue({KF::REC_SYS_BIAS, SatSys(E_Sys::GPS), id}, dtRec_m);
			
			sol.numMeas	= numMeas;
			sol.time	= obsList.front()->time - dtRec_m / CLIGHT;

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

			if (validateDOP(trace, obsList, sol.dop) == false)
			{
				tracepdeex(4, trace, " - Bad DOP %f", sol.dop[0]);
				return E_Solution::SINGLE_X;
			}
			
			return E_Solution::SINGLE;
		}
	}
    tracepdeex (5, trace, "\n ---- END OF SPP LSQ, iterations = %d ----", iter);

	if (iter >= MAXITR)
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
	string		id)			///< Id of receiver
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
		E_Solution status = estpos(trace, testList, sol_e, id, (string)"RAIM-" + testObs.Sat.id());
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
			tracepdeex(3, std::cout, "raim_fde: exsat=%s lack of satellites nvsat=%2d\n", testObs.Sat.id().c_str(), nvsat);
			continue;
		}
		
		rms_e = sqrt(rms_e / nvsat);

		tracepdeex(3, trace, "raim_fde: exsat=%s rms=%8.3f\n", testObs.Sat.id().c_str(), rms_e);

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
void sppos(
	Trace&		trace,			///< Trace file to output to
	ObsList&	obsList,		///< List of observations for this epoch
	Solution&	sol,			///< Solution object containing initial state and results
	string		id)				///< Id of receiver
{
	if (obsList.empty())
	{
		BOOST_LOG_TRIVIAL(info) <<  "SPP failed due to no observation data on " << id;
		
		sol.status = E_Solution::NONE;
	
		return;
	}

	tracepdeex(3,trace,	"\n%s  : tobs=%s n=%zu", __FUNCTION__, obsList.front()->time.to_string(3).c_str(), obsList.size());

	//estimate receiver position with pseudorange
	sol.status = estpos(trace, obsList, sol, id);
	
	//Receiver Autonomous Integrity Monitoring
	if (sol.status != +E_Solution::SINGLE)
	{
		trace << std::endl << "Spp error with " << sol.numMeas << " measurements.";
		
		if	( sol.numMeas >= 6		//need 6 so that 6-1 is still overconstrained, otherwise they all pass equally.
			&&acsConfig.raim)
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

