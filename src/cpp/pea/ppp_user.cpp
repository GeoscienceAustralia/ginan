
#include "observations.hpp"
#include "streamTrace.hpp"
#include "linearCombo.hpp"
#include "corrections.hpp"
#include "navigation.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "constants.h"
#include "satStat.hpp"
#include "preceph.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "constants.h"
#include "antenna.hpp"
#include "common.hpp"
#include "wancorr.h"
#include "tides.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "vmf3.h"
#include "trop.h"

#include "eigenIncluder.hpp"

#define MIN_NSAT_SOL	4				// min satellite number for solution


#define VAR_CLK     	SQR(2)       	// init variance receiver clock (m^2)
#define VAR_DCB     	SQR(30.0)       // init variance dcb (m^2)
#define VAR_GLO_IFB 	SQR( 0.6)       // variance of glonass ifb

/* write solution status for PPP
*/
void pppoutstat(
	Trace&		trace,
	KFState&	kfState,
	bool		rts,
	int 		solStat,
	int			numSat)
{
	int week;
	double tow = time2gpst(kfState.time, &week);
//
// 	double* x = rtk->sol.stat == SOLQ_FIX ? rtk->xa : rtk->xx;
//
	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		KFKey key = kfKey;
		if	( key.type	== KF::REC_POS
			&&key.num	== 0)
		{
			double x[3]	= {};
			double v[3]	= {};

			for (key.num = 0; key.num < 3; key.num++)
			{
				kfState.getKFValue(key, x[key.num], &v[key.num]);
			}
			tracepdeex(1, trace, "\n$POS,%d,%.3f,%d,%.4f,%.4f,%.4f,%.7f,%.7f,%.7f\n",
						week,
						tow,
						solStat,
						x[0],
						x[1],
						x[2],
						sqrt(v[0]),
						sqrt(v[1]),
						sqrt(v[2]));
		}

		if (key.type == KF::PHASE_BIAS)
		{
			double phase_bias		= 0;
			double phase_biasVar	= 0;
			kfState.getKFValue(key, phase_bias, &phase_biasVar);
			tracepdeex(1, trace, "$AMB,%d,%.3f,%d,%s,%d,%.4f,%.7f\n",
						week,
						tow,
						solStat,
						key.Sat.id().c_str(),
						key.num,
						phase_bias,
						sqrt(phase_biasVar));
		}

		if (key.type == KF::TROP)
		{
			string grad = "";
			double trop		= 0;
			double tropVar	= 0;
			if (key.num == 1)	grad = "_N";
			if (key.num == 2)	grad = "_E";
			kfState.getKFValue(key, trop, &tropVar);
			tracepdeex(1, trace, "$TROP%s,%d,%.3f,%d,%d,%f,%.7f\n",
					grad.c_str(),
					week,
					tow,
					solStat,
					numSat,
					trop,
					sqrt(tropVar));
		}

		if	( key.type	== KF::REC_SYS_BIAS
			&&key.num	== SatSys(E_Sys::GPS).biasGroup())
		{
			double rClkGPS	= 0;
			double rClkGLO	= 0;
			double rClkGAL	= 0;
			double rClkBDS	= 0;
			double GPSclkVar= 0;
			double GLOclkVar= 0;
			double GALclkVar= 0;
			double BDSclkVar= 0;
			
			kfState.getKFValue(key, rClkGPS, &GPSclkVar);
			key.num = SatSys(E_Sys::GLO).biasGroup();
			kfState.getKFValue(key, rClkGLO, &GLOclkVar);
			key.num = SatSys(E_Sys::GAL).biasGroup();
			kfState.getKFValue(key, rClkGAL, &GALclkVar);
			key.num = SatSys(E_Sys::CMP).biasGroup();
			kfState.getKFValue(key, rClkBDS, &BDSclkVar);
			
			tracepdeex(1, trace, "$CLK,%d,%.3f,%d,%d,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f\n",
					week,
					tow,
					solStat,
					numSat,
					rClkGPS			* 1E9 / CLIGHT,
					rClkGLO			* 1E9 / CLIGHT,
					rClkGAL			* 1E9 / CLIGHT,
					rClkBDS			* 1E9 / CLIGHT,
					sqrt(GPSclkVar)	* 1E9 / CLIGHT,
					sqrt(GLOclkVar)	* 1E9 / CLIGHT,
					sqrt(GALclkVar)	* 1E9 / CLIGHT,
					sqrt(BDSclkVar)	* 1E9 / CLIGHT);
		}
	}
//
// 	/* receiver velocity and acceleration */
// 	{
// 		ecef2pos(rtk->sol.rr, pos);
// 		ecef2enu(pos, rtk->xx + 3, vel);
// 		ecef2enu(pos, rtk->xx + 6, acc);
// 		p += sprintf(p, "$VELACC,%d,%.3f,%d,%.4f,%.4f,%.4f,%.5f,%.5f,%.5f,%.4f,%.4f,"
// 		             "%.4f,%.5f,%.5f,%.5f\n", week, tow, rtk->sol.stat, vel[0], vel[1],
// 		             vel[2], acc[0], acc[1], acc[2], 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
// 	}
}

/* temporal update of phase biases -------------------------------------------*/
void udbias_ppp(
	Trace&		trace,
	rtk_t&		rtk,
	ObsList&	obsList)
{
	tracepde(3,trace, "udbias  : n=%d\n", obsList.size());

	//handle day-boundary clock jump
	bool clk_jump = false;
	if (acsConfig.clock_jump)
	{
		if (ROUND(time2gpst(obsList.front().time, NULL) * 10) % (60*60*24) == 0)
		{
			clk_jump = true;
		}
	}

	for (auto& [key, index] : rtk.pppState.kfIndexMap)
	{
		if (key.type != KF::PHASE_BIAS)
		{
			continue;
		}

		SatStat& satStat = rtk.satStatMap[key.Sat];
		SigStat& sigStat = satStat.sigStatMap[(E_FType)key.num];

		if (sigStat.userPhaseRejectCount >= acsConfig.pppOpts.phase_reject_limit)
		{
			sigStat.userPhaseRejectCount = 0;

			trace << "Removing " << key << " due to repeated rejections > " << acsConfig.pppOpts.phase_reject_limit << std::endl;
			rtk.pppState.removeState(key);
		}

		if	(clk_jump)
		{
			rtk.pppState.removeState(key);
		}

		sigStat.userPhaseOutageCount++;
		if (sigStat.userPhaseOutageCount == acsConfig.pppOpts.outage_reset_limit)
		{
			sigStat.userPhaseOutageCount = 0;

			trace << "Removing " << key << " due to extended outage > " << acsConfig.pppOpts.outage_reset_limit << std::endl;
			rtk.pppState.removeState(key);
		}
	}

	//Get sums and nums for averages of each frequency
	map<E_FType, double>	sumMap;
	map<E_FType, int>		numMap;

	for (auto& obs			: obsList)
	for (auto& [ft, sig]	: obs.Sigs)
	{
		if (obs.exclude)
		{
			continue;
		}

		if	( (acsConfig.ionoOpts.corr_mode == E_IonoMode::IONO_FREE_LINEAR_COMBO && ft != FTYPE_IF12)
			||(acsConfig.ionoOpts.corr_mode != E_IonoMode::IONO_FREE_LINEAR_COMBO && ft == FTYPE_IF12)
			||(sig.L_corr_m == 0)
			||(sig.P_corr_m == 0))
		{
			continue;
		}

		E_FType l = obs.Sat.sys == +E_Sys::GAL ? F5 : F2;

		double	bias	= 0;
		int		slip	= obs.satStat_ptr->sigStatMap[ft].slip.any;
		auto&	lam		= obs.satNav_ptr->lamMap;

		if		(acsConfig.ionoOpts.corr_mode == E_IonoMode::IONO_FREE_LINEAR_COMBO)
		{
			bias = sig.L_corr_m - sig.P_corr_m;
		}
		else if (acsConfig.ionoOpts.corr_mode == E_IonoMode::ESTIMATE)
		{
			if	( obs.Sigs[F1].P	== 0
				||obs.Sigs[l].P		== 0
				||lam[F1]			== 0
				||lam[l]			== 0
				||lam[ft]			== 0)
			{
				continue;
			}

			double ion = (obs.Sigs[F1].P - obs.Sigs[l].P) / (1 - SQR(lam[l] / lam[F1]));

			bias	= obs.Sigs[ft].L_corr_m
					- obs.Sigs[ft].P_corr_m
					+ 2 * ion * SQR(lam[ft] / lam[F1]);
		}
		else
		{
			if	( obs.Sigs[ft].P	== 0
				||obs.Sigs[ft].L	== 0
				||lam[ft]			== 0)
			{
				continue;
			}

			if (obs.satStat_ptr->extionovar <= 0)
			{
				continue;
			}

			double ion = obs.satStat_ptr->extiono;

			bias	= obs.Sigs[ft].L_corr_m
					- obs.Sigs[ft].P_corr_m
					+ 2 * ion * SQR(lam[ft] / lam[F1]);
		}

		KFKey phaseBiasKey;
		phaseBiasKey.type	= KF::PHASE_BIAS;
		phaseBiasKey.Sat	= obs.Sat;
		phaseBiasKey.num	= ft;
		phaseBiasKey.str	= obs.mount;

		if	( bias
			&&slip)
		{
			// reinitialize phase-bias if detecting cycle slip
			rtk.pppState.removeState(phaseBiasKey);

			tracepde(2, trace, "\nudbias_ppp: sat=%s bias=%.3f\n", obs.Sat.id().c_str(), bias);
			continue;
		}

		double oldBias;
		bool initialised = rtk.pppState.getKFValue(phaseBiasKey, oldBias);

		if	( initialised	== false
			||slip			!= false
			||bias			== 0)
		{
			continue;
		}

		sumMap[ft] += bias - oldBias;
		numMap[ft] ++;
	}

	for (auto& obs			: obsList)
	for (auto& [ft, sig]	: obs.Sigs)
	{
		if (obs.exclude)
		{
			continue;
		}

		double averageOffset = sumMap[ft] / numMap[ft];			//todo aaron, this probably needs bias group separation, not just frequency?

		/* correct phase-code jump to ensure phase-code coherency */
		if	( numMap[ft] >= 2
			&&fabs(averageOffset) > 0.0005 * CLIGHT)		//todo aaron, need define , half millisecond?
		{
			for (auto& [key, index] : rtk.pppState.kfIndexMap)
			{
				if (key.type == KF::PHASE_BIAS)
				{
					double oldBias;
					rtk.pppState.getKFValue(key, oldBias);

					double newBias = oldBias + averageOffset;
					rtk.pppState.setKFValue(key, newBias);
				}
			}

			tracepde(2,trace, "phase-code jump corrected: %s n=%2d dt=%12.9fs\n", rtk.sol.time.to_string(0).c_str(), numMap[ft], averageOffset / CLIGHT);
		}
	}
}



/* phase and code residuals --------------------------------------------------*/
int ppp_filter(
	Trace&		trace,
	ObsList&	obsList,
	Vector3d&	dr,
	rtk_t&		rtk,
	Station&	rec)
{
	TestStack ts(__FUNCTION__);

	KFState&	kfState = rtk.pppState;

	kfState.initFilterEpoch();

	char str[32];
	time2str(obsList.front().time, str, 2);

	int lv = 3;

	GTime obsTime = obsList.front().time;
	double ep[6];
	time2epoch(obsTime, ep);
	double jd	= ymdhms2jd(ep);
	double mjd	= jd - JD2MJD;

	auto& recOpts = acsConfig.getRecOpts(obsList.front().mount);

	//Get the previous estimate of position or load from the point positioning solution if not initialised.
	Vector3d x0 = Vector3d::Zero();
	bool sppInit = false;
	for (int i = 0; i < 3; i++)
	{
		KFKey xKey;
		xKey.type	= KF::REC_POS;
		xKey.str	= obsList.front().mount;
		xKey.num	= i;
		bool pass = kfState.getKFValue(xKey, x0[i]);
		if (pass == false)
		{
			selectAprioriSource(rec, sppInit);
			
			x0(i) = rec.aprioriPos(i);
		}
	}

	double pos[3];
	ecef2pos(x0.data(), pos);

	// update receiver position with tide and antenna delta
	Vector3d dr1;
	enu2ecef(pos, rtk.opt.antdel.data(), dr1.data());

	//remove elements of receiver position that we dont want included in the state estimate
	Vector3d rRec	= x0
					+ dr
					+ dr1;

	tracepdeex(lv, trace, "\n   *-------- Observed minus computed           --------*\n");

	pppCorrections(trace, obsList, rRec, rtk, rec);

	udbias_ppp(trace, rtk, obsList);

	KFMeasEntryList		kfMeasEntryList;
	map<KFKey, bool>	measuredStates;

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		TestStack ts(obs.Sat);

		int			sys			= obs.Sat.sys;
		auto&		lam			= obs.satNav_ptr->lamMap;
		SatStat&	satStat		= *(obs.satStat_ptr);
		int			biasGroup	= obs.Sat.biasGroup();

		E_FType ft2 = F2;
		if (obs.Sat.sys == +E_Sys::GAL)	ft2 = F5;
		double ionfact=SQR(lam[F1]) / (SQR(lam[F1])-SQR(lam[ft2]));
		
		//Satellite already has precise clocks if available
		double C_dtSat = CLIGHT * obs.dtSat[0];

		//try to get precicse clock for receiver//todo aaron, there is a comment about precise ephemeris in original code

		double precDtRec	= 0;
		double precDtRecVar	= 0;
		pephclk(rtk.sol.time, obs.mount, nav, &precDtRec, &precDtRecVar);
		if (precDtRec != 0)
		{
// 			C_dtRec = CLIGHT * precDtRec;
		}

		// tropospheric  model
		double dTrop			= 0;
		double varTrop			= 0;
		double tropStates[3]	= {};
		double dTropDx[3]		= {};

		//get the previous filter states for linearisation around this operating point
		for (short i = 0; i < 3; i++)
		{
			bool pass = kfState.getKFValue({KF::TROP, {}, obs.mount, i}, tropStates[i]);
			if	( (i	== 0)
				&&(pass == false))
			{
				double ztd = sbstropcorr(rtk.sol.time, rRec, PI/2);
				tropStates[i] = ztd;
			}
		}

		//calculate the trop values, variances, and gradients at the operating points
		if (recOpts.trop.estimate)
		{
			bool pass = model_trop(obs.time, pos, satStat.azel, tropStates,	dTropDx, 	dTrop, varTrop);
			if	(pass == false)
			{
				continue;
			}
		}

		// ionospheric model
		double dIono			= 0;
		double varIono			= 0;
		double ionoState		= 0;
		double ionInitValue		= 0;
		double ionInitVari		= -1;
		
		
		switch (acsConfig.ionoOpts.corr_mode)
		{
			case E_IonoMode::IONO_FREE_LINEAR_COMBO:
			case E_IonoMode::OFF:					 
				dIono   = 0; 
				varIono = 0;
				break;
			case E_IonoMode::ESTIMATE:
				if (obs.Sigs[F1].P_corr_m>0  &&  obs.Sigs[ft2].P_corr_m>0)	{
					ionInitValue = obs.Sigs[F1].P_corr_m - obs.Sigs[ft2].P_corr_m;		// code GF
					ionInitVari  = obs.Sigs[F1].codeVar  + obs.Sigs[ft2].codeVar;
					ionInitValue*= ionfact;												// from GF to slant Iono(F1)
					ionInitVari *= ionfact*ionfact;
					
				}
				if(kfState.getKFValue({KF::IONOSPHERIC, obs.Sat, obs.mount}, ionoState)) dIono = ionoState;
				else dIono = ionInitValue;
				varIono = 0;
				break;
			default:
				dIono	= satStat.extiono;
				varIono	= satStat.extionovar;
		}

		tracepde(lv, trace, "*---------------------------------------------------*\n");
		tracepde(lv, trace, " %.6f %s  recpos               = %14.4f %14.4f %14.4f\n",  mjd, obs.Sat.id().c_str(), rec.aprioriPos[0], rec.aprioriPos[1], rec.aprioriPos[2]);
		tracepde(lv, trace, " %.6f %s  tide                 = %14.4f %14.4f %14.4f\n",  mjd, obs.Sat.id().c_str(), dr(0), dr(1), dr(2));
		tracepde(lv, trace, " %.6f %s  recpos+tide+ant      = %14.4f %14.4f %14.4f\n",  mjd, obs.Sat.id().c_str(), rRec[0], rRec[1], rRec[2]);
		tracepde(lv, trace, " %.6f %s  sinexpos             = %14.4f %14.4f %14.4f\n",	mjd, obs.Sat.id().c_str(), rec.aprioriPos[0], rec.aprioriPos[1], rec.aprioriPos[2]);
		tracepde(lv, trace, " %.6f %s  satpos               = %14.4f %14.4f %14.4f\n",	mjd, obs.Sat.id().c_str(), obs.rSat[0], obs.rSat[1], obs.rSat[2]);
		tracepde(lv, trace, " %.6f %s  trop mf              = %14.4f\n",	         	mjd, obs.Sat.id().c_str(), dTropDx[0]);
		tracepde(lv, trace, " %.6f %s  trop                 = %14.4f\n",	         	mjd, obs.Sat.id().c_str(), dTrop);


		if(varIono<0) continue;
		
		
		for (auto& [ft, sig] : obs.Sigs)
		{
			TestStack ts(std::to_string(ft));

			SigStat& sigStat = satStat.sigStatMap[ft];

			if (acsConfig.ionoOpts.corr_mode != E_IonoMode::IONO_FREE_LINEAR_COMBO)
			{
				if	( ft == FTYPE_IF12
					||ft == FTYPE_IF15
					||ft == FTYPE_IF25)
				{
					continue;
				}
			}

			if (acsConfig.ionoOpts.corr_mode == E_IonoMode::IONO_FREE_LINEAR_COMBO)
			{
				/*if	( ft != FTYPE_IF12
					&&ft != FTYPE_IF15)
				{
					continue;
				}

				if	( (acsConfig.ionoOpts.iflc_freqs == E_LinearCombo::L1L2_ONLY && ft != FTYPE_IF12)
					||(acsConfig.ionoOpts.iflc_freqs == E_LinearCombo::L1L5_ONLY && ft != FTYPE_IF15))   
				{
					continue;
				}*/
				
				// Ken: It would be nice to be able to use both L1L2 and L1L5 combinations, but not if the correlation matrix cannot be introduced 
				if(sys == +E_Sys::GPS && ft != FTYPE_IF12) continue;
				if(sys == +E_Sys::GLO && ft != FTYPE_IF12) continue;
				if(sys == +E_Sys::GAL && ft != FTYPE_IF15) continue;
				if(sys == +E_Sys::CMP && ft != FTYPE_IF12) continue;		/* need to confirm this */
				if(sys == +E_Sys::QZS && ft != FTYPE_IF12) continue;
				
			}

			if (acsConfig.ionoOpts.corr_mode == E_IonoMode::TOTAL_ELECTRON_CONTENT)
			{
				if	(ft != F1)
				{
					continue;  // for now, vtec maps are only valid for F1 frequency
				}
			}

			if	( (sig.P_corr_m == 0)
				||(sig.L_corr_m == 0))
			{
				continue;
			}

			// Ionosphere dependent values
			double ionC;
			switch (acsConfig.ionoOpts.corr_mode)
			{
				case E_IonoMode::IONO_FREE_LINEAR_COMBO: ionC = 0; break;
				case E_IonoMode::OFF:					 ionC = 0; break;
				case E_IonoMode::ESTIMATE:				 ionC = SQR(lam[ft]/lam[F1]); break;
				default:								 ionC = SQR(lam[ft]/lam[F1]); break;
			}
			double  phaseBias = sig.L_corr_m - sig.P_corr_m + 2 * ionC * dIono;
					

			double varSysIFB = 0;
			if	(sys == E_Sys::GLO)
			{
				varSysIFB = VAR_GLO_IFB;
			}


			//Do some kalman filtering to do the PPP

			// PR	=	R	+ C*dtRec	- C*dtSat	+ dTrop	+ dIono*K + dcb			+ innovation;
			// P	=	R	+ C*dtRec	- C*dtSat	+ dTrop	- dIono*K + ambiguity	+ innovation;


			//Prepare keys for filter states

			KFKey posKeys[3];
			for (short i = 0; i < 3; i++)
			{
				posKeys[i].type		= KF::REC_POS;
				posKeys[i].num		= i;
				posKeys[i].str		= obs.mount;
			}

			KFKey posRateKeys[3];
			for (short i = 0; i < 3; i++)
			{
				posRateKeys[i].type	= KF::REC_POS_RATE;
				posRateKeys[i].num	= i;
				posRateKeys[i].str	= obs.mount;
			}

			KFKey tropKeys[3];
			for (short i = 0; i < 3; i++)
			{
				tropKeys[i].type	= KF::TROP;
				tropKeys[i].num		= i;
				tropKeys[i].str		= obs.mount;
			}

			KFKey phaseBiasKey;
			{
				phaseBiasKey.type	= KF::PHASE_BIAS;
				phaseBiasKey.Sat	= obs.Sat;
				phaseBiasKey.num	= ft;
				phaseBiasKey.str	= obs.mount;
			}

			KFKey systemBiasKey;
			{
				systemBiasKey.type	= KF::REC_SYS_BIAS;
				systemBiasKey.num	= biasGroup;
				systemBiasKey.str	= obs.mount;
			}

			KFKey systemBiasRateKey;
			{
				systemBiasRateKey.type	= KF::REC_SYS_BIAS_RATE;
				systemBiasRateKey.num	= biasGroup;
				systemBiasRateKey.str	= obs.mount;
			}

			KFKey dcbKey;
			{
				dcbKey.type			= KF::DCB;
				dcbKey.num			= ft;
				dcbKey.str			= obs.mount;
			}

			KFKey ionoKey;
			{
				ionoKey.type 		= KF::IONOSPHERIC;
				ionoKey.Sat			= obs.Sat;
				ionoKey.str			= obs.mount;
			}

			//get any values required from state, and use variances in the filter instead of below
			double dcb			= 0;
			double C_dtRec		= 0;					//todo aaron, there is a comment about precise ephemeris in original code

			kfState.getKFValue(dcbKey,			dcb);
			kfState.getKFValue(phaseBiasKey,	phaseBias);
			kfState.getKFValue(systemBiasKey,	C_dtRec);

			double C_dtRecAdj	= rtk.sol.dtRec_m			[biasGroup]
								- rtk.sol.dtRec_m_ppp_old	[biasGroup];
			C_dtRec += C_dtRecAdj;

			InitialState systemBiasInit;
			systemBiasInit.x = C_dtRec;
			systemBiasInit.P = VAR_CLK;
			kfState.resetKFValue(systemBiasKey, systemBiasInit);

			//Prepare the measurement innovation values and sigmas

			double codeMeasured	= sig.P_corr_m;
			double phasMeasured	= sig.L_corr_m;

			double codeComputed	= sig.Range
								+ C_dtRec
								- C_dtSat
								+ dTrop
								+ dIono * ionC
								+ dcb;

			double phasComputed	= sig.Range
								+ C_dtRec
								- C_dtSat
								+ dTrop
								- dIono * ionC
								+ phaseBias;

											TestStack::testMat("codeMeasured",	codeMeasured);
											TestStack::testMat("phasMeasured",	phasMeasured);
											TestStack::testMat("codeComputed",	codeComputed);
											TestStack::testMat("phasComputed",	phasComputed);
											TestStack::testMat("sig.P_corr_m",	sig.P_corr_m);
											TestStack::testMat("sig.L_corr_m",	sig.L_corr_m);
											TestStack::testMat("sig.Range",		sig.Range);
											TestStack::testMat("C_dtRec",		C_dtRec);
											TestStack::testMat("C_dtSat",		C_dtSat);
											TestStack::testMat("dTrop",			dTrop);
											TestStack::testMat("dIono",			dIono);
											TestStack::testMat("ionC",			ionC);
											TestStack::testMat("dcb",			dcb);
											TestStack::testMat("phaseBias",		phaseBias);
											TestStack::testMat("dTropDx",		dTropDx,		3);

			tracepde(4, trace, "   PPP_states %s %d %8.4f  %9.4f %9.4f  %9.4f %8.4f %8.4f %9.4f %9.4f %.6e\n", obs.Sat.id().c_str(), ft, satStat.el, codeMeasured-sig.Range+C_dtSat, phasMeasured-sig.Range+C_dtSat, C_dtRec, dTrop,ionC*dIono,dcb,phaseBias, sqrt(varIono) * ionC);

			double codeInnov	= codeMeasured - codeComputed;
			double phasInnov	= phasMeasured - phasComputed;

			double codeVar		= sig.codeVar
								+ obs.var
								+ varTrop
								+ varIono * SQR(ionC);

			double phasVar		= sig.phasVar
								+ obs.var
								+ varTrop
								+ varIono * SQR(ionC)
								+ varSysIFB;

			ObsKey obsKeyCode = {obs.Sat, obs.mount, "P", ft};
			ObsKey obsKeyPhas = {obs.Sat, obs.mount, "L", ft};

			KFMeasEntry	codeMeas(&kfState, obsKeyCode);
			KFMeasEntry	phasMeas(&kfState, obsKeyPhas);

			codeMeas.setInnov(codeInnov);
			phasMeas.setInnov(phasInnov);

			codeMeas.setNoise(codeVar);
			phasMeas.setNoise(phasVar);

			codeMeas.metaDataMap["obs_ptr"] = &obs;
			phasMeas.metaDataMap["obs_ptr"] = &obs;

			// (Re)Initialise any states with values, covariances, and process noises

			InitialState posInits[3];
			for (int i = 0; i < 3; i++)
			{
				posInits[i]					= initialStateFromConfig(recOpts.pos,			i);

				posInits[i].x = x0[i];
				if (sppInit)
				{
					posInits[i].P = 1;
				}
			}

			InitialState posRateInits[3];
			for (int i = 0; i < 3; i++)
			{
				posRateInits[i]				= initialStateFromConfig(recOpts.pos_rate,		i);

// 				posRateInits[i].x = rtk.sol.sppRRec[i];	//todo aaron, get from doppler
			}

			InitialState clockBiasRateInit	= initialStateFromConfig(recOpts.clk_rate);

			InitialState phaseBiasInit		= initialStateFromConfig(recOpts.amb);
			{
				phaseBiasInit.x = phaseBias;
			}

			InitialState ionoInit			= initialStateFromConfig(acsConfig.ionFilterOpts.ion);
			{
				ionoInit.x = ionInitValue;
				ionoInit.P = ionInitVari;
				ionoInit.Q = SQR(acsConfig.ionFilterOpts.ion.proc_noise[0] / sin(satStat.el));
			}

			InitialState tropInits[3];
			for (int i = 0; i < 1; i++)
			{
				tropInits[i]				= initialStateFromConfig(recOpts.trop,			i);

				tropInits[i].x = tropStates[0];
				tropInits[i].P = varTrop;
			}
			for (int i = 0; i < 2; i++)
			{
				tropInits[i+1]				= initialStateFromConfig(recOpts.trop_grads,	i);
			}

			InitialState dcbInit;
			{
				/*switch (ft)
				{
					case F2:
					{
						//use dcb from station file
						double dcb;
						if (sys == +E_Sys::GLO)		dcb = -refstat.rbias[1][0];
						else						dcb = -refstat.rbias[0][0];

						dcbInit.x = dcb;
						dcbInit.P = SQR(0.0001);
						dcbInit.Q = 0;
						break;
					}
					case F5:
					{
						//estimate dcbIndex
						dcbInit.x = 0;
						dcbInit.P = VAR_DCB;
						dcbInit.Q = 0;			//todo aaron, had zero process noise before, check?
						break;
					}
					default:
					{
						//do not use these frequencies for dcb.
						break;
					}
				}*/
				/* These "DCBs" are not the P1-P2 DCBs */
				dcbInit.x = 0;
				dcbInit.P = VAR_DCB;
				dcbInit.Q = 0;
			}

			//Add the entries to the design matrix
			if (recOpts.pos.estimate)
			for (int i = 0; i < 3; i++)
			{
				codeMeas.addDsgnEntry(posKeys[i],		-satStat.e[i],		posInits[i]);
				phasMeas.addDsgnEntry(posKeys[i],		-satStat.e[i],		posInits[i]);

				if (recOpts.pos_rate.estimate)
				{
					kfState.setKFTransRate(posKeys[i],		posRateKeys[i],		1,	posRateInits[i]);
				}
			}

			if (recOpts.clk.estimate)
			{
				codeMeas.addDsgnEntry(systemBiasKey,	1,					systemBiasInit);
				phasMeas.addDsgnEntry(systemBiasKey,	1,					systemBiasInit);

				if (recOpts.clk_rate.estimate)
				{
					kfState.setKFTransRate(systemBiasKey,	systemBiasRateKey,	1,	clockBiasRateInit);
				}
			}

			if (acsConfig.ionoOpts.corr_mode == E_IonoMode::ESTIMATE)
			{
				codeMeas.addDsgnEntry(ionoKey,			-ionC,				ionoInit);
				phasMeas.addDsgnEntry(ionoKey,			+ionC,				ionoInit);
			}

			if	( (ft != F1)
				&&(ft != ft2)
				&&(ft != FTYPE_IF12)
				&&(ft != FTYPE_IF15))
			{
				codeMeas.addDsgnEntry(dcbKey,			1,					dcbInit);
			}

			if (recOpts.amb.estimate)
			{
				phasMeas.addDsgnEntry(phaseBiasKey,		1,					phaseBiasInit);
				measuredStates[phaseBiasKey] = true;
			}

			if (recOpts.trop.estimate)
			{
				codeMeas.addDsgnEntry(tropKeys[0],		dTropDx[0],			tropInits[0]);
				phasMeas.addDsgnEntry(tropKeys[0],		dTropDx[0],			tropInits[0]);
			}

			if (recOpts.trop_grads.estimate)
			for (int i = 1; i < 3; i++)
			{
				codeMeas.addDsgnEntry(tropKeys[i],		dTropDx[i],			tropInits[i]);
				phasMeas.addDsgnEntry(tropKeys[i],		dTropDx[i],			tropInits[i]);
			}


			if	(   acsConfig.max_inno	> 0
				&&( fabs(codeInnov)		> acsConfig.max_inno
				  ||fabs(phasInnov)		> acsConfig.max_inno))
			{
				tracepde(2, std::cout, "outlier rejected  sat=%s %d res=%9.4f %9.4f el=%4.1f\n", obs.Sat.id().c_str(), ft, codeInnov, phasInnov, satStat.el * R2D);
				obs.excludeOutlier = true;
				continue;			//todo aaron, moved to filer
			}

			//Add the completed measurements to the filter

			kfMeasEntryList.push_back(phasMeas);
			kfMeasEntryList.push_back(codeMeas);

			tracepde(4, trace, "%s sat=%2d P%d res=%9.4f sig=%9.4f el=%4.1f\n", str, obs.Sat, ft, codeInnov, sqrt(codeVar), satStat.el * R2D);
			tracepde(4, trace, "%s sat=%2d L%d res=%9.4f sig=%9.4f el=%4.1f\n", str, obs.Sat, ft, phasInnov, sqrt(phasVar), satStat.el * R2D);

			sig.vsig = true;
		}
	}

	removeUnmeasuredAmbiguities(trace, rtk.pppState, measuredStates);

	//add process noise to existing states as per their initialisations.
	kfState.stateTransition(std::cout, obsTime);

	//combine the measurement list into a single matrix
	KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);
	combinedMeas.time = obsList.front().time;

	if (combinedMeas.V.rows() == 0)
	{
		return SOLQ_NONE;
	}

	//if there are uninitialised state values, estimate them using least squared
	if (kfState.lsqRequired)
	{
		kfState.lsqRequired = false;
		trace << std::endl << " -------INITIALISING USING LEAST SQUARES--------" << std::endl;
		kfState.leastSquareInitStates(trace, combinedMeas);
	}

	//perform kalman filtering
	trace << std::endl << " -------DOING KALMAN FILTER --------" << std::endl;
	kfState.filterKalman(trace, combinedMeas, true);

	rtk.sol.dtRec_m_ppp_old = rtk.sol.dtRec_m;

	TestStack::testMat("combinedMeas.V", combinedMeas.V);
	TestStack::testMat("combinedMeas.A", combinedMeas.A);
	TestStack::testMat("kfState.x", kfState.x, 0, &kfState.P);

	return SOLQ_PPP;
}


/* update solution status ----------------------------------------------------*/
void update_stat(
	rtk_t&		rtk,
	ObsList&	obsList,
	int			stat)
{
	//test # of valid satellites
	rtk.sol.numSats = 0;

	std::string recId = "";
	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		bool hasValidSig = false;

		for (auto& [key, Sig] : obs.Sigs)
		{
			if (epoch > 5 && obs.Sat == SatSys(E_Sys::GPS, 16))
				continue;

			if (Sig.vsig == false)
				continue;

			hasValidSig = true;

			SatStat& satStat = *obs.satStat_ptr;
			SigStat& sigStat = satStat.sigStatMap[key];

			sigStat.userPhaseOutageCount = 0;
			if (Sig.phaseError)		sigStat.userPhaseRejectCount++;
			else 					sigStat.userPhaseRejectCount = 0;

		}

		if (hasValidSig)
		{
			rtk.sol.numSats++;
		}
		if (recId != "")
			assert(recId == obs.mount);
		recId = obs.mount;
	}

	if (rtk.sol.numSats >= MIN_NSAT_SOL)	rtk.sol.stat = stat;
	else									rtk.sol.stat = SOLQ_NONE;

	for (short i = 0; i < 3; i++)	rtk.pppState.getKFValue({KF::REC_POS,		{}, recId,	i}, rtk.sol.pppRRec		[i]);
	for (short i = 0; i < 2; i++)	rtk.pppState.getKFValue({KF::REC_SYS_BIAS,	{}, recId,	i}, rtk.sol.pppdtRec_m	[i]);

	//remove gps from glonass bias
	rtk.sol.pppdtRec_m[1] -= rtk.sol.pppdtRec_m[0];
}

/* precise point positioning -------------------------------------------------*/
void pppos(
	Trace&		trace,
	rtk_t&		rtk,
	ObsList&	obsList,
	Station&	rec)
{
	TestStack ts(__FUNCTION__);

	if (rtk.sol.stat == SOLQ_NONE)
	{
		return;
	}

	const prcopt_t* opt = &rtk.opt;
	char str[32];

	time2str(obsList.front().time, str, 2);
// 	tracepde(2, trace, "pppos   : time=%s nx=%d n=%d\n", str, rtk.nx, obsList.size());

	/* satellite positions and clocks */
	satposs(trace, obsList.front().time, obsList, nav, acsConfig.ppp_ephemeris);

	double pos[3];
	ecef2pos(rtk.sol.sppRRec.data(), pos);

	double ionoState = 0;
	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		tracepdeex(4,trace, "\n Ephem for %s: %13.3f %13.3f %13.3f %13.3f", obs.Sat.id().c_str(), obs.rSat[0], obs.rSat[1], obs.rSat[2],CLIGHT * obs.dtSat[0]);

		SatStat& satStat	= *(obs.satStat_ptr);

		bool pass = model_iono(obs.time, pos, satStat.azel, ionoState, satStat.extiono, satStat.extionovar);
		if (pass == false)
		{
			satStat.extionovar = -1;
		}
		tracepde(5, trace, "   extion %s : %.4f %.4f\n", obs.Sat.id().c_str(), satStat.extiono, satStat.extionovar);
	}

	/* earth tides correction */
	Vector3d dTide = Vector3d::Zero();
	if	( acsConfig.tide_solid
		||acsConfig.tide_otl
		||acsConfig.tide_pole)
	{
		tidedisp(trace, gpst2utc(obsList.front().time), rtk.sol.sppRRec, &nav.erp, opt->odisp[0], dTide);
	}

	int stat = ppp_filter(trace, obsList, dTide, rtk, rec);

	if (stat == SOLQ_PPP)
	{
		/* update solution status */
		update_stat(rtk, obsList, stat);

		if (rtk.sol.stat)
		{
			pppoutstat(trace, rtk.pppState,false, rtk.sol.stat,rtk.sol.numSats);
			rtk.pppState.outputStates(trace);
		}
	}
}
