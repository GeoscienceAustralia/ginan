
// #pragma GCC optimize ("O0")

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "linearCombo.hpp"
#include "corrections.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "ephPrecise.hpp"
#include "testUtils.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "satStat.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "antenna.hpp"
#include "common.hpp"
#include "wancorr.h"
#include "tides.hpp"
#include "trace.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "vmf3.h"
#include "trop.h"


#define MIN_NSAT_SOL	4				// min satellite number for solution


#define VAR_CLK     	SQR(2)       	// init variance receiver clock (m^2)
#define VAR_DCB     	SQR(30.0)       // init variance dcb (m^2)
#define VAR_GLO_IFB 	SQR( 0.6)       // variance of glonass ifb

/* write solution status for PPP
*/
void pppoutstat(
	Trace&		trace,
	KFState&	kfState,
	string		id)
{
	GWeek	week	= kfState.time;
	GTow	tow		= kfState.time;
	
	Block block(trace, "NMEA");
//
// 	double* x = rtk->sol.stat == SOLQ_FIX ? rtk->xa : rtk->xx;
//
	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		KFKey key = kfKey;
		if	( key.type	== KF::REC_POS
			&&key.num	== 0
			&&key.str	== id)
		{
			double x[3]	= {};
			double v[3]	= {};

			for (key.num = 0; key.num < 3; key.num++)
			{
				kfState.getKFValue(key, x[key.num], &v[key.num]);
			}
			tracepdeex(1, trace, "$POS,%d,%.3f,%.4f,%.4f,%.4f,%.7f,%.7f,%.7f\n",
						week,
						tow,
						x[0],
						x[1],
						x[2],
						sqrt(v[0]),
						sqrt(v[1]),
						sqrt(v[2]));
		}

// 		if (key.type == KF::PHASE_BIAS)
// 		{
// 			double phase_bias		= 0;
// 			double phase_biasVar	= 0;
// 			kfState.getKFValue(key, phase_bias, &phase_biasVar);
// 			tracepdeex(1, trace, "$AMB,%d,%.3f,%d,%s,%d,%.4f,%.7f\n",
// 						week,
// 						tow,
// 						solStat,
// 						key.Sat.id().c_str(),
// 						key.num,
// 						phase_bias,
// 						sqrt(phase_biasVar));
// 		}

		if	( key.type	== KF::TROP	//todo aaron needs iteration
			&&key.str	== id)
		{
			string grad = "";
			double trop		= 0;
			double tropVar	= 0;
			if (key.num == 1)	grad = "_N";
			if (key.num == 2)	grad = "_E";
			kfState.getKFValue(key, trop, &tropVar);
			tracepdeex(1, trace, "$TROP%s,%d,%.3f,%f,%.7f\n",
					grad.c_str(),
					week,
					tow,
					trop,
					sqrt(tropVar));
		}

		if	( key.type	== KF::REC_SYS_BIAS
			&&key.Sat	== SatSys(E_Sys::GPS)
			&&key.str	== id)
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
			key.Sat = SatSys(E_Sys::GLO);			kfState.getKFValue(key, rClkGLO, &GLOclkVar);
			key.Sat = SatSys(E_Sys::GAL);			kfState.getKFValue(key, rClkGAL, &GALclkVar);
			key.Sat = SatSys(E_Sys::BDS);			kfState.getKFValue(key, rClkBDS, &BDSclkVar);

			tracepdeex(1, trace, "$CLK,%d,%.3f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f\n",
					week,
					tow,
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
	Station&	rec,
	ObsList&	obsList)
{
	tracepdeex(3, trace, "\n%s : n=%d", __FUNCTION__, obsList.size());

	for (auto& [key, index] : rec.pppState.kfIndexMap)
	{
		if (key.type != KF::PHASE_BIAS)
		{
			continue;
		}

		SatStat& satStat = rec.satStatMap[key.Sat];
		SigStat& sigStat = satStat.sigStatMap[ft2string((E_FType)key.num)];

		if (sigStat.phaseRejectCount >= acsConfig.pppOpts.phase_reject_limit)
		{
			sigStat.phaseRejectCount = 0;

			trace << "Removing " << key << " due to repeated rejections > " << acsConfig.pppOpts.phase_reject_limit << std::endl;
			rec.pppState.removeState(key);
			
			continue;
		}

		if (rec.savedSlips.find(key.Sat) != rec.savedSlips.end())
		{
			trace << "Removing " << key << " from slip saved at: " << rec.savedSlips[key.Sat].to_string() << std::endl;
			rec.pppState.removeState(key);
			
			continue;
		}

		sigStat.phaseOutageCount++;
		if (sigStat.phaseOutageCount == acsConfig.pppOpts.outage_reset_limit)
		{
			sigStat.phaseOutageCount = 0;

			trace << "Removing " << key << " due to extended outage > " << acsConfig.pppOpts.outage_reset_limit << std::endl;
			rec.pppState.removeState(key);
			
			continue;
		}
		
		if 	(  acsConfig.reinit_on_all_slips
			&& sigStat.slip.any
			&& (  (acsConfig.excludeSlip.LLI	&& satStat.sigStatMap[ft2string(F1)].slip.LLI)
				||(acsConfig.excludeSlip.GF		&& satStat.sigStatMap[ft2string(F1)].slip.GF)	
				||(acsConfig.excludeSlip.MW		&& satStat.sigStatMap[ft2string(F1)].slip.MW)	
				||(acsConfig.excludeSlip.SCDIA	&& satStat.sigStatMap[ft2string(F1)].slip.SCDIA)))
		{
			trace << "Removing " << key << " due to cycle slip detection: " << std::endl;
			
			rec.pppState.removeState(key);
			
			InitialState init = initialStateFromConfig(acsConfig.getRecOpts("").ion_stec);
			
			if (init.estimate == false)
			{
				KFKey kfKey = key;
				for (kfKey.num = 0; kfKey.num < NUM_FTYPES; kfKey.num++)
				{
					rec.pppState.removeState(kfKey);
					continue;
				}
			}
			
			continue;
		}
	}
	
	rec.savedSlips.clear();

	for (auto& obs			: only<GObs>(obsList))
	for (auto& [ft, sig]	: obs.Sigs)
	{
		if (obs.exclude)
		{
			continue;
		}

		E_FType ifft	= FTYPE_IF12;
		E_FType ft1;
		E_FType ft2;
		E_FType ft3;
		if (!satFreqs(obs.Sat.sys,ft1,ft2,ft3))
			continue;
			
		if (ft1 == F1
		 && ft2 == F5)
			ifft = FTYPE_IF15;
		
		if	( (acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO && ft != ifft)
			||(acsConfig.ionoOpts.corr_mode != +E_IonoMode::IONO_FREE_LINEAR_COMBO && ft == ifft)
			||(sig.L_corr_m == 0)
			||(sig.P_corr_m == 0))
		{
			continue;
		}

		double	bias	= 0;
		SigStat& sigStat = obs.satStat_ptr->sigStatMap[ft2string(ft)];
		bool slip = sigStat.slip.any
					&&( (acsConfig.excludeSlip.LLI		&& sigStat.slip.LLI)
					  ||(acsConfig.excludeSlip.GF		&& sigStat.slip.GF)	
					  ||(acsConfig.excludeSlip.MW		&& sigStat.slip.MW)	
					  ||(acsConfig.excludeSlip.SCDIA	&& sigStat.slip.SCDIA));
					
		auto&	lam		= obs.satNav_ptr->lamMap;

		if		(acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
		{
			bias = sig.L_corr_m - sig.P_corr_m;
		}
		else if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::ESTIMATE)
		{
			if	( obs.Sigs[ft1].P	== 0
				||obs.Sigs[ft2].P	== 0
				||lam[ft1]			== 0
				||lam[ft2]			== 0
				||lam[ft]			== 0)
			{
				continue;
			}

			double ion = (obs.Sigs[ft1].P - obs.Sigs[ft2].P) / (1 - SQR(lam[ft2] / lam[ft1]));

			bias	= obs.Sigs[ft].L_corr_m
					- obs.Sigs[ft].P_corr_m
					+ 2 * ion * SQR(lam[ft] / lam[ft1]);
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
					+ 2 * ion * SQR(lam[ft] / lam[ft1]);
		}

		if	( bias
			&&slip)
		{
			// reinitialize phase-bias if detecting cycle slip

			KFKey phaseBiasKey;
			phaseBiasKey.type	= KF::PHASE_BIAS;
			phaseBiasKey.Sat	= obs.Sat;
			phaseBiasKey.num	= ft;
			phaseBiasKey.str	= obs.mount;
			
			rec.pppState.removeState(phaseBiasKey);

			tracepdeex(2, trace, "\n%s: sat=%s bias=%.3f\n", __FUNCTION__, obs.Sat.id().c_str(), bias);
			continue;
		}
	}
}



/* phase and code residuals --------------------------------------------------*/
E_Solution ppp_filter(
	Trace&		trace,
	ObsList&	obsList,
	Vector3d&	dr,
	Station&	rec)
{
	KFState&	kfState = rec.pppState;

	kfState.initFilterEpoch();

	string timeStr = obsList.front()->time.to_string(2);

	int lv = 3;

	GTime obsTime = obsList.front()->time;
	MjDateTT mjd	= obsTime;

	string id = rec.id;
	
	auto& recOpts = acsConfig.getRecOpts(id);

	//Get the previous estimate of position or load from the point positioning solution if not initialised.
	VectorEcef x0;
	bool sppInit = false;
	for (int i = 0; i < 3; i++)
	{
		KFKey xKey;
		xKey.type	= KF::REC_POS;
		xKey.str	= id;
		xKey.num	= i;
		bool pass = kfState.getKFValue(xKey, x0[i]);
		if (pass == false)
		{
			selectAprioriSource(rec, sppInit);

			x0(i) = rec.aprioriPos(i);			
			
			if (sppInit)
			{
				trace << std::endl << "Spp init used";
			}
		}
	}

	auto& pos = rec.pos;
	
	pos = ecef2pos(x0);

	recAtt(rec, obsTime, recOpts.rec_attitude.sources);
	
	Vector3d recAntVector = body2ecef(rec.attStatus, rec.antDelta);

	//update receiver position with tide and antenna delta
	//remove elements of receiver position that we dont want include in the state estimate
	VectorEcef rRec	= x0
					+ dr;

	tracepdeex(lv, trace, "\n\n   *-------- Observed minus computed --------*");

	pppCorrections(trace, obsList, rRec, rec);

	udbias_ppp(trace, rec, obsList);

	KFMeasEntryList		kfMeasEntryList;
	map<KFKey, bool>	measuredStates;

	for (auto sys : E_Sys::_values())
	{
		if (acsConfig.process_sys[sys] == false)
		{
			continue;
		}
		
		auto sysSat = SatSys(sys);
		
		InitialState systemBiasInit = initialStateFromConfig(recOpts.clk);
		
		KFKey systemBiasKey;
		{
			systemBiasKey.type		= KF::REC_SYS_BIAS;
			systemBiasKey.Sat		= sysSat;
			systemBiasKey.str		= id;
			systemBiasKey.rec_ptr	= &rec;
		}
		
		double C_dtRecAdj	= rec.sol.dtRec_m			[sys]
							- rec.sol.dtRec_m_ppp_old	[sys];

		rec.sol.dtRec_m_ppp_old[sys] = rec.sol.dtRec_m[sys];
		
		//get, modify and set the old bias in the state according to SPP estimates
		trace << std::endl
		<< "Adjusting " << systemBiasKey.str
		<< " clock by " << C_dtRecAdj;
		
		InitialState oneInit;
		
		if (kfState.time == GTime::noTime())
		{
			kfState.setKFTrans(systemBiasKey, KFState::oneKey, C_dtRecAdj, systemBiasInit);
		}
		else
		{
			double dt = (rec.sol.time - kfState.time).to_double();
			kfState.setKFTransRate(systemBiasKey, KFState::oneKey, C_dtRecAdj / dt, oneInit, systemBiasInit);
		}
	}
	
	//add process noise to existing states as per their initialisations.
	kfState.stateTransition(std::cout, obsTime);
	
	for (auto& obs : only<GObs>(obsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		auto& satOpts = acsConfig.getSatOpts(obs.Sat);
		
		if (satOpts.exclude)
		{
			continue;
		}
		
		auto		sys			= obs.Sat.sys;
		auto&		lam			= obs.satNav_ptr->lamMap;
		SatStat&	satStat		= *(obs.satStat_ptr);
		SatSys		sysSat		= SatSys(sys);

		E_FType ft1;
		E_FType ft2;
		E_FType ft3;
		if (!satFreqs(sys,ft1,ft2,ft3))
			continue;
		
		double ionfact = SQR(lam[ft1]) / (SQR(lam[ft1]) - SQR(lam[ft2]));

		//Satellite already has precise clocks if available
		double C_dtSat = CLIGHT * obs.satClk;

		//try to get precicse clock for receiver

		double precDtRec	= 0;
		double precDtRecVar	= 0;
		pephclk(trace, rec.sol.time, obs.mount, nav, precDtRec, &precDtRecVar);
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
		for (short i = 0; i < 1; i++)
		{
			kfState.getKFValue({KF::TROP,		{}, obs.mount, i}, tropStates[i]);
		}
		for (short i = 1; i < 3; i++)
		{
			kfState.getKFValue({KF::TROP_GRAD,	{}, obs.mount, i-1}, tropStates[i]);
		}
		if (tropStates[0] == 0)
		{
			tropStates[0] = sbstropcorr(rec.sol.time, rRec, PI/2);
		}

		//calculate the trop values, variances, and gradients at the operating points
		
		auto& recOpts = acsConfig.getRecOpts("");
		
		InitialState tropInit = initialStateFromConfig(recOpts.trop);
		if (tropInit.estimate)
		{
			dTrop = trop_model_prec(obs.time, pos, satStat.azel, tropStates, dTropDx, varTrop);
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
				dIono	= 0;
				varIono	= 0;
				break;
			case E_IonoMode::ESTIMATE:
				if	(  obs.Sigs[ft1].P_corr_m > 0  
					&& obs.Sigs[ft2].P_corr_m > 0)	
				{
					ionInitValue = obs.Sigs[ft1].P_corr_m - obs.Sigs[ft2].P_corr_m;		// code GF
					ionInitVari  = obs.Sigs[ft1].codeVar  + obs.Sigs[ft2].codeVar;
					
					ionInitValue	*=		ionfact;												// from GF to slant Iono(F1)
					ionInitVari		*= SQR(	ionfact);
				}
				
				if	(kfState.getKFValue({KF::IONOSPHERIC, obs.Sat, obs.mount}, ionoState))	dIono = ionoState;
				else																		dIono = ionInitValue;
				
				varIono = 0;
				
				break;
			default:
				dIono	= satStat.extiono;
				varIono	= satStat.extionovar;
		}

		tracepdeex(lv, trace, "\n*---------------------------------------------------*");
		tracepdeex(lv, trace, "\n%s %s  recpos               = %14.4f %14.4f %14.4f",	timeStr.c_str(), obs.Sat.id().c_str(), x0[0], x0[1], x0[2]);
		tracepdeex(lv, trace, "\n%s %s  tide                 = %14.4f %14.4f %14.4f",	timeStr.c_str(), obs.Sat.id().c_str(), dr(0), dr(1), dr(2));
		tracepdeex(lv, trace, "\n%s %s  recpos+tide          = %14.4f %14.4f %14.4f",	timeStr.c_str(), obs.Sat.id().c_str(), rRec[0], rRec[1], rRec[2]);
		tracepdeex(lv, trace, "\n%s %s  sinexpos             = %14.4f %14.4f %14.4f",	timeStr.c_str(), obs.Sat.id().c_str(), rec.aprioriPos[0], rec.aprioriPos[1], rec.aprioriPos[2]);
		tracepdeex(lv, trace, "\n%s %s  rSat                 = %14.4f %14.4f %14.4f",	timeStr.c_str(), obs.Sat.id().c_str(), obs.rSat[0], obs.rSat[1], obs.rSat[2]);
		tracepdeex(lv, trace, "\n%s %s  trop mf              = %14.4f",					timeStr.c_str(), obs.Sat.id().c_str(), dTropDx[0]);
		tracepdeex(lv, trace, "\n%s %s  trop                 = %14.4f",					timeStr.c_str(), obs.Sat.id().c_str(), dTrop);


		if (varIono < 0) 
			continue;

		for (auto& [ft, sig] : obs.Sigs)
		{
			auto& sigStat = satStat.sigStatMap[ft2string(ft)];

			if (acsConfig.ionoOpts.corr_mode != +E_IonoMode::IONO_FREE_LINEAR_COMBO)
			{
				if	( ft == FTYPE_IF12
					||ft == FTYPE_IF15
					||ft == FTYPE_IF25)
				{
					continue;
				}
			}

			if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
			{
				if(ft != FTYPE_IF12   && ft != FTYPE_IF15)	continue;
				if(sys == +E_Sys::GLO && ft != FTYPE_IF12)	continue;
				if(sys == +E_Sys::GAL && ft != FTYPE_IF15)	continue;
				if(sys == +E_Sys::BDS && ft != FTYPE_IF12)	continue;		/* need to confirm this */
				if(sys == +E_Sys::QZS && ft != FTYPE_IF12)	continue;
			}

			if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::TOTAL_ELECTRON_CONTENT)
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
				case E_IonoMode::ESTIMATE:				 ionC = SQR(lam[ft]/lam[ft1]); break;
				default:								 ionC = SQR(lam[ft]/lam[ft1]); break;
			}

			double varSysIFB = 0;
			if	(sys == +E_Sys::GLO)
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
			for (short i = 0; i < 1; i++)
			{
				tropKeys[i].type	= KF::TROP;
				tropKeys[i].num		= i;
				tropKeys[i].str		= obs.mount;
				tropKeys[i].rec_ptr	= &rec;
			}
			for (short i = 1; i < 3; i++)
			{
				tropKeys[i].type	= KF::TROP_GRAD;
				tropKeys[i].num		= i-1;
				tropKeys[i].str		= obs.mount;
				tropKeys[i].rec_ptr	= &rec;
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
				systemBiasKey.type		= KF::REC_SYS_BIAS;
				systemBiasKey.Sat		= sysSat;
				systemBiasKey.str		= obs.mount;
				systemBiasKey.rec_ptr	= &rec;
			}

			KFKey systemBiasRateKey;
			{
				systemBiasRateKey.type	= KF::REC_SYS_BIAS_RATE;
				systemBiasRateKey.Sat	= sysSat;
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
			
			double recAntDelta	= -recAntVector.dot(satStat.e);
			//get any values required from state, and use variances in the filter instead of below
			double dcb			= 0;
			double C_dtRec		= 0;
			double phaseBias	= 0;

			kfState.getKFValue(dcbKey,			dcb);
			kfState.getKFValue(systemBiasKey,	C_dtRec);
			kfState.getKFValue(phaseBiasKey,	phaseBias);

			//Prepare the measurement innovation values and sigmas

			double codeMeasured	= sig.P_corr_m;
			double phasMeasured	= sig.L_corr_m;

			double codeComputed = 0;

			if (acsConfig.model.range)					codeComputed += sig.Range;
			if (acsConfig.model.sagnac)					codeComputed += sagnac(obs.rSat, rRec);
			if (recOpts.rec_pco)						codeComputed -= sigStat.recPco;
			if (recOpts.rec_clock.enable)				codeComputed += C_dtRec;
			if (satOpts.sat_clock.enable)				codeComputed -= C_dtSat;
			if (acsConfig.model.trop.enable)			codeComputed += dTrop;
			if (acsConfig.model.ionospheric_component)	codeComputed += dIono * ionC;
			if (0)										codeComputed += dcb;
			if (satOpts.sat_code_bias.enable)			codeComputed += sig.biases[CODE];
			if (recOpts.rec_pcv)						codeComputed += sigStat.recPcv;
			if (satOpts.sat_pcv)						codeComputed += sigStat.satPcv;
			if (recOpts.rec_ant_delta)					codeComputed += recAntDelta;

			double phasComputed = 0;
			
			if (acsConfig.model.range)					phasComputed += sig.Range;
			if (acsConfig.model.sagnac)					phasComputed += sagnac(obs.rSat, rRec);
			if (recOpts.rec_pco)						phasComputed -= sigStat.recPco;
			if (recOpts.rec_clock.enable)				phasComputed += C_dtRec;
			if (satOpts.sat_clock.enable)				phasComputed -= C_dtSat;
			if (acsConfig.model.trop.enable)			phasComputed += dTrop;
			if (acsConfig.model.ionospheric_component)	phasComputed -= dIono * ionC;
			if (acsConfig.model.integer_ambiguity)		phasComputed += phaseBias;		
			if (satOpts.sat_phase_bias.enable)			phasComputed += sig.biases[PHAS];
			if (satOpts.sat_pcv)						phasComputed += sigStat.satPcv;
			if (recOpts.rec_pcv)						phasComputed += sigStat.recPcv;
			if (acsConfig.model.phase_windup)			phasComputed += satStat.phw * sigStat.lambda;
			if (recOpts.rec_ant_delta)					phasComputed += recAntDelta;

			double codeInnov	= codeMeasured - codeComputed;
			double phasInnov	= phasMeasured - phasComputed;

			double codeVar		= sig.codeVar
								+ obs.satClkVar	
								+ varTrop		
								+ varIono * SQR(ionC);

			double phasVar		= sig.phasVar
								+ obs.satClkVar	
								+ varTrop		
								+ varIono * SQR(ionC)
								+ varSysIFB;

// 			tracepdeex(0, std::cout, "\n%14.6f %14.6f %14.6f %14.6f %14.6f %14.6f", sig.codeVar, sig.phasVar, obs.ephVar, varTrop, varIono, varSysIFB);

			KFKey obsKey;

			obsKey.Sat	= obs.Sat;	
			obsKey.str	= obs.mount;
			obsKey.num	= ft;		
			
			KFKey obsKeyCode = obsKey;		obsKeyCode.type = KF::CODE_MEAS;	obsKeyCode.comment = "P";
			KFKey obsKeyPhas = obsKey;		obsKeyPhas.type = KF::PHAS_MEAS;	obsKeyPhas.comment = "L";
			
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

// 				posRateInits[i].x = rec.sol.sppRRec[i];
			}

			InitialState systemBiasInit		= initialStateFromConfig(recOpts.clk);
			{
				systemBiasInit.x = rec.sol.dtRec_m[obs.Sat.sys];
				systemBiasInit.P = VAR_CLK;
			}
		
			InitialState clockBiasRateInit	= initialStateFromConfig(recOpts.clk_rate);

			InitialState phaseBiasInit		= initialStateFromConfig(recOpts.amb);
			{
				phaseBiasInit.x = phaseBias;
			}

			InitialState ionoInit			= initialStateFromConfig(acsConfig.ionModelOpts.ion);
			{
				ionoInit.x = ionInitValue;
				ionoInit.P = ionInitVari;
				ionoInit.Q = SQR(acsConfig.ionModelOpts.ion.proc_noise[0] / sin(satStat.el));
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
						dcbInit.Q = 0;	
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
// 			if (recOpts.pos.estimate)
			for (int i = 0; i < 3; i++)
			{
				if (posInits[i].estimate == false)
				{
					continue;
				}
				
				codeMeas.addDsgnEntry(posKeys[i],		-satStat.e[i],		posInits[i]);
				phasMeas.addDsgnEntry(posKeys[i],		-satStat.e[i],		posInits[i]);

				if (posRateInits[i].estimate)
				{
					kfState.setKFTransRate(posKeys[i],		posRateKeys[i],		1,	posRateInits[i]);
				}
			}

			if (systemBiasInit.estimate)
			{
				codeMeas.addDsgnEntry(systemBiasKey,	1,					systemBiasInit);
				phasMeas.addDsgnEntry(systemBiasKey,	1,					systemBiasInit);

				if (clockBiasRateInit.estimate)
				{
					kfState.setKFTransRate(systemBiasKey,	systemBiasRateKey,	1,	clockBiasRateInit);
				}
			}

			if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::ESTIMATE)
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

			if (phaseBiasInit.estimate)
			{
				phasMeas.addDsgnEntry(phaseBiasKey,		1,					phaseBiasInit);
				measuredStates[phaseBiasKey] = true;
			}

			if (tropInits[0].estimate)
			{
				codeMeas.addDsgnEntry(tropKeys[0],		dTropDx[0],			tropInits[0]);
				phasMeas.addDsgnEntry(tropKeys[0],		dTropDx[0],			tropInits[0]);
			}

// 			if (recOpts.trop_grads.estimate)
			for (int i = 1; i < 3; i++)
			{
				if (tropInits[i].estimate == false)
				{
					continue;
				}
				
				codeMeas.addDsgnEntry(tropKeys[i],		dTropDx[i],			tropInits[i]);
				phasMeas.addDsgnEntry(tropKeys[i],		dTropDx[i],			tropInits[i]);
			}

			//Add the completed measurements to the filter

			kfMeasEntryList.push_back(phasMeas);
			kfMeasEntryList.push_back(codeMeas);

			tracepdeex(4, trace, "\n%s %s P%d res=%9.4f sig=%9.4f el=%4.1f", timeStr.c_str(), obs.Sat.id().c_str(), ft, codeInnov, sqrt(codeVar), satStat.el * R2D);
			tracepdeex(4, trace, "\n%s %s L%d res=%9.4f sig=%9.4f el=%4.1f", timeStr.c_str(), obs.Sat.id().c_str(), ft, phasInnov, sqrt(phasVar), satStat.el * R2D);

			sig.vsig = true;
		}
	}

	removeUnmeasuredAmbiguities(trace, rec.pppState, measuredStates);

	//add process noise to existing states as per their initialisations.
	kfState.stateTransition(std::cout, obsTime);
	
	//combine the measurement list into a single matrix
	KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);
	combinedMeas.time = obsList.front()->time;

	if (combinedMeas.V.rows() == 0)
	{
		trace << std::endl << "-------NO MEASUREMENTS TO FILTER!--------" << std::endl;
		return E_Solution::NONE;
	}

	//if there are uninitialised state values, estimate them using least squared
	if (kfState.lsqRequired)
	{
		kfState.lsqRequired = false;
		trace << std::endl << "-------INITIALISING USING LEAST SQUARES--------" << std::endl;
		kfState.leastSquareInitStates(trace, combinedMeas);
	}

	//perform kalman filtering
	trace << std::endl << "-------DOING KALMAN FILTER --------" << std::endl;
	kfState.filterKalman(trace, combinedMeas, true);

	return E_Solution::PPP;
}


/** update solution status
 */
void updateSolutionStatus(
	Station&	rec,
	ObsList&	obsList,
	E_Solution	stat)
{
	//test # of valid satellites
	rec.sol.numMeas = 0;

	string recId = "";
	for (auto& obs : only<GObs>(obsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		bool hasValidSig = false;

		for (auto& [key, Sig] : obs.Sigs)
		{
			if (Sig.vsig == false)
				continue;

			hasValidSig = true;

			SatStat& satStat = *obs.satStat_ptr;
			SigStat& sigStat = satStat.sigStatMap[ft2string(key)];

			sigStat.phaseOutageCount = 0;
			if (Sig.phaseError)		sigStat.phaseRejectCount++;
			else 					sigStat.phaseRejectCount = 0;

		}

		if (hasValidSig)
		{
			rec.sol.numMeas++;
		}
		if (recId != "")
			assert(recId == obs.mount);
		recId = obs.mount;
	}

	if (rec.sol.numMeas >= MIN_NSAT_SOL)	rec.sol.status = stat;
	else									rec.sol.status = E_Solution::NONE;

	for (short i = 0; i < 3; i++)				rec.pppState.getKFValue({KF::REC_POS,		{},				recId,	i}, rec.sol.pppRRec[i]);
	for (E_Sys sys : {E_Sys::GPS, E_Sys::GLO})	rec.pppState.getKFValue({KF::REC_SYS_BIAS,	SatSys(sys),	recId}, 	rec.sol.pppdtRec_m[sys]);

	//remove gps from glonass bias
	rec.sol.pppdtRec_m[E_Sys::GLO] -= rec.sol.pppdtRec_m[E_Sys::GPS];
}

/** precise point positioning
 */
void pppos(
	Trace&		trace,
	ObsList&	obsList,
	Station&	rec)
{
	if (rec.sol.status == +E_Solution::NONE)
	{
		return;
	}

	tracepdeex(3, trace, "\n%-10s: time=%s n=%d", __FUNCTION__, obsList.front()->time.to_string().c_str(), obsList.size());

	// satellite positions and clocks
	for (auto& obs : only<GObs>(obsList))
	{
		auto& satOpts = acsConfig.getSatOpts(obs.Sat);
		
		satPosClk(trace, obsList.front()->time, obs, nav, satOpts.sat_pos.ephemeris_sources, satOpts.sat_clock.ephemeris_sources, nullptr, E_OffsetType::APC);
	}
	
	auto& pos = rec.pos;
	
	pos = ecef2pos(rec.sol.sppRRec);

	double ionoState = 0;
	for (auto& obs : only<GObs>(obsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		tracepdeex(4,trace, "\nEphem for %s: %13.3f %13.3f %13.3f %13.3f", obs.Sat.id().c_str(), obs.rSat[0], obs.rSat[1], obs.rSat[2], CLIGHT * obs.satClk);

		SatStat& satStat	= *(obs.satStat_ptr);

		bool pass = ionoModel(obs.time, pos, satStat.azel, ionoState, satStat.extiono, satStat.extionovar);
		if (pass == false)
		{
			satStat.extionovar = -1;
		}
		tracepdeex(5, trace, "   extion %s : %.4f %.4f", obs.Sat.id().c_str(), satStat.extiono, satStat.extionovar);
	}

	// earth tides correction
	Vector3d dTide = Vector3d::Zero();
	if (acsConfig.model.tides.enable)
	{
		tideDisp(trace, obsList.front()->time, rec.sol.sppRRec, nav.erp, rec.otlDisplacement, dTide);
	}

	E_Solution stat = ppp_filter(trace, obsList, dTide, rec);
	
	if (stat == +E_Solution::PPP)
	{
		updateSolutionStatus(rec, obsList, stat);

// 		pppoutstat(trace, rec.pppState,false, rec.sol.stat,rec.sol.numSats);
//		rec.pppState.outputStates(trace);
	}
}
