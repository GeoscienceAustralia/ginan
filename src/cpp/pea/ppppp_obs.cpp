
// #pragma GCC optimize ("O0")

#include "eigenIncluder.hpp"
#include "streamTrace.hpp"
#include "corrections.hpp"
#include "instrument.hpp"
#include "acsConfig.hpp"
#include "biasSINEX.hpp"
#include "antenna.hpp"
#include "preceph.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "tides.hpp"



#define DEFAULT_BIAS_VAR SQR(20)


void stationPPP(
			Trace&				netTrace,			///< Trace to output to
			Station&			rec,				///< Receiver to perform calculations for
/*	const*/	KFState&			kfState,			///< Kalman filter object containing the network state parameters
			KFMeasEntryList&	kfMeasEntryList,	///< List to append kf measurements to
			gptgrid_t&			gptg,				///< [in/out]
// 			vmf3_t*				vmf3,				///< [in/out]
			double*				orography)			///< Pointer to orography maps
{
	auto trace = getTraceFile(rec);
	
// 	Instrument	instrument(__FUNCTION__ + rec.id);
	
	if (rec.obsList.empty())
	{
		return;
	}
	
	GTime time = rec.obsList.front().time;
	
	satposs(trace, time, rec.obsList, nav, E_Ephemeris::PRECISE, E_OffsetType::COM, false);
	
	ERPValues erpv;
	geterp(nav.erp, time, erpv);
	
	Matrix3d i2tMatrix = Matrix3d::Identity();
	eci2ecef(time, erpv, i2tMatrix);
	
	for (auto& [satId, satNav] : nav.satNavMap)
	{
		SatSys Sat;
		Sat.fromHash(satId);
		
		if (acsConfig.process_sys[Sat.sys] == false)
		{
			continue;
		}
		
		auto& satOpts = acsConfig.getSatOpts(Sat);
		
		InitialState init	= initialStateFromConfig(satOpts.orb);
		if (init.estimate)
			orbPartials(trace, time, Sat, satNav.satPartialMat);	
	}
	
	for (auto& obs			: rec.obsList)
	for (int measType		: {PHAS, CODE})
	for (auto& [ft, sig]	: obs.Sigs)
	{
		SatNav&		satNav	= *obs.satNav_ptr;
		SatStat&	satStat	= *obs.satStat_ptr;
		SigStat&	sigStat	= satStat.sigStatMap[ft];
		
		if (acsConfig.process_freq[obs.Sat.sys][ft] == false)
		{
			continue;
		}
		
		if (acsConfig.process_meas[measType] == false)
		{
			continue;
		}
		
		if	(satexclude(obs.Sat, obs.svh))
		{
			printf("\n%s excludeSvh",			obs.Sat.id().c_str());
			
			obs.exclude = true;
			continue;
		}
		
		if (sigStat.slip.any)
		{
			if (acsConfig.excludeSlip.LLI	&& satStat.sigStatMap[F1].slip.LLI)		{	continue;   }
			if (acsConfig.excludeSlip.GF	&& satStat.sigStatMap[F1].slip.GF)		{	continue;   }
			if (acsConfig.excludeSlip.MW	&& satStat.sigStatMap[F1].slip.MW)		{	continue;   }
			if (acsConfig.excludeSlip.EMW	&& satStat.sigStatMap[F1].slip.EMW)		{	continue;   }
			if (acsConfig.excludeSlip.CJ	&& satStat.sigStatMap[F1].slip.CJ)		{	continue;   }
			if (acsConfig.excludeSlip.SCDIA	&& satStat.sigStatMap[F1].slip.SCDIA)	{	continue;   }
		}
		
		if (obs.exclude)
		{
// 			if (0)
			{
				continue;
			}

			if (obs.excludeBadSPP)		{	printf("\n%s excludeBadSPP",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeConfig)		{	printf("\n%s excludeConfig",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeEclipse)		{	printf("\n%s excludeEclipse",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeElevation)	{	printf("\n%s excludeElevation",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeOutlier)		{	printf("\n%s excludeOutlier",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeSlip)		{	printf("\n%s excludeSlip",			obs.Sat.id().c_str());	continue;	}
			if (obs.excludeSystem)		{	printf("\n%s excludeSystem",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeSVH)			{	printf("\n%s excludeSVH",			obs.Sat.id().c_str());	continue;	}
			if (obs.excludeTrop)		{	printf("\n%s excludeTrop",			obs.Sat.id().c_str());	continue;	}
		}
		
		auto& satOpts = acsConfig.getSatOpts(obs.Sat);
		auto& recOpts = acsConfig.getRecOpts(rec.id);
			
		if (satOpts.exclude)
		{
			printf("\n%s excludeSatOpts",			obs.Sat.id().c_str());
			continue;
		}
		
		if (recOpts.exclude)
		{
			printf("\n%s excludeRecOpts",			obs.Sat.id().c_str());
			continue;
		}
		
		auto		Sat			= obs.Sat;
		auto		sys			= Sat.sys;
		int			biasGroup	= Sat.biasGroup();
		double		lambda		= obs.satNav_ptr->lamMap[ft];
		auto		code		= sig.code;
		
// 		PhaseCenterData* satPCD_ptr = findAntenna(Sat.id(), time, nav, ft);
	
		double observed = 0;
		if		(measType == CODE)		observed = sig.P;
		else if	(measType == PHAS)		observed = sig.L * lambda;
		
		if (observed == 0)
		{
			continue;
		}
		
		KFMeasEntry measEntry(&kfState);
		
		measEntry.metaDataMap["obs_ptr"]	= &obs;
		
		if (measType == PHAS)
		{
			measEntry.metaDataMap["PhaseRejectCount_ptr"] = &sigStat.netwPhaseRejectCount;
			measEntry.metaDataMap["PhaseOutageCount_ptr"] = &sigStat.netwPhaseOutageCount;
		}
		
		measEntry.obsKey.Sat	= obs.Sat;
		measEntry.obsKey.str	= rec.id;
		measEntry.obsKey.num	= ft;
		if		(measType == CODE)		measEntry.obsKey.type = string("P-") + code._to_string();
		else if	(measType == PHAS)		measEntry.obsKey.type = string("L-") + code._to_string();
		
		//Start with the observed measurement and its noise
		
		measEntry.componentList.push_back({"Observed", -observed, "- Phi"});
		{
			ObsKey obsKey;
			obsKey.str	= rec.id;
			obsKey.Sat	= obs.Sat;
			obsKey.type	= measEntry.obsKey.type;
			obsKey.num	= ft;
			
			if		(measType == CODE)	{	measEntry.addNoiseEntry(obsKey, 1, sig.codeVar);	}
			else if	(measType == PHAS)	{	measEntry.addNoiseEntry(obsKey, 1, sig.phasVar);	}
		}
		
		if (acsConfig.output_residual_chain)
		{
			tracepdeex(0, trace, "\n----------------------------------------------------");
			tracepdeex(0, trace, "\nMeasurement for %s", ((string) measEntry.obsKey).c_str());
		}
		
		//Calculate the basic range
		
		Vector3d rRec = Vector3d::Zero();
		Vector3d vRec = Vector3d::Zero();
		{
			bool sppInit = false;
			selectAprioriSource(rec, sppInit);

			rRec = rec.aprioriPos;
			
			for (int i = 0; i < 3; i++)
			{
				InitialState init = initialStateFromConfig(recOpts.pos, i);
				
				if (init.estimate == false)
				{
					continue;
				}
				
				KFKey kfKey;
				kfKey.type	= KF::REC_POS;
				kfKey.str	= rec.id;
				kfKey.num	= i;
				
				kfState.getKFValue(kfKey, rRec[i]);
				
				init.x = rRec[i];
				
				measEntry.addDsgnEntry(kfKey, -satStat.e[i], init);
				
				InitialState rateInit = initialStateFromConfig(recOpts.pos_rate, i);
				if (rateInit.estimate)
				{
					KFKey rateKey;
					rateKey.type	= KF::REC_POS_RATE;
					rateKey.str	= rec.id;
					rateKey.num	= i;
					
					kfState.getKFValue(kfKey, vRec[i]);
					
					kfState.setKFTransRate(kfKey, rateKey,	1,	rateInit);
				}
			}
		}
		
		
			
		
		
		double pos[3];
		ecef2pos(rRec, pos);
		
		bool satNoise = true;
		Vector3d rSat = Vector3d::Zero();
		{
			//initialise using ephemeris values if available
			rSat					= obs.rSat;
			
			ObsKey obsKey;
			obsKey.Sat	= obs.Sat;
			obsKey.type	= "satEph";
			
			measEntry.addNoiseEntry(obsKey, 1, obs.ephVar);
		}
		
		//Range
		double rRecSat = (rSat - rRec).norm();
		if (acsConfig.model.range)
		{
			measEntry.componentList.push_back({"Range", rRecSat, "+ rho"});
		}
		
		//Add modelled adjustments and estimated parameter
		
		//Receiver Clock
		
		if (acsConfig.model.rec_clock.enable)
		{
			double	recClk_m = 0;
			double	precDtRecVar	= 0;
			double	precDtRec		= 0;
			pephclk(obs.time, rec.id, nav, precDtRec, &precDtRecVar);
			
			recClk_m		= precDtRec * CLIGHT;
			precDtRecVar	*= SQR(CLIGHT);
			
			InitialState init		= initialStateFromConfig(recOpts.clk);
				
			if (init.estimate)
			{
				KFKey kfKey;
				kfKey.type		= KF::REC_CLOCK;
				kfKey.str		= rec.id;
				kfKey.rec_ptr	= &rec;
				// 			kfKey.num	= i;
				
				
				bool found = kfState.getKFValue(kfKey,	recClk_m);
				
				init.x = recClk_m;
				
				measEntry.addDsgnEntry(kfKey, 1, init);
			}
			else
			{
				ObsKey obsKey;
				obsKey.str	= rec.id;
				obsKey.type	= "recClk";		
				measEntry.addNoiseEntry(obsKey, 1, precDtRecVar);
			}
				
			measEntry.componentList.push_back({"Rec Clock", recClk_m, "+ Cdt_r"});
		}
		
		//Satellite Clock
		if (acsConfig.model.sat_clock.enable)
		{
			double satClk_m = obs.dtSat[0] * CLIGHT;	//todo aaron, check its valid before use.
			
			InitialState init		= initialStateFromConfig(satOpts.clk);
			
			if (init.estimate)
			{
				KFKey kfKey;
				kfKey.type	= KF::SAT_CLOCK;
				kfKey.Sat	= Sat;
				kfState.getKFValue(kfKey,	satClk_m);
			
				init.x = satClk_m;
				
				measEntry.addDsgnEntry(kfKey, -1, init);
			}
			measEntry.componentList.push_back({"Sat Clock", -satClk_m, "- Cdt_s"});
		}
		
		//Receiver Antenna delta
		if (acsConfig.model.rec_ant_delta)
		{
			Vector3d recAntVector;
			enu2ecef(pos, rec.antDelta, recAntVector);
			
			{
				
			}
			double recAntDelta = -recAntVector.dot(satStat.e);
			measEntry.componentList.push_back({"Rec Antenna Delta", recAntDelta, "- E.dR_r"});
		}
		
		//Satellite Antenna delta
 		if (0)
		{
			//doesnt exist
		}
		
		double gamma	= SQR(satNav.lamMap[L2]) / SQR(satNav.lamMap[L1]);	
		double C1		= gamma	/ (gamma - 1);
		double C2		= -1	/ (gamma - 1);
			
		//Receiver Phase Center Offset
		if (acsConfig.model.rec_pco)
		{
			Vector3d pco_enu;
			
			/* receiver pco correction to the coordinates */
			if (acsConfig.dumb_rec_pco == false)
			{
				pco_enu = antPco(rec.antId, Sat.sys, ft, time);
			}
			else
			{
				Vector3d pco_enus[2];
				pco_enus[0] = antPco(rec.antId, Sat.sys, F1, time);
				pco_enus[1] = antPco(rec.antId, Sat.sys, F2, time);
				
				pco_enu	= C1 * pco_enus[0]
						+ C2 * pco_enus[1];
			}
			
			Vector3d recPCOVector = Vector3d::Zero();
			enu2ecef(pos, pco_enu, recPCOVector);    /* convert enu to xyz */
			
			double recPCODelta = -recPCOVector.dot(satStat.e);
			measEntry.componentList.push_back({"Rec PCO", recPCODelta, "- E.PCO_r"});
		}
		
		//Satellite Phase Center Offset
		if (acsConfig.model.sat_pco)
		{
			Vector3d satPCOVector;
			
			if (acsConfig.dumb_sat_pco == false)
			{
				satPCOVector	= satAntOff(trace, time, obs.rSat, Sat, ft, &satStat);
			}
			else
			{
				Vector3d pcos[2];
				pcos[0]			= satAntOff(trace, time, obs.rSat, Sat, F1, &satStat);
				pcos[1]			= satAntOff(trace, time, obs.rSat, Sat, F2, &satStat);
				
				satPCOVector	= C1 * pcos[0]
								+ C2 * pcos[1];
			}
			
			double satPCODelta = satPCOVector.dot(satStat.e);
			measEntry.componentList.push_back({"Sat PCO", satPCODelta, "+ E.PCO_s"});
		}
		
		//Receiver Phase Center Variation
		if (acsConfig.model.rec_pcv)
		{
			double recPCVDelta = antPcv(rec.antId, Sat.sys, ft, time, PI/2 - satStat.el, satStat.az);
			
			measEntry.componentList.push_back({"Rec PCV", recPCVDelta, "+ PCV_r"});
		}
		
		//Satellite Phase Center Variation
		if (acsConfig.model.sat_pcv)
		{
			satStat.nadir = satNadir(rSat, rRec);	//todo aaron move up
			
			double satPCVDelta = antPcv(Sat.id(), Sat.sys, ft, time, satStat.nadir);
			
			measEntry.componentList.push_back({"Sat PCV", satPCVDelta, "+ PCV_s"});
		}
		
		//Tide delta
		if (acsConfig.model.tides.enable)
		{
			Vector3d tideVectorSum		= Vector3d::Zero();
			Vector3d tideVectorSolid	= Vector3d::Zero();
			Vector3d tideVectorOTL		= Vector3d::Zero();
			Vector3d tideVectorPole		= Vector3d::Zero();
			if	( acsConfig.model.tides.solid
				||acsConfig.model.tides.otl
				||acsConfig.model.tides.pole)
			{
				tidedisp(trace, gpst2utc(time), rRec, nav.erp, rec.otlDisplacement, tideVectorSum, &tideVectorSolid, &tideVectorOTL, &tideVectorPole);
			}
			
			measEntry.componentList.push_back({"Tides Solid",	-tideVectorSolid.dot(satStat.e), "- E.dT1"});
			measEntry.componentList.push_back({"Tides OTL",		-tideVectorOTL	.dot(satStat.e), "- E.dT2"});
			measEntry.componentList.push_back({"Tides Pole",	-tideVectorPole	.dot(satStat.e), "- E.dT3"});
		}
		
		//Relativity corrections
		//no rel in ppp code 1
		if (acsConfig.model.relativity)
		{
			/* note that relativity effect to estimate sat clock */
			double dtRel1	= relativity1(rSat, obs.satVel);
			
			measEntry.componentList.push_back({"Relativity1", dtRel1	* CLIGHT, "+ rel1"});
		}
		
		if (acsConfig.model.relativity2)
		{
			/* secondary relativity effect (Shapiro effect) */
			double ln		= log((rSat.norm() + rRec.norm() + rRecSat)
							/ (obs.rSat.norm() + rRec.norm() - rRecSat));
			double dtRel2 	= 2 * MU * ln / CLIGHT / CLIGHT / CLIGHT;
								
			measEntry.componentList.push_back({"Relativity2", dtRel2	* CLIGHT, "+ rel2"});
		}
		
		//Sagnac effect
		if (acsConfig.model.sagnac)
		{
			double dSagnac = sagnac(rSat, rRec);
			
			measEntry.componentList.push_back({"Sagnac", dSagnac, "+ sag"});
		}
		
		//Ionospheric delay
		if (acsConfig.model.ionospheric_component)
		{
			double ionosphere_m = 0;
			double freq = CLIGHT / lambda;
			double alpha = 40.3e16 / SQR(freq);
			//calculate the ionosphere values, variances, and gradients at the operating points
			
			InitialState init = initialStateFromConfig(recOpts.ion);
				
			if (init.estimate)
			{
				KFKey kfKey;
				kfKey.type	= KF::IONO_STEC;
				kfKey.str	= rec.id;
				kfKey.Sat	= obs.Sat;
// 				if (acsConfig.common_atmosphere				== false)		{		kfKey.str	= rec.id;		}	//dont use common atmospheres for all receivers
				if (acsConfig.ionoOpts.common_ionosphere	== false)		{		kfKey.num	= measType;		}	//dont use common ionospheres for code and phase
				
				double ionosphere_stec  = 0;
				
				if (measType == CODE)		alpha *= +1;
				else						alpha *= -1;
				
				kfState.getKFValue(kfKey, ionosphere_stec);
				
				ionosphere_m = alpha * ionosphere_stec;
				
				init.Q = 100;
				
				measEntry.addDsgnEntry(kfKey, alpha, init);
			}			
			else
			{
				double diono	= 0;
				double dummy	= 0;
				double varIono	= 0;
				bool pass = model_iono(time, pos, satStat.azel, dummy, diono, varIono);
				if (pass)
				{
					double ionC = SQR(lambda / obs.satNav_ptr->lamMap[F1]);
					
					double sign;
					if (measType == CODE)		sign = +1;
					else						sign = -1;   
										
					ionosphere_m = sign * ionC * diono;
					
					ObsKey obsKey;
					obsKey.str	= rec.id;
					obsKey.Sat	= Sat;
					obsKey.type	= "iono";
// 					obsKey.num	= ft + measType * 100;	//todo aaron remove meastype
					
					measEntry.addNoiseEntry(obsKey, sign * SQR(ionC), varIono);
				}
			}
			
			measEntry.componentList.push_back({"Ionospheric component", ionosphere_m, "+ " + std::to_string(alpha) + ".I"});
		}
		
		//Tropospheric delay
		if (acsConfig.model.trop.enable)
		{
			double tropStates[3]	= {};
			double dTropDx[3]		= {};
			double varTrop			= 0;

			double troposphere_m = 0;
			//calculate the trop values, variances, and gradients at the operating points
			
			InitialState init = initialStateFromConfig(recOpts.trop);
				
			if (init.estimate)
			{
				KFKey kfKey;
				kfKey.type	= KF::TROP;
				if (acsConfig.common_atmosphere == false)
				{
					kfKey.str	= rec.id;
				}
				
				//get the previous filter states for linearisation around this operating point
				for (short i = 0; i < 3; i++)
				{
					auto kfKeyX = kfKey;
					kfKeyX.num = i;
					
					bool pass = kfState.getKFValue(kfKeyX, tropStates[i]);
					if	( (i	== 0)
						&&(pass == false))
					{
						double ztd = sbstropcorr(time, rRec, PI/2);
						tropStates[i] = ztd;
					}
				}
				
				troposphere_m = trop_model_prec(obs.time, pos, satStat.azel, tropStates, dTropDx, varTrop);
				
				init.x = tropStates[0];
				init.P = varTrop;
				
				measEntry.addDsgnEntry(kfKey, dTropDx[0], init);
				
// 				if (recOpts.trop_grads.estimate)
				for (short i = 0; i < 2; i++)
				{
					InitialState init = initialStateFromConfig(recOpts.trop_grads, i);
					
					if (init.estimate == false)
					{
						continue;
					}
					
					auto kfKeyX = kfKey;
					kfKeyX.num = i+1;
					
					measEntry.addDsgnEntry(kfKeyX, dTropDx[i+1], init);
				}
			}
			else
			{
				ObsKey obsKey;
				obsKey.str	= rec.id;
				obsKey.Sat	= obs.Sat;
				obsKey.type	= "trop";
				measEntry.addNoiseEntry(obsKey, 1, varTrop);
			}
			
			measEntry.componentList.push_back({"Troposphere", troposphere_m, "+ " + std::to_string(dTropDx[0]) + ".T"});

			{
				// 		double tropGrads_m = 0;
				// 		componentList.push_back({"Troposphere Gradients", tropGrads_m});
			}
		}
		
		//Phase wind-up
		if (acsConfig.model.phase_windup)
		if (measType == PHAS)
		{
			bool pass = model_phw(time, obs, rRec, satStat.phw);
			if (pass == false)
			{
				continue;
			}
		
			double phaseWindup_m = satStat.phw * lambda;
			
			measEntry.componentList.push_back({"Phase Wind-up", phaseWindup_m, "+ phi"});
		}
		
		//Phase integer ambiguity
		if (acsConfig.model.integer_ambiguity)
		if (measType == PHAS)
		{
			double ambiguity_m = 0;
			
			InitialState init		= initialStateFromConfig(recOpts.amb);
			
			if (init.estimate)
			{
				KFKey kfKey;
				kfKey.type		= KF::AMBIGUITY;
				kfKey.str		= rec.id;
				kfKey.Sat		= obs.Sat;
				kfKey.num		= ft;
				kfKey.rec_ptr	= &rec;
				
				
				double ambiguity = 0;
				
				kfState.getKFValue(kfKey, ambiguity);
				
				ambiguity_m = ambiguity * lambda;
				
				init.P /= SQR(lambda);
				
				measEntry.addDsgnEntry(kfKey, lambda, init);
			}
			
			measEntry.componentList.push_back({"Phase Ambiguity", ambiguity_m, "+ " + std::to_string(lambda) + ".N"});
		}
		
		//Receiver Code biases
		if (acsConfig.model.rec_code_bias)
		if (measType == CODE)
		{
			double	recCodeBias		= 0;
			double	recCodeBiasVar	= DEFAULT_BIAS_VAR;
			getBiasSinex(trace, time, rec.id, Sat, sig.code, CODE, recCodeBias, recCodeBiasVar);
				
			InitialState init		= initialStateFromConfig(recOpts.code_bias, ft);
			
			if (init.estimate)
			if (ft != F1)
			{
				
				KFKey kfKey;
				kfKey.type	= KF::CODE_BIAS;
				kfKey.str	= rec.id;
				kfKey.num	= ft;
				
				kfState.getKFValue(kfKey, recCodeBias);
				
				measEntry.addDsgnEntry(kfKey, 1, init);
			}
			else
			{
				ObsKey obsKey;
				obsKey.str	= rec.id;
				obsKey.type	= "recCodeBias";
				obsKey.num	= ft;
				measEntry.addNoiseEntry(obsKey, 1, recCodeBiasVar);
			}
				
			measEntry.componentList.push_back({"Rec Code Bias", recCodeBias, "+ d_" + std::to_string(ft) + "r"}); 
		}
		
		//Satellite Code biases
		if (acsConfig.model.sat_code_bias)
		if (measType == CODE)
		{
			double	satCodeBias		= 0;
			double	satCodeBiasVar	= DEFAULT_BIAS_VAR;
			getBiasSinex(trace, time, Sat.id(), Sat, sig.code, CODE, satCodeBias, satCodeBiasVar);
				
			InitialState init		= initialStateFromConfig(satOpts.code_bias, ft);
			
			if (init.estimate)
			if (ft != F1)
			{
				KFKey kfKey;
				kfKey.type	= KF::CODE_BIAS;
				kfKey.Sat	= Sat;
				kfKey.num	= ft;
				
				kfState.getKFValue(kfKey, satCodeBias);
				
				measEntry.addDsgnEntry(kfKey, 1, init);
			}
			else
			{
				ObsKey obsKey;
				obsKey.Sat	= obs.Sat;
				obsKey.type	= "satCodeBias";	
				obsKey.num	= ft;
				measEntry.addNoiseEntry(obsKey, 1, satCodeBiasVar);
			}
			
			measEntry.componentList.push_back({"Sat Code Bias", satCodeBias, "+ d_" + std::to_string(ft) + "s"});
		}
		
		//Receiver Phase biases
		if (acsConfig.model.rec_phase_bias)
		if (measType == PHAS)
		{
			double	recPhasBias		= 0;
			double	recPhasBiasVar	= DEFAULT_BIAS_VAR;
			getBiasSinex(trace, time, rec.id, Sat, sig.code, PHAS, recPhasBias, recPhasBiasVar);
				
			InitialState init		= initialStateFromConfig(recOpts.phase_bias, ft);

			if (init.estimate)
			{
				KFKey kfKey;
				kfKey.type	= KF::PHASE_BIAS;
				kfKey.str	= rec.id;
				kfKey.num	= ft;
				
				kfState.getKFValue(kfKey, recPhasBias);
				
				measEntry.addDsgnEntry(kfKey, 1, init);
			}
			else
			{
				ObsKey obsKey;
				obsKey.str	= rec.id;
				obsKey.type	= "recPhasBias";
				obsKey.num	= ft;
				measEntry.addNoiseEntry(obsKey, 1, recPhasBiasVar);
			}
			
			measEntry.componentList.push_back({"Rec Phase Bias", recPhasBias, "+ b_" + std::to_string(ft) + "r"});
		}
	
		//Satellite Phase biases
		if (acsConfig.model.sat_phase_bias)
		if (measType == PHAS)
		{
			double	satPhasBias		= 0;
			double	satPhasBiasVar	= DEFAULT_BIAS_VAR;
			getBiasSinex(trace, time, Sat.id(), Sat, sig.code, PHAS, satPhasBias, satPhasBiasVar);
				
			InitialState init		= initialStateFromConfig(satOpts.phase_bias, ft);
			
			if (init.estimate)
			{
				KFKey kfKey;
				kfKey.type	= KF::PHASE_BIAS;
				kfKey.Sat	= Sat;
				kfKey.num	= ft;
				
				kfState.getKFValue(kfKey, satPhasBias);
				
				measEntry.addDsgnEntry(kfKey, 1, init);
			}
			else
			{
				ObsKey obsKey;
				obsKey.Sat	= obs.Sat;
				obsKey.type	= "satPhasBias";
				obsKey.num	= ft;
				measEntry.addNoiseEntry(obsKey, 1, satPhasBiasVar);
			}
			
			measEntry.componentList.push_back({"Sat Phase Bias", satPhasBias, "+ b_" + std::to_string(ft) + "s"});
		}
		
		//Satellite orbit adjustments
		if (0)
		{
			VectorXd orbitPartials = satNav.satPartialMat * satStat.e;

			if (satNav.satOrbit.numUnknowns == orbitPartials.rows())
			for (int i = 0; i < satNav.satOrbit.numUnknowns; i++)
			{
				string name = satNav.satOrbit.parameterNames[i];
				KFKey kfKey;
				kfKey.type	= KF::ORBIT_PTS;
				kfKey.Sat	= obs.Sat;
				kfKey.str	= std::to_string(100 + i).substr(1) + "_" + name;

				double adjustment;
				
				kfState.getKFValue(kfKey, adjustment);
				
				InitialState init	= initialStateFromConfig(satOpts.orb, i);
				measEntry.addDsgnEntry(kfKey, orbitPartials(i), init);
				
				double computed = adjustment * orbitPartials(i);
				
				measEntry.componentList.push_back({"Orbit " + name, computed, "+ dOrb"});
			}
		}

		//Calculate residuals and form up the measurement
		
		measEntry.componentList.push_back({"Net Residual", 0, ""});
		double residual = 0;
		for (auto& [componentName, componentVal, eq] : measEntry.componentList)
		{
			residual -= componentVal;
			
			if (acsConfig.output_residual_chain)
			{
				tracepdeex(0, trace, "\n%-20s %+14.4f -> %13.4f", componentName.c_str(), -componentVal, residual);
			}
		}

		if (acsConfig.output_residual_chain)
		{
			trace << std::endl << std::endl << "0 =";
		
			for (auto& [componentName, componentVal, eq] : measEntry.componentList)
			{
				tracepdeex(0, trace, " %s", eq.c_str());
			}
		}
		measEntry.setInnov(residual);
		
		kfMeasEntryList.push_back(measEntry);
	}
	
	trace << std::endl << std::endl;
}




// check using same frame, check using ecef velocities
