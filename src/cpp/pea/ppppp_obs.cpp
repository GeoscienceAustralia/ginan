
// #pragma GCC optimize ("O0")

#include "eigenIncluder.hpp"
#include "streamTrace.hpp"
#include "corrections.hpp"
#include "instrument.hpp"
#include "acsConfig.hpp"
#include "biasSINEX.hpp"
#include "antenna.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "common.hpp"
#include "tides.hpp"



#define DEFAULT_BIAS_VAR SQR(2)


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
		
		if (satOpts.orb.estimate)
			orbPartials(trace, time, Sat, satNav.satPartialMat);	
	}
	
	for (auto& obs			: rec.obsList)
	for (int measType		: {PHAS, CODE})
	for (auto& [ft, sig]	: obs.Sigs)
	{
		if (obs.exclude)
		{
			continue;
		}
		
		if (acsConfig.process_freq[ft] == false)
		{
			continue;
		}
		
		SatNav&		satNav	= *obs.satNav_ptr;
		SatStat&	satStat	= *obs.satStat_ptr;
		SigStat&	sigStat	= satStat.sigStatMap[ft];
		
		auto& satOpts = acsConfig.getSatOpts(obs.Sat);
		auto& recOpts = acsConfig.getRecOpts(rec.id);
		
		if (satOpts.exclude)
		{
			continue;
		}
		
		if (recOpts.exclude)
		{
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
		
		list<tuple<string, double>> obsComponentList;
		
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
		if		(measType == CODE)		measEntry.obsKey.type = string("P ") + code._to_string();
		else if	(measType == PHAS)		measEntry.obsKey.type = string("L ") + code._to_string();
		
		//Start with the observed measurement and its noise
		
		obsComponentList.push_back({"Observed", -observed});
		{
			ObsKey obsKey;
			obsKey.str	= obs.mount;
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
		
		Vector3d vRec = Vector3d::Zero();
		{
			if (recOpts.pos_rate.estimate)
			for (int i = 0; i < 3; i++)
			{
				KFKey kfKey;
				kfKey.type	= KF::REC_POS_RATE;
				kfKey.str	= obs.mount;
				kfKey.num	= i;
				
				kfState.getKFValue(kfKey, vRec[i]);
				
				InitialState init = initialStateFromConfig(recOpts.pos_rate, i);
				
// 				measEntry.addDsgnEntry(kfKey, -satStat.e[i], init);
			}
		}
		
		Vector3d rRec = Vector3d::Zero();
		{
			bool sppInit = false;
			selectAprioriSource(rec, sppInit);

			rRec = rec.aprioriPos;
			
			if	(  recOpts.pos.estimate
				&& recOpts.keplers.estimate)
			{
				std::cout << "Incompatible estimation schemes" << std::endl;
			}
			
			if (recOpts.pos.estimate)
			for (int i = 0; i < 3; i++)
			{
				KFKey kfKey;
				kfKey.type	= KF::REC_POS;
				kfKey.str	= obs.mount;
				kfKey.num	= i;
				
				kfState.getKFValue(kfKey, rRec[i]);
				
				InitialState init = initialStateFromConfig(recOpts.pos, i);
				init.x = rRec[i];
				
				measEntry.addDsgnEntry(kfKey, -satStat.e[i], init);
			}
		}
		
		
			
		
		
		double pos[3];
		ecef2pos(rRec, pos);
		
		bool satNoise = true;
		Vector3d rSat = Vector3d::Zero();
		{
			//initialise using ephemeris values if available
			rSat					= obs.rSat;
			
			if (satOpts.pos.estimate)
			{
				Vector3d omegaVector	= Vector3d(0, 0, OMGE);
				Vector3d rSatInertial	= i2tMatrix.transpose() * rSat;
				Vector3d vSatInertial 	= i2tMatrix.transpose() * obs.satVel 
										+ omegaVector.cross(rSatInertial);
										
				Vector3d lookVectorInertial = i2tMatrix.transpose() * satStat.e;
				
				KFKey satPosKeys[3];
				KFKey satVelKeys[3];
				for (int i = 0; i < 3; i++)
				{
					satPosKeys[i].type	= KF::SAT_POS;
					satPosKeys[i].Sat	= Sat;
					satPosKeys[i].num	= i;
					
					satVelKeys[i].type	= KF::SAT_POS_RATE;
					satVelKeys[i].Sat	= Sat;
					satVelKeys[i].num	= i;
				}
				
				if (satOpts.pos.estimate)
				for (int i = 0; i < 3; i++)
				{
					bool found = kfState.getKFValue(satPosKeys[i], rSatInertial[i]);
					if (found) 
					{
						satNoise = false;
					}
				}
				
				
				if (satOpts.pos_rate.estimate)
				for (int i = 0; i < 3; i++)
				{				
					kfState.getKFValue(satVelKeys[i], vSatInertial[i]);
				}
				
				
				if (satOpts.pos.estimate)
				for (int i = 0; i < 3; i++)
				{
					InitialState init = initialStateFromConfig(satOpts.pos,			i);
					init.x = rSatInertial[i];
					
					measEntry.addDsgnEntry(satPosKeys[i], lookVectorInertial[i], init);
				}
				
				if (satOpts.pos_rate.estimate)
				for (int i = 0; i < 3; i++)
				{
					InitialState init = initialStateFromConfig(satOpts.pos_rate,	i);
					
					init.x = vSatInertial[i];
					
					kfState.setKFTransRate(satPosKeys[i], satVelKeys[i],	1,	init);
				}
				
				//refer actual time to estimated time
				//add offset from orbital motion				
				double approxRange	= sig.P;
				double flightTime	= approxRange / CLIGHT;
		
				rSatInertial -= flightTime * vSatInertial;
				
	// 			rSat = i2tMatrix * rSatInertial;
			}
			else
			{
				ObsKey obsKey;
				obsKey.Sat	= obs.Sat;
				obsKey.type	= "satEph";
				
				measEntry.addNoiseEntry(obsKey, 1, obs.ephVar);
			}
		}
		
		//Range
// 		if (0)
		double rRecSat = (rSat - rRec).norm();
		{
			obsComponentList.push_back({"Range", rRecSat});
		}
		
		//Add modelled adjustments and estimated parameter
		
		//Receiver Clock
		
// 		if (0)
		{
			double	recClk_m = 0;
			double	precDtRecVar	= 0;
			double	precDtRec		= 0;
			pephclk(obs.time, obs.mount, nav, precDtRec, &precDtRecVar);
			
			recClk_m		= precDtRec * CLIGHT;
			precDtRecVar	*= SQR(CLIGHT);
			
			if (recOpts.clk.estimate)
			{
				KFKey kfKey;
				kfKey.type		= KF::REC_CLOCK;
				kfKey.str		= obs.mount;
				kfKey.rec_ptr	= &rec;
				// 			kfKey.num	= i;
				
				
				bool found = kfState.getKFValue(kfKey,	recClk_m);
				
				InitialState init		= initialStateFromConfig(recOpts.clk);
				init.x = recClk_m;
				init.P = 4;
				
				measEntry.addDsgnEntry(kfKey, 1, init);
			}
			else
			{
				ObsKey obsKey;
				obsKey.str	= obs.mount;
				obsKey.type	= "recClk";		
				measEntry.addNoiseEntry(obsKey, 1, precDtRecVar);
			}
				
			obsComponentList.push_back({"Rec Clock", recClk_m});
		}
		
		//Satellite Clock
// 		if (0)
		{
			double satClk_m = obs.dtSat[0] * CLIGHT;
			
			if (satOpts.clk.estimate)
			{
				KFKey kfKey;
				kfKey.type	= KF::SAT_CLOCK;
				kfKey.Sat	= Sat;
				kfState.getKFValue(kfKey,	satClk_m);
			
				InitialState init		= initialStateFromConfig(satOpts.clk);
				init.x = satClk_m;
				
				measEntry.addDsgnEntry(kfKey, -1, init);
			}
			obsComponentList.push_back({"Sat Clock", -satClk_m});
		}
		
		//Receiver Antenna delta
// 		if (0)
		{
			Vector3d recAntVector;
			enu2ecef(pos, rec.rtk.opt.antdel, recAntVector);
			
			{
				
			}
			double recAntDelta = -recAntVector.dot(satStat.e);
			obsComponentList.push_back({"Rec Antenna Delta", recAntDelta});
		}
		
		//Satellite Antenna delta
 		if (0)
		{
			
		}
			
		//Receiver Phase Center Offset
// 		if (0)
		{
			Vector3d pco_enu = antPco(rec.rtk.antId, ft, time);
			
			/* receiver pco correction to the coordinates */
			Vector3d recPCOVector = Vector3d::Zero();
			enu2ecef(pos, pco_enu, recPCOVector);    /* convert enu to xyz */
			
			double recPCODelta = -recPCOVector.dot(satStat.e);
			obsComponentList.push_back({"Rec PCO", recPCODelta});
		}
		
		//Satellite Phase Center Offset
		// if (0)
		{
			Vector3d satPCOVector = satAntOff(trace, time, obs.rSat, Sat, ft, &satStat);
			
			double satPCODelta = satPCOVector.dot(satStat.e);
			obsComponentList.push_back({"Sat PCO", satPCODelta});
		}
		
		//Receiver Phase Center Variation
		if (acsConfig.rec_pcv)
		{
			double recPCVDelta = antPcv(rec.rtk.antId, ft, time, PI/2 - satStat.el, satStat.az);
			
			obsComponentList.push_back({"Rec PCV", recPCVDelta});
		}
		
		//Satellite Phase Center Variation
		if (acsConfig.sat_pcv)
		{
			satStat.nadir = satNadir(rSat, rRec);	//todo aaron move up
			
			double satPCVDelta = antPcv(Sat.id(), ft, time, satStat.nadir);
			
			obsComponentList.push_back({"Sat PCV", satPCVDelta});
		}
		
		//Tide delta
// 		if (0)
		{
			Vector3d tideVectorSum		= Vector3d::Zero();
			Vector3d tideVectorSolid	= Vector3d::Zero();
			Vector3d tideVectorOTL		= Vector3d::Zero();
			Vector3d tideVectorPole		= Vector3d::Zero();
			if	( acsConfig.tide_solid
				||acsConfig.tide_otl
				||acsConfig.tide_pole)
			{
				tidedisp(trace, gpst2utc(time), rRec, nav.erp, &rec.rtk.opt.otlDisplacement[0][0], tideVectorSum, &tideVectorSolid, &tideVectorOTL, &tideVectorPole);
			}
			
			obsComponentList.push_back({"Tides Solid",	-tideVectorSolid.dot(satStat.e)});
			obsComponentList.push_back({"Tides OTL",	-tideVectorOTL	.dot(satStat.e)});
			obsComponentList.push_back({"Tides Pole",	-tideVectorPole	.dot(satStat.e)});
		}
		
		//Relativity corrections
		//no rel in ppp code 1
		// if (0)
		{
			/* note that relativity effect to estimate sat clock */
			double dtRel1	= relativity1(rSat, obs.satVel);
			
			/* secondary relativity effect (Shapiro effect) */
			double ln		= log((rSat.norm() + rRec.norm() + rRecSat)
							/ (obs.rSat.norm() + rRec.norm() - rRecSat));
			double dtRel2 	= 2 * MU * ln / CLIGHT / CLIGHT / CLIGHT;
								
			obsComponentList.push_back({"Relativity1", dtRel1	* CLIGHT});
			obsComponentList.push_back({"Relativity2", dtRel2	* CLIGHT});
		}
		
		//Sagnac effect
// 		if (0)
		{
			double dSagnac = sagnac(rSat, rRec);
			
			obsComponentList.push_back({"Sagnac", dSagnac});
		}
		
		//Ionospheric delay
// 		if (0)
		{
			double ionosphere_m = 0;
			//calculate the ionosphere values, variances, and gradients at the operating points
			if (recOpts.ion.estimate)
			{
				KFKey kfKey;
				kfKey.type	= KF::IONO_STEC;
				kfKey.str	= obs.mount;
				kfKey.Sat	= obs.Sat;
				if (acsConfig.ionoOpts.common_ionosphere == false)
				{
					//use separate ionospheres for code and phase
					kfKey.num	= measType;
				}
				
				double freq = CLIGHT / lambda;
				double alpha = 40.3e16 / SQR(freq);
				
				double ionosphere_stec  = 0;
				
				if (measType == CODE)		alpha *= +1;
				else						alpha *= -1;
				
				kfState.getKFValue(kfKey, ionosphere_stec);
				
				ionosphere_m = alpha * ionosphere_stec;
				
				InitialState init = initialStateFromConfig(recOpts.ion);
				init.Q = 100;
				
				measEntry.addDsgnEntry(kfKey, alpha, init);
			}
			
			obsComponentList.push_back({"Ionosphere", ionosphere_m});
		}
		
		//Tropospheric delay
// 		if (0)
		{
			double tropStates[3]	= {};
			double dTropDx[3]		= {};
			double varTrop			= 0;

			double troposphere_m = 0;
			//calculate the trop values, variances, and gradients at the operating points
			if (recOpts.trop.estimate)
			{
				KFKey kfKey;
				kfKey.type	= KF::TROP;
				kfKey.str	= obs.mount;
				
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
				
				InitialState init = initialStateFromConfig(recOpts.trop);
				init.x = tropStates[0];
				init.P = varTrop;
				
				measEntry.addDsgnEntry(kfKey, dTropDx[0], init);
				
				if (recOpts.trop_grads.estimate)
				for (short i = 0; i < 2; i++)
				{
					auto kfKeyX = kfKey;
					kfKeyX.num = i+1;
					
					InitialState init = initialStateFromConfig(recOpts.trop_grads, i);
					
					measEntry.addDsgnEntry(kfKeyX, dTropDx[i+1], init);
				}
			}
			else
			{
				ObsKey obsKey;
				obsKey.str	= obs.mount;
				obsKey.Sat	= obs.Sat;
				obsKey.type	= "trop";
				measEntry.addNoiseEntry(obsKey, 1, varTrop);
			}
			
			obsComponentList.push_back({"Troposphere", troposphere_m});
			
			{
				// 		double tropGrads_m = 0;
				// 		componentList.push_back({"Troposphere Gradients", tropGrads_m});
			}
		}
		
		//Phase wind-up
		if (acsConfig.phase_windup)
		if (measType == PHAS)
		{
			bool pass = model_phw(time, obs, rRec, satStat.phw);
			if (pass == false)
			{
				continue;
			}
		
			double phaseWindup_m = satStat.phw * lambda;
			
			obsComponentList.push_back({"Phase Wind-up", phaseWindup_m});
		}
		
		//Phase integer ambiguity
		if (measType == PHAS)
		{
			double ambiguity_m = 0;
			
			if (recOpts.amb.estimate)
			{
				KFKey kfKey;
				kfKey.type		= KF::AMBIGUITY;
				kfKey.str		= obs.mount;
				kfKey.Sat		= obs.Sat;
				kfKey.num		= ft;
				kfKey.rec_ptr	= &rec;
				
				
				double ambiguity = 0;
				
				kfState.getKFValue(kfKey, ambiguity);
				
				ambiguity_m = ambiguity * lambda;
				
				InitialState init		= initialStateFromConfig(recOpts.amb);
				init.P /= SQR(lambda);
				
				measEntry.addDsgnEntry(kfKey, lambda, init);
			}
			
			obsComponentList.push_back({"Phase Ambiguity", ambiguity_m});
		}
		
		//Receiver Code biases
		if (measType == CODE)
		{
			double	recCodeBias		= 0;
			double	recCodeBiasVar	= DEFAULT_BIAS_VAR;
			getBiasSinex(trace, time, rec.id, Sat, sig.code, CODE, recCodeBias, recCodeBiasVar);
			
			if (recOpts.code_bias.estimate)
			if (ft != F1)
			{
				
				KFKey kfKey;
				kfKey.type	= KF::CODE_BIAS;
				kfKey.str	= obs.mount;
				kfKey.num	= ft;
				
				kfState.getKFValue(kfKey, recCodeBias);
				
				InitialState init		= initialStateFromConfig(recOpts.code_bias, ft);
				
				measEntry.addDsgnEntry(kfKey, 1, init);
			}
			else
			{
				ObsKey obsKey;
				obsKey.str	= obs.mount;
				obsKey.type	= "recCodeBias";
				obsKey.num	= ft;
				measEntry.addNoiseEntry(obsKey, 1, recCodeBiasVar);
			}
				
			obsComponentList.push_back({"Rec Code Bias", recCodeBias}); 
		}
		
		//Satellite Code biases
		if (measType == CODE)
		{
			double	satCodeBias		= 0;
			double	satCodeBiasVar	= DEFAULT_BIAS_VAR;
			getBiasSinex(trace, time, Sat.id(), Sat, sig.code, CODE, satCodeBias, satCodeBiasVar);
			
			if (satOpts.code_bias.estimate)
			if (ft != F1)
			{
				KFKey kfKey;
				kfKey.type	= KF::CODE_BIAS;
				kfKey.Sat	= Sat;
				kfKey.num	= ft;
				
				kfState.getKFValue(kfKey, satCodeBias);
				
				InitialState init		= initialStateFromConfig(satOpts.code_bias, ft);
				
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
			
			obsComponentList.push_back({"Sat Code Bias", satCodeBias});
		}
		
		//Receiver Phase biases
		if (measType == PHAS)
		{
			double	recPhasBias		= 0;
			double	recPhasBiasVar	= DEFAULT_BIAS_VAR;
			getBiasSinex(trace, time, rec.id, Sat, sig.code, PHAS, recPhasBias, recPhasBiasVar);

			if (recOpts.phase_bias.estimate)
			{
				KFKey kfKey;
				kfKey.type	= KF::PHASE_BIAS;
				kfKey.str	= obs.mount;
				kfKey.num	= ft;
				
				kfState.getKFValue(kfKey, recPhasBias);
				
				InitialState init		= initialStateFromConfig(recOpts.phase_bias, ft);
				
				measEntry.addDsgnEntry(kfKey, 1, init);
			}
			else
			{
				ObsKey obsKey;
				obsKey.str	= obs.mount;
				obsKey.type	= "recPhasBias";
				obsKey.num	= ft;
				measEntry.addNoiseEntry(obsKey, 1, recPhasBiasVar);
			}
			
			obsComponentList.push_back({"Rec Phase Bias", recPhasBias});
		}
	
		//Satellite Phase biases
		if (measType == PHAS)
		{
			double	satPhasBias		= 0;
			double	satPhasBiasVar	= DEFAULT_BIAS_VAR;
			getBiasSinex(trace, time, Sat.id(), Sat, sig.code, PHAS, satPhasBias, satPhasBiasVar);
			
			if (satOpts.phase_bias.estimate)
			{
				KFKey kfKey;
				kfKey.type	= KF::PHASE_BIAS;
				kfKey.Sat	= Sat;
				kfKey.num	= ft;
				
				kfState.getKFValue(kfKey, satPhasBias);
				
				InitialState init		= initialStateFromConfig(satOpts.phase_bias, ft);
				
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
			
			obsComponentList.push_back({"Sat Phase Bias", satPhasBias});
		}
		
		//Satellite orbit adjustments
		if (satOpts.orb.estimate)
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
				
				obsComponentList.push_back({"Orbit " + name, computed});
			}
		}

		//Calculate residuals and form up the measurement
		
		obsComponentList.push_back({"Net Residual", 0});
		double residual = 0;
		for (auto& [componentName, componentVal] : obsComponentList)
		{
			residual -= componentVal;
			
			if (acsConfig.output_residual_chain)
			{
				tracepdeex(0, trace, "\n%-20s %+14.4f -> %13.4f", componentName.c_str(), -componentVal, residual);
			}
		}
		measEntry.setInnov(residual);
		
		kfMeasEntryList.push_back(measEntry);
	}
	
	trace << std::endl << std::endl;
}




// check using same frame, check using ecef velocities
