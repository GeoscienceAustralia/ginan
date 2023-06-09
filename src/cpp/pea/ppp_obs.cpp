
// #pragma GCC optimize ("O0")

/** References
* 1. M.Fritsche, R.Dietrich, C.Knöfel, A.Rülke, S.Vey, M.Rothacher & P.Steigenberger, Impact of higher‐order ionospheric terms on GPS estimates. Geophysical research letters, 2005.
* 2. GAMIT 10.71
* 3. U.Hugentobler, S.Schaer, G.Beutler, H.Bock, R.Dach, A.Jäggi, M.Meindl, C.Urschl, L.Mervart, M.Rothacher & U.Wild, CODE IGS analysis center technical report 2002, 2002.
*/


#include "eigenIncluder.hpp"
#include "corrections.hpp"
#include "coordinates.hpp"
#include "geomagField.hpp"
#include "ephPrecise.hpp"
#include "instrument.hpp"
#include "acsConfig.hpp"
#include "biasSINEX.hpp"
#include "ionoModel.hpp"
#include "orbitProp.hpp"
#include "antenna.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "cache.hpp"
#include "gTime.hpp"
#include "tides.hpp"
#include "trace.hpp"


#define DEFAULT_CODE_BIAS_VAR SQR(2)
#define DEFAULT_PHAS_BIAS_VAR SQR(0.02)


//this hideousness is a horrible hack to make this code compile on macs.
//blame apple for this. look what they've made me do...

//this will ust copy and paste the types and variable names for now, this is undefined later to remove the type names to pass in the parameters in the same order
#define COMMON_ARG(type)    type

#define COMMON_PPP_ARGS											\
	COMMON_ARG(			Trace&				)	trace,			\
	COMMON_ARG(			GObs&				)	obs,			\
	COMMON_ARG(			Station&			)	rec,			\
	COMMON_ARG(			SatSys&				)	Sat,			\
	COMMON_ARG(			Sig&				)	sig,			\
	COMMON_ARG(			string&				)	sigName,		\
	COMMON_ARG(const	E_FType&			)	ft,				\
	COMMON_ARG(			E_MeasType&			)	measType,		\
	COMMON_ARG(			GTime&				)	time,			\
	COMMON_ARG(			SatStat&			)	satStat,		\
	COMMON_ARG(			SatNav&				)	satNav,			\
	COMMON_ARG(			ReceiverOptions&	)	recOpts,		\
	COMMON_ARG(			SatelliteOptions&	)	satOpts,		\
	COMMON_ARG(const	KFState&			)	kfState,		\
	COMMON_ARG(			KFMeasEntry&		)	measEntry,		\
	COMMON_ARG(			VectorEcef&			)	rRec,			\
	COMMON_ARG(			VectorEcef&			)	rSat,			\
	COMMON_ARG(			double&				)	rRecSat,		\
	COMMON_ARG(			double&				)	lambda,			\
	COMMON_ARG(			SatSys&				)	sysSat,			\
	COMMON_ARG(			VectorPos&			)	pos


inline void pppRecClocks(COMMON_PPP_ARGS)
{
	double	dtRecVar	= 0;
	double	dtRec		= 0;
	pephclk(trace, time, rec.id, nav, dtRec, &dtRecVar);		//todo aaron, wrong clock here
	
	double	recClk_m	= dtRec * CLIGHT;
	dtRecVar			*= SQR(CLIGHT);		//todo aaron do we want to use these override variances?
	
	for (int i = 0; i < recOpts.clk.estimate.size(); i++)
	{
		InitialState init		= initialStateFromConfig(recOpts.clk, i);
			
		if (init.estimate == false)
		{
			continue;
		}
		
		if (i == 0)
		{
			init.x		= recClk_m;
			recClk_m	= 0;
		}
		
		KFKey kfKey;
		kfKey.type		= KF::REC_CLOCK;
		kfKey.str		= rec.id;
		kfKey.num		= i;
		kfKey.rec_ptr	= &rec;
		kfKey.comment	= init.comment;
		
		double stateVal = init.x;
		
		kfState.getKFValue(kfKey, stateVal);
		
		recClk_m += stateVal;
		
		measEntry.addDsgnEntry(kfKey, 1, init);
		
		InitialState rateInit = initialStateFromConfig(recOpts.clk_rate, i);
		
		if (rateInit.estimate)
		{
			KFKey rateKey	= kfKey;
			rateKey.type	= KF::REC_CLOCK_RATE;
		
			kfState.setKFTransRate(kfKey, rateKey, 1, rateInit);
		}
	}
		
	measEntry.componentList.push_back({E_Component::REC_CLOCK, recClk_m, "+ Cdt_r"});
};


inline void pppSatClocks(COMMON_PPP_ARGS)
{
	double satClkVar	= 0;
	double satClk_m		= obs.satClk * CLIGHT;
	
	for (int i = 0; i < satOpts.clk.estimate.size(); i++)
	{
		InitialState init		= initialStateFromConfig(satOpts.clk, i);
		
		if (init.estimate == false)
		{
			continue;
		}
		
		if (i == 0)
		{
			init.x		= satClk_m;
			satClk_m	= 0;
		}
		
		KFKey kfKey;
		kfKey.type		= KF::SAT_CLOCK;
		kfKey.Sat		= Sat;
		kfKey.num		= i;
		kfKey.comment	= init.comment;
		
		double stateVal = init.x;
		
		kfState.getKFValue(kfKey, stateVal);
		
		satClk_m += stateVal;
		
		measEntry.addDsgnEntry	(kfKey, -1, init);
		
		InitialState rateInit = initialStateFromConfig(satOpts.clk_rate, i);
		
		if (rateInit.estimate)
		{
			KFKey rateKey	= kfKey;
			rateKey.type	= KF::SAT_CLOCK_RATE;
		
			kfState.setKFTransRate(kfKey, rateKey, 1, rateInit);
		}
	}
	
	measEntry.componentList.push_back({E_Component::SAT_CLOCK, -satClk_m, "- Cdt_s"});
};

inline void pppRecAntDelta(COMMON_PPP_ARGS)
{
	VectorEcef recAntVector = body2ecef(rec.attStatus, rec.antDelta);
	
	double recAntDelta = -recAntVector.dot(satStat.e);

	measEntry.componentList.push_back({E_Component::REC_ANTENNA_DELTA, recAntDelta, "- E.dR_r"});
};

		
inline void pppRecPCO(COMMON_PPP_ARGS)
{
	E_FType recAtxFt = ft;
	if (acsConfig.common_rec_pco)
	{
		if (Sat.sys == +E_Sys::GLO)	recAtxFt = G1;
		else						recAtxFt = F1;
	}
	
	Vector3d pcoEnu = antPco(rec.antennaId, Sat.sys, recAtxFt, time, E_Radio::RECEIVER);

	VectorEcef recPCOVector = antenna2ecef(rec.attStatus, pcoEnu);
	
	double recPCODelta = -recPCOVector.dot(satStat.e);
	
	measEntry.componentList.push_back({E_Component::REC_PCO, recPCODelta, "- E.PCO_r"});
};
		
inline void pppSatPCO(COMMON_PPP_ARGS)
{
	Vector3d satPCOVector;
	
	E_FType satAtxFt = ft;
	
	if (acsConfig.common_rec_pco)
	{
		if (Sat.sys == +E_Sys::GLO)	satAtxFt = G1;
		else						satAtxFt = F1;
	}	
	
	auto& attStatus = satNav.attStatus;
	
	satPCOVector	= satAntOff(trace, time, attStatus, Sat, ft);
	
	double satPCODelta = satPCOVector.dot(satStat.e);
	
	measEntry.componentList.push_back({E_Component::SAT_PCO, satPCODelta, "+ E.PCO_s"});
};

inline void pppRecPCV(COMMON_PPP_ARGS)
{
	E_FType recAtxFt = ft;
	
	if (acsConfig.common_rec_pco)
	{
		if (Sat.sys == +E_Sys::GLO)	recAtxFt = G1;
		else						recAtxFt = F1;
	}
	
	double recPCVDelta = antPcv(rec.antennaId,	Sat.sys, recAtxFt, time, rec.attStatus,		satStat.e * +1);	
	
	measEntry.componentList.push_back({E_Component::REC_PCV, recPCVDelta, "+ PCV_r"});
};

inline void pppSatPCV(COMMON_PPP_ARGS)
{
	E_FType satAtxFt = ft;
	
	if (acsConfig.common_rec_pco)
	{
		if (Sat.sys == +E_Sys::GLO)	satAtxFt = G1;
		else						satAtxFt = F1;
	}
	
	double satPCVDelta = antPcv(Sat.id(),		Sat.sys, satAtxFt, time, satNav.attStatus,	satStat.e * -1);
	
	measEntry.componentList.push_back({E_Component::SAT_PCV, satPCVDelta, "+ PCV_s"});
};

inline void pppTides(COMMON_PPP_ARGS)
{
	auto& [solid, otl, pole] = rec.pppTideCache.useCache([&]() -> tuple<Vector3d, Vector3d, Vector3d>
	{
		Vector3d tideVectorSum		= Vector3d::Zero();
		Vector3d tideVectorSolid	= Vector3d::Zero();
		Vector3d tideVectorOTL		= Vector3d::Zero();
		Vector3d tideVectorPole		= Vector3d::Zero();
		
		if	( acsConfig.model.tides.solid
			||acsConfig.model.tides.otl
			||acsConfig.model.tides.pole)
		{
			tideDisp(trace, time, rRec, nav.erp, rec.otlDisplacement, tideVectorSum, &tideVectorSolid, &tideVectorOTL, &tideVectorPole);
		}
		
		return {tideVectorSolid, tideVectorOTL, tideVectorPole};
	});
	
	measEntry.componentList.push_back({E_Component::TIDES_SOLID,	-solid	.dot(satStat.e), "- E.dT1"});
	measEntry.componentList.push_back({E_Component::TIDES_OTL,		-otl	.dot(satStat.e), "- E.dT2"});
	measEntry.componentList.push_back({E_Component::TIDES_POLE,		-pole	.dot(satStat.e), "- E.dT3"});
};

inline void pppRelativity(COMMON_PPP_ARGS)
{
	/* note that relativity effect to estimate sat clock */
	double dtRel1	= relativity1(rSat, obs.satVel);
	
	measEntry.componentList.push_back({E_Component::RELATIVITY1, dtRel1	* CLIGHT, "+ rel1"});
};

inline void pppRelativity2(COMMON_PPP_ARGS)
{
	/* secondary relativity effect (Shapiro effect) */
	double ln		= log(	(rSat.norm() + rRec.norm() + rRecSat)
						/	(rSat.norm() + rRec.norm() - rRecSat));
	double dtRel2 	= 2 * MU * ln / CLIGHT / CLIGHT / CLIGHT;
						
	measEntry.componentList.push_back({E_Component::RELATIVITY2, dtRel2	* CLIGHT, "+ rel2"});
};

inline void pppSagnac(COMMON_PPP_ARGS)
{
	double dSagnac = sagnac(rSat, rRec);
	
	measEntry.componentList.push_back({E_Component::SAGNAC, dSagnac, "+ sag"});
};

inline void pppIonStec(COMMON_PPP_ARGS)
{
	double ionosphere_m	= 0;
	double varIono		= 0;
	double freq			= CLIGHT / lambda;
	double alpha		= 40.3e16 / SQR(freq);
	double factor		= 1;
	
	if (measType == CODE)		factor = +1;
	else						factor = -1;  
	
	//calculate the ionosphere values, variances, and gradients at the operating points
	
	InitialState init = initialStateFromConfig(recOpts.ion_stec);
			
// 	if (init.estimate == false)
	{
		double diono	= 0;
		double dummy	= 0;
		bool pass = ionoModel(time, pos, satStat.azel, dummy, diono, varIono);
		if (pass)
		{
			double ionC = SQR(lambda / obs.satNav_ptr->lamMap[F1]);
			
			ionosphere_m = factor * ionC * diono;
		}
	}
	
	if (init.estimate)
	{
		if (init.Q < 0)
		{
// 			init.P = varIono;	//bad things happen (no iflc) if this is zero, as is often the case for varIono
		}
		
		KFKey kfKey;
		kfKey.type		= KF::IONO_STEC;
		kfKey.str		= rec.id;
		kfKey.Sat		= obs.Sat;
		kfKey.comment	= init.comment;

		if (acsConfig.ionoOpts.common_ionosphere == false)
			kfKey.num = measType;

		double ionosphere_stec  = 0;
		
		kfState.getKFValue(kfKey, ionosphere_stec);
		
		ionosphere_m = factor * alpha * ionosphere_stec;
		
		measEntry.addDsgnEntry(kfKey, factor * alpha, init);
	}
	
	measEntry.componentList.push_back({E_Component::IONOSPHERIC_COMPONENT, ionosphere_m, "+ " + std::to_string(factor * alpha) + ".I"});
};
		
/** 2nd order ionospheric correction
* See ref [1]
*/
inline void pppIonStec2(COMMON_PPP_ARGS)
{
	double ionosphere_m	= 0;
	double varIono		= 0;
	double freq			= CLIGHT / lambda;
	double alpha_		= 7527e16 * lambda / SQR(freq);
	double factor		= 1;

	if (measType == CODE)		factor = +1;
	else						factor = -1 / 2.0;  

	VectorPos posp;
	ionppp(pos, satStat.azel, RE_MEAN / 1000, acsConfig.ionoOpts.pierce_point_layer_height, posp);

	Vector3d rB = getGeomagIntensityEcef(time, posp) * 1E-9;	// nTesla -> Tesla

	double alpha = -1 * alpha_ * rB.dot(satStat.e);
	
	//calculate the ionosphere values, variances, and gradients at the operating points
	
	InitialState init = initialStateFromConfig(recOpts.ion_stec, 1);
			
	if (init.estimate)
	{
		if (initialStateFromConfig(recOpts.ion_stec).estimate == false)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: Higher order ionosphere estimation requires lower order ionosphere estimation.";
		}
		
		KFKey kfKey;
		kfKey.type	= KF::IONO_STEC;
		kfKey.str	= rec.id;
		kfKey.Sat	= obs.Sat;

		if (acsConfig.ionoOpts.common_ionosphere == false)
			kfKey.num = measType;

		measEntry.addDsgnEntry(kfKey, factor * alpha);
		
		//component will be added in primary ionospheric model
	}
	else
	{
		double diono	= 0;
		double dummy	= 0;
		bool pass = ionoModel(time, pos, satStat.azel, dummy, diono, varIono);
		if (pass)
		{
			double stec = diono * SQR(FREQ1) / TEC_CONSTANT;	// restore STEC
			
			ionosphere_m = factor * alpha * stec;
		}
		
		measEntry.componentList.push_back({E_Component::IONOSPHERIC_COMPONENT1, ionosphere_m, ""});
	}
};
		
/** 3rd order ionospheric correction
* See ref [1]
*/
inline void pppIonStec3(COMMON_PPP_ARGS)
{
	double ionosphere_m	= 0;
	double varIono		= 0;
	double freq			= CLIGHT / lambda;
	double alpha		= 2437.12557e16 / POW4(freq);
	double factor		= 1;
	
	if (measType == CODE)		factor = +1;
	else						factor = -1 / 3.0;

	E_IonoMapFn mapFn;
	if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::BROADCAST)		mapFn = E_IonoMapFn::KLOBUCHAR;
	else															mapFn = acsConfig.ionoOpts.mapping_function;
	
	double fs = ionmapf(pos, satStat.azel, mapFn, acsConfig.ionoOpts.mapping_function_layer_height);
	
	//calculate the ionosphere values, variances, and gradients at the operating points
	
	InitialState init = initialStateFromConfig(recOpts.ion_stec, 2);

	if (init.estimate)
	{
		if (initialStateFromConfig(recOpts.ion_stec).estimate == false)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: Higher order ionosphere estimation requires lower order ionosphere estimation.";
		}
		
		KFKey kfKey;
		kfKey.type	= KF::IONO_STEC;
		kfKey.str	= rec.id;
		kfKey.Sat	= obs.Sat;

		if (acsConfig.ionoOpts.common_ionosphere == false)
			kfKey.num = measType;

		double ionosphere_stec = 0;
		
		kfState.getKFValue(kfKey, ionosphere_stec);
		
		double vtec = ionosphere_stec / fs;	// restore VTEC for nMax calculation

		double nMax = 20.0e12 + 14.0e12 / 3.17e18 * (vtec * 1e16 - 4.55e18);	// calculate nMax with linear interpolation, ref: GAMIT code
		
		if (nMax < 0)	
			nMax = 0;	// avoid being negative
		
		measEntry.addDsgnEntry(kfKey, factor * alpha * (2 * nMax + 0.3e12 / 3.17) * 0.66);
		
		//component will be added in primary ionospheric model
	}
	else
	{
		double diono	= 0;
		double dummy	= 0;
		bool pass = ionoModel(time, pos, satStat.azel, dummy, diono, varIono);
		if (pass)
		{
			double stec = diono * SQR(FREQ1) / TEC_CONSTANT;	// restore STEC
			double vtec = stec / fs;	// restore VTEC for nMax calculation

			// nMax = 14.0e12 / 3.17e18 * stec*1e16;	// calculate nMax, see ref [1]
			double nMax = 20.0e12 + 14.0e12 / 3.17e18 * (vtec * 1e16 - 4.55e18);	// calculate nMax with linear interpolation, ref: GAMIT code
			
			if (nMax < 0)
				nMax = 0;	// avoid being negative

			ionosphere_m = factor * alpha * nMax * 0.66 * stec;
		}
		
		measEntry.componentList.push_back({E_Component::IONOSPHERIC_COMPONENT2, ionosphere_m, ""});
	}
};

inline void pppIonModel(COMMON_PPP_ARGS)
{
	double ionosphere_m	= 0;
	double varIono		= 0;
	double freq			= CLIGHT / lambda;
	double alpha		= TEC_CONSTANT / SQR(freq);
	double sign			= 1;
	
	if (measType == CODE)		sign = +1;
	else						sign = -1;  
	
	InitialState init = initialStateFromConfig(satOpts.ion_model);
			
	if	( init.estimate
		&&acsConfig.use_for_iono_model[obs.Sat.sys])
	{				
		VectorPos posp;
		for (int i = 0; i < acsConfig.ionModelOpts.layer_heights.size(); i++)
		{
			obs.ippMap[i].ang = ionppp(pos, satStat.azel, RE_WGS84 / 1000, acsConfig.ionModelOpts.layer_heights[i] / 1000, posp);
			obs.ippMap[i].lat = posp.lat();
			obs.ippMap[i].lon = posp.lon();
		}

		for (int i = 0; i < acsConfig.ionModelOpts.NBasis; i++)
		{
			double basis = ionModelCoef(i, obs, true);
			if (basis == 0)
				continue;
			
			double coef = 0;
			
			KFKey kfKey;
			kfKey.type	= KF::IONOSPHERIC;
			kfKey.num	= i;
			
			bool pass = kfState.getKFValue(kfKey, coef);
			
			ionosphere_m += sign * alpha * basis * coef;
			
			measEntry.addDsgnEntry(kfKey, sign * alpha * basis, init);
		}
				
		measEntry.componentList.push_back({E_Component::IONOSPHERIC_MODEL, ionosphere_m, "+ " + std::to_string(sign * alpha) + ".I"});
	}
};

inline void pppTrop(COMMON_PPP_ARGS)
{
	double tropStates[3]	= {};
	double dTropDx[3]		= {};
	double varTrop			= 0;

	double troposphere_m	= 0;
	
	string recStr;
	if (acsConfig.pppOpts.common_atmosphere == false)
	{
		recStr	= rec.id;
	}
	
	//get the previous filter states for linearisation around this operating point
	for (int i = 0; i < recOpts.trop.estimate.size(); i++)
	{
		KFKey kfKey;
		kfKey.type	= KF::TROP;
		kfKey.str	= recStr;
		kfKey.num	= i;
		
		double value = 0;
		bool pass = kfState.getKFValue(kfKey, value);
		
		tropStates[0] += value;
	}
	
	for (short i = 0; i < 2; i++)
	{
		KFKey kfKey;
		kfKey.type	= KF::TROP_GRAD;
		kfKey.str	= recStr;
		kfKey.num	= i;
		
		kfState.getKFValue(kfKey, tropStates[i+1]);
	}
	
	if (tropStates[0] == 0)
	{
		tropStates[0] = sbstropcorr(time, rRec, PI/2);
	}
	
	//calculate the trop values, variances, and gradients at the operating points
	troposphere_m = trop_model_prec(time, pos, satStat.azel, tropStates, dTropDx, varTrop);
	obs.tropSlant		= troposphere_m;
	obs.tropSlantVar	= varTrop;
		
	
	for (int i = 0; i < recOpts.trop.estimate.size(); i++)
	{
		InitialState init = initialStateFromConfig(recOpts.trop);
		
		if (init.estimate == false)
		{
			continue;
		}
		
		if (i == 0)
		{
			init.x = tropStates[0];
		}
		
		if (init.Q < 0)
		{
			init.P = varTrop;
		}
		
		KFKey kfKey;
		kfKey.type		= KF::TROP;
		kfKey.str 		= recStr;
		kfKey.num		= i;
		kfKey.comment	= init.comment;
		
		measEntry.addDsgnEntry(kfKey, dTropDx[0], init);
	}
	
	for (short i = 0; i < 2; i++)
	{
		InitialState init = initialStateFromConfig(recOpts.trop_grads, i);
		
		if (init.estimate == false)
		{
			continue;
		}
		
		KFKey kfKey;
		kfKey.type		= KF::TROP_GRAD;
		kfKey.str 		= recStr;
		kfKey.num		= i;
		kfKey.comment	= init.comment;
		
		measEntry.addDsgnEntry(kfKey, dTropDx[i+1], init);
	}
	
	measEntry.componentList.push_back({E_Component::TROPOSPHERE, troposphere_m, "+ " + std::to_string(dTropDx[0]) + ".T"});
};

inline void pppOrbits(COMMON_PPP_ARGS)
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
		
		measEntry.componentList.push_back({E_Component::ORBIT_PT, computed, "+ dOrb"});
	}
};

inline void pppPhaseWindup(COMMON_PPP_ARGS)
{
	phaseWindup(obs, rec, satStat.phw);

	double phaseWindup_m = satStat.phw * lambda;
	
	measEntry.componentList.push_back({E_Component::PHASE_WIND_UP, phaseWindup_m, "+ phi"});
};

inline void pppHeading(COMMON_PPP_ARGS)
{
	double headingWindup_m = 0;
	
	InitialState init		= initialStateFromConfig(recOpts.heading);
	
	if (init.estimate)
	{
		KFKey kfKey;
		kfKey.type		= KF::HEADING;
		kfKey.str		= rec.id;
		kfKey.comment	= init.comment;
		
		double angleOffset;
		
		kfState.getKFValue(kfKey, angleOffset);
		
		headingWindup_m = angleOffset * lambda / (2*PI);
		
		measEntry.addDsgnEntry(kfKey, lambda / (2*PI), init);
	
		measEntry.componentList.push_back({E_Component::HEADING, headingWindup_m, ""});
	}
}

inline void pppIntegerAmbiguity(COMMON_PPP_ARGS)
{
	double ambiguity_m = 0;
	
	InitialState init		= initialStateFromConfig(recOpts.amb);
	
	if (init.estimate)
	{
		KFKey kfKey;
		kfKey.type		= KF::AMBIGUITY;
		kfKey.str		= rec.id;
		kfKey.Sat		= obs.Sat;
		kfKey.num		= sig.code;
		kfKey.rec_ptr	= &rec;
		kfKey.comment	= sigName;
		
		double ambiguity = 0;
		
		kfState.getKFValue(kfKey, ambiguity);
		
		ambiguity_m = ambiguity * lambda;
		
		init.P /= SQR(lambda);
		
		measEntry.addDsgnEntry(kfKey, lambda, init);
	}
	
	measEntry.componentList.push_back({E_Component::PHASE_AMBIGUITY, ambiguity_m, "+ " + std::to_string(lambda) + ".N"});
};

inline void pppRecPhasBias(COMMON_PPP_ARGS)
{
	double	recPhasBias		= 0;
	double	recPhasBiasVar	= DEFAULT_PHAS_BIAS_VAR;
	getBiasSinex(trace, time, rec.id, Sat, sig.code, PHAS, recPhasBias, recPhasBiasVar);
		
	InitialState init		= initialStateFromConfig(recOpts.phase_bias);

	if (init.estimate)
	{
		init.x = recPhasBias;
		
		if (init.Q < 0)
		{
			init.P = recPhasBiasVar;
		}
	
		init.x = recPhasBias;
		
		KFKey kfKey;
		kfKey.type		= KF::PHASE_BIAS;
		kfKey.str		= rec.id;
		kfKey.Sat		= sysSat;
		kfKey.num		= sig.code;
		kfKey.rec_ptr	= &rec;
		kfKey.comment	= sigName;
	
		kfState.getKFValue(kfKey, recPhasBias);
		
		measEntry.addDsgnEntry	(kfKey, 1, init);
	}
	
	measEntry.componentList.push_back({E_Component::REC_PHASE_BIAS, recPhasBias, "+ b_" + std::to_string(ft) + "r"});
};

inline void pppSatPhasBias(COMMON_PPP_ARGS)
{
	double	satPhasBias		= 0;
	double	satPhasBiasVar	= DEFAULT_PHAS_BIAS_VAR;
	getBiasSinex(trace, time, Sat, Sat, sig.code, PHAS, satPhasBias, satPhasBiasVar);
		
	InitialState init		= initialStateFromConfig(satOpts.phase_bias);
	
	if (init.estimate)
	{
		if (init.Q < 0)
		{
			init.P = satPhasBiasVar;
		}
	
		init.x = satPhasBias;
		
		E_ObsCode phsBiaCode = sig.code;
		
		KFKey kfKey;
		kfKey.type		= KF::PHASE_BIAS;
		kfKey.Sat		= Sat;
		kfKey.num		= phsBiaCode;
		kfKey.comment	= sigName;
		
		kfState.getKFValue(kfKey, satPhasBias);
		
		measEntry.addDsgnEntry	(kfKey, 1, init);
	}
	
	measEntry.componentList.push_back({E_Component::SAT_PHASE_BIAS, satPhasBias, "+ b_" + std::to_string(ft) + "s"});
};
		

inline void pppRecCodeBias(COMMON_PPP_ARGS)
{
	double	recCodeBias		= 0;
	double	recCodeBiasVar	= DEFAULT_CODE_BIAS_VAR;
	getBiasSinex(trace, time, rec.id, Sat, sig.code, CODE, recCodeBias, recCodeBiasVar);
		
	InitialState init		= initialStateFromConfig(recOpts.code_bias);
	
	if (init.estimate)
	{
		init.x = recCodeBias;
		
		if (init.Q < 0)
		{
			init.P = recCodeBiasVar;
		}
	
		init.x = recCodeBias;
		
		KFKey kfKey;
		kfKey.type		= KF::CODE_BIAS;
		kfKey.str		= rec.id;
		kfKey.Sat		= sysSat;
		kfKey.num		= sig.code;
		kfKey.rec_ptr	= &rec;
		kfKey.comment	= sigName;
		
		kfState.getKFValue(kfKey, recCodeBias);
		
		measEntry.addDsgnEntry	(kfKey, 1, init);
	}
		
	measEntry.componentList.push_back({E_Component::REC_CODE_BIAS, recCodeBias, "+ d_" + std::to_string(ft) + "r"}); 
};

inline void pppSatCodeBias(COMMON_PPP_ARGS)
{
	double	satCodeBias		= 0;
	double	satCodeBiasVar	= DEFAULT_CODE_BIAS_VAR;
	getBiasSinex(trace, time, Sat, Sat, sig.code, CODE, satCodeBias, satCodeBiasVar);
		
	InitialState init		= initialStateFromConfig(satOpts.code_bias);
	
	if (init.estimate)
	{
		init.x = satCodeBias;
		
		if (init.Q < 0)
		{
			init.P = satCodeBiasVar;
		}
	
		init.x = satCodeBias;
		
		KFKey kfKey;
		kfKey.type		= KF::CODE_BIAS;
		kfKey.Sat		= Sat;
		kfKey.num		= sig.code;
		kfKey.comment	= sigName;
		
		kfState.getKFValue(kfKey, satCodeBias);
		
		measEntry.addDsgnEntry	(kfKey, 1, init);
	}
	
	measEntry.componentList.push_back({E_Component::SAT_CODE_BIAS, satCodeBias, "+ d_" + std::to_string(ft) + "s"});
};	

inline void pppEopAdjustment(COMMON_PPP_ARGS)
{
	if (acsConfig.pppOpts.eop.estimate[0] == false)
	{
		return;
	}
	
	Matrix3d partialMatrix	= stationEopPartials(rec.aprioriPos);
	Vector3d eopPartials	= partialMatrix * satStat.e;

	double adjustment = 0;
	
	for (int i = 0; i < 3; i++)
	{
		InitialState init	= initialStateFromConfig(acsConfig.pppOpts.eop,					i);
		
		if (init.estimate == false)
		{
			continue;
		}
		
		KFKey kfKey;
		kfKey.type		= KF::EOP_ADJUST;
		kfKey.num		= i;
		kfKey.comment	= eopComments[i];
		
		double component = 0;
		kfState.getKFValue(kfKey, component);
		
		adjustment += eopPartials(i) * component;

		measEntry.addDsgnEntry(kfKey,	eopPartials(i),				init);
		
		InitialState eopRateInit	= initialStateFromConfig(acsConfig.pppOpts.eop_rates,	i);
							
		if (eopRateInit.estimate == false)
		{
			continue;
		}
		
		KFKey rateKey;
		rateKey.type	= KF::EOP_RATE_ADJUST;
		rateKey.num		= i;
		kfKey.comment	= eopComments[i];

		kfState.setKFTransRate(kfKey, rateKey,	1/86400.0,	eopRateInit);
	}
	
	measEntry.componentList.push_back({E_Component::EOP, adjustment, "+ eop"});
}

















//redefine this to replace with nothing from now on - ie, use the argument name but not its type
#undef	COMMON_ARG
#define	COMMON_ARG(type)
		
	
void stationPPP(
			Trace&				netTrace,			///< Trace to output to
			Station&			rec,				///< Receiver to perform calculations for
	const	KFState&			kfState,			///< Kalman filter object containing the network state parameters
			KFMeasEntryList&	kfMeasEntryList)	///< List to append kf measurements to
{
	auto trace = getTraceFile(rec);
	
// 	Instrument	instrument(__FUNCTION__ + rec.id);
	
	tracepdeex(0, trace, "\n--------------------- Performing PPP -------------------");
	
		
	if	(  rec.obsList.empty()
		|| rec.invalid)
	{
		tracepdeex(1, trace, "\n\nReceiver not ready for PPP. Obs=%d", rec.obsList.size());
		
		return;
	}
	
	rec.pppTideCache.uninit();
	
	GTime time = rec.obsList.front()->time;
	
	for (auto& obs : only<GObs>(rec.obsList))
	{
		satPosClk(trace, time, obs, nav, acsConfig.model.sat_pos.ephemeris_sources, acsConfig.model.sat_clock.ephemeris_sources, &kfState, E_OffsetType::COM, E_Relativity::OFF);
	}
	
	ERPValues erpv = geterp(nav.erp, time);
	
	FrameSwapper frameSwapper(time, erpv);
	
	for (auto& [Sat, satNav] : nav.satNavMap)
	{
		if (acsConfig.process_sys[Sat.sys] == false)
		{
			continue;
		}
		
		auto& satOpts = acsConfig.getSatOpts(Sat);
		
		InitialState init	= initialStateFromConfig(satOpts.orb);
		if (init.estimate)
			orbPartials(trace, time, Sat, satNav.satPartialMat);	
	}
	
	for (auto&	obs				: only<GObs>(rec.obsList))
	for (auto&	[ft, sigList]	: obs.SigsLists)
	for (auto&	sig				: sigList)
	for (auto	measType		: {PHAS, CODE})
	{
		char measDescription[64];
		
		snprintf(measDescription, sizeof(measDescription), "%s %s %s", obs.Sat.id().c_str(), sig.code._to_string(), (measType == PHAS) ? "L" : "P");
		
		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			tracepdeex(4, trace, "\n%s - System skipped", measDescription);
			continue;
		}
		
		if (acsConfig.process_meas[measType] == false)
		{
			tracepdeex(4, trace, "\n%s - Measurement type skipped", measDescription);
			continue;
		}
		
		auto& sysCodes = acsConfig.code_priorities[obs.Sat.sys];
		
		auto prioritityIt = std::find(sysCodes.begin(), sysCodes.end(), sig.code);
		if (prioritityIt == sysCodes.end())
		{
			tracepdeex(4, trace, "\n%s - Code type skipped", measDescription);
			continue;
		}
		
		tracepdeex(1, trace, "\n\n----------------------------------------------------");
		tracepdeex(1, trace, "\nProcessing %s: ", measDescription);
	
		
		string		sigName			= sig.code._to_string();
		SatNav&		satNav			= *obs.satNav_ptr;
		SatStat&	satStat			= *obs.satStat_ptr;
		SigStat&	sigStat			= satStat.sigStatMap[sigName];
		SigStat&	preprocSigStat	= satStat.sigStatMap[ft2string(ft)];
		
// 		if	( obs.ephPosValid == false
// 			||obs.ephClkValid == false
// 			)
		{
// 			tracepdeex(2,trace, "\n%s excludeSvh", obs.Sat.id().c_str());
			
// 			obs.excludeSVH = true;
		}
		
		if (acsConfig.excludeSlip.LLI	&& preprocSigStat.slip.LLI)		{	tracepdeex(2, trace, " - LLI slip excluded");	continue;   }
		if (acsConfig.excludeSlip.GF	&& preprocSigStat.slip.GF)		{	tracepdeex(2, trace, " - GF slip excluded");	continue;   }
		if (acsConfig.excludeSlip.MW	&& preprocSigStat.slip.MW)		{	tracepdeex(2, trace, " - MW slip excluded");	continue;   }
		if (acsConfig.excludeSlip.SCDIA	&& preprocSigStat.slip.SCDIA)	{	tracepdeex(2, trace, " - SCDIA slip excluded");	continue;   }
		
		if (obs.exclude)
		{
			if (obs.excludeBadSPP)		{	tracepdeex(5, std::cout, "\n%s excludeBadSPP",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeConfig)		{	tracepdeex(5, std::cout, "\n%s excludeConfig",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeEclipse)		{	tracepdeex(5, std::cout, "\n%s excludeEclipse",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeElevation)	{	tracepdeex(5, std::cout, "\n%s excludeElevation",	obs.Sat.id().c_str());	continue;	}
			if (obs.excludeOutlier)		{	tracepdeex(5, std::cout, "\n%s excludeOutlier",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeSlip)		{	tracepdeex(5, std::cout, "\n%s excludeSlip",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeSystem)		{	tracepdeex(5, std::cout, "\n%s excludeSystem",		obs.Sat.id().c_str());	continue;	}
			if (obs.excludeSVH)			{	tracepdeex(5, std::cout, "\n%s excludeSVH",			obs.Sat.id().c_str());	continue;	}
			if (obs.excludeTrop)		{	tracepdeex(5, std::cout, "\n%s excludeTrop",		obs.Sat.id().c_str());	continue;	}
		}
		
		auto& satOpts = acsConfig.getSatOpts(obs.Sat,	{sigName});
		auto& recOpts = acsConfig.getRecOpts(rec.id,	{obs.Sat.sys._to_string(), sigName});
		
		if (satOpts.exclude)
		{
			tracepdeex(2,trace, "\n%s excludeSatOpts",			obs.Sat.id().c_str());
			continue;
		}
		
		if (recOpts.exclude)
		{
			tracepdeex(2,trace,"\n%s excludeRecOpts",			obs.Sat.id().c_str());
			continue;
		}
		
		auto		Sat			= obs.Sat;
		auto		sys			= Sat.sys;
		auto		sysSat		= SatSys(sys);
		double		lambda		= obs.satNav_ptr->lamMap[ft];
		auto		code		= sig.code;
		

		double observed = 0;
		if		(measType == CODE)		observed = sig.P;
		else if	(measType == PHAS)		observed = sig.L * lambda;
		
		if (observed == 0)
		{
			tracepdeex(2,trace,"\n No %s observable for %s %s %s", (measType==CODE)?"CODE":"PHASE", obs.Sat.id().c_str(), rec.id.c_str(), sig.code._to_string());
			continue;
		}
		
		KFMeasEntry measEntry(&kfState);
		
		measEntry.metaDataMap["obs_ptr"]	= &obs;
		
		if (measType == PHAS)
		{
			measEntry.metaDataMap["phaseRejectCount"] = &sigStat.phaseRejectCount;
			measEntry.metaDataMap["phaseOutageCount"] = &sigStat.phaseOutageCount;
			tracepdeex(2,trace,"\n PPP Phase counts: %s %s %s %d %d", rec.id.c_str(), obs.Sat.id().c_str(), sig.code._to_string(), sigStat.phaseOutageCount, sigStat.phaseRejectCount);
		}
		
		measEntry.obsKey.Sat	= obs.Sat;
		measEntry.obsKey.str	= rec.id;
		measEntry.obsKey.num	= sig.code;
		if		(measType == CODE)		{	measEntry.obsKey.type = KF::CODE_MEAS;	measEntry.obsKey.comment = "P-" + (string)sig.code._to_string();	}
		else if	(measType == PHAS)		{	measEntry.obsKey.type = KF::PHAS_MEAS;	measEntry.obsKey.comment = "L-" + (string)sig.code._to_string();	}
		
		//Start with the observed measurement and its noise
		
		measEntry.componentList.push_back({E_Component::OBSERVED, -observed, "- Phi"});
		{
			KFKey obsKey;
			obsKey.str		= rec.id;
			obsKey.Sat		= obs.Sat;
			obsKey.type		= measEntry.obsKey.type;
			obsKey.num		= sig.code;
			
			if		(measType == CODE)	{	measEntry.addNoiseEntry(obsKey, 1, sig.codeVar);	}
			else if	(measType == PHAS)	{	measEntry.addNoiseEntry(obsKey, 1, sig.phasVar);	}
		}
		
		//Calculate the basic range
		
		VectorEcef rRec			= rec.aprioriPos;
	
		{
			for (int i = 0; i < 3; i++)
			{
				InitialState init = initialStateFromConfig(recOpts.pos, i);
				
				if (init.estimate == false)
				{
					continue;
				}
				
				KFKey kfKey;
				kfKey.type		= KF::REC_POS;
				kfKey.str		= rec.id;
				kfKey.num		= i;
				kfKey.rec_ptr	= &rec;
				kfKey.comment	= init.comment;
				
				kfState.getKFValue(kfKey, rRec[i]);
				
				init.x = rRec[i];
				
				measEntry.addDsgnEntry(kfKey, -satStat.e[i], init);
				
				InitialState rateInit = initialStateFromConfig(recOpts.pos_rate, i);
				if (rateInit.estimate)
				{
					KFKey rateKey;
					rateKey.type	= KF::REC_POS_RATE;
					rateKey.str		= rec.id;
					rateKey.num		= i;
					rateKey.comment	= rateInit.comment;
					
					kfState.setKFTransRate(kfKey, rateKey,	1,	rateInit);
				}
			}
			
			if (initialStateFromConfig(recOpts.orbit).estimate)
			{
				bool found = false;
				VectorEci rRecInertial = frameSwapper(rRec);
				
				for (int i = 0; i < 3; i++)
				{
					InitialState posInit = initialStateFromConfig(recOpts.orbit,i);
					InitialState velInit = initialStateFromConfig(recOpts.orbit,i + 3);
					
					KFKey posKey;
					posKey.type		= KF::ORBIT;
					posKey.num		= i;
					posKey.comment	= posInit.comment;
					
					if (recOpts.sat_id.empty())	posKey.str		= rec.id;
					else						posKey.Sat		= SatSys(recOpts.sat_id.c_str());
					
					KFKey velKey	= posKey;
					velKey.num		= i + 3;
					velKey.comment	= velInit.comment;
					
					
					found |= kfState.getKFValue(posKey, rRec[i]);
					
					if (posInit.x == 0)			posInit.x = rRecInertial[i];
					
					VectorEci eSatInertial	= frameSwapper(satStat.e);
					
					measEntry.addDsgnEntry	(posKey,			-eSatInertial[i],				posInit);
					
					kfState.setKFTransRate	(posKey, velKey,	1,					velInit,	posInit);	//todo aaron will create noise elements that kill something else
				}
				
				if (found)
				{
					rRec = frameSwapper(rRecInertial);
				}
			}
		}
		
		auto& pos = rec.pos;
	
		pos = ecef2pos(rRec);
		
		VectorEcef& rSat = obs.rSat;
		
		if (initialStateFromConfig(satOpts.orbit).estimate)
		{			
			bool found = false;
			for (int i = 0; i < 3; i++)
			{
				InitialState posInit = initialStateFromConfig(satOpts.orbit,i);
				InitialState velInit = initialStateFromConfig(satOpts.orbit,i + 3);
				
				KFKey posKey;
				posKey.type		= KF::ORBIT;
				posKey.Sat		= obs.Sat;
				posKey.num		= i;
				posKey.comment	= posInit.comment;
				
				KFKey velKey	= posKey;
				velKey.num		= i + 3;
				velKey.comment	= velInit.comment;
				
				if (obs.rSatEci0.isZero())
				{
					//dont obliterate obs.rSat below, we still need the old one for next signal/phase
					SatPos satPos0 = obs;
				
					//use this to avoid adding the dt component of position
					bool pass = satpos(trace, time, time, satPos0, acsConfig.model.sat_pos.ephemeris_sources, E_OffsetType::COM, nav, &kfState);
					
					obs.rSatEci0 = frameSwapper(satPos0.rSat, &satPos0.satVel, &obs.vSatEci0);
				}
				
				if (posInit.x == 0)			posInit.x = obs.rSatEci0[i];
				if (velInit.x == 0)			velInit.x = obs.vSatEci0[i];
				
				VectorEci eSatInertial	= frameSwapper(satStat.e);
				
				measEntry.addDsgnEntry	(posKey,			+eSatInertial[i],				posInit);
				
				kfState.setKFTransRate	(posKey, velKey,	1,					velInit,	posInit);	//todo aaron will create noise elements that kill something else
			}
		}
	
		tracepdeex(3, trace, "\n%s rSatEcef   : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.rSat		.x(),	obs.rSat		.y(),	obs.rSat		.z());
		tracepdeex(3, trace, "\n%s vSatEcef   : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.satVel		.x(),	obs.satVel		.y(),	obs.satVel		.z());
		tracepdeex(4, trace, "\n%s rSatEciDt  : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.rSatEciDt	.x(),	obs.rSatEciDt	.y(),	obs.rSatEciDt	.z());
		tracepdeex(4, trace, "\n%s rSatEci0   : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.rSatEci0	.x(),	obs.rSatEci0	.y(),	obs.rSatEci0	.z());
		
		//Range
		
		
		double rRecSat = (rSat - rRec).norm();
		
// 		VectorEci rRecEci	= frameSwapper(rRec);
// 		obs.rSatEciDt		= frameSwapper(rSat, obs.time - obs.tof - obs.dtSat[0]);
// 		
// 		rRecSat = (obs.rSatEciDt - rRecEci).norm();
		
		if (acsConfig.model.range)
		{
			measEntry.componentList.push_back({E_Component::RANGE, rRecSat, "+ rho"});
		}
		
		if (acsConfig.output_residual_chain)
		{
			tracepdeex(0, trace, "\n----------------------------------------------------");
			tracepdeex(0, trace, "\nMeasurement for %s %s\n", ((string) measEntry.obsKey).c_str(), sig.code._to_string());
		}
		
		
		
		//Add modelled adjustments and estimated parameter
		if (1)
		{
			if (acsConfig.model.rec_clock.enable)			{	Instrument inst("pppRecClocks");		pppRecClocks		(COMMON_PPP_ARGS);	}
			if (acsConfig.model.sat_clock.enable)			{	Instrument inst("pppSatClocks");		pppSatClocks		(COMMON_PPP_ARGS);	}
			if (acsConfig.model.rec_ant_delta)				{	Instrument inst("pppRecAntDelta");		pppRecAntDelta		(COMMON_PPP_ARGS);	}
	// 		if (acsConfig.model.sat_ant_delta)				{	Instrument inst("pppSatAntDelta");		pppSatAntDelta		(COMMON_PPP_ARGS);	}
			if (acsConfig.model.rec_pco)					{	Instrument inst("pppRecPCO");			pppRecPCO			(COMMON_PPP_ARGS);	}
			if (acsConfig.model.sat_pco)					{	Instrument inst("pppSatPCO");			pppSatPCO			(COMMON_PPP_ARGS);	}
			if (acsConfig.model.rec_pcv)					{	Instrument inst("pppRecPCV");			pppRecPCV			(COMMON_PPP_ARGS);	}
			if (acsConfig.model.sat_pcv)					{	Instrument inst("pppSatPCV");			pppSatPCV			(COMMON_PPP_ARGS);	}
			if (acsConfig.model.tides.enable)				{	Instrument inst("pppTides");			pppTides			(COMMON_PPP_ARGS);	}
			if (acsConfig.model.relativity)					{	Instrument inst("pppRelativity");		pppRelativity		(COMMON_PPP_ARGS);	}
			if (acsConfig.model.relativity2)				{	Instrument inst("pppRelativity2");		pppRelativity2		(COMMON_PPP_ARGS);	}
			if (acsConfig.model.sagnac)						{	Instrument inst("pppSagnac");			pppSagnac			(COMMON_PPP_ARGS);	}
			if (acsConfig.model.ionospheric_component)		{	Instrument inst("pppIonStec");			pppIonStec			(COMMON_PPP_ARGS);	}
			if (acsConfig.model.ionospheric_component2)		{	Instrument inst("pppIonStec2");			pppIonStec2			(COMMON_PPP_ARGS);  }
			if (acsConfig.model.ionospheric_component3)		{	Instrument inst("pppIonStec3");			pppIonStec3			(COMMON_PPP_ARGS);  }
			if (acsConfig.model.ionospheric_model)			{	Instrument inst("pppIonModel");			pppIonModel			(COMMON_PPP_ARGS);	}
			if (acsConfig.model.trop.enable)				{	Instrument inst("pppTrop");				pppTrop				(COMMON_PPP_ARGS);	}
// 			if (acsConfig.model.orbits)						{	Instrument inst("pppOrbits");			pppOrbits			(COMMON_PPP_ARGS);	}
			if (acsConfig.model.eop)						{	Instrument inst("pppEopAdjustment");	pppEopAdjustment	(COMMON_PPP_ARGS);  }
		}
		
		if (measType == PHAS)
		{
			if (acsConfig.model.phase_windup)				{	Instrument inst("pppPhaseWindup");		pppPhaseWindup		(COMMON_PPP_ARGS);	}
			if (acsConfig.model.integer_ambiguity)			{	Instrument inst("pppIntegerAmbiguity");	pppIntegerAmbiguity	(COMMON_PPP_ARGS);	}
			if (acsConfig.model.rec_phase_bias)				{	Instrument inst("pppRecPhasBias");		pppRecPhasBias		(COMMON_PPP_ARGS);	}
			if (acsConfig.model.sat_phase_bias)				{	Instrument inst("pppSatPhasBias");		pppSatPhasBias		(COMMON_PPP_ARGS);	}
			if (acsConfig.model.heading)					{	Instrument inst("pppHeading");			pppHeading			(COMMON_PPP_ARGS);  }
		}
		
		if (measType == CODE)
		{
			if (acsConfig.model.rec_code_bias)				{	Instrument inst("pppRecCodeBias");		pppRecCodeBias		(COMMON_PPP_ARGS);	}
			if (acsConfig.model.sat_code_bias)				{	Instrument inst("pppSatCodeBias");		pppSatCodeBias		(COMMON_PPP_ARGS);	}
		}
		
		addKFSatEMPStates(satOpts.emp_dyb_0,	kfState,	KF::EMP_DYB_0,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_1c,	kfState,	KF::EMP_DYB_1C,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_1s,	kfState,	KF::EMP_DYB_1S,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_2c,	kfState,	KF::EMP_DYB_2C,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_2s,	kfState,	KF::EMP_DYB_2S,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_3c,	kfState,	KF::EMP_DYB_3C,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_3s,	kfState,	KF::EMP_DYB_3S,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_4c,	kfState,	KF::EMP_DYB_4C,	obs.Sat);
		addKFSatEMPStates(satOpts.emp_dyb_4s,	kfState,	KF::EMP_DYB_4S,	obs.Sat);
		
		addKFSatEMPStates(satOpts.srp_dyb_0,	kfState,	KF::SRP_DYB_0,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_1c,	kfState,	KF::SRP_DYB_1C,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_1s,	kfState,	KF::SRP_DYB_1S,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_2c,	kfState,	KF::SRP_DYB_2C,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_2s,	kfState,	KF::SRP_DYB_2S,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_3c,	kfState,	KF::SRP_DYB_3C,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_3s,	kfState,	KF::SRP_DYB_3S,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_4c,	kfState,	KF::SRP_DYB_4C,	obs.Sat);
		addKFSatEMPStates(satOpts.srp_dyb_4s,	kfState,	KF::SRP_DYB_4S,	obs.Sat);
	
		//Calculate residuals and form up the measurement
		
		measEntry.componentList.push_back({E_Component::NET_RESIDUAL, 0, ""});
		double residual = 0;
		for (auto& [component, componentVal, eq] : measEntry.componentList)
		{
			residual -= componentVal;
			
			if (acsConfig.output_residual_chain)
			{
				tracepdeex(0, trace, "\n");
				tracepdeex(4, trace, "%s",		time.to_string());
				tracepdeex(3, trace, "%30s",	((string)measEntry.obsKey).c_str());
				tracepdeex(0, trace, " %-20s %+14.4f -> %13.4f", component._to_string(), -componentVal, residual);
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

