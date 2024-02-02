
// #pragma GCC optimize ("O0")

/** References
* 1. M.Fritsche, R.Dietrich, C.Knöfel, A.Rülke, S.Vey, M.Rothacher & P.Steigenberger, Impact of higher‐order ionospheric terms on GPS estimates. Geophysical research letters, 2005.
* 2. GAMIT 10.71
* 3. U.Hugentobler, S.Schaer, G.Beutler, H.Bock, R.Dach, A.Jäggi, M.Meindl, C.Urschl, L.Mervart, M.Rothacher & U.Wild, CODE IGS analysis center technical report 2002, 2002.
*/


#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "geomagField.hpp"
#include "instrument.hpp"
#include "tropModels.hpp"
#include "acsConfig.hpp"
#include "ionoModel.hpp"
#include "orbitProp.hpp"
#include "ionModels.hpp"
#include "receiver.hpp"
#include "posProp.hpp"
#include "antenna.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "biases.hpp"
#include "cache.hpp"
#include "gTime.hpp"
#include "tides.hpp"
#include "trace.hpp"



struct AutoSender
{
	int		level = 0;
	Trace&	trace;
	GTime	time;

	vector<ArbitraryKVP> baseKVPs;
	vector<ArbitraryKVP> valueKVPs;

	AutoSender(
		int		level,
		Trace&	trace,
		GTime	time)
	:	level	{level},
		trace	{trace},
		time	{time}
	{

	}

	~AutoSender()
	{
		if	( baseKVPs	.empty()
			||valueKVPs	.empty())
		{
			return;
		}

		traceJson(level, trace, time, baseKVPs, valueKVPs);
	}
};


//this hideousness is a horrible hack to make this code compile on macs.
//blame apple for this. look what they've made me do...

//this will ust copy and paste the types and variable names for now, this is undefined later to remove the type names to pass in the parameters in the same order
#define COMMON_ARG(type)    type

#define COMMON_PPP_ARGS												\
	COMMON_ARG(			Trace&				)	trace,				\
	COMMON_ARG(			Trace&				)	jsonTrace,			\
	COMMON_ARG(			GObs&				)	obs,				\
	COMMON_ARG(			Receiver&			)	rec,				\
	COMMON_ARG(			SatSys&				)	Sat,				\
	COMMON_ARG(			Sig&				)	sig,				\
	COMMON_ARG(			string&				)	sigName,			\
	COMMON_ARG(const	E_FType&			)	ft,					\
	COMMON_ARG(			E_MeasType&			)	measType,			\
	COMMON_ARG(			GTime&				)	time,				\
	COMMON_ARG(			SatStat&			)	satStat,			\
	COMMON_ARG(			SatNav&				)	satNav,				\
	COMMON_ARG(			ReceiverOptions&	)	recOpts,			\
	COMMON_ARG(			SatelliteOptions&	)	satOpts,			\
	COMMON_ARG(const	KFState&			)	kfState,			\
	COMMON_ARG(const	KFState&			)	remoteKF,			\
	COMMON_ARG(			KFMeasEntry&		)	measEntry,			\
	COMMON_ARG(			VectorEcef&			)	rRec,				\
	COMMON_ARG(			VectorEcef&			)	rSat,				\
	COMMON_ARG(			double&				)	rRecSat,			\
	COMMON_ARG(			double&				)	lambda,				\
	COMMON_ARG(			SatSys&				)	sysSat,				\
	COMMON_ARG(			AutoSender&			)	autoSenderTemplate,	\
	COMMON_ARG(			VectorPos&			)	pos


inline void pppRecClocks(COMMON_PPP_ARGS)
{
	double	recClk_m	= rec.aprioriClk;
	double	dtRecVar	= rec.aprioriClkVar;

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

		kfState.getKFValue(kfKey, init.x);

		recClk_m += init.x;

		measEntry.addDsgnEntry(kfKey, 1, init);

		InitialState rateInit = initialStateFromConfig(recOpts.clk_rate, i);

		if (rateInit.estimate)
		{
			KFKey rateKey	= kfKey;
			rateKey.type	= KF::REC_CLOCK_RATE;

			kfState.setKFTransRate(kfKey, rateKey, 1, rateInit);
		}

		dtRecVar = -1;
	}

	measEntry.componentsMap[E_Component::REC_CLOCK] = {recClk_m, "+ Cdt_r", dtRecVar};
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

		kfState.getKFValue(kfKey, init.x);

		satClk_m += init.x;

		measEntry.addDsgnEntry(kfKey, -1,									init);
		measEntry.addDsgnEntry(kfKey, -obs.satVel.dot(satStat.e) / CLIGHT,	init);

		InitialState rateInit = initialStateFromConfig(satOpts.clk_rate, i);

		if (rateInit.estimate)
		{
			KFKey rateKey	= kfKey;
			rateKey.type	= KF::SAT_CLOCK_RATE;

			kfState.setKFTransRate(kfKey, rateKey, 1, rateInit);
		}

		satClkVar = -1;
	}

	measEntry.componentsMap[E_Component::SAT_CLOCK] = {-satClk_m, "- Cdt_s", satClkVar};
};

inline void pppRecAntDelta(COMMON_PPP_ARGS)
{
	VectorEcef recAntVector = body2ecef(rec.attStatus, rec.antDelta);

	double recAntDelta = -recAntVector.dot(satStat.e);

	measEntry.componentsMap[E_Component::REC_ANTENNA_DELTA] = {recAntDelta, "- E.dR_r", 0};
};


inline void pppRecPCO(COMMON_PPP_ARGS)
{
	E_FType recAtxFt = ft;
	if (acsConfig.common_rec_pco)
	{
		if		(Sat.sys == +E_Sys::GLO)	recAtxFt = G1;
		else if	(Sat.sys == +E_Sys::BDS)	recAtxFt = B1;
		else								recAtxFt = F1;
	}

	auto& attStatus = rec.attStatus;

	double variance = 0;
	MatrixXd dEdQ;
	if (initialStateFromConfig(recOpts.orientation).estimate)
		dEdQ = MatrixXd::Zero(3,4);

	Vector3d bodyPCO	= antPco(rec.antennaId, Sat.sys, recAtxFt, time, variance, E_Radio::RECEIVER, acsConfig.interpolate_rec_pco);
	Vector3d bodyLook	= ecef2body(attStatus, satStat.e, &dEdQ);	//todo aaron, move this to antDelta instead

	for (int i = 0; i < 3; i++)
	{
		int enumNum = KF::REC_PCO_X + i;

		InitialState init = initialStateFromConfig(recOpts.pco, i);

		if (init.estimate)
		{
			if (init.Q < 0)
			{
	// 			init.P = variance;	//bad things happen (no iflc) if this is zero, as is often the case for varIono
			}

			KFKey kfKey;
			kfKey.type		= KF::_from_integral(enumNum);
			kfKey.str		= rec.id;
			kfKey.num		= recAtxFt;

			init.x = bodyPCO(i);

			kfState.getKFValue(kfKey, init.x);

			bodyPCO(i) = init.x;

			measEntry.addDsgnEntry(kfKey, -bodyLook(i), init);

			variance = -1;
		}
	}

	if (initialStateFromConfig(recOpts.orientation).estimate)
	for (int i = 0; i < 4; i++)
	{
		InitialState init = initialStateFromConfig(recOpts.orientation, i);

		KFKey kfKey;
		kfKey.type	= KF::ORIENTATION;
		kfKey.str	= rec.id;
		kfKey.num	= i;

		measEntry.addDsgnEntry(kfKey, -bodyPCO.dot(dEdQ.col(i)));
	}


	double recPCODelta = -bodyPCO.dot(bodyLook);

	measEntry.componentsMap[E_Component::REC_PCO] = {recPCODelta, "- E.PCO_r", variance};
};

inline void pppSatPCO(COMMON_PPP_ARGS)
{
	E_FType satAtxFt = ft;

	if (acsConfig.common_sat_pco)
	{
		if		(Sat.sys == +E_Sys::GLO)	satAtxFt = G1;
		else if	(Sat.sys == +E_Sys::BDS)	satAtxFt = B1;
		else								satAtxFt = F1;
	}

	auto& attStatus = satNav.attStatus;

	double variance = 0;

	Vector3d bodyPCO	= antPco(Sat.id(), Sat.sys, satAtxFt, time, variance, E_Radio::TRANSMITTER);
	Vector3d bodyLook	= ecef2body(attStatus, satStat.e);

	for (int i = 0; i < 3; i++)
	{
		int enumNum = KF::SAT_PCO_X + i;

		InitialState init = initialStateFromConfig(satOpts.pco, i);

		if (init.estimate)
		{
			if (init.Q < 0)
			{
	// 			init.P = variance;	//bad things happen (no iflc) if this is zero, as is often the case for varIono
			}

			KFKey kfKey;
			kfKey.type		= KF::_from_integral(enumNum);
			kfKey.Sat		= obs.Sat;
			kfKey.num		= satAtxFt;

			init.x = bodyPCO(i);

			kfState.getKFValue(kfKey, init.x);

			bodyPCO(i) = init.x;

			measEntry.addDsgnEntry(kfKey, bodyLook(i), init);

			variance = -1;
		}
	}

	AutoSender autoSender = autoSenderTemplate;

	autoSender.baseKVPs.push_back({"data", __FUNCTION__});
	autoSender.valueKVPs =
	{
		{"bodyLook[0]",	bodyLook[0]},
		{"bodyLook[1]",	bodyLook[1]},
		{"bodyLook[2]",	bodyLook[2]},
		{"bodyPCO[0]",	bodyPCO[0]},
		{"bodyPCO[1]",	bodyPCO[1]},
		{"bodyPCO[2]",	bodyPCO[2]},
		{"nominalYaw",	attStatus.nominalYaw},
		{"modelYaw",	attStatus.modelYaw}
	};

	double satPCODelta = bodyPCO.dot(bodyLook);

	measEntry.componentsMap[E_Component::SAT_PCO] = {satPCODelta, "+ E.PCO_s", variance};

};

inline void pppRecPCV(COMMON_PPP_ARGS)
{
	E_FType recAtxFt = ft;

	if (acsConfig.common_rec_pco)
	{
		if		(Sat.sys == +E_Sys::GLO)	recAtxFt = G1;
		else if	(Sat.sys == +E_Sys::BDS)	recAtxFt = B1;
		else								recAtxFt = F1;
	}

	double az	= 0;
	double zen	= 0;
	double recPCVDelta = antPcv(rec.antennaId,	Sat.sys, recAtxFt, time, rec.attStatus,		satStat.e * +1, &az, &zen);

	InitialState init = initialStateFromConfig(recOpts.pcv);

	if (init.estimate)
	{
		init.x		= recPCVDelta;
		recPCVDelta	= 0;

		double az_delta = 5;
		double el_delta = 5;

		double azFrac = az	/ az_delta;
		double elFrac = zen	/ el_delta;

		KFKey kfKey;
		kfKey.type	= KF::REC_PCV;
		kfKey.str	= rec.id;

		for (auto iAz : {0, 1})
		for (auto iEl : {0, 1})
		{
			int indexAz = ((int) (azFrac + iAz)) * az_delta;
			int indexEl = ((int) (elFrac + iEl)) * el_delta;

			if (indexAz > 360)		indexAz -= 360;
			if (indexEl > 90)		indexEl = 90;

			kfKey.num	= 10000	* indexAz
						+ 100	* indexEl
						+ 1		* recAtxFt;

			double scalar = 1;
			if (iAz)		scalar *= azFrac - (int) azFrac;		else	scalar *= ((int) (azFrac + 1)) - azFrac;
			if (iEl)		scalar *= elFrac - (int) elFrac;		else	scalar *= ((int) (elFrac + 1)) - elFrac;

			kfState.getKFValue(kfKey, init.x);

			recPCVDelta += scalar * init.x;

			measEntry.addDsgnEntry(kfKey, scalar, init);
		}
	}

	measEntry.componentsMap[E_Component::REC_PCV] = {recPCVDelta, "+ PCV_r", 0};
};

inline void pppSatPCV(COMMON_PPP_ARGS)
{
	E_FType satAtxFt = ft;

	if (acsConfig.common_sat_pco)
	{
		if		(Sat.sys == +E_Sys::GLO)	satAtxFt = G1;
		else if	(Sat.sys == +E_Sys::BDS)	satAtxFt = B1;
		else								satAtxFt = F1;
	}

	double satPCVDelta = antPcv(Sat.id(),		Sat.sys, satAtxFt, time, satNav.attStatus,	satStat.e * -1);

	measEntry.componentsMap[E_Component::SAT_PCV] = {satPCVDelta, "+ PCV_s", 0};
};

inline void pppTides(COMMON_PPP_ARGS)
{
	auto& [solid, otl, atl, spole, opole] = rec.pppTideCache.useCache([&]() -> tuple<Vector3d, Vector3d, Vector3d, Vector3d, Vector3d>
	{
		Vector3d tideVectorSum		= Vector3d::Zero();
		Vector3d tideVectorSolid	= Vector3d::Zero();
		Vector3d tideVectorOTL		= Vector3d::Zero();
		Vector3d tideVectorATL		= Vector3d::Zero();
		Vector3d tideVectorSPole	= Vector3d::Zero();
		Vector3d tideVectorOPole	= Vector3d::Zero();

		if	( recOpts.tideModels.solid
			||recOpts.tideModels.otl
			||recOpts.tideModels.atl
			||recOpts.tideModels.spole
			||recOpts.tideModels.opole)
		{
			tideDisp(trace, time, rec, rRec, tideVectorSum, &tideVectorSolid, &tideVectorOTL, &tideVectorATL, &tideVectorSPole, &tideVectorOPole);
		}

		return {tideVectorSolid, tideVectorOTL, tideVectorATL, tideVectorSPole, tideVectorOPole};
	});

	measEntry.componentsMap[E_Component::TIDES_SOLID	] = {-solid	.dot(satStat.e), "- E.dT1", 0};
	measEntry.componentsMap[E_Component::TIDES_OTL		] = {-otl	.dot(satStat.e), "- E.dT2", 0};
	measEntry.componentsMap[E_Component::TIDES_ATL		] = {-atl	.dot(satStat.e), "- E.dT3", 0};
	measEntry.componentsMap[E_Component::TIDES_SPOLE	] = {-spole	.dot(satStat.e), "- E.dT4", 0};
	measEntry.componentsMap[E_Component::TIDES_OPOLE	] = {-opole	.dot(satStat.e), "- E.dT5", 0};
};

inline void pppRelativity(COMMON_PPP_ARGS)
{
	/* note that relativity effect to estimate sat clock */
	double dtRel1	= relativity1(rSat, obs.satVel);

	measEntry.componentsMap[E_Component::RELATIVITY1] = {dtRel1	* CLIGHT, "+ rel1", 0};
};

inline void pppRelativity2(COMMON_PPP_ARGS)
{
	/* secondary relativity effect (Shapiro effect) */
	double ln		= log(	(rSat.norm() + rRec.norm() + rRecSat)
						/	(rSat.norm() + rRec.norm() - rRecSat));
	double dtRel2 	= 2 * MU * ln / CLIGHT / CLIGHT / CLIGHT;

	measEntry.componentsMap[E_Component::RELATIVITY2] = {dtRel2	* CLIGHT, "+ rel2", 0};
};

inline void pppSagnac(COMMON_PPP_ARGS)
{
	double dSagnac = sagnac(rSat, rRec);

	measEntry.componentsMap[E_Component::SAGNAC] = {dSagnac, "+ sag", 0};
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
		bool pass = ionoModel(time, pos, satStat, recOpts.mapping_function, acsConfig.pppOpts.ionoOpts.corr_mode, recOpts.mapping_function_layer_height, dummy, diono, varIono);
		if (pass)
		{
			double ionC = SQR(lambda / obs.satNav_ptr->lamMap[F1]);

			ionosphere_m = factor * ionC * diono;
		}
	}

	double ionosphereStec = ionosphere_m / (factor * alpha);

	if (init.estimate)
	{
		if (init.Q < 0)
		{
// 			init.P = varIono;	//bad things happen (no iflc) if this is zero, as is often the case for varIono
		}

		string recStr;
		if (acsConfig.pppOpts.common_atmosphere == false)
			recStr	= rec.id;

		KFKey kfKey;
		kfKey.type		= KF::IONO_STEC;
		kfKey.str		= recStr;
		kfKey.Sat		= obs.Sat;
		kfKey.rec_ptr	= &rec;
		kfKey.comment	= init.comment;

		if (acsConfig.pppOpts.ionoOpts.common_ionosphere == false)
			kfKey.num = measType;

		init.x = ionosphereStec;

		kfState.getKFValue(kfKey, init.x);

		ionosphereStec  = init.x;

		ionosphere_m = factor * alpha * ionosphereStec;

		measEntry.addDsgnEntry(kfKey, factor * alpha, init);

		varIono = -1;
	}

	measEntry.componentsMap[E_Component::IONOSPHERIC_COMPONENT] = {ionosphere_m, "+ " + std::to_string(factor * alpha) + ".I", varIono};
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
	ionppp(pos, satStat, RE_MEAN / 1000, recOpts.geomagnetic_field_height, posp);

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

		string recStr;
		if (acsConfig.pppOpts.common_atmosphere == false)
			recStr	= rec.id;

		KFKey kfKey;
		kfKey.type		= KF::IONO_STEC;
		kfKey.str		= recStr;
		kfKey.Sat		= obs.Sat;
		kfKey.rec_ptr	= &rec;
		kfKey.comment	= init.comment;

		if (acsConfig.pppOpts.ionoOpts.common_ionosphere == false)
			kfKey.num = measType;

		measEntry.addDsgnEntry(kfKey, factor * alpha, init);

		kfState.getKFValue(kfKey, init.x);

		double ionosphere_stec = init.x;

		ionosphere_m = factor * alpha * ionosphere_stec;

		measEntry.componentsMap[E_Component::IONOSPHERIC_COMPONENT1] = {ionosphere_m, "", 0};
		//component will be added in primary ionospheric model <- F this
	}
	else
	{
		double diono	= 0;
		double dummy	= 0;
		bool pass = ionoModel(time, pos, satStat, recOpts.mapping_function, acsConfig.pppOpts.ionoOpts.corr_mode, recOpts.mapping_function_layer_height, dummy, diono, varIono);
		if (pass)
		{
			double stec = diono * SQR(FREQ1) / TEC_CONSTANT;	// restore STEC

			ionosphere_m = factor * alpha * stec;
		}

		measEntry.componentsMap[E_Component::IONOSPHERIC_COMPONENT1] = {ionosphere_m, "", 0};
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
	if (acsConfig.pppOpts.ionoOpts.corr_mode == +E_IonoMode::BROADCAST)		mapFn = E_IonoMapFn::KLOBUCHAR;
	else																	mapFn = recOpts.mapping_function;

	double fs = ionmapf(pos, satStat, mapFn, recOpts.mapping_function_layer_height);

	//calculate the ionosphere values, variances, and gradients at the operating points

	InitialState init = initialStateFromConfig(recOpts.ion_stec, 2);

	if (init.estimate)
	{
		if (initialStateFromConfig(recOpts.ion_stec).estimate == false)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: Higher order ionosphere estimation requires lower order ionosphere estimation.";
		}

		string recStr;
		if (acsConfig.pppOpts.common_atmosphere == false)
			recStr	= rec.id;

		KFKey kfKey;
		kfKey.type		= KF::IONO_STEC;
		kfKey.str		= recStr;
		kfKey.Sat		= obs.Sat;
		kfKey.rec_ptr	= &rec;
		kfKey.comment	= init.comment;

		if (acsConfig.pppOpts.ionoOpts.common_ionosphere == false)
			kfKey.num = measType;

		kfState.getKFValue(kfKey, init.x);

		double ionosphere_stec = init.x;

		double vtec = ionosphere_stec / fs;	// restore VTEC for nMax calculation

		double nMax = 20.0e12 + 14.0e12 / 3.17e18 * (vtec * 1e16 - 4.55e18);	// calculate nMax with linear interpolation, ref: GAMIT code

		if (nMax < 0)
			nMax = 0;	// avoid being negative

		measEntry.addDsgnEntry(kfKey, factor * alpha * (2 * nMax + 0.3e12 / 3.17) * 0.66, init);

		ionosphere_m = factor * alpha * nMax * 0.66 * ionosphere_stec;
		measEntry.componentsMap[E_Component::IONOSPHERIC_COMPONENT2] = {ionosphere_m, "", 0};
		//component will be added in primary ionospheric model  <- F this
	}
	else
	{
		double diono	= 0;
		double dummy	= 0;
		bool pass = ionoModel(time, pos, satStat, recOpts.mapping_function, acsConfig.pppOpts.ionoOpts.corr_mode, recOpts.mapping_function_layer_height, dummy, diono, varIono);
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

		measEntry.componentsMap[E_Component::IONOSPHERIC_COMPONENT2] = {ionosphere_m, "", 0};
	}
};

inline void pppIonModel(COMMON_PPP_ARGS)
{
	if (acsConfig.use_for_iono_model[Sat.sys] == false)
	{
		return;
	}

	double ionosphere_m	= 0;
	double varIono		= 0;
	double freq			= CLIGHT / lambda;
	double alpha		= TEC_CONSTANT / SQR(freq);
	double sign			= 1;

	if (measType == CODE)		sign = +1;
	else						sign = -1;

	InitialState init = initialStateFromConfig(recOpts.ion_model);

	if (init.estimate)
	{
		for (int i = 0; i < acsConfig.ionModelOpts.layer_heights.size(); i++)
		{
			VectorPos posp;
			obs.ippMap[i].slantFactor	= ionppp(pos, satStat, RE_WGS84 / 1000, acsConfig.ionModelOpts.layer_heights[i] / 1000, posp);
			obs.ippMap[i].latDeg		= posp.latDeg();
			obs.ippMap[i].lonDeg		= posp.lonDeg();
			obs.ionoSat					= Sat;
		}

		tracepdeex(0, trace, "\n Iono Model : %.4f, %.4f     %s", obs.ippMap[0].latDeg , obs.ippMap[0].lonDeg, Sat.id());

		for (int i = 0; i < acsConfig.ionModelOpts.numBasis; i++)
		{
			double coeff = ionModelCoef(trace, i, obs);

			if (coeff == 0)
				continue;


			KFKey kfKey;
			kfKey.type		= KF::IONOSPHERIC;
			kfKey.rec_ptr	= &rec;
			kfKey.num		= i;

			bool pass = kfState.getKFValue(kfKey, init.x);

			double value = init.x;

			ionosphere_m += sign * alpha * coeff * value;

			tracepdeex(0, trace, "\n     basis (%3d);  + %.4e x %.4e x %.4e = %.4e", i, alpha, coeff, value, ionosphere_m);

			measEntry.addDsgnEntry(kfKey, sign * alpha * coeff, init);
		}

		measEntry.componentsMap[E_Component::IONOSPHERIC_MODEL] = {ionosphere_m, "+ " + std::to_string(sign * alpha) + ".I", -1};
	}
};

inline void pppTropMap(COMMON_PPP_ARGS)
{
	double troposphere_m	= tropDryZTD(trace, recOpts.tropModel.models, kfState.time, pos);

	double		varTrop			= 0;
	TropStates	tropStates;					//todo aaron unused?
	TropMapping	dTropDx;

	double modelTroposphere_m	= tropModel(trace, recOpts.tropModel.models, time, pos, satStat, tropStates, dTropDx, varTrop);

	InitialState init = initialStateFromConfig(recOpts.trop_maps);

	if (init.estimate)
	{
		modelTroposphere_m = dTropDx.dryMap * tropDryZTD(trace, recOpts.tropModel.models, time, pos);

		for (int i = 0; i < acsConfig.ssrOpts.nbasis; i++)
		{
			double geoMap = tropModelCoef(i, pos);
			if (geoMap == 0)
				continue;

			KFKey kfKey;
			kfKey.type	= KF::TROP_MODEL;
			kfKey.num	= i;


			double value=0;
			bool pass = kfState.getKFValue(kfKey, value);

			modelTroposphere_m += geoMap * dTropDx.wetMap * value;

			tracepdeex(0, trace, "\n Trop Model (%3d);  + %.4e x %.4e = %.4e", i, geoMap * dTropDx.wetMap, value, modelTroposphere_m);

			measEntry.addDsgnEntry(kfKey, geoMap * dTropDx.wetMap, init);
		}
	}

	measEntry.componentsMap[E_Component::TROPOSPHERE_MODEL] = {modelTroposphere_m, "+ " + std::to_string(dTropDx.wetMap) + ".T", -1};
}


inline void pppTrop(COMMON_PPP_ARGS)
{
	TropStates	tropStates;
	TropMapping	dTropDx;
	double		varTrop			= 0;

	double troposphere_m	= 0;

	string recStr;
// 	if (acsConfig.pppOpts.common_atmosphere == false)
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

		tropStates.zenith += value;
	}

	for (short i = 0; i < 2; i++)
	{
		KFKey kfKey;
		kfKey.type	= KF::TROP_GRAD;
		kfKey.str	= recStr;
		kfKey.num	= i;

		kfState.getKFValue(kfKey, tropStates.grads[i]);
	}

	//calculate the trop values, variances, and gradients at the operating points
	troposphere_m		= tropModel(trace, recOpts.tropModel.models, time, pos, satStat, tropStates, dTropDx, varTrop);
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
			init.x = tropStates.zenith;
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

		measEntry.addDsgnEntry(kfKey, dTropDx.wetMap, init);

		varTrop = -1;
	}

	for (short i = 0; i < 2; i++)
	{
		InitialState init = initialStateFromConfig(recOpts.trop_grads, i);

		if (init.estimate == false)
		{
			continue;
		}

		init.x = tropStates.grads[i];

		KFKey kfKey;
		kfKey.type		= KF::TROP_GRAD;
		kfKey.str 		= recStr;
		kfKey.num		= i;
		kfKey.comment	= init.comment;

		if (i == 0)		measEntry.addDsgnEntry(kfKey, dTropDx.northMap,	init);
		else			measEntry.addDsgnEntry(kfKey, dTropDx.eastMap,	init);
	}

	measEntry.componentsMap[E_Component::TROPOSPHERE] = {troposphere_m, "+ " + std::to_string(dTropDx.wetMap) + ".T", varTrop};
};

inline void pppRecPhaseWindup(COMMON_PPP_ARGS)
{
	phaseWindup(obs, rec, satStat.phw);

	double phaseWindup_m = satStat.phw * lambda;

	AutoSender autoSender = autoSenderTemplate;
	autoSender.valueKVPs.push_back({"satStat-phw",	satStat.phw});

	measEntry.componentsMap[E_Component::PHASE_WIND_UP] = {phaseWindup_m, "+ phi", 0};
};

inline void pppSatPhaseWindup(COMMON_PPP_ARGS)
{
// 	phaseWindup(obs, rec, satStat.phw);
//
// 	double phaseWindup_m = satStat.phw * lambda;
//
// 	AutoSender autoSender = autoSenderTemplate;
// 	autoSender.valueKVPs.push_back({"satStat-phw",	satStat.phw});
//
// 	measEntry.componentsMap[E_Component::PHASE_WIND_UP] = {phaseWindup_m, "+ phi", 0};
};

inline void pppIntegerAmbiguity(COMMON_PPP_ARGS)
{
	InitialState init		= initialStateFromConfig(recOpts.ambiguity);

	if (init.estimate == false)
	{
		return;
	}

	double ambiguity_m = 0;

	{
		KFKey kfKey;
		kfKey.type		= KF::AMBIGUITY;
		kfKey.str		= rec.id;
		kfKey.Sat		= obs.Sat;
		kfKey.num		= sig.code;
		kfKey.rec_ptr	= &rec;
		kfKey.comment	= sigName;

		double ambiguity = 0;

		if (sig.P)
		{
			ambiguity = sig.L - sig.P / lambda;
		}

		init.x = ambiguity;

		kfState.getKFValue(kfKey, init.x);

		ambiguity = init.x;

		ambiguity_m = ambiguity * lambda;

		init.x	= ambiguity;
		init.P /= SQR(lambda);

		measEntry.addDsgnEntry(kfKey, lambda, init);
	}

	measEntry.componentsMap[E_Component::PHASE_AMBIGUITY] = {ambiguity_m, "+ " + std::to_string(lambda) + ".N", -1};
};

inline void pppRecPhasBias(COMMON_PPP_ARGS)
{
	double	recPhasBias		=  		recOpts.phaseBiasModel.default_bias;
	double	recPhasBiasVar	= SQR(	recOpts.phaseBiasModel.undefined_sigma);
	getBias(trace, time, rec.id, Sat, sig.code, PHAS, recPhasBias, recPhasBiasVar);

	KFKey kfKey;
	kfKey.type		= KF::PHASE_BIAS;
	kfKey.str		= rec.id;
	kfKey.Sat		= sysSat;
	kfKey.num		= sig.code;
	kfKey.rec_ptr	= &rec;
	kfKey.comment	= sigName;

	InitialState init		= initialStateFromConfig(recOpts.phase_bias);

	if (init.estimate)
	{
		init.x = recPhasBias;

		if (init.Q < 0)
		{
			init.P = recPhasBiasVar;
		}

		init.x = recPhasBias;

		kfState.getKFValue(kfKey, init.x);

		recPhasBias = init.x;

		measEntry.addDsgnEntry(kfKey, 1, init);

		recPhasBiasVar = -1;
	}

	measEntry.addNoiseEntry(kfKey, 1, recPhasBiasVar);

	measEntry.componentsMap[E_Component::REC_PHASE_BIAS] = {recPhasBias, "+ b_" + std::to_string(ft) + "r", recPhasBiasVar};
};

inline void pppSatPhasBias(COMMON_PPP_ARGS)
{
	double	satPhasBias		=		satOpts.phaseBiasModel.default_bias;
	double	satPhasBiasVar	= SQR(	satOpts.phaseBiasModel.undefined_sigma);
	getBias(trace, time, Sat, Sat, sig.code, PHAS, satPhasBias, satPhasBiasVar);

	KFKey kfKey;
	kfKey.type		= KF::PHASE_BIAS;
	kfKey.Sat		= Sat;
	kfKey.num		= sig.code;
	kfKey.comment	= sigName;

	InitialState init		= initialStateFromConfig(satOpts.phase_bias);

	if (init.estimate)
	{
		if (init.Q < 0)
		{
			init.P = satPhasBiasVar;
		}

		init.x = satPhasBias;

		kfState.getKFValue(kfKey, init.x);

		satPhasBias = init.x;

		measEntry.addDsgnEntry(kfKey, 1, init);

		satPhasBiasVar = -1;
	}

	measEntry.addNoiseEntry(kfKey, 1, satPhasBiasVar);

	measEntry.componentsMap[E_Component::SAT_PHASE_BIAS] = {satPhasBias, "+ b_" + std::to_string(ft) + "s", satPhasBiasVar};
};


inline void pppRecCodeBias(COMMON_PPP_ARGS)
{
	double	recCodeBias		= 		recOpts.codeBiasModel.default_bias;
	double	recCodeBiasVar	= SQR(	recOpts.codeBiasModel.undefined_sigma);
	getBias(trace, time, rec.id, Sat, sig.code, CODE, recCodeBias, recCodeBiasVar);

	KFKey kfKey;
	kfKey.type		= KF::CODE_BIAS;
	kfKey.str		= rec.id;
	kfKey.Sat		= sysSat;
	kfKey.num		= sig.code;
	kfKey.rec_ptr	= &rec;
	kfKey.comment	= sigName;

	InitialState init		= initialStateFromConfig(recOpts.code_bias);

	if (init.estimate)
	{
		init.x = recCodeBias;

		if (init.Q < 0)
		{
			init.P = recCodeBiasVar;
		}

		init.x = recCodeBias;

		kfState.getKFValue(kfKey, init.x);

		auto& recSysOpts = acsConfig.getRecOpts(rec.id, {sysSat.sys._to_string()});

		auto thisIt = std::find		(recSysOpts.clock_codes.begin(), recSysOpts.clock_codes.end(), sig.code);
		auto autoIt = std::find		(recSysOpts.clock_codes.begin(), recSysOpts.clock_codes.end(), +E_ObsCode::AUTO);
		auto freqIt = std::find_if	(recSysOpts.clock_codes.begin(), recSysOpts.clock_codes.end(), [&](E_ObsCode code)
		{
			return code2Freq[obs.Sat.sys][code] == code2Freq[obs.Sat.sys][sig.code];
		});

		bool foundCode = thisIt != recSysOpts.clock_codes.end();
		bool foundAuto = autoIt != recSysOpts.clock_codes.end();
		bool foundFreq = freqIt != recSysOpts.clock_codes.end();

		if	( foundAuto
			&&foundFreq)
		{
			//this frequency is already used in the clock codes, dont use again
			foundAuto = false;
		}

		if	( foundCode
			||foundAuto)
		{
			//set the bias to zero, and dont let it change
			init.x = 0;
			init.P = 0;
			init.Q = 0;

			if (foundCode == false)
			{
				*autoIt = sig.code;
			}
		}

		recCodeBias = init.x;

		measEntry.addDsgnEntry	(kfKey, 1, init);

		recCodeBiasVar = -1;
	}

	measEntry.addNoiseEntry(kfKey, 1, recCodeBiasVar);

	measEntry.componentsMap[E_Component::REC_CODE_BIAS] = {recCodeBias, "+ d_" + std::to_string(ft) + "r", recCodeBiasVar};
};

inline void pppSatCodeBias(COMMON_PPP_ARGS)
{
	double	satCodeBias		=  		satOpts.codeBiasModel.default_bias;
	double	satCodeBiasVar	= SQR(	satOpts.codeBiasModel.undefined_sigma);
	getBias(trace, time, Sat, Sat, sig.code, CODE, satCodeBias, satCodeBiasVar);

	KFKey kfKey;
	kfKey.type		= KF::CODE_BIAS;
	kfKey.Sat		= Sat;
	kfKey.num		= sig.code;
	kfKey.comment	= sigName;

	InitialState init		= initialStateFromConfig(satOpts.code_bias);

	if (init.estimate)
	{
		init.x = satCodeBias;

		if (init.Q < 0)
		{
			init.P = satCodeBiasVar;
		}

		init.x = satCodeBias;

		kfState.getKFValue(kfKey, init.x);

		auto& satSysOpts = acsConfig.getSatOpts(obs.Sat);

		auto thisIt = std::find		(satSysOpts.clock_codes.begin(), satSysOpts.clock_codes.end(), sig.code);
		auto autoIt = std::find		(satSysOpts.clock_codes.begin(), satSysOpts.clock_codes.end(), +E_ObsCode::AUTO);
		auto freqIt = std::find_if	(satSysOpts.clock_codes.begin(), satSysOpts.clock_codes.end(), [&](E_ObsCode code)
		{
			return code2Freq[obs.Sat.sys][code] == code2Freq[obs.Sat.sys][sig.code];
		});

		bool foundCode = thisIt != satSysOpts.clock_codes.end();
		bool foundAuto = autoIt != satSysOpts.clock_codes.end();
		bool foundFreq = freqIt != satSysOpts.clock_codes.end();

		if	( foundAuto
			&&foundFreq)
		{
			//this frequency is already used in the clock codes, dont use again
			foundAuto = false;
		}

		if	( foundCode
			||foundAuto)
		{
			//set the bias to zero, and dont let it change
			init.x = 0;
			init.P = 0;
			init.Q = 0;

			if (foundCode == false)
			{
				*autoIt = sig.code;
			}
		}

		satCodeBias = init.x;

		measEntry.addDsgnEntry	(kfKey, 1, init);

		satCodeBiasVar = -1;
	}

	measEntry.addNoiseEntry(kfKey, 1, satCodeBiasVar);

	measEntry.componentsMap[E_Component::SAT_CODE_BIAS] = {satCodeBias, "+ d_" + std::to_string(ft) + "s", satCodeBiasVar};
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

		kfState.getKFValue(kfKey, init.x);

		double component = init.x;

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

	measEntry.componentsMap[E_Component::EOP] = {adjustment, "+ eop", -1};
}


void checkModels(
	ReceiverOptions&	recOpts,
	SatelliteOptions&	satOpts)
{
	auto test = [](
		string	label,
		bool	estimated,
		bool&	enable)
	{
		if	(  estimated
			&& enable == false)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: " << label << " is estimated but model is not enabled";
		}
	};

	test("rec_phase_bias",	recOpts.phase_bias.	estimate[0], recOpts.phaseBiasModel.enable);
	test("rec_code_bias",	recOpts.code_bias.	estimate[0], recOpts.codeBiasModel.	enable);
	test("sat_phase_bias",	satOpts.phase_bias.	estimate[0], satOpts.phaseBiasModel.enable);
	test("sat_code_bias",	satOpts.code_bias.	estimate[0], satOpts.codeBiasModel.	enable);
	test("emp_d_0",			satOpts.emp_d_0.	estimate[0], satOpts.empirical);
}












//redefine this to replace with nothing from now on - ie, use the argument name but not its type
#undef	COMMON_ARG
#define	COMMON_ARG(type)


void receiverPPP(
			Trace&				pppTrace,			///< Trace to output to
			Receiver&			rec,				///< Receiver to perform calculations for
	const	KFState&			kfState,			///< Kalman filter object containing the state parameters
			KFMeasEntryList&	kfMeasEntryList,	///< List to append kf measurements to
	const	KFState&			remoteKF)			///< Kalman filter object containing remote filter values
{
	auto trace		= getTraceFile(rec);
	auto jsonTrace	= getTraceFile(rec, true);

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
		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			continue;
		}

		auto& satOpts = acsConfig.getSatOpts(obs.Sat);

		satPosClk(trace, time, obs, nav, satOpts.posModel.sources, satOpts.clockModel.sources, &kfState, nullptr, E_OffsetType::COM, E_Relativity::OFF);

		traceJson(1, jsonTrace, time,
		{
			{"data",	__FUNCTION__		},
			{"Sat",		obs.Sat.id()		},
			{"Rec",		obs.mount			}
		},
		{
			{"rSatEciDt[0]",	obs.rSatEciDt[0]},
			{"rSatEciDt[1]",	obs.rSatEciDt[1]},
			{"rSatEciDt[2]",	obs.rSatEciDt[2]},
			{"satClk",			obs.satClk}
		});
	}

	ERPValues erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapper(time, erpv);

	for (auto&	obs				: only<GObs>(rec.obsList))
	for (auto&	[ft, sigList]	: obs.sigsLists)
	for (auto&	sig				: sigList)
	for (auto	measType		: {PHAS, CODE})
	{
		string		sigName			= sig.code._to_string();

		AutoSender autoSenderTemplate (1, jsonTrace, time);

		autoSenderTemplate.baseKVPs =
		{
			{"Sat",		obs.Sat.id()		},
			{"Rec",		obs.mount			},
			{"Sig",		sigName				},
			{"Type",	(long int) measType	}
		};

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

		tracepdeex(1, trace, "\n\n----------------------------------------------------");
		tracepdeex(1, trace, "\nProcessing %s: ", measDescription);

		if	( obs.ephPosValid == false
			||obs.ephClkValid == false
			)
		{
			tracepdeex(2,trace, "\n%s excludeSvh", obs.Sat.id().c_str());

			obs.excludeSVH = true;
		}

		if (obs.exclude)
		{
			auto& asKVPs = autoSenderTemplate.valueKVPs;

			if (acsConfig.exclude.bad_spp	&& obs.excludeBadSPP)			{	tracepdeex(5, trace, " - excludeBadSPP");		asKVPs.push_back({"exclude", "bad_spp"	});		continue;	}
			if (acsConfig.exclude.config	&& obs.excludeConfig)			{	tracepdeex(5, trace, " - excludeConfig");		asKVPs.push_back({"exclude", "config"	});		continue;	}
			if (acsConfig.exclude.eclipse	&& obs.excludeEclipse)			{	tracepdeex(5, trace, " - excludeEclipse");		asKVPs.push_back({"exclude", "eclipse"	});		continue;	}
			if (acsConfig.exclude.elevation	&& obs.excludeElevation)		{	tracepdeex(5, trace, " - excludeElevation");	asKVPs.push_back({"exclude", "elevation"});		continue;	}
			if (acsConfig.exclude.outlier	&& obs.excludeOutlier)			{	tracepdeex(5, trace, " - excludeOutlier");		asKVPs.push_back({"exclude", "outlier"	});		continue;	}
			if (acsConfig.exclude.system	&& obs.excludeSystem)			{	tracepdeex(5, trace, " - excludeSystem");		asKVPs.push_back({"exclude", "system"	});		continue;	}
			if (acsConfig.exclude.svh		&& obs.excludeSVH)				{	tracepdeex(5, trace, " - excludeSVH");			asKVPs.push_back({"exclude", "svh"		});		continue;	}
		}

		SatNav&		satNav			= *obs.satNav_ptr;
		SatStat&	satStat			= *obs.satStat_ptr;
		SigStat&	sigStat			= satStat.sigStatMap[sigName];
		SigStat&	preprocSigStat	= satStat.sigStatMap[ft2string(ft)];

		if (preprocSigStat.slip.any)
		{
			auto& asKVPs = autoSenderTemplate.valueKVPs;

			if (acsConfig.exclude.LLI		&& preprocSigStat.slip.LLI)		{	tracepdeex(2, trace, " - LLI slip excluded");	asKVPs.push_back({"excludeSlip","LLI"	});		continue;	}
			if (acsConfig.exclude.GF		&& preprocSigStat.slip.GF)		{	tracepdeex(2, trace, " - GF slip excluded");	asKVPs.push_back({"excludeSlip","GF"	});		continue;	}
			if (acsConfig.exclude.MW		&& preprocSigStat.slip.MW)		{	tracepdeex(2, trace, " - MW slip excluded");	asKVPs.push_back({"excludeSlip","MW"	});		continue;	}
			if (acsConfig.exclude.SCDIA		&& preprocSigStat.slip.SCDIA)	{	tracepdeex(2, trace, " - SCDIA slip excluded");	asKVPs.push_back({"excludeSlip","SCDIA"	});		continue;	}
		}

		auto& satOpts = acsConfig.getSatOpts(obs.Sat,	{sigName});
		auto& recOpts = acsConfig.getRecOpts(rec.id,	{obs.Sat.sys._to_string(), sigName});

		checkModels(recOpts, satOpts);

		auto		Sat			= obs.Sat;
		auto		sys			= Sat.sys;
		auto		sysSat		= SatSys(sys);
		double		lambda		= obs.satNav_ptr->lamMap[ft];
		auto		code		= sig.code;

		string recSatId;
		if (recOpts.sat_id.empty())	recSatId		= rec.id;
		else						recSatId		= (string)SatSys(recOpts.sat_id.c_str());

		SatSys recSat(recSatId.c_str());

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

		{
			measEntry.metaDataMap["ionoOutageCount"] = &satStat.ionoOutageCount;
			tracepdeex(2,trace,"\n PPP Iono  counts: %s %s %s %d", rec.id.c_str(), obs.Sat.id().c_str(), sig.code._to_string(), satStat.ionoOutageCount);
		}

		measEntry.obsKey.Sat	= obs.Sat;
		measEntry.obsKey.str	= rec.id;
		measEntry.obsKey.num	= sig.code;
		if		(measType == CODE)		{	measEntry.obsKey.type = KF::CODE_MEAS;	measEntry.obsKey.comment = "P-" + (string)sig.code._to_string();	}
		else if	(measType == PHAS)		{	measEntry.obsKey.type = KF::PHAS_MEAS;	measEntry.obsKey.comment = "L-" + (string)sig.code._to_string();	}

		//Start with the observed measurement and its noise
		{
			KFKey obsKey;
			obsKey.str		= rec.id;
			obsKey.Sat		= obs.Sat;
			obsKey.type		= measEntry.obsKey.type;
			obsKey.num		= sig.code;

			double var = 0;
			if		(measType == CODE)	{	var = sig.codeVar;	}
			else if	(measType == PHAS)	{	var = sig.phasVar;	}

			measEntry.addNoiseEntry(obsKey, 1, var);

			measEntry.componentsMap[E_Component::OBSERVED] = {-observed, "- Phi", var};
		}

		//Calculate the basic range

		VectorEcef rRec			= rec.aprioriPos;

		{
			for (int i = 0; i < 3; i++)
			{
				InitialState init = initialStateFromConfig(recOpts.pos, i);

				KFKey kfKey;
				kfKey.type		= KF::REC_POS;
				kfKey.str		= rec.id;
				kfKey.num		= i;
				kfKey.rec_ptr	= &rec;
				kfKey.comment	= init.comment;

				if (init.estimate == false)
				{
					remoteKF.getKFValue(kfKey, rRec[i]);		//todo aaron this would be nice if it came from a function instead

					continue;
				}

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
					InitialState posInit = initialStateFromConfig(recOpts.orbit, i);
					InitialState velInit = initialStateFromConfig(recOpts.orbit, i + 3);

					KFKey posKey;
					posKey.type		= KF::ORBIT;
					posKey.num		= i;
					posKey.comment	= posInit.comment;

					if (recSat.prn)		posKey.Sat	= recSat;
					else				posKey.str	= recSatId;

					KFKey velKey	= posKey;
					velKey.num		= i + 3;
					velKey.comment	= velInit.comment;


					found |= kfState.getKFValue(posKey, rRecInertial[i]);

					if (posInit.x == 0)			posInit.x = rRecInertial[i];

					VectorEci eSatInertial	= frameSwapper(satStat.e);

					measEntry.addDsgnEntry	(posKey,			-eSatInertial[i],				posInit);

					kfState.addKFState		(velKey, 											velInit);
				}

				if (found)
				{
					rRec = frameSwapper(rRecInertial);
				}

				addEmpStates(recOpts, kfState, recSatId);
			}
		}

		auto& pos = rec.pos;

		pos = ecef2pos(rRec);

		VectorEcef& rSat = obs.rSatCom;

		if (obs.rSatCom.isZero())
		{
			BOOST_LOG_TRIVIAL(error) << "Error: Satpos is unexpectedly zero for " << rec.id << " - " << Sat.id();
			continue;
		}

		if (initialStateFromConfig(satOpts.orbit).estimate)
		for (int i = 0; i < 3; i++)
		{
			InitialState posInit = initialStateFromConfig(satOpts.orbit, i);
			InitialState velInit = initialStateFromConfig(satOpts.orbit, i + 3);

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
				bool pass = satpos(trace, time, time, satPos0, satOpts.posModel.sources, E_OffsetType::COM, nav, &kfState);

				obs.rSatEci0 = frameSwapper(satPos0.rSatCom, &satPos0.satVel, &obs.vSatEci0);
			}
			if (posInit.x == 0)			posInit.x = obs.rSatEci0[i];
			if (velInit.x == 0)			velInit.x = obs.vSatEci0[i];

			VectorEci eSatInertial	= frameSwapper(satStat.e);

			measEntry.addDsgnEntry(posKey,	+eSatInertial[i],							posInit);
			measEntry.addDsgnEntry(velKey,	-eSatInertial[i] * (obs.tof + obs.satClk),	velInit);

			addEmpStates(satOpts, kfState, Sat);
		}


		if (initialStateFromConfig(recOpts.orientation).estimate)
		{
			Matrix3d body2Ecef = rotBasisMat(rec.attStatus.eXBody, rec.attStatus.eYBody, rec.attStatus.eZBody);

			Quaterniond quat = Quaterniond(body2Ecef);

			Quaterniond roter(Eigen::AngleAxis(90 * PI/ 180, Vector3d::UnitZ()));

			Quaterniond extra2 = quat * roter;

			for (int i = 0; i < 4; i++)
			{
				InitialState init = initialStateFromConfig(recOpts.orientation, i);

				KFKey kfKey;
				kfKey.type	= KF::ORIENTATION;
				kfKey.str	= rec.id;
				kfKey.num	= i;

				if (i == 0)		{	init.x = extra2.w();		kfState.getKFValue(kfKey, init.x);		quat.w() = init.x;	}
				if (i == 1)		{	init.x = extra2.x();		kfState.getKFValue(kfKey, init.x);		quat.x() = init.x;	}
				if (i == 2)		{	init.x = extra2.y();		kfState.getKFValue(kfKey, init.x);		quat.y() = init.x;	}
				if (i == 3)		{	init.x = extra2.z();		kfState.getKFValue(kfKey, init.x);		quat.z() = init.x;	}

				kfState.addKFState(kfKey, init);
			}
		}

		tracepdeex(3, trace, "\n%s rSatEcef   : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.rSatCom		.x(),	obs.rSatCom		.y(),	obs.rSatCom		.z());
		tracepdeex(3, trace, "\n%s vSatEcef   : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.satVel		.x(),	obs.satVel		.y(),	obs.satVel		.z());
		tracepdeex(4, trace, "\n%s rSatEciDt  : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.rSatEciDt	.x(),	obs.rSatEciDt	.y(),	obs.rSatEciDt	.z());
		tracepdeex(4, trace, "\n%s rSatEci0   : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.rSatEci0	.x(),	obs.rSatEci0	.y(),	obs.rSatEci0	.z());


		//Range


		double rRecSat	= (rSat - rRec).norm();
		satStat.e		= (rSat - rRec).normalized();
		satStat.nadir	= acos(satStat.e.dot(rSat.normalized()));

// 		VectorEci rRecEci	= frameSwapper(rRec);
// 		obs.rSatEciDt		= frameSwapper(rSat, obs.time - obs.tof - obs.dtSat[0]);
//
// 		rRecSat = (obs.rSatEciDt - rRecEci).norm();

		if (recOpts.range)
		{
			measEntry.componentsMap[E_Component::RANGE] = {rRecSat, "+ rho", 0};
		}

		if (acsConfig.output_residual_chain)
		{
			tracepdeex(0, trace, "\n----------------------------------------------------");
			tracepdeex(0, trace, "\nMeasurement for %s %s\n", ((string) measEntry.obsKey).c_str(), sig.code._to_string());
		}

		//Add modelled adjustments and estimated parameter
		{
			if (recOpts.clockModel.enable)			{	Instrument inst("pppRecClocks");		pppRecClocks		(COMMON_PPP_ARGS);	}
			if (satOpts.clockModel.enable)			{	Instrument inst("pppSatClocks");		pppSatClocks		(COMMON_PPP_ARGS);	}
			if (recOpts.eccentricityModel.enable)	{	Instrument inst("pppRecAntDelta");		pppRecAntDelta		(COMMON_PPP_ARGS);	}
	// 		if (acsConfig.model.sat_ant_delta)		{	Instrument inst("pppSatAntDelta");		pppSatAntDelta		(COMMON_PPP_ARGS);	}
			if (recOpts.pcoModel.enable)			{	Instrument inst("pppRecPCO");			pppRecPCO			(COMMON_PPP_ARGS);	}
			if (satOpts.pcoModel.enable)			{	Instrument inst("pppSatPCO");			pppSatPCO			(COMMON_PPP_ARGS);	}
			if (recOpts.pcvModel.enable)			{	Instrument inst("pppRecPCV");			pppRecPCV			(COMMON_PPP_ARGS);	}
			if (satOpts.pcvModel.enable)			{	Instrument inst("pppSatPCV");			pppSatPCV			(COMMON_PPP_ARGS);	}
			if (recOpts.tideModels.enable)			{	Instrument inst("pppTides");			pppTides			(COMMON_PPP_ARGS);	}
			if (recOpts.relativity)					{	Instrument inst("pppRelativity");		pppRelativity		(COMMON_PPP_ARGS);	}
			if (recOpts.relativity2)				{	Instrument inst("pppRelativity2");		pppRelativity2		(COMMON_PPP_ARGS);	}
			if (recOpts.sagnac)						{	Instrument inst("pppSagnac");			pppSagnac			(COMMON_PPP_ARGS);	}
			if (recOpts.ionospheric_component)		{	Instrument inst("pppIonStec");			pppIonStec			(COMMON_PPP_ARGS);	}
			if (recOpts.ionospheric_component2)		{	Instrument inst("pppIonStec2");			pppIonStec2			(COMMON_PPP_ARGS);  }
			if (recOpts.ionospheric_component3)		{	Instrument inst("pppIonStec3");			pppIonStec3			(COMMON_PPP_ARGS);  }
			if (recOpts.ionospheric_model)			{	Instrument inst("pppIonModel");			pppIonModel			(COMMON_PPP_ARGS);	}
			if (recOpts.tropModel.enable)			{	Instrument inst("pppTrop");				pppTrop				(COMMON_PPP_ARGS);	}
// 			if (recOpts.orbits)						{	Instrument inst("pppOrbits");			pppOrbits			(COMMON_PPP_ARGS);	}
			if (recOpts.eop)						{	Instrument inst("pppEopAdjustment");	pppEopAdjustment	(COMMON_PPP_ARGS);  }
		}

		if (measType == PHAS)
		{
			if (recOpts.phaseWindupModel.enable)	{	Instrument inst("pppRecPhaseWindup");	pppRecPhaseWindup	(COMMON_PPP_ARGS);	}
			if (satOpts.phaseWindupModel.enable)	{	Instrument inst("pppSatPhaseWindup");	pppSatPhaseWindup	(COMMON_PPP_ARGS);	}
			if (recOpts.integer_ambiguity)			{	Instrument inst("pppIntegerAmbiguity");	pppIntegerAmbiguity	(COMMON_PPP_ARGS);	}
			if (recOpts.phaseBiasModel.enable)		{	Instrument inst("pppRecPhasBias");		pppRecPhasBias		(COMMON_PPP_ARGS);	}
			if (satOpts.phaseBiasModel.enable)		{	Instrument inst("pppSatPhasBias");		pppSatPhasBias		(COMMON_PPP_ARGS);	}
		}

		if (measType == CODE)
		{
			if (recOpts.codeBiasModel.enable)		{	Instrument inst("pppRecCodeBias");		pppRecCodeBias		(COMMON_PPP_ARGS);	}
			if (satOpts.codeBiasModel.enable)		{	Instrument inst("pppSatCodeBias");		pppSatCodeBias		(COMMON_PPP_ARGS);	}
		}

		addNilDesignStates(recOpts.gyro_bias,			kfState,	KF::GYRO_BIAS,	3, recSatId);
		addNilDesignStates(recOpts.accelerometer_bias,	kfState,	KF::ACCL_BIAS,	3, recSatId);
		addNilDesignStates(recOpts.gyro_scale,			kfState,	KF::GYRO_SCALE,	3, recSatId);
		addNilDesignStates(recOpts.accelerometer_scale,	kfState,	KF::ACCL_SCALE,	3, recSatId);
		addNilDesignStates(recOpts.imu_offset,			kfState,	KF::IMU_OFFSET,	3, recSatId);

		//Calculate residuals and form up the measurement

		measEntry.componentsMap[E_Component::NET_RESIDUAL] = {0, "", 0};
		double residual		= 0;
		double residualVar	= 0;

		AutoSender autoSender = autoSenderTemplate;
		autoSender.baseKVPs	.push_back({"data", __FUNCTION__});

		autoSender.valueKVPs.push_back({"rRec[0]",	rRec[0]});
		autoSender.valueKVPs.push_back({"rRec[1]",	rRec[1]});
		autoSender.valueKVPs.push_back({"rRec[2]",	rRec[2]});
		autoSender.valueKVPs.push_back({"El",		satStat.el});
		autoSender.valueKVPs.push_back({"Az",		satStat.az});
		autoSender.valueKVPs.push_back({"Nadir",	satStat.nadir});

		for (auto& [component, details] : measEntry.componentsMap)
		{
			auto& [componentVal, eq, var] = details;

			residual -= componentVal;

			if (var > 0)
			{
				residualVar += var;
			}

			if (acsConfig.output_residual_chain)
			{
				tracepdeex(0, trace, "\n");
				tracepdeex(4, trace, "%s",		time.to_string().c_str());
				tracepdeex(3, trace, "%30s",	((string)measEntry.obsKey).c_str());
				tracepdeex(0, trace, " %-21s %+14.4f", component._to_string(), -componentVal);

				if		(var >	0)		tracepdeex(3, trace, " ± %.2e ", var);
				else if (var == 0)		tracepdeex(3, trace, " ± 0        ");
				else					tracepdeex(3, trace, "   Estimated");

				tracepdeex(0, trace, " -> %13.4f",	residual);
				tracepdeex(3, trace, " ± %.2e ",	residualVar);

				autoSender.valueKVPs.push_back({component._to_string(), componentVal});
			}
		}

		if (1)
		for (auto& [component, details] : measEntry.componentsMap)
		{
			auto& [componentVal, eq, var] = details;

			if (var > 100)
			{
				BOOST_LOG_TRIVIAL(warning)
				<< "Warning: Unestimated component '" << component._to_string() << "' for '" << measEntry.obsKey
				<< "' has large variance (" << var << "), valid inputs may not (yet) be available";

				trace << std::endl
				<< "Warning: Unestimated component '" << component._to_string() << "' for '" << measEntry.obsKey
				<< "' has large variance (" << var << "), valid inputs may not (yet) be available";
			}
		}

		if (acsConfig.output_residual_chain)
		{
			trace << std::endl << std::endl << "0 =";

			for (auto& [component, details] : measEntry.componentsMap)
			{
				auto& [componentVal, eq, var] = details;

				tracepdeex(0, trace, " %s", eq.c_str());
			}
		}

		if (abs(residual) > 1e30)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: " << measEntry.obsKey << " has very large residual: " << residual;
		}

		measEntry.setInnov(residual);

		kfMeasEntryList.push_back(measEntry);
	}

	trace << std::endl << std::endl;
}

