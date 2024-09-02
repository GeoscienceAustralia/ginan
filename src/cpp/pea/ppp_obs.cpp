
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

/**
 */
ParallelArchitecture UDUC_GNSS_Measurements__()
{

}

/** References
* 1. M.Fritsche, R.Dietrich, C.Knöfel, A.Rülke, S.Vey, M.Rothacher & P.Steigenberger, Impact of higher‐order ionospheric terms on GPS estimates. Geophysical research letters, 2005.
* 2. GAMIT 10.71
* 3. U.Hugentobler, S.Schaer, G.Beutler, H.Bock, R.Dach, A.Jäggi, M.Meindl, C.Urschl, L.Mervart, M.Rothacher & U.Wild, CODE IGS analysis center technical report 2002, 2002.
*/

#include <sstream>

using std::ostringstream;


#include "interactiveTerminal.hpp"
#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "geomagField.hpp"
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

#include <functional>

using std::function;


struct AutoSender
{
	Trace&	trace;
	GTime	time;
private:
	vector<ArbitraryKVP> baseKVPs;
	vector<ArbitraryKVP> valueKVPs;

public:
	AutoSender(
		Trace&	trace,
		GTime	time)
	:	trace	{trace},
		time	{time}
	{

	}

	void setBaseKVPs(	int level, vector<ArbitraryKVP>&&	kvps)	{	if (level < traceLevel)		return;		baseKVPs	= std::move(kvps);			}
	void setValueKVPs(	int level, vector<ArbitraryKVP>&&	kvps)	{	if (level < traceLevel)		return;		valueKVPs	= std::move(kvps);			}

	void pushBaseKVP(	int level, ArbitraryKVP&&			kvp)	{	if (level < traceLevel)		return;		baseKVPs	.push_back(std::move(kvp));	}
	void pushValueKVP(	int level, ArbitraryKVP&&			kvp)	{	if (level < traceLevel)		return;		valueKVPs	.push_back(std::move(kvp));	}

	~AutoSender()
	{
		if	( baseKVPs	.empty()
			||valueKVPs	.empty())
		{
			return;
		}

		traceJson(0, trace, time, baseKVPs, valueKVPs);
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
	COMMON_ARG(			double&				)	lambda,				\
	COMMON_ARG(			SatSys&				)	sysSat,				\
	COMMON_ARG(			AutoSender&			)	autoSenderTemplate,	\
	COMMON_ARG(			VectorPos&			)	pos,				\
	COMMON_ARG(			ERPValues&			)	erpv,				\
	COMMON_ARG(			FrameSwapper&		)	frameSwapper


double relativity2(
	VectorEcef&		rSat,
	VectorEcef&		rRec)
{
	double rRecSat	= (rSat - rRec).norm();

	double ln		= log(	(rSat.norm() + rRec.norm() + rRecSat)
						/	(rSat.norm() + rRec.norm() - rRecSat));
	return 2 * MU * ln / CLIGHT / CLIGHT / CLIGHT;
}

tuple<Vector3d, Vector3d, Vector3d, Vector3d, Vector3d> tideDelta(
	Trace&				trace,
	GTime				time,
	Receiver&			rec,
	VectorEcef&			rRec,
	ReceiverOptions&	recOpts)
{
	auto& tideVectors = rec.pppTideCache.useCache([&]() -> tuple<Vector3d, Vector3d, Vector3d, Vector3d, Vector3d>
	{
		Vector3d solid	= Vector3d::Zero();
		Vector3d otl	= Vector3d::Zero();
		Vector3d atl	= Vector3d::Zero();
		Vector3d spole	= Vector3d::Zero();
		Vector3d opole	= Vector3d::Zero();

		if	( recOpts.tideModels.solid
			||recOpts.tideModels.otl
			||recOpts.tideModels.atl
			||recOpts.tideModels.spole
			||recOpts.tideModels.opole)
		{
			tideDisp(trace, time, rec, rRec, solid, otl, atl, spole, opole);
		}

		return {solid, otl, atl, spole, opole};
	});

	return tideVectors;
}

void eopAdjustment(
	GTime&			time,
	VectorEcef&		e,
	ERPValues&		erpv,
	FrameSwapper&	frameSwapper,
	Receiver&		rec,
	VectorEcef&		rRec,
	KFMeasEntry&	measEntry,
	const KFState&	kfState)
{
	Matrix3d partialMatrix	= stationEopPartials(rRec);
	Vector3d eopPartials	= partialMatrix * e;

	for (int i = 0; i < 3; i++)
	{
		InitialState init			= initialStateFromConfig(acsConfig.pppOpts.eop,			i);
		InitialState eopRateInit	= initialStateFromConfig(acsConfig.pppOpts.eop_rates,	i);

		if (init.estimate == false)
		{
			continue;
		}

		KFKey kfKey;
		kfKey.type		= KF::EOP;
		kfKey.num		= i;
		kfKey.comment	= eopComments[i];

		kfState.getKFValue(kfKey, init.x);

		if (init.x == 0)
		switch (i)
		{
			case 0:	init.x = erpv.xp		* R2MAS;		eopRateInit.x = +erpv.xpr	* R2MAS;	break;
			case 1:	init.x = erpv.yp		* R2MAS;		eopRateInit.x = +erpv.ypr	* R2MAS;	break;
			case 2:	init.x = erpv.ut1Utc	* S2MTS;		eopRateInit.x = -erpv.lod	* S2MTS;	break;
		}

		measEntry.addDsgnEntry(kfKey,	eopPartials(i),				init);

		if (eopRateInit.estimate == false)
		{
			continue;
		}

		KFKey rateKey;
		rateKey.type	= KF::EOP_RATE;
		rateKey.num		= i;
		kfKey.comment	= (string) eopComments[i] + "/day";

		kfState.setKFTransRate(kfKey, rateKey,	1/S_IN_DAY,	eopRateInit);
	}

	if (acsConfig.pppOpts.add_eop_component)
	{
		auto& [eopAdjustment] = rec.pppEopCache.useCache([&]() -> tuple<Vector3d>
		{
			ERPValues erpvBase = getErp(nav.erp, time, false);

			FrameSwapper frameSwapperBase(time, erpvBase);

			Matrix3d erpEcefAdjustmentMat	= frameSwapper.i2t_mat * frameSwapperBase.i2t_mat.transpose()
											- Matrix3d::Identity();

			Vector3d eopAdjustment			= erpEcefAdjustmentMat * rRec;

			return {eopAdjustment};
		});

		double adjustment = e.dot(eopAdjustment);

		measEntry.componentsMap[E_Component::EOP] = {adjustment, "+ eop", -1};
	}
}

inline static void pppRecClocks(COMMON_PPP_ARGS)
{
	double	recClk_m	= rec.aprioriClk;
	double	dtRecVar	= rec.aprioriClkVar;

	KFKey kfKey;
	kfKey.type		= KF::REC_CLOCK;
	kfKey.str		= rec.id;
	kfKey.rec_ptr	= &rec;

	E_Source found = kfState.getKFValue(kfKey, recClk_m, &dtRecVar);

	for (int i = 0; i < recOpts.clk.estimate.size(); i++)
	{
		InitialState init		= initialStateFromConfig(recOpts.clk, i);

		if (init.estimate == false)
		{
			continue;
		}

		if	( found == +E_Source::REMOTE
			&&init.use_remote_sigma)
		{
			init.P = dtRecVar;
		}

		dtRecVar = -1;

		if (i == 0)
		{
			init.x		= recClk_m;
			recClk_m	= 0;
		}

		kfKey.num		= i;
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
	}

	measEntry.addNoiseEntry(kfKey, 1, dtRecVar);

	measEntry.componentsMap[E_Component::REC_CLOCK] = {recClk_m, "+ Cdt_r", dtRecVar};
}

inline static void pppSatClocks(COMMON_PPP_ARGS)
{
	double satClk_m		= obs.satClk * CLIGHT;
	double satClkVar	= 0;

	KFKey kfKey;
	kfKey.type		= KF::SAT_CLOCK;
	kfKey.Sat		= Sat;

	E_Source found = kfState.getKFValue(kfKey, satClk_m, &satClkVar);

	for (int i = 0; i < satOpts.clk.estimate.size(); i++)
	{
		InitialState init		= initialStateFromConfig(satOpts.clk, i);

		if (init.estimate == false)
		{
			continue;
		}

		if	( found == +E_Source::REMOTE
			&&init.use_remote_sigma)
		{
			init.P = satClkVar;
		}

		satClkVar = -1;

		if (i == 0)
		{
			init.x		= satClk_m;
			satClk_m	= 0;
		}

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
	}

	measEntry.addNoiseEntry(kfKey, 1, satClkVar);

	measEntry.componentsMap[E_Component::SAT_CLOCK] = {-satClk_m, "- Cdt_s", satClkVar};
}

inline static void pppRecAntDelta(COMMON_PPP_ARGS)
{
	Vector3d	bodyAntVector	= rec.antDelta;
	Vector3d	bodyLook		= ecef2body(rec.attStatus, satStat.e);

	double variance = 0;

	for (int i = 0; i < 3; i++)
	{
		int enumNum = KF::ANT_DELTA;

		InitialState init = initialStateFromConfig(recOpts.ant_delta, i);

		if (init.estimate == false)
		{
			continue;
		}

		if (init.Q < 0)
		{
			init.P = variance;
		}

		variance = -1;

		KFKey kfKey;
		kfKey.type		= KF::ANT_DELTA;
		kfKey.str		= rec.id;
		kfKey.num		= i;

		init.x = bodyAntVector(i);

		kfState.getKFValue(kfKey, init.x);

		bodyAntVector(i) = init.x;

		measEntry.addDsgnEntry(kfKey, -bodyLook(i), init);
	}

	//todo aaron needs noise

	double recAntDelta = -bodyAntVector.dot(bodyLook);

	measEntry.componentsMap[E_Component::REC_ANTENNA_DELTA] = {recAntDelta, "- E.dR_r", 0};
}

inline static void pppRecPCO(COMMON_PPP_ARGS)
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

		if (init.estimate == false)
		{
			continue;
		}

		if (init.Q < 0)
		{
// 			init.P = variance;	//bad things happen (no iflc) if this is zero, as is often the case for varIono
		}

		variance = -1;

		KFKey kfKey;
		kfKey.type		= KF::_from_integral(enumNum);
		kfKey.str		= rec.id;
		kfKey.num		= recAtxFt;

		init.x = bodyPCO(i);

		kfState.getKFValue(kfKey, init.x);

		bodyPCO(i) = init.x;

		measEntry.addDsgnEntry(kfKey, -bodyLook(i), init);
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

	//todo aaron, needs noise

	double recPCODelta = -bodyPCO.dot(bodyLook);

	measEntry.componentsMap[E_Component::REC_PCO] = {recPCODelta, "- E.PCO_r", variance};
}

inline static void pppSatPCO(COMMON_PPP_ARGS)
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

		if (init.estimate == false)
		{
			continue;
		}

		if (init.Q < 0)
		{
// 			init.P = variance;	//bad things happen (no iflc) if this is zero, as is often the case for varIono
		}

		variance = -1;

		KFKey kfKey;
		kfKey.type		= KF::_from_integral(enumNum);
		kfKey.Sat		= obs.Sat;
		kfKey.num		= satAtxFt;

		init.x = bodyPCO(i);

		kfState.getKFValue(kfKey, init.x);

		bodyPCO(i) = init.x;

		measEntry.addDsgnEntry(kfKey, bodyLook(i), init);
	}

	AutoSender autoSender = autoSenderTemplate;

	autoSender.pushBaseKVP	(4, {"data", __FUNCTION__});
	autoSender.setValueKVPs	(4,
	{
		{"bodyLook[0]",	bodyLook[0]},
		{"bodyLook[1]",	bodyLook[1]},
		{"bodyLook[2]",	bodyLook[2]},
		{"bodyPCO[0]",	bodyPCO[0]},
		{"bodyPCO[1]",	bodyPCO[1]},
		{"bodyPCO[2]",	bodyPCO[2]},
		{"nominalYaw",	attStatus.nominalYaw},
		{"modelYaw",	attStatus.modelYaw}
	});

	//todo aaron, needs noise

	double satPCODelta = bodyPCO.dot(bodyLook);

	measEntry.componentsMap[E_Component::SAT_PCO] = {satPCODelta, "+ E.PCO_s", variance};
}

inline static void pppRecPCV(COMMON_PPP_ARGS)
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
}

inline static void pppSatPCV(COMMON_PPP_ARGS)
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
}

inline static void pppTides(COMMON_PPP_ARGS)
{
	auto [solid, otl, atl, spole, opole] = tideDelta(trace, time, rec, rRec, recOpts);

	measEntry.componentsMap[E_Component::TIDES_SOLID	] = {-satStat.e.dot(solid),	"- E.dT1", 0};
	measEntry.componentsMap[E_Component::TIDES_OTL		] = {-satStat.e.dot(otl),	"- E.dT2", 0};
	measEntry.componentsMap[E_Component::TIDES_ATL		] = {-satStat.e.dot(atl),	"- E.dT3", 0};
	measEntry.componentsMap[E_Component::TIDES_SPOLE	] = {-satStat.e.dot(spole),	"- E.dT4", 0};
	measEntry.componentsMap[E_Component::TIDES_OPOLE	] = {-satStat.e.dot(opole),	"- E.dT5", 0};
};

inline static void pppRelativity(COMMON_PPP_ARGS)
{
	/* note that relativity effect to estimate sat clock */
	double dtRel1	= relativity1(rSat, obs.satVel);

	measEntry.componentsMap[E_Component::RELATIVITY1] = {dtRel1	* CLIGHT, "+ rel1", 0};
}

inline static void pppRelativity2(COMMON_PPP_ARGS)
{
	/* secondary relativity effect (Shapiro effect) */
	double dtRel2 	= relativity2(rSat, rRec);

	measEntry.componentsMap[E_Component::RELATIVITY2] = {dtRel2	* CLIGHT, "+ rel2", 0};
}

inline static void pppSagnac(COMMON_PPP_ARGS)
{
	double dSagnac	= sagnac(rSat, rRec);

	measEntry.componentsMap[E_Component::SAGNAC] = {dSagnac, "+ sag", 0};
}

inline static void pppIonStec(COMMON_PPP_ARGS)
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
			double ionC = SQR(lambda / genericWavelength[F1]);

			ionosphere_m = factor * ionC * diono;
		}
	}

	double ionosphereStec = ionosphere_m / (factor * alpha);

	string recStr;
	if (acsConfig.pppOpts.equate_ionospheres == false)
		recStr	= rec.id;

	KFKey kfKey;
	kfKey.type		= KF::IONO_STEC;
	kfKey.str		= recStr;
	kfKey.Sat		= obs.Sat;
	kfKey.rec_ptr	= &rec;

	if (acsConfig.pppOpts.ionoOpts.common_ionosphere == false)
	{
		kfKey.num		=  measType;

		if		(measType == CODE)		{	kfKey.comment = "CODE";		}
		else if	(measType == PHAS)		{	kfKey.comment = "PHAS";		}
	}

	kfKey.comment	+= init.comment;

	E_Source found = kfState.getKFValue(kfKey, ionosphereStec, &varIono);

	if (init.estimate)
	{
		if	( found == +E_Source::REMOTE
			&&init.use_remote_sigma)
		{
			init.P = varIono;
		}

		if (init.Q < 0)
		{
// 			init.P = varIono;	//bad things happen (no iflc) if this is zero, as is often the case for varIono
		}

		varIono = -1;

		init.x = ionosphereStec;

		ionosphereStec  = init.x;

		ionosphere_m = factor * alpha * ionosphereStec;

		measEntry.addDsgnEntry(kfKey, factor * alpha, init);
	}

	//todo aaron, needs noise

	measEntry.componentsMap[E_Component::IONOSPHERIC_COMPONENT] = {ionosphere_m, "+ " + std::to_string(factor * alpha) + ".I", varIono};
}

/** 2nd order ionospheric correction
* See ref [1]
*/
inline static void pppIonStec2(COMMON_PPP_ARGS)
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
		if (acsConfig.pppOpts.equate_ionospheres == false)
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
}

/** 3rd order ionospheric correction
* See ref [1]
*/
inline static void pppIonStec3(COMMON_PPP_ARGS)
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
		if (acsConfig.pppOpts.equate_ionospheres == false)
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
}

inline static void pppIonModel(COMMON_PPP_ARGS)
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
}

inline static void pppTropMap(COMMON_PPP_ARGS)
{
	// double troposphere_m	= tropDryZTD(trace, recOpts.tropModel.models, kfState.time, pos);
	TropStates	tropStates;
	TropMapping	dTropDx;
	double		varTrop = 0;

	double modelTroposphere_m = tropModel(trace, recOpts.tropModel.models, time, pos, satStat, tropStates, dTropDx, varTrop);

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

			double value = 0;
			bool pass = kfState.getKFValue(kfKey, value);

			modelTroposphere_m += geoMap * dTropDx.wetMap * value;

			tracepdeex(0, trace, "\n Trop Model (%3d);  + %.4e x %.4e = %.4e", i, geoMap * dTropDx.wetMap, value, modelTroposphere_m);

			measEntry.addDsgnEntry(kfKey, geoMap * dTropDx.wetMap, init);
		}
	}

	measEntry.componentsMap[E_Component::TROPOSPHERE_MODEL] = {modelTroposphere_m, "+ " + std::to_string(dTropDx.wetMap) + ".T", -1};
}

inline static void pppTrop(COMMON_PPP_ARGS)
{
	TropStates	tropStates;
	TropMapping	dTropDx;
	double		varTrop		= 0;
	double		filterVar	= 0;
	double		gradVars[2]	= {};

	double troposphere_m	= 0;

	string recStr;
	if (acsConfig.pppOpts.equate_tropospheres == false)
	{
		recStr	= rec.id;
	}

	E_Source found		= E_Source::NONE;
	E_Source gradsFound	= E_Source::NONE;

	//get the previous filter states for linearisation around this operating point
	for (int i = 0; i < recOpts.trop.estimate.size(); i++)
	{
		KFKey kfKey;
		kfKey.type	= KF::TROP;
		kfKey.str	= recStr;
		kfKey.num	= i;

		double value = 0;
		found = kfState.getKFValue(kfKey, value, &filterVar);

		tropStates.zenith += value;
	}

	for (short i = 0; i < 2; i++)
	{
		KFKey kfKey;
		kfKey.type	= KF::TROP_GRAD;
		kfKey.str	= recStr;
		kfKey.num	= i;

		gradsFound = kfState.getKFValue(kfKey, tropStates.grads[i], &gradVars[i]);
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

		if	( found == +E_Source::REMOTE
			&&init.use_remote_sigma)
		{
			init.P = filterVar;
		}

		if (i == 0)
		{
			init.x = tropStates.zenith;
		}

		if (init.Q < 0)
		{
			init.P = varTrop;
		}

		varTrop = -1;

		KFKey kfKey;
		kfKey.type		= KF::TROP;
		kfKey.str 		= recStr;
		kfKey.num		= i;
		kfKey.comment	= init.comment;

		measEntry.addDsgnEntry(kfKey, dTropDx.wetMap, init);
	}

	for (short i = 0; i < 2; i++)
	{
		InitialState init = initialStateFromConfig(recOpts.trop_grads, i);

		if (init.estimate == false)
		{
			continue;
		}

		if	( gradsFound == +E_Source::REMOTE
			&&init.use_remote_sigma)
		{
			init.P = gradVars[i];
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
}

inline static void pppRecPhaseWindup(COMMON_PPP_ARGS)
{
	phaseWindup(obs, rec, satStat.phw);

	double phaseWindup_m = satStat.phw * lambda;

	AutoSender autoSender = autoSenderTemplate;
	autoSender.pushValueKVP(4, {"satStat-phw",	satStat.phw});

	measEntry.componentsMap[E_Component::PHASE_WIND_UP] = {phaseWindup_m, "+ phi", 0};
}

inline static void pppSatPhaseWindup(COMMON_PPP_ARGS)
{
// 	phaseWindup(obs, rec, satStat.phw);
//
// 	double phaseWindup_m = satStat.phw * lambda;
//
// 	AutoSender autoSender = autoSenderTemplate;
// 	autoSender.valueKVPs.push_back({"satStat-phw",	satStat.phw});
//
// 	measEntry.componentsMap[E_Component::PHASE_WIND_UP] = {phaseWindup_m, "+ phi", 0};
}

inline static void pppIntegerAmbiguity(COMMON_PPP_ARGS)
{
	double ambiguity	= 0;
	double ambiguity_m	= 0;
	double varAmb		= 0;

	KFKey kfKey;
	kfKey.type		= KF::AMBIGUITY;
	kfKey.str		= rec.id;
	kfKey.Sat		= obs.Sat;
	kfKey.num		= sig.code;
	kfKey.rec_ptr	= &rec;
	kfKey.comment	= sigName;

	if (sig.P)
	{
		ambiguity = sig.L - sig.P / lambda;
	}

	E_Source found = kfState.getKFValue(kfKey, ambiguity, &varAmb);

	InitialState init = initialStateFromConfig(recOpts.ambiguity);

	if (init.estimate)
	{
		if	( found == +E_Source::REMOTE
			&&init.use_remote_sigma)
		{
			init.P = varAmb;
		}

		varAmb = -1;

		init.x = ambiguity;

		ambiguity = init.x;

		ambiguity_m = ambiguity * lambda;

		init.x	= ambiguity;
		init.P /= SQR(lambda);

		measEntry.addDsgnEntry(kfKey, lambda, init);
	}

	measEntry.addNoiseEntry(kfKey, 1, varAmb);

	measEntry.componentsMap[E_Component::PHASE_AMBIGUITY] = {ambiguity_m, "+ " + std::to_string(lambda) + ".N", varAmb};
}

inline static void pppRecPhasBias(COMMON_PPP_ARGS)
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

	E_Source found = kfState.getKFValue(kfKey, recPhasBias, &recPhasBiasVar);

	InitialState init = initialStateFromConfig(recOpts.phase_bias);

	if (init.estimate)
	{
		if	( found == +E_Source::REMOTE
			&&init.use_remote_sigma)
		{
			init.P = recPhasBiasVar;
		}

		init.x = recPhasBias;

		if (init.Q < 0)
		{
			init.P = recPhasBiasVar;
		}

		recPhasBiasVar = -1;

		recPhasBias = init.x;

		measEntry.addDsgnEntry(kfKey, 1, init);
	}

	measEntry.addNoiseEntry(kfKey, 1, recPhasBiasVar);

	measEntry.componentsMap[E_Component::REC_PHASE_BIAS] = {recPhasBias, "+ b_" + std::to_string(ft) + "r", recPhasBiasVar};
}

inline static void pppSatPhasBias(COMMON_PPP_ARGS)
{
	double	satPhasBias		=		satOpts.phaseBiasModel.default_bias;
	double	satPhasBiasVar	= SQR(	satOpts.phaseBiasModel.undefined_sigma);
	getBias(trace, time, Sat, Sat, sig.code, PHAS, satPhasBias, satPhasBiasVar);

	KFKey kfKey;
	kfKey.type		= KF::PHASE_BIAS;
	kfKey.Sat		= Sat;
	kfKey.num		= sig.code;
	kfKey.comment	= sigName;

	E_Source found = kfState.getKFValue(kfKey, satPhasBias, &satPhasBiasVar);

	InitialState init = initialStateFromConfig(satOpts.phase_bias);

	if (init.estimate)
	{
		if	( found == +E_Source::REMOTE
			&&init.use_remote_sigma)
		{
			init.P = satPhasBiasVar;
		}

		init.x = satPhasBias;

		if (init.Q < 0)
		{
			init.P = satPhasBiasVar;
		}

		satPhasBiasVar = -1;

		satPhasBias = init.x;

		measEntry.addDsgnEntry(kfKey, 1, init);
	}

	measEntry.addNoiseEntry(kfKey, 1, satPhasBiasVar);

	measEntry.componentsMap[E_Component::SAT_PHASE_BIAS] = {satPhasBias, "+ b_" + std::to_string(ft) + "s", satPhasBiasVar};
}

inline static void pppRecCodeBias(COMMON_PPP_ARGS)
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

	if (sysSat.sys == +E_Sys::GLO)
	{
		//Glonass has different frequencies per (pair of) sattelite(s), use separate bias for each (ignore pair because geph may not be available)
		kfKey.Sat	= obs.Sat;
	}

	E_Source found = kfState.getKFValue(kfKey, recCodeBias, &recCodeBiasVar);

	InitialState init = initialStateFromConfig(recOpts.code_bias, sig.code);

	if (init.estimate)
	{
		if	( found == +E_Source::REMOTE
			&&init.use_remote_sigma)
		{
			init.P = recCodeBiasVar;
		}

		if (recCodeBias != 0)
		{
			init.x = recCodeBias;
		}

		if (init.Q < 0)
		{
			init.P = recCodeBiasVar;
		}

		recCodeBiasVar = -1;

		if (Sat.sys == recOpts.receiver_reference_system)
		{
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
		}

		recCodeBias = init.x;

		measEntry.addDsgnEntry(kfKey, 1, init);
	}

	measEntry.addNoiseEntry(kfKey, 1, recCodeBiasVar);

	measEntry.componentsMap[E_Component::REC_CODE_BIAS] = {recCodeBias, "+ d_" + std::to_string(ft) + "r", recCodeBiasVar};
}

inline static void pppSatCodeBias(COMMON_PPP_ARGS)
{
	double	satCodeBias		=  		satOpts.codeBiasModel.default_bias;
	double	satCodeBiasVar	= SQR(	satOpts.codeBiasModel.undefined_sigma);
	getBias(trace, time, Sat, Sat, sig.code, CODE, satCodeBias, satCodeBiasVar);

	KFKey kfKey;
	kfKey.type		= KF::CODE_BIAS;
	kfKey.Sat		= Sat;
	kfKey.num		= sig.code;
	kfKey.comment	= sigName;

	E_Source found = kfState.getKFValue(kfKey, satCodeBias, &satCodeBiasVar);

	InitialState init = initialStateFromConfig(satOpts.code_bias);

	if (init.estimate)
	{
		if	( found == +E_Source::REMOTE
			&&init.use_remote_sigma)
		{
			init.P = satCodeBiasVar;
		}

		init.x = satCodeBias;

		if (init.Q < 0)
		{
			init.P = satCodeBiasVar;
		}

		satCodeBiasVar = -1;

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
	}

	measEntry.addNoiseEntry(kfKey, 1, satCodeBiasVar);

	measEntry.componentsMap[E_Component::SAT_CODE_BIAS] = {satCodeBias, "+ d_" + std::to_string(ft) + "s", satCodeBiasVar};
}

inline static void pppEopAdjustment(COMMON_PPP_ARGS)
{
	if (acsConfig.pppOpts.eop.estimate[0] == false)
	{
		return;
	}

	eopAdjustment(time, satStat.e, erpv, frameSwapper, rec, rRec, measEntry, kfState);
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

	test("eop",				acsConfig.pppOpts.eop.	estimate[0], recOpts.eop);
	test("rec_phase_bias",	recOpts.phase_bias.		estimate[0], recOpts.phaseBiasModel.enable);
	test("rec_code_bias",	recOpts.code_bias.		estimate[0], recOpts.codeBiasModel.	enable);
	test("sat_phase_bias",	satOpts.phase_bias.		estimate[0], satOpts.phaseBiasModel.enable);
	test("sat_code_bias",	satOpts.code_bias.		estimate[0], satOpts.codeBiasModel.	enable);
	test("emp_d_0",			satOpts.emp_d_0.		estimate[0], satOpts.empirical);

	if (acsConfig.minimise_sat_clock_offsets		&& nav.ephMap.empty())		{	BOOST_LOG_TRIVIAL(warning) << "Warning: `minimise_sat_clock_offsets` configured, but no broadcast ephemerides are available";	}
	if (acsConfig.minimise_sat_orbit_offsets		&& nav.ephMap.empty())		{	BOOST_LOG_TRIVIAL(warning) << "Warning: `minimise_sat_orbit_offsets` configured, but no broadcast ephemerides are available";	}
}












//redefine this to replace with nothing from now on - ie, use the argument name but not its type
#undef	COMMON_ARG
#define	COMMON_ARG(type)


void receiverUducGnss(
			Trace&				pppTrace,			///< Trace to output to
			Receiver&			rec,				///< Receiver to perform calculations for
	const	KFState&			kfState,			///< Kalman filter object containing the state parameters
			KFMeasEntryList&	kfMeasEntryList,	///< List to append kf measurements to
	const	KFState&			remoteKF)			///< Kalman filter object containing remote filter values
{
	DOCS_REFERENCE(UDUC_GNSS_Measurements__);

	auto trace		= getTraceFile(rec);
	auto jsonTrace	= getTraceFile(rec, true);

	tracepdeex(0, trace, "\n--------------------- Performing PPP -------------------");

	if	(  rec.obsList.empty()
		|| rec.invalid)
	{
		tracepdeex(1, trace, "\n\nReceiver not ready for PPP. Obs=%d", rec.obsList.size());

		return;
	}

	for (auto& obs : only<GObs>(rec.obsList))
	{
		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			continue;
		}

		auto& satOpts = acsConfig.getSatOpts(obs.Sat);

		satPosClk(trace, obs.time, obs, nav, satOpts.posModel.sources, satOpts.clockModel.sources, &kfState, &remoteKF, E_OffsetType::COM, E_Relativity::OFF);

		traceJson(1, jsonTrace, tsync,
		{
			{"data",	__FUNCTION__		},
			{"Sat",		obs.Sat.id()		},
			{"Rec",		obs.mount			}
		},
		{
			{"rSatCom[0]",	obs.rSatCom[0]},
			{"rSatCom[1]",	obs.rSatCom[1]},
			{"rSatCom[2]",	obs.rSatCom[2]},
			{"satClk",		obs.satClk}
		});
	}

	ERPValues erpv = getErp(nav.erp, tsync);

	FrameSwapper frameSwapper(tsync, erpv);

	for (auto&	obs				: only<GObs>(rec.obsList))
	for (auto&	[ft, sigList]	: obs.sigsLists)
	for (auto&	sig				: sigList)
	for (auto	measType		: {PHAS, CODE})
	{
		string sigName = sig.code._to_string();

		AutoSender autoSenderTemplate(jsonTrace, tsync);

		autoSenderTemplate.setBaseKVPs(0,
		{
			{"Sat",		obs.Sat.id()		},
			{"Rec",		obs.mount			},
			{"Sig",		sigName				},
			{"Type",	(long int) measType	}
		});

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
			||obs.ephClkValid == false)
		{
			tracepdeex(2,trace, "\n%s excludeSvh", obs.Sat.id().c_str());

			obs.excludeSVH = true;
		}

		if (obs.exclude)
		{
			auto& ast = autoSenderTemplate;

			if (acsConfig.exclude.bad_spp	&& obs.excludeBadSPP)			{	tracepdeex(5, trace, " - excludeBadSPP");		ast.pushValueKVP(2, {"exclude", "bad_spp"	});		continue;	}
			if (acsConfig.exclude.config	&& obs.excludeConfig)			{	tracepdeex(5, trace, " - excludeConfig");		ast.pushValueKVP(2, {"exclude", "config"	});		continue;	}
			if (acsConfig.exclude.eclipse	&& obs.excludeEclipse)			{	tracepdeex(5, trace, " - excludeEclipse");		ast.pushValueKVP(2, {"exclude", "eclipse"	});		continue;	}
			if (acsConfig.exclude.elevation	&& obs.excludeElevation)		{	tracepdeex(5, trace, " - excludeElevation");	ast.pushValueKVP(2, {"exclude", "elevation"	});		continue;	}
			if (acsConfig.exclude.outlier	&& obs.excludeOutlier)			{	tracepdeex(5, trace, " - excludeOutlier");		ast.pushValueKVP(2, {"exclude", "outlier"	});		continue;	}
			if (acsConfig.exclude.system	&& obs.excludeSystem)			{	tracepdeex(5, trace, " - excludeSystem");		ast.pushValueKVP(2, {"exclude", "system"	});		continue;	}
			if (acsConfig.exclude.svh		&& obs.excludeSVH)				{	tracepdeex(5, trace, " - excludeSVH");			ast.pushValueKVP(2, {"exclude", "svh"		});		continue;	}
		}

		SatNav&		satNav			= *obs.satNav_ptr;
		SatStat&	satStat			= *obs.satStat_ptr;
		SigStat&	sigStat			= satStat.sigStatMap[sigName];
		SigStat&	preprocSigStat	= satStat.sigStatMap[ft2string(ft)];

		if (preprocSigStat.slip.any)
		{
			auto& ast = autoSenderTemplate;

			if (acsConfig.exclude.LLI		&& preprocSigStat.slip.LLI)		{	tracepdeex(2, trace, " - LLI slip excluded");	ast.pushValueKVP(2, {"excludeSlip","LLI"	});		continue;	}
			if (acsConfig.exclude.GF		&& preprocSigStat.slip.GF)		{	tracepdeex(2, trace, " - GF slip excluded");	ast.pushValueKVP(2, {"excludeSlip","GF"		});		continue;	}
			if (acsConfig.exclude.MW		&& preprocSigStat.slip.MW)		{	tracepdeex(2, trace, " - MW slip excluded");	ast.pushValueKVP(2, {"excludeSlip","MW"		});		continue;	}
			if (acsConfig.exclude.SCDIA		&& preprocSigStat.slip.SCDIA)	{	tracepdeex(2, trace, " - SCDIA slip excluded");	ast.pushValueKVP(2, {"excludeSlip","SCDIA"	});		continue;	}
		}

		auto& satOpts = acsConfig.getSatOpts(obs.Sat,	{sigName});
		auto& recOpts = acsConfig.getRecOpts(rec.id,	{obs.Sat.sys._to_string(), sigName});

		checkModels(recOpts, satOpts);

		GTime time = obs.time;

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

		{
			measEntry.metaDataMap["receiverErrorCount"]	= &rec.receiverErrorCount;
			measEntry.metaDataMap["lastIonTime"]		= &satStat.lastIonTime;
		}

		if (measType == PHAS)
		{
			measEntry.metaDataMap["phaseRejectCount"]	= &sigStat.phaseRejectCount;
			measEntry.metaDataMap["lastPhaseTime"]		= &sigStat.lastPhaseTime;
			tracepdeex(2,trace,"\n PPP Phase count: %s %s %s %d", rec.id.c_str(), obs.Sat.id().c_str(), sig.code._to_string(), sigStat.phaseRejectCount);
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

		VectorEcef	rRec			= rec.aprioriPos;
		VectorEci	rRecInertial	= frameSwapper(rRec);

		vector<function<void(Vector3d, Vector3d)>>	delayedInits;

		Vector3d recPosVars		= -1 * Vector3d::Ones();
		Vector3d recOrbitVars	= -1 * Vector3d::Ones();
		Vector3d satOrbitVars	= -1 * Vector3d::Ones();

		//get keys early to declutter other code
		KFKey recPosKey;
							recPosKey.type		= KF::REC_POS;
							recPosKey.str		= rec.id;
							recPosKey.rec_ptr	= &rec;

		KFKey recVelKey;
							recVelKey.type		= KF::REC_POS_RATE;
							recVelKey.str		= rec.id;

		KFKey recOrbitPosKey;
							recOrbitPosKey.type	= KF::ORBIT;
		if (recSat.prn)		recOrbitPosKey.Sat	= recSat;
		else				recOrbitPosKey.str	= recSatId;

		KFKey satOrbitPosKey;
							satOrbitPosKey.type	= KF::ORBIT;
							satOrbitPosKey.Sat	= obs.Sat;

		KFKey satOrbitVelKey = satOrbitPosKey;

		KFKey recOrbitVelKey = recOrbitPosKey;

		//Linear receiver positions
		for (int i = 0; i < 3; i++)
		{
			InitialState posInit	= initialStateFromConfig(recOpts.pos,		i);
			InitialState velInit	= initialStateFromConfig(recOpts.pos_rate,	i);

			recPosKey.num		= i;
			recPosKey.comment	= posInit.comment;

			E_Source found = kfState.getKFValue(recPosKey, rRec[i], &recPosVars[i]);

			if (posInit.estimate == false)
			{
				continue;
			}

			if	( found == +E_Source::REMOTE
				&&posInit.use_remote_sigma)
			{
				posInit.P = recPosVars[i];
			}

			if (posInit.x == 0)		posInit.x = rRec[i];

			recPosVars[i] = -1;

			delayedInits.push_back([recPosKey, posInit, i, &measEntry]
				(Vector3d satStat_e, Vector3d eSatInertial)
			{
				measEntry.addDsgnEntry(recPosKey, -satStat_e[i], posInit);
			});

			if (velInit.estimate == false)
			{
				continue;
			}

			recVelKey.num		= i;
			recVelKey.comment	= velInit.comment;

			delayedInits.push_back([recPosKey, recVelKey, velInit, &kfState]
				(Vector3d satStat_e, Vector3d eSatInertial)
			{
				kfState.setKFTransRate(recPosKey, recVelKey,	1,	velInit);
			});
		}

		//Orbital receiver positions
		bool orbitalReceiver = false;
		for (int i = 0; i < 3; i++)
		{
			InitialState posInit = initialStateFromConfig(recOpts.orbit, i);
			InitialState velInit = initialStateFromConfig(recOpts.orbit, i + 3);

			recOrbitPosKey.num		= i;
			recOrbitPosKey.comment	= posInit.comment;

			E_Source found = kfState.getKFValue(recOrbitPosKey, rRecInertial[i], &recOrbitVars[i]);

			if (found)
			{
				orbitalReceiver = true;
			}

			if (posInit.estimate == false)
			{
				continue;
			}

			if	( found == +E_Source::REMOTE
				&&posInit.use_remote_sigma)
			{
				posInit.P = recOrbitVars[i];
			}

			if (posInit.x == 0)		posInit.x = rRecInertial[i];

			recOrbitVars[i] = -1;

			recOrbitVelKey.num		= i + 3;
			recOrbitVelKey.comment	= velInit.comment;

			delayedInits.push_back([recOrbitPosKey, posInit, i, &measEntry]
				(Vector3d satStat_e, Vector3d eSatInertial)
			{
				measEntry.addDsgnEntry(recOrbitPosKey, -eSatInertial[i], posInit);
			});

			kfState.addKFState(recOrbitVelKey, velInit);
		}

		if (orbitalReceiver)
		{
			rRec = frameSwapper(rRecInertial);
		}

		auto& pos = rec.pos;

		pos = ecef2pos(rRec);

		VectorEcef& rSat = obs.rSatCom;

		if (rSat.isZero())
		{
			BOOST_LOG_TRIVIAL(error) << "Error: Satpos is unexpectedly zero for " << rec.id << " - " << Sat.id();
			continue;
		}

		if (rRec.isZero())
		{
			BOOST_LOG_TRIVIAL(error) << "Error: rRec is unexpectedly zero for " << rec.id;
			continue;
		}

		if	( initialStateFromConfig(satOpts.orbit).estimate
			&&obs.rSatEci0.isZero())
		{
			//dont obliterate obs.rSat in satpos below, we still need the old one for next signal/phase
			SatPos satPos0 = obs;

			//use this to avoid adding the dt component of position
			satpos(trace, tsync, tsync, satPos0, satOpts.posModel.sources, E_OffsetType::COM, nav, &kfState, &remoteKF);

			obs.rSatEci0 = frameSwapper(satPos0.rSatCom, &satPos0.satVel, &obs.vSatEci0);
		}

		//Orbital satellite positions. This is mainly for setting up estimation, the positions estimated by this are already calculated elsewhere
		for (int i = 0; i < 3; i++)
		{
			InitialState posInit = initialStateFromConfig(satOpts.orbit, i);
			InitialState velInit = initialStateFromConfig(satOpts.orbit, i + 3);

			satOrbitPosKey.num		= i;
			satOrbitPosKey.comment	= posInit.comment;

			double dummyPos	= 0;

			E_Source found = kfState.getKFValue(satOrbitPosKey, dummyPos, &satOrbitVars[i]);

			if (posInit.estimate == false)
			{
				continue;
			}

			if	( found == +E_Source::REMOTE
				&&posInit.use_remote_sigma)
			{
				posInit.P = satOrbitVars[i];
			}

			if (posInit.x == 0)		posInit.x = obs.rSatEci0[i];
			if (velInit.x == 0)		velInit.x = obs.vSatEci0[i];

			satOrbitVars[i] = -1;

			satOrbitVelKey.num		= i + 3;
			satOrbitVelKey.comment	= velInit.comment;

			delayedInits.push_back([satOrbitPosKey, satOrbitVelKey, posInit, velInit, i, &obs, &measEntry]
				(Vector3d satStat_e, Vector3d eSatInertial)
			{
				measEntry.addDsgnEntry(satOrbitPosKey,	+eSatInertial[i],							posInit);
				measEntry.addDsgnEntry(satOrbitVelKey,	-eSatInertial[i] * (obs.tof + obs.satClk),	velInit);
			});
		}

		if (initialStateFromConfig(satOpts.orbit).estimate)		addEmpStates(satOpts, kfState, Sat);
		if (initialStateFromConfig(recOpts.orbit).estimate)		addEmpStates(recOpts, kfState, recSatId);

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

		//Range and geometry

		double rRecSat	= (rSat - rRec).norm();
		satStat.e		= (rSat - rRec).normalized();
		satStat.nadir	= acos(satStat.e.dot(rSat.normalized()));

		VectorEci eSatInertial = frameSwapper(satStat.e);

		satazel(pos, satStat.e, satStat);

		//add initialisations for things waiting for an up-to-date satstat
		for (auto& delayedInit : delayedInits)
		{
			delayedInit(satStat.e, eSatInertial);
		}

		//add 3d noise for unestimated positions
		for (int i = 0; i < 3; i++)
		{
			recPosKey.		num = i;
			recOrbitPosKey.	num = i;
			satOrbitPosKey.	num = i;

			measEntry.addNoiseEntry(recPosKey, 		1, SQR(satStat.e	[i]) * recPosVars	[i]);
			measEntry.addNoiseEntry(recOrbitPosKey,	1, SQR(eSatInertial	[i]) * recOrbitVars	[i]);
			measEntry.addNoiseEntry(satOrbitPosKey, 1, SQR(eSatInertial	[i]) * satOrbitVars	[i]);
		}


		tracepdeex(3, trace, "\n%s satstat.e  : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	satStat.e		.x(),	satStat.e		.y(),	satStat.e		.z());
		tracepdeex(3, trace, "\n%s apriori    : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	rec.aprioriPos	.x(),	rec.aprioriPos	.y(),	rec.aprioriPos	.z());
		tracepdeex(3, trace, "\n%s rSatEcef   : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.rSatCom		.x(),	obs.rSatCom		.y(),	obs.rSatCom		.z());
		tracepdeex(3, trace, "\n%s vSatEcef   : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.satVel		.x(),	obs.satVel		.y(),	obs.satVel		.z());
		tracepdeex(4, trace, "\n%s rSatEciDt  : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.rSatEciDt	.x(),	obs.rSatEciDt	.y(),	obs.rSatEciDt	.z());
		tracepdeex(4, trace, "\n%s rSatEci0   : %20.4f\t%20.4f\t%20.4f", obs.Sat.id().c_str(),	obs.rSatEci0	.x(),	obs.rSatEci0	.y(),	obs.rSatEci0	.z());

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
			if (recOpts.clockModel.enable)			{	pppRecClocks		(COMMON_PPP_ARGS);	}
			if (satOpts.clockModel.enable)			{	pppSatClocks		(COMMON_PPP_ARGS);	}
			if (recOpts.eccentricityModel.enable)	{	pppRecAntDelta		(COMMON_PPP_ARGS);	}
	// 		if (acsConfig.model.sat_ant_delta)		{	pppSatAntDelta		(COMMON_PPP_ARGS);	}
			if (recOpts.pcoModel.enable)			{	pppRecPCO			(COMMON_PPP_ARGS);	}
			if (satOpts.pcoModel.enable)			{	pppSatPCO			(COMMON_PPP_ARGS);	}
			if (recOpts.pcvModel.enable)			{	pppRecPCV			(COMMON_PPP_ARGS);	}
			if (satOpts.pcvModel.enable)			{	pppSatPCV			(COMMON_PPP_ARGS);	}
			if (recOpts.tideModels.enable)			{	pppTides			(COMMON_PPP_ARGS);	}
			if (recOpts.relativity)					{	pppRelativity		(COMMON_PPP_ARGS);	}
			if (recOpts.relativity2)				{	pppRelativity2		(COMMON_PPP_ARGS);	}
			if (recOpts.sagnac)						{	pppSagnac			(COMMON_PPP_ARGS);	}
			if (recOpts.ionospheric_component)		{	pppIonStec			(COMMON_PPP_ARGS);	}
			if (recOpts.ionospheric_component2)		{	pppIonStec2			(COMMON_PPP_ARGS);  }
			if (recOpts.ionospheric_component3)		{	pppIonStec3			(COMMON_PPP_ARGS);  }
			if (recOpts.ionospheric_model)			{	pppIonModel			(COMMON_PPP_ARGS);	}
			if (recOpts.tropModel.enable)			{	pppTrop				(COMMON_PPP_ARGS);	}
			if (recOpts.eop)						{	pppEopAdjustment	(COMMON_PPP_ARGS);  }
		}

		if (measType == PHAS)
		{
			if (recOpts.phaseWindupModel.enable)	{	pppRecPhaseWindup	(COMMON_PPP_ARGS);	}
			if (satOpts.phaseWindupModel.enable)	{	pppSatPhaseWindup	(COMMON_PPP_ARGS);	}
			if (recOpts.integer_ambiguity)			{	pppIntegerAmbiguity	(COMMON_PPP_ARGS);	}
			if (recOpts.phaseBiasModel.enable)		{	pppRecPhasBias		(COMMON_PPP_ARGS);	}
			if (satOpts.phaseBiasModel.enable)		{	pppSatPhasBias		(COMMON_PPP_ARGS);	}
		}

		if (measType == CODE)
		{
			if (recOpts.codeBiasModel.enable)		{	pppRecCodeBias		(COMMON_PPP_ARGS);	}
			if (satOpts.codeBiasModel.enable)		{	pppSatCodeBias		(COMMON_PPP_ARGS);	}
		}

		addNilDesignStates(recOpts.gyro_bias,			kfState,	KF::GYRO_BIAS,	3, recSatId);
		addNilDesignStates(recOpts.accelerometer_bias,	kfState,	KF::ACCL_BIAS,	3, recSatId);
		addNilDesignStates(recOpts.gyro_scale,			kfState,	KF::GYRO_SCALE,	3, recSatId);
		addNilDesignStates(recOpts.accelerometer_scale,	kfState,	KF::ACCL_SCALE,	3, recSatId);
		addNilDesignStates(recOpts.imu_offset,			kfState,	KF::IMU_OFFSET,	3, recSatId);

		//Calculate residuals and form up the measurement

		measEntry.componentsMap[E_Component::NET_RESIDUAL] = {0, "", 0};

		AutoSender autoSender = autoSenderTemplate;
		autoSender.pushBaseKVP	(0, {"data", __FUNCTION__});

		autoSender.pushValueKVP	(1, {"rRec[0]",	rRec[0]});
		autoSender.pushValueKVP	(1, {"rRec[1]",	rRec[1]});
		autoSender.pushValueKVP	(1, {"rRec[2]",	rRec[2]});
		autoSender.pushValueKVP	(1, {"El",		satStat.el});
		autoSender.pushValueKVP	(1, {"Az",		satStat.az});
		autoSender.pushValueKVP	(1, {"Nadir",	satStat.nadir});

		InteractiveTerminal ss(string("Chains/") + (string) measEntry.obsKey, trace);

		double residual = netResidualAndChainOutputs(ss, obs, measEntry);

		measEntry.setInnov(residual);

		// measEntry.metaDataMap["explain"]	= (void*) true;

		kfMeasEntryList.push_back(measEntry);
	}

	trace << "\n" << "\n";
}

