
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

/** Satellite Laser Ranging.
 *
 *
 */
ParallelArchitecture SLR_Mesaurements__()
{

}

#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "tropModels.hpp"
#include "acsConfig.hpp"
#include "orbitProp.hpp"
#include "receiver.hpp"
#include "antenna.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "biases.hpp"
#include "gTime.hpp"
#include "tides.hpp"
#include "ppp.hpp"
#include "slr.hpp"

#include <functional>

using std::function;

#define DEFAULT_RANG_BIAS_VAR SQR(0.001)
#define DEFAULT_TIME_BIAS_VAR SQR(0.001)




//this hideousness is a horrible hack to make this code compile on macs.
//blame apple for this. look what they've made me do...

//this will ust copy and paste the type for now, this is undefined later to remove the type names to pass in the parameters in the same order
#define COMMON_ARG(type)    type

#define COMMON_PPP_ARGS											\
	COMMON_ARG(			Trace&				)	trace,			\
	COMMON_ARG(			LObs&				)	obs,			\
	COMMON_ARG(			GTime&				)	time,			\
	COMMON_ARG(			SatStat&			)	satStat,		\
	COMMON_ARG(			ReceiverOptions&	)	recOpts,		\
	COMMON_ARG(			SatelliteOptions&	)	satOpts,		\
	COMMON_ARG(			SatNav&				)	satNav,			\
	COMMON_ARG(			VectorEcef&			)	rRec,			\
	COMMON_ARG(			VectorEcef&			)	rSat,			\
	COMMON_ARG(			double&				)	rRecSat,		\
	COMMON_ARG(			VectorPos&			)	pos,			\
	COMMON_ARG(			Receiver&			)	rec,			\
	COMMON_ARG(const	KFState&			)	kfState,		\
	COMMON_ARG(			ERPValues&			)	erpv,			\
	COMMON_ARG(			FrameSwapper&		)	frameSwapper,	\
	COMMON_ARG(			KFMeasEntry&		)	measEntry


/** Satellite retroreflector delta from CoM
 */
inline void slrReflectorPCO(COMMON_PPP_ARGS)
{
	// satellite CoM to LRA offset correction
	Vector3d satReflectorCom = Vector3d::Zero();

	if (isSpherical(obs.satName))		satReflectorCom = satComOffSphere	(obs) * satStat.e * -1;
	else								satReflectorCom = satComOffGnss		(obs);

	double satReflectorDelta = satReflectorCom.dot(satStat.e);

	measEntry.componentsMap[E_Component::SAT_REFLECTOR_DELTA] = {satReflectorDelta * 2, "+ 2*satRefl", 0};
}

/** Tide delta
 */
inline void slrTides(COMMON_PPP_ARGS)
{
	auto [solid, otl, atl, spole, opole] = tideDelta(trace, time, rec, rRec, recOpts);

	measEntry.componentsMap[E_Component::TIDES_SOLID	] = {-2 * satStat.e.dot(solid),	"- 2*E.dT1", 0};
	measEntry.componentsMap[E_Component::TIDES_OTL		] = {-2 * satStat.e.dot(otl),	"- 2*E.dT2", 0};
	measEntry.componentsMap[E_Component::TIDES_ATL		] = {-2 * satStat.e.dot(atl),	"- 2*E.dT3", 0};
	measEntry.componentsMap[E_Component::TIDES_SPOLE	] = {-2 * satStat.e.dot(spole),	"- 2*E.dT4", 0};
	measEntry.componentsMap[E_Component::TIDES_OPOLE	] = {-2 * satStat.e.dot(opole),	"- 2*E.dT5", 0};
}

/** Relativity corrections
 */
inline void slrRelativity2(COMMON_PPP_ARGS)
{
	// secondary relativity effect (Shapiro effect)
	double dtRel2 	= relativity2(rSat, rRec);

	measEntry.componentsMap[E_Component::RELATIVITY2] = {dtRel2	* CLIGHT * 2, "+ rel2", 0};
}

/** Sagnac effect
 */
inline void slrSagnac(COMMON_PPP_ARGS)
{
	double dSagnacOut	= sagnac(rSat, rRec);
	double dSagnacIn	= sagnac(rRec, rSat);	//todo aaron, is it that simple? look at area

	measEntry.componentsMap[E_Component::SAGNAC] = {dSagnacOut + dSagnacIn, "+ sag", 0};
}

/** Tropospheric delay
 */
inline void slrTrop(COMMON_PPP_ARGS)
{
	TropStates	tropStates;
	TropMapping	dTropDx;
	double		varTrop	= 0;

	//calculate the trop values, variances, and gradients at the operating points
	double troposphere_m = laserTropDelay(obs, pos, satStat, tropStates, dTropDx, varTrop);

	InitialState init = initialStateFromConfig(recOpts.trop);

	if (init.estimate)
	{
		KFKey obsKey;
		obsKey.type	= KF::TROP;
		obsKey.str	= obs.recName;
		obsKey.Sat	= obs.Sat;

		measEntry.addNoiseEntry(obsKey, 1, varTrop);

		varTrop = -1;
	}

	measEntry.componentsMap[E_Component::TROPOSPHERE] = {troposphere_m * 2, "+ 2*" + std::to_string(dTropDx.dryMap) + ".T", varTrop};
}

inline void slrRecAntDelta(COMMON_PPP_ARGS)
{
	Vector3d recAntVector = body2ecef(rec.attStatus, rec.antDelta);

	double recAntDelta = -recAntVector.dot(satStat.e);

	measEntry.componentsMap[E_Component::REC_ANTENNA_DELTA] = {recAntDelta * 2, "- 2*E.dR_r", 0};
};

/** Rec range bias
 */
inline void slrRecRangeBias(COMMON_PPP_ARGS)
{
	double	recRangeBias = obs.rangeBias;
	double	recRangeBiasVar	= DEFAULT_RANG_BIAS_VAR;	// todo Eugene: use actual var?

	InitialState init	= initialStateFromConfig(recOpts.slr_range_bias);

	if (init.estimate)
	{
		if (init.Q < 0)
		{
			init.P = recRangeBiasVar;
		}

		KFKey kfKey;
		kfKey.type	= KF::SLR_REC_RANGE_BIAS;
		kfKey.str	= obs.recName;

		init.x = recRangeBias;

		kfState.getKFValue(kfKey, init.x);

		recRangeBias = init.x;

		measEntry.addDsgnEntry(kfKey, 1, init);

		recRangeBiasVar = -1;
	}

	measEntry.componentsMap[E_Component::REC_RANGE_BIAS] = {recRangeBias, "+ recRangeBias", recRangeBiasVar};
}

/** Rec time bias
 */
inline void slrRecTimeBias(COMMON_PPP_ARGS)
{
//			VectorXd recTimeBiasPartial = slrObs.satVel.transpose() * slrObs.e * 0.001; //ms
	double recTimeBias			= obs.timeBias * CLIGHT;
	double recTimeBiasVar		= DEFAULT_TIME_BIAS_VAR;	// todo Eugene: use actual var?
	double recTimeBiasPartial	= -obs.satVel.dot(satStat.e) / CLIGHT;

	InitialState init	= initialStateFromConfig(recOpts.slr_time_bias);

	if (init.estimate)
	{
		if (init.Q < 0)
		{
			init.P = recTimeBiasVar;
		}

		KFKey kfKey;
		kfKey.type	= KF::SLR_REC_TIME_BIAS;
		kfKey.str	= obs.recName;

		init.x = recTimeBias;

		kfState.getKFValue(kfKey, init.x);

		recTimeBias = init.x;

		measEntry.addDsgnEntry(kfKey, recTimeBiasPartial, init);

		recTimeBiasVar = -1;
	}

	measEntry.componentsMap[E_Component::REC_TIME_BIAS] = {recTimeBiasPartial * recTimeBias * 2, "+ 2*recTimeBiasPartial*recTimeBias", recTimeBiasVar};
}

inline void slrEopAdjustment(COMMON_PPP_ARGS)
{
	if (acsConfig.pppOpts.eop.estimate[0] == false)
	{
		return;
	}

	VectorEcef e2 = satStat.e * 2; // eopPartials * 2 & adjustment * 2

	eopAdjustment(time, e2, erpv, frameSwapper, rec, rRec, measEntry, kfState);
}












//redefine this to replace with nothing from now on - ie, use the argument name but not its type
#undef	COMMON_ARG
#define	COMMON_ARG(type)

void receiverSlr(
			Trace&				pppTrace,			///< Trace to output to
			Receiver&			rec,				///< Receiver to perform calculations for
	const	KFState&			kfState,			///< Kalman filter object containing the network state parameters
			KFMeasEntryList&	kfMeasEntryList)	///< List to append kf measurements to
{
	DOCS_REFERENCE(SLR_Mesaurements__);

	if (acsConfig.slrOpts.process_slr == false)
	{
		return;
	}

	auto trace = getTraceFile(rec);

	tracepdeex(0, trace, "\n--------------------- Processing SLR -------------------");

	if	(  rec.obsList.empty()
		|| rec.invalid)
	{
		tracepdeex(1, trace, "\n\nReceiver not ready for SLR. Obs=%d", rec.obsList.size());	return;
	}

	ERPValues erpv = getErp(nav.erp, tsync);
	FrameSwapper frameSwapper(tsync, erpv);

	int obsNum = 0;
	for (auto& obs : only<LObs>(rec.obsList))
	{
		updateSlrRecBiases(obs);

		if (obs.exclude)
		{
			continue;
		}

		SatNav&		satNav	= *obs.satNav_ptr;
		SatStat&	satStat	= *obs.satStat_ptr;

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

		GTime time = obs.time;

		satpos(trace, time, time, obs, satOpts.posModel.sources, E_OffsetType::COM, nav, &kfState);

		if (obs.ephPosValid == false)
			BOOST_LOG_TRIVIAL(warning) << "Warning: Invalid position for " << obs.Sat.id() << " in " << __FUNCTION__;

		auto Sat = obs.Sat;
		auto sys = Sat.sys;

		double observed = obs.twoWayTimeOfFlight * CLIGHT;

		if (observed == 0)
		{
			continue;
		}

		KFMeasEntry measEntry(&kfState);

		measEntry.metaDataMap["obs_ptr"]	= &obs;

		measEntry.obsKey.Sat	= obs.Sat;
		measEntry.obsKey.str	= rec.id;
		measEntry.obsKey.type	= KF::RANGE;

		//Start with the observed measurement and its noise
		{
			KFKey obsKey;
			obsKey.type		= KF::LASER_MEAS;
			obsKey.str		= obs.recName;
			obsKey.Sat		= obs.Sat;
			obsKey.num		= obsNum;
			obsKey.comment	= measEntry.obsKey.comment;

			double var = SQR(recOpts.laser_sigma);

			measEntry.addNoiseEntry(obsKey, 1, var);

			measEntry.componentsMap[E_Component::OBSERVED] = {-observed, "- obs", var};
		}

		//Calculate the basic range

		VectorEcef rRec = rec.aprioriPos;

		vector<function<void(Vector3d, Vector3d)>>	delayedInits;

		{
			for (int i = 0; i < 3; i++)
			{
				InitialState init = initialStateFromConfig(recOpts.pos, i);

				if (init.estimate)
				{
					KFKey kfKey;
					kfKey.type	= KF::REC_POS;
					kfKey.str	= obs.recName;
					kfKey.num	= i;

					init.x = rRec[i];

					kfState.getKFValue(kfKey, init.x);

					rRec[i] = init.x;

					delayedInits.push_back([kfKey, init, i, &measEntry]
						(Vector3d satStat_e, Vector3d eSatInertial)
					{
						measEntry.addDsgnEntry(kfKey, -satStat_e[i] * 2, init);
					});

					// measEntry.addDsgnEntry(kfKey, -satStat.e[i] * 2, init);
				}
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
		{
			double tgap = (time - obs.timeBias - tsync).to_double();

			if (obs.rSatEci0.isZero())
			{
				//dont obliterate obs.rSat below, we dont need it, but maintain consistency with ppp_obs.cpp
				SatPos satPos0 = obs;

				//use this to avoid adding the dt component of position
				bool pass = satpos(trace, tsync, tsync, satPos0, satOpts.posModel.sources, E_OffsetType::COM, nav, &kfState);

				obs.rSatEci0 = frameSwapper(satPos0.rSatCom, &satPos0.satVel, &obs.vSatEci0);
			}

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

				if (posInit.x == 0)			posInit.x = obs.rSatEci0[i];
				if (velInit.x == 0)			velInit.x = obs.vSatEci0[i];

				delayedInits.push_back([posKey, velKey, posInit, velInit, i, &measEntry, tgap]
					(Vector3d satStat_e, Vector3d eSatInertial)
				{
					measEntry.addDsgnEntry(posKey,	+eSatInertial[i]		* 2,	posInit);
					measEntry.addDsgnEntry(velKey,	+eSatInertial[i] * tgap	* 2,	velInit);		//todo aaron, eugene copied this from ppp_obs, but i think it is not necessary (bad?)
				});

				// measEntry.addDsgnEntry(posKey,	+eSatInertial[i]		* 2,	posInit);
			}

			addEmpStates(satOpts, kfState, Sat);
		}
		else
		{
			KFKey obsKey;
			obsKey.type	= KF::ORBIT;
			obsKey.Sat	= obs.Sat;
// 			obsKey.num	= i;

			measEntry.addNoiseEntry(obsKey, 1, obs.ephVar);		//todo aaron, need many more noise entries
		}


		//Range and geometry

		double rRecSat	= (rSat - rRec).norm();
		satStat.e		= (rSat - rRec).normalized();

		VectorEci eSatInertial	= frameSwapper(satStat.e);

		satazel(pos, satStat.e, satStat);

		if (satStat.el < recOpts.elevation_mask_deg * D2R)
		{
			obs.excludeElevation = true;
			continue;
		}

		//add initialisations for things waiting for an up-to-date satstat
		for (auto& delayedInit : delayedInits)
		{
			delayedInit(satStat.e, eSatInertial);
		}

		if (recOpts.range)
		{
			measEntry.componentsMap[E_Component::RANGE] = {rRecSat * 2, "+ range", 0};
		}

		if (acsConfig.output_residual_chain)
		{
			tracepdeex(0, trace, "\n----------------------------------------------------");
			tracepdeex(0, trace, "\nMeasurement for %s", ((string) measEntry.obsKey).c_str());
		}

		//Add modelled adjustments and estimated parameter
		{
			slrRecAntDelta		(COMMON_PPP_ARGS);
			slrReflectorPCO		(COMMON_PPP_ARGS);
			slrTides			(COMMON_PPP_ARGS);
			slrRelativity2		(COMMON_PPP_ARGS);
			slrSagnac			(COMMON_PPP_ARGS);
			slrTrop				(COMMON_PPP_ARGS);
			slrRecRangeBias		(COMMON_PPP_ARGS);
			slrRecTimeBias		(COMMON_PPP_ARGS);
			slrEopAdjustment	(COMMON_PPP_ARGS);
		}

		if (obs.exclude)	// slrReflectorPCO() may have observations excluded
		{
			continue;
		}

		//Calculate residuals and form up the measurement

		measEntry.componentsMap[E_Component::NET_RESIDUAL] = {0, "", 0};

		double residual = netResidualAndChainOutputs(trace, obs, measEntry);

		measEntry.setInnov(residual);

		kfMeasEntryList.push_back(measEntry);

		obsNum++;
	}

	trace << "\n" << "\n";
}

