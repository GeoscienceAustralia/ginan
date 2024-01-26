
// #pragma GCC optimize ("O0")

#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "instrument.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "antenna.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "biases.hpp"
#include "gTime.hpp"
#include "tides.hpp"
#include "slr.hpp"

using std::cout;
using std::endl;

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
	COMMON_ARG(			Vector3d&			)	rRec,			\
	COMMON_ARG(			Vector3d&			)	rSat,			\
	COMMON_ARG(			double&				)	rRecSat,		\
	COMMON_ARG(			VectorPos&			)	pos,			\
	COMMON_ARG(			Receiver&			)	rec,			\
	COMMON_ARG(const	KFState&			)	kfState,		\
	COMMON_ARG(			KFMeasEntry&		)	measEntry


/** Satellite retroreflector delta from CoM
 */
inline void reflectorPCO(COMMON_PPP_ARGS)
{
	// satellite CoM to LRA offset correction
	Vector3d satReflectorCom = Vector3d::Zero();

	if (isSpherical(obs.satName))		satReflectorCom = satComOffSphere	(obs);
	else								satReflectorCom = satComOffGnss		(obs);

	double satReflectorDelta = satReflectorCom.dot(satStat.e);

	measEntry.componentsMap[E_Component::SAT_REFLECTOR_DELTA] = {satReflectorDelta * 2, "+ 2*satRefl", 0};
}

/** Tide delta
 */
inline void tideDelta(COMMON_PPP_ARGS)
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

	measEntry.componentsMap[E_Component::TIDES_SOLID	] = {-tideVectorSolid	.dot(satStat.e) * 2, "- 2*E.dT1", 0};
	measEntry.componentsMap[E_Component::TIDES_OTL		] = {-tideVectorOTL		.dot(satStat.e) * 2, "- 2*E.dT2", 0};
	measEntry.componentsMap[E_Component::TIDES_ATL		] = {-tideVectorOTL		.dot(satStat.e) * 2, "- 2*E.dT3", 0};
	measEntry.componentsMap[E_Component::TIDES_SPOLE	] = {-tideVectorSPole	.dot(satStat.e) * 2, "- 2*E.dT4", 0};
	measEntry.componentsMap[E_Component::TIDES_OPOLE	] = {-tideVectorOPole	.dot(satStat.e) * 2, "- 2*E.dT5", 0};
}

/** Relativity corrections
 */
inline void slrRelativity(COMMON_PPP_ARGS)
{
	// secondary relativity effect (Shapiro effect)
	double ln		= log(	(rSat.norm() + rRec.norm() + rRecSat)
					/ 		(rSat.norm() + rRec.norm() - rRecSat));
	double dtRel2 	= 2 * MU * ln / CLIGHT / CLIGHT / CLIGHT;

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
inline void slrTroposphere(COMMON_PPP_ARGS)
{
	double tropStates[3]	= {};
	double dTropDx[3]		= {};
	double varTrop			= 0;

	double troposphere_m = laserTropDelay(obs, pos, satStat.el);
	//calculate the trop values, variances, and gradients at the operating points

	InitialState init = initialStateFromConfig(recOpts.trop);

	if (init.estimate)
	{

		KFKey obsKey;
		obsKey.str	= obs.recName;
		obsKey.Sat	= obs.Sat;
		obsKey.type	= KF::TROP;

		measEntry.addNoiseEntry(obsKey, 1, varTrop);	//todo aaron, needs iteration, gradients

		varTrop = -1;
	}

	measEntry.componentsMap[E_Component::TROPOSPHERE] = {troposphere_m * 2, "+ 2*" + std::to_string(dTropDx[0]) + ".T", varTrop};
}

inline void slrRecAntDelta(COMMON_PPP_ARGS)
{
	Vector3d recAntVector = body2ecef(rec.attStatus, rec.antDelta);

	double recAntDelta = -recAntVector.dot(satStat.e);

	measEntry.componentsMap[E_Component::REC_ANTENNA_DELTA] = {recAntDelta * 2, "+ 2*E.dR_r", 0};
};

/** Rec range bias
 */
inline void recRangeBias(COMMON_PPP_ARGS)
{
	double	recRangeBias = 0;
	double	recRangeBiasVar	= DEFAULT_RANG_BIAS_VAR;

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

		kfState.getKFValue(kfKey, recRangeBias);

		measEntry.addDsgnEntry(kfKey, 1, init);

		recRangeBiasVar = -1;
	}

	measEntry.componentsMap[E_Component::REC_RANGE_BIAS] = {recRangeBias, "+ recRangeBias", recRangeBiasVar};
}

/** Rec time bias
 */
inline void recTimeBias(COMMON_PPP_ARGS)
{
//			VectorXd recTimeBiasPartial = slrObs.satVel.transpose() * slrObs.e * 0.001; //ms
	double recTimeBias			= 0;
	double recTimeBiasVar		= DEFAULT_TIME_BIAS_VAR;
	double recTimeBiasPartial = obs.satVel.transpose() * satStat.e; //sec

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

		kfState.getKFValue(kfKey, recTimeBias);

		measEntry.addDsgnEntry(kfKey, recTimeBiasPartial, init);

		recTimeBiasVar = -1;
	}

	measEntry.componentsMap[E_Component::REC_TIME_BIAS] = {recTimeBias, "+ recTimeBias", recTimeBiasVar};
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
	// 	Instrument	instrument(__FUNCTION__ + rec.id);

	if (acsConfig.slrOpts.process_slr == false)
	{
		return;
	}

	auto trace = getTraceFile(rec);

	GTime time = rec.obsList.front()->time;

	// Prepare apriori's
	for (auto& obs : only<LObs>(rec.obsList))
	{
		applyBiases			(obs);		//todo aaron, components?
		getRecPosApriori	(obs, rec);
	}

	satPossSlr(trace, rec.obsList, nav, {E_Source::PRECISE}, E_OffsetType::COM, E_Relativity::OFF, &kfState);

	ERPValues erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapper(time, erpv);

	for (auto& obs : only<LObs>(rec.obsList))
	{
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
			obsKey.str		= obs.recName;
			obsKey.Sat		= obs.Sat;
			obsKey.comment	= measEntry.obsKey.comment;

			double var = SQR(recOpts.laser_sigma);

			measEntry.addNoiseEntry(obsKey, 1, var);

			measEntry.componentsMap[E_Component::OBSERVED] = {-observed, "- obs", var};
		}

		if (acsConfig.output_residual_chain)
		{
			tracepdeex(0, trace, "\n----------------------------------------------------");
			tracepdeex(0, trace, "\nMeasurement for %s", ((string) measEntry.obsKey).c_str());
		}

		//Calculate the basic range
		geodist(obs.rSatCom, rec.aprioriPos, satStat.e);
		Vector3d rRec = rec.aprioriPos;
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

					kfState.getKFValue(kfKey, rRec[i]);

					init.x = rRec[i];

					measEntry.addDsgnEntry(kfKey, -satStat.e[i] * 2, init);
				}
			}
		}


		auto& pos = rec.pos;

		pos = ecef2pos(rRec);

		satazel(pos, satStat.e, satStat);

		if (satStat.el < recOpts.elevation_mask_deg * D2R)
		{
			obs.excludeElevation = true;
			return;
		}

		bool satNoise = true;
		Vector3d rSat = Vector3d::Zero();
		{
			//initialise using ephemeris values if available
			rSat					= obs.rSatCom;
			if (satOpts.pos.estimate[0])
			{
				VectorEci vSatInertial;
				VectorEci rSatInertial			= frameSwapper(rSat, &obs.satVel, &vSatInertial);
				VectorEci lookVectorInertial	= frameSwapper(satStat.e);

				KFKey satPosKeys[3];
				KFKey satVelKeys[3];
				for (int i = 0; i < 3; i++)
				{
					satPosKeys[i].type	= KF::ORBIT;
					satPosKeys[i].Sat	= Sat;
					satPosKeys[i].num	= i;

					satVelKeys[i].type	= KF::ORBIT;
					satVelKeys[i].Sat	= Sat;
					satVelKeys[i].num	= i + 3;
				}

				for (int i = 0; i < 3; i++)
				{
					InitialState init = initialStateFromConfig(satOpts.pos,			i);

					if (init.estimate)
					{
						if (init.Q < 0)
						{
							init.P = obs.ephVar;
						}

						bool found = kfState.getKFValue(satPosKeys[i], rSatInertial[i]);
						if (found)
						{
							satNoise = false;
						}

						init.x = rSatInertial[i];

						measEntry.addDsgnEntry(satPosKeys[i], lookVectorInertial[i], init);
					}
				}

				for (int i = 0; i < 3; i++)
				{
					InitialState init = initialStateFromConfig(satOpts.pos_rate,	i);

					if (init.estimate)
					{
						kfState.getKFValue(satVelKeys[i], vSatInertial[i]);

						init.x = vSatInertial[i];

						kfState.addKFState(satVelKeys[i], init);
					}
				}

				//refer actual time to estimated time
				//add offset from orbital motion
				double flightTime	= obs.twoWayTimeOfFlight / 2;

				rSatInertial -= flightTime * vSatInertial;		//todo aaron, use kalman orbit thing
			}
			else
			{
				KFKey obsKey;
				obsKey.type	= KF::ORBIT;
				obsKey.Sat	= obs.Sat;
// 				obsKey.num	= i;

				measEntry.addNoiseEntry(obsKey, 1, obs.ephVar);		//todo aaron, need many more noise entries
			}
		}

		//Range
		double rRecSat = (rSat - rRec).norm();
		{
			measEntry.componentsMap[E_Component::RANGE] = {rRecSat * 2, "+ range", 0};
		}

		//Add modelled adjustments and estimated parameter

		slrRecAntDelta		(COMMON_PPP_ARGS);
		reflectorPCO		(COMMON_PPP_ARGS);
		tideDelta			(COMMON_PPP_ARGS);
		slrRelativity		(COMMON_PPP_ARGS);
		slrSagnac			(COMMON_PPP_ARGS);
		slrTroposphere		(COMMON_PPP_ARGS);
		recRangeBias		(COMMON_PPP_ARGS);
		recTimeBias			(COMMON_PPP_ARGS);
		//todo aaron replace eops

		//Calculate residuals and form up the measurement

		measEntry.componentsMap[E_Component::NET_RESIDUAL] = {0, "", 0};

		double residual = 0;
		for (auto& [component, details] : measEntry.componentsMap)
		{
			auto& [componentVal, eq, var] = details;

			residual -= componentVal;

			if (acsConfig.output_residual_chain)
			{
				tracepdeex(0, trace, "\n");
				tracepdeex(4, trace, "%s",		time.to_string());
				tracepdeex(3, trace, "%30s",	((string)measEntry.obsKey).c_str());
				tracepdeex(0, trace, " %-20s %+14.4f -> %13.4f", component._to_string(), -componentVal, residual);

				if (var >= 0)		tracepdeex(0, trace, " ± %5e", var);
				else				tracepdeex(0, trace, " Estimated");
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

		measEntry.setInnov(residual);

		kfMeasEntryList.push_back(measEntry);
	}

	trace << std::endl << std::endl;
}




// check using same frame, check using ecef velocities
