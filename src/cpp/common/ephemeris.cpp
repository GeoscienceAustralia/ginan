
// #pragma GCC optimize ("O0")

#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "ephPrecise.hpp"
#include "mongoRead.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "ephemeris.hpp"
#include "testUtils.hpp"
#include "algebra.hpp"
#include "orbits.hpp"
#include "satSys.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "enums.h"
#include "ssr.hpp"

/** URA SSR by variance
*/
double ephVarToUra(
	double ephVar)
{
	// to be implemented

	return ephVar;
}

double relativity1(
	Vector3d& rSat,
	Vector3d& satVel)
{
	return 2 * rSat.dot(satVel) / CLIGHT / CLIGHT;
}

template<typename TYPE>
void cullEphMap(
	GTime	time,
	TYPE&	map)
{
	for (auto& [satid, satEphMap] : map)
	{
		SatSys Sat;
		Sat.fromHash(satid);

		double tmax;
		switch (Sat.sys)
		{
			case E_Sys::QZS:	tmax = MAXDTOE_QZS	+ 1; break;
			case E_Sys::GAL:	tmax = MAXDTOE_GAL	+ 1; break;
			case E_Sys::BDS:	tmax = MAXDTOE_CMP	+ 1; break;
			default: 			tmax = MAXDTOE		+ 1; break;
		}

		for (auto& [navtyp, navMap] : satEphMap)
		for (auto it = navMap.begin(); it != navMap.end(); )
		{
			auto& [ephtime, eph] = *it;

			if (ephtime < time - tmax)
			{
				it = navMap.erase(it);
			}
			else
			{
				++it;
			}
		}
	}
}

void cullOldEphs(
	GTime	time)
{
	cullEphMap(time, nav.ephMap);
	cullEphMap(time, nav.gephMap);
	cullEphMap(time, nav.sephMap);
	cullEphMap(time, nav.cephMap);
}

bool satclk(
	Trace&				trace,
	GTime				time,
	GTime				teph,
	SatPos&				satPos,
	vector<E_Source>	ephTypes,
	Navigation&			nav,
	const KFState*		kfState_ptr,
	const KFState*		remote_ptr)
{
	satPos.ephClkValid = false;

	bool returnValue = false;

	for (auto& ephType : ephTypes)
	{
		tracepdeex(4, trace, "\n%-10s: time=%s sat=%s ephType=%d", __FUNCTION__, time.to_string().c_str(), satPos.Sat.id().c_str(), ephType);

		switch (ephType)
		{
			case E_Source::SSR:			//fallthrough
 			case E_Source::BROADCAST:	returnValue = satClkBroadcast	(trace, time, teph,		satPos, nav			);	break;
			case E_Source::PRECISE:		returnValue = satClkPrecise		(trace, time, 			satPos,	nav			);	break;
			case E_Source::KALMAN:		returnValue = satClkKalman		(trace, time, 			satPos,	kfState_ptr	);	break;
			case E_Source::REMOTE:		returnValue = satClkKalman		(trace, time, 			satPos,	remote_ptr	);	break;
			default:					continue;
		}

		if (returnValue == false)
		{
			continue;
		}

		satPos.clkSource	= ephType;
		satPos.ephClkValid	= true;

		if	(acsConfig.check_broadcast_differences
			&&ephType != +E_Source::BROADCAST)
		{
			SatPos copy = satPos;

			bool pass = satClkBroadcast(trace, time, teph, copy, nav);
			double delta = (copy.satClk - satPos.satClk) * CLIGHT;
			if	( pass
				&&fabs(delta) > 30)
			{
				BOOST_LOG_TRIVIAL(warning) << "Warning, clock for " << satPos.Sat.id() << " is " << delta << " from broadcast";
			}
		}

		break;
	}

	return returnValue;
}

/** compute satellite position and clock
* satellite clock does not include code bias correction (tgd or bgd)
*/
bool satpos(
	Trace&				trace,				///< Trace to output to
	GTime				time,				///< time (gpst)
	GTime				teph,				///< time to select ephemeris (gpst)
	SatPos&				satPos,				///< Data required for determining and storing satellite positions/clocks
	vector<E_Source>	ephTypes,			///< Source of ephemeris
	E_OffsetType		offsetType,			///< Type of antenna offset to apply
	Navigation&			nav,				///< navigation data
	const KFState*		kfState_ptr,		///< Optional pointer to a kalman filter to take values from
	const KFState*		remote_ptr)			///< Optional pointer to a kalman filter to take values from
{
	double	antennaScalar	= 0;
	bool	returnValue		= false;

	for (auto& ephType : ephTypes)
	{
		tracepdeex(4, trace, "\n%-10s: time=%s sat=%s ephType=%s offsetType=%d", __FUNCTION__, time.to_string().c_str(), satPos.Sat.id().c_str(), ephType._to_string(), offsetType);

		if (returnValue == false)
		switch (ephType)
		{
			case E_Source::BROADCAST:	returnValue = satPosBroadcast	(trace, time, teph,		satPos, nav			);	break;
			case E_Source::SSR:			returnValue = satPosSSR			(trace, time, teph,		satPos, nav			);	break;
			case E_Source::PRECISE:		returnValue = satPosPrecise		(trace, time, 			satPos, nav			);	break;
			case E_Source::KALMAN:		returnValue = satPosKalman		(trace, time, 			satPos,	kfState_ptr	);	break;
			case E_Source::REMOTE:		returnValue = satPosKalman		(trace, time, 			satPos,	remote_ptr	);	break;
			case E_Source::CONFIG:		continue;
			default:					continue;
		}

		if (returnValue == false)
		{
			continue;
		}

		switch (ephType)
		{
			case E_Source::BROADCAST:	satPos.rSatCom = satPos.rSatApc;	break;
			case E_Source::SSR:			satPos.rSatApc = satPos.rSatCom;	break;
			case E_Source::PRECISE:		satPos.rSatApc = satPos.rSatCom;	break;
			case E_Source::KALMAN:		satPos.rSatApc = satPos.rSatCom;	break;
			case E_Source::REMOTE:		satPos.rSatApc = satPos.rSatCom;	break;
		}

		if	( acsConfig.check_broadcast_differences
			&&ephType != +E_Source::BROADCAST)
		{
			SatPos copy = satPos;
			bool pass = satPosBroadcast(trace, time, teph, copy, nav);
			double delta = (satPos.rSatApc - copy.rSatApc).norm();
			if	( pass
				&&delta > 10)
			{
				BOOST_LOG_TRIVIAL(warning) << "Warning, orbit for " << satPos.Sat.id() << " is " << delta << " from broadcast";
			}
		}

		tracepdeex(4, trace, " - FOUND");

		satPos.posTime		= time;
		satPos.posSource	= ephType;
		satPos.ephPosValid	= true;

		if (ephType == +E_Source::SSR)
		{
			satPos.clkSource	= ephType;
			satPos.ephClkValid	= true;
		}

		break;
	}

	if (satPos.posSource == +E_Source::SSR												&& acsConfig.ssr_input_antenna_offset == +E_OffsetType::UNSPECIFIED)
		BOOST_LOG_TRIVIAL(error) << "Error: ssr_antenna_offset has not been set in config.\n";

	if (satPos.posSource == +E_Source::SSR			&& offsetType == +E_OffsetType::APC	&& acsConfig.ssr_input_antenna_offset == +E_OffsetType::COM)		antennaScalar = +1;
	if (satPos.posSource == +E_Source::SSR			&& offsetType == +E_OffsetType::COM	&& acsConfig.ssr_input_antenna_offset == +E_OffsetType::APC)		antennaScalar = -1;
	if (satPos.posSource == +E_Source::PRECISE		&& offsetType == +E_OffsetType::APC)																	antennaScalar = +1;
	if (satPos.posSource == +E_Source::KALMAN		&& offsetType == +E_OffsetType::APC)																	antennaScalar = +1;
	if (satPos.posSource == +E_Source::REMOTE		&& offsetType == +E_OffsetType::APC)																	antennaScalar = +1;
	if (satPos.posSource == +E_Source::BROADCAST	&& offsetType == +E_OffsetType::COM)																	antennaScalar = -1;

	// satellite antenna offset correction
	if (antennaScalar)
	{
		if (satPos.satNav_ptr == nullptr)
		{
			BOOST_LOG_TRIVIAL(debug) << "Sat nav pointer undefined";
			return returnValue;
		}

		auto& attStatus = satPos.satNav_ptr->attStatus;

		if	( attStatus.eXBody.isZero()
			||attStatus.eYBody.isZero()
			||attStatus.eZBody.isZero())
		{
			BOOST_LOG_TRIVIAL(debug) << "Satellite attitude of " << satPos.Sat.id() << " not available, antenna offset not corrected.";
			return returnValue;
		}

		E_FType j;
		E_FType k;
		E_FType l;
		E_Sys sys = satPos.Sat.sys;
		if (!satFreqs(sys,j,k,l))
			return false;

		if	( satPos.satNav_ptr->lamMap.empty()
			||satPos.satNav_ptr->lamMap[j] == 0
			||satPos.satNav_ptr->lamMap[k] == 0)
		{
			// satAntOff() requries lamMap
			updateLamMap(time, satPos);
		}

		Vector3d dAnt = Vector3d::Zero();
		if (acsConfig.common_sat_pco)
		{
			double varDummy = 0;

			Vector3d bodyPCO	= antPco(satPos.Sat.id(), satPos.Sat.sys, j, time, varDummy, E_Radio::TRANSMITTER);

			dAnt = body2ecef(attStatus, bodyPCO);
		}
		else
		{
			dAnt = satAntOff(trace, time, attStatus, satPos.Sat, nav.satNavMap[satPos.Sat].lamMap);
		}

		if (antennaScalar > 0)		satPos.rSatApc = satPos.rSatCom + dAnt;
		if (antennaScalar < 0)		satPos.rSatCom = satPos.rSatApc - dAnt;
	}

	return returnValue;
}

void adjustRelativity(
	SatPos&			satPos,
	E_Relativity	applyRelativity)
{
	E_Relativity clockHasRelativity;

	if (satPos.clkSource == +E_Source::BROADCAST && satPos.Sat.sys == +E_Sys::GLO)	clockHasRelativity = E_Relativity::ON;	// Ref: RTCM STANDARD 10403.3
	else																			clockHasRelativity = E_Relativity::OFF;

	if (clockHasRelativity == applyRelativity)
	{
		return;
	}

	double scalar = 0;

	if		(clockHasRelativity == +E_Relativity::ON	&& applyRelativity == +E_Relativity::OFF)		scalar = -1;
	else if	(clockHasRelativity == +E_Relativity::OFF	&& applyRelativity == +E_Relativity::ON)		scalar = +1;

	satPos.satClk -= scalar * relativity1(satPos.rSatCom, satPos.satVel);
}

/** satellite positions and clocks.
* satellite position and clock are values at signal transmission time.
* satellite clock does not include code bias correction (tgd or bgd).
* any pseudorange and broadcast ephemeris are always needed to get signal transmission time.
*/
bool satPosClk(
	Trace&				trace,				///< Trace to output to
	GTime				teph,				///< time to select ephemeris (gpst)
	GObs&				obs,				///< observations to complete with satellite positions
	Navigation&			nav,				///< Navigation data
	vector<E_Source>	posSources,			///< Source of ephemeris data
	vector<E_Source>	clkSources,			///< Source of ephemeris data
	const KFState*		kfState_ptr,		///< Optional pointer to a kalman filter to take values from
	const KFState*		remote_ptr,			///< Optional pointer to a kalman filter to take values from
	E_OffsetType		offsetType,			///< Point of satellite to output position of
	E_Relativity		applyRelativity)	///< Option to apply relativistic correction to clock
{
	if (obs.exclude)
	{
		obs.failureExclude = true;

		return false;
	}

	tracepdeex(3, trace, "\n%-10s: teph=%s %s", __FUNCTION__, teph.to_string().c_str(), obs.Sat.id());

	double pr = 0;

	for (auto& [a, sig] : obs.sigs)
	{
		if (sig.P == 0)
			continue;

		pr = sig.P;

		break;
	}

	if (pr == 0)
	{
		obs.failureNoPseudorange = true;

		tracepdeex(2, trace, "\nno pseudorange %s sat=%s", obs.time.to_string().c_str(), obs.Sat.id().c_str());
		return false;
	}

	obs.tof = pr / CLIGHT;

	// transmission time by satellite clock
	GTime time = obs.time;

	time -= obs.tof;

	bool pass;

	pass = satclk(trace, time, teph, obs, clkSources,				nav,	kfState_ptr, remote_ptr);

	if (pass == false)
	{
		obs.failureNoSatClock = true;

		tracepdeex(2, trace, "\nno satellite clock %s sat=%s", time.to_string().c_str(), obs.Sat.id().c_str());
		return false;
	}

	tracepdeex(5, trace, "\neph time %s %s pr=%.5f, satClk= %.5f", obs.Sat.id().c_str(), time.to_string().c_str(), pr / CLIGHT, obs.satClk);

	time -= obs.satClk;	// Eugene: what if using ssr?

	// satellite position and clock at transmission time
	pass = satpos(trace, time, teph, obs, posSources, offsetType,	nav,	kfState_ptr, remote_ptr);

	if (pass == false)
	{
		obs.failureNoSatPos = true;

		tracepdeex(3, trace, "\n%s failed (no ephemeris?) %s sat=%s", __FUNCTION__, time.to_string().c_str(), obs.Sat.id().c_str());

		return false;
	}

	adjustRelativity(obs, applyRelativity);

	tracepdeex(3, trace, "\n%s sat=%s rs=%13.3f %13.3f %13.3f dtSat=%12.3f varPos=%7.3f varClk=%7.3f ephPosValid=%1X %s ephClkValid=%1X %s",
			obs.time.to_string().c_str(),
			obs.Sat.id().c_str(),
			obs.rSatCom[0],
			obs.rSatCom[1],
			obs.rSatCom[2],
			obs.satClk * 1E9,
			obs.posVar,
			obs.satClkVar,
			obs.ephPosValid,
			obs.posSource._to_string(),
			obs.ephClkValid,
			obs.clkSource._to_string());

	return true;
}
