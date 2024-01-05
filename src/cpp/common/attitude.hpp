
#pragma once

#include "eigenIncluder.hpp"
#include "trace.hpp"
#include "gTime.hpp"
#include "enums.h"

/** Persistent data for yaw model
*/
struct AttStatus
{
	GTime	startTime			= GTime::noTime();	///< Time of switchover to modified yaw steering (due to noon/midnight turn)
	double	startSign			= 0;				///< Sign of yaw rate at switchover
	double	startYaw			= 0;				///< Yaw at switchover
	double	startYawRate		= 0;				///< Yaw rate at switchover
	GTime	excludeTime			= GTime::noTime();	///< Time to skip yaw modelling until, due to unknown yaw behaviour

	double	nominalYaw			= 0;				///< Latest nominal yaw
	double	modelYaw			= 0;				///< Latest modelled yaw (i.e. considering noon/midnight turns)
	GTime	modelYawTime		= GTime::noTime();	///< Time of modelYaw (and nominalYaw)
	bool	modelYawValid		= false;			///< Model yaw was calculated sucessfully

	VectorEcef	eXBody;		///< X+ unit vector of body-fixed coordinates (ECEF)
	VectorEcef	eYBody;		///< Y+ unit vector of body-fixed coordinates (ECEF)
	VectorEcef	eZBody;		///< Z+ unit vector of body-fixed coordinates (ECEF)

	VectorEcef	eXAnt;		///< X+ unit vector of antenna-fixed coordinates (ECEF)
	VectorEcef	eYAnt;		///< Y+ unit vector of antenna-fixed coordinates (ECEF)
	VectorEcef	eZAnt;		///< Z+ unit vector of antenna-fixed coordinates (ECEF)
};

struct Receiver;
struct KFState;
struct SatPos;

void recAtt(
			Receiver&			rec,
			GTime				time,
			vector<E_Source>	attitudeTypes,
	const	KFState*			kfState_ptr	= nullptr,
	const	KFState*			remote_ptr	= nullptr);

void updateSatAtts(
	SatPos&		satPos);

void updateSatYaw(
		SatPos&		satPos,
		AttStatus&	attStatus);
