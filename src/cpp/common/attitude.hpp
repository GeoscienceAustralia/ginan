
#pragma once

#include "eigenIncluder.hpp"
#include "trace.hpp"
#include "gTime.hpp"
#include "enums.h"

/** Persistent data for yaw model
*/
struct AttStatus
{
	double	nominalYaw			= 0;			///< Latest nominal yaw
	GTime	nominalYawTime		= {};			///< Time of latest nominal yaw
	
	bool	modelYawValid		= false;		///< Model yaw was calculated sucessfully
	double	modelYaw			= 0;			///< Latest model yaw
	GTime	modelYawTime		= {};			///< Time of latest model yaw
	double	prevBeta			= 0;			///< Previous beta angle
	GTime	prevBetaTime		= {};			///< Time of previous beta angle
	double	eclipseYawRate		= 0;			///< Calculated yaw rate to exit eclipse at nominal yaw
	double	signAtSwitch		= 0;			///< Sign (1/-1) of Sun y-position (orbital ref. frame) at switchover to modified yaw steering
	double	yawAtSwitch			= 0;			///< Yaw at switchover to modified yaw steering
	GTime	switchTime			= {};			///< Time of switchover to modified yaw steering (due to noon/midnight turn)
	
	VectorEcef	eXBody;		///< X+ unit vector of body-fixed coordinates (ECEF)
	VectorEcef	eYBody;		///< Y+ unit vector of body-fixed coordinates (ECEF)
	VectorEcef	eZBody;		///< Z+ unit vector of body-fixed coordinates (ECEF)

	VectorEcef	eXAnt;		///< X+ unit vector of antenna-fixed coordinates (ECEF)
	VectorEcef	eYAnt;		///< Y+ unit vector of antenna-fixed coordinates (ECEF)
	VectorEcef	eZAnt;		///< Z+ unit vector of antenna-fixed coordinates (ECEF)
};

struct Station;
struct SatPos;

void recAtt(
	Station&			rec,
	GTime				time,
	vector<E_Source>	attitudeTypes);

void updateSatAtts(
	SatPos&		satPos);
