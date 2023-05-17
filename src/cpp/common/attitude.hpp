
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
	double	eclipseYawRate		= 0;			///< (GPS-IIF) Calculated yaw rate to exit eclipse at nominal yaw
	double	sunDeltaAtSwitch	= 0;			///< (GAL IOV) Sign (1/-1) of Sun y-position (orbital ref. frame) at switchover to modified yaw steering
	double	yawAtSwitch			= 0;			///< (GAL FOC) Yaw at switchover to modified yaw steering
	GTime	switchTime			= {};			///< (GAL FOC) Time of switchover to modified yaw steering (due to noon/midnight turn)
	
	VectorEcef	eXBody;		///< X+ unit vector of body-fixed coordinates (ECEF)
	VectorEcef	eYBody;		///< Y+ unit vector of body-fixed coordinates (ECEF)
	VectorEcef	eZBody;		///< Z+ unit vector of body-fixed coordinates (ECEF)

	VectorEcef	eXAnt;		///< X+ unit vector of antenna-fixed coordinates (ECEF)
	VectorEcef	eYAnt;		///< Y+ unit vector of antenna-fixed coordinates (ECEF)
	VectorEcef	eZAnt;		///< Z+ unit vector of antenna-fixed coordinates (ECEF)

	double		var		= 0;
};

struct Station;
struct GObs;

void recAtt(
	Station&			rec,
	GTime				time,
	vector<E_Source>	attitudeTypes);

bool satAtt(	
	GObs&				obs,
	vector<E_Source>	attitudeTypes,
	AttStatus&			attStatus,
	bool				origGal);

void updateSatAtts(
	GObs&		obs);
