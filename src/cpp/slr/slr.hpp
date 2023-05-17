
#pragma once

#include <string>
#include <map>

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "navigation.hpp"
#include "station.hpp"
#include "gTime.hpp"
#include "enums.h"


using std::string;
using std::map;

struct SphericalCom
{
	GTime	endTime;
	GTime	startTime;
	double	comValue = 0;
};

typedef map<string, map<string, map<GTime, SphericalCom, std::greater<GTime>>>> SphericalComMap; // index by sat, then by rec

void readCom(
	string filepath);

bool isSpherical(
	string satName);

VectorEcef satComOffSphere(
	LObs&	obs);

VectorEcef satComOffGnss(
	LObs&	obs);

double getWaterVapPressure(
	double	temperature,
	double	humidity); // This function used in slrDataStructs.cpp

void getRecPosApriori(
	LObs&		obs,
	Station&	rec);

double getTropDelay(
	LObs&		obs,
	VectorPos&	pos,
	double		elevation);

void applyBiases(
	LObs&	obs);

void satPossSlr(
	Trace&				trace,				///< Trace to output to
	GTime				teph,				///< time to select ephemeris (gpst)
	ObsList&			slrObsList,			///< List of observations to complete with satellite positions
	Navigation&			nav,				///< Navigation data
	vector<E_Source>	ephTypes,			///< Source of ephemeris data
	E_OffsetType		offsetType,			///< Point of satellite to output position of
	E_Relativity		applyRelativity,	///< Option to apply relativistic correction to clock
	const KFState*		kfState_ptr);		///< Optional pointer to a kalman filter to take values from

extern SphericalComMap sphericalComMap;
