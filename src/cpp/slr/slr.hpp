
#pragma once

#include <memory>
#include <string>
#include <map>

#include "enums.h"

using std::shared_ptr;
using std::string;
using std::map;

struct VectorPos;
struct Navigation;
struct Receiver;
struct ObsList;
struct KFState;
struct LObs;


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
	Receiver&	rec);

double getTropDelay(
	LObs&		obs,
	VectorPos&	pos,
	double		elevation);

void applyBiases(
	LObs&	obs);

void satPossSlr(
	Trace&				trace,				///< Trace to output to
	ObsList&			slrObsList,			///< List of observations to complete with satellite positions
	Navigation&			nav,				///< Navigation data
	vector<E_Source>	ephTypes,			///< Source of ephemeris data
	E_OffsetType		offsetType,			///< Point of satellite to output position of
	E_Relativity		applyRelativity,	///< Option to apply relativistic correction to clock
	const KFState*		kfState_ptr);		///< Optional pointer to a kalman filter to take values from

extern map<string, map<GTime, shared_ptr<LObs>>> slrSiteObsMap;

void readCrd(
	string filepath);

void outputSortedSlrObsPerRec(
	string		filepath,
	ObsList&	slrObsList);

map<string, vector<string>> outputSortedSlrObs();

int readSlrObs(
	std::istream&	inputStream,
	ObsList&		slrObsList);

extern map<string, vector<string>>	slrObsFiles;
extern SphericalComMap				sphericalComMap;
