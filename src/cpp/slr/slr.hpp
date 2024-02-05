
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

void getRecPosApriori(
	LObs&		obs,
	Receiver&	rec);

double laserTropDelay(
	LObs&		obs,
	VectorPos&	pos,
	double		elevation);

void applyBiases(
	LObs&	obs);

void satPossSlr(
	Trace&				trace,
	ObsList&			slrObsList,
	Navigation&			nav,
	vector<E_Source>	ephTypes,
	E_OffsetType		offsetType,
	E_Relativity		applyRelativity,
	const KFState*		kfState_ptr);

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
