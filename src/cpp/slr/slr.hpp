
#pragma once

#include <memory>
#include <string>
#include <map>

#include "enums.h"

using std::shared_ptr;
using std::string;
using std::map;

struct VectorPos;
struct TropStates;
struct TropMapping;
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

struct SphericalComMap :  map<string, map<string, map<GTime, SphericalCom, std::greater<GTime>>>>  // index by sat, then by rec
{

};

void readCom(
	string filepath);

bool isSpherical(
	string satName);

double		satComOffSphere(
	LObs&	obs);

VectorEcef	satComOffGnss(
	LObs&	obs);

double laserTropDelay(
	LObs&			obs,
	VectorPos&		pos,
	AzEl&			azel,
	TropStates&		tropStates,
	TropMapping&	dTropDx,
	double&			var);

void updateSlrRecBiases(
	LObs&		obs);

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
extern map<string, int>				cdpIdMap;
