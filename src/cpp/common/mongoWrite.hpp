
#pragma once

#include <set>

using std::set;

#include "satSys.hpp"
#include "mongo.hpp"

struct StationMap;
struct KFMeas;
struct Geph;
struct Eph;

struct TestStatistics
{
	int		numMeas				= 0;
	double	sumOfSquaresPre		= 0;
	double	sumOfSquaresPost	= 0;
	double	averageRatioPre		= 0;
	double	averageRatioPost	= 0;
	double	chiSq				= 0;
	double	dof					= 0;
	double	chiSqPerDof			= 0;
	double	qc					= 0;
};

void mongoMeasResiduals(
	GTime				time,
	KFMeas&				kfMeas,
	string				suffix	= "",
	int					beg		= 0,
	int					num		= -1);

void mongoTrace(
	string		json);

void mongoOutputConfig(
	string& config);

void mongoStates(
	KFState&			kfState,
	string				suffix = "");

void mongoMeasSatStat(
	StationMap&			stationMap);

void mongoTestStat(
	KFState&			kfState,
	TestStatistics&		statistics);

struct MongoOptions;
struct OrbitState;
typedef vector<OrbitState> Orbits;

void	outputMongoPredictions(
	Trace&			trace,		
	Orbits&			orbits,		
	GTime 			time,
	MongoOptions&	config);	

void mongoCull(
	GTime time);
