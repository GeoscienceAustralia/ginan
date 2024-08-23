
#pragma once

#include <set>

using std::set;

#include "satSys.hpp"
#include "mongo.hpp"
#include "trace.hpp"
#include "gTime.hpp"


struct MongoOptions;
struct ReceiverMap;
struct OrbitState;
struct KFState;
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
	const	GTime&	time,
			KFMeas&	kfMeas,
			bool	queue	= false,
			string	suffix	= "",
			int		beg		= 0,
			int		num		= -1);

void mongoTrace(
	const vector<string>&				jsons,
	bool								queue = false);

void mongoOutputConfig(
	string& config);

struct MongoStatesOptions
{
	string			suffix;
	string			collection	= STATES_DB;
	E_Mongo			instances;
	bool			force		= false;
	bool			upsert		= false;
	bool			queue		= false;
	bool			index		= true;
	GTime			updated;
};

void mongoStatesAvailable(
	GTime				time,
	MongoStatesOptions	opts = {});

void mongoStates(
	KFState&			kfState,
	MongoStatesOptions	opts = {});

void mongoMeasSatStat(
	ReceiverMap&		receiverMap);

void mongoTestStat(
	KFState&			kfState,
	TestStatistics&		statistics);

void mongoCull(
	GTime time);
