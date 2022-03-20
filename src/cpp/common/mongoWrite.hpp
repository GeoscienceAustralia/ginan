
#ifndef ___WRITEMONGO_HPP__
#define ___WRITEMONGO_HPP__

#ifdef ENABLE_MONGODB

#include <set>

using std::set;

#include "satSys.hpp"
#include "mongo.hpp"



void mongoMeasResiduals(
	vector<ObsKey>		obsKeys,
	VectorXd&			prefits,
	VectorXd&			postfits,
	MatrixXd&			variance,
	int					beg = 0,
	int					num = -1);

void mongoStates(
	KFState&			kfState,
	string				suffix = "");

void mongoMeasSatStat_all(
	StationMap&			stationMap);

void mongoTestStat(
	KFState&			kfState,
	double				prefitSumOfSqTestStat,
	double				postfitSumOfSqTestStat,
	double				chiSq,
	double				qc,
	int					dof,
	double				chiSqPerDof);

#endif

#endif
