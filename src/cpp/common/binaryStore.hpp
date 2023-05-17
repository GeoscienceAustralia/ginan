
#pragma once

#include "algebra.hpp"


// struct MeasDataEntry
// {
// 	GTime	time;
// 	double	prefit		= 0;
// 	double	postfit		= 0;
// 	double	rtsPostfit	= 0;
// 	double	variance	= 0;
// };

// struct StateDataEntry
// {
// 	GTime	time;
// 	double	x			= 0;
// 	double	dx			= 0;
// 	double	P			= 0;
// 	double	apriori		= 0;
// 	double	rts			= 0;
// };

struct GeneralDataEntry
{
	GTime	time;
	double	data;
};


template<typename TYPE>
struct AlgebraData
{
	vector<TYPE>		entries;
	map<GTime, int>		timeIndexMap;
};

extern map<string, map<KFKey, map<E_Component, vector<GeneralDataEntry>>>> generalDataMap;

// extern map<string, map<KFKey, AlgebraData<GeneralDataEntry>>>	generalDataMap;
// extern map<string, map<KFKey, AlgebraData<MeasDataEntry>>>		measDataMap;
// extern map<string, map<KFKey, AlgebraData<StateDataEntry>>>		stateDataMap;


void storeResiduals(
	GTime				time,
	vector<KFKey>&		obsKeys,
	VectorXd&			prefits,
	VectorXd&			postfits,
	MatrixXd&			variance,
	string				suffix,
	int					beg,
	int					num);

void storeResiduals(
	GTime		time,
	VectorXd&	rtsPostfits,
	string		suffix = "");

void storeStates(
	KFState&	kfState,
	string		sufflx = "");

// struct Plotter
// {
// 	bool plotted = false;
// 	vector<PlotData> data;
// };
// 
// extern map<string, Plotter> plotMap;
