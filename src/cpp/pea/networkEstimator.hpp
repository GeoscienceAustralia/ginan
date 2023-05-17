
#pragma once

#include "gTime.hpp"
#include "trace.hpp"

#include <map>

using std::map;

//forward declarations
struct KFState;
struct Station;

using StationMap = map<string, Station>;

void networkEstimator(
	Trace&			trace,
	StationMap&		stations,
	KFState&		kfState,
	GTime			time);
