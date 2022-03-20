
#ifndef __NETWORK_ESTIMATOR_HPP__
#define __NETWORK_ESTIMATOR_HPP__

#include "streamTrace.hpp"
#include "gTime.hpp"

#include <map>

using std::map;

//forward declarations
struct prcopt_t;
struct KFState;
struct Station;

using StationMap = map<string, Station>;

void networkEstimator(
	Trace&			trace,
	StationMap&		stations,
	KFState&		kfState,
	GTime			time);

#endif
