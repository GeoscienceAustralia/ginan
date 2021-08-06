
#ifndef __NETWORK_ESTIMATOR_HPP__
#define __NETWORK_ESTIMATOR_HPP__

#include "streamTrace.hpp"
#include "gTime.hpp"

#include <list>

using std::list;

//forward declarations
struct prcopt_t;
struct KFState;
struct Station;

// typedef list<Station*> StationList;
using StationList = list<Station*>;

void networkEstimator(
	Trace&			trace,
	StationList&	streams,
	KFState&		kfState,
	GTime			tsync);

#endif
