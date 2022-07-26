#ifndef __MINIMUM_CONSTRAINTS_HPP__
#define __MINIMUM_CONSTRAINTS_HPP__

#include <map>

using std::map;

#include "streamTrace.hpp"
#include "station.hpp"

struct KFState;


void mincon(
	Trace&					trace,
	KFState&				kfStateStations);

KFState minconOnly(
	Trace&		trace,
	StationMap&	stationMap);

#endif
