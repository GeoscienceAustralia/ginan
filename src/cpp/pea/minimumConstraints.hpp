

#pragma once

#include <map>

using std::map;

#include "station.hpp"
#include "trace.hpp"

struct KFState;


void mincon(
	Trace&		trace,
	KFState&	kfStateStations,
	bool		commentSinex	= false);

KFState minconOnly(
	Trace&		trace,
	StationMap&	stationMap);
