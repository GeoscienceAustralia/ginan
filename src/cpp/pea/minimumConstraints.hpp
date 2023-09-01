

#pragma once


#include "trace.hpp"

struct StationMap;
struct KFState;



void mincon(
	Trace&		trace,
	KFState&	kfStateStations,
	bool		commentSinex	= false);

KFState minconOnly(
	Trace&		trace,
	StationMap&	stationMap);
