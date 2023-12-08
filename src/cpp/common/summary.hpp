
#pragma once

#include <map>

using std::map;

#include "trace.hpp"

struct StationMap;

void outputStatistics(
	Trace&					trace,
	map<string, int>&		statisticsMap,
	map<string, int>&		statisticsMapSum);

void outputSummaries(
	Trace&		trace,	
	StationMap&	stationMap);
