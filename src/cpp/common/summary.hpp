
#pragma once

#include <map>

using std::map;

#include "station.hpp"
#include "trace.hpp"

void outputStatistics(
	Trace&					trace,
	map<string, int>&		statisticsMap,
	map<string, int>&		statisticsMapSum);

void outputSummaries(
	Trace&					trace,	
	map<string, Station>&	stationMap);
