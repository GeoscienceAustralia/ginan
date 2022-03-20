#ifndef __SUMMARY_HPP__
#define __SUMMARY_HPP__

#include <map>

using std::map;

#include "streamTrace.hpp"
#include "station.hpp"

void outputSummaries(
	Trace&					trace,	
	map<string, Station>&	stationMap);

#endif
