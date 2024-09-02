
#pragma once

#include <map>

using std::map;

#include "trace.hpp"

struct ReceiverMap;

void outputStatistics(
	Trace&					trace,
	map<string, int>&		statisticsMap,
	map<string, int>&		statisticsMapSum);

void outputSummaries(
	Trace&		trace,
	ReceiverMap&	receiverMap);
