#pragma once

#include <map>
#include "common/trace.hpp"

using std::map;

struct ReceiverMap;

void outputStatistics(
    Trace&            trace,
    map<string, int>& statisticsMap,
    map<string, int>& statisticsMapSum
);

void outputSummaries(Trace& trace, ReceiverMap& receiverMap);
