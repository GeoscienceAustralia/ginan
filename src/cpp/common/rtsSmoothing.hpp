#pragma once

#include <map>
#include <string>
#include "common/algebra.hpp"

using std::map;
using std::string;

struct ReceiverMap;

void rtsSmoothing(KFState& kfState, ReceiverMap& receiverMap, bool write = false);
