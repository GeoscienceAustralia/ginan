
#pragma once

#include <map>
#include <string>

using std::map;
using std::string;

#include "algebra.hpp"

struct ReceiverMap;

void rtsSmoothing(
	KFState&		kfState,
	ReceiverMap&	receiverMap,
	bool			write = false);


