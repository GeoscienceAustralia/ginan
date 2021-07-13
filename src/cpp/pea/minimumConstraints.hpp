#ifndef __MINIMUM_CONSTRAINTS_HPP__
#define __MINIMUM_CONSTRAINTS_HPP__

#include <map>

using std::map;

#include "streamTrace.hpp"

struct KFState;


void minimum(
	Trace&					trace,
	KFState&				kfStateStations);


#endif
