
#pragma once

#include <map>
#include <string>

using std::map;
using std::string;

#include "algebra.hpp"

struct StationMap;

KFState rtsSmoothing(
	KFState&	kfState, 
	bool		write			= false,
	StationMap*	stationMap_ptr	= nullptr);


