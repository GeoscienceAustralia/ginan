
#pragma once

#include <map>
#include <string>

using std::map;
using std::string;

#include "algebra.hpp"

struct Station;
typedef map<string, Station> StationMap;

KFState RTS_Process(
	KFState&	kfState, 
	bool		write			= false,
	StationMap*	stationMap_ptr	= nullptr);


