
#ifndef __RTS_SMOOTHING_HPP__
#define __RTS_SMOOTHING_HPP__

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
	StationMap*	stationMap_ptr	= nullptr,
	string		clockFilename	= "",
	string		tropFilename	= "");


#endif
