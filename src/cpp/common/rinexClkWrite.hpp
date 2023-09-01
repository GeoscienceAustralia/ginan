
#pragma once

#include <vector>
#include <string>
#include <map>

using std::vector;
using std::string;
using std::map;

#include "enums.h"

struct GTime;
struct StationMap;
class E_Source;

void outputClocks(
	string				filename,
	vector<E_Source>	clkDataRecSrcs,
	vector<E_Source>	clkDataSatSrcs,
	GTime&				time,
	KFState&			kfState,
	StationMap*			stationMap_ptr = nullptr);
