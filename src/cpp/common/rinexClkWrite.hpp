#ifndef WRITECLK_HPP
#define WRITECLK_HPP

// Needed as StationMap is a kind of typedef.
#include "station.hpp"

#include <vector>
#include <string>
#include <map>

using std::vector;
using std::string;
using std::pair;
using std::map;

#include "enums.h"

struct GTime;
struct KFState;
class E_Ephemeris;

void tryPrepareFilterPointers(
	KFState&		kfState, 
	StationMap*		stationMap_ptr);

void outputClocks(
	string				filename,
	E_Ephemeris			clkDataRecSrc,
	E_Ephemeris			clkDataSatSrc,
	GTime&				time,
	OutSys&				outSys,
	KFState&			kfState,
	StationMap*			stationMap_ptr,
	bool				isUser = false);

#endif
