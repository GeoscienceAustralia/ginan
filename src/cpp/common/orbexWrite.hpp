#pragma once

#include <string>
#include <map>

using std::string;
using std::map;

#include "rinexObsWrite.hpp"

struct GTime;
class E_Source;
struct KFState;

/** File editing information for ORBEX writing
*/
struct OrbexFileData
{
	map<SatSys, bool>	satList;				///< List of satellites included in the ORBEX file
	long 				headerTimePos	= 0;	///< Position of put pointer for END_TIME line
	long				satListPos		= 0;	///< Position of put pointer for beginning of satellite list
	long 				endDataPos		= 0;	///< Position of put pointer for end of EPHEMERIS/DATA block
};

void outputOrbex(
	string				filename,
	GTime				time,
	KFState&			kfState,
	vector<E_Source>	orbDataSrcs,
	vector<E_Source>	clkDataSrcs,
	vector<E_Source>	attDataSrcs);

