
#pragma once


#include <algorithm>
#include <fstream>
#include <string>
#include <math.h>
#include <map>


using std::string;

#include "observations.hpp"

struct SinexRecData;

void writeRinexObs(
	string&			id,
	SinexRecData&	snx,
	GTime&			time,
	ObsList&		obsList,
	const double	rnxver = 3.05);

map<string, map<E_Sys, bool>> getSysOutputFilenames(
	string	filename,
	GTime	logtime,
	bool	replaceSys	= true,
	string	id			= "");

extern map<string, string> rinexObsFilenameMap;

