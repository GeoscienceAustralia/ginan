#ifndef __RINEXOUTPUT_H__
#define __RINEXOUTPUT_H__

#include <algorithm>
#include <fstream>
#include <string>
#include <math.h>
#include <list>
#include <map>


using std::string;

#include "observations.hpp"

struct Sinex_stn_snx_t;

void writeRinexObs(
	string&				id,
	Sinex_stn_snx_t&	snx,
	GTime&				time,
	ObsList&			obsList);

map<string, map<E_Sys, bool>> getSysOutputFilenames(
	string	filename,
	GTime	logtime,
	string	id			= "");

extern map<string, string> rinexObsFilenameMap;

typedef map<E_Sys,bool> OutSys;

#endif
