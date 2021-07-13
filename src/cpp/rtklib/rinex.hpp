#ifndef __RINEX__HPP__
#define __RINEX__HPP__

#include <iostream>

#include "observations.hpp"
#include "gTime.hpp"
#include "enums.h"

struct RinexStation;
struct obs_t;
struct nav_t;

struct CodeType
{
	char		type;
	E_ObsCode	code = E_ObsCode::NONE;
};

int readrnx(
	std::istream& 					inputStream,
	char&							type,
	ObsList&						obsList,
	nav_t&							nav,
	RinexStation*					sta,
	double&							ver,
	E_Sys&							sys,
	int&							tsys,
	map<E_Sys, vector<CodeType>>&	sysCodeTypes);

#endif
