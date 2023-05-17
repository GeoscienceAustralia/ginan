
#pragma once

#include <iostream>

#include "observations.hpp"
#include "gTime.hpp"
#include "enums.h"

struct RinexStation;
struct Navigation;

struct CodeType
{
	char		type = 0;
	E_ObsCode	code = E_ObsCode::NONE;
};

int readRnx(
	std::istream& 					inputStream,
	char&							type,
	ObsList&						obsList,
	Navigation&						nav,
	RinexStation*					sta,
	double&							ver,
	E_Sys&							sys,
	E_TimeSys&						tsys,
	map<E_Sys, map<int, CodeType>>&	sysCodeTypes);



string rinexSysDesc(
	E_Sys sys);
