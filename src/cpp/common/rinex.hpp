
#pragma once

#include <iostream>

#include "enums.h"

struct RinexStation;
struct Navigation;
struct ObsList;

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
	RinexStation&					rnxRec,
	double&							ver,
	E_Sys&							sys,
	E_TimeSys&						tsys,
	map<E_Sys, map<int, CodeType>>&	sysCodeTypes);

string rinexSysDesc(
	E_Sys sys);
