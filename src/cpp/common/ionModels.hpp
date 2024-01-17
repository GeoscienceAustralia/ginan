
#pragma once

#include <string>

using std::string;

#include "eigenIncluder.hpp"
#include "gTime.hpp"
#include "enums.h"


struct Navigation;

double ionmodel(
	GTime				t,
	const double*		ion,
	const VectorPos&	pos,
	const AzEl&			azel);

double ionmapf(
	const VectorPos&	pos,
	const AzEl&			azel,
	E_IonoMapFn			mapFn,
	double				hion);

double ionppp(
	const VectorPos&	pos,
	const AzEl&			azel,
	double				re,
	double				hion,
	VectorPos&			pppos);

bool iontec(
	GTime				time,
	const Navigation*	nav,
	const VectorPos&	pos,
	const AzEl&			azel,
	E_IonoMapFn			mapFn,
	double				layerHeight,
	E_IonoFrame			frame,
	double&				delay,
	double&				var);

void readTec(
	string				file,
	Navigation*			nav);

