
#pragma once

#include <string>

using std::string;

#include "common/rinexClkWrite.hpp"

struct GTime;
class E_Source;
struct KFState;

void outputSp3(
	string				filename,
	GTime				time,
	KFState&			kfState,
	vector<E_Source>	sp3OrbitSrcs,
	vector<E_Source>	sp3ClockSrcs,
	bool				predicted	= false);

