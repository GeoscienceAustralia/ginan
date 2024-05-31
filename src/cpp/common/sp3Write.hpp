
#pragma once

#include <string>

using std::string;

#include "rinexClkWrite.hpp"

struct GTime;
class E_Source;
struct KFState;

void outputSp3(
	string				filename,
	GTime				time,
	vector<E_Source>	sp3OrbitSrcs,
	vector<E_Source>	sp3ClockSrcs,
	KFState*			kfState_ptr	= nullptr,
	bool				predicted	= false);

