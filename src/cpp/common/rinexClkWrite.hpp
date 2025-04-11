
#pragma once

#include <vector>
#include <string>
#include <map>

using std::vector;
using std::string;
using std::map;

#include "common/enums.h"

struct GTime;
struct ReceiverMap;
class E_Source;

void outputClocks(
	string				filename,
	const GTime&		time,
	KFState&			kfState,
	vector<E_Source>	clkDataRecSrcs,
	vector<E_Source>	clkDataSatSrcs,
	ReceiverMap*		receiverMap_ptr = nullptr);
