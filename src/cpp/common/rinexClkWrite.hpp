#pragma once

#include <map>
#include <string>
#include <vector>
#include "common/enums.h"

using std::map;
using std::string;
using std::vector;

struct GTime;
struct KFState;
struct ReceiverMap;

void outputClocks(
    string           filename,
    const GTime&     time,
    KFState&         kfState,
    vector<E_Source> clkDataRecSrcs,
    vector<E_Source> clkDataSatSrcs,
    ReceiverMap*     receiverMap_ptr = nullptr
);
