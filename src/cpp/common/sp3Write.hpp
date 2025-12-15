#pragma once

#include <string>
#include "common/rinexClkWrite.hpp"

using std::string;

struct GTime;
struct KFState;

void outputSp3(
    string           filename,
    GTime            time,
    KFState&         kfState,
    vector<E_Source> sp3OrbitSrcs,
    vector<E_Source> sp3ClockSrcs,
    bool             predicted = false
);
