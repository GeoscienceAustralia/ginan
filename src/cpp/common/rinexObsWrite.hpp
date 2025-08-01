#pragma once

#include <algorithm>
#include <fstream>
#include <map>
#include <math.h>
#include <string>
#include "common/observations.hpp"

using std::string;

struct Receiver;

void writeRinexObs(
    string&      id,
    Receiver&    snx,
    GTime&       time,
    ObsList&     obsList,
    const double rnxver = 3.05
);

map<string, map<E_Sys, bool>>
getSysOutputFilenames(string filename, GTime logtime, bool replaceSys = true, string id = "");

extern map<string, string> rinexObsFilenameMap;
