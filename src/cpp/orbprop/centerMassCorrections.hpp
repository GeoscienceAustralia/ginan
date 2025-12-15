#pragma once

#include <map>
#include <string>
#include "common/eigenIncluder.hpp"

using std::map;
using std::string;

struct CenterMassCorrections
{
    map<string, Vector6d> data;
    bool                  initialized = false;

    void read(const string& filename);

    Vector3d estimate(Array6d& dood);

    // Need to add the fundamental args to the cmc estimate function. QnD
    map<string, Array6d> doodsonNumbers;
};

extern CenterMassCorrections cmc;
