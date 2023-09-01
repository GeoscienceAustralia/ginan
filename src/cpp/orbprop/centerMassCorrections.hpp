#pragma once

#include "eigenIncluder.hpp"



struct centerMassCorrections {
    centerMassCorrections(){};
    std::map<std::string, Vector6d> data;
    std::string filename;
    bool initialized = false;

    void readcmc();
    Vector3d estimate(Array6d dood);

    //Need to add the fundamental args to the cmc estimate function. QnD
    std::map<std::string, Array6d> DoodsonNumbers;

};
extern centerMassCorrections cmc;

