#include <string>
#include <fstream>
#include <iostream>

#include "centerMassCorrections.hpp"

using namespace std;

centerMassCorrections cmc;

void centerMassCorrections::readcmc()
{
    if (filename.empty())
    {
        return;
    }

    std::ifstream infile(filename);
    if (!infile)
    {
        return;
    }

    string line;

    std::getline(infile, line);

    while (std::getline(infile, line))
    {
        std::istringstream iss(line);
        string wavname, dummy;
        double zIn, zOut, xIn, xOut, yIn, yOut;
        iss >> wavname >> dummy >> zIn >> zOut >> xIn >> xOut >> yIn >> yOut;
        data[wavname] << xIn, yIn, zIn, xOut, yOut, zOut;
    }
    for (auto& [wave, coeff] : data)
    {
        std::cout << wave << " " << coeff.transpose() << std::endl;
    }
    initialized = true;
//    DoodsonNumbers["K1"] = Array6d (6);
    DoodsonNumbers["K1"] << 1, 1, 0, 0, 0, 0; // 165.555
    DoodsonNumbers["K2"] << 2, 2, 0, 0, 0, 0; // 275.555
    DoodsonNumbers["M2"] << 2, 0, 0, 0, 0, 0; // 255.555
    DoodsonNumbers["Mf"] << 0, 2, 0, 0, 0, 0; // 75.555
    DoodsonNumbers["Mm"] << 0, 1, -1, -1, 0, 0; // 64.455
    DoodsonNumbers["N2"] << 2, -2, 0, 2, 0, 0; // 235.755
    DoodsonNumbers["O1"] << 1, -1, 0, 0, 0, 0; // 145.555
    DoodsonNumbers["P1"] << 1, 1, -2, 0, 0, 0; //163.555
    DoodsonNumbers["Q1"] << 1, -2, 0, 1, 0, 0; // 135.655
    DoodsonNumbers["S2"] << 2, 2, -2, 0, 0, 0; // 273.555
    DoodsonNumbers["Ssa"] << 0, 0, 2, 0, 0, 0; // 57.555

//    exit(0);
}

Vector3d centerMassCorrections::estimate(Array6d dood)
{
    Vector3d cmcEstimate = Vector3d::Zero();
    for (auto& [wave, coeff] : data)
    {
        double theta = (dood * DoodsonNumbers[wave]).sum();
        for (int i = 0; i < 3; i++ )
        {
            cmcEstimate(i) += coeff[i*2] * cos(theta) + coeff[i*2+1] * sin(theta);
        }
    }
    return cmcEstimate;
}