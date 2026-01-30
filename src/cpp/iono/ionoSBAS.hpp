#pragma once
#include "common/gTime.hpp"
#include "common/receiver.hpp"

bool addSBASIGP(Trace& trace, int IODI, int Band, int ID, int entry, GTime tof, int nband);

bool writeIonoData(Trace& trace, int IODI, int band, int entry, GTime tof, int GIVDI, int GIVEI);

double ionmodelSBAS(GTime t, const VectorPos& pos, const AzEl& azel, double& ionVar);