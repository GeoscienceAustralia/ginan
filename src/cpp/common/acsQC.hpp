#pragma once

#include "common/enums.h"
#include "common/trace.hpp"

struct GObs;
struct ObsList;

int lsqqc(
    Trace&        trace,
    const double* H,
    const double* P,
    const double* Z,
    double*       v,
    int           m,
    int           n,
    int           ind,
    int           norb,
    double*       xo = nullptr,
    double*       Po = nullptr
);

int chiqc(
    Trace&        trace,
    const double* H,
    const double* P,
    const double* Z,
    const double* xp,
    double*       v,
    int           m,
    int           n,
    int           ind
);

void clearSlips(ObsList& obsList);

void detectslips(Trace& trace, ObsList& obsList);

void detslp_gf(ObsList& obsList);

void detslp_mw(ObsList& obsList);

void detslp_ll(ObsList& obsList);

int obsFreqs(const GObs& obs, E_FType& ft1, E_FType& ft2, E_FType& ft3);
