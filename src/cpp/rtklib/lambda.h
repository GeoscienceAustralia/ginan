#pragma once

#include "common/eigenIncluder.hpp"

/* integer ambiguity resolution ----------------------------------------------*/
int lambda(
    Trace&        trace,
    int           n,
    int           m,
    const double* a,
    const double* Q,
    double*       F,
    double*       s,
    double        Pf,
    bool&         pass
);
/** Integer least-squares estimation with the exact decorrelation transform
 * used by LAMBDA exposed as z=Z^T*a. */
int lambdaWithTransform(
    Trace&        trace,
    int           n,
    int           m,
    const double* a,
    const double* Q,
    double*       F,
    double*       s,
    double        Pf,
    bool&         pass,
    MatrixXd&     ZMat,
    MatrixXd&     reducedCovariance,
    VectorXd&     conditionalVariances,
    VectorXd&     conditionalSuccessRates,
    double&       bootstrappedSuccessRate
);
int newLambda(
    Trace&    trace,
    int       numInts,
    int       numSols,
    VectorXd& floatSol,
    MatrixXd& QMat,
    MatrixXd& ZMat,
    double*   F,
    double*   solResiduals,
    double    Pf,
    int*      index
);
void lambdaCalcs(KFState& kfState);
