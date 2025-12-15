#pragma once

#include "common/eigenIncluder.hpp"
#include "common/enums.h"
#include "common/lapackWrapper.hpp"

/**
 * @brief BLAS/LAPACK optimized operations for Kalman filtering
 *
 * This class provides high-performance implementations of core Kalman filter
 * operations using CBLAS and LapackWrapper while maintaining Eigen matrix interfaces
 * for compatibility with existing code.
 *
 * Design principle: Single Responsibility - handles only mathematical computations
 */
class KalmanFilterBLAS
{
   public:
    /**
     * @brief Compute innovation covariance Q = H*P*H' + R using BLAS
     *
     * @param H Design matrix (numH x numX)
     * @param P State covariance (numX x numX)
     * @param R Measurement noise (numH x numH)
     * @param Q Output innovation covariance (numH x numH)
     */
    static void computeInnovationCovariance(
        const MatrixXd& H,
        const MatrixXd& P,
        const MatrixXd& R,
        MatrixXd&       Q
    );

    /**
     * @brief Compute Kalman gain K = P*H'*Q^-1 using BLAS/LAPACKE
     *
     * @param H Design matrix (numH x numX)
     * @param P State covariance (numX x numX)
     * @param Q Innovation covariance (numH x numH) - will be modified!
     * @param K Output Kalman gain (numX x numH)
     * @param Qinv Optional output for Q inverse
     * @param inverter Inversion method to use
     * @return true if successful, false if inversion failed
     */
    static bool computeKalmanGain(
        const MatrixXd& H,
        const MatrixXd& P,
        MatrixXd&       Q,  // Non-const: will be modified by LAPACK
        MatrixXd&       K,
        MatrixXd*       Qinv,
        E_Inverter      inverter
    );

    /**
     * @brief Compute H*P matrix product using BLAS
     *
     * @param H Design matrix (numH x numX)
     * @param P State covariance (numX x numX)
     * @param HP Output matrix (numH x numX)
     */
    static void computeHP(const MatrixXd& H, const MatrixXd& P, MatrixXd& HP);

    /**
     * @brief Update state vector: xp = x + K*v using BLAS
     *
     * @param x Prior state (numX)
     * @param K Kalman gain (numX x numH)
     * @param v Innovation vector (numH)
     * @param xp Output posterior state (numX)
     * @param dx Output state change (numX)
     * @param begX Starting index in full state vector
     * @param numX Number of states to update
     * @param begH Starting index in measurement vector
     * @param numH Number of measurements
     */
    static void updateStateVector(
        const VectorXd& x,
        const MatrixXd& K,
        const VectorXd& v,
        VectorXd&       xp,
        VectorXd&       dx,
        int             begX,
        int             numX,
        int             begH,
        int             numH
    );

    /**
     * @brief Update covariance: Pp = P - K*H*P (standard form) using BLAS
     *
     * @param P Prior covariance (numX x numX)
     * @param K Kalman gain (numX x numH)
     * @param H Design matrix (numH x numX)
     * @param HP Pre-computed H*P (numH x numX)
     * @param Pp Output posterior covariance (numX x numX)
     */
    static void updateCovarianceStandard(
        const MatrixXd& P,
        const MatrixXd& K,
        const MatrixXd& H,
        const MatrixXd& HP,
        MatrixXd&       Pp
    );

    /**
     * @brief Update covariance: Pp = (I-K*H)*P*(I-K*H)' + K*R*K' (Joseph form) using BLAS
     *
     * @param P Prior covariance (numX x numX)
     * @param K Kalman gain (numX x numH)
     * @param H Design matrix (numH x numH)
     * @param R Measurement noise (numH x numH)
     * @param Pp Output posterior covariance (numX x numX)
     */
    static void updateCovarianceJoseph(
        const MatrixXd& P,
        const MatrixXd& K,
        const MatrixXd& H,
        const MatrixXd& R,
        MatrixXd&       Pp
    );

    /**
     * @brief Solve Q*X = B for multiple right-hand sides using LAPACKE
     *
     * @param Q Coefficient matrix (n x n) - will be modified!
     * @param B Right-hand side / solution matrix (n x m) - will be modified!
     * @param inverter Solver method to use
     * @return true if successful, false otherwise
     */
    static bool solveLinearSystem(MatrixXd& Q, MatrixXd& B, E_Inverter inverter);

   private:
    /**
     * @brief Symmetrize matrix: A = (A + A') / 2
     *
     * @param A Matrix to symmetrize (modified in place)
     */
    static void symmetrizeMatrix(MatrixXd& A);
};
