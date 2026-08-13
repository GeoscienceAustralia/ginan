#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>
#include "common/eigenIncluder.hpp"

struct ZhangIfUserCoefficients
{
    double alpha = std::numeric_limits<double>::quiet_NaN();
    double beta = std::numeric_limits<double>::quiet_NaN();
    double narrowLaneWavelength = std::numeric_limits<double>::quiet_NaN();
    bool valid = false;
};

struct ZhangIfConditionalEstimate
{
    VectorXd mean;
    MatrixXd covariance;
    bool valid = false;
    std::string failureReason = "INVALID_DIMENSIONS";
};

/** Condition N1 = N_IF + k*N_WL on an accepted integer WL vector.
 *
 * The IF and WL float estimates may be produced by separate estimators, but
 * their shared raw-observation errors must be supplied in crossCovariance.
 * Setting that block to zero is only valid for statistically independent raw
 * observations and is explicitly not the E27 user model.
 */
inline ZhangIfConditionalEstimate zhangConditionFirstIntegerGivenWideLane(
    const VectorXd& ifMean,
    const MatrixXd& ifCovariance,
    const VectorXd& wideLaneMean,
    const MatrixXd& wideLaneCovariance,
    const MatrixXd& crossCovariance,
    const VectorXd& fixedWideLane,
    double wideLaneCoefficient)
{
    ZhangIfConditionalEstimate result;
    const int dimension = ifMean.size();
    if (dimension == 0 || wideLaneMean.size() != dimension ||
        fixedWideLane.size() != dimension ||
        ifCovariance.rows() != dimension || ifCovariance.cols() != dimension ||
        wideLaneCovariance.rows() != dimension ||
        wideLaneCovariance.cols() != dimension ||
        crossCovariance.rows() != dimension ||
        crossCovariance.cols() != dimension ||
        !ifMean.allFinite() || !wideLaneMean.allFinite() ||
        !fixedWideLane.allFinite() || !ifCovariance.allFinite() ||
        !wideLaneCovariance.allFinite() || !crossCovariance.allFinite() ||
        !std::isfinite(wideLaneCoefficient))
    {
        return result;
    }

    const MatrixXd symmetricWideLane =
        0.5 * (wideLaneCovariance + wideLaneCovariance.transpose());
    Eigen::SelfAdjointEigenSolver<MatrixXd> solver(symmetricWideLane);
    if (solver.info() != Eigen::Success ||
        !solver.eigenvalues().allFinite())
    {
        result.failureReason = "WL_EIGENSOLVER_FAILURE";
        return result;
    }
    const double maximum = solver.eigenvalues().maxCoeff();
    const double tolerance = std::max(1e-14, maximum * 1e-12);
    if (!(maximum > 0) || solver.eigenvalues().minCoeff() <= tolerance)
    {
        result.failureReason = "WL_COVARIANCE_SINGULAR";
        return result;
    }
    const MatrixXd inverse = solver.eigenvectors() *
        solver.eigenvalues().cwiseInverse().asDiagonal() *
        solver.eigenvectors().transpose();

    result.mean = ifMean + wideLaneCoefficient * fixedWideLane +
        crossCovariance * inverse * (fixedWideLane - wideLaneMean);
    result.covariance = ifCovariance -
        crossCovariance * inverse * crossCovariance.transpose();
    result.covariance =
        0.5 * (result.covariance + result.covariance.transpose());
    if (!result.mean.allFinite() || !result.covariance.allFinite())
    {
        result.failureReason = "CONDITIONAL_NONFINITE";
        return result;
    }
    Eigen::SelfAdjointEigenSolver<MatrixXd> conditionalSolver(result.covariance);
    const double conditionalScale = std::max(
        1.0, result.covariance.diagonal().cwiseAbs().maxCoeff());
    if (conditionalSolver.info() != Eigen::Success ||
        !conditionalSolver.eigenvalues().allFinite() ||
        conditionalSolver.eigenvalues().minCoeff() < -1e-11 * conditionalScale)
    {
        result.failureReason = "CONDITIONAL_NOT_PSD";
        return result;
    }
    result.valid = true;
    result.failureReason = "NONE";
    return result;
}

inline ZhangIfUserCoefficients zhangIfUserCoefficients(
    double firstWavelength,
    double secondWavelength
)
{
    ZhangIfUserCoefficients result;
    const double denominator =
        secondWavelength * secondWavelength -
        firstWavelength * firstWavelength;
    if (!(firstWavelength > 0) || !(secondWavelength > 0) ||
        std::abs(denominator) <= std::numeric_limits<double>::epsilon())
    {
        return result;
    }
    result.alpha = secondWavelength * secondWavelength / denominator;
    result.beta = -firstWavelength * firstWavelength / denominator;
    result.narrowLaneWavelength =
        firstWavelength * secondWavelength /
        (firstWavelength + secondWavelength);
    result.valid = std::isfinite(result.alpha) &&
        std::isfinite(result.beta) &&
        std::isfinite(result.narrowLaneWavelength);
    return result;
}

inline double zhangIfAmbiguityMetres(
    const ZhangIfUserCoefficients& coefficients,
    double firstWavelength,
    double secondWavelength,
    double firstInteger,
    double secondInteger
)
{
    return coefficients.alpha * firstWavelength * firstInteger +
        coefficients.beta * secondWavelength * secondInteger;
}

inline double zhangIfConditionedFirstInteger(
    const ZhangIfUserCoefficients& coefficients,
    double secondWavelength,
    double ifAmbiguityMetres,
    double wideLaneInteger
)
{
    // N2 = N1 - NW, hence A_IF = lambda_NL*N1 - beta*lambda2*NW.
    return (ifAmbiguityMetres +
            coefficients.beta * secondWavelength * wideLaneInteger) /
        coefficients.narrowLaneWavelength;
}

/** Build the product functional applied by an IF user.
 *
 * Product parameters are ordered [clock, phase_L1, phase_L2] per satellite.
 * Code correction is clock; phase correction is clock-phase.  Rows are
 * satellite single differences against referenceSatellite.
 */
inline MatrixXd zhangIfProductSdFunctional(
    int satelliteCount,
    int referenceSatellite,
    const ZhangIfUserCoefficients& coefficients,
    bool phase
)
{
    if (!coefficients.valid || satelliteCount < 2 ||
        referenceSatellite < 0 || referenceSatellite >= satelliteCount)
    {
        return {};
    }
    MatrixXd transform = MatrixXd::Zero(satelliteCount - 1, 3 * satelliteCount);
    int row = 0;
    for (int satellite = 0; satellite < satelliteCount; satellite++)
    {
        if (satellite == referenceSatellite)
        {
            continue;
        }
	for (const auto& [index, sign] :
             {std::pair{satellite, +1.0},
              std::pair{referenceSatellite, -1.0}})
        {
            transform(row, 3 * index) += sign;
            if (phase)
            {
                transform(row, 3 * index + 1) -= sign * coefficients.alpha;
                transform(row, 3 * index + 2) -= sign * coefficients.beta;
            }
        }
        row++;
    }
    return transform;
}

inline MatrixXd zhangPropagateIfProductSdCovariance(
    const MatrixXd& productCovariance,
    int satelliteCount,
    int referenceSatellite,
    const ZhangIfUserCoefficients& coefficients,
    bool phase
)
{
    MatrixXd transform = zhangIfProductSdFunctional(
        satelliteCount, referenceSatellite, coefficients, phase);
    if (transform.cols() != productCovariance.rows() ||
        productCovariance.rows() != productCovariance.cols())
    {
        return {};
    }
    return transform * productCovariance * transform.transpose();
}
