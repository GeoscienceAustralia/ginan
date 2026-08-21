#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"

using ZhangIarFunctional = Eigen::SparseMatrix<double, Eigen::RowMajor>;

inline bool zhangIarSparseAllFinite(const ZhangIarFunctional& matrix)
{
    for (int row = 0; row < matrix.outerSize(); row++)
    {
        for (ZhangIarFunctional::InnerIterator entry(matrix, row);
             entry; ++entry)
        {
            if (!std::isfinite(entry.value()))
            {
                return false;
            }
        }
    }
    return true;
}

/** Low-rank representation of an exact zero-noise covariance conditioning.
 *
 * For constraints A x = integer and prior covariance P, factor satisfies
 *
 *   P_after = P - factor factor'.
 *
 * Only the covariance is represented; no integer value or state mean is
 * required for the E24a information-flow shadow audit.
 */
struct ZhangIarCovarianceCondition
{
    bool     valid = false;
    int      inputRows = 0;
    int      rank = 0;
    double   minimumRetainedEigenvalue =
        std::numeric_limits<double>::quiet_NaN();
    double   maximumEigenvalue =
        std::numeric_limits<double>::quiet_NaN();
    MatrixXd reductionFactor;
};

/** Product-weighted real-mode ceiling for an integer covariance block.
 *
 * For integer coordinates a and product coordinates q, this is the spectrum
 * of
 *
 *   Qaa^(-1/2) Qaq Wq Qqa Qaa^(-1/2).
 *
 * It is a diagnostic upper bound over arbitrary real directions.  It does not
 * claim that the modes are integer-valued or fixable and never authorises AR.
 */
struct ZhangIarProductGainSpectrum
{
    bool        valid = false;
    int         ambiguityDimension = 0;
    int         ambiguityRank = 0;
    int         productDimension = 0;
    double      minimumRetainedAmbiguityEigenvalue =
        std::numeric_limits<double>::quiet_NaN();
    double      maximumAmbiguityEigenvalue =
        std::numeric_limits<double>::quiet_NaN();
    double      totalWeightedGain =
        std::numeric_limits<double>::quiet_NaN();
    VectorXd    eigenvaluesDescending;
    std::string failureReason;

    double rho(int requestedRank) const
    {
        if (!valid || requestedRank < 0 ||
            !(totalWeightedGain > 0) ||
            !std::isfinite(totalWeightedGain))
        {
            return std::numeric_limits<double>::quiet_NaN();
        }
        const int retained = std::min(
            requestedRank,
            static_cast<int>(eigenvaluesDescending.size()));
        return eigenvaluesDescending.head(retained).sum() /
            totalWeightedGain;
    }

    int minimumRankForRho(double requestedCoverage) const
    {
        if (!valid || !(totalWeightedGain > 0) ||
            !std::isfinite(totalWeightedGain) ||
            !std::isfinite(requestedCoverage) ||
            requestedCoverage <= 0 || requestedCoverage > 1)
        {
            return 0;
        }
        double cumulative = 0;
        for (int index = 0; index < eigenvaluesDescending.size(); index++)
        {
            cumulative += eigenvaluesDescending(index);
            if (cumulative / totalWeightedGain >= requestedCoverage)
            {
                return index + 1;
            }
        }
        return eigenvaluesDescending.size();
    }
};

inline ZhangIarProductGainSpectrum zhangIarProductGainSpectrum(
    const MatrixXd& ambiguityCovariance,
    const MatrixXd& ambiguityProductCrossCovariance,
    const MatrixXd& productWeight
)
{
    ZhangIarProductGainSpectrum result;
    result.ambiguityDimension = ambiguityCovariance.rows();
    result.productDimension = productWeight.rows();
    if (ambiguityCovariance.rows() == 0 ||
        ambiguityCovariance.rows() != ambiguityCovariance.cols() ||
        ambiguityProductCrossCovariance.rows() != ambiguityCovariance.rows() ||
        ambiguityProductCrossCovariance.cols() != productWeight.rows() ||
        productWeight.rows() != productWeight.cols() ||
        !ambiguityCovariance.allFinite() ||
        !ambiguityProductCrossCovariance.allFinite() ||
        !productWeight.allFinite())
    {
        result.failureReason = "DIMENSION_OR_FINITE_CHECK_FAILED";
        return result;
    }

    const MatrixXd symmetricAmbiguity = 0.5 *
        (ambiguityCovariance + ambiguityCovariance.transpose());
    const MatrixXd symmetricWeight = 0.5 *
        (productWeight + productWeight.transpose());
    Eigen::SelfAdjointEigenSolver<MatrixXd> ambiguitySolver(
        symmetricAmbiguity);
    Eigen::SelfAdjointEigenSolver<MatrixXd> weightSolver(symmetricWeight);
    if (ambiguitySolver.info() != Eigen::Success ||
        weightSolver.info() != Eigen::Success ||
        !ambiguitySolver.eigenvalues().allFinite() ||
        !weightSolver.eigenvalues().allFinite())
    {
        result.failureReason = "EIGENSOLVER_FAILED";
        return result;
    }

    result.maximumAmbiguityEigenvalue =
        ambiguitySolver.eigenvalues().maxCoeff();
    const double ambiguityScale = std::max(
        1.0, std::abs(result.maximumAmbiguityEigenvalue));
    const double ambiguityNegativeTolerance = 1e-10 * ambiguityScale;
    const double weightScale = std::max(
        1.0, std::abs(weightSolver.eigenvalues().maxCoeff()));
    if (ambiguitySolver.eigenvalues().minCoeff() <
            -ambiguityNegativeTolerance ||
        weightSolver.eigenvalues().minCoeff() < -1e-10 * weightScale)
    {
        result.failureReason = "COVARIANCE_OR_WEIGHT_NOT_POSITIVE_SEMIDEFINITE";
        return result;
    }

    const double rankTolerance = std::max(
        1e-14,
        ambiguityScale * std::numeric_limits<double>::epsilon() *
            ambiguityCovariance.rows() * 32.0);
    std::vector<int> retained;
    for (int index = 0; index < ambiguitySolver.eigenvalues().size(); index++)
    {
        if (ambiguitySolver.eigenvalues()(index) > rankTolerance)
        {
            retained.push_back(index);
        }
    }
    result.ambiguityRank = retained.size();
    if (retained.empty())
    {
        result.failureReason = "ZERO_AMBIGUITY_RANK";
        return result;
    }

    MatrixXd retainedVectors(
        ambiguityCovariance.rows(), retained.size());
    MatrixXd inverseSquareRoot = MatrixXd::Zero(
        retained.size(), retained.size());
    for (int local = 0; local < static_cast<int>(retained.size()); local++)
    {
        const int index = retained[local];
        const double eigenvalue = ambiguitySolver.eigenvalues()(index);
        retainedVectors.col(local) = ambiguitySolver.eigenvectors().col(index);
        inverseSquareRoot(local, local) = 1 / std::sqrt(eigenvalue);
        result.minimumRetainedAmbiguityEigenvalue = std::isfinite(
            result.minimumRetainedAmbiguityEigenvalue)
                ? std::min(result.minimumRetainedAmbiguityEigenvalue, eigenvalue)
                : eigenvalue;
    }

    const MatrixXd retainedCross =
        retainedVectors.transpose() * ambiguityProductCrossCovariance;
    const MatrixXd nullspaceCross = ambiguityProductCrossCovariance -
        retainedVectors * retainedCross;
    const double crossScale = std::max(
        1.0, ambiguityProductCrossCovariance.norm());
    if (nullspaceCross.norm() > 1e-10 * crossScale)
    {
        result.failureReason = "CROSS_COVARIANCE_OUTSIDE_AMBIGUITY_RANGE";
        return result;
    }

    const MatrixXd whitenedCross = inverseSquareRoot * retainedCross;
    MatrixXd gain = whitenedCross * symmetricWeight *
        whitenedCross.transpose();
    gain = 0.5 * (gain + gain.transpose());
    Eigen::SelfAdjointEigenSolver<MatrixXd> gainSolver(gain);
    if (gainSolver.info() != Eigen::Success ||
        !gainSolver.eigenvalues().allFinite())
    {
        result.failureReason = "GAIN_EIGENSOLVER_FAILED";
        return result;
    }
    const double gainScale = std::max(
        1.0, std::abs(gainSolver.eigenvalues().maxCoeff()));
    if (gainSolver.eigenvalues().minCoeff() < -1e-10 * gainScale)
    {
        result.failureReason = "GAIN_MATRIX_NOT_POSITIVE_SEMIDEFINITE";
        return result;
    }

    result.eigenvaluesDescending.resize(gainSolver.eigenvalues().size());
    for (int index = 0; index < gainSolver.eigenvalues().size(); index++)
    {
        result.eigenvaluesDescending(index) = std::max(
            0.0,
            gainSolver.eigenvalues()(
                gainSolver.eigenvalues().size() - 1 - index));
    }
    result.totalWeightedGain = result.eigenvaluesDescending.sum();
    result.valid = result.eigenvaluesDescending.allFinite() &&
        std::isfinite(result.totalWeightedGain);
    if (!result.valid)
    {
        result.failureReason = "NONFINITE_GAIN_SPECTRUM";
    }
    return result;
}

inline ZhangIarCovarianceCondition zhangIarCovarianceCondition(
    const MatrixXd& covariance,
    const ZhangIarFunctional& constraintRows
)
{
    ZhangIarCovarianceCondition result;
    result.inputRows = constraintRows.rows();
    if (covariance.rows() == 0 ||
        covariance.rows() != covariance.cols() ||
        constraintRows.cols() != covariance.cols() ||
        !covariance.allFinite() ||
        !zhangIarSparseAllFinite(constraintRows))
    {
        return result;
    }
    if (constraintRows.rows() == 0)
    {
        result.reductionFactor = MatrixXd::Zero(covariance.rows(), 0);
        result.valid = true;
        return result;
    }

    const MatrixXd cross = covariance * constraintRows.transpose();
    MatrixXd constraintCovariance = constraintRows * cross;
    constraintCovariance = 0.5 *
        (constraintCovariance + constraintCovariance.transpose());
    Eigen::SelfAdjointEigenSolver<MatrixXd> solver(constraintCovariance);
    if (solver.info() != Eigen::Success || !solver.eigenvalues().allFinite())
    {
        return result;
    }
    result.maximumEigenvalue = solver.eigenvalues().maxCoeff();
    const double scale = std::max(1.0, std::abs(result.maximumEigenvalue));
    const double negativeTolerance = 1e-10 * scale;
    if (solver.eigenvalues().minCoeff() < -negativeTolerance)
    {
        return result;
    }
    const double rankTolerance = std::max(
        1e-14,
        scale * std::numeric_limits<double>::epsilon() *
            std::max(constraintCovariance.rows(),
                     constraintCovariance.cols()) * 32.0);
    std::vector<int> retained;
    for (int index = 0; index < solver.eigenvalues().size(); index++)
    {
        if (solver.eigenvalues()(index) > rankTolerance)
        {
            retained.push_back(index);
        }
    }
    result.rank = retained.size();
    result.reductionFactor = MatrixXd::Zero(
        covariance.rows(), result.rank);
    for (int local = 0; local < result.rank; local++)
    {
        const int eigenIndex = retained[local];
        const double eigenvalue = solver.eigenvalues()(eigenIndex);
        result.reductionFactor.col(local) =
            cross * solver.eigenvectors().col(eigenIndex) /
            std::sqrt(eigenvalue);
        result.minimumRetainedEigenvalue = std::isfinite(
            result.minimumRetainedEigenvalue)
                ? std::min(result.minimumRetainedEigenvalue, eigenvalue)
                : eigenvalue;
    }
    result.valid = result.reductionFactor.allFinite();
    return result;
}

inline double zhangIarProjectedCovarianceTrace(
    const MatrixXd& covariance,
    const ZhangIarCovarianceCondition& condition,
    const ZhangIarFunctional& functional
)
{
    if (!condition.valid || covariance.rows() != covariance.cols() ||
        functional.cols() != covariance.cols() ||
        condition.reductionFactor.rows() != covariance.rows() ||
        !zhangIarSparseAllFinite(functional))
    {
        return std::numeric_limits<double>::quiet_NaN();
    }
    const MatrixXd leftCovariance = functional * covariance;
    double trace = 0;
    for (int row = 0; row < functional.outerSize(); row++)
    {
        for (ZhangIarFunctional::InnerIterator entry(functional, row);
             entry; ++entry)
        {
            trace += entry.value() *
                leftCovariance(entry.row(), entry.col());
        }
    }
    const double unconditionedTrace = trace;
    if (condition.reductionFactor.cols() > 0)
    {
        trace -= (functional * condition.reductionFactor).squaredNorm();
    }
    const double scale = std::max(
        1.0,
        std::abs(unconditionedTrace));
    if (trace < 0 && trace >= -1e-10 * scale)
    {
        trace = 0;
    }
    return trace >= 0 && std::isfinite(trace)
        ? trace
        : std::numeric_limits<double>::quiet_NaN();
}

inline double zhangIarProjectedCovarianceTrace(
    const MatrixXd& covariance,
    const ZhangIarFunctional& functional
)
{
    ZhangIarCovarianceCondition identity;
    identity.valid = true;
    identity.reductionFactor = MatrixXd::Zero(covariance.rows(), 0);
    return zhangIarProjectedCovarianceTrace(
        covariance, identity, functional);
}
