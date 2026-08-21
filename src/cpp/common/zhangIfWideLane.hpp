#pragma once

#include <algorithm>
#include <deque>
#include <map>
#include <set>
#include <string>
#include <vector>
#include "common/eigenIncluder.hpp"

struct ZhangIfWideLaneEstimate
{
    VectorXd mean;
    MatrixXd covariance;
    /** Linear sensitivity of every reported WL target to one stamped raw
     * noise scalar.  The key contains the epoch as well as the original
     * KF noise identity, so equal observation labels at different epochs are
     * never mistaken for one temporally common error. */
    std::map<std::string, VectorXd> noiseSensitivity;
    std::map<std::string, double> noiseVariance;
    int factorCount = 0;
    int informationRank = 0;
    bool valid = false;
    std::string failureReason = "NO_FACTORS";
};

/** Gauge-free fixed-lag estimator for physical satellite WL potentials.
 *
 * Every epoch contributes correlated satellite-difference observations.  The
 * accumulated node information is independent of the arbitrary reference
 * used to form those observations.  Factors carry physical arc versions and
 * are ignored after a true arc change; changing only the requested S-basis
 * reference never resets the window.
 */
class ZhangIfWideLaneAccumulator
{
public:
    struct Factor
    {
        double time = 0;
        std::vector<int> satellites;
        std::map<int, int> arcVersions;
        MatrixXd design;
        VectorXd observed;
        MatrixXd covariance;
        std::vector<std::string> noiseKeys;
        VectorXd noiseVariances;
        MatrixXd noiseDesign;
    };

    explicit ZhangIfWideLaneAccumulator(
        double lagSeconds = 3600,
        int maximumFactors = 360,
        int maximumSatellite = 64)
        : lagSeconds_(lagSeconds),
          maximumFactors_(maximumFactors),
          nodeCount_(maximumSatellite + 1)
    {
    }

    void setArcVersion(int satellite, int version)
    {
        arcVersions_[satellite] = version;
    }

    int arcVersion(int satellite) const
    {
        auto found = arcVersions_.find(satellite);
        return found == arcVersions_.end() ? 0 : found->second;
    }

    void addEpoch(
        double time,
        const std::vector<int>& satellites,
        const VectorXd& values,
        const MatrixXd& covariance,
        const std::vector<std::string>& noiseKeys = {},
        const VectorXd& noiseVariances = {},
        const MatrixXd& satelliteNoiseDesign = {})
    {
        if (satellites.size() < 2 || values.size() != satellites.size() ||
            covariance.rows() != values.size() ||
            covariance.cols() != values.size() ||
            !values.allFinite() || !covariance.allFinite())
        {
            return;
        }
        const int reference = 0;
        Factor factor;
        factor.time = time;
        factor.satellites = satellites;
        if (*std::max_element(satellites.begin(), satellites.end()) >= nodeCount_)
        {
            return;
        }
        factor.design = MatrixXd::Zero(satellites.size() - 1, nodeCount_);
        MatrixXd difference = MatrixXd::Zero(
            satellites.size() - 1, satellites.size());
        for (int row = 0; row < difference.rows(); row++)
        {
            difference(row, row + 1) = +1;
            difference(row, reference) = -1;
            factor.design(row, satellites[row + 1]) = +1;
            factor.design(row, satellites[reference]) = -1;
        }
        factor.observed = difference * values;
        factor.covariance = difference * covariance * difference.transpose();
        const bool noiseFactorDimensionsValid =
            noiseKeys.size() == static_cast<std::size_t>(noiseVariances.size()) &&
            satelliteNoiseDesign.rows() == values.size() &&
            satelliteNoiseDesign.cols() == noiseVariances.size();
        if (noiseFactorDimensionsValid && !noiseKeys.empty())
        {
            factor.noiseKeys = noiseKeys;
            factor.noiseVariances = noiseVariances;
            factor.noiseDesign = difference * satelliteNoiseDesign;
        }
        for (int satellite : satellites)
        {
            factor.arcVersions[satellite] = arcVersion(satellite);
        }
        factors_.push_back(std::move(factor));
        prune(time);
    }

    ZhangIfWideLaneEstimate estimate(
        const std::vector<int>& satellites,
        int referenceSatellite,
        double time)
    {
        prune(time);
        ZhangIfWideLaneEstimate result;
        if (satellites.size() < 2 ||
            std::find(satellites.begin(), satellites.end(), referenceSatellite) ==
                satellites.end())
        {
            result.failureReason = "INVALID_TARGET";
            return result;
        }
        const int nodes = nodeCount_;
        MatrixXd information = MatrixXd::Zero(nodes, nodes);
        VectorXd rhs = VectorXd::Zero(nodes);
        for (const Factor& factor : factors_)
        {
            std::vector<int> validRows;
            if (!factor.satellites.empty())
            {
                const int referenceSatellite = factor.satellites.front();
                const bool referenceValid =
                    factor.arcVersions.count(referenceSatellite) &&
                    arcVersion(referenceSatellite) ==
                        factor.arcVersions.at(referenceSatellite);
                for (int row = 0; referenceValid && row < factor.design.rows(); row++)
                {
                    const int targetSatellite = factor.satellites[row + 1];
                    if (factor.arcVersions.count(targetSatellite) &&
                        arcVersion(targetSatellite) ==
                            factor.arcVersions.at(targetSatellite))
                    {
                        validRows.push_back(row);
                    }
                }
            }
            if (validRows.empty() || factor.design.cols() != nodes)
            {
                continue;
            }
            MatrixXd validDesign(validRows.size(), nodes);
            VectorXd validObserved(validRows.size());
            MatrixXd validCovariance(validRows.size(), validRows.size());
            for (int row = 0; row < static_cast<int>(validRows.size()); row++)
            {
                validDesign.row(row) = factor.design.row(validRows[row]);
                validObserved(row) = factor.observed(validRows[row]);
                for (int column = 0;
                     column < static_cast<int>(validRows.size()); column++)
                {
                    validCovariance(row, column) =
                        factor.covariance(validRows[row], validRows[column]);
                }
            }
            Eigen::SelfAdjointEigenSolver<MatrixXd> covarianceSolver(
                0.5 * (validCovariance + validCovariance.transpose()));
            if (covarianceSolver.info() != Eigen::Success ||
                !covarianceSolver.eigenvalues().allFinite())
            {
                continue;
            }
            const double maximum = covarianceSolver.eigenvalues().maxCoeff();
            const double tolerance = std::max(1e-14, maximum * 1e-12);
            if (!(maximum > 0) || covarianceSolver.eigenvalues().minCoeff() <= 0)
            {
                continue;
            }
            VectorXd inverseEigenvalues = covarianceSolver.eigenvalues();
            for (int index = 0; index < inverseEigenvalues.size(); index++)
            {
                inverseEigenvalues(index) = inverseEigenvalues(index) > tolerance
                    ? 1 / inverseEigenvalues(index) : 0;
            }
            const MatrixXd inverse = covarianceSolver.eigenvectors() *
                inverseEigenvalues.asDiagonal() *
                covarianceSolver.eigenvectors().transpose();
            information += validDesign.transpose() * inverse * validDesign;
            rhs += validDesign.transpose() * inverse * validObserved;
            result.factorCount++;
        }
        if (result.factorCount == 0)
        {
            return result;
        }
        Eigen::SelfAdjointEigenSolver<MatrixXd> informationSolver(
            0.5 * (information + information.transpose()));
        if (informationSolver.info() != Eigen::Success ||
            !informationSolver.eigenvalues().allFinite())
        {
            result.failureReason = "INFORMATION_EIGENSOLVER_FAILURE";
            return result;
        }
        const double maximum = informationSolver.eigenvalues().maxCoeff();
        const double tolerance = std::max(1e-14, maximum * 1e-12);
        VectorXd inverseEigenvalues = VectorXd::Zero(
            informationSolver.eigenvalues().size());
        for (int index = 0; index < inverseEigenvalues.size(); index++)
        {
            if (informationSolver.eigenvalues()(index) > tolerance)
            {
                inverseEigenvalues(index) =
                    1 / informationSolver.eigenvalues()(index);
                result.informationRank++;
            }
        }
        const MatrixXd pseudoInverse = informationSolver.eigenvectors() *
            inverseEigenvalues.asDiagonal() *
            informationSolver.eigenvectors().transpose();
        const VectorXd potentials = pseudoInverse * rhs;
        MatrixXd target = MatrixXd::Zero(satellites.size() - 1, nodes);
        int row = 0;
        for (int satellite : satellites)
        {
            if (satellite == referenceSatellite)
            {
                continue;
            }
            target(row, satellite) = +1;
            target(row, referenceSatellite) = -1;
            row++;
        }
        const MatrixXd observableProjector = pseudoInverse * information;
        const double unobservableNorm =
            (target * (MatrixXd::Identity(nodes, nodes) -
                       observableProjector)).norm();
        if (unobservableNorm > 1e-8)
        {
            result.failureReason = "TARGET_UNOBSERVABLE";
            return result;
        }
        result.mean = target * potentials;
        result.covariance = target * pseudoInverse * target.transpose();
        for (const Factor& factor : factors_)
        {
            std::vector<int> validRows;
            if (!factor.satellites.empty())
            {
                const int referenceSatellite = factor.satellites.front();
                const bool referenceValid =
                    factor.arcVersions.count(referenceSatellite) &&
                    arcVersion(referenceSatellite) ==
                        factor.arcVersions.at(referenceSatellite);
                for (int row = 0; referenceValid && row < factor.design.rows(); row++)
                {
                    const int targetSatellite = factor.satellites[row + 1];
                    if (factor.arcVersions.count(targetSatellite) &&
                        arcVersion(targetSatellite) ==
                            factor.arcVersions.at(targetSatellite))
                    {
                        validRows.push_back(row);
                    }
                }
            }
            if (validRows.empty() || factor.noiseKeys.empty() ||
                factor.noiseDesign.rows() != factor.design.rows() ||
                factor.noiseDesign.cols() !=
                    static_cast<int>(factor.noiseKeys.size()))
            {
                continue;
            }
            MatrixXd validDesign(validRows.size(), nodes);
            MatrixXd validCovariance(validRows.size(), validRows.size());
            MatrixXd validNoiseDesign(
                validRows.size(), factor.noiseDesign.cols());
            for (int row = 0; row < static_cast<int>(validRows.size()); row++)
            {
                validDesign.row(row) = factor.design.row(validRows[row]);
                validNoiseDesign.row(row) =
                    factor.noiseDesign.row(validRows[row]);
                for (int column = 0;
                     column < static_cast<int>(validRows.size()); column++)
                {
                    validCovariance(row, column) =
                        factor.covariance(validRows[row], validRows[column]);
                }
            }
            Eigen::SelfAdjointEigenSolver<MatrixXd> covarianceSolver(
                0.5 * (validCovariance + validCovariance.transpose()));
            if (covarianceSolver.info() != Eigen::Success ||
                !covarianceSolver.eigenvalues().allFinite())
            {
                continue;
            }
            const double factorMaximum =
                covarianceSolver.eigenvalues().maxCoeff();
            const double factorTolerance =
                std::max(1e-14, factorMaximum * 1e-12);
            if (!(factorMaximum > 0) ||
                covarianceSolver.eigenvalues().minCoeff() <= 0)
            {
                continue;
            }
            VectorXd inverseEigenvalues = covarianceSolver.eigenvalues();
            for (int index = 0; index < inverseEigenvalues.size(); index++)
            {
                inverseEigenvalues(index) =
                    inverseEigenvalues(index) > factorTolerance
                        ? 1 / inverseEigenvalues(index) : 0;
            }
            const MatrixXd inverse = covarianceSolver.eigenvectors() *
                inverseEigenvalues.asDiagonal() *
                covarianceSolver.eigenvectors().transpose();
            const MatrixXd factorSensitivity = target * pseudoInverse *
                validDesign.transpose() * inverse * validNoiseDesign;
            for (int column = 0;
                 column < static_cast<int>(factor.noiseKeys.size()); column++)
            {
                const std::string& key = factor.noiseKeys[column];
                if (result.noiseSensitivity.count(key) == 0)
                {
                    result.noiseSensitivity[key] =
                        VectorXd::Zero(target.rows());
                }
                result.noiseSensitivity[key] += factorSensitivity.col(column);
                result.noiseVariance[key] = factor.noiseVariances(column);
            }
        }
        result.valid = result.mean.allFinite() && result.covariance.allFinite() &&
            (result.covariance.diagonal().array() >= -1e-12).all();
        result.failureReason = result.valid ? "NONE" : "TARGET_NONFINITE";
        return result;
    }

    std::size_t factorCount() const { return factors_.size(); }

private:
    void prune(double time)
    {
        while (!factors_.empty() &&
               (time - factors_.front().time > lagSeconds_ ||
                static_cast<int>(factors_.size()) > maximumFactors_))
        {
            factors_.pop_front();
        }
    }

    double lagSeconds_ = 3600;
    int maximumFactors_ = 360;
    int nodeCount_ = 65;
    std::map<int, int> arcVersions_;
    std::deque<Factor> factors_;
};
