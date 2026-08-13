#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
#include <numeric>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"
#include "common/zhangFullRank.hpp"
#include "common/zhangIarGainAudit.hpp"

/** Express one physical receiver/satellite rectangle in the current
 * fundamental-cycle coordinates.
 *
 * Each non-tree edge owns one fundamental cycle and occurs with coefficient
 * +1 only in its own basis row.  The non-tree entries of the requested
 * rectangle therefore determine its cycle-coordinate row uniquely.  The
 * final exact reconstruction check prevents a graph/S-basis mismatch from
 * silently entering the theory regression.
 */
inline bool zhangDdCycleCoordinateRow(
    const ZhangGraphBasis&              basis,
    const std::map<ZhangGraphEdge, int>& chordColumns,
    const std::string&                  referenceReceiver,
    const std::string&                  receiver,
    const SatSys&                       referenceSatellite,
    const SatSys&                       satellite,
    VectorXd&                           coordinateRow
)
{
    coordinateRow = VectorXd::Zero(chordColumns.size());
    if (referenceReceiver == receiver ||
        referenceSatellite == satellite || !basis.connected)
    {
        return false;
    }

    std::map<ZhangGraphEdge, int> physicalRow = {
        {{receiver,          satellite},          +1},
        {{receiver,          referenceSatellite}, -1},
        {{referenceReceiver, satellite},          -1},
        {{referenceReceiver, referenceSatellite}, +1}
    };
    for (const auto& [edge, coefficient] : physicalRow)
    {
        if (coefficient != 0 && basis.edges.count(edge) == 0)
        {
            return false;
        }
    }

    for (const auto& [edge, coefficient] : physicalRow)
    {
        if (coefficient == 0 || basis.isTreeEdge(
                edge.receiver, edge.satellite))
        {
            continue;
        }
        auto column = chordColumns.find(edge);
        if (column == chordColumns.end() || column->second < 0 ||
            column->second >= coordinateRow.size())
        {
            return false;
        }
        coordinateRow(column->second) = coefficient;
    }

    std::map<ZhangGraphEdge, int> reconstructed;
    for (const auto& [chord, column] : chordColumns)
    {
        const double rawMultiplier = coordinateRow(column);
        const int multiplier = static_cast<int>(std::llround(rawMultiplier));
        if (std::abs(rawMultiplier - multiplier) > 1e-12 || multiplier == 0)
        {
            continue;
        }
        const auto cycle = zhangFundamentalCycle(basis, chord);
        if (cycle.empty())
        {
            return false;
        }
        for (const auto& [edge, coefficient] : cycle)
        {
            reconstructed[edge] += multiplier * coefficient;
        }
    }
    for (auto it = reconstructed.begin(); it != reconstructed.end();)
    {
        if (it->second == 0)
        {
            it = reconstructed.erase(it);
        }
        else
        {
            ++it;
        }
    }
    return reconstructed == physicalRow;
}

struct ZhangPairedCorrelationSummary
{
    bool                valid = false;
    int                 pairs = 0;
    double              meanAbsolute =
        std::numeric_limits<double>::quiet_NaN();
    double              rmsAbsolute =
        std::numeric_limits<double>::quiet_NaN();
    double              medianAbsolute =
        std::numeric_limits<double>::quiet_NaN();
    double              pooledCorrelation =
        std::numeric_limits<double>::quiet_NaN();
    std::vector<double> coefficients;
};

/** Row-wise correlations Corr(A_i x, L_i x). */
inline ZhangPairedCorrelationSummary zhangPairedCorrelations(
    const MatrixXd&           covariance,
    const ZhangIarFunctional& ambiguityRows,
    const ZhangIarFunctional& targetRows
)
{
    ZhangPairedCorrelationSummary result;
    if (covariance.rows() == 0 || covariance.rows() != covariance.cols() ||
        ambiguityRows.rows() != targetRows.rows() ||
        ambiguityRows.cols() != covariance.cols() ||
        targetRows.cols() != covariance.cols() ||
        !covariance.allFinite() ||
        !zhangIarSparseAllFinite(ambiguityRows) ||
        !zhangIarSparseAllFinite(targetRows))
    {
        return result;
    }

    const MatrixXd ambiguityCovariance = ambiguityRows * covariance;
    const MatrixXd targetCovariance = targetRows * covariance;
    result.coefficients.reserve(ambiguityRows.rows());
    double ambiguityVarianceSum = 0;
    double targetVarianceSum = 0;
    double crossCovarianceSum = 0;
    for (int row = 0; row < ambiguityRows.rows(); row++)
    {
        double ambiguityVariance = 0;
        double targetVariance = 0;
        double crossCovariance = 0;
        for (ZhangIarFunctional::InnerIterator entry(ambiguityRows, row);
             entry; ++entry)
        {
            ambiguityVariance += entry.value() *
                ambiguityCovariance(row, entry.col());
            crossCovariance += entry.value() *
                targetCovariance(row, entry.col());
        }
        for (ZhangIarFunctional::InnerIterator entry(targetRows, row);
             entry; ++entry)
        {
            targetVariance += entry.value() *
                targetCovariance(row, entry.col());
        }
        if (!(ambiguityVariance > 0) || !(targetVariance > 0) ||
            !std::isfinite(crossCovariance))
        {
            return ZhangPairedCorrelationSummary{};
        }
        double correlation = crossCovariance /
            std::sqrt(ambiguityVariance * targetVariance);
        if (!std::isfinite(correlation) || std::abs(correlation) > 1 + 1e-8)
        {
            return ZhangPairedCorrelationSummary{};
        }
        result.coefficients.push_back(std::clamp(correlation, -1.0, 1.0));
        ambiguityVarianceSum += ambiguityVariance;
        targetVarianceSum += targetVariance;
        crossCovarianceSum += crossCovariance;
    }

    result.pairs = result.coefficients.size();
    if (result.pairs == 0)
    {
        return result;
    }
    std::vector<double> absolute;
    absolute.reserve(result.pairs);
    double squaredSum = 0;
    for (double coefficient : result.coefficients)
    {
        const double value = std::abs(coefficient);
        absolute.push_back(value);
        squaredSum += value * value;
    }
    std::sort(absolute.begin(), absolute.end());
    result.meanAbsolute = std::accumulate(
        absolute.begin(), absolute.end(), 0.0) / result.pairs;
    result.rmsAbsolute = std::sqrt(squaredSum / result.pairs);
    result.medianAbsolute = result.pairs % 2
        ? absolute[result.pairs / 2]
        : 0.5 * (absolute[result.pairs / 2 - 1] +
                 absolute[result.pairs / 2]);
    result.pooledCorrelation = crossCovarianceSum /
        std::sqrt(ambiguityVarianceSum * targetVarianceSum);
    result.valid = std::isfinite(result.pooledCorrelation) &&
        std::abs(result.pooledCorrelation) <= 1 + 1e-8;
    result.pooledCorrelation = std::clamp(
        result.pooledCorrelation, -1.0, 1.0);
    return result;
}
