#pragma once

#include <algorithm>
#include <cmath>
#include <functional>
#include <limits>
#include <set>
#include <string>
#include <vector>

#include <Eigen/Dense>
#include <boost/math/distributions/chi_squared.hpp>

#include "common/zhangQuotientIntegerLattice.hpp"

/** Reliability-first integer-constrained product-gain audit.
 *
 * The primitive row pool is exhaustive inside |a_i| <= coefficientBound.
 * Rank one is therefore an exact bounded-coefficient optimum.  Higher ranks
 * use a bounded beam over that exhaustive row pool and are deliberately
 * reported as lower bounds, never as G_k^Z.
 */
struct ZhangIntegerGainCandidate
{
	ZhangExactVector row;
	double failureProbability = 1;
	double gain = 0;
};

struct ZhangIntegerGainFrontierPoint
{
	int rank = 0;
	double gain = 0;
	double gainFraction = 0;
	double failureProbabilityBound = 1;
	double jointNis = std::numeric_limits<double>::quiet_NaN();
	double jointNisThreshold = std::numeric_limits<double>::quiet_NaN();
	ZhangExactMatrix rows;
	bool reliable = false;
	bool exactBoundedOptimum = false;
};

struct ZhangIntegerGainFrontier
{
	int dimension = 0;
	int coefficientBound = 0;
	int maximumEnumerationSupport = 0;
	std::size_t enumeratedPrimitiveRows = 0;
	std::size_t reliablePrimitiveRows = 0;
	std::size_t explicitSeedRowsAdded = 0;
	std::size_t reliableExplicitSeedRows = 0;
	double totalProductVariance = 0;
	std::vector<ZhangIntegerGainFrontierPoint> points;
	bool valid = false;
	std::string status;
};

inline Eigen::VectorXd zhangExactRowToDouble(const ZhangExactVector& row)
{
	Eigen::VectorXd result(row.size());
	for (int index = 0; index < static_cast<int>(row.size()); index++)
		result(index) = row[index].convert_to<double>();
	return result;
}

inline bool zhangCanonicalPrimitiveIntegerRow(ZhangExactVector& row)
{
	auto exactGcd = [](ZhangExactInteger left, ZhangExactInteger right)
	{
		left = zhangExactAbs(left);
		right = zhangExactAbs(right);
		while (right != 0)
		{
			const ZhangExactInteger remainder = left % right;
			left = right;
			right = remainder;
		}
		return left;
	};
	ZhangExactInteger divisor = 0;
	int first = -1;
	for (int index = 0; index < static_cast<int>(row.size()); index++)
	{
		if (row[index] != 0 && first < 0) first = index;
		divisor = exactGcd(divisor, row[index]);
	}
	if (first < 0 || zhangExactAbs(divisor) != 1) return false;
	if (row[first] < 0)
		for (auto& value : row) value = -value;
	return true;
}

inline double zhangIntegerConstraintProductGain(
	const Eigen::MatrixXd& rows,
	const Eigen::MatrixXd& covariance,
	const Eigen::MatrixXd& productQuotientCrossCovariance)
{
	if (rows.rows() == 0) return 0;
	const Eigen::MatrixXd constraintCovariance = rows * covariance * rows.transpose();
	Eigen::CompleteOrthogonalDecomposition<Eigen::MatrixXd> decomposition(
		constraintCovariance);
	if (decomposition.rank() != rows.rows()) return -1;
	const Eigen::MatrixXd cross =
		productQuotientCrossCovariance * rows.transpose();
	const Eigen::MatrixXd solved = decomposition.solve(cross.transpose());
	return std::max(0.0, (cross * solved).trace());
}

inline double zhangIntegerRoundFailureProbability(double fractional, double variance)
{
	if (!(variance > 0) || !std::isfinite(variance))
		return std::abs(fractional) <= 1e-10 ? 0 : 1;
	const double sigma = std::sqrt(variance);
	const double lower = (-0.5 - fractional) / sigma;
	const double upper = ( 0.5 - fractional) / sigma;
	auto normalCdf = [](double value)
	{
		return 0.5 * std::erfc(-value / std::sqrt(2.0));
	};
	return std::clamp(1 - (normalCdf(upper) - normalCdf(lower)), 0.0, 1.0);
}

inline ZhangIntegerGainFrontier zhangBoundedIntegerProductGainFrontier(
	const Eigen::VectorXd& mean,
	const Eigen::MatrixXd& covariance,
	const Eigen::MatrixXd& productQuotientCrossCovariance,
	int coefficientBound,
	double maximumFailureProbability,
	double nisAlpha,
	int maximumRank,
	std::size_t beamWidth = 128,
	double totalProductVarianceOverride = 0,
	const ZhangExactMatrix& explicitSeedRows = {},
	int maximumEnumerationSupport = 0)
{
	ZhangIntegerGainFrontier result;
	result.dimension = mean.size();
	result.coefficientBound = coefficientBound;
	result.maximumEnumerationSupport = maximumEnumerationSupport <= 0
		? result.dimension
		: std::min(maximumEnumerationSupport, result.dimension);
	if (mean.size() <= 0 || covariance.rows() != mean.size() ||
		covariance.cols() != mean.size() ||
		productQuotientCrossCovariance.cols() != mean.size() ||
		coefficientBound < 1 || maximumRank < 1 || beamWidth < 1)
	{
		result.status = "INVALID_DIMENSION";
		return result;
	}
	const Eigen::MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
	result.totalProductVariance = totalProductVarianceOverride > 0
		? totalProductVarianceOverride
		: productQuotientCrossCovariance.squaredNorm();
	if (!(result.totalProductVariance > 0))
	{
		result.status = "NONPOSITIVE_PRODUCT_VARIANCE";
		return result;
	}

	std::vector<ZhangIntegerGainCandidate> candidates;
	std::set<ZhangExactVector> candidateRows;
	boost::math::chi_squared scalarDistribution(1);
	const double scalarNisThreshold = boost::math::quantile(
		boost::math::complement(scalarDistribution, nisAlpha));
	auto evaluate = [&](ZhangExactVector candidate, bool explicitSeed)
	{
		if (!zhangCanonicalPrimitiveIntegerRow(candidate)) return;
		if (!candidateRows.insert(candidate).second) return;
		if (explicitSeed) result.explicitSeedRowsAdded++;
		const Eigen::VectorXd numeric = zhangExactRowToDouble(candidate);
		const double variance = (numeric.transpose() * symmetric * numeric)(0, 0);
		const double floating = numeric.dot(mean);
		const double fractional = floating - std::round(floating);
		const double failure = zhangIntegerRoundFailureProbability(fractional, variance);
		const double nis = variance > 0
			? fractional * fractional / variance
			: std::numeric_limits<double>::infinity();
		if (failure > maximumFailureProbability || nis > scalarNisThreshold) return;
		Eigen::MatrixXd oneRow(1, mean.size()); oneRow.row(0) = numeric.transpose();
		const double gain = zhangIntegerConstraintProductGain(
			oneRow, symmetric, productQuotientCrossCovariance);
		if (gain >= 0)
		{
			candidates.push_back({candidate, failure, gain});
			if (explicitSeed) result.reliableExplicitSeedRows++;
		}
	};
	ZhangExactVector row(mean.size());
	std::function<void(int, int)> enumerate = [&](int column, int nonzeroSupport)
	{
		if (column < mean.size())
		{
			for (int value = -coefficientBound; value <= coefficientBound; value++)
			{
				const int nextSupport = nonzeroSupport + (value != 0);
				if (nextSupport > result.maximumEnumerationSupport) continue;
				row[column] = value;
				enumerate(column + 1, nextSupport);
			}
			return;
		}
		ZhangExactVector canonical = row;
		if (!zhangCanonicalPrimitiveIntegerRow(canonical) || canonical != row) return;
		result.enumeratedPrimitiveRows++;
		evaluate(row, false);
	};
	enumerate(0, 0);
	for (const auto& seed : explicitSeedRows)
	{
		if (seed.size() == static_cast<size_t>(mean.size())) evaluate(seed, true);
	}
	result.reliablePrimitiveRows = candidates.size();
	std::sort(candidates.begin(), candidates.end(), [](const auto& left, const auto& right)
	{
		if (left.gain != right.gain) return left.gain > right.gain;
		return left.failureProbability < right.failureProbability;
	});
	if (candidates.empty())
	{
		result.status = "NO_RELIABLE_PRIMITIVE_ROW";
		result.valid = true;
		return result;
	}
	// Rank one used the complete primitive pool.  Higher-rank exploration is
	// deliberately bounded and therefore only supplies a reliable lower bound.
	const std::size_t higherRankPoolSize = std::min<std::size_t>(256, candidates.size());

	ZhangIntegerGainFrontierPoint rankOne;
	rankOne.rank = 1;
	rankOne.gain = candidates.front().gain;
	rankOne.gainFraction = rankOne.gain / result.totalProductVariance;
	rankOne.failureProbabilityBound = candidates.front().failureProbability;
	const Eigen::VectorXd rankOneNumeric = zhangExactRowToDouble(candidates.front().row);
	const double rankOneFloat = rankOneNumeric.dot(mean);
	const double rankOneResidual = std::round(rankOneFloat) - rankOneFloat;
	const double rankOneVariance =
		(rankOneNumeric.transpose() * symmetric * rankOneNumeric)(0, 0);
	rankOne.jointNis = rankOneResidual * rankOneResidual / rankOneVariance;
	rankOne.jointNisThreshold = scalarNisThreshold;
	rankOne.rows = {candidates.front().row};
	rankOne.reliable = rankOne.jointNis <= rankOne.jointNisThreshold;
	rankOne.exactBoundedOptimum = true;
	if (rankOne.reliable) result.points.push_back(rankOne);

	struct Branch { ZhangExactMatrix rows; };
	std::vector<Branch> beam;
	for (std::size_t index = 0; index < higherRankPoolSize; index++)
		beam.push_back({{candidates[index].row}});
	bool beamIsExhaustive = candidates.size() <= higherRankPoolSize;
	for (int targetRank = 2;
		 targetRank <= std::min(maximumRank, result.dimension); targetRank++)
	{
		struct Evaluated { Branch branch; ZhangIntegerGainFrontierPoint point; };
		std::vector<Evaluated> expanded;
		for (const auto& branch : beam)
		for (std::size_t candidateIndex = 0;
			 candidateIndex < higherRankPoolSize;
			 candidateIndex++)
		{
			const auto& candidate = candidates[candidateIndex];
			if (!branch.rows.empty() && !(branch.rows.back() < candidate.row)) continue;
			ZhangExactMatrix rows = branch.rows; rows.push_back(candidate.row);
			int exactRank = 0;
			if (!zhangExactPrimitiveRowLattice(rows, result.dimension, &exactRank) ||
				exactRank != targetRank) continue;
			Eigen::MatrixXd numeric(targetRank, result.dimension);
			for (int r = 0; r < targetRank; r++)
				numeric.row(r) = zhangExactRowToDouble(rows[r]).transpose();
			const Eigen::MatrixXd constraintCovariance = numeric * symmetric * numeric.transpose();
			Eigen::CompleteOrthogonalDecomposition<Eigen::MatrixXd> decomposition(
				constraintCovariance);
			if (decomposition.rank() != targetRank) continue;
			const Eigen::VectorXd floating = numeric * mean;
			const Eigen::VectorXd residual = floating.array().round().matrix() - floating;
			const double nis = residual.dot(decomposition.solve(residual));
			boost::math::chi_squared distribution(targetRank);
			const double threshold = boost::math::quantile(
				boost::math::complement(distribution, nisAlpha));
			double failureBound = 0;
			for (int r = 0; r < targetRank; r++)
			{
				const double variance = constraintCovariance(r, r);
				const double fractional = floating(r) - std::round(floating(r));
				failureBound += zhangIntegerRoundFailureProbability(fractional, variance);
			}
			if (failureBound > maximumFailureProbability || nis > threshold) continue;
			const double gain = zhangIntegerConstraintProductGain(
				numeric, symmetric, productQuotientCrossCovariance);
			ZhangIntegerGainFrontierPoint point;
			point.rank = targetRank;
			point.gain = gain;
			point.gainFraction = gain / result.totalProductVariance;
			point.failureProbabilityBound = failureBound;
			point.jointNis = nis;
			point.jointNisThreshold = threshold;
			point.rows = rows;
			point.reliable = true;
			point.exactBoundedOptimum = targetRank == 1;
			expanded.push_back({{rows}, point});
		}
		std::sort(expanded.begin(), expanded.end(), [](const auto& left, const auto& right)
		{
			if (left.point.gain != right.point.gain)
				return left.point.gain > right.point.gain;
			return left.point.failureProbabilityBound < right.point.failureProbabilityBound;
		});
		if (expanded.empty()) break;
		expanded.front().point.exactBoundedOptimum = beamIsExhaustive;
		result.points.push_back(expanded.front().point);
		const bool truncated = expanded.size() > beamWidth ||
			candidates.size() > higherRankPoolSize;
		beam.clear();
		for (std::size_t index = 0; index < std::min(beamWidth, expanded.size()); index++)
			beam.push_back(std::move(expanded[index].branch));
		beamIsExhaustive = beamIsExhaustive && !truncated;
	}
	result.valid = true;
	result.status = result.points.empty() ? "NO_RELIABLE_BLOCK" : "COMPLETE";
	return result;
}
