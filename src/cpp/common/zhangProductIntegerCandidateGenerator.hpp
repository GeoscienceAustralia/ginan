#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <Eigen/Dense>
#include <boost/math/distributions/chi_squared.hpp>

#include "common/zhangIntegerProductGainFrontier.hpp"

struct ProductIntegerCandidate
{
	ZhangExactVector row;
	double variance = std::numeric_limits<double>::quiet_NaN();
	double fractional = std::numeric_limits<double>::quiet_NaN();
	double perr = 1;
	double incrementalProductGain = 0;
	int pairGraphRankGain = 0;
	std::string source = "UNKNOWN";
	bool reliabilityPassed = false;
};

struct ProductIntegerCandidateGenerationResult
{
	std::vector<ProductIntegerCandidate> candidates;
	int allPairRows = 0;
	int reducedRows = 0;
	int realModeApproximations = 0;
	int reliableRows = 0;
	bool valid = false;
	std::string failureReason = "NOT_EVALUATED";
};

/** Convert a bootstrapped success probability into a conservative failure
 * bound without letting binary representation of a configured decimal
 * threshold (for example 1 - 0.999) spuriously exceed that same budget. */
inline double zhangProductFailureProbabilityBound(
	double bootstrapSuccess,
	double failureProbabilityBudget)
{
	if (!std::isfinite(bootstrapSuccess) || bootstrapSuccess <= 0)
		return 1;
	const double budget = std::clamp(failureProbabilityBudget, 0.0, 1.0);
	const double roundoff = 64 * std::numeric_limits<double>::epsilon() *
		std::max({1.0, std::abs(bootstrapSuccess), std::abs(budget)});
	if (bootstrapSuccess > 1 + roundoff) return 1;
	const double failure = std::max(0.0, 1 - bootstrapSuccess);
	// Only collapse the sub-ulp decimal tail at the configured boundary.  The
	// previous unconditional min(budget, failure) converted every genuinely
	// unsafe candidate (for example success=0.9) into an apparent pass.
	return failure <= budget + roundoff
		? std::min(failure, budget)
		: failure;
}

inline bool zhangProductFailureProbabilityPassed(
	double failureProbability,
	double failureProbabilityBudget)
{
	return std::isfinite(failureProbability) &&
		failureProbability <= failureProbabilityBudget + 1e-12;
}

inline bool zhangNormalisePrimitiveIntegerCandidate(ZhangExactVector& row)
{
	auto gcd = [](ZhangExactInteger left, ZhangExactInteger right)
	{
		left = zhangExactAbs(left);
		right = zhangExactAbs(right);
		while (right != 0)
		{
			const auto remainder = left % right;
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
		divisor = gcd(divisor, row[index]);
	}
	if (first < 0 || divisor == 0) return false;
	for (auto& coefficient : row) coefficient /= divisor;
	if (row[first] < 0)
		for (auto& coefficient : row) coefficient = -coefficient;
	return true;
}

/** A product-coordinate row is a named satellite pair only when it is one
 * reference edge (+/- one unit coordinate) or one exact +/-1 difference.
 * Merely having support two is insufficient: 2*z1-z2 is a mixed lattice row,
 * not a graph certificate. */
inline bool zhangProductCandidateIsNamedPairRow(
	const ZhangExactVector& row)
{
	int positive = 0;
	int negative = 0;
	int nonzero = 0;
	for (const auto& coefficient : row)
	{
		if (coefficient == 0) continue;
		nonzero++;
		if (coefficient == 1) positive++;
		else if (coefficient == -1) negative++;
		else return false;
	}
	return (nonzero == 1 && positive + negative == 1) ||
		(nonzero == 2 && positive == 1 && negative == 1);
}

/** Generate legal primitive rows in the product coordinate itself.
 *
 * Real product-gain modes are candidate-generation guides only.  Every
 * returned row is an exact primitive integer vector and is independently
 * evaluated with scalar Perr/NIS and Mahalanobis length.  No support-count
 * gate is applied to real-mode approximations. */
inline ProductIntegerCandidateGenerationResult
generateProductIntegerCandidates(
	const Eigen::VectorXd& mean,
	const Eigen::MatrixXd& covariance,
	const Eigen::MatrixXd& productCrossCovariance,
	double maximumPerr,
	double nisAlpha,
	const ZhangExactMatrix& reducedRows = {},
	int realModeCount = 8,
	int maximumApproximationScale = 12,
	std::size_t maximumCandidates = 512)
{
	ProductIntegerCandidateGenerationResult result;
	const int dimension = mean.size();
	if (dimension < 1 || covariance.rows() != dimension ||
		covariance.cols() != dimension ||
		productCrossCovariance.cols() != dimension || maximumPerr <= 0 ||
		maximumPerr >= 1 || nisAlpha <= 0 || nisAlpha >= 1 ||
		realModeCount < 0 || maximumApproximationScale < 1 ||
		maximumCandidates < 1)
	{
		result.failureReason = "PRODUCT_CANDIDATE_INPUT_INVALID";
		return result;
	}
	const Eigen::MatrixXd symmetric = 0.5 *
		(covariance + covariance.transpose());
	if (!symmetric.allFinite())
	{
		result.failureReason = "PRODUCT_CANDIDATE_COVARIANCE_NONFINITE";
		return result;
	}
	std::map<ZhangExactVector, std::string> rows;
	auto add = [&](ZhangExactVector row, const std::string& source)
	{
		if (row.size() != static_cast<std::size_t>(dimension) ||
			!zhangNormalisePrimitiveIntegerCandidate(row)) return;
		rows.try_emplace(std::move(row), source);
	};
	// Named star rows and every pair difference form the complete graphic seed
	// set, including the implicit reference node represented by the zero vector.
	for (int first = 0; first < dimension; first++)
	{
		ZhangExactVector unit(dimension);
		unit[first] = 1;
		add(unit, "ALL_PAIR_ROWS");
		result.allPairRows++;
		for (int second = first + 1; second < dimension; second++)
		{
			ZhangExactVector pair(dimension);
			pair[first] = 1;
			pair[second] = -1;
			add(pair, "ALL_PAIR_ROWS");
			result.allPairRows++;
		}
	}
	for (auto row : reducedRows)
	{
		add(std::move(row), "LAMBDA_REDUCED_ROWS");
		result.reducedRows++;
	}

	// M describes product information captured by one product integer row.
	const Eigen::MatrixXd information = productCrossCovariance.transpose() *
		productCrossCovariance;
	Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> qSolver(symmetric);
	if (qSolver.info() != Eigen::Success)
	{
		result.failureReason = "PRODUCT_CANDIDATE_COVARIANCE_EIGEN_FAILED";
		return result;
	}
	const double largestQ = std::max(1.0,
		qSolver.eigenvalues().cwiseAbs().maxCoeff());
	const double floor = 1e-12 * largestQ;
	Eigen::MatrixXd regularised = symmetric;
	regularised.diagonal().array() += floor;
	Eigen::GeneralizedSelfAdjointEigenSolver<Eigen::MatrixXd> gainSolver(
		0.5 * (information + information.transpose()), regularised);
	if (gainSolver.info() == Eigen::Success)
	{
		const int modes = std::min(realModeCount, dimension);
		for (int order = 0; order < modes; order++)
		{
			Eigen::VectorXd mode = gainSolver.eigenvectors().col(
				dimension - 1 - order);
			const double maximum = mode.cwiseAbs().maxCoeff();
			if (!(maximum > 0) || !std::isfinite(maximum)) continue;
			mode /= maximum;
			for (int scale = 1; scale <= maximumApproximationScale; scale++)
			{
				ZhangExactVector approximation(dimension);
				for (int index = 0; index < dimension; index++)
					approximation[index] = std::llround(scale * mode(index));
				const auto before = rows.size();
				add(std::move(approximation),
					"PRODUCT_GAIN_REAL_MODE_APPROXIMATION");
				result.realModeApproximations += rows.size() > before;
			}
		}
	}

	boost::math::chi_squared scalarDistribution(1);
	const double scalarThreshold = boost::math::quantile(
		boost::math::complement(scalarDistribution, nisAlpha));
	for (const auto& [row, source] : rows)
	{
		const Eigen::VectorXd numeric = zhangExactRowToDouble(row);
		const double variance =
			(numeric.transpose() * symmetric * numeric)(0, 0);
		const double floating = numeric.dot(mean);
		const double fractional = floating - std::round(floating);
		const double perr = zhangIntegerRoundFailureProbability(
			fractional, variance);
		const double nis = variance > 0
			? fractional * fractional / variance
			: std::numeric_limits<double>::infinity();
		Eigen::MatrixXd oneRow(1, dimension);
		oneRow.row(0) = numeric.transpose();
		const double gain = zhangIntegerConstraintProductGain(
			oneRow, symmetric, productCrossCovariance);
		ProductIntegerCandidate candidate;
		candidate.row = row;
		candidate.variance = variance;
		candidate.fractional = fractional;
		candidate.perr = perr;
		candidate.incrementalProductGain = std::max(0.0, gain);
		candidate.pairGraphRankGain =
			zhangProductCandidateIsNamedPairRow(row) ? 1 : 0;
		candidate.source = source;
		candidate.reliabilityPassed = std::isfinite(variance) && variance > 0 &&
			std::isfinite(perr) && perr <= maximumPerr && nis <= scalarThreshold;
		result.reliableRows += candidate.reliabilityPassed;
		result.candidates.push_back(std::move(candidate));
	}
	std::sort(result.candidates.begin(), result.candidates.end(),
		[](const auto& left, const auto& right)
		{
			if (left.reliabilityPassed != right.reliabilityPassed)
				return left.reliabilityPassed > right.reliabilityPassed;
			if (left.pairGraphRankGain != right.pairGraphRankGain)
				return left.pairGraphRankGain > right.pairGraphRankGain;
			if (left.incrementalProductGain != right.incrementalProductGain)
				return left.incrementalProductGain > right.incrementalProductGain;
			if (left.variance != right.variance)
				return left.variance < right.variance;
			return left.row < right.row;
		});
	if (result.candidates.size() > maximumCandidates)
		result.candidates.resize(maximumCandidates);
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}
