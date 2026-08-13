#pragma once

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <limits>
#include <sstream>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"
#include "common/zhangIntegerAudit.hpp"

struct ZhangConstraintNisLeverage
{
	bool     valid = false;
	double   nis = 0;
	VectorXd deletionReduction;
};

inline double zhangLogErfc(double value)
{
	if (!(value >= 0) || !std::isfinite(value))
	{
		return 0;
	}
	if (value < 20)
	{
		const double probability = std::erfc(value);
		return probability > 0 ? std::log(probability)
			: -std::numeric_limits<double>::infinity();
	}
	const double inverseSquare = 1 / (value * value);
	const double correction = 1 - 0.5 * inverseSquare
		+ 0.75 * inverseSquare * inverseSquare
		- 1.875 * inverseSquare * inverseSquare * inverseSquare;
	constexpr double pi = 3.141592653589793238462643383279502884;
	return -value * value - std::log(value) - 0.5 * std::log(pi)
		+ std::log(correction);
}

/** Log of the bootstrap failure probability without saturation at success=1. */
inline double zhangBootstrapLogFailure(
	const VectorXd& conditionalVariances)
{
	double logFailure = -std::numeric_limits<double>::infinity();
	double logSurvival = 0;
	for (double variance : conditionalVariances)
	{
		if (!(variance > 0) || !std::isfinite(variance))
		{
			return 0;
		}
		const double logConditionalFailure = zhangLogErfc(
			std::sqrt(1 / (8 * variance)));
		const double failure = std::exp(logConditionalFailure);
		const double term = logSurvival + logConditionalFailure;
		if (!std::isfinite(logFailure))
		{
			logFailure = term;
		}
		else
		{
			const double maximum = std::max(logFailure, term);
			logFailure = maximum + std::log(
				std::exp(logFailure - maximum) + std::exp(term - maximum));
		}
		if (failure >= 1)
		{
			return 0;
		}
		logSurvival += std::log1p(-failure);
	}
	return logFailure;
}

/** Exact Schur-complement reduction of v' S^-1 v after deleting each row.
 *
 * For A=S^-1, deleting row i reduces the joint NIS by
 * ((A v)_i)^2 / A_ii.  LDLT solves are used instead of an explicit inverse.
 */
inline ZhangConstraintNisLeverage zhangConstraintNisLeverage(
	const VectorXd& innovation,
	const MatrixXd& covariance)
{
	ZhangConstraintNisLeverage result;
	const int dimension = innovation.size();
	result.deletionReduction = VectorXd::Zero(dimension);
	if (dimension == 0 || covariance.rows() != dimension ||
		covariance.cols() != dimension || !innovation.allFinite() ||
		!covariance.allFinite())
	{
		return result;
	}

	MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
	LDLT<MatrixXd> ldlt(symmetric);
	if (ldlt.info() != Eigen::Success || !ldlt.isPositive())
	{
		return result;
	}
	VectorXd inverseInnovation = ldlt.solve(innovation);
	MatrixXd inverse = ldlt.solve(MatrixXd::Identity(dimension, dimension));
	if (!inverseInnovation.allFinite() || !inverse.allFinite())
	{
		return result;
	}

	result.nis = innovation.dot(inverseInnovation);
	for (int index = 0; index < dimension; index++)
	{
		const double diagonal = inverse(index, index);
		if (!(diagonal > 0) || !std::isfinite(diagonal))
		{
			return ZhangConstraintNisLeverage{};
		}
		result.deletionReduction(index) =
			inverseInnovation(index) * inverseInnovation(index) / diagonal;
	}
	result.valid = std::isfinite(result.nis) &&
		result.deletionReduction.allFinite();
	return result;
}

/** Fraction of the weighted product variance removed by exact constraints.
 *
 * productCrossCovariance is Cov(p,a), ambiguityCovariance is Cov(a), and
 * integerRows contains H in H a = n.  The numerator is
 * tr(C H' (H P H')^+ H C').
 */
inline double zhangConstraintProductInformationGain(
	const MatrixXd& productCrossCovariance,
	double          productVarianceTrace,
	const MatrixXd& ambiguityCovariance,
	const MatrixXd& integerRows)
{
	if (!(productVarianceTrace > 0) || !std::isfinite(productVarianceTrace) ||
		integerRows.rows() == 0 ||
		integerRows.cols() != ambiguityCovariance.rows() ||
		ambiguityCovariance.rows() != ambiguityCovariance.cols() ||
		productCrossCovariance.cols() != ambiguityCovariance.rows() ||
		!productCrossCovariance.allFinite() ||
		!ambiguityCovariance.allFinite() || !integerRows.allFinite())
	{
		return std::numeric_limits<double>::quiet_NaN();
	}

	MatrixXd constraintCovariance = integerRows * ambiguityCovariance *
		integerRows.transpose();
	constraintCovariance = 0.5 *
		(constraintCovariance + constraintCovariance.transpose());
	Eigen::SelfAdjointEigenSolver<MatrixXd> eigenSolver(constraintCovariance);
	if (eigenSolver.info() != Eigen::Success ||
		!eigenSolver.eigenvalues().allFinite())
	{
		return std::numeric_limits<double>::quiet_NaN();
	}

	const double largest = eigenSolver.eigenvalues().maxCoeff();
	const double tolerance = std::max(1e-14, 1e-12 * std::max(0.0, largest));
	MatrixXd cross = productCrossCovariance * integerRows.transpose();
	double reductionTrace = 0;
	for (int mode = 0; mode < eigenSolver.eigenvalues().size(); mode++)
	{
		const double eigenvalue = eigenSolver.eigenvalues()(mode);
		if (eigenvalue <= tolerance)
		{
			continue;
		}
		VectorXd projected = cross * eigenSolver.eigenvectors().col(mode);
		reductionTrace += projected.squaredNorm() / eigenvalue;
	}
	if (!std::isfinite(reductionTrace))
	{
		return std::numeric_limits<double>::quiet_NaN();
	}
	return std::clamp(reductionTrace / productVarianceTrace, 0.0, 1.0);
}

inline std::string zhangIntegerRowHnfCanonicalKey(const MatrixXd& rows)
{
	if (rows.rows() == 0)
	{
		return "EMPTY";
	}
	ZhangExactMatrix exactRows = zhangExactZeroMatrix(rows.rows(), rows.cols());
	for (int row = 0; row < rows.rows(); row++)
	{
		for (int column = 0; column < rows.cols(); column++)
		{
			const double rounded = std::round(rows(row, column));
			if (!std::isfinite(rows(row, column)) ||
				std::abs(rows(row, column) - rounded) > 1e-7)
			{
				return "NON_INTEGER";
			}
			exactRows[row][column] = static_cast<long long>(rounded);
		}
	}
	ZhangExactRowHnf hnf = zhangExactRowHermiteNormalForm(std::move(exactRows));
	if (!hnf.consistent)
	{
		return "INCONSISTENT";
	}
	std::ostringstream key;
	key << hnf.basis.size() << "x"
		<< (hnf.basis.empty() ? 0 : hnf.basis.front().size()) << "|";
	for (const auto& row : hnf.basis)
	{
		for (const auto& value : row)
		{
			const std::string text = value.convert_to<std::string>();
			key << text.size() << ":" << text;
		}
		key << ";";
	}
	return key.str();
}

inline std::string zhangIntegerRowHnfFingerprint(const MatrixXd& rows)
{
	const std::string key = zhangIntegerRowHnfCanonicalKey(rows);
	if (key == "EMPTY" || key == "NON_INTEGER" || key == "INCONSISTENT")
	{
		return key;
	}
	std::uint64_t hash = zhangAuditFnv1a(1469598103934665603ULL, key);
	std::ostringstream stream;
	stream << std::hex << std::setw(16) << std::setfill('0') << hash;
	return stream.str();
}

/** Canonical affine integer-lattice identity for H a = n.
 *
 * The exact row-HNF transformation is applied to both H and n.  Two row bases
 * are therefore equal only when they describe the same integer sublattice and
 * the same integer coset, not merely the same homogeneous row space.
 */
inline std::string zhangIntegerAffineHnfCanonicalKey(
	const MatrixXd& rows,
	const VectorXd& values)
{
	if (rows.rows() == 0 || values.size() != rows.rows())
	{
		return "EMPTY_OR_DIMENSION_MISMATCH";
	}
	ZhangExactMatrix exactRows = zhangExactZeroMatrix(rows.rows(), rows.cols());
	ZhangExactVector exactValues(rows.rows());
	for (int row = 0; row < rows.rows(); row++)
	{
		const double roundedValue = std::round(values(row));
		if (!std::isfinite(values(row)) ||
			std::abs(values(row) - roundedValue) > 1e-7)
		{
			return "NON_INTEGER";
		}
		exactValues[row] = static_cast<long long>(roundedValue);
		for (int column = 0; column < rows.cols(); column++)
		{
			const double rounded = std::round(rows(row, column));
			if (!std::isfinite(rows(row, column)) ||
				std::abs(rows(row, column) - rounded) > 1e-7)
			{
				return "NON_INTEGER";
			}
			exactRows[row][column] = static_cast<long long>(rounded);
		}
	}
	ZhangExactRowHnf hnf = zhangExactRowHermiteNormalForm(
		std::move(exactRows), std::move(exactValues));
	if (!hnf.consistent)
	{
		return "INCONSISTENT";
	}
	std::ostringstream key;
	key << hnf.basis.size() << "x"
		<< (hnf.basis.empty() ? 0 : hnf.basis.front().size()) << "|";
	for (int row = 0; row < static_cast<int>(hnf.basis.size()); row++)
	{
		for (const auto& coefficient : hnf.basis[row])
		{
			const std::string text = coefficient.convert_to<std::string>();
			key << text.size() << ":" << text;
		}
		const std::string rhs = hnf.values[row].convert_to<std::string>();
		key << "=" << rhs.size() << ":" << rhs << ";";
	}
	return key.str();
}

inline std::string zhangIntegerAffineHnfFingerprint(
	const MatrixXd& rows,
	const VectorXd& values)
{
	const std::string key = zhangIntegerAffineHnfCanonicalKey(rows, values);
	if (key == "EMPTY_OR_DIMENSION_MISMATCH" || key == "NON_INTEGER" ||
		key == "INCONSISTENT")
	{
		return key;
	}
	std::uint64_t hash = zhangAuditFnv1a(1469598103934665603ULL, key);
	std::ostringstream stream;
	stream << std::hex << std::setw(16) << std::setfill('0') << hash;
	return stream.str();
}
