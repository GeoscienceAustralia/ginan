#pragma once

#include <algorithm>
#include <cmath>
#include <string>
#include <vector>

#include <Eigen/Dense>

#include "common/zhangIntegerAudit.hpp"

/** Exact target/held quotient audit in a shared physical-ambiguity ambient
 * coordinate.
 *
 * targetRows must be an integer basis of the product target lattice. heldRows
 * are already certified integer facts in the same physical-arc coordinate.
 * No covariance or small-variance decision is accepted as integer evidence.
 */
struct ZhangHeldQuotientAudit
{
	ZhangExactMatrix targetBasis;
	ZhangExactMatrix heldIntersectionPhysicalBasis;
	ZhangExactMatrix heldIntersectionTargetCoordinates;
	ZhangExactVector heldIntersectionValues;
	ZhangExactMatrix quotientTargetCoordinates;

	int targetRank = 0;
	int heldIntersectionRank = 0;
	int quotientRank = 0;
	bool heldIntersectionPrimitiveInTarget = false;
	bool exactClosure = false;
	bool valid = false;
	std::string failureReason;
};

struct ZhangCertifiedUnionAudit
{
	ZhangExactMatrix certifiedBasis;
	ZhangExactVector certifiedValues;
	int targetRank = 0;
	int heldRank = 0;
	int newlyFixedRank = 0;
	int combinedCertifiedRank = 0;
	bool targetContainedInCertified = false;
	bool certifiedContainedInTarget = false;
	bool exactTargetEquality = false;
	bool consistent = false;
	std::string failureReason;
};

struct ZhangDeterministicQuotientAudit
{
	int covarianceRank = 0;
	int nullity = 0;
	double maximumNullFractionalInteger = 0;
	bool covarianceValid = false;
	bool integerConsistent = true;
	std::string status;
};

/** Distinguish an untracked deterministic integer direction from an affine
 * contradiction.  Eigenvectors are used only to locate the real nullspace;
 * authorization still fails closed and never turns the mode into an integer
 * certificate. */
inline ZhangDeterministicQuotientAudit zhangAuditDeterministicQuotientModes(
	const Eigen::VectorXd& mean,
	const Eigen::MatrixXd& covariance,
	double relativeTolerance = 1e-12,
	double integerTolerance = 1e-8)
{
	ZhangDeterministicQuotientAudit result;
	if (mean.size() == 0 || covariance.rows() != mean.size() ||
		covariance.cols() != mean.size() || !mean.allFinite() ||
		!covariance.allFinite())
	{
		result.status = "INVALID_QUOTIENT_COVARIANCE";
		return result;
	}
	Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigen(
		0.5 * (covariance + covariance.transpose()));
	if (eigen.info() != Eigen::Success)
	{
		result.status = "QUOTIENT_EIGENSOLVER_FAILED";
		return result;
	}
	const double largest = std::max(0.0, eigen.eigenvalues().maxCoeff());
	const double tolerance = std::max(1e-14, relativeTolerance * largest);
	for (int mode = 0; mode < eigen.eigenvalues().size(); mode++)
	{
		if (eigen.eigenvalues()(mode) > tolerance)
		{
			result.covarianceRank++;
			continue;
		}
		result.nullity++;
	}
	// Eigenvectors have arbitrary real scale and cannot be treated as integer
	// functions.  A contradiction is asserted only for a canonical quotient
	// coordinate whose own variance is zero.  Non-axis-aligned null modes remain
	// UNTRACKED until an exact integer null row is recovered.
	bool canonicalContradiction = false;
	for (int coordinate = 0; coordinate < mean.size(); coordinate++)
	{
		if (std::abs(covariance(coordinate, coordinate)) > tolerance) continue;
		const double fractional = std::abs(
			mean(coordinate) - std::round(mean(coordinate)));
		result.maximumNullFractionalInteger = std::max(
			result.maximumNullFractionalInteger, fractional);
		canonicalContradiction |= fractional > integerTolerance;
	}
	result.covarianceValid = true;
	result.integerConsistent = !canonicalContradiction;
	result.status = result.nullity == 0 ? "FULL_RANK" :
		(result.integerConsistent ? "UNTRACKED_DETERMINISTIC_RELATION" :
			"DETERMINISTIC_INTEGER_INCONSISTENCY");
	return result;
}

inline bool zhangExactRectangularMatrix(
	const ZhangExactMatrix& matrix,
	std::size_t columns)
{
	return std::all_of(matrix.begin(), matrix.end(),
		[columns](const auto& row) { return row.size() == columns; });
}

inline ZhangExactVector zhangExactRowCombination(
	const ZhangExactVector& coefficients,
	const ZhangExactMatrix& rows)
{
	if (coefficients.size() != rows.size() || rows.empty()) return {};
	ZhangExactVector result(rows.front().size());
	for (std::size_t row = 0; row < rows.size(); row++)
	for (std::size_t column = 0; column < result.size(); column++)
	{
		result[column] += coefficients[row] * rows[row][column];
	}
	return result;
}

inline bool zhangExactLatticeContainsAll(
	const ZhangExactMatrix& lattice,
	const ZhangExactMatrix& targets)
{
	return std::all_of(targets.begin(), targets.end(),
		[&](const auto& row)
		{
			return zhangIntegerRowLatticeContains(lattice, row).contained;
		});
}

inline bool zhangExactPrimitiveRowLattice(
	const ZhangExactMatrix& rows,
	std::size_t dimension,
	int* rank = nullptr)
{
	if (!zhangExactRectangularMatrix(rows, dimension)) return false;
	if (rows.empty())
	{
		if (rank) *rank = 0;
		return true;
	}
	const auto smith = zhangIntegerRowLatticeContains(
		rows, ZhangExactVector(dimension));
	if (rank) *rank = smith.rank;
	return std::all_of(smith.smithInvariants.begin(),
		smith.smithInvariants.end(), [](const auto& invariant)
		{
			return zhangExactAbs(invariant) == 1;
		});
}

inline ZhangHeldQuotientAudit zhangExactHeldQuotientAudit(
	const ZhangExactMatrix& targetRows,
	const ZhangExactMatrix& heldRows,
	const ZhangExactVector& heldValues = {})
{
	ZhangHeldQuotientAudit result;
	if (targetRows.empty())
	{
		result.failureReason = "EMPTY_TARGET_LATTICE";
		return result;
	}
	const std::size_t dimension = targetRows.front().size();
	if (!zhangExactRectangularMatrix(targetRows, dimension) ||
		!zhangExactRectangularMatrix(heldRows, dimension) ||
		(!heldValues.empty() && heldValues.size() != heldRows.size()))
	{
		result.failureReason = "TARGET_HELD_DIMENSION_MISMATCH";
		return result;
	}
	const ZhangExactRowHnf targetHnf =
		zhangExactRowHermiteNormalForm(targetRows);
	result.targetBasis = targetHnf.basis;
	result.targetRank = static_cast<int>(result.targetBasis.size());
	if (!targetHnf.consistent || result.targetRank !=
		static_cast<int>(targetRows.size()))
	{
		result.failureReason = "TARGET_ROWS_NOT_AN_INTEGER_BASIS";
		return result;
	}
	if (heldRows.empty())
	{
		result.heldIntersectionPrimitiveInTarget = true;
		for (int coordinate = 0; coordinate < result.targetRank; coordinate++)
		{
			ZhangExactVector unit(result.targetRank);
			unit[coordinate] = 1;
			result.quotientTargetCoordinates.push_back(std::move(unit));
		}
		result.quotientRank = result.targetRank;
		result.exactClosure = true;
		result.valid = true;
		return result;
	}

	// Solve a*T = b*H exactly.  Kernel vectors of [T^T,-H^T] contain
	// target coefficients a followed by held coefficients b.
	ZhangExactMatrix relation(dimension,
		ZhangExactVector(targetRows.size() + heldRows.size()));
	for (std::size_t column = 0; column < dimension; column++)
	{
		for (std::size_t row = 0; row < targetRows.size(); row++)
			relation[column][row] = targetRows[row][column];
		for (std::size_t row = 0; row < heldRows.size(); row++)
			relation[column][targetRows.size() + row] = -heldRows[row][column];
	}
	const ZhangExactMatrix relationKernel = zhangExactIntegerKernel(relation);
	ZhangExactMatrix intersectionCoordinates;
	ZhangExactVector intersectionValues;
	for (const auto& kernelRow : relationKernel)
	{
		ZhangExactVector targetCoefficients(
			kernelRow.begin(), kernelRow.begin() + targetRows.size());
		if (std::all_of(targetCoefficients.begin(), targetCoefficients.end(),
			[](const auto& value) { return value == 0; })) continue;
		intersectionCoordinates.push_back(std::move(targetCoefficients));
		ZhangExactInteger value = 0;
		if (!heldValues.empty())
		{
			for (std::size_t row = 0; row < heldRows.size(); row++)
				value += kernelRow[targetRows.size() + row] * heldValues[row];
		}
		intersectionValues.push_back(value);
	}
	const ZhangExactRowHnf intersectionHnf = zhangExactRowHermiteNormalForm(
		intersectionCoordinates, intersectionValues);
	if (!intersectionHnf.consistent)
	{
		result.failureReason = "HELD_INTERSECTION_AFFINE_INCONSISTENCY";
		return result;
	}
	result.heldIntersectionTargetCoordinates = intersectionHnf.basis;
	result.heldIntersectionValues = intersectionHnf.values;
	result.heldIntersectionRank = static_cast<int>(intersectionHnf.basis.size());
	for (const auto& coordinateRow : intersectionHnf.basis)
	{
		result.heldIntersectionPhysicalBasis.push_back(
			zhangExactRowCombination(coordinateRow, targetRows));
	}
	result.heldIntersectionPrimitiveInTarget = zhangExactPrimitiveRowLattice(
		result.heldIntersectionTargetCoordinates, result.targetRank);
	if (!result.heldIntersectionPrimitiveInTarget)
	{
		result.failureReason = "HELD_INTERSECTION_NOT_PRIMITIVE_IN_TARGET";
		return result;
	}

	// Choose an explicit primitive complement from canonical target unit rows.
	// Every accepted addition must increase rank while retaining index one.
	ZhangExactMatrix completed = result.heldIntersectionTargetCoordinates;
	int completedRank = result.heldIntersectionRank;
	for (int coordinate = 0;
		 coordinate < result.targetRank && completedRank < result.targetRank;
		 coordinate++)
	{
		ZhangExactVector unit(result.targetRank);
		unit[coordinate] = 1;
		ZhangExactMatrix candidate = completed;
		candidate.push_back(unit);
		int candidateRank = 0;
		if (zhangExactPrimitiveRowLattice(
				candidate, result.targetRank, &candidateRank) &&
			candidateRank == completedRank + 1)
		{
			result.quotientTargetCoordinates.push_back(std::move(unit));
			completed = std::move(candidate);
			completedRank = candidateRank;
		}
	}
	result.quotientRank = static_cast<int>(
		result.quotientTargetCoordinates.size());
	result.exactClosure = completedRank == result.targetRank &&
		result.heldIntersectionRank + result.quotientRank == result.targetRank &&
		zhangExactLatticeContainsAll(completed,
			zhangExactIdentityMatrix(result.targetRank));
	if (!result.exactClosure)
	{
		result.failureReason = "PRIMITIVE_QUOTIENT_COMPLETION_FAILED";
		return result;
	}
	result.valid = true;
	return result;
}

inline ZhangCertifiedUnionAudit zhangExactCertifiedUnionAudit(
	const ZhangExactMatrix& targetRows,
	const ZhangExactMatrix& heldIntersectionRows,
	const ZhangExactVector& heldIntersectionValues,
	const ZhangExactMatrix& newlyFixedRows,
	const ZhangExactVector& newlyFixedValues)
{
	ZhangCertifiedUnionAudit result;
	if (targetRows.empty() ||
		heldIntersectionRows.size() != heldIntersectionValues.size() ||
		newlyFixedRows.size() != newlyFixedValues.size())
	{
		result.failureReason = "CERTIFIED_UNION_DIMENSION_MISMATCH";
		return result;
	}
	ZhangExactMatrix rows = heldIntersectionRows;
	rows.insert(rows.end(), newlyFixedRows.begin(), newlyFixedRows.end());
	ZhangExactVector values = heldIntersectionValues;
	values.insert(values.end(), newlyFixedValues.begin(), newlyFixedValues.end());
	const auto targetHnf = zhangExactRowHermiteNormalForm(targetRows);
	const auto heldHnf = zhangExactRowHermiteNormalForm(
		heldIntersectionRows, heldIntersectionValues);
	const auto fixedHnf = zhangExactRowHermiteNormalForm(
		newlyFixedRows, newlyFixedValues);
	const auto unionHnf = zhangExactRowHermiteNormalForm(rows, values);
	result.targetRank = targetHnf.basis.size();
	result.heldRank = heldHnf.basis.size();
	result.combinedCertifiedRank = unionHnf.basis.size();
	// This is the *incremental* certified rank, not the standalone rank of
	// the proposed batch.  Reporting fixedHnf here made a duplicate component
	// edge look like a new certificate and could inflate a closure audit.
	result.newlyFixedRank = std::max(
		0, result.combinedCertifiedRank - result.heldRank);
	result.consistent = targetHnf.consistent && heldHnf.consistent &&
		fixedHnf.consistent && unionHnf.consistent;
	if (!result.consistent)
	{
		result.failureReason = "CERTIFIED_UNION_AFFINE_INCONSISTENCY";
		return result;
	}
	result.certifiedBasis = unionHnf.basis;
	result.certifiedValues = unionHnf.values;
	result.targetContainedInCertified = zhangExactLatticeContainsAll(
		result.certifiedBasis, targetHnf.basis);
	result.certifiedContainedInTarget = zhangExactLatticeContainsAll(
		targetHnf.basis, result.certifiedBasis);
	result.exactTargetEquality = result.targetContainedInCertified &&
		result.certifiedContainedInTarget;
	if (!result.certifiedContainedInTarget)
		result.failureReason = "CERTIFIED_ROW_OUTSIDE_TARGET_LATTICE";
	else if (!result.targetContainedInCertified)
		result.failureReason = "TARGET_LATTICE_NOT_FULLY_CERTIFIED";
	return result;
}
