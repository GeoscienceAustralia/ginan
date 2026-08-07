#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>

#include "common/eigenIncluder.hpp"

struct ZhangSquareRootMarginal
{
	bool valid = false;
	int nuisanceRank = 0;
	int targetRank = 0;
	VectorXd mean;
	MatrixXd covariance;
	MatrixXd residualDesign;
	VectorXd residualRightHandSide;
	std::string failureReason;
};

/** Eliminate leading nuisance columns directly from a square-root factor.
 *
 * `factor * delta = rhs` is already whitened.  Sparse QR projects the target
 * columns and right-hand side onto the orthogonal complement of the nuisance
 * column space.  Only the small retained target information block is made
 * dense.  No full state covariance or explicit dense Schur inverse is formed.
 */
inline ZhangSquareRootMarginal zhangMarginaliseSquareRootFactors(
	const SparseMatrix<double>& factor,
	const VectorXd&             rhs,
	int                         nuisanceColumns,
	double                      rankTolerance = 1e-11)
{
	ZhangSquareRootMarginal result;
	if (factor.rows() != rhs.size()
	 || nuisanceColumns < 0
	 || nuisanceColumns > factor.cols())
	{
		result.failureReason = "INVALID_FACTOR_DIMENSIONS";
		return result;
	}
	const int targetColumns = factor.cols() - nuisanceColumns;
	if (targetColumns <= 0)
	{
		result.failureReason = "NO_RETAINED_TARGET";
		return result;
	}

	MatrixXd projectedTarget;
	VectorXd projectedRhs;
	if (nuisanceColumns > 0)
	{
		SparseMatrix<double> nuisance = factor.leftCols(nuisanceColumns);
		SparseQR<SparseMatrix<double>, COLAMDOrdering<int>> qr;
		qr.setPivotThreshold(rankTolerance);
		qr.compute(nuisance);
		if (qr.info() != Eigen::Success)
		{
			result.failureReason = "NUISANCE_SPARSE_QR_FAILED";
			return result;
		}
		result.nuisanceRank = qr.rank();
		MatrixXd targetDense = MatrixXd(factor.rightCols(targetColumns));
		MatrixXd rotatedTarget = qr.matrixQ().transpose() * targetDense;
		VectorXd rotatedRhs = qr.matrixQ().transpose() * rhs;
		const int retainedRows = factor.rows() - result.nuisanceRank;
		if (retainedRows <= 0)
		{
			result.failureReason = "NO_RESIDUAL_TARGET_INFORMATION";
			return result;
		}
		projectedTarget = rotatedTarget.bottomRows(retainedRows);
		projectedRhs = rotatedRhs.tail(retainedRows);
	}
	else
	{
		projectedTarget = MatrixXd(factor);
		projectedRhs = rhs;
	}

	Eigen::ColPivHouseholderQR<MatrixXd> targetQr(projectedTarget);
	targetQr.setThreshold(rankTolerance);
	result.targetRank = targetQr.rank();
	if (result.targetRank != targetColumns)
	{
		result.failureReason = "RETAINED_TARGET_RANK_DEFICIENT";
		return result;
	}
	result.mean = targetQr.solve(projectedRhs);

	MatrixXd information = projectedTarget.transpose() * projectedTarget;
	information = 0.5 * (information + information.transpose());
	LDLT<MatrixXd> ldlt(information);
	if (ldlt.info() != Eigen::Success)
	{
		result.failureReason = "RETAINED_INFORMATION_LDLT_FAILED";
		return result;
	}
	double diagonalScale = std::max(
		1.0,
		ldlt.vectorD().cwiseAbs().maxCoeff()
	);
	if ((ldlt.vectorD().array() <= rankTolerance * diagonalScale).any())
	{
		result.failureReason = "RETAINED_INFORMATION_NOT_POSITIVE_DEFINITE";
		return result;
	}
	result.covariance = ldlt.solve(
		MatrixXd::Identity(targetColumns, targetColumns)
	);
	result.covariance =
		0.5 * (result.covariance + result.covariance.transpose());
	result.residualDesign = std::move(projectedTarget);
	result.residualRightHandSide = std::move(projectedRhs);
	if (!result.mean.allFinite() || !result.covariance.allFinite())
	{
		result.failureReason = "NONFINITE_RETAINED_MARGINAL";
		return result;
	}
	result.valid = true;
	return result;
}

struct ZhangWhitenedBlock
{
	bool valid = false;
	int rank = 0;
	VectorXd residual;
	double squaredNorm = std::numeric_limits<double>::quiet_NaN();
	std::string failureReason;
};

/** Rank-revealing symmetric square-root whitening for a small retained block.
 *
 * Near-null non-negative eigen-directions are removed and counted in the
 * resulting rank.  A materially negative direction is a hard failure.
 */
inline ZhangWhitenedBlock zhangWhitenRetainedResidual(
	const VectorXd& residual,
	const MatrixXd& covariance,
	double relativeRankTolerance = 1e-11)
{
	ZhangWhitenedBlock result;
	if (covariance.rows() != covariance.cols()
	 || covariance.rows() != residual.size())
	{
		result.failureReason = "INVALID_WHITENING_DIMENSIONS";
		return result;
	}
	MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
	Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(symmetric);
	if (eigen.info() != Eigen::Success)
	{
		result.failureReason = "COVARIANCE_EIGEN_DECOMPOSITION_FAILED";
		return result;
	}
	const double scale = std::max(
		1.0,
		eigen.eigenvalues().cwiseAbs().maxCoeff()
	);
	const double threshold = relativeRankTolerance * scale;
	if (eigen.eigenvalues().minCoeff() < -threshold)
	{
		result.failureReason = "NEGATIVE_COVARIANCE_DIRECTION";
		return result;
	}
	for (int index = 0; index < eigen.eigenvalues().size(); index++)
	{
		if (eigen.eigenvalues()(index) > threshold)
		{
			result.rank++;
		}
	}
	if (result.rank == 0)
	{
		result.failureReason = "ZERO_COVARIANCE_RANK";
		return result;
	}
	result.residual = VectorXd::Zero(result.rank);
	int output = 0;
	for (int index = 0; index < eigen.eigenvalues().size(); index++)
	{
		if (eigen.eigenvalues()(index) <= threshold)
		{
			continue;
		}
		result.residual(output++) =
			eigen.eigenvectors().col(index).dot(residual)
			/ std::sqrt(eigen.eigenvalues()(index));
	}
	result.squaredNorm = result.residual.squaredNorm();
	result.valid = result.residual.allFinite()
		&& std::isfinite(result.squaredNorm);
	if (!result.valid)
	{
		result.failureReason = "NONFINITE_WHITENED_RESIDUAL";
	}
	return result;
}
