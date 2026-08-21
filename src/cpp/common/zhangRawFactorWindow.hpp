#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"
#include "common/zhangFixedLagSquareRoot.hpp"

struct ZhangRawFactorWindowMarginal
{
	bool valid = false;
	bool quotientValid = false;
	bool absoluteDatumValid = false;
	int requestedTargetCount = 0;
	int unresolvedGaugeRank = 0;
	int latentRank = 0;
	int nuisanceRank = 0;
	int targetRank = 0;
	VectorXd mean;
	VectorXd fractionalMean;
	MatrixXd covariance;
	MatrixXd residualDesign;
	VectorXd residualRightHandSide;
	std::string failureReason;
};

/** Batch square-root window expressed in independent Gaussian driving modes.
 *
 * The boundary state and every positive process-noise direction are written
 * as x = x_bar + B u, u~N(0,I).  Zero process-noise directions introduce no
 * latent variable, so deterministic dynamics and exact S-basis transforms are
 * enforced by substitution rather than by an arbitrary large weight.  The
 * stored rows are the finally accepted, whitened measurement equations.
 */
class ZhangRawFactorWindow
{
public:
	bool initialise(
		const VectorXd& boundaryMean,
		const MatrixXd& boundaryCovariance,
		double relativeRankTolerance = 1e-11)
	{
		clear();
		rankTolerance = relativeRankTolerance;
		if (boundaryCovariance.rows() != boundaryMean.size()
		 || boundaryCovariance.cols() != boundaryMean.size()
		 || !boundaryMean.allFinite() || !boundaryCovariance.allFinite())
		{
			failureReason = "INVALID_BOUNDARY_GAUSSIAN";
			return false;
		}
		mean = boundaryMean;
		MatrixXd squareRoot;
		if (!positiveSquareRoot(boundaryCovariance, squareRoot, true))
		{
			return false;
		}
		sensitivity = squareRoot.sparseView(0, 0);
		initialised = true;
		return true;
	}

	bool addAcceptedMeasurement(
		const MatrixXd& design,
		const MatrixXd& covariance,
		const VectorXd& absoluteRightHandSide)
	{
		if (!initialised
		 || design.cols() != mean.size()
		 || design.rows() != absoluteRightHandSide.size()
		 || covariance.rows() != design.rows()
		 || covariance.cols() != design.rows()
		 || !design.allFinite() || !covariance.allFinite()
		 || !absoluteRightHandSide.allFinite())
		{
			failureReason = "INVALID_ACCEPTED_MEASUREMENT_FACTOR";
			return false;
		}
		MatrixXd inverseSquareRoot;
		if (!inversePositiveSquareRoot(covariance, inverseSquareRoot))
		{
			return false;
		}
		FactorBlock block;
		block.design = (inverseSquareRoot * design * sensitivity)
			.sparseView(0, 0);
		block.rightHandSide = inverseSquareRoot
			* (absoluteRightHandSide - design * mean);
		blocks.push_back(std::move(block));
		return true;
	}

	bool addStateTransition(
		const MatrixXd& transition,
		const MatrixXd& processCovariance)
	{
		if (!initialised
		 || transition.cols() != mean.size()
		 || processCovariance.rows() != transition.rows()
		 || processCovariance.cols() != transition.rows()
		 || !transition.allFinite() || !processCovariance.allFinite())
		{
			failureReason = "INVALID_STATE_TRANSITION_FACTOR";
			return false;
		}
		MatrixXd processSquareRoot;
		if (!positiveSquareRoot(processCovariance, processSquareRoot, true))
		{
			return false;
		}
		SparseMatrix<double> transitionSparse = transition.sparseView(0, 0);
		SparseMatrix<double> propagated = transitionSparse * sensitivity;
		SparseMatrix<double> processSparse = processSquareRoot.sparseView(0, 0);
		std::vector<Triplet<double>> entries;
		entries.reserve(propagated.nonZeros() + processSparse.nonZeros());
		for (int outer = 0; outer < propagated.outerSize(); outer++)
		for (SparseMatrix<double>::InnerIterator entry(propagated, outer);
			 entry; ++entry)
		{
			entries.emplace_back(entry.row(), entry.col(), entry.value());
		}
		for (int outer = 0; outer < processSparse.outerSize(); outer++)
		for (SparseMatrix<double>::InnerIterator entry(processSparse, outer);
			 entry; ++entry)
		{
			entries.emplace_back(
				entry.row(), propagated.cols() + entry.col(), entry.value());
		}
		SparseMatrix<double> nextSensitivity(
			transition.rows(), propagated.cols() + processSparse.cols());
		nextSensitivity.setFromTriplets(entries.begin(), entries.end());
		sensitivity = std::move(nextSensitivity);
		mean = transition * mean;
		return true;
	}

	bool addExactCoordinateTransform(const MatrixXd& transform)
	{
		if (!initialised
		 || transform.cols() != mean.size()
		 || !transform.allFinite())
		{
			failureReason = "INVALID_EXACT_COORDINATE_TRANSFORM";
			return false;
		}
		mean = transform * mean;
		SparseMatrix<double> transformSparse = transform.sparseView(0, 0);
		sensitivity = transformSparse * sensitivity;
		return true;
	}

	/** Eliminate every continuous latent mode while retaining only G*x+offset.
	 * The target row rank must be exact in the stochastic support.  A target in
	 * a zero-variance direction is a fixed constant, not an estimable integer
	 * functional, and is therefore rejected here. */
	ZhangRawFactorWindowMarginal marginaliseToIntegerDatum(
		const MatrixXd& targetRows,
		const VectorXd& targetOffsets) const
	{
		ZhangRawFactorWindowMarginal result;
		if (!initialised
		 || targetRows.cols() != mean.size()
		 || targetRows.rows() != targetOffsets.size()
		 || targetRows.rows() == 0
		 || !targetRows.allFinite() || !targetOffsets.allFinite())
		{
			result.failureReason = "INVALID_INTEGER_DATUM_FUNCTIONAL";
			return result;
		}
		const int latentCount = sensitivity.cols();
		if (latentCount == 0)
		{
			result.failureReason = "ZERO_STOCHASTIC_SUPPORT";
			return result;
		}
		const MatrixXd targetSensitivity = targetRows * sensitivity;
		Eigen::FullPivLU<MatrixXd> targetLu(targetSensitivity);
		targetLu.setThreshold(rankTolerance);
		if (targetLu.rank() != targetRows.rows())
		{
			result.failureReason = "INTEGER_DATUM_NOT_FULL_ROW_RANK";
			return result;
		}

		int measurementRows = 0;
		for (const auto& block : blocks)
		{
			measurementRows += block.design.rows();
		}
		const int totalRows = latentCount + measurementRows;
		std::vector<Triplet<double>> entries;
		entries.reserve(latentCount + measurementRows * 8);
		VectorXd rhs = VectorXd::Zero(totalRows);
		for (int index = 0; index < latentCount; index++)
		{
			entries.emplace_back(index, index, 1.0);
		}
		int outputRow = latentCount;
		for (const auto& block : blocks)
		{
			for (int outer = 0; outer < block.design.outerSize(); outer++)
			for (SparseMatrix<double>::InnerIterator entry(block.design, outer);
				 entry; ++entry)
			{
				entries.emplace_back(
					outputRow + entry.row(), entry.col(), entry.value());
			}
			rhs.segment(outputRow, block.rightHandSide.size()) =
				block.rightHandSide;
			outputRow += block.design.rows();
		}
		SparseMatrix<double> whitened(totalRows, latentCount);
		whitened.setFromTriplets(entries.begin(), entries.end());
		SparseQR<SparseMatrix<double>, COLAMDOrdering<int>> qr;
		qr.setPivotThreshold(rankTolerance);
		qr.compute(whitened);
		if (qr.info() != Eigen::Success || qr.rank() != latentCount)
		{
			result.failureReason = "RAW_FACTOR_SPARSE_QR_RANK_FAILURE";
			return result;
		}
		const VectorXd latentMean = qr.solve(rhs);
		// SparseQR already provides the square-root information factor.  Never
		// form A^T A here: its Cholesky/LDLT fill was the dominant memory cost at
		// 35 network epochs and also squares the condition number.  Eigen uses
		// A*P=Q*R, so express the target in the pivoted latent coordinates and
		// solve R^T Y=(L*P)^T.  Then cov(Lu)=Y^T Y.
		const SparseMatrix<double> rectangularUpper = qr.matrixR();
		if (rectangularUpper.rows() < latentCount
			|| rectangularUpper.cols() != latentCount)
		{
			result.failureReason = "RAW_FACTOR_SPARSE_QR_R_DIMENSION_FAILURE";
			return result;
		}
		const SparseMatrix<double> upper =
			rectangularUpper.topRows(latentCount);
		const MatrixXd permutedTargetSensitivity =
			targetSensitivity * qr.colsPermutation();
		const SparseMatrix<double> lower = upper.transpose();
		const MatrixXd whitenedTarget =
			lower.triangularView<Eigen::Lower>().solve(
				permutedTargetSensitivity.transpose());
		if (!latentMean.allFinite() || !whitenedTarget.allFinite())
		{
			result.failureReason = "RAW_FACTOR_SQUARE_ROOT_TARGET_SOLVE_FAILED";
			return result;
		}
		result.latentRank = latentCount;
		result.nuisanceRank = latentCount - targetRows.rows();
		result.targetRank = targetRows.rows();
		result.mean = targetRows * mean + targetOffsets
			+ targetSensitivity * latentMean;
		result.covariance = whitenedTarget.transpose() * whitenedTarget;
		result.covariance = 0.5
			* (result.covariance + result.covariance.transpose());
		result.valid = result.mean.allFinite() && result.covariance.allFinite();
		result.quotientValid = result.valid;
		result.absoluteDatumValid = result.valid;
		result.fractionalMean.resize(result.mean.size());
		for (int index = 0; index < result.mean.size(); index++)
		{
			result.fractionalMean(index) =
				result.mean(index) - std::round(result.mean(index));
		}
		if (!result.valid)
		{
			result.failureReason = "NONFINITE_INTEGER_DATUM_MARGINAL";
		}
		return result;
	}

	const VectorXd& currentMean() const { return mean; }
	const SparseMatrix<double>& currentSensitivity() const { return sensitivity; }
	const std::string& lastFailureReason() const { return failureReason; }

private:
	struct FactorBlock
	{
		SparseMatrix<double> design;
		VectorXd rightHandSide;
	};

	void clear()
	{
		initialised = false;
		mean.resize(0);
		sensitivity.resize(0, 0);
		blocks.clear();
		failureReason.clear();
	}

	bool positiveSquareRoot(
		const MatrixXd& covariance,
		MatrixXd& squareRoot,
		bool allowSemidefinite)
	{
		MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
		LLT<MatrixXd> cholesky(symmetric);
		if (cholesky.info() == Eigen::Success)
		{
			MatrixXd lower = cholesky.matrixL();
			const double diagonalScale = std::max(
				1.0, lower.diagonal().cwiseAbs().maxCoeff());
			if ((lower.diagonal().array().abs()
				> std::sqrt(rankTolerance) * diagonalScale).all())
			{
				squareRoot = std::move(lower);
				return true;
			}
		}
		Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(symmetric);
		if (eigen.info() != Eigen::Success)
		{
			failureReason = "COVARIANCE_EIGEN_DECOMPOSITION_FAILED";
			return false;
		}
		const double scale = std::max(
			1.0, eigen.eigenvalues().cwiseAbs().maxCoeff());
		const double threshold = rankTolerance * scale;
		if (eigen.eigenvalues().minCoeff() < -threshold)
		{
			failureReason = "NEGATIVE_COVARIANCE_DIRECTION";
			return false;
		}
		std::vector<int> positive;
		for (int index = 0; index < eigen.eigenvalues().size(); index++)
		{
			if (eigen.eigenvalues()(index) > threshold)
			{
				positive.push_back(index);
			}
		}
		if (!allowSemidefinite
		 && positive.size() != static_cast<std::size_t>(covariance.rows()))
		{
			failureReason = "SINGULAR_MEASUREMENT_COVARIANCE";
			return false;
		}
		squareRoot = MatrixXd::Zero(covariance.rows(), positive.size());
		for (int column = 0; column < static_cast<int>(positive.size()); column++)
		{
			const int index = positive[column];
			squareRoot.col(column) = eigen.eigenvectors().col(index)
				* std::sqrt(eigen.eigenvalues()(index));
		}
		return true;
	}

	bool inversePositiveSquareRoot(
		const MatrixXd& covariance,
		MatrixXd& inverseSquareRoot)
	{
		const MatrixXd symmetric =
			0.5 * (covariance + covariance.transpose());
		LLT<MatrixXd> cholesky(symmetric);
		if (cholesky.info() == Eigen::Success)
		{
			inverseSquareRoot = cholesky.matrixL().solve(
				MatrixXd::Identity(covariance.rows(), covariance.cols()));
			if (inverseSquareRoot.allFinite())
			{
				return true;
			}
		}
		MatrixXd squareRoot;
		if (!positiveSquareRoot(covariance, squareRoot, false))
		{
			return false;
		}
		Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(symmetric);
		inverseSquareRoot = eigen.eigenvalues().cwiseSqrt()
			.cwiseInverse().asDiagonal() * eigen.eigenvectors().transpose();
		return inverseSquareRoot.allFinite();
	}

	bool initialised = false;
	double rankTolerance = 1e-11;
	VectorXd mean;
		SparseMatrix<double> sensitivity;
	std::vector<FactorBlock> blocks;
	std::string failureReason;
};
