#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"
#include "common/zhangFixedLagSquareRoot.hpp"

struct ZhangIncrementalRawSquareRootSummary
{
	bool valid = false;
	int stateDimension = 0;
	int storedRows = 0;
	int storedColumns = 0;
	int maximumStoredRows = 0;
	int maximumStoredColumns = 0;
	int batchOrthogonalDof = 0;
	double batchOrthogonalSquaredNorm = 0;
	int exactConstraintsApplied = 0;
	std::string failureReason;
};

/** Incremental raw-factor square-root boundary filter.
 *
 * Only a current affine stochastic support x=a+B*z and its information factor
 * R*z=d are stored.  This represents singular state covariances exactly: zero
 * variance directions remain deterministic instead of being regularised.  An
 * accepted H/R block is appended in z coordinates and QR-compressed.  A
 * transition x+=F*x+w is propagated with [F*S,Q^(1/2)], retaining only its
 * numerical row rank.  Exact coordinate transforms use the same zero-noise
 * propagation.  No historical measurement or transition matrix is retained.
 */
class ZhangIncrementalRawSquareRoot
{
public:
	explicit ZhangIncrementalRawSquareRoot(
		double relativeRankTolerance = 1e-11)
	:	rankTolerance(relativeRankTolerance)
	{}

	void clear()
	{
		initialised = false;
		anchor.resize(0);
		support.resize(0, 0);
		factor.resize(0, 0);
		rhs.resize(0);
		maximumRows = 0;
		maximumColumns = 0;
		orthogonalDof = 0;
		orthogonalSquaredNorm = 0;
		exactConstraintCount = 0;
		failureReason.clear();
	}

	bool initialise(const VectorXd& mean, const MatrixXd& covariance)
	{
		clear();
		if (mean.size() == 0
		 || covariance.rows() != mean.size()
		 || covariance.cols() != mean.size()
		 || !mean.allFinite() || !covariance.allFinite())
		{
			return fail("INVALID_RAW_SQUARE_ROOT_BOUNDARY");
		}
		MatrixXd squareRoot;
		if (!positiveSquareRoot(covariance, squareRoot, true))
		{
			return false;
		}
		if (squareRoot.cols() == 0)
		{
			return fail("DETERMINISTIC_RAW_SQUARE_ROOT_BOUNDARY");
		}
		anchor = mean;
		support = squareRoot;
		factor = MatrixXd::Identity(support.cols(), support.cols());
		rhs = VectorXd::Zero(support.cols());
		initialised = true;
		updateBounds();
		return factor.allFinite() && rhs.allFinite()
			? true : fail("NONFINITE_RAW_SQUARE_ROOT_BOUNDARY");
	}

	bool addAcceptedMeasurement(
		const MatrixXd& design,
		const MatrixXd& covariance,
		const VectorXd& absoluteObservation)
	{
		if (!initialised
		 || design.cols() != support.rows()
		 || design.rows() != absoluteObservation.size()
		 || covariance.rows() != design.rows()
		 || covariance.cols() != design.rows()
		 || !design.allFinite() || !covariance.allFinite()
		 || !absoluteObservation.allFinite())
		{
			return fail("INVALID_RAW_SQUARE_ROOT_MEASUREMENT");
		}
		MatrixXd inverseSquareRoot;
		if (!inversePositiveSquareRoot(covariance, inverseSquareRoot))
		{
			return false;
		}
		const MatrixXd latentDesign = design * support;
		const VectorXd centredObservation = absoluteObservation - design * anchor;
		MatrixXd augmented(factor.rows() + design.rows(), factor.cols());
		augmented.topRows(factor.rows()) = factor;
		augmented.bottomRows(design.rows()) = inverseSquareRoot * latentDesign;
		VectorXd augmentedRhs(rhs.size() + absoluteObservation.size());
		augmentedRhs.head(rhs.size()) = rhs;
		augmentedRhs.tail(absoluteObservation.size()) =
			inverseSquareRoot * centredObservation;
		return compress(augmented, augmentedRhs, true);
	}

	bool advance(
		const MatrixXd& transition,
		const MatrixXd& processCovariance)
	{
		if (!initialised
		 || transition.cols() != support.rows()
		 || processCovariance.rows() != transition.rows()
		 || processCovariance.cols() != transition.rows()
		 || !transition.allFinite() || !processCovariance.allFinite())
		{
			return fail("INVALID_RAW_SQUARE_ROOT_TRANSITION");
		}
		VectorXd mean;
		MatrixXd covarianceSquareRoot;
		if (!boundary(mean, covarianceSquareRoot))
		{
			return false;
		}
		MatrixXd processSquareRoot;
		// Q=0 is a legitimate deterministic state transition and is required
		// for the E28-2 fixed-dynamics control.  A covariance boundary may not
		// be wholly deterministic, but an additive process-noise block may have
		// zero rank; represent it by an empty square-root factor.
		if (processCovariance.isZero(0))
		{
			processSquareRoot.resize(processCovariance.rows(), 0);
		}
		else if (!positiveSquareRoot(
				processCovariance, processSquareRoot, true))
		{
			return false;
		}
		MatrixXd propagated(
			transition.rows(), covarianceSquareRoot.cols()
				+ processSquareRoot.cols());
		propagated.leftCols(covarianceSquareRoot.cols()) =
			transition * covarianceSquareRoot;
		if (processSquareRoot.cols() > 0)
		{
			propagated.rightCols(processSquareRoot.cols()) = processSquareRoot;
		}
		return reanchorFromSquareRoot(
			transition * mean, propagated, "RAW_SQUARE_ROOT_TRANSITION");
	}

	bool applyExactCoordinateTransform(
		const MatrixXd& transform,
		const VectorXd& translation = {})
	{
		if (!initialised || transform.cols() != support.rows()
		 || !transform.allFinite())
		{
			return fail("INVALID_RAW_SQUARE_ROOT_EXACT_TRANSFORM");
		}
		const VectorXd shift = translation.size() == 0
			? VectorXd::Zero(transform.rows()) : translation;
		if (shift.size() != transform.rows() || !shift.allFinite())
		{
			return fail("INVALID_RAW_SQUARE_ROOT_EXACT_TRANSLATION");
		}
		VectorXd mean;
		MatrixXd covarianceSquareRoot;
		if (!boundary(mean, covarianceSquareRoot))
		{
			return false;
		}
		return reanchorFromSquareRoot(
			transform * mean + shift,
			transform * covarianceSquareRoot,
			"RAW_SQUARE_ROOT_EXACT_TRANSFORM");
	}

	/** Condition the current Gaussian support on exact affine relations
	 * design*x=observation.  The constraint is applied by parameterising the
	 * latent support with a particular solution plus the exact null space; no
	 * artificial epsilon variance is introduced. */
	bool applyExactConstraint(
		const MatrixXd& design,
		const VectorXd& observation)
	{
		if (!initialised || design.rows() == 0
		 || design.cols() != support.rows()
		 || observation.size() != design.rows()
		 || !design.allFinite() || !observation.allFinite())
		{
			return fail("INVALID_RAW_SQUARE_ROOT_EXACT_CONSTRAINT");
		}
		const MatrixXd latentDesign = design * support;
		const VectorXd centredObservation = observation - design * anchor;
		const double latentAbsoluteScale = std::max(
			1.0, design.norm() * support.norm());
		const double centredAbsoluteScale = std::max({
			1.0, observation.norm(), design.norm() * anchor.norm()});
		if (latentDesign.norm()
			<= 10 * rankTolerance * latentAbsoluteScale)
		{
			if (centredObservation.norm()
				> 10 * rankTolerance * centredAbsoluteScale)
			{
				return fail("INCONSISTENT_DETERMINISTIC_EXACT_CONSTRAINT");
			}
			exactConstraintCount += design.rows();
			return true;
		}
		Eigen::FullPivLU<MatrixXd> lu(latentDesign);
		lu.setThreshold(rankTolerance);
		const int constraintRank = lu.rank();
		const double consistencyScale = std::max(
			1.0, centredObservation.norm());
		if (constraintRank == 0)
		{
			if (centredObservation.norm()
				> rankTolerance * consistencyScale)
			{
				return fail("INCONSISTENT_DETERMINISTIC_EXACT_CONSTRAINT");
			}
			exactConstraintCount += design.rows();
			return true;
		}
		const VectorXd particular = latentDesign
			.completeOrthogonalDecomposition().solve(centredObservation);
		if ((latentDesign * particular - centredObservation).norm()
			> 10 * rankTolerance * consistencyScale)
		{
			return fail("INCONSISTENT_RAW_SQUARE_ROOT_EXACT_CONSTRAINT");
		}
		const MatrixXd nullSpace = lu.kernel();
		if (nullSpace.cols() == 0)
		{
			return fail("EXACT_CONSTRAINT_REMOVED_ALL_STOCHASTIC_DIRECTIONS");
		}
		const VectorXd constrainedAnchor = anchor + support * particular;
		const MatrixXd constrainedSupport = support * nullSpace;
		const MatrixXd constrainedFactor = factor * nullSpace;
		const VectorXd constrainedRhs = rhs - factor * particular;
		Eigen::ColPivHouseholderQR<MatrixXd> qr(constrainedFactor);
		qr.setThreshold(rankTolerance);
		const int columns = constrainedFactor.cols();
		if (qr.rank() != columns)
		{
			return fail("EXACT_CONSTRAINT_FACTOR_RANK_DEFICIENT");
		}
		const VectorXd rotatedRhs =
			qr.householderQ().adjoint() * constrainedRhs;
		MatrixXd upper = MatrixXd::Zero(columns, columns);
		const MatrixXd rawUpper = qr.matrixR();
		for (int row = 0; row < columns; row++)
		for (int column = row; column < columns; column++)
		{
			upper(row, column) = rawUpper(row, column);
		}
		const MatrixXd compressedFactor =
			upper * qr.colsPermutation().transpose();
		const VectorXd compressedRhs = rotatedRhs.head(columns);
		if (!constrainedAnchor.allFinite() || !constrainedSupport.allFinite()
		 || !compressedFactor.allFinite() || !compressedRhs.allFinite())
		{
			return fail("NONFINITE_RAW_SQUARE_ROOT_EXACT_CONSTRAINT");
		}
		anchor = constrainedAnchor;
		support = constrainedSupport;
		factor = compressedFactor;
		rhs = compressedRhs;
		exactConstraintCount += constraintRank;
		updateBounds();
		return true;
	}

	ZhangSquareRootMarginal marginaliseTargets(
		const MatrixXd& targetRows,
		const VectorXd& targetOffsets) const
	{
		ZhangSquareRootMarginal rejected;
		if (!initialised
		 || targetRows.rows() == 0
		 || targetRows.cols() != support.rows()
		 || targetOffsets.size() != targetRows.rows()
		 || !targetRows.allFinite() || !targetOffsets.allFinite())
		{
			rejected.failureReason = "INVALID_RAW_SQUARE_ROOT_TARGETS";
			return rejected;
		}
		const MatrixXd latentTargets = targetRows * support;
		const VectorXd latentOffsets = targetOffsets + targetRows * anchor;
		Eigen::ColPivHouseholderQR<MatrixXd> pivotQr(latentTargets);
		pivotQr.setThreshold(rankTolerance);
		if (pivotQr.rank() != latentTargets.rows())
		{
			rejected.failureReason = "RAW_SQUARE_ROOT_TARGET_RANK_DEFICIENT";
			return rejected;
		}
		const auto permutation = pivotQr.colsPermutation().indices();
		std::vector<bool> pivoted(factor.cols(), false);
		for (int index = 0; index < targetRows.rows(); index++)
		{
			pivoted[permutation(index)] = true;
		}
		MatrixXd exact = MatrixXd::Zero(factor.cols(), factor.cols());
		int output = 0;
		for (int column = 0; column < factor.cols(); column++)
		{
			if (!pivoted[column])
			{
				exact(output++, column) = 1;
			}
		}
		exact.bottomRows(latentTargets.rows()) = latentTargets;
		Eigen::FullPivLU<MatrixXd> inverse(exact);
		inverse.setThreshold(rankTolerance);
		if (!inverse.isInvertible())
		{
			rejected.failureReason = "RAW_SQUARE_ROOT_TARGET_TRANSFORM_SINGULAR";
			return rejected;
		}
		VectorXd translation = VectorXd::Zero(factor.cols());
		translation.tail(latentOffsets.size()) = latentOffsets;
		const MatrixXd transformedFactor = factor * inverse.inverse();
		const VectorXd transformedRhs = rhs + transformedFactor * translation;
		return zhangMarginaliseSquareRootFactors(
			transformedFactor.sparseView(0, 0), transformedRhs,
			factor.cols() - targetRows.rows(), rankTolerance);
	}

	bool currentMarginal(VectorXd& mean, MatrixXd& covariance) const
	{
		MatrixXd squareRoot;
		if (!boundary(mean, squareRoot))
		{
			return false;
		}
		covariance = squareRoot * squareRoot.transpose();
		covariance = 0.5 * (covariance + covariance.transpose());
		return covariance.allFinite();
	}

	ZhangIncrementalRawSquareRootSummary summary() const
	{
		ZhangIncrementalRawSquareRootSummary result;
		result.valid = initialised && failureReason.empty();
		result.stateDimension = support.rows();
		result.storedRows = factor.rows();
		result.storedColumns = factor.cols();
		result.maximumStoredRows = maximumRows;
		result.maximumStoredColumns = maximumColumns;
		result.batchOrthogonalDof = orthogonalDof;
		result.batchOrthogonalSquaredNorm = orthogonalSquaredNorm;
		result.exactConstraintsApplied = exactConstraintCount;
		result.failureReason = failureReason;
		return result;
	}

private:
	bool boundary(VectorXd& mean, MatrixXd& covarianceSquareRoot) const
	{
		if (!initialised || factor.rows() != factor.cols())
		{
			return false;
		}
		Eigen::FullPivLU<MatrixXd> inverse(factor);
		inverse.setThreshold(rankTolerance);
		if (!inverse.isInvertible())
		{
			return false;
		}
		const MatrixXd latentSquareRoot = inverse.inverse();
		covarianceSquareRoot = support * latentSquareRoot;
		mean = anchor + support * (latentSquareRoot * rhs);
		return mean.allFinite() && covarianceSquareRoot.allFinite();
	}

	bool reanchorFromSquareRoot(
		const VectorXd& mean,
		const MatrixXd& covarianceSquareRoot,
		const std::string& label)
	{
		if (covarianceSquareRoot.rows() != mean.size()
		 || covarianceSquareRoot.cols() == 0
		 || !mean.allFinite() || !covarianceSquareRoot.allFinite())
		{
			return fail("INVALID_" + label + "_PROPAGATION");
		}
		Eigen::ColPivHouseholderQR<MatrixXd> qr(
			covarianceSquareRoot.transpose());
		qr.setThreshold(rankTolerance);
		const int rank = qr.rank();
		if (rank == 0)
		{
			return fail(label + "_RANK_DEFICIENT");
		}
		const MatrixXd rawUpper = qr.matrixR().topRows(rank);
		MatrixXd upper = MatrixXd::Zero(rank, mean.size());
		for (int row = 0; row < rank; row++)
		for (int column = row; column < mean.size(); column++)
		{
			upper(row, column) = rawUpper(row, column);
		}
		anchor = mean;
		support = qr.colsPermutation()
			* upper.transpose();
		factor = MatrixXd::Identity(rank, rank);
		rhs = VectorXd::Zero(rank);
		updateBounds();
		return factor.allFinite() && rhs.allFinite()
			? true : fail("NONFINITE_" + label);
	}

	bool compress(
		const MatrixXd& inputFactor,
		const VectorXd& inputRhs,
		bool accountOrthogonalResidual)
	{
		Eigen::ColPivHouseholderQR<MatrixXd> qr(inputFactor);
		qr.setThreshold(rankTolerance);
		const int columns = inputFactor.cols();
		if (qr.rank() != columns)
		{
			return fail("RAW_SQUARE_ROOT_FACTOR_RANK_DEFICIENT");
		}
		const VectorXd rotatedRhs = qr.householderQ().adjoint() * inputRhs;
		MatrixXd upper = MatrixXd::Zero(columns, columns);
		const MatrixXd rawUpper = qr.matrixR();
		for (int row = 0; row < columns; row++)
		for (int column = row; column < columns; column++)
		{
			upper(row, column) = rawUpper(row, column);
		}
		factor = upper * qr.colsPermutation().transpose();
		rhs = rotatedRhs.head(columns);
		if (accountOrthogonalResidual && inputFactor.rows() > columns)
		{
			const VectorXd residual = rotatedRhs.tail(inputFactor.rows() - columns);
			orthogonalDof += residual.size();
			orthogonalSquaredNorm += residual.squaredNorm();
		}
		updateBounds();
		return factor.allFinite() && rhs.allFinite()
			? true : fail("NONFINITE_RAW_SQUARE_ROOT_COMPRESSION");
	}

	bool positiveSquareRoot(
		const MatrixXd& covariance,
		MatrixXd& squareRoot,
		bool allowSemidefinite)
	{
		const MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
		MatrixXd offDiagonalMatrix = symmetric;
		offDiagonalMatrix.diagonal().setZero();
		const double diagonalScale = std::max(
			1.0, symmetric.diagonal().cwiseAbs().maxCoeff());
		if (offDiagonalMatrix.cwiseAbs().maxCoeff()
			<= rankTolerance * diagonalScale)
		{
			const double threshold = rankTolerance * diagonalScale;
			if (symmetric.diagonal().minCoeff() < -threshold)
			{
				return fail("RAW_SQUARE_ROOT_NEGATIVE_COVARIANCE_DIRECTION");
			}
			std::vector<int> positive;
			for (int index = 0; index < symmetric.rows(); index++)
			{
				if (symmetric(index, index) > 0)
				{
					positive.push_back(index);
				}
			}
			if ((!allowSemidefinite
				&& positive.size() != static_cast<std::size_t>(symmetric.rows()))
			 || positive.empty())
			{
				return fail("RAW_SQUARE_ROOT_SINGULAR_COVARIANCE");
			}
			squareRoot = MatrixXd::Zero(symmetric.rows(), positive.size());
			for (int column = 0; column < static_cast<int>(positive.size()); column++)
			{
				const int index = positive[column];
				squareRoot(index, column) = std::sqrt(symmetric(index, index));
			}
			return true;
		}
		LLT<MatrixXd> cholesky(symmetric);
		if (cholesky.info() == Eigen::Success)
		{
			squareRoot = MatrixXd(cholesky.matrixL());
			return squareRoot.allFinite();
		}
		// A PSD Kalman state commonly contains explicit deterministic entries
		// (for example the unit/datum state) with exactly zero variance.  Remove
		// only those coordinate directions and factor the stochastic principal
		// block.  A global eigenvalue threshold is invalid here because clock,
		// ionosphere and ambiguity covariances use very different scales.
		std::vector<int> stochastic;
		std::vector<int> deterministic;
		for (int index = 0; index < symmetric.rows(); index++)
		{
			if (symmetric(index, index) > 0)
			{
				stochastic.push_back(index);
			}
			else
			{
				deterministic.push_back(index);
			}
		}
		if (allowSemidefinite && !stochastic.empty() && !deterministic.empty())
		{
			for (int index : deterministic)
			{
				if (symmetric.row(index).cwiseAbs().maxCoeff()
					> rankTolerance * diagonalScale)
				{
					return fail("RAW_SQUARE_ROOT_INCONSISTENT_ZERO_VARIANCE");
				}
			}
			MatrixXd principal(stochastic.size(), stochastic.size());
			for (int row = 0; row < static_cast<int>(stochastic.size()); row++)
			for (int column = 0; column < static_cast<int>(stochastic.size()); column++)
			{
				principal(row, column) = symmetric(
					stochastic[row], stochastic[column]);
			}
			LLT<MatrixXd> principalCholesky(principal);
			if (principalCholesky.info() == Eigen::Success)
			{
				const MatrixXd lower = MatrixXd(principalCholesky.matrixL());
				squareRoot = MatrixXd::Zero(symmetric.rows(), stochastic.size());
				for (int row = 0; row < static_cast<int>(stochastic.size()); row++)
				{
					squareRoot.row(stochastic[row]) = lower.row(row);
				}
				return squareRoot.allFinite();
			}
		}
		Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(symmetric);
		if (eigen.info() != Eigen::Success)
		{
			return fail("RAW_SQUARE_ROOT_COVARIANCE_EIGEN_FAILED");
		}
		const double scale = std::max(
			1.0, eigen.eigenvalues().cwiseAbs().maxCoeff());
		const double threshold = rankTolerance * scale;
		if (eigen.eigenvalues().minCoeff() < -threshold)
		{
			return fail("RAW_SQUARE_ROOT_NEGATIVE_COVARIANCE_DIRECTION");
		}
		std::vector<int> positive;
		for (int index = 0; index < eigen.eigenvalues().size(); index++)
		{
			if (eigen.eigenvalues()(index) > threshold)
			{
				positive.push_back(index);
			}
		}
		if ((!allowSemidefinite
			&& positive.size() != static_cast<std::size_t>(covariance.rows()))
		 || positive.empty())
		{
			return fail("RAW_SQUARE_ROOT_SINGULAR_COVARIANCE");
		}
		squareRoot.resize(covariance.rows(), positive.size());
		for (int column = 0; column < static_cast<int>(positive.size()); column++)
		{
			const int index = positive[column];
			squareRoot.col(column) = eigen.eigenvectors().col(index)
				* std::sqrt(eigen.eigenvalues()(index));
		}
		return squareRoot.allFinite();
	}

	bool inversePositiveSquareRoot(
		const MatrixXd& covariance,
		MatrixXd& inverseSquareRoot)
	{
		const MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
		MatrixXd offDiagonalMatrix = symmetric;
		offDiagonalMatrix.diagonal().setZero();
		const double offDiagonal = offDiagonalMatrix.cwiseAbs().maxCoeff();
		const double scale = std::max(1.0, symmetric.diagonal().cwiseAbs().maxCoeff());
		if (offDiagonal <= rankTolerance * scale)
		{
			if ((symmetric.diagonal().array() <= 0).any())
			{
				return fail("RAW_SQUARE_ROOT_MEASUREMENT_COVARIANCE_NOT_POSITIVE");
			}
			inverseSquareRoot = symmetric.diagonal().cwiseSqrt()
				.cwiseInverse().asDiagonal();
			return true;
		}
		LLT<MatrixXd> cholesky(symmetric);
		if (cholesky.info() != Eigen::Success)
		{
			return fail("RAW_SQUARE_ROOT_MEASUREMENT_COVARIANCE_NOT_POSITIVE");
		}
		inverseSquareRoot = cholesky.matrixL().solve(
			MatrixXd::Identity(covariance.rows(), covariance.cols()));
		return inverseSquareRoot.allFinite();
	}

	bool fail(const std::string& reason)
	{
		failureReason = reason;
		return false;
	}

	void updateBounds()
	{
		maximumRows = std::max(maximumRows, static_cast<int>(factor.rows()));
		maximumColumns = std::max(maximumColumns, static_cast<int>(factor.cols()));
	}

	double rankTolerance = 1e-11;
	bool initialised = false;
	VectorXd anchor;
	MatrixXd support;
	MatrixXd factor;
	VectorXd rhs;
	int maximumRows = 0;
	int maximumColumns = 0;
	int orthogonalDof = 0;
	double orthogonalSquaredNorm = 0;
	int exactConstraintCount = 0;
	std::string failureReason;
};
