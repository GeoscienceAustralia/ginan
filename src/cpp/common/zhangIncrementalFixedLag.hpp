#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>

#include "common/eigenIncluder.hpp"
#include "common/zhangFixedLagSquareRoot.hpp"

struct ZhangIncrementalFixedLagSummary
{
	bool valid = false;
	int activeEpochs = 0;
	int separatorDimension = 0;
	int storedRows = 0;
	int storedColumns = 0;
	int maximumStoredRows = 0;
	int maximumStoredColumns = 0;
	int retiredOrthogonalDof = 0;
	double retiredOrthogonalSquaredNorm = 0;
	std::string failureReason;
};

/** Incremental square-root fixed-lag posterior over bounded separators.
 *
 * The class never retains the historical measurement matrix.  At an epoch it
 * combines the existing square-root prior, the transition, and the new
 * measurement block.  Epoch-local nuisance columns are projected out
 * immediately.  The remaining factor is QR-compressed, and separators older
 * than lagEpochs are marginalised once into a new square-root prior.
 *
 * State ordering is [separator(epoch_0), ..., separator(epoch_k)].  Local
 * nuisance variables are temporary leading columns and never enter the stored
 * factor.  Exact S-basis changes are affine coordinate substitutions on the
 * latest separator, not stochastic pseudo-observations.
 */
class ZhangIncrementalFixedLagSquareRoot
{
public:
	explicit ZhangIncrementalFixedLagSquareRoot(
		int lagEpochs,
		double relativeRankTolerance = 1e-11)
	:	lag(std::max(1, lagEpochs)),
		rankTolerance(relativeRankTolerance)
	{}

	bool initialise(
		const VectorXd& separatorMean,
		const MatrixXd& separatorCovariance)
	{
		clear();
		if (separatorMean.size() == 0
		 || separatorCovariance.rows() != separatorMean.size()
		 || separatorCovariance.cols() != separatorMean.size()
		 || !separatorMean.allFinite() || !separatorCovariance.allFinite())
		{
			return fail("INVALID_SEPARATOR_BOUNDARY");
		}
		MatrixXd inverseSquareRoot;
		if (!inversePositiveSquareRoot(
			separatorCovariance, inverseSquareRoot, false))
		{
			return false;
		}
		separatorSize = separatorMean.size();
		active = 1;
		factor = inverseSquareRoot;
		rhs = inverseSquareRoot * separatorMean;
		initialised = true;
		updateBounds();
		return true;
	}

	/** Add the complete finally accepted measurement block for the latest epoch.
	 * localDesign columns are epoch-local nuisance shared by all rows in this
	 * call; they are eliminated before the method returns. */
	bool addLatestMeasurement(
		const MatrixXd& separatorDesign,
		const MatrixXd& localDesign,
		const MatrixXd& measurementCovariance,
		const VectorXd& observation)
	{
		if (!initialised
		 || separatorDesign.rows() != observation.size()
		 || separatorDesign.cols() != separatorSize
		 || localDesign.rows() != observation.size()
		 || measurementCovariance.rows() != observation.size()
		 || measurementCovariance.cols() != observation.size()
		 || !separatorDesign.allFinite() || !localDesign.allFinite()
		 || !measurementCovariance.allFinite() || !observation.allFinite())
		{
			return fail("INVALID_INCREMENTAL_MEASUREMENT");
		}
		MatrixXd inverseSquareRoot;
		if (!inversePositiveSquareRoot(
			measurementCovariance, inverseSquareRoot, false))
		{
			return false;
		}

		const int localColumns = localDesign.cols();
		const int storedColumns = factor.cols();
		MatrixXd augmented = MatrixXd::Zero(
			factor.rows() + observation.size(),
			localColumns + storedColumns);
		augmented.block(0, localColumns, factor.rows(), storedColumns) = factor;
		if (localColumns > 0)
		{
			augmented.bottomLeftCorner(observation.size(), localColumns) =
				inverseSquareRoot * localDesign;
		}
		augmented.block(
			factor.rows(), localColumns + storedColumns - separatorSize,
			observation.size(), separatorSize) =
			inverseSquareRoot * separatorDesign;
		VectorXd augmentedRhs(factor.rows() + observation.size());
		augmentedRhs.head(factor.rows()) = rhs;
		augmentedRhs.tail(observation.size()) =
			inverseSquareRoot * observation;

		MatrixXd projected;
		VectorXd projectedRhs;
		if (!projectLeading(
			augmented, augmentedRhs, localColumns,
			projected, projectedRhs))
		{
			return false;
		}
		if (!compress(projected, projectedRhs, true))
		{
			return false;
		}
		updateBounds();
		return true;
	}

	/** Append a new separator with x_new=F*x_latest+w and Q=cov(w). */
	bool advance(
		const MatrixXd& transition,
		const MatrixXd& processCovariance)
	{
		if (!initialised
		 || transition.rows() != separatorSize
		 || transition.cols() != separatorSize
		 || processCovariance.rows() != separatorSize
		 || processCovariance.cols() != separatorSize
		 || !transition.allFinite() || !processCovariance.allFinite())
		{
			return fail("INVALID_INCREMENTAL_TRANSITION");
		}
		MatrixXd inverseSquareRoot;
		if (!inversePositiveSquareRoot(
			processCovariance, inverseSquareRoot, false))
		{
			return false;
		}
		const int oldColumns = factor.cols();
		MatrixXd augmented = MatrixXd::Zero(
			factor.rows() + separatorSize,
			oldColumns + separatorSize);
		augmented.topLeftCorner(factor.rows(), oldColumns) = factor;
		augmented.block(
			factor.rows(), oldColumns - separatorSize,
			separatorSize, separatorSize) =
			-inverseSquareRoot * transition;
		augmented.bottomRightCorner(separatorSize, separatorSize) =
			inverseSquareRoot;
		VectorXd augmentedRhs = VectorXd::Zero(
			factor.rows() + separatorSize);
		augmentedRhs.head(factor.rows()) = rhs;
		factor = std::move(augmented);
		rhs = std::move(augmentedRhs);
		active++;

		if (!compress(factor, rhs, true))
		{
			return false;
		}
		while (active > lag)
		{
			if (!marginaliseOldest())
			{
				return false;
			}
		}
		updateBounds();
		return true;
	}

	/** x_new=T*x_old+b for the latest separator. */
	bool applyExactLatestCoordinateTransform(
		const MatrixXd& transform,
		const VectorXd& translation)
	{
		if (!initialised
		 || transform.rows() != separatorSize
		 || transform.cols() != separatorSize
		 || translation.size() != separatorSize
		 || !transform.allFinite() || !translation.allFinite())
		{
			return fail("INVALID_INCREMENTAL_EXACT_TRANSFORM");
		}
		Eigen::FullPivLU<MatrixXd> inverse(transform);
		inverse.setThreshold(rankTolerance);
		if (!inverse.isInvertible())
		{
			return fail("SINGULAR_INCREMENTAL_EXACT_TRANSFORM");
		}
		const MatrixXd transformInverse = inverse.inverse();
		const int latestColumn = factor.cols() - separatorSize;
		const MatrixXd oldBlock = factor.middleCols(
			latestColumn, separatorSize);
		const MatrixXd newBlock = oldBlock * transformInverse;
		rhs += newBlock * translation;
		factor.middleCols(latestColumn, separatorSize) = newBlock;
		if (!compress(factor, rhs, false))
		{
			return false;
		}
		updateBounds();
		return true;
	}

	ZhangSquareRootMarginal latestMarginal() const
	{
		if (!initialised)
		{
			ZhangSquareRootMarginal result;
			result.failureReason = "INCREMENTAL_WINDOW_NOT_INITIALISED";
			return result;
		}
		return zhangMarginaliseSquareRootFactors(
			factor.sparseView(), rhs,
			factor.cols() - separatorSize, rankTolerance);
	}

	ZhangIncrementalFixedLagSummary summary() const
	{
		ZhangIncrementalFixedLagSummary result;
		result.valid = initialised && failureReason.empty();
		result.activeEpochs = active;
		result.separatorDimension = separatorSize;
		result.storedRows = factor.rows();
		result.storedColumns = factor.cols();
		result.maximumStoredRows = maximumRows;
		result.maximumStoredColumns = maximumColumns;
		result.retiredOrthogonalDof = orthogonalDof;
		result.retiredOrthogonalSquaredNorm = orthogonalSquaredNorm;
		result.failureReason = failureReason;
		return result;
	}

private:
	bool marginaliseOldest()
	{
		MatrixXd projected;
		VectorXd projectedRhs;
		if (!projectLeading(
			factor, rhs, separatorSize, projected, projectedRhs))
		{
			return false;
		}
		active--;
		return compress(projected, projectedRhs, true);
	}

	bool projectLeading(
		const MatrixXd& inputFactor,
		const VectorXd& inputRhs,
		int leadingColumns,
		MatrixXd& projected,
		VectorXd& projectedRhs)
	{
		if (leadingColumns == 0)
		{
			projected = inputFactor;
			projectedRhs = inputRhs;
			return true;
		}
		if (leadingColumns < 0 || leadingColumns >= inputFactor.cols()
		 || inputFactor.rows() != inputRhs.size())
		{
			return fail("INVALID_INCREMENTAL_PROJECTION");
		}
		Eigen::ColPivHouseholderQR<MatrixXd> qr(
			inputFactor.leftCols(leadingColumns));
		qr.setThreshold(rankTolerance);
		const int eliminatedRank = qr.rank();
		const MatrixXd rotatedRetained = qr.householderQ().adjoint()
			* inputFactor.rightCols(inputFactor.cols() - leadingColumns);
		const VectorXd rotatedRhs = qr.householderQ().adjoint() * inputRhs;
		const int retainedRows = inputFactor.rows() - eliminatedRank;
		if (retainedRows < inputFactor.cols() - leadingColumns)
		{
			return fail("INCREMENTAL_RETAINED_SYSTEM_UNDERDETERMINED");
		}
		projected = rotatedRetained.bottomRows(retainedRows);
		projectedRhs = rotatedRhs.tail(retainedRows);
		return true;
	}

	bool compress(
		const MatrixXd& inputFactor,
		const VectorXd& inputRhs,
		bool accountOrthogonalResidual)
	{
		if (inputFactor.rows() != inputRhs.size()
		 || inputFactor.rows() < inputFactor.cols())
		{
			return fail("INVALID_INCREMENTAL_COMPRESSION");
		}
		Eigen::ColPivHouseholderQR<MatrixXd> qr(inputFactor);
		qr.setThreshold(rankTolerance);
		const int columns = inputFactor.cols();
		if (qr.rank() != columns)
		{
			return fail("INCREMENTAL_SEPARATOR_RANK_DEFICIENT");
		}
		const VectorXd rotatedRhs = qr.householderQ().adjoint() * inputRhs;
		const MatrixXd upper = qr.matrixR().topRows(columns)
			.leftCols(columns).template triangularView<Eigen::Upper>();
		factor = upper * qr.colsPermutation().transpose();
		rhs = rotatedRhs.head(columns);
		if (accountOrthogonalResidual && inputFactor.rows() > columns)
		{
			const VectorXd retired = rotatedRhs.tail(
				inputFactor.rows() - columns);
			orthogonalDof += retired.size();
			orthogonalSquaredNorm += retired.squaredNorm();
		}
		return factor.allFinite() && rhs.allFinite()
			? true : fail("NONFINITE_INCREMENTAL_COMPRESSION");
	}

	bool inversePositiveSquareRoot(
		const MatrixXd& covariance,
		MatrixXd& inverseSquareRoot,
		bool allowSemidefinite)
	{
		const MatrixXd symmetric = 0.5
			* (covariance + covariance.transpose());
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
		Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(symmetric);
		if (eigen.info() != Eigen::Success)
		{
			return fail("INCREMENTAL_COVARIANCE_EIGEN_FAILED");
		}
		const double scale = std::max(
			1.0, eigen.eigenvalues().cwiseAbs().maxCoeff());
		const double threshold = rankTolerance * scale;
		if (eigen.eigenvalues().minCoeff() < -threshold)
		{
			return fail("INCREMENTAL_NEGATIVE_COVARIANCE_DIRECTION");
		}
		int rank = 0;
		for (int index = 0; index < eigen.eigenvalues().size(); index++)
		{
			if (eigen.eigenvalues()(index) > threshold)
			{
				rank++;
			}
		}
		if (!allowSemidefinite && rank != covariance.rows())
		{
			return fail("INCREMENTAL_SINGULAR_COVARIANCE");
		}
		inverseSquareRoot = MatrixXd::Zero(rank, covariance.rows());
		int output = 0;
		for (int index = 0; index < eigen.eigenvalues().size(); index++)
		{
			if (eigen.eigenvalues()(index) <= threshold)
			{
				continue;
			}
			inverseSquareRoot.row(output++) =
				eigen.eigenvectors().col(index).transpose()
				/ std::sqrt(eigen.eigenvalues()(index));
		}
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
		maximumColumns = std::max(
			maximumColumns, static_cast<int>(factor.cols()));
	}

	void clear()
	{
		initialised = false;
		separatorSize = 0;
		active = 0;
		maximumRows = 0;
		maximumColumns = 0;
		orthogonalDof = 0;
		orthogonalSquaredNorm = 0;
		factor.resize(0, 0);
		rhs.resize(0);
		failureReason.clear();
	}

	int lag = 1;
	double rankTolerance = 1e-11;
	bool initialised = false;
	int separatorSize = 0;
	int active = 0;
	int maximumRows = 0;
	int maximumColumns = 0;
	int orthogonalDof = 0;
	double orthogonalSquaredNorm = 0;
	MatrixXd factor;
	VectorXd rhs;
	std::string failureReason;
};
