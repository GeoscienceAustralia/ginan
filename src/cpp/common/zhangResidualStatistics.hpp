#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>

#include "common/eigenIncluder.hpp"
#include "common/zhangFixedLagSquareRoot.hpp"

enum class ZhangResidualDomain
{
	PREFIT_INNOVATION,
	BATCH_ORTHOGONAL,
	HELD_OUT_PREDICTION,
	TARGET_TO_INTEGER
};

struct ZhangResidualStatistic
{
	bool				valid = false;
	ZhangResidualDomain	domain = ZhangResidualDomain::PREFIT_INNOVATION;
	int				dof = 0;
	int				removedGaugeRank = 0;
	VectorXd			residual;
	VectorXd			whitenedResidual;
	double				squaredNorm = std::numeric_limits<double>::quiet_NaN();
	std::string			failureReason;
};

inline ZhangResidualStatistic zhangCovarianceWhitenedStatistic(
	ZhangResidualDomain domain,
	const VectorXd& residual,
	const MatrixXd& covariance,
	double rankTolerance = 1e-11)
{
	ZhangResidualStatistic result;
	result.domain = domain;
	result.residual = residual;
	const ZhangWhitenedBlock whitened = zhangWhitenRetainedResidual(
		residual, covariance, rankTolerance);
	if (!whitened.valid)
	{
		result.failureReason = whitened.failureReason;
		return result;
	}
	result.dof = whitened.rank;
	result.whitenedResidual = whitened.residual;
	result.squaredNorm = whitened.squaredNorm;
	result.valid = true;
	return result;
}

/** Project continuous quotient/gauge directions before covariance whitening. */
inline ZhangResidualStatistic zhangProjectGaugeAndWhitenStatistic(
	ZhangResidualDomain domain,
	const VectorXd& residual,
	const MatrixXd& covariance,
	const MatrixXd& quotientDirections,
	double rankTolerance = 1e-11)
{
	ZhangResidualStatistic rejected;
	rejected.domain = domain;
	if (covariance.rows() != residual.size()
	 || covariance.cols() != residual.size()
	 || (quotientDirections.size() != 0
		&& quotientDirections.rows() != residual.size()))
	{
		rejected.failureReason = "INVALID_GAUGE_PROJECTION_DIMENSIONS";
		return rejected;
	}
	if (quotientDirections.cols() == 0)
	{
		return zhangCovarianceWhitenedStatistic(
			domain, residual, covariance, rankTolerance);
	}

	Eigen::JacobiSVD<MatrixXd> svd(
		quotientDirections.transpose(), Eigen::ComputeFullV);
	const double scale = std::max(
		1.0,
		svd.singularValues().size() == 0
			? 0.0 : svd.singularValues().maxCoeff());
	svd.setThreshold(rankTolerance * scale);
	const int gaugeRank = svd.rank();
	const int invariantRank = residual.size() - gaugeRank;
	if (invariantRank <= 0)
	{
		rejected.removedGaugeRank = gaugeRank;
		rejected.failureReason = "NO_QUOTIENT_INVARIANT_TARGET_DIRECTION";
		return rejected;
	}
	const MatrixXd invariantBasis = svd.matrixV().rightCols(invariantRank);
	ZhangResidualStatistic result = zhangCovarianceWhitenedStatistic(
		domain,
		invariantBasis.transpose() * residual,
		invariantBasis.transpose() * covariance * invariantBasis,
		rankTolerance);
	result.removedGaugeRank = gaugeRank;
	return result;
}

/** Prefit innovation v=y-H*x_prior with S=H*P_prior*H^T+R. */
inline ZhangResidualStatistic zhangPrefitInnovationStatistic(
	const VectorXd& observation,
	const MatrixXd& design,
	const MatrixXd& measurementCovariance,
	const VectorXd& priorMean,
	const MatrixXd& priorCovariance,
	double rankTolerance = 1e-11)
{
	ZhangResidualStatistic result;
	result.domain = ZhangResidualDomain::PREFIT_INNOVATION;
	if (design.rows() != observation.size()
	 || design.cols() != priorMean.size()
	 || priorCovariance.rows() != priorMean.size()
	 || priorCovariance.cols() != priorMean.size()
	 || measurementCovariance.rows() != observation.size()
	 || measurementCovariance.cols() != observation.size())
	{
		result.failureReason = "INVALID_PREFIT_DIMENSIONS";
		return result;
	}
	return zhangCovarianceWhitenedStatistic(
		ZhangResidualDomain::PREFIT_INNOVATION,
		observation - design * priorMean,
		design * priorCovariance * design.transpose()
			+ measurementCovariance,
		rankTolerance);
}

/** Orthogonal residual of an already whitened batch A*x=b.
 *
 * Its degrees of freedom are rows(A)-rank(A).  This is deliberately not the
 * prefit innovation dimension and not the retained integer-target rank.
 */
inline ZhangResidualStatistic zhangBatchOrthogonalResidualStatistic(
	const MatrixXd& whitenedDesign,
	const VectorXd& whitenedRightHandSide,
	double rankTolerance = 1e-11)
{
	ZhangResidualStatistic result;
	result.domain = ZhangResidualDomain::BATCH_ORTHOGONAL;
	if (whitenedDesign.rows() != whitenedRightHandSide.size())
	{
		result.failureReason = "INVALID_BATCH_RESIDUAL_DIMENSIONS";
		return result;
	}
	Eigen::ColPivHouseholderQR<MatrixXd> qr(whitenedDesign);
	qr.setThreshold(rankTolerance);
	const int rank = qr.rank();
	result.dof = whitenedDesign.rows() - rank;
	const VectorXd rotated = qr.householderQ().adjoint()
		* whitenedRightHandSide;
	result.residual = rotated.tail(result.dof);
	result.whitenedResidual = result.residual;
	result.squaredNorm = result.residual.squaredNorm();
	result.valid = result.residual.allFinite()
		&& std::isfinite(result.squaredNorm);
	if (!result.valid)
	{
		result.failureReason = "NONFINITE_BATCH_ORTHOGONAL_RESIDUAL";
	}
	return result;
}

/** Held-out prediction uses no fitted residual from the training batch. */
inline ZhangResidualStatistic zhangHeldOutPredictionStatistic(
	const VectorXd& observation,
	const MatrixXd& design,
	const MatrixXd& measurementCovariance,
	const VectorXd& posteriorMean,
	const MatrixXd& posteriorCovariance,
	double rankTolerance = 1e-11)
{
	ZhangResidualStatistic result;
	result.domain = ZhangResidualDomain::HELD_OUT_PREDICTION;
	if (design.rows() != observation.size()
	 || design.cols() != posteriorMean.size()
	 || posteriorCovariance.rows() != posteriorMean.size()
	 || posteriorCovariance.cols() != posteriorMean.size()
	 || measurementCovariance.rows() != observation.size()
	 || measurementCovariance.cols() != observation.size())
	{
		result.failureReason = "INVALID_HELD_OUT_DIMENSIONS";
		return result;
	}
	return zhangCovarianceWhitenedStatistic(
		ZhangResidualDomain::HELD_OUT_PREDICTION,
		observation - design * posteriorMean,
		design * posteriorCovariance * design.transpose()
			+ measurementCovariance,
		rankTolerance);
}

/** Integer-candidate residual after removing quotient/gauge directions.
 *
 * Columns of quotientDirections span translations that do not change the
 * physical integer class.  U spans null(D^T), so whitening is performed on
 * U^T(mu-z) with covariance U^T Q U.  Whitening Q before this projection
 * would assign false degrees of freedom to unresolved integer gauges.
 */
inline ZhangResidualStatistic zhangTargetToIntegerStatistic(
	const VectorXd& targetMean,
	const MatrixXd& targetCovariance,
	const VectorXd& integerCandidate,
	const MatrixXd& quotientDirections = MatrixXd(),
	double rankTolerance = 1e-11)
{
	ZhangResidualStatistic result;
	result.domain = ZhangResidualDomain::TARGET_TO_INTEGER;
	if (targetCovariance.rows() != targetMean.size()
	 || targetCovariance.cols() != targetMean.size()
	 || integerCandidate.size() != targetMean.size()
	 || (quotientDirections.size() != 0
		&& quotientDirections.rows() != targetMean.size()))
	{
		result.failureReason = "INVALID_INTEGER_RESIDUAL_DIMENSIONS";
		return result;
	}

	result = zhangProjectGaugeAndWhitenStatistic(
		ZhangResidualDomain::TARGET_TO_INTEGER,
		targetMean - integerCandidate,
		targetCovariance,
		quotientDirections,
		rankTolerance);
	return result;
}
