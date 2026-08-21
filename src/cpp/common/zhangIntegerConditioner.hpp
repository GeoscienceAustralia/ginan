#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"
#include "common/zhangIarGainAudit.hpp"

struct ZhangIntegerConditionedState
{
	bool     valid = false;
	int      constraintRows = 0;
	int      constraintRank = 0;
	int      covarianceRank = 0;
	double   minimumConstraintEigenvalue =
		std::numeric_limits<double>::quiet_NaN();
	double   minimumSquareRootDiagonal =
		std::numeric_limits<double>::quiet_NaN();
	double   maximumSquareRootDiagonal =
		std::numeric_limits<double>::quiet_NaN();
	double   maximumConstraintResidual =
		std::numeric_limits<double>::quiet_NaN();
	VectorXd mean;
	MatrixXd covariance;
	std::string failureReason;
};

inline bool zhangIntegerConditioningInputsValid(
	const VectorXd& mean,
	const MatrixXd& covariance,
	const ZhangIarFunctional& constraints,
	const VectorXd& integers);

/** Independent square-root equality conditioning without P^-1 or normal
 * equations.
 *
 * A rank-revealing eigensquare-root P=L*L' is followed by an orthogonal QR of
 * (A*L)'.  For B=A*L and B'=Q*R*Pi', the minimum-norm constrained increment
 * is L*Q1*R^{-T}*Pi'*innovation, and the conditional covariance is
 * P-(L*Q1)*(L*Q1)'.  This path is deliberately independent of the analytical
 * LDLT solve above and remains defined for positive-semidefinite covariances
 * produced by earlier exact WL constraints. */
inline ZhangIntegerConditionedState
zhangConditionIntegersSquareRootOrthogonal(
	const VectorXd& mean,
	const MatrixXd& covariance,
	const ZhangIarFunctional& constraints,
	const VectorXd& integers)
{
	ZhangIntegerConditionedState result;
	result.constraintRows = constraints.rows();
	if (!zhangIntegerConditioningInputsValid(
			mean, covariance, constraints, integers))
	{
		result.failureReason = "INVALID_INTEGER_SQUARE_ROOT_INPUT";
		return result;
	}

	const MatrixXd symmetric =
		0.5 * (covariance + covariance.transpose());
	Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(symmetric);
	if (eigen.info() != Eigen::Success || !eigen.eigenvalues().allFinite())
	{
		result.failureReason = "INTEGER_SQUARE_ROOT_EIGEN_FAILED";
		return result;
	}
	const double maximumEigenvalue = eigen.eigenvalues().maxCoeff();
	const double eigenTolerance = std::max(
		1e-14,
		std::max(1.0, std::abs(maximumEigenvalue))
			* std::numeric_limits<double>::epsilon()
			* covariance.rows() * 64.0);
	if (eigen.eigenvalues().minCoeff() < -eigenTolerance)
	{
		result.failureReason = "INTEGER_SQUARE_ROOT_COVARIANCE_NOT_PSD";
		return result;
	}
	std::vector<int> retained;
	retained.reserve(covariance.rows());
	for (int index = 0; index < eigen.eigenvalues().size(); index++)
	{
		if (eigen.eigenvalues()(index) > eigenTolerance)
		{
			retained.push_back(index);
		}
	}
	result.covarianceRank = retained.size();
	if (result.covarianceRank < constraints.rows())
	{
		result.failureReason = "INTEGER_SQUARE_ROOT_INSUFFICIENT_COVARIANCE_RANK";
		return result;
	}
	MatrixXd squareRoot(covariance.rows(), result.covarianceRank);
	for (int column = 0; column < result.covarianceRank; column++)
	{
		const int eigenIndex = retained[column];
		squareRoot.col(column) = eigen.eigenvectors().col(eigenIndex)
			* std::sqrt(eigen.eigenvalues()(eigenIndex));
	}

	const MatrixXd whitenedConstraints = constraints * squareRoot;
	Eigen::ColPivHouseholderQR<MatrixXd> qr(
		whitenedConstraints.transpose());
	qr.setThreshold(std::max(
		1e-14,
		std::numeric_limits<double>::epsilon()
			* std::max(whitenedConstraints.rows(), whitenedConstraints.cols())
			* 64.0));
	result.constraintRank = qr.rank();
	if (result.constraintRank != constraints.rows())
	{
		result.failureReason = "INTEGER_SQUARE_ROOT_CONSTRAINT_NOT_FULL_ROW_RANK";
		return result;
	}

	const int rows = constraints.rows();
	const MatrixXd upper = qr.matrixR().topLeftCorner(rows, rows)
		.template triangularView<Eigen::Upper>();
	const VectorXd absoluteDiagonal = upper.diagonal().cwiseAbs();
	result.minimumSquareRootDiagonal = absoluteDiagonal.minCoeff();
	result.maximumSquareRootDiagonal = absoluteDiagonal.maxCoeff();
	if (!upper.allFinite() || result.minimumSquareRootDiagonal <= 0)
	{
		result.failureReason = "INTEGER_SQUARE_ROOT_QR_SINGULAR";
		return result;
	}
	MatrixXd selector = MatrixXd::Zero(result.covarianceRank, rows);
	selector.topRows(rows).setIdentity();
	const MatrixXd q1 = qr.householderQ() * selector;
	const VectorXd innovation = integers - constraints * mean;
	const VectorXd permutedInnovation =
		qr.colsPermutation().transpose() * innovation;
	const VectorXd orthogonalIncrement = upper.transpose()
		.template triangularView<Eigen::Lower>()
		.solve(permutedInnovation);
	const MatrixXd constrainedDirections = squareRoot * q1;
	result.mean = mean + constrainedDirections * orthogonalIncrement;
	result.covariance = symmetric
		- constrainedDirections * constrainedDirections.transpose();
	result.covariance = 0.5
		* (result.covariance + result.covariance.transpose());
	result.maximumConstraintResidual =
		(constraints * result.mean - integers).cwiseAbs().maxCoeff();
	if (!result.mean.allFinite() || !result.covariance.allFinite()
	 || !std::isfinite(result.maximumConstraintResidual))
	{
		result.failureReason = "NONFINITE_INTEGER_SQUARE_ROOT_STATE";
		return result;
	}
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}

inline bool zhangIntegerConditioningInputsValid(
	const VectorXd& mean,
	const MatrixXd& covariance,
	const ZhangIarFunctional& constraints,
	const VectorXd& integers)
{
	return mean.size() > 0
		&& covariance.rows() == mean.size()
		&& covariance.cols() == mean.size()
		&& constraints.rows() > 0
		&& constraints.cols() == mean.size()
		&& integers.size() == constraints.rows()
		&& mean.allFinite() && covariance.allFinite()
		&& integers.allFinite() && zhangIarSparseAllFinite(constraints);
}

inline bool zhangFactorConstraintCovariance(
	const MatrixXd& covariance,
	const ZhangIarFunctional& constraints,
	MatrixXd& cross,
	MatrixXd& constraintCovariance,
	Eigen::LDLT<MatrixXd>& factor,
	ZhangIntegerConditionedState& result,
	double diagonalVariance = 0)
{
	cross = covariance * constraints.transpose();
	constraintCovariance = constraints * cross;
	constraintCovariance = 0.5
		* (constraintCovariance + constraintCovariance.transpose());
	Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(constraintCovariance);
	if (eigen.info() != Eigen::Success || !eigen.eigenvalues().allFinite())
	{
		result.failureReason = "INTEGER_CONSTRAINT_EIGEN_FAILED";
		return false;
	}
	const double maximum = eigen.eigenvalues().maxCoeff();
	const double tolerance = std::max(
		1e-14,
		std::max(1.0, std::abs(maximum))
			* std::numeric_limits<double>::epsilon()
			* constraintCovariance.rows() * 64.0);
	result.constraintRank =
		(eigen.eigenvalues().array() > tolerance).count();
	if (result.constraintRank != constraints.rows())
	{
		result.failureReason = "INTEGER_CONSTRAINT_NOT_FULL_ROW_RANK";
		return false;
	}
	result.minimumConstraintEigenvalue = eigen.eigenvalues().minCoeff();
	MatrixXd innovationCovariance = constraintCovariance;
	innovationCovariance.diagonal().array() += diagonalVariance;
	factor.compute(innovationCovariance);
	if (factor.info() != Eigen::Success || !factor.isPositive())
	{
		result.failureReason = "INTEGER_CONSTRAINT_LDLT_FAILED";
		return false;
	}
	return true;
}

/** Exact one-shot Gaussian conditioning for primitive full-row-rank A*x=z. */
inline ZhangIntegerConditionedState zhangConditionIntegersExact(
	const VectorXd& mean,
	const MatrixXd& covariance,
	const ZhangIarFunctional& constraints,
	const VectorXd& integers)
{
	ZhangIntegerConditionedState result;
	result.constraintRows = constraints.rows();
	if (!zhangIntegerConditioningInputsValid(
			mean, covariance, constraints, integers))
	{
		result.failureReason = "INVALID_INTEGER_CONDITIONING_INPUT";
		return result;
	}
	MatrixXd cross;
	MatrixXd constraintCovariance;
	Eigen::LDLT<MatrixXd> factor;
	if (!zhangFactorConstraintCovariance(
			covariance, constraints, cross, constraintCovariance,
			factor, result))
	{
		return result;
	}
	const VectorXd innovation = integers - constraints * mean;
	result.mean = mean + cross * factor.solve(innovation);
	result.covariance = covariance
		- cross * factor.solve(cross.transpose());
	result.covariance = 0.5
		* (result.covariance + result.covariance.transpose());
	result.maximumConstraintResidual =
		(constraints * result.mean - integers).cwiseAbs().maxCoeff();
	if (!result.mean.allFinite() || !result.covariance.allFinite())
	{
		result.failureReason = "NONFINITE_INTEGER_CONDITIONED_STATE";
		return result;
	}
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}

/** Independent near-zero-noise pseudo-observation re-solve.  Joseph covariance
 * form is deliberately used instead of the exact subtraction above. */
inline ZhangIntegerConditionedState zhangConditionIntegersPseudoObservation(
	const VectorXd& mean,
	const MatrixXd& covariance,
	const ZhangIarFunctional& constraints,
	const VectorXd& integers,
	double sigmaCycles = 1e-8)
{
	ZhangIntegerConditionedState result;
	result.constraintRows = constraints.rows();
	if (!zhangIntegerConditioningInputsValid(
			mean, covariance, constraints, integers)
	 || !std::isfinite(sigmaCycles) || sigmaCycles <= 0)
	{
		result.failureReason = "INVALID_INTEGER_PSEUDO_OBSERVATION_INPUT";
		return result;
	}
	MatrixXd cross;
	MatrixXd constraintCovariance;
	Eigen::LDLT<MatrixXd> factor;
	const double variance = sigmaCycles * sigmaCycles;
	if (!zhangFactorConstraintCovariance(
			covariance, constraints, cross, constraintCovariance,
			factor, result, variance))
	{
		return result;
	}
	const MatrixXd gain = factor.solve(cross.transpose()).transpose();
	const VectorXd innovation = integers - constraints * mean;
	result.mean = mean + gain * innovation;
	const MatrixXd identity = MatrixXd::Identity(mean.size(), mean.size());
	const MatrixXd residualTransform = identity - gain * constraints;
	result.covariance = residualTransform * covariance
		* residualTransform.transpose()
		+ variance * gain * gain.transpose();
	result.covariance = 0.5
		* (result.covariance + result.covariance.transpose());
	result.maximumConstraintResidual =
		(constraints * result.mean - integers).cwiseAbs().maxCoeff();
	if (!result.mean.allFinite() || !result.covariance.allFinite())
	{
		result.failureReason = "NONFINITE_INTEGER_PSEUDO_OBSERVATION_STATE";
		return result;
	}
	result.valid = true;
	result.failureReason = "NONE";
	return result;
}
