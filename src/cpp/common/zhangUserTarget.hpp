#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>

#include "common/eigenIncluder.hpp"

/** Affine scalar defined by the user observation equation.
 *
 * The offset is intentionally explicit: an exact S-basis change can contain
 * an integer translation even when its linear part is unimodular.
 */
struct ZhangAffineUserTarget
{
	VectorXd	row;
	double		offset = 0;
	std::string	units;

	double value(const VectorXd& state) const
	{
		if (row.size() != state.size())
		{
			return std::numeric_limits<double>::quiet_NaN();
		}
		return row.dot(state) + offset;
	}

	double variance(const MatrixXd& covariance) const
	{
		if (covariance.rows() != row.size()
		 || covariance.cols() != row.size())
		{
			return std::numeric_limits<double>::quiet_NaN();
		}
		return (row.transpose() * covariance * row)(0, 0);
	}
};

inline ZhangAffineUserTarget zhangUserPhaseCorrectionTarget(
	int		stateSize,
	int		clockIndex,
	int		phaseBiasIndex,
	double	wavelengthMetres,
	long long	alignmentCycles)
{
	ZhangAffineUserTarget target;
	target.row = VectorXd::Zero(stateSize);
	target.units = "metre";
	if (clockIndex < 0 || clockIndex >= stateSize
	 || phaseBiasIndex < 0 || phaseBiasIndex >= stateSize
	 || !std::isfinite(wavelengthMetres)
	 || wavelengthMetres <= 0)
	{
		target.row = VectorXd();
		target.offset = std::numeric_limits<double>::quiet_NaN();
		return target;
	}

	// c^phi_s,j = C_s - (B^phi_s,j + n_align * lambda_j).
	target.row(clockIndex) = +1;
	target.row(phaseBiasIndex) = -1;
	target.offset = -static_cast<double>(alignmentCycles) * wavelengthMetres;
	return target;
}

/** Hou-style OSB-like phase correction in the internal service datum.
 *
 * The correction is an estimable affine coordinate, not an absolute hardware
 * phase delay and not an absolute satellite ambiguity.  One common additive
 * datum per system/signal is absorbed by the user receiver phase datum. */
inline ZhangAffineUserTarget zhangHouOsbLikePhaseCorrectionTarget(
	int		stateSize,
	int		clockIndex,
	int		phaseBiasIndex,
	double	wavelengthMetres,
	double	alignmentCycles)
{
	ZhangAffineUserTarget target;
	target.row = VectorXd::Zero(stateSize);
	target.units = "metre";
	if (clockIndex < 0 || clockIndex >= stateSize
	 || phaseBiasIndex < 0 || phaseBiasIndex >= stateSize
	 || !std::isfinite(wavelengthMetres)
	 || wavelengthMetres <= 0
	 || !std::isfinite(alignmentCycles))
	{
		target.row = VectorXd();
		target.offset = std::numeric_limits<double>::quiet_NaN();
		return target;
	}
	target.row(clockIndex) = +1;
	target.row(phaseBiasIndex) = -1;
	target.offset = -alignmentCycles * wavelengthMetres;
	return target;
}

/** Evaluate the phase correction in the legacy product operation order.
 *
 * This is algebraically identical to the affine target above.  Keeping the
 * parentheses preserves byte-for-byte product regression at CSV precision.
 */
inline double zhangUserPhaseCorrectionValue(
	double		clockMetres,
	double		phaseBiasMetres,
	double		wavelengthMetres,
	long long	alignmentCycles)
{
	return clockMetres
		 - (phaseBiasMetres
			+ static_cast<double>(alignmentCycles) * wavelengthMetres);
}

inline ZhangAffineUserTarget zhangLinearCombination(
	const ZhangAffineUserTarget& first,
	double firstCoefficient,
	const ZhangAffineUserTarget& second,
	double secondCoefficient,
	const std::string& units)
{
	ZhangAffineUserTarget result;
	if (first.row.size() == 0 || first.row.size() != second.row.size()
	 || !std::isfinite(firstCoefficient) || !std::isfinite(secondCoefficient))
	{
		result.offset = std::numeric_limits<double>::quiet_NaN();
		return result;
	}
	result.row = firstCoefficient * first.row
			   + secondCoefficient * second.row;
	result.offset = firstCoefficient * first.offset
				  + secondCoefficient * second.offset;
	result.units = units;
	return result;
}

/** Transport a target through x_to = A_to_from x_from + b_to_from.
 *
 * The returned target is expressed in x_to coordinates.  A non-square or
 * rank-deficient change is rejected; join/leave events must instead project a
 * target on the explicitly declared common physical subspace.
 */
inline bool zhangTransportAffineUserTarget(
	const ZhangAffineUserTarget& from,
	const MatrixXd&              AToFrom,
	const VectorXd&              bToFrom,
	ZhangAffineUserTarget&       to,
	double                       rankTolerance = 1e-12)
{
	if (from.row.size() == 0
	 || AToFrom.rows() != AToFrom.cols()
	 || AToFrom.cols() != from.row.size()
	 || bToFrom.size() != from.row.size())
	{
		return false;
	}

	Eigen::FullPivLU<MatrixXd> factor(AToFrom.transpose());
	factor.setThreshold(rankTolerance);
	if (factor.rank() != AToFrom.cols())
	{
		return false;
	}

	to.row = factor.solve(from.row);
	to.offset = from.offset - to.row.dot(bToFrom);
	to.units = from.units;
	return to.row.allFinite() && std::isfinite(to.offset);
}

inline double zhangProtectedRelativeVarianceDifference(
	double first,
	double second,
	double varianceFloor = 1e-24)
{
	if (!std::isfinite(first) || !std::isfinite(second))
	{
		return std::numeric_limits<double>::infinity();
	}
	return std::abs(first - second)
		 / std::max({std::abs(first), std::abs(second), varianceFloor});
}

enum class ZhangFixedLagIdentityTransition
{
	CONTINUE,
	CONTINUE_EXACT_TRANSFORM,
	START_NEW_TARGET,
	RETIRE_TARGET,
	RESET_PHYSICAL_IDENTITY,
	RESET_EXACT_TRANSFORM_UNAVAILABLE
};

/** Classify a fixed-lag target transition independently of a tree name.
 *
 * phaseIdentityChanged and physicalArcVersionChanged describe real physical
 * events.  coordinateChanged alone is harmless only when the complete affine
 * target/factor transport is available.
 */
inline ZhangFixedLagIdentityTransition zhangClassifyFixedLagTransition(
	bool targetExistedBefore,
	bool targetExistsNow,
	bool phaseIdentityChanged,
	bool physicalArcVersionChanged,
	bool coordinateChanged,
	bool exactTargetTransportAvailable)
{
	if (!targetExistedBefore && targetExistsNow)
	{
		return ZhangFixedLagIdentityTransition::START_NEW_TARGET;
	}
	if (targetExistedBefore && !targetExistsNow)
	{
		return ZhangFixedLagIdentityTransition::RETIRE_TARGET;
	}
	if (!targetExistedBefore && !targetExistsNow)
	{
		return ZhangFixedLagIdentityTransition::RETIRE_TARGET;
	}
	if (phaseIdentityChanged || physicalArcVersionChanged)
	{
		return ZhangFixedLagIdentityTransition::RESET_PHYSICAL_IDENTITY;
	}
	if (!coordinateChanged)
	{
		return ZhangFixedLagIdentityTransition::CONTINUE;
	}
	return exactTargetTransportAvailable
		? ZhangFixedLagIdentityTransition::CONTINUE_EXACT_TRANSFORM
		: ZhangFixedLagIdentityTransition::RESET_EXACT_TRANSFORM_UNAVAILABLE;
}
