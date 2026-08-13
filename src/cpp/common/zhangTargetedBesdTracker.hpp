#pragma once

#include <cmath>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"

/**
 * Lightweight fixed-lag shadow for a small set of immutable BESD targets.
 *
 * The authoritative filter remains untouched.  Instead of retaining the full
 * network factor history, this class carries only
 *
 *     E[f], Cov(f), Cov(f, x)
 *
 * for the selected old/new product functionals f and the current network
 * state x.  This is the exact Gaussian Schur boundary needed to update the
 * targets with later accepted measurements.  Its storage is O(m*n + m^2),
 * where m is the number of selected target functionals, rather than a second
 * O(n^2) network covariance plus every accepted factor.
 */
class ZhangTargetedBesdTracker
{
public:
	struct Marginal
	{
		bool valid = false;
		VectorXd mean;
		MatrixXd covariance;
		std::vector<std::string> identities;
		std::string failureReason;
	};

	void clear()
	{
		active = false;
		stateDimension = 0;
		targetIdentities.clear();
		targetMean.resize(0);
		targetCovariance.resize(0, 0);
		targetStateCrossCovariance.resize(0, 0);
		lastFailure.clear();
	}

	bool initialise(
		const std::vector<std::string>& identities,
		const MatrixXd& targetRows,
		const VectorXd& targetOffsets,
		const VectorXd& stateMean,
		const MatrixXd& stateCovariance)
	{
		clear();
		const int targets = identities.size();
		const int dimension = stateMean.size();
		if (targets == 0
		 || targetRows.rows() != targets
		 || targetRows.cols() != dimension
		 || targetOffsets.size() != targets
		 || stateCovariance.rows() != dimension
		 || stateCovariance.cols() != dimension
		 || !targetRows.allFinite()
		 || !targetOffsets.allFinite()
		 || !stateMean.allFinite()
		 || !stateCovariance.allFinite())
		{
			return fail("INVALID_TARGETED_BESD_INITIALISATION");
		}
		for (const auto& identity : identities)
		{
			if (identity.empty())
			{
				return fail("EMPTY_TARGETED_BESD_IDENTITY");
			}
		}
		for (int first = 0; first < targets; first++)
		for (int second = first + 1; second < targets; second++)
		{
			if (identities[first] == identities[second])
			{
				return fail("DUPLICATE_TARGETED_BESD_IDENTITY");
			}
		}

		stateDimension = dimension;
		targetIdentities = identities;
		targetMean = targetRows * stateMean + targetOffsets;
		targetStateCrossCovariance = targetRows * stateCovariance;
		targetCovariance = targetStateCrossCovariance * targetRows.transpose();
		symmetrise(targetCovariance);
		if (!allFinite())
		{
			return fail("NONFINITE_TARGETED_BESD_INITIALISATION");
		}
		active = true;
		return true;
	}

	/** Apply one finally accepted measurement using its prefit residual.
	 *
	 * The caller supplies the authoritative pre-update P, design H, measurement
	 * covariance R and innovation v=z-H*x.  No state or filter covariance is
	 * modified here.
	 */
	bool updateAcceptedMeasurement(
		const MatrixXd& stateCovariance,
		const MatrixXd& design,
		const MatrixXd& measurementCovariance,
		const VectorXd& prefitResidual)
	{
		if (!active
		 || stateCovariance.rows() != stateDimension
		 || stateCovariance.cols() != stateDimension
		 || design.cols() != stateDimension
		 || design.rows() != prefitResidual.size()
		 || measurementCovariance.rows() != design.rows()
		 || measurementCovariance.cols() != design.rows()
		 || !stateCovariance.allFinite()
		 || !design.allFinite()
		 || !measurementCovariance.allFinite()
		 || !prefitResidual.allFinite())
		{
			return fail("INVALID_TARGETED_BESD_MEASUREMENT");
		}
		MatrixXd innovation = design * stateCovariance * design.transpose()
			+ measurementCovariance;
		symmetrise(innovation);
		LDLT<MatrixXd> decomposition(innovation);
		if (decomposition.info() != Eigen::Success
		 || (decomposition.vectorD().array() <= 0).any())
		{
			return fail("TARGETED_BESD_INNOVATION_NOT_POSITIVE_DEFINITE");
		}
		const MatrixXd targetInnovation =
			targetStateCrossCovariance * design.transpose();
		const MatrixXd targetGain = decomposition.solve(
			targetInnovation.transpose()).transpose();
		if (!targetGain.allFinite())
		{
			return fail("TARGETED_BESD_GAIN_SOLVE_FAILED");
		}
		targetMean += targetGain * prefitResidual;
		targetCovariance -= targetGain * innovation * targetGain.transpose();
		targetStateCrossCovariance -=
			targetGain * design * stateCovariance;
		symmetrise(targetCovariance);
		if (!allFinite())
		{
			return fail("NONFINITE_TARGETED_BESD_MEASUREMENT_UPDATE");
		}
		return true;
	}

	/** Carry immutable targets through x+ = F*x + w, Cov(f,w)=0. */
	bool advanceState(const MatrixXd& transition)
	{
		if (!active
		 || transition.cols() != stateDimension
		 || !transition.allFinite())
		{
			return fail("INVALID_TARGETED_BESD_STATE_TRANSITION");
		}
		targetStateCrossCovariance =
			targetStateCrossCovariance * transition.transpose();
		stateDimension = transition.rows();
		if (!allFinite())
		{
			return fail("NONFINITE_TARGETED_BESD_STATE_TRANSITION");
		}
		return true;
	}

	/** Carry immutable targets through an exact rectangular coordinate map. */
	bool applyExactStateTransform(const MatrixXd& transform)
	{
		return advanceState(transform);
	}

	Marginal marginal() const
	{
		Marginal result;
		result.identities = targetIdentities;
		if (!active)
		{
			result.failureReason = lastFailure.empty()
				? "TARGETED_BESD_TRACKER_INACTIVE" : lastFailure;
			return result;
		}
		result.mean = targetMean;
		result.covariance = targetCovariance;
		result.valid = result.mean.allFinite() && result.covariance.allFinite();
		if (!result.valid)
		{
			result.failureReason = "NONFINITE_TARGETED_BESD_MARGINAL";
		}
		return result;
	}

	bool isActive() const { return active; }
	int targetCount() const { return targetMean.size(); }
	int currentStateDimension() const { return stateDimension; }
	const MatrixXd& crossCovariance() const
	{
		return targetStateCrossCovariance;
	}
	const std::string& failureReason() const { return lastFailure; }

private:
	static void symmetrise(MatrixXd& matrix)
	{
		matrix = 0.5 * (matrix + matrix.transpose());
	}

	bool allFinite() const
	{
		return targetMean.allFinite()
			&& targetCovariance.allFinite()
			&& targetStateCrossCovariance.allFinite();
	}

	bool fail(const std::string& reason)
	{
		lastFailure = reason;
		active = false;
		return false;
	}

	bool active = false;
	int stateDimension = 0;
	std::vector<std::string> targetIdentities;
	VectorXd targetMean;
	MatrixXd targetCovariance;
	MatrixXd targetStateCrossCovariance;
	std::string lastFailure;
};
