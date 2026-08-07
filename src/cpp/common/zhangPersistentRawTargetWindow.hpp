#pragma once

#include <algorithm>
#include <limits>
#include <map>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"
#include "common/zhangIncrementalRawSquareRoot.hpp"

struct ZhangPersistentRawTargetMarginal
{
	bool valid = false;
	int stateDimension = 0;
	int targetCount = 0;
	int exactConstraintsApplied = 0;
	VectorXd mean;
	MatrixXd covariance;
	std::vector<std::string> identities;
	std::vector<std::string> physicalVersions;
	std::string failureReason;
};

/** Raw-factor square-root boundary augmented with persistent physical target
 * variables.  The augmented order is [current network state, persistent
 * targets].  Targets are introduced by an exact affine transform, carried
 * with identity transition and zero process noise, and reconnected to each
 * epoch's current state coordinates by zero-variance exact constraints. */
class ZhangPersistentRawTargetWindow
{
public:
	void clear()
	{
		initialised = false;
		stateDimension = 0;
		targets.clear();
		targetIndex.clear();
		window.clear();
		failureReason.clear();
	}

	bool initialise(const VectorXd& mean, const MatrixXd& covariance)
	{
		clear();
		if (!window.initialise(mean, covariance))
		{
			return fail(window.summary().failureReason);
		}
		stateDimension = mean.size();
		initialised = true;
		return true;
	}

	bool addAcceptedMeasurement(
		const MatrixXd& design,
		const MatrixXd& covariance,
		const VectorXd& observation)
	{
		if (!initialised || design.cols() != stateDimension)
		{
			return fail("INVALID_PERSISTENT_TARGET_MEASUREMENT");
		}
		MatrixXd augmented = MatrixXd::Zero(
			design.rows(), stateDimension + targets.size());
		augmented.leftCols(stateDimension) = design;
		if (!window.addAcceptedMeasurement(
				augmented, covariance, observation))
		{
			return fail(window.summary().failureReason);
		}
		return true;
	}

	bool advance(
		const MatrixXd& transition,
		const MatrixXd& processCovariance)
	{
		if (!initialised || transition.cols() != stateDimension
		 || processCovariance.rows() != transition.rows()
		 || processCovariance.cols() != transition.rows())
		{
			return fail("INVALID_PERSISTENT_TARGET_TRANSITION");
		}
		const int oldStateDimension = stateDimension;
		const int newStateDimension = transition.rows();
		const int targetCount = targets.size();
		MatrixXd augmentedTransition = MatrixXd::Zero(
			newStateDimension + targetCount,
			oldStateDimension + targetCount);
		augmentedTransition.topLeftCorner(
			newStateDimension, oldStateDimension) = transition;
		augmentedTransition.bottomRightCorner(targetCount, targetCount)
			= MatrixXd::Identity(targetCount, targetCount);
		MatrixXd augmentedProcess = MatrixXd::Zero(
			newStateDimension + targetCount,
			newStateDimension + targetCount);
		augmentedProcess.topLeftCorner(
			newStateDimension, newStateDimension) = processCovariance;
		if (!window.advance(augmentedTransition, augmentedProcess))
		{
			return fail(window.summary().failureReason);
		}
		stateDimension = newStateDimension;
		return true;
	}

	bool applyExactCoordinateTransform(
		const MatrixXd& transform,
		const VectorXd& translation = {})
	{
		if (!initialised || transform.cols() != stateDimension)
		{
			return fail("INVALID_PERSISTENT_TARGET_COORDINATE_TRANSFORM");
		}
		const int oldStateDimension = stateDimension;
		const int newStateDimension = transform.rows();
		const int targetCount = targets.size();
		MatrixXd augmentedTransform = MatrixXd::Zero(
			newStateDimension + targetCount,
			oldStateDimension + targetCount);
		augmentedTransform.topLeftCorner(
			newStateDimension, oldStateDimension) = transform;
		augmentedTransform.bottomRightCorner(targetCount, targetCount)
			= MatrixXd::Identity(targetCount, targetCount);
		VectorXd augmentedTranslation = VectorXd::Zero(
			newStateDimension + targetCount);
		if (translation.size() != 0)
		{
			if (translation.size() != newStateDimension)
			{
				return fail("INVALID_PERSISTENT_TARGET_COORDINATE_TRANSLATION");
			}
			augmentedTranslation.head(newStateDimension) = translation;
		}
		if (!window.applyExactCoordinateTransform(
				augmentedTransform, augmentedTranslation))
		{
			return fail(window.summary().failureReason);
		}
		stateDimension = newStateDimension;
		return true;
	}

	/** Bind or rebind one canonical target at an event sequence.  The
	 * physicalVersion must remain unchanged; a changed version is a hard reset
	 * boundary and is never mixed into the current window. */
	bool bindTarget(
		const std::string& identity,
		const std::string& physicalVersion,
		const VectorXd& row,
		double offset,
		std::size_t eventSequence)
	{
		if (!initialised || identity.empty() || physicalVersion.empty()
		 || row.size() != stateDimension || !row.allFinite()
		 || !std::isfinite(offset))
		{
			return fail("INVALID_PERSISTENT_RAW_TARGET");
		}
		auto found = targetIndex.find(identity);
		if (found == targetIndex.end())
		{
			const int oldDimension = stateDimension + targets.size();
			MatrixXd augment = MatrixXd::Zero(oldDimension + 1, oldDimension);
			augment.topRows(oldDimension) =
				MatrixXd::Identity(oldDimension, oldDimension);
			augment.bottomLeftCorner(1, stateDimension) = row.transpose();
			VectorXd translation = VectorXd::Zero(oldDimension + 1);
			translation(oldDimension) = offset;
			if (!window.applyExactCoordinateTransform(augment, translation))
			{
				return fail(window.summary().failureReason);
			}
			targetIndex[identity] = targets.size();
			targets.push_back({identity, physicalVersion, eventSequence});
			return true;
		}
		auto& target = targets[found->second];
		if (target.physicalVersion != physicalVersion)
		{
			return fail("PERSISTENT_RAW_TARGET_PHYSICAL_VERSION_CHANGED");
		}
		if (target.lastBoundSequence == eventSequence)
		{
			return true;
		}
		MatrixXd constraint = MatrixXd::Zero(
			1, stateDimension + targets.size());
		constraint.leftCols(stateDimension) = -row.transpose();
		constraint(0, stateDimension + found->second) = 1;
		if (!window.applyExactConstraint(
				constraint, VectorXd::Constant(1, offset)))
		{
			return fail(window.summary().failureReason);
		}
		target.lastBoundSequence = eventSequence;
		return true;
	}

	ZhangPersistentRawTargetMarginal targetMarginal() const
	{
		ZhangPersistentRawTargetMarginal result;
		result.stateDimension = stateDimension;
		result.targetCount = targets.size();
		result.exactConstraintsApplied =
			window.summary().exactConstraintsApplied;
		for (const auto& target : targets)
		{
			result.identities.push_back(target.identity);
			result.physicalVersions.push_back(target.physicalVersion);
		}
		if (!initialised || targets.empty())
		{
			result.failureReason = "NO_PERSISTENT_RAW_TARGETS";
			return result;
		}
		VectorXd fullMean;
		MatrixXd fullCovariance;
		if (!window.currentMarginal(fullMean, fullCovariance))
		{
			result.failureReason = window.summary().failureReason.empty()
				? "PERSISTENT_RAW_TARGET_MARGINAL_FAILED"
				: window.summary().failureReason;
			return result;
		}
		result.mean = fullMean.tail(targets.size());
		result.covariance = fullCovariance.bottomRightCorner(
			targets.size(), targets.size());
		result.covariance = 0.5
			* (result.covariance + result.covariance.transpose());
		result.valid = result.mean.allFinite() && result.covariance.allFinite();
		if (!result.valid)
		{
			result.failureReason = "NONFINITE_PERSISTENT_RAW_TARGET_MARGINAL";
		}
		return result;
	}

	int currentStateDimension() const { return stateDimension; }
	int targetCount() const { return targets.size(); }
	const std::string& lastFailureReason() const { return failureReason; }
	const ZhangIncrementalRawSquareRootSummary summary() const
	{
		return window.summary();
	}

private:
	struct Target
	{
		std::string identity;
		std::string physicalVersion;
		std::size_t lastBoundSequence = 0;
	};

	bool fail(const std::string& reason)
	{
		failureReason = reason.empty()
			? "UNKNOWN_PERSISTENT_RAW_TARGET_FAILURE" : reason;
		return false;
	}

	bool initialised = false;
	int stateDimension = 0;
	std::vector<Target> targets;
	std::map<std::string, int> targetIndex;
	ZhangIncrementalRawSquareRoot window;
	std::string failureReason;
};
