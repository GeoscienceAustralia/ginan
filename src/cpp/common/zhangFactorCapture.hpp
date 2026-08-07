#pragma once

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <deque>
#include <limits>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include "common/algebra.hpp"
#include "common/zhangFixedLagSquareRoot.hpp"
#include "common/zhangRawFactorWindow.hpp"
#include "common/zhangResidualStatistics.hpp"
#include "common/zhangIncrementalTargetSeparator.hpp"
#include "common/zhangIncrementalRawSquareRoot.hpp"
#include "common/zhangPersistentRawTargetWindow.hpp"

enum class ZhangCapturedFactorKind
{
	MEASUREMENT,
	STATE_TRANSITION,
	EXACT_COORDINATE_TRANSFORM
};

struct ZhangCapturedStateKey
{
	int type = 0;
	SatSys satellite;
	std::string receiver;
	int number = 0;

	bool operator==(const ZhangCapturedStateKey& other) const
	{
		return type == other.type
			&& satellite == other.satellite
			&& receiver == other.receiver
			&& number == other.number;
	}
};

inline ZhangCapturedStateKey zhangCapturedStateKey(const KFKey& key)
{
	return {
		static_cast<int>(key.type), key.Sat, key.str, key.num
	};
}

struct ZhangCapturedFactorEvent
{
	ZhangCapturedFactorKind kind = ZhangCapturedFactorKind::MEASUREMENT;
	GTime time;
	std::size_t sequence = 0;
	std::string label;
	std::vector<ZhangCapturedStateKey> sourceKeys;
	std::vector<ZhangCapturedStateKey> destinationKeys;
	SparseMatrix<double> design;
	SparseMatrix<double> covariance;
	VectorXd rightHandSide;
	std::vector<ZhangCapturedStateKey> observationKeys;
	bool dimensionPreserving = false;
	bool nonsingularCoordinateTransform = false;
};

struct ZhangFactorCaptureSummary
{
	bool valid = false;
	std::size_t events = 0;
	std::size_t measurements = 0;
	std::size_t transitions = 0;
	std::size_t coordinateTransforms = 0;
	std::size_t physicalTargets = 0;
	std::size_t physicalTargetIdentityResets = 0;
	std::size_t physicalTargetCoordinateContinuations = 0;
	std::size_t retainedTargetBlocks = 0;
	std::size_t invalidRetainedTargetBlocks = 0;
	std::size_t retainedTargetInformationRank = 0;
	double retainedTargetWhitenedSquaredNorm = 0;
	std::size_t measurementRows = 0;
	std::size_t measurementNonZeros = 0;
	std::size_t covarianceNonZeros = 0;
	std::size_t transitionNonZeros = 0;
	std::size_t transformNonZeros = 0;
	double maximumReplayPriorMeanRelativeError = 0;
	double maximumReplayPriorCovarianceRelativeError = 0;
	double maximumTargetMeanRelativeError = 0;
	double maximumTargetVarianceRelativeError = 0;
	double maximumRawSquareRootMeanRelativeError = 0;
	double maximumRawSquareRootCovarianceRelativeError = 0;
	std::string failureReason;
};

/** Diagnostic-only marginal innovation scale for one accepted-measurement
 * family.  The prefit ratios are already normalised by diag(H P H' + R) in
 * the Kalman quality-control path.  Their squared sum is therefore useful for
 * locating signal-dependent scale errors, but correlated rows are deliberately
 * not advertised as independent chi-square degrees of freedom. */
struct ZhangInnovationScaleGroup
{
	std::string identity;
	std::size_t blocks = 0;
	std::size_t samples = 0;
	double marginalStandardisedSquaredSum = 0;
	double maximumAbsoluteRatio = 0;

	double predictiveCovarianceScaleMle() const
	{
		return samples > 0
			? marginalStandardisedSquaredSum / samples
			: std::numeric_limits<double>::quiet_NaN();
	}
};

struct ZhangCapturedPhysicalArcVersion
{
	std::string arc;
	int version = 0;

	bool operator==(const ZhangCapturedPhysicalArcVersion& other) const
	{
		return arc == other.arc && version == other.version;
	}
};

struct ZhangCapturedPhysicalTarget
{
	GTime time;
	std::size_t afterEventSequence = 0;
	std::string identity;
	std::string physicalArcSignature;
	std::string phaseSegmentIdentity;
	std::vector<ZhangCapturedPhysicalArcVersion> physicalArcVersions;
	bool resetPhysicalIdentity = false;
	bool continuedAcrossCoordinateChange = false;
	std::vector<ZhangCapturedStateKey> keys;
	SparseMatrix<double> row;
	double offset = 0;
	double mean = 0;
	double variance = 0;
	int unresolvedIntegerGaugeRank = 0;
	std::string integerGaugeIdentity;
	std::string separatorIdentity;
	std::string canonicalCoordinateIdentity;
	std::string productDatumIdentity;
	int productDatumVersion = 0;
};

struct ZhangCapturedRetainedTargetBlock
{
	GTime time;
	std::size_t afterEventSequence = 0;
	int targetCount = 0;
	int informationRank = 0;
	int residualDof = 0;
	int projectedGaugeRank = 0;
	bool likelihoodValid = false;
	bool valid = false;
	double whitenedSquaredNorm = std::numeric_limits<double>::quiet_NaN();
	VectorXd whitenedResidual;
	std::vector<std::string> separatorIdentities;
	std::vector<std::string> gaugeIdentities;
	std::vector<bool> absoluteValid;
	std::vector<double> coordinateOffsets;
	MatrixXd likelihoodDesign;
	VectorXd likelihoodObservation;
	MatrixXd likelihoodCovariance;
	std::string failureReason;
};

struct ZhangCapturedUnresolvedIntegerDatum
{
	GTime time;
	std::size_t afterEventSequence = 0;
	std::string identity;
	int missingGaugeRank = 1;
};

struct ZhangRawSquareRootTargetMarginal
{
	bool valid = false;
	int requestedTargetCount = 0;
	int informationRank = 0;
	int quotientValidRank = 0;
	int absoluteValidRank = 0;
	int unresolvedGaugeRank = 0;
	int batchOrthogonalDof = 0;
	double batchOrthogonalSquaredNorm = 0;
	int storedRows = 0;
	int storedColumns = 0;
	int maximumStoredRows = 0;
	int maximumStoredColumns = 0;
	int exactConstraintsApplied = 0;
	std::vector<std::string> identities;
	std::vector<std::string> gaugeIdentities;
	std::vector<bool> absoluteValidity;
	VectorXd mean;
	MatrixXd covariance;
	std::string failureReason;
};

inline std::vector<ZhangCapturedStateKey> zhangKeysByIndex(
	const std::map<KFKey, int>& indexMap)
{
	std::vector<ZhangCapturedStateKey> keys(indexMap.size());
	std::vector<bool> populated(indexMap.size(), false);
	for (const auto& [key, index] : indexMap)
	{
		if (index < 0 || index >= static_cast<int>(keys.size()) || populated[index])
		{
			return {};
		}
		keys[index] = zhangCapturedStateKey(key);
		populated[index] = true;
	}
	if (std::find(populated.begin(), populated.end(), false) != populated.end())
	{
		return {};
	}
	return keys;
}

/** Chronological, read-only shadow copy of the factors actually accepted by
 * the network Kalman filter.
 *
 * Measurement factors are stored in absolute linearised form
 * H*x = V + H*x_minus, so replay does not depend on retaining an implicit
 * linearisation origin.  The first accepted measurement supplies the sole
 * Gaussian anchor.  Subsequent events must join exactly by their ordered
 * KFKey maps.
 */
class ZhangFactorCaptureBuffer
{
public:
	void clear()
	{
		anchored = false;
		anchorKeys.clear();
		currentKeys.clear();
		physicalTargets.clear();
		unresolvedIntegerDatums.clear();
		lastTargets.clear();
		incrementalTargetSeparator.clear();
		incrementalRawSquareRoot.clear();
		persistentRawTargetWindow.clear();
		retainedTargetBlocks.clear();
		currentRetainedTargetBlock = {};
		lastMeasurementPriorMean.resize(0);
		lastMeasurementPriorCovariance.resize(0, 0);
		lastMeasurementTargetStart = 0;
		lastTargetDispositionReason.clear();
		anchorMean.resize(0);
		anchorCovariance.resize(0, 0);
		events.clear();
		nextSequence = 0;
		replayMean.resize(0);
		replayCovariance.resize(0, 0);
		lastFailure.clear();
		maximumReplayPriorMeanRelativeError = 0;
		maximumReplayPriorCovarianceRelativeError = 0;
		maximumTargetMeanRelativeError = 0;
		maximumTargetVarianceRelativeError = 0;
		maximumRawSquareRootMeanRelativeError = 0;
		maximumRawSquareRootCovarianceRelativeError = 0;
		innovationScaleGroups.clear();
	}

	/** Close the current factor chronology after a real physical-arc change.
	 *
	 * A non-invertible local phase-coordinate reinitialisation is not an
	 * S-basis exchange.  If it removes a state direction used by a persistent
	 * physical functional, the old and new raw-factor windows must not be
	 * joined.  The separately owned product-datum manager survives this reset;
	 * the next accepted measurement supplies a fresh Gaussian anchor and the
	 * next target registration supplies the new physical-arc version.
	 */
	void resetForPhysicalArcChange()
	{
		clear();
	}

	bool recordPhysicalTarget(
		const GTime& time,
		const std::string& identity,
		const std::string& physicalArcSignature,
		const std::string& phaseSegmentIdentity,
		const std::vector<ZhangCapturedPhysicalArcVersion>& physicalArcVersions,
		const std::vector<ZhangCapturedStateKey>& keys,
		const VectorXd& row,
		double offset,
		const VectorXd& stateMean,
		const MatrixXd& stateCovariance,
		int unresolvedIntegerGaugeRank = 0,
		const std::string& integerGaugeIdentity = "",
		const std::string& canonicalCoordinateIdentity = "",
		const std::string& productDatumIdentity = "",
		int productDatumVersion = 0)
	{
		lastTargetDispositionReason.clear();
		if (!anchored
		 || identity.empty()
		 || physicalArcSignature.empty()
		 || phaseSegmentIdentity.empty()
		 || physicalArcVersions.empty()
		 || keys != currentKeys
		 || row.size() != static_cast<int>(keys.size())
		 || stateMean.size() != row.size()
		 || stateCovariance.rows() != row.size()
		 || stateCovariance.cols() != row.size()
		 || !row.allFinite() || !std::isfinite(offset)
		 || !stateMean.allFinite() || !stateCovariance.allFinite()
		 || unresolvedIntegerGaugeRank < 0
		 || (unresolvedIntegerGaugeRank > 0 && integerGaugeIdentity.empty()))
		{
			lastFailure = "INVALID_PHYSICAL_TARGET_OR_KEY_CHAIN";
			return false;
		}
		auto previous = lastTargets.find(identity);
		bool resetPhysicalIdentity = false;
		if (previous != lastTargets.end())
		{
			resetPhysicalIdentity =
				previous->second.phaseSegmentIdentity != phaseSegmentIdentity
				|| previous->second.productDatumIdentity != productDatumIdentity
				|| previous->second.productDatumVersion != productDatumVersion;
			for (const auto& arc : physicalArcVersions)
			{
				auto historical = previous->second.arcVersions.find(arc.arc);
				if (historical != previous->second.arcVersions.end()
				 && historical->second != arc.version)
				{
					resetPhysicalIdentity = true;
				}
			}
		}
		VectorXd effectiveRow = row;
		double effectiveOffset = offset;
		if (previous != lastTargets.end() && !resetPhysicalIdentity)
		{
			if (!previous->second.functionalAvailable)
			{
				if (unresolvedIntegerGaugeRank > 0)
				{
					lastTargetDispositionReason =
						"PERSISTENT_QUOTIENT_FUNCTIONAL_NOT_TRANSPORTABLE";
					return false;
				}
				// A separately versioned absolute product datum gives an exact
				// affine definition and may rebind an unavailable state row.
			}
			else if (previous->second.keys != keys
			 || previous->second.row.rows() != 1
			 || previous->second.row.cols() != row.size())
			{
				lastFailure = "PERSISTENT_CANONICAL_FUNCTIONAL_UNAVAILABLE";
				return false;
			}
			else
			{
				effectiveRow = VectorXd::Zero(row.size());
				for (int outer = 0; outer < previous->second.row.outerSize(); outer++)
				for (SparseMatrix<double>::InnerIterator entry(
						previous->second.row, outer); entry; ++entry)
				{
					effectiveRow(entry.col()) = entry.value();
				}
				effectiveOffset = previous->second.offset;
			}
		}
		std::vector<std::pair<int, double>> nonZeros;
		for (int index = 0; index < effectiveRow.size(); index++)
		{
			if (effectiveRow(index) != 0)
			{
				nonZeros.push_back({index, effectiveRow(index)});
			}
		}
		if (nonZeros.empty())
		{
			lastFailure = "EMPTY_PHYSICAL_TARGET_ROW";
			return false;
		}
		auto targetMean = [&](const VectorXd& mean)
		{
			double value = effectiveOffset;
			for (const auto& [index, coefficient] : nonZeros)
			{
				value += coefficient * mean(index);
			}
			return value;
		};
		auto targetVariance = [&](const MatrixXd& covariance)
		{
			double value = 0.0;
			for (const auto& [left, leftCoefficient] : nonZeros)
			for (const auto& [right, rightCoefficient] : nonZeros)
			{
				value += leftCoefficient * rightCoefficient
					* covariance(left, right);
			}
			return value;
		};
		const double replayTargetMean = targetMean(replayMean);
		const double actualTargetMean = targetMean(stateMean);
		const double replayTargetVariance = targetVariance(replayCovariance);
		const double actualTargetVariance = targetVariance(stateCovariance);
		if (!std::isfinite(replayTargetMean)
		 || !std::isfinite(actualTargetMean)
		 || !std::isfinite(replayTargetVariance)
		 || !std::isfinite(actualTargetVariance)
		 || replayTargetVariance < -varianceTolerance
		 || actualTargetVariance < -varianceTolerance)
		{
			lastFailure = "NONFINITE_OR_NEGATIVE_PHYSICAL_TARGET_MARGINAL";
			return false;
		}
		const double meanError = std::abs(replayTargetMean - actualTargetMean)
			/ std::max({1.0, std::abs(replayTargetMean), std::abs(actualTargetMean)});
		const double varianceError = std::abs(
			replayTargetVariance - actualTargetVariance)
			/ std::max({varianceTolerance,
				std::abs(replayTargetVariance), std::abs(actualTargetVariance)});
		maximumTargetMeanRelativeError = std::max(
			maximumTargetMeanRelativeError, meanError
		);
		maximumTargetVarianceRelativeError = std::max(
			maximumTargetVarianceRelativeError, varianceError
		);
		if (meanError > replayTolerance || varianceError > replayTolerance)
		{
			lastFailure = "PHYSICAL_TARGET_REPLAY_MISMATCH";
			return false;
		}

		ZhangCapturedPhysicalTarget target;
		target.time = time;
		target.afterEventSequence = nextSequence;
		target.identity = identity;
		target.physicalArcSignature = physicalArcSignature;
		target.phaseSegmentIdentity = phaseSegmentIdentity;
		target.physicalArcVersions = physicalArcVersions;
		target.resetPhysicalIdentity = resetPhysicalIdentity;
		if (previous != lastTargets.end())
		{
			target.continuedAcrossCoordinateChange =
				!target.resetPhysicalIdentity
				&& previous->second.physicalArcSignature
					!= physicalArcSignature;
		}
		target.keys = keys;
		target.row = effectiveRow.transpose().sparseView(0, sparseTolerance);
		target.offset = effectiveOffset;
		target.mean = actualTargetMean;
		target.variance = std::max(0.0, actualTargetVariance);
		target.unresolvedIntegerGaugeRank = unresolvedIntegerGaugeRank;
		target.integerGaugeIdentity = integerGaugeIdentity;
		target.canonicalCoordinateIdentity = canonicalCoordinateIdentity.empty()
			? identity : canonicalCoordinateIdentity;
		target.productDatumIdentity = productDatumIdentity;
		target.productDatumVersion = productDatumVersion;
		if (previous != lastTargets.end() && !target.resetPhysicalIdentity)
		{
			// A different exact raw-arc representation is an S-coordinate
			// continuation.  Reuse the physical separator identity so that the
			// accumulated likelihood is not reset by a pure basis/path change.
			target.separatorIdentity = previous->second.separatorIdentity;
		}
		else
		{
			std::ostringstream separatorIdentity;
			separatorIdentity << target.canonicalCoordinateIdentity;
			if (!productDatumIdentity.empty())
			{
				separatorIdentity << "|datum=" << productDatumIdentity;
			}
			separatorIdentity << "|phase=" << phaseSegmentIdentity;
			for (const auto& arc : physicalArcVersions)
			{
				separatorIdentity << "|" << arc.arc << "@" << arc.version;
			}
			target.separatorIdentity = separatorIdentity.str();
		}
		if (!persistentRawTargetWindow.bindTarget(
				target.canonicalCoordinateIdentity,
				target.separatorIdentity,
				effectiveRow,
				effectiveOffset,
				nextSequence))
		{
			lastTargetDispositionReason =
				persistentRawTargetWindow.lastFailureReason();
			return false;
		}
		physicalTargets.push_back(std::move(target));
		auto& history = lastTargets[identity];
		if (resetPhysicalIdentity
		 || history.phaseSegmentIdentity != phaseSegmentIdentity)
		{
			history.arcVersions.clear();
		}
		history.phaseSegmentIdentity = phaseSegmentIdentity;
		history.physicalArcSignature = physicalArcSignature;
		history.separatorIdentity = physicalTargets.back().separatorIdentity;
		history.productDatumIdentity = productDatumIdentity;
		history.productDatumVersion = productDatumVersion;
		history.keys = keys;
		history.row = physicalTargets.back().row;
		history.offset = physicalTargets.back().offset;
		history.unresolvedIntegerGaugeRank = unresolvedIntegerGaugeRank;
		history.integerGaugeIdentity = integerGaugeIdentity;
		history.canonicalCoordinateIdentity =
			physicalTargets.back().canonicalCoordinateIdentity;
		history.functionalAvailable = true;
		for (const auto& arc : physicalArcVersions)
		{
			history.arcVersions[arc.arc] = arc.version;
		}
		recomputeRetainedTargetBlock();
		return true;
	}

	void recordUnresolvedIntegerDatum(
		const GTime& time,
		const std::string& identity,
		int missingGaugeRank = 1)
	{
		if (!anchored || identity.empty() || missingGaugeRank <= 0)
		{
			lastFailure = "INVALID_UNRESOLVED_INTEGER_DATUM";
			return;
		}
		unresolvedIntegerDatums.push_back({
			time, nextSequence, identity, missingGaugeRank
		});
	}

	void setMaximumEvents(std::size_t maximum)
	{
		maximumEvents = maximum;
	}

	bool recordMeasurement(
		const GTime& time,
		const std::vector<ZhangCapturedStateKey>& keys,
		const VectorXd& priorMean,
		const MatrixXd& priorCovariance,
		const KFMeas& measurement,
		const std::string& label,
		const VectorXd& posteriorMean,
		const MatrixXd& posteriorCovariance)
	{
		finalizeRetainedTargetBlock();
		if (keys.empty()
		 || priorMean.size() != static_cast<int>(keys.size())
		 || priorCovariance.rows() != static_cast<int>(keys.size())
		 || priorCovariance.cols() != static_cast<int>(keys.size())
		 || measurement.H.cols() != static_cast<int>(keys.size())
		 || measurement.H.rows() != measurement.V.size()
		 || measurement.R.rows() != measurement.H.rows()
		 || measurement.R.cols() != measurement.H.rows()
		 || posteriorMean.size() != priorMean.size()
		 || posteriorCovariance.rows() != priorCovariance.rows()
		 || posteriorCovariance.cols() != priorCovariance.cols())
		{
			lastFailure = "INVALID_MEASUREMENT_CAPTURE_DIMENSIONS";
			return false;
		}
		if (!priorMean.allFinite() || !priorCovariance.allFinite()
		 || !measurement.V.allFinite() || !measurement.H.allFinite()
		 || !measurement.R.allFinite()
		 || !posteriorMean.allFinite() || !posteriorCovariance.allFinite())
		{
			lastFailure = "NONFINITE_MEASUREMENT_CAPTURE";
			return false;
		}
		accumulateInnovationScaleDiagnostics(measurement);
		if (!anchored
		 && !incrementalRawSquareRoot.initialise(priorMean, priorCovariance))
		{
			lastFailure = incrementalRawSquareRoot.summary().failureReason;
			return false;
		}
		if (!anchored
		 && !persistentRawTargetWindow.initialise(priorMean, priorCovariance))
		{
			lastFailure = persistentRawTargetWindow.lastFailureReason();
			return false;
		}
		if (!auditRawSquareRootBoundary(
				priorMean, priorCovariance, "RAW_SQUARE_ROOT_PRIOR_MISMATCH"))
		{
			return false;
		}
		const VectorXd absoluteObservation =
			measurement.V + measurement.H * priorMean;
		if (!incrementalRawSquareRoot.addAcceptedMeasurement(
				measurement.H, measurement.R, absoluteObservation))
		{
			lastFailure = incrementalRawSquareRoot.summary().failureReason;
			return false;
		}
		if (!persistentRawTargetWindow.addAcceptedMeasurement(
				measurement.H, measurement.R, absoluteObservation))
		{
			lastFailure = persistentRawTargetWindow.lastFailureReason();
			return false;
		}
		if (!auditRawSquareRootBoundary(
				posteriorMean, posteriorCovariance,
				"RAW_SQUARE_ROOT_POSTERIOR_MISMATCH"))
		{
			return false;
		}

		if (!anchored)
		{
			anchored = true;
			anchorKeys = keys;
			anchorMean = priorMean;
			anchorCovariance = priorCovariance;
			replayMean = priorMean;
			replayCovariance = priorCovariance;
			events.clear();
			nextSequence = 0;
		}
		else if (!currentKeys.empty() && currentKeys != keys)
		{
			lastFailure = "MEASUREMENT_KEY_CHAIN_MISMATCH";
			return false;
		}

		double meanScale = std::max(1.0, priorMean.norm());
		double covarianceScale = std::max(1.0, priorCovariance.norm());
		double meanError = (replayMean - priorMean).norm() / meanScale;
		double covarianceError =
			(replayCovariance - priorCovariance).norm() / covarianceScale;
		maximumReplayPriorMeanRelativeError = std::max(
			maximumReplayPriorMeanRelativeError, meanError
		);
		maximumReplayPriorCovarianceRelativeError = std::max(
			maximumReplayPriorCovarianceRelativeError, covarianceError
		);
		if (meanError > replayTolerance || covarianceError > replayTolerance)
		{
			lastFailure = "REPLAY_PRIOR_MISMATCH";
			return false;
		}

		ZhangCapturedFactorEvent event;
		event.kind = ZhangCapturedFactorKind::MEASUREMENT;
		event.time = time;
		event.sequence = nextSequence++;
		event.label = label;
		event.sourceKeys = keys;
		event.destinationKeys = keys;
		event.design = measurement.H.sparseView(0, sparseTolerance);
		event.covariance = measurement.R.sparseView(0, sparseTolerance);
		event.rightHandSide = absoluteObservation;
		for (const auto& key : measurement.obsKeys)
		{
			event.observationKeys.push_back(zhangCapturedStateKey(key));
		}
		event.dimensionPreserving = true;
		event.nonsingularCoordinateTransform = true;
		events.push_back(std::move(event));
		currentKeys = keys;
		lastMeasurementPriorMean = priorMean;
		lastMeasurementPriorCovariance = priorCovariance;
		lastMeasurementTime = time;
		lastMeasurementTargetStart = physicalTargets.size();
		replayMean = posteriorMean;
		replayCovariance = posteriorCovariance;
		trimFailClosed();
		return true;
	}

	bool recordTransition(
		const GTime& time,
		const std::vector<ZhangCapturedStateKey>& source,
		const std::vector<ZhangCapturedStateKey>& destination,
		const SparseMatrix<double>& transition,
		const MatrixXd& processCovariance,
		const std::string& label)
	{
		if (!anchored)
		{
			return true;
		}
		if (source.empty() || destination.empty()
		 || transition.rows() != static_cast<int>(destination.size())
		 || transition.cols() != static_cast<int>(source.size())
		 || processCovariance.rows() != transition.rows()
		 || processCovariance.cols() != transition.rows()
		 || (!currentKeys.empty() && currentKeys != source)
		 || !processCovariance.allFinite())
		{
			lastFailure = "INVALID_TRANSITION_CAPTURE_OR_KEY_CHAIN";
			return false;
		}
		auto transportedTargets = lastTargets;
		if (!transportPersistentFunctionals(
				transportedTargets, source, destination, transition,
				&processCovariance, "STATE_TRANSITION", false))
		{
			return false;
		}
		const VectorXd predictedMean = transition * replayMean;
		MatrixXd predictedCovariance =
			transition * replayCovariance * transition.transpose()
			+ processCovariance;
		predictedCovariance = 0.5
			* (predictedCovariance + predictedCovariance.transpose());
		if (!incrementalRawSquareRoot.advance(
				MatrixXd(transition), processCovariance))
		{
			lastFailure = incrementalRawSquareRoot.summary().failureReason;
			return false;
		}
		if (!persistentRawTargetWindow.advance(
				MatrixXd(transition), processCovariance))
		{
			lastFailure = persistentRawTargetWindow.lastFailureReason();
			return false;
		}
		if (!auditRawSquareRootBoundary(
				predictedMean, predictedCovariance,
				"RAW_SQUARE_ROOT_TRANSITION_MISMATCH"))
		{
			return false;
		}

		ZhangCapturedFactorEvent event;
		event.kind = ZhangCapturedFactorKind::STATE_TRANSITION;
		event.time = time;
		event.sequence = nextSequence++;
		event.label = label;
		event.sourceKeys = source;
		event.destinationKeys = destination;
		event.design = transition;
		event.covariance = processCovariance.sparseView(0, sparseTolerance);
		event.dimensionPreserving =
			transition.rows() == transition.cols();
		events.push_back(std::move(event));
		currentKeys = destination;
		replayMean = predictedMean;
		replayCovariance = predictedCovariance;
		lastTargets = std::move(transportedTargets);
		trimFailClosed();
		return true;
	}

	bool recordCoordinateTransform(
		const GTime& time,
		const std::vector<ZhangCapturedStateKey>& source,
		const std::vector<ZhangCapturedStateKey>& destination,
		const SparseMatrix<double>& transform,
		const std::string& label)
	{
		if (!anchored)
		{
			return true;
		}
		if (source.empty() || destination.empty()
		 || transform.rows() != static_cast<int>(destination.size())
		 || transform.cols() != static_cast<int>(source.size())
		 || (!currentKeys.empty() && currentKeys != source))
		{
			lastFailure = "INVALID_COORDINATE_TRANSFORM_OR_KEY_CHAIN";
			return false;
		}
		auto transportedTargets = lastTargets;
		if (!transportPersistentFunctionals(
				transportedTargets, source, destination, transform,
				nullptr, "EXACT_COORDINATE_TRANSFORM", true))
		{
			return false;
		}
		const VectorXd transformedMean = transform * replayMean;
		MatrixXd transformedCovariance =
			transform * replayCovariance * transform.transpose();
		transformedCovariance = 0.5
			* (transformedCovariance + transformedCovariance.transpose());
		if (!incrementalRawSquareRoot.applyExactCoordinateTransform(
				MatrixXd(transform)))
		{
			lastFailure = incrementalRawSquareRoot.summary().failureReason;
			return false;
		}
		if (!persistentRawTargetWindow.applyExactCoordinateTransform(
				MatrixXd(transform)))
		{
			lastFailure = persistentRawTargetWindow.lastFailureReason();
			return false;
		}
		if (!auditRawSquareRootBoundary(
				transformedMean, transformedCovariance,
				"RAW_SQUARE_ROOT_EXACT_TRANSFORM_MISMATCH"))
		{
			return false;
		}

		ZhangCapturedFactorEvent event;
		event.kind = ZhangCapturedFactorKind::EXACT_COORDINATE_TRANSFORM;
		event.time = time;
		event.sequence = nextSequence++;
		event.label = label;
		event.sourceKeys = source;
		event.destinationKeys = destination;
		event.design = transform;
		event.dimensionPreserving = transform.rows() == transform.cols();
		if (event.dimensionPreserving)
		{
			SparseMatrix<double> compressedTransform = transform;
			compressedTransform.makeCompressed();
			SparseQR<SparseMatrix<double>, COLAMDOrdering<int>> qr;
			qr.compute(compressedTransform);
			event.nonsingularCoordinateTransform =
				qr.info() == Eigen::Success
				&& qr.rank() == compressedTransform.cols();
		}
		events.push_back(std::move(event));
		currentKeys = destination;
		replayMean = transformedMean;
		replayCovariance = transformedCovariance;
		lastTargets = std::move(transportedTargets);
		trimFailClosed();
		return true;
	}

	ZhangFactorCaptureSummary summary() const
	{
		ZhangFactorCaptureSummary result;
		result.events = events.size();
		result.physicalTargets = physicalTargets.size();
		for (const auto& target : physicalTargets)
		{
			result.physicalTargetIdentityResets += target.resetPhysicalIdentity;
			result.physicalTargetCoordinateContinuations +=
				target.continuedAcrossCoordinateChange;
		}
		auto accumulateBlock = [&](const ZhangCapturedRetainedTargetBlock& block)
		{
			result.retainedTargetBlocks++;
			if (!block.valid)
			{
				result.invalidRetainedTargetBlocks++;
				return;
			}
			result.retainedTargetInformationRank += block.informationRank;
			result.retainedTargetWhitenedSquaredNorm += block.whitenedSquaredNorm;
		};
		for (const auto& block : retainedTargetBlocks)
		{
			accumulateBlock(block);
		}
		if (currentRetainedTargetBlock.targetCount > 0)
		{
			accumulateBlock(currentRetainedTargetBlock);
		}
		for (const auto& event : events)
		{
			switch (event.kind)
			{
				case ZhangCapturedFactorKind::MEASUREMENT:
					result.measurements++;
					result.measurementRows += event.design.rows();
					result.measurementNonZeros += event.design.nonZeros();
					result.covarianceNonZeros += event.covariance.nonZeros();
					break;
				case ZhangCapturedFactorKind::STATE_TRANSITION:
					result.transitions++;
					result.transitionNonZeros += event.design.nonZeros();
					result.covarianceNonZeros += event.covariance.nonZeros();
					break;
				case ZhangCapturedFactorKind::EXACT_COORDINATE_TRANSFORM:
					result.coordinateTransforms++;
					result.transformNonZeros += event.design.nonZeros();
					break;
			}
		}
		result.failureReason = lastFailure;
		result.maximumReplayPriorMeanRelativeError =
			maximumReplayPriorMeanRelativeError;
		result.maximumReplayPriorCovarianceRelativeError =
			maximumReplayPriorCovarianceRelativeError;
		result.maximumTargetMeanRelativeError = maximumTargetMeanRelativeError;
		result.maximumTargetVarianceRelativeError =
			maximumTargetVarianceRelativeError;
		result.maximumRawSquareRootMeanRelativeError =
			maximumRawSquareRootMeanRelativeError;
		result.maximumRawSquareRootCovarianceRelativeError =
			maximumRawSquareRootCovarianceRelativeError;
		result.valid = anchored && lastFailure.empty() && !events.empty();
		return result;
	}

	const std::vector<ZhangCapturedStateKey>& initialKeys() const { return anchorKeys; }
	std::vector<ZhangInnovationScaleGroup> innovationScaleDiagnostics() const
	{
		std::vector<ZhangInnovationScaleGroup> result;
		result.reserve(innovationScaleGroups.size());
		for (const auto& [identity, group] : innovationScaleGroups)
		{
			result.push_back(group);
		}
		return result;
	}
	const VectorXd& initialMean() const { return anchorMean; }
	const MatrixXd& initialCovariance() const { return anchorCovariance; }
	const std::deque<ZhangCapturedFactorEvent>& capturedEvents() const
	{
		return events;
	}
	const std::deque<ZhangCapturedPhysicalTarget>& capturedPhysicalTargets() const
	{
		return physicalTargets;
	}
	const std::deque<ZhangCapturedRetainedTargetBlock>&
	capturedRetainedTargetBlocks() const
	{
		return retainedTargetBlocks;
	}
	const ZhangCapturedRetainedTargetBlock& currentRetainedBlock() const
	{
		return currentRetainedTargetBlock;
	}
	const std::string& lastTargetReason() const
	{
		return lastTargetDispositionReason;
	}

	ZhangRawSquareRootTargetMarginal currentRawSquareRootTargetMarginal() const
	{
		ZhangRawSquareRootTargetMarginal result;
		const auto squareRootSummary = incrementalRawSquareRoot.summary();
		result.batchOrthogonalDof = squareRootSummary.batchOrthogonalDof;
		result.batchOrthogonalSquaredNorm =
			squareRootSummary.batchOrthogonalSquaredNorm;
		result.storedRows = squareRootSummary.storedRows;
		result.storedColumns = squareRootSummary.storedColumns;
		result.maximumStoredRows = squareRootSummary.maximumStoredRows;
		result.maximumStoredColumns = squareRootSummary.maximumStoredColumns;
		if (!squareRootSummary.valid)
		{
			result.failureReason = squareRootSummary.failureReason;
			return result;
		}
		const auto targets = currentPersistentTargets();
		if (targets.empty())
		{
			result.failureReason = "NO_CURRENT_RAW_SQUARE_ROOT_TARGETS";
			return result;
		}
		const int count = targets.size();
		MatrixXd rows = MatrixXd::Zero(count, currentKeys.size());
		VectorXd offsets(count);
		for (int index = 0; index < count; index++)
		{
			const auto& target = targets[index];
			if (target.keys != currentKeys
			 || target.row.rows() != 1
			 || target.row.cols() != static_cast<int>(currentKeys.size()))
			{
				result.failureReason = "RAW_SQUARE_ROOT_TARGET_KEY_MISMATCH";
				return result;
			}
			for (int outer = 0; outer < target.row.outerSize(); outer++)
			for (SparseMatrix<double>::InnerIterator entry(target.row, outer);
				 entry; ++entry)
			{
				rows(index, entry.col()) = entry.value();
			}
			offsets(index) = target.offset;
			result.identities.push_back(target.separatorIdentity);
		}
		const auto marginal =
			incrementalRawSquareRoot.marginaliseTargets(rows, offsets);
		if (!marginal.valid)
		{
			result.failureReason = marginal.failureReason;
			return result;
		}
		std::set<std::string> unresolved;
		for (int index = 0; index < count; index++)
		{
			const auto& target = targets[index];
			const bool absolute = target.unresolvedIntegerGaugeRank == 0;
			result.absoluteValidity.push_back(absolute);
			const std::string gauge = absolute
				? "" : target.integerGaugeIdentity;
			result.gaugeIdentities.push_back(gauge);
			if (!gauge.empty())
			{
				unresolved.insert(gauge);
			}
		}
		result.requestedTargetCount = count;
		result.informationRank = marginal.targetRank;
		result.unresolvedGaugeRank = unresolved.size();
		result.quotientValidRank = std::max(
			0, result.informationRank - result.unresolvedGaugeRank);
		result.absoluteValidRank = std::count(
			result.absoluteValidity.begin(),
			result.absoluteValidity.end(), true);
		result.mean = marginal.mean;
		result.covariance = marginal.covariance;
		result.valid = result.mean.allFinite() && result.covariance.allFinite();
		if (!result.valid)
		{
			result.failureReason = "NONFINITE_RAW_SQUARE_ROOT_TARGET_MARGINAL";
		}
		return result;
	}

	/** Marginal of explicit persistent physical target variables carried inside
	 * the raw multi-epoch square-root boundary. */
	ZhangRawSquareRootTargetMarginal
	currentPersistentRawTargetMarginal() const
	{
		ZhangRawSquareRootTargetMarginal result;
		const auto marginal = persistentRawTargetWindow.targetMarginal();
		const auto summary = persistentRawTargetWindow.summary();
		result.batchOrthogonalDof = summary.batchOrthogonalDof;
		result.batchOrthogonalSquaredNorm = summary.batchOrthogonalSquaredNorm;
		result.storedRows = summary.storedRows;
		result.storedColumns = summary.storedColumns;
		result.maximumStoredRows = summary.maximumStoredRows;
		result.maximumStoredColumns = summary.maximumStoredColumns;
		result.exactConstraintsApplied = marginal.exactConstraintsApplied;
		if (!marginal.valid)
		{
			result.failureReason = marginal.failureReason;
			return result;
		}
		struct Metadata
		{
			std::string separatorIdentity;
			std::string integerGaugeIdentity;
			int unresolvedIntegerGaugeRank = 0;
		};
		std::map<std::string, Metadata> metadata;
		for (const auto& [identity, history] : lastTargets)
		{
			if (!history.canonicalCoordinateIdentity.empty())
			{
				metadata[history.canonicalCoordinateIdentity] = {
					history.separatorIdentity,
					history.integerGaugeIdentity,
					history.unresolvedIntegerGaugeRank};
			}
		}
		std::set<std::string> unresolved;
		for (int index = 0;
			 index < static_cast<int>(marginal.identities.size()); index++)
		{
			auto found = metadata.find(marginal.identities[index]);
			if (found == metadata.end())
			{
				result.failureReason =
					"PERSISTENT_RAW_TARGET_METADATA_UNAVAILABLE";
				return result;
			}
			const auto& history = found->second;
			result.identities.push_back(history.separatorIdentity);
			const bool absolute = history.unresolvedIntegerGaugeRank == 0;
			result.absoluteValidity.push_back(absolute);
			result.gaugeIdentities.push_back(
				absolute ? "" : history.integerGaugeIdentity);
			if (!absolute)
			{
				unresolved.insert(history.integerGaugeIdentity);
			}
		}
		result.requestedTargetCount = marginal.targetCount;
		result.informationRank = Eigen::FullPivLU<MatrixXd>(
			marginal.covariance).rank();
		result.unresolvedGaugeRank = unresolved.size();
		result.quotientValidRank = std::max(
			0, result.informationRank - result.unresolvedGaugeRank);
		result.absoluteValidRank = std::count(
			result.absoluteValidity.begin(),
			result.absoluteValidity.end(), true);
		result.mean = marginal.mean;
		result.covariance = marginal.covariance;
		result.valid = result.informationRank > 0
			&& result.mean.allFinite() && result.covariance.allFinite();
		if (!result.valid)
		{
			result.failureReason =
				"INVALID_PERSISTENT_RAW_TARGET_INFORMATION";
		}
		return result;
	}

	ZhangIncrementalTargetMarginal currentIncrementalTargetMarginal() const
	{
		ZhangIncrementalTargetSeparator replay = incrementalTargetSeparator;
		if (currentRetainedTargetBlock.likelihoodValid)
		{
			if (!replay.addLikelihood(
				currentRetainedTargetBlock.separatorIdentities,
				currentRetainedTargetBlock.likelihoodDesign,
				currentRetainedTargetBlock.likelihoodCovariance,
				currentRetainedTargetBlock.likelihoodObservation,
				currentRetainedTargetBlock.gaugeIdentities,
				currentRetainedTargetBlock.absoluteValid,
				currentRetainedTargetBlock.coordinateOffsets))
			{
				ZhangIncrementalTargetMarginal rejected;
				rejected.failureReason = replay.lastFailureReason();
				return rejected;
			}
			std::set<std::string> active;
			for (const auto& [identity, history] : lastTargets)
			{
				if (!history.separatorIdentity.empty())
				{
					active.insert(history.separatorIdentity);
				}
			}
			if (!replay.retainOnly(active))
			{
				ZhangIncrementalTargetMarginal rejected;
				rejected.failureReason = replay.lastFailureReason();
				return rejected;
			}
		}
		return replay.marginal();
	}

	/** Rebuild the captured chronology as one raw Gaussian factor window and
	 * eliminate every state direction except the targets registered after the
	 * last accepted measurement.  H/R and F/Q are consumed directly; exact
	 * coordinate transforms and zero-Q directions are substituted exactly. */
	ZhangRawFactorWindowMarginal currentRawIntegerDatumMarginal() const
	{
		ZhangRawFactorWindowMarginal rejected;
		const auto targets = currentPersistentTargets();
		if (!anchored || events.empty()
		 || targets.empty())
		{
			int unresolved = 0;
			int missingRank = 0;
			for (const auto& datum : unresolvedIntegerDatums)
			{
				if (datum.afterEventSequence == nextSequence)
				{
					unresolved++;
					missingRank += datum.missingGaugeRank;
				}
			}
			rejected.requestedTargetCount = unresolved;
			rejected.targetRank = 0;
			rejected.unresolvedGaugeRank = missingRank;
			rejected.failureReason = unresolved > 0
				? "UNCONSTRAINED_INTEGER_DATUM_GAUGE"
				: "NO_CURRENT_RAW_INTEGER_DATUM_BLOCK";
			return rejected;
		}
		ZhangRawFactorWindow window;
		if (!window.initialise(anchorMean, anchorCovariance))
		{
			rejected.failureReason = window.lastFailureReason();
			return rejected;
		}
		for (const auto& event : events)
		{
			const MatrixXd design(event.design);
			if (event.kind == ZhangCapturedFactorKind::MEASUREMENT)
			{
				if (!window.addAcceptedMeasurement(
						design, MatrixXd(event.covariance),
						event.rightHandSide))
				{
					rejected.failureReason = window.lastFailureReason();
					return rejected;
				}
			}
			else if (event.kind == ZhangCapturedFactorKind::STATE_TRANSITION)
			{
				if (!window.addStateTransition(
						design, MatrixXd(event.covariance)))
				{
					rejected.failureReason = window.lastFailureReason();
					return rejected;
				}
			}
			else if (!window.addExactCoordinateTransform(design))
			{
				rejected.failureReason = window.lastFailureReason();
				return rejected;
			}
		}

		const int targetCount = targets.size();
		MatrixXd targetRows = MatrixXd::Zero(
			targetCount, currentKeys.size());
		VectorXd targetOffsets(targetCount);
		// The requested block is explicit even if a later numerical rank gate
		// rejects it.
		for (int index = 0; index < targetCount; index++)
		{
			const auto& target = targets[index];
			if (target.keys != currentKeys
			 || target.row.rows() != 1
			 || target.row.cols() != static_cast<int>(currentKeys.size()))
			{
				rejected.failureReason =
					"RAW_INTEGER_DATUM_KEY_CHAIN_MISMATCH";
				return rejected;
			}
			targetRows.row(index) = MatrixXd(target.row);
			targetOffsets(index) = target.offset;
		}
		auto result = window.marginaliseToIntegerDatum(
			targetRows, targetOffsets);
		result.requestedTargetCount = targetCount;
		for (int index = 0; index < targetCount; index++)
		{
			result.unresolvedGaugeRank +=
				targets[index].unresolvedIntegerGaugeRank;
		}
		result.quotientValid = result.valid;
		result.absoluteDatumValid =
			result.valid && result.unresolvedGaugeRank == 0;
		return result;
	}

private:
	static std::string innovationScaleIdentity(const KFKey& key)
	{
		std::ostringstream identity;
		identity << static_cast<int>(key.type)
			<< ":" << key.Sat.sysChar() << ":";
		const auto observable = magic_enum::enum_name(
			static_cast<E_ObsCode>(key.num));
		if (!observable.empty())
		{
			identity << observable;
		}
		else
		{
			identity << key.num;
		}
		return identity.str();
	}

	void accumulateInnovationScaleDiagnostics(const KFMeas& measurement)
	{
		if (measurement.prefitRatios.size() != measurement.V.size()
		 || measurement.obsKeys.size()
			!= static_cast<std::size_t>(measurement.V.size())
		 || measurement.prefitRatios.size() == 0
		 || !measurement.prefitRatios.allFinite()
		 || measurement.prefitRatios.cwiseAbs().maxCoeff() == 0)
		{
			return;
		}
		std::set<std::string> groupsInBlock;
		for (int row = 0; row < measurement.prefitRatios.size(); row++)
		{
			const std::string identity =
				innovationScaleIdentity(measurement.obsKeys[row]);
			auto& group = innovationScaleGroups[identity];
			group.identity = identity;
			group.samples++;
			const double ratio = measurement.prefitRatios(row);
			group.marginalStandardisedSquaredSum += ratio * ratio;
			group.maximumAbsoluteRatio = std::max(
				group.maximumAbsoluteRatio, std::abs(ratio));
			groupsInBlock.insert(identity);
		}
		for (const auto& identity : groupsInBlock)
		{
			innovationScaleGroups[identity].blocks++;
		}
	}

	std::vector<ZhangCapturedPhysicalTarget> currentPersistentTargets() const
	{
		std::vector<ZhangCapturedPhysicalTarget> result;
		std::set<std::string> present;
		for (std::size_t index = lastMeasurementTargetStart;
			 index < physicalTargets.size(); index++)
		{
			result.push_back(physicalTargets[index]);
			present.insert(physicalTargets[index].identity);
		}
		for (const auto& [identity, history] : lastTargets)
		{
			if (present.find(identity) != present.end()
			 || !history.functionalAvailable
			 || history.keys != currentKeys
			 || history.row.rows() != 1
			 || history.row.cols() != static_cast<int>(currentKeys.size()))
			{
				continue;
			}
			ZhangCapturedPhysicalTarget target;
			target.time = lastMeasurementTime;
			target.afterEventSequence = nextSequence;
			target.identity = identity;
			target.physicalArcSignature = history.physicalArcSignature;
			target.phaseSegmentIdentity = history.phaseSegmentIdentity;
			for (const auto& [arc, version] : history.arcVersions)
			{
				target.physicalArcVersions.push_back({arc, version});
			}
			target.keys = history.keys;
			target.row = history.row;
			target.offset = history.offset;
			target.unresolvedIntegerGaugeRank =
				history.unresolvedIntegerGaugeRank;
			target.integerGaugeIdentity = history.integerGaugeIdentity;
			target.separatorIdentity = history.separatorIdentity;
			target.canonicalCoordinateIdentity =
				history.canonicalCoordinateIdentity;
			target.productDatumIdentity = history.productDatumIdentity;
			target.productDatumVersion = history.productDatumVersion;
			double mean = target.offset;
			double variance = 0;
			for (int outer = 0; outer < target.row.outerSize(); outer++)
			for (SparseMatrix<double>::InnerIterator left(target.row, outer);
				 left; ++left)
			{
				mean += left.value() * replayMean(left.col());
				for (int inner = 0; inner < target.row.outerSize(); inner++)
				for (SparseMatrix<double>::InnerIterator right(target.row, inner);
					 right; ++right)
				{
					variance += left.value() * right.value()
						* replayCovariance(left.col(), right.col());
				}
			}
			target.mean = mean;
			target.variance = std::max(0.0, variance);
			result.push_back(std::move(target));
		}
		std::sort(result.begin(), result.end(), [](const auto& left, const auto& right)
		{
			return left.canonicalCoordinateIdentity
				< right.canonicalCoordinateIdentity;
		});
		return result;
	}

	bool auditRawSquareRootBoundary(
		const VectorXd& expectedMean,
		const MatrixXd& expectedCovariance,
		const std::string& mismatchReason)
	{
		VectorXd actualMean;
		MatrixXd actualCovariance;
		if (!incrementalRawSquareRoot.currentMarginal(
				actualMean, actualCovariance))
		{
			lastFailure = incrementalRawSquareRoot.summary().failureReason.empty()
				? "RAW_SQUARE_ROOT_BOUNDARY_EXTRACTION_FAILED"
				: incrementalRawSquareRoot.summary().failureReason;
			return false;
		}
		if (actualMean.size() != expectedMean.size()
		 || actualCovariance.rows() != expectedCovariance.rows()
		 || actualCovariance.cols() != expectedCovariance.cols())
		{
			lastFailure = mismatchReason;
			return false;
		}
		const double meanError = (actualMean - expectedMean).norm()
			/ std::max(1.0, expectedMean.norm());
		const double covarianceError =
			(actualCovariance - expectedCovariance).norm()
			/ std::max(1.0, expectedCovariance.norm());
		maximumRawSquareRootMeanRelativeError = std::max(
			maximumRawSquareRootMeanRelativeError, meanError);
		maximumRawSquareRootCovarianceRelativeError = std::max(
			maximumRawSquareRootCovarianceRelativeError, covarianceError);
		// The production filter updates a heterogeneous, thousands-dimensional
		// covariance in standard/Joseph form, while this audit uses square-root
		// information QR.  Their roundoff is measurably larger than the small
		// deterministic-system invariant tolerance, but remains bounded here.
		if (meanError > rawSquareRootReplayTolerance
		 || covarianceError > rawSquareRootReplayTolerance)
		{
			lastFailure = mismatchReason;
			return false;
		}
		return true;
	}

	void finalizeRetainedTargetBlock()
	{
		if (currentRetainedTargetBlock.targetCount > 0)
		{
			if (currentRetainedTargetBlock.likelihoodValid)
			{
				if (!incrementalTargetSeparator.addLikelihood(
					currentRetainedTargetBlock.separatorIdentities,
					currentRetainedTargetBlock.likelihoodDesign,
					currentRetainedTargetBlock.likelihoodCovariance,
					currentRetainedTargetBlock.likelihoodObservation,
					currentRetainedTargetBlock.gaugeIdentities,
					currentRetainedTargetBlock.absoluteValid,
					currentRetainedTargetBlock.coordinateOffsets))
				{
					lastFailure = incrementalTargetSeparator.lastFailureReason();
				}
				std::set<std::string> active;
				for (const auto& [identity, history] : lastTargets)
				{
					if (!history.separatorIdentity.empty())
					{
						active.insert(history.separatorIdentity);
					}
				}
				if (lastFailure.empty()
				 && !incrementalTargetSeparator.retainOnly(active))
				{
					lastFailure = incrementalTargetSeparator.lastFailureReason();
				}
			}
			retainedTargetBlocks.push_back(currentRetainedTargetBlock);
		}
		currentRetainedTargetBlock = {};
	}

	void recomputeRetainedTargetBlock()
	{
		currentRetainedTargetBlock = {};
		const auto targets = currentPersistentTargets();
		if (lastMeasurementPriorMean.size() == 0
		 || lastMeasurementPriorCovariance.rows() == 0
		 || targets.empty())
		{
			return;
		}
		const int count = targets.size();
		currentRetainedTargetBlock.time = lastMeasurementTime;
		currentRetainedTargetBlock.afterEventSequence = nextSequence;
		currentRetainedTargetBlock.targetCount = count;
		std::vector<std::vector<std::pair<int, double>>> rows(count);
		VectorXd priorMean(count);
		VectorXd posteriorMean(count);
		for (int targetIndex = 0; targetIndex < count; targetIndex++)
		{
			const auto& target = targets[targetIndex];
			if (target.keys != currentKeys
			 || target.row.rows() != 1
			 || target.row.cols() != replayMean.size())
			{
				currentRetainedTargetBlock.failureReason =
					"RETAINED_TARGET_KEY_CHAIN_MISMATCH";
				return;
			}
			priorMean(targetIndex) = target.offset;
			posteriorMean(targetIndex) = target.offset;
			for (int outer = 0; outer < target.row.outerSize(); outer++)
			for (SparseMatrix<double>::InnerIterator entry(target.row, outer);
				 entry; ++entry)
			{
				rows[targetIndex].push_back({entry.col(), entry.value()});
				priorMean(targetIndex) +=
					entry.value() * lastMeasurementPriorMean(entry.col());
				posteriorMean(targetIndex) +=
					entry.value() * replayMean(entry.col());
			}
		}
		auto marginalCovariance = [&](const MatrixXd& covariance)
		{
			MatrixXd marginal = MatrixXd::Zero(count, count);
			for (int leftTarget = 0; leftTarget < count; leftTarget++)
			for (int rightTarget = 0; rightTarget <= leftTarget; rightTarget++)
			{
				double value = 0;
				for (const auto& [left, leftCoefficient] : rows[leftTarget])
				for (const auto& [right, rightCoefficient] : rows[rightTarget])
				{
					value += leftCoefficient * rightCoefficient
						* covariance(left, right);
				}
				marginal(leftTarget, rightTarget) = value;
				marginal(rightTarget, leftTarget) = value;
			}
			return marginal;
		};
		MatrixXd priorCovariance =
			marginalCovariance(lastMeasurementPriorCovariance);
		MatrixXd posteriorCovariance = marginalCovariance(replayCovariance);
		LDLT<MatrixXd> priorLdlt(priorCovariance);
		LDLT<MatrixXd> posteriorLdlt(posteriorCovariance);
		if (priorLdlt.info() != Eigen::Success
		 || posteriorLdlt.info() != Eigen::Success
		 || (priorLdlt.vectorD().array() <= varianceTolerance).any()
		 || (posteriorLdlt.vectorD().array() <= varianceTolerance).any())
		{
			currentRetainedTargetBlock.failureReason =
				"TARGET_MARGINAL_NOT_POSITIVE_DEFINITE";
			return;
		}
		const MatrixXd identity = MatrixXd::Identity(count, count);
		const MatrixXd priorInformation = priorLdlt.solve(identity);
		const MatrixXd posteriorInformation = posteriorLdlt.solve(identity);
		MatrixXd increment = posteriorInformation - priorInformation;
		increment = 0.5 * (increment + increment.transpose());
		const VectorXd natural = posteriorInformation * posteriorMean
			- priorInformation * priorMean;
		Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(increment);
		if (eigen.info() != Eigen::Success)
		{
			currentRetainedTargetBlock.failureReason =
				"TARGET_INFORMATION_EIGEN_FAILED";
			return;
		}
		const double scale = std::max(
			1.0, eigen.eigenvalues().cwiseAbs().maxCoeff()
		);
		const double threshold = informationRankTolerance * scale;
		if (eigen.eigenvalues().minCoeff() < -threshold)
		{
			currentRetainedTargetBlock.failureReason =
				"NEGATIVE_TARGET_INFORMATION_INCREMENT";
			return;
		}
		std::vector<int> retained;
		for (int index = 0; index < count; index++)
		{
			if (eigen.eigenvalues()(index) > threshold)
			{
				retained.push_back(index);
			}
		}
		if (retained.empty())
		{
			currentRetainedTargetBlock.failureReason =
				"ZERO_TARGET_INFORMATION_RANK";
			return;
		}
		MatrixXd directions(count, retained.size());
		VectorXd informationEigenvalues(retained.size());
		for (int column = 0; column < static_cast<int>(retained.size()); column++)
		{
			directions.col(column) =
				eigen.eigenvectors().col(retained[column]);
			informationEigenvalues(column) =
				eigen.eigenvalues()(retained[column]);
		}
		const VectorXd observation =
			informationEigenvalues.cwiseInverse().asDiagonal()
			* directions.transpose() * natural;
		const VectorXd predictionMean = directions.transpose() * priorMean;
		const MatrixXd observationCovariance =
			informationEigenvalues.cwiseInverse().asDiagonal();
		currentRetainedTargetBlock.likelihoodDesign = directions.transpose();
		currentRetainedTargetBlock.likelihoodObservation = observation;
		currentRetainedTargetBlock.likelihoodCovariance = observationCovariance;
		for (int targetIndex = 0; targetIndex < count; targetIndex++)
		{
			const auto& target = targets[targetIndex];
			currentRetainedTargetBlock.separatorIdentities.push_back(
				target.separatorIdentity);
			currentRetainedTargetBlock.gaugeIdentities.push_back(
				target.integerGaugeIdentity);
			currentRetainedTargetBlock.absoluteValid.push_back(
				target.unresolvedIntegerGaugeRank == 0);
			currentRetainedTargetBlock.coordinateOffsets.push_back(target.offset);
		}
		currentRetainedTargetBlock.likelihoodValid = true;
		const MatrixXd predictionCovariance = observationCovariance
			+ directions.transpose() * priorCovariance * directions;
		std::map<std::string, int> gaugeColumns;
		for (int targetIndex = 0; targetIndex < count; targetIndex++)
		{
			const auto& target = targets[targetIndex];
			if (target.unresolvedIntegerGaugeRank <= 0)
			{
				continue;
			}
			if (gaugeColumns.find(target.integerGaugeIdentity)
				== gaugeColumns.end())
			{
				gaugeColumns[target.integerGaugeIdentity] = gaugeColumns.size();
			}
		}
		MatrixXd targetGaugeDirections = MatrixXd::Zero(
			count, gaugeColumns.size());
		for (int targetIndex = 0; targetIndex < count; targetIndex++)
		{
			const auto& target = targets[targetIndex];
			if (target.unresolvedIntegerGaugeRank > 0)
			{
				targetGaugeDirections(
					targetIndex,
					gaugeColumns.at(target.integerGaugeIdentity)) = 1;
			}
		}
		const MatrixXd informationGaugeDirections =
			directions.transpose() * targetGaugeDirections;
		const ZhangResidualStatistic whitened =
			zhangProjectGaugeAndWhitenStatistic(
			ZhangResidualDomain::PREFIT_INNOVATION,
			observation - predictionMean,
			predictionCovariance,
			informationGaugeDirections,
			informationRankTolerance
		);
		if (!whitened.valid)
		{
			if (whitened.failureReason
				== "NO_QUOTIENT_INVARIANT_TARGET_DIRECTION")
			{
				// This is a valid zero-dof block: all newly available target
				// information lies in an unresolved integer gauge.  Do not add
				// it to the persistent separator and do not call it a numerical
				// failure.
				currentRetainedTargetBlock.likelihoodValid = false;
				currentRetainedTargetBlock.informationRank = retained.size();
				currentRetainedTargetBlock.residualDof = 0;
				currentRetainedTargetBlock.projectedGaugeRank =
					Eigen::FullPivLU<MatrixXd>(informationGaugeDirections).rank();
				currentRetainedTargetBlock.whitenedSquaredNorm = 0;
				currentRetainedTargetBlock.whitenedResidual.resize(0);
				currentRetainedTargetBlock.valid = true;
				currentRetainedTargetBlock.failureReason.clear();
				return;
			}
			currentRetainedTargetBlock.failureReason = whitened.failureReason;
			return;
		}
		currentRetainedTargetBlock.informationRank = retained.size();
		currentRetainedTargetBlock.residualDof = whitened.dof;
		currentRetainedTargetBlock.projectedGaugeRank =
			whitened.removedGaugeRank;
		currentRetainedTargetBlock.whitenedSquaredNorm = whitened.squaredNorm;
		currentRetainedTargetBlock.whitenedResidual = whitened.whitenedResidual;
		currentRetainedTargetBlock.valid = true;
	}

	void trimFailClosed()
	{
		if (maximumEvents > 0 && events.size() > maximumEvents)
		{
			// A scientifically valid sliding anchor requires marginalising the
			// retired events.  Until that is implemented, never silently drop
			// factors and pretend the remaining chain is complete.
			lastFailure = "CAPTURE_EVENT_LIMIT_REQUIRES_MARGINAL_ANCHOR";
		}
	}

	static constexpr double sparseTolerance = 0;
	static constexpr double replayTolerance = 1e-10;
	static constexpr double varianceTolerance = 1e-24;
	static constexpr double informationRankTolerance = 1e-10;
	static constexpr double functionalTransportTolerance = 1e-10;
	bool anchored = false;
	std::size_t maximumEvents = 0;
	std::size_t nextSequence = 0;
	std::vector<ZhangCapturedStateKey> anchorKeys;
	std::vector<ZhangCapturedStateKey> currentKeys;
	VectorXd anchorMean;
	MatrixXd anchorCovariance;
	VectorXd replayMean;
	MatrixXd replayCovariance;
	std::deque<ZhangCapturedFactorEvent> events;
	std::deque<ZhangCapturedPhysicalTarget> physicalTargets;
	std::deque<ZhangCapturedUnresolvedIntegerDatum> unresolvedIntegerDatums;
	std::deque<ZhangCapturedRetainedTargetBlock> retainedTargetBlocks;
	ZhangCapturedRetainedTargetBlock currentRetainedTargetBlock;
	ZhangIncrementalTargetSeparator incrementalTargetSeparator;
	ZhangIncrementalRawSquareRoot incrementalRawSquareRoot;
	ZhangPersistentRawTargetWindow persistentRawTargetWindow;
	std::map<std::string, ZhangInnovationScaleGroup> innovationScaleGroups;
	VectorXd lastMeasurementPriorMean;
	MatrixXd lastMeasurementPriorCovariance;
	GTime lastMeasurementTime;
	std::size_t lastMeasurementTargetStart = 0;
	struct PhysicalTargetHistory
	{
		std::string phaseSegmentIdentity;
		std::string physicalArcSignature;
		std::string separatorIdentity;
		std::string productDatumIdentity;
		int productDatumVersion = 0;
		std::map<std::string, int> arcVersions;
		std::vector<ZhangCapturedStateKey> keys;
		SparseMatrix<double> row;
		double offset = 0;
		int unresolvedIntegerGaugeRank = 0;
		std::string integerGaugeIdentity;
		std::string canonicalCoordinateIdentity;
		bool functionalAvailable = false;
	};

	bool transportPersistentFunctionals(
		std::map<std::string, PhysicalTargetHistory>& histories,
		const std::vector<ZhangCapturedStateKey>& source,
		const std::vector<ZhangCapturedStateKey>& destination,
		const SparseMatrix<double>& transform,
		const MatrixXd* processCovariance,
		const std::string& eventKind,
		bool failIfUnrepresentable)
	{
		for (auto& [identity, history] : histories)
		{
			if (!history.functionalAvailable)
			{
				continue;
			}
			if (history.keys != source
			 || history.row.rows() != 1
			 || history.row.cols() != static_cast<int>(source.size()))
			{
				lastFailure = "PERSISTENT_FUNCTIONAL_KEY_CHAIN_MISMATCH_"
					+ eventKind;
				return false;
			}

			VectorXd sourceRow = VectorXd::Zero(source.size());
			for (int outer = 0; outer < history.row.outerSize(); outer++)
			for (SparseMatrix<double>::InnerIterator entry(history.row, outer);
				 entry; ++entry)
			{
				sourceRow(entry.col()) = entry.value();
			}
			VectorXd destinationRow = VectorXd::Zero(destination.size());
			for (int sourceIndex = 0; sourceIndex < sourceRow.size(); sourceIndex++)
			{
				if (sourceRow(sourceIndex) == 0)
				{
					continue;
				}
				for (int destinationIndex = 0;
					 destinationIndex < static_cast<int>(destination.size());
					 destinationIndex++)
				{
					if (destination[destinationIndex] == source[sourceIndex])
					{
						destinationRow(destinationIndex) = sourceRow(sourceIndex);
						break;
					}
				}
			}
			auto relativeResidual = [&](const VectorXd& candidate)
			{
				return (transform.transpose() * candidate - sourceRow).norm()
					/ std::max(1.0, sourceRow.norm());
			};
			double residual = relativeResidual(destinationRow);
			if (!std::isfinite(residual)
			 || residual > functionalTransportTolerance)
			{
				SparseMatrix<double> transposed = transform.transpose();
				transposed.makeCompressed();
				SparseQR<SparseMatrix<double>, COLAMDOrdering<int>> qr;
				qr.compute(transposed);
				if (qr.info() != Eigen::Success)
				{
					if (failIfUnrepresentable)
					{
						lastFailure = "PERSISTENT_FUNCTIONAL_TRANSPORT_QR_FAILED_"
							+ eventKind;
						return false;
					}
					history.functionalAvailable = false;
					history.keys = destination;
					history.row.resize(0, destination.size());
					continue;
				}
				destinationRow = qr.solve(sourceRow);
				residual = relativeResidual(destinationRow);
			}
			if (!destinationRow.allFinite()
			 || !std::isfinite(residual)
			 || residual > functionalTransportTolerance)
			{
				if (failIfUnrepresentable)
				{
					lastFailure = "PERSISTENT_FUNCTIONAL_NOT_TRANSPORTABLE_"
						+ eventKind;
					return false;
				}
				history.functionalAvailable = false;
				history.keys = destination;
				history.row.resize(0, destination.size());
				continue;
			}
			if (processCovariance != nullptr)
			{
				const double injectedVariance = destinationRow.dot(
					*processCovariance * destinationRow);
				const double covarianceScale = std::max(
					1.0, processCovariance->norm());
				if (!std::isfinite(injectedVariance)
				 || std::abs(injectedVariance)
					> functionalTransportTolerance * covarianceScale)
				{
					if (failIfUnrepresentable)
					{
						lastFailure = "PERSISTENT_FUNCTIONAL_HAS_PROCESS_NOISE_"
							+ eventKind;
						return false;
					}
					history.functionalAvailable = false;
					history.keys = destination;
					history.row.resize(0, destination.size());
					continue;
				}
			}
			history.keys = destination;
			history.row = destinationRow.transpose().sparseView(
				0, sparseTolerance);
		}
		return true;
	}

	std::map<std::string, PhysicalTargetHistory> lastTargets;
	std::string lastFailure;
	std::string lastTargetDispositionReason;
	double maximumReplayPriorMeanRelativeError = 0;
	double maximumReplayPriorCovarianceRelativeError = 0;
	double maximumTargetMeanRelativeError = 0;
	double maximumTargetVarianceRelativeError = 0;
	double maximumRawSquareRootMeanRelativeError = 0;
	double maximumRawSquareRootCovarianceRelativeError = 0;
	// Shadow continuity tolerance only.  The strict acceptance tests are the
	// retained physical target marginal and integer candidate, not equality of
	// every heterogeneous nuisance-state covariance entry.
	double rawSquareRootReplayTolerance = 1e-4;
};
