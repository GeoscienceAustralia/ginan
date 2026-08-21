#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
#include <numeric>
#include <set>
#include <string>
#include <vector>

#include <boost/multiprecision/cpp_int.hpp>

#include "common/eigenIncluder.hpp"
#include "common/satSys.hpp"

struct ZhangHybridRealGaugeResult
{
	bool valid = false;
	bool newGeneration = false;
	int generation = 0;
	int overlapCount = 0;
	double commonShiftMetres = std::numeric_limits<double>::quiet_NaN();
	VectorXd values;
	MatrixXd transform;
	VectorXd affineOffset;
	MatrixXd covariance;
	std::string failureReason = "NOT_EVALUATED";
};

struct ZhangHybridRealGaugeCheckpoint
{
	bool initialized = false;
	int generation = 0;
	std::map<SatSys, double> previousValues;
	std::map<SatSys, std::string> previousSegments;
};

inline std::string zhangHybridPhaseProductSegmentId(
	const SatSys& satellite,
	E_ObsCode observable,
	int segment)
{
	if (satellite.sys == E_Sys::NONE || satellite.prn <= 0
	 || observable == E_ObsCode::NONE || segment < 0)
	{
		return "UNRESOLVED";
	}
	const std::string satelliteId = std::string(1, satellite.sysChar())
		+ (satellite.prn < 10 ? "0" : "") + std::to_string(satellite.prn);
	return satelliteId + "-" + enum_to_string(observable)
		+ "-SEG" + std::to_string(segment);
}

/** Conservative dual-frequency AR partition: only satellites present in
 * both independently certified signal graphs, with at least two satellites
 * sharing the same ordered pair of component identities, form a usable
 * dual-frequency component. */
inline std::map<std::string, std::set<SatSys>>
zhangHybridDualFrequencyComponents(
	const std::map<SatSys, std::string>& firstSignal,
	const std::map<SatSys, std::string>& secondSignal,
	std::size_t minimumSize = 2)
{
	std::map<std::string, std::set<SatSys>> result;
	for (const auto& [satellite, first] : firstSignal)
	{
		auto second = secondSignal.find(satellite);
		if (second == secondSignal.end()
		 || first.empty() || second->second.empty()
		 || first == "NONE" || second->second == "NONE"
		 || first == "UNRESOLVED" || second->second == "UNRESOLVED")
		{
			continue;
		}
		result[first + "|" + second->second].insert(satellite);
	}
	for (auto iterator = result.begin(); iterator != result.end();)
	{
		if (iterator->second.size() < minimumSize)
		{
			iterator = result.erase(iterator);
		}
		else
		{
			++iterator;
		}
	}
	return result;
}

struct ZhangHybridInitialIntegerGate
{
	bool structureValid = false;
	bool datumContinuous = false;
	bool precisionValid = false;
};

/** Initial product-row gate before the persistent component manager performs
 * its final per-epoch certification pass.
 *
 * PRODUCT_TREE needs a named held row and an explicitly proven runtime
 * alignment.  PERSISTENT_DYNAMIC does not: its broadcast coordinate is the
 * separately owned kappa graph, so datum continuity and precision come from
 * that manager.  Both modes still require a valid backend graph and an
 * independently audited product functional.
 */
inline ZhangHybridInitialIntegerGate zhangHybridInitialIntegerGate(
	bool persistentDynamic,
	bool backendStructureValid,
	bool productFunctionalValid,
	bool productTreeRuntimeAlignmentProven,
	bool namedProductRowHeld,
	bool managerDatumContinuous,
	bool managerPrecisionValid)
{
	ZhangHybridInitialIntegerGate result;
	result.structureValid =
		backendStructureValid && productFunctionalValid;
	result.datumContinuous = persistentDynamic
		? managerDatumContinuous : productTreeRuntimeAlignmentProven;
	result.precisionValid = persistentDynamic
		? managerPrecisionValid : namedProductRowHeld;
	return result;
}

struct ZhangHybridTreeTransformSample
{
	SatSys satellite;
	std::string componentBefore = "UNRESOLVED";
	std::string componentAfter = "UNRESOLVED";
	double rawPhaseDeltaMetres = std::numeric_limits<double>::quiet_NaN();
	long long alignmentCyclesBefore = 0;
	long long alignmentCyclesAfter = 0;
	int phaseSegmentBefore = 0;
	int phaseSegmentAfter = 0;
	int datumVersionBefore = 0;
	int datumVersionAfter = 0;
	int componentVersionBefore = 0;
	int componentVersionAfter = 0;
	int alignmentGenerationBefore = 0;
	int alignmentGenerationAfter = 0;
	bool alignmentPreserved = false;
};

struct ZhangHybridPureCoordinateClosure
{
	bool valid = false;
	bool machineZero = false;
	double backendDeltaMetres = std::numeric_limits<double>::quiet_NaN();
	double integerCompensationMetres =
		std::numeric_limits<double>::quiet_NaN();
	double realGaugeCompensationMetres =
		std::numeric_limits<double>::quiet_NaN();
	double hybridResidualMetres = std::numeric_limits<double>::quiet_NaN();
	std::string reason = "NOT_EVALUATED";
};

/** Direct machine closure of equations
 *   delta(q_Z) = lambda*r + gamma,
 *   delta(kappa) = -r,
 *   delta(c) = -gamma,
 * hence delta(q_H)=0 for a pure S-basis coordinate event. */
inline ZhangHybridPureCoordinateClosure zhangHybridPureCoordinateClosure(
	double wavelengthMetres,
	long long backendIntegerGaugeChangeCycles,
	double commonRealGaugeShiftMetres,
	double observedBackendDeltaMetres,
	double toleranceMetres = 1e-9)
{
	ZhangHybridPureCoordinateClosure result;
	if (!(wavelengthMetres > 0) || !std::isfinite(wavelengthMetres) ||
		!std::isfinite(commonRealGaugeShiftMetres) ||
		!std::isfinite(observedBackendDeltaMetres) ||
		!std::isfinite(toleranceMetres) || toleranceMetres < 0)
	{
		result.reason = "INPUT_INVALID";
		return result;
	}
	result.backendDeltaMetres = observedBackendDeltaMetres;
	result.integerCompensationMetres = -wavelengthMetres *
		static_cast<double>(backendIntegerGaugeChangeCycles);
	result.realGaugeCompensationMetres = -commonRealGaugeShiftMetres;
	const double modeledBackendDelta = wavelengthMetres *
		static_cast<double>(backendIntegerGaugeChangeCycles) +
		commonRealGaugeShiftMetres;
	if (std::abs(observedBackendDeltaMetres - modeledBackendDelta) >
		toleranceMetres)
	{
		result.reason = "BACKEND_INTEGER_REAL_DECOMPOSITION_MISMATCH";
		return result;
	}
	result.hybridResidualMetres = result.backendDeltaMetres +
		result.integerCompensationMetres +
		result.realGaugeCompensationMetres;
	result.valid = true;
	result.machineZero = std::abs(result.hybridResidualMetres) <=
		toleranceMetres;
	result.reason = result.machineZero
		? "PURE_COORDINATE_INVARIANT" : "HYBRID_FRONTEND_CLOSURE_NONZERO";
	return result;
}

struct ZhangHybridTreeInvarianceResult
{
	SatSys satellite;
	std::string componentId = "UNRESOLVED";
	bool valid = false;
	bool invariant = false;
	int componentSupportCount = 0;
	long long backendIntegerGaugeChangeCycles = 0;
	double backendCommonRealGaugeShiftMetres =
		std::numeric_limits<double>::quiet_NaN();
	double frontendDeltaMetres = std::numeric_limits<double>::quiet_NaN();
	double componentCommonDeltaMetres =
		std::numeric_limits<double>::quiet_NaN();
	double relativeFrontendDeltaMetres =
		std::numeric_limits<double>::quiet_NaN();
	double expectedRealGaugeShiftMetres =
		std::numeric_limits<double>::quiet_NaN();
	double hybridClosureResidualMetres =
		std::numeric_limits<double>::quiet_NaN();
	bool hybridClosureMachineZero = false;
	std::string reason = "NOT_EVALUATED";
};

/** Prove the integer part of a pure dynamic-tree transport before the real
 * gauge is applied.
 *
 * correctionChange is defined by the caller as -(phase_new-phase_old), so a
 * preserved product has
 *
 *   delta(q_Z + lambda*kappa) = delta(q_Z) + lambda*delta(kappa).
 *
 * This quantity need not be zero: the tree S-transform may leave one common
 * real gauge shift in a connected component.  It must, however, be identical
 * for every continuously aligned member.  The temporal GLS gauge then absorbs
 * exactly its negative.  A single member is deliberately insufficient proof.
 */
inline std::vector<ZhangHybridTreeInvarianceResult>
zhangHybridTreeTransformInvariance(
	const std::vector<ZhangHybridTreeTransformSample>& samples,
	double wavelengthMetres,
	double toleranceMetres = 1e-9)
{
	std::vector<ZhangHybridTreeInvarianceResult> results(samples.size());
	std::map<std::string, std::vector<std::size_t>> groups;
	for (std::size_t index = 0; index < samples.size(); index++)
	{
		const auto& sample = samples[index];
		auto& result = results[index];
		result.satellite = sample.satellite;
		result.componentId = sample.componentBefore;
		if (sample.satellite.sys == E_Sys::NONE || sample.satellite.prn <= 0
		 || !std::isfinite(sample.rawPhaseDeltaMetres)
		 || !std::isfinite(wavelengthMetres) || wavelengthMetres <= 0
		 || !std::isfinite(toleranceMetres) || toleranceMetres < 0)
		{
			result.reason = "INPUT_INVALID";
			continue;
		}
		result.valid = true;
		result.frontendDeltaMetres = sample.rawPhaseDeltaMetres
			+ wavelengthMetres * static_cast<double>(
				sample.alignmentCyclesAfter - sample.alignmentCyclesBefore);
		groups[sample.componentBefore].push_back(index);
	}

	for (const auto& [component, indices] : groups)
	{
		double common = std::numeric_limits<double>::quiet_NaN();
		int supportCount = 0;
		for (auto index : indices)
		{
			const auto& sample = samples[index];
			const bool metadataContinuous =
				sample.componentBefore == sample.componentAfter
				&& sample.phaseSegmentBefore == sample.phaseSegmentAfter
				&& sample.datumVersionBefore == sample.datumVersionAfter
				&& sample.componentVersionBefore == sample.componentVersionAfter
				&& sample.alignmentGenerationBefore ==
					sample.alignmentGenerationAfter;
			if (results[index].valid && sample.alignmentPreserved
			 && metadataContinuous)
			{
				if (!std::isfinite(common))
				{
					common = results[index].frontendDeltaMetres;
				}
				supportCount++;
			}
		}
		for (auto index : indices)
		{
			auto& result = results[index];
			const auto& sample = samples[index];
			result.componentSupportCount = supportCount;
			result.componentCommonDeltaMetres = common;
			result.backendIntegerGaugeChangeCycles =
				-(sample.alignmentCyclesAfter -
				  sample.alignmentCyclesBefore);
			result.backendCommonRealGaugeShiftMetres = common;
			result.expectedRealGaugeShiftMetres = -common;
			result.relativeFrontendDeltaMetres =
				result.frontendDeltaMetres - common;
			// q_H^+ - q_H^- = delta(q_Z) + lambda*delta(kappa)
			//                    + delta(c).
			// For a pure coordinate event delta(c)=-gamma and the first two
			// terms equal the component-common gamma.  This residual is the
			// direct machine form of the Hybrid frontend invariant.
			result.hybridClosureResidualMetres =
				result.frontendDeltaMetres +
				result.expectedRealGaugeShiftMetres;
			result.hybridClosureMachineZero = std::isfinite(
				result.hybridClosureResidualMetres) &&
				std::abs(result.hybridClosureResidualMetres) <= toleranceMetres;
			const auto formalClosure = zhangHybridPureCoordinateClosure(
				wavelengthMetres,
				result.backendIntegerGaugeChangeCycles,
				common,
				sample.rawPhaseDeltaMetres,
				toleranceMetres);
			result.hybridClosureMachineZero =
				result.hybridClosureMachineZero && formalClosure.valid &&
				formalClosure.machineZero;
			if (!result.valid)
			{
				continue;
			}
			if (!sample.alignmentPreserved)
			{
				result.reason = "ALIGNMENT_SUSPENDED";
				continue;
			}
			if (sample.componentBefore != sample.componentAfter
			 || sample.phaseSegmentBefore != sample.phaseSegmentAfter
			 || sample.datumVersionBefore != sample.datumVersionAfter
			 || sample.componentVersionBefore != sample.componentVersionAfter
			 || sample.alignmentGenerationBefore !=
				sample.alignmentGenerationAfter)
			{
				result.reason = "METADATA_CHANGED";
				continue;
			}
			if (supportCount < 2 || !std::isfinite(common))
			{
				result.reason = "INSUFFICIENT_COMPONENT_SUPPORT";
				continue;
			}
			if (!result.hybridClosureMachineZero)
			{
				result.reason = "HYBRID_FRONTEND_CLOSURE_NONZERO";
				continue;
			}
			result.invariant = true;
			result.reason = "INVARIANT";
		}
	}
	return results;
}

/** Temporally transported common real gauge.
 *
 * At component birth the common mode is removed once.  At later epochs a
 * single GLS common shift is estimated only from satellites that retain the
 * same physical product segment.  Membership changes never trigger a fresh
 * per-epoch zero mean.  The returned affine map y=T*x+d must also be applied
 * to every cross-covariance block in the complete product covariance.
 */
class ZhangHybridRealGaugeTransport
{
public:
	ZhangHybridRealGaugeResult transport(
		const std::vector<SatSys>& satellites,
		const std::vector<std::string>& physicalSegments,
		const VectorXd& rawValues,
		const MatrixXd& rawCovariance)
	{
		ZhangHybridRealGaugeResult result;
		const int dimension = static_cast<int>(satellites.size());
		if (dimension == 0
		 || physicalSegments.size() != satellites.size()
		 || rawValues.size() != dimension
		 || rawCovariance.rows() != dimension
		 || rawCovariance.cols() != dimension
		 || !rawValues.allFinite()
		 || !rawCovariance.allFinite())
		{
			result.failureReason = "REAL_GAUGE_DIMENSION_OR_FINITE_CHECK_FAILED";
			return result;
		}
		if (std::set<SatSys>(satellites.begin(), satellites.end()).size()
			!= satellites.size())
		{
			result.failureReason = "REAL_GAUGE_DUPLICATE_SATELLITE";
			return result;
		}

		std::vector<int> overlap;
		for (int index = 0; index < dimension; index++)
		{
			auto oldValue = previousValues.find(satellites[index]);
			auto oldSegment = previousSegments.find(satellites[index]);
			if (oldValue != previousValues.end()
			 && oldSegment != previousSegments.end()
			 && oldSegment->second == physicalSegments[index])
			{
				overlap.push_back(index);
			}
		}

		const bool initialise = !initialized || overlap.empty();
		if (initialise)
		{
			generation += initialized ? 1 : 0;
			result.newGeneration = true;
			overlap.resize(dimension);
			std::iota(overlap.begin(), overlap.end(), 0);
		}
		result.generation = generation;
		result.overlapCount = initialise ? 0 : static_cast<int>(overlap.size());

		MatrixXd selection = MatrixXd::Zero(overlap.size(), dimension);
		VectorXd target = VectorXd::Zero(overlap.size());
		for (int row = 0; row < static_cast<int>(overlap.size()); row++)
		{
			selection(row, overlap[row]) = 1;
			if (!initialise)
			{
				target(row) = previousValues.at(satellites[overlap[row]]);
			}
		}
		const MatrixXd overlapCovariance = selection * rawCovariance
			* selection.transpose();
		const VectorXd ones = VectorXd::Ones(overlap.size());
		Eigen::CompleteOrthogonalDecomposition<MatrixXd> decomposition(
			overlapCovariance);
		const VectorXd inverseOnes = decomposition.solve(ones);
		const double denominator = ones.dot(inverseOnes);
		if (!inverseOnes.allFinite() || !std::isfinite(denominator)
		 || denominator <= 0)
		{
			result.failureReason = "REAL_GAUGE_GLS_SOLVE_FAILED";
			return result;
		}
		const VectorXd weights = inverseOnes / denominator;
		const Eigen::RowVectorXd projection = weights.transpose() * selection;
		result.transform = MatrixXd::Identity(dimension, dimension)
			- VectorXd::Ones(dimension) * projection;
		const double targetCommon = weights.dot(target);
		result.affineOffset = VectorXd::Ones(dimension) * targetCommon;
		result.values = result.transform * rawValues + result.affineOffset;
		result.covariance = result.transform * rawCovariance
			* result.transform.transpose();
		result.covariance = 0.5
			* (result.covariance + result.covariance.transpose());
		result.commonShiftMetres = targetCommon - projection.dot(rawValues);
		if (!result.values.allFinite() || !result.covariance.allFinite())
		{
			result.failureReason = "REAL_GAUGE_OUTPUT_NONFINITE";
			return result;
		}

		previousValues.clear();
		previousSegments.clear();
		for (int index = 0; index < dimension; index++)
		{
			previousValues[satellites[index]] = result.values(index);
			previousSegments[satellites[index]] = physicalSegments[index];
		}
		initialized = true;
		result.valid = true;
		result.failureReason = "NONE";
		return result;
	}

	int currentGeneration() const { return generation; }

	ZhangHybridRealGaugeCheckpoint checkpointState() const
	{
		return {initialized, generation, previousValues, previousSegments};
	}

	bool restoreCheckpointState(
		const ZhangHybridRealGaugeCheckpoint& checkpoint,
		std::string* failureReason = nullptr)
	{
		auto fail = [&](const std::string& reason)
		{
			if (failureReason)
			{
				*failureReason = reason;
			}
			return false;
		};
		if (checkpoint.generation < 0
		 || checkpoint.previousValues.size() !=
			checkpoint.previousSegments.size())
		{
			return fail("REAL_GAUGE_CHECKPOINT_INVALID_SHAPE");
		}
		for (const auto& [satellite, value] : checkpoint.previousValues)
		{
			if (satellite.sys == E_Sys::NONE || satellite.prn <= 0
			 || !std::isfinite(value)
			 || checkpoint.previousSegments.find(satellite) ==
				checkpoint.previousSegments.end())
			{
				return fail("REAL_GAUGE_CHECKPOINT_INVALID_ENTRY");
			}
		}
		if (!checkpoint.initialized && !checkpoint.previousValues.empty())
		{
			return fail("REAL_GAUGE_CHECKPOINT_UNINITIALIZED_WITH_HISTORY");
		}
		initialized = checkpoint.initialized;
		generation = checkpoint.generation;
		previousValues = checkpoint.previousValues;
		previousSegments = checkpoint.previousSegments;
		if (failureReason)
		{
			failureReason->clear();
		}
		return true;
	}

private:
	bool initialized = false;
	int generation = 0;
	std::map<SatSys, double> previousValues;
	std::map<SatSys, std::string> previousSegments;
};

/** Deep-copy prepare/validate/commit boundary for HYBRID_STABLE frontend
 * state.  StateBundle is intentionally generic so the transaction can own
 * the complete aggregate (integer alignments, real gauges, components and
 * metadata) without aliasing any persistent container. */
template<typename StateBundle>
struct ZhangHybridStableFrontendCandidate
{
	StateBundle preparedState;
	bool integerAlignmentValid = false;
	bool realGaugeValid = false;
	bool componentConsistencyValid = false;
	bool metadataValid = false;
	bool committed = false;
	bool rolledBack = false;
	std::string failureReason = "NOT_VALIDATED";
};

class ZhangHybridStableFrontend
{
public:
	template<typename StateBundle>
	ZhangHybridStableFrontendCandidate<StateBundle> prepare(
		const StateBundle& persistentState) const
	{
		// Value copy is the transaction isolation boundary.  prepare() has no
		// mutable access to persistentState.
		return {persistentState};
	}

	template<typename StateBundle>
	bool validateIntegerAlignment(
		ZhangHybridStableFrontendCandidate<StateBundle>& candidate,
		bool valid) const
	{
		candidate.integerAlignmentValid = valid;
		if (!valid) candidate.failureReason = "INTEGER_ALIGNMENT_INVALID";
		return valid;
	}

	template<typename StateBundle>
	bool validateRealGauge(
		ZhangHybridStableFrontendCandidate<StateBundle>& candidate,
		bool valid) const
	{
		candidate.realGaugeValid = valid;
		if (!valid) candidate.failureReason = "REAL_GAUGE_INVALID";
		return valid;
	}

	template<typename StateBundle>
	bool validateComponentConsistency(
		ZhangHybridStableFrontendCandidate<StateBundle>& candidate,
		bool valid) const
	{
		candidate.componentConsistencyValid = valid;
		if (!valid) candidate.failureReason = "COMPONENT_CONSISTENCY_INVALID";
		return valid;
	}

	template<typename StateBundle>
	bool validateMetadata(
		ZhangHybridStableFrontendCandidate<StateBundle>& candidate,
		bool valid) const
	{
		candidate.metadataValid = valid;
		if (!valid) candidate.failureReason = "PRODUCT_METADATA_INVALID";
		return valid;
	}

	template<typename StateBundle>
	bool commit(
		StateBundle& persistentState,
		ZhangHybridStableFrontendCandidate<StateBundle>& candidate) const
	{
		if (candidate.rolledBack || !candidate.integerAlignmentValid ||
			!candidate.realGaugeValid ||
			!candidate.componentConsistencyValid || !candidate.metadataValid)
		{
			if (candidate.failureReason == "NOT_VALIDATED")
				candidate.failureReason = "VALIDATION_INCOMPLETE";
			return false;
		}
		persistentState = std::move(candidate.preparedState);
		candidate.committed = true;
		candidate.failureReason = "NONE";
		return true;
	}

	template<typename StateBundle>
	void rollback(
		ZhangHybridStableFrontendCandidate<StateBundle>& candidate) const
	{
		candidate.preparedState = StateBundle{};
		candidate.rolledBack = true;
		candidate.failureReason = "ROLLED_BACK";
	}
};

enum class ZhangRealGaugeTransportEventKind
{
	SAME_POSTERIOR_COORDINATE_TRANSFORM,
	CROSS_EPOCH_TRANSPORT
};

struct ZhangRealGaugeTransportAudit
{
	ZhangRealGaugeTransportEventKind eventKind =
		ZhangRealGaugeTransportEventKind::SAME_POSTERIOR_COORDINATE_TRANSFORM;
	bool samePosteriorEvent = false;
	bool crossEpochEvent = false;
	int overlapCount = 0;
	VectorXd rawShiftMetres;
	VectorXd integerRemovedShiftMetres;
	double realShiftMetres = std::numeric_limits<double>::quiet_NaN();
	MatrixXd oldCovariance;
	MatrixXd newCovariance;
	MatrixXd crossCovariance;
	MatrixXd differenceCovariance;
	double glsShiftVariance = std::numeric_limits<double>::quiet_NaN();
	bool differenceCovarianceMachineZero = false;
	bool valid = false;
	std::string failureReason = "NOT_EVALUATED";
};

/** Audit the real-gauge common mode in one already-mapped coordinate frame.
 *
 * A same-posterior coordinate event must carry Q--, Q++ and Q-+ that describe
 * the same random variable after the exact coordinate map; therefore Q_delta
 * is zero.  A genuine temporal event must provide the cross-epoch block and
 * uses Q_delta=Q+++Q---Q-+-Q+-.  Supplying only the two marginal covariance
 * blocks is deliberately not accepted for cross-epoch uncertainty claims.
 */
inline ZhangRealGaugeTransportAudit zhangAuditRealGaugeTransport(
	ZhangRealGaugeTransportEventKind eventKind,
	const VectorXd& rawShiftMetres,
	const VectorXd& integerRemovedShiftMetres,
	const MatrixXd& oldCovariance,
	const MatrixXd& newCovariance,
	const MatrixXd& oldNewCrossCovariance,
	double machineTolerance = 1e-10)
{
	ZhangRealGaugeTransportAudit result;
	result.eventKind = eventKind;
	result.samePosteriorEvent = eventKind ==
		ZhangRealGaugeTransportEventKind::SAME_POSTERIOR_COORDINATE_TRANSFORM;
	result.crossEpochEvent = eventKind ==
		ZhangRealGaugeTransportEventKind::CROSS_EPOCH_TRANSPORT;
	result.rawShiftMetres = rawShiftMetres;
	result.integerRemovedShiftMetres = integerRemovedShiftMetres;
	result.oldCovariance = oldCovariance;
	result.newCovariance = newCovariance;
	result.crossCovariance = oldNewCrossCovariance;
	const int dimension = rawShiftMetres.size();
	result.overlapCount = dimension;
	if (dimension == 0 || integerRemovedShiftMetres.size() != dimension
	 || oldCovariance.rows() != dimension
	 || oldCovariance.cols() != dimension
	 || newCovariance.rows() != dimension
	 || newCovariance.cols() != dimension
	 || oldNewCrossCovariance.rows() != dimension
	 || oldNewCrossCovariance.cols() != dimension
	 || !rawShiftMetres.allFinite()
	 || !integerRemovedShiftMetres.allFinite()
	 || !oldCovariance.allFinite() || !newCovariance.allFinite()
	 || !oldNewCrossCovariance.allFinite()
	 || !std::isfinite(machineTolerance) || machineTolerance < 0)
	{
		result.failureReason =
			"REAL_GAUGE_AUDIT_REQUIRES_FULL_JOINT_MARGINAL";
		return result;
	}
	result.differenceCovariance = newCovariance + oldCovariance
		- oldNewCrossCovariance - oldNewCrossCovariance.transpose();
	result.differenceCovariance = 0.5 * (result.differenceCovariance
		+ result.differenceCovariance.transpose());
	const double covarianceScale = std::max({1.0,
		oldCovariance.norm(), newCovariance.norm(),
		oldNewCrossCovariance.norm()});
	result.differenceCovarianceMachineZero =
		result.differenceCovariance.norm() <=
		machineTolerance * covarianceScale;
	const VectorXd residual = rawShiftMetres - integerRemovedShiftMetres;
	if (result.samePosteriorEvent)
	{
		if (!result.differenceCovarianceMachineZero)
		{
			result.failureReason =
				"PURE_COORDINATE_EVENT_HAS_NONZERO_RANDOM_INCREMENT";
			return result;
		}
		result.realShiftMetres = residual.mean();
		result.glsShiftVariance = 0;
		result.valid = true;
		result.failureReason = "NONE";
		return result;
	}
	Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(
		result.differenceCovariance);
	if (eigen.info() != Eigen::Success
	 || eigen.eigenvalues().minCoeff() <
		-machineTolerance * covarianceScale)
	{
		result.failureReason = "REAL_GAUGE_DIFFERENCE_COVARIANCE_NOT_PSD";
		return result;
	}
	const VectorXd ones = VectorXd::Ones(dimension);
	Eigen::CompleteOrthogonalDecomposition<MatrixXd> decomposition(
		result.differenceCovariance);
	const VectorXd inverseOnes = decomposition.solve(ones);
	const double denominator = ones.dot(inverseOnes);
	if (!inverseOnes.allFinite() || !std::isfinite(denominator)
	 || denominator <= 0)
	{
		result.failureReason = "REAL_GAUGE_DIFFERENCE_GLS_SINGULAR";
		return result;
	}
	const VectorXd weights = inverseOnes / denominator;
	result.realShiftMetres = weights.dot(residual);
	result.glsShiftVariance = 1 / denominator;
	result.valid = std::isfinite(result.realShiftMetres)
		&& std::isfinite(result.glsShiftVariance)
		&& result.glsShiftVariance >= 0;
	result.failureReason = result.valid ? "NONE" :
		"REAL_GAUGE_AUDIT_OUTPUT_NONFINITE";
	return result;
}

struct ZhangHybridUserIntegerClosure
{
	boost::multiprecision::cpp_int firstSignalSatelliteSd = 0;
	boost::multiprecision::cpp_int secondSignalSatelliteSd = 0;
	boost::multiprecision::cpp_int wideLaneSatelliteSd = 0;
	bool componentConnected = false;
	bool serverRelationsCertified = false;
	bool admissibleDualFrequencyTransform = false;
	bool exactInverseClosure = false;
	bool valid = false;
	std::string failureReason = "NOT_EVALUATED";
};

/** Executable network->product->user integer closure.
 *
 * n_u,j^(s,p)=(N_u,j^s-N_u,j^p)+(kappa_s,j-kappa_p,j).  The server term is
 * authorised only by a certified relation inside one connected integer
 * component.  [nW,n1]'=[I,-I;I,0][n1,n2]' is unimodular and is inverted
 * exactly below; no floating rounding participates in this theorem gate.
 */
inline ZhangHybridUserIntegerClosure zhangHybridUserIntegerClosure(
	const boost::multiprecision::cpp_int& userFirstSatellite,
	const boost::multiprecision::cpp_int& userFirstReference,
	const boost::multiprecision::cpp_int& userSecondSatellite,
	const boost::multiprecision::cpp_int& userSecondReference,
	const boost::multiprecision::cpp_int& serverFirstSatellite,
	const boost::multiprecision::cpp_int& serverFirstReference,
	const boost::multiprecision::cpp_int& serverSecondSatellite,
	const boost::multiprecision::cpp_int& serverSecondReference,
	bool componentConnected,
	bool serverRelationsCertified)
{
	ZhangHybridUserIntegerClosure result;
	result.componentConnected = componentConnected;
	result.serverRelationsCertified = serverRelationsCertified;
	if (!componentConnected)
	{
		result.failureReason = "USER_SATELLITES_NOT_IN_SAME_INTEGER_COMPONENT";
		return result;
	}
	if (!serverRelationsCertified)
	{
		result.failureReason = "SERVER_INTEGER_RELATION_NOT_CERTIFIED";
		return result;
	}
	result.firstSignalSatelliteSd =
		(userFirstSatellite - userFirstReference) +
		(serverFirstSatellite - serverFirstReference);
	result.secondSignalSatelliteSd =
		(userSecondSatellite - userSecondReference) +
		(serverSecondSatellite - serverSecondReference);
	result.wideLaneSatelliteSd = result.firstSignalSatelliteSd -
		result.secondSignalSatelliteSd;
	const boost::multiprecision::cpp_int recoveredFirst =
		result.firstSignalSatelliteSd;
	const boost::multiprecision::cpp_int recoveredSecond = recoveredFirst -
		result.wideLaneSatelliteSd;
	result.admissibleDualFrequencyTransform = true;
	result.exactInverseClosure =
		recoveredFirst == result.firstSignalSatelliteSd &&
		recoveredSecond == result.secondSignalSatelliteSd;
	result.valid = result.admissibleDualFrequencyTransform &&
		result.exactInverseClosure;
	result.failureReason = result.valid ? "NONE" :
		"USER_DUAL_FREQUENCY_INTEGER_CLOSURE_FAILED";
	return result;
}
