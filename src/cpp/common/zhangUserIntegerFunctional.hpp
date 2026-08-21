#pragma once

#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "common/zhangIntegerTargets.hpp"

/** Exact integer path absorbed by one Hou/AV-style satellite product.
 *
 * networkCoefficients is expressed on physical receiver-satellite ambiguity
 * arcs in physicalEdges order.  It is deliberately structural: evaluating a
 * current float state is not an acceptable replacement for this row.
 */
struct ZhangProductIntegerFunctional
{
	SatSys					 satellite;
	SatSys					 referenceSatellite;
	std::vector<ZhangGraphEdge> physicalEdges;
	ZhangExactVector			 networkCoefficients;
	std::vector<int>			 physicalArcVersions;
	ZhangExactInteger		 affineOffsetCycles = 0;
	int					 temporalBasisVersion = 0;
	bool					 valid = false;
	std::string				 failureReason;
};

/** Exact change between two physical satellite-product functionals.
 *
 * A change is an integer-estimable current-epoch target only when this signed
 * versioned-arc row belongs to the current fundamental-cycle lattice.  If an
 * arc version has retired, the same object is instead a BESD target for a
 * multi-epoch raw-factor window; silently evaluating it on the new state is
 * not valid.
 */
struct ZhangProductIntegerTransition
{
	std::vector<ZhangGraphEdge> physicalEdges;
	std::vector<int>            physicalArcVersions;
	ZhangExactVector            coefficients;
	ZhangExactInteger           affineOffsetCycles = 0;
	bool                        valid = false;
	std::string                 failureReason;
};

struct ZhangTargetedBesdSelection
{
	bool selected = false;
	int physicalTerms = 0;
	std::string reason = "UNCLASSIFIED";
};

/** Strict routing gate for the lightweight fixed-lag BESD shadow.
 *
 * Current-cycle relations belong to ProductRelationAdmission and an already
 * held relation is transported exactly.  Only a non-held transition that
 * references an edge/version absent from the post-event physical graph may
 * allocate a target Schur tracker.  A phase-segment change is a product reset,
 * never evidence for a cross-segment BESD integer.
 */
inline ZhangTargetedBesdSelection zhangSelectTargetedBesdTransition(
	const ZhangProductIntegerTransition& transition,
	const std::set<ZhangGraphEdge>& postEventEdges,
	const std::map<ZhangGraphEdge, int>& postEventArcVersions,
	bool heldContained,
	bool phaseSegmentChanged)
{
	ZhangTargetedBesdSelection result;
	if (!transition.valid
	 || transition.physicalEdges.size() != transition.coefficients.size()
	 || transition.physicalEdges.size() !=
		transition.physicalArcVersions.size())
	{
		result.reason = "INVALID_TRANSITION";
		return result;
	}
	if (phaseSegmentChanged)
	{
		result.reason = "PHASE_SEGMENT_RESET";
		return result;
	}
	if (transition.coefficients.empty())
	{
		result.reason = "IDENTICAL_PHYSICAL_FUNCTIONAL";
		return result;
	}
	if (heldContained)
	{
		result.reason = "EXACT_HELD_TRANSPORT";
		return result;
	}
	for (std::size_t index = 0; index < transition.coefficients.size(); index++)
	{
		if (transition.coefficients[index] == 0)
		{
			continue;
		}
		result.physicalTerms++;
		auto version = postEventArcVersions.find(
			transition.physicalEdges[index]);
		if (postEventEdges.count(transition.physicalEdges[index]) == 0
		 || version == postEventArcVersions.end()
		 || version->second != transition.physicalArcVersions[index])
		{
			result.selected = true;
			result.reason = "REQUIRES_BESD_RETIRED_ARC";
			return result;
		}
	}
	result.reason = "CURRENT_PHYSICAL_GRAPH_RELATION";
	return result;
}

inline ZhangProductIntegerTransition zhangProductIntegerFunctionalDifferenceImpl(
	const ZhangProductIntegerFunctional& previous,
	const ZhangProductIntegerFunctional& current,
	bool requireSameSatellite)
{
	ZhangProductIntegerTransition result;
	if (!previous.valid || !current.valid
	 || (requireSameSatellite && previous.satellite != current.satellite)
	 || previous.physicalEdges.size() != previous.networkCoefficients.size()
	 || previous.physicalEdges.size() != previous.physicalArcVersions.size()
	 || current.physicalEdges.size() != current.networkCoefficients.size()
	 || current.physicalEdges.size() != current.physicalArcVersions.size())
	{
		result.failureReason = "INVALID_FUNCTIONAL_DIFFERENCE";
		return result;
	}

	using VersionedEdge = std::pair<ZhangGraphEdge, int>;
	std::map<VersionedEdge, ZhangExactInteger> difference;
	for (std::size_t index = 0; index < previous.physicalEdges.size(); index++)
	{
		difference[{previous.physicalEdges[index],
			previous.physicalArcVersions[index]}] -=
			previous.networkCoefficients[index];
	}
	for (std::size_t index = 0; index < current.physicalEdges.size(); index++)
	{
		difference[{current.physicalEdges[index],
			current.physicalArcVersions[index]}] +=
			current.networkCoefficients[index];
	}
	for (const auto& [arc, coefficient] : difference)
	{
		if (coefficient == 0)
		{
			continue;
		}
		result.physicalEdges.push_back(arc.first);
		result.physicalArcVersions.push_back(arc.second);
		result.coefficients.push_back(coefficient);
	}
	result.affineOffsetCycles =
		current.affineOffsetCycles - previous.affineOffsetCycles;
	result.valid = true;
	result.failureReason = result.coefficients.empty()
		? "IDENTICAL_PHYSICAL_FUNCTIONAL"
		: "EXACT_VERSIONED_ARC_DIFFERENCE";
	return result;
}

/** Same physical product observed at two epochs/coordinates. */
inline ZhangProductIntegerTransition zhangProductIntegerFunctionalDifference(
	const ZhangProductIntegerFunctional& previous,
	const ZhangProductIntegerFunctional& current)
{
	return zhangProductIntegerFunctionalDifferenceImpl(
		previous, current, true);
}

/** Complete one exact transition value from its held physical-arc row.
 * The affine term is part of the integer functional and must be transported
 * together with the lattice value. */
inline ZhangExactInteger zhangCompleteProductTransitionInteger(
	const ZhangProductIntegerTransition& transition,
	const ZhangExactInteger&             physicalRowValue)
{
	return physicalRowValue + transition.affineOffsetCycles;
}

/** Between-satellite product integer functional at one epoch.
 *
 * Unlike the temporal transition above, the endpoints must be different
 * satellites.  The returned row represents current-previous, including the
 * exact affine offset and physical arc versions, and is the object that must
 * be tested against the held network lattice before a G_AR edge is promoted.
 */
inline ZhangProductIntegerTransition zhangProductIntegerFunctionalPairDifference(
	const ZhangProductIntegerFunctional& first,
	const ZhangProductIntegerFunctional& second)
{
	ZhangProductIntegerTransition result;
	if (first.satellite == second.satellite)
	{
		result.failureReason = "PAIR_REQUIRES_DISTINCT_SATELLITES";
		return result;
	}
	return zhangProductIntegerFunctionalDifferenceImpl(first, second, false);
}

inline std::string zhangProductPhysicalFunctionalFingerprint(
	const ZhangProductIntegerFunctional& functional)
{
	if (!functional.valid
	 || functional.physicalEdges.size() != functional.networkCoefficients.size()
	 || functional.physicalEdges.size() != functional.physicalArcVersions.size())
	{
		return "INVALID";
	}
	std::ostringstream stream;
	stream << zhangAuditSatelliteLabel(functional.referenceSatellite) << "->"
		   << zhangAuditSatelliteLabel(functional.satellite) << ":";
	for (std::size_t index = 0; index < functional.physicalEdges.size(); index++)
	{
		if (functional.networkCoefficients[index] == 0)
		{
			continue;
		}
		stream << functional.physicalEdges[index].receiver << "/"
			   << zhangAuditSatelliteLabel(
					functional.physicalEdges[index].satellite) << "@"
			   << functional.physicalArcVersions[index] << "="
			   << functional.networkCoefficients[index] << ";";
	}
	stream << "d=" << functional.affineOffsetCycles;
	return stream.str();
}

/** Diagnostic identity including the auxiliary product-tree generation.
 * Product continuity must use zhangProductPhysicalFunctionalFingerprint()
 * instead: a global tree generation can change because an unrelated receiver
 * arc changed while this satellite's non-zero physical path stayed identical.
 */
inline std::string zhangProductIntegerFunctionalFingerprint(
	const ZhangProductIntegerFunctional& functional)
{
	return zhangProductPhysicalFunctionalFingerprint(functional) +
		";tb=" + std::to_string(functional.temporalBasisVersion);
}

/** Build the network ambiguity path absorbed by every satellite product.
 *
 * For the Zhang/Hou all-in-view special case this path reduces to
 * z_x(s)^s-z_x(s)^ref.  For a general sparse product tree it is the signed
 * integer path between the two satellite nodes.
 */
inline std::map<SatSys, ZhangProductIntegerFunctional>
zhangBuildProductIntegerFunctionals(
	const ZhangGraphBasis&               productBasis,
	const std::map<ZhangGraphEdge, int>& arcVersions,
	const SatSys&                        requestedReference = SatSys(),
	int                                  temporalBasisVersion = 0)
{
	std::map<SatSys, ZhangProductIntegerFunctional> result;
	ZhangCanonicalIntegerAudit audit =
		zhangCanonicalIntegerAudit(productBasis);
	if (!audit.valid || productBasis.satellites.empty())
	{
		return result;
	}

	SatSys reference = requestedReference;
	if (reference.prn == 0)
	{
		reference = *productBasis.satellites.begin();
	}
	if (productBasis.satellites.find(reference) == productBasis.satellites.end())
	{
		return result;
	}

	std::vector<ZhangGraphEdge> physicalEdges(
		productBasis.edges.begin(), productBasis.edges.end());
	std::map<ZhangGraphEdge, std::size_t> physicalIndex;
	for (std::size_t index = 0; index < physicalEdges.size(); index++)
	{
		physicalIndex[physicalEdges[index]] = index;
	}

	auto makeFunctional = [&](const SatSys& satellite)
	{
		ZhangProductIntegerFunctional functional;
		functional.satellite = satellite;
		functional.referenceSatellite = reference;
		functional.physicalEdges = physicalEdges;
		functional.networkCoefficients = ZhangExactVector(physicalEdges.size());
		functional.physicalArcVersions.reserve(physicalEdges.size());
		functional.temporalBasisVersion = temporalBasisVersion;
		for (const auto& edge : physicalEdges)
		{
			auto version = arcVersions.find(edge);
			functional.physicalArcVersions.push_back(
				version == arcVersions.end() ? 0 : version->second);
		}
		functional.valid = true;
		return functional;
	};

	result[reference] = makeFunctional(reference);
	std::size_t targetRow = 0;
	for (const auto& satellite : productBasis.satellites)
	{
		if (satellite == reference)
		{
			continue;
		}
		if (targetRow >= audit.satelliteDatumSingleDifferences.size())
		{
			result.clear();
			return result;
		}
		auto functional = makeFunctional(satellite);
		const auto& treeRow = audit.satelliteDatumSingleDifferences[targetRow++];
		if (treeRow.size() != audit.treeEdges.size())
		{
			result.clear();
			return result;
		}
		for (std::size_t edge = 0; edge < treeRow.size(); edge++)
		{
			functional.networkCoefficients[
				physicalIndex.at(audit.treeEdges[edge])] = treeRow[edge];
		}
		result[satellite] = std::move(functional);
	}
	return result;
}

/** Joint network-user integer functional after applying the phase product.
 *
 * integerRows uses the primitive order [network physical arcs, user
 * ambiguities].  nuisanceRows and affineOffsets remain explicit so the audit
 * can prove, rather than assume, their cancellation.
 */
struct ZhangJointUserIntegerFunctional
{
	ZhangExactMatrix integerRows;
	MatrixXd         nuisanceRows;
	VectorXd         affineOffsetsCycles;
	std::vector<SatSys> targetSatellites;
	std::vector<SatSys> userAmbiguitySatellites;
	std::vector<ZhangGraphEdge> networkPhysicalEdges;
	std::vector<std::string> rowIdentities;
	bool        valid = false;
	std::string failureReason;

	VectorXd value(
		const ZhangExactVector& integerState,
		const VectorXd&         nuisanceState) const
	{
		if (!valid || integerRows.empty()
		 || integerState.size() != integerRows.front().size()
		 || nuisanceRows.cols() != nuisanceState.size()
		 || nuisanceRows.rows() != static_cast<int>(integerRows.size())
		 || affineOffsetsCycles.size() != static_cast<int>(integerRows.size()))
		{
			return VectorXd();
		}
		VectorXd result(integerRows.size());
		for (std::size_t row = 0; row < integerRows.size(); row++)
		{
			ZhangExactInteger exact = 0;
			for (std::size_t column = 0; column < integerRows[row].size(); column++)
			{
				exact += integerRows[row][column] * integerState[column];
			}
			result(row) = exact.convert_to<double>();
		}
		return result + nuisanceRows * nuisanceState + affineOffsetsCycles;
	}
};

/** Construct (z_u^s-z_u^ref)-h_s^T z_N for a fixed product S-system. */
inline ZhangJointUserIntegerFunctional zhangBuildJointUserIntegerFunctional(
	const std::map<SatSys, ZhangProductIntegerFunctional>& products,
	const SatSys& userReference,
	int nuisanceDimension = 0)
{
	ZhangJointUserIntegerFunctional result;
	if (products.empty() || products.find(userReference) == products.end()
	 || nuisanceDimension < 0)
	{
		result.failureReason = "USER_REFERENCE_NOT_IN_PRODUCT_COMPONENT";
		return result;
	}
	const auto& reference = products.at(userReference);
	if (!reference.valid)
	{
		result.failureReason = "INVALID_REFERENCE_PRODUCT_FUNCTIONAL";
		return result;
	}
	result.networkPhysicalEdges = reference.physicalEdges;
	for (const auto& [satellite, product] : products)
	{
		if (!product.valid || product.physicalEdges != result.networkPhysicalEdges
		 || product.networkCoefficients.size() !=
			reference.networkCoefficients.size())
		{
			result.failureReason = "INCONSISTENT_PRODUCT_FUNCTIONAL_LAYOUT";
			return result;
		}
		result.userAmbiguitySatellites.push_back(satellite);
	}
	std::map<SatSys, std::size_t> userIndex;
	for (std::size_t index = 0; index < result.userAmbiguitySatellites.size(); index++)
	{
		userIndex[result.userAmbiguitySatellites[index]] = index;
	}

	const std::size_t networkSize = result.networkPhysicalEdges.size();
	const std::size_t primitiveSize = networkSize + userIndex.size();
	std::vector<double> affineOffsets;
	for (const auto& [satellite, product] : products)
	{
		if (satellite == userReference)
		{
			continue;
		}
		ZhangExactVector row(primitiveSize);
		for (std::size_t edge = 0; edge < networkSize; edge++)
		{
			// Subtract the network datum absorbed by the satellite product,
			// relative to the datum absorbed by the user reference product.
			row[edge] = -(
				product.networkCoefficients[edge]
				- reference.networkCoefficients[edge]);
		}
		row[networkSize + userIndex.at(satellite)] = +1;
		row[networkSize + userIndex.at(userReference)] = -1;
		result.integerRows.push_back(std::move(row));
		result.targetSatellites.push_back(satellite);
		result.rowIdentities.push_back(
			zhangAuditSatelliteLabel(satellite) + "-" +
			zhangAuditSatelliteLabel(userReference));
		// The emitted satellite phase contains the product functional's exact
		// integer affine branch.  Since the user model applies -(C-B), the
		// corresponding ambiguity functional subtracts the satellite-minus-
		// reference product branch together with its network-arc row.
		affineOffsets.push_back(-(
			product.affineOffsetCycles
			- reference.affineOffsetCycles).convert_to<double>());
	}
	result.nuisanceRows = MatrixXd::Zero(result.integerRows.size(), nuisanceDimension);
	result.affineOffsetsCycles.resize(result.integerRows.size());
	for (std::size_t row = 0; row < affineOffsets.size(); row++)
	{
		result.affineOffsetsCycles(row) = affineOffsets[row];
	}
	result.valid = !result.integerRows.empty();
	if (!result.valid)
	{
		result.failureReason = "NO_NON_REFERENCE_USER_INTEGER_FUNCTIONALS";
	}
	return result;
}

struct ZhangUserIntegerLatticeAudit
{
	bool dimensionsValid = false;
	bool nuisanceOrthogonal = false;
	bool affineInteger = false;
	bool integerCoefficientsRepresentable = false;
	bool primitiveAdmissible = false;
	bool valid = false;
	double maximumNuisanceCoefficient = std::numeric_limits<double>::infinity();
	double maximumAffineIntegerError = std::numeric_limits<double>::infinity();
	ZhangIntegerTransformAudit primitiveAudit;
	std::string failureReason;
};

inline ZhangUserIntegerLatticeAudit zhangAuditUserIntegerLattice(
	const ZhangJointUserIntegerFunctional& functional,
	double nuisanceTolerance = 1e-12,
	double affineTolerance = 1e-10)
{
	ZhangUserIntegerLatticeAudit result;
	if (!functional.valid || functional.integerRows.empty())
	{
		result.failureReason = "INVALID_USER_INTEGER_FUNCTIONAL";
		return result;
	}
	const std::size_t primitiveSize = functional.integerRows.front().size();
	result.dimensionsValid = primitiveSize > 0
		&& functional.nuisanceRows.rows() ==
			static_cast<int>(functional.integerRows.size())
		&& functional.affineOffsetsCycles.size() ==
			static_cast<int>(functional.integerRows.size())
		&& std::all_of(
			functional.integerRows.begin(), functional.integerRows.end(),
			[&](const auto& row) { return row.size() == primitiveSize; });
	if (!result.dimensionsValid)
	{
		result.failureReason = "USER_INTEGER_FUNCTIONAL_DIMENSION_MISMATCH";
		return result;
	}

	result.maximumNuisanceCoefficient = functional.nuisanceRows.size() == 0
		? 0.0
		: functional.nuisanceRows.cwiseAbs().maxCoeff();
	result.nuisanceOrthogonal = functional.nuisanceRows.allFinite()
		&& result.maximumNuisanceCoefficient <= nuisanceTolerance;
	result.maximumAffineIntegerError = 0;
	for (int row = 0; row < functional.affineOffsetsCycles.size(); row++)
	{
		const double value = functional.affineOffsetsCycles(row);
		if (!std::isfinite(value))
		{
			result.maximumAffineIntegerError =
				std::numeric_limits<double>::infinity();
			break;
		}
		result.maximumAffineIntegerError = std::max(
			result.maximumAffineIntegerError,
			std::abs(value - std::round(value)));
	}
	result.affineInteger =
		result.maximumAffineIntegerError <= affineTolerance;

	// Exact Smith invariants scale to the global network.  Enumerating every
	// maximal minor is combinatorial (thousands choose tens) and an audit-size
	// failure is not evidence that the row lattice is non-primitive.
	result.integerCoefficientsRepresentable = true;
	ZhangIntegerLatticeMembership smith = zhangIntegerRowLatticeContains(
		functional.integerRows, ZhangExactVector(primitiveSize));
	result.primitiveAudit.baseRank = primitiveSize;
	result.primitiveAudit.targetRank = functional.integerRows.size();
	result.primitiveAudit.fullColumnRank =
		smith.rank == static_cast<int>(functional.integerRows.size());
	ZhangExactInteger saturationIndex = 1;
	for (const auto& invariant : smith.smithInvariants)
	{
		saturationIndex *= zhangExactAbs(invariant);
	}
	result.primitiveAudit.gcdMaximalMinors = saturationIndex;
	result.primitiveAudit.primitive =
		result.primitiveAudit.fullColumnRank && saturationIndex == 1;
	result.primitiveAudit.unimodular =
		primitiveSize == functional.integerRows.size() &&
		result.primitiveAudit.primitive;
	result.primitiveAudit.valid =
		result.primitiveAudit.fullColumnRank &&
		result.primitiveAudit.primitive;
	if (!result.primitiveAudit.valid)
	{
		result.primitiveAudit.failureReason =
			!result.primitiveAudit.fullColumnRank
				? "INTEGER_TARGET_TRANSFORM_RANK_DEFICIENT"
				: "INTEGER_TARGET_TRANSFORM_NOT_PRIMITIVE";
	}
	result.primitiveAdmissible = result.primitiveAudit.valid;
	result.valid = result.dimensionsValid
		&& result.nuisanceOrthogonal
		&& result.affineInteger
		&& result.integerCoefficientsRepresentable
		&& result.primitiveAdmissible;
	if (!result.valid)
	{
		if (!result.nuisanceOrthogonal)
			result.failureReason = "REAL_NUISANCE_LEAKS_INTO_INTEGER_FUNCTIONAL";
		else if (!result.affineInteger)
			result.failureReason = "NON_INTEGER_AFFINE_OFFSET";
		else if (!result.integerCoefficientsRepresentable)
			result.failureReason = "INTEGER_COEFFICIENT_OUT_OF_RANGE";
		else
			result.failureReason = result.primitiveAudit.failureReason;
	}
	return result;
}

enum class ZhangTemporalIntegerDatumAction
{
	EXACT_TRANSPORT_NO_BESD,
	ESTIMATE_BESD,
	RESET_PRODUCT_DATUM
};

inline ZhangTemporalIntegerDatumAction zhangClassifyTemporalIntegerDatumAction(
	bool physicalArcVersionChanged,
	bool exactStructuralTransportAvailable,
	bool besdIntegerFunctionalAvailable)
{
	if (!physicalArcVersionChanged && exactStructuralTransportAvailable)
	{
		return ZhangTemporalIntegerDatumAction::EXACT_TRANSPORT_NO_BESD;
	}
	if (physicalArcVersionChanged && besdIntegerFunctionalAvailable)
	{
		return ZhangTemporalIntegerDatumAction::ESTIMATE_BESD;
	}
	return ZhangTemporalIntegerDatumAction::RESET_PRODUCT_DATUM;
}
