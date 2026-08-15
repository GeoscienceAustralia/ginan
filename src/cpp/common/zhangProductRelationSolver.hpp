#pragma once

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <map>
#include <numeric>
#include <set>
#include <string>
#include <vector>

#include "common/eigenIncluder.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "common/zhangIarGainAudit.hpp"
#include "common/zhangProductRelationBasis.hpp"
#include "common/zhangQuotientIntegerLattice.hpp"

/** One exact satellite-pair coordinate in a named star ambient lattice.
 * Nodes [0,namedCount) are named satellite-minus-reference coordinates and
 * node namedCount is the canonical reference satellite. */
struct ZhangCertifiedPairRelation
{
	int firstNode = -1;
	int secondNode = -1;
	ZhangExactInteger value = 0;
	ZhangExactVector parentCombination;
	std::string coordinate = "UNKNOWN";
	// True only when at least one of the WL/L1 coordinates forming this
	// dual-frequency edge came from a previously certified ledger row that
	// survived the current private-branch joint-NIS admission.
	bool fromTemporalLedger = false;
};

/** Exact product-lattice constraints returned by the product IAR solver.
 *
 * Product rows are expressed in the complete mappable product coordinate,
 * not in a transient named-subset or LAMBDA-reduced suffix.  networkRows and
 * networkIntegers are the exact affine pull-back to the ambiguity coordinates
 * consumed by GinAR_mtx.  Mixed rows remain valid conditioning information
 * even when they contain no individually recoverable satellite-pair edge. */
struct ZhangProductIntegerConstraintSet
{
	bool reliable = false;
	bool exactNetworkMapping = false;
	E_Sys system = E_Sys::NONE;
	E_ObsCode firstObservable = E_ObsCode::NONE;
	E_ObsCode secondObservable = E_ObsCode::NONE;
	SatSys referenceSatellite;
	// Node i of every product row denotes coordinateSatellites[i] minus
	// referenceSatellite.  The reference itself is graph node N.
	std::vector<SatSys> coordinateSatellites;
	int productCoordinateDimension = 0;
	int networkAmbiguityDimension = 0;

	ZhangExactMatrix wideLaneProductRows;
	ZhangExactVector wideLaneIntegers;
	ZhangExactMatrix firstSignalProductRows;
	ZhangExactVector firstSignalIntegers;

	ZhangExactMatrix networkRows;
	ZhangExactVector networkIntegers;
	// Joint [z1,z2] rows corresponding one-to-one with networkRows.
	ZhangExactMatrix jointProductRows;
	// Immutable current physical-arc identities, populated by AMBRES after the
	// solver returns because only it owns the KF ambiguity map and arc versions.
	std::vector<std::map<std::string, ZhangExactInteger>> physicalNetworkRows;
	std::string phaseSegmentFingerprint;
	std::uint64_t backendBasisGeneration = 0;

	std::vector<ZhangCertifiedPairRelation> certifiedPairs;
	// Pair relations proved by both WL and conditional-L1.  These, and only
	// these, are allowed to form the dual-frequency broadcast certificate graph.
	std::vector<ZhangCertifiedPairRelation> dualFrequencyCertifiedPairs;
	ZhangExactMatrix conditioningOnlyRows;
	ZhangExactVector conditioningOnlyIntegers;

	int conditioningRank = 0;
	int certifiedPairRank = 0;
	double jointNis = std::numeric_limits<double>::quiet_NaN();
	double jointNisThreshold = std::numeric_limits<double>::quiet_NaN();
	double failureProbability = 1;
	double referenceInvariantProductGain = 0;
	std::string failureReason = "NOT_EVALUATED";
};

/** Result boundary for the direct satellite-product integer solver.
 *
 * This object deliberately separates structural estimability, statistical
 * reliability and frontend admission.  A positive network fixed rank alone
 * can never set certifiedForProduct. */
struct ZhangProductRelationFixResult
{
	bool basisValid = false;
	bool mappingValid = false;
	bool wideLaneReliable = false;
	bool firstSignalReliable = false;
	bool certifiedForProduct = false;
	int fullTargetRank = 0;
	int mappableTargetRank = 0;
	int wideLaneFixedRank = 0;
	int firstSignalFixedRank = 0;
	int namedFirstSignalFixed = 0;
	int namedSecondSignalFixed = 0;
	int evaluatedBranches = 0;
	int namedRoundWideLaneCandidates = 0;
	int namedRoundWideLaneRetained = 0;
	int selectedRawPartialFixedRank = 0;
	int selectedRecoveredNamedRank = 0;
	int selectedParentBranchRank = 0;
	double selectedPartialFixFraction = 0;
	int componentCoverageGain = 0;
	int certifiedJointIntegerRank = 0;
	double maximumWideLanePerr = 1;
	double maximumWideLaneMarginalRoundPerr = 1;
	double wideLaneParentFailureProbabilityBound = 1;
	double maximumFirstSignalPerr = 1;
	double productInformationGain = 0;
	double realSubspaceUpperBoundAtSelectedRank =
		std::numeric_limits<double>::quiet_NaN();
	// Ratio to the unconstrained real-subspace relaxation.  This is not an
	// integer-search efficiency until an integer-constrained frontier exists.
	double relaxedRealUpperBoundCapture =
		std::numeric_limits<double>::quiet_NaN();
	double realIntegerGainGap =
		std::numeric_limits<double>::quiet_NaN();
	double relationRho5 = std::numeric_limits<double>::quiet_NaN();
	double relationRho10 = std::numeric_limits<double>::quiet_NaN();
	double relationRho20 = std::numeric_limits<double>::quiet_NaN();
	double relationRho40 = std::numeric_limits<double>::quiet_NaN();
	double relationRho80 = std::numeric_limits<double>::quiet_NaN();
	int realSubspaceRank80 = 0;
	int realSubspaceRank90 = 0;
	int realSubspaceRank95 = 0;
	std::string gainSpectrumDiagnosis = "NOT_EVALUATED";
	double jointNis = std::numeric_limits<double>::quiet_NaN();
	double jointNisThreshold = std::numeric_limits<double>::quiet_NaN();
	std::map<std::size_t, ZhangExactInteger> namedWideLane;
	std::map<std::size_t, ZhangExactInteger> namedFirstSignal;
	std::map<std::size_t, ZhangExactInteger> namedSecondSignal;
	std::vector<int> selectedNamedRelationIndices;
	std::string selectedCanonicalHnf;
	std::string wideLaneCertificateSource = "NONE";
	bool namedOrderingValid = false;
	bool namedSubsetCertificate = false;
	ZhangProductIntegerConstraintSet constraints;
	std::string status = "NOT_EVALUATED";
	std::string failureReason = "NONE";
};

/** One product-aware PAR beam state.
 *
 * The candidate coordinates are always original named satellite relations.
 * LAMBDA may decorrelate internally, but a branch is certifiable only when
 * its fixed integer row lattice recovers every named coordinate in this list.
 */
struct ProductParBranch
{
	std::vector<int> namedRelationIndices;
	int integerRank = 0;
	bool reliabilityPassed = false;
	int rawPartialFixedRank = 0;
	int recoveredNamedRank = 0;
	int parentBranchRank = 0;
	bool inheritedFromParentFixedLattice = false;
	double partialFixFraction = 0;
	double maximumNamedPerr = 1;
	double normalizedCandidateNis = std::numeric_limits<double>::infinity();
	double maxPerr = 1;
	double jointNis = std::numeric_limits<double>::quiet_NaN();
	double jointNisThreshold = std::numeric_limits<double>::quiet_NaN();
	int componentCoverageGain = 0;
	double productInformationGain = 0;
	std::string canonicalHnf;
};

/** Lexicographic product-PAR score.  Reliability is an admission gate, not a
 * weighted reward: an unreliable candidate always ranks below every reliable
 * one, regardless of coverage or gain. */
struct ZhangProductRelationLexicographicScore
{
	bool reliabilityPassed = false;
	double partialFixFraction = 0;
	int rawPartialFixedRank = 0;
	int recoveredNamedRank = 0;
	double normalizedCandidateNis = std::numeric_limits<double>::infinity();
	double maximumNamedPerr = 1;
	int componentCoverageGain = 0;
	double productInformationGain = 0;

	bool operator<(const ZhangProductRelationLexicographicScore& other) const
	{
		if (reliabilityPassed != other.reliabilityPassed)
		{
			return reliabilityPassed < other.reliabilityPassed;
		}
		if (!reliabilityPassed)
		{
			if (recoveredNamedRank != other.recoveredNamedRank)
			{
				return recoveredNamedRank < other.recoveredNamedRank;
			}
			if (partialFixFraction != other.partialFixFraction)
			{
				return partialFixFraction < other.partialFixFraction;
			}
			if (rawPartialFixedRank != other.rawPartialFixedRank)
			{
				return rawPartialFixedRank < other.rawPartialFixedRank;
			}
			const double leftNis = std::isfinite(normalizedCandidateNis)
				? normalizedCandidateNis : std::numeric_limits<double>::infinity();
			const double rightNis = std::isfinite(other.normalizedCandidateNis)
				? other.normalizedCandidateNis : std::numeric_limits<double>::infinity();
			if (leftNis != rightNis)
			{
				return leftNis > rightNis;
			}
			if (maximumNamedPerr != other.maximumNamedPerr)
			{
				return maximumNamedPerr > other.maximumNamedPerr;
			}
		}
		if (componentCoverageGain != other.componentCoverageGain)
		{
			return componentCoverageGain < other.componentCoverageGain;
		}
		return productInformationGain < other.productInformationGain;
	}
};

inline ZhangProductRelationLexicographicScore zhangProductParScore(
	const ProductParBranch& branch)
{
	return {
		branch.reliabilityPassed,
		branch.partialFixFraction,
		branch.rawPartialFixedRank,
		branch.recoveredNamedRank,
		branch.normalizedCandidateNis,
		branch.maximumNamedPerr,
		branch.componentCoverageGain,
		branch.productInformationGain
	};
}

/** One named satellite edge proposed to the product-aware forest beam. */
struct ZhangNamedPairBeamCandidate
{
	ZhangExactVector row;
	double perr = 1;
	double gain = 0;
	double variance = std::numeric_limits<double>::infinity();
	std::vector<int> nodes;
};

struct ZhangNamedPairBeamBranch
{
	std::vector<int> selected;
	double maximumPerr = 0;
	double summedGain = 0;
	double summedVariance = 0;
	int coveredNodes = 0;
};

/** Retain alternate primitive named-edge forests at every rank.
 *
 * This is deliberately an expansion beam, not deletion from one greedy tree:
 * an edge excluded from the first high-gain forest remains reachable through
 * another branch.  Ordering is the requested reliability -> satellite
 * coverage -> product gain lexicographic order. */
inline std::vector<std::vector<ZhangNamedPairBeamBranch>>
zhangNamedPairForestBeamLevels(
	const std::vector<ZhangNamedPairBeamCandidate>& candidates,
	int dimension,
	int beamWidth)
{
	std::vector<std::vector<ZhangNamedPairBeamBranch>> levels;
	if (dimension <= 0 || beamWidth <= 0 || candidates.empty()) return levels;
	auto quality = [&](const std::vector<int>& selected)
	{
		ZhangNamedPairBeamBranch branch;
		branch.selected = selected;
		std::set<int> nodes;
		for (int index : selected)
		{
			if (index < 0 || index >= static_cast<int>(candidates.size()))
				return ZhangNamedPairBeamBranch{};
			branch.maximumPerr = std::max(
				branch.maximumPerr, candidates[index].perr);
			branch.summedGain += candidates[index].gain;
			branch.summedVariance += candidates[index].variance;
			nodes.insert(candidates[index].nodes.begin(),
				candidates[index].nodes.end());
		}
		branch.coveredNodes = nodes.size();
		return branch;
	};
	auto better = [](const auto& left, const auto& right)
	{
		if (left.maximumPerr != right.maximumPerr)
			return left.maximumPerr < right.maximumPerr;
		if (left.coveredNodes != right.coveredNodes)
			return left.coveredNodes > right.coveredNodes;
		if (left.summedGain != right.summedGain)
			return left.summedGain > right.summedGain;
		if (left.summedVariance != right.summedVariance)
			return left.summedVariance < right.summedVariance;
		return left.selected < right.selected;
	};

	std::vector<ZhangNamedPairBeamBranch> frontier = {
		ZhangNamedPairBeamBranch{}};
	for (int targetRank = 1;
		targetRank <= dimension && !frontier.empty(); targetRank++)
	{
		std::map<std::vector<int>, ZhangNamedPairBeamBranch> unique;
		for (const auto& parent : frontier)
		for (int index = 0; index < static_cast<int>(candidates.size()); index++)
		{
			if (std::binary_search(
				parent.selected.begin(), parent.selected.end(), index)) continue;
			auto selected = parent.selected;
			selected.push_back(index);
			std::sort(selected.begin(), selected.end());
			ZhangExactMatrix rows;
			for (int selectedIndex : selected)
				rows.push_back(candidates[selectedIndex].row);
			int exactRank = 0;
			if (!zhangExactPrimitiveRowLattice(rows, dimension, &exactRank) ||
				exactRank != static_cast<int>(rows.size())) continue;
			unique.try_emplace(selected, quality(selected));
		}
		frontier.clear();
		for (auto& [selected, branch] : unique)
			frontier.push_back(std::move(branch));
		std::sort(frontier.begin(), frontier.end(), better);
		if (frontier.size() > static_cast<std::size_t>(beamWidth))
			frontier.resize(beamWidth);
		if (!frontier.empty()) levels.push_back(frontier);
	}
	return levels;
}

/** Prove that L1 and L2 use the same named satellite-minus-reference rows.
 * Numeric row indices alone are insufficient because two independently
 * compiled bases can assign the same index to different satellites. */
inline bool zhangProductNamedOrderingMatches(
	const ZhangProductRelationBasis& first,
	const ZhangProductRelationBasis& second)
{
	if (first.mappableNamedIndices.size() !=
		second.mappableNamedIndices.size())
	{
		return false;
	}
	for (std::size_t local = 0;
		 local < first.mappableNamedIndices.size(); local++)
	{
		const int firstIndex = first.mappableNamedIndices[local];
		const int secondIndex = second.mappableNamedIndices[local];
		if (firstIndex < 0 || secondIndex < 0 ||
			firstIndex >= static_cast<int>(first.namedRelations.size()) ||
			secondIndex >= static_cast<int>(second.namedRelations.size()))
		{
			return false;
		}
		const auto& firstRow = first.namedRelations[firstIndex];
		const auto& secondRow = second.namedRelations[secondIndex];
		if (firstRow.satellite != secondRow.satellite ||
			firstRow.referenceSatellite != secondRow.referenceSatellite)
		{
			return false;
		}
	}
	return true;
}

/** Exact affine pull-back of product WL and first-signal integer rows.
 *
 * For Fw(z1-z2)=nw and F1 z1=n1 this produces
 * [Fw(G1-G2); F1 G1] a =
 * [nw-Fw(c1-c2); n1-F1 c1].  Numeric basis matrices are accepted only when
 * every coefficient is exactly integral within the representation tolerance. */
inline bool zhangPullBackProductIntegerConstraints(
	const ZhangProductRelationBasis& firstBasis,
	const ZhangProductRelationBasis& secondBasis,
	const ZhangExactMatrix& wideLaneRows,
	const ZhangExactVector& wideLaneIntegers,
	const ZhangExactMatrix& firstSignalRows,
	const ZhangExactVector& firstSignalIntegers,
	ZhangExactMatrix& networkRows,
	ZhangExactVector& networkIntegers,
	std::string& failureReason)
{
	networkRows.clear();
	networkIntegers.clear();
	const int productRank = firstBasis.mappableTargetRank;
	const int networkDimension = firstBasis.transform.cols();
	if (productRank <= 0 || networkDimension <= 0 ||
		secondBasis.mappableTargetRank != productRank ||
		secondBasis.transform.cols() != networkDimension ||
		firstBasis.transform.rows() != productRank ||
		secondBasis.transform.rows() != productRank ||
		firstBasis.affineOffsets.size() != static_cast<std::size_t>(productRank) ||
		secondBasis.affineOffsets.size() != static_cast<std::size_t>(productRank) ||
		wideLaneRows.size() != wideLaneIntegers.size() ||
		firstSignalRows.size() != firstSignalIntegers.size())
	{
		failureReason = "PRODUCT_CONSTRAINT_DIMENSION_MISMATCH";
		return false;
	}
	auto exactBasisCoefficient = [](const MatrixXd& transform,
		int row, int column, ZhangExactInteger& coefficient)
	{
		const long long rounded = std::llround(transform(row, column));
		if (std::abs(transform(row, column) - rounded) > 1e-8) return false;
		coefficient = rounded;
		return true;
	};
	auto append = [&](const ZhangExactVector& productRow,
		const ZhangExactInteger& integer, bool wideLane)
	{
		if (productRow.size() != static_cast<std::size_t>(productRank))
			return false;
		ZhangExactVector networkRow(networkDimension);
		ZhangExactInteger rhs = integer;
		for (int product = 0; product < productRank; product++)
		{
			const ZhangExactInteger multiplier = productRow[product];
			if (multiplier == 0) continue;
			for (int column = 0; column < networkDimension; column++)
			{
				ZhangExactInteger first = 0;
				ZhangExactInteger second = 0;
				if (!exactBasisCoefficient(firstBasis.transform,
					product, column, first) ||
					!exactBasisCoefficient(secondBasis.transform,
					product, column, second)) return false;
				networkRow[column] += multiplier *
					(wideLane ? first - second : first);
			}
			const ZhangExactInteger offset = wideLane
				? firstBasis.affineOffsets.at(product) -
					secondBasis.affineOffsets.at(product)
				: firstBasis.affineOffsets.at(product);
			rhs -= multiplier * offset;
		}
		networkRows.push_back(std::move(networkRow));
		networkIntegers.push_back(std::move(rhs));
		return true;
	};
	for (std::size_t row = 0; row < wideLaneRows.size(); row++)
	{
		if (!append(wideLaneRows[row], wideLaneIntegers[row], true))
		{
			failureReason = "WL_PRODUCT_TO_NETWORK_MAPPING_FAILED";
			return false;
		}
	}
	for (std::size_t row = 0; row < firstSignalRows.size(); row++)
	{
		if (!append(firstSignalRows[row], firstSignalIntegers[row], false))
		{
			failureReason = "L1_PRODUCT_TO_NETWORK_MAPPING_FAILED";
			return false;
		}
	}
	failureReason = "NONE";
	return true;
}

/** Recover every individually proven named target from an exact fixed row
 * lattice.  Each returned unit row has passed exact integer HNF/Smith lattice
 * containment; unrecoverable named rows are omitted rather than fabricated. */
inline std::map<std::size_t, ZhangExactInteger>
zhangRecoverCertifiedNamedProductSubset(
	const ZhangExactMatrix& fixedRows,
	const ZhangExactVector& fixedValues,
	int namedCount)
{
	return ProductConstraintPromotion::recoverNamedTargets(
		fixedRows, fixedValues, namedCount);
}

/** Full-subset wrapper used by outer named PAR.  Inner LAMBDA/PAR may expose a
 * smaller exact named seed through zhangRecoverCertifiedNamedProductSubset(),
 * but a branch is a complete certificate only when every row is recovered. */
inline std::map<std::size_t, ZhangExactInteger>
zhangRecoverCompleteNamedProductSubset(
	const ZhangExactMatrix& fixedRows,
	const ZhangExactVector& fixedValues,
	int namedCount)
{
	auto recovered = zhangRecoverCertifiedNamedProductSubset(
		fixedRows, fixedValues, namedCount);
	if (recovered.size() != static_cast<std::size_t>(namedCount))
	{
		return {};
	}
	for (int index = 0; index < namedCount; index++)
	{
		if (!recovered.contains(index)) return {};
	}
	return recovered;
}

/** Exact promotion of named coordinates from an already accepted parent
 * integer lattice.
 *
 * If the parent mixed LAMBDA/PAR lattice has passed its bootstrap-success and
 * absolute NIS gates, every unit row contained in that lattice is a
 * deterministic integer function of the accepted parent solution.  Such a
 * row must inherit the parent certificate; re-running LAMBDA on its marginal
 * covariance is mathematically different because it discards the joint
 * constraints that made the row identifiable.  This helper performs only the
 * exact HNF membership/value proof.  The caller remains responsible for
 * proving that the parent search was statistically accepted. */
struct ZhangInheritedNamedCertificate
{
	bool exact = false;
	int parentFixedRank = 0;
	std::map<std::size_t, ZhangExactInteger> values;
};

/** Recover every directly named satellite-pair edge contained in an accepted
 * mixed fixed lattice.  Unlike named-star promotion this tests both unit rows
 * e_s and pair rows e_s-e_t.  Higher-order fixed integers that contain no pair
 * edge are deliberately retained only as conditioning evidence by callers. */
inline std::vector<ZhangCertifiedPairRelation>
zhangRecoverCertifiedPairRelations(
	const ZhangExactMatrix& fixedRows,
	const ZhangExactVector& fixedValues,
	int namedCount,
	bool parentStatisticallyAccepted)
{
	std::vector<ZhangCertifiedPairRelation> result;
	if (!parentStatisticallyAccepted || namedCount <= 0 ||
		fixedRows.empty() || fixedRows.size() != fixedValues.size())
	{
		return result;
	}
	for (int first = 0; first <= namedCount; first++)
	for (int second = first + 1; second <= namedCount; second++)
	{
		ZhangExactVector pairRow(namedCount);
		if (first < namedCount) pairRow[first] += 1;
		if (second < namedCount) pairRow[second] -= 1;
		const auto membership = zhangIntegerRowLatticeContains(
			fixedRows, pairRow);
		if (!membership.contained ||
			membership.combination.size() != fixedValues.size())
		{
			continue;
		}
		ZhangExactInteger value = 0;
		for (std::size_t row = 0; row < fixedValues.size(); row++)
		{
			value += membership.combination[row] * fixedValues[row];
		}
		result.push_back({first, second, value,
			membership.combination});
	}
	return result;
}

struct ZhangPairReliabilityEdge
{
	int firstNode = -1;
	int secondNode = -1;
	double perr = 1;
	double variance = std::numeric_limits<double>::infinity();
};

/** Reference-invariant all-pair incidence for a star-coordinate ambient
 * lattice.  Columns are K_s-K_ref for s<namedCount and the implicit final
 * node is the canonical reference.  Every unordered satellite pair appears
 * once, so D Q D' and its trace are invariant to the chosen star reference. */
inline MatrixXd zhangAllPairIncidence(int namedCount)
{
	if (namedCount <= 0) return MatrixXd(0, 0);
	const int nodeCount = namedCount + 1;
	MatrixXd result = MatrixXd::Zero(
		nodeCount * (nodeCount - 1) / 2, namedCount);
	int row = 0;
	for (int first = 0; first < nodeCount; first++)
	for (int second = first + 1; second < nodeCount; second++)
	{
		if (first < namedCount) result(row, first) += 1;
		if (second < namedCount) result(row, second) -= 1;
		row++;
	}
	return result;
}

struct ZhangExactConditioningAudit
{
	bool valid = false;
	int effectiveRank = 0;
	double maximumNullInnovation = 0;
	double nis = std::numeric_limits<double>::quiet_NaN();
	VectorXd mean;
	MatrixXd covariance;
	MatrixXd reduction;
};

/** Condition a Gaussian product coordinate on exact integer rows A z=n.
 * Rank-deficient constraint covariance is handled with an eigen pseudo-
 * inverse; a nonzero innovation in its null space fails closed. */
inline ZhangExactConditioningAudit zhangConditionExactProductRows(
	const VectorXd& mean,
	const MatrixXd& covariance,
	const MatrixXd& rows,
	const VectorXd& integers)
{
	ZhangExactConditioningAudit result;
	result.mean = mean;
	result.covariance = covariance;
	result.reduction = MatrixXd::Zero(covariance.rows(), covariance.cols());
	if (covariance.rows() != covariance.cols() || mean.size() != covariance.rows() ||
		rows.cols() != mean.size() || rows.rows() != integers.size()) return result;
	if (rows.rows() == 0)
	{
		result.valid = true;
		return result;
	}
	const MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
	MatrixXd constraint = rows * symmetric * rows.transpose();
	constraint = 0.5 * (constraint + constraint.transpose());
	Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(constraint);
	if (eigen.info() != Eigen::Success || !eigen.eigenvalues().allFinite())
		return result;
	const double largest = std::max(0.0, eigen.eigenvalues().maxCoeff());
	const double tolerance = std::max(1e-14, 1e-12 * largest);
	VectorXd inverse = VectorXd::Zero(rows.rows());
	const VectorXd innovation = integers - rows * mean;
	const VectorXd coordinates = eigen.eigenvectors().transpose() * innovation;
	for (int index = 0; index < rows.rows(); index++)
	{
		if (eigen.eigenvalues()(index) > tolerance)
		{
			inverse(index) = 1 / eigen.eigenvalues()(index);
			result.effectiveRank++;
		}
		else result.maximumNullInnovation = std::max(
			result.maximumNullInnovation, std::abs(coordinates(index)));
	}
	if (result.maximumNullInnovation > 1e-7) return result;
	const MatrixXd pseudoInverse = eigen.eigenvectors() * inverse.asDiagonal() *
		eigen.eigenvectors().transpose();
	const MatrixXd cross = symmetric * rows.transpose();
	result.reduction = cross * pseudoInverse * cross.transpose();
	result.mean = mean + cross * pseudoInverse * innovation;
	result.covariance = symmetric - result.reduction;
	result.covariance = 0.5 * (result.covariance + result.covariance.transpose());
	result.nis = innovation.dot(pseudoInverse * innovation);
	result.valid = result.mean.allFinite() && result.covariance.allFinite() &&
		result.reduction.allFinite() && std::isfinite(result.nis);
	return result;
}

inline double zhangReferenceInvariantPairTrace(const MatrixXd& covariance)
{
	if (covariance.rows() <= 0 || covariance.rows() != covariance.cols()) return 0;
	const MatrixXd pairs = zhangAllPairIncidence(covariance.rows());
	return (pairs * covariance * pairs.transpose()).trace();
}

struct ZhangComponentBridgeGls
{
	bool valid = false;
	int effectiveRank = 0;
	double mean = std::numeric_limits<double>::quiet_NaN();
	double variance = std::numeric_limits<double>::quiet_NaN();
	double residualNis = std::numeric_limits<double>::quiet_NaN();
	double maximumNullResidual = 0;
};

struct ZhangComponentGaugeGls
{
	bool valid = false;
	int measurementRank = 0;
	int gaugeRank = 0;
	Eigen::VectorXd mean;
	Eigen::MatrixXd covariance;
	double residualNis = std::numeric_limits<double>::quiet_NaN();
	double maximumNullResidual = 0;
};

struct ZhangComponentGaugeProductRow
{
	bool valid = false;
	ZhangExactVector row;
	ZhangExactInteger value = 0;
};

/** Map one fixed datum-free component-gauge row back to the named satellite
 * product lattice, including the already-certified within-component offsets.
 * An anchor equal to namedDimension denotes the implicit canonical reference
 * and therefore contributes no explicit coordinate column. */
inline ZhangComponentGaugeProductRow zhangComponentGaugeToProductRow(
	const ZhangExactVector& gaugeCombination,
	const std::vector<int>& componentAnchors,
	const ZhangExactVector& componentAnchorPotentials,
	int namedDimension,
	const ZhangExactInteger& gaugeIntegerValue)
{
	ZhangComponentGaugeProductRow result;
	if (namedDimension <= 0 || componentAnchors.size() < 2 ||
		componentAnchors.size() != componentAnchorPotentials.size() ||
		gaugeCombination.size() + 1 != componentAnchors.size()) return result;
	if (std::any_of(componentAnchors.begin(), componentAnchors.end(),
		[namedDimension](int anchor)
		{ return anchor < 0 || anchor > namedDimension; })) return result;
	result.row = ZhangExactVector(namedDimension);
	result.value = gaugeIntegerValue;
	const int datumAnchor = componentAnchors.front();
	for (int gauge = 0; gauge < static_cast<int>(gaugeCombination.size()); gauge++)
	{
		const auto& coefficient = gaugeCombination[gauge];
		const int componentAnchor = componentAnchors[gauge + 1];
		if (componentAnchor < namedDimension)
			result.row[componentAnchor] += coefficient;
		if (datumAnchor < namedDimension)
			result.row[datumAnchor] -= coefficient;
		result.value += coefficient *
			(componentAnchorPotentials[gauge + 1] -
			 componentAnchorPotentials.front());
	}
	result.valid = std::any_of(result.row.begin(), result.row.end(),
		[](const auto& value) { return value != 0; });
	return result;
}

/** Joint GLS reduction of all correlated cross-component observations
 *
 *     y = D_C c + e,  c in Z^(K-1).
 *
 * The measurement covariance may be singular because the complete set of
 * satellite-pair edges is deliberately retained.  The returned gauge
 * covariance is usable for ILS/PAR only when every datum-free component gauge
 * is estimable.  Null-space disagreement fails closed instead of being
 * discarded by the pseudo inverse.
 */
inline ZhangComponentGaugeGls zhangComponentGaugeGls(
	const Eigen::VectorXd& measurements,
	const Eigen::MatrixXd& covariance,
	const Eigen::MatrixXd& design)
{
	ZhangComponentGaugeGls result;
	const int count = measurements.size();
	const int gauges = design.cols();
	if (count == 0 || gauges == 0 || design.rows() != count ||
		covariance.rows() != count || covariance.cols() != count ||
		!measurements.allFinite() || !design.allFinite() ||
		!covariance.allFinite()) return result;

	const Eigen::MatrixXd symmetric =
		0.5 * (covariance + covariance.transpose());
	Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigen(symmetric);
	if (eigen.info() != Eigen::Success || !eigen.eigenvalues().allFinite())
		return result;
	const double largest = std::max(0.0, eigen.eigenvalues().maxCoeff());
	const double tolerance = std::max(1e-14, 1e-12 * largest);
	Eigen::VectorXd inverse = Eigen::VectorXd::Zero(count);
	for (int index = 0; index < count; index++)
	{
		if (eigen.eigenvalues()(index) <= tolerance) continue;
		inverse(index) = 1 / eigen.eigenvalues()(index);
		result.measurementRank++;
	}
	const Eigen::MatrixXd pseudoInverse =
		eigen.eigenvectors() * inverse.asDiagonal() *
		eigen.eigenvectors().transpose();
	const Eigen::MatrixXd information =
		design.transpose() * pseudoInverse * design;
	Eigen::CompleteOrthogonalDecomposition<Eigen::MatrixXd> informationSolver(
		information);
	result.gaugeRank = informationSolver.rank();
	if (result.gaugeRank != gauges) return result;
	result.covariance = informationSolver.solve(
		Eigen::MatrixXd::Identity(gauges, gauges));
	result.covariance = 0.5 *
		(result.covariance + result.covariance.transpose());
	result.mean = result.covariance * design.transpose() *
		pseudoInverse * measurements;
	const Eigen::VectorXd residual = measurements - design * result.mean;
	result.residualNis = residual.dot(pseudoInverse * residual);
	const Eigen::VectorXd nullResidual =
		residual - symmetric * pseudoInverse * residual;
	result.maximumNullResidual = nullResidual.lpNorm<Eigen::Infinity>();
	result.valid = result.mean.allFinite() && result.covariance.allFinite() &&
		std::isfinite(result.residualNis) &&
		result.maximumNullResidual <= 1e-7;
	return result;
}

/** GLS estimate of one shared relative integer gauge from correlated cross-
 * component edge measurements y=1*d+e. */
inline ZhangComponentBridgeGls zhangComponentBridgeGls(
	const VectorXd& measurements, const MatrixXd& covariance)
{
	ZhangComponentBridgeGls result;
	const int count = measurements.size();
	if (count == 0 || covariance.rows() != count || covariance.cols() != count)
		return result;
	const MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
	Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(symmetric);
	if (eigen.info() != Eigen::Success || !eigen.eigenvalues().allFinite())
		return result;
	const double largest = std::max(0.0, eigen.eigenvalues().maxCoeff());
	const double tolerance = std::max(1e-14, 1e-12 * largest);
	VectorXd inverse = VectorXd::Zero(count);
	for (int index = 0; index < count; index++)
	{
		if (eigen.eigenvalues()(index) > tolerance)
		{
			inverse(index) = 1 / eigen.eigenvalues()(index);
			result.effectiveRank++;
		}
	}
	const MatrixXd pseudoInverse = eigen.eigenvectors() * inverse.asDiagonal() *
		eigen.eigenvectors().transpose();
	const VectorXd ones = VectorXd::Ones(count);
	const double information = ones.dot(pseudoInverse * ones);
	if (!(information > 0) || !std::isfinite(information)) return result;
	result.variance = 1 / information;
	result.mean = result.variance * ones.dot(pseudoInverse * measurements);
	const VectorXd residual = measurements - ones * result.mean;
	result.residualNis = residual.dot(pseudoInverse * residual);
	const VectorXd nullResidual = residual - symmetric * pseudoInverse * residual;
	result.maximumNullResidual = nullResidual.lpNorm<Eigen::Infinity>();
	result.valid = std::isfinite(result.mean) && std::isfinite(result.variance) &&
		result.variance > 0 && std::isfinite(result.residualNis) &&
		result.maximumNullResidual <= 1e-7;
	return result;
}

/** Deterministic Kruskal forest over statistically admissible pair edges.
 * Perr is the primary order and variance only breaks ties.  The routine never
 * relaxes the reliability ceiling and therefore cannot turn a weak edge into
 * an integer certificate. */
inline std::vector<ZhangPairReliabilityEdge> zhangPairReliabilityForest(
	int nodeCount,
	std::vector<ZhangPairReliabilityEdge> edges,
	double maximumPerr)
{
	std::vector<ZhangPairReliabilityEdge> forest;
	if (nodeCount < 2 || maximumPerr < 0) return forest;
	std::sort(edges.begin(), edges.end(), [](const auto& left, const auto& right)
	{
		if (left.perr != right.perr) return left.perr < right.perr;
		if (left.variance != right.variance)
			return left.variance < right.variance;
		if (left.firstNode != right.firstNode)
			return left.firstNode < right.firstNode;
		return left.secondNode < right.secondNode;
	});
	std::vector<int> parent(nodeCount);
	std::iota(parent.begin(), parent.end(), 0);
	auto root = [&](int node)
	{
		int value = node;
		while (parent[value] != value) value = parent[value];
		while (parent[node] != node)
		{
			const int next = parent[node];
			parent[node] = value;
			node = next;
		}
		return value;
	};
	for (const auto& edge : edges)
	{
		if (!std::isfinite(edge.perr) || edge.perr > maximumPerr ||
			edge.firstNode < 0 || edge.secondNode < 0 ||
			edge.firstNode >= nodeCount || edge.secondNode >= nodeCount)
		{
			continue;
		}
		int first = root(edge.firstNode);
		int second = root(edge.secondNode);
		if (first == second) continue;
		parent[second] = first;
		forest.push_back(edge);
	}
	return forest;
}

inline ZhangInheritedNamedCertificate
zhangPromoteNamedCertificateFromAcceptedParent(
	const ZhangExactMatrix& parentFixedRows,
	const ZhangExactVector& parentFixedValues,
	int namedCount,
	bool parentStatisticallyAccepted)
{
	ZhangInheritedNamedCertificate result;
	result.parentFixedRank = static_cast<int>(parentFixedRows.size());
	if (!parentStatisticallyAccepted || namedCount <= 0 ||
		parentFixedRows.empty() ||
		parentFixedRows.size() != parentFixedValues.size())
	{
		return result;
	}
	result.values = zhangRecoverCertifiedNamedProductSubset(
		parentFixedRows, parentFixedValues, namedCount);
	result.exact = !result.values.empty();
	return result;
}

/** Primitive satellite-difference basis that removes one component-common
 * real gauge.  Different anchors are coordinate choices only: their exact row
 * HNFs must agree. */
inline ZhangExactMatrix zhangComponentRelativeGaugeBasis(
	int satelliteCount, int anchorIndex = 0)
{
	ZhangExactMatrix result;
	if (satelliteCount < 2 || anchorIndex < 0 ||
		anchorIndex >= satelliteCount)
	{
		return result;
	}
	for (int satellite = 0; satellite < satelliteCount; satellite++)
	{
		if (satellite == anchorIndex) continue;
		ZhangExactVector row(satelliteCount);
		row[satellite] = 1;
		row[anchorIndex] = -1;
		result.push_back(std::move(row));
	}
	return result;
}

/** Generate one bounded outer named-PAR child without enumerating all n
 * deletions.  An exact recoverable mixed-LAMBDA seed is preferred; otherwise
 * one deterministic backward-elimination child is returned.  Starting from a
 * single full branch therefore reaches minimumRank in at most fullRank steps. */
inline std::vector<std::vector<int>> zhangProductNamedBackwardChildren(
	const std::vector<int>& selected,
	const std::vector<int>& recoverableLocalIndices,
	int worstNamedLocalPosition,
	int minimumRank)
{
	std::vector<std::vector<int>> children;
	if (static_cast<int>(selected.size()) <= minimumRank)
	{
		return children;
	}
	if (!recoverableLocalIndices.empty() &&
		recoverableLocalIndices.size() < selected.size())
	{
		std::vector<int> exactSeed;
		for (int local : recoverableLocalIndices)
		{
			if (local >= 0 && local < static_cast<int>(selected.size()))
			{
				exactSeed.push_back(selected.at(local));
			}
		}
		std::sort(exactSeed.begin(), exactSeed.end());
		exactSeed.erase(std::unique(exactSeed.begin(), exactSeed.end()),
			exactSeed.end());
		if (static_cast<int>(exactSeed.size()) >= minimumRank)
		{
			children.push_back(std::move(exactSeed));
			return children;
		}
	}
	if (worstNamedLocalPosition >= 0 &&
		worstNamedLocalPosition < static_cast<int>(selected.size()))
	{
		auto child = selected;
		child.erase(child.begin() + worstNamedLocalPosition);
		if (static_cast<int>(child.size()) >= minimumRank)
		{
			children.push_back(std::move(child));
		}
	}
	return children;
}

/** Identity-weighted product covariance gain from exact constraints Bq=k.
 *
 * With q=Lx and candidate rows A_S x, exact fixing gives
 *   P_fix = P - P A_S' (A_S P A_S')^-1 A_S P,
 *   Delta Q_q = L P A_S' (A_S P A_S')^-1 A_S P L'.
 * A general product weighting would use
 *   g(S)=tr(W_q Delta Q_q)/tr(W_q Q_q).
 * Here q is already the joint named product coordinate and B maps q to the
 * candidate WL/L1 relations.  The returned scalar is
 *   tr(Delta Q_q) / tr(Q_q).
 * No user geometry is introduced in this first implementation. */
inline double zhangNamedProductInformationGain(
	const MatrixXd& productCovariance,
	const ZhangIarFunctional& candidateRows)
{
	const double denominator = productCovariance.trace();
	const ZhangIarCovarianceCondition condition =
		zhangIarCovarianceCondition(productCovariance, candidateRows);
	if (!condition.valid || !(denominator > 0) ||
		!std::isfinite(denominator))
	{
		return 0;
	}
	const double reduction = condition.reductionFactor.squaredNorm();
	return std::clamp(reduction / denominator, 0.0, 1.0);
}

/** Separate a rank ceiling problem from an integer candidate alignment
 * problem.  The thresholds only classify diagnostics; they never authorize
 * fixing or alter the beam. */
inline std::string zhangProductGainSpectrumDiagnosis(
	double realSubspaceUpperBound,
	double namedIntegerSubsetGain,
	double desiredCoverage = 0.80,
	double minimumEfficiency = 0.25)
{
	if (!std::isfinite(realSubspaceUpperBound) ||
		!std::isfinite(namedIntegerSubsetGain) ||
		realSubspaceUpperBound < 0 || namedIntegerSubsetGain < 0)
	{
		return "GAIN_COMPARISON_INVALID";
	}
	if (realSubspaceUpperBound < desiredCoverage)
	{
		return "REAL_RANK_CEILING_LOW_INCREASE_RANK";
	}
	const double efficiency = realSubspaceUpperBound > 0
		? namedIntegerSubsetGain / realSubspaceUpperBound : 0;
	if (efficiency < minimumEfficiency)
	{
		return "INTEGER_CANDIDATE_SUBSPACE_MISALIGNED";
	}
	return "INTEGER_SUBSET_USES_REAL_CEILING_EFFICIENTLY";
}
