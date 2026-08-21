#pragma once

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "common/zhangIntegerAudit.hpp"

/** One named satellite-minus-reference product relation, independently
 * expanded from current fundamental cycles to physical ambiguity arcs.
 *
 * The representation deliberately contains no ZhangProductIntegerFunctional:
 * it is structural evidence derived from the graph/S-system itself, not a
 * self-attestation by the product coordinate that will later consume it.
 */
struct ZhangProductRelationRow
{
    SatSys                                  satellite;
    SatSys                                  referenceSatellite;
    ZhangExactVector                        currentCycleCoefficients;
    std::map<ZhangGraphEdge, ZhangExactInteger> physicalArcCoefficients;
    ZhangExactVector                        nuisanceCoefficients;
};

struct ZhangProductRelationBasis
{
    E_Sys                                   system = E_Sys::NONE;
    E_ObsCode                               observable = E_ObsCode::NONE;
    SatSys                                  referenceSatellite;
    std::vector<SatSys>                     satellites;
    std::vector<ZhangGraphEdge>             currentChords;
    std::vector<ZhangProductRelationRow>     namedRelations;
    std::vector<int>                        independentNamedIndices;
    std::vector<int>                        mappableNamedIndices;
    std::vector<ZhangGraphEdge>             physicalArcColumns;
    ZhangExactMatrix                        networkIntegerBasis;
    ZhangExactMatrix                        exactRowBasis;
    ZhangExactMatrix                        networkContainmentTransform;
    MatrixXd                                transform;
    ZhangExactVector                        affineOffsets;
    int                                     fullTargetRank = 0;
    int                                     mappableTargetRank = 0;
    int                                     primitiveRank = 0;
    int                                     unmappableTargetRank = 0;
    int                                     namedRelationCount = 0;
    int                                     exactRank = 0;
    ZhangExactInteger                       saturationIndex = 0;
    std::string                             exactHnf;
    bool                                    primitive = false;
    bool                                    admissibleCompletionProven = false;
    bool                                    networkLatticeContained = false;
    bool                                    networkClosureExactZero = false;
    bool                                    temporalRecoveryRequired = false;
    bool                                    nuisanceOrthogonal = false;
    bool                                    physicalExpansionValid = false;
    bool                                    valid = false;
    std::string                             failureReason;
};

/** Build the product-relevant satellite integer lattice without estimator or
 * ProductRelationManager state.
 *
 * Each named row is first obtained from the exact current-graph/product-tree
 * incidence map.  It is then independently expanded through the current
 * fundamental cycles to original receiver-satellite ambiguity arcs.  The
 * nuisance block is explicit and identically zero.  Exact row HNF and Smith
 * invariants prove rank and primitivity; any non-primitive or malformed
 * result fails closed instead of being silently treated as an integer basis.
 */
struct ProductRelationBasisBuilder
{
    static ZhangProductRelationBasis build(
        const ZhangGraphBasis& currentBasis,
        const ZhangGraphBasis& productBasis,
        const SatSys& requestedReference = SatSys(),
        E_Sys system = E_Sys::NONE,
        E_ObsCode observable = E_ObsCode::NONE)
    {
        ZhangProductRelationBasis result;
        result.system = system;
        result.observable = observable;
        const ZhangSatelliteProductTarget target =
            zhangBuildSatelliteProductTarget(
                currentBasis, productBasis, requestedReference);
        if (!target.valid)
        {
            result.failureReason = target.failureReason;
            return result;
        }

        result.referenceSatellite = target.referenceSatellite;
        result.satellites = target.targetSatellites;
        result.satellites.push_back(target.referenceSatellite);
        std::sort(result.satellites.begin(), result.satellites.end());
        result.satellites.erase(
            std::unique(result.satellites.begin(), result.satellites.end()),
            result.satellites.end());
        result.currentChords = target.currentChords;
        result.namedRelationCount = target.matrix.size();
        result.fullTargetRank = zhangExactRowHermiteNormalForm(
            target.matrix).basis.size();
        if (target.matrix.size() != target.targetSatellites.size() ||
            target.matrix.empty())
        {
            result.failureReason = "PRODUCT_RELATION_TARGET_DIMENSION_MISMATCH";
            return result;
        }

        std::map<std::string, std::size_t> receiverIndex;
        std::map<SatSys, std::size_t> satelliteIndex;
        std::size_t nuisanceIndex = 0;
        for (const auto& receiver : currentBasis.receivers)
        {
            receiverIndex[receiver] = nuisanceIndex++;
        }
        for (const auto& satellite : currentBasis.satellites)
        {
            satelliteIndex[satellite] = nuisanceIndex++;
        }
        result.physicalExpansionValid = true;
        result.nuisanceOrthogonal = true;
        for (std::size_t row = 0; row < target.matrix.size(); row++)
        {
            if (target.matrix[row].size() != target.currentChords.size())
            {
                result.failureReason =
                    "PRODUCT_RELATION_CYCLE_DIMENSION_MISMATCH";
                result.physicalExpansionValid = false;
                return result;
            }
            ZhangProductRelationRow relation;
            relation.satellite = target.targetSatellites[row];
            relation.referenceSatellite = target.referenceSatellite;
            relation.currentCycleCoefficients = target.matrix[row];
            relation.nuisanceCoefficients = ZhangExactVector(nuisanceIndex);

            for (std::size_t chord = 0;
                 chord < target.currentChords.size(); chord++)
            {
                const ZhangExactInteger multiplier = target.matrix[row][chord];
                if (multiplier == 0)
                {
                    continue;
                }
                const auto cycle = zhangFundamentalCycle(
                    currentBasis, target.currentChords[chord]);
                if (cycle.empty())
                {
                    result.failureReason =
                        "PRODUCT_RELATION_PHYSICAL_CYCLE_EXPANSION_FAILED";
                    result.physicalExpansionValid = false;
                    return result;
                }
                for (const auto& [edge, coefficient] : cycle)
                {
                    relation.physicalArcCoefficients[edge] +=
                        multiplier * coefficient;
                }
            }
            for (auto iterator = relation.physicalArcCoefficients.begin();
                 iterator != relation.physicalArcCoefficients.end();)
            {
                if (iterator->second == 0)
                {
                    iterator = relation.physicalArcCoefficients.erase(iterator);
                }
                else
                {
                    const auto& edge = iterator->first;
                    const auto& coefficient = iterator->second;
                    // A physical ambiguity arc carries one additive receiver
                    // and one additive satellite nuisance datum.  Exact cycle
                    // relations must annihilate both incidences.  Compute the
                    // coefficients from the expanded physical row instead of
                    // declaring them zero by construction.
                    relation.nuisanceCoefficients[
                        receiverIndex.at(edge.receiver)] += coefficient;
                    relation.nuisanceCoefficients[
                        satelliteIndex.at(edge.satellite)] += coefficient;
                    ++iterator;
                }
            }
            result.nuisanceOrthogonal &= std::all_of(
                relation.nuisanceCoefficients.begin(),
                relation.nuisanceCoefficients.end(),
                [](const auto& coefficient) { return coefficient == 0; });
            result.namedRelations.push_back(std::move(relation));
        }

        // Use the complete current physical ambiguity ambient space.  This
        // makes the canonical HNF comparable across product reference choices
        // and provides a common coordinate for H_P = U D_N^T.
        result.physicalArcColumns.assign(
            currentBasis.edges.begin(), currentBasis.edges.end());
        std::map<ZhangGraphEdge, std::size_t> physicalColumnIndex;
        for (std::size_t column = 0;
             column < result.physicalArcColumns.size(); column++)
        {
            physicalColumnIndex[result.physicalArcColumns[column]] = column;
        }
        ZhangExactMatrix physicalRows;
        for (const auto& relation : result.namedRelations)
        {
            ZhangExactVector row(result.physicalArcColumns.size());
            for (const auto& [edge, coefficient] :
                 relation.physicalArcCoefficients)
            {
                row[physicalColumnIndex.at(edge)] = coefficient;
            }
            physicalRows.push_back(std::move(row));
        }

        // D_N^T: the already legal network integer-estimable basis, expressed
        // as exact fundamental-cycle rows in the same physical-arc ambient
        // coordinate as H_P.
        for (const auto& chord : target.currentChords)
        {
            ZhangExactVector row(result.physicalArcColumns.size());
            const auto cycle = zhangFundamentalCycle(currentBasis, chord);
            if (cycle.empty())
            {
                result.failureReason =
                    "NETWORK_INTEGER_BASIS_CYCLE_EXPANSION_FAILED";
                return result;
            }
            for (const auto& [edge, coefficient] : cycle)
            {
                auto column = physicalColumnIndex.find(edge);
                if (column == physicalColumnIndex.end())
                {
                    result.failureReason =
                        "NETWORK_INTEGER_BASIS_ARC_OUTSIDE_AMBIENT_SPACE";
                    return result;
                }
                row[column->second] = coefficient;
            }
            result.networkIntegerBasis.push_back(std::move(row));
        }
        if (!result.nuisanceOrthogonal)
        {
            result.failureReason =
                "PRODUCT_RELATION_REAL_NUISANCE_NOT_ANNIHILATED";
            return result;
        }

        ZhangExactRowHnf hnf = zhangExactRowHermiteNormalForm(physicalRows);
        if (!hnf.consistent)
        {
            result.failureReason = "PRODUCT_RELATION_HNF_FAILED";
            return result;
        }
        result.exactRowBasis = hnf.basis;
        result.exactRank = hnf.basis.size();
        result.exactHnf = zhangExactMatrixFingerprint(result.exactRowBasis);

        // Machine closure of H_P = U D_N^T.  Membership returns each exact
        // integer coefficient row of U; equality below is cpp_int exact zero,
        // never a floating tolerance check.
        result.networkLatticeContained = true;
        for (const auto& productRow : result.exactRowBasis)
        {
            const ZhangIntegerLatticeMembership membership =
                zhangIntegerRowLatticeContains(
                    result.networkIntegerBasis, productRow);
            if (!membership.contained ||
                membership.combination.size() !=
                    result.networkIntegerBasis.size())
            {
                result.networkLatticeContained = false;
                break;
            }
            result.networkContainmentTransform.push_back(
                membership.combination);
        }
        result.networkClosureExactZero = result.networkLatticeContained &&
            zhangExactMultiply(
                result.networkContainmentTransform,
                result.networkIntegerBasis) == result.exactRowBasis;
        if (!result.networkClosureExactZero)
        {
            result.failureReason =
                "PRODUCT_RELATION_NOT_IN_NETWORK_INTEGER_LATTICE";
            return result;
        }
        ZhangExactMatrix independentRows;
        int independentRank = 0;
        for (int row = 0; row < static_cast<int>(physicalRows.size()); row++)
        {
            ZhangExactMatrix candidate = independentRows;
            candidate.push_back(physicalRows[row]);
            const int candidateRank = zhangExactRowHermiteNormalForm(
                candidate).basis.size();
            if (candidateRank > independentRank)
            {
                result.independentNamedIndices.push_back(row);
                independentRows.push_back(physicalRows[row]);
                independentRank = candidateRank;
            }
        }
        if (independentRank != result.exactRank)
        {
            result.failureReason =
                "PRODUCT_RELATION_INDEPENDENT_NAMED_RANK_MISMATCH";
            return result;
        }
        const ZhangIntegerLatticeMembership smith =
            zhangIntegerRowLatticeContains(
                physicalRows,
                ZhangExactVector(result.physicalArcColumns.size()));
        if (smith.rank != result.exactRank)
        {
            result.failureReason = "PRODUCT_RELATION_HNF_SNF_RANK_MISMATCH";
            return result;
        }
        result.saturationIndex = 1;
        result.primitive = true;
        for (const auto& invariant : smith.smithInvariants)
        {
            const ZhangExactInteger magnitude = zhangExactAbs(invariant);
            result.saturationIndex *= magnitude;
            result.primitive &= magnitude == 1;
        }
        if (!result.primitive)
        {
            result.failureReason =
                "PRODUCT_RELATION_EXACT_LATTICE_NOT_PRIMITIVE";
            return result;
        }
        result.primitiveRank = result.primitive ? result.exactRank : 0;
        // A saturated (index-one) primitive sublattice of Z^E is a direct
        // summand, hence it admits an integer unimodular completion.
        result.admissibleCompletionProven = result.primitive &&
            result.saturationIndex == 1 &&
            result.primitiveRank == result.exactRank;
        result.mappableNamedIndices = result.independentNamedIndices;
        result.mappableTargetRank = result.exactRank;
        result.unmappableTargetRank =
            result.fullTargetRank - result.mappableTargetRank;
        result.temporalRecoveryRequired = result.unmappableTargetRank > 0;
        result.affineOffsets = ZhangExactVector(result.exactRank);
        result.valid = result.exactRank > 0 &&
            result.physicalExpansionValid && result.nuisanceOrthogonal &&
            result.networkClosureExactZero &&
            result.admissibleCompletionProven;
        if (!result.valid && result.failureReason.empty())
        {
            result.failureReason = "EMPTY_PRODUCT_RELATION_EXACT_LATTICE";
        }
        return result;
    }
};
