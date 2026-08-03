// #pragma GCC optimize ("O0")
/**------------------------------------------------------------------------------
 * reference :
 *     [1] P.J.G.Teunissen, The least-square ambiguity decorrelation adjustment:
 *         a method for fast GPS ambiguity estimation, J.Geodesy, Vol.70, 65-82,
 *         1995
 *     [2] X.-W.Chang, X.Yang, T.Zhou, MLAMBDA: A modified LAMBDA method for
 *         integer least-squares estimation, J.Geodesy, Vol.79, 552-565, 2005
 *-----------------------------------------------------------------------------*/

#include <algorithm>
#include <iostream>
#include <iomanip>
#include <limits>
#include <math.h>
#include <optional>
#include <set>
#include <sstream>
#include "ambres/GNSSambres.hpp"
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/biases.hpp"
#include "common/common.hpp"
#include "common/eigenIncluder.hpp"
#include "common/phaseClockOsb.hpp"
#include "common/trace.hpp"
#include "common/zhangFullRank.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "pea/zhangReference.hpp"
#include "pea/zhangPppAr.hpp"

static bool filterError = false;

static bool useAmbiguityForPhaseClockOsb(const KFKey& key)
{
    auto& controller = acsConfig.phaseClockOsb;
    if (controller.enable == false || controller.baseline_only_ambiguity_resolution == false)
    {
        return true;
    }

    auto sysIt = controller.sysOpts.find(key.Sat.sys);
    if (sysIt == controller.sysOpts.end() ||
        sysIt->second.baseline_phase_observables.size() != 2)
    {
        return false;
    }

    E_ObsCode code = int_to_enum<E_ObsCode>(key.num);
    for (E_ObsCode baselineCode : sysIt->second.baseline_phase_observables)
    {
        if (code == baselineCode)
        {
            return true;
        }
    }

    return false;
}

static bool useAmbiguityForZhang(const KFState& kfState, const KFKey& key)
{
    if (acsConfig.zhangPppAr.user_adapter)
    {
        return zhangPppArUsesObservable(
                   key.Sat.sys,
                   static_cast<E_ObsCode>(key.num)
               ) &&
               zhangPppArUserAmbiguityIntegerValid(
                   kfState,
                   key.str,
                   key.Sat,
                   static_cast<E_ObsCode>(key.num)
               );
    }

    if (!acsConfig.zhangFullRank.enable)
    {
        return true;
    }

    auto optionsIt = acsConfig.zhangFullRank.sysOpts.find(key.Sat.sys);
    if (optionsIt == acsConfig.zhangFullRank.sysOpts.end())
    {
        return false;
    }

    E_ObsCode code = static_cast<E_ObsCode>(key.num);
    if (!zhangFullRankUsesObservable(code, optionsIt->second.baseline_observables))
    {
        return false;
    }

    if (!optionsIt->second.use_spanning_tree)
    {
        return true;
    }

    return zhangGraphRetainsAmbiguity(kfState, key.str, key.Sat, code);
}

/** A physical ambiguity arc survives a spanning-tree exchange unchanged.  The
 * arc version makes a cycle slip or local reinitialisation a different integer
 * coordinate even when receiver and satellite names are unchanged. */
struct ZhangPhysicalIntegerArc
{
    E_ObsCode      code = E_ObsCode::NONE;
    ZhangGraphEdge edge;
    int            version = 0;

    bool operator<(const ZhangPhysicalIntegerArc& other) const
    {
        return std::tie(code, edge, version) <
               std::tie(other.code, other.edge, other.version);
    }

    bool operator==(const ZhangPhysicalIntegerArc& other) const
    {
        return code == other.code && edge == other.edge &&
               version == other.version;
    }
};

struct ZhangPersistentHeldRow
{
    map<ZhangPhysicalIntegerArc, ZhangExactInteger> coefficients;
    ZhangExactInteger                                value = 0;
};

struct ZhangPersistentHeldLattice
{
    vector<ZhangPersistentHeldRow> rows;
    int                            lastEventId = 0;
    bool                           consistent = true;
};

static map<pair<const KFState*, E_Sys>, ZhangPersistentHeldLattice>
    zhangPersistentHeldLattices;

static void removeZeroPhysicalCoefficients(
    map<ZhangPhysicalIntegerArc, ZhangExactInteger>& coefficients
)
{
    for (auto it = coefficients.begin(); it != coefficients.end();)
    {
        if (it->second == 0)
        {
            it = coefficients.erase(it);
        }
        else
        {
            ++it;
        }
    }
}

static void normalisePersistentHeldLattice(
    ZhangPersistentHeldLattice& lattice
)
{
    set<ZhangPhysicalIntegerArc> columnSet;
    for (const auto& row : lattice.rows)
    {
        for (const auto& [arc, coefficient] : row.coefficients)
        {
            if (coefficient != 0)
            {
                columnSet.insert(arc);
            }
        }
    }
    vector<ZhangPhysicalIntegerArc> columns(
        columnSet.begin(),
        columnSet.end()
    );
    map<ZhangPhysicalIntegerArc, int> columnIndex;
    for (int index = 0; index < static_cast<int>(columns.size()); index++)
    {
        columnIndex[columns[index]] = index;
    }

    ZhangExactMatrix denseRows;
    ZhangExactVector values;
    for (const auto& row : lattice.rows)
    {
        ZhangExactVector dense(columns.size());
        for (const auto& [arc, coefficient] : row.coefficients)
        {
            dense[columnIndex.at(arc)] = coefficient;
        }
        denseRows.push_back(std::move(dense));
        values.push_back(row.value);
    }

    ZhangExactRowHnf hnf = zhangExactRowHermiteNormalForm(
        std::move(denseRows),
        std::move(values)
    );
    lattice.consistent = hnf.consistent;
    lattice.rows.clear();
    for (int row = 0; row < static_cast<int>(hnf.basis.size()); row++)
    {
        ZhangPersistentHeldRow held;
        held.value = hnf.values[row];
        for (int column = 0; column < static_cast<int>(columns.size()); column++)
        {
            if (hnf.basis[row][column] != 0)
            {
                held.coefficients[columns[column]] = hnf.basis[row][column];
            }
        }
        lattice.rows.push_back(std::move(held));
    }
}

static bool addCurrentCycleToPhysicalRow(
    const ZhangGraphIntegerContext& context,
    E_ObsCode                       code,
    const ZhangGraphEdge&           chord,
    const ZhangExactInteger&        multiplier,
    map<ZhangPhysicalIntegerArc, ZhangExactInteger>& row
)
{
    if (context.basis.edges.find(chord) == context.basis.edges.end() ||
        context.basis.isTreeEdge(chord.receiver, chord.satellite))
    {
        return false;
    }
    auto cycle = zhangFundamentalCycle(context.basis, chord);
    if (cycle.empty())
    {
        return false;
    }
    for (const auto& [edge, coefficient] : cycle)
    {
        auto version = context.arcVersions.find(edge);
        if (version == context.arcVersions.end())
        {
            return false;
        }
        row[{code, edge, version->second}] += multiplier * coefficient;
    }
    removeZeroPhysicalCoefficients(row);
    return true;
}

static pair<int, int> appendPersistentHeldRows(
    Trace&          trace,
    const KFState&  kfState,
    const GinAR_mtx& fixed
)
{
    int added = 0;
    int rejected = 0;
    map<E_Sys, ZhangGraphIntegerContext> contexts;

    for (int row = 0; row < fixed.Ztrs.rows(); row++)
    {
        std::optional<E_Sys> targetSystem;
        ZhangPersistentHeldRow physical;
        bool valid = true;
        for (int column = 0; column < fixed.Ztrs.cols(); column++)
        {
            double coefficient = fixed.Ztrs(row, column);
            if (std::abs(coefficient) < 1e-10)
            {
                continue;
            }
            long long rounded = std::llround(coefficient);
            if (std::abs(coefficient - rounded) > 1e-8)
            {
                valid = false;
                break;
            }

            const KFKey& key = fixed.ambmap.at(column);
            if (!targetSystem)
            {
                targetSystem = key.Sat.sys;
            }
            if (*targetSystem != key.Sat.sys)
            {
                valid = false;
                break;
            }

            auto context = contexts.find(*targetSystem);
            if (context == contexts.end())
            {
                ZhangGraphIntegerContext snapshot;
                if (!zhangGraphIntegerContext(kfState, *targetSystem, snapshot))
                {
                    valid = false;
                    break;
                }
                context = contexts.emplace(*targetSystem, std::move(snapshot)).first;
            }
            if (!addCurrentCycleToPhysicalRow(
                    context->second,
                    static_cast<E_ObsCode>(key.num),
                    {key.str, key.Sat},
                    ZhangExactInteger(rounded),
                    physical.coefficients
                ))
            {
                valid = false;
                break;
            }
        }

        long long roundedValue = std::llround(fixed.zfix(row));
        valid &= std::abs(fixed.zfix(row) - roundedValue) <= 1e-8;
        valid &= targetSystem.has_value() && !physical.coefficients.empty();
        if (!valid)
        {
            rejected++;
            continue;
        }

        physical.value = roundedValue;
        auto& lattice = zhangPersistentHeldLattices[{&kfState, *targetSystem}];
        lattice.rows.push_back(std::move(physical));
        added++;
    }

    for (auto& [system, context] : contexts)
    {
        auto& lattice = zhangPersistentHeldLattices[{&kfState, system}];
        normalisePersistentHeldLattice(lattice);
        lattice.lastEventId = context.eventId;
        trace << "\nZHANG_HELD_LATTICE_NORMALISE time="
              << kfState.time.to_string(0)
              << " system=" << enum_to_string(system)
              << " added_rows=" << added
              << " rejected_rows=" << rejected
              << " hnf_rows=" << lattice.rows.size()
              << " consistent=" << lattice.consistent
              << " coordinate=PHYSICAL_ARC_VERSION";
    }
    return {added, rejected};
}

/** Project invariant physical-cycle rows into the current fundamental-cycle
 * coordinates.  Superseded arcs are eliminated by the exact integer left
 * kernel of H_R, preserving every old held-lattice combination that lives
 * entirely in the surviving physical-arc subspace. */
static vector<GinAR_mtx> projectPersistentHeldRows(
    Trace&         trace,
    const KFState& kfState
)
{
    vector<GinAR_mtx> projectedSets;
    for (auto& [identity, lattice] : zhangPersistentHeldLattices)
    {
        if (identity.first != &kfState || lattice.rows.empty())
        {
            continue;
        }
        E_Sys system = identity.second;
        ZhangGraphIntegerContext context;
        if (!zhangGraphIntegerContext(kfState, system, context))
        {
            continue;
        }

        vector<KFKey> chordKeys;
        for (const auto& [key, index] : kfState.kfIndexMap)
        {
            if (key.type != KF::AMBIGUITY || key.Sat.sys != system)
            {
                continue;
            }
            E_ObsCode code = static_cast<E_ObsCode>(key.num);
            if (!zhangPppArUsesObservable(system, code))
            {
                continue;
            }
            ZhangGraphEdge edge{key.str, key.Sat};
            if (context.basis.edges.find(edge) != context.basis.edges.end() &&
                !context.basis.isTreeEdge(edge.receiver, edge.satellite))
            {
                chordKeys.push_back(key);
            }
        }

        set<ZhangPhysicalIntegerArc> physicalColumnSet;
        for (const auto& held : lattice.rows)
        {
            for (const auto& [arc, coefficient] : held.coefficients)
            {
                if (coefficient != 0)
                {
                    physicalColumnSet.insert(arc);
                }
            }
        }
        vector<ZhangPhysicalIntegerArc> physicalColumns(
            physicalColumnSet.begin(), physicalColumnSet.end()
        );
        map<ZhangPhysicalIntegerArc, int> physicalColumnIndex;
        vector<bool> survivingMask(physicalColumns.size(), false);
        vector<ZhangPhysicalIntegerArc> removedArcs;
        for (int column = 0;
             column < static_cast<int>(physicalColumns.size());
             column++)
        {
            const auto& arc = physicalColumns[column];
            physicalColumnIndex[arc] = column;
            auto version = context.arcVersions.find(arc.edge);
            survivingMask[column] =
                arc.edge.satellite.sys == system &&
                context.basis.edges.find(arc.edge) != context.basis.edges.end() &&
                version != context.arcVersions.end() &&
                version->second == arc.version;
            if (!survivingMask[column])
            {
                removedArcs.push_back(arc);
            }
        }

        ZhangExactMatrix oldRows = zhangExactZeroMatrix(
            lattice.rows.size(), physicalColumns.size()
        );
        ZhangExactVector oldValues;
        for (int row = 0; row < static_cast<int>(lattice.rows.size()); row++)
        {
            for (const auto& [arc, coefficient] : lattice.rows[row].coefficients)
            {
                oldRows[row][physicalColumnIndex.at(arc)] = coefficient;
            }
            oldValues.push_back(lattice.rows[row].value);
        }

        // Preserve the old delete-touched-rows result as a diagnostic control.
        vector<std::size_t> survivingColumnIndices;
        for (std::size_t column = 0; column < survivingMask.size(); column++)
        {
            if (survivingMask[column])
            {
                survivingColumnIndices.push_back(column);
            }
        }
        ZhangExactMatrix deleteRows;
        ZhangExactVector deleteValues;
        for (int row = 0; row < static_cast<int>(oldRows.size()); row++)
        {
            bool touchesRemoved = false;
            for (std::size_t column = 0; column < survivingMask.size(); column++)
            {
                touchesRemoved |= !survivingMask[column] && oldRows[row][column] != 0;
            }
            if (touchesRemoved)
            {
                continue;
            }
            ZhangExactVector survivingRow(survivingColumnIndices.size());
            for (int column = 0;
                 column < static_cast<int>(survivingColumnIndices.size());
                 column++)
            {
                survivingRow[column] = oldRows[row][survivingColumnIndices[column]];
            }
            deleteRows.push_back(std::move(survivingRow));
            deleteValues.push_back(oldValues[row]);
        }
        ZhangExactRowHnf deleteHnf = zhangExactRowHermiteNormalForm(
            std::move(deleteRows), std::move(deleteValues)
        );

        ZhangExactSurvivingLattice exactSurviving =
            zhangExactSurvivingLattice(oldRows, oldValues, survivingMask);
        vector<ZhangPersistentHeldRow> survivingHeldRows;
        for (int row = 0;
             row < static_cast<int>(exactSurviving.basis.size());
             row++)
        {
            ZhangPersistentHeldRow held;
            held.value = exactSurviving.values[row];
            for (int column = 0;
                 column < static_cast<int>(survivingColumnIndices.size());
                 column++)
            {
                ZhangExactInteger coefficient =
                    exactSurviving.basis[row][column];
                if (coefficient != 0)
                {
                    held.coefficients[
                        physicalColumns[survivingColumnIndices[column]]
                    ] = coefficient;
                }
            }
            survivingHeldRows.push_back(std::move(held));
        }

        const int rankBefore = lattice.rows.size();
        lattice.rows = std::move(survivingHeldRows);
        lattice.consistent = exactSurviving.consistent;

        vector<ZhangExactVector>       projectedRows;
        vector<ZhangExactInteger>      projectedValues;
        for (const auto& held : lattice.rows)
        {
            bool current = true;
            ZhangExactVector coordinates(chordKeys.size());
            map<ZhangPhysicalIntegerArc, ZhangExactInteger> reconstructed;
            for (int column = 0; column < static_cast<int>(chordKeys.size()); column++)
            {
                const KFKey& key = chordKeys[column];
                ZhangGraphEdge edge{key.str, key.Sat};
                int version = context.arcVersions.at(edge);
                ZhangPhysicalIntegerArc chord{
                    static_cast<E_ObsCode>(key.num), edge, version
                };
                auto coefficient = held.coefficients.find(chord);
                if (coefficient != held.coefficients.end())
                {
                    coordinates[column] = coefficient->second;
                    if (!addCurrentCycleToPhysicalRow(
                            context,
                            chord.code,
                            chord.edge,
                            coefficient->second,
                            reconstructed
                        ))
                    {
                        current = false;
                        break;
                    }
                }
            }
            removeZeroPhysicalCoefficients(reconstructed);
            if (!current || reconstructed != held.coefficients)
            {
                lattice.consistent = false;
                continue;
            }
            projectedRows.push_back(std::move(coordinates));
            projectedValues.push_back(held.value);
        }

        if (context.eventId != lattice.lastEventId || !removedArcs.empty())
        {
            std::ostringstream removedArcIds;
            for (int index = 0; index < static_cast<int>(removedArcs.size()); index++)
            {
                const auto& arc = removedArcs[index];
                removedArcIds << (index ? "," : "")
                              << enum_to_string(arc.code) << ":"
                              << arc.edge.receiver << ":"
                              << arc.edge.satellite.id() << ":A"
                              << arc.version;
            }
            trace << "\nZHANG_HELD_LATTICE_EVENT time="
                  << kfState.time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " event_id=" << context.eventId
                  << " held_rank_before=" << rankBefore
                  << " held_rows_touched=" << exactSurviving.touchedRows
                  << " held_rows_removed="
                  << std::max(0, rankBefore - static_cast<int>(lattice.rows.size()))
                  << " held_rank_after=" << lattice.rows.size()
                  << " delete_touched_rows_rank=" << deleteHnf.basis.size()
                  << " exact_surviving_lattice_rank=" << lattice.rows.size()
                  << " surviving_integer_nullity="
                  << exactSurviving.combinationRank
                  << " removed_arc_count=" << removedArcs.size()
                  << " removed_arc_ids="
                  << (removedArcs.empty() ? "NONE" : removedArcIds.str())
                  << " exact_hnf=1"
                  << " consistent=" << lattice.consistent;
        }
        lattice.lastEventId = context.eventId;

        if (!lattice.consistent || projectedRows.empty())
        {
            trace << "\nZHANG_HELD_LATTICE_STATUS time="
                  << kfState.time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " event_id=" << context.eventId
                  << " exact_held_rank=" << lattice.rows.size()
                  << " reapplicable_rows=0"
                  << " consistent=" << lattice.consistent;
            continue;
        }
        GinAR_mtx projected;
        for (int column = 0; column < static_cast<int>(chordKeys.size()); column++)
        {
            projected.ambmap[column] = chordKeys[column];
        }
        projected.Ztrs = MatrixXd::Zero(projectedRows.size(), chordKeys.size());
        projected.zfix = VectorXd::Zero(projectedValues.size());
        for (int row = 0; row < static_cast<int>(projectedRows.size()); row++)
        {
            for (int column = 0; column < static_cast<int>(chordKeys.size()); column++)
            {
                projected.Ztrs(row, column) =
                    projectedRows[row][column].convert_to<double>();
            }
            projected.zfix(row) = projectedValues[row].convert_to<double>();
        }
        trace << "\nZHANG_HELD_LATTICE_STATUS time="
              << kfState.time.to_string(0)
              << " system=" << enum_to_string(system)
              << " event_id=" << context.eventId
              << " exact_held_rank=" << lattice.rows.size()
              << " reapplicable_rows=" << projected.zfix.size()
              << " consistent=" << lattice.consistent;
        projectedSets.push_back(std::move(projected));
    }
    return projectedSets;
}

static pair<int, double> zhangHeldRankFromCovariance(
    const MatrixXd& covariance
)
{
    int heldIntegerRank = 0;
    double heldMinEigenvalue = std::numeric_limits<double>::quiet_NaN();
    if (covariance.rows() == 0 || covariance.rows() != covariance.cols())
    {
        return {heldIntegerRank, heldMinEigenvalue};
    }

    Eigen::SelfAdjointEigenSolver<MatrixXd> postEigenSolver(covariance);
    if (postEigenSolver.info() != Eigen::Success)
    {
        return {heldIntegerRank, heldMinEigenvalue};
    }

    heldMinEigenvalue = postEigenSolver.eigenvalues().minCoeff();
    double largest =
        std::max(1.0, postEigenSolver.eigenvalues().maxCoeff());
    double heldThreshold =
        std::max(100 * FIXED_AMB_VAR, 1e-10 * largest);
    heldIntegerRank =
        (postEigenSolver.eigenvalues().array() <= heldThreshold).count();

    return {heldIntegerRank, heldMinEigenvalue};
}

static pair<int, double> zhangHeldIntegerRank(
    const KFState&   kfState,
    const GinAR_mtx& ambiguityResolution,
    std::optional<E_ObsCode> observable = std::nullopt,
    std::optional<E_Sys>     system = std::nullopt
)
{
    vector<int> postIndices;
    for (const auto& [ambiguityIndex, key] : ambiguityResolution.ambmap)
    {
        if ((observable && key.num != static_cast<int>(*observable)) ||
            (system && key.Sat.sys != *system))
        {
            continue;
        }

        auto postIt = kfState.kfIndexMap.find(key);
        if (postIt == kfState.kfIndexMap.end())
        {
            return {0, std::numeric_limits<double>::quiet_NaN()};
        }
        postIndices.push_back(postIt->second);
    }

    if (postIndices.empty())
    {
        return {0, std::numeric_limits<double>::quiet_NaN()};
    }
    return zhangHeldRankFromCovariance(kfState.P(postIndices, postIndices));
}

struct ZhangWideLaneHeldRank
{
    int    pairs            = 0;
    int    firstCandidates  = 0;
    int    secondCandidates = 0;
    int    rank             = 0;
    double minEigenvalue = std::numeric_limits<double>::quiet_NaN();
};

struct ZhangSatelliteProductCoverage
{
    int requiredRank = 0;
    int targetRank = 0;
    int coveredRank = 0;
    int largestComponent = 0;
    int validSatelliteCount = 0;
    bool complete = false;
    string componentId = "NONE";
    string uncoveredSatellites = "NONE";
};

static ZhangWideLaneHeldRank zhangWideLaneHeldIntegerRank(
    const KFState&   kfState,
    const GinAR_mtx& ambiguityResolution,
    E_ObsCode        firstObservable,
    E_ObsCode        secondObservable,
    E_Sys            system
)
{
    ZhangWideLaneHeldRank result;
    map<pair<string, SatSys>, pair<int, int>> matchedIndices;
    for (const auto& [ambiguityIndex, key] : ambiguityResolution.ambmap)
    {
        if (key.Sat.sys != system)
        {
            continue;
        }
        auto postIt = kfState.kfIndexMap.find(key);
        if (postIt == kfState.kfIndexMap.end())
        {
            continue;
        }

        auto& pairIndices = matchedIndices[{key.str, key.Sat}];
        if (key.num == static_cast<int>(firstObservable))
        {
            result.firstCandidates++;
            pairIndices.first = postIt->second + 1;
        }
        if (key.num == static_cast<int>(secondObservable))
        {
            result.secondCandidates++;
            pairIndices.second = postIt->second + 1;
        }
    }

    vector<pair<int, int>> completePairs;
    for (const auto& [edge, indices] : matchedIndices)
    {
        if (indices.first > 0 && indices.second > 0)
        {
            completePairs.push_back({indices.first - 1, indices.second - 1});
        }
    }

    result.pairs = completePairs.size();
    if (completePairs.empty())
    {
        return result;
    }

    vector<int> postIndices;
    postIndices.reserve(2 * completePairs.size());
    for (const auto& [first, second] : completePairs)
    {
        postIndices.push_back(first);
        postIndices.push_back(second);
    }

    MatrixXd transform = MatrixXd::Zero(
        completePairs.size(),
        postIndices.size()
    );
    for (int row = 0; row < static_cast<int>(completePairs.size()); row++)
    {
        transform(row, 2 * row)     = +1;
        transform(row, 2 * row + 1) = -1;
    }
    MatrixXd covariance =
        transform * kfState.P(postIndices, postIndices) * transform.transpose();
    std::tie(result.rank, result.minEigenvalue) =
        zhangHeldRankFromCovariance(covariance);
    return result;
}

/** Evaluate the exact satellite-product integer lattice and promote every
 * nonzero HNF-proven satellite relation into the persistent product ledger.
 * A fixed-row count or real-valued rank alone is never accepted as proof.
 */
static void traceZhangSatelliteIntegerLattice(
    Trace&           trace,
    const KFState&   kfState,
    const GinAR_mtx& ambiguityResolution
)
{
    // This pass now promotes exact physical-HNF facts into the production
    // satellite datum ledger.  It must run whenever products are enabled;
    // product correctness may not depend on a diagnostic-output switch.
    if (!acsConfig.zhangPppAr.output_products)
    {
        return;
    }

    constexpr double integerTolerance = 1e-8;
    for (const auto& [sys, options] : acsConfig.zhangFullRank.sysOpts)
    {
        ZhangGraphIntegerContext graphContext;
        bool hasGraphContext = zhangGraphIntegerContext(
            kfState, sys, graphContext
        );
        ZhangSatelliteProductTarget productTarget;
        if (hasGraphContext)
        {
            productTarget = zhangBuildSatelliteProductTarget(
                graphContext.basis,
                graphContext.productBasis
            );
        }

        ZhangExactMatrix heldCycleRows;
        ZhangExactVector heldCycleValues;
        vector<pair<E_ObsCode, ZhangGraphEdge>> cycleColumns;
        if (productTarget.valid)
        {
            for (E_ObsCode code : options.baseline_observables)
            {
                for (const auto& chord : productTarget.currentChords)
                {
                    cycleColumns.push_back({code, chord});
                }
            }
            auto latticeIt = zhangPersistentHeldLattices.find({&kfState, sys});
            if (latticeIt != zhangPersistentHeldLattices.end() &&
                latticeIt->second.consistent)
            {
                for (const auto& held : latticeIt->second.rows)
                {
                    ZhangExactVector cycleRow(cycleColumns.size());
                    map<ZhangPhysicalIntegerArc, ZhangExactInteger> reconstructed;
                    bool representable = true;
                    for (int column = 0;
                         column < static_cast<int>(cycleColumns.size());
                         column++)
                    {
                        const auto& [code, chord] = cycleColumns[column];
                        auto version = graphContext.arcVersions.find(chord);
                        if (version == graphContext.arcVersions.end())
                        {
                            representable = false;
                            break;
                        }
                        ZhangPhysicalIntegerArc physicalChord{
                            code, chord, version->second
                        };
                        auto coefficient = held.coefficients.find(physicalChord);
                        if (coefficient == held.coefficients.end())
                        {
                            continue;
                        }
                        cycleRow[column] = coefficient->second;
                        representable &= addCurrentCycleToPhysicalRow(
                            graphContext,
                            code,
                            chord,
                            coefficient->second,
                            reconstructed
                        );
                    }
                    removeZeroPhysicalCoefficients(reconstructed);
                    if (representable && reconstructed == held.coefficients)
                    {
                        heldCycleRows.push_back(std::move(cycleRow));
                        heldCycleValues.push_back(held.value);
                    }
                }
            }
        }

        auto evaluateProductTargets = [&]
            (const string& label,
             const map<SatSys, ZhangExactVector>& rows,
             std::optional<E_ObsCode> productCode = std::nullopt)
        {
            ZhangSatelliteProductCoverage coverage;
            coverage.requiredRank = std::max(0, static_cast<int>(rows.size()) - 1);
            if (rows.empty())
            {
                return coverage;
            }

            map<SatSys, SatSys> parent;
            for (const auto& [satellite, row] : rows)
            {
                parent[satellite] = satellite;
            }
            auto findRoot = [&](SatSys satellite)
            {
                SatSys root = satellite;
                while (parent[root] != root)
                {
                    root = parent[root];
                }
                while (parent[satellite] != satellite)
                {
                    SatSys next = parent[satellite];
                    parent[satellite] = root;
                    satellite = next;
                }
                return root;
            };
            auto unite = [&](SatSys left, SatSys right)
            {
                SatSys leftRoot = findRoot(left);
                SatSys rightRoot = findRoot(right);
                if (leftRoot != rightRoot)
                {
                    parent[rightRoot] = leftRoot;
                }
            };

            ZhangExactMatrix coveredReferenceRows;
            ZhangExactMatrix referenceTargetRows;
            auto referenceIt = rows.find(productTarget.referenceSatellite);
            for (auto left = rows.begin(); left != rows.end(); left++)
            {
                auto right = left;
                for (++right; right != rows.end(); right++)
                {
                    ZhangExactVector target = left->second;
                    for (int column = 0;
                         column < static_cast<int>(target.size());
                         column++)
                    {
                        target[column] -= right->second[column];
                    }
                    ZhangIntegerLatticeMembership membership =
                        zhangIntegerRowLatticeContains(heldCycleRows, target);
                    if (!membership.contained)
                    {
                        continue;
                    }
                    unite(left->first, right->first);
                    ZhangExactInteger integerShift = 0;
                    for (int row = 0;
                         row < static_cast<int>(membership.combination.size());
                         row++)
                    {
                        integerShift += membership.combination[row] *
                                        heldCycleValues[row];
                    }
                    trace << "\nZHANG_SATELLITE_INTEGER_EDGE time="
                          << kfState.time.to_string(0)
                          << " system=" << enum_to_string(sys)
                          << " target=" << label
                          << " satellite_a=" << left->first.id()
                          << " satellite_b=" << right->first.id()
                          << " integer_shift=" << integerShift
                          << " product_datum_version="
                          << graphContext.productDatumVersion;
                    bool usesHeldInteger = std::any_of(
                        membership.combination.begin(),
                        membership.combination.end(),
                        [](const auto& coefficient) { return coefficient != 0; }
                    );
                    if (productCode && usesHeldInteger)
                    {
                        // The exact HNF membership and its evaluated integer
                        // value are now a satellite-only fact.  The source
                        // physical held rows may retire after this promotion.
                        promoteZhangSatelliteProductRelation(
                            kfState.time,
                            sys,
                            *productCode,
                            right->first,
                            left->first,
                            integerShift.convert_to<long long>(),
                            "physical_HNF_exact_membership"
                        );
                    }
                }
            }

            if (referenceIt != rows.end())
            {
                for (const auto& [satellite, row] : rows)
                {
                    if (satellite == productTarget.referenceSatellite)
                    {
                        continue;
                    }
                    ZhangExactVector target = row;
                    for (int column = 0;
                         column < static_cast<int>(target.size());
                         column++)
                    {
                        target[column] -= referenceIt->second[column];
                    }
                    referenceTargetRows.push_back(target);
                    if (zhangIntegerRowLatticeContains(
                            heldCycleRows, target
                        ).contained)
                    {
                        coveredReferenceRows.push_back(std::move(target));
                    }
                }
            }
            coverage.targetRank = zhangExactRowHermiteNormalForm(
                std::move(referenceTargetRows)
            ).basis.size();
            coverage.coveredRank = zhangExactRowHermiteNormalForm(
                coveredReferenceRows
            ).basis.size();

            map<SatSys, vector<SatSys>> components;
            for (const auto& [satellite, row] : rows)
            {
                components[findRoot(satellite)].push_back(satellite);
            }
            vector<SatSys> largest;
            for (const auto& [root, members] : components)
            {
                if (members.size() > largest.size())
                {
                    largest = members;
                }
            }
            coverage.largestComponent = largest.size();
            // A singleton is only an isolated graph vertex, not a strict
            // satellite relation component.
            coverage.validSatelliteCount =
                largest.size() >= 2 ? largest.size() : 0;
            coverage.complete = coverage.coveredRank == coverage.requiredRank;

            std::uint64_t hash = 1469598103934665603ULL;
            hash = zhangAuditFnv1a(
                hash,
                label + ":" +
                    std::to_string(graphContext.productDatumVersion) + ":"
            );
            for (const auto& satellite : largest)
            {
                hash = zhangAuditFnv1a(hash, satellite.id() + ";");
            }
            if (largest.size() >= 2)
            {
                std::ostringstream component;
                component << label << "-D" << graphContext.productDatumVersion
                          << "-" << std::hex << std::setw(16)
                          << std::setfill('0') << hash;
                coverage.componentId = component.str();
            }

            set<SatSys> largestSet;
            if (largest.size() >= 2)
            {
                largestSet.insert(largest.begin(), largest.end());
            }
            std::ostringstream uncovered;
            bool first = true;
            for (const auto& [satellite, row] : rows)
            {
                if (largestSet.find(satellite) != largestSet.end())
                {
                    continue;
                }
                uncovered << (first ? "" : ",") << satellite.id();
                first = false;
            }
            coverage.uncoveredSatellites = first ? "NONE" : uncovered.str();
            return coverage;
        };

        map<E_ObsCode, ZhangSatelliteProductCoverage> productCoverage;
        map<E_ObsCode, map<SatSys, ZhangExactVector>> embeddedTargets;
        if (productTarget.valid)
        {
            const int chordCount = productTarget.currentChords.size();
            for (int signal = 0;
                 signal < static_cast<int>(options.baseline_observables.size());
                 signal++)
            {
                E_ObsCode code = options.baseline_observables[signal];
                auto& targets = embeddedTargets[code];
                targets[productTarget.referenceSatellite] =
                    ZhangExactVector(cycleColumns.size());
                for (int target = 0;
                     target < static_cast<int>(productTarget.matrix.size());
                     target++)
                {
                    ZhangExactVector embedded(cycleColumns.size());
                    for (int chord = 0; chord < chordCount; chord++)
                    {
                        embedded[signal * chordCount + chord] =
                            productTarget.matrix[target][chord];
                    }
                    targets[productTarget.targetSatellites[target]] =
                        std::move(embedded);
                }
                productCoverage[code] = evaluateProductTargets(
                    enum_to_string(code), targets, code
                );
            }

            if (options.baseline_observables.size() == 2)
            {
                E_ObsCode firstCode = options.baseline_observables[0];
                E_ObsCode secondCode = options.baseline_observables[1];
                map<SatSys, ZhangExactVector> wideLaneTargets;
                for (const auto& [satellite, first] : embeddedTargets[firstCode])
                {
                    auto second = embeddedTargets[secondCode].find(satellite);
                    if (second == embeddedTargets[secondCode].end())
                    {
                        continue;
                    }
                    ZhangExactVector wideLane = first;
                    for (int column = 0;
                         column < static_cast<int>(wideLane.size());
                         column++)
                    {
                        wideLane[column] -= second->second[column];
                    }
                    wideLaneTargets[satellite] = std::move(wideLane);
                }
                ZhangSatelliteProductCoverage wideLaneCoverage =
                    evaluateProductTargets("WL", wideLaneTargets);
                trace << "\nZHANG_SATELLITE_PRODUCT_LATTICE time="
                      << kfState.time.to_string(0)
                      << " system=" << enum_to_string(sys)
                      << " target=WL"
                      << " required_satellite_rank="
                      << wideLaneCoverage.requiredRank
                      << " product_target_exact_rank="
                      << wideLaneCoverage.targetRank
                      << " covered_satellite_rank="
                      << wideLaneCoverage.coveredRank
                      << " largest_component="
                      << wideLaneCoverage.largestComponent
                      << " component_id=" << wideLaneCoverage.componentId
                      << " product_datum_version="
                      << graphContext.productDatumVersion
                      << " lattice_index="
                      << (wideLaneCoverage.complete ? "1" : "NOT_FULLY_COVERED");
            }
        }

        for (E_ObsCode code : options.baseline_observables)
        {
            set<SatSys> satellites;
            for (const auto& [key, index] : kfState.kfIndexMap)
            {
                if (key.type == KF::PHASE_BIAS &&
                    key.Sat.sys == sys &&
                    key.Sat.prn > 0 &&
                    key.str.empty() &&
                    key.num == static_cast<int>(code))
                {
                    satellites.insert(key.Sat);
                }
            }

            vector<int> signalColumns;
            set<int> signalColumnSet;
            for (const auto& [column, key] : ambiguityResolution.ambmap)
            {
                if (key.Sat.sys == sys && key.num == static_cast<int>(code))
                {
                    signalColumns.push_back(column);
                    signalColumnSet.insert(column);
                }
            }

            vector<int> signalLocalRows;
            int integerRows = 0;
            for (int row = 0; row < ambiguityResolution.Ztrs.rows(); row++)
            {
                bool integerRow = true;
                bool signalLocal = true;
                bool hasSignalCoefficient = false;
                for (int column = 0; column < ambiguityResolution.Ztrs.cols(); column++)
                {
                    double coefficient = ambiguityResolution.Ztrs(row, column);
                    if (std::abs(coefficient - std::round(coefficient)) > integerTolerance)
                    {
                        integerRow = false;
                    }
                    if (std::abs(coefficient) <= integerTolerance)
                    {
                        continue;
                    }
                    if (signalColumnSet.find(column) == signalColumnSet.end())
                    {
                        signalLocal = false;
                    }
                    else
                    {
                        hasSignalCoefficient = true;
                    }
                }
                integerRows += integerRow;
                if (integerRow && signalLocal && hasSignalCoefficient)
                {
                    signalLocalRows.push_back(row);
                }
            }

            int signalLocalRationalRank = 0;
            int signalLocalExactRank = 0;
            int signalLocalHnfRows = 0;
            ZhangExactInteger signalLocalLatticeIndex = 1;
            if (!signalLocalRows.empty() && !signalColumns.empty())
            {
                MatrixXd local = MatrixXd::Zero(
                    signalLocalRows.size(),
                    signalColumns.size()
                );
                for (int row = 0; row < static_cast<int>(signalLocalRows.size()); row++)
                {
                    for (int column = 0;
                         column < static_cast<int>(signalColumns.size());
                         column++)
                    {
                        local(row, column) = ambiguityResolution.Ztrs(
                            signalLocalRows[row],
                            signalColumns[column]
                        );
                    }
                }
                Eigen::FullPivLU<MatrixXd> decomposition(local);
                decomposition.setThreshold(1e-11);
                signalLocalRationalRank = decomposition.rank();

                ZhangExactMatrix exactRows = zhangExactZeroMatrix(
                    signalLocalRows.size(),
                    signalColumns.size()
                );
                for (int row = 0; row < static_cast<int>(signalLocalRows.size()); row++)
                {
                    for (int column = 0;
                         column < static_cast<int>(signalColumns.size());
                         column++)
                    {
                        exactRows[row][column] = static_cast<long long>(std::llround(
                            ambiguityResolution.Ztrs(
                                signalLocalRows[row],
                                signalColumns[column]
                            )
                        ));
                    }
                }
                ZhangIntegerLatticeMembership exact =
                    zhangIntegerRowLatticeContains(
                        exactRows,
                        ZhangExactVector(signalColumns.size())
                    );
                signalLocalExactRank = exact.rank;
                ZhangExactRowHnf hnf = zhangExactRowHermiteNormalForm(exactRows);
                signalLocalHnfRows = hnf.basis.size();
                for (const auto& invariant : exact.smithInvariants)
                {
                    signalLocalLatticeIndex *= zhangExactAbs(invariant);
                }
            }

            auto coverage = productCoverage.find(code);
            bool evaluated = productTarget.valid &&
                coverage != productCoverage.end();
            ZhangSatelliteProductCoverage exactCoverage = evaluated
                ? coverage->second
                : ZhangSatelliteProductCoverage{};

            trace << "\nZHANG_SATELLITE_INTEGER_LATTICE time="
                  << kfState.time.to_string(0)
                  << " system=" << enum_to_string(sys)
                  << " observable=" << enum_to_string(code)
                  << " satellites=" << satellites.size()
                  << " satellite_integer_rank_required="
                  << std::max(0, static_cast<int>(satellites.size()) - 1)
                  << " satellite_integer_rank_covered="
                  << (evaluated
                          ? std::to_string(exactCoverage.coveredRank)
                          : "NOT_EVALUATED")
                  << " product_target_exact_rank="
                  << (evaluated
                          ? std::to_string(exactCoverage.targetRank)
                          : "NOT_EVALUATED")
                  << " fixed_rows=" << ambiguityResolution.Ztrs.rows()
                  << " integer_rows=" << integerRows
                  << " signal_local_integer_rows=" << signalLocalRows.size()
                  << " signal_local_real_rank_diagnostic="
                  << signalLocalRationalRank
                  << " new_fixed_exact_rank=" << signalLocalExactRank
                  << " new_fixed_hnf_rows=" << signalLocalHnfRows
                  << " new_fixed_lattice_index=" << signalLocalLatticeIndex
                  << " integer_lattice_containment="
                  << (evaluated
                          ? (exactCoverage.complete ? "FULL" : "PARTIAL")
                          : "NOT_EVALUATED")
                  << " lattice_index="
                  << (evaluated && exactCoverage.complete
                          ? "1" : "NOT_FULLY_COVERED")
                  << " valid_satellite_count="
                  << (evaluated ? exactCoverage.validSatelliteCount : 0)
                  << " valid_satellite_component="
                  << (evaluated ? exactCoverage.componentId : "NONE")
                  << " largest_component="
                  << (evaluated ? exactCoverage.largestComponent : 0)
                  << " uncovered_satellites="
                  << (evaluated
                          ? exactCoverage.uncoveredSatellites
                          : "NOT_EVALUATED")
                  << " product_datum_version="
                  << (hasGraphContext
                          ? std::to_string(graphContext.productDatumVersion)
                          : "NOT_EVALUATED")
                  << " gate=DIAGNOSTIC_ONLY"
                  << " reason="
                  << (evaluated
                          ? "G_sat_exact_mapping_evaluated"
                          : productTarget.failureReason);
        }
    }
}

static void traceZhangAmbiguityAndFixedProducts(
    Trace&           trace,
    const KFState&   kfState,
    const GinAR_mtx& ambiguityResolution,
    int              fixedCount
)
{
    // Product/AR diagnostics must remain available when the expensive pure-
    // observation SVD is disabled for a long network run.  These two controls
    // describe different costs and different evidence.
    bool networkDiagnostics =
        (acsConfig.zhangFullRank.enable &&
         acsConfig.zhangFullRank.output_diagnostics) ||
        (acsConfig.zhangPppAr.output_products &&
         acsConfig.zhangPppAr.output_diagnostics);
    bool userDiagnostics =
        acsConfig.zhangPppAr.user_adapter &&
        acsConfig.zhangPppAr.output_diagnostics;
    if (!networkDiagnostics && !userDiagnostics)
    {
        return;
    }

    const int ambiguityCount = ambiguityResolution.aflt.size();
    if (ambiguityCount)
    {
        auto [heldIntegerRank, heldMinEigenvalue] =
            zhangHeldIntegerRank(kfState, ambiguityResolution);

        std::vector<double> fractionalResiduals;
        fractionalResiduals.reserve(ambiguityCount);
        for (double value : ambiguityResolution.aflt)
        {
            fractionalResiduals.push_back(std::abs(value - std::round(value)));
        }
        std::sort(fractionalResiduals.begin(), fractionalResiduals.end());

        double median = fractionalResiduals[fractionalResiduals.size() / 2];
        double p90 =
            fractionalResiduals[
                static_cast<size_t>(0.9 * (fractionalResiduals.size() - 1))
            ];

        double adop = std::numeric_limits<double>::quiet_NaN();
        double minEigenvalue = std::numeric_limits<double>::quiet_NaN();
        Eigen::SelfAdjointEigenSolver<MatrixXd> eigenSolver(ambiguityResolution.Paflt);
        if (eigenSolver.info() == Eigen::Success)
        {
            minEigenvalue = eigenSolver.eigenvalues().minCoeff();

            // Integer pseudo-observations deliberately make parts of the held ambiguity
            // covariance numerically semidefinite.  Clamp only round-off-scale eigenvalues;
            // a materially negative eigenvalue remains visible as a failed ADOP diagnostic.
            double largestEigenvalue =
                std::max(1.0, eigenSolver.eigenvalues().maxCoeff());
            double negativeTolerance =
                1e-10 * largestEigenvalue;
            if (minEigenvalue >= -negativeTolerance)
            {
                ArrayXd variances =
                    eigenSolver.eigenvalues().array().max(1e-16 * largestEigenvalue);
                adop = std::exp(
                    variances.log().sum() /
                    (2.0 * ambiguityCount)
                );
            }
        }

        trace << (userDiagnostics
                      ? "\nZHANG_USER_AR_SUMMARY time="
                      : "\nZHANG_AR_SUMMARY time=")
              << kfState.time.to_string(0)
              << " integer_strategy=" << acsConfig.zhangPppAr.integer_strategy
              << " candidates=" << ambiguityCount
              << " newly_fixed=" << fixedCount
              << " held_integer_rank=" << heldIntegerRank
              << " held_min_covariance_eigenvalue="
              << heldMinEigenvalue
              << " adop_cycles=" << adop
              << " min_covariance_eigenvalue=" << minEigenvalue
              << " median_fractional_cycle=" << median
              << " p90_fractional_cycle=" << p90;

        if (networkDiagnostics)
        {
            for (const auto& [sys, options] : acsConfig.zhangFullRank.sysOpts)
            {
                for (E_ObsCode code : options.baseline_observables)
                {
                    int candidates = 0;
                    for (const auto& [column, key] : ambiguityResolution.ambmap)
                    {
                        candidates +=
                            key.Sat.sys == sys &&
                            key.num == static_cast<int>(code);
                    }
                    auto [rank, minEigenvalue] = zhangHeldIntegerRank(
                        kfState,
                        ambiguityResolution,
                        code,
                        sys
                    );
                    trace << "\nZHANG_SIGNAL_HELD_RANK time="
                          << kfState.time.to_string(0)
                          << " system=" << enum_to_string(sys)
                          << " observable=" << enum_to_string(code)
                          << " candidates=" << candidates
                          << " metric=POSTERIOR_COVARIANCE_THRESHOLD"
                          << " held_integer_rank=" << rank
                          << " held_min_covariance_eigenvalue=" << minEigenvalue;
                }

                if (options.baseline_observables.size() == 2)
                {
                    auto iterator = options.baseline_observables.begin();
                    E_ObsCode firstObservable = *iterator++;
                    E_ObsCode secondObservable = *iterator;
                    ZhangWideLaneHeldRank wideLane =
                        zhangWideLaneHeldIntegerRank(
                            kfState,
                            ambiguityResolution,
                            firstObservable,
                            secondObservable,
                            sys
                        );
                    trace << "\nZHANG_WIDE_LANE_HELD_RANK time="
                          << kfState.time.to_string(0)
                          << " system=" << enum_to_string(sys)
                          << " observable_1=" << enum_to_string(firstObservable)
                          << " observable_2=" << enum_to_string(secondObservable)
                          << " pairs=" << wideLane.pairs
                          << " signal_1_candidates=" << wideLane.firstCandidates
                          << " signal_2_candidates=" << wideLane.secondCandidates
                          << " signal_1_only="
                          << wideLane.firstCandidates - wideLane.pairs
                          << " signal_2_only="
                          << wideLane.secondCandidates - wideLane.pairs
                          << " mapping=COMMON_RECEIVER_SATELLITE_ARC"
                          << " metric=POSTERIOR_COVARIANCE_THRESHOLD"
                          << " held_integer_rank=" << wideLane.rank
                          << " held_min_covariance_eigenvalue="
                          << wideLane.minEigenvalue;
                }
            }
        }
    }

    if (!networkDiagnostics)
    {
        return;
    }

    for (const auto& [phaseKey, phaseIndex] : kfState.kfIndexMap)
    {
        if (phaseKey.type != KF::PHASE_BIAS ||
            phaseKey.Sat.prn <= 0 ||
            phaseKey.str.empty() == false)
        {
            continue;
        }

        auto optionsIt = acsConfig.zhangFullRank.sysOpts.find(phaseKey.Sat.sys);
        if (optionsIt == acsConfig.zhangFullRank.sysOpts.end() ||
            !zhangFullRankUsesObservable(
                static_cast<E_ObsCode>(phaseKey.num),
                optionsIt->second.baseline_observables
            ))
        {
            continue;
        }

        KFKey clockKey;
        clockKey.type = KF::SAT_CLOCK;
        clockKey.Sat  = phaseKey.Sat;

        auto clockIt = kfState.kfIndexMap.find(clockKey);
        if (clockIt == kfState.kfIndexMap.end())
        {
            continue;
        }

        int clockIndex = clockIt->second;
        double clock   = kfState.x(clockIndex);
        double phase   = kfState.x(phaseIndex);
        double correction = clock - phase;
        double variance =
            kfState.P(clockIndex, clockIndex) +
            kfState.P(phaseIndex, phaseIndex) -
            2 * kfState.P(clockIndex, phaseIndex);

        trace << "\nZHANG_FIXED_PRODUCT time=" << kfState.time.to_string(0)
              << " fixed_update_applied=" << (fixedCount > 0)
              << " satellite=" << phaseKey.Sat.id()
              << " observable="
              << enum_to_string(static_cast<E_ObsCode>(phaseKey.num))
              << " ambiguity_fixed_clock_m=" << clock
              << " internal_satellite_phase_m=" << phase
              << " phase_observation_correction_m=" << correction
              << " correction_sigma_m=" << std::sqrt(std::max(0.0, variance));
    }
}

static void tracePhaseClockOsbAmbiguityClosure(
    Trace&               trace,
    const vector<double>& ambiguities
)
{
    if (acsConfig.phaseClockOsb.output_diagnostics == false || ambiguities.empty())
    {
        return;
    }

    double sumSquares = 0;
    int    within015  = 0;
    int    within025  = 0;

    for (double ambiguity : ambiguities)
    {
        double residual = phaseClockOsbFractionalCycle(ambiguity);
        sumSquares += SQR(residual);
        within015 += std::abs(residual) < 0.15;
        within025 += std::abs(residual) < 0.25;
    }

    tracepdeex(
        2,
        trace,
        "\nPHASE_CLOCK_OSB AMBIGUITY_CLOSURE scope=NETWORK_FLOAT count=%d "
        "rms_cycle=%.6f p015=%.6f p025=%.6f",
        (int)ambiguities.size(),
        std::sqrt(sumSquares / ambiguities.size()),
        (double)within015 / ambiguities.size(),
        (double)within025 / ambiguities.size()
    );
}

static map<SatSys, double> phaseClockOsbClockBiasInvariants(KFState& kfState)
{
    map<SatSys, double> invariants;

    for (auto& [sys, opts] : acsConfig.phaseClockOsb.sysOpts)
    {
        if (opts.baseline_code_observables.size() != 2)
        {
            continue;
        }

        E_ObsCode code1 = opts.baseline_code_observables[0];
        E_ObsCode code2 = opts.baseline_code_observables[1];
        auto coefficients = phaseClockOsbCoefficients(sys, code1, code2);
        if (!coefficients)
        {
            continue;
        }

        for (auto& [key1, index1] : kfState.kfIndexMap)
        {
            if (key1.type != KF::CODE_BIAS || key1.Sat.sys != sys || key1.Sat.prn == 0 ||
                key1.str.empty() == false || key1.num != static_cast<int>(code1))
            {
                continue;
            }

            KFKey key2 = key1;
            key2.num   = static_cast<int>(code2);
            if (kfState.kfIndexMap.find(key2) == kfState.kfIndexMap.end())
            {
                continue;
            }

            KFKey clockKey;
            clockKey.type = KF::SAT_CLOCK;
            clockKey.Sat  = key1.Sat;

            double bias1   = 0;
            double bias2   = 0;
            double satClock = 0;
            kfState.getKFValue(key1, bias1);
            kfState.getKFValue(key2, bias2);
            if (kfState.getKFValue(clockKey, satClock) == E_Source::NONE)
            {
                continue;
            }

            // Ginan applies the satellite-clock state with coefficient -1 and
            // satellite code biases with coefficient +1 in ppp_obs.cpp.
            invariants[key1.Sat] =
                -satClock + coefficients->alpha * bias1 - coefficients->beta * bias2;
        }
    }

    return invariants;
}

static void tracePhaseClockOsbProductClosures(
    Trace&                     trace,
    KFState&                   kfState,
    const map<SatSys, double>* beforeAmbiguityFix = nullptr
)
{
    auto& controller = acsConfig.phaseClockOsb;
    if (controller.enable == false || controller.output_diagnostics == false)
    {
        return;
    }

    for (auto& [sys, opts] : controller.sysOpts)
    {
        if (opts.baseline_code_observables.size() == 2)
        {
            E_ObsCode code1 = opts.baseline_code_observables[0];
            E_ObsCode code2 = opts.baseline_code_observables[1];
            auto coefficients = phaseClockOsbCoefficients(sys, code1, code2);

            if (coefficients)
                for (auto& [key1, index1] : kfState.kfIndexMap)
                {
                    if (key1.type != KF::CODE_BIAS || key1.Sat.sys != sys ||
                        key1.Sat.prn == 0 || key1.str.empty() == false ||
                        key1.num != static_cast<int>(code1))
                    {
                        continue;
                    }

                    KFKey key2 = key1;
                    key2.num   = static_cast<int>(code2);
                    if (kfState.kfIndexMap.find(key2) == kfState.kfIndexMap.end())
                    {
                        continue;
                    }

                    double bias1 = 0;
                    double bias2 = 0;
                    kfState.getKFValue(key1, bias1);
                    kfState.getKFValue(key2, bias2);

                    double codeClosure =
                        coefficients->alpha * bias1 - coefficients->beta * bias2;

                    tracepdeex(
                        2,
                        trace,
                        "\nPHASE_CLOCK_OSB CODE_DATUM_CLOSURE sat=%s value_m=%.12e",
                        key1.Sat.id().c_str(),
                        codeClosure
                    );

                    KFKey clockKey;
                    clockKey.type = KF::SAT_CLOCK;
                    clockKey.Sat  = key1.Sat;

                    double satClock = 0;
                    if (kfState.getKFValue(clockKey, satClock) != E_Source::NONE)
                    {
                        double invariant = -satClock + codeClosure;
                        double delta     = 0;
                        bool   hasBefore = false;
                        if (beforeAmbiguityFix)
                        {
                            auto before = beforeAmbiguityFix->find(key1.Sat);
                            if (before != beforeAmbiguityFix->end())
                            {
                                delta     = invariant - before->second;
                                hasBefore = true;
                            }
                        }

                        tracepdeex(
                            2,
                            trace,
                            "\nPHASE_CLOCK_OSB CLOCK_BIAS_CLOSURE sat=%s invariant_m=%.12e "
                            "ar_delta_m=%.12e compared=%d",
                            key1.Sat.id().c_str(),
                            invariant,
                            delta,
                            hasBefore
                        );
                    }
                }
        }

        if (opts.baseline_phase_observables.size() != 2)
        {
            continue;
        }

        E_ObsCode code1 = opts.baseline_phase_observables[0];
        E_ObsCode code2 = opts.baseline_phase_observables[1];
        auto coefficients = phaseClockOsbCoefficients(sys, code1, code2);
        if (!coefficients)
        {
            continue;
        }

        for (auto& [key1, index1] : kfState.kfIndexMap)
        {
            if (key1.type != KF::PHASE_BIAS || key1.Sat.sys != sys ||
                key1.Sat.prn == 0 || key1.str.empty() == false ||
                key1.num != static_cast<int>(code1))
            {
                continue;
            }

            KFKey key2 = key1;
            key2.num   = static_cast<int>(code2);
            if (kfState.kfIndexMap.find(key2) == kfState.kfIndexMap.end())
            {
                continue;
            }

            double phase1 = 0;
            double phase2 = 0;
            kfState.getKFValue(key1, phase1);
            kfState.getKFValue(key2, phase2);

            double wide = coefficients->frequencyRatio /
                              (coefficients->frequencyRatio - 1) *
                              phase1 -
                          1 / (coefficients->frequencyRatio - 1) * phase2;
            double narrow = coefficients->alpha * phase1 - coefficients->beta * phase2;

            double reconstructed1 =
                (coefficients->frequencyRatio + 1) / coefficients->frequencyRatio * narrow -
                wide / coefficients->frequencyRatio;
            double reconstructed2 =
                (coefficients->frequencyRatio + 1) * narrow -
                coefficients->frequencyRatio * wide;
            double frequencyClosure =
                std::max(std::abs(reconstructed1 - phase1), std::abs(reconstructed2 - phase2));

            tracepdeex(
                2,
                trace,
                "\nPHASE_CLOCK_OSB FREQUENCY_CLOSURE sat=%s wide_m=%.12e narrow_m=%.12e "
                "reconstruction_m=%.12e",
                key1.Sat.id().c_str(),
                wide,
                narrow,
                frequencyClosure
            );
        }
    }
}

bool recordFilterError(RejectCallbackDetails rejectDetails)
{
    filterError = true;

    return true;
}

bool applyBestIntegerAmbiguity(
    Trace&   trace,   ///< Debug trace
    KFState& kfState  ///< Reference to Kalman filter containing float solutions
)
{
    KFKey  bestKey;
    double smallestVar = 1e10;

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::AMBIGUITY)
        {
            continue;
        }

        double var = kfState.P(index, index);

        if (var > smallestVar || var < FIXED_AMB_VAR * 5)
        {
            continue;
        }

        smallestVar = var;
        bestKey     = key;
    }

    if (bestKey.type == KF::NONE)
    {
        return false;
    }

    KFMeasEntryList kfMeasEntryList;

    int index = kfState.kfIndexMap[bestKey];

    double closest = round(kfState.x(index));

    KFMeasEntry measEntry(&kfState);

    measEntry.obsKey = bestKey;

    measEntry.addDsgnEntry(bestKey, 1);

    measEntry.setValue(closest);
    measEntry.setNoise(FIXED_AMB_VAR);

    kfMeasEntryList.push_back(measEntry);

    KFMeas kfMeas(kfState, kfMeasEntryList, kfState.time);

    filterError = false;
    kfState.measRejectCallbacks.push_back(recordFilterError);
    {
        kfState.filterKalman(trace, kfMeas);
    }
    kfState.measRejectCallbacks.pop_back();

    if (filterError)
    {
        return false;
    }

    kfState.outputStates(trace, "/AR1");

    return true;
}

void applyUCAmbiguities(
    Trace&     trace,    ///< Debug trace
    KFState&   kfState,  ///< Reference to Kalman filter containing float solutions
    GinAR_mtx& mtrx  ///< Reference to structure containing fixed ambiguities and Z transformations
)
{
    int nz = mtrx.zfix.size();
    int nx = mtrx.ambmap.size();

    tracepdeex(1, trace, "   %d out of %d ambiguities resolved, applying...\n", nz, nx);

    MatrixXd Z    = mtrx.Ztrs;
    VectorXd zfix = mtrx.zfix;

    if (AR_VERBO)
    {
        trace << "\n"
              << "zfix =" << "\n"
              << zfix.transpose() << "\n";
        trace << "\n"
              << "Ztrs =" << "\n"
              << Z << "\n";
    }

    KFMeasEntryList kfMeasEntryList;

    for (int i = 0; i < nz; i++)
    {
        double residual = zfix(i);

        KFMeasEntry measEntry(&kfState);

        measEntry.obsKey.type    = KF::Z_AMB;
        measEntry.obsKey.comment = "Ambiguity Psueodobs";

        measEntry.addNoiseEntry(measEntry.obsKey, 1, FIXED_AMB_VAR);

        tracepdeex(4, trace, "      Applying:  ");

        for (int j = 0; j < nx; j++)
        {
            if (Z(i, j) == 0)
            {
                continue;
            }

            double ambiguity = 0;

            KFKey key = mtrx.ambmap[j];
            kfState.getKFValue(key, ambiguity);

            residual -= Z(i, j) * ambiguity;

            tracepdeex(
                4,
                trace,
                "%+3.0f A(%s,%s,%3s) ",
                Z(i, j),
                key.str.c_str(),
                key.Sat.id().c_str(),
                key.code().c_str()
            );

            InitialState init;
            init.x = ambiguity;
            init.P = 3600;

            measEntry.addDsgnEntry(mtrx.ambmap[j], Z(i, j), init);
        }

        tracepdeex(4, trace, "= %+10.5f\n", zfix(i));

        measEntry.setInnov(residual);

        kfMeasEntryList.push_back(measEntry);
    }

    KFMeas kfMeas(kfState, kfMeasEntryList, kfState.time);

    kfState.filterKalman(trace, kfMeas, "/AR", true);
}

/** Build the integer-estimable ambiguity coordinates used by a standalone
 * PPP-AR receiver.
 *
 * Satellite phase OSBs remove the satellite fractional datum, but one
 * receiver-side phase datum remains for every system/signal.  Consequently,
 * undifferenced ambiguities must not be passed directly to LAMBDA.  When
 * receiver_amb_pivot is enabled, this function replaces each receiver/system/
 * signal group by satellite single differences and leaves one ambiguity as
 * the datum.  D maps original undifferenced ambiguities to integer-estimable
 * coordinates.  Systems without receiver_amb_pivot retain identity rows.
 */
static MatrixXd receiverAmbiguityIntegerTransform(
    Trace&           trace,
    const GinAR_mtx& ambiguityResolution
)
{
    using GroupKey = tuple<string, E_Sys, int>;

    map<GroupKey, vector<int>> groups;
    for (const auto& [localIndex, key] : ambiguityResolution.ambmap)
    {
        groups[{key.str, key.Sat.sys, key.num}].push_back(localIndex);
    }

    int rowCount = 0;
    for (const auto& [group, members] : groups)
    {
        E_Sys system = get<1>(group);
        if (acsConfig.receiver_amb_pivot[system])
        {
            rowCount += std::max(0, static_cast<int>(members.size()) - 1);
        }
        else
        {
            rowCount += members.size();
        }
    }

    MatrixXd transform = MatrixXd::Zero(
        rowCount,
        ambiguityResolution.aflt.size()
    );
    int row = 0;
    for (const auto& [group, members] : groups)
    {
        const auto& [receiver, system, observation] = group;
        if (acsConfig.receiver_amb_pivot[system] == false)
        {
            for (int member : members)
            {
                transform(row++, member) = 1;
            }
            continue;
        }

        if (members.size() < 2)
        {
            tracepdeex(
                2,
                trace,
                "\nPPP_AR RECEIVER_SD receiver=%s system=%s signal=%s "
                "status=INSUFFICIENT_SATELLITES count=%d",
                receiver.c_str(),
                enum_to_string(system).c_str(),
                enum_to_string(int_to_enum<E_ObsCode>(observation)).c_str(),
                static_cast<int>(members.size())
            );
            continue;
        }

        int pivot = *std::min_element(
            members.begin(),
            members.end(),
            [&](int left, int right)
            {
                double leftVariance =
                    ambiguityResolution.Paflt(left, left);
                double rightVariance =
                    ambiguityResolution.Paflt(right, right);
                if (leftVariance != rightVariance)
                {
                    return leftVariance < rightVariance;
                }
                return ambiguityResolution.ambmap.at(left).Sat <
                       ambiguityResolution.ambmap.at(right).Sat;
            }
        );

        const KFKey& pivotKey = ambiguityResolution.ambmap.at(pivot);
        tracepdeex(
            2,
            trace,
            "\nPPP_AR RECEIVER_SD receiver=%s system=%s signal=%s "
            "reference=%s integers=%d",
            receiver.c_str(),
            enum_to_string(system).c_str(),
            enum_to_string(int_to_enum<E_ObsCode>(observation)).c_str(),
            pivotKey.Sat.id().c_str(),
            static_cast<int>(members.size()) - 1
        );

        for (int member : members)
        {
            if (member == pivot)
            {
                continue;
            }
            transform(row, member) = +1;
            transform(row, pivot)  = -1;
            row++;
        }
    }

    return transform;
}

/** Resolve each constellation/signal ambiguity block independently.
 *
 * This is the E1 ablation from the Stage-B product-lattice audit.  It prevents
 * LAMBDA decorrelation from creating arbitrary cross-signal integer rows.  The
 * independently fixed rows are mapped back to the original ambiguity columns
 * before a single pseudo-observation update is applied to the filter.
 */
static int resolveIndependentSignalAmbiguities(
    Trace&       trace,
    GinAR_mtx&   ambiguityResolution,
    const GinAR_opt& options,
    GTime        time
)
{
    map<pair<E_Sys, int>, vector<int>> signalColumns;
    for (const auto& [column, key] : ambiguityResolution.ambmap)
    {
        signalColumns[{key.Sat.sys, key.num}].push_back(column);
    }

    vector<VectorXd> fixedRows;
    vector<double>   fixedValues;
    for (const auto& [signal, columns] : signalColumns)
    {
        GinAR_mtx subset;
        vector<int> stateIndices;
        stateIndices.reserve(columns.size());
        for (int local = 0; local < static_cast<int>(columns.size()); local++)
        {
            int original = columns[local];
            subset.ambmap[local] = ambiguityResolution.ambmap.at(original);
            stateIndices.push_back(original);
        }
        subset.aflt  = ambiguityResolution.aflt(stateIndices);
        subset.Paflt = ambiguityResolution.Paflt(stateIndices, stateIndices);

        // Zhang ambiguity states are already the full-rank fundamental-cycle
        // coordinates k_j.  Applying the standalone-user receiver single-
        // difference transform here would remove valid graph integers a
        // second time and would not be the E1 ablation defined in the Stage-B
        // experiment.  Non-Zhang PPP still needs its receiver phase datum
        // removed before integer search.
        MatrixXd integerTransform = acsConfig.zhangFullRank.enable
            ? MatrixXd::Identity(subset.aflt.size(), subset.aflt.size())
            : receiverAmbiguityIntegerTransform(trace, subset);
        GinAR_mtx integerResolution;
        integerResolution.aflt = integerTransform * subset.aflt;
        integerResolution.Paflt =
            integerTransform * subset.Paflt * integerTransform.transpose();

        int fixed = GNSS_AR(trace, integerResolution, options);
        trace << "\nZHANG_SIGNAL_AR_RESULT time="
              << time.to_string(0)
              << " system=" << enum_to_string(signal.first)
              << " observable="
              << enum_to_string(static_cast<E_ObsCode>(signal.second))
              << " candidates=" << columns.size()
              << " fixed=" << fixed
              << " input_coordinates="
              << (acsConfig.zhangFullRank.enable
                      ? "ZHANG_FUNDAMENTAL_CYCLES"
                      : "RECEIVER_SINGLE_DIFFERENCE")
              << " strategy=INDEPENDENT_SIGNAL";
        if (fixed <= 0)
        {
            continue;
        }

        MatrixXd localRows = integerResolution.Ztrs * integerTransform;
        for (int row = 0; row < localRows.rows(); row++)
        {
            VectorXd fullRow = VectorXd::Zero(ambiguityResolution.aflt.size());
            for (int local = 0; local < static_cast<int>(columns.size()); local++)
            {
                fullRow(columns[local]) = localRows(row, local);
            }
            fixedRows.push_back(std::move(fullRow));
            fixedValues.push_back(integerResolution.zfix(row));
        }
    }

    ambiguityResolution.Ztrs = MatrixXd::Zero(
        fixedRows.size(),
        ambiguityResolution.aflt.size()
    );
    ambiguityResolution.zfix = VectorXd::Zero(fixedValues.size());
    for (int row = 0; row < static_cast<int>(fixedRows.size()); row++)
    {
        ambiguityResolution.Ztrs.row(row) = fixedRows[row].transpose();
        ambiguityResolution.zfix(row) = fixedValues[row];
    }
    return fixedRows.size();
}

/** E2: resolve common-arc wide lanes first, apply them, then resolve the L1
 * fundamental-cycle block in the WL-conditioned covariance. */
static int resolveLayeredWideLaneL1(
    Trace&       trace,
    KFState&     kfState,
    GinAR_mtx&   ambiguityResolution,
    const GinAR_opt& options,
    GTime        time
)
{
    struct PairColumns
    {
        int first  = -1;
        int second = -1;
    };

    vector<VectorXd> fixedRows;
    vector<double>   fixedValues;
    int              totalFixed = 0;

    auto appendAndApply = [&](const MatrixXd& rows, const VectorXd& values)
    {
        if (rows.rows() == 0)
        {
            return;
        }
        GinAR_mtx stage;
        stage.ambmap = ambiguityResolution.ambmap;
        stage.Ztrs   = rows;
        stage.zfix   = values;
        applyUCAmbiguities(trace, kfState, stage);
        for (int row = 0; row < rows.rows(); row++)
        {
            fixedRows.push_back(rows.row(row).transpose());
            fixedValues.push_back(values(row));
        }
        totalFixed += rows.rows();
    };

    auto refreshFloatState = [&]()
    {
        vector<int> stateIndices;
        for (int column = 0;
             column < static_cast<int>(ambiguityResolution.ambmap.size());
             column++)
        {
            stateIndices.push_back(
                kfState.kfIndexMap.at(ambiguityResolution.ambmap.at(column))
            );
        }
        ambiguityResolution.aflt = kfState.x(stateIndices);
        ambiguityResolution.Paflt = kfState.P(stateIndices, stateIndices);
    };

    for (const auto& [system, systemOptions] : acsConfig.zhangFullRank.sysOpts)
    {
        if (systemOptions.baseline_observables.size() != 2)
        {
            continue;
        }
        E_ObsCode firstCode  = systemOptions.baseline_observables[0];
        E_ObsCode secondCode = systemOptions.baseline_observables[1];
        map<ZhangGraphEdge, PairColumns> pairs;
        for (const auto& [column, key] : ambiguityResolution.ambmap)
        {
            if (key.Sat.sys != system)
            {
                continue;
            }
            auto& pair = pairs[{key.str, key.Sat}];
            E_ObsCode code = static_cast<E_ObsCode>(key.num);
            if (code == firstCode)
            {
                pair.first = column;
            }
            else if (code == secondCode)
            {
                pair.second = column;
            }
        }

        vector<PairColumns> commonPairs;
        for (const auto& [edge, pair] : pairs)
        {
            if (pair.first >= 0 && pair.second >= 0)
            {
                commonPairs.push_back(pair);
            }
        }
        MatrixXd wideLaneTransform = MatrixXd::Zero(
            commonPairs.size(),
            ambiguityResolution.aflt.size()
        );
        for (int row = 0; row < static_cast<int>(commonPairs.size()); row++)
        {
            wideLaneTransform(row, commonPairs[row].first)  = +1;
            wideLaneTransform(row, commonPairs[row].second) = -1;
        }

        GinAR_mtx wideLane;
        wideLane.aflt = wideLaneTransform * ambiguityResolution.aflt;
        wideLane.Paflt =
            wideLaneTransform * ambiguityResolution.Paflt *
            wideLaneTransform.transpose();
        int wideLaneFixed = GNSS_AR(trace, wideLane, options);
        trace << "\nZHANG_LAYERED_AR_RESULT time="
              << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " stage=WL"
              << " observable_1=" << enum_to_string(firstCode)
              << " observable_2=" << enum_to_string(secondCode)
              << " candidates=" << commonPairs.size()
              << " fixed=" << wideLaneFixed
              << " mapping=COMMON_RECEIVER_SATELLITE_ARC";

        if (wideLaneFixed <= 0)
        {
            trace << "\nZHANG_LAYERED_AR_RESULT time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " stage=L1"
                  << " observable=" << enum_to_string(firstCode)
                  << " candidates=0 fixed=0 status=SKIPPED_NO_WL_FIX";
            continue;
        }

        MatrixXd fullWideLaneRows = wideLane.Ztrs * wideLaneTransform;
        appendAndApply(fullWideLaneRows, wideLane.zfix);
        refreshFloatState();

        vector<int> firstColumns;
        for (const auto& [column, key] : ambiguityResolution.ambmap)
        {
            if (key.Sat.sys == system &&
                static_cast<E_ObsCode>(key.num) == firstCode)
            {
                firstColumns.push_back(column);
            }
        }
        GinAR_mtx firstSignal;
        firstSignal.aflt = ambiguityResolution.aflt(firstColumns);
        firstSignal.Paflt =
            ambiguityResolution.Paflt(firstColumns, firstColumns);
        int firstFixed = GNSS_AR(trace, firstSignal, options);
        trace << "\nZHANG_LAYERED_AR_RESULT time="
              << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " stage=L1"
              << " observable=" << enum_to_string(firstCode)
              << " candidates=" << firstColumns.size()
              << " fixed=" << firstFixed
              << " status=WL_CONDITIONED";
        if (firstFixed > 0)
        {
            MatrixXd fullFirstRows = MatrixXd::Zero(
                firstSignal.Ztrs.rows(),
                ambiguityResolution.aflt.size()
            );
            for (int local = 0; local < static_cast<int>(firstColumns.size()); local++)
            {
                fullFirstRows.col(firstColumns[local]) =
                    firstSignal.Ztrs.col(local);
            }
            appendAndApply(fullFirstRows, firstSignal.zfix);
            refreshFloatState();
        }
    }

    ambiguityResolution.Ztrs = MatrixXd::Zero(
        fixedRows.size(),
        ambiguityResolution.aflt.size()
    );
    ambiguityResolution.zfix = VectorXd::Zero(fixedValues.size());
    for (int row = 0; row < static_cast<int>(fixedRows.size()); row++)
    {
        ambiguityResolution.Ztrs.row(row) = fixedRows[row].transpose();
        ambiguityResolution.zfix(row) = fixedValues[row];
    }
    return totalFixed;
}

/** E5 diagnostic: resolve the satellite-product target integers themselves.
 *
 * For each constellation this forms the exact independent rows of G_sat in
 * the current fundamental-cycle coordinates, resolves b_WL = b_1 - b_2, feeds
 * those fixed rows back to the full filter, then resolves b_1 conditionally.
 * The resulting rows remain exact integer combinations of current cycle
 * ambiguities and are transported into the persistent physical-arc HNF by the
 * existing appendPersistentHeldRows path.
 */
static int resolveProductTargetWideLaneL1(
    Trace&           trace,
    KFState&         kfState,
    GinAR_mtx&       ambiguityResolution,
    const GinAR_opt& options,
    GTime            time
)
{
    vector<VectorXd> fixedRows;
    vector<double>   fixedValues;
    int              totalFixed = 0;

    auto recoverNamedTargets = [&](const GinAR_mtx& fixed,
                                   std::size_t namedTargetCount)
    {
        ZhangExactMatrix exactRows;
        ZhangExactVector exactValues;
        bool exact = fixed.Ztrs.cols() == static_cast<int>(namedTargetCount) &&
                     fixed.Ztrs.rows() == fixed.zfix.size();
        for (int row = 0; exact && row < fixed.Ztrs.rows(); row++)
        {
            ZhangExactVector exactRow(namedTargetCount);
            for (int column = 0; column < fixed.Ztrs.cols(); column++)
            {
                long long value = std::llround(fixed.Ztrs(row, column));
                if (std::abs(fixed.Ztrs(row, column) - value) > 1e-8)
                {
                    exact = false;
                    break;
                }
                exactRow[column] = value;
            }
            if (!exact)
            {
                break;
            }
            long long fixedValue = std::llround(fixed.zfix(row));
            if (std::abs(fixed.zfix(row) - fixedValue) > 1e-8)
            {
                exact = false;
                break;
            }
            exactRows.push_back(std::move(exactRow));
            exactValues.push_back(fixedValue);
        }
        if (!exact)
        {
            return std::map<std::size_t, ZhangExactInteger>{};
        }
        return ProductConstraintPromotion::recoverNamedTargets(
            exactRows, exactValues, namedTargetCount
        );
    };

    auto appendAndApply = [&](const MatrixXd& rows, const VectorXd& values)
    {
        if (rows.rows() == 0)
        {
            return;
        }
        GinAR_mtx stage;
        stage.ambmap = ambiguityResolution.ambmap;
        stage.Ztrs   = rows;
        stage.zfix   = values;
        applyUCAmbiguities(trace, kfState, stage);
        for (int row = 0; row < rows.rows(); row++)
        {
            fixedRows.push_back(rows.row(row).transpose());
            fixedValues.push_back(values(row));
        }
        totalFixed += rows.rows();
    };
    auto refreshFloatState = [&]()
    {
        vector<int> stateIndices;
        for (int column = 0;
             column < static_cast<int>(ambiguityResolution.ambmap.size());
             column++)
        {
            stateIndices.push_back(
                kfState.kfIndexMap.at(ambiguityResolution.ambmap.at(column))
            );
        }
        ambiguityResolution.aflt = kfState.x(stateIndices);
        ambiguityResolution.Paflt = kfState.P(stateIndices, stateIndices);
    };

    map<pair<E_ObsCode, ZhangGraphEdge>, int> ambiguityColumns;
    for (const auto& [column, key] : ambiguityResolution.ambmap)
    {
        ambiguityColumns[
            {static_cast<E_ObsCode>(key.num), {key.str, key.Sat}}
        ] = column;
    }

    for (const auto& [system, systemOptions] : acsConfig.zhangFullRank.sysOpts)
    {
        if (systemOptions.baseline_observables.size() != 2)
        {
            continue;
        }
        E_ObsCode firstCode  = systemOptions.baseline_observables[0];
        E_ObsCode secondCode = systemOptions.baseline_observables[1];
        ZhangGraphIntegerContext context;
        if (!zhangGraphIntegerContext(kfState, system, context))
        {
            trace << "\nZHANG_PRODUCT_TARGET_AR_RESULT time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " stage=WL candidates=0 fixed=0"
                  << " status=SKIPPED_NO_GRAPH_CONTEXT";
            continue;
        }
        ZhangSatelliteProductTarget target =
            ZhangProductTargetBuilder::build(
                context.basis, context.productBasis
            );
        if (!target.valid)
        {
            trace << "\nZHANG_PRODUCT_TARGET_AR_RESULT time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " stage=WL candidates=0 fixed=0"
                  << " status=SKIPPED_" << target.failureReason;
            continue;
        }

        // Retain original satellite-target rows greedily when they add exact
        // rational rank.  This avoids zero/dependent variables without
        // replacing named satellite relations by opaque HNF combinations.
        const int fullTargetExactRank = zhangExactRowHermiteNormalForm(
            target.matrix
        ).basis.size();
        vector<int> independentRows;
        ZhangExactMatrix independentBasis;
        int exactRank = 0;
        for (int row = 0; row < static_cast<int>(target.matrix.size()); row++)
        {
            bool mappable = true;
            for (int chord = 0;
                 chord < static_cast<int>(target.currentChords.size());
                 chord++)
            {
                if (target.matrix[row][chord] == 0)
                {
                    continue;
                }
                mappable &= ambiguityColumns.find(
                    {firstCode, target.currentChords[chord]}
                ) != ambiguityColumns.end();
                mappable &= ambiguityColumns.find(
                    {secondCode, target.currentChords[chord]}
                ) != ambiguityColumns.end();
            }
            if (!mappable)
            {
                continue;
            }
            ZhangExactMatrix candidateRows = independentBasis;
            candidateRows.push_back(target.matrix[row]);
            int candidateRank = zhangExactRowHermiteNormalForm(
                candidateRows
            ).basis.size();
            if (candidateRank > exactRank)
            {
                independentRows.push_back(row);
                independentBasis.push_back(target.matrix[row]);
                exactRank = candidateRank;
            }
        }

        MatrixXd firstTransform = MatrixXd::Zero(
            independentRows.size(), ambiguityResolution.aflt.size()
        );
        MatrixXd secondTransform = MatrixXd::Zero(
            independentRows.size(), ambiguityResolution.aflt.size()
        );
        bool completeMapping = true;
        for (int localRow = 0;
             localRow < static_cast<int>(independentRows.size());
             localRow++)
        {
            const auto& exactRow = target.matrix[independentRows[localRow]];
            for (int chord = 0;
                 chord < static_cast<int>(target.currentChords.size());
                 chord++)
            {
                if (exactRow[chord] == 0)
                {
                    continue;
                }
                auto firstColumn = ambiguityColumns.find(
                    {firstCode, target.currentChords[chord]}
                );
                auto secondColumn = ambiguityColumns.find(
                    {secondCode, target.currentChords[chord]}
                );
                if (firstColumn == ambiguityColumns.end() ||
                    secondColumn == ambiguityColumns.end())
                {
                    completeMapping = false;
                    break;
                }
                double coefficient = exactRow[chord].convert_to<double>();
                firstTransform(localRow, firstColumn->second) = coefficient;
                secondTransform(localRow, secondColumn->second) = coefficient;
            }
            if (!completeMapping)
            {
                break;
            }
        }
        if (!completeMapping || independentRows.empty())
        {
            trace << "\nZHANG_PRODUCT_TARGET_AR_RESULT time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " stage=WL candidates=" << independentRows.size()
                  << " fixed=0 status="
                  << (independentRows.empty()
                          ? "SKIPPED_ZERO_TARGET_RANK"
                          : "SKIPPED_INCOMPLETE_CYCLE_MAPPING")
                  << " product_datum_version="
                  << context.productDatumVersion
                  << " full_target_exact_rank=" << fullTargetExactRank
                  << " mappable_target_exact_rank=" << exactRank;
            continue;
        }

        map<SatSys, ZhangExactVector> namedTargetRows;
        namedTargetRows[target.referenceSatellite] =
            ZhangExactVector(target.currentChords.size());
        for (int row = 0; row < static_cast<int>(target.targetSatellites.size()); row++)
        {
            namedTargetRows[target.targetSatellites[row]] = target.matrix[row];
        }

        struct TopologyTargetCandidate
        {
            SatSys       anchor;
            SatSys       satellite;
            string       type;
            string       topologyKey;
            VectorXd     firstRow;
            VectorXd     secondRow;
            double       wideLaneVariance = 0;
            double       conditionalFirstVariance = 0;
            double       score = 0;
            int          componentGain = 0;
            int          supportCount = 0;
        };

        auto resolveTopologyTargets = [&]()
        {
            if (!acsConfig.zhangPppAr.component_bridge_targeting &&
                !acsConfig.zhangPppAr.current_state_relinking)
            {
                return 0;
            }

            auto firstComponents = zhangSatelliteDatumComponents(
                system, firstCode
            );
            auto secondComponents = zhangSatelliteDatumComponents(
                system, secondCode
            );
            map<SatSys, const ZhangSatelliteDatumComponent*> secondBySatellite;
            for (const auto& component : secondComponents)
            {
                for (const auto& satellite : component.satellites)
                {
                    secondBySatellite[satellite] = &component;
                }
            }

            struct DualComponent
            {
                string       firstId;
                string       secondId;
                set<SatSys>  satellites;
                set<SatSys>  alignedSatellites;
            };
            map<pair<string, string>, DualComponent> dualComponents;
            for (const auto& firstComponent : firstComponents)
            {
                for (const auto& satellite : firstComponent.satellites)
                {
                    auto second = secondBySatellite.find(satellite);
                    if (second == secondBySatellite.end())
                    {
                        continue;
                    }
                    auto key = std::make_pair(
                        firstComponent.id, second->second->id
                    );
                    auto& dual = dualComponents[key];
                    dual.firstId = firstComponent.id;
                    dual.secondId = second->second->id;
                    dual.satellites.insert(satellite);
                    if (firstComponent.alignedSatellites.find(satellite) !=
                            firstComponent.alignedSatellites.end() &&
                        second->second->alignedSatellites.find(satellite) !=
                            second->second->alignedSatellites.end())
                    {
                        dual.alignedSatellites.insert(satellite);
                    }
                }
            }

            auto makeAmbiguityRow = [&](const ZhangExactVector& exact,
                                        E_ObsCode code,
                                        VectorXd& row)
            {
                row = VectorXd::Zero(ambiguityResolution.aflt.size());
                for (int chord = 0;
                     chord < static_cast<int>(target.currentChords.size());
                     chord++)
                {
                    if (exact[chord] == 0)
                    {
                        continue;
                    }
                    auto column = ambiguityColumns.find(
                        {code, target.currentChords[chord]}
                    );
                    if (column == ambiguityColumns.end())
                    {
                        return false;
                    }
                    row(column->second) = exact[chord].convert_to<double>();
                }
                return true;
            };

            auto physicalSupport = [&](const SatSys& left, const SatSys& right)
            {
                map<string, int> receiverMask;
                for (const auto& edge : context.basis.edges)
                {
                    if (edge.satellite == left)
                    {
                        receiverMask[edge.receiver] |= 1;
                    }
                    if (edge.satellite == right)
                    {
                        receiverMask[edge.receiver] |= 2;
                    }
                }
                return static_cast<int>(std::count_if(
                    receiverMask.begin(), receiverMask.end(),
                    [](const auto& item) { return item.second == 3; }
                ));
            };

            vector<TopologyTargetCandidate> candidates;
            auto addCandidate = [&](const SatSys& anchor,
                                    const SatSys& satellite,
                                    const string& type,
                                    const string& topologyKey,
                                    int componentGain)
            {
                auto anchorRow = namedTargetRows.find(anchor);
                auto satelliteRow = namedTargetRows.find(satellite);
                if (anchorRow == namedTargetRows.end() ||
                    satelliteRow == namedTargetRows.end())
                {
                    return;
                }
                ZhangExactVector exact = satelliteRow->second;
                bool nonzero = false;
                for (int column = 0; column < static_cast<int>(exact.size()); column++)
                {
                    exact[column] -= anchorRow->second[column];
                    nonzero |= exact[column] != 0;
                }
                if (!nonzero)
                {
                    return;
                }

                TopologyTargetCandidate candidate;
                candidate.anchor = anchor;
                candidate.satellite = satellite;
                candidate.type = type;
                candidate.topologyKey = topologyKey;
                candidate.componentGain = componentGain;
                if (!makeAmbiguityRow(exact, firstCode, candidate.firstRow) ||
                    !makeAmbiguityRow(exact, secondCode, candidate.secondRow))
                {
                    return;
                }
                VectorXd wideLaneRow =
                    candidate.firstRow - candidate.secondRow;
                candidate.wideLaneVariance = std::max(
                    0.0,
                    (wideLaneRow.transpose() *
                     ambiguityResolution.Paflt * wideLaneRow)(0, 0)
                );
                double firstVariance = std::max(
                    0.0,
                    (candidate.firstRow.transpose() *
                     ambiguityResolution.Paflt * candidate.firstRow)(0, 0)
                );
                double covariance =
                    (candidate.firstRow.transpose() *
                     ambiguityResolution.Paflt * wideLaneRow)(0, 0);
                candidate.conditionalFirstVariance = firstVariance;
                if (candidate.wideLaneVariance > 1e-14)
                {
                    candidate.conditionalFirstVariance = std::max(
                        0.0,
                        firstVariance - covariance * covariance /
                            candidate.wideLaneVariance
                    );
                }
                candidate.supportCount = physicalSupport(anchor, satellite);
                candidate.score =
                    1e6 * componentGain -
                    1e3 * candidate.wideLaneVariance -
                    1e3 * candidate.conditionalFirstVariance +
                    10 * candidate.supportCount;
                candidates.push_back(std::move(candidate));
            };

            vector<const DualComponent*> dual;
            for (const auto& [key, component] : dualComponents)
            {
                if (component.satellites.size() >= 2)
                {
                    dual.push_back(&component);
                }
            }
            if (acsConfig.zhangPppAr.component_bridge_targeting)
            {
                for (int left = 0; left < static_cast<int>(dual.size()); left++)
                {
                    for (int right = left + 1;
                         right < static_cast<int>(dual.size()); right++)
                    {
                        int gain = dual[left]->satellites.size() +
                                   dual[right]->satellites.size();
                        for (const auto& anchor : dual[left]->satellites)
                        for (const auto& satellite : dual[right]->satellites)
                        {
                            addCandidate(
                                anchor, satellite, "COMPONENT_BRIDGE",
                                "BRIDGE:" + dual[left]->firstId + ":" +
                                    dual[left]->secondId + ":" +
                                    dual[right]->firstId + ":" +
                                    dual[right]->secondId,
                                gain
                            );
                        }
                    }
                }
            }
            if (acsConfig.zhangPppAr.current_state_relinking)
            {
                for (const auto* component : dual)
                {
                    if (component->alignedSatellites.empty())
                    {
                        continue;
                    }
                    const SatSys anchor = *component->alignedSatellites.begin();
                    for (const auto& satellite : component->satellites)
                    {
                        if (component->alignedSatellites.find(satellite) !=
                            component->alignedSatellites.end())
                        {
                            continue;
                        }
                        addCandidate(
                            anchor, satellite, "CURRENT_RELINK",
                            "RELINK:" + satellite.id(),
                            component->satellites.size()
                        );
                    }
                }
            }

            std::sort(
                candidates.begin(), candidates.end(),
                [](const auto& left, const auto& right)
                {
                    if (left.score != right.score)
                    {
                        return left.score > right.score;
                    }
                    return std::tie(left.anchor, left.satellite) <
                           std::tie(right.anchor, right.satellite);
                }
            );
            // Relations joining the same pair of persistent dual-frequency
            // components differ only by already-known internal relations.
            // Retaining several of them makes the topology covariance
            // singular without adding a product datum degree of freedom.
            std::set<string> selectedTopologyKeys;
            vector<TopologyTargetCandidate> independentCandidates;
            for (auto& candidate : candidates)
            {
                if (selectedTopologyKeys.insert(candidate.topologyKey).second)
                {
                    independentCandidates.push_back(std::move(candidate));
                }
            }
            candidates = std::move(independentCandidates);
            if (candidates.size() >
                static_cast<size_t>(acsConfig.zhangPppAr.max_topology_targets))
            {
                candidates.resize(acsConfig.zhangPppAr.max_topology_targets);
            }
            if (candidates.empty())
            {
                trace << "\nZHANG_TOPOLOGY_TARGET_RESULT time="
                      << time.to_string(0)
                      << " system=" << enum_to_string(system)
                      << " candidates=0 fixed_wl=0 fixed_l1=0"
                      << " status=NO_TOPOLOGY_TARGET";
                return 0;
            }

            MatrixXd topologyFirst(
                candidates.size(), ambiguityResolution.aflt.size()
            );
            MatrixXd topologySecond(
                candidates.size(), ambiguityResolution.aflt.size()
            );
            for (int row = 0; row < static_cast<int>(candidates.size()); row++)
            {
                topologyFirst.row(row) = candidates[row].firstRow.transpose();
                topologySecond.row(row) = candidates[row].secondRow.transpose();
                trace << "\nZHANG_TOPOLOGY_TARGET_CANDIDATE time="
                      << time.to_string(0)
                      << " system=" << enum_to_string(system)
                      << " type=" << candidates[row].type
                      << " anchor=" << candidates[row].anchor.id()
                      << " satellite=" << candidates[row].satellite.id()
                      << " component_gain=" << candidates[row].componentGain
                      << " wl_variance=" << candidates[row].wideLaneVariance
                      << " conditional_l1_variance="
                      << candidates[row].conditionalFirstVariance
                      << " physical_support=" << candidates[row].supportCount
                      << " score=" << candidates[row].score;
            }

            MatrixXd topologyWideLane = topologyFirst - topologySecond;
            GinAR_mtx wideLaneStage;
            wideLaneStage.aflt = topologyWideLane * ambiguityResolution.aflt;
            wideLaneStage.Paflt = topologyWideLane *
                ambiguityResolution.Paflt * topologyWideLane.transpose();
            GinAR_opt topologyOptions = options;
            // The missing product-topology dimension is often one bridge,
            // while the general LAMBDA implementation intentionally rejects
            // searches below three dimensions.  ROUND retains the configured
            // distance and integer-error-probability acceptance thresholds
            // and is therefore the appropriate low-dimensional gate here.
            topologyOptions.mode = E_ARmode::ROUND;
            int fixedWideLane = GNSS_AR(
                trace, wideLaneStage, topologyOptions
            );
            auto namedWideLane = recoverNamedTargets(
                wideLaneStage, candidates.size()
            );
            if (fixedWideLane <= 0)
            {
                trace << "\nZHANG_TOPOLOGY_TARGET_RESULT time="
                      << time.to_string(0)
                      << " system=" << enum_to_string(system)
                      << " candidates=" << candidates.size()
                      << " fixed_wl=0 fixed_l1=0 status=WL_NOT_FIXED";
                return 0;
            }
            appendAndApply(
                wideLaneStage.Ztrs * topologyWideLane,
                wideLaneStage.zfix
            );
            refreshFloatState();

            GinAR_mtx firstStage;
            firstStage.aflt = topologyFirst * ambiguityResolution.aflt;
            firstStage.Paflt = topologyFirst *
                ambiguityResolution.Paflt * topologyFirst.transpose();
            int fixedFirst = GNSS_AR(trace, firstStage, topologyOptions);
            auto namedFirst = recoverNamedTargets(
                firstStage, candidates.size()
            );
            if (fixedFirst > 0)
            {
                appendAndApply(firstStage.Ztrs * topologyFirst, firstStage.zfix);
                refreshFloatState();
            }

            int topologyEvents = 0;
            for (const auto& [candidateIndex, firstValueExact] : namedFirst)
            {
                auto wideLaneValue = namedWideLane.find(candidateIndex);
                if (candidateIndex >= candidates.size() ||
                    wideLaneValue == namedWideLane.end())
                {
                    continue;
                }
                const auto& candidate = candidates[candidateIndex];
                long long firstValue = firstValueExact.convert_to<long long>();
                long long secondValue = firstValue -
                    wideLaneValue->second.convert_to<long long>();
                ZhangProductRelationEvent firstEvent;
                ZhangProductRelationEvent secondEvent;
                if (candidate.type == "CURRENT_RELINK")
                {
                    firstEvent = relinkZhangSatelliteProductRelation(
                        time, system, firstCode,
                        candidate.anchor, candidate.satellite,
                        firstValue, "topology_relink_L1"
                    );
                    secondEvent = relinkZhangSatelliteProductRelation(
                        time, system, secondCode,
                        candidate.anchor, candidate.satellite,
                        secondValue, "topology_relink_L2"
                    );
                }
                else
                {
                    firstEvent = promoteZhangSatelliteProductRelationDetailed(
                        time, system, firstCode,
                        candidate.anchor, candidate.satellite,
                        firstValue, "topology_component_bridge_L1"
                    );
                    secondEvent = promoteZhangSatelliteProductRelationDetailed(
                        time, system, secondCode,
                        candidate.anchor, candidate.satellite,
                        secondValue, "topology_component_bridge_L2"
                    );
                }
                topologyEvents += firstEvent.accepted && secondEvent.accepted;
            }
            trace << "\nZHANG_TOPOLOGY_TARGET_RESULT time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " candidates=" << candidates.size()
                  << " fixed_wl=" << fixedWideLane
                  << " named_wl=" << namedWideLane.size()
                  << " fixed_l1=" << fixedFirst
                  << " named_l1=" << namedFirst.size()
                  << " dual_events=" << topologyEvents
                  << " status=" << (topologyEvents ? "PROMOTED" : "NO_NAMED_DUAL_FIX");
            return topologyEvents;
        };

        MatrixXd wideLaneTransform = firstTransform - secondTransform;
        GinAR_mtx wideLane;
        wideLane.aflt = wideLaneTransform * ambiguityResolution.aflt;
        wideLane.Paflt =
            wideLaneTransform * ambiguityResolution.Paflt *
            wideLaneTransform.transpose();
        int wideLaneFixed = GNSS_AR(trace, wideLane, options);
        auto fixedWideLaneTargets = recoverNamedTargets(
            wideLane, independentRows.size()
        );
        trace << "\nZHANG_PRODUCT_TARGET_AR_RESULT time="
              << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " stage=WL"
              << " candidates=" << independentRows.size()
              << " full_target_exact_rank=" << fullTargetExactRank
              << " mappable_target_exact_rank=" << exactRank
              << " fixed=" << wideLaneFixed
              << " mapping=G_SAT_WL"
              << " product_datum_version=" << context.productDatumVersion;
        if (wideLaneFixed <= 0)
        {
            trace << "\nZHANG_PRODUCT_TARGET_AR_RESULT time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " stage=L1 candidates=0 fixed=0"
                  << " status=SKIPPED_NO_PRODUCT_WL_FIX"
                  << " product_datum_version="
                  << context.productDatumVersion;
            resolveTopologyTargets();
            continue;
        }

        appendAndApply(wideLane.Ztrs * wideLaneTransform, wideLane.zfix);
        refreshFloatState();

        GinAR_mtx firstSignal;
        firstSignal.aflt = firstTransform * ambiguityResolution.aflt;
        firstSignal.Paflt =
            firstTransform * ambiguityResolution.Paflt *
            firstTransform.transpose();
        int firstFixed = GNSS_AR(trace, firstSignal, options);
        auto fixedFirstTargets = recoverNamedTargets(
            firstSignal, independentRows.size()
        );
        trace << "\nZHANG_PRODUCT_TARGET_AR_RESULT time="
              << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " stage=L1"
              << " candidates=" << independentRows.size()
              << " full_target_exact_rank=" << fullTargetExactRank
              << " mappable_target_exact_rank=" << exactRank
              << " fixed=" << firstFixed
              << " status=PRODUCT_WL_CONDITIONED"
              << " mapping=G_SAT_L1"
              << " product_datum_version=" << context.productDatumVersion;
        if (firstFixed > 0)
        {
            appendAndApply(firstSignal.Ztrs * firstTransform, firstSignal.zfix);
            refreshFloatState();
        }

        int promotedFirst = 0;
        int promotedSecond = 0;
        int rejected = 0;
        int pending = 0;
        int quarantined = 0;
        for (const auto& [localTarget, firstValueExact] : fixedFirstTargets)
        {
            if (localTarget >= independentRows.size())
            {
                continue;
            }
            const SatSys& satellite =
                target.targetSatellites[independentRows[localTarget]];
            long long firstValue = firstValueExact.convert_to<long long>();
            auto firstEvent = promoteZhangSatelliteProductRelationDetailed(
                time,
                system,
                firstCode,
                target.referenceSatellite,
                satellite,
                firstValue,
                "G_sat_L1_named_target"
            );
            promotedFirst += firstEvent.accepted;
            rejected += firstEvent.type ==
                ZhangProductRelationEventType::CONFLICT_REJECTED;
            pending += firstEvent.type ==
                ZhangProductRelationEventType::PENDING_CONFIRMATION;
            quarantined += firstEvent.type ==
                ZhangProductRelationEventType::CURRENT_ALIGNMENT_QUARANTINED;

            auto wideLaneValue = fixedWideLaneTargets.find(localTarget);
            if (wideLaneValue == fixedWideLaneTargets.end())
            {
                continue;
            }
            long long secondValue =
                firstValue - wideLaneValue->second.convert_to<long long>();
            auto secondEvent = promoteZhangSatelliteProductRelationDetailed(
                time,
                system,
                secondCode,
                target.referenceSatellite,
                satellite,
                secondValue,
                "G_sat_L2_from_L1_minus_WL"
            );
            promotedSecond += secondEvent.accepted;
            rejected += secondEvent.type ==
                ZhangProductRelationEventType::CONFLICT_REJECTED;
            pending += secondEvent.type ==
                ZhangProductRelationEventType::PENDING_CONFIRMATION;
            quarantined += secondEvent.type ==
                ZhangProductRelationEventType::CURRENT_ALIGNMENT_QUARANTINED;
        }
        trace << "\nZHANG_PRODUCT_CONSTRAINT_PROMOTION time="
              << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " reference=" << target.referenceSatellite.id()
              << " wl_named=" << fixedWideLaneTargets.size()
              << " l1_named=" << fixedFirstTargets.size()
              << " promoted_l1=" << promotedFirst
              << " promoted_l2=" << promotedSecond
              << " rejected_inconsistent=" << rejected
              << " pending_confirmation=" << pending
              << " quarantined_alignment=" << quarantined
              << " physical_source_rows_retirable=1";
        resolveTopologyTargets();
    }

    ambiguityResolution.Ztrs = MatrixXd::Zero(
        fixedRows.size(), ambiguityResolution.aflt.size()
    );
    ambiguityResolution.zfix = VectorXd::Zero(fixedValues.size());
    for (int row = 0; row < static_cast<int>(fixedRows.size()); row++)
    {
        ambiguityResolution.Ztrs.row(row) = fixedRows[row].transpose();
        ambiguityResolution.zfix(row) = fixedValues[row];
    }
    return totalFixed;
}

void fixAndHoldAmbiguities(
    Trace&   trace,   ///< Debug trace
    KFState& kfState  ///< Filter state
)
{
    tracepdeex(3, trace, "%s: %s\n", __FUNCTION__, kfState.time.to_string().c_str());

    if (acsConfig.ambrOpts.mode == E_ARmode::OFF)
    {
        return;
    }

    GinAR_mtx        ARmtx;
    map<string, int> nsat;  // number of satellites visible by station
    map<SatSys, int> nsta;  // number of stations visible by satellite

    vector<pair<KFKey, int>> ambiguityCandidates;
    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::AMBIGUITY)
        {
            continue;
        }

        if (acsConfig.solve_amb_for[key.Sat.sys] == false)
        {
            continue;
        }

        if (useAmbiguityForPhaseClockOsb(key) == false)
        {
            continue;
        }

        if (useAmbiguityForZhang(kfState, key) == false)
        {
            continue;
        }

        ambiguityCandidates.emplace_back(key, index);
    }

    int userCap = acsConfig.zhangPppAr.user_max_ambiguities_per_signal;
    if (acsConfig.zhangPppAr.user_adapter && userCap > 0)
    {
        map<pair<E_Sys, int>, vector<pair<KFKey, int>>> grouped;
        for (const auto& candidate : ambiguityCandidates)
        {
            grouped[{candidate.first.Sat.sys, candidate.first.num}]
                .push_back(candidate);
        }

        int originalCount = ambiguityCandidates.size();
        ambiguityCandidates.clear();
        for (auto& [signal, candidates] : grouped)
        {
            std::sort(
                candidates.begin(),
                candidates.end(),
                [&](const auto& left, const auto& right)
                {
                    double leftVariance = kfState.P(left.second, left.second);
                    double rightVariance = kfState.P(right.second, right.second);
                    if (leftVariance != rightVariance)
                    {
                        return leftVariance < rightVariance;
                    }
                    return left.first.Sat < right.first.Sat;
                }
            );
            if (static_cast<int>(candidates.size()) > userCap)
            {
                candidates.resize(userCap);
            }
            ambiguityCandidates.insert(
                ambiguityCandidates.end(),
                candidates.begin(),
                candidates.end()
            );
        }
        std::sort(
            ambiguityCandidates.begin(),
            ambiguityCandidates.end(),
            [](const auto& left, const auto& right)
            {
                return left.second < right.second;
            }
        );
        trace << "\nZHANG_USER_PAR_SELECTION time="
              << kfState.time.to_string(0)
              << " original_candidates=" << originalCount
              << " selected_candidates=" << ambiguityCandidates.size()
              << " max_per_signal=" << userCap;
    }

    int ind = 0;
    vector<int> indices;
    for (const auto& [key, index] : ambiguityCandidates)
    {
        indices.push_back(index);
        ARmtx.ambmap[ind++] = key;
    }

    if (acsConfig.zhangFullRank.enable)
    {
        auto heldSets = projectPersistentHeldRows(trace, kfState);
        for (auto& held : heldSets)
        {
            trace << "\nZHANG_HELD_LATTICE_REAPPLY time="
                  << kfState.time.to_string(0)
                  << " rows=" << held.zfix.size()
                  << " columns=" << held.ambmap.size();
            applyUCAmbiguities(trace, kfState, held);
        }
    }

    if (ind == 0)
    {
        auto floatInvariants = phaseClockOsbClockBiasInvariants(kfState);
        tracePhaseClockOsbProductClosures(trace, kfState, &floatInvariants);
        traceZhangAmbiguityAndFixedProducts(trace, kfState, ARmtx, 0);
        traceZhangSatelliteIntegerLattice(trace, kfState, ARmtx);
        writeZhangInternalProducts(trace, kfState, kfState, 0, false);
        return;
    }

    ARmtx.aflt  = kfState.x(indices);
    ARmtx.Paflt = kfState.P(indices, indices);

    vector<double> floatAmbiguities(ARmtx.aflt.data(), ARmtx.aflt.data() + ARmtx.aflt.size());
    tracePhaseClockOsbAmbiguityClosure(trace, floatAmbiguities);
    auto floatInvariants = phaseClockOsbClockBiasInvariants(kfState);
    KFState floatState = kfState;

    GinAR_opt ARopt;
    ARopt.mode   = acsConfig.ambrOpts.mode;
    ARopt.sucthr = acsConfig.ambrOpts.succsThres;
    ARopt.ratthr = acsConfig.ambrOpts.ratioThres;
    ARopt.nset   = acsConfig.ambrOpts.lambda_set;
    ARopt.nitr   = acsConfig.ambrOpts.AR_max_itr;

    if (traceLevel > 4)
        AR_VERBO = true;

    int  nfix = 0;
    bool fixedRowsAlreadyApplied = false;
    if (acsConfig.zhangFullRank.enable &&
        acsConfig.zhangPppAr.integer_strategy == "INDEPENDENT_SIGNAL")
    {
        nfix = resolveIndependentSignalAmbiguities(
            trace,
            ARmtx,
            ARopt,
            kfState.time
        );
    }
    else if (acsConfig.zhangFullRank.enable &&
             acsConfig.zhangPppAr.integer_strategy == "LAYERED_WL_L1")
    {
        nfix = resolveLayeredWideLaneL1(
            trace,
            kfState,
            ARmtx,
            ARopt,
            kfState.time
        );
        fixedRowsAlreadyApplied = true;
    }
    else if (acsConfig.zhangFullRank.enable &&
             acsConfig.zhangPppAr.integer_strategy == "PRODUCT_TARGET_WL_L1")
    {
        nfix = resolveProductTargetWideLaneL1(
            trace,
            kfState,
            ARmtx,
            ARopt,
            kfState.time
        );
        fixedRowsAlreadyApplied = true;
    }
    else
    {
        // Resolve only integer-estimable ambiguity coordinates.  For standalone
        // PPP-AR this removes one receiver phase datum per system/signal.
        MatrixXd integerTransform =
            receiverAmbiguityIntegerTransform(trace, ARmtx);

        GinAR_mtx integerResolution;
        integerResolution.aflt =
            integerTransform * ARmtx.aflt;
        integerResolution.Paflt =
            integerTransform * ARmtx.Paflt * integerTransform.transpose();

        nfix = GNSS_AR(trace, integerResolution, ARopt);
        if (nfix > 0)
        {
            ARmtx.Ztrs =
                integerResolution.Ztrs * integerTransform;
            ARmtx.zfix = integerResolution.zfix;
        }
        else
        {
            ARmtx.Ztrs.resize(0, ARmtx.aflt.size());
            ARmtx.zfix.resize(0);
        }
    }

    if (nfix > 0)
    {
        if (!fixedRowsAlreadyApplied)
        {
            applyUCAmbiguities(trace, kfState, ARmtx);
        }
        if (acsConfig.zhangFullRank.enable)
        {
            appendPersistentHeldRows(trace, kfState, ARmtx);
        }
    }

    tracePhaseClockOsbProductClosures(trace, kfState, &floatInvariants);
    traceZhangAmbiguityAndFixedProducts(trace, kfState, ARmtx, nfix);
    traceZhangSatelliteIntegerLattice(trace, kfState, ARmtx);
    auto [heldIntegerRank, heldMinEigenvalue] =
        zhangHeldIntegerRank(kfState, ARmtx);
    bool integerDatumComplete =
        ARmtx.aflt.size() > 0 &&
        heldIntegerRank == ARmtx.aflt.size();
    writeZhangInternalProducts(
        trace,
        floatState,
        kfState,
        nfix,
        integerDatumComplete
    );

    while (0)
    {
        bool applied = applyBestIntegerAmbiguity(trace, kfState);

        if (applied == false)
        {
            break;
        }
    }
}

bool queryBiasUC(
    Trace&   trace,    ///< debug stream
    GTime    time,     ///< time of biases
    KFState& kfState,  ///< filter state to take biases from
    SatSys   Sat,    ///< satellite (for receiver biases, sat.sys needs to be set to the appropriate
                     ///< system, and sat.prn must be 0)
    string     rec,  ///< receiver  (for satellite biases nees to be "")
    E_ObsCode  code,  ///< signal code
    double&    bias,  ///< bias value
    double&    var,   ///< bias variance
    E_MeasType type   ///< measurement type
)
{
    KFKey kfKey;
    kfKey.str = rec;
    kfKey.Sat = Sat;
    kfKey.num = static_cast<int>(code);

    if (Sat.prn == 0)  // todo? check if needed and reverse logic
    {
        auto& recOpts = acsConfig.getRecOpts(rec, {Sat.sysName(), enum_to_string(code)});

        if (type == CODE)
        {
            if (recOpts.codeBiasModel.enable == false)
                return true;

            InitialState init = initialStateFromConfig(recOpts.code_bias);
            if (init.estimate == false)
            {
                getBias(trace, time, rec, Sat, code, CODE, bias, var);
                return true;
            }

            kfKey.type = KF::CODE_BIAS;

            return kfState.getKFValue(kfKey, bias, &var) != E_Source::NONE;
        }

        if (type == PHAS)
        {
            if (recOpts.phaseBiasModel.enable == false)
                return true;

            InitialState init = initialStateFromConfig(recOpts.phase_bias);
            if (init.estimate == false)
            {
                getBias(trace, time, rec, Sat, code, PHAS, bias, var);

                return true;
            }

            kfKey.type = KF::PHASE_BIAS;

            return kfState.getKFValue(kfKey, bias, &var) != E_Source::NONE;
        }
    }
    else if (rec.empty())
    {
        auto& satOpts = acsConfig.getSatOpts(Sat);

        if (type == CODE)
        {
            if (!satOpts.codeBiasModel.enable)
                return true;

            InitialState init = initialStateFromConfig(satOpts.code_bias);
            if (init.estimate == false)
            {
                getBias(trace, time, Sat.id(), Sat, code, CODE, bias, var);
                return true;
            }

            kfKey.type       = KF::CODE_BIAS;
            E_Source passSrc = kfState.getKFValue(kfKey, bias, &var);
            bool     pass    = passSrc != E_Source::NONE;

            tracepdeex(
                5,
                trace,
                "\n Searching UC %s - %s",
                ((string)kfKey).c_str(),
                pass ? "found" : "not found"
            );

            return pass;
        }

        if (type == PHAS)
        {
            if (satOpts.phaseBiasModel.enable == false)
                return true;

            InitialState init = initialStateFromConfig(satOpts.phase_bias);
            if (init.estimate == false)
            {
                getBias(trace, time, Sat.id(), Sat, code, PHAS, bias, var);
                return true;
            }

            kfKey.type       = KF::PHASE_BIAS;
            E_Source passSrc = kfState.getKFValue(kfKey, bias, &var);
            bool     pass    = passSrc != E_Source::NONE;

            tracepdeex(
                5,
                trace,
                "\n Searching UC %s - %s",
                ((string)kfKey).c_str(),
                pass ? "found" : "not found"
            );

            return pass;
        }
    }

    return false;
}
