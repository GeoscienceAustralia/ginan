#pragma once

#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "zhangIntegerAudit.hpp"

enum class TemporalCertificateKind
{
    SELF_GAUGE_SHIFT,
    INTER_SATELLITE_BRIDGE
};

inline const char* zhangTemporalCertificateKindName(
    TemporalCertificateKind kind)
{
    return kind == TemporalCertificateKind::SELF_GAUGE_SHIFT
        ? "SELF_GAUGE_SHIFT" : "INTER_SATELLITE_BRIDGE";
}

/** One statistically certified temporal product relation expressed directly
 * in immutable physical-arc coordinates.  Reliability fields are retained so
 * the admission boundary cannot silently weaken the upstream gates. */
struct ZhangProductRelationAdmissionCandidate
{
    TemporalCertificateKind certificateKind =
        TemporalCertificateKind::SELF_GAUGE_SHIFT;
    std::string relationId;
    std::string satellite;
    std::string observable;
    std::map<std::string, ZhangExactInteger> physicalCoefficients;
    ZhangExactInteger integerValue = 0;
    bool exactIntegerEstimable = false;
    bool phaseSegmentCompatible = false;
    bool scalarReliabilityPassed = false;
    bool jointNisPassed = false;
};

struct TemporalCertificateConfirmationState
{
    std::string canonicalRelationKey;
    ZhangExactInteger integerValue = 0;
    int consistentEpochs = 0;
    long int firstEpoch = 0;
    long int lastEpoch = 0;
    std::set<std::string> supportSignatures;
    bool redundancyConfirmed = false;
};

struct TemporalCertificateConfirmationResult
{
    bool accepted = false;
    bool reset = false;
    int consistentEpochs = 0;
    std::string reason = "NOT_EVALUATED";
};

inline TemporalCertificateConfirmationResult
zhangConfirmTemporalCertificate(
    TemporalCertificateConfirmationState& state,
    const std::string& canonicalRelationKey,
    const ZhangExactInteger& integerValue,
    long int epoch,
    const std::string& supportSignature,
    bool redundancyConfirmed,
    int requiredEpochs,
    double maximumGapSeconds,
    bool requireRedundancy = false)
{
    TemporalCertificateConfirmationResult result;
    if (canonicalRelationKey.empty() || epoch <= 0 || requiredEpochs < 1 ||
        maximumGapSeconds < 0)
    {
        result.reason = "CONFIRMATION_INPUT_INVALID";
        return result;
    }
    const bool sameEpoch = state.consistentEpochs > 0 &&
        epoch == state.lastEpoch;
    const bool sameValue = state.consistentEpochs > 0 &&
        state.canonicalRelationKey == canonicalRelationKey &&
        state.integerValue == integerValue;
    const bool monotonic = state.consistentEpochs == 0 ||
        epoch >= state.lastEpoch;
    const bool gapValid = monotonic && (state.consistentEpochs == 0 ||
        sameEpoch || maximumGapSeconds <= 0 ||
        epoch - state.lastEpoch <= maximumGapSeconds);
    if (state.consistentEpochs > 0 && (!sameValue || !gapValid))
    {
        state = {};
        result.reset = true;
    }
    if (state.consistentEpochs == 0)
    {
        state.canonicalRelationKey = canonicalRelationKey;
        state.integerValue = integerValue;
        state.firstEpoch = epoch;
    }
    if (epoch != state.lastEpoch)
    {
        state.lastEpoch = epoch;
        state.consistentEpochs++;
    }
    if (!supportSignature.empty())
    {
        state.supportSignatures.insert(supportSignature);
    }
    state.redundancyConfirmed |= redundancyConfirmed ||
        state.supportSignatures.size() >= 2;
    result.consistentEpochs = state.consistentEpochs;
    result.accepted = state.consistentEpochs >= requiredEpochs &&
        (!requireRedundancy || state.redundancyConfirmed);
    result.reason = result.accepted
        ? "TEMPORAL_CERTIFICATE_CONFIRMED"
        : (requireRedundancy && !state.redundancyConfirmed
            ? "AWAITING_REDUNDANCY" : "AWAITING_CONSISTENT_EPOCHS");
    return result;
}

struct ZhangProductRelationAdmissionState
{
    std::vector<std::map<std::string, ZhangExactInteger>> certifiedRows;
    ZhangExactVector certifiedValues;
    std::set<std::string> certifiedSatellites;
    std::set<std::string> certifiedRelationIds;
    std::map<std::string, ZhangProductRelationAdmissionCandidate>
        pendingCandidates;
    std::map<std::string, TemporalCertificateConfirmationState>
        temporalCertificateConfirmations;
};

/** Process-wide frontend admission registry shared by the ambiguity solver
 * and the delayed targeted-BESD path.  The stable runtime ID prevents copied
 * KF branches from sharing mutable scientific state accidentally. */
inline std::map<std::pair<std::string, E_Sys>,
	ZhangProductRelationAdmissionState>&
zhangProductRelationAdmissionStateRegistry()
{
	static std::map<std::pair<std::string, E_Sys>,
		ZhangProductRelationAdmissionState> registry;
	return registry;
}

struct ZhangProductRelationAdmissionResult
{
    int candidateRows = 0;
    int freshRows = 0;
    int duplicateRows = 0;
    int candidateExactRank = 0;
    int candidateRedundantRows = 0;
    int persistentRankBefore = 0;
    int persistentRankAfter = 0;
    int restoredSatellites = 0;
    int observableGroups = 0;
    int redundancyCheckedGroups = 0;
    bool allExactIntegerEstimable = false;
    bool allPhaseSegmentsCompatible = false;
    bool allScalarReliabilityPassed = false;
    bool allJointNisPassed = false;
    bool candidateCycleClosureConsistent = false;
    bool persistentCycleClosureConsistent = false;
    bool redundancyCheckPassed = false;
    bool committed = false;
    std::string status = "NO_CANDIDATES";
};

/** Transactional frontend-only relation admission.
 *
 * Candidate rows are canonicalised in the union physical coordinate using
 * exact row-HNF with the integer right-hand sides transformed by the same
 * unimodular operations.  A batch commits only when every scientific gate
 * passes, at least one independent relation is present, redundant closure
 * evidence exists, and the augmented persistent system remains consistent.
 * The supplied state is unchanged on every rejection path. */
class ProductRelationAdmission
{
public:
    static ZhangProductRelationAdmissionResult admit(
        ZhangProductRelationAdmissionState& state,
        const std::vector<ZhangProductRelationAdmissionCandidate>& candidates,
        bool requireRedundantClosure = true)
    {
        ZhangProductRelationAdmissionResult result;
        std::vector<ZhangProductRelationAdmissionCandidate> fresh;
        fresh.reserve(candidates.size());
        for (const auto& candidate : candidates)
        {
            if (state.certifiedRelationIds.count(candidate.relationId) > 0 ||
                state.pendingCandidates.count(candidate.relationId) > 0)
            {
                result.duplicateRows++;
            }
            else
            {
                fresh.push_back(candidate);
            }
        }
        result.freshRows = static_cast<int>(fresh.size());
        if (fresh.empty())
        {
            result.status = candidates.empty() ? "NO_CANDIDATES" :
                (state.pendingCandidates.empty()
                    ? "ALREADY_CERTIFIED" : "ALREADY_PENDING");
            return result;
        }

        auto proposedPending = state.pendingCandidates;
        for (const auto& candidate : fresh)
        {
            proposedPending[candidate.relationId] = candidate;
        }
        std::vector<ZhangProductRelationAdmissionCandidate> auditCandidates;
        auditCandidates.reserve(proposedPending.size());
        for (const auto& [relationId, candidate] : proposedPending)
        {
            auditCandidates.push_back(candidate);
        }
        result.candidateRows = static_cast<int>(auditCandidates.size());

        result.allExactIntegerEstimable = true;
        result.allPhaseSegmentsCompatible = true;
        result.allScalarReliabilityPassed = true;
        result.allJointNisPassed = true;
        for (const auto& candidate : auditCandidates)
        {
            result.allExactIntegerEstimable &=
                candidate.exactIntegerEstimable;
            result.allPhaseSegmentsCompatible &=
                candidate.phaseSegmentCompatible;
            result.allScalarReliabilityPassed &=
                candidate.scalarReliabilityPassed;
            result.allJointNisPassed &= candidate.jointNisPassed;
        }
        if (!result.allExactIntegerEstimable)
        {
            result.status = "REJECTED_NOT_EXACT_INTEGER_ESTIMABLE";
            return result;
        }
        if (!result.allPhaseSegmentsCompatible)
        {
            result.status = "REJECTED_PHASE_SEGMENT_INCOMPATIBLE";
            return result;
        }
        if (!result.allScalarReliabilityPassed)
        {
            result.status = "REJECTED_SCALAR_RELIABILITY";
            return result;
        }
        if (!result.allJointNisPassed)
        {
            result.status = "REJECTED_JOINT_NIS";
            return result;
        }

        const auto candidateSparse = sparseRows(auditCandidates);
        const auto candidateValues = integerValues(auditCandidates);
        const auto candidateAudit = exactAudit(
            candidateSparse, candidateValues);
        result.candidateExactRank =
            static_cast<int>(candidateAudit.hnf.basis.size());
        result.candidateRedundantRows = result.candidateRows -
            result.candidateExactRank;
        result.candidateCycleClosureConsistent =
            candidateAudit.hnf.consistent;
        std::map<std::string,
            std::vector<ZhangProductRelationAdmissionCandidate>> byObservable;
        for (const auto& candidate : auditCandidates)
        {
            byObservable[candidate.observable].push_back(candidate);
        }
        result.observableGroups = static_cast<int>(byObservable.size());
        bool everyObservableRedundant = true;
        for (const auto& [observable, group] : byObservable)
        {
            const auto groupAudit = exactAudit(
                sparseRows(group), integerValues(group));
            const int groupRank =
                static_cast<int>(groupAudit.hnf.basis.size());
            const bool groupRedundant = groupAudit.hnf.consistent &&
                groupRank > 0 && static_cast<int>(group.size()) > groupRank;
            everyObservableRedundant &= groupRedundant;
            result.redundancyCheckedGroups += groupRedundant;
        }
        result.redundancyCheckPassed = !requireRedundantClosure ||
            everyObservableRedundant;
        if (!candidateAudit.hnf.consistent)
        {
            result.status = "ABORT_INCONSISTENT_CANDIDATE_CYCLE_CLOSURE";
            return result;
        }
        if (result.candidateExactRank == 0)
        {
            result.status = "REJECTED_ZERO_EXACT_RANK";
            return result;
        }
        if (!result.redundancyCheckPassed)
        {
            state.pendingCandidates = std::move(proposedPending);
            result.status = "PREPARE_MERGE_AWAITING_REDUNDANCY";
            return result;
        }

        const auto beforeAudit = exactAudit(
            state.certifiedRows, state.certifiedValues);
        result.persistentRankBefore =
            static_cast<int>(beforeAudit.hnf.basis.size());
        if (!beforeAudit.hnf.consistent)
        {
            result.status = "ABORT_PREVIOUS_STATE_INCONSISTENT";
            return result;
        }

        auto proposedRows = state.certifiedRows;
        proposedRows.insert(proposedRows.end(),
            candidateSparse.begin(), candidateSparse.end());
        auto proposedValues = state.certifiedValues;
        proposedValues.insert(proposedValues.end(),
            candidateValues.begin(), candidateValues.end());
        const auto proposedAudit = exactAudit(
            proposedRows, proposedValues);
        result.persistentRankAfter =
            static_cast<int>(proposedAudit.hnf.basis.size());
        result.persistentCycleClosureConsistent =
            proposedAudit.hnf.consistent;
        if (!proposedAudit.hnf.consistent)
        {
            result.status = "ABORT_PERSISTENT_CYCLE_CLOSURE_CONFLICT";
            return result;
        }

        ZhangProductRelationAdmissionState proposed = state;
        proposed.certifiedRows.clear();
        for (const auto& exactRow : proposedAudit.hnf.basis)
        {
            std::map<std::string, ZhangExactInteger> sparse;
            for (std::size_t column = 0;
                 column < exactRow.size(); column++)
            {
                if (exactRow[column] != 0)
                {
                    sparse[proposedAudit.columns[column]] = exactRow[column];
                }
            }
            proposed.certifiedRows.push_back(std::move(sparse));
        }
        proposed.certifiedValues = proposedAudit.hnf.values;
        for (const auto& candidate : auditCandidates)
        {
            if (proposed.certifiedSatellites.insert(candidate.satellite).second)
            {
                result.restoredSatellites++;
            }
            proposed.certifiedRelationIds.insert(candidate.relationId);
        }
        proposed.pendingCandidates.clear();
        state = std::move(proposed);
        result.committed = true;
        result.status = "CERTIFIED_NEW_RELATION";
        return result;
    }

    /** Validate a persisted admission state independently of archive syntax.
     * A checkpoint may be structurally decodable yet contain an inconsistent
     * HNF/value pair or a pending relation that bypasses a scientific gate. */
    static bool validateState(
        const ZhangProductRelationAdmissionState& state,
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
        if (state.certifiedRows.size() != state.certifiedValues.size())
        {
            return fail("PRODUCT_RELATION_STATE_ROW_VALUE_MISMATCH");
        }
        if (state.certifiedRelationIds.count("") != 0
            || state.certifiedSatellites.count("") != 0)
        {
            return fail("PRODUCT_RELATION_STATE_EMPTY_CERTIFIED_IDENTITY");
        }
        for (const auto& row : state.certifiedRows)
        {
            if (row.empty())
            {
                return fail("PRODUCT_RELATION_STATE_EMPTY_CERTIFIED_ROW");
            }
            for (const auto& [column, coefficient] : row)
            {
                if (column.empty() || coefficient == 0)
                {
                    return fail("PRODUCT_RELATION_STATE_INVALID_CERTIFIED_TERM");
                }
            }
        }
        const auto certifiedAudit = exactAudit(
            state.certifiedRows, state.certifiedValues);
        if (!certifiedAudit.hnf.consistent
            || certifiedAudit.hnf.basis.size() != state.certifiedRows.size())
        {
            return fail("PRODUCT_RELATION_STATE_CERTIFIED_HNF_INVALID");
        }
        if (!state.certifiedRows.empty()
            && (state.certifiedRelationIds.empty()
                || state.certifiedSatellites.empty()))
        {
            return fail("PRODUCT_RELATION_STATE_CERTIFIED_PROVENANCE_MISSING");
        }
        for (const auto& [relationId, candidate] : state.pendingCandidates)
        {
            if (relationId.empty() || candidate.relationId != relationId
                || candidate.satellite.empty() || candidate.observable.empty()
                || candidate.physicalCoefficients.empty()
                || state.certifiedRelationIds.count(relationId) != 0)
            {
                return fail("PRODUCT_RELATION_STATE_INVALID_PENDING_IDENTITY");
            }
            if (!candidate.exactIntegerEstimable
                || !candidate.phaseSegmentCompatible
                || !candidate.scalarReliabilityPassed
                || !candidate.jointNisPassed)
            {
                return fail("PRODUCT_RELATION_STATE_PENDING_GATE_BYPASS");
            }
            for (const auto& [column, coefficient] :
                 candidate.physicalCoefficients)
            {
                if (column.empty() || coefficient == 0)
                {
                    return fail("PRODUCT_RELATION_STATE_INVALID_PENDING_TERM");
                }
            }
        }
        for (const auto& [relationKey, confirmation] :
             state.temporalCertificateConfirmations)
        {
            if (relationKey.empty()
                || confirmation.canonicalRelationKey != relationKey
                || confirmation.consistentEpochs <= 0
                || confirmation.firstEpoch <= 0
                || confirmation.lastEpoch < confirmation.firstEpoch
                || confirmation.supportSignatures.count("") != 0)
            {
                return fail(
                    "PRODUCT_RELATION_STATE_INVALID_TEMPORAL_CONFIRMATION");
            }
        }
        if (failureReason)
        {
            failureReason->clear();
        }
        return true;
    }

private:
    struct ExactAudit
    {
        ZhangExactRowHnf hnf;
        std::vector<std::string> columns;
    };

    static std::vector<std::map<std::string, ZhangExactInteger>> sparseRows(
        const std::vector<ZhangProductRelationAdmissionCandidate>& rows)
    {
        std::vector<std::map<std::string, ZhangExactInteger>> result;
        result.reserve(rows.size());
        for (const auto& relation : rows)
        {
            std::map<std::string, ZhangExactInteger> sparse;
            for (const auto& [column, coefficient] :
                 relation.physicalCoefficients)
            {
                if (coefficient != 0)
                {
                    sparse[relation.observable + ":" + column] +=
                        coefficient;
                }
            }
            result.push_back(std::move(sparse));
        }
        return result;
    }

    static ZhangExactVector integerValues(
        const std::vector<ZhangProductRelationAdmissionCandidate>& rows)
    {
        ZhangExactVector result;
        result.reserve(rows.size());
        for (const auto& row : rows)
        {
            result.push_back(row.integerValue);
        }
        return result;
    }

    static ExactAudit exactAudit(
        const std::vector<std::map<std::string, ZhangExactInteger>>& rows,
        const ZhangExactVector& values)
    {
        ExactAudit result;
        if (rows.empty())
        {
            result.hnf.consistent = values.empty();
            return result;
        }
        if (rows.size() != values.size())
        {
            result.hnf.consistent = false;
            return result;
        }
        std::set<std::string> columnSet;
        for (const auto& row : rows)
        for (const auto& [column, coefficient] : row)
        {
            if (coefficient != 0)
            {
                columnSet.insert(column);
            }
        }
        result.columns.assign(columnSet.begin(), columnSet.end());
        std::map<std::string, std::size_t> columnIndex;
        for (std::size_t index = 0; index < result.columns.size(); index++)
        {
            columnIndex[result.columns[index]] = index;
        }

        ZhangExactMatrix matrix;
        matrix.reserve(rows.size());
        for (const auto& relation : rows)
        {
            ZhangExactVector row(result.columns.size());
            for (const auto& [column, coefficient] : relation)
            {
                row[columnIndex.at(column)] += coefficient;
            }
            matrix.push_back(std::move(row));
        }
        result.hnf = zhangExactRowHermiteNormalForm(matrix, values);
        return result;
    }
};
