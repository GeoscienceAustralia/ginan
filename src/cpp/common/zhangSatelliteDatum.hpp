#pragma once

#include <algorithm>
#include <cmath>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "common/enums.h"
#include "common/satSys.hpp"

/** Weighted disjoint-set forest with exact integer node potentials.
 *
 * The stored convention is potential(b) - potential(a) = difference for an
 * accepted relation (a,b,difference).  Contradictory cycle closures are
 * rejected without changing the forest.
 */
template<typename Node>
class IntegerPotentialUnionFind
{
public:
    void add(const Node& node)
    {
        if (parent.find(node) != parent.end())
        {
            return;
        }
        parent[node] = node;
        rank[node] = 0;
        potentialToParent[node] = 0;
    }

    bool contains(const Node& node) const
    {
        return parent.find(node) != parent.end();
    }

    std::pair<Node, long long> find(const Node& node)
    {
        add(node);
        Node directParent = parent.at(node);
        if (directParent == node)
        {
            return {node, 0};
        }
        auto [root, parentPotential] = find(directParent);
        long long potential = potentialToParent.at(node) + parentPotential;
        parent[node] = root;
        potentialToParent[node] = potential;
        return {root, potential};
    }

    bool relate(const Node& a, const Node& b, long long difference)
    {
        auto [rootA, potentialA] = find(a);
        auto [rootB, potentialB] = find(b);
        if (rootA == rootB)
        {
            return potentialB - potentialA == difference;
        }

        // If rootB is attached below rootA:
        // p(rootB)-p(rootA)=difference-potentialB+potentialA.
        long long rootBDifference = difference - potentialB + potentialA;
        if (rank.at(rootA) < rank.at(rootB))
        {
            parent[rootA] = rootB;
            potentialToParent[rootA] = -rootBDifference;
        }
        else
        {
            parent[rootB] = rootA;
            potentialToParent[rootB] = rootBDifference;
            if (rank.at(rootA) == rank.at(rootB))
            {
                rank[rootA]++;
            }
        }
        return true;
    }

    bool difference(const Node& a, const Node& b, long long& value)
    {
        if (!contains(a) || !contains(b))
        {
            return false;
        }
        auto [rootA, potentialA] = find(a);
        auto [rootB, potentialB] = find(b);
        if (rootA != rootB)
        {
            return false;
        }
        value = potentialB - potentialA;
        return true;
    }

    std::set<Node> component(const Node& node)
    {
        std::set<Node> result;
        if (!contains(node))
        {
            return result;
        }
        Node root = find(node).first;
        for (const auto& [candidate, ignored] : parent)
        {
            if (find(candidate).first == root)
            {
                result.insert(candidate);
            }
        }
        return result;
    }

private:
    std::map<Node, Node>      parent;
    std::map<Node, int>       rank;
    std::map<Node, long long> potentialToParent;
};

struct ZhangSatellitePhaseSegment
{
    SatSys satellite;
    int    segment = 0;

    bool operator<(const ZhangSatellitePhaseSegment& other) const
    {
        return std::tie(satellite, segment) <
               std::tie(other.satellite, other.segment);
    }

    bool operator==(const ZhangSatellitePhaseSegment& other) const
    {
        return satellite == other.satellite && segment == other.segment;
    }
};

struct ZhangSatelliteDatumRelation
{
    ZhangSatellitePhaseSegment a;
    ZhangSatellitePhaseSegment b;
    long long                   difference = 0;
    bool                        promoted = true;
    std::string                 provenance;
};

struct ZhangSatelliteDatumStatus
{
    bool        integerStructureValid = false;
    bool        integerDatumContinuous = false;
    bool        integerPrecisionValid = false;
    bool        integerValid = false;
    long long   alignmentCycles = 0;
    int         phaseSegment = 0;
    int         discontinuityCounter = 0;
    int         datumVersion = 0;
    std::size_t componentSize = 0;
    std::string componentId = "UNRESOLVED";
};

enum class ZhangCurrentAlignmentState
{
    PERSISTENT_RELATION_KNOWN,
    CURRENT_ALIGNMENT_VALID,
    CURRENT_ALIGNMENT_PENDING,
    CURRENT_ALIGNMENT_LOST
};

enum class ZhangProductRelationEventType
{
    PENDING_CONFIRMATION,
    NEW_COMPONENT_EDGE,
    COMPONENT_MERGE,
    REDUNDANT_CONFIRMATION,
    CURRENT_ALIGNMENT_QUARANTINED,
    CURRENT_REALIGNMENT,
    CONFLICT_REJECTED
};

struct ZhangProductRelationEvent
{
    ZhangProductRelationEventType type =
        ZhangProductRelationEventType::CONFLICT_REJECTED;
    bool        accepted = false;
    std::size_t oldComponentSizeA = 0;
    std::size_t oldComponentSizeB = 0;
    std::size_t newComponentSize = 0;
    int         confirmationCount = 0;
    int         confirmationRequired = 0;
    SatSys      quarantinedSatellite;
};

struct ZhangSatelliteDatumComponent
{
    std::string      id;
    std::set<SatSys> satellites;
    std::set<SatSys> alignedSatellites;
};

inline const char* zhangProductRelationEventName(
    ZhangProductRelationEventType type
)
{
    switch (type)
    {
        case ZhangProductRelationEventType::PENDING_CONFIRMATION:
            return "PENDING_CONFIRMATION";
        case ZhangProductRelationEventType::NEW_COMPONENT_EDGE:
            return "NEW_COMPONENT_EDGE";
        case ZhangProductRelationEventType::COMPONENT_MERGE:
            return "COMPONENT_MERGE";
        case ZhangProductRelationEventType::REDUNDANT_CONFIRMATION:
            return "REDUNDANT_CONFIRMATION";
        case ZhangProductRelationEventType::CURRENT_ALIGNMENT_QUARANTINED:
            return "CURRENT_ALIGNMENT_QUARANTINED";
        case ZhangProductRelationEventType::CURRENT_REALIGNMENT:
            return "CURRENT_REALIGNMENT";
        case ZhangProductRelationEventType::CONFLICT_REJECTED:
            return "CONFLICT_REJECTED";
    }
    return "CONFLICT_REJECTED";
}

inline const char* zhangCurrentAlignmentStateName(
    ZhangCurrentAlignmentState state
)
{
    switch (state)
    {
        case ZhangCurrentAlignmentState::PERSISTENT_RELATION_KNOWN:
            return "PERSISTENT_RELATION_KNOWN";
        case ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_VALID:
            return "CURRENT_ALIGNMENT_VALID";
        case ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_PENDING:
            return "CURRENT_ALIGNMENT_PENDING";
        case ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_LOST:
            return "CURRENT_ALIGNMENT_LOST";
    }
    return "CURRENT_ALIGNMENT_LOST";
}

/** Persistent satellite-product integer datum, independent of physical
 * receiver-satellite ambiguity-arc lifetimes.
 *
 * Relations are retained after their source rows retire.  Exact dynamic-tree
 * coordinate changes update relation values and alpha alignments but do not
 * change component identity or datum version.  Only an explicit satellite
 * phase discontinuity creates a new segment/version.
 */
class ZhangSatelliteDatumManager
{
public:
    explicit ZhangSatelliteDatumManager(
        E_Sys system = E_Sys::NONE,
        E_ObsCode observable = E_ObsCode::NONE
    ) : system(system), observable(observable) {}

    ZhangSatellitePhaseSegment currentNode(const SatSys& satellite)
    {
        int& segment = currentSegments[satellite];
        ZhangSatellitePhaseSegment node{satellite, segment};
        ensure(node);
        return node;
    }

    bool promoteRelation(
        const SatSys&      a,
        const SatSys&      b,
        long long          difference,
        const std::string& provenance,
        bool               promoted = true
    )
    {
        return promoteRelationDetailed(
            a, b, difference, provenance, promoted
        ).accepted;
    }

    ZhangProductRelationEvent promoteRelationDetailed(
        const SatSys&      a,
        const SatSys&      b,
        long long          difference,
        const std::string& provenance,
        bool               promoted = true
    )
    {
        auto nodeA = currentNode(a);
        auto nodeB = currentNode(b);
        auto componentA = forest.component(nodeA);
        auto componentB = forest.component(nodeB);
        ZhangSatellitePhaseSegment alignmentReference = nodeA;
        if (componentB.size() > componentA.size() ||
            (componentB.size() == componentA.size() && nodeB < nodeA))
        {
            alignmentReference = nodeB;
        }
        long long referenceAlignment = alignmentCycles[alignmentReference];
        ZhangProductRelationEvent event;
        event.oldComponentSizeA = componentA.size();
        event.oldComponentSizeB = componentB.size();
        long long existing = 0;
        if (forest.difference(nodeA, nodeB, existing))
        {
            if (existing != difference)
            {
                conflictCount++;
                event.type = ZhangProductRelationEventType::CONFLICT_REJECTED;
                return event;
            }
            precisionValid.insert(nodeA);
            precisionValid.insert(nodeB);
            // Confirming an already-known persistent relation must not also
            // change current alpha alignment.  Current-state recovery is a
            // separately gated operation (realignRelation), which keeps the
            // bridge-only and relink-only experiments causally separable.
            event.type = ZhangProductRelationEventType::REDUNDANT_CONFIRMATION;
            event.accepted = true;
            event.newComponentSize = componentA.size();
            eventCounts[event.type]++;
            return event;
        }

        if (!forest.relate(nodeA, nodeB, difference))
        {
            conflictCount++;
            event.type = ZhangProductRelationEventType::CONFLICT_REJECTED;
            return event;
        }
        relations.push_back({nodeA, nodeB, difference, promoted, provenance});
        precisionValid.insert(nodeA);
        precisionValid.insert(nodeB);
        // Preserve the established (normally larger) component's alpha
        // alignment when a singleton or smaller component is attached.  The
        // old nodeA-only rule could re-anchor an entire product component to
        // a newly arriving satellite and create a large common product jump.
        alignFrom(alignmentReference, referenceAlignment);
        event.type = componentA.size() > 1 && componentB.size() > 1
            ? ZhangProductRelationEventType::COMPONENT_MERGE
            : ZhangProductRelationEventType::NEW_COMPONENT_EDGE;
        event.accepted = true;
        event.newComponentSize = componentA.size() + componentB.size();
        eventCounts[event.type]++;
        return event;
    }

    ZhangProductRelationEvent quarantineCurrentAlignment(
        const SatSys& a,
        const SatSys& b,
        const SatSys& trustedAnchor = SatSys()
    )
    {
        auto nodeA = currentNode(a);
        auto nodeB = currentNode(b);
        auto members = forest.component(nodeA);
        ZhangProductRelationEvent event;
        event.oldComponentSizeA = members.size();
        event.oldComponentSizeB = members.size();
        event.newComponentSize = members.size();

        if (members.find(nodeB) == members.end())
        {
            return event;
        }

        auto isAligned = [&](const auto& node)
        {
            return alignmentKnown.find(node) != alignmentKnown.end();
        };
        ZhangSatellitePhaseSegment suspect;
        bool found = false;
        if (isAligned(nodeA) && !isAligned(nodeB))
        {
            suspect = nodeB;
            found = true;
        }
        else if (!isAligned(nodeA) && isAligned(nodeB))
        {
            suspect = nodeA;
            found = true;
        }
        else if (trustedAnchor == a && isAligned(nodeA))
        {
            suspect = nodeB;
            found = true;
        }
        else if (trustedAnchor == b && isAligned(nodeB))
        {
            suspect = nodeA;
            found = true;
        }
        else
        {
            const auto& componentAnchor = *members.begin();
            if (nodeA == componentAnchor && isAligned(nodeA))
            {
                suspect = nodeB;
                found = true;
            }
            else if (nodeB == componentAnchor && isAligned(nodeB))
            {
                suspect = nodeA;
                found = true;
            }
        }

        if (!found)
        {
            conflictCount++;
            eventCounts[ZhangProductRelationEventType::CONFLICT_REJECTED]++;
            return event;
        }

        alignmentKnown.erase(suspect);
        precisionValid.erase(suspect);
        event.type =
            ZhangProductRelationEventType::CURRENT_ALIGNMENT_QUARANTINED;
        event.quarantinedSatellite = suspect.satellite;
        eventCounts[event.type]++;
        return event;
    }

    ZhangProductRelationEvent realignRelation(
        const SatSys&      anchor,
        const SatSys&      satellite,
        long long          currentDifference,
        const std::string& provenance
    )
    {
        auto anchorNode = currentNode(anchor);
        auto satelliteNode = currentNode(satellite);
        ZhangProductRelationEvent event;
        auto members = forest.component(anchorNode);
        event.oldComponentSizeA = members.size();
        event.oldComponentSizeB = members.size();
        event.newComponentSize = members.size();
        long long existing = 0;
        if (!forest.difference(anchorNode, satelliteNode, existing) ||
            alignmentKnown.find(anchorNode) == alignmentKnown.end())
        {
            conflictCount++;
            eventCounts[ZhangProductRelationEventType::CONFLICT_REJECTED]++;
            return event;
        }

        if (alignmentKnown.find(satelliteNode) != alignmentKnown.end())
        {
            if (existing != currentDifference)
            {
                conflictCount++;
                eventCounts[ZhangProductRelationEventType::CONFLICT_REJECTED]++;
                return event;
            }
        }
        else
        {
            applyDynamicTreeShift(
                satellite, currentDifference - existing
            );
            satelliteNode = currentNode(satellite);
            alignmentKnown.insert(satelliteNode);
        }
        precisionValid.insert(satelliteNode);
        event.type = ZhangProductRelationEventType::CURRENT_REALIGNMENT;
        event.accepted = true;
        eventCounts[event.type]++;
        return event;
    }

    /** Apply alpha_new-alpha_old for one satellite after an exact S-transform. */
    void applyDynamicTreeShift(const SatSys& satellite, long long cycleShift)
    {
        auto node = currentNode(satellite);
        alignmentCycles[node] += cycleShift;
        for (auto& relation : relations)
        {
            if (relation.a == node)
            {
                relation.difference -= cycleShift;
            }
            if (relation.b == node)
            {
                relation.difference += cycleShift;
            }
        }
        rebuildForest();
    }

    /** Apply one exact dynamic S-transform as a component operation.
     *
     * A common real-valued shift of every satellite in a component is the
     * unobservable satellite-phase gauge and does not change its integer
     * single differences.  Only relative shifts must be integer.  Processing
     * the batch avoids falsely declaring every satellite discontinuous when a
     * tree exchange produces the same fractional gauge shift for all of them.
     */
    std::map<SatSys, bool> applyDynamicTreeTransform(
        const std::map<SatSys, double>& cycleChanges,
        double                         integerTolerance = 1e-8
    )
    {
        std::map<SatSys, bool> preserved;
        std::set<ZhangSatellitePhaseSegment> visited;
        for (const auto& [satellite, ignored] : cycleChanges)
        {
            auto node = currentNode(satellite);
            if (visited.find(node) != visited.end())
            {
                continue;
            }
            auto members = forest.component(node);
            visited.insert(members.begin(), members.end());

            std::vector<ZhangSatellitePhaseSegment> transformed;
            for (const auto& member : members)
            {
                if (cycleChanges.find(member.satellite) != cycleChanges.end())
                {
                    transformed.push_back(member);
                }
            }
            if (transformed.empty())
            {
                continue;
            }
            const double common =
                cycleChanges.at(transformed.front().satellite);
            std::map<SatSys, long long> relativeShifts;
            for (const auto& member : transformed)
            {
                double relative = cycleChanges.at(member.satellite) - common;
                long long integer = std::llround(relative);
                if (std::abs(relative - integer) > integerTolerance)
                {
                    // A non-integer relative change invalidates only this
                    // member's current alpha.  Keep the component's exact
                    // persistent relation and every still-aligned member so
                    // that the satellite can subsequently be relinked to a
                    // valid anchor in the same product component.
                    alignmentKnown.erase(member);
                    preserved[member.satellite] = false;
                    continue;
                }
                relativeShifts[member.satellite] = integer;
            }
            for (const auto& [memberSatellite, shift] : relativeShifts)
            {
                if (shift != 0)
                {
                    applyDynamicTreeShift(memberSatellite, shift);
                }
                preserved[memberSatellite] = true;
            }
        }
        return preserved;
    }

    /** A dynamic local reset does not erase promoted satellite facts. */
    void markDynamicAlignmentUnknown(const std::set<SatSys>& satellites)
    {
        for (const auto& satellite : satellites)
        {
            auto node = currentNode(satellite);
            auto members = forest.component(node);
            bool hasPromotedFact = std::any_of(
                relations.begin(), relations.end(),
                [&](const auto& relation)
                {
                    return relation.promoted &&
                        (relation.a == node || relation.b == node);
                }
            );
            if (!hasPromotedFact)
            {
                alignmentKnown.erase(node);
            }
        }
    }

    /** Remove only provisional support bridges crossing a detached set. */
    void retireUnprovedBridges(const std::set<SatSys>& detached)
    {
        relations.erase(
            std::remove_if(
                relations.begin(), relations.end(),
                [&](const auto& relation)
                {
                    bool left = detached.find(relation.a.satellite) != detached.end();
                    bool right = detached.find(relation.b.satellite) != detached.end();
                    return !relation.promoted && left != right;
                }
            ),
            relations.end()
        );
        rebuildForest();
    }

    void recordSatelliteDiscontinuity(const SatSys& satellite)
    {
        int& segment = currentSegments[satellite];
        segment++;
        discontinuityCounters[satellite]++;
        datumVersions[satellite]++;
        ensure({satellite, segment});
    }

    ZhangSatelliteDatumStatus status(
        const SatSys& satellite,
        bool          structureValid
    )
    {
        auto node = currentNode(satellite);
        auto members = forest.component(node);
        ZhangSatelliteDatumStatus result;
        result.integerStructureValid = structureValid;
        result.integerDatumContinuous =
            members.size() >= 2 && alignmentKnown.find(node) != alignmentKnown.end();
        result.integerPrecisionValid =
            precisionValid.find(node) != precisionValid.end();
        result.integerValid =
            result.integerStructureValid &&
            result.integerDatumContinuous &&
            result.integerPrecisionValid;
        result.alignmentCycles = alignmentCycles[node];
        result.phaseSegment = node.segment;
        result.discontinuityCounter = discontinuityCounters[satellite];
        result.datumVersion = datumVersions[satellite];
        result.componentSize = members.size();
        if (members.size() >= 2)
        {
            const auto& anchor = *members.begin();
            result.componentId = enum_to_string(system) + "-" +
                enum_to_string(observable) + "-P" +
                std::to_string(anchor.satellite.prn) +
                "-S" + std::to_string(anchor.segment);
        }
        return result;
    }

    bool relation(
        const SatSys& a,
        const SatSys& b,
        long long&    difference
    )
    {
        return forest.difference(currentNode(a), currentNode(b), difference);
    }

    std::size_t relationCount() const { return relations.size(); }
    std::size_t conflicts() const { return conflictCount; }

    ZhangCurrentAlignmentState alignmentState(const SatSys& satellite)
    {
        auto node = currentNode(satellite);
        auto members = forest.component(node);
        if (members.size() < 2)
        {
            return ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_PENDING;
        }
        if (alignmentKnown.find(node) != alignmentKnown.end())
        {
            return ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_VALID;
        }
        bool anyAligned = std::any_of(
            members.begin(), members.end(),
            [&](const auto& member)
            {
                return alignmentKnown.find(member) != alignmentKnown.end();
            }
        );
        return anyAligned
            ? ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_PENDING
            : ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_LOST;
    }

    std::vector<ZhangSatelliteDatumComponent> components()
    {
        std::vector<ZhangSatelliteDatumComponent> result;
        std::set<ZhangSatellitePhaseSegment> visited;
        for (const auto& [satellite, segment] : currentSegments)
        {
            ZhangSatellitePhaseSegment node{satellite, segment};
            if (visited.find(node) != visited.end())
            {
                continue;
            }
            auto members = forest.component(node);
            visited.insert(members.begin(), members.end());
            if (members.size() < 2)
            {
                continue;
            }
            ZhangSatelliteDatumComponent component;
            component.id = status(satellite, false).componentId;
            for (const auto& member : members)
            {
                component.satellites.insert(member.satellite);
                if (alignmentKnown.find(member) != alignmentKnown.end())
                {
                    component.alignedSatellites.insert(member.satellite);
                }
            }
            result.push_back(std::move(component));
        }
        return result;
    }

    std::size_t eventCount(ZhangProductRelationEventType type) const
    {
        auto found = eventCounts.find(type);
        return found == eventCounts.end() ? 0 : found->second;
    }

private:
    void ensure(const ZhangSatellitePhaseSegment& node)
    {
        if (forest.contains(node))
        {
            return;
        }
        forest.add(node);
        alignmentCycles[node] = 0;
        alignmentKnown.insert(node); // alpha=0 at component birth
    }

    void alignFrom(
        const ZhangSatellitePhaseSegment& reference,
        long long                         referenceAlignment
    )
    {
        auto members = forest.component(reference);
        for (const auto& member : members)
        {
            long long difference = 0;
            forest.difference(reference, member, difference);
            alignmentCycles[member] = referenceAlignment + difference;
            alignmentKnown.insert(member);
        }
    }

    void rebuildForest()
    {
        IntegerPotentialUnionFind<ZhangSatellitePhaseSegment> rebuilt;
        for (const auto& [satellite, segment] : currentSegments)
        {
            rebuilt.add({satellite, segment});
        }
        for (const auto& relation : relations)
        {
            rebuilt.add(relation.a);
            rebuilt.add(relation.b);
            if (!rebuilt.relate(relation.a, relation.b, relation.difference))
            {
                conflictCount++;
            }
        }
        forest = std::move(rebuilt);
    }

    E_Sys     system = E_Sys::NONE;
    E_ObsCode observable = E_ObsCode::NONE;
    IntegerPotentialUnionFind<ZhangSatellitePhaseSegment> forest;
    std::map<SatSys, int> currentSegments;
    std::map<SatSys, int> discontinuityCounters;
    std::map<SatSys, int> datumVersions;
    std::map<ZhangSatellitePhaseSegment, long long> alignmentCycles;
    std::set<ZhangSatellitePhaseSegment> alignmentKnown;
    std::set<ZhangSatellitePhaseSegment> precisionValid;
    std::vector<ZhangSatelliteDatumRelation> relations;
    std::map<ZhangProductRelationEventType, std::size_t> eventCounts;
    std::size_t conflictCount = 0;
};
