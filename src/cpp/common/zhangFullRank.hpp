#pragma once

#include <algorithm>
#include <map>
#include <queue>
#include <set>
#include <string>
#include <tuple>
#include <vector>
#include "common/enums.h"
#include "common/satSys.hpp"

/** One receiver-satellite edge in the Zhang phase/ambiguity bipartite graph. */
struct ZhangGraphEdge
{
    std::string receiver;
    SatSys      satellite;

    bool operator<(const ZhangGraphEdge& other) const
    {
        return std::tie(receiver, satellite) <
               std::tie(other.receiver, other.satellite);
    }

    bool operator==(const ZhangGraphEdge& other) const
    {
        return receiver == other.receiver && satellite == other.satellite;
    }
};

/** A rooted spanning-tree S-basis for one constellation.
 *
 * The graph is the common-valid-observable graph of the two configured
 * baseline frequencies.  Tree-edge ambiguities are S-bases and are absorbed
 * into receiver/satellite phase states.  Every non-tree edge represents one
 * independent integer fundamental-cycle ambiguity.
 */
struct ZhangGraphBasis
{
    std::string               rootReceiver;
    std::set<ZhangGraphEdge>  edges;
    std::set<ZhangGraphEdge>  treeEdges;
    std::set<std::string>     receivers;
    std::set<SatSys>          satellites;
    int                       componentCount = 0;
    bool                      connected      = false;

    bool models(const std::string& receiver, const SatSys& satellite) const
    {
        return edges.find({receiver, satellite}) != edges.end();
    }

    bool isTreeEdge(const std::string& receiver, const SatSys& satellite) const
    {
        return treeEdges.find({receiver, satellite}) != treeEdges.end();
    }
};

namespace zhang_graph_detail
{
inline std::string receiverNode(const std::string& receiver)
{
    return "R:" + receiver;
}

inline std::string satelliteNode(const SatSys& satellite)
{
    return
        "S:" + std::to_string(static_cast<int>(satellite.sys)) + ":" +
        std::to_string(satellite.prn);
}

struct DisjointSet
{
    std::map<std::string, std::string> parent;

    std::string find(const std::string& node)
    {
        auto [it, inserted] = parent.emplace(node, node);
        if (inserted || it->second == node)
        {
            return node;
        }

        it->second = find(it->second);
        return it->second;
    }

    bool unite(const std::string& left, const std::string& right)
    {
        std::string leftRoot  = find(left);
        std::string rightRoot = find(right);
        if (leftRoot == rightRoot)
        {
            return false;
        }

        parent[rightRoot] = leftRoot;
        return true;
    }
};
}  // namespace zhang_graph_detail

/** Retain only the connected component containing rootReceiver. */
inline std::set<ZhangGraphEdge> zhangRootComponentEdges(
    const std::set<ZhangGraphEdge>& edges,
    const std::string&              rootReceiver
)
{
    std::set<std::string> receivers;
    std::set<SatSys>      satellites;
    receivers.insert(rootReceiver);

    bool changed = true;
    while (changed)
    {
        changed = false;
        for (const auto& edge : edges)
        {
            if (receivers.find(edge.receiver) != receivers.end() &&
                satellites.insert(edge.satellite).second)
            {
                changed = true;
            }

            if (satellites.find(edge.satellite) != satellites.end() &&
                receivers.insert(edge.receiver).second)
            {
                changed = true;
            }
        }
    }

    std::set<ZhangGraphEdge> component;
    for (const auto& edge : edges)
    {
        if (receivers.find(edge.receiver) != receivers.end() &&
            satellites.find(edge.satellite) != satellites.end())
        {
            component.insert(edge);
        }
    }
    return component;
}

/** Construct a deterministic maximum-quality spanning tree.
 *
 * Existing tree edges are preferred first so a stable valid basis does not
 * churn between epochs.  Represented historical edges and longer continuous
 * arcs are preferred next, followed by root incidence, quality and lexical
 * ordering.  If the input graph is disconnected, the returned tree is a
 * spanning forest and connected is false.
 */
inline ZhangGraphBasis zhangBuildSpanningTree(
    const std::set<ZhangGraphEdge>&         edges,
    const std::string&                     rootReceiver,
    const std::set<ZhangGraphEdge>&         preferredEdges = {},
    const std::map<ZhangGraphEdge, double>& quality         = {},
    const std::set<ZhangGraphEdge>&         representedEdges = {},
    const std::map<ZhangGraphEdge, int>&    persistence      = {}
)
{
    using namespace zhang_graph_detail;

    ZhangGraphBasis result;
    result.rootReceiver = rootReceiver;
    result.edges        = edges;

    for (const auto& edge : edges)
    {
        result.receivers.insert(edge.receiver);
        result.satellites.insert(edge.satellite);
    }

    std::vector<ZhangGraphEdge> ordered(edges.begin(), edges.end());
    std::sort(
        ordered.begin(),
        ordered.end(),
        [&](const auto& left, const auto& right)
        {
            bool leftPreferred  = preferredEdges.find(left) != preferredEdges.end();
            bool rightPreferred = preferredEdges.find(right) != preferredEdges.end();
            if (leftPreferred != rightPreferred)
            {
                return leftPreferred > rightPreferred;
            }

            bool leftRepresented =
                representedEdges.find(left) != representedEdges.end();
            bool rightRepresented =
                representedEdges.find(right) != representedEdges.end();
            if (leftRepresented != rightRepresented)
            {
                return leftRepresented > rightRepresented;
            }

            auto leftPersistence  = persistence.find(left);
            auto rightPersistence = persistence.find(right);
            int leftArc =
                leftPersistence == persistence.end() ? 0 : leftPersistence->second;
            int rightArc =
                rightPersistence == persistence.end() ? 0 : rightPersistence->second;
            if (leftArc != rightArc)
            {
                return leftArc > rightArc;
            }

            bool leftRoot  = left.receiver == rootReceiver;
            bool rightRoot = right.receiver == rootReceiver;
            if (leftRoot != rightRoot)
            {
                return leftRoot > rightRoot;
            }

            auto leftQuality  = quality.find(left);
            auto rightQuality = quality.find(right);
            double leftScore  = leftQuality == quality.end() ? 0 : leftQuality->second;
            double rightScore = rightQuality == quality.end() ? 0 : rightQuality->second;
            if (leftScore != rightScore)
            {
                return leftScore > rightScore;
            }

            return left < right;
        }
    );

    DisjointSet components;
    for (const auto& edge : ordered)
    {
        std::string receiver  = receiverNode(edge.receiver);
        std::string satellite = satelliteNode(edge.satellite);
        components.find(receiver);
        components.find(satellite);
        if (components.unite(receiver, satellite))
        {
            result.treeEdges.insert(edge);
        }
    }

    std::set<std::string> roots;
    for (const auto& [node, parent] : components.parent)
    {
        roots.insert(components.find(node));
    }
    result.componentCount = static_cast<int>(roots.size());

    const int nodeCount =
        static_cast<int>(result.receivers.size() + result.satellites.size());
    result.connected =
        nodeCount > 0 &&
        result.componentCount == 1 &&
        static_cast<int>(result.treeEdges.size()) == nodeCount - 1;
    return result;
}

/** Integer coefficients of the fundamental cycle formed by nonTreeEdge.
 *
 * Coefficient +1 belongs to the non-tree edge.  Tree-path coefficients are
 * +/-1 according to the common receiver-to-satellite orientation.  The result
 * is empty when the supplied tree does not connect the edge endpoints.
 */
inline std::map<ZhangGraphEdge, int> zhangFundamentalCycle(
    const ZhangGraphBasis& basis,
    const ZhangGraphEdge&  nonTreeEdge
)
{
    using namespace zhang_graph_detail;

    struct Step
    {
        std::string    next;
        ZhangGraphEdge edge;
        int            direction = 0;
    };

    std::map<std::string, std::vector<Step>> adjacency;
    for (const auto& edge : basis.treeEdges)
    {
        std::string receiver  = receiverNode(edge.receiver);
        std::string satellite = satelliteNode(edge.satellite);
        adjacency[receiver].push_back({satellite, edge, +1});
        adjacency[satellite].push_back({receiver, edge, -1});
    }

    const std::string start  = receiverNode(nonTreeEdge.receiver);
    const std::string target = satelliteNode(nonTreeEdge.satellite);

    struct Parent
    {
        std::string    previous;
        ZhangGraphEdge edge;
        int            direction = 0;
    };

    std::queue<std::string> queue;
    std::set<std::string>   visited = {start};
    std::map<std::string, Parent> parent;
    queue.push(start);

    while (!queue.empty() && visited.find(target) == visited.end())
    {
        std::string node = queue.front();
        queue.pop();

        for (const auto& step : adjacency[node])
        {
            if (!visited.insert(step.next).second)
            {
                continue;
            }

            parent[step.next] = {node, step.edge, step.direction};
            queue.push(step.next);
        }
    }

    if (visited.find(target) == visited.end())
    {
        return {};
    }

    std::map<ZhangGraphEdge, int> cycle;
    cycle[nonTreeEdge] = +1;

    std::string node = target;
    while (node != start)
    {
        const Parent& step = parent.at(node);
        cycle[step.edge]   = -step.direction;
        node               = step.previous;
    }

    return cycle;
}

inline bool zhangFullRankUsesObservable(
    E_ObsCode                    code,
    const std::vector<E_ObsCode>& baselineObservables
)
{
    return std::find(baselineObservables.begin(), baselineObservables.end(), code) !=
           baselineObservables.end();
}

inline bool zhangFullRankIsReferenceReceiver(
    const std::string& receiver,
    const std::string& referenceReceiver
)
{
    return !referenceReceiver.empty() && receiver == referenceReceiver;
}

inline bool zhangFullRankIsReferenceSatellite(
    const SatSys&      satellite,
    const std::string& referenceSatellite
)
{
    if (referenceSatellite.empty())
    {
        return false;
    }

    SatSys reference(referenceSatellite.c_str());
    return satellite.sys == reference.sys && satellite.prn == reference.prn;
}

/** The retained ambiguity is the receiver/satellite double-difference interior.
 *
 * The reference receiver row and reference satellite column are S-bases and
 * are absorbed by the receiver/satellite phase-bias states.
 */
inline bool zhangFullRankRetainsAmbiguity(
    const std::string& receiver,
    const SatSys&      satellite,
    E_ObsCode          code,
    const std::vector<E_ObsCode>& baselineObservables,
    const std::string& referenceReceiver,
    const std::string& referenceSatellite
)
{
    if (!zhangFullRankUsesObservable(code, baselineObservables))
    {
        return false;
    }

    return !zhangFullRankIsReferenceReceiver(receiver, referenceReceiver) &&
           !zhangFullRankIsReferenceSatellite(satellite, referenceSatellite);
}
