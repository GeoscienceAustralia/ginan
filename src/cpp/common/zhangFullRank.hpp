#pragma once

#include <algorithm>
#include <limits>
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

/** Build a shallow product tree rooted at the product reference receiver.
 *
 * Kruskal's algorithm preserves edges well but does not control how many
 * satellite product paths depend on one non-root edge.  A single internal
 * receiver arc can consequently reset a large satellite subtree.  Breadth-
 * first discovery gives every node its shortest-hop path from the root;
 * persistence and quality only resolve choices within the same depth.  The
 * result is still an ordinary spanning-tree S-basis.
 */
inline ZhangGraphBasis zhangBuildRootedProductTree(
    const std::set<ZhangGraphEdge>&         edges,
    const std::string&                      rootReceiver,
    const std::set<ZhangGraphEdge>&         preferredEdges = {},
    const std::map<ZhangGraphEdge, double>& quality = {},
    const std::set<ZhangGraphEdge>&         representedEdges = {},
    const std::map<ZhangGraphEdge, int>&    persistence = {})
{
    using namespace zhang_graph_detail;

    ZhangGraphBasis result = zhangBuildSpanningTree(edges, rootReceiver);
    result.treeEdges.clear();
    const std::string root = receiverNode(rootReceiver);
    if (rootReceiver.empty() || result.receivers.find(rootReceiver) ==
            result.receivers.end())
    {
        result.connected = false;
        return result;
    }

    struct Neighbour
    {
        std::string    node;
        ZhangGraphEdge edge;
    };
    std::map<std::string, std::vector<Neighbour>> adjacency;
    for (const auto& edge : edges)
    {
        const std::string receiver = receiverNode(edge.receiver);
        const std::string satellite = satelliteNode(edge.satellite);
        adjacency[receiver].push_back({satellite, edge});
        adjacency[satellite].push_back({receiver, edge});
    }
    auto preferred = [&](const ZhangGraphEdge& edge)
    {
        return preferredEdges.find(edge) != preferredEdges.end();
    };
    auto represented = [&](const ZhangGraphEdge& edge)
    {
        return representedEdges.find(edge) != representedEdges.end();
    };
    auto persistent = [&](const ZhangGraphEdge& edge)
    {
        auto found = persistence.find(edge);
        return found == persistence.end() ? 0 : found->second;
    };
    auto qualityScore = [&](const ZhangGraphEdge& edge)
    {
        auto found = quality.find(edge);
        return found == quality.end() ? 0.0 : found->second;
    };
    for (auto& [node, neighbours] : adjacency)
    {
        std::sort(neighbours.begin(), neighbours.end(),
            [&](const Neighbour& left, const Neighbour& right)
            {
                if (preferred(left.edge) != preferred(right.edge))
                    return preferred(left.edge) > preferred(right.edge);
                if (represented(left.edge) != represented(right.edge))
                    return represented(left.edge) > represented(right.edge);
                if (persistent(left.edge) != persistent(right.edge))
                    return persistent(left.edge) > persistent(right.edge);
                if (qualityScore(left.edge) != qualityScore(right.edge))
                    return qualityScore(left.edge) > qualityScore(right.edge);
                if (!(left.edge == right.edge))
                    return left.edge < right.edge;
                return left.node < right.node;
            });
    }

    std::queue<std::string> pending;
    std::set<std::string> visited = {root};
    pending.push(root);
    while (!pending.empty())
    {
        const std::string node = pending.front();
        pending.pop();
        for (const auto& neighbour : adjacency[node])
        {
            if (!visited.insert(neighbour.node).second)
            {
                continue;
            }
            result.treeEdges.insert(neighbour.edge);
            pending.push(neighbour.node);
        }
    }
    const int nodeCount = static_cast<int>(
        result.receivers.size() + result.satellites.size());
    result.connected =
        nodeCount > 0 &&
        static_cast<int>(visited.size()) == nodeCount &&
        static_cast<int>(result.treeEdges.size()) == nodeCount - 1;
    return result;
}

/** Number of satellite root paths that depend on each product-tree edge. */
inline std::map<ZhangGraphEdge, int> zhangProductTreeSatellitePathLoads(
    const ZhangGraphBasis& basis)
{
    using namespace zhang_graph_detail;
    struct Parent
    {
        std::string    node;
        ZhangGraphEdge edge;
    };
    std::map<std::string,
        std::vector<std::pair<std::string, ZhangGraphEdge>>> adjacency;
    for (const auto& edge : basis.treeEdges)
    {
        const std::string receiver = receiverNode(edge.receiver);
        const std::string satellite = satelliteNode(edge.satellite);
        adjacency[receiver].push_back({satellite, edge});
        adjacency[satellite].push_back({receiver, edge});
    }
    const std::string root = receiverNode(basis.rootReceiver);
    std::queue<std::string> pending;
    std::set<std::string> visited = {root};
    std::map<std::string, Parent> parent;
    pending.push(root);
    while (!pending.empty())
    {
        const std::string node = pending.front();
        pending.pop();
        for (const auto& [next, edge] : adjacency[node])
        {
            if (!visited.insert(next).second)
            {
                continue;
            }
            parent[next] = {node, edge};
            pending.push(next);
        }
    }

    std::map<ZhangGraphEdge, int> loads;
    for (const SatSys& satellite : basis.satellites)
    {
        std::string node = satelliteNode(satellite);
        while (node != root)
        {
            auto found = parent.find(node);
            if (found == parent.end())
            {
                return {};
            }
            loads[found->second.edge]++;
            node = found->second.node;
        }
    }
    return loads;
}

struct ZhangProductReceiverCore
{
    bool connected = false;
    int minimumSatelliteSupport = 0;
    std::set<std::string> receivers;
    std::set<SatSys> satellites;
    std::set<ZhangGraphEdge> edges;
};

/** Build a small connected receiver subgraph that still spans every current
 * satellite.  The configured support is a target, clipped per satellite to
 * the support physically present in the input graph.  Previously selected
 * receivers are retained when they remain connected; new receivers are added
 * greedily by reduction of the satellite-support deficit.  No future data or
 * ambiguity-fixing outcome enters the selection. */
inline ZhangProductReceiverCore zhangBuildProductReceiverCore(
    const std::set<ZhangGraphEdge>& edges,
    const std::string& rootReceiver,
    const std::set<std::string>& previousReceivers,
    int requestedSatelliteSupport,
    const std::map<ZhangGraphEdge, double>& quality = {},
    const std::map<ZhangGraphEdge, int>& persistence = {})
{
    ZhangProductReceiverCore result;
    if (edges.empty() || rootReceiver.empty() || requestedSatelliteSupport < 1)
    {
        return result;
    }
    std::set<std::string> allReceivers;
    std::map<SatSys, int> availableSupport;
    for (const auto& edge : edges)
    {
        allReceivers.insert(edge.receiver);
        result.satellites.insert(edge.satellite);
        availableSupport[edge.satellite]++;
    }
    if (allReceivers.find(rootReceiver) == allReceivers.end())
    {
        return result;
    }

    std::set<std::string> selected = {rootReceiver};
    for (const auto& receiver : previousReceivers)
    {
        if (allReceivers.find(receiver) != allReceivers.end())
        {
            selected.insert(receiver);
        }
    }
    auto selectedComponent = [&](const std::set<std::string>& receivers)
    {
        std::set<ZhangGraphEdge> selectedEdges;
        for (const auto& edge : edges)
        {
            if (receivers.find(edge.receiver) != receivers.end())
            {
                selectedEdges.insert(edge);
            }
        }
        return zhangRootComponentEdges(selectedEdges, rootReceiver);
    };
    auto deficit = [&](const std::set<ZhangGraphEdge>& component)
    {
        std::map<SatSys, int> support;
        for (const auto& edge : component)
        {
            support[edge.satellite]++;
        }
        int total = 0;
        for (const auto& satellite : result.satellites)
        {
            const int target = std::min(
                requestedSatelliteSupport, availableSupport[satellite]);
            total += std::max(0, target - support[satellite]);
        }
        return total;
    };

    std::set<ZhangGraphEdge> component = selectedComponent(selected);
    int currentDeficit = deficit(component);
    while (currentDeficit > 0 && selected.size() < allReceivers.size())
    {
        std::string bestReceiver;
        int bestImprovement = 0;
        bool bestWasPrevious = false;
        long long bestPersistence = std::numeric_limits<long long>::min();
        double bestQuality = -std::numeric_limits<double>::infinity();
        std::set<ZhangGraphEdge> bestComponent;
        for (const auto& receiver : allReceivers)
        {
            if (selected.find(receiver) != selected.end())
            {
                continue;
            }
            auto trialReceivers = selected;
            trialReceivers.insert(receiver);
            auto trialComponent = selectedComponent(trialReceivers);
            const int improvement = currentDeficit - deficit(trialComponent);
            if (improvement <= 0)
            {
                continue;
            }
            long long persistenceScore = 0;
            double qualityScore = 0;
            const bool wasPrevious =
                previousReceivers.find(receiver) != previousReceivers.end();
            for (const auto& edge : trialComponent)
            {
                if (edge.receiver != receiver)
                {
                    continue;
                }
                auto persistenceIt = persistence.find(edge);
                persistenceScore += persistenceIt == persistence.end()
                    ? 0 : persistenceIt->second;
                auto qualityIt = quality.find(edge);
                qualityScore += qualityIt == quality.end()
                    ? 0 : qualityIt->second;
            }
            if (improvement > bestImprovement
             || (improvement == bestImprovement
                 && wasPrevious > bestWasPrevious)
             || (improvement == bestImprovement
                 && wasPrevious == bestWasPrevious
                 && persistenceScore > bestPersistence)
             || (improvement == bestImprovement
                 && wasPrevious == bestWasPrevious
                 && persistenceScore == bestPersistence
                 && qualityScore > bestQuality)
             || (improvement == bestImprovement
                 && wasPrevious == bestWasPrevious
                 && persistenceScore == bestPersistence
                 && qualityScore == bestQuality
                 && (bestReceiver.empty() || receiver < bestReceiver)))
            {
                bestReceiver = receiver;
                bestImprovement = improvement;
                bestWasPrevious = wasPrevious;
                bestPersistence = persistenceScore;
                bestQuality = qualityScore;
                bestComponent = std::move(trialComponent);
            }
        }
        if (bestReceiver.empty())
        {
            return result;
        }
        selected.insert(bestReceiver);
        component = std::move(bestComponent);
        currentDeficit -= bestImprovement;
    }
    if (currentDeficit != 0)
    {
        return result;
    }

    // A new receiver may repair support lost by an old core member while also
    // making that old member redundant.  Remove such redundancy after the
    // deficit is closed so the core can replace receivers without growing
    // monotonically.  Prefer retaining prior, persistent, high-quality
    // receivers whenever several removals are possible.
    while (selected.size() > 1)
    {
        std::string removable;
        bool removableWasPrevious = true;
        long long removablePersistence = std::numeric_limits<long long>::max();
        double removableQuality = std::numeric_limits<double>::infinity();
        std::set<ZhangGraphEdge> removableComponent;
        for (const auto& receiver : selected)
        {
            if (receiver == rootReceiver)
            {
                continue;
            }
            auto trialReceivers = selected;
            trialReceivers.erase(receiver);
            auto trialComponent = selectedComponent(trialReceivers);
            if (deficit(trialComponent) != 0)
            {
                continue;
            }
            const bool wasPrevious =
                previousReceivers.find(receiver) != previousReceivers.end();
            long long persistenceScore = 0;
            double qualityScore = 0;
            for (const auto& edge : component)
            {
                if (edge.receiver != receiver)
                {
                    continue;
                }
                auto persistenceIt = persistence.find(edge);
                persistenceScore += persistenceIt == persistence.end()
                    ? 0 : persistenceIt->second;
                auto qualityIt = quality.find(edge);
                qualityScore += qualityIt == quality.end()
                    ? 0 : qualityIt->second;
            }
            if (removable.empty()
             || wasPrevious < removableWasPrevious
             || (wasPrevious == removableWasPrevious
                 && persistenceScore < removablePersistence)
             || (wasPrevious == removableWasPrevious
                 && persistenceScore == removablePersistence
                 && qualityScore < removableQuality)
             || (wasPrevious == removableWasPrevious
                 && persistenceScore == removablePersistence
                 && qualityScore == removableQuality
                 && receiver > removable))
            {
                removable = receiver;
                removableWasPrevious = wasPrevious;
                removablePersistence = persistenceScore;
                removableQuality = qualityScore;
                removableComponent = std::move(trialComponent);
            }
        }
        if (removable.empty())
        {
            break;
        }
        selected.erase(removable);
        component = std::move(removableComponent);
    }

    result.edges = std::move(component);
    result.receivers = {rootReceiver};
    std::map<SatSys, int> finalSupport;
    for (const auto& edge : result.edges)
    {
        result.receivers.insert(edge.receiver);
        finalSupport[edge.satellite]++;
    }
    result.minimumSatelliteSupport = std::numeric_limits<int>::max();
    for (const auto& satellite : result.satellites)
    {
        result.minimumSatelliteSupport = std::min(
            result.minimumSatelliteSupport, finalSupport[satellite]);
    }
    const auto spanning = zhangBuildSpanningTree(
        result.edges, rootReceiver);
    result.connected = spanning.connected
        && spanning.satellites == result.satellites;
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
