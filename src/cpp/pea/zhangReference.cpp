#include "pea/zhangReference.hpp"

#include <algorithm>
#include <limits>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <vector>
#include <boost/log/trivial.hpp>
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/constants.hpp"
#include "common/observations.hpp"
#include "common/receiver.hpp"
#include "common/satStat.hpp"
#include "common/trace.hpp"
#include "common/zhangFullRank.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "pea/zhangPppAr.hpp"

using std::map;
using std::set;
using std::string;
using std::vector;

namespace
{
struct ReferenceAvailability
{
    map<string, set<SatSys>> satellitesByReceiver;
    map<SatSys, double>      elevationScore;
    set<ZhangGraphEdge>      edges;
    set<ZhangGraphEdge>      rawEdges;
    set<ZhangGraphEdge>      discontinuousEdges;
    map<ZhangGraphEdge, set<E_ObsCode>> discontinuitySignals;
    set<ZhangGraphEdge>      qcExcludedEdges;
    set<ZhangGraphEdge>      elevationExcludedEdges;
    set<ZhangGraphEdge>      signalUnavailableEdges;
    map<ZhangGraphEdge, double> edgeQuality;
};

struct ReferenceOutageState
{
    int receiverEpochs = 0;
    int satelliteEpochs = 0;
};

map<std::pair<KFState*, E_Sys>, ReferenceOutageState> outageStateMap;

struct GraphRuntimeState
{
    struct EdgeHistory
    {
        int continuousEpochs = 0;
        int outageEpochs = 0;
        int arcVersion = 0;
    };

    ZhangGraphBasis          basis;
    ZhangGraphBasis          activeBasis;
    ZhangGraphBasis          productBasis;
    map<ZhangGraphEdge, int> productArcVersions;
    set<ZhangGraphEdge>      observationEdges;
    set<ZhangGraphEdge>      stateEdges;
    map<ZhangGraphEdge, EdgeHistory> edgeHistory;
    bool                     initialized = false;
    int                      deferredEpochs = 0;
    int                      datumVersion = 0;
    int                      eventCounter = 0;
    int                      productDatumVersion = 0;
    bool                     productInitialized = false;
};

map<std::pair<const KFState*, E_Sys>, GraphRuntimeState> graphStateMap;

bool slipIsExcluded(const SigStat::SlipStat& slip)
{
    if (!slip.any)
    {
        return false;
    }

    return
        (acsConfig.exclude.LLI         && slip.LLI)        ||
        (acsConfig.exclude.GF          && slip.GF)         ||
        (acsConfig.exclude.MW          && slip.MW)         ||
        (acsConfig.exclude.SCDIA       && slip.SCDIA)      ||
        (acsConfig.exclude.retrack     && slip.retrack)    ||
        (acsConfig.exclude.single_freq && slip.singleFreq);
}

bool signalIsUsable(const GObs& obs, E_ObsCode code)
{
    for (auto& [frequency, signal] : obs.sigs)
    {
        if (signal.code != code || signal.P == 0 || signal.L == 0 || signal.invalid)
        {
            continue;
        }

        if (obs.satStat_ptr)
        {
            auto slipIt = obs.satStat_ptr->sigStatMap.find(ft2string(frequency));
            if (slipIt != obs.satStat_ptr->sigStatMap.end() &&
                slipIsExcluded(slipIt->second.slip))
            {
                continue;
            }
        }

        return true;
    }

    return false;
}

bool signalHasExcludedSlip(const GObs& obs, E_ObsCode code)
{
    if (!obs.satStat_ptr)
    {
        return false;
    }

    for (const auto& [frequency, signal] : obs.sigs)
    {
        if (signal.code != code)
        {
            continue;
        }

        auto slipIt = obs.satStat_ptr->sigStatMap.find(ft2string(frequency));
        if (slipIt != obs.satStat_ptr->sigStatMap.end() &&
            slipIsExcluded(slipIt->second.slip))
        {
            return true;
        }
    }

    return false;
}

ReferenceAvailability referenceAvailability(
    ReceiverMap&                         receiverMap,
    E_Sys                                sys,
    const vector<E_ObsCode>&             baselineObservables
)
{
    ReferenceAvailability availability;

    for (auto& [id, receiver] : receiverMap)
    {
        if (!receiver.ready || receiver.obsList.empty())
        {
            continue;
        }

        auto& receiverOptions = acsConfig.getRecOpts(id);
        for (auto& obs : only<GObs>(receiver.obsList))
        {
            if (obs.Sat.sys != sys)
            {
                continue;
            }

            ZhangGraphEdge edge{id, obs.Sat};
            availability.rawEdges.insert(edge);
            bool hardDiscontinuity = false;
            for (E_ObsCode code : baselineObservables)
            {
                if (signalHasExcludedSlip(obs, code))
                {
                    hardDiscontinuity = true;
                    availability.discontinuitySignals[edge].insert(code);
                }
            }
            if (hardDiscontinuity)
            {
                availability.discontinuousEdges.insert(edge);
            }

            if (obs.exclude)
            {
                availability.qcExcludedEdges.insert(edge);
                continue;
            }

            // The graph must be built from edges that can actually create PPP rows.  The
            // elevation exclusion in receiverUducGnss is evaluated after the generic
            // observation flags, so checking only obs.exclude may select an unmodelled tree
            // edge and leave the satellite phase state rank deficient.
            if (acsConfig.exclude.elevation &&
                obs.satStat_ptr &&
                obs.satStat_ptr->el < receiverOptions.elevation_mask_deg * D2R)
            {
                availability.elevationExcludedEdges.insert(edge);
                continue;
            }

            bool usable = true;
            for (E_ObsCode code : baselineObservables)
            {
                usable &= signalIsUsable(obs, code);
            }

            if (!usable)
            {
                availability.signalUnavailableEdges.insert(edge);
                continue;
            }

            availability.satellitesByReceiver[id].insert(obs.Sat);
            availability.edges.insert(edge);
            if (obs.satStat_ptr)
            {
                availability.elevationScore[obs.Sat] += obs.satStat_ptr->el;
                availability.edgeQuality[edge] = obs.satStat_ptr->el;
            }
        }
    }

    return availability;
}

set<SatSys> commonSatellites(const ReferenceAvailability& availability)
{
    set<SatSys> common;
    bool first = true;

    for (auto& [receiver, satellites] : availability.satellitesByReceiver)
    {
        if (first)
        {
            common = satellites;
            first = false;
            continue;
        }

        set<SatSys> intersection;
        std::set_intersection(
            common.begin(),
            common.end(),
            satellites.begin(),
            satellites.end(),
            std::inserter(intersection, intersection.begin())
        );
        common = std::move(intersection);
    }

    return common;
}

vector<string> orderedReceivers(
    const ReferenceAvailability& availability,
    const vector<string>&        candidates
)
{
    vector<string> ordered;
    set<string>    inserted;

    for (auto& candidate : candidates)
    {
        auto it = availability.satellitesByReceiver.find(candidate);
        if (it != availability.satellitesByReceiver.end() &&
            !it->second.empty() &&
            inserted.insert(candidate).second)
        {
            ordered.push_back(candidate);
        }
    }

    vector<std::pair<string, size_t>> remaining;
    for (auto& [receiver, satellites] : availability.satellitesByReceiver)
    {
        if (inserted.find(receiver) == inserted.end())
        {
            remaining.emplace_back(receiver, satellites.size());
        }
    }

    std::sort(
        remaining.begin(),
        remaining.end(),
        [](const auto& left, const auto& right)
        {
            if (left.second != right.second)
            {
                return left.second > right.second;
            }
            return left.first < right.first;
        }
    );
    for (auto& [receiver, count] : remaining)
    {
        ordered.push_back(receiver);
    }

    return ordered;
}

vector<SatSys> orderedSatellites(
    const set<SatSys>&            common,
    const map<SatSys, double>&    elevationScore,
    const vector<string>&         candidates
)
{
    vector<SatSys> ordered;
    set<SatSys>    inserted;

    for (auto& candidateId : candidates)
    {
        SatSys candidate(candidateId.c_str());
        if (common.find(candidate) != common.end() && inserted.insert(candidate).second)
        {
            ordered.push_back(candidate);
        }
    }

    vector<std::pair<SatSys, double>> remaining;
    for (auto& satellite : common)
    {
        if (inserted.find(satellite) == inserted.end())
        {
            auto scoreIt = elevationScore.find(satellite);
            remaining.emplace_back(
                satellite,
                scoreIt == elevationScore.end() ? 0 : scoreIt->second
            );
        }
    }

    std::sort(
        remaining.begin(),
        remaining.end(),
        [](const auto& left, const auto& right)
        {
            if (left.second != right.second)
            {
                return left.second > right.second;
            }
            return left.first < right.first;
        }
    );
    for (auto& [satellite, score] : remaining)
    {
        ordered.push_back(satellite);
    }

    return ordered;
}

void addCoefficient(
    map<KFKey, double>& coefficients,
    const KFKey&        source,
    double              value
)
{
    if (value == 0)
    {
        return;
    }

    coefficients[source] += value;
    if (coefficients[source] == 0)
    {
        coefficients.erase(source);
    }
}

bool addRequiredCoefficient(
    const KFState&      kfState,
    map<KFKey, double>& coefficients,
    const KFKey&        source,
    double              value
)
{
    if (value == 0)
    {
        return true;
    }

    if (kfState.kfIndexMap.find(source) == kfState.kfIndexMap.end())
    {
        return false;
    }

    addCoefficient(coefficients, source, value);
    return true;
}

bool transformClockDatum(
    const KFState&                         kfState,
    map<KFKey, map<KFKey, double>>&        transform,
    const string&                          oldReceiver,
    const string&                          newReceiver,
    KF                                     receiverType,
    KF                                     satelliteType
)
{
    set<int> components;
    set<string> receivers = {oldReceiver};
    vector<KFKey> satellites;

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type == receiverType)
        {
            components.insert(key.num);
            receivers.insert(key.str);
        }
        else if (key.type == satelliteType)
        {
            components.insert(key.num);
            satellites.push_back(key);
        }
    }

    if (components.empty())
    {
        return true;
    }

    receivers.insert(newReceiver);

    for (int component : components)
    {
        KFKey newReferenceKey;
        newReferenceKey.type = receiverType;
        newReferenceKey.str  = newReceiver;
        newReferenceKey.num  = component;

        if (newReceiver != oldReceiver &&
            kfState.kfIndexMap.find(newReferenceKey) == kfState.kfIndexMap.end())
        {
            return false;
        }

        for (auto& receiver : receivers)
        {
            if (receiver == newReceiver)
            {
                continue;
            }

            KFKey destination;
            destination.type = receiverType;
            destination.str  = receiver;
            destination.num  = component;

            auto& coefficients = transform[destination];

            if (receiver != oldReceiver)
            {
                KFKey oldReceiverKey = destination;
                if (!addRequiredCoefficient(kfState, coefficients, oldReceiverKey, +1))
                {
                    return false;
                }
            }

            if (newReceiver != oldReceiver)
            {
                addCoefficient(coefficients, newReferenceKey, -1);
            }
        }

        for (auto& satellite : satellites)
        {
            if (satellite.num != component)
            {
                continue;
            }

            auto& coefficients = transform[satellite];
            addCoefficient(coefficients, satellite, +1);
            if (newReceiver != oldReceiver)
            {
                addCoefficient(coefficients, newReferenceKey, -1);
            }
        }
    }

    return true;
}

double zhangWavelength(E_Sys sys, E_ObsCode code)
{
    auto sysFrequencyIt = code2Freq.find(sys);
    if (sysFrequencyIt == code2Freq.end())
    {
        return 0;
    }

    auto frequencyIt = sysFrequencyIt->second.find(code);
    if (frequencyIt == sysFrequencyIt->second.end())
    {
        return 0;
    }

    auto wavelengthIt = genericWavelength.find(frequencyIt->second);
    if (wavelengthIt == genericWavelength.end())
    {
        return 0;
    }

    return wavelengthIt->second;
}

KFKey zhangReceiverPhaseKey(
    E_Sys       sys,
    E_ObsCode   code,
    const string& receiver
)
{
    KFKey key;
    key.type = KF::PHASE_BIAS;
    key.str  = receiver;
    key.Sat  = SatSys(sys, 0);
    key.num  = static_cast<int>(code);
    return key;
}

KFKey zhangSatellitePhaseKey(E_ObsCode code, const SatSys& satellite)
{
    KFKey key;
    key.type = KF::PHASE_BIAS;
    key.Sat  = satellite;
    key.num  = static_cast<int>(code);
    return key;
}

KFKey zhangAmbiguityKey(E_ObsCode code, const ZhangGraphEdge& edge)
{
    KFKey key;
    key.type = KF::AMBIGUITY;
    key.str  = edge.receiver;
    key.Sat  = edge.satellite;
    key.num  = static_cast<int>(code);
    return key;
}

void addExpression(
    map<KFKey, double>&       destination,
    const map<KFKey, double>& source,
    double                    scale = 1
)
{
    for (const auto& [key, coefficient] : source)
    {
        addCoefficient(destination, key, scale * coefficient);
    }
}

/** Re-express the existing Zhang phase state in a new spanning-tree basis. */
bool transformZhangGraphBasis(
    Trace&                   trace,
    KFState&                 kfState,
    E_Sys                    sys,
    const vector<E_ObsCode>& baselineObservables,
    const ZhangGraphBasis&   oldBasis,
    const ZhangGraphBasis&   newTree
)
{
    map<std::pair<SatSys, E_ObsCode>, double> oldSatellitePhases;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type == KF::PHASE_BIAS &&
            key.Sat.sys == sys &&
            key.Sat.prn > 0 &&
            key.str.empty() &&
            zhangFullRankUsesObservable(
                static_cast<E_ObsCode>(key.num),
                baselineObservables
            ))
        {
            oldSatellitePhases[
                {key.Sat, static_cast<E_ObsCode>(key.num)}
            ] = kfState.x(index);
        }
    }

    map<KFKey, map<KFKey, double>> transform;

    auto isTargetPhase = [&](const KFKey& key)
    {
        if (key.type != KF::PHASE_BIAS ||
            !zhangFullRankUsesObservable(
                static_cast<E_ObsCode>(key.num),
                baselineObservables
            ))
        {
            return false;
        }

        return key.Sat.sys == sys;
    };

    auto isTargetAmbiguity = [&](const KFKey& key)
    {
        return key.type == KF::AMBIGUITY &&
               key.Sat.sys == sys &&
               zhangFullRankUsesObservable(
                   static_cast<E_ObsCode>(key.num),
                   baselineObservables
               );
    };

    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        if (!isTargetPhase(key) && !isTargetAmbiguity(key))
        {
            transform[key][key] = 1;
        }
    }

    for (E_ObsCode code : baselineObservables)
    {
        const double wavelength = zhangWavelength(sys, code);
        if (wavelength <= 0)
        {
            return false;
        }

        set<string> stateReceivers = {oldBasis.rootReceiver};
        set<SatSys> stateSatellites;
        set<ZhangGraphEdge> ambiguityEdges;

        for (const auto& [key, index] : kfState.kfIndexMap)
        {
            if (key.num != static_cast<int>(code))
            {
                continue;
            }

            if (key.type == KF::PHASE_BIAS && key.Sat.sys == sys)
            {
                if (!key.str.empty() && key.Sat.prn == 0)
                {
                    stateReceivers.insert(key.str);
                }
                else if (key.str.empty() && key.Sat.prn > 0)
                {
                    stateSatellites.insert(key.Sat);
                }
            }
            else if (key.type == KF::AMBIGUITY && key.Sat.sys == sys)
            {
                ambiguityEdges.insert({key.str, key.Sat});
            }
        }

        set<ZhangGraphEdge> modelledEdges = oldBasis.treeEdges;
        modelledEdges.insert(ambiguityEdges.begin(), ambiguityEdges.end());

        // Nodes with no current baseline edge may be retired exactly by omitting their phase and
        // incident cycle states from the destination coordinate system.  Adding a genuinely new
        // node is handled separately as a leaf extension, because no old-state expression exists
        // for its phase datum.
        if (!std::includes(
                stateReceivers.begin(),
                stateReceivers.end(),
                newTree.receivers.begin(),
                newTree.receivers.end()
            ) ||
            !std::includes(
                stateSatellites.begin(),
                stateSatellites.end(),
                newTree.satellites.begin(),
                newTree.satellites.end()
            ))
        {
            BOOST_LOG_TRIVIAL(warning)
                << "Zhang graph transform cannot introduce a new phase node for "
                << enum_to_string(sys) << " " << enum_to_string(code);
            return false;
        }

        for (auto edgeIt = modelledEdges.begin(); edgeIt != modelledEdges.end();)
        {
            if (newTree.receivers.find(edgeIt->receiver) == newTree.receivers.end() ||
                newTree.satellites.find(edgeIt->satellite) == newTree.satellites.end())
            {
                edgeIt = modelledEdges.erase(edgeIt);
            }
            else
            {
                ++edgeIt;
            }
        }

        const set<string>& receivers = newTree.receivers;
        const set<SatSys>& satellites = newTree.satellites;
        set<string> modelledReceivers;
        set<SatSys> modelledSatellites;
        for (const auto& edge : modelledEdges)
        {
            modelledReceivers.insert(edge.receiver);
            modelledSatellites.insert(edge.satellite);
        }

        if (modelledReceivers != receivers ||
            modelledSatellites != satellites)
        {
            BOOST_LOG_TRIVIAL(warning)
                << "Zhang graph transform node-set mismatch for "
                << enum_to_string(sys) << " " << enum_to_string(code)
                << ": state receivers/satellites=" << stateReceivers.size() << "/"
                << stateSatellites.size()
                << ", modelled=" << modelledReceivers.size() << "/"
                << modelledSatellites.size()
                << ", new tree=" << newTree.receivers.size() << "/"
                << newTree.satellites.size();
            return false;
        }

        for (const auto& edge : newTree.treeEdges)
        {
            if (modelledEdges.find(edge) == modelledEdges.end())
            {
                BOOST_LOG_TRIVIAL(warning)
                    << "Zhang graph transform lacks new tree edge "
                    << edge.receiver << "/" << edge.satellite.id() << " "
                    << enum_to_string(code);
                return false;
            }
        }

        auto oldEdgeExpression = [&](const ZhangGraphEdge& edge,
                                     map<KFKey, double>& expression)
        {
            if (edge.receiver != oldBasis.rootReceiver)
            {
                if (!addRequiredCoefficient(
                        kfState,
                        expression,
                        zhangReceiverPhaseKey(sys, code, edge.receiver),
                        +1
                    ))
                {
                    return false;
                }
            }

            if (!addRequiredCoefficient(
                    kfState,
                    expression,
                    zhangSatellitePhaseKey(code, edge.satellite),
                    +1
                ))
            {
                return false;
            }

            if (!oldBasis.isTreeEdge(edge.receiver, edge.satellite))
            {
                if (!addRequiredCoefficient(
                        kfState,
                        expression,
                        zhangAmbiguityKey(code, edge),
                        wavelength
                    ))
                {
                    return false;
                }
            }

            return true;
        };

        map<ZhangGraphEdge, map<KFKey, double>> edgeExpressions;
        for (const auto& edge : modelledEdges)
        {
            if (!oldEdgeExpression(edge, edgeExpressions[edge]))
            {
                BOOST_LOG_TRIVIAL(warning)
                    << "Zhang graph transform cannot reconstruct old edge "
                    << edge.receiver << "/" << edge.satellite.id() << " "
                    << enum_to_string(code);
                return false;
            }
        }

        map<string, map<KFKey, double>> receiverExpressions;
        map<SatSys, map<KFKey, double>> satelliteExpressions;
        set<string> knownReceivers = {newTree.rootReceiver};
        set<SatSys> knownSatellites;

        bool progress = true;
        while (progress)
        {
            progress = false;
            for (const auto& edge : newTree.treeEdges)
            {
                bool receiverKnown =
                    knownReceivers.find(edge.receiver) != knownReceivers.end();
                bool satelliteKnown =
                    knownSatellites.find(edge.satellite) != knownSatellites.end();

                if (receiverKnown && !satelliteKnown)
                {
                    auto expression = edgeExpressions.at(edge);
                    addExpression(expression, receiverExpressions[edge.receiver], -1);
                    satelliteExpressions[edge.satellite] = std::move(expression);
                    knownSatellites.insert(edge.satellite);
                    progress = true;
                }
                else if (!receiverKnown && satelliteKnown)
                {
                    auto expression = edgeExpressions.at(edge);
                    addExpression(expression, satelliteExpressions[edge.satellite], -1);
                    receiverExpressions[edge.receiver] = std::move(expression);
                    knownReceivers.insert(edge.receiver);
                    progress = true;
                }
            }
        }

        if (knownReceivers != receivers || knownSatellites != satellites)
        {
            BOOST_LOG_TRIVIAL(warning)
                << "Zhang graph transform new tree is not connected in state space for "
                << enum_to_string(sys) << " " << enum_to_string(code)
                << ": reached receivers/satellites=" << knownReceivers.size() << "/"
                << knownSatellites.size()
                << ", expected=" << receivers.size() << "/" << satellites.size();
            return false;
        }

        for (const auto& receiver : receivers)
        {
            if (receiver == newTree.rootReceiver)
            {
                continue;
            }
            transform[zhangReceiverPhaseKey(sys, code, receiver)] =
                receiverExpressions.at(receiver);
        }

        for (const auto& satellite : satellites)
        {
            transform[zhangSatellitePhaseKey(code, satellite)] =
                satelliteExpressions.at(satellite);
        }

        for (const auto& edge : modelledEdges)
        {
            if (newTree.isTreeEdge(edge.receiver, edge.satellite))
            {
                continue;
            }

            auto expression = edgeExpressions.at(edge);
            addExpression(expression, receiverExpressions[edge.receiver], -1);
            addExpression(expression, satelliteExpressions[edge.satellite], -1);

            map<KFKey, double> ambiguityExpression;
            addExpression(ambiguityExpression, expression, 1 / wavelength);
            transform[zhangAmbiguityKey(code, edge)] = std::move(ambiguityExpression);
        }
    }

    string label =
        "Zhang graph " + enum_to_string(sys) + " tree exchange";
    bool applied = kfState.applyStateTransform(trace, transform, label);
    if (!applied)
    {
        return false;
    }

    map<E_ObsCode, map<SatSys, double>> correctionChanges;
    for (const auto& [key, oldPhase] : oldSatellitePhases)
    {
        const auto& [satellite, code] = key;
        KFKey phaseKey = zhangSatellitePhaseKey(code, satellite);
        auto phaseIt = kfState.kfIndexMap.find(phaseKey);
        if (phaseIt == kfState.kfIndexMap.end())
        {
            continue;
        }

        double newPhase = kfState.x(phaseIt->second);
        correctionChanges[code][satellite] = -(newPhase - oldPhase);
    }
    for (const auto& [code, changes] : correctionChanges)
    {
        recordZhangExactPhaseTransforms(
            kfState.time, sys, code, changes
        );
    }

    return true;
}

bool resetZhangGraphPhaseCoordinates(
    Trace&                   trace,
    KFState&                 kfState,
    E_Sys                    sys,
    const vector<E_ObsCode>& baselineObservables,
    const set<string>&        affectedReceivers,
    const set<SatSys>&        affectedSatellites
)
{
    map<KFKey, map<KFKey, double>> transform;
    int removedStates = 0;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        bool targetReceiverPhase =
            key.type == KF::PHASE_BIAS &&
            key.Sat.sys == sys &&
            key.Sat.prn == 0 &&
            !key.str.empty() &&
            affectedReceivers.find(key.str) != affectedReceivers.end() &&
            zhangFullRankUsesObservable(
                static_cast<E_ObsCode>(key.num),
                baselineObservables
            );
        bool targetSatellitePhase =
            key.type == KF::PHASE_BIAS &&
            key.Sat.sys == sys &&
            key.Sat.prn > 0 &&
            key.str.empty() &&
            affectedSatellites.find(key.Sat) != affectedSatellites.end() &&
            zhangFullRankUsesObservable(
                static_cast<E_ObsCode>(key.num),
                baselineObservables
            );
        bool targetAmbiguity =
            key.type == KF::AMBIGUITY &&
            key.Sat.sys == sys &&
            (affectedReceivers.find(key.str) != affectedReceivers.end() ||
             affectedSatellites.find(key.Sat) != affectedSatellites.end()) &&
            zhangFullRankUsesObservable(
                static_cast<E_ObsCode>(key.num),
                baselineObservables
            );

        if (!targetReceiverPhase && !targetSatellitePhase && !targetAmbiguity)
        {
            transform[key][key] = 1;
        }
        else
        {
            removedStates++;
        }
    }

    return removedStates > 0 &&
           !transform.empty() &&
           kfState.applyStateTransform(
               trace,
               transform,
               "Zhang graph local phase-coordinate reinitialisation"
           );
}

bool transformZhangDatum(
    Trace&                       trace,
    KFState&                     kfState,
    E_Sys                        sys,
    const vector<E_ObsCode>&     baselineObservables,
    const string&                oldReceiver,
    const SatSys&                oldSatellite,
    const string&                newReceiver,
    const SatSys&                newSatellite
)
{
    map<KFKey, map<KFKey, double>> transform;

    auto isZhangPhaseState = [&](const KFKey& key)
    {
        if (key.type != KF::PHASE_BIAS ||
            !zhangFullRankUsesObservable(
                static_cast<E_ObsCode>(key.num),
                baselineObservables
            ))
        {
            return false;
        }

        if (!key.str.empty())
        {
            return key.Sat.sys == sys && key.Sat.prn == 0;
        }

        return key.Sat.sys == sys && key.Sat.prn > 0;
    };

    auto isZhangAmbiguity = [&](const KFKey& key)
    {
        return key.type == KF::AMBIGUITY &&
               key.Sat.sys == sys &&
               zhangFullRankUsesObservable(
                   static_cast<E_ObsCode>(key.num),
                   baselineObservables
               );
    };

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        bool clockState =
            key.type == KF::REC_CLOCK ||
            key.type == KF::SAT_CLOCK ||
            key.type == KF::REC_CLOCK_RATE ||
            key.type == KF::SAT_CLOCK_RATE;

        if (clockState || isZhangPhaseState(key) || isZhangAmbiguity(key))
        {
            continue;
        }

        transform[key][key] = 1;
    }

    if (!transformClockDatum(
            kfState,
            transform,
            oldReceiver,
            newReceiver,
            KF::REC_CLOCK,
            KF::SAT_CLOCK
        ) ||
        !transformClockDatum(
            kfState,
            transform,
            oldReceiver,
            newReceiver,
            KF::REC_CLOCK_RATE,
            KF::SAT_CLOCK_RATE
        ))
    {
        BOOST_LOG_TRIVIAL(error)
            << "Cannot change Zhang receiver reference from " << oldReceiver << " to "
            << newReceiver << ": required receiver clock state is absent";
        return false;
    }

    for (E_ObsCode code : baselineObservables)
    {
        auto sysFrequencyIt = code2Freq.find(sys);
        if (sysFrequencyIt == code2Freq.end())
        {
            return false;
        }
        auto frequencyIt = sysFrequencyIt->second.find(code);
        if (frequencyIt == sysFrequencyIt->second.end())
        {
            return false;
        }
        auto wavelengthIt = genericWavelength.find(frequencyIt->second);
        if (wavelengthIt == genericWavelength.end())
        {
            return false;
        }
        const double wavelength = wavelengthIt->second;

        set<string> receivers = {oldReceiver};
        set<SatSys> satellites;
        set<std::pair<string, SatSys>> ambiguityEdges;

        for (auto& [key, index] : kfState.kfIndexMap)
        {
            if (key.type == KF::PHASE_BIAS && key.num == static_cast<int>(code))
            {
                if (!key.str.empty() && key.Sat.sys == sys && key.Sat.prn == 0)
                {
                    receivers.insert(key.str);
                }
                else if (key.str.empty() && key.Sat.sys == sys && key.Sat.prn > 0)
                {
                    satellites.insert(key.Sat);
                }
            }
            else if (
                key.type == KF::AMBIGUITY &&
                key.num == static_cast<int>(code) &&
                key.Sat.sys == sys
            )
            {
                ambiguityEdges.emplace(key.str, key.Sat);
            }
        }

        auto receiverPhaseKey = [&](const string& receiver)
        {
            KFKey key;
            key.type = KF::PHASE_BIAS;
            key.str  = receiver;
            key.Sat  = SatSys(sys, 0);
            key.num  = static_cast<int>(code);
            return key;
        };

        auto satellitePhaseKey = [&](const SatSys& satellite)
        {
            KFKey key;
            key.type = KF::PHASE_BIAS;
            key.Sat  = satellite;
            key.num  = static_cast<int>(code);
            return key;
        };

        auto ambiguityKey = [&](const string& receiver, const SatSys& satellite)
        {
            KFKey key;
            key.type = KF::AMBIGUITY;
            key.str  = receiver;
            key.Sat  = satellite;
            key.num  = static_cast<int>(code);
            return key;
        };

        if (receivers.find(newReceiver) == receivers.end() ||
            satellites.find(newSatellite) == satellites.end())
        {
            BOOST_LOG_TRIVIAL(debug)
                << "Zhang S-transform candidate lacks a phase tree state for "
                << newReceiver << "/" << newSatellite.id() << " " << enum_to_string(code);
            return false;
        }

        // The current filter may be sparse after outages and arc resets.  Its phase states form
        // a graph: the old reference receiver row and satellite column are the tree edges, while
        // each retained DD ambiguity is a non-tree edge.  Reject internally inconsistent dormant
        // ambiguities rather than silently inventing a missing receiver/satellite phase state.
        for (auto& [receiver, satellite] : ambiguityEdges)
        {
            if (receivers.find(receiver) == receivers.end() ||
                satellites.find(satellite) == satellites.end())
            {
                BOOST_LOG_TRIVIAL(debug)
                    << "Zhang S-transform found an ambiguity without both phase tree states: "
                    << receiver << "/" << satellite.id() << " " << enum_to_string(code);
                return false;
            }
        }

        auto edgeExpression = [&](const string& receiver,
                                  const SatSys& satellite,
                                  map<KFKey, double>& coefficients)
        {
            if (receiver != oldReceiver &&
                !addRequiredCoefficient(
                    kfState,
                    coefficients,
                    receiverPhaseKey(receiver),
                    +1
                ))
            {
                return false;
            }

            if (!addRequiredCoefficient(
                    kfState,
                    coefficients,
                    satellitePhaseKey(satellite),
                    +1
                ))
            {
                return false;
            }

            if (receiver != oldReceiver && satellite != oldSatellite)
            {
                if (ambiguityEdges.find({receiver, satellite}) == ambiguityEdges.end() ||
                    !addRequiredCoefficient(
                        kfState,
                        coefficients,
                        ambiguityKey(receiver, satellite),
                        wavelength
                    ))
                {
                    return false;
                }
            }

            return true;
        };

        map<SatSys, map<KFKey, double>> newSatelliteExpressions;
        for (auto& satellite : satellites)
        {
            if (!edgeExpression(
                    newReceiver,
                    satellite,
                    newSatelliteExpressions[satellite]
                ))
            {
                BOOST_LOG_TRIVIAL(debug)
                    << "Zhang S-transform candidate lacks receiver tree edge "
                    << newReceiver << "/" << satellite.id() << " " << enum_to_string(code);
                return false;
            }

            transform[satellitePhaseKey(satellite)] = newSatelliteExpressions[satellite];
        }

        map<string, map<KFKey, double>> newReceiverExpressions;
        for (auto& receiver : receivers)
        {
            if (receiver == newReceiver)
            {
                continue;
            }

            map<KFKey, double> receiverAtNewSatellite;
            map<KFKey, double> newReferenceAtNewSatellite;
            if (!edgeExpression(receiver, newSatellite, receiverAtNewSatellite) ||
                !edgeExpression(newReceiver, newSatellite, newReferenceAtNewSatellite))
            {
                BOOST_LOG_TRIVIAL(debug)
                    << "Zhang S-transform candidate lacks satellite tree edge "
                    << receiver << "/" << newSatellite.id() << " " << enum_to_string(code);
                return false;
            }

            auto& expression = newReceiverExpressions[receiver];
            for (auto& [source, coefficient] : receiverAtNewSatellite)
            {
                addCoefficient(expression, source, coefficient);
            }
            for (auto& [source, coefficient] : newReferenceAtNewSatellite)
            {
                addCoefficient(expression, source, -coefficient);
            }

            transform[receiverPhaseKey(receiver)] = expression;
        }

        // Re-express exactly the graph edges represented by the old state.  Removing the new tree
        // edges and retaining all remaining graph edges gives the same number of independent DD
        // states even when the receiver/satellite network is not a complete Cartesian product.
        set<std::pair<string, SatSys>> modelledEdges = ambiguityEdges;
        for (auto& satellite : satellites)
        {
            modelledEdges.emplace(oldReceiver, satellite);
        }
        for (auto& receiver : receivers)
        {
            modelledEdges.emplace(receiver, oldSatellite);
        }

        for (auto& [receiver, satellite] : modelledEdges)
        {
            if (receiver == newReceiver || satellite == newSatellite)
            {
                continue;
            }

            map<KFKey, double> edge;
            if (!edgeExpression(receiver, satellite, edge))
            {
                return false;
            }

            auto& expression = transform[ambiguityKey(receiver, satellite)];
            for (auto& [source, coefficient] : edge)
            {
                addCoefficient(expression, source, coefficient / wavelength);
            }
            for (auto& [source, coefficient] : newReceiverExpressions[receiver])
            {
                addCoefficient(expression, source, -coefficient / wavelength);
            }
            for (auto& [source, coefficient] : newSatelliteExpressions[satellite])
            {
                addCoefficient(expression, source, -coefficient / wavelength);
            }
        }
    }

    string label =
        "Zhang " + enum_to_string(sys) + " " + oldReceiver + "/" + oldSatellite.id() + " -> " +
        newReceiver + "/" + newSatellite.id();

    if (transform.size() != kfState.kfIndexMap.size())
    {
        BOOST_LOG_TRIVIAL(debug)
            << "Zhang S-transform is not dimension preserving for " << label << ": "
            << kfState.kfIndexMap.size() << " -> " << transform.size();
        return false;
    }

    return kfState.applyStateTransform(trace, transform, label);
}

void updateZhangGraphBasis(
    Trace&                             trace,
    KFState&                           kfState,
    E_Sys                              sys,
    const ZhangFullRankSystemOptions& options,
    const ReferenceAvailability&       availability
)
{
    auto& runtime = graphStateMap[{&kfState, sys}];

    auto traceCanonicalAudit = [&](const ZhangGraphBasis& basis,
                                   const string&           action,
                                   bool                    exactTransition)
    {
        if (!acsConfig.zhangPppAr.output_diagnostics)
        {
            return;
        }

        ZhangCanonicalIntegerAudit audit = zhangCanonicalIntegerAudit(basis);
        if (!audit.valid)
        {
            trace << "\nZHANG_CANONICAL_INTEGER_AUDIT time="
                  << kfState.time.to_string(0)
                  << " system=" << enum_to_string(sys)
                  << " action=" << action
                  << " valid=0 reason=canonical_graph_construction_failed"
                  << " detail=" << audit.failureReason;
            return;
        }

        const string componentId = zhangIntegerComponentId(basis);
        for (E_ObsCode code : options.baseline_observables)
        {
            trace << "\nZHANG_CANONICAL_INTEGER_AUDIT time="
                  << kfState.time.to_string(0)
                  << " system=" << enum_to_string(sys)
                  << " signal=" << enum_to_string(code)
                  << " action=" << action
                  << " valid=1"
                  << " component_id=" << componentId
                  << " root_node=R:" << basis.rootReceiver
                  << " datum_version=" << runtime.datumVersion
                  << " arcs=" << basis.edges.size()
                  << " tree_datum_integers=" << audit.treeEdges.size()
                  << " cycle_integers=" << audit.chordEdges.size()
                  << " satellite_datum_rows="
                  << audit.satelliteDatumSingleDifferences.size()
                  << " satellite_fix_quotient_rows="
                  << audit.satelliteFixQuotient.size()
                  << " satellite_fix_quotient_nonzero_rows=0"
                  << " canonical_to_arc_fingerprint="
                  << audit.canonicalToArcFingerprint
                  << " datum_mapping_fingerprint="
                  << audit.datumMappingFingerprint
                  << " fix_quotient_fingerprint="
                  << audit.fixQuotientFingerprint
                  << " dense_canonical_materialised="
                  << audit.denseCanonicalMaterialised
                  << " canonical_to_arc_unimodular=STRUCTURAL_UNIT_BLOCK"
                  << " exact_epoch_transition=" << exactTransition;

            for (const auto& edge : audit.treeEdges)
            {
                int arcVersion = runtime.edgeHistory[edge].arcVersion;
                trace << "\nZHANG_CANONICAL_INTEGER_COORDINATE time="
                      << kfState.time.to_string(0)
                      << " signal=" << enum_to_string(code)
                      << " component_id=" << componentId
                      << " type=TREE_DATUM"
                      << " integer_id=D:" << edge.receiver << ":"
                      << edge.satellite.id() << ":A" << arcVersion
                      << " arc_id=" << edge.receiver << ":"
                      << edge.satellite.id() << ":" << enum_to_string(code)
                      << ":A" << arcVersion;
            }
            for (const auto& edge : audit.chordEdges)
            {
                int arcVersion = runtime.edgeHistory[edge].arcVersion;
                trace << "\nZHANG_CANONICAL_INTEGER_COORDINATE time="
                      << kfState.time.to_string(0)
                      << " signal=" << enum_to_string(code)
                      << " component_id=" << componentId
                      << " type=CYCLE"
                      << " integer_id=K:" << edge.receiver << ":"
                      << edge.satellite.id() << ":A" << arcVersion
                      << " arc_id=" << edge.receiver << ":"
                      << edge.satellite.id() << ":" << enum_to_string(code)
                      << ":A" << arcVersion;
            }
        }
    };

    auto edgeList = [](const set<ZhangGraphEdge>& edges)
    {
        std::ostringstream stream;
        bool first = true;
        for (const auto& edge : edges)
        {
            stream << (first ? "" : ",") << edge.receiver << ":"
                   << edge.satellite.id();
            first = false;
        }
        return first ? string("NONE") : stream.str();
    };
    auto receiverList = [](const set<string>& receivers)
    {
        std::ostringstream stream;
        bool first = true;
        for (const auto& receiver : receivers)
        {
            stream << (first ? "" : ",") << receiver;
            first = false;
        }
        return first ? string("NONE") : stream.str();
    };
    auto satelliteList = [](const set<SatSys>& satellites)
    {
        std::ostringstream stream;
        bool first = true;
        for (const auto& satellite : satellites)
        {
            stream << (first ? "" : ",") << satellite.id();
            first = false;
        }
        return first ? string("NONE") : stream.str();
    };
    auto traceGraphEvent = [&](const string&              action,
                               const string&              reason,
                               const set<ZhangGraphEdge>& removedTreeEdges,
                               const set<ZhangGraphEdge>& replacementEdges,
                               const set<string>&         resetReceivers,
                               const set<SatSys>&         resetSatellites,
                               int                        removedIntegerColumns,
                               bool                       exactTransform)
    {
        runtime.eventCounter++;
        if (!acsConfig.zhangPppAr.output_diagnostics)
        {
            return;
        }
        trace << "\nZHANG_GRAPH_INTEGER_EVENT time="
              << kfState.time.to_string(0)
              << " system=" << enum_to_string(sys)
              << " event_id=" << runtime.eventCounter
              << " event_type=" << action
              << " reason=" << reason
              << " affected_tree_edges=" << edgeList(removedTreeEdges)
              << " replacement_edges=" << edgeList(replacementEdges)
              << " local_reset_nodes=" << receiverList(resetReceivers)
              << " local_reset_satellites=" << satelliteList(resetSatellites)
              << " removed_integer_columns=" << removedIntegerColumns
              << " held_rows_touched=DEFERRED_TO_HELD_LATTICE_EVENT"
              << " held_rows_removed=DEFERRED_TO_HELD_LATTICE_EVENT"
              << " exact_unimodular_transform_available=" << exactTransform
              << " held_lattice_storage=PHYSICAL_ARC_VERSION_HNF";
    };

    // The filter state graph is deliberately allowed to outlive the instantaneous
    // observation graph.  Old tree edges and retained cycle ambiguities provide exact
    // integer coordinates across short data gaps, but an excluded slip is a hard arc break.
    set<ZhangGraphEdge> modelledEdges = runtime.basis.treeEdges;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type == KF::AMBIGUITY &&
            key.Sat.sys == sys &&
            zhangFullRankUsesObservable(
                static_cast<E_ObsCode>(key.num),
                options.baseline_observables
            ))
        {
            modelledEdges.insert({key.str, key.Sat});
        }
    }

    for (const auto& edge : availability.discontinuousEdges)
    {
        auto& history = runtime.edgeHistory[edge];
        history.continuousEpochs = 0;
        history.outageEpochs = options.state_edge_grace_epochs + 1;
        history.arcVersion++;
    }

    set<ZhangGraphEdge> trackedEdges = modelledEdges;
    trackedEdges.insert(availability.edges.begin(), availability.edges.end());
    for (const auto& [edge, history] : runtime.edgeHistory)
    {
        trackedEdges.insert(edge);
    }

    for (const auto& edge : trackedEdges)
    {
        if (availability.discontinuousEdges.find(edge) !=
            availability.discontinuousEdges.end())
        {
            continue;
        }

        auto& history = runtime.edgeHistory[edge];
        if (availability.edges.find(edge) != availability.edges.end())
        {
            history.continuousEpochs++;
            history.outageEpochs = 0;
        }
        else
        {
            history.outageEpochs++;
        }
    }

    set<ZhangGraphEdge> stateCandidates = availability.edges;
    for (const auto& edge : modelledEdges)
    {
        auto historyIt = runtime.edgeHistory.find(edge);
        if (historyIt != runtime.edgeHistory.end() &&
            historyIt->second.outageEpochs <= options.state_edge_grace_epochs)
        {
            stateCandidates.insert(edge);
        }
    }

    set<ZhangGraphEdge> stateEdges =
        zhangRootComponentEdges(stateCandidates, options.reference_receiver);
    set<ZhangGraphEdge> observationEdges;
    std::set_intersection(
        availability.edges.begin(),
        availability.edges.end(),
        stateEdges.begin(),
        stateEdges.end(),
        std::inserter(observationEdges, observationEdges.begin())
    );

    auto retainOldTreeRootComponent =
        [&]()
        {
            set<ZhangGraphEdge> activeOldTreeEdges;
            std::set_intersection(
                stateEdges.begin(),
                stateEdges.end(),
                runtime.basis.treeEdges.begin(),
                runtime.basis.treeEdges.end(),
                std::inserter(activeOldTreeEdges, activeOldTreeEdges.begin())
            );

            set<ZhangGraphEdge> connectedTreeEdges =
                zhangRootComponentEdges(
                    activeOldTreeEdges,
                    runtime.basis.rootReceiver
                );

            set<string> connectedReceivers = {runtime.basis.rootReceiver};
            set<SatSys> connectedSatellites;
            for (const auto& edge : connectedTreeEdges)
            {
                connectedReceivers.insert(edge.receiver);
                connectedSatellites.insert(edge.satellite);
            }

            set<ZhangGraphEdge> safeEdges;
            for (const auto& edge : stateEdges)
            {
                if (connectedReceivers.find(edge.receiver) != connectedReceivers.end() &&
                    connectedSatellites.find(edge.satellite) != connectedSatellites.end())
                {
                    safeEdges.insert(edge);
                }
            }

            runtime.stateEdges = std::move(safeEdges);
            runtime.observationEdges.clear();
            std::set_intersection(
                observationEdges.begin(),
                observationEdges.end(),
                runtime.stateEdges.begin(),
                runtime.stateEdges.end(),
                std::inserter(
                    runtime.observationEdges,
                    runtime.observationEdges.begin()
                )
            );
        };

    if (stateEdges.empty())
    {
        set<ZhangGraphEdge> brokenTreeEdges;
        std::set_intersection(
            runtime.basis.treeEdges.begin(),
            runtime.basis.treeEdges.end(),
            availability.discontinuousEdges.begin(),
            availability.discontinuousEdges.end(),
            std::inserter(brokenTreeEdges, brokenTreeEdges.begin())
        );
        if (runtime.initialized && !brokenTreeEdges.empty())
        {
            set<string> affectedReceivers = runtime.basis.receivers;
            affectedReceivers.erase(runtime.basis.rootReceiver);
            set<SatSys> affectedSatellites = runtime.basis.satellites;
            if (resetZhangGraphPhaseCoordinates(
                    trace,
                    kfState,
                    sys,
                    options.baseline_observables,
                    affectedReceivers,
                    affectedSatellites
                ))
            {
                recordZhangPhaseReinitialisation(
                    kfState.time,
                    sys,
                    options.baseline_observables,
                    "root_component_arc_break",
                    affectedSatellites
                );
                runtime.basis = {};
                runtime.activeBasis = {};
                runtime.initialized = false;
                runtime.deferredEpochs = 0;
                runtime.datumVersion++;
            }
        }

        BOOST_LOG_TRIVIAL(warning)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " skipped: root receiver " << options.reference_receiver
            << " has no retained baseline-observable state component";
        runtime.observationEdges.clear();
        runtime.stateEdges.clear();
        return;
    }

    map<ZhangGraphEdge, double> activeQuality;
    map<ZhangGraphEdge, int> persistence;
    for (const auto& edge : stateEdges)
    {
        auto qualityIt = availability.edgeQuality.find(edge);
        if (qualityIt != availability.edgeQuality.end())
        {
            activeQuality[edge] = qualityIt->second;
        }

        auto historyIt = runtime.edgeHistory.find(edge);
        if (historyIt != runtime.edgeHistory.end())
        {
            persistence[edge] = historyIt->second.continuousEpochs;
        }
    }

    ZhangGraphBasis candidate =
        zhangBuildSpanningTree(
            stateEdges,
            options.reference_receiver,
            runtime.basis.treeEdges,
            activeQuality,
            options.prefer_historical_edges ? modelledEdges : set<ZhangGraphEdge>{},
            options.prefer_historical_edges ? persistence : map<ZhangGraphEdge, int>{}
        );

    if (!candidate.connected)
    {
        BOOST_LOG_TRIVIAL(warning)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " skipped: retained root component did not yield a spanning tree";
        runtime.observationEdges.clear();
        runtime.stateEdges.clear();
        return;
    }

    auto updateProductDatum = [&](const string& reason)
    {
        const ZhangGraphBasis oldProduct = runtime.productBasis;
        const map<ZhangGraphEdge, int> oldProductArcVersions =
            runtime.productArcVersions;
        ZhangGraphBasis nextProduct = zhangBuildSpanningTree(
            candidate.edges,
            candidate.rootReceiver,
            runtime.productBasis.treeEdges
        );
        if (!nextProduct.connected)
        {
            return false;
        }

        map<ZhangGraphEdge, int> nextProductArcVersions;
        for (const auto& edge : nextProduct.treeEdges)
        {
            auto history = runtime.edgeHistory.find(edge);
            if (history != runtime.edgeHistory.end())
            {
                nextProductArcVersions[edge] = history->second.arcVersion;
            }
        }

        set<ZhangGraphEdge> removedProductEdges;
        set<ZhangGraphEdge> addedProductEdges;
        set<ZhangGraphEdge> versionChangedEdges;
        if (runtime.productInitialized)
        {
            std::set_difference(
                oldProduct.treeEdges.begin(), oldProduct.treeEdges.end(),
                nextProduct.treeEdges.begin(), nextProduct.treeEdges.end(),
                std::inserter(removedProductEdges, removedProductEdges.begin())
            );
            std::set_difference(
                nextProduct.treeEdges.begin(), nextProduct.treeEdges.end(),
                oldProduct.treeEdges.begin(), oldProduct.treeEdges.end(),
                std::inserter(addedProductEdges, addedProductEdges.begin())
            );
            for (const auto& edge : nextProduct.treeEdges)
            {
                auto oldVersion = oldProductArcVersions.find(edge);
                auto newVersion = nextProductArcVersions.find(edge);
                if (oldProduct.treeEdges.find(edge) !=
                        oldProduct.treeEdges.end() &&
                    oldVersion != oldProductArcVersions.end() &&
                    newVersion != nextProductArcVersions.end() &&
                    oldVersion->second != newVersion->second)
                {
                    versionChangedEdges.insert(edge);
                }
            }
        }

        bool preserved = runtime.productInitialized;
        if (preserved)
        {
            set<string> commonReceivers;
            set<SatSys> commonSatellites;
            std::set_intersection(
                runtime.productBasis.receivers.begin(),
                runtime.productBasis.receivers.end(),
                candidate.receivers.begin(),
                candidate.receivers.end(),
                std::inserter(commonReceivers, commonReceivers.end())
            );
            std::set_intersection(
                runtime.productBasis.satellites.begin(),
                runtime.productBasis.satellites.end(),
                candidate.satellites.begin(),
                candidate.satellites.end(),
                std::inserter(commonSatellites, commonSatellites.end())
            );
            set<ZhangGraphEdge> restrictedOldTree;
            for (const auto& edge : runtime.productBasis.treeEdges)
            {
                if (commonReceivers.find(edge.receiver) !=
                        commonReceivers.end() &&
                    commonSatellites.find(edge.satellite) !=
                        commonSatellites.end())
                {
                    restrictedOldTree.insert(edge);
                }
            }
            // Added receiver/satellite leaves extend the persistent datum and
            // removed leaves contract it.  Neither operation changes the
            // integer potentials of the common connected nodes.  Only a break
            // that disconnects the old tree on the common node set requires a
            // new product-datum version.
            const std::size_t requiredCommon =
                commonReceivers.empty() && commonSatellites.empty()
                    ? 0
                    : commonReceivers.size() + commonSatellites.size() - 1;
            preserved =
                runtime.productBasis.rootReceiver == candidate.rootReceiver &&
                restrictedOldTree.size() == requiredCommon &&
                versionChangedEdges.empty() &&
                std::includes(
                    nextProduct.treeEdges.begin(), nextProduct.treeEdges.end(),
                    restrictedOldTree.begin(), restrictedOldTree.end()
                );
        }
        if (runtime.productInitialized && !preserved)
        {
            runtime.productDatumVersion++;
        }
        bool changed = !runtime.productInitialized ||
            runtime.productBasis.treeEdges != nextProduct.treeEdges ||
            runtime.productBasis.receivers != nextProduct.receivers ||
            runtime.productBasis.satellites != nextProduct.satellites ||
            !versionChangedEdges.empty();

        ZhangSatelliteSupportMetrics oldSatelliteMetrics;
        ZhangSatelliteSupportMetrics newSatelliteMetrics;
        if (changed && acsConfig.zhangPppAr.output_diagnostics)
        {
            oldSatelliteMetrics = zhangSatelliteSupportMetrics(oldProduct.edges);
            newSatelliteMetrics = zhangSatelliteSupportMetrics(nextProduct.edges);
        }

        struct ProductEdgeDiagnostic
        {
            std::optional<ZhangGraphEdge> oldEdge;
            std::optional<ZhangGraphEdge> newEdge;
            string                        eventReason;
            string                        signal = "ALL_BASELINE";
            int                           oldArcVersion = -1;
            int                           newArcVersion = -1;
            int                           oldAlternativePaths = 0;
            int                           newAlternativePaths = 0;
        };
        vector<ZhangGraphEdge> oldChanged(
            removedProductEdges.begin(), removedProductEdges.end()
        );
        oldChanged.insert(
            oldChanged.end(),
            versionChangedEdges.begin(), versionChangedEdges.end()
        );
        vector<ZhangGraphEdge> newChanged(
            addedProductEdges.begin(), addedProductEdges.end()
        );
        newChanged.insert(
            newChanged.end(),
            versionChangedEdges.begin(), versionChangedEdges.end()
        );

        auto rawContainsReceiver = [&](const string& receiver)
        {
            return std::any_of(
                availability.rawEdges.begin(), availability.rawEdges.end(),
                [&](const auto& edge) { return edge.receiver == receiver; }
            );
        };
        auto rawContainsSatellite = [&](const SatSys& satellite)
        {
            return std::any_of(
                availability.rawEdges.begin(), availability.rawEdges.end(),
                [&](const auto& edge) { return edge.satellite == satellite; }
            );
        };
        auto classifyOldEdge = [&](const ZhangGraphEdge& edge,
                                   int oldAlternativePaths)
        {
            if (versionChangedEdges.find(edge) != versionChangedEdges.end() ||
                availability.discontinuousEdges.find(edge) !=
                    availability.discontinuousEdges.end())
            {
                return string("CONFIRMED_CYCLE_SLIP");
            }
            if (availability.qcExcludedEdges.find(edge) !=
                    availability.qcExcludedEdges.end() ||
                availability.elevationExcludedEdges.find(edge) !=
                    availability.elevationExcludedEdges.end())
            {
                return string("STATION_QC_REMOVAL");
            }
            if (availability.signalUnavailableEdges.find(edge) !=
                    availability.signalUnavailableEdges.end())
            {
                return string("TEMPORARY_OBSERVATION_LOSS");
            }
            if (!rawContainsSatellite(edge.satellite))
            {
                return string("SATELLITE_RISE_SET");
            }
            if (!rawContainsReceiver(edge.receiver) ||
                availability.rawEdges.find(edge) == availability.rawEdges.end())
            {
                return string("TEMPORARY_OBSERVATION_LOSS");
            }
            if (oldAlternativePaths == 0)
            {
                return string("PRODUCT_EDGE_NO_ALTERNATIVE_SUPPORT");
            }
            if (nextProduct.satellites.size() < oldProduct.satellites.size() ||
                nextProduct.receivers.size() < oldProduct.receivers.size())
            {
                return string("COMPONENT_SPLIT");
            }
            return string("TREE_REOPTIMIZATION");
        };

        vector<ProductEdgeDiagnostic> edgeDiagnostics;
        const size_t diagnosticCount = std::max(
            oldChanged.size(), newChanged.size()
        );
        set<string> classifiedReasons;
        for (size_t index = 0; index < diagnosticCount; index++)
        {
            ProductEdgeDiagnostic diagnostic;
            if (index < oldChanged.size())
            {
                diagnostic.oldEdge = oldChanged[index];
                auto version = oldProductArcVersions.find(*diagnostic.oldEdge);
                if (version != oldProductArcVersions.end())
                {
                    diagnostic.oldArcVersion = version->second;
                }
                diagnostic.oldAlternativePaths =
                    zhangAlternativePhysicalPathCount(
                        oldProduct.edges, *diagnostic.oldEdge
                    );
                diagnostic.eventReason = classifyOldEdge(
                    *diagnostic.oldEdge,
                    diagnostic.oldAlternativePaths
                );
                auto signals = availability.discontinuitySignals.find(
                    *diagnostic.oldEdge
                );
                if (signals != availability.discontinuitySignals.end())
                {
                    std::ostringstream listed;
                    for (E_ObsCode code : signals->second)
                    {
                        listed << (listed.tellp() > 0 ? "," : "")
                               << enum_to_string(code);
                    }
                    diagnostic.signal = listed.str();
                }
            }
            if (index < newChanged.size())
            {
                diagnostic.newEdge = newChanged[index];
                auto version = nextProductArcVersions.find(*diagnostic.newEdge);
                if (version != nextProductArcVersions.end())
                {
                    diagnostic.newArcVersion = version->second;
                }
                diagnostic.newAlternativePaths =
                    zhangAlternativePhysicalPathCount(
                        nextProduct.edges, *diagnostic.newEdge
                    );
            }
            if (diagnostic.eventReason.empty())
            {
                diagnostic.eventReason =
                    nextProduct.satellites.size() > oldProduct.satellites.size() ||
                    nextProduct.receivers.size() > oldProduct.receivers.size()
                        ? "COMPONENT_MERGE"
                        : "TREE_REOPTIMIZATION";
            }
            classifiedReasons.insert(diagnostic.eventReason);
            edgeDiagnostics.push_back(std::move(diagnostic));
        }

        struct TreeSupportSummary
        {
            int minimum = 0;
            int maximum = 0;
            int bridgeCount = 0;
            double mean = 0;
        };
        auto treeSupportSummary = [&](const ZhangGraphBasis& basis)
        {
            TreeSupportSummary summary;
            if (basis.treeEdges.empty())
            {
                return summary;
            }
            summary.minimum = std::numeric_limits<int>::max();
            long long total = 0;
            for (const auto& edge : basis.treeEdges)
            {
                int support = 1 + zhangAlternativePhysicalPathCount(
                    basis.edges, edge
                );
                summary.minimum = std::min(summary.minimum, support);
                summary.maximum = std::max(summary.maximum, support);
                summary.bridgeCount += support == 1;
                total += support;
            }
            summary.mean = static_cast<double>(total) / basis.treeEdges.size();
            return summary;
        };
        TreeSupportSummary oldTreeSupport;
        TreeSupportSummary newTreeSupport;
        if (changed && acsConfig.zhangPppAr.output_diagnostics)
        {
            oldTreeSupport = treeSupportSummary(oldProduct);
            newTreeSupport = treeSupportSummary(nextProduct);
        }

        runtime.productBasis = std::move(nextProduct);
        runtime.productArcVersions = std::move(nextProductArcVersions);
        runtime.productInitialized = true;
        if (changed && acsConfig.zhangPppAr.output_diagnostics)
        {
            std::ostringstream reasons;
            for (const auto& classified : classifiedReasons)
            {
                reasons << (reasons.tellp() > 0 ? "," : "") << classified;
            }
            trace << "\nZHANG_PRODUCT_DATUM_EVENT time="
                  << kfState.time.to_string(0)
                  << " system=" << enum_to_string(sys)
                  << " reason=" << reason
                  << " classified_reasons="
                  << (classifiedReasons.empty() ? "INITIALISE" : reasons.str())
                  << " datum_version=" << runtime.productDatumVersion
                  << " continuity_preserved=" << preserved
                  << " component_id="
                  << zhangIntegerComponentId(runtime.productBasis)
                  << " product_tree_edges="
                  << runtime.productBasis.treeEdges.size()
                  << " product_receivers="
                  << runtime.productBasis.receivers.size()
                  << " product_satellites="
                  << runtime.productBasis.satellites.size()
                  << " old_product_tree_edges=" << edgeList(removedProductEdges)
                  << " new_product_tree_edges=" << edgeList(addedProductEdges)
                  << " arc_version_changes=" << edgeList(versionChangedEdges)
                  << " old_tree_min_support=" << oldTreeSupport.minimum
                  << " old_tree_mean_support=" << oldTreeSupport.mean
                  << " old_tree_bridge_count=" << oldTreeSupport.bridgeCount
                  << " new_tree_min_support=" << newTreeSupport.minimum
                  << " new_tree_mean_support=" << newTreeSupport.mean
                  << " new_tree_bridge_count=" << newTreeSupport.bridgeCount
                  << " old_satellite_bridge_count="
                  << oldSatelliteMetrics.bridgeEdges.size()
                  << " new_satellite_bridge_count="
                  << newSatelliteMetrics.bridgeEdges.size()
                  << " old_satellite_edge_connectivity="
                  << oldSatelliteMetrics.edgeConnectivity
                  << " new_satellite_edge_connectivity="
                  << newSatelliteMetrics.edgeConnectivity;

            for (const auto& diagnostic : edgeDiagnostics)
            {
                auto edgeText = [](const std::optional<ZhangGraphEdge>& edge)
                {
                    return edge
                        ? edge->receiver + ":" + edge->satellite.id()
                        : string("NONE");
                };
                trace << "\nZHANG_PRODUCT_DATUM_EDGE_EVENT time="
                      << kfState.time.to_string(0)
                      << " system=" << enum_to_string(sys)
                      << " old_product_tree_edge="
                      << edgeText(diagnostic.oldEdge)
                      << " new_product_tree_edge="
                      << edgeText(diagnostic.newEdge)
                      << " event_reason=" << diagnostic.eventReason
                      << " receiver="
                      << (diagnostic.oldEdge
                              ? diagnostic.oldEdge->receiver
                              : diagnostic.newEdge
                                    ? diagnostic.newEdge->receiver
                                    : "NONE")
                      << " satellite="
                      << (diagnostic.oldEdge
                              ? diagnostic.oldEdge->satellite.id()
                              : diagnostic.newEdge
                                    ? diagnostic.newEdge->satellite.id()
                                    : "NONE")
                      << " signal=" << diagnostic.signal
                      << " old_arc_version=" << diagnostic.oldArcVersion
                      << " new_arc_version=" << diagnostic.newArcVersion
                      << " old_support_count="
                      << (diagnostic.oldEdge
                              ? 1 + diagnostic.oldAlternativePaths : 0)
                      << " new_support_count="
                      << (diagnostic.newEdge
                              ? 1 + diagnostic.newAlternativePaths : 0)
                      << " old_alternative_exact_paths="
                      << diagnostic.oldAlternativePaths
                      << " new_alternative_exact_paths="
                      << diagnostic.newAlternativePaths
                      << " bridge_before="
                      << (diagnostic.oldEdge &&
                          diagnostic.oldAlternativePaths == 0)
                      << " bridge_after="
                      << (diagnostic.newEdge &&
                          diagnostic.newAlternativePaths == 0)
                      << " component_size_before="
                      << oldSatelliteMetrics.largestComponent
                      << " component_size_after="
                      << newSatelliteMetrics.largestComponent
                      << " datum_version_changed=" << !preserved;
            }

            trace << "\nZHANG_PRODUCT_GRAPH_REDUNDANCY time="
                  << kfState.time.to_string(0)
                  << " system=" << enum_to_string(sys)
                  << " product_satellites="
                  << newSatelliteMetrics.satellites.size()
                  << " product_relation_edges="
                  << newSatelliteMetrics.supportCounts.size()
                  << " mean_support_count="
                  << newSatelliteMetrics.meanSupport
                  << " min_support_count="
                  << newSatelliteMetrics.minimumSupport
                  << " max_support_count="
                  << newSatelliteMetrics.maximumSupport
                  << " bridge_count="
                  << newSatelliteMetrics.bridgeEdges.size()
                  << " edge_connectivity="
                  << newSatelliteMetrics.edgeConnectivity
                  << " component_count="
                  << newSatelliteMetrics.componentCount
                  << " largest_component="
                  << newSatelliteMetrics.largestComponent
                  << " product_tree_min_support="
                  << newTreeSupport.minimum
                  << " product_tree_mean_support="
                  << newTreeSupport.mean
                  << " product_tree_bridge_count="
                  << newTreeSupport.bridgeCount
                  << " datum_version=" << runtime.productDatumVersion;
        }
        return true;
    };

    bool hasEstimatedPhaseState = false;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.Sat.sys != sys)
        {
            continue;
        }

        bool targetCode = zhangFullRankUsesObservable(
            static_cast<E_ObsCode>(key.num),
            options.baseline_observables
        );
        if (targetCode && (key.type == KF::PHASE_BIAS || key.type == KF::AMBIGUITY))
        {
            hasEstimatedPhaseState = true;
            break;
        }
    }

    if (!runtime.initialized || !hasEstimatedPhaseState)
    {
        updateProductDatum("initialise");
        runtime.basis            = candidate;
        runtime.activeBasis      = candidate;
        runtime.observationEdges = observationEdges;
        runtime.stateEdges       = stateEdges;
        runtime.initialized = true;
        runtime.deferredEpochs = 0;
        traceGraphEvent(
            "initialise",
            "initial_component",
            {},
            candidate.treeEdges,
            {},
            {},
            0,
            true
        );
        traceCanonicalAudit(runtime.basis, "initialise", true);

        BOOST_LOG_TRIVIAL(info)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " action=initialise"
            << " nodes=" << candidate.receivers.size() + candidate.satellites.size()
            << " edges=" << candidate.edges.size()
            << " tree_edges=" << candidate.treeEdges.size()
            << " cycles=" << candidate.edges.size() - candidate.treeEdges.size();
        return;
    }

    if (candidate.treeEdges == runtime.basis.treeEdges)
    {
        updateProductDatum("graph_update");
        runtime.activeBasis      = candidate;
        runtime.observationEdges = observationEdges;
        runtime.stateEdges       = stateEdges;
        runtime.deferredEpochs = 0;
        return;
    }

    // A new receiver or satellite may extend the existing tree before any state for that node
    // exists.  Such a leaf extension leaves every existing coordinate unchanged.
    bool oldTreeRetained = std::includes(
        candidate.treeEdges.begin(),
        candidate.treeEdges.end(),
        runtime.basis.treeEdges.begin(),
        runtime.basis.treeEdges.end()
    );
    bool leafExtension = oldTreeRetained;
    if (leafExtension)
    {
        for (const auto& edge : candidate.treeEdges)
        {
            if (runtime.basis.treeEdges.find(edge) != runtime.basis.treeEdges.end())
            {
                continue;
            }

            bool newReceiver =
                runtime.basis.receivers.find(edge.receiver) == runtime.basis.receivers.end();
            bool newSatellite =
                runtime.basis.satellites.find(edge.satellite) == runtime.basis.satellites.end();
            if (!newReceiver && !newSatellite)
            {
                leafExtension = false;
                break;
            }
        }
    }

    if (leafExtension)
    {
        set<ZhangGraphEdge> addedTreeEdges;
        std::set_difference(
            candidate.treeEdges.begin(),
            candidate.treeEdges.end(),
            runtime.basis.treeEdges.begin(),
            runtime.basis.treeEdges.end(),
            std::inserter(addedTreeEdges, addedTreeEdges.begin())
        );
        updateProductDatum("leaf_extension");
        runtime.basis            = candidate;
        runtime.activeBasis      = candidate;
        runtime.observationEdges = observationEdges;
        runtime.stateEdges       = stateEdges;
        runtime.deferredEpochs = 0;
        traceGraphEvent(
            "leaf_extension",
            "new_leaf_node",
            {},
            addedTreeEdges,
            {},
            {},
            0,
            true
        );
        traceCanonicalAudit(runtime.basis, "leaf_extension", true);
        BOOST_LOG_TRIVIAL(info)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " action=leaf_extension"
            << " tree_edges=" << candidate.treeEdges.size();
        return;
    }

    auto detachedNodes = [&]()
    {
        set<ZhangGraphEdge> retainedOldTree;
        std::set_intersection(
            runtime.basis.treeEdges.begin(),
            runtime.basis.treeEdges.end(),
            stateEdges.begin(),
            stateEdges.end(),
            std::inserter(retainedOldTree, retainedOldTree.begin())
        );
        set<ZhangGraphEdge> rootTree =
            zhangRootComponentEdges(retainedOldTree, runtime.basis.rootReceiver);

        set<string> rootReceivers = {runtime.basis.rootReceiver};
        set<SatSys> rootSatellites;
        for (const auto& edge : rootTree)
        {
            rootReceivers.insert(edge.receiver);
            rootSatellites.insert(edge.satellite);
        }

        set<string> affectedReceivers;
        set<SatSys> affectedSatellites;
        std::set_difference(
            runtime.basis.receivers.begin(),
            runtime.basis.receivers.end(),
            rootReceivers.begin(),
            rootReceivers.end(),
            std::inserter(affectedReceivers, affectedReceivers.begin())
        );
        std::set_difference(
            runtime.basis.satellites.begin(),
            runtime.basis.satellites.end(),
            rootSatellites.begin(),
            rootSatellites.end(),
            std::inserter(affectedSatellites, affectedSatellites.begin())
        );
        return std::make_pair(affectedReceivers, affectedSatellites);
    };

    auto localReinitialise = [&](const string& reason)
    {
        auto [affectedReceivers, affectedSatellites] = detachedNodes();
        if (affectedReceivers.empty() && affectedSatellites.empty())
        {
            return false;
        }

        set<ZhangGraphEdge> removedTreeEdges;
        set<ZhangGraphEdge> replacementEdges;
        std::set_difference(
            runtime.basis.treeEdges.begin(),
            runtime.basis.treeEdges.end(),
            candidate.treeEdges.begin(),
            candidate.treeEdges.end(),
            std::inserter(removedTreeEdges, removedTreeEdges.begin())
        );
        std::set_difference(
            candidate.treeEdges.begin(),
            candidate.treeEdges.end(),
            runtime.basis.treeEdges.begin(),
            runtime.basis.treeEdges.end(),
            std::inserter(replacementEdges, replacementEdges.begin())
        );
        int removedIntegerColumns = 0;
        for (const auto& [key, index] : kfState.kfIndexMap)
        {
            removedIntegerColumns +=
                key.type == KF::AMBIGUITY &&
                key.Sat.sys == sys &&
                zhangFullRankUsesObservable(
                    static_cast<E_ObsCode>(key.num),
                    options.baseline_observables
                ) &&
                (affectedReceivers.find(key.str) != affectedReceivers.end() ||
                 affectedSatellites.find(key.Sat) != affectedSatellites.end());
        }

        if (!resetZhangGraphPhaseCoordinates(
                trace,
                kfState,
                sys,
                options.baseline_observables,
                affectedReceivers,
                affectedSatellites
            ))
        {
            return false;
        }

        const size_t preservedReceivers =
            runtime.basis.receivers.size() - affectedReceivers.size();
        const size_t preservedSatellites =
            runtime.basis.satellites.size() - affectedSatellites.size();

        updateProductDatum("local_reinitialise");
        runtime.basis            = candidate;
        runtime.activeBasis      = candidate;
        runtime.observationEdges = observationEdges;
        runtime.stateEdges       = stateEdges;
        runtime.deferredEpochs   = 0;
        runtime.datumVersion++;
        traceGraphEvent(
            "local_reinitialise",
            reason,
            removedTreeEdges,
            replacementEdges,
            affectedReceivers,
            affectedSatellites,
            removedIntegerColumns,
            false
        );
        traceCanonicalAudit(runtime.basis, "local_reinitialise", false);
        recordZhangPhaseReinitialisation(
            kfState.time,
            sys,
            options.baseline_observables,
            reason,
            affectedSatellites
        );
        BOOST_LOG_TRIVIAL(warning)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " action=local_reinitialise"
            << " reason=" << reason
            << " affected_receivers=" << affectedReceivers.size()
            << " affected_satellites=" << affectedSatellites.size()
            << " preserved_receivers=" << preservedReceivers
            << " preserved_satellites=" << preservedSatellites
            << " phase_datum_discontinuity=local";
        return true;
    };

    set<ZhangGraphEdge> brokenTreeEdges;
    std::set_intersection(
        runtime.basis.treeEdges.begin(),
        runtime.basis.treeEdges.end(),
        availability.discontinuousEdges.begin(),
        availability.discontinuousEdges.end(),
        std::inserter(brokenTreeEdges, brokenTreeEdges.begin())
    );
    if (!brokenTreeEdges.empty())
    {
        if (localReinitialise("tree_edge_arc_break"))
        {
            return;
        }

        retainOldTreeRootComponent();
        BOOST_LOG_TRIVIAL(error)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " action=defer tree exchange: local arc-break reset failed"
            << ", broken_tree_edges=" << brokenTreeEdges.size()
            << ", retained_safe_edges=" << runtime.observationEdges.size();
        return;
    }

    if (options.core_skeleton)
    {
        runtime.deferredEpochs = 0;
        retainOldTreeRootComponent();
        BOOST_LOG_TRIVIAL(warning)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " action=core_skeleton_hold"
            << " reason=non_leaf_tree_change"
            << " retained_observation_edges=" << runtime.observationEdges.size();
        return;
    }

    bool newTreeRepresented = std::includes(
        modelledEdges.begin(),
        modelledEdges.end(),
        candidate.treeEdges.begin(),
        candidate.treeEdges.end()
    );
    if (!newTreeRepresented)
    {
        runtime.deferredEpochs++;
        if (runtime.deferredEpochs >= std::max(1, options.reference_outage_epochs) &&
            localReinitialise("replacement_edge_without_prior_state"))
        {
            return;
        }

        retainOldTreeRootComponent();
        BOOST_LOG_TRIVIAL(warning)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " action=defer tree exchange: a replacement edge has no prior state"
            << ", retained_safe_edges=" << runtime.observationEdges.size();
        return;
    }

    ZhangGraphBasis oldBasis = runtime.basis;
    oldBasis.edges           = modelledEdges;
    oldBasis.receivers.clear();
    oldBasis.satellites.clear();
    for (const auto& edge : modelledEdges)
    {
        oldBasis.receivers.insert(edge.receiver);
        oldBasis.satellites.insert(edge.satellite);
    }

    ZhangGraphBasis transformedBasis = candidate;
    transformedBasis.edges           = modelledEdges;

    if (!transformZhangGraphBasis(
            trace,
            kfState,
            sys,
            options.baseline_observables,
            oldBasis,
            transformedBasis
        ))
    {
        runtime.deferredEpochs++;
        if (runtime.deferredEpochs >= std::max(1, options.reference_outage_epochs) &&
            localReinitialise("exact_state_transform_unavailable"))
        {
            return;
        }

        retainOldTreeRootComponent();
        BOOST_LOG_TRIVIAL(warning)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " action=defer tree exchange: exact state transform failed"
            << ", retained_safe_edges=" << runtime.observationEdges.size();
        return;
    }

    set<ZhangGraphEdge> removedTreeEdges;
    set<ZhangGraphEdge> replacementEdges;
    std::set_difference(
        oldBasis.treeEdges.begin(),
        oldBasis.treeEdges.end(),
        candidate.treeEdges.begin(),
        candidate.treeEdges.end(),
        std::inserter(removedTreeEdges, removedTreeEdges.begin())
    );
    std::set_difference(
        candidate.treeEdges.begin(),
        candidate.treeEdges.end(),
        oldBasis.treeEdges.begin(),
        oldBasis.treeEdges.end(),
        std::inserter(replacementEdges, replacementEdges.begin())
    );

    updateProductDatum("tree_exchange");
    runtime.basis            = transformedBasis;
    runtime.activeBasis      = candidate;
    runtime.observationEdges = observationEdges;
    runtime.stateEdges       = stateEdges;
    runtime.deferredEpochs = 0;
    traceGraphEvent(
        "tree_exchange",
        "exact_state_transform",
        removedTreeEdges,
        replacementEdges,
        {},
        {},
        0,
        true
    );
    // transformedBasis retains stale ambiguity arcs solely to make the state
    // transform dimension preserving.  Canonical integer coordinates belong
    // to the active retained component represented by candidate/stateEdges.
    traceCanonicalAudit(candidate, "tree_exchange", true);

    BOOST_LOG_TRIVIAL(info)
        << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
        << " action=tree_exchange"
        << " modelled_edges=" << modelledEdges.size()
        << " tree_edges=" << transformedBasis.treeEdges.size()
        << " cycles=" << modelledEdges.size() - transformedBasis.treeEdges.size();
}
}  // namespace

void updateZhangFullRankReferences(
    Trace&       trace,
    ReceiverMap& receiverMap,
    KFState&     kfState
)
{
    if (!acsConfig.zhangFullRank.enable)
    {
        return;
    }

    for (auto& [sys, options] : acsConfig.zhangFullRank.sysOpts)
    {
        if (!acsConfig.process_sys[sys])
        {
            continue;
        }

        ReferenceAvailability availability =
            referenceAvailability(receiverMap, sys, options.baseline_observables);
        if (availability.satellitesByReceiver.empty())
        {
            continue;
        }

        if (options.use_spanning_tree)
        {
            updateZhangGraphBasis(trace, kfState, sys, options, availability);
            continue;
        }

        if (!options.auto_reference_switch)
        {
            continue;
        }

        set<SatSys> common = commonSatellites(availability);
        if (common.empty())
        {
            BOOST_LOG_TRIVIAL(warning)
                << "ZHANG_REFERENCE_SWITCH sys=" << enum_to_string(sys)
                << " skipped: no baseline satellite is common to all active receivers";
            continue;
        }

        auto& outage = outageStateMap[{&kfState, sys}];

        bool receiverAvailable =
            availability.satellitesByReceiver.find(options.reference_receiver) !=
            availability.satellitesByReceiver.end();

        SatSys oldSatellite(options.reference_satellite.c_str());
        bool satelliteAvailable = common.find(oldSatellite) != common.end();

        outage.receiverEpochs = receiverAvailable ? 0 : outage.receiverEpochs + 1;
        outage.satelliteEpochs = satelliteAvailable ? 0 : outage.satelliteEpochs + 1;

        bool changeReceiver = outage.receiverEpochs >= options.reference_outage_epochs;
        bool changeSatellite = outage.satelliteEpochs >= options.reference_outage_epochs;
        if (!changeReceiver && !changeSatellite)
        {
            continue;
        }

        vector<string> receiverChoices = {options.reference_receiver};
        if (changeReceiver)
        {
            receiverChoices =
                orderedReceivers(availability, options.reference_receiver_candidates);
        }

        vector<SatSys> satelliteChoices = {oldSatellite};
        if (changeSatellite)
        {
            satelliteChoices =
                orderedSatellites(
                    common,
                    availability.elevationScore,
                    options.reference_satellite_candidates
                );
        }

        if (receiverChoices.empty() || satelliteChoices.empty())
        {
            BOOST_LOG_TRIVIAL(warning)
                << "ZHANG_REFERENCE_SWITCH sys=" << enum_to_string(sys)
                << " skipped: no valid replacement reference";
            continue;
        }

        string oldReceiver = options.reference_receiver;

        bool hasEstimatedZhangState = false;
        for (auto& [key, index] : kfState.kfIndexMap)
        {
            if (key.type == KF::AMBIGUITY && key.Sat.sys == sys)
            {
                hasEstimatedZhangState = true;
                break;
            }
        }

        string newReceiver;
        SatSys newSatellite;
        bool   transformed = !hasEstimatedZhangState;

        for (auto& receiverCandidate : receiverChoices)
        {
            for (auto& satelliteCandidate : satelliteChoices)
            {
                if (hasEstimatedZhangState &&
                    !transformZhangDatum(
                        trace,
                        kfState,
                        sys,
                        options.baseline_observables,
                        oldReceiver,
                        oldSatellite,
                        receiverCandidate,
                        satelliteCandidate
                    ))
                {
                    continue;
                }

                newReceiver  = receiverCandidate;
                newSatellite = satelliteCandidate;
                transformed  = hasEstimatedZhangState;
                break;
            }

            if (!newReceiver.empty())
            {
                break;
            }
        }

        if (newReceiver.empty() || newSatellite.prn <= 0)
        {
            BOOST_LOG_TRIVIAL(warning)
                << "ZHANG_REFERENCE_SWITCH sys=" << enum_to_string(sys)
                << " deferred: no candidate has the complete phase-state tree required for an "
                   "exact transform; retaining "
                << oldReceiver << "/" << oldSatellite.id();
            continue;
        }

        options.reference_receiver  = newReceiver;
        options.reference_satellite = newSatellite.id();
        outage = {};

        BOOST_LOG_TRIVIAL(info)
            << "ZHANG_REFERENCE_SWITCH sys=" << enum_to_string(sys)
            << " old_receiver=" << oldReceiver
            << " new_receiver=" << newReceiver
            << " old_satellite=" << oldSatellite.id()
            << " new_satellite=" << newSatellite.id()
            << " transformed=" << transformed;
    }
}

bool zhangGraphModelsObservation(
    const KFState&     kfState,
    const std::string& receiver,
    const SatSys&      satellite,
    E_ObsCode          code
)
{
    if (!acsConfig.zhangFullRank.enable)
    {
        return true;
    }

    auto optionsIt = acsConfig.zhangFullRank.sysOpts.find(satellite.sys);
    if (optionsIt == acsConfig.zhangFullRank.sysOpts.end() ||
        !zhangFullRankUsesObservable(code, optionsIt->second.baseline_observables) ||
        !optionsIt->second.use_spanning_tree)
    {
        return true;
    }

    auto stateIt = graphStateMap.find({&kfState, satellite.sys});
    if (stateIt == graphStateMap.end() || !stateIt->second.initialized)
    {
        return false;
    }

    return stateIt->second.observationEdges.find({receiver, satellite}) !=
           stateIt->second.observationEdges.end();
}

bool zhangGraphRetainsAmbiguity(
    const KFState&     kfState,
    const std::string& receiver,
    const SatSys&      satellite,
    E_ObsCode          code
)
{
    auto optionsIt = acsConfig.zhangFullRank.sysOpts.find(satellite.sys);
    if (optionsIt == acsConfig.zhangFullRank.sysOpts.end() ||
        !optionsIt->second.use_spanning_tree)
    {
        return true;
    }

    auto stateIt = graphStateMap.find({&kfState, satellite.sys});
    if (stateIt == graphStateMap.end() || !stateIt->second.initialized)
    {
        return false;
    }

    ZhangGraphEdge edge{receiver, satellite};
    return stateIt->second.observationEdges.find(edge) !=
               stateIt->second.observationEdges.end() &&
           stateIt->second.basis.treeEdges.find(edge) ==
           stateIt->second.basis.treeEdges.end();
}

bool zhangGraphProductSatelliteActive(
    const KFState& kfState,
    const SatSys&  satellite
)
{
    auto optionsIt = acsConfig.zhangFullRank.sysOpts.find(satellite.sys);
    if (optionsIt == acsConfig.zhangFullRank.sysOpts.end() ||
        !optionsIt->second.use_spanning_tree)
    {
        return true;
    }

    auto stateIt = graphStateMap.find({&kfState, satellite.sys});
    if (stateIt == graphStateMap.end() || !stateIt->second.initialized)
    {
        return false;
    }

    for (const auto& edge : stateIt->second.stateEdges)
    {
        if (edge.satellite == satellite)
        {
            return true;
        }
    }
    return false;
}

bool zhangGraphIntegerContext(
    const KFState&             kfState,
    E_Sys                      system,
    ZhangGraphIntegerContext& context
)
{
    context = {};
    auto stateIt = graphStateMap.find({&kfState, system});
    if (stateIt == graphStateMap.end() || !stateIt->second.initialized)
    {
        return false;
    }

    context.basis   = stateIt->second.activeBasis.connected
        ? stateIt->second.activeBasis
        : stateIt->second.basis;
    context.productBasis = stateIt->second.productBasis;
    context.eventId = stateIt->second.eventCounter;
    context.productDatumVersion = stateIt->second.productDatumVersion;
    for (const auto& [edge, history] : stateIt->second.edgeHistory)
    {
        context.arcVersions[edge] = history.arcVersion;
    }
    context.initialized = true;
    return true;
}

void cloneZhangGraphRuntime(
    const KFState& source,
    const KFState& destination
)
{
    eraseZhangGraphRuntime(destination);
    vector<pair<E_Sys, GraphRuntimeState>> copies;
    for (const auto& [identity, runtime] : graphStateMap)
    {
        if (identity.first == &source)
        {
            copies.emplace_back(identity.second, runtime);
        }
    }
    for (auto& [system, runtime] : copies)
    {
        graphStateMap[{&destination, system}] = std::move(runtime);
    }
}

void eraseZhangGraphRuntime(const KFState& state)
{
    for (auto it = graphStateMap.begin(); it != graphStateMap.end();)
    {
        if (it->first.first == &state)
        {
            it = graphStateMap.erase(it);
        }
        else
        {
            ++it;
        }
    }
}
