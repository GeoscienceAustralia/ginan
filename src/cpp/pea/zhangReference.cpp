#include "pea/zhangReference.hpp"

#include <algorithm>
#include <map>
#include <set>
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
    ZhangGraphBasis          basis;
    set<ZhangGraphEdge>      activeEdges;
    bool                     initialized = false;
    int                      deferredEpochs = 0;
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
            if (obs.Sat.sys != sys || obs.exclude)
            {
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
                continue;
            }

            bool usable = true;
            for (E_ObsCode code : baselineObservables)
            {
                usable &= signalIsUsable(obs, code);
            }

            if (!usable)
            {
                continue;
            }

            availability.satellitesByReceiver[id].insert(obs.Sat);
            ZhangGraphEdge edge{id, obs.Sat};
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
        recordZhangExactPhaseTransform(
            kfState.time,
            sys,
            code,
            satellite,
            -(newPhase - oldPhase)
        );
    }

    return true;
}

bool resetZhangGraphPhaseCoordinates(
    Trace&                   trace,
    KFState&                 kfState,
    E_Sys                    sys,
    const vector<E_ObsCode>& baselineObservables
)
{
    map<KFKey, map<KFKey, double>> transform;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        bool targetPhase =
            key.type == KF::PHASE_BIAS &&
            key.Sat.sys == sys &&
            zhangFullRankUsesObservable(
                static_cast<E_ObsCode>(key.num),
                baselineObservables
            );
        bool targetAmbiguity =
            key.type == KF::AMBIGUITY &&
            key.Sat.sys == sys &&
            zhangFullRankUsesObservable(
                static_cast<E_ObsCode>(key.num),
                baselineObservables
            );

        if (!targetPhase && !targetAmbiguity)
        {
            transform[key][key] = 1;
        }
    }

    return !transform.empty() &&
           kfState.applyStateTransform(
               trace,
               transform,
               "Zhang graph phase-coordinate reinitialisation"
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

    auto retainOldTreeRootComponent =
        [&](const set<ZhangGraphEdge>& currentEdges)
        {
            set<ZhangGraphEdge> activeOldTreeEdges;
            std::set_intersection(
                currentEdges.begin(),
                currentEdges.end(),
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
            for (const auto& edge : currentEdges)
            {
                if (connectedReceivers.find(edge.receiver) != connectedReceivers.end() &&
                    connectedSatellites.find(edge.satellite) != connectedSatellites.end())
                {
                    safeEdges.insert(edge);
                }
            }

            runtime.activeEdges = std::move(safeEdges);
        };

    set<ZhangGraphEdge> activeEdges =
        zhangRootComponentEdges(availability.edges, options.reference_receiver);
    if (activeEdges.empty())
    {
        BOOST_LOG_TRIVIAL(warning)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " skipped: root receiver " << options.reference_receiver
            << " has no active baseline-observable component";
        runtime.activeEdges.clear();
        return;
    }

    map<ZhangGraphEdge, double> activeQuality;
    for (const auto& edge : activeEdges)
    {
        auto qualityIt = availability.edgeQuality.find(edge);
        if (qualityIt != availability.edgeQuality.end())
        {
            activeQuality[edge] = qualityIt->second;
        }
    }

    ZhangGraphBasis candidate =
        zhangBuildSpanningTree(
            activeEdges,
            options.reference_receiver,
            runtime.basis.treeEdges,
            activeQuality
        );

    if (!candidate.connected)
    {
        BOOST_LOG_TRIVIAL(warning)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " skipped: active root component did not yield a spanning tree";
        runtime.activeEdges.clear();
        return;
    }

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
        runtime.basis       = candidate;
        runtime.activeEdges = activeEdges;
        runtime.initialized = true;
        runtime.deferredEpochs = 0;

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
        runtime.activeEdges = activeEdges;
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
        runtime.basis       = candidate;
        runtime.activeEdges = activeEdges;
        runtime.deferredEpochs = 0;
        BOOST_LOG_TRIVIAL(info)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " action=leaf_extension"
            << " tree_edges=" << candidate.treeEdges.size();
        return;
    }

    // Reconstruct the complete edge set represented by the current filter: old tree edges plus
    // one edge for every retained fundamental-cycle ambiguity.
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
            resetZhangGraphPhaseCoordinates(
                trace,
                kfState,
                sys,
                options.baseline_observables
            ))
        {
            runtime.basis          = candidate;
            runtime.activeEdges    = activeEdges;
            runtime.deferredEpochs = 0;
            recordZhangPhaseReinitialisation(
                kfState.time,
                sys,
                options.baseline_observables,
                "replacement_edge_without_prior_state"
            );
            BOOST_LOG_TRIVIAL(warning)
                << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
                << " action=reinitialise"
                << " reason=replacement edge has no prior state"
                << " nodes=" << candidate.receivers.size() + candidate.satellites.size()
                << " tree_edges=" << candidate.treeEdges.size()
                << " cycles=" << candidate.edges.size() - candidate.treeEdges.size()
                << " phase_datum_discontinuity=true";
            return;
        }

        retainOldTreeRootComponent(activeEdges);
        BOOST_LOG_TRIVIAL(warning)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " action=defer tree exchange: a replacement edge has no prior state"
            << ", retained_safe_edges=" << runtime.activeEdges.size();
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
            resetZhangGraphPhaseCoordinates(
                trace,
                kfState,
                sys,
                options.baseline_observables
            ))
        {
            runtime.basis          = candidate;
            runtime.activeEdges    = activeEdges;
            runtime.deferredEpochs = 0;
            recordZhangPhaseReinitialisation(
                kfState.time,
                sys,
                options.baseline_observables,
                "exact_state_transform_unavailable"
            );
            BOOST_LOG_TRIVIAL(warning)
                << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
                << " action=reinitialise"
                << " reason=exact state transform unavailable"
                << " nodes=" << candidate.receivers.size() + candidate.satellites.size()
                << " tree_edges=" << candidate.treeEdges.size()
                << " cycles=" << candidate.edges.size() - candidate.treeEdges.size()
                << " phase_datum_discontinuity=true";
            return;
        }

        retainOldTreeRootComponent(activeEdges);
        BOOST_LOG_TRIVIAL(warning)
            << "ZHANG_GRAPH_BASIS sys=" << enum_to_string(sys)
            << " action=defer tree exchange: exact state transform failed"
            << ", retained_safe_edges=" << runtime.activeEdges.size();
        return;
    }

    runtime.basis       = transformedBasis;
    runtime.activeEdges = activeEdges;
    runtime.deferredEpochs = 0;

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

    return stateIt->second.activeEdges.find({receiver, satellite}) !=
           stateIt->second.activeEdges.end();
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
    return stateIt->second.activeEdges.find(edge) != stateIt->second.activeEdges.end() &&
           stateIt->second.basis.treeEdges.find(edge) ==
               stateIt->second.basis.treeEdges.end();
}
