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
#include <chrono>
#include <cctype>
#include <cstdint>
#include <deque>
#include <iostream>
#include <iomanip>
#include <limits>
#include <math.h>
#include <optional>
#include <random>
#include <set>
#include <sstream>
#include <mutex>
#include <numeric>
#include <stdexcept>
#include <tuple>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/math/distributions/chi_squared.hpp>
#include <boost/serialization/deque.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/serialization/vector.hpp>
#include "ambres/GNSSambres.hpp"
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/biases.hpp"
#include "common/common.hpp"
#include "common/eigenIncluder.hpp"
#include "common/linearCombo.hpp"
#include "common/observations.hpp"
#include "common/phaseClockOsb.hpp"
#include "common/receiver.hpp"
#include "common/trace.hpp"
#include "common/zhangFullRank.hpp"
#include "common/zhangCheckpoint.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "common/zhangIarGainAudit.hpp"
#include "common/zhangIntegerConditioner.hpp"
#include "common/zhangIntegerCandidateNis.hpp"
#include "common/zhangLambdaBeam.hpp"
#include "common/zhangProductRelationBasis.hpp"
#include "common/zhangProductIntegerLedger.hpp"
#include "common/zhangProductIntegerCandidateGenerator.hpp"
#include "common/zhangFullProductLatticeOracle.hpp"
#include "common/zhangQuotientIntegerLattice.hpp"
#include "common/zhangIntegerProductGainFrontier.hpp"
#include "common/zhangProductRelationSolver.hpp"
#include "common/zhangProductRelationAdmission.hpp"
#include "common/zhangTheoryRegression.hpp"
#include "common/zhangIfUser.hpp"
#include "common/zhangIfWideLane.hpp"
#include "pea/zhangReference.hpp"
#include "pea/zhangPppAr.hpp"
#include "pea/zhangE29MathClosure.hpp"

static bool filterError = false;
static bool zhangTransactionalConditioningFailed = false;
static string zhangTransactionalConditioningReason;

struct ZhangHeldUserWideLane
{
	SatSys reference;
	map<SatSys, ZhangExactInteger> integers;
};
using ZhangHeldUserWideLaneKey = tuple<string, string, E_Sys, bool>;
static map<ZhangHeldUserWideLaneKey, ZhangHeldUserWideLane>
	zhangHeldUserWideLaneRegistry;

namespace
{
constexpr const char* ZHANG_AMBRES_BRANCH_PREFIX = "/branch/";

bool validZhangAmbresRuntimeId(const string& runtimeId)
{
    if (runtimeId.empty() || runtimeId.size() > 512)
    {
        return false;
    }
    return std::all_of(runtimeId.begin(), runtimeId.end(), [](unsigned char ch)
    {
        return ch >= 0x21 && ch <= 0x7e;
    });
}

string zhangAmbresRuntimeId(const KFState& state)
{
    auto found = state.metaDataMap.find(
        ZHANG_CHECKPOINT_RUNTIME_BRANCH_ID_METADATA);
    if (found != state.metaDataMap.end())
    {
        return found->second;
    }
    return zhangCheckpointRuntimeId(state);
}

bool zhangAmbresRuntimeBelongsTo(
    const string& candidate,
    const string& root)
{
    return candidate == root ||
        candidate.rfind(root + ZHANG_AMBRES_BRANCH_PREFIX, 0) == 0;
}

bool bindZhangAmbresEphemeralBranch(
    KFState&       branch,
    const KFState& source,
    const string&  role)
{
    const string sourceId = zhangAmbresRuntimeId(source);
    if (!validZhangAmbresRuntimeId(sourceId) || role.empty() ||
        role.find('/') != string::npos)
    {
        return false;
    }
    const string branchId =
        sourceId + ZHANG_AMBRES_BRANCH_PREFIX + role;
    if (!validZhangAmbresRuntimeId(branchId))
    {
        return false;
    }
    branch.metaDataMap[ZHANG_CHECKPOINT_RUNTIME_BRANCH_ID_METADATA] =
        branchId;
    return true;
}
}

static bool zhangE27IfCoordinateCodes(
	E_Sys       system,
	int         coordinateNumber,
	E_ObsCode&  first,
	E_ObsCode&  second)
{
	if (acsConfig.zhangPppAr.integer_strategy != "CANONICAL_USER_IF_WL_L1")
	{
		return false;
	}
	auto observables = acsConfig.zhangPppAr.baseline_observables.find(system);
	if (observables == acsConfig.zhangPppAr.baseline_observables.end() ||
		observables->second.size() != 2)
	{
		return false;
	}
	first = observables->second[0];
	second = observables->second[1];
	return coordinateNumber ==
		zhangPppArUserPhaseCoordinateNumber(system, first);
}

struct ZhangE27WideLaneKey
{
    string runtimeId;
    string receiver;
    E_Sys system = E_Sys::NONE;
    string solution;

    bool operator<(const ZhangE27WideLaneKey& other) const
    {
        return std::tie(runtimeId, receiver, system, solution) <
            std::tie(other.runtimeId, other.receiver, other.system, other.solution);
    }
};

struct ZhangE27WideLaneRawFactor
{
    GTime time;
    vector<int> satellites;
    VectorXd values;
    MatrixXd covariance;
    vector<string> noiseKeys;
    VectorXd noiseVariances;
    MatrixXd satelliteNoiseDesign;
};

struct ZhangE27WideLaneRuntime
{
    ZhangIfWideLaneAccumulator accumulator{3600, 360, 64};
    map<int, int> accumulatorArcVersions;
    std::deque<ZhangE27WideLaneRawFactor> rawFactors;
    map<SatSys, GTime> lastValid;
    map<SatSys, int> arcVersion;
    map<SatSys, tuple<int, int, int, int>> productDatum;
    SatSys reference;
};

static map<ZhangE27WideLaneKey, ZhangE27WideLaneRuntime>
    zhangE27WideLaneRuntimes;

static double zhangE27Wavelength(E_Sys system, E_ObsCode code)
{
	auto systemIt = code2Freq.find(system);
	if (systemIt == code2Freq.end())
	{
		return 0;
	}
	auto frequencyIt = systemIt->second.find(code);
	if (frequencyIt == systemIt->second.end())
	{
		return 0;
	}
	auto wavelengthIt = genericWavelength.find(frequencyIt->second);
	return wavelengthIt == genericWavelength.end() ? 0 : wavelengthIt->second;
}

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
		E_ObsCode first = E_ObsCode::NONE;
		E_ObsCode second = E_ObsCode::NONE;
		if (zhangE27IfCoordinateCodes(key.Sat.sys, key.num, first, second))
		{
			return zhangPppArUserAmbiguityIntegerValid(
				kfState, key.str, key.Sat, first) &&
				zhangPppArUserAmbiguityIntegerValid(
				kfState, key.str, key.Sat, second);
		}
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

struct ZhangRelinkMomentKey
{
    E_Sys  system = E_Sys::NONE;
    SatSys anchor;
    SatSys satellite;

    bool operator<(const ZhangRelinkMomentKey& other) const
    {
        return std::tie(system, anchor, satellite) <
               std::tie(other.system, other.anchor, other.satellite);
    }
};

struct ZhangRelinkPriorMoment
{
    GTime     time;
    E_ObsCode firstCode = E_ObsCode::NONE;
    E_ObsCode secondCode = E_ObsCode::NONE;
    long int  productDatumVersion = 0;
    double    firstMean = 0;
    double    firstVariance = 0;
    double    wideLaneMean = 0;
    double    wideLaneVariance = 0;
    double    firstWideLaneCovariance = 0;
};

static map<string, map<ZhangRelinkMomentKey, ZhangRelinkPriorMoment>>
    zhangRelinkPriorMoments;

/** Capture only scalar physical satellite-relation marginals, rather than a
 * second copy of the full network covariance.  The snapshot is taken after
 * state transition and immediately before the measurement update. */
void captureZhangPppArFloatPrior(const KFState& kfState)
{
    const string runtimeId = zhangAmbresRuntimeId(kfState);
    if (!validZhangAmbresRuntimeId(runtimeId))
    {
        return;
    }
    auto& runtimeMoments = zhangRelinkPriorMoments[runtimeId];
    runtimeMoments.clear();
    if (!acsConfig.zhangFullRank.enable ||
        (!acsConfig.zhangPppAr.multi_epoch_relink_shadow &&
         !acsConfig.zhangPppAr.whitened_wl_fixed_lag_shadow))
    {
        return;
    }

    map<pair<E_ObsCode, ZhangGraphEdge>, int> ambiguityIndices;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::AMBIGUITY ||
            !useAmbiguityForPhaseClockOsb(key) ||
            !useAmbiguityForZhang(kfState, key))
        {
            continue;
        }
        ambiguityIndices[
            {static_cast<E_ObsCode>(key.num), {key.str, key.Sat}}
        ] = index;
    }

    using SparseRow = vector<pair<int, double>>;
    auto project = [&](const SparseRow& row)
    {
        pair<double, double> moment{0, 0};
        for (const auto& [index, coefficient] : row)
        {
            moment.first += coefficient * kfState.x(index);
            for (const auto& [otherIndex, otherCoefficient] : row)
            {
                moment.second += coefficient * otherCoefficient *
                    kfState.P(index, otherIndex);
            }
        }
        return moment;
    };
    auto covariance = [&](const SparseRow& left, const SparseRow& right)
    {
        double value = 0;
        for (const auto& [leftIndex, leftCoefficient] : left)
        for (const auto& [rightIndex, rightCoefficient] : right)
        {
            value += leftCoefficient * rightCoefficient *
                kfState.P(leftIndex, rightIndex);
        }
        return value;
    };

    for (const auto& [system, systemOptions] : acsConfig.zhangFullRank.sysOpts)
    {
        if (systemOptions.baseline_observables.size() != 2)
        {
            continue;
        }
        const E_ObsCode firstCode = systemOptions.baseline_observables[0];
        const E_ObsCode secondCode = systemOptions.baseline_observables[1];
        ZhangGraphIntegerContext context;
        if (!zhangGraphIntegerContext(kfState, system, context))
        {
            continue;
        }
        ZhangSatelliteProductTarget target = ZhangProductTargetBuilder::build(
            context.basis, context.productBasis
        );
        if (!target.valid)
        {
            continue;
        }

        map<SatSys, ZhangExactVector> namedRows;
        namedRows[target.referenceSatellite] =
            ZhangExactVector(target.currentChords.size());
        for (int row = 0; row < static_cast<int>(target.targetSatellites.size()); row++)
        {
            namedRows[target.targetSatellites[row]] = target.matrix[row];
        }

        auto makeSparse = [&](const ZhangExactVector& exact,
                              E_ObsCode code,
                              SparseRow& row)
        {
            row.clear();
            for (int chord = 0;
                 chord < static_cast<int>(target.currentChords.size()); chord++)
            {
                if (exact[chord] == 0)
                {
                    continue;
                }
                auto index = ambiguityIndices.find(
                    {code, target.currentChords[chord]}
                );
                if (index == ambiguityIndices.end())
                {
                    return false;
                }
                row.push_back({index->second, exact[chord].convert_to<double>()});
            }
            return !row.empty();
        };

        for (const auto& [anchor, anchorRow] : namedRows)
        for (const auto& [satellite, satelliteRow] : namedRows)
        {
            if (anchor == satellite)
            {
                continue;
            }
            ZhangExactVector difference = satelliteRow;
            bool nonzero = false;
            for (int column = 0; column < static_cast<int>(difference.size()); column++)
            {
                difference[column] -= anchorRow[column];
                nonzero |= difference[column] != 0;
            }
            if (!nonzero)
            {
                continue;
            }
            SparseRow firstRow;
            SparseRow secondRow;
            if (!makeSparse(difference, firstCode, firstRow) ||
                !makeSparse(difference, secondCode, secondRow))
            {
                continue;
            }
            auto first = project(firstRow);
            auto second = project(secondRow);
            double firstSecondCovariance = covariance(firstRow, secondRow);
            ZhangRelinkPriorMoment moment;
            moment.time = kfState.time;
            moment.firstCode = firstCode;
            moment.secondCode = secondCode;
            moment.productDatumVersion = context.productDatumVersion;
            moment.firstMean = first.first;
            moment.firstVariance = std::max(0.0, first.second);
            moment.wideLaneMean = first.first - second.first;
            moment.wideLaneVariance = std::max(
                0.0, first.second + second.second - 2 * firstSecondCovariance
            );
            moment.firstWideLaneCovariance =
                first.second - firstSecondCovariance;
            runtimeMoments[{system, anchor, satellite}] = moment;
        }
    }
}

struct ZhangL1NoIonoProcessPrediction
{
    GTime           time;
    map<KFKey, int> indexMap;
    MatrixXd        covariance;
    int             transitionCount = 0;
    int             exactTransformCount = 0;
    bool            valid = false;
    string          failureReason;
};

static map<string, ZhangL1NoIonoProcessPrediction>
    zhangL1NoIonoProcessPredictions;

static MatrixXd zhangL1ReplayAugmentedSourceCovariance(
    const KFState&                         state,
    const map<KFKey, int>&                 source,
    const ZhangL1NoIonoProcessPrediction& counterfactual
)
{
    MatrixXd covariance = state.P;
    if (!counterfactual.valid)
    {
        return covariance;
    }
    for (const auto& [leftKey, leftCounterIndex] : counterfactual.indexMap)
    {
        auto left = source.find(leftKey);
        if (left == source.end())
        {
            continue;
        }
        for (const auto& [rightKey, rightCounterIndex] :
             counterfactual.indexMap)
        {
            auto right = source.find(rightKey);
            if (right == source.end())
            {
                continue;
            }
            covariance(left->second, right->second) =
                counterfactual.covariance(
                    leftCounterIndex, rightCounterIndex);
        }
    }
    return covariance;
}

void configureZhangL1MeasurementReplayTransitionCapture(KFState& kfState)
{
    if (!acsConfig.zhangPppAr.l1_measurement_replay_shadow)
    {
        return;
    }
    if (acsConfig.zhangPppAr.l1_measurement_replay_target_epoch !=
        tsync.to_string(0))
    {
        return;
    }
    const string runtimeId = zhangAmbresRuntimeId(kfState);
    if (!validZhangAmbresRuntimeId(runtimeId))
    {
        return;
    }
    auto previousTransition = kfState.stateTransitionFactorCallback;
    auto previousExactTransform = kfState.exactStateTransformCallback;
    const KFState* expectedOwner = &kfState;
    kfState.stateTransitionFactorCallback =
        [=](const KFState& state,
            GTime time,
            const map<KFKey, int>& source,
            const map<KFKey, int>& destination,
            const SparseMatrix<double>& transition,
            const MatrixXd& processCovariance,
            const string& label)
        {
            if (previousTransition)
            {
                previousTransition(
                    state, time, source, destination, transition,
                    processCovariance, label);
            }
            if (&state != expectedOwner ||
                acsConfig.zhangPppAr.l1_measurement_replay_target_epoch !=
                    time.to_string(0))
            {
                return;
            }
            auto& counterfactual =
                zhangL1NoIonoProcessPredictions[runtimeId];
            if (counterfactual.time != time)
            {
                counterfactual = {};
                counterfactual.time = time;
            }
            MatrixXd sourceCovariance =
                zhangL1ReplayAugmentedSourceCovariance(
                    state, source, counterfactual);
            MatrixXd noIonoProcess = processCovariance;
            for (const auto& [key, index] : destination)
            {
                if (key.type != KF::IONO_STEC)
                {
                    continue;
                }
                noIonoProcess.row(index).setZero();
                noIonoProcess.col(index).setZero();
            }
            counterfactual.covariance =
                transition * sourceCovariance * transition.transpose() +
                noIonoProcess;
            counterfactual.covariance = 0.5 *
                (counterfactual.covariance +
                 counterfactual.covariance.transpose());
            counterfactual.indexMap = destination;
            counterfactual.transitionCount++;
            counterfactual.valid =
                counterfactual.covariance.rows() ==
                    static_cast<int>(destination.size()) &&
                counterfactual.covariance.allFinite();
            if (!counterfactual.valid)
            {
                counterfactual.failureReason =
                    "NO_IONO_PROCESS_TRANSITION_NONFINITE";
            }
        };
    kfState.exactStateTransformCallback =
        [=](const KFState& state,
            GTime time,
            const map<KFKey, int>& source,
            const map<KFKey, int>& destination,
            const SparseMatrix<double>& transform,
            const string& label)
        {
            if (previousExactTransform)
            {
                previousExactTransform(
                    state, time, source, destination, transform, label);
            }
            if (&state != expectedOwner ||
                acsConfig.zhangPppAr.l1_measurement_replay_target_epoch !=
                    time.to_string(0))
            {
                return;
            }
            auto found = zhangL1NoIonoProcessPredictions.find(runtimeId);
            if (found == zhangL1NoIonoProcessPredictions.end() ||
                !found->second.valid || found->second.time != time)
            {
                return;
            }
            MatrixXd sourceCovariance =
                zhangL1ReplayAugmentedSourceCovariance(
                    state, source, found->second);
            found->second.covariance =
                transform * sourceCovariance * transform.transpose();
            found->second.covariance = 0.5 *
                (found->second.covariance +
                 found->second.covariance.transpose());
            found->second.indexMap = destination;
            found->second.exactTransformCount++;
            found->second.valid = found->second.covariance.allFinite();
            if (!found->second.valid)
            {
                found->second.failureReason =
                    "NO_IONO_PROCESS_EXACT_TRANSFORM_NONFINITE";
            }
        };
}

struct ZhangL1MeasurementReplayPosterior
{
    string  group;
    string  semantics;
    KFState posterior;
    int     inputRows = 0;
    int     retainedRows = 0;
    int     removedRows = 0;
    bool    valid = false;
    string  failureReason;
};

static map<string, vector<ZhangL1MeasurementReplayPosterior>>
    zhangL1MeasurementReplayPosteriors;

/** Replay a single already-screened measurement update from the identical
 * predicted state.  The authoritative filter has already completed its QC,
 * so the final R (including any deweighting) is reused and all callbacks are
 * disabled.  This is a measurement-update counterfactual, not a second
 * estimator history. */
void captureZhangL1MeasurementReplayPosteriors(
    Trace&          trace,
    const KFState&  predictedState,
    const KFState&  authoritativePosterior,
    KFMeas&         finalMeasurements
)
{
    if (!acsConfig.zhangPppAr.l1_measurement_replay_shadow ||
        acsConfig.zhangPppAr.l1_measurement_replay_target_epoch !=
            predictedState.time.to_string(0))
    {
        return;
    }
    const string runtimeId = zhangAmbresRuntimeId(authoritativePosterior);
    if (!validZhangAmbresRuntimeId(runtimeId))
    {
        trace << "\nZHANG_L1_MEASUREMENT_REPLAY_CAPTURE time="
              << predictedState.time.to_string(0)
              << " status=INVALID_RUNTIME_ID feedback=SHADOW_NONE";
        return;
    }
    auto& output = zhangL1MeasurementReplayPosteriors[runtimeId];
    output.clear();
    const bool measurementDimensionsValid =
        finalMeasurements.H.rows() ==
            static_cast<int>(finalMeasurements.obsKeys.size()) &&
        finalMeasurements.V.size() == finalMeasurements.H.rows() &&
        finalMeasurements.R.rows() == finalMeasurements.H.rows() &&
        finalMeasurements.R.cols() == finalMeasurements.H.rows() &&
        finalMeasurements.H.cols() == predictedState.x.size();
    if (!measurementDimensionsValid)
    {
        trace << "\nZHANG_L1_MEASUREMENT_REPLAY_CAPTURE time="
              << predictedState.time.to_string(0)
              << " status=MEASUREMENT_DIMENSION_MISMATCH"
              << " measurement_rows=" << finalMeasurements.H.rows()
              << " obs_keys=" << finalMeasurements.obsKeys.size()
              << " state_dimension=" << predictedState.x.size()
              << " feedback=SHADOW_NONE";
        return;
    }

    string targetReceiver =
        acsConfig.zhangPppAr.l1_measurement_replay_receiver;
    string targetSatellite =
        acsConfig.zhangPppAr.l1_measurement_replay_satellite;
    boost::to_upper(targetReceiver);
    boost::to_upper(targetSatellite);

    struct ReplayGroup
    {
        string name;
        string semantics;
        std::function<bool(const KFKey&)> keep;
        bool holdIonosphere = false;
        bool removeIonosphereProcessNoise = false;
    };
    auto physicalObservation = [](const KFKey& key)
    {
        return key.type == KF::CODE_MEAS || key.type == KF::PHAS_MEAS;
    };
    const vector<ReplayGroup> groups = {
        {
            "A0_ALL_FINAL_QC",
            "IDENTICAL_POST_QC_WEIGHTED_MEASUREMENT_UPDATE",
            [](const KFKey&) { return true; },
			false,
            false
        },
        {
            "A1_DROP_RECEIVER_" + targetReceiver,
            "DROP_TARGET_RECEIVER_PHYSICAL_OBSERVATIONS",
            [=](const KFKey& key)
            {
                string receiver = key.str;
                boost::to_upper(receiver);
                return !(physicalObservation(key) &&
                         receiver == targetReceiver);
            },
            false,
            false
        },
        {
            "A2_DROP_SATELLITE_" + targetSatellite,
            "DROP_TARGET_SATELLITE_PHYSICAL_OBSERVATIONS",
            [=](const KFKey& key)
            {
                string satellite = key.Sat.id();
                boost::to_upper(satellite);
                return !(physicalObservation(key) &&
                         satellite == targetSatellite);
            },
            false,
            false
        },
        {
            "A3_PHASE_ONLY",
            "DROP_ALL_CODE_MEASUREMENTS_KEEP_PHASE_AND_PSEUDO_OBSERVATIONS",
            [](const KFKey& key) { return key.type != KF::CODE_MEAS; },
            false,
            false
        },
        {
            "A6_NO_IONO_PROCESS_OR_UPDATE",
            "REMOVE_IONO_STEC_PROCESS_COVARIANCE_AND_HOLD_AT_PREDICTED",
            [](const KFKey&) { return true; },
            true,
            true
        }
    };

    for (const ReplayGroup& group : groups)
    {
        vector<Triplet<double>> selection;
        vector<KFKey> obsKeys;
        vector<map<string, void*>> metadata;
        selection.reserve(finalMeasurements.obsKeys.size());
        obsKeys.reserve(finalMeasurements.obsKeys.size());
        metadata.reserve(finalMeasurements.obsKeys.size());
        for (int oldRow = 0;
             oldRow < static_cast<int>(finalMeasurements.obsKeys.size());
             oldRow++)
        {
            if (!group.keep(finalMeasurements.obsKeys[oldRow]))
            {
                continue;
            }
            const int newRow = obsKeys.size();
            selection.emplace_back(newRow, oldRow, 1.0);
            obsKeys.push_back(finalMeasurements.obsKeys[oldRow]);
            metadata.push_back(
                oldRow < static_cast<int>(finalMeasurements.metaDataMaps.size())
                    ? finalMeasurements.metaDataMaps[oldRow]
                    : map<string, void*>{}
            );
        }

        ZhangL1MeasurementReplayPosterior result;
        result.group = group.name;
        result.semantics = group.semantics;
        result.inputRows = finalMeasurements.H.rows();
        result.retainedRows = obsKeys.size();
        result.removedRows = result.inputRows - result.retainedRows;
        if (obsKeys.empty())
        {
            result.failureReason = "NO_RETAINED_MEASUREMENTS";
            output.push_back(std::move(result));
            continue;
        }

        KFMeas replayMeasurements(
            finalMeasurements,
            std::move(selection),
            std::move(obsKeys),
            std::move(metadata)
        );
        result.posterior = predictedState;
        if (group.removeIonosphereProcessNoise)
        {
            auto noIono = zhangL1NoIonoProcessPredictions.find(runtimeId);
            bool covarianceAvailable =
                noIono != zhangL1NoIonoProcessPredictions.end() &&
                noIono->second.valid &&
                noIono->second.time == predictedState.time &&
                noIono->second.indexMap == predictedState.kfIndexMap &&
                noIono->second.covariance.rows() == predictedState.P.rows();
            if (!covarianceAvailable)
            {
                result.valid = false;
                result.failureReason = "NO_IONO_PROCESS_PRIOR_UNAVAILABLE";
                trace << "\nZHANG_L1_MEASUREMENT_REPLAY_CAPTURE time="
                      << predictedState.time.to_string(0)
                      << " group=" << group.name
                      << " semantics=" << group.semantics
                      << " input_rows=" << result.inputRows
                      << " retained_rows=" << result.retainedRows
                      << " removed_rows=" << result.removedRows
                      << " status=" << result.failureReason
                      << " feedback=SHADOW_NONE";
                output.push_back(std::move(result));
                continue;
            }
            result.posterior.P = noIono->second.covariance;
        }
        result.posterior.stateRejectCallbacks.clear();
        result.posterior.measRejectCallbacks.clear();
        result.posterior.acceptedMeasurementFactorCallback = {};
        result.posterior.stateTransitionFactorCallback = {};
        result.posterior.exactStateTransformCallback = {};
        result.posterior.rts_basename.clear();
        result.posterior.output_residuals = false;
        result.posterior.outputMongoMeasurements = false;
        result.posterior.prefitOpts.sigma_check = false;
        result.posterior.prefitOpts.omega_test = false;
        result.posterior.postfitOpts.sigma_check = false;
        result.posterior.postfitOpts.omega_test = false;
        result.posterior.postfitOpts.max_iterations = 1;
        result.posterior.chiSquareTest.enable = false;
        result.posterior.simulate_filter_only = false;
        if (group.holdIonosphere)
        {
            for (const auto& [key, index] : result.posterior.kfIndexMap)
            {
                if (key.type != KF::IONO_STEC)
                {
                    continue;
                }
                result.posterior.P.row(index).setZero();
                result.posterior.P.col(index).setZero();
            }
        }
        result.posterior.filterKalman(
            nullStream,
            replayMeasurements,
            "/ZHANG_R4_MEASUREMENT_REPLAY",
            true
        );
        result.valid = result.posterior.x.size() == predictedState.x.size() &&
            result.posterior.P.rows() == predictedState.P.rows() &&
            result.posterior.x.allFinite() &&
            result.posterior.P.allFinite();
        if (!result.valid)
        {
            result.failureReason = "NONFINITE_OR_DIMENSION_MISMATCH";
        }

        double stateClosure = std::numeric_limits<double>::quiet_NaN();
        double covarianceClosure = std::numeric_limits<double>::quiet_NaN();
        if (group.name == "A0_ALL_FINAL_QC" && result.valid &&
            result.posterior.x.size() == authoritativePosterior.x.size() &&
            result.posterior.P.rows() == authoritativePosterior.P.rows())
        {
            stateClosure = result.posterior.x.size() == 0 ? 0 :
                (result.posterior.x - authoritativePosterior.x)
                    .cwiseAbs().maxCoeff();
            covarianceClosure = result.posterior.P.size() == 0 ? 0 :
                (result.posterior.P - authoritativePosterior.P)
                    .cwiseAbs().maxCoeff();
        }
        trace << "\nZHANG_L1_MEASUREMENT_REPLAY_CAPTURE time="
              << predictedState.time.to_string(0)
              << " group=" << group.name
              << " semantics=" << group.semantics
              << " input_rows=" << result.inputRows
              << " retained_rows=" << result.retainedRows
              << " removed_rows=" << result.removedRows
              << " baseline_state_maximum_difference=" << stateClosure
              << " baseline_covariance_maximum_difference="
              << covarianceClosure
              << " status=" << (result.valid ? "CAPTURED" : result.failureReason)
              << " feedback=SHADOW_NONE";
        output.push_back(std::move(result));
    }
    auto noIono = zhangL1NoIonoProcessPredictions.find(runtimeId);
    trace << "\nZHANG_L1_NO_IONO_PROCESS_PRIOR time="
          << predictedState.time.to_string(0)
          << " transitions="
          << (noIono == zhangL1NoIonoProcessPredictions.end()
                ? 0 : noIono->second.transitionCount)
          << " exact_transforms="
          << (noIono == zhangL1NoIonoProcessPredictions.end()
                ? 0 : noIono->second.exactTransformCount)
          << " status="
          << (noIono != zhangL1NoIonoProcessPredictions.end() &&
              noIono->second.valid ? "CAPTURED" : "INVALID")
          << " feedback=SHADOW_NONE";
    zhangL1NoIonoProcessPredictions.erase(runtimeId);
}

static void conditionZhangL1MeasurementReplayPosteriors(
    Trace&                    trace,
    const KFState&            authoritativeState,
    const map<int, KFKey>&    ambiguityMap,
    const MatrixXd&           ambiguityRows,
    const VectorXd&           integers,
    const string&             stage
)
{
    if (ambiguityRows.rows() == 0)
    {
        return;
    }
    const string runtimeId = zhangAmbresRuntimeId(authoritativeState);
    auto found = zhangL1MeasurementReplayPosteriors.find(runtimeId);
    if (found == zhangL1MeasurementReplayPosteriors.end())
    {
        found = std::find_if(
            zhangL1MeasurementReplayPosteriors.begin(),
            zhangL1MeasurementReplayPosteriors.end(),
            [&](const auto& entry)
            {
                return zhangAmbresRuntimeBelongsTo(
                    runtimeId, entry.first);
            }
        );
    }
    if (found == zhangL1MeasurementReplayPosteriors.end())
    {
        return;
    }
    for (auto& replay : found->second)
    {
        if (!replay.valid || replay.group == "A0_ALL_FINAL_QC")
        {
            continue;
        }
        vector<Triplet<double>> triplets;
        bool mappingValid = integers.size() == ambiguityRows.rows();
        for (int row = 0; mappingValid && row < ambiguityRows.rows(); row++)
        for (int column = 0; column < ambiguityRows.cols(); column++)
        {
            const double coefficient = ambiguityRows(row, column);
            if (coefficient == 0)
            {
                continue;
            }
            auto key = ambiguityMap.find(column);
            auto state = key == ambiguityMap.end()
                ? replay.posterior.kfIndexMap.end()
                : replay.posterior.kfIndexMap.find(key->second);
            if (key == ambiguityMap.end() ||
                state == replay.posterior.kfIndexMap.end())
            {
                mappingValid = false;
                break;
            }
            triplets.emplace_back(row, state->second, coefficient);
        }
        if (!mappingValid)
        {
            replay.valid = false;
            replay.failureReason = "CONSTRAINT_STATE_MAPPING_FAILED_" + stage;
            continue;
        }
        ZhangIarFunctional constraints(
            ambiguityRows.rows(), replay.posterior.x.size());
        constraints.setFromTriplets(triplets.begin(), triplets.end());
        constraints.makeCompressed();
        const ZhangIntegerConditionedState conditioned =
            zhangConditionIntegersExact(
                replay.posterior.x,
                replay.posterior.P,
                constraints,
                integers);
        if (!conditioned.valid)
        {
            replay.valid = false;
            replay.failureReason =
                "CONDITIONING_FAILED_" + stage + "_" +
                conditioned.failureReason;
            trace << "\nZHANG_L1_MEASUREMENT_REPLAY_CONDITION time="
                  << authoritativeState.time.to_string(0)
                  << " group=" << replay.group
                  << " stage=" << stage
                  << " rows=" << ambiguityRows.rows()
                  << " status=" << replay.failureReason
                  << " feedback=SHADOW_NONE";
            continue;
        }
        replay.posterior.x = conditioned.mean;
        replay.posterior.P = conditioned.covariance;
        trace << "\nZHANG_L1_MEASUREMENT_REPLAY_CONDITION time="
              << authoritativeState.time.to_string(0)
              << " group=" << replay.group
              << " stage=" << stage
              << " rows=" << ambiguityRows.rows()
              << " rank=" << conditioned.constraintRank
              << " maximum_residual="
              << conditioned.maximumConstraintResidual
              << " status=APPLIED feedback=SHADOW_NONE";
    }
}

static void traceZhangL1MeasurementReplayNis(
    Trace&                    trace,
    const KFState&            authoritativeState,
    const GinAR_mtx&          baselineFirstSignal,
    const MatrixXd&           testedRows,
    const VectorXd&           testedIntegers,
    double                    baselineSearchNis,
    double                    baselineSearchThreshold
)
{
    const string runtimeId = zhangAmbresRuntimeId(authoritativeState);
    auto found = zhangL1MeasurementReplayPosteriors.find(runtimeId);
    if (found == zhangL1MeasurementReplayPosteriors.end())
    {
        found = std::find_if(
            zhangL1MeasurementReplayPosteriors.begin(),
            zhangL1MeasurementReplayPosteriors.end(),
            [&](const auto& entry)
            {
                return zhangAmbresRuntimeBelongsTo(
                    runtimeId, entry.first);
            }
        );
    }
    if (found == zhangL1MeasurementReplayPosteriors.end())
    {
        return;
    }
    auto eraseOnExit = [&]()
    {
        zhangL1MeasurementReplayPosteriors.erase(found);
    };
    if (testedRows.rows() == 0 ||
        testedRows.cols() != baselineFirstSignal.aflt.size() ||
        testedIntegers.size() != testedRows.rows())
    {
        trace << "\nZHANG_L1_MEASUREMENT_REPLAY_SUMMARY time="
              << authoritativeState.time.to_string(0)
              << " status=NO_COMMON_TESTED_INTEGER_FUNCTIONAL"
              << " feedback=SHADOW_NONE";
        eraseOnExit();
        return;
    }

    const VectorXd baselineMean = testedRows * baselineFirstSignal.aflt;
    const MatrixXd baselineCovariance =
        testedRows * baselineFirstSignal.Paflt * testedRows.transpose();
    const VectorXd baselineInnovation = testedIntegers - baselineMean;
    const ZhangIntegerCandidateNis baselineNis =
        assessZhangIntegerCandidateNis(
            baselineInnovation,
            baselineCovariance,
            acsConfig.zhangPppAr.held_constraint_nis_alpha);

    for (auto& replay : found->second)
    {
        if (!replay.valid)
        {
            trace << "\nZHANG_L1_MEASUREMENT_REPLAY_RESULT time="
                  << authoritativeState.time.to_string(0)
                  << " group=" << replay.group
                  << " status=" << replay.failureReason
                  << " feedback=SHADOW_NONE";
            continue;
        }
        if (replay.group == "A0_ALL_FINAL_QC")
        {
            trace << "\nZHANG_L1_MEASUREMENT_REPLAY_RESULT time="
                  << authoritativeState.time.to_string(0)
                  << " group=" << replay.group
                  << " semantics=" << replay.semantics
                  << " tested_rank=" << testedRows.rows()
                  << " input_rows=" << replay.inputRows
                  << " retained_rows=" << replay.retainedRows
                  << " removed_rows=" << replay.removedRows
                  << " nis=" << baselineNis.nis
                  << " nis_threshold=" << baselineNis.threshold
                  << " delta_nis=0"
                  << " delta_mean_l2_cycles=0"
                  << " delta_mean_max_cycles=0"
                  << " delta_covariance_frobenius_cycles2=0"
                  << " delta_covariance_max_cycles2=0"
                  << " baseline_lambda_search_nis=" << baselineSearchNis
                  << " baseline_lambda_search_threshold="
                  << baselineSearchThreshold
                  << " status=" << (baselineNis.valid
                        ? "EVALUATED" : "INVALID_NIS")
                  << " feedback=SHADOW_NONE";
            for (int row = 0; row < testedRows.rows(); row++)
            {
                trace << "\nZHANG_L1_MEASUREMENT_REPLAY_FUNCTIONAL time="
                      << authoritativeState.time.to_string(0)
                      << " group=" << replay.group
                      << " row=" << row
                      << " integer=" << testedIntegers(row)
                      << " baseline_mean_cycles=" << baselineMean(row)
                      << " replay_mean_cycles=" << baselineMean(row)
                      << " delta_mean_cycles=0"
                      << " baseline_variance_cycles2="
                      << baselineCovariance(row, row)
                      << " replay_variance_cycles2="
                      << baselineCovariance(row, row)
                      << " delta_variance_cycles2=0"
                      << " feedback=SHADOW_NONE";
            }
            continue;
        }
        vector<int> indices;
        bool mappingValid = true;
        for (int column = 0;
             column < static_cast<int>(baselineFirstSignal.ambmap.size());
             column++)
        {
            auto key = baselineFirstSignal.ambmap.find(column);
            auto state = key == baselineFirstSignal.ambmap.end()
                ? replay.posterior.kfIndexMap.end()
                : replay.posterior.kfIndexMap.find(key->second);
            if (key == baselineFirstSignal.ambmap.end() ||
                state == replay.posterior.kfIndexMap.end())
            {
                mappingValid = false;
                break;
            }
            indices.push_back(state->second);
        }
        if (!mappingValid)
        {
            trace << "\nZHANG_L1_MEASUREMENT_REPLAY_RESULT time="
                  << authoritativeState.time.to_string(0)
                  << " group=" << replay.group
                  << " status=FUNCTIONAL_STATE_MAPPING_FAILED"
                  << " feedback=SHADOW_NONE";
            continue;
        }
        const VectorXd replayAmbiguities = replay.posterior.x(indices);
        const MatrixXd replayAmbiguityCovariance =
            replay.posterior.P(indices, indices);
        const VectorXd replayMean = testedRows * replayAmbiguities;
        const MatrixXd replayCovariance =
            testedRows * replayAmbiguityCovariance * testedRows.transpose();
        const VectorXd replayInnovation = testedIntegers - replayMean;
        const ZhangIntegerCandidateNis replayNis =
            assessZhangIntegerCandidateNis(
                replayInnovation,
                replayCovariance,
                acsConfig.zhangPppAr.held_constraint_nis_alpha);
        const VectorXd meanDelta = replayMean - baselineMean;
        const MatrixXd covarianceDelta =
            replayCovariance - baselineCovariance;
        trace << "\nZHANG_L1_MEASUREMENT_REPLAY_RESULT time="
              << authoritativeState.time.to_string(0)
              << " group=" << replay.group
              << " semantics=" << replay.semantics
              << " tested_rank=" << testedRows.rows()
              << " input_rows=" << replay.inputRows
              << " retained_rows=" << replay.retainedRows
              << " removed_rows=" << replay.removedRows
              << " nis=" << replayNis.nis
              << " nis_threshold=" << replayNis.threshold
              << " delta_nis=" << (replayNis.nis - baselineNis.nis)
              << " delta_mean_l2_cycles=" << meanDelta.norm()
              << " delta_mean_max_cycles="
              << meanDelta.cwiseAbs().maxCoeff()
              << " delta_covariance_frobenius_cycles2="
              << covarianceDelta.norm()
              << " delta_covariance_max_cycles2="
              << covarianceDelta.cwiseAbs().maxCoeff()
              << " baseline_lambda_search_nis=" << baselineSearchNis
              << " baseline_lambda_search_threshold="
              << baselineSearchThreshold
              << " status=" << (replayNis.valid ? "EVALUATED" : "INVALID_NIS")
              << " feedback=SHADOW_NONE";
        for (int row = 0; row < testedRows.rows(); row++)
        {
            trace << "\nZHANG_L1_MEASUREMENT_REPLAY_FUNCTIONAL time="
                  << authoritativeState.time.to_string(0)
                  << " group=" << replay.group
                  << " row=" << row
                  << " integer=" << testedIntegers(row)
                  << " baseline_mean_cycles=" << baselineMean(row)
                  << " replay_mean_cycles=" << replayMean(row)
                  << " delta_mean_cycles=" << meanDelta(row)
                  << " baseline_variance_cycles2="
                  << baselineCovariance(row, row)
                  << " replay_variance_cycles2="
                  << replayCovariance(row, row)
                  << " delta_variance_cycles2="
                  << covarianceDelta(row, row)
                  << " feedback=SHADOW_NONE";
        }
    }
    trace << "\nZHANG_L1_MEASUREMENT_REPLAY_SUMMARY time="
          << authoritativeState.time.to_string(0)
          << " tested_rank=" << testedRows.rows()
          << " baseline_nis=" << baselineNis.nis
          << " baseline_threshold=" << baselineNis.threshold
          << " baseline_lambda_search_nis=" << baselineSearchNis
          << " baseline_lambda_search_threshold="
          << baselineSearchThreshold
          << " baseline_nis_closure="
          << std::abs(baselineNis.nis - baselineSearchNis)
          << " replay_groups=" << found->second.size()
          << " same_functional_for_all_groups=1"
          << " measurement_update_only=1"
          << " ionosphere_process_noise_ablation=A6_INCLUDED"
          << " status=EVALUATED feedback=SHADOW_NONE";
    eraseOnExit();
}

struct ZhangRelinkShadowIncrement
{
    GTime  time;
    double information = 0;
    double natural = 0;
};

struct ZhangRelinkShadowAccumulator
{
    std::deque<ZhangRelinkShadowIncrement> increments;
    long long integerHypothesis = 0;
    bool initialized = false;
};

static map<string, map<string, ZhangRelinkShadowAccumulator>>
    zhangRelinkShadowAccumulators;

struct ZhangRelinkJointIncrement
{
    GTime    time;
    Matrix2d information = Matrix2d::Zero();
    Vector2d natural = Vector2d::Zero();
};

struct ZhangRelinkJointAccumulator
{
    std::deque<ZhangRelinkJointIncrement> increments;
    long long wideLaneHypothesis = 0;
    long long firstHypothesis = 0;
    bool initialized = false;
};

static map<string, map<string, ZhangRelinkJointAccumulator>>
    zhangRelinkJointAccumulators;

static void traceZhangRelinkJointInformationIncrement(
    Trace&             trace,
    const KFState&     floatState,
    const GinAR_mtx&   ambiguityResolution,
    E_Sys              system,
    const SatSys&      anchor,
    const SatSys&      satellite,
    const string&      topologyKey,
    const VectorXd&    firstRow,
    const VectorXd&    secondRow,
    GTime              time,
    long int           posteriorDatumVersion
)
{
    if (!acsConfig.zhangPppAr.multi_epoch_relink_shadow)
    {
        return;
    }
    const string runtimeId = zhangAmbresRuntimeId(floatState);
    auto runtimeMoments = zhangRelinkPriorMoments.find(runtimeId);
    auto prior = runtimeMoments == zhangRelinkPriorMoments.end()
        ? map<ZhangRelinkMomentKey, ZhangRelinkPriorMoment>::iterator{}
        : runtimeMoments->second.find({system, anchor, satellite});
    auto reject = [&](const string& reason)
    {
        trace << "\nZHANG_RELINK_JOINT_INFORMATION time="
              << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " topology_key=" << topologyKey
              << " anchor=" << anchor.id()
              << " satellite=" << satellite.id()
              << " status=REJECTED reason=" << reason
              << " feedback=0";
    };
    if (!validZhangAmbresRuntimeId(runtimeId) ||
        runtimeMoments == zhangRelinkPriorMoments.end() ||
        prior == runtimeMoments->second.end() ||
        std::abs((time - prior->second.time).to_double()) > 1e-3)
    {
        reject("PRIOR_RELATION_MISSING_OR_MISMATCHED");
        return;
    }

    const VectorXd wideLaneRow = firstRow - secondRow;
    auto project = [&](const VectorXd& row,
                       double& mean,
                       vector<pair<int, double>>& sparse)
    {
        mean = 0;
        sparse.clear();
        for (int column = 0; column < row.size(); column++)
        {
            if (row(column) == 0)
            {
                continue;
            }
            auto key = ambiguityResolution.ambmap.find(column);
            auto state = key == ambiguityResolution.ambmap.end()
                ? floatState.kfIndexMap.end()
                : floatState.kfIndexMap.find(key->second);
            if (key == ambiguityResolution.ambmap.end() ||
                state == floatState.kfIndexMap.end())
            {
                return false;
            }
            sparse.push_back({state->second, row(column)});
            mean += row(column) * floatState.x(state->second);
        }
        return !sparse.empty();
    };
    auto covariance = [&](const vector<pair<int, double>>& left,
                          const vector<pair<int, double>>& right)
    {
        double value = 0;
        for (const auto& [leftIndex, leftCoefficient] : left)
        for (const auto& [rightIndex, rightCoefficient] : right)
        {
            value += leftCoefficient * rightCoefficient *
                floatState.P(leftIndex, rightIndex);
        }
        return value;
    };

    double posteriorWideLaneMean = 0;
    double posteriorFirstMean = 0;
    vector<pair<int, double>> posteriorWideLaneRow;
    vector<pair<int, double>> posteriorFirstRow;
    if (!project(wideLaneRow, posteriorWideLaneMean, posteriorWideLaneRow) ||
        !project(firstRow, posteriorFirstMean, posteriorFirstRow))
    {
        reject("POSTERIOR_AMBIGUITY_MISSING");
        return;
    }

    Vector2d priorMean;
    priorMean << prior->second.wideLaneMean, prior->second.firstMean;
    Matrix2d priorCovariance;
    priorCovariance <<
        prior->second.wideLaneVariance,
        prior->second.firstWideLaneCovariance,
        prior->second.firstWideLaneCovariance,
        prior->second.firstVariance;
    Vector2d posteriorMean;
    posteriorMean << posteriorWideLaneMean, posteriorFirstMean;
    Matrix2d posteriorCovariance;
    posteriorCovariance <<
        covariance(posteriorWideLaneRow, posteriorWideLaneRow),
        covariance(posteriorWideLaneRow, posteriorFirstRow),
        covariance(posteriorFirstRow, posteriorWideLaneRow),
        covariance(posteriorFirstRow, posteriorFirstRow);
    priorCovariance = (priorCovariance + priorCovariance.transpose()) / 2;
    posteriorCovariance =
        (posteriorCovariance + posteriorCovariance.transpose()) / 2;
    Eigen::SelfAdjointEigenSolver<Matrix2d> priorEigen(priorCovariance);
    Eigen::SelfAdjointEigenSolver<Matrix2d> posteriorEigen(posteriorCovariance);
    if (priorEigen.info() != Eigen::Success ||
        posteriorEigen.info() != Eigen::Success ||
        priorEigen.eigenvalues().minCoeff() <= 0 ||
        posteriorEigen.eigenvalues().minCoeff() <= 0)
    {
        reject("NON_POSITIVE_MARGINAL_COVARIANCE");
        return;
    }
    Matrix2d information = posteriorCovariance.inverse() -
        priorCovariance.inverse();
    information = (information + information.transpose()) / 2;
    Vector2d natural = posteriorCovariance.inverse() * posteriorMean -
        priorCovariance.inverse() * priorMean;
    Eigen::SelfAdjointEigenSolver<Matrix2d> informationEigen(information);
    if (informationEigen.info() != Eigen::Success ||
        informationEigen.eigenvalues().minCoeff() <
            -acsConfig.zhangPppAr.multi_epoch_relink_shadow_information_floor ||
        informationEigen.eigenvalues().maxCoeff() <
            acsConfig.zhangPppAr.multi_epoch_relink_shadow_information_floor ||
        !information.allFinite() || !natural.allFinite())
    {
        reject("NON_POSITIVE_JOINT_INFORMATION_INCREMENT");
        return;
    }

    const long long wideLaneHypothesis = std::llround(posteriorMean(0));
    const long long firstHypothesis = std::llround(posteriorMean(1));
    const string accumulatorKey = enum_to_string(system) + ":JOINT:" +
        anchor.id() + ":" + satellite.id();
    auto& accumulator =
        zhangRelinkJointAccumulators[runtimeId][accumulatorKey];
    bool reset = false;
    string resetReason = "NONE";
    if (accumulator.initialized)
    {
        if (accumulator.wideLaneHypothesis != wideLaneHypothesis ||
            accumulator.firstHypothesis != firstHypothesis)
        {
            reset = true;
            resetReason = "INTEGER_HYPOTHESIS_CHANGED";
        }
        else if (!accumulator.increments.empty())
        {
            double gap = (time - accumulator.increments.back().time).to_double();
            if (gap < -1e-3 ||
                (acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_gap_seconds > 0 &&
                 gap > acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_gap_seconds))
            {
                reset = true;
                resetReason = "EPOCH_GAP";
            }
            else if (std::abs(gap) <= 1e-3)
            {
                reject("DUPLICATE_EPOCH_INCREMENT");
                return;
            }
        }
    }
    if (reset)
    {
        accumulator.increments.clear();
    }
    accumulator.initialized = true;
    accumulator.wideLaneHypothesis = wideLaneHypothesis;
    accumulator.firstHypothesis = firstHypothesis;
    accumulator.increments.push_back({time, information, natural});
    while (static_cast<int>(accumulator.increments.size()) >
           acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_epochs)
    {
        accumulator.increments.pop_front();
    }

    Matrix2d accumulatedInformation = Matrix2d::Zero();
    Vector2d accumulatedNatural = Vector2d::Zero();
    for (const auto& increment : accumulator.increments)
    {
        accumulatedInformation += increment.information;
        accumulatedNatural += increment.natural;
    }
    Eigen::SelfAdjointEigenSolver<Matrix2d> accumulatedEigen(
        accumulatedInformation
    );
    if (accumulatedEigen.info() != Eigen::Success ||
        accumulatedEigen.eigenvalues().minCoeff() <= 0)
    {
        reject("ACCUMULATED_INFORMATION_SINGULAR");
        return;
    }
    Matrix2d accumulatedCovariance = accumulatedInformation.inverse();
    Vector2d accumulatedMean = accumulatedCovariance * accumulatedNatural;
    const double wideLaneFractional =
        accumulatedMean(0) - std::round(accumulatedMean(0));
    const double wideLanePerr = round_perr(
        wideLaneFractional, accumulatedCovariance(0, 0)
    );
    const long long fixedWideLane = std::llround(accumulatedMean(0));
    const double conditionalFirstMean = accumulatedMean(1) +
        accumulatedCovariance(1, 0) / accumulatedCovariance(0, 0) *
        (fixedWideLane - accumulatedMean(0));
    const double conditionalFirstVariance = std::max(
        0.0,
        accumulatedCovariance(1, 1) -
            accumulatedCovariance(1, 0) * accumulatedCovariance(0, 1) /
                accumulatedCovariance(0, 0)
    );
    const double conditionalFirstFractional =
        conditionalFirstMean - std::round(conditionalFirstMean);
    const double conditionalFirstPerr = round_perr(
        conditionalFirstFractional, conditionalFirstVariance
    );

    trace << "\nZHANG_RELINK_JOINT_INFORMATION time="
          << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " topology_key=" << topologyKey
          << " anchor=" << anchor.id()
          << " satellite=" << satellite.id()
          << " prior_datum_version=" << prior->second.productDatumVersion
          << " posterior_datum_version=" << posteriorDatumVersion
          << " j00=" << information(0, 0)
          << " j01=" << information(0, 1)
          << " j11=" << information(1, 1)
          << " h0=" << natural(0)
          << " h1=" << natural(1)
          << " minimum_eigenvalue="
          << informationEigen.eigenvalues().minCoeff()
          << " status=ACCEPTED feedback=0";
    trace << "\nZHANG_RELINK_JOINT_SHADOW time="
          << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " topology_key=" << topologyKey
          << " anchor=" << anchor.id()
          << " satellite=" << satellite.id()
          << " epochs=" << accumulator.increments.size()
          << " wl_mean=" << accumulatedMean(0)
          << " wl_variance=" << accumulatedCovariance(0, 0)
          << " wl_perr=" << wideLanePerr
          << " fixed_wl=" << fixedWideLane
          << " conditional_l1_mean=" << conditionalFirstMean
          << " conditional_l1_variance=" << conditionalFirstVariance
          << " conditional_l1_perr=" << conditionalFirstPerr
          << " reset=" << reset
          << " reset_reason=" << resetReason
          << " feedback=0";
}

static void traceZhangRelinkInformationIncrement(
    Trace&             trace,
    const KFState&     floatState,
    const GinAR_mtx&   ambiguityResolution,
    E_Sys              system,
    const SatSys&      anchor,
    const SatSys&      satellite,
    const string&      topologyKey,
    const string&      stage,
    const VectorXd&    row,
    GTime              time,
    long int           posteriorDatumVersion
)
{
    if (!acsConfig.zhangPppAr.multi_epoch_relink_shadow)
    {
        return;
    }
    const string runtimeId = zhangAmbresRuntimeId(floatState);
    auto runtimeMoments = zhangRelinkPriorMoments.find(runtimeId);
    auto prior = runtimeMoments == zhangRelinkPriorMoments.end()
        ? map<ZhangRelinkMomentKey, ZhangRelinkPriorMoment>::iterator{}
        : runtimeMoments->second.find({system, anchor, satellite});
    auto reject = [&](const string& reason)
    {
        trace << "\nZHANG_RELINK_INFORMATION_INCREMENT time="
              << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " stage=" << stage
              << " topology_key=" << topologyKey
              << " anchor=" << anchor.id()
              << " satellite=" << satellite.id()
              << " status=REJECTED reason=" << reason
              << " feedback=0";
    };
    if (!validZhangAmbresRuntimeId(runtimeId) ||
        runtimeMoments == zhangRelinkPriorMoments.end() ||
        prior == runtimeMoments->second.end())
    {
        reject("PRIOR_RELATION_MISSING");
        return;
    }
    if (std::abs((time - prior->second.time).to_double()) > 1e-3)
    {
        reject("PRIOR_EPOCH_MISMATCH");
        return;
    }

    vector<pair<int, double>> sparse;
    for (int column = 0; column < row.size(); column++)
    {
        if (row(column) == 0)
        {
            continue;
        }
        auto key = ambiguityResolution.ambmap.find(column);
        auto state = key == ambiguityResolution.ambmap.end()
            ? floatState.kfIndexMap.end()
            : floatState.kfIndexMap.find(key->second);
        if (key == ambiguityResolution.ambmap.end() ||
            state == floatState.kfIndexMap.end())
        {
            reject("POSTERIOR_AMBIGUITY_MISSING");
            return;
        }
        sparse.push_back({state->second, row(column)});
    }
    if (sparse.empty())
    {
        reject("EMPTY_TARGET_ROW");
        return;
    }

    double posteriorMean = 0;
    double posteriorVariance = 0;
    for (const auto& [index, coefficient] : sparse)
    {
        posteriorMean += coefficient * floatState.x(index);
        for (const auto& [otherIndex, otherCoefficient] : sparse)
        {
            posteriorVariance += coefficient * otherCoefficient *
                floatState.P(index, otherIndex);
        }
    }
    const bool wideLane = stage == "WL";
    const double priorMean = wideLane
        ? prior->second.wideLaneMean
        : prior->second.firstMean;
    const double priorVariance = wideLane
        ? prior->second.wideLaneVariance
        : prior->second.firstVariance;
    if (!std::isfinite(priorMean) || !std::isfinite(posteriorMean) ||
        !std::isfinite(priorVariance) || !std::isfinite(posteriorVariance) ||
        priorVariance <= 0 || posteriorVariance <= 0)
    {
        reject("NON_POSITIVE_OR_NONFINITE_MARGINAL");
        return;
    }

    // For a scalar Gaussian marginal, posterior natural parameters minus
    // prior natural parameters describe this epoch's effective measurement
    // likelihood after nuisance-state elimination.  Cross-epoch nuisance
    // correlations are not reconstructed here, so this remains shadow-only.
    const double information =
        1 / posteriorVariance - 1 / priorVariance;
    const double natural =
        posteriorMean / posteriorVariance - priorMean / priorVariance;
    if (!std::isfinite(information) || !std::isfinite(natural) ||
        information <
            acsConfig.zhangPppAr.multi_epoch_relink_shadow_information_floor)
    {
        reject("NON_POSITIVE_INFORMATION_INCREMENT");
        return;
    }

    const double observation = natural / information;
    const double observationVariance = 1 / information;
    const long long hypothesis = std::llround(posteriorMean);
    const string accumulatorKey = enum_to_string(system) + ":" + stage + ":" +
        anchor.id() + ":" + satellite.id();
    auto& accumulator =
        zhangRelinkShadowAccumulators[runtimeId][accumulatorKey];
    bool reset = false;
    string resetReason = "NONE";
    if (accumulator.initialized)
    {
        if (accumulator.integerHypothesis != hypothesis)
        {
            reset = true;
            resetReason = "INTEGER_HYPOTHESIS_CHANGED";
        }
        else if (!accumulator.increments.empty())
        {
            double gap = (time - accumulator.increments.back().time).to_double();
            if (gap < -1e-3 ||
                (acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_gap_seconds > 0 &&
                 gap > acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_gap_seconds))
            {
                reset = true;
                resetReason = "EPOCH_GAP";
            }
            else if (std::abs(gap) <= 1e-3)
            {
                reject("DUPLICATE_EPOCH_INCREMENT");
                return;
            }
        }
    }
    if (reset)
    {
        accumulator.increments.clear();
    }
    accumulator.initialized = true;
    accumulator.integerHypothesis = hypothesis;
    accumulator.increments.push_back({time, information, natural});
    while (static_cast<int>(accumulator.increments.size()) >
           acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_epochs)
    {
        accumulator.increments.pop_front();
    }

    double accumulatedInformation = 0;
    double accumulatedNatural = 0;
    for (const auto& increment : accumulator.increments)
    {
        accumulatedInformation += increment.information;
        accumulatedNatural += increment.natural;
    }
    const double accumulatedMean =
        accumulatedNatural / accumulatedInformation;
    const double accumulatedVariance = 1 / accumulatedInformation;
    const double fractional = accumulatedMean - std::round(accumulatedMean);
    const double perr = round_perr(fractional, accumulatedVariance);

    trace << "\nZHANG_RELINK_INFORMATION_INCREMENT time="
          << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " stage=" << stage
          << " topology_key=" << topologyKey
          << " anchor=" << anchor.id()
          << " satellite=" << satellite.id()
          << " prior_datum_version=" << prior->second.productDatumVersion
          << " posterior_datum_version=" << posteriorDatumVersion
          << " prior_mean=" << priorMean
          << " prior_variance=" << priorVariance
          << " posterior_mean=" << posteriorMean
          << " posterior_variance=" << posteriorVariance
          << " information=" << information
          << " natural=" << natural
          << " effective_observation=" << observation
          << " effective_variance=" << observationVariance
          << " status=ACCEPTED feedback=0";
    trace << "\nZHANG_RELINK_SHADOW time="
          << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " stage=" << stage
          << " topology_key=" << topologyKey
          << " anchor=" << anchor.id()
          << " satellite=" << satellite.id()
          << " epochs=" << accumulator.increments.size()
          << " integer_hypothesis=" << hypothesis
          << " accumulated_mean=" << accumulatedMean
          << " accumulated_variance=" << accumulatedVariance
          << " fractional=" << fractional
          << " perr=" << perr
          << " reset=" << reset
          << " reset_reason=" << resetReason
          << " feedback=0";
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

    bool operator<(const ZhangPersistentHeldRow& other) const
    {
        return std::tie(coefficients, value) <
               std::tie(other.coefficients, other.value);
    }
};

struct ZhangPersistentHeldEvidence
{
    int      confirmations = 0;
    long int lastEpoch = 0;
};

struct ZhangPersistentHeldLattice
{
    vector<ZhangPersistentHeldRow> rows;
    int                            lastEventId = 0;
    bool                           consistent = true;
};

struct ZhangProjectedHeldSet
{
    E_Sys system = E_Sys::NONE;
    GinAR_mtx constraints;
    vector<map<E_ObsCode, set<SatSys>>> rowProductSupport;
};

static map<pair<string, E_Sys>, ZhangPersistentHeldLattice>
    zhangPersistentHeldLattices;


static map<string, vector<ZhangPendingProductTransition>>
    zhangActiveTemporalBesdTransitions;

static vector<ZhangPendingProductTransition>
activateZhangTemporalProductTransitions(
	Trace& trace,
	const KFState& state,
	bool onlyNewlyDiscovered = false)
{
	auto discovered = takeZhangPendingProductTransitions(state);
	if (!acsConfig.zhangPppAr.fixed_lag_factor_capture_shadow)
	{
		return discovered;
	}

	const double lagSeconds = std::max(
		60.0, acsConfig.zhangPppAr.whitened_wl_fixed_lag_seconds);
	const string runtimeId = zhangAmbresRuntimeId(state);
	if (!validZhangAmbresRuntimeId(runtimeId))
	{
		trace << "\nZHANG_TEMPORAL_PRODUCT_TRANSITION_SUMMARY time="
			  << state.time.to_string(0)
			  << " status=REJECTED reason=CHECKPOINT_RUNTIME_ID_UNBOUND"
			  << " feedback=0";
		return {};
	}
	for (auto& pending : discovered)
	{
		pending.expiryTime = pending.eventTime + lagSeconds;
	}
	auto newlyDiscovered = discovered;
	auto& active = zhangActiveTemporalBesdTransitions[runtimeId];
	for (auto& pending : discovered)
	{
		const bool duplicate = std::any_of(
			active.begin(), active.end(), [&](const auto& existing)
			{
				return existing.eventTime == pending.eventTime &&
					existing.system == pending.system &&
					existing.satellite == pending.satellite &&
					existing.observable == pending.observable &&
					existing.oldSnapshotIdentity ==
						pending.oldSnapshotIdentity &&
					existing.newSnapshotIdentity ==
						pending.newSnapshotIdentity;
			});
		if (!duplicate)
		{
			active.push_back(std::move(pending));
		}
	}
	active.erase(std::remove_if(active.begin(), active.end(),
		[&](const auto& pending)
		{
			return (state.time - pending.eventTime).to_double() > lagSeconds;
		}), active.end());
	map<string, int> snapshotReferenceCounts;
	for (const auto& pending : active)
	{
		if (!pending.oldSnapshotIdentity.empty())
		{
			snapshotReferenceCounts[pending.oldSnapshotIdentity]++;
		}
		if (!pending.newSnapshotIdentity.empty())
		{
			snapshotReferenceCounts[pending.newSnapshotIdentity]++;
		}
	}
	for (auto& pending : active)
	{
		pending.oldSnapshotReferenceCount =
			snapshotReferenceCounts[pending.oldSnapshotIdentity];
		pending.newSnapshotReferenceCount =
			snapshotReferenceCounts[pending.newSnapshotIdentity];
	}
	for (auto& pending : newlyDiscovered)
	{
		pending.oldSnapshotReferenceCount =
			snapshotReferenceCounts[pending.oldSnapshotIdentity];
		pending.newSnapshotReferenceCount =
			snapshotReferenceCounts[pending.newSnapshotIdentity];
		trace << "\nZHANG_TEMPORAL_PRODUCT_EVENT_IMMUTABLE time="
			  << state.time.to_string(0)
			  << " event_id=" << pending.eventId
			  << " event_time=" << pending.eventTime.to_string(0)
			  << " system=" << enum_to_string(pending.system)
			  << " satellite=" << pending.satellite.id()
			  << " observable=" << enum_to_string(pending.observable)
			  << " old_physical_identity=" << pending.oldIdentity
			  << " new_physical_identity=" << pending.newIdentity
			  << " old_s_basis_fingerprint="
			  << pending.oldSBasisFingerprint
			  << " new_s_basis_fingerprint="
			  << pending.newSBasisFingerprint
			  << " old_phase_segment_identity="
			  << pending.oldPhaseSegmentIdentity
			  << " new_phase_segment_identity="
			  << pending.newPhaseSegmentIdentity
			  << " phase_segment_changed=" << pending.phaseSegmentChanged
			  << " event_cause=" << pending.eventCause
			  << " old_product_segment=" << pending.oldProductSegment
			  << " new_product_segment=" << pending.newProductSegment
			  << " old_snapshot=" << pending.oldSnapshotIdentity
			  << " new_snapshot=" << pending.newSnapshotIdentity
			  << " exact_transform_chain_id="
			  << pending.exactTransformChainId
			  << " old_snapshot_reference_count="
			  << pending.oldSnapshotReferenceCount
			  << " new_snapshot_reference_count="
			  << pending.newSnapshotReferenceCount
			  << " expiry_time=" << pending.expiryTime.to_string(0)
			  << " feedback=0";
	}
	const auto lifecycle = maintainZhangTemporalProductSnapshots(state, active);
	if (!newlyDiscovered.empty() ||
		lifecycle.retainedBefore != lifecycle.retainedAfter ||
		!lifecycle.valid)
	{
		trace << "\nZHANG_TEMPORAL_SNAPSHOT_LIFECYCLE time="
			  << state.time.to_string(0)
			  << " active_transitions=" << lifecycle.activeTransitions
			  << " referenced_identities=" << lifecycle.referencedIdentities
			  << " pending_pinned_identities="
			  << lifecycle.pendingPinnedIdentities
			  << " retained_before=" << lifecycle.retainedBefore
			  << " retained_after=" << lifecycle.retainedAfter
			  << " status=" << (lifecycle.valid ? "AVAILABLE" : "REJECTED")
			  << " reason=" << lifecycle.failureReason
			  << " feedback=0";
	}
	return onlyNewlyDiscovered ? newlyDiscovered : active;
}

ZhangNamedProductIntegerSupport zhangNamedProductIntegerSupport(
    const KFState&                     integerLedgerState,
    E_Sys                              system,
    E_ObsCode                          observable,
    const vector<ZhangGraphEdge>&      physicalEdges,
    const vector<int>&                 physicalArcVersions,
    const ZhangExactVector&            coefficients
)
{
    ZhangNamedProductIntegerSupport result;
    if (physicalEdges.size() != physicalArcVersions.size() ||
        physicalEdges.size() != coefficients.size())
    {
        result.reason = "TARGET_DIMENSION_MISMATCH";
        return result;
    }
    const string runtimeId = zhangAmbresRuntimeId(integerLedgerState);
    if (!validZhangAmbresRuntimeId(runtimeId))
    {
        result.reason = "CHECKPOINT_RUNTIME_ID_UNBOUND";
        return result;
    }
    auto found = zhangPersistentHeldLattices.find({runtimeId, system});
    if (found == zhangPersistentHeldLattices.end() ||
        !found->second.consistent)
    {
        return result;
    }
    const auto& lattice = found->second;
    result.heldRank = static_cast<int>(lattice.rows.size());

    set<ZhangPhysicalIntegerArc> columnSet;
    for (const auto& held : lattice.rows)
    for (const auto& [arc, coefficient] : held.coefficients)
    {
        if (coefficient != 0)
        {
            columnSet.insert(arc);
        }
    }
    map<ZhangPhysicalIntegerArc, ZhangExactInteger> targetSparse;
    for (size_t index = 0; index < physicalEdges.size(); index++)
    {
        if (coefficients[index] == 0)
        {
            continue;
        }
        ZhangPhysicalIntegerArc arc{
            observable, physicalEdges[index], physicalArcVersions[index]};
        targetSparse[arc] += coefficients[index];
        columnSet.insert(arc);
    }
    vector<ZhangPhysicalIntegerArc> columns(columnSet.begin(), columnSet.end());
    map<ZhangPhysicalIntegerArc, size_t> columnIndex;
    for (size_t index = 0; index < columns.size(); index++)
    {
        columnIndex[columns[index]] = index;
    }
    ZhangExactMatrix rows;
    ZhangExactVector values;
    for (const auto& held : lattice.rows)
    {
        ZhangExactVector row(columns.size());
        for (const auto& [arc, coefficient] : held.coefficients)
        {
            row[columnIndex.at(arc)] = coefficient;
        }
        rows.push_back(std::move(row));
        values.push_back(held.value);
    }
    ZhangExactVector target(columns.size());
    for (const auto& [arc, coefficient] : targetSparse)
    {
        target[columnIndex.at(arc)] = coefficient;
    }
    auto membership = zhangIntegerRowLatticeContains(rows, target);
    if (!membership.contained)
    {
        result.reason = "NAMED_ROW_NOT_IN_HELD_LATTICE";
        return result;
    }
    ZhangExactInteger exactValue = 0;
    for (size_t row = 0; row < membership.combination.size(); row++)
    {
        exactValue += membership.combination[row] * values[row];
    }
    try
    {
        result.value = exactValue.convert_to<long long>();
    }
    catch (...)
    {
        result.reason = "NAMED_ROW_VALUE_OUT_OF_RANGE";
        return result;
    }
    result.contained = true;
    result.reason = "EXACT_AFFINE_HNF_MEMBERSHIP";
    return result;
}

/** Same-epoch integer decisions are provisional.  Admit a physical row to the
 * held lattice only after it has survived the configured multi-epoch product
 * confirmation window unchanged. */
static map<
    pair<string, E_Sys>,
    map<ZhangPersistentHeldRow, ZhangPersistentHeldEvidence>
> zhangPersistentHeldEvidence;

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

static string zhangPhysicalHeldLatticeFingerprint(
    ZhangPersistentHeldLattice lattice
)
{
    normalisePersistentHeldLattice(lattice);
    if (!lattice.consistent)
    {
        return "INCONSISTENT";
    }
    std::uint64_t hash = 1469598103934665603ULL;
    for (const ZhangPersistentHeldRow& row : lattice.rows)
    {
        for (const auto& [arc, coefficient] : row.coefficients)
        {
            const string identity = enum_to_string(arc.code) + ":" +
                arc.edge.receiver + ":" + arc.edge.satellite.id() +
                ":A" + std::to_string(arc.version) + ":" +
                coefficient.convert_to<string>() + ";";
            hash = zhangAuditFnv1a(hash, identity);
        }
        hash = zhangAuditFnv1a(hash, "|");
    }
    std::ostringstream stream;
    stream << std::hex << std::setw(16) << std::setfill('0') << hash;
    return stream.str();
}

static string zhangPhysicalAffineLatticeCanonicalKey(
    ZhangPersistentHeldLattice lattice
)
{
    normalisePersistentHeldLattice(lattice);
    if (!lattice.consistent)
    {
        return "INCONSISTENT";
    }
    std::ostringstream key;
    key << lattice.rows.size() << "|";
    for (const ZhangPersistentHeldRow& row : lattice.rows)
    {
        key << row.coefficients.size() << "{";
        for (const auto& [arc, coefficient] : row.coefficients)
        {
            const string identity = enum_to_string(arc.code) + ":" +
                arc.edge.receiver + ":" + arc.edge.satellite.id() +
                ":A" + std::to_string(arc.version);
            const string value = coefficient.convert_to<string>();
            key << identity.size() << ":" << identity << "="
                << value.size() << ":" << value << ";";
        }
        const string rhs = row.value.convert_to<string>();
        key << "}=" << rhs.size() << ":" << rhs << "|";
    }
    return key.str();
}

static string zhangPhysicalAffineLatticeFingerprint(
    const ZhangPersistentHeldLattice& lattice
)
{
    const string key = zhangPhysicalAffineLatticeCanonicalKey(lattice);
    if (key == "INCONSISTENT")
    {
        return key;
    }
    std::uint64_t hash = zhangAuditFnv1a(1469598103934665603ULL, key);
    std::ostringstream stream;
    stream << std::hex << std::setw(16) << std::setfill('0') << hash;
    return stream.str();
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

static bool addExactTargetToPhysicalRow(
    const ZhangGraphIntegerContext& context,
    E_ObsCode                       code,
    const vector<ZhangGraphEdge>&   chords,
    const ZhangExactVector&         exact,
    const ZhangExactInteger&        combinationMultiplier,
    map<ZhangPhysicalIntegerArc, ZhangExactInteger>& row
)
{
    if (exact.size() != chords.size())
    {
        return false;
    }
    for (int column = 0; column < static_cast<int>(exact.size()); column++)
    {
        if (exact[column] == 0)
        {
            continue;
        }
        if (!addCurrentCycleToPhysicalRow(
                context,
                code,
                chords[column],
                combinationMultiplier * exact[column],
                row
            ))
        {
            return false;
        }
    }
    removeZeroPhysicalCoefficients(row);
    return !row.empty();
}

struct ZhangCanonicalPhysicalSearchFrame
{
    bool     valid = false;
    GinAR_mtx source;
    MatrixXd absoluteProductCross;
    MatrixXd userProductCross;
    MatrixXd currentToCanonical;
    MatrixXd currentToPhysical;
    string   physicalAmbientFingerprint;
    string   canonicalPhysicalHnf;
};

static bool zhangCurrentCyclePhysicalMatrix(
    const ZhangGraphIntegerContext& context,
    const map<int, KFKey>&          ambiguityMap,
    MatrixXd&                       currentToPhysical,
    string&                         ambientFingerprint
)
{
    const int dimension = ambiguityMap.size();
    vector<map<ZhangPhysicalIntegerArc, ZhangExactInteger>> physicalRows(
        dimension);
    set<ZhangPhysicalIntegerArc> columnSet;
    for (int row = 0; row < dimension; row++)
    {
        auto key = ambiguityMap.find(row);
        if (key == ambiguityMap.end() ||
            !addCurrentCycleToPhysicalRow(
                context,
                static_cast<E_ObsCode>(key->second.num),
                {key->second.str, key->second.Sat},
                ZhangExactInteger(1),
                physicalRows[row]))
        {
            return false;
        }
        for (const auto& [arc, coefficient] : physicalRows[row])
        {
            if (coefficient != 0)
            {
                columnSet.insert(arc);
            }
        }
    }
    vector<ZhangPhysicalIntegerArc> columns(
        columnSet.begin(), columnSet.end());
    map<ZhangPhysicalIntegerArc, int> columnIndex;
    std::uint64_t hash = 1469598103934665603ULL;
    for (int column = 0; column < static_cast<int>(columns.size()); column++)
    {
        columnIndex[columns[column]] = column;
        const auto& arc = columns[column];
        hash = zhangAuditFnv1a(
            hash,
            enum_to_string(arc.code) + ":" + arc.edge.receiver + ":" +
                arc.edge.satellite.id() + ":A" +
                std::to_string(arc.version) + ";");
    }
    std::ostringstream fingerprint;
    fingerprint << std::hex << std::setw(16) << std::setfill('0') << hash;
    ambientFingerprint = fingerprint.str();

    currentToPhysical = MatrixXd::Zero(dimension, columns.size());
    for (int row = 0; row < dimension; row++)
    {
        for (const auto& [arc, coefficient] : physicalRows[row])
        {
            currentToPhysical(row, columnIndex.at(arc)) =
                coefficient.convert_to<double>();
        }
    }
    return currentToPhysical.allFinite();
}

static ZhangCanonicalPhysicalSearchFrame
zhangCanonicalPhysicalSearchFrame(
    const GinAR_mtx& source,
    const MatrixXd& absoluteProductCross,
    const MatrixXd& userProductCross,
    const MatrixXd& currentToPhysical,
    const string&   ambientFingerprint
)
{
    ZhangCanonicalPhysicalSearchFrame result;
    const int dimension = source.aflt.size();
    if (dimension == 0 || currentToPhysical.rows() != dimension ||
        source.Paflt.rows() != dimension ||
        source.Paflt.cols() != dimension ||
        absoluteProductCross.cols() != dimension ||
        userProductCross.cols() != dimension)
    {
        return result;
    }
    ZhangExactMatrix exactRows = zhangExactZeroMatrix(
        dimension, currentToPhysical.cols());
    for (int row = 0; row < currentToPhysical.rows(); row++)
    {
        for (int column = 0; column < currentToPhysical.cols(); column++)
        {
            const double raw = currentToPhysical(row, column);
            const double rounded = std::round(raw);
            if (!std::isfinite(raw) || std::abs(raw - rounded) > 1e-10)
            {
                return result;
            }
            exactRows[row][column] = static_cast<long long>(rounded);
        }
    }
    ZhangExactRowHnf hnf = zhangExactRowHermiteNormalForm(
        std::move(exactRows), {}, true);
    if (!hnf.consistent ||
        hnf.basis.size() != static_cast<size_t>(dimension) ||
        hnf.rowTransform.size() != static_cast<size_t>(dimension))
    {
        return result;
    }
    result.currentToCanonical = MatrixXd::Zero(dimension, dimension);
    MatrixXd canonicalPhysical = MatrixXd::Zero(
        dimension, currentToPhysical.cols());
    for (int row = 0; row < dimension; row++)
    {
        for (int column = 0; column < dimension; column++)
        {
            const ZhangExactInteger& value = hnf.rowTransform[row][column];
            const double converted = value.convert_to<double>();
            if (!std::isfinite(converted) ||
                std::abs(converted) > 9007199254740991.0 ||
                ZhangExactInteger(static_cast<long long>(converted)) != value)
            {
                return ZhangCanonicalPhysicalSearchFrame{};
            }
            result.currentToCanonical(row, column) = converted;
        }
        for (int column = 0;
             column < currentToPhysical.cols(); column++)
        {
            const ZhangExactInteger& value = hnf.basis[row][column];
            const double converted = value.convert_to<double>();
            if (!std::isfinite(converted) ||
                std::abs(converted) > 9007199254740991.0 ||
                ZhangExactInteger(static_cast<long long>(converted)) != value)
            {
                return ZhangCanonicalPhysicalSearchFrame{};
            }
            canonicalPhysical(row, column) = converted;
        }
    }
    const double physicalScale = std::max(1.0, canonicalPhysical.norm());
    if ((result.currentToCanonical * currentToPhysical -
         canonicalPhysical).norm() > 1e-11 * physicalScale)
    {
        return ZhangCanonicalPhysicalSearchFrame{};
    }
    result.source = source;
    result.source.aflt = result.currentToCanonical * source.aflt;
    result.source.Paflt = result.currentToCanonical * source.Paflt *
        result.currentToCanonical.transpose();
    result.source.Paflt = 0.5 *
        (result.source.Paflt + result.source.Paflt.transpose());
    result.source.ambmap.clear();
    result.absoluteProductCross = absoluteProductCross *
        result.currentToCanonical.transpose();
    result.userProductCross = userProductCross *
        result.currentToCanonical.transpose();
    result.currentToPhysical = currentToPhysical;
    result.physicalAmbientFingerprint = ambientFingerprint;
    result.canonicalPhysicalHnf = zhangExactMatrixFingerprint(hnf.basis);
    result.valid = result.source.aflt.allFinite() &&
        result.source.Paflt.allFinite() &&
        result.absoluteProductCross.allFinite() &&
        result.userProductCross.allFinite();
    return result;
}

struct ZhangWhitenedWlObservation
{
    GTime  time;
    double value = 0;
    double variance = 0;
};

struct ZhangWhitenedWlAccumulator
{
    std::deque<ZhangWhitenedWlObservation> observations;
    map<ZhangPhysicalIntegerArc, ZhangExactInteger> physicalRow;
    string physicalSegmentIdentity;
    int    lastGraphEventId = 0;
    int    lastProductDatumVersion = 0;
    bool   initialized = false;
};

static map<string, map<string, ZhangWhitenedWlAccumulator>>
    zhangWhitenedWlAccumulators;

/** E17 shadow estimator.  The scalar posterior/prior likelihood ratio is the
 * exact innovation-equivalent WL observation after nuisance-state elimination.
 * It is transported from the current S-basis to a persistent product
 * coordinate with the datum manager's exact alignment cycles.  The resulting
 * leave-one-out innovations are logged so temporal whiteness can be tested;
 * no constraint is fed back into the network state. */
static void traceZhangWhitenedWlFixedLag(
    Trace&                                      trace,
    const KFState&                             floatState,
    const GinAR_mtx&                           ambiguityResolution,
    E_Sys                                      system,
    E_ObsCode                                  firstCode,
    E_ObsCode                                  secondCode,
    const SatSys&                              anchor,
    const SatSys&                              satellite,
    const string&                              topologyKey,
    const VectorXd&                            wideLaneRow,
    const map<ZhangPhysicalIntegerArc, ZhangExactInteger>& physicalRow,
    const ZhangGraphIntegerContext&            context,
    GTime                                      time
)
{
    if (!acsConfig.zhangPppAr.whitened_wl_fixed_lag_shadow)
    {
        return;
    }
    const string runtimeId = zhangAmbresRuntimeId(floatState);
    auto reject = [&](const string& reason)
    {
        trace << "\nZHANG_WL_WHITENED_OBSERVATION time="
              << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " topology_key=" << topologyKey
              << " anchor=" << anchor.id()
              << " satellite=" << satellite.id()
              << " status=REJECTED reason=" << reason
              << " source=KALMAN_INNOVATION_LIKELIHOOD_RATIO feedback=0";
    };

    auto runtimeMoments = zhangRelinkPriorMoments.find(runtimeId);
    auto prior = runtimeMoments == zhangRelinkPriorMoments.end()
        ? map<ZhangRelinkMomentKey, ZhangRelinkPriorMoment>::iterator{}
        : runtimeMoments->second.find({system, anchor, satellite});
    if (!validZhangAmbresRuntimeId(runtimeId) ||
        runtimeMoments == zhangRelinkPriorMoments.end() ||
        prior == runtimeMoments->second.end() ||
        std::abs((time - prior->second.time).to_double()) > 1e-3)
    {
        reject("PRIOR_RELATION_MISSING_OR_MISMATCHED");
        return;
    }

    vector<pair<int, double>> sparse;
    double posteriorMean = 0;
    double posteriorVariance = 0;
    for (int column = 0; column < wideLaneRow.size(); column++)
    {
        if (wideLaneRow(column) == 0)
        {
            continue;
        }
        auto ambiguity = ambiguityResolution.ambmap.find(column);
        auto state = ambiguity == ambiguityResolution.ambmap.end()
            ? floatState.kfIndexMap.end()
            : floatState.kfIndexMap.find(ambiguity->second);
        if (ambiguity == ambiguityResolution.ambmap.end() ||
            state == floatState.kfIndexMap.end())
        {
            reject("POSTERIOR_AMBIGUITY_MISSING");
            return;
        }
        sparse.push_back({state->second, wideLaneRow(column)});
        posteriorMean += wideLaneRow(column) * floatState.x(state->second);
    }
    if (sparse.empty() || physicalRow.empty())
    {
        reject("EMPTY_TARGET_OR_PHYSICAL_ROW");
        return;
    }
    for (const auto& [leftIndex, leftCoefficient] : sparse)
    for (const auto& [rightIndex, rightCoefficient] : sparse)
    {
        posteriorVariance += leftCoefficient * rightCoefficient *
            floatState.P(leftIndex, rightIndex);
    }

    const double priorMean = prior->second.wideLaneMean;
    const double priorVariance = prior->second.wideLaneVariance;
    if (!std::isfinite(priorMean) || !std::isfinite(posteriorMean) ||
        !std::isfinite(priorVariance) || !std::isfinite(posteriorVariance) ||
        priorVariance <= 0 || posteriorVariance <= 0)
    {
        reject("NON_POSITIVE_OR_NONFINITE_MARGINAL");
        return;
    }
    const double information =
        1 / posteriorVariance - 1 / priorVariance;
    const double natural =
        posteriorMean / posteriorVariance - priorMean / priorVariance;
    if (!std::isfinite(information) || !std::isfinite(natural) ||
        information <
            acsConfig.zhangPppAr.multi_epoch_relink_shadow_information_floor)
    {
        reject("NON_POSITIVE_INFORMATION_INCREMENT");
        return;
    }

    const auto firstAnchor = zhangSatelliteDatumStatus(
        system, firstCode, anchor
    );
    const auto firstSatellite = zhangSatelliteDatumStatus(
        system, firstCode, satellite
    );
    const auto secondAnchor = zhangSatelliteDatumStatus(
        system, secondCode, anchor
    );
    const auto secondSatellite = zhangSatelliteDatumStatus(
        system, secondCode, satellite
    );
    const string segmentIdentity =
        anchor.id() + ":" + std::to_string(firstAnchor.phaseSegment) + ":" +
        std::to_string(secondAnchor.phaseSegment) + "->" + satellite.id() +
        ":" + std::to_string(firstSatellite.phaseSegment) + ":" +
        std::to_string(secondSatellite.phaseSegment);
    const long long firstAlignment =
        firstSatellite.alignmentCycles - firstAnchor.alignmentCycles;
    const long long secondAlignment =
        secondSatellite.alignmentCycles - secondAnchor.alignmentCycles;
    const double rawObservation = natural / information;
    const double observation =
        rawObservation - firstAlignment + secondAlignment;
    const double observationVariance = 1 / information;

    const string accumulatorKey = enum_to_string(system) + ":WL:" +
        anchor.id() + ":" + satellite.id();
    auto& accumulator =
        zhangWhitenedWlAccumulators[runtimeId][accumulatorKey];
    bool reset = false;
    bool basisTransport = false;
    bool basisSwitch = false;
    bool physicalSignatureChanged = false;
    bool arcVersionConflict = false;
    string resetReason = "NONE";

    if (accumulator.initialized)
    {
        if (accumulator.physicalSegmentIdentity != segmentIdentity)
        {
            reset = true;
            resetReason = "SATELLITE_PHASE_SEGMENT_CHANGED";
        }
        else if (accumulator.lastProductDatumVersion !=
                 context.productDatumVersion)
        {
            reset = true;
            resetReason = "PRODUCT_DATUM_VERSION_CHANGED";
        }
        else if (!accumulator.observations.empty())
        {
            const double gap =
                (time - accumulator.observations.back().time).to_double();
            if (gap < -1e-3)
            {
                reset = true;
                resetReason = "TIME_REVERSED";
            }
            else if (std::abs(gap) <= 1e-3)
            {
                reject("DUPLICATE_EPOCH_OBSERVATION");
                return;
            }
        }

        physicalSignatureChanged = accumulator.physicalRow != physicalRow;
        if (!reset && physicalSignatureChanged)
        {
            map<pair<E_ObsCode, ZhangGraphEdge>, int> previousVersions;
            map<pair<E_ObsCode, ZhangGraphEdge>, int> currentVersions;
            for (const auto& [arc, coefficient] : accumulator.physicalRow)
            {
                if (coefficient != 0)
                {
                    previousVersions[{arc.code, arc.edge}] = arc.version;
                }
            }
            for (const auto& [arc, coefficient] : physicalRow)
            {
                if (coefficient != 0)
                {
                    currentVersions[{arc.code, arc.edge}] = arc.version;
                }
            }
            for (const auto& [edge, version] : previousVersions)
            {
                auto current = currentVersions.find(edge);
                if (current != currentVersions.end() &&
                    current->second != version)
                {
                    arcVersionConflict = true;
                    break;
                }
            }
            if (arcVersionConflict)
            {
                reset = true;
                resetReason = "PHYSICAL_ARC_VERSION_CHANGED";
            }
            else if (context.eventId != accumulator.lastGraphEventId)
            {
                // G*k is not itself invariant under a tree exchange; the
                // omitted z_T term can change it by a large integer even when
                // every physical arc version is continuous.  Until the exact
                // z_T stochastic target is carried in this estimator, fail
                // closed instead of mixing two integer coordinates.
                basisSwitch = true;
                reset = true;
                resetReason = "S_BASIS_PHYSICAL_COORDINATE_CHANGED";
            }
            else
            {
                reset = true;
                resetReason = "PHYSICAL_SUPPORT_CHANGED_WITHOUT_GRAPH_EVENT";
            }
        }
    }
    if (reset)
    {
        accumulator.observations.clear();
    }

    accumulator.initialized = true;
    accumulator.physicalSegmentIdentity = segmentIdentity;
    accumulator.physicalRow = physicalRow;
    accumulator.lastGraphEventId = context.eventId;
    accumulator.lastProductDatumVersion = context.productDatumVersion;

    const double lagSeconds =
        acsConfig.zhangPppAr.whitened_wl_fixed_lag_seconds;
    while (!accumulator.observations.empty() &&
           (time - accumulator.observations.front().time).to_double() >
                lagSeconds + 1e-3)
    {
        accumulator.observations.pop_front();
    }

    double priorWindowInformation = 0;
    double priorWindowNatural = 0;
    for (const auto& item : accumulator.observations)
    {
        priorWindowInformation += 1 / item.variance;
        priorWindowNatural += item.value / item.variance;
    }
    double predictionResidual = std::numeric_limits<double>::quiet_NaN();
    if (priorWindowInformation > 0)
    {
        const double predictionMean =
            priorWindowNatural / priorWindowInformation;
        const double predictionVariance =
            observationVariance + 1 / priorWindowInformation;
        predictionResidual =
            (observation - predictionMean) / std::sqrt(predictionVariance);
    }
    const bool predictionRejected =
        static_cast<int>(accumulator.observations.size()) >=
            acsConfig.zhangPppAr.whitened_wl_prediction_gate_min_observations &&
        std::isfinite(predictionResidual) &&
        std::abs(predictionResidual) >
            acsConfig.zhangPppAr.whitened_wl_prediction_gate_sigma;

    trace << "\nZHANG_WL_WHITENED_OBSERVATION time="
          << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " topology_key=" << topologyKey
          << " anchor=" << anchor.id()
          << " satellite=" << satellite.id()
          << " raw_observation=" << rawObservation
          << " l1_alignment=" << firstAlignment
          << " l2_alignment=" << secondAlignment
          << " persistent_observation=" << observation
          << " variance=" << observationVariance
          << " whitened_prediction_residual=" << predictionResidual
          << " phase_segment_identity=" << segmentIdentity
          << " physical_arcs=" << physicalRow.size()
          << " graph_event_id=" << context.eventId
          << " product_datum_version=" << context.productDatumVersion
          << " physical_signature_changed=" << physicalSignatureChanged
          << " arc_version_conflict=" << arcVersionConflict
          << " basis_transport=" << basisTransport
          << " basis_switch=" << basisSwitch
          << " window_reset=" << reset
          << " window_reset_reason=" << resetReason
          << " status=" << (predictionRejected ? "REJECTED" : "ACCEPTED")
          << " reason="
          << (predictionRejected ? "PREDICTION_GATE" : "NONE")
          << " source=KALMAN_INNOVATION_LIKELIHOOD_RATIO feedback=0";
    if (predictionRejected)
    {
        return;
    }

    accumulator.observations.push_back({
        time, observation, observationVariance
    });
    while (static_cast<int>(accumulator.observations.size()) >
           acsConfig.zhangPppAr.whitened_wl_fixed_lag_max_observations)
    {
        accumulator.observations.pop_front();
    }

    double accumulatedInformation = 0;
    double accumulatedNatural = 0;
    for (const auto& item : accumulator.observations)
    {
        accumulatedInformation += 1 / item.variance;
        accumulatedNatural += item.value / item.variance;
    }
    const double accumulatedMean =
        accumulatedNatural / accumulatedInformation;
    const double accumulatedVariance = 1 / accumulatedInformation;
    double chiSquare = 0;
    for (const auto& item : accumulator.observations)
    {
        const double residual = item.value - accumulatedMean;
        chiSquare += residual * residual / item.variance;
    }
    const int degreesOfFreedom = std::max(
        0, static_cast<int>(accumulator.observations.size()) - 1
    );
    const double reducedChiSquare = degreesOfFreedom > 0
        ? chiSquare / degreesOfFreedom
        : 0;
    const double fractional =
        accumulatedMean - std::round(accumulatedMean);
    const double perr = round_perr(fractional, accumulatedVariance);

    trace << "\nZHANG_WL_FIXED_LAG_SHADOW time="
          << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " topology_key=" << topologyKey
          << " anchor=" << anchor.id()
          << " satellite=" << satellite.id()
          << " observations=" << accumulator.observations.size()
          << " lag_seconds=" << lagSeconds
          << " mean=" << accumulatedMean
          << " variance=" << accumulatedVariance
          << " fractional=" << fractional
          << " perr=" << perr
          << " chi_square=" << chiSquare
          << " degrees_of_freedom=" << degreesOfFreedom
          << " reduced_chi_square=" << reducedChiSquare
          << " reset=" << reset
          << " reset_reason=" << resetReason
          << " basis_transport=" << basisTransport
          << " phase_segment_identity=" << segmentIdentity
          << " feedback=0";
}

static pair<int, int> appendPersistentHeldRows(
    Trace&           trace,
    const KFState&   ledgerState,
    const KFState&   graphState,
    const GinAR_mtx& fixed
)
{
    int added = 0;
    int pending = 0;
    int rejected = 0;
    const string ledgerRuntimeId = zhangAmbresRuntimeId(ledgerState);
    if (!validZhangAmbresRuntimeId(ledgerRuntimeId))
    {
        trace << "\nZHANG_HELD_LATTICE_ADMISSION time="
              << ledgerState.time.to_string(0)
              << " status=REJECTED reason=CHECKPOINT_RUNTIME_ID_UNBOUND";
        return {0, fixed.Ztrs.rows()};
    }
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
                if (!zhangGraphIntegerContext(graphState, *targetSystem, snapshot))
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
        auto identity = std::make_pair(ledgerRuntimeId, *targetSystem);
        auto& evidenceSet = zhangPersistentHeldEvidence[identity];
        long int epoch = static_cast<long int>(
            std::llround(ledgerState.time.bigTime)
        );
        double maxGap =
            acsConfig.zhangPppAr.promotion_confirmation_max_gap_seconds;
        for (auto evidence = evidenceSet.begin(); evidence != evidenceSet.end();)
        {
            bool expired = evidence->second.confirmations > 0 &&
                epoch != evidence->second.lastEpoch &&
                maxGap > 0 &&
                epoch - evidence->second.lastEpoch > maxGap;
            if (expired)
            {
                evidence = evidenceSet.erase(evidence);
            }
            else
            {
                ++evidence;
            }
        }

        auto& evidence = evidenceSet[physical];
        bool sameEpoch = epoch == evidence.lastEpoch;
        bool sameSequence = evidence.confirmations > 0 &&
            !sameEpoch &&
            (maxGap <= 0 || epoch - evidence.lastEpoch <= maxGap);
        if (!sameSequence && !sameEpoch)
        {
            evidence.confirmations = 0;
        }
        if (!sameEpoch)
        {
            evidence.lastEpoch = epoch;
            evidence.confirmations++;
        }

        int required = std::max(
            1,
            acsConfig.zhangPppAr.promotion_confirmation_epochs
        );
        if (evidence.confirmations < required)
        {
            pending++;
            trace << "\nZHANG_HELD_LATTICE_ADMISSION time="
                  << ledgerState.time.to_string(0)
                  << " system=" << enum_to_string(*targetSystem)
                  << " status=PENDING_CONFIRMATION"
                  << " confirmation_count=" << evidence.confirmations
                  << " confirmation_required=" << required
                  << " physical_terms=" << physical.coefficients.size();
            continue;
        }

        auto& lattice = zhangPersistentHeldLattices[identity];
        lattice.rows.push_back(physical);
        evidenceSet.erase(physical);
        added++;
        trace << "\nZHANG_HELD_LATTICE_ADMISSION time="
              << ledgerState.time.to_string(0)
              << " system=" << enum_to_string(*targetSystem)
              << " status=ACCEPTED"
              << " confirmation_count=" << required
              << " confirmation_required=" << required
              << " physical_terms=" << physical.coefficients.size();
    }

    for (auto& [system, context] : contexts)
    {
        auto& lattice =
            zhangPersistentHeldLattices[{ledgerRuntimeId, system}];
        normalisePersistentHeldLattice(lattice);
        lattice.lastEventId = context.eventId;
        trace << "\nZHANG_HELD_LATTICE_NORMALISE time="
              << ledgerState.time.to_string(0)
              << " system=" << enum_to_string(system)
              << " added_rows=" << added
              << " pending_rows=" << pending
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
static vector<ZhangProjectedHeldSet> projectPersistentHeldRows(
    Trace&         trace,
    const KFState& kfState
)
{
    vector<ZhangProjectedHeldSet> projectedSets;
    const string runtimeId = zhangAmbresRuntimeId(kfState);
    if (!validZhangAmbresRuntimeId(runtimeId))
    {
        trace << "\nZHANG_HELD_LATTICE_PROJECT time="
              << kfState.time.to_string(0)
              << " status=REJECTED reason=CHECKPOINT_RUNTIME_ID_UNBOUND";
        return projectedSets;
    }
    for (auto& [identity, lattice] : zhangPersistentHeldLattices)
    {
        if (identity.first != runtimeId || lattice.rows.empty())
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
        vector<map<E_ObsCode, set<SatSys>>> projectedProductSupport;
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
            map<E_ObsCode, set<SatSys>> support;
            for (const auto& [arc, coefficient] : held.coefficients)
            {
                if (coefficient != 0)
                {
                    support[arc.code].insert(arc.edge.satellite);
                }
            }
            projectedProductSupport.push_back(std::move(support));
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
        ZhangProjectedHeldSet projectedSet;
        projectedSet.system = system;
        projectedSet.constraints = std::move(projected);
        projectedSet.rowProductSupport = std::move(projectedProductSupport);
        projectedSets.push_back(std::move(projectedSet));
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
            const string runtimeId = zhangAmbresRuntimeId(kfState);
            auto latticeIt = validZhangAmbresRuntimeId(runtimeId)
                ? zhangPersistentHeldLattices.find({runtimeId, sys})
                : zhangPersistentHeldLattices.end();
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

/** Apply exact linear equality constraints to a disposable Zhang fixed branch.
 *
 * The authoritative network filter must never call this function.  The
 * covariance becomes positive semidefinite in the constrained directions,
 * which is valid for same-epoch product extraction but must not be propagated
 * through the ordinary next-epoch KF machinery.
 */
static bool conditionZhangAmbiguitiesExactly(
    Trace&           trace,
    KFState&         kfState,
    const GinAR_mtx& mtrx,
    const string&    provenance
)
{
    const int rows = mtrx.zfix.size();
    const int ambiguityColumns = mtrx.ambmap.size();
    if (rows == 0)
    {
        return true;
    }
    if (mtrx.Ztrs.rows() != rows || mtrx.Ztrs.cols() != ambiguityColumns)
    {
        zhangTransactionalConditioningReason = "CONSTRAINT_DIMENSION_MISMATCH";
        zhangTransactionalConditioningFailed = true;
        return false;
    }

    MatrixXd A = MatrixXd::Zero(rows, kfState.x.size());
    for (int column = 0; column < ambiguityColumns; column++)
    {
        auto keyIt = mtrx.ambmap.find(column);
        if (keyIt == mtrx.ambmap.end())
        {
            zhangTransactionalConditioningReason = "AMBIGUITY_COLUMN_MISSING";
            zhangTransactionalConditioningFailed = true;
            return false;
        }
        auto stateIt = kfState.kfIndexMap.find(keyIt->second);
        if (stateIt == kfState.kfIndexMap.end())
        {
            zhangTransactionalConditioningReason = "AMBIGUITY_STATE_MISSING";
            zhangTransactionalConditioningFailed = true;
            return false;
        }
        A.col(stateIt->second) = mtrx.Ztrs.col(column);
    }

    VectorXd innovation = mtrx.zfix - A * kfState.x;
    MatrixXd AP = A * kfState.P;
    MatrixXd constraintCovariance = AP * A.transpose();
    constraintCovariance =
        0.5 * (constraintCovariance + constraintCovariance.transpose());
    Eigen::SelfAdjointEigenSolver<MatrixXd> eigenSolver(constraintCovariance);
    if (eigenSolver.info() != Eigen::Success ||
        !eigenSolver.eigenvalues().allFinite())
    {
        zhangTransactionalConditioningReason = "CONSTRAINT_EIGENSOLVER_FAILED";
        zhangTransactionalConditioningFailed = true;
        return false;
    }

    const double largestEigenvalue = eigenSolver.eigenvalues().maxCoeff();
    const double smallestEigenvalue = eigenSolver.eigenvalues().minCoeff();
    const double rankTolerance = std::max(1e-14, 1e-12 * largestEigenvalue);
    if (largestEigenvalue < -rankTolerance)
    {
        zhangTransactionalConditioningReason = "CONSTRAINT_COVARIANCE_NEGATIVE";
        zhangTransactionalConditioningFailed = true;
        trace << "\nZHANG_TRANSACTIONAL_CONDITION time="
              << kfState.time.to_string(0)
              << " provenance=" << provenance
              << " rows=" << rows
              << " status=REJECTED"
              << " reason=" << zhangTransactionalConditioningReason
              << " min_eigenvalue=" << smallestEigenvalue
              << " max_eigenvalue=" << largestEigenvalue;
        return false;
    }

    VectorXd innovationCoordinates =
        eigenSolver.eigenvectors().transpose() * innovation;
    VectorXd inverseEigenvalues = VectorXd::Zero(rows);
    int effectiveRank = 0;
    double maximumNullInnovation = 0;
    for (int index = 0; index < rows; index++)
    {
        if (eigenSolver.eigenvalues()(index) > rankTolerance)
        {
            inverseEigenvalues(index) =
                1.0 / eigenSolver.eigenvalues()(index);
            effectiveRank++;
        }
        else
        {
            maximumNullInnovation = std::max(
                maximumNullInnovation,
                std::abs(innovationCoordinates(index))
            );
        }
    }
    if (maximumNullInnovation > 1e-7)
    {
        zhangTransactionalConditioningReason =
            "REDUNDANT_CONSTRAINT_INCONSISTENT";
        zhangTransactionalConditioningFailed = true;
        trace << "\nZHANG_TRANSACTIONAL_CONDITION time="
              << kfState.time.to_string(0)
              << " provenance=" << provenance
              << " rows=" << rows
              << " effective_rank=" << effectiveRank
              << " status=REJECTED"
              << " reason=" << zhangTransactionalConditioningReason
              << " maximum_null_innovation=" << maximumNullInnovation;
        return false;
    }
    if (effectiveRank == 0)
    {
        trace << "\nZHANG_TRANSACTIONAL_CONDITION time="
              << kfState.time.to_string(0)
              << " provenance=" << provenance
              << " rows=" << rows
              << " effective_rank=0"
              << " status=APPLIED"
              << " reason=REDUNDANT_CONSTRAINT_ALREADY_SATISFIED"
              << " maximum_null_innovation=" << maximumNullInnovation;
        return true;
    }
    MatrixXd inverseConstraintCovariance =
        eigenSolver.eigenvectors() * inverseEigenvalues.asDiagonal() *
        eigenSolver.eigenvectors().transpose();
    double nis = innovation.dot(inverseConstraintCovariance * innovation);
    vector<pair<double, int>> marginalNis;
    marginalNis.reserve(rows);
    for (int row = 0; row < rows; row++)
    {
        const double variance = constraintCovariance(row, row);
        if (std::isfinite(variance) && variance > rankTolerance &&
            std::isfinite(innovation(row)))
        {
            marginalNis.push_back({
                innovation(row) * innovation(row) / variance,
                row
            });
        }
    }
    std::sort(marginalNis.begin(), marginalNis.end());
    auto marginalQuantile = [&](double probability)
    {
        if (marginalNis.empty())
        {
            return 0.0;
        }
        const int index = std::clamp(
            static_cast<int>(std::floor(
                probability * (marginalNis.size() - 1)
            )),
            0,
            static_cast<int>(marginalNis.size()) - 1
        );
        return marginalNis[index].first;
    };
    const double maximumMarginalNis = marginalNis.empty()
        ? 0
        : marginalNis.back().first;
    const int maximumMarginalRow = marginalNis.empty()
        ? -1
        : marginalNis.back().second;
    double alpha = acsConfig.zhangPppAr.held_constraint_nis_alpha;
    boost::math::chi_squared distribution(effectiveRank);
    double nisThreshold = quantile(complement(distribution, alpha));
    if (!std::isfinite(nis) || nis > nisThreshold)
    {
        zhangTransactionalConditioningReason = "CONSTRAINT_NIS_REJECTED";
        zhangTransactionalConditioningFailed = true;
        trace << "\nZHANG_TRANSACTIONAL_CONDITION time="
              << kfState.time.to_string(0)
              << " provenance=" << provenance
              << " rows=" << rows
              << " effective_rank=" << effectiveRank
              << " status=REJECTED"
              << " reason=" << zhangTransactionalConditioningReason
              << " nis=" << nis
              << " threshold=" << nisThreshold
              << " marginal_nis_p50=" << marginalQuantile(0.50)
              << " marginal_nis_p90=" << marginalQuantile(0.90)
              << " marginal_nis_max=" << maximumMarginalNis
              << " marginal_nis_max_row=" << maximumMarginalRow;
        return false;
    }

    MatrixXd PAt = AP.transpose();
    VectorXd conditionedState =
        kfState.x + PAt * inverseConstraintCovariance * innovation;

    // Form the constrained covariance as a Gram matrix in a square-root
    // coordinate system.  The algebraically equivalent subtractive update
    // P - PA' (APA')+ AP loses positive semidefiniteness when the float state
    // spans metre-level clock modes and near-zero ambiguity directions.
    MatrixXd priorCovariance =
        0.5 * (kfState.P + kfState.P.transpose());
    Eigen::SelfAdjointEigenSolver<MatrixXd> priorEigenSolver(priorCovariance);
    if (priorEigenSolver.info() != Eigen::Success ||
        !priorEigenSolver.eigenvalues().allFinite())
    {
        zhangTransactionalConditioningReason = "PRIOR_COVARIANCE_EIGENSOLVER_FAILED";
        zhangTransactionalConditioningFailed = true;
        return false;
    }
    const double priorScale = std::max(
        1.0, priorEigenSolver.eigenvalues().cwiseAbs().maxCoeff());
    if (priorEigenSolver.eigenvalues().minCoeff() < -1e-9 * priorScale)
    {
        zhangTransactionalConditioningReason = "PRIOR_COVARIANCE_NOT_PSD";
        zhangTransactionalConditioningFailed = true;
        return false;
    }
    VectorXd squareRootEigenvalues =
        priorEigenSolver.eigenvalues().cwiseMax(0).cwiseSqrt();
    MatrixXd priorSquareRoot = priorEigenSolver.eigenvectors() *
        squareRootEigenvalues.asDiagonal();
    MatrixXd whitenedConstraint = A * priorSquareRoot;
    MatrixXd constraintRightBasis = MatrixXd::Zero(
        kfState.x.size(), effectiveRank);
    int basisColumn = 0;
    for (int index = 0; index < rows; index++)
    {
        const double eigenvalue = eigenSolver.eigenvalues()(index);
        if (eigenvalue <= rankTolerance)
        {
            continue;
        }
        constraintRightBasis.col(basisColumn++) =
            whitenedConstraint.transpose() * eigenSolver.eigenvectors().col(index) /
            std::sqrt(eigenvalue);
    }
    MatrixXd conditionedSquareRoot = priorSquareRoot;
    if (basisColumn > 0)
    {
        const MatrixXd activeBasis = constraintRightBasis.leftCols(basisColumn);
        conditionedSquareRoot -=
            (priorSquareRoot * activeBasis) * activeBasis.transpose();
    }
    MatrixXd conditionedCovariance =
        conditionedSquareRoot * conditionedSquareRoot.transpose();
    conditionedCovariance =
        0.5 * (conditionedCovariance + conditionedCovariance.transpose());
    double closure = (A * conditionedState - mtrx.zfix)
                         .lpNorm<Eigen::Infinity>();
    double diagonalScale = std::max(
        1.0,
        conditionedCovariance.diagonal().cwiseAbs().maxCoeff()
    );
    double minimumDiagonal = conditionedCovariance.diagonal().minCoeff();
    double covarianceClosure =
        (A * conditionedCovariance).lpNorm<Eigen::Infinity>();
    if (!conditionedState.allFinite() || !conditionedCovariance.allFinite() ||
        closure > 1e-7 || minimumDiagonal < 0 ||
        covarianceClosure > 1e-8 * diagonalScale)
    {
        zhangTransactionalConditioningReason = "CONDITIONED_STATE_NUMERIC_FAILURE";
        zhangTransactionalConditioningFailed = true;
        trace << "\nZHANG_TRANSACTIONAL_CONDITION time="
              << kfState.time.to_string(0)
              << " provenance=" << provenance
              << " rows=" << rows
              << " status=REJECTED"
              << " reason=" << zhangTransactionalConditioningReason
              << " closure=" << closure
              << " covariance_closure=" << covarianceClosure
              << " minimum_diagonal=" << minimumDiagonal;
        return false;
    }

    kfState.x = std::move(conditionedState);
    kfState.P = std::move(conditionedCovariance);
    trace << "\nZHANG_TRANSACTIONAL_CONDITION time="
          << kfState.time.to_string(0)
          << " provenance=" << provenance
          << " rows=" << rows
          << " effective_rank=" << effectiveRank
          << " status=APPLIED"
          << " nis=" << nis
          << " threshold=" << nisThreshold
          << " closure=" << closure
          << " covariance_closure=" << covarianceClosure
          << " minimum_diagonal=" << minimumDiagonal
          << " min_eigenvalue=" << smallestEigenvalue;
    return true;
}

void applyUCAmbiguities(
    Trace&     trace,    ///< Debug trace
    KFState&   kfState,  ///< Reference to Kalman filter containing float solutions
    GinAR_mtx& mtrx,  ///< Reference to structure containing fixed ambiguities and Z transformations
    const string& provenance = "NEW_INTEGER"
)
{
    const bool canonicalUserIntegerStrategy =
        acsConfig.zhangPppAr.integer_strategy == "CANONICAL_USER_SD_WL_L1" ||
        acsConfig.zhangPppAr.integer_strategy == "CANONICAL_USER_IF_WL_L1";
    if ((acsConfig.zhangFullRank.enable || canonicalUserIntegerStrategy) &&
        acsConfig.zhangPppAr.transactional_integer_fixing)
    {
        conditionZhangAmbiguitiesExactly(trace, kfState, mtrx, provenance);
        return;
    }

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
/** Select a positive-variance principal subset while preserving original
 * integer target rows.  This is the numerical rank half of the exact-HNF plus
 * QR/Cholesky policy: deterministic held directions are omitted from the new
 * integer search rather than regularised with artificial noise. */
static vector<int> positiveVarianceTargetSubset(const MatrixXd& covariance)
{
    const int size = covariance.rows();
    if (size == 0 || covariance.cols() != size)
    {
        return {};
    }
    MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
    VectorXd residual = symmetric.diagonal();
    MatrixXd factor = MatrixXd::Zero(size, size);
    vector<bool> available(size, true);
    vector<int> selected;
    double scale = std::max(1e-12, residual.cwiseAbs().maxCoeff());
    double tolerance = std::max(1e-14, 1e-10 * scale);

    for (int rank = 0; rank < size; rank++)
    {
        int pivot = -1;
        double pivotVariance = -std::numeric_limits<double>::infinity();
        for (int index = 0; index < size; index++)
        {
            if (available[index] && residual(index) > pivotVariance)
            {
                pivot = index;
                pivotVariance = residual(index);
            }
        }
        if (pivot < 0 || !(pivotVariance > tolerance))
        {
            break;
        }

        available[pivot] = false;
        selected.push_back(pivot);
        factor(pivot, rank) = std::sqrt(pivotVariance);
        for (int index = 0; index < size; index++)
        {
            if (!available[index])
            {
                continue;
            }
            double previous = 0;
            for (int column = 0; column < rank; column++)
            {
                previous += factor(index, column) * factor(pivot, column);
            }
            double entry =
                (symmetric(index, pivot) - previous) / factor(pivot, rank);
            factor(index, rank) = entry;
            residual(index) = std::max(0.0, residual(index) - entry * entry);
        }
    }
    std::sort(selected.begin(), selected.end());
    return selected;
}

/** Recover current-relink targets that are already exact consequences of the
 * conditioned integer lattice.  A zero-variance named target is not a new
 * stochastic ambiguity fix and must not be regularised or applied again, but
 * an integer-valued closure is valid evidence for reacquiring the product
 * coordinate of an existing persistent relation. */
static std::map<std::size_t, ZhangExactInteger>
recoverDeterministicRelinkTargets(
    Trace&                   trace,
    const VectorXd&          values,
    const MatrixXd&          covariance,
    const std::vector<bool>& currentRelink,
    GTime                    time,
    const std::string&       label
)
{
    std::map<std::size_t, ZhangExactInteger> recovered;
    if (values.size() == 0 || covariance.rows() != values.size() ||
        covariance.cols() != values.size() ||
        currentRelink.size() != static_cast<std::size_t>(values.size()))
    {
        return recovered;
    }

    const VectorXd diagonal = covariance.diagonal();
    const double scale = std::max(1e-12, diagonal.cwiseAbs().maxCoeff());
    const double varianceTolerance = std::max({
        1e-14,
        1e-10 * scale,
        acsConfig.zhangPppAr.deterministic_relink_variance_tolerance_cycles2
    });
    constexpr double integerTolerance = 1e-8;
    int rejectedNonInteger = 0;
    int rejectedNegativeVariance = 0;
    for (int index = 0; index < values.size(); index++)
    {
        if (!currentRelink[index] || !std::isfinite(values(index)) ||
            !std::isfinite(diagonal(index)))
        {
            continue;
        }
        if (diagonal(index) < -varianceTolerance)
        {
            rejectedNegativeVariance++;
            continue;
        }
        if (diagonal(index) > varianceTolerance)
        {
            continue;
        }
        const long long integer = std::llround(values(index));
        if (std::abs(values(index) - integer) > integerTolerance)
        {
            rejectedNonInteger++;
            continue;
        }
        recovered[index] = integer;
    }
    trace << "\nZHANG_DETERMINISTIC_RELINK time=" << time.to_string(0)
          << " label=" << label
          << " candidates=" << values.size()
          << " recovered=" << recovered.size()
          << " rejected_noninteger=" << rejectedNonInteger
          << " rejected_negative_variance=" << rejectedNegativeVariance
          << " variance_tolerance=" << varianceTolerance
          << " integer_tolerance=" << integerTolerance;
    return recovered;
}

static ZhangIntegerCandidateNis assessZhangIntegerCandidateNis(
    const GinAR_mtx& search,
    double           alpha
)
{
    if (search.zfix.size() == 0 ||
        search.Ztrs.rows() != search.zfix.size() ||
        search.Ztrs.cols() != search.aflt.size())
    {
        return {};
    }
    return assessZhangIntegerCandidateNis(
        search.zfix - search.Ztrs * search.aflt,
        search.Ztrs * search.Paflt * search.Ztrs.transpose(),
        alpha
    );
}

struct ZhangTemporalTransitionProjection
{
    bool     representable = false;
    bool     requiresBesd = false;
    VectorXd row;
    double   affineCycles = 0;
    int      physicalTerms = 0;
    int      chordTerms = 0;
    string   reason = "UNCLASSIFIED";
};

/** Project one product-functional change onto the current fundamental-cycle
 * coordinates.  The chord coefficient is unique, so reconstruction and exact
 * equality provide a basis-independent membership proof without rounding. */
static ZhangTemporalTransitionProjection projectTemporalProductTransition(
    const ZhangPendingProductTransition& pending,
    const ZhangGraphIntegerContext&       context,
    const GinAR_mtx&                      ambiguityResolution
)
{
    ZhangTemporalTransitionProjection result;
    result.row = VectorXd::Zero(ambiguityResolution.aflt.size());
	if (pending.phaseSegmentChanged)
	{
		result.requiresBesd = true;
		result.reason = "REQUIRES_BESD_PHASE_SEGMENT_CHANGED";
		return result;
	}
    const auto& transition = pending.transition;
    if (!transition.valid ||
        transition.physicalEdges.size() != transition.coefficients.size() ||
        transition.physicalEdges.size() !=
            transition.physicalArcVersions.size())
    {
        result.reason = transition.failureReason.empty()
            ? "INVALID_TRANSITION" : transition.failureReason;
        return result;
    }
    result.affineCycles = transition.affineOffsetCycles.convert_to<double>();

    map<ZhangPhysicalIntegerArc, ZhangExactInteger> target;
    for (size_t index = 0; index < transition.coefficients.size(); index++)
    {
        if (transition.coefficients[index] == 0)
        {
            continue;
        }
        ZhangPhysicalIntegerArc arc{
            pending.observable,
            transition.physicalEdges[index],
            transition.physicalArcVersions[index]
        };
        target[arc] += transition.coefficients[index];
    }
    removeZeroPhysicalCoefficients(target);
    result.physicalTerms = target.size();
    if (target.empty())
    {
        result.representable = true;
        result.reason = "EXACT_AFFINE_ONLY";
        return result;
    }

    for (const auto& [arc, coefficient] : target)
    {
        auto version = context.arcVersions.find(arc.edge);
        if (arc.edge.satellite.sys != pending.system ||
            context.basis.edges.find(arc.edge) == context.basis.edges.end() ||
            version == context.arcVersions.end() ||
            version->second != arc.version)
        {
            result.requiresBesd = true;
            result.reason = "REQUIRES_BESD_RETIRED_ARC";
            return result;
        }
    }

    map<ZhangPhysicalIntegerArc, ZhangExactInteger> reconstructed;
    set<ZhangPhysicalIntegerArc> representedChords;
    for (const auto& [column, key] : ambiguityResolution.ambmap)
    {
        if (column < 0 || column >= result.row.size() ||
            key.Sat.sys != pending.system ||
            static_cast<E_ObsCode>(key.num) != pending.observable)
        {
            continue;
        }
        ZhangGraphEdge edge{key.str, key.Sat};
        if (context.basis.edges.find(edge) == context.basis.edges.end() ||
            context.basis.isTreeEdge(edge.receiver, edge.satellite))
        {
            continue;
        }
        auto version = context.arcVersions.find(edge);
        if (version == context.arcVersions.end())
        {
            continue;
        }
        ZhangPhysicalIntegerArc chord{
            pending.observable, edge, version->second};
        auto coefficient = target.find(chord);
        if (coefficient == target.end() || coefficient->second == 0)
        {
            continue;
        }
        result.row(column) = coefficient->second.convert_to<double>();
        representedChords.insert(chord);
        result.chordTerms++;
        if (!addCurrentCycleToPhysicalRow(
                context,
                pending.observable,
                edge,
                coefficient->second,
                reconstructed))
        {
            result.reason = "CURRENT_CYCLE_RECONSTRUCTION_FAILED";
            return result;
        }
    }
    removeZeroPhysicalCoefficients(reconstructed);

    for (const auto& [arc, coefficient] : target)
    {
        if (!context.basis.isTreeEdge(
                arc.edge.receiver, arc.edge.satellite) &&
            representedChords.find(arc) == representedChords.end())
        {
            result.reason = "CURRENT_CHORD_STATE_MISSING";
            return result;
        }
    }
    if (reconstructed != target)
    {
        result.reason = "NOT_CURRENT_CYCLE_FUNCTIONAL";
        return result;
    }
    result.representable = true;
    result.reason = "EXACT_CURRENT_CYCLE_FUNCTIONAL";
    return result;
}

struct ZhangTemporalRelationCertificate
{
    TemporalCertificateKind kind =
        TemporalCertificateKind::SELF_GAUGE_SHIFT;
    GTime eventTime;
    E_Sys system = E_Sys::NONE;
    SatSys satellite;
    E_ObsCode firstCode = E_ObsCode::NONE;
    E_ObsCode secondCode = E_ObsCode::NONE;
    string route;
    long long wideLaneInteger = 0;
    long long firstInteger = 0;
    long long secondInteger = 0;
    double wideLanePerr = 1;
    double firstPerr = 1;
    double jointNis = std::numeric_limits<double>::quiet_NaN();
    double jointNisThreshold = std::numeric_limits<double>::quiet_NaN();
    bool exactIntegerEstimable = false;
    bool phaseSegmentCompatible = false;
	bool redundantSupportConfirmed = false;
    bool reliable = false;
    string oldIdentity;
    string newIdentity;
    string certificateId;
    vector<ZhangProductRelationAdmissionCandidate> admissionCandidates;
    map<E_ObsCode, map<SatSys, long long>> frontendShifts;
};

/** E27 temporal product-datum evaluator.  It consumes transitions discovered
 * after the preceding epoch's AR step and returns immutable certificates; it
 * cannot mutate the frontend or estimator. */
static vector<ZhangTemporalRelationCertificate>
evaluateTemporalProductRelations(
    Trace&                                        trace,
    const KFState&                                captureOwner,
    const KFState&                                state,
    const GinAR_mtx&                              ambiguityResolution,
    vector<ZhangPendingProductTransition>         transitions
)
{
    struct Evaluated
    {
        ZhangPendingProductTransition   pending;
        ZhangTemporalTransitionProjection projection;
		ZhangNamedProductIntegerSupport heldSupport;
        double mean = std::numeric_limits<double>::quiet_NaN();
        double variance = std::numeric_limits<double>::quiet_NaN();
        double fractional = std::numeric_limits<double>::quiet_NaN();
        double perr = 1;
        ZhangIntegerCandidateNis nis;
    };

    map<E_Sys, ZhangGraphIntegerContext> contexts;
    map<tuple<string, E_Sys, SatSys>, map<E_ObsCode, Evaluated>> paired;
    vector<ZhangTemporalRelationCertificate> certificates;
    int currentRepresentable = 0;
    int requiresBesd = 0;
    int rejected = 0;
    for (auto& pending : transitions)
    {
        auto context = contexts.find(pending.system);
        if (context == contexts.end())
        {
            ZhangGraphIntegerContext snapshot;
            if (zhangGraphIntegerContext(state, pending.system, snapshot))
            {
                context = contexts.emplace(
                    pending.system, std::move(snapshot)).first;
            }
        }

        Evaluated evaluated;
        evaluated.pending = pending;
		if (pending.transition.valid)
		{
			evaluated.heldSupport = zhangNamedProductIntegerSupport(
				captureOwner,
				pending.system,
				pending.observable,
				pending.transition.physicalEdges,
				pending.transition.physicalArcVersions,
				pending.transition.coefficients);
		}
        if (context == contexts.end())
        {
            evaluated.projection.row =
                VectorXd::Zero(ambiguityResolution.aflt.size());
            evaluated.projection.reason = "NO_CURRENT_GRAPH_CONTEXT";
        }
        else
        {
            evaluated.projection = projectTemporalProductTransition(
                pending, context->second, ambiguityResolution);
        }
        if (evaluated.projection.representable)
        {
            evaluated.mean = evaluated.projection.row.dot(
                ambiguityResolution.aflt) +
                evaluated.projection.affineCycles;
            evaluated.variance = (
                evaluated.projection.row.transpose() *
                ambiguityResolution.Paflt *
                evaluated.projection.row)(0, 0);
            evaluated.fractional = evaluated.mean - std::round(evaluated.mean);
            if (evaluated.variance > 0 &&
                std::isfinite(evaluated.variance))
            {
                evaluated.perr = round_perr(
                    evaluated.fractional, evaluated.variance);
                VectorXd innovation(1);
                innovation(0) = std::round(evaluated.mean) - evaluated.mean;
                MatrixXd covariance(1, 1);
                covariance(0, 0) = evaluated.variance;
                evaluated.nis = assessZhangIntegerCandidateNis(
                    innovation,
                    covariance,
                    acsConfig.zhangPppAr.held_constraint_nis_alpha);
            }
            currentRepresentable++;
        }
        else if (evaluated.projection.requiresBesd)
        {
            requiresBesd++;
        }
        else
        {
            rejected++;
        }

        trace << "\nZHANG_TEMPORAL_PRODUCT_TRANSITION time="
              << state.time.to_string(0)
              << " event_time=" << pending.eventTime.to_string(0)
              << " system=" << enum_to_string(pending.system)
              << " satellite=" << pending.satellite.id()
              << " observable=" << enum_to_string(pending.observable)
              << " status=" << evaluated.projection.reason
              << " physical_terms=" << evaluated.projection.physicalTerms
              << " chord_terms=" << evaluated.projection.chordTerms
              << " mean_cycles=" << evaluated.mean
              << " variance_cycles2=" << evaluated.variance
              << " fractional_cycles=" << evaluated.fractional
              << " perr=" << evaluated.perr
              << " nis=" << evaluated.nis.nis
              << " nis_threshold=" << evaluated.nis.threshold
			  << " held_contained=" << evaluated.heldSupport.contained
			  << " held_value=" << evaluated.heldSupport.value
			  << " held_rank=" << evaluated.heldSupport.heldRank
			  << " held_reason=" << evaluated.heldSupport.reason
              << " feedback=0";

        paired[{pending.eventTime.to_string(0), pending.system,
                pending.satellite}][pending.observable] =
            std::move(evaluated);
    }

    int completePairs = 0;
    int reliablePairs = 0;
	vector<pair<string, string>> besdSnapshotRows;
	map<tuple<string, E_Sys, SatSys>, int> besdPairOffsets;
	for (const auto& [key, signalRows] : paired)
	{
		const auto& [eventTime, system, satellite] = key;
		auto configured = acsConfig.zhangPppAr.baseline_observables.find(system);
		if (configured == acsConfig.zhangPppAr.baseline_observables.end() ||
			configured->second.size() != 2)
		{
			continue;
		}
		auto first = signalRows.find(configured->second[0]);
		auto second = signalRows.find(configured->second[1]);
		if (first == signalRows.end() || second == signalRows.end() ||
			first->second.pending.oldSnapshotIdentity.empty() ||
			first->second.pending.newSnapshotIdentity.empty() ||
			second->second.pending.oldSnapshotIdentity.empty() ||
			second->second.pending.newSnapshotIdentity.empty())
		{
			continue;
		}
		besdPairOffsets[key] = besdSnapshotRows.size();
		besdSnapshotRows.push_back({
			first->second.pending.oldSnapshotIdentity,
			first->second.pending.newSnapshotIdentity});
		besdSnapshotRows.push_back({
			second->second.pending.oldSnapshotIdentity,
			second->second.pending.newSnapshotIdentity});
	}
	VectorXd batchBesdMean;
	MatrixXd batchBesdCovariance;
	vector<bool> batchBesdAvailable;
	string batchBesdFailure = "NO_BESD_ROWS";
	const bool batchBesdValid = !besdSnapshotRows.empty() &&
		queryZhangTemporalProductBesdMarginal(
			captureOwner, besdSnapshotRows, batchBesdMean,
			batchBesdCovariance, batchBesdAvailable, batchBesdFailure);
    for (const auto& [key, signalRows] : paired)
    {
        const auto& [eventTime, system, satellite] = key;
        auto configured = acsConfig.zhangPppAr.baseline_observables.find(system);
        if (configured == acsConfig.zhangPppAr.baseline_observables.end() ||
            configured->second.size() != 2)
        {
            continue;
        }
        E_ObsCode firstCode = configured->second[0];
        E_ObsCode secondCode = configured->second[1];
        auto first = signalRows.find(firstCode);
        auto second = signalRows.find(secondCode);
        if (first == signalRows.end() || second == signalRows.end())
        {
            continue;
        }
        completePairs++;
        const auto& a = first->second;
        const auto& b = second->second;
		string estimationRoute;
		bool exactHeldRoute = false;
        Vector2d differenceMean;
        Matrix2d differenceCovariance;
        string besdFailure;
        if (a.heldSupport.contained && b.heldSupport.contained)
		{
			estimationRoute = "PERSISTENT_HELD_LATTICE";
			exactHeldRoute = true;
			differenceMean <<
				a.heldSupport.value +
					a.pending.transition.affineOffsetCycles.convert_to<double>(),
				b.heldSupport.value +
					b.pending.transition.affineOffsetCycles.convert_to<double>();
			differenceCovariance = Matrix2d::Zero();
		}
		else
		{
			// Recovery order is semantic, not an implementation accident:
			// held certificate -> exact current-cycle re-certification ->
			// targeted old/new BESD marginal -> suspend.  A tree event alone is
			// never sufficient reason to invoke the temporal path.
			if (a.projection.representable && b.projection.representable)
			{
				estimationRoute = "CURRENT_CYCLE_EXACT_RECERTIFICATION";
				differenceMean << a.mean, b.mean;
				differenceCovariance(0, 0) = a.variance;
				differenceCovariance(1, 1) = b.variance;
				const double cross = (
					a.projection.row.transpose() * ambiguityResolution.Paflt *
					b.projection.row)(0, 0);
				differenceCovariance(0, 1) = cross;
				differenceCovariance(1, 0) = cross;
			}
			else
			{
				auto offset = besdPairOffsets.find(key);
				if (batchBesdValid && offset != besdPairOffsets.end() &&
                    offset->second + 1 < batchBesdMean.size() &&
					offset->second + 1 <
						static_cast<int>(batchBesdAvailable.size()) &&
					batchBesdAvailable[offset->second] &&
					batchBesdAvailable[offset->second + 1])
				{
					estimationRoute = "TARGETED_RAW_FACTOR_BESD";
					differenceMean =
						batchBesdMean.segment<2>(offset->second);
                    differenceCovariance = batchBesdCovariance.block<2, 2>(
						offset->second, offset->second);
                }
				else
				{
					besdFailure = batchBesdValid
						? "BESD_SNAPSHOT_TARGET_MISSING"
						: batchBesdFailure;
				}
			}
		}
        if (estimationRoute.empty())
        {
            trace << "\nZHANG_TEMPORAL_PRODUCT_PAIR time="
                  << state.time.to_string(0)
                  << " event_time=" << eventTime
                  << " system=" << enum_to_string(system)
                  << " satellite=" << satellite.id()
                  << " first_status=" << a.projection.reason
                  << " second_status=" << b.projection.reason
                  << " besd_reason="
                  << (besdFailure.empty() ? "NOT_REQUESTED" : besdFailure)
                  << " status=UNAVAILABLE"
                  << " feedback=0";
            continue;
        }

        Vector2d wideLaneTransform;
        wideLaneTransform << 1, -1;
        double wideLaneMean = wideLaneTransform.dot(differenceMean);
        double wideLaneVariance = (wideLaneTransform.transpose() *
            differenceCovariance * wideLaneTransform)(0, 0);
        double firstWideLaneCovariance =
            differenceCovariance.row(0).dot(wideLaneTransform);
        double wideLaneInteger = std::round(wideLaneMean);
        double wideLaneFractional = wideLaneMean - wideLaneInteger;
        double wideLanePerr = 1;
        double conditionalFirstMean = differenceMean(0);
        double conditionalFirstVariance = differenceCovariance(0, 0);
        if (wideLaneVariance > 0 && std::isfinite(wideLaneVariance))
        {
            wideLanePerr = round_perr(
                wideLaneFractional, wideLaneVariance);
            conditionalFirstMean += firstWideLaneCovariance /
                wideLaneVariance * (wideLaneInteger - wideLaneMean);
            conditionalFirstVariance -= firstWideLaneCovariance *
                firstWideLaneCovariance / wideLaneVariance;
        }
		else if (wideLaneVariance == 0 &&
			std::abs(wideLaneFractional) <= 1e-10)
		{
			wideLanePerr = 0;
		}
        if (std::isfinite(conditionalFirstVariance) &&
            conditionalFirstVariance < 0 &&
            conditionalFirstVariance > -1e-10 *
                std::max(1.0, std::abs(differenceCovariance(0, 0))))
        {
            conditionalFirstVariance = 0;
        }
        double firstInteger = std::round(conditionalFirstMean);
        double conditionalFirstFractional =
            conditionalFirstMean - firstInteger;
        double conditionalFirstPerr =
            std::isfinite(conditionalFirstVariance) &&
            conditionalFirstVariance > 0
                ? round_perr(conditionalFirstFractional,
                    conditionalFirstVariance)
                : (conditionalFirstVariance == 0 &&
                   std::abs(conditionalFirstFractional) <= 1e-10 ? 0 : 1);

        Matrix2d jointTransform;
        jointTransform << 1, -1,
                          1,  0;
        VectorXd jointMean(2);
        jointMean << wideLaneMean, differenceMean(0);
        VectorXd jointInteger(2);
        jointInteger << wideLaneInteger, firstInteger;
        MatrixXd jointCovariance = jointTransform * differenceCovariance *
            jointTransform.transpose();
        ZhangIntegerCandidateNis jointNis = assessZhangIntegerCandidateNis(
            jointInteger - jointMean,
            jointCovariance,
            acsConfig.zhangPppAr.held_constraint_nis_alpha);
        const double maximumPerr =
            acsConfig.zhangPppAr.canonical_user_target_max_perr;
		const bool jointAccepted = exactHeldRoute
			? std::abs(wideLaneFractional) <= 1e-10 &&
			  std::abs(conditionalFirstFractional) <= 1e-10
			: jointNis.valid && jointNis.nis <= jointNis.threshold;
        const bool reliable = wideLanePerr <= maximumPerr &&
            conditionalFirstPerr <= maximumPerr && jointAccepted;
        reliablePairs += reliable;

        // Always return a certificate, including rejected evidence.  The
        // controller below is solely responsible for admission filtering.
        {
            const auto exactPhysicalTransition = []
            (const Evaluated& evaluated)
            {
                return evaluated.pending.transition.valid &&
                    !evaluated.pending.transition.physicalEdges.empty() &&
                    evaluated.pending.transition.physicalEdges.size() ==
                        evaluated.pending.transition.coefficients.size() &&
                    evaluated.pending.transition.physicalEdges.size() ==
                        evaluated.pending.transition.physicalArcVersions.size();
            };
            auto makeCandidate = [&]
            (
                const Evaluated& evaluated,
                const ZhangExactInteger& integerValue
            )
            {
                ZhangProductRelationAdmissionCandidate candidate;
                candidate.relationId = eventTime + "|" +
                    enum_to_string(system) + "|" + satellite.id() + "|" +
                    enum_to_string(evaluated.pending.observable) + "|" +
                    evaluated.pending.oldSnapshotIdentity + "|" +
                    evaluated.pending.newSnapshotIdentity;
                candidate.satellite = satellite.id();
                candidate.observable =
                    enum_to_string(evaluated.pending.observable);
                candidate.integerValue = integerValue -
                    evaluated.pending.transition.affineOffsetCycles;
                // A retired-arc relation cannot be represented in the current
                // cycle lattice by definition.  A complete immutable physical
                // transition evaluated from its old/new BESD snapshots is an
                // independent exact proof and must not be rejected merely for
                // lacking a current-state coordinate.
                candidate.exactIntegerEstimable =
                    exactPhysicalTransition(evaluated) &&
                    (exactHeldRoute || evaluated.projection.representable ||
                     estimationRoute == "TARGETED_RAW_FACTOR_BESD") &&
                    (estimationRoute != "TARGETED_RAW_FACTOR_BESD" ||
                     (!evaluated.pending.oldSnapshotIdentity.empty() &&
                      !evaluated.pending.newSnapshotIdentity.empty()));
                candidate.phaseSegmentCompatible =
                    !evaluated.pending.phaseSegmentChanged;
                candidate.scalarReliabilityPassed =
                    wideLanePerr <= maximumPerr &&
                    conditionalFirstPerr <= maximumPerr;
                candidate.jointNisPassed = jointAccepted;
                if (evaluated.pending.transition.valid &&
                    evaluated.pending.transition.physicalEdges.size() ==
                        evaluated.pending.transition.coefficients.size() &&
                    evaluated.pending.transition.physicalEdges.size() ==
                        evaluated.pending.transition.physicalArcVersions.size())
                {
                    for (size_t index = 0;
                         index < evaluated.pending.transition.coefficients.size();
                         index++)
                    {
                        const auto coefficient =
                            evaluated.pending.transition.coefficients[index];
                        if (coefficient == 0)
                        {
                            continue;
                        }
                        const auto& edge =
                            evaluated.pending.transition.physicalEdges[index];
                        const int version = evaluated.pending.transition
                            .physicalArcVersions[index];
                        const string column = edge.receiver + ">" +
                            edge.satellite.id() + "@v" +
                            std::to_string(version);
                        candidate.physicalCoefficients[column] += coefficient;
                    }
                }
                return candidate;
            };
            const ZhangExactInteger firstValue =
                static_cast<long long>(std::llround(firstInteger));
            const ZhangExactInteger secondValue =
                static_cast<long long>(std::llround(
                    firstInteger - wideLaneInteger));
            ZhangTemporalRelationCertificate certificate;
            certificate.eventTime = a.pending.eventTime;
            certificate.kind = TemporalCertificateKind::SELF_GAUGE_SHIFT;
            certificate.system = system;
            certificate.satellite = satellite;
            certificate.firstCode = firstCode;
            certificate.secondCode = secondCode;
            certificate.route = estimationRoute;
            certificate.wideLaneInteger = std::llround(wideLaneInteger);
            certificate.firstInteger = firstValue.convert_to<long long>();
            certificate.secondInteger = secondValue.convert_to<long long>();
            certificate.wideLanePerr = wideLanePerr;
            certificate.firstPerr = conditionalFirstPerr;
            certificate.jointNis = jointNis.nis;
            certificate.jointNisThreshold = jointNis.threshold;
            certificate.exactIntegerEstimable =
                exactPhysicalTransition(a) && exactPhysicalTransition(b);
            certificate.phaseSegmentCompatible =
                !a.pending.phaseSegmentChanged &&
                !b.pending.phaseSegmentChanged;
            certificate.reliable = reliable;
            certificate.oldIdentity = a.pending.oldSnapshotIdentity + "+" +
                b.pending.oldSnapshotIdentity;
            certificate.newIdentity = a.pending.newSnapshotIdentity + "+" +
                b.pending.newSnapshotIdentity;
            certificate.certificateId = eventTime + "|" +
                enum_to_string(system) + "|" + satellite.id() + "|" +
                certificate.oldIdentity + "|" + certificate.newIdentity;
            certificate.admissionCandidates.push_back(
                makeCandidate(a, firstValue));
            certificate.admissionCandidates.push_back(
                makeCandidate(b, secondValue));
            certificate.frontendShifts[firstCode][satellite] =
                certificate.firstInteger;
            certificate.frontendShifts[secondCode][satellite] =
                certificate.secondInteger;
            trace << "\nZHANG_TEMPORAL_RELATION_CERTIFICATE time="
                  << state.time.to_string(0)
                  << " certificate_id=" << certificate.certificateId
                  << " certificate_kind="
                  << zhangTemporalCertificateKindName(certificate.kind)
                  << " event_time=" << eventTime
                  << " system=" << enum_to_string(system)
                  << " satellite=" << satellite.id()
                  << " route=" << certificate.route
                  << " wl_integer=" << certificate.wideLaneInteger
                  << " first_integer=" << certificate.firstInteger
                  << " second_integer=" << certificate.secondInteger
                  << " wl_perr=" << certificate.wideLanePerr
                  << " first_perr=" << certificate.firstPerr
                  << " joint_nis=" << certificate.jointNis
                  << " joint_nis_threshold="
                  << certificate.jointNisThreshold
                  << " exact_integer_estimable="
                  << certificate.exactIntegerEstimable
                  << " phase_segment_compatible="
                  << certificate.phaseSegmentCompatible
                  << " reliable=" << certificate.reliable
                  << " feedback=0";
            certificates.push_back(std::move(certificate));
        }

        trace << "\nZHANG_TEMPORAL_PRODUCT_PAIR time="
              << state.time.to_string(0)
              << " event_time=" << eventTime
              << " system=" << enum_to_string(system)
              << " satellite=" << satellite.id()
              << " estimation_route=" << estimationRoute
              << " first_observable=" << enum_to_string(firstCode)
              << " second_observable=" << enum_to_string(secondCode)
              << " wl_mean_cycles=" << wideLaneMean
              << " wl_variance_cycles2=" << wideLaneVariance
              << " wl_fractional_cycles=" << wideLaneFractional
              << " wl_perr=" << wideLanePerr
              << " conditional_l1_mean_cycles=" << conditionalFirstMean
              << " conditional_l1_variance_cycles2="
              << conditionalFirstVariance
              << " conditional_l1_fractional_cycles="
              << conditionalFirstFractional
              << " conditional_l1_perr=" << conditionalFirstPerr
              << " joint_nis=" << jointNis.nis
              << " joint_nis_rank=" << jointNis.rank
              << " joint_nis_threshold=" << jointNis.threshold
              << " maximum_perr=" << maximumPerr
              << " exact_physical_first="
              << (a.pending.transition.valid &&
                  !a.pending.transition.physicalEdges.empty() &&
                  a.pending.transition.physicalEdges.size() ==
                      a.pending.transition.coefficients.size() &&
                  a.pending.transition.physicalEdges.size() ==
                      a.pending.transition.physicalArcVersions.size())
              << " exact_physical_second="
              << (b.pending.transition.valid &&
                  !b.pending.transition.physicalEdges.empty() &&
                  b.pending.transition.physicalEdges.size() ==
                      b.pending.transition.coefficients.size() &&
                  b.pending.transition.physicalEdges.size() ==
                      b.pending.transition.physicalArcVersions.size())
              << " status=" << (reliable ? "RELIABLE" : "REJECTED")
              << " fixed_rows=0 feedback=0";
    }

	// A component split can add the same non-integer real gauge to many
	// satellite product transitions.  Rounding each absolute transition then
	// confuses gamma with an integer r_s and must fail (the 00:16 event did
	// exactly this).  Eliminate the common mode first by forming direct
	// inter-satellite transition rows.  Only their integer differences enter
	// kappa; the existing HYBRID real-gauge GLS absorbs the remaining common
	// mode because the physical phase segments themselves are unchanged.
	struct ComponentMember
	{
		SatSys satellite;
		const Evaluated* first = nullptr;
		const Evaluated* second = nullptr;
	};
	map<tuple<string, E_Sys, string>, vector<ComponentMember>> componentGroups;
	for (const auto& [key, signalRows] : paired)
	{
		const auto& [eventTime, system, satellite] = key;
		auto configured = acsConfig.zhangPppAr.baseline_observables.find(system);
		if (configured == acsConfig.zhangPppAr.baseline_observables.end() ||
			configured->second.size() != 2)
		{
			continue;
		}
		auto first = signalRows.find(configured->second[0]);
		auto second = signalRows.find(configured->second[1]);
		if (first == signalRows.end() || second == signalRows.end() ||
			first->second.pending.eventCause != "COMPONENT_SPLIT" ||
			second->second.pending.eventCause != "COMPONENT_SPLIT" ||
			!first->second.projection.representable ||
			!second->second.projection.representable ||
			first->second.pending.phaseSegmentChanged ||
			second->second.pending.phaseSegmentChanged)
		{
			continue;
		}
		const auto firstStatus = zhangSatelliteDatumStatus(
			system, configured->second[0], satellite);
		const auto secondStatus = zhangSatelliteDatumStatus(
			system, configured->second[1], satellite);
		if (firstStatus.componentId.empty() ||
			secondStatus.componentId.empty())
		{
			continue;
		}
		const string component = firstStatus.componentId + "&" +
			secondStatus.componentId;
		componentGroups[{eventTime, system, component}].push_back({
			satellite, &first->second, &second->second});
	}
	int componentBridgeGroups = 0;
	int reliableComponentBridgeGroups = 0;
	for (auto& [groupKey, members] : componentGroups)
	{
		const auto& [eventTime, system, component] = groupKey;
		std::sort(members.begin(), members.end(), [](const auto& a, const auto& b)
			{ return a.satellite < b.satellite; });
		if (members.size() < 3)
		{
			continue;
		}
		componentBridgeGroups++;
		const ComponentMember& anchor = members.front();
		struct RelativeMember
		{
			const ComponentMember* member = nullptr;
			long long wideLane = 0;
			long long first = 0;
			long long second = 0;
			double wideLanePerr = 1;
			double firstPerr = 1;
		};
		vector<RelativeMember> relative;
		const double maximumPerr =
			acsConfig.zhangPppAr.canonical_user_target_max_perr;
		for (size_t index = 1; index < members.size(); index++)
		{
			const auto& member = members[index];
			MatrixXd rawRows(2, ambiguityResolution.aflt.size());
			rawRows.row(0) = member.first->projection.row -
				anchor.first->projection.row;
			rawRows.row(1) = member.second->projection.row -
				anchor.second->projection.row;
			Vector2d rawMean = rawRows * ambiguityResolution.aflt;
			rawMean(0) += member.first->projection.affineCycles -
				anchor.first->projection.affineCycles;
			rawMean(1) += member.second->projection.affineCycles -
				anchor.second->projection.affineCycles;
			Matrix2d rawCovariance = rawRows * ambiguityResolution.Paflt *
				rawRows.transpose();
			rawCovariance = 0.5 * (rawCovariance + rawCovariance.transpose());
			Vector2d wlMap;
			wlMap << 1, -1;
			const double wlMean = wlMap.dot(rawMean);
			const double wlVariance =
				(wlMap.transpose() * rawCovariance * wlMap)(0, 0);
			const long long wlInteger = std::llround(wlMean);
			const double wlFractional = wlMean - wlInteger;
			double wlPerr = wlVariance > 0 && std::isfinite(wlVariance)
				? round_perr(wlFractional, wlVariance)
				: (std::abs(wlFractional) <= 1e-10 ? 0 : 1);
			double firstMean = rawMean(0);
			double firstVariance = rawCovariance(0, 0);
			const double firstWlCovariance = rawCovariance.row(0).dot(wlMap);
			if (wlVariance > 0 && std::isfinite(wlVariance))
			{
				firstMean += firstWlCovariance / wlVariance *
					(wlInteger - wlMean);
				firstVariance -= firstWlCovariance * firstWlCovariance /
					wlVariance;
			}
			if (firstVariance < 0 && firstVariance > -1e-10 *
				std::max(1.0, std::abs(rawCovariance(0, 0))))
			{
				firstVariance = 0;
			}
			const long long firstInteger = std::llround(firstMean);
			const double firstFractional = firstMean - firstInteger;
			const double firstPerr = firstVariance > 0 &&
				std::isfinite(firstVariance)
				? round_perr(firstFractional, firstVariance)
				: (std::abs(firstFractional) <= 1e-10 ? 0 : 1);
			Matrix2d admissible;
			admissible << 1, -1, 1, 0;
			Vector2d transformedMean = admissible * rawMean;
			Vector2d transformedInteger;
			transformedInteger << wlInteger, firstInteger;
			const auto pairNis = assessZhangIntegerCandidateNis(
				transformedInteger - transformedMean,
				admissible * rawCovariance * admissible.transpose(),
				acsConfig.zhangPppAr.held_constraint_nis_alpha);
			const bool accepted = wlPerr <= maximumPerr &&
				firstPerr <= maximumPerr && pairNis.valid &&
				pairNis.nis <= pairNis.threshold;
			trace << "\nZHANG_TEMPORAL_COMPONENT_GAUGE_CANCELLATION time="
				  << state.time.to_string(0)
				  << " event_time=" << eventTime
				  << " system=" << enum_to_string(system)
				  << " component=" << component
				  << " anchor=" << anchor.satellite.id()
				  << " satellite=" << member.satellite.id()
				  << " wl_relative_mean_cycles=" << wlMean
				  << " wl_relative_variance_cycles2=" << wlVariance
				  << " wl_relative_perr=" << wlPerr
				  << " conditional_l1_relative_mean_cycles=" << firstMean
				  << " conditional_l1_relative_variance_cycles2="
				  << firstVariance
				  << " conditional_l1_relative_perr=" << firstPerr
				  << " joint_nis=" << pairNis.nis
				  << " joint_nis_threshold=" << pairNis.threshold
				  << " status=" << (accepted ? "RELIABLE" : "REJECTED")
				  << " common_real_gauge_removed=1 feedback=0";
			if (accepted)
			{
				relative.push_back({&member, wlInteger, firstInteger,
					firstInteger - wlInteger, wlPerr, firstPerr});
			}
		}
		// Three satellites provide two independent star rows plus one exact
		// redundant closure row.  Anything smaller remains quarantined.
		if (relative.size() < 2)
		{
			continue;
		}
		const int bridgeRank = static_cast<int>(relative.size());
		MatrixXd componentRows(2 * bridgeRank, ambiguityResolution.aflt.size());
		VectorXd componentOffsets = VectorXd::Zero(2 * bridgeRank);
		VectorXd componentIntegers = VectorXd::Zero(2 * bridgeRank);
		for (int row = 0; row < bridgeRank; row++)
		{
			const auto& member = *relative[row].member;
			componentRows.row(row) = member.first->projection.row -
				member.second->projection.row -
				anchor.first->projection.row + anchor.second->projection.row;
			componentRows.row(bridgeRank + row) =
				member.first->projection.row - anchor.first->projection.row;
			componentOffsets(row) =
				member.first->projection.affineCycles -
				member.second->projection.affineCycles -
				anchor.first->projection.affineCycles +
				anchor.second->projection.affineCycles;
			componentOffsets(bridgeRank + row) =
				member.first->projection.affineCycles -
				anchor.first->projection.affineCycles;
			componentIntegers(row) = relative[row].wideLane;
			componentIntegers(bridgeRank + row) = relative[row].first;
		}
		const VectorXd componentMean = componentRows *
			ambiguityResolution.aflt + componentOffsets;
		const MatrixXd componentCovariance = componentRows *
			ambiguityResolution.Paflt * componentRows.transpose();
		const auto componentNis = assessZhangIntegerCandidateNis(
			componentIntegers - componentMean, componentCovariance,
			acsConfig.zhangPppAr.held_constraint_nis_alpha);
		trace << "\nZHANG_TEMPORAL_COMPONENT_JOINT_NIS time="
			  << state.time.to_string(0)
			  << " event_time=" << eventTime
			  << " system=" << enum_to_string(system)
			  << " component=" << component
			  << " bridge_rank=" << bridgeRank
			  << " joint_integer_rank=" << 2 * bridgeRank
			  << " nis=" << componentNis.nis
			  << " threshold=" << componentNis.threshold
			  << " status=" << (componentNis.valid &&
				componentNis.nis <= componentNis.threshold
				? "ACCEPTED" : "REJECTED")
			  << " feedback=0";
		if (!componentNis.valid || componentNis.nis > componentNis.threshold)
		{
			continue;
		}
		auto addPhysicalDifference = [](
			ZhangProductRelationAdmissionCandidate& candidate,
			const Evaluated& subject,
			const Evaluated& reference)
		{
			auto add = [&](const Evaluated& value, const ZhangExactInteger& sign)
			{
				for (size_t term = 0;
					 term < value.pending.transition.coefficients.size(); term++)
				{
					const auto coefficient =
						value.pending.transition.coefficients[term] * sign;
					if (coefficient == 0) continue;
					const auto& edge =
						value.pending.transition.physicalEdges[term];
					const int version = value.pending.transition
						.physicalArcVersions[term];
					candidate.physicalCoefficients[
						edge.receiver + ">" + edge.satellite.id() + "@v" +
						std::to_string(version)] += coefficient;
				}
			};
			add(subject, 1);
			add(reference, -1);
			for (auto it = candidate.physicalCoefficients.begin();
				 it != candidate.physicalCoefficients.end();)
			{
				if (it->second == 0) it = candidate.physicalCoefficients.erase(it);
				else ++it;
			}
		};
		ZhangTemporalRelationCertificate certificate;
		certificate.kind = TemporalCertificateKind::INTER_SATELLITE_BRIDGE;
		certificate.eventTime = anchor.first->pending.eventTime;
		certificate.system = system;
		certificate.satellite = anchor.satellite;
		certificate.firstCode = anchor.first->pending.observable;
		certificate.secondCode = anchor.second->pending.observable;
		certificate.route = "COMPONENT_COMMON_REAL_GAUGE_CANCELLED";
		certificate.exactIntegerEstimable = true;
		certificate.phaseSegmentCompatible = true;
		certificate.redundantSupportConfirmed = true;
		certificate.reliable = true;
		certificate.jointNis = componentNis.nis;
		certificate.jointNisThreshold = componentNis.threshold;
		certificate.wideLanePerr = 0;
		certificate.firstPerr = 0;
		certificate.oldIdentity = eventTime + "|" + component + "|OLD";
		certificate.newIdentity = eventTime + "|" + component + "|NEW";
		certificate.certificateId = eventTime + "|" +
			enum_to_string(system) + "|" + component +
			"|COMPONENT_GAUGE_CANCELLED";
		certificate.frontendShifts[certificate.firstCode][anchor.satellite] = 0;
		certificate.frontendShifts[certificate.secondCode][anchor.satellite] = 0;
		auto makeCandidate = [&](const ComponentMember& subject,
			const ComponentMember& reference, E_ObsCode code,
			long long integer, const string& suffix)
		{
			const Evaluated& subjectRow = code == certificate.firstCode
				? *subject.first : *subject.second;
			const Evaluated& referenceRow = code == certificate.firstCode
				? *reference.first : *reference.second;
			ZhangProductRelationAdmissionCandidate candidate;
			candidate.certificateKind = certificate.kind;
			candidate.relationId = certificate.certificateId + "|" + suffix +
				"|" + enum_to_string(code);
			candidate.satellite = subject.satellite.id();
			candidate.observable = enum_to_string(code);
			addPhysicalDifference(candidate, subjectRow, referenceRow);
			candidate.integerValue = ZhangExactInteger(integer) -
				(subjectRow.pending.transition.affineOffsetCycles -
				 referenceRow.pending.transition.affineOffsetCycles);
			candidate.exactIntegerEstimable =
				!candidate.physicalCoefficients.empty();
			candidate.phaseSegmentCompatible = true;
			candidate.scalarReliabilityPassed = true;
			candidate.jointNisPassed = true;
			certificate.exactIntegerEstimable &=
				candidate.exactIntegerEstimable;
			return candidate;
		};
		for (const auto& member : relative)
		{
			certificate.wideLanePerr = std::max(
				certificate.wideLanePerr, member.wideLanePerr);
			certificate.firstPerr = std::max(
				certificate.firstPerr, member.firstPerr);
			certificate.frontendShifts[certificate.firstCode]
				[member.member->satellite] = member.first;
			certificate.frontendShifts[certificate.secondCode]
				[member.member->satellite] = member.second;
			certificate.admissionCandidates.push_back(makeCandidate(
				*member.member, anchor, certificate.firstCode,
				member.first, "STAR_" + member.member->satellite.id()));
			certificate.admissionCandidates.push_back(makeCandidate(
				*member.member, anchor, certificate.secondCode,
				member.second, "STAR_" + member.member->satellite.id()));
		}
		// Add one HNF-consistent triangle closure row per signal.  The admission
		// layer still independently verifies rank, exact values and redundancy.
		const auto& a = relative[0];
		const auto& b = relative[1];
		certificate.admissionCandidates.push_back(makeCandidate(
			*b.member, *a.member, certificate.firstCode,
			b.first - a.first, "REDUNDANT_TRIANGLE"));
		certificate.admissionCandidates.push_back(makeCandidate(
			*b.member, *a.member, certificate.secondCode,
			b.second - a.second, "REDUNDANT_TRIANGLE"));
		certificate.reliable &= certificate.exactIntegerEstimable;
		if (certificate.reliable)
		{
			reliableComponentBridgeGroups++;
			certificates.push_back(std::move(certificate));
		}
	}

    trace << "\nZHANG_TEMPORAL_PRODUCT_TRANSITION_SUMMARY time="
          << state.time.to_string(0)
          << " input_transitions=" << transitions.size()
          << " current_representable=" << currentRepresentable
          << " requires_besd=" << requiresBesd
          << " other_rejected=" << rejected
          << " complete_dual_frequency_pairs=" << completePairs
          << " reliable_pairs=" << reliablePairs
		  << " component_bridge_groups=" << componentBridgeGroups
		  << " reliable_component_bridge_groups="
		  << reliableComponentBridgeGroups
          << " feedback=0";
    return certificates;
}

/** The only temporal-certificate consumer allowed to update the persistent
 * frontend.  ProductRelationAdmission retains exact HNF/cycle/redundancy hard
 * gates; successful commits transport frontend alignment only. */
static void processTemporalProductRelationAdmissions(
    Trace& trace,
    const KFState& captureOwner,
    const KFState& state,
    const vector<ZhangTemporalRelationCertificate>& certificates)
{
    if (!acsConfig.zhangPppAr.product_relation_admission_shadow)
    {
        return;
    }
    map<tuple<string, E_Sys>,
        vector<ZhangProductRelationAdmissionCandidate>> batches;
    map<tuple<string, E_Sys>,
        map<E_ObsCode, map<SatSys, long long>>> shifts;
    const string runtimeId = zhangAmbresRuntimeId(captureOwner);
    for (const auto& certificate : certificates)
    {
        if (!certificate.reliable ||
            !certificate.exactIntegerEstimable ||
            !certificate.phaseSegmentCompatible)
        {
            continue;
        }
        if (!validZhangAmbresRuntimeId(runtimeId))
        {
            continue;
        }
        auto& confirmationOwner =
            zhangProductRelationAdmissionStateRegistry()[
                {runtimeId, certificate.system}];
        bool allConfirmed = true;
        int minimumConfirmations = std::numeric_limits<int>::max();
        for (const auto& candidate : certificate.admissionCandidates)
        {
            auto& confirmation = confirmationOwner
                .temporalCertificateConfirmations[candidate.relationId];
            const auto confirmed = zhangConfirmTemporalCertificate(
                confirmation,
                candidate.relationId,
                candidate.integerValue,
                static_cast<long int>(std::llround(state.time.bigTime)),
                certificate.oldIdentity + ">" + certificate.newIdentity +
                    "|" + certificate.route,
				certificate.redundantSupportConfirmed,
                std::max(1,
                    acsConfig.zhangPppAr.promotion_confirmation_epochs),
                acsConfig.zhangPppAr
                    .promotion_confirmation_max_gap_seconds,
                certificate.kind ==
                    TemporalCertificateKind::INTER_SATELLITE_BRIDGE);
            allConfirmed &= confirmed.accepted;
            minimumConfirmations = std::min(
                minimumConfirmations, confirmed.consistentEpochs);
        }
        trace << "\nZHANG_TEMPORAL_CERTIFICATE_CONFIRMATION time="
              << state.time.to_string(0)
              << " certificate_id=" << certificate.certificateId
              << " certificate_kind="
              << zhangTemporalCertificateKindName(certificate.kind)
              << " consistent_epochs="
              << (minimumConfirmations == std::numeric_limits<int>::max()
                    ? 0 : minimumConfirmations)
              << " required_epochs="
              << std::max(1,
                    acsConfig.zhangPppAr.promotion_confirmation_epochs)
              << " maximum_gap_seconds="
              << acsConfig.zhangPppAr
                    .promotion_confirmation_max_gap_seconds
              << " status=" << (allConfirmed ? "CONFIRMED" : "PENDING")
              << " feedback=0";
        if (!allConfirmed)
        {
            continue;
        }
        const auto key = make_tuple(
            certificate.eventTime.to_string(0), certificate.system);
        batches[key].insert(
            batches[key].end(),
            certificate.admissionCandidates.begin(),
            certificate.admissionCandidates.end());
        for (const auto& [code, satelliteShifts] :
             certificate.frontendShifts)
        for (const auto& [satellite, value] : satelliteShifts)
        {
            shifts[key][code][satellite] = value;
        }
    }

    for (const auto& [batchKey, candidates] : batches)
    {
        const auto& [eventTime, system] = batchKey;
        ZhangProductRelationAdmissionResult admission;
        size_t certifiedSatellites = 0;
        bool frontendCommitted = false;
        size_t frontendRestoredSatellites = 0;
        if (!validZhangAmbresRuntimeId(runtimeId))
        {
            admission.status = "REJECTED_INVALID_RUNTIME_ID";
        }
        else
        {
            auto& admissionState =
                zhangProductRelationAdmissionStateRegistry()[
                    {runtimeId, system}];
            const auto admissionStateBefore = admissionState;
            admission = ProductRelationAdmission::admit(
                admissionState, candidates, true);
            if (admission.committed)
            {
                const auto applied =
                    applyZhangCertifiedTemporalProductShiftBatch(
                        state.time, system, shifts[batchKey],
                        "PRODUCT_RELATION_ADMISSION");
                frontendCommitted = applied.accepted;
                frontendRestoredSatellites = applied.restoredSatellites;
                if (!frontendCommitted)
                {
                    admissionState = admissionStateBefore;
                    admission.committed = false;
                    admission.status = "ABORT_FRONTEND_ALIGNMENT_REJECTED";
                }
            }
            certifiedSatellites = admissionState.certifiedSatellites.size();
        }
        trace << "\nZHANG_PRODUCT_RELATION_ADMISSION time="
              << state.time.to_string(0)
              << " event_time=" << eventTime
              << " system=" << enum_to_string(system)
              << " certificate_count=" << candidates.size() / 2
              << " candidate_rows=" << admission.candidateRows
              << " fresh_rows=" << admission.freshRows
              << " duplicate_rows=" << admission.duplicateRows
              << " candidate_exact_rank=" << admission.candidateExactRank
              << " candidate_redundant_rows="
              << admission.candidateRedundantRows
              << " persistent_rank_before=" << admission.persistentRankBefore
              << " persistent_rank_after=" << admission.persistentRankAfter
              << " certified_satellites=" << certifiedSatellites
              << " candidate_cycle_closure_zero="
              << admission.candidateCycleClosureConsistent
              << " persistent_cycle_closure_zero="
              << admission.persistentCycleClosureConsistent
              << " redundancy_check_passed="
              << admission.redundancyCheckPassed
              << " status=" << admission.status
              << " certified_for_product=" << admission.committed
              << " frontend_committed=" << frontendCommitted
              << " frontend_restored_satellites="
              << frontendRestoredSatellites
              << " estimator_feedback=0 feedback=SHADOW_NONE";
    }
}

static int rankAwareGnssAr(
    Trace&           trace,
    GinAR_mtx&       search,
    const GinAR_opt& options,
    GTime            time,
    const string&    label,
    bool             enforceCandidateNis = false
)
{
    const int originalSize = search.aflt.size();
    auto selected = positiveVarianceTargetSubset(search.Paflt);
    if (selected.empty())
    {
        search.Ztrs.resize(0, originalSize);
        search.zfix.resize(0);
        trace << "\nZHANG_INTEGER_SEARCH_RANK time=" << time.to_string(0)
              << " label=" << label
              << " original_dimension=" << originalSize
              << " stochastic_rank=0 status=SKIPPED_DETERMINISTIC";
        return 0;
    }

    MatrixXd selection = MatrixXd::Zero(selected.size(), originalSize);
    for (int row = 0; row < static_cast<int>(selected.size()); row++)
    {
        selection(row, selected[row]) = 1;
    }
    if (static_cast<int>(selected.size()) < originalSize)
    {
        search.aflt = selection * search.aflt;
        search.Paflt = selection * search.Paflt * selection.transpose();
    }
    GinAR_opt searchOptions = options;
    if (searchOptions.lambda_candidate_row_ablation != "NONE")
    {
        const set<int> originalTargets(
            options.lambda_candidate_ablation_target_columns.begin(),
            options.lambda_candidate_ablation_target_columns.end()
        );
        searchOptions.lambda_candidate_ablation_target_columns.clear();
        for (int reduced = 0;
             reduced < static_cast<int>(selected.size());
             reduced++)
        {
            if (originalTargets.count(selected[reduced]) > 0)
            {
                searchOptions.lambda_candidate_ablation_target_columns
                    .push_back(reduced);
            }
        }
    }
    if (enforceCandidateNis && options.mode == E_ARmode::LAMBDA)
    {
        searchOptions.lambda_candidate_nis_alpha =
            acsConfig.zhangPppAr.held_constraint_nis_alpha;
    }
    int fixed = GNSS_AR(trace, search, searchOptions);
    if (search.lambda_candidate_tested_rows.cols() ==
            static_cast<int>(selected.size()) &&
        static_cast<int>(selected.size()) < originalSize)
    {
        search.lambda_candidate_tested_rows =
            search.lambda_candidate_tested_rows * selection;
    }
    if (searchOptions.lambda_candidate_row_ablation != "NONE")
    {
        trace << "\nZHANG_L1_CANDIDATE_ROW_ABLATION time="
              << time.to_string(0)
              << " label=" << label
              << " mode="
              << searchOptions.lambda_candidate_row_ablation
              << " physical_target_columns="
              << searchOptions.lambda_candidate_ablation_target_columns.size()
              << " input_integer_rows="
              << search.lambda_ablation_input_rows
              << " support_integer_rows="
              << search.lambda_ablation_support_rows
              << " removed_integer_rows="
              << search.lambda_ablation_removed_rows
              << " retained_integer_rows="
              << search.lambda_ablation_retained_rows
              << " target_mean_conditional_sigma_cycles="
              << search.lambda_ablation_target_mean_sigma
              << " removed_mean_conditional_sigma_cycles="
              << search.lambda_ablation_removed_mean_sigma
              << " maximum_log_variance_mismatch="
              << search.lambda_ablation_max_log_var_mismatch
              << " seed="
              << searchOptions.lambda_candidate_ablation_seed
              << " status=" << search.lambda_ablation_status
              << " scope=L1_INTEGER_ROWS_ONLY";
    }
    const int initialFixed = search.lambda_initial_fix_count > 0
        ? search.lambda_initial_fix_count
        : fixed;
    ZhangIntegerCandidateNis candidateNis =
        assessZhangIntegerCandidateNis(
            search,
            acsConfig.zhangPppAr.held_constraint_nis_alpha
        );
    const bool candidateAccepted = fixed > 0 && candidateNis.valid &&
        candidateNis.nis <= candidateNis.threshold;
    if (enforceCandidateNis && !candidateAccepted)
    {
        fixed = 0;
        search.Ztrs.resize(0, search.aflt.size());
        search.zfix.resize(0);
    }
    if (enforceCandidateNis)
    {
        trace << "\nZHANG_INTEGER_CANDIDATE_NIS_PAR time="
              << time.to_string(0)
              << " label=" << label
              << " original_fixed=" << initialFixed
              << " retained_fixed=" << fixed
              << " removed=" << std::max(0, initialFixed - fixed)
              << " nis=" << candidateNis.nis
              << " threshold=" << candidateNis.threshold
              << " rank=" << candidateNis.rank
              << " lambda_search_nis=" << search.lambda_candidate_nis
              << " lambda_search_threshold="
              << search.lambda_candidate_nis_threshold
              << " tested_fixed="
              << search.lambda_candidate_tested_fix_count
              << " rms_innovation_cycles="
              << search.lambda_candidate_rms_innovation
              << " max_innovation_cycles="
              << search.lambda_candidate_max_innovation
              << " min_sigma_cycles="
              << search.lambda_candidate_min_sigma
              << " max_sigma_cycles="
              << search.lambda_candidate_max_sigma
              << " max_marginal_nis="
              << search.lambda_candidate_max_marginal_nis
              << " status=" << (initialFixed == 0
                    ? "SKIPPED"
                    : (candidateAccepted ? "ACCEPTED" : "REJECTED"))
              << " ordering=LAMBDA_NESTED_SUFFIX"
              << " decomposition=REUSED";
        if (search.lambda_dominant_whitened_mode >= 0)
        {
            trace << "\nZHANG_INTEGER_WHITENED_MODE time="
                  << time.to_string(0)
                  << " label=" << label
                  << " tested_fixed="
                  << search.lambda_candidate_tested_fix_count
                  << " effective_rank="
                  << search.lambda_whitened_effective_rank
                  << " total_nis=" << search.lambda_candidate_nis
                  << " dominant_mode="
                  << search.lambda_dominant_whitened_mode
                  << " dominant_whitened_residual="
                  << search.lambda_dominant_whitened_residual
                  << " dominant_nis="
                  << search.lambda_dominant_whitened_nis
                  << " dominant_share="
                  << search.lambda_dominant_whitened_share
                  << " second_share="
                  << search.lambda_second_whitened_share
                  << " covariance_condition_number="
                  << search.lambda_whitened_condition_number
                  << " nis_closure="
                  << search.lambda_whitened_nis_closure;

            const VectorXd& loading =
                search.lambda_dominant_original_loading;
            if (loading.size() == static_cast<int>(selected.size()) &&
                !search.ambmap.empty())
            {
                vector<pair<double, int>> orderedLoading;
                orderedLoading.reserve(loading.size());
                for (int column = 0; column < loading.size(); column++)
                {
                    orderedLoading.push_back({
                        std::abs(loading(column)),
                        column
                    });
                }
                std::sort(
                    orderedLoading.begin(),
                    orderedLoading.end(),
                    [](const auto& left, const auto& right)
                    {
                        return left.first > right.first;
                    }
                );
                const double maximumLoading = orderedLoading.empty()
                    ? 0
                    : orderedLoading.front().first;
                const int outputTerms = std::min(
                    8,
                    static_cast<int>(orderedLoading.size())
                );
                for (int term = 0; term < outputTerms; term++)
                {
                    const int selectedColumn =
                        orderedLoading[term].second;
                    const int originalColumn = selected[selectedColumn];
                    auto keyIt = search.ambmap.find(originalColumn);
                    if (keyIt == search.ambmap.end())
                    {
                        continue;
                    }
                    const KFKey& key = keyIt->second;
                    trace << "\nZHANG_INTEGER_WHITENED_MODE_TERM time="
                          << time.to_string(0)
                          << " label=" << label
                          << " rank=" << (term + 1)
                          << " selected_column=" << selectedColumn
                          << " original_column=" << originalColumn
                          << " standardized_loading="
                          << loading(selectedColumn)
                          << " normalized_loading="
                          << (maximumLoading > 0
                                ? loading(selectedColumn) / maximumLoading
                                : 0)
                          << " receiver=" << key.str
                          << " satellite=" << key.Sat.id()
                          << " observable=" << key.code();
                }
            }
        }
    }
    if (fixed > 0 && static_cast<int>(selected.size()) < originalSize)
    {
        search.Ztrs = search.Ztrs * selection;
    }
    trace << "\nZHANG_INTEGER_SEARCH_RANK time=" << time.to_string(0)
          << " label=" << label
              << " original_dimension=" << originalSize
              << " stochastic_rank=" << selected.size()
              << " fixed=" << fixed
              << " candidate_nis=" << candidateNis.nis
              << " candidate_nis_threshold=" << candidateNis.threshold
              << " candidate_nis_rank=" << candidateNis.rank;
    return fixed;
}

/** Retain a conservative, jointly NIS-compatible subset of named integer
 * targets.  ROUND supplies identity rows in the named G_sat target space.
 * Candidate rows are ordered by marginal normalized innovation, then admitted
 * only if the growing joint set passes a Bonferroni-tightened chi-square gate.
 * This preserves explicit satellite relations while avoiding all-or-nothing
 * rejection of an otherwise useful PAR subset.
 */
static int retainNisCompatibleNamedRows(
    Trace&     trace,
    GinAR_mtx& fixed,
    GTime      time,
    const string& label,
    vector<int>* selectedRows = nullptr,
    bool         bonferroniAdmission = true
)
{
    const int candidates = fixed.zfix.size();
    if (candidates == 0 || fixed.Ztrs.rows() != candidates)
    {
        return 0;
    }

    VectorXd innovation = fixed.zfix - fixed.Ztrs * fixed.aflt;
    MatrixXd covariance = fixed.Ztrs * fixed.Paflt * fixed.Ztrs.transpose();
    covariance = 0.5 * (covariance + covariance.transpose());

    vector<pair<double, int>> ordered;
    ordered.reserve(candidates);
    double covarianceScale = std::max(
        1.0,
        covariance.diagonal().cwiseAbs().maxCoeff()
    );
    double varianceTolerance = 1e-12 * covarianceScale;
    for (int row = 0; row < candidates; row++)
    {
        double variance = covariance(row, row);
        if (!std::isfinite(variance) || variance <= varianceTolerance ||
            !std::isfinite(innovation(row)))
        {
            continue;
        }
        ordered.push_back({innovation(row) * innovation(row) / variance, row});
    }
    std::sort(
        ordered.begin(), ordered.end(),
        [](const auto& left, const auto& right)
        {
            if (left.first != right.first)
            {
                return left.first < right.first;
            }
            return left.second < right.second;
        }
    );

    vector<int> selected;
    const double familyAlpha =
        acsConfig.zhangPppAr.held_constraint_nis_alpha;
    const double admissionAlpha = bonferroniAdmission
        ? familyAlpha / std::max(1, candidates)
        : familyAlpha;
    for (const auto& [marginalNis, candidate] : ordered)
    {
        vector<int> trial = selected;
        trial.push_back(candidate);
        VectorXd trialInnovation = innovation(trial);
        MatrixXd trialCovariance = covariance(trial, trial);
        Eigen::SelfAdjointEigenSolver<MatrixXd> eigenSolver(trialCovariance);
        if (eigenSolver.info() != Eigen::Success ||
            !eigenSolver.eigenvalues().allFinite())
        {
            continue;
        }

        double largestEigenvalue = eigenSolver.eigenvalues().maxCoeff();
        double rankTolerance = std::max(1e-14, 1e-12 * largestEigenvalue);
        VectorXd coordinates =
            eigenSolver.eigenvectors().transpose() * trialInnovation;
        VectorXd inverseEigenvalues = VectorXd::Zero(trial.size());
        int effectiveRank = 0;
        double maximumNullInnovation = 0;
        for (int index = 0; index < static_cast<int>(trial.size()); index++)
        {
            if (eigenSolver.eigenvalues()(index) > rankTolerance)
            {
                inverseEigenvalues(index) =
                    1.0 / eigenSolver.eigenvalues()(index);
                effectiveRank++;
            }
            else
            {
                maximumNullInnovation = std::max(
                    maximumNullInnovation, std::abs(coordinates(index))
                );
            }
        }
        if (effectiveRank == 0 || maximumNullInnovation > 1e-7)
        {
            continue;
        }

        double nis = coordinates.dot(
            inverseEigenvalues.asDiagonal() * coordinates
        );
        boost::math::chi_squared distribution(effectiveRank);
        double threshold = quantile(complement(distribution, admissionAlpha));
        if (std::isfinite(nis) && nis <= threshold)
        {
            selected.push_back(candidate);
        }
        else
        {
            trace << "\nZHANG_NAMED_TARGET_PAR_REJECT time="
                  << time.to_string(0)
                  << " label=" << label
                  << " candidate_row=" << candidate
                  << " marginal_nis=" << marginalNis
                  << " trial_rank=" << effectiveRank
                  << " joint_nis=" << nis
                  << " threshold=" << threshold;
        }
    }

    MatrixXd retainedRows = MatrixXd::Zero(
        selected.size(), fixed.Ztrs.cols()
    );
    VectorXd retainedValues = VectorXd::Zero(selected.size());
    for (int row = 0; row < static_cast<int>(selected.size()); row++)
    {
        retainedRows.row(row) = fixed.Ztrs.row(selected[row]);
        retainedValues(row) = fixed.zfix(selected[row]);
    }
    fixed.Ztrs = std::move(retainedRows);
    fixed.zfix = std::move(retainedValues);
    if (selectedRows)
    {
        *selectedRows = selected;
    }
    trace << "\nZHANG_NAMED_TARGET_PAR time=" << time.to_string(0)
          << " label=" << label
          << " candidates=" << candidates
          << " selected=" << selected.size()
          << " rejected=" << candidates - selected.size()
          << " family_alpha=" << familyAlpha
          << " admission_alpha=" << admissionAlpha;
    return selected.size();
}

/** E26/E27 user-domain PAR.
 *
 * The integer objects are named between-satellite user ambiguity functions,
 * not the server's tree-dependent G*k coordinate correction.  Applying the
 * Hou product removes the satellite fractional phase term; differencing the
 * user ambiguities against one common dual-frequency reference then removes
 * the remaining receiver phase datum.  WL is resolved first and L1 is tested
 * only in the WL-conditioned state.  In shadow mode all conditioning is done
 * on a private state copy.
 */
static int resolveCanonicalUserSdWideLaneL1(
    Trace&           trace,
    KFState&         kfState,
    GinAR_mtx&       ambiguityResolution,
    const GinAR_opt& options,
    GTime            time
)
{
	const bool ifAcceptance =
		acsConfig.zhangPppAr.integer_strategy == "CANONICAL_USER_IF_WL_L1";
	const string tracePrefix = ifAcceptance
		? "ZHANG_E27_USER_" : "ZHANG_E26_USER_";
	const bool shadowOnly =
		!acsConfig.zhangPppAr.canonical_user_target_feedback;
	KFState shadowState;
	KFState* conditioningState = &kfState;
	if (shadowOnly)
	{
		shadowState = kfState;
		bindZhangAmbresEphemeralBranch(
			shadowState, kfState, "canonical-user-shadow");
		conditioningState = &shadowState;
	}

	vector<VectorXd> fixedRows;
	vector<double> fixedValues;
	int totalFixed = 0;
	int groupsEvaluated = 0;
	int namedWideLane = 0;
	int namedFirst = 0;
	double maximumPerr = 0;
	const string userRuntimeId = zhangAmbresRuntimeId(kfState);

	auto recoverNamedTargets = [](const GinAR_mtx& fixed,
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
				const long long value = std::llround(fixed.Ztrs(row, column));
				if (std::abs(fixed.Ztrs(row, column) - value) > 1e-8)
				{
					exact = false;
					break;
				}
				exactRow[column] = value;
			}
			const long long value = row < fixed.zfix.size()
				? std::llround(fixed.zfix(row)) : 0;
			if (!exact || std::abs(fixed.zfix(row) - value) > 1e-8)
			{
				exact = false;
				break;
			}
			exactRows.push_back(std::move(exactRow));
			exactValues.push_back(value);
		}
		if (!exact)
		{
			return std::map<std::size_t, ZhangExactInteger>{};
		}
		return ProductConstraintPromotion::recoverNamedTargets(
			exactRows, exactValues, namedTargetCount);
	};

	auto applyAndCapture = [&](const MatrixXd& rows,
							   const VectorXd& values,
							   const string& stageName)
	{
		if (rows.rows() == 0)
		{
			return true;
		}
		GinAR_mtx stage;
		stage.ambmap = ambiguityResolution.ambmap;
		stage.Ztrs = rows;
		stage.zfix = values;
		applyUCAmbiguities(trace, *conditioningState, stage, stageName);
		if (zhangTransactionalConditioningFailed)
		{
			trace << "\n" << tracePrefix << "TRANSACTION time=" << time.to_string(0)
				  << " stage=" << stageName
				  << " status=ROLLED_BACK reason="
				  << zhangTransactionalConditioningReason
				  << " feedback=" << !shadowOnly;
			if (shadowOnly)
			{
				zhangTransactionalConditioningFailed = false;
				zhangTransactionalConditioningReason.clear();
			}
			return false;
		}
		for (int row = 0; row < rows.rows(); row++)
		{
			fixedRows.push_back(rows.row(row).transpose());
			fixedValues.push_back(values(row));
		}
		totalFixed += rows.rows();
		return true;
	};

	auto refreshAmbiguities = [&]()
	{
		vector<int> indices;
		for (int column = 0;
			 column < static_cast<int>(ambiguityResolution.ambmap.size());
			 column++)
		{
			indices.push_back(conditioningState->kfIndexMap.at(
				ambiguityResolution.ambmap.at(column)));
		}
		ambiguityResolution.aflt = conditioningState->x(indices);
		ambiguityResolution.Paflt = conditioningState->P(indices, indices);
	};

	using SignalKey = tuple<string, E_Sys, int>;
	map<SignalKey, map<SatSys, int>> columns;
	for (const auto& [column, key] : ambiguityResolution.ambmap)
	{
		columns[{key.str, key.Sat.sys, key.num}][key.Sat] = column;
	}

	GinAR_opt namedOptions = options;
	namedOptions.mode = E_ARmode::ROUND;
	for (const auto& [system, observables] :
		 acsConfig.zhangPppAr.baseline_observables)
	{
		if (observables.size() != 2)
		{
			continue;
		}
		const E_ObsCode firstCode = observables[0];
		const E_ObsCode secondCode = observables[1];
		set<string> receivers;
		for (const auto& [key, ignored] : columns)
		{
			if (get<1>(key) == system)
			{
				receivers.insert(get<0>(key));
			}
		}
		for (const string& receiver : receivers)
		{
			const int firstCoordinate = ifAcceptance
				? zhangPppArUserPhaseCoordinateNumber(system, firstCode)
				: static_cast<int>(firstCode);
			const int secondCoordinate = ifAcceptance
				? firstCoordinate : static_cast<int>(secondCode);
			auto first = columns.find({receiver, system, firstCoordinate});
			auto second = columns.find({receiver, system, secondCoordinate});
			if (first == columns.end() || second == columns.end())
			{
				continue;
			}
			vector<SatSys> common;
			for (const auto& [satellite, ignored] : first->second)
			{
				if (second->second.count(satellite))
				{
					common.push_back(satellite);
				}
			}
			if (common.empty() || (!ifAcceptance && common.size() < 2))
			{
				continue;
			}

			std::optional<ZhangIfWideLaneEstimate> ifWideLaneEstimate;
			std::optional<MatrixXd> ifWideLaneCrossCovariance;
			ZhangIfUserCoefficients ifUserCoefficients;
			double conditionedWideLaneCoefficient = 0;
			SatSys persistentIfReference;
			if (ifAcceptance)
			{
				const SatSys firstUserReference = zhangPppArUserReference(
					kfState, receiver, system, firstCode);
				const SatSys secondUserReference = zhangPppArUserReference(
					kfState, receiver, system, secondCode);
				if (firstUserReference.prn <= 0 ||
					firstUserReference != secondUserReference)
				{
					trace << "\n" << tracePrefix << "WL_FACTOR time="
						  << time.to_string(0)
						  << " receiver=" << receiver
						  << " system=" << enum_to_string(system)
						  << " valid=0 reason=REFERENCE_CONFLICT"
						  << " first="
						  << (firstUserReference.prn > 0
							  ? firstUserReference.id() : "NONE")
						  << " second="
						  << (secondUserReference.prn > 0
							  ? secondUserReference.id() : "NONE");
					continue;
				}
				persistentIfReference = firstUserReference;
				if (std::find(common.begin(), common.end(), persistentIfReference) ==
					common.end())
				{
					common.push_back(persistentIfReference);
					std::sort(common.begin(), common.end());
				}
				Receiver* receiverPointer = ambiguityResolution.ambmap.at(
					first->second.begin()->second).rec_ptr;
				if (!receiverPointer)
				{
					trace << "\n" << tracePrefix << "WL_FACTOR time="
						  << time.to_string(0)
						  << " receiver=" << receiver
						  << " system=" << enum_to_string(system)
						  << " valid=0 reason=RECEIVER_POINTER_MISSING";
					continue;
				}
				const double lambdaFirst = zhangE27Wavelength(system, firstCode);
				const double lambdaSecond = zhangE27Wavelength(system, secondCode);
				ifUserCoefficients = zhangIfUserCoefficients(
					lambdaFirst, lambdaSecond);
				const double wideLaneWavelength = lambdaFirst * lambdaSecond /
					(lambdaSecond - lambdaFirst);
				if (!ifUserCoefficients.valid || !(wideLaneWavelength > 0))
				{
					continue;
				}
				conditionedWideLaneCoefficient =
					ifUserCoefficients.beta * lambdaSecond /
					ifUserCoefficients.narrowLaneWavelength;
				vector<SatSys> usable;
				vector<double> measurements;
				vector<vector<string>> rawNoiseKeys;
				vector<VectorXd> rawNoiseCoefficients;
				vector<VectorXd> rawNoiseVariances;
				const string runtimeId = zhangAmbresRuntimeId(kfState);
				if (!validZhangAmbresRuntimeId(runtimeId))
				{
					trace << "\n" << tracePrefix << "WL_FACTOR time="
						  << time.to_string(0)
						  << " receiver=" << receiver
						  << " system=" << enum_to_string(system)
						  << " valid=0 reason=CHECKPOINT_RUNTIME_ID_UNBOUND";
					continue;
				}
				ZhangE27WideLaneKey runtimeKey{
					runtimeId, receiver, system,
					acsConfig.zhangPppAr.product_solution};
				auto& runtime = zhangE27WideLaneRuntimes[runtimeKey];
				for (const SatSys& satellite : common)
				{
					const GObs* observation = nullptr;
					for (const auto& candidate : only<GObs>(receiverPointer->obsList))
					{
						if (candidate.Sat == satellite)
						{
							observation = &candidate;
							break;
						}
					}
					if (!observation)
					{
						continue;
					}
					const Sig* firstSignal = nullptr;
					const Sig* secondSignal = nullptr;
					for (const auto& [frequency, signal] : observation->sigs)
					{
						if (signal.code == firstCode)
						{
							firstSignal = &signal;
						}
						if (signal.code == secondCode)
						{
							secondSignal = &signal;
						}
					}
					if (!firstSignal || !secondSignal ||
						firstSignal->P == 0 || secondSignal->P == 0 ||
						firstSignal->L == 0 || secondSignal->L == 0)
					{
						continue;
					}
					ZhangInternalProduct firstProduct;
					ZhangInternalProduct secondProduct;
					if (!queryZhangInternalProduct(
							time, satellite, firstCode, firstProduct) ||
						!queryZhangInternalProduct(
							time, satellite, secondCode, secondProduct))
					{
						continue;
					}
					const double firstPhase =
						lambdaFirst * firstSignal->L + firstProduct.correction_m;
					const double secondPhase =
						lambdaSecond * secondSignal->L + secondProduct.correction_m;
					const double firstCodeMeasurement =
						firstSignal->P + firstProduct.clock_m;
					const double secondCodeMeasurement =
						secondSignal->P + secondProduct.clock_m;
					const S_LC combination = getLC(
						firstPhase, secondPhase,
						firstCodeMeasurement, secondCodeMeasurement,
						lambdaFirst, lambdaSecond, nullptr, nullptr);
					if (!combination.valid)
					{
						continue;
					}
					vector<string> stampedNoiseKeys;
					VectorXd noiseCoefficients;
					VectorXd noiseVariances;
					if (!queryZhangE27WideLaneRawNoiseFactors(
							kfState, time, receiver, system, satellite,
							stampedNoiseKeys, noiseCoefficients,
							noiseVariances))
					{
						trace << "\n" << tracePrefix << "WL_FACTOR time="
							  << time.to_string(0)
							  << " receiver=" << receiver
							  << " satellite=" << satellite.id()
							  << " valid=0 reason=RAW_NOISE_FACTOR_UNAVAILABLE";
						continue;
					}
					const double rawVariance = (noiseCoefficients.array().square() *
						noiseVariances.array()).sum();
					if (!(rawVariance > 0) || !std::isfinite(rawVariance))
					{
						continue;
					}
					bool arcChanged = false;
					auto last = runtime.lastValid.find(satellite);
					if (runtime.arcVersion[satellite] == 0)
					{
						runtime.arcVersion[satellite] = 1;
					}
					else if (last != runtime.lastValid.end() &&
						 (time - last->second).to_double() > 90)
					{
						arcChanged = true;
					}
					const auto datum = std::make_tuple(
						firstProduct.discontinuity_counter,
						firstProduct.datum_version,
						secondProduct.discontinuity_counter,
						secondProduct.datum_version);
					auto oldDatum = runtime.productDatum.find(satellite);
					if (oldDatum != runtime.productDatum.end() &&
						oldDatum->second != datum)
					{
						arcChanged = true;
					}
					if (arcChanged)
					{
						runtime.arcVersion[satellite]++;
						trace << "\n" << tracePrefix << "ARC time="
							  << time.to_string(0)
							  << " receiver=" << receiver
							  << " satellite=" << satellite.id()
							  << " version=" << runtime.arcVersion[satellite]
							  << " action=RESET_PHYSICAL_ARC";
					}
					runtime.lastValid[satellite] = time;
					runtime.productDatum[satellite] = datum;
					runtime.accumulator.setArcVersion(
						satellite.prn, runtime.arcVersion[satellite]);
					runtime.accumulatorArcVersions[satellite.prn] =
						runtime.arcVersion[satellite];
					usable.push_back(satellite);
					measurements.push_back(combination.MW_c);
					rawNoiseKeys.push_back(std::move(stampedNoiseKeys));
					rawNoiseCoefficients.push_back(std::move(noiseCoefficients));
					rawNoiseVariances.push_back(std::move(noiseVariances));
				}
				if (usable.size() < 2)
				{
					trace << "\n" << tracePrefix << "WL_FACTOR time="
						  << time.to_string(0)
						  << " receiver=" << receiver
						  << " system=" << enum_to_string(system)
						  << " valid=0 reason=INSUFFICIENT_CORRECTED_MW"
						  << " satellites=" << usable.size();
					continue;
				}
				VectorXd mw = VectorXd::Zero(usable.size());
				map<string, int> noiseIndex;
				map<string, double> noiseVarianceByKey;
				for (int row = 0; row < static_cast<int>(usable.size()); row++)
				for (int column = 0;
					 column < static_cast<int>(rawNoiseKeys[row].size()); column++)
				{
					const string& key = rawNoiseKeys[row][column];
					if (noiseIndex.count(key) == 0)
					{
						noiseIndex[key] = noiseIndex.size();
					}
					const double variance = rawNoiseVariances[row](column);
					auto old = noiseVarianceByKey.find(key);
					if (old != noiseVarianceByKey.end() &&
						std::abs(old->second - variance) >
							1e-10 * std::max(1.0, std::max(old->second, variance)))
					{
						trace << "\n" << tracePrefix << "WL_FACTOR time="
							  << time.to_string(0)
							  << " receiver=" << receiver
							  << " valid=0 reason=RAW_NOISE_VARIANCE_CONFLICT";
						noiseIndex.clear();
						break;
					}
					noiseVarianceByKey[key] = variance;
				}
				if (noiseIndex.empty())
				{
					continue;
				}
				vector<string> factorKeys(noiseIndex.size());
				VectorXd factorVariances = VectorXd::Zero(noiseIndex.size());
				for (const auto& [key, index] : noiseIndex)
				{
					factorKeys[index] = key;
					factorVariances(index) = noiseVarianceByKey.at(key);
				}
				MatrixXd rawNoiseDesign = MatrixXd::Zero(
					usable.size(), noiseIndex.size());
				for (int row = 0; row < static_cast<int>(usable.size()); row++)
				{
					mw(row) = measurements[row];
					for (int factor = 0;
						 factor < static_cast<int>(rawNoiseKeys[row].size()); factor++)
					{
						rawNoiseDesign(row, noiseIndex.at(rawNoiseKeys[row][factor])) +=
							rawNoiseCoefficients[row](factor);
					}
				}
				MatrixXd mwCovariance = rawNoiseDesign *
					factorVariances.asDiagonal() * rawNoiseDesign.transpose();
				vector<int> satelliteNumbers;
				for (const SatSys& satellite : usable)
				{
					satelliteNumbers.push_back(satellite.prn);
				}
				runtime.accumulator.addEpoch(
					static_cast<double>(time.bigTime), satelliteNumbers,
					mw, mwCovariance, factorKeys, factorVariances,
					rawNoiseDesign);
				if (mw.allFinite() && mwCovariance.allFinite() &&
					*std::max_element(
						satelliteNumbers.begin(), satelliteNumbers.end()) < 65)
				{
					runtime.rawFactors.push_back({
						time, satelliteNumbers, mw, mwCovariance, factorKeys,
						factorVariances, rawNoiseDesign});
				}
				while (!runtime.rawFactors.empty() &&
					((time - runtime.rawFactors.front().time).to_double() > 3600 ||
					 static_cast<int>(runtime.rawFactors.size()) > 360))
				{
					runtime.rawFactors.pop_front();
				}
				if (std::find(usable.begin(), usable.end(), persistentIfReference) ==
					usable.end())
				{
					trace << "\n" << tracePrefix << "WL_FACTOR time="
						  << time.to_string(0)
						  << " receiver=" << receiver
						  << " system=" << enum_to_string(system)
						  << " valid=0 reason=REFERENCE_NOT_IF_VALID";
					continue;
				}
				SatSys oldReference = runtime.reference;
				runtime.reference = persistentIfReference;
				if (oldReference != runtime.reference)
				{
					trace << "\n" << tracePrefix << "REFERENCE time="
						  << time.to_string(0)
						  << " receiver=" << receiver
						  << " system=" << enum_to_string(system)
						  << " old="
						  << (oldReference.prn > 0 ? oldReference.id() : "NONE")
						  << " new=" << runtime.reference.id()
						  << " exact_integer_transform="
						  << (oldReference.prn > 0);
				}
				const auto estimate = runtime.accumulator.estimate(
					satelliteNumbers, runtime.reference.prn,
					static_cast<double>(time.bigTime));
				trace << "\n" << tracePrefix << "WL_FACTOR time="
					  << time.to_string(0)
					  << " receiver=" << receiver
					  << " system=" << enum_to_string(system)
					  << " satellites=" << usable.size()
					  << " factors=" << estimate.factorCount
					  << " information_rank=" << estimate.informationRank
					  << " valid=" << estimate.valid
					  << " reason=" << estimate.failureReason;
				if (!estimate.valid)
				{
					continue;
				}
				common = std::move(usable);
				persistentIfReference = runtime.reference;
				ifWideLaneEstimate = estimate;
				vector<KFKey> ambiguityKeys(ambiguityResolution.ambmap.size());
				bool ambiguityKeysValid = true;
				for (const auto& [column, key] : ambiguityResolution.ambmap)
				{
					if (column < 0 ||
						column >= static_cast<int>(ambiguityKeys.size()))
					{
						ambiguityKeysValid = false;
						break;
					}
					ambiguityKeys[column] = key;
				}
				MatrixXd crossCovariance;
				string crossFailure;
				const bool crossValid = ambiguityKeysValid &&
					queryZhangE27IfWideLaneCrossCovariance(
						kfState, ambiguityKeys, estimate,
						crossCovariance, &crossFailure);
				trace << "\n" << tracePrefix << "JOINT_COVARIANCE time="
					  << time.to_string(0)
					  << " receiver=" << receiver
					  << " system=" << enum_to_string(system)
					  << " valid=" << crossValid
					  << " rows=" << crossCovariance.rows()
					  << " columns=" << crossCovariance.cols()
					  << " maximum_absolute="
					  << (crossValid ? crossCovariance.cwiseAbs().maxCoeff() : 0)
					  << " reason=" << (crossValid ? "NONE" : crossFailure)
					  << " feedback=0";
				if (crossValid)
				{
					ifWideLaneCrossCovariance = std::move(crossCovariance);
				}
			}

			const SatSys reference = ifAcceptance
				? persistentIfReference
				: *std::min_element(
				common.begin(), common.end(),
				[&](const SatSys& left, const SatSys& right)
				{
					const double leftVariance =
						ambiguityResolution.Paflt(
							first->second.at(left), first->second.at(left)) +
						ambiguityResolution.Paflt(
							second->second.at(left), second->second.at(left));
					const double rightVariance =
						ambiguityResolution.Paflt(
							first->second.at(right), first->second.at(right)) +
						ambiguityResolution.Paflt(
							second->second.at(right), second->second.at(right));
					return leftVariance != rightVariance
						? leftVariance < rightVariance : left < right;
				});
			vector<SatSys> targets;
			for (const SatSys& satellite : common)
			{
				if (satellite != reference)
				{
					targets.push_back(satellite);
				}
			}
			const int dimension = targets.size();
			MatrixXd firstTransform = MatrixXd::Zero(
				dimension, ambiguityResolution.aflt.size());
			MatrixXd secondTransform = MatrixXd::Zero(
				dimension, ambiguityResolution.aflt.size());
			for (int row = 0; row < dimension; row++)
			{
				firstTransform(row, first->second.at(targets[row])) = +1;
				secondTransform(row, second->second.at(targets[row])) = +1;
				if (!ifAcceptance)
				{
					firstTransform(row, first->second.at(reference)) = -1;
					secondTransform(row, second->second.at(reference)) = -1;
				}
			}
			groupsEvaluated++;
			trace << "\n" << tracePrefix << "LATTICE time=" << time.to_string(0)
				  << " receiver=" << receiver
				  << " system=" << enum_to_string(system)
				  << " reference=" << reference.id()
				  << " common_satellites=" << common.size()
				  << " named_rank=" << dimension
				  << " primitive=1 receiver_datum_cancelled=1"
				  << " product_functional=USER_SD_AFTER_HOU_CORRECTION"
				  << " feedback=" << !shadowOnly;

			auto resolveStage = [&](const string& stageName,
								const MatrixXd& transform,
								GinAR_mtx& stage,
								map<std::size_t, ZhangExactInteger>& named,
								const VectorXd* externalMean = nullptr,
								const MatrixXd* externalCovariance = nullptr,
								const vector<int>* targetRowMap = nullptr)
			{
				stage.aflt = externalMean
					? *externalMean
					: transform * ambiguityResolution.aflt;
				stage.Paflt = externalCovariance
					? *externalCovariance
					: transform * ambiguityResolution.Paflt * transform.transpose();
				const VectorXd rawMean = stage.aflt;
				const MatrixXd rawCovariance = stage.Paflt;
				bool covarianceValid = rawCovariance.rows() == rawMean.size() &&
					rawCovariance.cols() == rawMean.size() &&
					rawCovariance.allFinite();
				double minimumCovarianceEigenvalue = std::numeric_limits<double>::quiet_NaN();
				if (covarianceValid && rawMean.size() > 0)
				{
					const MatrixXd symmetricCovariance =
						0.5 * (rawCovariance + rawCovariance.transpose());
					Eigen::SelfAdjointEigenSolver<MatrixXd> covarianceSolver(
						symmetricCovariance);
					covarianceValid = covarianceSolver.info() == Eigen::Success &&
						covarianceSolver.eigenvalues().allFinite();
					if (covarianceValid)
					{
						minimumCovarianceEigenvalue =
							covarianceSolver.eigenvalues().minCoeff();
						const double covarianceScale = std::max(
							1.0, covarianceSolver.eigenvalues().cwiseAbs().maxCoeff());
						covarianceValid = minimumCovarianceEigenvalue >=
							-1e-12 * covarianceScale &&
							rawCovariance.diagonal().minCoeff() > 0;
					}
				}
				int fixed = 0;
				if (covarianceValid)
				{
					fixed = rankAwareGnssAr(
						trace, stage, namedOptions, time, stageName);
				}
				else
				{
					trace << "\n" << tracePrefix << "COVARIANCE_GATE time="
						  << time.to_string(0)
						  << " receiver=" << receiver
						  << " system=" << enum_to_string(system)
						  << " stage=" << stageName
						  << " valid=0 reason=NON_PSD_OR_NON_POSITIVE_VARIANCE"
						  << " minimum_eigenvalue=" << minimumCovarianceEigenvalue;
				}
				if (fixed > 0)
				{
					stage.aflt = rawMean;
					stage.Paflt = rawCovariance;
					fixed = retainNisCompatibleNamedRows(
						trace, stage, time, stageName);
				}
				const int candidateCount = rawMean.size();
				named = recoverNamedTargets(stage, candidateCount);
				const auto provisionalNamed = named;
				const std::size_t provisionalSelected = named.size();
				double stageMaximumPerr = 0;
				for (int row = 0; row < candidateCount; row++)
				{
					const double fractional = rawMean(row) -
						std::round(rawMean(row));
					const double variance = rawCovariance(row, row);
					const double perr = variance > 0 && std::isfinite(variance)
						? round_perr(fractional, variance)
						: std::numeric_limits<double>::quiet_NaN();
					if (named.count(row) > 0)
					{
						stageMaximumPerr = std::max(stageMaximumPerr, perr);
					}
				}
				const ZhangIntegerCandidateNis nis =
					assessZhangIntegerCandidateNis(
						stage, acsConfig.zhangPppAr.held_constraint_nis_alpha);
				const bool reliable = covarianceValid && !named.empty() && nis.valid &&
					nis.nis <= nis.threshold && stageMaximumPerr <=
						acsConfig.zhangPppAr.canonical_user_target_max_perr;
				if (!reliable)
				{
					named.clear();
					stage.Ztrs.resize(0, candidateCount);
					stage.zfix.resize(0);
					fixed = 0;
				}
				for (int row = 0; row < candidateCount; row++)
				{
					const int targetRow = targetRowMap
						? targetRowMap->at(row) : row;
					const double fractional = rawMean(row) -
						std::round(rawMean(row));
					const double variance = rawCovariance(row, row);
					const double perr = variance > 0 && std::isfinite(variance)
						? round_perr(fractional, variance)
						: std::numeric_limits<double>::quiet_NaN();
					trace << "\n" << tracePrefix << "TARGET time=" << time.to_string(0)
						  << " receiver=" << receiver
						  << " system=" << enum_to_string(system)
						  << " stage=" << stageName
						  << " reference=" << reference.id()
						  << " satellite=" << targets[targetRow].id()
						  << " mean=" << rawMean(row)
						  << " variance=" << variance
						  << " fractional=" << fractional
						  << " perr=" << perr
						  << " provisional_selected="
						  << (provisionalNamed.count(row) > 0)
						  << " selected=" << (named.count(row) > 0)
						  << " feedback=" << !shadowOnly;
				}
				trace << "\n" << tracePrefix << "STAGE time=" << time.to_string(0)
					  << " receiver=" << receiver
					  << " system=" << enum_to_string(system)
					  << " stage=" << stageName
					  << " candidates=" << candidateCount
					  << " provisional_selected=" << provisionalSelected
					  << " selected=" << named.size()
					  << " joint_nis=" << nis.nis
					  << " joint_threshold=" << nis.threshold
					  << " joint_rank=" << nis.rank
					  << " maximum_perr=" << stageMaximumPerr
					  << " reliability_gate=" << reliable
					  << " feedback=" << !shadowOnly;
				if (reliable)
				{
					maximumPerr = std::max(maximumPerr, stageMaximumPerr);
				}
				if (targetRowMap)
				{
					map<std::size_t, ZhangExactInteger> translated;
					for (const auto& [local, value] : named)
					{
						translated[targetRowMap->at(local)] = value;
					}
					named = std::move(translated);
				}
				return fixed;
			};

			MatrixXd wideLaneTransform = firstTransform - secondTransform;
			GinAR_mtx wideLane;
			map<std::size_t, ZhangExactInteger> fixedWideLane;
			const string wideLaneStage = ifAcceptance
				? "USER_IF_WL_SD" : "USER_WL_SD";
			int wideLaneFixed = resolveStage(
				wideLaneStage, wideLaneTransform, wideLane, fixedWideLane,
				ifWideLaneEstimate ? &ifWideLaneEstimate->mean : nullptr,
				ifWideLaneEstimate ? &ifWideLaneEstimate->covariance : nullptr);
			const ZhangHeldUserWideLaneKey heldKey{
				userRuntimeId, receiver, system, ifAcceptance};
			auto& heldWideLane = zhangHeldUserWideLaneRegistry[heldKey];
			if (heldWideLane.reference != reference)
			{
				heldWideLane = {};
				heldWideLane.reference = reference;
			}
			string wideLaneSource = "NEW_FIX";
			if (wideLaneFixed > 0)
			{
				for (const auto& [target, value] : fixedWideLane)
				{
					if (target < targets.size())
					{
						heldWideLane.integers[targets[target]] = value;
					}
				}
			}
			else if (!heldWideLane.integers.empty())
			{
				const VectorXd heldMean = ifWideLaneEstimate
					? ifWideLaneEstimate->mean
					: wideLaneTransform * ambiguityResolution.aflt;
				const MatrixXd heldCovariance = ifWideLaneEstimate
					? ifWideLaneEstimate->covariance
					: wideLaneTransform * ambiguityResolution.Paflt *
						wideLaneTransform.transpose();
				vector<int> heldRows;
				vector<double> heldValues;
				double heldMaximumPerr = 0;
				for (int row = 0; row < dimension; row++)
				{
					auto held = heldWideLane.integers.find(targets[row]);
					if (held == heldWideLane.integers.end())
					{
						continue;
					}
					heldRows.push_back(row);
					heldValues.push_back(held->second.convert_to<double>());
					heldMaximumPerr = std::max(
						heldMaximumPerr,
						round_perr(
							heldMean(row) - heldValues.back(),
							heldCovariance(row, row)));
				}
				wideLane.aflt = heldMean;
				wideLane.Paflt = heldCovariance;
				wideLane.Ztrs = MatrixXd::Zero(heldRows.size(), dimension);
				wideLane.zfix = VectorXd::Zero(heldRows.size());
				for (int local = 0; local < static_cast<int>(heldRows.size()); local++)
				{
					wideLane.Ztrs(local, heldRows[local]) = 1;
					wideLane.zfix(local) = heldValues[local];
				}
				const ZhangIntegerCandidateNis heldNis =
					assessZhangIntegerCandidateNis(
						wideLane,
						acsConfig.zhangPppAr.held_constraint_nis_alpha);
				const bool heldReliable = !heldRows.empty() && heldNis.valid &&
					heldNis.nis <= heldNis.threshold &&
					heldMaximumPerr <=
						acsConfig.zhangPppAr.canonical_user_target_max_perr;
				if (heldReliable)
				{
					for (int local = 0;
						 local < static_cast<int>(heldRows.size()); local++)
					{
						fixedWideLane[heldRows[local]] =
							static_cast<long long>(std::llround(heldValues[local]));
					}
					wideLaneFixed = heldRows.size();
					wideLaneSource = "HELD_REEVALUATION";
				}
				else
				{
					wideLane.Ztrs.resize(0, dimension);
					wideLane.zfix.resize(0);
				}
				trace << "\n" << tracePrefix << "HELD_WL time="
					  << time.to_string(0)
					  << " receiver=" << receiver
					  << " system=" << enum_to_string(system)
					  << " available=" << heldRows.size()
					  << " nis=" << heldNis.nis
					  << " nis_threshold=" << heldNis.threshold
					  << " maximum_perr=" << heldMaximumPerr
					  << " accepted=" << heldReliable
					  << " feedback=" << !shadowOnly;
			}
			namedWideLane += fixedWideLane.size();
			trace << "\n" << tracePrefix << "WL_SCHEDULER time="
				  << time.to_string(0)
				  << " receiver=" << receiver
				  << " system=" << enum_to_string(system)
				  << " source=" << wideLaneSource
				  << " held_rank=" << fixedWideLane.size()
				  << " evaluate_l1=" << (wideLaneFixed > 0);
			if (wideLaneFixed <= 0)
			{
				continue;
			}
			if (!shadowOnly && static_cast<int>(fixedWideLane.size()) <
				acsConfig.zhangPppAr.canonical_user_target_min_named_wl)
			{
				trace << "\n" << tracePrefix << "FEEDBACK_GATE time="
					  << time.to_string(0)
					  << " receiver=" << receiver
					  << " system=" << enum_to_string(system)
					  << " status=REJECTED_INSUFFICIENT_NAMED_WL"
					  << " selected=" << fixedWideLane.size()
					  << " required="
					  << acsConfig.zhangPppAr.canonical_user_target_min_named_wl;
				continue;
			}
			if (!ifAcceptance)
			{
				if (!applyAndCapture(
						wideLane.Ztrs * wideLaneTransform,
						wideLane.zfix,
						wideLaneStage))
				{
					continue;
				}
				refreshAmbiguities();
			}

			GinAR_mtx firstStage;
			map<std::size_t, ZhangExactInteger> fixedFirst;
			const string firstStageName = ifAcceptance
				? "USER_IF_L1_SD_CONDITIONAL"
				: "USER_L1_SD_CONDITIONAL";
			VectorXd conditionalFirstMean;
			MatrixXd conditionalFirstCovariance;
			MatrixXd firstStageTransform = firstTransform;
			vector<int> conditionalTargetRows;
			if (ifAcceptance)
			{
				// The shared ambiguity state is N_IF in cycles, where
				// A_IF = lambda_NL * N_IF.  Once N_WL is fixed externally,
				// N1 = N_IF + beta*lambda2/lambda_NL*N_WL is the true
				// integer-estimable first-frequency target.  N_IF itself is
				// not an integer and must never be rounded directly.
				for (const auto& [target, value] : fixedWideLane)
				{
					if (target < static_cast<std::size_t>(dimension))
					{
						conditionalTargetRows.push_back(target);
					}
				}
				firstStageTransform = MatrixXd::Zero(
					conditionalTargetRows.size(), ambiguityResolution.aflt.size());
				for (int local = 0;
					 local < static_cast<int>(conditionalTargetRows.size()); local++)
				{
					firstStageTransform.row(local) =
						firstTransform.row(conditionalTargetRows[local]);
				}
				if (!ifWideLaneEstimate || !ifWideLaneCrossCovariance)
				{
					trace << "\n" << tracePrefix << "CONDITIONAL_GATE time="
						  << time.to_string(0)
						  << " receiver=" << receiver
						  << " system=" << enum_to_string(system)
						  << " valid=0 reason=JOINT_IF_WL_COVARIANCE_MISSING"
						  << " feedback=0";
					continue;
				}
				const int conditionalDimension = conditionalTargetRows.size();
				VectorXd selectedWideLaneMean =
					VectorXd::Zero(conditionalDimension);
				VectorXd selectedFixedWideLane =
					VectorXd::Zero(conditionalDimension);
				MatrixXd selectedWideLaneCovariance =
					MatrixXd::Zero(conditionalDimension, conditionalDimension);
				MatrixXd selectedCrossCovariance = firstStageTransform *
					*ifWideLaneCrossCovariance;
				MatrixXd compactCrossCovariance =
					MatrixXd::Zero(conditionalDimension, conditionalDimension);
				for (int local = 0;
					 local < static_cast<int>(conditionalTargetRows.size()); local++)
				{
					const int target = conditionalTargetRows[local];
					selectedWideLaneMean(local) =
						ifWideLaneEstimate->mean(target);
					selectedFixedWideLane(local) =
						fixedWideLane.at(target).convert_to<double>();
					for (int other = 0; other < conditionalDimension; other++)
					{
						const int otherTarget = conditionalTargetRows[other];
						selectedWideLaneCovariance(local, other) =
							ifWideLaneEstimate->covariance(target, otherTarget);
						compactCrossCovariance(local, other) =
							selectedCrossCovariance(local, otherTarget);
					}
				}
				const VectorXd selectedIfMean =
					firstStageTransform * ambiguityResolution.aflt;
				const MatrixXd selectedIfCovariance = firstStageTransform *
					ambiguityResolution.Paflt * firstStageTransform.transpose();
				const ZhangIfConditionalEstimate conditioned =
					zhangConditionFirstIntegerGivenWideLane(
						selectedIfMean, selectedIfCovariance,
						selectedWideLaneMean, selectedWideLaneCovariance,
						compactCrossCovariance, selectedFixedWideLane,
						conditionedWideLaneCoefficient);
				trace << "\n" << tracePrefix << "CONDITIONAL_GATE time="
					  << time.to_string(0)
					  << " receiver=" << receiver
					  << " system=" << enum_to_string(system)
					  << " valid=" << conditioned.valid
					  << " dimension=" << conditionalDimension
					  << " cross_covariance_maximum_absolute="
					  << (conditionalDimension > 0
						  ? compactCrossCovariance.cwiseAbs().maxCoeff() : 0)
					  << " trace_before=" << selectedIfCovariance.trace()
					  << " trace_after="
					  << (conditioned.valid ? conditioned.covariance.trace() : 0)
					  << " reason=" << conditioned.failureReason
					  << " feedback=0";
				if (!conditioned.valid)
				{
					continue;
				}
				conditionalFirstMean = conditioned.mean;
				conditionalFirstCovariance = conditioned.covariance;
			}
			const int firstFixed = resolveStage(
				firstStageName, firstStageTransform,
				firstStage, fixedFirst,
				ifAcceptance ? &conditionalFirstMean : nullptr,
				ifAcceptance ? &conditionalFirstCovariance : nullptr,
				ifAcceptance ? &conditionalTargetRows : nullptr);
			namedFirst += fixedFirst.size();
			if (firstFixed > 0)
			{
				MatrixXd stateRows = firstStage.Ztrs * firstStageTransform;
				VectorXd stateValues = firstStage.zfix;
				bool stateConstraintValid = true;
				if (ifAcceptance)
				{
					for (int row = 0; row < firstStage.Ztrs.rows(); row++)
					for (int local = 0;
						 local < static_cast<int>(conditionalTargetRows.size()); local++)
					{
						const double coefficient =
							firstStage.Ztrs(row, local);
						if (std::abs(coefficient) <= 1e-12)
						{
							continue;
						}
						const int target = conditionalTargetRows[local];
						auto fixed = fixedWideLane.find(target);
						if (fixed == fixedWideLane.end())
						{
							stateConstraintValid = false;
							break;
						}
						stateValues(row) -= coefficient *
							conditionedWideLaneCoefficient *
							fixed->second.convert_to<double>();
					}
				}
				if (stateConstraintValid)
				{
					applyAndCapture(
						stateRows, stateValues, firstStageName);
				}
				else
				{
					trace << "\n" << tracePrefix << "FEEDBACK_GATE time="
						  << time.to_string(0)
						  << " receiver=" << receiver
						  << " system=" << enum_to_string(system)
						  << " status=REJECTED_L1_WITHOUT_NAMED_WL";
				}
				refreshAmbiguities();
			}
		}
	}

	trace << "\n" << tracePrefix << "SUMMARY time=" << time.to_string(0)
		  << " groups=" << groupsEvaluated
		  << " named_wl=" << namedWideLane
		  << " named_l1=" << namedFirst
		  << " maximum_perr=" << maximumPerr
		  << " shadow=" << shadowOnly
		  << " feedback=" << !shadowOnly;
	if (shadowOnly)
	{
		ambiguityResolution.Ztrs.resize(0, ambiguityResolution.aflt.size());
		ambiguityResolution.zfix.resize(0);
		return 0;
	}

	ambiguityResolution.Ztrs = MatrixXd::Zero(
		fixedRows.size(), ambiguityResolution.aflt.size());
	ambiguityResolution.zfix = VectorXd::Zero(fixedValues.size());
	for (int row = 0; row < static_cast<int>(fixedRows.size()); row++)
	{
		ambiguityResolution.Ztrs.row(row) = fixedRows[row].transpose();
		ambiguityResolution.zfix(row) = fixedValues[row];
	}
	return totalFixed;
}

static int resolveIndependentSignalAmbiguities(
    Trace&       trace,
    GinAR_mtx&   ambiguityResolution,
    const GinAR_opt& options,
    GTime        time,
    bool*        allSignalsFixed = nullptr
)
{
    map<pair<E_Sys, int>, vector<int>> signalColumns;
    for (const auto& [column, key] : ambiguityResolution.ambmap)
    {
        signalColumns[{key.Sat.sys, key.num}].push_back(column);
    }

    vector<VectorXd> fixedRows;
    vector<double>   fixedValues;
    int              resolvedSignals = 0;
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

        int fixed = rankAwareGnssAr(
            trace, integerResolution, options, time, "INDEPENDENT_SIGNAL"
        );
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
        resolvedSignals++;

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
    if (allSignalsFixed)
    {
        *allSignalsFixed = !signalColumns.empty() &&
            resolvedSignals == static_cast<int>(signalColumns.size());
    }
    return fixedRows.size();
}

static std::uint64_t stableZhangAblationSeed(
    int           configuredSeed,
    const string& epoch,
    E_Sys         system
)
{
    std::uint64_t hash = 1469598103934665603ULL;
    const string material = std::to_string(configuredSeed) + "|" + epoch +
        "|" + enum_to_string(system);
    for (unsigned char value : material)
    {
        hash ^= value;
        hash *= 1099511628211ULL;
    }
    return hash;
}

static MatrixXd zhangRandomElementaryUnimodularTransform(
    int dimension,
    std::uint64_t seed,
    int operationCount
)
{
    MatrixXd transform = MatrixXd::Identity(dimension, dimension);
    if (dimension < 2)
    {
        return transform;
    }
    std::mt19937_64 generator(seed);
    std::uniform_int_distribution<int> rowDistribution(0, dimension - 1);
    std::uniform_int_distribution<int> operationDistribution(0, 2);
    for (int operation = 0; operation < operationCount; operation++)
    {
        int left = rowDistribution(generator);
        int right = rowDistribution(generator);
        while (right == left)
        {
            right = rowDistribution(generator);
        }
        switch (operationDistribution(generator))
        {
            case 0:
                transform.row(left).swap(transform.row(right));
                break;
            case 1:
                transform.row(left) *= -1;
                break;
            default:
                transform.row(left) +=
                    (generator() & 1 ? 1.0 : -1.0) *
                    transform.row(right);
                break;
        }
    }
    return transform;
}

struct ZhangL1BeamProductProjection
{
    MatrixXd crossCovariance;
    MatrixXd userQuotientCrossCovariance;
    vector<E_ObsCode> productObservables;
    double   varianceTrace = 0;
    double   userQuotientVarianceTrace = 0;
    int      productCount = 0;
    int      userQuotientRank = 0;
};

/** Build the exact covariance terms needed by
 * tr(C H' (H P H')^+ H C') for the Hou clock-minus-phase products.
 * The additive continuity alignment is deterministic and therefore does not
 * enter this covariance projection.
 */
static ZhangL1BeamProductProjection zhangL1BeamProductProjection(
    const KFState&          kfState,
    E_Sys                   system,
    const map<int, KFKey>&  ambiguityMap
)
{
    ZhangL1BeamProductProjection projection;
    vector<int> ambiguityStateIndices;
    ambiguityStateIndices.reserve(ambiguityMap.size());
    for (int local = 0; local < static_cast<int>(ambiguityMap.size()); local++)
    {
        auto key = ambiguityMap.find(local);
        if (key == ambiguityMap.end())
        {
            return projection;
        }
        auto state = kfState.kfIndexMap.find(key->second);
        if (state == kfState.kfIndexMap.end())
        {
            return projection;
        }
        ambiguityStateIndices.push_back(state->second);
    }

    vector<VectorXd> crossRows;
    struct ProductStateIndices
    {
        int       clock = -1;
        int       phase = -1;
        E_ObsCode code = E_ObsCode::NONE;
    };
    vector<ProductStateIndices> productStates;
    for (const auto& [phaseKey, phaseIndex] : kfState.kfIndexMap)
    {
        if (phaseKey.type != KF::PHASE_BIAS ||
            phaseKey.Sat.sys != system || phaseKey.Sat.prn <= 0 ||
            !phaseKey.str.empty() ||
            !zhangPppArUsesObservable(
                system,
                static_cast<E_ObsCode>(phaseKey.num)))
        {
            continue;
        }
        KFKey clockKey;
        clockKey.type = KF::SAT_CLOCK;
        clockKey.Sat = phaseKey.Sat;
        auto clock = kfState.kfIndexMap.find(clockKey);
        if (clock == kfState.kfIndexMap.end())
        {
            continue;
        }

        const double variance =
            kfState.P(clock->second, clock->second) +
            kfState.P(phaseIndex, phaseIndex) -
            2 * kfState.P(clock->second, phaseIndex);
        const double scale = std::max(
            1.0,
            std::max(
                std::abs(kfState.P(clock->second, clock->second)),
                std::abs(kfState.P(phaseIndex, phaseIndex))
            )
        );
        if (!std::isfinite(variance) || variance < -1e-10 * scale)
        {
            continue;
        }
        VectorXd cross = VectorXd::Zero(ambiguityStateIndices.size());
        for (int local = 0;
             local < static_cast<int>(ambiguityStateIndices.size());
             local++)
        {
            const int ambiguityIndex = ambiguityStateIndices[local];
            cross(local) =
                kfState.P(clock->second, ambiguityIndex) -
                kfState.P(phaseIndex, ambiguityIndex);
        }
        if (!cross.allFinite())
        {
            continue;
        }
        crossRows.push_back(std::move(cross));
        projection.productObservables.push_back(
            static_cast<E_ObsCode>(phaseKey.num));
        productStates.push_back({
            clock->second,
            phaseIndex,
            static_cast<E_ObsCode>(phaseKey.num)
        });
        projection.varianceTrace += std::max(0.0, variance);
    }

    projection.productCount = crossRows.size();
    projection.crossCovariance = MatrixXd::Zero(
        crossRows.size(), ambiguityStateIndices.size()
    );
    for (int row = 0; row < static_cast<int>(crossRows.size()); row++)
    {
        projection.crossCovariance.row(row) = crossRows[row].transpose();
    }
    MatrixXd productCovariance = MatrixXd::Zero(
        productStates.size(), productStates.size());
    for (int row = 0; row < static_cast<int>(productStates.size()); row++)
    {
        const auto& left = productStates[row];
        for (int column = 0;
             column < static_cast<int>(productStates.size()); column++)
        {
            const auto& right = productStates[column];
            productCovariance(row, column) =
                kfState.P(left.clock, right.clock) -
                kfState.P(left.clock, right.phase) -
                kfState.P(left.phase, right.clock) +
                kfState.P(left.phase, right.phase);
        }
    }
    productCovariance = 0.5 *
        (productCovariance + productCovariance.transpose());

    // A user receiver can absorb one common phase-datum mode per signal.
    // Project each signal block onto its satellite-difference quotient without
    // choosing an arbitrary reference satellite.
    MatrixXd quotientProjector = MatrixXd::Zero(
        productStates.size(), productStates.size());
    map<E_ObsCode, vector<int>> signalGroups;
    for (int row = 0; row < static_cast<int>(productStates.size()); row++)
    {
        signalGroups[productStates[row].code].push_back(row);
    }
    for (const auto& [code, rows] : signalGroups)
    {
        if (rows.size() < 2)
        {
            continue;
        }
        const double common = 1.0 / rows.size();
        for (int left : rows)
        {
            for (int right : rows)
            {
                quotientProjector(left, right) =
                    (left == right ? 1.0 : 0.0) - common;
            }
        }
        projection.userQuotientRank += rows.size() - 1;
    }
    projection.userQuotientCrossCovariance =
        quotientProjector * projection.crossCovariance;
    const MatrixXd quotientCovariance =
        quotientProjector * productCovariance * quotientProjector;
    projection.userQuotientVarianceTrace = std::max(
        0.0, quotientCovariance.trace());
    return projection;
}

struct ZhangIarSignalFunctionalRow
{
    E_ObsCode code = E_ObsCode::NONE;
    SatSys    satellite;
    vector<pair<int, double>> coefficients;
};

struct ZhangIarAuditTarget
{
    string               name;
    ZhangIarFunctional   functional;
    int                  rank = 0;
};

static ZhangIarFunctional zhangIarRawFunctional(
    const vector<ZhangIarSignalFunctionalRow>& rows,
    int                                        stateDimension
)
{
    vector<Eigen::Triplet<double>> triplets;
    for (int row = 0; row < static_cast<int>(rows.size()); row++)
    {
        for (const auto& [column, coefficient] : rows[row].coefficients)
        {
            if (coefficient != 0)
            {
                triplets.emplace_back(row, column, coefficient);
            }
        }
    }
    ZhangIarFunctional functional(rows.size(), stateDimension);
    functional.setFromTriplets(
        triplets.begin(), triplets.end(),
        [](double left, double right) { return left + right; });
    functional.makeCompressed();
    return functional;
}

static ZhangIarFunctional zhangIarDatumFreeFunctional(
    const vector<ZhangIarSignalFunctionalRow>& rows,
    int                                        stateDimension,
    int&                                       quotientRank
)
{
    quotientRank = 0;
    map<E_ObsCode, vector<int>> groups;
    for (int row = 0; row < static_cast<int>(rows.size()); row++)
    {
        groups[rows[row].code].push_back(row);
    }
    vector<Eigen::Triplet<double>> triplets;
    for (const auto& [code, group] : groups)
    {
        if (group.size() < 2)
        {
            continue;
        }
        quotientRank += group.size() - 1;
        const double common = 1.0 / group.size();
        for (int output : group)
        {
            for (const auto& [column, coefficient] :
                 rows[output].coefficients)
            {
                triplets.emplace_back(output, column, coefficient);
            }
            for (int source : group)
            {
                for (const auto& [column, coefficient] :
                     rows[source].coefficients)
                {
                    triplets.emplace_back(
                        output, column, -common * coefficient);
                }
            }
        }
    }
    ZhangIarFunctional functional(rows.size(), stateDimension);
    functional.setFromTriplets(
        triplets.begin(), triplets.end(),
        [](double left, double right) { return left + right; });
    functional.prune(1e-15);
    functional.makeCompressed();
    return functional;
}

static ZhangIarFunctional zhangIarSelectionFunctional(
    const vector<int>& indices,
    int                stateDimension
)
{
    vector<ZhangIarSignalFunctionalRow> rows;
    rows.reserve(indices.size());
    for (int index : indices)
    {
        ZhangIarSignalFunctionalRow row;
        row.coefficients.push_back({index, 1});
        rows.push_back(std::move(row));
    }
    return zhangIarRawFunctional(rows, stateDimension);
}

static vector<ZhangIarAuditTarget> zhangIarAuditTargets(
    const KFState& kfState,
    E_Sys         system
)
{
    const int stateDimension = kfState.P.rows();
    vector<ZhangIarSignalFunctionalRow> satellitePhaseRows;
    vector<int> receiverPhaseIndices;
    vector<int> satelliteClockIndices;
    vector<int> receiverClockIndices;
    vector<int> ionosphereIndices;
    map<SatSys, int> satelliteClocks;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        if (index < 0 || index >= stateDimension)
        {
            continue;
        }
        if (key.type == KF::SAT_CLOCK &&
            key.Sat.sys == system && key.Sat.prn > 0)
        {
            satelliteClocks[key.Sat] = index;
            satelliteClockIndices.push_back(index);
        }
        else if (key.type == KF::REC_CLOCK && !key.str.empty())
        {
            receiverClockIndices.push_back(index);
        }
        else if (key.type == KF::IONO_STEC && key.Sat.sys == system)
        {
            ionosphereIndices.push_back(index);
        }
        else if (key.type == KF::PHASE_BIAS &&
                 key.Sat.sys == system &&
                 zhangPppArUsesObservable(
                     system, static_cast<E_ObsCode>(key.num)))
        {
            if (key.Sat.prn > 0 && key.str.empty())
            {
                ZhangIarSignalFunctionalRow row;
                row.code = static_cast<E_ObsCode>(key.num);
                row.satellite = key.Sat;
                row.coefficients.push_back({index, 1});
                satellitePhaseRows.push_back(std::move(row));
            }
            else if (key.Sat.prn == 0 && !key.str.empty())
            {
                receiverPhaseIndices.push_back(index);
            }
        }
    }

    vector<ZhangIarSignalFunctionalRow> houProductRows;
    for (const auto& phase : satellitePhaseRows)
    {
        auto clock = satelliteClocks.find(phase.satellite);
        if (clock == satelliteClocks.end())
        {
            continue;
        }
        ZhangIarSignalFunctionalRow product;
        product.code = phase.code;
        product.satellite = phase.satellite;
        product.coefficients = {
            {clock->second, +1},
            {phase.coefficients.front().first, -1}
        };
        houProductRows.push_back(std::move(product));
    }

    int satelliteDifferenceRank = 0;
    int userProductRank = 0;
    vector<ZhangIarAuditTarget> targets;
    targets.push_back({
        "SATELLITE_PHASE_BIAS",
        zhangIarRawFunctional(satellitePhaseRows, stateDimension),
        static_cast<int>(satellitePhaseRows.size())});
    targets.push_back({
        "BETWEEN_SATELLITE_SD_PHASE_BIAS",
        zhangIarDatumFreeFunctional(
            satellitePhaseRows,
            stateDimension,
            satelliteDifferenceRank),
        satelliteDifferenceRank});
    targets.push_back({
        "RECEIVER_PHASE_BIAS",
        zhangIarSelectionFunctional(
            receiverPhaseIndices, stateDimension),
        static_cast<int>(receiverPhaseIndices.size())});
    targets.push_back({
        "SATELLITE_CLOCK",
        zhangIarSelectionFunctional(
            satelliteClockIndices, stateDimension),
        static_cast<int>(satelliteClockIndices.size())});
    targets.push_back({
        "RECEIVER_CLOCK",
        zhangIarSelectionFunctional(
            receiverClockIndices, stateDimension),
        static_cast<int>(receiverClockIndices.size())});
    targets.push_back({
        "IONOSPHERE",
        zhangIarSelectionFunctional(
            ionosphereIndices, stateDimension),
        static_cast<int>(ionosphereIndices.size())});
    targets.push_back({
        "HOU_CLOCK_MINUS_PHASE_PRODUCT",
        zhangIarRawFunctional(houProductRows, stateDimension),
        static_cast<int>(houProductRows.size())});
    targets.push_back({
        "USER_DATUM_FREE_PRODUCT",
        zhangIarDatumFreeFunctional(
            houProductRows,
            stateDimension,
            userProductRank),
        userProductRank});
    return targets;
}

static double zhangIarRatio(double after, double before)
{
    return std::isfinite(after) && std::isfinite(before) && before > 0
        ? after / before
        : std::numeric_limits<double>::quiet_NaN();
}

struct ZhangTheoryRegressionPair
{
    string receiver;
    SatSys satellite;
};

static double zhangTheoryDominanceFraction(
    const ZhangPairedCorrelationSummary& undifferenced,
    const ZhangPairedCorrelationSummary& satelliteDifference
)
{
    if (!undifferenced.valid || !satelliteDifference.valid ||
        undifferenced.coefficients.size() !=
            satelliteDifference.coefficients.size() ||
        undifferenced.coefficients.empty())
    {
        return std::numeric_limits<double>::quiet_NaN();
    }
    int dominated = 0;
    for (int index = 0;
         index < static_cast<int>(undifferenced.coefficients.size()); index++)
    {
        dominated += std::abs(undifferenced.coefficients[index]) <
            std::abs(satelliteDifference.coefficients[index]);
    }
    return static_cast<double>(dominated) /
        undifferenced.coefficients.size();
}

static ZhangIarFunctional zhangTheorySparseFunctional(
    int                                      rows,
    int                                      columns,
    const vector<Eigen::Triplet<double>>&    triplets
)
{
    ZhangIarFunctional result(rows, columns);
    result.setFromTriplets(
        triplets.begin(), triplets.end(),
        [](double left, double right) { return left + right; });
    result.prune(1e-15);
    result.makeCompressed();
    return result;
}

static void traceZhangTheoryCorrelation(
    Trace&                                 trace,
    GTime                                  time,
    E_Sys                                  system,
    const string&                          domain,
    const vector<ZhangTheoryRegressionPair>& pairs,
    const ZhangPairedCorrelationSummary&   undifferenced,
    const ZhangPairedCorrelationSummary&   satelliteDifference
)
{
    const double dominance = zhangTheoryDominanceFraction(
        undifferenced, satelliteDifference);
    const bool ordering = undifferenced.valid &&
        satelliteDifference.valid &&
        std::abs(undifferenced.pooledCorrelation) <
            std::abs(satelliteDifference.pooledCorrelation);
    if (undifferenced.valid && satelliteDifference.valid &&
        pairs.size() == undifferenced.coefficients.size())
    {
        for (int row = 0; row < static_cast<int>(pairs.size()); row++)
        {
            trace << "\nZHANG_E24B_THEORY_CORRELATION_PAIR time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " domain=" << domain
                  << " receiver=" << pairs[row].receiver
                  << " satellite=" << pairs[row].satellite.id()
                  << " rho_UD=" << undifferenced.coefficients[row]
                  << " rho_satellite_SD="
                  << satelliteDifference.coefficients[row]
                  << " abs_order="
                  << (std::abs(undifferenced.coefficients[row]) <
                      std::abs(satelliteDifference.coefficients[row])
                        ? "PASS" : "FAIL")
                  << " feedback=SHADOW_NONE";
        }
    }
    trace << "\nZHANG_E24B_THEORY_CORRELATION time=" << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " domain=" << domain
          << " pairs=" << undifferenced.pairs
          << " pooled_rho_UD=" << undifferenced.pooledCorrelation
          << " pooled_rho_satellite_SD="
          << satelliteDifference.pooledCorrelation
          << " pooled_abs_rho_UD="
          << std::abs(undifferenced.pooledCorrelation)
          << " pooled_abs_rho_satellite_SD="
          << std::abs(satelliteDifference.pooledCorrelation)
          << " mean_abs_rho_UD=" << undifferenced.meanAbsolute
          << " mean_abs_rho_satellite_SD="
          << satelliteDifference.meanAbsolute
          << " rms_abs_rho_UD=" << undifferenced.rmsAbsolute
          << " rms_abs_rho_satellite_SD="
          << satelliteDifference.rmsAbsolute
          << " median_abs_rho_UD=" << undifferenced.medianAbsolute
          << " median_abs_rho_satellite_SD="
          << satelliteDifference.medianAbsolute
          << " pairwise_dominance_fraction=" << dominance
          << " hypothesis_abs_rho_UD_lt_satellite_SD="
          << (ordering ? "PASS" : "FAIL")
          << " status="
          << (undifferenced.valid && satelliteDifference.valid
                ? "VALID" : "INVALID")
          << " feedback=SHADOW_NONE";
}

struct ZhangTheoryGainResult
{
    string name;
    double traceF0 = std::numeric_limits<double>::quiet_NaN();
    double traceFixL1 = std::numeric_limits<double>::quiet_NaN();
    double traceFixWideLane = std::numeric_limits<double>::quiet_NaN();
    double traceFixAll = std::numeric_limits<double>::quiet_NaN();
    double gammaAll = std::numeric_limits<double>::quiet_NaN();
    bool valid = false;
};

static ZhangTheoryGainResult traceZhangTheoryGain(
    Trace&                               trace,
    GTime                                time,
    E_Sys                                system,
    const string&                        name,
    const MatrixXd&                      covariance,
    const ZhangIarFunctional&            target,
    const ZhangIarCovarianceCondition&   fixL1,
    const ZhangIarCovarianceCondition&   fixWideLane,
    const ZhangIarCovarianceCondition&   fixAll
)
{
    ZhangTheoryGainResult result;
    result.name = name;
    result.traceF0 = zhangIarProjectedCovarianceTrace(covariance, target);
    result.traceFixL1 = zhangIarProjectedCovarianceTrace(
        covariance, fixL1, target);
    result.traceFixWideLane = zhangIarProjectedCovarianceTrace(
        covariance, fixWideLane, target);
    result.traceFixAll = zhangIarProjectedCovarianceTrace(
        covariance, fixAll, target);
    result.gammaAll = zhangIarRatio(result.traceFixAll, result.traceF0);
    const double gammaL1 = zhangIarRatio(
        result.traceFixL1, result.traceF0);
    const double gammaWideLane = zhangIarRatio(
        result.traceFixWideLane, result.traceF0);
    const double gammaL1AfterWideLane = zhangIarRatio(
        result.traceFixAll, result.traceFixWideLane);
    result.valid = std::isfinite(result.traceF0) &&
        std::isfinite(result.traceFixL1) &&
        std::isfinite(result.traceFixWideLane) &&
        std::isfinite(result.traceFixAll) && result.traceF0 > 0 &&
        result.traceFixL1 <= result.traceF0 * (1 + 1e-9) + 1e-12 &&
        result.traceFixWideLane <= result.traceF0 * (1 + 1e-9) + 1e-12 &&
        result.traceFixAll <= result.traceFixL1 * (1 + 1e-9) + 1e-12 &&
        result.traceFixAll <=
            result.traceFixWideLane * (1 + 1e-9) + 1e-12;
    trace << "\nZHANG_E24B_THEORY_GAIN time=" << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " target=" << name
          << " target_rows=" << target.rows()
          << " trace_F0=" << result.traceF0
          << " trace_FIX_L1_DD=" << result.traceFixL1
          << " trace_FIX_WL_DD=" << result.traceFixWideLane
          << " trace_FIX_ALL_DD=" << result.traceFixAll
          << " gamma_FIX_L1_over_F0=" << gammaL1
          << " std_gamma_FIX_L1_over_F0=" << std::sqrt(gammaL1)
          << " gamma_FIX_WL_over_F0=" << gammaWideLane
          << " std_gamma_FIX_WL_over_F0=" << std::sqrt(gammaWideLane)
          << " gamma_FIX_ALL_over_F0=" << result.gammaAll
          << " std_gamma_FIX_ALL_over_F0=" << std::sqrt(result.gammaAll)
          << " gamma_L1_after_WL=" << gammaL1AfterWideLane
          << " std_gamma_L1_after_WL="
          << std::sqrt(gammaL1AfterWideLane)
          << " status=" << (result.valid ? "VALID" : "INVALID")
          << " feedback=SHADOW_NONE";
    return result;
}

static void traceZhangCanonicalTheoryRegression(
    Trace&                trace,
    const KFState&        kfState,
    const GinAR_mtx&      ambiguityResolution,
    const ZhangGraphIntegerContext& graphContext,
    E_Sys                 system,
    E_ObsCode             firstCode,
    E_ObsCode             secondCode,
    GTime                  time
)
{
    vector<string> receivers;
    for (string receiver :
         acsConfig.zhangPppAr.canonical_theory_regression_receivers)
    {
        boost::to_upper(receiver);
        if (std::find(receivers.begin(), receivers.end(), receiver) ==
            receivers.end())
        {
            receivers.push_back(receiver);
        }
    }
    const int requestedReceivers = receivers.size();
    bool receiverSetValid = receivers.size() >= 2;
    for (const string& receiver : receivers)
    {
        receiverSetValid = receiverSetValid &&
            graphContext.basis.receivers.count(receiver) > 0;
    }

    set<SatSys> commonSatellites;
    if (receiverSetValid)
    {
        commonSatellites = graphContext.basis.satellites;
        for (const string& receiver : receivers)
        {
            set<SatSys> visible;
            for (const ZhangGraphEdge& edge : graphContext.basis.edges)
            {
                if (edge.receiver == receiver)
                {
                    visible.insert(edge.satellite);
                }
            }
            set<SatSys> intersection;
            std::set_intersection(
                commonSatellites.begin(), commonSatellites.end(),
                visible.begin(), visible.end(),
                std::inserter(intersection, intersection.begin()));
            commonSatellites = std::move(intersection);
        }
    }
    const int minimumSatellites = std::max(
        2,
        acsConfig.zhangPppAr
            .canonical_theory_regression_min_common_satellites);
    const bool bicliqueValid = receiverSetValid &&
        static_cast<int>(commonSatellites.size()) >= minimumSatellites;
    if (!bicliqueValid)
    {
        trace << "\nZHANG_E24B_THEORY_SUMMARY time=" << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " requested_receivers=" << requestedReceivers
              << " active_requested_receivers="
              << (receiverSetValid ? requestedReceivers : 0)
              << " common_satellites=" << commonSatellites.size()
              << " minimum_common_satellites=" << minimumSatellites
              << " status=SKIPPED_NO_COMPLETE_BICLIQUE"
              << " ar_authorized=0 feedback=SHADOW_NONE";
        return;
    }

    map<E_ObsCode, map<ZhangGraphEdge, int>> chordColumns;
    map<E_ObsCode, vector<int>> stateIndices;
    for (const auto& [globalColumn, key] : ambiguityResolution.ambmap)
    {
        const E_ObsCode code = static_cast<E_ObsCode>(key.num);
        if (key.Sat.sys != system ||
            (code != firstCode && code != secondCode))
        {
            continue;
        }
        const int local = stateIndices[code].size();
        auto state = kfState.kfIndexMap.find(key);
        if (state == kfState.kfIndexMap.end())
        {
            continue;
        }
        chordColumns[code][{key.str, key.Sat}] = local;
        stateIndices[code].push_back(state->second);
    }

    const vector<SatSys> satellites(
        commonSatellites.begin(), commonSatellites.end());
    const string& referenceReceiver = receivers.front();
    const SatSys& referenceSatellite = satellites.front();
    vector<ZhangTheoryRegressionPair> pairs;
    vector<VectorXd> firstRows;
    vector<VectorXd> secondRows;
    bool coordinateRowsValid = !stateIndices[firstCode].empty() &&
        !stateIndices[secondCode].empty();
    for (int receiverIndex = 1;
         coordinateRowsValid && receiverIndex < receivers.size();
         receiverIndex++)
    {
        for (int satelliteIndex = 1;
             satelliteIndex < satellites.size(); satelliteIndex++)
        {
            VectorXd first;
            VectorXd second;
            coordinateRowsValid = zhangDdCycleCoordinateRow(
                graphContext.basis,
                chordColumns[firstCode],
                referenceReceiver,
                receivers[receiverIndex],
                referenceSatellite,
                satellites[satelliteIndex],
                first) && zhangDdCycleCoordinateRow(
                graphContext.basis,
                chordColumns[secondCode],
                referenceReceiver,
                receivers[receiverIndex],
                referenceSatellite,
                satellites[satelliteIndex],
                second);
            if (!coordinateRowsValid)
            {
                break;
            }
            pairs.push_back({
                receivers[receiverIndex], satellites[satelliteIndex]});
            firstRows.push_back(std::move(first));
            secondRows.push_back(std::move(second));
        }
    }

    const auto wavelengths = phaseClockOsbCoefficients(
        system, firstCode, secondCode);
    map<pair<E_ObsCode, SatSys>, int> phaseIndices;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type == KF::PHASE_BIAS && key.Sat.sys == system &&
            key.Sat.prn > 0 && key.str.empty())
        {
            phaseIndices[{static_cast<E_ObsCode>(key.num), key.Sat}] = index;
        }
    }
    bool phaseRowsValid = wavelengths.has_value();
    for (const SatSys& satellite : satellites)
    {
        phaseRowsValid = phaseRowsValid &&
            phaseIndices.count({firstCode, satellite}) > 0 &&
            phaseIndices.count({secondCode, satellite}) > 0;
    }
    const int expectedRows = (receivers.size() - 1) *
        (satellites.size() - 1);
    if (!coordinateRowsValid || !phaseRowsValid ||
        pairs.size() != expectedRows)
    {
        trace << "\nZHANG_E24B_THEORY_SUMMARY time=" << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " requested_receivers=" << requestedReceivers
              << " common_satellites=" << commonSatellites.size()
              << " expected_dd_rows=" << expectedRows
              << " constructed_dd_rows=" << pairs.size()
              << " coordinate_rows_valid=" << coordinateRowsValid
              << " phase_rows_valid=" << phaseRowsValid
              << " status=SKIPPED_INVALID_FUNCTIONAL_MAPPING"
              << " ar_authorized=0 feedback=SHADOW_NONE";
        return;
    }

    const int stateDimension = kfState.P.rows();
    vector<Eigen::Triplet<double>> firstTriplets;
    vector<Eigen::Triplet<double>> secondTriplets;
    vector<Eigen::Triplet<double>> wideLaneTriplets;
    vector<Eigen::Triplet<double>> allTriplets;
    vector<Eigen::Triplet<double>> l1UdTriplets;
    vector<Eigen::Triplet<double>> l1SdTriplets;
    vector<Eigen::Triplet<double>> wlUdTriplets;
    vector<Eigen::Triplet<double>> wlSdTriplets;
    for (int row = 0; row < expectedRows; row++)
    {
        for (int column = 0; column < firstRows[row].size(); column++)
        {
            const double coefficient = firstRows[row](column);
            if (coefficient == 0)
            {
                continue;
            }
            const int state = stateIndices[firstCode][column];
            firstTriplets.emplace_back(row, state, coefficient);
            wideLaneTriplets.emplace_back(row, state, coefficient);
            allTriplets.emplace_back(row, state, coefficient);
        }
        for (int column = 0; column < secondRows[row].size(); column++)
        {
            const double coefficient = secondRows[row](column);
            if (coefficient == 0)
            {
                continue;
            }
            const int state = stateIndices[secondCode][column];
            secondTriplets.emplace_back(row, state, coefficient);
            wideLaneTriplets.emplace_back(row, state, -coefficient);
            allTriplets.emplace_back(
                expectedRows + row, state, coefficient);
        }

        const SatSys& satellite = pairs[row].satellite;
        const int firstSatellite = phaseIndices.at({firstCode, satellite});
        const int firstReference = phaseIndices.at(
            {firstCode, referenceSatellite});
        const int secondSatellite = phaseIndices.at({secondCode, satellite});
        const int secondReference = phaseIndices.at(
            {secondCode, referenceSatellite});
        const double inverseFirst = 1 / wavelengths->lambda1;
        const double inverseSecond = 1 / wavelengths->lambda2;
        l1UdTriplets.emplace_back(row, firstSatellite, inverseFirst);
        l1SdTriplets.emplace_back(row, firstSatellite, inverseFirst);
        l1SdTriplets.emplace_back(row, firstReference, -inverseFirst);
        wlUdTriplets.emplace_back(row, firstSatellite, inverseFirst);
        wlUdTriplets.emplace_back(row, secondSatellite, -inverseSecond);
        wlSdTriplets.emplace_back(row, firstSatellite, inverseFirst);
        wlSdTriplets.emplace_back(row, firstReference, -inverseFirst);
        wlSdTriplets.emplace_back(row, secondSatellite, -inverseSecond);
        wlSdTriplets.emplace_back(row, secondReference, inverseSecond);
    }

    const ZhangIarFunctional l1Ambiguities = zhangTheorySparseFunctional(
        expectedRows, stateDimension, firstTriplets);
    const ZhangIarFunctional l2Ambiguities = zhangTheorySparseFunctional(
        expectedRows, stateDimension, secondTriplets);
    const ZhangIarFunctional wlAmbiguities = zhangTheorySparseFunctional(
        expectedRows, stateDimension, wideLaneTriplets);
    const ZhangIarFunctional allAmbiguities = zhangTheorySparseFunctional(
        2 * expectedRows, stateDimension, allTriplets);
    const ZhangIarFunctional l1Ud = zhangTheorySparseFunctional(
        expectedRows, stateDimension, l1UdTriplets);
    const ZhangIarFunctional l1Sd = zhangTheorySparseFunctional(
        expectedRows, stateDimension, l1SdTriplets);
    const ZhangIarFunctional wlUd = zhangTheorySparseFunctional(
        expectedRows, stateDimension, wlUdTriplets);
    const ZhangIarFunctional wlSd = zhangTheorySparseFunctional(
        expectedRows, stateDimension, wlSdTriplets);

    const ZhangPairedCorrelationSummary l1UdCorrelation =
        zhangPairedCorrelations(kfState.P, l1Ambiguities, l1Ud);
    const ZhangPairedCorrelationSummary l1SdCorrelation =
        zhangPairedCorrelations(kfState.P, l1Ambiguities, l1Sd);
    const ZhangPairedCorrelationSummary wlUdCorrelation =
        zhangPairedCorrelations(kfState.P, wlAmbiguities, wlUd);
    const ZhangPairedCorrelationSummary wlSdCorrelation =
        zhangPairedCorrelations(kfState.P, wlAmbiguities, wlSd);
    traceZhangTheoryCorrelation(
        trace, time, system, "L1", pairs,
        l1UdCorrelation, l1SdCorrelation);
    traceZhangTheoryCorrelation(
        trace, time, system, "WL", pairs,
        wlUdCorrelation, wlSdCorrelation);

    const ZhangIarCovarianceCondition fixL1 =
        zhangIarCovarianceCondition(kfState.P, l1Ambiguities);
    const ZhangIarCovarianceCondition fixWideLane =
        zhangIarCovarianceCondition(kfState.P, wlAmbiguities);
    const ZhangIarCovarianceCondition fixAll =
        zhangIarCovarianceCondition(kfState.P, allAmbiguities);
    const vector<ZhangTheoryGainResult> gains = {
        traceZhangTheoryGain(
            trace, time, system, "L1_UD_PHASE_BIAS",
            kfState.P, l1Ud, fixL1, fixWideLane, fixAll),
        traceZhangTheoryGain(
            trace, time, system, "L1_SATELLITE_SD_PHASE_BIAS",
            kfState.P, l1Sd, fixL1, fixWideLane, fixAll),
        traceZhangTheoryGain(
            trace, time, system, "WL_UD_PHASE_BIAS",
            kfState.P, wlUd, fixL1, fixWideLane, fixAll),
        traceZhangTheoryGain(
            trace, time, system, "WL_SATELLITE_SD_PHASE_BIAS",
            kfState.P, wlSd, fixL1, fixWideLane, fixAll)
    };
    const bool correlationsValid = l1UdCorrelation.valid &&
        l1SdCorrelation.valid && wlUdCorrelation.valid &&
        wlSdCorrelation.valid;
    const bool gainsValid = std::all_of(
        gains.begin(), gains.end(),
        [](const ZhangTheoryGainResult& gain) { return gain.valid; });
    const bool correlationHypothesis =
        std::abs(l1UdCorrelation.pooledCorrelation) <
        std::abs(l1SdCorrelation.pooledCorrelation);
    const bool gainHypothesis = gains[3].gammaAll < gains[1].gammaAll;

    trace << "\nZHANG_E24B_THEORY_SUBNET time=" << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " requested_receivers=" << requestedReceivers
          << " active_receivers=" << receivers.size()
          << " common_satellites=" << satellites.size()
          << " dd_rows=" << expectedRows
          << " reference_receiver=" << referenceReceiver
          << " reference_satellite=" << referenceSatellite.id()
          << " receivers=";
    for (int index = 0; index < receivers.size(); index++)
    {
        trace << (index == 0 ? "" : ",") << receivers[index];
    }
    trace << " satellites=";
    for (int index = 0; index < satellites.size(); index++)
    {
        trace << (index == 0 ? "" : ",") << satellites[index].id();
    }
    trace << " complete_dual_frequency_biclique=1"
          << " coordinate_model=CURRENT_FUNDAMENTAL_CYCLES_TO_PHYSICAL_DD"
          << " feedback=SHADOW_NONE";
    trace << "\nZHANG_E24B_THEORY_SUMMARY time=" << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " covariance_stage=F0_PRE_IAR"
          << " paper_replication="
             "FORMAL_COVARIANCE_FIG6_TYPE_NOT_FILTER_RESIDUAL_SCATTER"
          << " correlation_hypothesis="
          << (correlationHypothesis ? "PASS" : "FAIL")
          << " wl_gain_gt_l1_gain_hypothesis="
          << (gainHypothesis ? "PASS" : "FAIL")
          << " l1_sd_full_fix_variance_reduction="
          << (1 - gains[1].gammaAll)
          << " wl_sd_full_fix_variance_reduction="
          << (1 - gains[3].gammaAll)
          << " l1_constraint_rank=" << fixL1.rank
          << " wl_constraint_rank=" << fixWideLane.rank
          << " all_constraint_rank=" << fixAll.rank
          << " status="
          << (correlationsValid && gainsValid && fixL1.valid &&
              fixWideLane.valid && fixAll.valid ? "VALID" : "INVALID")
          << " ar_authorized=0 feedback=SHADOW_NONE";
}

static void traceZhangIarGainAudit(
    Trace&                      trace,
    const KFState&              kfState,
    E_Sys                       system,
    GTime                       time,
    const MatrixXd&             pF0,
    const MatrixXd&             pWideLane,
    const ZhangIarFunctional&   parConstraintRows,
    const ZhangIarFunctional&   fullConstraintRows
)
{
    trace << std::setprecision(16);
    const bool dimensionsValid = pF0.rows() == pWideLane.rows() &&
        pF0.cols() == pWideLane.cols() &&
        pF0.rows() == pF0.cols() &&
        pF0.rows() == kfState.P.rows();
    const ZhangIarCovarianceCondition par = dimensionsValid
        ? zhangIarCovarianceCondition(pWideLane, parConstraintRows)
        : ZhangIarCovarianceCondition{};
    const ZhangIarCovarianceCondition full = dimensionsValid
        ? zhangIarCovarianceCondition(pWideLane, fullConstraintRows)
        : ZhangIarCovarianceCondition{};
    const vector<ZhangIarAuditTarget> targets = dimensionsValid
        ? zhangIarAuditTargets(kfState, system)
        : vector<ZhangIarAuditTarget>{};
    int validTargets = 0;
    for (const auto& target : targets)
    {
        const double traceF0 = zhangIarProjectedCovarianceTrace(
            pF0, target.functional);
        const double traceWideLane = zhangIarProjectedCovarianceTrace(
            pWideLane, target.functional);
        const double tracePar = zhangIarProjectedCovarianceTrace(
            pWideLane, par, target.functional);
        const double traceFull = zhangIarProjectedCovarianceTrace(
            pWideLane, full, target.functional);
        const double gammaWideLane = zhangIarRatio(
            traceWideLane, traceF0);
        const double gammaParIncremental = zhangIarRatio(
            tracePar, traceWideLane);
        const double gammaFullIncremental = zhangIarRatio(
            traceFull, traceWideLane);
        const double gammaParCumulative = zhangIarRatio(
            tracePar, traceF0);
        const double gammaFullCumulative = zhangIarRatio(
            traceFull, traceF0);
        const bool valid = std::isfinite(traceF0) &&
            std::isfinite(traceWideLane) && std::isfinite(tracePar) &&
            std::isfinite(traceFull) &&
            traceWideLane <= traceF0 * (1 + 1e-9) + 1e-12 &&
            tracePar <= traceWideLane * (1 + 1e-9) + 1e-12 &&
            traceFull <= traceWideLane * (1 + 1e-9) + 1e-12;
        validTargets += valid;
        trace << "\nZHANG_E24A_IAR_GAIN time=" << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " target=" << target.name
              << " target_rows=" << target.functional.rows()
              << " target_rank=" << target.rank
              << " trace_F0=" << traceF0
              << " trace_WL=" << traceWideLane
              << " trace_WL_PAR=" << tracePar
              << " trace_WL_FULL=" << traceFull
              << " gamma_WL_over_F0=" << gammaWideLane
              << " std_gamma_WL_over_F0=" << std::sqrt(gammaWideLane)
              << " gamma_PAR_over_WL=" << gammaParIncremental
              << " std_gamma_PAR_over_WL="
              << std::sqrt(gammaParIncremental)
              << " gamma_PAR_over_F0=" << gammaParCumulative
              << " std_gamma_PAR_over_F0="
              << std::sqrt(gammaParCumulative)
              << " gamma_FULL_over_WL=" << gammaFullIncremental
              << " std_gamma_FULL_over_WL="
              << std::sqrt(gammaFullIncremental)
              << " gamma_FULL_over_F0=" << gammaFullCumulative
              << " std_gamma_FULL_over_F0="
              << std::sqrt(gammaFullCumulative)
              << " status=" << (valid ? "VALID" : "INVALID")
              << " feedback=SHADOW_NONE";
    }
    trace << "\nZHANG_E24A_IAR_GAIN_SUMMARY time=" << time.to_string(0)
          << " system=" << enum_to_string(system)
          << " state_dimension=" << pWideLane.rows()
          << " covariance_snapshots=F0,WL,WL_PAR,WL_FULL"
          << " covariance_representation="
             "DENSE_F0_DENSE_WL_LOW_RANK_CONDITIONAL_PAR_FULL"
          << " par_constraint_rows=" << par.inputRows
          << " par_constraint_rank=" << par.rank
          << " full_constraint_rows=" << full.inputRows
          << " full_constraint_rank=" << full.rank
          << " par_min_retained_eigenvalue="
          << par.minimumRetainedEigenvalue
          << " full_min_retained_eigenvalue="
          << full.minimumRetainedEigenvalue
          << " targets=" << targets.size()
          << " valid_targets=" << validTargets
          << " status="
          << (dimensionsValid && par.valid && full.valid &&
              validTargets == static_cast<int>(targets.size())
                ? "VALID" : "INVALID")
          << " ar_authorized=0 feedback=SHADOW_NONE";
}

/** E2: resolve common-arc wide lanes first, apply them, then resolve the L1
 * fundamental-cycle block in the WL-conditioned covariance. */
/** Compile the exact structural satellite-product lattice into the current
 * ambiguity coordinates for one signal.  Integer estimability is decided by
 * exact HNF upstream; floating-point values are used only after an exact row
 * has been proven and mapped column-for-column.  Missing current arcs reduce
 * mappableTargetRank instead of invalidating unrelated product relations. */
static ZhangProductRelationBasis compileZhangProductRelationBasis(
	const KFState& state,
	const GinAR_mtx& ambiguityState,
	E_Sys system,
	E_ObsCode observable)
{
	ZhangGraphIntegerContext context;
	ZhangProductRelationBasis result;
	result.system = system;
	result.observable = observable;
	if (!zhangGraphIntegerContext(state, system, context))
	{
		result.failureReason = "NO_GRAPH_INTEGER_CONTEXT";
		return result;
	}
	result = ProductRelationBasisBuilder::build(
		context.basis, context.productBasis, SatSys(), system, observable);
	if (!result.valid)
	{
		return result;
	}

	map<ZhangGraphEdge, int> columns;
	for (const auto& [column, key] : ambiguityState.ambmap)
	{
		if (key.Sat.sys == system
		 && static_cast<E_ObsCode>(key.num) == observable)
		{
			columns[{key.str, key.Sat}] = column;
		}
	}
	vector<VectorXd> mappedRows;
	vector<int> mappedIndices;
	for (const int namedIndex : result.independentNamedIndices)
	{
		const auto& relation = result.namedRelations.at(namedIndex);
		VectorXd row = VectorXd::Zero(ambiguityState.aflt.size());
		bool mappable = true;
		for (int chord = 0;
			 chord < static_cast<int>(result.currentChords.size()); chord++)
		{
			const ZhangExactInteger coefficient =
				relation.currentCycleCoefficients[chord];
			if (coefficient == 0)
			{
				continue;
			}
			auto column = columns.find(result.currentChords[chord]);
			if (column == columns.end())
			{
				mappable = false;
				break;
			}
			try
			{
				const long long exactCoefficient =
					coefficient.convert_to<long long>();
				if (ZhangExactInteger(exactCoefficient) != coefficient)
				{
					mappable = false;
					break;
				}
				row(column->second) = static_cast<double>(exactCoefficient);
			}
			catch (const std::exception&)
			{
				mappable = false;
				break;
			}
		}
		if (mappable)
		{
			mappedRows.push_back(std::move(row));
			mappedIndices.push_back(namedIndex);
		}
	}
	result.transform = MatrixXd::Zero(
		mappedRows.size(), ambiguityState.aflt.size());
	for (int row = 0; row < static_cast<int>(mappedRows.size()); row++)
	{
		result.transform.row(row) = mappedRows[row].transpose();
	}
	result.mappableNamedIndices = std::move(mappedIndices);
	result.mappableTargetRank = result.mappableNamedIndices.size();
	result.unmappableTargetRank = std::max(
		0, result.fullTargetRank - result.mappableTargetRank);
	result.temporalRecoveryRequired = result.unmappableTargetRank > 0;
	result.affineOffsets = ZhangExactVector(result.mappableTargetRank);
	if (result.mappableTargetRank == 0)
	{
		result.failureReason = "NO_EXACT_MAPPABLE_PRODUCT_RELATION";
	}
	return result;
}

static std::map<std::size_t, ZhangExactInteger>
recoverCertifiedNamedProductCoordinates(const GinAR_mtx& fixed, int namedCount)
{
	if (fixed.Ztrs.cols() != namedCount ||
		fixed.Ztrs.rows() != fixed.zfix.size())
	{
		return {};
	}
	ZhangExactMatrix fixedRows;
	ZhangExactVector fixedValues;
	for (int row = 0; row < fixed.Ztrs.rows(); row++)
	{
		ZhangExactVector exactRow(namedCount);
		for (int column = 0; column < namedCount; column++)
		{
			const long long rounded = std::llround(fixed.Ztrs(row, column));
			if (std::abs(fixed.Ztrs(row, column) - rounded) > 1e-8)
			{
				return {};
			}
			exactRow[column] = rounded;
		}
		const long long roundedValue = std::llround(fixed.zfix(row));
		if (std::abs(fixed.zfix(row) - roundedValue) > 1e-8)
		{
			return {};
		}
		fixedRows.push_back(std::move(exactRow));
		fixedValues.push_back(roundedValue);
	}
	return zhangRecoverCertifiedNamedProductSubset(
		fixedRows, fixedValues, namedCount);
}

static ZhangInheritedNamedCertificate
promoteNamedProductCoordinatesFromAcceptedParent(
	const GinAR_mtx& fixed,
	int namedCount,
	bool parentStatisticallyAccepted)
{
	ZhangExactMatrix fixedRows;
	ZhangExactVector fixedValues;
	if (fixed.Ztrs.cols() != namedCount ||
		fixed.Ztrs.rows() != fixed.zfix.size())
	{
		return {};
	}
	for (int row = 0; row < fixed.Ztrs.rows(); row++)
	{
		ZhangExactVector exactRow(namedCount);
		for (int column = 0; column < namedCount; column++)
		{
			const long long rounded = std::llround(fixed.Ztrs(row, column));
			if (std::abs(fixed.Ztrs(row, column) - rounded) > 1e-8)
			{
				return {};
			}
			exactRow[column] = rounded;
		}
		const long long roundedValue = std::llround(fixed.zfix(row));
		if (std::abs(fixed.zfix(row) - roundedValue) > 1e-8)
		{
			return {};
		}
		fixedRows.push_back(std::move(exactRow));
		fixedValues.push_back(roundedValue);
	}
	return zhangPromoteNamedCertificateFromAcceptedParent(
		fixedRows, fixedValues, namedCount, parentStatisticallyAccepted);
}

static std::string productSubsetCanonicalHnf(
	const ZhangProductRelationBasis& basis,
	const std::vector<int>& localIndices)
{
	ZhangExactMatrix rows;
	for (int local : localIndices)
	{
		const int namedIndex = basis.mappableNamedIndices.at(local);
		ZhangExactVector row(basis.physicalArcColumns.size());
		for (std::size_t column = 0;
			 column < basis.physicalArcColumns.size(); column++)
		{
			auto coefficient = basis.namedRelations[namedIndex]
				.physicalArcCoefficients.find(basis.physicalArcColumns[column]);
			if (coefficient != basis.namedRelations[namedIndex]
				.physicalArcCoefficients.end())
			{
				row[column] = coefficient->second;
			}
		}
		rows.push_back(std::move(row));
	}
	return zhangExactMatrixFingerprint(
		zhangExactRowHermiteNormalForm(rows).basis);
}

static int productSubsetComponentCoverageGain(
	const ZhangProductRelationBasis& firstBasis,
	const ZhangProductRelationBasis& secondBasis,
	const std::vector<int>& localIndices)
{
	const int satelliteCount = firstBasis.satellites.size();
	std::vector<int> parent(satelliteCount);
	std::iota(parent.begin(), parent.end(), 0);
	auto find = [&](int node)
	{
		int root = node;
		while (parent[root] != root) root = parent[root];
		while (parent[node] != node)
		{
			const int next = parent[node];
			parent[node] = root;
			node = next;
		}
		return root;
	};
	auto join = [&](int a, int b)
	{
		a = find(a);
		b = find(b);
		if (a == b) return false;
		parent[b] = a;
		return true;
	};
	for (int a = 0; a < satelliteCount; a++)
	for (int b = a + 1; b < satelliteCount; b++)
	{
		long long firstDifference = 0;
		long long secondDifference = 0;
		if (queryZhangSatelliteProductRelation(
				firstBasis.system, firstBasis.observable,
				firstBasis.satellites[a], firstBasis.satellites[b],
				firstDifference) &&
			queryZhangSatelliteProductRelation(
				secondBasis.system, secondBasis.observable,
				secondBasis.satellites[a], secondBasis.satellites[b],
				secondDifference))
		{
			join(a, b);
		}
	}
	int gain = 0;
	for (int local : localIndices)
	{
		const int namedIndex = firstBasis.mappableNamedIndices.at(local);
		const auto& relation = firstBasis.namedRelations.at(namedIndex);
		auto satellite = std::find(
			firstBasis.satellites.begin(), firstBasis.satellites.end(),
			relation.satellite);
		auto reference = std::find(
			firstBasis.satellites.begin(), firstBasis.satellites.end(),
			relation.referenceSatellite);
		if (satellite != firstBasis.satellites.end() &&
			reference != firstBasis.satellites.end())
		{
			gain += join(
				std::distance(firstBasis.satellites.begin(), satellite),
				std::distance(firstBasis.satellites.begin(), reference));
		}
	}
	return gain;
}

static bool zhangProductPairAuditEpoch(GTime time)
{
	if (!acsConfig.zhangPppAr.product_relation_pair_audit_shadow)
	{
		return false;
	}
	const string epoch = time.to_string(0);
	return std::find(
		acsConfig.zhangPppAr.product_relation_pair_audit_epochs.begin(),
		acsConfig.zhangPppAr.product_relation_pair_audit_epochs.end(), epoch) !=
		acsConfig.zhangPppAr.product_relation_pair_audit_epochs.end();
}

static string zhangProductPairNodeId(
	const ZhangProductRelationBasis& basis, int localNode)
{
	if (localNode == basis.mappableTargetRank)
	{
		return basis.referenceSatellite.id();
	}
	if (localNode < 0 || localNode >= basis.mappableTargetRank)
	{
		return "INVALID";
	}
	return basis.namedRelations.at(
		basis.mappableNamedIndices.at(localNode)).satellite.id();
}

struct ZhangProductPairFloatMetric
{
	int first = -1;
	int second = -1;
	double mean = 0;
	double variance = 0;
	double sigma = 0;
	double fractional = 0;
	double perr = 1;
	double productInformationGain = 0;
	int physicalArcCount = 0;
	int receiverSupportCount = 0;
	int commonReceiverSupportCount = 0;
	long long cycleL1Norm = 0;
	bool exactHeldConsequence = false;
	long long exactHeldValue = 0;
};

struct ZhangComponentQuotientInput
{
	vector<ZhangPhysicalIntegerArc> physicalColumns;
	ZhangExactMatrix targetPhysicalRows;
	ZhangExactMatrix heldPhysicalRows;
	ZhangExactVector heldValues;
	ZhangExactMatrix persistentHeldPhysicalRows;
	ZhangExactVector persistentHeldValues;
	ZhangExactMatrix currentCertifiedPhysicalRows;
	ZhangExactVector currentCertifiedValues;
	bool valid = false;
	string failureReason;
};

struct ZhangCollapsedArcVersionAuditInput
{
	ZhangExactMatrix targetRows;
	ZhangExactMatrix heldRows;
	ZhangExactVector heldValues;
	ZhangExactMatrix persistentHeldRows;
	ZhangExactVector persistentHeldValues;
	int versionedDimension = 0;
	int collapsedDimension = 0;
	bool valid = false;
};

/** Diagnostic-only projection that deliberately erases physical arc version.
 * It can identify integer information stranded on retired arc versions, but it
 * can never authorize a product certificate because cycle slips and local
 * reinitialisations are distinct physical integers. */
static ZhangCollapsedArcVersionAuditInput zhangCollapsePhysicalArcVersions(
	const ZhangComponentQuotientInput& input)
{
	ZhangCollapsedArcVersionAuditInput result;
	result.versionedDimension = input.physicalColumns.size();
	if (!input.valid || input.physicalColumns.empty()) return result;
	using CollapsedKey = std::tuple<E_ObsCode, ZhangGraphEdge>;
	std::map<CollapsedKey, int> collapsedIndex;
	std::vector<int> projection(input.physicalColumns.size());
	for (int column = 0;
		 column < static_cast<int>(input.physicalColumns.size()); column++)
	{
		const auto& arc = input.physicalColumns[column];
		const CollapsedKey key{arc.code, arc.edge};
		auto [iterator, inserted] = collapsedIndex.try_emplace(
			key, collapsedIndex.size());
		projection[column] = iterator->second;
	}
	result.collapsedDimension = collapsedIndex.size();
	auto collapse = [&](const ZhangExactMatrix& rows)
	{
		ZhangExactMatrix output = zhangExactZeroMatrix(
			rows.size(), result.collapsedDimension);
		for (int row = 0; row < static_cast<int>(rows.size()); row++)
		{
			if (rows[row].size() != input.physicalColumns.size())
				return ZhangExactMatrix{};
			for (int column = 0;
				 column < static_cast<int>(projection.size()); column++)
				output[row][projection[column]] += rows[row][column];
		}
		return output;
	};
	result.targetRows = collapse(input.targetPhysicalRows);
	result.heldRows = collapse(input.heldPhysicalRows);
	result.persistentHeldRows = collapse(input.persistentHeldPhysicalRows);
	result.heldValues = input.heldValues;
	result.persistentHeldValues = input.persistentHeldValues;
	result.valid = !result.targetRows.empty() &&
		result.targetRows.size() == input.targetPhysicalRows.size() &&
		result.heldRows.size() == input.heldPhysicalRows.size() &&
		result.persistentHeldRows.size() ==
			input.persistentHeldPhysicalRows.size();
	return result;
}

/** Convert statistically accepted rows in the current ambiguity coordinates
 * to the invariant physical-arc lattice without admitting them to the
 * persistent held ledger.  This is required when the same rows have already
 * conditioned the disposable same-epoch covariance: omitting them from the
 * exact quotient audit would manufacture unexplained zero-variance modes. */
static bool zhangCurrentCertifiedPhysicalLattice(
	const KFState& state,
	const GinAR_mtx& fixed,
	E_Sys system,
	ZhangPersistentHeldLattice& result,
	string& failureReason)
{
	ZhangGraphIntegerContext context;
	if (!zhangGraphIntegerContext(state, system, context))
	{
		failureReason = "NO_GRAPH_INTEGER_CONTEXT";
		return false;
	}
	if (fixed.Ztrs.rows() != fixed.zfix.size())
	{
		failureReason = "CURRENT_CERTIFIED_VALUE_DIMENSION_MISMATCH";
		return false;
	}
	Eigen::FullPivLU<MatrixXd> sourceRank(fixed.Ztrs);
	if (sourceRank.rank() != fixed.Ztrs.rows())
	{
		failureReason = "CURRENT_CERTIFIED_SOURCE_ROWS_NOT_INDEPENDENT";
		return false;
	}
	for (int row = 0; row < fixed.Ztrs.rows(); row++)
	{
		ZhangPersistentHeldRow physical;
		for (int column = 0; column < fixed.Ztrs.cols(); column++)
		{
			const double raw = fixed.Ztrs(row, column);
			if (std::abs(raw) < 1e-10) continue;
			const long long coefficient = std::llround(raw);
			if (std::abs(raw - static_cast<double>(coefficient)) > 1e-8)
			{
				failureReason = "CURRENT_CERTIFIED_NON_INTEGER_ROW";
				return false;
			}
			auto key = fixed.ambmap.find(column);
			if (key == fixed.ambmap.end() || key->second.Sat.sys != system ||
				!addCurrentCycleToPhysicalRow(
					context,
					static_cast<E_ObsCode>(key->second.num),
					{key->second.str, key->second.Sat},
					ZhangExactInteger(coefficient),
					physical.coefficients))
			{
				failureReason = "CURRENT_CERTIFIED_PHYSICAL_MAPPING_FAILED";
				return false;
			}
		}
		const double rawValue = fixed.zfix(row);
		const long long value = std::llround(rawValue);
		if (std::abs(rawValue - static_cast<double>(value)) > 1e-8 ||
			physical.coefficients.empty())
		{
			failureReason = "CURRENT_CERTIFIED_INVALID_AFFINE_VALUE";
			return false;
		}
		physical.value = value;
		result.rows.push_back(std::move(physical));
	}
	// Do not run a global multiprecision HNF across thousands of physical-arc
	// columns here.  These rows are an independent, statistically accepted
	// same-epoch source lattice.  Exact HNF/SNF is performed after projection
	// into each small product target component by
	// zhangExactHeldQuotientAudit().  The persistent held ledger continues to
	// use normalisePersistentHeldLattice() before admission.
	result.consistent = true;
	return true;
}

static ZhangComponentQuotientInput zhangBuildComponentQuotientInput(
	const KFState& state,
	const ZhangProductRelationBasis& basis,
	const ZhangProductRelationBasis* subtractBasis,
	const ZhangExactMatrix& targetNamedRows,
	const ZhangPersistentHeldLattice* currentCertified = nullptr)
{
	ZhangComponentQuotientInput result;
	ZhangGraphIntegerContext context;
	if (!zhangGraphIntegerContext(state, basis.system, context))
	{
		result.failureReason = "NO_GRAPH_INTEGER_CONTEXT";
		return result;
	}
	const string runtimeId = zhangAmbresRuntimeId(state);
	auto held = zhangPersistentHeldLattices.find({runtimeId, basis.system});
	if (!validZhangAmbresRuntimeId(runtimeId))
	{
		result.failureReason = "CHECKPOINT_RUNTIME_ID_UNBOUND";
		return result;
	}
	if (held != zhangPersistentHeldLattices.end() && !held->second.consistent)
	{
		result.failureReason = "INCONSISTENT_HELD_LATTICE";
		return result;
	}
	if (currentCertified && !currentCertified->consistent)
	{
		result.failureReason = "INCONSISTENT_CURRENT_CERTIFIED_LATTICE";
		return result;
	}
	set<ZhangPhysicalIntegerArc> columnSet;
	vector<map<ZhangPhysicalIntegerArc, ZhangExactInteger>> targetSparse;
	for (const auto& namedRow : targetNamedRows)
	{
		if (namedRow.size() != static_cast<size_t>(basis.mappableTargetRank))
		{
			result.failureReason = "TARGET_NAMED_ROW_DIMENSION_MISMATCH";
			return result;
		}
		map<ZhangPhysicalIntegerArc, ZhangExactInteger> sparse;
		for (int local = 0; local < basis.mappableTargetRank; local++)
		{
			if (namedRow[local] == 0) continue;
			const int namedIndex = basis.mappableNamedIndices.at(local);
			auto addPhysical = [&](const ZhangProductRelationBasis& source,
				const ZhangExactInteger& sign)
			{
				const int sourceNamedIndex = source.mappableNamedIndices.at(local);
				for (const auto& [edge, coefficient] :
					 source.namedRelations.at(sourceNamedIndex).physicalArcCoefficients)
				{
					auto version = context.arcVersions.find(edge);
					if (version == context.arcVersions.end())
					{
						result.failureReason = "TARGET_PHYSICAL_ARC_VERSION_MISSING";
						return false;
					}
					ZhangPhysicalIntegerArc arc{source.observable, edge, version->second};
					sparse[arc] += sign * namedRow[local] * coefficient;
				}
				return true;
			};
			if (!addPhysical(basis, 1) ||
				(subtractBasis && !addPhysical(*subtractBasis, -1))) return result;
		}
		for (auto iterator = sparse.begin(); iterator != sparse.end();)
		{
			if (iterator->second == 0) iterator = sparse.erase(iterator);
			else { columnSet.insert(iterator->first); ++iterator; }
		}
		targetSparse.push_back(std::move(sparse));
	}
	if (held != zhangPersistentHeldLattices.end())
	for (const auto& row : held->second.rows)
	for (const auto& [arc, coefficient] : row.coefficients)
		if (coefficient != 0) columnSet.insert(arc);
	if (currentCertified)
	for (const auto& row : currentCertified->rows)
	for (const auto& [arc, coefficient] : row.coefficients)
		if (coefficient != 0) columnSet.insert(arc);
	vector<ZhangPhysicalIntegerArc> columns(columnSet.begin(), columnSet.end());
	result.physicalColumns = columns;
	map<ZhangPhysicalIntegerArc, size_t> columnIndex;
	for (size_t column = 0; column < columns.size(); column++)
		columnIndex[columns[column]] = column;
	for (const auto& sparse : targetSparse)
	{
		ZhangExactVector row(columns.size());
		for (const auto& [arc, coefficient] : sparse)
			row[columnIndex.at(arc)] = coefficient;
		result.targetPhysicalRows.push_back(std::move(row));
	}
	if (held != zhangPersistentHeldLattices.end())
	for (const auto& heldRow : held->second.rows)
	{
		ZhangExactVector row(columns.size());
		for (const auto& [arc, coefficient] : heldRow.coefficients)
			row[columnIndex.at(arc)] = coefficient;
		result.heldPhysicalRows.push_back(std::move(row));
		result.heldValues.push_back(heldRow.value);
		result.persistentHeldPhysicalRows.push_back(result.heldPhysicalRows.back());
		result.persistentHeldValues.push_back(heldRow.value);
	}
	if (currentCertified)
	for (const auto& certifiedRow : currentCertified->rows)
	{
		ZhangExactVector row(columns.size());
		for (const auto& [arc, coefficient] : certifiedRow.coefficients)
			row[columnIndex.at(arc)] = coefficient;
		result.heldPhysicalRows.push_back(row);
		result.heldValues.push_back(certifiedRow.value);
		result.currentCertifiedPhysicalRows.push_back(std::move(row));
		result.currentCertifiedValues.push_back(certifiedRow.value);
	}
	result.valid = true;
	return result;
}

static double zhangMedian(std::vector<double> values)
{
	values.erase(std::remove_if(values.begin(), values.end(),
		[](double value) { return !std::isfinite(value); }), values.end());
	if (values.empty()) return std::numeric_limits<double>::quiet_NaN();
	std::sort(values.begin(), values.end());
	const size_t middle = values.size() / 2;
	return values.size() % 2
		? values[middle] : 0.5 * (values[middle - 1] + values[middle]);
}

static bool zhangExactIntegerMatrix(
	const MatrixXd& numeric,
	ZhangExactMatrix& exact,
	string& failureReason)
{
	exact = zhangExactZeroMatrix(numeric.rows(), numeric.cols());
	for (int row = 0; row < numeric.rows(); row++)
	for (int column = 0; column < numeric.cols(); column++)
	{
		const double raw = numeric(row, column);
		const long long rounded = std::llround(raw);
		if (!std::isfinite(raw) || std::abs(raw - rounded) > 1e-8)
		{
			failureReason = "NON_INTEGER_COORDINATE_MATRIX";
			return false;
		}
		exact[row][column] = rounded;
	}
	return true;
}

/** Independent R-Q0 audit in the current network ambiguity coordinates.
 *
 * This deliberately bypasses physical-arc expansion.  Comparing its exact
 * lattice intersection with the physical-arc result distinguishes an omitted
 * arc/version mapping from a genuinely absent product relation in the
 * accepted same-epoch integer lattice.
 */
static void traceZhangCurrentCoordinateProductLatticeAudit(
	Trace& trace,
	const ZhangProductRelationBasis& firstBasis,
	const ZhangProductRelationBasis& secondBasis,
	const ZhangExactMatrix& targetNamedRows,
	const GinAR_mtx* currentCertifiedCoordinates,
	GTime time,
	const string& auditScenario,
	int component)
{
	if (!currentCertifiedCoordinates || auditScenario != "BASELINE") return;
	string failureReason;
	ZhangExactMatrix currentRows;
	ZhangExactMatrix namedWideLaneRows;
	const MatrixXd namedWideLane = firstBasis.transform - secondBasis.transform;
	bool valid = zhangExactIntegerMatrix(
		currentCertifiedCoordinates->Ztrs, currentRows, failureReason) &&
		zhangExactIntegerMatrix(namedWideLane, namedWideLaneRows, failureReason);
	ZhangExactMatrix targetRows;
	if (valid)
	{
		for (const auto& row : targetNamedRows)
		{
			if (row.size() != namedWideLaneRows.size())
			{
				failureReason = "DIRECT_TARGET_NAMED_DIMENSION_MISMATCH";
				valid = false;
				break;
			}
			targetRows.push_back(zhangExactRowCombination(row, namedWideLaneRows));
		}
	}
	ZhangExactVector currentValues;
	if (valid)
	{
		if (currentCertifiedCoordinates->zfix.size() !=
			static_cast<int>(currentRows.size()))
		{
			failureReason = "DIRECT_CURRENT_VALUE_DIMENSION_MISMATCH";
			valid = false;
		}
		else
		for (int row = 0; row < currentCertifiedCoordinates->zfix.size(); row++)
		{
			const double raw = currentCertifiedCoordinates->zfix(row);
			const long long rounded = std::llround(raw);
			if (!std::isfinite(raw) || std::abs(raw - rounded) > 1e-8)
			{
				failureReason = "DIRECT_CURRENT_NON_INTEGER_VALUE";
				valid = false;
				break;
			}
			currentValues.push_back(rounded);
		}
	}
	ZhangHeldQuotientAudit direct;
	if (valid)
	{
		direct = zhangExactHeldQuotientAudit(
			targetRows, currentRows, currentValues);
		valid = direct.valid;
		if (!valid) failureReason = direct.failureReason;
	}
	trace << "\nZHANG_CURRENT_COORDINATE_PRODUCT_LATTICE_AUDIT time="
		  << time.to_string(0)
		  << " audit_scenario=" << auditScenario
		  << " component=" << component
		  << " target_rank=" << targetNamedRows.size()
		  << " current_fixed_rank=" << currentRows.size()
		  << " direct_intersection_rank="
		  << (valid ? direct.heldIntersectionRank : 0)
		  << " direct_quotient_rank=" << (valid ? direct.quotientRank : 0)
		  << " valid=" << valid
		  << " status=" << (valid ? "EXACT_CURRENT_COORDINATE_AUDIT" : failureReason)
		  << " feedback=0";
}

/** Frozen Star-vs-All-Pair/eigenmode audit.  The canonical star remains the
 * deterministic product coordinate, while this routine constructs every
 * e_s-e_t search edge in that ambient space.  It is diagnostics-only. */
static void traceZhangProductPairAudit(
	Trace& trace,
	const KFState& state,
	const ZhangProductRelationBasis& basis,
	const ZhangProductRelationBasis& secondBasis,
	const VectorXd& wideLaneMean,
	const MatrixXd& wideLaneCovariance,
	const GinAR_opt& options,
	GTime time,
	const string& auditScenario = "BASELINE",
	const ZhangPersistentHeldLattice* currentCertified = nullptr,
	const GinAR_mtx* currentCertifiedCoordinates = nullptr)
{
	if (!zhangProductPairAuditEpoch(time)) return;
	const int rank = basis.mappableTargetRank;
	if (rank <= 0 || wideLaneMean.size() != rank ||
		wideLaneCovariance.rows() != rank ||
		wideLaneCovariance.cols() != rank)
	{
		trace << "\nZHANG_PRODUCT_RELATION_PAIR_AUDIT_SUMMARY time="
			  << time.to_string(0) << " audit_scenario=" << auditScenario
			  << " status=INVALID_DIMENSION feedback=0";
		return;
	}
	std::vector<std::set<string>> relationReceivers(rank);
	std::vector<ZhangProductPairFloatMetric> metrics;
	std::vector<ZhangPairReliabilityEdge> reliabilityEdges;
	std::vector<double> starSigmas;
	const MatrixXd symmetric = 0.5 *
		(wideLaneCovariance + wideLaneCovariance.transpose());
	const double covarianceScale = std::max(
		1.0, symmetric.diagonal().cwiseAbs().maxCoeff());
	const double deterministicTolerance = 1e-12 * covarianceScale;
	for (int local = 0; local < rank; local++)
	{
		const auto& relation = basis.namedRelations.at(
			basis.mappableNamedIndices.at(local));
		for (const auto& [edge, coefficient] : relation.physicalArcCoefficients)
		{
			if (coefficient != 0) relationReceivers[local].insert(edge.receiver);
		}
	}
	for (int first = 0; first <= rank; first++)
	for (int second = first + 1; second <= rank; second++)
	{
		VectorXd row = VectorXd::Zero(rank);
		if (first < rank) row(first) += 1;
		if (second < rank) row(second) -= 1;
		ZhangProductPairFloatMetric metric;
		metric.first = first;
		metric.second = second;
		metric.mean = row.dot(wideLaneMean);
		metric.variance = (row.transpose() * wideLaneCovariance * row)(0, 0);
		metric.sigma = metric.variance > 0 ? std::sqrt(metric.variance) : 0;
		metric.fractional = metric.mean - std::round(metric.mean);
		metric.perr = metric.variance > 0 && std::isfinite(metric.variance)
			? round_perr(metric.fractional, metric.variance)
			: (std::abs(metric.fractional) <= 1e-10 ? 0 : 1);
		const double covarianceTrace = wideLaneCovariance.trace();
		if (metric.variance > 0 && covarianceTrace > 0)
		{
			const VectorXd cross = wideLaneCovariance * row;
			metric.productInformationGain = std::clamp(
				cross.squaredNorm() / (metric.variance * covarianceTrace),
				0.0, 1.0);
		}

		map<ZhangGraphEdge, ZhangExactInteger> physical;
		ZhangExactVector cycles(basis.currentChords.size());
		auto addRelation = [&](int node, const ZhangExactInteger& sign)
		{
			if (node >= rank) return;
			const auto& relation = basis.namedRelations.at(
				basis.mappableNamedIndices.at(node));
			for (const auto& [edge, coefficient] : relation.physicalArcCoefficients)
				physical[edge] += sign * coefficient;
			for (size_t column = 0;
				 column < relation.currentCycleCoefficients.size(); column++)
				cycles[column] += sign * relation.currentCycleCoefficients[column];
		};
		addRelation(first, 1);
		addRelation(second, -1);
		set<string> receivers;
		for (auto iterator = physical.begin(); iterator != physical.end();)
		{
			if (iterator->second == 0) iterator = physical.erase(iterator);
			else
			{
				receivers.insert(iterator->first.receiver);
				++iterator;
			}
		}
		metric.physicalArcCount = physical.size();
		metric.receiverSupportCount = receivers.size();
		if (first < rank && second < rank)
		{
			std::vector<string> common;
			std::set_intersection(
				relationReceivers[first].begin(), relationReceivers[first].end(),
				relationReceivers[second].begin(), relationReceivers[second].end(),
				std::back_inserter(common));
			metric.commonReceiverSupportCount = common.size();
		}
		else
		{
			const int target = first < rank ? first : second;
			metric.commonReceiverSupportCount = relationReceivers[target].size();
			starSigmas.push_back(metric.sigma);
		}
		for (const auto& coefficient : cycles)
			metric.cycleL1Norm += zhangExactAbs(coefficient).convert_to<long long>();
		const int targetNamedCount = basis.mappableTargetRank;
		ZhangExactVector exactNamedRow(targetNamedCount);
		if (first < targetNamedCount) exactNamedRow[first] += 1;
		if (second < targetNamedCount) exactNamedRow[second] -= 1;
		const auto exactInput = zhangBuildComponentQuotientInput(
			state, basis, &secondBasis, ZhangExactMatrix{exactNamedRow},
			currentCertified);
		if (exactInput.valid && !exactInput.targetPhysicalRows.empty())
		{
			const auto membership = zhangIntegerRowLatticeContains(
				exactInput.heldPhysicalRows, exactInput.targetPhysicalRows.front());
			metric.exactHeldConsequence = membership.contained &&
				membership.combination.size() == exactInput.heldValues.size();
			if (metric.exactHeldConsequence)
			{
				ZhangExactInteger exactValue = 0;
				for (size_t row = 0; row < membership.combination.size(); row++)
					exactValue += membership.combination[row] * exactInput.heldValues[row];
				try { metric.exactHeldValue = exactValue.convert_to<long long>(); }
				catch (...) { metric.exactHeldConsequence = false; }
			}
		}
		metrics.push_back(metric);
		reliabilityEdges.push_back({first, second, metric.perr,
			metric.variance});
	}
	std::sort(metrics.begin(), metrics.end(), [](const auto& left, const auto& right)
	{
		if (left.perr != right.perr) return left.perr < right.perr;
		if (left.variance != right.variance) return left.variance < right.variance;
		if (left.first != right.first) return left.first < right.first;
		return left.second < right.second;
	});
	for (int order = 0; order < static_cast<int>(metrics.size()); order++)
	{
		const auto& metric = metrics[order];
		trace << "\nZHANG_PRODUCT_RELATION_PAIR_FLOAT time=" << time.to_string(0)
			  << " audit_scenario=" << auditScenario
			  << " order=" << order + 1
			  << " satellite=" << zhangProductPairNodeId(basis, metric.first)
			  << " reference=" << zhangProductPairNodeId(basis, metric.second)
			  << " mean_cycles=" << metric.mean
			  << " fractional_cycles=" << metric.fractional
			  << " variance_cycles2=" << metric.variance
			  << " effective_resistance_cycles2=" << metric.variance
			  << " sigma_cycles=" << metric.sigma
			  << " round_perr=" << metric.perr
			  << " product_information_gain="
			  << metric.productInformationGain
			  << " physical_arc_count=" << metric.physicalArcCount
			  << " receiver_support_count=" << metric.receiverSupportCount
			  << " common_receiver_support_count="
			  << metric.commonReceiverSupportCount
			  << " cycle_l1_norm=" << metric.cycleL1Norm
			  << " canonical_star_edge=" << (metric.second == rank)
			  << " evidence_source="
			  << (metric.exactHeldConsequence
				? "EXACT_HELD_CONSEQUENCE"
				: metric.variance <= deterministicTolerance
					? "ZERO_VARIANCE_NUMERICAL" : "CURRENT_FLOAT")
			  << " exact_held_value=" << metric.exactHeldValue
			  << " feedback=0";
	}
	const double maximumPerr =
		acsConfig.zhangPppAr.canonical_user_target_max_perr;
	const auto forest = zhangPairReliabilityForest(
		rank + 1, reliabilityEdges, maximumPerr);
	std::vector<ZhangPairReliabilityEdge> freshReliabilityEdges;
	for (const auto& edge : reliabilityEdges)
	{
		const auto metric = std::find_if(metrics.begin(), metrics.end(),
			[&](const auto& item)
			{
				return item.first == edge.firstNode && item.second == edge.secondNode;
			});
		if (edge.variance > deterministicTolerance &&
			(metric == metrics.end() || !metric->exactHeldConsequence))
			freshReliabilityEdges.push_back(edge);
	}
	const auto freshForest = zhangPairReliabilityForest(
		rank + 1, freshReliabilityEdges, maximumPerr);
	for (int order = 0; order < static_cast<int>(forest.size()); order++)
	{
		const auto& edge = forest[order];
		trace << "\nZHANG_PRODUCT_RELATION_RELIABILITY_FOREST_EDGE time="
			  << time.to_string(0)
			  << " order=" << order + 1
			  << " satellite=" << zhangProductPairNodeId(basis, edge.firstNode)
			  << " reference=" << zhangProductPairNodeId(basis, edge.secondNode)
			  << " perr=" << edge.perr
			  << " sigma_cycles=" << std::sqrt(std::max(0.0, edge.variance))
			  << " feedback=0";
	}
	for (int order = 0; order < static_cast<int>(freshForest.size()); order++)
	{
		const auto& edge = freshForest[order];
		trace << "\nZHANG_PRODUCT_RELATION_FRESH_RELIABILITY_FOREST_EDGE time="
			  << time.to_string(0)
			  << " order=" << order + 1
			  << " satellite=" << zhangProductPairNodeId(basis, edge.firstNode)
			  << " reference=" << zhangProductPairNodeId(basis, edge.secondNode)
			  << " perr=" << edge.perr
			  << " sigma_cycles=" << std::sqrt(edge.variance)
			  << " evidence_source=CURRENT_FLOAT feedback=0";
	}
	const int bestCount = std::min(
		static_cast<int>(metrics.size()),
		acsConfig.zhangPppAr.product_relation_pair_audit_best_edge_count);
	std::vector<double> bestSigmas;
	for (int index = 0; index < bestCount; index++)
		bestSigmas.push_back(metrics[index].sigma);
	const int reliableEdgeCount = std::count_if(
		metrics.begin(), metrics.end(), [maximumPerr](const auto& metric)
		{ return metric.perr <= maximumPerr; });

	const MatrixXd allPairIncidence = zhangAllPairIncidence(rank);
	const double allPairTrace = zhangReferenceInvariantPairTrace(symmetric);
	trace << "\nZHANG_PRODUCT_RELATION_REFERENCE_INVARIANT_PAIR_GAIN time="
		  << time.to_string(0)
		  << " pair_rows=" << allPairIncidence.rows()
		  << " star_rank=" << rank
		  << " pair_covariance_trace_cycles2=" << allPairTrace
		  << " coordinate=ALL_UNORDERED_PAIR_INCIDENCE"
		  << " reference_invariant=1 feedback=0";
	for (int row = 0; row < rank; row++)
	for (int column = 0; column < rank; column++)
	{
		trace << "\nZHANG_PRODUCT_RELATION_WL_COVARIANCE time="
			  << time.to_string(0)
			  << " row=" << row
			  << " row_satellite=" << zhangProductPairNodeId(basis, row)
			  << " column=" << column
			  << " column_satellite=" << zhangProductPairNodeId(basis, column)
			  << " covariance_cycles2=" << symmetric(row, column)
			  << " complete_matrix=1 feedback=0";
	}

	// E2: every connected component of the fresh reliable graph is tested as
	// one correlated named tree.  Marginal edge screening creates candidates;
	// only joint LAMBDA/NIS can certify the complete component.
	std::vector<std::vector<int>> adjacency(rank + 1);
	for (const auto& edge : freshForest)
	{
		adjacency[edge.firstNode].push_back(edge.secondNode);
		adjacency[edge.secondNode].push_back(edge.firstNode);
	}
	std::vector<int> component(rank + 1, -1);
	std::vector<std::vector<int>> components;
	for (int node = 0; node <= rank; node++)
	{
		if (component[node] >= 0) continue;
		const int id = components.size();
		components.push_back({});
		std::vector<int> stack = {node};
		component[node] = id;
		while (!stack.empty())
		{
			const int current = stack.back(); stack.pop_back();
			components[id].push_back(current);
			for (int neighbour : adjacency[current])
			{
				if (component[neighbour] < 0)
				{
					component[neighbour] = id;
					stack.push_back(neighbour);
				}
			}
		}
	}
	std::vector<MatrixXd> certifiedRows(components.size());
	std::vector<VectorXd> certifiedValues(components.size());
	std::vector<ZhangExactMatrix> certifiedExactNamedRows(components.size());
	std::vector<ZhangExactVector> certifiedExactValues(components.size());
	std::vector<bool> certified(components.size(), false);
	std::vector<std::map<int, double>> componentPotentials(components.size());
	for (int id = 0; id < static_cast<int>(components.size()); id++)
	{
		const auto& nodes = components[id];
		if (nodes.size() < 2)
		{
			trace << "\nZHANG_PRODUCT_COMPONENT_JOINT_IAR time=" << time.to_string(0)
				  << " component=" << id << " size=1 target_rank=0 fixed_rank=0"
				  << " certified=0 status=ISOLATED_SINGLETON feedback=0";
			continue;
		}
		std::vector<ZhangPairReliabilityEdge> tree;
		for (const auto& edge : freshForest)
			if (component[edge.firstNode] == id && component[edge.secondNode] == id)
				tree.push_back(edge);
		MatrixXd rows = MatrixXd::Zero(tree.size(), rank);
		ZhangExactMatrix exactNamedRows;
		for (int row = 0; row < static_cast<int>(tree.size()); row++)
		{
			if (tree[row].firstNode < rank) rows(row, tree[row].firstNode) += 1;
			if (tree[row].secondNode < rank) rows(row, tree[row].secondNode) -= 1;
			ZhangExactVector exactRow(rank);
			if (tree[row].firstNode < rank) exactRow[tree[row].firstNode] += 1;
			if (tree[row].secondNode < rank) exactRow[tree[row].secondNode] -= 1;
			exactNamedRows.push_back(std::move(exactRow));
		}
		const auto quotientInput = zhangBuildComponentQuotientInput(
			state, basis, &secondBasis, exactNamedRows, currentCertified);
		traceZhangCurrentCoordinateProductLatticeAudit(
			trace, basis, secondBasis, exactNamedRows,
			currentCertifiedCoordinates, time, auditScenario, id);
		const auto quotient = quotientInput.valid
			? zhangExactHeldQuotientAudit(
				quotientInput.targetPhysicalRows,
				quotientInput.heldPhysicalRows,
				quotientInput.heldValues)
			: ZhangHeldQuotientAudit{};
		const auto persistentQuotient = quotientInput.valid
			? zhangExactHeldQuotientAudit(
				quotientInput.targetPhysicalRows,
				quotientInput.persistentHeldPhysicalRows,
				quotientInput.persistentHeldValues)
			: ZhangHeldQuotientAudit{};
		// Diagnostic R-Q0 control: erase only the physical arc-version label and
		// repeat the exact lattice audit.  A positive intersection here, paired
		// with a zero intersection above, locates certified information on a
		// retired arc version.  It must not be used for certification because a
		// cycle slip creates a genuinely different physical integer.
		const auto collapsedInput = zhangCollapsePhysicalArcVersions(quotientInput);
		const auto collapsedQuotient = collapsedInput.valid
			? zhangExactHeldQuotientAudit(
				collapsedInput.targetRows,
				collapsedInput.heldRows,
				collapsedInput.heldValues)
			: ZhangHeldQuotientAudit{};
		const auto collapsedPersistentQuotient = collapsedInput.valid
			? zhangExactHeldQuotientAudit(
				collapsedInput.targetRows,
				collapsedInput.persistentHeldRows,
				collapsedInput.persistentHeldValues)
			: ZhangHeldQuotientAudit{};
		trace << "\nZHANG_ARC_VERSION_COLLAPSED_PRODUCT_LATTICE_AUDIT time="
			  << time.to_string(0)
			  << " audit_scenario=" << auditScenario
			  << " component=" << id
			  << " target_rank=" << tree.size()
			  << " versioned_dimension=" << collapsedInput.versionedDimension
			  << " collapsed_dimension=" << collapsedInput.collapsedDimension
			  << " collapsed_intersection_rank="
			  << (collapsedQuotient.valid
				? collapsedQuotient.heldIntersectionRank : 0)
			  << " persistent_collapsed_intersection_rank="
			  << (collapsedPersistentQuotient.valid
				? collapsedPersistentQuotient.heldIntersectionRank : 0)
			  << " collapsed_quotient_rank="
			  << (collapsedQuotient.valid ? collapsedQuotient.quotientRank : 0)
			  << " valid="
			  << (collapsedInput.valid && collapsedQuotient.valid &&
				collapsedPersistentQuotient.valid)
			  << " status=DIAGNOSTIC_ONLY"
			  << " certificate_authorized=0 feedback=0";
		const int persistentIntersectionRank = persistentQuotient.valid
			? persistentQuotient.heldIntersectionRank : 0;
		const int currentCertifiedIncrementRank = quotient.valid
			? std::max(0, quotient.heldIntersectionRank - persistentIntersectionRank)
			: 0;
		if (!quotientInput.valid || !quotient.valid)
		{
			trace << "\nZHANG_PRODUCT_COMPONENT_QUOTIENT_IAR time="
				  << time.to_string(0) << " component=" << id
				  << " audit_scenario=" << auditScenario
				  << " size=" << nodes.size()
				  << " target_rank=" << tree.size()
				  << " persistent_held_intersection_rank=0"
				  << " current_certified_increment_rank=0"
				  << " held_rank=0 quotient_rank=0 quotient_covariance_rank=0"
				  << " newly_fixed_rank=0 combined_certified_rank=0"
				  << " certified=0 status=QUOTIENT_AUDIT_REJECTED reason="
				  << (quotientInput.valid ? quotient.failureReason
					: quotientInput.failureReason) << " feedback=0";
			continue;
		}
		MatrixXd quotientCoordinates = MatrixXd::Zero(
			quotient.quotientRank, quotient.targetRank);
		for (int row = 0; row < quotient.quotientRank; row++)
		for (int column = 0; column < quotient.targetRank; column++)
			quotientCoordinates(row, column) =
				quotient.quotientTargetCoordinates[row][column].convert_to<double>();
		const MatrixXd quotientRows = quotientCoordinates * rows;
		GinAR_mtx joint;
		joint.aflt = quotientRows * wideLaneMean;
		joint.Paflt = quotientRows * symmetric * quotientRows.transpose();
		// R-Q4: finite integer-constrained gain frontier.  Small quotients use the
		// dense coefficient box.  Larger quotients use a declared sparse-support
		// box together with all named pair rows and LAMBDA decorrelation rows, so
		// the audit remains finite without silently skipping the 12-dimensional
		// component.  No finite-dictionary result is reported as global G_k^Z.
		if (quotient.quotientRank > 0 && quotient.quotientRank <= 16)
		{
			const MatrixXd productQuotientCrossCovariance =
				zhangAllPairIncidence(rank) * symmetric *
				quotientRows.transpose();
			const double fullPairVariance = zhangReferenceInvariantPairTrace(symmetric);
			ZhangExactMatrix quotientNamedRows;
			for (const auto& coordinates : quotient.quotientTargetCoordinates)
				quotientNamedRows.push_back(zhangExactRowCombination(
					coordinates, exactNamedRows));
			ZhangExactMatrix completedNamedRows;
			for (const auto& coordinates :
				 quotient.heldIntersectionTargetCoordinates)
				completedNamedRows.push_back(zhangExactRowCombination(
					coordinates, exactNamedRows));
			completedNamedRows.insert(completedNamedRows.end(),
				quotientNamedRows.begin(), quotientNamedRows.end());
			ZhangExactMatrix allPairSeeds;
			for (int first = 0; first <= rank; first++)
			for (int second = first + 1; second <= rank; second++)
			{
				ZhangExactVector pairRow(rank);
				if (first < rank) pairRow[first] += 1;
				if (second < rank) pairRow[second] -= 1;
				const auto membership = zhangIntegerRowLatticeContains(
					completedNamedRows, pairRow);
				if (membership.contained &&
					membership.combination.size() ==
						completedNamedRows.size())
				{
					ZhangExactVector quotientSeed(
						membership.combination.begin() +
							quotient.heldIntersectionRank,
						membership.combination.end());
					if (std::any_of(quotientSeed.begin(), quotientSeed.end(),
						[](const auto& value) { return value != 0; }))
						allPairSeeds.push_back(std::move(quotientSeed));
				}
			}
			GinAR_mtx reducedQuotient = joint;
			ZhangExactMatrix lambdaReducedSeeds;
			const bool lambdaReductionValid =
				Ztrans_reduction(trace, reducedQuotient) >= 0;
			if (lambdaReductionValid)
			for (int row = 0; row < reducedQuotient.Ztrs.rows(); row++)
			{
				ZhangExactVector exactRow(quotient.quotientRank);
				bool exact = true;
				for (int column = 0; column < reducedQuotient.Ztrs.cols(); column++)
				{
					const long long value = std::llround(
						reducedQuotient.Ztrs(row, column));
					exact &= std::abs(reducedQuotient.Ztrs(row, column) - value) <= 1e-8;
					exactRow[column] = value;
				}
				if (exact) lambdaReducedSeeds.push_back(std::move(exactRow));
			}
			ZhangExactMatrix explicitSeeds = allPairSeeds;
			explicitSeeds.insert(explicitSeeds.end(),
				lambdaReducedSeeds.begin(), lambdaReducedSeeds.end());
			const int maximumEnumerationSupport =
				quotient.quotientRank <= 7 ? quotient.quotientRank : 2;
			trace << "\nZHANG_INTEGER_PRODUCT_GAIN_CANDIDATE_DICTIONARY time="
				  << time.to_string(0)
				  << " audit_scenario=" << auditScenario
				  << " component=" << id
				  << " quotient_rank=" << quotient.quotientRank
				  << " all_pair_quotient_rows=" << allPairSeeds.size()
				  << " lambda_reduced_rows=" << lambdaReducedSeeds.size()
				  << " lambda_reduction_valid=" << lambdaReductionValid
				  << " enumeration_scope="
				  << (maximumEnumerationSupport == quotient.quotientRank
					? "DENSE_COEFFICIENT_BOX" : "SPARSE_SUPPORT_BOX")
				  << " maximum_support=" << maximumEnumerationSupport
				  << " component_gauge_candidates=R_Q3_CROSS_COMPONENT"
				  << " feedback=0";
			for (int coefficientBound : {2, 3})
			{
				const auto frontier = zhangBoundedIntegerProductGainFrontier(
					joint.aflt,
					joint.Paflt,
					productQuotientCrossCovariance,
					coefficientBound,
					maximumPerr,
					std::max(1e-9, options.lambda_candidate_nis_alpha),
					std::min(quotient.quotientRank, 7),
					128,
					fullPairVariance,
					explicitSeeds,
					maximumEnumerationSupport);
				trace << "\nZHANG_INTEGER_PRODUCT_GAIN_FRONTIER_SUMMARY time="
					  << time.to_string(0)
					  << " audit_scenario=" << auditScenario
					  << " component=" << id
					  << " quotient_rank=" << quotient.quotientRank
					  << " coefficient_bound=" << coefficientBound
					  << " maximum_support="
					  << frontier.maximumEnumerationSupport
					  << " primitive_rows_enumerated="
					  << frontier.enumeratedPrimitiveRows
					  << " reliable_primitive_rows="
					  << frontier.reliablePrimitiveRows
					  << " explicit_seed_rows_added="
					  << frontier.explicitSeedRowsAdded
					  << " reliable_explicit_seed_rows_added="
					  << frontier.reliableExplicitSeedRows
					  << " product_variance=" << frontier.totalProductVariance
					  << " status=" << frontier.status
					  << " feedback=0";
				for (const auto& point : frontier.points)
				{
					trace << "\nZHANG_INTEGER_PRODUCT_GAIN_FRONTIER_POINT time="
						  << time.to_string(0)
						  << " audit_scenario=" << auditScenario
						  << " component=" << id
						  << " quotient_rank=" << quotient.quotientRank
						  << " coefficient_bound=" << coefficientBound
						  << " fixed_rank=" << point.rank
						  << " gain=" << point.gain
						  << " gain_fraction=" << point.gainFraction
						  << " failure_probability_bound="
						  << point.failureProbabilityBound
						  << " joint_nis=" << point.jointNis
						  << " joint_nis_threshold=" << point.jointNisThreshold
						  << " optimum_scope="
						  << (point.exactBoundedOptimum
							? "EXACT_WITHIN_FINITE_CANDIDATE_DICTIONARY"
							: "RELIABLE_BEAM_LOWER_BOUND")
						  << " canonical_hnf="
						  << zhangExactMatrixFingerprint(
							zhangExactRowHermiteNormalForm(point.rows).basis)
						  << " feedback=0";
				}
			}
		}
		const auto deterministicAudit = zhangAuditDeterministicQuotientModes(
			joint.aflt, joint.Paflt);
		const int quotientCovarianceRank = deterministicAudit.covarianceRank;
		if (quotientCovarianceRank != quotient.quotientRank)
		{
			trace << "\nZHANG_PRODUCT_COMPONENT_QUOTIENT_IAR time="
				  << time.to_string(0) << " component=" << id
				  << " audit_scenario=" << auditScenario
				  << " size=" << nodes.size()
				  << " target_rank=" << quotient.targetRank
				  << " held_rank=" << quotient.heldIntersectionRank
				  << " persistent_held_intersection_rank="
				  << persistentIntersectionRank
				  << " current_certified_increment_rank="
				  << currentCertifiedIncrementRank
				  << " quotient_rank=" << quotient.quotientRank
				  << " quotient_covariance_rank=" << quotientCovarianceRank
				  << " newly_fixed_rank=0 combined_certified_rank="
				  << quotient.heldIntersectionRank
				  << " certified=0 status=" << deterministicAudit.status
				  << " deterministic_nullity=" << deterministicAudit.nullity
				  << " maximum_null_fractional_integer="
				  << deterministicAudit.maximumNullFractionalInteger
				  << " reason=QUOTIENT_COVARIANCE_RANK_DEFICIENT feedback=0";
			continue;
		}
		GinAR_opt jointOptions = options;
		jointOptions.min_lambda_fix_count = 1;
		const int fixed = quotient.quotientRank > 0 ? rankAwareGnssAr(
			trace, joint, jointOptions, time,
			"PRODUCT_COMPONENT_QUOTIENT_WL_SHADOW", true) : 0;
		const bool statisticallyAccepted = quotient.quotientRank == 0 ||
			(fixed > 0 && joint.lambda_candidate_nis_valid &&
			 joint.lambda_candidate_nis <= joint.lambda_candidate_nis_threshold);
		ZhangExactMatrix newPhysicalRows;
		ZhangExactMatrix newNamedRows;
		ZhangExactVector newValues;
		if (statisticallyAccepted && fixed > 0 &&
			joint.Ztrs.cols() == quotient.quotientRank &&
			joint.Ztrs.rows() == joint.zfix.size())
		{
			for (int fixedRow = 0; fixedRow < joint.Ztrs.rows(); fixedRow++)
			{
				ZhangExactVector quotientCombination(quotient.quotientRank);
				for (int column = 0; column < quotient.quotientRank; column++)
				{
					const long long rounded = std::llround(joint.Ztrs(fixedRow, column));
					if (std::abs(joint.Ztrs(fixedRow, column) - rounded) > 1e-8)
					{
						newPhysicalRows.clear(); newNamedRows.clear(); newValues.clear();
						break;
					}
					quotientCombination[column] = rounded;
				}
				if (quotientCombination.empty()) break;
				ZhangExactVector targetCombination(quotient.targetRank);
				for (int q = 0; q < quotient.quotientRank; q++)
				for (int target = 0; target < quotient.targetRank; target++)
					targetCombination[target] += quotientCombination[q] *
						quotient.quotientTargetCoordinates[q][target];
				newPhysicalRows.push_back(zhangExactRowCombination(
					targetCombination, quotientInput.targetPhysicalRows));
				newNamedRows.push_back(zhangExactRowCombination(
					targetCombination, exactNamedRows));
				newValues.push_back(std::llround(joint.zfix(fixedRow)));
			}
		}
		const auto unionAudit = zhangExactCertifiedUnionAudit(
			quotientInput.targetPhysicalRows,
			quotient.heldIntersectionPhysicalBasis,
			quotient.heldIntersectionValues,
			newPhysicalRows, newValues);
		const bool accepted = statisticallyAccepted && unionAudit.exactTargetEquality;
		trace << "\nZHANG_PRODUCT_COMPONENT_QUOTIENT_IAR time=" << time.to_string(0)
			  << " audit_scenario=" << auditScenario
			  << " component=" << id
			  << " size=" << nodes.size()
			  << " target_rank=" << quotient.targetRank
			  << " held_rank=" << quotient.heldIntersectionRank
			  << " persistent_held_intersection_rank="
			  << persistentIntersectionRank
			  << " current_certified_increment_rank="
			  << currentCertifiedIncrementRank
			  << " quotient_rank=" << quotient.quotientRank
			  << " quotient_covariance_rank=" << quotientCovarianceRank
			  << " newly_fixed_rank=" << unionAudit.newlyFixedRank
			  << " combined_certified_rank=" << unionAudit.combinedCertifiedRank
			  << " nis=" << joint.lambda_candidate_nis
			  << " nis_threshold=" << joint.lambda_candidate_nis_threshold
			  << " certified=" << accepted
			  << " status=" << (accepted ? "CERTIFIED" :
				(statisticallyAccepted ? unionAudit.failureReason : "QUOTIENT_IAR_REJECTED"))
			  << " feedback=0";
		const int combinedRank = unionAudit.combinedCertifiedRank;
		certifiedRows[id] = MatrixXd::Zero(combinedRank, rank);
		certifiedValues[id] = VectorXd::Zero(combinedRank);
		for (int row = 0; row < quotient.heldIntersectionRank; row++)
		{
			const auto named = zhangExactRowCombination(
				quotient.heldIntersectionTargetCoordinates[row], exactNamedRows);
			certifiedExactNamedRows[id].push_back(named);
			certifiedExactValues[id].push_back(quotient.heldIntersectionValues[row]);
			for (int column = 0; column < rank; column++)
				certifiedRows[id](row, column) = named[column].convert_to<double>();
			certifiedValues[id](row) = quotient.heldIntersectionValues[row].convert_to<double>();
		}
		for (int row = 0; row < static_cast<int>(newNamedRows.size()); row++)
		{
			certifiedExactNamedRows[id].push_back(newNamedRows[row]);
			certifiedExactValues[id].push_back(newValues[row]);
			for (int column = 0; column < rank; column++)
				certifiedRows[id](quotient.heldIntersectionRank + row, column) =
					newNamedRows[row][column].convert_to<double>();
			certifiedValues[id](quotient.heldIntersectionRank + row) =
				newValues[row].convert_to<double>();
		}
		certified[id] = accepted;
	}

	// R-Q2: formal components come from exact pair rows contained in the
	// certified union, never from the marginal-Perr candidate graph.
	ZhangExactMatrix globalCertifiedNamedRows;
	ZhangExactVector globalCertifiedValues;
	for (int id = 0; id < static_cast<int>(components.size()); id++)
	{
		globalCertifiedNamedRows.insert(globalCertifiedNamedRows.end(),
			certifiedExactNamedRows[id].begin(), certifiedExactNamedRows[id].end());
		globalCertifiedValues.insert(globalCertifiedValues.end(),
			certifiedExactValues[id].begin(), certifiedExactValues[id].end());
	}
	std::vector<ZhangPairReliabilityEdge> certifiedPairEdges;
	std::map<std::pair<int, int>, double> certifiedPairValues;
	for (int first = 0; first <= rank; first++)
	for (int second = first + 1; second <= rank; second++)
	{
		ZhangExactVector pairRow(rank);
		if (first < rank) pairRow[first] += 1;
		if (second < rank) pairRow[second] -= 1;
		const auto membership = zhangIntegerRowLatticeContains(
			globalCertifiedNamedRows, pairRow);
		if (!membership.contained ||
			membership.combination.size() != globalCertifiedValues.size()) continue;
		ZhangExactInteger value = 0;
		for (size_t row = 0; row < membership.combination.size(); row++)
			value += membership.combination[row] * globalCertifiedValues[row];
		certifiedPairEdges.push_back({first, second, 0, 0});
		certifiedPairValues[{first, second}] = value.convert_to<double>();
		trace << "\nZHANG_ACTUAL_CERTIFIED_PRODUCT_EDGE time="
			  << time.to_string(0)
			  << " audit_scenario=" << auditScenario
			  << " satellite=" << zhangProductPairNodeId(basis, first)
			  << " reference=" << zhangProductPairNodeId(basis, second)
			  << " integer_value=" << value
			  << " exact_hnf_membership=1 evidence_source=EXACT_CERTIFIED_DERIVED"
			  << " feedback=0";
	}
	const auto actualCertifiedForest = zhangPairReliabilityForest(
		rank + 1, certifiedPairEdges, 0);
	std::vector<std::vector<int>> certifiedAdjacency(rank + 1);
	for (const auto& edge : actualCertifiedForest)
	{
		certifiedAdjacency[edge.firstNode].push_back(edge.secondNode);
		certifiedAdjacency[edge.secondNode].push_back(edge.firstNode);
	}
	std::vector<int> actualComponent(rank + 1, -1);
	std::vector<std::vector<int>> actualComponents;
	std::vector<std::map<int, double>> actualPotentials;
	for (int node = 0; node <= rank; node++)
	{
		if (actualComponent[node] >= 0) continue;
		const int id = actualComponents.size();
		actualComponents.push_back({});
		actualPotentials.push_back({{node, 0}});
		std::vector<int> stack = {node};
		actualComponent[node] = id;
		while (!stack.empty())
		{
			const int current = stack.back(); stack.pop_back();
			actualComponents[id].push_back(current);
			for (int neighbour : certifiedAdjacency[current])
			{
				const int first = std::min(current, neighbour);
				const int second = std::max(current, neighbour);
				const double canonicalValue = certifiedPairValues.at({first, second});
				const double directedValue = current == first
					? canonicalValue : -canonicalValue;
				if (!actualPotentials[id].contains(neighbour))
					actualPotentials[id][neighbour] =
						actualPotentials[id].at(current) - directedValue;
				if (actualComponent[neighbour] < 0)
				{
					actualComponent[neighbour] = id;
					stack.push_back(neighbour);
				}
			}
		}
	}
	trace << "\nZHANG_ACTUAL_CERTIFIED_PRODUCT_GRAPH_SUMMARY time="
		  << time.to_string(0)
		  << " audit_scenario=" << auditScenario
		  << " certified_integer_rank="
		  << zhangExactRowHermiteNormalForm(globalCertifiedNamedRows).basis.size()
		  << " certified_pair_edges=" << certifiedPairEdges.size()
		  << " certified_pair_graph_rank=" << actualCertifiedForest.size()
		  << " certified_components=" << actualComponents.size()
		  << " candidate_graph_rank=" << freshForest.size()
		  << " component_source=EXACT_CERTIFIED_PAIR_MEMBERSHIP feedback=0";

	MatrixXd combinedRows(0, rank);
	VectorXd combinedValues(0);
	int combinedCount = 0;
	for (int id = 0; id < static_cast<int>(components.size()); id++)
		combinedCount += certifiedRows[id].rows();
	if (combinedCount > 0)
	{
		combinedRows.resize(combinedCount, rank);
		combinedValues.resize(combinedCount);
		int offset = 0;
		for (int id = 0; id < static_cast<int>(components.size()); id++)
		if (certifiedRows[id].rows() > 0)
		{
			combinedRows.middleRows(offset, certifiedRows[id].rows()) = certifiedRows[id];
			combinedValues.segment(offset, certifiedValues[id].size()) = certifiedValues[id];
			offset += certifiedRows[id].rows();
		}
	}
	const auto conditioned = zhangConditionExactProductRows(
		wideLaneMean, symmetric, combinedRows, combinedValues);
	// R-Q3: use every correlated cross edge between components of the actual
	// exact certified graph to estimate the complete datum-free component-gauge
	// vector jointly.  Pairwise GLS records below remain useful diagnostics, but
	// they are not a substitute for y=D_C*c+e with c in Z^(K-1).
	if (conditioned.valid && actualComponents.size() > 1)
	{
		std::vector<VectorXd> jointRows;
		std::vector<VectorXd> jointDesignRows;
		std::vector<double> jointMeasurements;
		const int gaugeDimension = actualComponents.size() - 1;
		for (int firstComponent = 0;
			 firstComponent < static_cast<int>(actualComponents.size());
			 firstComponent++)
		for (int secondComponent = firstComponent + 1;
			 secondComponent < static_cast<int>(actualComponents.size());
			 secondComponent++)
		for (int firstNode : actualComponents[firstComponent])
		for (int secondNode : actualComponents[secondComponent])
		{
			VectorXd row = VectorXd::Zero(rank);
			if (firstNode < rank) row(firstNode) += 1;
			if (secondNode < rank) row(secondNode) -= 1;
			VectorXd designRow = VectorXd::Zero(gaugeDimension);
			if (firstComponent > 0) designRow(firstComponent - 1) += 1;
			if (secondComponent > 0) designRow(secondComponent - 1) -= 1;
			const double firstPotential =
				actualPotentials[firstComponent].at(firstNode);
			const double secondPotential =
				actualPotentials[secondComponent].at(secondNode);
			jointRows.push_back(row);
			jointDesignRows.push_back(designRow);
			jointMeasurements.push_back(row.dot(conditioned.mean) -
				(firstPotential - secondPotential));
		}
		MatrixXd observationRows(jointRows.size(), rank);
		MatrixXd componentDesign(jointRows.size(), gaugeDimension);
		for (int row = 0; row < static_cast<int>(jointRows.size()); row++)
		{
			observationRows.row(row) = jointRows[row].transpose();
			componentDesign.row(row) = jointDesignRows[row].transpose();
		}
		const VectorXd observations = Eigen::Map<VectorXd>(
			jointMeasurements.data(), jointMeasurements.size());
		const MatrixXd observationCovariance = observationRows *
			conditioned.covariance * observationRows.transpose();
		const auto gauge = zhangComponentGaugeGls(
			observations, observationCovariance, componentDesign);
		GinAR_mtx gaugeInteger;
		int fixedGaugeRank = 0;
		bool statisticallyAccepted = false;
		ZhangExactMatrix fixedGaugeNamedRows;
		ZhangExactVector fixedGaugeValues;
		if (gauge.valid)
		{
			gaugeInteger.aflt = gauge.mean;
			gaugeInteger.Paflt = gauge.covariance;
			GinAR_opt gaugeOptions = options;
			gaugeOptions.min_lambda_fix_count = 1;
			fixedGaugeRank = rankAwareGnssAr(
				trace, gaugeInteger, gaugeOptions, time,
				"PRODUCT_COMPONENT_GAUGE_JOINT_SHADOW", true);
			statisticallyAccepted = fixedGaugeRank > 0 &&
				gaugeInteger.lambda_candidate_nis_valid &&
				gaugeInteger.lambda_candidate_nis <=
					gaugeInteger.lambda_candidate_nis_threshold;
			if (statisticallyAccepted &&
				gaugeInteger.Ztrs.cols() == gaugeDimension &&
				gaugeInteger.Ztrs.rows() == gaugeInteger.zfix.size())
			{
				std::vector<int> componentAnchors(actualComponents.size());
				ZhangExactVector componentAnchorPotentials(actualComponents.size());
				for (int component = 0;
					 component < static_cast<int>(actualComponents.size()); component++)
				{
					componentAnchors[component] = actualComponents[component].front();
					const double rawPotential = actualPotentials[component].at(
						componentAnchors[component]);
					const long long potential = std::llround(rawPotential);
					if (std::abs(rawPotential - potential) > 1e-8)
					{
						componentAnchors.clear();
						break;
					}
					componentAnchorPotentials[component] = potential;
				}
				if (!componentAnchors.empty())
				for (int fixedRow = 0;
					 fixedRow < gaugeInteger.Ztrs.rows(); fixedRow++)
				{
					ZhangExactVector gaugeCombination(gaugeDimension);
					bool exact = true;
					for (int gaugeColumn = 0;
						 gaugeColumn < gaugeDimension; gaugeColumn++)
					{
						const double raw = gaugeInteger.Ztrs(
							fixedRow, gaugeColumn);
						const long long coefficient = std::llround(raw);
						if (std::abs(raw - coefficient) > 1e-8)
						{
							exact = false;
							break;
						}
						gaugeCombination[gaugeColumn] = coefficient;
					}
					const double rawValue = gaugeInteger.zfix(fixedRow);
					const long long value = std::llround(rawValue);
					exact &= std::abs(rawValue - value) <= 1e-8;
					const auto mapped = exact ? zhangComponentGaugeToProductRow(
						gaugeCombination, componentAnchors,
						componentAnchorPotentials, rank, value)
						: ZhangComponentGaugeProductRow{};
					if (!mapped.valid)
					{
						fixedGaugeNamedRows.clear();
						fixedGaugeValues.clear();
						break;
					}
					fixedGaugeNamedRows.push_back(mapped.row);
					fixedGaugeValues.push_back(mapped.value);
				}
			}
		}
		const ZhangExactMatrix fullProductTarget =
			zhangExactIdentityMatrix(rank);
		const auto gaugeUnion = zhangExactCertifiedUnionAudit(
			fullProductTarget,
			globalCertifiedNamedRows,
			globalCertifiedValues,
			fixedGaugeNamedRows,
			fixedGaugeValues);
		std::vector<ZhangPairReliabilityEdge> mergedPairEdges;
		if (gaugeUnion.consistent && gaugeUnion.certifiedContainedInTarget)
		for (int first = 0; first <= rank; first++)
		for (int second = first + 1; second <= rank; second++)
		{
			ZhangExactVector pairRow(rank);
			if (first < rank) pairRow[first] += 1;
			if (second < rank) pairRow[second] -= 1;
			if (zhangIntegerRowLatticeContains(
				gaugeUnion.certifiedBasis, pairRow).contained)
				mergedPairEdges.push_back({first, second, 0, 0});
		}
		const auto mergedForest = zhangPairReliabilityForest(
			rank + 1, mergedPairEdges, 0);
		const int mergedComponentCount = rank + 1 - mergedForest.size();
		const int preBridgeCertifiedRank =
			zhangExactRowHermiteNormalForm(globalCertifiedNamedRows).basis.size();
		trace << "\nZHANG_PRODUCT_COMPONENT_GAUGE_JOINT_IAR time="
			  << time.to_string(0)
			  << " audit_scenario=" << auditScenario
			  << " certified_components=" << actualComponents.size()
			  << " datum_component=0"
			  << " gauge_target_rank=" << gaugeDimension
			  << " cross_edges=" << jointRows.size()
			  << " measurement_rank=" << gauge.measurementRank
			  << " estimable_gauge_rank=" << gauge.gaugeRank
			  << " newly_fixed_gauge_rank=" << fixedGaugeRank
			  << " exact_gauge_rows=" << fixedGaugeNamedRows.size()
			  << " pre_bridge_certified_rank="
			  << preBridgeCertifiedRank
			  << " combined_certified_rank="
			  << gaugeUnion.combinedCertifiedRank
			  << " merged_pair_edges=" << mergedPairEdges.size()
			  << " merged_pair_graph_rank=" << mergedForest.size()
			  << " merged_components=" << mergedComponentCount
			  << " exact_union_consistent=" << gaugeUnion.consistent
			  << " exact_rows_inside_product_lattice="
			  << gaugeUnion.certifiedContainedInTarget
			  << " residual_nis=" << gauge.residualNis
			  << " maximum_null_residual=" << gauge.maximumNullResidual
			  << " lambda_nis=" << gaugeInteger.lambda_candidate_nis
			  << " lambda_nis_threshold="
			  << gaugeInteger.lambda_candidate_nis_threshold
			  << " statistically_accepted=" << statisticallyAccepted
			  << " valid=" << gauge.valid
			  << " status=" << (gauge.valid
				? (statisticallyAccepted && gaugeUnion.consistent &&
					gaugeUnion.certifiedContainedInTarget
					? "JOINT_GAUGE_IAR_ACCEPTED_EXACT_UNION"
					: "JOINT_GAUGE_IAR_REJECTED")
				: "JOINT_GAUGE_NOT_ESTIMABLE")
			  << " certificate_role=SHADOW_CANDIDATE"
			  << " merge_authorized=0 feedback=0";
	}
	// Retain pairwise aggregated bridge records as explainable controls for
	// individual component pairs.  Formal R-Q3 rank comes from the joint system
	// above, not from counting accepted pairwise controls.
	if (conditioned.valid)
	for (int firstComponent = 0;
		 firstComponent < static_cast<int>(actualComponents.size()); firstComponent++)
	for (int secondComponent = firstComponent + 1;
		 secondComponent < static_cast<int>(actualComponents.size()); secondComponent++)
	{
		std::vector<VectorXd> crossRows;
		std::vector<double> adjusted;
		for (int firstNode : actualComponents[firstComponent])
		for (int secondNode : actualComponents[secondComponent])
		{
			VectorXd row = VectorXd::Zero(rank);
			if (firstNode < rank) row(firstNode) += 1;
			if (secondNode < rank) row(secondNode) -= 1;
			crossRows.push_back(row);
			const double firstPotential = actualPotentials[firstComponent].at(firstNode);
			const double secondPotential = actualPotentials[secondComponent].at(secondNode);
			adjusted.push_back(row.dot(conditioned.mean) -
				(firstPotential - secondPotential));
		}
		MatrixXd cross(crossRows.size(), rank);
		for (int row = 0; row < static_cast<int>(crossRows.size()); row++)
			cross.row(row) = crossRows[row];
		const MatrixXd crossCovariance = cross * conditioned.covariance * cross.transpose();
		const VectorXd values = Eigen::Map<VectorXd>(adjusted.data(), adjusted.size());
		const auto bridge = zhangComponentBridgeGls(values, crossCovariance);
		const double fractional = bridge.valid ? bridge.mean - std::round(bridge.mean) : 0;
		const double perr = bridge.valid
			? round_perr(fractional, bridge.variance) : 1;
		const int residualDof = std::max(1, bridge.effectiveRank - 1);
		boost::math::chi_squared distribution(residualDof);
		const double threshold = quantile(complement(
			distribution, acsConfig.zhangPppAr.held_constraint_nis_alpha));
		const bool accepted = bridge.valid && perr <= maximumPerr &&
			bridge.residualNis <= threshold;
		const bool bothSingletons =
			actualComponents[firstComponent].size() == 1 &&
			actualComponents[secondComponent].size() == 1;
		const bool oneSingleton =
			actualComponents[firstComponent].size() == 1 ||
			actualComponents[secondComponent].size() == 1;
		const char* bridgeScope = bothSingletons
			? "SINGLETON_TO_SINGLETON_CONTROL"
			: (oneSingleton
				? "SINGLETON_TO_CERTIFIED_COMPONENT"
				: "CERTIFIED_COMPONENT_BRIDGE");
		double componentGaugeProductGain = 0;
		double componentGaugeProductGainFraction = 0;
		if (!crossRows.empty())
		{
			MatrixXd gaugeRow(1, rank);
			gaugeRow.row(0) = crossRows.front().transpose();
			const MatrixXd productCross =
				zhangAllPairIncidence(rank) * conditioned.covariance;
			componentGaugeProductGain = zhangIntegerConstraintProductGain(
				gaugeRow, conditioned.covariance, productCross);
			const double pairVariance =
				zhangReferenceInvariantPairTrace(conditioned.covariance);
			componentGaugeProductGainFraction = pairVariance > 0
				? componentGaugeProductGain / pairVariance : 0;
		}
		trace << "\nZHANG_PRODUCT_COMPONENT_BRIDGE_GLS time=" << time.to_string(0)
			  << " audit_scenario=" << auditScenario
			  << " component_a=" << firstComponent
			  << " component_b=" << secondComponent
			  << " size_a=" << actualComponents[firstComponent].size()
			  << " size_b=" << actualComponents[secondComponent].size()
			  << " cross_edges=" << crossRows.size()
			  << " effective_rank=" << bridge.effectiveRank
			  << " mean_cycles=" << bridge.mean
			  << " fractional_cycles=" << fractional
			  << " sigma_cycles=" << (bridge.valid ? std::sqrt(bridge.variance) : 0)
			  << " round_perr=" << perr
			  << " residual_nis=" << bridge.residualNis
			  << " residual_nis_threshold=" << threshold
			  << " product_gain=" << componentGaugeProductGain
			  << " product_gain_fraction=" << componentGaugeProductGainFraction
			  << " accepted=" << accepted
			  << " scope=" << bridgeScope
			  << " certificate_role="
			  << (bothSingletons ? "CONTROL_ONLY" : "CANDIDATE_COMPONENT_MERGE")
			  << " merge_authorized=0"
			  << " component_source=EXACT_CERTIFIED_PAIR_GRAPH"
			  << " feedback=0";
	}
	Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(symmetric);
	if (eigen.info() == Eigen::Success)
	{
		const VectorXd common = VectorXd::Ones(rank).normalized();
		const int weakModes = std::min(8, rank);
		for (int order = 0; order < weakModes; order++)
		{
			const int index = rank - 1 - order;
			const VectorXd eigenvector = eigen.eigenvectors().col(index);
			trace << "\nZHANG_PRODUCT_RELATION_WL_WEAK_MODE time="
				  << time.to_string(0)
				  << " order=" << order + 1
				  << " eigenvalue_cycles2=" << eigen.eigenvalues()(index)
				  << " common_reference_cosine=" << std::abs(eigenvector.dot(common))
				  << " feedback=0";
			vector<pair<double, int>> loading;
			for (int node = 0; node < rank; node++)
				loading.push_back({std::abs(eigenvector(node)), node});
			std::sort(loading.begin(), loading.end(), std::greater<>());
			for (int term = 0; term < std::min(6, rank); term++)
			{
				const int node = loading[term].second;
				trace << "\nZHANG_PRODUCT_RELATION_WL_WEAK_MODE_TERM time="
					  << time.to_string(0)
					  << " mode=" << order + 1
					  << " term=" << term + 1
					  << " satellite=" << zhangProductPairNodeId(basis, node)
					  << " loading=" << eigenvector(node)
					  << " feedback=0";
			}
		}
	}
	trace << "\nZHANG_PRODUCT_RELATION_PAIR_AUDIT_SUMMARY time="
		  << time.to_string(0)
		  << " audit_scenario=" << auditScenario
		  << " mapped_satellites_plus_reference=" << rank + 1
		  << " all_pair_edges=" << metrics.size()
		  << " reliable_pair_edges=" << reliableEdgeCount
		  << " reliability_forest_rank=" << forest.size()
		  << " fresh_evidence_forest_rank=" << freshForest.size()
		  << " deterministic_or_held_edges="
		  << std::count_if(metrics.begin(), metrics.end(),
			[deterministicTolerance](const auto& metric)
			{ return metric.variance <= deterministicTolerance; })
		  << " maximum_perr=" << maximumPerr
		  << " best_edge_count=" << bestCount
		  << " best_edge_median_sigma_cycles=" << zhangMedian(bestSigmas)
		  << " canonical_star_median_sigma_cycles=" << zhangMedian(starSigmas)
		  << " status=COMPLETE feedback=0";
}

static std::vector<ZhangCertifiedPairRelation>
recoverCertifiedPairProductCoordinates(
	const GinAR_mtx& fixed, int namedCount, bool parentStatisticallyAccepted)
{
	ZhangExactMatrix fixedRows;
	ZhangExactVector fixedValues;
	if (fixed.Ztrs.cols() != namedCount ||
		fixed.Ztrs.rows() != fixed.zfix.size()) return {};
	for (int row = 0; row < fixed.Ztrs.rows(); row++)
	{
		ZhangExactVector exactRow(namedCount);
		for (int column = 0; column < namedCount; column++)
		{
			const long long rounded = std::llround(fixed.Ztrs(row, column));
			if (std::abs(fixed.Ztrs(row, column) - rounded) > 1e-8) return {};
			exactRow[column] = rounded;
		}
		const long long value = std::llround(fixed.zfix(row));
		if (std::abs(fixed.zfix(row) - value) > 1e-8) return {};
		fixedRows.push_back(std::move(exactRow));
		fixedValues.push_back(value);
	}
	return zhangRecoverCertifiedPairRelations(
		fixedRows, fixedValues, namedCount, parentStatisticallyAccepted);
}

static bool zhangExactRowsFromNumeric(
	const MatrixXd& rows,
	const VectorXd& values,
	ZhangExactMatrix& exactRows,
	ZhangExactVector& exactValues)
{
	exactRows.clear();
	exactValues.clear();
	if (rows.rows() != values.size()) return false;
	for (int row = 0; row < rows.rows(); row++)
	{
		ZhangExactVector exactRow(rows.cols());
		for (int column = 0; column < rows.cols(); column++)
		{
			const long long rounded = std::llround(rows(row, column));
			if (std::abs(rows(row, column) - rounded) > 1e-8) return false;
			exactRow[column] = rounded;
		}
		const long long roundedValue = std::llround(values(row));
		if (std::abs(values(row) - roundedValue) > 1e-8) return false;
		exactRows.push_back(std::move(exactRow));
		exactValues.push_back(roundedValue);
	}
	return true;
}

/** Pull accepted product-coordinate integers back to the complete network
 * ambiguity coordinate.  The affine offsets are part of the integer RHS;
 * dropping them changes the fixed integer whenever the compiled product
 * coordinate uses a non-zero affine datum. */
static ZhangProductIntegerConstraintSet zhangBuildProductConstraintSet(
	const ZhangProductRelationBasis& firstBasis,
	const ZhangProductRelationBasis& secondBasis,
	const ZhangExactMatrix& wideLaneRows,
	const ZhangExactVector& wideLaneIntegers,
	const ZhangExactMatrix& firstSignalRows,
	const ZhangExactVector& firstSignalIntegers,
	double jointNis,
	double jointNisThreshold,
	double failureProbability,
	double referenceInvariantProductGain)
{
	ZhangProductIntegerConstraintSet result;
	result.system = firstBasis.system;
	result.firstObservable = firstBasis.observable;
	result.secondObservable = secondBasis.observable;
	result.referenceSatellite = firstBasis.referenceSatellite;
	result.productCoordinateDimension = firstBasis.mappableTargetRank;
	result.networkAmbiguityDimension = firstBasis.transform.cols();
	result.wideLaneProductRows = wideLaneRows;
	result.wideLaneIntegers = wideLaneIntegers;
	result.firstSignalProductRows = firstSignalRows;
	result.firstSignalIntegers = firstSignalIntegers;
	result.jointNis = jointNis;
	result.jointNisThreshold = jointNisThreshold;
	result.failureProbability = failureProbability;
	result.referenceInvariantProductGain = referenceInvariantProductGain;

	const int productRank = result.productCoordinateDimension;
	const int networkDimension = result.networkAmbiguityDimension;
	for (int namedIndex : firstBasis.mappableNamedIndices)
	{
		if (namedIndex < 0 || namedIndex >=
			static_cast<int>(firstBasis.namedRelations.size()))
		{
			result.failureReason = "PRODUCT_COORDINATE_SATELLITE_MAPPING_INVALID";
			return result;
		}
		result.coordinateSatellites.push_back(
			firstBasis.namedRelations[namedIndex].satellite);
	}
	const bool dimensionsValid = productRank > 0 && networkDimension > 0 &&
		secondBasis.mappableTargetRank == productRank &&
		secondBasis.transform.cols() == networkDimension &&
		firstBasis.affineOffsets.size() == static_cast<std::size_t>(productRank) &&
		secondBasis.affineOffsets.size() == static_cast<std::size_t>(productRank) &&
		wideLaneRows.size() == wideLaneIntegers.size() &&
		firstSignalRows.size() == firstSignalIntegers.size() &&
		result.coordinateSatellites.size() == static_cast<std::size_t>(productRank) &&
		firstBasis.system == secondBasis.system &&
		firstBasis.referenceSatellite == secondBasis.referenceSatellite;
	if (!dimensionsValid)
	{
		result.failureReason = "PRODUCT_CONSTRAINT_DIMENSION_MISMATCH";
		return result;
	}

	if (!zhangPullBackProductIntegerConstraints(
		firstBasis, secondBasis,
		wideLaneRows, wideLaneIntegers,
		firstSignalRows, firstSignalIntegers,
		result.networkRows, result.networkIntegers,
		result.failureReason)) return result;
	for (const auto& row : wideLaneRows)
	{
		ZhangExactVector joint(2 * productRank);
		for (int column = 0; column < productRank; column++)
		{
			joint[column] = row[column];
			joint[productRank + column] = -row[column];
		}
		result.jointProductRows.push_back(std::move(joint));
	}
	for (const auto& row : firstSignalRows)
	{
		ZhangExactVector joint(2 * productRank);
		for (int column = 0; column < productRank; column++)
			joint[column] = row[column];
		result.jointProductRows.push_back(std::move(joint));
	}

	auto recoverStagePairs = [&](const ZhangExactMatrix& rows,
		const ZhangExactVector& integers, const std::string& coordinate)
	{
		auto pairs = zhangRecoverCertifiedPairRelations(
			rows, integers, productRank, true);
		for (const auto& pair : pairs)
		{
			auto labelled = pair;
			labelled.coordinate = coordinate;
			result.certifiedPairs.push_back(std::move(labelled));
		}
		return pairs;
	};
	const auto wideLanePairs = recoverStagePairs(
		wideLaneRows, wideLaneIntegers, "WL");
	const auto firstSignalPairs = recoverStagePairs(
		firstSignalRows, firstSignalIntegers, "L1");
	auto canonicalPair = [](ZhangCertifiedPairRelation pair)
	{
		if (pair.secondNode < pair.firstNode)
		{
			std::swap(pair.firstNode, pair.secondNode);
			pair.value = -pair.value;
			for (auto& coefficient : pair.parentCombination)
				coefficient = -coefficient;
		}
		return pair;
	};
	std::set<std::pair<int, int>> wideLanePairIds;
	for (const auto& pair : wideLanePairs)
	{
		const auto canonical = canonicalPair(pair);
		wideLanePairIds.insert({canonical.firstNode, canonical.secondNode});
	}
	std::vector<ZhangPairReliabilityEdge> dualFrequencyEdges;
	for (const auto& pair : firstSignalPairs)
	{
		auto canonical = canonicalPair(pair);
		if (wideLanePairIds.contains({canonical.firstNode, canonical.secondNode}))
		{
			dualFrequencyEdges.push_back({
				canonical.firstNode, canonical.secondNode, 0, 0});
			canonical.coordinate = "L1_AND_WL";
			result.dualFrequencyCertifiedPairs.push_back(std::move(canonical));
		}
	}
	result.certifiedPairRank = zhangPairReliabilityForest(
		productRank + 1, dualFrequencyEdges, 0).size();

	auto appendConditioningOnly = [&](const ZhangExactMatrix& rows,
		const ZhangExactVector& integers, bool wideLane)
	{
		if (rows.empty()) return true;
		const auto pairs = zhangRecoverCertifiedPairRelations(
			rows, integers, productRank, true);
		ZhangExactMatrix pairRows;
		ZhangExactVector pairValues;
		for (const auto& pair : pairs)
		{
			ZhangExactVector pairRow(productRank);
			if (pair.firstNode < productRank) pairRow[pair.firstNode] += 1;
			if (pair.secondNode < productRank) pairRow[pair.secondNode] -= 1;
			pairRows.push_back(std::move(pairRow));
			pairValues.push_back(pair.value);
		}
		const auto quotient = zhangExactHeldQuotientAudit(
			rows, pairRows, pairValues);
		if (!quotient.valid) return false;
		for (int q = 0; q < quotient.quotientRank; q++)
		{
			const auto residual = zhangExactRowCombination(
				quotient.quotientTargetCoordinates[q], rows);
			ZhangExactVector jointRow(2 * productRank);
			for (int column = 0; column < productRank; column++)
			{
				jointRow[column] = residual[column];
				if (wideLane) jointRow[productRank + column] = -residual[column];
			}
			ZhangExactInteger value = 0;
			for (std::size_t row = 0; row < integers.size(); row++)
				value += quotient.quotientTargetCoordinates[q][row] * integers[row];
			result.conditioningOnlyRows.push_back(std::move(jointRow));
			result.conditioningOnlyIntegers.push_back(std::move(value));
		}
		return true;
	};
	if (!appendConditioningOnly(wideLaneRows, wideLaneIntegers, true) ||
		!appendConditioningOnly(firstSignalRows, firstSignalIntegers, false))
	{
		result.failureReason = "CONDITIONING_ONLY_QUOTIENT_FAILED";
		return result;
	}
	result.conditioningRank = static_cast<int>(
		zhangExactRowHermiteNormalForm(result.networkRows).basis.size());
	result.exactNetworkMapping = true;
	result.reliable = !result.networkRows.empty() &&
		std::isfinite(jointNis) && std::isfinite(jointNisThreshold) &&
		jointNis <= jointNisThreshold && failureProbability <= 1e-3 + 1e-12;
	result.failureReason = result.reliable
		? "NONE" : "PRODUCT_CONSTRAINT_RELIABILITY_GATE_FAILED";
	return result;
}

/** Build the deliberately non-causal FULL Product-Lattice Oracle control.
 *
 * Oracle values are exact satellite-minus-reference WL and L1 integers proved
 * offline from repeated pair certificates.  They are re-expressed in the
 * current named product basis, then use the same exact affine pull-back as the
 * online solver.  The local NIS is diagnostic only: statistical proximity of
 * the float estimate cannot veto externally supplied ground-truth integers.
 */
static ZhangProductIntegerConstraintSet zhangBuildFullProductLatticeOracle(
	Trace& trace,
	const ZhangFullProductLatticeOracle& oracle,
	const ZhangProductRelationBasis& firstBasis,
	const ZhangProductRelationBasis& secondBasis,
	const VectorXd& fullJointMean,
	const MatrixXd& fullJointCovariance,
	GTime time)
{
	ZhangProductIntegerConstraintSet result;
	const int rank = firstBasis.mappableTargetRank;
	auto fail = [&](const std::string& reason)
	{
		result.failureReason = reason;
		trace << "\nZHANG_FULL_PRODUCT_LATTICE_ORACLE time="
			  << time.to_string(0)
			  << " status=REJECTED reason=" << reason
			  << " noncausal_diagnostic=1 feedback=0";
		return result;
	};
	if (!oracle.valid) return fail(oracle.failureReason);
	if (oracle.system != enum_to_string(firstBasis.system) ||
		rank != oracle.rank || rank <= 0 ||
		secondBasis.mappableTargetRank != rank ||
		fullJointMean.size() != 2 * rank ||
		fullJointCovariance.rows() != 2 * rank ||
		fullJointCovariance.cols() != 2 * rank)
		return fail("ORACLE_CURRENT_BASIS_RANK_MISMATCH");

	ZhangExactMatrix wideLaneRows;
	ZhangExactVector wideLaneIntegers;
	ZhangExactMatrix firstRows;
	ZhangExactVector firstIntegers;
	for (int local = 0; local < rank; local++)
	{
		const int namedIndex = firstBasis.mappableNamedIndices.at(local);
		if (namedIndex < 0 || namedIndex >= static_cast<int>(
			firstBasis.namedRelations.size()))
			return fail("ORACLE_NAMED_RELATION_INDEX_INVALID");
		const auto& relation = firstBasis.namedRelations[namedIndex];
		auto satellite = oracle.potentials.find(relation.satellite.id());
		auto reference = oracle.potentials.find(
			relation.referenceSatellite.id());
		if (satellite == oracle.potentials.end() ||
			reference == oracle.potentials.end())
			return fail("ORACLE_CURRENT_SATELLITE_NOT_COVERED");
		ZhangExactVector unit(rank);
		unit[local] = 1;
		wideLaneRows.push_back(unit);
		firstRows.push_back(std::move(unit));
		wideLaneIntegers.push_back(
			satellite->second.wideLane - reference->second.wideLane);
		firstIntegers.push_back(
			satellite->second.firstSignal - reference->second.firstSignal);
	}

	MatrixXd jointRows = MatrixXd::Zero(2 * rank, 2 * rank);
	VectorXd jointIntegers = VectorXd::Zero(2 * rank);
	for (int row = 0; row < rank; row++)
	{
		jointRows(row, row) = 1;
		jointRows(row, rank + row) = -1;
		jointRows(rank + row, row) = 1;
		jointIntegers(row) = wideLaneIntegers[row].convert_to<double>();
		jointIntegers(rank + row) = firstIntegers[row].convert_to<double>();
	}
	const auto nis = assessZhangIntegerCandidateNis(
		jointIntegers - jointRows * fullJointMean,
		jointRows * fullJointCovariance * jointRows.transpose(), 1e-6);
	const MatrixXd allPairs = zhangAllPairIncidence(rank);
	MatrixXd pairProducts = MatrixXd::Zero(
		2 * allPairs.rows(), 2 * rank);
	pairProducts.topLeftCorner(allPairs.rows(), rank) = allPairs;
	pairProducts.bottomRightCorner(allPairs.rows(), rank) = allPairs;
	const MatrixXd pairProductCross = pairProducts * fullJointCovariance;
	const double productGain = zhangIntegerConstraintProductGain(
		jointRows, fullJointCovariance, pairProductCross);
	result = zhangBuildProductConstraintSet(
		firstBasis, secondBasis,
		wideLaneRows, wideLaneIntegers,
		firstRows, firstIntegers,
		nis.nis, nis.threshold, 0, productGain);
	const bool exactFullRank = result.exactNetworkMapping &&
		result.wideLaneProductRows.size() == static_cast<std::size_t>(rank) &&
		result.firstSignalProductRows.size() == static_cast<std::size_t>(rank) &&
		result.certifiedPairRank == rank;
	if (!exactFullRank)
		return fail(result.failureReason == "NONE"
			? "ORACLE_EXACT_FULL_RANK_PULLBACK_FAILED"
			: result.failureReason);
	result.reliable = true;
	result.failureProbability = 0;
	result.failureReason = "NONE";
	trace << "\nZHANG_FULL_PRODUCT_LATTICE_ORACLE time="
		  << time.to_string(0)
		  << " status=APPLIED"
		  << " rank=" << rank
		  << " conditioning_rank=" << result.conditioningRank
		  << " certified_pair_rank=" << result.certifiedPairRank
		  << " joint_nis=" << nis.nis
		  << " joint_nis_threshold=" << nis.threshold
		  << " local_nis_pass="
		  << (nis.valid && nis.nis <= nis.threshold)
		  << " reference_invariant_product_gain=" << productGain
		  << " external_truth_reliability=1"
		  << " noncausal_diagnostic=1 feedback=0";
	return result;
}

static ZhangFullProductLatticeOracle zhangCachedFullProductLatticeOracle(
	const std::string& filename)
{
	static std::mutex mutex;
	static std::map<std::string, ZhangFullProductLatticeOracle> cache;
	const std::lock_guard<std::mutex> lock(mutex);
	auto iterator = cache.find(filename);
	if (iterator == cache.end())
		iterator = cache.emplace(
			filename, loadZhangFullProductLatticeOracle(filename)).first;
	return iterator->second;
}

static bool zhangCurrentProductPhysicalAmbiguityIdentities(
	const KFState& authoritativeState,
	const std::map<int, KFKey>& ambiguityMap,
	E_Sys system,
	std::map<int, std::string>& identities,
	std::uint64_t& backendBasisGeneration)
{
	identities.clear();
	ZhangGraphIntegerContext graph;
	if (system == E_Sys::NONE ||
		!zhangGraphIntegerContext(authoritativeState, system, graph)) return false;
	backendBasisGeneration = graph.eventId;
	for (const auto& [column, key] : ambiguityMap)
	{
		if (key.Sat.sys != system || key.str.empty()) continue;
		const ZhangGraphEdge edge{key.str, key.Sat};
		auto version = graph.arcVersions.find(edge);
		if (version == graph.arcVersions.end()) return false;
		std::ostringstream identity;
		identity << enum_to_string(static_cast<E_ObsCode>(key.num))
			<< "|" << edge.receiver << "|" << edge.satellite.id()
			<< "|V" << version->second;
		identities[column] = identity.str();
	}
	return !identities.empty();
}

static bool zhangAnnotateProductConstraintPhysicalIdentities(
	const KFState& authoritativeState,
	const std::map<int, KFKey>& ambiguityMap,
	ZhangProductIntegerConstraintSet& constraints)
{
	constraints.physicalNetworkRows.clear();
	constraints.phaseSegmentFingerprint.clear();
	std::map<int, std::string> identities;
	std::uint64_t backendBasisGeneration = 0;
	if (!constraints.reliable || constraints.system == E_Sys::NONE ||
		constraints.networkRows.size() != constraints.networkIntegers.size() ||
		constraints.networkRows.size() != constraints.jointProductRows.size() ||
		!zhangCurrentProductPhysicalAmbiguityIdentities(
			authoritativeState, ambiguityMap, constraints.system,
			identities, backendBasisGeneration))
		return false;
	constraints.backendBasisGeneration = backendBasisGeneration;
	for (const auto& row : constraints.networkRows)
	{
		if (row.size() != ambiguityMap.size()) return false;
		std::map<std::string, ZhangExactInteger> physical;
		for (int column = 0; column < static_cast<int>(row.size()); column++)
		{
			if (row[column] == 0) continue;
			auto identity = identities.find(column);
			if (identity == identities.end()) return false;
			physical[identity->second] += row[column];
		}
		for (auto iterator = physical.begin(); iterator != physical.end();)
		{
			if (iterator->second == 0) iterator = physical.erase(iterator);
			else ++iterator;
		}
		if (physical.empty()) return false;
		constraints.physicalNetworkRows.push_back(std::move(physical));
	}
	std::vector<SatSys> satellites = constraints.coordinateSatellites;
	satellites.push_back(constraints.referenceSatellite);
	std::sort(satellites.begin(), satellites.end());
	satellites.erase(std::unique(satellites.begin(), satellites.end()),
		satellites.end());
	std::ostringstream segments;
	for (const auto& satellite : satellites)
	for (E_ObsCode observable : {
		constraints.firstObservable, constraints.secondObservable})
	{
		const auto status = zhangSatelliteDatumStatus(
			constraints.system, observable, satellite);
		segments << satellite.id() << "|" << enum_to_string(observable)
			<< "|SEG" << status.phaseSegment << ";";
	}
	constraints.phaseSegmentFingerprint = segments.str();
	return !constraints.phaseSegmentFingerprint.empty() &&
		constraints.physicalNetworkRows.size() == constraints.networkRows.size();
}

/** Emit the exact dual-frequency edge before any ledger admission or private
 * conditioning.  A later NIS rejection must not erase which product integer
 * was actually certified by the current product lattice. */
static void traceZhangProductLatticeCertifiedPairs(
	Trace& trace,
	GTime time,
	const ZhangProductIntegerConstraintSet& constraints)
{
	auto canonical = [](ZhangCertifiedPairRelation pair)
	{
		if (pair.secondNode < pair.firstNode)
		{
			std::swap(pair.firstNode, pair.secondNode);
			pair.value = -pair.value;
		}
		return pair;
	};
	auto nodeId = [&](int node)
	{
		if (node >= 0 && node < static_cast<int>(
			constraints.coordinateSatellites.size()))
			return constraints.coordinateSatellites[node].id();
		if (node == constraints.productCoordinateDimension)
			return constraints.referenceSatellite.id();
		return std::string{"INVALID"};
	};
	for (auto dual : constraints.dualFrequencyCertifiedPairs)
	{
		dual = canonical(std::move(dual));
		const auto wideLane = std::find_if(
			constraints.certifiedPairs.begin(), constraints.certifiedPairs.end(),
			[&](auto pair)
			{
				if (pair.coordinate != "WL") return false;
				pair = canonical(std::move(pair));
				return pair.firstNode == dual.firstNode &&
					pair.secondNode == dual.secondNode;
			});
		if (wideLane == constraints.certifiedPairs.end()) continue;
		auto canonicalWideLane = canonical(*wideLane);
		const auto first = nodeId(dual.firstNode);
		const auto second = nodeId(dual.secondNode);
		if (first == "INVALID" || second == "INVALID") continue;
		trace << "\nZHANG_PRODUCT_LATTICE_CERTIFIED_PAIR time="
			  << time.to_string(0)
			  << " system=" << enum_to_string(constraints.system)
			  << " first=" << first
			  << " second=" << second
			  << " wl_integer=" << canonicalWideLane.value
			  << " l1_integer=" << dual.value
			  << " l2_integer=" << dual.value - canonicalWideLane.value
			  << " backend_generation="
			  << constraints.backendBasisGeneration
			  << " phase_segment_fingerprint="
			  << constraints.phaseSegmentFingerprint
			  << " exact_pair_membership=1"
			  << " feedback=PRE_CONDITION_CERTIFICATE";
	}
}

/** Merge only ledger pair rows that survived the same private-branch NIS
 * admission.  Conditioning-only history may improve covariance but can never
 * enter this graph. */
static bool zhangMergeSelectedLedgerPairCertificates(
	Trace& trace,
	GTime time,
	const std::vector<ProductIntegerLedgerRow>& selectedLedgerPairs,
	ZhangProductIntegerConstraintSet& constraints,
	std::string& failureReason)
{
	std::map<std::string, int> satelliteNodes;
	for (int node = 0; node < static_cast<int>(
		constraints.coordinateSatellites.size()); node++)
		satelliteNodes[constraints.coordinateSatellites[node].id()] = node;
	satelliteNodes[constraints.referenceSatellite.id()] =
		constraints.productCoordinateDimension;
	using Edge = std::pair<int, int>;
	std::map<Edge, std::map<std::string, ZhangExactInteger>> values;
	std::set<Edge> ledgerEdges;
	auto insert = [&](int first, int second, std::string coordinate,
		ZhangExactInteger value, bool fromLedger)
	{
		if (first < 0 || second < 0 || first == second) return true;
		if (second < first)
		{
			std::swap(first, second);
			value = -value;
		}
		const Edge edge{first, second};
		auto existing = values[edge].find(coordinate);
		if (existing != values[edge].end() && existing->second != value)
			return false;
		values[edge][std::move(coordinate)] = value;
		if (fromLedger) ledgerEdges.insert(edge);
		return true;
	};
	for (const auto& pair : constraints.certifiedPairs)
	{
		if ((pair.coordinate != "WL" && pair.coordinate != "L1") ||
			!insert(pair.firstNode, pair.secondNode, pair.coordinate,
				pair.value, false))
		{
			failureReason = "CURRENT_PRODUCT_PAIR_VALUE_CONFLICT";
			return false;
		}
	}
	for (const auto& row : selectedLedgerPairs)
	{
		if (!row.pairCertificate ||
			(row.coordinate != "WL" && row.coordinate != "L1")) continue;
		auto first = satelliteNodes.find(row.firstSatellite);
		auto second = satelliteNodes.find(row.secondSatellite);
		if (first == satelliteNodes.end() || second == satelliteNodes.end())
			continue;
		if (!insert(first->second, second->second, row.coordinate,
			row.integerValue, true))
		{
			failureReason = "LEDGER_PRODUCT_PAIR_VALUE_CONFLICT";
			return false;
		}
	}
	constraints.dualFrequencyCertifiedPairs.clear();
	std::vector<ZhangPairReliabilityEdge> graphEdges;
	for (const auto& [edge, coordinates] : values)
	{
		auto wideLane = coordinates.find("WL");
		auto firstSignal = coordinates.find("L1");
		if (wideLane == coordinates.end() || firstSignal == coordinates.end())
			continue;
		ZhangCertifiedPairRelation dual;
		dual.firstNode = edge.first;
		dual.secondNode = edge.second;
		dual.value = firstSignal->second;
		dual.coordinate = "L1_AND_WL";
		dual.fromTemporalLedger = ledgerEdges.contains(edge);
		constraints.dualFrequencyCertifiedPairs.push_back(dual);
		graphEdges.push_back({edge.first, edge.second, 0, 0});
		auto nodeId = [&](int node)
		{
			return node == constraints.productCoordinateDimension
				? constraints.referenceSatellite.id()
				: constraints.coordinateSatellites.at(node).id();
		};
		trace << "\nZHANG_PRODUCT_LATTICE_CERTIFIED_PAIR time="
			  << time.to_string(0)
			  << " system=" << enum_to_string(constraints.system)
			  << " first=" << nodeId(edge.first)
			  << " second=" << nodeId(edge.second)
			  << " wl_integer=" << wideLane->second
			  << " l1_integer=" << firstSignal->second
			  << " l2_integer=" << firstSignal->second - wideLane->second
			  << " backend_generation="
			  << constraints.backendBasisGeneration
			  << " phase_segment_fingerprint="
			  << constraints.phaseSegmentFingerprint
			  << " exact_pair_membership=1"
			  << " certificate_source="
			  << (dual.fromTemporalLedger
				? "CURRENT_PLUS_NIS_SELECTED_LEDGER" : "CURRENT_PRODUCT_LATTICE")
			  << " feedback=PRE_CONDITION_CERTIFICATE";
	}
	constraints.certifiedPairRank = zhangPairReliabilityForest(
		constraints.productCoordinateDimension + 1, graphEdges, 0).size();
	failureReason = "NONE";
	return true;
}

static bool zhangProductConstraintsWithLedgerAsGinAr(
	Trace& trace,
	GTime time,
	const ZhangProductIntegerConstraintSet& constraints,
	const std::string& authoritativeRuntimeId,
	const KFState& identityState,
	const KFState& conditioningState,
	const std::map<int, KFKey>& ambiguityMap,
	GinAR_mtx& result,
	int& ledgerProjectedRank,
	int& ledgerSelectedRank,
	int& ledgerRejectedRows,
	double& combinedNis,
	double& combinedNisThreshold,
	std::vector<ProductIntegerLedgerRow>& selectedLedgerPairRows,
	std::string& failureReason)
{
	result = GinAR_mtx{};
	ledgerProjectedRank = 0;
	ledgerSelectedRank = 0;
	ledgerRejectedRows = 0;
	combinedNis = std::numeric_limits<double>::quiet_NaN();
	combinedNisThreshold = std::numeric_limits<double>::quiet_NaN();
	selectedLedgerPairRows.clear();
	failureReason = "NOT_EVALUATED";
	if (!constraints.reliable || !constraints.exactNetworkMapping ||
		constraints.networkRows.empty())
	{
		failureReason = "CURRENT_PRODUCT_CONSTRAINTS_INVALID";
		return false;
	}
	std::map<int, std::string> columnIdentities;
	std::uint64_t backendGeneration = 0;
	if (!zhangCurrentProductPhysicalAmbiguityIdentities(
		identityState, ambiguityMap, constraints.system,
		columnIdentities, backendGeneration))
	{
		failureReason = "CURRENT_PRODUCT_IDENTITIES_UNAVAILABLE";
		return false;
	}
	if (backendGeneration != constraints.backendBasisGeneration)
	{
		failureReason = "CURRENT_PRODUCT_BACKEND_GENERATION_CHANGED";
		return false;
	}
	std::map<std::string, int> identityColumns;
	for (const auto& [column, identity] : columnIdentities)
		identityColumns[identity] = column;
	using NamedPairKey = std::tuple<std::string, std::string, std::string>;
	std::map<NamedPairKey, ZhangExactInteger> mandatoryCurrentPairValues;
	auto productNodeId = [&](int node)
	{
		if (node >= 0 && node < static_cast<int>(
			constraints.coordinateSatellites.size()))
			return constraints.coordinateSatellites[node].id();
		if (node == constraints.productCoordinateDimension)
			return constraints.referenceSatellite.id();
		return std::string{};
	};
	auto canonicalNamedPair = [](std::string first, std::string second,
		std::string coordinate, ZhangExactInteger value)
	{
		if (second < first)
		{
			std::swap(first, second);
			value = -value;
		}
		return std::pair{
			NamedPairKey{std::move(first), std::move(second),
				std::move(coordinate)}, value};
	};
	for (const auto& pair : constraints.certifiedPairs)
	{
		if (pair.coordinate != "WL" && pair.coordinate != "L1") continue;
		auto first = productNodeId(pair.firstNode);
		auto second = productNodeId(pair.secondNode);
		if (first.empty() || second.empty() || first == second)
		{
			failureReason = "CURRENT_PRODUCT_PAIR_IDENTITY_INVALID";
			return false;
		}
		auto [key, value] = canonicalNamedPair(
			std::move(first), std::move(second), pair.coordinate, pair.value);
		auto [existing, inserted] = mandatoryCurrentPairValues.emplace(key, value);
		if (!inserted && existing->second != value)
		{
			failureReason = "CURRENT_PRODUCT_PAIR_VALUE_CONFLICT";
			return false;
		}
	}
	auto registry = zhangProductIntegerLedgerRegistry().find(
		{authoritativeRuntimeId, constraints.system});
	ZhangExactMatrix ledgerRows;
	ZhangExactVector ledgerValues;
	std::vector<ProductIntegerLedgerRow> ledgerMetadata;
	if (registry != zhangProductIntegerLedgerRegistry().end())
	{
		for (const auto& held : registry->second.rows())
		{
			if (!held.certified || held.system != constraints.system) continue;
			const string currentSegmentFingerprint =
				zhangProductPhysicalRowSegmentFingerprint(
					held.system, held.physicalExpansion);
			if (currentSegmentFingerprint.empty() ||
				held.phaseSegmentFingerprint != currentSegmentFingerprint)
			{
				trace << "\nZHANG_PRODUCT_INTEGER_LEDGER_SEGMENT_REJECT time="
					  << time.to_string(0)
					  << " stored_segment_fingerprint="
					  << held.phaseSegmentFingerprint
					  << " current_segment_fingerprint="
					  << (currentSegmentFingerprint.empty()
						? "INVALID_PHYSICAL_IDENTITY" : currentSegmentFingerprint)
					  << " physical_row="
					  << zhangProductPhysicalRowFingerprint(
						 held.physicalExpansion)
					  << " action=REJECT_ROW feedback=PRIVATE_PRODUCT_BRANCH";
				continue;
			}
			if (held.pairCertificate &&
				(held.coordinate == "WL" || held.coordinate == "L1"))
			{
				auto [key, ledgerValue] = canonicalNamedPair(
					held.firstSatellite, held.secondSatellite,
					held.coordinate, held.integerValue);
				auto current = mandatoryCurrentPairValues.find(key);
				if (current != mandatoryCurrentPairValues.end() &&
					current->second != ledgerValue)
				{
					const auto& [first, second, coordinate] = key;
					ledgerRejectedRows++;
					trace << "\nZHANG_PRODUCT_INTEGER_LEDGER_PAIR_CONFLICT time="
						  << time.to_string(0)
						  << " system=" << enum_to_string(constraints.system)
						  << " coordinate=" << coordinate
						  << " first=" << first
						  << " second=" << second
						  << " current_integer=" << current->second
						  << " ledger_integer=" << ledgerValue
						  << " ledger_backend_generation="
						  << held.backendBasisGeneration
						  << " current_backend_generation="
						  << backendGeneration
						  << " action=REJECT_OPTIONAL_LEDGER_ROW"
						  << " reason=MANDATORY_CURRENT_PAIR_VALUE_PRECEDENCE";
					continue;
				}
			}
			ZhangExactVector row;
			if (!zhangProjectProductLedgerPhysicalRow(
				held, identityColumns,
				static_cast<int>(ambiguityMap.size()), row)) continue;
			if (held.backendBasisGeneration != backendGeneration)
			{
				// This is not a cross-generation coordinate splice.  Every
				// physical arc/version in the retained integer has been mapped
				// exactly into the current ambiguity state above; the resulting
				// row must still survive the joint-NIS admission below.
				trace << "\nZHANG_PRODUCT_INTEGER_LEDGER_TRANSPORT time="
					  << time.to_string(0)
					  << " from_backend_generation="
					  << held.backendBasisGeneration
					  << " to_backend_generation=" << backendGeneration
					  << " phase_segment_fingerprint="
					  << held.phaseSegmentFingerprint
					  << " physical_row="
					  << zhangProductPhysicalRowFingerprint(
						 held.physicalExpansion)
					  << " status=CANDIDATE"
					  << " admission=PENDING_JOINT_NIS";
			}
			ledgerRows.push_back(row);
			ledgerValues.push_back(held.integerValue);
			ledgerMetadata.push_back(held);
		}
	}
	ledgerProjectedRank = static_cast<int>(
		zhangExactRowHermiteNormalForm(ledgerRows).basis.size());

	VectorXd ambiguityMean(ambiguityMap.size());
	MatrixXd ambiguityCovariance(
		ambiguityMap.size(), ambiguityMap.size());
	for (int row = 0; row < static_cast<int>(ambiguityMap.size()); row++)
	{
		auto rowKey = ambiguityMap.find(row);
		if (rowKey == ambiguityMap.end())
		{
			failureReason = "CURRENT_PRODUCT_AMBIGUITY_COLUMN_MISSING";
			return false;
		}
		auto rowState = conditioningState.kfIndexMap.find(rowKey->second);
		if (rowState == conditioningState.kfIndexMap.end())
		{
			failureReason = "CURRENT_PRODUCT_AMBIGUITY_STATE_MISSING";
			return false;
		}
		ambiguityMean(row) = conditioningState.x(rowState->second);
		for (int column = 0; column < static_cast<int>(ambiguityMap.size()); column++)
		{
			auto columnKey = ambiguityMap.find(column);
			if (columnKey == ambiguityMap.end()) return false;
			auto columnState = conditioningState.kfIndexMap.find(columnKey->second);
			if (columnState == conditioningState.kfIndexMap.end()) return false;
			ambiguityCovariance(row, column) = conditioningState.P(
				rowState->second, columnState->second);
		}
	}
	ambiguityCovariance = 0.5 *
		(ambiguityCovariance + ambiguityCovariance.transpose());

	auto numericHnf = [&](const ZhangExactMatrix& exactRows,
		const ZhangExactVector& exactValues,
		MatrixXd& numericRows, VectorXd& numericValues,
		ZhangExactMatrix* canonicalRows = nullptr,
		ZhangExactVector* canonicalValues = nullptr)
	{
		if (exactRows.empty() || exactRows.size() != exactValues.size())
			return false;
		for (const auto& exactRow : exactRows)
		{
			if (exactRow.size() != ambiguityMap.size()) return false;
		}
		const auto hnf = zhangExactRowHermiteNormalForm(exactRows, exactValues);
		if (!hnf.consistent || hnf.basis.empty() ||
			hnf.basis.size() != hnf.values.size()) return false;
		numericRows = MatrixXd::Zero(hnf.basis.size(), ambiguityMap.size());
		numericValues = VectorXd::Zero(hnf.values.size());
		for (int row = 0; row < static_cast<int>(hnf.basis.size()); row++)
		{
			numericRows.row(row) =
				zhangExactRowToDouble(hnf.basis[row]).transpose();
			numericValues(row) = hnf.values[row].convert_to<double>();
		}
		if (!numericRows.allFinite() || !numericValues.allFinite()) return false;
		if (canonicalRows) *canonicalRows = hnf.basis;
		if (canonicalValues) *canonicalValues = hnf.values;
		return true;
	};
	auto statisticallyCompatible = [&](const ZhangExactMatrix& exactRows,
		const ZhangExactVector& exactValues, double alpha)
	{
		MatrixXd rows;
		VectorXd values;
		if (!numericHnf(exactRows, exactValues, rows, values)) return false;
		const auto nis = assessZhangIntegerCandidateNis(
			values - rows * ambiguityMean,
			rows * ambiguityCovariance * rows.transpose(), alpha);
		return nis.valid && std::isfinite(nis.nis) &&
			std::isfinite(nis.threshold) && nis.nis <= nis.threshold;
	};

	ZhangExactMatrix selectedRows = constraints.networkRows;
	ZhangExactVector selectedValues = constraints.networkIntegers;
	const double familyAlpha = std::max(
		1e-12, acsConfig.zhangPppAr.held_constraint_nis_alpha);
	if (!statisticallyCompatible(selectedRows, selectedValues, familyAlpha))
	{
		failureReason = "CURRENT_PRODUCT_CONSTRAINT_NIS_REJECTED";
		return false;
	}

	std::vector<std::pair<double, int>> orderedLedger;
	for (int row = 0; row < static_cast<int>(ledgerRows.size()); row++)
	{
		const VectorXd numeric = zhangExactRowToDouble(ledgerRows[row]);
		const double variance =
			(numeric.transpose() * ambiguityCovariance * numeric)(0, 0);
		const double innovation = ledgerValues[row].convert_to<double>() -
			numeric.dot(ambiguityMean);
		if (!(variance > 0) || !std::isfinite(variance) ||
			!std::isfinite(innovation))
		{
			ledgerRejectedRows++;
			if (ledgerMetadata[row].backendBasisGeneration != backendGeneration)
			{
				trace << "\nZHANG_PRODUCT_INTEGER_LEDGER_TRANSPORT time="
					  << time.to_string(0)
					  << " from_backend_generation="
					  << ledgerMetadata[row].backendBasisGeneration
					  << " to_backend_generation=" << backendGeneration
					  << " physical_row="
					  << zhangProductPhysicalRowFingerprint(
						 ledgerMetadata[row].physicalExpansion)
					  << " status=REJECTED"
					  << " admission=INVALID_NUMERIC_PROJECTION";
			}
			continue;
		}
		orderedLedger.push_back({innovation * innovation / variance, row});
	}
	std::sort(orderedLedger.begin(), orderedLedger.end());
	ZhangExactMatrix acceptedLedgerRows;
	// Do not relax the consistency gate as the optional ledger grows.  For a
	// chi-square upper-tail test, dividing alpha would increase the threshold
	// and make stale integer rows easier to admit, which is the opposite of the
	// required false-fix guard.
	const double admissionAlpha = familyAlpha;
	for (const auto& [marginalNis, index] : orderedLedger)
	{
		auto trialRows = selectedRows;
		auto trialValues = selectedValues;
		trialRows.push_back(ledgerRows[index]);
		trialValues.push_back(ledgerValues[index]);
		if (statisticallyCompatible(trialRows, trialValues, admissionAlpha))
		{
			selectedRows = std::move(trialRows);
			selectedValues = std::move(trialValues);
			acceptedLedgerRows.push_back(ledgerRows[index]);
			if (ledgerMetadata[index].pairCertificate)
				selectedLedgerPairRows.push_back(ledgerMetadata[index]);
			if (ledgerMetadata[index].backendBasisGeneration != backendGeneration)
			{
				trace << "\nZHANG_PRODUCT_INTEGER_LEDGER_TRANSPORT time="
					  << time.to_string(0)
					  << " from_backend_generation="
					  << ledgerMetadata[index].backendBasisGeneration
					  << " to_backend_generation=" << backendGeneration
					  << " physical_row="
					  << zhangProductPhysicalRowFingerprint(
						 ledgerMetadata[index].physicalExpansion)
					  << " status=ADMITTED"
					  << " admission=JOINT_NIS_PASS";
			}
		}
		else
		{
			ledgerRejectedRows++;
			trace << "\nZHANG_PRODUCT_INTEGER_LEDGER_ADMISSION_REJECT time="
				  << time.to_string(0)
				  << " candidate_row=" << index
				  << " marginal_nis=" << marginalNis
				  << " selected_before=" << acceptedLedgerRows.size()
				  << " backend_generation=" << backendGeneration
				  << " reason=JOINT_NIS_INCOMPATIBLE";
			if (ledgerMetadata[index].backendBasisGeneration != backendGeneration)
			{
				trace << "\nZHANG_PRODUCT_INTEGER_LEDGER_TRANSPORT time="
					  << time.to_string(0)
					  << " from_backend_generation="
					  << ledgerMetadata[index].backendBasisGeneration
					  << " to_backend_generation=" << backendGeneration
					  << " physical_row="
					  << zhangProductPhysicalRowFingerprint(
						 ledgerMetadata[index].physicalExpansion)
					  << " status=REJECTED"
					  << " admission=JOINT_NIS_FAIL";
			}
		}
	}
	ledgerSelectedRank = static_cast<int>(
		zhangExactRowHermiteNormalForm(acceptedLedgerRows).basis.size());

	MatrixXd finalRows;
	VectorXd finalValues;
	ZhangExactMatrix canonicalRows;
	ZhangExactVector canonicalValues;
	if (!numericHnf(selectedRows, selectedValues, finalRows, finalValues,
		&canonicalRows, &canonicalValues))
	{
		failureReason = "PRODUCT_LEDGER_FINAL_HNF_FAILED";
		return false;
	}
	const auto finalNis = assessZhangIntegerCandidateNis(
		finalValues - finalRows * ambiguityMean,
		finalRows * ambiguityCovariance * finalRows.transpose(),
		acceptedLedgerRows.empty() ? familyAlpha : admissionAlpha);
	if (!finalNis.valid || !std::isfinite(finalNis.nis) ||
		!std::isfinite(finalNis.threshold) || finalNis.nis > finalNis.threshold)
	{
		failureReason = "PRODUCT_LEDGER_FINAL_JOINT_NIS_REJECTED";
		return false;
	}
	combinedNis = finalNis.nis;
	combinedNisThreshold = finalNis.threshold;
	result.ambmap = ambiguityMap;
	result.Ztrs = std::move(finalRows);
	result.zfix = std::move(finalValues);
	failureReason = "NONE";
	trace << "\nZHANG_PRODUCT_INTEGER_LEDGER_ADMISSION time="
		  << time.to_string(0)
		  << " projected_rank=" << ledgerProjectedRank
		  << " selected_rank=" << ledgerSelectedRank
		  << " rejected_rows=" << ledgerRejectedRows
		  << " mandatory_current_rank=" << constraints.conditioningRank
		  << " final_rank=" << result.Ztrs.rows()
		  << " combined_joint_nis=" << combinedNis
		  << " combined_joint_nis_threshold=" << combinedNisThreshold
		  << " backend_generation=" << backendGeneration
		  << " status=COMPATIBLE_SUBSET_SELECTED";
	return true;
}

/** One accepted block from the direct satellite-product integer lattice.
 * Rows remain exact mixed product coordinates; pair certificates are recovered
 * later by exact lattice membership and are not required for conditioning. */
struct ZhangProductLatticeStageFix
{
	ZhangExactMatrix rows;
	ZhangExactVector integers;
	int generatedCandidates = 0;
	int reliableCandidates = 0;
	int selectedBasisRank = 0;
	int rawFixedRank = 0;
	double bootstrapSuccess = 0;
	double failureProbabilityBound = 1;
	double nis = std::numeric_limits<double>::quiet_NaN();
	double nisThreshold = std::numeric_limits<double>::quiet_NaN();
	double productGain = 0;
	int certifiedPairRank = 0;
	bool reliable = false;
	std::string selectionSource = "NONE";
	std::string failureReason = "NOT_EVALUATED";
};

/** Search a named satellite-pair forest without treating a partial set of
 * decorrelated coordinates as named certificates.  Each accepted branch must
 * fix the complete selected forest; branches are visited by descending graph
 * rank and then product gain. */
static ZhangProductLatticeStageFix zhangSolveNamedPairForestStage(
	Trace& trace,
	const VectorXd& mean,
	const MatrixXd& covariance,
	const MatrixXd& productCrossCovariance,
	const ProductIntegerCandidateGenerationResult& generated,
	const GinAR_opt& options,
	GTime time,
	const std::string& stage,
	double failureProbabilityBudget)
{
	ZhangProductLatticeStageFix best;
	best.failureReason = "NO_RELIABLE_NAMED_PAIR_FOREST";
	if (mean.size() <= 0) return best;
	std::vector<ZhangNamedPairBeamCandidate> forest;
	const bool unitCoordinateOnly =
		stage == "L1_GIVEN_WL_PAIR_SUBLATTICE";
	for (const auto& candidate : generated.candidates)
	{
		if (!candidate.reliabilityPassed) break;
		if (candidate.source != "ALL_PAIR_ROWS") continue;
		if (unitCoordinateOnly)
		{
			int support = 0;
			for (const auto& coefficient : candidate.row)
				support += coefficient != 0;
			if (support != 1) continue;
		}
		std::vector<int> nodes;
		for (int index = 0; index < static_cast<int>(candidate.row.size()); index++)
			if (candidate.row[index] != 0) nodes.push_back(index);
		if (nodes.size() == 1) nodes.push_back(mean.size());
		if (nodes.size() != 2) continue;
		forest.push_back({candidate.row, candidate.perr,
			candidate.incrementalProductGain, candidate.variance, nodes});
	}
	if (forest.empty()) return best;

	const int minimumRank = std::max(1,
		acsConfig.zhangPppAr.product_relation_minimum_rank);
	const int beamWidth = std::max(1,
		acsConfig.zhangPppAr.product_relation_beam_width);
	const int maximumEvaluations = std::max(1,
		acsConfig.zhangPppAr.product_relation_maximum_evaluations);
	// Expand actual named-edge subsets.  The previous implementation built one
	// greedy forest and could only delete from it, so any useful edge excluded
	// from that first tree was unreachable.  This beam retains alternate
	// forests at every rank and therefore performs genuine candidate expansion.
	const auto levels = zhangNamedPairForestBeamLevels(
		forest, mean.size(), beamWidth);

	int evaluated = 0;
	int fullRankAttempts = 0;
	int selectedRank = 0;
	const int maximumGeneratedRank = static_cast<int>(levels.size());
	for (int levelIndex = static_cast<int>(levels.size()) - 1;
		levelIndex >= 0 && evaluated < maximumEvaluations; levelIndex--)
	{
		struct Evaluated
		{
			std::vector<int> selected;
			ZhangProductLatticeStageFix fix;
			double proposalGain = 0;
		};
		std::vector<Evaluated> level;
		for (const auto& branch : levels[levelIndex])
		{
			if (evaluated >= maximumEvaluations) break;
			const auto& selected = branch.selected;
			evaluated++;
			const int rank = selected.size();
			if (rank < minimumRank) continue;
			ZhangExactMatrix selectedRows;
			MatrixXd proposal(rank, mean.size());
			for (int row = 0; row < rank; row++)
			{
				selectedRows.push_back(forest[selected[row]].row);
				proposal.row(row) =
					zhangExactRowToDouble(forest[selected[row]].row).transpose();
			}
			Evaluated candidate;
			candidate.selected = selected;
			candidate.proposalGain = zhangIntegerConstraintProductGain(
				proposal, covariance, productCrossCovariance);
			GinAR_mtx block;
			block.aflt = proposal * mean;
			block.Paflt = proposal * covariance * proposal.transpose();
			GinAR_opt blockOptions = options;
			blockOptions.min_lambda_fix_count = 1;
			blockOptions.sucthr = std::max(options.sucthr,
				1 - std::min(1e-3, std::max(1e-12,
					failureProbabilityBudget)));
			const int fixed = rankAwareGnssAr(
				trace, block, blockOptions, time,
				"PRODUCT_NAMED_PAIR_FOREST_" + stage, true);
			candidate.fix.rawFixedRank = fixed;
			candidate.fix.selectedBasisRank = rank;
			candidate.fix.nis = block.lambda_candidate_nis;
			candidate.fix.nisThreshold = block.lambda_candidate_nis_threshold;
			candidate.fix.bootstrapSuccess = fixed > 0 &&
				block.lambda_selected_bootstrap_success > 0
				? block.lambda_selected_bootstrap_success
				: (fixed > 0 ? blockOptions.sucthr : 0);
			candidate.fix.failureProbabilityBound = fixed > 0
				? zhangProductFailureProbabilityBound(
					candidate.fix.bootstrapSuccess,
					failureProbabilityBudget) : 1;
			candidate.fix.productGain = candidate.proposalGain;
			if (fixed == rank && block.Ztrs.rows() == rank)
			{
				fullRankAttempts++;
				ZhangExactMatrix parentRows;
				ZhangExactVector parentValues;
				if (block.lambda_candidate_nis_valid &&
					block.lambda_candidate_nis <=
						block.lambda_candidate_nis_threshold &&
					zhangExactRowsFromNumeric(block.Ztrs, block.zfix,
						parentRows, parentValues))
				{
					for (const auto& parent : parentRows)
						candidate.fix.rows.push_back(
							zhangExactRowCombination(parent, selectedRows));
					candidate.fix.integers = std::move(parentValues);
					int exactRank = 0;
					const bool primitive = zhangExactPrimitiveRowLattice(
						candidate.fix.rows, mean.size(), &exactRank) &&
						exactRank == rank;
					candidate.fix.reliable = primitive &&
						zhangProductFailureProbabilityPassed(
							candidate.fix.failureProbabilityBound,
							failureProbabilityBudget);
					if (candidate.fix.reliable)
					{
						candidate.fix.certifiedPairRank = rank;
						candidate.fix.selectionSource =
							"NAMED_PAIR_FOREST_BEAM";
						candidate.fix.failureReason = "NONE";
					}
				}
			}
			level.push_back(std::move(candidate));
		}
		if (level.empty()) break;
		std::sort(level.begin(), level.end(), [](const auto& left, const auto& right)
		{
			if (left.fix.reliable != right.fix.reliable)
				return left.fix.reliable > right.fix.reliable;
			if (left.selected.size() != right.selected.size())
				return left.selected.size() > right.selected.size();
			if (left.proposalGain != right.proposalGain)
				return left.proposalGain > right.proposalGain;
			return left.selected < right.selected;
		});
		for (const auto& candidate : level)
		{
			if (!candidate.fix.reliable) continue;
			if (!best.reliable ||
				candidate.fix.certifiedPairRank > best.certifiedPairRank ||
				(candidate.fix.certifiedPairRank == best.certifiedPairRank &&
				 candidate.fix.productGain > best.productGain))
				best = candidate.fix;
		}
		if (best.reliable)
		{
			selectedRank = best.certifiedPairRank;
			break;
		}
	}
	trace << "\nZHANG_PRODUCT_NAMED_PAIR_FOREST_SEARCH time="
		  << time.to_string(0)
		  << " stage=" << stage
		  << " coordinate_mode="
		  << (unitCoordinateOnly ? "NAMED_FOREST_EDGE_UNITS" : "ALL_SATELLITE_PAIRS")
		  << " candidate_edges=" << forest.size()
		  << " maximum_generated_rank=" << maximumGeneratedRank
		  << " evaluated_branches=" << evaluated
		  << " full_rank_attempts=" << fullRankAttempts
		  << " selected_rank=" << selectedRank
		  << " reliable=" << best.reliable
		  << " product_gain=" << best.productGain
		  << " status=" << best.failureReason;
	return best;
}

/** Solve one integer stage directly in the legal product coordinate.
 *
 * Candidate rows include every satellite pair, LAMBDA-reduced rows, and dense
 * primitive approximations to product-gain generalized eigenmodes.  The
 * candidate dictionary is only a coordinate proposal: one joint LAMBDA/PAR
 * solve decides the accepted block and its returned mixed rows are composed
 * exactly back into the product lattice before they can leave this function.
 */
static ZhangProductLatticeStageFix zhangSolveProductLatticeStage(
	Trace& trace,
	const VectorXd& mean,
	const MatrixXd& covariance,
	const MatrixXd& referenceInvariantProductCrossCovariance,
	const GinAR_opt& options,
	GTime time,
	const std::string& stage,
	double failureProbabilityBudget)
{
	ZhangProductLatticeStageFix result;
	const auto stageStarted = std::chrono::steady_clock::now();
	auto reductionFinished = stageStarted;
	auto generationFinished = stageStarted;
	auto ilsFinished = stageStarted;
	auto milliseconds = [](auto first, auto second)
	{
		return std::chrono::duration_cast<std::chrono::milliseconds>(
			second - first).count();
	};
	auto traceStageResult = [&]()
	{
		trace << "\nZHANG_PRODUCT_LATTICE_STAGE time=" << time.to_string(0)
			  << " stage=" << stage
			  << " generated_candidates=" << result.generatedCandidates
			  << " reliable_candidates=" << result.reliableCandidates
			  << " proposal_rank=" << result.selectedBasisRank
			  << " raw_fixed_rank=" << result.rawFixedRank
			  << " exact_fixed_rank=" << result.rows.size()
			  << " certified_pair_rank=" << result.certifiedPairRank
			  << " product_gain=" << result.productGain
			  << " selection_source=" << result.selectionSource
			  << " bootstrap_success=" << result.bootstrapSuccess
			  << " failure_probability_bound=" << result.failureProbabilityBound
			  << " failure_probability_budget=" << failureProbabilityBudget
			  << " joint_nis=" << result.nis
			  << " joint_nis_threshold=" << result.nisThreshold
			  << " reliable=" << result.reliable
			  << " status=" << result.failureReason
			  << " reduction_ms="
			  << milliseconds(stageStarted, reductionFinished)
			  << " generation_ms="
			  << milliseconds(reductionFinished, generationFinished)
			  << " ils_ms="
			  << milliseconds(generationFinished, ilsFinished)
			  << " total_ms="
			  << milliseconds(stageStarted,
				std::chrono::steady_clock::now());
	};
	if (mean.size() <= 0 || covariance.rows() != mean.size() ||
		covariance.cols() != mean.size() ||
		referenceInvariantProductCrossCovariance.cols() != mean.size())
	{
		result.failureReason = "PRODUCT_LATTICE_STAGE_DIMENSION_MISMATCH";
		traceStageResult();
		return result;
	}
	ZhangExactMatrix reducedRows;
	GinAR_mtx reduced;
	reduced.aflt = mean;
	reduced.Paflt = covariance;
	const bool reductionValid = Ztrans_reduction(trace, reduced) >= 0;
	if (reductionValid && reduced.Ztrs.cols() == mean.size())
	{
		VectorXd dummy = VectorXd::Zero(reduced.Ztrs.rows());
		ZhangExactVector unused;
		zhangExactRowsFromNumeric(reduced.Ztrs, dummy, reducedRows, unused);
	}
	reductionFinished = std::chrono::steady_clock::now();
	const double maximumFailureProbability = std::min(
		1e-3, std::max(1e-12, failureProbabilityBudget));
	const auto generated = generateProductIntegerCandidates(
		mean,
		0.5 * (covariance + covariance.transpose()),
		referenceInvariantProductCrossCovariance,
		maximumFailureProbability,
		std::max(1e-9, options.lambda_candidate_nis_alpha),
		reducedRows,
		std::min<int>(8, mean.size()),
		12,
		std::max<std::size_t>(512, static_cast<std::size_t>(std::max(
			1, acsConfig.zhangPppAr.product_relation_maximum_evaluations))));
	result.generatedCandidates = generated.candidates.size();
	result.reliableCandidates = generated.reliableRows;
	generationFinished = std::chrono::steady_clock::now();
	if (!generated.valid)
	{
		result.failureReason = generated.failureReason;
		traceStageResult();
		return result;
	}
	const auto namedPairFix =
		zhangSolveNamedPairForestStage(
			trace, mean, covariance,
			referenceInvariantProductCrossCovariance,
			generated, options, time, stage, maximumFailureProbability);
	auto finishWithNamedFallback = [&](std::string failure)
	{
		if (namedPairFix.reliable)
		{
			const int generatedCandidates = result.generatedCandidates;
			const int reliableCandidates = result.reliableCandidates;
			result = namedPairFix;
			result.generatedCandidates = generatedCandidates;
			result.reliableCandidates = reliableCandidates;
		}
		else
		{
			result.failureReason = std::move(failure);
		}
		traceStageResult();
		return result;
	};

	// Build a primitive independent proposal basis in the documented
	// reliability -> pair-graph -> product-gain order.  Dense rows are not
	// support-truncated and fill any product directions not spanned by reliable
	// pair coordinates.
	ZhangExactMatrix proposalRows;
	for (const auto& candidate : generated.candidates)
	{
		if (!candidate.reliabilityPassed) break;
		auto trial = proposalRows;
		trial.push_back(candidate.row);
		int exactRank = 0;
		if (!zhangExactPrimitiveRowLattice(trial, mean.size(), &exactRank) ||
			exactRank != static_cast<int>(trial.size())) continue;
		proposalRows = std::move(trial);
		if (proposalRows.size() == static_cast<std::size_t>(mean.size())) break;
	}
	result.selectedBasisRank = proposalRows.size();
	if (proposalRows.empty())
	{
		return finishWithNamedFallback(
			"NO_RELIABLE_PRODUCT_LATTICE_CANDIDATE");
	}
	MatrixXd proposal = MatrixXd::Zero(proposalRows.size(), mean.size());
	for (int row = 0; row < proposal.rows(); row++)
		proposal.row(row) = zhangExactRowToDouble(proposalRows[row]).transpose();
	GinAR_mtx block;
	block.aflt = proposal * mean;
	block.Paflt = proposal * covariance * proposal.transpose();
	GinAR_opt blockOptions = options;
	blockOptions.min_lambda_fix_count = 1;
	blockOptions.sucthr = std::max(
		options.sucthr, 1 - maximumFailureProbability);
	result.rawFixedRank = rankAwareGnssAr(
		trace, block, blockOptions, time,
		"PRODUCT_LATTICE_" + stage, true);
	ilsFinished = std::chrono::steady_clock::now();
	// Prefer the selected suffix probability retained by GNSS_AR.  The threshold
	// fallback only covers alternate/legacy paths that accepted a rank without
	// recording the selected suffix statistic.
	result.bootstrapSuccess = result.rawFixedRank > 0 &&
		block.lambda_selected_bootstrap_success > 0
		? block.lambda_selected_bootstrap_success
		: (result.rawFixedRank > 0 ? blockOptions.sucthr : 0);
	result.failureProbabilityBound = result.rawFixedRank > 0
		? zhangProductFailureProbabilityBound(
			result.bootstrapSuccess, maximumFailureProbability) : 1;
	result.nis = block.lambda_candidate_nis;
	result.nisThreshold = block.lambda_candidate_nis_threshold;
	ZhangExactMatrix parentRows;
	ZhangExactVector parentIntegers;
	const bool exactParent = result.rawFixedRank > 0 &&
		block.lambda_candidate_nis_valid &&
		block.lambda_candidate_nis <= block.lambda_candidate_nis_threshold &&
		zhangExactRowsFromNumeric(
			block.Ztrs, block.zfix, parentRows, parentIntegers);
	if (!exactParent)
	{
		return finishWithNamedFallback(result.rawFixedRank > 0
			? "PRODUCT_LATTICE_PARENT_NOT_EXACT_OR_NIS_REJECTED"
			: "PRODUCT_LATTICE_ILS_NOT_FIXED");
	}
	for (const auto& parentRow : parentRows)
	{
		if (parentRow.size() != proposalRows.size())
		{
			return finishWithNamedFallback(
				"PRODUCT_LATTICE_PARENT_DIMENSION_MISMATCH");
		}
		result.rows.push_back(
			zhangExactRowCombination(parentRow, proposalRows));
	}
	result.integers = std::move(parentIntegers);
	int finalRank = 0;
	const bool primitive = zhangExactPrimitiveRowLattice(
		result.rows, mean.size(), &finalRank) &&
		finalRank == static_cast<int>(result.rows.size());
	const bool probabilityPassed = zhangProductFailureProbabilityPassed(
		result.failureProbabilityBound, maximumFailureProbability);
	result.reliable = probabilityPassed && primitive;
	result.selectionSource = "MIXED_PRODUCT_LATTICE";
	if (!result.rows.empty())
	{
		MatrixXd fixedRows(result.rows.size(), mean.size());
		for (int row = 0; row < fixedRows.rows(); row++)
			fixedRows.row(row) =
				zhangExactRowToDouble(result.rows[row]).transpose();
		result.productGain = zhangIntegerConstraintProductGain(
			fixedRows, covariance, referenceInvariantProductCrossCovariance);
		const bool namedForestEdgeUnits =
			stage == "L1_GIVEN_WL_PAIR_SUBLATTICE";
		const int localReferenceNode = mean.size();
		std::vector<ZhangPairReliabilityEdge> pairEdges;
		for (const auto& pair : zhangRecoverCertifiedPairRelations(
			result.rows, result.integers, mean.size(), true))
		{
			if (namedForestEdgeUnits &&
				pair.firstNode != localReferenceNode &&
				pair.secondNode != localReferenceNode)
				continue;
			pairEdges.push_back({pair.firstNode, pair.secondNode, 0, 0});
		}
		result.certifiedPairRank = namedForestEdgeUnits
			? pairEdges.size()
			: zhangPairReliabilityForest(
				mean.size() + 1, pairEdges, 0).size();
	}
	result.failureReason = result.reliable
		? "NONE"
		: (!probabilityPassed
			? "PRODUCT_LATTICE_FAILURE_PROBABILITY_EXCEEDED"
			: "PRODUCT_LATTICE_FINAL_BLOCK_NOT_PRIMITIVE");
	if (namedPairFix.reliable &&
		(!result.reliable ||
		 namedPairFix.certifiedPairRank > result.certifiedPairRank ||
		 (namedPairFix.certifiedPairRank == result.certifiedPairRank &&
		  namedPairFix.productGain > result.productGain)))
	{
		const int generatedCandidates = result.generatedCandidates;
		const int reliableCandidates = result.reliableCandidates;
		result = namedPairFix;
		result.generatedCandidates = generatedCandidates;
		result.reliableCandidates = reliableCandidates;
	}
	traceStageResult();
	return result;
}

static ZhangProductRelationFixResult solveZhangProductRelations(
	Trace& trace,
	const KFState& state,
	const GinAR_mtx& networkAmbiguities,
	const ZhangProductRelationBasis& firstBasis,
	const ZhangProductRelationBasis& secondBasis,
	const GinAR_opt& options,
	GTime time,
	const ZhangPersistentHeldLattice* currentCertified = nullptr,
	const GinAR_mtx* currentCertifiedCoordinates = nullptr)
{
	ZhangProductRelationFixResult result;
	result.basisValid = firstBasis.valid && secondBasis.valid;
	result.fullTargetRank = firstBasis.fullTargetRank;
	result.mappableTargetRank = firstBasis.mappableTargetRank;
	result.namedOrderingValid = zhangProductNamedOrderingMatches(
		firstBasis, secondBasis);
	result.mappingValid = result.basisValid &&
		firstBasis.mappableTargetRank > 0 &&
		firstBasis.mappableTargetRank == secondBasis.mappableTargetRank &&
		result.namedOrderingValid &&
		firstBasis.transform.rows() == firstBasis.mappableTargetRank &&
		secondBasis.transform.rows() == firstBasis.mappableTargetRank &&
		firstBasis.affineOffsets.size() ==
			static_cast<std::size_t>(firstBasis.mappableTargetRank) &&
		secondBasis.affineOffsets.size() ==
			static_cast<std::size_t>(firstBasis.mappableTargetRank);
	if (!result.mappingValid)
	{
		result.status = "INVALID_RELATION_MAPPING";
		result.failureReason = !firstBasis.failureReason.empty()
			? firstBasis.failureReason
			: (!secondBasis.failureReason.empty()
				? secondBasis.failureReason
				: "FIRST_SECOND_RELATION_BASIS_MISMATCH");
		return result;
	}

	const int fullRank = result.mappableTargetRank;
	MatrixXd fullJointTransform(2 * fullRank, networkAmbiguities.aflt.size());
	fullJointTransform.topRows(fullRank) = firstBasis.transform;
	fullJointTransform.bottomRows(fullRank) = secondBasis.transform;
	VectorXd fullJointMean = fullJointTransform * networkAmbiguities.aflt;
	for (int row = 0; row < fullRank; row++)
	{
		fullJointMean(row) += firstBasis.affineOffsets.at(row).convert_to<double>();
		fullJointMean(fullRank + row) +=
			secondBasis.affineOffsets.at(row).convert_to<double>();
	}
	const MatrixXd fullJointCovariance = fullJointTransform *
		networkAmbiguities.Paflt * fullJointTransform.transpose();
	const ZhangIarProductGainSpectrum relationGainSpectrum =
		zhangIarProductGainSpectrum(
			fullJointCovariance,
			fullJointCovariance,
			MatrixXd::Identity(2 * fullRank, 2 * fullRank));
	MatrixXd fullWideLaneMap = MatrixXd::Zero(fullRank, 2 * fullRank);
	fullWideLaneMap.leftCols(fullRank) = MatrixXd::Identity(fullRank, fullRank);
	fullWideLaneMap.rightCols(fullRank) = -MatrixXd::Identity(fullRank, fullRank);
	const VectorXd fullWideLaneMean = fullWideLaneMap * fullJointMean;
	const MatrixXd fullWideLaneCovariance = fullWideLaneMap *
		fullJointCovariance * fullWideLaneMap.transpose();
	traceZhangProductPairAudit(
		trace, state, firstBasis, secondBasis,
		fullWideLaneMean, fullWideLaneCovariance,
		options, time, "BASELINE", currentCertified,
		currentCertifiedCoordinates);
	for (int local = 0; local < fullRank; local++)
	{
		const int namedIndex = firstBasis.mappableNamedIndices.at(local);
		const auto& relation = firstBasis.namedRelations.at(namedIndex);
		const double mean = fullWideLaneMean(local);
		const double variance = fullWideLaneCovariance(local, local);
		const double integer = std::round(mean);
		const double fractional = mean - integer;
		const double marginalNis = variance > 0 && std::isfinite(variance)
			? fractional * fractional / variance
			: std::numeric_limits<double>::infinity();
		trace << "\nZHANG_PRODUCT_RELATION_NAMED_FLOAT time="
			  << time.to_string(0)
			  << " stage=WL"
			  << " satellite=" << relation.satellite.id()
			  << " reference=" << relation.referenceSatellite.id()
			  << " named_index=" << namedIndex
			  << " mean_cycles=" << mean
			  << " fractional_cycles=" << fractional
			  << " variance_cycles2=" << variance
			  << " sigma_cycles=" << (variance > 0 ? std::sqrt(variance) : 0)
			  << " round_perr=" << round_perr(fractional, variance)
			  << " marginal_nis=" << marginalNis;
	}

	// B diagnostic: identity named rows, scalar ROUND, then conservative joint
	// NIS admission.  This never feeds the estimator or the ProductRelation
	// certificate; it separates named-edge fixability from mixed LAMBDA modes.
	GinAR_mtx namedRoundWideLane;
	namedRoundWideLane.aflt = fullWideLaneMean;
	namedRoundWideLane.Paflt = fullWideLaneCovariance;
	std::vector<int> scalarNamedRows;
	const double namedMaximumPerr =
		acsConfig.zhangPppAr.canonical_user_target_max_perr;
	for (int row = 0; row < fullRank; row++)
	{
		const double variance = fullWideLaneCovariance(row, row);
		const double integer = std::round(fullWideLaneMean(row));
		const double perr = round_perr(
			fullWideLaneMean(row) - integer, variance);
		if (std::isfinite(variance) && variance > 0 &&
			std::isfinite(perr) && perr <= namedMaximumPerr)
		{
			scalarNamedRows.push_back(row);
		}
	}
	namedRoundWideLane.Ztrs = MatrixXd::Zero(
		scalarNamedRows.size(), fullRank);
	namedRoundWideLane.zfix = VectorXd::Zero(scalarNamedRows.size());
	for (int local = 0; local < static_cast<int>(scalarNamedRows.size()); local++)
	{
		const int row = scalarNamedRows[local];
		namedRoundWideLane.Ztrs(local, row) = 1;
		namedRoundWideLane.zfix(local) = std::round(fullWideLaneMean(row));
	}
	const int namedRoundCandidates = scalarNamedRows.size();
	int namedRoundRetained = 0;
	if (namedRoundCandidates > 0)
	{
		namedRoundRetained = retainNisCompatibleNamedRows(
			trace, namedRoundWideLane, time,
			"PRODUCT_RELATION_NAMED_ROUND_WL_SHADOW");
	}
	trace << "\nZHANG_PRODUCT_RELATION_NAMED_ROUND_NIS_SHADOW time="
		  << time.to_string(0)
		  << " stage=WL"
		  << " named_relations=" << fullRank
		  << " rounded_candidates=" << namedRoundCandidates
		  << " retained_named=" << namedRoundRetained
		  << " scalar_maximum_perr=" << namedMaximumPerr
		  << " scalar_gate=PER_NAMED_ROW"
		  << " feedback=SHADOW_NONE";
	result.namedRoundWideLaneCandidates = namedRoundCandidates;
	result.namedRoundWideLaneRetained = namedRoundRetained;

	struct EvaluatedProductBranch
	{
		ProductParBranch branch;
		std::map<std::size_t, ZhangExactInteger> wideLane;
		std::map<std::size_t, ZhangExactInteger> first;
		std::map<std::size_t, ZhangExactInteger> second;
		bool wideLaneReliable = false;
		bool inheritedWideLaneCertificate = false;
		bool firstReliable = false;
		double maximumWideLanePerr = 1;
		double maximumWideLaneMarginalRoundPerr = 1;
		double wideLaneParentFailureProbabilityBound = 1;
		double maximumFirstPerr = 1;
		bool wideLaneParentAccepted = false;
		bool firstSignalParentAccepted = false;
		bool productConstraintsReliable = false;
		double constraintJointNis = std::numeric_limits<double>::quiet_NaN();
		double constraintJointNisThreshold =
			std::numeric_limits<double>::quiet_NaN();
		ZhangExactMatrix wideLaneFixedRows;
		ZhangExactVector wideLaneFixedValues;
		ZhangExactMatrix firstSignalFixedRows;
		ZhangExactVector firstSignalFixedValues;
		std::vector<int> recoverableNamedLocalIndices;
		int worstNamedLocalPosition = -1;
		int parentBranchRank = 0;
		int parentFixedRank = 0;
		std::map<int, ZhangExactInteger> inheritedWideLaneValues;
		std::string failureReason = "NOT_EVALUATED";
	};
	auto evaluate = [&](
		const std::vector<int>& selected,
		const EvaluatedProductBranch* inheritedParent)
	{
		EvaluatedProductBranch evaluated;
		evaluated.branch.namedRelationIndices = selected;
		evaluated.branch.integerRank = selected.size();
		evaluated.branch.parentBranchRank = inheritedParent
			? inheritedParent->branch.integerRank : 0;
		evaluated.parentBranchRank = evaluated.branch.parentBranchRank;
		evaluated.branch.canonicalHnf =
			productSubsetCanonicalHnf(firstBasis, selected);
		evaluated.branch.componentCoverageGain =
			productSubsetComponentCoverageGain(
				firstBasis, secondBasis, selected);
		const int rank = selected.size();
		MatrixXd firstTransform(rank, networkAmbiguities.aflt.size());
		MatrixXd secondTransform(rank, networkAmbiguities.aflt.size());
		VectorXd firstOffset(rank);
		VectorXd secondOffset(rank);
		for (int row = 0; row < rank; row++)
		{
			firstTransform.row(row) = firstBasis.transform.row(selected[row]);
			secondTransform.row(row) = secondBasis.transform.row(selected[row]);
			firstOffset(row) = firstBasis.affineOffsets.at(selected[row])
				.convert_to<double>();
			secondOffset(row) = secondBasis.affineOffsets.at(selected[row])
				.convert_to<double>();
		}
		MatrixXd jointTransform(2 * rank, networkAmbiguities.aflt.size());
		jointTransform.topRows(rank) = firstTransform;
		jointTransform.bottomRows(rank) = secondTransform;
		VectorXd jointMean = jointTransform * networkAmbiguities.aflt;
		jointMean.head(rank) += firstOffset;
		jointMean.tail(rank) += secondOffset;
		const MatrixXd jointCovariance = jointTransform *
			networkAmbiguities.Paflt * jointTransform.transpose();

		ZhangIarFunctional selectedConstraints(2 * rank, 2 * fullRank);
		for (int row = 0; row < rank; row++)
		{
			const int index = selected[row];
			selectedConstraints.insert(row, index) = 1;
			selectedConstraints.insert(row, fullRank + index) = -1;
			selectedConstraints.insert(rank + row, index) = 1;
		}
		selectedConstraints.makeCompressed();
		evaluated.branch.productInformationGain =
			zhangNamedProductInformationGain(
				fullJointCovariance, selectedConstraints);

		GinAR_mtx wideLane;
		wideLane.aflt = jointMean.head(rank) - jointMean.tail(rank);
		MatrixXd wlMap = MatrixXd::Zero(rank, 2 * rank);
		wlMap.leftCols(rank) = MatrixXd::Identity(rank, rank);
		wlMap.rightCols(rank) = -MatrixXd::Identity(rank, rank);
		// This multiplication is exactly Q11+Q22-Q12-Q21.  Do not replace
		// it with marginal covariance addition: the cross-frequency blocks
		// are part of the admissible WL transform.
		wideLane.Paflt = wlMap * jointCovariance * wlMap.transpose();
		const VectorXd wideLaneMean = wideLane.aflt;
		const MatrixXd wideLaneCovariance = wideLane.Paflt;
		double worstMarginalNis = -1;
		evaluated.branch.maximumNamedPerr = 0;
		for (int row = 0; row < rank; row++)
		{
			const double variance = wideLaneCovariance(row, row);
			const double fractional = wideLaneMean(row) -
				std::round(wideLaneMean(row));
			const double perr = round_perr(fractional, variance);
			const double marginalNis = variance > 0 && std::isfinite(variance)
				? fractional * fractional / variance
				: std::numeric_limits<double>::infinity();
			evaluated.branch.maximumNamedPerr = std::max(
				evaluated.branch.maximumNamedPerr, perr);
			if (marginalNis > worstMarginalNis)
			{
				worstMarginalNis = marginalNis;
				evaluated.worstNamedLocalPosition = row;
			}
		}
		bool inheritedComplete = inheritedParent &&
			inheritedParent->wideLaneParentFailureProbabilityBound <=
				1 - options.sucthr &&
			!inheritedParent->inheritedWideLaneValues.empty();
		std::map<std::size_t, ZhangExactInteger> localWideLane;
		if (inheritedComplete)
		{
			for (int row = 0; row < rank; row++)
			{
				auto value = inheritedParent->inheritedWideLaneValues.find(
					selected[row]);
				if (value == inheritedParent->inheritedWideLaneValues.end())
				{
					inheritedComplete = false;
					break;
				}
				localWideLane[row] = value->second;
			}
		}
		int rawWideLaneFixed = 0;
		if (inheritedComplete)
		{
			rawWideLaneFixed = rank;
			evaluated.inheritedWideLaneCertificate = true;
			evaluated.branch.inheritedFromParentFixedLattice = true;
			evaluated.parentFixedRank = inheritedParent->parentFixedRank > 0
				? inheritedParent->parentFixedRank
				: inheritedParent->branch.rawPartialFixedRank;
			evaluated.wideLaneParentFailureProbabilityBound =
				inheritedParent->wideLaneParentFailureProbabilityBound;
			evaluated.wideLaneParentAccepted = true;
			evaluated.productConstraintsReliable = true;
			evaluated.constraintJointNis =
				inheritedParent->constraintJointNis;
			evaluated.constraintJointNisThreshold =
				inheritedParent->constraintJointNisThreshold;
			for (int row = 0; row < rank; row++)
			{
				ZhangExactVector unit(rank);
				unit[row] = 1;
				evaluated.wideLaneFixedRows.push_back(std::move(unit));
				evaluated.wideLaneFixedValues.push_back(localWideLane.at(row));
			}
		}
		else
		{
			GinAR_opt namedOptions = options;
			namedOptions.min_lambda_fix_count = 1;
			rawWideLaneFixed = rankAwareGnssAr(
				trace, wideLane, namedOptions, time,
				"PRODUCT_RELATION_NAMED_WL_BEAM_SHADOW", true);
		}
		evaluated.branch.rawPartialFixedRank = rawWideLaneFixed;
		evaluated.branch.partialFixFraction = rank > 0
			? static_cast<double>(rawWideLaneFixed) / rank : 0;
		if (std::isfinite(wideLane.lambda_candidate_nis) &&
			wideLane.lambda_candidate_nis_threshold > 0)
		{
			evaluated.branch.normalizedCandidateNis =
				wideLane.lambda_candidate_nis /
				wideLane.lambda_candidate_nis_threshold;
		}
		if (rawWideLaneFixed <= 0)
		{
			evaluated.failureReason = "WL_NOT_FIXED";
			return evaluated;
		}
		if (!inheritedComplete)
		{
			const bool parentAccepted = rawWideLaneFixed > 0 &&
				wideLane.lambda_candidate_nis_valid &&
				wideLane.lambda_candidate_nis <=
					wideLane.lambda_candidate_nis_threshold;
			evaluated.wideLaneParentAccepted = parentAccepted &&
				zhangExactRowsFromNumeric(
					wideLane.Ztrs, wideLane.zfix,
					evaluated.wideLaneFixedRows,
					evaluated.wideLaneFixedValues);
			if (evaluated.wideLaneParentAccepted)
			{
				evaluated.constraintJointNis = wideLane.lambda_candidate_nis;
				evaluated.constraintJointNisThreshold =
					wideLane.lambda_candidate_nis_threshold;
				evaluated.productConstraintsReliable = true;
			}
			const auto promoted =
				promoteNamedProductCoordinatesFromAcceptedParent(
					wideLane, rank, parentAccepted);
			localWideLane = promoted.values;
			evaluated.parentFixedRank = promoted.parentFixedRank;
			evaluated.wideLaneParentFailureProbabilityBound =
				parentAccepted ? 1 - options.sucthr : 1;
			if (zhangProductPairAuditEpoch(time) && rank == fullRank &&
				rawWideLaneFixed > 0)
			{
				const auto pairs = recoverCertifiedPairProductCoordinates(
					wideLane, rank, parentAccepted);
				std::vector<ZhangPairReliabilityEdge> graphEdges;
				for (const auto& pair : pairs)
				{
					VectorXd pairRow = VectorXd::Zero(rank);
					if (pair.firstNode < rank) pairRow(pair.firstNode) += 1;
					if (pair.secondNode < rank) pairRow(pair.secondNode) -= 1;
					const double pairMean = pairRow.dot(wideLaneMean);
					const double pairVariance =
						(pairRow.transpose() * wideLaneCovariance * pairRow)(0, 0);
					const double pairFractional = pairMean - std::round(pairMean);
					const double pairPerr = pairVariance > 0
						? round_perr(pairFractional, pairVariance) : 1;
					graphEdges.push_back({pair.firstNode, pair.secondNode, 0, 0});
					trace << "\nZHANG_PRODUCT_RELATION_MIXED_LATTICE_PAIR_CERTIFICATE time="
						  << time.to_string(0)
						  << " satellite="
						  << zhangProductPairNodeId(firstBasis, pair.firstNode)
						  << " reference="
						  << zhangProductPairNodeId(firstBasis, pair.secondNode)
						  << " integer_value=" << pair.value
						  << " parent_fixed_rank=" << rawWideLaneFixed
						  << " exact_hnf_membership=1"
						  << " parent_joint_nis=" << wideLane.lambda_candidate_nis
						  << " parent_joint_nis_threshold="
						  << wideLane.lambda_candidate_nis_threshold
						  << " named_marginal_perr=" << pairPerr
						  << " named_marginal_reliable="
						  << (pairPerr <= acsConfig.zhangPppAr.canonical_user_target_max_perr)
						  << " evidence_source=HELD_LATTICE_EXACT_DERIVED"
						  << " certificate_scope=PAIR_EDGE"
						  << " frontend_feedback=0";
				}
				const auto pairForest = zhangPairReliabilityForest(
					rank + 1, graphEdges, 0);
				trace << "\nZHANG_PRODUCT_RELATION_MIXED_LATTICE_PAIR_SUMMARY time="
					  << time.to_string(0)
					  << " parent_candidate_accepted=" << parentAccepted
					  << " parent_fixed_rank=" << rawWideLaneFixed
					  << " recovered_star_edges=" << promoted.values.size()
					  << " recovered_pair_edges=" << pairs.size()
					  << " recovered_pair_graph_rank=" << pairForest.size()
					  << " residual_mixed_mode_class="
					  << (pairs.empty() ? "CONDITIONING_ONLY_INTEGER"
						: "PAIR_EDGES_RECOVERED")
					  << " frontend_feedback=0";
				if (parentAccepted && wideLane.Ztrs.cols() == rank)
				{
					const MatrixXd symmetric = 0.5 *
						(wideLaneCovariance + wideLaneCovariance.transpose());
					const auto conditioned = zhangConditionExactProductRows(
						wideLaneMean, symmetric, wideLane.Ztrs, wideLane.zfix);
					const double pairTraceBefore =
						zhangReferenceInvariantPairTrace(symmetric);
					const double pairTraceAfter = conditioned.valid
						? zhangReferenceInvariantPairTrace(conditioned.covariance)
						: pairTraceBefore;
					trace << "\nZHANG_PRODUCT_RELATION_MIXED_REFERENCE_INVARIANT_GAIN time="
						  << time.to_string(0)
						  << " fixed_rank=" << rawWideLaneFixed
						  << " pair_trace_before_cycles2=" << pairTraceBefore
						  << " pair_trace_after_cycles2=" << pairTraceAfter
						  << " delta_pair_trace_cycles2="
						  << pairTraceBefore - pairTraceAfter
						  << " capture_fraction="
						  << (pairTraceBefore > 0
							? (pairTraceBefore - pairTraceAfter) / pairTraceBefore : 0)
						  << " conditioning_valid=" << conditioned.valid
						  << " reference_invariant=1 feedback=0";
					// R-Q5: remove all exactly recoverable pair consequences from the
					// accepted mixed lattice.  Only the primitive residual quotient is
					// allowed to condition this private posterior; it never becomes a
					// product certificate or authoritative estimator feedback.
					ZhangExactMatrix mixedRows;
					ZhangExactVector mixedValues;
					bool mixedExact = wideLane.Ztrs.rows() == wideLane.zfix.size();
					for (int fixedRow = 0; fixedRow < wideLane.Ztrs.rows() && mixedExact;
						 fixedRow++)
					{
						ZhangExactVector exactRow(rank);
						for (int column = 0; column < rank; column++)
						{
							const long long value = std::llround(
								wideLane.Ztrs(fixedRow, column));
							mixedExact &= std::abs(
								wideLane.Ztrs(fixedRow, column) - value) <= 1e-8;
							exactRow[column] = value;
						}
						const long long integer = std::llround(wideLane.zfix(fixedRow));
						mixedExact &= std::abs(wideLane.zfix(fixedRow) - integer) <= 1e-8;
						mixedRows.push_back(std::move(exactRow));
						mixedValues.push_back(integer);
					}
					ZhangExactMatrix pairRows;
					ZhangExactVector pairValues;
					for (const auto& pair : pairs)
					{
						ZhangExactVector pairRow(rank);
						if (pair.firstNode < rank) pairRow[pair.firstNode] += 1;
						if (pair.secondNode < rank) pairRow[pair.secondNode] -= 1;
						pairRows.push_back(std::move(pairRow));
						pairValues.push_back(pair.value);
					}
					const auto conditioningOnly = mixedExact
						? zhangExactHeldQuotientAudit(
							mixedRows, pairRows, pairValues)
						: ZhangHeldQuotientAudit{};
					if (conditioningOnly.valid && conditioningOnly.quotientRank > 0)
					{
						MatrixXd privateRows = MatrixXd::Zero(
							conditioningOnly.quotientRank, rank);
						VectorXd privateValues = VectorXd::Zero(
							conditioningOnly.quotientRank);
						ZhangExactMatrix privateExactNamedRows;
						ZhangExactVector privateExactValues;
						for (int q = 0; q < conditioningOnly.quotientRank; q++)
						{
							const auto exactRow = zhangExactRowCombination(
								conditioningOnly.quotientTargetCoordinates[q], mixedRows);
							privateExactNamedRows.push_back(exactRow);
							for (int column = 0; column < rank; column++)
								privateRows(q, column) = exactRow[column].convert_to<double>();
							ZhangExactInteger value = 0;
							for (int row = 0; row < static_cast<int>(mixedValues.size()); row++)
								value += conditioningOnly.quotientTargetCoordinates[q][row] *
									mixedValues[row];
							privateValues(q) = value.convert_to<double>();
							privateExactValues.push_back(value);
						}
						const auto privatePosterior = zhangConditionExactProductRows(
							wideLaneMean, symmetric, privateRows, privateValues);
						ZhangPersistentHeldLattice privateCertified;
						if (currentCertified) privateCertified = *currentCertified;
						const auto privatePhysical = zhangBuildComponentQuotientInput(
							state, firstBasis, &secondBasis, privateExactNamedRows);
						bool privateCertifiedValid = privatePhysical.valid &&
							privatePhysical.targetPhysicalRows.size() ==
								privateExactValues.size();
						if (privateCertifiedValid)
						{
							for (int row = 0;
								 row < static_cast<int>(privateExactValues.size()); row++)
							{
								ZhangPersistentHeldRow certifiedRow;
								for (int column = 0;
									 column < static_cast<int>(
										privatePhysical.physicalColumns.size()); column++)
								{
									const auto coefficient =
										privatePhysical.targetPhysicalRows[row][column];
									if (coefficient != 0)
										certifiedRow.coefficients[
											privatePhysical.physicalColumns[column]] = coefficient;
								}
								certifiedRow.value = privateExactValues[row];
								privateCertified.rows.push_back(std::move(certifiedRow));
							}
							normalisePersistentHeldLattice(privateCertified);
							privateCertifiedValid = privateCertified.consistent;
						}
						trace << "\nZHANG_CONDITIONING_ONLY_MIXED_RQ5 time="
							  << time.to_string(0)
							  << " mixed_fixed_rank=" << mixedRows.size()
							  << " exact_pair_consequence_rank="
							  << conditioningOnly.heldIntersectionRank
							  << " conditioning_only_rank="
							  << conditioningOnly.quotientRank
							  << " private_conditioning_valid=" << privatePosterior.valid
							  << " private_certificate_lattice_valid="
							  << privateCertifiedValid
							  << " product_certificate_authorized=0 feedback=0";
						if (privatePosterior.valid && privateCertifiedValid)
						{
							traceZhangProductPairAudit(
								trace, state, firstBasis, secondBasis,
								privatePosterior.mean, privatePosterior.covariance,
								options, time, "PRIVATE_CONDITIONING_ONLY",
								&privateCertified, currentCertifiedCoordinates);
						}
					}
					Eigen::SelfAdjointEigenSolver<MatrixXd> eigen(symmetric);
					if (eigen.info() == Eigen::Success)
					{
						const int weakModes = std::min(8, rank);
						for (int fixedRow = 0;
							 fixedRow < wideLane.Ztrs.rows(); fixedRow++)
						{
							const VectorXd row = wideLane.Ztrs.row(fixedRow).transpose();
							const double norm = row.norm();
							if (!(norm > 0)) continue;
							for (int mode = 0; mode < weakModes; mode++)
							{
							const int index = rank - 1 - mode;
							const VectorXd eigenvector =
								eigen.eigenvectors().col(index);
							const double eigenvalue = eigen.eigenvalues()(index);
							const double captured = conditioned.valid && eigenvalue > 0
								? std::clamp((eigenvector.transpose() *
									conditioned.reduction * eigenvector)(0, 0) /
									eigenvalue, 0.0, 1.0) : 0;
							trace << "\nZHANG_PRODUCT_RELATION_MIXED_WEAK_MODE_ALIGNMENT time="
									  << time.to_string(0)
									  << " fixed_row=" << fixedRow
									  << " weak_mode=" << mode + 1
									  << " absolute_cosine="
								  << std::abs(row.dot(
									  eigenvector) / norm)
								  << " eigenvalue_cycles2="
								  << eigenvalue
								  << " joint_mode_capture_fraction=" << captured
									  << " feedback=0";
							}
						}
					}
				}
			}
		}
		evaluated.branch.recoveredNamedRank = localWideLane.size();
		for (const auto& [local, value] : localWideLane)
		{
			(void)value;
			evaluated.recoverableNamedLocalIndices.push_back(local);
		}
		if (localWideLane.size() != static_cast<std::size_t>(rank))
		{
			if (!localWideLane.empty() &&
				evaluated.wideLaneParentFailureProbabilityBound <=
					1 - options.sucthr)
			{
				for (const auto& [local, value] : localWideLane)
				{
					if (local >= 0 && local < rank)
					{
						evaluated.inheritedWideLaneValues[selected[local]] = value;
					}
				}
			}
			evaluated.failureReason = localWideLane.empty()
				? "WL_DECORRELATED_PARTIAL_HAS_NO_NAMED_CERTIFICATE"
				: "WL_DECORRELATED_PARTIAL_NAMED_SEED_ONLY";
			return evaluated;
		}
		VectorXd wideLaneIntegers(rank);
		evaluated.branch.maxPerr = 0;
		evaluated.maximumWideLaneMarginalRoundPerr = 0;
		for (int row = 0; row < rank; row++)
		{
			wideLaneIntegers(row) = localWideLane.at(row).convert_to<double>();
			const double marginalRoundPerr = round_perr(
				wideLaneMean(row) - wideLaneIntegers(row),
				wideLaneCovariance(row, row));
			evaluated.maximumWideLaneMarginalRoundPerr = std::max(
				evaluated.maximumWideLaneMarginalRoundPerr,
				marginalRoundPerr);
		}
		const double inheritedRisk = evaluated.inheritedWideLaneCertificate
			? evaluated.wideLaneParentFailureProbabilityBound
			: evaluated.maximumWideLaneMarginalRoundPerr;
		evaluated.branch.maxPerr = inheritedRisk;
		evaluated.maximumWideLanePerr = inheritedRisk;
		const auto wideLaneNis = assessZhangIntegerCandidateNis(
			wideLaneIntegers - wideLaneMean,
			wideLaneCovariance,
			options.lambda_candidate_nis_alpha > 0
				? options.lambda_candidate_nis_alpha : 1e-6);
		evaluated.wideLaneReliable = evaluated.inheritedWideLaneCertificate
			? evaluated.wideLaneParentFailureProbabilityBound <=
				1 - options.sucthr
			: evaluated.branch.maxPerr <= 1 - options.sucthr &&
				wideLaneNis.valid && wideLaneNis.nis <= wideLaneNis.threshold;
		if (!evaluated.wideLaneReliable)
		{
			evaluated.branch.jointNis = wideLaneNis.nis;
			evaluated.branch.jointNisThreshold = wideLaneNis.threshold;
			evaluated.failureReason = "WL_PERR_OR_NIS_GATE_FAILED";
			return evaluated;
		}
		for (const auto& [local, value] : localWideLane)
		{
			evaluated.inheritedWideLaneValues[selected[local]] = value;
		}
		trace << "\nZHANG_PRODUCT_RELATION_NAMED_WL_CERTIFICATE time="
			  << time.to_string(0)
			  << " source=" << (evaluated.inheritedWideLaneCertificate
				? "INHERITED_ACCEPTED_PARENT_LATTICE"
				: "DIRECT_COMPLETE_NAMED_BRANCH")
			  << " parent_branch_rank=" << evaluated.parentBranchRank
			  << " parent_fixed_rank=" << evaluated.parentFixedRank
			  << " named_rank=" << rank
			  << " parent_failure_probability_bound="
			  << evaluated.wideLaneParentFailureProbabilityBound
			  << " maximum_marginal_round_perr_diagnostic="
			  << evaluated.maximumWideLaneMarginalRoundPerr
			  << " exact_hnf_membership=1"
			  << " rerun_lambda=" << !evaluated.inheritedWideLaneCertificate;

		ZhangIarFunctional wlConstraints(rank, 2 * rank);
		for (int row = 0; row < rank; row++)
		{
			wlConstraints.insert(row, row) = 1;
			wlConstraints.insert(row, rank + row) = -1;
		}
		wlConstraints.makeCompressed();
		const ZhangIntegerConditionedState conditioned =
			zhangConditionIntegersExact(
				jointMean, jointCovariance, wlConstraints, wideLaneIntegers);
		if (!conditioned.valid)
		{
			evaluated.failureReason = conditioned.failureReason;
			return evaluated;
		}

		GinAR_mtx firstSignal;
		firstSignal.aflt = conditioned.mean.head(rank);
		firstSignal.Paflt = conditioned.covariance.topLeftCorner(rank, rank);
		const VectorXd firstMean = firstSignal.aflt;
		const MatrixXd firstCovariance = firstSignal.Paflt;
		for (int row = 0; row < rank; row++)
		{
			const int namedIndex = firstBasis.mappableNamedIndices.at(selected[row]);
			const auto& relation = firstBasis.namedRelations.at(namedIndex);
			const double mean = firstMean(row);
			const double variance = firstCovariance(row, row);
			const double integer = std::round(mean);
			const double fractional = mean - integer;
			const double marginalNis = variance > 0 && std::isfinite(variance)
				? fractional * fractional / variance
				: std::numeric_limits<double>::infinity();
			trace << "\nZHANG_PRODUCT_RELATION_NAMED_FLOAT time="
				  << time.to_string(0)
				  << " stage=L1_GIVEN_WL"
				  << " candidate_rank=" << rank
				  << " satellite=" << relation.satellite.id()
				  << " reference=" << relation.referenceSatellite.id()
				  << " named_index=" << namedIndex
				  << " mean_cycles=" << mean
				  << " fractional_cycles=" << fractional
				  << " variance_cycles2=" << variance
				  << " sigma_cycles=" << (variance > 0 ? std::sqrt(variance) : 0)
				  << " round_perr=" << round_perr(fractional, variance)
				  << " marginal_nis=" << marginalNis;
		}
		GinAR_opt namedFirstOptions = options;
		namedFirstOptions.min_lambda_fix_count = 1;
		const int rawFirstFixed = rankAwareGnssAr(
			trace, firstSignal, namedFirstOptions, time,
			"PRODUCT_RELATION_NAMED_L1_BEAM_SHADOW", true);
		if (rawFirstFixed <= 0)
		{
			evaluated.failureReason = "L1_NOT_FIXED";
			return evaluated;
		}
		evaluated.firstSignalParentAccepted =
			firstSignal.lambda_candidate_nis_valid &&
			firstSignal.lambda_candidate_nis <=
				firstSignal.lambda_candidate_nis_threshold &&
			zhangExactRowsFromNumeric(
				firstSignal.Ztrs, firstSignal.zfix,
				evaluated.firstSignalFixedRows,
				evaluated.firstSignalFixedValues);
		if (evaluated.wideLaneParentAccepted &&
			evaluated.firstSignalParentAccepted)
		{
			const int constraintRows =
				wideLane.Ztrs.rows() + firstSignal.Ztrs.rows();
			MatrixXd jointRows = MatrixXd::Zero(
				constraintRows, 2 * rank);
			VectorXd jointIntegers = VectorXd::Zero(constraintRows);
			jointRows.topLeftCorner(
				wideLane.Ztrs.rows(), rank) = wideLane.Ztrs;
			jointRows.block(0, rank,
				wideLane.Ztrs.rows(), rank) = -wideLane.Ztrs;
			jointIntegers.head(wideLane.zfix.size()) = wideLane.zfix;
			jointRows.block(wideLane.Ztrs.rows(), 0,
				firstSignal.Ztrs.rows(), rank) = firstSignal.Ztrs;
			jointIntegers.tail(firstSignal.zfix.size()) = firstSignal.zfix;
			const auto constraintNis = assessZhangIntegerCandidateNis(
				jointIntegers - jointRows * jointMean,
				jointRows * jointCovariance * jointRows.transpose(),
				options.lambda_candidate_nis_alpha > 0
					? options.lambda_candidate_nis_alpha : 1e-6);
			evaluated.constraintJointNis = constraintNis.nis;
			evaluated.constraintJointNisThreshold = constraintNis.threshold;
			evaluated.productConstraintsReliable = constraintNis.valid &&
				constraintNis.nis <= constraintNis.threshold;
		}
		auto localFirst = recoverCertifiedNamedProductCoordinates(firstSignal, rank);
		if (localFirst.size() != static_cast<std::size_t>(rank))
		{
			evaluated.failureReason = "L1_DECORRELATED_PARTIAL_NOT_NAMED_CERTIFICATE";
			return evaluated;
		}
		VectorXd firstIntegers(rank);
		double maximumFirstPerr = 0;
		for (int row = 0; row < rank; row++)
		{
			firstIntegers(row) = localFirst.at(row).convert_to<double>();
			maximumFirstPerr = std::max(
				maximumFirstPerr,
				round_perr(
					firstMean(row) - firstIntegers(row),
					firstCovariance(row, row)));
		}
		evaluated.maximumFirstPerr = maximumFirstPerr;
		evaluated.branch.maxPerr = std::max(
			evaluated.branch.maxPerr, maximumFirstPerr);
		const auto firstNis = assessZhangIntegerCandidateNis(
			firstIntegers - firstMean,
			firstCovariance,
			options.lambda_candidate_nis_alpha > 0
				? options.lambda_candidate_nis_alpha : 1e-6);
		evaluated.firstReliable =
			maximumFirstPerr <= 1 - options.sucthr &&
			firstNis.valid && firstNis.nis <= firstNis.threshold;

		ZhangIarFunctional admissibleRows(2 * rank, 2 * rank);
		VectorXd admissibleIntegers(2 * rank);
		for (int row = 0; row < rank; row++)
		{
			admissibleRows.insert(row, row) = 1;
			admissibleRows.insert(row, rank + row) = -1;
			admissibleRows.insert(rank + row, row) = 1;
			admissibleIntegers(row) = wideLaneIntegers(row);
			admissibleIntegers(rank + row) = firstIntegers(row);
		}
		admissibleRows.makeCompressed();
		const auto jointNis = assessZhangIntegerCandidateNis(
			admissibleIntegers - admissibleRows * jointMean,
			admissibleRows * jointCovariance * admissibleRows.transpose(),
			options.lambda_candidate_nis_alpha > 0
				? options.lambda_candidate_nis_alpha : 1e-6);
		evaluated.branch.jointNis = jointNis.nis;
		evaluated.branch.jointNisThreshold = jointNis.threshold;
		evaluated.branch.reliabilityPassed =
			evaluated.wideLaneReliable && evaluated.firstReliable &&
			jointNis.valid && jointNis.nis <= jointNis.threshold;
		if (!evaluated.branch.reliabilityPassed)
		{
			evaluated.failureReason = "L1_OR_JOINT_NIS_GATE_FAILED";
			return evaluated;
		}
		for (int row = 0; row < rank; row++)
		{
			const std::size_t namedIndex =
				firstBasis.mappableNamedIndices.at(selected[row]);
			evaluated.wideLane[namedIndex] = localWideLane.at(row);
			evaluated.first[namedIndex] = localFirst.at(row);
			evaluated.second[namedIndex] =
				localFirst.at(row) - localWideLane.at(row);
		}
		evaluated.failureReason = "NONE";
		return evaluated;
	};

	struct ProductBranchRequest
	{
		std::vector<int> selected;
		std::optional<EvaluatedProductBranch> inheritedParent;
	};
	std::vector<ProductBranchRequest> frontier(1);
	frontier.front().selected.resize(fullRank);
	std::iota(frontier.front().selected.begin(),
		frontier.front().selected.end(), 0);
	std::set<std::vector<int>> seen;
	std::optional<EvaluatedProductBranch> best;
	std::optional<EvaluatedProductBranch> bestProductConstraints;
	const int minimumRank = std::min(
		fullRank, acsConfig.zhangPppAr.product_relation_minimum_rank);
	// HYBRID_PRODUCT_WL_L1 solves the legal product lattice directly below.
	// The legacy full named branch scales with the growing network lattice and
	// must not remain on this online path: even one evaluation dominated the
	// late frozen epochs.  Its control experiment belongs in an offline replay.
	const int maximumNamedEvaluations =
		acsConfig.zhangPppAr.integer_strategy == "HYBRID_PRODUCT_WL_L1"
			? 0
			: acsConfig.zhangPppAr.product_relation_maximum_evaluations;
	while (!frontier.empty() &&
		result.evaluatedBranches < maximumNamedEvaluations)
	{
		std::vector<EvaluatedProductBranch> level;
		for (const auto& request : frontier)
		{
			const auto& selected = request.selected;
			if (!seen.insert(selected).second ||
				result.evaluatedBranches >=
					acsConfig.zhangPppAr.product_relation_maximum_evaluations)
			{
				continue;
			}
			level.push_back(evaluate(selected,
				request.inheritedParent ? &*request.inheritedParent : nullptr));
			result.evaluatedBranches++;
		}
		if (level.empty()) break;
		auto better = [](const auto& left, const auto& right)
		{
			const auto leftScore = zhangProductParScore(left.branch);
			const auto rightScore = zhangProductParScore(right.branch);
			if (leftScore < rightScore) return false;
			if (rightScore < leftScore) return true;
			return left.branch.canonicalHnf < right.branch.canonicalHnf;
		};
		std::sort(level.begin(), level.end(), better);
		for (const auto& candidate : level)
		{
			if (!candidate.productConstraintsReliable) continue;
			auto constraintBetter = [](const auto& left, const auto& right)
			{
				if (left.branch.recoveredNamedRank !=
					right.branch.recoveredNamedRank)
					return left.branch.recoveredNamedRank >
						right.branch.recoveredNamedRank;
				if (left.branch.productInformationGain !=
					right.branch.productInformationGain)
					return left.branch.productInformationGain >
						right.branch.productInformationGain;
				const auto leftRank = left.wideLaneFixedRows.size() +
					left.firstSignalFixedRows.size();
				const auto rightRank = right.wideLaneFixedRows.size() +
					right.firstSignalFixedRows.size();
				return leftRank > rightRank;
			};
			if (!bestProductConstraints ||
				constraintBetter(candidate, *bestProductConstraints))
				bestProductConstraints = candidate;
		}
		if (!best || better(level.front(), *best)) best = level.front();
		if (level.front().branch.reliabilityPassed) break;
		frontier.clear();
		const int retained = std::min(
			static_cast<int>(level.size()),
			acsConfig.zhangPppAr.product_relation_beam_width);
		for (int branch = 0; branch < retained; branch++)
		{
			const auto& evaluated = level[branch];
			const auto& selected = evaluated.branch.namedRelationIndices;
			if (static_cast<int>(selected.size()) <= minimumRank) continue;

			const int removed = evaluated.worstNamedLocalPosition;
			const int removedNamed = removed >= 0 &&
				removed < static_cast<int>(selected.size())
				? selected.at(removed) : -1;
			for (auto child : zhangProductNamedBackwardChildren(
				selected, evaluated.recoverableNamedLocalIndices,
				removed, minimumRank))
			{
				const bool backwardChild =
					child.size() + 1 == selected.size() &&
					removedNamed >= 0 &&
					std::find(child.begin(), child.end(), removedNamed) == child.end();
				trace << "\nZHANG_PRODUCT_RELATION_NAMED_BACKWARD_STEP time="
					  << time.to_string(0)
					  << " child_source="
					  << (backwardChild ? "WORST_NAMED_REMOVAL" : "EXACT_NAMED_SEED")
					  << " parent_rank=" << selected.size()
					  << " child_rank=" << child.size()
					  << " removed_local_relation="
					  << (backwardChild ? removedNamed : -1)
					  << " raw_partial_fixed_rank="
					  << evaluated.branch.rawPartialFixedRank
					  << " recovered_named_rank="
					  << evaluated.branch.recoveredNamedRank
					  << " partial_fix_fraction="
					  << evaluated.branch.partialFixFraction;
				if (!seen.contains(child))
				{
					ProductBranchRequest request;
					request.selected = std::move(child);
					if (!backwardChild &&
						!evaluated.inheritedWideLaneValues.empty())
					{
						request.inheritedParent = evaluated;
					}
					frontier.push_back(std::move(request));
				}
			}
		}
		std::sort(frontier.begin(), frontier.end(), [](const auto& a, const auto& b)
			{ return a.selected < b.selected; });
		frontier.erase(std::unique(frontier.begin(), frontier.end(),
			[](const auto& a, const auto& b)
			{ return a.selected == b.selected; }), frontier.end());
	}
	// The direct product-lattice solver below is independent of the legacy
	// named-subset control.  In HYBRID_PRODUCT_WL_L1 that control is deliberately
	// disabled, so an empty `best` is valid and must never short-circuit or be
	// dereferenced before the direct solve.
	if (best)
	{
	result.selectedNamedRelationIndices.clear();
	for (int local : best->branch.namedRelationIndices)
	{
		result.selectedNamedRelationIndices.push_back(
			firstBasis.mappableNamedIndices.at(local));
	}
	result.componentCoverageGain = best->branch.componentCoverageGain;
	result.selectedRawPartialFixedRank = best->branch.rawPartialFixedRank;
	result.selectedRecoveredNamedRank = best->branch.recoveredNamedRank;
	result.selectedParentBranchRank = best->parentBranchRank;
	result.selectedPartialFixFraction = best->branch.partialFixFraction;
	result.productInformationGain = best->branch.productInformationGain;
	result.selectedCanonicalHnf = best->branch.canonicalHnf;
	result.certifiedJointIntegerRank = best->branch.reliabilityPassed
		? 2 * best->branch.integerRank : 0;
	const int comparableRank = 2 * best->branch.integerRank;
	result.realSubspaceUpperBoundAtSelectedRank = relationGainSpectrum.rho(
		comparableRank);
	result.realIntegerGainGap =
		result.realSubspaceUpperBoundAtSelectedRank -
		result.productInformationGain;
	if (result.realSubspaceUpperBoundAtSelectedRank > 0)
	{
		result.relaxedRealUpperBoundCapture = result.productInformationGain /
			result.realSubspaceUpperBoundAtSelectedRank;
	}
	result.realSubspaceRank80 = relationGainSpectrum.minimumRankForRho(0.80);
	result.realSubspaceRank90 = relationGainSpectrum.minimumRankForRho(0.90);
	result.realSubspaceRank95 = relationGainSpectrum.minimumRankForRho(0.95);
	result.relationRho5 = relationGainSpectrum.rho(5);
	result.relationRho10 = relationGainSpectrum.rho(10);
	result.relationRho20 = relationGainSpectrum.rho(20);
	result.relationRho40 = relationGainSpectrum.rho(40);
	result.relationRho80 = relationGainSpectrum.rho(80);
	result.gainSpectrumDiagnosis = zhangProductGainSpectrumDiagnosis(
		result.realSubspaceUpperBoundAtSelectedRank,
		result.productInformationGain);
	result.maximumWideLanePerr = best->maximumWideLanePerr;
	result.maximumWideLaneMarginalRoundPerr =
		best->maximumWideLaneMarginalRoundPerr;
	result.wideLaneParentFailureProbabilityBound =
		best->wideLaneParentFailureProbabilityBound;
	result.wideLaneCertificateSource = best->inheritedWideLaneCertificate
		? "INHERITED_ACCEPTED_PARENT_LATTICE"
		: (best->branch.reliabilityPassed
			? "DIRECT_COMPLETE_NAMED_BRANCH" : "NONE");
	result.maximumFirstSignalPerr = best->maximumFirstPerr;
	result.jointNis = best->branch.jointNis;
	result.jointNisThreshold = best->branch.jointNisThreshold;
	result.wideLaneReliable = best->wideLaneReliable;
	result.firstSignalReliable = best->firstReliable;
	result.namedSubsetCertificate = best->branch.reliabilityPassed;
	result.wideLaneFixedRank = best->wideLane.size();
	result.firstSignalFixedRank = best->first.size();
	result.namedWideLane = best->wideLane;
	result.namedFirstSignal = best->first;
	result.namedSecondSignal = best->second;
	result.namedFirstSignalFixed = best->first.size();
	result.namedSecondSignalFixed = best->second.size();
	}
	else
	{
		result.realSubspaceRank80 = relationGainSpectrum.minimumRankForRho(0.80);
		result.realSubspaceRank90 = relationGainSpectrum.minimumRankForRho(0.90);
		result.realSubspaceRank95 = relationGainSpectrum.minimumRankForRho(0.95);
		result.relationRho5 = relationGainSpectrum.rho(5);
		result.relationRho10 = relationGainSpectrum.rho(10);
		result.relationRho20 = relationGainSpectrum.rho(20);
		result.relationRho40 = relationGainSpectrum.rho(40);
		result.relationRho80 = relationGainSpectrum.rho(80);
		result.status = "NO_LEGACY_NAMED_CONTROL_EVALUATED";
		result.failureReason = "PRODUCT_PAR_CONTROL_DISABLED";
	}
	if (bestProductConstraints)
	{
		const auto& source = *bestProductConstraints;
		auto embedRows = [&](const ZhangExactMatrix& localRows)
		{
			ZhangExactMatrix embedded;
			for (const auto& localRow : localRows)
			{
				if (localRow.size() != source.branch.namedRelationIndices.size())
					return ZhangExactMatrix{};
				ZhangExactVector fullRow(fullRank);
				for (int local = 0; local < static_cast<int>(localRow.size()); local++)
					fullRow[source.branch.namedRelationIndices[local]] = localRow[local];
				embedded.push_back(std::move(fullRow));
			}
			return embedded;
		};
		const auto embeddedWideLane = embedRows(source.wideLaneFixedRows);
		const auto embeddedFirst = embedRows(source.firstSignalFixedRows);
		result.constraints = zhangBuildProductConstraintSet(
			firstBasis, secondBasis,
			embeddedWideLane, source.wideLaneFixedValues,
			embeddedFirst, source.firstSignalFixedValues,
			source.constraintJointNis,
			source.constraintJointNisThreshold,
			1 - options.sucthr,
			source.branch.productInformationGain);
	}

	// Direct product-lattice solve.  Use the complete reference-invariant
	// all-pair product covariance as the gain target.  WL and conditional L1
	// may return arbitrary mixed primitive rows; pair edges are recovered only
	// afterwards by exact lattice membership.
	const MatrixXd allPairs = zhangAllPairIncidence(fullRank);
	MatrixXd pairProducts = MatrixXd::Zero(
		2 * allPairs.rows(), 2 * fullRank);
	pairProducts.topLeftCorner(allPairs.rows(), fullRank) = allPairs;
	pairProducts.bottomRightCorner(allPairs.rows(), fullRank) = allPairs;
	const MatrixXd pairProductCross = pairProducts * fullJointCovariance;
	const bool oracleEpochRequested =
		!acsConfig.zhangPppAr.full_product_lattice_oracle_filename.empty() &&
		(acsConfig.zhangPppAr.full_product_lattice_oracle_epochs.empty() ||
		 std::find(
			acsConfig.zhangPppAr.full_product_lattice_oracle_epochs.begin(),
			acsConfig.zhangPppAr.full_product_lattice_oracle_epochs.end(),
			time.to_string(0)) !=
			acsConfig.zhangPppAr.full_product_lattice_oracle_epochs.end());
	if (oracleEpochRequested)
	{
		const auto oracle = zhangCachedFullProductLatticeOracle(
			acsConfig.zhangPppAr.full_product_lattice_oracle_filename);
		result.constraints = zhangBuildFullProductLatticeOracle(
			trace, oracle, firstBasis, secondBasis,
			fullJointMean, fullJointCovariance, time);
		if (!result.constraints.reliable)
		{
			result.status = "FULL_PRODUCT_LATTICE_ORACLE_REJECTED";
			result.failureReason = result.constraints.failureReason;
			return result;
		}
		result.wideLaneFixedRank =
			result.constraints.wideLaneProductRows.size();
		result.firstSignalFixedRank =
			result.constraints.firstSignalProductRows.size();
		result.certifiedJointIntegerRank =
			result.constraints.conditioningRank;
		result.productInformationGain =
			result.constraints.referenceInvariantProductGain;
		result.maximumWideLanePerr = 0;
		result.maximumFirstSignalPerr = 0;
		result.jointNis = result.constraints.jointNis;
		result.jointNisThreshold = result.constraints.jointNisThreshold;
		result.wideLaneReliable = true;
		result.firstSignalReliable = true;
		result.namedSubsetCertificate = true;
		result.wideLaneCertificateSource = "FULL_PRODUCT_LATTICE_ORACLE";
		result.status = "FULL_PRODUCT_LATTICE_ORACLE";
		result.failureReason = "NONE";
		result.certifiedForProduct = true;
		return result;
	}
	const MatrixXd wideLaneProductCross =
		pairProductCross * fullWideLaneMap.transpose();
	const auto productWideLane = zhangSolveProductLatticeStage(
		trace, fullWideLaneMean, fullWideLaneCovariance,
		wideLaneProductCross, options, time, "WL", 1e-3);
	ZhangProductIntegerConstraintSet directConstraints;
	double directFirstSignalFailureProbability = 1;
	if (productWideLane.reliable && !productWideLane.rows.empty())
	{
		ZhangIarFunctional wlRows(
			productWideLane.rows.size(), 2 * fullRank);
		VectorXd wlIntegers(productWideLane.integers.size());
		for (int row = 0; row < static_cast<int>(productWideLane.rows.size()); row++)
		{
			for (int column = 0; column < fullRank; column++)
			{
				const double coefficient =
					productWideLane.rows[row][column].convert_to<double>();
				if (coefficient == 0) continue;
				wlRows.insert(row, column) = coefficient;
				wlRows.insert(row, fullRank + column) = -coefficient;
			}
			wlIntegers(row) = productWideLane.integers[row].convert_to<double>();
		}
		wlRows.makeCompressed();
		const auto conditionedOnWideLane = zhangConditionIntegersExact(
			fullJointMean, fullJointCovariance, wlRows, wlIntegers);
		struct ConditionalL1Alternative
		{
			ZhangExactMatrix rows;
			ZhangExactVector integers;
			double failureProbability = 0;
			std::string source = "WL_ONLY";
		};
		std::vector<ConditionalL1Alternative> l1Alternatives(1);
		if (conditionedOnWideLane.valid)
		{
			MatrixXd firstSelector = MatrixXd::Zero(fullRank, 2 * fullRank);
			firstSelector.leftCols(fullRank) =
				MatrixXd::Identity(fullRank, fullRank);
			const MatrixXd conditionalFirstCross = pairProducts *
				conditionedOnWideLane.covariance * firstSelector.transpose();
			const double remainingFailureProbability = std::max(
				1e-12, 1e-3 - productWideLane.failureProbabilityBound);
			const auto unrestrictedFirstSignal = zhangSolveProductLatticeStage(
				trace,
				conditionedOnWideLane.mean.head(fullRank),
				conditionedOnWideLane.covariance.topLeftCorner(fullRank, fullRank),
				conditionalFirstCross,
				options, time, "L1_GIVEN_WL",
				remainingFailureProbability);
			if (unrestrictedFirstSignal.reliable &&
				!unrestrictedFirstSignal.rows.empty())
			{
				l1Alternatives.push_back({
					unrestrictedFirstSignal.rows,
					unrestrictedFirstSignal.integers,
					unrestrictedFirstSignal.failureProbabilityBound,
					"UNRESTRICTED_PRODUCT_LATTICE"});
			}

			// A second, certificate-directed path is built only from exact pair
			// rows already contained in the accepted WL lattice.  Its ambient
			// coordinate is a primitive spanning forest, so any recovered named
			// coordinate has the same satellite identity as an existing WL edge.
			const auto wideLanePairs = zhangRecoverCertifiedPairRelations(
				productWideLane.rows, productWideLane.integers, fullRank, true);
			ZhangExactMatrix pairForestRows;
			for (auto pair : wideLanePairs)
			{
				if (pair.secondNode < pair.firstNode)
				{
					std::swap(pair.firstNode, pair.secondNode);
					pair.value = -pair.value;
				}
				ZhangExactVector row(fullRank);
				if (pair.firstNode < fullRank) row[pair.firstNode] += 1;
				if (pair.secondNode < fullRank) row[pair.secondNode] -= 1;
				auto trial = pairForestRows;
				trial.push_back(row);
				int trialRank = 0;
				if (!zhangExactPrimitiveRowLattice(
					trial, fullRank, &trialRank) ||
					trialRank != static_cast<int>(trial.size())) continue;
				pairForestRows = std::move(trial);
			}
			if (!pairForestRows.empty())
			{
				MatrixXd pairForest = MatrixXd::Zero(
					pairForestRows.size(), fullRank);
				for (int row = 0; row < pairForest.rows(); row++)
					pairForest.row(row) =
						zhangExactRowToDouble(pairForestRows[row]).transpose();
				const auto guidedFirstSignal = zhangSolveProductLatticeStage(
					trace,
					pairForest * conditionedOnWideLane.mean.head(fullRank),
					pairForest * conditionedOnWideLane.covariance.topLeftCorner(
						fullRank, fullRank) * pairForest.transpose(),
					conditionalFirstCross * pairForest.transpose(),
					options, time, "L1_GIVEN_WL_PAIR_SUBLATTICE",
					remainingFailureProbability);
				if (guidedFirstSignal.reliable &&
					!guidedFirstSignal.rows.empty())
				{
					ConditionalL1Alternative guided;
					guided.integers = guidedFirstSignal.integers;
					guided.failureProbability =
						guidedFirstSignal.failureProbabilityBound;
					guided.source = "WL_CERTIFIED_PAIR_SUBLATTICE";
					for (const auto& row : guidedFirstSignal.rows)
						guided.rows.push_back(
							zhangExactRowCombination(row, pairForestRows));
					l1Alternatives.push_back(std::move(guided));
				}
				trace << "\nZHANG_PRODUCT_LATTICE_PAIR_GUIDED_L1 time="
					  << time.to_string(0)
					  << " wl_pair_forest_rank=" << pairForestRows.size()
					  << " guided_fixed_rank=" << guidedFirstSignal.rows.size()
					  << " reliable=" << guidedFirstSignal.reliable
					  << " status=" << guidedFirstSignal.failureReason;
			}
		}

		const int wlRank = productWideLane.rows.size();
		double selectedJointNis = std::numeric_limits<double>::quiet_NaN();
		double selectedJointNisThreshold =
			std::numeric_limits<double>::quiet_NaN();
		double selectedFailureProbability = 1;
		double selectedProductGain = 0;
		std::string selectedL1Source = "NONE";
		auto directBetter = [](const auto& left, const auto& right)
		{
			if (left.reliable != right.reliable) return left.reliable;
			if (left.certifiedPairRank != right.certifiedPairRank)
				return left.certifiedPairRank > right.certifiedPairRank;
			if (left.referenceInvariantProductGain !=
				right.referenceInvariantProductGain)
				return left.referenceInvariantProductGain >
					right.referenceInvariantProductGain;
			return left.conditioningRank > right.conditioningRank;
		};
		for (const auto& alternative : l1Alternatives)
		{
			const int l1Rank = alternative.rows.size();
			MatrixXd jointRows = MatrixXd::Zero(
				wlRank + l1Rank, 2 * fullRank);
			VectorXd jointIntegers = VectorXd::Zero(wlRank + l1Rank);
			for (int row = 0; row < wlRank; row++)
			{
				const VectorXd numeric =
					zhangExactRowToDouble(productWideLane.rows[row]);
				jointRows.block(row, 0, 1, fullRank) = numeric.transpose();
				jointRows.block(row, fullRank, 1, fullRank) = -numeric.transpose();
				jointIntegers(row) =
					productWideLane.integers[row].convert_to<double>();
			}
			for (int row = 0; row < l1Rank; row++)
			{
				jointRows.block(wlRank + row, 0, 1, fullRank) =
					zhangExactRowToDouble(alternative.rows[row]).transpose();
				jointIntegers(wlRank + row) =
					alternative.integers[row].convert_to<double>();
			}
			const auto jointNis = assessZhangIntegerCandidateNis(
				jointIntegers - jointRows * fullJointMean,
				jointRows * fullJointCovariance * jointRows.transpose(),
				options.lambda_candidate_nis_alpha > 0
					? options.lambda_candidate_nis_alpha : 1e-6);
			const double failureProbability = std::min(1.0,
				productWideLane.failureProbabilityBound +
				alternative.failureProbability);
			const double productGain = zhangIntegerConstraintProductGain(
				jointRows, fullJointCovariance, pairProductCross);
			auto candidate = zhangBuildProductConstraintSet(
				firstBasis, secondBasis,
				productWideLane.rows, productWideLane.integers,
				alternative.rows, alternative.integers,
				jointNis.nis, jointNis.threshold,
				failureProbability, productGain);
			trace << "\nZHANG_PRODUCT_LATTICE_L1_ALTERNATIVE time="
				  << time.to_string(0)
				  << " source=" << alternative.source
				  << " l1_rank=" << l1Rank
				  << " certified_pair_rank=" << candidate.certifiedPairRank
				  << " product_gain=" << productGain
				  << " reliable=" << candidate.reliable
				  << " status=" << candidate.failureReason;
			if (directConstraints.networkRows.empty() ||
				directBetter(candidate, directConstraints))
			{
				directConstraints = std::move(candidate);
				directFirstSignalFailureProbability =
					l1Rank > 0 ? alternative.failureProbability : 1;
				selectedJointNis = jointNis.nis;
				selectedJointNisThreshold = jointNis.threshold;
				selectedFailureProbability = failureProbability;
				selectedProductGain = productGain;
				selectedL1Source = alternative.source;
			}
		}
		const int l1Rank = directConstraints.firstSignalProductRows.size();
		trace << "\nZHANG_PRODUCT_LATTICE_BLOCK time=" << time.to_string(0)
			  << " wl_rank=" << wlRank
			  << " conditional_l1_rank=" << l1Rank
			  << " conditioning_rank=" << directConstraints.conditioningRank
			  << " certified_pair_rank=" << directConstraints.certifiedPairRank
			  << " l1_source=" << selectedL1Source
			  << " joint_nis=" << selectedJointNis
			  << " joint_nis_threshold=" << selectedJointNisThreshold
			  << " failure_probability_bound=" << selectedFailureProbability
			  << " reference_invariant_product_gain=" << selectedProductGain
			  << " reliable=" << directConstraints.reliable
			  << " status=" << directConstraints.failureReason;
	}

	auto constraintBetter = [](const auto& left, const auto& right)
	{
		if (left.reliable != right.reliable) return left.reliable;
		if (left.certifiedPairRank != right.certifiedPairRank)
			return left.certifiedPairRank > right.certifiedPairRank;
		if (left.referenceInvariantProductGain !=
			right.referenceInvariantProductGain)
			return left.referenceInvariantProductGain >
				right.referenceInvariantProductGain;
		return left.conditioningRank > right.conditioningRank;
	};
	const bool directSelected = directConstraints.reliable &&
		(!result.constraints.reliable ||
			constraintBetter(directConstraints, result.constraints));
	if (directSelected)
	{
		result.constraints = std::move(directConstraints);
		result.wideLaneFixedRank = result.constraints.wideLaneProductRows.size();
		result.firstSignalFixedRank =
			result.constraints.firstSignalProductRows.size();
		result.productInformationGain =
			result.constraints.referenceInvariantProductGain;
		result.maximumWideLanePerr =
			productWideLane.failureProbabilityBound;
		result.maximumFirstSignalPerr =
			result.firstSignalFixedRank > 0
				? directFirstSignalFailureProbability : 1;
		result.jointNis = result.constraints.jointNis;
		result.jointNisThreshold = result.constraints.jointNisThreshold;
		result.wideLaneReliable = true;
		result.firstSignalReliable = result.firstSignalFixedRank > 0;
		result.namedSubsetCertificate = false;
		result.wideLaneCertificateSource = "PRODUCT_LATTICE_ILS";
		result.status = "RELIABLE_PRODUCT_LATTICE_SOLUTION";
		result.failureReason = "NONE";
	}
	else
	{
		result.status = best && best->branch.reliabilityPassed
			? "RELIABLE_NAMED_SUBSET_SOLUTION"
			: "NO_RELIABLE_PRODUCT_LATTICE_SOLUTION";
		result.failureReason = best
			? best->failureReason
			: (!directConstraints.failureReason.empty()
				? directConstraints.failureReason
				: "NO_RELIABLE_DIRECT_PRODUCT_LATTICE_SOLUTION");
	}
	// Solver certification is strictly the exact dual-frequency product graph.
	// The writer applies the independent reference-invariant precision/effect
	// gate before any component can become PPP-AR usable.
	result.certifiedForProduct = result.constraints.reliable &&
		result.constraints.certifiedPairRank > 0;
	return result;
}

/** Build the private positive-feedback search posterior from previously
 * certified product integers.
 *
 * FLOAT and the network-WL KF branch remain untouched.  Every retained row is
 * reprojected from its exact physical arc/version identity, checked against
 * the row-local current phase segments, canonicalised jointly with its integer
 * value, and admitted by a fresh joint-NIS subset test.  Only the ambiguity
 * mean/covariance handed to the product solver is conditioned.  The solver's
 * newly found constraints are subsequently combined with the same ledger and
 * rechecked on the original WL branch before PRODUCT_FIXED is created. */
static GinAR_mtx zhangProductLedgerPreconditionedSearch(
	Trace& trace,
	GTime time,
	const std::string& authoritativeRuntimeId,
	const KFState& identityState,
	E_Sys system,
	const GinAR_mtx& source,
	bool& applied)
{
	applied = false;
	GinAR_mtx result = source;
	int certifiedRows = 0;
	int projectedRows = 0;
	int segmentRejectedRows = 0;
	int unavailableRows = 0;
	int crossGenerationRows = 0;
	int exactRank = 0;
	int selectedRank = 0;
	double maximumResidual = std::numeric_limits<double>::quiet_NaN();
	std::string status = "NO_CERTIFIED_LEDGER_ROWS";
	auto traceResult = [&]()
	{
		trace << "\nZHANG_PRODUCT_INTEGER_LEDGER_PRESEARCH time="
			  << time.to_string(0)
			  << " system=" << enum_to_string(system)
			  << " certified_rows=" << certifiedRows
			  << " projected_rows=" << projectedRows
			  << " segment_rejected_rows=" << segmentRejectedRows
			  << " unavailable_rows=" << unavailableRows
			  << " cross_generation_rows=" << crossGenerationRows
			  << " exact_rank=" << exactRank
			  << " selected_rank=" << selectedRank
			  << " maximum_constraint_residual=" << maximumResidual
			  << " applied=" << applied
			  << " authoritative_float_unchanged=1"
			  << " status=" << status
			  << " feedback=PRIVATE_PRODUCT_SEARCH_ONLY";
	};
	if (authoritativeRuntimeId.empty() || system == E_Sys::NONE ||
		source.aflt.size() <= 0 || source.Paflt.rows() != source.aflt.size() ||
		source.Paflt.cols() != source.aflt.size() ||
		source.ambmap.size() != static_cast<std::size_t>(source.aflt.size()))
	{
		status = "PRESEARCH_INPUT_INVALID";
		traceResult();
		return result;
	}
	auto registry = zhangProductIntegerLedgerRegistry().find(
		{authoritativeRuntimeId, system});
	if (registry == zhangProductIntegerLedgerRegistry().end())
	{
		traceResult();
		return result;
	}
	std::map<int, std::string> columnIdentities;
	std::uint64_t currentGeneration = 0;
	if (!zhangCurrentProductPhysicalAmbiguityIdentities(
		identityState, source.ambmap, system,
		columnIdentities, currentGeneration))
	{
		status = "CURRENT_PRODUCT_IDENTITIES_UNAVAILABLE";
		traceResult();
		return result;
	}
	std::map<std::string, int> identityColumns;
	for (const auto& [column, identity] : columnIdentities)
		identityColumns[identity] = column;
	ZhangExactMatrix exactRows;
	ZhangExactVector exactValues;
	for (const auto& held : registry->second.rows())
	{
		if (!held.certified || held.system != system) continue;
		certifiedRows++;
		const std::string currentSegments =
			zhangProductPhysicalRowSegmentFingerprint(
				held.system, held.physicalExpansion);
		if (currentSegments.empty() ||
			currentSegments != held.phaseSegmentFingerprint)
		{
			segmentRejectedRows++;
			continue;
		}
		ZhangExactVector projected;
		if (!zhangProjectProductLedgerPhysicalRow(
			held, identityColumns, source.aflt.size(), projected))
		{
			unavailableRows++;
			continue;
		}
		crossGenerationRows +=
			held.backendBasisGeneration != currentGeneration;
		exactRows.push_back(std::move(projected));
		exactValues.push_back(held.integerValue);
		projectedRows++;
	}
	if (exactRows.empty())
	{
		status = certifiedRows > 0
			? "NO_CURRENT_SEGMENT_PROJECTABLE_ROWS"
			: "NO_CERTIFIED_LEDGER_ROWS";
		traceResult();
		return result;
	}
	const auto hnf = zhangExactRowHermiteNormalForm(exactRows, exactValues);
	if (!hnf.consistent || hnf.basis.empty() ||
		hnf.basis.size() != hnf.values.size())
	{
		status = "LEDGER_EXACT_ROWS_INCONSISTENT";
		traceResult();
		return result;
	}
	exactRank = hnf.basis.size();
	GinAR_mtx candidate;
	candidate.ambmap = source.ambmap;
	candidate.aflt = source.aflt;
	candidate.Paflt = 0.5 * (source.Paflt + source.Paflt.transpose());
	candidate.Ztrs = MatrixXd::Zero(exactRank, source.aflt.size());
	candidate.zfix = VectorXd::Zero(exactRank);
	for (int row = 0; row < exactRank; row++)
	{
		candidate.Ztrs.row(row) =
			zhangExactRowToDouble(hnf.basis[row]).transpose();
		candidate.zfix(row) = hnf.values[row].convert_to<double>();
	}
	selectedRank = retainNisCompatibleNamedRows(
		trace, candidate, time,
		"PRODUCT_INTEGER_LEDGER_PRESEARCH", nullptr, true);
	if (selectedRank <= 0)
	{
		status = "NO_NIS_COMPATIBLE_LEDGER_ROWS";
		traceResult();
		return result;
	}
	ZhangIarFunctional constraints(selectedRank, source.aflt.size());
	for (int row = 0; row < selectedRank; row++)
	for (int column = 0; column < source.aflt.size(); column++)
	{
		const double coefficient = candidate.Ztrs(row, column);
		if (coefficient != 0) constraints.insert(row, column) = coefficient;
	}
	constraints.makeCompressed();
	const auto conditioned = zhangConditionIntegersExact(
		source.aflt, candidate.Paflt, constraints, candidate.zfix);
	if (!conditioned.valid)
	{
		status = conditioned.failureReason;
		traceResult();
		return result;
	}
	result.aflt = conditioned.mean;
	result.Paflt = conditioned.covariance;
	maximumResidual = conditioned.maximumConstraintResidual;
	applied = true;
	status = "NIS_SELECTED_LEDGER_CONDITIONED";
	traceResult();
	return result;
}

static int resolveLayeredWideLaneL1(
    Trace&       trace,
    KFState&     kfState,
    GinAR_mtx&   ambiguityResolution,
    const GinAR_opt& options,
    GTime        time,
    bool*        allSystemsPhaseFixed = nullptr,
    KFState*     wideLaneState = nullptr,
    bool*        wideLaneStateValid = nullptr,
    ZhangProductRelationFixResult* productRelationResult = nullptr,
	std::string productLedgerRuntimeId = {}
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
    int              configuredSystems = 0;
    int              phaseFixedSystems = 0;
    const bool enforceHouCandidateNis =
        acsConfig.zhangPppAr.product_mode == "HOU_OSB_LIKE";
    if (wideLaneStateValid)
    {
        *wideLaneStateValid = false;
    }
	if (productRelationResult)
	{
		*productRelationResult = ZhangProductRelationFixResult{};
	}
    int configuredDualFrequencySystems = 0;
    for (const auto& [system, systemOptions] : acsConfig.zhangFullRank.sysOpts)
    {
        configuredDualFrequencySystems +=
            systemOptions.baseline_observables.size() == 2;
    }

    auto appendAndApply = [&](const MatrixXd& rows,
                              const VectorXd& values,
                              const string& stageName)
    {
        if (rows.rows() == 0)
        {
            return true;
        }
        GinAR_mtx stage;
        stage.ambmap = ambiguityResolution.ambmap;
        stage.Ztrs   = rows;
        stage.zfix   = values;
        applyUCAmbiguities(trace, kfState, stage, stageName);
        if (zhangTransactionalConditioningFailed)
        {
            string rejectionReason = zhangTransactionalConditioningReason;
            trace << "\nZHANG_FIXED_SUBTRANSACTION time="
                  << time.to_string(0)
                  << " stage=" << stageName
                  << " status=ROLLED_BACK"
                  << " reason=" << rejectionReason
                  << " retained_rows=" << fixedRows.size();
            // Exact conditioning is atomic and does not mutate x/P before all
            // gates pass.  Clear only the local stage failure so the already
            // committed HELD/WL branch remains usable.
            if (acsConfig.zhangPppAr.transactional_integer_fixing)
            {
                zhangTransactionalConditioningFailed = false;
                zhangTransactionalConditioningReason.clear();
            }
            return false;
        }
        for (int row = 0; row < rows.rows(); row++)
        {
            fixedRows.push_back(rows.row(row).transpose());
            fixedValues.push_back(values(row));
        }
        totalFixed += rows.rows();
        trace << "\nZHANG_FIXED_SUBTRANSACTION time="
              << time.to_string(0)
              << " stage=" << stageName
              << " status=COMMITTED"
              << " committed_rows=" << rows.rows()
              << " total_committed_rows=" << fixedRows.size();
        return true;
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
        if (commonPairs.empty())
        {
            continue;
        }
        configuredSystems++;
        const bool e24bTheoryRegressionRequested =
            acsConfig.zhangPppAr.canonical_theory_regression_shadow &&
            (acsConfig.zhangPppAr
                 .canonical_theory_regression_target_epoch.empty() ||
             acsConfig.zhangPppAr
                 .canonical_theory_regression_target_epoch ==
                    time.to_string(0));
        if (e24bTheoryRegressionRequested)
        {
            ZhangGraphIntegerContext regressionGraphContext;
            if (zhangGraphIntegerContext(
                    kfState, system, regressionGraphContext))
            {
                traceZhangCanonicalTheoryRegression(
                    trace,
                    kfState,
                    ambiguityResolution,
                    regressionGraphContext,
                    system,
                    firstCode,
                    secondCode,
                    time);
            }
            else
            {
                trace << "\nZHANG_E24B_THEORY_SUMMARY time="
                      << time.to_string(0)
                      << " system=" << enum_to_string(system)
                      << " status=SKIPPED_NO_GRAPH_CONTEXT"
                      << " ar_authorized=0 feedback=SHADOW_NONE";
            }
        }
        if (acsConfig.zhangPppAr.canonical_theory_regression_shadow)
        {
            trace << "\nZHANG_E24B_FLOAT_CONTROL time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " action=SKIP_WL_L1_SEARCH_AND_FEEDBACK"
                  << " covariance_semantics=PURE_FLOAT_HISTORY"
                  << " ar_authorized=0 feedback=SHADOW_NONE";
            continue;
        }
        const bool e24aGainAuditRequested =
            acsConfig.zhangPppAr.l1_iar_gain_audit_shadow &&
            (acsConfig.zhangPppAr.l1_iar_gain_audit_target_epoch.empty() ||
             acsConfig.zhangPppAr.l1_iar_gain_audit_target_epoch ==
                time.to_string(0));
        MatrixXd e24aPF0;
        MatrixXd e24aPWideLane;
        if (e24aGainAuditRequested)
        {
            e24aPF0 = kfState.P;
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
        int wideLaneFixed = rankAwareGnssAr(
            trace,
            wideLane,
            options,
            time,
            "LAYERED_WIDE_LANE",
            enforceHouCandidateNis
        );
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
        bool wideLaneCommitted = appendAndApply(
            fullWideLaneRows, wideLane.zfix, "LAYERED_WIDE_LANE"
        );
        if (!wideLaneCommitted)
        {
            continue;
        }
        conditionZhangL1MeasurementReplayPosteriors(
            trace,
            kfState,
            ambiguityResolution.ambmap,
            fullWideLaneRows,
            wideLane.zfix,
            "LAYERED_WIDE_LANE"
        );
        refreshFloatState();
        if (wideLaneState && wideLaneStateValid &&
            configuredDualFrequencySystems == 1)
        {
            *wideLaneState = kfState;
            bindZhangAmbresEphemeralBranch(
                *wideLaneState, kfState, "wide-lane-snapshot");
            *wideLaneStateValid = true;
            trace << "\nZHANG_WL_PRODUCT_SNAPSHOT time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " status=CAPTURED"
                  << " feedback=DISPOSABLE_SAME_EPOCH_BRANCH";
        }
        if (e24aGainAuditRequested)
        {
            e24aPWideLane = kfState.P;
        }

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
        for (int local = 0;
             local < static_cast<int>(firstColumns.size());
             local++)
        {
            auto keyIt = ambiguityResolution.ambmap.find(firstColumns[local]);
            if (keyIt != ambiguityResolution.ambmap.end())
            {
                firstSignal.ambmap[local] = keyIt->second;
            }
        }
        const GinAR_mtx firstSignalFloat = firstSignal;
        const ZhangL1BeamProductProjection beamProductProjection =
            zhangL1BeamProductProjection(
                kfState,
                system,
                firstSignal.ambmap
            );
		if (acsConfig.zhangPppAr.product_relation_l1_par_shadow ||
			acsConfig.zhangPppAr.integer_strategy == "HYBRID_PRODUCT_WL_L1")
		{
			GinAR_mtx currentCertifiedRows;
			currentCertifiedRows.ambmap = ambiguityResolution.ambmap;
			currentCertifiedRows.Ztrs = fullWideLaneRows;
			currentCertifiedRows.zfix = wideLane.zfix;
			ZhangPersistentHeldLattice currentCertifiedPhysical;
			string currentCertifiedFailure;
			const bool currentCertifiedValid =
				zhangCurrentCertifiedPhysicalLattice(
					kfState, currentCertifiedRows, system,
					currentCertifiedPhysical, currentCertifiedFailure);
			trace << "\nZHANG_CURRENT_EPOCH_CERTIFIED_LATTICE time="
				  << time.to_string(0)
				  << " system=" << enum_to_string(system)
				  << " input_rank=" << fullWideLaneRows.rows()
				  << " physical_rank=" << currentCertifiedPhysical.rows.size()
				  << " valid=" << currentCertifiedValid
				  << " status=" << (currentCertifiedValid
					? "EXACT_CURRENT_EPOCH_CERTIFICATE"
					: currentCertifiedFailure)
				  << " persistent_admission=0 feedback=0";
			bool productLedgerPresearchApplied = false;
			GinAR_mtx productSearchAmbiguities = ambiguityResolution;
			if (!productLedgerRuntimeId.empty())
			{
				productSearchAmbiguities =
					zhangProductLedgerPreconditionedSearch(
						trace, time, productLedgerRuntimeId, kfState, system,
						ambiguityResolution, productLedgerPresearchApplied);
			}
			ZhangProductRelationBasis relationBasis =
                compileZhangProductRelationBasis(
                    kfState, productSearchAmbiguities, system, firstCode);
            ZhangProductRelationBasis secondRelationBasis =
                compileZhangProductRelationBasis(
                    kfState, productSearchAmbiguities, system, secondCode);
            const int relationRank = relationBasis.mappableTargetRank;
            const MatrixXd& relationFirst = relationBasis.transform;
            const MatrixXd& relationSecond = secondRelationBasis.transform;
            const bool relationMappingValid = relationBasis.valid &&
                secondRelationBasis.valid && relationRank > 0 &&
                relationRank == secondRelationBasis.mappableTargetRank &&
                zhangProductNamedOrderingMatches(
                    relationBasis, secondRelationBasis) &&
                relationFirst.rows() == relationRank &&
                relationSecond.rows() == relationRank;

			const ZhangProductRelationFixResult relationFix =
				solveZhangProductRelations(
					trace,
					kfState,
					productSearchAmbiguities,
                    relationBasis,
					secondRelationBasis,
					options,
					time,
					currentCertifiedValid ? &currentCertifiedPhysical : nullptr,
					currentCertifiedValid ? &currentCertifiedRows : nullptr);
			if (productRelationResult &&
				(!productRelationResult->constraints.reliable ||
				 relationFix.constraints.conditioningRank >
					productRelationResult->constraints.conditioningRank))
			{
				*productRelationResult = relationFix;
			}
            int productWlFixed = relationFix.wideLaneFixedRank;
            int productL1Fixed = relationFix.firstSignalFixedRank;
            int namedL1Fixed = relationFix.namedFirstSignalFixed;
            string conditioningStatus = relationFix.status;
            // Frozen comparison only: the extracted solver above is the sole
            // live implementation and this legacy inline body is unreachable.
            if (false && relationMappingValid && relationRank > 0)
            {
                const MatrixXd relationWideLane =
                    relationFirst - relationSecond;
                GinAR_mtx productWideLane;
                productWideLane.aflt = relationWideLane *
                    ambiguityResolution.aflt;
                productWideLane.Paflt = relationWideLane *
                    ambiguityResolution.Paflt *
                    relationWideLane.transpose();
                productWlFixed = rankAwareGnssAr(
                    trace,
                    productWideLane,
                    options,
                    time,
                    "PRODUCT_RELATION_WIDE_LANE_SHADOW",
                    true);
                if (productWlFixed > 0)
                {
                    VectorXd jointMean(2 * relationRank);
                    jointMean.head(relationRank) =
                        relationFirst * ambiguityResolution.aflt;
                    jointMean.tail(relationRank) =
                        relationSecond * ambiguityResolution.aflt;
                    MatrixXd jointTransform(2 * relationRank,
                                            ambiguityResolution.aflt.size());
                    jointTransform.topRows(relationRank) = relationFirst;
                    jointTransform.bottomRows(relationRank) = relationSecond;
                    const MatrixXd jointCovariance = jointTransform *
                        ambiguityResolution.Paflt * jointTransform.transpose();
                    ZhangIarFunctional wlConstraints(
                        productWideLane.Ztrs.rows(), 2 * relationRank);
                    for (int row = 0; row < productWideLane.Ztrs.rows(); row++)
                    {
                        for (int column = 0; column < relationRank; column++)
                        {
                            const double coefficient =
                                productWideLane.Ztrs(row, column);
                            if (coefficient != 0)
                            {
                                wlConstraints.insert(row, column) = coefficient;
                                wlConstraints.insert(
                                    row, relationRank + column) = -coefficient;
                            }
                        }
                    }
                    wlConstraints.makeCompressed();
                    const ZhangIntegerConditionedState conditioned =
                        zhangConditionIntegersExact(
                            jointMean,
                            jointCovariance,
                            wlConstraints,
                            productWideLane.zfix);
                    conditioningStatus = conditioned.valid
                        ? "WL_CONDITIONED" : conditioned.failureReason;
                    if (conditioned.valid)
                    {
                        GinAR_mtx productFirst;
                        productFirst.aflt = conditioned.mean.head(relationRank);
                        productFirst.Paflt = conditioned.covariance.topLeftCorner(
                            relationRank, relationRank);
                        productL1Fixed = rankAwareGnssAr(
                            trace,
                            productFirst,
                            options,
                            time,
                            "PRODUCT_RELATION_FIRST_SIGNAL_SHADOW",
                            true);
                        if (productL1Fixed > 0 &&
                            productFirst.Ztrs.cols() == relationRank &&
                            productFirst.Ztrs.rows() == productFirst.zfix.size())
                        {
                            ZhangExactMatrix fixedRows;
                            ZhangExactVector fixedValues;
                            bool exact = true;
                            for (int row = 0;
                                 row < productFirst.Ztrs.rows() && exact; row++)
                            {
                                ZhangExactVector fixedRow(relationRank);
                                for (int column = 0;
                                     column < relationRank; column++)
                                {
                                    const long long value = std::llround(
                                        productFirst.Ztrs(row, column));
                                    exact &= std::abs(
                                        productFirst.Ztrs(row, column) - value
                                    ) <= 1e-8;
                                    fixedRow[column] = value;
                                }
                                const long long integer = std::llround(
                                    productFirst.zfix(row));
                                exact &= std::abs(
                                    productFirst.zfix(row) - integer) <= 1e-8;
                                fixedRows.push_back(std::move(fixedRow));
                                fixedValues.push_back(integer);
                            }
                            if (exact)
                            {
                                namedL1Fixed =
                                    ProductConstraintPromotion::recoverNamedTargets(
                                        fixedRows,
                                        fixedValues,
                                        relationRank).size();
                            }
                        }
                    }
                }
                else
                {
                    conditioningStatus = "WL_NOT_FIXED";
                }
            }
            trace << "\nZHANG_PRODUCT_RELATION_IAR_SHADOW time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " observable=" << enum_to_string(firstCode)
                  << " named_relations="
                  << relationBasis.namedRelationCount
                  << " full_target_rank=" << relationBasis.fullTargetRank
                  << " exact_rank=" << relationBasis.exactRank
                  << " mappable_target_rank="
                  << relationBasis.mappableTargetRank
                  << " unmappable_target_rank="
                  << relationBasis.unmappableTargetRank
                  << " independent_named_rank=" << relationRank
                  << " exact_hnf=" << relationBasis.exactHnf
                  << " physical_arc_dimension="
                  << relationBasis.physicalArcColumns.size()
                  << " primitive=" << relationBasis.primitive
                  << " saturation_index="
                  << relationBasis.saturationIndex
                  << " admissible_completion_proven="
                  << relationBasis.admissibleCompletionProven
                  << " network_integer_rank="
                  << relationBasis.networkIntegerBasis.size()
                  << " network_lattice_contained="
                  << relationBasis.networkLatticeContained
                  << " network_closure_exact_zero="
                  << relationBasis.networkClosureExactZero
                  << " network_containment_u_hnf="
                  << zhangExactMatrixFingerprint(
                        relationBasis.networkContainmentTransform)
                  << " temporal_recovery_required="
                  << relationBasis.temporalRecoveryRequired
                  << " nuisance_orthogonal="
                  << relationBasis.nuisanceOrthogonal
                  << " physical_expansion_valid="
                  << relationBasis.physicalExpansionValid
                  << " physical_proof=CURRENT_CYCLES_TO_RAW_AMBIGUITY_ARCS"
                  << " nuisance_block=RECEIVER_PLUS_SATELLITE_ADDITIVE"
                  << " relation_mapping_valid=" << relationMappingValid
                  << " wl_fixed=" << productWlFixed
                  << " l1_fixed=" << productL1Fixed
                  << " named_l1_fixed=" << namedL1Fixed
                  << " named_l2_fixed="
                  << relationFix.namedSecondSignalFixed
                  << " named_ordering_valid="
                  << relationFix.namedOrderingValid
                  << " named_subset_certificate="
                  << relationFix.namedSubsetCertificate
				  << " wl_search_strategy=NAMED_BACKWARD_FULL_LAMBDA"
                  << " evaluated_branches="
                  << relationFix.evaluatedBranches
				  << " named_round_wl_candidates="
				  << relationFix.namedRoundWideLaneCandidates
				  << " named_round_wl_retained="
				  << relationFix.namedRoundWideLaneRetained
				  << " selected_raw_partial_fixed_rank="
				  << relationFix.selectedRawPartialFixedRank
				  << " selected_recovered_named_rank="
				  << relationFix.selectedRecoveredNamedRank
				  << " selected_parent_branch_rank="
				  << relationFix.selectedParentBranchRank
				  << " selected_partial_fix_fraction="
				  << relationFix.selectedPartialFixFraction
                  << " selected_named_rank="
                  << relationFix.selectedNamedRelationIndices.size()
                  << " comparable_real_mode_rank="
                  << 2 * relationFix.selectedNamedRelationIndices.size()
                  << " certified_joint_integer_rank="
                  << relationFix.certifiedJointIntegerRank
				  << " selected_canonical_hnf="
				  << relationFix.selectedCanonicalHnf
				  << " wl_certificate_source="
				  << relationFix.wideLaneCertificateSource
				  << " wl_parent_failure_probability_bound="
				  << relationFix.wideLaneParentFailureProbabilityBound
				  << " wl_maximum_marginal_round_perr_diagnostic="
				  << relationFix.maximumWideLaneMarginalRoundPerr
                  << " component_coverage_gain="
                  << relationFix.componentCoverageGain
                  << " product_information_gain="
                  << relationFix.productInformationGain
                  << " real_subspace_upper_bound_selected_rank="
                  << relationFix.realSubspaceUpperBoundAtSelectedRank
                  << " relation_rho5=" << relationFix.relationRho5
                  << " relation_rho10=" << relationFix.relationRho10
                  << " relation_rho20=" << relationFix.relationRho20
                  << " relation_rho40=" << relationFix.relationRho40
                  << " relation_rho80=" << relationFix.relationRho80
				  << " relaxed_real_upper_bound_capture="
				  << relationFix.relaxedRealUpperBoundCapture
                  << " real_integer_gain_gap="
                  << relationFix.realIntegerGainGap
                  << " real_subspace_rank80="
                  << relationFix.realSubspaceRank80
                  << " real_subspace_rank90="
                  << relationFix.realSubspaceRank90
                  << " real_subspace_rank95="
                  << relationFix.realSubspaceRank95
                  << " gain_spectrum_diagnosis="
                  << relationFix.gainSpectrumDiagnosis
                  << " gain_comparison_coordinate="
                  << "JOINT_NAMED_SATELLITE_PRODUCT"
                  << " wl_maximum_perr="
                  << relationFix.maximumWideLanePerr
                  << " l1_maximum_perr="
                  << relationFix.maximumFirstSignalPerr
                  << " joint_nis=" << relationFix.jointNis
                  << " joint_nis_threshold="
                  << relationFix.jointNisThreshold
                  << " wl_reliable=" << relationFix.wideLaneReliable
                  << " l1_reliable=" << relationFix.firstSignalReliable
                  << " reliability_success_threshold=" << options.sucthr
                  << " reliability_failure_ceiling="
                  << 1 - options.sucthr
				  << " conditioning_status=" << conditioningStatus
				  << " ledger_presearch_applied="
				  << productLedgerPresearchApplied
                  << " status="
                  << relationFix.status
                  << " failure_reason="
                  << relationFix.failureReason
                  << " certified_for_product="
                  << relationFix.certifiedForProduct
                  << " ar_authorized=0 feedback=SHADOW_NONE";
        }
        const string productGainSpectrumEpoch = time.to_string(0);
        const bool productGainSpectrumRequested =
            acsConfig.zhangPppAr.l1_product_gain_spectrum_shadow &&
            std::find(
                acsConfig.zhangPppAr.l1_product_gain_spectrum_epochs.begin(),
                acsConfig.zhangPppAr.l1_product_gain_spectrum_epochs.end(),
                productGainSpectrumEpoch
            ) != acsConfig.zhangPppAr.l1_product_gain_spectrum_epochs.end();
        if (productGainSpectrumRequested)
        {
            vector<int> productRows;
            for (int row = 0;
                 row < static_cast<int>(
                    beamProductProjection.productObservables.size());
                 row++)
            {
                if (beamProductProjection.productObservables[row] == firstCode)
                {
                    productRows.push_back(row);
                }
            }
            ZhangIarProductGainSpectrum spectrum;
            if (beamProductProjection.userQuotientCrossCovariance.rows() !=
                    static_cast<int>(
                        beamProductProjection.productObservables.size()) ||
                productRows.empty())
            {
                spectrum.failureReason =
                    "L1_PRODUCT_PROJECTION_NOT_AVAILABLE";
            }
            else
            {
                const int productDimension =
                    static_cast<int>(productRows.size());
                MatrixXd ambiguityProductCross(
                    firstSignalFloat.Paflt.rows(), productDimension);
                for (int column = 0;
                     column < productDimension; column++)
                {
                    ambiguityProductCross.col(column) =
                        beamProductProjection.userQuotientCrossCovariance.row(
                            productRows[column]).transpose();
                }
                spectrum = zhangIarProductGainSpectrum(
                    firstSignalFloat.Paflt,
                    ambiguityProductCross,
                    MatrixXd::Identity(productDimension, productDimension)
                );
            }
            trace << "\nZHANG_L1_PRODUCT_GAIN_SPECTRUM time="
                  << productGainSpectrumEpoch
                  << " system=" << enum_to_string(system)
                  << " observable=" << enum_to_string(firstCode)
                  << " ambiguity_dimension=" << spectrum.ambiguityDimension
                  << " ambiguity_rank=" << spectrum.ambiguityRank
                  << " product_dimension=" << spectrum.productDimension
                  << " valid=" << spectrum.valid
                  << " total_weighted_gain=" << spectrum.totalWeightedGain
                  << " rho5=" << spectrum.rho(5)
                  << " rho10=" << spectrum.rho(10)
                  << " rho20=" << spectrum.rho(20)
                  << " rho40=" << spectrum.rho(40)
                  << " rho80=" << spectrum.rho(80)
                  << " rho120=" << spectrum.rho(120)
                  << " minimum_retained_ambiguity_eigenvalue="
                  << spectrum.minimumRetainedAmbiguityEigenvalue
                  << " maximum_ambiguity_eigenvalue="
                  << spectrum.maximumAmbiguityEigenvalue
                      << " failure_reason="
                      << (spectrum.failureReason.empty()
                            ? "NONE" : spectrum.failureReason)
                      << " product_weight=IDENTITY_L1_USER_QUOTIENT"
                      << " posterior=WL_CONDITIONED_BEFORE_L1"
                      << " product_coordinate=USER_QUOTIENT_CLOCK_MINUS_PHASE"
                      << " mode_semantics=ARBITRARY_REAL_DIRECTIONS_UPPER_BOUND"
                  << " ar_authorized=0 feedback=SHADOW_NONE";
            const int reportedModes = std::min(
                120,
                static_cast<int>(spectrum.eigenvaluesDescending.size()));
            for (int mode = 0; mode < reportedModes; mode++)
            {
                trace << "\nZHANG_L1_PRODUCT_GAIN_SPECTRUM_MODE time="
                      << productGainSpectrumEpoch
                      << " system=" << enum_to_string(system)
                      << " observable=" << enum_to_string(firstCode)
                      << " mode=" << mode + 1
                      << " eigenvalue="
                      << spectrum.eigenvaluesDescending(mode)
                      << " ar_authorized=0 feedback=SHADOW_NONE";
            }
        }
        set<string> ablationSatellites;
        set<string> ablationReceivers;
        for (string satellite :
             acsConfig.zhangPppAr.l1_candidate_shadow_satellites)
        {
            boost::to_upper(satellite);
            ablationSatellites.insert(satellite);
        }
        for (string receiver :
             acsConfig.zhangPppAr.l1_candidate_shadow_receivers)
        {
            boost::to_upper(receiver);
            ablationReceivers.insert(receiver);
        }
        auto makeFirstSignalOptions = [&](
            const string& rowAblation,
            bool          useSatelliteTargets,
            bool          useReceiverTargets
        )
        {
            GinAR_opt configured = options;
            configured.lambda_candidate_row_ablation = rowAblation;
            configured.lambda_candidate_ablation_seed =
                stableZhangAblationSeed(
                    acsConfig.zhangPppAr.l1_candidate_shadow_random_seed,
                    time.to_string(0),
                    system
                );
            for (const auto& [local, key] : firstSignal.ambmap)
            {
                string receiver = key.str;
                string satellite = key.Sat.id();
                boost::to_upper(receiver);
                boost::to_upper(satellite);
                if ((useSatelliteTargets &&
                     ablationSatellites.count(satellite) > 0) ||
                    (useReceiverTargets &&
                     ablationReceivers.count(receiver) > 0))
                {
                    configured.lambda_candidate_ablation_target_columns
                        .push_back(local);
                }
            }
            return configured;
        };

        int firstFixed = 0;
        const bool causalFiveGroup =
            acsConfig.zhangPppAr.l1_candidate_shadow_ablation ==
                "CAUSAL_FIVE_GROUP";
        if (causalFiveGroup)
        {
            struct CausalGroup
            {
                string name;
                string mode;
                bool   satellites;
                bool   receivers;
            };
            const vector<CausalGroup> groups = {
                {"A0_BASELINE",       "NONE",             false, false},
                {"A1_G03",            "PHYSICAL_SUPPORT", true,  false},
                {"A2_SOLO",           "PHYSICAL_SUPPORT", false, true },
                {"A3_G03_SOLO",       "PHYSICAL_SUPPORT", true,  true },
                {"A4_MATCHED_RANDOM", "MATCHED_RANDOM",   true,  true }
            };
            for (const CausalGroup& group : groups)
            {
                GinAR_mtx groupSignal = firstSignal;
                GinAR_opt groupOptions = makeFirstSignalOptions(
                    group.mode,
                    group.satellites,
                    group.receivers
                );
                const int groupFixed = rankAwareGnssAr(
                    trace,
                    groupSignal,
                    groupOptions,
                    time,
                    "LAYERED_FIRST_SIGNAL_" + group.name,
                    enforceHouCandidateNis
                );
                trace << "\nZHANG_L1_CANDIDATE_SUBLATTICE_SHADOW time="
                      << time.to_string(0)
                      << " system=" << enum_to_string(system)
                      << " group=" << group.name
                      << " input_l1_coordinate_columns="
                      << firstColumns.size()
                      << " input_integer_rows="
                      << groupSignal.lambda_ablation_input_rows
                      << " support_integer_rows="
                      << groupSignal.lambda_ablation_support_rows
                      << " removed_integer_rows="
                      << groupSignal.lambda_ablation_removed_rows
                      << " retained_integer_rows="
                      << groupSignal.lambda_ablation_retained_rows
                      << " fixed=" << groupFixed
                      << " nis=" << groupSignal.lambda_candidate_nis
                      << " threshold="
                      << groupSignal.lambda_candidate_nis_threshold
                      << " status=" << (groupFixed > 0
                            ? "ACCEPTED"
                            : "REJECTED")
                      << " scope=L1_INTEGER_ROWS_ONLY"
                      << " causal_measurement_replay=0"
                      << " feedback=" << (group.name == "A0_BASELINE"
                            ? "BASELINE_BRANCH_ONLY"
                            : "SHADOW_NONE");
                if (group.name == "A0_BASELINE")
                {
                    firstSignal = std::move(groupSignal);
                    firstFixed = groupFixed;
                }
            }
        }
        else
        {
            const string configuredMode =
                acsConfig.zhangPppAr.l1_candidate_shadow_ablation;
            GinAR_opt firstSignalOptions = makeFirstSignalOptions(
                configuredMode,
                true,
                true
            );
            firstFixed = rankAwareGnssAr(
                trace,
                firstSignal,
                firstSignalOptions,
                time,
                "LAYERED_FIRST_SIGNAL",
                enforceHouCandidateNis
            );
        }
        if (acsConfig.zhangPppAr.l1_measurement_replay_shadow &&
            acsConfig.zhangPppAr.l1_measurement_replay_target_epoch ==
                time.to_string(0))
        {
            traceZhangL1MeasurementReplayNis(
                trace,
                kfState,
                firstSignalFloat,
                firstSignal.lambda_candidate_tested_rows,
                firstSignal.lambda_candidate_tested_integers,
                firstSignal.lambda_candidate_nis,
                firstSignal.lambda_candidate_nis_threshold
            );
        }
        if (acsConfig.zhangPppAr.l1_multibranch_par_shadow)
        {
            if (firstFixed > 0)
            {
                trace << "\nZHANG_L1_MULTIBRANCH_SHADOW time="
                      << time.to_string(0)
                      << " system=" << enum_to_string(system)
                      << " baseline_fixed=" << firstFixed
                      << " status=SKIPPED_BASELINE_NIS_ACCEPTED"
                      << " feedback=SHADOW_NONE";
            }
            else
            {
                GinAR_mtx beamSource = firstSignalFloat;
                MatrixXd beamProductCrossCovariance =
                    beamProductProjection.crossCovariance;
                MatrixXd beamUserProductCrossCovariance =
                    beamProductProjection.userQuotientCrossCovariance;
                const vector<int> beamSelected =
                    positiveVarianceTargetSubset(beamSource.Paflt);
                if (beamSelected.empty())
                {
                    beamSource.aflt.resize(0);
                    beamSource.Paflt.resize(0, 0);
                    beamProductCrossCovariance.resize(
                        beamProductCrossCovariance.rows(), 0);
                    beamUserProductCrossCovariance.resize(
                        beamUserProductCrossCovariance.rows(), 0);
                }
                else if (static_cast<int>(beamSelected.size()) <
                         beamSource.aflt.size())
                {
                    beamSource.aflt =
                        beamSource.aflt(beamSelected).eval();
                    beamSource.Paflt = beamSource.Paflt(
                        beamSelected, beamSelected).eval();
                    beamProductCrossCovariance =
                        beamProductCrossCovariance(
                            Eigen::all, beamSelected).eval();
                    beamUserProductCrossCovariance =
                        beamUserProductCrossCovariance(
                            Eigen::all, beamSelected).eval();
                    map<int, KFKey> reducedMap;
                    for (int local = 0;
                         local < static_cast<int>(beamSelected.size());
                         local++)
                    {
                        auto key = firstSignalFloat.ambmap.find(
                            beamSelected[local]);
                        if (key != firstSignalFloat.ambmap.end())
                        {
                            reducedMap[local] = key->second;
                        }
                    }
                    beamSource.ambmap = std::move(reducedMap);
                }
                const bool canonicalPhysicalSearch =
                    acsConfig.zhangPppAr.l1_canonical_physical_search;
                ZhangCanonicalPhysicalSearchFrame canonicalFrame;
                MatrixXd searchCurrentToPhysical;
                string searchPhysicalAmbientFingerprint = "NOT_AVAILABLE";
                bool canonicalFrameValid = !canonicalPhysicalSearch;
                if (canonicalPhysicalSearch)
                {
                    ZhangGraphIntegerContext canonicalGraphContext;
                    const bool graphContextValid = zhangGraphIntegerContext(
                        kfState,
                        system,
                        canonicalGraphContext);
                    const bool physicalMatrixValid = graphContextValid &&
                        zhangCurrentCyclePhysicalMatrix(
                            canonicalGraphContext,
                            beamSource.ambmap,
                            searchCurrentToPhysical,
                            searchPhysicalAmbientFingerprint);
                    if (physicalMatrixValid)
                    {
                        canonicalFrame = zhangCanonicalPhysicalSearchFrame(
                            beamSource,
                            beamProductCrossCovariance,
                            beamUserProductCrossCovariance,
                            searchCurrentToPhysical,
                            searchPhysicalAmbientFingerprint);
                        canonicalFrameValid = canonicalFrame.valid;
                    }
                    trace << "\nZHANG_L1_CANONICAL_PHYSICAL_FRAME time="
                          << time.to_string(0)
                          << " system=" << enum_to_string(system)
                          << " stochastic_dimension="
                          << beamSource.aflt.size()
                          << " physical_ambient_dimension="
                          << searchCurrentToPhysical.cols()
                          << " physical_ambient_fingerprint="
                          << searchPhysicalAmbientFingerprint
                          << " canonical_physical_hnf="
                          << (canonicalFrameValid
                                ? canonicalFrame.canonicalPhysicalHnf
                                : "NOT_AVAILABLE")
                          << " valid=" << canonicalFrameValid
                          << " rank_tier_policy=SINGLE_CANONICAL_ROOT"
                          << " feedback=SHADOW_NONE";
                }
                GinAR_mtx searchSource = canonicalFrameValid &&
                    canonicalPhysicalSearch
                        ? canonicalFrame.source : beamSource;
                MatrixXd searchProductCrossCovariance =
                    canonicalFrameValid && canonicalPhysicalSearch
                        ? canonicalFrame.absoluteProductCross
                        : beamProductCrossCovariance;
                MatrixXd searchUserProductCrossCovariance =
                    canonicalFrameValid && canonicalPhysicalSearch
                        ? canonicalFrame.userProductCross
                        : beamUserProductCrossCovariance;
                if (canonicalPhysicalSearch && !canonicalFrameValid)
                {
                    searchSource.aflt.resize(0);
                    searchSource.Paflt.resize(0, 0);
                    searchProductCrossCovariance.resize(
                        searchProductCrossCovariance.rows(), 0);
                    searchUserProductCrossCovariance.resize(
                        searchUserProductCrossCovariance.rows(), 0);
                }
                GinAR_opt beamSearchOptions = options;
                beamSearchOptions.lambda_candidate_nis_alpha =
                    acsConfig.zhangPppAr.held_constraint_nis_alpha;
                beamSearchOptions.lambda_candidate_row_ablation = "NONE";
                beamSearchOptions.lambda_candidate_ablation_target_columns
                    .clear();
                GinAR_lambda_beam_options beamOptions;
                beamOptions.core_max_dimension =
                    acsConfig.zhangPppAr.l1_multibranch_core_max_dimension;
                beamOptions.reserve_dimension =
                    acsConfig.zhangPppAr.l1_multibranch_reserve_dimension;
                beamOptions.branch_factor =
                    acsConfig.zhangPppAr.l1_multibranch_branch_factor;
                beamOptions.beam_width =
                    acsConfig.zhangPppAr.l1_multibranch_beam_width;
                beamOptions.maximum_depth =
                    acsConfig.zhangPppAr.l1_multibranch_maximum_depth;
                beamOptions.minimum_rank =
                    acsConfig.zhangPppAr.l1_multibranch_minimum_rank;
                beamOptions.fixed_failure_rate =
                    acsConfig.predefined_fail;
                const bool userProductObjective = boost::iequals(
                    acsConfig.zhangPppAr.l1_multibranch_product_objective,
                    "USER_QUOTIENT");
                const MatrixXd& beamObjectiveCrossCovariance =
                    userProductObjective
                        ? searchUserProductCrossCovariance
                        : searchProductCrossCovariance;
                const MatrixXd& originalBeamObjectiveCrossCovariance =
                    userProductObjective
                        ? beamUserProductCrossCovariance
                        : beamProductCrossCovariance;
                const double beamObjectiveVarianceTrace =
                    userProductObjective
                        ? beamProductProjection.userQuotientVarianceTrace
                        : beamProductProjection.varianceTrace;
                beamOptions.prefer_product_gain = canonicalPhysicalSearch ||
                    acsConfig.zhangPppAr.l1_multibranch_evaluate_all_tiers;
                vector<int> coreCaps;
                int coreCap = beamOptions.core_max_dimension;
                if (canonicalPhysicalSearch)
                {
                    coreCaps.push_back(coreCap);
                }
                while (true)
                {
                    if (canonicalPhysicalSearch)
                    {
                        break;
                    }
                    coreCaps.push_back(coreCap);
                    if (coreCap <= beamOptions.minimum_rank)
                    {
                        break;
                    }
                    const int next = std::max(
                        beamOptions.minimum_rank,
                        coreCap / 2
                    );
                    if (next == coreCap)
                    {
                        break;
                    }
                    coreCap = next;
                }

                GinAR_lambda_beam_result beam;
                int attemptedTiers = 0;
                int selectedCoreCap = 0;
                int totalExplored = 0;
                int totalUnique = 0;
                int totalDuplicates = 0;
                int totalCompatible = 0;
                int firstCoreRank = 0;
                int firstPoolRank = 0;
                bool selectedFeasibleTier = false;
                for (int tierCoreCap : coreCaps)
                {
                    GinAR_lambda_beam_options tierOptions = beamOptions;
                    tierOptions.core_max_dimension = tierCoreCap;
                    tierOptions.context = time.to_string(0) + "_" +
                        enum_to_string(system) + "_L1_CORE" +
                        std::to_string(tierCoreCap);
                    GinAR_lambda_beam_result tier =
                        GNSS_AR_LAMBDA_BEAM_SHADOW(
                            trace,
                            searchSource,
                            beamSearchOptions,
                            tierOptions,
                            beamObjectiveCrossCovariance,
                            beamObjectiveVarianceTrace
                        );
                    attemptedTiers++;
                    totalExplored += tier.explored_nodes;
                    totalUnique += tier.unique_nodes;
                    totalDuplicates += tier.duplicate_nodes;
                    totalCompatible += tier.nis_compatible_nodes;
                    if (attemptedTiers == 1)
                    {
                        firstCoreRank = tier.initial_core_rank;
                        firstPoolRank = tier.initial_pool_rank;
                    }
                    trace << "\nZHANG_L1_MULTIBRANCH_TIER time="
                          << time.to_string(0)
                          << " system=" << enum_to_string(system)
                          << " core_cap=" << tierCoreCap
                          << " initial_core_rank="
                          << tier.initial_core_rank
                          << " initial_pool_rank="
                          << tier.initial_pool_rank
                          << " explored_nodes=" << tier.explored_nodes
                          << " duplicate_nodes=" << tier.duplicate_nodes
                          << " selected_rank=" << tier.selected_rank
                          << " nis=" << tier.selected_nis
                          << " nis_threshold="
                          << tier.selected_nis_threshold
                          << " product_objective="
                          << (userProductObjective
                                ? "USER_QUOTIENT" : "ABSOLUTE")
                          << " objective_gain_fraction="
                          << tier.selected_product_gain
                          << " status="
                          << (tier.nis_compatible_found
                                ? "NIS_COMPATIBLE_SUBLATTICE_FOUND"
                                : "NO_NIS_COMPATIBLE_SUBLATTICE")
                          << " feedback=SHADOW_NONE";
                    bool selectTier = false;
                    if (tier.nis_compatible_found)
                    {
                        selectTier = !selectedFeasibleTier;
                        if (selectedFeasibleTier)
                        {
                            const double candidateGain = std::isfinite(
                                tier.selected_product_gain)
                                    ? tier.selected_product_gain : -1;
                            const double selectedGain = std::isfinite(
                                beam.selected_product_gain)
                                    ? beam.selected_product_gain : -1;
                            if (beamOptions.prefer_product_gain)
                            {
                                selectTier = candidateGain > selectedGain ||
                                    (candidateGain == selectedGain &&
                                     tier.selected_rank > beam.selected_rank);
                            }
                            else
                            {
                                selectTier =
                                    tier.selected_rank > beam.selected_rank ||
                                    (tier.selected_rank == beam.selected_rank &&
                                     candidateGain > selectedGain);
                            }
                        }
                    }
                    else if (!selectedFeasibleTier)
                    {
                        beam = tier;
                    }
                    if (selectTier)
                    {
                        beam = std::move(tier);
                        selectedCoreCap = tierCoreCap;
                        selectedFeasibleTier = true;
                    }
                    if (selectedFeasibleTier &&
                        !acsConfig.zhangPppAr
                            .l1_multibranch_evaluate_all_tiers)
                    {
                        break;
                    }
                }
                beam.initial_core_rank = firstCoreRank;
                beam.initial_pool_rank = firstPoolRank;
                beam.explored_nodes = totalExplored;
                beam.unique_nodes = totalUnique;
                beam.duplicate_nodes = totalDuplicates;
                beam.nis_compatible_nodes = totalCompatible;
                if (canonicalPhysicalSearch && canonicalFrameValid &&
                    beam.nis_compatible_found)
                {
                    beam.selected_integer_rows =
                        (beam.selected_integer_rows *
                         canonicalFrame.currentToCanonical).eval();
                    beam.selected_hnf_fingerprint =
                        zhangIntegerRowHnfFingerprint(
                            beam.selected_integer_rows);
                    beam.selected_product_gain =
                        zhangConstraintProductInformationGain(
                            originalBeamObjectiveCrossCovariance,
                            beamObjectiveVarianceTrace,
                            beamSource.Paflt,
                            beam.selected_integer_rows);
                    beam.product_gain_available =
                        std::isfinite(beam.selected_product_gain);
                }
                if (e24aGainAuditRequested)
                {
                    vector<int> beamStateIndices;
                    bool stateMappingValid =
                        beam.nis_compatible_found &&
                        e24aPF0.rows() == kfState.P.rows() &&
                        e24aPWideLane.rows() == kfState.P.rows();
                    for (int column = 0;
                         stateMappingValid &&
                         column < beamSource.aflt.size(); column++)
                    {
                        auto ambiguity = beamSource.ambmap.find(column);
                        auto state = ambiguity == beamSource.ambmap.end()
                            ? kfState.kfIndexMap.end()
                            : kfState.kfIndexMap.find(ambiguity->second);
                        if (state == kfState.kfIndexMap.end())
                        {
                            stateMappingValid = false;
                            break;
                        }
                        beamStateIndices.push_back(state->second);
                    }
                    if (stateMappingValid &&
                        beam.selected_integer_rows.cols() ==
                            static_cast<int>(beamStateIndices.size()))
                    {
                        vector<Eigen::Triplet<double>> parTriplets;
                        for (int row = 0;
                             row < beam.selected_integer_rows.rows(); row++)
                        {
                            for (int column = 0;
                                 column <
                                    beam.selected_integer_rows.cols();
                                 column++)
                            {
                                const double coefficient =
                                    beam.selected_integer_rows(row, column);
                                if (coefficient != 0)
                                {
                                    parTriplets.emplace_back(
                                        row,
                                        beamStateIndices[column],
                                        coefficient);
                                }
                            }
                        }
                        ZhangIarFunctional parConstraints(
                            beam.selected_integer_rows.rows(),
                            kfState.P.rows());
                        parConstraints.setFromTriplets(
                            parTriplets.begin(), parTriplets.end(),
                            [](double left, double right)
                            {
                                return left + right;
                            });
                        parConstraints.makeCompressed();

                        vector<Eigen::Triplet<double>> fullTriplets;
                        fullTriplets.reserve(beamStateIndices.size());
                        for (int row = 0;
                             row < static_cast<int>(
                                beamStateIndices.size()); row++)
                        {
                            fullTriplets.emplace_back(
                                row, beamStateIndices[row], 1);
                        }
                        ZhangIarFunctional fullConstraints(
                            beamStateIndices.size(), kfState.P.rows());
                        fullConstraints.setFromTriplets(
                            fullTriplets.begin(), fullTriplets.end());
                        fullConstraints.makeCompressed();
                        traceZhangIarGainAudit(
                            trace,
                            kfState,
                            system,
                            time,
                            e24aPF0,
                            e24aPWideLane,
                            parConstraints,
                            fullConstraints);
                        if (acsConfig.zhangPppAr
                                .e29_real_math_closure_shadow
                            && (acsConfig.zhangPppAr
                                    .e29_real_math_closure_target_epoch.empty()
                                || acsConfig.zhangPppAr
                                    .e29_real_math_closure_target_epoch
                                    == time.to_string(0)))
                        {
                            traceZhangE29RealMathClosure(
                                trace,
                                kfState,
                                system,
                                time,
                                e24aPF0,
                                e24aPWideLane,
                                parConstraints,
                                fullConstraints);
                        }
                    }
                    else
                    {
                        trace << "\nZHANG_E24A_IAR_GAIN_SUMMARY time="
                              << time.to_string(0)
                              << " system=" << enum_to_string(system)
                              << " status=SKIPPED_INVALID_PAR_OR_STATE_"
                                 "MAPPING"
                              << " ar_authorized=0 feedback=SHADOW_NONE";
                    }
                }
                const MatrixXd fullIntegerSpace = MatrixXd::Identity(
                    beamSource.aflt.size(), beamSource.aflt.size());
                const double absoluteProductCeiling =
                    zhangConstraintProductInformationGain(
                        beamProductCrossCovariance,
                        beamProductProjection.varianceTrace,
                        beamSource.Paflt,
                        fullIntegerSpace);
                const double userProductCeiling =
                    zhangConstraintProductInformationGain(
                        beamUserProductCrossCovariance,
                        beamProductProjection.userQuotientVarianceTrace,
                        beamSource.Paflt,
                        fullIntegerSpace);
                const double selectedUserProductGain =
                    zhangConstraintProductInformationGain(
                        beamUserProductCrossCovariance,
                        beamProductProjection.userQuotientVarianceTrace,
                        beamSource.Paflt,
                        beam.selected_integer_rows);
                const double selectedAbsoluteProductGain =
                    zhangConstraintProductInformationGain(
                        beamProductCrossCovariance,
                        beamProductProjection.varianceTrace,
                        beamSource.Paflt,
                        beam.selected_integer_rows);
                const double absoluteProductEfficiency =
                    absoluteProductCeiling > 0 &&
                    std::isfinite(selectedAbsoluteProductGain)
                        ? selectedAbsoluteProductGain / absoluteProductCeiling
                        : std::numeric_limits<double>::quiet_NaN();
                const double userProductEfficiency =
                    userProductCeiling > 0 &&
                    std::isfinite(selectedUserProductGain)
                        ? selectedUserProductGain / userProductCeiling
                        : std::numeric_limits<double>::quiet_NaN();
                if (acsConfig.zhangPppAr.l1_subset_oracle_shadow &&
                    (acsConfig.zhangPppAr.l1_subset_oracle_target_epoch
                        .empty() ||
                     acsConfig.zhangPppAr.l1_subset_oracle_target_epoch ==
                        time.to_string(0)))
                {
                    GinAR_lambda_subset_oracle_options oracleOptions;
                    oracleOptions.pool_dimension = acsConfig.zhangPppAr
                        .l1_subset_oracle_pool_dimension;
                    oracleOptions.minimum_rank = acsConfig.zhangPppAr
                        .l1_subset_oracle_minimum_rank;
                    oracleOptions.maximum_rank = acsConfig.zhangPppAr
                        .l1_subset_oracle_maximum_rank;
                    oracleOptions.maximum_subsets = acsConfig.zhangPppAr
                        .l1_subset_oracle_maximum_subsets;
                    oracleOptions.fixed_failure_rate =
                        beamOptions.fixed_failure_rate;
                    oracleOptions.context = time.to_string(0) + "_" +
                        enum_to_string(system) + "_E23B_BOUNDED_ORACLE";
                    GinAR_lambda_subset_oracle_result oracle =
                        GNSS_AR_LAMBDA_SUBSET_ORACLE_SHADOW(
                            trace,
                            searchSource,
                            beamSearchOptions,
                            oracleOptions,
                            beamObjectiveCrossCovariance,
                            beamObjectiveVarianceTrace);
                    const double gainRatio =
                        beam.selected_product_gain > 0 &&
                        std::isfinite(oracle.selected_product_gain)
                            ? oracle.selected_product_gain /
                                beam.selected_product_gain
                            : std::numeric_limits<double>::quiet_NaN();
                    trace << "\nZHANG_E23B_ORACLE_AUDIT time="
                          << time.to_string(0)
                          << " system=" << enum_to_string(system)
                          << " dictionary_rank="
                          << oracle.dictionary_rank
                          << " enumerated_subsets="
                          << oracle.enumerated_subsets
                          << " unique_sublattices="
                          << oracle.unique_sublattices
                          << " feasible_sublattices="
                          << oracle.feasible_sublattices
                          << " oracle_selected_rank="
                          << oracle.selected_rank
                          << " oracle_objective_gain_fraction="
                          << oracle.selected_product_gain
                          << " beam_selected_rank="
                          << beam.selected_rank
                          << " beam_objective_gain_fraction="
                          << beam.selected_product_gain
                          << " oracle_over_beam_gain_ratio="
                          << gainRatio
                          << " oracle_hnf="
                          << oracle.selected_hnf_fingerprint
                          << " beam_hnf="
                          << beam.selected_hnf_fingerprint
                          << " oracle_scope="
                             "BOUNDED_SUBSETS_OF_LAMBDA_RELIABLE_DICTIONARY"
                          << " ar_authorized=0 feedback=SHADOW_NONE";

                    const string baseMappedHnf =
                        zhangIntegerRowHnfFingerprint(
                            beam.selected_integer_rows);
                    const string baseMappedAffine =
                        zhangIntegerAffineHnfFingerprint(
                            beam.selected_integer_rows,
                            beam.selected_integer_values);
                    for (int trial = 0; trial < 4; trial++)
                    {
                        const MatrixXd unimodular =
                            zhangRandomElementaryUnimodularTransform(
                                beamSource.aflt.size(),
                                stableZhangAblationSeed(
                                    2300 + trial,
                                    time.to_string(0),
                                    system),
                                24);
                        GinAR_mtx rebasedSource = beamSource;
                        rebasedSource.aflt =
                            unimodular * beamSource.aflt;
                        rebasedSource.Paflt =
                            unimodular * beamSource.Paflt *
                            unimodular.transpose();
                        const MatrixXd rebasedAbsoluteProductCross =
                            beamProductCrossCovariance *
                            unimodular.transpose();
                        const MatrixXd rebasedUserProductCross =
                            beamUserProductCrossCovariance *
                            unimodular.transpose();
                        MatrixXd rebasedObjectiveCross =
                            userProductObjective
                                ? rebasedUserProductCross
                                : rebasedAbsoluteProductCross;
                        ZhangCanonicalPhysicalSearchFrame rebasedFrame;
                        bool rebasedCanonicalFrameValid =
                            !canonicalPhysicalSearch;
                        if (canonicalPhysicalSearch && canonicalFrameValid)
                        {
                            const MatrixXd rebasedCurrentToPhysical =
                                unimodular *
                                canonicalFrame.currentToPhysical;
                            rebasedFrame =
                                zhangCanonicalPhysicalSearchFrame(
                                    rebasedSource,
                                    rebasedAbsoluteProductCross,
                                    rebasedUserProductCross,
                                    rebasedCurrentToPhysical,
                                    canonicalFrame
                                        .physicalAmbientFingerprint);
                            rebasedCanonicalFrameValid =
                                rebasedFrame.valid;
                            if (rebasedCanonicalFrameValid)
                            {
                                rebasedSource = rebasedFrame.source;
                                rebasedObjectiveCross = userProductObjective
                                    ? rebasedFrame.userProductCross
                                    : rebasedFrame.absoluteProductCross;
                            }
                            else
                            {
                                rebasedSource.aflt.resize(0);
                                rebasedSource.Paflt.resize(0, 0);
                                rebasedObjectiveCross.resize(
                                    rebasedObjectiveCross.rows(), 0);
                            }
                        }
                        else if (canonicalPhysicalSearch)
                        {
                            rebasedSource.aflt.resize(0);
                            rebasedSource.Paflt.resize(0, 0);
                            rebasedObjectiveCross.resize(
                                rebasedObjectiveCross.rows(), 0);
                        }
                        GinAR_lambda_beam_options rebasedOptions =
                            beamOptions;
                        rebasedOptions.core_max_dimension =
                            selectedCoreCap > 0
                                ? selectedCoreCap
                                : beamOptions.core_max_dimension;
                        rebasedOptions.prefer_product_gain = true;
                        rebasedOptions.context = time.to_string(0) + "_" +
                            enum_to_string(system) + "_E23B_REBASE_" +
                            std::to_string(trial);
                        GinAR_lambda_beam_result rebased =
                            GNSS_AR_LAMBDA_BEAM_SHADOW(
                                trace,
                                rebasedSource,
                                beamSearchOptions,
                                rebasedOptions,
                                rebasedObjectiveCross,
                                beamObjectiveVarianceTrace);
                        MatrixXd mappedRows;
                        if (rebased.nis_compatible_found &&
                            (!canonicalPhysicalSearch ||
                             rebasedCanonicalFrameValid))
                        {
                            MatrixXd rebasedCurrentRows =
                                rebased.selected_integer_rows;
                            if (canonicalPhysicalSearch)
                            {
                                rebasedCurrentRows =
                                    rebasedCurrentRows *
                                    rebasedFrame.currentToCanonical;
                            }
                            mappedRows = rebasedCurrentRows * unimodular;
                        }
                        const double mappedGain =
                            zhangConstraintProductInformationGain(
                                originalBeamObjectiveCrossCovariance,
                                beamObjectiveVarianceTrace,
                                beamSource.Paflt,
                                mappedRows);
                        const string mappedHnf =
                            zhangIntegerRowHnfFingerprint(mappedRows);
                        const string mappedAffine =
                            zhangIntegerAffineHnfFingerprint(
                                mappedRows,
                                rebased.selected_integer_values);
                        const bool rankInvariant =
                            rebased.selected_rank == beam.selected_rank;
                        const bool gainInvariant =
                            std::isfinite(mappedGain) &&
                            std::abs(
                                mappedGain - beam.selected_product_gain) <=
                                1e-12;
                        const bool homogeneousInvariant =
                            mappedHnf == baseMappedHnf;
                        const bool affineInvariant =
                            mappedAffine == baseMappedAffine;
                        trace << "\nZHANG_E23C_REBASE_AUDIT time="
                              << time.to_string(0)
                              << " system=" << enum_to_string(system)
                              << " trial=" << trial
                              << " elementary_operations=24"
                              << " canonical_physical_search="
                              << canonicalPhysicalSearch
                              << " canonical_frame_valid="
                              << rebasedCanonicalFrameValid
                              << " rebased_feasible="
                              << rebased.nis_compatible_found
                              << " base_rank=" << beam.selected_rank
                              << " rebased_rank="
                              << rebased.selected_rank
                              << " rank_invariant=" << rankInvariant
                              << " base_objective_gain_fraction="
                              << beam.selected_product_gain
                              << " rebased_mapped_gain_fraction="
                              << mappedGain
                              << " gain_invariant=" << gainInvariant
                              << " base_hnf=" << baseMappedHnf
                              << " mapped_hnf=" << mappedHnf
                              << " homogeneous_invariant="
                              << homogeneousInvariant
                              << " base_affine=" << baseMappedAffine
                              << " mapped_affine=" << mappedAffine
                              << " affine_invariant=" << affineInvariant
                              << " search_invariant="
                              << (rankInvariant && gainInvariant &&
                                  homogeneousInvariant && affineInvariant)
                              << " interpretation="
                                 "CANONICAL_PHYSICAL_LATTICE_AUDIT"
                              << " ar_authorized=0 feedback=SHADOW_NONE";
                    }
                }
                string selectedPhysicalHnf = "NOT_AVAILABLE";
                string selectedPhysicalAffine = "NOT_AVAILABLE";
                bool physicalMappingValid = false;
                bool jointWlL1AuditValid = false;
                bool jointWlL1Primitive = false;
                bool jointWlL1AffineConsistent = false;
                int jointWlL1Rank = 0;
                string jointWlL1SaturationIndex = "NOT_AVAILABLE";
                if (beam.nis_compatible_found &&
                    beam.selected_integer_rows.cols() ==
                        static_cast<int>(beamSelected.size()))
                {
                    ZhangGraphIntegerContext graphContext;
                    physicalMappingValid = zhangGraphIntegerContext(
                        kfState,
                        system,
                        graphContext
                    );
                    ZhangPersistentHeldLattice selectedPhysicalLattice;
                    vector<string> physicalTerms(
                        beam.selected_integer_rows.rows()
                    );
                    vector<int> physicalSupport(
                        beam.selected_integer_rows.rows(),
                        0
                    );
                    vector<bool> rowMappingValid(
                        beam.selected_integer_rows.rows(),
                        physicalMappingValid
                    );
                    for (int row = 0;
                         row < beam.selected_integer_rows.rows(); row++)
                    {
                        ZhangPersistentHeldRow physicalRow;
                        for (int stochastic = 0;
                             stochastic < beam.selected_integer_rows.cols();
                             stochastic++)
                        {
                            const double rawCoefficient =
                                beam.selected_integer_rows(row, stochastic);
                            const long long coefficient =
                                std::llround(rawCoefficient);
                            if (coefficient == 0)
                            {
                                continue;
                            }
                            if (std::abs(
                                    rawCoefficient -
                                    static_cast<double>(coefficient)
                                ) > 1e-10)
                            {
                                rowMappingValid[row] = false;
                                break;
                            }
                            const int firstSignalColumn =
                                beamSelected[stochastic];
                            auto key = firstSignalFloat.ambmap.find(
                                firstSignalColumn);
                            if (key == firstSignalFloat.ambmap.end())
                            {
                                rowMappingValid[row] = false;
                                break;
                            }
                            if (!addCurrentCycleToPhysicalRow(
                                    graphContext,
                                    static_cast<E_ObsCode>(key->second.num),
                                    {key->second.str, key->second.Sat},
                                    ZhangExactInteger(coefficient),
                                    physicalRow.coefficients
                                ))
                            {
                                rowMappingValid[row] = false;
                                break;
                            }
                        }
                        const double rawInteger =
                            beam.selected_integer_values(row);
                        const long long fixedInteger =
                            std::llround(rawInteger);
                        if (std::abs(
                                rawInteger -
                                static_cast<double>(fixedInteger)
                            ) > 1e-10 || physicalRow.coefficients.empty())
                        {
                            rowMappingValid[row] = false;
                        }
                        physicalRow.value = ZhangExactInteger(fixedInteger);
                        if (rowMappingValid[row])
                        {
                            std::ostringstream terms;
                            for (const auto& [arc, coefficient] :
                                 physicalRow.coefficients)
                            {
                                if (physicalSupport[row]++)
                                {
                                    terms << ";";
                                }
                                terms << enum_to_string(arc.code) << ":"
                                      << arc.edge.receiver << ":"
                                      << arc.edge.satellite.id() << ":A"
                                      << arc.version << ":"
                                      << coefficient;
                            }
                            physicalTerms[row] = terms.str();
                            selectedPhysicalLattice.rows.push_back(
                                std::move(physicalRow)
                            );
                        }
                        physicalMappingValid &= rowMappingValid[row];
                    }
                    if (physicalMappingValid)
                    {
                        ZhangPersistentHeldLattice canonical =
                            selectedPhysicalLattice;
                        normalisePersistentHeldLattice(canonical);
                        physicalMappingValid = canonical.consistent &&
                            canonical.rows.size() ==
                                static_cast<size_t>(
                                    beam.selected_integer_rows.rows());
                    }
                    if (physicalMappingValid)
                    {
                        selectedPhysicalHnf =
                            zhangPhysicalHeldLatticeFingerprint(
                                selectedPhysicalLattice);
                        selectedPhysicalAffine =
                            zhangPhysicalAffineLatticeFingerprint(
                                selectedPhysicalLattice);

                        ZhangPersistentHeldLattice jointPhysicalLattice =
                            selectedPhysicalLattice;
                        bool wideLanePhysicalMappingValid =
                            fullWideLaneRows.rows() == wideLane.zfix.size();
                        for (int row = 0;
                             row < fullWideLaneRows.rows() &&
                                wideLanePhysicalMappingValid;
                             row++)
                        {
                            ZhangPersistentHeldRow physicalWideLane;
                            for (int column = 0;
                                 column < fullWideLaneRows.cols(); column++)
                            {
                                const double rawCoefficient =
                                    fullWideLaneRows(row, column);
                                const long long coefficient =
                                    std::llround(rawCoefficient);
                                if (coefficient == 0)
                                {
                                    continue;
                                }
                                if (std::abs(
                                        rawCoefficient -
                                        static_cast<double>(coefficient)
                                    ) > 1e-10)
                                {
                                    wideLanePhysicalMappingValid = false;
                                    break;
                                }
                                auto key = ambiguityResolution.ambmap.find(
                                    column);
                                if (key == ambiguityResolution.ambmap.end() ||
                                    !addCurrentCycleToPhysicalRow(
                                        graphContext,
                                        static_cast<E_ObsCode>(
                                            key->second.num),
                                        {key->second.str, key->second.Sat},
                                        ZhangExactInteger(coefficient),
                                        physicalWideLane.coefficients))
                                {
                                    wideLanePhysicalMappingValid = false;
                                    break;
                                }
                            }
                            const double rawValue = wideLane.zfix(row);
                            const long long value = std::llround(rawValue);
                            if (std::abs(
                                    rawValue - static_cast<double>(value)
                                ) > 1e-10 ||
                                physicalWideLane.coefficients.empty())
                            {
                                wideLanePhysicalMappingValid = false;
                                break;
                            }
                            physicalWideLane.value =
                                ZhangExactInteger(value);
                            jointPhysicalLattice.rows.push_back(
                                std::move(physicalWideLane));
                        }
                        if (wideLanePhysicalMappingValid)
                        {
                            normalisePersistentHeldLattice(
                                jointPhysicalLattice);
                            jointWlL1AffineConsistent =
                                jointPhysicalLattice.consistent;
                        }
                        if (jointWlL1AffineConsistent)
                        {
                            set<ZhangPhysicalIntegerArc> jointColumns;
                            for (const auto& held :
                                 jointPhysicalLattice.rows)
                            {
                                for (const auto& [arc, coefficient] :
                                     held.coefficients)
                                {
                                    if (coefficient != 0)
                                    {
                                        jointColumns.insert(arc);
                                    }
                                }
                            }
                            vector<ZhangPhysicalIntegerArc> columns(
                                jointColumns.begin(), jointColumns.end());
                            map<ZhangPhysicalIntegerArc, int> columnIndex;
                            for (int column = 0;
                                 column < static_cast<int>(columns.size());
                                 column++)
                            {
                                columnIndex[columns[column]] = column;
                            }
                            ZhangExactMatrix exactRows;
                            for (const auto& held :
                                 jointPhysicalLattice.rows)
                            {
                                ZhangExactVector exact(columns.size());
                                for (const auto& [arc, coefficient] :
                                     held.coefficients)
                                {
                                    exact[columnIndex.at(arc)] = coefficient;
                                }
                                exactRows.push_back(std::move(exact));
                            }
                            ZhangIntegerLatticeMembership saturation =
                                zhangIntegerRowLatticeContains(
                                    exactRows,
                                    ZhangExactVector(columns.size()));
                            jointWlL1Rank = saturation.rank;
                            ZhangExactInteger saturationIndex = 1;
                            jointWlL1Primitive =
                                saturation.rank ==
                                    static_cast<int>(exactRows.size());
                            for (const ZhangExactInteger& invariant :
                                 saturation.smithInvariants)
                            {
                                const ZhangExactInteger magnitude =
                                    zhangExactAbs(invariant);
                                saturationIndex *= magnitude;
                                jointWlL1Primitive &= magnitude == 1;
                            }
                            jointWlL1SaturationIndex =
                                saturationIndex.convert_to<string>();
                            jointWlL1AuditValid = true;
                        }
                    }
                    for (int row = 0;
                         row < beam.selected_integer_rows.rows(); row++)
                    {
                        trace << "\nZHANG_L1_MULTIBRANCH_SELECTED_ROW time="
                              << time.to_string(0)
                              << " system=" << enum_to_string(system)
                              << " row=" << row
                              << " rank="
                              << beam.selected_integer_rows.rows()
                              << " fixed_integer="
                              << beam.selected_integer_values(row)
                              << " physical_mapping_valid="
                              << rowMappingValid[row]
                              << " physical_support_count="
                              << physicalSupport[row]
                              << " physical_terms=" << physicalTerms[row]
                              << " physical_hnf=" << selectedPhysicalHnf
                              << " physical_affine_key="
                              << selectedPhysicalAffine
                              << " ar_authorized=0"
                              << " feedback=SHADOW_NONE";
                    }
                }
                trace << "\nZHANG_L1_MULTIBRANCH_SHADOW time="
                      << time.to_string(0)
                      << " system=" << enum_to_string(system)
                      << " baseline_fixed=0"
                      << " input_l1_coordinate_columns="
                      << firstColumns.size()
                      << " stochastic_columns=" << beamSource.aflt.size()
                      << " product_targets="
                      << beamProductProjection.productCount
                      << " user_quotient_rank="
                      << beamProductProjection.userQuotientRank
                      << " canonical_physical_search="
                      << canonicalPhysicalSearch
                      << " canonical_frame_valid="
                      << canonicalFrameValid
                      << " physical_ambient_fingerprint="
                      << searchPhysicalAmbientFingerprint
                      << " canonical_physical_hnf="
                      << (canonicalFrameValid && canonicalPhysicalSearch
                            ? canonicalFrame.canonicalPhysicalHnf
                            : "NOT_AVAILABLE")
                      << " rank_tier_policy="
                      << (canonicalPhysicalSearch
                            ? "SINGLE_CANONICAL_ROOT"
                            : "GEOMETRIC_RANK_TIERS")
                      << " attempted_core_tiers=" << attemptedTiers
                      << " evaluated_all_core_tiers="
                      << (canonicalPhysicalSearch ||
                          acsConfig.zhangPppAr
                            .l1_multibranch_evaluate_all_tiers)
                      << " search_product_objective="
                      << (userProductObjective
                            ? "USER_QUOTIENT" : "ABSOLUTE")
                      << " search_objective_gain_fraction="
                      << beam.selected_product_gain
                      << " selected_core_cap=" << selectedCoreCap
                      << " initial_core_rank=" << beam.initial_core_rank
                      << " initial_pool_rank=" << beam.initial_pool_rank
                      << " explored_nodes=" << beam.explored_nodes
                      << " unique_nodes=" << beam.unique_nodes
                      << " duplicate_nodes=" << beam.duplicate_nodes
                      << " nis_compatible_nodes="
                      << beam.nis_compatible_nodes
                      << " selected_rank=" << beam.selected_rank
                      << " selected_depth=" << beam.selected_depth
                      << " bootstrap_success="
                      << beam.selected_bootstrap_success
                      << " bootstrap_log_failure="
                      << beam.selected_bootstrap_log_failure
                      << " candidate_ratio="
                      << beam.selected_candidate_ratio
                      << " fixed_failure_rate="
                      << beamOptions.fixed_failure_rate
                      << " nis_alpha="
                      << beamSearchOptions.lambda_candidate_nis_alpha
                      << " nis_gate_semantics="
                         "UPPER_TAIL_FALSE_REJECTION_DIAGNOSTIC_NOT_"
                         "WRONG_FIX_CONTROL"
                      << " ffrt_status="
                      << (beam.selected_ffrt_pass ? "PASSED" : "NOT_PASSED")
                      << " nis=" << beam.selected_nis
                      << " nis_threshold="
                      << beam.selected_nis_threshold
                      << " product_information_gain_fraction="
                      << selectedAbsoluteProductGain
                      << " product_full_fix_ceiling_fraction="
                      << absoluteProductCeiling
                      << " product_ceiling_efficiency="
                      << absoluteProductEfficiency
                      << " user_product_information_gain_fraction="
                      << selectedUserProductGain
                      << " user_product_full_fix_ceiling_fraction="
                      << userProductCeiling
                      << " user_product_ceiling_efficiency="
                      << userProductEfficiency
                      << " product_gain_status="
                      << (beam.product_gain_available
                            ? "EXACT_HOU_PRODUCT_TRACE_REDUCTION_FRACTION"
                            : "NOT_AVAILABLE_NO_PRODUCT_FUNCTIONAL")
                      << " coordinate_hnf="
                      << beam.selected_hnf_fingerprint
                      << " physical_hnf=" << selectedPhysicalHnf
                      << " physical_affine_key="
                      << selectedPhysicalAffine
                      << " physical_mapping_valid="
                      << physicalMappingValid
                      << " joint_wl_l1_audit_valid="
                      << jointWlL1AuditValid
                      << " joint_wl_l1_affine_consistent="
                      << jointWlL1AffineConsistent
                      << " joint_wl_l1_rank=" << jointWlL1Rank
                      << " joint_wl_l1_primitive="
                      << jointWlL1Primitive
                      << " joint_wl_l1_saturation_index="
                      << jointWlL1SaturationIndex
                      << " status="
                      << (beam.nis_compatible_found
                            ? "NIS_COMPATIBLE_SUBLATTICE_FOUND"
                            : "NO_NIS_COMPATIBLE_SUBLATTICE")
                      << " ar_authorized=0"
                      << " authorization_reason="
                         "E23C_WHOLE_ALGORITHM_FAILURE_RATE_NOT_CALIBRATED"
                      << " feedback=SHADOW_NONE";
            }
        }
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
            bool firstSignalCommitted = appendAndApply(
                fullFirstRows, firstSignal.zfix, "LAYERED_FIRST_SIGNAL"
            );
            if (!firstSignalCommitted)
            {
                continue;
            }
            phaseFixedSystems++;
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
    if (allSystemsPhaseFixed)
    {
        *allSystemsPhaseFixed = configuredSystems > 0 &&
            phaseFixedSystems == configuredSystems;
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
    const KFState&   floatState,
    GinAR_mtx&       ambiguityResolution,
    const GinAR_opt& options,
    GTime            time
)
{
	if (acsConfig.zhangPppAr.product_mode == "HOU_OSB_LIKE")
	{
		trace << "\nZHANG_HOU_OSB_LIKE_AR_REJECT time="
			<< time.to_string(0)
			<< " requested_strategy=PRODUCT_TARGET_WL_L1"
			<< " required_strategy=NETWORK_CYCLE_LATTICE"
			<< " action=NO_INTEGER_FEEDBACK";
		return 0;
	}
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

    auto appendAndApply = [&](const MatrixXd& rows,
                              const VectorXd& values,
                              const string& stageName)
    {
        if (rows.rows() == 0)
        {
            return true;
        }
        GinAR_mtx stage;
        stage.ambmap = ambiguityResolution.ambmap;
        stage.Ztrs   = rows;
        stage.zfix   = values;
        applyUCAmbiguities(trace, kfState, stage, stageName);
        if (zhangTransactionalConditioningFailed)
        {
            string rejectionReason = zhangTransactionalConditioningReason;
            trace << "\nZHANG_FIXED_SUBTRANSACTION time="
                  << time.to_string(0)
                  << " stage=" << stageName
                  << " status=ROLLED_BACK"
                  << " reason=" << rejectionReason
                  << " retained_rows=" << fixedRows.size();
            // Exact equality conditioning has not modified the state on a
            // failed gate, so only the local-stage failure marker is cleared.
            if (acsConfig.zhangPppAr.transactional_integer_fixing)
            {
                zhangTransactionalConditioningFailed = false;
                zhangTransactionalConditioningReason.clear();
            }
            return false;
        }
        for (int row = 0; row < rows.rows(); row++)
        {
            fixedRows.push_back(rows.row(row).transpose());
            fixedValues.push_back(values(row));
        }
        totalFixed += rows.rows();
        trace << "\nZHANG_FIXED_SUBTRANSACTION time="
              << time.to_string(0)
              << " stage=" << stageName
              << " status=COMMITTED"
              << " committed_rows=" << rows.rows()
              << " total_committed_rows=" << fixedRows.size();
        return true;
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
            map<ZhangPhysicalIntegerArc, ZhangExactInteger> physicalWideLaneRow;
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
                if (!addExactTargetToPhysicalRow(
                        context,
                        firstCode,
                        target.currentChords,
                        exact,
                        ZhangExactInteger(1),
                        candidate.physicalWideLaneRow
                    ) ||
                    !addExactTargetToPhysicalRow(
                        context,
                        secondCode,
                        target.currentChords,
                        exact,
                        ZhangExactInteger(-1),
                        candidate.physicalWideLaneRow
                    ))
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

			auto captureAlignedIntegerDatum = [&](const SatSys& anchor,
											const SatSys& satellite,
											const string& canonicalSetIdentity)
			{
				auto anchorRow = namedTargetRows.find(anchor);
				auto satelliteRow = namedTargetRows.find(satellite);
				if (anchorRow == namedTargetRows.end()
				 || satelliteRow == namedTargetRows.end())
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
				VectorXd firstRow;
				VectorXd secondRow;
				if (!makeAmbiguityRow(exact, firstCode, firstRow)
				 || !makeAmbiguityRow(exact, secondCode, secondRow))
				{
					return;
				}
				map<ZhangPhysicalIntegerArc, ZhangExactInteger> firstPhysicalRow;
				map<ZhangPhysicalIntegerArc, ZhangExactInteger> secondPhysicalRow;
				if (!addExactTargetToPhysicalRow(
						context, firstCode, target.currentChords, exact,
						ZhangExactInteger(1), firstPhysicalRow)
				 || !addExactTargetToPhysicalRow(
						context, secondCode, target.currentChords, exact,
						ZhangExactInteger(1), secondPhysicalRow))
				{
					return;
				}
				auto mapToFullState = [&](const VectorXd& ambiguityRow,
					VectorXd& fullStateRow)
				{
					fullStateRow = VectorXd::Zero(floatState.x.size());
					for (int column = 0; column < ambiguityRow.size(); column++)
					{
						if (ambiguityRow(column) == 0)
						{
							continue;
						}
						auto ambiguityKey = ambiguityResolution.ambmap.find(column);
						auto stateColumn = ambiguityKey == ambiguityResolution.ambmap.end()
							? floatState.kfIndexMap.end()
							: floatState.kfIndexMap.find(ambiguityKey->second);
						if (ambiguityKey == ambiguityResolution.ambmap.end()
						 || stateColumn == floatState.kfIndexMap.end())
						{
							return false;
						}
						fullStateRow(stateColumn->second) = ambiguityRow(column);
					}
					return true;
				};
				VectorXd firstFullStateRow;
				VectorXd secondFullStateRow;
				if (!mapToFullState(firstRow, firstFullStateRow)
				 || !mapToFullState(secondRow, secondFullStateRow))
				{
					return;
				}
				const auto firstAnchorStatus = zhangSatelliteDatumStatus(
					system, firstCode, anchor);
				const auto firstSatelliteStatus = zhangSatelliteDatumStatus(
					system, firstCode, satellite);
				const auto secondAnchorStatus = zhangSatelliteDatumStatus(
					system, secondCode, anchor);
				const auto secondSatelliteStatus = zhangSatelliteDatumStatus(
					system, secondCode, satellite);
				long long firstPersistentDifference = 0;
				long long secondPersistentDifference = 0;
				const bool firstRelationKnown = queryZhangSatelliteProductRelation(
					system, firstCode, anchor, satellite,
					firstPersistentDifference);
				const bool secondRelationKnown = queryZhangSatelliteProductRelation(
					system, secondCode, anchor, satellite,
					secondPersistentDifference);
				const bool firstExactTransport =
					firstAnchorStatus.integerDatumContinuous
					&& firstSatelliteStatus.integerDatumContinuous
					&& firstRelationKnown
					&& firstAnchorStatus.componentId
						== firstSatelliteStatus.componentId;
				const bool secondExactTransport =
					secondAnchorStatus.integerDatumContinuous
					&& secondSatelliteStatus.integerDatumContinuous
					&& secondRelationKnown
					&& secondAnchorStatus.componentId
						== secondSatelliteStatus.componentId;
				const double firstOffset = -static_cast<double>(
						firstSatelliteStatus.alignmentCycles
						- firstAnchorStatus.alignmentCycles);
				const double secondOffset = -static_cast<double>(
						secondSatelliteStatus.alignmentCycles
						- secondAnchorStatus.alignmentCycles);
				auto physicalMetadata = [&](
					const map<ZhangPhysicalIntegerArc, ZhangExactInteger>& physicalRow,
					string& signature,
					vector<std::pair<string, int>>& versions)
				{
					std::ostringstream stream;
					for (const auto& [arc, coefficient] : physicalRow)
					{
						stream << enum_to_string(arc.code) << ":"
							<< arc.edge.receiver << ":" << arc.edge.satellite.id()
							<< ":A" << arc.version << "=" << coefficient << ";";
						versions.push_back({
							enum_to_string(arc.code) + ":" + arc.edge.receiver + ":"
								+ arc.edge.satellite.id(), arc.version});
					}
					signature = stream.str();
				};
				string firstSignature;
				string secondSignature;
				vector<std::pair<string, int>> firstArcVersions;
				vector<std::pair<string, int>> secondArcVersions;
				physicalMetadata(
					firstPhysicalRow, firstSignature, firstArcVersions);
				physicalMetadata(
					secondPhysicalRow, secondSignature, secondArcVersions);
				const ZhangCanonicalSatelliteRelation canonicalRelation =
					ZhangCanonicalSatelliteRelation::ordered(anchor, satellite);
				const auto firstDatum = observeZhangE18PersistentProductDatum(
					floatState, system, firstCode, canonicalRelation,
					firstAnchorStatus.phaseSegment,
					firstSatelliteStatus.phaseSegment,
					firstAnchorStatus.datumVersion,
					firstSatelliteStatus.datumVersion,
					firstExactTransport);
				const auto secondDatum = observeZhangE18PersistentProductDatum(
					floatState, system, secondCode, canonicalRelation,
					secondAnchorStatus.phaseSegment,
					secondSatelliteStatus.phaseSegment,
					secondAnchorStatus.datumVersion,
					secondSatelliteStatus.datumVersion,
					secondExactTransport);
				if (!firstDatum.valid || !secondDatum.valid)
				{
					return;
				}
				const string gaugeComponent = "CANONICAL:" + canonicalSetIdentity;
				const string topology = canonicalSetIdentity + ":"
					+ canonicalRelation.id();
				const string firstPhaseIdentity =
					anchor.id() + ":" + std::to_string(firstAnchorStatus.phaseSegment)
					+ "->" + satellite.id() + ":"
					+ std::to_string(firstSatelliteStatus.phaseSegment);
				const string secondPhaseIdentity =
					anchor.id() + ":" + std::to_string(secondAnchorStatus.phaseSegment)
					+ "->" + satellite.id() + ":"
					+ std::to_string(secondSatelliteStatus.phaseSegment);
				recordZhangE18IntegerDatumTarget(
					trace, floatState, floatState, system,
					"K1_" + enum_to_string(firstCode), anchor, satellite,
					firstFullStateRow, firstOffset, firstExactTransport,
					firstDatum.canonicalCoordinateId,
					firstDatum.productDatumId,
					firstDatum.version,
					topology, gaugeComponent, firstPhaseIdentity,
					firstSignature, firstArcVersions, time);
				recordZhangE18IntegerDatumTarget(
					trace, floatState, floatState, system,
					"K2_" + enum_to_string(secondCode), anchor, satellite,
					secondFullStateRow, secondOffset, secondExactTransport,
					secondDatum.canonicalCoordinateId,
					secondDatum.productDatumId,
					secondDatum.version,
					topology, gaugeComponent, secondPhaseIdentity,
					secondSignature, secondArcVersions, time);
			};

			// Retain the product functional even before z_T is identified.  In
			// that case the recorder reports an explicit one-dimensional integer
			// gauge deficiency instead of silently replacing z_T+Gk by Gk.
			vector<ZhangCanonicalSatelliteRelation> bootstrapRelations;
			for (int localRow = 0;
				 localRow < static_cast<int>(independentRows.size())
				 && localRow < acsConfig.zhangPppAr.max_topology_targets;
				 localRow++)
			{
				bootstrapRelations.push_back(
					ZhangCanonicalSatelliteRelation::ordered(
					target.referenceSatellite,
					target.targetSatellites[independentRows[localRow]]));
			}
			set<SatSys> availableCanonicalSatellites;
			for (const auto& [satellite, ignored] : namedTargetRows)
			{
				availableCanonicalSatellites.insert(satellite);
			}
			const auto canonicalSelection =
				selectZhangE18CanonicalProductRelations(
					floatState, system, bootstrapRelations,
					availableCanonicalSatellites,
					acsConfig.zhangPppAr.max_topology_targets);
			auto encodeRelations = [](const auto& relations)
			{
				std::ostringstream encoded;
				for (const auto& relation : relations)
				{
					if (encoded.tellp() > 0)
					{
						encoded << ";";
					}
					encoded << relation.id();
				}
				return encoded.str().empty() ? string("NONE") : encoded.str();
			};
			trace << "\nZHANG_E18_CANONICAL_TARGET_SET time=" << time.to_string(0)
				  << " system=" << enum_to_string(system)
				  << " canonical_set_id=" << canonicalSelection.canonicalSetId
				  << " established=" << canonicalSelection.established
				  << " selected=" << encodeRelations(canonicalSelection.selected)
				  << " missing=" << encodeRelations(canonicalSelection.missing)
				  << " ignored_substitutes="
				  << encodeRelations(canonicalSelection.ignoredSubstitutes)
				  << " silent_substitution_rejected="
				  << canonicalSelection.silentSubstitutionRejected
				  << " feedback=0";
			for (const auto& relation : canonicalSelection.selected)
			{
				captureAlignedIntegerDatum(
					relation.anchor, relation.satellite,
					canonicalSelection.canonicalSetId);
			}

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
            // E15 diagnostics must describe the complete ranked pool before
            // equivalent topology keys are removed and max_topology_targets
            // truncates the search.  Without this record a reliable relink
            // can be hidden below a higher component-gain candidate and the
            // selected-target TRACE alone cannot distinguish that policy
            // failure from a genuinely unfixable candidate pool.
            if (acsConfig.zhangPppAr.output_diagnostics)
            {
                std::set<string> diagnosticTopologyKeys;
                int independentRank = 0;
                int componentBridgeRank = 0;
                for (int rawRank = 0;
                     rawRank < static_cast<int>(candidates.size());
                     rawRank++)
                {
                    const auto& candidate = candidates[rawRank];
                    const bool topologyUnique = diagnosticTopologyKeys.insert(
                        candidate.topologyKey
                    ).second;
                    if (topologyUnique)
                    {
                        independentRank++;
                        if (candidate.type == "COMPONENT_BRIDGE")
                        {
                            componentBridgeRank++;
                        }
                    }
                    // CURRENT_RELINK does not introduce a new persistent
                    // integer relation.  It only reacquires the current
                    // coordinate of an already known relation, so it must not
                    // compete with unknown component bridges for the small
                    // stochastic-topology search cap.
                    const bool selected = topologyUnique &&
                        (candidate.type == "CURRENT_RELINK" ||
                         componentBridgeRank <=
                            acsConfig.zhangPppAr.max_topology_targets);
                    const double firstFloat =
                        candidate.firstRow.dot(ambiguityResolution.aflt);
                    const double secondFloat =
                        candidate.secondRow.dot(ambiguityResolution.aflt);
                    const double wideLaneFloat = firstFloat - secondFloat;
                    trace << "\nZHANG_TOPOLOGY_CANDIDATE_POOL_ENTRY time="
                          << time.to_string(0)
                          << " system=" << enum_to_string(system)
                          << " product_datum_version="
                          << context.productDatumVersion
                          << " raw_rank=" << rawRank + 1
                          << " independent_rank="
                          << (topologyUnique ? independentRank : 0)
                          << " topology_unique=" << topologyUnique
                          << " selected=" << selected
                          << " type=" << candidate.type
                          << " topology_key=" << candidate.topologyKey
                          << " anchor=" << candidate.anchor.id()
                          << " satellite=" << candidate.satellite.id()
                          << " component_gain=" << candidate.componentGain
                          << " wl_variance=" << candidate.wideLaneVariance
                          << " conditional_l1_variance="
                          << candidate.conditionalFirstVariance
                          << " wl_float=" << wideLaneFloat
                          << " wl_fractional="
                          << wideLaneFloat - std::round(wideLaneFloat)
                          << " l1_float=" << firstFloat
                          << " l1_fractional="
                          << firstFloat - std::round(firstFloat)
                          << " physical_support=" << candidate.supportCount
                          << " score=" << candidate.score;
                }
            }
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
            const std::size_t independentCandidateCount = candidates.size();
            vector<TopologyTargetCandidate> selectedCandidates;
            selectedCandidates.reserve(candidates.size());
            int selectedCurrentRelinks = 0;
            int selectedComponentBridges = 0;
            for (auto& candidate : candidates)
            {
                if (candidate.type == "CURRENT_RELINK")
                {
                    selectedCandidates.push_back(std::move(candidate));
                    selectedCurrentRelinks++;
                    continue;
                }
                if (selectedComponentBridges <
                    acsConfig.zhangPppAr.max_topology_targets)
                {
                    selectedCandidates.push_back(std::move(candidate));
                    selectedComponentBridges++;
                }
            }
            candidates = std::move(selectedCandidates);
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
                const double firstFloat =
                    candidates[row].firstRow.dot(ambiguityResolution.aflt);
                const double secondFloat =
                    candidates[row].secondRow.dot(ambiguityResolution.aflt);
                const double wideLaneFloat = firstFloat - secondFloat;
                const double firstFractional =
                    firstFloat - std::round(firstFloat);
                const double wideLaneFractional =
                    wideLaneFloat - std::round(wideLaneFloat);
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
                      << " wl_float=" << wideLaneFloat
                      << " wl_fractional=" << wideLaneFractional
                      << " l1_float=" << firstFloat
                      << " l1_fractional=" << firstFractional
                      << " physical_support=" << candidates[row].supportCount
                      << " score=" << candidates[row].score;
                if (candidates[row].type == "CURRENT_RELINK")
                {
                    traceZhangWhitenedWlFixedLag(
                        trace,
                        floatState,
                        ambiguityResolution,
                        system,
                        firstCode,
                        secondCode,
                        candidates[row].anchor,
                        candidates[row].satellite,
                        candidates[row].topologyKey,
                        candidates[row].firstRow - candidates[row].secondRow,
                        candidates[row].physicalWideLaneRow,
                        context,
                        time
                    );
                    traceZhangRelinkJointInformationIncrement(
                        trace,
                        floatState,
                        ambiguityResolution,
                        system,
                        candidates[row].anchor,
                        candidates[row].satellite,
                        candidates[row].topologyKey,
                        candidates[row].firstRow,
                        candidates[row].secondRow,
                        time,
                        context.productDatumVersion
                    );
                    traceZhangRelinkInformationIncrement(
                        trace,
                        floatState,
                        ambiguityResolution,
                        system,
                        candidates[row].anchor,
                        candidates[row].satellite,
                        candidates[row].topologyKey,
                        "WL",
                        candidates[row].firstRow - candidates[row].secondRow,
                        time,
                        context.productDatumVersion
                    );
                    traceZhangRelinkInformationIncrement(
                        trace,
                        floatState,
                        ambiguityResolution,
                        system,
                        candidates[row].anchor,
                        candidates[row].satellite,
                        candidates[row].topologyKey,
                        "L1",
                        candidates[row].firstRow,
                        time,
                        context.productDatumVersion
                    );
                }
            }
            trace << "\nZHANG_TOPOLOGY_CANDIDATE_POOL time="
                  << time.to_string(0)
                  << " system=" << enum_to_string(system)
                  << " independent_candidates=" << independentCandidateCount
                  << " selected_candidates=" << candidates.size()
                  << " selected_current_relinks=" << selectedCurrentRelinks
                  << " selected_component_bridges="
                  << selectedComponentBridges;

            MatrixXd topologyWideLane = topologyFirst - topologySecond;
            GinAR_mtx wideLaneStage;
            wideLaneStage.aflt = topologyWideLane * ambiguityResolution.aflt;
            wideLaneStage.Paflt = topologyWideLane *
                ambiguityResolution.Paflt * topologyWideLane.transpose();
            vector<bool> currentRelink(candidates.size(), false);
            for (int row = 0; row < static_cast<int>(candidates.size()); row++)
            {
                currentRelink[row] = candidates[row].type == "CURRENT_RELINK";
            }
            auto deterministicWideLane = recoverDeterministicRelinkTargets(
                trace,
                wideLaneStage.aflt,
                wideLaneStage.Paflt,
                currentRelink,
                time,
                "TOPOLOGY_WIDE_LANE"
            );
            GinAR_opt topologyOptions = options;
            // The missing product-topology dimension is often one bridge,
            // while the general LAMBDA implementation intentionally rejects
            // searches below three dimensions.  ROUND retains the configured
            // distance and integer-error-probability acceptance thresholds
            // and is therefore the appropriate low-dimensional gate here.
            topologyOptions.mode = E_ARmode::ROUND;
            int stochasticWideLaneFixed = rankAwareGnssAr(
                trace,
                wideLaneStage,
                topologyOptions,
                time,
                "TOPOLOGY_WIDE_LANE"
            );
            auto namedWideLane = recoverNamedTargets(
                wideLaneStage, candidates.size()
            );
            namedWideLane.insert(
                deterministicWideLane.begin(), deterministicWideLane.end()
            );
            int fixedWideLane = namedWideLane.size();
            if (fixedWideLane <= 0)
            {
                trace << "\nZHANG_TOPOLOGY_TARGET_RESULT time="
                      << time.to_string(0)
                      << " system=" << enum_to_string(system)
                      << " candidates=" << candidates.size()
                      << " fixed_wl=0 fixed_l1=0 status=WL_NOT_FIXED";
                return 0;
            }
            bool topologyWideLaneCommitted = stochasticWideLaneFixed <= 0 ||
                appendAndApply(
                    wideLaneStage.Ztrs * topologyWideLane,
                    wideLaneStage.zfix,
                    "TOPOLOGY_WIDE_LANE"
                );
            if (!topologyWideLaneCommitted)
            {
                return 0;
            }
            refreshFloatState();

            GinAR_mtx firstStage;
            firstStage.aflt = topologyFirst * ambiguityResolution.aflt;
            firstStage.Paflt = topologyFirst *
                ambiguityResolution.Paflt * topologyFirst.transpose();
            auto deterministicFirst = recoverDeterministicRelinkTargets(
                trace,
                firstStage.aflt,
                firstStage.Paflt,
                currentRelink,
                time,
                "TOPOLOGY_FIRST_SIGNAL"
            );
            int stochasticFirstFixed = rankAwareGnssAr(
                trace,
                firstStage,
                topologyOptions,
                time,
                "TOPOLOGY_FIRST_SIGNAL"
            );
            auto namedFirst = recoverNamedTargets(
                firstStage, candidates.size()
            );
            namedFirst.insert(
                deterministicFirst.begin(), deterministicFirst.end()
            );
            int fixedFirst = namedFirst.size();
            if (stochasticFirstFixed > 0)
            {
                bool topologyFirstCommitted = appendAndApply(
                    firstStage.Ztrs * topologyFirst,
                    firstStage.zfix,
                    "TOPOLOGY_FIRST_SIGNAL"
                );
                if (!topologyFirstCommitted)
                {
                    fixedFirst = 0;
                    namedFirst.clear();
                }
                else
                {
                    refreshFloatState();
                }
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

        GinAR_opt productOptions = options;
        if (acsConfig.zhangPppAr.product_target_named_rounding)
        {
            // A persistent satellite product needs named target relations.
            // ROUND returns an identity subset of the original G_sat targets;
            // opaque partial LAMBDA combinations may be valid integers but
            // cannot necessarily be promoted into per-satellite datum edges.
            productOptions.mode = E_ARmode::ROUND;
        }

        MatrixXd wideLaneTransform = firstTransform - secondTransform;
        GinAR_mtx wideLane;
        wideLane.aflt = wideLaneTransform * ambiguityResolution.aflt;
        wideLane.Paflt =
            wideLaneTransform * ambiguityResolution.Paflt *
            wideLaneTransform.transpose();
        VectorXd namedWideLaneFloat = wideLane.aflt;
        MatrixXd namedWideLaneCovariance = wideLane.Paflt;
        int wideLaneFixed = rankAwareGnssAr(
            trace, wideLane, productOptions, time, "PRODUCT_WIDE_LANE"
        );
        if (wideLaneFixed > 0 &&
            acsConfig.zhangPppAr.product_target_named_rounding)
        {
            wideLane.aflt = namedWideLaneFloat;
            wideLane.Paflt = namedWideLaneCovariance;
            wideLaneFixed = retainNisCompatibleNamedRows(
                trace, wideLane, time, "PRODUCT_WIDE_LANE"
            );
        }
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

        bool wideLaneCommitted = appendAndApply(
            wideLane.Ztrs * wideLaneTransform,
            wideLane.zfix,
            "PRODUCT_WIDE_LANE"
        );
        if (!wideLaneCommitted)
        {
            resolveTopologyTargets();
            continue;
        }
        refreshFloatState();

        GinAR_mtx firstSignal;
        firstSignal.aflt = firstTransform * ambiguityResolution.aflt;
        firstSignal.Paflt =
            firstTransform * ambiguityResolution.Paflt *
            firstTransform.transpose();
        VectorXd namedFirstFloat = firstSignal.aflt;
        MatrixXd namedFirstCovariance = firstSignal.Paflt;
        int firstFixed = rankAwareGnssAr(
            trace, firstSignal, productOptions, time, "PRODUCT_FIRST_SIGNAL"
        );
        if (firstFixed > 0 &&
            acsConfig.zhangPppAr.product_target_named_rounding)
        {
            firstSignal.aflt = namedFirstFloat;
            firstSignal.Paflt = namedFirstCovariance;
            firstFixed = retainNisCompatibleNamedRows(
                trace, firstSignal, time, "PRODUCT_FIRST_SIGNAL"
            );
        }
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
            bool firstSignalCommitted = appendAndApply(
                firstSignal.Ztrs * firstTransform,
                firstSignal.zfix,
                "PRODUCT_FIRST_SIGNAL"
            );
            if (!firstSignalCommitted)
            {
                firstFixed = 0;
                fixedFirstTargets.clear();
            }
            else
            {
                refreshFloatState();
            }
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
    trace << "\nZHANG_AR_RUNTIME_CONFIG time="
          << kfState.time.to_string(0)
          << " mode=" << enum_to_string(acsConfig.ambrOpts.mode)
          << " once_per_epoch=" << acsConfig.ambrOpts.once_per_epoch
          << " output_products=" << acsConfig.zhangPppAr.output_products
          << " output_diagnostics="
          << acsConfig.zhangPppAr.output_diagnostics
          << " product_mode=" << acsConfig.zhangPppAr.product_mode;
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

    // The authoritative Zhang filter is the float estimator.  Every held or
    // newly fixed equality is applied only to this same-epoch disposable copy.
    KFState& floatState = kfState;
    KFState fixedBranch = kfState;
    bool transactional =
        acsConfig.zhangFullRank.enable &&
        acsConfig.zhangPppAr.transactional_integer_fixing;
    KFState* workingState = transactional ? &fixedBranch : &kfState;
    ZhangCheckpointKfCore floatAuthorityBefore;
    if (transactional)
    {
        floatAuthorityBefore = captureZhangCheckpointKfCore(kfState);
    }
    auto traceFloatAuthorityClosure = [&]()
    {
        if (!transactional)
        {
            return;
        }
        const auto after = captureZhangCheckpointKfCore(kfState);
        const bool coreEqual = zhangCheckpointKfCoreBitwiseEqual(
            floatAuthorityBefore, after);
        const double stateDifference =
            floatAuthorityBefore.x.size() == after.x.size()
            ? (after.x.size() == 0 ? 0
                : (floatAuthorityBefore.x - after.x)
                    .cwiseAbs().maxCoeff())
            : std::numeric_limits<double>::infinity();
        const double covarianceDifference =
            floatAuthorityBefore.P.rows() == after.P.rows()
                && floatAuthorityBefore.P.cols() == after.P.cols()
            ? (after.P.size() == 0 ? 0
                : (floatAuthorityBefore.P - after.P)
                    .cwiseAbs().maxCoeff())
            : std::numeric_limits<double>::infinity();
        trace << "\nZHANG_FLOAT_AUTHORITY_CLOSURE time="
              << kfState.time.to_string(0)
              << " before_core_sha256="
              << zhangCheckpointSha256(
                    serializeZhangCheckpointSectionPayload(
                        floatAuthorityBefore))
              << " after_core_sha256="
              << zhangCheckpointSha256(
                    serializeZhangCheckpointSectionPayload(after))
              << " state_maximum_difference=" << stateDifference
              << " covariance_maximum_difference="
              << covarianceDifference
              << " core_bitwise_equal=" << coreEqual
              << " status=" << (coreEqual ? "PASS" : "FAIL")
              << " fixed_branch=DISPOSABLE feedback=0";
    };
    zhangTransactionalConditioningFailed = false;
    zhangTransactionalConditioningReason.clear();
    if (transactional)
    {
        if (!bindZhangAmbresEphemeralBranch(
                fixedBranch, kfState, "fixed-transaction"))
        {
            zhangTransactionalConditioningFailed = true;
            zhangTransactionalConditioningReason =
                "CHECKPOINT_RUNTIME_ID_UNBOUND";
        }
        cloneZhangGraphRuntime(kfState, fixedBranch);
        trace << "\nZHANG_FLOAT_BRANCH_ISOLATION time="
              << kfState.time.to_string(0)
              << " authoritative_state=FLOAT"
              << " fixed_branch=DISPOSABLE";
    }

    if (acsConfig.zhangFullRank.enable)
    {
        auto heldSets = projectPersistentHeldRows(trace, kfState);
        for (auto& heldSet : heldSets)
        {
            auto& held = heldSet.constraints;
            const int originalRows = held.zfix.size();
            trace << "\nZHANG_HELD_LATTICE_REAPPLY time="
                  << kfState.time.to_string(0)
                  << " rows=" << originalRows
                  << " columns=" << held.ambmap.size();
            if (transactional)
            {
                vector<int> stateIndices;
                stateIndices.reserve(held.ambmap.size());
                for (int column = 0;
                     column < static_cast<int>(held.ambmap.size());
                     column++)
                {
                    auto key = held.ambmap.find(column);
                    auto state = key == held.ambmap.end()
                        ? workingState->kfIndexMap.end()
                        : workingState->kfIndexMap.find(key->second);
                    if (key == held.ambmap.end() ||
                        state == workingState->kfIndexMap.end())
                    {
                        zhangTransactionalConditioningFailed = true;
                        zhangTransactionalConditioningReason =
                            "HELD_AMBIGUITY_STATE_MISSING";
                        break;
                    }
                    stateIndices.push_back(state->second);
                }
                if (zhangTransactionalConditioningFailed)
                {
                    break;
                }

                held.aflt = workingState->x(stateIndices);
                held.Paflt = workingState->P(stateIndices, stateIndices);
                vector<int> selectedRows;
                int compatibleRows = retainNisCompatibleNamedRows(
                    trace,
                    held,
                    workingState->time,
                    "PERSISTENT_HELD_BLOCK",
                    &selectedRows,
                    false
                );

                set<int> selected(selectedRows.begin(), selectedRows.end());
                map<E_ObsCode, set<SatSys>> rejectedSupport;
                auto rejectOriginalRow = [&](int row)
                {
                    if (row < 0 ||
                        row >= static_cast<int>(heldSet.rowProductSupport.size()))
                    {
                        return;
                    }
                    for (const auto& [code, satellites] :
                         heldSet.rowProductSupport[row])
                    {
                        rejectedSupport[code].insert(
                            satellites.begin(), satellites.end()
                        );
                    }
                };
                for (int row = 0; row < originalRows; row++)
                {
                    if (selected.find(row) != selected.end())
                    {
                        continue;
                    }
                    rejectOriginalRow(row);
                }

                int numericFallbackRows = 0;
                bool heldApplied = false;
                while (compatibleRows > 0)
                {
                    zhangTransactionalConditioningFailed = false;
                    zhangTransactionalConditioningReason.clear();
                    heldApplied = conditionZhangAmbiguitiesExactly(
                        trace, *workingState, held, "PERSISTENT_HELD_SUBSET"
                    );
                    if (heldApplied)
                    {
                        break;
                    }
                    bool subsetRetryable =
                        zhangTransactionalConditioningReason ==
                            "CONDITIONED_STATE_NUMERIC_FAILURE" ||
                        zhangTransactionalConditioningReason ==
                            "CONSTRAINT_NIS_REJECTED" ||
                        zhangTransactionalConditioningReason ==
                            "REDUNDANT_CONSTRAINT_INCONSISTENT";
                    if (!subsetRetryable || selectedRows.empty())
                    {
                        break;
                    }
                    rejectOriginalRow(selectedRows.back());
                    selectedRows.pop_back();
                    compatibleRows--;
                    numericFallbackRows++;
                    held.Ztrs.conservativeResize(
                        compatibleRows, held.Ztrs.cols()
                    );
                    held.zfix.conservativeResize(compatibleRows);
                    trace << "\nZHANG_HELD_BLOCK_NUMERIC_FALLBACK time="
                          << workingState->time.to_string(0)
                          << " system=" << enum_to_string(heldSet.system)
                          << " remaining_rows=" << compatibleRows
                          << " removed_rows=" << numericFallbackRows
                          << " previous_reason="
                          << zhangTransactionalConditioningReason;
                }
                if (compatibleRows == 0)
                {
                    zhangTransactionalConditioningFailed = false;
                    zhangTransactionalConditioningReason.clear();
                }

                std::size_t quarantined = 0;
                for (const auto& [code, satellites] : rejectedSupport)
                {
                    SatSys trustedAnchor;
                    for (const auto& satellite : satellites)
                    {
                        if (zhangSatelliteAlignmentState(
                                heldSet.system, code, satellite
                            ) == ZhangCurrentAlignmentState::
                                CURRENT_ALIGNMENT_VALID)
                        {
                            trustedAnchor = satellite;
                            break;
                        }
                    }
                    quarantined += quarantineZhangSatelliteProductAlignments(
                        workingState->time,
                        heldSet.system,
                        code,
                        satellites,
                        trustedAnchor,
                        "PERSISTENT_HELD_BLOCK_NIS_REJECTED"
                    );
                }
                trace << "\nZHANG_HELD_BLOCK_ISOLATION time="
                      << workingState->time.to_string(0)
                      << " system=" << enum_to_string(heldSet.system)
                      << " candidates=" << originalRows
                      << " selected=" << compatibleRows
                      << " rejected=" << originalRows - compatibleRows
                      << " numeric_fallback_rows=" << numericFallbackRows
                      << " affected_observables=" << rejectedSupport.size()
                      << " quarantined_satellites=" << quarantined
                      << " joint_selection=1"
                      << " family_alpha="
                      << acsConfig.zhangPppAr.held_constraint_nis_alpha;

                if (heldApplied)
                {
                    conditionZhangL1MeasurementReplayPosteriors(
                        trace,
                        kfState,
                        held.ambmap,
                        held.Ztrs,
                        held.zfix,
                        "PERSISTENT_HELD_SUBSET"
                    );
                }

            }
            else
            {
                applyUCAmbiguities(trace, *workingState, held);
            }
            if (zhangTransactionalConditioningFailed)
            {
                break;
            }
        }
    }

    vector<ZhangPendingProductTransition> temporalProductTransitions;
    if (acsConfig.zhangPppAr.temporal_product_transition_shadow)
    {
		temporalProductTransitions =
			activateZhangTemporalProductTransitions(trace, kfState);
    }

    if (ind == 0)
    {
		if (!temporalProductTransitions.empty())
		{
			trace << "\nZHANG_TEMPORAL_PRODUCT_TRANSITION_SUMMARY time="
			      << kfState.time.to_string(0)
			      << " input_transitions="
			      << temporalProductTransitions.size()
			      << " status=SKIPPED_NO_AMBIGUITY_COORDINATES"
			      << " feedback=0";
		}
        auto floatInvariants = phaseClockOsbClockBiasInvariants(floatState);
        tracePhaseClockOsbProductClosures(trace, *workingState, &floatInvariants);
        traceZhangAmbiguityAndFixedProducts(trace, *workingState, ARmtx, 0);
        traceZhangSatelliteIntegerLattice(trace, *workingState, ARmtx);
        writeZhangInternalProducts(
            trace,
            kfState,
            floatState,
            nullptr,
            workingState,
            nullptr,
            0,
            false,
            false,
            !zhangTransactionalConditioningFailed,
            false,
            nullptr
        );
        if (transactional)
        {
            eraseZhangGraphRuntime(fixedBranch);
        }
        traceFloatAuthorityClosure();
        return;
    }

    ARmtx.aflt  = workingState->x(indices);
    ARmtx.Paflt = workingState->P(indices, indices);
	if (!temporalProductTransitions.empty())
	{
		const auto temporalCertificates = evaluateTemporalProductRelations(
			trace,
			kfState,
			*workingState,
			ARmtx,
			std::move(temporalProductTransitions));
		processTemporalProductRelationAdmissions(
			trace, kfState, *workingState, temporalCertificates);
	}

    vector<double> floatAmbiguities(ARmtx.aflt.data(), ARmtx.aflt.data() + ARmtx.aflt.size());
    tracePhaseClockOsbAmbiguityClosure(trace, floatAmbiguities);
    auto floatInvariants = phaseClockOsbClockBiasInvariants(floatState);

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
    bool networkIntegerReady = false;
    KFState wideLaneState;
    bool wideLaneStateValid = false;
	KFState productFixedState;
	bool productFixedStateValid = false;
	ZhangProductRelationFixResult productRelationFix;
    if (zhangTransactionalConditioningFailed)
    {
        nfix = 0;
    }
    else if (acsConfig.zhangPppAr.user_adapter &&
		(acsConfig.zhangPppAr.integer_strategy == "CANONICAL_USER_SD_WL_L1" ||
		 acsConfig.zhangPppAr.integer_strategy == "CANONICAL_USER_IF_WL_L1"))
	{
		nfix = resolveCanonicalUserSdWideLaneL1(
			trace,
			*workingState,
			ARmtx,
			ARopt,
			workingState->time);
		fixedRowsAlreadyApplied = true;
	}
    else if (acsConfig.zhangFullRank.enable &&
        acsConfig.zhangPppAr.integer_strategy == "INDEPENDENT_SIGNAL")
    {
        nfix = resolveIndependentSignalAmbiguities(
            trace,
            ARmtx,
            ARopt,
            workingState->time,
            &networkIntegerReady
        );
    }
    else if (acsConfig.zhangFullRank.enable &&
             (acsConfig.zhangPppAr.integer_strategy == "LAYERED_WL_L1" ||
			  acsConfig.zhangPppAr.integer_strategy == "HYBRID_PRODUCT_WL_L1"))
    {
		if (acsConfig.zhangPppAr.product_mode == "HOU_OSB_LIKE")
		{
			trace << "\nZHANG_HOU_OSB_LIKE_NETWORK_AR time="
				<< workingState->time.to_string(0)
				<< " lattice=FUNDAMENTAL_CYCLE"
				<< " stages=WL_THEN_L1"
				<< " product_projection=DIRECT_FIXED_PHASE_STATE"
				<< " absolute_satellite_integer_required=0";
		}
			nfix = resolveLayeredWideLaneL1(
            trace,
            *workingState,
            ARmtx,
            ARopt,
            workingState->time,
            &networkIntegerReady,
            &wideLaneState,
            &wideLaneStateValid,
			acsConfig.zhangPppAr.integer_strategy == "HYBRID_PRODUCT_WL_L1"
				? &productRelationFix : nullptr,
			acsConfig.zhangPppAr.integer_strategy == "HYBRID_PRODUCT_WL_L1"
				? zhangAmbresRuntimeId(kfState) : std::string{}
        );
        fixedRowsAlreadyApplied = true;
		if (acsConfig.zhangPppAr.integer_strategy ==
				"HYBRID_PRODUCT_WL_L1")
		{
			GinAR_mtx productConstraints;
			const int currentProductConstraintRank =
				productRelationFix.constraints.conditioningRank;
			int ledgerProjectedRank = 0;
			int ledgerSelectedRank = 0;
			int ledgerRejectedRows = 0;
			double ledgerCombinedNis = std::numeric_limits<double>::quiet_NaN();
			double ledgerCombinedNisThreshold =
				std::numeric_limits<double>::quiet_NaN();
			vector<ProductIntegerLedgerRow> selectedLedgerPairRows;
			string ledgerAdmissionReason = "NOT_EVALUATED";
			const bool physicalIdentityAnnotated = wideLaneStateValid &&
				zhangAnnotateProductConstraintPhysicalIdentities(
					kfState, ARmtx.ambmap, productRelationFix.constraints);
			if (!physicalIdentityAnnotated &&
				productRelationFix.constraints.reliable)
			{
				productRelationFix.constraints.failureReason =
					"PRODUCT_PHYSICAL_IDENTITY_ANNOTATION_FAILED";
			}
			if (physicalIdentityAnnotated)
			{
				traceZhangProductLatticeCertifiedPairs(
					trace, workingState->time,
					productRelationFix.constraints);
			}
			bool constraintsMapped = physicalIdentityAnnotated &&
				zhangProductConstraintsWithLedgerAsGinAr(
					trace,
					workingState->time,
					productRelationFix.constraints,
					zhangAmbresRuntimeId(kfState),
					kfState,
					wideLaneState,
					ARmtx.ambmap,
					productConstraints,
					ledgerProjectedRank,
					ledgerSelectedRank,
					ledgerRejectedRows,
					ledgerCombinedNis,
					ledgerCombinedNisThreshold,
					selectedLedgerPairRows,
					ledgerAdmissionReason);
			if (constraintsMapped &&
				!zhangMergeSelectedLedgerPairCertificates(
					trace, workingState->time, selectedLedgerPairRows,
					productRelationFix.constraints, ledgerAdmissionReason))
			{
				constraintsMapped = false;
			}
			if (constraintsMapped)
			{
				productRelationFix.constraints.conditioningRank =
					productConstraints.Ztrs.rows();
			}
			// Ledger rows may expand the exact dual-frequency certified graph
			// only after they survive the private-branch joint-NIS admission.
			// Keep the result-level authorization bit synchronized with that
			// post-admission graph rather than the pre-ledger solver snapshot.
			productRelationFix.certifiedForProduct = constraintsMapped &&
				productRelationFix.constraints.reliable &&
				productRelationFix.constraints.certifiedPairRank > 0;
			trace << "\nZHANG_PRODUCT_LATTICE_POST_LEDGER_GRAPH time="
				  << workingState->time.to_string(0)
				  << " current_conditioning_rank="
				  << currentProductConstraintRank
				  << " applied_conditioning_rank="
				  << (constraintsMapped ? productConstraints.Ztrs.rows() : 0)
				  << " ledger_selected_pair_rows="
				  << selectedLedgerPairRows.size()
				  << " certified_pair_rank="
				  << productRelationFix.constraints.certifiedPairRank
				  << " reliable=" << productRelationFix.constraints.reliable
				  << " certified_for_product="
				  << productRelationFix.certifiedForProduct
				  << " status="
				  << (constraintsMapped ? "ADMITTED" : ledgerAdmissionReason)
				  << " feedback="
				  << (constraintsMapped
					? "PRIVATE_PRODUCT_BRANCH" : "NONE");
			if (constraintsMapped)
			{
				productFixedState = wideLaneState;
				bindZhangAmbresEphemeralBranch(
					productFixedState, kfState, "product-fixed");
				cloneZhangGraphRuntime(kfState, productFixedState);
				const bool previousFailure =
					zhangTransactionalConditioningFailed;
				const string previousReason =
					zhangTransactionalConditioningReason;
				zhangTransactionalConditioningFailed = false;
				zhangTransactionalConditioningReason.clear();
				productFixedStateValid = conditionZhangAmbiguitiesExactly(
					trace,
					productFixedState,
					productConstraints,
					"PRODUCT_RELATION_JOINT");
				const string productFailureReason =
					zhangTransactionalConditioningReason;
				zhangTransactionalConditioningFailed = previousFailure;
				zhangTransactionalConditioningReason = previousReason;
				trace << "\nZHANG_PRODUCT_FIXED_BRANCH time="
					  << workingState->time.to_string(0)
					  << " constraint_rank="
					  << productConstraints.Ztrs.rows()
					  << " current_constraint_rank="
					  << currentProductConstraintRank
					  << " ledger_projected_rank=" << ledgerProjectedRank
					  << " ledger_selected_rank=" << ledgerSelectedRank
					  << " ledger_rejected_rows=" << ledgerRejectedRows
					  << " ledger_selected_pair_rows="
					  << selectedLedgerPairRows.size()
					  << " pair_certificate_rank="
					  << productRelationFix.constraints.certifiedPairRank
					  << " current_joint_nis="
					  << productRelationFix.constraints.jointNis
					  << " current_joint_nis_threshold="
					  << productRelationFix.constraints.jointNisThreshold
					  << " combined_joint_nis=" << ledgerCombinedNis
					  << " combined_joint_nis_threshold="
					  << ledgerCombinedNisThreshold
					  << " status="
					  << (productFixedStateValid ? "CONDITIONED" : "REJECTED")
					  << " reason="
					  << (productFixedStateValid ? "NONE" : productFailureReason)
					  << " ledger_admission_reason=" << ledgerAdmissionReason
					  << " authoritative_float_unchanged=1"
					  << " feedback=PRIVATE_PRODUCT_BRANCH";
			}
			else
			{
				trace << "\nZHANG_PRODUCT_FIXED_BRANCH time="
					  << workingState->time.to_string(0)
					  << " constraint_rank=0 pair_certificate_rank=0"
					  << " status=SKIPPED"
					  << " reason="
					  << (physicalIdentityAnnotated
						? ledgerAdmissionReason
						: productRelationFix.constraints.failureReason)
					  << " authoritative_float_unchanged=1"
					  << " feedback=NONE";
			}
		}
    }
    else if (acsConfig.zhangFullRank.enable &&
             acsConfig.zhangPppAr.integer_strategy == "PRODUCT_TARGET_WL_L1")
    {
        nfix = resolveProductTargetWideLaneL1(
            trace,
            *workingState,
            floatState,
            ARmtx,
            ARopt,
            workingState->time
        );
        traceZhangE18RawIntegerDatumWindow(
            trace, floatState, workingState->time
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
        if (acsConfig.zhangFullRank.enable == false &&
            acsConfig.phaseClockOsb.enable == false &&
            acsConfig.zhangPppAr.user_adapter == false)
        {
            trace << "\nGINAN_NATIVE_AR_SUMMARY time="
                  << workingState->time.to_string(0)
                  << " mode=" << enum_to_string(ARopt.mode)
                  << " raw_ambiguities=" << ARmtx.aflt.size()
                  << " integer_dimension=" << integerResolution.aflt.size()
                  << " initial_fix_count="
                  << integerResolution.lambda_initial_fix_count
                  << " fixed_count=" << nfix
                  << " candidate_nis="
                  << integerResolution.lambda_candidate_nis
                  << " candidate_nis_threshold="
                  << integerResolution.lambda_candidate_nis_threshold
                  << " candidate_nis_valid="
                  << integerResolution.lambda_candidate_nis_valid
                  << " receiver_ambiguity_pivot="
                  << acsConfig.receiver_amb_pivot[E_Sys::GPS]
                  << " feedback="
                  << (acsConfig.ambrOpts.fix_and_hold ? "HOLD" : "EPOCH_ONLY");
        }
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
            applyUCAmbiguities(trace, *workingState, ARmtx);
        }
        if (acsConfig.zhangFullRank.enable &&
            !zhangTransactionalConditioningFailed)
        {
            appendPersistentHeldRows(trace, kfState, *workingState, ARmtx);
        }
    }

    bool fixedBranchValid = !zhangTransactionalConditioningFailed;
    if (!fixedBranchValid)
    {
        trace << "\nZHANG_FIXED_TRANSACTION_ABORT time="
              << kfState.time.to_string(0)
              << " reason=" << zhangTransactionalConditioningReason
              << " action=DISCARD_FIXED_BRANCH";
        nfix = 0;
        networkIntegerReady = false;
    }

    tracePhaseClockOsbProductClosures(trace, *workingState, &floatInvariants);
    traceZhangAmbiguityAndFixedProducts(trace, *workingState, ARmtx, nfix);
    traceZhangSatelliteIntegerLattice(trace, *workingState, ARmtx);
    auto [heldIntegerRank, heldMinEigenvalue] =
        zhangHeldIntegerRank(*workingState, ARmtx);
    bool integerDatumComplete =
        fixedBranchValid &&
        ARmtx.aflt.size() > 0 &&
        heldIntegerRank == ARmtx.aflt.size();
    writeZhangInternalProducts(
        trace,
        kfState,
        floatState,
        wideLaneStateValid ? &wideLaneState : nullptr,
        workingState,
        productFixedStateValid ? &productFixedState : nullptr,
        nfix,
        integerDatumComplete,
        wideLaneStateValid,
        fixedBranchValid,
        productFixedStateValid
			? productRelationFix.constraints.certifiedPairRank > 0
			: networkIntegerReady,
		productFixedStateValid ? &productRelationFix.constraints : nullptr
    );
	if (acsConfig.zhangPppAr.temporal_product_transition_shadow)
	{
		auto sameEpochTransitions = activateZhangTemporalProductTransitions(
			trace, kfState, true);
		if (!sameEpochTransitions.empty())
		{
			// Product functionals and their persistent snapshots are registered by
			// writeZhangInternalProducts above.  Evaluate only the newly discovered
			// transitions now, while the identical epoch float AR marginal is still
			// available, to provide a genuine t0 shadow sample without feedback.
			const auto temporalCertificates = evaluateTemporalProductRelations(
				trace, kfState, *workingState, ARmtx,
				std::move(sameEpochTransitions));
			processTemporalProductRelationAdmissions(
				trace, kfState, *workingState, temporalCertificates);
		}
	}
    if (transactional)
    {
		if (productFixedStateValid)
		{
			eraseZhangGraphRuntime(productFixedState);
		}
        eraseZhangGraphRuntime(fixedBranch);
    }

    // In Zhang transactional mode the authoritative network state is the
    // float branch.  The legacy fix-and-hold loop below applies scalar integer
    // pseudo-observations directly to that state, which defeats the branch
    // isolation even though all Zhang constraints above were evaluated on the
    // disposable fixed branch.  Keep the legacy behaviour only for the
    // non-transactional ambiguity-resolution path.
    if (transactional)
    {
        trace << "\nZHANG_LEGACY_FIX_AND_HOLD time="
              << kfState.time.to_string(0)
              << " status=SKIPPED_TRANSACTIONAL_FLOAT_AUTHORITY"
              << " authoritative_state=UNCHANGED feedback=0";
    }
    else
    {
        while (0)
        {
            bool applied = applyBestIntegerAmbiguity(trace, kfState);

            if (applied == false)
            {
                break;
            }
        }
    }
    traceFloatAuthorityClosure();
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

// Kept in a source include so the checkpoint adapter can see the deliberately
// private cross-epoch runtime types above without exposing them as public API.
#include "ppp_ambres_checkpoint.inc"
