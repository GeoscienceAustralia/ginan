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
#include <deque>
#include <iostream>
#include <iomanip>
#include <limits>
#include <math.h>
#include <optional>
#include <set>
#include <sstream>
#include <boost/math/distributions/chi_squared.hpp>
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
static bool zhangTransactionalConditioningFailed = false;
static string zhangTransactionalConditioningReason;

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

static map<ZhangRelinkMomentKey, ZhangRelinkPriorMoment>
    zhangRelinkPriorMoments;

/** Capture only scalar physical satellite-relation marginals, rather than a
 * second copy of the full network covariance.  The snapshot is taken after
 * state transition and immediately before the measurement update. */
void captureZhangPppArFloatPrior(const KFState& kfState)
{
    zhangRelinkPriorMoments.clear();
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
            zhangRelinkPriorMoments[{system, anchor, satellite}] = moment;
        }
    }
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

static map<string, ZhangRelinkShadowAccumulator> zhangRelinkShadowAccumulators;

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

static map<string, ZhangRelinkJointAccumulator>
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
    auto prior = zhangRelinkPriorMoments.find({system, anchor, satellite});
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
    if (prior == zhangRelinkPriorMoments.end() ||
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
    auto& accumulator = zhangRelinkJointAccumulators[accumulatorKey];
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
    auto prior = zhangRelinkPriorMoments.find({system, anchor, satellite});
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
    if (prior == zhangRelinkPriorMoments.end())
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
    auto& accumulator = zhangRelinkShadowAccumulators[accumulatorKey];
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

static map<pair<const KFState*, E_Sys>, ZhangPersistentHeldLattice>
    zhangPersistentHeldLattices;

/** Same-epoch integer decisions are provisional.  Admit a physical row to the
 * held lattice only after it has survived the configured multi-epoch product
 * confirmation window unchanged. */
static map<
    pair<const KFState*, E_Sys>,
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

static map<string, ZhangWhitenedWlAccumulator>
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

    auto prior = zhangRelinkPriorMoments.find({system, anchor, satellite});
    if (prior == zhangRelinkPriorMoments.end() ||
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
    auto& accumulator = zhangWhitenedWlAccumulators[accumulatorKey];
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
        auto identity = std::make_pair(&ledgerState, *targetSystem);
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
        auto& lattice = zhangPersistentHeldLattices[{&ledgerState, system}];
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
              << " threshold=" << nisThreshold;
        return false;
    }

    MatrixXd PAt = AP.transpose();
    VectorXd conditionedState =
        kfState.x + PAt * inverseConstraintCovariance * innovation;
    MatrixXd conditionedCovariance =
        kfState.P - PAt * inverseConstraintCovariance * AP;
    conditionedCovariance =
        0.5 * (conditionedCovariance + conditionedCovariance.transpose());
    double closure = (A * conditionedState - mtrx.zfix)
                         .lpNorm<Eigen::Infinity>();
    double diagonalScale = std::max(
        1.0,
        conditionedCovariance.diagonal().cwiseAbs().maxCoeff()
    );
    double minimumDiagonal = conditionedCovariance.diagonal().minCoeff();
    if (!conditionedState.allFinite() || !conditionedCovariance.allFinite() ||
        closure > 1e-7 || minimumDiagonal < -1e-9 * diagonalScale)
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
    if (acsConfig.zhangFullRank.enable &&
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

static int rankAwareGnssAr(
    Trace&           trace,
    GinAR_mtx&       search,
    const GinAR_opt& options,
    GTime            time,
    const string&    label
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
    int fixed = GNSS_AR(trace, search, options);
    if (fixed > 0 && static_cast<int>(selected.size()) < originalSize)
    {
        search.Ztrs = search.Ztrs * selection;
    }
    trace << "\nZHANG_INTEGER_SEARCH_RANK time=" << time.to_string(0)
          << " label=" << label
          << " original_dimension=" << originalSize
          << " stochastic_rank=" << selected.size()
          << " fixed=" << fixed;
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

/** E2: resolve common-arc wide lanes first, apply them, then resolve the L1
 * fundamental-cycle block in the WL-conditioned covariance. */
static int resolveLayeredWideLaneL1(
    Trace&       trace,
    KFState&     kfState,
    GinAR_mtx&   ambiguityResolution,
    const GinAR_opt& options,
    GTime        time,
    bool*        allSystemsPhaseFixed = nullptr
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
            trace, wideLane, options, time, "LAYERED_WIDE_LANE"
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
        int firstFixed = rankAwareGnssAr(
            trace, firstSignal, options, time, "LAYERED_FIRST_SIGNAL"
        );
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
    zhangTransactionalConditioningFailed = false;
    zhangTransactionalConditioningReason.clear();
    if (transactional)
    {
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

    if (ind == 0)
    {
        auto floatInvariants = phaseClockOsbClockBiasInvariants(floatState);
        tracePhaseClockOsbProductClosures(trace, *workingState, &floatInvariants);
        traceZhangAmbiguityAndFixedProducts(trace, *workingState, ARmtx, 0);
        traceZhangSatelliteIntegerLattice(trace, *workingState, ARmtx);
        writeZhangInternalProducts(
            trace,
            floatState,
            *workingState,
            0,
            false,
            !zhangTransactionalConditioningFailed,
            false
        );
        if (transactional)
        {
            eraseZhangGraphRuntime(fixedBranch);
        }
        return;
    }

    ARmtx.aflt  = workingState->x(indices);
    ARmtx.Paflt = workingState->P(indices, indices);

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
    if (zhangTransactionalConditioningFailed)
    {
        nfix = 0;
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
             acsConfig.zhangPppAr.integer_strategy == "LAYERED_WL_L1")
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
            &networkIntegerReady
        );
        fixedRowsAlreadyApplied = true;
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
        floatState,
        *workingState,
        nfix,
        integerDatumComplete,
        fixedBranchValid,
        networkIntegerReady
    );
    if (transactional)
    {
        eraseZhangGraphRuntime(fixedBranch);
    }

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
