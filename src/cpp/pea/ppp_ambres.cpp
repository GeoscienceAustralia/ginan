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
#include <math.h>
#include "ambres/GNSSambres.hpp"
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/biases.hpp"
#include "common/common.hpp"
#include "common/eigenIncluder.hpp"
#include "common/phaseClockOsb.hpp"
#include "common/trace.hpp"

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

    int         ind = 0;
    vector<int> indices;
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

        indices.push_back(index);

        ARmtx.ambmap[ind] = key;
        ind++;
    }

    if (ind == 0)
    {
        auto floatInvariants = phaseClockOsbClockBiasInvariants(kfState);
        tracePhaseClockOsbProductClosures(trace, kfState, &floatInvariants);
        return;
    }

    ARmtx.aflt  = kfState.x(indices);
    ARmtx.Paflt = kfState.P(indices, indices);

    vector<double> floatAmbiguities(ARmtx.aflt.data(), ARmtx.aflt.data() + ARmtx.aflt.size());
    tracePhaseClockOsbAmbiguityClosure(trace, floatAmbiguities);
    auto floatInvariants = phaseClockOsbClockBiasInvariants(kfState);

    GinAR_opt ARopt;
    ARopt.mode   = acsConfig.ambrOpts.mode;
    ARopt.sucthr = acsConfig.ambrOpts.succsThres;
    ARopt.ratthr = acsConfig.ambrOpts.ratioThres;
    ARopt.nset   = acsConfig.ambrOpts.lambda_set;
    ARopt.nitr   = acsConfig.ambrOpts.AR_max_itr;

    if (traceLevel > 4)
        AR_VERBO = true;

    // Resolve and apply ambiguities
    int nfix = GNSS_AR(trace, ARmtx, ARopt);
    if (nfix > 0)
    {
        applyUCAmbiguities(trace, kfState, ARmtx);
    }

    tracePhaseClockOsbProductClosures(trace, kfState, &floatInvariants);

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
