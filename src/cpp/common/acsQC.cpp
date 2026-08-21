// #pragma GCC optimize ("O0")

#include "common/acsQC.hpp"
#include <array>
#include <cmath>
#include <iostream>
#include <vector>
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/common.hpp"
#include "common/enums.h"
#include "common/navigation.hpp"
#include "common/observations.hpp"
#include "common/satStat.hpp"
#include "common/trace.hpp"
#include "rtklib/lambda.h"

#define THRES_MW_JUMP 10.0
#define PDEGAP 60.0
#define PDESLIPTHRESHOLD 0.5
#define PROC_NOISE_IONO 0.001

enum class E_SlipDiagReason
{
    NONE,
    RECEIVER_LLI,
    NO_FREQUENCY_PAIR,
    NO_FREQUENCIES,
    INVALID_LC,
    NO_PREVIOUS_GF,
    NO_PREVIOUS_MW,
    PHASE_JUMP,
    MW_JUMP,
    WITHIN_THRESHOLD,
    FIRST_EPOCH,
    TIME_GAP,
    LOW_ELEVATION,
    DUAL_FREQUENCY,
    TRIPLE_FREQUENCY,
    THIRD_FREQUENCY_REACQUIRED,
    FREQUENCY_REACQUIRED,
    SINGLE_FREQUENCY,
    MISSING_FREQUENCY_VARIANCE,
    INVALID_CODE_VARIANCE,
    INVALID_PHASE_VARIANCE,
    VALID_VARIANCE,
    LOM_WITHIN_THRESHOLD,
    LOM_OUTLIER,
    LAMBDA_FIXED,
    LAMBDA_FLOAT,
    KALMAN_WAITING
};

/** Emit one structured cycle-slip diagnostic trace line.
 *
 * These lines are intended to make detector decisions auditable without changing
 * the detector result.  They are emitted at trace level 2 with stable
 * key=value fields so trace files can be summarised with simple text tools.
 */
void traceSlipEvent(
    Trace&           trace,
    const char*      detector,
    const GObs&      obs,
    const char*      action,
    E_FType          frq1,
    E_FType          frq2,
    E_FType          frq3,
    double           value1,
    double           value2,
    double           threshold,
    E_SlipDiagReason reason
)
{
    tracepdeex(
        2,
        trace,
        "\nPDE-CS-DIAG detector=%s action=%s epoch=%s rec=%s sat=%s f1=%s f2=%s f3=%s "
        "value1=%.6f value2=%.6f threshold=%.6f reason=%s",
        detector,
        action,
        obs.time.to_string(2).c_str(),
        obs.mount.c_str(),
        obs.Sat.id().c_str(),
        enum_to_string(frq1).c_str(),
        enum_to_string(frq2).c_str(),
        enum_to_string(frq3).c_str(),
        value1,
        value2,
        threshold,
        enum_to_lowerstring(reason).c_str()
    );
}

/** Emit a structured diagnostic line for SCDIA internals.
 *
 * `PDE-SCDIA-DIAG` is intentionally separate from `PDE-CS-DIAG` so existing
 * detector-routing summaries remain stable while SCDIA outcomes become
 * machine-readable.
 */
void traceScdiaEvent(
    Trace&           trace,
    const GObs&      obs,
    const char*      action,
    E_FType          frq1,
    E_FType          frq2,
    E_FType          frq3,
    int              nf,
    E_FilterMode     filterMode,
    double           value1,
    double           value2,
    double           threshold,
    double           amb1,
    double           amb2,
    double           amb3,
    E_SlipDiagReason reason
)
{
    tracepdeex(
        2,
        trace,
        "\nPDE-SCDIA-DIAG action=%s epoch=%s rec=%s sat=%s nf=%d mode=%s f1=%s f2=%s f3=%s "
        "value1=%.6f value2=%.6f threshold=%.6f amb1=%.6f amb2=%.6f amb3=%.6f reason=%s",
        action,
        obs.time.to_string(2).c_str(),
        obs.mount.c_str(),
        obs.Sat.id().c_str(),
        nf,
        enum_to_string(filterMode).c_str(),
        enum_to_string(frq1).c_str(),
        enum_to_string(frq2).c_str(),
        enum_to_string(frq3).c_str(),
        value1,
        value2,
        threshold,
        amb1,
        amb2,
        amb3,
        enum_to_lowerstring(reason).c_str()
    );
}

double lomThreshold(int dof)
{
    const double chisqr_arr[100] = {
        10.8, 13.8, 16.3, 18.5, 20.5, 22.5, 24.3, 26.1, 27.9, 29.6, 31.3, 32.9, 34.5, 36.1, 37.7,
        39.3, 40.8, 42.3, 43.8, 45.3, 46.8, 48.3, 49.7, 51.2, 52.6, 54.1, 55.5, 56.9, 58.3, 59.7,
        61.1, 62.5, 63.9, 65.2, 66.6, 68.0, 69.3, 70.7, 72.1, 73.4, 74.7, 76.0, 77.3, 78.6, 80.0,
        81.3, 82.6, 84.0, 85.4, 86.7, 88.0, 89.3, 90.6, 91.9, 93.3, 94.7, 96.0, 97.4, 98.7, 100,
        101,  102,  103,  104,  105,  107,  108,  109,  110,  112,  113,  114,  115,  116,  118,
        119,  120,  122,  123,  125,  126,  127,  128,  129,  131,  132,  133,  134,  135,  137,
        138,  139,  140,  142,  143,  144,  145,  147,  148,  149
    };

    if (dof <= 0 || dof > 100)
        return 0;

    return chisqr_arr[dof - 1] / dof;
}

struct SlipNoise
{
    double           sigmaCode  = 0;
    double           sigmaPhase = 0;
    E_SlipDiagReason reason     = E_SlipDiagReason::NONE;
};

/** Derive conservative slip-detector noise from the selected observation bands.
 *
 * Slip detection chooses concrete observed frequencies with `obsFreqs()`.  The
 * corresponding noise values must come from those same bands; using an
 * arbitrary first signal can pass zero or invalid variances into PDE/SCDIA and
 * produce NaNs in trace output.
 *
 * This helper requires each selected band to have finite, positive code and
 * phase variance.  It then uses the largest selected variance for each
 * observable type as a conservative single-noise input to the current PDE/SCDIA
 * equations.
 *
 * @param[in]  obs   Observation containing selected signals.
 * @param[in]  freqs Selected frequency bands.
 * @param[in]  nf    Number of selected frequencies to validate.
 * @param[out] noise Derived standard deviations, or failure reason.
 *
 * @return true when all selected bands have usable variances.
 */
bool slipNoise(const GObs& obs, const E_FType freqs[], int nf, SlipNoise& noise)
{
    double maxCodeVar  = 0;
    double maxPhaseVar = 0;

    for (int i = 0; i < nf; i++)
    {
        auto sigIt = obs.sigs.find(freqs[i]);
        if (sigIt == obs.sigs.end())
        {
            noise.reason = E_SlipDiagReason::MISSING_FREQUENCY_VARIANCE;
            return false;
        }

        double codeVar = sigIt->second.codeVar;
        double phasVar = sigIt->second.phasVar;

        if (!std::isfinite(codeVar) || codeVar <= 0)
        {
            noise.reason = E_SlipDiagReason::INVALID_CODE_VARIANCE;
            return false;
        }

        if (!std::isfinite(phasVar) || phasVar <= 0)
        {
            noise.reason = E_SlipDiagReason::INVALID_PHASE_VARIANCE;
            return false;
        }

        if (codeVar > maxCodeVar)
            maxCodeVar = codeVar;

        if (phasVar > maxPhaseVar)
            maxPhaseVar = phasVar;
    }

    noise.sigmaCode  = sqrt(maxCodeVar);
    noise.sigmaPhase = sqrt(maxPhaseVar);
    noise.reason     = E_SlipDiagReason::VALID_VARIANCE;

    return true;
}

/** Detect cycle slip by reported loss of lock
 */
void detslp_ll(
    Trace&   trace,   ///< Trace to output to
    ObsList& obsList  ///< List of observations to detect slips within
)
{
    if (obsList.empty())
    {
        tracepdeex(3, trace, "\n%s: epoch=? n=%zu (empty obsList)", __FUNCTION__, obsList.size());
        return;
    }

    // Find first non-null element for the timestamp
    std::string epoch = "?";
    for (const auto& sp : obsList)
    {
        if (sp)
        {
            epoch = sp->time.to_string(2);
            break;
        }
    }

    tracepdeex(3, trace, "\n%s: epoch=%s n=%zu", __FUNCTION__, epoch.c_str(), obsList.size());

    // 	auto begin_iter = boost::make_filter_iterator([]

    for (auto& obs : only<GObs>(obsList))
        for (auto& [ft, sig] : obs.sigs)
        {
            if (obs.exclude)
            {
                continue;
            }

            // removed unused variable 'f'
            if (sig.L == 0 || (sig.LLI & 0x03) == 0)
            {
                continue;
            }

            traceSlipEvent(
                trace,
                "LLI",
                obs,
                "detected",
                ft,
                NONE,
                NONE,
                1,
                0,
                0,
                E_SlipDiagReason::RECEIVER_LLI
            );

            obs.satStat_ptr->sigStatMap[ft2string(ft)].slip.LLI      = true;
            obs.satStat_ptr->sigStatMap[ft2string(ft)].savedSlip.LLI = true;
        }
}

/** Detect cycle slip by geometry free phase jump
 */
void detslp_gf(
    Trace&   trace,   ///< Trace to output to
    ObsList& obsList  ///< List of observations to detect slips within
)
{
    if (obsList.empty())
    {
        tracepdeex(3, trace, "\n%s: epoch=? n=%zu (empty obsList)", __FUNCTION__, obsList.size());
        return;
    }

    // Find first non-null element for the timestamp
    std::string epoch = "?";
    for (const auto& sp : obsList)
    {
        if (sp)
        {
            epoch = sp->time.to_string(2);
            break;
        }
    }

    tracepdeex(3, trace, "\n%s: epoch=%s n=%zu", __FUNCTION__, epoch.c_str(), obsList.size());

    for (auto& obs : only<GObs>(obsList))
    {
        if (obs.exclude)
        {
            continue;
        }

        E_FType frq1;
        E_FType frq2;
        E_FType frq3;
        int     nf = obsFreqs(obs, frq1, frq2, frq3);
        if (nf < 2)
        {
            traceSlipEvent(
                trace,
                "GF",
                obs,
                "skipped",
                NONE,
                NONE,
                NONE,
                0,
                0,
                0,
                E_SlipDiagReason::NO_FREQUENCY_PAIR
            );
            continue;
        }

        S_LC& lc = getLC(obs.satStat_ptr->lc_new, frq1, frq2);

        double gf1 = lc.GF_Phas_m;
        if (lc.valid == false || gf1 == 0)
        {
            traceSlipEvent(
                trace,
                "GF",
                obs,
                "skipped",
                frq1,
                frq2,
                NONE,
                gf1,
                0,
                0,
                E_SlipDiagReason::INVALID_LC
            );
            continue;
        }

        double gf0          = obs.satStat_ptr->gf;
        obs.satStat_ptr->gf = gf1;

        if (gf0 == 0)
        {
            traceSlipEvent(
                trace,
                "GF",
                obs,
                "initialised",
                frq1,
                frq2,
                NONE,
                gf1,
                gf0,
                0,
                E_SlipDiagReason::NO_PREVIOUS_GF
            );
            continue;
        }

        if (fabs(gf1 - gf0) > acsConfig.preprocOpts.slip_threshold)
        {
            obs.satStat_ptr->sigStatMap[ft2string(frq1)].slip.GF      = true;
            obs.satStat_ptr->sigStatMap[ft2string(frq2)].slip.GF      = true;
            obs.satStat_ptr->sigStatMap[ft2string(frq1)].savedSlip.GF = true;
            obs.satStat_ptr->sigStatMap[ft2string(frq2)].savedSlip.GF = true;
            traceSlipEvent(
                trace,
                "GF",
                obs,
                "detected",
                frq1,
                frq2,
                NONE,
                gf1,
                gf0,
                acsConfig.preprocOpts.slip_threshold,
                E_SlipDiagReason::PHASE_JUMP
            );
        }
        else
        {
            traceSlipEvent(
                trace,
                "GF",
                obs,
                "accepted",
                frq1,
                frq2,
                NONE,
                gf1,
                gf0,
                acsConfig.preprocOpts.slip_threshold,
                E_SlipDiagReason::WITHIN_THRESHOLD
            );
        }
    }
}

/** Detect slip by Melbourne-Wubbena linear combination jump
 */
void detslp_mw(
    Trace&   trace,   ///< Trace to output to
    ObsList& obsList  ///< List of observations to detect slips within
)
{
    if (obsList.empty())
    {
        tracepdeex(3, trace, "\n%s: epoch=? n=%zu (empty obsList)", __FUNCTION__, obsList.size());
        return;
    }

    // Find first non-null element for the timestamp
    std::string epoch = "?";
    for (const auto& sp : obsList)
    {
        if (sp)
        {
            epoch = sp->time.to_string(2);
            break;
        }
    }

    tracepdeex(3, trace, "\n%s: epoch=%s n=%zu", __FUNCTION__, epoch.c_str(), obsList.size());

    for (auto& obs : only<GObs>(obsList))
    {
        if (obs.exclude)
        {
            continue;
        }

        E_FType frq1;
        E_FType frq2;
        E_FType frq3;
        int     nf = obsFreqs(obs, frq1, frq2, frq3);
        if (nf < 2)
        {
            traceSlipEvent(
                trace,
                "MW",
                obs,
                "skipped",
                NONE,
                NONE,
                NONE,
                0,
                0,
                0,
                E_SlipDiagReason::NO_FREQUENCY_PAIR
            );
            continue;
        }

        S_LC& lc = getLC(obs.satStat_ptr->lc_new, frq1, frq2);

        double mw1 = lc.MW_c;
        if (lc.valid == false || mw1 == 0)
        {
            traceSlipEvent(
                trace,
                "MW",
                obs,
                "skipped",
                frq1,
                frq2,
                NONE,
                mw1,
                0,
                0,
                E_SlipDiagReason::INVALID_LC
            );
            continue;
        }

        double mw0          = obs.satStat_ptr->mw;
        obs.satStat_ptr->mw = mw1;

        if (mw0 == 0)
        {
            traceSlipEvent(
                trace,
                "MW",
                obs,
                "initialised",
                frq1,
                frq2,
                NONE,
                mw1,
                mw0,
                0,
                E_SlipDiagReason::NO_PREVIOUS_MW
            );
            continue;
        }

        if (fabs(mw1 - mw0) > THRES_MW_JUMP)
        {
            obs.satStat_ptr->sigStatMap[ft2string(frq1)].slip.MW      = true;
            obs.satStat_ptr->sigStatMap[ft2string(frq2)].slip.MW      = true;
            obs.satStat_ptr->sigStatMap[ft2string(frq1)].savedSlip.MW = true;
            obs.satStat_ptr->sigStatMap[ft2string(frq2)].savedSlip.MW = true;
            traceSlipEvent(
                trace,
                "MW",
                obs,
                "detected",
                frq1,
                frq2,
                NONE,
                mw1,
                mw0,
                THRES_MW_JUMP,
                E_SlipDiagReason::MW_JUMP
            );
        }
        else
        {
            traceSlipEvent(
                trace,
                "MW",
                obs,
                "accepted",
                frq1,
                frq2,
                NONE,
                mw1,
                mw0,
                THRES_MW_JUMP,
                E_SlipDiagReason::WITHIN_THRESHOLD
            );
        }
    }
}

/** Melbourne-Wenbunna (MW) measurement noise (m)
 */
double mwnoise(
    double sigcode,   ///< Code noise
    double sigphase,  ///< Phase noise
    double lam1,      ///< L1 wavelength
    double lam2       ///< L2 wavelength
)
{
    double a =
        lam2 * lam2 / (lam2 + lam1) / (lam2 + lam1) + lam1 * lam1 / (lam2 + lam1) / (lam2 + lam1);
    double b =
        lam2 * lam2 / (lam2 - lam1) / (lam2 - lam1) + lam1 * lam1 / (lam2 - lam1) / (lam2 - lam1);
    return SQRT(a * SQR(sigcode) + b * SQR(sigphase));
}

/** Single channel detection–identification–adaptation (DIA) for integer cycle slips
 */
void scdia(
    Trace&            trace,       ///< Trace to output to
    SatStat&          satStat,     ///< Persistant satellite status parameters
    lc_t&             lc,          ///< Linear combinations
    const GObs&       obs,         ///< Observation context for diagnostics
    map<int, double>& lam,         ///< Signal wavelength map
    double            sigmaPhase,  ///< Phase noise
    double            sigmaCode,   ///< Code noise
    int               nf,          ///< Number of frequencies
    E_FilterMode      filterMode,  ///< LSQ/Kalman filter flag
    E_FType           frq1,
    E_FType           frq2,
    E_FType           frq3
)
{
    if (nf == 0)
        return;

    lc_t* lc_pre_ptr;

    if (filterMode == E_FilterMode::LSQ)
        lc_pre_ptr = &satStat.lc_pre;
    else
        lc_pre_ptr = &satStat.flt.lc_pre;
    if (nf == 1)
        lc_pre_ptr = &satStat.flt.lc_pre;

    auto& lc_pre = *lc_pre_ptr;

    /* single frequency not supported in current PDE */
    if (nf == 1)
    {
        return;
    }

    E_FType freqs[3] = {frq1, frq2, frq3};

    /* m-rows measurements, n-cols unknowns */
    int      m = 2 * nf + 1;
    int      n = 2 + nf;
    VectorXd Z = VectorXd::Zero(m);
    MatrixXd R = MatrixXd::Identity(m, m);
    MatrixXd H = MatrixXd::Zero(m, n);

    double lam1 = lam[frq1];
    int    i    = 0;

    // phase and code
    for (int f = 0; f < nf; f++)
    {
        E_FType frqX = freqs[f];
        double  lamX = lam[frqX];

        Z[i]        = lc.L_m[frqX] - lc_pre.L_m[frqX];
        R(i, i)     = 1 / (2 * SQR(sigmaPhase));
        H(i, 0)     = 1;
        H(i, 1)     = -SQR(lamX) / SQR(lam1);
        H(i, 2 + f) = lamX;
        i++;

        Z[i]    = lc.P[frqX] - lc_pre.P[frqX];
        R(i, i) = 1 / (2 * SQR(sigmaCode));
        H(i, 0) = 1;
        H(i, 1) = +SQR(lamX) / SQR(lam1);
        i++;
    }

    // ionosphere
    {
        Z[i]    = satStat.dIono;
        R(i, i) = 1 / SQR(satStat.sigmaIono);
        H(i, 1) = 1;
        i++;
    }

    /* perform LOM test for outlier detection */
    /* design matrix for LOM test */
    MatrixXd Hlom  = H.leftCols(2);
    VectorXd v     = VectorXd::Zero(m);
    int      ind   = lsqqc(trace, Hlom.data(), R.data(), Z.data(), v.data(), m, 2, 0, 0);
    double   vtpv  = v.dot(R * v);
    int      dof   = m - 2;
    double   val   = dof > 0 ? vtpv / dof : 0;
    double   thres = lomThreshold(dof);
    if (ind == 0)
    {
        traceScdiaEvent(
            trace,
            obs,
            "accepted",
            frq1,
            frq2,
            frq3,
            nf,
            filterMode,
            vtpv,
            val,
            thres,
            0,
            0,
            0,
            E_SlipDiagReason::LOM_WITHIN_THRESHOLD
        );
        return;
    }

    traceScdiaEvent(
        trace,
        obs,
        "detected",
        frq1,
        frq2,
        frq3,
        nf,
        filterMode,
        vtpv,
        val,
        thres,
        0,
        0,
        0,
        E_SlipDiagReason::LOM_OUTLIER
    );

    satStat.sigStatMap[ft2string(frq1)].slip.SCDIA = true;
    satStat.sigStatMap[ft2string(frq2)].slip.SCDIA = true;
    if (nf == 3)
        satStat.sigStatMap[ft2string(frq3)].slip.SCDIA = true;
    satStat.sigStatMap[ft2string(frq1)].savedSlip.SCDIA = true;
    satStat.sigStatMap[ft2string(frq2)].savedSlip.SCDIA = true;
    if (nf == 3)
        satStat.sigStatMap[ft2string(frq3)].savedSlip.SCDIA = true;

    VectorXd xp = VectorXd::Zero(n);
    MatrixXd Pp = MatrixXd::Zero(n, n);

    if (filterMode == E_FilterMode::LSQ)
    {
        MatrixXd N  = MatrixXd::Zero(n, m);
        VectorXd N1 = VectorXd::Zero(n);
        matmul("TN", n, m, m, 1, H.data(), R.data(), 0, N.data());  /* H'*R */
        matmul("NN", n, n, m, 1, N.data(), H.data(), 0, Pp.data()); /* H'*R*H */
        matmul("NN", n, 1, m, 1, N.data(), Z.data(), 0, N1.data()); /* Nl=H'*R*Z */
        if (!matinv(Pp.data(), n))
        {
            matmul("NN", n, 1, n, 1, Pp.data(), N1.data(), 0, xp.data());
        }
        /* store float solution and vc matrix */
        matcpy(satStat.flt.a, xp.data() + 2, 1, nf);

        for (int i = 0; i < nf; i++)
            for (int j = 0; j < nf; j++)
                satStat.flt.Qa[i][j] = Pp.data()[(i + 2) * n + j + 2];
    }
    else
    {
        satStat.flt.ne++;
        if (satStat.flt.ne < 2)
        {
            satStat.flt.slip = 0;
            satStat.flt.ne   = 0;

            traceScdiaEvent(
                trace,
                obs,
                "waiting",
                frq1,
                frq2,
                frq3,
                nf,
                filterMode,
                satStat.flt.ne,
                2,
                0,
                0,
                0,
                0,
                E_SlipDiagReason::KALMAN_WAITING
            );
            return;
        }

        VectorXd x = VectorXd::Zero(n);
        matcpy(x.data() + 2, satStat.flt.a, 1, nf);

        /* time update */
        MatrixXd Px = MatrixXd::Zero(n, n);
        for (int i = 0; i < nf; i++)
            for (int j = 0; j < nf; j++)
                Px.data()[(i + 2) * n + j + 2] = satStat.flt.Qa[i][j];

        Px.data()[0]     = 1E6;
        Px.data()[1 + n] = 1E6;

        /* measurement-prediction */
        matmul("NN", m, 1, n, -1, H.data(), x.data(), 1, Z.data());

        /* transpose of desgin matrix */
        MatrixXd I  = MatrixXd::Identity(m, m);
        MatrixXd H1 = MatrixXd::Zero(n, m);
        matmul("TN", n, m, m, +1, H.data(), I.data(), 0, H1.data());

        /* measurement update */
        if (!matinv(R.data(), m))
            filter_(x.data(), Px.data(), H1.data(), Z.data(), R.data(), n, m, xp.data(), Pp.data());

        matcpy(satStat.flt.a, xp.data() + 2, 1, nf);

        for (int i = 0; i < nf; i++)
            for (int j = 0; j < nf; j++)
                satStat.flt.Qa[i][j] = Pp.data()[(i + 2) * n + j + 2];
    }

    /* ambiguity vector and its variance */
    VectorXd a = VectorXd::Zero(nf);
    matcpy(a.data(), xp.data() + n - nf, nf, 1);

    MatrixXd Qa = MatrixXd::Zero(nf, nf);
    for (int i = 0; i < nf; i++)
        for (int j = 0; j < nf; j++)
        {
            Qa.data()[i * nf + j] = Pp.data()[(n - nf + i) * n + j + n - nf];
        }

    /* integer cycle slip estimation */
    MatrixXd F = MatrixXd::Zero(nf, 2);
    double   s[2];
    bool     pass = false;
    lambda(trace, nf, 2, a.data(), Qa.data(), F.data(), s, acsConfig.predefined_fail, pass);

    double ratio = s[1] != 0 ? s[0] / s[1] : 0;
    traceScdiaEvent(
        trace,
        obs,
        pass ? "fixed" : "float",
        frq1,
        frq2,
        frq3,
        nf,
        filterMode,
        s[0],
        s[1],
        ratio,
        F.data()[0],
        nf > 1 ? F.data()[1] : 0,
        nf > 2 ? F.data()[2] : 0,
        pass ? E_SlipDiagReason::LAMBDA_FIXED : E_SlipDiagReason::LAMBDA_FLOAT
    );

    if (filterMode == E_FilterMode::LSQ)
    {
        /* least-squares */
        satStat.amb[0] = 0;
        satStat.amb[1] = 0;
        satStat.amb[2] = 0;
        tracepdeex(2, trace, "(freq=%d) ", nf);
        if (pass)
        {
            tracepdeex(2, trace, "fixed ");
            for (int i = 0; i < nf; i++)
                satStat.amb[i] = ROUND(F.data()[i]);
        }
    }
    else
    {
        /* kalman filter */
        satStat.flt.amb[0] = 0;
        satStat.flt.amb[1] = 0;
        satStat.flt.amb[2] = 0;
        if (pass)
        {
            memset(satStat.flt.a, 0, 3 * sizeof(double));
            memset(satStat.flt.Qa, 0, 9);  // todo? looks sketchy
            satStat.flt.slip |= 2;
            tracepdeex(1, trace, "     ACC fixed ");
            for (int i = 0; i < nf; i++)
            {
                satStat.flt.amb[i] = ROUND(F.data()[i]);
            }
        }
        tracepdeex(1, trace, "ACC epoch used=%2d\n", satStat.flt.ne);
        if (pass)
            satStat.flt.ne = 0;
    }
}

/** Cycle slip detection for dual-frequency
 */
void cycleslip2(
    Trace&   trace,    ///< Trace to output to
    SatStat& satStat,  ///< Persistant satellite status parameters
    lc_t&    lcBase,   ///< Linear combinations
    GObs&    obs       ///< Navigation object for this satellite
)
{
    string timeStr = lcBase.time.to_string(2);

    auto& recOpts = acsConfig.getRecOpts(obs.mount);

    double dt = (lcBase.time - satStat.lc_pre.time).to_double();

    if (dt < 20 || dt > PDEGAP)
    {
        // small interval or reset

        satStat.dIono = 0;
        // approximation of ionosphere residual

        satStat.sigmaIono = PROC_NOISE_IONO * SQRT(dt);
    }
    else
    {
        // medium interval ~30s

        if (satStat.dIono == 0)
        {
            satStat.sigmaIono = PROC_NOISE_IONO * SQRT(dt);
        }
    }

    if (satStat.sigmaIono == 0)
    {
        satStat.sigmaIono = 0.001;
    }

    E_FType frq1;
    E_FType frq2;
    E_FType frq3;
    int     nf = obsFreqs(obs, frq1, frq2, frq3);
    if (nf < 2)
    {
        return;
    }

    auto& lam = obs.satNav_ptr->lamMap;

    double lam1 = lam[frq1];
    double lam2 = lam[frq2];

    double lamw = lam1 * lam2 / (lam2 - lam1);  // todo? rename

    /* ionosphere coefficient */
    double coef = SQR(lam2) / SQR(lam1) - 1;

    E_FType   freqs[] = {frq1, frq2};
    SlipNoise noise;
    if (!slipNoise(obs, freqs, 2, noise))
    {
        traceSlipEvent(trace, "PDE", obs, "skipped", frq1, frq2, NONE, 0, 0, 0, noise.reason);
        return;
    }

    double sigmaCode  = noise.sigmaCode;
    double sigmaPhase = noise.sigmaPhase;

    double sigmaGF = 2 * sigmaPhase;

    S_LC lcNew = getLC(lcBase, frq1, frq2);
    S_LC lcPre = getLC(satStat.lc_pre, frq1, frq2);

    double mwNoise = mwnoise(sigmaCode, sigmaPhase, lam1, lam2);

    /* averaged MW measurement and noise */
    double fNw;
    if (acsConfig.preprocOpts.mw_proc_noise)
    {
        fNw = lcNew.MW_c - satStat.mwSlip.mean;
    }
    else
    {
        fNw = lcNew.MW_c - lcPre.MW_c;
    } /* Eq (6) in TN */

    /* clock jump */
    if (fabs(fNw * lamw) > 10e-3 * CLIGHT)
    {
        tracepdeex(1, trace, "Potential clock jump rather than cycle slip -cs2\n");
    }

    double deltaGF = lcNew.GF_Phas_m - lcPre.GF_Phas_m; /* Eq (9) in TN */

    tracepdeex(
        2,
        trace,
        "\nPDE-CS GPST DUAL  %s %4s %5.2f %5.3f %8.4f %7.4f %8.4f                           "
        "     ",
        timeStr.c_str(),
        lcBase.Sat.id().c_str(),
        satStat.el * R2D,
        lamw,
        deltaGF,
        fNw,
        sigmaGF
    );

    /* cycle slip detection */
    if (satStat.el >= recOpts.elevation_mask_deg * D2R)
    {
        scdia(
            trace,
            satStat,
            lcBase,
            obs,
            lam,
            sigmaPhase,
            sigmaCode,
            2,
            E_FilterMode::LSQ,
            frq1,
            frq2,
            frq3
        );
    }

    /* update TD ionosphere residual */
    if (satStat.sigStatMap[ft2string(frq1)].slip.any == 0 &&
        satStat.sigStatMap[ft2string(frq2)].slip.any == 0)
    {
        satStat.dIono     = deltaGF / coef;
        satStat.sigmaIono = sigmaGF / coef;
    }
}

/** Cycle slip detection and repair for triple-frequency
 */
void cycleslip3(
    Trace&   trace,    ///< Trace to output to
    SatStat& satStat,  ///< Persistant satellite status parameters
    lc_t&    lc,       ///< Linear combinations
    GObs&    obs       ///< Navigation object for this satellite
)
{
    string timeStr = lc.time.to_string(2);

    auto& recOpts = acsConfig.getRecOpts(obs.mount);

    double dt = (lc.time - satStat.lc_pre.time).to_double();

    /* small interval */
    if (dt < 20)
    {
        satStat.dIono = 0;

        /* approximation of ionosphere residual */
        satStat.sigmaIono = PROC_NOISE_IONO * SQRT(dt);
    }
    else
    {
        /* large interval */
        if (satStat.sigmaIono == 0)
        {
            satStat.sigmaIono = PROC_NOISE_IONO * SQRT(dt);
        }
    }

    if (satStat.sigmaIono == 0)
    {
        satStat.sigmaIono = 0.001;
    }

    E_FType frq1;
    E_FType frq2;
    E_FType frq3;
    int     nf = obsFreqs(obs, frq1, frq2, frq3);
    if (nf < 3)
        return;

    auto& lam = obs.satNav_ptr->lamMap;

    std::array<E_FType, 3> selectedFreqs = {frq1, frq2, frq3};
    std::array<double, 3>  wavelengths   = {lam[frq1], lam[frq2], lam[frq3]};
    std::array<std::pair<int, int>, 3> pairIndexes = {
        std::pair<int, int>{0, 1},
        std::pair<int, int>{0, 2},
        std::pair<int, int>{1, 2}
    };

    /* TD MW noise (m) */
    double lamExtraWide =
        wavelengths[1] * wavelengths[2] / (wavelengths[2] - wavelengths[1]);
    if (lamExtraWide < 0)
        lamExtraWide *= -1;

    SlipNoise noise;
    if (!slipNoise(obs, selectedFreqs.data(), 3, noise))
    {
        traceSlipEvent(trace, "PDE", obs, "skipped", frq1, frq2, frq3, 0, 0, 0, noise.reason);
        return;
    }

    double sigmaCode  = noise.sigmaCode;
    double sigmaPhase = noise.sigmaPhase;

    std::array<double, 3> mwNoises = {};
    std::array<S_LC, 3>   lcNew    = {};
    std::array<S_LC, 3>   lcPre    = {};
    for (int i = 0; i < pairIndexes.size(); i++)
    {
        auto [freqA, freqB] = pairIndexes[i];

        mwNoises[i] = mwnoise(sigmaCode, sigmaPhase, wavelengths[freqA], wavelengths[freqB]);
        lcNew[i]    = getLC(lc, selectedFreqs[freqA], selectedFreqs[freqB]);
        lcPre[i]    = getLC(satStat.lc_pre, selectedFreqs[freqA], selectedFreqs[freqB]);
    }

    double sigmaGF = 2 * sigmaPhase; /* TD GF noise */

    /* averaged EMW measurement and noise */
    double fNew;
    // 	double sigmaEMW;
    if (acsConfig.preprocOpts.mw_proc_noise)
    {
        fNew = lcNew[2].MW_c - satStat.emwSlip.mean;
    }
    else
    {
        fNew = lcNew[2].MW_c - lcPre[2].MW_c;
    } /* Eq (13) in TN */

    double deltaGF23 = lcNew[2].GF_Phas_m - lcPre[2].GF_Phas_m;

    /* clock jump */
    if (fabs(fNew * lamExtraWide) > 10e-3 * CLIGHT)
    {
        fprintf(stdout, "Potential clock jump rather than cycle slip -cs3\n");
        return;
    }

    /* ionosphere coefficient for selected frequencies 2 and 3 */
    double coef1 = SQR(CLIGHT / wavelengths[0]) / SQR(CLIGHT / wavelengths[2]) -
                   SQR(CLIGHT / wavelengths[0]) / SQR(CLIGHT / wavelengths[1]);
    if (coef1 < 0)
        coef1 = -coef1;

    double lamw = wavelengths[0] * wavelengths[1] / (wavelengths[1] - wavelengths[0]);

    double coef = SQR(wavelengths[1]) / SQR(wavelengths[0]) - 1;

    /* averaged MW measurement and noise */
    double fNw;
    if (acsConfig.preprocOpts.mw_proc_noise)
    {
        fNw = lcNew[0].MW_c - satStat.mwSlip.mean;
    }
    else
    {
        fNw = lcNew[0].MW_c - lcPre[0].MW_c;
    } /* Eq (6) in TN */

    double deltaGF = lcNew[0].GF_Phas_m - lcPre[0].GF_Phas_m;

    tracepdeex(
        2,
        trace,
        "\nPDE-CS GPST TRIP  %s %4s %5.2f %5.3f %8.4f %7.4f %8.4f        %6.2f %8.4f %7.4f ",
        timeStr.c_str(),
        lc.Sat.id().c_str(),
        satStat.el * R2D,
        lamw,
        deltaGF,
        fNw,
        sigmaGF,
        lamExtraWide,
        deltaGF23,
        fNew
    );

    if (satStat.el >= recOpts.elevation_mask_deg * D2R)
    {
        scdia(
            trace,
            satStat,
            lc,
            obs,
            lam,
            sigmaPhase,
            sigmaCode,
            3,
            E_FilterMode::LSQ,
            frq1,
            frq2,
            frq3
        );
    }

    /* update TD ionosphere residual */
    if (satStat.sigStatMap[ft2string(frq1)].slip.any == 0 &&
        satStat.sigStatMap[ft2string(frq2)].slip.any == 0 &&
        satStat.sigStatMap[ft2string(frq3)].slip.any == 0)
    {
        satStat.dIono     = deltaGF / coef;
        satStat.sigmaIono = sigmaGF / coef;
    }
}

/** Cycle slip detection and repair
 */
void detectslip(
    Trace&   trace,    ///< Trace to output to
    SatStat& satStat,  ///< Persistant satellite status parameters
    lc_t&    lc_new,   ///< Linear combination for this epoch
    lc_t&    lc_old,   ///< Linear combination from previous epoch
    GObs&    obs       ///< Navigation object for this satellite
)
{
    bool  dualFreq = false;
    E_Sys sys      = lc_new.Sat.sys;

    char id[32];
    lc_new.Sat.getId(id);

    string timeStr = lc_new.time.to_string(2);

    auto& recOpts = acsConfig.getRecOpts(obs.mount);

    if (acsConfig.process_sys[sys] == false)
        return;

    E_FType frq1;
    E_FType frq2;
    E_FType frq3;
    int     nf = obsFreqs(obs, frq1, frq2, frq3);
    if (nf < 2)
    {
        traceSlipEvent(
            trace,
            "PDE",
            obs,
            "skipped",
            NONE,
            NONE,
            NONE,
            0,
            0,
            0,
            E_SlipDiagReason::NO_FREQUENCIES
        );
        return;
    }

    /* first epoch or large gap or low elevation */  // todo? initialisation stuff, remove
    if (satStat.lc_pre.time.bigTime == 0 || satStat.el < recOpts.elevation_mask_deg * D2R ||
        lc_new.time > lc_old.time + PDEGAP)
    {
        satStat.mwSlip  = {};
        satStat.emwSlip = {};

        if (lc_new.time > lc_old.time + PDEGAP)
            tracepdeex(
                1,
                trace,
                "\nPDE-CS GPST       %s %4s %5.2f --time gap --",
                timeStr.c_str(),
                id,
                satStat.el * R2D
            );
        if (satStat.el < recOpts.elevation_mask_deg * D2R)
            tracepdeex(
                1,
                trace,
                "\nPDE-CS GPST       %s %4s %5.2f --low_elevation --",
                timeStr.c_str(),
                id,
                satStat.el * R2D
            );
        else
            tracepdeex(
                1,
                trace,
                "\nPDE-CS GPST       %s %4s %5.2f --satStat.lc_pre.time.time --",
                timeStr.c_str(),
                id,
                satStat.el * R2D
            );

        E_SlipDiagReason reason = E_SlipDiagReason::FIRST_EPOCH;
        if (lc_new.time > lc_old.time + PDEGAP)
        {
            reason = E_SlipDiagReason::TIME_GAP;
        }
        else if (satStat.el < recOpts.elevation_mask_deg * D2R)
        {
            reason = E_SlipDiagReason::LOW_ELEVATION;
        }
        traceSlipEvent(
            trace,
            "PDE",
            obs,
            "initialised",
            frq1,
            frq2,
            frq3,
            satStat.el * R2D,
            recOpts.elevation_mask_deg,
            PDEGAP,
            reason
        );

        return;
    }

    if (nf == 2 && lc_new.L_m[frq1] != 0 && lc_new.L_m[frq2] != 0)
    {
        dualFreq = true;
    }

    if (dualFreq && lc_old.L_m[frq1] != 0 && lc_old.L_m[frq2] != 0)
    {
        traceSlipEvent(
            trace,
            "PDE",
            obs,
            "evaluating",
            frq1,
            frq2,
            NONE,
            2,
            0,
            0,
            E_SlipDiagReason::DUAL_FREQUENCY
        );
        cycleslip2(trace, satStat, lc_new, obs);

        /* update averaged MW noise when no cycle slip */
        if (satStat.sigStatMap[ft2string(frq1)].slip.any == 0 &&
            satStat.sigStatMap[ft2string(frq2)].slip.any == 0)
        {
            S_LC& lcPair12 = getLC(lc_new, frq1, frq2);
            lowPassFilter(satStat.mwSlip, lcPair12.MW_c, acsConfig.preprocOpts.mw_proc_noise);
        }
        else
        {
            satStat.mwSlip = {};
        }
    }
    /* track selected third frequency again */
    else if (
        nf >= 3 && lc_new.L_m[frq1] != 0 && lc_new.L_m[frq2] != 0 && lc_new.L_m[frq3] != 0 &&
        lc_old.L_m[frq1] != 0 && lc_old.L_m[frq2] != 0 && lc_old.L_m[frq3] == 0
    )  // was zero, now not.
    {
        /* set slip flag for the selected third frequency and introduce a new ambiguity */
        satStat.sigStatMap[ft2string(frq3)].slip.retrack      = true;
        satStat.sigStatMap[ft2string(frq3)].savedSlip.retrack = true;
        traceSlipEvent(
            trace,
            "PDE",
            obs,
            "retracking",
            frq1,
            frq2,
            frq3,
            3,
            2,
            0,
            E_SlipDiagReason::THIRD_FREQUENCY_REACQUIRED
        );
        cycleslip2(trace, satStat, lc_new, obs);

        /* update averaged MW noise when no cycle slip */
        if (satStat.sigStatMap[ft2string(frq1)].slip.any == 0 &&
            satStat.sigStatMap[ft2string(frq2)].slip.any == 0)
        {
            S_LC& lcPair12 = getLC(lc_new, frq1, frq2);
            lowPassFilter(satStat.mwSlip, lcPair12.MW_c, acsConfig.preprocOpts.mw_proc_noise);
        }
        else
        {
            satStat.mwSlip = {};
        }
    }
    /* Triple-frequency */
    else if (
        nf >= 3 && lc_new.L_m[frq1] != 0 && lc_new.L_m[frq2] != 0 && lc_new.L_m[frq3] != 0 &&
        lc_old.L_m[frq1] != 0 && lc_old.L_m[frq2] != 0 && lc_old.L_m[frq3] != 0
    )
    {
        traceSlipEvent(
            trace,
            "PDE",
            obs,
            "evaluating",
            frq1,
            frq2,
            frq3,
            3,
            0,
            0,
            E_SlipDiagReason::TRIPLE_FREQUENCY
        );
        cycleslip3(trace, satStat, lc_new, obs);

        if (satStat.el * R2D > 30)
        {
            if (satStat.sigStatMap[ft2string(frq1)].slip.any == 2  // todo? check the 2
                && satStat.amb[0] == 0 && satStat.amb[1] == 0 && satStat.amb[2] == 0)
            {
                satStat.sigStatMap[ft2string(frq1)].slip.any = 0;
                satStat.sigStatMap[ft2string(frq2)].slip.any = 0;
                satStat.sigStatMap[ft2string(frq3)].slip.any = 0;
            }
        }

        /* update averaged MW noise for selected frequencies 2 and 3 when no cycle slip */
        if (satStat.sigStatMap[ft2string(frq1)].slip.any == 0 &&
            satStat.sigStatMap[ft2string(frq2)].slip.any == 0 &&
            satStat.sigStatMap[ft2string(frq3)].slip.any == 0)
        {
            S_LC& lcPair23 = getLC(lc_new, frq2, frq3);
            lowPassFilter(satStat.emwSlip, lcPair23.MW_c, acsConfig.preprocOpts.mw_proc_noise);
        }
        else
        {
            satStat.emwSlip = {};
        }
    }
    /* track L1 or L2 again, new rising satellite */
    else if (dualFreq && (lc_old.L_m[frq1] == 0 || lc_old.L_m[frq2] == 0))
    {
        satStat.flt.slip = 0;
        satStat.flt.ne   = 0;
        for (auto& [key, sigStat] : satStat.sigStatMap)
        {
            sigStat.slip.retrack      = true;
            sigStat.savedSlip.retrack = true;
        }

        tracepdeex(
            1,
            trace,
            "\nPDE-CS GPST       %s %4s %5.2f --  re-tracking   --\n",
            timeStr.c_str(),
            id,
            satStat.el * R2D
        );
        traceSlipEvent(
            trace,
            "PDE",
            obs,
            "retracking",
            frq1,
            frq2,
            NONE,
            2,
            0,
            0,
            E_SlipDiagReason::FREQUENCY_REACQUIRED
        );
    }
    else
    {
        satStat.flt.slip = 0;
        satStat.flt.ne   = 0;
        for (auto& [key, sigStat] : satStat.sigStatMap)
        {
            sigStat.slip.singleFreq      = true;
            sigStat.savedSlip.singleFreq = true;
        }

        tracepdeex(
            1,
            trace,
            "\nPDE-CS GPST       %s %4s %5.2f --single frequency--\n",
            timeStr.c_str(),
            id,
            satStat.el * R2D
        );
        traceSlipEvent(
            trace,
            "PDE",
            obs,
            "flagged",
            frq1,
            frq2,
            frq3,
            1,
            0,
            0,
            E_SlipDiagReason::SINGLE_FREQUENCY
        );
    }
}

void clearSlips(ObsList& obsList)
{
    // clear non-persistent status values.
    for (auto& obs : only<GObs>(obsList))
    {
        if (acsConfig.process_sys[obs.Sat.sys] == false)
        {
            continue;
        }

        auto& satOpts = acsConfig.getSatOpts(obs.Sat);

        if (satOpts.exclude)
        {
            continue;
        }

        for (auto& [sigKey, sigStat] : obs.satStat_ptr->sigStatMap)
        {
            SatStat& satStat = *(obs.satStat_ptr);

            satStat.slip     = false;  // todo? is this used?
            sigStat.slip.any = 0;
        }
    }
}

/** Detect slips for multiple observations
 */
void detectslips(
    Trace&   trace,   ///< Trace to output to
    ObsList& obsList  ///< List of observations to detect slips within
)
{
    tracepdeex(2, trace, "\n   *-------- PDE cycle slip detection & repair --------*\n");

    detslp_ll(trace, obsList);
    detslp_gf(trace, obsList);
    detslp_mw(trace, obsList);

    tracepdeex(
        2,
        trace,
        "\nPDE-CS GPST       epoch                   prn  el   lamw    gf12    mw12     siggf  "
        "sigmw  "
        "lamew     gf23    mw23   "
        "            LC                   N1   N2   N3\n"
    );

    for (auto& obs : only<GObs>(obsList))
    {
        if (obs.exclude)
        {
            continue;
        }

        SatStat& satStat = *(obs.satStat_ptr);

        detectslip(trace, satStat, satStat.lc_new, satStat.lc_pre, obs);

        for (auto& [ft, sig] : obs.sigs)
        {
            auto& sigStat = obs.satStat_ptr->sigStatMap[ft2string(ft)];

            if (sigStat.slip.any)
            {
                satStat.slip = true;
            }
        }
    }
}
