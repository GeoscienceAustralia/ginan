// #pragma GCC optimize ("O0")

#include <algorithm>
#include <math.h>
#include <sstream>
#include <string>
#include "architectureDocs.hpp"
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/biases.hpp"
#include "common/common.hpp"
#include "common/constants.hpp"
#include "common/eigenIncluder.hpp"
#include "common/enums.h"
#include "common/ephPrecise.hpp"
#include "common/ionModels.hpp"
#include "common/mongoWrite.hpp"
#include "common/navigation.hpp"
#include "common/receiver.hpp"
#include "common/satStat.hpp"
#include "common/trace.hpp"
#include "orbprop/coordinates.hpp"
#include "pea/ppp.hpp"
#include "trop/tropModels.hpp"

using std::ostringstream;

Architecture SPP__() {}

/** Calculate pseudorange and code bias correction
 */
bool prange(
    Trace&      trace,          ///< Trace file to output to
    GObs&       obs,            ///< Observation to calculate pseudorange for
    E_IonoMode& ionoMode,       ///< Ionospheric correction mode
    E_FType&    ft_A,           ///< Primary frequency used for calculating pseudorange
    E_FType&    ft_B,           ///< Secondary frequency used for calculating pseudorange
    double&     range,          ///< Pseudorange value output
    double&     measVar,        ///< Pseudorange variance output
    double&     bias,           ///< Bias value output
    double&     biasVar,        ///< Bias variance output
    KFState*    kfState_ptr,    ///< Optional kfstate to retrieve biases from
    bool        smooth = false  ///< Update smoothing filter
)
{
    ft_A    = NONE;
    ft_B    = NONE;
    range   = 0;
    measVar = 0;
    bias    = 0;
    biasVar = 0;

    E_Sys sys = obs.Sat.sys;
    if (sys == E_Sys::NONE)
    {
        return false;
    }

    E_FType f_1;
    E_FType f_2;
    E_FType f_3;
    if (!satFreqs(sys, f_1, f_2, f_3))
    {
        return false;
    }

    // list<E_FType> ftList = {f_1, f_2, f_3};
    list<E_FType> ftList = {f_1};  // Temporary fix
    if (f_2 != f_1)
    {
        ftList.push_back(f_2);
    }
    if (f_3 != f_1 && f_3 != f_2)
    {
        ftList.push_back(f_3);
    }

    SatNav& satNav = *obs.satNav_ptr;
    auto&   lam    = satNav.lamMap;

    // Get the first available pseudorange and its code bias according to ftList and update
    // ftList
    auto getPrange = [&](E_FType& ft, double& meas, double& varMeas, double& bias, double& varBias)
    {
        while (ftList.size() > 0)
        {
            ft = ftList.front();
            ftList.pop_front();

            if (obs.sigs[ft].P == 0 || lam[ft] == 0)
            {
                ft = NONE;

                continue;
            }

            meas    = obs.sigs[ft].P;
            varMeas = obs.sigs[ft].codeVar;

            // Get a bias if the default invalid value is still present
            string sigName = enum_to_string(obs.sigs[ft].code);
            auto&  satOpts = acsConfig.getSatOpts(obs.Sat, {sigName});

            bias      = 0;
            varBias   = SQR(satOpts.codeBiasModel.undefined_sigma);
            bool pass = getBias(
                trace,
                obs.time,
                obs.Sat.id(),
                obs.Sat,
                obs.sigs[ft].code,
                CODE,
                bias,
                varBias,
                kfState_ptr
            );

            if (pass == false)
            {
                BOOST_LOG_TRIVIAL(warning)
                    << "Bias not found for " << obs.Sat.id() << " on "
                    << enum_to_string(obs.sigs[ft].code)
                    << ", using undefined_sigma: " << satOpts.codeBiasModel.undefined_sigma;
            }

            break;
        }
    };

    double P_A       = 0;
    double var_A     = 0;
    double bias_A    = 0;
    double varBias_A = 0;
    getPrange(ft_A, P_A, var_A, bias_A, varBias_A);

    if (P_A == 0)  // No pseudorange available
    {
        return false;
    }

    range   = P_A;
    measVar = var_A;
    bias    = bias_A;
    biasVar = varBias_A;

    if (ionoMode == E_IonoMode::IONO_FREE_LINEAR_COMBO)
    {
        double P_B       = 0;
        double var_B     = 0;
        double bias_B    = 0;
        double varBias_B = 0;
        getPrange(ft_B, P_B, var_B, bias_B, varBias_B);

        if (P_B == 0 || ft_B == NONE)
        {
            BOOST_LOG_TRIVIAL(warning)
                << "Code measurement not available on secondary frequency for " << obs.Sat.id()
                << " at " << obs.mount << ", falling back to single-frequency";

            ionoMode = E_IonoMode::BROADCAST;
        }
        else
        {
            // Iono-free combination
            double c1 = SQR(lam[ft_B]) / (SQR(lam[ft_B]) - SQR(lam[ft_A]));
            double c2 = 1 - c1;

            range = c1 * P_A + c2 * P_B;
            bias  = c1 * bias_A + c2 * bias_B;

            measVar = SQR(c1) * var_A + SQR(c2) * var_B;
            biasVar = abs(SQR(c1) * varBias_A - SQR(c2) * varBias_B);  // Eugene: bias_A and
            // bias_B are expected to be fully correlated?
        }
    }

    if (acsConfig.sbsInOpts.smth_win > 0)
    {
        double LC   = obs.sigs[ft_A].L * lam[ft_A];
        double varL = obs.sigs[ft_A].phasVar;
        if (LC == 0)
            return false;

        if (ionoMode == E_IonoMode::IONO_FREE_LINEAR_COMBO)
        {
            double L2 = obs.sigs[ft_B].L * lam[ft_B];
            if (L2 == 0)
                return false;

            double c1 = SQR(lam[ft_B]) / (SQR(lam[ft_B]) - SQR(lam[ft_A]));
            double c2 = 1 - c1;
            LC        = c1 * LC + c2 * L2;
            varL      = c1 * c1 * varL + c2 * c2 * obs.sigs[ft_B].phasVar;
        }

        range = sbasSmoothedPsudo(
            trace,
            obs.time,
            obs.Sat,
            obs.mount,
            range,
            LC,
            measVar,
            varL,
            measVar,
            smooth
        );
        biasVar = 0;

        if (measVar < 0)
            return false;
    }

    return true;
}

/** Validate Dilution of Precision of solution
 */
bool validateDOP(
    ObsList& obsList,            ///< List of observations for this epoch
    double   elevationMaskDeg,   ///< Elevation mask
    Dops*    dops_ptr = nullptr  ///< Optional pointer to output for DOP
)
{
    vector<AzEl> azels;
    azels.reserve(8);
    double dop[4] = {};

    // Large GDOP check
    for (auto& obs : only<GObs>(obsList))
    {
        if (obs.exclude)
        {
            continue;
        }

        if (obs.sppValid == false)
            continue;

        auto& satStat = *obs.satStat_ptr;

        if (satStat.el < elevationMaskDeg * D2R)
        {
            continue;
        }

        azels.push_back(satStat);
    }

    Dops dops = dopCalc(azels);

    if (dops_ptr != nullptr)
    {
        *dops_ptr = dops;
    }

    if (dops.gdop <= 0 || dops.gdop > acsConfig.sppOpts.max_gdop)
    {
        BOOST_LOG_TRIVIAL(warning)
            << "DOP Validation failed for " << obsList.front()->mount << ", gdop=" << dops.gdop;

        return false;
    }

    return true;
}

void printFailures(
    const string& id,      ///< Id of receiver
    ObsList&      obsList  ///< List of observations for this epoch
)
{
    tracepdeex(4, std::cout, "\nFailures:");
    tracepdeex(4, std::cout, "\n%20s ", "");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%c", obs.Sat.sysChar());
    tracepdeex(4, std::cout, "\n%20s ", "");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", obs.Sat.prn / 10 % 10);
    tracepdeex(4, std::cout, "\n%20s ", "");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", obs.Sat.prn % 10);

    tracepdeex(4, std::cout, "\n%20s:", "failExclude");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureExclude);
    tracepdeex(4, std::cout, "\n%20s:", "failNoSatPos");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureNoSatPos);
    tracepdeex(4, std::cout, "\n%20s:", "failNoSatClock");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureNoSatClock);
    tracepdeex(4, std::cout, "\n%20s:", "failNoPseudorange");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureNoPseudorange);
    tracepdeex(4, std::cout, "\n%20s:", "failIodeConsistency");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureIodeConsistency);
    tracepdeex(4, std::cout, "\n%20s:", "failBroadcastEph");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureBroadcastEph);
    tracepdeex(4, std::cout, "\n%20s:", "failRSat");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureRSat);
    tracepdeex(4, std::cout, "\n%20s:", "failSSRFail");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureSSRFail);
    tracepdeex(4, std::cout, "\n%20s:", "failSsrPosEmpty");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosEmpty);
    tracepdeex(4, std::cout, "\n%20s:", "failSsrClkEmpty");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkEmpty);
    tracepdeex(4, std::cout, "\n%20s:", "failSsrPosTime");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosTime);
    tracepdeex(4, std::cout, "\n%20s:", "failSsrClkTime");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkTime);
    tracepdeex(4, std::cout, "\n%20s:", "failSsrPosMag");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosMag);
    tracepdeex(4, std::cout, "\n%20s:", "failSsrClkMag");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkMag);
    tracepdeex(4, std::cout, "\n%20s:", "failSsrPosUdi");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrPosUdi);
    tracepdeex(4, std::cout, "\n%20s:", "failSsrClkUdi");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureSsrClkUdi);
    tracepdeex(4, std::cout, "\n%20s:", "failGeodist");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureGeodist);
    tracepdeex(4, std::cout, "\n%20s:", "failElevation");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failureElevation);
    tracepdeex(4, std::cout, "\n%20s:", "failPrange");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.failurePrange);
    tracepdeex(4, std::cout, "\n%20s:", "excludeElevation");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.excludeElevation);
    tracepdeex(4, std::cout, "\n%20s:", "excludeEclipse");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.excludeEclipse);
    tracepdeex(4, std::cout, "\n%20s:", "excludeSystem");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.excludeSystem);
    tracepdeex(4, std::cout, "\n%20s:", "excludeOutlier");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.excludeOutlier);
    tracepdeex(4, std::cout, "\n%20s:", "excludeBadSPP");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.excludeBadSPP);
    tracepdeex(4, std::cout, "\n%20s:", "excludeConfig");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.excludeConfig);
    tracepdeex(4, std::cout, "\n%20s:", "excludeSVH");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.excludeSVH);
    tracepdeex(4, std::cout, "\n%20s:", "excludeBadRange");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, std::cout, "%d", (bool)obs.excludeBadRange);

    tracepdeex(4, std::cout, "\n\n");
}

void removeUnmeasuredStates(
    KFState&         kfState,         ///< Filter to remove states from
    KFMeasEntryList& kfMeasEntryList  ///< List of measurements for this filter iteration
)
{
    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type == KF::ONE)
        {
            continue;
        }

        bool found = false;

        for (auto& measEntry : kfMeasEntryList)
        {
            auto it = measEntry.designEntryMap.find(key);
            if (it != measEntry.designEntryMap.end())
            {
                found = true;
                break;
            }
        }

        if (found)
        {
            continue;
        }

        kfState.removeState(key);
    }
}

/** Estimate receiver position and clock biases using pseudorange measurements
 */
E_Solution estpos(
    Trace&    trace,                  ///< Trace file to output to
    ObsList&  obsList,                ///< List of observations for this epoch
    Solution& sol,                    ///< Solution object containing initial conditions and results
    string    id,                     ///< Id of receiver
    KFState*  kfState_ptr = nullptr,  ///< Optional kfstate pointer to retrieve ppp values from
    string    description = "SPP",    ///< Description to prepend to clarify outputs
    bool      inRaim      = false     ///< Is in RAIM
)
{
    if (obsList.empty())
    {
        return E_Solution::NONE;
    }

    string suffix = (string) "/" + description;

    auto& recOpts = acsConfig.getRecOpts(id);

    auto& kfState = sol.sppState;
    if (acsConfig.sppOpts.always_reinitialise || inRaim)
    {
        kfState = KFState();  // Reset to apriori to prevent lock-in of bad states
    }

    kfState.FilterOptions::operator=(acsConfig.sppOpts);

    if (inRaim)
    {
        kfState.chiSquareTest.enable = true;  // RAIM requires Chi-square test
    }

    kfState.output_residuals = false;  // Residuals can be outputted each SPP iteration but not each
                                       // least squares iteration

    kfState.measRejectCallbacks.clear();
    kfState.measRejectCallbacks.push_back(deweightMeas);

    int      iter;
    int      numMeas      = 0;
    double   adjustment   = 1E7;
    bool     skipLsqCheck = true;  // Don't do outlier screening before nearly converge or in RAIM
    Vector3d rRec         = receiverMap[id].aprioriPos;
    double   dtRec        = receiverMap[id].aprioriClk;

    tracepdeex(5, trace, "\n\n ---- STARTING SPP LSQ ----");

    for (iter = 0; iter < acsConfig.sppOpts.max_lsq_iterations; iter++)
    {
        tracepdeex(2, trace, "\n\nSPP Iteration: %d", iter);

        kfState.initFilterEpoch(trace);

        // Rec apriori pos
        KFKey recPosKeys[3];
        for (short i = 0; i < 3; i++)
        {
            recPosKeys[i].type = KF::REC_POS;
            recPosKeys[i].num  = i;
            recPosKeys[i].str  = id;

            kfState.getKFValue(recPosKeys[i], rRec(i));
        }

        tracepdeex(4, trace, "\nSPP apriori pos: %f %f %f", rRec(0), rRec(1), rRec(2));

        VectorPos pos = ecef2pos(rRec);
        if (pos.hgt() > 6E7)
        {
            tracepdeex(
                3,
                trace,
                "\n%s\tSPP found unfeasible position with height: %f",
                tsync.to_string().c_str(),
                pos.hgt()
            );

            return E_Solution::FAILED;
        }

        KFMeasEntryList kfMeasEntryList;

        for (auto& obs : only<GObs>(obsList))
        {
            // Reset valid and failure flags that may be updated across iterations
            obs.sppValid         = false;
            obs.failureGeodist   = false;
            obs.failureElevation = false;
            obs.failurePrange    = false;

            std::stringstream traceBuffer;
            traceBuffer << "\nSPP meas: sat=" << obs.Sat.id().c_str();

            if (obs.exclude)
            {
                obs.failureExclude = true;

                traceBuffer << " ... SPP exclusion: " << obs.exclude;
                if (obs.excludeSystem)
                    tracepdeex(6, trace, "%s", traceBuffer.str());
                else
                    tracepdeex(2, trace, "%s", traceBuffer.str());

                continue;
            }

            // Sat pos
            Vector3d rSat      = obs.rSatApc;
            double   varSatPos = obs.posVar;
            if (obs.ephPosValid == false || rSat.isZero())
            {
                obs.failureNoSatPos = true;

                traceBuffer << " ... Sat pos fail";
                tracepdeex(2, trace, "%s", traceBuffer.str());

                continue;
            }

            SatStat& satStat = *obs.satStat_ptr;

            // Update line-of-sight vector, elevation and observation variance, these should go
            // before prange()
            double r = geodist(rSat, rRec, satStat.e);
            satazel(pos, satStat.e, satStat);
            obsVariance(obs);

            // Pseudorange and code bias
            E_IonoMode ionoMode = acsConfig.sppOpts.iono_mode;
            E_FType    ft1;
            E_FType    ft2;
            double     range;
            double     varMeas;
            double     bias;
            double     varBias;
            bool       smooth = (inRaim == false && iter == 0);
            bool       pass   = prange(
                trace,
                obs,
                ionoMode,
                ft1,
                ft2,
                range,
                varMeas,
                bias,
                varBias,
                kfState_ptr,
                smooth
            );
            if (pass == false)
            {
                obs.failurePrange = true;

                traceBuffer << " ... Pseudorange fail";
                tracepdeex(2, trace, "%s", traceBuffer.str());

                continue;
            }

            string codeStr = enum_to_string(obs.sigs[ft1].code);
            if (ft2 != NONE)
                codeStr = codeStr + "-" + enum_to_string(obs.sigs[ft2].code);

            tracepdeex(
                2,
                trace,
                "%s, obs=%s, range=%.3f, bias=%.3f",
                traceBuffer.str(),
                codeStr,
                range,
                bias
            );

            // Geodistance
            if (r <= 0)
            {
                obs.failureGeodist = true;

                tracepdeex(2, trace, " ... Geodist fail");

                continue;
            }

            tracepdeex(2, trace, ", dist=%.3f", r);

            // Elevation mask
            double elevation = satStat.el * R2D;
            if (elevation < acsConfig.sppOpts.elevation_mask_deg && adjustment < 5E4)
            {
                obs.failureElevation = true;

                tracepdeex(2, trace, " ... Elevation mask fail");

                continue;
            }

            tracepdeex(2, trace, ", el=%.2f", elevation);

            // Sat clock
            if (obs.ephClkValid == false)
            {
                obs.failureNoSatClock = true;

                tracepdeex(2, trace, " ... Sat clk fail");

                continue;
            }

            double dtSat     = -obs.satClk * CLIGHT;
            double varSatClk = obs.satClkVar * SQR(CLIGHT);

            tracepdeex(2, trace, ", satClk=%.3f", dtSat);

            // Rec clock
            KFKey recSysBiasKey{KF::REC_SYS_BIAS, {obs.Sat.sys}, id};
            kfState.getKFValue(recSysBiasKey, dtRec);

            tracepdeex(2, trace, ", recClk=%.3f", dtRec);

            // Ionospheric correction
            double dummy   = 0;
            double dIono   = 0;
            double varIono = 0;
            ionoModel(
                obs.time,
                pos,
                satStat,
                recOpts.mapping_function,
                ionoMode,
                recOpts.mapping_function_layer_height,
                dummy,
                dIono,
                varIono
            );

            if (dIono)
            {
                double ionC = SQR(obs.satNav_ptr->lamMap[ft1] / genericWavelength[F1]);
                dIono *= ionC;
                varIono *= SQR(ionC);
            }

            if (acsConfig.sbsInOpts.dfmc_uire)
                varIono = SQR(0.018 + 40 / (261 + SQR(satStat.el * R2D)));

            tracepdeex(2, trace, ", dIono=%.5f", dIono);

            // Tropospheric correction
            double      varTrop;
            TropStates  tropStates;
            TropMapping dTropDx;
            double      dTrop = tropModel(
                trace,
                acsConfig.sppOpts.trop_models,
                obs.time,
                pos,
                satStat,
                tropStates,
                dTropDx,
                varTrop
            );

            tracepdeex(2, trace, ", dTrop=%.5f", dTrop);

            // Pseudorange residual
            double expected = r + bias + dtSat + dtRec + dIono + dTrop;
            double res      = range - expected;

            if (abs(res) > 7E6)  // Assume worst apriori rec pos is 0 and rec clock offset
                                 // can be up to 2ms (usually less than 1ms)
            {
                obs.failurePrange = true;

                tracepdeex(2, trace, " ... Residual fail");

                continue;
            }

            tracepdeex(2, trace, ", res=%.6f", res);

            // Error variance
            double var = varMeas + varBias + varSatPos + varSatClk + varIono + varTrop;
            var *= SQR(acsConfig.sppOpts.sigma_scaling);

            tracepdeex(
                5,
                trace,
                "\nSPP meas: sat=%s, obs=%s, varMeas=%f, varBias=%f, varSatPos=%f, varSatClk=%f, "
                "varIono=%f, varTrop=%f, scale=%f, var=%f",
                obs.Sat.id().c_str(),
                codeStr,
                varMeas,
                varBias,
                varSatPos,
                varSatClk,
                varIono,
                varTrop,
                acsConfig.sppOpts.sigma_scaling,
                var
            );

            // Calculate design entry and form up the measurement
            KFMeasEntry codeMeas(&kfState);

            for (short i = 0; i < 3; i++)
            {
                InitialState posInit;
                posInit.x = rRec[i];

                codeMeas.addDsgnEntry(recPosKeys[i], -satStat.e[i], posInit);
            }
            {
                InitialState clkInit;
                clkInit.x = dtRec;

                codeMeas.addDsgnEntry(recSysBiasKey, 1, clkInit);
            }

            codeMeas.obsKey.Sat     = obs.Sat;
            codeMeas.obsKey.str     = id;
            codeMeas.obsKey.num     = ft2 ? (static_cast<int>(obs.sigs[ft1].code) * 100 +
                                         static_cast<int>(obs.sigs[ft2].code))
                                          : static_cast<int>(obs.sigs[ft1].code);
            codeMeas.obsKey.type    = KF::CODE_MEAS;
            codeMeas.obsKey.comment = "";

            codeMeas.setInnov(res);
            codeMeas.setNoise(var);

            codeMeas.metaDataMap["sppObs_ptr"] = &obs;

            kfMeasEntryList.push_back(codeMeas);

            obs.sppValid = true;  // todo aaron, this is messy, lots of excludes dont work if spp
                                  // not run, harmonise the spp/ppp exclusion methods.
            obs.sppCodeResidual = res;
        }

        tracepdeex(2, trace, "\n");

        // Force reinitialisation of everything by least squares
        kfState.P.setIdentity();
        kfState.P *= -1;

        removeUnmeasuredStates(kfState, kfMeasEntryList);

        // Use state transition to initialise states
        kfState.stateTransition(trace, tsync);

        // Combine the measurement list into a single matrix
        numMeas     = kfMeasEntryList.size();
        sol.numMeas = numMeas;

        KFMeas kfMeas(kfState, kfMeasEntryList, tsync);

        if (kfMeas.H.cols() == 0)
        {
            printFailures(id, obsList);

            tracepdeex(3, trace, "\nNo valid states, END OF SPP LSQ");

            return E_Solution::NONE;
        }

        if (numMeas < kfMeas.H.cols() - 1 || numMeas == 0)
        {
            printFailures(id, obsList);

            tracepdeex(3, trace, "\nLack of valid measurements, END OF SPP LSQ");

            return E_Solution::FAILED;
        }

        // Least squares estimation
        bool pass = kfState.leastSquareInitStates(trace, kfMeas, suffix, true, true, skipLsqCheck);

        if (pass == false)
        {
            tracepdeex(4, trace, "\n%s\tSPP failed due to LSQ failure", tsync.to_string().c_str());
            return E_Solution::FAILED;
        }

        if (traceLevel >= 4)
        {
            outputResiduals(trace, kfMeas, suffix, iter, 0, kfMeas.H.rows());
        }

        if (traceLevel >= 5)
        {
            kfState.outputStates(trace, suffix, iter);
        }

        adjustment =
            kfState.dx.cwiseAbs().maxCoeff();  // Avoid using norm() as numX may vary w/ multi-GNSS
        tracepdeex(4, trace, "\nSPP dx: %15.4f\n", adjustment);

        if (inRaim == false && adjustment < 2E5)
        {
            // Only check outliers (if turned on) after nearly converge
            skipLsqCheck = false;
        }

        if ((kfState.lsqOpts.sigma_check || kfState.lsqOpts.omega_test) && skipLsqCheck == false &&
            kfState.sigmaPass == false)
        {
            // Outlier(s) still present, iterate SPP again despite convergence
            continue;
        }

        // Least squares converged
        if (adjustment < 1E-4)
        {
            if (traceLevel == 4)
            {
                kfState.outputStates(trace, suffix);
            }

            if (kfState.chiSquareTest.enable)  // todo Eugene: use meas chi-square test in algebra
            {
                double a =
                    sqrt(kfState.P(1, 1) + kfState.P(2, 2) + kfState.P(3, 3)) * kfState.chi2PerDof;
                double b = sqrt(kfState.P(4, 4)) * kfState.chi2PerDof;

                tracepdeex(4, trace, "\nchi2stats: chi^2     = %10f", kfState.chi2);
                tracepdeex(4, trace, "\nchi2stats: dof       = %10f", kfState.dof);
                tracepdeex(4, trace, "\nchi2stats: chi^2/dof = %10f", kfState.chi2PerDof);
                tracepdeex(5, trace, "\nchi2stats: sqrt(varPos) * chi^2/dof = %10f", a);
                tracepdeex(5, trace, "\nchi2stats: sqrt(varClk) * chi^2/dof = %10f", b);

                if (kfState.chiQCPass == false)
                {
                    tracepdeex(3, trace, "\n%s\tSPP error - Bad chiQC", tsync.to_string().c_str());

                    return E_Solution::SINGLE_X;
                }
            }

            bool dopPass = validateDOP(obsList, acsConfig.sppOpts.elevation_mask_deg, &sol.dops);
            if (dopPass == false)
            {
                tracepdeex(
                    3,
                    trace,
                    "\n%s\tSPP error - Bad DOP: %f",
                    tsync.to_string().c_str(),
                    sol.dops.gdop
                );

                return E_Solution::SINGLE_X;
            }

            return E_Solution::SINGLE;
        }
    }

    tracepdeex(5, trace, "\n ---- END OF SPP LSQ, iterations = %d ----", iter);

    if (iter >= acsConfig.sppOpts.max_lsq_iterations)
    {
        tracepdeex(
            3,
            trace,
            "\n%s\tSPP failed to converge after %d iterations",
            tsync.to_string().c_str(),
            iter
        );
    }

    if (traceLevel == 4)
    {
        // Still output states if fails
        kfState.outputStates(trace, suffix);
    }

    return E_Solution::FAILED;
}

/** Receiver autonomous integrity monitoring (RAIM) failure detection and exclution
 * Note: This is a simplified version of RAIM algorithm that tries to exclude multiple outliers
 * iteratively instead of checking all possible subsets when more than one outliers present
 */
bool raim(
    Trace&    trace,    ///< Trace file to output to
    ObsList&  obsList,  ///< List of observations for this epoch
    Solution& sol,      ///< Solution object containing initial conditions and results
    string    id,       ///< Id of receiver
    KFState*  kfState_ptr = nullptr
)
{
    trace << "\n" << tsync << "\tPerforming RAIM.";

    vector<GObs*> exList;
    ObsList       testList = obsList;
    ObsList       bestList;

    Solution bestSol = sol;
    double   bestVar = bestSol.sppState.chi2PerDof;

    map<SatSys, SatStat> origSatStats;
    map<SatSys, SatStat> bestSatStats;

    auto backupSatStats = [&](map<SatSys, SatStat>& dest, bool backup)
    {
        for (auto& obs : only<GObs>(obsList))
        {
            if (obs.exclude)
            {
                continue;
            }

            if (backup)
                dest[obs.Sat] = *obs.satStat_ptr;
            else
                *obs.satStat_ptr = dest[obs.Sat];
        }
    };

    // Backup original satStats
    backupSatStats(origSatStats, true);

    int iter;
    for (iter = 0; iter < acsConfig.sppOpts.raim.max_iterations; iter++)
    {
        tracepdeex(4, trace, "\n\nRAIM Iteration: %d", iter);

        if (bestSol.sppState.dof < 2)  // Need dof >=2, otherwise all candidate subsets pass equally
        {
            tracepdeex(
                3,
                trace,
                "\n%s: lack of satellites to perform RAIM, numSat=%2d",
                __FUNCTION__,
                bestSol.numMeas
            );
            return false;
        }

        GObs* exObs_ptr = nullptr;

        for (auto& testObs : only<GObs>(testList))
        {
            if (testObs.exclude)
            {
                continue;
            }

            // Restore original satStats before each test
            backupSatStats(origSatStats, false);

            ObsList candList;

            // Push a list of candidate subset, i.e. everything that's not the test observation
            for (auto& obs : only<GObs>(testList))
            {
                if (&obs == &testObs)
                {
                    continue;
                }
                if (obs.exclude)
                {
                    continue;
                }

                candList.push_back((shared_ptr<GObs>)obs);
            }

            Solution candSol = sol;

            // Try to get position using candidate subset of all observations
            E_Solution status = estpos(
                trace,
                candList,
                candSol,
                id,
                kfState_ptr,
                (string) "RAIM/" + id + "/" + testObs.Sat.id(),
                true  // In RAIM, skip sigma check and omega test to avoid interferance with RAIM
            );
            if (status == E_Solution::NONE)  // Could have multiple bad measurements and may need
                                             // to exclude one worst obs first (in case all sebsets
                                             // fail but NONE solutions)
            {
                continue;
            }

            if (candSol.sppState.dof < 1)
            {
                tracepdeex(
                    3,
                    trace,
                    "\n%s: exSat=%s, lack of satellites nSat=%2d",
                    __FUNCTION__,
                    testObs.Sat.id().c_str(),
                    candSol.numMeas
                );

                continue;
            }

            double candVar = candSol.sppState.chi2PerDof;
            tracepdeex(
                3,
                trace,
                "\n%s: exSat=%s, chi^2/dof=%f, status=%s",
                __FUNCTION__,
                testObs.Sat.id().c_str(),
                candVar,
                enum_to_string(status)
            );
            tracepdeex(
                5,
                trace,
                "\nBest: exSat=%s, chi^2/dof=%f, status=%s",
                (exObs_ptr ? exObs_ptr->Sat.id() : "Non"),
                bestVar,
                enum_to_string(bestSol.status)
            );

            if ((status < bestSol.status) || (status == bestSol.status && candVar > bestVar))
            {
                // This solution is worse
                continue;
            }

            bestSol        = candSol;
            bestSol.status = status;
            bestVar        = candVar;
            exObs_ptr      = &testObs;
            bestList       = candList;

            // Store 'best' satStats for later use
            backupSatStats(bestSatStats, true);
        }

        if (exObs_ptr)
        {
            exList.push_back(exObs_ptr);

            // Exclude corresponding obs from test list
            for (auto it = testList.begin(); it != testList.end(); it++)
            {
                if (it->get() == exObs_ptr)
                {
                    testList.erase(it);

                    break;
                }
            }

            if (bestSol.status == E_Solution::SINGLE)
            {
                sol = bestSol;

                backupSatStats(
                    bestSatStats,
                    false
                );  // Update satStats (AzEl and line-of-sight unit vector) w/ best SPP solution

                // Copy best obs to real result
                for (auto& bestObs : only<GObs>(bestList))
                    for (auto& origObs : only<GObs>(obsList))
                    {
                        if (bestObs.Sat != origObs.Sat)
                        {
                            // Only use the equivalent obs in the real list according to the best
                            // list
                            continue;
                        }

                        origObs.sppValid        = bestObs.sppValid;
                        origObs.sppCodeResidual = bestObs.sppCodeResidual;
                    }

                for (auto& obs_ptr : exList)
                {
                    obs_ptr->excludeOutlier = true;

                    tracepdeex(
                        3,
                        trace,
                        "\n%s\t%s excluded by RAIM",
                        tsync.to_string().c_str(),
                        obs_ptr->Sat.id().c_str()
                    );
                    BOOST_LOG_TRIVIAL(debug) << obs_ptr->Sat.id() << " was excluded from "
                                             << obsList.front()->mount << " by RAIM";
                }

                BOOST_LOG_TRIVIAL(debug) << "SPP converged after RAIM";

                return true;
            }
        }
        else
        {
            break;  // No observation excluded, stop iterating as testList hasn't changed
        }
    }

    // Restore original satStats if fails
    backupSatStats(origSatStats, false);

    tracepdeex(3, trace, "\n%s\tRAIM failed after %d iterations", tsync.to_string().c_str(), iter);
    BOOST_LOG_TRIVIAL(debug) << "RAIM failed after " << iter << " iterations";

    return false;
}

/** Compute receiver position, clock biases by single-point positioning with pseudorange
 * measurements
 */
void spp(
    Trace&    trace,        ///< Trace file to output to
    Receiver& rec,          ///< Receiver to perform SPP for
    KFState*  kfState_ptr,  ///< Optional pointer to filter to take ephemerides from
    KFState*  remote_ptr    ///< Optional pointer to filter to take ephemerides from
)
{
    auto&  obsList = rec.obsList;
    auto&  sol     = rec.sol;
    string id      = rec.id;

    if (obsList.empty())
    {
        BOOST_LOG_TRIVIAL(error) << "SPP failed due to no observation data on " << id;

        sol.status = E_Solution::NONE;

        return;
    }

    for (auto& obs : only<GObs>(obsList))
    {
        if (acsConfig.process_sys[obs.Sat.sys] == false)
        {
            continue;
        }

        if (obs.ephPosValid == false || obs.ephClkValid == false ||
            acsConfig.preprocOpts.preprocess_all_data ==
                true)  // KALMAN or REMOTE is not available, or SSR is not updated when
                       // preprocessing all data
        {
            auto& satOpts = acsConfig.getSatOpts(obs.Sat);

            satPosClk(
                trace,
                obs.time,
                obs,
                nav,
                satOpts.posModel.sources,
                satOpts.clockModel.sources,
                kfState_ptr,
                remote_ptr,
                E_OffsetType::APC
            );
        }
    }

    tracepdeex(
        3,
        trace,
        "\n\n%s: time=%s, nObs=%zu",
        __FUNCTION__,
        tsync.to_string().c_str(),
        obsList.size()
    );

    // Estimate receiver position with pseudorange
    sol.status = estpos(trace, obsList, sol, id, kfState_ptr, (string) "SPP/" + id);  // todo aaron,
                                                                                      // remote too?

    auto& sppState = sol.sppState;

    if (sol.status != E_Solution::SINGLE)
    {
        // Receiver Autonomous Integrity Monitoring
        if (acsConfig.sppOpts.raim.enable &&
            sol.status != E_Solution::NONE)  // Meaningless to perform RAIM for NONE solution
        {
            int numMeas = 0;
            for (auto& obs : only<GObs>(obsList))
            {
                obs.excludeOutlier =
                    false;  // Clear outlier flags from SPP and let RAIM do the exclusion

                if (obs.exclude)
                {
                    continue;
                }

                numMeas++;
            }

            sppState.dof        = numMeas - (sppState.x.rows() - 1);
            sppState.chi2PerDof = INFINITY;

            bool pass = raim(trace, obsList, sol, id, kfState_ptr);

            if (pass && traceLevel >= 4)
            {
                sppState.outputStates(
                    trace,
                    "/SPP/" + id
                );  // Only output states again when RAIM is successful
            }
        }
    }

    if (sol.status != E_Solution::SINGLE && sol.status != E_Solution::SINGLE_X)
    {
        string stringBuffer = acsConfig.exclude.bad_spp ? ", excluding all observations" : "";
        BOOST_LOG_TRIVIAL(warning) << "SPP failed for " << id << " at " << tsync << stringBuffer;
        trace << "\n" << tsync << "\tSPP failed for " << id << stringBuffer;
    }

    // Set observations that were valid
    for (auto& obs : only<GObs>(obsList))
    {
        if (sol.status != E_Solution::SINGLE && sol.status != E_Solution::SINGLE_X)
        {
            // All measurements are bad if we cant get SPP
            obs.excludeBadSPP = true;
            continue;
        }

        if (obs.exclude || obs.sppValid)
        {
            continue;
        }

        if (obs.failureElevation)  // Don't exclude low elevation obs here as PPP may use a
                                   // different elevation mask
        {
            continue;
        }

        obs.excludeBadSPP = true;
    }

    if (sol.status == E_Solution::NONE)
    {
        return;
    }

    // Copy states to often-used vectors
    for (short i = 0; i < 3; i++)
    {
        sppState.getKFValue({KF::REC_POS, {}, id, i}, sol.sppPos[i]);
    }

    auto& recOpts = acsConfig.getRecOpts(id);
    sol.clkRefSys = recOpts.receiver_reference_system;

    KFKey    clkKey{KF::REC_SYS_BIAS, {sol.clkRefSys}, id};
    E_Source foundSrc = sol.sppState.getKFValue(clkKey, sol.sppClk);
    bool     found    = foundSrc != E_Source::NONE;

    if (found == false)
    {
        for (auto [sys, proc] : acsConfig.process_sys)
        {
            if (proc == false || sys == sol.clkRefSys)  // Has already checked sol.clkRefSys
            {
                continue;
            }

            clkKey.Sat = SatSys(sys);
            foundSrc   = sol.sppState.getKFValue(clkKey, sol.sppClk);
            found      = foundSrc != E_Source::NONE;

            if (found)  // Rec clock must be available for at least one system
            {
                sol.clkRefSys = sys;
                break;
            }
        }

        BOOST_LOG_TRIVIAL(warning)
            << "Receiver clock for " << id << " of "
            << enum_to_string(recOpts.receiver_reference_system) << " system not found, using "
            << enum_to_string(sol.clkRefSys) << " as reference clock";
    }

    sol.sppTime = tsync - sol.sppClk / CLIGHT;

    sol.horzPL = -1;
    sol.vertPL = -1;
    if (sol.status == E_Solution::SINGLE)
    {
        Matrix3d ecefP = sol.sppState.P.block(1, 1, 3, 3);
        estimateSBASProtLvl(sol.sppPos, ecefP, sol.horzPL, sol.vertPL);
    }

    tracepdeex(
        2,
        trace,
        "\n%s pos: %f %f %f",
        __FUNCTION__,
        sol.sppPos[0],
        sol.sppPos[1],
        sol.sppPos[2]
    );
    tracepdeex(
        2,
        trace,
        "\n%s clk (%s): %f\n",
        __FUNCTION__,
        enum_to_string(sol.clkRefSys),
        sol.sppClk
    );

    mongoStates(
        sppState,
        {.suffix    = "/SPP",
         .instances = acsConfig.mongoOpts.output_states,
         .queue     = acsConfig.mongoOpts.queue_outputs}
    );
}
