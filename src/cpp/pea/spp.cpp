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
#include "common/interactiveTerminal.hpp"
#include "common/ionModels.hpp"
#include "common/navigation.hpp"
#include "common/receiver.hpp"
#include "common/satStat.hpp"
#include "common/trace.hpp"
#include "orbprop/coordinates.hpp"
#include "pea/ppp.hpp"
#include "trop/tropModels.hpp"

using std::ostringstream;

Architecture SPP__() {}

constexpr double ERR_ION   = 7.0;  ///< ionospheric delay std (m)
constexpr double ERR_BRDCI = 0.5;  ///< broadcast iono model error factor
constexpr double ERR_CBIAS = 0.3;  ///< code bias error std (m)

/** Calculate pseudorange with code bias correction
 */
bool prange(
    Trace&     trace,       ///< Trace file to output to
    GObs&      obs,         ///< Observation to calculate pseudorange for
    E_IonoMode ionoMode,    ///< Ionospheric correction mode
    double&    range,       ///< Pseudorange value output
    double&    measVar,     ///< Pseudorange variance output
    double&    biasVar,     ///< Bias variance output
    KFState*   kfState_ptr  ///< Optional kfstate to retrieve biases from
)
{
    SatNav& satNav = *obs.satNav_ptr;
    auto&   lam    = satNav.lamMap;

    range   = 0;
    measVar = 0;
    biasVar = SQR(ERR_CBIAS);

    E_Sys sys = obs.Sat.sys;
    if (sys == +E_Sys::NONE)
    {
        return false;
    }

    E_FType f_1;
    E_FType f_2;
    E_FType f_3;
    if (!satFreqs(sys, f_1, f_2, f_3))
        return false;

    if (obs.sigs[f_1].P == 0 || lam[f_1] == 0)
    {
        return false;
    }

    // Get a bias if the default invalid value is still present
    double& B1     = obs.sigs[f_1].biases[CODE];
    double& var1   = obs.sigs[f_1].biasVars[CODE];
    bool    passB1 = true;
    if (isnan(B1))
    {
        B1     = 0;
        var1   = 0;
        passB1 = getBias(
            trace,
            obs.time,
            obs.Sat.id(),
            obs.Sat,
            obs.sigs[f_1].code,
            CODE,
            B1,
            var1,
            kfState_ptr
        );
    }

    double P1 = obs.sigs[f_1].P - B1;

    double PC = 0;
    if (ionoMode == +E_IonoMode::IONO_FREE_LINEAR_COMBO) /* dual-frequency */
    {
        if (obs.sigs[f_2].P == 0 || lam[f_2] == 0)
        {
            return false;
        }

        double gamma = SQR(lam[f_2]) / SQR(lam[f_1]); /* f1^2/f2^2 */

        double& B2     = obs.sigs[f_2].biases[CODE];
        double& var2   = obs.sigs[f_2].biasVars[CODE];
        bool    passB2 = true;
        if (isnan(B2))
        {
            B2     = 0;
            var2   = 0;
            passB2 = getBias(
                trace,
                obs.time,
                obs.Sat.id(),
                obs.Sat,
                obs.sigs[f_2].code,
                CODE,
                B2,
                var2,
                kfState_ptr
            );
        }

        double P2 = obs.sigs[f_2].P - B2;

        /* iono-free combination */
        PC = (gamma * P1 - P2) / (gamma - 1);
    }
    else /* single-frequency */
    {
        if (P1 == 0)
        {
            return false;
        }

        if (passB1 == false)
        {
            BOOST_LOG_TRIVIAL(warning) << "Bias not found in " << __FUNCTION__ << " for "
                                       << obs.Sat.id() << " on " << obs.sigs[f_1].code._to_string();
        }

        PC = P1;
    }

    range   = PC;
    measVar = obs.sigs[f_1].codeVar;  // todo aaron, use combo?

    if (var1)
        biasVar = var1;

    return true;
}

/** Compute ionospheric corrections
 */
bool ionocorr(
    GTime      time,      ///< Time
    VectorPos& pos,       ///< Receiver position in LLH
    AzEl&      azel,      ///< Azimuth and elevation
    E_IonoMode ionoMode,  ///< Ionospheric correction model
    double&    dIono,     ///< Ionospheric delay (L1) value output
    double&    var        ///< Ionospheric delay (L1) variance output
)
{
    // 	trace(4, "ionocorr: time=%s opt=%d sat=%s pos=%.3f %.3f azel=%.3f %.3f\n",
    // 			time.to_string(3).c_str(),
    // 			ionoopt,
    // 			id,
    // 			pos.latDeg(),
    // 			pos.lonDeg(),
    // 			azel[0]	*R2D,
    // 			azel[1]	*R2D);

    if (ionoMode == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
    {
        dIono = 0;
        var   = 0;

        return true;
    }

    /* broadcast model */
    if (ionoMode == +E_IonoMode::BROADCAST)
    {
        E_Sys        sys  = E_Sys::GPS;
        E_NavMsgType type = defNavMsgType[sys];

        auto ion_ptr = seleph<ION>(std::cout, time, sys, type, nav);

        double* vals = nullptr;
        if (ion_ptr != nullptr)
            vals = ion_ptr->vals;

        dIono = ionmodel(time, vals, pos, azel);
        var   = SQR(dIono * ERR_BRDCI);

        return true;
    }

    dIono = 0;
    var   = SQR(ERR_ION);

    /* tmp fix : KH
    if (ionoopt ==  E_IonoMode::TOTAL_ELECTRON_CONTENT)
    {
        int res = iontec(time, &nav, pos, azel, 1, dIono, var);
        if (!res)
        {
            dIono = 0;
            var =	SQR(ERR_ION);
        }
        return res;
    }
    */
    // if (ionoopt != E_IonoMode::OFF)	fprintf(stderr,"SPP not unsupporting ionosphere mode %s",
    // ionoopt._to_string());

    return true;
}

/** Validate Dilution of Precision of solution
 */
bool validateDOP(
    Trace&   trace,              ///< Trace file to output to
    ObsList& obsList,            ///< List of observations for this epoch
    double   elevationMaskDeg,   ///< Elevation mask
    Dops*    dops_ptr = nullptr  ///< Optional pointer to output for DOP
)
{
    vector<AzEl> azels;
    azels.reserve(8);
    double dop[4] = {};
    // 	tracepde(3, trace, "valsol  : n=%d nv=%d\n", obsList.size(), nv);

    // Large gdop check
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
        BOOST_LOG_TRIVIAL(warning) << "Warning: DOP Validation failed for "
                                   << obsList.front()->mount << ", gdop=" << dops.gdop;

        return false;
    }

    return true;
}

void printFailures(const string& id, ObsList& obsList)
{
    InteractiveTerminal ss(string("Failures/") + id, nullStream);

    tracepdeex(4, ss, "\nFailures:");
    tracepdeex(4, ss, "\n%20s ", "");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%c", obs.Sat.sysChar());
    tracepdeex(4, ss, "\n%20s ", "");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", obs.Sat.prn / 10 % 10);
    tracepdeex(4, ss, "\n%20s ", "");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", obs.Sat.prn % 10);

    tracepdeex(4, ss, "\n%20s:", "failExclude");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureExclude);
    tracepdeex(4, ss, "\n%20s:", "failNoSatPos");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureNoSatPos);
    tracepdeex(4, ss, "\n%20s:", "failNoSatClock");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureNoSatClock);
    tracepdeex(4, ss, "\n%20s:", "failNoPseudorange");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureNoPseudorange);
    tracepdeex(4, ss, "\n%20s:", "failIodeConsistency");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureIodeConsistency);
    tracepdeex(4, ss, "\n%20s:", "failBroadcastEph");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureBroadcastEph);
    tracepdeex(4, ss, "\n%20s:", "failRSat");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureRSat);
    tracepdeex(4, ss, "\n%20s:", "failSSRFail");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureSSRFail);
    tracepdeex(4, ss, "\n%20s:", "failSsrPosEmpty");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureSsrPosEmpty);
    tracepdeex(4, ss, "\n%20s:", "failSsrClkEmpty");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureSsrClkEmpty);
    tracepdeex(4, ss, "\n%20s:", "failSsrPosTime");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureSsrPosTime);
    tracepdeex(4, ss, "\n%20s:", "failSsrClkTime");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureSsrClkTime);
    tracepdeex(4, ss, "\n%20s:", "failSsrPosMag");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureSsrPosMag);
    tracepdeex(4, ss, "\n%20s:", "failSsrClkMag");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureSsrClkMag);
    tracepdeex(4, ss, "\n%20s:", "failSsrPosUdi");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureSsrPosUdi);
    tracepdeex(4, ss, "\n%20s:", "failSsrClkUdi");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureSsrClkUdi);
    tracepdeex(4, ss, "\n%20s:", "failGeodist");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureGeodist);
    tracepdeex(4, ss, "\n%20s:", "failElevation");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureElevation);
    tracepdeex(4, ss, "\n%20s:", "failPrange");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failurePrange);
    tracepdeex(4, ss, "\n%20s:", "failIonocorr");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.failureIonocorr);
    tracepdeex(4, ss, "\n%20s:", "excludeElevation");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.excludeElevation);
    tracepdeex(4, ss, "\n%20s:", "excludeEclipse");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.excludeEclipse);
    tracepdeex(4, ss, "\n%20s:", "excludeSystem");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.excludeSystem);
    tracepdeex(4, ss, "\n%20s:", "excludeOutlier");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.excludeOutlier);
    tracepdeex(4, ss, "\n%20s:", "excludeBadSPP");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.excludeBadSPP);
    tracepdeex(4, ss, "\n%20s:", "excludeConfig");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.excludeConfig);
    tracepdeex(4, ss, "\n%20s:", "excludeSVH");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.excludeSVH);
    tracepdeex(4, ss, "\n%20s:", "excludeBadRange");
    for (auto& obs : only<GObs>(obsList))
        tracepdeex(4, ss, "%d", (bool)obs.excludeBadRange);

    tracepdeex(4, ss, "\n\n");

    BOOST_LOG_TRIVIAL(debug) << ss.str();
}

void removeUnmeasuredStates(
    Trace&           trace,           ///< Trace to output to
    KFState&         kfState,         ///< Filter to remove states from
    KFMeasEntryList& kfMeasEntryList  ///< List of measurements for this filter iteration
)
{
    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type == +KF::ONE)
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

/** Estimate receiver position and biases using code measurements
 */
E_Solution estpos(
    Trace&    trace,                  ///< Trace file to output to
    ObsList&  obsList,                ///< List of observations for this epoch
    Solution& sol,                    ///< Solution object containing initial conditions and results
    string    id,                     ///< Id of receiver
    KFState*  kfState_ptr = nullptr,  ///< Optional kfstate pointer to retrieve ppp values from
    string    description = "SPP"     ///< Description to prepend to clarify outputs
)
{
    if (obsList.empty())
    {
        return E_Solution::NONE;
    }

    string suffix = (string) "/" + description;

    auto& recOpts = acsConfig.getRecOpts(id);

    auto& kfState = sol.sppState;
    if (acsConfig.sppOpts.always_reinitialise)
    {
        kfState = KFState();  // reset to zero to prevent lock-in of bad positions
    }

    kfState.FilterOptions::operator=(acsConfig.sppOpts);

    int    iter;
    int    numMeas    = 0;
    int    removals   = 0;
    double adjustment = 10000;

    tracepdeex(5, trace, "\n\n ---- STARTING SPP LSQ ----");

    for (iter = 0; iter < acsConfig.sppOpts.max_lsq_iterations; iter++)
    {
        tracepdeex(2, trace, "\n\nSPP Iteration: %d", iter);

        kfState.initFilterEpoch(trace);

        // Rec apriori pos
        Vector3d rRec  = Vector3d::Zero();  // todo Eugene: use apriori pos
        double   dtRec = 0;                 // todo Eugene: use apriori pos

        KFKey recPosKeys[3];
        KFKey recSysBiasKey = {KF::REC_SYS_BIAS};

        for (short i = 0; i < 3; i++)
        {
            recPosKeys[i].type = KF::REC_POS;
            recPosKeys[i].num  = i;
            recPosKeys[i].str  = id;

            kfState.getKFValue(recPosKeys[i], rRec(i));
        }

        tracepdeex(4, trace, "\nSPP apriori pos: %f %f %f", rRec(0), rRec(1), rRec(2));

        VectorPos pos = ecef2pos(rRec);
        if (pos.hgt() > 60'000'000)
        {
            tracepdeex(
                3,
                trace,
                "\n%s\tSPP found unfeasible position with height: %f",
                tsync.to_string().c_str(),
                pos.hgt()
            );

            return E_Solution::NONE;
        }

        KFMeasEntryList kfMeasEntryList;

        for (auto& obs : only<GObs>(obsList))
        {
            obs.sppValid = false;

            if (obs.exclude)
            {
                obs.failureExclude = true;

                tracepdeex(
                    2,
                    trace,
                    "\nSPP meas: sat=%s ... SPP exclusion: %x",
                    obs.Sat.id().c_str(),
                    obs.exclude
                );

                continue;
            }

            // Pseudorange with code bias correction
            double range;
            double vMeas;
            double vBias;
            bool   pass =
                prange(trace, obs, acsConfig.sppOpts.iono_mode, range, vMeas, vBias, kfState_ptr);
            if (pass == false)
            {
                obs.failurePrange = true;

                tracepdeex(
                    2,
                    trace,
                    "\nSPP meas: sat=%s ... Pseudorange fail",
                    obs.Sat.id().c_str()
                );

                continue;
            }

            tracepdeex(
                2,
                trace,
                "\nSPP meas: sat=%s, range+bias=%12.3f",
                obs.Sat.id().c_str(),
                range
            );

            // Sat pos
            Vector3d rSat = obs.rSatApc;
            if (obs.ephPosValid == false || rSat.isZero())
            {
                obs.failureNoSatPos = true;

                tracepdeex(2, trace, " ... Sat pos fail");

                continue;
            }

            SatStat& satStat = *obs.satStat_ptr;

            // Geodistance & line-of-sight vector
            double r = geodist(rSat, rRec, satStat.e);
            if (r <= 0)
            {
                obs.failureGeodist = true;

                tracepdeex(2, trace, " ... Geodist fail", obs.Sat.id().c_str());

                continue;
            }

            tracepdeex(2, trace, ", dist=%12.3f", r);

            // Elevation
            satazel(pos, satStat.e, satStat);

            double elevation = satStat.el * R2D;
            if (elevation < recOpts.elevation_mask_deg && adjustment < 1000)
            {
                obs.failureElevation = true;

                tracepdeex(2, trace, " ... Elevation mask fail");

                continue;
            }

            tracepdeex(2, trace, ", el=%5.2f", elevation);

            // Sat clock
            if (obs.ephClkValid == false)
            {
                obs.failureNoSatClock = true;

                tracepdeex(2, trace, " ... Sat clk fail");

                continue;
            }

            double dtSat = -obs.satClk * CLIGHT;

            tracepdeex(2, trace, ", satClk=%12.3f", dtSat);

            // Rec clock
            recSysBiasKey.Sat = SatSys(obs.Sat.sys);
            recSysBiasKey.str = id;
            kfState.getKFValue(recSysBiasKey, dtRec);

            tracepdeex(2, trace, ", recClk=%12.3f", dtRec);

            // Ionospheric correction
            double dIono;
            double vIono;
            pass = ionocorr(obs.time, pos, satStat, acsConfig.sppOpts.iono_mode, dIono, vIono);
            if (pass == false)
            {
                obs.failureIonocorr = true;

                tracepdeex(2, trace, " ... Iono fail");

                continue;
            }

            if (obs.Sat.sys == +E_Sys::GLO)
            {
                double lamG1 = obs.satNav_ptr->lamMap[G1];
                if (lamG1 > 0)
                    dIono *= SQR(lamG1 * FREQ1 / CLIGHT);
            }
            if (obs.Sat.sys == +E_Sys::BDS)
            {
                double lamB1 = obs.satNav_ptr->lamMap[B1];
                if (lamB1 > 0)
                    dIono *= SQR(lamB1 * FREQ1 / CLIGHT);
            }

            tracepdeex(2, trace, ", dIono=%9.5f", dIono);

            // Tropospheric correction
            double dryZTD;
            double dryMap;
            double wetZTD;
            double wetMap;
            double vTrop;
            double dTrop =
                tropSAAS(trace, obs.time, pos, satStat.el, dryZTD, dryMap, wetZTD, wetMap, vTrop);

            tracepdeex(2, trace, ", dTrop=%9.5f", dTrop);

            // Pseudorange residual
            double expected = r + dtSat + dtRec + dIono + dTrop;
            double res      = range - expected;

            tracepdeex(2, trace, ", res=%15.6f", res);

            // Error variance
            if (acsConfig.sppOpts.iono_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
                vMeas *= 3;

            double vSatPos = obs.posVar;
            double vSatClk = obs.satClkVar * SQR(CLIGHT);

            double var = vMeas + vBias + vSatPos + vSatClk + vIono + vTrop;
            var *= SQR(acsConfig.sppOpts.sigma_scaling);

            tracepdeex(
                5,
                trace,
                ", vMeas=%f, vBias=%f, vSatPos=%f, vSatClk=%f, vIono=%f, vTrop=%f, var=%f",
                vMeas,
                vBias,
                vSatPos,
                vSatClk,
                vIono,
                vTrop,
                var
            );

            // Calculate design entry and form up the measurement
            KFMeasEntry codeMeas(&kfState);

            for (short i = 0; i < 3; i++)
            {
                codeMeas.addDsgnEntry(recPosKeys[i], -satStat.e[i]);
            }
            {
                // 				codeMeas.addDsgnEntry(recClockKey,		1);
            }
            // 			if (recSysBiasKey.num != recClockKey.num)
            {
                codeMeas.addDsgnEntry(recSysBiasKey, 1);
            }

            codeMeas.obsKey.Sat  = obs.Sat;
            codeMeas.obsKey.str  = id;
            codeMeas.obsKey.num  = 0;  // todo Eugene: get sig code from prange
            codeMeas.obsKey.type = KF::CODE_MEAS;
            codeMeas.obsKey.comment =
                "SPP It " + std::to_string(iter);  // todo Eugene: use sig code

            codeMeas.setValue(res);
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

        removeUnmeasuredStates(trace, kfState, kfMeasEntryList);

        // Use state transition to initialise states
        kfState.stateTransition(trace, tsync);

        // Combine the measurement list into a single matrix
        numMeas = kfMeasEntryList.size();

        KFMeas kfMeas(kfState, kfMeasEntryList, tsync);

        if (kfMeas.H.cols() == 0)
        {
            BOOST_LOG_TRIVIAL(error) << "Error: " << __FUNCTION__ << ": no valid states on " << id
                                     << " after " << iter << " iterations";

            printFailures(id, obsList);

            tracepdeex(3, trace, "\nNo valid states, END OF SPP LSQ");

            return E_Solution::NONE;
        }

        if (numMeas < kfMeas.H.cols() - 1 || numMeas == 0)
        {
            BOOST_LOG_TRIVIAL(error)
                << "Error: " << __FUNCTION__ << ": lack of valid measurements nObs=" << numMeas
                << " on " << id << " after " << iter << " iterations." << " (has " << obsList.size()
                << " total observations)";

            printFailures(id, obsList);

            tracepdeex(3, trace, "\nLack of valid measurements, END OF SPP LSQ");

            return E_Solution::NONE;
        }

        if (rRec.isZero() && iter == 0)
        {
            kfMeas.Y /= 2;
        }

        // Least squares estimation
        VectorXd dx;
        kfState.leastSquareInitStates(trace, kfMeas, true, &dx);

        if (traceLevel >= 4)
        {
            kfMeas.V = kfMeas.Y;
            outputResiduals(trace, kfMeas, iter, suffix, 0, kfMeas.H.rows());
        }

        adjustment = dx.norm();
        tracepdeex(4, trace, "\nSPP dx: %15.4f\n", adjustment);

        if (adjustment < 4000 && acsConfig.sppOpts.postfitOpts.sigma_check &&
            removals < acsConfig.sppOpts.postfitOpts.max_iterations)
        {
            // Use 'array' for component-wise calculations
            auto measVariations = kfMeas.Y.array().square();  // delta squared

            auto measVariances = kfMeas.R.diagonal().array().max(SQR(adjustment));

            // trace << measVariances.sqrt();

            ArrayXd measRatios = measVariations / measVariances;
            measRatios         = measRatios.isFinite().select(measRatios, 0);

            // If any are outside the expected values, flag an error

            Eigen::ArrayXd::Index measIndex;

            // 			std::cout << "\nmeasRatios\n" << measRatios;

            double maxMeasRatio = measRatios.maxCoeff(&measIndex);

            if (maxMeasRatio > SQR(acsConfig.sppOpts.postfitOpts.meas_sigma_threshold))
            {
                trace << "\n"
                      << "LARGE MEAS  ERROR OF " << maxMeasRatio << " AT " << measIndex << " : "
                      << kfMeas.obsKeys[measIndex];

                GObs& badObs = *(GObs*)kfMeas.metaDataMaps[measIndex]["sppObs_ptr"];

                badObs.excludeOutlier = true;
                continue;
            }
        }

        if (adjustment < 1E-4)
        {
            sol.numMeas = numMeas;

            if (traceLevel >= 4)
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

            bool dopPass = validateDOP(trace, obsList, recOpts.elevation_mask_deg, &sol.dops);
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
        BOOST_LOG_TRIVIAL(error) << "Error: SPP failed to converge after " << iter
                                 << " iterations for " << id << " - " << description;
        tracepdeex(
            3,
            trace,
            "\n%s\tSPP failed to converge after %d iterations",
            tsync.to_string().c_str(),
            iter
        );

        if (traceLevel >= 5)
        {
            // Still output states if fails to converge
            kfState.outputStates(trace, suffix);
        }
    }

    return E_Solution::NONE;
}

/** Receiver autonomous integrity monitoring (RAIM) failure detection and exclution
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

    SatSys  exSat;
    ObsList testList = obsList;

    double bestVar = 10000;

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

        // Avoid lock-in for raim despite config
        candSol.sppState = KFState();

        // Try to get position using candidate subset of all observations
        E_Solution status = estpos(
            trace,
            candList,
            candSol,
            id,
            kfState_ptr,
            (string) "RAIM/" + id + "/" + testObs.Sat.id()
        );
        if (status != +E_Solution::SINGLE)
        {
            continue;
        }

        int    numSat  = 0;
        double candVar = 0;

        for (auto& obs : only<GObs>(testList))
        {
            if (obs.sppValid == false)
                continue;

            candVar += SQR(obs.sppCodeResidual);
            numSat++;
        }

        candVar = candVar / numSat;

        if (numSat < 5)
        {
            tracepdeex(
                3,
                trace,
                "\n%s: exSat=%s, lack of satellites nSat=%2d",
                __FUNCTION__,
                testObs.Sat.id().c_str(),
                numSat
            );

            continue;
        }

        tracepdeex(
            3,
            trace,
            "\n%s: exSat=%s, var=%8.3f",
            __FUNCTION__,
            testObs.Sat.id().c_str(),
            candVar
        );

        if (candVar > bestVar)
        {
            // This solution is worse
            continue;
        }

        // Copy best obs to real result
        for (auto& bestObs : only<GObs>(candList))
            for (auto& origObs : only<GObs>(obsList))
            {
                if (bestObs.Sat != origObs.Sat)
                {
                    // Only use the equivalent obs in the real list according to the best list
                    continue;
                }

                origObs.sppValid        = bestObs.sppValid;
                origObs.sppCodeResidual = bestObs.sppCodeResidual;
            }

        // Store 'best' satStats for later use
        backupSatStats(bestSatStats, true);

        sol              = candSol;
        sol.status       = E_Solution::SINGLE;
        exSat            = testObs.Sat;
        bestVar          = candVar;
        testObs.sppValid = false;
    }

    if ((int)exSat)
    {
        // Update satStats (AzEl and line-of-sight unit vector) w/ best SPP solution
        backupSatStats(bestSatStats, false);

        tracepdeex(
            3,
            trace,
            "\n%s\t%s excluded by RAIM",
            tsync.to_string().c_str(),
            exSat.id().c_str()
        );
        BOOST_LOG_TRIVIAL(debug) << exSat.id() << " was excluded from " << obsList.front()->mount
                                 << " by RAIM";

        BOOST_LOG_TRIVIAL(debug) << "SPP converged after RAIM";

        return true;
    }

    return false;
}

/** Compute receiver position, velocity, clock bias by single-point positioning with pseudorange
 * observables
 */
void spp(
    Trace&    trace,        ///< Trace file to output to
    ObsList&  obsList,      ///< List of observations for this epoch
    Solution& sol,          ///< Solution object containing initial state and results
    string    id,           ///< Id of receiver
    KFState*  kfState_ptr,  ///< Optional pointer to filter to take ephemerides from
    KFState*  remote_ptr    ///< Optional pointer to filter to take ephemerides from
)
{
    if (obsList.empty())
    {
        BOOST_LOG_TRIVIAL(error) << "Error: SPP failed due to no observation data on " << id;

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

    if (sol.status != +E_Solution::SINGLE)
    {
        // Receiver Autonomous Integrity Monitoring
        if (sol.numMeas >=
                6  // need 6 so that 6-1 is still overconstrained, otherwise they all pass equally.
            && acsConfig.sppOpts.raim)
        {
            raim(trace, obsList, sol, id);
        }
    }

    if (sol.status != +E_Solution::SINGLE)
    {
        BOOST_LOG_TRIVIAL(warning) << "Warning: SPP error for " << id;
        trace << "\n"
              << tsync << "\tSPP error for " << id << " with " << sol.numMeas << " measurements";
    }

    // Set observations that were valid
    for (auto& obs : only<GObs>(obsList))
    {
        if (sol.status != +E_Solution::SINGLE && sol.status != +E_Solution::SINGLE_X)
        {
            // All measurements are bad if we cant get SPP
            //  printf("\n all SPP bad");
            obs.excludeBadSPP = true;
            continue;
        }

        if (obs.exclude || obs.sppValid)
        {
            continue;
        }

        // printf("\n %s SPP bad", obs.Sat.id().c_str());
        obs.excludeBadSPP = true;
    }

    // Copy states to often-used vectors
    for (short i = 0; i < 3; i++)
    {
        sol.sppState.getKFValue({KF::REC_POS, {}, id, i}, sol.sppRRec[i]);
    }
    for (short i = E_Sys::GPS; i < +E_Sys::SUPPORTED; i++)
    {
        E_Sys sys = E_Sys::_values()[i];
        sol.sppState.getKFValue({KF::REC_SYS_BIAS, {sys}, id}, sol.dtRec_m[sys]);
    }

    sol.sppTime = tsync - sol.dtRec_m[E_Sys::GPS] / CLIGHT;

    tracepdeex(
        2,
        trace,
        "\n%s pos: %f %f %f",
        __FUNCTION__,
        sol.sppRRec[0],
        sol.sppRRec[1],
        sol.sppRRec[2]
    );
    tracepdeex(2, trace, "\n%s clk: %f\n", __FUNCTION__, sol.dtRec_m[E_Sys::GPS]);
}
