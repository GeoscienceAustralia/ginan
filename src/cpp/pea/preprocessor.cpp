// #pragma GCC optimize ("O0")

#include <algorithm>
#include <map>
#include <set>
#include "ambres/GNSSambres.hpp"
#include "architectureDocs.hpp"
#include "common/acsConfig.hpp"
#include "common/acsQC.hpp"
#include "common/constants.hpp"
#include "common/navigation.hpp"
#include "common/observations.hpp"
#include "common/receiver.hpp"
#include "common/satStat.hpp"
#include "common/sinex.hpp"
#include "common/trace.hpp"
#include "orbprop/coordinates.hpp"
#include "pea/ppp.hpp"

/** Use linear combinations of primary signals to detect jumps in carrier phase observations
 */
Architecture Cycle_Slip_Detection__() {}

/** Perform basic quality checks on observations
 */
Architecture Preprocessing__()
{
    DOCS_REFERENCE(Cycle_Slip_Detection__);
    DOCS_REFERENCE(SPP__);
}

#include "ambres/GNSSambres.hpp"
#include "common/acsConfig.hpp"
#include "common/acsQC.hpp"
#include "common/constants.hpp"
#include "common/mongoWrite.hpp"
#include "common/navigation.hpp"
#include "common/observations.hpp"
#include "common/receiver.hpp"
#include "common/satStat.hpp"
#include "common/sinex.hpp"
#include "common/trace.hpp"
#include "orbprop/coordinates.hpp"
#include "pea/ppp.hpp"

// Get frequency bands that a satellite block type broadcasts
// Returns frequencies as integers (e.g., 1, 2, 5 for L1, L2, L5)
vector<int> getExpectedFrequencies(string blockType)
{
    vector<int> frequencies;

    // GPS Block types
    if (blockType == "GPS-IIF" || blockType == "GPS-IIIA")
    {
        // L1, L2, L5
        frequencies = {1, 2, 5};
    }
    else if (blockType == "GPS-IIR-M")
    {
        // L1, L2 (no L5)
        frequencies = {1, 2};
    }
    else if (blockType == "GPS-IIR-A" || blockType == "GPS-IIR-B" || blockType == "GPS-IIA" || blockType == "GPS-II")
    {
        // L1, L2 only
        frequencies = {1, 2};
    }
    // Galileo
    else if (blockType.find("GAL") == 0)
    {
        // E1, E5a, E5b, E5, E6
        frequencies = {1, 5, 7, 8, 6};
    }
    // GLONASS
    else if (blockType.find("GLO") == 0)
    {
        // G1, G2
        frequencies = {1, 2};
    }
    // BeiDou
    else if (blockType.find("BDS") == 0)
    {
        // B1, B2, B3, B2a
        frequencies = {1, 2, 6, 7};
    }
    // QZSS
    else if (blockType.find("QZS") == 0)
    {
        // L1, L2, L5
        frequencies = {1, 2, 5};
    }

    return frequencies;
}

void outputObservations(Trace& trace, Trace& jsonTrace, ObsList& obsList, Receiver& rec, VectorPos& recPos)
{
    if (obsList.empty())
    {
        return;
    }

    GTime time = obsList.front()->time;
    auto& recOpts = acsConfig.getRecOpts(rec.id);
    double elevationMask = recOpts.elevation_mask_deg * D2R;

    // Build map of satellites that were observed in RINEX
    map<SatSys, GObs*> observedSatMap;
    for (auto& obs : only<GObs>(obsList))
    {
        if (obs.exclude == false)
        {
            observedSatMap[obs.Sat] = &obs;
        }
    }

    // Iterate through ALL satellites with available ephemeris
    for (auto& [sat, satNav] : nav.satNavMap)
    {
        // Check if this satellite system is being processed
        if (acsConfig.process_sys[sat.sys] == false)
        {
            continue;
        }

        auto& satOpts = acsConfig.getSatOpts(sat);
        if (satOpts.exclude)
        {
            continue;
        }

        // Get or create satellite status for this receiver
        auto& satStat = rec.satStatMap[sat];

        // Compute satellite position and elevation if not already done
        double el = 0;
        double az = 0;
        bool positionAvailable = false;

        // Check if satellite was in observation list (position already computed)
        auto obsIt = observedSatMap.find(sat);
        if (obsIt != observedSatMap.end())
        {
            GObs* obs = obsIt->second;
            if (obs->satStat_ptr)
            {
                el = obs->satStat_ptr->el;
                az = obs->satStat_ptr->az;
                positionAvailable = true;
            }
        }
        else
        {
            // Satellite not in RINEX - need to compute position ourselves
            GObs tempObs = {};
            tempObs.Sat = sat;
            tempObs.time = time;
            tempObs.mount = rec.id;
            tempObs.rec_ptr = &rec;
            tempObs.satNav_ptr = &satNav;
            tempObs.satStat_ptr = &satStat;

            updateLamMap(time, tempObs);

            satPosClk(
                trace,
                time,
                tempObs,
                nav,
                satOpts.posModel.sources,
                satOpts.clockModel.sources,
                nullptr,
                nullptr,
                E_OffsetType::APC
            );

            Vector3d rSat = tempObs.rSatApc;
            if (rSat.isZero() == false)
            {
                Vector3d e;
                double r = geodist(rSat, rec.aprioriPos, e);
                satazel(recPos, e, satStat);
                el = satStat.el;
                az = satStat.az;
                positionAvailable = true;
            }
        }

        // Skip if position not available
        if (positionAvailable == false)
        {
            continue;
        }

        // Get frequency bands that this satellite broadcasts
        string blockType = sat.blockType();
        if (blockType.empty())
        {
            continue;  // Unknown block type, can't determine frequencies
        }

        vector<int> satFrequencies = getExpectedFrequencies(blockType);
        if (satFrequencies.empty())
        {
            continue;  // Unknown block type, can't determine frequencies
        }

        // Map frequencies to receiver's tracked signals
        // Expected signals = intersection of (satellite frequencies) AND (receiver tracked signals) AND (code_priorities)
        set<E_ObsCode> expectedSignals;  // Use set to avoid duplicates

        // Get receiver's tracked signals for this constellation
        auto trackedIt = rec.trackedSignals.find(sat.sys);
        if (trackedIt != rec.trackedSignals.end())
        {
            auto& receiverSignals = trackedIt->second;
            auto& codePriorities = acsConfig.code_priorities[sat.sys];

            // For each frequency the satellite broadcasts
            for (auto freq : satFrequencies)
            {
                char freqChar = '0' + freq;  // Convert int to char digit (e.g., 1 -> '1')

                // Find matching signals in receiver's tracked list that match this frequency
                for (auto& recSig : receiverSignals)
                {
                    string sigStr = recSig._to_string();

                    // Check if signal matches frequency (e.g., L1C has freq '1', L2W has freq '2')
                    // Signal format is like "L1C" where position [1] is the frequency digit
                    if (sigStr.length() >= 2 && sigStr[1] == freqChar)
                    {
                        // Also check if it's in code_priorities
                        if (std::find(codePriorities.begin(), codePriorities.end(), recSig) != codePriorities.end())
                        {
                            expectedSignals.insert(recSig);
                        }
                    }
                }
            }
        }

        if (expectedSignals.empty())
        {
            continue;  // No matching signals between satellite frequencies, receiver capabilities, and code_priorities
        }

        double el_deg = el * R2D;
        double az_deg = az * R2D;

        // Check if satellite was observed
        bool wasObserved = (obsIt != observedSatMap.end());

        // For satellites NOT observed in RINEX, only output if above elevation mask
        if (wasObserved == false && el < elevationMask)
        {
            continue;
        }

        if (wasObserved)
        {
            // Satellite was observed - output observed signals and missing signals
            GObs* obs = obsIt->second;

            // Track which signals we've observed
            set<E_ObsCode> observedSignals;

            // Output observed signals (filtered by code_priorities)
            for (auto& [ft, sigs] : obs->sigsLists)
                for (auto& sig : sigs)
                {
                    // Check if this signal is in expectedSignals (already filtered by code_priorities)
                    if (std::find(expectedSignals.begin(), expectedSignals.end(), sig.code) == expectedSignals.end())
                    {
                        continue;  // Skip signals not in code_priorities
                    }

                    observedSignals.insert(sig.code);

                    tracepdeex(
                        4,
                        trace,
                        "\n%s %5s %5s %14.4f %14.4f %8.2f %6.2f %6.2f %s",
                        obs->time.to_string().c_str(),
                        obs->Sat.id().c_str(),
                        sig.code._to_string(),
                        sig.P,
                        sig.L,
                        sig.snr,
                        el_deg,
                        az_deg,
                        "OBSERVED"
                    );

                    traceJson(
                        4,
                        jsonTrace,
                        obs->time,
                        {{"data", "observations"},
                         {"Sat", obs->Sat.id()},
                         {"Rec", obs->mount},
                         {"Sig", sig.code._to_string()}},
                        {
                            {"SNR", sig.snr},
                            {"L", sig.L},
                            {"P", sig.P},
                            {"D", sig.D},
                            {"el", el_deg},
                            {"az", az_deg},
                            {"status", "OBSERVED"}
                        }
                    );
                }

            // Output MISSING signals (expected but not observed)
            for (auto& expectedCode : expectedSignals)
            {
                if (observedSignals.find(expectedCode) == observedSignals.end())
                {
                    tracepdeex(
                        4,
                        trace,
                        "\n%s %5s %5s %14s %14s %8s %6.2f %6.2f %s",
                        obs->time.to_string().c_str(),
                        obs->Sat.id().c_str(),
                        expectedCode._to_string(),
                        "NaN",
                        "NaN",
                        "NaN",
                        el_deg,
                        az_deg,
                        "MISSING"
                    );

                    traceJson(
                        4,
                        jsonTrace,
                        obs->time,
                        {{"data", "observations"},
                         {"Sat", obs->Sat.id()},
                         {"Rec", obs->mount},
                         {"Sig", expectedCode._to_string()}},
                        {
                            {"el", el_deg},
                            {"az", az_deg},
                            {"status", "MISSING"}
                        }
                    );
                }
            }
        }
        else
        {
            // Satellite was NOT observed at all - output all expected signals as MISSING
            for (auto& expectedCode : expectedSignals)
            {
                tracepdeex(
                    4,
                    trace,
                    "\n%s %5s %5s %14s %14s %8s %6.2f %6.2f %s",
                    time.to_string().c_str(),
                    sat.id().c_str(),
                    expectedCode._to_string(),
                    "NaN",
                    "NaN",
                    "NaN",
                    el_deg,
                    az_deg,
                    "NOT_TRACKED"
                );

                traceJson(
                    4,
                    jsonTrace,
                    time,
                    {{"data", "observations"},
                     {"Sat", sat.id()},
                     {"Rec", rec.id},
                     {"Sig", expectedCode._to_string()}},
                    {
                        {"el", el_deg},
                        {"az", az_deg},
                        {"status", "NOT_TRACKED"}
                    }
                );
            }
        }
    }
}

void obsVariances(ObsList& obsList)
{
    for (auto& obs : only<GObs>(obsList))
        if (obs.satNav_ptr)
            if (obs.satStat_ptr)
                if (obs.exclude == false)
                    if (acsConfig.process_sys[obs.Sat.sys])
                    {
                        auto& recOpts = acsConfig.getRecOpts(obs.mount);
                        auto& satOpts = acsConfig.getSatOpts(obs.Sat);

                        double el = obs.satStat_ptr->el;
                        if (el == 0)
                            el = PI / 8;

                        double recElScaling = 1;
                        switch (recOpts.error_model)
                        {
                            case E_NoiseModel::UNIFORM:
                            {
                                recElScaling = 1;
                                break;
                            }
                            case E_NoiseModel::ELEVATION_DEPENDENT:
                            {
                                recElScaling = 1 / sin(el);
                                break;
                            }
                        }

                        double satElScaling = 1;
                        switch (satOpts.error_model)
                        {
                            case E_NoiseModel::UNIFORM:
                            {
                                satElScaling = 1;
                                break;
                            }
                            case E_NoiseModel::ELEVATION_DEPENDENT:
                            {
                                satElScaling = 1 / sin(el);
                                break;
                            }
                        }

                        for (auto& [ft, sig] : obs.sigs)
                        {
                            if (sig.P == 0)
                                continue;

                            string sigName = sig.code._to_string();

                            auto& satOpts = acsConfig.getSatOpts(obs.Sat, {sigName});
                            auto& recOpts = acsConfig.getRecOpts(
                                obs.mount,
                                {obs.Sat.sys._to_string(), sigName}
                            );

                            sig.codeVar = 0;
                            sig.phasVar = 0;

                            sig.codeVar += SQR(recElScaling * recOpts.code_sigma);
                            sig.codeVar += SQR(satElScaling * satOpts.code_sigma);
                            sig.phasVar += SQR(recElScaling * recOpts.phase_sigma);
                            sig.phasVar += SQR(satElScaling * satOpts.phase_sigma);
                        }

                        for (auto& [ft, sigList] : obs.sigsLists)
                            for (auto& sig : sigList)
                            {
                                string sigName = sig.code._to_string();

                                auto& satOpts = acsConfig.getSatOpts(obs.Sat, {sigName});
                                auto& recOpts = acsConfig.getRecOpts(
                                    obs.mount,
                                    {obs.Sat.sys._to_string(), sigName}
                                );

                                sig.codeVar = 0;
                                sig.phasVar = 0;

                                sig.codeVar += SQR(recElScaling * recOpts.code_sigma);
                                sig.codeVar += SQR(satElScaling * satOpts.code_sigma);
                                sig.phasVar += SQR(recElScaling * recOpts.phase_sigma);
                                sig.phasVar += SQR(satElScaling * satOpts.phase_sigma);
                            }
                    }
}

void excludeUnprocessed(ObsList& obsList)
{
    for (auto& obs : only<GObs>(obsList))
    {
        if (acsConfig.process_sys[obs.Sat.sys] == false)
        {
            obs.excludeSystem = true;
        }
    }
}

void recordSlips(Receiver& rec)
{
    for (auto& obs : only<GObs>(rec.obsList))
    {
        for (auto& [ft, sig] : obs.sigs)
        {
            if (obs.satStat_ptr)
            {
                SigStat& sigStat = obs.satStat_ptr->sigStatMap[ft2string(ft)];

                if (sigStat.slip.any &&
                    ((acsConfig.exclude.LLI && sigStat.slip.LLI) ||
                     (acsConfig.exclude.GF && sigStat.slip.GF) ||
                     (acsConfig.exclude.retrack && sigStat.slip.retrack) ||
                     (acsConfig.exclude.single_freq && sigStat.slip.singleFreq) ||
                     (acsConfig.exclude.MW && sigStat.slip.MW) ||
                     (acsConfig.exclude.SCDIA && sigStat.slip.SCDIA)))
                {
                    rec.savedSlips[obs.Sat] = obs.time;
                    mongoEditing(
                        obs.Sat.id(),
                        rec.id,
                        obs.time,
                        "PreprocSlip",
                        sig.code._to_string(),
                        static_cast<int>(sigStat.slip.any)
                    );
                }
            }
        }
        if (obs.satStat_ptr)
        {
            double gf0 = obs.satStat_ptr->gf;
            double mw0 = obs.satStat_ptr->mw;
            mongoEditing(obs.Sat.id(), rec.id, obs.time, "gf", "", gf0);
            mongoEditing(obs.Sat.id(), rec.id, obs.time, "mw", "", mw0);
        }
    }
}

void preprocessor(
    Trace&    trace,
    Receiver& rec,
    bool      realEpoch,
    KFState*  kfState_ptr,  ///< Optional pointer to filter to take ephemerides from
    KFState*  remote_ptr    ///< Optional pointer to filter to take ephemerides from
)
{
    DOCS_REFERENCE(Preprocessing__);

    if ((acsConfig.process_preprocessor == false) ||
        (acsConfig.preprocOpts.preprocess_all_data == true && realEpoch == true) ||
        (acsConfig.preprocOpts.preprocess_all_data == false && realEpoch == false))
    {
        return;
    }

    auto jsonTrace = getTraceFile(rec, true);

    auto& recOpts = acsConfig.getRecOpts(rec.id);

    auto& obsList = rec.obsList;

    if (obsList.empty())
    {
        return;
    }

    PTime start_time;
    start_time.bigTime = boost::posix_time::to_time_t(acsConfig.start_epoch);

    double tol;
    if (acsConfig.assign_closest_epoch)
        tol = acsConfig.epoch_interval / 2;  // todo aaron this should be the epoch_tolerance?
    else
        tol = 0.5;

    GTime time = obsList.front()->time;
    if (acsConfig.start_epoch.is_not_a_date_time() == false && time < (GTime)start_time - tol)
    {
        return;
    }

    getRecSnx(rec.id, time, rec.snx);

    bool dummy;
    updateAprioriRecPos(trace, rec, recOpts, dummy, remote_ptr);

    VectorPos pos = ecef2pos(rec.aprioriPos);

    // prepare and connect navigation objects to the observations
    for (auto& obs : only<GObs>(obsList))
    {
        obs.mount = rec.id;

        if (acsConfig.process_sys[obs.Sat.sys] == false)
        {
            obs.excludeSystem = true;

            continue;
        }

        auto& satOpts = acsConfig.getSatOpts(obs.Sat);

        if (satOpts.exclude)
        {
            obs.excludeConfig = true;

            continue;
        }

        auto& satNav  = nav.satNavMap[obs.Sat];
        auto& satStat = rec.satStatMap[obs.Sat];

        obs.rec_ptr     = &rec;
        obs.satNav_ptr  = &satNav;
        obs.satStat_ptr = &satStat;

        updateLamMap(obs.time, obs);

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

        Vector3d rSat = obs.rSatApc;
        if (rSat.isZero())
        {
            obs.failureRSat = true;

            continue;
        }

        double r = geodist(rSat, rec.aprioriPos, satStat.e);

        satazel(pos, satStat.e, satStat);
    }

    clearSlips(obsList);

    excludeUnprocessed(obsList);

    if (acsConfig.output_observations)
    {
        outputObservations(trace, jsonTrace, obsList, rec, pos);
    }

    /* linear combinations */
    for (auto& obs : only<GObs>(obsList))
        if (obs.satStat_ptr)
        {
            obs.satStat_ptr->lc_pre = obs.satStat_ptr->lc_new;
            obs.satStat_ptr->lc_new = {};
        }
    obs2lcs(trace, obsList);
    obsVariances(obsList);
    detectslips(trace, obsList);

    recordSlips(rec);

    for (auto& obs : only<GObs>(obsList))
        for (auto& [ft, Sig] : obs.sigs)
            if (obs.satStat_ptr)
            {
                if (obs.satStat_ptr->sigStatMap[ft2string(ft)].slip.any)
                {
                    rec.slipCount++;
                    break;
                }
            }
}
