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

// Observation status for signal tracking
enum class E_ObsStatus
{
    OBSERVED,      // Signal was observed by receiver (both code and phase)
    CODE_ONLY,     // Only code measurement available
    PHASE_ONLY,    // Only phase measurement available (this is unlikely as code is demodulated first and then try to extract phase)
    MISSING,       // Signal was expected, and we have other signals for this sat, but this one was not in rinex
    NOT_TRACKED    // Satellite not observed at all (above elevation mask but not in observation data)
};

// Convert observation status to string
const char* obsStatusToString(E_ObsStatus status)
{
    switch (status)
    {
        case E_ObsStatus::OBSERVED:    return "OBSERVED";
        case E_ObsStatus::CODE_ONLY:   return "CODE_ONLY";
        case E_ObsStatus::PHASE_ONLY:  return "PHASE_ONLY";
        case E_ObsStatus::MISSING:     return "MISSING";
        case E_ObsStatus::NOT_TRACKED: return "NOT_TRACKED";
        default:                       return "UNKNOWN";
    }
}

struct SatelliteVisibility
{
    SatSys sat;
    double el = 0;      // Elevation in radians
    double az = 0;      // Azimuth in radians
    set<E_ObsCode> expectedSignals;
    bool wasObserved = false;
    GObs* obs = nullptr;  // Pointer to observation if satellite was observed
};

// Observation record for output
struct ObservationRecord
{
    GTime time;
    SatSys sat;
    string recId;
    E_ObsCode code;
    double P = 0;
    double L = 0;
    double snr = 0;
    double el_deg = 0;
    double az_deg = 0;
    E_ObsStatus status;
    string blockType;
};

// Output a single observation record to trace files
void obsRec(Trace& trace, Trace& jsonTrace, const ObservationRecord& rec)
{
    const char* statusStr = obsStatusToString(rec.status);
    GTime time = rec.time;  // Make mutable copy for traceJson (expects non-const reference)

    // Determine which values to show based on status
    bool hasCode = (rec.status == E_ObsStatus::OBSERVED || rec.status == E_ObsStatus::CODE_ONLY);
    bool hasPhase = (rec.status == E_ObsStatus::OBSERVED || rec.status == E_ObsStatus::PHASE_ONLY);
    bool hasSnr = (rec.status == E_ObsStatus::OBSERVED || rec.status == E_ObsStatus::CODE_ONLY || rec.status == E_ObsStatus::PHASE_ONLY);

    // Format values as strings (without width specifiers - will be applied in tracepdeex format)
    char pStr[32], lStr[32], sStr[32];
    if (hasCode)    snprintf(pStr, sizeof(pStr), "%.6f", rec.P);
    else            snprintf(pStr, sizeof(pStr), "%s", "NaN");

    if (hasPhase)   snprintf(lStr, sizeof(lStr), "%.6f", rec.L);
    else            snprintf(lStr, sizeof(lStr), "%s", "NaN");

    if (hasSnr)     snprintf(sStr, sizeof(sStr), "%.2f", rec.snr);
    else            snprintf(sStr, sizeof(sStr), "%s", "NaN");

    tracepdeex(
        0,
        trace,
        "\n%s: epoch= %s sat= %5s sig= %5s P= %16s L= %16s S= %8s el= %6.2f az= %6.2f block= %12s status= %s",
        __FUNCTION__,
        rec.time.to_string().c_str(),
        rec.sat.id().c_str(),
        rec.code._to_string(),
        pStr,
        lStr,
        sStr,
        rec.el_deg,
        rec.az_deg,
        rec.blockType.c_str(),
        statusStr
    );

    // Output JSON trace - always include all fields, use NaN for unavailable measurements
    traceJson(
        0,
        jsonTrace,
        time,
        {{"data", "observations"},
         {"Sat", rec.sat.id()},
         {"Rec", rec.recId},
         {"Sig", rec.code._to_string()}},
        {
            {"SNR", hasSnr ? rec.snr : std::nan("")},
            {"L", hasPhase ? rec.L : std::nan("")},
            {"P", hasCode ? rec.P : std::nan("")},
            {"D", 0.0},  // Not stored in record currently
            {"el", rec.el_deg},
            {"az", rec.az_deg},
            {"blockType", rec.blockType},
            {"status", statusStr}
        }
    );
}

// Classify observed signals vs expected signals
void classifySignals(
    GObs* obs,
    const set<E_ObsCode>& expectedSignals,
    double el_deg,
    double az_deg,
    vector<ObservationRecord>& records)
{
    set<E_ObsCode> observedSignals;
    string blockType = obs->Sat.blockType();

    // Collect OBSERVED signals
    for (auto& [ft, sigs] : obs->sigsLists)
    {
        for (auto& sig : sigs)
        {
            // Only process signals that are in expectedSignals (already filtered by code_priorities)
            if (expectedSignals.find(sig.code) == expectedSignals.end())
            {
                continue;
            }

            observedSignals.insert(sig.code);

            ObservationRecord rec;
            rec.time = obs->time;
            rec.sat = obs->Sat;
            rec.recId = obs->mount;
            rec.code = sig.code;
            rec.P = sig.P;
            rec.L = sig.L;
            rec.snr = sig.snr;
            rec.el_deg = el_deg;
            rec.az_deg = az_deg;

            // Determine status based on which measurements are available
            bool hasCode = (sig.P != 0);
            bool hasPhase = (sig.L != 0);

            if (hasCode && hasPhase)
            {
                rec.status = E_ObsStatus::OBSERVED;
            }
            else if (hasCode && !hasPhase)
            {
                rec.status = E_ObsStatus::CODE_ONLY;
            }
            else if (!hasCode && hasPhase)
            {
                rec.status = E_ObsStatus::PHASE_ONLY;
            }
            else
            {
                rec.status = E_ObsStatus::MISSING;
            }

            rec.blockType = blockType;
            records.push_back(rec);
        }
    }

    // Collect MISSING signals (expected but not observed)
    for (auto& expectedCode : expectedSignals)
    {
        if (observedSignals.find(expectedCode) == observedSignals.end())
        {
            ObservationRecord rec;
            rec.time = obs->time;
            rec.sat = obs->Sat;
            rec.recId = obs->mount;
            rec.code = expectedCode;
            rec.el_deg = el_deg;
            rec.az_deg = az_deg;
            rec.status = E_ObsStatus::MISSING;
            rec.blockType = blockType;
            records.push_back(rec);
        }
    }
}

// Create NOT_TRACKED records for a satellite that was not observed
void createNotTrackedRecords(
    GTime time,
    SatSys sat,
    string recId,
    const set<E_ObsCode>& expectedSignals,
    double el_deg,
    double az_deg,
    vector<ObservationRecord>& records)
{
    string blockType = sat.blockType();

    for (auto& expectedCode : expectedSignals)
    {
        ObservationRecord rec;
        rec.time = time;
        rec.sat = sat;
        rec.recId = recId;
        rec.code = expectedCode;
        rec.el_deg = el_deg;
        rec.az_deg = az_deg;
        rec.status = E_ObsStatus::NOT_TRACKED;
        rec.blockType = blockType;
        records.push_back(rec);
    }
}

// Compute satellite position and elevation/azimuth
// Returns true if position was successfully computed
bool computeSatellitePosition(
    Trace& trace,
    GTime time,
    SatSys sat,
    SatNav& satNav,
    Receiver& rec,
    VectorPos& recPos,
    GObs* obs,  // If satellite was observed, use existing obs; otherwise nullptr
    double& el,
    double& az)
{
    auto& satStat = rec.satStatMap[sat];
    auto& satOpts = acsConfig.getSatOpts(sat);

    // If satellite was observed, use pre-computed position
    if (obs && obs->satStat_ptr)
    {
        el = obs->satStat_ptr->el;
        az = obs->satStat_ptr->az;
        return true;
    }

    // Satellite not observed - compute position ourselves
    //  For NOT_TRACKED detection, we only need satellite position, not clock or pseudorange
    SatPos satPos = {};
    satPos.Sat = sat;
    satPos.satNav_ptr = &satNav;

    bool posFound = satpos(
        trace,
        time,
        time,  // teph = time
        satPos,
        satOpts.posModel.sources,
        E_OffsetType::APC,
        nav
    );

    if (!posFound || satPos.rSatApc.isZero())
    {
        return false;
    }

    Vector3d rSat = satPos.rSatApc;

    Vector3d e;
    double r = geodist(rSat, rec.aprioriPos, e);
    satazel(recPos, e, satStat);
    el = satStat.el;
    az = satStat.az;
    return true;
}

// Determine expected signals for a satellite-receiver pair
// Returns the intersection of: (satellite frequencies) AND (receiver tracked signals) AND (code priorities)
set<E_ObsCode> determineExpectedSignals(
    SatSys sat,
    Receiver& rec)
{
    set<E_ObsCode> expectedSignals;

    // Get satellite block type and convert to enum
    string blockTypeStr = sat.blockType();
    if (blockTypeStr.empty())
    {
        return expectedSignals;  // Unknown block type
    }

    string enumStr = blockTypeStr;
    std::replace(enumStr.begin(), enumStr.end(), '-', '_');

    auto blockOpt = E_Block::_from_string_nothrow(enumStr.c_str());
    if (!blockOpt)
    {
        return expectedSignals;  // Unknown block type
    }

    // Get frequencies that this satellite broadcasts
    auto freqIt = blockTypeFrequencies.find(*blockOpt);
    if (freqIt == blockTypeFrequencies.end())
    {
        return expectedSignals;  // Block type not found
    }

    vector<E_FType> satFrequencies = freqIt->second;

    // Convert to set for faster lookup
    set<E_FType> satFreqSet(satFrequencies.begin(), satFrequencies.end());

    // Get receiver's tracked signals for this constellation
    auto trackedIt = rec.trackedSignals.find(sat.sys);
    if (trackedIt == rec.trackedSignals.end())
    {
        return expectedSignals;
    }

    auto& receiverSignals = trackedIt->second;
    auto& codePriorities = acsConfig.code_priorities[sat.sys];

    // Get code2Freq map for this constellation
    auto sysCodeIt = code2Freq.find(sat.sys);
    if (sysCodeIt == code2Freq.end())
    {
        return expectedSignals;
    }

    // Find receiver signals that match satellite frequencies AND are in code priorities
    for (auto& recSig : receiverSignals)
    {
        // Use code2Freq to get frequency type for this signal
        auto codeIt = sysCodeIt->second.find(recSig);
        if (codeIt != sysCodeIt->second.end())
        {
            E_FType sigFreq = codeIt->second;

            // Check if satellite broadcasts this frequency AND signal is in code priorities
            bool inSatFreqs = satFreqSet.count(sigFreq);
            bool inCodePriorities = std::find(codePriorities.begin(), codePriorities.end(), recSig) != codePriorities.end();

            // Filter out signals not supported by this block type (e.g., L2C not on older GPS blocks)
            // This prevents false "MISSING" reports for signals that a satellite block type
            // physically cannot transmit.
            bool supportedByBlock = isSignalSupportedByBlockType(recSig, *blockOpt);

            if (inSatFreqs && inCodePriorities && supportedByBlock)
            {
                expectedSignals.insert(recSig);
            }
        }
    }

    return expectedSignals;
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

        // Check if satellite was observed
        auto obsIt = observedSatMap.find(sat);
        GObs* obs = (obsIt != observedSatMap.end()) ? obsIt->second : nullptr;

        // Compute satellite position and elevation/azimuth
        double el = 0, az = 0;
        if (!computeSatellitePosition(trace, time, sat, satNav, rec, recPos, obs, el, az))
        {
            continue;  // Position not available
        }

        // Determine expected signals for this satellite-receiver pair
        set<E_ObsCode> expectedSignals = determineExpectedSignals(sat, rec);
        if (expectedSignals.empty())
        {
            continue;  // No matching signals
        }

        // For satellites NOT observed, only output if above elevation mask
        if (!obs && el < elevationMask)
        {
            continue;
        }

        double el_deg = el * R2D;
        double az_deg = az * R2D;

        // Classify signals and create observation records
        vector<ObservationRecord> records;

        if (obs)
        {
            // Satellite was observed - classify as OBSERVED or MISSING
            classifySignals(obs, expectedSignals, el_deg, az_deg, records);
        }
        else
        {
            // Satellite was NOT observed - all expected signals are NOT_TRACKED
            createNotTrackedRecords(time, sat, rec.id, expectedSignals, el_deg, az_deg, records);
        }

        // Output all records to trace files
        for (const auto& record : records)
        {
            obsRec(trace, jsonTrace, record);
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
