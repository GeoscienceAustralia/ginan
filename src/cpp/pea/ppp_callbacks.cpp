// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>
#include <sstream>
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/receiver.hpp"
#include "common/satStat.hpp"
#include "pea/ppp.hpp"

using std::ostringstream;

/** Deweight worst measurement
 */
bool deweightMeas(RejectCallbackDetails rejectDetails)
{
    auto& trace     = rejectDetails.trace;
    auto& kfState   = rejectDetails.kfState;
    auto& kfMeas    = rejectDetails.kfMeas;
    auto& measIndex = rejectDetails.measIndex;
    auto& stage     = rejectDetails.stage;

    if (acsConfig.measErrors.enable == false)
    {
        BOOST_LOG_TRIVIAL(warning)
            << "Warning: Bad measurement detected but `meas_deweighting` not enabled";

        return true;
    }

    double deweightFactor = acsConfig.measErrors.deweight_factor;

    auto& key = kfMeas.obsKeys[measIndex];

    double preSigma = sqrt(kfMeas.R(measIndex, measIndex));
    double residual = 0;

    string description;
    if (stage == E_FilterStage::LSQ)
    {
        description = "Least Squares";
        residual    = kfMeas.VV(measIndex);
    }
    else if (stage == E_FilterStage::PREFIT)
    {
        description = "Prefit";
        residual    = kfMeas.V(measIndex);
    }
    else if (stage == E_FilterStage::POSTFIT)
    {
        description = "Postfit";
        residual    = kfMeas.VV(measIndex);
    }

    addRejectDetails(
        kfState.time,
        trace,
        kfState,
        key,
        "Measurement Deweighted",
        description,
        {{description + "Residual", residual},
         {"preDeweightSigma", preSigma},
         {"postDeweightSigma", preSigma * deweightFactor}}
    );

    kfMeas.R.row(measIndex) *= deweightFactor;
    kfMeas.R.col(measIndex) *= deweightFactor;
    if (kfMeas.H_star.size() > 0)
        kfMeas.H_star.row(measIndex) *= deweightFactor;

    map<string, void*>& metaDataMap = kfMeas.metaDataMaps[measIndex];

    GObs* obs_ptr = (GObs*)metaDataMap["sppObs_ptr"];

    if (obs_ptr)
    {
        GObs& obs = *obs_ptr;

        obs.excludeOutlier = true;  // todo Eugene: exclude signal instead of obs

        trace << "\n" << obs.Sat.id() << " will be excluded next SPP iteration";
    }

    MatrixXd* otherNoiseMatrix_ptr = (MatrixXd*)metaDataMap["otherNoiseMatrix_ptr"];
    long int  otherIndex           = (long int)metaDataMap["otherIndex"];

    if (otherNoiseMatrix_ptr)
    {
        // some measurements have noise matrices in 2 places (mincon) - update the other one too if
        // its available.
        auto& otherNoiseMatrix = *otherNoiseMatrix_ptr;

        otherNoiseMatrix.row(otherIndex) *= deweightFactor;
        otherNoiseMatrix.col(otherIndex) *= deweightFactor;
    }

    return false;
}

/** Call state rejection functions when a measurement is a pseudo observation
 */
bool pseudoMeasTest(RejectCallbackDetails rejectDetails)
{
    auto& kfState   = rejectDetails.kfState;
    auto& kfMeas    = rejectDetails.kfMeas;
    auto& measIndex = rejectDetails.measIndex;

    if (kfMeas.metaDataMaps[measIndex]["pseudoObs"] == (void*)false)
    {
        return true;
    }

    for (auto& [key, stateIndex] : kfState.kfIndexMap)
    {
        if (kfMeas.H(measIndex, stateIndex) &&
            key.type == KF::ORBIT)  // Eugene: do this to other pseudoObs as well?
        {
            rejectDetails.kfKey      = key;
            rejectDetails.stateIndex = stateIndex;

            return relaxState(rejectDetails);
        }
    }

    return false;
}

/** Deweight measurement and its relatives
 */
bool deweightStationMeas(RejectCallbackDetails rejectDetails)
{
    auto& trace     = rejectDetails.trace;
    auto& kfState   = rejectDetails.kfState;
    auto& kfMeas    = rejectDetails.kfMeas;
    auto& measIndex = rejectDetails.measIndex;
    auto& stage     = rejectDetails.stage;

    string id = kfMeas.obsKeys[measIndex].str;

    for (int i = 0; i < kfMeas.obsKeys.size(); i++)
    {
        auto& key = kfMeas.obsKeys[i];

        if (key.str != id)
        {
            continue;
        }

        double deweightFactor = acsConfig.measErrors.deweight_factor;

        string description;
        if (stage == E_FilterStage::LSQ)
        {
            description = "Least Squares";
        }
        else if (stage == E_FilterStage::PREFIT)
        {
            description = "Prefit";
        }
        else if (stage == E_FilterStage::POSTFIT)
        {
            description = "Postfit";
        }

        addRejectDetails(kfState.time, trace, kfState, key, "Station Meas Deweighted", description);

        kfMeas.R.row(i) *= deweightFactor;
        kfMeas.R.col(i) *= deweightFactor;
        if (kfMeas.H_star.size() > 0)
            kfMeas.H_star.row(i) *= deweightFactor;

        map<string, void*>& metaDataMap = kfMeas.metaDataMaps[i];

        bool* used_ptr = (bool*)metaDataMap["used_ptr"];

        if (used_ptr)
        {
            *used_ptr = false;
        }

        MatrixXd* otherNoiseMatrix_ptr = (MatrixXd*)metaDataMap["otherNoiseMatrix_ptr"];
        long int  otherIndex           = (long int)metaDataMap["otherIndex"];

        if (otherNoiseMatrix_ptr)
        {
            // some measurements have noise matrices in 2 places (mincon) - update the other one too
            // if its available.
            auto& otherNoiseMatrix = *otherNoiseMatrix_ptr;

            otherNoiseMatrix.row(otherIndex) *= deweightFactor;
            otherNoiseMatrix.col(otherIndex) *= deweightFactor;
        }
    }

    return false;
}

/** Count worst measurement
 */
bool incrementPhaseSignalError(RejectCallbackDetails rejectDetails)
{
    auto& trace     = rejectDetails.trace;
    auto& kfState   = rejectDetails.kfState;
    auto& kfMeas    = rejectDetails.kfMeas;
    auto& measIndex = rejectDetails.measIndex;

    map<string, void*>& metaDataMap = kfMeas.metaDataMaps[measIndex];

    for (auto suffix : {"", "_alt"})
    {
        string metaData = "phaseRejectCount";
        metaData += suffix;

        unsigned int* phaseRejectCount_ptr = (unsigned int*)metaDataMap[metaData];

        if (phaseRejectCount_ptr == nullptr)
        {
            continue;
        }

        unsigned int& phaseRejectCount = *phaseRejectCount_ptr;

        // Increment counter, and clear the pointer so it cant be reset to zero in subsequent
        // operations (because this is a failure)
        phaseRejectCount++;
        metaDataMap[metaData] = nullptr;

        trace << "\n"
              << kfState.time << "\tIncrementing phaseRejectCount     on\t"
              << kfMeas.obsKeys[measIndex] << "\tto " << phaseRejectCount;
    }

    return true;
}

/** Count all errors on receiver
 */
bool incrementReceiverErrors(RejectCallbackDetails rejectDetails)
{
    auto& trace     = rejectDetails.trace;
    auto& kfState   = rejectDetails.kfState;
    auto& kfMeas    = rejectDetails.kfMeas;
    auto& measIndex = rejectDetails.measIndex;

    if (acsConfig.errorAccumulation.enable == false)
    {
        return true;
    }

    map<string, void*>& metaDataMap = kfMeas.metaDataMaps[measIndex];

    string metaData = "receiverErrorCount";

    unsigned int* receiverErrorCount_ptr = (unsigned int*)metaDataMap[metaData];

    if (receiverErrorCount_ptr == nullptr)
    {
        return true;
    }

    unsigned int& receiverErrorCount = *receiverErrorCount_ptr;

    // Increment counter, and clear the pointer so it wont increment again at current epoch
    receiverErrorCount++;
    metaDataMap[metaData] = nullptr;

    char idStr[100];
    snprintf(
        idStr,
        sizeof(idStr),
        "%10s\t%4s\t%4s\t%5s",
        "",
        "",
        kfMeas.obsKeys[measIndex].str.c_str(),
        ""
    );

    trace << "\n"
          << kfState.time << "\tIncrementing receiverErrorCount   on\t" << idStr << "\tto "
          << receiverErrorCount;

    return true;
}

/** Count all errors on satellite
 */
bool incrementSatelliteErrors(RejectCallbackDetails rejectDetails)
{
    auto& trace     = rejectDetails.trace;
    auto& kfState   = rejectDetails.kfState;
    auto& kfMeas    = rejectDetails.kfMeas;
    auto& measIndex = rejectDetails.measIndex;

    if (acsConfig.errorAccumulation.enable == false)
    {
        return true;
    }

    map<string, void*>& metaDataMap = kfMeas.metaDataMaps[measIndex];

    string metaData = "satelliteErrorCount";

    unsigned int* satelliteErrorCount_ptr = (unsigned int*)metaDataMap[metaData];

    if (satelliteErrorCount_ptr == nullptr)
    {
        return true;
    }

    unsigned int& satelliteErrorCount = *satelliteErrorCount_ptr;

    // Increment counter, and clear the pointer so it wont increment again at current epoch
    satelliteErrorCount++;
    metaDataMap[metaData] = nullptr;

    char idStr[100];
    snprintf(
        idStr,
        sizeof(idStr),
        "%10s\t%4s\t%4s\t%5s",
        "",
        kfMeas.obsKeys[measIndex].Sat.id().c_str(),
        "",
        ""
    );

    trace << "\n"
          << kfState.time << "\tIncrementing satelliteErrorCount  on\t" << idStr << "\tto "
          << satelliteErrorCount;

    return true;
}

/** Count all errors on an individual state
 */
bool incrementStateErrors(RejectCallbackDetails rejectDetails)
{
    auto& trace   = rejectDetails.trace;
    auto& kfState = rejectDetails.kfState;
    auto& kfKey   = rejectDetails.kfKey;

    KFKey kfKeyCopy = kfKey;
    kfKeyCopy.num   = 0;

    if (acsConfig.errorAccumulation.enable == false)
    {
        return true;
    }

    if (kfState.errorCountMap.find(kfKeyCopy) == kfState.errorCountMap.end())
    {
        kfState.errorCountMap[kfKeyCopy] = 0;  // initialise error count upon first occurance
    }

    kfState.errorCountMap[kfKeyCopy]++;

    trace << "\n"
          << kfState.time << "\tIncrementing stateErrorCount      on\t" << kfKeyCopy << "\tto "
          << kfState.errorCountMap[kfKeyCopy];

    return true;
}

void resetPhaseSignalError(const GTime& time, KFMeas& kfMeas, int index)
{
    map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

    // these will have been set to null if there was an error after adding the measurement to the
    // list
    for (auto suffix : {"", "_alt"})
    {
        unsigned int* phaseRejectCount_ptr =
            (unsigned int*)metaDataMap[(string) "phaseRejectCount" + suffix];

        if (phaseRejectCount_ptr == nullptr)
        {
            return;
        }

        unsigned int& phaseRejectCount = *phaseRejectCount_ptr;

        phaseRejectCount = 0;
    }

    return;
}

void resetIonoSignalOutage(const GTime& time, KFMeas& kfMeas, int index)
{
    map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

    for (auto suffix : {"", "_alt"})
    {
        GTime* lastIonTime_ptr = (GTime*)metaDataMap[(string) "lastIonTime" + suffix];

        if (lastIonTime_ptr == nullptr)
        {
            return;
        }

        GTime& lastIonTime = *lastIonTime_ptr;

        lastIonTime = time;
    }

    return;
}

/** Reject worst measurement attached to worst state using measurement reject callback list
 */
bool rejectWorstMeasByState(RejectCallbackDetails rejectDetails)
{
    auto& trace     = rejectDetails.trace;
    auto& kfState   = rejectDetails.kfState;
    auto& kfMeas    = rejectDetails.kfMeas;
    auto& kfKey     = rejectDetails.kfKey;
    auto& measIndex = rejectDetails.measIndex;

    KFKey kfKeyCopy = kfKey;
    kfKeyCopy.num   = 0;

    if (acsConfig.errorAccumulation.enable &&
        acsConfig.errorAccumulation.state_error_count_threshold > 0 &&
        kfState.errorCountMap[kfKeyCopy] >= acsConfig.errorAccumulation.state_error_count_threshold)
    {
        trace << "\n"
              << "High state error counts: " << kfKeyCopy;

        return true;
    }

    trace << "\n"
          << "Suspected bad state " << kfKey << " - try rejecting worst referencing measurement "
          << kfMeas.obsKeys[measIndex] << "\n";

    kfState.doMeasRejectCallbacks(rejectDetails);

    return false;
}

/** Reject all measurements attached to worst state using measurement reject callback list
 */
bool rejectAllMeasByState(RejectCallbackDetails rejectDetails)
{
    auto& trace      = rejectDetails.trace;
    auto& kfState    = rejectDetails.kfState;
    auto& kfMeas     = rejectDetails.kfMeas;
    auto& kfKey      = rejectDetails.kfKey;
    auto& stateIndex = rejectDetails.stateIndex;

    trace << "\n"
          << "Bad state detected " << kfKey << " - rejecting all referencing measurements" << "\n";

    for (int measIndex = 0; measIndex < kfMeas.H.rows(); measIndex++)
    {
        if (kfMeas.H(measIndex, stateIndex))
        {
            rejectDetails.measIndex = measIndex;

            kfState.doMeasRejectCallbacks(rejectDetails);
        }
    }

    return false;
}

/** Immediately executed reaction to orbital state errors.
 * Note there is also a 1 epoch delayed reaction function
 */
bool satelliteGlitchReaction(RejectCallbackDetails rejectDetails)
{
    auto& trace   = rejectDetails.trace;
    auto& kfState = rejectDetails.kfState;
    auto& kfKey   = rejectDetails.kfKey;

    if (kfKey.type != KF::NONE && kfKey.type != KF::ORBIT && kfKey.type != KF::SAT_CLOCK)
    {
        return true;
    }

    if (acsConfig.satelliteErrors.enable == false)
    {
        BOOST_LOG_TRIVIAL(warning) << "Bad satellite detected but `satellite_errors` not enabled";

        return true;
    }

    if (kfKey.type != KF::NONE)
        trace << "\n"
              << "Bad satellite orbit or clock detected " << kfKey;
    else
        trace << "\n"
              << "Bad satellite detected " << kfKey.Sat.id();

    kfState.statisticsMap["Satellite state reject"]++;

    Exponential exponentialNoise;
    exponentialNoise.tau   = acsConfig.satelliteErrors.vel_proc_noise_trail_tau;
    exponentialNoise.value = SQR(acsConfig.satelliteErrors.vel_proc_noise_trail);

    MatrixXd F = MatrixXd::Identity(kfState.x.rows(), kfState.x.rows());
    MatrixXd Q = MatrixXd::Zero(kfState.x.rows(), kfState.x.rows());

    bool transitionRequired = false;

    if (kfKey.type == KF::NONE || kfKey.type == KF::ORBIT)
        for (auto& [key, index] : kfState.kfIndexMap)
        {
            if (key.type != KF::ORBIT || key.str != kfKey.str || key.Sat != kfKey.Sat)
            {
                continue;
            }

            if (key.num < 3 && acsConfig.satelliteErrors.pos_proc_noise)
            {
                trace << "\n - Adding " << acsConfig.satelliteErrors.pos_proc_noise
                      << " to sigma of " << key;

                Q(index, index) = SQR(acsConfig.satelliteErrors.pos_proc_noise);

                transitionRequired = true;
            }

            if (key.num >= 3 && acsConfig.satelliteErrors.vel_proc_noise)
            {
                trace << "\n - Adding " << acsConfig.satelliteErrors.vel_proc_noise
                      << " to sigma of " << key;

                Q(index, index) = SQR(acsConfig.satelliteErrors.vel_proc_noise);

                kfState.setExponentialNoise(key, exponentialNoise);

                transitionRequired = true;
            }
        }

    if (kfKey.type == KF::NONE || kfKey.type == KF::SAT_CLOCK)
        for (auto& [key, index] : kfState.kfIndexMap)
        {
            if (key.type != KF::SAT_CLOCK || key.str != kfKey.str || key.Sat != kfKey.Sat)
            {
                continue;
            }

            if (acsConfig.satelliteErrors.clk_proc_noise)
            {
                trace << "\n - Adding " << acsConfig.satelliteErrors.clk_proc_noise
                      << " to sigma of " << key;

                Q(index, index) = SQR(acsConfig.satelliteErrors.clk_proc_noise);

                transitionRequired = true;
            }
        }

    if (transitionRequired)
    {
        int index = -1;

        auto it = kfState.kfIndexMap.find(kfKey);
        if (it != kfState.kfIndexMap.end())
        {
            index = it->second;
        }

        if (index >= 0)
        {
            trace << "\n - Pre-transition  state sigma for " << kfKey << ": "
                  << sqrt(kfState.P(index, index));
        }

        kfState.manualStateTransition(trace, kfState.time, F, Q);

        if (index >= 0)
        {
            trace << "\n - Post-transition state sigma for " << kfKey << ": "
                  << sqrt(kfState.P(index, index));
        }

        return false;
    }

    return true;
}

/** Relax state
 */
bool relaxState(RejectCallbackDetails rejectDetails)
{
    auto& trace      = rejectDetails.trace;
    auto& kfState    = rejectDetails.kfState;
    auto& kfKey      = rejectDetails.kfKey;
    auto& stateIndex = rejectDetails.stateIndex;
    auto& stage      = rejectDetails.stage;

    if (acsConfig.stateErrors.enable == false)
    {
        BOOST_LOG_TRIVIAL(warning)
            << "Warning: Bad state detected but `state_deweighting` not enabled";

        return true;
    }

    double deweightFactor = 1;
    if (stage == E_FilterStage::PREFIT)
    {
        deweightFactor = abs(kfState.prefitRatios(stateIndex));
    }
    else if (stage == E_FilterStage::POSTFIT)
    {
        deweightFactor = abs(kfState.postfitRatios(stateIndex));
    }
    // deweightFactor = std::min(abs(deweightFactor), 5000.0);  // To avoid breaking
    // filter, maximum process noise allowed is 5000 times of prefit sigma

    trace << "\n"
          << "Bad state detected " << kfKey << " - relaxing state";

    kfState.statisticsMap["State rejection"]++;

    MatrixXd F = MatrixXd::Identity(kfState.x.rows(), kfState.x.rows());
    MatrixXd Q = MatrixXd::Zero(kfState.x.rows(), kfState.x.rows());

    Exponential exponentialNoise;
    exponentialNoise.value = SQR(acsConfig.satelliteErrors.vel_proc_noise_trail);
    exponentialNoise.tau   = acsConfig.satelliteErrors.vel_proc_noise_trail_tau;

    bool transitionRequired = false;

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        // relax states for all components
        if (key.type != kfKey.type || key.str != kfKey.str || key.Sat != kfKey.Sat)
        {
            continue;
        }

        double procNoise = deweightFactor * sqrt(kfState.P(index, index));

        trace << "\n - Adding " << procNoise << " to sigma of " << key;

        Q(index, index) = SQR(procNoise);

        if (key.type == KF::ORBIT && key.num >= 3 && acsConfig.satelliteErrors.enable &&
            acsConfig.satelliteErrors.vel_proc_noise_trail &&
            acsConfig.satelliteErrors.vel_proc_noise_trail_tau)
        {
            trace << "\n - Adding exponential " << acsConfig.satelliteErrors.vel_proc_noise_trail
                  << " (per square root second) to process noise sigma of " << key
                  << " from next epoch";

            kfState.setExponentialNoise(key, exponentialNoise);
        }

        transitionRequired = true;
    }

    if (transitionRequired)
    {
        trace << "\n - Pre-transition  state sigma for " << kfKey << ": "
              << sqrt(kfState.P(stateIndex, stateIndex));

        kfState.manualStateTransition(trace, kfState.time, F, Q);

        trace << "\n - Post-transition state sigma for " << kfKey << ": "
              << sqrt(kfState.P(stateIndex, stateIndex));

        return false;
    }

    return true;
}
