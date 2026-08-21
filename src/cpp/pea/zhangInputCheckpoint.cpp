#include "pea/zhangInputCheckpoint.hpp"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <exception>
#include <filesystem>
#include <iterator>
#include <limits>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/list.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/vector.hpp>

#include "common/observations.hpp"
#include "common/rinex.hpp"
#include "common/streamFile.hpp"
#include "common/streamObs.hpp"
#include "common/streamRinex.hpp"

namespace
{
using std::list;
using std::map;
using std::string;
using std::vector;

constexpr std::size_t MAX_INPUT_CHECKPOINT_PAYLOAD_BYTES =
    std::size_t{4} * 1024 * 1024 * 1024;

struct ZhangPeaControllerCheckpointEnvelope
{
    std::uint32_t schemaVersion = ZHANG_PEA_CONTROLLER_CHECKPOINT_SCHEMA_VERSION;
    string sectionName = ZHANG_PEA_CONTROLLER_CHECKPOINT_SECTION_NAME;
    ZhangPeaControllerCheckpointState state;

    template <class ARCHIVE>
    void serialize(ARCHIVE& ar, const unsigned int& version)
    {
        ar & schemaVersion;
        ar & sectionName;
        ar & state;
    }
};

struct ZhangRinexCheckpointCodeType
{
    char type = 0;
    int code = 0;
    int code2 = 0;

    template <class ARCHIVE>
    void serialize(ARCHIVE& ar, const unsigned int& version)
    {
        ar & type;
        ar & code;
        ar & code2;
    }
};

struct ZhangRinexCheckpointSignal
{
    int code = 0;
    double phase = 0;
    double pseudorange = 0;
    double doppler = 0;
    bool lossOfLock = false;
    double snr = 0;
    bool invalid = false;
    double codeVariance = 0;
    double phaseVariance = 0;

    template <class ARCHIVE>
    void serialize(ARCHIVE& ar, const unsigned int& version)
    {
        ar & code;
        ar & phase;
        ar & pseudorange;
        ar & doppler;
        ar & lossOfLock;
        ar & snr;
        ar & invalid;
        ar & codeVariance;
        ar & phaseVariance;
    }
};

struct ZhangRinexCheckpointIonoPiercePoint
{
    double latitudeDegrees = 0;
    double longitudeDegrees = 0;
    double slantFactor = 1;

    template <class ARCHIVE>
    void serialize(ARCHIVE& ar, const unsigned int& version)
    {
        ar & latitudeDegrees;
        ar & longitudeDegrees;
        ar & slantFactor;
    }
};

/** Complete value state of a queued GObs.  Process addresses are not fields
 * in this DTO and capture rejects observations whose external pointers have
 * already been bound. */
struct ZhangRinexCheckpointObservation
{
    unsigned int excludeFlags = 0;
    ZhangCheckpointTime time;
    string mount;
    double ephemerisVariance = 0;

    double stecToDelay = 0;
    int stecType = 0;
    double stecValue = 0;
    double stecVariance = 0;
    int stecCodeCombination = 0;
    SatSys ionosphereSatellite;
    map<int, ZhangRinexCheckpointIonoPiercePoint> piercePoints;
    unsigned int ionosphereExcludeFlags = 0;

    double sppCodeResidual = 0;
    double troposphereSlant = 0;
    double troposphereSlantVariance = 0;

    ZhangCheckpointTime positionTime;
    SatSys satellite;
    int positionSource = 0;
    int clockSource = 0;
    VectorEcef satelliteComPosition;
    VectorEcef satelliteApcPosition;
    VectorEcef satelliteVelocity;
    VectorEci satelliteEciPositionAtTransmission;
    VectorEci satelliteEciVelocityAtTransmission;
    VectorEci satelliteEciPositionAtEpoch;
    VectorEci satelliteEciVelocityAtEpoch;
    double positionVariance = 0;
    double satelliteClock = 0;
    double satelliteClockVelocity = 0;
    double satelliteClockVariance = 0;
    bool sppValid = false;
    int clockIode = -1;
    int positionIode = -1;
    bool ephemerisPositionValid = false;
    bool ephemerisClockValid = false;
    double timeOfFlight = 0;
    unsigned int failureFlags = 0;

    map<int, ZhangRinexCheckpointSignal> selectedSignals;
    map<int, vector<ZhangRinexCheckpointSignal>> signalLists;

    template <class ARCHIVE>
    void serialize(ARCHIVE& ar, const unsigned int& version)
    {
        ar & excludeFlags;
        ar & time;
        ar & mount;
        ar & ephemerisVariance;
        ar & stecToDelay;
        ar & stecType;
        ar & stecValue;
        ar & stecVariance;
        ar & stecCodeCombination;
        ar & ionosphereSatellite;
        ar & piercePoints;
        ar & ionosphereExcludeFlags;
        ar & sppCodeResidual;
        ar & troposphereSlant;
        ar & troposphereSlantVariance;
        ar & positionTime;
        ar & satellite;
        ar & positionSource;
        ar & clockSource;
        ar & satelliteComPosition;
        ar & satelliteApcPosition;
        ar & satelliteVelocity;
        ar & satelliteEciPositionAtTransmission;
        ar & satelliteEciVelocityAtTransmission;
        ar & satelliteEciPositionAtEpoch;
        ar & satelliteEciVelocityAtEpoch;
        ar & positionVariance;
        ar & satelliteClock;
        ar & satelliteClockVelocity;
        ar & satelliteClockVariance;
        ar & sppValid;
        ar & clockIode;
        ar & positionIode;
        ar & ephemerisPositionValid;
        ar & ephemerisClockValid;
        ar & timeOfFlight;
        ar & failureFlags;
        ar & selectedSignals;
        ar & signalLists;
    }
};

using ZhangRinexCheckpointObservationEpoch =
    vector<ZhangRinexCheckpointObservation>;

struct ZhangRinexCheckpointStation
{
    string id;
    string marker;
    string antennaDescription;
    string antennaSerial;
    string receiverType;
    string receiverFirmware;
    string receiverSerial;
    Vector3d antennaDelta = Vector3d::Zero();
    Vector3d approximatePosition = Vector3d::Zero();

    template <class ARCHIVE>
    void serialize(ARCHIVE& ar, const unsigned int& version)
    {
        ar & id;
        ar & marker;
        ar & antennaDescription;
        ar & antennaSerial;
        ar & receiverType;
        ar & receiverFirmware;
        ar & receiverSerial;
        ar & antennaDelta;
        ar & approximatePosition;
    }
};

struct ZhangRinexFileStreamCheckpointState
{
    string stableKey;
    string receiverId;
    string sourceString;
    string canonicalPath;
    std::uint64_t occurrenceOrdinal = 0;
    std::uint64_t fileSize = 0;
    string fileSha256;
    std::int64_t filePosition = 0;
    bool dead = false;

    int observationAgeCode = 0;
    ZhangCheckpointTime lastReadTime;
    double observationInterval = 0;
    bool pseudoReceiver = false;

    char rinexContentType = 0;
    double rinexVersion = 0;
    int navigationSystem = 0;
    int timeSystem = 0;
    map<int, map<int, ZhangRinexCheckpointCodeType>> systemCodeTypes;
    ZhangRinexCheckpointObservationEpoch temporaryObservations;
    ZhangRinexCheckpointStation station;
    vector<ZhangRinexCheckpointObservationEpoch> futureObservationQueue;

    template <class ARCHIVE>
    void serialize(ARCHIVE& ar, const unsigned int& version)
    {
        ar & stableKey;
        ar & receiverId;
        ar & sourceString;
        ar & canonicalPath;
        ar & occurrenceOrdinal;
        ar & fileSize;
        ar & fileSha256;
        ar & filePosition;
        ar & dead;
        ar & observationAgeCode;
        ar & lastReadTime;
        ar & observationInterval;
        ar & pseudoReceiver;
        ar & rinexContentType;
        ar & rinexVersion;
        ar & navigationSystem;
        ar & timeSystem;
        ar & systemCodeTypes;
        ar & temporaryObservations;
        ar & station;
        ar & futureObservationQueue;
    }
};

struct ZhangRinexFileStreamsCheckpointEnvelope
{
    std::uint32_t schemaVersion =
        ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SCHEMA_VERSION;
    string sectionName = ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SECTION_NAME;
    vector<ZhangRinexFileStreamCheckpointState> streams;
    map<string, bool> streamDoneMap;

    template <class ARCHIVE>
    void serialize(ARCHIVE& ar, const unsigned int& version)
    {
        ar & schemaVersion;
        ar & sectionName;
        ar & streams;
        ar & streamDoneMap;
    }
};

template <typename TYPE>
bool serializePayload(
    const TYPE& value,
    string& payload,
    string& failureReason,
    const string& failurePrefix)
{
    payload.clear();
    try
    {
        std::ostringstream output(std::ios::binary | std::ios::out);
        boost::archive::binary_oarchive archive(
            output, boost::archive::no_header);
        archive << value;
        payload = output.str();
    }
    catch (const std::exception& exception)
    {
        payload.clear();
        failureReason = failurePrefix + "_SERIALIZE_FAILED:" + exception.what();
        return false;
    }
    if (payload.empty())
    {
        failureReason = failurePrefix + "_EMPTY_PAYLOAD";
        return false;
    }
    if (payload.size() > MAX_INPUT_CHECKPOINT_PAYLOAD_BYTES)
    {
        payload.clear();
        failureReason = failurePrefix + "_PAYLOAD_TOO_LARGE";
        return false;
    }
    return true;
}

template <typename TYPE>
bool deserializePayload(
    const string& payload,
    TYPE& value,
    string& failureReason,
    const string& failurePrefix)
{
    if (payload.empty())
    {
        failureReason = failurePrefix + "_EMPTY_PAYLOAD";
        return false;
    }
    if (payload.size() > MAX_INPUT_CHECKPOINT_PAYLOAD_BYTES)
    {
        failureReason = failurePrefix + "_PAYLOAD_TOO_LARGE";
        return false;
    }
    try
    {
        std::istringstream input(payload, std::ios::binary | std::ios::in);
        {
            boost::archive::binary_iarchive archive(
                input, boost::archive::no_header);
            archive >> value;
        }
        if (input.peek() != std::char_traits<char>::eof())
        {
            failureReason = failurePrefix + "_TRAILING_BYTES";
            return false;
        }
    }
    catch (const std::exception& exception)
    {
        failureReason = failurePrefix + "_DESERIALIZE_FAILED:" + exception.what();
        return false;
    }
    return true;
}

bool checkpointTimesEqual(
    const ZhangCheckpointTime& left,
    const ZhangCheckpointTime& right)
{
    return left.bigTimeBytes == right.bigTimeBytes;
}

bool validControllerState(
    const ZhangPeaControllerCheckpointState& state,
    double expectedEpochIntervalSeconds,
    string& failureReason)
{
    if (state.boundary != ZHANG_PEA_POST_EPOCH_BOUNDARY)
    {
        failureReason = "PEA_CONTROLLER_CHECKPOINT_UNSUPPORTED_BOUNDARY";
        return false;
    }
    if (state.resumePolicy != ZHANG_PEA_RESUME_NEXT_EPOCH)
    {
        failureReason = "PEA_CONTROLLER_CHECKPOINT_UNSUPPORTED_RESUME_POLICY";
        return false;
    }
    if (state.completedEpoch < 1 || state.nextEpoch != state.completedEpoch + 1)
    {
        failureReason = "PEA_CONTROLLER_CHECKPOINT_INVALID_EPOCH_SEQUENCE";
        return false;
    }
    if (!std::isfinite(state.epochIntervalSeconds)
        || state.epochIntervalSeconds <= 0)
    {
        failureReason = "PEA_CONTROLLER_CHECKPOINT_INVALID_EPOCH_INTERVAL";
        return false;
    }
    if (expectedEpochIntervalSeconds > 0
        && state.epochIntervalSeconds != expectedEpochIntervalSeconds)
    {
        failureReason = "PEA_CONTROLLER_CHECKPOINT_EPOCH_INTERVAL_MISMATCH";
        return false;
    }
    const GTime completed = restoreZhangCheckpointTime(state.completedTsync);
    if (completed == GTime::noTime())
    {
        failureReason = "PEA_CONTROLLER_CHECKPOINT_NO_COMPLETED_TIME";
        return false;
    }
    const ZhangCheckpointTime expectedNext = captureZhangCheckpointTime(
        completed + state.epochIntervalSeconds);
    if (!checkpointTimesEqual(expectedNext, state.nextTsync))
    {
        failureReason = "PEA_CONTROLLER_CHECKPOINT_NEXT_TIME_MISMATCH";
        return false;
    }
    return true;
}

bool decodeAndValidateController(
    const string& payload,
    double expectedEpochIntervalSeconds,
    ZhangPeaControllerCheckpointEnvelope& envelope,
    string& failureReason)
{
    if (!deserializePayload(
            payload, envelope, failureReason, "PEA_CONTROLLER_CHECKPOINT"))
    {
        return false;
    }
    if (envelope.schemaVersion
        != ZHANG_PEA_CONTROLLER_CHECKPOINT_SCHEMA_VERSION)
    {
        failureReason = "PEA_CONTROLLER_CHECKPOINT_SCHEMA_MISMATCH";
        return false;
    }
    if (envelope.sectionName != ZHANG_PEA_CONTROLLER_CHECKPOINT_SECTION_NAME)
    {
        failureReason = "PEA_CONTROLLER_CHECKPOINT_SECTION_NAME_MISMATCH";
        return false;
    }
    return validControllerState(
        envelope.state, expectedEpochIntervalSeconds, failureReason);
}

ZhangRinexCheckpointSignal checkpointSignal(const Sig& signal)
{
    ZhangRinexCheckpointSignal stored;
    stored.code = static_cast<int>(signal.code);
    stored.phase = signal.L;
    stored.pseudorange = signal.P;
    stored.doppler = signal.D;
    stored.lossOfLock = signal.LLI;
    stored.snr = signal.snr;
    stored.invalid = signal.invalid;
    stored.codeVariance = signal.codeVar;
    stored.phaseVariance = signal.phasVar;
    return stored;
}

Sig restoreSignal(const ZhangRinexCheckpointSignal& stored)
{
    Sig signal;
    signal.code = static_cast<E_ObsCode>(stored.code);
    signal.L = stored.phase;
    signal.P = stored.pseudorange;
    signal.D = stored.doppler;
    signal.LLI = stored.lossOfLock;
    signal.snr = stored.snr;
    signal.invalid = stored.invalid;
    signal.codeVar = stored.codeVariance;
    signal.phasVar = stored.phaseVariance;
    return signal;
}

bool checkpointObservation(
    const std::shared_ptr<Observation>& observation,
    ZhangRinexCheckpointObservation& stored,
    string& failureReason)
{
    if (!observation)
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_NULL_OBSERVATION";
        return false;
    }
    const auto* input = dynamic_cast<const GObs*>(observation.get());
    if (!input)
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_UNSUPPORTED_OBSERVATION_TYPE";
        return false;
    }
    if (input->rec_ptr || input->satNav_ptr || input->satStat_ptr)
    {
        failureReason =
            "RINEX_STREAM_CHECKPOINT_UNSUPPORTED_BOUND_OBSERVATION_POINTER";
        return false;
    }

    stored.excludeFlags = input->exclude;
    stored.time = captureZhangCheckpointTime(input->time);
    stored.mount = input->mount;
    stored.ephemerisVariance = input->ephVar;
    stored.stecToDelay = input->stecToDelay;
    stored.stecType = input->stecType;
    stored.stecValue = input->stecVal;
    stored.stecVariance = input->stecVar;
    stored.stecCodeCombination = input->stecCodeCombo;
    stored.ionosphereSatellite = input->ionoSat;
    for (const auto& [index, point] : input->ippMap)
    {
        stored.piercePoints[index] = {
            point.latDeg, point.lonDeg, point.slantFactor};
    }
    stored.ionosphereExcludeFlags = input->ionExclude;
    stored.sppCodeResidual = input->sppCodeResidual;
    stored.troposphereSlant = input->tropSlant;
    stored.troposphereSlantVariance = input->tropSlantVar;
    stored.positionTime = captureZhangCheckpointTime(input->posTime);
    stored.satellite = input->Sat;
    stored.positionSource = static_cast<int>(input->posSource);
    stored.clockSource = static_cast<int>(input->clkSource);
    stored.satelliteComPosition = input->rSatCom;
    stored.satelliteApcPosition = input->rSatApc;
    stored.satelliteVelocity = input->satVel;
    stored.satelliteEciPositionAtTransmission = input->rSatEciDt;
    stored.satelliteEciVelocityAtTransmission = input->vSatEciDt;
    stored.satelliteEciPositionAtEpoch = input->rSatEci0;
    stored.satelliteEciVelocityAtEpoch = input->vSatEci0;
    stored.positionVariance = input->posVar;
    stored.satelliteClock = input->satClk;
    stored.satelliteClockVelocity = input->satClkVel;
    stored.satelliteClockVariance = input->satClkVar;
    stored.sppValid = input->sppValid;
    stored.clockIode = input->iodeClk;
    stored.positionIode = input->iodePos;
    stored.ephemerisPositionValid = input->ephPosValid;
    stored.ephemerisClockValid = input->ephClkValid;
    stored.timeOfFlight = input->tof;
    stored.failureFlags = input->failure;
    for (const auto& [frequency, signal] : input->sigs)
    {
        stored.selectedSignals[static_cast<int>(frequency)] =
            checkpointSignal(signal);
    }
    for (const auto& [frequency, signals] : input->sigsLists)
    {
        auto& output = stored.signalLists[static_cast<int>(frequency)];
        for (const auto& signal : signals)
        {
            output.push_back(checkpointSignal(signal));
        }
    }
    return true;
}

void restoreExcludeFlags(GObs& observation, unsigned int flags)
{
    observation.excludeElevation = (flags >> 0) & 1;
    observation.excludeEclipse = (flags >> 1) & 1;
    observation.excludeSystem = (flags >> 2) & 1;
    observation.excludeOutlier = (flags >> 3) & 1;
    observation.excludeBadSPP = (flags >> 4) & 1;
    observation.excludeConfig = (flags >> 5) & 1;
    observation.excludeSVH = (flags >> 6) & 1;
    observation.excludeBadRange = (flags >> 7) & 1;
    observation.excludeDataHandling = (flags >> 8) & 1;
    observation.excludeCom = (flags >> 9) & 1;
    observation.excludeBadFlags = (flags >> 10) & 1;
    observation.excludeAlert = (flags >> 11) & 1;
}

void restoreFailureFlags(GObs& observation, unsigned int flags)
{
    observation.failureExclude = (flags >> 0) & 1;
    observation.failureNoSatPos = (flags >> 1) & 1;
    observation.failureNoSatClock = (flags >> 2) & 1;
    observation.failureNoPseudorange = (flags >> 3) & 1;
    observation.failureIodeConsistency = (flags >> 4) & 1;
    observation.failureBroadcastEph = (flags >> 5) & 1;
    observation.failureSSRFail = (flags >> 6) & 1;
    observation.failureSsrPosEmpty = (flags >> 7) & 1;
    observation.failureSsrClkEmpty = (flags >> 8) & 1;
    observation.failureSsrPosTime = (flags >> 9) & 1;
    observation.failureSsrClkTime = (flags >> 10) & 1;
    observation.failureSsrPosMag = (flags >> 11) & 1;
    observation.failureSsrClkMag = (flags >> 12) & 1;
    observation.failureSsrPosUdi = (flags >> 13) & 1;
    observation.failureSsrClkUdi = (flags >> 14) & 1;
    observation.failureGeodist = (flags >> 15) & 1;
    observation.failureRSat = (flags >> 16) & 1;
    observation.failureElevation = (flags >> 17) & 1;
    observation.failurePrange = (flags >> 18) & 1;
}

bool restoreObservation(
    const ZhangRinexCheckpointObservation& stored,
    std::shared_ptr<Observation>& observation,
    string& failureReason)
{
    GObs output = {};
    restoreExcludeFlags(output, stored.excludeFlags);
    output.time = restoreZhangCheckpointTime(stored.time);
    output.mount = stored.mount;
    output.ephVar = stored.ephemerisVariance;
    output.stecToDelay = stored.stecToDelay;
    output.stecType = stored.stecType;
    output.stecVal = stored.stecValue;
    output.stecVar = stored.stecVariance;
    output.stecCodeCombo = stored.stecCodeCombination;
    output.ionoSat = stored.ionosphereSatellite;
    for (const auto& [index, point] : stored.piercePoints)
    {
        output.ippMap[index] = {
            point.latitudeDegrees,
            point.longitudeDegrees,
            point.slantFactor};
    }
    output.ionExclude = stored.ionosphereExcludeFlags;
    output.rec_ptr = nullptr;
    output.sppCodeResidual = stored.sppCodeResidual;
    output.tropSlant = stored.troposphereSlant;
    output.tropSlantVar = stored.troposphereSlantVariance;
    output.posTime = restoreZhangCheckpointTime(stored.positionTime);
    output.Sat = stored.satellite;
    output.satNav_ptr = nullptr;
    output.satStat_ptr = nullptr;
    output.posSource = static_cast<E_Source>(stored.positionSource);
    output.clkSource = static_cast<E_Source>(stored.clockSource);
    output.rSatCom = stored.satelliteComPosition;
    output.rSatApc = stored.satelliteApcPosition;
    output.satVel = stored.satelliteVelocity;
    output.rSatEciDt = stored.satelliteEciPositionAtTransmission;
    output.vSatEciDt = stored.satelliteEciVelocityAtTransmission;
    output.rSatEci0 = stored.satelliteEciPositionAtEpoch;
    output.vSatEci0 = stored.satelliteEciVelocityAtEpoch;
    output.posVar = stored.positionVariance;
    output.satClk = stored.satelliteClock;
    output.satClkVel = stored.satelliteClockVelocity;
    output.satClkVar = stored.satelliteClockVariance;
    output.sppValid = stored.sppValid;
    output.iodeClk = stored.clockIode;
    output.iodePos = stored.positionIode;
    output.ephPosValid = stored.ephemerisPositionValid;
    output.ephClkValid = stored.ephemerisClockValid;
    output.tof = stored.timeOfFlight;
    restoreFailureFlags(output, stored.failureFlags);
    for (const auto& [frequency, signal] : stored.selectedSignals)
    {
        output.sigs[static_cast<E_FType>(frequency)] = restoreSignal(signal);
    }
    for (const auto& [frequency, signals] : stored.signalLists)
    {
        auto& target = output.sigsLists[static_cast<E_FType>(frequency)];
        for (const auto& signal : signals)
        {
            target.push_back(restoreSignal(signal));
        }
    }
    std::shared_ptr<GObs> typed = output;
    if (!typed)
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_OBSERVATION_REBUILD_FAILED";
        return false;
    }
    observation = std::move(typed);
    return true;
}

bool validFrequency(int value)
{
    return value != static_cast<int>(E_FType::NUM_FTYPES)
        && magic_enum::enum_contains(static_cast<E_FType>(value));
}

bool validSatellite(const SatSys& satellite, bool allowNone)
{
    if (!magic_enum::enum_contains(satellite.sys))
    {
        return false;
    }
    if (!allowNone && satellite.sys == E_Sys::NONE)
    {
        return false;
    }
    return satellite.prn >= 0;
}

bool validSignal(
    const ZhangRinexCheckpointSignal& signal,
    string& failureReason)
{
    if (!magic_enum::enum_contains(static_cast<E_ObsCode>(signal.code)))
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_SIGNAL_CODE";
        return false;
    }
    if (!std::isfinite(signal.phase)
        || !std::isfinite(signal.pseudorange)
        || !std::isfinite(signal.doppler)
        || !std::isfinite(signal.snr)
        || !std::isfinite(signal.codeVariance)
        || !std::isfinite(signal.phaseVariance))
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_NONFINITE_SIGNAL";
        return false;
    }
    return true;
}

bool validObservation(
    const ZhangRinexCheckpointObservation& observation,
    string& failureReason)
{
    if ((observation.excludeFlags & ~((1u << 12) - 1)) != 0
        || (observation.ionosphereExcludeFlags & ~((1u << 4) - 1)) != 0
        || (observation.failureFlags & ~((1u << 19) - 1)) != 0)
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_OBSERVATION_FLAGS";
        return false;
    }
    if (restoreZhangCheckpointTime(observation.time) == GTime::noTime()
        || !validSatellite(observation.satellite, false)
        || !validSatellite(observation.ionosphereSatellite, true))
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_OBSERVATION_IDENTITY";
        return false;
    }
    if (!magic_enum::enum_contains(
            static_cast<E_Source>(observation.positionSource))
        || !magic_enum::enum_contains(
            static_cast<E_Source>(observation.clockSource)))
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_OBSERVATION_SOURCE";
        return false;
    }
    const double values[] = {
        observation.ephemerisVariance,
        observation.stecToDelay,
        observation.stecValue,
        observation.stecVariance,
        observation.sppCodeResidual,
        observation.troposphereSlant,
        observation.troposphereSlantVariance,
        observation.positionVariance,
        observation.satelliteClock,
        observation.satelliteClockVelocity,
        observation.satelliteClockVariance,
        observation.timeOfFlight};
    if (!std::all_of(
            std::begin(values),
            std::end(values),
            [](double value) { return std::isfinite(value); })
        || !observation.satelliteComPosition.allFinite()
        || !observation.satelliteApcPosition.allFinite()
        || !observation.satelliteVelocity.allFinite()
        || !observation.satelliteEciPositionAtTransmission.allFinite()
        || !observation.satelliteEciVelocityAtTransmission.allFinite()
        || !observation.satelliteEciPositionAtEpoch.allFinite()
        || !observation.satelliteEciVelocityAtEpoch.allFinite())
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_NONFINITE_OBSERVATION";
        return false;
    }
    for (const auto& [index, point] : observation.piercePoints)
    {
        if (!std::isfinite(point.latitudeDegrees)
            || !std::isfinite(point.longitudeDegrees)
            || !std::isfinite(point.slantFactor))
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_NONFINITE_IONO_POINT";
            return false;
        }
    }
    for (const auto& [frequency, signal] : observation.selectedSignals)
    {
        if (!validFrequency(frequency) || !validSignal(signal, failureReason))
        {
            if (failureReason.empty())
            {
                failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_SIGNAL_FREQUENCY";
            }
            return false;
        }
    }
    for (const auto& [frequency, signals] : observation.signalLists)
    {
        if (!validFrequency(frequency))
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_SIGNAL_FREQUENCY";
            return false;
        }
        for (const auto& signal : signals)
        {
            if (!validSignal(signal, failureReason))
            {
                return false;
            }
        }
    }
    return true;
}

bool checkpointObservationEpoch(
    const ObsList& input,
    ZhangRinexCheckpointObservationEpoch& output,
    string& failureReason)
{
    output.clear();
    output.reserve(input.size());
    for (const auto& observation : input)
    {
        ZhangRinexCheckpointObservation stored;
        if (!checkpointObservation(observation, stored, failureReason)
            || !validObservation(stored, failureReason))
        {
            output.clear();
            return false;
        }
        output.push_back(std::move(stored));
    }
    return true;
}

bool restoreObservationEpoch(
    const ZhangRinexCheckpointObservationEpoch& input,
    ObsList& output,
    string& failureReason)
{
    output.clear();
    output.reserve(input.size());
    for (const auto& stored : input)
    {
        if (!validObservation(stored, failureReason))
        {
            output.clear();
            return false;
        }
        std::shared_ptr<Observation> observation;
        if (!restoreObservation(stored, observation, failureReason))
        {
            output.clear();
            return false;
        }
        output.push_back(std::move(observation));
    }
    return true;
}

ZhangRinexCheckpointStation checkpointStation(const RinexStation& input)
{
    ZhangRinexCheckpointStation output;
    output.id = input.id;
    output.marker = input.marker;
    output.antennaDescription = input.antDesc;
    output.antennaSerial = input.antSerial;
    output.receiverType = input.recType;
    output.receiverFirmware = input.recFWVersion;
    output.receiverSerial = input.recSerial;
    output.antennaDelta = input.del;
    output.approximatePosition = input.pos;
    return output;
}

RinexStation restoreStation(const ZhangRinexCheckpointStation& input)
{
    RinexStation output;
    output.id = input.id;
    output.marker = input.marker;
    output.antDesc = input.antennaDescription;
    output.antSerial = input.antennaSerial;
    output.recType = input.receiverType;
    output.recFWVersion = input.receiverFirmware;
    output.recSerial = input.receiverSerial;
    output.del = input.antennaDelta;
    output.pos = input.approximatePosition;
    return output;
}

string canonicalInputPath(const string& path, string& failureReason)
{
    std::error_code error;
    std::filesystem::path canonical = std::filesystem::weakly_canonical(
        std::filesystem::absolute(path, error), error);
    if (error || canonical.empty())
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_PATH_CANONICALIZATION_FAILED:" + path;
        return {};
    }
    return canonical.generic_string();
}

string stableStreamBaseKey(
    const string& receiverId,
    const string& sourceString,
    const string& canonicalPath)
{
    std::ostringstream output;
    output << receiverId.size() << ':' << receiverId
           << sourceString.size() << ':' << sourceString
           << canonicalPath.size() << ':' << canonicalPath;
    return output.str();
}

string stableStreamKey(
    const string& receiverId,
    const string& sourceString,
    const string& canonicalPath,
    std::uint64_t occurrenceOrdinal)
{
    std::ostringstream output;
    output << stableStreamBaseKey(receiverId, sourceString, canonicalPath)
           << '#' << occurrenceOrdinal;
    return output.str();
}

bool currentFileIdentity(
    const string& path,
    string& canonicalPath,
    std::uint64_t& fileSize,
    string& fileSha256,
    string& failureReason)
{
    canonicalPath = canonicalInputPath(path, failureReason);
    if (canonicalPath.empty())
    {
        return false;
    }
    std::error_code error;
    if (!std::filesystem::is_regular_file(canonicalPath, error) || error)
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INPUT_NOT_REGULAR_FILE:" + canonicalPath;
        return false;
    }
    const auto size = std::filesystem::file_size(canonicalPath, error);
    if (error)
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_FILE_SIZE_FAILED:" + canonicalPath;
        return false;
    }
    fileSize = static_cast<std::uint64_t>(size);
    fileSha256 = zhangCheckpointFileSha256(canonicalPath, &failureReason);
    if (fileSha256.empty())
    {
        if (failureReason.empty())
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_FILE_HASH_FAILED:" + canonicalPath;
        }
        return false;
    }
    return true;
}

struct RuntimeRinexFileStream
{
    string stableKey;
    string receiverId;
    string sourceString;
    string canonicalPath;
    std::uint64_t occurrenceOrdinal = 0;
    std::uint64_t fileSize = 0;
    string fileSha256;
    ObsStream* observationStream = nullptr;
    FileStream* fileStream = nullptr;
    RinexParser* rinexParser = nullptr;
};

bool inspectRuntimeStreams(
    const std::multimap<string, StreamParserPtr>& streams,
    vector<RuntimeRinexFileStream>& output,
    string& failureReason)
{
    output.clear();
    output.reserve(streams.size());
    map<string, std::uint64_t> nextOccurrenceByBaseKey;
    std::set<string> stableKeys;
    for (const auto& [receiverId, streamParser] : streams)
    {
        if (!streamParser)
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_NULL_STREAM_PARSER";
            return false;
        }
        auto* observationStream = dynamic_cast<ObsStream*>(streamParser.get());
        auto* fileStream = dynamic_cast<FileStream*>(&streamParser->stream);
        auto* rinexParser = dynamic_cast<RinexParser*>(&streamParser->parser);
        if (!observationStream || !fileStream || !rinexParser)
        {
            failureReason =
                "RINEX_STREAM_CHECKPOINT_UNSUPPORTED_STREAM_OR_PARSER:" + receiverId;
            return false;
        }
        RuntimeRinexFileStream item;
        item.receiverId = receiverId;
        item.sourceString = streamParser->stream.sourceString;
        if (!currentFileIdentity(
                fileStream->path,
                item.canonicalPath,
                item.fileSize,
                item.fileSha256,
                failureReason))
        {
            return false;
        }
        const string baseKey = stableStreamBaseKey(
            receiverId, item.sourceString, item.canonicalPath);
        item.occurrenceOrdinal = nextOccurrenceByBaseKey[baseKey]++;
        item.stableKey = stableStreamKey(
            receiverId,
            item.sourceString,
            item.canonicalPath,
            item.occurrenceOrdinal);
        if (!stableKeys.insert(item.stableKey).second)
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_INTERNAL_STABLE_KEY_COLLISION";
            return false;
        }
        item.observationStream = observationStream;
        item.fileStream = fileStream;
        item.rinexParser = rinexParser;
        output.push_back(std::move(item));
    }
    std::sort(
        output.begin(),
        output.end(),
        [](const auto& left, const auto& right)
        {
            return left.stableKey < right.stableKey;
        });
    return true;
}

bool checkpointRuntimeStream(
    const RuntimeRinexFileStream& input,
    ZhangRinexFileStreamCheckpointState& output,
    string& failureReason)
{
    const auto& observationStream = *input.observationStream;
    auto& fileStream = *input.fileStream;
    const auto& parser = *input.rinexParser;

    output.stableKey = input.stableKey;
    output.receiverId = input.receiverId;
    output.sourceString = input.sourceString;
    output.canonicalPath = input.canonicalPath;
    output.occurrenceOrdinal = input.occurrenceOrdinal;
    output.fileSize = input.fileSize;
    output.fileSha256 = input.fileSha256;
    output.filePosition = static_cast<std::int64_t>(fileStream.filePos);
    output.dead = fileStream.isDead();
    output.observationAgeCode = static_cast<int>(observationStream.obsAgeCode);
    output.lastReadTime = captureZhangCheckpointTime(observationStream.lastReadTime);
    output.observationInterval = observationStream.interval;
    output.pseudoReceiver = observationStream.isPseudoRec;
    output.rinexContentType = parser.ctype;
    output.rinexVersion = parser.version;
    output.navigationSystem = static_cast<int>(parser.nav_system);
    output.timeSystem = static_cast<int>(parser.time_system);
    for (const auto& [system, indexMap] : parser.sysCodeTypes)
    for (const auto& [index, codeType] : indexMap)
    {
        output.systemCodeTypes[static_cast<int>(system)][index] = {
            codeType.type,
            static_cast<int>(codeType.code),
            static_cast<int>(codeType.code2)};
    }
    if (!checkpointObservationEpoch(
            parser.tempObsList, output.temporaryObservations, failureReason))
    {
        return false;
    }
    output.station = checkpointStation(parser.rnxRec);
    output.futureObservationQueue.clear();
    output.futureObservationQueue.reserve(parser.obsListList.size());
    for (const auto& epoch : parser.obsListList)
    {
        ZhangRinexCheckpointObservationEpoch storedEpoch;
        if (!checkpointObservationEpoch(epoch, storedEpoch, failureReason))
        {
            return false;
        }
        output.futureObservationQueue.push_back(std::move(storedEpoch));
    }
    return true;
}

bool validSha256(const string& hash)
{
    return hash.size() == 64
        && std::all_of(
            hash.begin(),
            hash.end(),
            [](unsigned char value)
            {
                return (value >= '0' && value <= '9')
                    || (value >= 'a' && value <= 'f')
                    || (value >= 'A' && value <= 'F');
            });
}

bool validCodeType(
    const ZhangRinexCheckpointCodeType& codeType,
    string& failureReason)
{
    if (codeType.type != 'C' && codeType.type != 'P'
        && codeType.type != 'L' && codeType.type != 'D'
        && codeType.type != 'S' && codeType.type != 'X')
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_CODE_TYPE";
        return false;
    }
    if (!magic_enum::enum_contains(static_cast<E_ObsCode>(codeType.code))
        || !magic_enum::enum_contains(static_cast<E_ObsCode2>(codeType.code2)))
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_CODE_ENUM";
        return false;
    }
    return true;
}

bool validRinexStreamState(
    const ZhangRinexFileStreamCheckpointState& state,
    string& failureReason)
{
    if (state.stableKey.empty() || state.receiverId.empty()
        || state.sourceString.empty() || state.canonicalPath.empty())
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_MISSING_STREAM_IDENTITY";
        return false;
    }
    if (state.stableKey
        != stableStreamKey(
            state.receiverId,
            state.sourceString,
            state.canonicalPath,
            state.occurrenceOrdinal))
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_STABLE_KEY_MISMATCH";
        return false;
    }
    if (!validSha256(state.fileSha256))
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_FILE_SHA256";
        return false;
    }
    if (state.dead != (state.filePosition < 0)
        || (!state.dead
            && static_cast<std::uint64_t>(state.filePosition) > state.fileSize))
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_FILE_POSITION";
        return false;
    }
    if (state.filePosition < std::numeric_limits<long int>::min()
        || state.filePosition > std::numeric_limits<long int>::max())
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_FILE_POSITION_OUT_OF_RANGE";
        return false;
    }
    if (!magic_enum::enum_contains(
            static_cast<E_ObsAgeCode>(state.observationAgeCode))
        || !std::isfinite(state.observationInterval)
        || state.observationInterval < 0)
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_OBS_STREAM_STATE";
        return false;
    }
    if (state.rinexContentType != 'O'
        || !std::isfinite(state.rinexVersion)
        || state.rinexVersion < 2 || state.rinexVersion >= 5)
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_UNSUPPORTED_RINEX_HEADER";
        return false;
    }
    if (!magic_enum::enum_contains(static_cast<E_Sys>(state.navigationSystem))
        || !magic_enum::enum_contains(static_cast<E_TimeSys>(state.timeSystem))
        || state.timeSystem == static_cast<int>(E_TimeSys::NONE))
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_RINEX_SYSTEM";
        return false;
    }
    if (state.station.id != state.receiverId
        || !state.station.antennaDelta.allFinite()
        || !state.station.approximatePosition.allFinite())
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_STATION_IDENTITY_MISMATCH";
        return false;
    }
    for (const auto& [system, codes] : state.systemCodeTypes)
    {
        if (!magic_enum::enum_contains(static_cast<E_Sys>(system)))
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_CODE_SYSTEM";
            return false;
        }
        for (const auto& [index, codeType] : codes)
        {
            if (index < 0 || !validCodeType(codeType, failureReason))
            {
                if (failureReason.empty())
                {
                    failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_CODE_INDEX";
                }
                return false;
            }
        }
    }
    for (const auto& observation : state.temporaryObservations)
    {
        if (!validObservation(observation, failureReason))
        {
            return false;
        }
    }
    ZhangCheckpointTime previousTime;
    bool havePreviousTime = false;
    for (const auto& epoch : state.futureObservationQueue)
    {
        ZhangCheckpointTime epochTime;
        bool haveEpochTime = false;
        for (const auto& observation : epoch)
        {
            if (!validObservation(observation, failureReason))
            {
                return false;
            }
            if (!haveEpochTime)
            {
                epochTime = observation.time;
                haveEpochTime = true;
            }
            else if (!checkpointTimesEqual(epochTime, observation.time))
            {
                failureReason = "RINEX_STREAM_CHECKPOINT_MIXED_EPOCH_QUEUE_ENTRY";
                return false;
            }
        }
        if (haveEpochTime && havePreviousTime)
        {
            const GTime current = restoreZhangCheckpointTime(epochTime);
            const GTime previous = restoreZhangCheckpointTime(previousTime);
            if (current < previous)
            {
                failureReason = "RINEX_STREAM_CHECKPOINT_NONMONOTONIC_QUEUE";
                return false;
            }
        }
        if (haveEpochTime)
        {
            previousTime = epochTime;
            havePreviousTime = true;
        }
    }
    return true;
}

bool decodeAndValidateRinexStreams(
    const std::multimap<string, StreamParserPtr>& streams,
    const map<string, bool>& streamDoneMap,
    const string& payload,
    ZhangRinexFileStreamsCheckpointEnvelope& envelope,
    vector<RuntimeRinexFileStream>& runtimeStreams,
    string& failureReason)
{
    if (!deserializePayload(
            payload, envelope, failureReason, "RINEX_STREAM_CHECKPOINT"))
    {
        return false;
    }
    if (envelope.schemaVersion
        != ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SCHEMA_VERSION)
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_SCHEMA_MISMATCH";
        return false;
    }
    if (envelope.sectionName
        != ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SECTION_NAME)
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_SECTION_NAME_MISMATCH";
        return false;
    }
    if (envelope.streams.empty())
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_EMPTY_INVENTORY";
        return false;
    }
    if (!inspectRuntimeStreams(streams, runtimeStreams, failureReason))
    {
        return false;
    }
    if (streamDoneMap.size() != envelope.streamDoneMap.size())
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_DONE_INVENTORY_COUNT_MISMATCH";
        return false;
    }
    auto runtimeDone = streamDoneMap.begin();
    auto snapshotDone = envelope.streamDoneMap.begin();
    for (; runtimeDone != streamDoneMap.end(); ++runtimeDone, ++snapshotDone)
    {
        if (runtimeDone->first.empty() || snapshotDone->first.empty())
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_EMPTY_DONE_SOURCE";
            return false;
        }
        if (runtimeDone->first != snapshotDone->first)
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_DONE_INVENTORY_MISMATCH";
            return false;
        }
    }
    if (runtimeStreams.size() != envelope.streams.size())
    {
        failureReason = "RINEX_STREAM_CHECKPOINT_INVENTORY_COUNT_MISMATCH";
        return false;
    }
    string previousStableKey;
    for (std::size_t index = 0; index < envelope.streams.size(); ++index)
    {
        const auto& snapshot = envelope.streams[index];
        const auto& runtime = runtimeStreams[index];
        if (!validRinexStreamState(snapshot, failureReason))
        {
            return false;
        }
        if (!previousStableKey.empty()
            && snapshot.stableKey <= previousStableKey)
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_INVENTORY_NOT_CANONICAL";
            return false;
        }
        previousStableKey = snapshot.stableKey;
        if (snapshot.stableKey != runtime.stableKey
            || snapshot.receiverId != runtime.receiverId
            || snapshot.sourceString != runtime.sourceString
            || snapshot.canonicalPath != runtime.canonicalPath)
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_INVENTORY_MISMATCH";
            return false;
        }
        if (snapshot.fileSize != runtime.fileSize
            || snapshot.fileSha256 != runtime.fileSha256)
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_FILE_IDENTITY_MISMATCH";
            return false;
        }
        if (envelope.streamDoneMap.find(snapshot.sourceString)
            == envelope.streamDoneMap.end())
        {
            failureReason = "RINEX_STREAM_CHECKPOINT_ACTIVE_SOURCE_MISSING_DONE_STATE";
            return false;
        }
    }
    return true;
}

struct PreparedRinexFileStream
{
    RuntimeRinexFileStream runtime;
    std::int64_t filePosition = 0;
    E_ObsAgeCode observationAgeCode = E_ObsAgeCode::UNKNOWN;
    GTime lastReadTime = GTime::noTime();
    double observationInterval = 0;
    bool pseudoReceiver = false;
    char rinexContentType = 0;
    double rinexVersion = 0;
    E_Sys navigationSystem = E_Sys::NONE;
    E_TimeSys timeSystem = E_TimeSys::NONE;
    map<E_Sys, map<int, CodeType>> systemCodeTypes;
    ObsList temporaryObservations;
    RinexStation station;
    list<ObsList> futureObservationQueue;
};

bool prepareRinexStream(
    const RuntimeRinexFileStream& runtime,
    const ZhangRinexFileStreamCheckpointState& snapshot,
    PreparedRinexFileStream& output,
    string& failureReason)
{
    output.runtime = runtime;
    output.filePosition = snapshot.filePosition;
    output.observationAgeCode =
        static_cast<E_ObsAgeCode>(snapshot.observationAgeCode);
    output.lastReadTime = restoreZhangCheckpointTime(snapshot.lastReadTime);
    output.observationInterval = snapshot.observationInterval;
    output.pseudoReceiver = snapshot.pseudoReceiver;
    output.rinexContentType = snapshot.rinexContentType;
    output.rinexVersion = snapshot.rinexVersion;
    output.navigationSystem = static_cast<E_Sys>(snapshot.navigationSystem);
    output.timeSystem = static_cast<E_TimeSys>(snapshot.timeSystem);
    for (const auto& [system, codes] : snapshot.systemCodeTypes)
    for (const auto& [index, code] : codes)
    {
        output.systemCodeTypes[static_cast<E_Sys>(system)][index] = {
            code.type,
            static_cast<E_ObsCode>(code.code),
            static_cast<E_ObsCode2>(code.code2)};
    }
    if (!restoreObservationEpoch(
            snapshot.temporaryObservations,
            output.temporaryObservations,
            failureReason))
    {
        return false;
    }
    output.station = restoreStation(snapshot.station);
    for (const auto& storedEpoch : snapshot.futureObservationQueue)
    {
        ObsList epoch;
        if (!restoreObservationEpoch(storedEpoch, epoch, failureReason))
        {
            return false;
        }
        output.futureObservationQueue.push_back(std::move(epoch));
    }
    return true;
}

void commitPreparedRinexStream(PreparedRinexFileStream& prepared)
{
    auto& observationStream = *prepared.runtime.observationStream;
    auto& fileStream = *prepared.runtime.fileStream;
    auto& parser = *prepared.runtime.rinexParser;

    // No FileState can remain alive across parse(), so closing this persistent
    // handle cannot invalidate a live parser view.  The next parse reopens and
    // seeks to the restored byte position.
    fileStream.persistentStream.close();
    fileStream.persistentStream.clear();
    fileStream.filePos = static_cast<long int>(prepared.filePosition);

    observationStream.obsAgeCode = prepared.observationAgeCode;
    observationStream.lastReadTime = prepared.lastReadTime;
    observationStream.interval = prepared.observationInterval;
    observationStream.isPseudoRec = prepared.pseudoReceiver;

    parser.ctype = prepared.rinexContentType;
    parser.version = prepared.rinexVersion;
    parser.nav_system = prepared.navigationSystem;
    parser.time_system = prepared.timeSystem;
    parser.sysCodeTypes.swap(prepared.systemCodeTypes);
    parser.tempObsList.swap(prepared.temporaryObservations);
    std::swap(parser.rnxRec, prepared.station);
    parser.obsListList.swap(prepared.futureObservationQueue);
}

void countSnapshot(
    const ZhangRinexFileStreamsCheckpointEnvelope& envelope,
    ZhangInputCheckpointResult& result)
{
    result.streamCount = envelope.streams.size();
    result.streamDoneStateCount = envelope.streamDoneMap.size();
    for (const auto& stream : envelope.streams)
    {
        if (!stream.temporaryObservations.empty())
        {
            ++result.queuedEpochCount;
            result.queuedObservationCount +=
                stream.temporaryObservations.size();
        }
        result.queuedEpochCount += stream.futureObservationQueue.size();
        for (const auto& epoch : stream.futureObservationQueue)
        {
            result.queuedObservationCount += epoch.size();
        }
    }
}
}  // namespace

ZhangInputCheckpointResult makeZhangPeaPostEpochCheckpointState(
    int completedEpoch,
    const ZhangCheckpointTime& completedTsync,
    double epochIntervalSeconds,
    ZhangPeaControllerCheckpointState& state)
{
    ZhangInputCheckpointResult result;
    state = {};
    state.completedEpoch = completedEpoch;
    state.completedTsync = completedTsync;
    state.nextEpoch = completedEpoch + 1;
    state.epochIntervalSeconds = epochIntervalSeconds;
    const GTime completed = restoreZhangCheckpointTime(completedTsync);
    state.nextTsync = captureZhangCheckpointTime(completed + epochIntervalSeconds);
    state.boundary = ZHANG_PEA_POST_EPOCH_BOUNDARY;
    state.resumePolicy = ZHANG_PEA_RESUME_NEXT_EPOCH;
    if (!validControllerState(state, epochIntervalSeconds, result.failureReason))
    {
        state = {};
        return result;
    }
    result.valid = true;
    return result;
}

ZhangInputCheckpointResult exportZhangPeaControllerCheckpointSection(
    const ZhangPeaControllerCheckpointState& state,
    string& payload)
{
    ZhangInputCheckpointResult result;
    payload.clear();
    if (!validControllerState(state, 0, result.failureReason))
    {
        return result;
    }
    ZhangPeaControllerCheckpointEnvelope envelope;
    envelope.state = state;
    if (!serializePayload(
            envelope,
            payload,
            result.failureReason,
            "PEA_CONTROLLER_CHECKPOINT"))
    {
        return result;
    }
    result.valid = true;
    return result;
}

ZhangInputCheckpointResult preflightZhangPeaControllerCheckpointSection(
    const string& payload,
    double expectedEpochIntervalSeconds,
    ZhangPeaControllerCheckpointRestorePlan& plan)
{
    ZhangInputCheckpointResult result;
    plan = {};
    ZhangPeaControllerCheckpointEnvelope envelope;
    if (!decodeAndValidateController(
            payload,
            expectedEpochIntervalSeconds,
            envelope,
            result.failureReason))
    {
        return result;
    }
    plan.payload = payload;
    plan.payloadSha256 = zhangCheckpointSha256(payload);
    plan.state = envelope.state;
    result.valid = true;
    return result;
}

ZhangInputCheckpointResult commitZhangPeaControllerCheckpointSection(
    const ZhangPeaControllerCheckpointRestorePlan& plan,
    ZhangPeaControllerCheckpointState& restoredState)
{
    ZhangInputCheckpointResult result;
    if (plan.payload.empty()
        || plan.payloadSha256 != zhangCheckpointSha256(plan.payload))
    {
        result.failureReason = "PEA_CONTROLLER_CHECKPOINT_PLAN_DIGEST_MISMATCH";
        return result;
    }
    ZhangPeaControllerCheckpointEnvelope envelope;
    if (!decodeAndValidateController(
            plan.payload, 0, envelope, result.failureReason))
    {
        return result;
    }
    string plannedStatePayload;
    string decodedStatePayload;
    if (!serializePayload(
            plan.state,
            plannedStatePayload,
            result.failureReason,
            "PEA_CONTROLLER_CHECKPOINT_PLAN_STATE")
        || !serializePayload(
            envelope.state,
            decodedStatePayload,
            result.failureReason,
            "PEA_CONTROLLER_CHECKPOINT_DECODED_STATE")
        || plannedStatePayload != decodedStatePayload)
    {
        if (result.failureReason.empty())
        {
            result.failureReason = "PEA_CONTROLLER_CHECKPOINT_PLAN_STATE_MISMATCH";
        }
        return result;
    }
    restoredState = envelope.state;
    result.valid = true;
    return result;
}

ZhangInputCheckpointResult exportZhangRinexFileStreamsCheckpointSection(
    const std::multimap<string, StreamParserPtr>& streams,
    const map<string, bool>& streamDoneMap,
    string& payload)
{
    ZhangInputCheckpointResult result;
    payload.clear();
    vector<RuntimeRinexFileStream> runtimeStreams;
    if (!inspectRuntimeStreams(streams, runtimeStreams, result.failureReason))
    {
        return result;
    }
    if (runtimeStreams.empty())
    {
        result.failureReason = "RINEX_STREAM_CHECKPOINT_EMPTY_INVENTORY";
        return result;
    }
    if (streamDoneMap.empty()
        || std::any_of(
            streamDoneMap.begin(),
            streamDoneMap.end(),
            [](const auto& entry) { return entry.first.empty(); }))
    {
        result.failureReason = "RINEX_STREAM_CHECKPOINT_INVALID_DONE_INVENTORY";
        return result;
    }
    for (const auto& runtime : runtimeStreams)
    {
        if (streamDoneMap.find(runtime.sourceString) == streamDoneMap.end())
        {
            result.failureReason =
                "RINEX_STREAM_CHECKPOINT_ACTIVE_SOURCE_MISSING_DONE_STATE";
            return result;
        }
    }
    ZhangRinexFileStreamsCheckpointEnvelope envelope;
    envelope.streamDoneMap = streamDoneMap;
    envelope.streams.reserve(runtimeStreams.size());
    for (const auto& runtime : runtimeStreams)
    {
        ZhangRinexFileStreamCheckpointState snapshot;
        if (!checkpointRuntimeStream(runtime, snapshot, result.failureReason)
            || !validRinexStreamState(snapshot, result.failureReason))
        {
            return result;
        }
        envelope.streams.push_back(std::move(snapshot));
    }
    if (!serializePayload(
            envelope,
            payload,
            result.failureReason,
            "RINEX_STREAM_CHECKPOINT"))
    {
        return result;
    }
    countSnapshot(envelope, result);
    result.valid = true;
    return result;
}

ZhangInputCheckpointResult preflightZhangRinexFileStreamsCheckpointSection(
    const std::multimap<string, StreamParserPtr>& streams,
    const map<string, bool>& streamDoneMap,
    const string& payload,
    ZhangRinexFileStreamsCheckpointRestorePlan& plan)
{
    ZhangInputCheckpointResult result;
    plan = {};
    ZhangRinexFileStreamsCheckpointEnvelope envelope;
    vector<RuntimeRinexFileStream> runtimeStreams;
    if (!decodeAndValidateRinexStreams(
            streams,
            streamDoneMap,
            payload,
            envelope,
            runtimeStreams,
            result.failureReason))
    {
        return result;
    }
    countSnapshot(envelope, result);
    plan.payload = payload;
    plan.payloadSha256 = zhangCheckpointSha256(payload);
    plan.streamCount = result.streamCount;
    plan.streamDoneStateCount = result.streamDoneStateCount;
    plan.queuedEpochCount = result.queuedEpochCount;
    plan.queuedObservationCount = result.queuedObservationCount;
    result.valid = true;
    return result;
}

ZhangInputCheckpointResult commitZhangRinexFileStreamsCheckpointSection(
    std::multimap<string, StreamParserPtr>& streams,
    map<string, bool>& streamDoneMap,
    const ZhangRinexFileStreamsCheckpointRestorePlan& plan)
{
    ZhangInputCheckpointResult result;
    if (plan.payload.empty()
        || plan.payloadSha256 != zhangCheckpointSha256(plan.payload))
    {
        result.failureReason = "RINEX_STREAM_CHECKPOINT_PLAN_DIGEST_MISMATCH";
        return result;
    }
    ZhangRinexFileStreamsCheckpointEnvelope envelope;
    vector<RuntimeRinexFileStream> runtimeStreams;
    if (!decodeAndValidateRinexStreams(
            streams,
            streamDoneMap,
            plan.payload,
            envelope,
            runtimeStreams,
            result.failureReason))
    {
        return result;
    }
    countSnapshot(envelope, result);
    if (plan.streamCount != result.streamCount
        || plan.streamDoneStateCount != result.streamDoneStateCount
        || plan.queuedEpochCount != result.queuedEpochCount
        || plan.queuedObservationCount != result.queuedObservationCount)
    {
        result.failureReason = "RINEX_STREAM_CHECKPOINT_PLAN_COUNT_MISMATCH";
        return result;
    }

    map<string, bool> preparedDoneMap;
    vector<PreparedRinexFileStream> prepared;
    try
    {
        preparedDoneMap = envelope.streamDoneMap;
        prepared.reserve(envelope.streams.size());
        for (std::size_t index = 0; index < envelope.streams.size(); ++index)
        {
            PreparedRinexFileStream stream;
            if (!prepareRinexStream(
                    runtimeStreams[index],
                    envelope.streams[index],
                    stream,
                    result.failureReason))
            {
                return result;
            }
            prepared.push_back(std::move(stream));
        }
    }
    catch (const std::exception& exception)
    {
        result.failureReason =
            string("RINEX_STREAM_CHECKPOINT_PREPARE_FAILED:") + exception.what();
        return result;
    }

    // All allocation, decoding, file identity checks and observation rebuilds
    // have completed.  The remaining operations close handles, assign scalars
    // and swap already-allocated containers.
    for (auto& stream : prepared)
    {
        commitPreparedRinexStream(stream);
    }
    streamDoneMap.swap(preparedDoneMap);
    result.valid = true;
    return result;
}
