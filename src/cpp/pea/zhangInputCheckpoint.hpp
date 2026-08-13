#pragma once

#include <cstddef>
#include <cstdint>
#include <map>
#include <string>

#include "common/streamParser.hpp"
#include "common/zhangCheckpoint.hpp"

inline constexpr std::uint32_t ZHANG_PEA_CONTROLLER_CHECKPOINT_SCHEMA_VERSION = 1;
inline constexpr std::uint32_t ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SCHEMA_VERSION = 1;

inline constexpr const char* ZHANG_PEA_CONTROLLER_CHECKPOINT_SECTION_NAME =
    "pea.controller.v1";
inline constexpr const char* ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SECTION_NAME =
    "pea.rinex_file_streams.v1";

inline constexpr const char* ZHANG_PEA_POST_EPOCH_BOUNDARY =
    "POST_EPOCH_COMMITTED";
inline constexpr const char* ZHANG_PEA_RESUME_NEXT_EPOCH =
    "RESUME_NEXT_EPOCH";

/** Pointer-free controller state at the only E29 checkpoint boundary that is
 * currently supported.  completedTsync is the epoch that has already been
 * committed.  nextTsync is redundant by design and is checked against
 * completedTsync + epochIntervalSeconds before export and import. */
struct ZhangPeaControllerCheckpointState
{
    int completedEpoch = 0;
    ZhangCheckpointTime completedTsync;
    int nextEpoch = 0;
    ZhangCheckpointTime nextTsync;
    double epochIntervalSeconds = 0;
    std::string boundary = ZHANG_PEA_POST_EPOCH_BOUNDARY;
    std::string resumePolicy = ZHANG_PEA_RESUME_NEXT_EPOCH;

    template <class ARCHIVE>
    void serialize(ARCHIVE& ar, const unsigned int& version)
    {
        ar & completedEpoch;
        ar & completedTsync;
        ar & nextEpoch;
        ar & nextTsync;
        ar & epochIntervalSeconds;
        ar & boundary;
        ar & resumePolicy;
    }
};

struct ZhangInputCheckpointResult
{
    bool valid = false;
    std::string failureReason;
    std::size_t streamCount = 0;
    std::size_t streamDoneStateCount = 0;
    std::size_t queuedEpochCount = 0;
    std::size_t queuedObservationCount = 0;
};

/** The plan deliberately retains the exact validated bytes.  Commit verifies
 * their digest, decodes them again, reconstructs every queue off to the side,
 * and only then swaps state into the live streams. */
struct ZhangPeaControllerCheckpointRestorePlan
{
    std::string payload;
    std::string payloadSha256;
    ZhangPeaControllerCheckpointState state;
};

struct ZhangRinexFileStreamsCheckpointRestorePlan
{
    std::string payload;
    std::string payloadSha256;
    std::size_t streamCount = 0;
    std::size_t streamDoneStateCount = 0;
    std::size_t queuedEpochCount = 0;
    std::size_t queuedObservationCount = 0;
};

/** Construct the canonical post-epoch/resume-next controller state.  All
 * persisted times use ZhangCheckpointTime; this helper is the only place that
 * derives nextTsync. */
ZhangInputCheckpointResult makeZhangPeaPostEpochCheckpointState(
    int completedEpoch,
    const ZhangCheckpointTime& completedTsync,
    double epochIntervalSeconds,
    ZhangPeaControllerCheckpointState& state);

ZhangInputCheckpointResult exportZhangPeaControllerCheckpointSection(
    const ZhangPeaControllerCheckpointState& state,
    std::string& payload);

ZhangInputCheckpointResult preflightZhangPeaControllerCheckpointSection(
    const std::string& payload,
    double expectedEpochIntervalSeconds,
    ZhangPeaControllerCheckpointRestorePlan& plan);

ZhangInputCheckpointResult commitZhangPeaControllerCheckpointSection(
    const ZhangPeaControllerCheckpointRestorePlan& plan,
    ZhangPeaControllerCheckpointState& restoredState);

/** Export every configured input stream.  E29 currently supports only
 * ObsStream + FileStream + RinexParser.  Encountering any other stream or
 * parser is an explicit failure, because omitting it would make restart
 * provenance incomplete.  Repeated instances are identified by
 * (receiver, source, canonical path, occurrence ordinal), not rejected. */
ZhangInputCheckpointResult exportZhangRinexFileStreamsCheckpointSection(
    const std::multimap<std::string, StreamParserPtr>& streams,
    const std::map<std::string, bool>& streamDoneMap,
    std::string& payload);

/** Read-only validation.  The configured stream inventory, streamDOAMap key
 * inventory, canonical paths, file sizes and SHA-256 values must match
 * exactly.  Saved done/dead values are restored only during commit. */
ZhangInputCheckpointResult preflightZhangRinexFileStreamsCheckpointSection(
    const std::multimap<std::string, StreamParserPtr>& streams,
    const std::map<std::string, bool>& streamDoneMap,
    const std::string& payload,
    ZhangRinexFileStreamsCheckpointRestorePlan& plan);

/** Revalidates the plan and inventory, prepares all parser/queue objects and
 * the complete source->done map, and then commits using swaps.  A failed
 * validation leaves every stream and streamDoneMap intact. */
ZhangInputCheckpointResult commitZhangRinexFileStreamsCheckpointSection(
    std::multimap<std::string, StreamParserPtr>& streams,
    std::map<std::string, bool>& streamDoneMap,
    const ZhangRinexFileStreamsCheckpointRestorePlan& plan);
