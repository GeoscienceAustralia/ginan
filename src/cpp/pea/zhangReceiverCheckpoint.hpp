#pragma once

#include <cstddef>
#include <cstdint>
#include <string>

struct Navigation;
struct ReceiverMap;

inline constexpr std::uint32_t
	ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SCHEMA_VERSION = 1;
inline constexpr std::uint32_t
	ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SCHEMA_VERSION = 1;

inline constexpr const char*
	ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SECTION_NAME =
		"pea.receiver_runtime.v1";
inline constexpr const char*
	ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SECTION_NAME =
		"pea.satellite_runtime.v1";

/** Result of one receiver/satellite runtime checkpoint operation. */
struct ZhangReceiverRuntimeCheckpointResult
{
	bool valid = false;
	std::string failureReason;
	std::size_t receiverCount = 0;
	std::size_t satelliteStatusCount = 0;
	std::size_t signalStatusCount = 0;
	std::size_t resetObservationCount = 0;
	std::size_t dynamicAliasReceiverCount = 0;
	std::size_t dynamicAliasCount = 0;
};

struct ZhangSatelliteRuntimeCheckpointResult
{
	bool valid = false;
	std::string failureReason;
	std::size_t satelliteCount = 0;
	std::size_t satelliteAliasCount = 0;
	std::size_t svnHistoryCount = 0;
};

/** A preflight plan retains the exact validated bytes.  Import rechecks the
 * digest, runtime identity, configured object inventory and all pointer
 * rebinding prerequisites before changing live state. */
struct ZhangReceiverRuntimeCheckpointRestorePlan
{
	std::string payload;
	std::string payloadSha256;
	std::string runtimeId;
	std::size_t receiverCount = 0;
	std::size_t satelliteStatusCount = 0;
	std::size_t signalStatusCount = 0;
	std::size_t resetObservationCount = 0;
	std::size_t dynamicAliasReceiverCount = 0;
	std::size_t dynamicAliasCount = 0;
};

struct ZhangSatelliteRuntimeCheckpointRestorePlan
{
	std::string payload;
	std::string payloadSha256;
	std::string runtimeId;
	std::size_t satelliteCount = 0;
	std::size_t satelliteAliasCount = 0;
	std::size_t svnHistoryCount = 0;
};

/** Export the complete persistent receiver state needed by E29.  This API is
 * valid only at POST_EPOCH_COMMITTED: rec.obsList is deliberately represented
 * by an audited RESET_POST_EPOCH_WORKSET policy and is empty after import.
 * Derived tide/EOP caches are likewise reset.  SINEX pointers, KF callbacks,
 * filter-chunk trace pointers and KFKey::rec_ptr never enter the payload;
 * export records sufficient binding fingerprints for fail-closed rebinding.
 * The only supported SPP callback is the named deweightMeas callback, which is
 * reinstalled from the exact binary; unknown callback sets are UNSUPPORTED.
 * acsConfig.customAliasesMap is restored with this partition and both derived
 * option caches are invalidated.  The completed-epoch ready flag is retained
 * only as audit data; the resume contract always imports ready=false. */
ZhangReceiverRuntimeCheckpointResult
exportZhangReceiverRuntimeCheckpointSection(
	const ReceiverMap& receivers,
	const std::string& runtimeId,
	std::string& payload);

/** Decode and validate without mutating receivers.  Receiver inventory must
 * exactly match the configured destination, and all non-serializable binding
 * targets must already have been installed by the hashed configuration. */
ZhangReceiverRuntimeCheckpointResult
preflightZhangReceiverRuntimeCheckpointSection(
	const ReceiverMap& configuredReceivers,
	const std::string& runtimeId,
	const std::string& payload,
	ZhangReceiverRuntimeCheckpointRestorePlan& plan);

/** Import a previously preflighted receiver section in place.  Receiver object
 * addresses are retained so network KFKey::rec_ptr bindings remain valid. */
ZhangReceiverRuntimeCheckpointResult
importZhangReceiverRuntimeCheckpointSection(
	ReceiverMap& receivers,
	const std::string& runtimeId,
	const ZhangReceiverRuntimeCheckpointRestorePlan& plan);

/** Export the E29 satellite runtime partition at POST_EPOCH_COMMITTED.  The
 * supported scope is the precise-product path: SatNav attitude, wavelengths,
 * last propagated state,
 * error counters, SVN/block aliases, identity histories and the complete
 * epoch-dependent nav.erp.filterValues estimate are retained.  Static ERP
 * input tables stay configuration/input-manifest owned.
 * Non-empty SSR/SBAS runtime is rejected as UNSUPPORTED instead of omitted. */
ZhangSatelliteRuntimeCheckpointResult
exportZhangSatelliteRuntimeCheckpointSection(
	const Navigation& navigation,
	const std::string& runtimeId,
	std::string& payload);

ZhangSatelliteRuntimeCheckpointResult
preflightZhangSatelliteRuntimeCheckpointSection(
	const Navigation& configuredNavigation,
	const std::string& runtimeId,
	const std::string& payload,
	ZhangSatelliteRuntimeCheckpointRestorePlan& plan);

/** Import in place so existing SatNav addresses remain stable.  SatPos pointer
 * fields are reconstructed only when the payload proved that they referred to
 * their owning SatNav; a SatStat pointer in satellite-global SatPos is
 * unsupported. */
ZhangSatelliteRuntimeCheckpointResult
importZhangSatelliteRuntimeCheckpointSection(
	Navigation& navigation,
	const std::string& runtimeId,
	const ZhangSatelliteRuntimeCheckpointRestorePlan& plan);
