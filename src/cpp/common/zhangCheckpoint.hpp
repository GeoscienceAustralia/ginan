#pragma once

#include <array>
#include <cstdint>
#include <functional>
#include <map>
#include <string>
#include <vector>

#include <boost/serialization/array.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/vector.hpp>

#include "common/algebra.hpp"

struct Receiver;

/** Versioned, deliberately scoped checkpoint format for the frozen E29
 * GPS L1C/L2W Zhang full-rank experiments.  It is not the RTS archive and is
 * not a general PEA process snapshot. */
inline constexpr std::uint32_t ZHANG_CHECKPOINT_FORMAT_VERSION = 1;
inline constexpr std::uint32_t ZHANG_CHECKPOINT_CORE_SCHEMA_VERSION = 1;
inline constexpr const char* ZHANG_CHECKPOINT_RUNTIME_ID_METADATA =
	"zhang_checkpoint_runtime_id";
/** Stable identity for a disposable branch derived from one authoritative
 * runtime.  Modules that need branch-local state may prefer this key, while
 * owner-only ledgers deliberately continue to use the root runtime ID. */
inline constexpr const char* ZHANG_CHECKPOINT_RUNTIME_BRANCH_ID_METADATA =
	"zhang_checkpoint_runtime_branch_id";

struct ZhangCheckpointTime
{
	std::array<unsigned char, sizeof(long double)> bigTimeBytes{};

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & bigTimeBytes;
	}
};

struct ZhangCheckpointManifest
{
	std::uint32_t formatVersion = ZHANG_CHECKPOINT_FORMAT_VERSION;
	std::uint32_t coreSchemaVersion = ZHANG_CHECKPOINT_CORE_SCHEMA_VERSION;
	std::string experimentMode = "E29_GPS_L1C_L2W_ZHANG_FULL_RANK";
	std::string runtimeId;
	std::string checkpointId;
	std::string parentCheckpointId;
	std::string epoch;
	std::string binarySha256;
	std::string configSha256;
	std::string inputManifestSha256;
	std::string configText;
	std::string inputManifestText;
	std::string platformFingerprint;
	std::string compilerFingerprint;
	std::string linearAlgebraFingerprint;
	std::string endianness;
	std::string createdUtc;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & formatVersion;
		ar & coreSchemaVersion;
		ar & experimentMode;
		ar & runtimeId;
		ar & checkpointId;
		ar & parentCheckpointId;
		ar & epoch;
		ar & binarySha256;
		ar & configSha256;
		ar & inputManifestSha256;
		ar & configText;
		ar & inputManifestText;
		ar & platformFingerprint;
		ar & compilerFingerprint;
		ar & linearAlgebraFingerprint;
		ar & endianness;
		ar & createdUtc;
	}
};

struct ZhangCheckpointFilterChunk
{
	std::string id;
	int begX = 0;
	int numX = 0;
	int begH = 0;
	int numH = -1;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & id;
		ar & begX;
		ar & numX;
		ar & begH;
		ar & numH;
	}
};

/** Pointer-free KF key used only by the E29 checkpoint schema.  The generic
 * KFKey archive intentionally omits estimatedTime, but outage decisions use
 * it after restart, so the checkpoint must persist it explicitly. */
struct ZhangCheckpointKfKey
{
	KF type = KF::NONE;
	SatSys satellite;
	std::string receiver;
	int number = 0;
	std::string comment;
	ZhangCheckpointTime estimatedTime;
	/** Empty when the runtime key had no receiver pointer.  Otherwise this is
	 * the stable Receiver::id used to rebind the fresh-process address. */
	std::string receiverPointerId;

	bool operator<(const ZhangCheckpointKfKey& other) const
	{
		if (receiver != other.receiver)
		{
			return receiver < other.receiver;
		}
		if (satellite != other.satellite)
		{
			return satellite < other.satellite;
		}
		if (type != other.type)
		{
			return type < other.type;
		}
		return number < other.number;
	}

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & type;
		ar & satellite;
		ar & receiver;
		ar & number;
		ar & comment;
		ar & estimatedTime;
		ar & receiverPointerId;
	}
};

/** Serializable KF core.  Callback functions and pointer fields are
 * intentionally excluded: the destination is configured from the exact
 * hashed YAML/binary first, then this snapshot overwrites persistent numeric
 * and discrete filter state while retaining the freshly installed callbacks. */
struct ZhangCheckpointKfCore
{
	ZhangCheckpointTime time;
	VectorXd x;
	MatrixXd P;
	VectorXd dx;
	VectorXd prefitRatios;
	VectorXd postfitRatios;

	std::map<ZhangCheckpointKfKey, int> kfIndexMap;
	std::map<ZhangCheckpointKfKey,
		std::map<ZhangCheckpointKfKey, std::map<int, double>>> stateTransitionMap;
	std::map<ZhangCheckpointKfKey, double> gaussMarkovTauMap;
	std::map<ZhangCheckpointKfKey, double> gaussMarkovMuMap;
	std::map<ZhangCheckpointKfKey, double> procNoiseMap;
	std::map<ZhangCheckpointKfKey, double> initNoiseMap;
	std::map<ZhangCheckpointKfKey, double> sigmaMaxMap;
	std::map<ZhangCheckpointKfKey, double> outageLimitMap;
	std::map<ZhangCheckpointKfKey, Exponential> exponentialNoiseMap;
	std::map<ZhangCheckpointKfKey,
		std::map<ZhangCheckpointKfKey, double>> pseudoStateMap;
	std::map<ZhangCheckpointKfKey, ZhangCheckpointKfKey> pseudoParentMap;
	std::map<ZhangCheckpointKfKey, int> errorCountMap;
	std::map<std::string, ZhangCheckpointFilterChunk> filterChunkMap;
	std::map<std::string, std::string> metaDataMap;

	bool lsqRequired = false;
	bool sigmaPass = false;
	bool chiQCPass = false;
	double chi2 = 0;
	int dof = 0;
	double chi2PerDof = 0;
	double qc = 0;
	std::string id;
	std::string rtsBasename;
	bool outputResiduals = false;
	bool outputMongoMeasurements = false;
	std::map<std::string, int> statisticsMap;
	std::map<std::string, int> statisticsMapSum;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & time;
		ar & x;
		ar & P;
		ar & dx;
		ar & prefitRatios;
		ar & postfitRatios;
		ar & kfIndexMap;
		ar & stateTransitionMap;
		ar & gaussMarkovTauMap;
		ar & gaussMarkovMuMap;
		ar & procNoiseMap;
		ar & initNoiseMap;
		ar & sigmaMaxMap;
		ar & outageLimitMap;
		ar & exponentialNoiseMap;
		ar & pseudoStateMap;
		ar & pseudoParentMap;
		ar & errorCountMap;
		ar & filterChunkMap;
		ar & metaDataMap;
		ar & lsqRequired;
		ar & sigmaPass;
		ar & chiQCPass;
		ar & chi2;
		ar & dof;
		ar & chi2PerDof;
		ar & qc;
		ar & id;
		ar & rtsBasename;
		ar & outputResiduals;
		ar & outputMongoMeasurements;
		ar & statisticsMap;
		ar & statisticsMapSum;
	}
};

/** Opaque module-owned section.  No pointer identity may appear in payload;
 * module importers rebind the section to manifest.runtimeId and the restored
 * KFState instance. */
struct ZhangCheckpointSection
{
	std::uint32_t schemaVersion = 0;
	std::string payload;
	std::string sha256;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & schemaVersion;
		ar & payload;
		ar & sha256;
	}
};

struct ZhangCheckpointBundle
{
	ZhangCheckpointManifest manifest;
	ZhangCheckpointKfCore kfCore;
	std::map<std::string, ZhangCheckpointSection> sections;

	template <class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & manifest;
		ar & kfCore;
		ar & sections;
	}
};

struct ZhangCheckpointExpectations
{
	std::string experimentMode;
	std::string binarySha256;
	std::string configSha256;
	std::string inputManifestSha256;
	std::string platformFingerprint;
	std::string compilerFingerprint;
	std::string linearAlgebraFingerprint;
	std::string endianness;
};

struct ZhangCheckpointIoResult
{
	bool valid = false;
	std::string failureReason;
	std::string payloadSha256;
	std::uint64_t payloadBytes = 0;
};

struct ZhangCheckpointSectionRequirement
{
	std::string name;
	std::uint32_t schemaVersion = 0;
};

ZhangCheckpointKfCore captureZhangCheckpointKfCore(const KFState& state);

ZhangCheckpointTime captureZhangCheckpointTime(GTime time);

GTime restoreZhangCheckpointTime(const ZhangCheckpointTime& stored);

bool bindZhangCheckpointRuntimeId(
	KFState& state,
	const std::string& runtimeId,
	std::string* failureReason = nullptr);

std::string zhangCheckpointRuntimeId(const KFState& state);

bool restoreZhangCheckpointKfCore(
	const ZhangCheckpointKfCore& snapshot,
	KFState& state,
	std::string* failureReason = nullptr);

using ZhangCheckpointReceiverResolver =
	std::function<Receiver*(const std::string&)>;

/** Restore the core while rebuilding only semantically recorded Receiver
 * pointers against the freshly configured ReceiverMap. */
bool restoreZhangCheckpointKfCoreWithReceiverResolver(
	const ZhangCheckpointKfCore& snapshot,
	KFState& state,
	const ZhangCheckpointReceiverResolver& receiverResolver,
	std::string* failureReason = nullptr);

std::string zhangCheckpointSha256(const std::string& bytes);

std::string zhangCheckpointFileSha256(
	const std::string& path,
	std::string* failureReason = nullptr);

std::string serializeZhangCheckpointSectionPayload(
	const ZhangCheckpointKfCore& snapshot);

ZhangCheckpointIoResult writeZhangCheckpointBundle(
	const std::string& path,
	const ZhangCheckpointBundle& bundle);

/** Verify the atomic bundle envelope and payload checksum without allocating
 * or deserialising a second full ZhangCheckpointBundle. */
ZhangCheckpointIoResult verifyZhangCheckpointBundleEnvelope(
	const std::string& path,
	const std::string& expectedPayloadSha256 = {});

ZhangCheckpointIoResult readZhangCheckpointBundle(
	const std::string& path,
	const ZhangCheckpointExpectations& expectations,
	ZhangCheckpointBundle& bundle);

bool zhangCheckpointKfCoreBitwiseEqual(
	const ZhangCheckpointKfCore& left,
	const ZhangCheckpointKfCore& right);

bool validateZhangCheckpointRequiredSections(
	const ZhangCheckpointBundle& bundle,
	const std::vector<ZhangCheckpointSectionRequirement>& requirements,
	std::string* failureReason = nullptr);

ZhangCheckpointIoResult writeZhangCheckpointManifestJson(
	const std::string& path,
	const ZhangCheckpointBundle& bundle);
