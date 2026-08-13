#include "common/zhangCheckpoint.hpp"

#include <algorithm>
#include <array>
#include <chrono>
#include <cctype>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <limits>
#include <set>
#include <sstream>
#include <system_error>
#include <vector>

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <openssl/evp.h>

#include "common/receiver.hpp"

namespace
{
constexpr std::array<char, 16> CHECKPOINT_MAGIC = {
	'G', 'I', 'N', 'A', 'N', '_', 'E', '2', '9', '_', 'C', 'K', 'P', 'T', '1', '\0'};
constexpr std::uint64_t MAX_CHECKPOINT_PAYLOAD_BYTES =
	static_cast<std::uint64_t>(64) * 1024 * 1024 * 1024;

template <typename TYPE>
std::string boostBinarySerialize(const TYPE& value)
{
	std::ostringstream output(std::ios::binary);
	boost::archive::binary_oarchive archive(output, boost::archive::no_header);
	archive << value;
	return output.str();
}

std::string boostBinarySerializeCoreWithoutTime(
	const ZhangCheckpointKfCore& snapshot)
{
	std::ostringstream output(std::ios::binary | std::ios::out);
	boost::archive::binary_oarchive archive(output, boost::archive::no_header);
	archive & snapshot.x;
	archive & snapshot.P;
	archive & snapshot.dx;
	archive & snapshot.prefitRatios;
	archive & snapshot.postfitRatios;
	archive & snapshot.kfIndexMap;
	archive & snapshot.stateTransitionMap;
	archive & snapshot.gaussMarkovTauMap;
	archive & snapshot.gaussMarkovMuMap;
	archive & snapshot.procNoiseMap;
	archive & snapshot.initNoiseMap;
	archive & snapshot.sigmaMaxMap;
	archive & snapshot.outageLimitMap;
	archive & snapshot.exponentialNoiseMap;
	archive & snapshot.pseudoStateMap;
	archive & snapshot.pseudoParentMap;
	archive & snapshot.errorCountMap;
	archive & snapshot.filterChunkMap;
	archive & snapshot.metaDataMap;
	archive & snapshot.lsqRequired;
	archive & snapshot.sigmaPass;
	archive & snapshot.chiQCPass;
	archive & snapshot.chi2;
	archive & snapshot.dof;
	archive & snapshot.chi2PerDof;
	archive & snapshot.qc;
	archive & snapshot.id;
	archive & snapshot.rtsBasename;
	archive & snapshot.outputResiduals;
	archive & snapshot.outputMongoMeasurements;
	archive & snapshot.statisticsMap;
	archive & snapshot.statisticsMapSum;
	return output.str();
}

template <typename TYPE>
bool boostBinaryDeserialize(
	const std::string& bytes,
	TYPE& value,
	std::string& failureReason)
{
	try
	{
		std::istringstream input(bytes, std::ios::binary);
		boost::archive::binary_iarchive archive(input, boost::archive::no_header);
		archive >> value;
		if (input.peek() != std::char_traits<char>::eof())
		{
			failureReason = "CHECKPOINT_PAYLOAD_TRAILING_BYTES";
			return false;
		}
		return true;
	}
	catch (const std::exception& exception)
	{
		failureReason = "CHECKPOINT_DESERIALIZE_FAILED:" +
			std::string(exception.what());
		return false;
	}
}

bool sha256LooksValid(const std::string& value)
{
	if (value.size() != 64)
	{
		return false;
	}
	return std::all_of(value.begin(), value.end(), [](unsigned char character)
	{
		return std::isxdigit(character) != 0;
	});
}

bool validateManifest(
	const ZhangCheckpointManifest& manifest,
	std::string& failureReason)
{
	if (manifest.formatVersion != ZHANG_CHECKPOINT_FORMAT_VERSION ||
		manifest.coreSchemaVersion != ZHANG_CHECKPOINT_CORE_SCHEMA_VERSION)
	{
		failureReason = "CHECKPOINT_SCHEMA_VERSION_UNSUPPORTED";
		return false;
	}
	if (manifest.experimentMode.empty() || manifest.runtimeId.empty() ||
		manifest.checkpointId.empty() || manifest.epoch.empty() ||
		manifest.createdUtc.empty() || manifest.configText.empty() ||
		manifest.inputManifestText.empty() ||
		manifest.platformFingerprint.empty() ||
		manifest.compilerFingerprint.empty() ||
		manifest.linearAlgebraFingerprint.empty() ||
		(manifest.endianness != "LITTLE" && manifest.endianness != "BIG"))
	{
		failureReason = "CHECKPOINT_MANIFEST_IDENTITY_MISSING";
		return false;
	}
	if (!sha256LooksValid(manifest.binarySha256) ||
		!sha256LooksValid(manifest.configSha256) ||
		!sha256LooksValid(manifest.inputManifestSha256))
	{
		failureReason = "CHECKPOINT_MANIFEST_PROVENANCE_INVALID";
		return false;
	}
	if (zhangCheckpointSha256(manifest.configText) !=
			manifest.configSha256 ||
		zhangCheckpointSha256(manifest.inputManifestText) !=
			manifest.inputManifestSha256)
	{
		failureReason = "CHECKPOINT_MANIFEST_CONTENT_HASH_MISMATCH";
		return false;
	}
	return true;
}

bool validateCoreRuntimeIdentity(
	const ZhangCheckpointManifest& manifest,
	const ZhangCheckpointKfCore& core,
	std::string& failureReason)
{
	auto found = core.metaDataMap.find(ZHANG_CHECKPOINT_RUNTIME_ID_METADATA);
	if (found == core.metaDataMap.end() || found->second != manifest.runtimeId)
	{
		failureReason = "CHECKPOINT_CORE_RUNTIME_ID_MISMATCH";
		return false;
	}
	return true;
}

bool validateCore(
	const ZhangCheckpointKfCore& snapshot,
	std::string& failureReason)
{
	const int dimension = snapshot.x.size();
	if (dimension <= 0 || snapshot.P.rows() != dimension ||
		snapshot.P.cols() != dimension || snapshot.dx.size() != dimension)
	{
		failureReason = "CHECKPOINT_CORE_DIMENSION_MISMATCH";
		return false;
	}
	if (!snapshot.x.allFinite() || !snapshot.P.allFinite() ||
		!snapshot.dx.allFinite())
	{
		failureReason = "CHECKPOINT_CORE_NONFINITE";
		return false;
	}
	if (static_cast<int>(snapshot.kfIndexMap.size()) != dimension)
	{
		failureReason = "CHECKPOINT_CORE_INDEX_COUNT_MISMATCH";
		return false;
	}
	std::vector<bool> seen(dimension, false);
	for (const auto& [key, index] : snapshot.kfIndexMap)
	{
		if (index < 0 || index >= dimension || seen[index])
		{
			failureReason = "CHECKPOINT_CORE_INDEX_NOT_BIJECTIVE";
			return false;
		}
		seen[index] = true;
	}
	return true;
}

bool matchesExpectation(
	const std::string& actual,
	const std::string& expected)
{
	return expected.empty() || actual == expected;
}

ZhangCheckpointKfKey checkpointKey(const KFKey& key)
{
	ZhangCheckpointKfKey stored;
	stored.type = key.type;
	stored.satellite = key.Sat;
	stored.receiver = key.str;
	stored.number = key.num;
	stored.comment = key.comment;
	stored.estimatedTime = captureZhangCheckpointTime(key.estimatedTime);
	if (key.rec_ptr)
	{
		stored.receiverPointerId = key.rec_ptr->id;
	}
	return stored;
}

KFKey runtimeKey(
	const ZhangCheckpointKfKey& stored,
	const ZhangCheckpointReceiverResolver& receiverResolver)
{
	KFKey key;
	key.type = stored.type;
	key.Sat = stored.satellite;
	key.str = stored.receiver;
	key.num = stored.number;
	key.comment = stored.comment;
	key.estimatedTime = restoreZhangCheckpointTime(stored.estimatedTime);
	key.rec_ptr = stored.receiverPointerId.empty() || !receiverResolver
		? nullptr
		: receiverResolver(stored.receiverPointerId);
	return key;
}
}

ZhangCheckpointTime captureZhangCheckpointTime(GTime time)
{
	ZhangCheckpointTime stored;
	static_assert(sizeof(stored.bigTimeBytes) == sizeof(time.bigTime));
	std::memcpy(
		stored.bigTimeBytes.data(), &time.bigTime, sizeof(time.bigTime));
	// x86-64 System V stores an 80-bit extended value in a 16-byte object;
	// the upper six bytes are padding and may contain indeterminate stack
	// data.  They are not part of the numerical value and must never enter a
	// deterministic payload or equality check.  The bundle is same-ABI only,
	// but canonicalising this known representation is still mandatory.
	if constexpr (
		sizeof(long double) == 16
		&& std::numeric_limits<long double>::digits == 64
		&& std::numeric_limits<long double>::max_exponent == 16384)
	{
		const std::uint16_t endianProbe = 0x0102;
		const bool littleEndian =
			reinterpret_cast<const unsigned char*>(&endianProbe)[0] == 0x02;
		if (littleEndian)
		{
			std::fill(
				stored.bigTimeBytes.begin() + 10,
				stored.bigTimeBytes.end(), 0);
		}
		else
		{
			std::fill(
				stored.bigTimeBytes.begin(),
				stored.bigTimeBytes.begin() + 6, 0);
		}
	}
	return stored;
}

GTime restoreZhangCheckpointTime(const ZhangCheckpointTime& stored)
{
	GTime time;
	static_assert(sizeof(stored.bigTimeBytes) == sizeof(time.bigTime));
	std::memcpy(
		&time.bigTime, stored.bigTimeBytes.data(), sizeof(time.bigTime));
	return time;
}

ZhangCheckpointKfCore captureZhangCheckpointKfCore(const KFState& state)
{
	ZhangCheckpointKfCore snapshot;
	snapshot.time = captureZhangCheckpointTime(state.time);
	snapshot.x = state.x;
	snapshot.P = state.P;
	snapshot.dx = state.dx;
	snapshot.prefitRatios = state.prefitRatios;
	snapshot.postfitRatios = state.postfitRatios;
	for (const auto& [key, index] : state.kfIndexMap)
	{
		snapshot.kfIndexMap[checkpointKey(key)] = index;
	}
	for (const auto& [destination, sources] : state.stateTransitionMap)
	for (const auto& [source, orders] : sources)
	{
		snapshot.stateTransitionMap[checkpointKey(destination)]
			[checkpointKey(source)] = orders;
	}
	for (const auto& [key, value] : state.gaussMarkovTauMap)
		snapshot.gaussMarkovTauMap[checkpointKey(key)] = value;
	for (const auto& [key, value] : state.gaussMarkovMuMap)
		snapshot.gaussMarkovMuMap[checkpointKey(key)] = value;
	for (const auto& [key, value] : state.procNoiseMap)
		snapshot.procNoiseMap[checkpointKey(key)] = value;
	for (const auto& [key, value] : state.initNoiseMap)
		snapshot.initNoiseMap[checkpointKey(key)] = value;
	for (const auto& [key, value] : state.sigmaMaxMap)
		snapshot.sigmaMaxMap[checkpointKey(key)] = value;
	for (const auto& [key, value] : state.outageLimitMap)
		snapshot.outageLimitMap[checkpointKey(key)] = value;
	for (const auto& [key, value] : state.exponentialNoiseMap)
		snapshot.exponentialNoiseMap[checkpointKey(key)] = value;
	for (const auto& [destination, sources] : state.pseudoStateMap)
	for (const auto& [source, value] : sources)
	{
		snapshot.pseudoStateMap[checkpointKey(destination)]
			[checkpointKey(source)] = value;
	}
	for (const auto& [key, parent] : state.pseudoParentMap)
	{
		snapshot.pseudoParentMap[checkpointKey(key)] = checkpointKey(parent);
	}
	for (const auto& [key, value] : state.errorCountMap)
		snapshot.errorCountMap[checkpointKey(key)] = value;
	for (const auto& [name, chunk] : state.filterChunkMap)
	{
		snapshot.filterChunkMap[name] = {
			chunk.id, chunk.begX, chunk.numX, chunk.begH, chunk.numH};
	}
	snapshot.metaDataMap = state.metaDataMap;
	snapshot.lsqRequired = state.lsqRequired;
	snapshot.sigmaPass = state.sigmaPass;
	snapshot.chiQCPass = state.chiQCPass;
	snapshot.chi2 = state.chi2;
	snapshot.dof = state.dof;
	snapshot.chi2PerDof = state.chi2PerDof;
	snapshot.qc = state.qc;
	snapshot.id = state.id;
	snapshot.rtsBasename = state.rts_basename;
	snapshot.outputResiduals = state.output_residuals;
	snapshot.outputMongoMeasurements = state.outputMongoMeasurements;
	snapshot.statisticsMap = state.statisticsMap;
	snapshot.statisticsMapSum = state.statisticsMapSum;
	return snapshot;
}

bool bindZhangCheckpointRuntimeId(
	KFState& state,
	const std::string& runtimeId,
	std::string* failureReason)
{
	if (runtimeId.empty())
	{
		if (failureReason)
		{
			*failureReason = "CHECKPOINT_RUNTIME_ID_EMPTY";
		}
		return false;
	}
	auto found = state.metaDataMap.find(ZHANG_CHECKPOINT_RUNTIME_ID_METADATA);
	if (found != state.metaDataMap.end() && !found->second.empty() &&
		found->second != runtimeId)
	{
		if (failureReason)
		{
			*failureReason = "CHECKPOINT_RUNTIME_ID_ALREADY_BOUND";
		}
		return false;
	}
	state.metaDataMap[ZHANG_CHECKPOINT_RUNTIME_ID_METADATA] = runtimeId;
	if (failureReason)
	{
		*failureReason = "NONE";
	}
	return true;
}

std::string zhangCheckpointRuntimeId(const KFState& state)
{
	auto found = state.metaDataMap.find(ZHANG_CHECKPOINT_RUNTIME_ID_METADATA);
	if (found == state.metaDataMap.end())
	{
		return {};
	}
	return found->second;
}

bool restoreZhangCheckpointKfCoreWithReceiverResolver(
	const ZhangCheckpointKfCore& snapshot,
	KFState& state,
	const ZhangCheckpointReceiverResolver& receiverResolver,
	std::string* failureReason)
{
	std::string localFailure;
	if (!validateCore(snapshot, localFailure))
	{
		if (failureReason)
		{
			*failureReason = localFailure;
		}
		return false;
	}
	auto bindingValid = [&](const ZhangCheckpointKfKey& key)
	{
		if (key.receiverPointerId.empty() || !receiverResolver)
		{
			return true;
		}
		Receiver* receiver = receiverResolver(key.receiverPointerId);
		return receiver && receiver->id == key.receiverPointerId;
	};
	auto rejectInvalidBinding = [&](const ZhangCheckpointKfKey& key)
	{
		if (bindingValid(key))
		{
			return false;
		}
		if (failureReason)
		{
			*failureReason =
				"CHECKPOINT_CORE_RECEIVER_POINTER_REBIND_FAILED:" +
				key.receiverPointerId;
		}
		return true;
	};
	for (const auto& [key, ignored] : snapshot.kfIndexMap)
		if (rejectInvalidBinding(key)) return false;
	for (const auto& [destination, sources] : snapshot.stateTransitionMap)
	{
		if (rejectInvalidBinding(destination)) return false;
		for (const auto& [source, ignored] : sources)
			if (rejectInvalidBinding(source)) return false;
	}
	auto validateScalarKeys = [&](const auto& values)
	{
		for (const auto& [key, ignored] : values)
			if (rejectInvalidBinding(key)) return false;
		return true;
	};
	if (!validateScalarKeys(snapshot.gaussMarkovTauMap)
	 || !validateScalarKeys(snapshot.gaussMarkovMuMap)
	 || !validateScalarKeys(snapshot.procNoiseMap)
	 || !validateScalarKeys(snapshot.initNoiseMap)
	 || !validateScalarKeys(snapshot.sigmaMaxMap)
	 || !validateScalarKeys(snapshot.outageLimitMap)
	 || !validateScalarKeys(snapshot.exponentialNoiseMap)
	 || !validateScalarKeys(snapshot.errorCountMap))
	{
		return false;
	}
	for (const auto& [destination, sources] : snapshot.pseudoStateMap)
	{
		if (rejectInvalidBinding(destination)) return false;
		for (const auto& [source, ignored] : sources)
			if (rejectInvalidBinding(source)) return false;
	}
	for (const auto& [key, parent] : snapshot.pseudoParentMap)
	{
		if (rejectInvalidBinding(key) || rejectInvalidBinding(parent))
			return false;
	}

	// Preserve configuration-installed callbacks and pointer fields.  Every
	// persistent numeric/discrete filter member is restored explicitly.
	state.time = restoreZhangCheckpointTime(snapshot.time);
	state.x = snapshot.x;
	state.P = snapshot.P;
	state.dx = snapshot.dx;
	state.prefitRatios = snapshot.prefitRatios;
	state.postfitRatios = snapshot.postfitRatios;
	state.kfIndexMap.clear();
	for (const auto& [key, index] : snapshot.kfIndexMap)
		state.kfIndexMap[runtimeKey(key, receiverResolver)] = index;
	state.stateTransitionMap.clear();
	for (const auto& [destination, sources] : snapshot.stateTransitionMap)
	for (const auto& [source, orders] : sources)
	{
		state.stateTransitionMap[runtimeKey(destination, receiverResolver)]
			[runtimeKey(source, receiverResolver)] = orders;
	}
	state.gaussMarkovTauMap.clear();
	for (const auto& [key, value] : snapshot.gaussMarkovTauMap)
		state.gaussMarkovTauMap[runtimeKey(key, receiverResolver)] = value;
	state.gaussMarkovMuMap.clear();
	for (const auto& [key, value] : snapshot.gaussMarkovMuMap)
		state.gaussMarkovMuMap[runtimeKey(key, receiverResolver)] = value;
	state.procNoiseMap.clear();
	for (const auto& [key, value] : snapshot.procNoiseMap)
		state.procNoiseMap[runtimeKey(key, receiverResolver)] = value;
	state.initNoiseMap.clear();
	for (const auto& [key, value] : snapshot.initNoiseMap)
		state.initNoiseMap[runtimeKey(key, receiverResolver)] = value;
	state.sigmaMaxMap.clear();
	for (const auto& [key, value] : snapshot.sigmaMaxMap)
		state.sigmaMaxMap[runtimeKey(key, receiverResolver)] = value;
	state.outageLimitMap.clear();
	for (const auto& [key, value] : snapshot.outageLimitMap)
		state.outageLimitMap[runtimeKey(key, receiverResolver)] = value;
	state.exponentialNoiseMap.clear();
	for (const auto& [key, value] : snapshot.exponentialNoiseMap)
		state.exponentialNoiseMap[runtimeKey(key, receiverResolver)] = value;
	state.pseudoStateMap.clear();
	for (const auto& [destination, sources] : snapshot.pseudoStateMap)
	for (const auto& [source, value] : sources)
	{
		state.pseudoStateMap[runtimeKey(destination, receiverResolver)]
			[runtimeKey(source, receiverResolver)] = value;
	}
	state.pseudoParentMap.clear();
	for (const auto& [key, parent] : snapshot.pseudoParentMap)
		state.pseudoParentMap[runtimeKey(key, receiverResolver)] =
			runtimeKey(parent, receiverResolver);
	state.errorCountMap.clear();
	for (const auto& [key, value] : snapshot.errorCountMap)
		state.errorCountMap[runtimeKey(key, receiverResolver)] = value;
	state.filterChunkMap.clear();
	for (const auto& [name, chunk] : snapshot.filterChunkMap)
	{
		FilterChunk restored;
		restored.id = chunk.id;
		restored.begX = chunk.begX;
		restored.numX = chunk.numX;
		restored.begH = chunk.begH;
		restored.numH = chunk.numH;
		state.filterChunkMap[name] = restored;
	}
	state.metaDataMap = snapshot.metaDataMap;
	state.lsqRequired = snapshot.lsqRequired;
	state.sigmaPass = snapshot.sigmaPass;
	state.chiQCPass = snapshot.chiQCPass;
	state.chi2 = snapshot.chi2;
	state.dof = snapshot.dof;
	state.chi2PerDof = snapshot.chi2PerDof;
	state.qc = snapshot.qc;
	state.id = snapshot.id;
	state.rts_basename = snapshot.rtsBasename;
	state.output_residuals = snapshot.outputResiduals;
	state.outputMongoMeasurements = snapshot.outputMongoMeasurements;
	state.statisticsMap = snapshot.statisticsMap;
	state.statisticsMapSum = snapshot.statisticsMapSum;

	if (failureReason)
	{
		*failureReason = "NONE";
	}
	return true;
}

bool restoreZhangCheckpointKfCore(
	const ZhangCheckpointKfCore& snapshot,
	KFState& state,
	std::string* failureReason)
{
	return restoreZhangCheckpointKfCoreWithReceiverResolver(
		snapshot, state, {}, failureReason);
}

std::string zhangCheckpointSha256(const std::string& bytes)
{
	std::array<unsigned char, EVP_MAX_MD_SIZE> digest{};
	unsigned int digestLength = 0;
	EVP_MD_CTX* context = EVP_MD_CTX_new();
	if (context == nullptr ||
		EVP_DigestInit_ex(context, EVP_sha256(), nullptr) != 1 ||
		EVP_DigestUpdate(context, bytes.data(), bytes.size()) != 1 ||
		EVP_DigestFinal_ex(context, digest.data(), &digestLength) != 1)
	{
		if (context)
		{
			EVP_MD_CTX_free(context);
		}
		return {};
	}
	EVP_MD_CTX_free(context);

	std::ostringstream output;
	output << std::hex << std::setfill('0');
	for (unsigned int index = 0; index < digestLength; index++)
	{
		output << std::setw(2) << static_cast<unsigned int>(digest[index]);
	}
	return output.str();
}

std::string zhangCheckpointFileSha256(
	const std::string& path,
	std::string* failureReason)
{
	std::ifstream input(path, std::ios::binary);
	if (!input)
	{
		if (failureReason)
		{
			*failureReason = "CHECKPOINT_HASH_FILE_OPEN_FAILED";
		}
		return {};
	}
	EVP_MD_CTX* context = EVP_MD_CTX_new();
	if (context == nullptr ||
		EVP_DigestInit_ex(context, EVP_sha256(), nullptr) != 1)
	{
		if (context)
		{
			EVP_MD_CTX_free(context);
		}
		if (failureReason)
		{
			*failureReason = "CHECKPOINT_HASH_INITIALISE_FAILED";
		}
		return {};
	}
	std::array<char, 1024 * 1024> buffer{};
	while (input)
	{
		input.read(buffer.data(), buffer.size());
		const auto count = input.gcount();
		if (count > 0 &&
			EVP_DigestUpdate(
				context, buffer.data(), static_cast<std::size_t>(count)) != 1)
		{
			EVP_MD_CTX_free(context);
			if (failureReason)
			{
				*failureReason = "CHECKPOINT_HASH_UPDATE_FAILED";
			}
			return {};
		}
	}
	if (!input.eof())
	{
		EVP_MD_CTX_free(context);
		if (failureReason)
		{
			*failureReason = "CHECKPOINT_HASH_FILE_READ_FAILED";
		}
		return {};
	}
	std::array<unsigned char, EVP_MAX_MD_SIZE> digest{};
	unsigned int digestLength = 0;
	if (EVP_DigestFinal_ex(context, digest.data(), &digestLength) != 1)
	{
		EVP_MD_CTX_free(context);
		if (failureReason)
		{
			*failureReason = "CHECKPOINT_HASH_FINALISE_FAILED";
		}
		return {};
	}
	EVP_MD_CTX_free(context);
	std::ostringstream output;
	output << std::hex << std::setfill('0');
	for (unsigned int index = 0; index < digestLength; index++)
	{
		output << std::setw(2) << static_cast<unsigned int>(digest[index]);
	}
	if (failureReason)
	{
		*failureReason = "NONE";
	}
	return output.str();
}

std::string serializeZhangCheckpointSectionPayload(
	const ZhangCheckpointKfCore& snapshot)
{
	return boostBinarySerialize(snapshot);
}

ZhangCheckpointIoResult writeZhangCheckpointBundle(
	const std::string& path,
	const ZhangCheckpointBundle& bundle)
{
	ZhangCheckpointIoResult result;
	std::string validationFailure;
	if (!validateManifest(bundle.manifest, validationFailure))
	{
		result.failureReason = validationFailure;
		return result;
	}
	if (!validateCore(bundle.kfCore, validationFailure))
	{
		result.failureReason = validationFailure;
		return result;
	}
	if (!validateCoreRuntimeIdentity(
		bundle.manifest, bundle.kfCore, validationFailure))
	{
		result.failureReason = validationFailure;
		return result;
	}
	for (const auto& [name, section] : bundle.sections)
	{
		if (name.empty() || section.schemaVersion == 0)
		{
			result.failureReason = "CHECKPOINT_SECTION_METADATA_INVALID";
			return result;
		}
		if (!sha256LooksValid(section.sha256)
		 || zhangCheckpointSha256(section.payload) != section.sha256)
		{
			result.failureReason =
				"CHECKPOINT_SECTION_SHA256_MISMATCH:" + name;
			return result;
		}
	}

	const std::string payload = boostBinarySerialize(bundle);
	const std::string checksum = zhangCheckpointSha256(payload);
	if (!sha256LooksValid(checksum))
	{
		result.failureReason = "CHECKPOINT_PAYLOAD_SHA256_FAILED";
		return result;
	}
	const std::filesystem::path target(path);
	if (std::filesystem::exists(target))
	{
		result.failureReason = "CHECKPOINT_TARGET_ALREADY_EXISTS";
		return result;
	}
	const auto nonce = std::chrono::steady_clock::now().time_since_epoch().count();
	const std::filesystem::path temporary =
		target.string() + ".tmp." + std::to_string(nonce);
	std::error_code filesystemError;
	if (!target.parent_path().empty())
	{
		std::filesystem::create_directories(target.parent_path(), filesystemError);
		if (filesystemError)
		{
			result.failureReason = "CHECKPOINT_CREATE_DIRECTORY_FAILED:" +
				filesystemError.message();
			return result;
		}
	}

	try
	{
		std::ofstream output(temporary, std::ios::binary | std::ios::trunc);
		if (!output)
		{
			result.failureReason = "CHECKPOINT_TEMP_OPEN_FAILED";
			return result;
		}
		const std::uint32_t version = ZHANG_CHECKPOINT_FORMAT_VERSION;
		const std::uint64_t payloadSize = payload.size();
		output.write(CHECKPOINT_MAGIC.data(), CHECKPOINT_MAGIC.size());
		output.write(reinterpret_cast<const char*>(&version), sizeof(version));
		output.write(reinterpret_cast<const char*>(&payloadSize), sizeof(payloadSize));
		output.write(checksum.data(), checksum.size());
		output.write(payload.data(), payload.size());
		output.flush();
		if (!output)
		{
			output.close();
			std::filesystem::remove(temporary);
			result.failureReason = "CHECKPOINT_TEMP_WRITE_FAILED";
			return result;
		}
		output.close();
		std::filesystem::rename(temporary, target);
	}
	catch (const std::exception& exception)
	{
		std::filesystem::remove(temporary);
		result.failureReason = "CHECKPOINT_ATOMIC_WRITE_FAILED:" +
			std::string(exception.what());
		return result;
	}

	result.valid = true;
	result.failureReason = "NONE";
	result.payloadSha256 = checksum;
	result.payloadBytes = payload.size();
	return result;
}

ZhangCheckpointIoResult verifyZhangCheckpointBundleEnvelope(
	const std::string& path,
	const std::string& expectedPayloadSha256)
{
	ZhangCheckpointIoResult result;
	std::ifstream input(path, std::ios::binary);
	if (!input)
	{
		result.failureReason = "CHECKPOINT_OPEN_FAILED";
		return result;
	}
	std::array<char, CHECKPOINT_MAGIC.size()> magic{};
	std::uint32_t version = 0;
	std::uint64_t payloadSize = 0;
	std::array<char, 64> storedChecksumBytes{};
	input.read(magic.data(), magic.size());
	input.read(reinterpret_cast<char*>(&version), sizeof(version));
	input.read(reinterpret_cast<char*>(&payloadSize), sizeof(payloadSize));
	input.read(storedChecksumBytes.data(), storedChecksumBytes.size());
	if (!input || magic != CHECKPOINT_MAGIC)
	{
		result.failureReason = "CHECKPOINT_HEADER_INVALID";
		return result;
	}
	if (version != ZHANG_CHECKPOINT_FORMAT_VERSION)
	{
		result.failureReason = "CHECKPOINT_FORMAT_VERSION_UNSUPPORTED";
		return result;
	}
	if (payloadSize == 0 || payloadSize > MAX_CHECKPOINT_PAYLOAD_BYTES)
	{
		result.failureReason = "CHECKPOINT_PAYLOAD_SIZE_INVALID";
		return result;
	}

	EVP_MD_CTX* context = EVP_MD_CTX_new();
	if (!context || EVP_DigestInit_ex(context, EVP_sha256(), nullptr) != 1)
	{
		if (context)
			EVP_MD_CTX_free(context);
		result.failureReason = "CHECKPOINT_HASH_INITIALISE_FAILED";
		return result;
	}
	std::array<char, 1024 * 1024> buffer{};
	std::uint64_t remaining = payloadSize;
	while (remaining > 0)
	{
		const auto requested = static_cast<std::streamsize>(
			std::min<std::uint64_t>(remaining, buffer.size()));
		input.read(buffer.data(), requested);
		if (input.gcount() != requested
		 || EVP_DigestUpdate(
				context, buffer.data(), static_cast<std::size_t>(requested)) != 1)
		{
			EVP_MD_CTX_free(context);
			result.failureReason = "CHECKPOINT_PAYLOAD_READ_FAILED";
			return result;
		}
		remaining -= static_cast<std::uint64_t>(requested);
	}
	if (input.peek() != std::char_traits<char>::eof())
	{
		EVP_MD_CTX_free(context);
		result.failureReason = "CHECKPOINT_FILE_SIZE_MISMATCH";
		return result;
	}
	std::array<unsigned char, EVP_MAX_MD_SIZE> digest{};
	unsigned int digestLength = 0;
	if (EVP_DigestFinal_ex(context, digest.data(), &digestLength) != 1)
	{
		EVP_MD_CTX_free(context);
		result.failureReason = "CHECKPOINT_HASH_FINALISE_FAILED";
		return result;
	}
	EVP_MD_CTX_free(context);
	std::ostringstream hash;
	hash << std::hex << std::setfill('0');
	for (unsigned int index = 0; index < digestLength; index++)
		hash << std::setw(2) << static_cast<unsigned int>(digest[index]);
	const std::string actualChecksum = hash.str();
	const std::string storedChecksum(
		storedChecksumBytes.begin(), storedChecksumBytes.end());
	if (actualChecksum != storedChecksum
	 || (!expectedPayloadSha256.empty()
		 && actualChecksum != expectedPayloadSha256))
	{
		result.failureReason = "CHECKPOINT_PAYLOAD_SHA256_MISMATCH";
		return result;
	}
	result.valid = true;
	result.failureReason = "NONE";
	result.payloadSha256 = actualChecksum;
	result.payloadBytes = payloadSize;
	return result;
}

ZhangCheckpointIoResult readZhangCheckpointBundle(
	const std::string& path,
	const ZhangCheckpointExpectations& expectations,
	ZhangCheckpointBundle& bundle)
{
	ZhangCheckpointIoResult result;
	std::ifstream input(path, std::ios::binary);
	if (!input)
	{
		result.failureReason = "CHECKPOINT_OPEN_FAILED";
		return result;
	}
	std::array<char, CHECKPOINT_MAGIC.size()> magic{};
	std::uint32_t version = 0;
	std::uint64_t payloadSize = 0;
	std::array<char, 64> expectedChecksum{};
	input.read(magic.data(), magic.size());
	input.read(reinterpret_cast<char*>(&version), sizeof(version));
	input.read(reinterpret_cast<char*>(&payloadSize), sizeof(payloadSize));
	input.read(expectedChecksum.data(), expectedChecksum.size());
	if (!input || magic != CHECKPOINT_MAGIC)
	{
		result.failureReason = "CHECKPOINT_HEADER_INVALID";
		return result;
	}
	if (version != ZHANG_CHECKPOINT_FORMAT_VERSION)
	{
		result.failureReason = "CHECKPOINT_FORMAT_VERSION_UNSUPPORTED";
		return result;
	}
	if (payloadSize == 0 || payloadSize > MAX_CHECKPOINT_PAYLOAD_BYTES)
	{
		result.failureReason = "CHECKPOINT_PAYLOAD_SIZE_INVALID";
		return result;
	}
	std::string payload(payloadSize, '\0');
	input.read(payload.data(), payload.size());
	if (!input || input.peek() != std::char_traits<char>::eof())
	{
		result.failureReason = "CHECKPOINT_FILE_SIZE_MISMATCH";
		return result;
	}
	const std::string actualChecksum = zhangCheckpointSha256(payload);
	const std::string storedChecksum(
		expectedChecksum.begin(), expectedChecksum.end());
	if (actualChecksum != storedChecksum)
	{
		result.failureReason = "CHECKPOINT_PAYLOAD_SHA256_MISMATCH";
		return result;
	}
	std::string deserializeFailure;
	ZhangCheckpointBundle candidate;
	if (!boostBinaryDeserialize(payload, candidate, deserializeFailure))
	{
		result.failureReason = deserializeFailure;
		return result;
	}
	std::string validationFailure;
	if (!validateManifest(candidate.manifest, validationFailure))
	{
		result.failureReason = validationFailure;
		return result;
	}
	if (!matchesExpectation(
			candidate.manifest.experimentMode, expectations.experimentMode) ||
		!matchesExpectation(
			candidate.manifest.binarySha256, expectations.binarySha256) ||
		!matchesExpectation(
			candidate.manifest.configSha256, expectations.configSha256) ||
		!matchesExpectation(
			candidate.manifest.inputManifestSha256,
			expectations.inputManifestSha256) ||
		!matchesExpectation(
			candidate.manifest.platformFingerprint,
			expectations.platformFingerprint) ||
		!matchesExpectation(
			candidate.manifest.compilerFingerprint,
			expectations.compilerFingerprint) ||
		!matchesExpectation(
			candidate.manifest.linearAlgebraFingerprint,
			expectations.linearAlgebraFingerprint) ||
		!matchesExpectation(
			candidate.manifest.endianness, expectations.endianness))
	{
		result.failureReason = "CHECKPOINT_PROVENANCE_MISMATCH";
		return result;
	}
	if (!validateCore(candidate.kfCore, validationFailure))
	{
		result.failureReason = validationFailure;
		return result;
	}
	if (!validateCoreRuntimeIdentity(
		candidate.manifest, candidate.kfCore, validationFailure))
	{
		result.failureReason = validationFailure;
		return result;
	}
	for (const auto& [name, section] : candidate.sections)
	{
		if (name.empty() || section.schemaVersion == 0 ||
			zhangCheckpointSha256(section.payload) != section.sha256)
		{
			result.failureReason = "CHECKPOINT_SECTION_SHA256_MISMATCH:" + name;
			return result;
		}
	}
	bundle = std::move(candidate);

	result.valid = true;
	result.failureReason = "NONE";
	result.payloadSha256 = actualChecksum;
	result.payloadBytes = payload.size();
	return result;
}

bool zhangCheckpointKfCoreBitwiseEqual(
	const ZhangCheckpointKfCore& left,
	const ZhangCheckpointKfCore& right)
{
	// Boost's binary long-double representation may include ABI padding bytes.
	// GTime persists only bigTime, so compare that mathematical value directly
	// and require an identical binary archive for every remaining field.
	return left.time.bigTimeBytes == right.time.bigTimeBytes &&
		boostBinarySerializeCoreWithoutTime(left) ==
		boostBinarySerializeCoreWithoutTime(right);
}

bool validateZhangCheckpointRequiredSections(
	const ZhangCheckpointBundle& bundle,
	const std::vector<ZhangCheckpointSectionRequirement>& requirements,
	std::string* failureReason)
{
	std::set<std::string> seen;
	for (const auto& requirement : requirements)
	{
		if (requirement.name.empty() || requirement.schemaVersion == 0 ||
			!seen.insert(requirement.name).second)
		{
			if (failureReason)
			{
				*failureReason = "CHECKPOINT_REQUIRED_SECTION_SPEC_INVALID";
			}
			return false;
		}
		auto found = bundle.sections.find(requirement.name);
		if (found == bundle.sections.end())
		{
			if (failureReason)
			{
				*failureReason =
					"CHECKPOINT_REQUIRED_SECTION_MISSING:" + requirement.name;
			}
			return false;
		}
		if (found->second.schemaVersion != requirement.schemaVersion)
		{
			if (failureReason)
			{
				*failureReason =
					"CHECKPOINT_REQUIRED_SECTION_VERSION_MISMATCH:" +
					requirement.name;
			}
			return false;
		}
		if (found->second.payload.empty() ||
			zhangCheckpointSha256(found->second.payload) !=
				found->second.sha256)
		{
			if (failureReason)
			{
				*failureReason =
					"CHECKPOINT_REQUIRED_SECTION_HASH_MISMATCH:" +
					requirement.name;
			}
			return false;
		}
	}
	if (failureReason)
	{
		*failureReason = "NONE";
	}
	return true;
}

ZhangCheckpointIoResult writeZhangCheckpointManifestJson(
	const std::string& path,
	const ZhangCheckpointBundle& bundle)
{
	ZhangCheckpointIoResult result;
	std::string failure;
	if (!validateManifest(bundle.manifest, failure) ||
		!validateCore(bundle.kfCore, failure) ||
		!validateCoreRuntimeIdentity(bundle.manifest, bundle.kfCore, failure))
	{
		result.failureReason = failure;
		return result;
	}
	auto escape = [](const std::string& value)
	{
		std::ostringstream escaped;
		for (unsigned char character : value)
		{
			switch (character)
			{
				case '\\': escaped << "\\\\"; break;
				case '"': escaped << "\\\""; break;
				case '\n': escaped << "\\n"; break;
				case '\r': escaped << "\\r"; break;
				case '\t': escaped << "\\t"; break;
				default:
					if (character < 0x20)
					{
						escaped << "\\u" << std::hex << std::setw(4)
							<< std::setfill('0')
							<< static_cast<unsigned int>(character)
							<< std::dec;
					}
					else
					{
						escaped << character;
					}
			}
		}
		return escaped.str();
	};
	std::ostringstream json;
	json << "{\n"
		 << "  \"format_version\": " << bundle.manifest.formatVersion << ",\n"
		 << "  \"core_schema_version\": "
		 << bundle.manifest.coreSchemaVersion << ",\n"
		 << "  \"experiment_mode\": \""
		 << escape(bundle.manifest.experimentMode) << "\",\n"
		 << "  \"runtime_id\": \"" << escape(bundle.manifest.runtimeId)
		 << "\",\n"
		 << "  \"checkpoint_id\": \""
		 << escape(bundle.manifest.checkpointId) << "\",\n"
		 << "  \"parent_checkpoint_id\": \""
		 << escape(bundle.manifest.parentCheckpointId) << "\",\n"
		 << "  \"epoch\": \"" << escape(bundle.manifest.epoch) << "\",\n"
		 << "  \"created_utc\": \"" << escape(bundle.manifest.createdUtc)
		 << "\",\n"
		 << "  \"binary_sha256\": \"" << bundle.manifest.binarySha256
		 << "\",\n"
		 << "  \"config_sha256\": \"" << bundle.manifest.configSha256
		 << "\",\n"
		 << "  \"input_manifest_sha256\": \""
		 << bundle.manifest.inputManifestSha256 << "\",\n"
		 << "  \"platform\": \""
		 << escape(bundle.manifest.platformFingerprint) << "\",\n"
		 << "  \"compiler\": \""
		 << escape(bundle.manifest.compilerFingerprint) << "\",\n"
		 << "  \"linear_algebra\": \""
		 << escape(bundle.manifest.linearAlgebraFingerprint) << "\",\n"
		 << "  \"endianness\": \"" << bundle.manifest.endianness << "\",\n"
		 << "  \"state_dimension\": " << bundle.kfCore.x.size() << ",\n"
		 << "  \"sections\": [\n";
	std::size_t sectionIndex = 0;
	for (const auto& [name, section] : bundle.sections)
	{
		const std::string checksum = zhangCheckpointSha256(section.payload);
		if (name.empty() || section.schemaVersion == 0 ||
			checksum != section.sha256)
		{
			result.failureReason =
				"CHECKPOINT_SECTION_SHA256_MISMATCH:" + name;
			return result;
		}
		json << "    {\"name\": \"" << escape(name)
			 << "\", \"schema_version\": " << section.schemaVersion
			 << ", \"payload_bytes\": " << section.payload.size()
			 << ", \"sha256\": \"" << checksum << "\"}";
		if (++sectionIndex != bundle.sections.size())
		{
			json << ',';
		}
		json << '\n';
	}
	json << "  ]\n}\n";

	const std::string bytes = json.str();
	const std::filesystem::path target(path);
	if (std::filesystem::exists(target))
	{
		result.failureReason = "CHECKPOINT_TARGET_ALREADY_EXISTS";
		return result;
	}
	const auto nonce = std::chrono::steady_clock::now().time_since_epoch().count();
	const std::filesystem::path temporary =
		target.string() + ".tmp." + std::to_string(nonce);
	std::error_code filesystemError;
	if (!target.parent_path().empty())
	{
		std::filesystem::create_directories(target.parent_path(), filesystemError);
		if (filesystemError)
		{
			result.failureReason = "CHECKPOINT_CREATE_DIRECTORY_FAILED:" +
				filesystemError.message();
			return result;
		}
	}
	try
	{
		std::ofstream output(temporary, std::ios::binary | std::ios::trunc);
		output.write(bytes.data(), bytes.size());
		output.flush();
		if (!output)
		{
			output.close();
			std::filesystem::remove(temporary);
			result.failureReason = "CHECKPOINT_MANIFEST_WRITE_FAILED";
			return result;
		}
		output.close();
		std::filesystem::rename(temporary, target);
	}
	catch (const std::exception& exception)
	{
		std::filesystem::remove(temporary);
		result.failureReason = "CHECKPOINT_MANIFEST_ATOMIC_WRITE_FAILED:" +
			std::string(exception.what());
		return result;
	}
	result.valid = true;
	result.failureReason = "NONE";
	result.payloadSha256 = zhangCheckpointSha256(bytes);
	result.payloadBytes = bytes.size();
	return result;
}
