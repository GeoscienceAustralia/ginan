#include "pea/zhangCheckpointRuntime.hpp"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <ctime>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <limits>
#include <mutex>
#include <sstream>
#include <system_error>
#include <utility>

#include <boost/version.hpp>

#include "ambres/GNSSambres.hpp"
#include "common/navigation.hpp"
#include "common/receiver.hpp"
#include "pea/zhangPppAr.hpp"

namespace
{
std::recursive_mutex e29CheckpointRuntimeMutex;

struct PreparedProvenance
{
	std::string binarySha256;
	std::string configSha256;
	std::string inputManifestSha256;
	std::string platformFingerprint;
	std::string compilerFingerprint;
	std::string linearAlgebraFingerprint;
	std::string endianness;
};

ZhangE29CheckpointResult checkpointFailure(const std::string& reason)
{
	ZhangE29CheckpointResult result;
	result.failureReason = reason;
	return result;
}

std::string compilePlatformFingerprint()
{
	std::ostringstream output;
#if defined(_WIN32)
	output << "windows";
#elif defined(__linux__)
	output << "linux";
#elif defined(__APPLE__)
	output << "apple";
#else
	output << "unknown-os";
#endif
	output << ";pointer_bytes=" << sizeof(void*)
		<< ";size_t_bytes=" << sizeof(std::size_t)
		<< ";long_double_bytes=" << sizeof(long double)
		<< ";boost_version=" << BOOST_VERSION;
#if defined(_GLIBCXX_USE_CXX11_ABI)
	output << ";libstdcxx_cxx11_abi=" << _GLIBCXX_USE_CXX11_ABI;
#endif
	return output.str();
}

std::string compileCompilerFingerprint()
{
	std::ostringstream output;
#if defined(__clang__)
	output << "clang=" << __clang_version__;
#elif defined(__GNUC__)
	output << "gcc=" << __VERSION__;
#elif defined(_MSC_VER)
	output << "msvc=" << _MSC_VER;
#else
	output << "unknown-compiler";
#endif
	output << ";cplusplus=" << __cplusplus;
	return output.str();
}

std::string compileLinearAlgebraFingerprint()
{
	std::ostringstream output;
	output << "eigen=" << EIGEN_WORLD_VERSION << '.'
		<< EIGEN_MAJOR_VERSION << '.' << EIGEN_MINOR_VERSION
		<< ";index_bytes=" << sizeof(Eigen::Index);
#if defined(EIGEN_MAX_ALIGN_BYTES)
	output << ";max_align_bytes=" << EIGEN_MAX_ALIGN_BYTES;
#endif
#if defined(EIGEN_USE_MKL_ALL)
	output << ";backend=mkl";
#else
	output << ";backend=eigen-default";
#endif
	return output.str();
}

std::string nativeEndianness()
{
	const std::uint16_t value = 0x0102;
	const auto* bytes = reinterpret_cast<const unsigned char*>(&value);
	return bytes[0] == 0x02 ? "LITTLE" : "BIG";
}

bool prepareProvenance(
	const ZhangE29CheckpointProvenance& provenance,
	PreparedProvenance& prepared,
	std::string& failureReason)
{
	if (provenance.experimentMode != ZHANG_E29_CHECKPOINT_EXPERIMENT_MODE)
	{
		failureReason = "E29_CHECKPOINT_EXPERIMENT_MODE_UNSUPPORTED";
		return false;
	}
	if (provenance.binaryPath.empty() || provenance.configText.empty()
		|| provenance.inputManifestText.empty())
	{
		failureReason = "E29_CHECKPOINT_PROVENANCE_INPUT_MISSING";
		return false;
	}
	std::string hashFailure;
	prepared.binarySha256 = zhangCheckpointFileSha256(
		provenance.binaryPath, &hashFailure);
	if (prepared.binarySha256.empty())
	{
		failureReason = "E29_CHECKPOINT_BINARY_HASH_FAILED:" + hashFailure;
		return false;
	}
	prepared.configSha256 = zhangCheckpointSha256(provenance.configText);
	prepared.inputManifestSha256 =
		zhangCheckpointSha256(provenance.inputManifestText);
	if (prepared.configSha256.empty()
		|| prepared.inputManifestSha256.empty())
	{
		failureReason = "E29_CHECKPOINT_CONTENT_HASH_FAILED";
		return false;
	}
	prepared.platformFingerprint = compilePlatformFingerprint();
	prepared.compilerFingerprint = compileCompilerFingerprint();
	prepared.linearAlgebraFingerprint =
		compileLinearAlgebraFingerprint();
	prepared.endianness = nativeEndianness();
	return true;
}

ZhangCheckpointExpectations checkpointExpectations(
	const ZhangE29CheckpointProvenance& provenance,
	const PreparedProvenance& prepared)
{
	ZhangCheckpointExpectations expectations;
	expectations.experimentMode = provenance.experimentMode;
	expectations.binarySha256 = prepared.binarySha256;
	expectations.configSha256 = prepared.configSha256;
	expectations.inputManifestSha256 = prepared.inputManifestSha256;
	expectations.platformFingerprint = prepared.platformFingerprint;
	expectations.compilerFingerprint = prepared.compilerFingerprint;
	expectations.linearAlgebraFingerprint =
		prepared.linearAlgebraFingerprint;
	expectations.endianness = prepared.endianness;
	return expectations;
}

bool manifestMatchesProvenance(
	const ZhangCheckpointManifest& manifest,
	const ZhangE29CheckpointProvenance& provenance,
	const PreparedProvenance& prepared,
	std::string& failureReason)
{
	if (manifest.formatVersion != ZHANG_CHECKPOINT_FORMAT_VERSION
		|| manifest.coreSchemaVersion != ZHANG_CHECKPOINT_CORE_SCHEMA_VERSION)
	{
		failureReason = "E29_CHECKPOINT_SCHEMA_VERSION_UNSUPPORTED";
		return false;
	}
	if (manifest.experimentMode != provenance.experimentMode
		|| manifest.binarySha256 != prepared.binarySha256
		|| manifest.configSha256 != prepared.configSha256
		|| manifest.inputManifestSha256 != prepared.inputManifestSha256
		|| manifest.configText != provenance.configText
		|| manifest.inputManifestText != provenance.inputManifestText
		|| manifest.platformFingerprint != prepared.platformFingerprint
		|| manifest.compilerFingerprint != prepared.compilerFingerprint
		|| manifest.linearAlgebraFingerprint !=
			prepared.linearAlgebraFingerprint
		|| manifest.endianness != prepared.endianness)
	{
		failureReason = "E29_CHECKPOINT_PROVENANCE_MISMATCH";
		return false;
	}
	if (manifest.runtimeId.empty() || manifest.checkpointId.empty()
		|| manifest.epoch.empty() || manifest.createdUtc.empty())
	{
		failureReason = "E29_CHECKPOINT_MANIFEST_IDENTITY_MISSING";
		return false;
	}
	if (!manifest.parentCheckpointId.empty()
		&& manifest.parentCheckpointId == manifest.checkpointId)
	{
		failureReason = "E29_CHECKPOINT_PARENT_ID_SELF_REFERENCE";
		return false;
	}
	return true;
}

std::string currentUtcIso8601()
{
	const std::time_t now = std::chrono::system_clock::to_time_t(
		std::chrono::system_clock::now());
	std::tm utc{};
#if defined(_WIN32)
	gmtime_s(&utc, &now);
#else
	gmtime_r(&now, &utc);
#endif
	std::ostringstream output;
	output << std::put_time(&utc, "%Y-%m-%dT%H:%M:%SZ");
	return output.str();
}

void appendFingerprintField(
	std::ostringstream& output,
	const std::string& value)
{
	output << value.size() << ':' << value << ';';
}

std::string bundleIdentitySha256(const ZhangCheckpointBundle& bundle)
{
	std::ostringstream identity;
	appendFingerprintField(
		identity, std::to_string(bundle.manifest.formatVersion));
	appendFingerprintField(
		identity, std::to_string(bundle.manifest.coreSchemaVersion));
	appendFingerprintField(identity, bundle.manifest.experimentMode);
	appendFingerprintField(identity, bundle.manifest.runtimeId);
	appendFingerprintField(identity, bundle.manifest.checkpointId);
	appendFingerprintField(identity, bundle.manifest.parentCheckpointId);
	appendFingerprintField(identity, bundle.manifest.epoch);
	appendFingerprintField(identity, bundle.manifest.createdUtc);
	appendFingerprintField(identity, bundle.manifest.binarySha256);
	appendFingerprintField(identity, bundle.manifest.configSha256);
	appendFingerprintField(identity, bundle.manifest.inputManifestSha256);
	appendFingerprintField(identity, bundle.manifest.platformFingerprint);
	appendFingerprintField(identity, bundle.manifest.compilerFingerprint);
	appendFingerprintField(
		identity, bundle.manifest.linearAlgebraFingerprint);
	appendFingerprintField(identity, bundle.manifest.endianness);
	appendFingerprintField(
		identity,
		zhangCheckpointSha256(
			serializeZhangCheckpointSectionPayload(bundle.kfCore)));
	for (const auto& [name, section] : bundle.sections)
	{
		appendFingerprintField(identity, name);
		appendFingerprintField(identity, std::to_string(section.schemaVersion));
		appendFingerprintField(identity, section.sha256);
	}
	return zhangCheckpointSha256(identity.str());
}

bool validateExactSections(
	const ZhangCheckpointBundle& bundle,
	std::string& failureReason);

std::string jsonEscape(const std::string& value)
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
}

bool makeE29ManifestJson(
	const ZhangCheckpointBundle& bundle,
	std::string& bytes,
	std::string& failureReason)
{
	bytes.clear();
	if (!validateExactSections(bundle, failureReason))
	{
		return false;
	}
	std::ostringstream json;
	json << "{\n"
		<< "  \"format_version\": " << bundle.manifest.formatVersion << ",\n"
		<< "  \"core_schema_version\": "
		<< bundle.manifest.coreSchemaVersion << ",\n"
		<< "  \"experiment_mode\": \""
		<< jsonEscape(bundle.manifest.experimentMode) << "\",\n"
		<< "  \"runtime_id\": \""
		<< jsonEscape(bundle.manifest.runtimeId) << "\",\n"
		<< "  \"checkpoint_id\": \""
		<< jsonEscape(bundle.manifest.checkpointId) << "\",\n"
		<< "  \"parent_checkpoint_id\": \""
		<< jsonEscape(bundle.manifest.parentCheckpointId) << "\",\n"
		<< "  \"epoch\": \"" << jsonEscape(bundle.manifest.epoch)
		<< "\",\n"
		<< "  \"created_utc\": \""
		<< jsonEscape(bundle.manifest.createdUtc) << "\",\n"
		<< "  \"binary_sha256\": \""
		<< bundle.manifest.binarySha256 << "\",\n"
		<< "  \"config_sha256\": \""
		<< bundle.manifest.configSha256 << "\",\n"
		<< "  \"input_manifest_sha256\": \""
		<< bundle.manifest.inputManifestSha256 << "\",\n"
		<< "  \"config_text\": \""
		<< jsonEscape(bundle.manifest.configText) << "\",\n"
		<< "  \"input_manifest_text\": \""
		<< jsonEscape(bundle.manifest.inputManifestText) << "\",\n"
		<< "  \"platform\": \""
		<< jsonEscape(bundle.manifest.platformFingerprint) << "\",\n"
		<< "  \"compiler\": \""
		<< jsonEscape(bundle.manifest.compilerFingerprint) << "\",\n"
		<< "  \"linear_algebra\": \""
		<< jsonEscape(bundle.manifest.linearAlgebraFingerprint) << "\",\n"
		<< "  \"endianness\": \"" << bundle.manifest.endianness
		<< "\",\n"
		<< "  \"state_dimension\": " << bundle.kfCore.x.size() << ",\n"
		<< "  \"kf_core_sha256\": \""
		<< zhangCheckpointSha256(
			serializeZhangCheckpointSectionPayload(bundle.kfCore))
		<< "\",\n"
		<< "  \"bundle_identity_sha256\": \""
		<< bundleIdentitySha256(bundle) << "\",\n"
		<< "  \"sections\": [\n";
	std::size_t index = 0;
	for (const auto& [name, section] : bundle.sections)
	{
		json << "    {\"name\": \"" << jsonEscape(name)
			<< "\", \"schema_version\": " << section.schemaVersion
			<< ", \"payload_bytes\": " << section.payload.size()
			<< ", \"sha256\": \"" << section.sha256 << "\"}";
		if (++index != bundle.sections.size())
		{
			json << ',';
		}
		json << '\n';
	}
	json << "  ]\n}\n";

	bytes = json.str();
	return true;
}

ZhangCheckpointIoResult writeE29ManifestJson(
	const std::filesystem::path& path,
	const ZhangCheckpointBundle& bundle)
{
	ZhangCheckpointIoResult result;
	std::string bytes;
	if (!makeE29ManifestJson(bundle, bytes, result.failureReason))
	{
		return result;
	}
	std::ofstream output(path, std::ios::binary | std::ios::trunc);
	if (!output)
	{
		result.failureReason = "E29_CHECKPOINT_MANIFEST_OPEN_FAILED";
		return result;
	}
	output.write(bytes.data(), bytes.size());
	output.flush();
	if (!output)
	{
		result.failureReason = "E29_CHECKPOINT_MANIFEST_WRITE_FAILED";
		return result;
	}
	output.close();
	result.valid = true;
	result.failureReason = "NONE";
	result.payloadBytes = bytes.size();
	result.payloadSha256 = zhangCheckpointSha256(bytes);
	return result;
}

bool addSection(
	ZhangCheckpointBundle& bundle,
	const std::string& name,
	std::uint32_t schemaVersion,
	std::string payload,
	std::string& failureReason)
{
	if (name.empty() || schemaVersion == 0 || payload.empty())
	{
		failureReason = "E29_CHECKPOINT_SECTION_METADATA_INVALID:" + name;
		return false;
	}
	ZhangCheckpointSection section;
	section.schemaVersion = schemaVersion;
	section.payload = std::move(payload);
	section.sha256 = zhangCheckpointSha256(section.payload);
	if (section.sha256.empty())
	{
		failureReason = "E29_CHECKPOINT_SECTION_HASH_FAILED:" + name;
		return false;
	}
	if (!bundle.sections.emplace(name, std::move(section)).second)
	{
		failureReason = "E29_CHECKPOINT_SECTION_DUPLICATE:" + name;
		return false;
	}
	return true;
}

bool validateExactSections(
	const ZhangCheckpointBundle& bundle,
	std::string& failureReason)
{
	const auto& required = zhangE29CheckpointRequiredSections();
	if (!validateZhangCheckpointRequiredSections(
		bundle, required, &failureReason))
	{
		return false;
	}
	if (bundle.sections.size() != required.size())
	{
		for (const auto& [name, ignored] : bundle.sections)
		{
			const bool known = std::any_of(
				required.begin(), required.end(), [&](const auto& requirement)
				{
					return requirement.name == name;
				});
			if (!known)
			{
				failureReason =
					"E29_CHECKPOINT_UNEXPECTED_SECTION:" + name;
				return false;
			}
		}
		failureReason = "E29_CHECKPOINT_SECTION_CARDINALITY_MISMATCH";
		return false;
	}
	const auto rng = bundle.sections.find(
		ZHANG_E29_RNG_CHECKPOINT_SECTION_NAME);
	if (rng == bundle.sections.end()
		|| rng->second.payload != ZHANG_E29_RNG_CHECKPOINT_PAYLOAD)
	{
		failureReason = "E29_CHECKPOINT_RNG_DECLARATION_MISMATCH";
		return false;
	}
	return true;
}

const ZhangCheckpointSection* requiredSection(
	const ZhangCheckpointBundle& bundle,
	const std::string& name,
	std::string& failureReason)
{
	auto found = bundle.sections.find(name);
	if (found == bundle.sections.end())
	{
		failureReason = "E29_CHECKPOINT_REQUIRED_SECTION_MISSING:" + name;
		return nullptr;
	}
	return &found->second;
}

bool validateCrossSectionSnapshotReferences(
	const std::string& runtimeId,
	const std::string& pppArPayload,
	const std::string& ambresPayload,
	ZhangCheckpointSnapshotReferenceSummary& pppArSummary,
	ZhangCheckpointSnapshotReferenceSummary& ambresSummary,
	std::string& failureReason)
{
	auto pppArResult = inspectZhangPppArCheckpointSnapshotReferences(
		runtimeId, pppArPayload, pppArSummary);
	if (!pppArResult.valid || !pppArSummary.valid)
	{
		failureReason = "E29_CHECKPOINT_PPP_AR_SNAPSHOT_INVENTORY_FAILED:" +
			(pppArResult.failureReason.empty()
				? pppArSummary.failureReason : pppArResult.failureReason);
		return false;
	}
	auto ambresResult = inspectZhangAmbresCheckpointSnapshotReferences(
		runtimeId, ambresPayload, ambresSummary);
	if (!ambresResult.valid || !ambresSummary.valid)
	{
		failureReason = "E29_CHECKPOINT_AMBRES_SNAPSHOT_INVENTORY_FAILED:" +
			(ambresResult.failureReason.empty()
				? ambresSummary.failureReason : ambresResult.failureReason);
		return false;
	}
	const auto validation = validateZhangCheckpointSnapshotReferences(
		pppArSummary, ambresSummary);
	if (!validation.valid)
	{
		failureReason =
			"E29_CHECKPOINT_CROSS_MODULE_SNAPSHOT_REFERENCE_FAILED:" +
			validation.failureReason;
		return false;
	}
	if (validation.runtimeId != runtimeId)
	{
		failureReason =
			"E29_CHECKPOINT_CROSS_MODULE_SNAPSHOT_RUNTIME_ID_MISMATCH";
		return false;
	}
	return true;
}

ZhangCheckpointReceiverResolver receiverResolver(
	const ReceiverMap& receivers)
{
	return [&receivers](const std::string& receiverId) -> Receiver*
	{
		auto receiver = receivers.find(receiverId);
		if (receiver == receivers.end()
			|| receiver->second.id != receiverId)
		{
			return nullptr;
		}
		return const_cast<Receiver*>(&receiver->second);
	};
}

bool sameControllerEpochAsFilter(
	const KFState& state,
	const ZhangPeaControllerCheckpointState& controller)
{
	return state.time == restoreZhangCheckpointTime(controller.completedTsync);
}

} // namespace

const std::vector<ZhangCheckpointSectionRequirement>&
zhangE29CheckpointRequiredSections()
{
	static const std::vector<ZhangCheckpointSectionRequirement> required = {
		{ZHANG_PEA_CONTROLLER_CHECKPOINT_SECTION_NAME,
			ZHANG_PEA_CONTROLLER_CHECKPOINT_SCHEMA_VERSION},
		{ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SECTION_NAME,
			ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SCHEMA_VERSION},
		{ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SECTION_NAME,
			ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SCHEMA_VERSION},
		{ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SECTION_NAME,
			ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SCHEMA_VERSION},
		{ZHANG_GRAPH_CHECKPOINT_SECTION_NAME,
			ZHANG_GRAPH_CHECKPOINT_SCHEMA_VERSION},
		{ZHANG_PPP_AR_CHECKPOINT_SECTION_NAME,
			ZHANG_PPP_AR_CHECKPOINT_SCHEMA_VERSION},
		{ZHANG_AMBRES_CHECKPOINT_SECTION_NAME,
			ZHANG_AMBRES_CHECKPOINT_SCHEMA_VERSION},
		{ZHANG_E29_RNG_CHECKPOINT_SECTION_NAME,
			ZHANG_E29_RNG_CHECKPOINT_SCHEMA_VERSION},
	};
	return required;
}

ZhangE29CheckpointResult captureZhangE29CheckpointBundle(
	const KFState& authoritativeState,
	const ReceiverMap& receivers,
	const Navigation& navigation,
	const std::multimap<std::string, StreamParserPtr>& streams,
	const std::map<std::string, bool>& streamDoneMap,
	const ZhangPeaControllerCheckpointState& controller,
	const ZhangE29CheckpointIdentity& identity,
	const ZhangE29CheckpointProvenance& provenance,
	ZhangCheckpointBundle& bundle)
{
	std::lock_guard<std::recursive_mutex> lock(e29CheckpointRuntimeMutex);
	if (identity.runtimeId.empty() || identity.checkpointId.empty())
	{
		return checkpointFailure("E29_CHECKPOINT_CAPTURE_IDENTITY_MISSING");
	}
	if (!identity.parentCheckpointId.empty()
		&& identity.parentCheckpointId == identity.checkpointId)
	{
		return checkpointFailure("E29_CHECKPOINT_PARENT_ID_SELF_REFERENCE");
	}
	const std::string boundRuntimeId =
		zhangCheckpointRuntimeId(authoritativeState);
	if (boundRuntimeId.empty())
	{
		return checkpointFailure(
			"E29_CHECKPOINT_AUTHORITATIVE_RUNTIME_ID_NOT_BOUND");
	}
	if (boundRuntimeId != identity.runtimeId)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_AUTHORITATIVE_RUNTIME_ID_MISMATCH");
	}
	if (authoritativeState.alternate_ptr != nullptr)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_ALTERNATE_FILTER_POINTER_UNSUPPORTED");
	}
	if (auto branch = authoritativeState.metaDataMap.find(
		ZHANG_CHECKPOINT_RUNTIME_BRANCH_ID_METADATA);
		branch != authoritativeState.metaDataMap.end()
		&& !branch->second.empty())
	{
		return checkpointFailure(
			"E29_CHECKPOINT_AUTHORITATIVE_STATE_IS_BRANCH");
	}
	if (controller.boundary != ZHANG_PEA_POST_EPOCH_BOUNDARY
		|| controller.resumePolicy != ZHANG_PEA_RESUME_NEXT_EPOCH)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CAPTURE_BOUNDARY_NOT_POST_EPOCH_COMMITTED");
	}
	if (!sameControllerEpochAsFilter(authoritativeState, controller))
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CONTROLLER_FILTER_EPOCH_MISMATCH");
	}

	PreparedProvenance prepared;
	std::string failureReason;
	if (!prepareProvenance(provenance, prepared, failureReason))
	{
		return checkpointFailure(failureReason);
	}

	ZhangCheckpointBundle candidate;
	candidate.manifest.experimentMode = provenance.experimentMode;
	candidate.manifest.runtimeId = identity.runtimeId;
	candidate.manifest.checkpointId = identity.checkpointId;
	candidate.manifest.parentCheckpointId = identity.parentCheckpointId;
	candidate.manifest.epoch = authoritativeState.time.to_string(0);
	candidate.manifest.binarySha256 = prepared.binarySha256;
	candidate.manifest.configSha256 = prepared.configSha256;
	candidate.manifest.inputManifestSha256 = prepared.inputManifestSha256;
	candidate.manifest.configText = provenance.configText;
	candidate.manifest.inputManifestText = provenance.inputManifestText;
	candidate.manifest.platformFingerprint = prepared.platformFingerprint;
	candidate.manifest.compilerFingerprint = prepared.compilerFingerprint;
	candidate.manifest.linearAlgebraFingerprint =
		prepared.linearAlgebraFingerprint;
	candidate.manifest.endianness = prepared.endianness;
	candidate.manifest.createdUtc = identity.createdUtc.empty()
		? currentUtcIso8601() : identity.createdUtc;
	candidate.kfCore = captureZhangCheckpointKfCore(authoritativeState);
	if (candidate.kfCore.x.size() > std::numeric_limits<int>::max())
	{
		return checkpointFailure("E29_CHECKPOINT_STATE_DIMENSION_OVERFLOW");
	}

	std::string payload;
	auto controllerResult = exportZhangPeaControllerCheckpointSection(
		controller, payload);
	if (!controllerResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CONTROLLER_EXPORT_FAILED:" +
			controllerResult.failureReason);
	}
	if (!addSection(
		candidate, ZHANG_PEA_CONTROLLER_CHECKPOINT_SECTION_NAME,
		ZHANG_PEA_CONTROLLER_CHECKPOINT_SCHEMA_VERSION,
		std::move(payload), failureReason))
	{
		return checkpointFailure(failureReason);
	}

	payload.clear();
	auto streamResult = exportZhangRinexFileStreamsCheckpointSection(
		streams, streamDoneMap, payload);
	if (!streamResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_STREAM_EXPORT_FAILED:" +
			streamResult.failureReason);
	}
	if (!addSection(
		candidate, ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SECTION_NAME,
		ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SCHEMA_VERSION,
		std::move(payload), failureReason))
	{
		return checkpointFailure(failureReason);
	}

	payload.clear();
	auto receiverResult = exportZhangReceiverRuntimeCheckpointSection(
		receivers, identity.runtimeId, payload);
	if (!receiverResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_RECEIVER_EXPORT_FAILED:" +
			receiverResult.failureReason);
	}
	if (!addSection(
		candidate, ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SECTION_NAME,
		ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SCHEMA_VERSION,
		std::move(payload), failureReason))
	{
		return checkpointFailure(failureReason);
	}

	payload.clear();
	auto satelliteResult = exportZhangSatelliteRuntimeCheckpointSection(
		navigation, identity.runtimeId, payload);
	if (!satelliteResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_SATELLITE_EXPORT_FAILED:" +
			satelliteResult.failureReason);
	}
	if (!addSection(
		candidate, ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SECTION_NAME,
		ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SCHEMA_VERSION,
		std::move(payload), failureReason))
	{
		return checkpointFailure(failureReason);
	}

	payload.clear();
	if (!exportZhangGraphCheckpointSection(
		authoritativeState, identity.runtimeId, payload, failureReason))
	{
		return checkpointFailure(
			"E29_CHECKPOINT_GRAPH_EXPORT_FAILED:" + failureReason);
	}
	if (!addSection(
		candidate, ZHANG_GRAPH_CHECKPOINT_SECTION_NAME,
		ZHANG_GRAPH_CHECKPOINT_SCHEMA_VERSION,
		std::move(payload), failureReason))
	{
		return checkpointFailure(failureReason);
	}

	payload.clear();
	auto pppArResult = exportZhangPppArCheckpointSection(
		authoritativeState, identity.runtimeId, payload);
	if (!pppArResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_PPP_AR_EXPORT_FAILED:" +
			pppArResult.failureReason);
	}
	if (!addSection(
		candidate, ZHANG_PPP_AR_CHECKPOINT_SECTION_NAME,
		ZHANG_PPP_AR_CHECKPOINT_SCHEMA_VERSION,
		std::move(payload), failureReason))
	{
		return checkpointFailure(failureReason);
	}

	payload.clear();
	auto ambresResult = exportZhangAmbresCheckpointSection(
		authoritativeState, identity.runtimeId, payload);
	if (!ambresResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_AMBRES_EXPORT_FAILED:" +
			ambresResult.failureReason);
	}
	if (!addSection(
		candidate, ZHANG_AMBRES_CHECKPOINT_SECTION_NAME,
		ZHANG_AMBRES_CHECKPOINT_SCHEMA_VERSION,
		std::move(payload), failureReason))
	{
		return checkpointFailure(failureReason);
	}

	ZhangCheckpointSnapshotReferenceSummary pppArSummary;
	ZhangCheckpointSnapshotReferenceSummary ambresSummary;
	if (!validateCrossSectionSnapshotReferences(
		identity.runtimeId,
		candidate.sections.at(
			ZHANG_PPP_AR_CHECKPOINT_SECTION_NAME).payload,
		candidate.sections.at(
			ZHANG_AMBRES_CHECKPOINT_SECTION_NAME).payload,
		pppArSummary, ambresSummary, failureReason))
	{
		return checkpointFailure(failureReason);
	}
	if (pppArSummary.transitionCount != pppArResult.pendingTransitions
		|| pppArSummary.pinnedSnapshotIdentities.size() !=
			pppArResult.pendingSnapshotPins
		|| ambresSummary.transitionCount !=
			ambresResult.activeTemporalTransitions)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CROSS_MODULE_SUMMARY_COUNT_MISMATCH");
	}
	if (!addSection(
		candidate, ZHANG_E29_RNG_CHECKPOINT_SECTION_NAME,
		ZHANG_E29_RNG_CHECKPOINT_SCHEMA_VERSION,
		ZHANG_E29_RNG_CHECKPOINT_PAYLOAD, failureReason))
	{
		return checkpointFailure(failureReason);
	}
	if (!validateExactSections(candidate, failureReason))
	{
		return checkpointFailure(failureReason);
	}

	KFState candidateState = authoritativeState;
	if (!restoreZhangCheckpointKfCoreWithReceiverResolver(
		candidate.kfCore, candidateState, receiverResolver(receivers),
		&failureReason))
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CAPTURE_CORE_PREFLIGHT_FAILED:" + failureReason);
	}
	if (!zhangCheckpointKfCoreBitwiseEqual(
		candidate.kfCore, captureZhangCheckpointKfCore(candidateState)))
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CAPTURE_CORE_ROUNDTRIP_MISMATCH");
	}

	bundle = std::move(candidate);
	ZhangE29CheckpointResult result;
	result.valid = true;
	result.failureReason = "NONE";
	result.checkpointId = bundle.manifest.checkpointId;
	result.runtimeId = bundle.manifest.runtimeId;
	result.sectionCount = bundle.sections.size();
	result.stateDimension = static_cast<int>(bundle.kfCore.x.size());
	return result;
}

ZhangE29CheckpointResult writeZhangE29CheckpointDirectory(
	const std::string& targetDirectory,
	const ZhangCheckpointBundle& bundle)
{
	std::lock_guard<std::recursive_mutex> lock(e29CheckpointRuntimeMutex);
	std::string failureReason;
	if (targetDirectory.empty())
	{
		return checkpointFailure("E29_CHECKPOINT_TARGET_DIRECTORY_MISSING");
	}
	if (!validateExactSections(bundle, failureReason))
	{
		return checkpointFailure(failureReason);
	}
	std::error_code error;
	const std::filesystem::path target =
		std::filesystem::absolute(targetDirectory, error).lexically_normal();
	if (error || target.filename().empty())
	{
		return checkpointFailure("E29_CHECKPOINT_TARGET_DIRECTORY_INVALID");
	}
	if (std::filesystem::exists(target, error))
	{
		return checkpointFailure("E29_CHECKPOINT_TARGET_ALREADY_EXISTS");
	}
	if (error)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_TARGET_STATUS_FAILED:" + error.message());
	}
	std::filesystem::create_directories(target.parent_path(), error);
	if (error)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_PARENT_CREATE_FAILED:" + error.message());
	}

	const auto nonce =
		std::chrono::steady_clock::now().time_since_epoch().count();
	const std::filesystem::path staging = target.parent_path() /
		(target.filename().string() + ".tmp." + std::to_string(nonce));
	if (std::filesystem::exists(staging, error) || error)
	{
		return checkpointFailure("E29_CHECKPOINT_STAGING_COLLISION");
	}
	std::filesystem::create_directory(staging, error);
	if (error)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_STAGING_CREATE_FAILED:" + error.message());
	}

	auto cleanupStaging = [&]()
	{
		std::error_code ignored;
		std::filesystem::remove_all(staging, ignored);
	};

	const std::filesystem::path bundlePath =
		staging / ZHANG_E29_CHECKPOINT_BUNDLE_FILENAME;
	const std::filesystem::path manifestPath =
		staging / ZHANG_E29_CHECKPOINT_MANIFEST_FILENAME;
	auto binaryResult = writeZhangCheckpointBundle(
		bundlePath.string(), bundle);
	if (!binaryResult.valid)
	{
		cleanupStaging();
		return checkpointFailure(
			"E29_CHECKPOINT_BUNDLE_WRITE_FAILED:" +
			binaryResult.failureReason);
	}
	auto manifestResult = writeE29ManifestJson(manifestPath, bundle);
	if (!manifestResult.valid)
	{
		cleanupStaging();
		return checkpointFailure(
			"E29_CHECKPOINT_MANIFEST_WRITE_FAILED:" +
			manifestResult.failureReason);
	}

	auto verifyResult = verifyZhangCheckpointBundleEnvelope(
		bundlePath.string(), binaryResult.payloadSha256);
	if (!verifyResult.valid
	 || verifyResult.payloadBytes != binaryResult.payloadBytes)
	{
		cleanupStaging();
		return checkpointFailure(
			verifyResult.valid
				? "E29_CHECKPOINT_STAGED_BUNDLE_SIZE_MISMATCH"
				: "E29_CHECKPOINT_STAGED_BUNDLE_VERIFY_FAILED:" +
					verifyResult.failureReason);
	}

	std::filesystem::rename(staging, target, error);
	if (error)
	{
		cleanupStaging();
		return checkpointFailure(
			"E29_CHECKPOINT_DIRECTORY_PUBLISH_FAILED:" + error.message());
	}

	ZhangE29CheckpointResult result;
	result.valid = true;
	result.failureReason = "NONE";
	result.checkpointId = bundle.manifest.checkpointId;
	result.runtimeId = bundle.manifest.runtimeId;
	result.payloadSha256 = binaryResult.payloadSha256;
	result.payloadBytes = binaryResult.payloadBytes;
	result.sectionCount = bundle.sections.size();
	result.stateDimension = static_cast<int>(bundle.kfCore.x.size());
	return result;
}

ZhangE29CheckpointResult captureAndWriteZhangE29Checkpoint(
	const std::string& targetDirectory,
	const KFState& authoritativeState,
	const ReceiverMap& receivers,
	const Navigation& navigation,
	const std::multimap<std::string, StreamParserPtr>& streams,
	const std::map<std::string, bool>& streamDoneMap,
	const ZhangPeaControllerCheckpointState& controller,
	const ZhangE29CheckpointIdentity& identity,
	const ZhangE29CheckpointProvenance& provenance,
	ZhangCheckpointBundle* capturedBundle)
{
	std::lock_guard<std::recursive_mutex> lock(e29CheckpointRuntimeMutex);
	ZhangCheckpointBundle candidate;
	auto captureResult = captureZhangE29CheckpointBundle(
		authoritativeState, receivers, navigation, streams, streamDoneMap,
		controller, identity, provenance, candidate);
	if (!captureResult.valid)
	{
		return captureResult;
	}
	auto writeResult = writeZhangE29CheckpointDirectory(
		targetDirectory, candidate);
	if (!writeResult.valid)
	{
		return writeResult;
	}
	if (capturedBundle)
	{
		*capturedBundle = std::move(candidate);
	}
	return writeResult;
}

ZhangE29CheckpointResult readZhangE29CheckpointDirectory(
	const std::string& checkpointDirectory,
	const ZhangE29CheckpointProvenance& provenance,
	const std::string& expectedRuntimeId,
	ZhangCheckpointBundle& bundle)
{
	std::lock_guard<std::recursive_mutex> lock(e29CheckpointRuntimeMutex);
	if (checkpointDirectory.empty() || expectedRuntimeId.empty())
	{
		return checkpointFailure("E29_CHECKPOINT_READ_IDENTITY_MISSING");
	}
	PreparedProvenance prepared;
	std::string failureReason;
	if (!prepareProvenance(provenance, prepared, failureReason))
	{
		return checkpointFailure(failureReason);
	}

	const std::filesystem::path directory(checkpointDirectory);
	const std::filesystem::path bundlePath =
		directory / ZHANG_E29_CHECKPOINT_BUNDLE_FILENAME;
	const std::filesystem::path manifestPath =
		directory / ZHANG_E29_CHECKPOINT_MANIFEST_FILENAME;
	std::error_code error;
	if (!std::filesystem::is_directory(directory, error) || error
		|| !std::filesystem::is_regular_file(bundlePath, error) || error
		|| !std::filesystem::is_regular_file(manifestPath, error) || error
		|| std::filesystem::file_size(manifestPath, error) == 0 || error)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_PUBLISHED_DIRECTORY_INCOMPLETE");
	}

	ZhangCheckpointBundle candidate;
	auto ioResult = readZhangCheckpointBundle(
		bundlePath.string(), checkpointExpectations(provenance, prepared),
		candidate);
	if (!ioResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_BUNDLE_READ_FAILED:" + ioResult.failureReason);
	}
	if (!manifestMatchesProvenance(
		candidate.manifest, provenance, prepared, failureReason))
	{
		return checkpointFailure(failureReason);
	}
	if (candidate.manifest.runtimeId != expectedRuntimeId)
	{
		return checkpointFailure("E29_CHECKPOINT_RUNTIME_ID_MISMATCH");
	}
	if (!validateExactSections(candidate, failureReason))
	{
		return checkpointFailure(failureReason);
	}
	std::ifstream manifestInput(manifestPath, std::ios::binary);
	const std::string actualManifest(
		(std::istreambuf_iterator<char>(manifestInput)),
		std::istreambuf_iterator<char>());
	std::string expectedManifest;
	if (!makeE29ManifestJson(
			candidate, expectedManifest, failureReason)
		|| actualManifest != expectedManifest)
	{
		return checkpointFailure(
			failureReason.empty()
				? "E29_CHECKPOINT_MANIFEST_CONTENT_MISMATCH"
				: failureReason);
	}

	bundle = std::move(candidate);
	ZhangE29CheckpointResult result;
	result.valid = true;
	result.failureReason = "NONE";
	result.checkpointId = bundle.manifest.checkpointId;
	result.runtimeId = bundle.manifest.runtimeId;
	result.payloadSha256 = ioResult.payloadSha256;
	result.payloadBytes = ioResult.payloadBytes;
	result.sectionCount = bundle.sections.size();
	result.stateDimension = static_cast<int>(bundle.kfCore.x.size());
	return result;
}

ZhangE29CheckpointResult preflightZhangE29CheckpointBundle(
	const ZhangCheckpointBundle& bundle,
	const ZhangE29CheckpointProvenance& provenance,
	KFState& configuredState,
	const ReceiverMap& configuredReceivers,
	const Navigation& configuredNavigation,
	const std::multimap<std::string, StreamParserPtr>& configuredStreams,
	const std::map<std::string, bool>& configuredStreamDoneMap,
	double expectedEpochIntervalSeconds,
	ZhangE29CheckpointRestorePlan& plan)
{
	std::lock_guard<std::recursive_mutex> lock(e29CheckpointRuntimeMutex);
	std::string failureReason;
	PreparedProvenance prepared;
	if (!prepareProvenance(provenance, prepared, failureReason))
	{
		return checkpointFailure(failureReason);
	}
	if (!manifestMatchesProvenance(
		bundle.manifest, provenance, prepared, failureReason))
	{
		return checkpointFailure(failureReason);
	}
	if (!validateExactSections(bundle, failureReason))
	{
		return checkpointFailure(failureReason);
	}
	if (!std::isfinite(expectedEpochIntervalSeconds)
		|| expectedEpochIntervalSeconds <= 0)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_EXPECTED_EPOCH_INTERVAL_INVALID");
	}
	if (zhangCheckpointRuntimeId(configuredState) !=
		bundle.manifest.runtimeId)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CONFIGURED_RUNTIME_ID_MISMATCH");
	}
	if (configuredState.alternate_ptr != nullptr)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_ALTERNATE_FILTER_POINTER_UNSUPPORTED");
	}
	if (auto branch = configuredState.metaDataMap.find(
		ZHANG_CHECKPOINT_RUNTIME_BRANCH_ID_METADATA);
		branch != configuredState.metaDataMap.end()
		&& !branch->second.empty())
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CONFIGURED_STATE_IS_BRANCH");
	}
	auto coreRuntime = bundle.kfCore.metaDataMap.find(
		ZHANG_CHECKPOINT_RUNTIME_ID_METADATA);
	if (coreRuntime == bundle.kfCore.metaDataMap.end()
		|| coreRuntime->second != bundle.manifest.runtimeId)
	{
		return checkpointFailure("E29_CHECKPOINT_CORE_RUNTIME_ID_MISMATCH");
	}
	if (auto branch = bundle.kfCore.metaDataMap.find(
		ZHANG_CHECKPOINT_RUNTIME_BRANCH_ID_METADATA);
		branch != bundle.kfCore.metaDataMap.end() && !branch->second.empty())
	{
		return checkpointFailure("E29_CHECKPOINT_CORE_IS_BRANCH");
	}

	ZhangE29CheckpointRestorePlan candidatePlan;
	candidatePlan.bundleIdentitySha256 = bundleIdentitySha256(bundle);
	if (candidatePlan.bundleIdentitySha256.empty())
	{
		return checkpointFailure("E29_CHECKPOINT_BUNDLE_IDENTITY_HASH_FAILED");
	}
	candidatePlan.checkpointId = bundle.manifest.checkpointId;
	candidatePlan.runtimeId = bundle.manifest.runtimeId;
	if (bundle.kfCore.x.size() > std::numeric_limits<int>::max())
	{
		return checkpointFailure("E29_CHECKPOINT_STATE_DIMENSION_OVERFLOW");
	}
	candidatePlan.stateDimension = static_cast<int>(bundle.kfCore.x.size());

	KFState candidateCore = configuredState;
	if (!restoreZhangCheckpointKfCoreWithReceiverResolver(
		bundle.kfCore, candidateCore,
		receiverResolver(configuredReceivers), &failureReason))
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CORE_PREFLIGHT_FAILED:" + failureReason);
	}
	if (zhangCheckpointRuntimeId(candidateCore) != bundle.manifest.runtimeId
		|| candidateCore.x.size() != candidatePlan.stateDimension
		|| candidateCore.time.to_string(0) != bundle.manifest.epoch
		|| !zhangCheckpointKfCoreBitwiseEqual(
			bundle.kfCore, captureZhangCheckpointKfCore(candidateCore)))
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CORE_PREFLIGHT_ROUNDTRIP_MISMATCH");
	}

	const auto* controllerSection = requiredSection(
		bundle, ZHANG_PEA_CONTROLLER_CHECKPOINT_SECTION_NAME,
		failureReason);
	const auto* streamSection = requiredSection(
		bundle, ZHANG_RINEX_FILE_STREAMS_CHECKPOINT_SECTION_NAME,
		failureReason);
	const auto* receiverSection = requiredSection(
		bundle, ZHANG_RECEIVER_RUNTIME_CHECKPOINT_SECTION_NAME,
		failureReason);
	const auto* satelliteSection = requiredSection(
		bundle, ZHANG_SATELLITE_RUNTIME_CHECKPOINT_SECTION_NAME,
		failureReason);
	const auto* graphSection = requiredSection(
		bundle, ZHANG_GRAPH_CHECKPOINT_SECTION_NAME, failureReason);
	const auto* pppArSection = requiredSection(
		bundle, ZHANG_PPP_AR_CHECKPOINT_SECTION_NAME, failureReason);
	const auto* ambresSection = requiredSection(
		bundle, ZHANG_AMBRES_CHECKPOINT_SECTION_NAME, failureReason);
	if (!controllerSection || !streamSection || !receiverSection
		|| !satelliteSection || !graphSection || !pppArSection
		|| !ambresSection)
	{
		return checkpointFailure(failureReason);
	}

	auto controllerResult = preflightZhangPeaControllerCheckpointSection(
		controllerSection->payload, expectedEpochIntervalSeconds,
		candidatePlan.controller);
	if (!controllerResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CONTROLLER_PREFLIGHT_FAILED:" +
			controllerResult.failureReason);
	}
	if (restoreZhangCheckpointTime(
		candidatePlan.controller.state.completedTsync) != candidateCore.time)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CONTROLLER_CORE_EPOCH_MISMATCH");
	}

	auto satelliteResult =
		preflightZhangSatelliteRuntimeCheckpointSection(
			configuredNavigation, bundle.manifest.runtimeId,
			satelliteSection->payload, candidatePlan.satellites);
	if (!satelliteResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_SATELLITE_PREFLIGHT_FAILED:" +
			satelliteResult.failureReason);
	}
	auto receiverResult =
		preflightZhangReceiverRuntimeCheckpointSection(
			configuredReceivers, bundle.manifest.runtimeId,
			receiverSection->payload, candidatePlan.receivers);
	if (!receiverResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_RECEIVER_PREFLIGHT_FAILED:" +
			receiverResult.failureReason);
	}
	auto streamResult = preflightZhangRinexFileStreamsCheckpointSection(
		configuredStreams, configuredStreamDoneMap, streamSection->payload,
		candidatePlan.rinexStreams);
	if (!streamResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_STREAM_PREFLIGHT_FAILED:" +
			streamResult.failureReason);
	}

	if (!validateZhangGraphCheckpointSection(
		bundle.manifest.runtimeId, graphSection->payload, failureReason))
	{
		return checkpointFailure(
			"E29_CHECKPOINT_GRAPH_PREFLIGHT_FAILED:" + failureReason);
	}

	const ZhangCheckpointKfCore configuredCoreBefore =
		captureZhangCheckpointKfCore(configuredState);
	auto pppArResult = importZhangPppArCheckpointSection(
		configuredState, bundle.manifest.runtimeId, pppArSection->payload,
		true, candidatePlan.stateDimension);
	if (!pppArResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_PPP_AR_PREFLIGHT_FAILED:" +
			pppArResult.failureReason);
	}
	auto ambresResult = importZhangAmbresCheckpointSection(
		configuredState, bundle.manifest.runtimeId, ambresSection->payload,
		true);
	if (!ambresResult.valid)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_AMBRES_PREFLIGHT_FAILED:" +
			ambresResult.failureReason);
	}
	if (!zhangCheckpointKfCoreBitwiseEqual(
		configuredCoreBefore, captureZhangCheckpointKfCore(configuredState)))
	{
		return checkpointFailure(
			"E29_CHECKPOINT_MODULE_PREFLIGHT_MUTATED_KF_CORE");
	}

	candidatePlan.pppArPendingTransitions =
		pppArResult.pendingTransitions;
	candidatePlan.pppArPendingSnapshotPins =
		pppArResult.pendingSnapshotPins;
	candidatePlan.ambresActiveTemporalTransitions =
		ambresResult.activeTemporalTransitions;
	ZhangCheckpointSnapshotReferenceSummary pppArSummary;
	ZhangCheckpointSnapshotReferenceSummary ambresSummary;
	if (!validateCrossSectionSnapshotReferences(
		bundle.manifest.runtimeId, pppArSection->payload,
		ambresSection->payload, pppArSummary, ambresSummary,
		failureReason))
	{
		return checkpointFailure(failureReason);
	}
	if (pppArSummary.transitionCount !=
		candidatePlan.pppArPendingTransitions
		|| pppArSummary.pinnedSnapshotIdentities.size() !=
			candidatePlan.pppArPendingSnapshotPins
		|| ambresSummary.transitionCount !=
			candidatePlan.ambresActiveTemporalTransitions)
	{
		return checkpointFailure(
			"E29_CHECKPOINT_CROSS_MODULE_SUMMARY_COUNT_MISMATCH");
	}

	candidatePlan.valid = true;
	plan = std::move(candidatePlan);
	ZhangE29CheckpointResult result;
	result.valid = true;
	result.failureReason = "NONE";
	result.checkpointId = bundle.manifest.checkpointId;
	result.runtimeId = bundle.manifest.runtimeId;
	result.sectionCount = bundle.sections.size();
	result.stateDimension = static_cast<int>(bundle.kfCore.x.size());
	return result;
}

ZhangE29CheckpointResult commitZhangE29CheckpointBundle(
	const ZhangCheckpointBundle& bundle,
	const ZhangE29CheckpointProvenance& provenance,
	KFState& authoritativeState,
	ReceiverMap& receivers,
	Navigation& navigation,
	std::multimap<std::string, StreamParserPtr>& streams,
	std::map<std::string, bool>& streamDoneMap,
	const ZhangE29CheckpointRestorePlan& plan,
	ZhangPeaControllerCheckpointState& restoredController)
{
	std::lock_guard<std::recursive_mutex> lock(e29CheckpointRuntimeMutex);
	if (!plan.valid || plan.runtimeId != bundle.manifest.runtimeId
		|| plan.checkpointId != bundle.manifest.checkpointId
		|| plan.stateDimension != bundle.kfCore.x.size()
		|| plan.bundleIdentitySha256 != bundleIdentitySha256(bundle))
	{
		return checkpointFailure("E29_CHECKPOINT_RESTORE_PLAN_MISMATCH");
	}

	ZhangE29CheckpointRestorePlan checkedPlan;
	auto preflightResult = preflightZhangE29CheckpointBundle(
		bundle, provenance, authoritativeState, receivers, navigation,
		streams, streamDoneMap,
		plan.controller.state.epochIntervalSeconds, checkedPlan);
	if (!preflightResult.valid)
	{
		return preflightResult;
	}
	if (checkedPlan.bundleIdentitySha256 != plan.bundleIdentitySha256)
	{
		return checkpointFailure("E29_CHECKPOINT_RESTORE_PLAN_RECHECK_MISMATCH");
	}

	bool commitStarted = false;
	auto commitFailure = [&](const std::string& reason)
	{
		auto result = checkpointFailure(reason);
		result.checkpointId = bundle.manifest.checkpointId;
		result.runtimeId = bundle.manifest.runtimeId;
		result.sectionCount = bundle.sections.size();
		result.stateDimension = static_cast<int>(bundle.kfCore.x.size());
		result.liveStateMayBePartial = commitStarted;
		return result;
	};

	commitStarted = true;
	auto satelliteResult = importZhangSatelliteRuntimeCheckpointSection(
		navigation, bundle.manifest.runtimeId, checkedPlan.satellites);
	if (!satelliteResult.valid)
	{
		return commitFailure(
			"E29_CHECKPOINT_SATELLITE_COMMIT_FAILED:" +
			satelliteResult.failureReason);
	}
	auto receiverResult = importZhangReceiverRuntimeCheckpointSection(
		receivers, bundle.manifest.runtimeId, checkedPlan.receivers);
	if (!receiverResult.valid)
	{
		return commitFailure(
			"E29_CHECKPOINT_RECEIVER_COMMIT_FAILED:" +
			receiverResult.failureReason);
	}
	auto streamResult = commitZhangRinexFileStreamsCheckpointSection(
		streams, streamDoneMap, checkedPlan.rinexStreams);
	if (!streamResult.valid)
	{
		return commitFailure(
			"E29_CHECKPOINT_STREAM_COMMIT_FAILED:" +
			streamResult.failureReason);
	}

	std::string failureReason;
	if (!restoreZhangCheckpointKfCoreWithReceiverResolver(
		bundle.kfCore, authoritativeState, receiverResolver(receivers),
		&failureReason))
	{
		return commitFailure(
			"E29_CHECKPOINT_CORE_COMMIT_FAILED:" + failureReason);
	}
	if (zhangCheckpointRuntimeId(authoritativeState) !=
		bundle.manifest.runtimeId
		|| authoritativeState.x.size() != checkedPlan.stateDimension)
	{
		return commitFailure(
			"E29_CHECKPOINT_CORE_COMMIT_IDENTITY_MISMATCH");
	}

	const auto& graphPayload = bundle.sections.at(
		ZHANG_GRAPH_CHECKPOINT_SECTION_NAME).payload;
	if (!importZhangGraphCheckpointSection(
		authoritativeState, bundle.manifest.runtimeId, graphPayload,
		failureReason))
	{
		return commitFailure(
			"E29_CHECKPOINT_GRAPH_COMMIT_FAILED:" + failureReason);
	}
	const auto& pppArPayload = bundle.sections.at(
		ZHANG_PPP_AR_CHECKPOINT_SECTION_NAME).payload;
	auto pppArResult = importZhangPppArCheckpointSection(
		authoritativeState, bundle.manifest.runtimeId, pppArPayload,
		false, checkedPlan.stateDimension);
	if (!pppArResult.valid)
	{
		return commitFailure(
			"E29_CHECKPOINT_PPP_AR_COMMIT_FAILED:" +
			pppArResult.failureReason);
	}
	const auto& ambresPayload = bundle.sections.at(
		ZHANG_AMBRES_CHECKPOINT_SECTION_NAME).payload;
	auto ambresResult = importZhangAmbresCheckpointSection(
		authoritativeState, bundle.manifest.runtimeId, ambresPayload, false);
	if (!ambresResult.valid)
	{
		return commitFailure(
			"E29_CHECKPOINT_AMBRES_COMMIT_FAILED:" +
			ambresResult.failureReason);
	}

	configureZhangE18FactorCapture(authoritativeState);
	auto controllerResult = commitZhangPeaControllerCheckpointSection(
		checkedPlan.controller, restoredController);
	if (!controllerResult.valid)
	{
		return commitFailure(
			"E29_CHECKPOINT_CONTROLLER_COMMIT_FAILED:" +
			controllerResult.failureReason);
	}
	if (!sameControllerEpochAsFilter(
		authoritativeState, restoredController)
		|| !zhangCheckpointKfCoreBitwiseEqual(
			bundle.kfCore,
			captureZhangCheckpointKfCore(authoritativeState)))
	{
		return commitFailure(
			"E29_CHECKPOINT_POST_COMMIT_CORE_INVARIANT_FAILED");
	}

	ZhangE29CheckpointResult result;
	result.valid = true;
	result.failureReason = "NONE";
	result.checkpointId = bundle.manifest.checkpointId;
	result.runtimeId = bundle.manifest.runtimeId;
	result.sectionCount = bundle.sections.size();
	result.stateDimension = static_cast<int>(bundle.kfCore.x.size());
	return result;
}
