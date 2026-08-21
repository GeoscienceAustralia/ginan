#define BOOST_TEST_MODULE ZhangCheckpointInfra0Tests
#include <boost/test/unit_test.hpp>

#include <boost/archive/binary_oarchive.hpp>

#include <chrono>
#include <filesystem>
#include <fstream>
#include <memory>
#include <sstream>
#include <string>

#include "common/observations.hpp"
#include "common/receiver.hpp"
#include "common/streamFile.hpp"
#include "common/streamObs.hpp"
#include "common/streamRinex.hpp"
#include "common/zhangCheckpoint.hpp"
#include "pea/zhangInputCheckpoint.hpp"

namespace
{
struct ControllerEnvelopeForTest
{
	std::uint32_t schemaVersion = 0;
	std::string sectionName;
	ZhangPeaControllerCheckpointState state;

	template <class ARCHIVE>
	void serialize(ARCHIVE& archive, const unsigned int&)
	{
		archive & schemaVersion;
		archive & sectionName;
		archive & state;
	}
};

struct TemporaryPath
{
	std::filesystem::path path;

	explicit TemporaryPath(const std::string& suffix)
	{
		const auto nonce =
			std::chrono::steady_clock::now().time_since_epoch().count();
		path = std::filesystem::temp_directory_path() /
			("ginan_e29_infra0_" + std::to_string(nonce) + suffix);
	}

	~TemporaryPath()
	{
		std::error_code ignored;
		std::filesystem::remove_all(path, ignored);
	}
};

ZhangPeaControllerCheckpointState validControllerState()
{
	GTime completed;
	completed.bigTime = 1563408000.25L;
	ZhangPeaControllerCheckpointState state;
	const auto result = makeZhangPeaPostEpochCheckpointState(
		37, captureZhangCheckpointTime(completed), 60, state);
	BOOST_REQUIRE_MESSAGE(result.valid, result.failureReason);
	return state;
}

std::string serializeControllerEnvelope(
	std::uint32_t schemaVersion,
	const std::string& sectionName,
	const ZhangPeaControllerCheckpointState& state)
{
	std::ostringstream output(std::ios::binary | std::ios::out);
	boost::archive::binary_oarchive archive(
		output, boost::archive::no_header);
	archive << ControllerEnvelopeForTest{schemaVersion, sectionName, state};
	return output.str();
}

bool sameControllerState(
	const ZhangPeaControllerCheckpointState& left,
	const ZhangPeaControllerCheckpointState& right)
{
	return left.completedEpoch == right.completedEpoch
		&& left.completedTsync.bigTimeBytes == right.completedTsync.bigTimeBytes
		&& left.nextEpoch == right.nextEpoch
		&& left.nextTsync.bigTimeBytes == right.nextTsync.bigTimeBytes
		&& left.epochIntervalSeconds == right.epochIntervalSeconds
		&& left.boundary == right.boundary
		&& left.resumePolicy == right.resumePolicy;
}

std::shared_ptr<Observation> makeQueuedObservation(
	long double time,
	int prn,
	double phase)
{
	GObs observation = {};
	observation.time.bigTime = time;
	observation.mount = "R0";
	observation.ephVar = 0.25;
	observation.stecToDelay = 1;
	observation.stecType = 0;
	observation.stecVal = 2;
	observation.stecVar = 3;
	observation.stecCodeCombo = 0;
	observation.ionoSat = SatSys();
	observation.sppCodeResidual = 0.5;
	observation.tropSlant = 2.1;
	observation.tropSlantVar = 0.01;
	observation.posTime.bigTime = time - 0.07L;
	observation.Sat = SatSys(E_Sys::GPS, prn);
	observation.posSource = E_Source::PRECISE;
	observation.clkSource = E_Source::PRECISE;
	observation.rSatCom.setZero();
	observation.rSatApc.setZero();
	observation.satVel.setZero();
	observation.rSatEciDt.setZero();
	observation.vSatEciDt.setZero();
	observation.rSatEci0.setZero();
	observation.vSatEci0.setZero();
	observation.posVar = 0.04;
	observation.satClk = 1e-6;
	observation.satClkVel = 2e-12;
	observation.satClkVar = 1e-18;
	observation.sppValid = true;
	observation.iodeClk = 7;
	observation.iodePos = 7;
	observation.ephPosValid = true;
	observation.ephClkValid = true;
	observation.tof = 0.07;

	Sig signal;
	signal.code = E_ObsCode::L1C;
	signal.L = phase;
	signal.P = 22000000 + prn;
	signal.D = -123.5;
	signal.snr = 48;
	signal.codeVar = 0.5;
	signal.phasVar = 0.0004;
	observation.sigs[F1] = signal;
	observation.sigsLists[F1].push_back(signal);

	std::shared_ptr<GObs> typed = observation;
	return typed;
}

struct RinexFixture
{
	TemporaryPath input{".rnx"};
	std::shared_ptr<ObsStream> observationStream;
	std::multimap<std::string, StreamParserPtr> streams;
	std::map<std::string, bool> done;

	RinexFixture()
	{
		{
			std::ofstream output(input.path, std::ios::binary | std::ios::trunc);
			output << "deterministic-rinex-checkpoint-fixture-0123456789";
		}
		auto file = std::make_unique<FileStream>(input.path.string());
		file->sourceString = "rinex://R0";
		file->filePos = 17;
		auto parser = std::make_unique<RinexParser>();
		parser->ctype = 'O';
		parser->version = 3.04;
		parser->nav_system = E_Sys::GPS;
		parser->time_system = E_TimeSys::GPST;
		parser->rnxRec.id = "R0";
		parser->rnxRec.marker = "E29-CHECKPOINT";
		parser->rnxRec.del.setZero();
		parser->rnxRec.pos.setZero();
		parser->tempObsList.push_back(
			makeQueuedObservation(1563408015.0L, 3, 101.25));
		ObsList epoch1;
		epoch1.push_back(makeQueuedObservation(1563408030.0L, 7, 202.5));
		ObsList epoch2;
		epoch2.push_back(makeQueuedObservation(1563408060.0L, 8, 303.75));
		parser->obsListList.push_back(std::move(epoch1));
		parser->obsListList.push_back(std::move(epoch2));

		observationStream = std::make_shared<ObsStream>(
			std::move(file), std::move(parser), false);
		observationStream->obsAgeCode = E_ObsAgeCode::FUTURE_OBS;
		observationStream->lastReadTime.bigTime = 1563408000.0L;
		observationStream->interval = 30;
		streams.emplace("R0", observationStream);
		done["rinex://R0"] = true;
	}

	FileStream& file()
	{
		return dynamic_cast<FileStream&>(observationStream->stream);
	}

	RinexParser& parser()
	{
		return dynamic_cast<RinexParser&>(observationStream->parser);
	}
};

Receiver& resolverReceiver()
{
	static Receiver receiver;
	receiver.id = "R0";
	return receiver;
}

ZhangCheckpointBundle minimalBundle()
{
	KFState state;
	state.metaDataMap[ZHANG_CHECKPOINT_RUNTIME_ID_METADATA] = "runtime-00";
	state.time.bigTime = 1563408000.0L;

	ZhangCheckpointBundle bundle;
	bundle.manifest.runtimeId = "runtime-00";
	bundle.manifest.checkpointId = "checkpoint-00";
	bundle.manifest.parentCheckpointId = "cold-start";
	bundle.manifest.epoch = "2019-07-18T00:00:00Z";
	bundle.manifest.binarySha256 = std::string(64, 'a');
	bundle.manifest.configText = "config";
	bundle.manifest.inputManifestText = "inputs";
	bundle.manifest.configSha256 =
		zhangCheckpointSha256(bundle.manifest.configText);
	bundle.manifest.inputManifestSha256 =
		zhangCheckpointSha256(bundle.manifest.inputManifestText);
	bundle.manifest.platformFingerprint = "platform";
	bundle.manifest.compilerFingerprint = "compiler";
	bundle.manifest.linearAlgebraFingerprint = "eigen";
	bundle.manifest.endianness = "LITTLE";
	bundle.manifest.createdUtc = "2026-08-10T00:00:00Z";
	bundle.kfCore = captureZhangCheckpointKfCore(state);
	bundle.sections["section.a"] = {
		1, "payload-a", zhangCheckpointSha256("payload-a")};
	return bundle;
}

ZhangCheckpointExpectations minimalExpectations()
{
	ZhangCheckpointExpectations expectations;
	expectations.experimentMode = "E29_GPS_L1C_L2W_ZHANG_FULL_RANK";
	expectations.binarySha256 = std::string(64, 'a');
	expectations.configSha256 = zhangCheckpointSha256("config");
	expectations.inputManifestSha256 = zhangCheckpointSha256("inputs");
	expectations.platformFingerprint = "platform";
	expectations.compilerFingerprint = "compiler";
	expectations.linearAlgebraFingerprint = "eigen";
	expectations.endianness = "LITTLE";
	return expectations;
}
} // namespace

BOOST_AUTO_TEST_CASE(controller_post_epoch_boundary_roundtrips_exactly)
{
	const auto saved = validControllerState();
	std::string payload;
	auto exported = exportZhangPeaControllerCheckpointSection(saved, payload);
	BOOST_REQUIRE_MESSAGE(exported.valid, exported.failureReason);

	ZhangPeaControllerCheckpointRestorePlan plan;
	auto preflight = preflightZhangPeaControllerCheckpointSection(
		payload, 60, plan);
	BOOST_REQUIRE_MESSAGE(preflight.valid, preflight.failureReason);

	auto restored = validControllerState();
	restored.completedEpoch = 999;
	restored.nextEpoch = 1000;
	auto committed = commitZhangPeaControllerCheckpointSection(plan, restored);
	BOOST_REQUIRE_MESSAGE(committed.valid, committed.failureReason);
	BOOST_CHECK(sameControllerState(restored, saved));
}

BOOST_AUTO_TEST_CASE(controller_preflight_rejects_boundary_schema_and_interval_drift)
{
	const auto saved = validControllerState();
	std::string payload;
	BOOST_REQUIRE(exportZhangPeaControllerCheckpointSection(saved, payload).valid);

	ZhangPeaControllerCheckpointRestorePlan plan;
	auto intervalFailure = preflightZhangPeaControllerCheckpointSection(
		payload, 30, plan);
	BOOST_CHECK(!intervalFailure.valid);
	BOOST_CHECK_EQUAL(
		intervalFailure.failureReason,
		"PEA_CONTROLLER_CHECKPOINT_EPOCH_INTERVAL_MISMATCH");

	const auto wrongSchema = serializeControllerEnvelope(
		ZHANG_PEA_CONTROLLER_CHECKPOINT_SCHEMA_VERSION + 1,
		ZHANG_PEA_CONTROLLER_CHECKPOINT_SECTION_NAME,
		saved);
	auto schemaFailure = preflightZhangPeaControllerCheckpointSection(
		wrongSchema, 60, plan);
	BOOST_CHECK(!schemaFailure.valid);
	BOOST_CHECK_EQUAL(
		schemaFailure.failureReason,
		"PEA_CONTROLLER_CHECKPOINT_SCHEMA_MISMATCH");

	auto wrongBoundaryState = saved;
	wrongBoundaryState.boundary = "MID_EPOCH";
	const auto wrongBoundary = serializeControllerEnvelope(
		ZHANG_PEA_CONTROLLER_CHECKPOINT_SCHEMA_VERSION,
		ZHANG_PEA_CONTROLLER_CHECKPOINT_SECTION_NAME,
		wrongBoundaryState);
	auto boundaryFailure = preflightZhangPeaControllerCheckpointSection(
		wrongBoundary, 60, plan);
	BOOST_CHECK(!boundaryFailure.valid);
	BOOST_CHECK_EQUAL(
		boundaryFailure.failureReason,
		"PEA_CONTROLLER_CHECKPOINT_UNSUPPORTED_BOUNDARY");
}

BOOST_AUTO_TEST_CASE(controller_failed_commit_does_not_modify_destination)
{
	const auto saved = validControllerState();
	std::string payload;
	BOOST_REQUIRE(exportZhangPeaControllerCheckpointSection(saved, payload).valid);
	ZhangPeaControllerCheckpointRestorePlan plan;
	BOOST_REQUIRE(preflightZhangPeaControllerCheckpointSection(
		payload, 60, plan).valid);

	auto destination = validControllerState();
	destination.completedEpoch = 700;
	destination.nextEpoch = 701;
	const auto before = destination;
	plan.payloadSha256[0] = plan.payloadSha256[0] == '0' ? '1' : '0';
	auto result = commitZhangPeaControllerCheckpointSection(
		plan, destination);
	BOOST_CHECK(!result.valid);
	BOOST_CHECK_EQUAL(
		result.failureReason,
		"PEA_CONTROLLER_CHECKPOINT_PLAN_DIGEST_MISMATCH");
	BOOST_CHECK(sameControllerState(destination, before));
}

BOOST_AUTO_TEST_CASE(rinex_cursor_queue_and_done_ledger_roundtrip_exactly)
{
	RinexFixture fixture;
	std::string payload;
	auto exported = exportZhangRinexFileStreamsCheckpointSection(
		fixture.streams, fixture.done, payload);
	BOOST_REQUIRE_MESSAGE(exported.valid, exported.failureReason);
	BOOST_CHECK_EQUAL(exported.streamCount, 1);
	BOOST_CHECK_EQUAL(exported.queuedEpochCount, 3);
	BOOST_CHECK_EQUAL(exported.queuedObservationCount, 3);

	fixture.file().filePos = 2;
	fixture.observationStream->obsAgeCode = E_ObsAgeCode::PAST_OBS;
	fixture.observationStream->lastReadTime.bigTime = 1;
	fixture.observationStream->interval = 999;
	fixture.parser().tempObsList.clear();
	fixture.parser().obsListList.clear();
	fixture.done["rinex://R0"] = false;

	ZhangRinexFileStreamsCheckpointRestorePlan plan;
	auto preflight = preflightZhangRinexFileStreamsCheckpointSection(
		fixture.streams, fixture.done, payload, plan);
	BOOST_REQUIRE_MESSAGE(preflight.valid, preflight.failureReason);
	auto committed = commitZhangRinexFileStreamsCheckpointSection(
		fixture.streams, fixture.done, plan);
	BOOST_REQUIRE_MESSAGE(committed.valid, committed.failureReason);

	BOOST_CHECK_EQUAL(fixture.file().filePos, 17);
	BOOST_CHECK(fixture.observationStream->obsAgeCode == E_ObsAgeCode::FUTURE_OBS);
	BOOST_CHECK_EQUAL(fixture.observationStream->interval, 30);
	BOOST_CHECK_EQUAL(fixture.done.at("rinex://R0"), true);
	BOOST_REQUIRE_EQUAL(fixture.parser().tempObsList.size(), 1);
	BOOST_REQUIRE_EQUAL(fixture.parser().obsListList.size(), 2);
	const auto& firstFuture = fixture.parser().obsListList.front();
	BOOST_REQUIRE_EQUAL(firstFuture.size(), 1);
	const auto* observation = dynamic_cast<const GObs*>(firstFuture.front().get());
	BOOST_REQUIRE(observation != nullptr);
	BOOST_CHECK(observation->Sat == SatSys(E_Sys::GPS, 7));
	BOOST_CHECK_EQUAL(observation->sigs.at(F1).L, 202.5);
	BOOST_CHECK(observation->rec_ptr == nullptr);
	BOOST_CHECK(observation->satNav_ptr == nullptr);
	BOOST_CHECK(observation->satStat_ptr == nullptr);
}

BOOST_AUTO_TEST_CASE(rinex_failed_commit_and_preflight_leave_live_stream_unchanged)
{
	RinexFixture fixture;
	std::string payload;
	BOOST_REQUIRE(exportZhangRinexFileStreamsCheckpointSection(
		fixture.streams, fixture.done, payload).valid);
	ZhangRinexFileStreamsCheckpointRestorePlan plan;
	BOOST_REQUIRE(preflightZhangRinexFileStreamsCheckpointSection(
		fixture.streams, fixture.done, payload, plan).valid);

	fixture.file().filePos = 9;
	fixture.observationStream->interval = 12.5;
	fixture.done["rinex://R0"] = false;
	const auto queueSizeBefore = fixture.parser().obsListList.size();
	plan.payloadSha256[0] = plan.payloadSha256[0] == '0' ? '1' : '0';
	auto commitFailure = commitZhangRinexFileStreamsCheckpointSection(
		fixture.streams, fixture.done, plan);
	BOOST_CHECK(!commitFailure.valid);
	BOOST_CHECK_EQUAL(
		commitFailure.failureReason,
		"RINEX_STREAM_CHECKPOINT_PLAN_DIGEST_MISMATCH");
	BOOST_CHECK_EQUAL(fixture.file().filePos, 9);
	BOOST_CHECK_EQUAL(fixture.observationStream->interval, 12.5);
	BOOST_CHECK_EQUAL(fixture.done.at("rinex://R0"), false);
	BOOST_CHECK_EQUAL(fixture.parser().obsListList.size(), queueSizeBefore);

	auto wrongDoneInventory = fixture.done;
	wrongDoneInventory["unexpected-source"] = true;
	ZhangRinexFileStreamsCheckpointRestorePlan ignoredPlan;
	auto preflightFailure = preflightZhangRinexFileStreamsCheckpointSection(
		fixture.streams, wrongDoneInventory, payload, ignoredPlan);
	BOOST_CHECK(!preflightFailure.valid);
	BOOST_CHECK_EQUAL(fixture.file().filePos, 9);
	BOOST_CHECK_EQUAL(fixture.observationStream->interval, 12.5);
	BOOST_CHECK_EQUAL(fixture.done.at("rinex://R0"), false);
	BOOST_CHECK_EQUAL(fixture.parser().obsListList.size(), queueSizeBefore);
}

BOOST_AUTO_TEST_CASE(required_sections_reject_bad_spec_schema_payload_and_hash)
{
	const auto bundle = minimalBundle();
	std::string failure;
	BOOST_CHECK(validateZhangCheckpointRequiredSections(
		bundle, {{"section.a", 1}}, &failure));
	BOOST_CHECK_EQUAL(failure, "NONE");

	BOOST_CHECK(!validateZhangCheckpointRequiredSections(
		bundle, {{"section.a", 1}, {"section.a", 1}}, &failure));
	BOOST_CHECK_EQUAL(failure, "CHECKPOINT_REQUIRED_SECTION_SPEC_INVALID");
	BOOST_CHECK(!validateZhangCheckpointRequiredSections(
		bundle, {{"section.a", 0}}, &failure));
	BOOST_CHECK_EQUAL(failure, "CHECKPOINT_REQUIRED_SECTION_SPEC_INVALID");

	auto wrongSchema = bundle;
	wrongSchema.sections.at("section.a").schemaVersion = 2;
	BOOST_CHECK(!validateZhangCheckpointRequiredSections(
		wrongSchema, {{"section.a", 1}}, &failure));
	BOOST_CHECK_EQUAL(
		failure,
		"CHECKPOINT_REQUIRED_SECTION_VERSION_MISMATCH:section.a");

	auto emptyPayload = bundle;
	emptyPayload.sections.at("section.a").payload.clear();
	emptyPayload.sections.at("section.a").sha256 =
		zhangCheckpointSha256("");
	BOOST_CHECK(!validateZhangCheckpointRequiredSections(
		emptyPayload, {{"section.a", 1}}, &failure));
	BOOST_CHECK_EQUAL(
		failure, "CHECKPOINT_REQUIRED_SECTION_HASH_MISMATCH:section.a");

	auto wrongHash = bundle;
	wrongHash.sections.at("section.a").sha256 = std::string(64, 'f');
	BOOST_CHECK(!validateZhangCheckpointRequiredSections(
		wrongHash, {{"section.a", 1}}, &failure));
	BOOST_CHECK_EQUAL(
		failure, "CHECKPOINT_REQUIRED_SECTION_HASH_MISMATCH:section.a");
}

BOOST_AUTO_TEST_CASE(bundle_read_failure_does_not_replace_callers_live_bundle)
{
	TemporaryPath file{".checkpoint"};
	auto source = minimalBundle();
	auto writeResult = writeZhangCheckpointBundle(file.path.string(), source);
	BOOST_REQUIRE_MESSAGE(writeResult.valid, writeResult.failureReason);

	ZhangCheckpointBundle destination;
	destination.manifest.runtimeId = "live-sentinel";
	destination.manifest.checkpointId = "must-survive";
	auto expectations = minimalExpectations();
	expectations.configSha256 = std::string(64, 'f');
	auto readResult = readZhangCheckpointBundle(
		file.path.string(), expectations, destination);
	BOOST_CHECK(!readResult.valid);
	BOOST_CHECK_EQUAL(
		readResult.failureReason, "CHECKPOINT_PROVENANCE_MISMATCH");
	BOOST_CHECK_EQUAL(destination.manifest.runtimeId, "live-sentinel");
	BOOST_CHECK_EQUAL(destination.manifest.checkpointId, "must-survive");
}

BOOST_AUTO_TEST_CASE(receiver_pointer_rebind_failure_is_side_effect_free)
{
	KFState source;
	source.time.bigTime = 1563408000.0L;
	source.x = VectorXd::Ones(2);
	source.P = MatrixXd::Identity(2, 2);
	source.dx = VectorXd::Zero(2);
	KFKey receiverClock;
	receiverClock.type = KF::REC_CLOCK;
	receiverClock.str = "R0";
	receiverClock.rec_ptr = &resolverReceiver();
	source.kfIndexMap[receiverClock] = 1;
	source.metaDataMap[ZHANG_CHECKPOINT_RUNTIME_ID_METADATA] = "runtime-00";
	const auto snapshot = captureZhangCheckpointKfCore(source);

	KFState destination;
	destination.id = "live-sentinel";
	destination.x(0) = 7;
	const auto before = captureZhangCheckpointKfCore(destination);
	std::string failure;
	BOOST_CHECK(!restoreZhangCheckpointKfCoreWithReceiverResolver(
		snapshot,
		destination,
		[](const std::string&) -> Receiver* { return nullptr; },
		&failure));
	BOOST_CHECK_EQUAL(
		failure, "CHECKPOINT_CORE_RECEIVER_POINTER_REBIND_FAILED:R0");
	BOOST_CHECK(zhangCheckpointKfCoreBitwiseEqual(
		before, captureZhangCheckpointKfCore(destination)));

	BOOST_REQUIRE(restoreZhangCheckpointKfCoreWithReceiverResolver(
		snapshot,
		destination,
		[](const std::string& id) -> Receiver*
		{
			return id == "R0" ? &resolverReceiver() : nullptr;
		},
		&failure));
	bool rebound = false;
	for (const auto& [key, index] : destination.kfIndexMap)
	{
		if (index == 1)
		{
			rebound = key.rec_ptr == &resolverReceiver();
		}
	}
	BOOST_CHECK(rebound);
}
