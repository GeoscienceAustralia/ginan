#define BOOST_TEST_MODULE receiver_metadata_tests
#include <boost/test/unit_test.hpp>

#include "common/acsConfig.hpp"
#include "common/enumHelpers.hpp"
#include "common/receiver.hpp"
#include "common/rtcmDecoder.hpp"
#include "common/sinex.hpp"

static void expectVector3dEq(const Vector3d& actual, const Vector3d& expected)
{
    BOOST_TEST(actual.x() == expected.x());
    BOOST_TEST(actual.y() == expected.y());
    BOOST_TEST(actual.z() == expected.z());
}

static void expectPriorityEq(
    const vector<E_ReceiverMetaSource>& actual,
    const vector<E_ReceiverMetaSource>& expected)
{
    BOOST_REQUIRE(actual.size() == expected.size());

    for (size_t i = 0; i < expected.size(); i++)
    {
        BOOST_TEST(static_cast<int>(actual[i]) == static_cast<int>(expected[i]));
    }
}

BOOST_AUTO_TEST_CASE(receiver_metadata_source_priority_prefers_higher_priority_source)
{
    ReceiverMetaField<string> field;
    auto priority = vector<E_ReceiverMetaSource>{
        E_ReceiverMetaSource::CONFIG,
        E_ReceiverMetaSource::SINEX,
        E_ReceiverMetaSource::RINEX,
        E_ReceiverMetaSource::RTCM};

    ingestReceiverMetaField(
        field,
        string("rtcm-value"),
        true,
        E_ReceiverMetaSource::RTCM,
        priority
    );
    ingestReceiverMetaField(
        field,
        string("config-value"),
        true,
        E_ReceiverMetaSource::CONFIG,
        priority
    );

    BOOST_TEST(field.valid);
    BOOST_TEST(field.value == "config-value");
    BOOST_TEST(static_cast<int>(field.winningSource) == static_cast<int>(E_ReceiverMetaSource::CONFIG));
    BOOST_TEST(field.hasSource(E_ReceiverMetaSource::CONFIG));
    BOOST_TEST(field.hasSource(E_ReceiverMetaSource::RTCM));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_source_priority_keeps_existing_for_empty_update)
{
    ReceiverMetaField<string> field;
    auto priority = defaultReceiverMetaSourcePriority();

    ingestReceiverMetaField(
        field,
        string("rinex-value"),
        true,
        E_ReceiverMetaSource::RINEX,
        priority
    );
    ingestReceiverMetaField(
        field,
        string(""),
        false,
        E_ReceiverMetaSource::CONFIG,
        priority
    );

    BOOST_TEST(field.valid);
    BOOST_TEST(field.value == "rinex-value");
    BOOST_TEST(static_cast<int>(field.winningSource) == static_cast<int>(E_ReceiverMetaSource::RINEX));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_priority_index_orders_known_sources_first)
{
    auto priority = vector<E_ReceiverMetaSource>{
        E_ReceiverMetaSource::SINEX,
        E_ReceiverMetaSource::RINEX,
        E_ReceiverMetaSource::RTCM};

    BOOST_TEST(
        receiverMetaPriorityIndex(E_ReceiverMetaSource::SINEX, priority) <
        receiverMetaPriorityIndex(E_ReceiverMetaSource::RTCM, priority));
    BOOST_TEST(
        receiverMetaPriorityIndex(E_ReceiverMetaSource::CONFIG, priority) >
        receiverMetaPriorityIndex(E_ReceiverMetaSource::RTCM, priority));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_receiver_options_default_priority_matches_metadata_default)
{
    ReceiverOptions recOpts;

    expectPriorityEq(recOpts.meta_priority, defaultReceiverMetaSourcePriority());
}

BOOST_AUTO_TEST_CASE(receiver_metadata_receiver_options_default_pos_sources_use_meta_layering)
{
    ReceiverOptions recOpts;

    BOOST_REQUIRE(recOpts.posModel.sources.size() == 4);
    BOOST_TEST(static_cast<int>(recOpts.posModel.sources[0]) == static_cast<int>(E_Source::KALMAN));
    BOOST_TEST(static_cast<int>(recOpts.posModel.sources[1]) == static_cast<int>(E_Source::META));
    BOOST_TEST(static_cast<int>(recOpts.posModel.sources[2]) == static_cast<int>(E_Source::SPP));
    BOOST_TEST(static_cast<int>(recOpts.posModel.sources[3]) == static_cast<int>(E_Source::REMOTE));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_source_priority_strings_parse_case_insensitively)
{
    vector<string> sourceStrings = {"rtcm", "RINEX", "Sinex", "CONFIG"};

    vector<E_ReceiverMetaSource> priority;
    for (const auto& sourceString : sourceStrings)
    {
        priority.push_back(string_to_enum_nocase_throw<E_ReceiverMetaSource>(sourceString));
    }

    expectPriorityEq(
        priority,
        {
            E_ReceiverMetaSource::RTCM,
            E_ReceiverMetaSource::RINEX,
            E_ReceiverMetaSource::SINEX,
            E_ReceiverMetaSource::CONFIG});
}

BOOST_AUTO_TEST_CASE(receiver_metadata_ingest_config_applies_meta_priority)
{
    ReceiverMetadata metadata;

    ReceiverOptions recOpts;
    recOpts.meta_priority = {
        E_ReceiverMetaSource::RTCM,
        E_ReceiverMetaSource::CONFIG,
        E_ReceiverMetaSource::SINEX,
        E_ReceiverMetaSource::RINEX};
    recOpts.receiver_type = "CONFIG TYPE";

    RtcmStationInfo rtcmInfo;
    rtcmInfo.receiverType = "RTCM TYPE";

    metadata.ingestConfig(recOpts);
    metadata.ingestRtcm(rtcmInfo);

    BOOST_TEST(metadata.receiverType.value == "RTCM TYPE");
    BOOST_TEST(static_cast<int>(metadata.receiverType.winningSource) ==
               static_cast<int>(E_ReceiverMetaSource::RTCM));
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::CONFIG));
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::RTCM));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_config_enable_without_offset_does_not_mask_sinex_delta)
{
    ReceiverMetadata metadata;

    ReceiverOptions recOpts;
    recOpts.eccentricityModel.enable = true;

    SinexSiteEcc sinexEcc;
    sinexEcc.ecc = VectorEnu(Vector3d(0.4, 0.5, 0.6));

    SinexRecData recSnx;
    recSnx.ecc_ptr = &sinexEcc;

    metadata.ingestConfig(recOpts);
    metadata.ingestSinex(recSnx);

    BOOST_TEST(metadata.antennaDelta.valid);
    expectVector3dEq(metadata.antennaDelta.value, Vector3d(0.4, 0.5, 0.6));
    BOOST_TEST(static_cast<int>(metadata.antennaDelta.winningSource) ==
               static_cast<int>(E_ReceiverMetaSource::SINEX));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_source_priority_participates_in_option_inheritance)
{
    ReceiverOptions inheritedOpts;
    ReceiverOptions baseOpts;

    vector<E_ReceiverMetaSource> basePriority = {
        E_ReceiverMetaSource::RTCM,
        E_ReceiverMetaSource::RINEX,
        E_ReceiverMetaSource::SINEX,
        E_ReceiverMetaSource::CONFIG};

    setOption(baseOpts, baseOpts.meta_priority, basePriority);

    bool inherited =
        initIfNeeded(inheritedOpts, baseOpts, inheritedOpts.meta_priority);

    BOOST_TEST(inherited);
    BOOST_TEST(isInited(inheritedOpts, inheritedOpts.meta_priority));
    expectPriorityEq(inheritedOpts.meta_priority, basePriority);
}

BOOST_AUTO_TEST_CASE(receiver_metadata_ingest_rtcm_maps_station_fields)
{
    ReceiverMetadata metadata;

    RtcmStationInfo rtcmInfo;
    rtcmInfo.receiverType     = "TRIMBLE ALLOY";
    rtcmInfo.receiverFirmware = "6.45";
    rtcmInfo.receiverSerial   = "RTCM-REC";
    rtcmInfo.antennaDesc      = "TRM57971.00 NONE";
    rtcmInfo.antennaSerial    = "RTCM-ANT";
    rtcmInfo.antennaHeight    = 1.2345;
    rtcmInfo.hasAntennaHeight = true;
    rtcmInfo.physicalStationId = 7;
    rtcmInfo.physEcefX        = 1111.1;
    rtcmInfo.physEcefY        = 2222.2;
    rtcmInfo.physEcefZ        = 3333.3;

    metadata.ingestRtcm(rtcmInfo);

    BOOST_TEST(metadata.receiverType.valid);
    BOOST_TEST(metadata.receiverType.value == "TRIMBLE ALLOY");
    BOOST_TEST(metadata.receiverFirmware.value == "6.45");
    BOOST_TEST(metadata.receiverSerial.value == "RTCM-REC");
    BOOST_TEST(metadata.antennaDescriptor.value == "TRM57971.00 NONE");
    BOOST_TEST(metadata.antennaSerial.value == "RTCM-ANT");
    expectVector3dEq(metadata.antennaDelta.value, Vector3d(0, 0, 1.2345));
    expectVector3dEq(metadata.stationPosition.value, Vector3d(1111.1, 2222.2, 3333.3));
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::RTCM));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_ingest_rtcm_ignores_empty_physical_station_position)
{
    ReceiverMetadata metadata;

    ReceiverOptions recOpts;
    recOpts.apriori_pos = Vector3d(123.0, 456.0, 789.0);

    RtcmStationInfo rtcmInfo;
    rtcmInfo.physicalStationId = 7;

    metadata.ingestConfig(recOpts);
    metadata.ingestRtcm(rtcmInfo);

    BOOST_TEST(metadata.stationPosition.valid);
    expectVector3dEq(metadata.stationPosition.value, Vector3d(123.0, 456.0, 789.0));
    BOOST_TEST(static_cast<int>(metadata.stationPosition.winningSource) ==
               static_cast<int>(E_ReceiverMetaSource::CONFIG));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_ingest_rtcm_accepts_direct_station_position_without_physical_id)
{
    ReceiverMetadata metadata;

    RtcmStationInfo rtcmInfo;
    rtcmInfo.ecefX = 4444.4;
    rtcmInfo.ecefY = 5555.5;
    rtcmInfo.ecefZ = 6666.6;

    metadata.ingestRtcm(rtcmInfo);

    BOOST_TEST(metadata.stationPosition.valid);
    expectVector3dEq(metadata.stationPosition.value, Vector3d(4444.4, 5555.5, 6666.6));
    BOOST_TEST(static_cast<int>(metadata.stationPosition.winningSource) ==
               static_cast<int>(E_ReceiverMetaSource::RTCM));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_ingest_rinex_maps_header_fields)
{
    ReceiverMetadata metadata;

    RinexStation rnx;
    rnx.id           = "ABCD";
    rnx.marker       = "MARKER-01";
    rnx.antDesc      = "LEIAR25.R4 NONE";
    rnx.antSerial    = "RINEX-ANT";
    rnx.recType      = "SEPT POLARX5";
    rnx.recFWVersion = "5.4.0";
    rnx.recSerial    = "RINEX-REC";
    rnx.del          = Vector3d(0.1, 0.2, 0.3);
    rnx.pos          = Vector3d(4444.4, 5555.5, 6666.6);

    metadata.ingestRinex(rnx);

    BOOST_TEST(metadata.receiverType.value == "SEPT POLARX5");
    BOOST_TEST(metadata.receiverFirmware.value == "5.4.0");
    BOOST_TEST(metadata.receiverSerial.value == "RINEX-REC");
    BOOST_TEST(metadata.antennaDescriptor.value == "LEIAR25.R4 NONE");
    BOOST_TEST(metadata.antennaSerial.value == "RINEX-ANT");
    BOOST_TEST(metadata.markerName.value == "ABCD");
    BOOST_TEST(metadata.markerNumber.value == "MARKER-01");
    expectVector3dEq(metadata.antennaDelta.value, Vector3d(0.1, 0.2, 0.3));
    expectVector3dEq(metadata.stationPosition.value, Vector3d(4444.4, 5555.5, 6666.6));
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::RINEX));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_ingest_sinex_maps_lookup_fields)
{
    ReceiverMetadata metadata;

    SinexReceiver sinexReceiver;
    sinexReceiver.type = "JAVAD TRE_3";
    sinexReceiver.firm = "3.7.9";
    sinexReceiver.sn   = "SNXREC";

    SinexAntenna sinexAntenna;
    sinexAntenna.type = "JAVRINGANT_DM SCIS";
    sinexAntenna.sn   = "SNXANT";

    SinexSiteEcc sinexEcc;
    sinexEcc.ecc = VectorEnu(Vector3d(0.4, 0.5, 0.6));

    SinexRecData recSnx;
    recSnx.rec_ptr = &sinexReceiver;
    recSnx.ant_ptr = &sinexAntenna;
    recSnx.ecc_ptr = &sinexEcc;
    recSnx.pos     = VectorEcef(Vector3d(7777.7, 8888.8, 9999.9));

    metadata.ingestSinex(recSnx);

    BOOST_TEST(metadata.receiverType.value == "JAVAD TRE_3");
    BOOST_TEST(metadata.receiverFirmware.value == "3.7.9");
    BOOST_TEST(metadata.receiverSerial.value == "SNXREC");
    BOOST_TEST(metadata.antennaDescriptor.value == "JAVRINGANT_DM SCIS");
    BOOST_TEST(metadata.antennaSerial.value == "SNXANT");
    expectVector3dEq(metadata.antennaDelta.value, Vector3d(0.4, 0.5, 0.6));
    expectVector3dEq(metadata.stationPosition.value, Vector3d(7777.7, 8888.8, 9999.9));
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::SINEX));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_ingest_config_maps_receiver_options)
{
    ReceiverMetadata metadata;

    ReceiverOptions recOpts;
    recOpts.receiver_type = " TRIMBLE NETR9 ";
    recOpts.antenna_type  = " TRM59800.00 NONE ";
    recOpts.apriori_pos   = Vector3d(123.0, 456.0, 789.0);
    recOpts.eccentricityModel.enable = true;
    setOption(recOpts, recOpts.eccentricityModel.eccentricity, Vector3d(1.0, 2.0, 3.0));

    metadata.ingestConfig(recOpts);

    BOOST_TEST(metadata.receiverType.value == "TRIMBLE NETR9");
    BOOST_TEST(metadata.antennaDescriptor.value == "TRM59800.00 NONE");
    expectVector3dEq(metadata.antennaDelta.value, Vector3d(1.0, 2.0, 3.0));
    expectVector3dEq(metadata.stationPosition.value, Vector3d(123.0, 456.0, 789.0));
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::CONFIG));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_ingest_config_ignores_whitespace_strings)
{
    ReceiverMetadata metadata;

    ReceiverOptions recOpts;
    recOpts.receiver_type = "   ";
    recOpts.antenna_type  = "\t";

    metadata.ingestConfig(recOpts);

    BOOST_TEST(metadata.receiverType.valid == false);
    BOOST_TEST(metadata.antennaDescriptor.valid == false);
}

BOOST_AUTO_TEST_CASE(receiver_metadata_ingest_methods_respect_configured_priority)
{
    ReceiverMetadata metadata;
    metadata.setPriority({
        E_ReceiverMetaSource::SINEX,
        E_ReceiverMetaSource::RTCM,
        E_ReceiverMetaSource::RINEX,
        E_ReceiverMetaSource::CONFIG});

    RtcmStationInfo rtcmInfo;
    rtcmInfo.receiverType = "RTCM TYPE";

    RinexStation rnx;
    rnx.recType = "RINEX TYPE";

    SinexReceiver sinexReceiver;
    sinexReceiver.type = "SINEX TYPE";

    SinexRecData recSnx;
    recSnx.rec_ptr = &sinexReceiver;

    metadata.ingestRtcm(rtcmInfo);
    metadata.ingestRinex(rnx);
    metadata.ingestSinex(recSnx);

    BOOST_TEST(metadata.receiverType.value == "SINEX TYPE");
    BOOST_TEST(static_cast<int>(metadata.receiverType.winningSource) ==
               static_cast<int>(E_ReceiverMetaSource::SINEX));
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::RTCM));
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::RINEX));
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::SINEX));
}

BOOST_AUTO_TEST_CASE(receiver_metadata_unlisted_sources_are_ignored)
{
    ReceiverMetadata metadata;
    metadata.setPriority({E_ReceiverMetaSource::RTCM});

    RinexStation rnx;
    rnx.recType = "RINEX TYPE";
    rnx.pos     = Vector3d(4444.4, 5555.5, 6666.6);

    SinexReceiver sinexReceiver;
    sinexReceiver.type = "SINEX TYPE";

    SinexRecData recSnx;
    recSnx.rec_ptr = &sinexReceiver;
    recSnx.pos     = VectorEcef(Vector3d(7777.7, 8888.8, 9999.9));

    metadata.ingestRinex(rnx);
    metadata.ingestSinex(recSnx);

    BOOST_TEST(metadata.receiverType.valid == false);
    BOOST_TEST(metadata.stationPosition.valid == false);
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::RINEX) == false);
    BOOST_TEST(metadata.receiverType.hasSource(E_ReceiverMetaSource::SINEX) == false);
}

BOOST_AUTO_TEST_CASE(rtcm_metadata_selection_prefers_last_reference_station)
{
    map<int, RtcmStationInfo> stationInfoMap;
    stationInfoMap[10].receiverType = "STATION 10";
    stationInfoMap[20].receiverType = "STATION 20";

    auto* info = selectRtcmStationInfoForMetadata(stationInfoMap, 20);

    BOOST_REQUIRE(info != nullptr);
    BOOST_TEST(info->receiverType == "STATION 20");
}

BOOST_AUTO_TEST_CASE(rtcm_metadata_selection_uses_single_station_before_msm_reference)
{
    map<int, RtcmStationInfo> stationInfoMap;
    stationInfoMap[10].receiverType = "ONLY STATION";

    auto* info = selectRtcmStationInfoForMetadata(stationInfoMap, -1);

    BOOST_REQUIRE(info != nullptr);
    BOOST_TEST(info->receiverType == "ONLY STATION");
}

BOOST_AUTO_TEST_CASE(rtcm_metadata_selection_does_not_guess_with_multiple_stations)
{
    map<int, RtcmStationInfo> stationInfoMap;
    stationInfoMap[10].receiverType = "STATION 10";
    stationInfoMap[20].receiverType = "STATION 20";

    auto* info = selectRtcmStationInfoForMetadata(stationInfoMap, -1);

    BOOST_TEST(info == nullptr);
}

BOOST_AUTO_TEST_CASE(rtcm_metadata_selection_does_not_fallback_when_known_station_missing)
{
    map<int, RtcmStationInfo> stationInfoMap;
    stationInfoMap[10].receiverType = "STATION 10";

    auto* info = selectRtcmStationInfoForMetadata(stationInfoMap, 20);

    BOOST_TEST(info == nullptr);
}

BOOST_AUTO_TEST_CASE(rtcm_metadata_selection_keeps_non_physical_station_metadata_together)
{
    map<int, RtcmStationInfo> stationInfoMap;
    stationInfoMap[100].receiverType      = "VIRTUAL RECEIVER";
    stationInfoMap[100].antennaDesc       = "VIRTUAL ANTENNA";
    stationInfoMap[100].physicalStationId = 7;
    stationInfoMap[100].physEcefX         = 1111.1;
    stationInfoMap[100].physEcefY         = 2222.2;
    stationInfoMap[100].physEcefZ         = 3333.3;
    stationInfoMap[7].ecefX               = 4444.4;
    stationInfoMap[7].ecefY               = 5555.5;
    stationInfoMap[7].ecefZ               = 6666.6;

    auto* info = selectRtcmStationInfoForMetadata(stationInfoMap, 100);

    BOOST_REQUIRE(info != nullptr);
    BOOST_TEST(info->receiverType == "VIRTUAL RECEIVER");
    BOOST_TEST(info->antennaDesc == "VIRTUAL ANTENNA");

    ReceiverMetadata metadata;
    metadata.ingestRtcm(*info);

    expectVector3dEq(metadata.stationPosition.value, Vector3d(1111.1, 2222.2, 3333.3));
}
