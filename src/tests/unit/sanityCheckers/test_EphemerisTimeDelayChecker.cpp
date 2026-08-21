#define BOOST_TEST_MODULE EphemerisTimeDelayCheckerTests
#include <boost/test/included/unit_test.hpp>
#include "common/acsConfig.hpp"
#include "common/sanityCheckers/EphemerisTimeDelayChecker.hpp"

BOOST_AUTO_TEST_CASE(returns_false_to_warn_when_eph_time_delay_is_below_30_for_any_uploading_stream)
{
    ACSConfig config;
    config.netOpts.uploadingStreamData["TEST"].rtcmMsgOptsMap[RtcmMessageType::GPS_SSR_COMB_CORR] =
        RtcmMsgTypeOpts();
    config.netOpts.uploadingStreamData["TEST"].rtcmMsgOptsMap[RtcmMessageType::GLO_SSR_COMB_CORR] =
        RtcmMsgTypeOpts();
    config.netOpts.uploadingStreamData["TEST"].rtcmMsgOptsMap[RtcmMessageType::GAL_SSR_COMB_CORR] =
        RtcmMsgTypeOpts();
    config.process_sys[E_Sys::GPS] = true;
    config.process_sys[E_Sys::GLO] = true;
    config.process_sys[E_Sys::GAL] = true;

    for (E_Sys sys : magic_enum::enum_values<E_Sys>())
    {
        config.eph_time_delay[sys] = 20.0;
    }

    EphemerisTimeDelayChecker checker;

    BOOST_CHECK(!checker.check(config));
}

BOOST_AUTO_TEST_CASE(passes_when_eph_time_delay_is_at_least_30_for_all_uploading_streams)
{
    ACSConfig config;
    config.netOpts.uploadingStreamData["TEST"].rtcmMsgOptsMap[RtcmMessageType::GPS_SSR_COMB_CORR] =
        RtcmMsgTypeOpts();
    config.netOpts.uploadingStreamData["TEST"].rtcmMsgOptsMap[RtcmMessageType::GLO_SSR_COMB_CORR] =
        RtcmMsgTypeOpts();
    config.netOpts.uploadingStreamData["TEST"].rtcmMsgOptsMap[RtcmMessageType::GAL_SSR_COMB_CORR] =
        RtcmMsgTypeOpts();
    config.process_sys[E_Sys::GPS] = true;
    config.process_sys[E_Sys::GLO] = true;
    config.process_sys[E_Sys::GAL] = true;

    for (E_Sys sys : magic_enum::enum_values<E_Sys>())
    {
        config.eph_time_delay[sys] = 30.0;
    }

    EphemerisTimeDelayChecker checker;

    BOOST_CHECK(checker.check(config));
}

BOOST_AUTO_TEST_CASE(is_noop_when_there_is_no_uploading_stream)
{
    ACSConfig config;
    config.netOpts.uploadingStreamData.clear();

    for (E_Sys sys : magic_enum::enum_values<E_Sys>())
    {
        config.eph_time_delay[sys] = 10.0;
    }

    EphemerisTimeDelayChecker checker;

    BOOST_CHECK(checker.check(config));
}
