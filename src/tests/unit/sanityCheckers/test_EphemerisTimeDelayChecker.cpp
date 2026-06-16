#define BOOST_TEST_MODULE EphemerisTimeDelayCheckerTests
#include <boost/test/included/unit_test.hpp>
#include "common/acsConfig.hpp"
#include "common/sanityCheckers/EphemerisTimeDelayChecker.hpp"

BOOST_AUTO_TEST_CASE(resets_ephemeris_time_delay_outside_real_time)
{
    ACSConfig config;
    config.simulate_real_time = false;

    for (E_Sys sys : magic_enum::enum_values<E_Sys>())
    {
        config.default_eph_time_delay[sys] = 12.5;
        config.eph_time_delay[sys]         = 99.0;
    }

    EphemerisTimeDelayChecker checker;

    BOOST_CHECK(checker.check(config));

    for (E_Sys sys : magic_enum::enum_values<E_Sys>())
    {
        BOOST_CHECK_EQUAL(config.eph_time_delay[sys], config.default_eph_time_delay[sys]);
    }
}

BOOST_AUTO_TEST_CASE(does_not_reset_ephemeris_time_delay_in_real_time)
{
    ACSConfig config;
    config.simulate_real_time = true;

    for (E_Sys sys : magic_enum::enum_values<E_Sys>())
    {
        config.default_eph_time_delay[sys] = 12.5;
        config.eph_time_delay[sys]         = 99.0;
    }

    EphemerisTimeDelayChecker checker;

    BOOST_CHECK(checker.check(config));

    for (E_Sys sys : magic_enum::enum_values<E_Sys>())
    {
        BOOST_CHECK_EQUAL(config.eph_time_delay[sys], 99.0);
    }
}
