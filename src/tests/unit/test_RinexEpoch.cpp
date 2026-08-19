#define BOOST_TEST_MODULE rinex_epoch_tests
#include <boost/test/unit_test.hpp>

#include "common/acsConfig.hpp"
#include "common/gTime.hpp"
#include "common/navigation.hpp"

ACSConfig  acsConfig = {};
Navigation nav       = {};
GTime      tsync     = {};

BOOST_AUTO_TEST_CASE(rinex3_epoch_time_uses_columns_3_through_29)
{
    const char epoch[] = "> 2024 01 02 03 04  5.1234567  0 12";

    GTime time;
    BOOST_REQUIRE_EQUAL(str2time(epoch, 2, 27, time, E_TimeSys::GPST), 0);

    const GEpoch parsed = time;
    BOOST_CHECK_EQUAL(parsed.year, 2024);
    BOOST_CHECK_EQUAL(parsed.month, 1);
    BOOST_CHECK_EQUAL(parsed.day, 2);
    BOOST_CHECK_EQUAL(parsed.hour, 3);
    BOOST_CHECK_EQUAL(parsed.min, 4);
    BOOST_CHECK_SMALL(parsed.sec - 5.1234567, 1e-7);
}
