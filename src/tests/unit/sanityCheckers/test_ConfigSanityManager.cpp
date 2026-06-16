#define BOOST_TEST_MODULE ConfigSanityManagerTests
#include <boost/test/included/unit_test.hpp>
#include "common/sanityCheckers/ConfigSanityManager.hpp"

BOOST_AUTO_TEST_CASE(default_manager_registers_expected_checkers)
{
    auto manager = ConfigSanityManager::defaultManager();
    auto names   = manager.checkerNames();

    BOOST_CHECK_EQUAL(manager.checkerCount(), 6);
    BOOST_CHECK_EQUAL(names[0], "EpochToleranceChecker");
    BOOST_CHECK_EQUAL(names[1], "RequiredSiteEccentricityChecker");
    BOOST_CHECK_EQUAL(names[2], "IonosphericOutageChecker");
    BOOST_CHECK_EQUAL(names[3], "EphemerisTimeDelayChecker");
    BOOST_CHECK_EQUAL(names[4], "IonosphericFreeComboChecker");
    BOOST_CHECK_EQUAL(names[5], "SbasSanityChecker");
}
