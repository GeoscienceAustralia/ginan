#define BOOST_TEST_MODULE EpochToleranceCheckerTests
#include <boost/test/included/unit_test.hpp>
#include "common/acsConfig.hpp"
#include "common/sanityCheckers/EpochToleranceChecker.hpp"

BOOST_AUTO_TEST_CASE(limits_epoch_tolerance_to_half_epoch_interval)
{
    ACSConfig config;
    config.epoch_interval  = 30;
    config.epoch_tolerance = 20;

    EpochToleranceChecker checker;

    BOOST_CHECK(!checker.check(config));
    BOOST_CHECK_EQUAL(config.epoch_tolerance, 15);
}

BOOST_AUTO_TEST_CASE(passes_when_epoch_tolerance_is_within_limit)
{
    ACSConfig config;
    config.epoch_interval  = 30;
    config.epoch_tolerance = 10;

    EpochToleranceChecker checker;

    BOOST_CHECK(checker.check(config));
    BOOST_CHECK_EQUAL(config.epoch_tolerance, 10);
}
