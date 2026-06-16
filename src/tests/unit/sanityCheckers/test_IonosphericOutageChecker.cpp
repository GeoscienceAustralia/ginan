#define BOOST_TEST_MODULE IonosphericOutageCheckerTests
#include <boost/test/included/unit_test.hpp>
#include "common/acsConfig.hpp"
#include "common/sanityCheckers/IonosphericOutageChecker.hpp"

BOOST_AUTO_TEST_CASE(warns_when_reset_limit_is_less_than_epoch_interval)
{
    ACSConfig config;
    config.epoch_interval               = 30;
    config.ionErrors.outage_reset_limit = 10;

    IonosphericOutageChecker checker;

    BOOST_CHECK(!checker.check(config));
}

BOOST_AUTO_TEST_CASE(passes_when_reset_limit_is_at_least_epoch_interval)
{
    ACSConfig config;
    config.epoch_interval               = 30;
    config.ionErrors.outage_reset_limit = 30;

    IonosphericOutageChecker checker;

    BOOST_CHECK(checker.check(config));
}
