#define BOOST_TEST_MODULE RequiredSiteEccentricityCheckerTests
#include <boost/test/included/unit_test.hpp>
#include "common/acsConfig.hpp"
#include "common/sanityCheckers/RequiredSiteEccentricityChecker.hpp"

BOOST_AUTO_TEST_CASE(enables_receiver_eccentricity_model_when_required)
{
    ACSConfig config;
    config.require_site_eccentricity = true;
    config.recOptsMap["TEST"].eccentricityModel.enable = false;

    RequiredSiteEccentricityChecker checker;

    BOOST_CHECK(!checker.check(config));
    BOOST_CHECK(config.recOptsMap["TEST"].eccentricityModel.enable);
    BOOST_CHECK(isInited(config.recOptsMap["TEST"], config.recOptsMap["TEST"].eccentricityModel.enable));
}

BOOST_AUTO_TEST_CASE(is_noop_when_site_eccentricity_is_not_required)
{
    ACSConfig config;
    config.require_site_eccentricity = false;
    config.recOptsMap["TEST"].eccentricityModel.enable = false;

    RequiredSiteEccentricityChecker checker;

    BOOST_CHECK(checker.check(config));
    BOOST_CHECK(!config.recOptsMap["TEST"].eccentricityModel.enable);
}
