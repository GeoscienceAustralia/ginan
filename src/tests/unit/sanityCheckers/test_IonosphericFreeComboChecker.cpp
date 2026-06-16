#define BOOST_TEST_MODULE IonosphericFreeComboCheckerTests
#include <boost/test/included/unit_test.hpp>
#include "common/acsConfig.hpp"
#include "common/sanityCheckers/IonosphericFreeComboChecker.hpp"

BOOST_AUTO_TEST_CASE(disables_higher_order_ionospheric_components_when_if_combo_is_enabled)
{
    ACSConfig config;
    config.pppOpts.ionoOpts.use_if_combo = true;
    config.recOptsMap["TEST"].ionospheric_component2 = true;
    config.recOptsMap["TEST"].ionospheric_component3 = true;

    IonosphericFreeComboChecker checker;

    BOOST_CHECK(!checker.check(config));
    BOOST_CHECK(!config.recOptsMap["TEST"].ionospheric_component2);
    BOOST_CHECK(!config.recOptsMap["TEST"].ionospheric_component3);
    BOOST_CHECK(isInited(config.recOptsMap["TEST"], config.recOptsMap["TEST"].ionospheric_component2));
    BOOST_CHECK(isInited(config.recOptsMap["TEST"], config.recOptsMap["TEST"].ionospheric_component3));
}

BOOST_AUTO_TEST_CASE(is_noop_when_if_combo_is_disabled)
{
    ACSConfig config;
    config.pppOpts.ionoOpts.use_if_combo = false;
    config.recOptsMap["TEST"].ionospheric_component2 = true;
    config.recOptsMap["TEST"].ionospheric_component3 = true;

    IonosphericFreeComboChecker checker;

    BOOST_CHECK(checker.check(config));
    BOOST_CHECK(config.recOptsMap["TEST"].ionospheric_component2);
    BOOST_CHECK(config.recOptsMap["TEST"].ionospheric_component3);
}
