#define BOOST_TEST_MODULE SbasSanityCheckerTests
#include <boost/test/included/unit_test.hpp>
#include "common/acsConfig.hpp"
#include "common/sanityCheckers/SbasSanityChecker.hpp"

BOOST_AUTO_TEST_CASE(disables_l1_incompatible_input_flags_even_when_sbas_processing_is_off)
{
    ACSConfig config;
    config.process_sbas          = false;
    config.sbsInOpts.freq        = 1;
    config.sbsInOpts.use_do259   = true;
    config.sbsInOpts.pvs_on_dfmc = true;

    SbasSanityChecker checker;

    BOOST_CHECK(!checker.check(config));
    BOOST_CHECK(!config.sbsInOpts.use_do259);
    BOOST_CHECK(!config.sbsInOpts.pvs_on_dfmc);
}

BOOST_AUTO_TEST_CASE(configures_l1_sbas_processing)
{
    ACSConfig config;
    config.process_sbas                = true;
    config.process_preprocessor        = false;
    config.process_spp                 = false;
    config.sbsOpts.mode                = E_SbasMode::L1;
    config.sppOpts.smooth_window       = 30;
    config.sppOpts.use_smooth_only     = false;
    config.sbsOpts.use_sbas_rec_var    = false;
    config.process_sys[E_Sys::GPS]     = true;
    config.process_sys[E_Sys::GAL]     = true;
    config.process_sys[E_Sys::SBS]     = true;
    config.satOptsMap["G01"].posModel.enable = false;

    SbasSanityChecker checker;

    BOOST_CHECK(!checker.check(config));
    BOOST_CHECK(config.process_preprocessor);
    BOOST_CHECK(config.process_spp);
    BOOST_CHECK_EQUAL(config.sbsInOpts.freq, 1);
    BOOST_CHECK_EQUAL(config.sppOpts.smooth_window, 100);
    BOOST_CHECK(config.sppOpts.use_smooth_only);
    BOOST_CHECK(config.sbsOpts.use_sbas_rec_var);
    BOOST_CHECK(config.process_sys[E_Sys::GPS]);
    BOOST_CHECK(!config.process_sys[E_Sys::GAL]);
    BOOST_CHECK(config.process_sys[E_Sys::SBS]);
    BOOST_CHECK_EQUAL(config.code_priorities[E_Sys::GPS].front(), E_ObsCode::L1C);
    BOOST_CHECK(config.satOptsMap["G01"].posModel.enable);
    BOOST_CHECK_EQUAL(config.satOptsMap["G01"].posModel.sources.front(), E_Source::SBAS);
}

BOOST_AUTO_TEST_CASE(configures_dfmc_sbas_processing)
{
    ACSConfig config;
    config.process_sbas            = true;
    config.sbsOpts.mode            = E_SbasMode::DFMC;
    config.sbsInOpts.freq          = 5;
    config.sbsInOpts.pvs_on_dfmc   = true;
    config.process_sys[E_Sys::GPS] = true;
    config.process_sys[E_Sys::GLO] = true;
    config.process_sys[E_Sys::BDS] = true;

    SbasSanityChecker checker;

    BOOST_CHECK(checker.check(config));
    BOOST_CHECK_EQUAL(config.sbsInOpts.freq, 5);
    BOOST_CHECK(!config.sbsInOpts.pvs_on_dfmc);
    BOOST_CHECK(config.process_sys[E_Sys::GPS]);
    BOOST_CHECK(!config.process_sys[E_Sys::GLO]);
    BOOST_CHECK(config.process_sys[E_Sys::BDS]);
    BOOST_CHECK_EQUAL(config.sppOpts.iono_mode, E_IonoMode::SBAS);
    BOOST_CHECK_EQUAL(config.sppOpts.trop_models.front(), E_TropModel::SBAS);
}

BOOST_AUTO_TEST_CASE(configures_pvs_processing)
{
    ACSConfig config;
    config.process_sbas            = true;
    config.process_ppp             = false;
    config.sbsOpts.mode            = E_SbasMode::PVS;
    config.process_sys[E_Sys::GPS] = false;
    config.process_sys[E_Sys::GAL] = false;
    config.process_sys[E_Sys::GLO] = true;
    config.recOptsMap["TEST"].tideModels.solid = true;

    SbasSanityChecker checker;

    BOOST_CHECK(checker.check(config));
    BOOST_CHECK(config.process_ppp);
    BOOST_CHECK_EQUAL(config.sbsInOpts.freq, 5);
    BOOST_CHECK(config.sbsInOpts.pvs_on_dfmc);
    BOOST_CHECK(config.process_sys[E_Sys::GPS]);
    BOOST_CHECK(config.process_sys[E_Sys::GAL]);
    BOOST_CHECK(!config.process_sys[E_Sys::GLO]);
    BOOST_CHECK_EQUAL(config.recOptsMap["TEST"].receiver_reference_system, E_Sys::GPS);
    BOOST_CHECK(config.recOptsMap["TEST"].tropModel.enable);
    BOOST_CHECK_EQUAL(config.recOptsMap["TEST"].tropModel.models.front(), E_TropModel::STANDARD);
    BOOST_CHECK(config.recOptsMap["TEST"].tideModels.otl);
    BOOST_CHECK(!config.recOptsMap["TEST"].tideModels.atl);
    BOOST_CHECK(!config.recOptsMap["TEST"].tideModels.spole);
    BOOST_CHECK(!config.recOptsMap["TEST"].tideModels.opole);
    BOOST_CHECK(config.sppOpts.always_reinitialise);
    BOOST_CHECK(config.pppOpts.use_primary_signals);
    BOOST_CHECK(config.errorAccumulation.enable);
    BOOST_CHECK_EQUAL(config.ambErrors.phase_reject_limit, 2);
    BOOST_CHECK(config.ambErrors.resetOnSlip.LLI);
    BOOST_CHECK(config.ambErrors.resetOnSlip.retrack);
}
