#define BOOST_TEST_MODULE preprocessor_frequency_tests
#include <boost/test/unit_test.hpp>

#include "common/acsConfig.hpp"
#include "common/acsQC.hpp"
#include "common/navigation.hpp"
#include "common/observations.hpp"

ACSConfig acsConfig = {};

namespace
{
GObs makeGalObs(std::initializer_list<E_FType> freqs)
{
    GObs obs;
    obs.Sat = SatSys(E_Sys::GAL, 1);

    auto satNav = std::make_unique<SatNav>();
    for (auto freq : freqs)
    {
        Sig sig;
        sig.L          = 1;
        obs.sigs[freq] = sig;
        satNav->lamMap[freq] = 1;
    }

    obs.satNav_ptr = satNav.release();
    return obs;
}

void releaseNav(GObs& obs)
{
    delete obs.satNav_ptr;
    obs.satNav_ptr = nullptr;
}
}  // namespace

BOOST_AUTO_TEST_CASE(obs_freqs_keeps_first_three_usable_priority_frequencies)
{
    acsConfig.code_priorities.clear();
    acsConfig.code_priorities[E_Sys::GAL] = {
        E_ObsCode::L1C,
        E_ObsCode::L5Q,
        E_ObsCode::L6C,
        E_ObsCode::L7Q
    };

    auto obs = makeGalObs({F1, F5, F6, F7});

    E_FType ft1;
    E_FType ft2;
    E_FType ft3;
    int     nf = obsFreqs(obs, ft1, ft2, ft3);

    BOOST_CHECK_EQUAL(nf, 3);
    BOOST_CHECK_EQUAL(ft1, F1);
    BOOST_CHECK_EQUAL(ft2, F5);
    BOOST_CHECK_EQUAL(ft3, F6);

    releaseNav(obs);
}

BOOST_AUTO_TEST_CASE(obs_freqs_uses_l6_when_earlier_priority_frequency_is_missing)
{
    acsConfig.code_priorities.clear();
    acsConfig.code_priorities[E_Sys::GAL] = {
        E_ObsCode::L1C,
        E_ObsCode::L5Q,
        E_ObsCode::L6C,
        E_ObsCode::L7Q
    };

    auto obs = makeGalObs({F1, F6, F7});

    E_FType ft1;
    E_FType ft2;
    E_FType ft3;
    int     nf = obsFreqs(obs, ft1, ft2, ft3);

    BOOST_CHECK_EQUAL(nf, 3);
    BOOST_CHECK_EQUAL(ft1, F1);
    BOOST_CHECK_EQUAL(ft2, F6);
    BOOST_CHECK_EQUAL(ft3, F7);

    releaseNav(obs);
}

BOOST_AUTO_TEST_CASE(obs_freqs_honours_l6_before_l5_priority)
{
    acsConfig.code_priorities.clear();
    acsConfig.code_priorities[E_Sys::GAL] = {
        E_ObsCode::L1C,
        E_ObsCode::L6C,
        E_ObsCode::L5Q
    };

    auto obs = makeGalObs({F1, F5, F6});

    E_FType ft1;
    E_FType ft2;
    E_FType ft3;
    int     nf = obsFreqs(obs, ft1, ft2, ft3);

    BOOST_CHECK_EQUAL(nf, 3);
    BOOST_CHECK_EQUAL(ft1, F1);
    BOOST_CHECK_EQUAL(ft2, F6);
    BOOST_CHECK_EQUAL(ft3, F5);

    releaseNav(obs);
}
