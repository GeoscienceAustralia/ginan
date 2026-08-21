#define BOOST_TEST_MODULE PhaseClockOsbTests
#include <boost/test/unit_test.hpp>
#include "common/phaseClockOsb.hpp"

BOOST_AUTO_TEST_CASE(ionosphere_free_coefficients_preserve_clock_and_cancel_ionosphere)
{
    constexpr double gpsL1 = 0.190293672798365;
    constexpr double gpsL2 = 0.244210213424568;

    auto coefficients = phaseClockOsbCoefficients(gpsL1, gpsL2);
    BOOST_REQUIRE(coefficients);

    BOOST_CHECK_CLOSE(coefficients->lambda1, gpsL1, 1e-12);
    BOOST_CHECK_CLOSE(coefficients->lambda2, gpsL2, 1e-12);
    BOOST_CHECK_SMALL(coefficients->alpha - coefficients->beta - 1, 1e-12);

    double gamma = (gpsL2 * gpsL2) / (gpsL1 * gpsL1);
    BOOST_CHECK_SMALL(coefficients->alpha - coefficients->beta * gamma, 1e-12);
}

BOOST_AUTO_TEST_CASE(wide_narrow_round_trip_recovers_single_frequency_phase_osbs)
{
    constexpr double gpsL1 = 0.190293672798365;
    constexpr double gpsL2 = 0.244210213424568;
    constexpr double phase1 = 0.031;
    constexpr double phase2 = -0.017;

    auto coefficients = phaseClockOsbCoefficients(gpsL1, gpsL2);
    BOOST_REQUIRE(coefficients);

    double g = coefficients->frequencyRatio;
    double wide = g / (g - 1) * phase1 - 1 / (g - 1) * phase2;
    double narrow = coefficients->alpha * phase1 - coefficients->beta * phase2;

    double reconstructed1 = (g + 1) / g * narrow - wide / g;
    double reconstructed2 = (g + 1) * narrow - g * wide;

    BOOST_CHECK_SMALL(reconstructed1 - phase1, 1e-12);
    BOOST_CHECK_SMALL(reconstructed2 - phase2, 1e-12);
}

BOOST_AUTO_TEST_CASE(fractional_cycle_uses_symmetric_nearest_integer_interval)
{
    BOOST_CHECK_CLOSE(phaseClockOsbFractionalCycle(10.2), 0.2, 1e-12);
    BOOST_CHECK_CLOSE(phaseClockOsbFractionalCycle(-10.2), -0.2, 1e-12);
    BOOST_CHECK_CLOSE(phaseClockOsbFractionalCycle(10.8), -0.2, 1e-12);
}

BOOST_AUTO_TEST_CASE(invalid_or_equal_wavelengths_are_rejected)
{
    BOOST_CHECK(!phaseClockOsbCoefficients(0, 0.244));
    BOOST_CHECK(!phaseClockOsbCoefficients(0.190, 0.190));
    BOOST_CHECK(!phaseClockOsbCoefficients(std::nan(""), 0.244));
}
