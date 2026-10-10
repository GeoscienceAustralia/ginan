#define BOOST_TEST_MODULE iono_mapping_function_tests
#include <boost/test/unit_test.hpp>

#include "common/constants.hpp"
#include "common/ionModels.hpp"
#include "common/satStat.hpp"

namespace
{
AzEl elevation(double elevationDeg)
{
    AzEl azel;
    azel.az = 0;
    azel.el = elevationDeg * D2R;
    return azel;
}
}  // namespace

/** The single layer mapping function and the slant factor returned by ionppp() describe the same
 * thin-shell geometry, so for a receiver on the sphere they must agree. ionppp() is called with
 * the Earth radius and the layer height both in km, as every caller in pea does.
 */
BOOST_AUTO_TEST_CASE(slm_mapping_matches_pierce_point_slant_factor)
{
    VectorPos pos;

    for (double hionKm : {350.0, 450.0, 506.7})
        for (double elevationDeg : {5.0, 10.0, 20.0, 30.0, 60.0, 90.0})
        {
            AzEl      azel = elevation(elevationDeg);
            VectorPos posp;

            double expected = ionppp(pos, azel, RE_MEAN / 1000, hionKm, posp);
            double mapping  = ionmapf(pos, azel, E_IonoMapFn::SLM, hionKm);

            BOOST_TEST_CONTEXT("layer " << hionKm << " km, elevation " << elevationDeg << " deg")
            {
                BOOST_CHECK_CLOSE(mapping, expected, 1e-9);
            }
        }
}

/** Modified single layer model (Schaer 1999; alpha 0.9782, layer 506.7 km, R 6371 km):
 * F = 1 / sqrt(1 - (R / (R + H) * sin(alpha * z))^2), which is 2.3738 at 10 deg elevation.
 */
BOOST_AUTO_TEST_CASE(mslm_mapping_at_10_degrees_matches_closed_form)
{
    VectorPos pos;

    double mapping = ionmapf(pos, elevation(10), E_IonoMapFn::MSLM, 506.7);

    BOOST_CHECK_CLOSE(mapping, 2.3737847977, 1e-6);
}

/** A higher layer is pierced at a steeper angle, so the factor must fall as the layer rises:
 * 2.79 at 350 km against 2.44 at 506.7 km for 10 deg elevation.
 */
BOOST_AUTO_TEST_CASE(mapping_factor_falls_as_the_layer_rises)
{
    VectorPos pos;

    double low  = ionmapf(pos, elevation(10), E_IonoMapFn::SLM, 350);
    double high = ionmapf(pos, elevation(10), E_IonoMapFn::SLM, 506.7);

    BOOST_CHECK_GT(low - high, 0.3);
}

BOOST_AUTO_TEST_CASE(mapping_factor_is_one_at_zenith)
{
    VectorPos pos;

    for (auto mapFn : {E_IonoMapFn::SLM, E_IonoMapFn::MSLM})
    {
        BOOST_CHECK_CLOSE(ionmapf(pos, elevation(90), mapFn, 506.7), 1.0, 1e-9);
    }
}
