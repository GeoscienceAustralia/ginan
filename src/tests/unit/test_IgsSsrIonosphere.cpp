#define BOOST_TEST_MODULE igs_ssr_ionosphere_tests
#include <boost/test/unit_test.hpp>

#include "common/acsConfig.hpp"
#include "common/navigation.hpp"
#include "other_ssr/otherSSR.hpp"
#include <array>
#include <limits>

ACSConfig  acsConfig  = {};
Navigation nav        = {};
GTime      tsync      = {};
int        traceLevel = 0;

extern GTime               igsSSRlastTime;
extern map<SatSys, SSROut> igsSSRStorage;

namespace
{
// Independently packed from IGS SSR v1.00, Tables 24/25 and section 8.4.1.
// IM201: epoch 12345, IOD 7, provider 42, solution 3, quality 0.85 TECU.
// One layer at 450 km, N = 3, M = 2: 323 bits plus five zero padding bits.
const vector<uint8_t> im201 = {
    0xfe, 0xc3, 0x92, 0x06, 0x07, 0x20, 0x70, 0x02, 0xa3, 0x08, 0x85, 0xa4, 0x20, 0x19,
    0x1f, 0xce, 0x00, 0x4b, 0x1f, 0x9c, 0x00, 0x7d, 0x1f, 0x6a, 0x00, 0xaf, 0x1f, 0x38,
    0x00, 0xe1, 0x1f, 0x06, 0x01, 0x13, 0x1e, 0xd4, 0x01, 0x45, 0x1e, 0xa2, 0x00,
};

// The same message with a second layer: 550 km, N = M = 1, coefficients below.
const vector<uint8_t> twoLayerIm201 = {
    0xfe, 0xc3, 0x92, 0x06, 0x07, 0x20, 0x70, 0x02, 0xa3, 0x08, 0xa5, 0xa4, 0x20,
    0x19, 0x1f, 0xce, 0x00, 0x4b, 0x1f, 0x9c, 0x00, 0x7d, 0x1f, 0x6a, 0x00, 0xaf,
    0x1f, 0x38, 0x00, 0xe1, 0x1f, 0x06, 0x01, 0x13, 0x1e, 0xd4, 0x01, 0x45, 0x1e,
    0xa2, 0x06, 0xe0, 0x01, 0x77, 0x1e, 0x70, 0x01, 0xa9, 0x1e, 0x3e, 0x00,
};

const vector<uint8_t> minimumIm201 = {
    0xfe, 0xc3, 0x92, 0x06, 0x07, 0x20, 0x70, 0x02, 0xa3, 0x08, 0x86,
    0xe0, 0x01, 0x77, 0x1e, 0x70, 0x01, 0xa9, 0x1e, 0x3e, 0x00,
};

struct ExpectedCoefficient
{
    int        degree;
    int        order;
    E_TrigType type;
    double     value;
};

const std::array<ExpectedCoefficient, 14> coefficients = {{
    {0, 0, E_TrigType::COS, 1},
    {1, 0, E_TrigType::COS, -2},
    {2, 0, E_TrigType::COS, 3},
    {3, 0, E_TrigType::COS, -4},
    {1, 1, E_TrigType::COS, 5},
    {2, 1, E_TrigType::COS, -6},
    {3, 1, E_TrigType::COS, 7},
    {2, 2, E_TrigType::COS, -8},
    {3, 2, E_TrigType::COS, 9},
    {1, 1, E_TrigType::SIN, -10},
    {2, 1, E_TrigType::SIN, 11},
    {3, 1, E_TrigType::SIN, -12},
    {2, 2, E_TrigType::SIN, 13},
    {3, 2, E_TrigType::SIN, -14},
}};

struct IonosphereFixture
{
    GTime time = GTime(GWeek(2400), GTow(12345));

    IonosphereFixture()
    {
        nav.ssrAtm = {};
        igsSSRStorage.clear();
        igsSSRlastTime = {};
    }

    SSRAtm atmosphere()
    {
        SSRAtm result;
        result.ssrMeta.epochTime1s    = 12345;
        result.ssrMeta.updateIntIndex = 0;
        result.ssrMeta.provider       = 42;
        result.ssrMeta.solution       = 3;

        auto& global        = result.atmosGlobalMap[time];
        global.time         = time;
        global.numberLayers = 1;
        global.vtecQuality  = 0.85;
        global.iod          = 7;

        auto& layer     = global.layers[0];
        layer.height    = 450;
        layer.maxDegree = 4;
        layer.maxOrder  = 3;

        int index = 0;
        for (const auto& expected : coefficients)
        {
            layer.sphHarmonic[index++] = {
                0, expected.order, expected.degree, expected.type, expected.value, 0};
        }
        return result;
    }
};
}  // namespace

BOOST_FIXTURE_TEST_SUITE(igs_ssr_ionosphere, IonosphereFixture)

BOOST_AUTO_TEST_CASE(decodes_independent_im201_coefficients)
{
    auto data = im201;
    decodeigsSSR(data, time);

    BOOST_REQUIRE_EQUAL(nav.ssrAtm.atmosGlobalMap.size(), 1);
    const auto& global = nav.ssrAtm.atmosGlobalMap.at(time);
    BOOST_CHECK_EQUAL(global.iod, 7);
    BOOST_CHECK_SMALL(global.vtecQuality - 0.85, 1e-12);
    BOOST_CHECK_EQUAL(global.numberLayers, 1);
    BOOST_REQUIRE_EQUAL(global.layers.size(), 1);

    const auto& layer = global.layers.at(0);
    BOOST_CHECK_EQUAL(layer.height, 450);
    BOOST_CHECK_EQUAL(layer.maxDegree, 4);
    BOOST_CHECK_EQUAL(layer.maxOrder, 3);
    BOOST_REQUIRE(!layer.sphHarmonic.empty());
    BOOST_CHECK(layer.sphHarmonic.begin()->second.trigType == E_TrigType::COS);
    BOOST_REQUIRE_EQUAL(layer.sphHarmonic.size(), coefficients.size());

    for (size_t index = 0; index < coefficients.size(); index++)
    {
        const auto& actual   = layer.sphHarmonic.at(index);
        const auto& expected = coefficients[index];
        BOOST_TEST_CONTEXT("coefficient " << index)
        {
            BOOST_CHECK_EQUAL(actual.layer, 0);
            BOOST_CHECK_EQUAL(actual.degree, expected.degree);
            BOOST_CHECK_EQUAL(actual.order, expected.order);
            BOOST_CHECK(actual.trigType == expected.type);
            BOOST_CHECK_SMALL(actual.value - expected.value, 1e-12);
        }
    }
}

BOOST_AUTO_TEST_CASE(encodes_independent_im201_bytes)
{
    auto input         = atmosphere();
    nav.ssrAtm         = input;
    const auto encoded = encodeIGS_ATM(input, true);
    BOOST_CHECK_EQUAL_COLLECTIONS(encoded.begin(), encoded.end(), im201.begin(), im201.end());
}

BOOST_AUTO_TEST_CASE(encoder_uses_supplied_atmosphere)
{
    auto input = atmosphere();
    // The encoder must use its argument even when the navigation map is empty.
    const auto encoded = encodeIGS_ATM(input, true);
    BOOST_CHECK_EQUAL_COLLECTIONS(encoded.begin(), encoded.end(), im201.begin(), im201.end());
}

BOOST_AUTO_TEST_CASE(decodes_and_encodes_minimum_and_multiple_layers)
{
    for (const bool twoLayers : {false, true})
    {
        auto expected = twoLayers ? twoLayerIm201 : minimumIm201;
        nav.ssrAtm.atmosGlobalMap.clear();
        decodeigsSSR(expected, time);
        BOOST_REQUIRE_EQUAL(nav.ssrAtm.atmosGlobalMap.size(), 1);
        const auto& global = nav.ssrAtm.atmosGlobalMap.at(time);
        BOOST_REQUIRE_EQUAL(global.layers.size(), twoLayers ? 2 : 1);
        const auto& layer = global.layers.at(twoLayers ? 1 : 0);
        BOOST_CHECK_EQUAL(layer.height, 550);
        BOOST_CHECK_EQUAL(layer.maxDegree, 2);
        BOOST_CHECK_EQUAL(layer.maxOrder, 2);
        BOOST_REQUIRE_EQUAL(layer.sphHarmonic.size(), 4);
        const std::array<ExpectedCoefficient, 4> minimumCoefficients = {{
            {0, 0, E_TrigType::COS, 15},
            {1, 0, E_TrigType::COS, -16},
            {1, 1, E_TrigType::COS, 17},
            {1, 1, E_TrigType::SIN, -18},
        }};

        auto  input       = atmosphere();
        auto& inputGlobal = input.atmosGlobalMap.at(time);
        if (!twoLayers)
            inputGlobal.layers.clear();
        inputGlobal.numberLayers = twoLayers ? 2 : 1;
        auto& inputLayer         = inputGlobal.layers[twoLayers ? 1 : 0];
        inputLayer.height        = 550;
        inputLayer.maxDegree     = 2;
        inputLayer.maxOrder      = 2;
        for (size_t index = 0; index < minimumCoefficients.size(); index++)
        {
            const auto& coefficient = minimumCoefficients[index];
            const auto& actual      = layer.sphHarmonic.at(index);
            BOOST_CHECK_EQUAL(actual.layer, twoLayers ? 1 : 0);
            BOOST_CHECK_EQUAL(actual.degree, coefficient.degree);
            BOOST_CHECK_EQUAL(actual.order, coefficient.order);
            BOOST_CHECK(actual.trigType == coefficient.type);
            BOOST_CHECK_SMALL(actual.value - coefficient.value, 1e-12);
            inputLayer.sphHarmonic[index] = {twoLayers ? 1 : 0,  coefficient.order,
                                             coefficient.degree, coefficient.type,
                                             coefficient.value,  0};
        }
        const auto encoded = encodeIGS_ATM(input, true);
        BOOST_CHECK_EQUAL_COLLECTIONS(encoded.begin(), encoded.end(), expected.begin(),
                                      expected.end());
    }
}

BOOST_AUTO_TEST_CASE(rejects_truncated_header_layer_and_coefficients)
{
    for (size_t size = 0; size < im201.size(); size++)
    {
        vector<uint8_t> truncated(im201.begin(), im201.begin() + size);
        decodeigsSSR(truncated, time);
        BOOST_TEST_CONTEXT("payload bytes " << size)
        {
            BOOST_CHECK(nav.ssrAtm.atmosGlobalMap.empty());
        }
    }
}

BOOST_AUTO_TEST_CASE(truncated_future_message_does_not_advance_decoder_time)
{
    vector<uint8_t> truncated(im201.begin(), im201.end() - 1);
    decodeigsSSR(truncated, time - 1.0);
    BOOST_CHECK(igsSSRlastTime == GTime::noTime());
    BOOST_CHECK(nav.ssrAtm.atmosGlobalMap.empty());

    auto complete = im201;
    BOOST_CHECK(decodeigsSSR(complete, time - 1.0) == E_ReturnType::WAIT);
    BOOST_CHECK(igsSSRlastTime == time);
    BOOST_CHECK(nav.ssrAtm.atmosGlobalMap.empty());
    BOOST_CHECK(decodeigsSSR(complete, time) == E_ReturnType::OK);
    BOOST_CHECK_EQUAL(nav.ssrAtm.atmosGlobalMap.size(), 1);
}

BOOST_AUTO_TEST_CASE(rejects_unrepresentable_degree_and_order)
{
    for (const auto [degree, order] :
         std::array<std::pair<int, int>, 5>{{{1, 2}, {18, 2}, {4, 1}, {4, 5}, {17, 18}}})
    {
        auto  input     = atmosphere();
        auto& layer     = input.atmosGlobalMap.begin()->second.layers.at(0);
        layer.maxDegree = degree;
        layer.maxOrder  = order;
        BOOST_TEST_CONTEXT("exclusive degree " << degree << ", order " << order)
        {
            BOOST_CHECK(encodeIGS_ATM(input, true).empty());
        }
    }
}

BOOST_AUTO_TEST_CASE(rejects_unrepresentable_layer_height_and_payload)
{
    for (const double height : {-10.0, 2560.0, std::numeric_limits<double>::quiet_NaN()})
    {
        auto input                                               = atmosphere();
        input.atmosGlobalMap.begin()->second.layers.at(0).height = height;
        BOOST_CHECK(encodeIGS_ATM(input, true).empty());
    }

    auto  input                = atmosphere();
    auto& global               = input.atmosGlobalMap.begin()->second;
    global.layers.at(0).height = 2550;
    BOOST_CHECK_EQUAL(encodeIGS_ATM(input, true).size(), im201.size());

    global.layers.at(0).maxDegree = 17;
    global.layers.at(0).maxOrder  = 17;
    BOOST_CHECK_EQUAL(encodeIGS_ATM(input, true).size(), 591);
    global.layers[1]    = global.layers.at(0);
    global.numberLayers = 2;
    BOOST_CHECK(encodeIGS_ATM(input, true).empty());
}

BOOST_AUTO_TEST_SUITE_END()
