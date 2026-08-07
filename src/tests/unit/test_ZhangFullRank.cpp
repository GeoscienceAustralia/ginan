#define BOOST_TEST_MODULE ZhangFullRankTests
#include <boost/test/unit_test.hpp>
#include <numeric>
#include <random>
#include "common/eigenIncluder.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "common/zhangPhaseContinuity.hpp"
#include "common/zhangSatelliteDatum.hpp"
#include "common/zhangPersistentProductDatum.hpp"
#include "common/zhangFullRank.hpp"
#include "common/zhangFixedLagSquareRoot.hpp"
#include "common/zhangIncrementalFixedLag.hpp"
#include "common/zhangIncrementalRawSquareRoot.hpp"
#include "common/zhangPersistentRawTargetWindow.hpp"
#include "common/zhangIntegerTargets.hpp"
#include "common/zhangResidualStatistics.hpp"
#include "common/zhangFactorCapture.hpp"
#include "common/zhangRawFactorWindow.hpp"
#include "common/zhangUserTarget.hpp"

namespace
{
struct ZhangFormalDesign
{
    MatrixXd raw;
    MatrixXd full;
    MatrixXd nullSpace;
    MatrixXd rawToFull;
};

ZhangFormalDesign buildZhangFormalDesign()
{
    constexpr int receiverCount = 3;
    constexpr int satelliteCount = 4;
    constexpr int frequencyCount = 2;

    constexpr int rowCount = 2 * frequencyCount * receiverCount * satelliteCount;

    constexpr int rawTroposphereOffset = 0;
    constexpr int rawReceiverClockOffset = rawTroposphereOffset + receiverCount;
    constexpr int rawSatelliteClockOffset = rawReceiverClockOffset + receiverCount;
    constexpr int rawIonosphereOffset = rawSatelliteClockOffset + satelliteCount;
    constexpr int rawReceiverIfOffset =
        rawIonosphereOffset + receiverCount * satelliteCount;
    constexpr int rawSatelliteIfOffset = rawReceiverIfOffset + receiverCount;
    constexpr int rawReceiverGfOffset = rawSatelliteIfOffset + satelliteCount;
    constexpr int rawSatelliteGfOffset = rawReceiverGfOffset + receiverCount;
    constexpr int rawReceiverPhaseOffset = rawSatelliteGfOffset + satelliteCount;
    constexpr int rawSatellitePhaseOffset =
        rawReceiverPhaseOffset + frequencyCount * receiverCount;
    constexpr int rawAmbiguityOffset =
        rawSatellitePhaseOffset + frequencyCount * satelliteCount;
    constexpr int rawColumnCount =
        rawAmbiguityOffset + frequencyCount * receiverCount * satelliteCount;

    constexpr int fullTroposphereOffset = 0;
    constexpr int fullReceiverClockOffset = fullTroposphereOffset + receiverCount;
    constexpr int fullSatelliteClockOffset =
        fullReceiverClockOffset + receiverCount - 1;
    constexpr int fullIonosphereOffset = fullSatelliteClockOffset + satelliteCount;
    constexpr int fullReceiverPhaseOffset =
        fullIonosphereOffset + receiverCount * satelliteCount;
    constexpr int fullSatellitePhaseOffset =
        fullReceiverPhaseOffset + frequencyCount * (receiverCount - 1);
    constexpr int fullAmbiguityOffset =
        fullSatellitePhaseOffset + frequencyCount * satelliteCount;
    constexpr int fullColumnCount =
        fullAmbiguityOffset +
        frequencyCount * (receiverCount - 1) * (satelliteCount - 1);

    constexpr int nullity =
        2 * receiverCount + 2 * satelliteCount + 1 +
        frequencyCount * (receiverCount + satelliteCount);

    const double mu[frequencyCount] = {1.0, 1.6469444444444445};
    const double lambda[frequencyCount] = {0.190293672798365, 0.244210213424568};
    const double tropMapping[receiverCount][satelliteCount] = {
        {1.10, 1.43, 2.04, 3.12},
        {1.22, 1.67, 2.31, 2.86},
        {1.35, 1.82, 2.18, 3.57}
    };

    auto rawIonosphere = [&](int receiver, int satellite)
    {
        return rawIonosphereOffset + receiver * satelliteCount + satellite;
    };
    auto rawReceiverPhase = [&](int frequency, int receiver)
    {
        return rawReceiverPhaseOffset + frequency * receiverCount + receiver;
    };
    auto rawSatellitePhase = [&](int frequency, int satellite)
    {
        return rawSatellitePhaseOffset + frequency * satelliteCount + satellite;
    };
    auto rawAmbiguity = [&](int frequency, int receiver, int satellite)
    {
        return rawAmbiguityOffset +
               frequency * receiverCount * satelliteCount +
               receiver * satelliteCount + satellite;
    };
    auto fullReceiverClock = [&](int receiver)
    {
        return receiver == 0 ? -1 : fullReceiverClockOffset + receiver - 1;
    };
    auto fullIonosphere = [&](int receiver, int satellite)
    {
        return fullIonosphereOffset + receiver * satelliteCount + satellite;
    };
    auto fullReceiverPhase = [&](int frequency, int receiver)
    {
        return receiver == 0
                   ? -1
                   : fullReceiverPhaseOffset +
                         frequency * (receiverCount - 1) + receiver - 1;
    };
    auto fullSatellitePhase = [&](int frequency, int satellite)
    {
        return fullSatellitePhaseOffset + frequency * satelliteCount + satellite;
    };
    auto fullAmbiguity = [&](int frequency, int receiver, int satellite)
    {
        if (receiver == 0 || satellite == 0)
        {
            return -1;
        }

        return fullAmbiguityOffset +
               frequency * (receiverCount - 1) * (satelliteCount - 1) +
               (receiver - 1) * (satelliteCount - 1) + satellite - 1;
    };

    ZhangFormalDesign result;
    result.raw = MatrixXd::Zero(rowCount, rawColumnCount);
    result.full = MatrixXd::Zero(rowCount, fullColumnCount);
    result.nullSpace = MatrixXd::Zero(rawColumnCount, nullity);
    result.rawToFull = MatrixXd::Zero(fullColumnCount, rawColumnCount);

    int row = 0;
    for (int frequency = 0; frequency < frequencyCount; frequency++)
    {
        for (int receiver = 0; receiver < receiverCount; receiver++)
        {
            for (int satellite = 0; satellite < satelliteCount; satellite++)
            {
                // Raw code equation.
                result.raw(row, rawTroposphereOffset + receiver) =
                    tropMapping[receiver][satellite];
                result.raw(row, rawReceiverClockOffset + receiver) = +1;
                result.raw(row, rawSatelliteClockOffset + satellite) = -1;
                result.raw(row, rawIonosphere(receiver, satellite)) = +mu[frequency];
                result.raw(row, rawReceiverIfOffset + receiver) = +1;
                result.raw(row, rawSatelliteIfOffset + satellite) = -1;
                result.raw(row, rawReceiverGfOffset + receiver) = +mu[frequency];
                result.raw(row, rawSatelliteGfOffset + satellite) = -mu[frequency];

                // Full-rank code equation.
                result.full(row, fullTroposphereOffset + receiver) =
                    tropMapping[receiver][satellite];
                if (int column = fullReceiverClock(receiver); column >= 0)
                {
                    result.full(row, column) = +1;
                }
                result.full(row, fullSatelliteClockOffset + satellite) = -1;
                result.full(row, fullIonosphere(receiver, satellite)) = +mu[frequency];
                row++;

                // Raw phase equation in Ginan's satellite-phase sign convention.
                result.raw(row, rawTroposphereOffset + receiver) =
                    tropMapping[receiver][satellite];
                result.raw(row, rawReceiverClockOffset + receiver) = +1;
                result.raw(row, rawSatelliteClockOffset + satellite) = -1;
                result.raw(row, rawIonosphere(receiver, satellite)) = -mu[frequency];
                result.raw(row, rawReceiverPhase(frequency, receiver)) = +1;
                result.raw(row, rawSatellitePhase(frequency, satellite)) = +1;
                result.raw(row, rawAmbiguity(frequency, receiver, satellite)) =
                    lambda[frequency];

                // Full-rank phase equation.
                result.full(row, fullTroposphereOffset + receiver) =
                    tropMapping[receiver][satellite];
                if (int column = fullReceiverClock(receiver); column >= 0)
                {
                    result.full(row, column) = +1;
                }
                result.full(row, fullSatelliteClockOffset + satellite) = -1;
                result.full(row, fullIonosphere(receiver, satellite)) = -mu[frequency];
                if (int column = fullReceiverPhase(frequency, receiver); column >= 0)
                {
                    result.full(row, column) = +1;
                }
                result.full(row, fullSatellitePhase(frequency, satellite)) = +1;
                if (int column = fullAmbiguity(frequency, receiver, satellite);
                    column >= 0)
                {
                    result.full(row, column) = lambda[frequency];
                }
                row++;
            }
        }
    }

    int nullColumn = 0;

    // Receiver IF code-bias directions.
    for (int receiver = 0; receiver < receiverCount; receiver++)
    {
        result.nullSpace(rawReceiverClockOffset + receiver, nullColumn) = -1;
        result.nullSpace(rawReceiverIfOffset + receiver, nullColumn) = +1;
        for (int frequency = 0; frequency < frequencyCount; frequency++)
        {
            result.nullSpace(rawReceiverPhase(frequency, receiver), nullColumn) = +1;
        }
        nullColumn++;
    }

    // Satellite IF code-bias directions.
    for (int satellite = 0; satellite < satelliteCount; satellite++)
    {
        result.nullSpace(rawSatelliteClockOffset + satellite, nullColumn) = -1;
        result.nullSpace(rawSatelliteIfOffset + satellite, nullColumn) = +1;
        for (int frequency = 0; frequency < frequencyCount; frequency++)
        {
            result.nullSpace(rawSatellitePhase(frequency, satellite), nullColumn) = -1;
        }
        nullColumn++;
    }

    // Receiver GF code-bias directions.
    for (int receiver = 0; receiver < receiverCount; receiver++)
    {
        result.nullSpace(rawReceiverGfOffset + receiver, nullColumn) = +1;
        for (int satellite = 0; satellite < satelliteCount; satellite++)
        {
            result.nullSpace(rawIonosphere(receiver, satellite), nullColumn) = -1;
        }
        for (int frequency = 0; frequency < frequencyCount; frequency++)
        {
            result.nullSpace(rawReceiverPhase(frequency, receiver), nullColumn) =
                -mu[frequency];
        }
        nullColumn++;
    }

    // Satellite GF code-bias directions.
    for (int satellite = 0; satellite < satelliteCount; satellite++)
    {
        result.nullSpace(rawSatelliteGfOffset + satellite, nullColumn) = +1;
        for (int receiver = 0; receiver < receiverCount; receiver++)
        {
            result.nullSpace(rawIonosphere(receiver, satellite), nullColumn) = +1;
        }
        for (int frequency = 0; frequency < frequencyCount; frequency++)
        {
            result.nullSpace(rawSatellitePhase(frequency, satellite), nullColumn) =
                +mu[frequency];
        }
        nullColumn++;
    }

    // Common receiver/satellite clock datum.
    for (int receiver = 0; receiver < receiverCount; receiver++)
    {
        result.nullSpace(rawReceiverClockOffset + receiver, nullColumn) = +1;
    }
    for (int satellite = 0; satellite < satelliteCount; satellite++)
    {
        result.nullSpace(rawSatelliteClockOffset + satellite, nullColumn) = +1;
    }
    nullColumn++;

    // Receiver phase-bias/ambiguity directions.
    for (int frequency = 0; frequency < frequencyCount; frequency++)
    {
        for (int receiver = 0; receiver < receiverCount; receiver++)
        {
            result.nullSpace(rawReceiverPhase(frequency, receiver), nullColumn) =
                lambda[frequency];
            for (int satellite = 0; satellite < satelliteCount; satellite++)
            {
                result.nullSpace(
                    rawAmbiguity(frequency, receiver, satellite),
                    nullColumn
                ) = -1;
            }
            nullColumn++;
        }
    }

    // Satellite phase-bias/ambiguity directions.
    for (int frequency = 0; frequency < frequencyCount; frequency++)
    {
        for (int satellite = 0; satellite < satelliteCount; satellite++)
        {
            result.nullSpace(rawSatellitePhase(frequency, satellite), nullColumn) =
                lambda[frequency];
            for (int receiver = 0; receiver < receiverCount; receiver++)
            {
                result.nullSpace(
                    rawAmbiguity(frequency, receiver, satellite),
                    nullColumn
                ) = -1;
            }
            nullColumn++;
        }
    }

    BOOST_REQUIRE_EQUAL(nullColumn, nullity);

    auto addTransform = [&](int fullRow, int rawColumn, double coefficient)
    {
        if (fullRow >= 0)
        {
            result.rawToFull(fullRow, rawColumn) += coefficient;
        }
    };

    for (int receiver = 0; receiver < receiverCount; receiver++)
    {
        addTransform(
            fullTroposphereOffset + receiver,
            rawTroposphereOffset + receiver,
            +1
        );
    }

    for (int receiver = 1; receiver < receiverCount; receiver++)
    {
        int fullRow = fullReceiverClock(receiver);
        addTransform(fullRow, rawReceiverClockOffset + receiver, +1);
        addTransform(fullRow, rawReceiverIfOffset + receiver, +1);
        addTransform(fullRow, rawReceiverClockOffset, -1);
        addTransform(fullRow, rawReceiverIfOffset, -1);
    }

    for (int satellite = 0; satellite < satelliteCount; satellite++)
    {
        int fullRow = fullSatelliteClockOffset + satellite;
        addTransform(fullRow, rawSatelliteClockOffset + satellite, +1);
        addTransform(fullRow, rawSatelliteIfOffset + satellite, +1);
        addTransform(fullRow, rawReceiverClockOffset, -1);
        addTransform(fullRow, rawReceiverIfOffset, -1);
    }

    for (int receiver = 0; receiver < receiverCount; receiver++)
    {
        for (int satellite = 0; satellite < satelliteCount; satellite++)
        {
            int fullRow = fullIonosphere(receiver, satellite);
            addTransform(fullRow, rawIonosphere(receiver, satellite), +1);
            addTransform(fullRow, rawReceiverGfOffset + receiver, +1);
            addTransform(fullRow, rawSatelliteGfOffset + satellite, -1);
        }
    }

    for (int frequency = 0; frequency < frequencyCount; frequency++)
    {
        for (int receiver = 1; receiver < receiverCount; receiver++)
        {
            int fullRow = fullReceiverPhase(frequency, receiver);
            addTransform(fullRow, rawReceiverPhase(frequency, receiver), +1);
            addTransform(fullRow, rawReceiverIfOffset + receiver, -1);
            addTransform(fullRow, rawReceiverGfOffset + receiver, +mu[frequency]);
            addTransform(fullRow, rawReceiverPhase(frequency, 0), -1);
            addTransform(fullRow, rawReceiverIfOffset, +1);
            addTransform(fullRow, rawReceiverGfOffset, -mu[frequency]);
            addTransform(
                fullRow,
                rawAmbiguity(frequency, receiver, 0),
                +lambda[frequency]
            );
            addTransform(
                fullRow,
                rawAmbiguity(frequency, 0, 0),
                -lambda[frequency]
            );
        }

        for (int satellite = 0; satellite < satelliteCount; satellite++)
        {
            int fullRow = fullSatellitePhase(frequency, satellite);
            addTransform(fullRow, rawSatellitePhase(frequency, satellite), +1);
            addTransform(fullRow, rawSatelliteIfOffset + satellite, +1);
            addTransform(
                fullRow,
                rawSatelliteGfOffset + satellite,
                -mu[frequency]
            );
            addTransform(fullRow, rawReceiverPhase(frequency, 0), +1);
            addTransform(fullRow, rawReceiverIfOffset, -1);
            addTransform(fullRow, rawReceiverGfOffset, +mu[frequency]);
            addTransform(
                fullRow,
                rawAmbiguity(frequency, 0, satellite),
                +lambda[frequency]
            );
        }

        for (int receiver = 1; receiver < receiverCount; receiver++)
        {
            for (int satellite = 1; satellite < satelliteCount; satellite++)
            {
                int fullRow = fullAmbiguity(frequency, receiver, satellite);
                addTransform(
                    fullRow,
                    rawAmbiguity(frequency, receiver, satellite),
                    +1
                );
                addTransform(
                    fullRow,
                    rawAmbiguity(frequency, 0, satellite),
                    -1
                );
                addTransform(
                    fullRow,
                    rawAmbiguity(frequency, receiver, 0),
                    -1
                );
                addTransform(fullRow, rawAmbiguity(frequency, 0, 0), +1);
            }
        }
    }

    return result;
}
}  // namespace

BOOST_AUTO_TEST_CASE(reference_row_and_column_are_ambiguity_s_bases)
{
    std::vector<E_ObsCode> baseline = {E_ObsCode::L1C, E_ObsCode::L2W};

    BOOST_CHECK(!zhangFullRankRetainsAmbiguity(
        "ZIM2",
        SatSys(E_Sys::GPS, 8),
        E_ObsCode::L1C,
        baseline,
        "ZIM2",
        "G08"
    ));
    BOOST_CHECK(!zhangFullRankRetainsAmbiguity(
        "FFMJ",
        SatSys(E_Sys::GPS, 8),
        E_ObsCode::L1C,
        baseline,
        "ZIM2",
        "G08"
    ));
    BOOST_CHECK(zhangFullRankRetainsAmbiguity(
        "FFMJ",
        SatSys(E_Sys::GPS, 12),
        E_ObsCode::L2W,
        baseline,
        "ZIM2",
        "G08"
    ));
    BOOST_CHECK(!zhangFullRankRetainsAmbiguity(
        "FFMJ",
        SatSys(E_Sys::GPS, 12),
        E_ObsCode::L5Q,
        baseline,
        "ZIM2",
        "G08"
    ));
}

BOOST_AUTO_TEST_CASE(raw_model_null_space_matches_the_29_zhang_s_basis_directions)
{
    ZhangFormalDesign design = buildZhangFormalDesign();

    Eigen::FullPivLU<MatrixXd> rawDecomposition(design.raw);
    rawDecomposition.setThreshold(1e-11);
    Eigen::FullPivLU<MatrixXd> nullDecomposition(design.nullSpace);
    nullDecomposition.setThreshold(1e-11);

    BOOST_CHECK_EQUAL(design.raw.rows(), 48);
    BOOST_CHECK_EQUAL(design.raw.cols(), 74);
    BOOST_CHECK_EQUAL(rawDecomposition.rank(), 45);
    BOOST_CHECK_EQUAL(rawDecomposition.dimensionOfKernel(), 29);
    BOOST_CHECK_EQUAL(nullDecomposition.rank(), 29);
    BOOST_CHECK_SMALL((design.raw * design.nullSpace).norm(), 1e-12);
    BOOST_CHECK_SMALL((design.rawToFull * design.nullSpace).norm(), 1e-12);
    BOOST_CHECK_SMALL(
        (design.full * design.rawToFull - design.raw).norm(),
        1e-12
    );
}

BOOST_AUTO_TEST_CASE(raw_generalized_inverse_and_full_rank_solution_are_equivalent)
{
    ZhangFormalDesign design = buildZhangFormalDesign();

    VectorXd rawState = VectorXd::LinSpaced(design.raw.cols(), -2.5, 3.5);
    VectorXd observations = design.raw * rawState;
    for (int row = 0; row < observations.rows(); row++)
    {
        observations(row) += 1e-3 * std::sin(0.37 * row);
    }

    Eigen::CompleteOrthogonalDecomposition<MatrixXd> rawSolver(design.raw);
    rawSolver.setThreshold(1e-11);
    Eigen::CompleteOrthogonalDecomposition<MatrixXd> fullSolver(design.full);
    fullSolver.setThreshold(1e-11);

    VectorXd rawSolution = rawSolver.solve(observations);
    VectorXd fullSolution = fullSolver.solve(observations);
    VectorXd transformedRawSolution = design.rawToFull * rawSolution;

    VectorXd rawPrediction = design.raw * rawSolution;
    VectorXd fullPrediction = design.full * fullSolution;

    BOOST_CHECK_SMALL((transformedRawSolution - fullSolution).norm(), 1e-9);
    BOOST_CHECK_SMALL((rawPrediction - fullPrediction).norm(), 1e-10);
    BOOST_CHECK_SMALL(
        ((observations - rawPrediction) - (observations - fullPrediction)).norm(),
        1e-10
    );
}

BOOST_AUTO_TEST_CASE(code_phase_ionosphere_float_design_is_full_column_rank)
{
    constexpr int receiverCount = 3;
    constexpr int satelliteCount = 4;
    constexpr int frequencyCount = 2;

    constexpr int troposphereCount = receiverCount;
    constexpr int receiverClockCount = receiverCount - 1;
    constexpr int satelliteClockCount = satelliteCount;
    constexpr int ionosphereCount = receiverCount * satelliteCount;
    constexpr int receiverPhaseCount = frequencyCount * (receiverCount - 1);
    constexpr int satellitePhaseCount = frequencyCount * satelliteCount;
    constexpr int ambiguityCount =
        frequencyCount * (receiverCount - 1) * (satelliteCount - 1);

    constexpr int columnCount =
        troposphereCount + receiverClockCount + satelliteClockCount + ionosphereCount +
        receiverPhaseCount + satellitePhaseCount + ambiguityCount;
    constexpr int rowCount = 2 * frequencyCount * receiverCount * satelliteCount;

    MatrixXd design = MatrixXd::Zero(rowCount, columnCount);

    int receiverClockOffset = troposphereCount;
    int satelliteClockOffset = receiverClockOffset + receiverClockCount;
    int ionosphereOffset = satelliteClockOffset + satelliteClockCount;
    int receiverPhaseOffset = ionosphereOffset + ionosphereCount;
    int satellitePhaseOffset = receiverPhaseOffset + receiverPhaseCount;
    int ambiguityOffset = satellitePhaseOffset + satellitePhaseCount;

    const double mu[frequencyCount] = {1.0, 1.6469444444444445};
    const double lambda[frequencyCount] = {0.190293672798365, 0.244210213424568};
    const double tropMapping[receiverCount][satelliteCount] = {
        {1.10, 1.43, 2.04, 3.12},
        {1.22, 1.67, 2.31, 2.86},
        {1.35, 1.82, 2.18, 3.57}
    };

    int row = 0;
    for (int frequency = 0; frequency < frequencyCount; frequency++)
    {
        for (int receiver = 0; receiver < receiverCount; receiver++)
        {
            for (int satellite = 0; satellite < satelliteCount; satellite++)
            {
                int ionosphereColumn =
                    ionosphereOffset + receiver * satelliteCount + satellite;

                design(row, receiver) = tropMapping[receiver][satellite];
                if (receiver > 0)
                {
                    design(row, receiverClockOffset + receiver - 1) = 1;
                }
                design(row, satelliteClockOffset + satellite) = -1;
                design(row, ionosphereColumn) = mu[frequency];
                row++;

                design(row, receiver) = tropMapping[receiver][satellite];
                if (receiver > 0)
                {
                    design(row, receiverClockOffset + receiver - 1) = 1;
                }
                design(row, satelliteClockOffset + satellite) = -1;
                design(row, ionosphereColumn) = -mu[frequency];

                if (receiver > 0)
                {
                    int receiverPhaseColumn =
                        receiverPhaseOffset + frequency * (receiverCount - 1) + receiver - 1;
                    design(row, receiverPhaseColumn) = 1;
                }

                int satellitePhaseColumn =
                    satellitePhaseOffset + frequency * satelliteCount + satellite;
                design(row, satellitePhaseColumn) = 1;

                if (receiver > 0 && satellite > 0)
                {
                    int ambiguityColumn =
                        ambiguityOffset +
                        frequency * (receiverCount - 1) * (satelliteCount - 1) +
                        (receiver - 1) * (satelliteCount - 1) + satellite - 1;
                    design(row, ambiguityColumn) = lambda[frequency];
                }
                row++;
            }
        }
    }

    BOOST_REQUIRE_EQUAL(row, rowCount);
    BOOST_REQUIRE_EQUAL(design.cols(), columnCount);

    Eigen::FullPivLU<MatrixXd> decomposition(design);
    decomposition.setThreshold(1e-11);

    BOOST_CHECK_EQUAL(decomposition.rank(), columnCount);
    BOOST_CHECK_EQUAL(decomposition.dimensionOfKernel(), 0);
}

BOOST_AUTO_TEST_CASE(reference_change_preserves_code_and_phase_observables)
{
    constexpr int receiverCount = 3;
    constexpr int satelliteCount = 4;
    constexpr int frequencyCount = 2;

    const double receiverClock[receiverCount] = {13.2, -7.1, 4.3};
    const double satelliteClock[satelliteCount] = {-2.4, 8.7, 1.1, -5.2};
    const double ionosphere[receiverCount][satelliteCount] = {
        {2.1, 3.2, 1.7, 4.0},
        {2.5, 3.6, 2.0, 4.4},
        {1.9, 3.0, 1.5, 3.8}
    };
    const double receiverPhase[frequencyCount][receiverCount] = {
        {0.12, -0.31, 0.25},
        {-0.21, 0.17, 0.38}
    };
    const double satellitePhase[frequencyCount][satelliteCount] = {
        {0.45, -0.22, 0.31, -0.16},
        {-0.37, 0.28, 0.19, -0.42}
    };
    const int ambiguity[frequencyCount][receiverCount][satelliteCount] = {
        {
            {10, 14, 21, 8},
            {17, 5, 12, 23},
            {6, 19, 15, 11}
        },
        {
            {32, 27, 18, 41},
            {25, 39, 31, 16},
            {44, 22, 36, 29}
        }
    };
    const double mu[frequencyCount] = {1.0, 1.6469444444444445};
    const double lambda[frequencyCount] = {0.190293672798365, 0.244210213424568};

    auto reconstructed = [&](int referenceReceiver, int referenceSatellite)
    {
        VectorXd values(2 * frequencyCount * receiverCount * satelliteCount);
        int row = 0;

        for (int frequency = 0; frequency < frequencyCount; frequency++)
        {
            for (int receiver = 0; receiver < receiverCount; receiver++)
            {
                for (int satellite = 0; satellite < satelliteCount; satellite++)
                {
                    double receiverClockEstimate =
                        receiverClock[receiver] - receiverClock[referenceReceiver];
                    double satelliteClockEstimate =
                        satelliteClock[satellite] - receiverClock[referenceReceiver];

                    values(row++) =
                        receiverClockEstimate -
                        satelliteClockEstimate +
                        mu[frequency] * ionosphere[receiver][satellite];

                    double receiverPhaseEstimate = 0;
                    if (receiver != referenceReceiver)
                    {
                        receiverPhaseEstimate =
                            receiverPhase[frequency][receiver] -
                            receiverPhase[frequency][referenceReceiver] +
                            lambda[frequency] *
                                (ambiguity[frequency][receiver][referenceSatellite] -
                                 ambiguity[frequency][referenceReceiver][referenceSatellite]);
                    }

                    double satellitePhaseEstimate =
                        satellitePhase[frequency][satellite] +
                        receiverPhase[frequency][referenceReceiver] +
                        lambda[frequency] *
                            ambiguity[frequency][referenceReceiver][satellite];

                    double ambiguityEstimate = 0;
                    if (receiver != referenceReceiver && satellite != referenceSatellite)
                    {
                        ambiguityEstimate =
                            ambiguity[frequency][receiver][satellite] -
                            ambiguity[frequency][referenceReceiver][satellite] -
                            ambiguity[frequency][receiver][referenceSatellite] +
                            ambiguity[frequency][referenceReceiver][referenceSatellite];
                    }

                    values(row++) =
                        receiverClockEstimate -
                        satelliteClockEstimate -
                        mu[frequency] * ionosphere[receiver][satellite] +
                        receiverPhaseEstimate +
                        satellitePhaseEstimate +
                        lambda[frequency] * ambiguityEstimate;
                }
            }
        }

        return values;
    };

    VectorXd datumA = reconstructed(0, 0);
    VectorXd datumB = reconstructed(1, 2);

    BOOST_CHECK_SMALL((datumA - datumB).norm(), 1e-12);
}

BOOST_AUTO_TEST_CASE(reference_change_transforms_full_covariance_without_information_loss)
{
    constexpr int receiverCount = 3;
    constexpr int satelliteCount = 3;
    constexpr double wavelength = 0.190293672798365;

    // Per datum: two receiver clocks, three satellite clocks, two receiver phase states,
    // three satellite phase states, and four DD ambiguities.
    constexpr int stateCount =
        (receiverCount - 1) + satelliteCount +
        (receiverCount - 1) + satelliteCount +
        (receiverCount - 1) * (satelliteCount - 1);

    auto transform = [&](int oldReceiver, int oldSatellite, int newReceiver, int newSatellite)
    {
        MatrixXd T = MatrixXd::Zero(stateCount, stateCount);

        auto recClockIndex = [&](int receiver, int reference)
        {
            int index = 0;
            for (int r = 0; r < receiverCount; r++)
            {
                if (r == reference)
                    continue;
                if (r == receiver)
                    return index;
                index++;
            }
            return -1;
        };
        auto satClockIndex = [&](int satellite)
        {
            return receiverCount - 1 + satellite;
        };
        auto recPhaseIndex = [&](int receiver, int reference)
        {
            int offset = receiverCount - 1 + satelliteCount;
            int index = 0;
            for (int r = 0; r < receiverCount; r++)
            {
                if (r == reference)
                    continue;
                if (r == receiver)
                    return offset + index;
                index++;
            }
            return -1;
        };
        auto satPhaseIndex = [&](int satellite)
        {
            return 2 * (receiverCount - 1) + satelliteCount + satellite;
        };
        auto ambiguityIndex = [&](int receiver, int satellite, int referenceReceiver, int referenceSatellite)
        {
            if (receiver == referenceReceiver || satellite == referenceSatellite)
                return -1;

            int offset = 2 * (receiverCount - 1) + 2 * satelliteCount;
            int index = 0;
            for (int r = 0; r < receiverCount; r++)
                for (int s = 0; s < satelliteCount; s++)
                {
                    if (r == referenceReceiver || s == referenceSatellite)
                        continue;
                    if (r == receiver && s == satellite)
                        return offset + index;
                    index++;
                }
            return -1;
        };
        auto add = [&](int row, int column, double value)
        {
            if (column >= 0)
                T(row, column) += value;
        };

        for (int receiver = 0; receiver < receiverCount; receiver++)
        {
            if (receiver == newReceiver)
                continue;

            int row = recClockIndex(receiver, newReceiver);
            add(row, recClockIndex(receiver, oldReceiver), +1);
            add(row, recClockIndex(newReceiver, oldReceiver), -1);
        }

        for (int satellite = 0; satellite < satelliteCount; satellite++)
        {
            int row = satClockIndex(satellite);
            add(row, satClockIndex(satellite), +1);
            add(row, recClockIndex(newReceiver, oldReceiver), -1);
        }

        for (int receiver = 0; receiver < receiverCount; receiver++)
        {
            if (receiver == newReceiver)
                continue;

            int row = recPhaseIndex(receiver, newReceiver);
            add(row, recPhaseIndex(receiver, oldReceiver), +1);
            add(row, recPhaseIndex(newReceiver, oldReceiver), -1);
            add(
                row,
                ambiguityIndex(receiver, newSatellite, oldReceiver, oldSatellite),
                +wavelength
            );
            add(
                row,
                ambiguityIndex(newReceiver, newSatellite, oldReceiver, oldSatellite),
                -wavelength
            );
        }

        for (int satellite = 0; satellite < satelliteCount; satellite++)
        {
            int row = satPhaseIndex(satellite);
            add(row, satPhaseIndex(satellite), +1);
            add(row, recPhaseIndex(newReceiver, oldReceiver), +1);
            add(
                row,
                ambiguityIndex(newReceiver, satellite, oldReceiver, oldSatellite),
                +wavelength
            );
        }

        for (int receiver = 0; receiver < receiverCount; receiver++)
            for (int satellite = 0; satellite < satelliteCount; satellite++)
            {
                if (receiver == newReceiver || satellite == newSatellite)
                    continue;

                int row = ambiguityIndex(receiver, satellite, newReceiver, newSatellite);
                add(
                    row,
                    ambiguityIndex(receiver, satellite, oldReceiver, oldSatellite),
                    +1
                );
                add(
                    row,
                    ambiguityIndex(newReceiver, satellite, oldReceiver, oldSatellite),
                    -1
                );
                add(
                    row,
                    ambiguityIndex(receiver, newSatellite, oldReceiver, oldSatellite),
                    -1
                );
                add(
                    row,
                    ambiguityIndex(newReceiver, newSatellite, oldReceiver, oldSatellite),
                    +1
                );
            }

        return T;
    };

    MatrixXd forward = transform(0, 0, 1, 2);
    MatrixXd reverse = transform(1, 2, 0, 0);

    BOOST_CHECK_SMALL((reverse * forward - MatrixXd::Identity(stateCount, stateCount)).norm(), 1e-12);

    auto observationDesign = [&](int referenceReceiver, int referenceSatellite)
    {
        MatrixXd design =
            MatrixXd::Zero(2 * receiverCount * satelliteCount, stateCount);

        auto recClockIndex = [&](int receiver)
        {
            int index = 0;
            for (int r = 0; r < receiverCount; r++)
            {
                if (r == referenceReceiver)
                    continue;
                if (r == receiver)
                    return index;
                index++;
            }
            return -1;
        };
        auto satClockIndex = [&](int satellite)
        {
            return receiverCount - 1 + satellite;
        };
        auto recPhaseIndex = [&](int receiver)
        {
            int offset = receiverCount - 1 + satelliteCount;
            int index = 0;
            for (int r = 0; r < receiverCount; r++)
            {
                if (r == referenceReceiver)
                    continue;
                if (r == receiver)
                    return offset + index;
                index++;
            }
            return -1;
        };
        auto satPhaseIndex = [&](int satellite)
        {
            return 2 * (receiverCount - 1) + satelliteCount + satellite;
        };
        auto ambiguityIndex = [&](int receiver, int satellite)
        {
            if (receiver == referenceReceiver || satellite == referenceSatellite)
                return -1;

            int offset = 2 * (receiverCount - 1) + 2 * satelliteCount;
            int index = 0;
            for (int r = 0; r < receiverCount; r++)
                for (int s = 0; s < satelliteCount; s++)
                {
                    if (r == referenceReceiver || s == referenceSatellite)
                        continue;
                    if (r == receiver && s == satellite)
                        return offset + index;
                    index++;
                }
            return -1;
        };

        int row = 0;
        for (int receiver = 0; receiver < receiverCount; receiver++)
            for (int satellite = 0; satellite < satelliteCount; satellite++)
            {
                if (int column = recClockIndex(receiver); column >= 0)
                {
                    design(row, column) = +1;
                }
                design(row, satClockIndex(satellite)) = -1;
                row++;

                if (int column = recClockIndex(receiver); column >= 0)
                {
                    design(row, column) = +1;
                }
                design(row, satClockIndex(satellite)) = -1;
                if (int column = recPhaseIndex(receiver); column >= 0)
                {
                    design(row, column) = +1;
                }
                design(row, satPhaseIndex(satellite)) = +1;
                if (int column = ambiguityIndex(receiver, satellite); column >= 0)
                {
                    design(row, column) = wavelength;
                }
                row++;
            }

        return design;
    };

    MatrixXd oldDesign = observationDesign(0, 0);
    MatrixXd newDesign = observationDesign(1, 2);
    BOOST_CHECK_SMALL((newDesign * forward - oldDesign).norm(), 1e-12);

    MatrixXd generator = MatrixXd::Random(stateCount, stateCount);
    MatrixXd oldCovariance =
        generator * generator.transpose() + 0.1 * MatrixXd::Identity(stateCount, stateCount);
    MatrixXd newCovariance = forward * oldCovariance * forward.transpose();
    MatrixXd recoveredCovariance = reverse * newCovariance * reverse.transpose();

    BOOST_CHECK_SMALL((newCovariance - newCovariance.transpose()).norm(), 1e-12);
    BOOST_CHECK_SMALL((recoveredCovariance - oldCovariance).norm(), 1e-10);

    Eigen::SelfAdjointEigenSolver<MatrixXd> eigenSolver(newCovariance);
    BOOST_REQUIRE_EQUAL(eigenSolver.info(), Eigen::Success);
    BOOST_CHECK_GT(eigenSolver.eigenvalues().minCoeff(), 0);

    VectorXd oldState = VectorXd::LinSpaced(stateCount, -0.8, 1.3);
    VectorXd newState = forward * oldState;
    VectorXd measurementNoise =
        VectorXd::LinSpaced(oldDesign.rows(), -2e-3, 3e-3);
    VectorXd observations = oldDesign * oldState + measurementNoise;

    VectorXd oldPrediction = oldDesign * oldState;
    VectorXd newPrediction = newDesign * newState;
    VectorXd oldInnovation = observations - oldPrediction;
    VectorXd newInnovation = observations - newPrediction;

    MatrixXd measurementCovariance =
        0.04 * MatrixXd::Identity(oldDesign.rows(), oldDesign.rows());
    MatrixXd oldInnovationCovariance =
        oldDesign * oldCovariance * oldDesign.transpose() + measurementCovariance;
    MatrixXd newInnovationCovariance =
        newDesign * newCovariance * newDesign.transpose() + measurementCovariance;

    BOOST_CHECK_SMALL((newPrediction - oldPrediction).norm(), 1e-12);
    BOOST_CHECK_SMALL((newInnovation - oldInnovation).norm(), 1e-12);
    BOOST_CHECK_SMALL(
        (newInnovationCovariance - oldInnovationCovariance).norm() /
            oldInnovationCovariance.norm(),
        1e-12
    );
}

namespace
{
MatrixXd zhangGraphPhaseDesign(const ZhangGraphBasis& basis, double wavelength = 0.190293672798365)
{
    std::vector<std::string> receivers;
    for (const auto& receiver : basis.receivers)
    {
        if (receiver != basis.rootReceiver)
        {
            receivers.push_back(receiver);
        }
    }
    std::vector<SatSys> satellites(basis.satellites.begin(), basis.satellites.end());

    std::vector<ZhangGraphEdge> cycleEdges;
    for (const auto& edge : basis.edges)
    {
        if (!basis.isTreeEdge(edge.receiver, edge.satellite))
        {
            cycleEdges.push_back(edge);
        }
    }

    std::map<std::string, int> receiverColumn;
    std::map<SatSys, int> satelliteColumn;
    std::map<ZhangGraphEdge, int> cycleColumn;

    int column = 0;
    for (const auto& receiver : receivers)
    {
        receiverColumn[receiver] = column++;
    }
    for (const auto& satellite : satellites)
    {
        satelliteColumn[satellite] = column++;
    }
    for (const auto& edge : cycleEdges)
    {
        cycleColumn[edge] = column++;
    }

    MatrixXd design = MatrixXd::Zero(basis.edges.size(), column);
    int row = 0;
    for (const auto& edge : basis.edges)
    {
        if (edge.receiver != basis.rootReceiver)
        {
            design(row, receiverColumn.at(edge.receiver)) = +1;
        }
        design(row, satelliteColumn.at(edge.satellite)) = +1;

        auto cycleIt = cycleColumn.find(edge);
        if (cycleIt != cycleColumn.end())
        {
            design(row, cycleIt->second) = wavelength;
        }
        row++;
    }
    return design;
}

MatrixXd zhangCycleMatrix(const ZhangGraphBasis& basis)
{
    std::vector<ZhangGraphEdge> edges(basis.edges.begin(), basis.edges.end());
    std::map<ZhangGraphEdge, int> edgeColumn;
    for (int index = 0; index < edges.size(); index++)
    {
        edgeColumn[edges[index]] = index;
    }

    std::vector<ZhangGraphEdge> nonTreeEdges;
    for (const auto& edge : edges)
    {
        if (!basis.isTreeEdge(edge.receiver, edge.satellite))
        {
            nonTreeEdges.push_back(edge);
        }
    }

    MatrixXd cycles = MatrixXd::Zero(nonTreeEdges.size(), edges.size());
    for (int row = 0; row < nonTreeEdges.size(); row++)
    {
        auto cycle = zhangFundamentalCycle(basis, nonTreeEdges[row]);
        for (const auto& [edge, coefficient] : cycle)
        {
            cycles(row, edgeColumn.at(edge)) = coefficient;
        }
    }
    return cycles;
}

std::set<ZhangGraphEdge> sparseConnectedGraph(int receiverCount, int satelliteCount, int seed)
{
    std::set<ZhangGraphEdge> edges;

    edges.insert({"R0", SatSys(E_Sys::GPS, 1)});
    for (int receiver = 1; receiver < receiverCount; receiver++)
    {
        int previousSatellite = 1 + (receiver - 1) % satelliteCount;
        int nextSatellite     = 1 + receiver % satelliteCount;
        edges.insert(
            {"R" + std::to_string(receiver), SatSys(E_Sys::GPS, previousSatellite)}
        );
        edges.insert(
            {"R" + std::to_string(receiver), SatSys(E_Sys::GPS, nextSatellite)}
        );
    }
    for (int satellite = receiverCount + 1; satellite <= satelliteCount; satellite++)
    {
        int receiver = (satellite + seed) % receiverCount;
        edges.insert(
            {"R" + std::to_string(receiver), SatSys(E_Sys::GPS, satellite)}
        );
    }

    std::mt19937 generator(seed);
    std::uniform_int_distribution<int> receiverDistribution(0, receiverCount - 1);
    std::uniform_int_distribution<int> satelliteDistribution(1, satelliteCount);
    for (int extra = 0; extra < receiverCount + satelliteCount; extra++)
    {
        edges.insert(
            {"R" + std::to_string(receiverDistribution(generator)),
             SatSys(E_Sys::GPS, satelliteDistribution(generator))}
        );
    }
    return edges;
}
}  // namespace

BOOST_AUTO_TEST_CASE(random_sparse_connected_graphs_have_full_rank_tree_coordinates)
{
    for (int seed = 1; seed <= 30; seed++)
    {
        int receiverCount  = 3 + seed % 6;
        int satelliteCount = 4 + seed % 9;
        auto edges = sparseConnectedGraph(receiverCount, satelliteCount, seed);

        ZhangGraphBasis basis = zhangBuildSpanningTree(edges, "R0");
        BOOST_REQUIRE(basis.connected);
        BOOST_CHECK_EQUAL(
            basis.treeEdges.size(),
            basis.receivers.size() + basis.satellites.size() - 1
        );

        MatrixXd design = zhangGraphPhaseDesign(basis);
        Eigen::FullPivLU<MatrixXd> decomposition(design);
        decomposition.setThreshold(1e-11);
        BOOST_CHECK_EQUAL(decomposition.rank(), design.cols());
        BOOST_CHECK_EQUAL(design.rows(), design.cols());
    }
}

BOOST_AUTO_TEST_CASE(two_spanning_trees_preserve_observations_and_integer_cycle_lattice)
{
    std::set<ZhangGraphEdge> edges = {
        {"R0", SatSys(E_Sys::GPS, 1)},
        {"R0", SatSys(E_Sys::GPS, 2)},
        {"R1", SatSys(E_Sys::GPS, 1)},
        {"R1", SatSys(E_Sys::GPS, 3)},
        {"R1", SatSys(E_Sys::GPS, 4)},
        {"R2", SatSys(E_Sys::GPS, 2)},
        {"R2", SatSys(E_Sys::GPS, 4)},
        {"R2", SatSys(E_Sys::GPS, 5)},
        {"R3", SatSys(E_Sys::GPS, 3)},
        {"R3", SatSys(E_Sys::GPS, 5)}
    };

    std::set<ZhangGraphEdge> preferredA = {
        {"R0", SatSys(E_Sys::GPS, 1)},
        {"R0", SatSys(E_Sys::GPS, 2)},
        {"R1", SatSys(E_Sys::GPS, 1)},
        {"R1", SatSys(E_Sys::GPS, 3)},
        {"R1", SatSys(E_Sys::GPS, 4)},
        {"R2", SatSys(E_Sys::GPS, 2)},
        {"R2", SatSys(E_Sys::GPS, 5)},
        {"R3", SatSys(E_Sys::GPS, 3)}
    };
    std::set<ZhangGraphEdge> preferredB = {
        {"R0", SatSys(E_Sys::GPS, 2)},
        {"R1", SatSys(E_Sys::GPS, 1)},
        {"R1", SatSys(E_Sys::GPS, 3)},
        {"R2", SatSys(E_Sys::GPS, 2)},
        {"R2", SatSys(E_Sys::GPS, 4)},
        {"R2", SatSys(E_Sys::GPS, 5)},
        {"R3", SatSys(E_Sys::GPS, 3)},
        {"R3", SatSys(E_Sys::GPS, 5)}
    };

    ZhangGraphBasis basisA = zhangBuildSpanningTree(edges, "R0", preferredA);
    ZhangGraphBasis basisB = zhangBuildSpanningTree(edges, "R0", preferredB);
    BOOST_REQUIRE(basisA.connected);
    BOOST_REQUIRE(basisB.connected);
    BOOST_CHECK(basisA.treeEdges != basisB.treeEdges);

    MatrixXd designA = zhangGraphPhaseDesign(basisA);
    MatrixXd designB = zhangGraphPhaseDesign(basisB);
    VectorXd observations = VectorXd::LinSpaced(edges.size(), -1.7, 2.3);
    VectorXd stateA = designA.fullPivLu().solve(observations);
    VectorXd stateB = designB.fullPivLu().solve(observations);
    BOOST_CHECK_SMALL((designA * stateA - observations).norm(), 1e-12);
    BOOST_CHECK_SMALL((designB * stateB - observations).norm(), 1e-12);

    MatrixXd treeTransform = designB.inverse() * designA;
    BOOST_CHECK_SMALL((designB * treeTransform - designA).norm(), 1e-12);

    MatrixXd generator = MatrixXd::Random(designA.cols(), designA.cols());
    MatrixXd covarianceA =
        generator * generator.transpose() +
        0.1 * MatrixXd::Identity(designA.cols(), designA.cols());
    MatrixXd covarianceB = treeTransform * covarianceA * treeTransform.transpose();
    VectorXd transformedState = treeTransform * stateA;
    VectorXd noise = VectorXd::LinSpaced(observations.size(), -2e-3, 3e-3);
    VectorXd measured = observations + noise;
    MatrixXd measurementCovariance =
        0.02 * MatrixXd::Identity(observations.size(), observations.size());

    BOOST_CHECK_SMALL((designB * transformedState - designA * stateA).norm(), 1e-12);
    BOOST_CHECK_SMALL(
        ((measured - designB * transformedState) - (measured - designA * stateA)).norm(),
        1e-12
    );
    MatrixXd innovationCovarianceA =
        designA * covarianceA * designA.transpose() + measurementCovariance;
    MatrixXd innovationCovarianceB =
        designB * covarianceB * designB.transpose() + measurementCovariance;
    BOOST_CHECK_SMALL(
        (innovationCovarianceB - innovationCovarianceA).norm() /
            innovationCovarianceA.norm(),
        1e-12
    );

    MatrixXd cyclesA = zhangCycleMatrix(basisA);
    MatrixXd cyclesB = zhangCycleMatrix(basisB);
    BOOST_REQUIRE_EQUAL(cyclesA.rows(), cyclesB.rows());

    MatrixXd basisTransform =
        cyclesB * cyclesA.transpose() * (cyclesA * cyclesA.transpose()).inverse();
    MatrixXd integerTransform = basisTransform.array().round().matrix();
    BOOST_CHECK_SMALL((basisTransform - integerTransform).norm(), 1e-12);
    BOOST_CHECK_SMALL((cyclesB - integerTransform * cyclesA).norm(), 1e-12);
    BOOST_CHECK_CLOSE(std::abs(integerTransform.determinant()), 1.0, 1e-10);

    VectorXd rawIntegers(edges.size());
    for (int index = 0; index < rawIntegers.size(); index++)
    {
        rawIntegers(index) = (7 * index + 3) % 19 - 9;
    }
    VectorXd integersA = cyclesA * rawIntegers;
    VectorXd integersB = cyclesB * rawIntegers;
    BOOST_CHECK_SMALL((integersA.array() - integersA.array().round()).matrix().norm(), 1e-12);
    BOOST_CHECK_SMALL((integersB - integerTransform * integersA).norm(), 1e-12);
}

BOOST_AUTO_TEST_CASE(tree_edge_failure_exchanges_basis_without_losing_rank)
{
    auto edges = sparseConnectedGraph(5, 7, 42);
    ZhangGraphBasis oldBasis = zhangBuildSpanningTree(edges, "R0");
    BOOST_REQUIRE(oldBasis.connected);

    ZhangGraphEdge failed = *oldBasis.treeEdges.rbegin();
    std::set<ZhangGraphEdge> remaining = edges;
    remaining.erase(failed);
    ZhangGraphBasis newBasis =
        zhangBuildSpanningTree(remaining, "R0", oldBasis.treeEdges);

    BOOST_REQUIRE(newBasis.connected);
    BOOST_CHECK(newBasis.treeEdges.find(failed) == newBasis.treeEdges.end());
    BOOST_CHECK(newBasis.treeEdges != oldBasis.treeEdges);

    MatrixXd design = zhangGraphPhaseDesign(newBasis);
    Eigen::FullPivLU<MatrixXd> decomposition(design);
    decomposition.setThreshold(1e-11);
    BOOST_CHECK_EQUAL(decomposition.rank(), design.cols());
}

BOOST_AUTO_TEST_CASE(represented_edges_are_preferred_for_replacement_tree)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    std::set<ZhangGraphEdge> edges = {
        {"R0", g01}, {"R0", g02}, {"R1", g01}, {"R1", g02}
    };
    std::set<ZhangGraphEdge> represented = {
        {"R1", g01}, {"R1", g02}
    };

    ZhangGraphBasis basis = zhangBuildSpanningTree(
        edges,
        "R0",
        {},
        {},
        represented
    );

    BOOST_REQUIRE(basis.connected);
    BOOST_CHECK(basis.treeEdges.find({"R1", g01}) != basis.treeEdges.end());
    BOOST_CHECK(basis.treeEdges.find({"R1", g02}) != basis.treeEdges.end());
}

BOOST_AUTO_TEST_CASE(longer_continuous_arc_breaks_historical_edge_ties)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    std::set<ZhangGraphEdge> edges = {
        {"R0", g01}, {"R0", g02}, {"R1", g01}, {"R1", g02}
    };
    std::map<ZhangGraphEdge, int> persistence = {
        {{"R0", g01}, 5},
        {{"R0", g02}, 4},
        {{"R1", g01}, 100},
        {{"R1", g02}, 90}
    };

    ZhangGraphBasis basis = zhangBuildSpanningTree(
        edges,
        "R0",
        {},
        {},
        edges,
        persistence
    );

    BOOST_REQUIRE(basis.connected);
    BOOST_CHECK(basis.treeEdges.find({"R1", g01}) != basis.treeEdges.end());
    BOOST_CHECK(basis.treeEdges.find({"R1", g02}) != basis.treeEdges.end());
}

BOOST_AUTO_TEST_CASE(disconnected_graph_is_detected_and_root_component_isolated)
{
    std::set<ZhangGraphEdge> edges = {
        {"R0", SatSys(E_Sys::GPS, 1)},
        {"R1", SatSys(E_Sys::GPS, 1)},
        {"R2", SatSys(E_Sys::GPS, 8)},
        {"R2", SatSys(E_Sys::GPS, 9)}
    };

    ZhangGraphBasis forest = zhangBuildSpanningTree(edges, "R0");
    BOOST_CHECK(!forest.connected);
    BOOST_CHECK_EQUAL(forest.componentCount, 2);

    auto rootEdges = zhangRootComponentEdges(edges, "R0");
    BOOST_CHECK_EQUAL(rootEdges.size(), 2);
    BOOST_CHECK(rootEdges.find({"R2", SatSys(E_Sys::GPS, 8)}) == rootEdges.end());

    ZhangGraphBasis rootBasis = zhangBuildSpanningTree(rootEdges, "R0");
    BOOST_CHECK(rootBasis.connected);
}

BOOST_AUTO_TEST_CASE(canonical_integer_coordinates_close_exactly_on_two_by_two_graph)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    std::set<ZhangGraphEdge> edges = {
        {"R0", g01}, {"R0", g02}, {"R1", g01}, {"R1", g02}
    };
    ZhangGraphBasis basis = zhangBuildSpanningTree(edges, "R0");
    BOOST_REQUIRE(basis.connected);

    ZhangCanonicalIntegerAudit audit = zhangCanonicalIntegerAudit(basis);
    BOOST_REQUIRE(audit.valid);
    BOOST_REQUIRE_EQUAL(audit.treeEdges.size(), 3);
    BOOST_REQUIRE_EQUAL(audit.chordEdges.size(), 1);
    BOOST_CHECK(zhangExactAbs(zhangExactDeterminant(audit.canonicalToArc)) == 1);
    BOOST_REQUIRE_EQUAL(audit.satelliteDatumSingleDifferences.size(), 1);
    BOOST_REQUIRE_EQUAL(audit.satelliteFixQuotient.size(), 1);
    BOOST_CHECK(
        std::all_of(
            audit.satelliteFixQuotient.front().begin(),
            audit.satelliteFixQuotient.front().end(),
            [](const auto& value) { return value == 0; }
        )
    );

    ZhangExactVector canonical = {2, -1, 3, 5};
    ZhangExactVector raw =
        zhangExactMatrixTimesColumn(audit.canonicalToArc, canonical);
    std::vector<ZhangGraphEdge> arcs = audit.treeEdges;
    arcs.insert(arcs.end(), audit.chordEdges.begin(), audit.chordEdges.end());
    std::map<ZhangGraphEdge, std::size_t> arcIndex;
    for (std::size_t index = 0; index < arcs.size(); index++)
    {
        arcIndex[arcs[index]] = index;
    }
    ZhangExactInteger recoveredCycle = 0;
    for (const auto& [edge, coefficient] :
         zhangFundamentalCycle(basis, audit.chordEdges.front()))
    {
        recoveredCycle += coefficient * raw[arcIndex.at(edge)];
    }
    BOOST_CHECK(recoveredCycle == canonical.back());
}

BOOST_AUTO_TEST_CASE(canonical_audit_rejects_stale_arcs_outside_active_tree)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    std::set<ZhangGraphEdge> activeEdges = {
        {"R0", g01}, {"R0", g02}, {"R1", g01}, {"R1", g02}
    };
    ZhangGraphBasis activeBasis = zhangBuildSpanningTree(activeEdges, "R0");
    BOOST_REQUIRE(activeBasis.connected);

    ZhangGraphBasis stateTransformBasis = activeBasis;
    stateTransformBasis.edges.insert({"STALE", SatSys(E_Sys::GPS, 9)});
    ZhangCanonicalIntegerAudit audit =
        zhangCanonicalIntegerAudit(stateTransformBasis);
    BOOST_CHECK(!audit.valid);
    BOOST_CHECK(audit.failureReason.find("missing_chord_endpoint") == 0);

    ZhangCanonicalIntegerAudit activeAudit =
        zhangCanonicalIntegerAudit(activeBasis);
    BOOST_CHECK(activeAudit.valid);
}

BOOST_AUTO_TEST_CASE(global_scale_canonical_audit_uses_sparse_structure)
{
    std::set<ZhangGraphEdge> edges;
    for (int receiver = 0; receiver < 17; receiver++)
    {
        for (int satellite = 1; satellite <= 17; satellite++)
        {
            edges.insert(
                {"R" + std::to_string(receiver), SatSys(E_Sys::GPS, satellite)}
            );
        }
    }
    ZhangGraphBasis basis = zhangBuildSpanningTree(edges, "R0");
    BOOST_REQUIRE(basis.connected);

    ZhangCanonicalIntegerAudit audit = zhangCanonicalIntegerAudit(basis);
    BOOST_REQUIRE(audit.valid);
    BOOST_CHECK(!audit.denseCanonicalMaterialised);
    BOOST_CHECK(audit.canonicalToArc.empty());
    BOOST_CHECK(!audit.canonicalToArcFingerprint.empty());
    BOOST_CHECK_EQUAL(audit.treeEdges.size(), 33);
    BOOST_CHECK_EQUAL(audit.chordEdges.size(), 256);
}

BOOST_AUTO_TEST_CASE(sparse_tree_exchange_is_an_exact_unimodular_integer_transition)
{
    std::set<ZhangGraphEdge> edges = {
        {"R0", SatSys(E_Sys::GPS, 1)}, {"R0", SatSys(E_Sys::GPS, 2)},
        {"R1", SatSys(E_Sys::GPS, 1)}, {"R1", SatSys(E_Sys::GPS, 3)},
        {"R2", SatSys(E_Sys::GPS, 2)}, {"R2", SatSys(E_Sys::GPS, 3)},
        {"R0", SatSys(E_Sys::GPS, 3)}, {"R2", SatSys(E_Sys::GPS, 1)}
    };
    std::set<ZhangGraphEdge> preferredA = {
        {"R0", SatSys(E_Sys::GPS, 1)}, {"R0", SatSys(E_Sys::GPS, 2)},
        {"R0", SatSys(E_Sys::GPS, 3)}, {"R1", SatSys(E_Sys::GPS, 1)},
        {"R2", SatSys(E_Sys::GPS, 2)}
    };
    std::set<ZhangGraphEdge> preferredB = {
        {"R0", SatSys(E_Sys::GPS, 1)}, {"R1", SatSys(E_Sys::GPS, 1)},
        {"R1", SatSys(E_Sys::GPS, 3)}, {"R2", SatSys(E_Sys::GPS, 2)},
        {"R2", SatSys(E_Sys::GPS, 3)}
    };
    ZhangGraphBasis basisA = zhangBuildSpanningTree(edges, "R0", preferredA);
    ZhangGraphBasis basisB = zhangBuildSpanningTree(edges, "R0", preferredB);
    BOOST_REQUIRE(basisA.connected);
    BOOST_REQUIRE(basisB.connected);
    BOOST_REQUIRE(basisA.treeEdges != basisB.treeEdges);

    ZhangExactMatrix forward = zhangCanonicalTransition(basisA, basisB);
    ZhangExactMatrix reverse = zhangCanonicalTransition(basisB, basisA);
    BOOST_REQUIRE(!forward.empty());
    BOOST_CHECK(zhangExactAbs(zhangExactDeterminant(forward)) == 1);
    BOOST_CHECK(
        zhangExactMultiply(reverse, forward) ==
        zhangExactIdentityMatrix(forward.size())
    );
}

BOOST_AUTO_TEST_CASE(satellite_product_target_is_exact_across_tree_exchange)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);

    // K2,2 has one current fundamental cycle.  The persistent product tree
    // uses that current chord, so the G02-G01 product correction is +k.
    std::set<ZhangGraphEdge> k22Edges = {
        {"R0", g01}, {"R0", g02}, {"R1", g01}, {"R1", g02}
    };
    ZhangGraphBasis current = zhangBuildSpanningTree(k22Edges, "R0");
    ZhangGraphBasis product = zhangBuildSpanningTree(
        k22Edges,
        "R0",
        {{"R0", g01}, {"R1", g01}, {"R1", g02}}
    );
    ZhangSatelliteProductTarget k22 =
        zhangBuildSatelliteProductTarget(current, product, g01);
    BOOST_REQUIRE(k22.valid);
    BOOST_REQUIRE_EQUAL(k22.matrix.size(), 1);
    BOOST_CHECK(k22.matrix.front() == ZhangExactVector({1}));
    BOOST_CHECK(k22.targetSatellites == std::vector<SatSys>({g02}));

    // Three stations/three satellites: the correction G*k changes when the
    // dynamic tree changes, but z_T + G*k must equal the same persistent
    // product datum exactly.  Comparing G*k alone would be mathematically
    // wrong because the dynamic-tree node integer potential changes too.
    SatSys g03(E_Sys::GPS, 3);
    std::set<ZhangGraphEdge> edges = {
        {"R0", g01}, {"R0", g02}, {"R0", g03},
        {"R1", g01}, {"R1", g02}, {"R1", g03},
        {"R2", g01}, {"R2", g02}, {"R2", g03},
    };
    ZhangGraphBasis basisA = zhangBuildSpanningTree(
        edges, "R0",
        {{"R0", g01}, {"R0", g02}, {"R0", g03},
         {"R1", g01}, {"R2", g02}}
    );
    ZhangGraphBasis basisB = zhangBuildSpanningTree(
        edges, "R0",
        {{"R0", g01}, {"R1", g01}, {"R1", g03},
         {"R2", g02}, {"R2", g03}}
    );
    ZhangGraphBasis productBasis = zhangBuildSpanningTree(
        edges, "R0",
        {{"R0", g02}, {"R1", g02}, {"R1", g03},
         {"R2", g01}, {"R2", g03}}
    );
    BOOST_REQUIRE(basisA.connected && basisB.connected && productBasis.connected);

    map<ZhangGraphEdge, ZhangExactInteger> physical;
    int value = 1;
    for (const auto& edge : edges)
    {
        physical[edge] = value++;
    }
    auto cycleValues = [&](const ZhangGraphBasis& basis)
    {
        ZhangCanonicalIntegerAudit audit = zhangCanonicalIntegerAudit(basis);
        ZhangExactVector cycles;
        for (const auto& chord : audit.chordEdges)
        {
            ZhangExactInteger cycle = 0;
            for (const auto& [edge, coefficient] :
                 zhangFundamentalCycle(basis, chord))
            {
                cycle += coefficient * physical.at(edge);
            }
            cycles.push_back(cycle);
        }
        return cycles;
    };
    auto satelliteDatum = [&](const ZhangGraphBasis& basis)
    {
        ZhangCanonicalIntegerAudit audit = zhangCanonicalIntegerAudit(basis);
        ZhangExactVector treeValues;
        for (const auto& edge : audit.treeEdges)
        {
            treeValues.push_back(physical.at(edge));
        }
        ZhangExactVector nodes =
            zhangExactMatrixTimesColumn(audit.treeInverse, treeValues);
        const std::size_t satelliteOffset = basis.receivers.size() - 1;
        map<SatSys, ZhangExactInteger> satelliteValues;
        std::size_t row = satelliteOffset;
        for (const auto& satellite : basis.satellites)
        {
            satelliteValues[satellite] = nodes[row++];
        }
        ZhangExactVector differences;
        for (const auto& satellite : basis.satellites)
        {
            if (satellite != g01)
            {
                differences.push_back(
                    satelliteValues[satellite] - satelliteValues[g01]
                );
            }
        }
        return differences;
    };
    ZhangExactVector productDatum = satelliteDatum(productBasis);
    for (const auto& basis : {basisA, basisB})
    {
        ZhangSatelliteProductTarget target =
            zhangBuildSatelliteProductTarget(basis, productBasis, g01);
        BOOST_REQUIRE(target.valid);
        ZhangExactVector corrected = satelliteDatum(basis);
        ZhangExactVector correction =
            zhangExactMatrixTimesColumn(target.matrix, cycleValues(basis));
        for (std::size_t row = 0; row < corrected.size(); row++)
        {
            corrected[row] += correction[row];
        }
        BOOST_CHECK(corrected == productDatum);
    }
}

BOOST_AUTO_TEST_CASE(product_support_metrics_distinguish_paths_bridges_and_capacity)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    SatSys g03(E_Sys::GPS, 3);

    std::set<ZhangGraphEdge> k22 = {
        {"R0", g01}, {"R0", g02}, {"R1", g01}, {"R1", g02}
    };
    BOOST_CHECK_EQUAL(
        zhangAlternativePhysicalPathCount(k22, {"R0", g01}), 1
    );
    ZhangSatelliteSupportMetrics redundant =
        zhangSatelliteSupportMetrics(k22);
    BOOST_REQUIRE_EQUAL(redundant.supportCounts.size(), 1);
    BOOST_CHECK_EQUAL(redundant.supportCounts.at({g01, g02}), 2);
    BOOST_CHECK_EQUAL(redundant.bridgeEdges.size(), 1);
    BOOST_CHECK_EQUAL(redundant.edgeConnectivity, 2);

    std::set<ZhangGraphEdge> chain = k22;
    chain.insert({"R2", g02});
    chain.insert({"R2", g03});
    ZhangSatelliteSupportMetrics metrics =
        zhangSatelliteSupportMetrics(chain);
    BOOST_CHECK_EQUAL(metrics.satellites.size(), 3);
    BOOST_CHECK_EQUAL(metrics.supportCounts.at({g01, g02}), 2);
    BOOST_CHECK_EQUAL(metrics.supportCounts.at({g02, g03}), 1);
    BOOST_CHECK_EQUAL(metrics.bridgeEdges.size(), 2);
    BOOST_CHECK_EQUAL(metrics.minimumSupport, 1);
    BOOST_CHECK_EQUAL(metrics.maximumSupport, 2);
    BOOST_CHECK_EQUAL(metrics.edgeConnectivity, 1);

    std::set<ZhangGraphEdge> tree = {
        {"R0", g01}, {"R0", g02}, {"R1", g02}
    };
    BOOST_CHECK_EQUAL(
        zhangAlternativePhysicalPathCount(tree, {"R0", g02}), 0
    );
}

BOOST_AUTO_TEST_CASE(promoted_satellite_relation_survives_source_arc_retirement)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g03(E_Sys::GPS, 3);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(
        g01, g03, 2, "four_physical_arcs", true
    ));
    manager.retireUnprovedBridges({g03});

    long long difference = 0;
    BOOST_CHECK(manager.relation(g01, g03, difference));
    BOOST_CHECK_EQUAL(difference, 2);
    BOOST_CHECK_EQUAL(manager.relationCount(), 1);
}

BOOST_AUTO_TEST_CASE(product_support_path_switch_preserves_component_and_value)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g02, 5, "path_p1", true));
    auto before = manager.status(g02, true);
    double rawBefore = 0.37;
    double productBefore = rawBefore + 0.19 * before.alignmentCycles;

    // Retiring p1 is a provenance event only; p2 proves the same relation.
    manager.retireUnprovedBridges({g02});
    BOOST_REQUIRE(manager.promoteRelation(g01, g02, 5, "path_p2", true));
    auto after = manager.status(g02, true);
    double productAfter = rawBefore + 0.19 * after.alignmentCycles;

    BOOST_CHECK_EQUAL(after.datumVersion, before.datumVersion);
    BOOST_CHECK_EQUAL(after.componentId, before.componentId);
    BOOST_CHECK_SMALL(productAfter - productBefore, 1e-15);
}

BOOST_AUTO_TEST_CASE(detached_subtree_keeps_internal_promoted_relations)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    SatSys g03(E_Sys::GPS, 3);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(
        g01, g02, 7, "unproved_dynamic_bridge", false
    ));
    BOOST_REQUIRE(manager.promoteRelation(
        g02, g03, -3, "promoted_subtree_relation", true
    ));

    manager.retireUnprovedBridges({g02, g03});
    long long difference = 0;
    BOOST_CHECK(!manager.relation(g01, g02, difference));
    BOOST_CHECK(manager.relation(g02, g03, difference));
    BOOST_CHECK_EQUAL(difference, -3);
    BOOST_CHECK_EQUAL(manager.status(g01, true).componentSize, 1);
    BOOST_CHECK_EQUAL(manager.status(g02, true).componentSize, 2);
}

BOOST_AUTO_TEST_CASE(inconsistent_satellite_integer_bridge_is_rejected)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    SatSys g03(E_Sys::GPS, 3);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g02, 2, "edge_12"));
    BOOST_REQUIRE(manager.promoteRelation(g02, g03, 4, "edge_23"));
    BOOST_CHECK(!manager.promoteRelation(g01, g03, 7, "bad_cycle"));
    BOOST_CHECK_EQUAL(manager.conflicts(), 1);
    long long difference = 0;
    BOOST_REQUIRE(manager.relation(g01, g03, difference));
    BOOST_CHECK_EQUAL(difference, 6);
}

BOOST_AUTO_TEST_CASE(satellite_product_events_distinguish_topology_progress)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g03(E_Sys::GPS, 3);
    SatSys g12(E_Sys::GPS, 12);
    SatSys g25(E_Sys::GPS, 25);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);

    auto edgeA = manager.promoteRelationDetailed(
        g01, g03, 9, "component_a"
    );
    BOOST_REQUIRE(edgeA.accepted);
    BOOST_CHECK(edgeA.type ==
        ZhangProductRelationEventType::NEW_COMPONENT_EDGE);

    auto edgeB = manager.promoteRelationDetailed(
        g12, g25, 150, "component_b"
    );
    BOOST_REQUIRE(edgeB.accepted);
    BOOST_CHECK(edgeB.type ==
        ZhangProductRelationEventType::NEW_COMPONENT_EDGE);

    auto merge = manager.promoteRelationDetailed(
        g03, g12, -21, "bridge"
    );
    BOOST_REQUIRE(merge.accepted);
    BOOST_CHECK(merge.type ==
        ZhangProductRelationEventType::COMPONENT_MERGE);
    BOOST_CHECK_EQUAL(merge.oldComponentSizeA, 2);
    BOOST_CHECK_EQUAL(merge.oldComponentSizeB, 2);
    BOOST_CHECK_EQUAL(merge.newComponentSize, 4);

    auto confirmation = manager.promoteRelationDetailed(
        g01, g25, 138, "redundant_path"
    );
    BOOST_REQUIRE(confirmation.accepted);
    BOOST_CHECK(confirmation.type ==
        ZhangProductRelationEventType::REDUNDANT_CONFIRMATION);

    auto conflict = manager.promoteRelationDetailed(
        g01, g25, 139, "bad_cycle"
    );
    BOOST_CHECK(!conflict.accepted);
    BOOST_CHECK(conflict.type ==
        ZhangProductRelationEventType::CONFLICT_REJECTED);
    BOOST_CHECK_EQUAL(manager.eventCount(
        ZhangProductRelationEventType::NEW_COMPONENT_EDGE), 2);
    BOOST_CHECK_EQUAL(manager.eventCount(
        ZhangProductRelationEventType::COMPONENT_MERGE), 1);
    BOOST_CHECK_EQUAL(manager.eventCount(
        ZhangProductRelationEventType::REDUNDANT_CONFIRMATION), 1);
}

BOOST_AUTO_TEST_CASE(local_fractional_alignment_loss_can_relink_to_component_anchor)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g03(E_Sys::GPS, 3);
    SatSys g22(E_Sys::GPS, 22);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g03, 9, "fixed_13"));
    BOOST_REQUIRE(manager.promoteRelation(g01, g22, -3, "fixed_122"));

    auto before = manager.status(g03, true);
    auto preserved = manager.applyDynamicTreeTransform({
        {g01, 10.0}, {g03, 10.25}, {g22, 7.0}
    });
    BOOST_CHECK(preserved.at(g01));
    BOOST_CHECK(!preserved.at(g03));
    BOOST_CHECK(preserved.at(g22));
    BOOST_CHECK(manager.alignmentState(g03) ==
        ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_PENDING);
    BOOST_CHECK(manager.status(g01, true).integerDatumContinuous);
    BOOST_CHECK(!manager.status(g03, true).integerDatumContinuous);

    auto relink = manager.realignRelation(
        g01, g03, 11, "same_component_relink"
    );
    BOOST_REQUIRE(relink.accepted);
    BOOST_CHECK(relink.type ==
        ZhangProductRelationEventType::CURRENT_REALIGNMENT);
    BOOST_CHECK(manager.alignmentState(g03) ==
        ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_VALID);
    auto after = manager.status(g03, true);
    BOOST_CHECK(after.integerDatumContinuous);
    BOOST_CHECK_EQUAL(after.datumVersion, before.datumVersion);
    BOOST_CHECK_EQUAL(after.discontinuityCounter, before.discontinuityCounter);
    long long currentDifference = 0;
    BOOST_REQUIRE(manager.relation(g01, g03, currentDifference));
    BOOST_CHECK_EQUAL(currentDifference, 11);
}

BOOST_AUTO_TEST_CASE(attaching_left_singleton_preserves_established_component_alignment)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g03(E_Sys::GPS, 3);
    SatSys g11(E_Sys::GPS, 11);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g03, 9, "established"));
    auto g01Before = manager.status(g01, true);
    auto g03Before = manager.status(g03, true);

    BOOST_REQUIRE(manager.promoteRelation(
        g11, g01, -12, "left_singleton_attachment"
    ));
    auto g01After = manager.status(g01, true);
    auto g03After = manager.status(g03, true);
    auto g11After = manager.status(g11, true);
    BOOST_CHECK_EQUAL(
        g01After.alignmentCycles, g01Before.alignmentCycles
    );
    BOOST_CHECK_EQUAL(
        g03After.alignmentCycles, g03Before.alignmentCycles
    );
    BOOST_CHECK_EQUAL(g11After.alignmentCycles, 12);
}

BOOST_AUTO_TEST_CASE(conflicting_current_relation_is_quarantined_then_relinked)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g07(E_Sys::GPS, 7);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g07, 12, "initial"));
    auto before = manager.status(g07, true);

    auto quarantined = manager.quarantineCurrentAlignment(
        g01, g07, g01
    );
    BOOST_CHECK(quarantined.type ==
        ZhangProductRelationEventType::CURRENT_ALIGNMENT_QUARANTINED);
    BOOST_CHECK(quarantined.quarantinedSatellite == g07);
    BOOST_CHECK(!manager.status(g07, true).integerValid);
    BOOST_CHECK(manager.status(g01, true).integerValid);

    auto relink = manager.realignRelation(
        g01, g07, 7559, "confirmed_current_coordinate"
    );
    BOOST_REQUIRE(relink.accepted);
    BOOST_CHECK(relink.type ==
        ZhangProductRelationEventType::CURRENT_REALIGNMENT);
    auto after = manager.status(g07, true);
    BOOST_CHECK(after.integerValid);
    BOOST_CHECK_EQUAL(after.datumVersion, before.datumVersion);
    BOOST_CHECK_EQUAL(after.discontinuityCounter, before.discontinuityCounter);
    long long currentDifference = 0;
    BOOST_REQUIRE(manager.relation(g01, g07, currentDifference));
    BOOST_CHECK_EQUAL(currentDifference, 7559);
}

BOOST_AUTO_TEST_CASE(held_support_quarantine_preserves_trusted_anchor)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g07(E_Sys::GPS, 7);
    SatSys g23(E_Sys::GPS, 23);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g07, 12, "initial_07"));
    BOOST_REQUIRE(manager.promoteRelation(g01, g23, -94, "initial_23"));

    std::set<SatSys> support{g01, g07, g23};
    BOOST_CHECK_EQUAL(
        manager.quarantineCurrentAlignments(support, g01), 2
    );
    BOOST_CHECK(manager.status(g01, true).integerValid);
    BOOST_CHECK(!manager.status(g07, true).integerValid);
    BOOST_CHECK(!manager.status(g23, true).integerValid);
    long long persistentDifference = 0;
    BOOST_REQUIRE(manager.relation(g01, g23, persistentDifference));
    BOOST_CHECK_EQUAL(persistentDifference, -94);
}

BOOST_AUTO_TEST_CASE(dynamic_tree_integer_changes_leave_product_invariant)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g02, 3, "initial_target"));
    constexpr double lambda = 0.190293672798365;
    std::map<SatSys, double> raw = {{g01, 0.2}, {g02, -0.4}};
    std::map<SatSys, double> product;
    for (const auto& [satellite, value] : raw)
    {
        product[satellite] = value +
            lambda * manager.status(satellite, true).alignmentCycles;
    }
    auto component = manager.status(g02, true).componentId;

    for (const auto& [satellite, stateJump] :
         std::map<SatSys, long long>{{g01, 4}, {g02, -5}})
    {
        raw[satellite] += lambda * stateJump;
        manager.applyDynamicTreeShift(satellite, -stateJump);
    }
    for (const auto& [satellite, value] : raw)
    {
        double transformedProduct = value +
            lambda * manager.status(satellite, true).alignmentCycles;
        BOOST_CHECK_SMALL(transformedProduct - product.at(satellite), 1e-14);
        BOOST_CHECK_EQUAL(manager.status(satellite, true).datumVersion, 0);
        BOOST_CHECK_EQUAL(manager.status(satellite, true).componentId, component);
    }
}

BOOST_AUTO_TEST_CASE(component_common_fractional_gauge_preserves_integer_datum)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g03(E_Sys::GPS, 3);
    SatSys g22(E_Sys::GPS, 22);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g03, 9, "fixed_13"));
    BOOST_REQUIRE(manager.promoteRelation(g01, g22, -3, "fixed_122"));

    auto before = manager.status(g03, true);
    auto preserved = manager.applyDynamicTreeTransform({
        {g01, 111.76080459977078},
        {g03, 114.76080459977078},
        {g22, 107.76080459977078},
    });
    BOOST_CHECK(preserved.at(g01));
    BOOST_CHECK(preserved.at(g03));
    BOOST_CHECK(preserved.at(g22));
    auto after = manager.status(g03, true);
    BOOST_CHECK(after.integerDatumContinuous);
    BOOST_CHECK_EQUAL(after.datumVersion, before.datumVersion);
    BOOST_CHECK_EQUAL(after.componentId, before.componentId);
    BOOST_CHECK_EQUAL(after.alignmentCycles - before.alignmentCycles, 3);
}

BOOST_AUTO_TEST_CASE(product_reference_exchange_preserves_integer_relations)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g03(E_Sys::GPS, 3);
    SatSys g22(E_Sys::GPS, 22);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g03, 9, "fixed_13"));
    BOOST_REQUIRE(manager.promoteRelation(g01, g22, -3, "fixed_122"));

    long long fromG01ToG22 = 0;
    long long fromG03ToG22 = 0;
    BOOST_REQUIRE(manager.relation(g01, g22, fromG01ToG22));
    BOOST_REQUIRE(manager.relation(g03, g22, fromG03ToG22));
    BOOST_CHECK_EQUAL(fromG01ToG22, -3);
    BOOST_CHECK_EQUAL(fromG03ToG22, -12);
    BOOST_CHECK_EQUAL(fromG01ToG22 - 9, fromG03ToG22);
}

BOOST_AUTO_TEST_CASE(only_satellite_discontinuity_changes_product_version)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g03(E_Sys::GPS, 3);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g03, 2, "fixed_relation"));
    auto initial = manager.status(g03, true);

    manager.applyDynamicTreeTransform({{g01, 4.25}, {g03, 7.25}});
    manager.markDynamicAlignmentUnknown({g03});
    auto dynamic = manager.status(g03, true);
    BOOST_CHECK_EQUAL(dynamic.datumVersion, initial.datumVersion);
    BOOST_CHECK_EQUAL(dynamic.discontinuityCounter, initial.discontinuityCounter);
    BOOST_CHECK(dynamic.integerDatumContinuous);

    manager.recordSatelliteDiscontinuity(g03);
    auto discontinuous = manager.status(g03, true);
    BOOST_CHECK_EQUAL(discontinuous.datumVersion, initial.datumVersion + 1);
    BOOST_CHECK_EQUAL(
        discontinuous.discontinuityCounter,
        initial.discontinuityCounter + 1
    );
    BOOST_CHECK(!discontinuous.integerDatumContinuous);
}

BOOST_AUTO_TEST_CASE(canonical_product_relations_reject_silent_satellite_substitution)
{
	const SatSys g01(E_Sys::GPS, 1);
	const SatSys g02(E_Sys::GPS, 2);
	const SatSys g03(E_Sys::GPS, 3);
	const SatSys g05(E_Sys::GPS, 5);
	const SatSys g07(E_Sys::GPS, 7);
	ZhangPersistentProductDatumRegistry registry;
	const auto initial = registry.selectRelations(
		E_Sys::GPS,
		{{g01, g02}, {g01, g03}, {g01, g05}},
		{g01, g02, g03, g05}, 3);
	BOOST_REQUIRE(initial.established);
	BOOST_REQUIRE_EQUAL(initial.selected.size(), 3);
	BOOST_CHECK_EQUAL(initial.selected[0].id(), "G01->G02");

	// The current graph proposes G07 after G02 becomes unavailable.  The
	// persistent registry keeps G01-G02 missing and must not replace it.
	const auto changedGraph = registry.selectRelations(
		E_Sys::GPS,
		{{g01, g03}, {g01, g05}, {g01, g07}},
		{g01, g03, g05, g07}, 3);
	BOOST_CHECK(changedGraph.silentSubstitutionRejected);
	BOOST_REQUIRE_EQUAL(changedGraph.selected.size(), 2);
	BOOST_REQUIRE_EQUAL(changedGraph.missing.size(), 1);
	BOOST_CHECK_EQUAL(changedGraph.missing[0].id(), "G01->G02");
	BOOST_REQUIRE_EQUAL(changedGraph.ignoredSubstitutes.size(), 1);
	BOOST_CHECK_EQUAL(changedGraph.ignoredSubstitutes[0].id(), "G01->G07");

	// Reversed reference orientation is the same canonical coordinate.
	const auto restored = registry.selectRelations(
		E_Sys::GPS,
		{{g02, g01}, {g03, g01}, {g05, g01}},
		{g01, g02, g03, g05}, 3);
	BOOST_CHECK(!restored.silentSubstitutionRejected);
	BOOST_CHECK(restored.selected == initial.selected);
}

BOOST_AUTO_TEST_CASE(l1c_and_l2w_product_datum_versions_are_independent)
{
	const SatSys g01(E_Sys::GPS, 1);
	const SatSys g03(E_Sys::GPS, 3);
	const auto relation = ZhangCanonicalSatelliteRelation::ordered(g01, g03);
	ZhangPersistentProductDatumRegistry registry;
	const auto l1Initial = registry.observe(
		E_Sys::GPS, E_ObsCode::L1C, relation, 0, 0, 0, 0, true);
	const auto l2Initial = registry.observe(
		E_Sys::GPS, E_ObsCode::L2W, relation, 0, 0, 0, 0, true);
	BOOST_REQUIRE(l1Initial.valid && l2Initial.valid);
	BOOST_CHECK_EQUAL(l1Initial.version, 0);
	BOOST_CHECK_EQUAL(l2Initial.version, 0);
	BOOST_CHECK_NE(l1Initial.productDatumId, l2Initial.productDatumId);

	// Temporary loss of absolute observability is quotient-only, not a new
	// datum version.
	const auto l1Quotient = registry.observe(
		E_Sys::GPS, E_ObsCode::L1C, relation, 0, 0, 0, 0, false);
	BOOST_CHECK(l1Quotient.quotientOnly);
	BOOST_CHECK(!l1Quotient.absoluteValid);
	BOOST_CHECK(!l1Quotient.versionChanged);
	BOOST_CHECK_EQUAL(l1Quotient.version, 0);

	// A real L1C endpoint discontinuity advances L1C only.
	const auto l1Changed = registry.observe(
		E_Sys::GPS, E_ObsCode::L1C, relation, 0, 1, 0, 1, false);
	const auto l2Unchanged = registry.observe(
		E_Sys::GPS, E_ObsCode::L2W, relation, 0, 0, 0, 0, true);
	BOOST_CHECK(l1Changed.versionChanged);
	BOOST_CHECK_EQUAL(l1Changed.version, 1);
	BOOST_CHECK_EQUAL(l2Unchanged.version, 0);
}

BOOST_AUTO_TEST_CASE(wide_lane_alignment_transport_is_invariant_across_s_basis_changes)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g03(E_Sys::GPS, 3);
    ZhangSatelliteDatumManager l1(E_Sys::GPS, E_ObsCode::L1W);
    ZhangSatelliteDatumManager l2(E_Sys::GPS, E_ObsCode::L2W);
    BOOST_REQUIRE(l1.promoteRelation(g01, g03, 5, "l1_relation"));
    BOOST_REQUIRE(l2.promoteRelation(g01, g03, 2, "l2_relation"));

    long long l1Before = 0;
    long long l2Before = 0;
    BOOST_REQUIRE(l1.relation(g01, g03, l1Before));
    BOOST_REQUIRE(l2.relation(g01, g03, l2Before));
    const auto l1AnchorBefore = l1.status(g01, true);
    const auto l1SatelliteBefore = l1.status(g03, true);
    const auto l2AnchorBefore = l2.status(g01, true);
    const auto l2SatelliteBefore = l2.status(g03, true);
    const long long persistentWideLaneBefore =
        (l1Before - l2Before) -
        (l1SatelliteBefore.alignmentCycles -
            l1AnchorBefore.alignmentCycles) +
        (l2SatelliteBefore.alignmentCycles -
            l2AnchorBefore.alignmentCycles);

    l1.applyDynamicTreeTransform({{g01, 4.25}, {g03, 7.25}});
    l2.applyDynamicTreeTransform({{g01, -1.4}, {g03, 0.6}});
    long long l1After = 0;
    long long l2After = 0;
    BOOST_REQUIRE(l1.relation(g01, g03, l1After));
    BOOST_REQUIRE(l2.relation(g01, g03, l2After));
    const auto l1Anchor = l1.status(g01, true);
    const auto l1Satellite = l1.status(g03, true);
    const auto l2Anchor = l2.status(g01, true);
    const auto l2Satellite = l2.status(g03, true);
    const long long persistentWideLaneAfter =
        (l1After - l2After) -
        (l1Satellite.alignmentCycles - l1Anchor.alignmentCycles) +
        (l2Satellite.alignmentCycles - l2Anchor.alignmentCycles);

    BOOST_CHECK_NE(l1After - l2After, persistentWideLaneBefore);
    BOOST_CHECK_EQUAL(persistentWideLaneAfter, persistentWideLaneBefore);
    BOOST_CHECK_EQUAL(l1Satellite.phaseSegment, 0);
    BOOST_CHECK_EQUAL(l2Satellite.phaseSegment, 0);
}

BOOST_AUTO_TEST_CASE(product_constraint_promotion_requires_exact_named_membership)
{
    auto recovered = ProductConstraintPromotion::recoverNamedTargets(
        {{1, 1}, {0, 1}}, {7, 4}, 2
    );
    BOOST_REQUIRE_EQUAL(recovered.size(), 2);
    BOOST_CHECK(recovered.at(0) == 3);
    BOOST_CHECK(recovered.at(1) == 4);

    auto unsaturated = ProductConstraintPromotion::recoverNamedTargets(
        {{2}}, {6}, 1
    );
    BOOST_CHECK(unsaturated.empty());
}

BOOST_AUTO_TEST_CASE(local_subtree_break_preserves_unaffected_product_continuity)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    SatSys g03(E_Sys::GPS, 3);
    std::set<ZhangGraphEdge> edges = {
        {"R0", g01}, {"R0", g02}, {"R1", g02}, {"R1", g03}
    };
    ZhangGraphBasis oldBasis = zhangBuildSpanningTree(edges, "R0");
    BOOST_REQUIRE(oldBasis.connected);

    edges.erase({"R0", g02});
    std::set<ZhangGraphEdge> retained = zhangRootComponentEdges(edges, "R0");
    ZhangGraphBasis rootBasis = zhangBuildSpanningTree(retained, "R0");
    BOOST_REQUIRE(rootBasis.connected);
    BOOST_CHECK_EQUAL(rootBasis.satellites.size(), 1);
    BOOST_CHECK(rootBasis.satellites.find(g01) != rootBasis.satellites.end());

    ZhangPhaseContinuityState unaffected;
    ZhangPhaseContinuityState detached;
    unaffected.markFixed();
    detached.markFixed();
    GTime time;
    detached.reinitialise(time, "local_subtree_break", 2);
    BOOST_CHECK(unaffected.integerValid());
    BOOST_CHECK_EQUAL(unaffected.counter, 0);
    BOOST_CHECK(!detached.integerValid());
    BOOST_CHECK_EQUAL(detached.counter, 1);
}

BOOST_AUTO_TEST_CASE(wide_lane_only_lattice_does_not_validate_individual_signals)
{
    ZhangExactMatrix held = {{1, -1}};
    ZhangDualSignalLatticeValidity validity =
        zhangClassifyDualSignalLattice(held, 1);
    BOOST_CHECK(!validity.l1);
    BOOST_CHECK(!validity.l2);
    BOOST_CHECK(validity.wideLane);

    ZhangIntegerLatticeMembership unsaturated =
        zhangIntegerRowLatticeContains({{2}}, {1});
    BOOST_CHECK(!unsaturated.contained);
    BOOST_REQUIRE_EQUAL(unsaturated.smithInvariants.size(), 1);
    BOOST_CHECK(unsaturated.smithInvariants.front() == 2);
}

BOOST_AUTO_TEST_CASE(wide_lane_plus_l1_lattice_recovers_both_signals)
{
    ZhangExactMatrix held = {{1, -1}, {1, 0}};
    ZhangDualSignalLatticeValidity validity =
        zhangClassifyDualSignalLattice(held, 1);
    BOOST_CHECK(validity.l1);
    BOOST_CHECK(validity.l2);
    BOOST_CHECK(validity.wideLane);
}

BOOST_AUTO_TEST_CASE(exact_row_hnf_removes_redundancy_and_tracks_integer_values)
{
    ZhangExactMatrix rows = {
        {2, 0},
        {0, 3},
        {2, 3},
        {4, 0}
    };
    ZhangExactVector values = {4, 6, 10, 8};
    ZhangExactRowHnf hnf = zhangExactRowHermiteNormalForm(rows, values);
    BOOST_REQUIRE(hnf.consistent);
    BOOST_REQUIRE_EQUAL(hnf.basis.size(), 2);
    BOOST_CHECK(zhangIntegerRowLatticeContains(hnf.basis, {2, 0}).contained);
    BOOST_CHECK(zhangIntegerRowLatticeContains(hnf.basis, {0, 3}).contained);
    BOOST_CHECK(!zhangIntegerRowLatticeContains(hnf.basis, {1, 0}).contained);

    ZhangExactRowHnf inconsistent = zhangExactRowHermiteNormalForm(
        {{1, 0}, {1, 0}},
        {2, 3}
    );
    BOOST_CHECK(!inconsistent.consistent);

    // Equation (17): membership must return the actual integer row
    // combination so the persistent-product shift can be evaluated exactly.
    ZhangIntegerLatticeMembership represented =
        zhangIntegerRowLatticeContains({{2, 1}, {1, 1}}, {3, 2});
    BOOST_REQUIRE(represented.contained);
    BOOST_REQUIRE_EQUAL(represented.combination.size(), 2);
    BOOST_CHECK(represented.combination == ZhangExactVector({1, 1}));
    ZhangExactVector heldValues = {5, 7};
    ZhangExactInteger shift = 0;
    for (std::size_t row = 0; row < represented.combination.size(); row++)
    {
        shift += represented.combination[row] * heldValues[row];
    }
    BOOST_CHECK(shift == 12);
}

BOOST_AUTO_TEST_CASE(exact_surviving_lattice_eliminates_removed_arcs_without_rounding)
{
    ZhangExactMatrix kernel = zhangExactIntegerKernel({
        {1, 1, 0},
        {0, 1, 1},
    });
    BOOST_REQUIRE_EQUAL(kernel.size(), 1);
    BOOST_REQUIRE_EQUAL(kernel.front().size(), 3);
    BOOST_CHECK(
        zhangExactMatrixTimesColumn(
            {{1, 1, 0}, {0, 1, 1}},
            kernel.front()
        ) == ZhangExactVector({0, 0})
    );

    // Every input row touches the removed third arc.  Their exact integer
    // combination r1-r2 survives as n1-n2=-2; row 3 is redundant.
    ZhangExactSurvivingLattice surviving = zhangExactSurvivingLattice(
        {
            {1, 0, 1},
            {0, 1, 1},
            {1, 1, 2},
        },
        {5, 7, 12},
        {true, true, false}
    );
    BOOST_CHECK(surviving.consistent);
    BOOST_CHECK_EQUAL(surviving.touchedRows, 3);
    BOOST_REQUIRE_EQUAL(surviving.basis.size(), 1);
    BOOST_CHECK(surviving.basis.front() == ZhangExactVector({1, -1}));
    BOOST_CHECK(surviving.values.front() == -2);

    ZhangExactSurvivingLattice none = zhangExactSurvivingLattice(
        {{1, 1}},
        {3},
        {true, false}
    );
    BOOST_CHECK(none.consistent);
    BOOST_CHECK(none.basis.empty());
}

BOOST_AUTO_TEST_CASE(integer_cycle_fixing_feedback_updates_full_state_and_covariance)
{
    constexpr int stateCount     = 9;
    constexpr int ambiguityCount = 3;

    MatrixXd generator = MatrixXd::Random(stateCount, stateCount);
    MatrixXd covariance =
        generator * generator.transpose() +
        0.5 * MatrixXd::Identity(stateCount, stateCount);
    VectorXd state = VectorXd::LinSpaced(stateCount, -1.2, 2.4);

    MatrixXd selector = MatrixXd::Zero(ambiguityCount, stateCount);
    selector(0, 4) = 1;
    selector(1, 6) = 1;
    selector(2, 8) = 1;

    VectorXd floatAmbiguities = selector * state;
    VectorXd fixedAmbiguities = floatAmbiguities.array().round().matrix();
    MatrixXd ambiguityCovariance = selector * covariance * selector.transpose();
    MatrixXd crossCovariance = covariance * selector.transpose();

    VectorXd fixedState =
        state -
        crossCovariance * ambiguityCovariance.inverse() *
            (floatAmbiguities - fixedAmbiguities);
    MatrixXd fixedCovariance =
        covariance -
        crossCovariance * ambiguityCovariance.inverse() * crossCovariance.transpose();
    fixedCovariance = 0.5 * (fixedCovariance + fixedCovariance.transpose());

    BOOST_CHECK_SMALL((selector * fixedState - fixedAmbiguities).norm(), 1e-12);
    BOOST_CHECK_SMALL((selector * fixedCovariance * selector.transpose()).norm(), 1e-10);
    BOOST_CHECK_LE(fixedCovariance.diagonal().sum(), covariance.diagonal().sum());

    Eigen::SelfAdjointEigenSolver<MatrixXd> eigenSolver(fixedCovariance);
    BOOST_REQUIRE_EQUAL(eigenSolver.info(), Eigen::Success);
    BOOST_CHECK_GE(eigenSolver.eigenvalues().minCoeff(), -1e-10);

    // Satellite clock/phase products occupy non-ambiguity states and must receive the correlated
    // conditional update rather than leaving the float products unchanged.
    BOOST_CHECK_GT((fixedState.head(4) - state.head(4)).norm(), 1e-6);
}

BOOST_AUTO_TEST_CASE(leave_one_out_internal_phase_products_restore_user_integer_differences)
{
    constexpr double wavelength = 0.190293672798365;

    std::set<ZhangGraphEdge> networkEdges = {
        {"R0", SatSys(E_Sys::GPS, 1)},
        {"R0", SatSys(E_Sys::GPS, 2)},
        {"R1", SatSys(E_Sys::GPS, 1)},
        {"R1", SatSys(E_Sys::GPS, 3)},
        {"R1", SatSys(E_Sys::GPS, 4)},
        {"R2", SatSys(E_Sys::GPS, 2)},
        {"R2", SatSys(E_Sys::GPS, 3)},
        {"R2", SatSys(E_Sys::GPS, 4)}
    };
    ZhangGraphBasis basis = zhangBuildSpanningTree(networkEdges, "R0");
    BOOST_REQUIRE(basis.connected);

    std::map<std::string, double> receiverBias = {
        {"R0", 0.13}, {"R1", -0.27}, {"R2", 0.41}
    };
    std::map<SatSys, double> satelliteBias;
    for (const auto& satellite : basis.satellites)
    {
        satelliteBias[satellite] = 0.07 * satellite.prn - 0.19;
    }

    VectorXd networkObservations(networkEdges.size());
    int edgeIndex = 0;
    for (const auto& edge : networkEdges)
    {
        int integerAmbiguity = 11 * (edgeIndex + 1) - 17;
        networkObservations(edgeIndex) =
            receiverBias.at(edge.receiver) +
            satelliteBias.at(edge.satellite) +
            wavelength * integerAmbiguity;
        edgeIndex++;
    }

    MatrixXd networkDesign = zhangGraphPhaseDesign(basis, wavelength);
    VectorXd networkState = networkDesign.fullPivLu().solve(networkObservations);
    BOOST_CHECK_SMALL(
        (networkDesign * networkState - networkObservations).norm(),
        1e-12
    );

    const int receiverPhaseCount = basis.receivers.size() - 1;
    std::vector<SatSys> satellites(basis.satellites.begin(), basis.satellites.end());
    std::map<SatSys, double> internalSatelliteProduct;
    for (int satellite = 0; satellite < satellites.size(); satellite++)
    {
        internalSatelliteProduct[satellites[satellite]] =
            networkState(receiverPhaseCount + satellite);
    }

    const double userBias = -0.33;
    std::map<SatSys, double> correctedUserPhase;
    for (int satellite = 0; satellite < satellites.size(); satellite++)
    {
        int userAmbiguity = 23 + 7 * satellite;
        double rawUserPhase =
            userBias +
            satelliteBias.at(satellites[satellite]) +
            wavelength * userAmbiguity;
        correctedUserPhase[satellites[satellite]] =
            rawUserPhase - internalSatelliteProduct.at(satellites[satellite]);
    }

    SatSys referenceSatellite = satellites.front();
    for (const auto& satellite : satellites)
    {
        double userIntegerDifference =
            (correctedUserPhase.at(satellite) -
             correctedUserPhase.at(referenceSatellite)) /
            wavelength;
        BOOST_CHECK_SMALL(
            userIntegerDifference - std::round(userIntegerDifference),
            1e-11
        );
    }
}

BOOST_AUTO_TEST_CASE(phase_continuity_integer_branch_change_preserves_validity)
{
    ZhangPhaseContinuityState state;
    state.markFixed();

    GTime time;
    time.bigTime = 1000;
    auto change = state.applyExactTransform(time, 4.0, 2);

    BOOST_CHECK(change == ZhangPhaseContinuityChange::EXACT_INTEGER);
    BOOST_CHECK_EQUAL(state.integerShiftCycles, 4);
    BOOST_CHECK_EQUAL(state.counter, 0);
    BOOST_CHECK_EQUAL(state.datumVersion, 0);
    BOOST_CHECK(state.integerValid());
}

BOOST_AUTO_TEST_CASE(phase_continuity_fractional_change_forces_user_reinitialisation)
{
    ZhangPhaseContinuityState state;
    state.markFixed();

    GTime time;
    time.bigTime = 2000;
    auto change = state.applyExactTransform(time, -2.25, 2);

    BOOST_CHECK(change == ZhangPhaseContinuityChange::EXACT_FRACTIONAL);
    BOOST_CHECK_EQUAL(state.counter, 1);
    BOOST_CHECK_EQUAL(state.datumVersion, 1);
    BOOST_CHECK_EQUAL(state.iod, 1);
    BOOST_CHECK_CLOSE(state.fractionalShiftCycles, -2.25, 1e-12);
    BOOST_CHECK(!state.integerValid());
    BOOST_CHECK_EQUAL(state.stabilizationRemaining, 2);
}

BOOST_AUTO_TEST_CASE(hou_product_coordinate_absorbs_fractional_tree_transform)
{
    constexpr double wavelength = 0.190293672798365;
    ZhangPhaseContinuityState continuity;
    continuity.markFixed();

    Vector2d before;
    before << 12.4, -3.7; // satellite clock and internal phase node [m]
    auto beforeTarget = zhangHouOsbLikePhaseCorrectionTarget(
        2, 0, 1, wavelength, 0.0);
    const double referenceCorrection = beforeTarget.value(before);

    constexpr double treeOffsetCycles = -2.25;
    Vector2d after = before;
    after(1) -= treeOffsetCycles * wavelength;
    continuity.applyHouProductTransform(treeOffsetCycles);
    const double alignmentCycles =
        continuity.integerShiftCycles + continuity.fractionalShiftCycles;
    auto afterTarget = zhangHouOsbLikePhaseCorrectionTarget(
        2, 0, 1, wavelength, alignmentCycles);

    BOOST_CHECK_SMALL(
        afterTarget.value(after) - referenceCorrection,
        1e-12
    );
    BOOST_CHECK_EQUAL(continuity.counter, 0);
    BOOST_CHECK_EQUAL(continuity.datumVersion, 0);
    BOOST_CHECK_EQUAL(continuity.iod, 0);
    BOOST_CHECK(continuity.integerValid());
    BOOST_CHECK_EQUAL(
        continuity.resetReason,
        "hou_exact_affine_s_transform"
    );
}

BOOST_AUTO_TEST_CASE(phase_continuity_reinitialisation_resets_branch_and_stabilises)
{
    ZhangPhaseContinuityState state;
    state.integerShiftCycles = 7;
    state.fractionalShiftCycles = 0.3;
    state.markFixed();

    GTime resetTime;
    resetTime.bigTime = 3000;
    auto change = state.reinitialise(resetTime, "tree_state_missing", 2);

    BOOST_CHECK(change == ZhangPhaseContinuityChange::REINITIALISED);
    BOOST_CHECK_EQUAL(state.counter, 1);
    BOOST_CHECK_EQUAL(state.datumVersion, 1);
    BOOST_CHECK_EQUAL(state.integerShiftCycles, 0);
    BOOST_CHECK_SMALL(state.fractionalShiftCycles, 1e-15);
    BOOST_CHECK_EQUAL(state.resetReason, "tree_state_missing");
    BOOST_CHECK(!state.integerValid());

    state.markFixed();
    state.advanceEpoch(resetTime);
    BOOST_CHECK(!state.integerValid());
    GTime next = resetTime + 300;
    state.advanceEpoch(next);
    BOOST_CHECK(state.integerValid());
}

BOOST_AUTO_TEST_CASE(phase_continuity_lost_integer_rank_invalidates_once)
{
    ZhangPhaseContinuityState state;
    state.markFixed();
    BOOST_CHECK(state.integerValid());

    GTime changeTime;
    changeTime.bigTime = 1247461200;
    BOOST_CHECK(state.invalidateIntegerDatum(
        changeTime,
        "integer_datum_incomplete",
        2
    ));
    BOOST_CHECK(!state.integerValid());
    BOOST_CHECK_EQUAL(state.counter, 1);
    BOOST_CHECK_EQUAL(state.datumVersion, 1);
    BOOST_CHECK_EQUAL(state.iod, 1);
    BOOST_CHECK_EQUAL(state.resetReason, "integer_datum_incomplete");

    BOOST_CHECK(!state.invalidateIntegerDatum(
        changeTime,
        "integer_datum_incomplete",
        2
    ));
    BOOST_CHECK_EQUAL(state.counter, 1);
}

BOOST_AUTO_TEST_CASE(held_out_user_reference_exchange_preserves_phase_and_covariance)
{
    constexpr double lambda = 0.190293672798365;

    // Old coordinates: receiver phase relative to satellite A, followed by
    // single-difference ambiguities B-A and C-A.
    Vector3d oldState(0.27, 13.4, -7.2);
    Matrix3d generator;
    generator << 0.7, -0.2, 0.1,
                 0.3,  1.1, 0.4,
                -0.1,  0.5, 0.9;
    Matrix3d oldCovariance =
        generator * generator.transpose() +
        0.1 * Matrix3d::Identity();

    // New reference B: b_B=b_A+lambda*D_BA,
    // D_AB=-D_BA and D_CB=D_CA-D_BA.
    Matrix3d transform;
    transform << 1, lambda, 0,
                 0,     -1, 0,
                 0,     -1, 1;
    Vector3d newState = transform * oldState;
    Matrix3d newCovariance =
        transform * oldCovariance * transform.transpose();

    Vector3d oldPredictions(
        oldState(0),
        oldState(0) + lambda * oldState(1),
        oldState(0) + lambda * oldState(2)
    );
    Vector3d newPredictions(
        newState(0) + lambda * newState(1),
        newState(0),
        newState(0) + lambda * newState(2)
    );
    BOOST_CHECK_SMALL((oldPredictions - newPredictions).norm(), 1e-12);

    Matrix3d inverse = transform.inverse();
    BOOST_CHECK_SMALL(
        (inverse * newState - oldState).norm(),
        1e-12
    );
    BOOST_CHECK_SMALL(
        (
            inverse * newCovariance * inverse.transpose() -
            oldCovariance
        ).norm(),
        1e-12
    );
}

namespace
{
struct ZhangProjectedPhysicalTarget
{
    double mean = 0;
    double variance = 0;
    ZhangAffineUserTarget coordinateTarget;
};

VectorXd satelliteMeanContrast(
    const std::set<ZhangGraphEdge>& edges,
    const SatSys& positive,
    const SatSys& negative)
{
    int positiveCount = 0;
    int negativeCount = 0;
    for (const auto& edge : edges)
    {
        positiveCount += edge.satellite == positive;
        negativeCount += edge.satellite == negative;
    }
    VectorXd contrast = VectorXd::Zero(edges.size());
    if (positiveCount == 0 || negativeCount == 0)
    {
        return VectorXd();
    }
    int row = 0;
    for (const auto& edge : edges)
    {
        if (edge.satellite == positive)
        {
            contrast(row) = +1.0 / positiveCount;
        }
        if (edge.satellite == negative)
        {
            contrast(row) = -1.0 / negativeCount;
        }
        row++;
    }
    return contrast;
}

ZhangProjectedPhysicalTarget projectPhysicalTarget(
    const ZhangGraphBasis& basis,
    const VectorXd& physicalEdgeState,
    const MatrixXd& physicalEdgeCovariance,
    const VectorXd& physicalTargetRow,
    double wavelength = 0.190293672798365)
{
    MatrixXd design = zhangGraphPhaseDesign(basis, wavelength);
    MatrixXd inverse = design.inverse();
    VectorXd coordinateState = inverse * physicalEdgeState;
    MatrixXd coordinateCovariance =
        inverse * physicalEdgeCovariance * inverse.transpose();

    ZhangProjectedPhysicalTarget result;
    result.coordinateTarget.row = design.transpose() * physicalTargetRow;
    result.coordinateTarget.offset = 0;
    result.coordinateTarget.units = "cycle";
    result.mean = result.coordinateTarget.value(coordinateState);
    result.variance =
        result.coordinateTarget.variance(coordinateCovariance);
    return result;
}

void checkProjectedTargetInvariant(
    const ZhangProjectedPhysicalTarget& first,
    const ZhangProjectedPhysicalTarget& second)
{
    BOOST_CHECK_SMALL(first.mean - second.mean, 1e-10);
    BOOST_CHECK_LT(
        zhangProtectedRelativeVarianceDifference(
            first.variance, second.variance
        ),
        1e-10
    );
}
}

BOOST_AUTO_TEST_CASE(user_phase_and_wl_targets_survive_three_affine_s_bases)
{
    constexpr double lambda1 = 0.190293672798365;
    constexpr double lambda2 = 0.244210213424568;
    VectorXd physicalState(6);
    // C_s, C_r, B_s1, B_r1, B_s2, B_r2, all in metres.
    physicalState << 0.18, -0.07, 1.22, -0.41, 0.87, -1.31;
    MatrixXd generator(6, 6);
    generator <<
        1.0,  0.1,  0.0,  0.2, -0.1,  0.0,
        0.2,  0.9, -0.1,  0.0,  0.1,  0.2,
        0.1, -0.2,  1.1,  0.3,  0.0, -0.1,
        0.0,  0.1,  0.2,  0.8, -0.2,  0.0,
       -0.1,  0.0,  0.1, -0.1,  1.2,  0.2,
        0.2, -0.1,  0.0,  0.1,  0.3,  0.9;
    MatrixXd physicalCovariance =
        generator * generator.transpose()
        + 0.2 * MatrixXd::Identity(6, 6);

    auto s1 = zhangUserPhaseCorrectionTarget(6, 0, 2, lambda1, 5);
	auto houS1 = zhangHouOsbLikePhaseCorrectionTarget(
		6, 0, 2, lambda1, 5);
    auto r1 = zhangUserPhaseCorrectionTarget(6, 1, 3, lambda1, -2);
    auto s2 = zhangUserPhaseCorrectionTarget(6, 0, 4, lambda2, 3);
    auto r2 = zhangUserPhaseCorrectionTarget(6, 1, 5, lambda2, 1);
    auto l1Difference = zhangLinearCombination(s1, +1, r1, -1, "metre");
    auto l2Difference = zhangLinearCombination(s2, +1, r2, -1, "metre");
    auto wideLane = zhangLinearCombination(
        l1Difference, +1 / lambda1,
        l2Difference, -1 / lambda2,
        "cycle"
    );
    BOOST_CHECK_EQUAL(
        zhangUserPhaseCorrectionValue(
            physicalState(0), physicalState(2), lambda1, 5
        ),
        physicalState(0) - (physicalState(2) + 5 * lambda1)
    );
	BOOST_CHECK_SMALL((houS1.row - s1.row).norm(), 1e-15);
	BOOST_CHECK_SMALL(houS1.offset - s1.offset, 1e-15);
	VectorXd commonPhaseDatumState = physicalState;
	commonPhaseDatumState(0) += 37.25;
	commonPhaseDatumState(2) += 37.25;
	BOOST_CHECK_SMALL(
		houS1.value(commonPhaseDatumState) - houS1.value(physicalState),
		1e-12);
    const double referenceMean = wideLane.value(physicalState);
    const double referenceVariance = wideLane.variance(physicalCovariance);

    std::vector<MatrixXd> transforms;
    MatrixXd first = MatrixXd::Identity(6, 6);
    first(2, 0) = 1;
    first(3, 1) = -1;
    first(4, 2) = 1;
    transforms.push_back(first);
    MatrixXd second = MatrixXd::Identity(6, 6);
    second.row(0).swap(second.row(1));
    second(4, 0) = -2;
    second(5, 1) = 1;
    transforms.push_back(second);
    MatrixXd third = MatrixXd::Identity(6, 6);
    third(0, 2) = 1;
    third(1, 3) = 1;
    third(4, 5) = -1;
    transforms.push_back(third);

    int transformNumber = 1;
    for (const auto& transform : transforms)
    {
        VectorXd translation = VectorXd::LinSpaced(
            6, -0.03 * transformNumber, 0.02 * transformNumber
        );
        VectorXd transformedState = transform * physicalState + translation;
        MatrixXd transformedCovariance =
            transform * physicalCovariance * transform.transpose();
        ZhangAffineUserTarget transformedTarget;
        BOOST_REQUIRE(zhangTransportAffineUserTarget(
            wideLane, transform, translation, transformedTarget
        ));
        BOOST_CHECK_SMALL(
            transformedTarget.value(transformedState) - referenceMean,
            1e-10
        );
        BOOST_CHECK_LT(
            zhangProtectedRelativeVarianceDifference(
                transformedTarget.variance(transformedCovariance),
                referenceVariance
            ),
            1e-10
        );
        transformNumber++;
    }
}

BOOST_AUTO_TEST_CASE(
    user_target_is_invariant_for_tree_receiver_and_satellite_reference_changes)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    SatSys g03(E_Sys::GPS, 3);
    std::set<ZhangGraphEdge> edges = {
        {"R0", g01}, {"R0", g02}, {"R0", g03},
        {"R1", g01}, {"R1", g02}, {"R1", g03},
        {"R2", g01}, {"R2", g02}, {"R2", g03}
    };
    ZhangGraphBasis t1 = zhangBuildSpanningTree(
        edges, "R0",
        {{"R0", g01}, {"R0", g02}, {"R0", g03},
         {"R1", g01}, {"R2", g02}}
    );
    ZhangGraphBasis t2 = zhangBuildSpanningTree(
        edges, "R0",
        {{"R0", g01}, {"R1", g01}, {"R1", g03},
         {"R2", g02}, {"R2", g03}}
    );
    ZhangGraphBasis t3 = zhangBuildSpanningTree(
        edges, "R1",
        {{"R1", g01}, {"R1", g02}, {"R2", g02},
         {"R2", g03}, {"R0", g03}}
    );
    BOOST_REQUIRE(t1.connected && t2.connected && t3.connected);
    BOOST_REQUIRE(t1.treeEdges != t2.treeEdges);
    BOOST_CHECK_NE(t1.rootReceiver, t3.rootReceiver);

    VectorXd physicalState = VectorXd::LinSpaced(edges.size(), -2.1, 1.7);
    MatrixXd generator = MatrixXd::Zero(edges.size(), edges.size());
    for (int row = 0; row < generator.rows(); row++)
    for (int column = 0; column <= row; column++)
    {
        generator(row, column) =
            row == column ? 1.0 + 0.1 * row : 0.01 * (row + column + 1);
    }
    MatrixXd physicalCovariance =
        generator * generator.transpose()
        + 0.1 * MatrixXd::Identity(edges.size(), edges.size());
    VectorXd g03MinusG01 = satelliteMeanContrast(edges, g03, g01);
    VectorXd g03MinusG02 = satelliteMeanContrast(edges, g03, g02);
    VectorXd g02MinusG01 = satelliteMeanContrast(edges, g02, g01);
    BOOST_CHECK_SMALL(
        (g03MinusG01 - g03MinusG02 - g02MinusG01).norm(),
        1e-15
    );

    auto p1 = projectPhysicalTarget(
        t1, physicalState, physicalCovariance, g03MinusG01
    );
    auto p2 = projectPhysicalTarget(
        t2, physicalState, physicalCovariance, g03MinusG01
    );
    auto p3 = projectPhysicalTarget(
        t3, physicalState, physicalCovariance, g03MinusG01
    );
    checkProjectedTargetInvariant(p1, p2);
    checkProjectedTargetInvariant(p1, p3);

    // Re-express the same G03-G01 product through reference satellite G02.
    auto p32 = projectPhysicalTarget(
        t3, physicalState, physicalCovariance, g03MinusG02
    );
    auto p21 = projectPhysicalTarget(
        t3, physicalState, physicalCovariance, g02MinusG01
    );
    BOOST_CHECK_SMALL(p3.mean - p32.mean - p21.mean, 1e-10);
    BOOST_CHECK_SMALL(
        (p3.coordinateTarget.row
            - p32.coordinateTarget.row
            - p21.coordinateTarget.row).norm(),
        1e-10
    );
}

BOOST_AUTO_TEST_CASE(
    satellite_join_and_leaf_exit_preserve_common_physical_target_subspace)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    SatSys g03(E_Sys::GPS, 3);
    std::set<ZhangGraphEdge> oldEdges = {
        {"R0", g01}, {"R0", g02},
        {"R1", g01}, {"R1", g02}
    };
    std::set<ZhangGraphEdge> joinedEdges = oldEdges;
    joinedEdges.insert({"R0", g03});
    joinedEdges.insert({"R1", g03});
    ZhangGraphBasis oldBasis = zhangBuildSpanningTree(oldEdges, "R0");
    ZhangGraphBasis joinedBasis = zhangBuildSpanningTree(
        joinedEdges, "R1",
        {{"R1", g01}, {"R1", g02}, {"R1", g03},
         {"R0", g02}}
    );
    BOOST_REQUIRE(oldBasis.connected && joinedBasis.connected);

    std::map<ZhangGraphEdge, double> value;
    std::map<ZhangGraphEdge, double> variance;
    int index = 1;
    for (const auto& edge : joinedEdges)
    {
        value[edge] = -0.8 + 0.17 * index;
        variance[edge] = 0.05 + 0.01 * index;
        index++;
    }
    auto assembleState = [&](const std::set<ZhangGraphEdge>& selected)
    {
        VectorXd state(selected.size());
        int row = 0;
        for (const auto& edge : selected)
        {
            state(row++) = value.at(edge);
        }
        return state;
    };
    auto assembleCovariance = [&](const std::set<ZhangGraphEdge>& selected)
    {
        MatrixXd covariance = MatrixXd::Zero(selected.size(), selected.size());
        int row = 0;
        for (const auto& edge : selected)
        {
            covariance(row, row) = variance.at(edge);
            row++;
        }
        return covariance;
    };

    auto beforeJoin = projectPhysicalTarget(
        oldBasis,
        assembleState(oldEdges),
        assembleCovariance(oldEdges),
        satelliteMeanContrast(oldEdges, g02, g01)
    );
    auto afterJoin = projectPhysicalTarget(
        joinedBasis,
        assembleState(joinedEdges),
        assembleCovariance(joinedEdges),
        satelliteMeanContrast(joinedEdges, g02, g01)
    );
    checkProjectedTargetInvariant(beforeJoin, afterJoin);

    // The reverse comparison is the leaf-exit case.  G03 targets are retired,
    // while the common G02-G01 target remains unchanged.
    checkProjectedTargetInvariant(afterJoin, beforeJoin);
}

BOOST_AUTO_TEST_CASE(dual_frequency_common_target_survives_independent_backbone_changes)
{
    constexpr double lambda1 = 0.190293672798365;
    constexpr double lambda2 = 0.244210213424568;
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    SatSys g03(E_Sys::GPS, 3);
    std::set<ZhangGraphEdge> edges = {
        {"R0", g01}, {"R0", g02}, {"R0", g03},
        {"R1", g01}, {"R1", g02}, {"R1", g03},
        {"R2", g01}, {"R2", g02}, {"R2", g03}
    };
    ZhangGraphBasis a = zhangBuildSpanningTree(edges, "R0");
    ZhangGraphBasis b = zhangBuildSpanningTree(
        edges, "R1",
        {{"R1", g01}, {"R1", g02}, {"R1", g03},
         {"R0", g01}, {"R2", g03}}
    );
    ZhangGraphBasis c = zhangBuildSpanningTree(
        edges, "R2",
        {{"R2", g01}, {"R2", g02}, {"R2", g03},
         {"R0", g02}, {"R1", g01}}
    );
    BOOST_REQUIRE(a.connected && b.connected && c.connected);

    const int edgeCount = edges.size();
    VectorXd physicalState = VectorXd::LinSpaced(2 * edgeCount, -1.9, 2.4);
    MatrixXd generator = MatrixXd::Identity(2 * edgeCount, 2 * edgeCount);
    for (int row = 1; row < generator.rows(); row++)
    {
        generator(row, row - 1) = 0.13;
    }
    MatrixXd physicalCovariance =
        generator * generator.transpose()
        + 0.1 * MatrixXd::Identity(2 * edgeCount, 2 * edgeCount);
    VectorXd satelliteContrast = satelliteMeanContrast(edges, g03, g01);
    VectorXd wideLanePhysicalRow(2 * edgeCount);
    wideLanePhysicalRow.head(edgeCount) = satelliteContrast / lambda1;
    wideLanePhysicalRow.tail(edgeCount) = -satelliteContrast / lambda2;

    auto projectDual = [&](const ZhangGraphBasis& first,
                           const ZhangGraphBasis& second)
    {
        MatrixXd design = MatrixXd::Zero(2 * edgeCount, 2 * edgeCount);
        design.topLeftCorner(edgeCount, edgeCount) =
            zhangGraphPhaseDesign(first, lambda1);
        design.bottomRightCorner(edgeCount, edgeCount) =
            zhangGraphPhaseDesign(second, lambda2);
        MatrixXd inverse = design.inverse();
        VectorXd state = inverse * physicalState;
        MatrixXd covariance =
            inverse * physicalCovariance * inverse.transpose();
        ZhangProjectedPhysicalTarget result;
        result.coordinateTarget.row = design.transpose() * wideLanePhysicalRow;
        result.coordinateTarget.units = "cycle";
        result.mean = result.coordinateTarget.value(state);
        result.variance = result.coordinateTarget.variance(covariance);
        return result;
    };

    auto first = projectDual(a, b);
    auto second = projectDual(c, a);
    auto third = projectDual(b, c);
    checkProjectedTargetInvariant(first, second);
    checkProjectedTargetInvariant(first, third);
}

BOOST_AUTO_TEST_CASE(fixed_lag_identity_resets_only_for_real_physical_change)
{
    using Transition = ZhangFixedLagIdentityTransition;
    BOOST_CHECK(
        zhangClassifyFixedLagTransition(true, true, false, false, false, false)
        == Transition::CONTINUE
    );
    BOOST_CHECK(
        zhangClassifyFixedLagTransition(true, true, false, false, true, true)
        == Transition::CONTINUE_EXACT_TRANSFORM
    );
    BOOST_CHECK(
        zhangClassifyFixedLagTransition(true, true, false, false, true, false)
        == Transition::RESET_EXACT_TRANSFORM_UNAVAILABLE
    );
    BOOST_CHECK(
        zhangClassifyFixedLagTransition(true, true, true, false, true, true)
        == Transition::RESET_PHYSICAL_IDENTITY
    );
    BOOST_CHECK(
        zhangClassifyFixedLagTransition(true, true, false, true, true, true)
        == Transition::RESET_PHYSICAL_IDENTITY
    );
    BOOST_CHECK(
        zhangClassifyFixedLagTransition(false, true, false, false, true, true)
        == Transition::START_NEW_TARGET
    );
    BOOST_CHECK(
        zhangClassifyFixedLagTransition(true, false, false, false, true, true)
        == Transition::RETIRE_TARGET
    );
}

BOOST_AUTO_TEST_CASE(square_root_window_matches_dense_schur_marginal)
{
    // Columns are [two epoch-local nuisance, two retained physical targets].
    MatrixXd denseFactor(8, 4);
    denseFactor <<
        1.0,  0.0,  0.4, -0.1,
        0.0,  1.0, -0.2,  0.3,
        0.7, -0.1,  1.0,  0.0,
       -0.3,  0.8,  0.0,  1.0,
        0.5,  0.2,  0.3,  0.7,
       -0.2,  0.4,  0.8, -0.5,
        0.1, -0.6,  0.2,  0.9,
        0.9,  0.3, -0.4,  0.2;
    VectorXd rhs(8);
    rhs << 0.7, -0.2, 1.1, -0.4, 0.5, 0.9, -0.7, 0.3;
    SparseMatrix<double> sparseFactor = denseFactor.sparseView();
    ZhangSquareRootMarginal marginal =
        zhangMarginaliseSquareRootFactors(sparseFactor, rhs, 2);
    BOOST_REQUIRE_MESSAGE(marginal.valid, marginal.failureReason);
    BOOST_CHECK_EQUAL(marginal.nuisanceRank, 2);
    BOOST_CHECK_EQUAL(marginal.targetRank, 2);

    MatrixXd normal = denseFactor.transpose() * denseFactor;
    VectorXd natural = denseFactor.transpose() * rhs;
    Matrix2d Nnn = normal.topLeftCorner<2, 2>();
    Matrix2d Nnt = normal.topRightCorner<2, 2>();
    Matrix2d Ntn = normal.bottomLeftCorner<2, 2>();
    Matrix2d Ntt = normal.bottomRightCorner<2, 2>();
    Vector2d hn = natural.head<2>();
    Vector2d ht = natural.tail<2>();
    Matrix2d denseSchur = Ntt - Ntn * Nnn.inverse() * Nnt;
    Vector2d denseNatural = ht - Ntn * Nnn.inverse() * hn;
    Vector2d denseMean = denseSchur.inverse() * denseNatural;
    Matrix2d denseCovariance = denseSchur.inverse();
    BOOST_CHECK_SMALL((marginal.mean - denseMean).norm(), 1e-11);
    BOOST_CHECK_SMALL(
        (marginal.covariance - denseCovariance).norm()
            / denseCovariance.norm(),
        1e-11
    );
}

BOOST_AUTO_TEST_CASE(square_root_window_is_invariant_to_retained_coordinate_change)
{
    MatrixXd originalFactor(7, 4);
    originalFactor <<
        1.0,  0.0,  0.2, -0.3,
        0.0,  1.0,  0.4,  0.1,
        0.6, -0.2,  1.0,  0.0,
        0.1,  0.7,  0.0,  1.0,
       -0.4,  0.3,  0.5,  0.8,
        0.8,  0.1, -0.2,  0.6,
        0.2, -0.5,  0.7, -0.4;
    VectorXd originalRhs(7);
    originalRhs << 0.3, -0.6, 1.2, 0.4, -0.1, 0.8, -0.5;
    auto original = zhangMarginaliseSquareRootFactors(
        originalFactor.sparseView(), originalRhs, 2
    );
    BOOST_REQUIRE_MESSAGE(original.valid, original.failureReason);

    // z_new = T z_old + b.  Substitute z_old=T^-1(z_new-b) in every factor.
    Matrix2d transform;
    transform << 1, 1, 0, 1;
    Vector2d translation(3, -2);
    Matrix2d inverse = transform.inverse();
    MatrixXd changedFactor = originalFactor;
    changedFactor.rightCols(2) = originalFactor.rightCols(2) * inverse;
    VectorXd changedRhs = originalRhs
        + originalFactor.rightCols(2) * inverse * translation;
    auto changed = zhangMarginaliseSquareRootFactors(
        changedFactor.sparseView(), changedRhs, 2
    );
    BOOST_REQUIRE_MESSAGE(changed.valid, changed.failureReason);
    Vector2d expectedMean = transform * original.mean + translation;
    Matrix2d expectedCovariance =
        transform * original.covariance * transform.transpose();
    BOOST_CHECK_SMALL((changed.mean - expectedMean).norm(), 1e-11);
    BOOST_CHECK_SMALL(
        (changed.covariance - expectedCovariance).norm()
            / expectedCovariance.norm(),
        1e-11
    );
}

BOOST_AUTO_TEST_CASE(whitening_reveals_rank_and_rejects_negative_variance)
{
    Vector3d residual(0.3, -0.5, 0.2);
    Matrix3d semidefinite;
    semidefinite <<
        2.0, 0.4, 0.0,
        0.4, 1.0, 0.0,
        0.0, 0.0, 1e-16;
    ZhangWhitenedBlock whitened =
        zhangWhitenRetainedResidual(residual, semidefinite, 1e-12);
    BOOST_REQUIRE_MESSAGE(whitened.valid, whitened.failureReason);
    BOOST_CHECK_EQUAL(whitened.rank, 2);
    Matrix2d leading = semidefinite.topLeftCorner<2, 2>();
    Vector2d leadingResidual = residual.head<2>();
    BOOST_CHECK_SMALL(
        whitened.squaredNorm
            - leadingResidual.dot(leading.ldlt().solve(leadingResidual)),
        1e-12
    );

    Matrix3d invalid = semidefinite;
    invalid(2, 2) = -1e-3;
    auto rejected = zhangWhitenRetainedResidual(residual, invalid, 1e-12);
    BOOST_CHECK(!rejected.valid);
    BOOST_CHECK_EQUAL(rejected.failureReason, "NEGATIVE_COVARIANCE_DIRECTION");
}

BOOST_AUTO_TEST_CASE(final_accepted_factor_capture_preserves_order_and_linearisation)
{
    KFKey clock;
    clock.type = KF::SAT_CLOCK;
    clock.Sat = SatSys(E_Sys::GPS, 1);
    KFKey phase;
    phase.type = KF::PHASE_BIAS;
    phase.Sat = SatSys(E_Sys::GPS, 1);
    phase.num = static_cast<int>(E_ObsCode::L1W);
    std::vector<ZhangCapturedStateKey> keys = {
        zhangCapturedStateKey(clock), zhangCapturedStateKey(phase)
    };

    Vector2d priorMean(0.4, -0.7);
    Matrix2d priorCovariance;
    priorCovariance << 0.8, 0.1, 0.1, 1.2;
    KFMeas firstMeasurement;
    firstMeasurement.time.bigTime = 1000;
    firstMeasurement.H = MatrixXd::Zero(2, 2);
    firstMeasurement.H << 1, -1, 0.5, 0.2;
    firstMeasurement.V = Vector2d(0.03, -0.04);
    firstMeasurement.R = Matrix2d::Zero();
    firstMeasurement.R << 0.01, 0.002, 0.002, 0.02;
    firstMeasurement.obsKeys = {clock, phase};
	firstMeasurement.prefitRatios = Vector2d(0.5, -2.0);

    ZhangFactorCaptureBuffer capture;
    capture.setMaximumEvents(10);
	const Matrix2d firstInnovationCovariance = firstMeasurement.H
		* priorCovariance * firstMeasurement.H.transpose()
		+ firstMeasurement.R;
	const Matrix2d firstGain = priorCovariance * firstMeasurement.H.transpose()
		* firstInnovationCovariance.inverse();
	const Vector2d firstPosteriorMean = priorMean
		+ firstGain * firstMeasurement.V;
	Matrix2d firstPosteriorCovariance = priorCovariance
		- firstGain * firstMeasurement.H * priorCovariance;
	firstPosteriorCovariance = 0.5
		* (firstPosteriorCovariance + firstPosteriorCovariance.transpose());
    BOOST_REQUIRE(capture.recordMeasurement(
        firstMeasurement.time,
        keys,
        priorMean,
        priorCovariance,
        firstMeasurement,
        "/PPP",
        firstPosteriorMean,
        firstPosteriorCovariance
    ));
    BOOST_REQUIRE_EQUAL(capture.capturedEvents().size(), 1);
    const auto& first = capture.capturedEvents().front();
    BOOST_CHECK_SMALL(
        (first.rightHandSide
            - (firstMeasurement.V + firstMeasurement.H * priorMean)).norm(),
        1e-15
    );
	Vector2d firstTargetRow(1, 0);
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		firstMeasurement.time,
		"GPS:WL:G01:G02",
		"L1W:REC:G01:A0=1;L2W:REC:G01:A0=-1;",
		"G01:0:0->G02:0:0",
		{{"L1W:REC:G01", 0}, {"L2W:REC:G01", 0}},
		keys,
		firstTargetRow,
		2,
		firstPosteriorMean,
		firstPosteriorCovariance
	));

    SparseMatrix<double> transition(2, 2);
    transition.insert(0, 0) = 1;
    transition.insert(1, 1) = 1;
    Matrix2d processCovariance = Matrix2d::Zero();
    processCovariance(1, 1) = 0.04;
    GTime transitionTime;
    transitionTime.bigTime = 1030;
    BOOST_REQUIRE(capture.recordTransition(
        transitionTime,
        keys,
        keys,
        transition,
        processCovariance,
        "KF_STATE_TRANSITION"
    ));

    SparseMatrix<double> exactTransform(2, 2);
    exactTransform.insert(0, 0) = 1;
    exactTransform.insert(0, 1) = 1;
    exactTransform.insert(1, 1) = 1;
    BOOST_REQUIRE(capture.recordCoordinateTransform(
        transitionTime,
        keys,
        keys,
        exactTransform,
        "synthetic S-basis exchange"
    ));

    KFMeas secondMeasurement = firstMeasurement;
    secondMeasurement.time.bigTime = 1030;
    Matrix2d denseExactTransform = MatrixXd(exactTransform);
    Vector2d transitionedPrior = firstPosteriorMean;
    Matrix2d transitionedCovariance =
        firstPosteriorCovariance + processCovariance;
    Vector2d transformedPrior = denseExactTransform * transitionedPrior;
    Matrix2d transformedPriorCovariance =
        denseExactTransform * transitionedCovariance
            * denseExactTransform.transpose();
	const Matrix2d secondInnovationCovariance = secondMeasurement.H
		* transformedPriorCovariance * secondMeasurement.H.transpose()
		+ secondMeasurement.R;
	const Matrix2d secondGain = transformedPriorCovariance
		* secondMeasurement.H.transpose()
		* secondInnovationCovariance.inverse();
	const Vector2d secondPosteriorMean = transformedPrior
		+ secondGain * secondMeasurement.V;
	Matrix2d secondPosteriorCovariance = transformedPriorCovariance
		- secondGain * secondMeasurement.H * transformedPriorCovariance;
	secondPosteriorCovariance = 0.5
		* (secondPosteriorCovariance + secondPosteriorCovariance.transpose());
    BOOST_REQUIRE(capture.recordMeasurement(
        secondMeasurement.time,
        keys,
        transformedPrior,
        transformedPriorCovariance,
        secondMeasurement,
        "/PPP",
		secondPosteriorMean,
		secondPosteriorCovariance
    ));
	Vector2d transformedTargetRow(1, -1);
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		secondMeasurement.time,
		"GPS:WL:G01:G02",
		"L1W:REC:G02:A0=1;L2W:REC:G02:A0=-1;",
		"G01:0:0->G02:0:0",
		{{"L1W:REC:G01", 0}, {"L2W:REC:G01", 0}},
		keys,
		transformedTargetRow,
		2,
		secondPosteriorMean,
		secondPosteriorCovariance
	));
	const auto rawIntegerDatum = capture.currentRawIntegerDatumMarginal();
	BOOST_REQUIRE_MESSAGE(rawIntegerDatum.valid, rawIntegerDatum.failureReason);
	BOOST_CHECK_EQUAL(rawIntegerDatum.targetRank, 1);
	BOOST_CHECK(!capture.recordPhysicalTarget(
		secondMeasurement.time,
		"GPS:WL:G01:G02",
		"L1W:REC:G02:A1=1;L2W:REC:G02:A0=-1;",
		"G01:0:0->G02:0:0",
		{{"L1W:REC:G01", 1}, {"L2W:REC:G01", 0}},
		keys,
		transformedTargetRow,
		2,
		secondPosteriorMean,
		secondPosteriorCovariance
	));
	BOOST_CHECK_EQUAL(
		capture.lastTargetReason(),
		"PERSISTENT_RAW_TARGET_PHYSICAL_VERSION_CHANGED");
    ZhangFactorCaptureSummary summary = capture.summary();
    BOOST_REQUIRE_MESSAGE(summary.valid, summary.failureReason);
    BOOST_CHECK_EQUAL(summary.events, 4);
    BOOST_CHECK_EQUAL(summary.measurements, 2);
    BOOST_CHECK_EQUAL(summary.transitions, 1);
    BOOST_CHECK_EQUAL(summary.coordinateTransforms, 1);
	BOOST_CHECK_EQUAL(summary.physicalTargets, 2);
	BOOST_CHECK_EQUAL(summary.physicalTargetCoordinateContinuations, 1);
	BOOST_CHECK_EQUAL(summary.physicalTargetIdentityResets, 0);
    BOOST_CHECK_EQUAL(summary.measurementRows, 4);
    BOOST_CHECK_SMALL(summary.maximumReplayPriorMeanRelativeError, 1e-15);
    BOOST_CHECK_SMALL(
        summary.maximumReplayPriorCovarianceRelativeError, 1e-15
    );
    BOOST_CHECK(
        capture.capturedEvents()[2].nonsingularCoordinateTransform
    );
	const auto scaleDiagnostics = capture.innovationScaleDiagnostics();
	BOOST_REQUIRE_EQUAL(scaleDiagnostics.size(), 2);
	std::vector<double> diagnosticScales;
	for (const auto& scale : scaleDiagnostics)
	{
		BOOST_CHECK_EQUAL(scale.blocks, 2);
		BOOST_CHECK_EQUAL(scale.samples, 2);
		diagnosticScales.push_back(scale.predictiveCovarianceScaleMle());
	}
	std::sort(diagnosticScales.begin(), diagnosticScales.end());
	BOOST_CHECK_CLOSE(diagnosticScales[0], 0.25, 1e-12);
	BOOST_CHECK_CLOSE(diagnosticScales[1], 4.0, 1e-12);
	BOOST_REQUIRE_EQUAL(capture.capturedPhysicalTargets().size(), 2);
	BOOST_CHECK_SMALL(
		capture.capturedPhysicalTargets()[1].mean
			- (2 + transformedTargetRow.dot(secondPosteriorMean)),
		1e-15
	);
	BOOST_CHECK_SMALL(
		capture.capturedPhysicalTargets()[1].variance
			- (transformedTargetRow.transpose()
				* secondPosteriorCovariance * transformedTargetRow)(0, 0),
		1e-15
	);
	BOOST_CHECK_SMALL(summary.maximumTargetMeanRelativeError, 1e-15);
	BOOST_CHECK_SMALL(summary.maximumTargetVarianceRelativeError, 1e-15);
}

BOOST_AUTO_TEST_CASE(factor_capture_fails_closed_on_state_key_chain_mismatch)
{
    KFKey firstKey;
    firstKey.type = KF::SAT_CLOCK;
    firstKey.Sat = SatSys(E_Sys::GPS, 1);
    KFKey secondKey = firstKey;
    secondKey.Sat = SatSys(E_Sys::GPS, 2);
    std::vector<ZhangCapturedStateKey> firstMap = {
        zhangCapturedStateKey(firstKey)
    };
    std::vector<ZhangCapturedStateKey> secondMap = {
        zhangCapturedStateKey(secondKey)
    };
    KFMeas measurement;
    measurement.H = MatrixXd::Ones(1, 1);
    measurement.V = VectorXd::Zero(1);
    measurement.R = MatrixXd::Identity(1, 1);
    measurement.obsKeys = {firstKey};
    VectorXd mean = VectorXd::Zero(1);
    MatrixXd covariance = MatrixXd::Identity(1, 1);

    ZhangFactorCaptureBuffer capture;
	MatrixXd posteriorCovariance = MatrixXd::Constant(1, 1, 0.5);
    BOOST_REQUIRE(capture.recordMeasurement(
        GTime(),
        firstMap,
        mean,
        covariance,
        measurement,
        "/PPP",
		mean,
		posteriorCovariance
    ));
    SparseMatrix<double> identity(1, 1);
    identity.insert(0, 0) = 1;
    BOOST_CHECK(!capture.recordTransition(
        GTime(), secondMap, secondMap, identity, covariance, "bad chain"
    ));
    BOOST_CHECK_EQUAL(
        capture.summary().failureReason,
        "INVALID_TRANSITION_CAPTURE_OR_KEY_CHAIN"
    );
}

BOOST_AUTO_TEST_CASE(retained_target_information_block_matches_scalar_schur_update)
{
	KFKey first;
	first.type = KF::SAT_CLOCK;
	first.Sat = SatSys(E_Sys::GPS, 1);
	KFKey second = first;
	second.Sat = SatSys(E_Sys::GPS, 2);
	std::vector<ZhangCapturedStateKey> keys = {
		zhangCapturedStateKey(first), zhangCapturedStateKey(second)
	};
	Vector2d priorMean(0.2, -0.1);
	Matrix2d priorCovariance = Matrix2d::Zero();
	priorCovariance.diagonal() << 2.0, 3.0;
	KFMeas measurement;
	measurement.H = MatrixXd::Zero(1, 2);
	measurement.H(0, 0) = 1;
	measurement.V = VectorXd::Constant(1, 0.3);
	measurement.R = MatrixXd::Constant(1, 1, 0.5);
	measurement.obsKeys = {first};
	Vector2d posteriorMean(0.44, -0.1);
	Matrix2d posteriorCovariance = Matrix2d::Zero();
	posteriorCovariance.diagonal() << 0.4, 3.0;

	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, priorMean, priorCovariance, measurement, "/PPP",
		posteriorMean, posteriorCovariance
	));
	Vector2d targetRow(1, 0);
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "GPS:WL:L1W:L2W:G01:G02", "basis-a",
		"G01:0:0->G02:0:0", {{"L1W:REC:G01", 0}}, keys,
		targetRow, 0, posteriorMean, posteriorCovariance
	));
	const auto& block = capture.currentRetainedBlock();
	BOOST_REQUIRE_MESSAGE(block.valid, block.failureReason);
	BOOST_CHECK_EQUAL(block.targetCount, 1);
	BOOST_CHECK_EQUAL(block.informationRank, 1);
	BOOST_CHECK_SMALL(block.whitenedSquaredNorm - 0.036, 1e-12);
	const auto incremental = capture.currentIncrementalTargetMarginal();
	BOOST_REQUIRE_MESSAGE(incremental.valid, incremental.failureReason);
	BOOST_CHECK_EQUAL(incremental.informationRank, 1);
	BOOST_CHECK_EQUAL(incremental.quotientValidRank, 1);
	BOOST_CHECK_EQUAL(incremental.absoluteValidRank, 1);
	BOOST_CHECK_EQUAL(incremental.storedRows, 1);
	BOOST_CHECK_EQUAL(incremental.storedColumns, 1);
}

BOOST_AUTO_TEST_CASE(raw_multi_epoch_window_matches_gaussian_kalman_posterior)
{
	// x = [C_s, C_r, B_r1, B_r2, I, N1, N2].  These signs match
	// ppp_obs.cpp: +C_r-C_s, phase -alpha*I, code +alpha*I,
	// receiver phase bias +B_rj and ambiguity +lambda_j*N_j.
	constexpr double lambda1 = 0.190293672798365;
	constexpr double lambda2 = 0.244210213424568;
	constexpr double alpha1 = 1.0;
	constexpr double alpha2 = 1.646944444444444;
	MatrixXd H = MatrixXd::Zero(4, 7);
	H.row(0) << -1, +1, +1,  0, -alpha1, lambda1, 0;
	H.row(1) << -1, +1,  0, +1, -alpha2, 0, lambda2;
	H.row(2) << -1, +1,  0,  0, +alpha1, 0, 0;
	H.row(3) << -1, +1,  0,  0, +alpha2, 0, 0;
	MatrixXd R = MatrixXd::Zero(4, 4);
	R.diagonal() << 4e-4, 6e-4, 0.16, 0.25;
	R(0, 1) = R(1, 0) = 8e-5;

	VectorXd priorMean(7);
	priorMean << 0.3, -0.1, 0.04, -0.02, 1.4, 12.2, -3.7;
	MatrixXd priorCovariance = MatrixXd::Zero(7, 7);
	priorCovariance.diagonal() << 4, 4, 1, 1, 9, 16, 16;
	VectorXd truth(7);
	truth << 0.25, -0.08, 0.03, -0.01, 1.2, 12, -4;
	VectorXd y1 = H * truth;
	y1 << y1(0) + 0.006, y1(1) - 0.004,
		y1(2) + 0.05, y1(3) - 0.08;

	MatrixXd F = MatrixXd::Identity(7, 7);
	MatrixXd Q = MatrixXd::Zero(7, 7);
	Q.diagonal() << 0.04, 0.09, 0.0025, 0.0025, 0.16, 0, 0;
	VectorXd y2 = H * truth;
	y2 << y2(0) - 0.003, y2(1) + 0.005,
		y2(2) - 0.03, y2(3) + 0.04;

	ZhangRawFactorWindow window;
	BOOST_REQUIRE_MESSAGE(
		window.initialise(priorMean, priorCovariance),
		window.lastFailureReason());
	BOOST_REQUIRE_MESSAGE(
		window.addAcceptedMeasurement(H, R, y1),
		window.lastFailureReason());
	BOOST_REQUIRE_MESSAGE(
		window.addStateTransition(F, Q), window.lastFailureReason());
	BOOST_REQUIRE_MESSAGE(
		window.addAcceptedMeasurement(H, R, y2),
		window.lastFailureReason());
	MatrixXd integerDatum = MatrixXd::Zero(1, 7);
	integerDatum(0, 5) = 1;
	integerDatum(0, 6) = -1;
	auto marginal = window.marginaliseToIntegerDatum(
		integerDatum, VectorXd::Zero(1));
	BOOST_REQUIRE_MESSAGE(marginal.valid, marginal.failureReason);

	// Independent covariance-form control.  Agreement proves numerical
	// equivalence to the finite-prior Gaussian posterior; it is deliberately
	// not used as a proof of classical observation estimability.
	VectorXd controlMean = priorMean;
	MatrixXd controlCovariance = priorCovariance;
	auto update = [&](const VectorXd& observation)
	{
		MatrixXd innovation = H * controlCovariance * H.transpose() + R;
		MatrixXd gain = controlCovariance * H.transpose()
			* innovation.ldlt().solve(MatrixXd::Identity(4, 4));
		controlMean += gain * (observation - H * controlMean);
		MatrixXd I = MatrixXd::Identity(7, 7);
		controlCovariance = (I - gain * H) * controlCovariance
			* (I - gain * H).transpose() + gain * R * gain.transpose();
		controlCovariance = 0.5
			* (controlCovariance + controlCovariance.transpose());
	};
	update(y1);
	controlMean = F * controlMean;
	controlCovariance = F * controlCovariance * F.transpose() + Q;
	update(y2);
	const double expectedMean = (integerDatum * controlMean)(0);
	const double expectedVariance =
		(integerDatum * controlCovariance * integerDatum.transpose())(0, 0);
	BOOST_TEST_MESSAGE(
		"raw-window complete-equation mean_error="
		<< std::abs(marginal.mean(0) - expectedMean)
		<< " variance_error="
		<< std::abs(marginal.covariance(0, 0) - expectedVariance));
	BOOST_CHECK_SMALL(marginal.mean(0) - expectedMean, 2e-10);
	BOOST_CHECK_SMALL(marginal.covariance(0, 0) - expectedVariance, 2e-10);
	BOOST_CHECK_EQUAL(marginal.targetRank, 1);
}

BOOST_AUTO_TEST_CASE(single_receiver_wl_is_not_observation_estimable_with_free_phase_biases)
{
	constexpr double lambda1 = 0.190293672798365;
	constexpr double lambda2 = 0.244210213424568;
	constexpr double alpha1 = 1.0;
	constexpr double alpha2 = 1.646944444444444;
	MatrixXd H = MatrixXd::Zero(4, 7);
	H.row(0) << -1, +1, +1,  0, -alpha1, lambda1, 0;
	H.row(1) << -1, +1,  0, +1, -alpha2, 0, lambda2;
	H.row(2) << -1, +1,  0,  0, +alpha1, 0, 0;
	H.row(3) << -1, +1,  0,  0, +alpha2, 0, 0;
	VectorXd target = VectorXd::Zero(7);
	target(5) = 1;
	target(6) = -1;
	MatrixXd augmented(H.rows() + 1, H.cols());
	augmented.topRows(H.rows()) = H;
	augmented.bottomRows(1) = target.transpose();
	Eigen::FullPivLU<MatrixXd> observationRows(H);
	Eigen::FullPivLU<MatrixXd> augmentedRows(augmented);
	BOOST_CHECK_EQUAL(augmentedRows.rank(), observationRows.rank() + 1);
}

BOOST_AUTO_TEST_CASE(network_fundamental_cycle_is_primitive_and_nuisance_orthogonal)
{
	// Edge order: R0-S0, R0-S1, R1-S0, R1-S1.  Removing one node datum
	// gives the real-valued receiver/satellite phase-bias incidence B.
	MatrixXd B = MatrixXd::Zero(4, 3);
	B.row(0) << 1, -1,  0;
	B.row(1) << 1,  0, -1;
	B.row(2) << 0, -1,  0;
	B.row(3) << 0,  0, -1;
	Vector4d cycle;
	cycle << 1, -1, -1, 1;
	BOOST_CHECK_SMALL((B.transpose() * cycle).norm(), 1e-14);
	int coefficientGcd = 0;
	for (int index = 0; index < cycle.size(); index++)
	{
		coefficientGcd = std::gcd(
			coefficientGcd, std::abs(static_cast<int>(cycle(index))));
	}
	BOOST_CHECK_EQUAL(coefficientGcd, 1);
	BOOST_CHECK_EQUAL(Eigen::FullPivLU<MatrixXd>(B).rank(), 3);
	MatrixXd complete(4, 4);
	complete.leftCols(3) = B;
	complete.col(3) = cycle;
	BOOST_CHECK_EQUAL(Eigen::FullPivLU<MatrixXd>(complete).rank(), 4);
}

BOOST_AUTO_TEST_CASE(four_legal_trees_replay_identical_raw_factors_and_integer_datum)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g02(E_Sys::GPS, 2);
	SatSys g03(E_Sys::GPS, 3);
	std::set<ZhangGraphEdge> edges = {
		{"R0", g01}, {"R0", g02}, {"R0", g03},
		{"R1", g01}, {"R1", g02}, {"R1", g03},
		{"R2", g01}, {"R2", g02}, {"R2", g03}
	};
	std::vector<ZhangGraphBasis> trees;
	trees.push_back(zhangBuildSpanningTree(edges, "R0"));
	trees.push_back(zhangBuildSpanningTree(
		edges, "R0", {{"R0", g01}, {"R1", g01}, {"R1", g02},
			{"R2", g02}, {"R2", g03}}));
	trees.push_back(zhangBuildSpanningTree(
		edges, "R0", {{"R0", g03}, {"R1", g02}, {"R1", g03},
			{"R2", g01}, {"R2", g03}}));
	std::mt19937 generator(20260806);
	std::uniform_real_distribution<double> quality(0.0, 1.0);
	for (int attempt = 0; attempt < 100 && trees.size() < 4; attempt++)
	{
		std::map<ZhangGraphEdge, double> seededQuality;
		for (const auto& edge : edges)
		{
			seededQuality[edge] = quality(generator);
		}
		ZhangGraphBasis candidate = zhangBuildSpanningTree(
			edges, "R0", {}, seededQuality);
		bool duplicate = std::any_of(
			trees.begin(), trees.end(), [&](const auto& existing)
			{
				return existing.treeEdges == candidate.treeEdges;
			});
		if (!duplicate)
		{
			trees.push_back(std::move(candidate));
		}
	}
	BOOST_REQUIRE_EQUAL(trees.size(), 4);
	for (const auto& tree : trees)
	{
		BOOST_REQUIRE(tree.connected);
	}
	std::set<std::set<ZhangGraphEdge>> distinctTrees;
	for (const auto& tree : trees)
	{
		distinctTrees.insert(tree.treeEdges);
	}
	BOOST_REQUIRE_EQUAL(distinctTrees.size(), 4);

	// The four coordinate systems all map to one physical state containing
	// four continuous nuisance terms followed by the nine physical arc
	// ambiguities.  Thus every replay receives byte-identical observations.
	const int nuisanceCount = 4; // satellite clock, receiver clock, phase, iono
	const int arcCount = edges.size();
	const int stateCount = nuisanceCount + arcCount;
	VectorXd physicalMean = VectorXd::Zero(stateCount);
	physicalMean.head(nuisanceCount) << 0.3, -0.2, 0.05, 1.1;
	for (int index = 0; index < arcCount; index++)
	{
		physicalMean(nuisanceCount + index) = index - 3.25;
	}
	MatrixXd physicalCovariance = MatrixXd::Identity(stateCount, stateCount);
	physicalCovariance.topLeftCorner(nuisanceCount, nuisanceCount) *= 4;
	MatrixXd physicalDesign = MatrixXd::Zero(arcCount, stateCount);
	int edgeRow = 0;
	for (const auto& ignored : edges)
	{
		physicalDesign(edgeRow, 0) = -1;
		physicalDesign(edgeRow, 1) = +1;
		physicalDesign(edgeRow, 2) = (edgeRow % 2 == 0) ? +1 : -1;
		physicalDesign(edgeRow, 3) = 0.2 + 0.03 * edgeRow;
		physicalDesign(edgeRow, nuisanceCount + edgeRow) = 1;
		edgeRow++;
	}
	MatrixXd observationCovariance = 0.04
		* MatrixXd::Identity(arcCount, arcCount);
	VectorXd observation = physicalDesign * physicalMean;
	for (int row = 0; row < observation.size(); row++)
	{
		observation(row) += 0.002 * (row - 4);
	}
	VectorXd secondObservation = physicalDesign * physicalMean;
	for (int row = 0; row < secondObservation.size(); row++)
	{
		secondObservation(row) -= 0.0015 * (row - 3);
	}
	MatrixXd physicalProcess = MatrixXd::Zero(stateCount, stateCount);
	physicalProcess.topLeftCorner(nuisanceCount, nuisanceCount).diagonal()
		<< 0.04, 0.09, 0.0025, 0.16;

	// Physical product datum: G02-G01 in a fixed product tree.
	ZhangGraphBasis productTree = trees[1];
	ZhangCanonicalIntegerAudit productAudit =
		zhangCanonicalIntegerAudit(productTree);
	BOOST_REQUIRE(productAudit.valid);
	BOOST_REQUIRE(productAudit.denseCanonicalMaterialised);
	VectorXd productDatumCoordinate = VectorXd::Zero(arcCount);
	const auto& productDifference =
		productAudit.satelliteDatumSingleDifferences.front();
	for (int treeColumn = 0;
		 treeColumn < static_cast<int>(productAudit.treeEdges.size());
		 treeColumn++)
	{
		productDatumCoordinate(treeColumn) =
			productDifference[treeColumn].convert_to<double>();
	}
	std::map<ZhangGraphEdge, int> physicalEdgeIndex;
	edgeRow = 0;
	for (const auto& edge : edges)
	{
		physicalEdgeIndex[edge] = edgeRow++;
	}
	auto canonicalToPhysicalArcs = [&](const ZhangCanonicalIntegerAudit& audit)
	{
		std::vector<ZhangGraphEdge> canonicalArcs = audit.treeEdges;
		canonicalArcs.insert(
			canonicalArcs.end(), audit.chordEdges.begin(), audit.chordEdges.end());
		MatrixXd result = MatrixXd::Zero(arcCount, arcCount);
		for (int auditRow = 0; auditRow < arcCount; auditRow++)
		for (int column = 0; column < arcCount; column++)
		{
			result(physicalEdgeIndex.at(canonicalArcs[auditRow]), column) =
				audit.canonicalToArc[auditRow][column].convert_to<double>();
		}
		return result;
	};
	const MatrixXd productCanonicalToPhysical =
		canonicalToPhysicalArcs(productAudit);
	const VectorXd physicalDatum = productCanonicalToPhysical.transpose()
		.fullPivLu().solve(productDatumCoordinate);

	std::vector<double> means;
	std::vector<double> variances;
	for (const auto& tree : trees)
	{
		ZhangCanonicalIntegerAudit audit = zhangCanonicalIntegerAudit(tree);
		BOOST_REQUIRE(audit.valid);
		BOOST_REQUIRE(audit.denseCanonicalMaterialised);
		MatrixXd canonicalToPhysical = MatrixXd::Identity(
			stateCount, stateCount);
		canonicalToPhysical.bottomRightCorner(arcCount, arcCount) =
			canonicalToPhysicalArcs(audit);
		MatrixXd physicalToCanonical = canonicalToPhysical.inverse();
		VectorXd coordinateMean = physicalToCanonical * physicalMean;
		MatrixXd coordinateCovariance = physicalToCanonical
			* physicalCovariance * physicalToCanonical.transpose();
		MatrixXd coordinateDesign = physicalDesign * canonicalToPhysical;
		MatrixXd target = MatrixXd::Zero(1, stateCount);
		for (int column = 0; column < arcCount; column++)
		{
			target(0, nuisanceCount + column) = physicalDatum(column);
		}
		target *= canonicalToPhysical;

		ZhangRawFactorWindow replay;
		BOOST_REQUIRE_MESSAGE(
			replay.initialise(coordinateMean, coordinateCovariance),
			replay.lastFailureReason());
		BOOST_REQUIRE_MESSAGE(
			replay.addAcceptedMeasurement(
				coordinateDesign, observationCovariance, observation),
			replay.lastFailureReason());
		const MatrixXd coordinateProcess = physicalToCanonical
			* physicalProcess * physicalToCanonical.transpose();
		BOOST_REQUIRE_MESSAGE(
			replay.addStateTransition(
				MatrixXd::Identity(stateCount, stateCount), coordinateProcess),
			replay.lastFailureReason());
		BOOST_REQUIRE_MESSAGE(
			replay.addAcceptedMeasurement(
				coordinateDesign, observationCovariance, secondObservation),
			replay.lastFailureReason());
		auto marginal = replay.marginaliseToIntegerDatum(
			target, VectorXd::Zero(1));
		BOOST_REQUIRE_MESSAGE(marginal.valid, marginal.failureReason);
		means.push_back(marginal.mean(0));
		variances.push_back(marginal.covariance(0, 0));
	}
	double maximumMeanDifference = 0;
	double maximumVarianceDifference = 0;
	for (int strategy = 1; strategy < 4; strategy++)
	{
		maximumMeanDifference = std::max(
			maximumMeanDifference, std::abs(means[strategy] - means[0]));
		maximumVarianceDifference = std::max(
			maximumVarianceDifference,
			std::abs(variances[strategy] - variances[0]));
		BOOST_CHECK_SMALL(means[strategy] - means[0], 1e-10);
		BOOST_CHECK_SMALL(variances[strategy] - variances[0], 1e-10);
	}
	BOOST_TEST_MESSAGE(
		"four-tree same-factor maximum_mean_difference="
		<< maximumMeanDifference
		<< " maximum_variance_difference="
		<< maximumVarianceDifference);
}

BOOST_AUTO_TEST_CASE(incremental_fixed_lag_matches_dense_batch_and_kalman_at_2_5_10_epochs)
{
	const Vector2d boundaryMean(0.2, -0.1);
	Matrix2d boundaryCovariance;
	boundaryCovariance << 1.0, 0.2, 0.2, 0.8;
	Matrix2d transition;
	transition << 1.0, 0.1, 0.0, 1.0;
	Matrix2d processCovariance;
	processCovariance << 0.04, 0.006, 0.006, 0.02;
	Matrix2d design;
	design << 1.0, 0.3, -0.2, 1.0;
	Matrix2d measurementCovariance;
	measurementCovariance << 0.09, 0.01, 0.01, 0.16;
	const MatrixXd noLocal = MatrixXd::Zero(2, 0);

	for (int epochCount : {2, 5, 10})
	{
		std::vector<Vector2d> observations;
		Vector2d truth(0.35, -0.25);
		for (int epoch = 0; epoch < epochCount; epoch++)
		{
			if (epoch > 0)
			{
				truth = transition * truth;
			}
			Vector2d observation = design * truth;
			observation(0) += 0.004 * (epoch - 2);
			observation(1) -= 0.003 * (epoch + 1);
			observations.push_back(observation);
		}

		Vector2d kalmanMean = boundaryMean;
		Matrix2d kalmanCovariance = boundaryCovariance;
		auto kalmanUpdate = [&](const Vector2d& observation)
		{
			const Matrix2d innovation = design * kalmanCovariance
				* design.transpose() + measurementCovariance;
			const Matrix2d gain = kalmanCovariance * design.transpose()
				* innovation.inverse();
			kalmanMean += gain * (observation - design * kalmanMean);
			const Matrix2d identity = Matrix2d::Identity();
			kalmanCovariance = (identity - gain * design) * kalmanCovariance
				* (identity - gain * design).transpose()
				+ gain * measurementCovariance * gain.transpose();
			kalmanCovariance = 0.5
				* (kalmanCovariance + kalmanCovariance.transpose());
		};

		ZhangIncrementalFixedLagSquareRoot incremental(3);
		BOOST_REQUIRE(incremental.initialise(
			boundaryMean, boundaryCovariance));
		BOOST_REQUIRE(incremental.addLatestMeasurement(
			design, noLocal, measurementCovariance, observations[0]));
		kalmanUpdate(observations[0]);
		for (int epoch = 1; epoch < epochCount; epoch++)
		{
			kalmanMean = transition * kalmanMean;
			kalmanCovariance = transition * kalmanCovariance
				* transition.transpose() + processCovariance;
			BOOST_REQUIRE(incremental.advance(
				transition, processCovariance));
			BOOST_REQUIRE(incremental.addLatestMeasurement(
				design, noLocal, measurementCovariance,
				observations[epoch]));
			kalmanUpdate(observations[epoch]);
		}

		const int stateSize = 2 * epochCount;
		const int rows = 2 + 2 * epochCount + 2 * (epochCount - 1);
		MatrixXd batchFactor = MatrixXd::Zero(rows, stateSize);
		VectorXd batchRhs = VectorXd::Zero(rows);
		const Matrix2d boundaryWeight = boundaryCovariance.llt()
			.matrixL().solve(Matrix2d::Identity());
		const Matrix2d processWeight = processCovariance.llt()
			.matrixL().solve(Matrix2d::Identity());
		const Matrix2d measurementWeight = measurementCovariance.llt()
			.matrixL().solve(Matrix2d::Identity());
		int row = 0;
		batchFactor.block<2, 2>(row, 0) = boundaryWeight;
		batchRhs.segment<2>(row) = boundaryWeight * boundaryMean;
		row += 2;
		for (int epoch = 0; epoch < epochCount; epoch++)
		{
			batchFactor.block<2, 2>(row, 2 * epoch) =
				measurementWeight * design;
			batchRhs.segment<2>(row) =
				measurementWeight * observations[epoch];
			row += 2;
			if (epoch + 1 < epochCount)
			{
				batchFactor.block<2, 2>(row, 2 * epoch) =
					-processWeight * transition;
				batchFactor.block<2, 2>(row, 2 * (epoch + 1)) =
					processWeight;
				row += 2;
			}
		}
		const MatrixXd batchInformation = batchFactor.transpose() * batchFactor;
		const MatrixXd batchCovariance = batchInformation.inverse();
		const VectorXd batchMean = batchCovariance
			* batchFactor.transpose() * batchRhs;
		const Vector2d batchLatestMean = batchMean.tail<2>();
		const Matrix2d batchLatestCovariance =
			batchCovariance.bottomRightCorner<2, 2>();

		const auto qrLatest = incremental.latestMarginal();
		BOOST_REQUIRE_MESSAGE(qrLatest.valid, qrLatest.failureReason);
		BOOST_CHECK_SMALL((qrLatest.mean - batchLatestMean).norm(), 1e-10);
		BOOST_CHECK_SMALL(
			(qrLatest.covariance - batchLatestCovariance).norm()
				/ batchLatestCovariance.norm(),
			1e-10);
		BOOST_CHECK_SMALL((qrLatest.mean - kalmanMean).norm(), 1e-10);
		BOOST_CHECK_SMALL(
			(qrLatest.covariance - kalmanCovariance).norm()
				/ kalmanCovariance.norm(),
			1e-10);
		const auto summary = incremental.summary();
		BOOST_REQUIRE(summary.valid);
		BOOST_CHECK_LE(summary.activeEpochs, 3);
		BOOST_CHECK_LE(summary.storedColumns, 6);
		BOOST_CHECK_LE(summary.storedRows, 6);
		BOOST_TEST_MESSAGE(
			"incremental-equivalence epochs=" << epochCount
			<< " mean_error=" << (qrLatest.mean - batchLatestMean).norm()
			<< " covariance_relative_error="
			<< (qrLatest.covariance - batchLatestCovariance).norm()
				/ batchLatestCovariance.norm()
			<< " stored=" << summary.storedRows << "x"
			<< summary.storedColumns);
	}
}

BOOST_AUTO_TEST_CASE(raw_square_root_boundary_matches_kalman_with_semidefinite_process)
{
	for (int epochCount : {2, 5, 10})
	{
		Vector2d mean(0.2, -0.4);
		Matrix2d covariance;
		covariance << 0.8, 0.15, 0.15, 0.6;
		ZhangIncrementalRawSquareRoot incremental;
		BOOST_REQUIRE(incremental.initialise(mean, covariance));
		double innovationSquaredNorm = 0;
		for (int epoch = 0; epoch < epochCount; epoch++)
		{
			Matrix2d design;
			design << 1.0, 0.35, -0.2, 1.0;
			Matrix2d measurementCovariance;
			measurementCovariance << 0.09, 0.01, 0.01, 0.16;
			Vector2d observation(
				0.3 + 0.04 * epoch, -0.1 + 0.02 * epoch);
			const Vector2d innovation = observation - design * mean;
			const Matrix2d innovationCovariance = design * covariance
				* design.transpose() + measurementCovariance;
			innovationSquaredNorm += innovation.dot(
				innovationCovariance.ldlt().solve(innovation));
			const Matrix2d gain = covariance * design.transpose()
				* innovationCovariance.inverse();
			mean += gain * innovation;
			covariance -= gain * design * covariance;
			covariance = 0.5 * (covariance + covariance.transpose());
			BOOST_REQUIRE(incremental.addAcceptedMeasurement(
				design, measurementCovariance, observation));
			VectorXd actualMean;
			MatrixXd actualCovariance;
			BOOST_REQUIRE(incremental.currentMarginal(
				actualMean, actualCovariance));
			BOOST_CHECK_SMALL((actualMean - mean).norm(), 1e-11);
			BOOST_CHECK_SMALL(
				(actualCovariance - covariance).norm() / covariance.norm(),
				1e-11);

			MatrixXd targetRow(1, 2);
			targetRow << 1, -1;
			VectorXd targetOffset(1);
			targetOffset << 3;
			const auto target = incremental.marginaliseTargets(
				targetRow, targetOffset);
			BOOST_REQUIRE_MESSAGE(target.valid, target.failureReason);
			BOOST_CHECK_SMALL(
				target.mean(0) - (mean(0) - mean(1) + 3), 1e-11);
			BOOST_CHECK_SMALL(
				target.covariance(0, 0)
					- (targetRow * covariance * targetRow.transpose())(0, 0),
				1e-11);

			if (epoch + 1 == epochCount)
			{
				continue;
			}
			Matrix2d transition;
			transition << 1, 0.1, 0, 1;
			Matrix2d processCovariance = Matrix2d::Zero();
			processCovariance(0, 0) = 0.01;
			mean = transition * mean;
			covariance = transition * covariance * transition.transpose()
				+ processCovariance;
			BOOST_REQUIRE(incremental.advance(
				transition, processCovariance));
			if (epoch == 2)
			{
				Matrix2d exact;
				exact << 1, 1, 0, 1;
				Vector2d shift(2, -1);
				mean = exact * mean + shift;
				covariance = exact * covariance * exact.transpose();
				BOOST_REQUIRE(incremental.applyExactCoordinateTransform(
					exact, shift));
			}
		}
		const auto summary = incremental.summary();
		BOOST_REQUIRE_MESSAGE(summary.valid, summary.failureReason);
		BOOST_CHECK_EQUAL(summary.batchOrthogonalDof, 2 * epochCount);
		BOOST_CHECK_SMALL(
			summary.batchOrthogonalSquaredNorm - innovationSquaredNorm, 1e-10);
		BOOST_CHECK_EQUAL(summary.storedRows, 2);
		BOOST_CHECK_EQUAL(summary.storedColumns, 2);
		BOOST_TEST_MESSAGE(
			"raw-square-root epochs=" << epochCount
			<< " orthogonal=" << summary.batchOrthogonalSquaredNorm
			<< "/" << summary.batchOrthogonalDof
			<< " stored=" << summary.storedRows << "x"
			<< summary.storedColumns);
	}
}

BOOST_AUTO_TEST_CASE(raw_square_root_boundary_preserves_deterministic_prior_subspace)
{
	Vector2d mean(0.5, -2.0);
	Matrix2d covariance = Matrix2d::Zero();
	covariance(0, 0) = 0.4;
	ZhangIncrementalRawSquareRoot incremental;
	BOOST_REQUIRE(incremental.initialise(mean, covariance));

	Matrix2d design;
	design << 1, 0.25, -0.4, 1;
	Matrix2d measurementCovariance = Matrix2d::Identity() * 0.1;
	Vector2d observation(0.2, -1.8);
	const Vector2d innovation = observation - design * mean;
	const Matrix2d innovationCovariance = design * covariance
		* design.transpose() + measurementCovariance;
	const Matrix2d gain = covariance * design.transpose()
		* innovationCovariance.inverse();
	mean += gain * innovation;
	covariance -= gain * design * covariance;
	covariance = 0.5 * (covariance + covariance.transpose());
	BOOST_REQUIRE(incremental.addAcceptedMeasurement(
		design, measurementCovariance, observation));
	VectorXd actualMean;
	MatrixXd actualCovariance;
	BOOST_REQUIRE(incremental.currentMarginal(actualMean, actualCovariance));
	BOOST_CHECK_SMALL((actualMean - mean).norm(), 1e-12);
	BOOST_CHECK_SMALL((actualCovariance - covariance).norm(), 1e-12);
	BOOST_CHECK_SMALL(actualMean(1) + 2.0, 1e-15);
	BOOST_CHECK_SMALL(actualCovariance.row(1).norm(), 1e-15);

	Matrix2d transition = Matrix2d::Identity();
	Matrix2d processCovariance = Matrix2d::Zero();
	processCovariance(1, 1) = 0.03;
	mean = transition * mean;
	covariance = transition * covariance * transition.transpose()
		+ processCovariance;
	BOOST_REQUIRE(incremental.advance(transition, processCovariance));
	BOOST_REQUIRE(incremental.currentMarginal(actualMean, actualCovariance));
	BOOST_CHECK_SMALL((actualMean - mean).norm(), 1e-12);
	BOOST_CHECK_SMALL((actualCovariance - covariance).norm(), 1e-12);
	BOOST_CHECK_EQUAL(incremental.summary().storedColumns, 2);
}

BOOST_AUTO_TEST_CASE(
	persistent_target_exact_constraint_survives_process_and_s_basis_change)
{
	Vector2d stateMean(0.4, -0.7);
	Matrix2d stateCovariance;
	stateCovariance << 0.8, 0.1, 0.1, 1.2;
	ZhangIncrementalRawSquareRoot window;
	BOOST_REQUIRE(window.initialise(stateMean, stateCovariance));

	// Augment a persistent target a=x0-x1+2 without adding stochastic rank.
	MatrixXd augment = MatrixXd::Zero(3, 2);
	augment.topRows(2) = Matrix2d::Identity();
	augment.row(2) << 1, -1;
	Vector3d translation(0, 0, 2);
	BOOST_REQUIRE(window.applyExactCoordinateTransform(augment, translation));
	VectorXd mean;
	MatrixXd covariance;
	BOOST_REQUIRE(window.currentMarginal(mean, covariance));
	Vector3d relation(-1, 1, 1);
	BOOST_CHECK_SMALL(relation.dot(mean) - 2, 1e-12);
	BOOST_CHECK_SMALL(
		(relation.transpose() * covariance * relation)(0, 0), 1e-12);

	// State process noise would let the current state functional drift, while
	// the persistent target coordinate itself is carried with zero process
	// noise.  The next exact physical-functional factor reconnects them.
	Matrix3d transition = Matrix3d::Identity();
	Matrix3d processCovariance = Matrix3d::Zero();
	processCovariance(0, 0) = 0.1;
	BOOST_REQUIRE(window.advance(transition, processCovariance));
	BOOST_REQUIRE(window.currentMarginal(mean, covariance));
	BOOST_CHECK_GT(
		(relation.transpose() * covariance * relation)(0, 0), 0.09);
	BOOST_REQUIRE(window.applyExactConstraint(
		relation.transpose(), VectorXd::Constant(1, 2)));
	BOOST_REQUIRE(window.currentMarginal(mean, covariance));
	BOOST_CHECK_SMALL(relation.dot(mean) - 2, 1e-11);
	BOOST_CHECK_SMALL(
		std::abs((relation.transpose() * covariance * relation)(0, 0)),
		1e-11);

	// Pure S-basis coordinate change: x0'=x0+x1, x1'=x1, a'=a.
	Matrix3d basisTransform = Matrix3d::Identity();
	basisTransform(0, 1) = 1;
	BOOST_REQUIRE(window.applyExactCoordinateTransform(basisTransform));
	Vector3d transformedRelation(-1, 2, 1);
	BOOST_REQUIRE(window.applyExactConstraint(
		transformedRelation.transpose(), VectorXd::Constant(1, 2)));
	BOOST_REQUIRE(window.currentMarginal(mean, covariance));
	BOOST_CHECK_SMALL(transformedRelation.dot(mean) - 2, 1e-11);
	BOOST_CHECK_SMALL(
		std::abs((transformedRelation.transpose()
			* covariance * transformedRelation)(0, 0)), 1e-11);
	BOOST_CHECK_EQUAL(window.summary().exactConstraintsApplied, 2);
}

BOOST_AUTO_TEST_CASE(
	persistent_raw_target_variable_is_constant_across_s_basis_and_versions_reset)
{
	Vector2d mean(0.4, -0.7);
	Matrix2d covariance;
	covariance << 0.8, 0.1, 0.1, 1.2;
	ZhangPersistentRawTargetWindow window;
	BOOST_REQUIRE(window.initialise(mean, covariance));
	Matrix2d design;
	design << 1, 0.2, -0.3, 1;
	Matrix2d measurementCovariance = 0.1 * Matrix2d::Identity();
	Vector2d observation(0.1, -0.2);
	BOOST_REQUIRE(window.addAcceptedMeasurement(
		design, measurementCovariance, observation));
	Vector2d targetRow(1, -1);
	BOOST_REQUIRE(window.bindTarget(
		"GPS:WL:G01->G03", "G01:0->G03:0", targetRow, 2, 1));
	const auto initial = window.targetMarginal();
	BOOST_REQUIRE_MESSAGE(initial.valid, initial.failureReason);
	BOOST_REQUIRE_EQUAL(initial.targetCount, 1);

	Matrix2d transition = Matrix2d::Identity();
	Matrix2d processCovariance = Matrix2d::Zero();
	processCovariance(0, 0) = 0.05;
	BOOST_REQUIRE(window.advance(transition, processCovariance));
	BOOST_REQUIRE(window.addAcceptedMeasurement(
		design, measurementCovariance, observation));
	BOOST_REQUIRE(window.bindTarget(
		"GPS:WL:G01->G03", "G01:0->G03:0", targetRow, 2, 2));
	const auto constrained = window.targetMarginal();
	BOOST_REQUIRE_MESSAGE(constrained.valid, constrained.failureReason);

	Matrix2d basisTransform;
	basisTransform << 1, 1, 0, 1;
	BOOST_REQUIRE(window.applyExactCoordinateTransform(basisTransform));
	Vector2d transformedTargetRow(1, -2);
	BOOST_REQUIRE(window.bindTarget(
		"GPS:WL:G01->G03", "G01:0->G03:0",
		transformedTargetRow, 2, 3));
	const auto transformed = window.targetMarginal();
	BOOST_REQUIRE_MESSAGE(transformed.valid, transformed.failureReason);
	BOOST_CHECK_SMALL(
		transformed.mean(0) - constrained.mean(0), 1e-11);
	BOOST_CHECK_SMALL(
		transformed.covariance(0, 0) - constrained.covariance(0, 0),
		1e-11);
	BOOST_CHECK_EQUAL(transformed.identities[0], "GPS:WL:G01->G03");
	BOOST_CHECK_EQUAL(transformed.physicalVersions[0], "G01:0->G03:0");

	BOOST_CHECK(!window.bindTarget(
		"GPS:WL:G01->G03", "G01:1->G03:0",
		transformedTargetRow, 2, 4));
	BOOST_CHECK_EQUAL(
		window.lastFailureReason(),
		"PERSISTENT_RAW_TARGET_PHYSICAL_VERSION_CHANGED");
}

BOOST_AUTO_TEST_CASE(incremental_fixed_lag_eliminates_epoch_local_nuisance_immediately)
{
	VectorXd boundaryMean = VectorXd::Constant(1, 0.2);
	MatrixXd boundaryCovariance = MatrixXd::Constant(1, 1, 0.8);
	MatrixXd separatorDesign(3, 1);
	separatorDesign << 1.0, 0.7, -0.4;
	MatrixXd localDesign(3, 1);
	localDesign << 1.0, -1.0, 0.5;
	MatrixXd measurementCovariance = MatrixXd::Zero(3, 3);
	measurementCovariance.diagonal() << 0.09, 0.16, 0.25;
	VectorXd observation(3);
	observation << 0.31, -0.08, 0.17;

	ZhangIncrementalFixedLagSquareRoot incremental(2);
	BOOST_REQUIRE(incremental.initialise(boundaryMean, boundaryCovariance));
	BOOST_REQUIRE(incremental.addLatestMeasurement(
		separatorDesign, localDesign, measurementCovariance, observation));
	const auto summary = incremental.summary();
	BOOST_REQUIRE(summary.valid);
	BOOST_CHECK_EQUAL(summary.storedColumns, 1);
	BOOST_CHECK_EQUAL(summary.storedRows, 1);

	const double priorWeight = 1 / std::sqrt(boundaryCovariance(0, 0));
	MatrixXd denseFactor = MatrixXd::Zero(4, 2); // [local, separator]
	denseFactor(0, 1) = priorWeight;
	VectorXd denseRhs = VectorXd::Zero(4);
	denseRhs(0) = priorWeight * boundaryMean(0);
	const MatrixXd measurementWeight = measurementCovariance.llt()
		.matrixL().solve(MatrixXd::Identity(3, 3));
	denseFactor.bottomLeftCorner(3, 1) = measurementWeight * localDesign;
	denseFactor.bottomRightCorner(3, 1) = measurementWeight * separatorDesign;
	denseRhs.tail(3) = measurementWeight * observation;
	const auto dense = zhangMarginaliseSquareRootFactors(
		denseFactor.sparseView(), denseRhs, 1);
	const auto current = incremental.latestMarginal();
	BOOST_REQUIRE_MESSAGE(dense.valid, dense.failureReason);
	BOOST_REQUIRE_MESSAGE(current.valid, current.failureReason);
	BOOST_CHECK_SMALL((current.mean - dense.mean).norm(), 1e-11);
	BOOST_CHECK_SMALL(
		(current.covariance - dense.covariance).norm()
			/ dense.covariance.norm(),
		1e-11);
}

BOOST_AUTO_TEST_CASE(incremental_exact_s_basis_change_preserves_physical_posterior)
{
	Vector2d mean(0.4, -0.3);
	Matrix2d covariance;
	covariance << 0.7, 0.1, 0.1, 0.5;
	ZhangIncrementalFixedLagSquareRoot incremental(2);
	BOOST_REQUIRE(incremental.initialise(mean, covariance));
	const auto before = incremental.latestMarginal();
	BOOST_REQUIRE_MESSAGE(before.valid, before.failureReason);
	Matrix2d transform;
	transform << 1, 1, 0, 1;
	Vector2d translation(3, -2);
	BOOST_REQUIRE(incremental.applyExactLatestCoordinateTransform(
		transform, translation));
	const auto after = incremental.latestMarginal();
	BOOST_REQUIRE_MESSAGE(after.valid, after.failureReason);
	BOOST_CHECK_SMALL(
		(after.mean - (transform * before.mean + translation)).norm(), 1e-11);
	const Matrix2d expectedCovariance = transform * before.covariance
		* transform.transpose();
	BOOST_CHECK_SMALL(
		(after.covariance - expectedCovariance).norm()
			/ expectedCovariance.norm(),
		1e-11);
}

BOOST_AUTO_TEST_CASE(residual_domains_keep_independent_dof_and_project_integer_gauge)
{
	Vector2d priorMean(0.1, -0.2);
	Matrix2d priorCovariance;
	priorCovariance << 0.8, 0.1, 0.1, 0.6;
	Matrix2d design;
	design << 1.0, 0.2, -0.3, 1.0;
	Matrix2d measurementCovariance;
	measurementCovariance << 0.09, 0.01, 0.01, 0.16;
	Vector2d observation(0.24, -0.17);
	const auto prefit = zhangPrefitInnovationStatistic(
		observation, design, measurementCovariance,
		priorMean, priorCovariance);
	BOOST_REQUIRE_MESSAGE(prefit.valid, prefit.failureReason);
	BOOST_CHECK_EQUAL(prefit.dof, 2);

	MatrixXd batchDesign(5, 2);
	batchDesign << 1, 0, 0, 1, 1, 1, 0.5, -0.2, -0.3, 0.7;
	VectorXd batchRhs(5);
	batchRhs << 0.2, -0.1, 0.13, 0.09, -0.04;
	const auto orthogonal = zhangBatchOrthogonalResidualStatistic(
		batchDesign, batchRhs);
	BOOST_REQUIRE_MESSAGE(orthogonal.valid, orthogonal.failureReason);
	BOOST_CHECK_EQUAL(orthogonal.dof, 3);

	const Matrix2d innovation = design * priorCovariance
		* design.transpose() + measurementCovariance;
	const Matrix2d gain = priorCovariance * design.transpose()
		* innovation.inverse();
	const Vector2d posteriorMean = priorMean
		+ gain * (observation - design * priorMean);
	const Matrix2d posteriorCovariance = priorCovariance
		- gain * design * priorCovariance;
	Vector2d heldOutObservation(0.18, -0.11);
	const auto heldOut = zhangHeldOutPredictionStatistic(
		heldOutObservation, design, measurementCovariance,
		posteriorMean, posteriorCovariance);
	BOOST_REQUIRE_MESSAGE(heldOut.valid, heldOut.failureReason);
	BOOST_CHECK_EQUAL(heldOut.dof, 2);

	Vector2d targetMean(12.18, -3.81);
	Matrix2d targetCovariance;
	targetCovariance << 0.04, 0.01, 0.01, 0.09;
	Vector2d candidate(12, -4);
	MatrixXd quotientDirection(2, 1);
	quotientDirection << 1, 1;
	const auto integerDistance = zhangTargetToIntegerStatistic(
		targetMean, targetCovariance, candidate, quotientDirection);
	BOOST_REQUIRE_MESSAGE(
		integerDistance.valid, integerDistance.failureReason);
	BOOST_CHECK_EQUAL(integerDistance.removedGaugeRank, 1);
	BOOST_CHECK_EQUAL(integerDistance.dof, 1);
	const auto gaugeShifted = zhangTargetToIntegerStatistic(
		targetMean + 7 * quotientDirection.col(0),
		targetCovariance, candidate, quotientDirection);
	BOOST_REQUIRE(gaugeShifted.valid);
	BOOST_CHECK_SMALL(
		integerDistance.squaredNorm - gaugeShifted.squaredNorm, 1e-12);
	BOOST_TEST_MESSAGE(
		"residual-dof prefit=" << prefit.dof
		<< " batch_orthogonal=" << orthogonal.dof
		<< " held_out=" << heldOut.dof
		<< " integer_quotient=" << integerDistance.dof
		<< " target_distance=" << integerDistance.squaredNorm);
}

BOOST_AUTO_TEST_CASE(generic_primitive_integer_targets_do_not_require_wide_lane)
{
	MatrixXd baseRows = MatrixXd::Identity(2, 2);
	Vector2d baseMean(12.08, -3.96);
	Matrix2d baseCovariance;
	baseCovariance << 0.018, 0.006, 0.006, 0.025;

	const ZhangIntegerMatrix direct = zhangDirectJointIntegerTransform(2);
	const ZhangIntegerMatrix wideLaneL1 = zhangWideLaneL1IntegerTransform();
	const auto directTargets = zhangTransformIntegerTargets(
		baseRows, baseMean, baseCovariance, direct);
	const auto wlTargets = zhangTransformIntegerTargets(
		baseRows, baseMean, baseCovariance, wideLaneL1);
	BOOST_REQUIRE_MESSAGE(directTargets.valid, directTargets.failureReason);
	BOOST_REQUIRE_MESSAGE(wlTargets.valid, wlTargets.failureReason);
	BOOST_CHECK(directTargets.audit.unimodular);
	BOOST_CHECK(wlTargets.audit.unimodular);
	BOOST_CHECK_SMALL(wlTargets.mean(0) - (baseMean(0) - baseMean(1)), 1e-15);
	BOOST_CHECK_SMALL(wlTargets.mean(1) - baseMean(0), 1e-15);

	// Representative unimodular decorrelation returned by an integer solver.
	ZhangIntegerMatrix lambdaTransform(2, 2);
	lambdaTransform << 1, 0, -2, 1;
	const auto lambdaTargets = zhangTransformIntegerTargets(
		baseRows, baseMean, baseCovariance, lambdaTransform);
	BOOST_REQUIRE_MESSAGE(lambdaTargets.valid, lambdaTargets.failureReason);
	BOOST_CHECK(lambdaTargets.audit.unimodular);

	ZhangIntegerMatrix nonPrimitive = ZhangIntegerMatrix::Zero(2, 2);
	nonPrimitive.diagonal() << 2, 1;
	const auto rejected = zhangAuditPrimitiveIntegerTransform(nonPrimitive);
	BOOST_CHECK(!rejected.valid);
	BOOST_CHECK(!rejected.primitive);

	// All complete primitive coordinates recover the same base pair exactly.
	const Matrix2d wlInverse = wideLaneL1.cast<double>().transpose().inverse();
	BOOST_CHECK_SMALL(
		(wlInverse * wlTargets.mean - directTargets.mean).norm(), 1e-14);
	BOOST_CHECK_SMALL(
		(wlInverse * wlTargets.covariance * wlInverse.transpose()
			- directTargets.covariance).norm(),
		1e-14);
}

BOOST_AUTO_TEST_CASE(integer_quotient_and_wide_lane_coordinates_are_primitive)
{
	const std::vector<std::string> identities = {
		"GPS:K1_L1C:G01:G03|arc-a",
		"GPS:K2_L2W:G01:G03|arc-b",
		"GPS:K1_L1C:G01:G02|arc-c",
		"GPS:K2_L2W:G01:G02|arc-d",
		"GPS:K1_L1C:G01:G05|arc-e",
		"GPS:K2_L2W:G01:G05|arc-f"};
	const std::vector<std::string> gauges = {
		"GPS:K1", "GPS:K2", "GPS:K1", "GPS:K2", "GPS:K1", "GPS:K2"};
	const std::vector<bool> absolute(6, false);
	VectorXd mean(6);
	mean << 9.4, 0.6, -177.6, -24.1, -40.7, 40.0;
	MatrixXd covariance = MatrixXd::Identity(6, 6);
	const auto quotient = zhangBuildIntegerQuotientCoordinates(
		identities, gauges, absolute, mean, covariance);
	BOOST_REQUIRE_MESSAGE(quotient.valid, quotient.failureReason);
	BOOST_CHECK_EQUAL(quotient.transform.rows(), 6);
	BOOST_CHECK_EQUAL(quotient.transform.cols(), 4);
	BOOST_CHECK(zhangAuditPrimitiveIntegerTransform(quotient.transform).valid);
	BOOST_CHECK_SMALL(quotient.mean(0) - (mean(2) - mean(0)), 1e-14);
	BOOST_CHECK_SMALL(quotient.mean(1) - (mean(4) - mean(0)), 1e-14);
	BOOST_CHECK_SMALL(quotient.mean(2) - (mean(3) - mean(1)), 1e-14);
	BOOST_CHECK_SMALL(quotient.mean(3) - (mean(5) - mean(1)), 1e-14);

	const auto wideLane = zhangBuildWideLaneL1BlockCoordinates(quotient);
	BOOST_REQUIRE_MESSAGE(wideLane.valid, wideLane.failureReason);
	const auto audit = zhangAuditPrimitiveIntegerTransform(wideLane.transform);
	BOOST_CHECK(audit.valid);
	BOOST_CHECK(audit.unimodular);
	const MatrixXd transform = wideLane.transform.cast<double>();
	const VectorXd transformed = transform.transpose() * quotient.mean;
	BOOST_CHECK_SMALL(transformed(0) - (quotient.mean(0) - quotient.mean(2)), 1e-14);
	BOOST_CHECK_SMALL(transformed(1) - quotient.mean(0), 1e-14);
}

BOOST_AUTO_TEST_CASE(production_canonical_separator_identities_pair_l1c_l2w)
{
	const std::vector<std::string> identities = {
		"GPS:L1C:G01->G03|datum=GPS:L1C:G01->G03:V0|phase=G01:0->G03:0",
		"GPS:L2W:G01->G03|datum=GPS:L2W:G01->G03:V0|phase=G01:0->G03:0",
		"GPS:L1C:G01->G02|datum=GPS:L1C:G01->G02:V0|phase=G01:0->G02:0",
		"GPS:L2W:G01->G02|datum=GPS:L2W:G01->G02:V0|phase=G01:0->G02:0",
		"GPS:L1C:G01->G05|datum=GPS:L1C:G01->G05:V0|phase=G01:0->G05:0",
		"GPS:L2W:G01->G05|datum=GPS:L2W:G01->G05:V0|phase=G01:0->G05:0"};
	const std::vector<std::string> gauges = {
		"GPS:K1", "GPS:K2", "GPS:K1", "GPS:K2", "GPS:K1", "GPS:K2"};
	const std::vector<bool> absolute(6, false);
	VectorXd mean(6);
	mean << 9.4, 0.6, -177.6, -24.1, -40.7, 40.0;
	const MatrixXd covariance = MatrixXd::Identity(6, 6);
	const auto quotient = zhangBuildIntegerQuotientCoordinates(
		identities, gauges, absolute, mean, covariance);
	BOOST_REQUIRE_MESSAGE(quotient.valid, quotient.failureReason);
	BOOST_REQUIRE_EQUAL(quotient.relations.size(), 4);
	BOOST_CHECK_EQUAL(quotient.relations[0], "G03->G02");
	BOOST_CHECK_EQUAL(quotient.relations[1], "G03->G05");
	BOOST_CHECK_EQUAL(quotient.relations[2], "G03->G02");
	BOOST_CHECK_EQUAL(quotient.relations[3], "G03->G05");
	const auto wideLane = zhangBuildWideLaneL1BlockCoordinates(quotient);
	BOOST_REQUIRE_MESSAGE(wideLane.valid, wideLane.failureReason);
	BOOST_CHECK(zhangAuditPrimitiveIntegerTransform(wideLane.transform).unimodular);
}

BOOST_AUTO_TEST_CASE(par_subset_selection_uses_joint_covariance)
{
	Matrix3d covariance;
	covariance <<
		0.004, 0.001, 0,
		0.001, 0.006, 0,
		0, 0, 4.0;
	double success = 0;
	const auto subset = zhangSelectParSubset(covariance, 0.99, &success);
	BOOST_REQUIRE_EQUAL(subset.size(), 2);
	BOOST_CHECK_EQUAL(subset[0], 0);
	BOOST_CHECK_EQUAL(subset[1], 1);
	BOOST_CHECK_GE(success, 0.99);
}

BOOST_AUTO_TEST_CASE(lambda_reduction_diagnostics_require_conditional_variances)
{
	Matrix2d covariance;
	covariance << 0.84, 0.24, 0.24, 1.38;
	Matrix2d transform;
	transform << 1, 1, 0, 1;
	const Matrix2d reduced =
		transform.transpose() * covariance * transform;
	Eigen::LDLT<Matrix2d> conditionalFactor(reduced);
	BOOST_REQUIRE_EQUAL(conditionalFactor.info(), Eigen::Success);
	Vector2d best(9, 73);
	Vector2d second(9, 72);
	const auto diagnostics = zhangAuditLambdaReduction(
		covariance, transform, reduced, conditionalFactor.vectorD(),
		best, second);
	BOOST_REQUIRE_MESSAGE(diagnostics.valid, diagnostics.failureReason);
	BOOST_CHECK(diagnostics.transformUnimodular);
	BOOST_CHECK(diagnostics.candidateBackTransformConsistent);
	BOOST_CHECK_SMALL(diagnostics.covarianceTransformMaximumError, 1e-12);
	BOOST_CHECK_SMALL(diagnostics.conditionalDeterminantLogError, 1e-12);
	BOOST_CHECK_SMALL(diagnostics.bestCandidateBackTransformMaximumError, 1e-12);
	BOOST_CHECK_SMALL(diagnostics.reducedCandidateIntegerMaximumError, 1e-12);
	BOOST_CHECK_CLOSE(
		diagnostics.ambiguityDilutionOfPrecision,
		std::pow(covariance.determinant(), 0.25), 1e-10);
	BOOST_CHECK_CLOSE(
		diagnostics.jointBootstrappedSuccessRate,
		diagnostics.conditionalSuccessRates.prod(), 1e-10);

	// The marginal diagonal is not the LAMBDA conditional D for correlated
	// coordinates and must fail the determinant-consistency audit.
	const auto marginalMisuse = zhangAuditLambdaReduction(
		covariance, transform, reduced, reduced.diagonal(), best, second);
	BOOST_CHECK(!marginalMisuse.valid);
	BOOST_CHECK_EQUAL(
		marginalMisuse.failureReason, "INCONSISTENT_LAMBDA_REDUCTION");
}

BOOST_AUTO_TEST_CASE(lambda_par_diagnostics_report_joint_ranks_candidates_and_closure)
{
	Vector3d floatMean(5.04, -1.97, -3.02);
	Matrix3d covariance;
	covariance <<
		0.006, 0.001, -0.0005,
		0.001, 0.009, 0.0015,
		-0.0005, 0.0015, 0.012;
	ZhangIntegerVector best(3);
	best << 5, -2, -3;
	ZhangIntegerVector second(3);
	second << 5, -2, -2;
	MatrixXd relationDesign(2, 3);
	relationDesign << 1, -1, 0, 0, 1, -1;
	MatrixXd closureDesign(1, 3);
	closureDesign << 1, 1, 1;
	const auto diagnostics = zhangEvaluateLambdaParCandidates(
		floatMean, covariance, best, second,
		3, 2, relationDesign, closureDesign, 0.99);
	BOOST_REQUIRE_MESSAGE(diagnostics.valid, diagnostics.failureReason);
	BOOST_CHECK_EQUAL(diagnostics.quotientValidRank, 3);
	BOOST_CHECK_EQUAL(diagnostics.absoluteValidRank, 2);
	BOOST_CHECK_EQUAL(diagnostics.productRelationGraphRank, 2);
	BOOST_CHECK_EQUAL(diagnostics.recoverableSatelliteCount, 3);
	BOOST_CHECK_GT(diagnostics.secondCandidateDistance,
		diagnostics.bestCandidateDistance);
	BOOST_CHECK_GT(diagnostics.distanceRatio, 1);
	BOOST_CHECK_EQUAL(diagnostics.maximumCycleClosureError, 0);
	BOOST_CHECK_GT(diagnostics.parTargetCount, 0);
	BOOST_CHECK_LE(diagnostics.parTargetCount, 3);
	BOOST_TEST_MESSAGE(
		"integer-diagnostics quotient_rank="
		<< diagnostics.quotientValidRank
		<< " absolute_rank=" << diagnostics.absoluteValidRank
		<< " graph_rank=" << diagnostics.productRelationGraphRank
		<< " best=" << diagnostics.bestCandidateDistance
		<< " second=" << diagnostics.secondCandidateDistance
		<< " ratio=" << diagnostics.distanceRatio
		<< " joint_success=" << diagnostics.jointBootstrappedSuccessRate
		<< " par_targets=" << diagnostics.parTargetCount
		<< " par_success=" << diagnostics.parBootstrappedSuccessRate
		<< " recoverable_satellites="
		<< diagnostics.recoverableSatelliteCount
		<< " closure=" << diagnostics.maximumCycleClosureError);
}

BOOST_AUTO_TEST_CASE(retained_target_whitening_projects_shared_quotient_direction)
{
	KFKey first;
	first.type = KF::SAT_CLOCK;
	first.Sat = SatSys(E_Sys::GPS, 1);
	KFKey second = first;
	second.Sat = SatSys(E_Sys::GPS, 2);
	const std::vector<ZhangCapturedStateKey> keys = {
		zhangCapturedStateKey(first), zhangCapturedStateKey(second)};
	Vector2d priorMean = Vector2d::Zero();
	Matrix2d priorCovariance = Matrix2d::Identity();
	KFMeas measurement;
	measurement.H = Matrix2d::Identity();
	measurement.V = Vector2d(0.2, -0.1);
	measurement.R = Matrix2d::Identity();
	measurement.obsKeys = {first, second};
	const Vector2d posteriorMean = 0.5 * measurement.V;
	const Matrix2d posteriorCovariance = 0.5 * Matrix2d::Identity();
	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, priorMean, priorCovariance, measurement, "/PPP",
		posteriorMean, posteriorCovariance));
	Vector2d firstRow(1, 0);
	Vector2d secondRow(0, 1);
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K1", "arc-a", "segment-a", {{"arc-a", 0}},
		keys, firstRow, 0, posteriorMean, posteriorCovariance,
		1, "GPS:L1L2:COMPONENT-0"));
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K2", "arc-b", "segment-b", {{"arc-b", 0}},
		keys, secondRow, 0, posteriorMean, posteriorCovariance,
		1, "GPS:L1L2:COMPONENT-0"));
	const auto& block = capture.currentRetainedBlock();
	BOOST_REQUIRE_MESSAGE(block.valid, block.failureReason);
	BOOST_CHECK_EQUAL(block.targetCount, 2);
	BOOST_CHECK_EQUAL(block.informationRank, 2);
	BOOST_CHECK_EQUAL(block.projectedGaugeRank, 1);
	BOOST_CHECK_EQUAL(block.residualDof, 1);
	BOOST_CHECK_EQUAL(block.whitenedResidual.size(), 1);
	const auto originalSeparatorIdentities = block.separatorIdentities;

	// A disjoint exact raw-arc representation with the same physical phase
	// segments is a coordinate continuation, not a new separator identity.
	KFMeas secondMeasurement = measurement;
	secondMeasurement.V = Vector2d::Zero();
	const Matrix2d secondPosteriorCovariance =
		(1.0 / 3.0) * Matrix2d::Identity();
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, posteriorMean, posteriorCovariance, secondMeasurement,
		"/PPP", posteriorMean, secondPosteriorCovariance));
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K1", "coordinate-c", "segment-a", {{"arc-c", 0}},
		keys, firstRow, 0, posteriorMean, secondPosteriorCovariance,
		1, "GPS:L1L2:COMPONENT-0"));
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K2", "coordinate-d", "segment-b", {{"arc-d", 0}},
		keys, secondRow, 0, posteriorMean, secondPosteriorCovariance,
		1, "GPS:L1L2:COMPONENT-0"));
	const auto continuedIdentities =
		capture.currentRetainedBlock().separatorIdentities;
	BOOST_CHECK(continuedIdentities == originalSeparatorIdentities);

	// A version change on the same physical arc is a hard window boundary.  It
	// must not be appended as a new separator to the old chronology.
	KFMeas thirdMeasurement = measurement;
	thirdMeasurement.V = Vector2d::Zero();
	const Matrix2d thirdPosteriorCovariance =
		0.25 * Matrix2d::Identity();
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, posteriorMean, secondPosteriorCovariance,
		thirdMeasurement, "/PPP", posteriorMean, thirdPosteriorCovariance));
	BOOST_CHECK(!capture.recordPhysicalTarget(
		GTime(), "K1", "coordinate-c", "segment-a", {{"arc-c", 1}},
		keys, firstRow, 0, posteriorMean, thirdPosteriorCovariance,
		1, "GPS:L1L2:COMPONENT-0"));
	BOOST_CHECK_EQUAL(
		capture.lastTargetReason(),
		"PERSISTENT_RAW_TARGET_PHYSICAL_VERSION_CHANGED");
	BOOST_CHECK_EQUAL(capture.capturedPhysicalTargets().size(), 4);
	capture.resetForPhysicalArcChange();
	BOOST_CHECK(!capture.summary().valid);
}

BOOST_AUTO_TEST_CASE(raw_window_never_promotes_unresolved_peer_to_absolute_datum)
{
	KFKey first;
	first.type = KF::SAT_CLOCK;
	first.Sat = SatSys(E_Sys::GPS, 1);
	KFKey second = first;
	second.Sat = SatSys(E_Sys::GPS, 2);
	const std::vector<ZhangCapturedStateKey> keys = {
		zhangCapturedStateKey(first), zhangCapturedStateKey(second)};
	const Vector2d priorMean = Vector2d::Zero();
	const Matrix2d priorCovariance = Matrix2d::Identity();
	KFMeas measurement;
	measurement.H = Matrix2d::Identity();
	measurement.V = Vector2d(0.1, -0.2);
	measurement.R = Matrix2d::Identity();
	measurement.obsKeys = {first, second};
	const Vector2d posteriorMean = 0.5 * measurement.V;
	const Matrix2d posteriorCovariance = 0.5 * Matrix2d::Identity();
	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, priorMean, priorCovariance, measurement, "/PPP",
		posteriorMean, posteriorCovariance));
	const Vector2d firstRow(1, 0);
	const Vector2d secondRow(0, 1);
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "GPS:K1_L1C:G01:G02", "arc-a", "G01:0->G02:0",
		{{"arc-a", 0}}, keys, firstRow, 0,
		posteriorMean, posteriorCovariance,
		0, "", "GPS:L1C:G01->G02", "GPS:L1C:G01->G02:V0", 0));
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "GPS:K1_L1C:G01:G03", "arc-b", "G01:0->G03:0",
		{{"arc-b", 0}}, keys, secondRow, 0,
		posteriorMean, posteriorCovariance,
		1, "GPS:K1_L1C:CANONICAL", "GPS:L1C:G01->G03",
		"GPS:L1C:G01->G03:V0", 0));
	const auto marginal = capture.currentRawSquareRootTargetMarginal();
	BOOST_REQUIRE_MESSAGE(marginal.valid, marginal.failureReason);
	BOOST_REQUIRE_EQUAL(marginal.absoluteValidity.size(), 2);
	BOOST_CHECK(marginal.absoluteValidity[0]);
	BOOST_CHECK(!marginal.absoluteValidity[1]);
	BOOST_CHECK_EQUAL(marginal.absoluteValidRank, 1);
	BOOST_CHECK_EQUAL(marginal.unresolvedGaugeRank, 1);
	BOOST_CHECK_EQUAL(marginal.quotientValidRank, 1);
}

BOOST_AUTO_TEST_CASE(single_unresolved_target_is_valid_zero_dof_block)
{
	KFKey key;
	key.type = KF::PHASE_BIAS;
	key.Sat = SatSys(E_Sys::GPS, 1);
	const std::vector<ZhangCapturedStateKey> keys = {
		zhangCapturedStateKey(key)};
	KFMeas measurement;
	measurement.H = MatrixXd::Identity(1, 1);
	measurement.V = VectorXd::Constant(1, 0.1);
	measurement.R = MatrixXd::Identity(1, 1);
	measurement.obsKeys = {key};
	const VectorXd priorMean = VectorXd::Zero(1);
	const MatrixXd priorCovariance = MatrixXd::Identity(1, 1);
	const VectorXd posteriorMean = VectorXd::Constant(1, 0.05);
	const MatrixXd posteriorCovariance = MatrixXd::Constant(1, 1, 0.5);
	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, priorMean, priorCovariance, measurement, "/PPP",
		posteriorMean, posteriorCovariance));
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K1", "arc-a", "G01:0->G02:0", {{"arc-a", 0}},
		keys, VectorXd::Ones(1), 0, posteriorMean, posteriorCovariance,
		1, "GPS:L1C:CANONICAL", "GPS:L1C:G01->G02",
		"GPS:L1C:G01->G02:V0", 0));
	const auto& block = capture.currentRetainedBlock();
	BOOST_REQUIRE(block.valid);
	BOOST_CHECK_EQUAL(block.targetCount, 1);
	BOOST_CHECK_EQUAL(block.informationRank, 1);
	BOOST_CHECK_EQUAL(block.residualDof, 0);
	BOOST_CHECK_EQUAL(block.projectedGaugeRank, 1);
	BOOST_CHECK(!block.likelihoodValid);
	BOOST_CHECK_SMALL(block.whitenedSquaredNorm, 1e-15);
}

BOOST_AUTO_TEST_CASE(persistent_canonical_functional_survives_temporary_target_loss)
{
	KFKey first;
	first.type = KF::PHASE_BIAS;
	first.Sat = SatSys(E_Sys::GPS, 1);
	KFKey second = first;
	second.Sat = SatSys(E_Sys::GPS, 2);
	const std::vector<ZhangCapturedStateKey> keys = {
		zhangCapturedStateKey(first), zhangCapturedStateKey(second)};
	KFMeas measurement;
	measurement.H = Matrix2d::Identity();
	measurement.V = Vector2d(0.2, -0.1);
	measurement.R = Matrix2d::Identity();
	measurement.obsKeys = {first, second};
	const Vector2d priorMean = Vector2d::Zero();
	const Matrix2d priorCovariance = Matrix2d::Identity();
	const Vector2d posteriorMean = 0.5 * measurement.V;
	const Matrix2d posteriorCovariance = 0.5 * Matrix2d::Identity();
	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, priorMean, priorCovariance, measurement, "/PPP",
		posteriorMean, posteriorCovariance));
	const Vector2d firstRow(1, 0);
	const Vector2d secondRow(0, 1);
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K1", "coordinate-a", "G01:0->G02:0",
		{{"arc-a", 0}}, keys, firstRow, 0,
		posteriorMean, posteriorCovariance,
		1, "GPS:L1C:CANONICAL", "GPS:L1C:G01->G02",
		"GPS:L1C:G01->G02:V0", 0));
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K2", "coordinate-b", "G01:0->G03:0",
		{{"arc-b", 0}}, keys, secondRow, 0,
		posteriorMean, posteriorCovariance,
		1, "GPS:L1C:CANONICAL", "GPS:L1C:G01->G03",
		"GPS:L1C:G01->G03:V0", 0));

	SparseMatrix<double> transform(2, 2);
	transform.insert(0, 0) = 1;
	transform.insert(0, 1) = 1;
	transform.insert(1, 1) = 1;
	BOOST_REQUIRE(capture.recordCoordinateTransform(
		GTime(), keys, keys, transform, "synthetic tree exchange"));
	const Matrix2d denseTransform = MatrixXd(transform);
	const Vector2d transformedPrior = denseTransform * posteriorMean;
	const Matrix2d transformedPriorCovariance = denseTransform
		* posteriorCovariance * denseTransform.transpose();
	KFMeas secondMeasurement = measurement;
	secondMeasurement.V = Vector2d(0.01, -0.02);
	const Matrix2d innovationCovariance = transformedPriorCovariance
		+ secondMeasurement.R;
	const Matrix2d gain = transformedPriorCovariance
		* innovationCovariance.inverse();
	const Vector2d secondPosteriorMean = transformedPrior
		+ gain * secondMeasurement.V;
	Matrix2d secondPosteriorCovariance = transformedPriorCovariance
		- gain * transformedPriorCovariance;
	secondPosteriorCovariance = 0.5
		* (secondPosteriorCovariance + secondPosteriorCovariance.transpose());
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, transformedPrior, transformedPriorCovariance,
		secondMeasurement, "/PPP", secondPosteriorMean,
		secondPosteriorCovariance));

	// Only K1 is rebuilt in the new S-basis.  K2 must remain active through its
	// transported canonical functional; it must not be retired or replaced.
	const Vector2d transformedFirstRow(1, -1);
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K1", "coordinate-c", "G01:0->G02:0",
		{{"arc-c", 0}}, keys, transformedFirstRow, 0,
		secondPosteriorMean, secondPosteriorCovariance,
		1, "GPS:L1C:CANONICAL", "GPS:L1C:G01->G02",
		"GPS:L1C:G01->G02:V0", 0));
	const auto raw = capture.currentRawSquareRootTargetMarginal();
	BOOST_REQUIRE_MESSAGE(raw.valid, raw.failureReason);
	BOOST_CHECK_EQUAL(raw.requestedTargetCount, 2);
	BOOST_CHECK_EQUAL(raw.unresolvedGaugeRank, 1);
	BOOST_CHECK_EQUAL(raw.quotientValidRank, 1);
	const auto& retained = capture.currentRetainedBlock();
	BOOST_REQUIRE_EQUAL(retained.targetCount, 2);
	const auto incremental = capture.currentIncrementalTargetMarginal();
	BOOST_REQUIRE_MESSAGE(incremental.valid, incremental.failureReason);
	BOOST_CHECK_EQUAL(incremental.requestedTargetCount, 2);
	BOOST_CHECK_EQUAL(incremental.unresolvedGaugeRank, 1);
}

BOOST_AUTO_TEST_CASE(persistent_canonical_functional_rejects_unrepresentable_s_transform)
{
	KFKey first;
	first.type = KF::PHASE_BIAS;
	first.Sat = SatSys(E_Sys::GPS, 1);
	KFKey second = first;
	second.Sat = SatSys(E_Sys::GPS, 2);
	const std::vector<ZhangCapturedStateKey> sourceKeys = {
		zhangCapturedStateKey(first), zhangCapturedStateKey(second)};
	const std::vector<ZhangCapturedStateKey> destinationKeys = {
		zhangCapturedStateKey(first)};
	KFMeas measurement;
	measurement.H = Matrix2d::Identity();
	measurement.V = Vector2d::Zero();
	measurement.R = Matrix2d::Identity();
	measurement.obsKeys = {first, second};
	const Vector2d priorMean = Vector2d::Zero();
	const Matrix2d priorCovariance = Matrix2d::Identity();
	const Vector2d posteriorMean = Vector2d::Zero();
	const Matrix2d posteriorCovariance = 0.5 * Matrix2d::Identity();
	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), sourceKeys, priorMean, priorCovariance, measurement, "/PPP",
		posteriorMean, posteriorCovariance));
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K2", "coordinate-b", "G01:0->G03:0",
		{{"arc-b", 0}}, sourceKeys, Vector2d(0, 1), 0,
		posteriorMean, posteriorCovariance,
		1, "GPS:L1C:CANONICAL", "GPS:L1C:G01->G03",
		"GPS:L1C:G01->G03:V0", 0));
	SparseMatrix<double> projection(1, 2);
	projection.insert(0, 0) = 1;
	BOOST_CHECK(!capture.recordCoordinateTransform(
		GTime(), sourceKeys, destinationKeys, projection,
		"unrepresentable tree exchange"));
	const auto summary = capture.summary();
	BOOST_CHECK_EQUAL(
		summary.failureReason,
		"PERSISTENT_FUNCTIONAL_NOT_TRANSPORTABLE_EXACT_COORDINATE_TRANSFORM");

	// The same loss is a legal boundary only when the caller has classified it
	// as a real physical-arc reinitialisation.  The old chronology is then
	// closed and the next accepted measurement establishes a fresh anchor.
	capture.resetForPhysicalArcChange();
	BOOST_CHECK(capture.summary().failureReason.empty());
	KFMeas restartedMeasurement;
	restartedMeasurement.H = MatrixXd::Identity(1, 1);
	restartedMeasurement.V = VectorXd::Zero(1);
	restartedMeasurement.R = MatrixXd::Identity(1, 1);
	restartedMeasurement.obsKeys = {first};
	const VectorXd restartedPrior = VectorXd::Zero(1);
	const MatrixXd restartedPriorCovariance = MatrixXd::Identity(1, 1);
	const VectorXd restartedPosterior = VectorXd::Zero(1);
	const MatrixXd restartedPosteriorCovariance =
		0.5 * MatrixXd::Identity(1, 1);
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), destinationKeys, restartedPrior, restartedPriorCovariance,
		restartedMeasurement, "/PPP", restartedPosterior,
		restartedPosteriorCovariance));
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K2", "coordinate-c", "G01:1->G03:0",
		{{"arc-b", 1}}, destinationKeys, VectorXd::Ones(1), 0,
		restartedPosterior, restartedPosteriorCovariance,
		1, "GPS:L1C:CANONICAL:V1", "GPS:L1C:G01->G03",
		"GPS:L1C:G01->G03:V0", 0));
	BOOST_CHECK(capture.summary().valid);
}

BOOST_AUTO_TEST_CASE(persistent_quotient_is_held_when_state_transition_drops_its_row)
{
	KFKey first;
	first.type = KF::PHASE_BIAS;
	first.Sat = SatSys(E_Sys::GPS, 1);
	KFKey second = first;
	second.Sat = SatSys(E_Sys::GPS, 2);
	const std::vector<ZhangCapturedStateKey> sourceKeys = {
		zhangCapturedStateKey(first), zhangCapturedStateKey(second)};
	const std::vector<ZhangCapturedStateKey> destinationKeys = sourceKeys;
	KFMeas firstMeasurement;
	firstMeasurement.H = Matrix2d::Identity();
	firstMeasurement.V = Vector2d::Zero();
	firstMeasurement.R = Matrix2d::Identity();
	firstMeasurement.obsKeys = {first, second};
	const Vector2d sourceMean = Vector2d::Zero();
	const Matrix2d sourceCovariance = Matrix2d::Identity();
	const Matrix2d sourcePosteriorCovariance = 0.5 * Matrix2d::Identity();
	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), sourceKeys, sourceMean, sourceCovariance, firstMeasurement,
		"/PPP", sourceMean, sourcePosteriorCovariance));
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K1", "coordinate-a", "G01:0->G02:0",
		{{"arc-a", 0}}, sourceKeys, Vector2d(1, 0), 0,
		sourceMean, sourcePosteriorCovariance,
		1, "GPS:L1C:CANONICAL", "GPS:L1C:G01->G02",
		"GPS:L1C:G01->G02:V0", 0));
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K2", "coordinate-b", "G01:0->G03:0",
		{{"arc-b", 0}}, sourceKeys, Vector2d(0, 1), 0,
		sourceMean, sourcePosteriorCovariance,
		1, "GPS:L1C:CANONICAL", "GPS:L1C:G01->G03",
		"GPS:L1C:G01->G03:V0", 0));
	SparseMatrix<double> projection(2, 2);
	projection.insert(0, 0) = 1;
	Matrix2d processCovariance = Matrix2d::Zero();
	processCovariance(1, 1) = 1;
	BOOST_REQUIRE(capture.recordTransition(
		GTime(), sourceKeys, destinationKeys, projection,
		processCovariance, "state retirement"));

	KFMeas secondMeasurement = firstMeasurement;
	const Vector2d destinationMean = Vector2d::Zero();
	Matrix2d destinationPriorCovariance = Matrix2d::Zero();
	destinationPriorCovariance(0, 0) = 0.5;
	destinationPriorCovariance(1, 1) = 1;
	Matrix2d destinationPosteriorCovariance = Matrix2d::Zero();
	destinationPosteriorCovariance(0, 0) = 1.0 / 3.0;
	destinationPosteriorCovariance(1, 1) = 0.5;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), destinationKeys, destinationMean, destinationPriorCovariance,
		secondMeasurement, "/PPP", destinationMean,
		destinationPosteriorCovariance));
	BOOST_CHECK(!capture.recordPhysicalTarget(
		GTime(), "K2", "coordinate-c", "G01:0->G03:0",
		{{"arc-c", 0}}, destinationKeys, Vector2d(0, 1), 0,
		destinationMean, destinationPosteriorCovariance,
		1, "GPS:L1C:CANONICAL", "GPS:L1C:G01->G03",
		"GPS:L1C:G01->G03:V0", 0));
	BOOST_CHECK_EQUAL(
		capture.lastTargetReason(),
		"PERSISTENT_QUOTIENT_FUNCTIONAL_NOT_TRANSPORTABLE");
	BOOST_CHECK(capture.summary().valid);
	const auto held = capture.currentIncrementalTargetMarginal();
	BOOST_REQUIRE_MESSAGE(held.valid, held.failureReason);
	BOOST_CHECK_EQUAL(held.requestedTargetCount, 2);
	BOOST_CHECK_EQUAL(held.quotientValidRank, 1);

	// An exact absolute datum for the same version may rebind the unavailable
	// row without changing the canonical product identity.
	BOOST_REQUIRE(capture.recordPhysicalTarget(
		GTime(), "K2", "coordinate-c", "G01:0->G03:0",
		{{"arc-c", 0}}, destinationKeys, Vector2d(0, 1), 4,
		destinationMean, destinationPosteriorCovariance,
		0, "", "GPS:L1C:G01->G03", "GPS:L1C:G01->G03:V0", 0));
}

BOOST_AUTO_TEST_CASE(incremental_target_separator_accumulates_and_retires_without_history_rows)
{
	ZhangIncrementalTargetSeparator separator;
	const std::vector<std::string> firstKeys = {"A@0", "B@0"};
	const Matrix2d design = Matrix2d::Identity();
	Matrix2d firstCovariance;
	firstCovariance << 0.04, 0.01, 0.01, 0.09;
	Vector2d firstObservation(2.1, -0.9);
	BOOST_REQUIRE(separator.addLikelihood(
		firstKeys, design, firstCovariance, firstObservation,
		{"G0", "G0"}, {false, false}));
	Matrix2d secondCovariance;
	secondCovariance << 0.03, -0.004, -0.004, 0.05;
	Vector2d secondObservation(1.95, -1.05);
	BOOST_REQUIRE(separator.addLikelihood(
		firstKeys, design, secondCovariance, secondObservation,
		{"G0", "G0"}, {false, false}));
	const auto accumulated = separator.marginal();
	BOOST_REQUIRE_MESSAGE(accumulated.valid, accumulated.failureReason);
	const Matrix2d expectedInformation = firstCovariance.inverse()
		+ secondCovariance.inverse();
	const Matrix2d expectedCovariance = expectedInformation.inverse();
	const Vector2d expectedMean = expectedCovariance
		* (firstCovariance.inverse() * firstObservation
			+ secondCovariance.inverse() * secondObservation);
	BOOST_CHECK_SMALL((accumulated.mean - expectedMean).norm(), 1e-11);
	BOOST_CHECK_SMALL(
		(accumulated.covariance - expectedCovariance).norm()
			/ expectedCovariance.norm(),
		1e-11);
	BOOST_CHECK_EQUAL(accumulated.informationRank, 2);
	BOOST_CHECK_EQUAL(accumulated.quotientValidRank, 1);
	BOOST_CHECK_EQUAL(accumulated.absoluteValidRank, 0);

	// An exact datum promotes only that canonical relation.  Other relations in
	// the quotient group retain their unresolved integer gauge.
	ZhangIncrementalTargetSeparator promoted = separator;
	BOOST_REQUIRE(promoted.addLikelihood(
		firstKeys, design, 0.02 * Matrix2d::Identity(),
		Vector2d(2.0, 2.0), {"G0", ""}, {false, true}, {0, 3}));
	const auto promotedMarginal = promoted.marginal();
	BOOST_REQUIRE_MESSAGE(promotedMarginal.valid, promotedMarginal.failureReason);
	BOOST_CHECK_EQUAL(promotedMarginal.unresolvedGaugeRank, 1);
	BOOST_CHECK_EQUAL(promotedMarginal.quotientValidRank, 1);
	BOOST_CHECK_EQUAL(promotedMarginal.absoluteValidRank, 1);
	BOOST_REQUIRE_EQUAL(promotedMarginal.coordinateOffsets.size(), 2);
	BOOST_CHECK_SMALL(promotedMarginal.coordinateOffsets[1] - 3, 1e-15);

	const std::vector<std::string> nextKeys = {"B@0", "C@1"};
	BOOST_REQUIRE(separator.addLikelihood(
		nextKeys, design, 0.02 * Matrix2d::Identity(),
		Vector2d(-1.0, 3.02), {"G0", ""}, {true, true}));
	BOOST_REQUIRE(separator.retainOnly({"B@0", "C@1"}));
	const auto retired = separator.marginal();
	BOOST_REQUIRE_MESSAGE(retired.valid, retired.failureReason);
	BOOST_CHECK_EQUAL(retired.requestedTargetCount, 2);
	BOOST_CHECK_EQUAL(retired.informationRank, 2);
	BOOST_CHECK_EQUAL(retired.quotientValidRank, 2);
	BOOST_CHECK_EQUAL(retired.absoluteValidRank, 2);
	BOOST_CHECK_LE(retired.storedRows, 2);
	BOOST_CHECK_LE(retired.storedColumns, 2);
	BOOST_CHECK_LE(retired.maximumStoredColumns, 3);
}
