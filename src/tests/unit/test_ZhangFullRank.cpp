#define BOOST_TEST_MODULE ZhangFullRankTests
#include <boost/test/unit_test.hpp>
#include <random>
#include "common/eigenIncluder.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "common/zhangPhaseContinuity.hpp"
#include "common/zhangSatelliteDatum.hpp"
#include "common/zhangFullRank.hpp"

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
