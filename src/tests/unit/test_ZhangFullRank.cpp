#define BOOST_TEST_MODULE ZhangFullRankTests
#include <boost/test/unit_test.hpp>
#include "common/eigenIncluder.hpp"
#include "common/zhangFullRank.hpp"

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
}
