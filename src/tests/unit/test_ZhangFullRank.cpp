#define BOOST_TEST_MODULE ZhangFullRankTests
#include <boost/test/unit_test.hpp>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <functional>
#include <numeric>
#include <random>
#include <sstream>
#include <tuple>
#include "ambres/GNSSambres.hpp"
#include "common/eigenIncluder.hpp"
#include "common/receiver.hpp"
#include "common/zhangCheckpoint.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "common/zhangIarGainAudit.hpp"
#include "common/zhangProductRelationBasis.hpp"
#include "common/zhangProductRelationAdmission.hpp"
#include "common/zhangProductIntegerLedger.hpp"
#include "common/zhangProductIntegerCandidateGenerator.hpp"
#include "common/zhangFullProductLatticeOracle.hpp"
#include "common/zhangProductRelationSolver.hpp"
#include "common/zhangIntegerSupportQuality.hpp"
#include "common/zhangTargetedBesdTracker.hpp"
#include "common/zhangTheoryRegression.hpp"
#include "common/zhangPhaseContinuity.hpp"
#include "common/zhangSatelliteDatum.hpp"
#include "common/zhangPersistentProductDatum.hpp"
#include "common/zhangFullRank.hpp"
#include "common/zhangFixedLagSquareRoot.hpp"
#include "common/zhangIncrementalFixedLag.hpp"
#include "common/zhangIncrementalRawSquareRoot.hpp"
#include "common/zhangPersistentRawTargetWindow.hpp"
#include "common/zhangIntegerTargets.hpp"
#include "common/zhangLambdaBeam.hpp"
#include "common/zhangResidualStatistics.hpp"
#include "common/zhangFactorCapture.hpp"
#include "common/zhangRawFactorWindow.hpp"
#include "common/zhangUserTarget.hpp"
#include "common/zhangUserIntegerFunctional.hpp"
#include "common/zhangIfUser.hpp"
#include "common/zhangIfWideLane.hpp"
#include "common/zhangProductGaugeCompiler.hpp"
#include "common/zhangQuotientIntegerLattice.hpp"
#include "common/zhangIntegerProductGainFrontier.hpp"
#include "common/zhangIntegerConditioner.hpp"
#include "common/zhangHybridUserModel.hpp"
#include "common/zhangHybridService.hpp"
#include "pea/zhangPppAr.hpp"

namespace
{
Receiver& checkpointTestReceiver()
{
	static Receiver receiver;
	receiver.id = "R0";
	return receiver;
}

struct TemporaryCheckpointFile
{
	std::filesystem::path path;

	explicit TemporaryCheckpointFile(const std::string& suffix)
	{
		const auto nonce =
			std::chrono::steady_clock::now().time_since_epoch().count();
		path = std::filesystem::temp_directory_path() /
			("ginan_e29_checkpoint_" + std::to_string(nonce) + suffix);
	}

	~TemporaryCheckpointFile()
	{
		std::error_code error;
		std::filesystem::remove(path, error);
	}
};

KFState makeCheckpointTestState()
{
	KFState state;
	KFKey receiverClock;
	receiverClock.type = KF::REC_CLOCK;
	receiverClock.str = "R0";
	receiverClock.comment = "receiver datum";
	receiverClock.estimatedTime.bigTime = 123456700.5L;
	receiverClock.rec_ptr = &checkpointTestReceiver();
	KFKey satelliteClock;
	satelliteClock.type = KF::SAT_CLOCK;
	satelliteClock.Sat = SatSys(E_Sys::GPS, 7);
	satelliteClock.comment = "satellite datum";
	satelliteClock.estimatedTime.bigTime = 123456710.25L;

	state.time.bigTime = 123456789.25L;
	state.x = VectorXd(3);
	state.x << 1, -2.5, 3.75;
	state.P = MatrixXd(3, 3);
	state.P <<
		0, 0, 0,
		0, 4, -0.25,
		0, -0.25, 9;
	state.dx = VectorXd(3);
	state.dx << 0, 0.125, -0.5;
	state.prefitRatios = VectorXd(2);
	state.prefitRatios << 1.5, 2.5;
	state.postfitRatios = VectorXd(2);
	state.postfitRatios << 0.75, 1.25;
	state.kfIndexMap = {
		{KFState::oneKey, 0},
		{receiverClock, 1},
		{satelliteClock, 2}};
	state.stateTransitionMap[satelliteClock][receiverClock][0] = -1.25;
	state.gaussMarkovTauMap[receiverClock] = 3600;
	state.gaussMarkovMuMap[receiverClock] = 0.25;
	state.procNoiseMap[satelliteClock] = 0.01;
	state.initNoiseMap[satelliteClock] = 4;
	state.sigmaMaxMap[satelliteClock] = 20;
	state.outageLimitMap[satelliteClock] = 120;
	state.exponentialNoiseMap[receiverClock] = {0.75, 45};
	state.pseudoStateMap[satelliteClock][receiverClock] = -1;
	state.pseudoParentMap[receiverClock] = satelliteClock;
	state.errorCountMap[satelliteClock] = 2;
	FilterChunk chunk;
	chunk.id = "connected-product-core";
	chunk.begX = 1;
	chunk.numX = 2;
	chunk.begH = 7;
	chunk.numH = 11;
	state.filterChunkMap["zhang"] = chunk;
	state.metaDataMap["zhang_checkpoint_runtime_id"] = "runtime-00";
	state.lsqRequired = true;
	state.sigmaPass = true;
	state.chiQCPass = true;
	state.chi2 = 12.5;
	state.dof = 7;
	state.chi2PerDof = 12.5 / 7;
	state.qc = 0.875;
	state.id = "E29-test-state";
	state.rts_basename = "e29-test";
	state.output_residuals = true;
	state.outputMongoMeasurements = true;
	state.statisticsMap["accepted"] = 17;
	state.statisticsMapSum["accepted"] = 41;
	return state;
}

ZhangCheckpointBundle makeCheckpointTestBundle()
{
	ZhangCheckpointBundle bundle;
	bundle.manifest.runtimeId = "runtime-00";
	bundle.manifest.checkpointId = "seed-00";
	bundle.manifest.parentCheckpointId = "cold-start";
	bundle.manifest.epoch = "2019-07-18T00:00:00Z";
	bundle.manifest.binarySha256 = std::string(64, 'a');
	bundle.manifest.configText = "frozen-e29-config";
	bundle.manifest.inputManifestText = "frozen-e29-input-manifest";
	bundle.manifest.configSha256 =
		zhangCheckpointSha256(bundle.manifest.configText);
	bundle.manifest.inputManifestSha256 =
		zhangCheckpointSha256(bundle.manifest.inputManifestText);
	bundle.manifest.platformFingerprint = "x86_64-linux";
	bundle.manifest.compilerFingerprint = "gcc-11.4-cxx20";
	bundle.manifest.linearAlgebraFingerprint = "eigen-3.4.1-openblas";
	bundle.manifest.endianness = "LITTLE";
	bundle.manifest.createdUtc = "2026-08-10T00:00:00Z";
	bundle.kfCore = captureZhangCheckpointKfCore(makeCheckpointTestState());
	bundle.sections["zhang.graph"] = {
		1, "pointer-free-graph-runtime", ""};
	bundle.sections["zhang.graph"].sha256 =
		zhangCheckpointSha256(bundle.sections["zhang.graph"].payload);
	return bundle;
}

ZhangCheckpointExpectations checkpointTestExpectations()
{
	ZhangCheckpointExpectations expectations;
	expectations.experimentMode = "E29_GPS_L1C_L2W_ZHANG_FULL_RANK";
	expectations.binarySha256 = std::string(64, 'a');
	expectations.configSha256 =
		zhangCheckpointSha256("frozen-e29-config");
	expectations.inputManifestSha256 =
		zhangCheckpointSha256("frozen-e29-input-manifest");
	expectations.platformFingerprint = "x86_64-linux";
	expectations.compilerFingerprint = "gcc-11.4-cxx20";
	expectations.linearAlgebraFingerprint = "eigen-3.4.1-openblas";
	expectations.endianness = "LITTLE";
	return expectations;
}

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

BOOST_AUTO_TEST_CASE(product_receiver_core_spans_all_satellites_with_redundancy)
{
    const SatSys g1(E_Sys::GPS, 1);
    const SatSys g2(E_Sys::GPS, 2);
    const SatSys g3(E_Sys::GPS, 3);
    const SatSys g4(E_Sys::GPS, 4);
    const SatSys g5(E_Sys::GPS, 5);
    std::set<ZhangGraphEdge> edges = {
        {"R0", g1}, {"R0", g2},
        {"R1", g1}, {"R1", g3}, {"R1", g4},
        {"R2", g2}, {"R2", g4}, {"R2", g5},
        {"R3", g3}, {"R3", g5},
        {"R4", g1}, {"R4", g2}, {"R4", g3}, {"R4", g4}, {"R4", g5},
        {"R5", g1}, {"R5", g2}
    };
    const auto core = zhangBuildProductReceiverCore(
        edges, "R0", {"R3", "R5"}, 2);
    BOOST_REQUIRE(core.connected);
    BOOST_CHECK_EQUAL(core.satellites.size(), 5);
    BOOST_CHECK_EQUAL(core.minimumSatelliteSupport, 2);
    BOOST_CHECK(core.receivers.find("R0") != core.receivers.end());
    BOOST_CHECK(core.receivers.find("R3") != core.receivers.end());
    // A prior receiver that contributes no remaining support deficit is not
    // forced into the new core; this is the controlled-retirement invariant.
    BOOST_CHECK(core.receivers.find("R5") == core.receivers.end());
    BOOST_CHECK_LT(core.receivers.size(), 6);
    std::map<SatSys, int> support;
    for (const auto& edge : core.edges)
    {
        support[edge.satellite]++;
    }
    for (const auto& satellite : core.satellites)
    {
        BOOST_CHECK_GE(support[satellite], 2);
    }
    BOOST_CHECK(zhangBuildSpanningTree(core.edges, "R0").connected);
}

BOOST_AUTO_TEST_CASE(rooted_product_tree_limits_nonroot_satellite_path_load)
{
    const SatSys g1(E_Sys::GPS, 1);
    const SatSys g2(E_Sys::GPS, 2);
    const SatSys g3(E_Sys::GPS, 3);
    const SatSys g4(E_Sys::GPS, 4);
    const std::set<ZhangGraphEdge> edges = {
        {"R0", g1}, {"R0", g2}, {"R0", g3},
        {"R1", g1}, {"R1", g2},
        {"R2", g2}, {"R2", g3}, {"R2", g4}
    };
    const std::set<ZhangGraphEdge> chainPreferred = {
        {"R0", g1}, {"R1", g1}, {"R1", g2},
        {"R2", g2}, {"R2", g3}, {"R2", g4}
    };
    const auto kruskal = zhangBuildSpanningTree(
        edges, "R0", chainPreferred);
    const auto rooted = zhangBuildRootedProductTree(
        edges, "R0", chainPreferred);
    BOOST_REQUIRE(kruskal.connected);
    BOOST_REQUIRE(rooted.connected);

    auto vulnerableMaximum = [](const ZhangGraphBasis& basis)
    {
        int maximum = 0;
        for (const auto& [edge, load] :
             zhangProductTreeSatellitePathLoads(basis))
        {
            if (edge.receiver != basis.rootReceiver)
            {
                maximum = std::max(maximum, load);
            }
        }
        return maximum;
    };
    BOOST_CHECK_EQUAL(vulnerableMaximum(kruskal), 3);
    BOOST_CHECK_EQUAL(vulnerableMaximum(rooted), 1);
    BOOST_CHECK_LT(vulnerableMaximum(rooted), vulnerableMaximum(kruskal));
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

BOOST_AUTO_TEST_CASE(e27_if_user_integer_and_covariance_algebra_closes)
{
    constexpr double lambda1 = 0.190293672798365;
    constexpr double lambda2 = 0.244210213424568;
    const auto coefficients = zhangIfUserCoefficients(lambda1, lambda2);
    BOOST_REQUIRE(coefficients.valid);
    BOOST_CHECK_SMALL(coefficients.alpha + coefficients.beta - 1, 1e-14);
    BOOST_CHECK_SMALL(
        coefficients.narrowLaneWavelength -
            lambda1 * lambda2 / (lambda1 + lambda2),
        1e-14);

    constexpr double firstInteger = 123456;
    constexpr double secondInteger = 123411;
    constexpr double wideLaneInteger = firstInteger - secondInteger;
    const double ifAmbiguity = zhangIfAmbiguityMetres(
        coefficients, lambda1, lambda2, firstInteger, secondInteger);
    const double recoveredFirst = zhangIfConditionedFirstInteger(
        coefficients, lambda2, ifAmbiguity, wideLaneInteger);
    BOOST_CHECK_SMALL(recoveredFirst - firstInteger, 1e-10);

    // Per-frequency correction precedes IF construction.  The combined
    // correction must be exactly alpha*c1+beta*c2.
    constexpr double rawFirst = 12.125;
    constexpr double rawSecond = 15.750;
    constexpr double correctionFirst = 0.237;
    constexpr double correctionSecond = -0.119;
    const double correctedIf =
        coefficients.alpha * (rawFirst + correctionFirst) +
        coefficients.beta * (rawSecond + correctionSecond);
    const double rawIf = coefficients.alpha * rawFirst +
        coefficients.beta * rawSecond;
    BOOST_CHECK_SMALL(
        (correctedIf - rawIf) -
            (coefficients.alpha * correctionFirst +
             coefficients.beta * correctionSecond),
        1e-12);

    // Three satellites with [clock, phase1, phase2] parameters.  The exact
    // functional includes cross-frequency and cross-satellite covariance.
    MatrixXd factor(9, 4);
    factor <<
        0.30,  0.02,  0.00,  0.00,
        0.10,  0.15,  0.01,  0.00,
        0.12, -0.03,  0.18,  0.00,
        0.28,  0.01,  0.00,  0.02,
        0.09,  0.14, -0.02,  0.01,
        0.11, -0.04,  0.17, -0.01,
        0.31,  0.03,  0.01, -0.02,
        0.08,  0.16,  0.00,  0.03,
        0.13, -0.02,  0.19,  0.01;
    const MatrixXd covariance = factor * factor.transpose();
    const MatrixXd transform = zhangIfProductSdFunctional(
        3, 0, coefficients, true);
    const MatrixXd propagated = zhangPropagateIfProductSdCovariance(
        covariance, 3, 0, coefficients, true);
    BOOST_REQUIRE_EQUAL(propagated.rows(), 2);
    BOOST_CHECK_SMALL(
        (propagated - transform * covariance * transform.transpose()).norm(),
        1e-14);
    BOOST_CHECK((propagated.diagonal().array() >= 0).all());

    // Removing cross-satellite covariance is not an equivalent stochastic
    // model and must be observable in this regression.
    MatrixXd blockDiagonal = covariance;
    for (int left = 0; left < 3; left++)
    for (int right = 0; right < 3; right++)
    {
        if (left != right)
        {
            blockDiagonal.block<3, 3>(3 * left, 3 * right).setZero();
        }
    }
    const MatrixXd independentApproximation =
        transform * blockDiagonal * transform.transpose();
    BOOST_CHECK_GT((propagated - independentApproximation).norm(), 1e-6);
}

BOOST_AUTO_TEST_CASE(e29_hybrid_user_model_follows_frozen_document_equations)
{
	constexpr double lambda1 = 0.190293672798365;
	constexpr double mu1 = 1;
	constexpr double mu2 = 1.646944444444444;
	// The existing server stores B^phi.  The supplied user equations use the
	// correction-side phase bias delta^G=-B^phi, so the adapter must map signs
	// explicitly instead of silently renaming the internal coordinate.
	const auto product =
		zhangDualFrequencyHybridProductsFromInternalPhaseStates(
			12.5, -0.08, 0.11);
	BOOST_REQUIRE(product.valid);
	// Equations (24)--(27): both baseline code signals consume exactly the
	// same satellite clock.  Only phase has signal-specific products.
	BOOST_CHECK_SMALL(
		product.codeCorrectionToAddMetres(0)
		- product.codeCorrectionToAddMetres(1), 1e-14);
	BOOST_CHECK_SMALL(
		product.phaseCorrectionToAddMetres(0) - 12.58, 1e-14);
	BOOST_CHECK_SMALL(
		product.phaseCorrectionToAddMetres(1) - 12.39, 1e-14);
	constexpr double rawCode = 23456789.25;
	constexpr double rawPhase = 23456780.75;
	BOOST_CHECK_SMALL(
		zhangHybridApplyLeftCorrection(
			rawCode, product.codeCorrectionToAddMetres(0))
		- (rawCode + 12.5), 1e-12);
	BOOST_CHECK_SMALL(
		zhangHybridApplyLeftCorrection(
			rawPhase, product.phaseCorrectionToAddMetres(0))
		- (rawPhase + 12.58), 1e-12);

	// Equations (3)--(13): physical receiver/satellite code biases are not
	// discarded; their two IF/GF directions are absorbed by the estimable
	// clocks, ionosphere and phase biases.
	constexpr double receiverCodeIf = 2.75;
	constexpr double receiverCodeGf = -0.625;
	constexpr double satelliteCodeIf = -1.2;
	constexpr double satelliteCodeGf = 0.35;
	const double receiverCode1 = receiverCodeIf + mu1 * receiverCodeGf;
	const double receiverCode2 = receiverCodeIf + mu2 * receiverCodeGf;
	const double satelliteCode1 = satelliteCodeIf + mu1 * satelliteCodeGf;
	const double satelliteCode2 = satelliteCodeIf + mu2 * satelliteCodeGf;
	const auto receiverDatum = zhangHybridCodeIfGfDatum(
		receiverCode1, receiverCode2, mu1, mu2);
	const auto satelliteDatum = zhangHybridCodeIfGfDatum(
		satelliteCode1, satelliteCode2, mu1, mu2);
	BOOST_REQUIRE(receiverDatum.valid);
	BOOST_REQUIRE(satelliteDatum.valid);
	BOOST_CHECK_SMALL(
		receiverDatum.ifBiasMetres - receiverCodeIf, 1e-13);
	BOOST_CHECK_SMALL(
		receiverDatum.gfBiasMetres - receiverCodeGf, 1e-13);
	BOOST_CHECK_SMALL(
		satelliteDatum.ifBiasMetres - satelliteCodeIf, 1e-13);
	BOOST_CHECK_SMALL(
		satelliteDatum.gfBiasMetres - satelliteCodeGf, 1e-13);

	constexpr double receiverClock = 8.5;
	constexpr double satelliteClock = -3.25;
	constexpr double ionosphere = 4.2;
	const double estimableReceiverClock = receiverClock + receiverCodeIf;
	const double estimableSatelliteClock = satelliteClock + satelliteCodeIf;
	const double estimableIonosphere = ionosphere
		+ receiverCodeGf - satelliteCodeGf;
	for (const auto& [mu, receiverCode, satelliteCode] :
		{std::tuple{mu1, receiverCode1, satelliteCode1},
		 std::tuple{mu2, receiverCode2, satelliteCode2}})
	{
		BOOST_CHECK_SMALL(
			zhangHybridOriginalCodePrediction(
				receiverClock, satelliteClock, ionosphere, mu,
				receiverCode, satelliteCode)
			- zhangHybridFullRankCodePrediction(
				estimableReceiverClock, estimableSatelliteClock,
				estimableIonosphere, mu),
			1e-13);
	}

	constexpr double receiverPhaseBias = 0.14;
	constexpr double satellitePhaseBias = -0.09;
	constexpr long long ambiguity = 123456;
	const double estimableReceiverPhaseBias = receiverPhaseBias
		- receiverCodeIf + mu1 * receiverCodeGf;
	const double estimableSatellitePhaseBias = satellitePhaseBias
		- satelliteCodeIf + mu1 * satelliteCodeGf;
	BOOST_CHECK_SMALL(
		zhangHybridOriginalPhasePrediction(
			receiverClock, satelliteClock, ionosphere, mu1,
			receiverPhaseBias, satellitePhaseBias,
			lambda1, ambiguity)
		- zhangHybridFullRankPhasePrediction(
			estimableReceiverClock, estimableSatelliteClock,
			estimableIonosphere, mu1,
			estimableReceiverPhaseBias, estimableSatellitePhaseBias,
			lambda1, ambiguity),
		1e-12);

	const MatrixXd sd = zhangHybridSatelliteSingleDifferenceTransform(4, 1);
	BOOST_REQUIRE_EQUAL(sd.rows(), 3);
	BOOST_REQUIRE_EQUAL(sd.cols(), 4);
	BOOST_CHECK_SMALL((sd * Vector4d::Ones()).norm(), 1e-14);
	const Vector4d ambiguities(17, -4, 8, 21);
	const Vector3d expectedSd(21, 12, 25);
	BOOST_CHECK_SMALL((sd * ambiguities - expectedSd).norm(), 1e-14);

	const Matrix2d integerTransform =
		zhangHybridWideLaneFirstIntegerTransform();
	BOOST_CHECK_EQUAL(std::llround(integerTransform.determinant()), 1);
	const Vector2d integerPair(31, 24);
	const Vector2d wlFirst = integerTransform * integerPair;
	BOOST_CHECK_SMALL(wlFirst(0) - 7, 1e-14);
	BOOST_CHECK_SMALL(wlFirst(1) - 31, 1e-14);
	BOOST_CHECK_SMALL(
		(integerTransform.inverse() * wlFirst - integerPair).norm(), 1e-14);

	BOOST_CHECK(
		zhangHybridIntegerUsability(true, false)
		== ZhangHybridIntegerUsability::FLOAT_ONLY);
	BOOST_CHECK(
		zhangHybridIntegerUsability(true, true)
		== ZhangHybridIntegerUsability::PPP_AR_USABLE);
	BOOST_CHECK(
		zhangHybridIntegerUsability(false, true)
		== ZhangHybridIntegerUsability::UNUSABLE);
	BOOST_CHECK(zhangHybridRelativeIntegerPairCertified(
		true, "GPS-L1C-COMP-A", true, "GPS-L1C-COMP-A"));
	BOOST_CHECK(!zhangHybridRelativeIntegerPairCertified(
		true, "GPS-L1C-COMP-A", true, "GPS-L1C-COMP-B"));
	BOOST_CHECK(!zhangHybridRelativeIntegerPairCertified(
		true, "NONE", true, "NONE"));
	BOOST_CHECK(!zhangHybridRelativeIntegerPairCertified(
		true, "GPS-L1C-COMP-A", false, "GPS-L1C-COMP-A"));

	Matrix4d userNoise = Matrix4d::Identity() * 0.04;
	Matrix4d factor;
	factor <<
		0.30,  0.02, 0.00, 0.00,
		0.25, -0.01, 0.04, 0.00,
		0.28,  0.03, 0.00, 0.02,
		0.22, -0.02, 0.05, 0.01;
	const Matrix4d productCovariance = factor * factor.transpose();
	const MatrixXd corrected = zhangHybridCorrectedObservationCovariance(
		userNoise, productCovariance);
	BOOST_CHECK_SMALL(
		(corrected - userNoise - productCovariance).norm(), 1e-14);
	const MatrixXd propagated = zhangHybridSingleDifferenceCovariance(
		userNoise, productCovariance, sd);
	BOOST_CHECK_SMALL(
		(propagated - sd * corrected * sd.transpose()).norm(), 1e-14);

	Matrix4d diagonalProduct = productCovariance.diagonal().asDiagonal();
	const MatrixXd diagonalApproximation = zhangHybridSingleDifferenceCovariance(
		userNoise, diagonalProduct, sd);
	BOOST_CHECK_GT((propagated - diagonalApproximation).norm(), 1e-6);
}

BOOST_AUTO_TEST_CASE(e29_hybrid_real_gauge_is_overlap_gls_and_not_epoch_zero_mean)
{
	const std::vector<SatSys> firstSatellites = {
		SatSys(E_Sys::GPS, 1), SatSys(E_Sys::GPS, 2),
		SatSys(E_Sys::GPS, 3)};
	const std::vector<std::string> firstSegments = {"A", "B", "C"};
	Vector3d firstRaw(4.0, 7.0, 13.0);
	Matrix3d firstCovariance = Matrix3d::Zero();
	firstCovariance.diagonal() << 1.0, 4.0, 9.0;
	ZhangHybridRealGaugeTransport gauge;
	const auto first = gauge.transport(
		firstSatellites, firstSegments, firstRaw, firstCovariance);
	BOOST_REQUIRE_MESSAGE(first.valid, first.failureReason);
	BOOST_CHECK(first.newGeneration);
	BOOST_CHECK_EQUAL(first.generation, 0);
	const Vector3d inverseVarianceWeights(1.0, 0.25, 1.0 / 9.0);
	BOOST_CHECK_SMALL(
		inverseVarianceWeights.dot(first.values), 1e-12);
	BOOST_CHECK_SMALL(
		(first.covariance
		 - first.transform * firstCovariance * first.transform.transpose()).norm(),
		1e-13);

	// G04 joins, but the old overlap remains.  A single common offset aligns
	// G01--G03 to their previous gauge; the four-satellite result is therefore
	// not re-zeroed over the changed membership.
	const std::vector<SatSys> secondSatellites = {
		SatSys(E_Sys::GPS, 1), SatSys(E_Sys::GPS, 2),
		SatSys(E_Sys::GPS, 3), SatSys(E_Sys::GPS, 4)};
	const std::vector<std::string> secondSegments = {"A", "B", "C", "D"};
	Vector4d secondRaw;
	secondRaw.head<3>() = firstRaw.array() + 2.5;
	secondRaw(3) = 100.0;
	Matrix4d secondCovariance = Matrix4d::Identity();
	const auto second = gauge.transport(
		secondSatellites, secondSegments, secondRaw, secondCovariance);
	BOOST_REQUIRE_MESSAGE(second.valid, second.failureReason);
	BOOST_CHECK(!second.newGeneration);
	BOOST_CHECK_EQUAL(second.overlapCount, 3);
	BOOST_CHECK_SMALL((second.values.head<3>() - first.values).norm(), 1e-12);
	BOOST_CHECK_GT(std::abs(second.values.sum()), 1.0);

	// No unchanged physical segment remains: continuity cannot be fabricated.
	const std::vector<std::string> thirdSegments = {"A2", "B2", "C2", "D2"};
	const auto third = gauge.transport(
		secondSatellites, thirdSegments, secondRaw, secondCovariance);
	BOOST_REQUIRE_MESSAGE(third.valid, third.failureReason);
	BOOST_CHECK(third.newGeneration);
	BOOST_CHECK_EQUAL(third.generation, 1);
	BOOST_CHECK_EQUAL(third.overlapCount, 0);
	BOOST_CHECK_SMALL(third.values.sum(), 1e-12);
}

BOOST_AUTO_TEST_CASE(e29_persistent_dynamic_gate_uses_manager_not_product_tree_proof)
{
	const auto persistent = zhangHybridInitialIntegerGate(
		true,
		true,   // backend graph valid
		true,   // independently audited product functional valid
		false,  // PRODUCT_TREE runtime alignment deliberately unavailable
		false,  // named row deliberately absent from held lattice
		true,   // persistent kappa datum continuous
		true);  // persistent component precision valid
	BOOST_CHECK(persistent.structureValid);
	BOOST_CHECK(persistent.datumContinuous);
	BOOST_CHECK(persistent.precisionValid);

	const auto productTree = zhangHybridInitialIntegerGate(
		false, true, true, false, false, true, true);
	BOOST_CHECK(productTree.structureValid);
	BOOST_CHECK(!productTree.datumContinuous);
	BOOST_CHECK(!productTree.precisionValid);

	const auto invalidStructure = zhangHybridInitialIntegerGate(
		true, true, false, true, true, true, true);
	BOOST_CHECK(!invalidStructure.structureValid);
}

BOOST_AUTO_TEST_CASE(e29_hybrid_real_gauge_checkpoint_preserves_transport)
{
	const std::vector<SatSys> satellites = {
		SatSys(E_Sys::GPS, 1), SatSys(E_Sys::GPS, 3)};
	const std::vector<std::string> segments = {"G01-S0", "G03-S0"};
	Vector2d raw(2.0, 5.0);
	Matrix2d covariance;
	covariance << 1.0, 0.2, 0.2, 2.0;
	ZhangHybridRealGaugeTransport original;
	BOOST_REQUIRE(original.transport(
		satellites, segments, raw, covariance).valid);
	const auto checkpoint = original.checkpointState();
	ZhangHybridRealGaugeTransport restored;
	std::string failureReason;
	BOOST_REQUIRE_MESSAGE(
		restored.restoreCheckpointState(checkpoint, &failureReason),
		failureReason);
	Vector2d shifted = raw.array() + 7.0;
	const auto expected = original.transport(
		satellites, segments, shifted, covariance);
	const auto actual = restored.transport(
		satellites, segments, shifted, covariance);
	BOOST_REQUIRE(expected.valid);
	BOOST_REQUIRE(actual.valid);
	BOOST_CHECK_SMALL((actual.values - expected.values).norm(), 1e-13);
	BOOST_CHECK_SMALL((actual.covariance - expected.covariance).norm(), 1e-13);
	BOOST_CHECK_EQUAL(actual.generation, expected.generation);
}

BOOST_AUTO_TEST_CASE(e29_hybrid_real_gauge_candidate_copy_is_transactional)
{
	const std::vector<SatSys> satellites = {
		SatSys(E_Sys::GPS, 1), SatSys(E_Sys::GPS, 3)};
	const std::vector<std::string> segments = {"G01-S0", "G03-S0"};
	Vector2d raw(2.0, 5.0);
	const Matrix2d covariance = Matrix2d::Identity();
	ZhangHybridRealGaugeTransport committed;
	const auto initial = committed.transport(
		satellites, segments, raw, covariance);
	BOOST_REQUIRE(initial.valid);
	const auto before = committed.checkpointState();

	// appendProductCovariance evaluates every block on a copy and assigns the
	// copies only after every block succeeds.  Aborting the epoch must therefore
	// leave the committed history byte-for-byte equivalent to its checkpoint.
	ZhangHybridRealGaugeTransport candidate = committed;
	// Include a relative change as well as a common change.  A pure common
	// shift is deliberately removed by the gauge and may produce a committed
	// checkpoint numerically identical to the old one; that is not evidence
	// of a failed transaction.
	Vector2d shifted;
	shifted << raw(0) + 11.0, raw(1) + 12.0;
	const auto evaluated = candidate.transport(
		satellites, segments, shifted, covariance);
	BOOST_REQUIRE(evaluated.valid);
	const auto afterAbort = committed.checkpointState();
	BOOST_CHECK_EQUAL(afterAbort.initialized, before.initialized);
	BOOST_CHECK_EQUAL(afterAbort.generation, before.generation);
	BOOST_CHECK(afterAbort.previousValues == before.previousValues);
	BOOST_CHECK(afterAbort.previousSegments == before.previousSegments);

	const auto candidateCheckpoint = candidate.checkpointState();
	committed = std::move(candidate);
	const auto afterCommit = committed.checkpointState();
	BOOST_CHECK(afterCommit.previousValues != before.previousValues);
	BOOST_CHECK(afterCommit.previousValues == candidateCheckpoint.previousValues);
	BOOST_CHECK(afterCommit.previousSegments == candidateCheckpoint.previousSegments);
	BOOST_CHECK_SMALL(
		afterCommit.previousValues.at(satellites.front())
			- evaluated.values(0),
		1e-13);
}

BOOST_AUTO_TEST_CASE(hybrid_stable_frontend_controller_is_prepare_commit_atomic)
{
	using State = std::map<std::string, ZhangHybridRealGaugeTransport>;
	const std::vector<SatSys> satellites = {
		SatSys(E_Sys::GPS, 1), SatSys(E_Sys::GPS, 3)};
	const std::vector<std::string> segments = {"G01-S0", "G03-S0"};
	const Matrix2d covariance = Matrix2d::Identity();
	Vector2d raw(2.0, 5.0);
	State persistent;
	BOOST_REQUIRE(persistent["L1"].transport(
		satellites, segments, raw, covariance).valid);
	const auto before = persistent.at("L1").checkpointState();

	ZhangHybridStableFrontend controller;
	auto rejected = controller.prepare(persistent);
	Vector2d changed(14.0, 19.0);
	BOOST_REQUIRE(rejected.preparedState["L1"].transport(
		satellites, segments, changed, covariance).valid);
	controller.validateIntegerAlignment(rejected, true);
	controller.validateRealGauge(rejected, true);
	controller.validateComponentConsistency(rejected, false);
	controller.validateMetadata(rejected, true);
	BOOST_CHECK(!controller.commit(persistent, rejected));
	controller.rollback(rejected);
	const auto afterRollback = persistent.at("L1").checkpointState();
	BOOST_CHECK(afterRollback.previousValues == before.previousValues);
	BOOST_CHECK(afterRollback.previousSegments == before.previousSegments);

	auto accepted = controller.prepare(persistent);
	BOOST_REQUIRE(accepted.preparedState["L1"].transport(
		satellites, segments, changed, covariance).valid);
	controller.validateIntegerAlignment(accepted, true);
	controller.validateRealGauge(accepted, true);
	controller.validateComponentConsistency(accepted, true);
	controller.validateMetadata(accepted, true);
	const auto expected = accepted.preparedState.at("L1").checkpointState();
	BOOST_REQUIRE(controller.commit(persistent, accepted));
	const auto committed = persistent.at("L1").checkpointState();
	BOOST_CHECK(committed.previousValues == expected.previousValues);
	BOOST_CHECK(committed.previousSegments == expected.previousSegments);
}

BOOST_AUTO_TEST_CASE(e29_hybrid_real_gauge_maps_transform_cross_block_covariance)
{
	const std::vector<SatSys> satellites = {
		SatSys(E_Sys::GPS, 1), SatSys(E_Sys::GPS, 3)};
	const std::vector<std::string> clockSegments = {"CLOCK-G01", "CLOCK-G03"};
	const std::vector<std::string> phaseSegments = {"G01-L1-S0", "G03-L1-S0"};
	Matrix4d factor;
	factor <<
		0.8, 0.0, 0.0, 0.0,
		0.2, 0.7, 0.0, 0.0,
		0.3, 0.1, 0.6, 0.0,
		0.1, 0.2, 0.2, 0.5;
	const Matrix4d rawCovariance = factor * factor.transpose();
	Vector2d clockMean(4.0, 7.0);
	Vector2d phaseMean(-2.0, 3.0);
	ZhangHybridRealGaugeTransport clockGauge;
	ZhangHybridRealGaugeTransport phaseGauge;
	const auto clock = clockGauge.transport(
		satellites, clockSegments, clockMean,
		rawCovariance.block<2, 2>(0, 0));
	const auto phase = phaseGauge.transport(
		satellites, phaseSegments, phaseMean,
		rawCovariance.block<2, 2>(2, 2));
	BOOST_REQUIRE(clock.valid);
	BOOST_REQUIRE(phase.valid);

	Matrix4d fullTransform = Matrix4d::Zero();
	fullTransform.block<2, 2>(0, 0) = clock.transform;
	fullTransform.block<2, 2>(2, 2) = phase.transform;
	const Matrix4d propagated =
		fullTransform * rawCovariance * fullTransform.transpose();
	const Matrix2d expectedCross = clock.transform
		* rawCovariance.block<2, 2>(0, 2)
		* phase.transform.transpose();
	BOOST_CHECK_SMALL(
		(propagated.block<2, 2>(0, 2) - expectedCross).norm(), 1e-14);
	BOOST_CHECK_GT(
		(propagated.block<2, 2>(0, 2)
			- rawCovariance.block<2, 2>(0, 2)).norm(),
		1e-6);
}

BOOST_AUTO_TEST_CASE(e29_dual_frequency_partition_is_component_intersection)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g02(E_Sys::GPS, 2);
	SatSys g03(E_Sys::GPS, 3);
	SatSys g04(E_Sys::GPS, 4);
	const std::map<SatSys, std::string> l1 = {
		{g01, "L1-A"}, {g02, "L1-A"}, {g03, "L1-A"}, {g04, "L1-B"}};
	const std::map<SatSys, std::string> l2 = {
		{g01, "L2-X"}, {g02, "L2-X"}, {g03, "L2-Y"}, {g04, "L2-X"}};
	const auto components = zhangHybridDualFrequencyComponents(l1, l2);
	BOOST_REQUIRE_EQUAL(components.size(), 1);
	const auto& members = components.begin()->second;
	BOOST_CHECK_EQUAL(members.size(), 2);
	BOOST_CHECK(members.count(g01));
	BOOST_CHECK(members.count(g02));
	BOOST_CHECK(!members.count(g03));
	BOOST_CHECK(!members.count(g04));
}

BOOST_AUTO_TEST_CASE(e27_if_wl_conditioning_uses_the_full_cross_covariance)
{
    const Vector2d ifMean(17.35, -4.20);
    const Vector2d wideLaneMean(3.08, -1.12);
    const Vector2d fixedWideLane(3, -1);
    Matrix2d ifCovariance;
    ifCovariance << 0.40, 0.08,
                    0.08, 0.30;
    Matrix2d wideLaneCovariance;
    wideLaneCovariance << 0.20, 0.03,
                          0.03, 0.16;
    Matrix2d crossCovariance;
    crossCovariance << 0.050, -0.010,
                       0.015,  0.040;
    constexpr double coefficient = -0.73;

    const auto conditioned = zhangConditionFirstIntegerGivenWideLane(
        ifMean, ifCovariance, wideLaneMean, wideLaneCovariance,
        crossCovariance, fixedWideLane, coefficient);
    BOOST_REQUIRE_MESSAGE(conditioned.valid, conditioned.failureReason);
    const Matrix2d inverse = wideLaneCovariance.inverse();
    const Vector2d expectedMean = ifMean + coefficient * fixedWideLane +
        crossCovariance * inverse * (fixedWideLane - wideLaneMean);
    const Matrix2d expectedCovariance = ifCovariance -
        crossCovariance * inverse * crossCovariance.transpose();
    BOOST_CHECK_SMALL((conditioned.mean - expectedMean).norm(), 1e-13);
    BOOST_CHECK_SMALL(
        (conditioned.covariance - expectedCovariance).norm(), 1e-13);
    BOOST_CHECK_LT(conditioned.covariance.trace(), ifCovariance.trace());

    const auto independentApproximation =
        zhangConditionFirstIntegerGivenWideLane(
            ifMean, ifCovariance, wideLaneMean, wideLaneCovariance,
            Matrix2d::Zero(), fixedWideLane, coefficient);
    BOOST_REQUIRE(independentApproximation.valid);
    BOOST_CHECK_SMALL(
        (independentApproximation.covariance - ifCovariance).norm(), 1e-14);
    BOOST_CHECK_GT(
        (independentApproximation.mean - conditioned.mean).norm(), 1e-4);
}

BOOST_AUTO_TEST_CASE(e27_wide_lane_raw_noise_sensitivity_reconstructs_covariance)
{
    ZhangIfWideLaneAccumulator accumulator(3600, 60, 32);
    const std::vector<int> satellites = {3, 7, 11};
    for (int satellite : satellites)
    {
        accumulator.setArcVersion(satellite, 1);
    }
    const Vector3d physical(10.2, 15.2, 7.2);
    const Vector3d variances(0.04, 0.09, 0.16);
    const Matrix3d rawDesign = Matrix3d::Identity();
    const Matrix3d covariance = variances.asDiagonal();
    for (int epoch = 0; epoch < 4; epoch++)
    {
        const std::vector<std::string> keys = {
            "E" + std::to_string(epoch) + "-G03",
            "E" + std::to_string(epoch) + "-G07",
            "E" + std::to_string(epoch) + "-G11"};
        accumulator.addEpoch(
            epoch * 60, satellites, physical, covariance,
            keys, variances, rawDesign);
    }
    const auto estimate = accumulator.estimate(satellites, 3, 240);
    BOOST_REQUIRE_MESSAGE(estimate.valid, estimate.failureReason);
    BOOST_REQUIRE_EQUAL(estimate.noiseSensitivity.size(), 12);
    Matrix2d reconstructed = Matrix2d::Zero();
    for (const auto& [key, sensitivity] : estimate.noiseSensitivity)
    {
        reconstructed += estimate.noiseVariance.at(key) *
            sensitivity * sensitivity.transpose();
    }
    BOOST_CHECK_SMALL(
        (reconstructed - estimate.covariance).norm(), 1e-12);

    const auto exchanged = accumulator.estimate(satellites, 11, 240);
    BOOST_REQUIRE(exchanged.valid);
    Matrix2d transform;
    // ref=3 targets [7,11]; ref=11 targets [3,7].
    transform << 0, -1,
                 1, -1;
    for (const auto& [key, sensitivity] : estimate.noiseSensitivity)
    {
        BOOST_REQUIRE(exchanged.noiseSensitivity.count(key));
        BOOST_CHECK_SMALL(
            (exchanged.noiseSensitivity.at(key) -
             transform * sensitivity).norm(), 1e-12);
    }
}

BOOST_AUTO_TEST_CASE(e27_wide_lane_window_is_reference_invariant_and_arc_safe)
{
    ZhangIfWideLaneAccumulator accumulator(3600, 60, 32);
    for (int satellite : {3, 7, 11, 19})
    {
        accumulator.setArcVersion(satellite, 1);
    }
    const std::vector<int> satellites = {3, 7, 11, 19};
    const VectorXd physical = (VectorXd(4) << 10.2, 15.2, 7.2, 21.2).finished();
    MatrixXd covariance = MatrixXd::Constant(4, 4, 0.01);
    covariance.diagonal().array() += 0.04;
    for (int epoch = 0; epoch < 10; epoch++)
    {
        accumulator.addEpoch(epoch * 60, satellites, physical, covariance);
    }
    const auto reference3 = accumulator.estimate(satellites, 3, 600);
    const auto reference11 = accumulator.estimate(satellites, 11, 600);
    BOOST_REQUIRE(reference3.valid && reference11.valid);

    // Transform SDs relative to G03 into SDs relative to G11 exactly.
    MatrixXd exchange = MatrixXd::Zero(3, 3);
    // ref=3 targets [7,11,19]; ref=11 targets [3,7,19].
    exchange.row(0) << 0, -1, 0;
    exchange.row(1) << 1, -1, 0;
    exchange.row(2) << 0, -1, 1;
    BOOST_CHECK_SMALL(
        (reference11.mean - exchange * reference3.mean).norm(), 1e-12);
    BOOST_CHECK_SMALL(
        (reference11.covariance -
         exchange * reference3.covariance * exchange.transpose()).norm(),
        1e-12);

    // A real physical arc change invalidates old factors involving G11.
    accumulator.setArcVersion(11, 2);
    const auto changedArc = accumulator.estimate(satellites, 3, 600);
    BOOST_CHECK(!changedArc.valid);
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
		ZhangIntegerLatticeMembership smith =
			zhangIntegerRowLatticeContains(
				target.matrix,
				ZhangExactVector(target.currentChords.size()));
		BOOST_CHECK_EQUAL(smith.rank, target.matrix.size());
		for (const auto& invariant : smith.smithInvariants)
		{
			BOOST_CHECK(zhangExactAbs(invariant) == 1);
		}
        ZhangExactVector corrected = satelliteDatum(basis);
        ZhangExactVector correction =
            zhangExactMatrixTimesColumn(target.matrix, cycleValues(basis));
        for (std::size_t row = 0; row < corrected.size(); row++)
        {
            corrected[row] += correction[row];
        }
        BOOST_CHECK(corrected == productDatum);
    }

    // A product tree needs all target satellites but not every estimation
    // receiver.  The same exact projection must close when R2 remains in the
    // current state graph but is deliberately absent from the product core.
    std::set<ZhangGraphEdge> coreEdges;
    for (const auto& edge : edges)
    {
        if (edge.receiver != "R2")
        {
            coreEdges.insert(edge);
        }
    }
    ZhangGraphBasis coreProduct = zhangBuildSpanningTree(
        coreEdges, "R0",
        {{"R0", g02}, {"R1", g01}, {"R1", g03}});
    BOOST_REQUIRE(coreProduct.connected);
    BOOST_CHECK_LT(coreProduct.receivers.size(), basisA.receivers.size());
    const ZhangExactVector coreProductDatum = satelliteDatum(coreProduct);
    for (const auto& basis : {basisA, basisB})
    {
        ZhangSatelliteProductTarget target =
            zhangBuildSatelliteProductTarget(basis, coreProduct, g01);
        BOOST_REQUIRE_MESSAGE(target.valid, target.failureReason);
        ZhangExactVector corrected = satelliteDatum(basis);
        ZhangExactVector correction = zhangExactMatrixTimesColumn(
            target.matrix, cycleValues(basis));
        for (std::size_t row = 0; row < corrected.size(); row++)
        {
            corrected[row] += correction[row];
        }
        BOOST_CHECK(corrected == coreProductDatum);
    }
}

BOOST_AUTO_TEST_CASE(product_relation_basis_expands_independently_to_physical_arcs)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    const std::set<ZhangGraphEdge> edges = {
        {"R0", g01}, {"R0", g02}, {"R1", g01}, {"R1", g02}
    };
    const ZhangGraphBasis current = zhangBuildSpanningTree(edges, "R0");
    const ZhangGraphBasis product = zhangBuildSpanningTree(
        edges,
        "R0",
        {{"R0", g01}, {"R1", g01}, {"R1", g02}});
    const ZhangProductRelationBasis relationBasis =
        ProductRelationBasisBuilder::build(current, product, g01);

    BOOST_REQUIRE_MESSAGE(relationBasis.valid, relationBasis.failureReason);
    BOOST_CHECK_EQUAL(relationBasis.namedRelationCount, 1);
    BOOST_CHECK_EQUAL(relationBasis.exactRank, 1);
    BOOST_CHECK(relationBasis.independentNamedIndices == std::vector<int>({0}));
    BOOST_CHECK(relationBasis.primitive);
    BOOST_CHECK(relationBasis.saturationIndex == 1);
    BOOST_CHECK(relationBasis.admissibleCompletionProven);
    BOOST_CHECK(relationBasis.networkLatticeContained);
    BOOST_CHECK(relationBasis.networkClosureExactZero);
    BOOST_CHECK(
        zhangExactMultiply(
            relationBasis.networkContainmentTransform,
            relationBasis.networkIntegerBasis) ==
        relationBasis.exactRowBasis);
    BOOST_CHECK(relationBasis.nuisanceOrthogonal);
    BOOST_CHECK(relationBasis.physicalExpansionValid);
    BOOST_REQUIRE_EQUAL(relationBasis.namedRelations.size(), 1);
    const auto& relation = relationBasis.namedRelations.front();
    BOOST_CHECK(relation.satellite == g02);
    BOOST_CHECK(relation.referenceSatellite == g01);
    BOOST_CHECK(relation.currentCycleCoefficients == ZhangExactVector({1}));
    BOOST_CHECK(std::all_of(
        relation.nuisanceCoefficients.begin(),
        relation.nuisanceCoefficients.end(),
        [](const auto& coefficient) { return coefficient == 0; }));

    BOOST_REQUIRE_EQUAL(relationBasis.currentChords.size(), 1);
    const auto expectedCycle = zhangFundamentalCycle(
        current, relationBasis.currentChords.front());
    std::map<ZhangGraphEdge, ZhangExactInteger> expectedPhysical;
    for (const auto& [edge, coefficient] : expectedCycle)
    {
        expectedPhysical[edge] += coefficient;
    }
    BOOST_CHECK(relation.physicalArcCoefficients == expectedPhysical);
}

BOOST_AUTO_TEST_CASE(product_relation_basis_is_reference_invariant)
{
    const SatSys g01(E_Sys::GPS, 1);
    const SatSys g02(E_Sys::GPS, 2);
    const SatSys g03(E_Sys::GPS, 3);
    const std::set<ZhangGraphEdge> edges = {
        {"R0", g01}, {"R0", g02}, {"R0", g03},
        {"R1", g01}, {"R1", g02}, {"R1", g03}
    };
    const ZhangGraphBasis current = zhangBuildSpanningTree(edges, "R0");
    const ZhangGraphBasis product = zhangBuildSpanningTree(
        edges,
        "R0",
        {{"R0", g01}, {"R1", g01}, {"R1", g02}, {"R1", g03}});

    const auto relativeToG01 = ProductRelationBasisBuilder::build(
        current, product, g01, E_Sys::GPS, E_ObsCode::L1C);
    const auto relativeToG02 = ProductRelationBasisBuilder::build(
        current, product, g02, E_Sys::GPS, E_ObsCode::L1C);
    BOOST_REQUIRE_MESSAGE(relativeToG01.valid, relativeToG01.failureReason);
    BOOST_REQUIRE_MESSAGE(relativeToG02.valid, relativeToG02.failureReason);
    BOOST_CHECK_EQUAL(relativeToG01.fullTargetRank, 2);
    BOOST_CHECK_EQUAL(relativeToG02.fullTargetRank, 2);
    BOOST_CHECK(relativeToG01.referenceSatellite == g01);
    BOOST_CHECK(relativeToG02.referenceSatellite == g02);

    // The named coordinate matrices differ with the reference, but their
    // canonical physical row lattice must be exactly identical.
    BOOST_CHECK(relativeToG01.exactRowBasis == relativeToG02.exactRowBasis);
    BOOST_CHECK_EQUAL(relativeToG01.exactHnf, relativeToG02.exactHnf);
    BOOST_CHECK(relativeToG01.networkClosureExactZero);
    BOOST_CHECK(relativeToG02.networkClosureExactZero);
    BOOST_CHECK(
        zhangExactMultiply(
            relativeToG01.networkContainmentTransform,
            relativeToG01.networkIntegerBasis) ==
        relativeToG01.exactRowBasis);
    BOOST_CHECK(
        zhangExactMultiply(
            relativeToG02.networkContainmentTransform,
            relativeToG02.networkIntegerBasis) ==
        relativeToG02.exactRowBasis);
}

BOOST_AUTO_TEST_CASE(product_relation_basis_fails_closed_on_graph_mismatch)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    SatSys g03(E_Sys::GPS, 3);
    const std::set<ZhangGraphEdge> currentEdges = {
        {"R0", g01}, {"R0", g02}, {"R1", g01}, {"R1", g02}
    };
    const std::set<ZhangGraphEdge> productEdges = {
        {"R0", g01}, {"R0", g03}, {"R1", g01}, {"R1", g03}
    };
    const auto relationBasis = ProductRelationBasisBuilder::build(
        zhangBuildSpanningTree(currentEdges, "R0"),
        zhangBuildSpanningTree(productEdges, "R0"),
        g01);
    BOOST_CHECK(!relationBasis.valid);
    BOOST_CHECK_EQUAL(
        relationBasis.failureReason,
        "product_tree_target_node_or_root_mismatch");
}

BOOST_AUTO_TEST_CASE(product_transition_transport_includes_exact_affine_offset)
{
	ZhangProductIntegerFunctional previous;
	ZhangProductIntegerFunctional current;
	previous.satellite = SatSys(E_Sys::GPS, 7);
	current.satellite = previous.satellite;
	previous.valid = true;
	current.valid = true;
	previous.affineOffsetCycles = -4;
	current.affineOffsetCycles = 9;
	const auto transition = zhangProductIntegerFunctionalDifference(
		previous, current);
	BOOST_REQUIRE(transition.valid);
	BOOST_CHECK(transition.coefficients.empty());
	BOOST_CHECK(transition.affineOffsetCycles == 13);
	BOOST_CHECK(zhangCompleteProductTransitionInteger(transition, 21) == 34);
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

BOOST_AUTO_TEST_CASE(frontend_integer_gauge_birth_defines_zero_kappa_component)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g02(E_Sys::GPS, 2);
    SatSys g03(E_Sys::GPS, 3);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1C);
	// Estimator warm-up may replace a physical segment before the broadcast
	// frontend has a fixed integer from which to define its t0.
	manager.recordSatelliteDiscontinuity(g03);

    const auto initial = manager.initialiseFrontendGaugeComponent(
        {g03, g01, g02});
    BOOST_REQUIRE_MESSAGE(initial.accepted, initial.reason);
    BOOST_CHECK_EQUAL(initial.satelliteCount, 3);
    BOOST_CHECK_EQUAL(initial.relationCount, 2);

    long long difference = 99;
    BOOST_REQUIRE(manager.relation(g01, g02, difference));
    BOOST_CHECK_EQUAL(difference, 0);
    BOOST_REQUIRE(manager.relation(g01, g03, difference));
    BOOST_CHECK_EQUAL(difference, 0);
    const auto status = manager.status(g03, true);
    BOOST_CHECK_EQUAL(status.componentSize, 3);
    BOOST_CHECK_EQUAL(status.componentRank, 2);
    BOOST_CHECK(status.integerDatumContinuous);
    BOOST_CHECK(status.integerPrecisionValid);
    BOOST_CHECK(status.integerValid);

    const auto repeated = manager.initialiseFrontendGaugeComponent(
        {g01, g02, g03});
    BOOST_CHECK(!repeated.accepted);
    BOOST_CHECK_EQUAL(
        repeated.reason, "FRONTEND_GAUGE_ALREADY_INITIALISED");
    BOOST_CHECK_EQUAL(manager.relationCount(), 2);
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

BOOST_AUTO_TEST_CASE(hybrid_broadcast_component_metadata_is_persistent_and_exact)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g03(E_Sys::GPS, 3);
	SatSys g12(E_Sys::GPS, 12);
	ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1C);
	BOOST_CHECK_EQUAL(
		zhangHybridPhaseProductSegmentId(g03, E_ObsCode::L1C, 0),
		"G03-L1C-SEG0");
	BOOST_REQUIRE(manager.promoteRelation(g01, g03, 4, "edge_13"));
	BOOST_REQUIRE(manager.promoteRelation(g03, g12, -7, "edge_312"));
	BOOST_REQUIRE(manager.promoteRelation(g01, g12, -3, "cycle_112"));
	const auto before = manager.status(g03, true);
	BOOST_CHECK(before.integerValid);
	BOOST_CHECK_EQUAL(before.componentSize, 3);
	BOOST_CHECK_EQUAL(before.componentRank, 2);
	BOOST_CHECK_EQUAL(before.certifiedRelationCount, 3);
	BOOST_CHECK_EQUAL(before.redundantRelationCount, 1);
	BOOST_CHECK(before.cycleClosureValid);
	BOOST_CHECK_GT(before.componentVersion, 0);

	// An exact backend coordinate shift transports integer potentials but is
	// not a frontend physical-segment event.
	manager.applyDynamicTreeShift(g03, 2);
	const auto transported = manager.status(g03, true);
	BOOST_CHECK_EQUAL(
		transported.componentVersion, before.componentVersion);
	BOOST_CHECK_EQUAL(
		transported.alignmentGeneration, before.alignmentGeneration);
	BOOST_CHECK_EQUAL(transported.phaseSegment, before.phaseSegment);

	manager.recordSatelliteDiscontinuity(g03);
	const auto after = manager.status(g03, true);
	BOOST_CHECK_GT(after.componentVersion, before.componentVersion);
	BOOST_CHECK_GT(after.alignmentGeneration, before.alignmentGeneration);
	BOOST_CHECK_EQUAL(after.phaseSegment, before.phaseSegment + 1);
	BOOST_CHECK_EQUAL(after.componentSize, 1);
	BOOST_CHECK(!after.integerValid);
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

BOOST_AUTO_TEST_CASE(certified_temporal_batch_restores_quarantined_frontend)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g07(E_Sys::GPS, 7);
	SatSys g23(E_Sys::GPS, 23);
	ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
	BOOST_REQUIRE(manager.promoteRelation(g01, g07, 12, "initial_07"));
	BOOST_REQUIRE(manager.promoteRelation(g01, g23, -94, "initial_23"));
	BOOST_REQUIRE_EQUAL(
		manager.quarantineCurrentAlignments({g07, g23}, g01), 2);

	// The common +2 cycle raw shift is an unobservable component gauge.
	// Relative changes are +5 for G07 and -3 for G23, so kappa must change
	// by -5 and +3 respectively to keep raw+lambda*kappa invariant.
	const auto restored = manager.applyCertifiedTemporalTransform({
		{g01, 2}, {g07, 7}, {g23, -1}});
	BOOST_REQUIRE_MESSAGE(restored.accepted, restored.reason);
	BOOST_CHECK_EQUAL(restored.restoredSatellites, 2);
	BOOST_CHECK(manager.status(g07, true).integerValid);
	BOOST_CHECK(manager.status(g23, true).integerValid);
	long long difference = 0;
	BOOST_REQUIRE(manager.relation(g01, g07, difference));
	BOOST_CHECK_EQUAL(difference, 7);
	BOOST_REQUIRE(manager.relation(g01, g23, difference));
	BOOST_CHECK_EQUAL(difference, -91);
}

BOOST_AUTO_TEST_CASE(certified_temporal_batch_fails_without_aligned_anchor)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g07(E_Sys::GPS, 7);
	ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
	BOOST_REQUIRE(manager.promoteRelation(g01, g07, 12, "initial"));
	BOOST_REQUIRE_EQUAL(
		manager.quarantineCurrentAlignments({g01, g07}), 2);
	const auto before = manager.checkpointState();
	const auto rejected = manager.applyCertifiedTemporalTransform({{g07, 5}});
	BOOST_CHECK(!rejected.accepted);
	BOOST_CHECK_EQUAL(rejected.reason, "NO_ALIGNED_COMPONENT_ANCHOR");
	BOOST_CHECK(manager.checkpointState().alignmentCycles == before.alignmentCycles);
	BOOST_CHECK(manager.checkpointState().alignmentKnown == before.alignmentKnown);
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

BOOST_AUTO_TEST_CASE(hybrid_tree_invariance_is_relative_then_real_gauge)
{
    SatSys g01(E_Sys::GPS, 1);
    SatSys g03(E_Sys::GPS, 3);
    SatSys g22(E_Sys::GPS, 22);
    ZhangSatelliteDatumManager manager(E_Sys::GPS, E_ObsCode::L1W);
    BOOST_REQUIRE(manager.promoteRelation(g01, g03, 9, "fixed_13"));
    BOOST_REQUIRE(manager.promoteRelation(g01, g22, -3, "fixed_122"));
    constexpr double lambda = 0.190293672798365;
    const std::map<SatSys, double> cycleChanges = {
        {g01, 111.76080459977078},
        {g03, 114.76080459977078},
        {g22, 107.76080459977078}
    };
    std::map<SatSys, ZhangSatelliteDatumStatus> before;
    for (const auto& [satellite, ignored] : cycleChanges)
    {
        before[satellite] = manager.status(satellite, true);
    }
    const auto preserved = manager.applyDynamicTreeTransform(cycleChanges);
    std::vector<ZhangHybridTreeTransformSample> samples;
    for (const auto& [satellite, change] : cycleChanges)
    {
        const auto after = manager.status(satellite, true);
        const auto& old = before.at(satellite);
        samples.push_back({
            satellite, old.componentId, after.componentId, -lambda * change,
            old.alignmentCycles, after.alignmentCycles,
            old.phaseSegment, after.phaseSegment,
            old.datumVersion, after.datumVersion,
            old.componentVersion, after.componentVersion,
            old.alignmentGeneration, after.alignmentGeneration,
            preserved.at(satellite)
        });
    }
    const auto audit = zhangHybridTreeTransformInvariance(samples, lambda);
    BOOST_REQUIRE_EQUAL(audit.size(), 3);
    const double expectedCommon = -lambda * cycleChanges.at(g01);
    for (const auto& row : audit)
    {
        BOOST_CHECK(row.valid);
        BOOST_CHECK(row.invariant);
        BOOST_CHECK_EQUAL(row.reason, "INVARIANT");
        BOOST_CHECK_EQUAL(row.componentSupportCount, 3);
        BOOST_CHECK_SMALL(row.relativeFrontendDeltaMetres, 1e-12);
        BOOST_CHECK_SMALL(
            row.componentCommonDeltaMetres - expectedCommon, 1e-12);
        BOOST_CHECK_SMALL(
            row.expectedRealGaugeShiftMetres + expectedCommon, 1e-12);
		BOOST_CHECK(row.hybridClosureMachineZero);
		BOOST_CHECK_SMALL(row.hybridClosureResidualMetres, 1e-12);
    }

    // The second half of the frontend absorbs the one remaining component
    // common mode and leaves the broadcast products exactly unchanged.
    ZhangHybridRealGaugeTransport gauge;
    const std::vector<SatSys> satellites = {g01, g03, g22};
    const std::vector<std::string> segments = {
        "G01-L1W-SEG0", "G03-L1W-SEG0", "G22-L1W-SEG0"};
    VectorXd oldFrontend(3);
    oldFrontend << 0.2, -0.4, 0.2;
    const MatrixXd covariance = MatrixXd::Identity(3, 3);
    const auto first = gauge.transport(
        satellites, segments, oldFrontend, covariance);
    BOOST_REQUIRE(first.valid);
    const VectorXd shiftedFrontend =
        oldFrontend + VectorXd::Constant(3, expectedCommon);
    const auto second = gauge.transport(
        satellites, segments, shiftedFrontend, covariance);
    BOOST_REQUIRE(second.valid);
    BOOST_CHECK_EQUAL(second.overlapCount, 3);
    BOOST_CHECK_SMALL(
        second.commonShiftMetres + expectedCommon, 1e-12);
    BOOST_CHECK_SMALL((second.values - first.values).norm(), 1e-12);

    // One member losing integer alignment must fail closed even when the
    // remaining members still share a valid common real gauge.
    samples[1].alignmentPreserved = false;
    const auto suspended = zhangHybridTreeTransformInvariance(samples, lambda);
    BOOST_CHECK(!suspended[1].invariant);
    BOOST_CHECK_EQUAL(suspended[1].reason, "ALIGNMENT_SUSPENDED");
}

BOOST_AUTO_TEST_CASE(hybrid_pure_s_basis_event_closes_integer_and_real_gauges)
{
	constexpr double lambda = 0.190293672798365;
	constexpr long long backendIntegerGauge = 3;
	constexpr double gamma = -0.047;
	const double backendDelta = lambda * backendIntegerGauge + gamma;
	const auto closure = zhangHybridPureCoordinateClosure(
		lambda, backendIntegerGauge, gamma, backendDelta, 1e-12);
	BOOST_REQUIRE(closure.valid);
	BOOST_CHECK(closure.machineZero);
	BOOST_CHECK_EQUAL(closure.reason, "PURE_COORDINATE_INVARIANT");
	BOOST_CHECK_SMALL(closure.hybridResidualMetres, 1e-12);
	BOOST_CHECK_SMALL(
		closure.integerCompensationMetres + lambda * backendIntegerGauge,
		1e-12);
	BOOST_CHECK_SMALL(closure.realGaugeCompensationMetres + gamma, 1e-12);

	const auto rejected = zhangHybridPureCoordinateClosure(
		lambda, backendIntegerGauge, gamma, backendDelta + 1e-3, 1e-12);
	BOOST_CHECK(!rejected.valid);
	BOOST_CHECK_EQUAL(
		rejected.reason, "BACKEND_INTEGER_REAL_DECOMPOSITION_MISMATCH");
}

BOOST_AUTO_TEST_CASE(real_gauge_transport_audit_separates_coordinate_and_time)
{
	Vector2d raw;
	raw << 0.31, 0.29;
	Vector2d integerRemoved = Vector2d::Zero();
	Matrix2d posterior;
	posterior << 4, 1, 1, 2;
	auto pure = zhangAuditRealGaugeTransport(
		ZhangRealGaugeTransportEventKind::
			SAME_POSTERIOR_COORDINATE_TRANSFORM,
		raw, integerRemoved, posterior, posterior, posterior);
	BOOST_REQUIRE_MESSAGE(pure.valid, pure.failureReason);
	BOOST_CHECK(pure.samePosteriorEvent);
	BOOST_CHECK(pure.differenceCovarianceMachineZero);
	BOOST_CHECK_SMALL(pure.glsShiftVariance, 1e-15);
	BOOST_CHECK_SMALL(pure.realShiftMetres - 0.30, 1e-15);

	Matrix2d oldCovariance;
	oldCovariance << 4, 1, 1, 3;
	Matrix2d newCovariance;
	newCovariance << 5, 1.5, 1.5, 4;
	Matrix2d crossCovariance;
	crossCovariance << 3, 0.5, 0.75, 2;
	auto temporal = zhangAuditRealGaugeTransport(
		ZhangRealGaugeTransportEventKind::CROSS_EPOCH_TRANSPORT,
		raw, integerRemoved, oldCovariance, newCovariance,
		crossCovariance);
	BOOST_REQUIRE_MESSAGE(temporal.valid, temporal.failureReason);
	const Matrix2d expectedDifference = newCovariance + oldCovariance
		- crossCovariance - crossCovariance.transpose();
	BOOST_CHECK_SMALL(
		(temporal.differenceCovariance - expectedDifference).norm(), 1e-14);
	BOOST_CHECK_GT(temporal.glsShiftVariance, 0);

	// Marginals without Q-+ are not an admissible temporal audit input.
	auto missingCross = zhangAuditRealGaugeTransport(
		ZhangRealGaugeTransportEventKind::CROSS_EPOCH_TRANSPORT,
		raw, integerRemoved, oldCovariance, newCovariance, MatrixXd());
	BOOST_CHECK(!missingCross.valid);
	BOOST_CHECK_EQUAL(missingCross.failureReason,
		"REAL_GAUGE_AUDIT_REQUIRES_FULL_JOINT_MARGINAL");
}

BOOST_AUTO_TEST_CASE(hybrid_server_to_user_dual_frequency_integer_theorem_closes)
{
	auto closure = zhangHybridUserIntegerClosure(
		ZhangExactInteger(105), ZhangExactInteger(77),
		ZhangExactInteger(88), ZhangExactInteger(61),
		ZhangExactInteger(12), ZhangExactInteger(9),
		ZhangExactInteger(-4), ZhangExactInteger(-8),
		true, true);
	BOOST_REQUIRE_MESSAGE(closure.valid, closure.failureReason);
	BOOST_CHECK_EQUAL(closure.firstSignalSatelliteSd, 31);
	BOOST_CHECK_EQUAL(closure.secondSignalSatelliteSd, 31);
	BOOST_CHECK_EQUAL(closure.wideLaneSatelliteSd, 0);
	BOOST_CHECK(closure.admissibleDualFrequencyTransform);
	BOOST_CHECK(closure.exactInverseClosure);

	auto disconnected = zhangHybridUserIntegerClosure(
		1, 0, 1, 0, 0, 0, 0, 0, false, true);
	BOOST_CHECK(!disconnected.valid);
	BOOST_CHECK_EQUAL(disconnected.failureReason,
		"USER_SATELLITES_NOT_IN_SAME_INTEGER_COMPONENT");

	auto uncertified = zhangHybridUserIntegerClosure(
		1, 0, 1, 0, 0, 0, 0, 0, true, false);
	BOOST_CHECK(!uncertified.valid);
	BOOST_CHECK_EQUAL(uncertified.failureReason,
		"SERVER_INTEGER_RELATION_NOT_CERTIFIED");
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
    BOOST_CHECK_EQUAL(dynamic.phaseSegment, initial.phaseSegment);
    BOOST_CHECK_EQUAL(dynamic.discontinuityCounter, initial.discontinuityCounter);
    BOOST_CHECK(dynamic.integerDatumContinuous);

    manager.recordSatelliteDiscontinuity(g03);
    auto discontinuous = manager.status(g03, true);
    BOOST_CHECK_EQUAL(discontinuous.datumVersion, initial.datumVersion + 1);
    BOOST_CHECK_EQUAL(discontinuous.phaseSegment, initial.phaseSegment + 1);
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

BOOST_AUTO_TEST_CASE(exact_physical_row_hnf_tracks_a_basis_invariant_search_frame)
{
    const ZhangExactMatrix physicalRows = {
        { 1, -1,  0,  1,  0},
        { 0,  1, -1,  0,  1},
        { 0,  0,  0,  1, -1},
    };
    const ZhangExactMatrix unimodular = {
        {1,  1, 0},
        {0,  1, 1},
        {0, -1, 0},
    };
    const ZhangExactMatrix rebasedRows = zhangExactMultiply(
        unimodular, physicalRows);

    const ZhangExactRowHnf base = zhangExactRowHermiteNormalForm(
        physicalRows, {}, true);
    const ZhangExactRowHnf rebased = zhangExactRowHermiteNormalForm(
        rebasedRows, {}, true);
    BOOST_REQUIRE(base.consistent);
    BOOST_REQUIRE(rebased.consistent);
    BOOST_REQUIRE_EQUAL(base.basis.size(), physicalRows.size());
    BOOST_REQUIRE_EQUAL(rebased.basis.size(), physicalRows.size());
    BOOST_CHECK(base.basis == rebased.basis);
    BOOST_CHECK(
        zhangExactMultiply(base.rowTransform, physicalRows) == base.basis);
    BOOST_CHECK(
        zhangExactMultiply(rebased.rowTransform, rebasedRows) ==
        rebased.basis);
    BOOST_CHECK(
        zhangExactMultiply(rebased.rowTransform, unimodular) ==
        base.rowTransform);

    const ZhangExactVector currentState = {7, -2, 5};
    const ZhangExactVector rebasedState =
        zhangExactMatrixTimesColumn(unimodular, currentState);
    BOOST_CHECK(
        zhangExactMatrixTimesColumn(base.rowTransform, currentState) ==
        zhangExactMatrixTimesColumn(
            rebased.rowTransform, rebasedState));
}

BOOST_AUTO_TEST_CASE(iar_gain_low_rank_covariance_matches_dense_conditioning)
{
    Matrix4d covariance;
    covariance <<
        4.0, 0.8, 0.3, 0.1,
        0.8, 3.0, 0.4, 0.2,
        0.3, 0.4, 2.0, 0.5,
        0.1, 0.2, 0.5, 1.5;
    Matrix<double, 2, 4> denseConstraints;
    denseConstraints <<
        1, -1, 0, 0,
        0,  1, 1, -1;
    ZhangIarFunctional constraints = denseConstraints.sparseView();
    ZhangIarCovarianceCondition condition =
        zhangIarCovarianceCondition(covariance, constraints);
    BOOST_REQUIRE(condition.valid);
    BOOST_CHECK_EQUAL(condition.rank, 2);

    const Matrix2d constraintCovariance =
        denseConstraints * covariance * denseConstraints.transpose();
    const Matrix4d densePosterior = covariance -
        covariance * denseConstraints.transpose() *
        constraintCovariance.inverse() * denseConstraints * covariance;
    const Matrix4d factorPosterior = covariance -
        condition.reductionFactor * condition.reductionFactor.transpose();
    BOOST_CHECK_SMALL(
        (densePosterior - factorPosterior).norm(), 1e-11);

    Matrix<double, 2, 4> denseTarget;
    denseTarget <<
        1, 0, -1, 0,
        0, 1,  0, -1;
    ZhangIarFunctional target = denseTarget.sparseView();
    const double auditedTrace = zhangIarProjectedCovarianceTrace(
        covariance, condition, target);
    const double denseTrace =
        (denseTarget * densePosterior * denseTarget.transpose()).trace();
    BOOST_CHECK_SMALL(auditedTrace - denseTrace, 1e-11);

    Matrix2d unimodular;
    unimodular << 1, 1, 0, 1;
    Matrix<double, 2, 4> denseRebased =
        unimodular * denseConstraints;
    ZhangIarFunctional rebased = denseRebased.sparseView();
    ZhangIarCovarianceCondition rebasedCondition =
        zhangIarCovarianceCondition(covariance, rebased);
    BOOST_REQUIRE(rebasedCondition.valid);
    const double rebasedTrace = zhangIarProjectedCovarianceTrace(
        covariance, rebasedCondition, target);
    BOOST_CHECK_SMALL(rebasedTrace - auditedTrace, 1e-11);
}

BOOST_AUTO_TEST_CASE(theory_regression_physical_dd_row_closes_in_cycle_basis)
{
    const SatSys g01(E_Sys::GPS, 1);
    const SatSys g02(E_Sys::GPS, 2);
    const SatSys g03(E_Sys::GPS, 3);
    const std::set<ZhangGraphEdge> edges = {
        {"R0", g01}, {"R0", g02}, {"R0", g03},
        {"R1", g01}, {"R1", g02}, {"R1", g03},
        {"R2", g01}, {"R2", g02}, {"R2", g03}
    };
    const ZhangGraphBasis basis = zhangBuildSpanningTree(edges, "R0");
    BOOST_REQUIRE(basis.connected);
    std::map<ZhangGraphEdge, int> chordColumns;
    for (const ZhangGraphEdge& edge : basis.edges)
    {
        if (!basis.isTreeEdge(edge.receiver, edge.satellite))
        {
            chordColumns[edge] = chordColumns.size();
        }
    }
    BOOST_REQUIRE_EQUAL(chordColumns.size(), 4);

    VectorXd row;
    BOOST_REQUIRE(zhangDdCycleCoordinateRow(
        basis, chordColumns, "R0", "R2", g01, g03, row));
    BOOST_CHECK_EQUAL(row.size(), chordColumns.size());
    BOOST_CHECK(row.allFinite());
    for (double value : row)
    {
        BOOST_CHECK_SMALL(value - std::round(value), 1e-14);
    }
}

BOOST_AUTO_TEST_CASE(theory_regression_detects_stronger_satellite_sd_correlation)
{
    Matrix3d covariance;
    covariance <<
         1.00, -0.30, 0.30,
        -0.30,  1.13, 0.91,
         0.30,  0.91, 1.13;
    Matrix<double, 1, 3> ambiguityDense;
    ambiguityDense << 1, 0, 0;
    Matrix<double, 1, 3> undifferencedDense;
    undifferencedDense << 0, 0, 1;
    Matrix<double, 1, 3> satelliteDifferenceDense;
    satelliteDifferenceDense << 0, -1, 1;
    const ZhangIarFunctional ambiguity = ambiguityDense.sparseView();
    const ZhangIarFunctional undifferenced =
        undifferencedDense.sparseView();
    const ZhangIarFunctional satelliteDifference =
        satelliteDifferenceDense.sparseView();

    const ZhangPairedCorrelationSummary ud = zhangPairedCorrelations(
        covariance, ambiguity, undifferenced);
    const ZhangPairedCorrelationSummary sd = zhangPairedCorrelations(
        covariance, ambiguity, satelliteDifference);
    BOOST_REQUIRE(ud.valid);
    BOOST_REQUIRE(sd.valid);
    BOOST_REQUIRE_EQUAL(ud.pairs, 1);
    BOOST_REQUIRE_EQUAL(sd.pairs, 1);
    BOOST_CHECK_SMALL(ud.coefficients.front() -
        0.30 / std::sqrt(1.13), 1e-12);
    BOOST_CHECK_SMALL(sd.coefficients.front() -
        0.60 / std::sqrt(0.44), 1e-12);
    BOOST_CHECK_SMALL(
        ud.pooledCorrelation - ud.coefficients.front(), 1e-12);
    BOOST_CHECK_SMALL(
        sd.pooledCorrelation - sd.coefficients.front(), 1e-12);
    BOOST_CHECK_LT(ud.rmsAbsolute, sd.rmsAbsolute);
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

BOOST_AUTO_TEST_CASE(auxiliary_product_tree_generation_is_not_a_global_product_reset)
{
    ZhangProductDatumVersionTracker tracker;
    BOOST_CHECK(!tracker.observe(17));
    BOOST_CHECK(!tracker.observe(17));
    BOOST_CHECK(tracker.observe(18));
    BOOST_CHECK(!tracker.observe(18));

    ZhangPhaseContinuityState continuity;
    continuity.markFixed();
    BOOST_CHECK(tracker.observe(19));
    BOOST_CHECK_EQUAL(continuity.counter, 0);
    BOOST_CHECK_EQUAL(continuity.datumVersion, 0);
    BOOST_CHECK_EQUAL(continuity.iod, 0);
    BOOST_CHECK(continuity.integerValid());
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

double testScalarRoundErrorProbability(double fractional, double variance)
{
	if (variance < 1e-20)
	{
		return 0;
	}
	double alternateMass = 0;
	const double exponentScale = -0.25 / variance;
	for (int offset = 1; offset < 10; offset++)
	{
		alternateMass += std::exp(
			(offset + 2 * fractional) * offset * exponentScale);
		alternateMass += std::exp(
			(offset - 2 * fractional) * offset * exponentScale);
	}
	return alternateMass / (1 + alternateMass);
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
	BOOST_REQUIRE_GT(first.variance, 0);
	BOOST_REQUIRE_GT(second.variance, 0);
	const long long firstCandidate = std::llround(first.mean);
	const long long secondCandidate = std::llround(second.mean);
	BOOST_CHECK_EQUAL(firstCandidate, secondCandidate);
	const double firstFractional = first.mean - firstCandidate;
	const double secondFractional = second.mean - secondCandidate;
	BOOST_CHECK_SMALL(
		testScalarRoundErrorProbability(firstFractional, first.variance)
		- testScalarRoundErrorProbability(secondFractional, second.variance),
		1e-10);
	BOOST_CHECK_SMALL(
		firstFractional * firstFractional / first.variance
		- secondFractional * secondFractional / second.variance,
		1e-10);
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

BOOST_AUTO_TEST_CASE(temporal_product_snapshots_survive_rectangular_arc_reinitialisation)
{
	KFKey oldArc;
	oldArc.type = KF::AMBIGUITY;
	oldArc.str = "R0";
	oldArc.Sat = SatSys(E_Sys::GPS, 1);
	oldArc.num = static_cast<int>(E_ObsCode::L1C);
	KFKey surviving = oldArc;
	surviving.str = "R1";
	std::vector<ZhangCapturedStateKey> source = {
		zhangCapturedStateKey(surviving), zhangCapturedStateKey(oldArc)};
	std::vector<ZhangCapturedStateKey> destination = {
		zhangCapturedStateKey(surviving)};

	Vector2d priorMean(3.2, -1.4);
	Matrix2d priorCovariance;
	priorCovariance << 0.4, 0.1, 0.1, 0.7;
	KFMeas measurement;
	measurement.H = MatrixXd::Identity(2, 2);
	measurement.V = Vector2d(0.05, -0.02);
	measurement.R = Matrix2d::Identity() * 0.2;
	measurement.obsKeys = {surviving, oldArc};
	const Matrix2d innovation = priorCovariance + measurement.R;
	const Matrix2d gain = priorCovariance * innovation.inverse();
	const Vector2d posteriorMean = priorMean + gain * measurement.V;
	Matrix2d posteriorCovariance = priorCovariance
		- gain * priorCovariance;
	posteriorCovariance = 0.5 *
		(posteriorCovariance + posteriorCovariance.transpose());

	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), source, priorMean, priorCovariance, measurement, "/PPP",
		posteriorMean, posteriorCovariance));
	BOOST_REQUIRE(capture.bindPersistentSnapshot(
		"OLD", "OLD@A0", Vector2d(0, 1), 0));

	SparseMatrix<double> projection(1, 2);
	projection.insert(0, 0) = 1;
	BOOST_REQUIRE(capture.recordCoordinateTransform(
		GTime(), source, destination, projection,
		"local phase-coordinate reinitialisation", true));
	BOOST_REQUIRE(capture.bindPersistentSnapshot(
		"NEW", "NEW@A1", VectorXd::Ones(1), 0));
	const auto marginal = capture.persistentSnapshotMarginal();
	BOOST_REQUIRE_MESSAGE(marginal.valid, marginal.failureReason);
	const auto replayed = capture.replayPersistentSnapshotsKeepingRows(
		[](const ZhangCapturedFactorEvent&, int) { return true; });
	BOOST_REQUIRE_MESSAGE(replayed.valid, replayed.failureReason);
	BOOST_CHECK(marginal.identities == replayed.identities);
	BOOST_CHECK_SMALL((marginal.mean - replayed.mean).norm(), 1e-12);
	BOOST_CHECK_SMALL(
		(marginal.covariance - replayed.covariance).norm(), 1e-12);
	BOOST_REQUIRE_EQUAL(marginal.identities.size(), 2);
	BOOST_CHECK_EQUAL(marginal.identities[0], "OLD");
	BOOST_CHECK_EQUAL(marginal.identities[1], "NEW");
	const double expectedDifference = posteriorMean(0) - posteriorMean(1);
	const double expectedVariance = posteriorCovariance(0, 0)
		+ posteriorCovariance(1, 1) - 2 * posteriorCovariance(0, 1);
	BOOST_CHECK_SMALL(
		(marginal.mean(1) - marginal.mean(0)) - expectedDifference, 1e-12);
	BOOST_CHECK_SMALL(
		(marginal.covariance(1, 1) + marginal.covariance(0, 0)
			 - 2 * marginal.covariance(0, 1)) - expectedVariance, 1e-12);
}

BOOST_AUTO_TEST_CASE(candidate_product_snapshot_is_bound_before_its_coordinate_is_dropped)
{
	KFKey retained;
	retained.type = KF::AMBIGUITY;
	retained.str = "R0";
	retained.Sat = SatSys(E_Sys::GPS, 1);
	retained.num = static_cast<int>(E_ObsCode::L1C);
	KFKey candidateChord = retained;
	candidateChord.str = "R1";
	std::vector<ZhangCapturedStateKey> source = {
		zhangCapturedStateKey(retained),
		zhangCapturedStateKey(candidateChord)};
	std::vector<ZhangCapturedStateKey> destination = {
		zhangCapturedStateKey(retained)};

	Vector2d priorMean(2.4, -0.8);
	Matrix2d priorCovariance;
	priorCovariance << 0.5, 0.12, 0.12, 0.9;
	KFMeas measurement;
	measurement.H = MatrixXd::Identity(2, 2);
	measurement.V = Vector2d(0.03, -0.04);
	measurement.R = Matrix2d::Identity() * 0.25;
	measurement.obsKeys = {retained, candidateChord};
	const Matrix2d innovation = priorCovariance + measurement.R;
	const Matrix2d gain = priorCovariance * innovation.inverse();
	const Vector2d posteriorMean = priorMean + gain * measurement.V;
	Matrix2d posteriorCovariance = priorCovariance
		- gain * priorCovariance;
	posteriorCovariance = 0.5 *
		(posteriorCovariance + posteriorCovariance.transpose());

	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), source, priorMean, priorCovariance, measurement, "/PPP",
		posteriorMean, posteriorCovariance));
	BOOST_REQUIRE(capture.bindPersistentSnapshots({
		{"OLD_PRODUCT", "OLD_PRODUCT@A0", Vector2d(1, 0), 0},
		{"CANDIDATE_PRODUCT", "CANDIDATE_PRODUCT@A0", Vector2d(0, 1), 0}}));

	SparseMatrix<double> projection(1, 2);
	projection.insert(0, 0) = 1;
	BOOST_REQUIRE(capture.recordCoordinateTransform(
		GTime(), source, destination, projection,
		"candidate chord local reinitialisation", true));
	const auto marginal = capture.persistentSnapshotMarginal();
	BOOST_REQUIRE_MESSAGE(marginal.valid, marginal.failureReason);
	const auto replayed = capture.replayPersistentSnapshotsKeepingRows(
		[](const ZhangCapturedFactorEvent&, int) { return true; });
	BOOST_REQUIRE_MESSAGE(replayed.valid, replayed.failureReason);
	BOOST_CHECK(marginal.identities == replayed.identities);
	BOOST_CHECK_SMALL((marginal.mean - replayed.mean).norm(), 1e-12);
	BOOST_CHECK_SMALL(
		(marginal.covariance - replayed.covariance).norm(), 1e-12);
	BOOST_REQUIRE_EQUAL(marginal.identities.size(), 2);
	BOOST_CHECK_EQUAL(marginal.identities[0], "OLD_PRODUCT");
	BOOST_CHECK_EQUAL(marginal.identities[1], "CANDIDATE_PRODUCT");
	const double expectedDifference = posteriorMean(1) - posteriorMean(0);
	const double expectedVariance = posteriorCovariance(0, 0)
		+ posteriorCovariance(1, 1) - 2 * posteriorCovariance(0, 1);
	const double projectedDifference =
		marginal.mean(1) - marginal.mean(0);
	const double projectedVariance =
		marginal.covariance(1, 1) + marginal.covariance(0, 0)
		- 2 * marginal.covariance(0, 1);
	BOOST_CHECK_SMALL(
		projectedDifference - expectedDifference, 1e-12);
	BOOST_CHECK_SMALL(
		projectedVariance - expectedVariance, 1e-12);

	const long long expectedCandidate = std::llround(expectedDifference);
	const long long projectedCandidate = std::llround(projectedDifference);
	const double expectedFractional = expectedDifference - expectedCandidate;
	const double projectedFractional =
		projectedDifference - projectedCandidate;
	const double expectedPerr = testScalarRoundErrorProbability(
		expectedFractional, expectedVariance);
	const double projectedPerr = testScalarRoundErrorProbability(
		projectedFractional, projectedVariance);
	const double expectedNis =
		expectedFractional * expectedFractional / expectedVariance;
	const double projectedNis =
		projectedFractional * projectedFractional / projectedVariance;
	BOOST_CHECK_EQUAL(projectedCandidate, expectedCandidate);
	BOOST_CHECK_SMALL(projectedPerr - expectedPerr, 1e-10);
	BOOST_CHECK_SMALL(projectedNis - expectedNis, 1e-10);
	BOOST_CHECK_EQUAL(
		projectedPerr <= 1e-3 && projectedNis <= 23.9281,
		expectedPerr <= 1e-3 && expectedNis <= 23.9281);
}

BOOST_AUTO_TEST_CASE(temporal_zero_row_snapshot_is_a_valid_besd_endpoint)
{
	KFKey ambiguity;
	ambiguity.type = KF::AMBIGUITY;
	ambiguity.str = "R0";
	ambiguity.Sat = SatSys(E_Sys::GPS, 2);
	ambiguity.num = static_cast<int>(E_ObsCode::L1C);
	std::vector<ZhangCapturedStateKey> keys = {
		zhangCapturedStateKey(ambiguity)};

	VectorXd priorMean = VectorXd::Constant(1, 3.25);
	MatrixXd priorCovariance = MatrixXd::Constant(1, 1, 0.4);
	KFMeas measurement;
	measurement.H = MatrixXd::Identity(1, 1);
	measurement.V = VectorXd::Constant(1, 0.1);
	measurement.R = MatrixXd::Constant(1, 1, 0.2);
	measurement.obsKeys = {ambiguity};
	const double gain = 0.4 / 0.6;
	VectorXd posteriorMean = VectorXd::Constant(1, 3.25 + gain * 0.1);
	MatrixXd posteriorCovariance = MatrixXd::Constant(
		1, 1, 0.4 - gain * 0.4);

	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, priorMean, priorCovariance, measurement, "/PPP",
		posteriorMean, posteriorCovariance));
	BOOST_REQUIRE(capture.bindPersistentSnapshot(
		"ZERO", "ZERO@A0", VectorXd::Zero(1), 0));
	BOOST_REQUIRE(capture.bindPersistentSnapshot(
		"CURRENT", "CURRENT@A0", VectorXd::Ones(1), 0));

	const auto marginal = capture.persistentSnapshotMarginal();
	BOOST_REQUIRE_MESSAGE(marginal.valid, marginal.failureReason);
	BOOST_REQUIRE_EQUAL(marginal.identities.size(), 2);
	BOOST_CHECK_SMALL(marginal.mean(0), 1e-12);
	BOOST_CHECK_SMALL(marginal.covariance(0, 0), 1e-12);
	BOOST_CHECK_SMALL(
		(marginal.mean(1) - marginal.mean(0)) - posteriorMean(0), 1e-12);
	BOOST_CHECK_SMALL(
		(marginal.covariance(1, 1) + marginal.covariance(0, 0)
			- 2 * marginal.covariance(0, 1)) - posteriorCovariance(0, 0),
		1e-12);
}

BOOST_AUTO_TEST_CASE(
	persistent_snapshot_chronology_supports_correlated_measurement_row_ablation)
{
	KFKey ambiguity;
	ambiguity.type = KF::AMBIGUITY;
	ambiguity.str = "R0";
	ambiguity.Sat = SatSys(E_Sys::GPS, 4);
	ambiguity.num = static_cast<int>(E_ObsCode::L1C);
	std::vector<ZhangCapturedStateKey> keys = {
		zhangCapturedStateKey(ambiguity)};

	VectorXd priorMean = VectorXd::Zero(1);
	MatrixXd priorCovariance = MatrixXd::Ones(1, 1);
	KFMeas measurement;
	measurement.H = MatrixXd::Ones(2, 1);
	measurement.V = Vector2d(1.0, 0.0);
	measurement.R = Matrix2d::Zero();
	measurement.R(0, 0) = 1.0;
	measurement.R(1, 1) = 0.01;
	measurement.R(0, 1) = 0.05;
	measurement.R(1, 0) = 0.05;
	KFKey codeObservation = ambiguity;
	codeObservation.type = KF::CODE_MEAS;
	KFKey phaseObservation = ambiguity;
	phaseObservation.type = KF::PHAS_MEAS;
	measurement.obsKeys = {codeObservation, phaseObservation};
	const Matrix2d innovation = measurement.H * priorCovariance
		* measurement.H.transpose() + measurement.R;
	const MatrixXd gain = priorCovariance * measurement.H.transpose()
		* innovation.inverse();
	const VectorXd posteriorMean = priorMean + gain * measurement.V;
	MatrixXd posteriorCovariance = priorCovariance
		- gain * measurement.H * priorCovariance;
	posteriorCovariance = 0.5
		* (posteriorCovariance + posteriorCovariance.transpose());

	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, priorMean, priorCovariance, measurement, "/PPP",
		posteriorMean, posteriorCovariance));
	BOOST_REQUIRE(capture.bindPersistentSnapshots({
		{"ZERO", "ZERO@A0", VectorXd::Zero(1), 0},
		{"X", "X@A0", VectorXd::Ones(1), 0}}));

	const auto full = capture.persistentSnapshotMarginal();
	const auto replayedFull = capture.replayPersistentSnapshotsKeepingRows(
		[](const ZhangCapturedFactorEvent&, int) { return true; });
	BOOST_REQUIRE_MESSAGE(full.valid, full.failureReason);
	BOOST_REQUIRE_MESSAGE(replayedFull.valid, replayedFull.failureReason);
	BOOST_CHECK(full.identities == replayedFull.identities);
	BOOST_CHECK_SMALL((full.mean - replayedFull.mean).norm(), 1e-12);
	BOOST_CHECK_SMALL(
		(full.covariance - replayedFull.covariance).norm(), 1e-12);

	BOOST_REQUIRE(capture.retainPersistentSnapshots({"X"}));
	BOOST_REQUIRE_EQUAL(capture.capturedSnapshotOperations().size(), 2);
	const auto codeOnly = capture.replayPersistentSnapshotsKeepingRows(
		[](const ZhangCapturedFactorEvent& event, int row)
		{
			return event.observationKeys[row].type ==
				static_cast<int>(KF::CODE_MEAS);
		});
	BOOST_REQUIRE_MESSAGE(codeOnly.valid, codeOnly.failureReason);
	BOOST_REQUIRE_EQUAL(codeOnly.identities.size(), 1);
	BOOST_CHECK_EQUAL(codeOnly.identities.front(), "X");
	// N(0,1) updated by y=x+e, y=1, Var(e)=1.
	BOOST_CHECK_SMALL(codeOnly.mean(0) - 0.5, 1e-12);
	BOOST_CHECK_SMALL(codeOnly.covariance(0, 0) - 0.5, 1e-12);
	const auto phaseOnly = capture.replayPersistentSnapshotsKeepingRows(
		[](const ZhangCapturedFactorEvent& event, int row)
		{
			return event.observationKeys[row].type ==
				static_cast<int>(KF::PHAS_MEAS);
		});
	BOOST_REQUIRE_MESSAGE(phaseOnly.valid, phaseOnly.failureReason);
	BOOST_REQUIRE_EQUAL(phaseOnly.identities.size(), 1);
	BOOST_CHECK_SMALL(phaseOnly.mean(0), 1e-12);
	BOOST_CHECK_SMALL(
		phaseOnly.covariance(0, 0) - 1.0 / 101.0, 1e-12);
}

BOOST_AUTO_TEST_CASE(
	persistent_snapshot_replay_has_separate_zero_process_noise_control)
{
	KFKey state;
	state.type = KF::SAT_CLOCK;
	state.Sat = SatSys(E_Sys::GPS, 5);
	std::vector<ZhangCapturedStateKey> keys = {
		zhangCapturedStateKey(state)};
	VectorXd priorMean = VectorXd::Zero(1);
	MatrixXd priorCovariance = MatrixXd::Ones(1, 1);
	KFMeas anchorMeasurement;
	anchorMeasurement.H = MatrixXd::Zero(1, 1);
	anchorMeasurement.V = VectorXd::Zero(1);
	anchorMeasurement.R = MatrixXd::Ones(1, 1);
	anchorMeasurement.obsKeys = {state};

	ZhangFactorCaptureBuffer capture;
	BOOST_REQUIRE(capture.recordMeasurement(
		GTime(), keys, priorMean, priorCovariance, anchorMeasurement,
		"/PPP", priorMean, priorCovariance));
	SparseMatrix<double> transition(1, 1);
	transition.insert(0, 0) = 1;
	MatrixXd processCovariance = MatrixXd::Constant(1, 1, 4.0);
	BOOST_REQUIRE(capture.recordTransition(
		GTime(), keys, keys, transition, processCovariance, "random walk"));
	BOOST_REQUIRE(capture.bindPersistentSnapshot(
		"X", "X@A0", VectorXd::Ones(1), 0));

	const auto full = capture.replayPersistentSnapshotsKeepingRows(
		[](const ZhangCapturedFactorEvent&, int) { return true; });
	const auto zeroProcess = capture.replayPersistentSnapshotsKeepingRows(
		[](const ZhangCapturedFactorEvent&, int) { return true; }, 0);
	BOOST_REQUIRE_MESSAGE(full.valid, full.failureReason);
	BOOST_REQUIRE_MESSAGE(zeroProcess.valid, zeroProcess.failureReason);
	BOOST_CHECK_SMALL(full.covariance(0, 0) - 5.0, 1e-12);
	BOOST_CHECK_SMALL(zeroProcess.covariance(0, 0) - 1.0, 1e-12);
	auto invalid = capture.replayPersistentSnapshotsKeepingRows(
		[](const ZhangCapturedFactorEvent&, int) { return true; }, -1);
	BOOST_CHECK(!invalid.valid);
	BOOST_CHECK_EQUAL(invalid.failureReason, "INVALID_PROCESS_NOISE_SCALE");
}

BOOST_AUTO_TEST_CASE(
	e29_product_gauge_compiler_closes_ten_exact_three_by_three_by_two_s_bases)
{
	constexpr int receivers = 3;
	constexpr int satellites = 3;
	constexpr int frequencies = 2;
	constexpr int observations =
		2 * receivers * satellites * frequencies;
	constexpr int tauOffset = 0;
	constexpr int receiverClockOffset = tauOffset + receivers;
	constexpr int satelliteClockOffset = receiverClockOffset + receivers;
	constexpr int ionosphereOffset = satelliteClockOffset + satellites;
	constexpr int receiverCodeOffset =
		ionosphereOffset + receivers * satellites;
	constexpr int satelliteCodeOffset =
		receiverCodeOffset + receivers * frequencies;
	constexpr int receiverPhaseOffset =
		satelliteCodeOffset + satellites * frequencies;
	constexpr int satellitePhaseOffset =
		receiverPhaseOffset + receivers * frequencies;
	constexpr int ambiguityOffset =
		satellitePhaseOffset + satellites * frequencies;
	constexpr int parameters =
		ambiguityOffset + receivers * satellites * frequencies;
	ZhangExactMatrix raw = zhangExactZeroMatrix(observations, parameters);
	const int mu[frequencies] = {1, 2};
	const int wavelength[frequencies] = {1, 2};
	auto ionosphere = [](int receiver, int satellite)
	{
		return receiver * satellites + satellite;
	};
	auto receiverSignal = [](int receiver, int frequency)
	{
		return receiver * frequencies + frequency;
	};
	auto satelliteSignal = [](int satellite, int frequency)
	{
		return satellite * frequencies + frequency;
	};
	auto ambiguity = [](int receiver, int satellite, int frequency)
	{
		return (receiver * satellites + satellite) * frequencies + frequency;
	};
	int row = 0;
	for (int receiver = 0; receiver < receivers; receiver++)
	for (int satellite = 0; satellite < satellites; satellite++)
	for (int frequency = 0; frequency < frequencies; frequency++)
	{
		auto common = [&](ZhangExactVector& design)
		{
			design[tauOffset + receiver] = 1;
			design[receiverClockOffset + receiver] = 1;
			design[satelliteClockOffset + satellite] = -1;
		};
		auto& code = raw[row++];
		common(code);
		code[ionosphereOffset + ionosphere(receiver, satellite)] =
			mu[frequency];
		code[receiverCodeOffset + receiverSignal(receiver, frequency)] = 1;
		code[satelliteCodeOffset + satelliteSignal(satellite, frequency)] = -1;
		auto& phase = raw[row++];
		common(phase);
		phase[ionosphereOffset + ionosphere(receiver, satellite)] =
			-mu[frequency];
		phase[receiverPhaseOffset + receiverSignal(receiver, frequency)] =
			wavelength[frequency];
		phase[satellitePhaseOffset + satelliteSignal(satellite, frequency)] =
			-wavelength[frequency];
		phase[ambiguityOffset + ambiguity(receiver, satellite, frequency)] =
			wavelength[frequency];
	}
	BOOST_REQUIRE_EQUAL(row, observations);

	MatrixXd rawDouble(observations, parameters);
	for (int r = 0; r < observations; r++)
	for (int c = 0; c < parameters; c++)
	{
		rawDouble(r, c) = raw[r][c].convert_to<double>();
	}
	Eigen::FullPivLU<MatrixXd> rawLu(rawDouble);
	rawLu.setThreshold(1e-12);
	const int estimableRank = rawLu.rank();
	BOOST_REQUIRE_GT(estimableRank, 0);

	auto chooseBasis = [&](std::vector<int> order)
	{
		std::vector<int> selected;
		int rank = 0;
		for (int column : order)
		{
			MatrixXd candidate(observations, selected.size() + 1);
			for (int existing = 0;
				 existing < static_cast<int>(selected.size()); existing++)
			{
				candidate.col(existing) = rawDouble.col(selected[existing]);
			}
			candidate.col(selected.size()) = rawDouble.col(column);
			Eigen::FullPivLU<MatrixXd> lu(candidate);
			lu.setThreshold(1e-12);
			if (lu.rank() > rank)
			{
				selected.push_back(column);
				rank++;
				if (rank == estimableRank)
				{
					break;
				}
			}
		}
		return selected;
	};
	auto selectExactColumns = [&](const std::vector<int>& columns)
	{
		ZhangExactMatrix design = zhangExactZeroMatrix(
			observations, columns.size());
		for (int r = 0; r < observations; r++)
		for (int c = 0; c < static_cast<int>(columns.size()); c++)
		{
			design[r][c] = raw[r][columns[c]];
		}
		return design;
	};
	auto exactToDouble = [](const ZhangExactMatrix& design)
	{
		MatrixXd result(design.size(), design.front().size());
		for (int r = 0; r < result.rows(); r++)
		for (int c = 0; c < result.cols(); c++)
		{
			result(r, c) = design[r][c].convert_to<double>();
		}
		return result;
	};

	std::vector<int> natural(parameters);
	std::iota(natural.begin(), natural.end(), 0);
	const auto frontendColumns = chooseBasis(natural);
	BOOST_REQUIRE_EQUAL(frontendColumns.size(), estimableRank);
	const auto frontendExact = selectExactColumns(frontendColumns);
	const MatrixXd frontendDense = exactToDouble(frontendExact);
	std::set<std::vector<int>> distinctBases;
	std::mt19937 generator(29001);
	for (int trial = 0; trial < 200 && distinctBases.size() < 10; trial++)
	{
		auto order = natural;
		std::shuffle(order.begin(), order.end(), generator);
		auto columns = chooseBasis(order);
		if (columns.size() == static_cast<std::size_t>(estimableRank))
		{
			distinctBases.insert(std::move(columns));
		}
	}
	BOOST_REQUIRE_GE(distinctBases.size(), 10);

	std::normal_distribution<double> normal(0, 1);
	int audited = 0;
	for (const auto& backendColumns : distinctBases)
	{
		if (audited++ == 10)
		{
			break;
		}
		const auto backendExact = selectExactColumns(backendColumns);
		const auto exact = zhangCompileExactProductGaugeTransform(
			frontendExact, backendExact);
		BOOST_REQUIRE_MESSAGE(exact.valid, exact.failureReason);
		BOOST_REQUIRE_EQUAL(exact.rank, estimableRank);
		for (int r = 0; r < observations; r++)
		for (int c = 0; c < estimableRank; c++)
		{
			ZhangExactRational predicted = 0;
			for (int k = 0; k < estimableRank; k++)
			{
				predicted += ZhangExactRational(frontendExact[r][k])
					* exact.transform[k][c];
			}
			BOOST_CHECK(predicted == ZhangExactRational(backendExact[r][c]));
		}

		const MatrixXd backendDense = exactToDouble(backendExact);
		const auto compiled = zhangCompileProductGaugeTransform(
			frontendDense.sparseView(0, 0),
			backendDense.sparseView(0, 0), 1e-12);
		BOOST_REQUIRE_MESSAGE(compiled.valid, compiled.failureReason);
		BOOST_CHECK_SMALL(compiled.maximumClosureError, 1e-12);
		VectorXd backendState(estimableRank);
		MatrixXd squareRoot(estimableRank, estimableRank);
		for (int r = 0; r < estimableRank; r++)
		{
			backendState(r) = normal(generator);
			for (int c = 0; c < estimableRank; c++)
			{
				squareRoot(r, c) = normal(generator);
			}
		}
		const MatrixXd backendCovariance = squareRoot * squareRoot.transpose()
			+ 0.1 * MatrixXd::Identity(estimableRank, estimableRank);
		const VectorXd frontendState = compiled.transform * backendState;
		const MatrixXd frontendCovariance =
			zhangProjectProductGaugeCovariance(
				backendCovariance, compiled.transform);
		BOOST_REQUIRE_EQUAL(frontendCovariance.rows(), estimableRank);
		BOOST_CHECK_SMALL(
			(backendDense * backendState
				- frontendDense * frontendState).cwiseAbs().maxCoeff(),
			1e-10);
		const MatrixXd backendPredictionCovariance =
			backendDense * backendCovariance * backendDense.transpose();
		const MatrixXd frontendPredictionCovariance =
			frontendDense * frontendCovariance * frontendDense.transpose();
		BOOST_CHECK_SMALL(
			(backendPredictionCovariance - frontendPredictionCovariance)
				.cwiseAbs().maxCoeff(),
			1e-10);
	}
	BOOST_CHECK_EQUAL(audited, 10);
}

BOOST_AUTO_TEST_CASE(
	e29_integer_conditioner_matches_near_zero_noise_fixed_resolve)
{
	constexpr int dimension = 12;
	constexpr int constraintsCount = 4;
	std::mt19937 generator(29002);
	std::normal_distribution<double> normal(0, 1);
	VectorXd mean(dimension);
	MatrixXd squareRoot(dimension, dimension);
	for (int row = 0; row < dimension; row++)
	{
		mean(row) = normal(generator);
		for (int column = 0; column < dimension; column++)
		{
			squareRoot(row, column) = normal(generator);
		}
	}
	const MatrixXd covariance = squareRoot * squareRoot.transpose()
		+ 0.5 * MatrixXd::Identity(dimension, dimension);
	std::vector<Eigen::Triplet<double>> triplets = {
		{0, 0, 1}, {0, 1, -1},
		{1, 2, 1}, {1, 3, 1}, {1, 4, -1},
		{2, 5, 1},
		{3, 6, 1}, {3, 7, -1}, {3, 8, 1}};
	ZhangIarFunctional constraints(constraintsCount, dimension);
	constraints.setFromTriplets(triplets.begin(), triplets.end());
	constraints.makeCompressed();
	VectorXd integers = constraints * mean;
	for (int row = 0; row < integers.size(); row++)
	{
		integers(row) = std::round(integers(row));
	}

	const auto exact = zhangConditionIntegersExact(
		mean, covariance, constraints, integers);
	const auto squareRootConditioned =
		zhangConditionIntegersSquareRootOrthogonal(
		mean, covariance, constraints, integers);
	const auto pseudo = zhangConditionIntegersPseudoObservation(
		mean, covariance, constraints, integers, 1e-8);
	BOOST_REQUIRE_MESSAGE(exact.valid, exact.failureReason);
	BOOST_REQUIRE_MESSAGE(
		squareRootConditioned.valid,
		squareRootConditioned.failureReason);
	BOOST_REQUIRE_MESSAGE(pseudo.valid, pseudo.failureReason);
	BOOST_CHECK_EQUAL(exact.constraintRank, constraintsCount);
	BOOST_CHECK_EQUAL(
		squareRootConditioned.constraintRank,
		constraintsCount);
	BOOST_CHECK_EQUAL(pseudo.constraintRank, constraintsCount);
	BOOST_CHECK_SMALL(exact.maximumConstraintResidual, 1e-10);
	BOOST_CHECK_SMALL(
		squareRootConditioned.maximumConstraintResidual,
		1e-10);
	BOOST_CHECK_SMALL(
		(exact.mean - squareRootConditioned.mean)
			.cwiseAbs().maxCoeff(),
		1e-9);
	BOOST_CHECK_SMALL(
		(exact.covariance - squareRootConditioned.covariance)
			.cwiseAbs().maxCoeff(),
		1e-9);
	BOOST_CHECK_SMALL(
		(exact.mean - pseudo.mean).cwiseAbs().maxCoeff(), 1e-9);
	BOOST_CHECK_SMALL(
		(exact.covariance - pseudo.covariance).cwiseAbs().maxCoeff(), 1e-9);

	std::vector<Eigen::Triplet<double>> redundantTriplets = triplets;
	redundantTriplets.emplace_back(4, 0, 2);
	redundantTriplets.emplace_back(4, 1, -2);
	ZhangIarFunctional redundant(constraintsCount + 1, dimension);
	redundant.setFromTriplets(
		redundantTriplets.begin(), redundantTriplets.end());
	redundant.makeCompressed();
	VectorXd redundantIntegers(constraintsCount + 1);
	redundantIntegers.head(constraintsCount) = integers;
	redundantIntegers(4) = 2 * integers(0);
	const auto rejected = zhangConditionIntegersExact(
		mean, covariance, redundant, redundantIntegers);
	BOOST_CHECK(!rejected.valid);
	BOOST_CHECK_EQUAL(
		rejected.failureReason,
		"INTEGER_CONSTRAINT_NOT_FULL_ROW_RANK");
}

BOOST_AUTO_TEST_CASE(
	accepted_measurement_families_use_obs_key_and_actual_state_support)
{
	KFKey clock;
	clock.type = KF::SAT_CLOCK;
	clock.Sat = SatSys(E_Sys::GPS, 4);
	KFKey ionosphere = clock;
	ionosphere.type = KF::IONO_STEC;
	KFKey phaseDatum = clock;
	phaseDatum.type = KF::PHASE_BIAS;
	KFKey position = clock;
	position.type = KF::REC_POS;

	ZhangCapturedFactorEvent event;
	event.kind = ZhangCapturedFactorKind::MEASUREMENT;
	event.destinationKeys = {
		zhangCapturedStateKey(clock),
		zhangCapturedStateKey(ionosphere),
		zhangCapturedStateKey(phaseDatum),
		zhangCapturedStateKey(position)};
	event.design.resize(6, 4);
	event.design.insert(0, 3) = 1;
	event.design.insert(1, 3) = 1;
	event.design.insert(2, 0) = 1;
	event.design.insert(3, 1) = 1;
	event.design.insert(4, 2) = 1;
	event.design.insert(5, 0) = 1;
	event.design.insert(5, 1) = -1;
	KFKey phaseObservation = phaseDatum;
	phaseObservation.type = KF::PHAS_MEAS;
	KFKey codeObservation = phaseDatum;
	codeObservation.type = KF::CODE_MEAS;
	KFKey pseudo = phaseDatum;
	pseudo.type = KF::PSEUDO_MEAS;
	event.observationKeys = {
		zhangCapturedStateKey(phaseObservation),
		zhangCapturedStateKey(codeObservation),
		zhangCapturedStateKey(pseudo),
		zhangCapturedStateKey(pseudo),
		zhangCapturedStateKey(pseudo),
		zhangCapturedStateKey(pseudo)};

	BOOST_CHECK(
		zhangCapturedMeasurementFamily(event, 0) ==
		ZhangCapturedMeasurementFamily::PHASE_OBSERVATION);
	BOOST_CHECK(
		zhangCapturedMeasurementFamily(event, 1) ==
		ZhangCapturedMeasurementFamily::CODE_OBSERVATION);
	BOOST_CHECK(
		zhangCapturedMeasurementFamily(event, 2) ==
		ZhangCapturedMeasurementFamily::CLOCK_FACTOR);
	BOOST_CHECK(
		zhangCapturedMeasurementFamily(event, 3) ==
		ZhangCapturedMeasurementFamily::IONOSPHERE_FACTOR);
	BOOST_CHECK(
		zhangCapturedMeasurementFamily(event, 4) ==
		ZhangCapturedMeasurementFamily::PHASE_DATUM_FACTOR);
	BOOST_CHECK(
		zhangCapturedMeasurementFamily(event, 5) ==
		ZhangCapturedMeasurementFamily::MIXED_PSEUDO_FACTOR);
}

BOOST_AUTO_TEST_CASE(
	persistent_snapshot_is_not_rebound_after_unrepresentable_rectangular_reset)
{
	Vector2d priorMean(0.4, -1.2);
	Matrix2d priorCovariance;
	priorCovariance << 0.8, 0.25, 0.25, 1.1;
	ZhangPersistentRawTargetWindow window;
	BOOST_REQUIRE(window.initialise(priorMean, priorCovariance));
	Vector2d oldPhysicalRow(0, 1);
	BOOST_REQUIRE(window.bindTarget(
		"OLD", "OLD:R0/G01@0", oldPhysicalRow, 0, 1));
	const auto beforeReset = window.targetMarginal();
	BOOST_REQUIRE_MESSAGE(beforeReset.valid, beforeReset.failureReason);
	const long long beforeCandidate = std::llround(beforeReset.mean(0));
	const double beforeFractional = beforeReset.mean(0) - beforeCandidate;
	const double beforePerr = testScalarRoundErrorProbability(
		beforeFractional, beforeReset.covariance(0, 0));
	const double beforeNis = beforeFractional * beforeFractional /
		beforeReset.covariance(0, 0);

	// The current-state projection removes x1, but the explicit immutable
	// target variable is carried with identity.  Its marginal must survive.
	MatrixXd projection(1, 2);
	projection << 1, 0;
	BOOST_REQUIRE(window.applyExactCoordinateTransform(projection));
	const auto afterReset = window.targetMarginal();
	BOOST_REQUIRE_MESSAGE(afterReset.valid, afterReset.failureReason);
	BOOST_CHECK_SMALL(afterReset.mean(0) - beforeReset.mean(0), 1e-12);
	BOOST_CHECK_SMALL(
		afterReset.covariance(0, 0) - beforeReset.covariance(0, 0), 1e-12);
	const long long afterCandidate = std::llround(afterReset.mean(0));
	const double afterFractional = afterReset.mean(0) - afterCandidate;
	const double afterPerr = testScalarRoundErrorProbability(
		afterFractional, afterReset.covariance(0, 0));
	const double afterNis = afterFractional * afterFractional /
		afterReset.covariance(0, 0);
	BOOST_CHECK_EQUAL(afterCandidate, beforeCandidate);
	BOOST_CHECK_SMALL(afterPerr - beforePerr, 1e-12);
	BOOST_CHECK_SMALL(afterNis - beforeNis, 1e-12);
	BOOST_CHECK_EQUAL(
		beforePerr <= 1e-3 && beforeNis <= 23.9281,
		afterPerr <= 1e-3 && afterNis <= 23.9281);

	// A newly initialised current row is not an exact transport proof for the
	// removed covector.  Reusing the old snapshot identity must therefore not
	// inject a zero-noise constraint.
	VectorXd newlyInitialisedRow = VectorXd::Ones(1);
	BOOST_REQUIRE(window.bindTarget(
		"OLD", "OLD:R0/G01@0", newlyInitialisedRow, 0, 2));
	const auto afterFirstRebind = window.targetMarginal();
	BOOST_REQUIRE_MESSAGE(
		afterFirstRebind.valid, afterFirstRebind.failureReason);
	BOOST_CHECK_SMALL(
		afterFirstRebind.mean(0) - afterReset.mean(0), 1e-12);
	BOOST_CHECK_SMALL(
		afterFirstRebind.covariance(0, 0)
			- afterReset.covariance(0, 0), 1e-12);

	MatrixXd measurementDesign = MatrixXd::Ones(1, 1);
	MatrixXd measurementCovariance = MatrixXd::Constant(1, 1, 0.2);
	VectorXd observation = VectorXd::Constant(1, 0.1);
	BOOST_REQUIRE(window.addAcceptedMeasurement(
		measurementDesign, measurementCovariance, observation));
	const auto beforeSecondRebind = window.targetMarginal();
	BOOST_REQUIRE_MESSAGE(
		beforeSecondRebind.valid, beforeSecondRebind.failureReason);
	BOOST_REQUIRE(window.bindTarget(
		"OLD", "OLD:R0/G01@0", newlyInitialisedRow, 0, 3));
	const auto afterSecondRebind = window.targetMarginal();
	BOOST_REQUIRE_MESSAGE(
		afterSecondRebind.valid, afterSecondRebind.failureReason);
	BOOST_CHECK_SMALL(
		afterSecondRebind.mean(0) - beforeSecondRebind.mean(0), 1e-12);
	BOOST_CHECK_SMALL(
		afterSecondRebind.covariance(0, 0)
			- beforeSecondRebind.covariance(0, 0), 1e-12);
	BOOST_CHECK_EQUAL(window.summary().exactConstraintsApplied, 0);
}

BOOST_AUTO_TEST_CASE(
	persistent_snapshot_batch_augmentation_matches_sequential_bindings)
{
	Vector3d priorMean(0.4, -0.7, 1.2);
	Matrix3d priorCovariance;
	priorCovariance <<
		0.8, 0.1, -0.05,
		0.1, 1.2, 0.2,
		-0.05, 0.2, 0.6;
	MatrixXd design(2, 3);
	design << 1, 0.2, -0.1, -0.3, 1, 0.4;
	Matrix2d measurementCovariance = 0.1 * Matrix2d::Identity();
	Vector2d observation(0.1, -0.2);

	ZhangPersistentRawTargetWindow sequential;
	ZhangPersistentRawTargetWindow batch;
	BOOST_REQUIRE(sequential.initialise(priorMean, priorCovariance));
	BOOST_REQUIRE(batch.initialise(priorMean, priorCovariance));
	BOOST_REQUIRE(sequential.addAcceptedMeasurement(
		design, measurementCovariance, observation));
	BOOST_REQUIRE(batch.addAcceptedMeasurement(
		design, measurementCovariance, observation));

	Vector3d firstRow(1, -1, 0.5);
	Vector3d secondRow(-0.2, 0.4, 1);
	BOOST_REQUIRE(sequential.bindTarget(
		"FIRST", "FIRST@0", firstRow, 2.0, 1));
	BOOST_REQUIRE(sequential.bindTarget(
		"SECOND", "SECOND@0", secondRow, -0.3, 1));
	MatrixXd rows(2, 3);
	rows.row(0) = firstRow.transpose();
	rows.row(1) = secondRow.transpose();
	Vector2d offsets(2.0, -0.3);
	BOOST_REQUIRE(batch.bindNewTargets(
		{"FIRST", "SECOND"}, {"FIRST@0", "SECOND@0"},
		rows, offsets, 1));

	const auto sequentialMarginal = sequential.targetMarginal();
	const auto batchMarginal = batch.targetMarginal();
	BOOST_REQUIRE_MESSAGE(
		sequentialMarginal.valid, sequentialMarginal.failureReason);
	BOOST_REQUIRE_MESSAGE(batchMarginal.valid, batchMarginal.failureReason);
	BOOST_CHECK_EQUAL_COLLECTIONS(
		sequentialMarginal.identities.begin(),
		sequentialMarginal.identities.end(),
		batchMarginal.identities.begin(), batchMarginal.identities.end());
	BOOST_CHECK_SMALL(
		(sequentialMarginal.mean - batchMarginal.mean).norm(), 1e-11);
	BOOST_CHECK_SMALL(
		(sequentialMarginal.covariance - batchMarginal.covariance).norm(),
		1e-11);

	Matrix3d transition = Matrix3d::Identity();
	transition(0, 1) = 0.1;
	Matrix3d processCovariance = 0.02 * Matrix3d::Identity();
	BOOST_REQUIRE(sequential.advance(transition, processCovariance));
	BOOST_REQUIRE(batch.advance(transition, processCovariance));
	BOOST_REQUIRE(sequential.addAcceptedMeasurement(
		design, measurementCovariance, observation));
	BOOST_REQUIRE(batch.addAcceptedMeasurement(
		design, measurementCovariance, observation));
	const auto sequentialFuture = sequential.targetMarginal();
	const auto batchFuture = batch.targetMarginal();
	BOOST_REQUIRE_MESSAGE(sequentialFuture.valid, sequentialFuture.failureReason);
	BOOST_REQUIRE_MESSAGE(batchFuture.valid, batchFuture.failureReason);
	BOOST_CHECK_SMALL((sequentialFuture.mean - batchFuture.mean).norm(), 1e-10);
	BOOST_CHECK_SMALL(
		(sequentialFuture.covariance - batchFuture.covariance).norm(), 1e-10);
}

BOOST_AUTO_TEST_CASE(
	persistent_snapshot_lifecycle_marginalises_only_released_targets)
{
	Vector2d priorMean(0.3, -0.4);
	Matrix2d priorCovariance;
	priorCovariance << 0.8, 0.2, 0.2, 1.1;
	ZhangPersistentRawTargetWindow window;
	BOOST_REQUIRE(window.initialise(priorMean, priorCovariance));
	MatrixXd rows(3, 2);
	rows << 1, 0, 0, 1, 1, -1;
	BOOST_REQUIRE(window.bindNewTargets(
		{"A", "B", "C"}, {"A@0", "B@0", "C@0"}, rows,
		Vector3d(0.1, -0.2, 0.3), 1));
	const auto before = window.targetMarginal();
	BOOST_REQUIRE_MESSAGE(before.valid, before.failureReason);
	BOOST_REQUIRE(window.retainTargets({"A", "C"}));
	const auto after = window.targetMarginal();
	BOOST_REQUIRE_MESSAGE(after.valid, after.failureReason);
	BOOST_REQUIRE_EQUAL(after.identities.size(), 2);
	BOOST_CHECK_EQUAL(after.identities[0], "A");
	BOOST_CHECK_EQUAL(after.identities[1], "C");
	Vector2d expectedMean(before.mean(0), before.mean(2));
	Matrix2d expectedCovariance;
	expectedCovariance <<
		before.covariance(0, 0), before.covariance(0, 2),
		before.covariance(2, 0), before.covariance(2, 2);
	BOOST_CHECK_SMALL((after.mean - expectedMean).norm(), 1e-11);
	BOOST_CHECK_SMALL(
		(after.covariance - expectedCovariance).norm(), 1e-11);
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

BOOST_AUTO_TEST_CASE(six_legal_trees_and_receiver_roots_replay_identical_raw_factors)
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
	// Explicit receiver-root changes.  Preferred edges make the physical tree
	// sets distinct, so this is not merely the same tree traversed from another
	// root.
	trees.push_back(zhangBuildSpanningTree(
		edges, "R1", {{"R1", g01}, {"R1", g02}, {"R1", g03},
			{"R0", g01}, {"R2", g02}}));
	trees.push_back(zhangBuildSpanningTree(
		edges, "R2", {{"R2", g01}, {"R2", g02}, {"R2", g03},
			{"R0", g03}, {"R1", g02}}));
	std::mt19937 generator(20260806);
	std::uniform_real_distribution<double> quality(0.0, 1.0);
	for (int attempt = 0; attempt < 100 && trees.size() < 6; attempt++)
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
	BOOST_REQUIRE_EQUAL(trees.size(), 6);
	for (const auto& tree : trees)
	{
		BOOST_REQUIRE(tree.connected);
	}
	std::set<std::set<ZhangGraphEdge>> distinctTrees;
	for (const auto& tree : trees)
	{
		distinctTrees.insert(tree.treeEdges);
	}
	BOOST_REQUIRE_EQUAL(distinctTrees.size(), 6);

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
	std::vector<double> roundErrorProbabilities;
	std::vector<double> integerNis;
	std::vector<long long> integerCandidates;
	std::vector<bool> reliableStates;
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
		const double fractional = marginal.mean(0)
			- std::round(marginal.mean(0));
		integerCandidates.push_back(std::llround(marginal.mean(0)));
		roundErrorProbabilities.push_back(
			testScalarRoundErrorProbability(
				fractional, marginal.covariance(0, 0)));
		integerNis.push_back(
			fractional * fractional / marginal.covariance(0, 0));
		reliableStates.push_back(
			roundErrorProbabilities.back() <= 1e-3
			&& integerNis.back() <= 23.9281);
	}
	double maximumMeanDifference = 0;
	double maximumVarianceDifference = 0;
	double maximumPerrDifference = 0;
	double maximumNisDifference = 0;
	for (int strategy = 1; strategy < static_cast<int>(trees.size()); strategy++)
	{
		maximumMeanDifference = std::max(
			maximumMeanDifference, std::abs(means[strategy] - means[0]));
		maximumVarianceDifference = std::max(
			maximumVarianceDifference,
			std::abs(variances[strategy] - variances[0]));
		maximumPerrDifference = std::max(
			maximumPerrDifference,
			std::abs(
				roundErrorProbabilities[strategy]
				- roundErrorProbabilities[0]));
		maximumNisDifference = std::max(
			maximumNisDifference,
			std::abs(integerNis[strategy] - integerNis[0]));
		BOOST_CHECK_SMALL(means[strategy] - means[0], 1e-10);
		BOOST_CHECK_SMALL(variances[strategy] - variances[0], 1e-10);
		BOOST_CHECK_SMALL(
			roundErrorProbabilities[strategy]
			- roundErrorProbabilities[0], 1e-10);
		BOOST_CHECK_SMALL(
			integerNis[strategy] - integerNis[0], 1e-10);
		BOOST_CHECK_EQUAL(integerCandidates[strategy], integerCandidates[0]);
		BOOST_CHECK_EQUAL(reliableStates[strategy], reliableStates[0]);
	}
	BOOST_TEST_MESSAGE(
		"six-tree/root same-factor maximum_mean_difference="
		<< maximumMeanDifference
		<< " maximum_variance_difference="
		<< maximumVarianceDifference
		<< " maximum_perr_difference="
		<< maximumPerrDifference
		<< " maximum_nis_difference="
		<< maximumNisDifference);
}

BOOST_AUTO_TEST_CASE(
	satellite_reference_change_preserves_raw_integer_mean_variance_perr_and_nis)
{
	// Physical satellite potentials are only observed through differences.
	// Ref-G01 coordinates are [s2-s1,s3-s1], while Ref-G02 coordinates are
	// [s1-s2,s3-s2].  Both describe the same target s3-s1.
	Vector3d physicalMean(0.35, -1.2, 2.45);
	Matrix3d physicalCovariance;
	physicalCovariance <<
		0.9, 0.12, -0.04,
		0.12, 0.7, 0.08,
		-0.04, 0.08, 1.1;
	MatrixXd refG01(2, 3);
	refG01 << -1, 1, 0, -1, 0, 1;
	MatrixXd refG02(2, 3);
	refG02 << 1, -1, 0, 0, -1, 1;
	MatrixXd designG01(3, 2);
	designG01 << 1, 0, 0, 1, -1, 1;
	MatrixXd designG02(3, 2);
	designG02 << -1, 0, -1, 1, 0, 1;
	MatrixXd physicalDifferenceDesign(3, 3);
	physicalDifferenceDesign << -1, 1, 0, -1, 0, 1, 0, -1, 1;
	Vector3d observation = physicalDifferenceDesign * physicalMean;
	observation += Vector3d(0.006, -0.004, 0.002);
	Matrix3d observationCovariance = 0.04 * Matrix3d::Identity();

	std::vector<double> means;
	std::vector<double> variances;
	std::vector<double> perrs;
	std::vector<double> nises;
	std::vector<long long> candidates;
	for (int reference = 0; reference < 2; reference++)
	{
		const MatrixXd& coordinate = reference == 0 ? refG01 : refG02;
		const MatrixXd& design = reference == 0 ? designG01 : designG02;
		VectorXd target(2);
		target = reference == 0 ? Vector2d(0, 1) : Vector2d(-1, 1);
		ZhangRawFactorWindow replay;
		BOOST_REQUIRE(replay.initialise(
			coordinate * physicalMean,
			coordinate * physicalCovariance * coordinate.transpose()));
		BOOST_REQUIRE(replay.addAcceptedMeasurement(
			design, observationCovariance, observation));
		const auto marginal = replay.marginaliseToIntegerDatum(
			target.transpose(), VectorXd::Zero(1));
		BOOST_REQUIRE_MESSAGE(marginal.valid, marginal.failureReason);
		const long long candidate = std::llround(marginal.mean(0));
		const double fractional = marginal.mean(0) - candidate;
		means.push_back(marginal.mean(0));
		variances.push_back(marginal.covariance(0, 0));
		candidates.push_back(candidate);
		perrs.push_back(testScalarRoundErrorProbability(
			fractional, marginal.covariance(0, 0)));
		nises.push_back(fractional * fractional / marginal.covariance(0, 0));
	}
	BOOST_CHECK_SMALL(means[1] - means[0], 1e-10);
	BOOST_CHECK_SMALL(variances[1] - variances[0], 1e-10);
	BOOST_CHECK_EQUAL(candidates[1], candidates[0]);
	BOOST_CHECK_SMALL(perrs[1] - perrs[0], 1e-10);
	BOOST_CHECK_SMALL(nises[1] - nises[0], 1e-10);
	BOOST_CHECK_EQUAL(
		perrs[0] <= 1e-3 && nises[0] <= 23.9281,
		perrs[1] <= 1e-3 && nises[1] <= 23.9281);
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

BOOST_AUTO_TEST_CASE(lambda_beam_single_row_leverage_matches_direct_schur_deletion)
{
	Matrix3d covariance;
	covariance <<
		0.08, 0.02, -0.01,
		0.02, 0.11,  0.03,
		-0.01, 0.03, 0.09;
	Vector3d innovation(0.22, -0.31, 0.17);
	const auto leverage = zhangConstraintNisLeverage(
		innovation, covariance);
	BOOST_REQUIRE(leverage.valid);

	for (int removed = 0; removed < 3; removed++)
	{
		vector<int> retained;
		for (int row = 0; row < 3; row++)
		{
			if (row != removed)
			{
				retained.push_back(row);
			}
		}
		VectorXd reducedInnovation = innovation(retained);
		MatrixXd reducedCovariance = covariance(retained, retained);
		const double reducedNis = reducedInnovation.dot(
			reducedCovariance.ldlt().solve(reducedInnovation));
		BOOST_CHECK_SMALL(
			leverage.nis - reducedNis
				- leverage.deletionReduction(removed),
			1e-12);
	}
}

BOOST_AUTO_TEST_CASE(lambda_beam_product_gain_and_hnf_are_basis_invariant)
{
	Matrix3d ambiguityCovariance;
	ambiguityCovariance <<
		0.12, 0.03, 0.01,
		0.03, 0.15, 0.02,
		0.01, 0.02, 0.09;
	Matrix<double, 2, 3> productCross;
	productCross <<
		0.04, -0.02, 0.01,
		0.01,  0.03, 0.02;
	Matrix<double, 2, 3> rows;
	rows << 1, 0, -1,
		0, 1, 1;
	Matrix2d unimodular;
	unimodular << 1, 2, 0, 1;
	Matrix<double, 2, 3> changedBasis = unimodular * rows;

	const double gain = zhangConstraintProductInformationGain(
		productCross, 0.8, ambiguityCovariance, rows);
	const double changedGain = zhangConstraintProductInformationGain(
		productCross, 0.8, ambiguityCovariance, changedBasis);
	BOOST_REQUIRE(std::isfinite(gain));
	BOOST_CHECK_SMALL(gain - changedGain, 1e-12);
	BOOST_CHECK_EQUAL(
		zhangIntegerRowHnfFingerprint(rows),
		zhangIntegerRowHnfFingerprint(changedBasis));
	BOOST_CHECK_EQUAL(
		zhangIntegerRowHnfCanonicalKey(rows),
		zhangIntegerRowHnfCanonicalKey(changedBasis));

	Matrix<double, 1, 3> reduced = rows.topRows(1);
	const double reducedGain = zhangConstraintProductInformationGain(
		productCross, 0.8, ambiguityCovariance, reduced);
	BOOST_CHECK_GE(gain + 1e-12, reducedGain);
	BOOST_CHECK_NE(
		zhangIntegerRowHnfFingerprint(rows),
		zhangIntegerRowHnfFingerprint(reduced));
	BOOST_CHECK_NE(
		zhangIntegerRowHnfCanonicalKey(rows),
		zhangIntegerRowHnfCanonicalKey(reduced));

	Vector2d rhs(7, -3);
	Vector2d changedRhs = unimodular * rhs;
	BOOST_CHECK_EQUAL(
		zhangIntegerAffineHnfCanonicalKey(rows, rhs),
		zhangIntegerAffineHnfCanonicalKey(changedBasis, changedRhs));
	BOOST_CHECK_EQUAL(
		zhangIntegerAffineHnfFingerprint(rows, rhs),
		zhangIntegerAffineHnfFingerprint(changedBasis, changedRhs));
	Vector2d inconsistentRhs = changedRhs;
	inconsistentRhs(0) += 1;
	BOOST_CHECK_NE(
		zhangIntegerAffineHnfCanonicalKey(rows, rhs),
		zhangIntegerAffineHnfCanonicalKey(changedBasis, inconsistentRhs));
}

BOOST_AUTO_TEST_CASE(iar_product_gain_spectrum_matches_known_real_mode_ceiling)
{
	const Matrix3d ambiguityCovariance = Matrix3d::Identity();
	Matrix3d ambiguityProductCross = Matrix3d::Zero();
	ambiguityProductCross.diagonal() << 3, 2, 1;
	const auto spectrum = zhangIarProductGainSpectrum(
		ambiguityCovariance,
		ambiguityProductCross,
		Matrix3d::Identity());
	BOOST_REQUIRE_MESSAGE(spectrum.valid, spectrum.failureReason);
	BOOST_REQUIRE_EQUAL(spectrum.ambiguityRank, 3);
	BOOST_REQUIRE_EQUAL(spectrum.eigenvaluesDescending.size(), 3);
	BOOST_CHECK_SMALL(spectrum.eigenvaluesDescending(0) - 9, 1e-12);
	BOOST_CHECK_SMALL(spectrum.eigenvaluesDescending(1) - 4, 1e-12);
	BOOST_CHECK_SMALL(spectrum.eigenvaluesDescending(2) - 1, 1e-12);
	BOOST_CHECK_SMALL(spectrum.totalWeightedGain - 14, 1e-12);
	BOOST_CHECK_SMALL(spectrum.rho(1) - 9.0 / 14, 1e-12);
	BOOST_CHECK_SMALL(spectrum.rho(2) - 13.0 / 14, 1e-12);
	BOOST_CHECK_SMALL(spectrum.rho(20) - 1, 1e-12);
	BOOST_CHECK_EQUAL(spectrum.minimumRankForRho(0.80), 2);
	BOOST_CHECK_EQUAL(spectrum.minimumRankForRho(0.95), 3);
}

BOOST_AUTO_TEST_CASE(iar_product_gain_spectrum_is_ambiguity_basis_invariant)
{
	Matrix3d ambiguityCovariance;
	ambiguityCovariance <<
		0.12, 0.03, 0.01,
		0.03, 0.15, 0.02,
		0.01, 0.02, 0.09;
	Matrix<double, 3, 2> ambiguityProductCross;
	ambiguityProductCross <<
		0.04, -0.02,
		0.01,  0.03,
		0.02,  0.01;
	Matrix2d productWeight;
	productWeight << 2.0, 0.2, 0.2, 0.7;
	Matrix3d transform;
	transform <<
		1, 2, 0,
		0, 1, 1,
		1, 0, 1;
	const auto baseline = zhangIarProductGainSpectrum(
		ambiguityCovariance, ambiguityProductCross, productWeight);
	const auto changed = zhangIarProductGainSpectrum(
		transform * ambiguityCovariance * transform.transpose(),
		transform * ambiguityProductCross,
		productWeight);
	BOOST_REQUIRE_MESSAGE(baseline.valid, baseline.failureReason);
	BOOST_REQUIRE_MESSAGE(changed.valid, changed.failureReason);
	BOOST_REQUIRE_EQUAL(
		baseline.eigenvaluesDescending.size(),
		changed.eigenvaluesDescending.size());
	BOOST_CHECK_SMALL(
		(baseline.eigenvaluesDescending -
		 changed.eigenvaluesDescending).norm(),
		1e-11);
	BOOST_CHECK_SMALL(
		baseline.totalWeightedGain - changed.totalWeightedGain,
		1e-11);
	BOOST_CHECK_SMALL(baseline.rho(2) - changed.rho(2), 1e-11);
}

BOOST_AUTO_TEST_CASE(iar_product_gain_spectrum_rejects_nullspace_cross_covariance)
{
	Matrix2d singularCovariance = Matrix2d::Zero();
	singularCovariance(0, 0) = 4;
	Vector2d validCross(2, 0);
	const auto valid = zhangIarProductGainSpectrum(
		singularCovariance,
		validCross,
		Matrix<double, 1, 1>::Identity());
	BOOST_REQUIRE_MESSAGE(valid.valid, valid.failureReason);
	BOOST_CHECK_EQUAL(valid.ambiguityRank, 1);
	BOOST_CHECK_SMALL(valid.totalWeightedGain - 1, 1e-12);

	Vector2d invalidCross(2, 0.1);
	const auto invalid = zhangIarProductGainSpectrum(
		singularCovariance,
		invalidCross,
		Matrix<double, 1, 1>::Identity());
	BOOST_CHECK(!invalid.valid);
	BOOST_CHECK_EQUAL(
		invalid.failureReason,
		"CROSS_COVARIANCE_OUTSIDE_AMBIGUITY_RANGE");
}

BOOST_AUTO_TEST_CASE(lambda_beam_bootstrap_log_failure_does_not_saturate)
{
	Vector3d conditionalVariances(1e-4, 2e-4, 5e-4);
	const double logFailure =
		zhangBootstrapLogFailure(conditionalVariances);
	BOOST_CHECK(std::isfinite(logFailure));
	BOOST_CHECK_LT(logFailure, -100);

	Vector2d moderate(0.08, 0.12);
	const double moderateLogFailure = zhangBootstrapLogFailure(moderate);
	const double directSuccess =
		std::erf(std::sqrt(1 / (8 * moderate(0)))) *
		std::erf(std::sqrt(1 / (8 * moderate(1))));
	BOOST_CHECK_SMALL(
		std::exp(moderateLogFailure) - (1 - directSuccess), 1e-14);
}

BOOST_AUTO_TEST_CASE(e25b_joint_user_integer_functional_s0_s1_closes_exactly)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g02(E_Sys::GPS, 2);
	SatSys g03(E_Sys::GPS, 3);
	std::set<ZhangGraphEdge> edges = {
		{"R0", g01}, {"R0", g02}, {"R0", g03},
		{"R1", g01}, {"R1", g02}, {"R1", g03},
		{"R2", g01}, {"R2", g02}, {"R2", g03}
	};
	ZhangGraphBasis productBasis = zhangBuildSpanningTree(
		edges, "R0",
		{{"R0", g01}, {"R1", g01}, {"R1", g02},
		 {"R2", g02}, {"R2", g03}});
	BOOST_REQUIRE(productBasis.connected);
	std::map<ZhangGraphEdge, int> versions;
	for (const auto& edge : edges)
	{
		versions[edge] = 4;
	}
	auto products = zhangBuildProductIntegerFunctionals(
		productBasis, versions, g01, 7);
	BOOST_REQUIRE_EQUAL(products.size(), 3);
	for (const auto& [satellite, product] : products)
	{
		BOOST_REQUIRE(product.valid);
		BOOST_CHECK_NE(
			zhangProductIntegerFunctionalFingerprint(product), "INVALID");
	}

	auto functional = zhangBuildJointUserIntegerFunctional(
		products, g01, 5);
	BOOST_REQUIRE(functional.valid);
	auto audit = zhangAuditUserIntegerLattice(functional);
	BOOST_REQUIRE_MESSAGE(audit.valid, audit.failureReason);
	BOOST_CHECK(audit.nuisanceOrthogonal);
	BOOST_CHECK(audit.affineInteger);
	BOOST_CHECK(audit.primitiveAdmissible);
	BOOST_CHECK_SMALL(audit.maximumNuisanceCoefficient, 1e-15);
	BOOST_CHECK_SMALL(audit.maximumAffineIntegerError, 1e-15);

	std::mt19937 generator(20260808);
	std::uniform_int_distribution<int> integerDistribution(-100, 100);
	std::uniform_real_distribution<double> nuisanceDistribution(-100, 100);
	ZhangExactVector integers(functional.integerRows.front().size());
	for (auto& integer : integers)
	{
		integer = integerDistribution(generator);
	}
	VectorXd nuisanceA(5);
	VectorXd nuisanceB(5);
	for (int index = 0; index < 5; index++)
	{
		nuisanceA(index) = nuisanceDistribution(generator);
		nuisanceB(index) = nuisanceDistribution(generator);
	}
	VectorXd valueA = functional.value(integers, nuisanceA);
	VectorXd valueB = functional.value(integers, nuisanceB);
	BOOST_REQUIRE_EQUAL(valueA.size(), 2);
	BOOST_CHECK_SMALL((valueA - valueB).norm(), 1e-12);
	for (int row = 0; row < valueA.size(); row++)
	{
		BOOST_CHECK_SMALL(valueA(row) - std::round(valueA(row)), 1e-12);
	}
}

BOOST_AUTO_TEST_CASE(e25b_random_tree_exchange_preserves_fixed_product_lattice)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g02(E_Sys::GPS, 2);
	SatSys g03(E_Sys::GPS, 3);
	std::set<ZhangGraphEdge> edges = {
		{"R0", g01}, {"R0", g02}, {"R0", g03},
		{"R1", g01}, {"R1", g02}, {"R1", g03},
		{"R2", g01}, {"R2", g02}, {"R2", g03}
	};
	ZhangGraphBasis currentA = zhangBuildSpanningTree(
		edges, "R0",
		{{"R0", g01}, {"R0", g02}, {"R0", g03},
		 {"R1", g01}, {"R2", g02}});
	ZhangGraphBasis currentB = zhangBuildSpanningTree(
		edges, "R0",
		{{"R0", g01}, {"R1", g01}, {"R1", g03},
		 {"R2", g02}, {"R2", g03}});
	ZhangGraphBasis fixedProduct = zhangBuildSpanningTree(
		edges, "R0",
		{{"R0", g02}, {"R1", g02}, {"R1", g03},
		 {"R2", g01}, {"R2", g03}});
	BOOST_REQUIRE(currentA.connected && currentB.connected && fixedProduct.connected);
	BOOST_REQUIRE(currentA.treeEdges != currentB.treeEdges);

	ZhangSatelliteProductTarget representationA =
		zhangBuildSatelliteProductTarget(currentA, fixedProduct, g01);
	ZhangSatelliteProductTarget representationB =
		zhangBuildSatelliteProductTarget(currentB, fixedProduct, g01);
	BOOST_REQUIRE(representationA.valid && representationB.valid);
	BOOST_CHECK(
		zhangExactAbs(zhangExactDeterminant(
			zhangCanonicalTransition(currentA, currentB))) == 1);

	std::map<ZhangGraphEdge, int> versions;
	for (const auto& edge : edges)
	{
		versions[edge] = 9;
	}
	auto fixedProductsA = zhangBuildProductIntegerFunctionals(
		fixedProduct, versions, g01, 12);
	auto fixedProductsB = zhangBuildProductIntegerFunctionals(
		fixedProduct, versions, g01, 12);
	BOOST_REQUIRE_EQUAL(fixedProductsA.size(), fixedProductsB.size());
	for (const auto& [satellite, product] : fixedProductsA)
	{
		BOOST_CHECK_EQUAL(
			zhangProductIntegerFunctionalFingerprint(product),
			zhangProductIntegerFunctionalFingerprint(fixedProductsB.at(satellite)));
	}
	auto userA = zhangBuildJointUserIntegerFunctional(fixedProductsA, g01, 3);
	auto userB = zhangBuildJointUserIntegerFunctional(fixedProductsB, g01, 3);
	BOOST_REQUIRE(zhangAuditUserIntegerLattice(userA).valid);
	BOOST_REQUIRE(zhangAuditUserIntegerLattice(userB).valid);
	BOOST_CHECK(userA.integerRows == userB.integerRows);
	BOOST_CHECK(
		zhangClassifyTemporalIntegerDatumAction(false, true, false) ==
		ZhangTemporalIntegerDatumAction::EXACT_TRANSPORT_NO_BESD);
}

BOOST_AUTO_TEST_CASE(e25b_cycle_slip_requires_besd_or_datum_reset)
{
	BOOST_CHECK(
		zhangClassifyTemporalIntegerDatumAction(true, true, true) ==
		ZhangTemporalIntegerDatumAction::ESTIMATE_BESD);
	BOOST_CHECK(
		zhangClassifyTemporalIntegerDatumAction(true, true, false) ==
		ZhangTemporalIntegerDatumAction::RESET_PRODUCT_DATUM);
	BOOST_CHECK(
		zhangClassifyTemporalIntegerDatumAction(false, false, false) ==
		ZhangTemporalIntegerDatumAction::RESET_PRODUCT_DATUM);
}

BOOST_AUTO_TEST_CASE(targeted_besd_selector_routes_only_nonheld_retired_arcs)
{
	const SatSys g08(E_Sys::GPS, 8);
	const ZhangGraphEdge active{"R0", g08};
	const ZhangGraphEdge retired{"R1", g08};
	ZhangProductIntegerTransition transition;
	transition.physicalEdges = {active, retired};
	transition.physicalArcVersions = {3, 7};
	transition.coefficients = {1, -1};
	transition.valid = true;
	const std::set<ZhangGraphEdge> postEventEdges = {active};
	const std::map<ZhangGraphEdge, int> postEventVersions = {{active, 3}};

	const auto selected = zhangSelectTargetedBesdTransition(
		transition, postEventEdges, postEventVersions, false, false);
	BOOST_CHECK(selected.selected);
	BOOST_CHECK_EQUAL(selected.reason, "REQUIRES_BESD_RETIRED_ARC");
	BOOST_CHECK_EQUAL(selected.physicalTerms, 2);

	const auto held = zhangSelectTargetedBesdTransition(
		transition, postEventEdges, postEventVersions, true, false);
	BOOST_CHECK(!held.selected);
	BOOST_CHECK_EQUAL(held.reason, "EXACT_HELD_TRANSPORT");

	const auto reset = zhangSelectTargetedBesdTransition(
		transition, postEventEdges, postEventVersions, false, true);
	BOOST_CHECK(!reset.selected);
	BOOST_CHECK_EQUAL(reset.reason, "PHASE_SEGMENT_RESET");

	const std::set<ZhangGraphEdge> allEdges = {active, retired};
	const std::map<ZhangGraphEdge, int> allVersions = {
		{active, 3}, {retired, 7}};
	const auto current = zhangSelectTargetedBesdTransition(
		transition, allEdges, allVersions, false, false);
	BOOST_CHECK(!current.selected);
	BOOST_CHECK_EQUAL(current.reason, "CURRENT_PHYSICAL_GRAPH_RELATION");

	auto versionChanged = allVersions;
	versionChanged[retired] = 8;
	const auto changed = zhangSelectTargetedBesdTransition(
		transition, allEdges, versionChanged, false, false);
	BOOST_CHECK(changed.selected);
	BOOST_CHECK_EQUAL(changed.reason, "REQUIRES_BESD_RETIRED_ARC");
}

BOOST_AUTO_TEST_CASE(e25b_product_phase_transport_is_current_node_plus_cycle_target)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g02(E_Sys::GPS, 2);
	ZhangGraphEdge first{"R0", g01};
	ZhangGraphEdge second{"R0", g02};
	ZhangProductIntegerFunctional reference;
	reference.satellite = g01;
	reference.referenceSatellite = g01;
	reference.physicalEdges = {first, second};
	reference.networkCoefficients = {0, 0};
	reference.physicalArcVersions = {3, 3};
	reference.affineOffsetCycles = 2;
	reference.valid = true;
	auto satellite = reference;
	satellite.satellite = g02;
	satellite.networkCoefficients = {2, -1};
	satellite.affineOffsetCycles = 5;
	std::map<SatSys, ZhangProductIntegerFunctional> products = {
		{g01, reference}, {g02, satellite}
	};
	auto joint = zhangBuildJointUserIntegerFunctional(products, g01, 0);
	BOOST_REQUIRE(zhangAuditUserIntegerLattice(joint).valid);

	// Primitive order: two network arcs, then user G01/G02 ambiguities.
	ZhangExactVector primitive = {4, -3, 11, 24};
	const long long networkPath = 2 * 4 - (-3);
	const long long userSd = 24 - 11;
	const long long productAffineDifference = 5 - 2;
	VectorXd value = joint.value(primitive, VectorXd());
	BOOST_REQUIRE_EQUAL(value.size(), 1);
	BOOST_CHECK_EQUAL(
		value(0), userSd - networkPath - productAffineDifference);
	BOOST_CHECK_EQUAL(
		joint.affineOffsetsCycles(0), -productAffineDifference);

	// The service state is the satellite phase bias B_P.  The current-tree
	// state already contains z_T, so exact product transport must add G*k.
	// Since the user model applies -(C-B_P) = -C+B_P, the resulting ambiguity
	// is the user SD minus the complete product-tree node potential.
	const long long currentTreeNode = -5;
	const long long productTreeNode =
		currentTreeNode + networkPath + productAffineDifference;
	BOOST_CHECK_EQUAL(
		productTreeNode,
		currentTreeNode + networkPath + productAffineDifference);
	BOOST_CHECK_EQUAL(userSd - productTreeNode,
		userSd - currentTreeNode - networkPath - productAffineDifference);
}

BOOST_AUTO_TEST_CASE(product_physical_identity_ignores_unrelated_tree_generation)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g02(E_Sys::GPS, 2);
	ZhangProductIntegerFunctional functional;
	functional.satellite = g02;
	functional.referenceSatellite = g01;
	functional.physicalEdges = {{"R0", g01}, {"R0", g02}, {"R1", g02}};
	functional.networkCoefficients = {1, -1, 0};
	functional.physicalArcVersions = {4, 7, 99};
	functional.temporalBasisVersion = 12;
	functional.valid = true;

	auto nextGeneration = functional;
	nextGeneration.temporalBasisVersion = 13;
	nextGeneration.physicalArcVersions[2] = 100;
	BOOST_CHECK_EQUAL(
		zhangProductPhysicalFunctionalFingerprint(functional),
		zhangProductPhysicalFunctionalFingerprint(nextGeneration));
	BOOST_CHECK_NE(
		zhangProductIntegerFunctionalFingerprint(functional),
		zhangProductIntegerFunctionalFingerprint(nextGeneration));

	auto changedSupport = nextGeneration;
	changedSupport.physicalArcVersions[1] = 8;
	BOOST_CHECK_NE(
		zhangProductPhysicalFunctionalFingerprint(functional),
		zhangProductPhysicalFunctionalFingerprint(changedSupport));
}

BOOST_AUTO_TEST_CASE(product_functional_difference_preserves_arc_versions_exactly)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g02(E_Sys::GPS, 2);
	ZhangGraphEdge a{"R0", g01};
	ZhangGraphEdge b{"R0", g02};
	ZhangProductIntegerFunctional previous;
	previous.satellite = g02;
	previous.referenceSatellite = g01;
	previous.physicalEdges = {a, b};
	previous.networkCoefficients = {1, -1};
	previous.physicalArcVersions = {4, 7};
	previous.affineOffsetCycles = 2;
	previous.valid = true;

	auto generationOnly = previous;
	generationOnly.temporalBasisVersion = 99;
	auto zero = zhangProductIntegerFunctionalDifference(
		previous, generationOnly);
	BOOST_REQUIRE(zero.valid);
	BOOST_CHECK(zero.coefficients.empty());
	BOOST_CHECK_EQUAL(zero.affineOffsetCycles, 0);

	auto current = previous;
	current.physicalArcVersions[1] = 8;
	current.affineOffsetCycles = 5;
	auto changed = zhangProductIntegerFunctionalDifference(previous, current);
	BOOST_REQUIRE(changed.valid);
	BOOST_REQUIRE_EQUAL(changed.coefficients.size(), 2);
	BOOST_CHECK(changed.physicalEdges[0] == b);
	BOOST_CHECK(changed.physicalEdges[1] == b);
	BOOST_CHECK_EQUAL(changed.physicalArcVersions[0], 7);
	BOOST_CHECK_EQUAL(changed.physicalArcVersions[1], 8);
	BOOST_CHECK_EQUAL(changed.coefficients[0], 1);
	BOOST_CHECK_EQUAL(changed.coefficients[1], -1);
	BOOST_CHECK_EQUAL(changed.affineOffsetCycles, 3);
}

BOOST_AUTO_TEST_CASE(product_pair_difference_accepts_distinct_satellites_exactly)
{
	SatSys g01(E_Sys::GPS, 1);
	SatSys g02(E_Sys::GPS, 2);
	SatSys g03(E_Sys::GPS, 3);
	ZhangGraphEdge a{"R0", g01};
	ZhangGraphEdge b{"R0", g02};
	ZhangGraphEdge c{"R0", g03};
	ZhangProductIntegerFunctional first;
	first.satellite = g02;
	first.referenceSatellite = g01;
	first.physicalEdges = {a, b};
	first.networkCoefficients = {1, -1};
	first.physicalArcVersions = {4, 7};
	first.affineOffsetCycles = 2;
	first.valid = true;
	ZhangProductIntegerFunctional second;
	second.satellite = g03;
	second.referenceSatellite = g01;
	second.physicalEdges = {b, c};
	second.networkCoefficients = {1, -1};
	second.physicalArcVersions = {7, 9};
	second.affineOffsetCycles = 7;
	second.valid = true;

	BOOST_CHECK(!zhangProductIntegerFunctionalDifference(first, second).valid);
	const auto pair = zhangProductIntegerFunctionalPairDifference(first, second);
	BOOST_REQUIRE_MESSAGE(pair.valid, pair.failureReason);
	BOOST_REQUIRE_EQUAL(pair.coefficients.size(), 3);
	BOOST_CHECK_EQUAL(pair.coefficients[0], -1);
	BOOST_CHECK_EQUAL(pair.coefficients[1], 2);
	BOOST_CHECK_EQUAL(pair.coefficients[2], -1);
	BOOST_CHECK_EQUAL(pair.affineOffsetCycles, 5);
}

BOOST_AUTO_TEST_CASE(e25b_rejects_nuisance_fractional_offset_and_unsaturated_rows)
{
	ZhangJointUserIntegerFunctional valid;
	valid.integerRows = {{1, -1, 0}, {0, 1, -1}};
	valid.nuisanceRows = MatrixXd::Zero(2, 2);
	valid.affineOffsetsCycles = VectorXd::Zero(2);
	valid.valid = true;
	BOOST_REQUIRE(zhangAuditUserIntegerLattice(valid).valid);

	auto nuisanceLeak = valid;
	nuisanceLeak.nuisanceRows(0, 1) = 0.01;
	auto nuisanceAudit = zhangAuditUserIntegerLattice(nuisanceLeak);
	BOOST_CHECK(!nuisanceAudit.valid);
	BOOST_CHECK_EQUAL(
		nuisanceAudit.failureReason,
		"REAL_NUISANCE_LEAKS_INTO_INTEGER_FUNCTIONAL");

	auto fractionalOffset = valid;
	fractionalOffset.affineOffsetsCycles(1) = 0.25;
	auto offsetAudit = zhangAuditUserIntegerLattice(fractionalOffset);
	BOOST_CHECK(!offsetAudit.valid);
	BOOST_CHECK_EQUAL(offsetAudit.failureReason, "NON_INTEGER_AFFINE_OFFSET");

	auto unsaturated = valid;
	for (auto& row : unsaturated.integerRows)
	for (auto& coefficient : row)
	{
		coefficient *= 2;
	}
	auto saturationAudit = zhangAuditUserIntegerLattice(unsaturated);
	BOOST_CHECK(!saturationAudit.valid);
	BOOST_CHECK(!saturationAudit.primitiveAdmissible);
}

BOOST_AUTO_TEST_CASE(e29_checkpoint_core_roundtrip_is_bitwise_and_preserves_callbacks)
{
	TemporaryCheckpointFile file("_roundtrip.bin");
	auto bundle = makeCheckpointTestBundle();
	auto writeResult = writeZhangCheckpointBundle(
		file.path.string(), bundle);
	BOOST_REQUIRE_MESSAGE(writeResult.valid, writeResult.failureReason);
	BOOST_CHECK_EQUAL(writeResult.payloadSha256.size(), 64);
	BOOST_CHECK_GT(writeResult.payloadBytes, 0);
	std::string fileHashFailure;
	BOOST_CHECK_EQUAL(
		zhangCheckpointFileSha256(file.path.string(), &fileHashFailure).size(),
		64);
	BOOST_CHECK_EQUAL(fileHashFailure, "NONE");

	ZhangCheckpointBundle restoredBundle;
	auto readResult = readZhangCheckpointBundle(
		file.path.string(), checkpointTestExpectations(), restoredBundle);
	BOOST_REQUIRE_MESSAGE(readResult.valid, readResult.failureReason);
	BOOST_CHECK_EQUAL(readResult.payloadSha256, writeResult.payloadSha256);
	BOOST_REQUIRE_EQUAL(restoredBundle.sections.count("zhang.graph"), 1);
	const auto& graphSection = restoredBundle.sections.at("zhang.graph");
	BOOST_CHECK_EQUAL(graphSection.schemaVersion, 1);
	BOOST_CHECK_EQUAL(graphSection.payload, "pointer-free-graph-runtime");
	BOOST_CHECK_EQUAL(
		graphSection.sha256,
		zhangCheckpointSha256(graphSection.payload));

	KFState unresolvedDestination;
	const VectorXd unresolvedBefore = unresolvedDestination.x;
	std::string unresolvedFailure;
	BOOST_CHECK(!restoreZhangCheckpointKfCoreWithReceiverResolver(
		restoredBundle.kfCore,
		unresolvedDestination,
		[](const std::string&) -> Receiver* { return nullptr; },
		&unresolvedFailure));
	BOOST_CHECK_EQUAL(
		unresolvedFailure,
		"CHECKPOINT_CORE_RECEIVER_POINTER_REBIND_FAILED:R0");
	BOOST_CHECK(
		(unresolvedDestination.x.array() == unresolvedBefore.array()).all());

	KFState destination;
	destination.acceptedMeasurementFactorCallback = [](
		const KFState&,
		const KFMeas&,
		const std::string&,
		const VectorXd&,
		const MatrixXd&) {};
	std::string restoreFailure;
	BOOST_REQUIRE(restoreZhangCheckpointKfCoreWithReceiverResolver(
		restoredBundle.kfCore,
		destination,
		[](const std::string& id) -> Receiver*
		{
			return id == "R0" ? &checkpointTestReceiver() : nullptr;
		},
		&restoreFailure));
	BOOST_CHECK_EQUAL(restoreFailure, "NONE");
	BOOST_CHECK(static_cast<bool>(
		destination.acceptedMeasurementFactorCallback));
	BOOST_CHECK_EQUAL(zhangCheckpointRuntimeId(destination), "runtime-00");
	std::string bindFailure;
	BOOST_CHECK(bindZhangCheckpointRuntimeId(
		destination, "runtime-00", &bindFailure));
	BOOST_CHECK_EQUAL(bindFailure, "NONE");
	BOOST_CHECK(!bindZhangCheckpointRuntimeId(
		destination, "different-runtime", &bindFailure));
	BOOST_CHECK_EQUAL(bindFailure, "CHECKPOINT_RUNTIME_ID_ALREADY_BOUND");
	BOOST_CHECK(zhangCheckpointKfCoreBitwiseEqual(
		restoredBundle.kfCore,
		captureZhangCheckpointKfCore(destination)));
	BOOST_REQUIRE_EQUAL(destination.filterChunkMap.count("zhang"), 1);
	const auto& chunk = destination.filterChunkMap.at("zhang");
	BOOST_CHECK_EQUAL(chunk.begH, 7);
	BOOST_CHECK_EQUAL(chunk.numH, 11);
	bool estimatedTimeRestored = false;
	bool receiverPointerRestored = false;
	for (const auto& [key, index] : destination.kfIndexMap)
	{
		if (key.type == KF::REC_CLOCK && key.str == "R0")
		{
			receiverPointerRestored =
				key.rec_ptr == &checkpointTestReceiver();
		}
		if (key.type == KF::SAT_CLOCK && key.Sat == SatSys(E_Sys::GPS, 7))
		{
			estimatedTimeRestored =
				key.estimatedTime.bigTime == 123456710.25L;
		}
	}
	BOOST_CHECK(receiverPointerRestored);
	BOOST_CHECK(estimatedTimeRestored);
}

BOOST_AUTO_TEST_CASE(e29_checkpoint_rejects_corruption_and_provenance_drift)
{
	TemporaryCheckpointFile file("_corrupt.bin");
	auto bundle = makeCheckpointTestBundle();
	auto writeResult = writeZhangCheckpointBundle(
		file.path.string(), bundle);
	BOOST_REQUIRE_MESSAGE(writeResult.valid, writeResult.failureReason);

	auto wrongExpectations = checkpointTestExpectations();
	wrongExpectations.configSha256 = std::string(64, 'd');
	ZhangCheckpointBundle ignored;
	auto provenanceResult = readZhangCheckpointBundle(
		file.path.string(), wrongExpectations, ignored);
	BOOST_CHECK(!provenanceResult.valid);
	BOOST_CHECK_EQUAL(
		provenanceResult.failureReason,
		"CHECKPOINT_PROVENANCE_MISMATCH");

	std::fstream stream(
		file.path, std::ios::binary | std::ios::in | std::ios::out);
	BOOST_REQUIRE(stream);
	stream.seekg(-1, std::ios::end);
	char byte = 0;
	stream.read(&byte, 1);
	BOOST_REQUIRE(stream);
	byte ^= 0x5a;
	stream.seekp(-1, std::ios::end);
	stream.write(&byte, 1);
	stream.flush();
	BOOST_REQUIRE(stream);
	stream.close();

	auto corruptResult = readZhangCheckpointBundle(
		file.path.string(), checkpointTestExpectations(), ignored);
	BOOST_CHECK(!corruptResult.valid);
	BOOST_CHECK_EQUAL(
		corruptResult.failureReason,
		"CHECKPOINT_PAYLOAD_SHA256_MISMATCH");
}

BOOST_AUTO_TEST_CASE(e29_checkpoint_writer_fails_closed_on_invalid_identity_and_index)
{
	TemporaryCheckpointFile missingIdentityFile("_identity.bin");
	auto missingIdentity = makeCheckpointTestBundle();
	missingIdentity.manifest.runtimeId.clear();
	auto identityResult = writeZhangCheckpointBundle(
		missingIdentityFile.path.string(), missingIdentity);
	BOOST_CHECK(!identityResult.valid);
	BOOST_CHECK_EQUAL(
		identityResult.failureReason,
		"CHECKPOINT_MANIFEST_IDENTITY_MISSING");
	BOOST_CHECK(!std::filesystem::exists(missingIdentityFile.path));

	TemporaryCheckpointFile badIndexFile("_index.bin");
	auto badIndex = makeCheckpointTestBundle();
	auto firstIndex = badIndex.kfCore.kfIndexMap.begin();
	auto secondIndex = std::next(firstIndex);
	secondIndex->second = firstIndex->second;
	auto indexResult = writeZhangCheckpointBundle(
		badIndexFile.path.string(), badIndex);
	BOOST_CHECK(!indexResult.valid);
	BOOST_CHECK_EQUAL(
		indexResult.failureReason,
		"CHECKPOINT_CORE_INDEX_NOT_BIJECTIVE");
	BOOST_CHECK(!std::filesystem::exists(badIndexFile.path));

	TemporaryCheckpointFile badContentFile("_content.bin");
	auto badContent = makeCheckpointTestBundle();
	badContent.manifest.configText += "-tampered";
	auto contentResult = writeZhangCheckpointBundle(
		badContentFile.path.string(), badContent);
	BOOST_CHECK(!contentResult.valid);
	BOOST_CHECK_EQUAL(
		contentResult.failureReason,
		"CHECKPOINT_MANIFEST_CONTENT_HASH_MISMATCH");
}

BOOST_AUTO_TEST_CASE(e29_checkpoint_required_sections_are_strictly_validated)
{
	auto bundle = makeCheckpointTestBundle();
	const std::vector<ZhangCheckpointSectionRequirement> requirements = {
		{"zhang.graph", 1}};
	std::string failure;
	BOOST_CHECK(validateZhangCheckpointRequiredSections(
		bundle, requirements, &failure));
	BOOST_CHECK_EQUAL(failure, "NONE");

	auto missing = bundle;
	missing.sections.clear();
	BOOST_CHECK(!validateZhangCheckpointRequiredSections(
		missing, requirements, &failure));
	BOOST_CHECK_EQUAL(
		failure, "CHECKPOINT_REQUIRED_SECTION_MISSING:zhang.graph");

	auto wrongVersion = bundle;
	wrongVersion.sections.at("zhang.graph").schemaVersion = 2;
	BOOST_CHECK(!validateZhangCheckpointRequiredSections(
		wrongVersion, requirements, &failure));
	BOOST_CHECK_EQUAL(
		failure,
		"CHECKPOINT_REQUIRED_SECTION_VERSION_MISMATCH:zhang.graph");

	auto corrupt = bundle;
	corrupt.sections.at("zhang.graph").payload += "-corrupt";
	BOOST_CHECK(!validateZhangCheckpointRequiredSections(
		corrupt, requirements, &failure));
	BOOST_CHECK_EQUAL(
		failure, "CHECKPOINT_REQUIRED_SECTION_HASH_MISMATCH:zhang.graph");
}

BOOST_AUTO_TEST_CASE(e29_checkpoint_manifest_json_is_atomic_and_auditable)
{
	TemporaryCheckpointFile file("_manifest.json");
	auto bundle = makeCheckpointTestBundle();
	auto writeResult = writeZhangCheckpointManifestJson(
		file.path.string(), bundle);
	BOOST_REQUIRE_MESSAGE(writeResult.valid, writeResult.failureReason);
	BOOST_CHECK_EQUAL(writeResult.payloadSha256.size(), 64);
	BOOST_CHECK_GT(writeResult.payloadBytes, 0);

	std::ifstream input(file.path, std::ios::binary);
	BOOST_REQUIRE(input);
	const std::string json(
		(std::istreambuf_iterator<char>(input)),
		std::istreambuf_iterator<char>());
	BOOST_CHECK_NE(json.find("\"runtime_id\": \"runtime-00\""),
		std::string::npos);
	BOOST_CHECK_NE(json.find("\"state_dimension\": 3"),
		std::string::npos);
	BOOST_CHECK_NE(json.find("\"name\": \"zhang.graph\""),
		std::string::npos);
	BOOST_CHECK_NE(json.find(
		bundle.sections.at("zhang.graph").sha256), std::string::npos);

	auto secondWrite = writeZhangCheckpointManifestJson(
		file.path.string(), bundle);
	BOOST_CHECK(!secondWrite.valid);
	BOOST_CHECK_EQUAL(
		secondWrite.failureReason, "CHECKPOINT_TARGET_ALREADY_EXISTS");
}

BOOST_AUTO_TEST_CASE(product_relation_admission_commits_exact_redundant_batch)
{
	ZhangProductRelationAdmissionState state;
	auto candidate = [](
		const std::string& id,
		const std::string& satellite,
		const std::string& observable,
		const std::map<std::string, ZhangExactInteger>& row,
		long long value)
	{
		ZhangProductRelationAdmissionCandidate result;
		result.relationId = id;
		result.satellite = satellite;
		result.observable = observable;
		result.physicalCoefficients = row;
		result.integerValue = value;
		result.exactIntegerEstimable = true;
		result.phaseSegmentCompatible = true;
		result.scalarReliabilityPassed = true;
		result.jointNisPassed = true;
		return result;
	};
	const std::vector<ZhangProductRelationAdmissionCandidate> rows = {
		candidate("G02-L1-a", "G02", "L1C", {{"a", 1}}, 3),
		candidate("G02-L1-a-repeat", "G02", "L1C", {{"a", 1}}, 3),
		candidate("G02-L2-b", "G02", "L2W", {{"b", 1}}, -2),
		candidate("G02-L2-b-repeat", "G02", "L2W", {{"b", 1}}, -2),
	};
	const auto result = ProductRelationAdmission::admit(state, rows);
	BOOST_CHECK(result.committed);
	BOOST_CHECK_EQUAL(result.status, "CERTIFIED_NEW_RELATION");
	BOOST_CHECK_EQUAL(result.candidateRows, 4);
	BOOST_CHECK_EQUAL(result.candidateExactRank, 2);
	BOOST_CHECK_EQUAL(result.candidateRedundantRows, 2);
	BOOST_CHECK(result.candidateCycleClosureConsistent);
	BOOST_CHECK(result.persistentCycleClosureConsistent);
	BOOST_CHECK_EQUAL(result.persistentRankAfter, 2);
	BOOST_CHECK_EQUAL(result.restoredSatellites, 1);
	BOOST_CHECK_EQUAL(state.certifiedSatellites.count("G02"), 1);
}

BOOST_AUTO_TEST_CASE(product_relation_admission_aborts_conflicting_cycle_atomically)
{
	ZhangProductRelationAdmissionState state;
	ZhangProductRelationAdmissionCandidate first;
	first.relationId = "first";
	first.satellite = "G02";
	first.observable = "L1C";
	first.physicalCoefficients = {{"a", 1}};
	first.integerValue = 3;
	first.exactIntegerEstimable = true;
	first.phaseSegmentCompatible = true;
	first.scalarReliabilityPassed = true;
	first.jointNisPassed = true;
	auto duplicate = first;
	duplicate.relationId = "conflict";
	duplicate.integerValue = 4;
	const auto result = ProductRelationAdmission::admit(
		state, {first, duplicate});
	BOOST_CHECK(!result.committed);
	BOOST_CHECK_EQUAL(
		result.status, "ABORT_INCONSISTENT_CANDIDATE_CYCLE_CLOSURE");
	BOOST_CHECK(state.certifiedRows.empty());
	BOOST_CHECK(state.certifiedSatellites.empty());
}

BOOST_AUTO_TEST_CASE(product_relation_admission_waits_for_redundancy)
{
	ZhangProductRelationAdmissionState state;
	ZhangProductRelationAdmissionCandidate candidate;
	candidate.relationId = "single-bridge";
	candidate.satellite = "G02";
	candidate.observable = "L1C";
	candidate.physicalCoefficients = {{"a", 1}};
	candidate.integerValue = 3;
	candidate.exactIntegerEstimable = true;
	candidate.phaseSegmentCompatible = true;
	candidate.scalarReliabilityPassed = true;
	candidate.jointNisPassed = true;
	const auto result = ProductRelationAdmission::admit(state, {candidate});
	BOOST_CHECK(!result.committed);
	BOOST_CHECK_EQUAL(
		result.status, "PREPARE_MERGE_AWAITING_REDUNDANCY");
	BOOST_CHECK(state.certifiedRows.empty());
	BOOST_CHECK_EQUAL(state.pendingCandidates.size(), 1);
	auto redundant = candidate;
	redundant.relationId = "redundant-bridge";
	const auto committed = ProductRelationAdmission::admit(
		state, {redundant});
	BOOST_CHECK(committed.committed);
	BOOST_CHECK_EQUAL(committed.candidateRows, 2);
	BOOST_CHECK_EQUAL(committed.candidateExactRank, 1);
	BOOST_CHECK_EQUAL(committed.candidateRedundantRows, 1);
	BOOST_CHECK(state.pendingCandidates.empty());
	BOOST_CHECK_EQUAL(state.certifiedRows.size(), 1);
}

BOOST_AUTO_TEST_CASE(product_relation_admission_fails_closed_at_every_gate)
{
	auto validCandidate = []()
	{
		ZhangProductRelationAdmissionCandidate candidate;
		candidate.relationId = "gate-a";
		candidate.satellite = "G02";
		candidate.observable = "L1C";
		candidate.physicalCoefficients = {{"a", 1}};
		candidate.integerValue = 3;
		candidate.exactIntegerEstimable = true;
		candidate.phaseSegmentCompatible = true;
		candidate.scalarReliabilityPassed = true;
		candidate.jointNisPassed = true;
		return candidate;
	};
	struct GateCase
	{
		std::string expected;
		std::function<void(ZhangProductRelationAdmissionCandidate&)> fail;
	};
	const std::vector<GateCase> cases = {
		{"REJECTED_NOT_EXACT_INTEGER_ESTIMABLE", [](auto& row)
			{ row.exactIntegerEstimable = false; }},
		{"REJECTED_PHASE_SEGMENT_INCOMPATIBLE", [](auto& row)
			{ row.phaseSegmentCompatible = false; }},
		{"REJECTED_SCALAR_RELIABILITY", [](auto& row)
			{ row.scalarReliabilityPassed = false; }},
		{"REJECTED_JOINT_NIS", [](auto& row)
			{ row.jointNisPassed = false; }},
	};
	for (const auto& test : cases)
	{
		ZhangProductRelationAdmissionState state;
		auto first = validCandidate();
		auto second = first;
		second.relationId = "gate-b";
		test.fail(first);
		const auto result = ProductRelationAdmission::admit(
			state, {first, second});
		BOOST_CHECK(!result.committed);
		BOOST_CHECK_EQUAL(result.status, test.expected);
		BOOST_CHECK(state.certifiedRows.empty());
		BOOST_CHECK(state.pendingCandidates.empty());
	}
}

BOOST_AUTO_TEST_CASE(product_relation_admission_requires_redundancy_per_signal)
{
	auto make = [](const std::string& id, const std::string& observable,
		const std::string& column, long long value)
	{
		ZhangProductRelationAdmissionCandidate candidate;
		candidate.relationId = id;
		candidate.satellite = "G02";
		candidate.observable = observable;
		candidate.physicalCoefficients = {{column, 1}};
		candidate.integerValue = value;
		candidate.exactIntegerEstimable = true;
		candidate.phaseSegmentCompatible = true;
		candidate.scalarReliabilityPassed = true;
		candidate.jointNisPassed = true;
		return candidate;
	};
	ZhangProductRelationAdmissionState state;
	const auto result = ProductRelationAdmission::admit(state, {
		make("l1-a", "L1C", "a", 3),
		make("l1-b", "L1C", "a", 3),
		make("l2-a", "L2W", "b", -2),
	});
	BOOST_CHECK(!result.committed);
	BOOST_CHECK_EQUAL(
		result.status, "PREPARE_MERGE_AWAITING_REDUNDANCY");
	BOOST_CHECK_EQUAL(result.observableGroups, 2);
	BOOST_CHECK_EQUAL(result.redundancyCheckedGroups, 1);
	BOOST_CHECK_EQUAL(state.pendingCandidates.size(), 3);
}

BOOST_AUTO_TEST_CASE(temporal_certificate_confirmation_is_value_gap_and_kind_safe)
{
	TemporalCertificateConfirmationState state;
	auto first = zhangConfirmTemporalCertificate(
		state, "L1C:G03-G02", ZhangExactInteger(7), 100,
		"path-a", false, 3, 30, false);
	BOOST_CHECK(!first.accepted);
	BOOST_CHECK_EQUAL(first.consistentEpochs, 1);

	// Re-evaluating the same epoch must not manufacture confirmation count.
	auto duplicate = zhangConfirmTemporalCertificate(
		state, "L1C:G03-G02", ZhangExactInteger(7), 100,
		"path-a", false, 3, 30, false);
	BOOST_CHECK_EQUAL(duplicate.consistentEpochs, 1);

	zhangConfirmTemporalCertificate(
		state, "L1C:G03-G02", ZhangExactInteger(7), 110,
		"path-a", false, 3, 30, false);
	auto third = zhangConfirmTemporalCertificate(
		state, "L1C:G03-G02", ZhangExactInteger(7), 120,
		"path-a", false, 3, 30, false);
	BOOST_CHECK(third.accepted);

	// An integer change is a new hypothesis, not a continuation.
	auto changed = zhangConfirmTemporalCertificate(
		state, "L1C:G03-G02", ZhangExactInteger(8), 130,
		"path-a", false, 3, 30, false);
	BOOST_CHECK(changed.reset);
	BOOST_CHECK_EQUAL(changed.consistentEpochs, 1);

	// A bridge additionally requires independent redundant support.
	TemporalCertificateConfirmationState bridge;
	auto noRedundancy = zhangConfirmTemporalCertificate(
		bridge, "L1C:G05-G02", ZhangExactInteger(2), 200,
		"path-a", false, 1, 30, true);
	BOOST_CHECK(!noRedundancy.accepted);
	BOOST_CHECK_EQUAL(noRedundancy.reason, "AWAITING_REDUNDANCY");
	auto redundant = zhangConfirmTemporalCertificate(
		bridge, "L1C:G05-G02", ZhangExactInteger(2), 210,
		"path-b", true, 1, 30, true);
	BOOST_CHECK(redundant.accepted);

	BOOST_CHECK_EQUAL(
		zhangTemporalCertificateKindName(
			TemporalCertificateKind::SELF_GAUGE_SHIFT),
		"SELF_GAUGE_SHIFT");
	BOOST_CHECK_EQUAL(
		zhangTemporalCertificateKindName(
			TemporalCertificateKind::INTER_SATELLITE_BRIDGE),
		"INTER_SATELLITE_BRIDGE");
}

BOOST_AUTO_TEST_CASE(targeted_besd_tracker_matches_augmented_kalman_update)
{
	VectorXd stateMean(2);
	stateMean << 1.2, -0.7;
	MatrixXd stateCovariance(2, 2);
	stateCovariance << 4.0, 0.6,
		0.6, 2.0;
	MatrixXd targets(2, 2);
	targets << 1, -1,
		2, 1;
	VectorXd offsets(2);
	offsets << 3, -2;

	ZhangTargetedBesdTracker tracker;
	BOOST_REQUIRE(tracker.initialise(
		{"old", "new"}, targets, offsets, stateMean, stateCovariance));

	MatrixXd design(1, 2);
	design << 0.5, 1.5;
	MatrixXd noise(1, 1);
	noise << 0.25;
	VectorXd residual(1);
	residual << -0.8;

	// Independent augmented-state reference update for [x, f].
	VectorXd jointMean(4);
	jointMean << stateMean, targets * stateMean + offsets;
	MatrixXd jointCovariance(4, 4);
	jointCovariance.topLeftCorner(2, 2) = stateCovariance;
	jointCovariance.bottomLeftCorner(2, 2) = targets * stateCovariance;
	jointCovariance.topRightCorner(2, 2) =
		jointCovariance.bottomLeftCorner(2, 2).transpose();
	jointCovariance.bottomRightCorner(2, 2) =
		targets * stateCovariance * targets.transpose();
	MatrixXd jointDesign = MatrixXd::Zero(1, 4);
	jointDesign.leftCols(2) = design;
	MatrixXd innovation = jointDesign * jointCovariance
		* jointDesign.transpose() + noise;
	MatrixXd gain = jointCovariance * jointDesign.transpose()
		* innovation.inverse();
	jointMean += gain * residual;
	jointCovariance -= gain * innovation * gain.transpose();
	jointCovariance = 0.5
		* (jointCovariance + jointCovariance.transpose());

	BOOST_REQUIRE(tracker.updateAcceptedMeasurement(
		stateCovariance, design, noise, residual));
	const auto marginal = tracker.marginal();
	BOOST_REQUIRE(marginal.valid);
	BOOST_CHECK_SMALL(
		(marginal.mean - jointMean.tail(2)).norm(), 1e-12);
	BOOST_CHECK_SMALL(
		(marginal.covariance
			- jointCovariance.bottomRightCorner(2, 2)).norm(), 1e-12);
	BOOST_CHECK_SMALL(
		(tracker.crossCovariance()
			- jointCovariance.bottomLeftCorner(2, 2)).norm(), 1e-12);
}

BOOST_AUTO_TEST_CASE(targeted_besd_tracker_carries_only_target_schur_boundary)
{
	VectorXd stateMean(3);
	stateMean << 1, 2, 3;
	MatrixXd stateCovariance = MatrixXd::Identity(3, 3);
	MatrixXd targets(2, 3);
	targets << 1, 0, -1,
		0, 2, 1;
	ZhangTargetedBesdTracker tracker;
	BOOST_REQUIRE(tracker.initialise(
		{"old", "new"}, targets, VectorXd::Zero(2),
		stateMean, stateCovariance));

	MatrixXd transition(2, 3);
	transition << 1, 0, 0,
		0, 1, 1;
	const MatrixXd expectedCross =
		tracker.crossCovariance() * transition.transpose();
	const auto before = tracker.marginal();
	BOOST_REQUIRE(tracker.advanceState(transition));
	const auto after = tracker.marginal();
	BOOST_REQUIRE(after.valid);
	BOOST_CHECK_EQUAL(tracker.targetCount(), 2);
	BOOST_CHECK_EQUAL(tracker.currentStateDimension(), 2);
	BOOST_CHECK_SMALL((after.mean - before.mean).norm(), 1e-15);
	BOOST_CHECK_SMALL(
		(after.covariance - before.covariance).norm(), 1e-15);
	BOOST_CHECK_SMALL(
		(tracker.crossCovariance() - expectedCross).norm(), 1e-15);

	// A malformed update fails closed and leaves no usable marginal.
	BOOST_CHECK(!tracker.updateAcceptedMeasurement(
		MatrixXd::Identity(3, 3), MatrixXd::Zero(1, 3),
		MatrixXd::Identity(1, 1), VectorXd::Zero(1)));
	BOOST_CHECK(!tracker.isActive());
	BOOST_CHECK(!tracker.marginal().valid);
}

BOOST_AUTO_TEST_CASE(product_relation_score_is_reliability_lexicographic)
{
	ZhangProductRelationLexicographicScore unreliable;
	unreliable.componentCoverageGain = 100;
	unreliable.productInformationGain = 1;
	ZhangProductRelationLexicographicScore reliable;
	reliable.reliabilityPassed = true;
	reliable.componentCoverageGain = 1;
	reliable.productInformationGain = 1e-6;
	BOOST_CHECK(unreliable < reliable);

	ZhangProductRelationLexicographicScore moreCoverage = reliable;
	moreCoverage.componentCoverageGain = 2;
	BOOST_CHECK(reliable < moreCoverage);

	ProductParBranch lowerGain;
	lowerGain.reliabilityPassed = true;
	lowerGain.componentCoverageGain = 2;
	lowerGain.productInformationGain = 0.2;
	ProductParBranch higherGain = lowerGain;
	higherGain.productInformationGain = 0.3;
	BOOST_CHECK(zhangProductParScore(lowerGain) <
		zhangProductParScore(higherGain));

	ProductParBranch weakUnreliable;
	weakUnreliable.integerRank = 20;
	weakUnreliable.rawPartialFixedRank = 3;
	weakUnreliable.partialFixFraction = 0.15;
	weakUnreliable.componentCoverageGain = 20;
	weakUnreliable.productInformationGain = 1;
	ProductParBranch fixableUnreliable = weakUnreliable;
	fixableUnreliable.integerRank = 6;
	fixableUnreliable.rawPartialFixedRank = 5;
	fixableUnreliable.partialFixFraction = 5.0 / 6.0;
	fixableUnreliable.componentCoverageGain = 6;
	fixableUnreliable.productInformationGain = 0.2;
	BOOST_CHECK(zhangProductParScore(weakUnreliable) <
		zhangProductParScore(fixableUnreliable));
}

BOOST_AUTO_TEST_CASE(product_relation_named_ordering_is_semantic)
{
	const SatSys g01("G01");
	const SatSys g02("G02");
	const SatSys g03("G03");
	ZhangProductRelationBasis first;
	first.mappableNamedIndices = {0, 1};
	first.namedRelations.resize(2);
	first.namedRelations[0].satellite = g02;
	first.namedRelations[0].referenceSatellite = g01;
	first.namedRelations[1].satellite = g03;
	first.namedRelations[1].referenceSatellite = g01;
	ZhangProductRelationBasis second = first;
	BOOST_CHECK(zhangProductNamedOrderingMatches(first, second));

	// Equal numeric indices do not rescue a semantic L1/L2 row swap.
	std::swap(second.namedRelations[0].satellite,
		second.namedRelations[1].satellite);
	BOOST_CHECK(!zhangProductNamedOrderingMatches(first, second));
}

BOOST_AUTO_TEST_CASE(product_relation_wl_gain_uses_joint_cross_covariance)
{
	MatrixXd jointCovariance(2, 2);
	jointCovariance << 4, 3,
		3, 4;
	ZhangIarFunctional wideLane(1, 2);
	wideLane.insert(0, 0) = 1;
	wideLane.insert(0, 1) = -1;
	wideLane.makeCompressed();
	const double gain = zhangNamedProductInformationGain(
		jointCovariance, wideLane);
	BOOST_CHECK_CLOSE(gain, 0.125, 1e-10);

	// This is Q11+Q22-Q12-Q21 = 2, not the cross-covariance-free value 8.
	const MatrixXd wideLaneCovariance = wideLane * jointCovariance *
		wideLane.transpose();
	BOOST_CHECK_CLOSE(wideLaneCovariance(0, 0), 2.0, 1e-10);
}

BOOST_AUTO_TEST_CASE(product_gain_spectrum_separates_rank_and_search_failures)
{
	BOOST_CHECK_EQUAL(
		zhangProductGainSpectrumDiagnosis(0.82, 0.03),
		"INTEGER_CANDIDATE_SUBSPACE_MISALIGNED");
	BOOST_CHECK_EQUAL(
		zhangProductGainSpectrumDiagnosis(0.08, 0.03),
		"REAL_RANK_CEILING_LOW_INCREASE_RANK");
	BOOST_CHECK_EQUAL(
		zhangProductGainSpectrumDiagnosis(0.82, 0.50),
		"INTEGER_SUBSET_USES_REAL_CEILING_EFFICIENTLY");
}

BOOST_AUTO_TEST_CASE(product_relation_wl_l1_transform_is_unimodular)
{
	ZhangExactMatrix transform = {
		{1, 0, -1, 0},
		{0, 1, 0, -1},
		{1, 0, 0, 0},
		{0, 1, 0, 0}
	};
	const auto smith = zhangIntegerRowLatticeContains(
		transform, ZhangExactVector(4));
	BOOST_REQUIRE_EQUAL(smith.smithInvariants.size(), 4);
	for (const auto& invariant : smith.smithInvariants)
	{
		BOOST_CHECK_EQUAL(invariant, 1);
	}
}

BOOST_AUTO_TEST_CASE(product_relation_partial_decorrelated_rows_are_not_certificate)
{
	const auto none = zhangRecoverCertifiedNamedProductSubset(
		{{1, 1}}, {7}, 2);
	BOOST_CHECK(none.empty());

	const auto certifiedPartial = zhangRecoverCertifiedNamedProductSubset(
		{{1, 1, 0}, {0, 1, 0}}, {7, 3}, 3);
	BOOST_REQUIRE_EQUAL(certifiedPartial.size(), 2);
	BOOST_CHECK_EQUAL(certifiedPartial.at(0), 4);
	BOOST_CHECK_EQUAL(certifiedPartial.at(1), 3);
	BOOST_CHECK(!certifiedPartial.contains(2));

	const auto partial = zhangRecoverCompleteNamedProductSubset(
		{{1, 1}}, {7}, 2);
	BOOST_CHECK(partial.empty());

	const auto complete = zhangRecoverCompleteNamedProductSubset(
		{{1, 1}, {0, 1}}, {7, 3}, 2);
	BOOST_REQUIRE_EQUAL(complete.size(), 2);
	BOOST_CHECK_EQUAL(complete.at(0), 4);
	BOOST_CHECK_EQUAL(complete.at(1), 3);
}

BOOST_AUTO_TEST_CASE(only_product_fixed_is_a_formal_pppar_product)
{
	BOOST_CHECK(zhangFormalPppArProductSolution("PRODUCT_FIXED"));
	BOOST_CHECK(!zhangFormalPppArProductSolution("FIXED"));
	BOOST_CHECK(!zhangFormalPppArProductSolution(
		"NETWORK_FIXED_DIAGNOSTIC"));
	BOOST_CHECK(!zhangFormalPppArProductSolution("NETWORK_WL"));
	BOOST_CHECK(!zhangFormalPppArProductSolution("FLOAT"));
}

BOOST_AUTO_TEST_CASE(user_rejects_legacy_network_fixed_ar_claim)
{
	ZhangInternalProduct legacy;
	legacy.solution = "FIXED";
	legacy.ppp_usable = true;
	legacy.pppar_usable = true;
	legacy.ar_valid = true;
	legacy.dual_frequency_ar_valid = true;
	BOOST_CHECK(zhangRejectNonFormalPppArClaim(legacy));
	BOOST_CHECK(legacy.ppp_usable);
	BOOST_CHECK(!legacy.pppar_usable);
	BOOST_CHECK(!legacy.ar_valid);
	BOOST_CHECK(!legacy.dual_frequency_ar_valid);
	BOOST_CHECK_EQUAL(legacy.invalid_reason,
		"NON_PRODUCT_FIXED_AR_CLAIM_REJECTED");

	ZhangInternalProduct formal;
	formal.solution = "PRODUCT_FIXED";
	formal.pppar_usable = true;
	formal.ar_valid = true;
	formal.dual_frequency_ar_valid = true;
	BOOST_CHECK(!zhangRejectNonFormalPppArClaim(formal));
	BOOST_CHECK(formal.pppar_usable);
	BOOST_CHECK(formal.ar_valid);
	BOOST_CHECK(formal.dual_frequency_ar_valid);
}

BOOST_AUTO_TEST_CASE(
	product_relation_named_rows_inherit_accepted_parent_lattice_certificate)
{
	const ZhangExactMatrix parentRows = {
		{1, 1, 0},
		{0, 1, 0}
	};
	const ZhangExactVector parentValues = {7, 3};
	const auto accepted = zhangPromoteNamedCertificateFromAcceptedParent(
		parentRows, parentValues, 3, true);
	BOOST_REQUIRE(accepted.exact);
	BOOST_CHECK_EQUAL(accepted.parentFixedRank, 2);
	BOOST_REQUIRE_EQUAL(accepted.values.size(), 2);
	BOOST_CHECK(accepted.values.at(0) == 4);
	BOOST_CHECK(accepted.values.at(1) == 3);
	BOOST_CHECK(!accepted.values.contains(2));

	// Exact algebra alone cannot bypass the statistical parent gate.
	const auto rejected = zhangPromoteNamedCertificateFromAcceptedParent(
		parentRows, parentValues, 3, false);
	BOOST_CHECK(!rejected.exact);
	BOOST_CHECK(rejected.values.empty());

	// A non-primitive row does not determine a named integer coordinate.
	const auto unsaturated = zhangPromoteNamedCertificateFromAcceptedParent(
		{{2}}, {6}, 1, true);
	BOOST_CHECK(!unsaturated.exact);
	BOOST_CHECK(unsaturated.values.empty());
}

BOOST_AUTO_TEST_CASE(
	product_relation_mixed_lattice_recovers_pair_without_star_coordinate)
{
	// u=z0-z1 is a directly named satellite-pair edge, although neither z0
	// nor z1 relative to the canonical reference is determined.
	const auto pairs = zhangRecoverCertifiedPairRelations(
		{{1, -1, 0}}, {4}, 3, true);
	BOOST_REQUIRE_EQUAL(pairs.size(), 1);
	BOOST_CHECK_EQUAL(pairs.front().firstNode, 0);
	BOOST_CHECK_EQUAL(pairs.front().secondNode, 1);
	BOOST_CHECK(pairs.front().value == 4);
	const auto stars = zhangPromoteNamedCertificateFromAcceptedParent(
		{{1, -1, 0}}, {4}, 3, true);
	BOOST_CHECK(stars.values.empty());

	// Statistical rejection dominates exact membership.
	BOOST_CHECK(zhangRecoverCertifiedPairRelations(
		{{1, -1, 0}}, {4}, 3, false).empty());
	// A higher-order combination is conditioning evidence, not a pair edge.
	BOOST_CHECK(zhangRecoverCertifiedPairRelations(
		{{1, 1, 1}}, {9}, 3, true).empty());
}

BOOST_AUTO_TEST_CASE(
	product_relation_reliability_forest_uses_only_passed_independent_edges)
{
	std::vector<ZhangPairReliabilityEdge> edges = {
		{0, 1, 1e-5, 0.01},
		{1, 2, 2e-5, 0.02},
		{0, 2, 3e-5, 0.03}, // reliable but closes a cycle
		{2, 3, 2e-3, 0.001}, // precise-looking but fails Perr
		{0, 3, 5e-4, 0.04}
	};
	const auto forest = zhangPairReliabilityForest(4, edges, 1e-3);
	BOOST_REQUIRE_EQUAL(forest.size(), 3);
	BOOST_CHECK_EQUAL(forest[0].firstNode, 0);
	BOOST_CHECK_EQUAL(forest[0].secondNode, 1);
	BOOST_CHECK_EQUAL(forest[1].firstNode, 1);
	BOOST_CHECK_EQUAL(forest[1].secondNode, 2);
	BOOST_CHECK_EQUAL(forest[2].firstNode, 0);
	BOOST_CHECK_EQUAL(forest[2].secondNode, 3);
}

BOOST_AUTO_TEST_CASE(
	product_relation_all_pair_gain_is_reference_invariant)
{
	MatrixXd q(3, 3);
	q << 0.4, 0.1, 0.05,
		 0.1, 0.3, 0.02,
		 0.05, 0.02, 0.2;
	const MatrixXd d = zhangAllPairIncidence(3);
	BOOST_REQUIRE_EQUAL(d.rows(), 6);
	BOOST_REQUIRE_EQUAL(d.cols(), 3);
	const double trace = zhangReferenceInvariantPairTrace(q);
	BOOST_CHECK_CLOSE(trace, (d * q * d.transpose()).trace(), 1e-10);

	// Change star reference from implicit node 3 to node 0.  The new named
	// coordinates are [K1-K0,K2-K0,K3-K0].
	MatrixXd transform(3, 3);
	transform << -1, 1, 0,
		-1, 0, 1,
		-1, 0, 0;
	const MatrixXd changed = transform * q * transform.transpose();
	BOOST_CHECK_CLOSE(trace, zhangReferenceInvariantPairTrace(changed), 1e-9);
}

BOOST_AUTO_TEST_CASE(
	product_relation_exact_conditioning_reports_reference_free_gain)
{
	VectorXd mean(3); mean << 1.1, 2.2, 3.3;
	MatrixXd q = MatrixXd::Identity(3, 3);
	MatrixXd rows(1, 3); rows << 1, -1, 0;
	VectorXd integer(1); integer << -1;
	const auto conditioned = zhangConditionExactProductRows(
		mean, q, rows, integer);
	BOOST_REQUIRE(conditioned.valid);
	BOOST_CHECK_EQUAL(conditioned.effectiveRank, 1);
	BOOST_CHECK_SMALL((rows * conditioned.covariance).norm(), 1e-12);
	BOOST_CHECK_SMALL((rows * conditioned.mean - integer).norm(), 1e-12);
	BOOST_CHECK(zhangReferenceInvariantPairTrace(conditioned.covariance) <
		zhangReferenceInvariantPairTrace(q));
}

BOOST_AUTO_TEST_CASE(component_bridge_gls_aggregates_correlated_edges)
{
	VectorXd edges(3); edges << 4.15, 3.90, 4.05;
	MatrixXd q = MatrixXd::Identity(3, 3) * 0.09;
	const auto bridge = zhangComponentBridgeGls(edges, q);
	BOOST_REQUIRE(bridge.valid);
	BOOST_CHECK_CLOSE(bridge.mean, edges.mean(), 1e-10);
	BOOST_CHECK_CLOSE(bridge.variance, 0.03, 1e-10);
	BOOST_CHECK_EQUAL(bridge.effectiveRank, 3);
	BOOST_CHECK(bridge.residualNis > 0);
}

BOOST_AUTO_TEST_CASE(component_gauge_gls_jointly_recovers_all_component_gauges)
{
	// Three certified components, component zero is the integer datum.  The
	// observations contain redundant correlated edges for c1, c2 and c1-c2.
	MatrixXd design(5, 2);
	design << 1, 0,
		1, 0,
		0, 1,
		0, 1,
		1, -1;
	VectorXd measurements(5);
	measurements << 3.02, 2.98, -1.01, -0.99, 4.01;
	MatrixXd covariance = MatrixXd::Identity(5, 5) * 0.01;
	covariance(0, 1) = covariance(1, 0) = 0.002;
	covariance(2, 3) = covariance(3, 2) = 0.002;
	const auto result = zhangComponentGaugeGls(
		measurements, covariance, design);
	BOOST_REQUIRE(result.valid);
	BOOST_CHECK_EQUAL(result.gaugeRank, 2);
	BOOST_CHECK_EQUAL(result.measurementRank, 5);
	BOOST_REQUIRE_EQUAL(result.mean.size(), 2);
	BOOST_CHECK_SMALL(result.mean(0) - 3, 0.03);
	BOOST_CHECK_SMALL(result.mean(1) + 1, 0.03);
	BOOST_CHECK(result.covariance(0, 0) > 0);
	BOOST_CHECK(result.covariance(1, 1) > 0);
	BOOST_CHECK(result.residualNis >= 0);
}

BOOST_AUTO_TEST_CASE(component_gauge_gls_rejects_unestimable_gauge)
{
	VectorXd measurements(2); measurements << 2, 2;
	MatrixXd covariance = MatrixXd::Identity(2, 2);
	MatrixXd design = MatrixXd::Zero(2, 2);
	design.col(0).setOnes();
	const auto result = zhangComponentGaugeGls(
		measurements, covariance, design);
	BOOST_CHECK(!result.valid);
	BOOST_CHECK_EQUAL(result.gaugeRank, 1);
}

BOOST_AUTO_TEST_CASE(component_gauge_product_row_includes_certified_offsets)
{
	// Component anchors have internal potentials [2,-3,5] relative to their
	// own component gauges.  Fixed row 2*(c1-c0)-(c2-c0)=7 implies
	// 2*K1-K2-K0 = 7 + 2*(-3-2) - (5-2) = -6.
	const auto mapped = zhangComponentGaugeToProductRow(
		{2, -1}, {0, 2, 3}, {2, -3, 5}, 4, 7);
	BOOST_REQUIRE(mapped.valid);
	const ZhangExactVector expected = {-1, 0, 2, -1};
	BOOST_CHECK(mapped.row == expected);
	BOOST_CHECK_EQUAL(mapped.value, -6);
}

BOOST_AUTO_TEST_CASE(component_gauge_product_row_supports_implicit_reference)
{
	// Datum anchor is the implicit canonical reference at index dimension.
	const auto mapped = zhangComponentGaugeToProductRow(
		{1}, {3, 1}, {0, 4}, 3, -2);
	BOOST_REQUIRE(mapped.valid);
	const ZhangExactVector expected = {0, 1, 0};
	BOOST_CHECK(mapped.row == expected);
	BOOST_CHECK_EQUAL(mapped.value, 2);
}

BOOST_AUTO_TEST_CASE(
	temporal_component_integer_basis_removes_common_gauge_and_is_reference_invariant)
{
	const auto g02 = zhangComponentRelativeGaugeBasis(5, 0);
	const auto g03 = zhangComponentRelativeGaugeBasis(5, 1);
	BOOST_REQUIRE_EQUAL(g02.size(), 4);
	BOOST_REQUIRE_EQUAL(g03.size(), 4);
	const ZhangExactVector common(5, 1);
	BOOST_CHECK(zhangExactMatrixTimesColumn(g02, common) ==
		ZhangExactVector(4));
	BOOST_CHECK(zhangExactMatrixTimesColumn(g03, common) ==
		ZhangExactVector(4));
	const auto hnf02 = zhangExactRowHermiteNormalForm(g02);
	const auto hnf03 = zhangExactRowHermiteNormalForm(g03);
	BOOST_REQUIRE(hnf02.consistent);
	BOOST_REQUIRE(hnf03.consistent);
	BOOST_CHECK(hnf02.basis == hnf03.basis);
	const auto primitive = zhangIntegerRowLatticeContains(
		g02, ZhangExactVector(5));
	BOOST_REQUIRE_EQUAL(primitive.smithInvariants.size(), 4);
	for (const auto& invariant : primitive.smithInvariants)
	{
		BOOST_CHECK(zhangExactAbs(invariant) == 1);
	}
}

BOOST_AUTO_TEST_CASE(product_relation_named_backward_search_reaches_low_rank)
{
	const std::vector<int> full = {0, 1, 2, 3, 4, 5};
	const auto withSeed = zhangProductNamedBackwardChildren(
		full, {1, 4}, 5, 1);
	BOOST_REQUIRE_EQUAL(withSeed.size(), 1);
	const std::vector<int> expectedSeed = {1, 4};
	BOOST_CHECK_EQUAL_COLLECTIONS(
		withSeed.front().begin(), withSeed.front().end(),
		expectedSeed.begin(), expectedSeed.end());

	std::vector<int> path = full;
	int evaluations = 1;
	while (path.size() > 1)
	{
		const auto children = zhangProductNamedBackwardChildren(
			path, {}, static_cast<int>(path.size()) - 1, 1);
		BOOST_REQUIRE_EQUAL(children.size(), 1);
		path = children.front();
		evaluations++;
	}
	BOOST_CHECK_EQUAL(path.size(), 1);
	BOOST_CHECK_EQUAL(evaluations, 6);
}

BOOST_AUTO_TEST_CASE(product_named_pair_beam_expands_alternate_forests)
{
	const std::vector<ZhangNamedPairBeamCandidate> candidates = {
		{{1, 0, 0}, 1e-5, 1.0, 0.2, {0, 3}},
		{{0, 1, 0}, 1e-5, 100.0, 0.2, {1, 3}},
		{{0, 0, 1}, 1e-6, 0.1, 0.1, {2, 3}},
		{{1, -1, 0}, 1e-5, 0.5, 0.2, {0, 1}}};
	const auto levels = zhangNamedPairForestBeamLevels(candidates, 3, 2);
	BOOST_REQUIRE_EQUAL(levels.size(), 3);
	BOOST_REQUIRE_EQUAL(levels[0].size(), 2);
	// Candidate 0 is outside the rank-1 beam.  At rank two, candidate 3 joins
	// the retained most-reliable branch and wins on covered satellites before
	// the much larger gain of a lower-coverage alternative.
	BOOST_CHECK(std::find(
		levels[0][0].selected.begin(), levels[0][0].selected.end(), 0) ==
		levels[0][0].selected.end());
	BOOST_CHECK(std::find(
		levels[0][1].selected.begin(), levels[0][1].selected.end(), 0) ==
		levels[0][1].selected.end());
	const std::vector<int> expectedBest = {2, 3};
	BOOST_CHECK_EQUAL_COLLECTIONS(
		levels[1][0].selected.begin(), levels[1][0].selected.end(),
		expectedBest.begin(), expectedBest.end());
	BOOST_CHECK_EQUAL(levels[1][0].coveredNodes, 4);
	BOOST_CHECK_CLOSE_FRACTION(levels[1][0].summedGain, 0.6, 1e-14);
}

BOOST_AUTO_TEST_CASE(product_relation_constraints_pull_back_affine_wl_and_l1)
{
	ZhangProductRelationBasis first;
	first.mappableTargetRank = 2;
	first.transform.resize(2, 3);
	first.transform << 1, -1, 0,
		0, 1, -1;
	first.affineOffsets = {2, -1};
	ZhangProductRelationBasis second;
	second.mappableTargetRank = 2;
	second.transform.resize(2, 3);
	second.transform << 1, 0, -1,
		1, -1, 0;
	second.affineOffsets = {1, 3};

	const ZhangExactMatrix wideLaneRows = {{1, -1}};
	const ZhangExactVector wideLaneIntegers = {7};
	const ZhangExactMatrix firstRows = {{2, 1}};
	const ZhangExactVector firstIntegers = {-4};
	ZhangExactMatrix networkRows;
	ZhangExactVector networkIntegers;
	std::string failure;
	BOOST_REQUIRE(zhangPullBackProductIntegerConstraints(
		first, second,
		wideLaneRows, wideLaneIntegers,
		firstRows, firstIntegers,
		networkRows, networkIntegers, failure));
	BOOST_CHECK_EQUAL(failure, "NONE");
	BOOST_REQUIRE_EQUAL(networkRows.size(), 2);
	BOOST_REQUIRE_EQUAL(networkIntegers.size(), 2);
	BOOST_CHECK(networkRows[0] == ZhangExactVector({1, -3, 2}));
	BOOST_CHECK_EQUAL(networkIntegers[0], 2);
	BOOST_CHECK(networkRows[1] == ZhangExactVector({2, -1, -1}));
	BOOST_CHECK_EQUAL(networkIntegers[1], -7);
}

BOOST_AUTO_TEST_CASE(product_relation_constraint_pullback_rejects_noninteger_basis)
{
	ZhangProductRelationBasis first;
	first.mappableTargetRank = 1;
	first.transform = MatrixXd::Constant(1, 1, 0.5);
	first.affineOffsets = {0};
	ZhangProductRelationBasis second = first;
	second.transform(0, 0) = 0;
	ZhangExactMatrix networkRows;
	ZhangExactVector networkIntegers;
	std::string failure;
	BOOST_CHECK(!zhangPullBackProductIntegerConstraints(
		first, second, {{1}}, {0}, {}, {},
		networkRows, networkIntegers, failure));
	BOOST_CHECK_EQUAL(failure, "WL_PRODUCT_TO_NETWORK_MAPPING_FAILED");
	BOOST_CHECK(networkRows.empty());
}

BOOST_AUTO_TEST_CASE(product_integer_ledger_accumulates_exact_physical_rank)
{
	ProductIntegerLedger ledger;
	ProductIntegerLedgerRow first;
	first.system = E_Sys::GPS;
	first.firstObservable = E_ObsCode::L1C;
	first.secondObservable = E_ObsCode::L2W;
	first.productRow = {1, -1, 0, 0};
	first.integerValue = 7;
	first.physicalExpansion = {{"L1C|ABCD|G03|V4", 1},
		{"L1C|ABCD|G02|V2", -1}};
	first.phaseSegmentFingerprint = "G02|L1C|SEG1;G03|L1C|SEG1;";
	first.backendBasisGeneration = 12;

	auto firstEpoch = ledger.observe(100, {first}, 2);
	BOOST_REQUIRE(firstEpoch.valid);
	BOOST_CHECK_EQUAL(firstEpoch.activeRankAfter, 0);
	BOOST_REQUIRE_EQUAL(ledger.rows().size(), 1);
	BOOST_CHECK(!ledger.rows().front().certified);

	// Re-observing in the same epoch cannot manufacture a confirmation.
	auto duplicateEpoch = ledger.observe(100, {first}, 2);
	BOOST_REQUIRE(duplicateEpoch.valid);
	BOOST_CHECK_EQUAL(duplicateEpoch.activeRankAfter, 0);
	BOOST_CHECK_EQUAL(ledger.rows().front().confirmationEpochs, 1);

	auto confirmed = ledger.observe(130, {first}, 2);
	BOOST_REQUIRE(confirmed.valid);
	BOOST_CHECK_EQUAL(confirmed.activeRankAfter, 1);
	BOOST_CHECK(ledger.rows().front().certified);

	auto second = first;
	second.productRow = {0, 1, -1, 0};
	second.integerValue = -3;
	second.physicalExpansion = {{"L1C|EFGH|G08|V1", 1},
		{"L1C|EFGH|G02|V5", -1}};
	second.phaseSegmentFingerprint = "G02|L1C|SEG1;G08|L1C|SEG1;";
	auto secondFirst = ledger.observe(160, {second}, 2);
	BOOST_REQUIRE(secondFirst.valid);
	BOOST_CHECK_EQUAL(secondFirst.activeRankAfter, 1);
	auto secondConfirmed = ledger.observe(190, {second}, 2);
	BOOST_REQUIRE(secondConfirmed.valid);
	BOOST_CHECK_EQUAL(secondConfirmed.activeRankAfter, 2);
}

BOOST_AUTO_TEST_CASE(product_integer_ledger_rejects_nonprimitive_transactionally)
{
	ProductIntegerLedger ledger;
	ProductIntegerLedgerRow good;
	good.system = E_Sys::GPS;
	good.productRow = {1};
	good.integerValue = 4;
	good.physicalExpansion = {{"L1C|ABCD|G03|V1", 1}};
	good.phaseSegmentFingerprint = "G03|L1C|SEG1;";
	BOOST_REQUIRE(ledger.observe(100, {good}, 1).valid);
	BOOST_REQUIRE_EQUAL(ledger.rows().size(), 1);

	auto bad = good;
	bad.integerValue = 8;
	bad.physicalExpansion = {{"L1C|EFGH|G05|V1", 2}};
	const auto rejected = ledger.observe(130, {good, bad}, 1);
	BOOST_CHECK(!rejected.valid);
	BOOST_CHECK_EQUAL(rejected.failureReason,
		"PRODUCT_LEDGER_ROW_NOT_PRIMITIVE");
	BOOST_REQUIRE_EQUAL(ledger.rows().size(), 1);
	BOOST_CHECK_EQUAL(ledger.rows().front().integerValue, 4);
}

BOOST_AUTO_TEST_CASE(product_integer_ledger_conflict_restarts_confirmation)
{
	ProductIntegerLedger ledger;
	ProductIntegerLedgerRow row;
	row.system = E_Sys::GPS;
	row.firstObservable = E_ObsCode::L1C;
	row.secondObservable = E_ObsCode::L2W;
	row.productRow = {1, -1};
	row.integerValue = 7;
	row.physicalExpansion = {
		{"L1C|ABCD|G03|V1", 1}, {"L1C|ABCD|G02|V1", -1}};
	row.phaseSegmentFingerprint = "G02|L1C|SEG1;G03|L1C|SEG1;";
	row.backendBasisGeneration = 12;
	BOOST_REQUIRE(ledger.observe(100, {row}, 2).valid);
	BOOST_REQUIRE(ledger.observe(130, {row}, 2).valid);
	BOOST_REQUIRE_EQUAL(ledger.rows().size(), 1);
	BOOST_CHECK(ledger.rows().front().certified);

	auto replacement = row;
	replacement.integerValue = 8;
	const auto conflict = ledger.observe(160, {replacement}, 2);
	BOOST_REQUIRE(conflict.valid);
	BOOST_CHECK_EQUAL(conflict.conflictingRows, 1);
	BOOST_REQUIRE_EQUAL(ledger.rows().size(), 1);
	BOOST_CHECK_EQUAL(ledger.rows().front().integerValue, -8);
	BOOST_CHECK_EQUAL(ledger.rows().front().confirmationEpochs, 1);
	BOOST_CHECK(!ledger.rows().front().certified);

	const auto reconfirmed = ledger.observe(190, {replacement}, 2);
	BOOST_REQUIRE(reconfirmed.valid);
	BOOST_CHECK_EQUAL(reconfirmed.conflictingRows, 0);
	BOOST_CHECK(ledger.rows().front().certified);
	BOOST_CHECK_EQUAL(ledger.rows().front().confirmationEpochs, 2);
}

BOOST_AUTO_TEST_CASE(product_integer_ledger_canonicalises_negated_relation)
{
	ProductIntegerLedger ledger;
	ProductIntegerLedgerRow row;
	row.system = E_Sys::GPS;
	row.firstObservable = E_ObsCode::L1C;
	row.secondObservable = E_ObsCode::L2W;
	row.productRow = {1, -1};
	row.integerValue = 7;
	row.physicalExpansion = {
		{"L1C|ABCD|G02|V1", -1}, {"L1C|ABCD|G03|V1", 1}};
	row.phaseSegmentFingerprint = "segments-v1";
	row.backendBasisGeneration = 12;
	row.pairCertificate = true;
	row.conditioningOnly = false;
	row.coordinate = "WL";
	row.firstSatellite = "G03";
	row.secondSatellite = "G02";
	BOOST_REQUIRE(ledger.observe(100, {row}, 2).valid);

	auto negated = row;
	for (auto& coefficient : negated.productRow) coefficient = -coefficient;
	for (auto& [identity, coefficient] : negated.physicalExpansion)
		coefficient = -coefficient;
	negated.integerValue = -negated.integerValue;
	std::swap(negated.firstSatellite, negated.secondSatellite);
	const auto update = ledger.observe(130, {negated}, 2);
	BOOST_REQUIRE(update.valid);
	BOOST_CHECK_EQUAL(update.freshRows, 0);
	BOOST_CHECK_EQUAL(update.conflictingRows, 0);
	BOOST_REQUIRE_EQUAL(ledger.rows().size(), 1);
	BOOST_CHECK(ledger.rows().front().certified);
	BOOST_CHECK_EQUAL(ledger.rows().front().confirmationEpochs, 2);
	BOOST_CHECK(
		ledger.rows().front().physicalExpansion.begin()->second > 0);
}

BOOST_AUTO_TEST_CASE(product_ledger_physical_fingerprint_is_row_local)
{
	const std::map<std::string, ZhangExactInteger> row = {
		{"L2W|EFGH|G02|V4", -1},
		{"L1C|ABCD|G03|V1", 1},
		{"L1C|IJKL|G03|V9", -2}};
	const auto fingerprint = zhangProductPhysicalRowFingerprint(row);
	BOOST_CHECK(!fingerprint.empty());
	BOOST_CHECK(fingerprint.find("L2W|EFGH|G02|V4=-1") != std::string::npos);
	BOOST_CHECK(fingerprint.find("L1C|ABCD|G03|V1=1") != std::string::npos);
	BOOST_CHECK(fingerprint.find("G04") == std::string::npos);
	BOOST_CHECK_EQUAL(std::count(
		fingerprint.begin(), fingerprint.end(), ';'), 3);
}

BOOST_AUTO_TEST_CASE(product_integer_ledger_upgrades_conditioner_to_pair_certificate)
{
	ProductIntegerLedger ledger;
	ProductIntegerLedgerRow conditioner;
	conditioner.system = E_Sys::GPS;
	conditioner.firstObservable = E_ObsCode::L1C;
	conditioner.secondObservable = E_ObsCode::L2W;
	conditioner.productRow = {1, 0, -1, 0};
	conditioner.integerValue = 8;
	conditioner.physicalExpansion = {{"L1C|A|G02|V0", 1},
		{"L1C|A|G03|V0", -1}};
	conditioner.phaseSegmentFingerprint = "segments-v1";
	const auto first = ledger.observe(100, {conditioner}, 2);
	BOOST_REQUIRE(first.valid);
	BOOST_REQUIRE_EQUAL(ledger.rows().size(), 1);
	BOOST_CHECK(ledger.rows().front().conditioningOnly);
	BOOST_CHECK(!ledger.rows().front().pairCertificate);

	auto pair = conditioner;
	pair.source = ZhangProductIntegerLedgerSource::DERIVED_PAIR;
	pair.conditioningOnly = false;
	pair.pairCertificate = true;
	pair.coordinate = "WL";
	pair.firstSatellite = "G02";
	pair.secondSatellite = "G03";
	const auto second = ledger.observe(130, {pair}, 2);
	BOOST_REQUIRE(second.valid);
	BOOST_REQUIRE_EQUAL(ledger.rows().size(), 1);
	BOOST_CHECK(ledger.rows().front().certified);
	BOOST_CHECK(ledger.rows().front().pairCertificate);
	BOOST_CHECK(!ledger.rows().front().conditioningOnly);
	BOOST_CHECK_EQUAL(zhangProductIntegerLedgerSourceName(
		ledger.rows().front().source), "DERIVED_PAIR");
	BOOST_CHECK_EQUAL(ledger.rows().front().coordinate, "WL");
	BOOST_CHECK_EQUAL(ledger.rows().front().firstSatellite, "G02");
	BOOST_CHECK_EQUAL(ledger.rows().front().secondSatellite, "G03");
}

BOOST_AUTO_TEST_CASE(product_integer_ledger_isolates_backend_generations)
{
	ProductIntegerLedger ledger;
	ProductIntegerLedgerRow row;
	row.system = E_Sys::GPS;
	row.firstObservable = E_ObsCode::L1C;
	row.secondObservable = E_ObsCode::L2W;
	row.productRow = {1, -1};
	row.integerValue = 7;
	row.physicalExpansion = {
		{"L1C|ABCD|G03|V4", 1}, {"L1C|ABCD|G02|V2", -1}};
	row.phaseSegmentFingerprint = "G02|L1C|SEG1;G03|L1C|SEG1;";
	row.backendBasisGeneration = 12;
	BOOST_REQUIRE(ledger.observe(100, {row}, 1).valid);

	auto nextGeneration = row;
	nextGeneration.backendBasisGeneration = 13;
	nextGeneration.integerValue = -112;
	const auto update = ledger.observe(130, {nextGeneration}, 1);
	BOOST_REQUIRE(update.valid);
	BOOST_CHECK_EQUAL(update.freshRows, 1);
	BOOST_CHECK_EQUAL(update.conflictingRows, 0);

	auto nextSegment = nextGeneration;
	nextSegment.phaseSegmentFingerprint =
		"G02|L1C|SEG2;G03|L1C|SEG2;";
	nextSegment.integerValue = 32;
	const auto segmentUpdate = ledger.observe(160, {nextSegment}, 1);
	BOOST_REQUIRE(segmentUpdate.valid);
	BOOST_CHECK_EQUAL(segmentUpdate.freshRows, 1);
	BOOST_CHECK_EQUAL(segmentUpdate.conflictingRows, 0);
	BOOST_REQUIRE_EQUAL(ledger.rows().size(), 3);

	const auto generation12 = ledger.rowsForGeneration(12);
	const auto generation13 = ledger.rowsForGeneration(13);
	BOOST_REQUIRE_EQUAL(generation12.size(), 1);
	BOOST_REQUIRE_EQUAL(generation13.size(), 2);
	BOOST_CHECK_EQUAL(generation12.front().integerValue, -7);
	BOOST_CHECK_EQUAL(generation13[0].integerValue, 112);
	BOOST_CHECK_EQUAL(generation13[1].integerValue, -32);

	// A generation change alone does not invalidate an exact physical row.
	// It may be re-expressed only when every arc/version identity is present.
	ZhangExactVector projected;
	const std::map<std::string, int> currentColumns = {
		{"L1C|ABCD|G02|V2", 0}, {"L1C|ABCD|G03|V4", 1}};
	BOOST_REQUIRE(zhangProjectProductLedgerPhysicalRow(
		generation12.front(), currentColumns, 2, projected));
	BOOST_REQUIRE_EQUAL(projected.size(), 2);
	BOOST_CHECK_EQUAL(projected[0], 1);
	BOOST_CHECK_EQUAL(projected[1], -1);
	const std::map<std::string, int> missingArc = {
		{"L1C|ABCD|G03|V4", 0}};
	BOOST_CHECK(!zhangProjectProductLedgerPhysicalRow(
		generation12.front(), missingArc, 1, projected));
}

BOOST_AUTO_TEST_CASE(product_lattice_failure_probability_respects_decimal_budget)
{
	const double configuredBudget = 1e-3;
	const double configuredSuccess = 0.999;
	const double bound = zhangProductFailureProbabilityBound(
		configuredSuccess, configuredBudget);
	BOOST_CHECK_LE(bound, configuredBudget);
	BOOST_CHECK(zhangProductFailureProbabilityPassed(
		bound, configuredBudget));

	const double firstStage = zhangProductFailureProbabilityBound(
		0.9996, configuredBudget);
	const double remaining = configuredBudget - firstStage;
	const double secondStage = zhangProductFailureProbabilityBound(
		1 - remaining, remaining);
	BOOST_CHECK(zhangProductFailureProbabilityPassed(
		firstStage + secondStage, configuredBudget));

	const double unsafe = zhangProductFailureProbabilityBound(
		0.9, configuredBudget);
	BOOST_CHECK_CLOSE_FRACTION(unsafe, 0.1, 1e-14);
	BOOST_CHECK(!zhangProductFailureProbabilityPassed(
		unsafe, configuredBudget));
	BOOST_CHECK_EQUAL(
		zhangProductFailureProbabilityBound(1.01, configuredBudget), 1);
}

BOOST_AUTO_TEST_CASE(product_candidate_pair_rank_requires_exact_named_edge)
{
	BOOST_CHECK(zhangProductCandidateIsNamedPairRow({1, 0, 0}));
	BOOST_CHECK(zhangProductCandidateIsNamedPairRow({1, -1, 0}));
	BOOST_CHECK(zhangProductCandidateIsNamedPairRow({-1, 0, 0}));
	BOOST_CHECK(!zhangProductCandidateIsNamedPairRow({2, -1, 0}));
	BOOST_CHECK(!zhangProductCandidateIsNamedPairRow({1, 1, 0}));
	BOOST_CHECK(!zhangProductCandidateIsNamedPairRow({1, -1, 1}));
}

BOOST_AUTO_TEST_CASE(lambda_reports_selected_suffix_bootstrap_success)
{
	VectorXd conditionalVariances(3);
	conditionalVariances << 4, 0.04, 0.01;
	const double expected =
		std::erf(std::sqrt(1 / (8 * 0.04))) *
		std::erf(std::sqrt(1 / (8 * 0.01)));
	const double selected = lambdaSelectedSuffixBootstrapSuccess(
		conditionalVariances, 2);
	BOOST_CHECK_CLOSE_FRACTION(
		selected, expected, 1e-14);
	BOOST_CHECK_GT(selected,
		lambdaSelectedSuffixBootstrapSuccess(conditionalVariances, 3));
	BOOST_CHECK_EQUAL(
		lambdaSelectedSuffixBootstrapSuccess(conditionalVariances, 0), 0);
}

BOOST_AUTO_TEST_CASE(full_product_lattice_oracle_parser_requires_admissible_complete_rank)
{
	const std::string valid = R"json({
		"status":"FULL_ORACLE_READY",
		"hard_gate_passed":true,
		"oracle":{
			"schema":"ZHANG_FULL_PRODUCT_LATTICE_ORACLE_V1",
			"system":"GPS",
			"reference_satellite":"G02",
			"satellites":["G02","G03","G04"],
			"dual_frequency_rank":2,
			"relations":[
				{"satellite":"G03","reference":"G02",
				 "wl_satellite_minus_reference":3,
				 "l1_satellite_minus_reference":8,
				 "l2_satellite_minus_reference":5},
				{"satellite":"G04","reference":"G02",
				 "wl_satellite_minus_reference":1,
				 "l1_satellite_minus_reference":12,
				 "l2_satellite_minus_reference":11}
			]
		}
	})json";
	const auto oracle = parseZhangFullProductLatticeOracle(valid, 2);
	BOOST_REQUIRE(oracle.valid);
	BOOST_CHECK_EQUAL(oracle.rank, 2);
	BOOST_CHECK_EQUAL(oracle.referenceSatellite, "G02");
	BOOST_REQUIRE_EQUAL(oracle.potentials.size(), 3);
	BOOST_CHECK(oracle.potentials.at("G03").wideLane == 3);
	BOOST_CHECK(oracle.potentials.at("G04").secondSignal == 11);

	std::string inadmissible = valid;
	const auto position = inadmissible.find(
		"\"l2_satellite_minus_reference\":5");
	BOOST_REQUIRE(position != std::string::npos);
	inadmissible.replace(position,
		std::string("\"l2_satellite_minus_reference\":5").size(),
		"\"l2_satellite_minus_reference\":6");
	const auto rejected = parseZhangFullProductLatticeOracle(inadmissible, 2);
	BOOST_CHECK(!rejected.valid);
	BOOST_CHECK_EQUAL(rejected.failureReason,
		"ORACLE_WL_L1_L2_NOT_ADMISSIBLE");
	const auto incomplete = parseZhangFullProductLatticeOracle(valid);
	BOOST_CHECK(!incomplete.valid);
	BOOST_CHECK_EQUAL(incomplete.failureReason,
		"ORACLE_EXPECTED_FULL_RANK_MISMATCH");
}

BOOST_AUTO_TEST_CASE(integer_support_quality_fails_closed_without_residuals)
{
	ZhangIntegerArcQuality quality;
	quality.ageEpochs = 100;
	quality.observations = 100;
	const auto missing = zhangEvaluateIntegerSupportQuality(
		quality, ZhangIntegerSupportQualityGates{});
	BOOST_CHECK(!missing.eligibleForIntegerSupport);
	BOOST_CHECK_EQUAL(missing.failureReason, "PHASE_RMS_GATE_FAILED");

	quality.phaseResidualRms = 0.01;
	quality.codeResidualRms = 1;
	quality.phaseResidualMad = 0.01;
	quality.codeResidualMad = 1;
	quality.elevationScore = 0.5;
	quality.whitenedResidualScore = 1;
	const auto accepted = zhangEvaluateIntegerSupportQuality(
		quality, ZhangIntegerSupportQualityGates{});
	BOOST_CHECK(accepted.eligibleForIntegerSupport);
	BOOST_CHECK_EQUAL(accepted.failureReason, "ELIGIBLE");
}

BOOST_AUTO_TEST_CASE(exact_held_quotient_separates_certified_and_unresolved_rank)
{
	// Target is the full four-dimensional integer lattice.  Held evidence
	// certifies two primitive target directions plus one unrelated ambient row.
	const ZhangExactMatrix target = {
		{1, 0, 0, 0, 0},
		{0, 1, 0, 0, 0},
		{0, 0, 1, 0, 0},
		{0, 0, 0, 1, 0}};
	const ZhangExactMatrix held = {
		{1, 0, 0, 0, 0},
		{0, 1, 0, 0, 0},
		{0, 0, 0, 0, 1}};
	const ZhangExactVector values = {7, -3, 99};

	const auto audit = zhangExactHeldQuotientAudit(target, held, values);
	BOOST_REQUIRE(audit.valid);
	BOOST_CHECK_EQUAL(audit.targetRank, 4);
	BOOST_CHECK_EQUAL(audit.heldIntersectionRank, 2);
	BOOST_CHECK_EQUAL(audit.quotientRank, 2);
	BOOST_CHECK(audit.heldIntersectionPrimitiveInTarget);
	BOOST_CHECK(audit.exactClosure);
	BOOST_REQUIRE_EQUAL(audit.heldIntersectionValues.size(), 2);
	BOOST_CHECK_EQUAL(audit.heldIntersectionValues[0], 7);
	BOOST_CHECK_EQUAL(audit.heldIntersectionValues[1], -3);
}

BOOST_AUTO_TEST_CASE(exact_held_quotient_rejects_nonprimitive_intersection)
{
	const ZhangExactMatrix target = {{1, 0}, {0, 1}};
	const ZhangExactMatrix held = {{2, 0}};
	const auto audit = zhangExactHeldQuotientAudit(target, held, {4});
	BOOST_CHECK(!audit.valid);
	BOOST_CHECK_EQUAL(
		audit.failureReason, "HELD_INTERSECTION_NOT_PRIMITIVE_IN_TARGET");
}

BOOST_AUTO_TEST_CASE(exact_certified_union_recomputes_rank_and_target_equality)
{
	const ZhangExactMatrix target = {
		{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
	const ZhangExactMatrix held = {{1, 0, 0}};
	const ZhangExactVector heldValues = {5};
	const ZhangExactMatrix partialFixed = {{0, 1, 0}};
	const ZhangExactVector partialValues = {-2};
	const auto partial = zhangExactCertifiedUnionAudit(
		target, held, heldValues, partialFixed, partialValues);
	BOOST_CHECK(partial.consistent);
	BOOST_CHECK_EQUAL(partial.targetRank, 3);
	BOOST_CHECK_EQUAL(partial.heldRank, 1);
	BOOST_CHECK_EQUAL(partial.newlyFixedRank, 1);
	BOOST_CHECK_EQUAL(partial.combinedCertifiedRank, 2);
	BOOST_CHECK(!partial.exactTargetEquality);
	BOOST_CHECK_EQUAL(
		partial.failureReason, "TARGET_LATTICE_NOT_FULLY_CERTIFIED");

	const ZhangExactMatrix completeFixed = {{0, 1, 0}, {0, 0, 1}};
	const ZhangExactVector completeValues = {-2, 8};
	const auto complete = zhangExactCertifiedUnionAudit(
		target, held, heldValues, completeFixed, completeValues);
	BOOST_CHECK(complete.consistent);
	BOOST_CHECK_EQUAL(complete.combinedCertifiedRank, 3);
	BOOST_CHECK(complete.exactTargetEquality);
}

BOOST_AUTO_TEST_CASE(deterministic_quotient_distinguishes_integer_inconsistency)
{
	VectorXd integerMean(2); integerMean << 4, 1.25;
	MatrixXd covariance = MatrixXd::Zero(2, 2);
	covariance(1, 1) = 0.04;
	const auto consistent = zhangAuditDeterministicQuotientModes(
		integerMean, covariance);
	BOOST_REQUIRE(consistent.covarianceValid);
	BOOST_CHECK_EQUAL(consistent.covarianceRank, 1);
	BOOST_CHECK_EQUAL(consistent.nullity, 1);
	BOOST_CHECK(consistent.integerConsistent);
	BOOST_CHECK_EQUAL(consistent.status, "UNTRACKED_DETERMINISTIC_RELATION");

	VectorXd fractionalMean(2); fractionalMean << 4.25, 1.25;
	const auto inconsistent = zhangAuditDeterministicQuotientModes(
		fractionalMean, covariance);
	BOOST_REQUIRE(inconsistent.covarianceValid);
	BOOST_CHECK(!inconsistent.integerConsistent);
	BOOST_CHECK_EQUAL(
		inconsistent.status, "DETERMINISTIC_INTEGER_INCONSISTENCY");
}

BOOST_AUTO_TEST_CASE(integer_gain_frontier_is_reliability_first_and_exact_at_rank_one)
{
	VectorXd mean(2); mean << 3.01, -1.02;
	MatrixXd covariance = MatrixXd::Identity(2, 2) * 1e-4;
	MatrixXd products = MatrixXd::Identity(2, 2);
	MatrixXd productCross = products * covariance;
	const auto frontier = zhangBoundedIntegerProductGainFrontier(
		mean, covariance, productCross, 1, 1e-3, 1e-6, 2, 64,
		(products * covariance * products.transpose()).trace());
	BOOST_REQUIRE(frontier.valid);
	BOOST_CHECK_EQUAL(frontier.status, "COMPLETE");
	BOOST_CHECK(frontier.enumeratedPrimitiveRows > 0);
	BOOST_CHECK(frontier.reliablePrimitiveRows > 0);
	BOOST_REQUIRE(!frontier.points.empty());
	BOOST_CHECK_EQUAL(frontier.points.front().rank, 1);
	BOOST_CHECK(frontier.points.front().reliable);
	BOOST_CHECK(frontier.points.front().exactBoundedOptimum);
	BOOST_CHECK(frontier.points.front().failureProbabilityBound <= 1e-3);
}

BOOST_AUTO_TEST_CASE(integer_gain_frontier_rejects_fractional_low_variance_rows)
{
	VectorXd mean(1); mean << 0.25;
	MatrixXd covariance(1, 1); covariance << 1e-8;
	MatrixXd products(1, 1); products << 1;
	MatrixXd productCross = products * covariance;
	const auto frontier = zhangBoundedIntegerProductGainFrontier(
		mean, covariance, productCross, 2, 1e-3, 1e-6, 1, 128,
		(products * covariance * products.transpose()).trace());
	BOOST_REQUIRE(frontier.valid);
	BOOST_CHECK_EQUAL(frontier.status, "NO_RELIABLE_PRIMITIVE_ROW");
	BOOST_CHECK_EQUAL(frontier.reliablePrimitiveRows, 0);
	BOOST_CHECK(frontier.points.empty());
}

BOOST_AUTO_TEST_CASE(integer_gain_frontier_keeps_reliable_explicit_seed_beyond_bound)
{
	VectorXd mean(2); mean << 0.5, 0;
	MatrixXd covariance = MatrixXd::Identity(2, 2) * 1e-8;
	MatrixXd productCross(1, 2); productCross << 1e-8, 0;
	ZhangExactMatrix seeds = {{2, 1}};
	const auto frontier = zhangBoundedIntegerProductGainFrontier(
		mean, covariance, productCross, 1, 1e-3, 1e-6, 1, 64,
		1e-8, seeds);
	BOOST_REQUIRE(frontier.valid);
	BOOST_CHECK_EQUAL(frontier.explicitSeedRowsAdded, 1);
	BOOST_CHECK_EQUAL(frontier.reliableExplicitSeedRows, 1);
	BOOST_REQUIRE(!frontier.points.empty());
	BOOST_REQUIRE_EQUAL(frontier.points.front().rows.size(), 1);
	BOOST_CHECK_EQUAL(frontier.points.front().rows.front().at(0), 2);
	BOOST_CHECK_EQUAL(frontier.points.front().rows.front().at(1), 1);
}

BOOST_AUTO_TEST_CASE(integer_gain_frontier_sparse_support_reaches_high_dimension)
{
	VectorXd mean = VectorXd::Zero(12);
	MatrixXd covariance = MatrixXd::Identity(12, 12) * 1e-4;
	MatrixXd productCross = MatrixXd::Identity(12, 12) * 1e-4;
	const auto frontier = zhangBoundedIntegerProductGainFrontier(
		mean, covariance, productCross, 2, 1e-3, 1e-6, 2, 64,
		12e-4, {}, 2);
	BOOST_REQUIRE(frontier.valid);
	BOOST_CHECK_EQUAL(frontier.dimension, 12);
	BOOST_CHECK_EQUAL(frontier.maximumEnumerationSupport, 2);
	BOOST_CHECK(frontier.enumeratedPrimitiveRows > 0);
	BOOST_CHECK(frontier.enumeratedPrimitiveRows < 10000);
	BOOST_REQUIRE(!frontier.points.empty());
	BOOST_CHECK_EQUAL(frontier.points.front().rank, 1);
}

BOOST_AUTO_TEST_CASE(product_candidate_generator_adds_dense_real_mode_integer_rows)
{
	constexpr int dimension = 6;
	VectorXd mean = VectorXd::Zero(dimension);
	MatrixXd covariance = MatrixXd::Identity(dimension, dimension) * 1e-4;
	MatrixXd productCross(1, dimension);
	productCross.setOnes();
	productCross *= 1e-2;
	const auto generated = generateProductIntegerCandidates(
		mean, covariance, productCross, 1e-3, 1e-6, {}, 1, 8, 256);
	BOOST_REQUIRE(generated.valid);
	BOOST_CHECK_EQUAL(generated.failureReason, "NONE");
	BOOST_CHECK(generated.realModeApproximations > 0);
	bool foundDense = false;
	for (const auto& candidate : generated.candidates)
	{
		if (candidate.source != "PRODUCT_GAIN_REAL_MODE_APPROXIMATION") continue;
		int support = 0;
		for (const auto& coefficient : candidate.row)
			support += coefficient != 0;
		if (support > 2 && candidate.reliabilityPassed)
		{
			foundDense = true;
			BOOST_CHECK(candidate.variance > 0);
			BOOST_CHECK(candidate.perr <= 1e-3);
			BOOST_CHECK(candidate.incrementalProductGain > 0);
			break;
		}
	}
	BOOST_CHECK_MESSAGE(foundDense,
		"real product-gain modes must generate legal dense primitive rows");
}

BOOST_AUTO_TEST_CASE(product_candidate_generator_orders_reliability_then_graph_then_gain)
{
	VectorXd mean(3);
	mean << 0, 0, 0.25;
	MatrixXd covariance = MatrixXd::Identity(3, 3) * 1e-6;
	MatrixXd productCross(1, 3);
	productCross << 0.01, 0.01, 100;
	const auto generated = generateProductIntegerCandidates(
		mean, covariance, productCross, 1e-3, 1e-6, {}, 0, 1, 128);
	BOOST_REQUIRE(generated.valid);
	BOOST_REQUIRE(!generated.candidates.empty());
	BOOST_CHECK(generated.candidates.front().reliabilityPassed);
	BOOST_CHECK_EQUAL(generated.candidates.front().pairGraphRankGain, 1);
	bool reachedUnreliable = false;
	for (const auto& candidate : generated.candidates)
	{
		if (!candidate.reliabilityPassed) reachedUnreliable = true;
		else BOOST_CHECK(!reachedUnreliable);
	}
}
