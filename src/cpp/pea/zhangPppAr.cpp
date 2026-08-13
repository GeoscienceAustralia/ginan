#include "pea/zhangPppAr.hpp"

#include <algorithm>
#include <array>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <limits>
#include <map>
#include <mutex>
#include <numeric>
#include <set>
#include <sstream>
#include <tuple>
#include <vector>
#include <cctype>
#include <cstdint>
#include <stdexcept>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/log/trivial.hpp>
#include <boost/serialization/deque.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/set.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/serialization/vector.hpp>
#include "ambres/GNSSambres.hpp"
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/constants.hpp"
#include "common/observations.hpp"
#include "common/phaseClockOsb.hpp"
#include "common/receiver.hpp"
#include "common/satStat.hpp"
#include "common/trace.hpp"
#include "common/zhangPhaseContinuity.hpp"
#include "common/zhangProductRelationAdmission.hpp"
#include "common/zhangCheckpoint.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "common/zhangIntegerCandidateNis.hpp"
#include "common/zhangIfUser.hpp"
#include "common/zhangIfWideLane.hpp"
#include "common/zhangHybridService.hpp"
#include "common/zhangHybridUserModel.hpp"
#include "common/zhangSatelliteDatum.hpp"
#include "common/zhangTargetedBesdTracker.hpp"
#define ZHANG_FACTOR_CAPTURE_CHECKPOINT_IMPLEMENTATION
#include "common/zhangFactorCapture.hpp"
#undef ZHANG_FACTOR_CAPTURE_CHECKPOINT_IMPLEMENTATION
#include "common/zhangIntegerTargets.hpp"
#include "common/zhangUserTarget.hpp"
#include "common/zhangUserIntegerFunctional.hpp"
#include "orbprop/coordinates.hpp"
#include "pea/zhangReference.hpp"
#include "rtklib/lambda.h"

using std::map;
using std::set;
using std::string;
using std::tuple;
using std::vector;

namespace boost::serialization
{
template<class Archive>
void serialize(Archive& ar, ZhangCapturedStateKey& value, const unsigned int)
{
	ar & value.type;
	ar & value.satellite;
	ar & value.receiver;
	ar & value.number;
}

template<class Archive>
void serialize(Archive& ar, SparseMatrix<double>& value, const unsigned int)
{
	int rows = value.rows();
	int columns = value.cols();
	std::vector<int> rowIndices;
	std::vector<int> columnIndices;
	std::vector<double> coefficients;
	if constexpr (Archive::is_saving::value)
	{
		rowIndices.reserve(value.nonZeros());
		columnIndices.reserve(value.nonZeros());
		coefficients.reserve(value.nonZeros());
		for (int outer = 0; outer < value.outerSize(); outer++)
		for (SparseMatrix<double>::InnerIterator entry(value, outer);
			 entry; ++entry)
		{
			rowIndices.push_back(entry.row());
			columnIndices.push_back(entry.col());
			coefficients.push_back(entry.value());
		}
	}
	ar & rows;
	ar & columns;
	ar & rowIndices;
	ar & columnIndices;
	ar & coefficients;
	if constexpr (Archive::is_loading::value)
	{
		if (rows < 0 || columns < 0 || rows > 1000000 || columns > 1000000
		 || rowIndices.size() != columnIndices.size()
		 || rowIndices.size() != coefficients.size()
		 || rowIndices.size() > 100000000)
		{
			throw std::runtime_error("INVALID_ZHANG_SPARSE_MATRIX_ARCHIVE");
		}
		std::vector<Triplet<double>> triplets;
		triplets.reserve(coefficients.size());
		for (std::size_t index = 0; index < coefficients.size(); index++)
		{
			if (rowIndices[index] < 0 || rowIndices[index] >= rows
			 || columnIndices[index] < 0 || columnIndices[index] >= columns
			 || !std::isfinite(coefficients[index]))
			{
				throw std::runtime_error("INVALID_ZHANG_SPARSE_MATRIX_ENTRY");
			}
			triplets.emplace_back(
				rowIndices[index], columnIndices[index], coefficients[index]);
		}
		value.resize(rows, columns);
		value.setFromTriplets(triplets.begin(), triplets.end());
	}
}

template<class Archive>
void serialize(
	Archive& ar, ZhangCapturedPhysicalArcVersion& value, const unsigned int)
{
	ar & value.arc;
	ar & value.version;
}

template<class Archive>
void serialize(Archive& ar, ZhangInnovationScaleGroup& value,
	const unsigned int)
{
	ar & value.identity;
	ar & value.blocks;
	ar & value.samples;
	ar & value.marginalStandardisedSquaredSum;
	ar & value.maximumAbsoluteRatio;
}

template<class Archive>
void serialize(Archive& ar, ZhangCapturedSnapshotOperation& value,
	const unsigned int)
{
	ar & value.kind;
	ar & value.afterEventSequence;
	ar & value.operationSequence;
	ar & value.identities;
	ar & value.physicalVersions;
	ar & value.rows;
	ar & value.offsets;
}

template<class Archive>
void serialize(Archive& ar, ZhangCanonicalSatelliteRelation& value,
	const unsigned int)
{
	ar & value.anchor;
	ar & value.satellite;
}

template<class Archive>
void serialize(Archive& ar, ZhangPersistentProductDatumCheckpointState& value,
	const unsigned int)
{
	ar & value.system;
	ar & value.observable;
	ar & value.relation;
	ar & value.initialised;
	ar & value.version;
	ar & value.anchorPhaseSegment;
	ar & value.satellitePhaseSegment;
	ar & value.anchorDatumVersion;
	ar & value.satelliteDatumVersion;
}

template<class Archive>
void serialize(Archive& ar, ZhangPersistentProductDatumCheckpoint& value,
	const unsigned int)
{
	ar & value.canonicalRelations;
	ar & value.datumStates;
}

template<class Archive>
void serialize(Archive& ar, ZhangSatellitePhaseSegment& value,
	const unsigned int)
{
	ar & value.satellite;
	ar & value.segment;
}

template<class Archive>
void serialize(Archive& ar, ZhangSatelliteDatumRelation& value,
	const unsigned int)
{
	ar & value.a;
	ar & value.b;
	ar & value.difference;
	ar & value.promoted;
	ar & value.provenance;
}

template<class Archive>
void serialize(Archive& ar, ZhangSatelliteDatumManagerCheckpoint& value,
	const unsigned int)
{
	ar & value.system;
	ar & value.observable;
	ar & value.currentSegments;
	ar & value.discontinuityCounters;
	ar & value.datumVersions;
	ar & value.alignmentCycles;
	ar & value.alignmentKnown;
	ar & value.precisionValid;
	ar & value.relations;
	ar & value.redundantRelations;
	ar & value.eventCounts;
	ar & value.conflictCount;
	ar & value.topologyVersion;
	ar & value.alignmentGeneration;
}

template<class Archive>
void serialize(Archive& ar, ZhangHybridRealGaugeCheckpoint& value,
	const unsigned int)
{
	ar & value.initialized;
	ar & value.generation;
	ar & value.previousValues;
	ar & value.previousSegments;
}

template<class Archive>
void serialize(Archive& ar, ZhangProductDatumVersionTracker& value,
	const unsigned int)
{
	ar & value.initialized;
	ar & value.version;
}

} // namespace boost::serialization

namespace
{
struct ProductKey
{
    SatSys    satellite;
    E_ObsCode observable = E_ObsCode::NONE;

    bool operator<(const ProductKey& other) const
    {
        return std::tie(satellite, observable) <
               std::tie(other.satellite, other.observable);
    }

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & satellite;
		ar & observable;
	}
};

struct GlobalContinuityState
{
    int    counter = 0;
    int    datumVersion = 0;
    GTime  validFrom;
    int    iod = 0;
    string resetReason = "initial";
    int    stabilizationRemaining = 0;
};

map<ProductKey, ZhangPhaseContinuityState> continuityMap;
map<E_Sys, ZhangProductDatumVersionTracker> houProductDatumVersionTrackers;
map<ProductKey, string> houProductPhysicalFunctionalIdentities;
map<ProductKey, ZhangProductIntegerFunctional> houProductPhysicalFunctionals;
map<ProductKey, string> houProductSBasisFingerprints;
map<ProductKey, string> houProductPhaseSegmentIdentities;
map<ProductKey, long long> houProductTreeAlignmentCycles;
map<ProductKey, string> houProductSnapshotIdentities;
map<string, vector<ZhangPendingProductTransition>>
	pendingProductTransitions;

struct ZhangPendingSnapshotPins
{
	GTime       eventTime;
	set<string> identities;
};

// A pre-reset candidate snapshot is bound before writeZhangInternalProducts()
// discovers the corresponding immutable transition.  The normal lifecycle
// pass runs between those two operations, so reference counts alone cannot
// protect the new endpoint yet.  Pin it for that same-epoch transaction; the
// pin is released once the discovered transition takes ownership.
map<string, ZhangPendingSnapshotPins> pendingSnapshotPins;
map<std::pair<E_Sys, E_ObsCode>, GlobalContinuityState> globalContinuityMap;
map<std::pair<E_Sys, E_ObsCode>, ZhangSatelliteDatumManager>
    satelliteDatumManagers;
map<tuple<string, E_Sys, string, E_ObsCode>,
	ZhangHybridRealGaugeTransport> hybridRealGaugeTransports;
map<string, ZhangFactorCaptureBuffer> e18FactorCaptureBuffers;

struct ZhangTargetedBesdPair
{
	SatSys satellite;
	E_ObsCode observable = E_ObsCode::NONE;
	int oldTarget = -1;
	int newTarget = -1;
	ZhangProductIntegerTransition transition;
	string oldSnapshotIdentity;
	string newSnapshotIdentity;
};

struct ZhangTargetedBesdRuntime
{
	GTime eventTime;
	E_Sys system = E_Sys::NONE;
	int ageEpochs = 0;
	bool minimumLagReported = false;
	ZhangTargetedBesdTracker tracker;
	vector<ZhangTargetedBesdPair> pairs;
};

map<string, vector<ZhangTargetedBesdRuntime>> targetedBesdRuntimes;
set<string> e18ConfiguredFactorCaptureStates;
// Process-local reverse binding only.  Runtime state is owned by the stable
// string key above; this address guard detects copied KFState objects that
// accidentally retain the same ID.  It is cleared/rebound on restore and is
// deliberately absent from every checkpoint DTO.
map<string, const KFState*> e18RuntimeObjectBindings;
map<string, ZhangPersistentProductDatumRegistry>
    e18PersistentProductDatumRegistries;

string temporalProductSnapshotIdentity(
	E_Sys system,
	const SatSys& satellite,
	E_ObsCode observable,
	const string& physicalFunctionalIdentity,
	const string& phaseSegmentIdentity)
{
	return "TEMPORAL_PRODUCT:" + enum_to_string(system) + ":" +
		satellite.id() + ":" + enum_to_string(observable) + ":" +
		physicalFunctionalIdentity + ":phase=" + phaseSegmentIdentity;
}

struct E27RawNoiseRowKey
{
    string runtimeId;
    string epoch;
    string receiver;
    E_Sys system = E_Sys::NONE;
    SatSys satellite;

    bool operator<(const E27RawNoiseRowKey& other) const
    {
        return std::tie(runtimeId, epoch, receiver, system, satellite) <
            std::tie(
                other.runtimeId, other.epoch, other.receiver,
                other.system, other.satellite);
    }
};

struct E27RawNoiseRow
{
    vector<string> stampedKeys;
    VectorXd coefficients;
    VectorXd variances;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & stampedKeys;
		ar & coefficients;
		ar & variances;
	}
};

struct E27NoiseSensitivity
{
    GTime time;
    VectorXd stateDerivative;
    double variance = 0;
};

struct E27JointNoiseRuntime
{
    map<string, E27NoiseSensitivity> sensitivities;
    string failureReason;
};

map<E27RawNoiseRowKey, E27RawNoiseRow> e27RawNoiseRows;
map<string, E27JointNoiseRuntime> e27JointNoiseRuntimes;

string zhangPppArRuntimeId(const KFState& state)
{
	return zhangCheckpointRuntimeId(state);
}

bool e27JointNoiseEnabled()
{
    return acsConfig.zhangPppAr.user_adapter &&
        acsConfig.zhangPppAr.integer_strategy == "CANONICAL_USER_IF_WL_L1";
}

string e27EpochIdentity(GTime time)
{
    return time.to_string(3);
}

string e27NoiseIdentity(const KFKey& key)
{
    std::ostringstream stream;
    stream << static_cast<int>(key.type) << '|'
           << key.str << '|' << key.Sat.id() << '|'
           << key.num << '|' << key.comment;
    return stream.str();
}

string e27StampedNoiseIdentity(GTime time, const KFKey& key)
{
    return e27EpochIdentity(time) + "|" + e27NoiseIdentity(key);
}

bool e27SymmetricInverse(const MatrixXd& input, MatrixXd& inverse)
{
    if (input.rows() == 0 || input.rows() != input.cols() ||
        !input.allFinite())
    {
        return false;
    }
    Eigen::SelfAdjointEigenSolver<MatrixXd> solver(
        0.5 * (input + input.transpose()));
    if (solver.info() != Eigen::Success ||
        !solver.eigenvalues().allFinite())
    {
        return false;
    }
    const double maximum = solver.eigenvalues().maxCoeff();
    const double tolerance = std::max(1e-14, maximum * 1e-12);
    if (!(maximum > 0) || solver.eigenvalues().minCoeff() <= tolerance)
    {
        return false;
    }
    inverse = solver.eigenvectors() *
        solver.eigenvalues().cwiseInverse().asDiagonal() *
        solver.eigenvectors().transpose();
    return inverse.allFinite();
}

void e27TransformSensitivities(
	const string& runtimeId,
    const SparseMatrix<double>& transform,
    int sourceDimension,
    int destinationDimension,
    const string& label)
{
	auto found = e27JointNoiseRuntimes.find(runtimeId);
    if (found == e27JointNoiseRuntimes.end())
    {
        return;
    }
    for (auto& [ignored, sensitivity] : found->second.sensitivities)
    {
        if (sensitivity.stateDerivative.size() != sourceDimension)
        {
            found->second.failureReason =
                "STATE_SENSITIVITY_DIMENSION_MISMATCH_" + label;
            found->second.sensitivities.clear();
            return;
        }
        sensitivity.stateDerivative = transform * sensitivity.stateDerivative;
        if (sensitivity.stateDerivative.size() != destinationDimension ||
            !sensitivity.stateDerivative.allFinite())
        {
            found->second.failureReason =
                "STATE_SENSITIVITY_TRANSFORM_FAILURE_" + label;
            found->second.sensitivities.clear();
            return;
        }
    }
}

struct PromotionEvidenceKey
{
    E_Sys      system = E_Sys::NONE;
    E_ObsCode  observable = E_ObsCode::NONE;
    SatSys     a;
    int        segmentA = 0;
    SatSys     b;
    int        segmentB = 0;

    bool operator<(const PromotionEvidenceKey& other) const
    {
        return std::tie(system, observable, a, segmentA, b, segmentB) <
               std::tie(
                   other.system, other.observable,
                   other.a, other.segmentA, other.b, other.segmentB
               );
    }

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & system;
		ar & observable;
		ar & a;
		ar & segmentA;
		ar & b;
		ar & segmentB;
	}
};

struct PromotionEvidence
{
    long long difference = 0;
    long int  lastEpoch = 0;
    int       confirmations = 0;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & difference;
		ar & lastEpoch;
		ar & confirmations;
	}
};

map<PromotionEvidenceKey, PromotionEvidence> promotionEvidence;
map<PromotionEvidenceKey, PromotionEvidence> relinkEvidence;

ZhangSatelliteDatumManager& satelliteDatumManager(E_Sys sys, E_ObsCode code)
{
    auto key = std::make_pair(sys, code);
    auto found = satelliteDatumManagers.find(key);
    if (found == satelliteDatumManagers.end())
    {
        found = satelliteDatumManagers.emplace(
            key, ZhangSatelliteDatumManager(sys, code)
        ).first;
    }
    return found->second;
}

struct ProductLookupKey
{
    long int   epoch = 0;
    SatSys     satellite;
    E_ObsCode  observable = E_ObsCode::NONE;
    string     solution;

    bool operator<(const ProductLookupKey& other) const
    {
        return std::tie(epoch, satellite, observable, solution) <
               std::tie(other.epoch, other.satellite, other.observable, other.solution);
    }
};

map<ProductLookupKey, ZhangInternalProduct> productMap;
string loadedProductFilename;

struct ProductCovarianceKey
{
    SatSys satellite;
    string parameter;
    E_ObsCode observable = E_ObsCode::NONE;

    bool operator<(const ProductCovarianceKey& other) const
    {
        return std::tie(satellite, parameter, observable) <
            std::tie(other.satellite, other.parameter, other.observable);
    }
};

struct ProductCovarianceEpoch
{
    long int epoch = std::numeric_limits<long int>::min();
    string solution;
    map<ProductCovarianceKey, int> parameterIndex;
    MatrixXd squareRoot;
    int numericalRank = 0;
    bool valid = false;
    string failureReason = "NOT_LOADED";
};

struct ProductCovarianceReader
{
    string filename;
    std::ifstream stream;
    string pendingLine;
    long int lastRequestedEpoch = std::numeric_limits<long int>::min();
    ProductCovarianceEpoch cache;
};

ProductCovarianceReader productCovarianceReader;
std::mutex productCovarianceMutex;

struct ProductHistoryKey
{
    string     solution;
    SatSys     satellite;
    E_ObsCode  observable = E_ObsCode::NONE;

    bool operator<(const ProductHistoryKey& other) const
    {
        return std::tie(solution, satellite, observable) <
               std::tie(other.solution, other.satellite, other.observable);
    }

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & solution;
		ar & satellite;
		ar & observable;
	}
};

struct ProductHistory
{
    GTime  time;
    double correction = 0;
    int    discontinuityCounter = 0;
    int    datumVersion = 0;
};

map<ProductHistoryKey, ProductHistory> productHistoryMap;

struct UserReferenceKey
{
    string         runtimeId;
    string         receiver;
    E_Sys          sys = E_Sys::NONE;
    E_ObsCode      observable = E_ObsCode::NONE;

    bool operator<(const UserReferenceKey& other) const
    {
        return std::tie(runtimeId, receiver, sys, observable) <
               std::tie(
                   other.runtimeId, other.receiver,
                   other.sys, other.observable);
    }

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & runtimeId;
		ar & receiver;
		ar & sys;
		ar & observable;
	}
};

struct UserReferenceState
{
    SatSys reference;
    int    productCounter = -1;
    int    datumVersion = -1;
    map<SatSys, std::pair<int, int>> satelliteDatum;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & reference;
		ar & productCounter;
		ar & datumVersion;
		ar & satelliteDatum;
	}
};

map<UserReferenceKey, UserReferenceState> userReferenceMap;

struct UserDualReferenceKey
{
    string runtimeId;
    string receiver;
    E_Sys system = E_Sys::NONE;

    bool operator<(const UserDualReferenceKey& other) const
    {
        return std::tie(runtimeId, receiver, system) <
            std::tie(other.runtimeId, other.receiver, other.system);
    }

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & runtimeId;
		ar & receiver;
		ar & system;
	}
};

map<UserDualReferenceKey, SatSys> userDualReferenceMap;

struct ZhangCapturedFactorEventCheckpoint
{
	ZhangCapturedFactorKind kind = ZhangCapturedFactorKind::MEASUREMENT;
	ZhangCheckpointTime time;
	std::size_t sequence = 0;
	string label;
	vector<ZhangCapturedStateKey> sourceKeys;
	vector<ZhangCapturedStateKey> destinationKeys;
	SparseMatrix<double> design;
	SparseMatrix<double> covariance;
	VectorXd rightHandSide;
	VectorXd prefitRatios;
	vector<ZhangCapturedStateKey> observationKeys;
	bool dimensionPreserving = false;
	bool nonsingularCoordinateTransform = false;
	bool preserveUnrepresentablePersistentTargets = false;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & kind;
		ar & time;
		ar & sequence;
		ar & label;
		ar & sourceKeys;
		ar & destinationKeys;
		ar & design;
		ar & covariance;
		ar & rightHandSide;
		ar & prefitRatios;
		ar & observationKeys;
		ar & dimensionPreserving;
		ar & nonsingularCoordinateTransform;
		ar & preserveUnrepresentablePersistentTargets;
	}
};

struct ZhangCapturedPhysicalTargetCheckpoint
{
	ZhangCheckpointTime time;
	std::size_t afterEventSequence = 0;
	string identity;
	string physicalArcSignature;
	string phaseSegmentIdentity;
	vector<ZhangCapturedPhysicalArcVersion> physicalArcVersions;
	bool resetPhysicalIdentity = false;
	bool continuedAcrossCoordinateChange = false;
	vector<ZhangCapturedStateKey> keys;
	SparseMatrix<double> row;
	double offset = 0;
	double mean = 0;
	double variance = 0;
	int unresolvedIntegerGaugeRank = 0;
	string integerGaugeIdentity;
	string separatorIdentity;
	string canonicalCoordinateIdentity;
	string productDatumIdentity;
	int productDatumVersion = 0;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & time;
		ar & afterEventSequence;
		ar & identity;
		ar & physicalArcSignature;
		ar & phaseSegmentIdentity;
		ar & physicalArcVersions;
		ar & resetPhysicalIdentity;
		ar & continuedAcrossCoordinateChange;
		ar & keys;
		ar & row;
		ar & offset;
		ar & mean;
		ar & variance;
		ar & unresolvedIntegerGaugeRank;
		ar & integerGaugeIdentity;
		ar & separatorIdentity;
		ar & canonicalCoordinateIdentity;
		ar & productDatumIdentity;
		ar & productDatumVersion;
	}
};

struct ZhangCapturedUnresolvedIntegerDatumCheckpoint
{
	ZhangCheckpointTime time;
	std::size_t afterEventSequence = 0;
	string identity;
	int missingGaugeRank = 1;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & time;
		ar & afterEventSequence;
		ar & identity;
		ar & missingGaugeRank;
	}
};

struct ZhangCapturedRetainedTargetBlockCheckpoint
{
	ZhangCheckpointTime time;
	std::size_t afterEventSequence = 0;
	int targetCount = 0;
	int informationRank = 0;
	int residualDof = 0;
	int projectedGaugeRank = 0;
	bool likelihoodValid = false;
	bool valid = false;
	double whitenedSquaredNorm = std::numeric_limits<double>::quiet_NaN();
	VectorXd whitenedResidual;
	vector<string> separatorIdentities;
	vector<string> gaugeIdentities;
	vector<bool> absoluteValid;
	vector<double> coordinateOffsets;
	MatrixXd likelihoodDesign;
	VectorXd likelihoodObservation;
	MatrixXd likelihoodCovariance;
	string failureReason;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & time;
		ar & afterEventSequence;
		ar & targetCount;
		ar & informationRank;
		ar & residualDof;
		ar & projectedGaugeRank;
		ar & likelihoodValid;
		ar & valid;
		ar & whitenedSquaredNorm;
		ar & whitenedResidual;
		ar & separatorIdentities;
		ar & gaugeIdentities;
		ar & absoluteValid;
		ar & coordinateOffsets;
		ar & likelihoodDesign;
		ar & likelihoodObservation;
		ar & likelihoodCovariance;
		ar & failureReason;
	}
};

struct ZhangFactorCaptureCheckpoint
{
	std::size_t maximumEvents = 0;
	vector<ZhangCapturedStateKey> initialKeys;
	VectorXd initialMean;
	MatrixXd initialCovariance;
	vector<ZhangCapturedStateKey> currentKeys;
	VectorXd replayMean;
	MatrixXd replayCovariance;
	std::deque<ZhangCapturedFactorEventCheckpoint> events;
	std::deque<ZhangCapturedSnapshotOperation> snapshotOperations;
	std::deque<ZhangCapturedPhysicalTargetCheckpoint> physicalTargets;
	std::deque<ZhangCapturedUnresolvedIntegerDatumCheckpoint>
		unresolvedIntegerDatums;
	std::deque<ZhangCapturedRetainedTargetBlockCheckpoint>
		retainedTargetBlocks;
	ZhangCapturedRetainedTargetBlockCheckpoint currentRetainedTargetBlock;
	vector<ZhangInnovationScaleGroup> innovationScaleGroups;
	VectorXd lastMeasurementPriorMean;
	MatrixXd lastMeasurementPriorCovariance;
	ZhangCheckpointTime lastMeasurementTime;
	std::size_t lastMeasurementTargetStart = 0;
	string lastFailure;
	string lastTargetDispositionReason;
	double maximumReplayPriorMeanRelativeError = 0;
	double maximumReplayPriorCovarianceRelativeError = 0;
	double maximumTargetMeanRelativeError = 0;
	double maximumTargetVarianceRelativeError = 0;
	double maximumRawSquareRootMeanRelativeError = 0;
	double maximumRawSquareRootCovarianceRelativeError = 0;
	double maximumPersistentTransformMeanRelativeError = 0;
	double maximumPersistentTransformCovarianceRelativeError = 0;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & maximumEvents;
		ar & initialKeys;
		ar & initialMean;
		ar & initialCovariance;
		ar & currentKeys;
		ar & replayMean;
		ar & replayCovariance;
		ar & events;
		ar & snapshotOperations;
		ar & physicalTargets;
		ar & unresolvedIntegerDatums;
		ar & retainedTargetBlocks;
		ar & currentRetainedTargetBlock;
		ar & innovationScaleGroups;
		ar & lastMeasurementPriorMean;
		ar & lastMeasurementPriorCovariance;
		ar & lastMeasurementTime;
		ar & lastMeasurementTargetStart;
		ar & lastFailure;
		ar & lastTargetDispositionReason;
		ar & maximumReplayPriorMeanRelativeError;
		ar & maximumReplayPriorCovarianceRelativeError;
		ar & maximumTargetMeanRelativeError;
		ar & maximumTargetVarianceRelativeError;
		ar & maximumRawSquareRootMeanRelativeError;
		ar & maximumRawSquareRootCovarianceRelativeError;
		ar & maximumPersistentTransformMeanRelativeError;
		ar & maximumPersistentTransformCovarianceRelativeError;
	}
};

struct ZhangPhaseContinuityCheckpoint
{
	int counter = 0;
	long long integerShiftCycles = 0;
	double fractionalShiftCycles = 0;
	int datumVersion = 0;
	ZhangCheckpointTime validFrom;
	int iod = 0;
	string resetReason;
	int stabilizationRemaining = 0;
	bool hasFixedDatum = false;
	ZhangCheckpointTime lastEpoch;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & counter;
		ar & integerShiftCycles;
		ar & fractionalShiftCycles;
		ar & datumVersion;
		ar & validFrom;
		ar & iod;
		ar & resetReason;
		ar & stabilizationRemaining;
		ar & hasFixedDatum;
		ar & lastEpoch;
	}
};

struct GlobalContinuityCheckpoint
{
	int counter = 0;
	int datumVersion = 0;
	ZhangCheckpointTime validFrom;
	int iod = 0;
	string resetReason;
	int stabilizationRemaining = 0;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & counter;
		ar & datumVersion;
		ar & validFrom;
		ar & iod;
		ar & resetReason;
		ar & stabilizationRemaining;
	}
};

struct ZhangPendingSnapshotPinsCheckpoint
{
	ZhangCheckpointTime eventTime;
	set<string> identities;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & eventTime;
		ar & identities;
	}
};

struct E27NoiseSensitivityCheckpoint
{
	ZhangCheckpointTime time;
	VectorXd stateDerivative;
	double variance = 0;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & time;
		ar & stateDerivative;
		ar & variance;
	}
};

struct E27JointNoiseRuntimeCheckpoint
{
	map<string, E27NoiseSensitivityCheckpoint> sensitivities;
	string failureReason;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & sensitivities;
		ar & failureReason;
	}
};

struct ProductHistoryCheckpoint
{
	ZhangCheckpointTime time;
	double correction = 0;
	int discontinuityCounter = 0;
	int datumVersion = 0;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & time;
		ar & correction;
		ar & discontinuityCounter;
		ar & datumVersion;
	}
};

struct ZhangGraphEdgeCheckpoint
{
	string receiver;
	SatSys satellite;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & receiver;
		ar & satellite;
	}
};

struct ZhangProductIntegerFunctionalCheckpoint
{
	SatSys satellite;
	SatSys referenceSatellite;
	vector<ZhangGraphEdgeCheckpoint> physicalEdges;
	vector<string> networkCoefficients;
	vector<int> physicalArcVersions;
	string affineOffsetCycles;
	int temporalBasisVersion = 0;
	bool valid = false;
	string failureReason;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & satellite;
		ar & referenceSatellite;
		ar & physicalEdges;
		ar & networkCoefficients;
		ar & physicalArcVersions;
		ar & affineOffsetCycles;
		ar & temporalBasisVersion;
		ar & valid;
		ar & failureReason;
	}
};

struct ZhangProductIntegerTransitionCheckpoint
{
	vector<ZhangGraphEdgeCheckpoint> physicalEdges;
	vector<int> physicalArcVersions;
	vector<string> coefficients;
	string affineOffsetCycles;
	bool valid = false;
	string failureReason;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & physicalEdges;
		ar & physicalArcVersions;
		ar & coefficients;
		ar & affineOffsetCycles;
		ar & valid;
		ar & failureReason;
	}
};

struct ZhangPendingProductTransitionCheckpoint
{
	ZhangCheckpointTime eventTime;
	E_Sys system = E_Sys::NONE;
	SatSys satellite;
	E_ObsCode observable = E_ObsCode::NONE;
	string eventId;
	ZhangProductIntegerFunctionalCheckpoint oldFunctional;
	ZhangProductIntegerFunctionalCheckpoint newFunctional;
	ZhangProductIntegerTransitionCheckpoint transition;
	string oldIdentity;
	string newIdentity;
	string oldSBasisFingerprint;
	string newSBasisFingerprint;
	string oldPhaseSegmentIdentity;
	string newPhaseSegmentIdentity;
	bool phaseSegmentChanged = false;
	string eventCause;
	int oldProductSegment = 0;
	int newProductSegment = 0;
	string oldSnapshotIdentity;
	string newSnapshotIdentity;
	string exactTransformChainId;
	int oldSnapshotReferenceCount = 0;
	int newSnapshotReferenceCount = 0;
	ZhangCheckpointTime expiryTime;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & eventTime;
		ar & system;
		ar & satellite;
		ar & observable;
		ar & eventId;
		ar & oldFunctional;
		ar & newFunctional;
		ar & transition;
		ar & oldIdentity;
		ar & newIdentity;
		ar & oldSBasisFingerprint;
		ar & newSBasisFingerprint;
		ar & oldPhaseSegmentIdentity;
		ar & newPhaseSegmentIdentity;
		ar & phaseSegmentChanged;
		ar & eventCause;
		ar & oldProductSegment;
		ar & newProductSegment;
		ar & oldSnapshotIdentity;
		ar & newSnapshotIdentity;
		ar & exactTransformChainId;
		ar & oldSnapshotReferenceCount;
		ar & newSnapshotReferenceCount;
		ar & expiryTime;
	}
};

struct E27RawNoiseRowCheckpoint
{
	string epoch;
	string receiver;
	E_Sys system = E_Sys::NONE;
	SatSys satellite;
	E27RawNoiseRow row;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & epoch;
		ar & receiver;
		ar & system;
		ar & satellite;
		ar & row;
	}
};

struct ZhangHybridRealGaugeCheckpointEntry
{
	string solution;
	E_Sys system = E_Sys::NONE;
	string parameter;
	E_ObsCode observable = E_ObsCode::NONE;
	ZhangHybridRealGaugeCheckpoint gauge;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & solution;
		ar & system;
		ar & parameter;
		ar & observable;
		ar & gauge;
	}
};

struct ZhangPppArCheckpointEnvelope
{
	std::uint32_t schemaVersion = ZHANG_PPP_AR_CHECKPOINT_SCHEMA_VERSION;
	string runtimeId;
	vector<ZhangPendingProductTransitionCheckpoint> pendingTransitions;
	bool hasPendingSnapshotPins = false;
	ZhangPendingSnapshotPinsCheckpoint pendingPins;
	bool factorCaptureConfigured = false;
	bool hasFactorCapture = false;
	ZhangFactorCaptureCheckpoint factorCapture;
	bool hasPersistentProductDatumRegistry = false;
	ZhangPersistentProductDatumCheckpoint persistentProductDatumRegistry;
	vector<E27RawNoiseRowCheckpoint> e27RawRows;
	bool hasE27JointNoiseRuntime = false;
	E27JointNoiseRuntimeCheckpoint e27JointNoiseRuntime;

	map<ProductKey, ZhangPhaseContinuityCheckpoint> continuity;
	map<E_Sys, ZhangProductDatumVersionTracker> productDatumVersionTrackers;
	map<ProductKey, string> physicalFunctionalIdentities;
	map<ProductKey, ZhangProductIntegerFunctionalCheckpoint>
		physicalFunctionals;
	map<ProductKey, string> sBasisFingerprints;
	map<ProductKey, string> phaseSegmentIdentities;
	map<ProductKey, long long> treeAlignmentCycles;
	map<ProductKey, string> snapshotIdentities;
	map<std::pair<E_Sys, E_ObsCode>, GlobalContinuityCheckpoint>
		globalContinuity;
	vector<ZhangSatelliteDatumManagerCheckpoint> satelliteDatumManagers;
	vector<ZhangHybridRealGaugeCheckpointEntry> hybridRealGauges;
	map<PromotionEvidenceKey, PromotionEvidence> promotionEvidence;
	map<PromotionEvidenceKey, PromotionEvidence> relinkEvidence;
	map<ProductHistoryKey, ProductHistoryCheckpoint> productHistory;
	map<UserReferenceKey, UserReferenceState> userReferences;
	map<UserDualReferenceKey, SatSys> userDualReferences;

	template<class Archive>
	void serialize(Archive& ar, const unsigned int)
	{
		ar & schemaVersion;
		ar & runtimeId;
		ar & pendingTransitions;
		ar & hasPendingSnapshotPins;
		ar & pendingPins;
		ar & factorCaptureConfigured;
		ar & hasFactorCapture;
		ar & factorCapture;
		ar & hasPersistentProductDatumRegistry;
		ar & persistentProductDatumRegistry;
		ar & e27RawRows;
		ar & hasE27JointNoiseRuntime;
		ar & e27JointNoiseRuntime;
		ar & continuity;
		ar & productDatumVersionTrackers;
		ar & physicalFunctionalIdentities;
		ar & physicalFunctionals;
		ar & sBasisFingerprints;
		ar & phaseSegmentIdentities;
		ar & treeAlignmentCycles;
		ar & snapshotIdentities;
		ar & globalContinuity;
		ar & satelliteDatumManagers;
		ar & hybridRealGauges;
		ar & promotionEvidence;
		ar & relinkEvidence;
		ar & productHistory;
		ar & userReferences;
		ar & userDualReferences;
	}
};

// The continuity/Hou-datum/satellite-datum maps above form one product-service
// instance.  They are intentionally singleton in this PEA process and are
// claimed by exactly one stable runtime ID; a second ID fails closed.
string zhangPppArCheckpointRuntimeId;
set<string> e18RestoredRuntimeStates;
std::mutex zhangPppArCheckpointMutex;

bool validCheckpointRuntimeId(const string& runtimeId)
{
	if (runtimeId.empty() || runtimeId.size() > 512)
	{
		return false;
	}
	return std::all_of(runtimeId.begin(), runtimeId.end(), [](unsigned char ch)
	{
		return ch >= 0x21 && ch <= 0x7e;
	});
}

bool resolveZhangPppArRuntimeOwner(
	const KFState& state,
	string& runtimeId)
{
	runtimeId = zhangPppArRuntimeId(state);
	if (!validCheckpointRuntimeId(runtimeId))
	{
		return false;
	}
	auto [binding, inserted] = e18RuntimeObjectBindings.emplace(
		runtimeId, &state);
	return inserted || binding->second == &state;
}

bool claimZhangPppArServiceRuntime(
	const KFState& state,
	string& runtimeId)
{
	if (!resolveZhangPppArRuntimeOwner(state, runtimeId))
	{
		return false;
	}
	if (!zhangPppArCheckpointRuntimeId.empty()
	 && zhangPppArCheckpointRuntimeId != runtimeId)
	{
		return false;
	}
	zhangPppArCheckpointRuntimeId = runtimeId;
	return true;
}

bool finiteCheckpointTimeValue(const GTime& time)
{
	return std::isfinite(time.bigTime);
}

ZhangCapturedFactorEventCheckpoint checkpointFactorEvent(
	const ZhangCapturedFactorEvent& value)
{
	ZhangCapturedFactorEventCheckpoint result;
	result.kind = value.kind;
	result.time = captureZhangCheckpointTime(value.time);
	result.sequence = value.sequence;
	result.label = value.label;
	result.sourceKeys = value.sourceKeys;
	result.destinationKeys = value.destinationKeys;
	result.design = value.design;
	result.covariance = value.covariance;
	result.rightHandSide = value.rightHandSide;
	result.prefitRatios = value.prefitRatios;
	result.observationKeys = value.observationKeys;
	result.dimensionPreserving = value.dimensionPreserving;
	result.nonsingularCoordinateTransform =
		value.nonsingularCoordinateTransform;
	result.preserveUnrepresentablePersistentTargets =
		value.preserveUnrepresentablePersistentTargets;
	return result;
}

ZhangCapturedFactorEvent restoreFactorEvent(
	const ZhangCapturedFactorEventCheckpoint& value)
{
	ZhangCapturedFactorEvent result;
	result.kind = value.kind;
	result.time = restoreZhangCheckpointTime(value.time);
	result.sequence = value.sequence;
	result.label = value.label;
	result.sourceKeys = value.sourceKeys;
	result.destinationKeys = value.destinationKeys;
	result.design = value.design;
	result.covariance = value.covariance;
	result.rightHandSide = value.rightHandSide;
	result.prefitRatios = value.prefitRatios;
	result.observationKeys = value.observationKeys;
	result.dimensionPreserving = value.dimensionPreserving;
	result.nonsingularCoordinateTransform =
		value.nonsingularCoordinateTransform;
	result.preserveUnrepresentablePersistentTargets =
		value.preserveUnrepresentablePersistentTargets;
	return result;
}

ZhangCapturedPhysicalTargetCheckpoint checkpointPhysicalTarget(
	const ZhangCapturedPhysicalTarget& value)
{
	ZhangCapturedPhysicalTargetCheckpoint result;
	result.time = captureZhangCheckpointTime(value.time);
	result.afterEventSequence = value.afterEventSequence;
	result.identity = value.identity;
	result.physicalArcSignature = value.physicalArcSignature;
	result.phaseSegmentIdentity = value.phaseSegmentIdentity;
	result.physicalArcVersions = value.physicalArcVersions;
	result.resetPhysicalIdentity = value.resetPhysicalIdentity;
	result.continuedAcrossCoordinateChange =
		value.continuedAcrossCoordinateChange;
	result.keys = value.keys;
	result.row = value.row;
	result.offset = value.offset;
	result.mean = value.mean;
	result.variance = value.variance;
	result.unresolvedIntegerGaugeRank = value.unresolvedIntegerGaugeRank;
	result.integerGaugeIdentity = value.integerGaugeIdentity;
	result.separatorIdentity = value.separatorIdentity;
	result.canonicalCoordinateIdentity = value.canonicalCoordinateIdentity;
	result.productDatumIdentity = value.productDatumIdentity;
	result.productDatumVersion = value.productDatumVersion;
	return result;
}

ZhangCapturedPhysicalTarget restorePhysicalTarget(
	const ZhangCapturedPhysicalTargetCheckpoint& value)
{
	ZhangCapturedPhysicalTarget result;
	result.time = restoreZhangCheckpointTime(value.time);
	result.afterEventSequence = value.afterEventSequence;
	result.identity = value.identity;
	result.physicalArcSignature = value.physicalArcSignature;
	result.phaseSegmentIdentity = value.phaseSegmentIdentity;
	result.physicalArcVersions = value.physicalArcVersions;
	result.resetPhysicalIdentity = value.resetPhysicalIdentity;
	result.continuedAcrossCoordinateChange =
		value.continuedAcrossCoordinateChange;
	result.keys = value.keys;
	result.row = value.row;
	result.offset = value.offset;
	result.mean = value.mean;
	result.variance = value.variance;
	result.unresolvedIntegerGaugeRank = value.unresolvedIntegerGaugeRank;
	result.integerGaugeIdentity = value.integerGaugeIdentity;
	result.separatorIdentity = value.separatorIdentity;
	result.canonicalCoordinateIdentity = value.canonicalCoordinateIdentity;
	result.productDatumIdentity = value.productDatumIdentity;
	result.productDatumVersion = value.productDatumVersion;
	return result;
}

ZhangCapturedUnresolvedIntegerDatumCheckpoint checkpointUnresolvedDatum(
	const ZhangCapturedUnresolvedIntegerDatum& value)
{
	return {
		captureZhangCheckpointTime(value.time), value.afterEventSequence,
		value.identity, value.missingGaugeRank};
}

ZhangCapturedUnresolvedIntegerDatum restoreUnresolvedDatum(
	const ZhangCapturedUnresolvedIntegerDatumCheckpoint& value)
{
	return {
		restoreZhangCheckpointTime(value.time), value.afterEventSequence,
		value.identity, value.missingGaugeRank};
}

ZhangCapturedRetainedTargetBlockCheckpoint checkpointRetainedTargetBlock(
	const ZhangCapturedRetainedTargetBlock& value)
{
	ZhangCapturedRetainedTargetBlockCheckpoint result;
	result.time = captureZhangCheckpointTime(value.time);
	result.afterEventSequence = value.afterEventSequence;
	result.targetCount = value.targetCount;
	result.informationRank = value.informationRank;
	result.residualDof = value.residualDof;
	result.projectedGaugeRank = value.projectedGaugeRank;
	result.likelihoodValid = value.likelihoodValid;
	result.valid = value.valid;
	result.whitenedSquaredNorm = value.whitenedSquaredNorm;
	result.whitenedResidual = value.whitenedResidual;
	result.separatorIdentities = value.separatorIdentities;
	result.gaugeIdentities = value.gaugeIdentities;
	result.absoluteValid = value.absoluteValid;
	result.coordinateOffsets = value.coordinateOffsets;
	result.likelihoodDesign = value.likelihoodDesign;
	result.likelihoodObservation = value.likelihoodObservation;
	result.likelihoodCovariance = value.likelihoodCovariance;
	result.failureReason = value.failureReason;
	return result;
}

ZhangCapturedRetainedTargetBlock restoreRetainedTargetBlock(
	const ZhangCapturedRetainedTargetBlockCheckpoint& value)
{
	ZhangCapturedRetainedTargetBlock result;
	result.time = restoreZhangCheckpointTime(value.time);
	result.afterEventSequence = value.afterEventSequence;
	result.targetCount = value.targetCount;
	result.informationRank = value.informationRank;
	result.residualDof = value.residualDof;
	result.projectedGaugeRank = value.projectedGaugeRank;
	result.likelihoodValid = value.likelihoodValid;
	result.valid = value.valid;
	result.whitenedSquaredNorm = value.whitenedSquaredNorm;
	result.whitenedResidual = value.whitenedResidual;
	result.separatorIdentities = value.separatorIdentities;
	result.gaugeIdentities = value.gaugeIdentities;
	result.absoluteValid = value.absoluteValid;
	result.coordinateOffsets = value.coordinateOffsets;
	result.likelihoodDesign = value.likelihoodDesign;
	result.likelihoodObservation = value.likelihoodObservation;
	result.likelihoodCovariance = value.likelihoodCovariance;
	result.failureReason = value.failureReason;
	return result;
}

ZhangFactorCaptureCheckpoint checkpointFactorCapture(
	const ZhangFactorCaptureRuntimeReplay& value)
{
	ZhangFactorCaptureCheckpoint result;
	result.maximumEvents = value.maximumEvents;
	result.initialKeys = value.initialKeys;
	result.initialMean = value.initialMean;
	result.initialCovariance = value.initialCovariance;
	result.currentKeys = value.currentKeys;
	result.replayMean = value.replayMean;
	result.replayCovariance = value.replayCovariance;
	for (const auto& event : value.events)
	{
		result.events.push_back(checkpointFactorEvent(event));
	}
	result.snapshotOperations = value.snapshotOperations;
	for (const auto& target : value.physicalTargets)
	{
		result.physicalTargets.push_back(checkpointPhysicalTarget(target));
	}
	for (const auto& datum : value.unresolvedIntegerDatums)
	{
		result.unresolvedIntegerDatums.push_back(
			checkpointUnresolvedDatum(datum));
	}
	for (const auto& block : value.retainedTargetBlocks)
	{
		result.retainedTargetBlocks.push_back(
			checkpointRetainedTargetBlock(block));
	}
	result.currentRetainedTargetBlock =
		checkpointRetainedTargetBlock(value.currentRetainedTargetBlock);
	result.innovationScaleGroups = value.innovationScaleGroups;
	result.lastMeasurementPriorMean = value.lastMeasurementPriorMean;
	result.lastMeasurementPriorCovariance = value.lastMeasurementPriorCovariance;
	result.lastMeasurementTime =
		captureZhangCheckpointTime(value.lastMeasurementTime);
	result.lastMeasurementTargetStart = value.lastMeasurementTargetStart;
	result.lastFailure = value.lastFailure;
	result.lastTargetDispositionReason = value.lastTargetDispositionReason;
	result.maximumReplayPriorMeanRelativeError =
		value.maximumReplayPriorMeanRelativeError;
	result.maximumReplayPriorCovarianceRelativeError =
		value.maximumReplayPriorCovarianceRelativeError;
	result.maximumTargetMeanRelativeError =
		value.maximumTargetMeanRelativeError;
	result.maximumTargetVarianceRelativeError =
		value.maximumTargetVarianceRelativeError;
	result.maximumRawSquareRootMeanRelativeError =
		value.maximumRawSquareRootMeanRelativeError;
	result.maximumRawSquareRootCovarianceRelativeError =
		value.maximumRawSquareRootCovarianceRelativeError;
	result.maximumPersistentTransformMeanRelativeError =
		value.maximumPersistentTransformMeanRelativeError;
	result.maximumPersistentTransformCovarianceRelativeError =
		value.maximumPersistentTransformCovarianceRelativeError;
	return result;
}

ZhangFactorCaptureRuntimeReplay restoreFactorCapture(
	const ZhangFactorCaptureCheckpoint& value)
{
	ZhangFactorCaptureRuntimeReplay result;
	result.maximumEvents = value.maximumEvents;
	result.initialKeys = value.initialKeys;
	result.initialMean = value.initialMean;
	result.initialCovariance = value.initialCovariance;
	result.currentKeys = value.currentKeys;
	result.replayMean = value.replayMean;
	result.replayCovariance = value.replayCovariance;
	for (const auto& event : value.events)
	{
		result.events.push_back(restoreFactorEvent(event));
	}
	result.snapshotOperations = value.snapshotOperations;
	for (const auto& target : value.physicalTargets)
	{
		result.physicalTargets.push_back(restorePhysicalTarget(target));
	}
	for (const auto& datum : value.unresolvedIntegerDatums)
	{
		result.unresolvedIntegerDatums.push_back(
			restoreUnresolvedDatum(datum));
	}
	for (const auto& block : value.retainedTargetBlocks)
	{
		result.retainedTargetBlocks.push_back(
			restoreRetainedTargetBlock(block));
	}
	result.currentRetainedTargetBlock =
		restoreRetainedTargetBlock(value.currentRetainedTargetBlock);
	result.innovationScaleGroups = value.innovationScaleGroups;
	result.lastMeasurementPriorMean = value.lastMeasurementPriorMean;
	result.lastMeasurementPriorCovariance = value.lastMeasurementPriorCovariance;
	result.lastMeasurementTime =
		restoreZhangCheckpointTime(value.lastMeasurementTime);
	result.lastMeasurementTargetStart = value.lastMeasurementTargetStart;
	result.lastFailure = value.lastFailure;
	result.lastTargetDispositionReason = value.lastTargetDispositionReason;
	result.maximumReplayPriorMeanRelativeError =
		value.maximumReplayPriorMeanRelativeError;
	result.maximumReplayPriorCovarianceRelativeError =
		value.maximumReplayPriorCovarianceRelativeError;
	result.maximumTargetMeanRelativeError =
		value.maximumTargetMeanRelativeError;
	result.maximumTargetVarianceRelativeError =
		value.maximumTargetVarianceRelativeError;
	result.maximumRawSquareRootMeanRelativeError =
		value.maximumRawSquareRootMeanRelativeError;
	result.maximumRawSquareRootCovarianceRelativeError =
		value.maximumRawSquareRootCovarianceRelativeError;
	result.maximumPersistentTransformMeanRelativeError =
		value.maximumPersistentTransformMeanRelativeError;
	result.maximumPersistentTransformCovarianceRelativeError =
		value.maximumPersistentTransformCovarianceRelativeError;
	return result;
}

ZhangPhaseContinuityCheckpoint checkpointContinuity(
	const ZhangPhaseContinuityState& value)
{
	return {
		value.counter, value.integerShiftCycles,
		value.fractionalShiftCycles, value.datumVersion,
		captureZhangCheckpointTime(value.validFrom), value.iod,
		value.resetReason, value.stabilizationRemaining, value.hasFixedDatum,
		captureZhangCheckpointTime(value.lastEpoch)};
}

ZhangPhaseContinuityState restoreContinuity(
	const ZhangPhaseContinuityCheckpoint& value)
{
	ZhangPhaseContinuityState result;
	result.counter = value.counter;
	result.integerShiftCycles = value.integerShiftCycles;
	result.fractionalShiftCycles = value.fractionalShiftCycles;
	result.datumVersion = value.datumVersion;
	result.validFrom = restoreZhangCheckpointTime(value.validFrom);
	result.iod = value.iod;
	result.resetReason = value.resetReason;
	result.stabilizationRemaining = value.stabilizationRemaining;
	result.hasFixedDatum = value.hasFixedDatum;
	result.lastEpoch = restoreZhangCheckpointTime(value.lastEpoch);
	return result;
}

GlobalContinuityCheckpoint checkpointGlobalContinuity(
	const GlobalContinuityState& value)
{
	return {
		value.counter, value.datumVersion,
		captureZhangCheckpointTime(value.validFrom), value.iod,
		value.resetReason, value.stabilizationRemaining};
}

GlobalContinuityState restoreGlobalContinuity(
	const GlobalContinuityCheckpoint& value)
{
	return {
		value.counter, value.datumVersion,
		restoreZhangCheckpointTime(value.validFrom), value.iod,
		value.resetReason, value.stabilizationRemaining};
}

ZhangPendingSnapshotPinsCheckpoint checkpointPendingPins(
	const ZhangPendingSnapshotPins& value)
{
	return {captureZhangCheckpointTime(value.eventTime), value.identities};
}

ZhangPendingSnapshotPins restorePendingPins(
	const ZhangPendingSnapshotPinsCheckpoint& value)
{
	return {restoreZhangCheckpointTime(value.eventTime), value.identities};
}

E27JointNoiseRuntimeCheckpoint checkpointE27JointRuntime(
	const E27JointNoiseRuntime& value)
{
	E27JointNoiseRuntimeCheckpoint result;
	result.failureReason = value.failureReason;
	for (const auto& [identity, sensitivity] : value.sensitivities)
	{
		result.sensitivities.emplace(identity, E27NoiseSensitivityCheckpoint{
			captureZhangCheckpointTime(sensitivity.time),
			sensitivity.stateDerivative, sensitivity.variance});
	}
	return result;
}

E27JointNoiseRuntime restoreE27JointRuntime(
	const E27JointNoiseRuntimeCheckpoint& value)
{
	E27JointNoiseRuntime result;
	result.failureReason = value.failureReason;
	for (const auto& [identity, sensitivity] : value.sensitivities)
	{
		result.sensitivities.emplace(identity, E27NoiseSensitivity{
			restoreZhangCheckpointTime(sensitivity.time),
			sensitivity.stateDerivative, sensitivity.variance});
	}
	return result;
}

ProductHistoryCheckpoint checkpointProductHistory(
	const ProductHistory& value)
{
	return {
		captureZhangCheckpointTime(value.time), value.correction,
		value.discontinuityCounter, value.datumVersion};
}

ProductHistory restoreProductHistory(
	const ProductHistoryCheckpoint& value)
{
	return {
		restoreZhangCheckpointTime(value.time), value.correction,
		value.discontinuityCounter, value.datumVersion};
}

string exactIntegerText(const ZhangExactInteger& value)
{
	return value.str(0, std::ios_base::fmtflags(0));
}

bool parseExactInteger(
	const string& text,
	ZhangExactInteger& value,
	string& failureReason)
{
	if (text.empty() || text.size() > 4096)
	{
		failureReason = "PPP_AR_CHECKPOINT_INVALID_EXACT_INTEGER_LENGTH";
		return false;
	}
	std::size_t position = text.front() == '-' ? 1 : 0;
	if (position == text.size()
	 || !std::all_of(text.begin() + position, text.end(),
		[](unsigned char ch) { return std::isdigit(ch) != 0; }))
	{
		failureReason = "PPP_AR_CHECKPOINT_INVALID_EXACT_INTEGER_TEXT";
		return false;
	}
	try
	{
		value = ZhangExactInteger(text);
	}
	catch (const std::exception&)
	{
		failureReason = "PPP_AR_CHECKPOINT_EXACT_INTEGER_PARSE_FAILED";
		return false;
	}
	return true;
}

ZhangProductIntegerFunctionalCheckpoint checkpointFunctional(
	const ZhangProductIntegerFunctional& value)
{
	ZhangProductIntegerFunctionalCheckpoint result;
	result.satellite = value.satellite;
	result.referenceSatellite = value.referenceSatellite;
	for (const auto& edge : value.physicalEdges)
	{
		result.physicalEdges.push_back({edge.receiver, edge.satellite});
	}
	for (const auto& coefficient : value.networkCoefficients)
	{
		result.networkCoefficients.push_back(exactIntegerText(coefficient));
	}
	result.physicalArcVersions = value.physicalArcVersions;
	result.affineOffsetCycles = exactIntegerText(value.affineOffsetCycles);
	result.temporalBasisVersion = value.temporalBasisVersion;
	result.valid = value.valid;
	result.failureReason = value.failureReason;
	return result;
}

bool restoreFunctional(
	const ZhangProductIntegerFunctionalCheckpoint& snapshot,
	ZhangProductIntegerFunctional& value,
	string& failureReason)
{
	if (snapshot.physicalEdges.size() != snapshot.networkCoefficients.size()
	 || snapshot.physicalEdges.size() != snapshot.physicalArcVersions.size())
	{
		failureReason = "PPP_AR_CHECKPOINT_FUNCTIONAL_DIMENSION_MISMATCH";
		return false;
	}
	ZhangProductIntegerFunctional candidate;
	candidate.satellite = snapshot.satellite;
	candidate.referenceSatellite = snapshot.referenceSatellite;
	for (std::size_t index = 0; index < snapshot.physicalEdges.size(); index++)
	{
		const auto& edge = snapshot.physicalEdges[index];
		if (edge.receiver.empty() || edge.satellite.prn <= 0)
		{
			failureReason = "PPP_AR_CHECKPOINT_INVALID_FUNCTIONAL_EDGE";
			return false;
		}
		candidate.physicalEdges.push_back({edge.receiver, edge.satellite});
		ZhangExactInteger coefficient;
		if (!parseExactInteger(
			snapshot.networkCoefficients[index], coefficient, failureReason))
		{
			return false;
		}
		candidate.networkCoefficients.push_back(coefficient);
	}
	candidate.physicalArcVersions = snapshot.physicalArcVersions;
	if (!parseExactInteger(
		snapshot.affineOffsetCycles, candidate.affineOffsetCycles,
		failureReason))
	{
		return false;
	}
	candidate.temporalBasisVersion = snapshot.temporalBasisVersion;
	candidate.valid = snapshot.valid;
	candidate.failureReason = snapshot.failureReason;
	value = std::move(candidate);
	return true;
}

ZhangProductIntegerTransitionCheckpoint checkpointTransition(
	const ZhangProductIntegerTransition& value)
{
	ZhangProductIntegerTransitionCheckpoint result;
	for (const auto& edge : value.physicalEdges)
	{
		result.physicalEdges.push_back({edge.receiver, edge.satellite});
	}
	result.physicalArcVersions = value.physicalArcVersions;
	for (const auto& coefficient : value.coefficients)
	{
		result.coefficients.push_back(exactIntegerText(coefficient));
	}
	result.affineOffsetCycles = exactIntegerText(value.affineOffsetCycles);
	result.valid = value.valid;
	result.failureReason = value.failureReason;
	return result;
}

bool restoreTransition(
	const ZhangProductIntegerTransitionCheckpoint& snapshot,
	ZhangProductIntegerTransition& value,
	string& failureReason)
{
	if (snapshot.physicalEdges.size() != snapshot.coefficients.size()
	 || snapshot.physicalEdges.size() != snapshot.physicalArcVersions.size())
	{
		failureReason = "PPP_AR_CHECKPOINT_TRANSITION_DIMENSION_MISMATCH";
		return false;
	}
	ZhangProductIntegerTransition candidate;
	for (std::size_t index = 0; index < snapshot.physicalEdges.size(); index++)
	{
		const auto& edge = snapshot.physicalEdges[index];
		if (edge.receiver.empty() || edge.satellite.prn <= 0)
		{
			failureReason = "PPP_AR_CHECKPOINT_INVALID_TRANSITION_EDGE";
			return false;
		}
		candidate.physicalEdges.push_back({edge.receiver, edge.satellite});
		ZhangExactInteger coefficient;
		if (!parseExactInteger(
			snapshot.coefficients[index], coefficient, failureReason))
		{
			return false;
		}
		candidate.coefficients.push_back(coefficient);
	}
	candidate.physicalArcVersions = snapshot.physicalArcVersions;
	if (!parseExactInteger(
		snapshot.affineOffsetCycles, candidate.affineOffsetCycles,
		failureReason))
	{
		return false;
	}
	candidate.valid = snapshot.valid;
	candidate.failureReason = snapshot.failureReason;
	value = std::move(candidate);
	return true;
}

ZhangPendingProductTransitionCheckpoint checkpointPendingTransition(
	const ZhangPendingProductTransition& value)
{
	return {
		captureZhangCheckpointTime(value.eventTime),
		value.system, value.satellite, value.observable,
		value.eventId, checkpointFunctional(value.oldFunctional),
		checkpointFunctional(value.newFunctional),
		checkpointTransition(value.transition), value.oldIdentity,
		value.newIdentity, value.oldSBasisFingerprint,
		value.newSBasisFingerprint, value.oldPhaseSegmentIdentity,
		value.newPhaseSegmentIdentity, value.phaseSegmentChanged,
		value.eventCause, value.oldProductSegment, value.newProductSegment,
		value.oldSnapshotIdentity, value.newSnapshotIdentity,
		value.exactTransformChainId, value.oldSnapshotReferenceCount,
		value.newSnapshotReferenceCount,
		captureZhangCheckpointTime(value.expiryTime)};
}

bool restorePendingTransition(
	const ZhangPendingProductTransitionCheckpoint& snapshot,
	ZhangPendingProductTransition& value,
	string& failureReason)
{
	if (snapshot.system == E_Sys::NONE
	 || snapshot.observable == E_ObsCode::NONE
	 || snapshot.satellite.sys != snapshot.system
	 || snapshot.satellite.prn <= 0
	 || snapshot.eventId.empty()
	 || snapshot.oldSnapshotReferenceCount < 0
	 || snapshot.newSnapshotReferenceCount < 0)
	{
		failureReason = "PPP_AR_CHECKPOINT_INVALID_PENDING_TRANSITION";
		return false;
	}
	ZhangPendingProductTransition candidate;
	candidate.eventTime = restoreZhangCheckpointTime(snapshot.eventTime);
	candidate.system = snapshot.system;
	candidate.satellite = snapshot.satellite;
	candidate.observable = snapshot.observable;
	candidate.eventId = snapshot.eventId;
	if (!restoreFunctional(
		snapshot.oldFunctional, candidate.oldFunctional, failureReason)
	 || !restoreFunctional(
		snapshot.newFunctional, candidate.newFunctional, failureReason)
	 || !restoreTransition(
		snapshot.transition, candidate.transition, failureReason))
	{
		return false;
	}
	if (!finiteCheckpointTimeValue(candidate.eventTime)
	 || candidate.eventTime == GTime::noTime()
	 || candidate.oldFunctional.satellite != snapshot.satellite
	 || candidate.newFunctional.satellite != snapshot.satellite)
	{
		failureReason = "PPP_AR_CHECKPOINT_PENDING_TRANSITION_MISMATCH";
		return false;
	}
	candidate.oldIdentity = snapshot.oldIdentity;
	candidate.newIdentity = snapshot.newIdentity;
	candidate.oldSBasisFingerprint = snapshot.oldSBasisFingerprint;
	candidate.newSBasisFingerprint = snapshot.newSBasisFingerprint;
	candidate.oldPhaseSegmentIdentity = snapshot.oldPhaseSegmentIdentity;
	candidate.newPhaseSegmentIdentity = snapshot.newPhaseSegmentIdentity;
	candidate.phaseSegmentChanged = snapshot.phaseSegmentChanged;
	candidate.eventCause = snapshot.eventCause;
	candidate.oldProductSegment = snapshot.oldProductSegment;
	candidate.newProductSegment = snapshot.newProductSegment;
	candidate.oldSnapshotIdentity = snapshot.oldSnapshotIdentity;
	candidate.newSnapshotIdentity = snapshot.newSnapshotIdentity;
	candidate.exactTransformChainId = snapshot.exactTransformChainId;
	candidate.oldSnapshotReferenceCount = snapshot.oldSnapshotReferenceCount;
	candidate.newSnapshotReferenceCount = snapshot.newSnapshotReferenceCount;
	candidate.expiryTime = restoreZhangCheckpointTime(snapshot.expiryTime);
	if (!finiteCheckpointTimeValue(candidate.expiryTime)
	 || (candidate.expiryTime != GTime::noTime()
		 && candidate.expiryTime < candidate.eventTime))
	{
		failureReason = "PPP_AR_CHECKPOINT_INVALID_TRANSITION_EXPIRY";
		return false;
	}
	value = std::move(candidate);
	return true;
}

bool validateProductKey(const ProductKey& key)
{
	return key.satellite.prn > 0 && key.satellite.sys != E_Sys::NONE
		&& key.observable != E_ObsCode::NONE;
}

double wavelength(E_Sys sys, E_ObsCode code)
{
    auto sysIt = code2Freq.find(sys);
    if (sysIt == code2Freq.end())
    {
        return 0;
    }

    auto frequencyIt = sysIt->second.find(code);
    if (frequencyIt == sysIt->second.end())
    {
        return 0;
    }

    auto wavelengthIt = genericWavelength.find(frequencyIt->second);
    if (wavelengthIt == genericWavelength.end())
    {
        return 0;
    }

    return wavelengthIt->second;
}

bool usesSharedIfUserCoordinate(E_Sys sys, E_ObsCode code)
{
    if (acsConfig.zhangPppAr.integer_strategy != "CANONICAL_USER_IF_WL_L1")
    {
        return false;
    }
    auto observables = acsConfig.zhangPppAr.baseline_observables.find(sys);
    return observables != acsConfig.zhangPppAr.baseline_observables.end() &&
        observables->second.size() == 2 &&
        (code == observables->second[0] || code == observables->second[1]);
}

int userPhaseCoordinateNumber(E_Sys sys, E_ObsCode code)
{
    if (!usesSharedIfUserCoordinate(sys, code))
    {
        return static_cast<int>(code);
    }
    const auto& observables =
        acsConfig.zhangPppAr.baseline_observables.at(sys);
    return 100 * static_cast<int>(observables[0]) +
        static_cast<int>(observables[1]);
}

double userPhaseCoordinateWavelength(E_Sys sys, E_ObsCode code)
{
    if (!usesSharedIfUserCoordinate(sys, code))
    {
        return wavelength(sys, code);
    }
    const auto& observables =
        acsConfig.zhangPppAr.baseline_observables.at(sys);
    const auto coefficients = zhangIfUserCoefficients(
        wavelength(sys, observables[0]), wavelength(sys, observables[1]));
    return coefficients.valid ? coefficients.narrowLaneWavelength : 0;
}

KFKey userAmbiguityKey(const string& receiver, const SatSys& satellite, E_ObsCode code)
{
    KFKey key;
    key.type = KF::AMBIGUITY;
    key.str  = receiver;
    key.Sat  = satellite;
    key.num  = userPhaseCoordinateNumber(satellite.sys, code);
    return key;
}

bool slipIsExcluded(const SigStat::SlipStat& slip)
{
    if (!slip.any)
    {
        return false;
    }

    return
        (acsConfig.exclude.LLI         && slip.LLI)        ||
        (acsConfig.exclude.GF          && slip.GF)         ||
        (acsConfig.exclude.MW          && slip.MW)         ||
        (acsConfig.exclude.SCDIA       && slip.SCDIA)      ||
        (acsConfig.exclude.retrack     && slip.retrack)    ||
        (acsConfig.exclude.single_freq && slip.singleFreq);
}

bool signalUsable(const GObs& obs, E_ObsCode code)
{
    for (const auto& [frequency, signal] : obs.sigs)
    {
        if (signal.code != code || signal.P == 0 || signal.L == 0 || signal.invalid)
        {
            continue;
        }

        if (obs.satStat_ptr)
        {
            auto slipIt = obs.satStat_ptr->sigStatMap.find(ft2string(frequency));
            if (slipIt != obs.satStat_ptr->sigStatMap.end() &&
                slipIsExcluded(slipIt->second.slip))
            {
                continue;
            }
        }

        return true;
    }

    return false;
}

void initialiseContinuityState(
    const ProductKey& key,
    ZhangPhaseContinuityState& state
)
{
    if (state.validFrom != GTime::noTime())
    {
        return;
    }

    auto& global = globalContinuityMap[{key.satellite.sys, key.observable}];
    if (global.validFrom == GTime::noTime())
    {
        global.counter = acsConfig.zhangPppAr.initial_discontinuity_counter;
    }

    state.counter                   = global.counter;
    state.datumVersion              = global.datumVersion;
    state.validFrom                 = global.validFrom;
    state.iod                       = global.iod;
    state.resetReason               = global.resetReason;
    state.stabilizationRemaining    = global.stabilizationRemaining;
}

bool ensureProductFileHeader()
{
    static string initializedFilename;
	static const string expectedHeader =
		"gpst_seconds,solution,satellite,observable,clock_m,clock_sigma_m,"
		"phase_m,phase_sigma_m,clock_phase_covariance_m2,correction_m,"
		"correction_sigma_m,discontinuity_counter,integer_shift_cycles,"
		"fractional_shift_cycles,datum_version,valid_from_gpst_seconds,"
		"product_iod,reset_reason,persistent_relation_known,"
		"current_alignment_state,integer_structure_valid,"
		"integer_datum_continuous,integer_precision_valid,integer_valid,"
		"integer_component_id,integer_datum_id,"
		"solution_interval_start_gpst_seconds,"
		"solution_interval_end_gpst_seconds,numeric_valid,branch_valid,"
		"continuity_valid,ppp_usable,pppar_usable,invalid_reason,"
		"phase_product_segment_id,integer_component_version,"
		"integer_alignment_generation,real_gauge_generation,"
		"integer_component_size,integer_component_rank,"
		"certified_relation_count,redundant_relation_count,"
		"cycle_closure_valid,dual_frequency_ar_valid,product_state,"
		"discontinuity,ar_valid,support_segment_fingerprint,"
		"backend_s_basis_generation";

    const string& filename = acsConfig.zhangPppAr.product_filename;
    if (filename.empty())
    {
		return false;
	}
	if (initializedFilename == filename)
	{
		return true;
    }

    std::filesystem::path path(filename);
    if (path.has_parent_path())
    {
        std::filesystem::create_directories(path.parent_path());
    }

	std::error_code fileError;
	const bool existing = std::filesystem::exists(path, fileError)
		&& !fileError && std::filesystem::file_size(path, fileError) > 0;
	if (fileError)
	{
		BOOST_LOG_TRIVIAL(error)
			<< "ZHANG_PRODUCT_OUTPUT_HEADER_CHECK_FAILED file=" << filename
			<< " error=" << fileError.message();
		return false;
	}
	if (existing)
	{
		std::ifstream input(filename);
		string header;
		if (!input || !std::getline(input, header) || header != expectedHeader)
		{
			BOOST_LOG_TRIVIAL(error)
				<< "ZHANG_PRODUCT_OUTPUT_HEADER_MISMATCH file=" << filename;
			return false;
		}
	}
	else
	{
		std::ofstream output(filename, std::ios::trunc);
		output << expectedHeader << '\n';
		if (!output)
		{
			BOOST_LOG_TRIVIAL(error)
				<< "ZHANG_PRODUCT_OUTPUT_HEADER_WRITE_FAILED file=" << filename;
			return false;
		}
	}

    initializedFilename = filename;
	return true;
}

void appendProduct(const ZhangInternalProduct& product)
{
    if (!ensureProductFileHeader())
	{
		return;
	}

    std::ofstream output(acsConfig.zhangPppAr.product_filename, std::ios::app);
    output << std::setprecision(17)
           << static_cast<double>(product.time.bigTime) << ","
           << product.solution << ","
           << product.satellite.id() << ","
           << enum_to_string(product.observable) << ","
           << product.clock_m << ","
           << product.clock_sigma_m << ","
           << product.phase_m << ","
           << product.phase_sigma_m << ","
           << product.clock_phase_covariance_m2 << ","
           << product.correction_m << ","
           << product.correction_sigma_m << ","
           << product.discontinuity_counter << ","
           << product.integer_shift_cycles << ","
           << product.fractional_shift_cycles << ","
           << product.datum_version << ","
           << static_cast<double>(product.valid_from.bigTime) << ","
           << product.product_iod << ","
           << product.reset_reason << ","
           << product.persistent_relation_known << ","
           << product.current_alignment_state << ","
           << product.integer_structure_valid << ","
           << product.integer_datum_continuous << ","
           << product.integer_precision_valid << ","
           << product.integer_valid << ","
           << product.integer_component_id << ","
           << product.integer_datum_id << ","
           << static_cast<double>(product.valid_from.bigTime) << ","
           << static_cast<double>(product.time.bigTime) << ","
           << product.numeric_valid << ","
           << product.branch_valid << ","
           << product.continuity_valid << ","
           << product.ppp_usable << ","
           << product.pppar_usable << ","
           << product.invalid_reason << ","
		   << product.phase_product_segment_id << ","
		   << product.integer_component_version << ","
		   << product.integer_alignment_generation << ","
		   << product.real_gauge_generation << ","
		   << product.integer_component_size << ","
		   << product.integer_component_rank << ","
		   << product.certified_relation_count << ","
		   << product.redundant_relation_count << ","
		   << product.cycle_closure_valid << ","
		   << product.dual_frequency_ar_valid << ","
		   << product.product_state << ","
		   << product.discontinuity << ","
		   << product.ar_valid << ","
		   << product.support_segment_fingerprint << ","
		   << product.backend_s_basis_generation << "\n";
}

struct ProductCovarianceParameter
{
    KFKey       key;
    SatSys      satellite;
    string      parameter;
    E_ObsCode   observable = E_ObsCode::NONE;
    vector<pair<int, double>> stateTerms;
};

bool ensureProductCovarianceFileHeader()
{
    static string initializedFilename;
	static const string expectedHeader =
		"gpst_seconds,solution,row_satellite,row_parameter,row_observable,"
		"column_satellite,column_parameter,column_observable,covariance_m2";

    const string& filename =
        acsConfig.zhangPppAr.product_covariance_filename;
    if (filename.empty())
    {
		return false;
	}
	if (initializedFilename == filename)
	{
		return true;
    }

    std::filesystem::path path(filename);
    if (path.has_parent_path())
    {
        std::filesystem::create_directories(path.parent_path());
    }

	std::error_code fileError;
	const bool existing = std::filesystem::exists(path, fileError)
		&& !fileError && std::filesystem::file_size(path, fileError) > 0;
	if (fileError)
	{
		BOOST_LOG_TRIVIAL(error)
			<< "ZHANG_PRODUCT_COVARIANCE_HEADER_CHECK_FAILED file=" << filename
			<< " error=" << fileError.message();
		return false;
	}
	if (existing)
	{
		std::ifstream input(filename);
		string header;
		if (!input || !std::getline(input, header) || header != expectedHeader)
		{
			BOOST_LOG_TRIVIAL(error)
				<< "ZHANG_PRODUCT_COVARIANCE_HEADER_MISMATCH file=" << filename;
			return false;
		}
	}
	else
	{
		std::ofstream output(filename, std::ios::trunc);
		output << expectedHeader << '\n';
		if (!output)
		{
			BOOST_LOG_TRIVIAL(error)
				<< "ZHANG_PRODUCT_COVARIANCE_HEADER_WRITE_FAILED file="
				<< filename;
			return false;
		}
	}

    initializedFilename = filename;
	return true;
}

void appendProductCovariance(
    const KFState& state,
    const string&  solution,
    const KFState& graphState,
	vector<ZhangInternalProduct>& epochProducts
)
{
    const string& filename =
        acsConfig.zhangPppAr.product_covariance_filename;
    if (filename.empty())
    {
        return;
    }

    vector<ProductCovarianceParameter> parameters;
    set<SatSys> satellites;
    for (const auto& [key, index] : state.kfIndexMap)
    {
        if (key.type == KF::PHASE_BIAS &&
            key.Sat.prn > 0 &&
            key.str.empty() &&
            zhangGraphProductSatelliteActive(graphState, key.Sat) &&
            zhangPppArUsesObservable(
                key.Sat.sys,
                static_cast<E_ObsCode>(key.num)
            ))
        {
            satellites.insert(key.Sat);
        }
    }

    for (const SatSys& satellite : satellites)
    {
        KFKey clockKey;
        clockKey.type = KF::SAT_CLOCK;
        clockKey.Sat  = satellite;
        auto clockIt = state.kfIndexMap.find(clockKey);
        if (clockIt == state.kfIndexMap.end())
        {
            continue;
        }
        parameters.push_back({
            clockKey,
            satellite,
            "CLOCK",
            E_ObsCode::NONE,
            {{clockIt->second, 1.0}}
        });

        const auto& observables =
            acsConfig.zhangPppAr.baseline_observables[satellite.sys];
        for (E_ObsCode observable : observables)
        {
            KFKey phaseKey;
            phaseKey.type = KF::PHASE_BIAS;
            phaseKey.Sat  = satellite;
            phaseKey.num  = static_cast<int>(observable);
            auto phaseIt = state.kfIndexMap.find(phaseKey);
            if (phaseIt == state.kfIndexMap.end())
            {
                continue;
            }
            vector<pair<int, double>> phaseTerms = {{phaseIt->second, 1.0}};
			// The product tree is a backend coordinate and relation-evidence
			// source only.  The broadcast phase primitive is the satellite
			// phase state plus an exact integer potential from the persistent
			// G_AR manager; that potential is a constant and adds no covariance
			// terms.  Never leak ambiguity-tree rows into the frontend covariance.
            parameters.push_back({
                phaseKey, satellite, "PHASE", observable, std::move(phaseTerms)
            });
        }
    }

    if (parameters.empty())
	{
		return;
	}

	const int dimension = static_cast<int>(parameters.size());
	VectorXd rawMean = VectorXd::Zero(dimension);
	MatrixXd rawCovariance = MatrixXd::Zero(dimension, dimension);
	for (int row = 0; row < dimension; row++)
	{
		for (const auto& [index, coefficient] : parameters[row].stateTerms)
		{
			rawMean(row) += coefficient * state.x(index);
		}
		for (int column = row; column < dimension; column++)
		{
			double covariance = 0;
			for (const auto& [leftIndex, leftCoefficient] :
				 parameters[row].stateTerms)
			for (const auto& [rightIndex, rightCoefficient] :
				 parameters[column].stateTerms)
			{
				covariance += leftCoefficient * rightCoefficient *
					state.P(leftIndex, rightIndex);
			}
			rawCovariance(row, column) = covariance;
			rawCovariance(column, row) = covariance;
		}
	}

	// Integer alignment contributes exact constants that are absent from the
	// KF state terms.  The product rows are therefore the authoritative raw
	// frontend means, while stateTerms remain the authoritative covariance
	// Jacobian.
	auto matchingProducts = [&](const ProductCovarianceParameter& parameter)
		-> vector<ZhangInternalProduct*>
	{
		vector<ZhangInternalProduct*> matches;
		for (auto& product : epochProducts)
		{
			if (product.solution != solution
			 || product.satellite != parameter.satellite)
			{
				continue;
			}
			if (parameter.parameter == "CLOCK"
			 || product.observable == parameter.observable)
			{
				matches.push_back(&product);
			}
		}
		return matches;
	};
	auto failGaugeTransaction = [&](const string& reason)
	{
		const string fullReason = reason.rfind("REAL_GAUGE_", 0) == 0
			? reason : "REAL_GAUGE_" + reason;
		for (auto& product : epochProducts)
		{
			if (product.solution != solution)
			{
				continue;
			}
			product.numeric_valid = false;
			product.integer_precision_valid = false;
			product.integer_valid = false;
			product.continuity_valid = false;
			product.ppp_usable = false;
			product.pppar_usable = false;
			product.dual_frequency_ar_valid = false;
			product.ar_valid = false;
			product.invalid_reason = fullReason;
		}
		BOOST_LOG_TRIVIAL(error)
			<< "ZHANG_HYBRID_REAL_GAUGE_TRANSACTION time="
			<< state.time.to_string(0)
			<< " solution=" << solution
			<< " status=ABORTED reason=" << fullReason
			<< " covariance_written=0";
	};
	string parameterFailure;
	for (int index = 0; index < dimension; index++)
	{
		const auto matches = matchingProducts(parameters[index]);
		if (matches.empty())
		{
			parameterFailure = "REAL_GAUGE_PARAMETER_MISSING";
			break;
		}
		const bool clock = parameters[index].parameter == "CLOCK";
		const double authoritative = clock
			? matches.front()->clock_m : matches.front()->phase_m;
		if (!std::isfinite(authoritative))
		{
			parameterFailure = "REAL_GAUGE_PARAMETER_NONFINITE";
			break;
		}
		if (clock)
		{
			for (const auto* product : matches)
			{
				if (!std::isfinite(product->clock_m)
				 || std::abs(product->clock_m - authoritative) > 1e-10)
				{
					parameterFailure =
						"REAL_GAUGE_CLOCK_REPLICA_INCONSISTENT";
					break;
				}
			}
			if (!parameterFailure.empty())
			{
				break;
			}
		}
		rawMean(index) = authoritative;
	}
	if (!parameterFailure.empty())
	{
		failGaugeTransaction(parameterFailure);
		return;
	}

	MatrixXd transform = MatrixXd::Identity(dimension, dimension);
	VectorXd offset = VectorXd::Zero(dimension);
	vector<int> gaugeGenerations(dimension, 0);
	map<tuple<E_Sys, string, E_ObsCode>, vector<int>> blocks;
	for (int index = 0; index < dimension; index++)
	{
		blocks[{parameters[index].satellite.sys,
			parameters[index].parameter,
			parameters[index].observable}].push_back(index);
	}
	if (acsConfig.zhangPppAr.product_mode == "HOU_OSB_LIKE")
	{
		using RealGaugeKey = tuple<string, E_Sys, string, E_ObsCode>;
		ZhangHybridStableFrontend frontendController;
		auto frontendCandidate = frontendController.prepare(
			hybridRealGaugeTransports);
		int evaluatedGaugeBlocks = 0;
		string transactionFailure;
		for (const auto& [blockKey, indices] : blocks)
		{
			const auto& [system, parameter, observable] = blockKey;
			vector<SatSys> blockSatellites;
			vector<string> physicalSegments;
			VectorXd blockMean(indices.size());
			MatrixXd blockCovariance(indices.size(), indices.size());
			for (int row = 0; row < static_cast<int>(indices.size()); row++)
			{
				const int globalRow = indices[row];
				blockSatellites.push_back(parameters[globalRow].satellite);
				blockMean(row) = rawMean(globalRow);
				const auto status = satelliteDatumManager(
					system, observable == E_ObsCode::NONE
						? acsConfig.zhangPppAr.baseline_observables[system].front()
						: observable).status(
						parameters[globalRow].satellite, false);
				physicalSegments.push_back(
					parameter == "CLOCK"
					? "CLOCK-" + parameters[globalRow].satellite.id()
					: zhangHybridPhaseProductSegmentId(
						parameters[globalRow].satellite, observable,
						status.phaseSegment));
				for (int column = 0;
					 column < static_cast<int>(indices.size()); column++)
				{
					blockCovariance(row, column) =
						rawCovariance(globalRow, indices[column]);
				}
			}
			const RealGaugeKey gaugeKey =
				{solution, system, parameter, observable};
			auto& candidateGauge = frontendCandidate.preparedState[gaugeKey];
			const auto transported = candidateGauge.transport(
				blockSatellites, physicalSegments, blockMean, blockCovariance);
			if (!transported.valid)
			{
				transactionFailure = transported.failureReason;
				break;
			}
			evaluatedGaugeBlocks++;
			BOOST_LOG_TRIVIAL(info)
				<< "ZHANG_HYBRID_REAL_GAUGE time=" << state.time.to_string(0)
				<< " solution=" << solution
				<< " system=" << enum_to_string(system)
				<< " parameter=" << parameter
				<< " observable=" << enum_to_string(observable)
				<< " generation=" << transported.generation
				<< " new_generation=" << transported.newGeneration
				<< " overlap_count=" << transported.overlapCount
				<< " common_shift_m=" << transported.commonShiftMetres
				<< " covariance_transform=FULL_AFFINE";
			for (int row = 0; row < static_cast<int>(indices.size()); row++)
			{
				const int globalRow = indices[row];
				transform.row(globalRow).setZero();
				for (int column = 0;
					 column < static_cast<int>(indices.size()); column++)
				{
					transform(globalRow, indices[column]) =
						transported.transform(row, column);
				}
				offset(globalRow) = transported.affineOffset(row);
				gaugeGenerations[globalRow] = transported.generation;
			}
		}
		if (!transactionFailure.empty())
		{
			frontendController.validateRealGauge(frontendCandidate, false);
			frontendController.rollback(frontendCandidate);
			failGaugeTransaction(transactionFailure);
			return;
		}
		frontendController.validateIntegerAlignment(frontendCandidate, true);
		frontendController.validateRealGauge(frontendCandidate, true);
		frontendController.validateComponentConsistency(frontendCandidate, true);
		frontendController.validateMetadata(frontendCandidate, true);
		if (!frontendController.commit(
			hybridRealGaugeTransports, frontendCandidate))
		{
			failGaugeTransaction("HYBRID_STABLE_FRONTEND_COMMIT_REJECTED_" +
				frontendCandidate.failureReason);
			return;
		}
		BOOST_LOG_TRIVIAL(info)
			<< "ZHANG_HYBRID_REAL_GAUGE_TRANSACTION time="
			<< state.time.to_string(0)
			<< " solution=" << solution
			<< " status=COMMITTED blocks=" << evaluatedGaugeBlocks
			<< " controller=ZHANG_HYBRID_STABLE_FRONTEND"
			<< " covariance_transform=FULL_AFFINE";
	}
	const VectorXd canonicalMean = transform * rawMean + offset;
	MatrixXd covariance = transform * rawCovariance * transform.transpose();
	covariance = 0.5 * (covariance + covariance.transpose());

	map<ProductCovarianceKey, int> parameterIndex;
	for (int index = 0; index < dimension; index++)
	{
		parameterIndex[{parameters[index].satellite,
			parameters[index].parameter,
			parameters[index].observable}] = index;
	}
	for (auto& product : epochProducts)
	{
		if (product.solution != solution)
		{
			continue;
		}
		auto clock = parameterIndex.find(
			{product.satellite, "CLOCK", E_ObsCode::NONE});
		auto phase = parameterIndex.find(
			{product.satellite, "PHASE", product.observable});
		if (clock == parameterIndex.end() || phase == parameterIndex.end())
		{
			product.numeric_valid = false;
			product.integer_precision_valid = false;
			product.integer_valid = false;
			product.continuity_valid = false;
			product.ppp_usable = false;
			product.pppar_usable = false;
			product.dual_frequency_ar_valid = false;
			product.ar_valid = false;
			product.invalid_reason = "REAL_GAUGE_PARAMETER_MISSING";
			continue;
		}
		const int clockIndex = clock->second;
		const int phaseIndex = phase->second;
		product.clock_m = canonicalMean(clockIndex);
		product.phase_m = canonicalMean(phaseIndex);
		product.clock_sigma_m = std::sqrt(std::max(0.0,
			covariance(clockIndex, clockIndex)));
		product.phase_sigma_m = std::sqrt(std::max(0.0,
			covariance(phaseIndex, phaseIndex)));
		product.clock_phase_covariance_m2 =
			covariance(clockIndex, phaseIndex);
		product.correction_m = product.clock_m - product.phase_m;
		const double correctionVariance =
			covariance(clockIndex, clockIndex)
			+ covariance(phaseIndex, phaseIndex)
			- 2 * covariance(clockIndex, phaseIndex);
		product.correction_sigma_m =
			std::sqrt(std::max(0.0, correctionVariance));
		product.real_gauge_generation = std::max(
			gaugeGenerations[clockIndex], gaugeGenerations[phaseIndex]);
		const bool finite = std::isfinite(product.clock_m)
			&& std::isfinite(product.phase_m)
			&& std::isfinite(product.correction_m)
			&& std::isfinite(product.clock_sigma_m)
			&& std::isfinite(product.phase_sigma_m)
			&& std::isfinite(product.correction_sigma_m)
			&& std::isfinite(product.clock_phase_covariance_m2);
		const bool covarianceValid = covariance(clockIndex, clockIndex) >= -1e-10
			&& covariance(phaseIndex, phaseIndex) >= -1e-10
			&& correctionVariance >= -1e-10;
		product.numeric_valid = product.numeric_valid && finite && covarianceValid;
		const bool precisionPass =
			acsConfig.zhangPppAr.maximum_pppar_correction_sigma_m <= 0
			|| product.correction_sigma_m <=
				acsConfig.zhangPppAr.maximum_pppar_correction_sigma_m;
		if (!precisionPass)
		{
			product.integer_precision_valid = false;
			product.integer_valid = false;
			product.invalid_reason = "CANONICAL_CORRECTION_SIGMA_EXCEEDED";
		}
		else if (!finite || !covarianceValid)
		{
			product.invalid_reason = "REAL_GAUGE_COVARIANCE_INVALID";
		}
	}

    if (!ensureProductCovarianceFileHeader())
	{
		return;
	}
    std::ofstream output(filename, std::ios::app);
    output << std::setprecision(17);
    for (int row = 0; row < dimension; row++)
    {
        for (int column = row; column < dimension; column++)
        {
            const auto& left  = parameters[row];
            const auto& right = parameters[column];
            output
                << static_cast<double>(state.time.bigTime) << ","
                << solution << ","
                << left.satellite.id() << ","
                << left.parameter << ","
                << enum_to_string(left.observable) << ","
                << right.satellite.id() << ","
                << right.parameter << ","
                << enum_to_string(right.observable) << ","
                << covariance(row, column) << "\n";
        }
    }
}

vector<string> splitCsv(const string& line)
{
    vector<string> fields;
    std::stringstream stream(line);
    string field;
    while (std::getline(stream, field, ','))
    {
        fields.push_back(field);
    }
    return fields;
}

bool loadProductCovarianceEpoch(long int targetEpoch, const string& solution)
{
    auto& reader = productCovarianceReader;
    const string& filename = acsConfig.zhangPppAr.product_covariance_filename;
    if (reader.cache.epoch == targetEpoch &&
        reader.cache.solution == solution)
    {
        return reader.cache.valid;
    }

    const bool reset = reader.filename != filename || !reader.stream.is_open() ||
        targetEpoch < reader.lastRequestedEpoch;
    if (reset)
    {
        reader = {};
        reader.filename = filename;
        reader.stream.open(filename);
        if (!reader.stream)
        {
            reader.cache.epoch = targetEpoch;
            reader.cache.solution = solution;
            reader.cache.failureReason = "COVARIANCE_FILE_UNAVAILABLE";
            return false;
        }
        string header;
        std::getline(reader.stream, header);
    }
    reader.lastRequestedEpoch = targetEpoch;
    reader.cache = {};
    reader.cache.epoch = targetEpoch;
    reader.cache.solution = solution;

    struct Entry
    {
        ProductCovarianceKey row;
        ProductCovarianceKey column;
        double covariance = 0;
    };
    vector<Entry> entries;
    set<ProductCovarianceKey> parameters;
    while (true)
    {
        string line;
        if (!reader.pendingLine.empty())
        {
            line = std::move(reader.pendingLine);
            reader.pendingLine.clear();
        }
        else if (!std::getline(reader.stream, line))
        {
            break;
        }
        const auto fields = splitCsv(line);
        if (fields.size() != 9)
        {
            continue;
        }
        long int epoch = 0;
        try
        {
            epoch = static_cast<long int>(std::llround(std::stold(fields[0])));
        }
        catch (...)
        {
            continue;
        }
        if (epoch < targetEpoch)
        {
            continue;
        }
        if (epoch > targetEpoch)
        {
            reader.pendingLine = std::move(line);
            break;
        }
        if (fields[1] != solution)
        {
            continue;
        }
        Entry entry;
        try
        {
            entry.row = {
                SatSys(fields[2].c_str()),
                fields[3],
                string_to_enum<E_ObsCode>(fields[4])};
            entry.column = {
                SatSys(fields[5].c_str()),
                fields[6],
                string_to_enum<E_ObsCode>(fields[7])};
            entry.covariance = std::stod(fields[8]);
        }
        catch (...)
        {
            reader.cache.failureReason = "COVARIANCE_PARSE_FAILURE";
            return false;
        }
        if (!std::isfinite(entry.covariance))
        {
            reader.cache.failureReason = "COVARIANCE_NONFINITE";
            return false;
        }
        parameters.insert(entry.row);
        parameters.insert(entry.column);
        entries.push_back(std::move(entry));
    }
    if (entries.empty() || parameters.empty())
    {
        reader.cache.failureReason = "COVARIANCE_EPOCH_SOLUTION_MISSING";
        return false;
    }

    int index = 0;
    for (const auto& parameter : parameters)
    {
        reader.cache.parameterIndex[parameter] = index++;
    }
    MatrixXd covariance = MatrixXd::Zero(index, index);
    for (const auto& entry : entries)
    {
        const int row = reader.cache.parameterIndex.at(entry.row);
        const int column = reader.cache.parameterIndex.at(entry.column);
        covariance(row, column) = entry.covariance;
        covariance(column, row) = entry.covariance;
    }
    covariance = 0.5 * (covariance + covariance.transpose());
    Eigen::SelfAdjointEigenSolver<MatrixXd> solver(covariance);
    if (solver.info() != Eigen::Success || !solver.eigenvalues().allFinite())
    {
        reader.cache.failureReason = "COVARIANCE_EIGENSOLVER_FAILURE";
        return false;
    }
    const double maximumEigenvalue = solver.eigenvalues().maxCoeff();
    const double minimumEigenvalue = solver.eigenvalues().minCoeff();
    const double negativeTolerance = std::max(
        1e-14, std::abs(maximumEigenvalue) * 1e-9);
    if (!(maximumEigenvalue > 0) || minimumEigenvalue < -negativeTolerance)
    {
        reader.cache.failureReason = "COVARIANCE_NOT_POSITIVE_SEMIDEFINITE";
        return false;
    }
    const double rankTolerance = std::max(1e-16, maximumEigenvalue * 1e-12);
    vector<int> retained;
    for (int eigen = 0; eigen < solver.eigenvalues().size(); eigen++)
    {
        if (solver.eigenvalues()(eigen) > rankTolerance)
        {
            retained.push_back(eigen);
        }
    }
    if (retained.empty())
    {
        reader.cache.failureReason = "COVARIANCE_ZERO_RANK";
        return false;
    }
    reader.cache.squareRoot.resize(index, retained.size());
    for (int column = 0; column < static_cast<int>(retained.size()); column++)
    {
        const int eigen = retained[column];
        reader.cache.squareRoot.col(column) = solver.eigenvectors().col(eigen) *
            std::sqrt(solver.eigenvalues()(eigen));
    }
    reader.cache.numericalRank = retained.size();
    reader.cache.valid = reader.cache.squareRoot.allFinite();
    reader.cache.failureReason = reader.cache.valid
        ? "NONE" : "COVARIANCE_FACTOR_NONFINITE";
    return reader.cache.valid;
}

bool loadProducts()
{
    const string& filename = acsConfig.zhangPppAr.product_filename;
    if (filename.empty())
    {
        return false;
    }
    if (loadedProductFilename == filename && !productMap.empty())
    {
        return true;
    }

    std::ifstream input(filename);
    if (!input)
    {
        BOOST_LOG_TRIVIAL(error)
            << "Unable to open Zhang internal product file " << filename;
        return false;
    }

    productMap.clear();
    string line;
    std::getline(input, line);
    while (std::getline(input, line))
    {
        auto fields = splitCsv(line);
        if (fields.size() != 19 && fields.size() != 23 &&
            fields.size() != 26 && fields.size() != 28 &&
            fields.size() != 33 && fields.size() != 34 &&
			fields.size() != 45 && fields.size() != 48 &&
			fields.size() != 49)
        {
            continue;
        }

        ZhangInternalProduct product;
        product.time.bigTime                  = std::stold(fields[0]);
        product.solution                      = fields[1];
        product.satellite                     = SatSys(fields[2].c_str());
        product.observable                    = string_to_enum<E_ObsCode>(fields[3]);
        product.clock_m                       = std::stod(fields[4]);
        product.clock_sigma_m                 = std::stod(fields[5]);
        product.phase_m                       = std::stod(fields[6]);
        product.phase_sigma_m                 = std::stod(fields[7]);
        product.clock_phase_covariance_m2     = std::stod(fields[8]);
        product.correction_m                  = std::stod(fields[9]);
        product.correction_sigma_m            = std::stod(fields[10]);
        product.discontinuity_counter         = std::stoi(fields[11]);
        product.integer_shift_cycles          = std::stoll(fields[12]);
        product.fractional_shift_cycles       = std::stod(fields[13]);
        product.datum_version                 = std::stoi(fields[14]);
        product.valid_from.bigTime            = std::stold(fields[15]);
        product.product_iod                   = std::stoi(fields[16]);
        product.reset_reason                  = fields[17];
        if (fields.size() == 28 || fields.size() == 33
		 || fields.size() == 34 || fields.size() == 45
		 || fields.size() == 48 || fields.size() == 49)
        {
            product.persistent_relation_known = std::stoi(fields[18]) != 0;
            product.current_alignment_state   = fields[19];
            product.integer_structure_valid  = std::stoi(fields[20]) != 0;
            product.integer_datum_continuous = std::stoi(fields[21]) != 0;
            product.integer_precision_valid  = std::stoi(fields[22]) != 0;
            product.integer_valid            = std::stoi(fields[23]) != 0;
            product.integer_component_id     = fields[24];
            product.integer_datum_id          = fields[25];
            product.valid_from.bigTime        = std::stold(fields[26]);
            if (fields.size() == 33)
            {
                product.numeric_valid = std::stoi(fields[28]) != 0;
                product.branch_valid  = std::stoi(fields[29]) != 0;
                product.ppp_usable    = std::stoi(fields[30]) != 0;
                product.pppar_usable  = std::stoi(fields[31]) != 0;
                product.invalid_reason = fields[32];
                product.continuity_valid = product.ppp_usable;
            }
            else if (fields.size() == 34 || fields.size() == 45
			      || fields.size() == 48 || fields.size() == 49)
            {
                product.numeric_valid = std::stoi(fields[28]) != 0;
                product.branch_valid  = std::stoi(fields[29]) != 0;
                product.continuity_valid = std::stoi(fields[30]) != 0;
                product.ppp_usable    = std::stoi(fields[31]) != 0;
                product.pppar_usable  = std::stoi(fields[32]) != 0;
                product.invalid_reason = fields[33];
				if (fields.size() == 45 || fields.size() == 48
				 || fields.size() == 49)
				{
					product.phase_product_segment_id = fields[34];
					product.integer_component_version = std::stoi(fields[35]);
					product.integer_alignment_generation = std::stoi(fields[36]);
					product.real_gauge_generation = std::stoi(fields[37]);
					product.integer_component_size = std::stoull(fields[38]);
					product.integer_component_rank = std::stoull(fields[39]);
					product.certified_relation_count = std::stoull(fields[40]);
					product.redundant_relation_count = std::stoull(fields[41]);
					product.cycle_closure_valid = std::stoi(fields[42]) != 0;
					product.dual_frequency_ar_valid = std::stoi(fields[43]) != 0;
					product.product_state = fields[44];
					if (fields.size() == 48 || fields.size() == 49)
					{
						product.discontinuity = std::stoi(fields[45]) != 0;
						product.ar_valid = std::stoi(fields[46]) != 0;
						product.support_segment_fingerprint = fields[47];
						if (fields.size() == 49)
						{
							product.backend_s_basis_generation =
								std::stoi(fields[48]);
						}
					}
				}
            }
        }

        else if (fields.size() == 26)
        {
            product.integer_structure_valid  = std::stoi(fields[18]) != 0;
            product.integer_datum_continuous = std::stoi(fields[19]) != 0;
            product.integer_precision_valid  = std::stoi(fields[20]) != 0;
            product.integer_valid            = std::stoi(fields[21]) != 0;
            product.integer_component_id     = fields[22];
            product.integer_datum_id          = fields[23];
            product.valid_from.bigTime        = std::stold(fields[24]);
            product.persistent_relation_known =
                product.integer_component_id != "UNRESOLVED";
            product.current_alignment_state =
                product.integer_datum_continuous
                    ? "CURRENT_ALIGNMENT_VALID"
                    : (product.persistent_relation_known
                           ? "CURRENT_ALIGNMENT_LOST"
                           : "CURRENT_ALIGNMENT_PENDING");
        }
        else
        {
            product.integer_valid             = std::stoi(fields[18]) != 0;
            product.persistent_relation_known = product.integer_valid;
            product.current_alignment_state   = product.integer_valid
                ? "CURRENT_ALIGNMENT_VALID"
                : "CURRENT_ALIGNMENT_PENDING";
            product.integer_structure_valid   = product.integer_valid;
            product.integer_datum_continuous  = product.integer_valid;
            product.integer_precision_valid   = product.integer_valid;
            if (fields.size() == 23)
            {
                product.integer_component_id  = fields[19];
                product.integer_datum_id      = fields[20];
                product.valid_from.bigTime    = std::stold(fields[21]);
            }
        }

        if (fields.size() != 33 && fields.size() != 34
		 && fields.size() != 45 && fields.size() != 48
		 && fields.size() != 49)
        {
            product.numeric_valid =
                std::isfinite(product.clock_m) &&
                std::isfinite(product.phase_m) &&
                std::isfinite(product.correction_m) &&
                std::isfinite(product.clock_sigma_m) &&
                std::isfinite(product.phase_sigma_m) &&
                std::isfinite(product.correction_sigma_m);
            product.branch_valid = product.numeric_valid;
            product.continuity_valid = product.numeric_valid;
            product.ppp_usable = product.numeric_valid;
            product.pppar_usable =
                product.ppp_usable && product.integer_valid;
			product.ar_valid = product.pppar_usable;
			product.dual_frequency_ar_valid =
				product.dual_frequency_ar_valid && product.pppar_usable;
			product.discontinuity =
				product.discontinuity_counter >
					acsConfig.zhangPppAr.initial_discontinuity_counter
				&& product.valid_from != GTime::noTime()
				&& std::abs((product.time - product.valid_from).to_double()) < 1e-3;
            product.invalid_reason = product.numeric_valid
                ? "LEGACY_PRODUCT"
                : "LEGACY_NUMERIC_FAILURE";
		}

        ProductLookupKey key{
            static_cast<long int>(std::llround(product.time.bigTime)),
            product.satellite,
            product.observable,
            product.solution
        };
        productMap[key] = product;
    }

    loadedProductFilename = filename;
    BOOST_LOG_TRIVIAL(info)
        << "Loaded " << productMap.size()
        << " Zhang internal product records from " << filename;
    return !productMap.empty();
}

bool resetUserPhaseBlock(
    Trace&       trace,
    KFState&     kfState,
    const string& receiver,
    E_Sys        sys,
    E_ObsCode    code,
    const string& reason
)
{
    map<KFKey, map<KFKey, double>> transform;
    bool removed = false;

    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        bool targetReceiverPhase =
            key.type == KF::PHASE_BIAS &&
            key.str == receiver &&
            key.Sat.sys == sys &&
            key.num == userPhaseCoordinateNumber(sys, code);
        bool targetAmbiguity =
            key.type == KF::AMBIGUITY &&
            key.str == receiver &&
            key.Sat.sys == sys &&
            key.num == userPhaseCoordinateNumber(sys, code);

        if (targetReceiverPhase || targetAmbiguity)
        {
            removed = true;
            continue;
        }
        transform[key][key] = 1;
    }

    if (!removed)
    {
        return true;
    }

    return kfState.applyStateTransform(
        trace,
        transform,
        "Zhang held-out user phase reset: " + reason
    );
}

bool resetUserAmbiguity(
    Trace&        trace,
    KFState&      kfState,
    const string& receiver,
    const SatSys& satellite,
    E_ObsCode     code,
    const string& reason
)
{
    KFKey ambiguityKey =
        userAmbiguityKey(receiver, satellite, code);
    if (kfState.kfIndexMap.find(ambiguityKey) == kfState.kfIndexMap.end())
    {
        return true;
    }

    map<KFKey, map<KFKey, double>> transform;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        if (key == ambiguityKey)
        {
            continue;
        }
        transform[key][key] = 1;
    }

    return kfState.applyStateTransform(
        trace,
        transform,
        "Zhang held-out user ambiguity reset: " + reason
    );
}

bool transformUserReference(
    Trace&        trace,
    KFState&      kfState,
    const string& receiver,
    E_Sys         sys,
    E_ObsCode     code,
    const SatSys& oldReference,
    const SatSys& newReference
)
{
    if (oldReference == newReference)
    {
        return true;
    }

    const double lambda = userPhaseCoordinateWavelength(sys, code);
    KFKey newReferenceAmbiguity =
        userAmbiguityKey(receiver, newReference, code);
    if (lambda <= 0 ||
        kfState.kfIndexMap.find(newReferenceAmbiguity) == kfState.kfIndexMap.end())
    {
        return false;
    }

    auto physicalPredictions = [&](const KFState& state)
    {
        map<SatSys, pair<double, double>> predictions;
        KFKey phaseKey;
        phaseKey.type = KF::PHASE_BIAS;
        phaseKey.str = receiver;
        phaseKey.Sat = SatSys(sys, 0);
        phaseKey.num = userPhaseCoordinateNumber(sys, code);
        auto phase = state.kfIndexMap.find(phaseKey);
        if (phase == state.kfIndexMap.end())
        {
            return predictions;
        }
        set<SatSys> satellites{oldReference, newReference};
        for (const auto& [key, index] : state.kfIndexMap)
        {
            if (key.type == KF::AMBIGUITY && key.str == receiver &&
                key.Sat.sys == sys &&
                key.num == userPhaseCoordinateNumber(sys, code))
            {
                satellites.insert(key.Sat);
            }
        }
        for (const SatSys& satellite : satellites)
        {
            VectorXd row = VectorXd::Zero(state.x.size());
            row(phase->second) = 1;
            auto ambiguity = state.kfIndexMap.find(
                userAmbiguityKey(receiver, satellite, code));
            if (ambiguity != state.kfIndexMap.end())
            {
                row(ambiguity->second) = lambda;
            }
            predictions[satellite] = {
                row.dot(state.x),
                row.dot(state.P * row)
            };
        }
        return predictions;
    };
    const auto beforePredictions = physicalPredictions(kfState);

    map<KFKey, map<KFKey, double>> transform;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        bool targetReceiverPhase =
            key.type == KF::PHASE_BIAS &&
            key.str == receiver &&
            key.Sat.sys == sys &&
            key.num == userPhaseCoordinateNumber(sys, code);
        bool targetAmbiguity =
            key.type == KF::AMBIGUITY &&
            key.str == receiver &&
            key.Sat.sys == sys &&
            key.num == userPhaseCoordinateNumber(sys, code);

        if (!targetReceiverPhase && !targetAmbiguity)
        {
            transform[key][key] = 1;
            continue;
        }

        if (targetReceiverPhase)
        {
            transform[key][key] = 1;
            transform[key][newReferenceAmbiguity] = lambda;
            continue;
        }

        if (key.Sat == newReference)
        {
            continue;
        }

        transform[key][key] = 1;
        transform[key][newReferenceAmbiguity] = -1;
    }

    // The old reference ambiguity is absent from the old S-basis by
    // construction, so it can never be created from the loop above.  Add the
    // replacement coordinate explicitly: N_old,new = -N_new,old.  Omitting
    // this row drops one physical ambiguity direction at every reference
    // exchange and changes the complete phase prediction.
    KFKey oldReferenceDestination =
        userAmbiguityKey(receiver, oldReference, code);
    transform[oldReferenceDestination][newReferenceAmbiguity] = -1;

    KFState transformedState = kfState;
    if (!transformedState.applyStateTransform(
            trace,
            transform,
            "Zhang held-out user ambiguity-reference exchange candidate"))
    {
        return false;
    }
    transformedState.P =
        0.5 * (transformedState.P + transformedState.P.transpose());
    Eigen::SelfAdjointEigenSolver<MatrixXd> covarianceSolver(transformedState.P);
    if (covarianceSolver.info() != Eigen::Success ||
        !covarianceSolver.eigenvalues().allFinite())
    {
        trace << "\nZHANG_E27_REFERENCE_TRANSFORM time="
              << kfState.time.to_string(0)
              << " receiver=" << receiver
              << " observable=" << enum_to_string(code)
              << " status=REJECTED reason=COVARIANCE_EIGENSOLVER_FAILED";
        return false;
    }
    const double covarianceScale = std::max(
        1.0, covarianceSolver.eigenvalues().cwiseAbs().maxCoeff());
    const double minimumEigenvalue = covarianceSolver.eigenvalues().minCoeff();
    if (minimumEigenvalue < -1e-9 * covarianceScale)
    {
        trace << "\nZHANG_E27_REFERENCE_TRANSFORM time="
              << kfState.time.to_string(0)
              << " receiver=" << receiver
              << " observable=" << enum_to_string(code)
              << " status=REJECTED reason=TRANSFORMED_COVARIANCE_NOT_PSD"
              << " minimum_eigenvalue=" << minimumEigenvalue
              << " scale=" << covarianceScale;
        return false;
    }
    if (minimumEigenvalue < 0)
    {
        transformedState.P = covarianceSolver.eigenvectors() *
            covarianceSolver.eigenvalues().cwiseMax(0).asDiagonal() *
            covarianceSolver.eigenvectors().transpose();
        transformedState.P =
            0.5 * (transformedState.P + transformedState.P.transpose());
    }
    const auto afterPredictions = physicalPredictions(transformedState);
    double maximumMeanDifference = 0;
    double maximumVarianceRelativeDifference = 0;
    for (const auto& [satellite, before] : beforePredictions)
    {
        auto after = afterPredictions.find(satellite);
        if (after == afterPredictions.end())
        {
            continue;
        }
        maximumMeanDifference = std::max(
            maximumMeanDifference, std::abs(before.first - after->second.first));
        maximumVarianceRelativeDifference = std::max(
            maximumVarianceRelativeDifference,
            std::abs(before.second - after->second.second) /
                std::max({1.0, std::abs(before.second),
                          std::abs(after->second.second)}));
    }
    if (maximumMeanDifference > 1e-8 ||
        maximumVarianceRelativeDifference > 1e-8)
    {
        trace << "\nZHANG_E27_REFERENCE_TRANSFORM time="
              << kfState.time.to_string(0)
              << " receiver=" << receiver
              << " observable=" << enum_to_string(code)
              << " status=REJECTED reason=PHYSICAL_PREDICTION_CHANGED"
              << " maximum_mean_difference=" << maximumMeanDifference
              << " maximum_variance_relative_difference="
              << maximumVarianceRelativeDifference;
        return false;
    }
    kfState = transformedState;
    trace << "\nZHANG_E27_REFERENCE_TRANSFORM time="
          << kfState.time.to_string(0)
          << " receiver=" << receiver
          << " observable=" << enum_to_string(code)
          << " status=APPLIED"
          << " old=" << oldReference.id()
          << " new=" << newReference.id()
          << " minimum_eigenvalue=" << minimumEigenvalue
          << " covariance_scale=" << covarianceScale
          << " maximum_mean_difference=" << maximumMeanDifference
          << " maximum_variance_relative_difference="
          << maximumVarianceRelativeDifference;
    return true;
}
}  // namespace

ZhangPppArCheckpointResult exportZhangPppArCheckpointSection(
	const KFState& owner,
	const string& runtimeId,
	string& payload)
{
	std::lock_guard<std::mutex> checkpointLock(zhangPppArCheckpointMutex);
	ZhangPppArCheckpointResult result;
	payload.clear();
	if (!validCheckpointRuntimeId(runtimeId))
	{
		result.failureReason = "PPP_AR_CHECKPOINT_RUNTIME_ID_INVALID";
		return result;
	}
	const string boundRuntimeId = zhangPppArRuntimeId(owner);
	if (boundRuntimeId.empty() || boundRuntimeId != runtimeId)
	{
		result.failureReason = "PPP_AR_CHECKPOINT_OWNER_RUNTIME_ID_MISMATCH";
		return result;
	}
	string resolvedRuntimeId;
	if (!resolveZhangPppArRuntimeOwner(owner, resolvedRuntimeId)
	 || resolvedRuntimeId != runtimeId)
	{
		result.failureReason = "PPP_AR_CHECKPOINT_RUNTIME_OBJECT_CONFLICT";
		return result;
	}
	if (!zhangPppArCheckpointRuntimeId.empty()
	 && zhangPppArCheckpointRuntimeId != runtimeId)
	{
		result.failureReason = "PPP_AR_CHECKPOINT_RUNTIME_OWNER_CONFLICT";
		return result;
	}
	if (e18ConfiguredFactorCaptureStates.count(runtimeId) != 0)
	{
		auto binding = e18RuntimeObjectBindings.find(runtimeId);
		if (binding == e18RuntimeObjectBindings.end()
		 || binding->second != &owner)
		{
			result.failureReason =
				"PPP_AR_CHECKPOINT_RUNTIME_OBJECT_CONFLICT";
			return result;
		}
	}

	ZhangPppArCheckpointEnvelope snapshot;
	snapshot.runtimeId = runtimeId;
	if (auto found = pendingProductTransitions.find(runtimeId);
		found != pendingProductTransitions.end())
	{
		for (const auto& transition : found->second)
		{
			snapshot.pendingTransitions.push_back(
				checkpointPendingTransition(transition));
		}
	}
	if (auto found = pendingSnapshotPins.find(runtimeId);
		found != pendingSnapshotPins.end())
	{
		snapshot.hasPendingSnapshotPins = true;
		snapshot.pendingPins = checkpointPendingPins(found->second);
	}
	snapshot.factorCaptureConfigured =
		e18ConfiguredFactorCaptureStates.count(runtimeId) != 0;
	if (auto found = e18FactorCaptureBuffers.find(runtimeId);
		found != e18FactorCaptureBuffers.end())
	{
		snapshot.hasFactorCapture = true;
		snapshot.factorCapture = checkpointFactorCapture(
			found->second.checkpointReplay());
		result.capturedFactorEvents = snapshot.factorCapture.events.size();
	}
	if (auto found = e18PersistentProductDatumRegistries.find(runtimeId);
		found != e18PersistentProductDatumRegistries.end())
	{
		snapshot.hasPersistentProductDatumRegistry = true;
		snapshot.persistentProductDatumRegistry =
			found->second.checkpointState();
		result.persistentDatumStates =
			snapshot.persistentProductDatumRegistry.datumStates.size();
	}
	for (const auto& [key, row] : e27RawNoiseRows)
	{
		if (key.runtimeId == runtimeId)
		{
			snapshot.e27RawRows.push_back({
				key.epoch, key.receiver, key.system, key.satellite, row});
		}
	}
	if (auto found = e27JointNoiseRuntimes.find(runtimeId);
		found != e27JointNoiseRuntimes.end())
	{
		snapshot.hasE27JointNoiseRuntime = true;
		snapshot.e27JointNoiseRuntime =
			checkpointE27JointRuntime(found->second);
		result.e27SensitivityRows = found->second.sensitivities.size();
	}

	for (const auto& [key, state] : continuityMap)
	{
		snapshot.continuity.emplace(key, checkpointContinuity(state));
	}
	snapshot.productDatumVersionTrackers = houProductDatumVersionTrackers;
	snapshot.physicalFunctionalIdentities =
		houProductPhysicalFunctionalIdentities;
	for (const auto& [key, functional] : houProductPhysicalFunctionals)
	{
		snapshot.physicalFunctionals[key] =
			checkpointFunctional(functional);
	}
	snapshot.sBasisFingerprints = houProductSBasisFingerprints;
	snapshot.phaseSegmentIdentities = houProductPhaseSegmentIdentities;
	snapshot.treeAlignmentCycles = houProductTreeAlignmentCycles;
	snapshot.snapshotIdentities = houProductSnapshotIdentities;
	for (const auto& [key, state] : globalContinuityMap)
	{
		snapshot.globalContinuity.emplace(
			key, checkpointGlobalContinuity(state));
	}
	for (const auto& [ignored, manager] : satelliteDatumManagers)
	{
		snapshot.satelliteDatumManagers.push_back(manager.checkpointState());
	}
	for (const auto& [key, gauge] : hybridRealGaugeTransports)
	{
		const auto& [solution, system, parameter, observable] = key;
		snapshot.hybridRealGauges.push_back({
			solution, system, parameter, observable, gauge.checkpointState()});
	}
	snapshot.promotionEvidence = promotionEvidence;
	snapshot.relinkEvidence = relinkEvidence;
	for (const auto& [key, history] : productHistoryMap)
	{
		snapshot.productHistory.emplace(
			key, checkpointProductHistory(history));
	}
	for (const auto& [key, state] : userReferenceMap)
	{
		if (key.runtimeId == runtimeId)
		{
			snapshot.userReferences.emplace(key, state);
		}
	}
	for (const auto& [key, reference] : userDualReferenceMap)
	{
		if (key.runtimeId == runtimeId)
		{
			snapshot.userDualReferences.emplace(key, reference);
		}
	}
	try
	{
		std::ostringstream output(std::ios::binary | std::ios::out);
		boost::archive::binary_oarchive archive(
			output, boost::archive::no_header);
		archive << snapshot;
		payload = output.str();
	}
	catch (const std::exception& exception)
	{
		payload.clear();
		result.failureReason =
			string("PPP_AR_CHECKPOINT_SERIALIZE_FAILED:") + exception.what();
		return result;
	}
	if (payload.empty())
	{
		result.failureReason = "PPP_AR_CHECKPOINT_EMPTY_PAYLOAD";
		return result;
	}
	zhangPppArCheckpointRuntimeId = runtimeId;
	result.pendingTransitions = snapshot.pendingTransitions.size();
	result.pendingSnapshotPins = snapshot.hasPendingSnapshotPins
		? snapshot.pendingPins.identities.size() : 0;
	result.e27RawNoiseRows = snapshot.e27RawRows.size();
	result.userReferenceStates = snapshot.userReferences.size();
	result.userDualReferenceStates = snapshot.userDualReferences.size();
	result.valid = true;
	return result;
}

ZhangPppArCheckpointResult importZhangPppArCheckpointSection(
	KFState& owner,
	const string& runtimeId,
	const string& payload,
	bool validateOnly,
	int expectedStateDimension)
{
	std::lock_guard<std::mutex> checkpointLock(zhangPppArCheckpointMutex);
	ZhangPppArCheckpointResult result;
	if (!validCheckpointRuntimeId(runtimeId))
	{
		result.failureReason = "PPP_AR_CHECKPOINT_RUNTIME_ID_INVALID";
		return result;
	}
	if (payload.empty())
	{
		result.failureReason = "PPP_AR_CHECKPOINT_EMPTY_PAYLOAD";
		return result;
	}
	if (!zhangPppArCheckpointRuntimeId.empty()
	 && zhangPppArCheckpointRuntimeId != runtimeId)
	{
		result.failureReason = "PPP_AR_CHECKPOINT_RUNTIME_OWNER_CONFLICT";
		return result;
	}
	const string boundRuntimeId = zhangPppArRuntimeId(owner);
	if (!boundRuntimeId.empty() && boundRuntimeId != runtimeId)
	{
		result.failureReason = "PPP_AR_CHECKPOINT_OWNER_RUNTIME_ID_MISMATCH";
		return result;
	}

	ZhangPppArCheckpointEnvelope snapshot;
	try
	{
		std::istringstream input(
			payload, std::ios::binary | std::ios::in);
		{
			boost::archive::binary_iarchive archive(
				input, boost::archive::no_header);
			archive >> snapshot;
		}
		if (input.peek() != std::char_traits<char>::eof())
		{
			result.failureReason = "PPP_AR_CHECKPOINT_TRAILING_BYTES";
			return result;
		}
	}
	catch (const std::exception& exception)
	{
		result.failureReason =
			string("PPP_AR_CHECKPOINT_DESERIALIZE_FAILED:") + exception.what();
		return result;
	}
	if (snapshot.schemaVersion != ZHANG_PPP_AR_CHECKPOINT_SCHEMA_VERSION)
	{
		result.failureReason = "PPP_AR_CHECKPOINT_SCHEMA_MISMATCH";
		return result;
	}
	if (snapshot.runtimeId != runtimeId)
	{
		result.failureReason = "PPP_AR_CHECKPOINT_RUNTIME_ID_MISMATCH";
		return result;
	}
	if ((!snapshot.hasPendingSnapshotPins
		 && !snapshot.pendingPins.identities.empty())
	 || (!snapshot.hasFactorCapture
		 && (snapshot.factorCapture.maximumEvents != 0
			 || !snapshot.factorCapture.initialKeys.empty()
			 || snapshot.factorCapture.initialMean.size() != 0
			 || snapshot.factorCapture.initialCovariance.size() != 0
			 || !snapshot.factorCapture.currentKeys.empty()
			 || snapshot.factorCapture.replayMean.size() != 0
			 || snapshot.factorCapture.replayCovariance.size() != 0
			 || !snapshot.factorCapture.events.empty()
			 || !snapshot.factorCapture.snapshotOperations.empty()
			 || !snapshot.factorCapture.physicalTargets.empty()
			 || !snapshot.factorCapture.unresolvedIntegerDatums.empty()
			 || !snapshot.factorCapture.retainedTargetBlocks.empty()
			 || snapshot.factorCapture.currentRetainedTargetBlock.targetCount != 0
			 || !snapshot.factorCapture.innovationScaleGroups.empty()
			 || snapshot.factorCapture.lastMeasurementPriorMean.size() != 0
			 || snapshot.factorCapture.lastMeasurementPriorCovariance.size() != 0))
	 || (!snapshot.hasPersistentProductDatumRegistry
		 && (!snapshot.persistentProductDatumRegistry.canonicalRelations.empty()
			 || !snapshot.persistentProductDatumRegistry.datumStates.empty()))
	 || (!snapshot.hasE27JointNoiseRuntime
		 && (!snapshot.e27JointNoiseRuntime.sensitivities.empty()
			 || !snapshot.e27JointNoiseRuntime.failureReason.empty())))
	{
		result.failureReason = "PPP_AR_CHECKPOINT_SECTION_PRESENCE_MISMATCH";
		return result;
	}
	const int checkpointStateDimension = expectedStateDimension >= 0
		? expectedStateDimension : owner.x.size();

	string failureReason;
	vector<ZhangPendingProductTransition> restoredTransitions;
	for (const auto& transition : snapshot.pendingTransitions)
	{
		ZhangPendingProductTransition restored;
		if (!restorePendingTransition(
			transition, restored, failureReason))
		{
			result.failureReason = failureReason;
			return result;
		}
		restoredTransitions.push_back(std::move(restored));
	}
	ZhangPendingSnapshotPins restoredPendingPins;
	if (snapshot.hasPendingSnapshotPins)
	{
		restoredPendingPins = restorePendingPins(snapshot.pendingPins);
		if (restoredPendingPins.identities.empty()
		 || !finiteCheckpointTimeValue(restoredPendingPins.eventTime)
		 || restoredPendingPins.eventTime == GTime::noTime()
		 || std::any_of(
			restoredPendingPins.identities.begin(),
			restoredPendingPins.identities.end(),
			[](const string& identity) { return identity.empty(); }))
		{
			result.failureReason = "PPP_AR_CHECKPOINT_INVALID_SNAPSHOT_PINS";
			return result;
		}
	}

	ZhangFactorCaptureBuffer restoredFactorCapture;
	const ZhangFactorCaptureRuntimeReplay factorCaptureReplay =
		restoreFactorCapture(snapshot.factorCapture);
	auto validFactorEvent = [](const ZhangCapturedFactorEvent& event)
	{
		return finiteCheckpointTimeValue(event.time)
			&& event.time != GTime::noTime()
			&& (event.kind == ZhangCapturedFactorKind::MEASUREMENT
				|| event.kind == ZhangCapturedFactorKind::STATE_TRANSITION
				|| event.kind ==
					ZhangCapturedFactorKind::EXACT_COORDINATE_TRANSFORM);
	};
	auto validSnapshotOperation = [](const ZhangCapturedSnapshotOperation& op)
	{
		return op.kind == ZhangCapturedSnapshotOperationKind::BIND_NEW_TARGETS
			|| op.kind == ZhangCapturedSnapshotOperationKind::RETAIN_TARGETS;
	};
	const bool factorTimesValid = std::all_of(
		factorCaptureReplay.events.begin(), factorCaptureReplay.events.end(),
		validFactorEvent)
		&& std::all_of(
			factorCaptureReplay.snapshotOperations.begin(),
			factorCaptureReplay.snapshotOperations.end(),
			validSnapshotOperation)
		&& std::all_of(
			factorCaptureReplay.physicalTargets.begin(),
			factorCaptureReplay.physicalTargets.end(), [](const auto& target)
			{
				return finiteCheckpointTimeValue(target.time)
					&& target.time != GTime::noTime();
			})
		&& std::all_of(
			factorCaptureReplay.unresolvedIntegerDatums.begin(),
			factorCaptureReplay.unresolvedIntegerDatums.end(), [](const auto& datum)
			{
				return finiteCheckpointTimeValue(datum.time)
					&& datum.time != GTime::noTime();
			})
		&& std::all_of(
			factorCaptureReplay.retainedTargetBlocks.begin(),
			factorCaptureReplay.retainedTargetBlocks.end(), [](const auto& block)
			{
				return finiteCheckpointTimeValue(block.time)
					&& block.time != GTime::noTime();
			})
		&& (factorCaptureReplay.events.empty()
			|| (finiteCheckpointTimeValue(
				factorCaptureReplay.lastMeasurementTime)
				&& factorCaptureReplay.lastMeasurementTime != GTime::noTime()))
		&& (factorCaptureReplay.currentRetainedTargetBlock.targetCount == 0
			|| (finiteCheckpointTimeValue(
				factorCaptureReplay.currentRetainedTargetBlock.time)
				&& factorCaptureReplay.currentRetainedTargetBlock.time
					!= GTime::noTime()));
	if (snapshot.hasFactorCapture && !factorTimesValid)
	{
		result.failureReason = "PPP_AR_CHECKPOINT_INVALID_FACTOR_TIME";
		return result;
	}
	if (snapshot.hasFactorCapture
	 && !restoredFactorCapture.restoreCheckpointReplay(
		factorCaptureReplay, &failureReason))
	{
		result.failureReason = failureReason;
		return result;
	}
	ZhangPersistentProductDatumRegistry restoredRegistry;
	if (snapshot.hasPersistentProductDatumRegistry
	 && !restoredRegistry.restoreCheckpointState(
		snapshot.persistentProductDatumRegistry, &failureReason))
	{
		result.failureReason = failureReason;
		return result;
	}

	map<ProductKey, ZhangPhaseContinuityState> restoredContinuity;
	for (const auto& [key, stored] : snapshot.continuity)
	{
		const auto state = restoreContinuity(stored);
		if (!validateProductKey(key) || state.counter < 0
		 || state.datumVersion < 0 || state.iod < 0
		 || state.stabilizationRemaining < 0
		 || !std::isfinite(state.fractionalShiftCycles)
		 || !finiteCheckpointTimeValue(state.validFrom)
		 || !finiteCheckpointTimeValue(state.lastEpoch))
		{
			result.failureReason = "PPP_AR_CHECKPOINT_INVALID_CONTINUITY_STATE";
			return result;
		}
		restoredContinuity.emplace(key, state);
	}
	auto validateProductStringMap = [&](const auto& values)
	{
		return std::all_of(values.begin(), values.end(), [](const auto& item)
		{
			return validateProductKey(item.first) && !item.second.empty();
		});
	};
	if (!validateProductStringMap(snapshot.physicalFunctionalIdentities)
	 || !validateProductStringMap(snapshot.sBasisFingerprints)
	 || !validateProductStringMap(snapshot.phaseSegmentIdentities)
	 || !validateProductStringMap(snapshot.snapshotIdentities))
	{
		result.failureReason = "PPP_AR_CHECKPOINT_INVALID_PRODUCT_IDENTITY_MAP";
		return result;
	}
	auto sameProductKeys = [](const auto& left, const auto& right)
	{
		return left.size() == right.size()
			&& std::all_of(left.begin(), left.end(), [&](const auto& item)
			{
				return right.find(item.first) != right.end();
			});
	};
	if (!sameProductKeys(
			snapshot.physicalFunctionals,
			snapshot.physicalFunctionalIdentities)
	 || !sameProductKeys(
			snapshot.physicalFunctionals, snapshot.sBasisFingerprints)
	 || !sameProductKeys(
			snapshot.physicalFunctionals, snapshot.phaseSegmentIdentities)
	 || !sameProductKeys(
			snapshot.physicalFunctionals, snapshot.snapshotIdentities))
	{
		result.failureReason =
			"PPP_AR_CHECKPOINT_PRODUCT_FUNCTIONAL_MAP_MISMATCH";
		return result;
	}
	for (const auto& [key, ignored] : snapshot.treeAlignmentCycles)
	{
		if (!validateProductKey(key))
		{
			result.failureReason = "PPP_AR_CHECKPOINT_INVALID_ALIGNMENT_MAP";
			return result;
		}
	}
	map<ProductKey, ZhangProductIntegerFunctional> restoredFunctionals;
	for (const auto& [key, functional] : snapshot.physicalFunctionals)
	{
		ZhangProductIntegerFunctional restored;
		if (!validateProductKey(key)
		 || !restoreFunctional(functional, restored, failureReason)
		 || restored.satellite != key.satellite)
		{
			result.failureReason = failureReason.empty()
				? "PPP_AR_CHECKPOINT_FUNCTIONAL_KEY_MISMATCH"
				: failureReason;
			return result;
		}
		restoredFunctionals.emplace(key, std::move(restored));
	}
	for (const auto& [system, tracker] :
		snapshot.productDatumVersionTrackers)
	{
		if (system == E_Sys::NONE || tracker.version < 0
		 || (!tracker.initialized && tracker.version != 0))
		{
			result.failureReason = "PPP_AR_CHECKPOINT_INVALID_DATUM_TRACKER";
			return result;
		}
	}
	map<std::pair<E_Sys, E_ObsCode>, GlobalContinuityState>
		restoredGlobalContinuity;
	for (const auto& [key, stored] : snapshot.globalContinuity)
	{
		const auto state = restoreGlobalContinuity(stored);
		if (key.first == E_Sys::NONE || key.second == E_ObsCode::NONE
		 || state.counter < 0 || state.datumVersion < 0 || state.iod < 0
		 || state.stabilizationRemaining < 0
		 || !finiteCheckpointTimeValue(state.validFrom))
		{
			result.failureReason = "PPP_AR_CHECKPOINT_INVALID_GLOBAL_CONTINUITY";
			return result;
		}
		restoredGlobalContinuity.emplace(key, state);
	}
	map<std::pair<E_Sys, E_ObsCode>, ZhangSatelliteDatumManager>
		restoredSatelliteManagers;
	for (const auto& managerSnapshot : snapshot.satelliteDatumManagers)
	{
		ZhangSatelliteDatumManager manager;
		if (!manager.restoreCheckpointState(managerSnapshot, &failureReason))
		{
			result.failureReason = failureReason;
			return result;
		}
		auto key = std::make_pair(
			managerSnapshot.system, managerSnapshot.observable);
		if (!restoredSatelliteManagers.emplace(key, std::move(manager)).second)
		{
			result.failureReason =
				"PPP_AR_CHECKPOINT_DUPLICATE_SATELLITE_DATUM_MANAGER";
			return result;
		}
	}
	map<tuple<string, E_Sys, string, E_ObsCode>,
		ZhangHybridRealGaugeTransport> restoredHybridRealGauges;
	for (const auto& entry : snapshot.hybridRealGauges)
	{
		if (entry.solution.empty() || entry.system == E_Sys::NONE
		 || (entry.parameter != "CLOCK" && entry.parameter != "PHASE")
		 || (entry.parameter == "CLOCK"
			 ? entry.observable != E_ObsCode::NONE
			 : entry.observable == E_ObsCode::NONE))
		{
			result.failureReason =
				"PPP_AR_CHECKPOINT_INVALID_HYBRID_REAL_GAUGE_KEY";
			return result;
		}
		ZhangHybridRealGaugeTransport gauge;
		if (!gauge.restoreCheckpointState(entry.gauge, &failureReason))
		{
			result.failureReason = failureReason;
			return result;
		}
		if (!restoredHybridRealGauges.emplace(
				std::make_tuple(entry.solution, entry.system,
					entry.parameter, entry.observable), std::move(gauge)).second)
		{
			result.failureReason =
				"PPP_AR_CHECKPOINT_DUPLICATE_HYBRID_REAL_GAUGE";
			return result;
		}
	}
	auto validateEvidence = [](const auto& values)
	{
		return std::all_of(values.begin(), values.end(), [](const auto& item)
		{
			const auto& key = item.first;
			const auto& value = item.second;
			return key.system != E_Sys::NONE
				&& key.observable != E_ObsCode::NONE
				&& key.a.sys == key.system && key.a.prn > 0
				&& key.b.sys == key.system && key.b.prn > 0
				&& key.a != key.b
				&& key.segmentA >= 0 && key.segmentB >= 0
				&& value.lastEpoch > 0 && value.confirmations >= 0;
		});
	};
	if (!validateEvidence(snapshot.promotionEvidence)
	 || !validateEvidence(snapshot.relinkEvidence))
	{
		result.failureReason = "PPP_AR_CHECKPOINT_INVALID_RELATION_EVIDENCE";
		return result;
	}
	map<ProductHistoryKey, ProductHistory> restoredProductHistory;
	for (const auto& [key, stored] : snapshot.productHistory)
	{
		const auto history = restoreProductHistory(stored);
		if (key.solution.empty() || key.satellite.prn <= 0
		 || key.satellite.sys == E_Sys::NONE
		 || key.observable == E_ObsCode::NONE
		 || !std::isfinite(history.correction)
		 || !finiteCheckpointTimeValue(history.time)
		 || history.time == GTime::noTime()
		 || history.discontinuityCounter < 0 || history.datumVersion < 0)
		{
			result.failureReason = "PPP_AR_CHECKPOINT_INVALID_PRODUCT_HISTORY";
			return result;
		}
		restoredProductHistory.emplace(key, history);
	}
	map<UserReferenceKey, UserReferenceState> restoredUserReferences;
	for (const auto& [key, state] : snapshot.userReferences)
	{
		const bool referenceValid = state.reference.prn > 0
			&& state.reference.sys == key.sys;
		const bool satelliteDatumValid = std::all_of(
			state.satelliteDatum.begin(), state.satelliteDatum.end(),
			[&](const auto& item)
			{
				return item.first.sys == key.sys && item.first.prn > 0
					&& item.second.first >= 0 && item.second.second >= 0;
			});
		if (key.runtimeId != runtimeId || key.receiver.empty()
		 || key.sys == E_Sys::NONE || key.observable == E_ObsCode::NONE
		 || !referenceValid || state.productCounter < 0
		 || state.datumVersion < 0 || !satelliteDatumValid)
		{
			result.failureReason =
				"PPP_AR_CHECKPOINT_INVALID_USER_REFERENCE";
			return result;
		}
		restoredUserReferences.emplace(key, state);
	}
	map<UserDualReferenceKey, SatSys> restoredUserDualReferences;
	for (const auto& [key, reference] : snapshot.userDualReferences)
	{
		if (key.runtimeId != runtimeId || key.receiver.empty()
		 || key.system == E_Sys::NONE || reference.prn <= 0
		 || reference.sys != key.system)
		{
			result.failureReason =
				"PPP_AR_CHECKPOINT_INVALID_USER_DUAL_REFERENCE";
			return result;
		}
		restoredUserDualReferences.emplace(key, reference);
	}

	map<E27RawNoiseRowKey, E27RawNoiseRow> restoredRawRows;
	for (const auto& raw : snapshot.e27RawRows)
	{
		const set<string> uniqueStampedKeys(
			raw.row.stampedKeys.begin(), raw.row.stampedKeys.end());
		if (raw.epoch.empty() || raw.receiver.empty()
		 || raw.system == E_Sys::NONE || raw.satellite.sys != raw.system
		 || raw.satellite.prn <= 0
		 || raw.row.stampedKeys.empty()
		 || uniqueStampedKeys.size() != raw.row.stampedKeys.size()
		 || uniqueStampedKeys.count("") != 0
		 || raw.row.stampedKeys.size() !=
			static_cast<std::size_t>(raw.row.coefficients.size())
		 || raw.row.coefficients.size() != raw.row.variances.size()
		 || !raw.row.coefficients.allFinite()
		 || !raw.row.variances.allFinite()
		 || (raw.row.variances.array() <= 0).any())
		{
			result.failureReason = "PPP_AR_CHECKPOINT_INVALID_E27_RAW_ROW";
			return result;
		}
		E27RawNoiseRowKey key{
			runtimeId, raw.epoch, raw.receiver, raw.system, raw.satellite};
		if (!restoredRawRows.emplace(key, raw.row).second)
		{
			result.failureReason = "PPP_AR_CHECKPOINT_DUPLICATE_E27_RAW_ROW";
			return result;
		}
	}
	E27JointNoiseRuntime restoredJointNoiseRuntime =
		restoreE27JointRuntime(snapshot.e27JointNoiseRuntime);
	if (snapshot.hasE27JointNoiseRuntime)
	{
		if (!restoredJointNoiseRuntime.failureReason.empty()
		 && !restoredJointNoiseRuntime.sensitivities.empty())
		{
			result.failureReason =
				"PPP_AR_CHECKPOINT_E27_FAILURE_WITH_SENSITIVITIES";
			return result;
		}
		for (const auto& [identity, sensitivity] :
			restoredJointNoiseRuntime.sensitivities)
		{
			if (identity.empty()
			 || !finiteCheckpointTimeValue(sensitivity.time)
			 || sensitivity.time == GTime::noTime()
			 || sensitivity.stateDerivative.size() != checkpointStateDimension
			 || !sensitivity.stateDerivative.allFinite()
			 || !std::isfinite(sensitivity.variance)
			 || sensitivity.variance <= 0)
			{
				result.failureReason =
					"PPP_AR_CHECKPOINT_INVALID_E27_SENSITIVITY";
				return result;
			}
		}
	}

	result.pendingTransitions = snapshot.pendingTransitions.size();
	result.pendingSnapshotPins = snapshot.hasPendingSnapshotPins
		? snapshot.pendingPins.identities.size() : 0;
	result.capturedFactorEvents = snapshot.hasFactorCapture
		? snapshot.factorCapture.events.size() : 0;
	result.persistentDatumStates =
		snapshot.hasPersistentProductDatumRegistry
		? snapshot.persistentProductDatumRegistry.datumStates.size() : 0;
	result.e27RawNoiseRows = snapshot.e27RawRows.size();
	result.e27SensitivityRows = snapshot.hasE27JointNoiseRuntime
		? snapshot.e27JointNoiseRuntime.sensitivities.size() : 0;
	result.userReferenceStates = snapshot.userReferences.size();
	result.userDualReferenceStates = snapshot.userDualReferences.size();
	if (validateOnly)
	{
		result.valid = true;
		return result;
	}
	if (owner.x.size() != checkpointStateDimension)
	{
		result.valid = false;
		result.failureReason = "PPP_AR_CHECKPOINT_OWNER_DIMENSION_MISMATCH";
		return result;
	}
	// Build complete replacement containers before any module state changes.
	const string oldRuntimeId = zhangPppArCheckpointRuntimeId;
	auto replaceOwner = [&](auto& values)
	{
		values.erase(runtimeId);
		if (!oldRuntimeId.empty() && oldRuntimeId != runtimeId)
		{
			values.erase(oldRuntimeId);
		}
	};
	auto newPendingTransitions = pendingProductTransitions;
	replaceOwner(newPendingTransitions);
	if (!restoredTransitions.empty())
	{
		newPendingTransitions.emplace(runtimeId, std::move(restoredTransitions));
	}
	auto newPendingPins = pendingSnapshotPins;
	replaceOwner(newPendingPins);
	if (snapshot.hasPendingSnapshotPins)
	{
		newPendingPins.emplace(runtimeId, std::move(restoredPendingPins));
	}
	auto newFactorBuffers = e18FactorCaptureBuffers;
	replaceOwner(newFactorBuffers);
	if (snapshot.hasFactorCapture)
	{
		newFactorBuffers.emplace(runtimeId, std::move(restoredFactorCapture));
	}
	auto newRegistries = e18PersistentProductDatumRegistries;
	replaceOwner(newRegistries);
	if (snapshot.hasPersistentProductDatumRegistry)
	{
		newRegistries.emplace(runtimeId, std::move(restoredRegistry));
	}
	auto newRawRows = e27RawNoiseRows;
	for (auto it = newRawRows.begin(); it != newRawRows.end();)
	{
		if (it->first.runtimeId == runtimeId
		 || (!oldRuntimeId.empty()
			 && oldRuntimeId != runtimeId
			 && it->first.runtimeId == oldRuntimeId))
		{
			it = newRawRows.erase(it);
		}
		else
		{
			++it;
		}
	}
	newRawRows.insert(restoredRawRows.begin(), restoredRawRows.end());
	auto newJointRuntimes = e27JointNoiseRuntimes;
	replaceOwner(newJointRuntimes);
	if (snapshot.hasE27JointNoiseRuntime)
	{
		newJointRuntimes.emplace(
			runtimeId, std::move(restoredJointNoiseRuntime));
	}
	auto eraseUserRuntime = [&](auto& values)
	{
		for (auto iterator = values.begin(); iterator != values.end();)
		{
			if (iterator->first.runtimeId == runtimeId
			 || (!oldRuntimeId.empty() && oldRuntimeId != runtimeId
				 && iterator->first.runtimeId == oldRuntimeId))
			{
				iterator = values.erase(iterator);
			}
			else
			{
				++iterator;
			}
		}
	};
	auto newUserReferences = userReferenceMap;
	eraseUserRuntime(newUserReferences);
	newUserReferences.insert(
		restoredUserReferences.begin(), restoredUserReferences.end());
	auto newUserDualReferences = userDualReferenceMap;
	eraseUserRuntime(newUserDualReferences);
	newUserDualReferences.insert(
		restoredUserDualReferences.begin(), restoredUserDualReferences.end());
	auto newConfiguredStates = e18ConfiguredFactorCaptureStates;
	const auto currentBinding = e18RuntimeObjectBindings.find(runtimeId);
	const bool destinationCallbacksInstalled =
		newConfiguredStates.count(runtimeId) != 0
		&& currentBinding != e18RuntimeObjectBindings.end()
		&& currentBinding->second == &owner;
	newConfiguredStates.erase(runtimeId);
	if (!oldRuntimeId.empty())
	{
		newConfiguredStates.erase(oldRuntimeId);
	}
	if (snapshot.factorCaptureConfigured && destinationCallbacksInstalled)
	{
		newConfiguredStates.insert(runtimeId);
	}
	auto newRuntimeObjectBindings = e18RuntimeObjectBindings;
	newRuntimeObjectBindings.erase(runtimeId);
	if (!oldRuntimeId.empty())
	{
		newRuntimeObjectBindings.erase(oldRuntimeId);
	}
	if (snapshot.factorCaptureConfigured && destinationCallbacksInstalled)
	{
		newRuntimeObjectBindings[runtimeId] = &owner;
	}
	auto newRestoredStates = e18RestoredRuntimeStates;
	if (!oldRuntimeId.empty())
	{
		newRestoredStates.erase(oldRuntimeId);
	}
	newRestoredStates.erase(runtimeId);
	if (snapshot.factorCaptureConfigured && !destinationCallbacksInstalled)
	{
		newRestoredStates.insert(runtimeId);
	}
	string newRuntimeId = runtimeId;
	if (!bindZhangCheckpointRuntimeId(owner, runtimeId, &failureReason))
	{
		result.valid = false;
		result.failureReason = failureReason;
		return result;
	}

	// All operations below are swaps/moves of validated temporary state.
	pendingProductTransitions.swap(newPendingTransitions);
	pendingSnapshotPins.swap(newPendingPins);
	e18FactorCaptureBuffers.swap(newFactorBuffers);
	e18PersistentProductDatumRegistries.swap(newRegistries);
	e27RawNoiseRows.swap(newRawRows);
	e27JointNoiseRuntimes.swap(newJointRuntimes);
	e18ConfiguredFactorCaptureStates.swap(newConfiguredStates);
	e18RuntimeObjectBindings.swap(newRuntimeObjectBindings);
	e18RestoredRuntimeStates.swap(newRestoredStates);
	userReferenceMap.swap(newUserReferences);
	userDualReferenceMap.swap(newUserDualReferences);
	continuityMap.swap(restoredContinuity);
	houProductDatumVersionTrackers.swap(
		snapshot.productDatumVersionTrackers);
	houProductPhysicalFunctionalIdentities.swap(
		snapshot.physicalFunctionalIdentities);
	houProductPhysicalFunctionals.swap(restoredFunctionals);
	houProductSBasisFingerprints.swap(snapshot.sBasisFingerprints);
	houProductPhaseSegmentIdentities.swap(
		snapshot.phaseSegmentIdentities);
	houProductTreeAlignmentCycles.swap(snapshot.treeAlignmentCycles);
	houProductSnapshotIdentities.swap(snapshot.snapshotIdentities);
	globalContinuityMap.swap(restoredGlobalContinuity);
	satelliteDatumManagers.swap(restoredSatelliteManagers);
	hybridRealGaugeTransports.swap(restoredHybridRealGauges);
	promotionEvidence.swap(snapshot.promotionEvidence);
	relinkEvidence.swap(snapshot.relinkEvidence);
	productHistoryMap.swap(restoredProductHistory);
	zhangPppArCheckpointRuntimeId.swap(newRuntimeId);
	if (!(snapshot.factorCaptureConfigured && destinationCallbacksInstalled))
	{
		owner.acceptedMeasurementFactorCallback = {};
		owner.stateTransitionFactorCallback = {};
		owner.exactStateTransformCallback = {};
	}

	result.valid = true;
	return result;
}

ZhangPppArCheckpointResult inspectZhangPppArCheckpointSnapshotReferences(
	const string& runtimeId,
	const string& payload,
	ZhangCheckpointSnapshotReferenceSummary& summary)
{
	summary = {};
	summary.sectionName = ZHANG_PPP_AR_CHECKPOINT_SECTION_NAME;
	summary.runtimeId = runtimeId;
	ZhangPppArCheckpointResult result;
	auto fail = [&](const string& reason)
	{
		summary.valid = false;
		summary.failureReason = reason;
		result.valid = false;
		result.failureReason = reason;
		return result;
	};
	if (!validCheckpointRuntimeId(runtimeId))
	{
		return fail("PPP_AR_CHECKPOINT_RUNTIME_ID_INVALID");
	}
	if (payload.empty())
	{
		return fail("PPP_AR_CHECKPOINT_EMPTY_PAYLOAD");
	}

	ZhangPppArCheckpointEnvelope snapshot;
	try
	{
		std::istringstream input(payload, std::ios::binary | std::ios::in);
		{
			boost::archive::binary_iarchive archive(
				input, boost::archive::no_header);
			archive >> snapshot;
		}
		if (input.peek() != std::char_traits<char>::eof())
		{
			return fail("PPP_AR_CHECKPOINT_TRAILING_BYTES");
		}
	}
	catch (const std::exception& exception)
	{
		return fail(
			string("PPP_AR_CHECKPOINT_DESERIALIZE_FAILED:") +
			exception.what());
	}
	if (snapshot.schemaVersion != ZHANG_PPP_AR_CHECKPOINT_SCHEMA_VERSION)
	{
		return fail("PPP_AR_CHECKPOINT_SCHEMA_MISMATCH");
	}
	if (snapshot.runtimeId != runtimeId)
	{
		return fail("PPP_AR_CHECKPOINT_RUNTIME_ID_MISMATCH");
	}

	// This routine is deliberately a structural cross-section inspection,
	// not a second authoritative section preflight.  In particular, rebuilding
	// the E18 factor chronology here is both redundant during restore (the
	// caller has already run import(..., validateOnly=true)) and prohibitively
	// expensive during capture, where the live FactorCaptureBuffer is already
	// authoritative.  Keep all numerical replay validation in the PPP-AR
	// import preflight and validate only the fields needed by the snapshot
	// reference join below.
	if ((!snapshot.hasPendingSnapshotPins
		 && !snapshot.pendingPins.identities.empty())
	 || (!snapshot.hasFactorCapture
		 && !snapshot.factorCapture.snapshotOperations.empty()))
	{
		return fail("PPP_AR_CHECKPOINT_SNAPSHOT_REFERENCE_PRESENCE_MISMATCH");
	}

	set<string> retained;
	std::size_t expectedOperationSequence = 0;
	if (snapshot.hasFactorCapture)
	{
		for (const auto& operation : snapshot.factorCapture.snapshotOperations)
		{
			if (operation.operationSequence != expectedOperationSequence++)
			{
				return fail(
					"PPP_AR_CHECKPOINT_SNAPSHOT_OPERATION_SEQUENCE_INVALID");
			}
			set<string> identities(
				operation.identities.begin(), operation.identities.end());
			if (identities.size() != operation.identities.size()
			 || identities.count("") != 0)
			{
				return fail(
					"PPP_AR_CHECKPOINT_SNAPSHOT_OPERATION_IDENTITY_INVALID");
			}
			if (operation.kind ==
					ZhangCapturedSnapshotOperationKind::BIND_NEW_TARGETS)
			{
				for (const auto& identity : identities)
				{
					if (!retained.insert(identity).second)
					{
						return fail(
							"PPP_AR_CHECKPOINT_SNAPSHOT_BIND_DUPLICATE");
					}
				}
			}
			else if (operation.kind ==
					ZhangCapturedSnapshotOperationKind::RETAIN_TARGETS)
			{
				const bool allPreviouslyBound = std::all_of(
					identities.begin(), identities.end(), [&](const string& identity)
					{
						return retained.count(identity) != 0;
					});
				if (!allPreviouslyBound)
				{
					return fail(
						"PPP_AR_CHECKPOINT_SNAPSHOT_RETAIN_UNBOUND");
				}
				retained = std::move(identities);
			}
			else
			{
				return fail(
					"PPP_AR_CHECKPOINT_SNAPSHOT_OPERATION_KIND_INVALID");
			}
		}
	}

	summary.availableSnapshotIdentities = retained;
	for (const auto& [ignored, identity] : snapshot.snapshotIdentities)
	{
		if (identity.empty()
		 || retained.count(identity) == 0)
		{
			return fail("PPP_AR_CHECKPOINT_CURRENT_SNAPSHOT_UNAVAILABLE");
		}
		summary.currentSnapshotIdentities.insert(identity);
	}
	if (snapshot.hasPendingSnapshotPins)
	{
		if (snapshot.pendingPins.identities.empty())
		{
			return fail("PPP_AR_CHECKPOINT_PINNED_SNAPSHOT_EMPTY");
		}
		set<string> pinnedIdentities;
		for (const auto& identity : snapshot.pendingPins.identities)
		{
			if (identity.empty()
			 || retained.count(identity) == 0
			 || !pinnedIdentities.insert(identity).second)
			{
				return fail("PPP_AR_CHECKPOINT_PINNED_SNAPSHOT_UNAVAILABLE");
			}
			summary.pinnedSnapshotIdentities.insert(identity);
		}
	}

	auto accountReference = [&](const string& identity, int declaredCount)
	{
		if (identity.empty())
		{
			return declaredCount == 0;
		}
		if (declaredCount < 0 || retained.count(identity) == 0)
		{
			return false;
		}
		auto& actual = summary.actualReferenceCounts[identity];
		if (actual == std::numeric_limits<std::size_t>::max())
		{
			return false;
		}
		actual++;
		// Zero is the expected value while a transition is still owned by
		// PPP-AR; after activation AMBRES recomputes this diagnostic per
		// branch.  Preserve positive values for evidence, but do not pretend
		// they are a bundle-wide reference total.
		if (declaredCount > 0)
		{
			summary.declaredReferenceCounts.emplace(
				identity, static_cast<std::size_t>(declaredCount));
		}
		return true;
	};
	for (const auto& transition : snapshot.pendingTransitions)
	{
		if (!accountReference(
				transition.oldSnapshotIdentity,
				transition.oldSnapshotReferenceCount)
		 || !accountReference(
				transition.newSnapshotIdentity,
				transition.newSnapshotReferenceCount))
		{
			return fail(
				"PPP_AR_CHECKPOINT_PENDING_SNAPSHOT_REFERENCE_INVALID");
		}
	}
	summary.transitionCount = snapshot.pendingTransitions.size();
	summary.valid = true;
	summary.failureReason.clear();
	result.valid = true;
	result.failureReason.clear();
	return result;
}

ZhangCheckpointSnapshotReferenceValidation
validateZhangCheckpointSnapshotReferences(
	const ZhangCheckpointSnapshotReferenceSummary& pppArSummary,
	const ZhangCheckpointSnapshotReferenceSummary& ambresSummary)
{
	ZhangCheckpointSnapshotReferenceValidation result;
	auto fail = [&](const string& reason)
	{
		result.valid = false;
		result.failureReason = reason;
		return result;
	};
	if (!pppArSummary.valid || !ambresSummary.valid)
	{
		return fail("CHECKPOINT_SNAPSHOT_REFERENCE_SECTION_INVALID");
	}
	if (pppArSummary.sectionName != ZHANG_PPP_AR_CHECKPOINT_SECTION_NAME
	 || ambresSummary.sectionName != ZHANG_AMBRES_CHECKPOINT_SECTION_NAME)
	{
		return fail("CHECKPOINT_SNAPSHOT_REFERENCE_SECTION_MISMATCH");
	}
	if (!validCheckpointRuntimeId(pppArSummary.runtimeId)
	 || pppArSummary.runtimeId != ambresSummary.runtimeId)
	{
		return fail("CHECKPOINT_SNAPSHOT_REFERENCE_RUNTIME_ID_MISMATCH");
	}
	const auto subsetOfAvailable = [&](const set<string>& identities)
	{
		return std::all_of(
			identities.begin(), identities.end(), [&](const string& identity)
			{
				return !identity.empty()
					&& pppArSummary.availableSnapshotIdentities.count(
						identity) != 0;
			});
	};
	if (pppArSummary.availableSnapshotIdentities.count("") != 0
	 || !subsetOfAvailable(pppArSummary.currentSnapshotIdentities)
	 || !subsetOfAvailable(pppArSummary.pinnedSnapshotIdentities)
	 || !ambresSummary.availableSnapshotIdentities.empty()
	 || !ambresSummary.currentSnapshotIdentities.empty()
	 || !ambresSummary.pinnedSnapshotIdentities.empty())
	{
		return fail("CHECKPOINT_SNAPSHOT_REFERENCE_SUMMARY_STRUCTURE_INVALID");
	}
	auto sectionCountsValid = [](const auto& summary)
	{
		const bool actualValid = std::all_of(
			summary.actualReferenceCounts.begin(),
			summary.actualReferenceCounts.end(), [&](const auto& item)
			{
				return !item.first.empty() && item.second > 0;
			});
		const bool diagnosticsValid = std::all_of(
			summary.declaredReferenceCounts.begin(),
			summary.declaredReferenceCounts.end(), [&](const auto& item)
			{
				return !item.first.empty() && item.second > 0
					&& summary.actualReferenceCounts.count(item.first) != 0;
			});
		return actualValid && diagnosticsValid;
	};
	if (!sectionCountsValid(pppArSummary)
	 || !sectionCountsValid(ambresSummary))
	{
		return fail("CHECKPOINT_SNAPSHOT_REFERENCE_COUNT_SET_INVALID");
	}
	result.runtimeId = pppArSummary.runtimeId;

	auto mergeActual = [&](const auto& values)
	{
		for (const auto& [identity, count] : values)
		{
			if (identity.empty() || count == 0
			 || pppArSummary.availableSnapshotIdentities.count(identity) == 0)
			{
				return false;
			}
			auto& combined = result.combinedReferenceCounts[identity];
			if (count > std::numeric_limits<std::size_t>::max() - combined)
			{
				return false;
			}
			combined += count;
		}
		return true;
	};
	if (!mergeActual(pppArSummary.actualReferenceCounts)
	 || !mergeActual(ambresSummary.actualReferenceCounts))
	{
		return fail("CHECKPOINT_SNAPSHOT_REFERENCE_TARGET_UNAVAILABLE");
	}

	result.valid = true;
	result.failureReason.clear();
	return result;
}

ZhangCanonicalRelationSelection selectZhangE18CanonicalProductRelations(
    const KFState& captureOwner,
    E_Sys system,
    const vector<ZhangCanonicalSatelliteRelation>& bootstrapCandidates,
    const set<SatSys>& availableSatellites,
    int maximumRelations)
{
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(captureOwner, runtimeId))
	{
		return {};
	}
    return e18PersistentProductDatumRegistries[runtimeId].selectRelations(
        system, bootstrapCandidates, availableSatellites, maximumRelations);
}

ZhangPersistentProductDatumObservation observeZhangE18PersistentProductDatum(
    const KFState& captureOwner,
    E_Sys system,
    E_ObsCode observable,
    const ZhangCanonicalSatelliteRelation& relation,
    int anchorPhaseSegment,
    int satellitePhaseSegment,
    int anchorDatumVersion,
    int satelliteDatumVersion,
    bool absoluteAvailable)
{
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(captureOwner, runtimeId))
	{
		ZhangPersistentProductDatumObservation result;
		result.failureReason = "CHECKPOINT_RUNTIME_ID_UNBOUND";
		return result;
	}
    return e18PersistentProductDatumRegistries[runtimeId].observe(
        system, observable, relation,
        anchorPhaseSegment, satellitePhaseSegment,
        anchorDatumVersion, satelliteDatumVersion,
        absoluteAvailable);
}

void configureZhangE18FactorCapture(KFState& kfState)
{
    const KFState* owner = &kfState;
	const string runtimeId = zhangPppArRuntimeId(kfState);
    const bool e18Enabled =
        acsConfig.zhangPppAr.fixed_lag_factor_capture_shadow;
	const bool targetedBesdEnabled =
		acsConfig.zhangPppAr.targeted_besd_capture_shadow;
    const bool e27Enabled = e27JointNoiseEnabled();
    if (!e18Enabled && !targetedBesdEnabled && !e27Enabled)
    {
		kfState.acceptedMeasurementFactorCallback = {};
		kfState.stateTransitionFactorCallback = {};
		kfState.exactStateTransformCallback = {};
		if (validCheckpointRuntimeId(runtimeId))
		{
			auto binding = e18RuntimeObjectBindings.find(runtimeId);
			if (binding != e18RuntimeObjectBindings.end()
			 && binding->second == owner)
			{
				e18RestoredRuntimeStates.erase(runtimeId);
				e18ConfiguredFactorCaptureStates.erase(runtimeId);
				e18FactorCaptureBuffers.erase(runtimeId);
				targetedBesdRuntimes.erase(runtimeId);
				e27JointNoiseRuntimes.erase(runtimeId);
				e18RuntimeObjectBindings.erase(binding);
			}
		}
        return;
    }
	if (!validCheckpointRuntimeId(runtimeId))
	{
		kfState.acceptedMeasurementFactorCallback = {};
		kfState.stateTransitionFactorCallback = {};
		kfState.exactStateTransformCallback = {};
		BOOST_LOG_TRIVIAL(error)
			<< "ZHANG_RUNTIME_ID status=REJECTED"
			<< " reason=CHECKPOINT_RUNTIME_ID_UNBOUND"
			<< " state_id=" << kfState.id;
		return;
	}
    if (e18ConfiguredFactorCaptureStates.find(runtimeId) !=
        e18ConfiguredFactorCaptureStates.end())
    {
		auto binding = e18RuntimeObjectBindings.find(runtimeId);
		if (binding == e18RuntimeObjectBindings.end()
		 || binding->second != owner)
		{
			kfState.acceptedMeasurementFactorCallback = {};
			kfState.stateTransitionFactorCallback = {};
			kfState.exactStateTransformCallback = {};
			BOOST_LOG_TRIVIAL(error)
				<< "ZHANG_RUNTIME_ID status=REJECTED"
				<< " reason=CHECKPOINT_RUNTIME_ID_OBJECT_CONFLICT"
				<< " runtime_id=" << runtimeId;
        }
        return;
    }
	if (auto binding = e18RuntimeObjectBindings.find(runtimeId);
		binding != e18RuntimeObjectBindings.end()
		&& binding->second != owner)
	{
		kfState.acceptedMeasurementFactorCallback = {};
		kfState.stateTransitionFactorCallback = {};
		kfState.exactStateTransformCallback = {};
		BOOST_LOG_TRIVIAL(error)
			<< "ZHANG_RUNTIME_ID status=REJECTED"
			<< " reason=CHECKPOINT_RUNTIME_ID_OBJECT_CONFLICT"
			<< " runtime_id=" << runtimeId;
		return;
	}
	const bool restoredRuntime =
		e18RestoredRuntimeStates.erase(runtimeId) > 0;
	if (targetedBesdEnabled && !restoredRuntime)
	{
		targetedBesdRuntimes.erase(runtimeId);
	}

    if (e18Enabled)
    {
		auto& buffer = e18FactorCaptureBuffers[runtimeId];
		if (!restoredRuntime)
		{
			buffer.clear();
		}
        buffer.setMaximumEvents(
            acsConfig.zhangPppAr.fixed_lag_factor_capture_max_events
        );
    }
    if (e27Enabled)
    {
		if (!restoredRuntime
		 || e27JointNoiseRuntimes.find(runtimeId) == e27JointNoiseRuntimes.end())
		{
			e27JointNoiseRuntimes[runtimeId] = {};
		}
    }

    kfState.acceptedMeasurementFactorCallback =
        [owner, runtimeId](const KFState& state,
                const KFMeas& measurement,
                const string& suffix,
                const VectorXd& posteriorMean,
                const MatrixXd& posteriorCovariance)
        {
            if (&state != owner || suffix != "/PPP")
            {
                return;
            }
            if (e27JointNoiseEnabled())
            {
				auto& runtime = e27JointNoiseRuntimes[runtimeId];
                const int dimension = state.x.size();
                const bool dimensionsValid =
                    measurement.H.cols() == dimension &&
                    state.P.rows() == dimension && state.P.cols() == dimension &&
                    measurement.R.rows() == measurement.H.rows() &&
                    measurement.R.cols() == measurement.H.rows() &&
                    measurement.H_star.rows() == measurement.H.rows();
                MatrixXd innovationInverse;
                const MatrixXd innovationCovariance = dimensionsValid
                    ? measurement.H * state.P * measurement.H.transpose() +
                        measurement.R
                    : MatrixXd{};
                if (!dimensionsValid ||
                    !e27SymmetricInverse(innovationCovariance, innovationInverse))
                {
                    runtime.failureReason =
                        "ACCEPTED_MEASUREMENT_INNOVATION_NOT_INVERTIBLE";
                    runtime.sensitivities.clear();
                }
                else
                {
                    const MatrixXd gain = state.P * measurement.H.transpose() *
                        innovationInverse;
                    const MatrixXd attenuation =
                        MatrixXd::Identity(dimension, dimension) -
                        gain * measurement.H;
                    bool valid = gain.allFinite() && attenuation.allFinite();
                    for (auto& [ignored, sensitivity] : runtime.sensitivities)
                    {
                        if (sensitivity.stateDerivative.size() != dimension)
                        {
                            valid = false;
                            break;
                        }
                        sensitivity.stateDerivative =
                            attenuation * sensitivity.stateDerivative;
                    }
                    if (!valid)
                    {
                        runtime.failureReason =
                            "ACCEPTED_MEASUREMENT_SENSITIVITY_UPDATE_FAILURE";
                        runtime.sensitivities.clear();
                    }
                    else
                    {
                        for (const auto& [noiseKey, noiseIndex] :
                             measurement.noiseIndexMap)
                        {
                            if (noiseIndex < 0 ||
                                noiseIndex >= measurement.H_star.cols() ||
                                noiseIndex >= measurement.uncorrelatedNoise.size())
                            {
                                continue;
                            }
                            const string stamped = e27StampedNoiseIdentity(
                                measurement.time, noiseKey);
                            auto& sensitivity = runtime.sensitivities[stamped];
                            sensitivity.time = measurement.time;
                            sensitivity.variance =
                                measurement.uncorrelatedNoise(noiseIndex);
                            if (sensitivity.stateDerivative.size() == 0)
                            {
                                sensitivity.stateDerivative =
                                    VectorXd::Zero(dimension);
                            }
                            sensitivity.stateDerivative +=
                                gain * measurement.H_star.col(noiseIndex);
                        }
                        for (auto it = runtime.sensitivities.begin();
                             it != runtime.sensitivities.end();)
                        {
                            if ((measurement.time - it->second.time).to_double() >
                                3600)
                            {
                                it = runtime.sensitivities.erase(it);
                            }
                            else
                            {
                                ++it;
                            }
                        }
                        runtime.failureReason.clear();

                        const MatrixXd reconstructed = attenuation * state.P *
                                attenuation.transpose() +
                            gain * measurement.R * gain.transpose();
                        const double scale = std::max(1.0, posteriorCovariance.norm());
                        BOOST_LOG_TRIVIAL(info)
                            << "ZHANG_E27_JOINT_NOISE time="
                            << measurement.time.to_string(0)
                            << " event=ACCEPTED_MEASUREMENT"
                            << " accepted_rows=" << measurement.H.rows()
                            << " tracked_noise_factors="
                            << runtime.sensitivities.size()
                            << " posterior_covariance_relative_error="
                            << (reconstructed - posteriorCovariance).norm() / scale
                            << " feedback=0";
                    }
                }
            }
			if (acsConfig.zhangPppAr.targeted_besd_capture_shadow)
			{
				auto runtimes = targetedBesdRuntimes.find(runtimeId);
				if (runtimes != targetedBesdRuntimes.end())
				{
					for (auto event = runtimes->second.begin();
						 event != runtimes->second.end();)
					{
						const bool updated = event->tracker.updateAcceptedMeasurement(
							state.P, measurement.H, measurement.R, measurement.V);
						if (!updated)
						{
							BOOST_LOG_TRIVIAL(error)
								<< "ZHANG_TARGETED_BESD_CAPTURE time="
								<< measurement.time.to_string(0)
								<< " event_time=" << event->eventTime.to_string(0)
								<< " status=REJECTED reason="
								<< event->tracker.failureReason()
								<< " estimator_feedback=0";
							event = runtimes->second.erase(event);
							continue;
						}
						event->ageEpochs++;
						const bool minimumLag = event->ageEpochs >=
							acsConfig.zhangPppAr.targeted_besd_min_lag_epochs;
						const bool maximumLag = event->ageEpochs >=
							acsConfig.zhangPppAr.targeted_besd_max_lag_epochs;
						if ((minimumLag && !event->minimumLagReported) || maximumLag)
						{
							const auto marginal = event->tracker.marginal();
							for (const auto& pair : event->pairs)
							{
								double mean = std::numeric_limits<double>::quiet_NaN();
								double variance = std::numeric_limits<double>::quiet_NaN();
								double fractional = std::numeric_limits<double>::quiet_NaN();
								double perr = 1;
								const bool valid = marginal.valid
									&& pair.oldTarget >= 0
									&& pair.newTarget >= 0
									&& pair.oldTarget < marginal.mean.size()
									&& pair.newTarget < marginal.mean.size();
								if (valid)
								{
									mean = marginal.mean(pair.newTarget)
										- marginal.mean(pair.oldTarget);
									variance = marginal.covariance(pair.newTarget,
										pair.newTarget)
										+ marginal.covariance(pair.oldTarget,
											pair.oldTarget)
										- 2 * marginal.covariance(pair.newTarget,
											pair.oldTarget);
									variance = std::max(0.0, variance);
									fractional = mean - std::round(mean);
									perr = variance > 0
										? round_perr(fractional, variance)
										: (std::abs(fractional) <= 1e-12 ? 0 : 1);
								}
								BOOST_LOG_TRIVIAL(info)
									<< "ZHANG_TARGETED_BESD_MARGINAL time="
									<< measurement.time.to_string(0)
									<< " event_time=" << event->eventTime.to_string(0)
									<< " system=" << enum_to_string(event->system)
									<< " satellite=" << pair.satellite.id()
									<< " observable=" << enum_to_string(pair.observable)
									<< " age_epochs=" << event->ageEpochs
									<< " target_count=" << event->tracker.targetCount()
									<< " mean_cycles=" << mean
									<< " variance_cycles2=" << variance
									<< " fractional_cycles=" << fractional
									<< " perr=" << perr
									<< " status=" << (valid ? "AVAILABLE" : "UNAVAILABLE")
									<< " estimator_feedback=0";
							}
							map<SatSys, map<E_ObsCode,
								const ZhangTargetedBesdPair*>> bySatellite;
							for (const auto& pair : event->pairs)
							{
								bySatellite[pair.satellite][pair.observable] = &pair;
							}
							auto configured = acsConfig.zhangPppAr.baseline_observables.find(
								event->system);
							if (configured != acsConfig.zhangPppAr.baseline_observables.end()
							 && configured->second.size() == 2)
							{
								const E_ObsCode firstCode = configured->second[0];
								const E_ObsCode secondCode = configured->second[1];
								vector<ZhangProductRelationAdmissionCandidate>
									besdAdmissionCandidates;
								map<E_ObsCode, map<SatSys, long long>>
									besdFrontendShifts;
								for (const auto& [satellite, signalPairs] : bySatellite)
								{
									auto first = signalPairs.find(firstCode);
									auto second = signalPairs.find(secondCode);
									if (!marginal.valid || first == signalPairs.end()
									 || second == signalPairs.end())
									{
										continue;
									}
									auto differenceRow = [&](const ZhangTargetedBesdPair& pair)
									{
										VectorXd row = VectorXd::Zero(marginal.mean.size());
										row(pair.newTarget) = 1;
										row(pair.oldTarget) = -1;
										return row;
									};
									MatrixXd differenceRows(2, marginal.mean.size());
									differenceRows.row(0) =
										differenceRow(*first->second).transpose();
									differenceRows.row(1) =
										differenceRow(*second->second).transpose();
									Vector2d differenceMean = differenceRows * marginal.mean;
									Matrix2d differenceCovariance = differenceRows
										* marginal.covariance * differenceRows.transpose();
									differenceCovariance = 0.5 *
										(differenceCovariance + differenceCovariance.transpose());
									Vector2d wideLaneTransform;
									wideLaneTransform << 1, -1;
									const double wideLaneMean =
										wideLaneTransform.dot(differenceMean);
									const double wideLaneVariance = std::max(0.0,
										(wideLaneTransform.transpose() * differenceCovariance
											* wideLaneTransform)(0, 0));
									const double wideLaneInteger = std::round(wideLaneMean);
									const double wideLaneFractional =
										wideLaneMean - wideLaneInteger;
									const double wideLanePerr = wideLaneVariance > 0
										? round_perr(wideLaneFractional, wideLaneVariance)
										: (std::abs(wideLaneFractional) <= 1e-12 ? 0 : 1);
									const double firstWideLaneCovariance =
										differenceCovariance.row(0).dot(wideLaneTransform);
									double conditionalFirstMean = differenceMean(0);
									double conditionalFirstVariance =
										differenceCovariance(0, 0);
									if (wideLaneVariance > 0)
									{
										conditionalFirstMean += firstWideLaneCovariance /
											wideLaneVariance * (wideLaneInteger - wideLaneMean);
										conditionalFirstVariance -= firstWideLaneCovariance *
											firstWideLaneCovariance / wideLaneVariance;
									}
									if (conditionalFirstVariance < 0
									 && conditionalFirstVariance > -1e-10 * std::max(
											1.0, std::abs(differenceCovariance(0, 0))))
									{
										conditionalFirstVariance = 0;
									}
									const double firstInteger = std::round(conditionalFirstMean);
									const double conditionalFirstFractional =
										conditionalFirstMean - firstInteger;
									const double conditionalFirstPerr =
										conditionalFirstVariance > 0
											? round_perr(conditionalFirstFractional,
												conditionalFirstVariance)
											: (conditionalFirstVariance == 0 &&
												std::abs(conditionalFirstFractional) <= 1e-12
													? 0 : 1);
									Matrix2d jointTransform;
									jointTransform << 1, -1, 1, 0;
									Vector2d jointMean;
									jointMean << wideLaneMean, differenceMean(0);
									Vector2d jointInteger;
									jointInteger << wideLaneInteger, firstInteger;
									const auto jointNis = assessZhangIntegerCandidateNis(
										jointInteger - jointMean,
										jointTransform * differenceCovariance *
											jointTransform.transpose(),
										acsConfig.zhangPppAr.held_constraint_nis_alpha);
									const double maximumPerr =
										acsConfig.zhangPppAr.canonical_user_target_max_perr;
									const bool reliable = wideLanePerr <= maximumPerr
										&& conditionalFirstPerr <= maximumPerr
										&& jointNis.valid && jointNis.nis <= jointNis.threshold;
									if (reliable && acsConfig.zhangPppAr
										.product_relation_admission_shadow)
									{
										auto makeCandidate = [&](const ZhangTargetedBesdPair& pair,
											long long completeInteger)
										{
											ZhangProductRelationAdmissionCandidate candidate;
											candidate.relationId = event->eventTime.to_string(0) +
												"|" + enum_to_string(event->system) + "|" +
												pair.satellite.id() + "|" +
												enum_to_string(pair.observable) + "|" +
												pair.oldSnapshotIdentity + "|" +
												pair.newSnapshotIdentity;
											candidate.satellite = pair.satellite.id();
											candidate.observable = enum_to_string(pair.observable);
											candidate.integerValue = ZhangExactInteger(completeInteger) -
												pair.transition.affineOffsetCycles;
											candidate.exactIntegerEstimable = pair.transition.valid &&
												!pair.transition.physicalEdges.empty() &&
												pair.transition.physicalEdges.size() ==
													pair.transition.coefficients.size() &&
												pair.transition.physicalEdges.size() ==
													pair.transition.physicalArcVersions.size();
											candidate.phaseSegmentCompatible =
												!pair.oldSnapshotIdentity.empty() &&
												!pair.newSnapshotIdentity.empty();
											candidate.scalarReliabilityPassed = true;
											candidate.jointNisPassed = true;
											for (size_t index = 0;
												 index < pair.transition.coefficients.size(); index++)
											{
												const auto coefficient =
													pair.transition.coefficients[index];
												if (coefficient == 0) continue;
												const auto& edge = pair.transition.physicalEdges[index];
												const string column = edge.receiver + ">" +
													edge.satellite.id() + "@v" + std::to_string(
														pair.transition.physicalArcVersions[index]);
												candidate.physicalCoefficients[column] += coefficient;
											}
											return candidate;
										};
										const long long firstComplete =
											std::llround(firstInteger);
										const long long secondComplete =
											std::llround(firstInteger - wideLaneInteger);
										besdAdmissionCandidates.push_back(
											makeCandidate(*first->second, firstComplete));
										besdAdmissionCandidates.push_back(
											makeCandidate(*second->second, secondComplete));
										besdFrontendShifts[firstCode][satellite] = firstComplete;
										besdFrontendShifts[secondCode][satellite] = secondComplete;
									}
									BOOST_LOG_TRIVIAL(info)
										<< "ZHANG_TARGETED_BESD_PAIR time="
										<< measurement.time.to_string(0)
										<< " event_time=" << event->eventTime.to_string(0)
										<< " system=" << enum_to_string(event->system)
										<< " satellite=" << satellite.id()
										<< " age_epochs=" << event->ageEpochs
										<< " first_observable=" << enum_to_string(firstCode)
										<< " second_observable=" << enum_to_string(secondCode)
										<< " first_mean_cycles=" << differenceMean(0)
										<< " second_mean_cycles=" << differenceMean(1)
										<< " first_variance_cycles2="
										<< differenceCovariance(0, 0)
										<< " second_variance_cycles2="
										<< differenceCovariance(1, 1)
										<< " cross_covariance_cycles2="
										<< differenceCovariance(0, 1)
										<< " wl_mean_cycles=" << wideLaneMean
										<< " wl_variance_cycles2=" << wideLaneVariance
										<< " wl_perr=" << wideLanePerr
										<< " conditional_l1_mean_cycles="
										<< conditionalFirstMean
										<< " conditional_l1_variance_cycles2="
										<< conditionalFirstVariance
										<< " conditional_l1_perr=" << conditionalFirstPerr
										<< " joint_nis=" << jointNis.nis
										<< " joint_nis_threshold=" << jointNis.threshold
										<< " exact_physical_first="
										<< (first->second->transition.valid &&
											!first->second->transition.physicalEdges.empty() &&
											first->second->transition.physicalEdges.size() ==
												first->second->transition.coefficients.size())
										<< " exact_physical_second="
										<< (second->second->transition.valid &&
											!second->second->transition.physicalEdges.empty() &&
											second->second->transition.physicalEdges.size() ==
												second->second->transition.coefficients.size())
										<< " status=" << (reliable ? "RELIABLE" : "UNRELIABLE")
										<< " estimator_feedback=0";
								}
								if (!besdAdmissionCandidates.empty())
								{
									auto& admissionState =
										zhangProductRelationAdmissionStateRegistry()[
											{runtimeId, event->system}];
									const auto beforeAdmission = admissionState;
									auto admission = ProductRelationAdmission::admit(
										admissionState, besdAdmissionCandidates, true);
									bool frontendCommitted = false;
									size_t frontendRestored = 0;
									if (admission.committed)
									{
										const auto frontend =
											applyZhangCertifiedTemporalProductShiftBatch(
												measurement.time, event->system,
												besdFrontendShifts,
												"TARGETED_BESD_ADMISSION");
										frontendCommitted = frontend.accepted;
										frontendRestored = frontend.restoredSatellites;
										if (!frontendCommitted)
										{
											admissionState = beforeAdmission;
											admission.committed = false;
											admission.status =
												"ABORT_FRONTEND_ALIGNMENT_REJECTED";
										}
									}
									BOOST_LOG_TRIVIAL(info)
										<< "ZHANG_TARGETED_BESD_ADMISSION time="
										<< measurement.time.to_string(0)
										<< " event_time=" << event->eventTime.to_string(0)
										<< " system=" << enum_to_string(event->system)
										<< " age_epochs=" << event->ageEpochs
										<< " candidate_rows=" << admission.candidateRows
										<< " candidate_exact_rank=" << admission.candidateExactRank
										<< " candidate_redundant_rows="
										<< admission.candidateRedundantRows
										<< " status=" << admission.status
										<< " certified_for_product=" << admission.committed
										<< " frontend_committed=" << frontendCommitted
										<< " frontend_restored_satellites=" << frontendRestored
										<< " estimator_feedback=0";
								}
							}
							event->minimumLagReported = true;
						}
						if (maximumLag)
						{
							event = runtimes->second.erase(event);
						}
						else
						{
							++event;
						}
					}
				}
			}
			auto capture = e18FactorCaptureBuffers.find(runtimeId);
            if (capture == e18FactorCaptureBuffers.end())
            {
                return;
            }
            bool accepted = capture->second.recordMeasurement(
                measurement.time,
                zhangKeysByIndex(state.kfIndexMap),
                state.x,
                state.P,
                measurement,
                suffix,
                posteriorMean,
                posteriorCovariance
            );
            ZhangFactorCaptureSummary summary = capture->second.summary();
            BOOST_LOG_TRIVIAL(info)
                << "ZHANG_E18_FACTOR_CAPTURE time="
                << measurement.time.to_string(0)
                << " event=MEASUREMENT"
                << " status=" << (accepted ? "ACCEPTED" : "REJECTED")
                << " events=" << summary.events
                << " measurements=" << summary.measurements
                << " transitions=" << summary.transitions
                << " exact_transforms=" << summary.coordinateTransforms
                << " measurement_rows=" << summary.measurementRows
                << " measurement_nnz=" << summary.measurementNonZeros
                << " covariance_nnz=" << summary.covarianceNonZeros
                << " replay_prior_mean_relative_error="
                << summary.maximumReplayPriorMeanRelativeError
                << " replay_prior_covariance_relative_error="
                << summary.maximumReplayPriorCovarianceRelativeError
				<< " raw_square_root_mean_relative_error="
				<< summary.maximumRawSquareRootMeanRelativeError
				<< " raw_square_root_covariance_relative_error="
				<< summary.maximumRawSquareRootCovarianceRelativeError
                << " failure_reason="
                << (summary.failureReason.empty() ? "NONE" : summary.failureReason)
                << " feedback=0";
        };

    kfState.stateTransitionFactorCallback =
        [owner, runtimeId](const KFState& state,
                GTime time,
                const map<KFKey, int>& source,
                const map<KFKey, int>& destination,
                const SparseMatrix<double>& transition,
                const MatrixXd& processCovariance,
                const string& label)
        {
            if (&state != owner)
            {
                return;
            }
            if (e27JointNoiseEnabled())
            {
                e27TransformSensitivities(
					runtimeId, transition, source.size(), destination.size(), label);
            }
			if (acsConfig.zhangPppAr.targeted_besd_capture_shadow)
			{
				auto runtimes = targetedBesdRuntimes.find(runtimeId);
				if (runtimes != targetedBesdRuntimes.end())
				{
					for (auto event = runtimes->second.begin();
						 event != runtimes->second.end();)
					{
						if (!event->tracker.advanceState(MatrixXd(transition)))
						{
							BOOST_LOG_TRIVIAL(error)
								<< "ZHANG_TARGETED_BESD_CAPTURE time="
								<< time.to_string(0)
								<< " event_time=" << event->eventTime.to_string(0)
								<< " status=REJECTED reason="
								<< event->tracker.failureReason()
								<< " estimator_feedback=0";
							event = runtimes->second.erase(event);
						}
						else
						{
							++event;
						}
					}
				}
			}
			auto capture = e18FactorCaptureBuffers.find(runtimeId);
            if (capture == e18FactorCaptureBuffers.end())
            {
                return;
            }
            bool accepted = capture->second.recordTransition(
                time,
                zhangKeysByIndex(source),
                zhangKeysByIndex(destination),
                transition,
                processCovariance,
                label
            );
            if (!accepted)
            {
                auto summary = capture->second.summary();
                BOOST_LOG_TRIVIAL(error)
                    << "ZHANG_E18_FACTOR_CAPTURE time=" << time.to_string(0)
                    << " event=STATE_TRANSITION status=REJECTED"
                    << " failure_reason=" << summary.failureReason
                    << " feedback=0";
            }
        };

    kfState.exactStateTransformCallback =
        [owner, runtimeId](const KFState& state,
                GTime time,
                const map<KFKey, int>& source,
                const map<KFKey, int>& destination,
                const SparseMatrix<double>& transform,
                const string& label)
        {
            if (&state != owner)
            {
                return;
            }
            if (e27JointNoiseEnabled())
            {
                e27TransformSensitivities(
					runtimeId, transform, source.size(), destination.size(), label);
            }
			if (acsConfig.zhangPppAr.targeted_besd_capture_shadow)
			{
				auto runtimes = targetedBesdRuntimes.find(runtimeId);
				if (runtimes != targetedBesdRuntimes.end())
				{
					for (auto event = runtimes->second.begin();
						 event != runtimes->second.end();)
					{
						if (!event->tracker.applyExactStateTransform(
								MatrixXd(transform)))
						{
							BOOST_LOG_TRIVIAL(error)
								<< "ZHANG_TARGETED_BESD_CAPTURE time="
								<< time.to_string(0)
								<< " event_time=" << event->eventTime.to_string(0)
								<< " status=REJECTED reason="
								<< event->tracker.failureReason()
								<< " estimator_feedback=0";
							event = runtimes->second.erase(event);
						}
						else
						{
							++event;
						}
					}
				}
			}
			auto capture = e18FactorCaptureBuffers.find(runtimeId);
            if (capture == e18FactorCaptureBuffers.end())
            {
                return;
            }
            bool accepted = capture->second.recordCoordinateTransform(
                time,
                zhangKeysByIndex(source),
                zhangKeysByIndex(destination),
                transform,
                label,
                acsConfig.zhangPppAr.temporal_product_transition_shadow
            );
			auto transformSummary = capture->second.summary();
			const string transformFailure = transformSummary.failureReason;
			const bool localPhysicalReinitialisation =
				label.find("local phase-coordinate reinitialisation")
					!= string::npos;
			const bool physicalFunctionalRetired =
				transformFailure.find(
					"PERSISTENT_FUNCTIONAL_NOT_TRANSPORTABLE_") == 0;
			bool physicalArcReset = false;
			if (!accepted
			 && localPhysicalReinitialisation
			 && physicalFunctionalRetired)
			{
				// This projection removed a direction used by the physical target.
				// It is a real arc/version boundary, not an S-basis exchange.  Close
				// the old chronology and re-anchor at the next accepted measurement.
				capture->second.resetForPhysicalArcChange();
				physicalArcReset = true;
			}
            BOOST_LOG_TRIVIAL(info)
                << "ZHANG_E18_FACTOR_CAPTURE time=" << time.to_string(0)
                << " event=EXACT_COORDINATE_TRANSFORM"
                << " label=" << label
                << " source_states=" << source.size()
                << " destination_states=" << destination.size()
                << " transform_nnz=" << transform.nonZeros()
				<< " status=" << (accepted
					? "ACCEPTED"
					: physicalArcReset ? "RESET" : "REJECTED")
				<< " physical_arc_reset=" << physicalArcReset
				<< " failure_reason="
				<< (transformFailure.empty() ? "NONE" : transformFailure)
				<< " persistent_target_mean_relative_error="
				<< transformSummary.maximumPersistentTransformMeanRelativeError
				<< " persistent_target_covariance_relative_error="
				<< transformSummary.maximumPersistentTransformCovarianceRelativeError
				<< " feedback=0";
        };
	e18ConfiguredFactorCaptureStates.insert(runtimeId);
	e18RuntimeObjectBindings[runtimeId] = owner;
}

void captureZhangE27WideLaneRawNoiseFactors(
    const KFState& kfState,
    GTime time,
    const string& receiver,
    const KFMeasEntryList& entries)
{
    if (!e27JointNoiseEnabled())
    {
        return;
    }
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(kfState, runtimeId))
	{
		return;
	}

    using ObservationKey = tuple<SatSys, KF, int>;
    map<ObservationKey, const KFMeasEntry*> observations;
    for (const KFMeasEntry& entry : entries)
    {
        if (!entry.valid || entry.obsKey.str != receiver ||
            (entry.obsKey.type != KF::CODE_MEAS &&
             entry.obsKey.type != KF::PHAS_MEAS))
        {
            continue;
        }
        observations[{entry.obsKey.Sat, entry.obsKey.type, entry.obsKey.num}] =
            &entry;
    }

    for (const auto& [system, codes] :
         acsConfig.zhangPppAr.baseline_observables)
    {
        if (codes.size() != 2)
        {
            continue;
        }
        const E_ObsCode firstCode = codes[0];
        const E_ObsCode secondCode = codes[1];
        const double lambdaFirst = wavelength(system, firstCode);
        const double lambdaSecond = wavelength(system, secondCode);
        if (!(lambdaFirst > 0) || !(lambdaSecond > lambdaFirst))
        {
            continue;
        }
        const double wideLaneWavelength = lambdaFirst * lambdaSecond /
            (lambdaSecond - lambdaFirst);
        const double narrowLaneWavelength = lambdaFirst * lambdaSecond /
            (lambdaSecond + lambdaFirst);
        const double codeRatio =
            narrowLaneWavelength / wideLaneWavelength;

        set<SatSys> satellites;
        for (const auto& [key, ignored] : observations)
        {
            if (std::get<0>(key).sys == system)
            {
                satellites.insert(std::get<0>(key));
            }
        }
        for (const SatSys& satellite : satellites)
        {
            const array<pair<ObservationKey, double>, 4> terms = {{
                {{satellite, KF::PHAS_MEAS, static_cast<int>(firstCode)},
                 +1 / lambdaFirst},
                {{satellite, KF::PHAS_MEAS, static_cast<int>(secondCode)},
                 -1 / lambdaSecond},
                {{satellite, KF::CODE_MEAS, static_cast<int>(firstCode)},
                 -codeRatio / lambdaFirst},
                {{satellite, KF::CODE_MEAS, static_cast<int>(secondCode)},
                 -codeRatio / lambdaSecond}
            }};
            bool complete = true;
            map<string, pair<double, double>> combined;
            for (const auto& [observationKey, scalar] : terms)
            {
                auto found = observations.find(observationKey);
                if (found == observations.end())
                {
                    complete = false;
                    break;
                }
                const KFMeasEntry& entry = *found->second;
                for (const auto& [noiseKey, coefficient] : entry.noiseEntryMap)
                {
                    auto variance = entry.noiseElementMap.find(noiseKey);
                    if (variance == entry.noiseElementMap.end() ||
                        !(variance->second > 0))
                    {
                        complete = false;
                        break;
                    }
                    const string stamped =
                        e27StampedNoiseIdentity(time, noiseKey);
                    auto& value = combined[stamped];
                    if (value.second > 0 &&
                        std::abs(value.second - variance->second) >
                            1e-10 * std::max(value.second, variance->second))
                    {
                        complete = false;
                        break;
                    }
                    value.first += scalar * coefficient;
                    value.second = variance->second;
                }
                if (!complete)
                {
                    break;
                }
            }
            if (!complete)
            {
                continue;
            }

            E27RawNoiseRow row;
            for (const auto& [key, value] : combined)
            {
                if (std::abs(value.first) <= 1e-15)
                {
                    continue;
                }
                row.stampedKeys.push_back(key);
            }
            row.coefficients = VectorXd::Zero(row.stampedKeys.size());
            row.variances = VectorXd::Zero(row.stampedKeys.size());
            for (int index = 0;
                 index < static_cast<int>(row.stampedKeys.size()); index++)
            {
                const auto& value = combined.at(row.stampedKeys[index]);
                row.coefficients(index) = value.first;
                row.variances(index) = value.second;
            }
            if (!row.stampedKeys.empty())
            {
                e27RawNoiseRows[{
					runtimeId, e27EpochIdentity(time), receiver,
                    system, satellite}] = std::move(row);
            }
        }
    }

    for (auto it = e27RawNoiseRows.begin(); it != e27RawNoiseRows.end();)
    {
		if (it->first.runtimeId == runtimeId
		 && it->first.epoch != e27EpochIdentity(time))
        {
            // The fixed-lag accumulator owns all older numerical rows.  This
            // cache only bridges pre-IF construction to the current AR pass.
            it = e27RawNoiseRows.erase(it);
        }
        else
        {
            ++it;
        }
    }
}

bool queryZhangE27WideLaneRawNoiseFactors(
    const KFState& kfState,
    GTime time,
    const string& receiver,
    E_Sys system,
    const SatSys& satellite,
    vector<string>& stampedNoiseKeys,
    VectorXd& coefficients,
    VectorXd& variances)
{
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(kfState, runtimeId))
	{
		return false;
	}
    auto found = e27RawNoiseRows.find({
		runtimeId, e27EpochIdentity(time), receiver, system, satellite});
    if (found == e27RawNoiseRows.end())
    {
        return false;
    }
    stampedNoiseKeys = found->second.stampedKeys;
    coefficients = found->second.coefficients;
    variances = found->second.variances;
    return !stampedNoiseKeys.empty() &&
        coefficients.size() == static_cast<int>(stampedNoiseKeys.size()) &&
        variances.size() == coefficients.size();
}

bool queryZhangE27IfWideLaneCrossCovariance(
    const KFState& kfState,
    const vector<KFKey>& stateKeys,
    const ZhangIfWideLaneEstimate& wideLane,
    MatrixXd& crossCovariance,
    string* failureReason)
{
    auto fail = [&](const string& reason)
    {
        if (failureReason)
        {
            *failureReason = reason;
        }
        crossCovariance.resize(0, 0);
        return false;
    };
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(kfState, runtimeId))
	{
		return fail("CHECKPOINT_RUNTIME_ID_UNBOUND");
	}
	auto runtime = e27JointNoiseRuntimes.find(runtimeId);
    if (runtime == e27JointNoiseRuntimes.end())
    {
        return fail("JOINT_NOISE_RUNTIME_MISSING");
    }
    if (!runtime->second.failureReason.empty())
    {
        return fail(runtime->second.failureReason);
    }
    if (!wideLane.valid || wideLane.mean.size() == 0 ||
        wideLane.noiseSensitivity.empty())
    {
        return fail("WL_NOISE_SENSITIVITY_MISSING");
    }
    vector<int> stateIndices;
    for (const KFKey& key : stateKeys)
    {
        auto found = kfState.kfIndexMap.find(key);
        if (found == kfState.kfIndexMap.end())
        {
            return fail("STATE_KEY_MISSING");
        }
        stateIndices.push_back(found->second);
    }
    crossCovariance = MatrixXd::Zero(stateKeys.size(), wideLane.mean.size());
    int sharedFactors = 0;
    for (const auto& [stamped, wlSensitivity] : wideLane.noiseSensitivity)
    {
        auto stateSensitivity = runtime->second.sensitivities.find(stamped);
        auto variance = wideLane.noiseVariance.find(stamped);
        if (stateSensitivity == runtime->second.sensitivities.end() ||
            variance == wideLane.noiseVariance.end())
        {
            continue;
        }
        if (stateSensitivity->second.stateDerivative.size() != kfState.x.size() ||
            wlSensitivity.size() != wideLane.mean.size() ||
            !(variance->second > 0))
        {
            return fail("SHARED_NOISE_FACTOR_DIMENSION_INVALID");
        }
        const double scale = std::max(
            stateSensitivity->second.variance, variance->second);
        if (std::abs(stateSensitivity->second.variance - variance->second) >
            1e-10 * std::max(1.0, scale))
        {
            return fail("SHARED_NOISE_VARIANCE_CONFLICT");
        }
        for (int row = 0; row < static_cast<int>(stateIndices.size()); row++)
        {
            crossCovariance.row(row) +=
                stateSensitivity->second.stateDerivative(stateIndices[row]) *
                variance->second * wlSensitivity.transpose();
        }
        sharedFactors++;
    }
    if (sharedFactors == 0 || !crossCovariance.allFinite())
    {
        return fail("NO_ACCEPTED_SHARED_RAW_NOISE_FACTORS");
    }
    if (failureReason)
    {
        *failureReason = "NONE";
    }
    return true;
}

bool recordZhangE18IntegerDatumTarget(
    Trace&              trace,
    const KFState&      captureOwner,
    const KFState&      state,
    E_Sys               system,
    const string&       targetFamily,
    const SatSys&       anchor,
    const SatSys&       satellite,
    const VectorXd&     currentCoordinateRow,
    double              persistentDatumOffsetCycles,
    bool                exactDatumTransportValid,
    const string&       canonicalCoordinateIdentity,
    const string&       productDatumIdentity,
    int                 productDatumVersion,
    const string&       topologyKey,
    const string&       gaugeComponentIdentity,
    const string&       phaseSegmentIdentity,
    const string&       physicalArcSignature,
    const vector<std::pair<string, int>>& physicalArcVersions,
    GTime               time)
{
    if (!acsConfig.zhangPppAr.fixed_lag_factor_capture_shadow)
    {
        return false;
    }
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(captureOwner, runtimeId))
	{
		return false;
	}
	auto capture = e18FactorCaptureBuffers.find(runtimeId);
    if (capture == e18FactorCaptureBuffers.end())
    {
        return false;
    }
	if (currentCoordinateRow.size() != state.x.size()
	 || !currentCoordinateRow.allFinite()
	 || !std::isfinite(persistentDatumOffsetCycles))
	{
        trace << "\nZHANG_E18_INTEGER_DATUM_TARGET time=" << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " topology_key=" << topologyKey
              << " anchor=" << anchor.id()
              << " satellite=" << satellite.id()
              << " status=REJECTED reason="
			  << "INVALID_INTEGER_DATUM_FUNCTIONAL"
              << " feedback=0";
        return false;
    }
    const VectorXd& row = currentCoordinateRow;
	// An unresolved z_T is an integer translation, not a continuous random
	// state.  Retaining Gk modulo Z preserves the fractional likelihood and
	// perr while still blocking absolute product publication.
	const double offset = exactDatumTransportValid
		? persistentDatumOffsetCycles
		: 0.0;
	const int unresolvedGaugeRank = exactDatumTransportValid ? 0 : 1;
	const string integerGaugeIdentity = exactDatumTransportValid
		? ""
		: enum_to_string(system) + ":" + targetFamily + ":"
			+ gaugeComponentIdentity;
	const string identity = enum_to_string(system) + ":" + targetFamily + ":" +
		anchor.id() + ":" + satellite.id();
	vector<ZhangCapturedPhysicalArcVersion> capturedArcVersions;
	for (const auto& [arc, version] : physicalArcVersions)
	{
		capturedArcVersions.push_back({arc, version});
	}
	double targetMean = offset;
	double targetVariance = 0;
	vector<std::pair<int, double>> nonZeros;
	for (int index = 0; index < row.size(); index++)
	{
		if (row(index) != 0)
		{
			nonZeros.push_back({index, row(index)});
			targetMean += row(index) * state.x(index);
		}
	}
	for (const auto& [left, leftCoefficient] : nonZeros)
	for (const auto& [right, rightCoefficient] : nonZeros)
	{
		targetVariance += leftCoefficient * rightCoefficient
			* state.P(left, right);
	}
    const bool accepted = capture->second.recordPhysicalTarget(
        time,
        identity,
        physicalArcSignature,
		phaseSegmentIdentity,
		capturedArcVersions,
        zhangKeysByIndex(state.kfIndexMap),
        row,
        offset,
        state.x,
		state.P,
		unresolvedGaugeRank,
		integerGaugeIdentity,
		canonicalCoordinateIdentity,
		productDatumIdentity,
		productDatumVersion
    );
	if (accepted && !capture->second.capturedPhysicalTargets().empty())
	{
		const auto& persisted = capture->second.capturedPhysicalTargets().back();
		targetMean = persisted.mean;
		targetVariance = persisted.variance;
	}
    const auto summary = capture->second.summary();
	const auto& retainedBlock = capture->second.currentRetainedBlock();
	std::ostringstream whitenedResiduals;
	for (int index = 0; index < retainedBlock.whitenedResidual.size(); index++)
	{
		if (index > 0)
		{
			whitenedResiduals << ";";
		}
		whitenedResiduals << retainedBlock.whitenedResidual(index);
	}
	trace << "\nZHANG_E18_INTEGER_DATUM_TARGET time=" << time.to_string(0)
		  << " system=" << enum_to_string(system)
		  << " target_family=" << targetFamily
          << " topology_key=" << topologyKey
          << " anchor=" << anchor.id()
          << " satellite=" << satellite.id()
		  << " phase_segment_identity=" << phaseSegmentIdentity
          << " physical_signature=" << physicalArcSignature
          << " mean=" << targetMean
          << " variance=" << targetVariance
		  << " datum_offset_cycles="
		  << (accepted
			&& !capture->second.capturedPhysicalTargets().empty()
				? capture->second.capturedPhysicalTargets().back().offset
				: offset)
		  << " canonical_coordinate_id=" << canonicalCoordinateIdentity
		  << " product_datum_id=" << productDatumIdentity
		  << " product_datum_version=" << productDatumVersion
		  << " coordinate=PRIMITIVE_BASE_INTEGER_TARGET"
		  << " quotient_valid=" << accepted
		  << " absolute_datum_valid="
		  << (accepted && exactDatumTransportValid)
		  << " unresolved_gauge_rank=" << unresolvedGaugeRank
          << " targets=" << summary.physicalTargets
		  << " physical_identity_resets="
		  << summary.physicalTargetIdentityResets
		  << " coordinate_continuations="
		  << summary.physicalTargetCoordinateContinuations
          << " target_mean_replay_relative_error="
          << summary.maximumTargetMeanRelativeError
          << " target_variance_replay_relative_error="
          << summary.maximumTargetVarianceRelativeError
		  << " retained_block_targets=" << retainedBlock.targetCount
		  << " retained_block_rank=" << retainedBlock.informationRank
		  << " retained_block_residual_domain=PREFIT_INNOVATION"
		  << " retained_block_residual_dof=" << retainedBlock.residualDof
		  << " retained_block_projected_gauge_rank="
		  << retainedBlock.projectedGaugeRank
		  << " retained_block_whitened_squared_norm="
		  << retainedBlock.whitenedSquaredNorm
		  << " retained_block_whitened_residuals="
		  << (whitenedResiduals.str().empty() ? "NONE" : whitenedResiduals.str())
		  << " retained_block_valid=" << retainedBlock.valid
		  << " retained_block_reason="
		  << (retainedBlock.failureReason.empty()
				? "NONE" : retainedBlock.failureReason)
		  << " status=" << (accepted
				? (exactDatumTransportValid
					? "ACCEPTED_ABSOLUTE_DATUM"
					: "ACCEPTED_INTEGER_QUOTIENT")
				: "REJECTED")
          << " reason="
		  << (!accepted
				? (!capture->second.lastTargetReason().empty()
					? capture->second.lastTargetReason()
					: summary.failureReason.empty()
						? "UNKNOWN" : summary.failureReason)
				: (exactDatumTransportValid
					? "NONE" : "INTEGER_GAUGE_UNRESOLVED"))
          << " feedback=0";
    return accepted;
}

namespace
{
struct ZhangOperationalLambdaResult
{
	bool valid = false;
	bool validationPass = false;
	VectorXd best;
	VectorXd second;
	MatrixXd decorrelation;
	MatrixXd reducedCovariance;
	VectorXd conditionalVariances;
	VectorXd conditionalSuccessRates;
	VectorXd reducedBest;
	VectorXd reducedSecond;
	double bestDistance = std::numeric_limits<double>::quiet_NaN();
	double secondDistance = std::numeric_limits<double>::quiet_NaN();
	double bootstrappedSuccessRate = std::numeric_limits<double>::quiet_NaN();
	double bootstrapImplementationConsistencyError =
		std::numeric_limits<double>::quiet_NaN();
	double ambiguityDilutionOfPrecision =
		std::numeric_limits<double>::quiet_NaN();
	double covarianceTransformMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	double conditionalDeterminantLogError =
		std::numeric_limits<double>::quiet_NaN();
	double bestCandidateBackTransformMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	double secondCandidateBackTransformMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	double reducedCandidateIntegerMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	bool transformUnimodular = false;
	bool candidateBackTransformConsistent = false;
	std::string failureReason;
};

ZhangOperationalLambdaResult runZhangOperationalLambda(
	Trace& trace,
	const VectorXd& mean,
	const MatrixXd& covariance)
{
	ZhangOperationalLambdaResult result;
	if (mean.size() == 0
	 || covariance.rows() != mean.size()
	 || covariance.cols() != mean.size()
	 || !mean.allFinite() || !covariance.allFinite())
	{
		result.failureReason = "INVALID_OPERATIONAL_LAMBDA_DIMENSIONS";
		return result;
	}
	const MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
	Eigen::SelfAdjointEigenSolver<MatrixXd> spectrum(symmetric);
	if (spectrum.info() != Eigen::Success
	 || spectrum.eigenvalues().minCoeff() <= 0)
	{
		result.failureReason = "NON_POSITIVE_OPERATIONAL_LAMBDA_COVARIANCE";
		return result;
	}
	std::vector<double> candidates(mean.size() * 2);
	double distances[2] = {};
	const int status = lambdaWithTransform(
		trace, mean.size(), 2, mean.data(), symmetric.data(),
		candidates.data(), distances, acsConfig.predefined_fail,
		result.validationPass, result.decorrelation,
		result.reducedCovariance, result.conditionalVariances,
		result.conditionalSuccessRates,
		result.bootstrappedSuccessRate);
	if (status != 0)
	{
		result.failureReason = "OPERATIONAL_LAMBDA_FAILED_"
			+ std::to_string(status);
		return result;
	}
	result.best = Eigen::Map<VectorXd>(candidates.data(), mean.size());
	result.second = Eigen::Map<VectorXd>(
		candidates.data() + mean.size(), mean.size());
	result.bestDistance = distances[0];
	result.secondDistance = distances[1];
	const auto reductionAudit = zhangAuditLambdaReduction(
		symmetric, result.decorrelation, result.reducedCovariance,
		result.conditionalVariances, result.best, result.second);
	result.conditionalSuccessRates = reductionAudit.conditionalSuccessRates;
	result.reducedBest = reductionAudit.reducedBestCandidate;
	result.reducedSecond = reductionAudit.reducedSecondCandidate;
	result.ambiguityDilutionOfPrecision =
		reductionAudit.ambiguityDilutionOfPrecision;
	result.covarianceTransformMaximumError =
		reductionAudit.covarianceTransformMaximumError;
	result.conditionalDeterminantLogError =
		reductionAudit.conditionalDeterminantLogError;
	result.bestCandidateBackTransformMaximumError =
		reductionAudit.bestCandidateBackTransformMaximumError;
	result.secondCandidateBackTransformMaximumError =
		reductionAudit.secondCandidateBackTransformMaximumError;
	result.reducedCandidateIntegerMaximumError =
		reductionAudit.reducedCandidateIntegerMaximumError;
	result.transformUnimodular = reductionAudit.transformUnimodular;
	result.candidateBackTransformConsistent =
		reductionAudit.candidateBackTransformConsistent;
	result.bootstrapImplementationConsistencyError = std::abs(
		result.bootstrappedSuccessRate
		- reductionAudit.jointBootstrappedSuccessRate);
	constexpr double bootstrapAuditTolerance = 5e-7;
	result.valid = result.best.allFinite() && result.second.allFinite()
		&& result.decorrelation.allFinite()
		&& result.reducedCovariance.allFinite()
		&& result.conditionalVariances.allFinite()
		&& result.conditionalSuccessRates.allFinite()
		&& std::isfinite(result.bootstrappedSuccessRate)
		&& std::isfinite(result.ambiguityDilutionOfPrecision)
		&& reductionAudit.valid
		&& result.bootstrapImplementationConsistencyError
			<= bootstrapAuditTolerance;
	if (!result.valid)
	{
		if (!reductionAudit.valid)
		{
			result.failureReason = reductionAudit.failureReason;
		}
		else if (result.bootstrapImplementationConsistencyError
			> bootstrapAuditTolerance)
		{
			result.failureReason =
				"OPERATIONAL_LAMBDA_BOOTSTRAP_AUDIT_MISMATCH";
		}
		else
		{
			result.failureReason = "NONFINITE_OPERATIONAL_LAMBDA_RESULT";
		}
	}
	return result;
}

ZhangIntegerVector zhangIntegerCandidate(const VectorXd& candidate)
{
	ZhangIntegerVector integer(candidate.size());
	for (int index = 0; index < candidate.size(); index++)
	{
		integer(index) = std::llround(candidate(index));
	}
	return integer;
}

std::vector<int> zhangSelectOperationalParSubset(
	Trace& trace,
	const VectorXd& mean,
	const MatrixXd& covariance,
	double successThreshold,
	double& achievedSuccess)
{
	std::vector<int> retained(mean.size());
	std::iota(retained.begin(), retained.end(), 0);
	auto evaluate = [&](const std::vector<int>& indices)
	{
		VectorXd subsetMean(indices.size());
		MatrixXd subsetCovariance(indices.size(), indices.size());
		for (int row = 0; row < static_cast<int>(indices.size()); row++)
		{
			subsetMean(row) = mean(indices[row]);
			for (int column = 0;
				 column < static_cast<int>(indices.size()); column++)
			{
				subsetCovariance(row, column) =
					covariance(indices[row], indices[column]);
			}
		}
		return runZhangOperationalLambda(trace, subsetMean, subsetCovariance);
	};
	auto current = evaluate(retained);
	achievedSuccess = current.valid
		? current.bootstrappedSuccessRate
		: std::numeric_limits<double>::quiet_NaN();
	while (retained.size() > 1
		&& (!current.valid || achievedSuccess < successThreshold))
	{
		double bestSuccess = -1;
		std::vector<int> bestSubset;
		for (int removed = 0;
			 removed < static_cast<int>(retained.size()); removed++)
		{
			std::vector<int> candidate = retained;
			candidate.erase(candidate.begin() + removed);
			const auto candidateResult = evaluate(candidate);
			if (candidateResult.valid
			 && candidateResult.bootstrappedSuccessRate > bestSuccess)
			{
				bestSuccess = candidateResult.bootstrappedSuccessRate;
				bestSubset = std::move(candidate);
			}
		}
		if (bestSubset.empty())
		{
			retained.clear();
			break;
		}
		retained = std::move(bestSubset);
		current = evaluate(retained);
		achievedSuccess = current.valid
			? current.bootstrappedSuccessRate
			: std::numeric_limits<double>::quiet_NaN();
	}
	if (!current.valid || achievedSuccess < successThreshold)
	{
		retained.clear();
	}
	return retained;
}

MatrixXd zhangProductRelationIncidence(
	const std::vector<std::string>& relations)
{
	std::map<std::string, int> nodeIndex;
	for (const auto& relation : relations)
	{
		const auto delimiter = relation.find("->");
		if (delimiter == std::string::npos)
		{
			continue;
		}
		nodeIndex.emplace(
			relation.substr(0, delimiter), nodeIndex.size());
		nodeIndex.emplace(
			relation.substr(delimiter + 2), nodeIndex.size());
	}
	MatrixXd incidence = MatrixXd::Zero(relations.size(), nodeIndex.size());
	for (int row = 0; row < static_cast<int>(relations.size()); row++)
	{
		const auto delimiter = relations[row].find("->");
		if (delimiter == std::string::npos)
		{
			continue;
		}
		incidence(row, nodeIndex.at(relations[row].substr(0, delimiter))) = -1;
		incidence(row, nodeIndex.at(relations[row].substr(delimiter + 2))) = 1;
	}
	return incidence;
}

void traceZhangIntegerDiagnostic(
	Trace& trace,
	GTime time,
	const std::string& strategy,
	const VectorXd& mean,
	const MatrixXd& covariance,
	const std::vector<std::string>& labels,
	const std::vector<std::string>& relations,
	int quotientRank,
	int absoluteRank,
	bool transformUnimodular,
	const std::vector<int>& sourceIndices = {})
{
	const auto solution = runZhangOperationalLambda(trace, mean, covariance);
	const MatrixXd productIncidence = zhangProductRelationIncidence(relations);
	const MatrixXd noRedundantCycles(0, mean.size());
	ZhangLambdaParDiagnostics diagnostics;
	if (solution.valid)
	{
		diagnostics = zhangEvaluateLambdaParCandidates(
			mean, covariance,
			zhangIntegerCandidate(solution.best),
			zhangIntegerCandidate(solution.second),
			quotientRank, absoluteRank, productIncidence,
			noRedundantCycles, 0.999);
	}
	const int conditionalDirectionPassCount = solution.valid
		? (solution.conditionalSuccessRates.array() >= 0.999).count() : 0;
	const bool jointReliabilityPass = solution.valid
		&& solution.validationPass
		&& solution.bootstrappedSuccessRate >= 0.999;
	trace << "\nZHANG_E18_INTEGER_DIAGNOSTIC time=" << time.to_string(0)
		<< " strategy=" << strategy
		<< " valid=" << (solution.valid && diagnostics.valid)
		<< " target_count=" << mean.size()
		<< " quotient_valid_rank=" << quotientRank
		<< " absolute_valid_rank=" << absoluteRank
		<< " product_relation_graph_rank="
		<< diagnostics.productRelationGraphRank
		<< " conditional_direction_pass_count="
		<< conditionalDirectionPassCount
		<< " recoverable_satellite_count="
		<< diagnostics.recoverableSatelliteCount
		<< " best_candidate_distance=" << solution.bestDistance
		<< " second_candidate_distance=" << solution.secondDistance
		<< " second_to_best_distance_ratio="
		<< (solution.bestDistance > 0
			? solution.secondDistance / solution.bestDistance
			: std::numeric_limits<double>::infinity())
		<< " joint_bootstrapped_success_rate="
		<< solution.bootstrappedSuccessRate
		<< " bootstrap_implementation_consistency_error="
		<< solution.bootstrapImplementationConsistencyError
		<< " lambda_validation_pass=" << solution.validationPass
		<< " joint_reliability_pass=" << jointReliabilityPass
		<< " reliability_gate=JOINT_BOOTSTRAP_AND_FFRT"
		<< " ambiguity_dilution_of_precision="
		<< solution.ambiguityDilutionOfPrecision
		<< " lambda_transform_unimodular="
		<< solution.transformUnimodular
		<< " candidate_back_transform_consistent="
		<< solution.candidateBackTransformConsistent
		<< " covariance_transform_maximum_error="
		<< solution.covarianceTransformMaximumError
		<< " conditional_determinant_log_error="
		<< solution.conditionalDeterminantLogError
		<< " best_candidate_back_transform_maximum_error="
		<< solution.bestCandidateBackTransformMaximumError
		<< " second_candidate_back_transform_maximum_error="
		<< solution.secondCandidateBackTransformMaximumError
		<< " reduced_candidate_integer_maximum_error="
		<< solution.reducedCandidateIntegerMaximumError
		<< " maximum_cycle_closure_error="
		<< diagnostics.maximumCycleClosureError
		<< " cycle_constraint_count=0"
		<< " transform_unimodular=" << transformUnimodular
		<< " target_labels=";
	for (int index = 0; index < static_cast<int>(labels.size()); index++)
	{
		if (index) trace << ";";
		trace << labels[index];
	}
	trace << " best_candidate=";
	for (int index = 0; index < solution.best.size(); index++)
	{
		if (index) trace << ";";
		trace << std::llround(solution.best(index));
	}
	trace << " second_candidate=";
	for (int index = 0; index < solution.second.size(); index++)
	{
		if (index) trace << ";";
		trace << std::llround(solution.second(index));
	}
	trace << " lambda_Z=";
	for (int row = 0; row < solution.decorrelation.rows(); row++)
	for (int column = 0; column < solution.decorrelation.cols(); column++)
	{
		if (row || column) trace << ";";
		trace << std::llround(solution.decorrelation(row, column));
	}
	trace << " reduced_covariance=";
	for (int row = 0; row < solution.reducedCovariance.rows(); row++)
	for (int column = 0; column < solution.reducedCovariance.cols(); column++)
	{
		if (row || column) trace << ";";
		trace << solution.reducedCovariance(row, column);
	}
	trace << " conditional_variances=";
	for (int index = 0; index < solution.conditionalVariances.size(); index++)
	{
		if (index) trace << ";";
		trace << solution.conditionalVariances(index);
	}
	trace << " conditional_success_rates=";
	for (int index = 0; index < solution.conditionalSuccessRates.size(); index++)
	{
		if (index) trace << ";";
		trace << solution.conditionalSuccessRates(index);
	}
	trace << " reduced_best_candidate=";
	for (int index = 0; index < solution.reducedBest.size(); index++)
	{
		if (index) trace << ";";
		trace << std::llround(solution.reducedBest(index));
	}
	trace << " reduced_second_candidate=";
	for (int index = 0; index < solution.reducedSecond.size(); index++)
	{
		if (index) trace << ";";
		trace << std::llround(solution.reducedSecond(index));
	}
	trace << " source_indices=";
	if (sourceIndices.empty())
	{
		trace << "ALL";
	}
	else
	{
		for (int index = 0; index < static_cast<int>(sourceIndices.size()); index++)
		{
			if (index) trace << ";";
			trace << sourceIndices[index];
		}
	}
	trace << " status=" << (solution.valid && diagnostics.valid
			? "EVALUATED" : "REJECTED")
		<< " reason=" << (!solution.valid ? solution.failureReason
			: !diagnostics.valid ? diagnostics.failureReason : "NONE")
		<< " feedback=0";
}

template<typename Marginal>
void traceZhangE18IntegerDiagnostics(
	Trace& trace,
	GTime time,
	const Marginal& marginal,
	const std::string& strategyPrefix)
{
	if (!marginal.valid)
	{
		return;
	}
	const auto quotient = zhangBuildIntegerQuotientCoordinates(
		marginal.identities, marginal.gaugeIdentities,
		marginal.absoluteValidity, marginal.mean, marginal.covariance);
	if (!quotient.valid)
	{
		trace << "\nZHANG_E18_INTEGER_DIAGNOSTIC time=" << time.to_string(0)
			<< " strategy=" << strategyPrefix
			<< "QUOTIENT_CONSTRUCTION valid=0 status=REJECTED reason="
			<< quotient.failureReason << " feedback=0";
		return;
	}
	traceZhangIntegerDiagnostic(
		trace, time, strategyPrefix + "DIRECT_JOINT",
		quotient.mean, quotient.covariance,
		quotient.labels, quotient.relations, marginal.quotientValidRank,
		marginal.absoluteValidRank, true);

	const auto wideLane = zhangBuildWideLaneL1BlockCoordinates(quotient);
	if (wideLane.valid)
	{
		const MatrixXd transform = wideLane.transform.template cast<double>();
		traceZhangIntegerDiagnostic(
			trace, time, strategyPrefix + "WL_L1_UNIMODULAR",
			transform.transpose() * quotient.mean,
			transform.transpose() * quotient.covariance * transform,
			wideLane.labels, quotient.relations,
			marginal.quotientValidRank, marginal.absoluteValidRank, true);
	}
	else
	{
		trace << "\nZHANG_E18_INTEGER_DIAGNOSTIC time=" << time.to_string(0)
			<< " strategy=" << strategyPrefix
			<< "WL_L1_UNIMODULAR valid=0 status=REJECTED reason="
			<< wideLane.failureReason << " feedback=0";
	}

	double parSuccess = std::numeric_limits<double>::quiet_NaN();
	const std::vector<int> par = zhangSelectOperationalParSubset(
		trace, quotient.mean, quotient.covariance, 0.999, parSuccess);
	if (!par.empty())
	{
		VectorXd parMean(par.size());
		MatrixXd parCovariance(par.size(), par.size());
		std::vector<std::string> parLabels;
		std::vector<std::string> parRelations;
		for (int row = 0; row < static_cast<int>(par.size()); row++)
		{
			parMean(row) = quotient.mean(par[row]);
			parLabels.push_back(quotient.labels[par[row]]);
			parRelations.push_back(quotient.relations[par[row]]);
			for (int column = 0; column < static_cast<int>(par.size()); column++)
			{
				parCovariance(row, column) =
					quotient.covariance(par[row], par[column]);
			}
		}
		traceZhangIntegerDiagnostic(
			trace, time, strategyPrefix + "PAR_OPERATIONAL_SUBSET",
			parMean, parCovariance,
			parLabels, parRelations, par.size(), 0, true, par);
	}
	else
	{
		trace << "\nZHANG_E18_INTEGER_DIAGNOSTIC time=" << time.to_string(0)
			<< " strategy=" << strategyPrefix
			<< "PAR_OPERATIONAL_SUBSET valid=0 target_count=0"
			<< " joint_bootstrapped_success_rate=" << parSuccess
			<< " status=REJECTED reason=NO_SUBSET_REACHES_0.999 feedback=0";
	}
}
}

void traceZhangE18RawIntegerDatumWindow(
    Trace& trace,
    const KFState& captureOwner,
    GTime time)
{
    if (!acsConfig.zhangPppAr.fixed_lag_factor_capture_shadow)
    {
        return;
    }
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(captureOwner, runtimeId))
	{
		return;
	}
	auto capture = e18FactorCaptureBuffers.find(runtimeId);
    if (capture == e18FactorCaptureBuffers.end())
    {
        return;
    }
	const auto summary = capture->second.summary();
	const int evaluationStride = std::max(
		1, acsConfig.zhangPppAr.fixed_lag_factor_capture_evaluation_stride);
	if (summary.measurements == 0
	 || summary.measurements % evaluationStride != 0)
	{
		return;
	}
	const ZhangRawSquareRootTargetMarginal rawMarginal =
		capture->second.currentRawSquareRootTargetMarginal();
	trace << "\nZHANG_E18_RAW_SQUARE_ROOT_WINDOW time="
		<< time.to_string(0)
		<< " valid=" << rawMarginal.valid
		<< " quotient_valid="
		<< (rawMarginal.valid && rawMarginal.quotientValidRank > 0)
		<< " absolute_datum_valid="
		<< (rawMarginal.valid
			&& rawMarginal.absoluteValidRank
				== rawMarginal.requestedTargetCount)
		<< " requested_targets=" << rawMarginal.requestedTargetCount
		<< " unresolved_gauge_rank=" << rawMarginal.unresolvedGaugeRank
		<< " information_rank=" << rawMarginal.informationRank
		<< " quotient_valid_rank=" << rawMarginal.quotientValidRank
		<< " absolute_valid_rank=" << rawMarginal.absoluteValidRank
		<< " batch_orthogonal_residual_dof="
		<< rawMarginal.batchOrthogonalDof
		<< " batch_orthogonal_residual_squared_norm="
		<< rawMarginal.batchOrthogonalSquaredNorm
		<< " boundary_rows=" << rawMarginal.storedRows
		<< " boundary_columns=" << rawMarginal.storedColumns
		<< " maximum_boundary_rows=" << rawMarginal.maximumStoredRows
		<< " maximum_boundary_columns=" << rawMarginal.maximumStoredColumns
		<< " target_identities=";
	for (int index = 0;
		 index < static_cast<int>(rawMarginal.identities.size()); index++)
	{
		if (index) trace << ";";
		trace << rawMarginal.identities[index];
	}
	if (rawMarginal.identities.empty()) trace << "NONE";
	trace << " target_gauge_identities=";
	for (int index = 0;
		 index < static_cast<int>(rawMarginal.gaugeIdentities.size()); index++)
	{
		if (index) trace << ";";
		trace << (rawMarginal.gaugeIdentities[index].empty()
			? "ABSOLUTE" : rawMarginal.gaugeIdentities[index]);
	}
	if (rawMarginal.gaugeIdentities.empty()) trace << "NONE";
	trace << " target_absolute_valid=";
	for (int index = 0;
		 index < static_cast<int>(rawMarginal.absoluteValidity.size()); index++)
	{
		if (index) trace << ";";
		trace << rawMarginal.absoluteValidity[index];
	}
	if (rawMarginal.absoluteValidity.empty()) trace << "NONE";
	trace << " target_covariance_row_major=";
	for (int row = 0; row < rawMarginal.covariance.rows(); row++)
	for (int column = 0; column < rawMarginal.covariance.cols(); column++)
	{
		if (row || column) trace << ";";
		trace << rawMarginal.covariance(row, column);
	}
	if (rawMarginal.covariance.rows() == 0) trace << "NONE";
	trace << " target_mean=";
	for (int index = 0; index < rawMarginal.mean.size(); index++)
	{
		if (index) trace << ";";
		trace << rawMarginal.mean(index);
	}
	if (rawMarginal.mean.size() == 0) trace << "NONE";
	trace << " status=" << (rawMarginal.valid ? "ACCEPTED" : "REJECTED")
		<< " reason=" << (rawMarginal.failureReason.empty()
			? "NONE" : rawMarginal.failureReason)
		<< " source=FINAL_ACCEPTED_H_R_F_Q_SQUARE_ROOT feedback=0";
	traceZhangE18IntegerDiagnostics(
		trace, time, rawMarginal, "RAW_SQUARE_ROOT_");

	const ZhangRawSquareRootTargetMarginal persistentMarginal =
		capture->second.currentPersistentRawTargetMarginal();
	trace << "\nZHANG_E19_PERSISTENT_RAW_TARGET_WINDOW time="
		<< time.to_string(0)
		<< " valid=" << persistentMarginal.valid
		<< " requested_targets=" << persistentMarginal.requestedTargetCount
		<< " information_rank=" << persistentMarginal.informationRank
		<< " unresolved_gauge_rank="
		<< persistentMarginal.unresolvedGaugeRank
		<< " quotient_valid_rank=" << persistentMarginal.quotientValidRank
		<< " absolute_valid_rank=" << persistentMarginal.absoluteValidRank
		<< " exact_constraints_applied="
		<< persistentMarginal.exactConstraintsApplied
		<< " coordinate_representable_targets="
		<< persistentMarginal.coordinateRepresentableTargets
		<< " coordinate_unrepresentable_targets="
		<< persistentMarginal.coordinateUnrepresentableTargets
		<< " skipped_unrepresentable_rebinds="
		<< persistentMarginal.skippedUnrepresentableRebinds
		<< " batch_orthogonal_residual_dof="
		<< persistentMarginal.batchOrthogonalDof
		<< " batch_orthogonal_residual_squared_norm="
		<< persistentMarginal.batchOrthogonalSquaredNorm
		<< " boundary_rows=" << persistentMarginal.storedRows
		<< " boundary_columns=" << persistentMarginal.storedColumns
		<< " target_identities=";
	for (int index = 0;
		 index < static_cast<int>(persistentMarginal.identities.size()); index++)
	{
		if (index) trace << ";";
		trace << persistentMarginal.identities[index];
	}
	if (persistentMarginal.identities.empty()) trace << "NONE";
	trace << " target_gauge_identities=";
	for (int index = 0;
		 index < static_cast<int>(persistentMarginal.gaugeIdentities.size()); index++)
	{
		if (index) trace << ";";
		trace << (persistentMarginal.gaugeIdentities[index].empty()
			? "ABSOLUTE" : persistentMarginal.gaugeIdentities[index]);
	}
	if (persistentMarginal.gaugeIdentities.empty()) trace << "NONE";
	trace << " target_absolute_valid=";
	for (int index = 0;
		 index < static_cast<int>(persistentMarginal.absoluteValidity.size()); index++)
	{
		if (index) trace << ";";
		trace << persistentMarginal.absoluteValidity[index];
	}
	if (persistentMarginal.absoluteValidity.empty()) trace << "NONE";
	trace << " target_covariance_row_major=";
	for (int row = 0; row < persistentMarginal.covariance.rows(); row++)
	for (int column = 0;
		 column < persistentMarginal.covariance.cols(); column++)
	{
		if (row || column) trace << ";";
		trace << persistentMarginal.covariance(row, column);
	}
	if (persistentMarginal.covariance.size() == 0) trace << "NONE";
	trace << " target_mean=";
	for (int index = 0; index < persistentMarginal.mean.size(); index++)
	{
		if (index) trace << ";";
		trace << persistentMarginal.mean(index);
	}
	if (persistentMarginal.mean.size() == 0) trace << "NONE";
	trace << " status=" << (persistentMarginal.valid
			? "ACCEPTED" : "REJECTED")
		<< " reason=" << (persistentMarginal.failureReason.empty()
			? "NONE" : persistentMarginal.failureReason)
		<< " source=PERSISTENT_RAW_TARGET_EXACT_CONSTRAINT feedback=0";
	traceZhangE18IntegerDiagnostics(
		trace, time, persistentMarginal, "PERSISTENT_RAW_TARGET_");
	for (const auto& scale :
		capture->second.innovationScaleDiagnostics())
	{
		trace << "\nZHANG_E19_INNOVATION_SCALE_GROUP time="
			<< time.to_string(0)
			<< " group=" << scale.identity
			<< " blocks=" << scale.blocks
			<< " marginal_samples=" << scale.samples
			<< " marginal_standardised_squared_sum="
			<< scale.marginalStandardisedSquaredSum
			<< " predictive_covariance_scale_mle="
			<< scale.predictiveCovarianceScaleMle()
			<< " maximum_absolute_prefit_ratio="
			<< scale.maximumAbsoluteRatio
			<< " statistic=MARGINAL_PREFIT_RATIO_NOT_JOINT_CHI_SQUARE"
			<< " role=TRAINING_HOLDOUT_DIAGNOSTIC_ONLY feedback=0";
	}

	const ZhangIncrementalTargetMarginal marginal =
		capture->second.currentIncrementalTargetMarginal();
	trace << "\nZHANG_E18_INCREMENTAL_INTEGER_WINDOW time="
		  << time.to_string(0)
		  << " valid=" << marginal.valid
		  << " quotient_valid="
		  << (marginal.valid && marginal.quotientValidRank > 0)
		  << " absolute_datum_valid="
		  << (marginal.valid
			&& marginal.absoluteValidRank == marginal.requestedTargetCount)
		  << " requested_targets=" << marginal.requestedTargetCount
		  << " unresolved_gauge_rank=" << marginal.unresolvedGaugeRank
		  << " information_rank=" << marginal.informationRank
		  << " quotient_valid_rank=" << marginal.quotientValidRank
		  << " absolute_valid_rank=" << marginal.absoluteValidRank
		  << " orthogonal_residual_dof=" << marginal.orthogonalResidualDof
		  << " orthogonal_residual_squared_norm="
		  << marginal.orthogonalResidualSquaredNorm
		  << " separator_rows=" << marginal.storedRows
		  << " separator_columns=" << marginal.storedColumns
		  << " maximum_separator_rows=" << marginal.maximumStoredRows
		  << " maximum_separator_columns=" << marginal.maximumStoredColumns
		  << " target_identities=";
	if (marginal.identities.empty())
	{
		trace << "NONE";
	}
	else
	{
		for (int index = 0; index < static_cast<int>(marginal.identities.size()); index++)
		{
			if (index > 0) trace << ";";
			trace << marginal.identities[index];
		}
	}
	trace << " target_gauge_identities=";
	if (marginal.gaugeIdentities.empty())
	{
		trace << "NONE";
	}
	else
	{
		for (int index = 0;
			 index < static_cast<int>(marginal.gaugeIdentities.size());
			 index++)
		{
			if (index > 0) trace << ";";
			trace << (marginal.gaugeIdentities[index].empty()
				? "ABSOLUTE" : marginal.gaugeIdentities[index]);
		}
	}
	trace << " target_absolute_valid=";
	if (marginal.absoluteValidity.empty())
	{
		trace << "NONE";
	}
	else
	{
		for (int index = 0;
			 index < static_cast<int>(marginal.absoluteValidity.size());
			 index++)
		{
			if (index > 0) trace << ";";
			trace << marginal.absoluteValidity[index];
		}
	}
	trace << " target_coordinate_offsets=";
	if (marginal.coordinateOffsets.empty())
	{
		trace << "NONE";
	}
	else
	{
		for (int index = 0;
			 index < static_cast<int>(marginal.coordinateOffsets.size());
			 index++)
		{
			if (index > 0) trace << ";";
			trace << marginal.coordinateOffsets[index];
		}
	}
	trace << " target_covariance_row_major=";
	if (marginal.covariance.rows() == 0)
	{
		trace << "NONE";
	}
	else
	{
		for (int row = 0; row < marginal.covariance.rows(); row++)
		for (int column = 0; column < marginal.covariance.cols(); column++)
		{
			if (row > 0 || column > 0) trace << ";";
			trace << marginal.covariance(row, column);
		}
	}
	trace
		  << " target_mean=";
    if (marginal.mean.size() == 0)
    {
        trace << "NONE";
    }
    else
    {
        for (int index = 0; index < marginal.mean.size(); index++)
        {
            if (index > 0) trace << ";";
            trace << marginal.mean(index);
        }
    }
    trace << " target_variance_diagonal=";
    if (marginal.covariance.rows() == 0)
    {
        trace << "NONE";
    }
	else
	{
		for (int index = 0; index < marginal.covariance.rows(); index++)
		{
			if (index > 0) trace << ";";
			trace << marginal.covariance(index, index);
		}
	}
	trace << " target_fractional_mean=";
	if (marginal.fractionalMean.size() == 0)
	{
		trace << "NONE";
	}
	else
	{
		for (int index = 0; index < marginal.fractionalMean.size(); index++)
		{
			if (index > 0) trace << ";";
			trace << marginal.fractionalMean(index);
		}
	}
	trace << " status=" << (marginal.valid ? "ACCEPTED" : "REJECTED")
		  << " reason="
		  << (marginal.failureReason.empty() ? "NONE" : marginal.failureReason)
		  << " source=INCREMENTAL_TARGET_SEPARATOR feedback=0";

	// Compare only identical physical coordinates.  A covariance difference is
	// otherwise contaminated by a datum or coordinate change and cannot
	// distinguish stochastic scaling from information discarded by the
	// epoch-local separator.
	std::vector<int> rawCommon;
	std::vector<int> incrementalCommon;
	std::vector<std::string> commonIdentities;
	if (rawMarginal.valid && marginal.valid)
	{
		for (int rawIndex = 0;
			 rawIndex < static_cast<int>(rawMarginal.identities.size());
			 rawIndex++)
		{
			for (int incrementalIndex = 0;
				 incrementalIndex < static_cast<int>(marginal.identities.size());
				 incrementalIndex++)
			{
				if (rawMarginal.identities[rawIndex]
						!= marginal.identities[incrementalIndex]
				 || rawMarginal.gaugeIdentities[rawIndex]
						!= marginal.gaugeIdentities[incrementalIndex]
				 || rawMarginal.absoluteValidity[rawIndex]
						!= marginal.absoluteValidity[incrementalIndex])
				{
					continue;
				}
				rawCommon.push_back(rawIndex);
				incrementalCommon.push_back(incrementalIndex);
				commonIdentities.push_back(rawMarginal.identities[rawIndex]);
				break;
			}
		}
	}
	const int commonCount = rawCommon.size();
	VectorXd rawCommonMean(commonCount);
	VectorXd incrementalCommonMean(commonCount);
	MatrixXd rawCommonCovariance(commonCount, commonCount);
	MatrixXd incrementalCommonCovariance(commonCount, commonCount);
	for (int row = 0; row < commonCount; row++)
	{
		rawCommonMean(row) = rawMarginal.mean(rawCommon[row]);
		incrementalCommonMean(row) = marginal.mean(incrementalCommon[row]);
		for (int column = 0; column < commonCount; column++)
		{
			rawCommonCovariance(row, column) = rawMarginal.covariance(
				rawCommon[row], rawCommon[column]);
			incrementalCommonCovariance(row, column) = marginal.covariance(
				incrementalCommon[row], incrementalCommon[column]);
		}
	}
	auto informationMatrix = [](const MatrixXd& covariance,
		MatrixXd& information, int& rank)
	{
		rank = 0;
		information = MatrixXd::Zero(covariance.rows(), covariance.cols());
		if (covariance.rows() == 0 || covariance.rows() != covariance.cols())
		{
			return false;
		}
		Eigen::SelfAdjointEigenSolver<MatrixXd> spectrum(
			0.5 * (covariance + covariance.transpose()));
		if (spectrum.info() != Eigen::Success)
		{
			return false;
		}
		const double maximum = spectrum.eigenvalues().cwiseAbs().maxCoeff();
		const double threshold = std::max(1e-14, maximum * 1e-12);
		VectorXd inverse = VectorXd::Zero(covariance.rows());
		for (int index = 0; index < covariance.rows(); index++)
		{
			if (spectrum.eigenvalues()(index) > threshold)
			{
				inverse(index) = 1 / spectrum.eigenvalues()(index);
				rank++;
			}
		}
		information = spectrum.eigenvectors() * inverse.asDiagonal()
			* spectrum.eigenvectors().transpose();
		information = 0.5 * (information + information.transpose());
		return information.allFinite() && rank > 0;
	};
	MatrixXd rawInformation;
	MatrixXd incrementalInformation;
	int rawInformationRank = 0;
	int incrementalInformationRank = 0;
	const bool comparisonValid = commonCount > 0
		&& informationMatrix(rawCommonCovariance,
			rawInformation, rawInformationRank)
		&& informationMatrix(incrementalCommonCovariance,
			incrementalInformation, incrementalInformationRank);
	const MatrixXd covarianceDifference = comparisonValid
		? incrementalCommonCovariance - rawCommonCovariance : MatrixXd();
	const MatrixXd informationDifference = comparisonValid
		? incrementalInformation - rawInformation : MatrixXd();
	const VectorXd meanDifference = comparisonValid
		? incrementalCommonMean - rawCommonMean : VectorXd();
	const double covarianceRelativeDifference = comparisonValid
		? covarianceDifference.norm()
			/ std::max(1e-30, rawCommonCovariance.norm())
		: std::numeric_limits<double>::quiet_NaN();
	const double informationRelativeDifference = comparisonValid
		? informationDifference.norm()
			/ std::max(1e-30, rawInformation.norm())
		: std::numeric_limits<double>::quiet_NaN();
	trace << "\nZHANG_E19_TARGET_INFORMATION_COMPARISON time="
		<< time.to_string(0)
		<< " valid=" << comparisonValid
		<< " common_target_count=" << commonCount
		<< " raw_information_rank=" << rawInformationRank
		<< " incremental_information_rank=" << incrementalInformationRank
		<< " covariance_relative_difference="
		<< covarianceRelativeDifference
		<< " information_relative_difference="
		<< informationRelativeDifference
		<< " covariance_trace_ratio="
		<< (comparisonValid && rawCommonCovariance.trace() > 0
			? incrementalCommonCovariance.trace()
				/ rawCommonCovariance.trace()
			: std::numeric_limits<double>::quiet_NaN())
		<< " information_trace_ratio="
		<< (comparisonValid && rawInformation.trace() > 0
			? incrementalInformation.trace() / rawInformation.trace()
			: std::numeric_limits<double>::quiet_NaN())
		<< " common_target_identities=";
	for (int index = 0; index < static_cast<int>(commonIdentities.size()); index++)
	{
		if (index) trace << ";";
		trace << commonIdentities[index];
	}
	if (commonIdentities.empty()) trace << "NONE";
	auto traceVector = [&trace](const VectorXd& vector)
	{
		for (int index = 0; index < vector.size(); index++)
		{
			if (index) trace << ";";
			trace << vector(index);
		}
		if (vector.size() == 0) trace << "NONE";
	};
	auto traceMatrix = [&trace](const MatrixXd& matrix)
	{
		for (int row = 0; row < matrix.rows(); row++)
		for (int column = 0; column < matrix.cols(); column++)
		{
			if (row || column) trace << ";";
			trace << matrix(row, column);
		}
		if (matrix.size() == 0) trace << "NONE";
	};
	trace << " raw_mean=";
	traceVector(rawCommonMean);
	trace << " incremental_mean=";
	traceVector(incrementalCommonMean);
	trace << " mean_difference=";
	traceVector(meanDifference);
	trace << " raw_covariance=";
	traceMatrix(rawCommonCovariance);
	trace << " incremental_covariance=";
	traceMatrix(incrementalCommonCovariance);
	trace << " covariance_difference=";
	traceMatrix(covarianceDifference);
	trace << " raw_information=";
	traceMatrix(rawInformation);
	trace << " incremental_information=";
	traceMatrix(incrementalInformation);
	trace << " information_difference=";
	traceMatrix(informationDifference);
	trace << " status=" << (comparisonValid ? "EVALUATED" : "REJECTED")
		<< " reason=" << (comparisonValid ? "NONE" : "NO_COMMON_COORDINATE")
		<< " incremental_role=DIAGNOSTIC_ONLY feedback=0";
	traceZhangE18IntegerDiagnostics(
		trace, time, marginal, "TARGET_INCREMENT_");
}

bool zhangPppArUsesObservable(E_Sys sys, E_ObsCode code)
{
    auto it = acsConfig.zhangPppAr.baseline_observables.find(sys);
    if (it == acsConfig.zhangPppAr.baseline_observables.end())
    {
        return false;
    }
    return std::find(it->second.begin(), it->second.end(), code) != it->second.end();
}

void recordZhangExactPhaseTransform(
    GTime         time,
    E_Sys         sys,
    E_ObsCode     code,
    const SatSys& satellite,
    double        correctionChangeMetres
)
{
    recordZhangExactPhaseTransforms(
        time, sys, code, {{satellite, correctionChangeMetres}}
    );
}

void recordZhangExactPhaseTransforms(
    GTime                          time,
    E_Sys                          sys,
    E_ObsCode                      code,
    const map<SatSys, double>&     correctionChangesMetres
)
{
    if (!acsConfig.zhangPppAr.output_products ||
        correctionChangesMetres.empty())
    {
        return;
    }
    const double lambda = wavelength(sys, code);
    if (lambda <= 0)
    {
        return;
    }

    map<SatSys, double> cycleChanges;
    for (const auto& [satellite, metres] : correctionChangesMetres)
    {
        cycleChanges[satellite] = metres / lambda;
    }
    const bool houOsbLike =
        acsConfig.zhangPppAr.product_mode == "HOU_OSB_LIKE";
    const bool persistentHybridFrontend = houOsbLike &&
        (boost::iequals(
            acsConfig.zhangPppAr.hou_product_coordinate,
            "PERSISTENT_DYNAMIC") ||
         boost::iequals(
            acsConfig.zhangPppAr.hou_product_coordinate,
            "HYBRID_STABLE"));
    auto& datumManager = satelliteDatumManager(sys, code);
    map<SatSys, ZhangSatelliteDatumStatus> statusBefore;
    if (persistentHybridFrontend)
    {
        for (const auto& [satellite, ignored] : cycleChanges)
        {
            statusBefore[satellite] = datumManager.status(satellite, true);
        }
    }
    auto preserved = datumManager.applyDynamicTreeTransform(cycleChanges);
    if (persistentHybridFrontend)
    {
        vector<ZhangHybridTreeTransformSample> invarianceSamples;
        invarianceSamples.reserve(cycleChanges.size());
        for (const auto& [satellite, cycleChange] : cycleChanges)
        {
            const auto after = datumManager.status(satellite, true);
            const auto& before = statusBefore.at(satellite);
            invarianceSamples.push_back({
                satellite,
                before.componentId,
                after.componentId,
                -cycleChange * lambda,
                before.alignmentCycles,
                after.alignmentCycles,
                before.phaseSegment,
                after.phaseSegment,
                before.datumVersion,
                after.datumVersion,
                before.componentVersion,
                after.componentVersion,
                before.alignmentGeneration,
                after.alignmentGeneration,
                preserved[satellite]
            });
        }
        const auto invariance = zhangHybridTreeTransformInvariance(
            invarianceSamples, lambda);
        for (const auto& diagnostic : invariance)
        {
            BOOST_LOG_TRIVIAL(info)
                << "ZHANG_HYBRID_TREE_INVARIANCE time=" << time.to_string(0)
                << " system=" << enum_to_string(sys)
                << " observable=" << enum_to_string(code)
                << " satellite=" << diagnostic.satellite.id()
                << " component_id=" << diagnostic.componentId
                << " component_support=" << diagnostic.componentSupportCount
                << " backend_integer_gauge_change_cycles="
                << diagnostic.backendIntegerGaugeChangeCycles
                << " backend_common_real_gauge_shift_m="
                << diagnostic.backendCommonRealGaugeShiftMetres
                << " frontend_delta_m=" << diagnostic.frontendDeltaMetres
                << " component_common_delta_m="
                << diagnostic.componentCommonDeltaMetres
                << " relative_frontend_delta_m="
                << diagnostic.relativeFrontendDeltaMetres
                << " expected_real_gauge_shift_m="
                << diagnostic.expectedRealGaugeShiftMetres
                << " hybrid_closure_residual_m="
                << diagnostic.hybridClosureResidualMetres
                << " hybrid_closure_machine_zero="
                << diagnostic.hybridClosureMachineZero
                << " invariant=" << diagnostic.invariant
                << " status=" << diagnostic.reason;
        }
    }
    for (const auto& [satellite, cycleChange] : cycleChanges)
    {
        ProductKey key{satellite, code};
        auto& state = continuityMap[key];
        initialiseContinuityState(key, state);
        if (houOsbLike)
        {
            // The complete affine offset defines the fixed Hou product
            // coordinate.  A pure internal-tree exchange is never a product
            // discontinuity, even when its current float value is fractional.
            state.applyHouProductTransform(cycleChange);
        }
        else if (preserved[satellite])
        {
            state.resetReason = "component_gauge_s_transform";
            state.fractionalShiftCycles +=
                cycleChange - std::llround(cycleChange);
        }
        else
        {
            state.applyExactTransform(
                time,
                cycleChange,
                acsConfig.zhangPppAr.stabilization_epochs
            );
        }
    }
}

void recordZhangPhaseReinitialisation(
    GTime                         time,
    E_Sys                         sys,
    const vector<E_ObsCode>&      observables,
    const string&                 reason,
    const set<SatSys>&            affectedSatellites
)
{
    if (!acsConfig.zhangPppAr.output_products)
    {
        return;
    }

    for (E_ObsCode code : observables)
    {
        satelliteDatumManager(sys, code).markDynamicAlignmentUnknown(
            affectedSatellites
        );
        for (auto& [key, state] : continuityMap)
        {
            if (key.satellite.sys != sys || key.observable != code)
            {
                continue;
            }
            if (affectedSatellites.find(key.satellite) == affectedSatellites.end())
            {
                continue;
            }
            if (acsConfig.zhangPppAr.product_mode == "HOU_OSB_LIKE" &&
                (acsConfig.zhangPppAr.hou_product_coordinate ==
                    "PERSISTENT_DYNAMIC" ||
                 acsConfig.zhangPppAr.hou_product_coordinate ==
                    "HYBRID_STABLE"))
            {
                // The persistent coordinate is transported only through an
                // exact invertible S-transform.  A local projection removes
                // that proof for the affected satellite, so advance only its
                // product datum instead of resetting the whole constellation.
                state.reinitialise(
                    time,
                    "persistent_dynamic_coordinate_reset:" + reason,
                    acsConfig.zhangPppAr.stabilization_epochs);
            }
            else
            {
                // PRODUCT_TREE can reconstruct the current coordinate from
                // the retained graph.  Keep its product metadata unchanged
                // while withdrawing integer precision.
                state.resetReason = "dynamic_alignment_unknown:" + reason;
                state.hasFixedDatum = false;
                state.stabilizationRemaining =
                    acsConfig.zhangPppAr.stabilization_epochs;
            }
        }
    }
}

bool promoteZhangSatelliteProductRelation(
    GTime              time,
    E_Sys              sys,
    E_ObsCode          code,
    const SatSys&      a,
    const SatSys&      b,
    long long          integerDifferenceCycles,
    const string&      provenance
)
{
    return promoteZhangSatelliteProductRelationDetailed(
        time, sys, code, a, b, integerDifferenceCycles, provenance
    ).accepted;
}

ZhangProductRelationEvent promoteZhangSatelliteProductRelationDetailed(
    GTime              time,
    E_Sys              sys,
    E_ObsCode          code,
    const SatSys&      a,
    const SatSys&      b,
    long long          integerDifferenceCycles,
    const string&      provenance
)
{
    auto& manager = satelliteDatumManager(sys, code);
    long long existingDifference = 0;
    bool relationKnown = manager.relation(a, b, existingDifference);
    ZhangProductRelationEvent event;

    if (relationKnown && existingDifference != integerDifferenceCycles &&
        acsConfig.zhangPppAr.conflict_quarantine)
    {
        SatSys trustedAnchor;
        if (provenance.rfind("G_sat_", 0) == 0)
        {
            trustedAnchor = a;
        }
        event = manager.quarantineCurrentAlignment(a, b, trustedAnchor);
    }
    else if (!relationKnown &&
             acsConfig.zhangPppAr.promotion_confirmation_epochs > 1)
    {
        SatSys canonicalA = a;
        SatSys canonicalB = b;
        int segmentA = manager.status(a, false).phaseSegment;
        int segmentB = manager.status(b, false).phaseSegment;
        long long canonicalDifference = integerDifferenceCycles;
        if (canonicalB < canonicalA)
        {
            std::swap(canonicalA, canonicalB);
            std::swap(segmentA, segmentB);
            canonicalDifference = -canonicalDifference;
        }
        PromotionEvidenceKey key{
            sys, code, canonicalA, segmentA, canonicalB, segmentB
        };
        auto& evidence = promotionEvidence[key];
        long int epoch = static_cast<long int>(
            std::llround(time.bigTime)
        );
        double maxGap =
            acsConfig.zhangPppAr.promotion_confirmation_max_gap_seconds;
        bool sameSequence =
            evidence.confirmations > 0 &&
            evidence.difference == canonicalDifference &&
            epoch != evidence.lastEpoch &&
            (maxGap <= 0 || epoch - evidence.lastEpoch <= maxGap);
        if (!sameSequence && epoch != evidence.lastEpoch)
        {
            evidence.confirmations = 0;
        }
        if (epoch != evidence.lastEpoch)
        {
            evidence.difference = canonicalDifference;
            evidence.lastEpoch = epoch;
            evidence.confirmations++;
        }
        event.type = ZhangProductRelationEventType::PENDING_CONFIRMATION;
        event.confirmationCount = evidence.confirmations;
        event.confirmationRequired =
            acsConfig.zhangPppAr.promotion_confirmation_epochs;
        if (evidence.confirmations >= event.confirmationRequired)
        {
            promotionEvidence.erase(key);
            event = manager.promoteRelationDetailed(
                a, b, integerDifferenceCycles, provenance, true
            );
        }
    }
    else
    {
        event = manager.promoteRelationDetailed(
            a, b, integerDifferenceCycles, provenance, true
        );
    }

    const char* status = "REJECTED_INCONSISTENT";
    if (event.accepted)
    {
        status = "ACCEPTED";
    }
    else if (event.type ==
             ZhangProductRelationEventType::PENDING_CONFIRMATION)
    {
        status = "PENDING_CONFIRMATION";
    }
    else if (event.type ==
             ZhangProductRelationEventType::CURRENT_ALIGNMENT_QUARANTINED)
    {
        status = "QUARANTINED_CURRENT_ALIGNMENT";
    }
    std::ostringstream message;
    message << "ZHANG_PRODUCT_RELATION_PROMOTION time=" << time.to_string(0)
            << " system=" << enum_to_string(sys)
            << " observable=" << enum_to_string(code)
            << " satellite_a=" << a.id()
            << " satellite_b=" << b.id()
            << " integer_difference=" << integerDifferenceCycles
            << " status=" << status
            << " event_type=" << zhangProductRelationEventName(event.type)
            << " old_component_size_a=" << event.oldComponentSizeA
            << " old_component_size_b=" << event.oldComponentSizeB
            << " new_component_size=" << event.newComponentSize
            << " confirmation_count=" << event.confirmationCount
            << " confirmation_required=" << event.confirmationRequired
            << " quarantined_satellite="
            << (event.quarantinedSatellite.sys == E_Sys::NONE
                    ? "NONE" : event.quarantinedSatellite.id())
            << " provenance=" << provenance;
    if (event.accepted)
    {
        BOOST_LOG_TRIVIAL(info) << message.str();
    }
    else if (event.type == ZhangProductRelationEventType::CONFLICT_REJECTED)
    {
        BOOST_LOG_TRIVIAL(error) << message.str();
    }
    else if (event.type ==
             ZhangProductRelationEventType::CURRENT_ALIGNMENT_QUARANTINED)
    {
        BOOST_LOG_TRIVIAL(warning) << message.str();
    }
    else
    {
        BOOST_LOG_TRIVIAL(info) << message.str();
    }
    return event;
}

ZhangProductRelationEvent relinkZhangSatelliteProductRelation(
    GTime              time,
    E_Sys              sys,
    E_ObsCode          code,
    const SatSys&      anchor,
    const SatSys&      satellite,
    long long          currentDifferenceCycles,
    const string&      provenance
)
{
    auto& manager = satelliteDatumManager(sys, code);
    int segmentA = manager.status(anchor, false).phaseSegment;
    int segmentB = manager.status(satellite, false).phaseSegment;
    PromotionEvidenceKey key{
        sys, code, anchor, segmentA, satellite, segmentB
    };
    auto& evidence = relinkEvidence[key];
    long int epoch = static_cast<long int>(std::llround(time.bigTime));
    double maxGap =
        acsConfig.zhangPppAr.promotion_confirmation_max_gap_seconds;
    bool sameSequence =
        evidence.confirmations > 0 &&
        evidence.difference == currentDifferenceCycles &&
        epoch != evidence.lastEpoch &&
        (maxGap <= 0 || epoch - evidence.lastEpoch <= maxGap);
    if (!sameSequence && epoch != evidence.lastEpoch)
    {
        evidence.confirmations = 0;
    }
    if (epoch != evidence.lastEpoch)
    {
        evidence.difference = currentDifferenceCycles;
        evidence.lastEpoch = epoch;
        evidence.confirmations++;
    }

    ZhangProductRelationEvent event;
    event.type = ZhangProductRelationEventType::PENDING_CONFIRMATION;
    event.confirmationCount = evidence.confirmations;
    event.confirmationRequired = std::max(
        1, acsConfig.zhangPppAr.promotion_confirmation_epochs
    );
    if (evidence.confirmations >= event.confirmationRequired)
    {
        relinkEvidence.erase(key);
        event = manager.realignRelation(
            anchor, satellite, currentDifferenceCycles, provenance
        );
    }
    BOOST_LOG_TRIVIAL(info)
        << "ZHANG_PRODUCT_RELATION_PROMOTION time=" << time.to_string(0)
        << " system=" << enum_to_string(sys)
        << " observable=" << enum_to_string(code)
        << " satellite_a=" << anchor.id()
        << " satellite_b=" << satellite.id()
        << " integer_difference=" << currentDifferenceCycles
        << " status="
        << (event.accepted
                ? "ACCEPTED"
                : event.type == ZhangProductRelationEventType::PENDING_CONFIRMATION
                    ? "PENDING_CONFIRMATION"
                    : "REJECTED_INCONSISTENT")
        << " event_type=" << zhangProductRelationEventName(event.type)
        << " old_component_size_a=" << event.oldComponentSizeA
        << " old_component_size_b=" << event.oldComponentSizeB
        << " new_component_size=" << event.newComponentSize
        << " confirmation_count=" << event.confirmationCount
        << " confirmation_required=" << event.confirmationRequired
        << " provenance=" << provenance;
    return event;
}

ZhangCertifiedTemporalAlignmentResult
applyZhangCertifiedTemporalProductShifts(
	GTime                              time,
	E_Sys                              sys,
	E_ObsCode                          code,
	const map<SatSys, long long>&      rawProductChanges,
	const string&                      provenance)
{
	auto result = satelliteDatumManager(sys, code)
		.applyCertifiedTemporalTransform(rawProductChanges);
	BOOST_LOG_TRIVIAL(info)
		<< "ZHANG_CERTIFIED_TEMPORAL_FRONTEND time=" << time.to_string(0)
		<< " system=" << enum_to_string(sys)
		<< " observable=" << enum_to_string(code)
		<< " requested_satellites=" << result.requestedSatellites
		<< " restored_satellites=" << result.restoredSatellites
		<< " affected_components=" << result.affectedComponents
		<< " status=" << (result.accepted ? "COMMITTED" : "REJECTED")
		<< " reason=" << result.reason
		<< " provenance=" << provenance
		<< " estimator_feedback=0";
	return result;
}

ZhangCertifiedTemporalFrontendBatchResult
applyZhangCertifiedTemporalProductShiftBatch(
	GTime time,
	E_Sys sys,
	const map<E_ObsCode, map<SatSys, long long>>& rawProductChanges,
	const string& provenance)
{
	ZhangCertifiedTemporalFrontendBatchResult result;
	result.observableGroups = rawProductChanges.size();
	if (rawProductChanges.empty())
	{
		result.reason = "NO_OBSERVABLE_SHIFT_GROUPS";
		return result;
	}
	map<E_ObsCode, ZhangSatelliteDatumManagerCheckpoint> before;
	for (const auto& [observable, ignored] : rawProductChanges)
	{
		before[observable] = satelliteDatumManager(sys, observable)
			.checkpointState();
	}
	for (const auto& [observable, shifts] : rawProductChanges)
	{
		auto applied = satelliteDatumManager(sys, observable)
			.applyCertifiedTemporalTransform(shifts);
		if (!applied.accepted)
		{
			for (const auto& [restoreObservable, snapshot] : before)
			{
				std::string ignoredReason;
				satelliteDatumManager(sys, restoreObservable)
					.restoreCheckpointState(snapshot, &ignoredReason);
			}
			result.reason = "OBSERVABLE_FRONTEND_REJECTED_" +
				enum_to_string(observable) + "_" + applied.reason;
			BOOST_LOG_TRIVIAL(warning)
				<< "ZHANG_CERTIFIED_TEMPORAL_FRONTEND_BATCH time="
				<< time.to_string(0)
				<< " system=" << enum_to_string(sys)
				<< " observable_groups=" << result.observableGroups
				<< " status=ABORTED reason=" << result.reason
				<< " provenance=" << provenance
				<< " estimator_feedback=0";
			return result;
		}
		result.restoredSatellites += applied.restoredSatellites;
	}
	result.accepted = true;
	result.reason = "ALL_OBSERVABLE_FRONTENDS_COMMITTED";
	BOOST_LOG_TRIVIAL(info)
		<< "ZHANG_CERTIFIED_TEMPORAL_FRONTEND_BATCH time=" << time.to_string(0)
		<< " system=" << enum_to_string(sys)
		<< " observable_groups=" << result.observableGroups
		<< " restored_satellites=" << result.restoredSatellites
		<< " status=COMMITTED reason=" << result.reason
		<< " provenance=" << provenance
		<< " estimator_feedback=0";
	return result;
}

std::size_t quarantineZhangSatelliteProductAlignments(
    GTime                   time,
    E_Sys                   sys,
    E_ObsCode               code,
    const std::set<SatSys>& satellites,
    const SatSys&           trustedAnchor,
    const std::string&      reason
)
{
    auto quarantined = satelliteDatumManager(sys, code)
        .quarantineCurrentAlignments(satellites, trustedAnchor);
    BOOST_LOG_TRIVIAL(info)
        << "ZHANG_HELD_PRODUCT_QUARANTINE time=" << time.to_string(0)
        << " system=" << enum_to_string(sys)
        << " observable=" << enum_to_string(code)
        << " support_satellites=" << satellites.size()
        << " quarantined_satellites=" << quarantined
        << " trusted_anchor="
        << (trustedAnchor.sys == E_Sys::NONE ? "NONE" : trustedAnchor.id())
        << " reason=" << reason;
    return quarantined;
}

vector<ZhangSatelliteDatumComponent> zhangSatelliteDatumComponents(
    E_Sys sys,
    E_ObsCode code
)
{
    return satelliteDatumManager(sys, code).components();
}

ZhangCurrentAlignmentState zhangSatelliteAlignmentState(
    E_Sys sys,
    E_ObsCode code,
    const SatSys& satellite
)
{
    return satelliteDatumManager(sys, code).alignmentState(satellite);
}

ZhangSatelliteDatumStatus zhangSatelliteDatumStatus(
    E_Sys sys,
    E_ObsCode code,
    const SatSys& satellite
)
{
    return satelliteDatumManager(sys, code).status(satellite, false);
}

bool queryZhangSatelliteProductRelation(
    E_Sys sys,
    E_ObsCode code,
    const SatSys& a,
    const SatSys& b,
    long long& differenceCycles
)
{
    return satelliteDatumManager(sys, code).relation(
        a, b, differenceCycles
    );
}

void recordZhangSatellitePhaseDiscontinuity(
    GTime                         time,
    E_Sys                         sys,
    const vector<E_ObsCode>&      observables,
    const SatSys&                 satellite,
    const string&                 reason
)
{
    for (E_ObsCode code : observables)
    {
        satelliteDatumManager(sys, code).recordSatelliteDiscontinuity(satellite);
        ProductKey key{satellite, code};
        auto& state = continuityMap[key];
        initialiseContinuityState(key, state);
        state.reinitialise(
            time,
            "satellite_phase_discontinuity:" + reason,
            acsConfig.zhangPppAr.stabilization_epochs
        );
    }
}

vector<ZhangPendingProductTransition> takeZhangPendingProductTransitions(
	const KFState& integerLedgerState)
{
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(integerLedgerState, runtimeId))
	{
		return {};
	}
	auto found = pendingProductTransitions.find(runtimeId);
	if (found == pendingProductTransitions.end())
	{
		return {};
	}
	auto result = std::move(found->second);
	pendingProductTransitions.erase(found);
	auto pins = pendingSnapshotPins.find(runtimeId);
	if (pins != pendingSnapshotPins.end())
	{
		const bool transactionRegistered = std::any_of(
			result.begin(), result.end(), [&](const auto& transition)
			{
				return std::abs(
					(transition.eventTime - pins->second.eventTime).to_double())
					<= 1e-3;
			});
		if (transactionRegistered)
		{
			pendingSnapshotPins.erase(pins);
		}
	}
	return result;
}

bool registerZhangTemporalProductSnapshot(
	Trace& trace,
	const KFState& captureOwner,
	E_Sys system,
	const SatSys& satellite,
	E_ObsCode observable,
	const string& snapshotIdentity,
	const VectorXd& currentStateRow,
	double affineOffsetCycles,
	GTime time)
{
	return registerZhangTemporalProductSnapshots(trace, captureOwner, {{
		system, satellite, observable, snapshotIdentity, currentStateRow,
		affineOffsetCycles, time}});
}

bool registerZhangTemporalProductSnapshots(
	Trace& trace,
	const KFState& captureOwner,
	const vector<ZhangTemporalProductSnapshotRequest>& requests)
{
	if (!acsConfig.zhangPppAr.temporal_product_transition_shadow)
	{
		return false;
	}
	if (requests.empty())
	{
		return true;
	}
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(captureOwner, runtimeId))
	{
		for (const auto& request : requests)
		{
			trace << "\nZHANG_TEMPORAL_PRODUCT_SNAPSHOT time="
				  << request.time.to_string(0)
				  << " status=REJECTED"
				  << " reason=CHECKPOINT_RUNTIME_ID_UNBOUND"
				  << " feedback=0";
		}
		return false;
	}
	auto capture = e18FactorCaptureBuffers.find(runtimeId);
	if (capture == e18FactorCaptureBuffers.end())
	{
		for (const auto& request : requests)
		{
			trace << "\nZHANG_TEMPORAL_PRODUCT_SNAPSHOT time="
				  << request.time.to_string(0)
				  << " system=" << enum_to_string(request.system)
				  << " satellite=" << request.satellite.id()
				  << " observable=" << enum_to_string(request.observable)
				  << " status=REJECTED reason=RAW_FACTOR_CAPTURE_DISABLED"
				  << " feedback=0";
		}
		return false;
	}
	vector<ZhangPersistentSnapshotBinding> bindings;
	vector<const ZhangTemporalProductSnapshotRequest*> uniqueRequests;
	set<string> identities;
	for (const auto& request : requests)
	{
		if (!identities.insert(request.snapshotIdentity).second)
		{
			continue;
		}
		bindings.push_back({
			request.snapshotIdentity, request.snapshotIdentity,
			request.currentStateRow, request.affineOffsetCycles});
		uniqueRequests.push_back(&request);
	}
	const std::size_t before = capture->second.persistentSnapshotCount();
	const bool accepted = capture->second.bindPersistentSnapshots(bindings);
	const std::size_t after = capture->second.persistentSnapshotCount();
	const string reason = accepted
		? "EXPLICIT_PERSISTENT_TARGET_BATCH"
		: capture->second.summary().failureReason;
	for (const auto* request : uniqueRequests)
	{
		trace << "\nZHANG_TEMPORAL_PRODUCT_SNAPSHOT time="
			  << request->time.to_string(0)
			  << " system=" << enum_to_string(request->system)
			  << " satellite=" << request->satellite.id()
			  << " observable=" << enum_to_string(request->observable)
			  << " status=" << (accepted ? "AVAILABLE" : "REJECTED")
			  << " reason=" << reason
			  << " feedback=0";
	}
	trace << "\nZHANG_TEMPORAL_PRODUCT_SNAPSHOT_BATCH time="
		  << uniqueRequests.front()->time.to_string(0)
		  << " requested=" << requests.size()
		  << " unique=" << uniqueRequests.size()
		  << " newly_bound=" << (after - before)
		  << " status=" << (accepted ? "AVAILABLE" : "REJECTED")
		  << " reason=" << reason
		  << " feedback=0";
	return accepted;
}

bool registerZhangCandidateProductSnapshotsBeforeCoordinateReset(
	Trace& trace,
	const KFState& captureOwner,
	const KFState& state,
	E_Sys system,
	const vector<E_ObsCode>& observables,
	const ZhangGraphBasis& currentBasis,
	const ZhangGraphBasis& previousProductBasis,
	const map<ZhangGraphEdge, int>& previousProductArcVersions,
	const ZhangGraphBasis& proposedProductBasis,
	const map<ZhangGraphEdge, int>& proposedArcVersions,
	GTime time)
{
	if (!acsConfig.zhangPppAr.temporal_product_transition_shadow)
	{
		return true;
	}
	if (acsConfig.zhangPppAr.targeted_besd_capture_shadow)
	{
		string runtimeId;
		if (!resolveZhangPppArRuntimeOwner(captureOwner, runtimeId))
		{
			trace << "\nZHANG_TARGETED_BESD_CAPTURE time="
				  << time.to_string(0)
				  << " system=" << enum_to_string(system)
				  << " status=REJECTED reason=CHECKPOINT_RUNTIME_ID_UNBOUND"
				  << " estimator_feedback=0";
			return false;
		}
		const auto previousTarget = zhangBuildSatelliteProductTarget(
			currentBasis, previousProductBasis);
		const auto proposedTarget = zhangBuildSatelliteProductTarget(
			currentBasis, proposedProductBasis);
		if (!previousTarget.valid || !proposedTarget.valid)
		{
			trace << "\nZHANG_TARGETED_BESD_CAPTURE time="
				  << time.to_string(0)
				  << " system=" << enum_to_string(system)
				  << " status=REJECTED reason=COORDINATE_TARGET_UNAVAILABLE"
				  << " previous_reason=" << previousTarget.failureReason
				  << " proposed_reason=" << proposedTarget.failureReason
				  << " estimator_feedback=0";
			return false;
		}
		const auto previousFunctionals = zhangBuildProductIntegerFunctionals(
			previousProductBasis, previousProductArcVersions,
			previousTarget.referenceSatellite);
		const auto proposedFunctionals = zhangBuildProductIntegerFunctionals(
			proposedProductBasis, proposedArcVersions,
			proposedTarget.referenceSatellite);
		if (previousFunctionals.empty() || proposedFunctionals.empty())
		{
			trace << "\nZHANG_TARGETED_BESD_CAPTURE time="
				  << time.to_string(0)
				  << " system=" << enum_to_string(system)
				  << " status=REJECTED reason=PRODUCT_FUNCTIONALS_EMPTY"
				  << " estimator_feedback=0";
			return false;
		}

		auto stateRow = [&](const auto& coordinateTarget,
			const SatSys& satellite, E_ObsCode observable,
			VectorXd& row)
		{
			ZhangExactVector coordinateRow(
				coordinateTarget.currentChords.size());
			bool found = satellite == coordinateTarget.referenceSatellite;
			if (!found)
			{
				for (std::size_t index = 0;
					 index < coordinateTarget.targetSatellites.size(); index++)
				{
					if (coordinateTarget.targetSatellites[index] == satellite)
					{
						coordinateRow = coordinateTarget.matrix[index];
						found = true;
						break;
					}
				}
			}
			row = VectorXd::Zero(state.x.size());
			if (!found || coordinateRow.size() !=
					coordinateTarget.currentChords.size())
			{
				return false;
			}
			for (std::size_t chord = 0; chord < coordinateRow.size(); chord++)
			{
				if (coordinateRow[chord] == 0)
				{
					continue;
				}
				KFKey key;
				key.type = KF::AMBIGUITY;
				key.str = coordinateTarget.currentChords[chord].receiver;
				key.Sat = coordinateTarget.currentChords[chord].satellite;
				key.num = static_cast<int>(observable);
				auto ambiguity = state.kfIndexMap.find(key);
				if (ambiguity == state.kfIndexMap.end())
				{
					return false;
				}
				row(ambiguity->second) +=
					coordinateRow[chord].convert_to<double>();
			}
			return row.allFinite();
		};

		vector<string> identities;
		vector<VectorXd> rows;
		vector<double> offsets;
		map<string, int> identityIndex;
		vector<ZhangTargetedBesdPair> pairs;
		auto addTarget = [&](const string& identity, const VectorXd& row,
			double offset)
		{
			auto existing = identityIndex.find(identity);
			if (existing != identityIndex.end())
			{
				return existing->second;
			}
			const int index = identities.size();
			identityIndex[identity] = index;
			identities.push_back(identity);
			rows.push_back(row);
			offsets.push_back(offset);
			return index;
		};

		for (E_ObsCode observable : observables)
		for (const auto& [satellite, previous] : previousFunctionals)
		{
			auto proposed = proposedFunctionals.find(satellite);
			if (proposed == proposedFunctionals.end())
			{
				continue;
			}
			const auto difference = zhangProductIntegerFunctionalDifference(
				previous, proposed->second);
			if (!difference.valid || difference.coefficients.empty())
			{
				continue;
			}
			const auto held = zhangNamedProductIntegerSupport(
				state, system, observable,
				difference.physicalEdges,
				difference.physicalArcVersions,
				difference.coefficients);
			const auto selection = zhangSelectTargetedBesdTransition(
				difference, proposedProductBasis.edges, proposedArcVersions,
				held.contained, false);
			if (!selection.selected)
			{
				continue;
			}
			VectorXd previousRow;
			VectorXd proposedRow;
			if (!stateRow(previousTarget, satellite, observable, previousRow)
			 || !stateRow(proposedTarget, satellite, observable, proposedRow))
			{
				trace << "\nZHANG_TARGETED_BESD_TARGET time="
					  << time.to_string(0)
					  << " system=" << enum_to_string(system)
					  << " satellite=" << satellite.id()
					  << " observable=" << enum_to_string(observable)
					  << " status=REJECTED reason=EVENT_TIME_ROW_UNAVAILABLE"
					  << " estimator_feedback=0";
				continue;
			}
			const auto status = satelliteDatumManager(
				system, observable).status(satellite, false);
			const string phaseIdentity = zhangHybridPhaseProductSegmentId(
				satellite, observable, status.phaseSegment);
			const string oldIdentity = temporalProductSnapshotIdentity(
				system, satellite, observable,
				zhangProductPhysicalFunctionalFingerprint(previous),
				phaseIdentity);
			const string newIdentity = temporalProductSnapshotIdentity(
				system, satellite, observable,
				zhangProductPhysicalFunctionalFingerprint(proposed->second),
				phaseIdentity);
			const int oldIndex = addTarget(oldIdentity, previousRow,
				previous.affineOffsetCycles.convert_to<double>());
			const int newIndex = addTarget(newIdentity, proposedRow,
				proposed->second.affineOffsetCycles.convert_to<double>());
			pairs.push_back({
				satellite, observable, oldIndex, newIndex, difference,
				oldIdentity, newIdentity});
			trace << "\nZHANG_TARGETED_BESD_TARGET time="
				  << time.to_string(0)
				  << " system=" << enum_to_string(system)
				  << " satellite=" << satellite.id()
				  << " observable=" << enum_to_string(observable)
				  << " physical_terms=" << selection.physicalTerms
				  << " held_reason=" << held.reason
				  << " status=ARMED reason=" << selection.reason
				  << " estimator_feedback=0";
		}
		if (pairs.empty())
		{
			trace << "\nZHANG_TARGETED_BESD_CAPTURE time="
				  << time.to_string(0)
				  << " system=" << enum_to_string(system)
				  << " pairs=0 targets=0 status=SKIPPED"
				  << " reason=NO_REQUIRES_BESD_RETIRED_ARC"
				  << " estimator_feedback=0";
			return true;
		}
		MatrixXd targetRows(rows.size(), state.x.size());
		VectorXd targetOffsets(offsets.size());
		for (int index = 0; index < static_cast<int>(rows.size()); index++)
		{
			targetRows.row(index) = rows[index].transpose();
			targetOffsets(index) = offsets[index];
		}
		ZhangTargetedBesdRuntime runtime;
		runtime.eventTime = time;
		runtime.system = system;
		runtime.pairs = std::move(pairs);
		const bool accepted = runtime.tracker.initialise(
			identities, targetRows, targetOffsets, state.x, state.P);
		trace << "\nZHANG_TARGETED_BESD_CAPTURE time="
			  << time.to_string(0)
			  << " system=" << enum_to_string(system)
			  << " pairs=" << runtime.pairs.size()
			  << " targets=" << identities.size()
			  << " state_dimension=" << state.x.size()
			  << " stored_cross_covariance_elements="
			  << identities.size() * state.x.size()
			  << " stored_target_covariance_elements="
			  << identities.size() * identities.size()
			  << " status=" << (accepted ? "ARMED" : "REJECTED")
			  << " reason=" << (accepted ? "REQUIRES_BESD_ONLY"
				  : runtime.tracker.failureReason())
			  << " estimator_feedback=0";
		if (accepted)
		{
			targetedBesdRuntimes[runtimeId].push_back(std::move(runtime));
		}
		return accepted;
	}
	if (!acsConfig.zhangPppAr.fixed_lag_factor_capture_shadow)
	{
		return true;
	}
	const auto coordinateTarget = zhangBuildSatelliteProductTarget(
		currentBasis, proposedProductBasis);
	if (!coordinateTarget.valid)
	{
		trace << "\nZHANG_TEMPORAL_PRODUCT_PRECAPTURE time="
			  << time.to_string(0)
			  << " system=" << enum_to_string(system)
			  << " requested=0 rejected=0 status=REJECTED reason="
			  << "COORDINATE_TARGET_" << coordinateTarget.failureReason
			  << " feedback=0";
		return false;
	}
	const auto functionals = zhangBuildProductIntegerFunctionals(
		proposedProductBasis,
		proposedArcVersions,
		coordinateTarget.referenceSatellite);
	if (functionals.empty())
	{
		trace << "\nZHANG_TEMPORAL_PRODUCT_PRECAPTURE time="
			  << time.to_string(0)
			  << " system=" << enum_to_string(system)
			  << " requested=0 rejected=0 status=REJECTED reason="
			  << "PRODUCT_INTEGER_FUNCTIONALS_EMPTY feedback=0";
		return false;
	}

	vector<ZhangTemporalProductSnapshotRequest> requests;
	int rejected = 0;
	for (E_ObsCode observable : observables)
	{
		for (const auto& [satellite, functional] : functionals)
		{
			ZhangExactVector coordinateRow(
				coordinateTarget.currentChords.size());
			bool rowFound = satellite == coordinateTarget.referenceSatellite;
			if (!rowFound)
			{
				for (std::size_t row = 0;
					 row < coordinateTarget.targetSatellites.size(); row++)
				{
					if (coordinateTarget.targetSatellites[row] == satellite)
					{
						coordinateRow = coordinateTarget.matrix[row];
						rowFound = true;
						break;
					}
				}
			}
			VectorXd stateRow = VectorXd::Zero(state.x.size());
			bool evaluable = rowFound && coordinateRow.size() ==
				coordinateTarget.currentChords.size();
			for (std::size_t chord = 0;
				 evaluable && chord < coordinateRow.size(); chord++)
			{
				if (coordinateRow[chord] == 0)
				{
					continue;
				}
				KFKey ambiguityKey;
				ambiguityKey.type = KF::AMBIGUITY;
				ambiguityKey.str =
					coordinateTarget.currentChords[chord].receiver;
				ambiguityKey.Sat =
					coordinateTarget.currentChords[chord].satellite;
				ambiguityKey.num = static_cast<int>(observable);
				auto ambiguity = state.kfIndexMap.find(ambiguityKey);
				if (ambiguity == state.kfIndexMap.end())
				{
					evaluable = false;
					break;
				}
				stateRow(ambiguity->second) +=
					coordinateRow[chord].convert_to<double>();
			}
			const auto satelliteStatus = satelliteDatumManager(
				system, observable).status(satellite, false);
			const string phaseIdentity =
				zhangHybridPhaseProductSegmentId(
					satellite, observable, satelliteStatus.phaseSegment);
			const string physicalIdentity =
				zhangProductPhysicalFunctionalFingerprint(functional);
			const string snapshotIdentity = temporalProductSnapshotIdentity(
				system, satellite, observable,
				physicalIdentity, phaseIdentity);
			if (!evaluable || physicalIdentity == "INVALID")
			{
				rejected++;
				trace << "\nZHANG_TEMPORAL_PRODUCT_PRECAPTURE_TARGET time="
					  << time.to_string(0)
					  << " system=" << enum_to_string(system)
					  << " satellite=" << satellite.id()
					  << " observable=" << enum_to_string(observable)
					  << " status=REJECTED reason="
					  << (!rowFound ? "SATELLITE_TARGET_ROW_MISSING"
						  : "PRE_RESET_COORDINATE_ROW_UNAVAILABLE")
					  << " feedback=0";
				continue;
			}
			requests.push_back({
				system,
				satellite,
				observable,
				snapshotIdentity,
				stateRow,
				functional.affineOffsetCycles.convert_to<double>(),
				time});
		}
	}
	const bool accepted = !requests.empty() &&
		registerZhangTemporalProductSnapshots(
			trace, captureOwner, requests);
	if (accepted && rejected == 0)
	{
		string runtimeId;
		if (!resolveZhangPppArRuntimeOwner(captureOwner, runtimeId))
		{
			return false;
		}
		auto& pins = pendingSnapshotPins[runtimeId];
		if (std::abs((pins.eventTime - time).to_double()) > 1e-3)
		{
			pins.eventTime = time;
			pins.identities.clear();
		}
		for (const auto& request : requests)
		{
			pins.identities.insert(request.snapshotIdentity);
		}
	}
	trace << "\nZHANG_TEMPORAL_PRODUCT_PRECAPTURE time="
		  << time.to_string(0)
		  << " system=" << enum_to_string(system)
		  << " requested=" << requests.size()
		  << " rejected=" << rejected
		  << " status=" << (accepted && rejected == 0
			  ? "AVAILABLE" : "REJECTED")
		  << " reason=" << (accepted
			  ? (rejected == 0 ? "NONE" : "PARTIAL_PRE_RESET_COVERAGE")
			  : "PRE_RESET_SNAPSHOT_BINDING_FAILED")
		  << " feedback=0";
	return accepted && rejected == 0;
}

static bool zhangTemporalProductBesdFromMarginal(
	const ZhangPersistentRawTargetMarginal& marginal,
	const vector<pair<string, string>>& oldNewSnapshots,
	VectorXd& differences,
	MatrixXd& covariance,
	vector<bool>& availableRows,
	string& failureReason)
{
	differences.resize(0);
	covariance.resize(0, 0);
	availableRows.assign(oldNewSnapshots.size(), false);
	if (!marginal.valid)
	{
		failureReason = marginal.failureReason;
		return false;
	}
	map<string, int> indices;
	for (int index = 0; index < static_cast<int>(marginal.identities.size()); index++)
	{
		indices[marginal.identities[index]] = index;
	}
	MatrixXd transform = MatrixXd::Zero(
		oldNewSnapshots.size(), marginal.mean.size());
	int availableCount = 0;
	for (int row = 0; row < static_cast<int>(oldNewSnapshots.size()); row++)
	{
		auto oldTarget = indices.find(oldNewSnapshots[row].first);
		auto newTarget = indices.find(oldNewSnapshots[row].second);
		if (oldTarget == indices.end() || newTarget == indices.end())
		{
			continue;
		}
		transform(row, oldTarget->second) = -1;
		transform(row, newTarget->second) = +1;
		availableRows[row] = true;
		availableCount++;
	}
	if (availableCount == 0)
	{
		failureReason = "BESD_SNAPSHOT_TARGET_MISSING";
		return false;
	}
	differences = transform * marginal.mean;
	covariance = transform * marginal.covariance * transform.transpose();
	covariance = 0.5 * (covariance + covariance.transpose());
	if (!differences.allFinite() || !covariance.allFinite())
	{
		failureReason = "NONFINITE_BESD_MARGINAL";
		return false;
	}
	failureReason = availableCount == static_cast<int>(oldNewSnapshots.size())
		? "RAW_FACTOR_PERSISTENT_SNAPSHOT_MARGINAL"
		: "PARTIAL_RAW_FACTOR_PERSISTENT_SNAPSHOT_MARGINAL";
	return true;
}

bool queryZhangTemporalProductBesdMarginal(
	const KFState& captureOwner,
	const vector<pair<string, string>>& oldNewSnapshots,
	VectorXd& differences,
	MatrixXd& covariance,
	vector<bool>& availableRows,
	string& failureReason)
{
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(captureOwner, runtimeId))
	{
		differences.resize(0);
		covariance.resize(0, 0);
		availableRows.assign(oldNewSnapshots.size(), false);
		failureReason = "CHECKPOINT_RUNTIME_ID_UNBOUND";
		return false;
	}
	auto capture = e18FactorCaptureBuffers.find(runtimeId);
	if (capture == e18FactorCaptureBuffers.end())
	{
		differences.resize(0);
		covariance.resize(0, 0);
		availableRows.assign(oldNewSnapshots.size(), false);
		failureReason = "RAW_FACTOR_CAPTURE_DISABLED";
		return false;
	}
	return zhangTemporalProductBesdFromMarginal(
		capture->second.persistentSnapshotMarginal(), oldNewSnapshots,
		differences, covariance, availableRows, failureReason);
}

bool queryZhangTemporalProductBesdMarginalExcludingFamily(
	const KFState& captureOwner,
	const vector<pair<string, string>>& oldNewSnapshots,
	ZhangCapturedMeasurementFamily excludedFamily,
	VectorXd& differences,
	MatrixXd& covariance,
	vector<bool>& availableRows,
	string& failureReason)
{
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(captureOwner, runtimeId))
	{
		differences.resize(0);
		covariance.resize(0, 0);
		availableRows.assign(oldNewSnapshots.size(), false);
		failureReason = "CHECKPOINT_RUNTIME_ID_UNBOUND";
		return false;
	}
	auto capture = e18FactorCaptureBuffers.find(runtimeId);
	if (capture == e18FactorCaptureBuffers.end())
	{
		differences.resize(0);
		covariance.resize(0, 0);
		availableRows.assign(oldNewSnapshots.size(), false);
		failureReason = "RAW_FACTOR_CAPTURE_DISABLED";
		return false;
	}
	const auto marginal =
		capture->second.replayPersistentSnapshotsKeepingRows(
			[excludedFamily](
				const ZhangCapturedFactorEvent& event, int row)
			{
				return zhangCapturedMeasurementFamily(event, row)
					!= excludedFamily;
			});
	return zhangTemporalProductBesdFromMarginal(
		marginal, oldNewSnapshots,
		differences, covariance, availableRows, failureReason);
}

ZhangTemporalSnapshotLifecycle maintainZhangTemporalProductSnapshots(
	const KFState& captureOwner,
	const vector<ZhangPendingProductTransition>& activeTransitions)
{
	ZhangTemporalSnapshotLifecycle result;
	result.activeTransitions = activeTransitions.size();
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(captureOwner, runtimeId))
	{
		result.failureReason = "CHECKPOINT_RUNTIME_ID_UNBOUND";
		return result;
	}
	auto capture = e18FactorCaptureBuffers.find(runtimeId);
	if (capture == e18FactorCaptureBuffers.end())
	{
		result.failureReason = "RAW_FACTOR_CAPTURE_DISABLED";
		return result;
	}
	map<string, int> referenceCounts;
	for (const auto& [product, identity] : houProductSnapshotIdentities)
	{
		if (!identity.empty())
		{
			referenceCounts[identity]++;
		}
	}
	for (const auto& transition : activeTransitions)
	{
		if (!transition.oldSnapshotIdentity.empty())
		{
			referenceCounts[transition.oldSnapshotIdentity]++;
		}
		if (!transition.newSnapshotIdentity.empty())
		{
			referenceCounts[transition.newSnapshotIdentity]++;
		}
	}
	auto pins = pendingSnapshotPins.find(runtimeId);
	if (pins != pendingSnapshotPins.end())
	{
		if (std::abs((captureOwner.time - pins->second.eventTime).to_double())
			<= 1e-3)
		{
			result.pendingPinnedIdentities = pins->second.identities.size();
			for (const auto& identity : pins->second.identities)
			{
				referenceCounts[identity]++;
			}
		}
		else
		{
			pendingSnapshotPins.erase(pins);
		}
	}
	set<string> retained;
	for (const auto& [identity, count] : referenceCounts)
	{
		if (count > 0)
		{
			retained.insert(identity);
		}
	}
	result.referencedIdentities = retained.size();
	result.retainedBefore = capture->second.persistentSnapshotCount();
	result.valid = capture->second.retainPersistentSnapshots(retained);
	result.retainedAfter = capture->second.persistentSnapshotCount();
	result.failureReason = result.valid
		? "NONE" : capture->second.summary().failureReason;
	return result;
}

void writeZhangInternalProducts(
    Trace&         trace,
    const KFState& integerLedgerState,
    const KFState& floatState,
    const KFState* wideLaneState,
    const KFState& fixedState,
    int            newlyFixed,
    bool           integerDatumComplete,
    bool           wideLaneBranchValid,
    bool           fixedBranchValid,
    bool           networkIntegerReady
)
{
    trace << "\nZHANG_PRODUCT_WRITER_ENTRY time="
          << fixedState.time.to_string(0)
          << " output_products=" << acsConfig.zhangPppAr.output_products
          << " product_mode=" << acsConfig.zhangPppAr.product_mode
          << " newly_fixed=" << newlyFixed
          << " network_integer_ready=" << networkIntegerReady;
    if (!acsConfig.zhangPppAr.output_products)
    {
        return;
    }
	const bool houOsbLike =
		acsConfig.zhangPppAr.product_mode == "HOU_OSB_LIKE";
	const bool persistentHouCoordinate = houOsbLike &&
		(acsConfig.zhangPppAr.hou_product_coordinate == "PERSISTENT_DYNAMIC" ||
		 acsConfig.zhangPppAr.hou_product_coordinate == "HYBRID_STABLE");
	string integerLedgerRuntimeId;
	if (!claimZhangPppArServiceRuntime(
		integerLedgerState, integerLedgerRuntimeId))
	{
		trace << "\nZHANG_RUNTIME_ID status=REJECTED"
			  << " reason=CHECKPOINT_RUNTIME_ID_UNBOUND_OR_CONFLICT"
			  << " operation=WRITE_INTERNAL_PRODUCTS"
			  << " feedback=0";
		return;
	}
	auto functionalDifferenceSupport = [&](E_ObsCode code,
		const ZhangProductIntegerFunctional& previous,
		const ZhangProductIntegerFunctional& current)
	{
		ZhangNamedProductIntegerSupport invalid;
		auto difference = zhangProductIntegerFunctionalDifference(
			previous, current);
		invalid.reason = difference.failureReason;
		if (!difference.valid)
		{
			return invalid;
		}
		if (difference.coefficients.empty())
		{
			ZhangNamedProductIntegerSupport identical;
			const ZhangExactInteger exactValue =
				zhangCompleteProductTransitionInteger(difference, 0);
			try
			{
				identical.value = exactValue.convert_to<long long>();
			}
			catch (...)
			{
				identical.reason = "TRANSITION_INTEGER_OUT_OF_RANGE";
				return identical;
			}
			if (ZhangExactInteger(identical.value) != exactValue)
			{
				identical.reason = "TRANSITION_INTEGER_OUT_OF_RANGE";
				return identical;
			}
			identical.contained = true;
			identical.reason = "IDENTICAL_PHYSICAL_FUNCTIONAL";
			return identical;
		}
		auto support = zhangNamedProductIntegerSupport(
			integerLedgerState, current.satellite.sys, code,
			difference.physicalEdges, difference.physicalArcVersions,
			difference.coefficients);
		if (support.contained)
		{
			// The product functional is affine.  The held-lattice lookup
			// evaluates only the physical ambiguity row; omitting the exact
			// current-minus-previous affine term silently shifts the frontend
			// by an integer number of cycles whenever that term is non-zero.
			const ZhangExactInteger exactValue =
				zhangCompleteProductTransitionInteger(difference, support.value);
			try
			{
				support.value = exactValue.convert_to<long long>();
			}
			catch (...)
			{
				support.contained = false;
				support.reason = "TRANSITION_INTEGER_OUT_OF_RANGE";
				return support;
			}
			if (ZhangExactInteger(support.value) != exactValue)
			{
				support.contained = false;
				support.reason = "TRANSITION_INTEGER_OUT_OF_RANGE";
			}
		}
		return support;
	};
	map<E_Sys, int> backendSBasisGenerations;
	if (houOsbLike)
	{
		for (const auto& [system, observables] :
			 acsConfig.zhangPppAr.baseline_observables)
		{
			ZhangGraphIntegerContext graph;
			if (!zhangGraphIntegerContext(fixedState, system, graph))
			{
				continue;
			}
			backendSBasisGenerations[system] = graph.eventId;
			auto& tracker = houProductDatumVersionTrackers[system];
			const int previousVersion = tracker.version;
			if (!tracker.observe(graph.productDatumVersion))
			{
				continue;
			}
			trace << "\nZHANG_HOU_AUXILIARY_PRODUCT_TREE_EVENT time="
				  << fixedState.time.to_string(0)
				  << " system=" << enum_to_string(system)
				  << " old_version=" << previousVersion
				  << " new_version=" << graph.productDatumVersion
				  << " product_coordinate="
				  << acsConfig.zhangPppAr.hou_product_coordinate
				  << " constellation_reset=0"
				  << " reason=PHYSICAL_FUNCTIONALS_CLASSIFIED_PER_SATELLITE";
		}
	}
	if (houOsbLike && acsConfig.zhangPppAr.output_diagnostics)
	{
		trace << "\nZHANG_HOU_OSB_LIKE_PRODUCT_MODEL time="
			<< fixedState.time.to_string(0)
			<< " correction_definition=CLOCK_MINUS_PHASE"
			<< " user_equation_definition=CLOCK_PLUS_DELTA"
			<< " delta_definition=MINUS_INTERNAL_PHASE_BIAS_AFTER_INTEGER_ALIGNMENT"
			<< " code_correction_definition="
			   "ZHANG_DUAL_FREQUENCY_CLOCK_IF_GF_ABSORBED"
			<< " integer_source=NETWORK_CYCLE_LATTICE"
			<< " product_datum=RELATIVE_PER_SYSTEM_SIGNAL"
			<< " product_coordinate="
			<< acsConfig.zhangPppAr.hou_product_coordinate
			<< " absolute_satellite_integer_required=0"
			<< " user_ambiguity_datum=ONE_REFERENCE_PER_SYSTEM_SIGNAL"
			<< " network_integer_ready=" << networkIntegerReady
			<< " fixed_branch_transactional="
			<< acsConfig.zhangPppAr.transactional_integer_fixing;
	}

	struct E25bSystemStructure
	{
		ZhangGraphIntegerContext graph;
		ZhangSatelliteProductTarget coordinateTarget;
		std::map<SatSys, ZhangProductIntegerFunctional> products;
		ZhangUserIntegerLatticeAudit jointAudit;
		bool valid = false;
		std::string failureReason = "NOT_INITIALISED";
	};
	std::map<E_Sys, E25bSystemStructure> e25bStructures;
	auto e25bStructure = [&](E_Sys system) -> E25bSystemStructure&
	{
		auto [iterator, inserted] = e25bStructures.try_emplace(system);
		auto& structure = iterator->second;
		if (!inserted)
		{
			return structure;
		}
		if (!zhangGraphIntegerContext(fixedState, system, structure.graph))
		{
			structure.failureReason = "NO_GRAPH_INTEGER_CONTEXT";
			return structure;
		}
		structure.coordinateTarget = zhangBuildSatelliteProductTarget(
			structure.graph.basis, structure.graph.productBasis);
		if (!structure.coordinateTarget.valid)
		{
			structure.failureReason =
				"COORDINATE_TARGET_" + structure.coordinateTarget.failureReason;
			return structure;
		}
		structure.products = zhangBuildProductIntegerFunctionals(
			structure.graph.productBasis,
			structure.graph.arcVersions,
			structure.coordinateTarget.referenceSatellite,
			structure.graph.productDatumVersion);
		if (structure.products.empty())
		{
			structure.failureReason = "PRODUCT_INTEGER_FUNCTIONALS_EMPTY";
			return structure;
		}
		auto joint = zhangBuildJointUserIntegerFunctional(
			structure.products,
			structure.coordinateTarget.referenceSatellite,
			0);
		structure.jointAudit = zhangAuditUserIntegerLattice(joint);
		structure.valid = structure.jointAudit.valid;
		structure.failureReason = structure.valid
			? "NONE"
			: "JOINT_AUDIT_" + structure.jointAudit.failureReason;
		return structure;
	};

    vector<ZhangInternalProduct> epochProducts;
	vector<ZhangTemporalProductSnapshotRequest> temporalSnapshotRequests;
	set<string> queuedTemporalSnapshotIdentities;
    auto writeSolution = [&](const KFState& state, const string& solution)
    {
        for (const auto& [phaseKey, phaseIndex] : state.kfIndexMap)
        {
            if (phaseKey.type != KF::PHASE_BIAS ||
                phaseKey.Sat.prn <= 0 ||
                !phaseKey.str.empty() ||
                !zhangGraphProductSatelliteActive(fixedState, phaseKey.Sat) ||
                !zhangPppArUsesObservable(
                    phaseKey.Sat.sys,
                    static_cast<E_ObsCode>(phaseKey.num)
                ))
            {
                continue;
            }

            KFKey clockKey;
            clockKey.type = KF::SAT_CLOCK;
            clockKey.Sat  = phaseKey.Sat;
            auto clockIt = state.kfIndexMap.find(clockKey);
            if (clockIt == state.kfIndexMap.end())
            {
                continue;
            }

            E_ObsCode code = static_cast<E_ObsCode>(phaseKey.num);
            double lambda = wavelength(phaseKey.Sat.sys, code);
            if (lambda <= 0)
            {
                continue;
            }

            ProductKey productKey{phaseKey.Sat, code};
            auto& continuity = continuityMap[productKey];
            initialiseContinuityState(productKey, continuity);
            continuity.advanceEpoch(state.time);

            ZhangGraphIntegerContext graphContext;
            bool structureValid =
                zhangGraphIntegerContext(
                    fixedState, phaseKey.Sat.sys, graphContext
                ) &&
                zhangCanonicalIntegerAudit(graphContext.basis).valid;
            ZhangSatelliteDatumStatus datumStatus =
                satelliteDatumManager(phaseKey.Sat.sys, code).status(
                    phaseKey.Sat, structureValid
                );
            int clockIndex = clockIt->second;
            double clock = state.x(clockIndex);
            double rawPhase = state.x(phaseIndex);
            double houAlignmentCycles =
                static_cast<double>(continuity.integerShiftCycles) +
                continuity.fractionalShiftCycles;
            double productAlignmentCycles = houOsbLike
                ? houAlignmentCycles
                : static_cast<double>(datumStatus.alignmentCycles);
			bool e25bStructuralValid = false;
			bool e25bAlignmentEvaluable = false;
			bool e25bRuntimeAlignmentProven = false;
			ZhangExactVector e25bCoordinateRow;
			VectorXd e25bFullStateRow;
			string e25bSnapshotIdentity;
			double e25bExpectedRelativeAlignment =
				std::numeric_limits<double>::quiet_NaN();
			double e25bAppliedRelativeAlignment =
				std::numeric_limits<double>::quiet_NaN();
			double e25bLegacyRelativeAlignment =
				std::numeric_limits<double>::quiet_NaN();
			double e25bAlignmentError =
				std::numeric_limits<double>::quiet_NaN();
			std::string e25bFunctionalFingerprint = "NOT_AVAILABLE";
			std::string e25bPhysicalFunctionalIdentity = "NOT_AVAILABLE";
			std::string e25bPhaseSegmentIdentity = "NOT_AVAILABLE";
			const ZhangProductIntegerFunctional* e25bStructuralFunctional = nullptr;
			if (houOsbLike)
			{
				auto& e25b = e25bStructure(phaseKey.Sat.sys);
				auto structural = e25b.products.find(phaseKey.Sat);
				e25bStructuralValid = e25b.valid && structural != e25b.products.end();
				if (e25bStructuralValid)
				{
					e25bStructuralFunctional = &structural->second;
					e25bFunctionalFingerprint =
						zhangProductIntegerFunctionalFingerprint(structural->second);
					e25bPhysicalFunctionalIdentity =
						zhangProductPhysicalFunctionalFingerprint(structural->second);
					e25bPhaseSegmentIdentity =
						zhangHybridPhaseProductSegmentId(
							phaseKey.Sat, code, datumStatus.phaseSegment);
					e25bSnapshotIdentity = temporalProductSnapshotIdentity(
						phaseKey.Sat.sys,
						phaseKey.Sat,
						code,
						e25bPhysicalFunctionalIdentity,
						e25bPhaseSegmentIdentity);
					e25bCoordinateRow = ZhangExactVector(
						e25b.coordinateTarget.currentChords.size());
					if (phaseKey.Sat != e25b.coordinateTarget.referenceSatellite)
					{
						for (std::size_t row = 0;
							 row < e25b.coordinateTarget.targetSatellites.size(); row++)
						{
							if (e25b.coordinateTarget.targetSatellites[row] == phaseKey.Sat)
							{
								e25bCoordinateRow =
									e25b.coordinateTarget.matrix[row];
								break;
							}
						}
					}
					e25bExpectedRelativeAlignment = 0;
					e25bFullStateRow = VectorXd::Zero(state.x.size());
					e25bAlignmentEvaluable = e25bCoordinateRow.size() ==
						e25b.coordinateTarget.currentChords.size();
					for (std::size_t chord = 0;
						 e25bAlignmentEvaluable &&
						 chord < e25bCoordinateRow.size(); chord++)
					{
						if (e25bCoordinateRow[chord] == 0)
						{
							continue;
						}
						KFKey ambiguityKey;
						ambiguityKey.type = KF::AMBIGUITY;
						ambiguityKey.str =
							e25b.coordinateTarget.currentChords[chord].receiver;
						ambiguityKey.Sat =
							e25b.coordinateTarget.currentChords[chord].satellite;
						ambiguityKey.num = static_cast<int>(code);
						auto ambiguity = state.kfIndexMap.find(ambiguityKey);
						if (ambiguity == state.kfIndexMap.end())
						{
							e25bAlignmentEvaluable = false;
							break;
						}
						// The raw satellite phase is the current-tree node
						// potential z_T.  The exact product-tree coordinate is
						// z_P = z_T + G k; see zhangBuildSatelliteProductTarget().
						e25bExpectedRelativeAlignment +=
							e25bCoordinateRow[chord].convert_to<double>() *
							state.x(ambiguity->second);
						e25bFullStateRow(ambiguity->second) +=
							e25bCoordinateRow[chord].convert_to<double>();
					}
					ProductKey referenceKey{
						e25b.coordinateTarget.referenceSatellite, code};
					auto& referenceContinuity = continuityMap[referenceKey];
					initialiseContinuityState(referenceKey, referenceContinuity);
					e25bLegacyRelativeAlignment = houAlignmentCycles -
						(static_cast<double>(referenceContinuity.integerShiftCycles) +
						 referenceContinuity.fractionalShiftCycles);
				}
			}
			if (houOsbLike && !persistentHouCoordinate &&
				acsConfig.zhangPppAr.fixed_lag_factor_capture_shadow &&
				e25bStructuralValid && e25bAlignmentEvaluable &&
				e25bFullStateRow.size() == state.x.size())
			{
				// A zero alignment row is a valid deterministic snapshot.  It is
				// required when a product functional coincides with the current
				// tree coordinate: a later non-zero row must still be able to form
				// an old/new BESD against this exact zero target.
				if (queuedTemporalSnapshotIdentities.insert(
						e25bSnapshotIdentity).second)
				{
					temporalSnapshotRequests.push_back({
						phaseKey.Sat.sys,
						phaseKey.Sat,
						code,
						e25bSnapshotIdentity,
						e25bFullStateRow,
						e25bStructuralFunctional->affineOffsetCycles
							.convert_to<double>(),
						state.time});
				}
			}
			if (houOsbLike && e25bStructuralValid)
			{
				auto identity = houProductPhysicalFunctionalIdentities.find(productKey);
				if (identity == houProductPhysicalFunctionalIdentities.end())
				{
					houProductPhysicalFunctionalIdentities[productKey] =
						e25bPhysicalFunctionalIdentity;
					houProductPhysicalFunctionals[productKey] =
						*e25bStructuralFunctional;
					houProductSBasisFingerprints[productKey] =
						e25bFunctionalFingerprint;
					houProductPhaseSegmentIdentities[productKey] =
						e25bPhaseSegmentIdentity;
					houProductSnapshotIdentities[productKey] =
						e25bSnapshotIdentity;
				}
				else if (
					identity->second != e25bPhysicalFunctionalIdentity ||
					houProductPhaseSegmentIdentities.at(productKey) !=
						e25bPhaseSegmentIdentity)
				{
					const string previousIdentity = identity->second;
					const auto previousFunctional =
						houProductPhysicalFunctionals.at(productKey);
					const string previousSBasisFingerprint =
						houProductSBasisFingerprints.at(productKey);
					const string previousPhaseSegmentIdentity =
						houProductPhaseSegmentIdentities.at(productKey);
					const string previousSnapshotIdentity =
						houProductSnapshotIdentities[productKey];
					const auto transport = functionalDifferenceSupport(
						code, previousFunctional,
						*e25bStructuralFunctional);
					const bool phaseSegmentChanged =
						previousPhaseSegmentIdentity != e25bPhaseSegmentIdentity;
					const bool exactIntegerTransport =
						transport.contained && !phaseSegmentChanged;
					ZhangProductFunctionalEventDiagnostic eventDiagnostic;
					vector<ZhangGraphEdge> oldSupportEdges;
					vector<int> oldSupportVersions;
					vector<ZhangGraphEdge> newSupportEdges;
					vector<int> newSupportVersions;
					auto collectSupport = [](
						const ZhangProductIntegerFunctional& functional,
						vector<ZhangGraphEdge>& edges,
						vector<int>& versions)
					{
						for (size_t index = 0;
							 index < functional.physicalEdges.size(); index++)
						{
							if (functional.networkCoefficients[index] == 0)
							{
								continue;
							}
							edges.push_back(functional.physicalEdges[index]);
							versions.push_back(
								functional.physicalArcVersions[index]);
						}
					};
					collectSupport(previousFunctional,
						oldSupportEdges, oldSupportVersions);
					collectSupport(*e25bStructuralFunctional,
						newSupportEdges, newSupportVersions);
					const bool eventDiagnosticAvailable =
						zhangProductFunctionalEventDiagnostic(
							fixedState, phaseKey.Sat.sys,
							oldSupportEdges, oldSupportVersions,
							newSupportEdges, newSupportVersions,
							eventDiagnostic);
					identity->second = e25bPhysicalFunctionalIdentity;
					houProductPhysicalFunctionals[productKey] =
						*e25bStructuralFunctional;
					houProductSBasisFingerprints[productKey] =
						e25bFunctionalFingerprint;
					houProductPhaseSegmentIdentities[productKey] =
						e25bPhaseSegmentIdentity;
					houProductSnapshotIdentities[productKey] =
						e25bSnapshotIdentity;
					if (exactIntegerTransport)
					{
						houProductTreeAlignmentCycles[productKey] -= transport.value;
						continuity.resetReason =
							"hou_exact_integer_product_functional_transport";
					}
					else
					{
						if (acsConfig.zhangPppAr
								.temporal_product_transition_shadow)
						{
							ZhangPendingProductTransition pending;
							pending.eventTime = state.time;
							pending.system = phaseKey.Sat.sys;
							pending.satellite = phaseKey.Sat;
							pending.observable = code;
							pending.eventId = state.time.to_ISOstring(0) + ":" +
								enum_to_string(phaseKey.Sat.sys) + ":" +
								phaseKey.Sat.id() + ":" + enum_to_string(code);
							pending.oldFunctional = previousFunctional;
							pending.newFunctional = *e25bStructuralFunctional;
							pending.transition =
								zhangProductIntegerFunctionalDifference(
									previousFunctional,
									*e25bStructuralFunctional);
							pending.oldIdentity = previousIdentity;
							pending.newIdentity = e25bPhysicalFunctionalIdentity;
							pending.oldSBasisFingerprint =
								previousSBasisFingerprint;
							pending.newSBasisFingerprint =
								e25bFunctionalFingerprint;
							pending.oldPhaseSegmentIdentity =
								previousPhaseSegmentIdentity;
							pending.newPhaseSegmentIdentity =
								e25bPhaseSegmentIdentity;
							pending.phaseSegmentChanged = phaseSegmentChanged;
							pending.eventCause = eventDiagnosticAvailable
								? eventDiagnostic.eventCause
								: "DIAGNOSTIC_UNAVAILABLE";
							pending.oldProductSegment = continuity.counter;
							pending.newProductSegment = continuity.counter + 1;
							pending.oldSnapshotIdentity =
								previousSnapshotIdentity;
							pending.newSnapshotIdentity =
								e25bSnapshotIdentity;
							pending.exactTransformChainId =
								previousSnapshotIdentity + "->" +
								e25bSnapshotIdentity;
							if (!integerLedgerRuntimeId.empty())
							{
								pendingProductTransitions[integerLedgerRuntimeId]
									.push_back(std::move(pending));
							}
						}
						houProductTreeAlignmentCycles[productKey] = 0;
						if (phaseSegmentChanged)
						{
							continuity.reinitialise(
								state.time,
								"hou_product_phase_segment_change",
								acsConfig.zhangPppAr.stabilization_epochs);
						}
						else
						{
							// Loss of proof for a backend support change is an
							// alignment suspension, not a physical broadcast
							// segment event.  Preserve the product segment and
							// exact persistent relation graph, but fail AR closed
							// until the current potential is independently relinked.
							satelliteDatumManager(phaseKey.Sat.sys, code)
								.quarantineCurrentAlignments({phaseKey.Sat});
							continuity.resetReason =
								"hou_backend_functional_alignment_suspended";
						}
					}
					trace << "\nZHANG_HOU_PRODUCT_FUNCTIONAL_"
						  << (exactIntegerTransport ? "TRANSPORT"
							  : phaseSegmentChanged ? "RESET" : "SUSPEND")
						  << " time="
						  << state.time.to_string(0)
						  << " system=" << enum_to_string(phaseKey.Sat.sys)
						  << " satellite=" << phaseKey.Sat.id()
						  << " observable=" << enum_to_string(code)
						  << " counter=" << continuity.counter
						  << " datum_version=" << continuity.datumVersion
						  << " exact_integer_transport=" << exactIntegerTransport
						  << " phase_segment_changed=" << phaseSegmentChanged
						  << " transport_cycles=" << transport.value
						  << " held_rank=" << transport.heldRank
						  << " support_reason=" << transport.reason
						  << " event_cause="
						  << (eventDiagnosticAvailable
								  ? eventDiagnostic.eventCause
								  : "DIAGNOSTIC_UNAVAILABLE")
						  << " old_receiver_support_arcs=";
					auto traceSupport = [&](const auto& support)
					{
						if (support.empty())
						{
							trace << "NONE";
							return;
						}
						for (size_t index = 0; index < support.size(); index++)
						{
							const auto& item = support[index];
							trace << (index == 0 ? "" : "|")
								  << item.edge.receiver << ":"
								  << item.edge.satellite.id() << "@"
								  << item.arcVersion << ":age_epochs:"
								  << item.ageEpochs << ":n_epochs:"
								  << item.observationEpochs;
						}
					};
					traceSupport(eventDiagnostic.oldSupport);
					trace << " new_receiver_support_arcs=";
					traceSupport(eventDiagnostic.newSupport);
					auto minimumSupportMetric = [](const auto& support,
						auto member)
					{
						int minimum = std::numeric_limits<int>::max();
						for (const auto& item : support)
						{
							minimum = std::min(minimum, item.*member);
						}
						return minimum == std::numeric_limits<int>::max()
							? -1 : minimum;
					};
					trace << " old_arc_age_epochs="
						  << minimumSupportMetric(eventDiagnostic.oldSupport,
							  &ZhangPhysicalArcDiagnostic::ageEpochs)
						  << " new_arc_age_epochs="
						  << minimumSupportMetric(eventDiagnostic.newSupport,
							  &ZhangPhysicalArcDiagnostic::ageEpochs)
						  << " old_n_epochs="
						  << minimumSupportMetric(eventDiagnostic.oldSupport,
							  &ZhangPhysicalArcDiagnostic::observationEpochs)
						  << " new_n_epochs="
						  << minimumSupportMetric(eventDiagnostic.newSupport,
							  &ZhangPhysicalArcDiagnostic::observationEpochs)
						  << " common_support_epochs="
						  << eventDiagnostic.commonObservationEpochs
						  << " old_identity=" << previousIdentity
						  << " new_identity=" << e25bPhysicalFunctionalIdentity
						  << " reason=NONZERO_PHYSICAL_PATH_OR_ARC_VERSION_CHANGED";
				}
			}
			if (houOsbLike && !persistentHouCoordinate &&
				e25bStructuralValid && e25bAlignmentEvaluable)
			{
				// Direct structural S-transport into the fixed product tree.
				// The old cumulative scalar is retained only as a diagnostic.
				productAlignmentCycles = e25bExpectedRelativeAlignment;
				e25bAppliedRelativeAlignment = productAlignmentCycles;
				e25bAlignmentError = e25bAppliedRelativeAlignment -
					e25bExpectedRelativeAlignment;
				e25bRuntimeAlignmentProven =
					std::abs(e25bAlignmentError) <= 1e-8;
			}
			const long long productTreeAlignmentCycles =
				persistentHouCoordinate ? 0 :
				houProductTreeAlignmentCycles[productKey];
			if (houOsbLike && !persistentHouCoordinate &&
				e25bRuntimeAlignmentProven)
			{
				productAlignmentCycles += productTreeAlignmentCycles;
			}
            double emittedPhase =
                rawPhase + productAlignmentCycles * lambda;
            double covariance = state.P(clockIndex, phaseIndex);
            double clockVariance = state.P(clockIndex, clockIndex);
            double phaseVariance = state.P(phaseIndex, phaseIndex);
            double correction = zhangUserPhaseCorrectionValue(
                clock,
                rawPhase,
                lambda,
                datumStatus.alignmentCycles
            );
            double correctionVariance =
                clockVariance + phaseVariance - 2 * covariance;
			if (houOsbLike)
			{
				if (!persistentHouCoordinate && e25bRuntimeAlignmentProven)
				{
					// The alignment G*k is a state functional, not a known
					// constant.  Propagate its complete covariance and all
					// clock/phase/ambiguity cross-covariances.  Keep the row
					// sparse: a dense row^T P row for every satellite made a
					// one-hour replay more than an order of magnitude slower.
					std::vector<std::pair<int, double>> emittedPhaseTerms = {
						{phaseIndex, +1.0}
					};
					const auto& e25b = e25bStructure(phaseKey.Sat.sys);
					for (std::size_t chord = 0;
						 chord < e25bCoordinateRow.size(); chord++)
					{
						if (e25bCoordinateRow[chord] == 0)
						{
							continue;
						}
						KFKey ambiguityKey;
						ambiguityKey.type = KF::AMBIGUITY;
						ambiguityKey.str =
							e25b.coordinateTarget.currentChords[chord].receiver;
						ambiguityKey.Sat =
							e25b.coordinateTarget.currentChords[chord].satellite;
						ambiguityKey.num = static_cast<int>(code);
						auto ambiguity = state.kfIndexMap.find(ambiguityKey);
						if (ambiguity != state.kfIndexMap.end())
						{
							emittedPhaseTerms.push_back({
								ambiguity->second,
								+lambda * e25bCoordinateRow[chord].convert_to<double>()
							});
						}
					}
					emittedPhase = productTreeAlignmentCycles * lambda;
					phaseVariance = 0;
					covariance = 0;
					for (const auto& [index, coefficient] : emittedPhaseTerms)
					{
						emittedPhase += coefficient * state.x(index);
						covariance += coefficient * state.P(index, clockIndex);
						for (const auto& [otherIndex, otherCoefficient] :
							 emittedPhaseTerms)
						{
							phaseVariance += coefficient * otherCoefficient *
								state.P(index, otherIndex);
						}
					}
					correction = clock - emittedPhase;
					correctionVariance =
						clockVariance + phaseVariance - 2 * covariance;
				}
				else
				{
					const auto target = zhangHouOsbLikePhaseCorrectionTarget(
						state.x.size(), clockIndex, phaseIndex, lambda,
						productAlignmentCycles);
					correction = target.value(state.x);
					correctionVariance = target.variance(state.P);
				}
			}
			if (houOsbLike && persistentHouCoordinate)
			{
				// Final frontend integer coordinate.  G_obs/product-tree
				// quantities above remain diagnostics and relation evidence;
				// only the persistent satellite potential kappa enters the
				// broadcast phase bias.
				emittedPhase = rawPhase
					+ lambda * static_cast<double>(datumStatus.alignmentCycles);
				phaseVariance = state.P(phaseIndex, phaseIndex);
				covariance = state.P(clockIndex, phaseIndex);
				correction = clock - emittedPhase;
				correctionVariance = clockVariance + phaseVariance
					- 2 * covariance;
			}

            ZhangInternalProduct product;
            product.time = state.time;
            product.satellite = phaseKey.Sat;
            product.observable = code;
            product.solution = solution;
            // Zhang dual-frequency IF/GF code biases are already absorbed by
            // the estimable satellite clock and ionosphere.  L1C and L2W use
            // this same clock product; no observable-specific code OSB exists.
            product.clock_m = clock;
            product.clock_sigma_m =
                std::sqrt(std::max(0.0, clockVariance));
            product.phase_m = emittedPhase;
            product.phase_sigma_m =
                std::sqrt(std::max(0.0, phaseVariance));
            product.clock_phase_covariance_m2 = covariance;
            product.correction_m = correction;
            product.correction_sigma_m =
                std::sqrt(std::max(0.0, correctionVariance));
            bool productPrecisionValid =
                acsConfig.zhangPppAr.maximum_pppar_correction_sigma_m <= 0 ||
                product.correction_sigma_m <=
                    acsConfig.zhangPppAr.maximum_pppar_correction_sigma_m;
            ZhangNamedProductIntegerSupport namedIntegerSupport;
            if (houOsbLike && solution == "FIXED")
            {
                const auto& e25b = e25bStructure(phaseKey.Sat.sys);
                auto functional = e25b.products.find(phaseKey.Sat);
                if (functional != e25b.products.end())
                {
                    namedIntegerSupport = zhangNamedProductIntegerSupport(
                        integerLedgerState,
                        phaseKey.Sat.sys,
                        code,
                        functional->second.physicalEdges,
                        functional->second.physicalArcVersions,
                        functional->second.networkCoefficients
                    );
                }
                else
                {
                    namedIntegerSupport.reason = "NO_NAMED_PRODUCT_FUNCTIONAL";
                }
            }
			const auto hybridInitialGate = zhangHybridInitialIntegerGate(
				persistentHouCoordinate,
				structureValid,
				e25bStructuralValid,
				e25bRuntimeAlignmentProven,
				namedIntegerSupport.contained,
				datumStatus.integerDatumContinuous,
				datumStatus.integerPrecisionValid);
            bool productIntegerCoordinateReady = houOsbLike
                ? hybridInitialGate.precisionValid
                : datumStatus.integerValid;
            if (!houOsbLike && solution == "FIXED" && fixedBranchValid &&
                productIntegerCoordinateReady && productPrecisionValid)
            {
                // Integer validity is a per-satellite, per-signal property.
                // A newly precise product must also pass the configured
                // stabilization window before it can be consumed by PPP-AR.
                continuity.markFixed(
                    state.time, acsConfig.zhangPppAr.stabilization_epochs
                );
            }
            else if (solution == "FIXED" && !productPrecisionValid)
            {
                continuity.markIntegerPrecisionUnavailable(
                    "product_correction_sigma_exceeded",
                    acsConfig.zhangPppAr.stabilization_epochs
                );
            }
            else if (solution == "FIXED" && !houOsbLike &&
                     !productIntegerCoordinateReady)
            {
                continuity.markIntegerPrecisionUnavailable(
                    "named_product_row_not_fixed:" + namedIntegerSupport.reason,
                    acsConfig.zhangPppAr.stabilization_epochs
                );
            }
            product.discontinuity_counter = houOsbLike
                ? continuity.counter
                : datumStatus.discontinuityCounter;
            product.integer_shift_cycles = datumStatus.alignmentCycles;
            product.fractional_shift_cycles = continuity.fractionalShiftCycles;
            product.datum_version = houOsbLike
                ? continuity.datumVersion
                : datumStatus.datumVersion;
            product.valid_from = continuity.validFrom;
            product.product_iod = continuity.iod;
            product.reset_reason = continuity.resetReason;
            ZhangCurrentAlignmentState alignmentState = houOsbLike
                ? ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_VALID
                : satelliteDatumManager(
                    phaseKey.Sat.sys, code).alignmentState(phaseKey.Sat);
            // Hou products intentionally do not claim a conventional
            // satellite-only persistent integer relation.  Their integer
            // compatibility comes from the fixed network cycle lattice and
            // the explicitly transported product S-coordinate instead.
            product.persistent_relation_known = houOsbLike
                ? false
                : datumStatus.componentSize >= 2;
            product.current_alignment_state =
                zhangCurrentAlignmentStateName(alignmentState);
            product.integer_structure_valid =
                houOsbLike
					? hybridInitialGate.structureValid
                    : datumStatus.integerStructureValid;
            product.integer_datum_continuous =
                houOsbLike
					? hybridInitialGate.datumContinuous
                    : datumStatus.integerDatumContinuous;
            product.integer_precision_valid =
                solution == "FIXED" && fixedBranchValid &&
                (houOsbLike
					? hybridInitialGate.precisionValid
                    : datumStatus.integerPrecisionValid) &&
                productPrecisionValid &&
                continuity.integerValid();
            product.integer_valid =
                product.integer_structure_valid &&
                product.integer_datum_continuous &&
                product.integer_precision_valid;
            product.branch_valid = solution == "FLOAT" ||
                (solution == "WL"
                    ? wideLaneBranchValid
                    : fixedBranchValid);
            double covarianceScale = std::max(
                1.0,
                std::max(std::abs(clockVariance), std::abs(phaseVariance))
            );
            bool finite =
                std::isfinite(clock) && std::isfinite(rawPhase) &&
                std::isfinite(emittedPhase) && std::isfinite(covariance) &&
                std::isfinite(clockVariance) &&
                std::isfinite(phaseVariance) &&
                std::isfinite(correctionVariance);
            bool covarianceValid =
                clockVariance >= -1e-10 * covarianceScale &&
                phaseVariance >= -1e-10 * covarianceScale &&
                correctionVariance >= -1e-10 * covarianceScale &&
                covariance * covariance <=
                    clockVariance * phaseVariance +
                    1e-10 * covarianceScale * covarianceScale;
            product.numeric_valid = finite && covarianceValid;
            product.continuity_valid =
				!houOsbLike || persistentHouCoordinate ||
				e25bRuntimeAlignmentProven;
            product.ppp_usable = product.numeric_valid && product.branch_valid &&
				product.continuity_valid;
            product.pppar_usable =
                product.ppp_usable && product.integer_valid;
            product.invalid_reason = !product.branch_valid
                ? "FIXED_TRANSACTION_ABORTED"
                : (!finite
                       ? "NONFINITE_PRODUCT"
                       : (!covarianceValid
                              ? "INVALID_PRODUCT_COVARIANCE"
                              : "NONE"));
            product.integer_component_id = houOsbLike
                ? "HOU-" + enum_to_string(phaseKey.Sat.sys) + "-" +
                    enum_to_string(code) + "-NETWORK-CYCLE"
                : datumStatus.componentId;
            product.integer_datum_id = houOsbLike
                ? "HOU-" + enum_to_string(phaseKey.Sat.sys) + "-" +
                    enum_to_string(code) + "-V" +
                    std::to_string(continuity.datumVersion)
                : enum_to_string(phaseKey.Sat.sys) + "-" +
                    enum_to_string(code) + "-V" +
                    std::to_string(datumStatus.datumVersion) + "-SEG" +
                    std::to_string(datumStatus.phaseSegment);
			product.support_segment_fingerprint = houOsbLike
				? e25bPhysicalFunctionalIdentity : "MANAGER_RELATION_GRAPH";

            epochProducts.push_back(product);

            if (acsConfig.zhangPppAr.output_diagnostics)
            {
                trace << "\nZHANG_CONTINUITY_PRODUCT time=" << state.time.to_string(0)
                      << " solution=" << solution
					  << " product_mode="
					  << acsConfig.zhangPppAr.product_mode
					  << " absolute_satellite_integer_required="
					  << (!houOsbLike)
                      << " satellite=" << phaseKey.Sat.id()
                      << " observable=" << enum_to_string(code)
                      << " counter=" << continuity.counter
                      << " integer_shift_cycles=" << continuity.integerShiftCycles
                      << " fractional_shift_cycles=" << continuity.fractionalShiftCycles
                      << " datum_version=" << continuity.datumVersion
                      << " iod=" << continuity.iod
                      << " newly_fixed=" << newlyFixed
                      << " integer_datum_complete=" << integerDatumComplete
                      << " network_integer_ready=" << networkIntegerReady
					  << " named_product_integer_supported="
					  << namedIntegerSupport.contained
					  << " named_product_integer_value="
					  << namedIntegerSupport.value
					  << " named_product_held_rank="
					  << namedIntegerSupport.heldRank
					  << " named_product_support_reason="
					  << namedIntegerSupport.reason
                      << " persistent_relation_known="
                      << product.persistent_relation_known
                      << " current_alignment_state="
                      << product.current_alignment_state
                      << " integer_structure_valid="
                      << product.integer_structure_valid
                      << " integer_datum_continuous="
                      << product.integer_datum_continuous
                      << " integer_precision_valid="
                      << product.integer_precision_valid
                      << " integer_component_size="
                      << (houOsbLike ? 0 : datumStatus.componentSize)
                      << " integer_valid=" << product.integer_valid
                      << " numeric_valid=" << product.numeric_valid
                      << " branch_valid=" << product.branch_valid
                      << " ppp_usable=" << product.ppp_usable
                      << " pppar_usable=" << product.pppar_usable
                      << " invalid_reason=" << product.invalid_reason
                      << " reason=" << continuity.resetReason;
				if (houOsbLike)
				{
					trace << "\nZHANG_E25B_PRODUCT_INTEGER_FUNCTIONAL time="
						  << state.time.to_string(0)
						  << " solution=" << solution
						  << " satellite=" << phaseKey.Sat.id()
						  << " observable=" << enum_to_string(code)
						  << " product_coordinate_valid="
						  << product.continuity_valid
						  << " product_coordinate_mode="
						  << acsConfig.zhangPppAr.hou_product_coordinate
						  << " structural_functional_valid="
						  << e25bStructuralValid
						  << " user_integer_lattice_valid="
						  << (e25bStructuralValid && e25bRuntimeAlignmentProven)
						  << " alignment_evaluable="
						  << e25bAlignmentEvaluable
						  << " runtime_alignment_proven="
						  << e25bRuntimeAlignmentProven
						  << " expected_relative_alignment_cycles="
						  << e25bExpectedRelativeAlignment
						  << " applied_relative_alignment_cycles="
						  << e25bAppliedRelativeAlignment
						  << " persistent_product_tree_alignment_cycles="
						  << productTreeAlignmentCycles
						  << " legacy_relative_alignment_cycles="
						  << e25bLegacyRelativeAlignment
						  << " legacy_alignment_error_cycles="
						  << (e25bLegacyRelativeAlignment -
							  e25bExpectedRelativeAlignment)
						  << " alignment_error_cycles="
						  << e25bAlignmentError
						  << " max_real_nuisance_coefficient="
						  << e25bStructure(phaseKey.Sat.sys).
							 jointAudit.maximumNuisanceCoefficient
						  << " max_affine_integer_error="
						  << e25bStructure(phaseKey.Sat.sys).
							 jointAudit.maximumAffineIntegerError
						  << " primitive_admissible="
						  << e25bStructure(phaseKey.Sat.sys).
							 jointAudit.primitiveAdmissible
						  << " structural_failure_reason="
						  << e25bStructure(phaseKey.Sat.sys).failureReason
						  << " temporal_basis_version="
						  << e25bStructure(phaseKey.Sat.sys).
							 graph.productDatumVersion
						  << " functional=" << e25bFunctionalFingerprint;
				}
            }
        }
    };

    writeSolution(floatState, "FLOAT");
    if (wideLaneState && wideLaneBranchValid)
    {
        writeSolution(*wideLaneState, "WL");
    }
    writeSolution(fixedState, "FIXED");
    if (!temporalSnapshotRequests.empty())
    {
        registerZhangTemporalProductSnapshots(
            trace, integerLedgerState, temporalSnapshotRequests);
    }

    // The observation tree is only a backend coordinate system.  Promote
    // exact pair functionals into the persistent broadcast AR graph and let
    // that graph, rather than an epoch-local DSU or an arbitrary tree root,
    // define the satellite integer components exposed to users.
    if (houOsbLike)
    {
        using ProductGroup = tuple<string, E_Sys, E_ObsCode>;
        map<ProductGroup, vector<int>> groups;
        for (int index = 0; index < static_cast<int>(epochProducts.size()); index++)
        {
            const auto& product = epochProducts[index];
            if (product.solution == "FIXED")
            {
                groups[{product.solution, product.satellite.sys,
                        product.observable}].push_back(index);
            }
        }

        for (const auto& [group, indices] : groups)
        {
            const auto& [solution, system, observable] = group;
			auto& datumManager = satelliteDatumManager(system, observable);
			std::set<SatSys> initialGaugeSatellites;
			for (int index : indices)
			{
				const auto& product = epochProducts[index];
				if (product.numeric_valid && product.branch_valid
				 && product.continuity_valid
				 && product.integer_structure_valid)
				{
					initialGaugeSatellites.insert(product.satellite);
				}
			}
			ZhangFrontendGaugeInitialisation gaugeInitialisation;
			if (solution == "FIXED" && fixedBranchValid
			 && networkIntegerReady && newlyFixed > 0)
			{
				gaugeInitialisation =
					datumManager.initialiseFrontendGaugeComponent(
						initialGaugeSatellites);
				if (acsConfig.zhangPppAr.output_diagnostics
				 && gaugeInitialisation.reason !=
					"FRONTEND_GAUGE_ALREADY_INITIALISED")
				{
					trace << "\nZHANG_HYBRID_FRONTEND_GAUGE_INITIALISATION time="
						  << fixedState.time.to_string(0)
						  << " solution=" << solution
						  << " system=" << enum_to_string(system)
						  << " observable=" << enum_to_string(observable)
						  << " accepted=" << gaugeInitialisation.accepted
						  << " satellites=" << gaugeInitialisation.satelliteCount
						  << " zero_kappa_relations="
						  << gaugeInitialisation.relationCount
						  << " reason=" << gaugeInitialisation.reason
						  << " coordinate_definition=1"
						  << " statistical_fix=0";
				}
			}
            int testedPairs = 0;
            int certifiedPairs = 0;
            int promotedPairs = 0;
            int pendingPairs = 0;
            int rejectedPairs = 0;
			// Do not compile the current satellite-to-satellite product path as
			// an integer fact.  At frontend birth kappa=0 is an exact coordinate
			// definition.  Only a subsequent backend S-basis/physical-segment
			// transition produces the g_sp functional that must be tested against
			// the persistent fixed lattice before transport or component merge.

            set<string> usableComponents;
            int usableSatellites = 0;
            for (int index : indices)
            {
                auto& product = epochProducts[index];
				const auto status = datumManager.status(
					product.satellite, product.integer_structure_valid);
                product.phase_product_segment_id =
					zhangHybridPhaseProductSegmentId(
						product.satellite, observable, status.phaseSegment);
                product.integer_component_id = status.componentId;
                product.integer_component_version = status.componentVersion;
                product.integer_alignment_generation =
                    status.alignmentGeneration;
                product.backend_s_basis_generation =
                    backendSBasisGenerations[system];
                product.integer_component_size = status.componentSize;
                product.integer_component_rank = status.componentRank;
                product.certified_relation_count =
                    status.certifiedRelationCount;
                product.redundant_relation_count =
                    status.redundantRelationCount;
                product.cycle_closure_valid = status.cycleClosureValid;
                product.persistent_relation_known =
                    status.componentSize >= 2;
                product.current_alignment_state = zhangCurrentAlignmentStateName(
					datumManager.alignmentState(product.satellite));
                product.integer_datum_continuous =
                    status.integerDatumContinuous;
				const double integerAlignmentDeltaMetres = wavelength(
					system, observable) * static_cast<double>(
						status.alignmentCycles - product.integer_shift_cycles);
				product.phase_m += integerAlignmentDeltaMetres;
				product.correction_m = product.clock_m - product.phase_m;
                const bool precisionPass =
                    acsConfig.zhangPppAr.maximum_pppar_correction_sigma_m <= 0
                    || product.correction_sigma_m <=
                       acsConfig.zhangPppAr.maximum_pppar_correction_sigma_m;
				auto& continuity = continuityMap[
					{product.satellite, observable}];
				if (product.integer_structure_valid
				 && status.integerDatumContinuous
				 && status.integerPrecisionValid
				 && status.cycleClosureValid
				 && status.componentSize >= 2
				 && product.branch_valid && precisionPass)
				{
					continuity.markFixed(
						fixedState.time,
						acsConfig.zhangPppAr.stabilization_epochs);
				}
				else
				{
					continuity.markIntegerPrecisionUnavailable(
						"hybrid_component_ar_gate_unavailable",
						acsConfig.zhangPppAr.stabilization_epochs);
				}
                product.integer_precision_valid =
                    status.integerPrecisionValid
					&& continuity.integerValid()
                    && product.branch_valid && precisionPass;
                product.integer_valid =
                    product.integer_structure_valid
                    && product.integer_datum_continuous
                    && product.integer_precision_valid
                    && product.cycle_closure_valid
                    && product.integer_component_size >= 2;
				product.discontinuity_counter = continuity.counter;
				product.integer_shift_cycles = status.alignmentCycles;
				product.fractional_shift_cycles =
					continuity.fractionalShiftCycles;
				product.datum_version = continuity.datumVersion;
				product.valid_from = continuity.validFrom;
				product.product_iod = continuity.iod;
				product.reset_reason = continuity.resetReason;
                if (product.integer_component_size < 2)
                {
                    product.integer_component_id = "NONE";
                    product.invalid_reason = "NO_CERTIFIED_SATELLITE_PAIR";
                }
                else
                {
                    product.integer_datum_id =
                        status.componentId + "-V" +
                        std::to_string(status.componentVersion) + "-A" +
                        std::to_string(status.alignmentGeneration) +
                        "-RELATIVE";
                    usableComponents.insert(status.componentId);
                    usableSatellites++;
                    if (product.integer_valid)
                    {
                        product.invalid_reason = "NONE";
                    }
                }
            }
            if (acsConfig.zhangPppAr.output_diagnostics)
            {
                trace << "\nZHANG_HYBRID_PAIR_COMPONENT_SUMMARY time="
                      << fixedState.time.to_string(0)
                      << " solution=" << solution
                      << " system=" << enum_to_string(system)
                      << " observable=" << enum_to_string(observable)
                      << " satellites=" << indices.size()
                      << " tested_pairs=" << testedPairs
                      << " certified_pairs=" << certifiedPairs
                      << " promoted_pairs=" << promotedPairs
                      << " pending_pairs=" << pendingPairs
                      << " rejected_pairs=" << rejectedPairs
					  << " initial_gauge_accepted="
					  << gaugeInitialisation.accepted
					  << " initial_zero_kappa_relations="
					  << gaugeInitialisation.relationCount
                      << " usable_components=" << usableComponents.size()
                      << " usable_satellites=" << usableSatellites
                      << " absolute_satellite_integer_required=0";
            }
        }
    }

    // A dual-frequency integer product exists only on the intersection of
    // the two independently certified signal components.  Merely observing
    // the same satellite on L1 and L2 is not an integer certificate.
    if (houOsbLike)
    {
        for (const auto& [system, observables] :
             acsConfig.zhangPppAr.baseline_observables)
        {
            if (observables.size() != 2)
            {
                continue;
            }
            map<SatSys, array<int, 2>> bySatellite;
            for (int index = 0;
                 index < static_cast<int>(epochProducts.size()); index++)
            {
                const auto& product = epochProducts[index];
                if (product.solution != "FIXED"
                 || product.satellite.sys != system)
                {
                    continue;
                }
                int signal = product.observable == observables[0] ? 0
                    : product.observable == observables[1] ? 1 : -1;
                if (signal < 0)
                {
                    continue;
                }
                auto [it, inserted] = bySatellite.try_emplace(
                    product.satellite, array<int, 2>{-1, -1});
                it->second[signal] = index;
            }
			map<SatSys, string> firstComponents;
			map<SatSys, string> secondComponents;
            for (const auto& [satellite, pair] : bySatellite)
            {
                if (pair[0] < 0 || pair[1] < 0)
                {
                    continue;
                }
                const auto& first = epochProducts[pair[0]];
                const auto& second = epochProducts[pair[1]];
                if (!first.integer_valid || !second.integer_valid)
                {
                    continue;
                }
                firstComponents[satellite] =
                    first.integer_component_id + "@V" +
                    std::to_string(first.integer_component_version) + "@A" +
                    std::to_string(first.integer_alignment_generation);
				secondComponents[satellite] =
                    second.integer_component_id + "@V" +
                    std::to_string(second.integer_component_version) + "@A" +
                    std::to_string(second.integer_alignment_generation);
            }
			const auto intersections = zhangHybridDualFrequencyComponents(
				firstComponents, secondComponents);
            for (const auto& [identity, members] : intersections)
            {
				for (const auto& satellite : members)
                {
					const auto& pair = bySatellite.at(satellite);
                    epochProducts[pair[0]].dual_frequency_ar_valid = true;
                    epochProducts[pair[1]].dual_frequency_ar_valid = true;
                }
            }
        }
    }

    // A reference-satellite zero row is structurally contained in every
    // lattice, but it cannot authorize PPP-AR by itself.  Require at least
    // one supported non-reference partner in the same solution/signal group.
    map<pair<string, E_ObsCode>, int> integerProductCounts;
    for (const auto& product : epochProducts)
    {
        if (product.integer_valid)
        {
            integerProductCounts[{product.solution, product.observable}]++;
        }
    }
    for (auto& product : epochProducts)
    {
        if (product.integer_valid &&
            integerProductCounts[{product.solution, product.observable}] < 2)
        {
            product.integer_precision_valid = false;
            product.integer_valid = false;
            product.invalid_reason = "NO_NAMED_PRODUCT_PAIR";
        }
    }

	// Canonicalise the complete frontend primitive vector before any numeric
	// or continuity gate.  The same affine maps are applied to the full
	// covariance, including clock-phase, cross-signal and cross-satellite
	// blocks, and are then consumed by both the product CSV and the user noise
	// adapter.
    appendProductCovariance(
		floatState, "FLOAT", fixedState, epochProducts);
    if (wideLaneState && wideLaneBranchValid)
    {
        appendProductCovariance(
			*wideLaneState, "WL", fixedState, epochProducts);
    }
    appendProductCovariance(
		fixedState, "FIXED", fixedState, epochProducts);

    // Reject satellite-dependent correction jumps after removing the robust
    // per-signal common mode.  A common clock-datum change can be absorbed by
    // the user's receiver clock; a non-common jump cannot.
    map<pair<string, E_ObsCode>, vector<int>> continuityGroups;
    for (int index = 0; index < static_cast<int>(epochProducts.size()); index++)
    {
        continuityGroups[
            {epochProducts[index].solution, epochProducts[index].observable}
        ].push_back(index);
    }
    const double maximumGap = std::max(120.0, 2.5 * acsConfig.epoch_interval);
    const double maximumResidualStep =
        acsConfig.zhangPppAr.maximum_product_residual_step_m;
    for (const auto& [group, indices] : continuityGroups)
    {
        map<int, double> deltas;
        vector<double> commonModeCandidates;
        for (int index : indices)
        {
            const auto& product = epochProducts[index];
            ProductHistoryKey key{
                product.solution, product.satellite, product.observable
            };
            auto previous = productHistoryMap.find(key);
            if (previous == productHistoryMap.end() ||
                !product.numeric_valid ||
                previous->second.discontinuityCounter !=
                    product.discontinuity_counter ||
                previous->second.datumVersion != product.datum_version)
            {
                continue;
            }
            double gap = (product.time - previous->second.time).to_double();
            if (!(gap > 0) || gap > maximumGap)
            {
                continue;
            }
            double delta = product.correction_m - previous->second.correction;
            if (std::isfinite(delta))
            {
                deltas[index] = delta;
                commonModeCandidates.push_back(delta);
            }
        }

        double commonMode = 0;
        if (!commonModeCandidates.empty())
        {
            auto middle = commonModeCandidates.begin() +
                commonModeCandidates.size() / 2;
            std::nth_element(
                commonModeCandidates.begin(), middle, commonModeCandidates.end()
            );
            commonMode = *middle;
        }
        for (int index : indices)
        {
            auto& product = epochProducts[index];
            auto delta = deltas.find(index);
            double residualStep = 0;
            if (delta != deltas.end())
            {
                residualStep = std::abs(delta->second - commonMode);
                if (!std::isfinite(residualStep) ||
                    (maximumResidualStep > 0 &&
                     residualStep > maximumResidualStep))
                {
                    product.continuity_valid = false;
                    product.invalid_reason =
                        "COMMON_MODE_REMOVED_STEP_EXCEEDED";
                }
            }
            product.ppp_usable =
                product.numeric_valid && product.branch_valid &&
                product.continuity_valid;
            product.pppar_usable =
                product.ppp_usable && product.integer_valid;
			product.ar_valid = product.pppar_usable;
			product.dual_frequency_ar_valid =
				product.dual_frequency_ar_valid && product.pppar_usable;
			product.discontinuity =
				product.discontinuity_counter >
					acsConfig.zhangPppAr.initial_discontinuity_counter
				&& product.valid_from != GTime::noTime()
				&& std::abs(
					(product.time - product.valid_from).to_double()) < 1e-3;
			if (product.solution != "FIXED")
			{
				product.product_state = "FLOAT_ONLY";
			}
			else if (product.pppar_usable)
			{
				product.product_state = "AR_VALID";
			}
			else if (product.persistent_relation_known
			      && product.current_alignment_state !=
			         "CURRENT_ALIGNMENT_VALID")
			{
				product.product_state = "REACQUIRING";
			}
			else if (product.integer_component_size >= 2
			      || product.integer_structure_valid)
			{
				product.product_state = "AR_SUSPENDED";
			}
			else
			{
				product.product_state = "FLOAT_ONLY";
			}

            ProductHistoryKey key{
                product.solution, product.satellite, product.observable
            };
            if (product.ppp_usable)
            {
                productHistoryMap[key] = {
                    product.time,
                    product.correction_m,
                    product.discontinuity_counter,
                    product.datum_version
                };
            }
            appendProduct(product);
            if (acsConfig.zhangPppAr.output_diagnostics)
            {
                trace << "\nZHANG_PRODUCT_NUMERIC_GATE time="
                      << product.time.to_string(0)
                      << " solution=" << product.solution
                      << " satellite=" << product.satellite.id()
                      << " observable=" << enum_to_string(product.observable)
                      << " common_mode_step_m=" << commonMode
                      << " residual_step_m=" << residualStep
                      << " numeric_valid=" << product.numeric_valid
                      << " branch_valid=" << product.branch_valid
                      << " continuity_valid=" << product.continuity_valid
                      << " ppp_usable=" << product.ppp_usable
                      << " pppar_usable=" << product.pppar_usable
					  << " dual_frequency_ar_valid="
					  << product.dual_frequency_ar_valid
					  << " product_state=" << product.product_state
                      << " reason=" << product.invalid_reason;
            }
        }
    }
}

bool queryZhangInternalProduct(
    GTime                 time,
    const SatSys&         satellite,
    E_ObsCode             observable,
    ZhangInternalProduct& product
)
{
    if (!acsConfig.zhangPppAr.user_adapter || !loadProducts())
    {
        return false;
    }

    ProductLookupKey key{
        static_cast<long int>(std::llround(time.bigTime)),
        satellite,
        observable,
        acsConfig.zhangPppAr.product_solution
    };
    auto it = productMap.find(key);
    if (it == productMap.end())
    {
        return false;
    }
    product = it->second;
    return product.ppp_usable;
}

bool queryZhangInternalProductNoiseFactors(
    GTime                 time,
    const SatSys&         satellite,
    E_ObsCode             observable,
    bool                  phaseMeasurement,
    vector<double>&       factors,
    int*                  numericalRank,
    string*               failureReason
)
{
    factors.clear();
    if (!acsConfig.zhangPppAr.user_use_full_product_covariance)
    {
        if (failureReason)
        {
            *failureReason = "FULL_PRODUCT_COVARIANCE_DISABLED";
        }
        return false;
    }
    const std::lock_guard<std::mutex> lock(productCovarianceMutex);
    const long int epoch = static_cast<long int>(std::llround(time.bigTime));
    const string& solution = acsConfig.zhangPppAr.product_solution;
    if (!loadProductCovarianceEpoch(epoch, solution))
    {
        if (failureReason)
        {
            *failureReason = productCovarianceReader.cache.failureReason;
        }
        return false;
    }
    const auto& cache = productCovarianceReader.cache;
    ProductCovarianceKey clockKey{satellite, "CLOCK", E_ObsCode::NONE};
    auto clock = cache.parameterIndex.find(clockKey);
    if (clock == cache.parameterIndex.end())
    {
        if (failureReason)
        {
            *failureReason = "CLOCK_COVARIANCE_PARAMETER_MISSING";
        }
        return false;
    }
	Eigen::RowVectorXd row = cache.squareRoot.row(clock->second);
	if (phaseMeasurement)
	{
		ProductCovarianceKey phaseKey{satellite, "PHASE", observable};
		auto phase = cache.parameterIndex.find(phaseKey);
		if (phase == cache.parameterIndex.end())
		{
			if (failureReason)
			{
				*failureReason = "PHASE_COVARIANCE_PARAMETER_MISSING";
			}
			return false;
		}
		row -= cache.squareRoot.row(phase->second);
	}
    factors.assign(row.data(), row.data() + row.size());
    if (numericalRank)
    {
        *numericalRank = cache.numericalRank;
    }
    if (failureReason)
    {
        *failureReason = "NONE";
    }
    return std::all_of(
        factors.begin(), factors.end(),
        [](double value) { return std::isfinite(value); });
}

void updateZhangPppArUserReferences(
    Trace&       trace,
    ReceiverMap& receiverMap,
    KFState&     kfState
)
{
    if (!acsConfig.zhangPppAr.user_adapter)
    {
        return;
    }
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(kfState, runtimeId))
	{
		trace << "\nZHANG_USER_REFERENCE time="
			  << kfState.time.to_string(0)
			  << " status=REJECTED"
			  << " reason=CHECKPOINT_RUNTIME_ID_UNBOUND";
		return;
	}

    for (auto& [receiverId, receiver] : receiverMap)
    {
        if (!receiver.ready || receiver.obsList.empty())
        {
            continue;
        }

        for (const auto& [sys, observables] : acsConfig.zhangPppAr.baseline_observables)
        {
            SatSys forcedDualReference;
            if (acsConfig.zhangPppAr.integer_strategy ==
                    "CANONICAL_USER_IF_WL_L1" &&
                observables.size() == 2)
            {
                map<SatSys, double> dualCandidates;
				map<SatSys, string> dualIntegerComponents;
				map<string, int> dualIntegerComponentSizes;
                for (const auto& obs : only<GObs>(receiver.obsList))
                {
                    if (obs.Sat.sys != sys ||
                        !signalUsable(obs, observables[0]) ||
                        !signalUsable(obs, observables[1]))
                    {
                        continue;
                    }
                    ZhangInternalProduct firstProduct;
                    ZhangInternalProduct secondProduct;
                    vector<double> firstFactors;
                    vector<double> secondFactors;
                    if (!queryZhangInternalProduct(
                            kfState.time, obs.Sat, observables[0], firstProduct) ||
                        !queryZhangInternalProduct(
                            kfState.time, obs.Sat, observables[1], secondProduct) ||
                        !queryZhangInternalProductNoiseFactors(
                            kfState.time, obs.Sat, observables[0], true,
                            firstFactors) ||
                        !queryZhangInternalProductNoiseFactors(
                            kfState.time, obs.Sat, observables[1], true,
                            secondFactors))
                    {
                        continue;
                    }
                    dualCandidates[obs.Sat] =
                        obs.satStat_ptr ? obs.satStat_ptr->el : 0;
					if (firstProduct.pppar_usable && secondProduct.pppar_usable
					 && firstProduct.integer_component_id != "NONE"
					 && secondProduct.integer_component_id != "NONE")
					{
						const string component =
							firstProduct.integer_component_id + "|" +
							secondProduct.integer_component_id;
						dualIntegerComponents[obs.Sat] = component;
						dualIntegerComponentSizes[component]++;
					}
                }
                if (!dualCandidates.empty())
                {
					string preferredIntegerComponent;
					int preferredIntegerComponentSize = 1;
					for (const auto& [component, size] :
						 dualIntegerComponentSizes)
					{
						if (size > preferredIntegerComponentSize
						 || (size == preferredIntegerComponentSize
							 && !preferredIntegerComponent.empty()
							 && component < preferredIntegerComponent))
						{
							preferredIntegerComponent = component;
							preferredIntegerComponentSize = size;
						}
					}
					map<SatSys, double> referenceCandidates = dualCandidates;
					if (!preferredIntegerComponent.empty())
					{
						for (auto candidate = referenceCandidates.begin();
							 candidate != referenceCandidates.end();)
						{
							auto component = dualIntegerComponents.find(
								candidate->first);
							if (component == dualIntegerComponents.end()
							 || component->second != preferredIntegerComponent)
							{
								candidate = referenceCandidates.erase(candidate);
							}
							else
							{
								++candidate;
							}
						}
					}
                    UserDualReferenceKey dualKey{
						runtimeId, receiverId, sys};
                    auto& retained = userDualReferenceMap[dualKey];
                    if (retained.prn > 0 &&
						referenceCandidates.count(retained))
                    {
                        forcedDualReference = retained;
                    }
                    else
                    {
                        forcedDualReference = std::max_element(
                            referenceCandidates.begin(),
							referenceCandidates.end(),
                            [](const auto& left, const auto& right)
                            {
                                return left.second != right.second
                                    ? left.second < right.second
                                    : right.first < left.first;
                            })->first;
                        trace << "\nZHANG_E27_USER_REFERENCE time="
                              << kfState.time.to_string(0)
                              << " receiver=" << receiverId
                              << " system=" << enum_to_string(sys)
                              << " old="
                              << (retained.prn > 0 ? retained.id() : "NONE")
                              << " new=" << forcedDualReference.id()
							  << " integer_component_size="
							  << preferredIntegerComponentSize
							  << " integer_component="
							  << (preferredIntegerComponent.empty()
								  ? "NONE" : preferredIntegerComponent)
                              << " exact_integer_transform="
                              << (retained.prn > 0);
                        retained = forcedDualReference;
                    }
                }
            }
            if (acsConfig.pppOpts.ionoOpts.use_if_combo &&
                acsConfig.zhangPppAr.user_use_full_product_covariance &&
                observables.size() == 2)
            {
                const E_ObsCode first = observables[0];
                const E_ObsCode second = observables[1];
                int observedFirst = 0;
                int observedSecond = 0;
                int commonDual = 0;
                int productDual = 0;
                int ifValid = 0;
                for (const auto& obs : only<GObs>(receiver.obsList))
                {
                    if (obs.Sat.sys != sys)
                    {
                        continue;
                    }
                    auto present = [&](E_ObsCode code)
                    {
                        for (const auto& [frequency, signal] : obs.sigs)
                        {
                            if (signal.code == code && signal.P != 0 &&
                                signal.L != 0 && !signal.invalid)
                            {
                                return true;
                            }
                        }
                        return false;
                    };
                    const bool firstPresent = present(first);
                    const bool secondPresent = present(second);
                    observedFirst += firstPresent;
                    observedSecond += secondPresent;
                    string reason = "ACCEPTED";
                    if (!firstPresent)
                    {
                        reason = "NO_L1_OBS";
                    }
                    else if (!secondPresent)
                    {
                        reason = "NO_L2_OBS";
                    }
                    else
                    {
                        commonDual++;
                        if (!signalUsable(obs, first) ||
                            !signalUsable(obs, second))
                        {
                            reason = "ARC_INVALID";
                        }
                        else
                        {
                            ZhangInternalProduct firstProduct;
                            ZhangInternalProduct secondProduct;
                            if (!queryZhangInternalProduct(
                                    kfState.time, obs.Sat, first, firstProduct))
                            {
                                reason = "NO_L1_PRODUCT";
                            }
                            else if (!queryZhangInternalProduct(
                                         kfState.time, obs.Sat, second,
                                         secondProduct))
                            {
                                reason = "NO_L2_PRODUCT";
                            }
                            else
                            {
                                productDual++;
                                vector<double> firstFactors;
                                vector<double> secondFactors;
                                string firstFailure;
                                string secondFailure;
                                if (!queryZhangInternalProductNoiseFactors(
                                        kfState.time, obs.Sat, first, true,
                                        firstFactors, nullptr, &firstFailure) ||
                                    !queryZhangInternalProductNoiseFactors(
                                        kfState.time, obs.Sat, second, true,
                                        secondFactors, nullptr, &secondFailure))
                                {
                                    reason = "COV_INVALID";
                                }
                                else
                                {
                                    ifValid++;
                                }
                            }
                        }
                    }
                    trace << "\nZHANG_E27_COVERAGE_SAT time="
                          << kfState.time.to_string(0)
                          << " receiver=" << receiverId
                          << " system=" << enum_to_string(sys)
                          << " satellite=" << obs.Sat.id()
                          << " reason=" << reason;
                }
                trace << "\nZHANG_E27_COVERAGE time="
                      << kfState.time.to_string(0)
                      << " receiver=" << receiverId
                      << " system=" << enum_to_string(sys)
                      << " obs_l1=" << observedFirst
                      << " obs_l2=" << observedSecond
                      << " common_dual=" << commonDual
                      << " product_dual=" << productDual
                      << " if_valid=" << ifValid
                      << " canonical_sd=" << std::max(0, ifValid - 1);
            }
            for (E_ObsCode code : observables)
            {
                const bool sharedIfCoordinate =
                    usesSharedIfUserCoordinate(sys, code);
                const bool coordinateOwner =
                    !sharedIfCoordinate || code == observables.front();
                map<SatSys, double> candidates;
                map<SatSys, ZhangInternalProduct> products;

                for (const auto& obs : only<GObs>(receiver.obsList))
                {
                    if (obs.Sat.sys != sys || !signalUsable(obs, code))
                    {
                        continue;
                    }

                    ZhangInternalProduct product;
                    if (!queryZhangInternalProduct(kfState.time, obs.Sat, code, product))
                    {
                        continue;
                    }

                    double elevation =
                        obs.satStat_ptr ? obs.satStat_ptr->el : 0;
                    candidates[obs.Sat] = elevation;
                    products[obs.Sat] = product;
                }

                if (candidates.empty())
                {
                    continue;
                }

                UserReferenceKey key{
					runtimeId, receiverId, sys, code};
                auto& runtime = userReferenceMap[key];

                auto bestIt = std::max_element(
                    candidates.begin(),
                    candidates.end(),
                    [](const auto& left, const auto& right)
                    { return left.second < right.second; }
                );
                SatSys selected = bestIt->first;
                if (forcedDualReference.prn > 0 &&
                    candidates.count(forcedDualReference))
                {
                    selected = forcedDualReference;
                }
                if (runtime.reference.prn > 0 &&
                    candidates.find(runtime.reference) != candidates.end() &&
                    forcedDualReference.prn <= 0)
                {
                    selected = runtime.reference;
                }

                const ZhangInternalProduct& selectedProduct =
                    products.at(selected);
                bool productDatumChanged =
                    runtime.productCounter >= 0 &&
                    runtime.reference == selected &&
                    (runtime.productCounter !=
                         selectedProduct.discontinuity_counter ||
                     runtime.datumVersion !=
                         selectedProduct.datum_version);
                if (productDatumChanged)
                {
                    resetUserPhaseBlock(
                        trace,
                        kfState,
                        receiverId,
                        sys,
                        code,
                        "product discontinuity counter changed"
                    );
                    runtime.reference = {};
                }

                if (runtime.reference.prn > 0 &&
                    runtime.reference != selected &&
                    coordinateOwner)
                {
                    if (!transformUserReference(
                            trace,
                            kfState,
                            receiverId,
                            sys,
                            code,
                            runtime.reference,
                            selected
                        ))
                    {
                        resetUserPhaseBlock(
                            trace,
                            kfState,
                            receiverId,
                            sys,
                            code,
                            "ambiguity reference exchange unavailable"
                        );
                    }
                }

                for (const auto& [satellite, product] : products)
                {
                    auto datum = std::make_pair(
                        product.discontinuity_counter,
                        product.datum_version
                    );
                    auto oldDatumIt =
                        runtime.satelliteDatum.find(satellite);
                    bool satelliteDatumChanged =
                        oldDatumIt != runtime.satelliteDatum.end() &&
                        oldDatumIt->second != datum;
                    if (satelliteDatumChanged &&
                        satellite != selected)
                    {
                        resetUserAmbiguity(
                            trace,
                            kfState,
                            receiverId,
                            satellite,
                            code,
                            "satellite product datum changed"
                        );
                    }
                    runtime.satelliteDatum[satellite] = datum;
                }

                if (runtime.reference != selected ||
                    productDatumChanged)
                {
                    trace << "\nZHANG_USER_REFERENCE time=" << kfState.time.to_string(0)
                          << " receiver=" << receiverId
                          << " sys=" << enum_to_string(sys)
                          << " observable=" << enum_to_string(code)
                          << " old="
                          << (runtime.reference.prn > 0
                                  ? runtime.reference.id()
                                  : string("NONE"))
                          << " new=" << selected.id()
                          << " product_counter="
                          << selectedProduct.discontinuity_counter
                          << " datum_version="
                          << selectedProduct.datum_version
                          << " product_datum_changed=" << productDatumChanged;
                }

                runtime.reference = selected;
                runtime.productCounter =
                    selectedProduct.discontinuity_counter;
                runtime.datumVersion =
                    selectedProduct.datum_version;
            }
        }
    }
}

bool zhangPppArUserReferenceAmbiguity(
    const KFState&     kfState,
    const string&      receiver,
    const SatSys&      satellite,
    E_ObsCode          observable
)
{
    if (!acsConfig.zhangPppAr.user_adapter)
    {
        return false;
    }
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(kfState, runtimeId))
	{
		return false;
	}

    UserReferenceKey key{
        runtimeId,
        receiver,
        satellite.sys,
        observable
    };
    auto it = userReferenceMap.find(key);
    return it != userReferenceMap.end() &&
           it->second.reference == satellite;
}

int zhangPppArUserPhaseCoordinateNumber(
    E_Sys system,
    E_ObsCode observable
)
{
    return userPhaseCoordinateNumber(system, observable);
}

double zhangPppArUserPhaseCoordinateWavelength(
    E_Sys system,
    E_ObsCode observable
)
{
    return userPhaseCoordinateWavelength(system, observable);
}

SatSys zhangPppArUserReference(
    const KFState& kfState,
    const string& receiver,
    E_Sys system,
    E_ObsCode observable
)
{
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(kfState, runtimeId))
	{
		return {};
	}
    UserReferenceKey key{runtimeId, receiver, system, observable};
    auto found = userReferenceMap.find(key);
    return found == userReferenceMap.end() ? SatSys{} : found->second.reference;
}

bool zhangPppArUserAmbiguityIntegerValid(
    const KFState&     kfState,
    const string&      receiver,
    const SatSys&      satellite,
    E_ObsCode          observable
)
{
    if (!acsConfig.zhangPppAr.user_adapter)
    {
        return true;
    }
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(kfState, runtimeId))
	{
		return false;
	}

    UserReferenceKey key{
        runtimeId,
        receiver,
        satellite.sys,
        observable
    };
    auto referenceIt = userReferenceMap.find(key);
    if (referenceIt == userReferenceMap.end() ||
        referenceIt->second.reference.prn <= 0)
    {
        return false;
    }

    ZhangInternalProduct satelliteProduct;
    ZhangInternalProduct referenceProduct;
    if (!queryZhangInternalProduct(
            kfState.time,
            satellite,
            observable,
            satelliteProduct
        ) ||
        !queryZhangInternalProduct(
            kfState.time,
            referenceIt->second.reference,
            observable,
            referenceProduct
        ))
    {
        return false;
    }

    // A numerically usable Hybrid product is sufficient for float PPP, but
    // never for ambiguity fixing.  The historical experimental bypass used
    // PPP usability as an AR certificate; that violates the Zhang integer
    // functional requirement and is deliberately ignored here.
    const auto configured =
        acsConfig.zhangPppAr.baseline_observables.find(satellite.sys);
    const bool dualFrequencyRequired =
        configured != acsConfig.zhangPppAr.baseline_observables.end()
        && configured->second.size() == 2;
    return (!dualFrequencyRequired
            || (satelliteProduct.dual_frequency_ar_valid
                && referenceProduct.dual_frequency_ar_valid))
        && satelliteProduct.integer_component_version ==
           referenceProduct.integer_component_version
        && satelliteProduct.integer_alignment_generation ==
           referenceProduct.integer_alignment_generation
        && zhangHybridRelativeIntegerPairCertified(
        satelliteProduct.pppar_usable,
        satelliteProduct.integer_component_id,
        referenceProduct.pppar_usable,
        referenceProduct.integer_component_id);
}

void traceZhangPppArUserDiagnostics(
    Trace&       trace,
    ReceiverMap& receiverMap,
    KFState&     kfState
)
{
    if (!acsConfig.zhangPppAr.user_adapter ||
        !acsConfig.zhangPppAr.output_diagnostics)
    {
        return;
    }
	string runtimeId;
	if (!resolveZhangPppArRuntimeOwner(kfState, runtimeId))
	{
		trace << "\nZHANG_USER_DIAGNOSTICS time="
			  << kfState.time.to_string(0)
			  << " status=REJECTED"
			  << " reason=CHECKPOINT_RUNTIME_ID_UNBOUND";
		return;
	}

    for (auto& [receiverId, receiver] : receiverMap)
    {
        vector<double> fractions;
        int ambiguityCount = 0;
        int integerValidCount = 0;
        int maxCounter = -1;
        int maxDatumVersion = -1;

        for (const auto& [key, index] : kfState.kfIndexMap)
        {
            if (key.type != KF::AMBIGUITY ||
                key.str != receiverId ||
                !zhangPppArUsesObservable(
                    key.Sat.sys,
                    static_cast<E_ObsCode>(key.num)
                ))
            {
                continue;
            }

            ambiguityCount++;
            double value = kfState.x(index);
            fractions.push_back(std::abs(value - std::round(value)));

            E_ObsCode code = static_cast<E_ObsCode>(key.num);
            ZhangInternalProduct product;
            if (queryZhangInternalProduct(
                    kfState.time,
                    key.Sat,
                    code,
                    product
                ))
            {
                maxCounter = std::max(
                    maxCounter,
                    product.discontinuity_counter
                );
                maxDatumVersion = std::max(
                    maxDatumVersion,
                    product.datum_version
                );
            }

            bool integerValid = zhangPppArUserAmbiguityIntegerValid(
                kfState,
                receiverId,
                key.Sat,
                code
            );
            integerValidCount += integerValid;

            UserReferenceKey referenceKey{
                runtimeId,
                receiverId,
                key.Sat.sys,
                code
            };
            auto referenceIt = userReferenceMap.find(referenceKey);
            string reference =
                referenceIt != userReferenceMap.end() &&
                referenceIt->second.reference.prn > 0
                    ? referenceIt->second.reference.id()
                    : "NONE";

            trace << "\nZHANG_USER_AMBIGUITY time=" << kfState.time.to_string(0)
                  << " receiver=" << receiverId
                  << " satellite=" << key.Sat.id()
                  << " observable=" << enum_to_string(code)
                  << " reference=" << reference
                  << " value_cycles=" << value
                  << " rounded_cycles=" << std::llround(value)
                  << " fractional_cycle="
                  << std::abs(value - std::round(value))
                  << " integer_valid=" << integerValid
                  << " product_counter="
                  << (product.satellite.prn > 0
                          ? product.discontinuity_counter
                          : -1)
                  << " datum_version="
                  << (product.satellite.prn > 0
                          ? product.datum_version
                          : -1);
        }

        std::sort(fractions.begin(), fractions.end());
        double median = fractions.empty()
                            ? std::numeric_limits<double>::quiet_NaN()
                            : fractions[fractions.size() / 2];
        auto percentile = [&](double probability)
        {
            return fractions.empty()
                ? std::numeric_limits<double>::quiet_NaN()
                : fractions[static_cast<size_t>(
                      probability * (fractions.size() - 1)
                  )];
        };
        double p68 = percentile(0.68);
        double p90 = fractions.empty()
                         ? std::numeric_limits<double>::quiet_NaN()
                         : fractions[static_cast<size_t>(
                               0.9 * (fractions.size() - 1)
                           )];
        double p95 = percentile(0.95);

        Vector3d estimate = Vector3d::Zero();
        bool positionFound = true;
        for (int axis = 0; axis < 3; axis++)
        {
            KFKey positionKey;
            positionKey.type = KF::REC_POS;
            positionKey.str  = receiverId;
            positionKey.num  = axis;
            if (kfState.getKFValue(positionKey, estimate(axis)) == E_Source::NONE)
            {
                positionFound = false;
            }
        }

        Vector3d errorEnu = Vector3d::Constant(
            std::numeric_limits<double>::quiet_NaN()
        );
        if (positionFound && !receiver.aprioriPos.isZero())
        {
            Matrix3d rotation;
            pos2enu(ecef2pos(receiver.aprioriPos), rotation.data());
            errorEnu = rotation * (estimate - receiver.aprioriPos);
        }

        trace << "\nZHANG_USER_DIAGNOSTIC time=" << kfState.time.to_string(0)
              << " receiver=" << receiverId
              << " ambiguities=" << ambiguityCount
              << " integer_valid_ambiguities=" << integerValidCount
              << " product_counter=" << maxCounter
              << " datum_version=" << maxDatumVersion
              << " median_fractional_cycle=" << median
              << " p68_fractional_cycle=" << p68
              << " p90_fractional_cycle=" << p90
              << " p95_fractional_cycle=" << p95
              << " east_error_m=" << errorEnu(0)
              << " north_error_m=" << errorEnu(1)
              << " up_error_m=" << errorEnu(2);
    }
}
