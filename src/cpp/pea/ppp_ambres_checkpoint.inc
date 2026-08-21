/* E29 Infra-0 checkpoint adapter for ppp_ambres.cpp private runtime.
 *
 * This file is included once, at the end of ppp_ambres.cpp.  It is not a
 * separately compiled translation unit: keeping the adapter here avoids
 * making the AR implementation's physical-lattice/window types public.
 */

namespace
{
constexpr std::size_t ZHANG_AMBRES_MAX_CONTAINER = 200000;
constexpr std::size_t ZHANG_AMBRES_MAX_STRING = 1 << 20;
constexpr int ZHANG_E27_MAX_SATELLITE = 64;
constexpr int ZHANG_E27_MAX_FACTORS = 360;
constexpr double ZHANG_E27_LAG_SECONDS = 3600;

struct AmbresArcCheckpoint
{
    E_ObsCode code = E_ObsCode::NONE;
    string receiver;
    SatSys satellite;
    int version = 0;
    string coefficient;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & code;
        ar & receiver;
        ar & satellite;
        ar & version;
        ar & coefficient;
    }
};

struct AmbresHeldRowCheckpoint
{
    vector<AmbresArcCheckpoint> coefficients;
    string value;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & coefficients;
        ar & value;
    }
};

struct AmbresHeldLatticeCheckpoint
{
    string ownerRuntimeId;
    E_Sys system = E_Sys::NONE;
    vector<AmbresHeldRowCheckpoint> rows;
    int lastEventId = 0;
    bool consistent = true;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & ownerRuntimeId;
        ar & system;
        ar & rows;
        ar & lastEventId;
        ar & consistent;
    }
};

struct AmbresHeldEvidenceCheckpoint
{
    string ownerRuntimeId;
    E_Sys system = E_Sys::NONE;
    AmbresHeldRowCheckpoint row;
    int confirmations = 0;
    long int lastEpoch = 0;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & ownerRuntimeId;
        ar & system;
        ar & row;
        ar & confirmations;
        ar & lastEpoch;
    }
};

struct AmbresExactTermCheckpoint
{
    string column;
    string coefficient;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & column;
        ar & coefficient;
    }
};

struct AmbresSparseExactRowCheckpoint
{
    vector<AmbresExactTermCheckpoint> terms;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & terms;
    }
};

struct AmbresAdmissionCandidateCheckpoint
{
    int certificateKind = 0;
    string relationId;
    string satellite;
    string observable;
    AmbresSparseExactRowCheckpoint physicalCoefficients;
    string integerValue;
    bool exactIntegerEstimable = false;
    bool phaseSegmentCompatible = false;
    bool scalarReliabilityPassed = false;
    bool jointNisPassed = false;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & certificateKind;
        ar & relationId;
        ar & satellite;
        ar & observable;
        ar & physicalCoefficients;
        ar & integerValue;
        ar & exactIntegerEstimable;
        ar & phaseSegmentCompatible;
        ar & scalarReliabilityPassed;
        ar & jointNisPassed;
    }
};

struct AmbresTemporalConfirmationCheckpoint
{
    string relationKey;
    string integerValue;
    int consistentEpochs = 0;
    long int firstEpoch = 0;
    long int lastEpoch = 0;
    vector<string> supportSignatures;
    bool redundancyConfirmed = false;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & relationKey;
        ar & integerValue;
        ar & consistentEpochs;
        ar & firstEpoch;
        ar & lastEpoch;
        ar & supportSignatures;
        ar & redundancyConfirmed;
    }
};

struct AmbresAdmissionStateCheckpoint
{
    string ownerRuntimeId;
    E_Sys system = E_Sys::NONE;
    vector<AmbresSparseExactRowCheckpoint> certifiedRows;
    vector<string> certifiedValues;
    vector<string> certifiedSatellites;
    vector<string> certifiedRelationIds;
    vector<AmbresAdmissionCandidateCheckpoint> pendingCandidates;
    vector<AmbresTemporalConfirmationCheckpoint> temporalConfirmations;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & ownerRuntimeId;
        ar & system;
        ar & certifiedRows;
        ar & certifiedValues;
        ar & certifiedSatellites;
        ar & certifiedRelationIds;
        ar & pendingCandidates;
        ar & temporalConfirmations;
    }
};

struct AmbresHeldUserWideLaneIntegerCheckpoint
{
    SatSys satellite;
    string value;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & satellite;
        ar & value;
    }
};

struct AmbresHeldUserWideLaneCheckpoint
{
    string ownerRuntimeId;
    string receiver;
    E_Sys system = E_Sys::NONE;
    bool ifAcceptance = false;
    SatSys reference;
    vector<AmbresHeldUserWideLaneIntegerCheckpoint> integers;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & ownerRuntimeId;
        ar & receiver;
        ar & system;
        ar & ifAcceptance;
        ar & reference;
        ar & integers;
    }
};

struct AmbresGraphEdgeCheckpoint
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

struct AmbresFunctionalCheckpoint
{
    SatSys satellite;
    SatSys referenceSatellite;
    vector<AmbresGraphEdgeCheckpoint> physicalEdges;
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

struct AmbresTransitionCheckpoint
{
    vector<AmbresGraphEdgeCheckpoint> physicalEdges;
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

struct AmbresPendingTransitionCheckpoint
{
    ZhangCheckpointTime eventTime;
    E_Sys system = E_Sys::NONE;
    SatSys satellite;
    E_ObsCode observable = E_ObsCode::NONE;
    string eventId;
    AmbresFunctionalCheckpoint oldFunctional;
    AmbresFunctionalCheckpoint newFunctional;
    AmbresTransitionCheckpoint transition;
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

struct AmbresActiveTransitionsCheckpoint
{
    string ownerRuntimeId;
    vector<AmbresPendingTransitionCheckpoint> transitions;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & ownerRuntimeId;
        ar & transitions;
    }
};

struct AmbresE27ProductDatumCheckpoint
{
    SatSys satellite;
    int firstDiscontinuity = 0;
    int firstDatumVersion = 0;
    int secondDiscontinuity = 0;
    int secondDatumVersion = 0;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & satellite;
        ar & firstDiscontinuity;
        ar & firstDatumVersion;
        ar & secondDiscontinuity;
        ar & secondDatumVersion;
    }
};

struct AmbresE27LastValidCheckpoint
{
    SatSys satellite;
    ZhangCheckpointTime time;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & satellite;
        ar & time;
    }
};

struct AmbresE27RawFactorCheckpoint
{
    ZhangCheckpointTime time;
    vector<int> satellites;
    VectorXd values;
    MatrixXd covariance;
    vector<string> noiseKeys;
    VectorXd noiseVariances;
    MatrixXd satelliteNoiseDesign;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & time;
        ar & satellites;
        ar & values;
        ar & covariance;
        ar & noiseKeys;
        ar & noiseVariances;
        ar & satelliteNoiseDesign;
    }
};

struct AmbresE27RuntimeCheckpoint
{
    string ownerRuntimeId;
    string receiver;
    E_Sys system = E_Sys::NONE;
    string solution;
    vector<AmbresE27LastValidCheckpoint> lastValid;
    map<SatSys, int> arcVersions;
    vector<AmbresE27ProductDatumCheckpoint> productDatum;
    SatSys reference;
    map<int, int> accumulatorArcVersions;
    vector<AmbresE27RawFactorCheckpoint> factors;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & ownerRuntimeId;
        ar & receiver;
        ar & system;
        ar & solution;
        ar & lastValid;
        ar & arcVersions;
        ar & productDatum;
        ar & reference;
        ar & accumulatorArcVersions;
        ar & factors;
    }
};

struct AmbresRelinkPriorCheckpoint
{
    string ownerRuntimeId;
    E_Sys system = E_Sys::NONE;
    SatSys anchor;
    SatSys satellite;
    ZhangCheckpointTime time;
    E_ObsCode firstCode = E_ObsCode::NONE;
    E_ObsCode secondCode = E_ObsCode::NONE;
    long int productDatumVersion = 0;
    double firstMean = 0;
    double firstVariance = 0;
    double wideLaneMean = 0;
    double wideLaneVariance = 0;
    double firstWideLaneCovariance = 0;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & ownerRuntimeId;
        ar & system;
        ar & anchor;
        ar & satellite;
        ar & time;
        ar & firstCode;
        ar & secondCode;
        ar & productDatumVersion;
        ar & firstMean;
        ar & firstVariance;
        ar & wideLaneMean;
        ar & wideLaneVariance;
        ar & firstWideLaneCovariance;
    }
};

struct AmbresScalarIncrementCheckpoint
{
    ZhangCheckpointTime time;
    double information = 0;
    double natural = 0;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & time;
        ar & information;
        ar & natural;
    }
};

struct AmbresScalarAccumulatorCheckpoint
{
    string ownerRuntimeId;
    string key;
    vector<AmbresScalarIncrementCheckpoint> increments;
    long long integerHypothesis = 0;
    bool initialized = false;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & ownerRuntimeId;
        ar & key;
        ar & increments;
        ar & integerHypothesis;
        ar & initialized;
    }
};

struct AmbresJointIncrementCheckpoint
{
    ZhangCheckpointTime time;
    std::array<double, 4> information{};
    std::array<double, 2> natural{};

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & time;
        ar & information;
        ar & natural;
    }
};

struct AmbresJointAccumulatorCheckpoint
{
    string ownerRuntimeId;
    string key;
    vector<AmbresJointIncrementCheckpoint> increments;
    long long wideLaneHypothesis = 0;
    long long firstHypothesis = 0;
    bool initialized = false;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & ownerRuntimeId;
        ar & key;
        ar & increments;
        ar & wideLaneHypothesis;
        ar & firstHypothesis;
        ar & initialized;
    }
};

struct AmbresWhitenedObservationCheckpoint
{
    ZhangCheckpointTime time;
    double value = 0;
    double variance = 0;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & time;
        ar & value;
        ar & variance;
    }
};

struct AmbresWhitenedAccumulatorCheckpoint
{
    string ownerRuntimeId;
    string key;
    vector<AmbresWhitenedObservationCheckpoint> observations;
    vector<AmbresArcCheckpoint> physicalRow;
    string physicalSegmentIdentity;
    int lastGraphEventId = 0;
    int lastProductDatumVersion = 0;
    bool initialized = false;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & ownerRuntimeId;
        ar & key;
        ar & observations;
        ar & physicalRow;
        ar & physicalSegmentIdentity;
        ar & lastGraphEventId;
        ar & lastProductDatumVersion;
        ar & initialized;
    }
};

struct AmbresCheckpointEnvelope
{
    std::uint32_t schemaVersion = ZHANG_AMBRES_CHECKPOINT_SCHEMA_VERSION;
    string runtimeId;
    bool filterError = false;
    bool transactionalConditioningFailed = false;
    string transactionalConditioningReason;
    bool arVerbose = false;
    double fixedAmbiguityVariance = 0;
    vector<AmbresE27RuntimeCheckpoint> e27Runtimes;
    vector<AmbresHeldLatticeCheckpoint> heldLattices;
    vector<AmbresHeldEvidenceCheckpoint> heldEvidence;
    vector<AmbresAdmissionStateCheckpoint> admissionStates;
    vector<AmbresHeldUserWideLaneCheckpoint> heldUserWideLanes;
    vector<AmbresActiveTransitionsCheckpoint> activeTransitions;
    vector<AmbresRelinkPriorCheckpoint> relinkPriors;
    vector<AmbresScalarAccumulatorCheckpoint> scalarAccumulators;
    vector<AmbresJointAccumulatorCheckpoint> jointAccumulators;
    vector<AmbresWhitenedAccumulatorCheckpoint> whitenedAccumulators;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int)
    {
        ar & schemaVersion;
        ar & runtimeId;
        ar & filterError;
        ar & transactionalConditioningFailed;
        ar & transactionalConditioningReason;
        ar & arVerbose;
        ar & fixedAmbiguityVariance;
        ar & e27Runtimes;
        ar & heldLattices;
        ar & heldEvidence;
        ar & admissionStates;
        ar & heldUserWideLanes;
        ar & activeTransitions;
        ar & relinkPriors;
        ar & scalarAccumulators;
        ar & jointAccumulators;
        ar & whitenedAccumulators;
    }
};

std::mutex zhangAmbresCheckpointMutex;

bool ambresValidString(const string& value, bool allowEmpty = true)
{
    return value.size() <= ZHANG_AMBRES_MAX_STRING &&
        (allowEmpty || !value.empty());
}

bool ambresValidGpsSatellite(const SatSys& satellite, bool allowNone = false)
{
    if (allowNone && satellite.prn == 0)
    {
        return satellite.sys == E_Sys::NONE || satellite.sys == E_Sys::GPS;
    }
    return satellite.sys == E_Sys::GPS && satellite.prn > 0 &&
        satellite.prn <= ZHANG_E27_MAX_SATELLITE;
}

bool ambresValidObservable(E_ObsCode observable)
{
    return observable == E_ObsCode::L1C || observable == E_ObsCode::L2W;
}

bool ambresRestoreFiniteTime(
    const ZhangCheckpointTime& snapshot,
    GTime& value)
{
    value = restoreZhangCheckpointTime(snapshot);
    return std::isfinite(value.bigTime);
}

string ambresExactText(const ZhangExactInteger& value)
{
    return value.str(0, std::ios_base::fmtflags(0));
}

bool ambresParseExact(
    const string& text,
    ZhangExactInteger& value,
    string& reason)
{
    if (text.empty() || text.size() > 4096)
    {
        reason = "AMBRES_CHECKPOINT_EXACT_INTEGER_LENGTH_INVALID";
        return false;
    }
    std::size_t first = text.front() == '-' ? 1 : 0;
    if (first == text.size() ||
        !std::all_of(text.begin() + first, text.end(), [](unsigned char ch)
        {
            return std::isdigit(ch) != 0;
        }))
    {
        reason = "AMBRES_CHECKPOINT_EXACT_INTEGER_TEXT_INVALID";
        return false;
    }
    try
    {
        value = ZhangExactInteger(text);
    }
    catch (const std::exception&)
    {
        reason = "AMBRES_CHECKPOINT_EXACT_INTEGER_PARSE_FAILED";
        return false;
    }
    return true;
}

AmbresArcCheckpoint ambresCheckpointArc(
    const ZhangPhysicalIntegerArc& arc,
    const ZhangExactInteger& coefficient)
{
    return {arc.code, arc.edge.receiver, arc.edge.satellite, arc.version,
        ambresExactText(coefficient)};
}

bool ambresRestoreArc(
    const AmbresArcCheckpoint& snapshot,
    ZhangPhysicalIntegerArc& arc,
    ZhangExactInteger& coefficient,
    string& reason)
{
    if (!ambresValidObservable(snapshot.code) ||
        !ambresValidString(snapshot.receiver, false) ||
        !ambresValidGpsSatellite(snapshot.satellite) || snapshot.version < 0 ||
        !ambresParseExact(snapshot.coefficient, coefficient, reason) ||
        coefficient == 0)
    {
        if (reason.empty())
        {
            reason = "AMBRES_CHECKPOINT_PHYSICAL_ARC_INVALID";
        }
        return false;
    }
    arc = {snapshot.code, {snapshot.receiver, snapshot.satellite},
        snapshot.version};
    return true;
}

AmbresHeldRowCheckpoint ambresCheckpointHeldRow(
    const ZhangPersistentHeldRow& row)
{
    AmbresHeldRowCheckpoint result;
    for (const auto& [arc, coefficient] : row.coefficients)
    {
        result.coefficients.push_back(ambresCheckpointArc(arc, coefficient));
    }
    result.value = ambresExactText(row.value);
    return result;
}

bool ambresRestoreHeldRow(
    const AmbresHeldRowCheckpoint& snapshot,
    ZhangPersistentHeldRow& row,
    string& reason)
{
    if (snapshot.coefficients.empty() ||
        snapshot.coefficients.size() > ZHANG_AMBRES_MAX_CONTAINER ||
        !ambresParseExact(snapshot.value, row.value, reason))
    {
        if (reason.empty())
        {
            reason = "AMBRES_CHECKPOINT_HELD_ROW_INVALID";
        }
        return false;
    }
    for (const auto& item : snapshot.coefficients)
    {
        ZhangPhysicalIntegerArc arc;
        ZhangExactInteger coefficient;
        if (!ambresRestoreArc(item, arc, coefficient, reason) ||
            !row.coefficients.emplace(arc, coefficient).second)
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_HELD_ROW_DUPLICATE_ARC";
            }
            return false;
        }
    }
    return true;
}

AmbresSparseExactRowCheckpoint ambresCheckpointSparseExactRow(
    const map<string, ZhangExactInteger>& row)
{
    AmbresSparseExactRowCheckpoint result;
    for (const auto& [column, coefficient] : row)
    {
        result.terms.push_back({column, ambresExactText(coefficient)});
    }
    return result;
}

bool ambresRestoreSparseExactRow(
    const AmbresSparseExactRowCheckpoint& snapshot,
    map<string, ZhangExactInteger>& row,
    string& reason,
    bool allowEmpty = false)
{
    if ((!allowEmpty && snapshot.terms.empty())
        || snapshot.terms.size() > ZHANG_AMBRES_MAX_CONTAINER)
    {
        reason = "AMBRES_CHECKPOINT_SPARSE_EXACT_ROW_INVALID";
        return false;
    }
    for (const auto& term : snapshot.terms)
    {
        ZhangExactInteger coefficient;
        if (!ambresValidString(term.column, false)
            || !ambresParseExact(term.coefficient, coefficient, reason)
            || coefficient == 0
            || !row.emplace(term.column, coefficient).second)
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_SPARSE_EXACT_TERM_INVALID";
            }
            return false;
        }
    }
    return true;
}

AmbresAdmissionCandidateCheckpoint ambresCheckpointAdmissionCandidate(
    const ZhangProductRelationAdmissionCandidate& candidate)
{
    AmbresAdmissionCandidateCheckpoint result;
    result.certificateKind = static_cast<int>(candidate.certificateKind);
    result.relationId = candidate.relationId;
    result.satellite = candidate.satellite;
    result.observable = candidate.observable;
    result.physicalCoefficients =
        ambresCheckpointSparseExactRow(candidate.physicalCoefficients);
    result.integerValue = ambresExactText(candidate.integerValue);
    result.exactIntegerEstimable = candidate.exactIntegerEstimable;
    result.phaseSegmentCompatible = candidate.phaseSegmentCompatible;
    result.scalarReliabilityPassed = candidate.scalarReliabilityPassed;
    result.jointNisPassed = candidate.jointNisPassed;
    return result;
}

bool ambresRestoreAdmissionCandidate(
    const AmbresAdmissionCandidateCheckpoint& snapshot,
    ZhangProductRelationAdmissionCandidate& candidate,
    string& reason)
{
    if ((snapshot.certificateKind != static_cast<int>(
            TemporalCertificateKind::SELF_GAUGE_SHIFT)
            && snapshot.certificateKind != static_cast<int>(
                TemporalCertificateKind::INTER_SATELLITE_BRIDGE))
        || !ambresValidString(snapshot.relationId, false)
        || !ambresValidString(snapshot.satellite, false)
        || !ambresValidString(snapshot.observable, false)
        || !ambresParseExact(snapshot.integerValue,
            candidate.integerValue, reason)
        || !ambresRestoreSparseExactRow(snapshot.physicalCoefficients,
            candidate.physicalCoefficients, reason))
    {
        if (reason.empty())
        {
            reason = "AMBRES_CHECKPOINT_ADMISSION_CANDIDATE_INVALID";
        }
        return false;
    }
    candidate.certificateKind = static_cast<TemporalCertificateKind>(
        snapshot.certificateKind);
    candidate.relationId = snapshot.relationId;
    candidate.satellite = snapshot.satellite;
    candidate.observable = snapshot.observable;
    candidate.exactIntegerEstimable = snapshot.exactIntegerEstimable;
    candidate.phaseSegmentCompatible = snapshot.phaseSegmentCompatible;
    candidate.scalarReliabilityPassed = snapshot.scalarReliabilityPassed;
    candidate.jointNisPassed = snapshot.jointNisPassed;
    return true;
}

AmbresFunctionalCheckpoint ambresCheckpointFunctional(
    const ZhangProductIntegerFunctional& value)
{
    AmbresFunctionalCheckpoint result;
    result.satellite = value.satellite;
    result.referenceSatellite = value.referenceSatellite;
    for (const auto& edge : value.physicalEdges)
    {
        result.physicalEdges.push_back({edge.receiver, edge.satellite});
    }
    for (const auto& coefficient : value.networkCoefficients)
    {
        result.networkCoefficients.push_back(ambresExactText(coefficient));
    }
    result.physicalArcVersions = value.physicalArcVersions;
    result.affineOffsetCycles = ambresExactText(value.affineOffsetCycles);
    result.temporalBasisVersion = value.temporalBasisVersion;
    result.valid = value.valid;
    result.failureReason = value.failureReason;
    return result;
}

bool ambresRestoreFunctional(
    const AmbresFunctionalCheckpoint& snapshot,
    ZhangProductIntegerFunctional& value,
    string& reason)
{
    if (snapshot.physicalEdges.size() != snapshot.networkCoefficients.size() ||
        snapshot.physicalEdges.size() != snapshot.physicalArcVersions.size() ||
        snapshot.physicalEdges.size() > ZHANG_AMBRES_MAX_CONTAINER ||
        !ambresValidString(snapshot.failureReason))
    {
        reason = "AMBRES_CHECKPOINT_FUNCTIONAL_DIMENSION_INVALID";
        return false;
    }
    ZhangProductIntegerFunctional candidate;
    candidate.satellite = snapshot.satellite;
    candidate.referenceSatellite = snapshot.referenceSatellite;
    if (snapshot.valid &&
        (!ambresValidGpsSatellite(candidate.satellite) ||
         !ambresValidGpsSatellite(candidate.referenceSatellite)))
    {
        reason = "AMBRES_CHECKPOINT_FUNCTIONAL_SATELLITE_INVALID";
        return false;
    }
    for (std::size_t index = 0; index < snapshot.physicalEdges.size(); index++)
    {
        const auto& edge = snapshot.physicalEdges[index];
        ZhangExactInteger coefficient;
        if (!ambresValidString(edge.receiver, false) ||
            !ambresValidGpsSatellite(edge.satellite) ||
            // Version zero is the established representation for a physical
            // product-basis edge that has no registered arc version yet.
            // It must round-trip exactly; only negative versions are invalid.
            snapshot.physicalArcVersions[index] < 0 ||
            !ambresParseExact(
                snapshot.networkCoefficients[index], coefficient, reason))
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_FUNCTIONAL_EDGE_INVALID";
            }
            return false;
        }
        candidate.physicalEdges.push_back({edge.receiver, edge.satellite});
        candidate.networkCoefficients.push_back(coefficient);
    }
    candidate.physicalArcVersions = snapshot.physicalArcVersions;
    if (!ambresParseExact(
            snapshot.affineOffsetCycles, candidate.affineOffsetCycles, reason))
    {
        return false;
    }
    candidate.temporalBasisVersion = snapshot.temporalBasisVersion;
    candidate.valid = snapshot.valid;
    candidate.failureReason = snapshot.failureReason;
    value = std::move(candidate);
    return true;
}

AmbresTransitionCheckpoint ambresCheckpointTransition(
    const ZhangProductIntegerTransition& value)
{
    AmbresTransitionCheckpoint result;
    for (const auto& edge : value.physicalEdges)
    {
        result.physicalEdges.push_back({edge.receiver, edge.satellite});
    }
    result.physicalArcVersions = value.physicalArcVersions;
    for (const auto& coefficient : value.coefficients)
    {
        result.coefficients.push_back(ambresExactText(coefficient));
    }
    result.affineOffsetCycles = ambresExactText(value.affineOffsetCycles);
    result.valid = value.valid;
    result.failureReason = value.failureReason;
    return result;
}

bool ambresRestoreTransition(
    const AmbresTransitionCheckpoint& snapshot,
    ZhangProductIntegerTransition& value,
    string& reason)
{
    if (snapshot.physicalEdges.size() != snapshot.coefficients.size() ||
        snapshot.physicalEdges.size() != snapshot.physicalArcVersions.size() ||
        snapshot.physicalEdges.size() > ZHANG_AMBRES_MAX_CONTAINER ||
        !ambresValidString(snapshot.failureReason))
    {
        reason = "AMBRES_CHECKPOINT_TRANSITION_DIMENSION_INVALID";
        return false;
    }
    ZhangProductIntegerTransition candidate;
    for (std::size_t index = 0; index < snapshot.physicalEdges.size(); index++)
    {
        const auto& edge = snapshot.physicalEdges[index];
        ZhangExactInteger coefficient;
        if (!ambresValidString(edge.receiver, false) ||
            !ambresValidGpsSatellite(edge.satellite) ||
            snapshot.physicalArcVersions[index] < 0 ||
            !ambresParseExact(snapshot.coefficients[index], coefficient, reason) ||
            coefficient == 0)
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_TRANSITION_EDGE_INVALID";
            }
            return false;
        }
        candidate.physicalEdges.push_back({edge.receiver, edge.satellite});
        candidate.coefficients.push_back(coefficient);
    }
    candidate.physicalArcVersions = snapshot.physicalArcVersions;
    if (!ambresParseExact(
            snapshot.affineOffsetCycles, candidate.affineOffsetCycles, reason))
    {
        return false;
    }
    candidate.valid = snapshot.valid;
    candidate.failureReason = snapshot.failureReason;
    value = std::move(candidate);
    return true;
}

AmbresPendingTransitionCheckpoint ambresCheckpointPending(
    const ZhangPendingProductTransition& value)
{
    AmbresPendingTransitionCheckpoint result;
    result.eventTime = captureZhangCheckpointTime(value.eventTime);
    result.system = value.system;
    result.satellite = value.satellite;
    result.observable = value.observable;
    result.eventId = value.eventId;
    result.oldFunctional = ambresCheckpointFunctional(value.oldFunctional);
    result.newFunctional = ambresCheckpointFunctional(value.newFunctional);
    result.transition = ambresCheckpointTransition(value.transition);
    result.oldIdentity = value.oldIdentity;
    result.newIdentity = value.newIdentity;
    result.oldSBasisFingerprint = value.oldSBasisFingerprint;
    result.newSBasisFingerprint = value.newSBasisFingerprint;
    result.oldPhaseSegmentIdentity = value.oldPhaseSegmentIdentity;
    result.newPhaseSegmentIdentity = value.newPhaseSegmentIdentity;
    result.phaseSegmentChanged = value.phaseSegmentChanged;
    result.eventCause = value.eventCause;
    result.oldProductSegment = value.oldProductSegment;
    result.newProductSegment = value.newProductSegment;
    result.oldSnapshotIdentity = value.oldSnapshotIdentity;
    result.newSnapshotIdentity = value.newSnapshotIdentity;
    result.exactTransformChainId = value.exactTransformChainId;
    result.oldSnapshotReferenceCount = value.oldSnapshotReferenceCount;
    result.newSnapshotReferenceCount = value.newSnapshotReferenceCount;
    result.expiryTime = captureZhangCheckpointTime(value.expiryTime);
    return result;
}

bool ambresRestorePending(
    const AmbresPendingTransitionCheckpoint& snapshot,
    ZhangPendingProductTransition& value,
    string& reason)
{
    ZhangPendingProductTransition candidate;
    if (snapshot.system != E_Sys::GPS ||
        !ambresValidGpsSatellite(snapshot.satellite) ||
        !ambresValidObservable(snapshot.observable) ||
        !ambresValidString(snapshot.eventId, false) ||
        snapshot.oldSnapshotReferenceCount < 0 ||
        snapshot.newSnapshotReferenceCount < 0 ||
        !ambresRestoreFiniteTime(snapshot.eventTime, candidate.eventTime) ||
        !ambresRestoreFiniteTime(snapshot.expiryTime, candidate.expiryTime) ||
        candidate.expiryTime < candidate.eventTime)
    {
        reason = "AMBRES_CHECKPOINT_PENDING_TRANSITION_INVALID";
        return false;
    }
    candidate.system = snapshot.system;
    candidate.satellite = snapshot.satellite;
    candidate.observable = snapshot.observable;
    candidate.eventId = snapshot.eventId;
    if (!ambresRestoreFunctional(
            snapshot.oldFunctional, candidate.oldFunctional, reason) ||
        !ambresRestoreFunctional(
            snapshot.newFunctional, candidate.newFunctional, reason) ||
        !ambresRestoreTransition(
            snapshot.transition, candidate.transition, reason))
    {
        return false;
    }
    if (!candidate.oldFunctional.valid || !candidate.newFunctional.valid ||
        !candidate.transition.valid ||
        candidate.oldFunctional.satellite != candidate.satellite ||
        candidate.newFunctional.satellite != candidate.satellite)
    {
        reason = "AMBRES_CHECKPOINT_PENDING_FUNCTIONAL_REFERENCE_INVALID";
        return false;
    }
    const std::array<const string*, 11> texts = {
        &snapshot.oldIdentity, &snapshot.newIdentity,
        &snapshot.oldSBasisFingerprint, &snapshot.newSBasisFingerprint,
        &snapshot.oldPhaseSegmentIdentity, &snapshot.newPhaseSegmentIdentity,
        &snapshot.eventCause, &snapshot.oldSnapshotIdentity,
        &snapshot.newSnapshotIdentity, &snapshot.exactTransformChainId,
        &snapshot.transition.failureReason};
    if (!std::all_of(texts.begin(), texts.end(), [](const string* text)
        {
            return ambresValidString(*text);
        }))
    {
        reason = "AMBRES_CHECKPOINT_PENDING_TRANSITION_STRING_INVALID";
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
    value = std::move(candidate);
    return true;
}

struct AmbresRestoredRuntime
{
    map<ZhangE27WideLaneKey, ZhangE27WideLaneRuntime> e27Runtimes;
    map<pair<string, E_Sys>, ZhangPersistentHeldLattice> heldLattices;
    map<
        pair<string, E_Sys>,
        map<ZhangPersistentHeldRow, ZhangPersistentHeldEvidence>> heldEvidence;
    map<pair<string, E_Sys>, ZhangProductRelationAdmissionState>
        admissionStates;
    map<ZhangHeldUserWideLaneKey, ZhangHeldUserWideLane>
        heldUserWideLanes;
    map<string, vector<ZhangPendingProductTransition>> activeTransitions;
    map<string, map<ZhangRelinkMomentKey, ZhangRelinkPriorMoment>> relinkPriors;
    map<string, map<string, ZhangRelinkShadowAccumulator>> scalarAccumulators;
    map<string, map<string, ZhangRelinkJointAccumulator>> jointAccumulators;
    map<string, map<string, ZhangWhitenedWlAccumulator>> whitenedAccumulators;
};

bool ambresRestoreEnvelope(
    const AmbresCheckpointEnvelope& snapshot,
    const string& rootRuntimeId,
    AmbresRestoredRuntime& restored,
    ZhangAmbresCheckpointResult& result);

void ambresPrepareCommittedMaps(
    const string& runtimeId,
    const AmbresRestoredRuntime& restored,
    AmbresRestoredRuntime& committed);

bool ambresHasForeignRuntime(const string& rootRuntimeId)
{
    for (const auto& [key, ignored] : zhangE27WideLaneRuntimes)
    {
        if (!zhangAmbresRuntimeBelongsTo(key.runtimeId, rootRuntimeId))
        {
            return true;
        }
    }
    const auto nestedForeign = [&](const auto& values, auto runtimeIdOf)
    {
        return std::any_of(values.begin(), values.end(), [&](const auto& item)
        {
            return !zhangAmbresRuntimeBelongsTo(
                runtimeIdOf(item.first), rootRuntimeId);
        });
    };
    return nestedForeign(zhangPersistentHeldLattices,
               [](const auto& key) -> const string& { return key.first; }) ||
        nestedForeign(zhangPersistentHeldEvidence,
               [](const auto& key) -> const string& { return key.first; }) ||
        nestedForeign(zhangProductRelationAdmissionStateRegistry(),
               [](const auto& key) -> const string& { return key.first; }) ||
        nestedForeign(zhangHeldUserWideLaneRegistry,
               [](const auto& key) -> const string&
               { return std::get<0>(key); }) ||
        nestedForeign(zhangActiveTemporalBesdTransitions,
               [](const auto& key) -> const string& { return key; }) ||
        nestedForeign(zhangRelinkPriorMoments,
               [](const auto& key) -> const string& { return key; }) ||
        nestedForeign(zhangRelinkShadowAccumulators,
               [](const auto& key) -> const string& { return key; }) ||
        nestedForeign(zhangRelinkJointAccumulators,
               [](const auto& key) -> const string& { return key; }) ||
        nestedForeign(zhangWhitenedWlAccumulators,
               [](const auto& key) -> const string& { return key; });
}

} // namespace

ZhangAmbresCheckpointResult exportZhangAmbresCheckpointSection(
    const KFState& owner,
    const string& runtimeId,
    string& payload)
{
    std::lock_guard<std::mutex> lock(zhangAmbresCheckpointMutex);
    ZhangAmbresCheckpointResult result;
    payload.clear();
    if (!validZhangAmbresRuntimeId(runtimeId))
    {
        result.failureReason = "AMBRES_CHECKPOINT_RUNTIME_ID_INVALID";
        return result;
    }
    if (zhangAmbresRuntimeId(owner) != runtimeId)
    {
        result.failureReason = "AMBRES_CHECKPOINT_OWNER_RUNTIME_ID_MISMATCH";
        return result;
    }
    if (ambresHasForeignRuntime(runtimeId))
    {
        result.failureReason = "AMBRES_CHECKPOINT_RUNTIME_OWNER_CONFLICT";
        return result;
    }

    AmbresCheckpointEnvelope snapshot;
    snapshot.runtimeId = runtimeId;
    snapshot.filterError = ::filterError;
    snapshot.transactionalConditioningFailed =
        zhangTransactionalConditioningFailed;
    snapshot.transactionalConditioningReason =
        zhangTransactionalConditioningReason;
    snapshot.arVerbose = AR_VERBO;
    snapshot.fixedAmbiguityVariance = FIXED_AMB_VAR;

    for (const auto& [key, runtime] : zhangE27WideLaneRuntimes)
    {
        if (!zhangAmbresRuntimeBelongsTo(key.runtimeId, runtimeId))
        {
            continue;
        }
        if (runtime.accumulator.factorCount() != runtime.rawFactors.size())
        {
            result.failureReason =
                "AMBRES_CHECKPOINT_E27_LIVE_FACTOR_LEDGER_MISMATCH";
            return result;
        }
        AmbresE27RuntimeCheckpoint item;
        item.ownerRuntimeId = key.runtimeId;
        item.receiver = key.receiver;
        item.system = key.system;
        item.solution = key.solution;
        for (const auto& [satellite, time] : runtime.lastValid)
        {
            item.lastValid.push_back({
                satellite, captureZhangCheckpointTime(time)});
        }
        item.arcVersions = runtime.arcVersion;
        for (const auto& [satellite, datum] : runtime.productDatum)
        {
            item.productDatum.push_back({
                satellite, std::get<0>(datum), std::get<1>(datum),
                std::get<2>(datum), std::get<3>(datum)});
        }
        item.reference = runtime.reference;
        item.accumulatorArcVersions = runtime.accumulatorArcVersions;
        for (const auto& factor : runtime.rawFactors)
        {
            item.factors.push_back({
                captureZhangCheckpointTime(factor.time), factor.satellites,
                factor.values, factor.covariance, factor.noiseKeys,
                factor.noiseVariances, factor.satelliteNoiseDesign});
        }
        snapshot.e27Runtimes.push_back(std::move(item));
    }

    for (const auto& [identity, lattice] : zhangPersistentHeldLattices)
    {
        if (!zhangAmbresRuntimeBelongsTo(identity.first, runtimeId))
        {
            continue;
        }
        AmbresHeldLatticeCheckpoint item;
        item.ownerRuntimeId = identity.first;
        item.system = identity.second;
        item.lastEventId = lattice.lastEventId;
        item.consistent = lattice.consistent;
        for (const auto& row : lattice.rows)
        {
            item.rows.push_back(ambresCheckpointHeldRow(row));
        }
        snapshot.heldLattices.push_back(std::move(item));
    }

    for (const auto& [identity, rows] : zhangPersistentHeldEvidence)
    {
        if (!zhangAmbresRuntimeBelongsTo(identity.first, runtimeId))
        {
            continue;
        }
        for (const auto& [row, evidence] : rows)
        {
            snapshot.heldEvidence.push_back({
                identity.first, identity.second,
                ambresCheckpointHeldRow(row), evidence.confirmations,
                evidence.lastEpoch});
        }
    }

    for (const auto& [identity, state] :
         zhangProductRelationAdmissionStateRegistry())
    {
        if (!zhangAmbresRuntimeBelongsTo(identity.first, runtimeId))
        {
            continue;
        }
        string stateReason;
        if (!ProductRelationAdmission::validateState(state, &stateReason))
        {
            result.failureReason =
                "AMBRES_CHECKPOINT_ADMISSION_STATE_INVALID:" + stateReason;
            return result;
        }
        AmbresAdmissionStateCheckpoint item;
        item.ownerRuntimeId = identity.first;
        item.system = identity.second;
        for (const auto& row : state.certifiedRows)
        {
            item.certifiedRows.push_back(
                ambresCheckpointSparseExactRow(row));
        }
        for (const auto& value : state.certifiedValues)
        {
            item.certifiedValues.push_back(ambresExactText(value));
        }
        item.certifiedSatellites.assign(
            state.certifiedSatellites.begin(),
            state.certifiedSatellites.end());
        item.certifiedRelationIds.assign(
            state.certifiedRelationIds.begin(),
            state.certifiedRelationIds.end());
        for (const auto& [relationId, candidate] : state.pendingCandidates)
        {
            item.pendingCandidates.push_back(
                ambresCheckpointAdmissionCandidate(candidate));
        }
        for (const auto& [relationKey, confirmation] :
             state.temporalCertificateConfirmations)
        {
            AmbresTemporalConfirmationCheckpoint saved;
            saved.relationKey = relationKey;
            saved.integerValue = ambresExactText(confirmation.integerValue);
            saved.consistentEpochs = confirmation.consistentEpochs;
            saved.firstEpoch = confirmation.firstEpoch;
            saved.lastEpoch = confirmation.lastEpoch;
            saved.supportSignatures.assign(
                confirmation.supportSignatures.begin(),
                confirmation.supportSignatures.end());
            saved.redundancyConfirmed = confirmation.redundancyConfirmed;
            item.temporalConfirmations.push_back(std::move(saved));
        }
        snapshot.admissionStates.push_back(std::move(item));
    }

    for (const auto& [identity, held] : zhangHeldUserWideLaneRegistry)
    {
        const auto& [ownerRuntimeId, receiver, system, ifAcceptance] =
            identity;
        if (!zhangAmbresRuntimeBelongsTo(ownerRuntimeId, runtimeId))
        {
            continue;
        }
        if (held.integers.empty())
        {
            continue;
        }
        AmbresHeldUserWideLaneCheckpoint item;
        item.ownerRuntimeId = ownerRuntimeId;
        item.receiver = receiver;
        item.system = system;
        item.ifAcceptance = ifAcceptance;
        item.reference = held.reference;
        for (const auto& [satellite, value] : held.integers)
        {
            item.integers.push_back({satellite, ambresExactText(value)});
        }
        snapshot.heldUserWideLanes.push_back(std::move(item));
    }

    for (const auto& [ownerRuntimeId, transitions] :
         zhangActiveTemporalBesdTransitions)
    {
        if (!zhangAmbresRuntimeBelongsTo(ownerRuntimeId, runtimeId))
        {
            continue;
        }
        AmbresActiveTransitionsCheckpoint item;
        item.ownerRuntimeId = ownerRuntimeId;
        for (const auto& transition : transitions)
        {
            item.transitions.push_back(ambresCheckpointPending(transition));
        }
        snapshot.activeTransitions.push_back(std::move(item));
    }

    for (const auto& [ownerRuntimeId, moments] : zhangRelinkPriorMoments)
    {
        if (!zhangAmbresRuntimeBelongsTo(ownerRuntimeId, runtimeId))
        {
            continue;
        }
        for (const auto& [key, moment] : moments)
        {
            snapshot.relinkPriors.push_back({
                ownerRuntimeId, key.system, key.anchor, key.satellite,
                captureZhangCheckpointTime(moment.time), moment.firstCode,
                moment.secondCode, moment.productDatumVersion,
                moment.firstMean, moment.firstVariance, moment.wideLaneMean,
                moment.wideLaneVariance, moment.firstWideLaneCovariance});
        }
    }

    for (const auto& [ownerRuntimeId, accumulators] :
         zhangRelinkShadowAccumulators)
    {
        if (!zhangAmbresRuntimeBelongsTo(ownerRuntimeId, runtimeId))
        {
            continue;
        }
        for (const auto& [key, accumulator] : accumulators)
        {
            AmbresScalarAccumulatorCheckpoint item;
            item.ownerRuntimeId = ownerRuntimeId;
            item.key = key;
            item.integerHypothesis = accumulator.integerHypothesis;
            item.initialized = accumulator.initialized;
            for (const auto& increment : accumulator.increments)
            {
                item.increments.push_back({
                    captureZhangCheckpointTime(increment.time),
                    increment.information, increment.natural});
            }
            snapshot.scalarAccumulators.push_back(std::move(item));
        }
    }

    for (const auto& [ownerRuntimeId, accumulators] :
         zhangRelinkJointAccumulators)
    {
        if (!zhangAmbresRuntimeBelongsTo(ownerRuntimeId, runtimeId))
        {
            continue;
        }
        for (const auto& [key, accumulator] : accumulators)
        {
            AmbresJointAccumulatorCheckpoint item;
            item.ownerRuntimeId = ownerRuntimeId;
            item.key = key;
            item.wideLaneHypothesis = accumulator.wideLaneHypothesis;
            item.firstHypothesis = accumulator.firstHypothesis;
            item.initialized = accumulator.initialized;
            for (const auto& increment : accumulator.increments)
            {
                AmbresJointIncrementCheckpoint saved;
                saved.time = captureZhangCheckpointTime(increment.time);
                saved.information = {
                    increment.information(0, 0), increment.information(0, 1),
                    increment.information(1, 0), increment.information(1, 1)};
                saved.natural = {
                    increment.natural(0), increment.natural(1)};
                item.increments.push_back(std::move(saved));
            }
            snapshot.jointAccumulators.push_back(std::move(item));
        }
    }

    for (const auto& [ownerRuntimeId, accumulators] :
         zhangWhitenedWlAccumulators)
    {
        if (!zhangAmbresRuntimeBelongsTo(ownerRuntimeId, runtimeId))
        {
            continue;
        }
        for (const auto& [key, accumulator] : accumulators)
        {
            AmbresWhitenedAccumulatorCheckpoint item;
            item.ownerRuntimeId = ownerRuntimeId;
            item.key = key;
            item.physicalSegmentIdentity =
                accumulator.physicalSegmentIdentity;
            item.lastGraphEventId = accumulator.lastGraphEventId;
            item.lastProductDatumVersion =
                accumulator.lastProductDatumVersion;
            item.initialized = accumulator.initialized;
            for (const auto& observation : accumulator.observations)
            {
                item.observations.push_back({
                    captureZhangCheckpointTime(observation.time),
                    observation.value, observation.variance});
            }
            for (const auto& [arc, coefficient] : accumulator.physicalRow)
            {
                item.physicalRow.push_back(
                    ambresCheckpointArc(arc, coefficient));
            }
            snapshot.whitenedAccumulators.push_back(std::move(item));
        }
    }

    AmbresRestoredRuntime preflight;
    ZhangAmbresCheckpointResult preflightResult;
    if (!ambresRestoreEnvelope(
            snapshot, runtimeId, preflight, preflightResult))
    {
        return preflightResult;
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
        result.failureReason =
            string("AMBRES_CHECKPOINT_SERIALIZE_FAILED:") + exception.what();
        payload.clear();
        return result;
    }
    if (payload.empty())
    {
        result.failureReason = "AMBRES_CHECKPOINT_EMPTY_PAYLOAD";
        return result;
    }
    preflightResult.valid = true;
    return preflightResult;
}

ZhangAmbresCheckpointResult importZhangAmbresCheckpointSection(
    KFState& owner,
    const string& runtimeId,
    const string& payload,
    bool validateOnly)
{
    std::lock_guard<std::mutex> lock(zhangAmbresCheckpointMutex);
    ZhangAmbresCheckpointResult result;
    if (!validZhangAmbresRuntimeId(runtimeId))
    {
        result.failureReason = "AMBRES_CHECKPOINT_RUNTIME_ID_INVALID";
        return result;
    }
    if (payload.empty())
    {
        result.failureReason = "AMBRES_CHECKPOINT_EMPTY_PAYLOAD";
        return result;
    }
    if (zhangAmbresRuntimeId(owner) != runtimeId)
    {
        result.failureReason = "AMBRES_CHECKPOINT_OWNER_RUNTIME_ID_MISMATCH";
        return result;
    }
    if (ambresHasForeignRuntime(runtimeId))
    {
        result.failureReason = "AMBRES_CHECKPOINT_RUNTIME_OWNER_CONFLICT";
        return result;
    }

    AmbresCheckpointEnvelope snapshot;
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
            result.failureReason = "AMBRES_CHECKPOINT_TRAILING_BYTES";
            return result;
        }
    }
    catch (const std::exception& exception)
    {
        result.failureReason =
            string("AMBRES_CHECKPOINT_DESERIALIZE_FAILED:") + exception.what();
        return result;
    }

    AmbresRestoredRuntime restored;
    if (!ambresRestoreEnvelope(snapshot, runtimeId, restored, result))
    {
        return result;
    }
    if (validateOnly)
    {
        result.valid = true;
        return result;
    }

    AmbresRestoredRuntime committed;
    string committedConditioningReason;
    try
    {
        ambresPrepareCommittedMaps(runtimeId, restored, committed);
        committedConditioningReason =
            snapshot.transactionalConditioningReason;
    }
    catch (const std::exception& exception)
    {
        result.failureReason =
            string("AMBRES_CHECKPOINT_COMMIT_PREPARE_FAILED:") +
            exception.what();
        return result;
    }

    zhangE27WideLaneRuntimes.swap(committed.e27Runtimes);
    zhangPersistentHeldLattices.swap(committed.heldLattices);
    zhangPersistentHeldEvidence.swap(committed.heldEvidence);
    zhangProductRelationAdmissionStateRegistry().swap(
        committed.admissionStates);
    zhangHeldUserWideLaneRegistry.swap(committed.heldUserWideLanes);
    zhangActiveTemporalBesdTransitions.swap(committed.activeTransitions);
    zhangRelinkPriorMoments.swap(committed.relinkPriors);
    zhangRelinkShadowAccumulators.swap(committed.scalarAccumulators);
    zhangRelinkJointAccumulators.swap(committed.jointAccumulators);
    zhangWhitenedWlAccumulators.swap(committed.whitenedAccumulators);
    ::filterError = snapshot.filterError;
    zhangTransactionalConditioningFailed =
        snapshot.transactionalConditioningFailed;
    zhangTransactionalConditioningReason.swap(
        committedConditioningReason);
    AR_VERBO = snapshot.arVerbose;
    FIXED_AMB_VAR = snapshot.fixedAmbiguityVariance;
    result.valid = true;
    return result;
}

ZhangAmbresCheckpointResult inspectZhangAmbresCheckpointSnapshotReferences(
    const string& runtimeId,
    const string& payload,
    ZhangCheckpointSnapshotReferenceSummary& summary)
{
    std::lock_guard<std::mutex> lock(zhangAmbresCheckpointMutex);
    summary = {};
    summary.sectionName = ZHANG_AMBRES_CHECKPOINT_SECTION_NAME;
    summary.runtimeId = runtimeId;
    ZhangAmbresCheckpointResult result;
    auto fail = [&](const string& reason)
    {
        summary.valid = false;
        summary.failureReason = reason;
        result.valid = false;
        result.failureReason = reason;
        return result;
    };
    if (!validZhangAmbresRuntimeId(runtimeId))
    {
        return fail("AMBRES_CHECKPOINT_RUNTIME_ID_INVALID");
    }
    if (payload.empty())
    {
        return fail("AMBRES_CHECKPOINT_EMPTY_PAYLOAD");
    }

    AmbresCheckpointEnvelope snapshot;
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
            return fail("AMBRES_CHECKPOINT_TRAILING_BYTES");
        }
    }
    catch (const std::exception& exception)
    {
        return fail(
            string("AMBRES_CHECKPOINT_DESERIALIZE_FAILED:") +
            exception.what());
    }

    if (snapshot.schemaVersion != ZHANG_AMBRES_CHECKPOINT_SCHEMA_VERSION)
    {
        return fail("AMBRES_CHECKPOINT_SCHEMA_MISMATCH");
    }
    if (snapshot.runtimeId != runtimeId)
    {
        return fail("AMBRES_CHECKPOINT_RUNTIME_ID_MISMATCH");
    }
    if (snapshot.activeTransitions.size() > ZHANG_AMBRES_MAX_CONTAINER)
    {
        return fail("AMBRES_CHECKPOINT_ACTIVE_TRANSITION_CONTAINER_INVALID");
    }
    auto accountReference = [&](const string& identity, int declaredCount)
    {
        if (identity.empty())
        {
            return declaredCount == 0;
        }
        if (declaredCount <= 0)
        {
            return false;
        }
        auto& actual = summary.actualReferenceCounts[identity];
        if (actual == std::numeric_limits<std::size_t>::max())
        {
            return false;
        }
        actual++;
        // AMBRES computes this diagnostic within each branch.  Different
        // branch owners may legitimately report different positive counts
        // for the same physical snapshot, so bundle integrity is based on
        // the recomputed endpoint inventory, not this local diagnostic.
        if (declaredCount > 0)
        {
            summary.declaredReferenceCounts.emplace(
                identity, static_cast<std::size_t>(declaredCount));
        }
        return true;
    };
    set<string> ownerRuntimeIds;
    for (const auto& active : snapshot.activeTransitions)
    {
        if (!validZhangAmbresRuntimeId(active.ownerRuntimeId) ||
            !zhangAmbresRuntimeBelongsTo(active.ownerRuntimeId, runtimeId) ||
            active.transitions.size() > ZHANG_AMBRES_MAX_CONTAINER ||
            !ownerRuntimeIds.insert(active.ownerRuntimeId).second)
        {
            return fail("AMBRES_CHECKPOINT_ACTIVE_TRANSITION_HEADER_INVALID");
        }
        for (const auto& transition : active.transitions)
        {
            if (!accountReference(
                    transition.oldSnapshotIdentity,
                    transition.oldSnapshotReferenceCount) ||
                !accountReference(
                    transition.newSnapshotIdentity,
                    transition.newSnapshotReferenceCount))
            {
                return fail(
                    "AMBRES_CHECKPOINT_ACTIVE_SNAPSHOT_REFERENCE_INVALID");
            }
            summary.transitionCount++;
        }
    }
    summary.valid = true;
    summary.failureReason.clear();
    result.valid = true;
    return result;
}

namespace
{
bool ambresValidOwner(
    const string& ownerRuntimeId,
    const string& rootRuntimeId,
    string& reason)
{
    if (!validZhangAmbresRuntimeId(ownerRuntimeId) ||
        !zhangAmbresRuntimeBelongsTo(ownerRuntimeId, rootRuntimeId))
    {
        reason = "AMBRES_CHECKPOINT_OWNER_RUNTIME_ID_INVALID";
        return false;
    }
    return true;
}

bool ambresValidateFactor(
    const AmbresE27RawFactorCheckpoint& snapshot,
    ZhangE27WideLaneRawFactor& factor,
    string& reason)
{
    if (!ambresRestoreFiniteTime(snapshot.time, factor.time) ||
        snapshot.satellites.size() < 2 ||
        snapshot.satellites.size() > ZHANG_E27_MAX_SATELLITE ||
        snapshot.values.size() != static_cast<int>(snapshot.satellites.size()) ||
        snapshot.covariance.rows() != snapshot.values.size() ||
        snapshot.covariance.cols() != snapshot.values.size() ||
        snapshot.noiseKeys.size() !=
            static_cast<std::size_t>(snapshot.noiseVariances.size()) ||
        snapshot.satelliteNoiseDesign.rows() != snapshot.values.size() ||
        snapshot.satelliteNoiseDesign.cols() != snapshot.noiseVariances.size() ||
        !snapshot.values.allFinite() || !snapshot.covariance.allFinite() ||
        !snapshot.noiseVariances.allFinite() ||
        !snapshot.satelliteNoiseDesign.allFinite() ||
        (snapshot.noiseVariances.array() <= 0).any())
    {
        reason = "AMBRES_CHECKPOINT_E27_FACTOR_DIMENSION_OR_VALUE_INVALID";
        return false;
    }
    set<int> satellites;
    for (int satellite : snapshot.satellites)
    {
        if (satellite <= 0 || satellite > ZHANG_E27_MAX_SATELLITE ||
            !satellites.insert(satellite).second)
        {
            reason = "AMBRES_CHECKPOINT_E27_FACTOR_SATELLITE_INVALID";
            return false;
        }
    }
    set<string> noiseKeys;
    for (const string& key : snapshot.noiseKeys)
    {
        if (!ambresValidString(key, false) || !noiseKeys.insert(key).second)
        {
            reason = "AMBRES_CHECKPOINT_E27_FACTOR_NOISE_KEY_INVALID";
            return false;
        }
    }
    const MatrixXd symmetric =
        0.5 * (snapshot.covariance + snapshot.covariance.transpose());
    const double scale = std::max(1.0, symmetric.cwiseAbs().maxCoeff());
    if ((snapshot.covariance - snapshot.covariance.transpose()).norm() >
        1e-10 * scale)
    {
        reason = "AMBRES_CHECKPOINT_E27_FACTOR_COVARIANCE_ASYMMETRIC";
        return false;
    }
    Eigen::SelfAdjointEigenSolver<MatrixXd> solver(symmetric);
    if (solver.info() != Eigen::Success ||
        !solver.eigenvalues().allFinite() ||
        solver.eigenvalues().minCoeff() < -1e-10 * scale)
    {
        reason = "AMBRES_CHECKPOINT_E27_FACTOR_COVARIANCE_NOT_PSD";
        return false;
    }
    factor.time = restoreZhangCheckpointTime(snapshot.time);
    factor.satellites = snapshot.satellites;
    factor.values = snapshot.values;
    factor.covariance = snapshot.covariance;
    factor.noiseKeys = snapshot.noiseKeys;
    factor.noiseVariances = snapshot.noiseVariances;
    factor.satelliteNoiseDesign = snapshot.satelliteNoiseDesign;
    return true;
}

bool ambresRestoreE27Runtime(
    const AmbresE27RuntimeCheckpoint& snapshot,
    const string& rootRuntimeId,
    ZhangE27WideLaneKey& key,
    ZhangE27WideLaneRuntime& runtime,
    string& reason)
{
    if (!ambresValidOwner(snapshot.ownerRuntimeId, rootRuntimeId, reason) ||
        !ambresValidString(snapshot.receiver, false) ||
        !ambresValidString(snapshot.solution, false) ||
        snapshot.system != E_Sys::GPS ||
        snapshot.lastValid.size() > ZHANG_AMBRES_MAX_CONTAINER ||
        snapshot.productDatum.size() > ZHANG_AMBRES_MAX_CONTAINER ||
        snapshot.factors.size() > ZHANG_E27_MAX_FACTORS ||
        !ambresValidGpsSatellite(snapshot.reference, true))
    {
        if (reason.empty())
        {
            reason = "AMBRES_CHECKPOINT_E27_RUNTIME_HEADER_INVALID";
        }
        return false;
    }
    key = {snapshot.ownerRuntimeId, snapshot.receiver, snapshot.system,
        snapshot.solution};
    for (const auto& item : snapshot.lastValid)
    {
        GTime time;
        if (!ambresValidGpsSatellite(item.satellite) ||
            !ambresRestoreFiniteTime(item.time, time) ||
            !runtime.lastValid.emplace(item.satellite, time).second)
        {
            reason = "AMBRES_CHECKPOINT_E27_LAST_VALID_INVALID";
            return false;
        }
    }
    for (const auto& [satellite, version] : snapshot.arcVersions)
    {
        if (!ambresValidGpsSatellite(satellite) || version <= 0)
        {
            reason = "AMBRES_CHECKPOINT_E27_ARC_VERSION_INVALID";
            return false;
        }
        runtime.arcVersion[satellite] = version;
    }
    for (const auto& item : snapshot.productDatum)
    {
        if (!ambresValidGpsSatellite(item.satellite) ||
            item.firstDiscontinuity < 0 || item.firstDatumVersion < 0 ||
            item.secondDiscontinuity < 0 || item.secondDatumVersion < 0 ||
            !runtime.productDatum.emplace(item.satellite, std::make_tuple(
                item.firstDiscontinuity, item.firstDatumVersion,
                item.secondDiscontinuity, item.secondDatumVersion)).second)
        {
            reason = "AMBRES_CHECKPOINT_E27_PRODUCT_DATUM_INVALID";
            return false;
        }
    }
    if (runtime.lastValid.size() != runtime.arcVersion.size() ||
        runtime.lastValid.size() != runtime.productDatum.size())
    {
        reason = "AMBRES_CHECKPOINT_E27_SATELLITE_REGISTRY_INCOMPLETE";
        return false;
    }
    runtime.reference = snapshot.reference;
    runtime.accumulatorArcVersions = snapshot.accumulatorArcVersions;
    if (runtime.accumulatorArcVersions.size() != runtime.arcVersion.size())
    {
        reason = "AMBRES_CHECKPOINT_E27_ACCUMULATOR_ARC_REGISTRY_INCOMPLETE";
        return false;
    }
    for (const auto& [satellite, version] : runtime.accumulatorArcVersions)
    {
        if (satellite <= 0 || satellite > ZHANG_E27_MAX_SATELLITE ||
            version <= 0)
        {
            reason = "AMBRES_CHECKPOINT_E27_ACCUMULATOR_ARC_INVALID";
            return false;
        }
        SatSys sat(E_Sys::GPS, satellite);
        auto found = runtime.arcVersion.find(sat);
        if (found == runtime.arcVersion.end() || found->second != version)
        {
            reason = "AMBRES_CHECKPOINT_E27_ACCUMULATOR_ARC_CONFLICT";
            return false;
        }
        runtime.accumulator.setArcVersion(satellite, version);
    }
    GTime previousTime;
    bool havePrevious = false;
    for (const auto& item : snapshot.factors)
    {
        ZhangE27WideLaneRawFactor factor;
        if (!ambresValidateFactor(item, factor, reason) ||
            (havePrevious && factor.time < previousTime))
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_E27_FACTOR_CHRONOLOGY_INVALID";
            }
            return false;
        }
        havePrevious = true;
        previousTime = factor.time;
        const double replayTime = static_cast<double>(factor.time.bigTime);
        if (!std::isfinite(replayTime))
        {
            reason = "AMBRES_CHECKPOINT_E27_FACTOR_TIME_OUT_OF_RANGE";
            return false;
        }
        runtime.accumulator.addEpoch(
            replayTime, factor.satellites,
            factor.values, factor.covariance, factor.noiseKeys,
            factor.noiseVariances, factor.satelliteNoiseDesign);
        runtime.rawFactors.push_back(std::move(factor));
    }
    if (runtime.accumulator.factorCount() != runtime.rawFactors.size())
    {
        reason = "AMBRES_CHECKPOINT_E27_FACTOR_REPLAY_MISMATCH";
        return false;
    }
    if (runtime.rawFactors.size() > 1 &&
        (runtime.rawFactors.back().time - runtime.rawFactors.front().time)
                .to_double() > ZHANG_E27_LAG_SECONDS + 1e-6)
    {
        reason = "AMBRES_CHECKPOINT_E27_FACTOR_WINDOW_NOT_PRUNED";
        return false;
    }
    return true;
}

bool ambresValidateChronology(
    const vector<AmbresScalarIncrementCheckpoint>& increments,
    std::deque<ZhangRelinkShadowIncrement>& restored,
    string& reason)
{
    if (increments.size() > ZHANG_AMBRES_MAX_CONTAINER)
    {
        reason = "AMBRES_CHECKPOINT_SCALAR_WINDOW_TOO_LARGE";
        return false;
    }
    GTime previous;
    bool havePrevious = false;
    for (const auto& item : increments)
    {
        GTime time;
        if (!ambresRestoreFiniteTime(item.time, time) ||
            !std::isfinite(item.information) || item.information <= 0 ||
            !std::isfinite(item.natural) ||
            (havePrevious && !(previous < time)) ||
            (havePrevious &&
             acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_gap_seconds > 0 &&
             (time - previous).to_double() >
                acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_gap_seconds))
        {
            reason = "AMBRES_CHECKPOINT_SCALAR_INCREMENT_INVALID";
            return false;
        }
        restored.push_back({time, item.information, item.natural});
        previous = time;
        havePrevious = true;
    }
    return true;
}

bool ambresValidateJointChronology(
    const vector<AmbresJointIncrementCheckpoint>& increments,
    std::deque<ZhangRelinkJointIncrement>& restored,
    string& reason)
{
    if (increments.size() > ZHANG_AMBRES_MAX_CONTAINER)
    {
        reason = "AMBRES_CHECKPOINT_JOINT_WINDOW_TOO_LARGE";
        return false;
    }
    GTime previous;
    bool havePrevious = false;
    for (const auto& item : increments)
    {
        GTime time;
        Matrix2d information;
        Vector2d natural;
        information << item.information[0], item.information[1],
            item.information[2], item.information[3];
        natural << item.natural[0], item.natural[1];
        const double scale = std::max(1.0, information.cwiseAbs().maxCoeff());
        Eigen::SelfAdjointEigenSolver<Matrix2d> solver(
            0.5 * (information + information.transpose()));
        if (!ambresRestoreFiniteTime(item.time, time) ||
            !information.allFinite() || !natural.allFinite() ||
            std::abs(information(0, 1) - information(1, 0)) > 1e-10 * scale ||
            solver.info() != Eigen::Success ||
            solver.eigenvalues().minCoeff() < -1e-10 * scale ||
            solver.eigenvalues().maxCoeff() <= 0 ||
            (havePrevious && !(previous < time)) ||
            (havePrevious &&
             acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_gap_seconds > 0 &&
             (time - previous).to_double() >
                acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_gap_seconds))
        {
            reason = "AMBRES_CHECKPOINT_JOINT_INCREMENT_INVALID";
            return false;
        }
        restored.push_back({time, information, natural});
        previous = time;
        havePrevious = true;
    }
    return true;
}

bool ambresRestoreEnvelope(
    const AmbresCheckpointEnvelope& snapshot,
    const string& rootRuntimeId,
    AmbresRestoredRuntime& restored,
    ZhangAmbresCheckpointResult& result)
{
    string& reason = result.failureReason;
    const auto containerValid = [](std::size_t size)
    {
        return size <= ZHANG_AMBRES_MAX_CONTAINER;
    };
    if (snapshot.schemaVersion != ZHANG_AMBRES_CHECKPOINT_SCHEMA_VERSION)
    {
        reason = "AMBRES_CHECKPOINT_SCHEMA_MISMATCH";
        return false;
    }
    if (snapshot.runtimeId != rootRuntimeId)
    {
        reason = "AMBRES_CHECKPOINT_RUNTIME_ID_MISMATCH";
        return false;
    }
    if (snapshot.filterError ||
        !ambresValidString(snapshot.transactionalConditioningReason) ||
        (snapshot.transactionalConditioningFailed !=
            !snapshot.transactionalConditioningReason.empty()) ||
        !std::isfinite(snapshot.fixedAmbiguityVariance) ||
        snapshot.fixedAmbiguityVariance <= 0 ||
        !containerValid(snapshot.e27Runtimes.size()) ||
        !containerValid(snapshot.heldLattices.size()) ||
        !containerValid(snapshot.heldEvidence.size()) ||
        !containerValid(snapshot.admissionStates.size()) ||
        !containerValid(snapshot.heldUserWideLanes.size()) ||
        !containerValid(snapshot.activeTransitions.size()) ||
        !containerValid(snapshot.relinkPriors.size()) ||
        !containerValid(snapshot.scalarAccumulators.size()) ||
        !containerValid(snapshot.jointAccumulators.size()) ||
        !containerValid(snapshot.whitenedAccumulators.size()))
    {
        reason = "AMBRES_CHECKPOINT_ENVELOPE_VALUE_INVALID";
        return false;
    }

    for (const auto& item : snapshot.e27Runtimes)
    {
        ZhangE27WideLaneKey key;
        ZhangE27WideLaneRuntime runtime;
        if (!ambresRestoreE27Runtime(
                item, rootRuntimeId, key, runtime, reason) ||
            !restored.e27Runtimes.emplace(key, std::move(runtime)).second)
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_E27_RUNTIME_DUPLICATE";
            }
            return false;
        }
        result.e27Factors += item.factors.size();
    }
    result.e27Runtimes = restored.e27Runtimes.size();

    for (const auto& item : snapshot.heldLattices)
    {
        if (!ambresValidOwner(item.ownerRuntimeId, rootRuntimeId, reason) ||
            item.system != E_Sys::GPS || item.lastEventId < 0 ||
            !item.consistent || item.rows.size() > ZHANG_AMBRES_MAX_CONTAINER)
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_HELD_LATTICE_HEADER_INVALID";
            }
            return false;
        }
        ZhangPersistentHeldLattice lattice;
        lattice.lastEventId = item.lastEventId;
        lattice.consistent = item.consistent;
        for (const auto& row : item.rows)
        {
            ZhangPersistentHeldRow restoredRow;
            if (!ambresRestoreHeldRow(row, restoredRow, reason))
            {
                return false;
            }
            lattice.rows.push_back(std::move(restoredRow));
        }
        normalisePersistentHeldLattice(lattice);
        if (!lattice.consistent ||
            !restored.heldLattices.emplace(
                std::make_pair(item.ownerRuntimeId, item.system),
                std::move(lattice)).second)
        {
            reason = "AMBRES_CHECKPOINT_HELD_LATTICE_INCONSISTENT_OR_DUPLICATE";
            return false;
        }
        result.heldRows += item.rows.size();
    }
    result.heldLattices = restored.heldLattices.size();

    for (const auto& item : snapshot.heldEvidence)
    {
        ZhangPersistentHeldRow row;
        if (!ambresValidOwner(item.ownerRuntimeId, rootRuntimeId, reason) ||
            item.system != E_Sys::GPS || item.confirmations <= 0 ||
            item.confirmations >= std::max(
                1, acsConfig.zhangPppAr.promotion_confirmation_epochs) ||
            item.lastEpoch <= 0 ||
            !ambresRestoreHeldRow(item.row, row, reason))
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_HELD_EVIDENCE_INVALID";
            }
            return false;
        }
        auto& evidence = restored.heldEvidence[
            {item.ownerRuntimeId, item.system}];
        if (!evidence.emplace(
                std::move(row), ZhangPersistentHeldEvidence{
                    item.confirmations, item.lastEpoch}).second)
        {
            reason = "AMBRES_CHECKPOINT_HELD_EVIDENCE_DUPLICATE";
            return false;
        }
        result.heldEvidenceRows++;
    }

    for (const auto& item : snapshot.admissionStates)
    {
        if (!ambresValidOwner(item.ownerRuntimeId, rootRuntimeId, reason)
            || item.system != E_Sys::GPS
            || !containerValid(item.certifiedRows.size())
            || !containerValid(item.certifiedValues.size())
            || !containerValid(item.certifiedSatellites.size())
            || !containerValid(item.certifiedRelationIds.size())
            || !containerValid(item.pendingCandidates.size())
            || !containerValid(item.temporalConfirmations.size())
            || item.certifiedRows.size() != item.certifiedValues.size())
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_ADMISSION_HEADER_INVALID";
            }
            return false;
        }
        ZhangProductRelationAdmissionState state;
        for (const auto& row : item.certifiedRows)
        {
            map<string, ZhangExactInteger> restoredRow;
            if (!ambresRestoreSparseExactRow(row, restoredRow, reason))
            {
                return false;
            }
            state.certifiedRows.push_back(std::move(restoredRow));
        }
        for (const auto& text : item.certifiedValues)
        {
            ZhangExactInteger value;
            if (!ambresParseExact(text, value, reason))
            {
                return false;
            }
            state.certifiedValues.push_back(std::move(value));
        }
        for (const auto& satellite : item.certifiedSatellites)
        {
            if (!ambresValidString(satellite, false)
                || !state.certifiedSatellites.insert(satellite).second)
            {
                reason = "AMBRES_CHECKPOINT_ADMISSION_SATELLITE_INVALID";
                return false;
            }
        }
        for (const auto& relationId : item.certifiedRelationIds)
        {
            if (!ambresValidString(relationId, false)
                || !state.certifiedRelationIds.insert(relationId).second)
            {
                reason = "AMBRES_CHECKPOINT_ADMISSION_RELATION_ID_INVALID";
                return false;
            }
        }
        for (const auto& saved : item.pendingCandidates)
        {
            ZhangProductRelationAdmissionCandidate candidate;
            if (!ambresRestoreAdmissionCandidate(saved, candidate, reason)
                || !state.pendingCandidates.emplace(
                    candidate.relationId, std::move(candidate)).second)
            {
                if (reason.empty())
                {
                    reason =
                        "AMBRES_CHECKPOINT_ADMISSION_CANDIDATE_DUPLICATE";
                }
                return false;
            }
        }
        for (const auto& saved : item.temporalConfirmations)
        {
            TemporalCertificateConfirmationState confirmation;
            if (!ambresValidString(saved.relationKey, false)
                || saved.consistentEpochs <= 0
                || saved.firstEpoch <= 0
                || saved.lastEpoch < saved.firstEpoch
                || !containerValid(saved.supportSignatures.size())
                || !ambresParseExact(saved.integerValue,
                    confirmation.integerValue, reason))
            {
                if (reason.empty())
                {
                    reason =
                        "AMBRES_CHECKPOINT_TEMPORAL_CONFIRMATION_INVALID";
                }
                return false;
            }
            confirmation.canonicalRelationKey = saved.relationKey;
            confirmation.consistentEpochs = saved.consistentEpochs;
            confirmation.firstEpoch = saved.firstEpoch;
            confirmation.lastEpoch = saved.lastEpoch;
            confirmation.redundancyConfirmed = saved.redundancyConfirmed;
            for (const auto& signature : saved.supportSignatures)
            {
                if (!ambresValidString(signature, false)
                    || !confirmation.supportSignatures.insert(
                        signature).second)
                {
                    reason =
                        "AMBRES_CHECKPOINT_TEMPORAL_SUPPORT_INVALID";
                    return false;
                }
            }
            if (!state.temporalCertificateConfirmations.emplace(
                    saved.relationKey, std::move(confirmation)).second)
            {
                reason =
                    "AMBRES_CHECKPOINT_TEMPORAL_CONFIRMATION_DUPLICATE";
                return false;
            }
        }
        string stateReason;
        if (!ProductRelationAdmission::validateState(state, &stateReason)
            || !restored.admissionStates.emplace(
                make_pair(item.ownerRuntimeId, item.system),
                std::move(state)).second)
        {
            reason = stateReason.empty()
                ? "AMBRES_CHECKPOINT_ADMISSION_STATE_DUPLICATE"
                : "AMBRES_CHECKPOINT_ADMISSION_STATE_INVALID:" + stateReason;
            return false;
        }
        result.productRelationPendingCandidates +=
            item.pendingCandidates.size();
        result.temporalCertificateConfirmations +=
            item.temporalConfirmations.size();
    }
    result.productRelationAdmissionStates = restored.admissionStates.size();

    for (const auto& item : snapshot.heldUserWideLanes)
    {
        if (!ambresValidOwner(item.ownerRuntimeId, rootRuntimeId, reason)
            || !ambresValidString(item.receiver, false)
            || item.system != E_Sys::GPS
            || !ambresValidGpsSatellite(item.reference)
            || item.integers.empty()
            || !containerValid(item.integers.size()))
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_HELD_USER_WL_HEADER_INVALID";
            }
            return false;
        }
        ZhangHeldUserWideLane held;
        held.reference = item.reference;
        for (const auto& saved : item.integers)
        {
            ZhangExactInteger value;
            if (!ambresValidGpsSatellite(saved.satellite)
                || saved.satellite == held.reference
                || !ambresParseExact(saved.value, value, reason)
                || !held.integers.emplace(saved.satellite, value).second)
            {
                if (reason.empty())
                {
                    reason = "AMBRES_CHECKPOINT_HELD_USER_WL_INTEGER_INVALID";
                }
                return false;
            }
        }
        if (!restored.heldUserWideLanes.emplace(
                ZhangHeldUserWideLaneKey{item.ownerRuntimeId, item.receiver,
                    item.system, item.ifAcceptance},
                std::move(held)).second)
        {
            reason = "AMBRES_CHECKPOINT_HELD_USER_WL_DUPLICATE";
            return false;
        }
        result.heldUserWideLaneIntegers += item.integers.size();
    }
    result.heldUserWideLaneStates = restored.heldUserWideLanes.size();

    for (const auto& item : snapshot.activeTransitions)
    {
        if (!ambresValidOwner(item.ownerRuntimeId, rootRuntimeId, reason) ||
            item.transitions.size() > ZHANG_AMBRES_MAX_CONTAINER)
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_ACTIVE_TRANSITION_HEADER_INVALID";
            }
            return false;
        }
        vector<ZhangPendingProductTransition> transitions;
        set<string> identities;
        // Reference-count diagnostics are computed per AMBRES branch at
        // activation time.  Validate consistency within this owner only;
        // comparing the same snapshot identity across branches is invalid.
        map<string, int> declaredSnapshotReferences;
        auto validateDeclaredSnapshotReference = [&](
            const string& identity,
            int declaredCount)
        {
            if (identity.empty())
            {
                return declaredCount == 0;
            }
            if (declaredCount <= 0)
            {
                return false;
            }
            const auto [found, inserted] =
                declaredSnapshotReferences.emplace(identity, declaredCount);
            return inserted || found->second == declaredCount;
        };
        for (const auto& pending : item.transitions)
        {
            ZhangPendingProductTransition transition;
            if (!ambresRestorePending(pending, transition, reason))
            {
                return false;
            }
            const string identity = transition.eventId + "|" +
                transition.satellite.id() + "|" +
                enum_to_string(transition.observable);
            if (!identities.insert(identity).second)
            {
                reason = "AMBRES_CHECKPOINT_ACTIVE_TRANSITION_DUPLICATE";
                return false;
            }
            if (!validateDeclaredSnapshotReference(
                    transition.oldSnapshotIdentity,
                    transition.oldSnapshotReferenceCount) ||
                !validateDeclaredSnapshotReference(
                    transition.newSnapshotIdentity,
                    transition.newSnapshotReferenceCount))
            {
                reason =
                    "AMBRES_CHECKPOINT_ACTIVE_TRANSITION_REFERENCE_"
                    "DECLARATION_CONFLICT";
                return false;
            }
            transitions.push_back(std::move(transition));
        }
        if (!restored.activeTransitions.emplace(
                item.ownerRuntimeId, std::move(transitions)).second)
        {
            reason = "AMBRES_CHECKPOINT_ACTIVE_TRANSITION_OWNER_DUPLICATE";
            return false;
        }
        result.activeTemporalTransitions += item.transitions.size();
    }

    for (const auto& item : snapshot.relinkPriors)
    {
        GTime time;
        const bool valuesValid = std::isfinite(item.firstMean) &&
            std::isfinite(item.firstVariance) && item.firstVariance >= 0 &&
            std::isfinite(item.wideLaneMean) &&
            std::isfinite(item.wideLaneVariance) && item.wideLaneVariance >= 0 &&
            std::isfinite(item.firstWideLaneCovariance) &&
            std::isfinite(item.firstVariance * item.wideLaneVariance);
        const double covarianceScale = std::max(
            1.0, item.firstVariance * item.wideLaneVariance);
        const bool covarianceValid =
            item.firstWideLaneCovariance * item.firstWideLaneCovariance <=
                item.firstVariance * item.wideLaneVariance +
                    1e-10 * covarianceScale;
        if (!ambresValidOwner(item.ownerRuntimeId, rootRuntimeId, reason) ||
            item.system != E_Sys::GPS ||
            !ambresValidGpsSatellite(item.anchor) ||
            !ambresValidGpsSatellite(item.satellite) ||
            item.anchor == item.satellite ||
            !ambresValidObservable(item.firstCode) ||
            !ambresValidObservable(item.secondCode) ||
            item.firstCode == item.secondCode ||
            item.productDatumVersion < 0 || !valuesValid || !covarianceValid ||
            !ambresRestoreFiniteTime(item.time, time))
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_RELINK_PRIOR_INVALID";
            }
            return false;
        }
        ZhangRelinkPriorMoment moment;
        moment.time = time;
        moment.firstCode = item.firstCode;
        moment.secondCode = item.secondCode;
        moment.productDatumVersion = item.productDatumVersion;
        moment.firstMean = item.firstMean;
        moment.firstVariance = item.firstVariance;
        moment.wideLaneMean = item.wideLaneMean;
        moment.wideLaneVariance = item.wideLaneVariance;
        moment.firstWideLaneCovariance = item.firstWideLaneCovariance;
        if (!restored.relinkPriors[item.ownerRuntimeId].emplace(
                ZhangRelinkMomentKey{
                    item.system, item.anchor, item.satellite}, moment).second)
        {
            reason = "AMBRES_CHECKPOINT_RELINK_PRIOR_DUPLICATE";
            return false;
        }
        result.relinkPriorMoments++;
    }

    for (const auto& item : snapshot.scalarAccumulators)
    {
        ZhangRelinkShadowAccumulator accumulator;
        if (!ambresValidOwner(item.ownerRuntimeId, rootRuntimeId, reason) ||
            !ambresValidString(item.key, false) || !item.initialized ||
            item.increments.empty() ||
            static_cast<int>(item.increments.size()) >
                acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_epochs ||
            !ambresValidateChronology(
                item.increments, accumulator.increments, reason))
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_SCALAR_ACCUMULATOR_INVALID";
            }
            return false;
        }
        double accumulatedInformation = 0;
        double accumulatedNatural = 0;
        for (const auto& increment : accumulator.increments)
        {
            accumulatedInformation += increment.information;
            accumulatedNatural += increment.natural;
        }
        if (!(accumulatedInformation > 0) ||
            !std::isfinite(accumulatedInformation) ||
            !std::isfinite(accumulatedNatural))
        {
            reason = "AMBRES_CHECKPOINT_SCALAR_ACCUMULATED_INFORMATION_INVALID";
            return false;
        }
        accumulator.integerHypothesis = item.integerHypothesis;
        accumulator.initialized = item.initialized;
        if (!restored.scalarAccumulators[item.ownerRuntimeId].emplace(
                item.key, std::move(accumulator)).second)
        {
            reason = "AMBRES_CHECKPOINT_SCALAR_ACCUMULATOR_DUPLICATE";
            return false;
        }
        result.relinkScalarIncrements += item.increments.size();
    }

    for (const auto& item : snapshot.jointAccumulators)
    {
        ZhangRelinkJointAccumulator accumulator;
        if (!ambresValidOwner(item.ownerRuntimeId, rootRuntimeId, reason) ||
            !ambresValidString(item.key, false) || !item.initialized ||
            item.increments.empty() ||
            static_cast<int>(item.increments.size()) >
                acsConfig.zhangPppAr.multi_epoch_relink_shadow_max_epochs ||
            !ambresValidateJointChronology(
                item.increments, accumulator.increments, reason))
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_JOINT_ACCUMULATOR_INVALID";
            }
            return false;
        }
        Matrix2d accumulatedInformation = Matrix2d::Zero();
        Vector2d accumulatedNatural = Vector2d::Zero();
        for (const auto& increment : accumulator.increments)
        {
            accumulatedInformation += increment.information;
            accumulatedNatural += increment.natural;
        }
        Eigen::SelfAdjointEigenSolver<Matrix2d> accumulatedSolver(
            0.5 * (accumulatedInformation +
                   accumulatedInformation.transpose()));
        if (!accumulatedInformation.allFinite() ||
            !accumulatedNatural.allFinite() ||
            accumulatedSolver.info() != Eigen::Success ||
            accumulatedSolver.eigenvalues().minCoeff() <= 0)
        {
            reason = "AMBRES_CHECKPOINT_JOINT_ACCUMULATED_INFORMATION_INVALID";
            return false;
        }
        accumulator.wideLaneHypothesis = item.wideLaneHypothesis;
        accumulator.firstHypothesis = item.firstHypothesis;
        accumulator.initialized = item.initialized;
        if (!restored.jointAccumulators[item.ownerRuntimeId].emplace(
                item.key, std::move(accumulator)).second)
        {
            reason = "AMBRES_CHECKPOINT_JOINT_ACCUMULATOR_DUPLICATE";
            return false;
        }
        result.relinkJointIncrements += item.increments.size();
    }

    for (const auto& item : snapshot.whitenedAccumulators)
    {
        if (!ambresValidOwner(item.ownerRuntimeId, rootRuntimeId, reason) ||
            !ambresValidString(item.key, false) ||
            !ambresValidString(item.physicalSegmentIdentity) ||
            item.lastGraphEventId < 0 || item.lastProductDatumVersion < 0 ||
            item.observations.size() > ZHANG_AMBRES_MAX_CONTAINER ||
            static_cast<int>(item.observations.size()) >
                acsConfig.zhangPppAr.whitened_wl_fixed_lag_max_observations ||
            item.physicalRow.size() > ZHANG_AMBRES_MAX_CONTAINER)
        {
            if (reason.empty())
            {
                reason = "AMBRES_CHECKPOINT_WHITENED_ACCUMULATOR_HEADER_INVALID";
            }
            return false;
        }
        ZhangWhitenedWlAccumulator accumulator;
        GTime previous;
        bool havePrevious = false;
        for (const auto& observation : item.observations)
        {
            GTime time;
            if (!ambresRestoreFiniteTime(observation.time, time) ||
                !std::isfinite(observation.value) ||
                !std::isfinite(observation.variance) ||
                observation.variance <= 0 ||
                (havePrevious && !(previous < time)))
            {
                reason = "AMBRES_CHECKPOINT_WHITENED_OBSERVATION_INVALID";
                return false;
            }
            accumulator.observations.push_back({
                time, observation.value, observation.variance});
            previous = time;
            havePrevious = true;
        }
        if (accumulator.observations.size() > 1 &&
            (accumulator.observations.back().time -
             accumulator.observations.front().time).to_double() >
                acsConfig.zhangPppAr.whitened_wl_fixed_lag_seconds + 1e-3)
        {
            reason = "AMBRES_CHECKPOINT_WHITENED_WINDOW_NOT_PRUNED";
            return false;
        }
        for (const auto& arcSnapshot : item.physicalRow)
        {
            ZhangPhysicalIntegerArc arc;
            ZhangExactInteger coefficient;
            if (!ambresRestoreArc(
                    arcSnapshot, arc, coefficient, reason) ||
                !accumulator.physicalRow.emplace(arc, coefficient).second)
            {
                if (reason.empty())
                {
                    reason = "AMBRES_CHECKPOINT_WHITENED_PHYSICAL_ROW_DUPLICATE";
                }
                return false;
            }
        }
        double accumulatedInformation = 0;
        double accumulatedNatural = 0;
        for (const auto& observation : accumulator.observations)
        {
            accumulatedInformation += 1 / observation.variance;
            accumulatedNatural += observation.value / observation.variance;
        }
        if ((!accumulator.observations.empty() &&
             !(accumulatedInformation > 0)) ||
            !std::isfinite(accumulatedInformation) ||
            !std::isfinite(accumulatedNatural))
        {
            reason = "AMBRES_CHECKPOINT_WHITENED_INFORMATION_INVALID";
            return false;
        }
        accumulator.physicalSegmentIdentity = item.physicalSegmentIdentity;
        accumulator.lastGraphEventId = item.lastGraphEventId;
        accumulator.lastProductDatumVersion = item.lastProductDatumVersion;
        accumulator.initialized = item.initialized;
        if (accumulator.initialized &&
            (accumulator.physicalRow.empty() ||
             accumulator.physicalSegmentIdentity.empty()))
        {
            reason = "AMBRES_CHECKPOINT_WHITENED_INITIALIZED_ROW_EMPTY";
            return false;
        }
        if (!restored.whitenedAccumulators[item.ownerRuntimeId].emplace(
                item.key, std::move(accumulator)).second)
        {
            reason = "AMBRES_CHECKPOINT_WHITENED_ACCUMULATOR_DUPLICATE";
            return false;
        }
        result.whitenedWlObservations += item.observations.size();
    }
    return true;
}

template<class Map, class Predicate>
void ambresEraseIf(Map& values, Predicate predicate)
{
    for (auto iterator = values.begin(); iterator != values.end();)
    {
        if (predicate(*iterator))
        {
            iterator = values.erase(iterator);
        }
        else
        {
            ++iterator;
        }
    }
}

void ambresPrepareCommittedMaps(
    const string& runtimeId,
    const AmbresRestoredRuntime& restored,
    AmbresRestoredRuntime& committed)
{
    committed.e27Runtimes = zhangE27WideLaneRuntimes;
    committed.heldLattices = zhangPersistentHeldLattices;
    committed.heldEvidence = zhangPersistentHeldEvidence;
    committed.admissionStates =
        zhangProductRelationAdmissionStateRegistry();
    committed.heldUserWideLanes = zhangHeldUserWideLaneRegistry;
    committed.activeTransitions = zhangActiveTemporalBesdTransitions;
    committed.relinkPriors = zhangRelinkPriorMoments;
    committed.scalarAccumulators = zhangRelinkShadowAccumulators;
    committed.jointAccumulators = zhangRelinkJointAccumulators;
    committed.whitenedAccumulators = zhangWhitenedWlAccumulators;

    ambresEraseIf(committed.e27Runtimes, [&](const auto& item)
    {
        return zhangAmbresRuntimeBelongsTo(item.first.runtimeId, runtimeId);
    });
    ambresEraseIf(committed.heldLattices, [&](const auto& item)
    {
        return zhangAmbresRuntimeBelongsTo(item.first.first, runtimeId);
    });
    ambresEraseIf(committed.heldEvidence, [&](const auto& item)
    {
        return zhangAmbresRuntimeBelongsTo(item.first.first, runtimeId);
    });
    ambresEraseIf(committed.admissionStates, [&](const auto& item)
    {
        return zhangAmbresRuntimeBelongsTo(item.first.first, runtimeId);
    });
    ambresEraseIf(committed.heldUserWideLanes, [&](const auto& item)
    {
        return zhangAmbresRuntimeBelongsTo(
            std::get<0>(item.first), runtimeId);
    });
    ambresEraseIf(committed.activeTransitions, [&](const auto& item)
    {
        return zhangAmbresRuntimeBelongsTo(item.first, runtimeId);
    });
    ambresEraseIf(committed.relinkPriors, [&](const auto& item)
    {
        return zhangAmbresRuntimeBelongsTo(item.first, runtimeId);
    });
    ambresEraseIf(committed.scalarAccumulators, [&](const auto& item)
    {
        return zhangAmbresRuntimeBelongsTo(item.first, runtimeId);
    });
    ambresEraseIf(committed.jointAccumulators, [&](const auto& item)
    {
        return zhangAmbresRuntimeBelongsTo(item.first, runtimeId);
    });
    ambresEraseIf(committed.whitenedAccumulators, [&](const auto& item)
    {
        return zhangAmbresRuntimeBelongsTo(item.first, runtimeId);
    });

    committed.e27Runtimes.insert(
        restored.e27Runtimes.begin(), restored.e27Runtimes.end());
    committed.heldLattices.insert(
        restored.heldLattices.begin(), restored.heldLattices.end());
    committed.heldEvidence.insert(
        restored.heldEvidence.begin(), restored.heldEvidence.end());
    committed.admissionStates.insert(
        restored.admissionStates.begin(), restored.admissionStates.end());
    committed.heldUserWideLanes.insert(
        restored.heldUserWideLanes.begin(),
        restored.heldUserWideLanes.end());
    committed.activeTransitions.insert(
        restored.activeTransitions.begin(), restored.activeTransitions.end());
    committed.relinkPriors.insert(
        restored.relinkPriors.begin(), restored.relinkPriors.end());
    committed.scalarAccumulators.insert(
        restored.scalarAccumulators.begin(),
        restored.scalarAccumulators.end());
    committed.jointAccumulators.insert(
        restored.jointAccumulators.begin(), restored.jointAccumulators.end());
    committed.whitenedAccumulators.insert(
        restored.whitenedAccumulators.begin(),
        restored.whitenedAccumulators.end());
}

} // namespace
