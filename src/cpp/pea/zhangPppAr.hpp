#pragma once

#include <cstdint>
#include <cstddef>
#include <iosfwd>
#include <map>
#include <set>
#include <string>
#include <vector>
#include "common/eigenIncluder.hpp"
#include "common/enums.h"
#include "common/gTime.hpp"
#include "common/satSys.hpp"
#include "common/zhangSatelliteDatum.hpp"
#include "common/zhangPersistentProductDatum.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "common/zhangUserIntegerFunctional.hpp"

struct KFState;
struct KFKey;
struct KFMeasEntryList;
struct ZhangIfWideLaneEstimate;
struct ZhangProductIntegerConstraintSet;
struct ReceiverMap;
enum class ZhangCapturedMeasurementFamily;
using Trace = std::ostream;

inline constexpr std::uint32_t ZHANG_PPP_AR_CHECKPOINT_SCHEMA_VERSION = 3;
inline constexpr const char* ZHANG_PPP_AR_CHECKPOINT_SECTION_NAME =
	"zhang_ppp_ar_runtime";

struct ZhangPppArCheckpointResult
{
	bool valid = false;
	std::string failureReason;
	std::size_t pendingTransitions = 0;
	std::size_t pendingSnapshotPins = 0;
	std::size_t capturedFactorEvents = 0;
	std::size_t persistentDatumStates = 0;
	std::size_t e27RawNoiseRows = 0;
	std::size_t e27SensitivityRows = 0;
	std::size_t productRelationAdmissionStates = 0;
	std::size_t userReferenceStates = 0;
	std::size_t userDualReferenceStates = 0;
};

/** Pointer-free snapshot-reference inventory decoded from one checkpoint
 * section.  actualReferenceCounts are recomputed from the serialized
 * endpoints.  The transition reference-count fields are only runtime
 * diagnostics: PPP pending transitions legitimately carry zero until AMBRES
 * activates them, and AMBRES computes counts per branch.  They therefore
 * cannot serve as a cross-section integrity total. */
struct ZhangCheckpointSnapshotReferenceSummary
{
	bool valid = false;
	std::string failureReason;
	std::string sectionName;
	std::string runtimeId;
	std::set<std::string> availableSnapshotIdentities;
	std::set<std::string> currentSnapshotIdentities;
	std::set<std::string> pinnedSnapshotIdentities;
	std::map<std::string, std::size_t> actualReferenceCounts;
	std::map<std::string, std::size_t> declaredReferenceCounts;
	std::size_t transitionCount = 0;
};

struct ZhangCheckpointSnapshotReferenceValidation
{
	bool valid = false;
	std::string failureReason;
	std::string runtimeId;
	std::map<std::string, std::size_t> combinedReferenceCounts;
};

/** Export/import the module-owned E29 runtime partition.
 *
 * The payload contains no process address.  runtimeId is mandatory and is
 * checked inside the versioned envelope as well as by the outer checkpoint
 * manifest.  The authoritative KFState must be bound with
 * bindZhangCheckpointRuntimeId() before any Zhang runtime access; disposable
 * float/fixed/user branches require distinct derived runtime IDs.  Import
 * first decodes, validates and reconstructs every temporary
 * owner/global object, then replaces the destination owner's complete module
 * state in one commit.  Export/commit must run at the PEA epoch barrier; the
 * module lock prevents concurrent checkpoint calls but does not pause normal
 * filter callbacks.  For bundle-wide two-phase restore, pass validateOnly=1
 * and the uncommitted core snapshot dimension, preflight every section, then
 * restore the KF core and call again with validateOnly=0.  The caller must
 * also inspect and cross-validate PPP-AR/AMBRES snapshot references before
 * committing either section.  The caller must
 * reinstall callbacks by calling
 * configureZhangE18FactorCapture() after the KF core has been restored. */
ZhangPppArCheckpointResult exportZhangPppArCheckpointSection(
	const KFState& owner,
	const std::string& runtimeId,
	std::string& payload);

ZhangPppArCheckpointResult importZhangPppArCheckpointSection(
	KFState& owner,
	const std::string& runtimeId,
	const std::string& payload,
	bool validateOnly = false,
	int expectedStateDimension = -1);

/** Fully decode and validate the PPP-AR payload, replay its persistent
 * snapshot-operation chronology, and expose the final retained identities and
 * pending transition references without touching live runtime state. */
ZhangPppArCheckpointResult inspectZhangPppArCheckpointSnapshotReferences(
	const std::string& runtimeId,
	const std::string& payload,
	ZhangCheckpointSnapshotReferenceSummary& summary);

/** Cross-section preflight.  Every PPP-AR pending or AMBRES active endpoint
 * must name a retained PPP-AR snapshot.  Combined counts are recomputed from
 * the two payloads rather than trusting the branch-local diagnostic fields. */
ZhangCheckpointSnapshotReferenceValidation
validateZhangCheckpointSnapshotReferences(
	const ZhangCheckpointSnapshotReferenceSummary& pppArSummary,
	const ZhangCheckpointSnapshotReferenceSummary& ambresSummary);

struct ZhangInternalProduct
{
    GTime     time;
    SatSys    satellite;
    E_ObsCode observable = E_ObsCode::NONE;
    std::string solution;

    double clock_m = 0;
    double clock_sigma_m = 0;
    double phase_m = 0;
    double phase_sigma_m = 0;
    double clock_phase_covariance_m2 = 0;
    double correction_m = 0;
    double correction_sigma_m = 0;

    int         discontinuity_counter = 0;
    long long   integer_shift_cycles = 0;
    double      fractional_shift_cycles = 0;
    int         datum_version = 0;
    GTime       valid_from;
    int         product_iod = 0;
    std::string reset_reason;
    bool        persistent_relation_known = false;
    std::string current_alignment_state = "CURRENT_ALIGNMENT_PENDING";
    bool        integer_structure_valid = false;
    bool        integer_datum_continuous = false;
    bool        integer_precision_valid = false;
    bool        integer_valid = false;
    bool        numeric_valid = false;
    bool        branch_valid = false;
    bool        continuity_valid = false;
    bool        ppp_usable = false;
    bool        pppar_usable = false;
    bool        dual_frequency_ar_valid = false;
    bool        discontinuity = false;
    bool        ar_valid = false;
    std::string invalid_reason;
    std::string phase_product_segment_id = "UNRESOLVED";
    std::string integer_component_id = "UNRESOLVED";
    int         integer_component_version = 0;
    int         integer_alignment_generation = 0;
    int         real_gauge_generation = 0;
    int         backend_s_basis_generation = 0;
    std::size_t integer_component_size = 0;
    std::size_t integer_component_rank = 0;
    std::size_t certified_relation_count = 0;
    std::size_t redundant_relation_count = 0;
    bool        cycle_closure_valid = false;
    std::string product_state = "FLOAT_ONLY";
    std::string support_segment_fingerprint = "UNAVAILABLE";
    std::string integer_datum_id;
};

/** Only a product-lattice-conditioned branch may authorize user PPP-AR.
 * Legacy FIXED and NETWORK_FIXED_DIAGNOSTIC products remain useful as PPP
 * controls, but network ambiguity fixing alone is not a satellite-product
 * integer certificate. */
inline bool zhangFormalPppArProductSolution(const std::string& solution)
{
	return solution == "PRODUCT_FIXED";
}

/** Enforce the consumer-side boundary for historical CSV files.  Returns true
 * when a non-product-fixed AR claim was present and has been rejected. */
inline bool zhangRejectNonFormalPppArClaim(ZhangInternalProduct& product)
{
	if (zhangFormalPppArProductSolution(product.solution)) return false;
	const bool rejected = product.pppar_usable || product.ar_valid ||
		product.dual_frequency_ar_valid;
	product.pppar_usable = false;
	product.ar_valid = false;
	product.dual_frequency_ar_valid = false;
	if (rejected)
		product.invalid_reason = "NON_PRODUCT_FIXED_AR_CLAIM_REJECTED";
	return rejected;
}

bool zhangPppArUsesObservable(E_Sys sys, E_ObsCode code);

struct ZhangPendingProductTransition
{
    GTime                         eventTime;
    E_Sys                         system = E_Sys::NONE;
    SatSys                        satellite;
    E_ObsCode                     observable = E_ObsCode::NONE;
    std::string                   eventId;
    ZhangProductIntegerFunctional oldFunctional;
    ZhangProductIntegerFunctional newFunctional;
    ZhangProductIntegerTransition transition;
    std::string                   oldIdentity;
    std::string                   newIdentity;
    std::string                   oldSBasisFingerprint;
    std::string                   newSBasisFingerprint;
    std::string                   oldPhaseSegmentIdentity;
    std::string                   newPhaseSegmentIdentity;
    bool                          phaseSegmentChanged = false;
    std::string                   eventCause = "UNCLASSIFIED";
    int                           oldProductSegment = 0;
    int                           newProductSegment = 0;
    std::string                   oldSnapshotIdentity;
    std::string                   newSnapshotIdentity;
    std::string                   exactTransformChainId;
    int                           oldSnapshotReferenceCount = 0;
    int                           newSnapshotReferenceCount = 0;
    GTime                         expiryTime;
};

struct ZhangTemporalProductSnapshotRequest
{
    E_Sys          system = E_Sys::NONE;
    SatSys         satellite;
    E_ObsCode      observable = E_ObsCode::NONE;
    std::string    snapshotIdentity;
    VectorXd       currentStateRow;
    double         affineOffsetCycles = 0;
    GTime          time;
};

/** Consume transitions discovered by the preceding product-write epoch.
 * The caller may only diagnose/fix them on a disposable branch. */
std::vector<ZhangPendingProductTransition>
takeZhangPendingProductTransitions(const KFState& integerLedgerState);

bool registerZhangTemporalProductSnapshot(
    Trace&             trace,
    const KFState&     captureOwner,
    E_Sys              system,
    const SatSys&      satellite,
    E_ObsCode          observable,
    const std::string& snapshotIdentity,
    const VectorXd&    currentStateRow,
    double             affineOffsetCycles,
    GTime              time);

bool registerZhangTemporalProductSnapshots(
    Trace& trace,
    const KFState& captureOwner,
    const std::vector<ZhangTemporalProductSnapshotRequest>& requests);

/** Bind the proposed product-tree functionals while the pre-reset Zhang
 * coordinate system is still available.  A subsequent rectangular local
 * reinitialisation may remove exactly the chord needed by the new product
 * path; capturing the candidate first preserves a genuine event-time target
 * instead of trying to reconstruct it one epoch later. */
bool registerZhangCandidateProductSnapshotsBeforeCoordinateReset(
    Trace&                              trace,
    const KFState&                      captureOwner,
    const KFState&                      state,
    E_Sys                               system,
    const std::vector<E_ObsCode>&       observables,
    const ZhangGraphBasis&              currentBasis,
	const ZhangGraphBasis&              previousProductBasis,
	const std::map<ZhangGraphEdge, int>& previousProductArcVersions,
    const ZhangGraphBasis&              proposedProductBasis,
    const std::map<ZhangGraphEdge, int>& proposedArcVersions,
    GTime                               time);

/** Joint raw-factor marginal of new-minus-old temporal product snapshots. */
bool queryZhangTemporalProductBesdMarginal(
    const KFState& captureOwner,
    const std::vector<std::pair<std::string, std::string>>& oldNewSnapshots,
    VectorXd& differences,
    MatrixXd& covariance,
    std::vector<bool>& availableRows,
    std::string& failureReason);

/** Same immutable BESD block after removing one finally accepted measurement
 * family and replaying the complete raw-factor/snapshot chronology. */
bool queryZhangTemporalProductBesdMarginalExcludingFamily(
    const KFState& captureOwner,
    const std::vector<std::pair<std::string, std::string>>& oldNewSnapshots,
    ZhangCapturedMeasurementFamily excludedFamily,
    VectorXd& differences,
    MatrixXd& covariance,
    std::vector<bool>& availableRows,
    std::string& failureReason);

struct ZhangTemporalSnapshotLifecycle
{
	bool        valid = false;
	int         activeTransitions = 0;
	int         referencedIdentities = 0;
	std::size_t pendingPinnedIdentities = 0;
	std::size_t retainedBefore = 0;
	std::size_t retainedAfter = 0;
	std::string failureReason;
};

/** Retain the current product snapshots plus every old/new endpoint still
 * referenced by an active temporal event, marginalising all released targets. */
ZhangTemporalSnapshotLifecycle maintainZhangTemporalProductSnapshots(
	const KFState& captureOwner,
	const std::vector<ZhangPendingProductTransition>& activeTransitions);

/** Install read-only E18 factor taps on the authoritative network filter.
 * Copies of the filter fail closed because callbacks verify object identity. */
void configureZhangE18FactorCapture(KFState& kfState);

/** Capture corrected-MW raw noise rows before the IF combination discards the
 * complementary dual-frequency observation direction.  Rows are stamped by
 * epoch and later intersected with the Kalman filter's final accepted noise
 * factors; this is shadow-only and cannot change the authoritative filter. */
void captureZhangE27WideLaneRawNoiseFactors(
    const KFState&          kfState,
    GTime                   time,
    const std::string&      receiver,
    const KFMeasEntryList&  entries
);

bool queryZhangE27WideLaneRawNoiseFactors(
    const KFState&             kfState,
    GTime                      time,
    const std::string&         receiver,
    E_Sys                      system,
    const SatSys&              satellite,
    std::vector<std::string>&  stampedNoiseKeys,
    VectorXd&                  coefficients,
    VectorXd&                  variances
);

/** Cross covariance between selected current PPP state coordinates and the
 * gauge-free WL estimate, obtained from their common stamped raw noise
 * factors after final Kalman measurement screening. */
bool queryZhangE27IfWideLaneCrossCovariance(
    const KFState&                    kfState,
    const std::vector<KFKey>&         stateKeys,
    const ZhangIfWideLaneEstimate&    wideLane,
    MatrixXd&                         crossCovariance,
    std::string*                      failureReason = nullptr
);

/** Eliminate the captured raw H/R and F/Q chronology to the current affine
 * integer datum block.  This is read-only shadow output and never feeds a
 * constraint back to the network filter. */
void traceZhangE18RawIntegerDatumWindow(
    Trace& trace,
    const KFState& captureOwner,
    GTime time
);

ZhangCanonicalRelationSelection selectZhangE18CanonicalProductRelations(
    const KFState& captureOwner,
    E_Sys system,
    const std::vector<ZhangCanonicalSatelliteRelation>& bootstrapCandidates,
    const std::set<SatSys>& availableSatellites,
    int maximumRelations
);

ZhangPersistentProductDatumObservation observeZhangE18PersistentProductDatum(
    const KFState& captureOwner,
    E_Sys system,
    E_ObsCode observable,
    const ZhangCanonicalSatelliteRelation& relation,
    int anchorPhaseSegment,
    int satellitePhaseSegment,
    int anchorDatumVersion,
    int satelliteDatumVersion,
    bool absoluteAvailable
);

/** Register one primitive base integer target at the current posterior.
 *
 * The caller must supply the exact current-coordinate G row and the persistent
 * integer translation z_T when it is known.  Before exact datum transport is
 * available, G k is retained modulo an unknown integer translation: its
 * fractional residual, covariance and integer-error probability are still
 * well-defined, but it is not an absolute continuous OSB datum.  A
 * phase-correction row (C_s-B_s)/lambda is not an integer datum and must never
 * be passed here. */
bool recordZhangE18IntegerDatumTarget(
    Trace&              trace,
    const KFState&      captureOwner,
    const KFState&      state,
    E_Sys               system,
    const std::string&  targetFamily,
    const SatSys&       anchor,
    const SatSys&       satellite,
    const VectorXd&     currentCoordinateRow,
    double              persistentDatumOffsetCycles,
    bool                exactDatumTransportValid,
    const std::string&  canonicalCoordinateIdentity,
    const std::string&  productDatumIdentity,
    int                 productDatumVersion,
    const std::string&  topologyKey,
    const std::string&  gaugeComponentIdentity,
    const std::string&  phaseSegmentIdentity,
    const std::string&  physicalArcSignature,
    const std::vector<std::pair<std::string, int>>& physicalArcVersions,
    GTime               time
);

/** Notify the continuity manager that an exact S-transform changed a raw
 * satellite phase correction.  The manager removes integer branch changes
 * from the emitted product and increments the discontinuity metadata for
 * fractional changes.
 */
void recordZhangExactPhaseTransform(
    GTime          time,
    E_Sys          sys,
    E_ObsCode      code,
    const SatSys&  satellite,
    double         correctionChangeMetres
);

/** Batch form used by a tree exchange.  Satellite-component common
 * fractional gauge shifts are removed before testing integer alignment. */
void recordZhangExactPhaseTransforms(
    GTime                              time,
    E_Sys                              sys,
    E_ObsCode                          code,
    const std::map<SatSys, double>&    correctionChangesMetres
);

/** Notify the continuity manager that the old phase coordinate system could
 * not be transformed exactly and was reinitialised.
 */
void recordZhangPhaseReinitialisation(
    GTime                         time,
    E_Sys                         sys,
    const std::vector<E_ObsCode>& observables,
    const std::string&            reason,
    const std::set<SatSys>&       affectedSatellites
);

/** Promote a fixed named G_sat target into the persistent satellite ledger.
 * The relation convention is alpha_b - alpha_a = integerDifferenceCycles. */
bool promoteZhangSatelliteProductRelation(
    GTime              time,
    E_Sys              sys,
    E_ObsCode          code,
    const SatSys&      a,
    const SatSys&      b,
    long long          integerDifferenceCycles,
    const std::string& provenance
);

ZhangProductRelationEvent promoteZhangSatelliteProductRelationDetailed(
    GTime              time,
    E_Sys              sys,
    E_ObsCode          code,
    const SatSys&      a,
    const SatSys&      b,
    long long          integerDifferenceCycles,
    const std::string& provenance
);

ZhangProductRelationEvent relinkZhangSatelliteProductRelation(
    GTime              time,
    E_Sys              sys,
    E_ObsCode          code,
    const SatSys&      anchor,
    const SatSys&      satellite,
    long long          currentDifferenceCycles,
    const std::string& provenance
);

ZhangCertifiedTemporalAlignmentResult
applyZhangCertifiedTemporalProductShifts(
	GTime                                  time,
	E_Sys                                  sys,
	E_ObsCode                              code,
	const std::map<SatSys, long long>&     rawProductChanges,
	const std::string&                     provenance
);

struct ZhangCertifiedTemporalFrontendBatchResult
{
	bool        accepted = false;
	std::size_t observableGroups = 0;
	std::size_t restoredSatellites = 0;
	std::string reason = "NOT_ATTEMPTED";
};

ZhangCertifiedTemporalFrontendBatchResult
applyZhangCertifiedTemporalProductShiftBatch(
	GTime time,
	E_Sys sys,
	const std::map<E_ObsCode, std::map<SatSys, long long>>& rawProductChanges,
	const std::string& provenance
);

std::size_t quarantineZhangSatelliteProductAlignments(
    GTime                         time,
    E_Sys                         sys,
    E_ObsCode                     code,
    const std::set<SatSys>&       satellites,
    const SatSys&                 trustedAnchor,
    const std::string&            reason
);

std::vector<ZhangSatelliteDatumComponent> zhangSatelliteDatumComponents(
    E_Sys sys,
    E_ObsCode code
);

ZhangCurrentAlignmentState zhangSatelliteAlignmentState(
    E_Sys sys,
    E_ObsCode code,
    const SatSys& satellite
);

/** Current persistent-product identity and exact dynamic-coordinate alignment.
 * phaseSegment changes only for an explicit satellite phase discontinuity;
 * alignmentCycles transports a current S-basis coordinate to that segment's
 * persistent coordinate. */
ZhangSatelliteDatumStatus zhangSatelliteDatumStatus(
    E_Sys sys,
    E_ObsCode code,
    const SatSys& satellite
);

/** Build the segment identity for exactly the physical arcs used by one
 * product-ledger row.  Unrelated satellite discontinuities must not retire a
 * valid row; malformed or cross-system physical identities fail closed. */
std::string zhangProductPhysicalRowSegmentFingerprint(
    E_Sys system,
    const std::map<std::string, ZhangExactInteger>& physicalExpansion
);

bool queryZhangSatelliteProductRelation(
    E_Sys sys,
    E_ObsCode code,
    const SatSys& a,
    const SatSys& b,
    long long& differenceCycles
);

/** Explicit satellite-product phase discontinuity.  Receiver-arc slips and
 * dynamic-tree changes must not call this function. */
void recordZhangSatellitePhaseDiscontinuity(
    GTime                         time,
    E_Sys                         sys,
    const std::vector<E_ObsCode>& observables,
    const SatSys&                 satellite,
    const std::string&            reason
);

/** Write float, network diagnostic and independently certified product-fixed
 * products.  productFixedState is the only formal AR-product source when a
 * productCertification is supplied. */
void writeZhangInternalProducts(
    Trace&         trace,
    const KFState& integerLedgerState,
    const KFState& floatState,
    const KFState* wideLaneState,
    const KFState* networkFixedDiagnosticState,
    const KFState* productFixedState,
    int            newlyFixed,
    bool           integerDatumComplete,
    bool           wideLaneBranchValid,
    bool           fixedBranchValid,
    bool           networkIntegerReady,
    const ZhangProductIntegerConstraintSet* productCertification = nullptr
);

struct ZhangNamedProductIntegerSupport
{
    bool      contained = false;
    long long value = 0;
    int       heldRank = 0;
    std::string reason = "NO_HELD_LATTICE";
};

/** Prove that one named physical satellite-product row belongs to the
 * persistent affine held lattice.  Network-wide fixed rank is insufficient. */
ZhangNamedProductIntegerSupport zhangNamedProductIntegerSupport(
    const KFState&                    integerLedgerState,
    E_Sys                             system,
    E_ObsCode                         observable,
    const std::vector<ZhangGraphEdge>& physicalEdges,
    const std::vector<int>&           physicalArcVersions,
    const ZhangExactVector&           coefficients
);

/** Read and interpolate an exact-epoch internal product for a held-out user. */
bool queryZhangInternalProduct(
    GTime                 time,
    const SatSys&         satellite,
    E_ObsCode             observable,
    ZhangInternalProduct& product
);

/** Return a square-root row of the complete correlated product covariance.
 *
 * The factor columns are common to every measurement at this epoch.  Code
 * uses the satellite CLOCK row; phase uses CLOCK-PHASE(observable).  Applying
 * these rows as shared unit-variance noise elements preserves cross-frequency,
 * clock-phase and cross-satellite covariance in the user measurement block.
 */
bool queryZhangInternalProductNoiseFactors(
    GTime                 time,
    const SatSys&         satellite,
    E_ObsCode             observable,
    bool                  phaseMeasurement,
    std::vector<double>&  factors,
    int*                  numericalRank = nullptr,
    std::string*          failureReason = nullptr
);

/** Select the held-out user's ambiguity reference per signal, perform exact
 * single-difference state/covariance transforms on reference changes, and
 * reset the independent user phase block when the product counter changes.
 */
void updateZhangPppArUserReferences(
    Trace&       trace,
    ReceiverMap& receiverMap,
    KFState&     kfState
);

bool zhangPppArUserReferenceAmbiguity(
    const KFState&     kfState,
    const std::string& receiver,
    const SatSys&      satellite,
    E_ObsCode          observable
);

/** Return the state-coordinate number used by a held-out user phase signal.
 * E27 maps both baseline signals to one shared ionosphere-free coordinate;
 * all other strategies retain the native observation-code number.
 */
int zhangPppArUserPhaseCoordinateNumber(
    E_Sys      system,
    E_ObsCode  observable
);

/** Wavelength multiplying the user phase-coordinate state in the measurement
 * domain.  For the shared E27 coordinate this is the narrow-lane wavelength
 * of the normalized ionosphere-free combination.
 */
double zhangPppArUserPhaseCoordinateWavelength(
    E_Sys      system,
    E_ObsCode  observable
);

SatSys zhangPppArUserReference(
    const KFState&     kfState,
    const std::string& receiver,
    E_Sys              system,
    E_ObsCode          observable
);

bool zhangPppArUserAmbiguityIntegerValid(
    const KFState&     kfState,
    const std::string& receiver,
    const SatSys&      satellite,
    E_ObsCode          observable
);

/** Emit per-user ambiguity clustering and position-error diagnostics. */
void traceZhangPppArUserDiagnostics(
    Trace&       trace,
    ReceiverMap& receiverMap,
    KFState&     kfState
);
