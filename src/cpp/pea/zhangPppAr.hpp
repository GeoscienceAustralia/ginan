#pragma once

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

struct KFState;
struct ReceiverMap;
using Trace = std::ostream;

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
    std::string invalid_reason;
    std::string integer_component_id = "UNRESOLVED";
    std::string integer_datum_id;
};

bool zhangPppArUsesObservable(E_Sys sys, E_ObsCode code);

/** Install read-only E18 factor taps on the authoritative network filter.
 * Copies of the filter fail closed because callbacks verify object identity. */
void configureZhangE18FactorCapture(KFState& kfState);

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

/** Write both pre-fix and post-feedback internal products. */
void writeZhangInternalProducts(
    Trace&         trace,
    const KFState& floatState,
    const KFState& fixedState,
    int            newlyFixed,
    bool           integerDatumComplete,
    bool           fixedBranchValid,
    bool           networkIntegerReady
);

/** Read and interpolate an exact-epoch internal product for a held-out user. */
bool queryZhangInternalProduct(
    GTime                 time,
    const SatSys&         satellite,
    E_ObsCode             observable,
    ZhangInternalProduct& product
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
