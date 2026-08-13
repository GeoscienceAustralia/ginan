#pragma once

#include <cstdint>
#include <iosfwd>
#include <map>
#include <set>
#include <string>
#include <vector>
#include "common/eigenIncluder.hpp"
#include "common/enums.h"
#include "common/satSys.hpp"
#include "common/zhangFullRank.hpp"

struct KFState;
struct ReceiverMap;
using Trace = std::ostream;

inline constexpr std::uint32_t ZHANG_GRAPH_CHECKPOINT_SCHEMA_VERSION = 1;
inline constexpr const char* ZHANG_GRAPH_CHECKPOINT_SECTION_NAME =
    "zhang_graph_runtime";

/** Snapshot needed to express held integer rows in invariant physical arcs. */
struct ZhangGraphIntegerContext
{
    ZhangGraphBasis                  basis;
    ZhangGraphBasis                  productBasis;
    std::map<ZhangGraphEdge, int>    arcVersions;
    int                              eventId = 0;
    int                              productDatumVersion = 0;
    bool                             initialized = false;
};

/** Read-only observation support attached to one physical ambiguity-arc
 * version.  These counters are diagnostic only and are never consulted by the
 * graph controller, Kalman filter, or ambiguity resolver. */
struct ZhangPhysicalArcDiagnostic
{
    ZhangGraphEdge edge;
    int            arcVersion = 0;
    int            ageEpochs = 0;
    int            observationEpochs = 0;
};

/** Event-side support summary for one changed satellite product functional. */
struct ZhangProductFunctionalEventDiagnostic
{
    std::vector<ZhangPhysicalArcDiagnostic> oldSupport;
    std::vector<ZhangPhysicalArcDiagnostic> newSupport;
    int                                      commonObservationEpochs = 0;
    std::string                              eventCause = "UNCLASSIFIED";
};

/** Select usable Zhang references and apply an exact state/covariance S-transform when they change.
 *
 * This function must be called after the epoch prediction and before constructing measurements for
 * the same epoch.
 */
void updateZhangFullRankReferences(
    Trace&       trace,
    ReceiverMap& receiverMap,
    KFState&     kfState
);

/** Whether the active general-tree controller models this baseline edge.
 *
 * Fixed-star mode always returns true.  General-tree mode returns false for
 * edges outside the root receiver's active connected component.
 */
bool zhangGraphModelsObservation(
    const KFState&    kfState,
    const std::string& receiver,
    const SatSys&      satellite,
    E_ObsCode          code
);

/** Whether this edge owns an integer fundamental-cycle ambiguity state. */
bool zhangGraphRetainsAmbiguity(
    const KFState&    kfState,
    const std::string& receiver,
    const SatSys&      satellite,
    E_ObsCode          code
);

/** Whether a satellite belongs to the retained datum-state component and may
 * therefore be emitted as a current network product. */
bool zhangGraphProductSatelliteActive(
    const KFState& kfState,
    const SatSys&  satellite
);

/** Current graph and physical-arc versions for exact held-lattice transport. */
bool zhangGraphIntegerContext(
    const KFState&             kfState,
    E_Sys                      system,
    ZhangGraphIntegerContext& context
);

/** Diagnose the physical support that was removed/introduced by a product
 * functional change.  Inputs contain only non-zero support arcs. */
bool zhangProductFunctionalEventDiagnostic(
    const KFState&                       kfState,
    E_Sys                                system,
    const std::vector<ZhangGraphEdge>&   oldEdges,
    const std::vector<int>&              oldArcVersions,
    const std::vector<ZhangGraphEdge>&   newEdges,
    const std::vector<int>&              newArcVersions,
    ZhangProductFunctionalEventDiagnostic& diagnostic
);

/** Copy read-only graph coordinates to a disposable fixed branch. */
void cloneZhangGraphRuntime(
    const KFState& source,
    const KFState& destination
);

/** Remove graph coordinates belonging to a disposable state copy. */
void eraseZhangGraphRuntime(const KFState& state);

/** Export every Zhang graph/outage runtime owned by one filter into a
 * versioned, pointer-free checkpoint section.
 *
 * runtimeId is the stable identity supplied by the checkpoint orchestrator;
 * an empty identity is rejected.  The returned payload never contains the
 * address of state.
 */
bool exportZhangGraphCheckpointSection(
    const KFState&      state,
    const std::string& runtimeId,
    std::string&       payload,
    std::string&       failureReason
);

/** Decode and fully validate a graph checkpoint section without changing any
 * live runtime state.  Bundle restoration must preflight every section before
 * committing any one of them.
 */
bool validateZhangGraphCheckpointSection(
    const std::string& runtimeId,
    const std::string& payload,
    std::string&       failureReason
);

/** Restore a Zhang graph/outage checkpoint section onto one configured filter.
 *
 * The embedded runtime identity must exactly match runtimeId.  The complete
 * section is decoded and validated before the destination's runtime bindings
 * are replaced atomically; a false return leaves all live runtime maps
 * unchanged.
 */
bool importZhangGraphCheckpointSection(
    KFState&           state,
    const std::string& runtimeId,
    const std::string& payload,
    std::string&       failureReason
);

/** Apply one exact, dimension-preserving graph S-basis transform to a
 * disposable audit branch and return the numerical state transform.  This
 * entry point deliberately does not update product-continuity history or any
 * live graph controller state. */
bool applyZhangGraphBasisTransformForAudit(
    Trace&                         trace,
    KFState&                       branch,
    E_Sys                          system,
    const std::vector<E_ObsCode>&  baselineObservables,
    const ZhangGraphBasis&         oldBasis,
    const ZhangGraphBasis&         newBasis,
    SparseMatrix<double>&          transform,
    std::string&                   failureReason
);
