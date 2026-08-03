#pragma once

#include <iosfwd>
#include <map>
#include <set>
#include <string>
#include "common/enums.h"
#include "common/satSys.hpp"
#include "common/zhangFullRank.hpp"

struct KFState;
struct ReceiverMap;
using Trace = std::ostream;

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
