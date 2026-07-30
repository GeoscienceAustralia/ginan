#pragma once

#include <iosfwd>
#include <string>
#include "common/enums.h"
#include "common/satSys.hpp"

struct KFState;
struct ReceiverMap;
using Trace = std::ostream;

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
