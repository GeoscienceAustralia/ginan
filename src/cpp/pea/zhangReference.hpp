#pragma once

#include <iosfwd>

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
