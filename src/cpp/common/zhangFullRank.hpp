#pragma once

#include <algorithm>
#include <string>
#include <vector>
#include "common/enums.h"
#include "common/satSys.hpp"

inline bool zhangFullRankUsesObservable(
    E_ObsCode                    code,
    const std::vector<E_ObsCode>& baselineObservables
)
{
    return std::find(baselineObservables.begin(), baselineObservables.end(), code) !=
           baselineObservables.end();
}

inline bool zhangFullRankIsReferenceReceiver(
    const std::string& receiver,
    const std::string& referenceReceiver
)
{
    return !referenceReceiver.empty() && receiver == referenceReceiver;
}

inline bool zhangFullRankIsReferenceSatellite(
    const SatSys&      satellite,
    const std::string& referenceSatellite
)
{
    if (referenceSatellite.empty())
    {
        return false;
    }

    SatSys reference(referenceSatellite.c_str());
    return satellite.sys == reference.sys && satellite.prn == reference.prn;
}

/** The retained ambiguity is the receiver/satellite double-difference interior.
 *
 * The reference receiver row and reference satellite column are S-bases and
 * are absorbed by the receiver/satellite phase-bias states.
 */
inline bool zhangFullRankRetainsAmbiguity(
    const std::string& receiver,
    const SatSys&      satellite,
    E_ObsCode          code,
    const std::vector<E_ObsCode>& baselineObservables,
    const std::string& referenceReceiver,
    const std::string& referenceSatellite
)
{
    if (!zhangFullRankUsesObservable(code, baselineObservables))
    {
        return false;
    }

    return !zhangFullRankIsReferenceReceiver(receiver, referenceReceiver) &&
           !zhangFullRankIsReferenceSatellite(satellite, referenceSatellite);
}
