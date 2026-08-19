#include "common/acsQC.hpp"
#include "common/acsConfig.hpp"
#include "common/common.hpp"
#include "common/constants.hpp"
#include "common/navigation.hpp"
#include "common/observations.hpp"

/** Select up to three configured frequency bands for a satellite system.
 *
 * This is the legacy frequency selector used by several modelling paths outside
 * slip detection. It follows `acsConfig.code_priorities[sys]` and converts the
 * first unique codes into frequency bands, but it does not inspect a particular
 * observation to confirm that measurements are present.
 *
 * The outputs retain historical defaults (`F1`, `F2`, `F5`) when priorities are
 * incomplete. Do not use this helper when the algorithm must know which
 * frequencies are actually observed at the current epoch; use `obsFreqs()` for
 * slip detection.
 */
bool satFreqs(E_Sys sys, E_FType& ft1, E_FType& ft2, E_FType& ft3)
{
    bool ft1Ready = false;
    bool ft2Ready = false;

    ft1 = F1;
    ft2 = F2;
    ft3 = F5;

    if (acsConfig.code_priorities.find(sys) == acsConfig.code_priorities.end())
        return false;

    for (auto& code : acsConfig.code_priorities[sys])
    {
        E_FType ft = code2Freq[sys][code];

        if (ft1Ready == false)
        {
            ft1      = ft;
            ft1Ready = true;
            continue;
        }

        if (ft == ft1)
            continue;

        if (ft2Ready == false)
        {
            ft2      = ft;
            ft2Ready = true;
            continue;
        }

        if (ft == ft2)
            continue;

        ft3 = ft;
        break;
    }

    return true;
}

/** Select observed frequency bands that are usable for slip detection.
 *
 * This helper is intentionally stricter than `satFreqs()`: it still honours
 * configured code priorities, but only returns frequency bands that are present
 * in the current observation, have a non-zero representative phase measurement,
 * and have a non-zero wavelength available in the satellite navigation data.
 *
 * The selector returns the first three distinct usable frequencies only. Extra
 * usable frequencies later in `code_priorities` are left for future 4+
 * frequency processing rather than silently changing the current dual/triple
 * frequency slip-detection model.
 */
int obsFreqs(const GObs& obs, E_FType& ft1, E_FType& ft2, E_FType& ft3)
{
    ft1 = NONE;
    ft2 = NONE;
    ft3 = NONE;

    E_Sys sys = obs.Sat.sys;
    if (acsConfig.code_priorities.find(sys) == acsConfig.code_priorities.end())
        return 0;

    if (obs.satNav_ptr == nullptr)
        return 0;

    auto sysCodeIt = code2Freq.find(sys);
    if (sysCodeIt == code2Freq.end())
        return 0;

    int count = 0;

    for (auto& code : acsConfig.code_priorities[sys])
    {
        auto codeIt = sysCodeIt->second.find(code);
        if (codeIt == sysCodeIt->second.end())
            continue;

        E_FType ft = codeIt->second;
        if (ft == NONE || ft == ft1 || ft == ft2 || ft == ft3)
            continue;

        auto sigIt = obs.sigs.find(ft);
        if (sigIt == obs.sigs.end() || sigIt->second.L == 0)
            continue;

        auto lamIt = obs.satNav_ptr->lamMap.find(ft);
        if (lamIt == obs.satNav_ptr->lamMap.end() || lamIt->second == 0)
            continue;

        if (count == 0)
        {
            ft1 = ft;
            count++;
            continue;
        }
        if (count == 1)
        {
            ft2 = ft;
            count++;
            continue;
        }

        ft3 = ft;
        return 3;
    }

    return count;
}
