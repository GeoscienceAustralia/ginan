#pragma once

#ifdef ENABLE_MONGODB

#include "common/mongo.hpp"
#include "common/rtcmEncoder.hpp"

struct SSRMeta;

SsrOutMap mongoReadOrbClk(GTime referenceTime, SSRMeta& ssrMeta, int masterIod, E_Sys targetSys);

SsrCBMap mongoReadCodeBias(SSRMeta& ssrMeta, int masterIod, E_Sys targetSys);

SsrPBMap mongoReadPhaseBias(SSRMeta& ssrMeta, int masterIod, E_Sys targetSys);

Eph mongoReadEphemeris(GTime targetTime, SatSys Sat, RtcmMessageType rtcmMessCode);

Geph mongoReadGloEphemeris(GTime targetTime, SatSys Sat);

SSRAtm mongoReadIGSIonosphere(GTime time, const SSRMeta& ssrMeta, int masterIod);

SSRAtm mongoReadCmpAtmosphere(GTime time, SSRMeta ssrMeta);

#include "enums.h"  // For KF enum class definition
struct KFState;

void mongoReadFilter(
    KFState&          kfState,
    GTime             time  = GTime::noTime(),
    const vector<KF>& types = {},
    const string&     Sat   = "",
    const string&     str   = ""
);

#else  // !ENABLE_MONGODB

// Stub declarations when MongoDB is disabled
#include <string>
#include <vector>
#include "common/ephemeris.hpp"
#include "common/rtcmEncoder.hpp"
#include "enums.h"

struct SSRMeta;
struct KFState;
struct GTime;

inline SsrOutMap
mongoReadOrbClk(GTime referenceTime, SSRMeta& ssrMeta, int masterIod, E_Sys targetSys)
{
    return {};
}

inline SsrCBMap mongoReadCodeBias(SSRMeta& ssrMeta, int masterIod, E_Sys targetSys)
{
    return {};
}

inline SsrPBMap mongoReadPhaseBias(SSRMeta& ssrMeta, int masterIod, E_Sys targetSys)
{
    return {};
}

inline Eph mongoReadEphemeris(GTime targetTime, SatSys Sat, RtcmMessageType rtcmMessCode)
{
    return {};
}

inline Geph mongoReadGloEphemeris(GTime targetTime, SatSys Sat)
{
    return {};
}

inline SSRAtm mongoReadIGSIonosphere(GTime time, const SSRMeta& ssrMeta, int masterIod)
{
    return {};
}

inline SSRAtm mongoReadCmpAtmosphere(GTime time, SSRMeta ssrMeta)
{
    return {};
}

inline void mongoReadFilter(
    KFState&               kfState,
    GTime                  time  = GTime::noTime(),
    const std::vector<KF>& types = {},
    const std::string&     Sat   = "",
    const std::string&     str   = ""
)
{
    // No-op when MongoDB is disabled
}

#endif  // ENABLE_MONGODB
