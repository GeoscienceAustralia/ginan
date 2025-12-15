#pragma once

#include "common/attitude.hpp"
#include "common/cache.hpp"
#include "common/common.hpp"
#include "common/eigenIncluder.hpp"
#include "common/gTime.hpp"
#include "common/observations.hpp"
#include "common/satStat.hpp"
#include "pea/ppp.hpp"

struct Parser;

/** Solution of user mode processing functinos
 */
struct Solution
{
    GTime      sppTime;        ///< time (GPST)
    KFState    sppState;       ///< SPP filter object
    VectorEcef sppPos;         ///< Position vector from SPP
    double     sppClk    = 0;  ///< receiver clock/system biases to receiver reference system (m)
    E_Sys      clkRefSys = E_Sys::NONE;             ///< receiver clock reference system
    double     sppPppClkOffset = 0;                 ///< SPP to PPP offset of receiver clock
    bool       clkAdjustReady  = false;             ///< if SPP to PPP offset is ready
    E_Solution status          = E_Solution::NONE;  ///< solution status
    int        numMeas         = 0;  ///< number of valid measurements used to estimate solution
    Dops       dops;                 ///< dilution of precision (GDOP,PDOP,HDOP,VDOP)
    double     horzPL = -1;          ///< Horizontal Protection Level for SBAS
    double     vertPL = -1;          ///< Vertical Protection Level for SBAS
};

struct RinexStation
{
    string   id;                      ///< marker name
    string   marker;                  ///< marker number
    string   antDesc;                 ///< antenna descriptor
    string   antSerial;               ///< antenna serial number
    string   recType;                 ///< receiver type descriptor
    string   recFWVersion;            ///< receiver firmware version
    string   recSerial;               ///< receiver serial number
    Vector3d del = Vector3d::Zero();  ///< antenna position delta (e/n/u or x/y/z) (m)
    Vector3d pos = Vector3d::Zero();
};

struct ReceiverLogs
{
    PTime               firstEpoch = GTime::noTime();
    PTime               lastEpoch  = GTime::noTime();
    int                 epochCount = 0;
    int                 obsCount   = 0;
    int                 slipCount  = 0;
    map<E_ObsCode, int> codeCount;
    map<string, int>    satCount;

    int receiverErrorEpochs = 0;
    int receiverErrorCount  = 0;
};

struct Rtk
{
    Solution             sol;  ///< RTK solution
    string               antennaType;
    string               receiverType;
    string               antennaId;
    map<SatSys, SatStat> satStatMap;
    VectorEnu            antDelta;  ///< antenna delta {rov_e,rov_n,rov_u}
    AttStatus            attStatus;
};

struct SinexSiteId;
struct SinexReceiver;
struct SinexAntenna;
struct SinexSiteEcc;

extern SinexSiteId   dummySiteid;
extern SinexReceiver dummyReceiver;
extern SinexAntenna  dummyAntenna;
extern SinexSiteEcc  dummySiteEcc;

struct SinexRecData
{
    SinexSiteId*   id_ptr  = &dummySiteid;
    SinexReceiver* rec_ptr = &dummyReceiver;
    SinexAntenna*  ant_ptr = &dummyAntenna;
    SinexSiteEcc*  ecc_ptr = &dummySiteEcc;

    UYds start;
    UYds stop = UYds(-1, -1, -1);

    bool primary = false;  ///< this position estimate is considered to come from a primary source
    VectorEcef pos;
    VectorEcef var;
    VectorEcef vel;
    GTime      refEpoch = {};
};

/** Object to maintain receiver station data
 */
struct Receiver : ReceiverLogs, Rtk
{
    bool         isPseudoRec = false;
    bool         invalid     = false;
    SinexRecData snx;  ///< Antenna information

    map<string, string> metaDataMap;

    ObsList obsList;  ///< Observations available for this station at this epoch
    string  id;       ///< Unique name for this station (4 characters)
    string  source;   ///< Source of most recently synchronised data

    bool     primaryApriori = false;
    UYds     aprioriTime;
    double   aprioriClk    = 0;                 ///< receiver clock bias (m)
    double   aprioriClkVar = 0;                 ///< receiver clock variance (m^2)
    Vector3d aprioriPos    = Vector3d::Zero();  ///< receiver position (ecef) (m)
    Matrix3d aprioriPosVar = Matrix3d::Zero();  ///< receiver position variances (m^2)
    Vector3d minconApriori = Vector3d::Zero();

    VectorPos pos;

    bool ready = false;

    Vector3d antBoresight = {0, 0, 1};
    Vector3d antAzimuth   = {0, 1, 0};

    string traceFilename;
    string jsonTraceFilename;
    string sppOutputFile;

    map<SatSys, GTime> savedSlips;

    // Receiver-specific signal tracking capabilities from RINEX header
    map<E_Sys, vector<E_ObsCode>> trackedSignals;

    union
    {
        const unsigned int failure = 0;
        struct
        {
            unsigned failureSinex : 1;
            unsigned failureAprioriPos : 1;
            unsigned failureEccentricity : 1;
            unsigned failureAntenna : 1;
        };
    };
    Cache<tuple<Vector3d, Vector3d, Vector3d, Vector3d, Vector3d>> pppTideCache;
    Cache<tuple<Vector3d>>                                         pppEopCache;
};

struct ReceiverMap : map<string, Receiver>
{
};

extern ReceiverMap receiverMap;

void extractTrackedSignals(Receiver& rec, Parser& parser, ObsList* obsList = nullptr);

struct Network
{
    string traceFilename;
    string jsonTraceFilename;
    string id = "Network";

    KFState kfState = {};
};
