#pragma once

#include <mutex>
#include "common/constants.hpp"
#include "common/gTime.hpp"
#include "common/trace.hpp"

using std::mutex;

// forward declarations
struct Navigation;
struct Receiver;
struct SatSys;
struct SatPos;

#define MAX_SBAS_MESS_AGE 600

struct SBASMessage
{
    int           prn  = -1;
    int           freq = 1;
    int           type = 63;
    string        message;
    unsigned char data[32];
};

struct SBASFast
{
    GTime  tFast;
    int    iodp;
    double dClk;
    double ddClk   = 0;
    int    dIODF   = -1;
    double dt      = 0;
    double IValid  = -1;
    double accDegr = 10;

    GTime  tIntg;
    int    REint;
    bool   REBoost = false;
    double dRcorr  = 1.0;
};

struct SBASSlow
{
    GTime    toe;
    int      iodp;
    int      iode;
    Vector4d dPos;
    Vector4d ddPos;
    GTime    trec;
    double   Ivalid     = -1;
    bool     pvsEnabled = false;
};

struct SBASCova
{
    GTime    toe;
    double   Ivalid  = 240;
    double   REScale = -1;
    MatrixXd covr;
};
extern map<int, map<SatSys, SBASCova>> sbasUdreCov;

struct SBASMaps
{
    map<int, map<GTime, int, std::greater<GTime>>> fastUpdt;  // fastUpdt[IODP][time] = IODF
    map<int, map<GTime, int, std::greater<GTime>>> slowUpdt;  // slowUpdt[IODP][time] = IODE

    map<int, SBASFast> fastCorr;                              // fastCorr[IODF]  = SBASFast
    map<int, SBASSlow> slowCorr;                              // slowCorr[IODE]  = SBASSlow
};

struct SBASIono
{
    map<int, double> IGPLati;
    map<int, double> IGPLong;
    map<int, double> IGPGIVD;
    map<int, double> IGPGIVE;
};

extern mutex                   sbasMessagesMutex;
extern map<GTime, SBASMessage> sbasMessages;

struct usedSBASIODs
{
    int   iodp = -1;
    int   iodf = -1;
    int   iode = -1;
    GTime tUsed;
};
extern map<SatSys, usedSBASIODs> usedSBASIODMap;

void writeEMSdata(Trace& trace, string emsfile);

void readEMSdata(string emsfile);

void loadSBASdata(Trace& trace, GTime time, Navigation& nav);

void decodeSBASMessage(Trace& trace, GTime time, SBASMessage& mess, Navigation& nav);

void decodeDFMCMessage(Trace& trace, GTime time, SBASMessage& mess, Navigation& nav);

GTime adjustDay(double tod1, GTime nearTime);

void estimateSBASProtLvl(Vector3d& staPos, Matrix3d& ecefP, double& horPL, double& verPL);

void writeSPP(string filename, Receiver& rec);

double rangeErrFromCov(
    Trace&    trace,
    GTime     time,
    int       iodp,
    SatSys    sat,
    Vector3d& rRec,
    Vector3d& rSat,
    double    eCov
);

double estimateIonoVar(GTime time, GTime givdTime, double sigGIVE);

double estimateDFMCVar(
    Trace&    trace,
    GTime     time,
    SatSys    sat,
    Vector3d& rRec,
    Vector3d& rSat,
    SBASFast& sbsFast
);

double estimateSBASVar(
    Trace&    trace,
    GTime     time,
    SatSys    sat,
    Vector3d& rRec,
    Vector3d& rSat,
    SBASFast& sbsFast,
    SBASSlow& sbasSlow
);

double checkSBASVar(
    Trace&    trace,
    GTime     time,
    SatSys    sat,
    Vector3d& Rrec,
    Vector3d& Rsat,
    SBASMaps& sbsMaps
);

void checkForType0(GTime time, int type);