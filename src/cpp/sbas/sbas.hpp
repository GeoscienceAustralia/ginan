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
    GTime  toe;
    double dClk;
    double var;
};

struct SBASSlow
{
    GTime    toe;
    int      iode;
    Vector4d dPos;
    Vector4d ddPos;
    GTime    trec;
    double   Ivalid     = -1;
    bool     pvsEnabled = false;
};

struct SBASRegn
{
    int    priority;
    double Lat1;
    double Lat2;
    double Lon1;
    double Lon2;
    bool   triangular = false;
    double in_Factor;
    double outFactor;
};

struct SBASDegrL1
{
    double Brrc;
    double Cltc_lsb;
    double Cltc_v1;
    double Iltc_v1;
    double Cltc_v0;
    double Iltc_v0;
    double Cgeo_lsb;
    double Cgeo_v;
    double Igeo_v;
    double Cer;
    double Ciono_step;
    double Ciono_ramp;
    double Iiono;
    bool   RSSudre;
    bool   RSSiono;
    double Ccovar;
};

struct SBASDegrSys
{
    double Icorr;
    double Ccorr;
    double Rcorr;
};
struct SBASDegrL5
{
    double                  IValidGNSS = -1;
    double                  IValidGEO;
    double                  CER;
    double                  Ccov;
    map<E_Sys, SBASDegrSys> sysDegr;
    map<int, double>        DFREtable;
    int                     type = 0;
};

struct SBASIntg
{
    GTime trec;
    int   REint;
    bool  REBoost = false;

    double   REScale;
    MatrixXd covr;
    double   dRcorr;
};

struct SBASMaps
{
    map<GTime, int, std::greater<GTime>> corrUpdt;

    map<int, SBASSlow> slowCorr;
    map<int, SBASIntg> Integrity;

    map<int, SBASFast> fastCorr;
    map<int, SBASRegn> UDRERegn;
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
extern Vector3d                sbasRoughStaPos;

void writeEMSdata(Trace& trace, string emsfile);

void readEMSdata(string emsfile);

void loadSBASdata(Trace& trace, GTime time, Navigation& nav);

void decodeSBASMessage(Trace& trace, GTime time, SBASMessage& mess, Navigation& nav);

void decodeDFMCMessage(Trace& trace, GTime time, SBASMessage& mess, Navigation& nav);

GTime adjustDay(double tod1, GTime nearTime);

double estimateDFMCVar(Trace& trace, GTime time, SatPos& satPos, SBASIntg& sbsIntg);

void estimateSBASProtLvl(Vector3d& staPos, Matrix3d& ecefP, double& horPL, double& verPL);

void estimateDFMCPL(Vector3d& staPos, Matrix3d& ecefP, double& horPL, double& verPL);

double sbasSmoothedPsudo(
    Trace&  trace,
    GTime   time,
    SatSys  Sat,
    string  staId,
    double  measP,
    double  measL,
    double  variP,
    double  variL,
    double& varSmooth,
    bool    update
);

void writeSPP(string filename, Receiver& rec);