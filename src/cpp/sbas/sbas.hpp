#pragma once

#include <mutex>
#include "common/constants.hpp"
#include "common/gTime.hpp"
#include "common/trace.hpp"

using std::mutex;

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
    Vector4d dEph;
    MatrixXd covr;
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

struct SBASMaps
{
    map<int, SBASFast>   fastCorr;
    map<int, SBASSlow>   slowCorr;
    map<int, SBASRegn>   UDRERegn;
    map<int, SBASDegrL1> L1Degr;
    map<int, SBASDegrL5> L5Degr;
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

void writeEMSdata(Trace& trace, string emsfile);

void readEMSdata(Trace& trace, string emsfile);

void loadSBASdata(Trace& trace, GTime time, int prn, int freq);
