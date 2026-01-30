#include "common/acsConfig.hpp"
#include "common/common.hpp"
#include "common/navigation.hpp"
#include "common/rtcmDecoder.hpp"
#include "orbprop/coordinates.hpp"
#include "sbas/sbas.hpp"

#define DFMC_DEBUG_TRACE_LEVEL 5

struct sbsDFREsysDegr
{
    double Icorr = 30;
    double Ccorr = 2.55;
    double Rcorr = 0.051;
};

GTime                      mt37Time;
double                     IValidGNSS = 30;
double                     IValidGEO  = 30;
double                     mt37CER    = 31.5;
double                     mt37Ccov   = 12.7;
int                        degrdType  = 0;
map<E_Sys, sbsDFREsysDegr> sysDegr;
map<int, double>           DFREtable;

map<int, map<int, SatSys>> l5SBASSatMasks;
int                        lastIODM = -1;

SatSys l5SatIndex(int sati)
{
    SatSys sat;
    if (sati <= 0)
    {
        sat.sys = E_Sys::NONE;
        sat.prn = 0;
    }
    else if (sati < 33)
    {
        sat.sys = E_Sys::GPS;
        sat.prn = sati;
    }
    else if (sati > 37 && sati < 70)
    {
        sat.sys = E_Sys::GLO;
        sat.prn = sati - 37;
    }
    else if (sati > 74 && sati < 111)
    {
        sat.sys = E_Sys::GAL;
        sat.prn = sati - 74;
    }
    else if (sati > 119 && sati < 159)
    {
        sat.sys = E_Sys::SBS;
        sat.prn = sati - 100;
    }
    else if (sati > 158 && sati < 196)
    {
        sat.sys = E_Sys::BDS;
        sat.prn = sati - 158;
    }

    return sat;
}

void decodeL5SBASMask(Trace& trace, unsigned char* data)
{
    int iodm = getbitu(data, 224, 2);

    tracepdeex(DFMC_DEBUG_TRACE_LEVEL, trace, "L5 mask IODM: %1d", iodm);

    if (l5SBASSatMasks.find(iodm) != l5SBASSatMasks.end())
        l5SBASSatMasks[iodm].clear();

    int i = 0;
    for (int ind = 1; ind <= 214; ind++)
        if (getbitu(data, ind + 9, 1))
        {
            SatSys sat = l5SatIndex(ind);
            if (!sat)
                continue;
            l5SBASSatMasks[iodm][i++] = sat;

            tracepdeex(DFMC_DEBUG_TRACE_LEVEL, trace, ", %s", sat.id().c_str());
        }
    lastIODM = iodm;
}

void decodeL5DFMCCorr(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data)
{
    if (lastIODM < 0)
        return;

    int i  = 10;
    int ns = acsConfig.sbsInOpts.use_do259 ? 8 : 9;

    int sati = getbituInc(data, i, ns);

    SatSys sat = l5SatIndex(sati);
    if (!sat)
    {
        tracepdeex(DFMC_DEBUG_TRACE_LEVEL, trace, " Unknown satellite index %d", sati);
        return;
    }
    SBASSlow sbs;

    int iode     = getbituInc(data, i, 10);
    sbs.iode     = iode;
    sbs.iodp     = lastIODM;
    sbs.dPos[0]  = getbitsInc(data, i, 11) * 0.0625;
    sbs.dPos[1]  = getbitsInc(data, i, 11) * 0.0625;
    sbs.dPos[2]  = getbitsInc(data, i, 11) * 0.0625;
    sbs.dPos[3]  = getbitsInc(data, i, 12) * 0.03125;
    sbs.ddPos[0] = getbitsInc(data, i, 8) * P2_11;
    sbs.ddPos[1] = getbitsInc(data, i, 8) * P2_11;
    sbs.ddPos[2] = getbitsInc(data, i, 8) * P2_11;
    sbs.ddPos[3] = getbitsInc(data, i, 9) * P2_12;

    int tod_int = getbituInc(data, i, 13);
    sbs.Ivalid  = IValidGNSS;

    if (acsConfig.sbsInOpts.pvs_on_dfmc)
    {
        if (tod_int % 2 == 0)
            return;
        sbs.Ivalid = 100;
    }

    double tod1 = 16.0 * tod_int;
    GTime  teph = adjustDay(tod1, frameTime);
    sbs.toe     = teph;
    sbs.trec    = frameTime;

    auto& sbsMap                         = nav.satNavMap[sat].currentSBAS;
    sbsMap.slowCorr[iode]                = sbs;
    sbsMap.slowUpdt[lastIODM][frameTime] = iode;

    tracepdeex(
        DFMC_DEBUG_TRACE_LEVEL,
        trace,
        " L5 eph: %s, %s, %3d,   %f, %f, %f, %f,   %f, %f, %f, %f",
        sat.id().c_str(),
        teph.to_string(0).c_str(),
        iode,
        sbsMap.slowCorr[iode].dPos[0],
        sbsMap.slowCorr[iode].dPos[1],
        sbsMap.slowCorr[iode].dPos[2],
        sbsMap.slowCorr[iode].dPos[3],
        sbsMap.slowCorr[iode].ddPos[0],
        sbsMap.slowCorr[iode].ddPos[1],
        sbsMap.slowCorr[iode].ddPos[2],
        sbsMap.slowCorr[iode].ddPos[3]
    );

    //--------------------------------------------
    double   expnt = getbituInc(data, i, 3) - 5.0;
    double   scale = pow(2, expnt);
    MatrixXd E     = MatrixXd::Zero(4, 4);
    E(0, 0)        = getbituInc(data, i, 9);
    E(1, 1)        = getbituInc(data, i, 9);
    E(2, 2)        = getbituInc(data, i, 9);
    E(3, 3)        = getbituInc(data, i, 9);
    E(0, 1)        = getbitsInc(data, i, 10);
    E(0, 2)        = getbitsInc(data, i, 10);
    E(0, 3)        = getbitsInc(data, i, 10);
    E(1, 2)        = getbitsInc(data, i, 10);
    E(1, 3)        = getbitsInc(data, i, 10);
    E(2, 3)        = getbitsInc(data, i, 10);
    MatrixXd R     = scale * E;
    MatrixXd C     = R.transpose() * R;
    int      REint = getbituInc(data, i, 4);
    double   dRcorr;
    if (acsConfig.sbsInOpts.use_do259)
        dRcorr = (getbituInc(data, i, 4) + 1) / 15;
    else
        dRcorr = (getbituInc(data, i, 3) + 1) / 8;

    sbsMap.fastCorr[4].tIntg             = frameTime;
    sbsMap.fastCorr[4].REint             = REint;
    sbsMap.fastCorr[4].REBoost           = false;
    sbsMap.fastCorr[4].dRcorr            = dRcorr;
    sbsMap.fastUpdt[lastIODM][frameTime] = 4;

    auto& sbsCov   = sbasUdreCov[lastIODM][sat];
    sbsCov.toe     = frameTime;
    sbsCov.Ivalid  = IValidGNSS;
    sbsCov.REScale = scale;
    sbsCov.covr    = C;

    tracepdeex(
        DFMC_DEBUG_TRACE_LEVEL,
        trace,
        ", %2d, %.3f",
        sbsMap.fastCorr[4].REint,
        sbsMap.fastCorr[4].dRcorr
    );
}

void decodeL5DFMCInt1(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data)
{
    int iodm = getbitu(data, 224, 2);
    if (l5SBASSatMasks.find(iodm) == l5SBASSatMasks.end())
    {
        tracepdeex(DFMC_DEBUG_TRACE_LEVEL, trace, " corrections for unknown IODM: %1d", iodm);
        return;
    }

    tracepdeex(DFMC_DEBUG_TRACE_LEVEL, trace, " L5 DFRECI IODM: %1d", iodm);

    int                   i = 10;
    int                   j = 0;
    map<int, SatSys>      changedDFRE;
    map<SatSys, SBASFast> buffer;
    for (int slot = 0; slot < 92; slot++)
    {
        if (l5SBASSatMasks[iodm].find(slot) == l5SBASSatMasks[iodm].end())
            continue;
        SatSys sat          = l5SBASSatMasks[iodm][slot];
        auto&  sbs          = nav.satNavMap[sat].currentSBAS;
        buffer[sat]         = sbs.fastCorr[4];
        buffer[sat].REBoost = false;

        int DFRECI = getbituInc(data, i, 2);
        switch (DFRECI)
        {
            case 2:
                buffer[sat].REBoost = true;
                break;
            case 1:
                if (j < 7)
                    changedDFRE[j++] = sat;
            case 3:
                buffer[sat].REint = 15;
                break;
        }
        buffer[sat].tIntg = frameTime;
    }

    for (int slot = 0; slot < j; slot++)
    {
        SatSys sat        = changedDFRE[slot];
        buffer[sat].REint = getbituInc(data, i, 4);
    }

    for (auto [sat, fastData] : buffer)
    {
        auto& sbs                     = nav.satNavMap[sat].currentSBAS;
        sbs.fastCorr[4]               = buffer[sat];
        sbs.fastUpdt[iodm][frameTime] = 4;
    }
}

void decodeL5DFMCInt2(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data)
{
    int iodm = getbitu(data, 224, 2);
    if (l5SBASSatMasks.find(iodm) == l5SBASSatMasks.end())
    {
        tracepdeex(DFMC_DEBUG_TRACE_LEVEL, trace, " corrections for unknown IODM: %1d\n", iodm);
        return;
    }

    tracepdeex(DFMC_DEBUG_TRACE_LEVEL, trace, " L5 DFREI (1-53) IODM: %1d", iodm);

    int i = 10;
    for (int slot = 0; slot < 53; slot++)
    {
        if (l5SBASSatMasks[iodm].find(slot) == l5SBASSatMasks[iodm].end())
            continue;
        SatSys sat                    = l5SBASSatMasks[iodm][slot];
        auto&  sbs                    = nav.satNavMap[sat].currentSBAS;
        sbs.fastCorr[4].REint         = getbituInc(data, i, 4);
        sbs.fastCorr[4].REBoost       = false;
        sbs.fastCorr[4].tIntg         = frameTime;
        sbs.fastUpdt[iodm][frameTime] = 4;
    }
}

void decodeL5DFMCInt3(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data)
{
    int iodm = getbitu(data, 224, 2);
    if (l5SBASSatMasks.find(iodm) == l5SBASSatMasks.end())
    {
        tracepdeex(DFMC_DEBUG_TRACE_LEVEL, trace, " corrections for unknown IODM: %1d\n", iodm);
        return;
    }

    tracepdeex(DFMC_DEBUG_TRACE_LEVEL, trace, " L5 DFREI (54-92) IODM: %1d", iodm);

    int i = 10;
    for (int slot = 53; slot < 92; slot++)
    {
        if (l5SBASSatMasks[iodm].find(slot) == l5SBASSatMasks[iodm].end())
            continue;
        SatSys sat                    = l5SBASSatMasks[iodm][slot];
        auto&  sbs                    = nav.satNavMap[sat].currentSBAS;
        sbs.fastCorr[4].REint         = getbituInc(data, i, 4);
        sbs.fastCorr[4].REBoost       = false;
        sbs.fastCorr[4].tIntg         = frameTime;
        sbs.fastUpdt[iodm][frameTime] = 4;
    }
}

void decodeL5DFREDegr(Trace& trace, GTime frameTime, unsigned char* data)
{
    int i      = 10;
    IValidGNSS = getbituInc(data, i, 6) * 6.0 + 30.0;
    IValidGEO  = getbituInc(data, i, 6) * 6.0 + 30.0;
    mt37CER    = getbituInc(data, i, 6) * 0.5;
    mt37Ccov   = getbituInc(data, i, 7) * 0.1;
    mt37Time   = frameTime;

    if (!acsConfig.sbsInOpts.prec_aproach)
    {
        IValidGNSS *= 1.5;
        IValidGEO *= 1.5;
    }

    sysDegr[E_Sys::GPS].Icorr = getbituInc(data, i, 5) * 6.0 + 30.0;
    sysDegr[E_Sys::GPS].Ccorr = getbituInc(data, i, 8) * 0.01;
    sysDegr[E_Sys::GPS].Rcorr = getbituInc(data, i, 8) * 0.2;

    sysDegr[E_Sys::GLO].Icorr = getbituInc(data, i, 5) * 6.0 + 30.0;
    sysDegr[E_Sys::GLO].Ccorr = getbituInc(data, i, 8) * 0.01;
    sysDegr[E_Sys::GLO].Rcorr = getbituInc(data, i, 8) * 0.2;

    sysDegr[E_Sys::GAL].Icorr = getbituInc(data, i, 5) * 6.0 + 30.0;
    sysDegr[E_Sys::GAL].Ccorr = getbituInc(data, i, 8) * 0.01;
    sysDegr[E_Sys::GAL].Rcorr = getbituInc(data, i, 8) * 0.2;

    sysDegr[E_Sys::BDS].Icorr = getbituInc(data, i, 5) * 6.0 + 30.0;
    sysDegr[E_Sys::BDS].Ccorr = getbituInc(data, i, 8) * 0.01;
    sysDegr[E_Sys::BDS].Rcorr = getbituInc(data, i, 8) * 0.2;

    sysDegr[E_Sys::SBS].Icorr = getbituInc(data, i, 5) * 6.0 + 30.0;
    sysDegr[E_Sys::SBS].Ccorr = getbituInc(data, i, 8) * 0.01;
    sysDegr[E_Sys::SBS].Rcorr = getbituInc(data, i, 8) * 0.2;

    i += 21;

    DFREtable[0]  = getbituInc(data, i, 4) * 0.0625 + 0.125;
    DFREtable[1]  = getbituInc(data, i, 4) * 0.125 + 0.25;
    DFREtable[2]  = getbituInc(data, i, 4) * 0.125 + 0.375;
    DFREtable[3]  = getbituInc(data, i, 4) * 0.125 + 0.5;
    DFREtable[4]  = getbituInc(data, i, 4) * 0.125 + 0.625;
    DFREtable[5]  = getbituInc(data, i, 4) * 0.25 + 0.75;
    DFREtable[6]  = getbituInc(data, i, 4) * 0.25 + 1.0;
    DFREtable[7]  = getbituInc(data, i, 4) * 0.25 + 1.25;
    DFREtable[8]  = getbituInc(data, i, 4) * 0.25 + 1.5;
    DFREtable[9]  = getbituInc(data, i, 4) * 0.25 + 1.75;
    DFREtable[10] = getbituInc(data, i, 4) * 0.5 + 2.0;
    DFREtable[11] = getbituInc(data, i, 4) * 0.5 + 2.5;
    DFREtable[12] = getbituInc(data, i, 4) * 1.0 + 3.0;
    DFREtable[13] = getbituInc(data, i, 4) * 3.0 + 4.0;
    DFREtable[14] = getbituInc(data, i, 4) * 6.0 + 10;

    tracepdeex(
        DFMC_DEBUG_TRACE_LEVEL,
        trace,
        " L5 Degradation parameters: %f %f %f",
        IValidGNSS,
        mt37CER,
        mt37Ccov
    );

    int timeRef = getbituInc(data, i, 3);  // Only GPS time is supported, for now
    if (!acsConfig.sbsInOpts.use_do259)
        degrdType = getbituInc(data, i, 1);
}

void decodeDFMCMessage(Trace& trace, GTime time, SBASMessage& mess, Navigation& nav)
{
    int type = mess.type;
    if (type == 0)
        type = acsConfig.sbsInOpts.mt0;

    checkForType0(time, type);

    if (type == 65)  // Handling of SouthPAN L5 message type 0
        type = 33 + getbitu(mess.data, 222, 2);

    tracepdeex(
        DFMC_DEBUG_TRACE_LEVEL,
        trace,
        "\nDFMCMESS %s Decoding %2d: ",
        time.to_string().c_str(),
        type
    );

    switch (type)
    {
        case 31:
            decodeL5SBASMask(trace, mess.data);
            break;  // Satellite mask
        case 32:
            decodeL5DFMCCorr(trace, time, nav, mess.data);
            break;  // Satellite Corrections & Covariance
        case 34:
            decodeL5DFMCInt1(trace, time, nav, mess.data);
            break;  // Satellite Integrity Information (DFRECI)
        case 35:
            decodeL5DFMCInt2(trace, time, nav, mess.data);
            break;  // Satellite Integrity Information (DFREI 1 ~ 53)
        case 36:
            decodeL5DFMCInt3(trace, time, nav, mess.data);
            break;  // Satellite Integrity Information (DFREI 54 ~ 92)
        case 37:
            decodeL5DFREDegr(trace, time, mess.data);
            break;  // DFRE Correction degradation and scale
        // case 39:	decodeL5GEONavg1(trace,time,nav,mess.data);		break;	// GEO Ephemeris, clock
        // and covariance 1 case 40:	decodeL5GEONavg2(trace,time,nav,mess.data);		break;	//
        // GEO Ephemeris, clock and covariance 2 case 42:
        // decodeL5GNSSTime(trace,time,nav,mess.data);		break;	// GNSS Time Offset case 47:
        // decodeL5GEO_Almn(trace,time,nav,mess.data);		break;	// GEO satellite position data
        // (Almanac) case 62:
        case 63:
            break;
        default:
            tracepdeex(5, std::cout, "\nSBAS_MT%02d, not supported", type);
            break;
    }
    return;
}

double estimateDFMCVar(
    Trace&    trace,
    GTime     time,
    SatSys    sat,
    Vector3d& rRec,
    Vector3d& rSat,
    SBASFast& sbsIntg
)
{
    int DFREI = sbsIntg.REint;
    if (sbsIntg.REBoost)
        DFREI++;

    if (DFREI < 0)
        return -2;
    if (DFREI == 14)
        return -1;
    if (DFREI == 15)
        return -2;

    double dt    = (time - mt37Time).to_double();
    double maxDt = acsConfig.sbsInOpts.prec_aproach ? 240 : 360;
    if (dt > maxDt)
        return -2;

    double dDFRE = rangeErrFromCov(trace, time, sbsIntg.iodp, sat, rRec, rSat, mt37Ccov);
    if (dDFRE < 0)
        return -2;

    double sigDFRE = DFREtable[DFREI];

    double dRCorr = (dt > IValidGNSS) ? sbsIntg.dRcorr : 1;
    auto   sys    = sat.sys;
    double CCorr  = sysDegr[sys].Ccorr;
    double ICorr  = sysDegr[sys].Icorr;
    double RCorr  = sysDegr[sys].Rcorr;
    double eCorr  = CCorr * floor(dt / ICorr) + dRCorr * RCorr * dt / 1000;

    double eer = (dt > IValidGNSS) ? mt37CER : 0;

    double var;
    if (degrdType == 1)
        var = SQR(dDFRE * (SQR(sigDFRE) + eCorr + eer));
    else
        var = SQR(dDFRE * sigDFRE) + SQR(eCorr) + SQR(eer);

    tracepdeex(
        5,
        trace,
        "\nSBASVAR %s %s, DFRE= %2d %.4f, dDFRE: %.5e, eCorr: %.3f, eer: %3f, "
        "total: %.3f",
        time.to_string().c_str(),
        sat.id().c_str(),
        sbsIntg.REint,
        sigDFRE,
        dDFRE,
        eCorr,
        eer,
        sqrt(var)
    );

    return var;
}
