#include "common/acsConfig.hpp"
#include "common/navigation.hpp"
#include "common/rtcmDecoder.hpp"
#include "iono/ionoSBAS.hpp"
#include "orbprop/coordinates.hpp"
#include "sbas/sbas.hpp"

#define SBAS_DEBUG_TRACE_LEVEL 2
#define SBAS_DEGR_OUTAGE 360

map<int, map<int, SatSys>>
    l1SBASSatMasks;  // Satellite mask updated by MT1, l1SBASSatMasks[IODP][index] = SatSys;
int lastIODP = -1;

// Fast degradation
int              degrFCTlat = 0;
map<SatSys, int> degrL1Ind;
GTime            fstDegrUpdate;

double sig2UDREI[16] = {
    0.052,
    0.0924,
    0.1444,
    0.283,
    0.4678,
    0.8315,
    1.2992,
    1.8709,
    2.5465,
    3.326,
    5.1968,
    20.7870,
    230.9661,
    2078.695,
    -1,
    -2
};
double sig_UDREI[16] =
    {0.75, 1.0, 1.25, 1.75, 2.25, 3.0, 3.75, 4.5, 5.25, 6.0, 7.5, 15.0, 50, 150, -1, -2};
double degrL1Err[16] = {0, 5, 9, 12, 15, 20, 30, 45, 60, 90, 150, 210, 270, 330, 460, 580};
double degrL1Out[16] = {120, 120, 102, 90, 90, 78, 66, 54, 42, 30, 30, 18, 18, 18, 12, 12};

double sbasGEOUra[16] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};

double dUDRETable[16] = {1, 1.1, 1.25, 1.5, 2, 3, 4, 5, 6, 8, 10, 20, 30, 40, 50, 100};
struct SBASRegn
{
    double lat1;
    double lat2;
    double lon1;
    double lon2;
    bool   triangular = false;
    double in_Factor;
    double outFactor;
    GTime  time;
};
struct SBASRegnMap
{
    map<int, map<int, SBASRegn>> regions;
    map<int, int>                mess;
    int                          totmess = 8;
    GTime                        tUpdate;
};
map<int, SBASRegnMap> regnMaps;

struct SBASDegrL1
{
    GTime  tUpdate;
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
    double Iiono = -1;
    bool   RSSudre;
    bool   RSSiono;
    double Ccovar;
    bool   velCode = 0;
};
SBASDegrL1 degrL1Slow;

SatSys l1SatIndex(int sati)
{
    SatSys sat;
    sat.sys = E_Sys::NONE;
    sat.prn = 0;
    if (sati > 0 && sati < 38)
    {
        sat.sys = E_Sys::GPS;
        sat.prn = sati;
    }
    else if (sati > 37 && sati < 62)
    {
        sat.sys = E_Sys::GLO;
        sat.prn = sati - 37;
    }
    else if (sati > 119 && sati < 159)
    {
        sat.sys = E_Sys::SBS;
        sat.prn = sati - 100;
    }
    return sat;
}

void decodeL1SBASMask(Trace& trace, unsigned char* data)
{
    int iodp = getbitu(data, 224, 2);
    tracepdeex(SBAS_DEBUG_TRACE_LEVEL, trace, "L1 mask IODP: %1d", iodp);

    if (l1SBASSatMasks.find(iodp) != l1SBASSatMasks.end())
        l1SBASSatMasks[iodp].clear();

    int i = 0;
    for (int ind = 1; ind <= 214; ind++)
        if (getbitu(data, ind + 13, 1))
        {
            SatSys sat = l1SatIndex(ind);
            if (!sat)
                continue;
            l1SBASSatMasks[iodp][i++] = sat;

            tracepdeex(SBAS_DEBUG_TRACE_LEVEL, trace, ", %s", sat.id().c_str());
        }
    tracepdeex(SBAS_DEBUG_TRACE_LEVEL, trace, "\n");
    lastIODP = iodp;
}

void decodeL1FastCorr(
    Trace&         trace,
    GTime          frameTime,
    Navigation&    nav,
    unsigned char* data,
    int            start
)
{
    int iodf = getbitu(data, 14, 2);
    int iodp = getbitu(data, 16, 2);

    if (l1SBASSatMasks.find(iodp) == l1SBASSatMasks.end())
        return;

    for (int i = 0; i < 13; i++)
    {
        int slot = i + start;
        if (l1SBASSatMasks[iodp].find(slot) == l1SBASSatMasks[iodp].end())
            continue;
        SatSys sat = l1SBASSatMasks[iodp][slot];
        if (degrL1Ind.find(sat) == degrL1Ind.end())
            continue;

        int    iFast   = 18 + 12 * i;
        double dClk    = getbits(data, iFast, 12) * 0.125;
        double degrIfc = degrL1Out[degrL1Ind[sat]];
        double ddClk   = 0;
        int    dIODF   = -1;

        auto& sbs = nav.satNavMap[sat].currentSBAS;
        if (!sbs.fastUpdt[iodp].empty())
        {
            auto it       = sbs.fastUpdt[iodp].begin();
            int  prevIODF = it->second;
            if (sbs.fastCorr.find(prevIODF) != sbs.fastCorr.end())
            {
                double dt = (frameTime - sbs.fastCorr[prevIODF].tFast).to_double();
                if (degrL1Ind[sat] != 0 && dt < degrIfc)
                {
                    ddClk = (dClk - sbs.fastCorr[prevIODF].dClk) / dt;
                    dIODF = (iodf - prevIODF) % 3;
                    if (iodf == 3)
                        dIODF = 3;
                }
            }
        }

        int iUdre = 174 + 4 * i;
        int UDREI = getbitu(data, iUdre, 4);
        if (UDREI > 13)
        {
            sbs.fastCorr.clear();
            sbs.fastUpdt[iodp].clear();
            ddClk = 0.0;
            dIODF = -1;
        }

        sbs.fastUpdt[iodp][frameTime] = iodf;
        sbs.fastCorr[iodf].tFast      = frameTime;
        sbs.fastCorr[iodf].dClk       = dClk;
        sbs.fastCorr[iodf].ddClk      = ddClk;
        sbs.fastCorr[iodf].dIODF      = dIODF;
        sbs.fastCorr[iodf].IValid     = degrIfc;
        sbs.fastCorr[iodf].accDegr    = degrL1Err[degrL1Ind[sat]] * 1e-5;

        tracepdeex(
            SBAS_DEBUG_TRACE_LEVEL,
            trace,
            "L1 Fast Correction %s(%1d); dClk = %7.3f; ddClk = %7.3f; UDREI = %d;\n",
            sat.id().c_str(),
            iodf,
            dClk,
            ddClk,
            UDREI
        );
        if (iodf == 3)
        {
            for (auto& [iod, integr] : sbs.fastCorr)
            {
                integr.tIntg = frameTime;
                integr.REint = UDREI;
            }
        }
        else
        {
            sbs.fastCorr[iodf].tIntg = frameTime;
            sbs.fastCorr[iodf].REint = UDREI;
        }
    }
}

void decodeL1UDREIall(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data)
{
    if (lastIODP < 0)
        return;
    if (l1SBASSatMasks.find(lastIODP) == l1SBASSatMasks.end())
        return;

    int iodfInd[4];
    iodfInd[0] = getbitu(data, 14, 2);
    iodfInd[1] = getbitu(data, 16, 2);
    iodfInd[2] = getbitu(data, 18, 2);
    iodfInd[3] = getbitu(data, 20, 2);
    for (int i = 0; i <= 51; i++)
    {
        if (l1SBASSatMasks[lastIODP].find(i) == l1SBASSatMasks[lastIODP].end())
            continue;
        SatSys sat   = l1SBASSatMasks[lastIODP][i];
        auto&  sbs   = nav.satNavMap[sat].currentSBAS;
        int    j     = i / 13;
        int    iodf  = iodfInd[j];
        int    iUdre = 22 + 4 * i;
        int    UDREI = getbitu(data, iUdre, 4);
        if (UDREI > 13)
        {
            sbs.fastCorr.clear();
            sbs.fastUpdt[lastIODP].clear();
        }
        tracepdeex(
            SBAS_DEBUG_TRACE_LEVEL,
            trace,
            "L1 integrity %s(%d) UDREI = %d\n",
            sat.id().c_str(),
            iodf,
            UDREI
        );
        if (iodf == 3)
            for (auto& [iod, integr] : sbs.fastCorr)
            {
                integr.tIntg = frameTime;
                integr.REint = UDREI;
            }
        else
        {
            sbs.fastCorr[iodf].tIntg = frameTime;
            sbs.fastCorr[iodf].REint = UDREI;
        }
        sbs.fastUpdt[lastIODP][frameTime] = iodf;
    }
}

void decodeL1FastDegr(Trace& trace, GTime frameTime, unsigned char* data)
{
    degrFCTlat = getbitu(data, 14, 4);
    int iodp   = getbitu(data, 18, 2);

    if (l1SBASSatMasks.find(iodp) == l1SBASSatMasks.end())
        return;
    for (int i = 0; i <= 51; i++)
    {
        if (l1SBASSatMasks[iodp].find(i) == l1SBASSatMasks[iodp].end())
            continue;
        SatSys sat     = l1SBASSatMasks[iodp][i];
        degrL1Ind[sat] = getbitu(data, 22 + 4 * i, 4);
        tracepdeex(
            SBAS_DEBUG_TRACE_LEVEL,
            trace,
            "L1 fast degradation %s FDegr = %d\n",
            sat.id().c_str(),
            degrL1Ind[sat]
        );
    }
    fstDegrUpdate = frameTime;
}

void decodeL1SlowDegr(Trace& trace, GTime frameTime, unsigned char* data)
{
    degrL1Slow.tUpdate    = frameTime;
    int i                 = 14;
    degrL1Slow.Brrc       = getbituInc(data, i, 10) * 2e-3;
    degrL1Slow.Cltc_lsb   = getbituInc(data, i, 10) * 2e-3;
    degrL1Slow.Cltc_v1    = getbituInc(data, i, 10) * 5e-5;
    degrL1Slow.Iltc_v1    = getbituInc(data, i, 9) * 1.0;
    degrL1Slow.Cltc_v0    = getbituInc(data, i, 10) * 2e-3;
    degrL1Slow.Iltc_v0    = getbituInc(data, i, 9) * 1.0;
    degrL1Slow.Cgeo_lsb   = getbituInc(data, i, 10) * 5e-4;
    degrL1Slow.Cgeo_v     = getbituInc(data, i, 10) * 5e-5;
    degrL1Slow.Igeo_v     = getbituInc(data, i, 9) * 1.0;
    degrL1Slow.Cer        = getbituInc(data, i, 6) * 0.5;
    degrL1Slow.Ciono_step = getbituInc(data, i, 10) * 1e-3;
    degrL1Slow.Iiono      = getbituInc(data, i, 9) * 1.0;
    degrL1Slow.Ciono_ramp = getbituInc(data, i, 10) * 5e-6;
    degrL1Slow.RSSudre    = getbituInc(data, i, 1) ? true : false;
    degrL1Slow.RSSiono    = getbituInc(data, i, 1) ? true : false;
    degrL1Slow.Ccovar     = getbituInc(data, i, 7) * 0.1;

    if (degrL1Slow.Iiono == 0)
        degrL1Slow.Iiono = 1;

    if (degrL1Slow.Iltc_v0 == 0)
        degrL1Slow.Iltc_v0 = 1;

    tracepdeex(
        SBAS_DEBUG_TRACE_LEVEL,
        trace,
        "L1 slow degradation: %f %f %f\n",
        degrL1Slow.Iiono,
        degrL1Slow.Ciono_step,
        degrL1Slow.Ciono_ramp
    );
}

void decodeL1PosBlock(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data, int& ind)
{
    int    slot1  = getbituInc(data, ind, 6) - 1;
    int    iode1  = getbituInc(data, ind, 8);
    double ecefX1 = getbitsInc(data, ind, 9) * 0.125;
    double ecefY1 = getbitsInc(data, ind, 9) * 0.125;
    double ecefZ1 = getbitsInc(data, ind, 9) * 0.125;
    double dClk_1 = getbitsInc(data, ind, 10) * (P2_31 * CLIGHT);
    int    slot2  = getbituInc(data, ind, 6) - 1;
    int    iode2  = getbituInc(data, ind, 8);
    double ecefX2 = getbitsInc(data, ind, 9) * 0.125;
    double ecefY2 = getbitsInc(data, ind, 9) * 0.125;
    double ecefZ2 = getbitsInc(data, ind, 9) * 0.125;
    double dClk_2 = getbitsInc(data, ind, 10) * (P2_31 * CLIGHT);
    int    iodp   = getbituInc(data, ind, 2);
    tracepdeex(
        SBAS_DEBUG_TRACE_LEVEL,
        trace,
        "L1 slow correction IODP: %1d; slot: %2d\n",
        iodp,
        slot1
    );
    ind++;
    if (l1SBASSatMasks.find(iodp) == l1SBASSatMasks.end())
        return;
    if (l1SBASSatMasks[iodp].find(slot1) != l1SBASSatMasks[iodp].end())
    {
        SatSys sat                    = l1SBASSatMasks[iodp][slot1];
        auto&  sbs                    = nav.satNavMap[sat].currentSBAS;
        sbs.slowCorr[iode1].iodp      = iodp;
        sbs.slowCorr[iode1].iode      = iode1;
        sbs.slowCorr[iode1].dPos[0]   = ecefX1;
        sbs.slowCorr[iode1].dPos[1]   = ecefY1;
        sbs.slowCorr[iode1].dPos[2]   = ecefZ1;
        sbs.slowCorr[iode1].dPos[3]   = dClk_1;
        sbs.slowCorr[iode1].ddPos[0]  = 0.0;
        sbs.slowCorr[iode1].ddPos[1]  = 0.0;
        sbs.slowCorr[iode1].ddPos[2]  = 0.0;
        sbs.slowCorr[iode1].ddPos[3]  = 0.0;
        sbs.slowCorr[iode1].toe       = frameTime;
        sbs.slowCorr[iode1].trec      = frameTime;
        sbs.slowCorr[iode1].Ivalid    = acsConfig.sbsInOpts.prec_aproach ? 240 : 360;
        sbs.slowUpdt[iodp][frameTime] = iode1;
        tracepdeex(
            SBAS_DEBUG_TRACE_LEVEL,
            trace,
            "   %s dPos = (%7.3f,%7.3f,%7.3f,%7.3f); IODE: %d\n",
            sat.id().c_str(),
            ecefX1,
            ecefY1,
            ecefZ1,
            dClk_1,
            iode1
        );
    }
    if (l1SBASSatMasks[iodp].find(slot2) != l1SBASSatMasks[iodp].end())
    {
        SatSys sat                    = l1SBASSatMasks[iodp][slot2];
        auto&  sbs                    = nav.satNavMap[sat].currentSBAS;
        sbs.slowCorr[iode2].iodp      = iodp;
        sbs.slowCorr[iode2].iode      = iode2;
        sbs.slowCorr[iode2].dPos[0]   = ecefX2;
        sbs.slowCorr[iode2].dPos[1]   = ecefY2;
        sbs.slowCorr[iode2].dPos[2]   = ecefZ2;
        sbs.slowCorr[iode2].dPos[3]   = dClk_2;
        sbs.slowCorr[iode2].ddPos[0]  = 0.0;
        sbs.slowCorr[iode2].ddPos[1]  = 0.0;
        sbs.slowCorr[iode2].ddPos[2]  = 0.0;
        sbs.slowCorr[iode2].ddPos[3]  = 0.0;
        sbs.slowCorr[iode2].toe       = frameTime;
        sbs.slowCorr[iode2].trec      = frameTime;
        sbs.slowCorr[iode1].Ivalid    = acsConfig.sbsInOpts.prec_aproach ? 240 : 360;
        sbs.slowUpdt[iodp][frameTime] = iode2;
        tracepdeex(
            SBAS_DEBUG_TRACE_LEVEL,
            trace,
            "   %s dPos = (%7.3f,%7.3f,%7.3f,%7.3f); IODE: %d\n",
            sat.id().c_str(),
            ecefX2,
            ecefY2,
            ecefZ2,
            dClk_2 * CLIGHT,
            iode2
        );
    }
}

void decodeL1VelBlock(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data, int& ind)
{
    int    slot1  = getbituInc(data, ind, 6) - 1;
    int    iode1  = getbituInc(data, ind, 8);
    double ecefX1 = getbitsInc(data, ind, 11) * 0.125;
    double ecefY1 = getbitsInc(data, ind, 11) * 0.125;
    double ecefZ1 = getbitsInc(data, ind, 11) * 0.125;
    double dClk_1 = getbitsInc(data, ind, 11) * (P2_31 * CLIGHT);
    double vel_X1 = getbitsInc(data, ind, 8) * P2_11;
    double vel_Y1 = getbitsInc(data, ind, 8) * P2_11;
    double vel_Z1 = getbitsInc(data, ind, 8) * P2_11;
    double ddClk1 = getbitsInc(data, ind, 8) * (P2_39 * CLIGHT);
    double tod    = getbituInc(data, ind, 13) * 16.0;
    int    iodp   = getbituInc(data, ind, 2);
    if (l1SBASSatMasks.find(iodp) == l1SBASSatMasks.end())
        return;
    if (l1SBASSatMasks[iodp].find(slot1) != l1SBASSatMasks[iodp].end())
    {
        SatSys sat                    = l1SBASSatMasks[iodp][slot1];
        auto&  sbs                    = nav.satNavMap[sat].currentSBAS;
        sbs.slowCorr[iode1].iodp      = iodp;
        sbs.slowCorr[iode1].iode      = iode1;
        sbs.slowCorr[iode1].dPos[0]   = ecefX1;
        sbs.slowCorr[iode1].dPos[1]   = ecefY1;
        sbs.slowCorr[iode1].dPos[2]   = ecefZ1;
        sbs.slowCorr[iode1].dPos[3]   = dClk_1;
        sbs.slowCorr[iode1].ddPos[0]  = vel_X1;
        sbs.slowCorr[iode1].ddPos[1]  = vel_Y1;
        sbs.slowCorr[iode1].ddPos[2]  = vel_Z1;
        sbs.slowCorr[iode1].ddPos[3]  = ddClk1;
        sbs.slowCorr[iode1].toe       = adjustDay(tod, frameTime);
        sbs.slowCorr[iode1].trec      = frameTime;
        sbs.slowCorr[iode1].Ivalid    = acsConfig.sbsInOpts.prec_aproach ? 240 : 360;
        sbs.slowUpdt[iodp][frameTime] = iode1;
    }
}

void decodeL1SlowCorr(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data)
{
    int ind            = 14;
    int velCode        = getbituInc(data, ind, 1);
    degrL1Slow.velCode = velCode;
    if (velCode == 0)
        decodeL1PosBlock(trace, frameTime, nav, data, ind);
    else
        decodeL1VelBlock(trace, frameTime, nav, data, ind);
    velCode = getbituInc(data, ind, 1);
    if (velCode == 0)
        decodeL1PosBlock(trace, frameTime, nav, data, ind);
    else
        decodeL1VelBlock(trace, frameTime, nav, data, ind);
}

void decodeL1MixdCorr(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data)
{
    int iodp = getbitu(data, 110, 2);
    int strt = getbitu(data, 112, 2) * 13;
    int iodf = getbitu(data, 114, 2);

    if (l1SBASSatMasks.find(iodp) == l1SBASSatMasks.end())
        return;
    for (int i = 0; i < 6; i++)
    {
        int slot = i + strt;
        if (l1SBASSatMasks[iodp].find(slot) == l1SBASSatMasks[iodp].end())
            continue;
        SatSys sat = l1SBASSatMasks[iodp][slot];

        int    iFast   = 14 + 12 * i;
        int    dIODF   = -1;
        double dClk    = getbitu(data, iFast, 12) * 0.125;
        double degrIfc = degrL1Out[degrL1Ind[sat]];
        double ddClk   = 0;

        auto& sbs = nav.satNavMap[sat].currentSBAS;
        if (!sbs.fastUpdt[iodp].empty())
        {
            auto it       = sbs.fastUpdt[iodp].begin();
            int  prevIODF = it->second;
            if (sbs.fastCorr.find(prevIODF) != sbs.fastCorr.end())
            {
                double dt = (frameTime - sbs.fastCorr[prevIODF].tFast).to_double();
                if (degrL1Ind[sat] != 0 && dt < degrIfc)
                {
                    ddClk = (dClk - sbs.fastCorr[prevIODF].dClk) / dt;
                    dIODF = (iodf - prevIODF) % 3;
                    if (iodf == 3)
                        dIODF = 3;
                }
            }
        }

        int iUdre = 86 + 4 * i;
        int UDREI = getbitu(data, iUdre, 4);
        if (UDREI > 13)
        {
            sbs.fastCorr.clear();
            sbs.fastUpdt[iodp].clear();
            ddClk = 0.0;
            dIODF = -1;
        }

        sbs.fastUpdt[iodp][frameTime] = iodf;
        sbs.fastCorr[iodf].tFast      = frameTime;
        sbs.fastCorr[iodf].dClk       = dClk;
        sbs.fastCorr[iodf].ddClk      = ddClk;
        sbs.fastCorr[iodf].dIODF      = dIODF;
        sbs.fastCorr[iodf].IValid     = degrIfc;
        sbs.fastCorr[iodf].accDegr    = degrL1Err[degrL1Ind[sat]] * 1e-5;

        tracepdeex(
            SBAS_DEBUG_TRACE_LEVEL,
            trace,
            "L1 Fast Correction %s(%1d); dClk =%7.3f; UDREI = %d;\n",
            sat.id().c_str(),
            iodf,
            dClk,
            UDREI
        );
        if (iodf == 3)
        {
            for (auto& [iod, integr] : sbs.fastCorr)
            {
                integr.tIntg = frameTime;
                integr.REint = UDREI;
            }
        }
        else
        {
            sbs.fastCorr[iodf].tIntg = frameTime;
            sbs.fastCorr[iodf].REint = UDREI;
        }
    }
    int ind     = 120;
    int velCode = getbituInc(data, ind, 1);
    if (velCode == 0)
        decodeL1PosBlock(trace, frameTime, nav, data, ind);
    else
        decodeL1VelBlock(trace, frameTime, nav, data, ind);
}

void decodeL1IonoGrid(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data)
{
    int nband = getbitu(data, 14, 4);
    int band  = getbitu(data, 18, 4);
    int iodi  = getbitu(data, 22, 2);
    int entry = 0;
    tracepdeex(SBAS_DEBUG_TRACE_LEVEL, trace, "\nIGP map for Band %d: ", band);
    for (int ind = 1; ind <= 201; ind++)
        if (getbitu(data, ind + 23, 1))
            addSBASIGP(trace, iodi, band, ind, entry++, frameTime, nband);

    tracepdeex(SBAS_DEBUG_TRACE_LEVEL, trace, " %d points ", entry);
}

void decodeL1IonoGIVD(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data)
{
    int band  = getbitu(data, 14, 4);
    int block = getbitu(data, 18, 4);
    int iodi  = getbitu(data, 217, 2);
    int entry = block * 15;
    int ind   = 22;
    for (int i = 0; i < 15; i++)
    {
        int givdi = getbitu(data, 13 * i + 22, 9);
        int givei = getbitu(data, 13 * i + 31, 4);
        if (givdi == 511)
            givei = 15;
        writeIonoData(trace, iodi, band, entry + i, frameTime, givdi, givei);
    }
}

void decodeL1GEO_Navg(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data, int prn)
{
    Seph   seph = {};
    int    ind  = 22;
    double tod  = getbituInc(data, ind, 13) * 16.0;
    GTime  t0   = adjustDay(tod, frameTime);
    int    ura  = getbituInc(data, ind, 4);
    if (ura == 15)
        return;
    seph.type    = E_NavMsgType::SBAS;
    seph.Sat.sys = E_Sys::SBS;
    seph.Sat.prn = prn - 100;
    seph.t0      = t0;
    seph.tof     = frameTime;
    seph.ura     = sbasGEOUra[ura];
    seph.pos[0]  = getbitsInc(data, ind, 30) * 0.08;
    seph.pos[1]  = getbitsInc(data, ind, 30) * 0.08;
    seph.pos[2]  = getbitsInc(data, ind, 25) * 0.4;
    seph.vel[0]  = getbitsInc(data, ind, 17) * 0.000625;
    seph.vel[1]  = getbitsInc(data, ind, 17) * 0.000625;
    seph.vel[2]  = getbitsInc(data, ind, 18) * 0.004;
    seph.acc[0]  = getbitsInc(data, ind, 10) * 0.0000125;
    seph.acc[1]  = getbitsInc(data, ind, 10) * 0.0000125;
    seph.acc[2]  = getbitsInc(data, ind, 10) * 0.000625;
    seph.af0 = getbitsInc(data, ind, 12) * P2_31;
    seph.af0 = getbitsInc(data, ind, 8) * P2_40;

    nav.sephMap[seph.Sat][seph.type][seph.t0] = seph;
}

void decodeL1UDRERegn(Trace& trace, GTime frameTime, unsigned char* data)
{
    for (auto it = regnMaps.begin(); it != regnMaps.end();)
    {
        auto regMap = it->second;
        if ((frameTime - regMap.tUpdate).to_double() > 86400)
            it = regnMaps.erase(it);
        else
            it++;
    }

    int ind    = 14;
    int iods   = getbituInc(data, ind, 3);
    int nmes   = getbituInc(data, ind, 3) + 1;
    int imes   = getbituInc(data, ind, 3);
    int nreg   = getbituInc(data, ind, 3);
    int prio   = getbituInc(data, ind, 2);
    int inFact = getbituInc(data, ind, 4);
    int ouFact = getbituInc(data, ind, 4);

    regnMaps[iods].totmess    = nmes;
    regnMaps[iods].tUpdate    = frameTime;
    regnMaps[iods].mess[imes] = nreg;

    for (int i = 0; i < nreg; i++)
    {
        int   ireg     = imes * 5 + i;
        auto& reg      = regnMaps[iods].regions[prio][ireg];
        reg.lat1       = getbitsInc(data, ind, 8) * 1.0;
        reg.lon1       = getbitsInc(data, ind, 9) * 1.0;
        reg.lat2       = getbitsInc(data, ind, 8) * 1.0;
        reg.lon2       = getbitsInc(data, ind, 9) * 1.0;
        reg.triangular = getbitsInc(data, ind, 1) == 0 ? true : false;
        reg.in_Factor  = dUDRETable[inFact];
        reg.outFactor  = dUDRETable[ouFact];
        reg.time       = frameTime;
        tracepdeex(
            SBAS_DEBUG_TRACE_LEVEL,
            trace,
            "L1 dUdre region %d (%d): lat: %3.0f %3.0f;  lon: %4.0f %4.0f; %1d; dUDRe: %.2f %.2f\n",
            ireg,
            prio,
            reg.lat1,
            reg.lat2,
            reg.lon1,
            reg.lon2,
            reg.triangular ? 3 : 4,
            reg.in_Factor,
            reg.outFactor
        );
    }
}

void decodeL1UDRECovr(Trace& trace, GTime frameTime, Navigation& nav, unsigned char* data)
{
    int i    = 14;
    int iodp = getbituInc(data, i, 2);
    if (l1SBASSatMasks.find(iodp) == l1SBASSatMasks.end())
        return;
    int slot1 = getbituInc(data, i, 6);
    if (l1SBASSatMasks[iodp].find(slot1) == l1SBASSatMasks[iodp].end())
    {
        double   scale = 2 ^ (getbituInc(data, i, 3) - 5);
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

        SatSys sat                     = l1SBASSatMasks[iodp][slot1];
        sbasUdreCov[iodp][sat].REScale = scale;
        sbasUdreCov[iodp][sat].covr    = C;
        sbasUdreCov[iodp][sat].toe     = frameTime;
        sbasUdreCov[iodp][sat].Ivalid  = acsConfig.sbsInOpts.prec_aproach ? 240 : 360;
    }
    else
        i += 99;

    int slot2 = getbituInc(data, i, 6);
    if (l1SBASSatMasks[iodp].find(slot2) == l1SBASSatMasks[iodp].end())
    {
        double   scale = 2 ^ (getbituInc(data, i, 3) - 5);
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

        SatSys sat                     = l1SBASSatMasks[iodp][slot1];
        sbasUdreCov[iodp][sat].REScale = scale;
        sbasUdreCov[iodp][sat].covr    = C;
        sbasUdreCov[iodp][sat].toe     = frameTime;
        sbasUdreCov[iodp][sat].Ivalid  = acsConfig.sbsInOpts.prec_aproach ? 240 : 360;
    }
}

void decodeSBASMessage(Trace& trace, GTime time, SBASMessage& mess, Navigation& nav)
{
    int type = mess.type;
    if (type == 0)
        type = acsConfig.sbsInOpts.mt0;

    checkForType0(time, type);
    tracepdeex(SBAS_DEBUG_TRACE_LEVEL, trace, "Decoding SBAS message type %2d\n", type);

    switch (type)
    {
        case 1:
            decodeL1SBASMask(trace, mess.data);
            break;  // Satellite mask
        case 2:
            decodeL1FastCorr(trace, time, nav, mess.data, 0);
            break;  // Fast Corrections  1-13
        case 3:
            decodeL1FastCorr(trace, time, nav, mess.data, 13);
            break;  // Fast Corrections 14-26
        case 4:
            decodeL1FastCorr(trace, time, nav, mess.data, 26);
            break;  // Fast Corrections 27-39
        case 5:
            decodeL1FastCorr(trace, time, nav, mess.data, 39);
            break;  // Fast Corrections 40-51
        case 6:
            decodeL1UDREIall(trace, time, nav, mess.data);
            break;  // UDREI all satellites
        case 7:
            decodeL1FastDegr(trace, time, mess.data);
            break;  // Fast Correction degradation
        case 9:
            decodeL1GEO_Navg(trace, time, nav, mess.data, mess.prn);
            break;  // GEO satellite
        case 10:
            decodeL1SlowDegr(trace, time, mess.data);
            break;  // Slow Correction degradation
        case 18:
            decodeL1IonoGrid(trace, time, nav, mess.data);
            break;  // Ionosphere Grid points definition
        case 24:
            decodeL1MixdCorr(trace, time, nav, mess.data);
            break;  // Fast & Slow Correction
        case 25:
            decodeL1SlowCorr(trace, time, nav, mess.data);
            break;  // Slow corrections
        case 26:
            decodeL1IonoGIVD(trace, time, nav, mess.data);
            break;  // Ionosphere Correction at IGPs
        case 27:
            decodeL1UDRERegn(trace, time, mess.data);
            break;  // UDRE Region definition
        case 28:
            decodeL1UDRECovr(trace, time, nav, mess.data);
            break;  // UDRE covariance matrix
        case 62:
        case 63:
            break;
        default:
            tracepdeex(5, std::cout, "\nSBAS_MT%02d, not supported", type);
            break;
    }
    return;
}

bool recvInRegion(SBASRegn& reg, double latDeg, double lonDeg)
{
    double intLat = reg.lat2 - reg.lat1;
    double dLat   = (latDeg - reg.lat1) / intLat;
    if (0 < dLat || dLat > 1)
        return false;

    double intLon = reg.lon1 - reg.lon2;
    if (intLon > 180)
        intLon -= 360;
    if (intLon < -180)
        intLon += 360;

    double dLon_ = lonDeg - reg.lon2;
    if (dLon_ > 180)
        dLon_ -= 360;
    if (dLon_ < -180)
        dLon_ += 360;

    double dLon = dLon_ / intLon;

    if (0 < dLon || dLon > 1)
        return false;

    if (!reg.triangular)
        return true;

    if ((1 - dLat - dLon) < 0)
        return true;

    return false;
}

double rangeErrFromReg(Trace& trace, GTime time, Vector3d& rRec)
{
    double      dt = 86401;
    SBASRegnMap completeMap;
    for (auto& [iods, regMap] : regnMaps)
    {
        if (regMap.mess.size() < regMap.totmess)
            continue;
        double dtIods = (time - regMap.tUpdate).to_double();
        if (dtIods > dt)
        {
            dt          = dtIods;
            completeMap = regMap;
        }
    }

    if (dt > 86400)
        return 1;

    VectorPos pos    = ecef2pos(rRec);
    double    latDeg = pos.latDeg();
    double    lonDeg = pos.lonDeg();

    int    highPrio     = -1;
    int    highPrio_out = -1;
    double mindUDRE     = 100;
    double mindUDRE_out = 100;
    for (auto& [prio, regList] : completeMap.regions)
        for (auto& [ireg, reg] : regList)
        {
            if ((time - reg.time).to_double() > 86400)
                continue;
            if (recvInRegion(reg, latDeg, lonDeg))
            {
                if (prio > highPrio)
                {
                    highPrio = prio;
                    mindUDRE = reg.in_Factor;
                }
                else if (prio == highPrio && mindUDRE > reg.in_Factor)
                    mindUDRE = reg.in_Factor;
            }
            else if (highPrio < 0)
            {
                if (prio > highPrio_out)
                {
                    highPrio_out = prio;
                    mindUDRE_out = reg.outFactor;
                }
                else if (prio == highPrio_out && mindUDRE_out > reg.outFactor)
                    mindUDRE_out = reg.outFactor;
            }
        }

    if (highPrio >= 0)
        return mindUDRE;

    if (highPrio_out >= 0)
        return mindUDRE_out;

    return 1;
}

double estimateSBASVar(
    Trace&    trace,
    GTime     time,
    SatSys    sat,
    Vector3d& rRec,
    Vector3d& rSat,
    SBASFast& sbsFast,
    SBASSlow& sbasSlow
)
{
    int iodp = sbsFast.iodp;
    if (sbasSlow.iodp != iodp)
        return -2;

    int UDREI = sbsFast.REint;
    if (UDREI < 0)
        return -2;
    if (UDREI == 14)
        return -1;
    if (UDREI > 15)
        return -2;
    double sig2UDRE = sig2UDREI[UDREI];

    double dUDRE = 1;
    if (sbasUdreCov.find(iodp) != sbasUdreCov.end() &&
        sbasUdreCov[iodp].find(sat) != sbasUdreCov[iodp].end())
        dUDRE = rangeErrFromCov(trace, time, iodp, sat, rRec, rSat, degrL1Slow.Ccovar);
    else if (!regnMaps.empty())
        dUDRE = rangeErrFromReg(trace, time, rRec);

    double var = SQR(dUDRE * sqrt(sig2UDRE) + 8);

    double maxDegrAge = 360;
    if (acsConfig.sbsInOpts.prec_aproach)
        maxDegrAge = 240;

    if ((!fstDegrUpdate) || (time - fstDegrUpdate).to_double() > maxDegrAge)
        return var;

    if ((!degrL1Slow.tUpdate) || (time - degrL1Slow.tUpdate).to_double() > maxDegrAge)
        return var;

    double dtFast = (time - sbsFast.tFast).to_double() + degrFCTlat;
    double eFc    = sbsFast.accDegr * dtFast * dtFast / 2;

    double eRrc = 0;
    if (sbsFast.accDegr > 0)
        switch (sbsFast.dIODF)
        {
            case 0:
            case 2:
                eRrc = (sbsFast.accDegr * sbsFast.IValid / 4 + degrL1Slow.Brrc / sbsFast.dt) *
                       (time - sbsFast.tFast).to_double();
                break;
            case 3:
                double ddt = fabs(sbsFast.dt - sbsFast.IValid / 2);
                if (ddt > 0)
                    eRrc = (sbsFast.accDegr * ddt / 2 + degrL1Slow.Brrc / sbsFast.dt) *
                           (time - sbsFast.tFast).to_double();
                break;
        }

    double eLtc  = 0;
    double dtLtc = (time - sbasSlow.toe).to_double();
    if (degrL1Slow.velCode == 1)
    {
        if (dtLtc < 0)
            eLtc = degrL1Slow.Cltc_lsb - degrL1Slow.Cltc_v1 * dtLtc;
        if (dtLtc > degrL1Slow.Iltc_v1)
            eLtc = degrL1Slow.Cltc_lsb + degrL1Slow.Cltc_v1 * (dtLtc - degrL1Slow.Iltc_v1);
    }
    else
        eLtc = degrL1Slow.Cltc_v0 * floor(dtLtc / degrL1Slow.Iltc_v0);

    double eEr = 0;
    if (dtLtc > maxDegrAge || (time - sbsFast.tFast).to_double() > sbsFast.IValid)
    {
        if (acsConfig.sbsInOpts.prec_aproach)
            return -2;
        else
            eEr = degrL1Slow.Cer;
    }

    if (degrL1Slow.RSSudre)
        var = sig2UDRE * SQR(dUDRE) + SQR(eFc) + SQR(eLtc) + SQR(eEr);
    else
        var = SQR(sqrt(sig2UDRE) * dUDRE + eFc + eRrc + eLtc + eEr);
    return var;
}

double estimateIonoVar(GTime time, GTime givdTime, double sigGIVE)
{
    if (sigGIVE < 0)
        return -1;

    if (degrL1Slow.Iiono < 0)
        return -1;

    if ((time - degrL1Slow.tUpdate) > SBAS_DEGR_OUTAGE)
        return -1;

    double dt = (time - givdTime).to_double();
    double dGIVE =
        degrL1Slow.Ciono_step * floor(dt / degrL1Slow.Iiono) + degrL1Slow.Ciono_ramp * dt;

    double var = -1;
    if (degrL1Slow.RSSiono)
        var = sigGIVE * sigGIVE + dGIVE * dGIVE;
    else
        var = (sigGIVE + dGIVE) * (sigGIVE + dGIVE);

    return var;
}