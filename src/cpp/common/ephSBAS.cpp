
#include "common/acsConfig.hpp"
#include "common/eigenIncluder.hpp"
#include "common/ephemeris.hpp"
#include "common/gTime.hpp"
#include "common/navigation.hpp"
#include "sbas/sbas.hpp"

bool dfmc2Pos(Trace& trace, GTime time, SatPos& satPos, Navigation& nav)
{
    return false;
}

bool satPosSBAS(Trace& trace, GTime time, GTime teph, SatPos& satPos, Navigation& nav)
{
    loadSBASdata(trace, teph, nav);
    SBASMaps& sbsMaps     = satPos.satNav_ptr->currentSBAS;
    SatSys&   Sat         = satPos.Sat;
    Vector3d& rSat        = satPos.rSatApc;
    Vector3d& satVel      = satPos.satVel;
    double&   satClk      = satPos.satClk;
    double&   satClkVel   = satPos.satClkVel;
    bool&     ephPosValid = satPos.ephPosValid;
    bool&     ephClkValid = satPos.ephClkValid;
    int&      iodeClk     = satPos.iodeClk;
    int&      iodePos     = satPos.iodePos;
    double&   posVar      = satPos.posVar;
    double&   clkVar      = satPos.satClkVar;

    ephPosValid = false;
    ephClkValid = false;

    double maxdt = 30;
    switch (acsConfig.sbsInOpts.freq)
    {
        case 1:
            maxdt = acsConfig.sbsInOpts.prec_aproach ? 12 : 16;
            break;
        case 5:
            maxdt = acsConfig.sbsInOpts.prec_aproach ? 12 : 16;
            break;
        default:
            return false;
    }

    bool     pass = false;
    SBASIntg sbsInt;
    for (auto& [iodm, intData] : sbsMaps.Integrity)
    {
        if (fabs((time - intData.trec).to_double()) > maxdt)
            continue;
        pass   = true;
        sbsInt = intData;
    }
    if (!pass)
    {
        tracepdeex(4, trace, "\nSBASEPH No Integrity data for %s", Sat.id().c_str());
        return false;
    }

    pass        = false;
    int selIode = -1;
    for (auto& [updtTime, iode] : sbsMaps.corrUpdt)
    {
        auto& slowCorr = sbsMaps.slowCorr[iode];

        if (slowCorr.Ivalid < 0)
        {
            continue;
        }

        if (fabs((time - slowCorr.trec).to_double()) > slowCorr.Ivalid)
        {
            continue;
        }

        pass = true;
        pass &= satPosBroadcast(trace, time, teph, satPos, nav, iode);
        pass &= satClkBroadcast(trace, time, teph, satPos, nav, iode);
        if (pass)
        {
            selIode = iode;
            break;
        }
    }
    if (!pass)
    {
        tracepdeex(4, trace, "\nSBASEPH No Correction data for %s", Sat.id().c_str());
        return false;
    }
    tracepdeex(
        5,
        trace,
        "\nBRDCEPH %s    %s    %13.3f %13.3f %13.3f %13.3f %4d",
        time.to_string().c_str(),
        Sat.id().c_str(),
        rSat[0],
        rSat[1],
        rSat[2],
        satClk * CLIGHT,
        selIode
    );

    switch (acsConfig.sbsInOpts.freq)
    {
        case 5:
            clkVar = estimateDFMCVar(trace, time, satPos, sbsInt) / SQR(CLIGHT);
            break;
        default:
            return false;
    }
    posVar = 0.0;
    if (clkVar < 0)
    {
        tracepdeex(4, trace, "\nSBASEPH Unknown Vairance for %s", Sat.id().c_str());
        return false;
    }
    auto&  sbs = sbsMaps.slowCorr[selIode];
    double dt  = (time - sbs.toe).to_double();
    for (int i = 0; i < 3; i++)
    {
        rSat[i] += sbs.dPos[i] + dt * sbs.ddPos[i];
        satVel[i] += sbs.ddPos[i];
    }
    satClk += (sbs.dPos[3] + dt * sbs.ddPos[3]) / CLIGHT;
    satClkVel += (sbs.ddPos[3]) / CLIGHT;

    if (Sat.sys == E_Sys::GPS && acsConfig.sbsInOpts.use_do259)
    {
        Eph* eph_ptr = seleph<Eph>(trace, time, Sat, E_NavMsgType::LNAV, selIode, nav);
        if (eph_ptr == nullptr)
            return false;
        satClk -= eph_ptr->tgd[0];
    }

    tracepdeex(
        5,
        trace,
        "\nSBASEPH %s    %s    %13.3f %13.3f %13.3f %13.3f %4d",
        time.to_string().c_str(),
        Sat.id().c_str(),
        rSat[0],
        rSat[1],
        rSat[2],
        satClk * CLIGHT,
        selIode
    );

    ephPosValid = true;
    ephClkValid = true;

    iodeClk = selIode;
    iodePos = selIode;

    return true;
}
