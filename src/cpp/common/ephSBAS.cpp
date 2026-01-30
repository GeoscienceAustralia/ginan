
#include "common/acsConfig.hpp"
#include "common/eigenIncluder.hpp"
#include "common/ephemeris.hpp"
#include "common/gTime.hpp"
#include "common/navigation.hpp"
#include "sbas/sbas.hpp"

bool satPosSBAS(Trace& trace, GTime time, GTime teph, SatPos& satPos, Navigation& nav)
{
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

    double maxdt = acsConfig.sbsInOpts.prec_aproach ? 12 : 18;

    int      selIODP = -1;
    int      selIODF = -1;
    SBASFast sbsFast;
    for (auto& [iodp, fastUMap] : sbsMaps.fastUpdt)
        for (auto& [updtTime, iodf] : fastUMap)
        {
            if (fabs((time - updtTime).to_double()) > maxdt)
                continue;
            if (sbsMaps.fastCorr.find(iodf) == sbsMaps.fastCorr.end())
                continue;
            if ((time - sbsMaps.fastCorr[iodf].tIntg).to_double() > maxdt)
                continue;

            if (acsConfig.sbsInOpts.freq == 1 &&
                (time - sbsMaps.fastCorr[iodf].tFast).to_double() > sbsMaps.fastCorr[iodf].IValid)
                continue;

            sbsFast = sbsMaps.fastCorr[iodf];
            selIODP = iodp;
            selIODF = iodf;
            break;
        }
    if (selIODP < 0)
    {
        tracepdeex(4, trace, "\nSBASEPH No Integrity data for %s", Sat.id().c_str());
        return false;
    }

    int selIode = -1;
    for (auto& [updtTime, iode] : sbsMaps.slowUpdt[selIODP])
    {
        auto& slowCorr = sbsMaps.slowCorr[iode];

        if (slowCorr.iodp != selIODP)
            continue;

        if (slowCorr.Ivalid < 0)
            continue;
        if (fabs((time - slowCorr.trec).to_double()) > slowCorr.Ivalid)
            continue;

        bool pass = true;
        pass &= satPosBroadcast(trace, time, teph, satPos, nav, iode);
        pass &= satClkBroadcast(trace, time, teph, satPos, nav, iode);
        if (pass)
        {
            selIode = iode;
            break;
        }
    }
    if (selIode < 0)
    {
        tracepdeex(4, trace, "\nSBASEPH No Correction data for %s", Sat.id().c_str());
        return false;
    }

    // SBAS variance should be treated separately
    posVar = 0.0;
    if (acsConfig.sbsInOpts.pvs_on_dfmc)
    {
        clkVar = 2.5E-6;
    }
    else
    {
        clkVar                    = 1E4;
        usedSBASIODMap[Sat].tUsed = time;
        usedSBASIODMap[Sat].iodp  = selIODP;
        usedSBASIODMap[Sat].iodf  = selIODF;
        usedSBASIODMap[Sat].iode  = selIode;
    }

    tracepdeex(
        2,
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

    auto&  sbsSlow = sbsMaps.slowCorr[selIode];
    double dt      = (time - sbsSlow.toe).to_double();
    for (int i = 0; i < 3; i++)
    {
        rSat[i] += sbsSlow.dPos[i] + dt * sbsSlow.ddPos[i];
        satVel[i] += sbsSlow.ddPos[i];
    }
    satClk += (sbsSlow.dPos[3] + dt * sbsSlow.ddPos[3]) / CLIGHT;
    satClkVel += (sbsSlow.ddPos[3]) / CLIGHT;

    tracepdeex(
        2,
        trace,
        "\n %s SC %.3f from %s",
        Sat.id().c_str(),
        sbsSlow.dPos[3],
        sbsSlow.toe.to_string()
    );

    if (acsConfig.sbsInOpts.freq == 1)
    {
        dt = (time - sbsFast.tFast).to_double();
        satClk += (sbsFast.dClk + dt * sbsFast.ddClk) / CLIGHT;
        satClkVel += (sbsFast.ddClk) / CLIGHT;

        tracepdeex(
            2,
            trace,
            "\n %s FC %.3f from %s",
            Sat.id().c_str(),
            sbsFast.dClk + dt * sbsFast.ddClk,
            sbsFast.tFast.to_string()
        );
    }

    if (Sat.sys == E_Sys::GPS)
    {
        if (acsConfig.sbsInOpts.freq == 1 || acsConfig.sbsInOpts.use_do259)
        {
            Eph* eph_ptr = seleph<Eph>(trace, time, Sat, E_NavMsgType::LNAV, selIode, nav);
            if (eph_ptr == nullptr)
                return false;
            satClk -= eph_ptr->tgd[0];
            tracepdeex(2, trace, "\n %s tgd %.3f", Sat.id().c_str(), -eph_ptr->tgd[0] * CLIGHT);
        }
    }

    tracepdeex(
        2,
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
