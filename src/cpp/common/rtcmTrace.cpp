// #pragma GCC optimize ("O0")

#include "common/rtcmTrace.hpp"
#include <boost/json.hpp>
#include "common/common.hpp"
#include "common/ephemeris.hpp"
#include "common/ssr.hpp"

map<RtcmMessageType, E_Sys> rtcmMessageSystemMap = {
    {RtcmMessageType::GPS_EPHEMERIS, E_Sys::GPS},
    {RtcmMessageType::GLO_EPHEMERIS, E_Sys::GLO},
    {RtcmMessageType::BDS_EPHEMERIS, E_Sys::BDS},
    {RtcmMessageType::QZS_EPHEMERIS, E_Sys::QZS},
    {RtcmMessageType::GAL_FNAV_EPHEMERIS, E_Sys::GAL},
    {RtcmMessageType::GAL_INAV_EPHEMERIS, E_Sys::GAL},
    {RtcmMessageType::GPS_SSR_ORB_CORR, E_Sys::GPS},
    {RtcmMessageType::GPS_SSR_CLK_CORR, E_Sys::GPS},
    {RtcmMessageType::GPS_SSR_CODE_BIAS, E_Sys::GPS},
    {RtcmMessageType::GPS_SSR_COMB_CORR, E_Sys::GPS},
    {RtcmMessageType::GPS_SSR_URA, E_Sys::GPS},
    {RtcmMessageType::GPS_SSR_HR_CLK_CORR, E_Sys::GPS},
    {RtcmMessageType::GLO_SSR_ORB_CORR, E_Sys::GLO},
    {RtcmMessageType::GLO_SSR_CLK_CORR, E_Sys::GLO},
    {RtcmMessageType::GLO_SSR_CODE_BIAS, E_Sys::GLO},
    {RtcmMessageType::GLO_SSR_COMB_CORR, E_Sys::GLO},
    {RtcmMessageType::GLO_SSR_URA, E_Sys::GLO},
    {RtcmMessageType::GLO_SSR_HR_CLK_CORR, E_Sys::GLO},
    {RtcmMessageType::MSM4_GPS, E_Sys::GPS},
    {RtcmMessageType::MSM5_GPS, E_Sys::GPS},
    {RtcmMessageType::MSM6_GPS, E_Sys::GPS},
    {RtcmMessageType::MSM7_GPS, E_Sys::GPS},
    {RtcmMessageType::MSM4_GLONASS, E_Sys::GLO},
    {RtcmMessageType::MSM5_GLONASS, E_Sys::GLO},
    {RtcmMessageType::MSM6_GLONASS, E_Sys::GLO},
    {RtcmMessageType::MSM7_GLONASS, E_Sys::GLO},
    {RtcmMessageType::MSM4_GALILEO, E_Sys::GAL},
    {RtcmMessageType::MSM5_GALILEO, E_Sys::GAL},
    {RtcmMessageType::MSM6_GALILEO, E_Sys::GAL},
    {RtcmMessageType::MSM7_GALILEO, E_Sys::GAL},
    {RtcmMessageType::MSM4_QZSS, E_Sys::QZS},
    {RtcmMessageType::MSM5_QZSS, E_Sys::QZS},
    {RtcmMessageType::MSM6_QZSS, E_Sys::QZS},
    {RtcmMessageType::MSM7_QZSS, E_Sys::QZS},
    {RtcmMessageType::MSM4_BEIDOU, E_Sys::BDS},
    {RtcmMessageType::MSM5_BEIDOU, E_Sys::BDS},
    {RtcmMessageType::MSM6_BEIDOU, E_Sys::BDS},
    {RtcmMessageType::MSM7_BEIDOU, E_Sys::BDS},
    {RtcmMessageType::GAL_SSR_ORB_CORR, E_Sys::GAL},
    {RtcmMessageType::GAL_SSR_CLK_CORR, E_Sys::GAL},
    {RtcmMessageType::GAL_SSR_CODE_BIAS, E_Sys::GAL},
    {RtcmMessageType::GAL_SSR_COMB_CORR, E_Sys::GAL},
    {RtcmMessageType::GAL_SSR_URA, E_Sys::GAL},
    {RtcmMessageType::GAL_SSR_HR_CLK_CORR, E_Sys::GAL},
    {RtcmMessageType::QZS_SSR_ORB_CORR, E_Sys::QZS},
    {RtcmMessageType::QZS_SSR_CLK_CORR, E_Sys::QZS},
    {RtcmMessageType::QZS_SSR_CODE_BIAS, E_Sys::QZS},
    {RtcmMessageType::QZS_SSR_COMB_CORR, E_Sys::QZS},
    {RtcmMessageType::QZS_SSR_URA, E_Sys::QZS},
    {RtcmMessageType::QZS_SSR_HR_CLK_CORR, E_Sys::QZS},
    {RtcmMessageType::SBS_SSR_ORB_CORR, E_Sys::SBS},
    {RtcmMessageType::SBS_SSR_CLK_CORR, E_Sys::SBS},
    {RtcmMessageType::SBS_SSR_CODE_BIAS, E_Sys::SBS},
    {RtcmMessageType::SBS_SSR_COMB_CORR, E_Sys::SBS},
    {RtcmMessageType::SBS_SSR_URA, E_Sys::SBS},
    {RtcmMessageType::SBS_SSR_HR_CLK_CORR, E_Sys::SBS},
    {RtcmMessageType::BDS_SSR_ORB_CORR, E_Sys::BDS},
    {RtcmMessageType::BDS_SSR_CLK_CORR, E_Sys::BDS},
    {RtcmMessageType::BDS_SSR_CODE_BIAS, E_Sys::BDS},
    {RtcmMessageType::BDS_SSR_COMB_CORR, E_Sys::BDS},
    {RtcmMessageType::BDS_SSR_URA, E_Sys::BDS},
    {RtcmMessageType::BDS_SSR_HR_CLK_CORR, E_Sys::BDS},
    {RtcmMessageType::GPS_SSR_PHASE_BIAS, E_Sys::GPS},
    {RtcmMessageType::GLO_SSR_PHASE_BIAS, E_Sys::GLO},
    {RtcmMessageType::GAL_SSR_PHASE_BIAS, E_Sys::GAL},
    {RtcmMessageType::QZS_SSR_PHASE_BIAS, E_Sys::QZS},
    {RtcmMessageType::SBS_SSR_PHASE_BIAS, E_Sys::SBS},
    {RtcmMessageType::BDS_SSR_PHASE_BIAS, E_Sys::BDS},
    {RtcmMessageType::COMPACT_SSR, E_Sys::SUPPORTED},
    {RtcmMessageType::IGS_SSR, E_Sys::SUPPORTED}
};

void RtcmTrace::traceSsrEph(RtcmMessageType messCode, SatSys Sat, SSREph& ssrEph)
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    GTime nearTime = timeGet();

    boost::json::object doc;
    doc["type"]                     = "ssrEph";
    doc["Mountpoint"]               = rtcmMountpoint;
    doc["MessageNumber"]            = static_cast<int>(messCode);
    doc["MessageType"]              = enum_to_string(messCode);
    doc["ReceivedSentTimeGPST"]     = nearTime.to_string();
    doc["EpochTimeGPST"]            = ssrEph.ssrMeta.receivedTime.to_string();
    doc["ReferenceTimeGPST"]        = ssrEph.t0.to_string();
    doc["EpochTime1s"]              = ssrEph.ssrMeta.epochTime1s;
    doc["SSRUpdateIntervalSec"]     = ssrEph.udi;
    doc["SSRUpdateIntervalIndex"]   = ssrEph.ssrMeta.updateIntIndex;
    doc["MultipleMessageIndicator"] = ssrEph.ssrMeta.multipleMessage;
    doc["SatReferenceDatum"] =
        static_cast<int>(ssrEph.ssrMeta.referenceDatum);  // 0 = ITRF, 1 = Regional
    doc["IODSSR"]             = ssrEph.iod;
    doc["SSRProviderID"]      = static_cast<int>(ssrEph.ssrMeta.provider);
    doc["SSRSolutionID"]      = static_cast<int>(ssrEph.ssrMeta.solution);
    doc["Sat"]                = Sat.id();
    doc["IODE"]               = ssrEph.iode;
    doc["IODCRC"]             = ssrEph.iodcrc;
    doc["DeltaRadial"]        = ssrEph.deph[0];
    doc["DeltaAlongTrack"]    = ssrEph.deph[1];
    doc["DeltaCrossTrack"]    = ssrEph.deph[2];
    doc["DotDeltaRadial"]     = ssrEph.ddeph[0];
    doc["DotDeltaAlongTrack"] = ssrEph.ddeph[1];
    doc["DotDeltaCrossTrack"] = ssrEph.ddeph[2];

    fout << boost::json::serialize(doc) << "\n";
}

void RtcmTrace::traceSsrClk(RtcmMessageType messCode, SatSys Sat, SSRClk& ssrClk)
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    GTime nearTime = timeGet();

    boost::json::object doc;
    doc["type"]                     = "ssrClk";
    doc["Mountpoint"]               = rtcmMountpoint;
    doc["MessageNumber"]            = static_cast<int>(messCode);
    doc["MessageType"]              = enum_to_string(messCode);
    doc["ReceivedSentTimeGPST"]     = nearTime.to_string();
    doc["EpochTimeGPST"]            = ssrClk.ssrMeta.receivedTime.to_string();
    doc["ReferenceTimeGPST"]        = ssrClk.t0.to_string();
    doc["EpochTime1s"]              = ssrClk.ssrMeta.epochTime1s;
    doc["SSRUpdateIntervalSec"]     = ssrClk.udi;
    doc["SSRUpdateIntervalIndex"]   = ssrClk.ssrMeta.updateIntIndex;
    doc["MultipleMessageIndicator"] = ssrClk.ssrMeta.multipleMessage;
    doc["SatReferenceDatum"]        = static_cast<int>(ssrClk.ssrMeta.referenceDatum
    );  // 0 = ITRF, 1 = Regional	// could be combined corrections
    doc["IODSSR"]                   = ssrClk.iod;
    doc["SSRProviderID"]            = static_cast<int>(ssrClk.ssrMeta.provider);
    doc["SSRSolutionID"]            = static_cast<int>(ssrClk.ssrMeta.solution);
    doc["Sat"]                      = Sat.id();
    doc["DeltaClockC0"]             = ssrClk.dclk[0];
    doc["DeltaClockC1"]             = ssrClk.dclk[1];
    doc["DeltaClockC2"]             = ssrClk.dclk[2];

    fout << boost::json::serialize(doc) << "\n";
}

void RtcmTrace::traceSsrUra(RtcmMessageType messCode, SatSys Sat, SSRUra& ssrUra)
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    GTime nearTime = timeGet();

    boost::json::object doc;
    doc["type"]                     = "ssrURA";
    doc["Mountpoint"]               = rtcmMountpoint;
    doc["MessageNumber"]            = static_cast<int>(messCode);
    doc["MessageType"]              = enum_to_string(messCode);
    doc["ReceivedSentTimeGPST"]     = nearTime.to_string();
    doc["EpochTimeGPST"]            = ssrUra.ssrMeta.receivedTime.to_string();
    doc["ReferenceTimeGPST"]        = ssrUra.t0.to_string();
    doc["EpochTime1s"]              = ssrUra.ssrMeta.epochTime1s;
    doc["SSRUpdateIntervalSec"]     = ssrUra.udi;
    doc["SSRUpdateIntervalIndex"]   = ssrUra.ssrMeta.updateIntIndex;
    doc["MultipleMessageIndicator"] = ssrUra.ssrMeta.multipleMessage;
    doc["IODSSR"]                   = ssrUra.iod;
    doc["SSRProviderID"]            = (int)ssrUra.ssrMeta.provider;
    doc["SSRSolutionID"]            = (int)ssrUra.ssrMeta.solution;
    doc["Sat"]                      = Sat.id();
    doc["SSRURA"]                   = ssrUra.ura;

    fout << boost::json::serialize(doc) << "\n";
}

void RtcmTrace::traceSsrHRClk(RtcmMessageType messCode, SatSys Sat, SSRHRClk& SsrHRClk)
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    GTime nearTime = timeGet();

    boost::json::object doc;
    doc["type"]                     = "ssrHRClk";
    doc["Mountpoint"]               = rtcmMountpoint;
    doc["MessageNumber"]            = static_cast<int>(messCode);
    doc["MessageType"]              = enum_to_string(messCode);
    doc["ReceivedSentTimeGPST"]     = nearTime.to_string();
    doc["EpochTimeGPST"]            = SsrHRClk.ssrMeta.receivedTime.to_string();
    doc["ReferenceTimeGPST"]        = SsrHRClk.t0.to_string();
    doc["EpochTime1s"]              = SsrHRClk.ssrMeta.epochTime1s;
    doc["SSRUpdateIntervalSec"]     = SsrHRClk.udi;
    doc["SSRUpdateIntervalIndex"]   = SsrHRClk.ssrMeta.updateIntIndex;
    doc["MultipleMessageIndicator"] = SsrHRClk.ssrMeta.multipleMessage;
    doc["IODSSR"]                   = SsrHRClk.iod;
    doc["SSRProviderID"]            = (int)SsrHRClk.ssrMeta.provider;
    doc["SSRSolutionID"]            = (int)SsrHRClk.ssrMeta.solution;
    doc["Sat"]                      = Sat.id();
    doc["HighRateClockCorr"]        = SsrHRClk.hrclk;

    fout << boost::json::serialize(doc) << "\n";
}

void RtcmTrace::traceSsrCodeBias(
    RtcmMessageType messCode,
    SatSys          Sat,
    E_ObsCode       code,
    SSRCodeBias&    ssrBias
)
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    GTime nearTime = timeGet();

    boost::json::object doc;
    doc["type"]                     = "ssrCodeBias";
    doc["Mountpoint"]               = rtcmMountpoint;
    doc["MessageNumber"]            = static_cast<int>(messCode);
    doc["MessageType"]              = enum_to_string(messCode);
    doc["ReceivedSentTimeGPST"]     = nearTime.to_string();
    doc["EpochTimeGPST"]            = ssrBias.ssrMeta.receivedTime.to_string();
    doc["ReferenceTimeGPST"]        = ssrBias.t0.to_string();
    doc["EpochTime1s"]              = ssrBias.ssrMeta.epochTime1s;
    doc["SSRUpdateIntervalSec"]     = ssrBias.udi;
    doc["SSRUpdateIntervalIndex"]   = ssrBias.ssrMeta.updateIntIndex;
    doc["MultipleMessageIndicator"] = ssrBias.ssrMeta.multipleMessage;
    doc["IODSSR"]                   = ssrBias.iod;
    doc["SSRProviderID"]            = (int)ssrBias.ssrMeta.provider;
    doc["SSRSolutionID"]            = (int)ssrBias.ssrMeta.solution;
    doc["Sat"]                      = Sat.id();
    doc["Code"]                     = enum_to_string(code);
    doc["Bias"]                     = ssrBias.obsCodeBiasMap[code].bias;

    fout << boost::json::serialize(doc) << "\n";
}

void RtcmTrace::traceSsrPhasBias(
    RtcmMessageType messCode,
    SatSys          Sat,
    E_ObsCode       code,
    SSRPhasBias&    ssrBias
)
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    GTime nearTime = timeGet();

    boost::json::object doc;
    doc["type"]                      = "ssrPhasBias";
    doc["Mountpoint"]                = rtcmMountpoint;
    doc["MessageNumber"]             = static_cast<int>(messCode);
    doc["MessageType"]               = enum_to_string(messCode);
    doc["ReceivedSentTimeGPST"]      = nearTime.to_string();
    doc["EpochTimeGPST"]             = ssrBias.ssrMeta.receivedTime.to_string();
    doc["ReferenceTimeGPST"]         = ssrBias.t0.to_string();
    doc["EpochTime1s"]               = ssrBias.ssrMeta.epochTime1s;
    doc["SSRUpdateIntervalSec"]      = ssrBias.udi;
    doc["SSRUpdateIntervalIndex"]    = ssrBias.ssrMeta.updateIntIndex;
    doc["MultipleMessageIndicator"]  = ssrBias.ssrMeta.multipleMessage;
    doc["IODSSR"]                    = ssrBias.iod;
    doc["SSRProviderID"]             = (int)ssrBias.ssrMeta.provider;
    doc["SSRSolutionID"]             = (int)ssrBias.ssrMeta.solution;
    doc["DisperBiasConsisIndicator"] = ssrBias.ssrPhase.dispBiasConistInd;
    doc["MWConsistencyIndicator"]    = ssrBias.ssrPhase.MWConistInd;
    doc["Sat"]                       = Sat.id();
    doc["YawAngle"]                  = ssrBias.ssrPhase.yawAngle;
    doc["YawRate"]                   = ssrBias.ssrPhase.yawRate;
    doc["Code"]                      = enum_to_string(code);
    doc["SignalIntegerIndicator"]    = (int)ssrBias.ssrPhaseChs[code].signalIntInd;
    doc["SignalsWLIntegerIndicator"] = (int)ssrBias.ssrPhaseChs[code].signalWLIntInd;
    doc["SignalDiscontinuityCount"]  = (int)ssrBias.ssrPhaseChs[code].signalDisconCnt;
    doc["Bias"]                      = ssrBias.obsCodeBiasMap[code].bias;

    fout << boost::json::serialize(doc) << "\n";
}

void RtcmTrace::traceTimestamp(GTime time)
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    boost::json::object doc;
    doc["type"]       = "timestamp";
    doc["Mountpoint"] = rtcmMountpoint;
    doc["time"]       = (string)time;
    doc["ticks"]      = (double)time.bigTime;

    fout << boost::json::serialize(doc) << "\n";
}

/** Write decoded/encoded GPS/GAL/BDS/QZS ephemeris messages to a json file
 */
void RtcmTrace::traceBrdcEph(  // todo aaron, template this for gps/glo?
    RtcmMessageType messCode,
    Eph&            eph
)
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    boost::json::object doc;

    GTime nearTime = timeGet();

    // Note the Satellite id is not set in rinex correctly as we a mixing GNSS systems.
    doc["type"]                 = "brdcEph";
    doc["Mountpoint"]           = rtcmMountpoint;
    doc["MessageNumber"]        = static_cast<int>(messCode);
    doc["MessageType"]          = enum_to_string(messCode);
    doc["ReceivedSentTimeGPST"] = nearTime.to_string();
    doc["Type"]                 = enum_to_string(eph.type);

    doc["ToeGPST"] = eph.toe.to_string();
    doc["TocGPST"] = eph.toc.to_string();

    traceBrdcEphBody(doc, eph);

    fout << boost::json::serialize(doc) << "\n";
}

/** Write decoded/encoded GAL ephemeris messages to a json file
 */
void RtcmTrace::traceBrdcEph(RtcmMessageType messCode, Geph& geph)
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    boost::json::object doc;

    GTime nearTime = timeGet();

    // Note the Satellite id is not set in rinex correctly as we a mixing GNSS systems.
    doc["type"]                 = "brdcEph";
    doc["Mountpoint"]           = rtcmMountpoint;
    doc["MessageNumber"]        = static_cast<int>(messCode);
    doc["MessageType"]          = enum_to_string(messCode);
    doc["ReceivedSentTimeGPST"] = nearTime.to_string();
    doc["Sat"]                  = geph.Sat.id();
    doc["Type"]                 = enum_to_string(geph.type);

    doc["ToeGPST"] = geph.toe.to_string();
    doc["TofGPST"] = geph.tof.to_string();

    traceBrdcEphBody(doc, geph);

    fout << boost::json::serialize(doc) << "\n";
}

void traceBrdcEphBody(boost::json::object& doc, Eph& eph)
{
    doc["Sat"]          = eph.Sat.id();
    doc["weekRollOver"] = eph.weekRollOver;
    doc["week"]         = eph.week;
    doc["toes"]         = eph.toes;
    doc["tocs"]         = eph.tocs;
    doc["howTow"]       = eph.howTow;
    doc["toe"]          = eph.toe.to_string();
    doc["toc"]          = eph.toc.to_string();
    doc["ttm"]          = eph.ttm.to_string();

    doc["aode"] = eph.aode;
    doc["aodc"] = eph.aodc;
    doc["iode"] = eph.iode;
    doc["iodc"] = eph.iodc;

    doc["f0"] = eph.f0;
    doc["f1"] = eph.f1;
    doc["f2"] = eph.f2;

    doc["sqrtA"] = eph.sqrtA;
    doc["A"]     = eph.A;
    doc["e"]     = eph.e;
    doc["i0"]    = eph.i0;
    doc["idot"]  = eph.idot;
    doc["omg"]   = eph.omg;
    doc["OMG0"]  = eph.OMG0;
    doc["OMGd"]  = eph.OMGd;
    doc["M0"]    = eph.M0;
    doc["deln"]  = eph.deln;
    doc["crc"]   = eph.crc;
    doc["crs"]   = eph.crs;
    doc["cic"]   = eph.cic;
    doc["cis"]   = eph.cis;
    doc["cuc"]   = eph.cuc;
    doc["cus"]   = eph.cus;

    doc["tgd0"] = eph.tgd[0];
    doc["tgd1"] = eph.tgd[1];  // GPS/QZS no tgd[1]
    doc["sva"]  = eph.sva;

    if (eph.Sat.sys == E_Sys::GPS || eph.Sat.sys == E_Sys::QZS)
    {
        doc["ura"]     = eph.ura[0];
        doc["svh"]     = eph.svh;
        doc["code"]    = eph.code;
        doc["flag"]    = eph.flag;  // QZS no flag
        doc["fitFlag"] = eph.fitFlag;
        doc["fit"]     = eph.fit;
    }
    else if (eph.Sat.sys == E_Sys::GAL)
    {
        doc["SISA"]            = eph.ura[0];
        doc["SVHealth"]        = eph.svh;
        doc["E5aHealth"]       = eph.e5a_hs;
        doc["E5aDataValidity"] = eph.e5a_dvs;
        doc["E5bHealth"]       = eph.e5b_hs;
        doc["E5bDataValidity"] = eph.e5b_dvs;
        doc["E1Health"]        = eph.e1_hs;
        doc["E1DataValidity"]  = eph.e1_dvs;
        doc["DataSource"]      = eph.code;
    }
    else if (eph.Sat.sys == E_Sys::BDS)
    {
        doc["URA"]      = eph.ura[0];
        doc["SVHealth"] = eph.svh;
    }
}

void traceBrdcEphBody(boost::json::object& doc, Geph& geph)
{
    doc["ToeSecOfDay"] = geph.tb;
    doc["TofHour"]     = geph.tk_hour;
    doc["TofMin"]      = geph.tk_min;
    doc["TofSec"]      = geph.tk_sec;

    doc["IODE"] = geph.iode;

    doc["TauN"]      = geph.taun;
    doc["GammaN"]    = geph.gammaN;
    doc["DeltaTauN"] = geph.dtaun;

    doc["PosX"] = geph.pos[0];
    doc["PosY"] = geph.pos[1];
    doc["PosZ"] = geph.pos[2];
    doc["VelX"] = geph.vel[0];
    doc["VelY"] = geph.vel[1];
    doc["VelZ"] = geph.vel[2];
    doc["AccX"] = geph.acc[0];
    doc["AccY"] = geph.acc[1];
    doc["AccZ"] = geph.acc[2];

    doc["FrquencyNumber"] = geph.frq;
    doc["SVHealth"]       = geph.svh;
    doc["Age"]            = geph.age;

    doc["GLONASSM"]            = geph.glonassM;
    doc["NumberOfDayIn4Year"]  = geph.NT;
    doc["AdditionalData"]      = geph.moreData;
    doc["4YearIntervalNumber"] = geph.N4;
}

/** Writes nav.satNavMap[].ssrOut to a human-readable file
 */
void writeSsrOutToFile(int epochNum, map<SatSys, SSROut>& ssrOutMap)
{
    string        filename = "ssrOut.dbg";
    std::ofstream out(filename, std::ios::app);

    if (!out)
    {
        BOOST_LOG_TRIVIAL(error) << "Could not open trace file for SSR messages at " << filename;
        return;
    }
    out.precision(17);

    // Header
    out << "epochNum" << "\t";
    out << "satId" << "\t";

    // 	out << "SSREph.canExport"	<< "\t";
    out << "SSREph.t0" << "\t";
    out << "SSREph.udi" << "\t";
    out << "SSREph.iod" << "\t";
    out << "SSREph.iode" << "\t";
    out << "SSREph.deph[0]" << "\t";
    out << "SSREph.deph[1]" << "\t";
    out << "SSREph.deph[2]" << "\t";
    out << "SSREph.ddeph[0]" << "\t";
    out << "SSREph.ddeph[1]" << "\t";
    out << "SSREph.ddeph[2]" << "\t";

    // 	out << "SSRClk.canExport"	<< "\t";
    out << "SSRClk.t0" << "\t";
    out << "SSRClk.udi" << "\t";
    out << "SSRClk.iod" << "\t";
    out << "SSRClk.dclk[0]" << "\t";
    out << "SSRClk.dclk[1]" << "\t";
    out << "SSRClk.dclk[2]" << "\t";

    out << "SSRBias.t0_code" << "\t";
    out << "SSRBias.t0_phas" << "\t";
    out << "SSRBias.udi_code" << "\t";
    out << "SSRBias.udi_phas" << "\t";
    out << "SSRBias.iod_code" << "\t";
    out << "SSRBias.iod_phas" << "\t";
    for (int i = 0; i < 2; ++i)
        out << "ssrBias.cbias_" << i << "\t";
    for (int i = 0; i < 2; ++i)
        out << "ssrBias.cvari_" << i << "\t";
    for (int i = 0; i < 2; ++i)
        out << "ssrBias.pbias_" << i << "\t";
    for (int i = 0; i < 2; ++i)
        out << "ssrBias.pvari_" << i << "\t";
    for (int i = 0; i < 2; ++i)
    {
        out << "ssrBias.ssrPhaseCh.signalIntInd_" << i << "\t";
        out << "ssrBias.ssrPhaseCh.signalWLIntInd_" << i << "\t";
        out << "ssrBias.ssrPhaseCh.signalDisconCnt_" << i << "\t";
    }
    out << "\n";

    // Body
    for (auto& [Sat, ssrOut] : ssrOutMap)
    {
        out << epochNum << "\t";
        out << Sat.id() << "\t";
        // 		out << ssrOut.ssrEph.canExport<< "\t";
        out << ssrOut.ssrEph.t0 << "\t";
        out << ssrOut.ssrEph.udi << "\t";
        out << ssrOut.ssrEph.iod << "\t";
        out << ssrOut.ssrEph.iode << "\t";
        out << ssrOut.ssrEph.deph[0] << "\t";
        out << ssrOut.ssrEph.deph[1] << "\t";
        out << ssrOut.ssrEph.deph[2] << "\t";
        out << ssrOut.ssrEph.ddeph[0] << "\t";
        out << ssrOut.ssrEph.ddeph[1] << "\t";
        out << ssrOut.ssrEph.ddeph[2] << "\t";

        // 		out << ssrOut.ssrClk.canExport<< "\t";
        out << ssrOut.ssrClk.t0 << "\t";
        out << ssrOut.ssrClk.udi << "\t";
        out << ssrOut.ssrClk.iod << "\t";
        out << ssrOut.ssrClk.dclk[0] << "\t";
        out << ssrOut.ssrClk.dclk[1] << "\t";
        out << ssrOut.ssrClk.dclk[2] << "\t";

        out << ssrOut.ssrCodeBias.t0 << "\t";
        out << ssrOut.ssrPhasBias.t0 << "\t";
        out << ssrOut.ssrCodeBias.udi << "\t";
        out << ssrOut.ssrPhasBias.udi << "\t";
        out << ssrOut.ssrCodeBias.iod << "\t";
        out << ssrOut.ssrPhasBias.iod << "\t";
        for (auto& [key, val] : ssrOut.ssrCodeBias.obsCodeBiasMap)
            out << val.bias << "\t" << val.var << "\t";
        for (auto& [key, val] : ssrOut.ssrPhasBias.obsCodeBiasMap)
            out << val.bias << "\t" << val.var << "\t";

        for (auto& [key, ssrPhaseCh] : ssrOut.ssrPhasBias.ssrPhaseChs)
        {
            out << ssrPhaseCh.signalIntInd << "\t";
            out << ssrPhaseCh.signalWLIntInd << "\t";
            out << ssrPhaseCh.signalDisconCnt << "\t";
        }
        out << "\n";
    }
    out << "\n";
}

/** Write msm message to a json file
 */
void RtcmTrace::traceMSM(RtcmMessageType messCode, GTime time, SatSys Sat, Sig& sig)
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    GTime nearTime = timeGet();

    boost::json::object doc;
    doc["type"]                 = "MSM";
    doc["Mountpoint"]           = rtcmMountpoint;
    doc["MessageNumber"]        = static_cast<int>(messCode);
    doc["MessageType"]          = enum_to_string(messCode);
    doc["ReceivedSentTimeGPST"] = nearTime.to_string();
    doc["EpochTimeGPST"]        = time.to_string();
    doc["Sat"]                  = Sat.id();
    doc["Code"]                 = enum_to_string(sig.code);
    doc["Pseudorange"]          = sig.P;
    doc["CarrierPhase"]         = sig.L;
    doc["Doppler"]              = sig.D;
    doc["SNR"]                  = sig.snr;
    doc["LLI"]                  = sig.LLI;
    doc["IsInvalid"]            = sig.invalid;

    fout << boost::json::serialize(doc) << "\n";
}

/** Write unknown message to a json file
 */
void RtcmTrace::traceUnknown()
{
    if (rtcmTraceFilename.empty())
    {
        return;
    }

    std::ofstream fout(rtcmTraceFilename, std::ios::app);
    if (!fout)
    {
        std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        return;
    }

    boost::json::object doc;
    doc["type"] = "?";

    fout << boost::json::serialize(doc) << "\n";
}
