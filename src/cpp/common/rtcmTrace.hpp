#pragma once

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <chrono>
#include <fstream>
#include <iostream>
#include <string>
#include "common/gTime.hpp"
#include "common/satSys.hpp"

using std::string;

struct Sig;
struct Eph;
struct Geph;
struct SSREph;
struct SSRClk;
struct SSRUra;
struct SSRHRClk;
struct SSRCodeBias;
struct SSRPhasBias;

struct RtcmTrace
{
    string rtcmTraceFilename = "";
    string rtcmMountpoint;
    bool   qzssL6 = false;

    RtcmTrace(string mountpoint = "", string filename = "")
        : rtcmTraceFilename{filename}, rtcmMountpoint{mountpoint}
    {
    }

    void networkLog(string message)
    {
        std::ofstream outStream(rtcmTraceFilename, std::iostream::app);
        if (!outStream)
        {
            std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
            return;
        }

        outStream << timeGet();
        outStream << " " << __FUNCTION__ << message << "\n";
    }

    void messageChunkLog(string message) {}

    void messageRtcmLog(string message)
    {
        std::ofstream outStream(rtcmTraceFilename, std::ios::app);
        if (!outStream)
        {
            std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
            return;
        }

        outStream << timeGet();
        outStream << " messageRtcmLog" << message << "\n";
    }

    void traceSsrEph(RtcmMessageType messCode, SatSys Sat, SSREph& ssrEph);

    void traceSsrClk(RtcmMessageType messCode, SatSys Sat, SSRClk& ssrClk);

    void traceSsrUra(RtcmMessageType messCode, SatSys Sat, SSRUra& ssrUra);

    void traceSsrHRClk(RtcmMessageType messCode, SatSys Sat, SSRHRClk& ssrHRClk);

    void
    traceSsrCodeBias(RtcmMessageType messCode, SatSys Sat, E_ObsCode code, SSRCodeBias& ssrBias);

    void
    traceSsrPhasBias(RtcmMessageType messCode, SatSys Sat, E_ObsCode code, SSRPhasBias& ssrBias);

    void traceTimestamp(GTime time);

    void traceBrdcEph(RtcmMessageType messCode, Eph& eph);

    void traceBrdcEph(RtcmMessageType messCode, Geph& geph);

    void traceMSM(RtcmMessageType messCode, GTime time, SatSys Sat, Sig& sig);

    void traceUnknown();
};

void traceBrdcEphBody(boost::json::object& obj, Eph& eph);

void traceBrdcEphBody(boost::json::object& obj, Geph& geph);

extern map<RtcmMessageType, E_Sys> rtcmMessageSystemMap;
