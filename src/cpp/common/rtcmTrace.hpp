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
    string        rtcmTraceFilename = "";
    std::ofstream rtcmTraceFile;
    string        rtcmMountpoint;
    bool          qzssL6 = false;

    RtcmTrace(string mountpoint = "", string filename = "")
        : rtcmTraceFilename{filename}, rtcmMountpoint{mountpoint}
    {
        if (rtcmTraceFilename.empty())
        {
            return;
        }
        rtcmTraceFile.open(rtcmTraceFilename, std::ios::app);
        if (!rtcmTraceFile)
        {
            std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        }
        std::cout << "opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
    }

    void openTraceFile()
    {
        if (rtcmTraceFilename.empty())
            return;

        if (rtcmTraceFile.is_open())
        {
            rtcmTraceFile.flush();
            rtcmTraceFile.close();
        }

        rtcmTraceFile.open(rtcmTraceFilename, std::ios::app);
        if (!rtcmTraceFile)
        {
            std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << "\n";
        }
    }

    void setTraceFilename(const string& filename)
    {
        if (filename == rtcmTraceFilename && rtcmTraceFile.is_open())
            return;

        rtcmTraceFilename = filename;
        openTraceFile();
    }

    ~RtcmTrace()
    {
        if (rtcmTraceFile.is_open())
        {
            rtcmTraceFile.flush();
            rtcmTraceFile.close();
        }
    }

    void flush()
    {
        if (rtcmTraceFile.is_open())
            rtcmTraceFile.flush();
    }

    void networkLog(string message)
    {
        rtcmTraceFile << timeGet();
        rtcmTraceFile << " " << __FUNCTION__ << message << "\n";
    }

    void messageChunkLog(string message) {}

    void messageRtcmLog(string message)
    {
        rtcmTraceFile << timeGet();
        rtcmTraceFile << " messageRtcmLog" << message << "\n";
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
