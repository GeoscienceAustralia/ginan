#include "common/receiver.hpp"
#include "common/sinex.hpp"
#include "common/streamRinex.hpp"
#include "common/streamRtcm.hpp"
#include "common/streamParser.hpp"
#include <set>

SinexSiteId   dummySiteid;
SinexReceiver dummyReceiver;
SinexAntenna  dummyAntenna;
SinexSiteEcc  dummySiteEcc;

SinexSatIdentity dummySinexSatIdentity;
SinexSatEcc      dummySinexSatEcc;

ReceiverMap receiverMap;

void extractTrackedSignals(Receiver& rec, Parser& parser, ObsList* obsList)
{
    // Extract tracked signals from RINEX header or accumulate from RTCM observations
    // This enables receiver-specific signal tracking mode detection
    string parserType = parser.parserType();

    if (parserType == "RinexParser" && rec.trackedSignals.empty())
    {
        auto& rinexParser = static_cast<RinexParser&>(parser);

        for (auto& [sys, codeTypeMap] : rinexParser.sysCodeTypes)
        {
            vector<E_ObsCode> signals;
            for (auto& [idx, codeType] : codeTypeMap)
            {
                if (codeType.code != +E_ObsCode::NONE)
                {
                    signals.push_back(codeType.code);
                }
            }
            rec.trackedSignals[sys] = signals;
        }
    }
    else if (parserType == "RtcmParser" && obsList != nullptr)
    {
        // For RTCM streams, accumulate tracked signals from observations
        // This builds up the signal list dynamically as we receive data

        // Extract signals from the provided observation list
        for (auto& obs : only<GObs>(*obsList))
        {
            E_Sys sys = obs.Sat.sys;
            set<E_ObsCode> signalSet;

            // If we already have signals for this system, start with those
            if (rec.trackedSignals.count(sys))
            {
                signalSet.insert(rec.trackedSignals[sys].begin(), rec.trackedSignals[sys].end());
            }

            // Add signals from this observation
            for (auto& [ftype, sigsList] : obs.sigsLists)
            {
                for (auto& sig : sigsList)
                {
                    if (sig.code != +E_ObsCode::NONE)
                    {
                        signalSet.insert(sig.code);
                    }
                }
            }

            // Update the tracked signals with the expanded set
            rec.trackedSignals[sys] = vector<E_ObsCode>(signalSet.begin(), signalSet.end());
        }
    }
    else
    {
        // Signal extraction not supported for other parser types (UBX, Custom, etc.)
    }
}
