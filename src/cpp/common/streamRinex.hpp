#pragma once

#include "common/navigation.hpp"
#include "common/rinex.hpp"
#include "common/streamObs.hpp"

struct RinexParser : Parser, ObsLister
{
    // Keep the parser state deterministic before the first header is read.
    // The E29 input checkpoint exporter may inspect a freshly configured
    // parser during fail-closed inventory validation.
    char                           ctype      = 0;
    double                         version    = 0;
    E_Sys                          nav_system = E_Sys::NONE;
    E_TimeSys                      time_system = E_TimeSys::NONE;
    map<E_Sys, map<int, CodeType>> sysCodeTypes;
    ObsList                        tempObsList;
    RinexStation                   rnxRec = {};

    void parse(std::istream& inputStream)
    {
        // read some of the input,(up to next epoch header?)
        // save outputs to member variables.
        // eg. header metadata
        // eg. list of (ObsLists) with multiple sats, signals combined for each epoch.

        int stat = 0;
        // account for rinex comment in the middle of the file
        while (stat <= 0 && inputStream)
        {
            stat = readRnx(
                inputStream,
                ctype,
                tempObsList,
                nav,
                rnxRec,
                version,
                nav_system,
                time_system,
                sysCodeTypes
            );
        }

        if (tempObsList.size() > 0)
        {
            obsListList.push_back(std::move(tempObsList));

            BOOST_LOG_TRIVIAL(debug) << "Parsed " << tempObsList.size()
                                     << " obs, obsTime=" << tempObsList.front()->time.to_string(6);
        }
        else
        {
            BOOST_LOG_TRIVIAL(debug) << "No obs parsed";
        }
    }

    string parserType() { return "RinexParser"; }
};
