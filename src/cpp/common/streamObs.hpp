#pragma once

#include "common/acsConfig.hpp"
#include "common/enums.h"
#include "common/receiver.hpp"
#include "common/streamParser.hpp"

struct ObsLister
{
    list<ObsList> obsListList;
};

struct ObsStream : StreamParser
{
    E_ObsAgeCode obsAgeCode =
        E_ObsAgeCode::CURRENT_OBS;  ///< Age code of observation retrieved from memory
    GTime  lastReadTime = GTime::noTime();
    double interval     = 0;

    bool isPseudoRec;

    ObsStream(
        unique_ptr<Stream> stream_ptr,
        unique_ptr<Parser> parser_ptr,
        bool               isPseudoRec = false
    )
        : StreamParser(std::move(stream_ptr), std::move(parser_ptr)), isPseudoRec{isPseudoRec}
    {
    }

    ObsList getObs()
    {
        try
        {
            auto& obsLister = dynamic_cast<ObsLister&>(parser);

            for (auto it = obsLister.obsListList.begin(); it != obsLister.obsListList.end();)
            {
                if (it->empty())
                {
                    BOOST_LOG_TRIVIAL(info)
                        << "Dropping empty ObsList from parser queue before getObs"
                        << ", parser=" << parser.parserType() << ", source=" << stream.sourceString
                        << ", lastReadTime="
                        << (lastReadTime == GTime::noTime() ? string("noTime")
                                                            : lastReadTime.to_string(6))
                        << ", queued_epochs=" << obsLister.obsListList.size();
                    it = obsLister.obsListList.erase(it);
                }
                else
                {
                    ++it;
                }
            }

            BOOST_LOG_TRIVIAL(debug)
                << "obsLister.obsListList.size()=" << obsLister.obsListList.size();

            if (obsLister.obsListList.size() < 2 && stream.isDead() == false)
            {
                BOOST_LOG_TRIVIAL(debug) << "Not enough data in master list, reading new obs ...";

                parse();
            }
            else if (obsLister.obsListList.size() >= 2)
            {
                BOOST_LOG_TRIVIAL(debug)
                    << "Plenty of data in master list, no need to read more obs";
            }
            else
            {
                BOOST_LOG_TRIVIAL(debug) << "Input stream is dead, skip reading ...";
            }

            if (obsLister.obsListList.empty())
            {
                BOOST_LOG_TRIVIAL(debug) << "No obs";

                return ObsList();
            }

            ObsList& latestObsList = obsLister.obsListList.back();
            if (latestObsList.empty() == false)
            {
                if (lastReadTime != GTime::noTime())
                {
                    double newInterval = (latestObsList.front()->time - lastReadTime).to_double();

                    if (newInterval > 0 && (interval <= 0 || interval > newInterval))
                    {
                        interval = newInterval;
                    }
                }

                lastReadTime = latestObsList.front()->time;
            }
            else
            {
                BOOST_LOG_TRIVIAL(info)
                    << "Latest ObsList is empty after parse"
                    << ", parser=" << parser.parserType() << ", source=" << stream.sourceString
                    << ", lastReadTime="
                    << (lastReadTime == GTime::noTime() ? string("noTime")
                                                        : lastReadTime.to_string(6))
                    << ", queued_epochs=" << obsLister.obsListList.size();
            }

            ObsList& obsList = obsLister.obsListList.front();
            if (obsList.empty())
            {
                BOOST_LOG_TRIVIAL(info)
                    << "Front ObsList is empty after parse"
                    << ", parser=" << parser.parserType() << ", source=" << stream.sourceString
                    << ", lastReadTime="
                    << (lastReadTime == GTime::noTime() ? string("noTime")
                                                        : lastReadTime.to_string(6))
                    << ", queued_epochs=" << obsLister.obsListList.size();
                return ObsList();
            }

            BOOST_LOG_TRIVIAL(debug)
                << "Getting front ..., obsTime=" << obsList.front()->time.to_string(6);

            return obsList;
        }
        catch (...)
        {
            BOOST_LOG_TRIVIAL(debug) << "Error getting obs";
        }

        return ObsList();
    }

    /** Retrieve observations with a specified timestamp from memory where observations are
     * buffered, and update obsAgeCode according to the status of retrieved observations:
     *     NO_OBS:      No observation at all in memory
     *     PAST_OBS:    Closest observation time is earlier than current processing epoch without
     *                  tolerance
     *     CURRENT_OBS: First processing epoch, or suitable observations found for current
     *                  processing epoch
     *     FUTURE_OBS:  Closest observation time is later than current processing epoch
     *                  without tolerance
     * NOTE: This function may be overridden by objects that use this interface
     */
    ObsList getObs(
        GTime& time,        ///< Timestamp to get observations for
        double delta = 0.5  ///< Acceptable tolerance around requested time
    )
    {
        ObsList bigObsList;
        bool    foundGoodObs = false;
        while (1)
        {
            ObsList obsList = getObs();

            if (obsList.empty())
            {
                obsAgeCode = E_ObsAgeCode::NO_OBS;
                break;
            }
            else if (time == GTime::noTime())
            {
                // Start epoch not given, get first obs time
                obsAgeCode = E_ObsAgeCode::UNKNOWN;
                time       = obsList.front()->time;
                BOOST_LOG_TRIVIAL(debug) << "obsAgeCode=" << obsAgeCode << ", dropping front";
                break;
            }
            else if (obsList.front()->time < time - delta)
            {
                // Save earlier data to preprocess in case preprocess_all_data is on
                obsAgeCode = E_ObsAgeCode::PAST_OBS;
                dropObs();
                BOOST_LOG_TRIVIAL(debug) << "obsAgeCode=" << obsAgeCode << ", dropping front";
                if (foundGoodObs == false)
                {
                    // Only push past obs when good obs not found yet, i.e. drop past obs coming
                    // late after current ones and continue to find good ones in case data is out of
                    // order
                    bigObsList += obsList;
                    break;
                }
            }
            else if (obsList.front()->time > time + delta)
            {
                // Future obs, do nothing and leave the data to read later
                obsAgeCode = E_ObsAgeCode::FUTURE_OBS;
                BOOST_LOG_TRIVIAL(debug) << "obsAgeCode=" << obsAgeCode << ", checking next epoch";
                break;
            }
            else
            {
                // Current obs (within epoch tolerance), continue with loop to get all current obs
                foundGoodObs = true;
                dropObs();
                bigObsList += obsList;
                BOOST_LOG_TRIVIAL(debug)
                    << "obsAgeCode=" << E_ObsAgeCode::CURRENT_OBS << ", dropping front";
            }
        }

        if (foundGoodObs)
        {
            // Future obs may have been attempted (obsAgeCode is now FUTURE_OBS) or no more
            // obs (obsAgeCode is now NO_OBS) even good obs found, reset obsAgeCode to CURRENT_OBS
            obsAgeCode = E_ObsAgeCode::CURRENT_OBS;
        }

        return bigObsList;
    }

    /** Drop the front observation list from memory when it has been read sucessfully
     */
    void dropObs()
    {
        try
        {
            auto& obsLister = dynamic_cast<ObsLister&>(parser);

            if (obsLister.obsListList.size() > 0)
            {
                obsLister.obsListList.pop_front();
            }
        }
        catch (...)
        {
        }
    }

    bool hasObs()
    {
        try
        {
            auto& obsLister = dynamic_cast<ObsLister&>(parser);

            if (obsLister.obsListList.empty())
            {
                return false;
            }

            return true;
        }
        catch (...)
        {
            return false;
        }
    }
};
