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

            if (obsLister.obsListList.size() < 2)
            {
                parse();
            }

            if (obsLister.obsListList.empty())
            {
                return ObsList();
            }

            ObsList& obsList = obsLister.obsListList.front();

            for (auto& obs : only<GObs>(obsList))
                for (auto& [ftype, sigsList] : obs.sigsLists)
                {
                    E_Sys sys = obs.Sat.sys;

                    if (sys == E_Sys::GPS)
                    {
                        double dirty_C1W_phase = 0;
                        for (auto& sig : sigsList)
                        {
                            if (sig.code == E_ObsCode::L1C)
                                dirty_C1W_phase = sig.L;

                            if (sig.code == E_ObsCode::L1W && sig.P == 0)
                            {
                                sig.L = 0;
                            }
                        }

                        for (auto& sig : sigsList)
                            if (sig.code == E_ObsCode::L1W && sig.L == 0 && sig.P != 0)
                            {
                                sig.L = dirty_C1W_phase;
                                break;
                            }
                    }
                    sigsList.remove_if(
                        [sys](Sig& a)
                        {
                            return std::find(
                                       acsConfig.code_priorities[sys].begin(),
                                       acsConfig.code_priorities[sys].end(),
                                       a.code
                                   ) == acsConfig.code_priorities[sys].end();
                        }
                    );
                    sigsList.sort(
                        [sys](Sig& a, Sig& b)
                        {
                            auto iterA = std::find(
                                acsConfig.code_priorities[sys].begin(),
                                acsConfig.code_priorities[sys].end(),
                                a.code
                            );
                            auto iterB = std::find(
                                acsConfig.code_priorities[sys].begin(),
                                acsConfig.code_priorities[sys].end(),
                                b.code
                            );

                            if (a.L == 0)
                                return false;
                            if (b.L == 0)
                                return true;
                            if (a.P == 0)
                                return false;
                            if (b.P == 0)
                                return true;
                            if (iterA < iterB)
                                return true;
                            else
                                return false;
                        }
                    );

                    if (sigsList.empty())
                    {
                        continue;
                    }

                    Sig firstOfType = sigsList.front();

                    // use first of type as representative if its in the priority list
                    auto iter = std::find(
                        acsConfig.code_priorities[sys].begin(),
                        acsConfig.code_priorities[sys].end(),
                        firstOfType.code
                    );
                    if (iter != acsConfig.code_priorities[sys].end())
                    {
                        obs.sigs[ftype] = Sig(firstOfType);
                    }
                }

            return obsList;
        }
        catch (...)
        {
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
        GTime  time,        ///< Timestamp to get observations for
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
                // Start epoch not given, use time of first obs as start time
                foundGoodObs = true;
                dropObs();
                bigObsList += obsList;
                break;
            }
            else if (obsList.front()->time < time - delta)
            {
                // Save earlier data to preprocess in case preprocess_all_data is on
                obsAgeCode = E_ObsAgeCode::PAST_OBS;
                dropObs();
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
                break;
            }
            else
            {
                // Current obs (within epoch tolerance), continue with loop to get all current obs
                foundGoodObs = true;
                dropObs();
                bigObsList += obsList;
            }
        }

        if (foundGoodObs)
        {
            // Future obs may have been attempted (obsAgeCode is now FUTURE_OBS) or no more
            // obs (obsAgeCode is now NO_OBS) even good obs found, reset obsAgeCode to CURRENT_OBS
            obsAgeCode = E_ObsAgeCode::CURRENT_OBS;
        }
        else if (obsAgeCode == E_ObsAgeCode::FUTURE_OBS)
        {
            return ObsList();
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
