// #pragma GCC optimize ("O0")

#include <string>
#include <vector>
#include "common/acsConfig.hpp"
#include "common/attitude.hpp"
#include "common/constants.hpp"
#include "common/eigenIncluder.hpp"
#include "common/ephemeris.hpp"
#include "common/navigation.hpp"
#include "common/rinex.hpp"
#include "common/trace.hpp"
#include "orbprop/coordinates.hpp"
#include "pea/inputsOutputs.hpp"
#include "pea/minimumConstraints.hpp"

using std::string;
using std::vector;

struct TraceDummy
{
    string jsonTraceFilename;
    string traceFilename;
    string id;
};

vector<Navigation> navVec;

void compareClocks(vector<string> files) {}

void compareOrbits(vector<string> files)
{
    TraceDummy traceDummy;

    if (acsConfig.output_network_trace)
    {
        boost::posix_time::ptime logptime = currentLogptime();

        createDirectories(logptime);

        createNewTraceFile(
            "Network",
            "",
            logptime,
            acsConfig.network_trace_filename,
            traceDummy.traceFilename,
            true,
            acsConfig.output_config
        );
    }

    auto trace = getTraceFile(traceDummy);

    for (auto& file : files)
    {
        Navigation nav;
        readSp3ToNav(file, nav, 0);

        navVec.push_back(nav);
    }

    for (int i = 1; i < navVec.size(); i++)
    {
        std::cout << "\n"
                  << "Comparing files:" << "\n"
                  << files[0] << "\n"
                  << files[i] << "\n";

        // invert maps
        map<GTime, map<string, Peph>> pephMapMap0;
        map<GTime, map<string, Peph>> pephMapMap1;

        for (auto& [id, pephMap] : navVec[0].pephMap)
            for (auto& [time, peph] : pephMap)
            {
                auto satOpts = acsConfig.getSatOpts(peph.Sat);
                if (satOpts.exclude)
                {
                    continue;
                }

                pephMapMap0[time][id] = peph;
            }
        for (auto& [id, pephMap] : navVec[i].pephMap)
            for (auto& [time, peph] : pephMap)
            {
                auto satOpts = acsConfig.getSatOpts(peph.Sat);
                if (satOpts.exclude)
                {
                    continue;
                }

                pephMapMap1[time][id] = peph;
            }

        MinconStatistics           minconStatistics1;
        map<KFKey, vector<double>> transformStatisticsMap;

        KFState kfStateTransform;

        int iterations;
        if (acsConfig.minconOpts.once_per_epoch)
            iterations = 1;
        else
            iterations = 2;

        for (int iteration = 1; iteration <= iterations; iteration++)
            for (auto& [time, pephMap0] : pephMapMap0)
            {
                auto it = pephMapMap1.find(time);
                if (it == pephMapMap1.end())
                {
                    std::cout << "\n" << time << " not found in " << files[i];
                    continue;
                }

                auto& [dummy, pephMap1] = *it;

                if (acsConfig.minconOpts.once_per_epoch)
                {
                    // reset for each epoch
                    kfStateTransform = KFState();
                }

                KFState kfState;

                ERPValues erpv = getErp(nav.erp, time);

                FrameSwapper frameSwapper(time, erpv);

                for (auto& [id, peph0] : pephMap0)
                {
                    auto it2 = pephMap1.find(id);
                    if (it2 == pephMap1.end())
                    {
                        // std::cout << "\n" << id << " not found in " << files[i] << " for " <<
                        // time;
                        continue;
                    }

                    auto& [dummy, peph1] = *it2;

                    if (peph1.vel.isZero())
                    {
                        GTime    interpStartTime = time;
                        Vector3d interpStartPos  = peph1.pos;
                        GTime    interpStopTime  = time;
                        Vector3d interpStopPos   = peph1.pos;

                        it++;
                        if (it != pephMapMap1.end())
                        {
                            interpStopTime = it->first;
                            interpStopPos  = it->second[id].pos;
                        }
                        it--;
                        if (it != pephMapMap1.begin())
                        {
                            it--;
                            interpStartTime = it->first;
                            interpStartPos  = it->second[id].pos;
                            it++;
                        }

                        peph1.vel = (interpStopPos - interpStartPos) /
                                    (interpStopTime - interpStartTime).to_double();
                    }

                    // set noise value
                    auto& satOpts = acsConfig.getSatOpts(peph1.Sat);

                    // set apriori value
                    auto& satNav =
                        nav.satNavMap[peph1.Sat];  // this is the global nav used by mincon
                    satNav.aprioriPos = frameSwapper(peph0.pos);

                    // set test value
                    KFKey kfKey;
                    kfKey.type = KF::ORBIT;
                    kfKey.Sat  = peph1.Sat;
                    VectorEci velEci;
                    VectorEci posEci = frameSwapper(peph1.pos, &peph1.vel, &velEci);

                    for (int i = 0; i < 3; i++)
                    {
                        kfKey.num = i;
                        kfState.addKFState(kfKey, {.x = posEci[i], .P = 1});
                        kfKey.num = i + 3;
                        kfState.addKFState(kfKey, {.x = velEci[i], .P = 1});
                    }
                }

                kfState.stateTransition(trace, time);
                // 			kfState.outputStates	(trace);

                if (acsConfig.process_minimum_constraints)
                {
                    bool              estimate            = true;
                    MinconStatistics* totalStatistics_ptr = &minconStatistics1;

                    if (iterations == 2 && iteration == 2)
                    {
                        // dont reestimate the transform the second time around
                        estimate = false;
                    }

                    if (iterations == 2 && iteration == 1)
                    {
                        // dont add statistics the first time around
                        totalStatistics_ptr = nullptr;
                    }

                    MinconStatistics minconStatistics0;

                    mincon(
                        trace,
                        kfState,
                        &minconStatistics0,
                        totalStatistics_ptr,
                        false,
                        &kfStateTransform,
                        estimate,
                        !estimate
                    );

                    if (iterations == 1 || iteration == 2)
                    {
                        // only chance, or second time around, output statistics

                        outputMinconStatistics(trace, minconStatistics0, "/" + time.to_string());

                        for (auto& [kfKey, index] : kfStateTransform.kfIndexMap)
                        {
                            if (kfKey.type == KF::ONE)
                            {
                                continue;
                            }

                            double val = kfStateTransform.x(index);

                            transformStatisticsMap[kfKey].push_back(val);
                        }
                    }
                }
            }

        outputMinconStatistics(trace, minconStatistics1, "/TOTAL");
        outputMinconStatistics(std::cout, minconStatistics1, "/TOTAL");

        Block block(trace, "HELMERT_STATISTICS");
        for (auto& [key, vec] : transformStatisticsMap)
        {
            double n   = vec.size();
            double avg = 0;
            double std = 0;
            for (auto& entry : vec)
            {
                avg += entry;
            }
            avg = avg / n;
            for (auto& entry : vec)
            {
                std += SQR(entry - avg);
            }
            std = sqrt(std) / (n - 1);

            tracepdeex(
                0,
                trace,
                "^ %-10s %-3s averaged %+12.4e %-5s with std %12.4e over %d epochs\n",
                enum_to_string(key.type).c_str(),
                key.code().c_str(),
                avg,
                key.comment.c_str(),
                std,
                n
            );
        }
    }
}

void compareAttitudes(vector<string> files)
{
    for (auto& file : files)
    {
        Navigation nav;
        readOrbex(file, nav);

        navVec.push_back(nav);
    }

    for (int i = 1; i < navVec.size(); i++)
    {
        auto& nav0 = navVec[0];
        auto& nav1 = navVec[1];

        std::cout << "\n"
                  << "Comparing files:" << "\n"
                  << files[0] << "\n"
                  << files[i] << "\n";

        for (auto& [id, attMap0] : nav0.attMapMap)
        {
            auto it = nav1.attMapMap.find(id);
            if (it == nav1.attMapMap.end())
            {
                std::cout << "\n" << id << " not found in " << files[i];
                continue;
            }

            auto& [dummy, attMap] = *it;

            for (auto& [time, att0] : attMap0)
            {
                auto it = attMap.find(time);
                if (it == attMap.end())
                {
                    std::cout << "\n" << time << " not found in " << files[i] << " for " << id;
                    continue;
                }

                auto& [dummy, att] = *it;

                double angle = att0.q.angularDistance(att.q) * R2D;

                tracepdeex(
                    0,
                    std::cout,
                    "\n%s - %s - %6.1fdeg",
                    time.to_string().c_str(),
                    id.c_str(),
                    angle
                );
            }
        }
    }
}
