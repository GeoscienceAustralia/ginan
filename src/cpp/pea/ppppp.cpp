// #pragma GCC optimize ("O0")

#include "pea/ppp.hpp"
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include "architectureDocs.hpp"
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/common.hpp"
#include "common/eigenIncluder.hpp"
#include "common/metaData.hpp"
#include "common/mongoRead.hpp"
#include "common/mongoWrite.hpp"
#include "common/navigation.hpp"
#include "common/observations.hpp"
#include "common/receiver.hpp"
#include "common/trace.hpp"
#include "inertial/posProp.hpp"
#include "iono/ionoModel.hpp"
#include "omp.h"
#include "orbprop/coordinates.hpp"
#include "orbprop/orbitProp.hpp"
#include "rtklib/lambda.h"

using std::map;
using std::string;
using std::stringstream;
using std::tuple;

/** Primary estimation and filtering.
 *
 * While there are other auxiliary filters and states used within the Pea, all PPP processing flows
 * through a common filtering stage.
 *
 * The residuals for all observations are first computed in an undifferenced-uncombined state, which
 * leads to the greatest generality and extensibility. As each receiver's observations are then
 * independent of each other, these computations are computed in parallel, using openMP directives,
 * increasing thoughput.
 *
 * Bookkeeping around the initialisation of state elements and their state transitions is taken care
 * of automatically at the point they are first referenced in the observation equations, using
 * values as configured in the yaml file.
 *
 * The distinction between "Network" and "User" positioning modes that may be used in other software
 * packages is not required in Ginan. All receivers are always stored in a large single filter.
 * Depending on the configuration, the state and it's covariance matrix may turn out to be
 * block-diagonal (user-mode), which will automatically be treated optimally upon the estimation
 * stage by applying 'chunking'.
 *
 */
Architecture Main_Filter__()
{
    DOCS_REFERENCE(UDUC_GNSS_Measurements__);
    DOCS_REFERENCE(SLR_Mesaurements__);
    DOCS_REFERENCE(Combinators__);
    DOCS_REFERENCE(Pseudo_Observations__);
    DOCS_REFERENCE(Kalman_Filter__);
    DOCS_REFERENCE(Error_Handling__);
}

/**
 */
Architecture Combinators__() {}

#ifdef ENABLE_PARALLELISATION
#endif

struct Duo
{
    map<KFKey, int>& indexMap;
    MatrixXd&        designMatrix;
};

void explainMeasurements(Trace& trace, KFMeas& kfMeas, KFState& kfState)
{
    for (int i = 0; i < kfMeas.obsKeys.size(); i++)
    {
        auto& obsKey      = kfMeas.obsKeys[i];
        auto& metaDataMap = kfMeas.metaDataMaps[i];

        if (metaDataMap["explain"] == nullptr)
        {
            continue;
        }

        trace << "\n"
              << "============================";
        trace << "\n"
              << "Explaining " << obsKey << " : " << obsKey.comment;

        for (auto duo :
             {Duo{kfState.kfIndexMap, kfMeas.H}, Duo{kfMeas.noiseIndexMap, kfMeas.H_star}})
            for (int col = 0; col < duo.designMatrix.cols(); col++)
            {
                if (col == 0)
                {
                    trace << "\n"
                          << "============================";
                }

                double entry = duo.designMatrix(i, col);

                if (entry == 0)
                {
                    continue;
                }

                for (auto& [kfKey, index] : duo.indexMap)
                {
                    if (index == col)
                    {
                        trace << "\n";
                        if (traceLevel >= 4)
                            trace << kfState.time << " " << obsKey;

                        trace << kfKey << " : " << entry;
                        break;
                    }
                }
            }
        trace << "\n";
    }
}

void alternatePostfits(Trace& trace, KFMeas& kfMeas, KFState& kfState)
{
    if (kfState.advanced_postfits == false)
    {
        return;
    }

    for (auto& [kfKey, col] : kfMeas.noiseIndexMap)
    {
        if (kfKey.type > KF::BEGIN_MEAS_STATES && kfKey.type < KF::END_MEAS_STATES)
        {
            continue;
        }

        bool first = true;

        for (int row = 0; row < kfMeas.H_star.rows(); row++)
        {
            double& entry = kfMeas.H_star(row, col);

            if (entry == 0)
            {
                continue;
            }

            if (first)
            {
                first = false;

                trace << "\n"
                      << "Removing " << kfKey << " from postfit residual calculations";
            }

            entry = 0;
        }
    }
}

void makeIFLCs(Trace& trace, KFState& kfState, KFMeasEntryList& kfMeasEntryList)
{
    if (kfMeasEntryList.empty())
    {
        return;
    }

    bool iflcMade = false;

    for (int i = 0; i < kfMeasEntryList.size(); i++)
    {
        auto& kfMeasEntryI = kfMeasEntryList[i];

        if (kfMeasEntryI.valid == false)
        {
            continue;
        }

        double coeff_i = 0;
        KFKey  ionKey_i;
        bool   useDesignMap = true;

        for (auto designMap : {true, false})
        {
            map<KFKey, double>* entryMapPtr;
            if (designMap)
                entryMapPtr = &kfMeasEntryI.designEntryMap;
            else
                entryMapPtr = &kfMeasEntryI.noiseEntryMap;

            for (auto& [key, value] : *entryMapPtr)
            {
                if (key.type == KF::IONO_STEC)
                {
                    ionKey_i     = key;
                    coeff_i      = value;
                    useDesignMap = designMap;
                    goto breakI;
                }
            }
        }
    breakI:

        if (coeff_i == 0)
        {
            // no ionosphere reference
            continue;
        }

        // either this will be combined with something below, or it wont be, in either case this one
        // is not needed and invalid now
        kfMeasEntryI.valid = false;

        for (int j = i + 1; j < kfMeasEntryList.size(); j++)
        {
            auto& kfMeasEntryJ = kfMeasEntryList[j];

            if (kfMeasEntryI.metaDataMap["IFLCcombined"])
            {
                continue;
            }
            if (kfMeasEntryJ.metaDataMap["IFLCcombined"])
            {
                continue;
            }

            map<KFKey, double>* entryMapPtr;
            if (useDesignMap)
                entryMapPtr = &kfMeasEntryJ.designEntryMap;
            else
                entryMapPtr = &kfMeasEntryJ.noiseEntryMap;

            auto it = entryMapPtr->find(ionKey_i);
            if (it == entryMapPtr->end())
            {
                continue;
            }

            auto& [ionKey_j, coeff_j] = *it;

            if (coeff_i * coeff_j < 0)
            {
                continue;
            }  // only combine similarly signed (code/phase) components
            if (coeff_i == coeff_j)
            {
                continue;
            }  // dont combine if it will eliminate the entire measurement

            // these measurements both share a common ionosphere, remove it.

            iflcMade = true;

            double scalar = sqrt((SQR(coeff_i) + SQR(coeff_j)) / SQR(coeff_i - coeff_j));

            kfMeasEntryJ.obsKey.num = 100 * kfMeasEntryI.obsKey.num + kfMeasEntryJ.obsKey.num;

            kfMeasEntryJ.obsKey.comment =
                kfMeasEntryI.obsKey.comment + "-" + kfMeasEntryJ.obsKey.comment;

            kfMeasEntryJ.innov =
                coeff_j * scalar * kfMeasEntryI.innov - coeff_i * scalar * kfMeasEntryJ.innov;

            map<KFKey, double>                  newDesignEntryMap;
            map<KFKey, double>                  newNoiseEntryMap;
            map<E_Component, ComponentsDetails> newComponentsMap;

            for (auto& [key, valueI] : kfMeasEntryI.usedValueMap)
                kfMeasEntryJ.usedValueMap[key] = valueI;

            for (auto& [key, valueI] : kfMeasEntryI.noiseElementMap)
                kfMeasEntryJ.noiseElementMap[key] = valueI;

            for (auto& [key, valueI] : kfMeasEntryI.designEntryMap)
                newDesignEntryMap[key] += valueI * coeff_j * scalar * +1;
            for (auto& [key, valueJ] : kfMeasEntryJ.designEntryMap)
                newDesignEntryMap[key] += valueJ * coeff_i * scalar * -1;

            for (auto& [key, valueI] : kfMeasEntryI.noiseEntryMap)
                newNoiseEntryMap[key] += valueI * coeff_j * scalar * +1;
            for (auto& [key, valueJ] : kfMeasEntryJ.noiseEntryMap)
                newNoiseEntryMap[key] += valueJ * coeff_i * scalar * -1;

            for (auto& [key, valueI] : kfMeasEntryI.componentsMap)
                newComponentsMap[key] += valueI * coeff_j * scalar * +1;
            for (auto& [key, valueJ] : kfMeasEntryJ.componentsMap)
                newComponentsMap[key] += valueJ * coeff_i * scalar * -1;

            for (auto& [id, value] : kfMeasEntryI.metaDataMap)
            {
                kfMeasEntryJ.metaDataMap[id + "_alt"] = value;
            }

            kfMeasEntryI.metaDataMap["IFLCcombined"] = (void*)true;
            kfMeasEntryJ.metaDataMap["IFLCcombined"] = (void*)true;
            // 			kfMeasEntryJ.metaDataMap["explain"]			= (void*) true;

            if (useDesignMap)
                newDesignEntryMap[ionKey_i] = 0;
            else
                newNoiseEntryMap[ionKey_i] = 0;
            kfMeasEntryJ.designEntryMap = std::move(newDesignEntryMap);
            kfMeasEntryJ.noiseEntryMap  = std::move(newNoiseEntryMap);
            kfMeasEntryJ.componentsMap  = std::move(newComponentsMap);

            kfState.removeState(ionKey_i);
            break;
        }
    }

    if (iflcMade == false)
    {
        BOOST_LOG_TRIVIAL(warning)
            << "No IONO_STEC measurements found - 'use_if_combo' requires 'ion_stec' "
               "estimation to be enabled in the config file.";
    }
}

/** Replace individual measurements with linear combinations
 */
KFMeas makeGFLCs(KFMeas& kfMeas, KFState& kfState)
{
    int meas = 0;

    vector<Triplet<double>>        tripletList;
    decltype(KFMeas::metaDataMaps) newMetaDataMaps;
    decltype(KFMeas::obsKeys)      newObsKeys;

    for (auto duo : {Duo{kfState.kfIndexMap, kfMeas.H}, Duo{kfMeas.noiseIndexMap, kfMeas.H_star}})
        for (auto& [kfKey, index] : duo.indexMap)
        {
            if (kfKey.type != KF::IONO_STEC)
                continue;
            for (int i_2 = 0; i_2 < kfMeas.obsKeys.size(); i_2++)
            {
                double coeff_2 = duo.designMatrix(i_2, index);
                if (coeff_2 == 0)
                    continue;
                for (int i_1 = 0; i_1 < i_2; i_1++)
                {
                    double coeff_1 = duo.designMatrix(i_1, index);
                    if (coeff_1 == 0)
                        continue;
                    {
                        if (coeff_1 * coeff_2 < 0)
                        {
                            continue;
                        }
                        if (coeff_1 == coeff_2)
                        {
                            continue;
                        }  // dont combine if it will eliminate the entire measurement

                        if (kfMeas.metaDataMaps[i_1]["GFLCcombined"])
                        {
                            continue;
                        }
                        if (kfMeas.metaDataMaps[i_2]["GFLCcombined"])
                        {
                            continue;
                        }

                        // these measurements probably both share a common geometry, remove it.

                        double scalar = 0.5;

                        tripletList.push_back({meas, i_1, +1 * scalar});
                        tripletList.push_back({meas, i_2, -1 * scalar});
                        meas++;

                        auto& obsKey_1 = kfMeas.obsKeys[i_1];
                        auto& obsKey_2 = kfMeas.obsKeys[i_2];

                        auto newObsKey = obsKey_2;

                        newObsKey.num = 100 * obsKey_1.num + 1 * obsKey_2.num;

                        newObsKey.comment = obsKey_1.comment + "-" + obsKey_2.comment;

                        newObsKeys.push_back(newObsKey);

                        // copy metadata into the new measurement
                        map<string, void*> newMetaData;
                        for (auto& [id, value] : kfMeas.metaDataMaps[i_1])
                        {
                            newMetaData[id] = value;
                        }
                        for (auto& [id, value] : kfMeas.metaDataMaps[i_2])
                        {
                            newMetaData[id + "_alt"] = value;
                        }
                        newMetaData["explain"] = (void*)true;

                        newMetaDataMaps.push_back(std::move(newMetaData));

                        kfMeas.metaDataMaps[i_1]["GFLCcombined"] = (void*)true;
                        kfMeas.metaDataMaps[i_2]["GFLCcombined"] = (void*)true;
                    }
                }
            }
        }

    if (meas == 0)
    {
        BOOST_LOG_TRIVIAL(warning)
            << "No IONO_STEC measurements found - 'use_gf_combo' requires 'iono_stec' "
               "estimation to be enabled in the config file.";
    }

    for (int i = 0; i < kfMeas.obsKeys.size(); i++)
    {
        if (kfMeas.metaDataMaps[i]["pseudoObs"] == (void*)false)
        {
            continue;
        }

        // need to keep this measurement even if its not a valid ionospheric one, copy it over

        tripletList.push_back({meas, i, 1});
        meas++;

        newObsKeys.push_back(kfMeas.obsKeys[i]);
        newMetaDataMaps.push_back(kfMeas.metaDataMaps[i]);
    }

    return KFMeas(
        kfMeas,
        std::move(tripletList),
        std::move(newObsKeys),
        std::move(newMetaDataMaps)
    );
}

/** Replace individual measurements with linear combinations
 */
KFMeas makeRTKLCs1(KFMeas& kfMeas, KFState& kfState)
{
    int meas = 0;

    vector<Triplet<double>>        tripletList;
    decltype(KFMeas::metaDataMaps) newMetaDataMaps;
    decltype(KFMeas::obsKeys)      newObsKeys;

    for (auto duo : {Duo{kfState.kfIndexMap, kfMeas.H}, Duo{kfMeas.noiseIndexMap, kfMeas.H_star}})
        for (auto& [kfKey, index] : duo.indexMap)
        {
            if (kfKey.type != KF::PHASE_BIAS && kfKey.type != KF::CODE_BIAS)
                continue;
            for (int i_2 = 0; i_2 < kfMeas.obsKeys.size(); i_2++)
            {
                double coeff_2 = duo.designMatrix(i_2, index);
                if (coeff_2 == 0)
                    continue;
                for (int i_1 = 0; i_1 < i_2; i_1++)
                {
                    double coeff_1 = duo.designMatrix(i_1, index);
                    if (coeff_1 == 0)
                        continue;
                    {
                        if (kfMeas.metaDataMaps[i_1]["RTKcombined1"])
                        {
                            continue;
                        }
                        if (kfMeas.metaDataMaps[i_2]["RTKcombined1"])
                        {
                            continue;
                        }

                        auto& obsKey1 = kfMeas.obsKeys[i_1];
                        auto& obsKey2 = kfMeas.obsKeys[i_2];

                        if (kfKey.str.empty() == false)
                        {
                            continue;
                        }

                        if (obsKey1.str == obsKey2.str)
                        {
                            continue;
                        }

                        // these measurements both share a common satellite bias, remove it.

                        // std::cout << kfKey << " " << kfMeas.obsKeys[i_2]  << " " <<
                        // kfMeas.obsKeys[i_1] << "\n";

                        tripletList.push_back({meas, i_1, +coeff_2});
                        tripletList.push_back({meas, i_2, -coeff_1});
                        meas++;

                        auto& obsKey_1 = kfMeas.obsKeys[i_1];
                        auto& obsKey_2 = kfMeas.obsKeys[i_2];

                        auto newObsKey = obsKey_2;

                        newObsKey.str.clear();

                        newObsKey.comment = (string)obsKey_1 + "-" + (string)obsKey_2 + " " +
                                            obsKey_1.comment + "-" + obsKey_2.comment;

                        newObsKeys.push_back(newObsKey);

                        // copy metadata into the new measurement
                        map<string, void*> newMetaData;
                        for (auto& [id, value] : kfMeas.metaDataMaps[i_1])
                        {
                            newMetaData[id] = value;
                        }
                        for (auto& [id, value] : kfMeas.metaDataMaps[i_2])
                        {
                            newMetaData[id + "_alt"] = value;
                        }
                        newMetaData["explain"] = (void*)true;

                        newMetaDataMaps.push_back(std::move(newMetaData));

                        kfMeas.metaDataMaps[i_1]["RTKcombined"] = (void*)true;
                        kfMeas.metaDataMaps[i_2]["RTKcombined"] = (void*)true;
                    }
                }
            }
        }

    for (int i = 0; i < kfMeas.obsKeys.size(); i++)
    {
        if (kfMeas.metaDataMaps[i]["pseudoObs"] == (void*)false)
        {
            continue;
        }

        // need to keep this measurement even if its not a valid rtk one, copy it over

        tripletList.push_back({meas, i, 1});
        meas++;

        newObsKeys.push_back(kfMeas.obsKeys[i]);
        newMetaDataMaps.push_back(kfMeas.metaDataMaps[i]);
    }

    return KFMeas(
        kfMeas,
        std::move(tripletList),
        std::move(newObsKeys),
        std::move(newMetaDataMaps)
    );
}

/** Replace individual measurements with linear combinations
 */
KFMeas makeRTKLCs2(KFMeas& kfMeas, KFState& kfState)
{
    int meas = 0;

    vector<Triplet<double>>        tripletList;
    decltype(KFMeas::metaDataMaps) newMetaDataMaps;
    decltype(KFMeas::obsKeys)      newObsKeys;

    for (auto duo : {Duo{kfState.kfIndexMap, kfMeas.H}, Duo{kfMeas.noiseIndexMap, kfMeas.H_star}})
        for (auto& [kfKey, index] : duo.indexMap)
        {
            if (kfKey.type != KF::PHASE_BIAS && kfKey.type != KF::CODE_BIAS)
                continue;
            for (int i_2 = 0; i_2 < kfMeas.obsKeys.size(); i_2++)
            {
                double coeff_2 = duo.designMatrix(i_2, index);
                if (coeff_2 == 0)
                    continue;
                for (int i_1 = 0; i_1 < i_2; i_1++)
                {
                    double coeff_1 = duo.designMatrix(i_1, index);
                    if (coeff_1 == 0)
                        continue;
                    {
                        // if (kfMeas.metaDataMaps[i_1]["RTKcombined2"])		{	continue;	}
                        if (kfMeas.metaDataMaps[i_2]["RTKcombined2"])
                        {
                            continue;
                        }

                        auto& obsKey1 = kfMeas.obsKeys[i_1];
                        auto& obsKey2 = kfMeas.obsKeys[i_2];

                        if (kfKey.str.empty())
                        {
                            continue;
                        }

                        if (obsKey1.Sat == obsKey2.Sat)
                        {
                            continue;
                        }

                        // these measurements both share a common receiver bias, remove it.

                        tripletList.push_back({meas, i_1, +coeff_2});
                        tripletList.push_back({meas, i_2, -coeff_1});
                        meas++;

                        auto& obsKey_1 = kfMeas.obsKeys[i_1];
                        auto& obsKey_2 = kfMeas.obsKeys[i_2];

                        auto newObsKey = obsKey_2;
                        newObsKey.Sat  = SatSys();

                        newObsKey.comment = (string)obsKey_1 + "-" + (string)obsKey_2 + " " +
                                            obsKey_1.comment + "-" + obsKey_2.comment;

                        newObsKeys.push_back(newObsKey);

                        // copy metadata into the new measurement
                        map<string, void*> newMetaData;
                        for (auto& [id, value] : kfMeas.metaDataMaps[i_1])
                        {
                            newMetaData[id] = value;
                        }
                        for (auto& [id, value] : kfMeas.metaDataMaps[i_2])
                        {
                            newMetaData[id + "_alt"] = value;
                        }
                        newMetaData["explain"] = (void*)true;

                        newMetaDataMaps.push_back(std::move(newMetaData));

                        kfMeas.metaDataMaps[i_1]["RTKcombined2"] = (void*)true;
                        kfMeas.metaDataMaps[i_2]["RTKcombined2"] = (void*)true;
                    }
                }
            }
        }

    for (int i = 0; i < kfMeas.obsKeys.size(); i++)
    {
        if (kfMeas.metaDataMaps[i]["pseudoObs"] == (void*)false)
        {
            continue;
        }

        // need to keep this measurement even if its not a valid rtk one, copy it over

        tripletList.push_back({meas, i, 1});
        meas++;

        newObsKeys.push_back(kfMeas.obsKeys[i]);
        newMetaDataMaps.push_back(kfMeas.metaDataMaps[i]);
    }

    return KFMeas(
        kfMeas,
        std::move(tripletList),
        std::move(newObsKeys),
        std::move(newMetaDataMaps)
    );
}

void mergeCorrelated(Trace& trace, KFState& kfState, KFMeasEntryList& kfMeasEntryList)
{
    // count the number of measurements that refer to a given state in the design entry map
    map<KFKey, int> refCountMap;

    static map<KFKey, bool> dontMergeMap;

    for (auto& kfMeasEntry : kfMeasEntryList)
        for (auto& [key, value] : kfMeasEntry.designEntryMap)
        {
            if (kfMeasEntry.valid == false)
            {
                continue;
            }

            if (value == 0)
            {
                continue;
            }

            refCountMap[key]++;
        }

    for (auto& kfMeasEntry : kfMeasEntryList)
    {
        if (kfMeasEntry.valid == false)
        {
            continue;
        }

        map<KFKey, double> correlatedMap;

        for (auto& [key, value] : kfMeasEntry.designEntryMap)
        {
            if (value == 0)
            {
                continue;
            }

            if (dontMergeMap[key])
            {
                continue;
            }

            if (refCountMap[key] != 1)
            {
                dontMergeMap[key] = true;
                continue;
            }

            // this key appears in this measurement only.

            correlatedMap[key] = value;
        }

        if (correlatedMap.size() < 2)
        {
            continue;
        }

        stringstream ss;

        ss << "\n"
           << "Combining ";

        int scalar = 1;

        KFKey bigKey;
        for (auto& [key, value] : correlatedMap)
        {
            if (bigKey.type == KF::NONE)
            {
                bigKey = key;
            }
            else
            {
                scalar *= 100;
                bigKey.num += scalar * key.num;
                bigKey.comment += (string) ", " + key.comment;
            }

            ss << "\t[" << key.commaString() << "]: \t" << value << ",";

            kfMeasEntry.designEntryMap[key] = 0;
        }

        // add the new key and its design entry

        ss << "\tto create " << bigKey;

        try
        {
            bool stateCreated = kfState.addPseudoState(bigKey, correlatedMap);

            if (stateCreated)
            {
                trace << ss.str();
            }

            kfMeasEntry.designEntryMap[bigKey] = 1;
        }
        catch (...)
        {
            // trying to re-merge can alrady merged state
            BOOST_LOG_TRIVIAL(warning) << "Removing measurement " << kfMeasEntry.obsKey;
            kfMeasEntry.valid = false;
        }
    }
}

/** Prepare receiver clocks using SPP values to minimise pre-fit residuals
 */
void updateRecClocks(
    Trace&       trace,        ///< Trace to output to
    ReceiverMap& receiverMap,  ///< List of receivers containing observations for this epoch
    KFState&     kfState       ///< Kalman filter object containing the network state parameters
)
{
    if (acsConfig.adjust_rec_clocks_by_spp == false)
    {
        return;
    }

    for (auto& [id, rec] : receiverMap)
    {
        auto trace = getTraceFile(rec);

        if (rec.isPseudoRec)
        {
            continue;
        }

        if (rec.sol.status != E_Solution::SINGLE)
        {
            trace << "\n"
                  << "Receiver clock of " << id << " won't be adjusted due to bad SPP";
            continue;
        }

        auto& recOpts = acsConfig.getRecOpts(id);

        InitialState clkInit = initialStateFromConfig(recOpts.clk);

        if (clkInit.estimate == false)
        {
            continue;
        }

        KFKey clkKey;
        clkKey.type = KF::REC_CLOCK;
        clkKey.str  = id;

        double   recClk_m = 0;
        E_Source found    = kfState.getKFValue(
            clkKey,
            recClk_m
        );  // Get filtered rec clock of last epoch (not updated yet)

        if (found == E_Source::KALMAN && rec.sol.clkAdjustReady)
        {
            KFKey rateKey = clkKey;
            rateKey.type  = KF::REC_CLOCK_RATE;

            double clkRate = 0;
            found          = kfState.getKFValue(rateKey, clkRate);

            double dt = (tsync - kfState.time).to_double();
            recClk_m += clkRate * dt;     // Expected rec clock prediction
            double recClkAdj = rec.sol.sppClk + rec.sol.sppPppClkOffset -
                               recClk_m;  // Adjust rec clock prediction by SPP clock change

            // Do rounding if only adjust receiver clock jumps (i.e. integer times of
            // half-milliseconds)
            if (acsConfig.adjust_clocks_for_jumps_only)
            {
                const double scalar = 1000 * 2 / CLIGHT;

                double halfMilliseconds = recClkAdj * scalar;

                recClkAdj = round(halfMilliseconds) / scalar;

                if (recClkAdj)
                {
                    trace << "\n"
                          << "Jump of " << halfMilliseconds * 0.5 << "ms found, rounding";
                }
            }

            trace << "\n"
                  << tsync << "\tAdjusting " << clkKey.str << " clock by " << recClkAdj
                  << "\t- Pre-adjustment: " << recClk_m
                  << "\t- Post-adjustment: " << recClk_m + recClkAdj;

            if (recClkAdj)
            {
                kfState.setKFTrans(clkKey, KFState::oneKey, recClkAdj, clkInit);

                InitialState rateInit = initialStateFromConfig(recOpts.clk_rate);
                if (rateInit.estimate && found == E_Source::KALMAN)
                {
                    double clkRateAjd = recClkAdj / dt;
                    kfState.setKFTrans(rateKey, KFState::oneKey, clkRateAjd, rateInit);
                }
            }
        }
    }
}

/** Prepare stec values clocks to minimise residuals to klobuchar model
 */
void updateAvgIonosphere(
    Trace&   trace,   ///< Trace to output to
    GTime    time,    ///< Time
    KFState& kfState  ///< Kalman filter object containing the network state parameters
)
{
    if (acsConfig.minimise_ionosphere_offsets == false)
    {
        return;
    }

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::IONO_STEC)
        {
            continue;
        }

        if (key.rec_ptr == nullptr)
        {
            continue;
        }

        auto& rec = *key.rec_ptr;

        auto& satStat = rec.satStatMap[key.Sat];

        double diono  = 0;
        double dummy  = 0;
        double dummy2 = 0;
        bool   pass   = ionoModel(
            time,
            rec.pos,
            satStat,
            E_IonoMapFn::KLOBUCHAR,
            E_IonoMode::BROADCAST,
            0,
            dummy,
            diono,
            dummy2
        );
        if (pass == false)
        {
            continue;
        }

        double alpha = 40.3e16 / SQR(CLIGHT / genericWavelength[F1]);

        double ionosphereStec = diono / alpha;

        // update the mu value but dont use the state thing - it will re-add it after its deleted
        //  kfState.addKFState(key, init);
        kfState.gaussMarkovMuMap[key] = ionosphereStec;
    }
}

/** Prepare Satellite clocks to minimise residuals to broadcast orbits
 * Provide a weak tiedown using mu values, which the state will attempt to exponentially decay
 * toward using the configured tau value in stateTransition
 */
void updateAvgOrbits(
    Trace&   trace,   ///< Trace to output to
    GTime    time,    ///< Time
    KFState& kfState  ///< Kalman filter object containing the network state parameters
)
{
    if (acsConfig.minimise_sat_orbit_offsets == false)
    {
        return;
    }

    ERPValues erpv = getErp(nav.erp, time);

    FrameSwapper frameSwapper(time, erpv);

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::ORBIT || key.num != 0)
        {
            continue;
        }

        SatPos satPos;
        satPos.Sat        = key.Sat;
        satPos.satNav_ptr = &nav.satNavMap[key.Sat];

        bool pass = satPosBroadcast(trace, time, time, satPos, nav);
        if (pass == false)
        {
            continue;
        }

        auto& satOpts = acsConfig.getSatOpts(key.Sat);

        satPos.rSatEci0 = frameSwapper(satPos.rSatCom);

        for (int i = 0; i < 3; i++)
        {
            InitialState init = initialStateFromConfig(satOpts.orbit, i);

            init.mu = satPos.rSatEci0(i);

            // update the mu value,
            kfState.addKFState(key, init);
        }
    }
}

/** Prepare satellite clocks to minimise residuals to broadcast clocks.
 * Provide a weak tiedown using mu values, which the state will attempt to exponentially decay
 * toward using the configured tau value in stateTransition
 */
void updateAvgClocks(
    Trace&   trace,   ///< Trace to output to
    GTime    time,    ///< Time
    KFState& kfState  ///< Kalman filter object containing the network state parameters
)
{
    if (acsConfig.minimise_sat_clock_offsets.enable == false)
    {
        return;
    }

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::SAT_CLOCK)
        {
            continue;
        }

        auto&  Sat = key.Sat;
        SatPos satPosBrdc;
        SatPos satPosKf;
        satPosBrdc.Sat = Sat;
        satPosKf.Sat   = Sat;

        bool pass = satClkBroadcast(trace, time, time, satPosBrdc, nav);
        if (pass == false)
        {
            continue;
        }

        pass = satClkKalman(trace, time, satPosKf, &kfState);
        if (pass == false)
        {
            continue;
        }

        auto& satOpts = acsConfig.getSatOpts(Sat);

        // Restore tau value from config
        InitialState init = initialStateFromConfig(satOpts.clk);

        double satClkBrdc = satPosBrdc.satClk * CLIGHT;
        double satClkKf   = satPosKf.satClk * CLIGHT;
        double satClkDiff = satClkBrdc - satClkKf;

        if (abs(satClkDiff) > acsConfig.minimise_sat_clock_offsets.max_offset)
        {
            // Exclude this satellite clock from alignment if diff between estimated and broadcast
            // satellite clock offsets > 10 m
            trace << "\nLarge clock diff > " << acsConfig.minimise_sat_clock_offsets.max_offset
                  << " (m), excluding satellite from braodcast clock alignment:" << "\tSat: "
                  << Sat.id() << "\tBrdc sat clock (m): " << satClkBrdc
                  << "\tKf sat clock (m): " << satClkKf << "\tDiff: " << satClkDiff;

            init.tau = -1;  // Disable FOGM so that this satellite clock won't be tied down to old
                            // mu value		// Eugene: What if the clock doesn't jump
        }
        else
        {
            // Align this satellite clock with broadcast ephemeris
            init.mu = satClkBrdc;
        }

        // update tau & mu values
        kfState.addKFState(key, init);
    }
}

KFState propagateUncertainty(Trace& trace, KFState& kfState)
{
    MatrixXd F1;

    KFMeasEntryList kfMeasEntryList;

    map<KFKey, KFMeasEntry> kfMeasEntryMap;

    string& reference_clock = acsConfig.reference_clock;
    string& reference_bias  = acsConfig.reference_bias;

    static int pivotNum = 0;

    // add measEntries for each suitable state in the map.
    // 	if (0)
    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if ((key.type < KF::BEGIN_CLOCK_STATES || key.type > KF::END_CLOCK_STATES) &&
            (key.type != KF::PHASE_BIAS))
        {
            continue;
        }

        // use first receiver clock for auto reference
        if (reference_clock == "<AUTO>" && key.type == KF::REC_CLOCK && key.num == 0)
        {
            reference_clock = key.str;
        }

        // use first satellite bias for auto reference
        if (reference_bias == "<AUTO>" && key.type == KF::PHASE_BIAS && key.str.empty())
        {
            reference_bias = key.Sat.id();
            pivotNum       = key.num;
        }

        auto subKey = key;
        if (key.type > KF::BEGIN_CLOCK_STATES && key.type < KF::END_CLOCK_STATES)
        {
            // accumulate all clock nums together
            subKey.num = 0;
        }

        auto& kfMeasEntry = kfMeasEntryMap[subKey];

        kfMeasEntry.addDsgnEntry(key, +1);

        string newComment = kfMeasEntry.obsKey.comment;
        if (newComment.empty())
        {
            newComment += "=>";
        }
        newComment += " + [" + key.commaString() + "]";

        kfMeasEntry.obsKey = subKey;

        kfMeasEntry.obsKey.comment = newComment;
    }

    // add the right hand side for all the clock states
    for (auto& [id, entry] : kfMeasEntryMap)
    {
        if (id.type != KF::REC_CLOCK && id.type != KF::SAT_CLOCK)
        {
            continue;
        }

        if (reference_clock != "NO_REFERENCE")
            for (auto& [pivotKey, pivotEntry] : kfState.kfIndexMap)
            {
                if (pivotKey.type != KF::REC_CLOCK)
                {
                    continue;
                }

                if (pivotKey.str != reference_clock)
                {
                    continue;
                }

                entry.addDsgnEntry(pivotKey, -1);

                entry.obsKey.comment += " - [" + pivotKey.commaString() + "]";
            }

        kfMeasEntryList.push_back(entry);
    }

    // add the right hand side for all the bias states
    for (auto& [id, entry] : kfMeasEntryMap)
    {
        if (id.type != KF::PHASE_BIAS)
        {
            continue;
        }

        if (reference_bias != "NO_REFERENCE")
            for (auto& [pivotKey, pivotEntry] : kfState.kfIndexMap)
            {
                if (pivotKey.type != KF::PHASE_BIAS)
                {
                    continue;
                }

                if (pivotKey.Sat.id() != reference_bias || pivotKey.num != pivotNum)
                {
                    continue;
                }

                entry.addDsgnEntry(pivotKey, -1);

                entry.obsKey.comment += " - [" + pivotKey.commaString() + "]";
            }

        kfMeasEntryList.push_back(entry);
    }

    KFState propagatedState;

    if (kfMeasEntryList.empty())
    {
        return propagatedState;
    }

    KFMeas kfMeas(kfState, kfMeasEntryList);

    propagatedState.time = kfState.time;
    propagatedState.P    = kfMeas.H * kfState.P * kfMeas.H.transpose();
    propagatedState.x    = kfMeas.H * kfState.x;
    propagatedState.dx   = kfMeas.H * kfState.dx;

    propagatedState.kfIndexMap.clear();

    int i = 0;
    for (auto& obsKey : kfMeas.obsKeys)
    {
        propagatedState.kfIndexMap[obsKey] = i;
        i++;
    }

    return propagatedState;
}

void chunkFilter(
    Trace&                      trace,
    KFState&                    kfState,
    KFMeas&                     kfMeas,
    ReceiverMap&                receiverMap,
    map<string, FilterChunk>&   filterChunkMap,
    map<string, std::ofstream>& traceList
)
{
    filterChunkMap.clear();
    // Handle case when chunking is disabled
    if (acsConfig.pppOpts.receiver_chunking == false)
    {
        ///@todo temporary fix to double printing. removed the concept of "ALL" chunk
        return;

        FilterChunk filterChunk;
        filterChunk.id        = "ALL";
        filterChunk.begH      = 0;
        filterChunk.numH      = kfMeas.H.rows();
        filterChunk.begX      = 0;
        filterChunk.numX      = kfState.x.rows();
        filterChunk.trace_ptr = &trace;
        filterChunkMap["ALL"] = filterChunk;
        return;
    }

    map<string, int> begH;
    map<string, int> endH;
    map<string, int> begX;
    map<string, int> endX;

    // Find chunks based on state variables
    for (auto& [kfKey, x] : kfState.kfIndexMap)
    {
        if (kfKey.type == KF::ONE)
        {
            continue;
        }

        string chunkId = kfKey.str;

        // Initialize or update X bounds for ALL receivers with states
        if (begX.find(chunkId) == begX.end())
        {
            begX[chunkId] = x;
        }
        {
            endX[chunkId] = x;
        }

        // Find corresponding measurements in H matrix
        for (int h = 0; h < kfMeas.H.rows(); h++)
        {
            // std::cout << "Checking measurement " << h << " for chunkId " << chunkId << " : " <<
            // kfMeas.H(h,x) <<
            // "\n";
            if (kfMeas.H(h, x))
            {
                // Fixed initialization bug: check if chunkId exists before accessing
                if (begH.find(chunkId) == begH.end() || h < begH[chunkId])
                {
                    begH[chunkId] = h;
                }
                if (endH.find(chunkId) == endH.end() || h > endH[chunkId])
                {
                    endH[chunkId] = h;
                }
            }
        }
    }

    // Early return if no chunks were found
    if (begX.empty())
    {
        BOOST_LOG_TRIVIAL(warning) << "No filter chunks found - creating default chunk";
        FilterChunk filterChunk;
        filterChunk.id            = "DEFAULT";
        filterChunk.begH          = 0;
        filterChunk.numH          = kfMeas.H.rows();
        filterChunk.begX          = 0;
        filterChunk.numX          = kfState.x.rows();
        filterChunk.trace_ptr     = &trace;
        filterChunkMap["DEFAULT"] = filterChunk;
        return;
    }

    bool chunkH = true;
    bool chunkX = true;

    // Check for overlapping entries in both H and X dimensions
    for (auto& [str1, beg1] : begX)      // Changed from begH to begX
        for (auto& [str2, beg2] : begX)  // Changed from begH to begX
        {
            if (str1 >= str2)
                continue;  // Avoid double-checking and self-comparison

            auto& end1 = endX[str1];
            auto& end2 = endX[str2];

            // Check X overlap
            if ((beg2 <= end1 && end2 >= beg1))
            {
                chunkX = false;
                BOOST_LOG_TRIVIAL(debug)
                    << "X overlap detected between chunks " << str1 << " and " << str2;
            }

            // Check H overlap if both chunks exist in H maps
            if (begH.find(str1) != begH.end() && begH.find(str2) != begH.end())
            {
                auto& begH1 = begH[str1];
                auto& endH1 = endH[str1];
                auto& begH2 = begH[str2];
                auto& endH2 = endH[str2];

                if ((begH2 <= endH1 && endH2 >= begH1))
                {
                    chunkH = false;
                    BOOST_LOG_TRIVIAL(debug)
                        << "H overlap detected between chunks " << str1 << " and " << str2;
                }
            }
        }

    // If overlaps detected, disable chunking
    if (chunkX == false || chunkH == false)
    {
        BOOST_LOG_TRIVIAL(warning) << "Overlaps detected - chunking disabled";
        return;
    }

    // Create filter chunks - iterate over ALL receivers with states
    for (auto& [str, dummy] : begX)  // Changed from begH to begX
    {
        FilterChunk filterChunk;

        // Set up trace pointer
        if (str.empty() == false)
        {
            auto recIt = receiverMap.find(str);
            if (recIt != receiverMap.end())
            {
                auto& rec             = recIt->second;
                traceList[str]        = getTraceFile(rec);
                filterChunk.trace_ptr = &traceList[str];
            }
            else
            {
                BOOST_LOG_TRIVIAL(warning) << "Receiver " << str << " not found in receiverMap";
                filterChunk.trace_ptr = &trace;
            }
            filterChunk.id = str;
        }
        else
        {
            filterChunk.id        = "GLOBAL";
            filterChunk.trace_ptr = &trace;
        }

        // Set H dimensions - check if this receiver has measurements
        if (chunkH && begH.find(str) != begH.end())
        {
            filterChunk.begH = begH[str];
            filterChunk.numH = endH[str] - begH[str] + 1;
        }
        else
        {
            // No measurements for this receiver
            filterChunk.begH = 0;
            filterChunk.numH = 0;
        }

        // Set X dimensions - all receivers with states should have X dimensions
        if (chunkX && begX.find(str) != begX.end())
        {
            filterChunk.begX = begX[str];
            filterChunk.numX = endX[str] - begX[str] + 1;
        }
        else
        {
            filterChunk.begX = 0;
            filterChunk.numX = kfState.x.rows();
        }

        // Validate chunk dimensions
        if (filterChunk.numX < 0)
        {
            BOOST_LOG_TRIVIAL(error)
                << "Invalid chunk dimensions for " << str << ": numH=" << filterChunk.numH
                << ", numX=" << filterChunk.numX;
            continue;
        }

        filterChunkMap[str] = filterChunk;
    }

    // Add dummy chunks for states not covered by any receiver chunk (required for RTS)
    // Assume all chunks are in receiver order == state order
    {
        int                      x = 0;
        map<string, FilterChunk> filterChunkExtras;

        auto addDummyChunk = [&](int lastX, int nextX)
        {
            if (nextX <= lastX + 1)
                return;  // No gap to fill

            FilterChunk filterChunk;
            filterChunk.id =
                "dummy_" + std::to_string(lastX + 1) + "_to_" + std::to_string(nextX - 1);
            filterChunk.begX      = lastX + 1;
            filterChunk.numX      = nextX - lastX - 1;
            filterChunk.begH      = 0;
            filterChunk.numH      = 0;
            filterChunk.trace_ptr = &trace;

            filterChunkExtras[filterChunk.id] = filterChunk;
        };

        // Sort chunks by begX to ensure proper ordering
        vector<pair<string, FilterChunk*>> sortedChunks;
        for (auto& [str, filterChunk] : filterChunkMap)
        {
            sortedChunks.push_back({str, &filterChunk});
        }
        std::sort(
            sortedChunks.begin(),
            sortedChunks.end(),
            [](const auto& a, const auto& b) { return a.second->begX < b.second->begX; }
        );

        for (auto& [str, filterChunkPtr] : sortedChunks)
        {
            auto& filterChunk = *filterChunkPtr;

            if (filterChunk.begX > x)
            {
                addDummyChunk(x - 1, filterChunk.begX);
            }

            x = filterChunk.begX + filterChunk.numX;
        }

        // Check if we need a final dummy chunk
        if (x < kfState.x.rows())
        {
            addDummyChunk(x - 1, kfState.x.rows());
        }

        // Add dummy chunks to main map
        for (auto& [id, fc] : filterChunkExtras)
        {
            filterChunkMap[id] = std::move(fc);
        }
    }

    // Handle chunk size configuration
    if (acsConfig.pppOpts.chunk_size > 0)
    {
        map<string, FilterChunk> newFilterChunkMap;

        FilterChunk bigFilterChunk;

        int chunks =
            std::max(1, (int)((double)filterChunkMap.size() / acsConfig.pppOpts.chunk_size + 0.5));
        int chunkTarget = std::max(1, (int)((double)filterChunkMap.size() / chunks + 0.5));

        int count = 0;
        for (auto& [id, filterChunk] : filterChunkMap)
        {
            if (count == 0)
            {
                bigFilterChunk           = filterChunk;
                bigFilterChunk.trace_ptr = &trace;
            }
            else
            {
                bigFilterChunk.id += "-" + filterChunk.id;
            }

            int chunkEndX = filterChunk.begX + filterChunk.numX - 1;
            int chunkEndH = filterChunk.begH + filterChunk.numH - 1;

            // Update bounds to encompass all merged chunks
            bigFilterChunk.begX = std::min(bigFilterChunk.begX, filterChunk.begX);
            bigFilterChunk.begH = std::min(bigFilterChunk.begH, filterChunk.begH);

            int bigEndX = bigFilterChunk.begX + bigFilterChunk.numX - 1;
            int bigEndH = bigFilterChunk.begH + bigFilterChunk.numH - 1;

            if (chunkEndX > bigEndX)
            {
                bigFilterChunk.numX = chunkEndX - bigFilterChunk.begX + 1;
            }
            if (chunkEndH > bigEndH)
            {
                bigFilterChunk.numH = chunkEndH - bigFilterChunk.begH + 1;
            }

            count++;

            if (count == chunkTarget)
            {
                newFilterChunkMap[bigFilterChunk.id] = bigFilterChunk;
                count                                = 0;
            }
        }

        // Add final chunk if there are remaining
        if (count > 0)
        {
            newFilterChunkMap[bigFilterChunk.id] = bigFilterChunk;
        }

        filterChunkMap = std::move(newFilterChunkMap);
    }

    // Log chunk information
    BOOST_LOG_TRIVIAL(debug) << "Created " << filterChunkMap.size() << " filter chunks";
    for (auto& [id, fc] : filterChunkMap)
    {
        BOOST_LOG_TRIVIAL(debug) << "Chunk " << id << ": X[" << fc.begX << ":"
                                 << (fc.begX + fc.numX - 1) << "]" << ", H[" << fc.begH << ":"
                                 << (fc.begH + fc.numH - 1) << "]" << ", size of H"
                                 << kfMeas.H.rows() << "x" << kfMeas.H.cols() << ", size of X"
                                 << kfState.x.rows() << "x" << kfState.x.cols();
    }
}

/**
 * @brief Checks if a reset interval has been reached between two epochs.
 *
 * Determines whether the current epoch (`epoch`) or the transition
 * from the previous epoch (`prev_epoch`) to the current epoch crosses a
 * specified reset interval (`reset_interval`).
 *
 * @param epoch The current epoch time.
 * @param prev_epoch The previous epoch time.
 * @param reset_interval The interval at which resets occur.
 * @return True if a reset interval is reached or crossed, false otherwise.
 */
bool isIntervalReset(double epoch, double prev_epoch, double reset_interval)
{
    bool reset_epoch          = std::fmod(epoch, reset_interval) == 0;
    bool prev_epoch_reset     = std::fmod(prev_epoch, reset_interval) == 0;
    bool reset_between_epochs = int(prev_epoch / reset_interval) < int(epoch / reset_interval);

    return reset_epoch || (!prev_epoch_reset && reset_between_epochs);
}

/**
 * @brief Checks if a specific time reset has occurred between two epochs.
 *
 * Determines whether a given epoch or the transition between two epochs
 * corresponds to any of the specified reset times within a day. A reset time can
 * either match the exact time of the epoch or occur between the two epochs.
 *
 * @param epoch The current epoch time in seconds since the start of the day.
 * @param prev_epoch The previous epoch time in seconds since the start of the day.
 * @param resetTimes A vector of reset times in seconds within a day (0 to 86400).
 *
 * @return True If a reset time matches the current epoch or occurs between the
 *         previous and current epochs, False otherwise.
 */
bool isSpecificTimeReset(double epoch, double prev_epoch, const std::vector<double>& resetTimes)
{
    double seconds_in_day = 86400;
    double epoch_mod      = std::fmod(epoch, seconds_in_day);
    double prev_epoch_mod = std::fmod(prev_epoch, seconds_in_day);

    for (double resetTime : resetTimes)
    {
        double reset_mod            = std::fmod(resetTime, seconds_in_day);
        bool   reset_epoch          = epoch_mod == reset_mod;
        bool   prev_epoch_reset     = prev_epoch_mod == reset_mod;
        bool   reset_between_epochs = (prev_epoch_mod < reset_mod && epoch_mod > reset_mod) ||
                                    (prev_epoch_mod > epoch_mod && reset_mod == 0);

        if (reset_epoch || reset_between_epochs)
        {
            return true;
        }
    }
    return false;
}

void updatePseudoPulses(Trace& trace, KFState& kfState)
{
    static map<KFKey, double> nextEpochMap;

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::ORBIT)
        {
            continue;
        }

        auto& satOpts = acsConfig.getSatOpts(key.Sat);
        if (satOpts.pseudoPulses.enable == false ||
            (satOpts.pseudoPulses.interval == 0 && satOpts.pseudoPulses.epochs.empty()))
        {
            continue;
        }

        double yds[3];
        time2yds(tsync, yds, E_TimeSys::GPST);
        double sod        = yds[2];
        double prev_epoch = sod - acsConfig.epoch_interval;

        bool resetEpochInterval = isIntervalReset(sod, prev_epoch, satOpts.pseudoPulses.interval);
        bool resetEpochSpecific = isSpecificTimeReset(sod, prev_epoch, satOpts.pseudoPulses.epochs);

        if (!resetEpochInterval && !resetEpochSpecific)
        {
            return;
        }

        if (key.num < 3)
            kfState.setExponentialNoise(key, {SQR(satOpts.pseudoPulses.pos_proc_noise)});
        else
            kfState.setExponentialNoise(key, {SQR(satOpts.pseudoPulses.vel_proc_noise)});
    }
}

void resetFilterbyConfig(Trace& trace, KFState& kfState)
{
    if (acsConfig.pppOpts.filter_reset_enable == false ||
        (acsConfig.pppOpts.reset_interval == 0 && acsConfig.pppOpts.reset_epochs.empty()))
    {
        return;
    }

    if (epoch == 0)
    {
        // No need to reset as it is the first epoch.
        return;
    }

    double yds[3];
    time2yds(tsync, yds, E_TimeSys::GPST);
    double sod        = yds[2];
    double prev_epoch = sod - acsConfig.epoch_interval;

    bool resetEpochInterval = isIntervalReset(sod, prev_epoch, acsConfig.pppOpts.reset_interval);
    bool resetEpochSpecific = isSpecificTimeReset(sod, prev_epoch, acsConfig.pppOpts.reset_epochs);

    if (!resetEpochInterval && !resetEpochSpecific)
    {
        return;
    }

    // here will be a message to mongo about the reset (waiting on another PR to go in)

    auto& reset_states = acsConfig.pppOpts.reset_states;

    bool foundAll =
        (std::find(reset_states.begin(), reset_states.end(), KF::ALL) != reset_states.end());

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type == KF::ONE)
        {
            continue;
        }

        if (foundAll ||
            std::find(reset_states.begin(), reset_states.end(), key.type) != reset_states.end())
        {
            trace << "\n"
                  << "State removed due to reset config: " << key;
            kfState.removeState(key);
        }
    }
    // generate a string of the reset_states values.
    string reset_states_str = "";
    for (auto& state : reset_states)
    {
        reset_states_str += std::string(enum_to_string(state)) + " ";
    }
    mongoEditing("", "", tsync, "reset", "", 1, reset_states_str);
}

void removeBadAmbiguities(Trace& trace, KFState& kfState, ReceiverMap& receiverMap);

void removeBadSatellites(Trace& trace, KFState& kfState);

void removeBadReceivers(Trace& trace, KFState& kfState, ReceiverMap& receiverMap);

void removeBadIonospheres(Trace& trace, KFState& kfState);

void checkOrbits(Trace& trace, KFState& kfState);

void updateFilter(
    Trace&       trace,        ///< Trace to output to
    ReceiverMap& receiverMap,  ///< List of receivers containing observations for this epoch
    KFState&     kfState       ///< Kalman filter object containing the network state parameters
)
{
    removeBadSatellites(trace,
                        kfState);  // todo Eugene: revisit this as it doesn't work well
    removeBadReceivers(trace, kfState, receiverMap);  // todo Eugene: revisit this as well
    removeBadAmbiguities(trace, kfState, receiverMap);
    removeBadIonospheres(trace, kfState);
    resetFilterbyConfig(trace, kfState);
    updateRecClocks(trace, receiverMap, kfState);
    updateAvgClocks(trace, tsync, kfState);
    updateAvgOrbits(trace, tsync, kfState);
    updateAvgIonosphere(trace, tsync, kfState);
    updatePseudoPulses(trace, kfState);
}

void perRecMeasurements(
    Trace&           trace,
    Receiver&        rec,
    ReceiverMap&     receiverMap,
    KFMeasEntryList& kfMeasEntryList,
    KFState&         kfState,
    KFState&         remoteState
)
{
    rec.pppTideCache.uninit();
    rec.pppEopCache.uninit();

    orbitPseudoObs(trace, rec, kfState, kfMeasEntryList);
    receiverUducGnss(trace, rec, kfState, kfMeasEntryList, remoteState);
    receiverSlr(trace, rec, kfState, kfMeasEntryList);
    receiverPseudoObs(trace, rec, kfState, kfMeasEntryList, receiverMap);

    if (acsConfig.pppOpts.ionoOpts.use_if_combo)
        makeIFLCs(trace, kfState, kfMeasEntryList);
}

void pppLinearCombinations(KFMeas& kfMeas, KFState& kfState)
{
    if (acsConfig.pppOpts.ionoOpts.use_gf_combo)
    {
        kfMeas = makeGFLCs(kfMeas, kfState);
    }
    if (acsConfig.pppOpts.use_rtk_combo)
    {
        kfMeas = makeRTKLCs1(kfMeas, kfState);
    }
    if (acsConfig.pppOpts.use_rtk_combo)
    {
        kfMeas = makeRTKLCs2(kfMeas, kfState);
    }
}

void pppPseudoObs(
    Trace&           trace,
    ReceiverMap&     receiverMap,
    KFState&         kfState,
    KFMeasEntryList& kfMeasEntryList
)
{
    DOCS_REFERENCE(Pseudo_Observations__);

    // apply external estimates
    ionoPseudoObs(trace, receiverMap, kfState, kfMeasEntryList);
    tropPseudoObs(trace, receiverMap, kfState, kfMeasEntryList);

    // apply pseudoobs to states available from before
    pseudoRecDcb(trace, kfState, kfMeasEntryList);
    ambgPseudoObs(trace, kfState, kfMeasEntryList);
    initPseudoObs(trace, kfState, kfMeasEntryList);
    satClockPivotPseudoObs(trace, kfState, kfMeasEntryList);
    filterPseudoObs(trace, kfState, kfMeasEntryList);
    phasePseudoObs(trace, kfState, kfMeasEntryList);
}

void ppp(
    Trace&       trace,        ///< Trace to output to
    ReceiverMap& receiverMap,  ///< List of receivers containing observations for this epoch
    KFState&     kfState,      ///< Kalman filter object containing the network state parameters
    KFState&     remoteState   ///< Optional pointer to remote kalman filter
)
{
    DOCS_REFERENCE(Main_Filter__);

    updateFilter(trace, receiverMap, kfState);

    // add process noise and dynamics to existing states as a prediction of current state
    if (kfState.assume_linearity == false)
    {
        BOOST_LOG_TRIVIAL(info) << " ------- DOING STATE TRANSITION       --------" << "\n";

        kfState.stateTransition(trace, tsync);

        if (acsConfig.output_predicted_states)
        {
            kfState.outputStates(trace, "/PREDICTED");

            mongoStates(
                kfState,
                {.suffix    = "/PREDICTED",
                 .instances = acsConfig.mongoOpts.output_states,
                 .queue     = acsConfig.mongoOpts.queue_outputs}
            );
        }
    }

    // prepare a map of lists of measurements for use below
    map<string, KFMeasEntryList> receiverKFEntryListMap;
    for (auto& [id, rec] : receiverMap)
    {
        receiverKFEntryListMap[rec.id] = KFMeasEntryList();
    }

    {
        BOOST_LOG_TRIVIAL(info) << " ------- CALCULATING PPP MEASUREMENTS --------" << "\n";

        // calculate the measurements for each receiver
#ifdef ENABLE_PARALLELISATION
        Eigen::setNbThreads(1);
#pragma omp parallel for
#endif
        for (int i = 0; i < receiverMap.size(); i++)
        {
            auto recIterator = receiverMap.begin();
            std::advance(recIterator, i);

            auto& [id, rec] = *recIterator;

            if (rec.ready == false || rec.obsList.empty())
            {
                continue;
            }

            auto& kfMeasEntryList = receiverKFEntryListMap[rec.id];

            perRecMeasurements(trace, rec, receiverMap, kfMeasEntryList, kfState, remoteState);
        }
        Eigen::setNbThreads(0);
    }

    // combine all lists of measurements into a single list
    KFMeasEntryList kfMeasEntryList;
    for (auto& [rec, receiverKFEntryList] : receiverKFEntryListMap)
        for (auto& kfMeasEntry : receiverKFEntryList)
        {
            if (kfMeasEntry.valid)
            {
                kfMeasEntryList.push_back(std::move(kfMeasEntry));
            }
        }

    pppPseudoObs(trace, receiverMap, kfState, kfMeasEntryList);

    if (acsConfig.pppOpts.merge_correlated_states)
    {
        mergeCorrelated(trace, kfState, kfMeasEntryList);
    }

    // use state transition to initialise new state elements
    BOOST_LOG_TRIVIAL(info) << " ------- DOING STATE TRANSITION       --------" << "\n";

    kfState.stateTransition(trace, tsync);

    if (acsConfig.output_initialised_states)
    {
        kfState.outputStates(trace, "/INITIALISED");

        mongoStates(
            kfState,
            {.suffix    = "/INITIALISED",
             .instances = acsConfig.mongoOpts.output_states,
             .queue     = acsConfig.mongoOpts.queue_outputs}
        );
    }

    std::sort(
        kfMeasEntryList.begin(),
        kfMeasEntryList.end(),
        [](KFMeasEntry& a, KFMeasEntry& b) { return a.obsKey < b.obsKey; }
    );

    KFMeas kfMeas(kfState, kfMeasEntryList, tsync);

    pppLinearCombinations(kfMeas, kfState);

    if (acsConfig.explain_measurements)
    {
        explainMeasurements(trace, kfMeas, kfState);
    }

    alternatePostfits(trace, kfMeas, kfState);

    if (kfState.lsqRequired)
    {
        BOOST_LOG_TRIVIAL(info) << "-------INITIALISING PPPPP USING LEAST SQUARES--------" << "\n";

        string suffix = "/LSQ";
        kfState.leastSquareInitStates(trace, kfMeas, suffix, false, true);

        kfState.outputStates(trace, suffix);
    }

    map<string, FilterChunk>   filterChunkMap;
    map<string, std::ofstream> traceList;  // keep in large scope as we're using pointers

    chunkFilter(trace, kfState, kfMeas, receiverMap, filterChunkMap, traceList);

    BOOST_LOG_TRIVIAL(info) << " ------- DOING PPPPP KALMAN FILTER    --------" << "\n";

    kfState.filterKalman(trace, kfMeas, "/PPP", true, &filterChunkMap);

    postFilterChecks(tsync, receiverMap, kfState, kfMeas);

    // output chunks if we are actually chunking still
    if (acsConfig.pppOpts.receiver_chunking)
        for (auto& [id, filterChunk] : filterChunkMap)
        {
            if (filterChunk.trace_ptr)
            {
                kfState.outputStates(
                    *filterChunk.trace_ptr,
                    (string) "/PPPChunk/" + id,
                    filterChunk.begX,
                    filterChunk.numX
                );
            }
        }
}
