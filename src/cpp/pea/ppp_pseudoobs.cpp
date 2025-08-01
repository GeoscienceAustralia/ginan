// #pragma GCC optimize ("O0")

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>
#include "architectureDocs.hpp"
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/common.hpp"
#include "common/constants.hpp"
#include "common/eigenIncluder.hpp"
#include "common/gTime.hpp"
#include "common/navigation.hpp"
#include "common/receiver.hpp"
#include "common/sinex.hpp"
#include "common/trace.hpp"
#include "iono/ionoModel.hpp"
#include "orbprop/coordinates.hpp"
#include "orbprop/orbitProp.hpp"
#include "pea/minimumConstraints.hpp"
#include "trop/tropModels.hpp"
#define PIVOT_MEAS_VARIANCE SQR(1E-5)

Architecture Pseudo_Observations__() {}

void initPseudoObs(
    Trace&           trace,    ///< Trace to output to
    KFState&         kfState,  ///< Kalman filter object containing the network state parameters
    KFMeasEntryList& kfMeasEntryList  ///< List to append kf measurements to
)
{
    if (kfMeasEntryList.empty() == false || epoch != 1)
    {
        return;
    }

    for (auto& [Sat, satNav] : nav.satNavMap)
    {
        if (acsConfig.process_sys[Sat.sys] == false)
        {
            continue;
        }

        auto& satOpts = acsConfig.getSatOpts(Sat);

        if (satOpts.exclude)
        {
            continue;
        }

        bool newState = false;

        for (int i = 0; i < 3; i++)
        {
            InitialState posInit = initialStateFromConfig(satOpts.orbit, i);
            InitialState velInit = initialStateFromConfig(satOpts.orbit, i + 3);

            if (posInit.estimate == false || posInit.x == 0 || velInit.x == 0)
            {
                continue;
            }

            KFKey satPosKey;
            KFKey satVelKey;

            satPosKey.type = KF::ORBIT;
            satPosKey.Sat  = Sat;
            satPosKey.num  = i;

            satVelKey.type = KF::ORBIT;
            satVelKey.Sat  = Sat;
            satVelKey.num  = i + 3;

            newState |= kfState.addKFState(satPosKey, posInit);
            newState |= kfState.addKFState(satVelKey, velInit);
        }

        if (newState == false)
        {
            continue;
        }

        addEmpStates(satOpts, kfState, Sat);
#ifdef _ESTIMATE_CRCD
        if (initialStateFromConfig(satOpts.cr).estimate)
        {
            InitialState CrInit = initialStateFromConfig(satOpts.cr, 0);
            kfState.addKFState({KF::CR, Sat, 0}, CrInit);
        }
        if (initialStateFromConfig(satOpts.cd).estimate)
        {
            InitialState CdInit = initialStateFromConfig(satOpts.cd, 0);
            kfState.addKFState({KF::CD, Sat, 0}, CdInit);
        }
#endif
        // addKFSatStates(satOpts.cr,       	kfState,	KF::CR,	        obs.Sat, 3);
    }
}

struct Pseudo
{
    GTime  time;
    KFKey  kfKey;
    double value = 0;
    double sigma = 0;
};

map<GTime, vector<Pseudo>> pseudoListMap;

void readPseudosFromFile(string& file)
{
    std::ifstream fileStream(file);
    if (!fileStream)
    {
        return;
    }

    std::ofstream output(file + "_read", std::fstream::app);
    if (!output)
    {
        BOOST_LOG_TRIVIAL(warning) << "Warning: Error opening read file '" << file << "_read'\n";
        return;
    }

    while (fileStream)
    {
        string line;

        getline(fileStream, line);

        output << line << "\n";

        vector<string> tokens;
        boost::split(tokens, line, boost::is_any_of("\t"));

        if (tokens.size() < 8)
        {
            continue;
        }

        for (auto& token : tokens)
        {
            boost::trim(token);
        }

        Pseudo pseudo;
        pseudo.kfKey.type = KF::_from_string_nocase(tokens[2].c_str());
        pseudo.kfKey.str  = tokens[3];
        pseudo.kfKey.Sat  = SatSys(tokens[4].c_str());
        pseudo.kfKey.num  = std::stoi(tokens[5].c_str());
        pseudo.value      = std::stod(tokens[6].c_str());
        pseudo.sigma      = std::stod(tokens[7].c_str());

        vector<string> timeTokens;
        boost::split(timeTokens, tokens[1], boost::is_any_of(" -:"));

        GEpoch epoch;
        epoch.year  = std::stoi(timeTokens[0].c_str());
        epoch.month = std::stoi(timeTokens[1].c_str());
        epoch.day   = std::stoi(timeTokens[2].c_str());
        epoch.hour  = std::stoi(timeTokens[3].c_str());
        epoch.min   = std::stoi(timeTokens[4].c_str());
        epoch.sec   = std::stoi(timeTokens[5].c_str());

        pseudo.time = epoch;

        // std::cout << "\n" << pseudo.time << " " << pseudo.kfKey;

        pseudoListMap[pseudo.time].push_back(pseudo);
    }

    remove(file.c_str());
}

void filterPseudoObs(Trace& trace, KFState& kfState, KFMeasEntryList& kfMeasEntryList)
{
    for (auto it = pseudoListMap.begin(); it != pseudoListMap.end();)
    {
        auto& [time, pseudoList] = *it;

        if (time > tsync)
        {
            it++;
            continue;
        }

        it = pseudoListMap.erase(it);

        for (auto& pseudo : pseudoList)
        {
            double filterVal;

            bool found = kfState.getKFValue(pseudo.kfKey, filterVal);

            if (found == false)
            {
                continue;
            }

            KFMeasEntry kfMeasEntry(&kfState);

            kfMeasEntry.obsKey = pseudo.kfKey;

            kfMeasEntry.obsKey.comment = "Fitler PseudoObs";

            kfMeasEntry.metaDataMap["pseudoObs"] = (void*)true;

            kfMeasEntry.setInnov(pseudo.value - filterVal);

            kfMeasEntry.addDsgnEntry(pseudo.kfKey, 1);

            pseudo.kfKey.type = KF::FILTER_MEAS;

            kfMeasEntry.addNoiseEntry(pseudo.kfKey, 1, SQR(pseudo.sigma));

            kfMeasEntryList.push_back(kfMeasEntry);
        }
    }
}

void orbitPseudoObs(
    Trace&           trace,    ///< Trace to output to
    Receiver&        rec,      ///< Receiver to perform calculations for
    KFState&         kfState,  ///< Kalman filter object containing the network state parameters
    KFMeasEntryList& kfMeasEntryList  ///< List to append kf measurements to
)
{
    GTime time = rec.obsList.front()->time;

    ERPValues erpv = getErp(nav.erp, time);

    FrameSwapper frameSwapper(time, erpv);

    for (auto& obs : only<PObs>(rec.obsList))
    {
        if (acsConfig.process_sys[obs.Sat.sys] == false)
        {
            continue;
        }

        auto& satOpts = acsConfig.getSatOpts(obs.Sat);

        if (satOpts.exclude)
        {
            continue;
        }

        SatPos satPos;
        satPos.Sat = obs.Sat;

        satpos(trace, time, time, satPos, satOpts.posModel.sources, E_OffsetType::COM, nav);

        satPos.rSatEci0 = frameSwapper(satPos.rSatCom, &satPos.satVel, &satPos.vSatEci0);

        VectorEcef rSatEcef;
        VectorEcef vSatEcef;

        VectorEci rSatEci;
        VectorEci vSatEci;

        Matrix3d eopPartialMatrixEci = Matrix3d::Zero();

        if (acsConfig.eci_pseudoobs)
        {
            if (obs.vel.isZero())
            {
                obs.vel = satPos.vSatEci0;
            }

            // Get framed vectors because obs is undefined
            rSatEci = obs.pos;
            vSatEci = obs.vel;

            // No EOP Partials needed for ECI pseudo obs
        }
        else
        {
            if (obs.vel.isZero())
            {
                obs.vel = satPos.satVel;
            }

            // Get framed vectors because obs is undefined
            rSatEcef = obs.pos;
            vSatEcef = obs.vel;

            rSatEci = frameSwapper(rSatEcef, &vSatEcef, &vSatEci);

            if (acsConfig.pppOpts.eop.estimate[0])
            {
                eopPartialMatrixEci = receiverEopPartials(satPos.rSatCom) * frameSwapper.i2t_mat;
            }
        }

        KFKey satPosKeys[3];
        KFKey satVelKeys[3];
        KFKey eopKeys[3];
        KFKey rateKeys[3];
        for (int i = 0; i < 3; i++)
        {
            satPosKeys[i].type = KF::ORBIT;
            satPosKeys[i].Sat  = obs.Sat;
            satPosKeys[i].num  = i;

            satVelKeys[i].type = KF::ORBIT;
            satVelKeys[i].Sat  = obs.Sat;
            satVelKeys[i].num  = i + 3;

            eopKeys[i].type    = KF::EOP;
            eopKeys[i].num     = i;
            eopKeys[i].comment = eopComments[i];

            rateKeys[i].type    = KF::EOP_RATE;
            rateKeys[i].num     = i;
            rateKeys[i].comment = (string)eopComments[i] + "/day";
        }

        VectorEci statePosEci;

        for (int i = 0; i < 3; i++)
        {
            InitialState posInit = initialStateFromConfig(satOpts.orbit, i);
            InitialState velInit = initialStateFromConfig(satOpts.orbit, i + 3);

            if (posInit.estimate == false)
            {
                continue;
            }

            if (posInit.x == 0)
                posInit.x = satPos.rSatEci0[i];
            if (velInit.x == 0)
                velInit.x = satPos.vSatEci0[i];

            statePosEci[i] = posInit.x;

            kfState.getKFValue(satPosKeys[i], statePosEci[i]);

            KFMeasEntry kfMeasEntry(&kfState);
            kfMeasEntry.addDsgnEntry(satPosKeys[i], 1, posInit);

            kfState.addKFState(satVelKeys[i], velInit);

            for (int num = 0; num < 3; num++)
            {
                InitialState init        = initialStateFromConfig(acsConfig.pppOpts.eop, num);
                InitialState eopRateInit = initialStateFromConfig(acsConfig.pppOpts.eop_rates, num);

                if (init.estimate == false)
                {
                    continue;
                }

                if (init.x == 0)
                    switch (num)
                    {
                        case 0:
                            init.x        = erpv.xp * R2MAS;
                            eopRateInit.x = +erpv.xpr * R2MAS;
                            break;
                        case 1:
                            init.x        = erpv.yp * R2MAS;
                            eopRateInit.x = +erpv.ypr * R2MAS;
                            break;
                        case 2:
                            init.x        = erpv.ut1Utc * S2MTS;
                            eopRateInit.x = -erpv.lod * S2MTS;
                            break;
                    }

                kfMeasEntry.addDsgnEntry(eopKeys[num], eopPartialMatrixEci(num, i), init);

                if (eopRateInit.estimate == false)
                {
                    continue;
                }

                kfState.setKFTransRate(eopKeys[num], rateKeys[num], 1 / S_IN_DAY, eopRateInit);
            }

            double omc = rSatEci[i] - statePosEci[i];

            kfMeasEntry.setInnov(omc);

            kfMeasEntry.obsKey.comment           = "ECI PseudoPos";
            kfMeasEntry.obsKey.type              = KF::ORBIT_MEAS;
            kfMeasEntry.obsKey.Sat               = obs.Sat;
            kfMeasEntry.obsKey.num               = i;
            kfMeasEntry.metaDataMap["pseudoObs"] = (void*)true;

            kfMeasEntry.addNoiseEntry(kfMeasEntry.obsKey, 1, SQR(satOpts.pseudo_sigma));

            kfMeasEntryList.push_back(kfMeasEntry);
        }

        addEmpStates(satOpts, kfState, obs.Sat);
#ifdef _ESTIMATE_CRCD
        if (initialStateFromConfig(satOpts.cr).estimate)
        {
            InitialState CrInit = initialStateFromConfig(satOpts.cr, 0);
            kfState.addKFState({KF::CR, obs.Sat, 0}, CrInit);
        }
        if (initialStateFromConfig(satOpts.cd).estimate)
        {
            InitialState CdInit = initialStateFromConfig(satOpts.cd, 0);
            kfState.addKFState({KF::CD, obs.Sat, 0}, CdInit);
        }
#endif
    }
}

void pseudoRecDcb(
    Trace&           trace,    ///< Trace to output to
    KFState&         kfState,  ///< Kalman filter object containing the network state parameters
    KFMeasEntryList& kfMeasEntryList  ///< List to append kf measurements to
)
{
    string doneRec;
    SatSys doneSys;

    static map<KFKey, SatSys> setSatMap;

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::CODE_BIAS || key.str.empty())
        {
            // only do receiver code biases
            continue;
        }

        SatSys sysSat(key.Sat.sys);

        // there are code biases for this receiver+system, check the dcbs all at once at the first
        // one
        if (key.str == doneRec && sysSat == doneSys)
        {
            continue;
        }

        auto sys = sysSat.sys;

        KFKey sysKey = key;
        sysKey.Sat   = SatSys(sys);
        sysKey.num   = 0;

        auto& setSat = setSatMap[sysKey];
        if (setSat.sys && setSat != key.Sat)
        {
            // have used a different key before, only use that one again.
            continue;
        }

        doneRec = key.str;
        doneSys = sysSat;

        auto& recSysOpts = acsConfig.getRecOpts(key.str, {sys._to_string()});

        if (recSysOpts.zero_dcb_codes.size() != 2)
        {
            continue;
        }

        auto& firstCode  = recSysOpts.zero_dcb_codes[0];
        auto& secondCode = recSysOpts.zero_dcb_codes[1];

        list<KFKey> codeBiasKeys;

        auto it = kfState.kfIndexMap.find(key);

        KFKey testKey;

        while (it != kfState.kfIndexMap.end() && (testKey = it->first, true) &&
               testKey.type == +KF::CODE_BIAS && testKey.str == key.str && testKey.Sat == key.Sat)
        {
            codeBiasKeys.push_back(testKey);
            ++it;
            if (it == kfState.kfIndexMap.end())
            {
                break;
            }
        }

        if (firstCode == +E_ObsCode::AUTO || secondCode == +E_ObsCode::AUTO)
        {
            // get all available codes in priority order to resolve the autos

            codeBiasKeys.sort(
                [sys](KFKey& a, KFKey& b)
                {
                    auto& code_priorities = acsConfig.code_priorities[sys];

                    auto iterA = std::find(
                        code_priorities.begin(),
                        code_priorities.end(),
                        E_ObsCode::_from_integral(a.num)
                    );
                    auto iterB = std::find(
                        code_priorities.begin(),
                        code_priorities.end(),
                        E_ObsCode::_from_integral(b.num)
                    );

                    if (iterA < iterB)
                        return true;
                    else
                        return false;
                }
            );

            for (auto& code_ptr : {&firstCode, &secondCode})
            {
                auto& code = *code_ptr;

                if (code != +E_ObsCode::AUTO)
                {
                    continue;
                }

                // need to replace this code with the first one found in the sorted code_priorities

                try
                {
                    for (auto& codeBiasKey : codeBiasKeys)
                    {
                        E_ObsCode keyCode = E_ObsCode::_from_integral(codeBiasKey.num);

                        if (code2Freq[sys][keyCode] == code2Freq[sys][firstCode])
                        {
                            // would duplicate frequency, skip. (also works for auto in firstCode)
                            continue;
                        }

                        code = keyCode;

                        BOOST_LOG_TRIVIAL(debug) << "Setting zero_dcb_code for " << key.str << " "
                                                 << sys._to_string() << " to " << code;

                        break;
                    }
                }
                catch (...)
                {
                    continue;
                }
            }
        }

        KFKey key1 = key;
        key1.num   = firstCode;
        KFKey key2 = key;
        key2.num   = secondCode;

        if (std::find(codeBiasKeys.begin(), codeBiasKeys.end(), key1) == codeBiasKeys.end() ||
            std::find(codeBiasKeys.begin(), codeBiasKeys.end(), key2) == codeBiasKeys.end())
        {
            // both biases not found
            continue;
        }

        KFMeasEntry measEntry(&kfState);
        measEntry.obsKey.type    = KF::CODE_BIAS;
        measEntry.obsKey.Sat     = sysSat;
        measEntry.obsKey.str     = key.str;
        measEntry.obsKey.comment = "Zero DCB";

        measEntry.metaDataMap["pseudoObs"] = (void*)true;
        measEntry.metaDataMap["explain"]   = (void*)true;

        InitialState init1;
        InitialState init2;

        kfState.getKFValue(key1, init1.x);
        kfState.getKFValue(key2, init2.x);

        double bias1 = init1.x;
        double bias2 = init2.x;

        measEntry.addDsgnEntry(key1, +1, init1);
        measEntry.addDsgnEntry(key2, -1, init2);

        measEntry.setInnov(bias2 - bias1);

        measEntry.addNoiseEntry(measEntry.obsKey, 1, PIVOT_MEAS_VARIANCE);

        setSat = key1.Sat;

        kfMeasEntryList.push_back(measEntry);
    }
}

void receiverPseudoObs(
    Trace&           trace,    ///< Trace to output to
    Receiver&        rec,      ///< (Pseudo) Receiver to perform calculations for
    KFState&         kfState,  ///< Kalman filter object containing the network state parameters
    KFMeasEntryList& kfMeasEntryList,  ///< List to append kf measurements to
    ReceiverMap&     receiverMap       ///< Map of stations to retrieve receiver metadata from
)
{
    GTime time = rec.obsList.front()->time;

    vector<int> indices;

    for (auto& obs : only<FObs>(rec.obsList))
    {
        for (auto& [key, index] : obs.obsState.kfIndexMap)
        {
            if (key.type != KF::REC_POS || key.num != 0)
            {
                continue;
            }

            if (key.rec_ptr == nullptr)
            {
                continue;
            }

            auto& rec = *key.rec_ptr;
            // try to get apriori from the existing state, otherwise use sinex.

            Vector3d apriori = Vector3d::Zero();

            bool found = true;
            for (int i = 0; i < 3; i++)
            {
                KFKey posKey = key;
                posKey.num   = i;

                found &= kfState.getKFValue(posKey, apriori(i));
            }

            // make sure this receiver is initialised since this might be the first time anyone has
            // seen it

            // 			if (found == false)
            {
                rec.id = key.str;
                getRecSnx(rec.id, obs.time, rec.snx);
                apriori = rec.snx.pos;
            }

            rec.minconApriori = apriori;
        }

        obs.obsState.time = time;

        mincon(trace, obs.obsState);

        for (auto& [key, index] : obs.obsState.kfIndexMap)
        {
            auto& recOpts = acsConfig.getRecOpts(key.str);

            if (key.type != KF::REC_POS)
            {
                continue;
            }

            KFKey kfKey = key;

            auto& rec     = receiverMap[key.str];
            kfKey.rec_ptr = &rec;

            InitialState posInit = initialStateFromConfig(recOpts.pos, kfKey.num);
            if (posInit.estimate == false)
            {
                continue;
            }

            double obsX = obs.obsState.x[index];

            KFMeasEntry kfMeasEntry(&kfState, kfKey);

            double stateX = obsX;
            kfState.getKFValue(kfKey, stateX);

            posInit.x = obsX;

            kfMeasEntry.addDsgnEntry(kfKey, 1, posInit);

            InitialState velInit = initialStateFromConfig(recOpts.strain_rate, kfKey.num);
            if (velInit.estimate)
            {
                KFKey velKey   = kfKey;
                velKey.type    = KF::STRAIN_RATE;
                velKey.comment = "mm/year";

                kfState.setKFTransRate(kfKey, velKey, 1 / (365.25 * 24 * 60 * 60 * 1e3), velInit);
            }

            double omc = obsX - stateX;

            kfMeasEntry.setInnov(omc);
            kfMeasEntry.setNoise(obs.obsState.P(index, index));

            indices.push_back(index);

            kfMeasEntryList.push_back(kfMeasEntry);
        }
    }
}

/** Add pseudo-observations to set one satellite's phase biases variances to zero.
 * This shouldnt occur in the parallel section because multiple receivers may be trying to set
 * different satellites to 0 if they see different satellites.
 */
void phasePseudoObs(Trace& trace, KFState& kfState, KFMeasEntryList& kfMeasEntryList)
{
    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::PHASE_BIAS)
        {
            continue;
        }

        string& constrain_phase_bias = acsConfig.constrain_phase_bias[key.Sat.sys];

        if (constrain_phase_bias.empty())
        {
            continue;
        }

        if (constrain_phase_bias == "<AUTO>")
        {
            constrain_phase_bias = key.Sat.id();
        }

        if (key.Sat.id() != constrain_phase_bias)
        {
            continue;
        }

        double dummy;
        double var;
        kfState.getKFValue(key, dummy, &var);

        if (var < 2 * PIVOT_MEAS_VARIANCE)
        {
            continue;
        }

        KFMeasEntry kfMeasEntry(&kfState, key);

        kfMeasEntry.obsKey.comment = "Phase bias constraint";

        kfMeasEntry.addDsgnEntry(key, +1);
        kfMeasEntry.addNoiseEntry(key, +1, PIVOT_MEAS_VARIANCE);

        kfMeasEntryList.push_back(kfMeasEntry);
    }
}

void ambgPseudoObs(Trace& trace, KFState& kfState, KFMeasEntryList& kfMeasEntryList)
{
    static map<E_Sys, map<int, tuple<KFKey, double, bool>>> bestForSysCode;

    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::AMBIGUITY)
        {
            continue;
        }

        if (acsConfig.constrain_best_ambiguity_integer[key.Sat.sys] == false)
        {
            continue;
        }

        double dummy;
        double var = 0;

        kfState.getKFValue(key, dummy, &var);

        auto& [bestKey, bestVar, done] = bestForSysCode[key.Sat.sys][key.num];

        if (done)
        {
            continue;
        }

        if (bestVar == 0 || var < bestVar)
        {
            // new best variance for this frequency
            bestForSysCode[key.Sat.sys][key.num] = {key, var, false};
        }
    }

    for (auto& [sys, codeMap] : bestForSysCode)
        for (auto& [code, best] : codeMap)
        {
            auto& [key, var, done] = best;

            if (done)
            {
                continue;
            }

            if (var < 2 * PIVOT_MEAS_VARIANCE)
            {
                continue;
            }

            // this is the most constrained ambiguity of an unconstrained system, constrain it to an
            // integer value

            KFMeasEntry measEntry(&kfState);
            measEntry.obsKey         = key;
            measEntry.obsKey.comment = "Integer ambiguity constraint";

            measEntry.addDsgnEntry(key, +1);

            double floatAmb;
            kfState.getKFValue(key, floatAmb);

            double fixedAmb = round(floatAmb);

            measEntry.setInnov(fixedAmb - floatAmb);

            measEntry.metaDataMap["pseudoObs"] = (void*)true;
            //		measEntry.metaDataMap["explain"]	= (void*) true;

            measEntry.addNoiseEntry(measEntry.obsKey, 1, PIVOT_MEAS_VARIANCE);

            kfMeasEntryList.push_back(measEntry);

            done = true;
        }
}

void ionoPseudoObs(  // todo aaron, move to model section
    Trace&           pppTrace,
    ReceiverMap&     receiverMap,
    KFState&         kfState,
    KFMeasEntryList& kfMeasEntryList
)
{
    for (auto& [id, rec] : receiverMap)
        for (auto& obs : only<GObs>(rec.obsList))
        {
            if (acsConfig.use_iono_corrections[obs.Sat.sys] == false)
                continue;

            if (obs.satStat_ptr == nullptr)
            {
                continue;
            }

            auto& satStat = *obs.satStat_ptr;

            double extvar = 0;
            double extion = getSSRIono(
                pppTrace,
                obs.time,
                rec.aprioriPos,
                satStat,
                extvar,
                obs.Sat
            );  // todo aaron get from other sources too

            if (extvar <= 0)
                continue;

            auto& recOpts = acsConfig.getRecOpts(rec.id);

            InitialState init = initialStateFromConfig(recOpts.ion_stec);

            KFKey kfKey;
            kfKey.type = KF::IONO_STEC;
            kfKey.str  = rec.id;
            kfKey.Sat  = obs.Sat;

            kfState.getKFValue(kfKey, init.x);

            double kfion = init.x;

            tracepdeex(
                2,
                pppTrace,
                "    Checking Ionosphere pseudos: %s %s, %.4f, %.4f, %.2e\n",
                rec.id.c_str(),
                obs.Sat.id().c_str(),
                extion,
                kfion,
                extvar
            );

            KFMeasEntry measEntry(&kfState);
            measEntry.obsKey.type = KF::IONOSPHERIC;
            measEntry.obsKey.str  = rec.id;
            measEntry.obsKey.Sat  = obs.Sat;

            measEntry.addDsgnEntry(kfKey, +1, init);

            measEntry.setInnov(extion - kfion);

            measEntry.metaDataMap["pseudoObs"] = (void*)true;
            //		measEntry.metaDataMap["explain"]	= (void*) true;

            measEntry.addNoiseEntry(measEntry.obsKey, 1, extvar);

            kfMeasEntryList.push_back(measEntry);
        }
}

void tropPseudoObs(
    Trace&           trace,
    ReceiverMap&     receiverMap,
    KFState&         kfState,
    KFMeasEntryList& kfMeasEntryList
)
{
    if (acsConfig.use_trop_corrections == false)
    {
        return;
    }

    for (auto& [id, rec] : receiverMap)
    {
        auto& recOpts = acsConfig.getRecOpts(id);

        if (recOpts.exclude)
        {
            continue;
        }

        double dryZTD;
        double wetZTD;
        double dryMap;
        double wetMap;
        double extVar;
        double extZTD = tropCSSR(
            trace,
            kfState.time,
            rec.pos,
            PI / 2,
            dryZTD,
            dryMap,
            wetZTD,
            wetMap,
            extVar
        );  // todo aaron, take this from other places optionally

        if (extVar <= 0)
            continue;

        KFKey kfKey;
        kfKey.type = KF::TROP;
        kfKey.str  = rec.id;

        InitialState init = initialStateFromConfig(recOpts.trop);

        kfState.getKFValue(kfKey, init.x);

        double kftrop = init.x;

        tracepdeex(
            2,
            trace,
            "    Checking troposphere pseudos: %s, %.4f + %.4f = %.4f, %.4f, %.2e\n",
            rec.id.c_str(),
            dryZTD,
            wetZTD,
            extZTD,
            kftrop,
            extVar
        );

        KFMeasEntry measEntry(&kfState);
        measEntry.obsKey.type = KF::TROP;
        measEntry.obsKey.str  = rec.id;

        measEntry.addDsgnEntry(kfKey, +1, init);

        measEntry.setInnov(extZTD - kftrop);

        measEntry.metaDataMap["pseudoObs"] = (void*)true;

        measEntry.addNoiseEntry(measEntry.obsKey, 1, extVar);

        kfMeasEntryList.push_back(measEntry);
    }
}

void satClockPivotPseudoObs(Trace& trace, KFState& kfState, KFMeasEntryList& kfMeasEntryList)
{
    for (auto& [key, index] : kfState.kfIndexMap)
    {
        if (key.type != KF::SAT_CLOCK)
        {
            continue;
        }

        string& constrain_clock = acsConfig.constrain_clock[key.Sat.sys];

        if (constrain_clock.empty())
        {
            continue;
        }

        if (constrain_clock != "<AUTO>" && constrain_clock != key.Sat.id())
        {
            continue;
        }

        KFMeasEntry measEntry(&kfState);
        measEntry.obsKey.type    = KF::SAT_CLOCK;
        measEntry.obsKey.Sat     = key.Sat;
        measEntry.obsKey.comment = "Satellite pivot constraint";

        measEntry.addDsgnEntry(key, +1);

        measEntry.setInnov(0);

        measEntry.metaDataMap["pseudoObs"] = (void*)true;

        measEntry.addNoiseEntry(measEntry.obsKey, 1, 1e-6);

        kfMeasEntryList.push_back(measEntry);
    }
}
