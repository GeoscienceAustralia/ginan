#include "GNSSambres.hpp"

KFState netwWL12KF;
KFState netwWL23KF;
int wltrclvl = 4;

map<string,KFState> userWL12KF;
map<string,KFState> userWL23KF;

/** Remove ambiguity states from filter when they are not measured for an epoch.
 * This effectively reinitialises them on the following epoch as a new state, and can be used for simple
 * resolution of cycle-slips
 */
void removeUnmeasuredAmbiguities(
	Trace&				trace,
	KFState&			kfState, 			///< Filter to remove states from
	map<KFKey, bool>	measuredStates)		///< Map of measured states in this epoch to compare against.
{
	for (auto it = kfState.stateTransitionMap.cbegin(); it != kfState.stateTransitionMap.cend();  )
	{
		KFKey key = it->first;

		if	( (key.type == KF::AMBIGUITY)
			&&(measuredStates[key] == false))
		{
			trace << std::endl << "Removing " << key.str << " " << key.Sat.id();
			kfState.procNoiseMap.		erase(key);
			kfState.initNoiseMap.		erase(key);
			kfState.stateClampMaxMap.	erase(key);
			kfState.stateClampMinMap.	erase(key);
			kfState.rateTransitionMap.	erase(key);
			kfState.gaussMarkovTauMap.	erase(key);

			it = kfState.stateTransitionMap.erase(it);
		}
		else
		{
			++it;
		}
	}
}

/* reset station or satellite ambiguities in KF */
void ResetDisconnectedStates( Trace& trace, KFState& kfState)
{
	for (auto it = kfState.stateTransitionMap.cbegin(); it != kfState.stateTransitionMap.cend();  )
	{
		KFKey key = it->first;

		if	((key.type == KF::AMBIGUITY) &&
				(StatAmbMap_list[key.Sat.sys][key.str].reset  ||
				satpiv[key.Sat].reset      ||
				(key.num == E_AmbTyp::WL12 && StatAmbMap_list[key.Sat.sys][key.str].SignList[key.Sat].flt.WL12var < 0) ||
				(key.num == E_AmbTyp::WL23 && StatAmbMap_list[key.Sat.sys][key.str].SignList[key.Sat].flt.WL23var < 0)))
		{
			tracepdeex(wltrclvl, trace, "\n#WLR Resetting ambiguity between %s - %s", key.str.c_str(), key.Sat.id().c_str());
			kfState.procNoiseMap.		erase(key);
			kfState.initNoiseMap.		erase(key);
			kfState.stateClampMaxMap.	erase(key);
			kfState.stateClampMinMap.	erase(key);
			kfState.rateTransitionMap.	erase(key);

			it = kfState.stateTransitionMap.erase(it);
		}
		else if ((key.type == KF::PHASE_BIAS) && satpiv[key.Sat].reset)
		{
			tracepdeex(wltrclvl + 1, trace, "\n#WLR Resetting satellite bias for %s", key.Sat.id().c_str());
			kfState.procNoiseMap.		erase(key);
			kfState.initNoiseMap.		erase(key);
			kfState.stateClampMaxMap.	erase(key);
			kfState.stateClampMinMap.	erase(key);
			kfState.rateTransitionMap.	erase(key);

			it = kfState.stateTransitionMap.erase(it);
		}
		else if ((key.type == KF::REC_SYS_BIAS) && StatAmbMap_list[key.Sat.sys][key.str].reset)  			/* make sure that key.Sat.sys has the right value */
		{
			tracepdeex(wltrclvl + 1, trace, "\n#WLR Resetting station bias for %s", key.str.c_str());
			kfState.procNoiseMap.		erase(key);
			kfState.initNoiseMap.		erase(key);
			kfState.stateClampMaxMap.	erase(key);
			kfState.stateClampMinMap.	erase(key);
			kfState.rateTransitionMap.	erase(key);

			it = kfState.stateTransitionMap.erase(it);
		}
		else
		{
			++it;
		}
	}
}

/* solve and apply ambiguities */
void WLambRes(Trace& trace, KFState& KFstate, ARState& ambState, bool wlonly)
{
	KFstate.initFilterEpoch();
	list<KFKey>    kfKeyList;

	ambState.ambmap.clear();
	int ind = 0;

	for (auto& [kfKey, index] : KFstate.kfIndexMap)
	{
		if (kfKey.type == KF::AMBIGUITY)
		{
			if ( StatAmbMap_list[kfKey.Sat.sys][kfKey.str].SignList.find(kfKey.Sat) == StatAmbMap_list[kfKey.Sat.sys][kfKey.str].SignList.end()) continue;

			if ( StatAmbMap_list[kfKey.Sat.sys][kfKey.str].SignList[kfKey.Sat].elev < ambState.arelev ||
					(StatAmbMap_list[kfKey.Sat.sys][kfKey.str].SignList[kfKey.Sat].nepc < 4           && !wlonly) )
			{
				StatAmbMap_list[kfKey.Sat.sys][kfKey.str].SignList[kfKey.Sat].state = 0;
				continue;
			}

			ambState.ambmap[ind++] = kfKey;
			kfKeyList.push_back(kfKey);
		}
	}

	int namb = ind;

	ambState.aflt = KFstate.getSubState(kfKeyList, &ambState.Paflt);
	tracepdeex(wltrclvl, trace, "\n#WLR Solving WL ambiguities ...");
	int nfix = GNSS_AR(trace, &ambState);
	VectorXd fixX = ambState.zfix;

	InitialState ini0;
	ini0.x = 0;
	ini0.P = 0.0;
	ini0.Q = 0.0;
	KFMeasEntryList	MeasList;

	tracepdeex(wltrclvl, trace, "\n#WLR %4d ambiguities fixed, applying ...", nfix);

	for (int i = 0; i < nfix; i++)
	{
		KFKey kfKey = ambState.ambmap[i];		/* Need to review obsKey definition, this observation is not (necessary) associated with a single sat or station */
		ObsKey obsKey = { kfKey.Sat, "", "WLint", i	};
		KFMeasEntry	ARMeas(&KFstate, obsKey);
		ARMeas.setValue(fixX(i));
		ARMeas.setNoise(0.1 * POSTAR_VAR);

		for (int j = 0; j < namb; j++) if (ambState.Ztrs(i, j) != 0)
			{
				ARMeas.addDsgnEntry( ambState.ambmap[j], ambState.Ztrs(i, j), ini0);
			}

		MeasList.push_back(ARMeas);
	}

	KFMeas combined = KFstate.combineKFMeasList(MeasList);
	KFstate.filterKalman(trace, combined, false);

	for (auto& [kfKey, index] : KFstate.kfIndexMap)
	{
		double val, var;
		KFstate.getKFValue(kfKey, val, &var);

		if (kfKey.type == KF::AMBIGUITY)
		{
			SignAmbg& sigamb = StatAmbMap_list[kfKey.Sat.sys][kfKey.str].SignList[kfKey.Sat];

			if ( var < POSTAR_VAR)
			{
				if (kfKey.num == E_AmbTyp::WL12)
				{
					sigamb.fix.WL12 = ROUND(val);
					sigamb.fix.WL12var = 0.0;
					sigamb.state |= 2;
				}

				if (kfKey.num == E_AmbTyp::WL23)
				{
					sigamb.fix.WL23 = ROUND(val);
					sigamb.fix.WL23var = 0.0;
					sigamb.state |= 1;
				}
			}
			else
			{
				if (kfKey.num == E_AmbTyp::WL12)
				{
					sigamb.fix.WL12var = -1;
					sigamb.state &= 1;
				}

				if (kfKey.num == E_AmbTyp::WL23)
				{
					sigamb.fix.WL23var = -1;
					sigamb.state &= 6;
				}
			}
		}

		if (kfKey.type == KF::REC_SYS_BIAS)
		{
			if (kfKey.num == E_AmbTyp::WL12)
			{
				StatAmbMap_list[kfKey.Sat.sys][kfKey.str].stabias_fix.WL12 = val;
				StatAmbMap_list[kfKey.Sat.sys][kfKey.str].stabias_fix.WL12var = var;
			}

			if (kfKey.num == E_AmbTyp::WL23)
			{
				StatAmbMap_list[kfKey.Sat.sys][kfKey.str].stabias_fix.WL23 = val;
				StatAmbMap_list[kfKey.Sat.sys][kfKey.str].stabias_fix.WL23var = var;
			}
		}

		if (kfKey.type == KF::PHASE_BIAS)
		{
			if (kfKey.num == E_AmbTyp::WL12)
			{
				satpiv[kfKey.Sat].satbias_fix.WL12 = val;
				satpiv[kfKey.Sat].satbias_fix.WL12var = var;
			}

			if (kfKey.num == E_AmbTyp::WL23)
			{
				satpiv[kfKey.Sat].satbias_fix.WL23 = val;
				satpiv[kfKey.Sat].satbias_fix.WL23var = var;
			}
		}
	}
}

int WLambEstm( Trace& trace, GTime time, ARState& ambState, bool wlonly)
{
	tracepdeex(wltrclvl, trace, "\n#WLR Estimating WL ambiguities ... for %s", ambState.recv);
	//double dtime = stations.front()->obsList.front().time.time;
	
	KFState* kfptr12;
	KFState* kfptr23;
	if(ambState.endu)
	{
		kfptr12 = &userWL12KF[ambState.recv];
		kfptr23 = &userWL23KF[ambState.recv];
	}
	else
	{
		kfptr12 = &netwWL12KF;
		kfptr23 = &netwWL23KF;
	}
	auto& WL12ambKF = *kfptr12;
	auto& WL23ambKF = *kfptr23;
	
	int nmeas12 = 0, nmeas23 = 0;

	WL12ambKF.initFilterEpoch();
	KFMeasEntryList		kfMeasEntryList12;
	map<KFKey, bool>	activeStatesWL12;
	ResetDisconnectedStates(trace, WL12ambKF);

	WL23ambKF.initFilterEpoch();
	KFMeasEntryList		kfMeasEntryList23;
	map<KFKey, bool>	activeStatesWL23;
	ResetDisconnectedStates(trace, WL23ambKF);

	map<SatSys, int> sta_num;
	map<string, int> sat_num;

	for (auto& [sys, act] : sys_solve) 
	for (auto& [rec, staamb] : StatAmbMap_list[sys])
	for (auto& [sat, sigdat] : staamb.SignList)
	{
		if (act == false)
		{
			continue;
		}
		
		if( ambState.endu && rec!=ambState.recv ) continue;
		if (sigdat.outage) 					continue;
		if (sigdat.elev < ambState.prcele) 	continue;
		if (sigdat.nfreq < 2) 				continue;
		if (sigdat.raw.WL12var <= 0) 		continue;

		double measWL12 = sigdat.raw.WL12;

		if (sigdat.pivot) measWL12 -= sigdat.fix.WL12;

		double variWL12 = sigdat.raw.WL12var;

		sat_num[rec]++;

		tracepdeex(wltrclvl, trace, "\n#WLAR_MEA %s %s %s %10.4f %.4e", ambState.recv, rec, sat.id(), measWL12, variWL12);

		ObsKey obsKeyMW12 = { sat, rec, "MW12"	};
		KFMeasEntry	mw12Meas(&WL12ambKF, obsKeyMW12);
		mw12Meas.setValue(measWL12);
		mw12Meas.setNoise(variWL12);

		KFKey kfKey;

		InitialState init;
		init.x = 0;
		init.P = 0;

		/* WL12 satellite biases */
		if (!ambState.endu)
		{
			init.Q = ambState.staprn;
			kfKey  = {KF::PHASE_BIAS, sat, "", E_AmbTyp::WL12};
			mw12Meas.addDsgnEntry(kfKey, 1, init);

			if (sta_num.find(sat) == sta_num.end()) sta_num[sat] = 1;
			else sta_num[sat]++;
		}

		SatSys sat0 = {};
		sat0.sys = sys;
		sat0.prn = 0;

		/* WL12 station biases */
		if (ambState.endu || rec != ARrefsta)
		{
			init.Q = ambState.satprn;
			kfKey  = {KF::REC_SYS_BIAS, sat0, rec, E_AmbTyp::WL12};
			mw12Meas.addDsgnEntry(kfKey, 1, init);
		}

		/* WL12 ambiguities */
		if (!sigdat.pivot)
		{
			init.Q = 0;
			KFKey kfKey	= {KF::AMBIGUITY, sat, rec, E_AmbTyp::WL12};
			mw12Meas.addDsgnEntry( kfKey, 1, init);
			/*if(sigdat.nepc>60 && sigdat.state<2) activeStatesWL12[kfKey]=false;
			else */activeStatesWL12[kfKey] = true;
		}

		kfMeasEntryList12.push_back(mw12Meas);
		nmeas12++;

		tracepdeex(wltrclvl, trace, "\n#WLAR_MEA %d %s %s %10.4f %.4e", nmeas12, rec, sat.id().c_str(), measWL12, variWL12);

		if (sigdat.nfreq < 3) 
			continue;

		double measWL23 = sigdat.raw.WL23;

		if (sigdat.pivot)
			measWL23 -= sigdat.fix.WL23;

		double variWL23 = sigdat.raw.WL23var;

		ObsKey obsKeyMW23 = {sat, rec, "MW23"};
		KFMeasEntry	mw23Meas(&WL23ambKF, obsKeyMW23);
		mw23Meas.setValue(measWL23);
		mw23Meas.setNoise(variWL23);

		/* WL23 satellite biases */
		if (!ambState.endu)
		{
			init.Q = ambState.staprn;
			kfKey  = {KF::PHASE_BIAS, sat, "", E_AmbTyp::WL23};
			mw23Meas.addDsgnEntry(kfKey, 1, init);
		}

		/* WL23 station biases */
		if (rec != ARrefsta)
		{
			init.Q = ambState.satprn;
			kfKey  = {KF::REC_SYS_BIAS, sat0, rec, E_AmbTyp::WL23};
			mw23Meas.addDsgnEntry(kfKey, 1, init);
		}

		/* WL23 ambiguities */
		if (!sigdat.pivot)
		{
			init.Q = 0;
			KFKey kfKey	= {KF::AMBIGUITY, sat, rec, E_AmbTyp::WL23};
			mw23Meas.addDsgnEntry( kfKey, 1, init);
			activeStatesWL23[kfKey] = true;
		}

		kfMeasEntryList23.push_back(mw23Meas);
		nmeas23++;
	}

	int min_nmea = sat_num.size() + sta_num.size() + 1;

	tracepdeex(wltrclvl, trace, "\n#WLAR_FIL %d %d %d (%d %d)", nmeas12, nmeas23, min_nmea, sat_num.size(), sta_num.size());

	if (nmeas12 < min_nmea)
		return 0;

	removeUnmeasuredAmbiguities(trace, WL12ambKF, activeStatesWL12);
	WL12ambKF.stateTransition(trace, time);
	KFMeas combined12 = WL12ambKF.combineKFMeasList(kfMeasEntryList12);
	combined12.time = time;

	tracepdeex(wltrclvl, trace, "\n#WLR Y12: %4d x %4d", combined12.Y.rows(), combined12.Y.cols());
	tracepdeex(wltrclvl, trace, "\n#WLR A12: %4d x %4d", combined12.A.rows(), combined12.A.cols());

	if (WL12ambKF.lsqRequired)
	{
		WL12ambKF.lsqRequired = false;
		WL12ambKF.leastSquareInitStates(trace, combined12);
	}

	WL12ambKF.filterKalman(trace, combined12, false);

	for (auto& [kfKey, index] : WL12ambKF.kfIndexMap)
	{
		double val, var;
		WL12ambKF.getKFValue(kfKey, val, &var);

		if (kfKey.type == KF::AMBIGUITY)
		{
			SignAmbg& sigamb = StatAmbMap_list[kfKey.Sat.sys][kfKey.str].SignList[kfKey.Sat];
			sigamb.flt.WL12 = val;
			sigamb.flt.WL12var = var;
		}

		if (kfKey.type == KF::REC_SYS_BIAS)
		{
			StatAmbMap_list[kfKey.Sat.sys][kfKey.str].stabias.WL12 = val;
			StatAmbMap_list[kfKey.Sat.sys][kfKey.str].stabias.WL12var = var;
		}

		if (kfKey.type == KF::PHASE_BIAS)
		{
			satpiv[kfKey.Sat].satbias.WL12 = val;
			satpiv[kfKey.Sat].satbias.WL12var = var;
		}
	}


	KFState KFcopy12 = WL12ambKF;
	WLambRes(trace, KFcopy12, ambState, wlonly);

	if (wltrclvl == 2)
	{
		WL12ambKF.outputStates(trace);
		KFcopy12.outputStates(trace);
	}

	/*if(nmeas12<MIN_WL_MEAS)*/ return nmeas12;			/*not supporting triple frequency just yet... */

	removeUnmeasuredAmbiguities(trace, WL23ambKF, activeStatesWL23);
	WL23ambKF.stateTransition(trace, time);
	KFMeas combined23 = WL23ambKF.combineKFMeasList(kfMeasEntryList23);
	combined23.time = time;

	tracepdeex(wltrclvl, trace, "\n#WLR Y23: %4d x %4d", combined23.Y.rows(), combined23.Y.cols());
	tracepdeex(wltrclvl, trace, "\n#WLR A23: %4d x %4d", combined23.A.rows(), combined23.A.cols());

	if (WL23ambKF.lsqRequired)
	{
		WL23ambKF.lsqRequired = false;
		WL23ambKF.leastSquareInitStates(trace, combined23);
	}

	WL23ambKF.filterKalman(trace, combined23, false);

	for (auto& [kfKey, index] : WL23ambKF.kfIndexMap)
	{
		double val, var;
		WL23ambKF.getKFValue(kfKey, val, &var);

		if (kfKey.type == KF::AMBIGUITY)
		{
			StatAmbMap_list[kfKey.Sat.sys][kfKey.str].SignList[kfKey.Sat].flt.WL23    = val;
			StatAmbMap_list[kfKey.Sat.sys][kfKey.str].SignList[kfKey.Sat].flt.WL23var = var;
		}

		if (kfKey.type == KF::REC_SYS_BIAS)
		{
			StatAmbMap_list[kfKey.Sat.sys][kfKey.str].stabias.WL23   = val;
			StatAmbMap_list[kfKey.Sat.sys][kfKey.str].stabias.WL23var = var;
		}

		if (kfKey.type == KF::PHASE_BIAS)
		{
			satpiv[kfKey.Sat].satbias.WL23    = val;
			satpiv[kfKey.Sat].satbias.WL23var = var;
		}
	}

	KFState KFcopy23 = WL23ambKF;
	WLambRes(trace, KFcopy23, ambState, wlonly);
	return 0;
}
