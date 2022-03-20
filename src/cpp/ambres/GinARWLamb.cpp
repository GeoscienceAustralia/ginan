#include "GNSSambres.hpp"

map<KFKey,map<int,GinAR_amb>> WL_archive;
map<KFKey, int> WL_arch_ind;
map<string,map<E_AmbTyp,map<E_Sys,KFState>>> WL_ambfilt;

/** Remove ambiguity states from filter when they are not measured for an epoch.
 ** This effectively reinitialises them on the following epoch as a new state, and can be used for simple
 ** resolution of cycle-slips */
void removeUnmeasuredAmbiguities(
	Trace&				trace,				///< Debug trace
	KFState&			kfState, 			///< Filter to remove states from
	map<KFKey, bool>	measuredStates)		///< Map of measured states in this epoch to compare against.
{
	for (auto [key, index] : kfState.kfIndexMap)
	{
		if ( key.type == KF::AMBIGUITY 
		 && measuredStates[key] == false )
			{
			tracepdeex(3, trace, "\n Removing %s %s from Kalman filter", key.str.c_str(), key.Sat.id().c_str());
			kfState.removeState(key);
		}
	}
} 

/** save Resolved WL ambiguities */
void WL_newsect(
	Trace& trace,		///< Trace file to output to
	E_AmbTyp typ,		///< Ambiguity type e.g. WL12, WL23
	SatSys sat,			///< Satellite
	string rec,			///< Receiver name
	GTime time)			///< Solution time
{
	KFKey key = {KF::AMBIGUITY,sat,rec,typ};
	
	if (!WL_archive[key].empty())
	for (auto it = WL_archive[key].begin(); it != WL_archive[key].end(); )
	{
		bool remv = false;
		GinAR_amb amb = it->second;
		if (amb.hld_epc < 0)					remv = true;
		if (amb.sec_ini == GTime::noTime())		remv = true;
		if (amb.fix_fin == GTime::noTime())		remv = true;
		if (amb.mea_fin == GTime::noTime())		remv = true;
		
		if ((time-amb.mea_fin)>MAX_ARCH)		remv = true; 
		
		if (remv)	it=WL_archive[key].erase(it);
		else		it++;
	}
	
	int ind;
	if (WL_archive[key].empty())	ind = 1;
	else                			ind = WL_arch_ind[key]+1;
	
	GinAR_amb amb;
	amb.hld_epc = -1;
	amb.sec_ini = time;
	amb.mea_fin = time;
	amb.int_amb = INVALID_WLVAL;
	
	tracepdeex(ARTRCLVL+1, trace, "\n#ARES_WLAR New WL archive entry: %s %s %s %4d", rec.c_str(), sat.id().c_str(), time.to_string(0).c_str(), ind );
	
	WL_archive[key][ind] = amb;
	WL_arch_ind[key]     = ind;
	
	return;
}

/** Remove ambiguities and biases for systems, end user */
void reset_WLfilt(
	Trace& trace,		///< Trace file to output to
	E_AmbTyp typ,		///< Ambiguity type e.g. WL12, WL23
	GTime time,			///< Solution time
	string rec,			///< Receiver name
	E_Sys sys)			///< GNSS system e.g. GPS, GLO, GAL ...
{
	if (WL_ambfilt[rec][typ].find(sys) != WL_ambfilt[rec][typ].end()) 
		WL_ambfilt[rec][typ].erase(sys);
	
	for (auto & [key,WLmap] : WL_archive)
	{
		if (key.str     != rec) 												continue;
		if (key.num     != typ) 												continue;
		if (key.Sat.sys != sys) 												continue;
		
		WL_newsect(trace,typ,key.Sat,rec,time);
	}
	return;
}

/** Update WL filter */ 
void  updat_WLfilt(
	Trace& trace,		///< Trace file to output to
	GTime time,			///< Solution time
	GinAR_opt opt,		///< Ambiguity resolution options
	E_Sys sys,			///< GNSS system e.g. GPS, GLO, GAL ...
	E_AmbTyp typ)		///< Ambiguity type e.g. WL12, WL23
{
	string rec = opt.recv;
	KFState& WLambKF = WL_ambfilt[rec][typ][sys];
	WLambKF.max_filter_iter = opt.wlmxit;
	WLambKF.max_prefit_remv = opt.wlmxrm;
	
	WLambKF.initFilterEpoch();
	auto& pivlist = RECpivlist[typ][sys];
	map<KFKey,GinAR_amb> amblst;
	
	string recid = "NETWORK";
	if (opt.endu) recid = opt.recv;
	auto& AR_mealist = ARstations[recid].AR_meaMap; 
	
	for (auto& [key,amb] : AR_mealist)
	{
		if (key.num != typ) 													continue;
		if (key.Sat.sys != sys) 												continue;
		if (opt.endu && (key.str != rec))										continue;
		
		if (amb.out_epc > 0)													continue;
		
		if ( RECpivlist[typ][sys].find(key.str) == RECpivlist[typ][sys].end() )	continue;
		
		if ( !opt.endu 
		   && SATpivlist[typ][sys].find(key.Sat) == SATpivlist[typ][sys].end())	continue;
		
		amblst[key] = amb;
		
		if ( amblst[key].cyl_slp ) 
			WL_newsect(trace,typ,key.Sat,key.str,time);
		
		if (WL_archive.find(key) != WL_archive.end())
		{
			int WLind = WL_arch_ind[key];
			auto lastmeas = WL_archive[key][WLind].mea_fin;
			
			if ((time-lastmeas) > opt.Max_Hold_tim) WL_newsect(trace,typ,key.Sat,key.str,time);
			
			WL_archive[key][WLind].mea_fin = time;
			
			if (WL_archive[key][WLind].sec_ini == GTime::noTime())
				WL_archive[key][WLind].sec_ini = time;
		}
	}
	
	tracepdeex(ARTRCLVL, trace, "\n#ARES_WLAR Updating WL filter %s %s %s, nmea = %4d", time.to_string(0), rec, sys._to_string(), amblst.size());
	
	/* Remove ambiguities for Unmeasured States */
	for (auto [key, index] : WLambKF.kfIndexMap) 
	{
		if  ( key.type != KF::AMBIGUITY )
			continue;
		
		if ( amblst.find(key) == amblst.end() 
		  || amblst[key].cyl_slp )
		{
			tracepdeex(ARTRCLVL+1, trace, "\n#ARES_WLAR Removing %s %s from WL filter", key.str.c_str(), key.Sat.id().c_str());
			WLambKF.removeState(key);
		}
	}
	
	KFMeasEntryList	WLmeasList;
	int nmea = 0;
	SatSys sat0 = {};
	map<SatSys,int> satlst;
	map<string,int> reclst;
	InitialState init;
	
	/* Main measurements */
	for (auto& [mkey,amb] : amblst)
	{
		ObsKey obsKey = { mkey.Sat, mkey.str, "WLmea" };
		KFMeasEntry	WLmeas(&WLambKF, obsKey);
		
		WLmeas.setValue(amb.raw_amb);
		WLmeas.setNoise(amb.raw_var);
		tracepdeex(ARTRCLVL+1, trace, "\n WLmea %s %s %10.4f %13.4e", mkey.Sat.id().c_str(), mkey.str.c_str(), amb.raw_amb, amb.raw_var);
		KFKey kfKey;

		/* receiver bias */
		init.Q = opt.wlrecp;
		kfKey  = {KF::REC_SYS_BIAS, sat0, mkey.str, typ};
		WLmeas.addDsgnEntry(kfKey, 1, init);
		if (reclst.find(mkey.str) == reclst.end() ) reclst[mkey.str] = 1;
		else										reclst[mkey.str]+= 1;
		
		/* satellite bias */
		if (!opt.endu)
		{
			init.Q = opt.wlsatp;
			kfKey  = {KF::PHASE_BIAS, mkey.Sat, "", typ};
			WLmeas.addDsgnEntry(kfKey, 1, init);
			
			if (satlst.find(mkey.Sat) == satlst.end() )	satlst[mkey.Sat] = 1;
			else										satlst[mkey.Sat]+= 1;
		}
		
		/* Ambiguity */
		init.Q = 0;
		kfKey	= {KF::AMBIGUITY, mkey.Sat, mkey.str, typ};
		WLmeas.addDsgnEntry( kfKey, 1, init);
		
		WLmeasList.push_back(WLmeas);
		nmea++;
	}	
	
	int nsta = amblst.size() + satlst.size() + reclst.size();	/* number of states */
	
	/* pivot constraints */
	if (opt.endu)
	{
		string rec = opt.recv;
		SatSys sat = RECpivlist[typ][sys][rec].pre_sat;
		int    amb = RECpivlist[typ][sys][rec].sat_amb[sat];
		ObsKey obsKey = { sat, rec, "WLpiv" };
		KFMeasEntry	WLmeas(&WLambKF, obsKey);
		WLmeas.setValue(amb);
		WLmeas.setNoise(FIXED_AMB_VAR);
		KFKey kfKey	= {KF::AMBIGUITY, sat, rec, typ};
		init.Q = 0;
		WLmeas.addDsgnEntry( kfKey, 1, init);
		WLmeasList.push_back(WLmeas);
		nmea++;
	}
	else
	{
		for (auto& [rec,nsat] : reclst)
		{
			if ( rec == AR_reflist[sys] )
			{
				ObsKey obsKey = { sat0, rec, "WLpiv" };
				KFMeasEntry	WLmeas(&WLambKF, obsKey);
				WLmeas.setValue(0);
				WLmeas.setNoise(FIXED_AMB_VAR);
				
				tracepdeex(ARTRCLVL+1, trace, "\n WLpiv    %s %10.4f %13.4e", rec,  0, FIXED_AMB_VAR);
		
				init.Q = 0;
				KFKey kfKey	= {KF::REC_SYS_BIAS, sat0, rec, typ};
				WLmeas.addDsgnEntry( kfKey, 1, init);
				WLmeasList.push_back(WLmeas);
				nmea++;
			}
			else
			{
				SatSys sat = RECpivlist[typ][sys][rec].pre_sat;
				double amb = RECpivlist[typ][sys][rec].sat_amb[sat];
				ObsKey obsKey = { sat, rec, "WLpiv" };
				KFMeasEntry	WLmeas(&WLambKF, obsKey);
				WLmeas.setValue(amb);
				WLmeas.setNoise(FIXED_AMB_VAR);
				
				tracepdeex(ARTRCLVL+1, trace, "\n WLpiv %s %s %10.4f %13.4e", sat.id().c_str(), rec.c_str(), amb, FIXED_AMB_VAR);
				
				KFKey kfKey	= {KF::AMBIGUITY, sat, rec, typ};
				init.Q = 0;
				WLmeas.addDsgnEntry( kfKey, 1, init);
				WLmeasList.push_back(WLmeas);
				nmea++;
			}
		}
		
		for ( auto& [sat,nrec] : satlst )
		{
			string rec = SATpivlist[typ][sys][sat].pre_rec;
			double amb = SATpivlist[typ][sys][sat].rec_amb[rec];
			ObsKey obsKey = { sat, rec, "WLpiv" };
			KFMeasEntry	WLmeas(&WLambKF, obsKey);
			WLmeas.setValue(amb);
			WLmeas.setNoise(FIXED_AMB_VAR);
			tracepdeex(ARTRCLVL+1, trace, "\n WLpiv %s %s %10.4f %13.4e", sat.id().c_str(), rec.c_str(), amb, FIXED_AMB_VAR);
			KFKey kfKey	= {KF::AMBIGUITY, sat, rec, typ};
			init.Q = 0;
			WLmeas.addDsgnEntry( kfKey, 1, init);
			WLmeasList.push_back(WLmeas);
			nmea++;
		}
	}
	
	WLambKF.stateTransition(trace, time);
	KFMeas combWL = WLambKF.combineKFMeasList(WLmeasList);
	combWL.time = time;
	
	if (AR_VERBO)
	{
		tracepdeex(ARTRCLVL, trace, ", npiv= %4d, ntot = %4d, nsta = %4d", nmea-amblst.size(), nmea, nsta);
		trace << std::endl << "H:" << std::endl << combWL.A << std::endl << std::endl;
		trace << std::endl << "P:" << std::endl << WLambKF.P << std::endl << std::endl;
		WLambKF.output_residuals = true;
	}
	
	if (WLambKF.lsqRequired)
	{
		tracepdeex(ARTRCLVL, trace, "\n Initializing some states with LSQ");
		WLambKF.lsqRequired = false;
		WLambKF.leastSquareInitStates(trace, combWL);
	}
	WLambKF.filterKalman(trace, combWL, false);
	
	if (AR_VERBO)
	{
		WLambKF.outputStates(trace);
		WLambKF.output_residuals = false;
	}
	
	for (auto& [kfKey, index] : WLambKF.kfIndexMap)
	if (kfKey.type == KF::AMBIGUITY)
	{
		double val = 0;
		double var = 0;
		WLambKF.getKFValue(kfKey, val, &var);
		KFKey mKey	= {KF::AMBIGUITY, kfKey.Sat, kfKey.str, typ};
		AR_mealist[mKey].flt_amb=val;
		AR_mealist[mKey].flt_var=var;
	}
	
	return;
}

/** Resolve wide-lane ambiguities */
void  reslv_WLambg(
	Trace& trace,		///< Trace file to output to
	GTime time,			///< Solution time
	GinAR_opt opt,		///< Ambiguity resolution options
	E_Sys sys,			///< GNSS system e.g. GPS, GLO, GAL ...
	E_AmbTyp typ,		///< Ambiguity type e.g. WL12, WL23
	string rec)			///< Receiver name
{
	KFState&    WLambKF = WL_ambfilt[rec][typ][sys];
	GinAR_mtx   ambState;
	
	vector<int> AmbReadindx;
	int ind = 0;
	
	string recid = "NETWORK";
	if (opt.endu) recid = opt.recv;
	auto& AR_mealist = ARstations[recid].AR_meaMap; 
	
	for (auto& [kfKey, index] : WLambKF.kfIndexMap)
	{
		if (kfKey.type == KF::AMBIGUITY 
		&& AR_mealist[kfKey].sat_ele >= opt.MIN_Elev_AR)
		{
			AmbReadindx.push_back(index);
			ambState.ambmap[ind++] = kfKey;
		}
	}
	ambState.aflt  = WLambKF.x(AmbReadindx);
	ambState.Paflt = WLambKF.P(AmbReadindx,AmbReadindx);
	
	int nfix = GNSS_AR(trace, ambState, opt);
	
	tracepdeex(ARTRCLVL, trace, "\n#ARES_WLAR Resolving WL ambiguities %s %s %s %4d %4d", time.to_string(0), rec, sys._to_string(), ambState.aflt.size(), nfix);
	
	KFState KFcopy = WLambKF;
	VectorXd fixX = ambState.zfix;

	KFcopy.initFilterEpoch();

	InitialState ini0;
	KFMeasEntryList	FixedList;

	for (int i = 0; i < nfix; i++)
	{
		ObsKey obsKey;
		obsKey.str = "WLint ";
		obsKey.num = i;
		
		KFMeasEntry	ARMeas(&KFcopy, obsKey);
		ARMeas.setValue(fixX(i));
		ARMeas.setNoise(0.1 * POSTAR_VAR);

		tracepdeex(ARTRCLVL, trace, "\n#ARES_WLAR Applying: ");	
		for (int j = 0; j < ind; j++) 
		{
			if (ambState.Ztrs(i, j) == 0)
				continue;
				
			auto kfKey = ambState.ambmap[j];
			ARMeas.addDsgnEntry( kfKey, ambState.Ztrs(i, j), ini0);
			tracepdeex(ARTRCLVL, trace, "%+.2f x (%s,%s) ", ambState.Ztrs(i, j), kfKey.str.c_str(), kfKey.Sat.id().c_str());
		}
		tracepdeex(ARTRCLVL, trace, "= %.2f", fixX(i));	
		
		FixedList.push_back(ARMeas);
	}

	KFcopy.stateTransition(trace, time);
	KFMeas combined = KFcopy.combineKFMeasList(FixedList);

	KFcopy.filterKalman(trace, combined, false);

	if (AR_VERBO)
		KFcopy.outputStates(trace);

	for (auto& [kfKey, index] : KFcopy.kfIndexMap)
	{
		double val;
		double var;
		KFcopy.getKFValue(kfKey, val, &var);
	
		if (kfKey.type == KF::AMBIGUITY)
		{
			if ( var < POSTAR_VAR)
			{
				KFKey meakey = {KF::AMBIGUITY,kfKey.Sat,kfKey.str,typ};
				AR_mealist[meakey].fix_fin = time;
				AR_mealist[meakey].int_amb = ROUND(val);
				AR_mealist[meakey].hld_epc = 0;
				if (WL_archive.find(meakey) != WL_archive.end())
				{
					int WLind = WL_arch_ind[meakey];
					WL_archive[meakey][WLind].fix_fin = time;
					WL_archive[meakey][WLind].int_amb = ROUND(val);
					WL_archive[meakey][WLind].hld_epc = 0;
				} 
			}
		}
		
		if (kfKey.type == KF::REC_SYS_BIAS)
		{
			GinAR_bia& bia = RECbialist[typ][sys][kfKey.str];
			if (bia.numsamp == 0)
			{				
				bia.intlevl = ROUND(val);
				bia.rawbias = 0;
			}
			else
			{
				bia.intlevl += ROUND(val-bia.rawbias);
			}
			
			bia.numsamp++;
			bia.rawbias = val;
			bia.outbias = val - bia.intlevl;
			bia.outvari = var;
		}
		
		if (kfKey.type == KF::PHASE_BIAS)
		{
			GinAR_bia& bia = SATbialist[typ][kfKey.Sat];
			if (bia.numsamp == 0)
			{				
				bia.intlevl = ROUND(val);
				bia.rawbias = 0;
			}
			else
			{
				bia.intlevl += ROUND(val-bia.rawbias);
			}
			
			bia.numsamp++;
			
			bia.rawbias = val;
			bia.outbias = val - bia.intlevl;
			bia.outvari = var;
		}
	}
	
}

/** Remove ambiguities and biases for receivers, network */
void remov_WLrecv(
	Trace& trace,		///< Debug Trace
	E_AmbTyp typ,		///< Ambiguity type e.g. WL12, WL23
	string rec,			///< Receiver name	
	E_Sys sys)			///< GNSS system e.g. GPS, GLO, GAL ...
{
	string index = "NETWORK";
	if (WL_ambfilt[index][typ].find(sys) == WL_ambfilt[index][typ].end()) 
		return;
	
	KFState& kfState = WL_ambfilt[index][typ][sys];
	for (auto [key, index] : kfState.kfIndexMap) 
	{
		if( key.str != rec )
			continue;
		tracepdeex(ARTRCLVL+2, trace,  "\nRemoving %s %s from WL filter", key.str.c_str(), key.Sat.id().c_str());
		kfState.removeState(key);
	}
	
	for (auto & [key,ambmap] : WL_archive)
	{
		if (key.str != rec) 		continue;
		if (key.Sat.sys != sys) 	continue;
		
		WL_newsect(trace,typ,key.Sat,rec,GTime::noTime());
	}
	
	return;
}

/** Remove ambiguities and biases for satellites, network */
void remov_WLsate(
	Trace& trace,		///< Debug Trace
	E_AmbTyp typ,		///< Ambiguity type e.g. WL12, WL23
	SatSys sat)			///< Satellite
{
	string index = "NETWORK";
	if (WL_ambfilt[index][typ].find(sat.sys) == WL_ambfilt[index][typ].end()) 
		return;
	
	KFState& kfState = WL_ambfilt[index][typ][sat.sys];
	for (auto [key, index] : kfState.kfIndexMap) 
	if ( key.Sat == sat)
	{
		tracepdeex(ARTRCLVL+2, trace,  "\nRemoving %s %s from WL filter", key.str.c_str(), key.Sat.id().c_str());
		kfState.removeState(key);
	}
	
	for (auto & [key,ambmap] : WL_archive)
	if (key.Sat == sat)
	{
		WL_newsect(trace,typ,sat,key.str,GTime::noTime());
	}
	
	return;
}

/** Retrieve WL ambiguities from archive */
int  retrv_WLambg(
	Trace& trace,		///< Debug trace
	E_AmbTyp typ,		///< Ambiguity type e.g. WL12, WL23 		
	GTime time, 		///< Time of solutions
	string rec, 		///< Receiver name		
	SatSys sat)			///< Satellite 
{
	KFKey key = {KF::AMBIGUITY,sat,rec,typ};
	auto ambmap = WL_archive[key];
	
	tracepdeex(ARTRCLVL+2, trace, "\n#ARES_WLAR Retrieving ambiguity for %s %s %s %d", time.to_string(0).c_str(), rec.c_str(), sat.id().c_str(), ambmap.size());
	
	if (ambmap.size()==0) 
		return INVALID_WLVAL;
	
	for (auto& [ind,amb] : ambmap)
	{
		tracepdeex(ARTRCLVL+2, trace, "\n      Entry: %s %s %d", amb.sec_ini.to_string(0), amb.mea_fin.to_string(0), amb.hld_epc);
		
		if (      amb.hld_epc  < 0) 		continue;
		if ((time-amb.mea_fin) > DTTOL) 	continue;
		if ((time-amb.sec_ini) < -DTTOL)	continue;
		return amb.int_amb;
	}
	return INVALID_WLVAL;
}

/** Update WL ambiguities and save then to archive */
int  updat_WLambg(
	Trace& trace,		///< Debug Trace
	E_AmbTyp typ,		///< Ambiguity type e.g. WL12, WL23 
	GTime time, 		///< Time of latest observations
	E_Sys sys,			///< GNSS system e.g. GPS, GLO, GAL ...
	GinAR_opt opt)		///< Ambiguity resolution options
{
	string rec = opt.recv;
	
	string recid = "NETWORK";
	if (opt.endu) recid = opt.recv;
	auto& AR_mealist = ARstations[recid].AR_meaMap; 
	
	for (auto& [key,amb] : AR_mealist)
	{
		if (key.Sat.sys != sys)				continue;
		if (key.num     != typ)				continue;
		if (key.str     != rec && opt.endu)	continue;
		
		if (WL_archive[key].empty() 
		 || amb.cyl_slp) 
			WL_newsect(trace,typ,key.Sat,key.str,time);
		
		if (amb.hld_epc >= 0) 
			amb.hld_epc++; 
	}
	
	for (auto& [key,ambmap] : WL_archive)
	{
		if (key.Sat.sys != sys)				continue;
		if (key.num     != typ)				continue;
		if (key.str     != rec && opt.endu)	continue;
		if (ambmap.empty()) 			continue;
		
		int WLind = WL_arch_ind[key];
		GinAR_amb& amb = ambmap[WLind];
		if (amb.hld_epc >= 0) 
			amb.hld_epc++; 
	}
	
	updat_WLfilt   (trace,time,opt,sys,typ);
	reslv_WLambg   (trace,time,opt,sys,typ,rec);

	return  0;
}

/** Dump WL ambiguities in archive */
void dump__WLambg( 
	Trace& trace ) ///< Trace file to output to
{
	for (auto& [key,ambmap] : WL_archive)
	{
		for (auto& [ind,amb] : ambmap) 
		{
			E_AmbTyp typs = E_AmbTyp::_from_integral(key.num);
			tracepdeex(ARTRCLVL, trace, "\n#ARES_WLAR archived ambiguity: %s %s %s", key.Sat.id().c_str(), key.str.c_str(), typs._to_string());
			tracepdeex(ARTRCLVL, trace, " %s %s %s",amb.sec_ini.to_string(0).c_str(),amb.mea_fin.to_string(0).c_str(),amb.fix_fin.to_string(0).c_str());
			tracepdeex(ARTRCLVL, trace, " %4d %4d %5d", amb.hld_epc, ind, amb.int_amb);
		}
	}
}
