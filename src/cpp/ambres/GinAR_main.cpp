#include "GNSSambres.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "tides.hpp"

#define SMP2RESET	20

KFState ARcopy;
map<E_Sys, bool> sys_solve;
map<string,map<E_Sys, bool>> sys_activ;
GinAR_opt defAR_WL;
GinAR_opt defAR_NL;
map <E_Sys, map<E_FType,bool>> defCodesChecked;
bool FIXREADY = false;								/* network ambiguity resolution rate */

map<string,GinAR_rec> ARstations;
map<SatSys,GinAR_sat> ARsatellites;


map<KFKey,map<GTime,double>>	elev_archive;
map<KFKey,list<GTime>>      	slip_archive;

map<E_Sys, E_ObsCode> defCodesL1 =
{
	{E_Sys::GPS, E_ObsCode::L1C},
	{E_Sys::GLO, E_ObsCode::L1C},
	{E_Sys::GAL, E_ObsCode::L1C},
	{E_Sys::BDS, E_ObsCode::L2I},
	{E_Sys::QZS, E_ObsCode::L1C}
};

map<E_Sys, E_ObsCode> defCodesL2 =
{
	{E_Sys::GPS, E_ObsCode::L2W},
	{E_Sys::GLO, E_ObsCode::L2C},
	{E_Sys::GAL, E_ObsCode::L5Q},
	{E_Sys::BDS, E_ObsCode::L7I},
	{E_Sys::QZS, E_ObsCode::L2S}
};

map<E_Sys, E_ObsCode> defCodesL3 =
{
	{E_Sys::GPS, E_ObsCode::L5X},
	{E_Sys::GLO, E_ObsCode::L3Q},
	{E_Sys::GAL, E_ObsCode::L7Q},
	{E_Sys::BDS, E_ObsCode::L6I},
	{E_Sys::QZS, E_ObsCode::L5Q}
};

/** Default frequencies for each system */
bool sys_frq(
	short int sys,		///< GNSS system e.g. GPS, GLO, GAL ...
	E_FType& frq1,		///< frequency of first Carrier
	E_FType& frq2,		///< frequency of second Carrier
	E_FType& frq3)		///< frequency of third Carrier
{
	switch (sys)
	{
		case E_Sys::GPS:
			frq1 = F1;
			frq2 = F2;
			frq3 = F5;
			if (acsConfig.ionoOpts.iflc_freqs == +E_LinearCombo::L1L5_ONLY)
			{
				frq2 = F5; 
				frq3 = F2;	
			}
			return true;

		case E_Sys::GLO:
			frq1 = F1;
			frq2 = F2;
			frq3 = FTYPE_NONE;
			return true;

		case E_Sys::GAL:
			frq1 = F1;
			frq2 = F5;
			frq3 = F7;
			return true;

		case E_Sys::QZS:
			frq1 = F1;
			frq2 = F2;
			frq3 = F5;
			return true;

		case E_Sys::BDS:
			frq1 = B1;
			frq2 = B2;
			frq3 = B3;
			return true;
	}

	return false;
}

/** Retrieving satellte elevation at given time */
double retrv_elev(
	KFKey kfkey,			///< KFKey structure containing satelite and receiver
	GTime time)				///< Time
{
	if (elev_archive.find(kfkey) == elev_archive.end())	return -1;
	
	auto elev_list = elev_archive[kfkey];
	for (auto& [atim,elev] : elev_list)
	{
		double dt = time-atim;
		if (dt > DTTOL)  								continue;
		if (dt < -acsConfig.ambrOpts.Max_Hold_time)		return -1;
		return elev;
	}
	return -1;
}

/** Eliminated ambiguity measurements older than (current time - max hold epocs) for the specified receiver*/
void clean_obsolete(
	GTime time,			///< Current time
	string recv)		///< Receiver
{
	auto& AR_mealist = ARstations[recv].AR_meaMap;
	for (auto it = AR_mealist.begin(); it != AR_mealist.end();)
	{
		if (  recv != "NETWORK" 
		   && recv != it->first.str) 
			continue;
		
		auto& AR_mea = it->second;
		
		if ( (time - AR_mea.mea_fin) > defAR_WL.Max_Hold_tim
		  || (time - AR_mea.mea_fin) < -DTTOL
		  || (       AR_mea.out_epc) > defAR_WL.Max_Hold_epc)
		{
			it = AR_mealist.erase(it);
		}
		else
		{
			AR_mea.out_epc++;
			AR_mea.cyl_slp = false;
			it++;
		}
	}
}

GinAR_sat* GinAR_sat_metadata(SatSys sat)
{
	return &ARsatellites[sat];
}

double eclipse_safe(
	Obs& obs)
{
	ERPValues erpv;
	
	Vector3d rSun;
	sunmoonpos(gpst2utc(obs.time), erpv, &rSun);
	
	Vector3d rSat = obs.rSat;
	Vector3d vSat = obs.satVel;
	
	Vector3d rOrb = rSat.cross(vSat);
	
	Vector3d eOrb = rOrb.normalized();
	Vector3d eSun = rSun.normalized();
	
	return eSun.dot(eOrb);
}

/** Loading float ambiguity data from Observations (WL, cycle slip & elevation) */
void LoadFromObs( 
	Trace& trace,			///< Trace file to output to
	ObsList& obslst, 		///< Observation list
	GTime time, 			///< Observation time (usually from KF or solutions)
	string recv)			///< Receiver network identifier
{
	if (obslst.size() <= 0)
	{
		RecBlackList[recv]+=100;
		return;
	}
	else if (RecBlackList[recv]>0)
	{
		RecBlackList[recv]--;
	}
	
	auto& AR_mealist = ARstations[recv].AR_meaMap;
	
	tracepdeex(ARTRCLVL+1, trace, "\n#ARES_MEA Saving metadata for %s %s", time.to_string(0).c_str(), obslst.front().mount.c_str());
	for (auto& obs : obslst)
	{
		SatSys sat = obs.Sat;
		E_Sys  sys = sat.sys;
		
		if (!sys_solve[sys])								continue;
		
		E_FType frq1;
		E_FType frq2;
		E_FType frq3;
		if (!sys_frq(sys, frq1, frq2, frq3))				continue;
		
		double elev = obs.satStat_ptr->el;
		if (elev < acsConfig.elevation_mask)				continue;
		
		auto& satOpts = acsConfig.getSatOpts(sat);
		
		if (satOpts.exclude)
		{
			continue;
		}
		
		tracepdeex(ARTRCLVL+1, trace, ", %s", sat.id().c_str());
		
		if (!defAR_WL.endu)
		{
			if (ARsatellites.find(sat) == ARsatellites.end())
				ARsatellites[sat].anchor_potential = eclipse_safe(obs);
			
			ARsatellites[sat].elevations[obs.mount] = elev;
		}
		
		KFKey key = {KF::AMBIGUITY,sat,obs.mount,0};
		auto& elev_list = elev_archive[key];
		for (auto it = elev_list.begin(); it != elev_list.end();)
		{
			if ((time-(it->first)) > MAX_ARCH)
			{
				it = elev_list.erase(it);
			}
			else break;
		}
		elev_archive[key][time] = elev;
		
		if ( obs.Sigs[frq1].P_corr_m > 0
		 && defAR_WL.defCodes[sys].find(frq1) == defAR_WL.defCodes[sys].end())
		{
			defAR_WL.defCodes[sys][frq1] = obs.Sigs[frq1].code;
			defAR_NL.defCodes[sys][frq1] = obs.Sigs[frq1].code; 
		}
		
		if ( obs.Sigs[frq2].P_corr_m > 0
		 && defAR_WL.defCodes[sys].find(frq2) == defAR_WL.defCodes[sys].end())
		{
			defAR_WL.defCodes[sys][frq2] = obs.Sigs[frq2].code;
			defAR_NL.defCodes[sys][frq2] = obs.Sigs[frq2].code; 
		}
		
		if ( obs.Sigs[frq3].P_corr_m > 0
		 && defAR_WL.defCodes[sys].find(frq3) == defAR_WL.defCodes[sys].end())
		{
			defAR_WL.defCodes[sys][frq3] = obs.Sigs[frq3].code;
			defAR_NL.defCodes[sys][frq3] = obs.Sigs[frq3].code; 
		}
		
		auto& slip_list = slip_archive[key];
		for (auto it = slip_list.begin(); it != slip_list.end();)
		{
			GTime slip_time = *it;
			if ((time-slip_time) > MAX_ARCH)	it = slip_list.erase(it);
			else								break;
		}
		
		if ( defAR_WL.ionmod == +E_IonoMode::ESTIMATE)
		{
			if (obs.satStat_ptr->sigStatMap[frq1].slip.any)
			{
				key.num = E_AmbTyp::UCL1;
				slip_archive[key].push_back(time);
				tracepdeex(ARTRCLVL+2, trace, "L1 slip ");
			}
			if (obs.satStat_ptr->sigStatMap[frq2].slip.any)
			{
				key.num = E_AmbTyp::UCL2;
				slip_archive[key].push_back(time);
				tracepdeex(ARTRCLVL+2, trace, "L2 slip ");
			}
			if (obs.satStat_ptr->sigStatMap[frq3].slip.any)
			{
				key.num = E_AmbTyp::UCL3;
				slip_archive[key].push_back(time);
				tracepdeex(ARTRCLVL+2, trace, "L3 slip ");
			}
			tracepdeex(ARTRCLVL+2, trace, "\n");
			sys_activ[recv][sys]=true;
			continue;
		}
		
		if ( defAR_WL.ionmod == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
		{
			bool slip = false;
			if (obs.satStat_ptr->sigStatMap[frq1].slip.any						// To do change from "any" to MW, GF && CDIA
			 || obs.satStat_ptr->sigStatMap[frq2].slip.any )
			{
				key.num = E_AmbTyp::NL12;
				slip_archive[key].push_back(time);
				slip = true;
				tracepdeex(ARTRCLVL+2, trace, "L12 slip ");
			}
			
			double lam1 = obs.satNav_ptr->lamMap[frq1];
			double lam2 = obs.satNav_ptr->lamMap[frq2];
			if ( lam1 == 0 || lam2 == 0) 
				continue;
			
			if (!obs.Sigs[frq1].phaseBias
			 && defAR_WL.endu) 
				continue;
				
			if (!obs.Sigs[frq2].phaseBias
			 && defAR_WL.endu)
				continue;
			
			
			double L1 = obs.Sigs[frq1].L_corr_m / lam1;
			double P1 = obs.Sigs[frq1].P_corr_m / lam1;
			double L2 = obs.Sigs[frq2].L_corr_m / lam2;
			double P2 = obs.Sigs[frq2].P_corr_m / lam2;
			if ( L1 == 0 || L2 == 0 || P1 == 0 || P2 == 0) 
				continue;
			
			sys_activ[recv][sys]=true;
			
			double Kwl12 = (lam2 - lam1) / (lam1 + lam2);
			double WL12  = L1 - L2 - Kwl12 * (P1 + P2);
			double WL12v = (obs.Sigs[frq1].phasVar + SQR(Kwl12) * obs.Sigs[frq1].codeVar) / SQR(lam1);
				   WL12v+= (obs.Sigs[frq2].phasVar + SQR(Kwl12) * obs.Sigs[frq2].codeVar) / SQR(lam2);
			
			tracepdeex(ARTRCLVL+2, trace, "WL12: %.4f, var: %.4e\n", WL12, WL12v);
			
			key.num = E_AmbTyp::WL12;
			if (AR_mealist.find(key) == AR_mealist.end())
			{
				AR_mealist[key].sec_ini = obs.time;
				AR_mealist[key].hld_epc = -1;
				AR_mealist[key].flt_var = -1;
			}
			else if ( slip )
			{
				AR_mealist[key].hld_epc = -1;
				AR_mealist[key].flt_var = -1;
			}
			AR_mealist[key].raw_amb = WL12;
			AR_mealist[key].raw_var = WL12v;
			AR_mealist[key].sat_ele = elev;
			AR_mealist[key].out_epc = 0;
			AR_mealist[key].mea_fin = obs.time;
			AR_mealist[key].cyl_slp = slip;
			// if(AR_mealist[key].flt_var<=0)
			// {
			// 	AR_mealist[key].flt_amb = WL12;
			// 	AR_mealist[key].flt_var = WL12v
			// }
			// else
			// {
			// 	double Kasim = AR_mealist[key].flt_var/(WL12v + AR_mealist[key].flt_var)
			// 	AR_mealist[key].flt_amb+= Kasim*(WL12 - AR_mealist[key].flt_amb);
			// 	AR_mealist[key].flt_var = Kasim* WL12v;
			// }
		}
	}
}

/** Loading float ambiguity data from KF */
int LoadFromFlt( 
	Trace& trace,				///< Trace file to output to
	KFState& kfState, 			///< KF containing float ambiguity solutions
	int& nsat,					///< Number of satellites
	int& nrec,					///< Number of receivers
	string recv)				///< Receiver/Network ID
{	
	GTime time = kfState.time;
	int nmeas = 0;
	double ARthres = D2R * acsConfig.ambrOpts.min_el_AR;
	
	ARstations[recv].AmbTypMap.clear();
	
	map<SatSys,int> Satlist;
	map<string,int> Reclist;
	
	tracepdeex(ARTRCLVL, trace, "\n#ARES_MEA Loading from KF: %s", time.to_string(0).c_str());
	
	auto& AR_mealist = ARstations[recv].AR_meaMap;
	auto& AmbTyplist = ARstations[recv].AmbTypMap; 
	
	for (auto& [key,indx] : kfState.kfIndexMap)
	{
		SatSys sat = key.Sat;
		E_Sys sys  = key.Sat.sys;
		E_FType frq1;
		E_FType frq2;
		E_FType frq3;
		
		if ( defAR_WL.endu && key.type != KF::PHASE_BIAS)	continue;
		if (!defAR_WL.endu && key.type != KF::AMBIGUITY)	continue;
		if (!sys_solve[sys])								continue;
		if (!sys_frq(sys, frq1, frq2, frq3))				continue;
		
		string rec = key.str;
		KFKey  key2 = {KF::AMBIGUITY,sat,rec,0};
		double elev = retrv_elev(key2,time);
		if (elev < 0) 
		{
			tracepdeex(ARTRCLVL+2, trace, "\n#ARES_MEA Failed to retrieve elevation elevation for %s %s", key.str.c_str(), key.Sat.id().c_str());
			continue;
		}
		
		int    WLamb = 0;
		double lamNL = 1;
		if ( defAR_NL.ionmod == +E_IonoMode::IONO_FREE_LINEAR_COMBO )
		{
			WLamb = retrv_WLambg(trace, E_AmbTyp::WL12, time, rec, sat);
			if ( WLamb == INVALID_WLVAL)
			{
				tracepdeex(ARTRCLVL+2, trace, "\n#ARES_MEA Failed to retrieve WL ambiguities for %s %s", key.str.c_str(), key.Sat.id().c_str());
				continue;
			}
			lamNL = defAR_NL.wavlen[sys];
			tracepdeex(ARTRCLVL+2, trace, "\n#ARES_MEA Wavelength for measurements  %s %s %.5f", key.str.c_str(), key.Sat.id().c_str(), lamNL);
			if (lamNL<=0)
				continue;
		}
		
		tracepdeex(ARTRCLVL+1, trace, "\n#ARES_MEA Loading NL measurements for %s %s %2d ", key.str.c_str(), key.Sat.id().c_str(), key.num);
		
		/* retrieving cycle slips */
		bool sig_slip = false; 
		for(auto& slip_tim : slip_archive[key2])
		{
			double dt = time-slip_tim;
			if (fabs(dt)>DTTOL)
				continue;
			
			sig_slip = true;
		}
		
		double rawNL = 0, NLvar = 0;
		kfState.getKFValue(key, rawNL, &NLvar);
		rawNL = (rawNL - defAR_NL.wlfact[sys] * WLamb)/lamNL;
		NLvar/= lamNL*lamNL;
		
		E_AmbTyp typ = E_AmbTyp::NL12;
		if ( defAR_NL.ionmod == +E_IonoMode::ESTIMATE )
		{
			if (key.num == frq1) typ = E_AmbTyp::UCL1;
			if (key.num == frq2) typ = E_AmbTyp::UCL2;
			if (key.num == frq3) typ = E_AmbTyp::UCL3;
		}
		if (AmbTyplist.find(typ) == AmbTyplist.end())	AmbTyplist[typ] = 1;
		else											AmbTyplist[typ]+= 1;
		
		key2.num=typ;
		if (AR_mealist.find(key2) == AR_mealist.end())
		{
			AR_mealist[key2].sec_ini = time;
			AR_mealist[key2].hld_epc = -1;
		}
		else // independent test of cycle slip
		{
			double difNL = fabs(rawNL - AR_mealist[key2].raw_amb);
			double stdNL = sqrt(NLvar + AR_mealist[key2].raw_var);
			
			if (sig_slip
			 || (difNL>0.5 && difNL>(3*stdNL)) )
			{
				AR_mealist[key2].hld_epc = -1;
				AR_mealist[key2].cyl_slp = true;
				AR_mealist[key2].flt_var = -1;
			}
		}
		AR_mealist[key2].raw_amb = rawNL;
		AR_mealist[key2].raw_var = NLvar;
		AR_mealist[key2].sat_ele = elev;
		AR_mealist[key2].mea_fin = time;
		AR_mealist[key2].out_epc = 0;
		if (elev > ARthres)
		{
			nmeas++;
			Satlist[key.Sat]++;
			Reclist[key.str]++;
		}
	}	
	
	nsat = Satlist.size();
	nrec = Reclist.size();
	
	return nmeas; 						/// Number of ambituities loaded
}		

/** Loading configuration options from acsConfig */
void config_AmbigResl(void)
{
	for (auto& [sys,act]: acsConfig.solve_amb_for)
	if (acsConfig.process_sys[sys])
	{
		sys_solve[sys] = act;
		if(act)
			std::cout << std::endl << "Solving ambiguities for " << sys._to_string();
		else
			std::cout << std::endl << "NOT Solving ambiguities for " << sys._to_string();
	}
	std::cout << std::endl;

	if (acsConfig.trace_level > 4)    
		AR_VERBO = true;

	RECpivlist.clear();
	SATpivlist.clear();
	
	ARstations.clear();
	RECpivlist.clear();
	SATpivlist.clear();
	RECbialist.clear();
	SATbialist.clear();
	
	if (!acsConfig.process_network) 	defAR_WL.endu = true;
	else								defAR_WL.endu = false;
	
	defAR_WL.recv   = "";
	defAR_WL.mode   = acsConfig.ambrOpts.WLmode;
	defAR_WL.nset   = acsConfig.ambrOpts.lambda_set;
	defAR_WL.nitr   = acsConfig.ambrOpts.AR_max_itr;
	defAR_WL.MIN_Elev_prc = acsConfig.elevation_mask;
	defAR_WL.MIN_Elev_AR  = D2R * acsConfig.ambrOpts.min_el_AR;
	defAR_WL.MIN_Elev_piv = defAR_WL.MIN_Elev_AR + 5 * D2R;
	defAR_WL.sucthr = acsConfig.ambrOpts.WLsuccsThres;
	defAR_WL.ratthr = acsConfig.ambrOpts.WLratioThres;
	defAR_WL.Max_Hold_epc = acsConfig.ambrOpts.Max_Hold_epoc;
	defAR_WL.Max_Hold_tim = acsConfig.ambrOpts.Max_Hold_time;
	
	defAR_WL.wlsatp = (acsConfig.ambrOpts.WLSatPrcNois) * (acsConfig.ambrOpts.WLSatPrcNois);
	defAR_WL.wlrecp = (acsConfig.ambrOpts.WLRecPrcNois) * (acsConfig.ambrOpts.WLRecPrcNois);
	defAR_WL.wlmxit = acsConfig.ambrOpts.WL_filter_iter;
	defAR_WL.wlmxrm = acsConfig.ambrOpts.WL_prefit_remv;
	defAR_WL.wlfact.clear();
	
	if (!acsConfig.process_rts)
		defAR_WL.clear_old_amb = true;
	
	defAR_WL.ionmod = acsConfig.ionoOpts.corr_mode;
	if ( !(defAR_WL.ionmod == +E_IonoMode::IONO_FREE_LINEAR_COMBO) 	
	  && !(defAR_WL.ionmod == +E_IonoMode::ESTIMATE))					
		  defAR_WL.ionmod =  E_IonoMode::OFF;
	
	defAR_WL.bias_update = acsConfig.ambrOpts.biasOutrate;
	
	defAR_WL.type = E_AmbTyp::WL12;
	
	for (auto& [sys, act] : sys_solve) 
	{
		if (!act)
			continue;
			
		AR_reflist[sys] = acsConfig.pivot_station;
		defAR_WL.sys_solve[sys] = sys_solve[sys];
		defAR_WL.wavlen[sys] = 1;
		defAR_WL.wlfact[sys] = 0;
		
		
		E_FType frq1 = F1;
		E_FType frq2 = F2;
		E_FType frq3 = F5;
		sys_frq(sys, frq1, frq2, frq3);
		
		if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
		{
			double lam1 = lam_carr[frq1];
			double lam2 = lam_carr[frq2];
			defAR_WL.wavlen[sys] = lam1*lam2/(lam2+lam1);
			defAR_WL.wlfact[sys] = lam1*defAR_WL.wavlen[sys]/(lam2-lam1);
		}
		
		if (acsConfig.ionoOpts.auto_select_default_code == false)
		{
			defAR_WL.defCodes[sys][frq1] = defCodesL1[sys];
			defAR_WL.defCodes[sys][frq2] = defCodesL2[sys];
			defAR_WL.defCodes[sys][frq3] = defCodesL3[sys];
		}
	}
	
	defAR_NL		= defAR_WL;
	defAR_NL.mode	= acsConfig.ambrOpts.NLmode;
	defAR_NL.sucthr	= acsConfig.ambrOpts.NLsuccsThres;
	defAR_NL.ratthr	= acsConfig.ambrOpts.NLratioThres;
}

/** Ambiguity resolution for network solutions 
 */
int  networkAmbigResl( 
	Trace& trace,				///< Trace file to output to
	StationMap& stations,		///< Station struct, containing GNSS observations
	KFState& kfState)			///< KF containing float network solutions
{
	if ( acsConfig.ionoOpts.corr_mode != +E_IonoMode::IONO_FREE_LINEAR_COMBO )
		acsConfig.ambrOpts.WLmode = E_ARmode::OFF;
	
	if ( acsConfig.ambrOpts.NLmode == +E_ARmode::OFF )		return 0;
	if ( defAR_WL.endu )									return 0;
	
	GTime time = kfState.time;
	tracepdeex(ARTRCLVL, trace, "\n#ARES_MAIN Ambiguity Resolution on %3d Stations at %s",
	           stations.size(), time.to_string(0).c_str());
	

	/* Clean obsolete measurements */ 
	string recv = "NETWORK";
	if (!ARstations[recv].AR_meaMap.empty()) 
		clean_obsolete(time, recv);
	
	ARsatellites.clear();
	
	/* Load data from observations */
	for (auto& [rec,station] : stations)
	{
		auto& recOpts = acsConfig.getRecOpts(rec);

		if (recOpts.exclude) 
			continue;

		LoadFromObs( trace, station.obsList, time, recv );
	}
	
	for (auto& [typ,sysmap] : RECpivlist)
	for (auto& [sys,stamap] : sysmap)
	for (auto& [sta,pivmap] : stamap)
		if (RecBlackList.find(sta)==RecBlackList.end())
			RecBlackList[sta]=100;
	
	GinAR_opt opt = defAR_WL;
	opt.recv = "NETWORK";
	/* update pivot moved to WL and NL estimation functions */
	if	( acsConfig.ambrOpts.WLmode != +E_ARmode::OFF )
	{
		updt_net_pivot (trace, kfState.time,opt);
		for ( auto& [sys,act] : sys_solve ) 
		{
			if(!act)
				continue;
			updat_WLambg(trace,E_AmbTyp::WL12,time,sys,opt);
		}
	}
	
	if ( acsConfig.process_rts ) 
	{
		artrcout( trace, kfState.time, opt );
		return 0;
	}
	
	int nsat=0;
	int nrec=0;
	int namb = LoadFromFlt( trace, kfState, nsat, nrec, recv );
	
	opt = defAR_NL;
	opt.recv = "NETWORK";
	
	for(auto& [typ,nmeas] : ARstations[recv].AmbTypMap)
	{
		opt.type = typ;
		updt_net_pivot(trace, time,opt);
	}
	
	int nfix = updat_ambigt( trace, kfState, opt );
	
	if (nfix == 0) 
	{
		artrcout( trace, kfState.time, opt );
		return 0; 
	}
	
	nfix = apply_ambigt(trace, kfState, opt);
	
	if (nfix > 0.6*namb) FIXREADY = true;
	if (nfix < 0.4*namb) FIXREADY = false;
	
	artrcout(trace, kfState.time, opt);
	
	if	(  acsConfig.output_bias_sinex 
		&& acsConfig.ambrOpts.biasOutrate > 0)
	{
		arbiaout(trace, kfState.time, opt);
	}
	
	if (   acsConfig.process_ionosphere
		&& acsConfig.ionModelOpts.model != +E_IonoModel::NONE)
	for (auto& [rec,station] : stations)
	{ 
		auto& recOpts = acsConfig.getRecOpts(rec);

		if (recOpts.exclude) 
			continue;

		arionout( trace, kfState, station.obsList, opt );
	}
	
	if (AR_VERBO) 
		kfState.outputStates(trace, "/AR");
		
	ARcopy = kfState;
	return nfix;
}

/** Ambiguity resolution for end user solutions */
int  enduserAmbigResl( 
	Trace&		trace,			///< Trace file to output to
	ObsList&	obsList,		///< List of observables
	KFState&	kfState_float,	///< KF with end user solutions
	Vector3d	snxPos,			///< Apriori position estimate (from SINEX)
	double		dop,			///< Horizontal dilution of precision
	string		outfile)		///< solution filename
{
	if (acsConfig.ionoOpts.corr_mode != +E_IonoMode::IONO_FREE_LINEAR_COMBO)
		acsConfig.ambrOpts.WLmode = +E_ARmode::OFF;
	
	if (acsConfig.ambrOpts.NLmode == +E_ARmode::OFF )	return 0;
	if (!defAR_WL.endu )								return 0;
	if (obsList.empty())								return 0;
	
	GTime time = kfState_float.time;
	string recv = obsList.front().mount;
	GinAR_opt opt = defAR_WL;
	opt.recv = recv;
	
	ARstations[recv].kfState_fixed = kfState_float;
	
	TestStack::testMat(recv + "fixedx0", ARstations[recv].kfState_fixed.x);
	TestStack::testMat(recv + "fixedP0", ARstations[recv].kfState_fixed.P);
	
	ARstations[recv].snxPos_ = snxPos;
	for (short i = 0; i < 3; i++)
		kfState_float.getKFValue({KF::REC_POS,{}, recv,	i}, ARstations[recv].fltPos_[i]);

	if (!ARstations[recv].AR_meaMap.empty()) 
		clean_obsolete(time, recv);
	
	tracepdeex(ARTRCLVL, trace, "\n#ARES_MAIN End user ambiguity resolution: %s %s", time.to_string(0).c_str(), recv.c_str());
	sys_activ[recv].clear();
	LoadFromObs(trace, obsList, time, recv);
	
	if (acsConfig.ambrOpts.WLmode != +E_ARmode::OFF)
	{
		opt.sys_solve = sys_activ[recv];
		updt_usr_pivot (trace, time, opt);
		
		for (auto& [sys,act] : sys_solve) 
		if (act)
		{
			updat_WLambg(trace, E_AmbTyp::WL12, time, sys, opt);
		}
	}
	
	if (acsConfig.process_rts) 
	{
		artrcout(trace, time, opt);
		return 0;
	}
	
	int nsat = 0;
	int nrec = 0;
	int namb = LoadFromFlt(trace, ARstations[recv].kfState_fixed, nsat, nrec, recv);
	
	opt = defAR_NL;
	opt.recv = recv;
	opt.sys_solve = sys_activ[recv];
	for (auto& [typ,nmeas] : ARstations[recv].AmbTypMap)
	{
		opt.type = typ;
		updt_usr_pivot(trace,time,opt);
	}
	
	int nfix = updat_ambigt(trace, ARstations[recv].kfState_fixed, opt);
	
	nfix = apply_ambigt(trace, ARstations[recv].kfState_fixed, opt);
	
	ARstations[recv].kfState_fixed.outputStates(trace, "/FIXED");
	
	TestStack::testMat(recv + "fixed", ARstations[recv].kfState_fixed.x);
	TestStack::testMat(recv + "fixedP", ARstations[recv].kfState_fixed.P);

	for (short i = 0; i < 3; i++)
		 ARstations[recv].kfState_fixed.getKFValue({KF::REC_POS,{}, recv,	i}, ARstations[recv].fixPos_[i]);
		
	artrcout(trace, time, opt);

	if (acsConfig.output_ppp_sol)
	{
		if (nfix <= 0) 			gpggaout(outfile + "_AR", ARstations[recv].kfState_fixed, recv, 2, obsList.size(), dop,true);
		else					gpggaout(outfile + "_AR", ARstations[recv].kfState_fixed, recv, 5, obsList.size(), dop,true);
	}
	
	if	(  acsConfig.process_ionosphere  
		&& acsConfig.ionModelOpts.model != +E_IonoModel::NONE)
	{
		arionout(trace, ARstations[recv].kfState_fixed, obsList, opt);
	}
	
	if (   acsConfig.output_bias_sinex 
	    && acsConfig.ambrOpts.biasOutrate > 0 ) 
	{
    	arbiaout(trace, ARstations[recv].kfState_fixed.time, opt);
	}
	
	return nfix;
}

/** Ambiguity resolution on smoothed KF solutions*/
int smoothdAmbigResl( 
	KFState& kfState)		///< Smoothed Kf
{
	if (acsConfig.ambrOpts.NLmode == +E_ARmode::OFF)
		return 0;
	
	std::ofstream trace(acsConfig.config_description + "/RTS_AR_debug.trace",	std::ofstream::out | std::ofstream::app);
	kfState.rts_basename = acsConfig.pppOpts.rts_filename + "_RTS_AR";
	
	GTime time = kfState.time;
	
	tracepdeex(ARTRCLVL, trace, "\n#ARES__RTS Ambiguity resolution on smoothed solutions %s", time.to_string(0).c_str());
	
	string recv = "NETWORK";
	if (defAR_WL.endu) 
	for (auto& [key,indx] : kfState.kfIndexMap)
	{
		if	(  key.type == KF::REC_SYS_BIAS
			|| key.type == KF::REC_CLOCK)    
		{ 
			recv = key.str; 
			break; 
		}
	}
	
	GinAR_opt opt = defAR_NL;
	opt.recv = recv;
	
	if (!ARstations[recv].AR_meaMap.empty()) 
		clean_obsolete(time, recv);

	int nsat = 0;
	int nrec = 0;
	int namb = LoadFromFlt(trace, kfState, nsat, nrec, recv);
	tracepdeex(ARTRCLVL, trace, "\n#ARES_MAIN %5d measurements from %3d satellites and %4d", namb, nsat, nrec);
	
	for (auto& [typ,nmeas] : ARstations[recv].AmbTypMap)
	{
		opt.type = typ;
		if (defAR_WL.endu)		updt_usr_pivot(trace,kfState.time,opt);
		else					updt_net_pivot(trace,kfState.time,opt);
	}
	
	int nfix = updat_ambigt( trace, kfState, opt );
	
	if (nfix == 0) 
		return 0;
	
	tracepdeex(ARTRCLVL, trace, "\n#ARES_MAIN %5d fixed from %5d ambiguities", nfix, namb);
	nfix = apply_ambigt( trace, kfState, opt );
	
	if (nfix > 0.6 * namb)		FIXREADY = true;
	if (nfix < 0.4 * namb)		FIXREADY = false;
	
	if	(  acsConfig.output_bias_sinex 
		&& acsConfig.ambrOpts.biasOutrate > 0 )
	{
		arbiaout(trace, time, opt);
	}
	
	artrcout(trace, kfState.time, opt);
	
	if (AR_VERBO) 
		kfState.outputStates(trace, "/AR");
	
	ARcopy = kfState;
	
	return nfix;
}

/** Indicate if the AR solution is available/stable (60% of ambiguities solved) */
bool ARsol_ready()
{
	return FIXREADY;
}

/** Retrieved the last ambiguity resolved Kalman filter state */
KFState retrieve_last_ARcopy()
{
	return 	ARcopy;
}
