#include "GNSSambres.hpp"
#include "acsConfig.hpp"

#define SMP2RESET	10


map<E_Sys, bool> sys_solve;
int NumEpocAR		   = 0;
int NumEpocNL          = 0;
int NEPOC2NL		   = 120;
bool NLinactive		   = true;
bool AR_initialization = true;


bool AR_Iono_meas	= false;
bool ENDUSR_MODE	= false;
double biasOutrate	= 0;


KFState KF_ARcopy;
map<E_AmbTyp, ARState> ARcontr;

/* extract default frequencies for each system */
int sys_frq(short int sys, E_FType& frq1, E_FType& frq2, E_FType& frq3)
{
	switch (sys)
	{
		case E_Sys::GPS:
			frq1 = F1;
			frq2 = F2;
			frq3 = F5;
			return 1;

		case E_Sys::GLO:
			frq1 = F1;
			frq2 = F2;
			frq3 = FTYPE_NONE;
			return 2;

		case E_Sys::GAL:
			frq1 = F1;
			frq2 = F5;
			frq3 = F7;
			return 3;

		case E_Sys::QZS:
			frq1 = F1;
			frq2 = F2;
			frq3 = F5;
			return 4;

		case E_Sys::CMP:
			frq1 = B1;
			frq2 = B2;
			frq3 = B3;
			return 5;
	}

	return 0;
}

void start_from_acsConfig( double tgap )
{
	sys_solve[E_Sys::GPS] = acsConfig.ambrOpts.solvGPS;
	sys_solve[E_Sys::GLO] = acsConfig.ambrOpts.solvGLO;
	sys_solve[E_Sys::GAL] = acsConfig.ambrOpts.solvGAL;
	sys_solve[E_Sys::CMP] = acsConfig.ambrOpts.solvBDS;
	sys_solve[E_Sys::QZS] = acsConfig.ambrOpts.solvQZS;

	ARrefsta = "UNINIT";
	satpiv.clear();

	for (auto& [sys, act] : sys_solve)
	{
		StatAmbMap_list[sys].clear();
	}

	auto& AR_WL12 = ARcontr[E_AmbTyp::WL12];
	AR_WL12.endu   = ENDUSR_MODE;
	AR_WL12.recv   = "";
	AR_WL12.mode   = acsConfig.ambrOpts.WLmode;
	AR_WL12.nset   = acsConfig.ambrOpts.lambda_set;
	AR_WL12.nitr   = acsConfig.ambrOpts.AR_max_itr;

	if (AR_WL12.nitr <= 0) AR_WL12.nitr = 1;

	AR_WL12.prcele = acsConfig.elevation_mask;
	AR_WL12.arelev = D2R * acsConfig.ambrOpts.min_el_AR;
	AR_WL12.sucthr = acsConfig.ambrOpts.WLsuccsThres;
	AR_WL12.ratthr = acsConfig.ambrOpts.WLratioThres;
	AR_WL12.satprn = (acsConfig.ambrOpts.WLSatPrcNois) * (acsConfig.ambrOpts.WLSatPrcNois);
	AR_WL12.staprn = (acsConfig.ambrOpts.WLStaPrcNois) * (acsConfig.ambrOpts.WLStaPrcNois);

	auto& AR_NL12 = ARcontr[E_AmbTyp::NL12];
	AR_NL12.endu   = ENDUSR_MODE;
	AR_WL12.recv   = "";
	AR_NL12.mode   = acsConfig.ambrOpts.NLmode;
	AR_NL12.nset   = acsConfig.ambrOpts.lambda_set;
	AR_NL12.nitr   = acsConfig.ambrOpts.AR_max_itr;

	if (AR_NL12.nitr <= 0) AR_NL12.nitr = 1;

	AR_NL12.prcele = acsConfig.elevation_mask;
	AR_NL12.arelev = D2R * acsConfig.ambrOpts.min_el_AR;
	AR_NL12.sucthr = acsConfig.ambrOpts.NLsuccsThres;
	AR_NL12.ratthr = acsConfig.ambrOpts.NLratioThres;

	NLinactive = true;

	if (tgap > 0)
		NEPOC2NL = (int)(acsConfig.ambrOpts.NLstarttime / tgap);
}

/* Load ambiguity measurments ( NL ambiguity measurments and its covariance matrixes need to be loaded separately */
int Load_rawmeas(Trace& trace, ObsList& obslst, KFState& kfState, double tgap)
{
	E_FType frq1, frq2, frq3;
	GTime time = kfState.time;
	int trclvl = 3;
	int numobs = 0;

	for (auto& obs : obslst)
	{
		SatSys sat = obs.Sat;

		double elev = obs.satStat_ptr->el;

		if (!sys_solve[sat.sys]) 					continue;

		if (elev < acsConfig.elevation_mask) 		continue;

		if (!sys_frq(sat.sys, frq1, frq2, frq3)) 	continue;

		double lam1 = obs.satNav_ptr->lamMap[frq1];
		double lam2 = obs.satNav_ptr->lamMap[frq2];
		double lam3 = obs.satNav_ptr->lamMap[frq3];
		tracepdeex(4, trace, "\n#ARES_MEA %s lam= %.4f, %.4f, %.4f\n", sat.id().c_str(), lam1, lam2, lam3);

		if (lam1 == 0 || lam2 == 0) continue;

		/* Load WL12 measurements */
		double L1 = obs.Sigs[frq1].L_corr_m / lam1;
		double P1 = obs.Sigs[frq1].P_corr_m / lam1;
		double L2 = obs.Sigs[frq2].L_corr_m / lam2;
		double P2 = obs.Sigs[frq2].P_corr_m / lam2;

		if	( L1 == 0
		        || L2 == 0
		        || P1 == 0
		        || P2 == 0)
		{
			continue;
		}

		tracepdeex(3, trace, "\n#ARES_MEA Loading observables for %s-%s: %d %d %d", obs.mount, obs.Sat.id().c_str(), StatAmbMap_list[E_Sys::GPS].size(),  StatAmbMap_list[E_Sys::GAL].size(),  satpiv.size());

		if (satpiv.find(sat) == satpiv.end() || satpiv[sat].elev.size() == 0)
		{
			tracepdeex(3, trace, "\n#ARES_MAIN Initializing satellite biases for %s", sat.id().c_str());
			satpiv[sat].reset = true;
			satpiv[sat].npivot = 0;
			satpiv[sat].satbias.NL12var = -1;
			satpiv[sat].satbias.WL12var = -1;
			satpiv[sat].satbias.WL23var = -1;
			satpiv[sat].satbias_fix.NL12var = -1;
			satpiv[sat].satbias_fix.WL12var = -1;
			satpiv[sat].satbias_fix.WL23var = -1;
			satpiv[sat].bias_out.clear();
			satpiv[sat].NLwav = 0;
			satpiv[sat].WLinIF = 0;
		}

		for (int i = 0; i < 3; i++) satpiv[sat].rSat[i] = obs.rSat[i];

		if (satpiv[sat].NLwav <= 0.0)
		{
			satpiv[sat].NLwav = lam1 * lam2 / (lam1 + lam2);
			satpiv[sat].WLinIF = satpiv[sat].NLwav * lam1 / (lam2 - lam1);
			tracepdeex(3, trace, "#ARES_MAIN Starting satellite %s: lamNL= %.4f, WLcoef= %.4f\n",
			           sat.id().c_str(), satpiv[sat].NLwav, satpiv[sat].WLinIF);
		}

		string rec = obs.mount;

		auto& SystAmbMap = StatAmbMap_list[sat.sys];

		if (SystAmbMap.find(rec) == StatAmbMap_list[sat.sys].end())
			SystAmbMap[rec].reset = true;

		auto& recpiv = SystAmbMap[rec];

		double dtime = timediff(time, recpiv.update);

		if ( dtime > MEAS_OUTAG )
		{
			recpiv.SignList.clear();
			recpiv.reset = true;
		}

		recpiv.update = time;

		if (recpiv.reset)
		{
			recpiv.npivot = 0;
			recpiv.stabias.NL12var = -1;
			recpiv.stabias.WL12var = -1;
			recpiv.stabias.WL23var = -1;
			recpiv.stabias_fix.NL12var = -1;
			recpiv.stabias_fix.WL12var = -1;
			recpiv.stabias_fix.WL23var = -1;
		}


		bool rest = false;

		if (recpiv.SignList.find(sat) == recpiv.SignList.end())
			rest = true;

		SignAmbg& sigamb = recpiv.SignList[sat];

		if (rest)  /* new sat-rec pair */
		{
			tracepdeex(3, trace, "\n#ARES_SLP Initializing ambiguity for %s %s", sat.id().c_str(), rec);
			sigamb.pivot   = false;
			sigamb.state   = 0;
			sigamb.nepc    = 0;
			sigamb.flt.NL12var = -1;
			sigamb.flt.WL12var = -1;
			sigamb.flt.WL23var = -1;
			sigamb.fix.NL12var = -1;
			sigamb.fix.WL12var = -1;
			sigamb.fix.WL23var = -1;
		}

		if (obs.satStat_ptr->sigStatMap[frq1].slip.any) /* slip on frq1 */
		{
			tracepdeex(3, trace, "\n#ARES_SLP      Freq1 cycle slip %s %s", rec, sat.id().c_str());
			sigamb.state &= 1;
			sigamb.nepc   = 0;
			sigamb.flt.NL12var = -1;
			sigamb.flt.WL12var = -1;
			sigamb.fix.NL12var = -1;
			sigamb.fix.WL12var = -1;
			rest = true;
		}

		if (obs.satStat_ptr->sigStatMap[frq2].slip.any || (sigamb.nepc > 60 && sigamb.state < 2)) /* slip on frq2 */
		{
			tracepdeex(3, trace, "\n#ARES_SLP      Freq2 cycle slip %s %s", rec, sat.id().c_str());
			sigamb.state  = 0;
			sigamb.nepc   = 0;
			sigamb.flt.NL12var = -1;
			sigamb.flt.WL12var = -1;
			sigamb.flt.WL23var = -1;
			sigamb.fix.NL12var = -1;
			sigamb.fix.WL12var = -1;
			sigamb.fix.WL23var = -1;
			rest = true;
		}

		if (obs.satStat_ptr->sigStatMap[frq3].slip.any) /* slip on frq3 */
		{
			tracepdeex(3, trace, "\n#ARES_SLP      Freq3 cycle slip %s %s", rec, sat.id().c_str());
			sigamb.state &= 6;
			sigamb.nepc   = 0;
			sigamb.flt.WL23var = -1;
			sigamb.fix.WL23var = -1;
			rest = true;
		}

		/* saving ambiguity section data, and start new one */
		if ( rest ) start_new_sect(trace, sat, rec, time);

		sigamb.raw.NL12var = -1;
		sigamb.raw.WL12var = -1;
		sigamb.raw.WL23var = -1;
		sigamb.nfreq = 0;

		double Kwl12 = (lam2 - lam1) / (lam1 + lam2);
		sigamb.raw.WL12    = L1 - L2 - Kwl12 * (P1 + P2);
		sigamb.raw.WL12var = (obs.Sigs[frq1].phasVar + SQR(Kwl12) * obs.Sigs[frq1].codeVar) / SQR(lam1);
		sigamb.raw.WL12var += (obs.Sigs[frq2].phasVar + SQR(Kwl12) * obs.Sigs[frq2].codeVar) / SQR(lam2);
		sigamb.nfreq = 2;

		tracepdeex(4, trace, "\n#DEBG_MEA %s %d %d %10.4f %10.4e", sat.id().c_str(), obs.Sigs[frq1].code, obs.Sigs[frq2].code, sigamb.raw.WL12, sigamb.raw.WL12var);

		if (lam3 > 0)
		{
			double L3 = obs.Sigs[frq3].L_corr_m / lam3;
			double P3 = obs.Sigs[frq3].P_corr_m / lam3;

			if  (  L3 > 0
			        && P3 > 0)
			{
				double Kwl23 = (lam3 - lam2) / (lam2 + lam3);
				sigamb.raw.WL23    = L2 - L3 - Kwl23 * (P2 + P3);
				sigamb.raw.WL23var = (obs.Sigs[frq2].phasVar + SQR(Kwl23) * obs.Sigs[frq2].codeVar) / SQR(lam2);
				sigamb.raw.WL23var += (obs.Sigs[frq3].phasVar + SQR(Kwl23) * obs.Sigs[frq3].codeVar) / SQR(lam3);
				tracepdeex(4, trace, "  %d %10.4f %10.4e", obs.Sigs[frq3].code, sigamb.raw.WL23, sigamb.raw.WL23var);
				sigamb.nfreq = 3;
			}
		}

		/* Load NL12 measurments */
		KFKey NLambKey;

		if (ENDUSR_MODE)
		{
			if (acsConfig.ionoOpts.corr_mode == E_IonoMode::IONO_FREE_LINEAR_COMBO)
			{
				if (sat.sys == +E_Sys::GAL) 	NLambKey = {KF::PHASE_BIAS, sat, rec, 15};
				else							NLambKey = {KF::PHASE_BIAS, sat, rec, 12};
			}
		}
		else
			NLambKey = {KF::AMBIGUITY, sat, rec, 0};

		if (kfState.kfIndexMap.find(NLambKey) == kfState.kfIndexMap.end())
		{
			sigamb.raw.NL12var = -1;
			sigamb.flt.NL12var = -1;
			sigamb.fix.NL12var = -1;
			sigamb.state &= 3;
			tracepdeex(2, trace, "\n#ARES_NLR missing float ambiguity solution for %s %s", rec, sat.id().c_str());
		}
		else
		{
			double NLmeas = 0;
			double NLmeasVar = 0;
			kfState.getKFValue(NLambKey, NLmeas, &NLmeasVar);
			double WLint;

			if     	(sigamb.fix.WL12var == 0) 	WLint = ROUND(sigamb.fix.WL12);
			else if (sigamb.flt.WL12var >= 0) 	WLint = ROUND(sigamb.flt.WL12);
			else                             	WLint = ROUND(sigamb.raw.WL12);

			sigamb.raw.NL12		= (NLmeas - satpiv[sat].WLinIF * WLint) / satpiv[sat].NLwav;
			sigamb.raw.NL12var  = NLmeasVar / satpiv[sat].NLwav / satpiv[sat].NLwav;
			tracepdeex(4, trace, "#\nDEBG_MEA NLraw:  %.3f, %.3f, %.3f, %.3f", NLmeas, satpiv[sat].WLinIF, WLint, satpiv[sat].NLwav);
		}

		satpiv[sat].elev[rec] = elev;
		sigamb.elev = elev;
		sigamb.outage = 0;
		sigamb.nepc++;

		if (sigamb.nepc == 1)
		{
			sigamb.ref[0] = ROUND(sigamb.raw.NL12);
			sigamb.ref[1] = ROUND(sigamb.raw.WL12);
			sigamb.ref[2] = ROUND(sigamb.raw.WL23);
		}

		numobs++;

		int week;
		double tow = time2gpst(obs.time, &week);
		int dow = floor(tow / 86400);
		double tod = tow - 86400.0 * dow;
		tracepdeex(trclvl, trace, "\n#ARES_MEA,  %4d,%1d,%5.0f, %s, %s, %1d, %1d, %1d, %5.2f,%9.4f,%9.4f,%9.4f,%.3e,%.3e,%.3e,%d",
		           week, dow, tod, rec, sat.id().c_str(), sigamb.pivot ? 1 : 0, sigamb.state, sigamb.nfreq, sigamb.elev * R2D, sigamb.raw.NL12,
		           sigamb.raw.WL12, sigamb.raw.WL23, sigamb.raw.NL12var, sigamb.raw.WL12var, sigamb.raw.WL23var, recpiv.SignList.size());
	}

	return numobs;
}

/* Network ambiguity resolution */
int networkAmbigResl( Trace& trace, StationList& stations, KFState& kfState, double tgap)
{

	if (acsConfig.ambrOpts.WLmode == E_ARmode::OFF && acsConfig.ambrOpts.NLmode == E_ARmode::OFF) return 0;

	ENDUSR_MODE = false;
	int trclvl = 3;
	int nfix = 0;

	tracepdeex(trclvl, trace, "\n#ARES_CON Ambiguity Resolution on %3d Stations at %s",
	           stations.size(), kfState.time.to_string(0));

	bool Pivot_reset = false;

	if (AR_initialization)
	{
		AR_initialization = false;
		Pivot_reset = true;
		tracepdeex(trclvl, trace, "\n#ARES_MAIN Initializing configuration");
		start_from_acsConfig( tgap );

		if (acsConfig.process_ionosphere && acsConfig.ionFilterOpts.model != +E_IonoModel::NONE)
			AR_Iono_meas = true;

		if (acsConfig.output_biasSINEX && acsConfig.ambrOpts.biasOutrate > 0)		biasOutrate = acsConfig.ambrOpts.biasOutrate;
		else																	 	biasOutrate = 0;
	}


	for (auto& [sat, samb] : satpiv)
	{
		samb.reset = false;
	}

	for (auto& [sys, act] : sys_solve)
	{
		if (act == false)
		{
			continue;
		}

		for (auto& [rec, samb] : StatAmbMap_list[sys] )
		{
			for (auto it = samb.SignList.begin(); it != samb.SignList.end();  )
			{
				auto& sat    = it->first;
				auto& sigamb = it->second;
				sigamb.outage++;

				if (sigamb.outage > SMP2RESET)
				{
					start_new_sect(trace, sat, rec, kfState.time);
					tracepdeex(2, trace, "\n#ARES_CON Signal for %s %s out", rec, sat);
					it = samb.SignList.erase(it);
					satpiv[sat].elev.erase(rec);
				}
				else
					it++;
			}

			if (samb.SignList.size() == 0)		samb.reset = true;
			else								samb.reset = false;
		}
	}

	for (auto& rec_ptr : stations)
	{
		auto& rec = *rec_ptr;
		auto& recOpts = acsConfig.getRecOpts(rec.id);

		if (recOpts.exclude) continue;

		Load_rawmeas(trace, rec.obsList, kfState, tgap);
	}

	tracepdeex(2, trace, "\n#ARES_MAIN Solving pivot\n");

	if (Pivot_reset)
	{
		Pivot_reset = false;
		NumEpocAR = 1;
		NLinactive = true;
		init_net_pivot ( trace, acsConfig.ambrOpts.min_el_AR, acsConfig.pivot_station );

	}
	else
	{
		NumEpocAR++;
		updt_net_pivot ( trace, NLinactive);

		if (NumEpocAR > NEPOC2NL && NLinactive == true)
		{
			for (auto [sys, act] : sys_solve)
			{
				if (act == false)
				{
					continue;
				}

				SatSys sat0;
				sat0.sys = sys;
				rese_net_NL(trace, ARrefsta, sat0);
			}

			NLinactive = false;
		}
	}

	tracepdeex(3, trace, "\n#ARES_MAIN estimating WL ambiguities\n");

	if (acsConfig.ambrOpts.WLmode != E_ARmode::OFF)
	{
		WLambEstm(trace, kfState.time, tgap, ARcontr[E_AmbTyp::WL12], NLinactive);
	}

	tracepdeex(3, trace, "\n#ARES_MAIN estimating NL ambiguities\n");

	if (acsConfig.ambrOpts.NLmode != E_ARmode::OFF && !NLinactive)
	{
		nfix = NLambEstm(trace, kfState, ARcontr[E_AmbTyp::NL12]);
	}

	Netwrk_ARoutput(trace, stations, kfState.time, AR_Iono_meas, biasOutrate, D2R * acsConfig.ambrOpts.min_el_AR);

	return nfix;
}

/* Rover Ambiguity resolution */
int enduserAmbigResl( Trace& trace, ObsList& obsList, KFState& kfState, double tgap)
{

	if ( acsConfig.ambrOpts.WLmode == E_ARmode::OFF ) return 0;

	if ( obsList.size() <= 0 ) return 0;

	int trclvl = 4;
	ENDUSR_MODE = true;
	int nfix = 0;

	string rov = obsList.front().mount;
	tracepdeex(trclvl, trace, "\n#ARES_MAIN %s Ambiguity Resolution on for %s\n", kfState.time.to_string(0), rov);

	if (AR_initialization)
	{
		AR_initialization = false;
		tracepdeex(trclvl, trace, "#ARES_MAIN Initializing configuration\n");
		start_from_acsConfig( tgap );
	}

	for (auto& [sat, satamb] : satpiv )
		satamb.reset = false;

	for (auto& [sys, act] : sys_solve)
	{
		if (act == false)
		{
			continue;
		}

		auto& sysamb = StatAmbMap_list[sys];
		auto& staamb = sysamb[rov];

		for (auto it = staamb.SignList.begin(); it != staamb.SignList.end();  )
		{
			auto& sat    = it->first;
			auto& sigamb = it->second;
			sigamb.outage++;

			if (sigamb.outage > SMP2RESET)
			{
				it = staamb.SignList.erase(it);
				satpiv[sat].elev.erase(rov);
			}
			else
				it++;
		}

		staamb.reset = false;
	}

	Load_rawmeas(trace, obsList, kfState, tgap);
	updt_usr_pivot ( trace, acsConfig.ambrOpts.min_el_AR, rov );

	if (acsConfig.ambrOpts.WLmode != E_ARmode::OFF)
	{
		tracepdeex(trclvl, trace, "\n#ARES_MAIN estimating WL ambiguities\n");
		ARState arcnt = ARcontr[E_AmbTyp::WL12];
		arcnt.recv = rov;
		WLambEstm(trace, kfState.time, tgap, arcnt, false);
	}

	if (acsConfig.ambrOpts.NLmode != E_ARmode::OFF)
	{
		tracepdeex(trclvl, trace, "\n#ARES_MAIN estimating NL ambiguities\n");
		ARState arcnt = ARcontr[E_AmbTyp::NL12];
		arcnt.recv = rov;
		nfix = NLambEstm(trace, kfState, arcnt);
	}

	Netwrk_trace_out(trace, D2R * acsConfig.ambrOpts.min_el_AR, rov);

	return nfix;
}


bool ARsol_ready (void)
{
	/* more (bettter) criteria to be added here */

	if (!NLinactive) NumEpocNL++;
	else NumEpocNL = 0;

	return NumEpocNL > 5;
}
