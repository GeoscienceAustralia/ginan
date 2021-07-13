#include "GNSSambres.hpp"
#include "biasSINEX.hpp"

map<KFKey, Amb_Sec>	AmbSecMap;	/* Ambiguity Section Map */
map<KFKey, short>	AmbSecInd;	/* Current index for Ambiguity Section Map */

void start_new_sect(Trace& trace, SatSys sat, string sta, GTime time)
{
	tracepdeex(3, trace, "\n%s, Starting new section for %s - %s ", time.to_string(0), sat.id().c_str(), sta);
	Amb_Sec& sect = StatAmbMap_list[sat.sys][sta].SignList[sat].curr_sect;

	KFKey indxKey = { KF::AMBIGUITY, sat, sta, 0};

	if (AmbSecInd.find(indxKey) == AmbSecInd.end() || AmbSecInd[indxKey] == 0)
	{
		AmbSecInd[indxKey] = 1;
		sect.sec_num       = 1;
		sect.sec_fin = time;
		sect.sec_ini = time;
		sect.sec_am1 = -99999;
		sect.sec_am2 = -99999;
		sect.sec_am3 = -99999;
		return;
	}

	if (sect.sec_num == 0)
	{
		sect.sec_num = AmbSecInd[indxKey];
		sect.sec_fin = time;
		sect.sec_ini = time;
		sect.sec_am1 = -99999;
		sect.sec_am2 = -99999;
		sect.sec_am3 = -99999;
		return;
	}

	if (    sect.sec_num > 0
			&&  timediff(sect.sec_fin, sect.sec_ini) > 600.0
			&& (  sect.sec_am1 > -99999
				|| sect.sec_am2 > -99999) )
	{
		KFKey writKey = { KF::AMBIGUITY, sat, sta, sect.sec_num};
		AmbSecMap[writKey] = sect;
		AmbSecInd[indxKey] = sect.sec_num + 1;
		tracepdeex(3, trace, "... saving data %.4f, %4f", sect.sec_am1, sect.sec_am2);
	}

	sect.sec_num = AmbSecInd[indxKey];
	sect.sec_fin = time;
	sect.sec_ini = time;
	sect.sec_am1 = -99999;
	sect.sec_am2 = -99999;
	sect.sec_am3 = -99999;
}

/* Print ambigities and residuals on a trace file */
void Netwrk_trace_out(Trace& trace, double arelev, string recv)
{
	int trclvl = 3;
	int week;
	double tow = 0;
	/* output variables */
	map <SatSys, int> nValAmb;
	map <SatSys, int> nFixWL;
	map <SatSys, int> nFixNL;
	map <SatSys, int> nPivot;

	for (auto& [sat, satamb] : satpiv)
	{
		nValAmb[sat] = 0;
		nFixWL[sat] = 0;
		nFixNL[sat] = 0;
		nPivot[sat] = 0;
	}

	for (auto& [sys, act] : sys_solve)
	{
		if (act == false) continue;

		if (StatAmbMap_list.find(sys) == StatAmbMap_list.end()) continue;

		for ( auto& [rec, staamb] : StatAmbMap_list[sys] )
		{

			if (staamb.SignList.size() == 0) continue;

			if (!recv.empty() && rec != recv) continue;

			tow = time2gpst(staamb.update, &week);

			double staWLflt = staamb.stabias.WL12;
			double staNLflt = staamb.stabias.NL12;
			double staWLfix = staamb.stabias_fix.WL12;
			double staNLfix = staamb.stabias_fix.NL12;

			int stanamb = 0;
			int stannlf = 0;
			int stanwlf = 0;
			int stanpiv = 0;

			for ( auto& [sat, sigamb] : staamb.SignList )
			{
				if (sigamb.outage) 			continue;

				if (sigamb.elev < arelev) 	continue;

				if (sigamb.nfreq < 2) 		continue;

				int pivt;

				if (!sigamb.pivot) 							pivt = 0;
				else if (satpiv[sat].pivot_in == rec) 		pivt = 2;
				else 										pivt = 1;

				if (sigamb.nepc > 3)
				{
					nValAmb[sat]++;
					stanamb++;
				}

				if (pivt > 0)
				{
					nPivot[sat]++;
					stanpiv++;

					if (sigamb.state < 7) tracepdeex(1, trace, "\n unfixed pivot? %s %s", rec, sat.id().c_str());
				}

				double rawWLmea = sigamb.raw.WL12;
				double ambWLflt = sigamb.flt.WL12;
				double ambWLfix = sigamb.fix.WL12;
				double satWLflt = satpiv[sat].satbias.WL12;
				double satWLfix = satpiv[sat].satbias_fix.WL12;

				double rawNLmea = sigamb.raw.NL12;
				double ambNLflt = sigamb.flt.NL12;
				double ambNLfix = sigamb.fix.NL12;
				double satNLflt = satpiv[sat].satbias.NL12;
				double satNLfix = satpiv[sat].satbias_fix.NL12;

				if (sigamb.state >= 6)
				{
					nFixNL[sat]++;
					stannlf++;
				}

				if (sigamb.state >= 2)
				{
					nFixWL[sat]++;
					stanwlf++;
				}

				double NLref = sigamb.ref[0];
				double WLref = sigamb.ref[1];

				Amb_Sec& sect = sigamb.curr_sect;

				if (sigamb.state > 0)
				{
					if (sigamb.state & 4)
					{
						if (sect.sec_am1 == -99999.0) sect.sec_am1 = ambNLfix;
						else if (sect.sec_am1 != ambNLfix)
						{
							tracepdeex(2, trace, "\n#ARES_SEC WARNING: Inconsisten NL for section %s %s at %s, %d -> %d",
									rec, sat.id().c_str(), staamb.update.to_string(0), sect.sec_am1, ambNLfix);
							sect.sec_am1 = ambNLfix;
						}
					}

					if (sigamb.state & 2)
					{
						if (sect.sec_am2 == -99999.0) sect.sec_am2 = ambWLfix;
						else if (sect.sec_am2 != ambWLfix)
						{
							tracepdeex(2, trace, "\n#ARES_SEC WARNING: Inconsisten WL for section %s %s at %s, %d -> %d",
									rec, sat.id().c_str(), staamb.update.to_string(0), sect.sec_am2, ambWLfix);
							sect.sec_am2 = ambWLfix;
						}
					}
				}

				sect.sec_fin = staamb.update;

				tracepdeex(5, trace, "\n#ARES_SEC Current section %s %s, ini: %s, fin: %s, %4d, %4d, %4d",
						rec, sat.id(), sect.sec_ini.to_string(0), sect.sec_fin.to_string(0), sect.sec_am1, sect.sec_am2, sect.sec_am3);

				tracepdeex(trclvl, trace, "\n#ARES_SIG, %4d, %6.0f, %s, %s, %1d, %1d, %5d, %5.2f,    %11.4f, %11.4f,%11.4f,%11.4f, %11.4f,%11.4f,%11.4f,    %11.4f, %11.4f,%11.4f,%11.4f, %11.4f,%11.4f,%11.4f",
						week, tow, rec, sat.id().c_str(), pivt, sigamb.state, sigamb.nepc, sigamb.elev * R2D,
						rawWLmea, ambWLflt, satWLflt, staWLflt, ambWLfix, satWLfix, staWLfix,
						rawNLmea, ambNLflt, satNLflt, staNLflt, ambNLfix, satNLfix, staNLfix);
			}

			if (!recv.empty())
			{
				tracepdeex(2, trace, "\n#ARES_STA, %4d, %6.0f, %s, %s, %3d, %11.4f, %11.4f, %11.4f, %11.4f, %2d, %2d, %2d, %2d",
						week, tow, rec, sys._to_string(), staamb.npivot, staWLflt, staWLfix, staNLflt, staNLfix, stanamb, stannlf, stanwlf, stanpiv);
			}
			else tracepdeex(trclvl, trace, "\n#ARES_STA, %4d, %6.0f, %s, %s, %3d, %11.4f, %11.4f, %11.4f, %11.4f, %2d, %2d, %2d, %2d",
								week, tow, rec, sys._to_string(), staamb.npivot, staWLflt, staWLfix, staNLflt, staNLfix, stanamb, stannlf, stanwlf, stanpiv);


		}
	}

	if (!recv.empty())  return;

	int ntotVal = 0;
	int ntotWLf = 0;
	int ntotNLf = 0;
	int ntotPiv = 0;

	for ( auto& [sat, satamb] : satpiv )
	{
		double pos[3] = {0};
		ecef2pos(satamb.rSat, pos);

		double WLflt = satamb.satbias.WL12var 		< 0 ? 0 : satamb.satbias.WL12;
		double NLflt = satamb.satbias.NL12var 		< 0 ? 0 : satamb.satbias.NL12;
		double WLfix = satamb.satbias_fix.WL12var 	< 0 ? 0 : satamb.satbias_fix.WL12;
		double NLfix = satamb.satbias_fix.NL12var 	< 0 ? 0 : satamb.satbias_fix.NL12;

		tracepdeex(trclvl, trace, "\n#ARES_SAT, %4d, %6.0f, %s, %7.2f, %7.2f, %3d, %3d, %3d, %3d, %11.4f, %11.4f, %11.4f, %11.4f",
				week, tow, sat.id().c_str(), pos[0]*R2D, pos[1]*R2D, nValAmb[sat], nFixWL[sat], nFixNL[sat], nPivot[sat], WLflt, WLfix, NLflt, NLfix);

		ntotVal += nValAmb[sat];
		ntotWLf += nFixWL[sat];
		ntotNLf += nFixNL[sat];
		ntotPiv += nPivot[sat];
	}

	tracepdeex(2, trace, "\n#ARES_ALL, %4d, %6.0f, %d, %d, %d, %d, %d\n",
			week, tow, ntotVal, ntotWLf, ntotNLf, ntotPiv, satpiv.size());
}

int net_sect_out ( Trace& trace )
{
	int nsec = 0;

	for (auto& [sys, act] : sys_solve)
		for (auto& [rec, staamb] : StatAmbMap_list[sys] )
			for (auto& [sat, sigamb] : staamb.SignList)
			{
				if (act == false)
				{
					continue;
				}

				/* saving ambiguity section data, it could be used for fix and hold...*/
				Amb_Sec& sect = sigamb.curr_sect;

				if (timediff(sect.sec_fin, sect.sec_ini) > 600.0 && (sect.sec_am1 > -99999 || sect.sec_am2 > -99999))
				{
					KFKey sectKey = { KF::AMBIGUITY, sat, rec, sect.sec_num};
					AmbSecMap[sectKey] = sect;
				}
			}

	for (auto& [key, sect] : AmbSecMap)
	{
		tracepdeex(2, trace, "\n#ARES_SEC, %s, %s, %4d, %s, %s, %6.0f, %6.0f, %6.0f\n",
				key.str, key.Sat.id().c_str(), key.num,
				sect.sec_ini.to_string(0), sect.sec_fin.to_string(0),
				sect.sec_am1, sect.sec_am2, sect.sec_am3);
		nsec++;
	}

	return 0;
}

int Netwrk_iono_out ( Trace& trace, StationList& stations)
{
	for (auto& rec_ptr : stations)
	{
		auto& rec = *rec_ptr;

		for (auto& obs : rec.obsList)
		{
			if (obs.ionExclude) 																										continue;

			if (!sys_solve[obs.Sat.sys]) 																								continue;

			if (StatAmbMap_list[obs.Sat.sys].find(rec.id) == StatAmbMap_list[obs.Sat.sys].end()) 										continue;

			if (StatAmbMap_list[obs.Sat.sys][rec.id].SignList.find(obs.Sat) == StatAmbMap_list[obs.Sat.sys][rec.id].SignList.end()) 	continue;

			SignAmbg& sigamb = StatAmbMap_list[obs.Sat.sys][rec.id].SignList[obs.Sat];

			if (sigamb.state < 6)
			{
				obs.ionExclude = 1;
				continue;
			}

			double lam1 = 0;
			double lam2 = 0;
			E_FType f1 = F1;
			E_FType f2 = F2;

			switch (obs.Sat.sys)
			{
				case E_Sys::QZS:
				case E_Sys::GPS:
					lam1 = CLIGHT / FREQ1;
					lam2 = CLIGHT / FREQ2;
					break;

				case E_Sys::GAL:
					lam1 = CLIGHT / FREQ1;
					lam2 = CLIGHT / FREQ5;
					f2 = F5;
					break;

				case E_Sys::CMP:
					lam1 = CLIGHT / FREQ1_CMP;
					lam2 = CLIGHT / FREQ2_CMP;
					break;
			}

			if (lam1 == 0)
				continue;

			S_LC lc		= getLC(obs, obs.satStat_ptr->lc_new, f1, f2);
			double WL_amb = sigamb.fix.WL12;
			double NL_amb = sigamb.fix.NL12;
			double WL_bia = satpiv[obs.Sat].satbias_fix.WL12;
			double NL_bia = satpiv[obs.Sat].satbias_fix.NL12;
			double WL_var = satpiv[obs.Sat].satbias_fix.WL12var;
			double NL_var = satpiv[obs.Sat].satbias_fix.NL12var;
			double GF_phs = -lc.GF_Phas_m;
			double varL = 2 * obs.Sigs.begin()->second.phasVar;

			double c1 = lam1 * lam1 / (lam2 * lam2 - lam1 * lam1);
			obs.STECtype = 2;
			//obs.STECsmth = c1*(GF_phs + (lam1-lam2)*NL_amb + lam2*WL_amb);
			//obs.STECsmvr = c1*c1*varL;
			obs.STECsmth = c1 * (GF_phs + (lam1 - lam2) * (NL_amb + NL_bia) - lam2 * WL_amb + (lam1 + lam2) * WL_bia);
			obs.STECsmvr = c1 * c1 * (varL + (lam1 - lam2) * (lam1 - lam2) * NL_bia + (lam1 + lam2) * (lam1 + lam2) * WL_bia);
		}
	}

	return 0;
}

double update_out_bias(SatSys sat, E_AmbTyp ambtyp, double rawbia)
{

	auto& satamb = satpiv[sat];

	if (satamb.bias_out.find(ambtyp) == satamb.bias_out.end())
	{
		satamb.bias_out[ambtyp].numsamp = 0;
		satamb.bias_out[ambtyp].intlevl = 0;
		satamb.bias_out[ambtyp].outbias = 0.0;
	}

	satamb.bias_out[ambtyp].rawbias = rawbia;

	double dbia = rawbia + 1.0 * satamb.bias_out[ambtyp].intlevl - satamb.bias_out[ambtyp].outbias;

	if (fabs(dbia) > 0.5)
	{
		satamb.bias_out[ambtyp].intlevl -= ROUND(dbia);
		dbia -= ROUND(dbia);
	}

	if (satamb.bias_out[ambtyp].numsamp < 10)
		satamb.bias_out[ambtyp].numsamp++;

	satamb.bias_out[ambtyp].outbias += dbia / satamb.bias_out[ambtyp].numsamp;

	return satamb.bias_out[ambtyp].outbias;
}

int Satelt_bias_out(
	Trace& trace,
	GTime time,
	double tupdt,
	double arelev)
{
	int nbia = 0;

	for (auto& [sat, satamb] : satpiv)
	{
		int nval = 0;
		int nfix = 0;

		if (sys_solve[sat.sys] == false)
			continue;

		if (satamb.reset)
			satamb.bias_out.clear();

		for (auto& [rec, elev] : satamb.elev)
		{
			if (elev < arelev)
				continue;

			nval++;
			SignAmbg& sigamb = StatAmbMap_list[sat.sys][rec].SignList[sat];

			if (sigamb.state >= 6)
				nfix++;

			/*to do, Ken: consider including a residual check here */
		}

		if (satamb.satbias_fix.NL12var < 0)
			continue;

		if (nfix > (0.4 * nval) && nfix > 3)
		{

			double NLbias = update_out_bias(sat, E_AmbTyp::NL12, satamb.satbias_fix.NL12);
			double WLbias = update_out_bias(sat, E_AmbTyp::WL12, satamb.satbias_fix.WL12);

			E_ObsCode def1, def2;

			switch (sat.sys)
			{
				case E_Sys::GPS:
					def1 = E_ObsCode::L1C;
					def2 = E_ObsCode::L2W;
					break;

				case E_Sys::GAL:
					def1 = E_ObsCode::L1C;
					def2 = E_ObsCode::L5Q;
					break;

				case E_Sys::CMP:
					def1 = E_ObsCode::L2I;
					def2 = E_ObsCode::L7I;
					break;

				case E_Sys::QZS:
					def1 = E_ObsCode::L1C;
					def2 = E_ObsCode::L2L;
					break;
			}

			double lam1 = lambdas[def1];
			double lam2 = lambdas[def2];

			if (lam1 == 0)
				continue;

			tracepdeex(4, trace, "\n NL/WL %s Bias entry: %.4f %.4f lam= %.3f %.3f %3d %3d", sat.id().c_str(), NLbias, WLbias, lam1, lam2, nfix, nval);

			double c1		= lam1 / (lam2 - lam1);
			double c2		= lam2 / (lam2 - lam1);
			double L1bias	= lam1 * (NLbias - c1 * WLbias);
			double L2bias	= lam2 * (NLbias - c2 * WLbias);
			double L1var	= lam1 * lam1 * (satamb.satbias_fix.NL12var + c1 * c1 * satamb.satbias_fix.WL12var);
			double L2var	= lam2 * lam2 * (satamb.satbias_fix.NL12var + c2 * c2 * satamb.satbias_fix.WL12var);
			outp_bias(trace, time, E_BiasType::OSB, "", sat, def1, E_ObsCode::NONE, L1bias, L1var, tupdt, PHAS);
			outp_bias(trace, time, E_BiasType::OSB, "", sat, def2, E_ObsCode::NONE, L2bias, L2var, tupdt, PHAS);

			satamb.bias_out[E_AmbTyp::UCL1].outbias = L1bias;
			satamb.bias_out[E_AmbTyp::UCL1].outvari = L1var;
			satamb.bias_out[E_AmbTyp::UCL2].outbias = L2bias;
			satamb.bias_out[E_AmbTyp::UCL2].outvari = L2var;

			if (acsConfig.ssrOpts.calculate_ssr)
			{
				nav.satNavMap[sat].ssrOut.ssrPhasBias.canExport		= false;
				nav.satNavMap[sat].ssrOut.ssrPhasBias.bias	[def1]	= L1bias;
				nav.satNavMap[sat].ssrOut.ssrPhasBias.var	[def1]	= L1var;
				nav.satNavMap[sat].ssrOut.ssrPhasBias.bias	[def2]	= L2bias;
				nav.satNavMap[sat].ssrOut.ssrPhasBias.var	[def2]	= L2var;
				nav.satNavMap[sat].ssrOut.ssrPhasBias.isSet			= true;
			}

			nbia++;
		}
	}

	return nbia;
}

void Netwrk_ARoutput(
	Trace&			trace,
	StationList&	stations,
	GTime			time,
	bool			ionout,
	double			biaupdt,
	double			arelev)
{
	if (ionout)
		Netwrk_iono_out(trace, stations);

	if (biaupdt > 0)
		Satelt_bias_out(trace, time, biaupdt, arelev);

	Netwrk_trace_out(trace, arelev, "");
}
