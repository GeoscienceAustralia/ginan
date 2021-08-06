#include "GNSSambres.hpp"

int NPIVTRCLVL;

double MIN_ARS_ELE;
double MIN_PIV_ELE;
string ARrefsta;
map<SatSys, Satlpivt> satpiv;
map<E_Sys, map<string, StatAmbg>> StatAmbMap_list;
map<E_Sys, list<SatSys>> miscon_sat;
map<E_Sys, list<string>> miscon_sta;

map<string,map <E_Sys, SatSys>> pivotsats;
map<string,map <E_Sys, int>>    sysnfrq;

void init_usr_pivot( Trace& trace, E_Sys sys, double arelev, string rover)
{
	MIN_ARS_ELE = D2R * arelev;
	MIN_PIV_ELE = D2R * (arelev + 5.0);

	StatAmbg& stadat = StatAmbMap_list[sys][rover];

	tracepdeex(2, trace, "\n#ARES_PIV  Initialising pivot for %s, ncand=%d, rov=%s ", sys._to_string(), stadat.SignList.size(), rover);

	double elmx2 = 0.0, elmx3 = 0.0;
	SatSys piv2, piv3;

	for (auto& [sat, sigdat] : stadat.SignList)
	{
		if (sat.sys != sys)
			continue;

		if (sigdat.nfreq > 2 && sigdat.elev > elmx3)
		{
			piv3 = sat;
			elmx3 = sigdat.elev;
		}

		if (sigdat.nfreq >= 2 && sigdat.elev > elmx2)
		{
			piv2 = sat;
			elmx2 = sigdat.elev;
		}

		sigdat.pivot = false;
		sigdat.state = 0;
		satpiv[sat].npivot = 0;
		satpiv[sat].pivot_in = "NONE";
		satpiv[sat].reset = false;
		satpiv[sat].satbias.NL12 = 0.0;
		satpiv[sat].satbias.WL12 = 0.0;
		satpiv[sat].satbias.WL23 = 0.0;
		satpiv[sat].satbias.NL12var = 0.0;
		satpiv[sat].satbias.WL12var = 0.0;
		satpiv[sat].satbias.WL23var = 0.0;
		satpiv[sat].bias_out.clear();
	}

	if (elmx3 > MIN_PIV_ELE)
	{
		tracepdeex(2, trace, "... %s selected (3)", piv3.id().c_str());
		pivotsats[rover][sys] = piv3;
		sysnfrq[rover][sys] = 2;											/* <- not fully supporting triple frequency yet... */
		satpiv[piv3].npivot = 1;
		satpiv[piv3].pivot_in = rover;
		auto& sigdat = stadat.SignList[piv3];
		sigdat.pivot = true;
		sigdat.state = 7;
		sigdat.flt.NL12 = 1.0 * ROUND(sigdat.raw.NL12);
		sigdat.flt.WL12 = 1.0 * ROUND(sigdat.raw.WL12);
		sigdat.flt.WL23 = 1.0 * ROUND(sigdat.raw.WL23);
		sigdat.flt.NL12var = 0.0;
		sigdat.flt.WL12var = 0.0;
		sigdat.flt.WL23var = 0.0;
		sigdat.fix = sigdat.flt;
		stadat.stabias.NL12 = sigdat.raw.NL12 - sigdat.fix.NL12;
		stadat.stabias.WL12 = sigdat.raw.WL12 - sigdat.fix.WL12;
		stadat.stabias.WL23 = sigdat.raw.WL23 - sigdat.fix.WL23;
		stadat.stabias.NL12var = sigdat.raw.NL12var;
		stadat.stabias.WL12var = sigdat.raw.WL12var;
		stadat.stabias.WL23var = sigdat.raw.WL23var;
		stadat.stabias_fix = stadat.stabias;
	}
	else if (elmx2 > MIN_PIV_ELE)
	{
		tracepdeex(2, trace, "... %s selected (2)", piv2.id().c_str());
		pivotsats[rover][sys] = piv2;
		sysnfrq[rover][sys] = 2;
		satpiv[piv2].npivot = 1;
		satpiv[piv2].pivot_in = rover;
		satpiv[piv2].reset = false;
		auto& sigdat = stadat.SignList[piv2];
		sigdat.pivot = true;
		sigdat.state = 6;
		sigdat.flt.NL12 = 1.0 * ROUND(sigdat.raw.NL12);
		sigdat.flt.WL12 = 1.0 * ROUND(sigdat.raw.WL12);
		sigdat.flt.WL23 = 0.0;
		sigdat.flt.NL12var = 0.0;
		sigdat.flt.WL12var = 0.0;
		sigdat.flt.WL23var = -1.0;
		sigdat.fix = sigdat.flt;
		stadat.stabias.NL12 = sigdat.raw.NL12 - sigdat.fix.NL12;
		stadat.stabias.WL12 = sigdat.raw.WL12 - sigdat.fix.WL12;
		stadat.stabias.WL23 = 0.0;
		stadat.stabias.NL12var = sigdat.raw.NL12var;
		stadat.stabias.WL12var = sigdat.raw.WL12var;
		stadat.stabias.WL23var = -1.0;
		stadat.stabias_fix = stadat.stabias;
	}
	else
	{
		tracepdeex(2, trace, "... not found");
		sysnfrq[rover][sys] = 0;
	}

	stadat.reset = true;
}

void updt_usr_pivot ( Trace& trace, double arelev, string rover )
{
	ARrefsta = "UINIT";

	for (auto& [sys, act] : sys_solve)
	{
		if (act == false)
		{
			continue;
		}

		StatAmbg& stadat = StatAmbMap_list[sys][rover];

		bool init=false;
		if (pivotsats.find(rover) == pivotsats.end()) init=true;
		else if(pivotsats[rover].find(sys) == pivotsats[rover].end()) init=true;
		else if(pivotsats[rover][sys].prn == 0) init=true;

		if (init) init_usr_pivot( trace, sys, arelev, rover );
		else  /* update bias */
		{
			SatSys& pivt = pivotsats[rover][sys];
			int& sfrq = sysnfrq[rover][sys];

			SignAmbg& pivdat = stadat.SignList[pivt];
			string mess = "slip";

			if (pivdat.outage)
			{
				pivdat.state = 0;
				mess = "outage";
			}

			if (pivdat.elev < MIN_ARS_ELE)
			{
				pivdat.state = 0;
				mess = "elevation";
			}

			if ( pivdat.state < 6 || ( sfrq > 2 && pivdat.state < 7))
			{
				pivdat.pivot = false;
				int week;
				double tow = time2gpst(stadat.update, &week);
				tracepdeex(2, trace, "\n#ARES_PIV  %4d %6.0f Switching pivot for %s due to %s ", week, tow, sys._to_string(), mess);

				satpiv[pivt].npivot = 0;
				satpiv[pivt].pivot_in = "NONE";

				SatSys newpiv;
				SatSys newpiv2;
				double maxel	= 0;
				double maxel2	= 0;

				for (auto& [sat, sigdat] : stadat.SignList)
				{
					if (sat.sys != sys) 			continue;
					if (sigdat.elev < MIN_PIV_ELE) 	continue;
					if (sigdat.outage) 				continue;
					if (sigdat.state < 2) 			continue;

					if (sigdat.elev > maxel)
					{
						newpiv2 = sat;
						maxel2  = sigdat.elev;
					}

					if (sigdat.state < 6) 						continue;
					if (sfrq > 2 && pivdat.state < 7)	continue;

					if (sigdat.elev > maxel)
					{
						newpiv = sat;
						maxel  = sigdat.elev;
					}
				}

				if (maxel > MIN_PIV_ELE)
				{
					pivt = newpiv;
					stadat.SignList[newpiv].pivot = true;
					stadat.SignList[newpiv].state = 7;
					satpiv[newpiv].npivot = 1;
					satpiv[newpiv].pivot_in = rover;
					tracepdeex(2, trace, "... switched to %s ", newpiv.id().c_str());
				}
				else init_usr_pivot( trace, sys, arelev, rover);
			}
		}
	}
}


int reset_sat(Trace& trace, SatSys sat)
{
	satpiv[sat].reset = true;
	satpiv[sat].npivot = 0;
	auto& StatAmbMap = StatAmbMap_list[sat.sys];

	tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Resetting bias for satellite %s", sat.id().c_str());

	double maxel = 0.0, maxqlt = 0.0;
	string pivsta;

	for (auto& [stan, elev] : satpiv[sat].elev)
	{
		SignAmbg& sigamb = StatAmbMap[stan].SignList[sat];
		sigamb.state = 0;

		if (elev < MIN_PIV_ELE) 			continue;
		if (sigamb.nfreq < 2) 				continue;
		if (StatAmbMap[stan].npivot <= 0) 	continue;

		double qlt = elev + 20.0 - 1.0 * StatAmbMap[stan].dept;

		if (qlt > maxqlt)
		{
			pivsta = stan;
			maxel = elev;
			maxqlt = qlt;
			tracepdeex(NPIVTRCLVL, trace, " <- %s %.4f", stan, qlt);
		}
	}

	if (maxel > MIN_PIV_ELE)
	{
		satpiv[sat].npivot = 1;
		StatAmbMap[pivsta].npivot++;
		satpiv[sat].pivot_in = pivsta;
		satpiv[sat].dept = StatAmbMap[pivsta].dept + 1;

		SignAmbg& sigamb = StatAmbMap[pivsta].SignList[sat];
		ambMeas&  stabia = StatAmbMap[pivsta].stabias;

		sigamb.pivot = true;
		sigamb.state = 7;
		sigamb.flt.NL12 = 1.0 * ROUND(sigamb.raw.NL12 - stabia.NL12);
		sigamb.flt.WL12 = 1.0 * ROUND(sigamb.raw.WL12 - stabia.WL12);
		sigamb.flt.WL23 = 1.0 * ROUND(sigamb.raw.WL23 - stabia.WL23);
		sigamb.flt.NL12var = 0.0;
		sigamb.flt.WL12var = 0.0;
		sigamb.flt.WL23var = 0.0;
		sigamb.fix = sigamb.flt;

		satpiv[sat].satbias.NL12    = sigamb.raw.NL12 - stabia.NL12 - sigamb.fix.NL12;
		satpiv[sat].satbias.WL12    = sigamb.raw.WL12 - stabia.WL12 - sigamb.fix.WL12;
		satpiv[sat].satbias.WL23    = sigamb.raw.WL23 - stabia.WL23 - sigamb.fix.WL23;
		satpiv[sat].satbias.NL12var = sigamb.raw.NL12var + stabia.NL12var;
		satpiv[sat].satbias.WL12var = sigamb.raw.WL12var + stabia.WL12var;
		satpiv[sat].satbias.WL23var = sigamb.raw.WL23var + stabia.WL23var;

		tracepdeex(NPIVTRCLVL, trace, " <- %s %.4f", pivsta, maxqlt);
		return 1;
	}
	else
	{
		tracepdeex(NPIVTRCLVL, trace, " ... failed");
		return 0;
	}
}

int reset_sta(Trace& trace, string stat, E_Sys sys)
{
	auto& StatAmbMap = StatAmbMap_list[sys];
	StatAmbMap[stat].reset = true;
	StatAmbMap[stat].npivot = 0;

	tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Resetting %s bias for station %s ", sys._to_string(), stat);

	double maxel = 0.0, maxqlt = 0.0;
	SatSys pivsat;

	for (auto& [satl, sigamb] : StatAmbMap[stat].SignList)
	{
		sigamb.state = 0;

		if (sigamb.nfreq < 2) 			continue;
		if (sigamb.elev < MIN_PIV_ELE) 	continue;
		if (satpiv[satl].npivot <= 0) 	continue;

		double qlt = sigamb.elev + 20.0 - 1.0 * satpiv[satl].dept;

		if (qlt > maxqlt)
		{
			pivsat = satl;
			maxel  = sigamb.elev;
			maxqlt = qlt;
		}
	}

	if (maxel > MIN_PIV_ELE)
	{
		StatAmbMap[stat].npivot = 1;
		satpiv[pivsat].npivot++;
		StatAmbMap[stat].dept = satpiv[pivsat].dept + 1;

		SignAmbg& sigamb = StatAmbMap[stat].SignList[pivsat];
		ambMeas& satbia = satpiv[pivsat].satbias;

		sigamb.pivot = true;
		sigamb.state = 7;
		sigamb.flt.NL12 = 1.0 * ROUND(sigamb.raw.NL12 - satbia.NL12);
		sigamb.flt.WL12 = 1.0 * ROUND(sigamb.raw.WL12 - satbia.WL12);
		sigamb.flt.WL23 = 1.0 * ROUND(sigamb.raw.WL23 - satbia.WL23);
		sigamb.flt.NL12var = 0.0;
		sigamb.flt.WL12var = 0.0;
		sigamb.flt.WL23var = 0.0;
		sigamb.fix = sigamb.flt;

		StatAmbMap[stat].stabias.NL12 = sigamb.raw.NL12 - satbia.NL12 - sigamb.fix.NL12;
		StatAmbMap[stat].stabias.WL12 = sigamb.raw.WL12 - satbia.WL12 - sigamb.fix.WL12;
		StatAmbMap[stat].stabias.WL23 = sigamb.raw.WL23 - satbia.WL23 - sigamb.fix.WL23;
		StatAmbMap[stat].stabias.NL12var = sigamb.raw.NL12var + satbia.NL12var;
		StatAmbMap[stat].stabias.WL12var = sigamb.raw.WL12var + satbia.WL12var;
		StatAmbMap[stat].stabias.WL23var = sigamb.raw.WL23var + satbia.WL23var;

		tracepdeex(NPIVTRCLVL, trace, "<- %s", pivsat.id().c_str());

		return 1;
	}
	else
	{
		tracepdeex(NPIVTRCLVL, trace, " ... failed ");
		return 0;
	}
}

void check_satellite(Trace& trace, SatSys sat, int dept);

void check_station(Trace& trace, string sta, int dept, E_Sys sys)
{
	int nsat = 0;
	tracepdeex(NPIVTRCLVL, trace, "%s", sta);
	auto& StatAmbMap = StatAmbMap_list[sys];
	StatAmbMap[sta].npivot = 0;

	for (auto it = StatAmbMap[sta].SignList.begin(); it != StatAmbMap[sta].SignList.end();  )
	{
		auto& [sat, sigamb] = *it;

		if (sigamb.pivot)
		{
			StatAmbMap[sta].npivot++;
			StatAmbMap[sta].dept = dept;

			if (satpiv[sat].pivot_in == sta)
			{
				if (nsat)
				{
					tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV     ");

					for (int i = 0; i < dept; i++)
						trace << "        ";
				}

				if (sigamb.outage || sigamb.elev < MIN_ARS_ELE || sigamb.state < 7)
				{
					miscon_sat[sys].push_back(sat);
					sigamb.pivot = false;

					if 		(sigamb.outage) 	tracepdeex(NPIVTRCLVL, trace, " X- ");
					else if (sigamb.state < 7) 	tracepdeex(NPIVTRCLVL, trace, " *- ");
					else 						tracepdeex(NPIVTRCLVL, trace, " x- ");

					StatAmbMap[sta].npivot--;
				}
				else
					tracepdeex(NPIVTRCLVL, trace, " -- ");

				check_satellite(trace, sat, dept + 1);
				nsat++;
			}
		}

		if (sigamb.outage)
		{
			it = StatAmbMap[sta].SignList.erase(it);
			//if(satpiv[sat].elev.find(sta)!=satpiv[sat].elev.end()) satpiv[sat].elev.erase(sta);
		}
		else
			++it;
	}
}



void check_satellite(Trace& trace, SatSys sat, int dept)
{
	int nsta = 0;
	tracepdeex(NPIVTRCLVL, trace, "%s ", sat.id().c_str());
	satpiv[sat].npivot = 0;
	auto& StatAmbMap = StatAmbMap_list[sat.sys];

	for (auto it = satpiv[sat].elev.begin(); it != satpiv[sat].elev.end();  )
	{
		string sta = it->first;
		double ele = it->second;
		//if( StatAmbMap[sta].SignList.find(sat)==StatAmbMap[sta].SignList.end()) continue;
		SignAmbg& recamb = StatAmbMap[sta].SignList[sat];

		if (recamb.pivot)
		{
			satpiv[sat].npivot++;
			satpiv[sat].dept = dept;

			if (sta != satpiv[sat].pivot_in)
			{
				if (nsta)
				{
					tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV     ");

					for (int i = 0; i < dept; i++)
						tracepdeex(2, trace, "        ");
				}

				if (recamb.outage || ele < MIN_ARS_ELE || recamb.state < 7)
				{
					miscon_sta[sat.sys].push_back(sta);
					recamb.pivot = false;

					if 		(recamb.outage) 	tracepdeex(NPIVTRCLVL, trace, " X- ");
					else if (recamb.state < 7) 	tracepdeex(NPIVTRCLVL, trace, " *- ");
					else 						tracepdeex(NPIVTRCLVL, trace, " x- ");

					satpiv[sat].npivot--;
				}
				else
					tracepdeex(NPIVTRCLVL, trace, " -- ");

				check_station(trace, sta, dept + 1, sat.sys);
				nsta++;
			}
		}

		if (recamb.outage)
			it = satpiv[sat].elev.erase(it);
		else
			++it;
	}
}



string connect_sat(Trace& trace, string base, SatSys sat, double* maxel, double* maxqlt)
{
	double melev = 0.0, melev2 = 0.0;
	double mqlit = 0.0, mqlit2 = 0.0;
	string out = "NONE", out2 = "NONE";
	auto& StatAmbMap = StatAmbMap_list[sat.sys];

	for (auto& [satl, samb] : StatAmbMap[base].SignList)
	{

		if (samb.pivot && satpiv[satl].pivot_in == base) /* satellites in the pivot */
		{
			for (auto& [statn, elev] : satpiv[satl].elev)
			{
				if (statn == base)
					continue;

				if (!StatAmbMap[statn].SignList[satl].pivot)
					continue;

				double tmpel = 0.0, qlit = 0.0;
				string tmp = connect_sat(trace, statn, sat, &tmpel, &qlit);


				if (tmpel < MIN_PIV_ELE)
					continue;

				qlit -= 2.0;

				if (StatAmbMap[tmp].SignList[sat].state < 2)
					continue;

				if (qlit > mqlit)
				{
					mqlit = qlit;
					melev = tmpel;
					out   = tmp;
				}

				if (StatAmbMap[tmp].SignList[sat].state < 6)
					continue;

				if (qlit > mqlit2)
				{
					mqlit2 = qlit;
					melev2 = tmpel;
					out2   = tmp;
				}
			}

			continue;
		}

		if (sat != satl) 				continue;
		if ( samb.outage )				continue;
		if ( samb.nfreq < 2 )			continue;
		if ( samb.state < 2 )			continue;
		if ( samb.elev < MIN_PIV_ELE )	continue;


		double qlt = samb.elev + 20.0;

		if (qlt > mqlit)
		{
			mqlit = qlt;
			melev = samb.elev;
			out = base;
		}

		if ( samb.state < 6 )
			continue;

		if (qlt > mqlit2)
		{
			mqlit2 = qlt;
			melev2 = samb.elev;
			out2   = base;
		}
	}

	if (melev2 >= MIN_PIV_ELE)
	{
		maxel[0] = melev2;
		maxqlt[0] = mqlit2;
		return out2;
	}

	if (melev >= MIN_PIV_ELE)
	{
		maxel[0] = melev;
		maxqlt[0] = mqlit;
		return out;
	}

	maxel[0] = 0.0;
	maxqlt[0] = 0.0;
	return "NONE";
}

/* Trying to connect station "sta" to station base */
SatSys connect_sta(Trace& trace, string base, string sta, double* maxel, double* maxqlt, E_Sys sys)
{
	double melev = 0.0, melev2 = 0.0;
	double mqlit = 0.0, mqlit2 = 0.0;
	SatSys out, out2;
	auto& StatAmbMap = StatAmbMap_list[sys];

	for (auto& [satl, samb] : StatAmbMap[base].SignList) /* satellites in the pivot */
	{
		if (!samb.pivot)
			continue;

		if (satpiv[satl].pivot_in != base)
			continue;

		for (auto& [statn, elev] : satpiv[satl].elev)
		{
			SignAmbg& sigamb = StatAmbMap[statn].SignList[satl];

			if (sigamb.outage)
				continue;

			if (sigamb.nfreq < 2)
				continue;

			if (sta == statn)
			{
				//tracepdeex(2,trace,"  -->   cosidering %s %s\n",statn,satl.id().c_str());
				if (sigamb.state < 2)
					continue;

				double qlt = elev + 20.0;

				if (qlt > mqlit)
				{
					mqlit = qlt;
					melev = elev;
					out = satl;
				}

				if (sigamb.state < 6)
					continue;

				if (qlt > mqlit2)
				{
					mqlit2 = qlt;
					melev2 = elev;
					out2 = satl;
				}

				continue;
			}

			if (statn == base)
				continue;

			if (!sigamb.pivot)
				continue;

			double tmpel = 0.0, tmpqlt = 0.0;
			SatSys tmp = connect_sta(trace, statn, sta, &tmpel, &tmpqlt, sys);

			if (tmpel < MIN_PIV_ELE)
				continue;

			tmpqlt -= 2.0;

			SignAmbg& sigamb2 = StatAmbMap[sta].SignList[tmp];

			if (sigamb2.state < 2)
				continue;

			if ( tmpqlt > mqlit)
			{
				melev = tmpel;
				mqlit = tmpqlt;
				out  = tmp;
			}

			if (sigamb2.state < 6)
				continue;

			if ( tmpqlt > mqlit2)
			{
				melev2 = tmpel;
				mqlit2 = tmpqlt;
				out2  = tmp;
			}
		}
	}

	if (melev2 >= MIN_PIV_ELE)
	{
		maxel[0] = melev2;
		maxqlt[0] = mqlit2;
		return out2;
	}

	if (melev >= MIN_PIV_ELE)
	{
		maxel[0] = melev;
		maxqlt[0] = mqlit;
		return out;
	}

	maxel[0] = 0.0;
	maxqlt[0] = 0.0;
	return out;
}


/************************************************************************************************************************************************
* reconnect_pivot : Try to reconnect the satellites and stations in miscon_sat and miscon_sta to the pivot 									*
* 		- Try to connect each member of miscon_sat:																								*
*			- If a state 7 connection was found,   set this as the new pivot connection	( connect_sat )											*
*			- If a state 2-3 connection was found, reset the NL ambiguity																		*
*				- Ambiguity is ROUND( Raw - stabias )																							*
*				- Satellite bias is Raw - ambiguity -stabias																					*
*				- Add all stations behind sat to the miscon_sta;																				*
*		- Try to connect each member of miscon_sta:																								*
*			- If a state 7 connection was found,   set this as the new pivot connection  														*
*			- If a state 2-3 connection was found, reset the NL ambiguity																		*
*				- Ambiguity is ROUND( Raw - satbias )																							*
*				- Station bias is Raw - ambiguity -satbias																						*
*				- Add all satellites behind sta to the miscon_sat																				*
* Stop when no new satellite/station can be connected, return the number of satellites/stations in miscon_sat and miscon_sta
************************************************************************************************************************************************/
int reconnect_pivot(Trace& trace, E_Sys sys, bool wlonly)
{
	int nnew = 0;
	auto& StatAmbMap = StatAmbMap_list[sys];

	while (miscon_sat[sys].size() > 0 || miscon_sta[sys].size() > 0)
	{
		nnew = 0;

		for (auto itr = miscon_sat[sys].begin(); itr != miscon_sat[sys].end(); )
		{
			SatSys sat = *itr;
			tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Trying to reconnect sat %s ...", sat.id().c_str());
			double maxel = 0.0, maxql = 0.0;
			string sta = connect_sat(trace, ARrefsta, sat, &maxel, &maxql);
			SignAmbg& sigamb = StatAmbMap[sta].SignList[sat];

			bool found = true;

			if (!wlonly && sigamb.state < 6)
				found = false;

			if (!wlonly && sigamb.fix.NL12var < 0.0)
				found = false;

			if (maxel < MIN_PIV_ELE)
				found = false;

			if (found)
			{
				satpiv[sat].pivot_in = sta;
				satpiv[sat].npivot++;
				tracepdeex(NPIVTRCLVL, trace, " connected to %s %1d %.4f", sta, sigamb.state, maxql);
				sigamb.pivot = true;
				sigamb.state = 7;

				if (sigamb.fix.NL12var >= 0.0)
				{
					sigamb.flt.NL12 = ROUND(sigamb.fix.NL12);
				}
				else sigamb.flt.NL12 = ROUND(sigamb.flt.NL12);

				sigamb.fix.NL12 = sigamb.flt.NL12;
				sigamb.flt.NL12var = 0.0;
				sigamb.fix.NL12var = 0.0;
				itr = miscon_sat[sys].erase(itr);
				nnew++;
				break;
			}
			else
			{
				++itr;
				tracepdeex(NPIVTRCLVL, trace, " failed");
			}

		}

		for (auto it = miscon_sta[sys].begin(); it != miscon_sta[sys].end(); )
		{
			string sta = *it;
			tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Trying to reconnect sta %s %s...", sta, sys._to_string());
			double maxel = 0.0, maxql = 0.0;;
			SatSys sat = connect_sta(trace, ARrefsta, sta, &maxel, &maxql, sys);
			SignAmbg& sigamb = StatAmbMap[sta].SignList[sat];


			bool found = true;

			if (!wlonly && sigamb.state < 6)
				found = false;

			if (!wlonly && sigamb.fix.NL12var < 0.0)
				found = false;

			if (maxel < MIN_PIV_ELE) found = false;

			if (found)
			{
				tracepdeex(NPIVTRCLVL, trace, " connected to %s %1d %.4f", sat.id().c_str(), sigamb.state, maxql);
				sigamb.pivot = true;
				StatAmbMap[sta].npivot++;
				sigamb.state = 7;

				if (sigamb.fix.NL12var >= 0.0)
				{
					sigamb.flt.NL12 = ROUND(sigamb.fix.NL12);
				}
				else sigamb.flt.NL12 = ROUND(sigamb.flt.NL12);

				sigamb.fix.NL12 = sigamb.flt.NL12;
				sigamb.flt.NL12var = 0.0;
				sigamb.fix.NL12var = 0.0;
				it = miscon_sta[sys].erase(it);
				nnew++;
			}
			else
			{
				++it;
				tracepdeex(NPIVTRCLVL, trace, " failed");
			}
		}

		if (nnew == 0)
			return miscon_sta[sys].size() + miscon_sat[sys].size();
	}

	return 0;
}


/************************************************************************************************************************************************
* rese_net_NL : Resets the narrowlane ambiguities for a particular system, insat.sys, starting from the station sta 							*
*				 If "sta" is not "ARrefsta" the satellite insat should be the link between "sta" and the rest of the pivot						*
* 		- Station bias is 0 for "ARrefsta"																										*
*		- Sta-insat ambiguity is ROUND( Raw - satbias ) 																						*
*		- Station bias is Raw - ambiguity -satbias																								*
*		- For all satellites "sat" behind "sta":																								*
*			- Sat-sta ambiguity is ROUND( Raw - stabias )																						*
*			- Satellite bias is Raw - ambiguity -stabias																						*
*			- For all stations "sta2" behind "sat":																								*
*				- Call rese_net_NL with "sta2" and "sat" as inputs																				*
************************************************************************************************************************************************/
int rese_net_NL(Trace& trace, string rec, SatSys insat)
{

	int namb = 0;

	if (StatAmbMap_list[insat.sys].find(rec) == StatAmbMap_list[insat.sys].end())
		return 0;

	auto& stadat = StatAmbMap_list[insat.sys][rec];

	tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Initializing NL pivot for %s station %s  npiv:%d", insat.sys._to_string(), rec, stadat.npivot);

	if (stadat.npivot == 0)
		return 0;

	namb = 1;

	if (rec == ARrefsta)
	{
		tracepdeex(NPIVTRCLVL, trace, " ... anchor station");
		stadat.stabias.NL12 = 0.0;
		stadat.stabias.NL12var = 0.0;
	}
	else
	{
		tracepdeex(NPIVTRCLVL, trace, " ... connected to satellite %s", insat.id().c_str());
		double satbia = satpiv[insat].satbias.NL12;
		SignAmbg& sigamb       = stadat.SignList[insat];
		sigamb.flt.NL12        = 1.0 * ROUND(sigamb.raw.NL12 - satbia);
		sigamb.flt.NL12var     = 0.0;
		sigamb.fix.NL12        = sigamb.flt.NL12;
		sigamb.fix.NL12var     = 0.0;
		stadat.stabias.NL12    = sigamb.raw.NL12    - satbia - sigamb.fix.NL12;
		stadat.stabias.NL12var = sigamb.raw.NL12var + satpiv[insat].satbias.NL12var;
	}

	if (stadat.npivot == 0)
		return 1;

	double stabias    = stadat.stabias.NL12;
	double stabiasvar = stadat.stabias.NL12var;

	for (auto& [sat, sigamb] : stadat.SignList)
	{
		if (!sigamb.pivot)
			continue;

		if (satpiv[sat].pivot_in != rec)
			continue;

		tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Initializing NL pivot for satellite %s  ", sat.id().c_str());

		sigamb.flt.NL12    = 1.0 * ROUND(sigamb.raw.NL12 - stabias);
		sigamb.flt.NL12var = 0.0;
		sigamb.fix.NL12    = sigamb.flt.NL12;
		sigamb.fix.NL12var = 0.0;
		satpiv[sat].satbias.NL12    = sigamb.raw.NL12 - stabias - sigamb.fix.NL12;
		satpiv[sat].satbias.NL12var = sigamb.raw.NL12var + stabiasvar;

		if (satpiv[sat].npivot == 1)
			continue;

		for (auto& [rec2, elev] : satpiv[sat].elev )
		{
			if (rec == rec2)
				continue;

			if (StatAmbMap_list[insat.sys][rec2].SignList[sat].pivot)
				namb += rese_net_NL(trace, rec2, sat);
		}
	}

	return namb;
}


void init_net_pivot ( Trace& trace, double arelev, string defref )
{
	MIN_ARS_ELE = D2R * arelev;
	MIN_PIV_ELE = D2R * (arelev + 5.0);
	NPIVTRCLVL = 3;

	string tmp = "UNINIT";
	int nmx2 = 0, nmx3 = 0;

	for (auto& [sys, ambMap] : StatAmbMap_list)
	{
		if (sys_solve[sys] == false)
		{
			continue;
		}

		tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Initializing network pivot for %s  %d %d", sys._to_string(), ambMap.size(), satpiv.size());

		for (auto& [rec, stadat] : ambMap)
		{
			if (rec == defref)
				ARrefsta = rec;

			stadat.npivot = 0;
			stadat.reset = false;
			int nsat2 = 0, nsat3 = 0;

			for (auto& [sat, sigdat] : stadat.SignList)
			{
				if (sigdat.nfreq > 2 && sigdat.elev > MIN_PIV_ELE) 		nsat3++;
				if (sigdat.nfreq >= 2 && sigdat.elev > MIN_PIV_ELE)		nsat2++;

				sigdat.pivot = false;
				sigdat.state = 0;
				satpiv[sat].npivot = 0;
				satpiv[sat].pivot_in = "NONE";
				satpiv[sat].reset = false;
			}

			if (nsat3 > nmx3)
			{
				nmx3 = nsat3;
				tmp = rec;
			}

			if (nmx3 == 0 && nsat2 > nmx2)
			{
				nmx2 = nsat2;
				tmp = rec;
			}
		}

		if (ARrefsta == "UNINIT")
		{
			ARrefsta = tmp;
		}

		tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Using %s  as anchor %d", ARrefsta, ambMap[ARrefsta].SignList.size());
		ambMap[ARrefsta].anchor = true;
		ambMap[ARrefsta].stabias.NL12 = 0.0;
		ambMap[ARrefsta].stabias.WL12 = 0.0;
		ambMap[ARrefsta].stabias.WL23 = 0.0;
		ambMap[ARrefsta].stabias.NL12var = 0.0;
		ambMap[ARrefsta].stabias.WL12var = 0.0;
		ambMap[ARrefsta].stabias.WL23var = 0.0;
		ambMap[ARrefsta].npivot = 0;

		for (auto& [sat, amb] : ambMap[ARrefsta].SignList)
		{
			if (amb.elev > MIN_PIV_ELE)
			{
				amb.pivot = true;
				amb.state = 7;
				amb.flt.NL12 = 1.0 * ROUND(amb.raw.NL12);
				amb.flt.WL12 = 1.0 * ROUND(amb.raw.WL12);
				amb.flt.WL23 = 1.0 * ROUND(amb.raw.WL23);
				amb.flt.NL12var = 0;
				amb.flt.WL12var = 0;
				amb.flt.WL23var = 0;
				amb.fix = amb.flt;

				satpiv[sat].npivot = 1;
				satpiv[sat].pivot_in = ARrefsta;
				satpiv[sat].satbias.NL12 = amb.raw.NL12 - amb.fix.NL12;
				satpiv[sat].satbias.WL12 = amb.raw.WL12 - amb.fix.WL12;
				satpiv[sat].satbias.WL23 = amb.raw.WL23 - amb.fix.WL23;
				satpiv[sat].satbias.NL12var = amb.raw.NL12var;
				satpiv[sat].satbias.WL12var = amb.raw.WL12var;
				satpiv[sat].satbias.WL23var = amb.raw.WL23var;

				ambMap[ARrefsta].npivot++;

				tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Starting %2d %s -> %s  amb: %.0f, %.0f, %.0f", ambMap[ARrefsta].npivot, ARrefsta, sat.id().c_str(), amb.fix.NL12, amb.fix.WL12, amb.fix.WL23);
			}
		}

		int nleft = ambMap.size() - 1;
		int nnew;

		while (nleft > 0)
		{
			nnew = 0;

			for (auto& [id, staamb] : ambMap) 							/* connecting stations to the pivot */
			{
				if (staamb.npivot > 0)
					continue;								/* station already part of the pivot */

				SatSys pivt;
				double maxel = 0;

				for (auto& [sat, satamb] : staamb.SignList)
				{
					if (satamb.nfreq < 2)
						continue;							/* ToDo modify this to account for trple frequency */

					if (satpiv[sat].npivot > 0 && maxel < satamb.elev)
					{
						pivt = sat;
						maxel = satamb.elev;
					}
				}

				if (maxel > MIN_PIV_ELE)
				{
					SignAmbg& sigamb = staamb.SignList[pivt];
					staamb.npivot   = 1;
					ambMeas& satbia = satpiv[pivt].satbias;
					satpiv[pivt].npivot++;

					sigamb.pivot = true;
					sigamb.state = 7;
					sigamb.flt.NL12 = 1.0 * ROUND(sigamb.raw.NL12 - satbia.NL12);
					sigamb.flt.WL12 = 1.0 * ROUND(sigamb.raw.WL12 - satbia.WL12);
					sigamb.flt.WL23 = 1.0 * ROUND(sigamb.raw.WL23 - satbia.WL23);
					sigamb.flt.NL12var = 0;
					sigamb.flt.WL12var = 0;
					sigamb.flt.WL23var = 0;
					sigamb.fix = sigamb.flt;

					staamb.stabias.NL12 = sigamb.raw.NL12 - satbia.NL12 - sigamb.fix.NL12;
					staamb.stabias.WL12 = sigamb.raw.WL12 - satbia.WL12 - sigamb.fix.WL12;
					staamb.stabias.WL23 = sigamb.raw.WL23 - satbia.WL23 - sigamb.fix.WL23;
					staamb.stabias.NL12var = sigamb.raw.NL12var + satbia.NL12var;
					staamb.stabias.WL12var = sigamb.raw.WL12var + satbia.WL12var;
					staamb.stabias.WL23var = sigamb.raw.WL23var + satbia.WL23var;

					nleft--;
					nnew++;
					tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Starting %2d %s  -> %s amb: %.0f, %.0f, %.0f", satpiv[pivt].npivot, pivt.id().c_str(), id, sigamb.fix.NL12, sigamb.fix.WL12, sigamb.fix.WL23);
				}
			}

			if (nnew == 0)
			{
				tracepde(2, trace, "#ARES_PIV  Warning some stations left out of the network pivot\n");
				break;
			}

			for (auto& [sat, satamb] : satpiv) 								/* connecting satellites to the pivot */
			{
				if (satamb.npivot > 0)
					continue;								/* satellite is already part of the pivot */

				string pvt;
				double maxel = 0;

				for (auto& [staid, elv] : satamb.elev)
				{
					if (ambMap[staid].SignList[sat].nfreq < 2)
						continue;

					if (ambMap[staid].npivot > 0 && maxel < elv)
					{
						pvt = staid;
						maxel = elv;
					}
				}

				if (maxel > MIN_PIV_ELE)
				{
					SignAmbg& sigamb = ambMap[pvt].SignList[sat];
					ambMeas& stabia = ambMap[pvt].stabias;
					ambMap[pvt].npivot++;

					sigamb.pivot = true;
					sigamb.state = 7;
					sigamb.flt.NL12 = 1.0 * ROUND(sigamb.raw.NL12 - stabia.NL12);
					sigamb.flt.WL12 = 1.0 * ROUND(sigamb.raw.WL12 - stabia.WL12);
					sigamb.flt.WL23 = 1.0 * ROUND(sigamb.raw.WL23 - stabia.WL23);
					sigamb.flt.NL12var = 0.0;
					sigamb.flt.WL12var = 0.0;
					sigamb.flt.WL23var = 0.0;
					sigamb.fix = sigamb.flt;

					satamb.npivot = 1;
					satamb.pivot_in = pvt;
					satamb.satbias.NL12 = sigamb.raw.NL12 - stabia.NL12 - sigamb.fix.NL12;
					satamb.satbias.WL12 = sigamb.raw.WL12 - stabia.WL12 - sigamb.fix.WL12;
					satamb.satbias.WL23 = sigamb.raw.WL23 - stabia.WL23 - sigamb.fix.WL23;
					satamb.satbias.NL12var = sigamb.raw.NL12var + stabia.NL12var;
					satamb.satbias.WL12var = sigamb.raw.WL12var + stabia.WL12var;
					satamb.satbias.WL23var = sigamb.raw.WL23var + stabia.WL23var;


					tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Starting %2d %s -> %s  amb: %.0f, %.0f, %.0f", ambMap[pvt].npivot, pvt, sat.id().c_str(), sigamb.fix.NL12, sigamb.fix.WL12, sigamb.fix.WL23);
				}
			}
		}

		tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV ");
		check_station(trace, ARrefsta, 0, sys);
	}
}


/************************************************************************************************************************************************
* Updates the network pivot:																													*
*		- Resets satellites/stations with npivot=0 or reset=true ( reset_sta, reset_sat )														*
*		- Checks the current state of network pivot ( check_station )																			*
*		- if miscon_sat or miscon_sta is set, try to reconnect the pivot ( reconnect_pivot ) +													*
*		- if pivot cannot be fully restored:																									*
*			- fully disconnect a station, add satellites behind the station to miscon_sat, repeat from +										*
*			- if there are no satellites could be added, fully disconnect a satellite, add stations behind it to  miscon_sta, repeat from +		*
*			- if no new satellite or station could be added break																				*
*		- reset satellites in miscon_sat and stations in miscon_sta ( reset_sta, reset_sat )													*
*		- remove satellite/stations that could not be reset from the pivot/solution ( set .reset = true )										*
************************************************************************************************************************************************/
void updt_net_pivot ( Trace& trace, bool wlonly )
{

	for (auto& [sys, StatAmbMap] : StatAmbMap_list)
	{
		if (sys_solve[sys] == false)
		{
			continue;
		}
		tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Updating network pivot for %s %d", sys._to_string(), StatAmbMap.size());
		miscon_sta[sys].clear();
		miscon_sat[sys].clear();

		for (auto it = StatAmbMap.begin();  it != StatAmbMap.end(); )
		{
			string sta = it->first;
			auto& samb = it->second;

			if (samb.reset || samb.npivot == 0)
			{
				if (!reset_sta(trace, sta, sys))
				{
					tracepdeex(2, trace, "\n#ARES_PIV WARNING: Cannot include %s in pivot for %s", sta, sys._to_string());
					it = StatAmbMap.erase(it);
				}
				else
					it++;
			}
			else
				it++;
		}

		for (auto it = satpiv.begin();  it != satpiv.end(); )
		{
			SatSys sat = it->first;
			auto& samb = it->second;

			if (sat.sys == sys && (samb.reset || samb.npivot == 0))
			{
				if (!reset_sat(trace, sat))
				{
					tracepdeex(2, trace, "\n#ARES_PIV WARNING: Cannot include %s in pivot", sat.id().c_str());
					it = satpiv.erase(it);
				}
				else
					it++;
			}
			else
				it++;
		}

		GTime gtime = StatAmbMap[ARrefsta].update;


		tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV %s Pivot state at %s", sys._to_string(), gtime.to_string(0));
		tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV ");
		check_station(trace, ARrefsta, 0, sys);

		int nnew = 0;

		while (miscon_sat[sys].size() > 0 || miscon_sta[sys].size() > 0)
		{
			if (!reconnect_pivot(trace, sys, wlonly))
				break;

			nnew = 0;

			for (string sta : miscon_sta[sys] )
			{
				tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Disconnecting %s from pivot", sta);
				StatAmbMap[sta].npivot = 0;

				for (auto& [satl, sigamb] : StatAmbMap[sta].SignList)
				{
					if (sigamb.pivot)
					{
						if (satpiv[satl].pivot_in == sta)
						{
							tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Disconnecting %s - %s ", sta, satl.id().c_str());
							miscon_sat[sys].push_back(satl);
							nnew++;											/* new satellites to reconnect */
							satpiv[satl].pivot_in = "NONE";
							sigamb.pivot = false;
							satpiv[satl].npivot--;
							break;
						}
					}
				}

				if (nnew)
					break;
			}

			if (nnew)
				continue;

			for (SatSys sat : miscon_sat[sys] )
			{
				satpiv[sat].npivot = 0;

				for (auto& [stan, ele] : satpiv[sat].elev)
				{
					SignAmbg& sigamb = StatAmbMap[stan].SignList[sat];

					if (sigamb.pivot)
					{
						if (satpiv[sat].pivot_in != stan)
						{
							tracepdeex(NPIVTRCLVL, trace, "\n#ARES_PIV Disconnecting %s - %s ", sat.id().c_str(), stan);
							miscon_sta[sys].push_back(stan);
							nnew++;											/* new stations to reconnect */
							sigamb.pivot = false;
							StatAmbMap[stan].npivot--;
							break;
						}
					}
				}

				if (nnew)
					break;
			}

			if (!nnew)
				break;
		}

		while (miscon_sat[sys].size() > 0 || miscon_sta[sys].size() > 0)
		{
			/* At this point, all stations satellites should be individually isolated from the pivot,
			all its left is to reset each element */
			nnew = 0;

			for (auto itr = miscon_sat[sys].begin(); itr != miscon_sat[sys].end(); )
			{
				SatSys sat = *itr;
				satpiv[sat].npivot = 0;

				if (reset_sat(trace, sat))
				{
					itr = miscon_sat[sys].erase(itr);
					nnew++;
				}
				else
					++itr;
			}

			for (auto it = miscon_sta[sys].begin(); it != miscon_sta[sys].end(); )
			{
				string sta = *it;
				StatAmbMap[sta].npivot = 0;

				if (reset_sta(trace, sta, sys))
				{
					it = miscon_sta[sys].erase(it);
					nnew++;
				}
				else
					++it;
			}

			if (nnew == 0)
				break;
		}

		for (auto& sat : miscon_sat[sys])
		{
			tracepdeex(2, trace, "\n#ARES_PIV WARNING satellite %s could not be reconnected nor resetted, needs to be removed from AR solutions ", sat.id().c_str());
			satpiv[sat].elev.clear();

			for (auto& [rec, samb] : StatAmbMap )
				samb.SignList.erase(sat);

			/* procedures for satellite outages needs to be put here */
		}

		for (auto& rec : miscon_sta[sys])
		{
			tracepdeex(2, trace, "\n#ARES_PIV WARNING station  %s could not be reconnected nor resetted, needs to be removed from %s solutions ", rec, sys._to_string());
			StatAmbMap[rec].SignList.clear();
			StatAmbMap[rec].npivot = 0;

			for (auto& [sat, samb] : satpiv )
			if (sat.sys == sys)
			{
				samb.elev.erase(rec);
			}
		}

		miscon_sat[sys].clear();
		miscon_sta[sys].clear();
	}

	return;
}
