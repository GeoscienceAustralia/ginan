#include "GNSSambres.hpp"

map<E_Sys, string>					AR_reflist;
map<E_AmbTyp,map<E_Sys,ARsatpivts>>	SATpivlist;
map<E_AmbTyp,map<E_Sys,ARrecpivts>>	RECpivlist;

map<string,int> disc_rec;
map<SatSys,int> disc_sat;
map<string,int> aval_rec;
map<SatSys,int> aval_sat;
double MIN_PIV_ELE = 20*D2R;
	
/** Add ambiguity to pivot tree */
void   Link_amb(
	E_AmbTyp typ, 			///< Ambiguity type
	SatSys sat, 			///< Satellite
	string rec, 			///< Receiver
	bool satp,				///< Link direction indicator, true = rec -> sat link
	int amb, 				///< Ambiguity value
	GTime time, 			///< Solution time
	bool enduser = false)	///< End user solution
{
	GinAR_piv& satpiv = SATpivlist[typ][sat.sys][sat];
	GinAR_piv& recpiv = RECpivlist[typ][sat.sys][rec];
	if (satp) satpiv.pre_rec=rec;
	else 	  recpiv.pre_sat=sat;
	
	string recv;
	if (enduser)	recv = rec;
	else			recv = "NETWORK"; 
	
	satpiv.rec_amb[rec] = amb;
	recpiv.sat_amb[sat] = amb;
	
	KFKey  key    = {KF::AMBIGUITY,sat,rec,typ};
	
	auto& AR_mealist = ARstations[recv].AR_meaMap;
	AR_mealist[key].int_amb = amb;
	AR_mealist[key].hld_epc = 0;
	AR_mealist[key].fix_fin = time;
}


/** Remove ambiguity from pivot tree */
void   Unlnk_amb(
	E_AmbTyp typ,			///< Ambiguity type
	SatSys sat, 			///< Satellite
	string rec, 			///< Receiver
	bool satp,				///< Link direction indicator, true = rec -> sat link
	bool enduser = false)	///< End user solution
{
	GinAR_piv& satpiv = SATpivlist[typ][sat.sys][sat];
	GinAR_piv& recpiv = RECpivlist[typ][sat.sys][rec];
	SatSys s0={};
	if (satp) satpiv.pre_rec="";
	else 	  recpiv.pre_sat=s0;
	
	if (satpiv.rec_amb.find(rec) != satpiv.rec_amb.end()) satpiv.rec_amb.erase(rec);
	if (recpiv.sat_amb.find(sat) != recpiv.sat_amb.end()) recpiv.sat_amb.erase(sat);
	
	KFKey  key = {KF::AMBIGUITY,sat,rec,typ};
	
	string recv;
	if (enduser)	recv = rec;
	else			recv = "NETWORK";
	
	if (ARstations[recv].AR_meaMap.find(key) != ARstations[recv].AR_meaMap.end()) 
		ARstations[recv].AR_meaMap[key].hld_epc = -1;
}

/** Check Network pivot tree and list diconnected satellite / receiver */
void Check_rec( 
	Trace& trace, 	///< Debug trace
	E_Sys sys,		///< GNSS system
	E_AmbTyp typ, 	///< Ambiguity type
	string rec, 	///< Station/receiver that is part of the pivot
	int dept)		///< distance from anchor
{
	GinAR_piv& recpiv = RECpivlist[typ][sys][rec];
	if (dept==0) tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV %s", rec);
	else		 tracepdeex(ARTRCLVL, trace, "%s", rec);
	
	auto& AR_mealist = ARstations["NETWORK"].AR_meaMap;
	
	int nsat=0;
	for ( auto& [sat,amb] : recpiv.sat_amb )
	{
		if ( recpiv.pre_sat == sat ) 
			continue;
		
		if (nsat>0) 
		{
			tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV     ");
			for (int i = 0; i < dept; i++)
				tracepdeex(ARTRCLVL, trace, "        ");
		}
		
		KFKey key = {KF::AMBIGUITY,sat,rec,typ};
		if (AR_mealist[key].out_epc > 0)				{	disc_sat[sat] = E_SigWarning::SIG_OUTG;		tracepdeex(ARTRCLVL, trace, " X- ");	}		
		else if (AR_mealist[key].sat_ele < MIN_PIV_ELE) {	disc_sat[sat] = E_SigWarning::LOW_ELEV;		tracepdeex(ARTRCLVL, trace, " x- ");	}		
		else if (AR_mealist[key].cyl_slp)				{	disc_sat[sat] = E_SigWarning::CYC_SLIP;		tracepdeex(ARTRCLVL, trace, " *- ");	}
		else											{												tracepdeex(ARTRCLVL, trace, " -- ");	}
		if ( aval_sat.find(sat) == aval_sat.end() )		{	disc_sat[sat] = E_SigWarning::MAJ_OUTG;												}
		
		tracepdeex(ARTRCLVL, trace, "%s ", sat.id());
		
		GinAR_piv& satpiv = SATpivlist[typ][sys][sat];
		int nrec=0;
		for ( auto& [rec2,amb2] : satpiv.rec_amb )
		{
			if ( rec2 == rec ) 
				continue;
			
			if (nrec>0) 
			{
				tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV     ");
				for (int i = 0; i < dept+1; i++) tracepdeex(ARTRCLVL, trace, "        ");
			}
			
			KFKey key2 = {KF::AMBIGUITY,sat,rec2,typ};
			
			if (AR_mealist[key2].out_epc > 0)					{	disc_rec[rec2] = E_SigWarning::SIG_OUTG;	tracepdeex(ARTRCLVL, trace, " X- ");	}		
			else if (AR_mealist[key2].sat_ele < MIN_PIV_ELE) 	{	disc_rec[rec2] = E_SigWarning::LOW_ELEV;	tracepdeex(ARTRCLVL, trace, " x- ");	}		
			else if (AR_mealist[key2].cyl_slp)					{	disc_rec[rec2] = E_SigWarning::CYC_SLIP;	tracepdeex(ARTRCLVL, trace, " *- ");	}
			else												{												tracepdeex(ARTRCLVL, trace, " -- ");	}
			if ( aval_rec.find(rec2) == aval_rec.end() )		{	disc_rec[rec2] = E_SigWarning::MAJ_OUTG;											}
					
			Check_rec( trace, sys, typ, rec2, dept+2);
			
			nrec++;
		}
		
		nsat++;
	}
}

/** Find receiver (in pivot tree) to link disconnected satellite */
string Find__rec( 
	Trace& trace,	///< Debug trace
	E_Sys sys,		///< GNSS system
	E_AmbTyp typ, 	///< Ambiguity type
	string base, 	///< Station/receiver that is part of the pivot
	SatSys sat, 	///< Satellite to connect to pivot
	double& dist) 	///< Selection critetia (distance to anchor + (pi/2 - elev))
{
	KFKey key = {KF::AMBIGUITY,sat,base,typ};
	auto& AR_mealist = ARstations["NETWORK"].AR_meaMap;
	if (AR_mealist.find(key)!=AR_mealist.end())
	{
		tracepdeex(ARTRCLVL+2, trace, "\n  Testing %s ...  %2d %1d ", base, AR_mealist[key].hld_epc, AR_mealist[key].cyl_slp?1:0);
		if (   AR_mealist[key].hld_epc == 0
			&& AR_mealist[key].cyl_slp == false
			&& AR_mealist[key].sat_ele >= MIN_PIV_ELE)
		{
			dist=dist+(PI/2-AR_mealist[key].sat_ele);	
			tracepdeex(ARTRCLVL+2, trace, "  %.2f\n", dist);
			return base;
		}
	}
	
	double mindist=9999.9;
	string candidt="NOTFOUND"; 
	for (auto& [sat2,amb] : RECpivlist[typ][sys][base].sat_amb)
	{
		if  (RECpivlist[typ][sys][base].pre_sat == sat2)
			continue;
		
		for (auto& [rec,amb2] : SATpivlist[typ][sys][sat2].rec_amb)
		{
			if (rec == base)
				continue;
		
			double dis2 = dist+1;
			string rec2 = Find__rec( trace, sys, typ,rec,sat,dis2);
			if(	dis2 < mindist)
			{
				mindist=dis2;
				candidt=rec2;
			}
		}
		
	}
	dist = mindist;
	return candidt;
}

/** Find satellite (in pivot tree) to link disconnected receiver */
SatSys Find__sat( 
	Trace& trace,	///< Debug trace
	E_Sys sys,		///< GNSS system
	E_AmbTyp typ,	///< Ambiguity type
	string base,	///< Station/receiver that is part of the pivot
	string rec, 	///< Station/receiver to connect to pivot
	double& dist)	///< Selection critetia (distance to anchor + (pi/2 - elev))
{
	
	double mindist=9999.9;
	SatSys candidt ={};
	auto& AR_mealist = ARstations["NETWORK"].AR_meaMap;
	
	for (auto& [sat,amb] : RECpivlist[typ][sys][base].sat_amb)
	{
		if (RECpivlist[typ][sys][base].pre_sat == sat)
			continue;
		
		KFKey key = {KF::AMBIGUITY,sat,rec,typ};
		if (AR_mealist.find(key) != AR_mealist.end())
		{
			tracepdeex(ARTRCLVL+2, trace, "\n  Testing %s ...  %2d %1d ", sat.id().c_str(), AR_mealist[key].hld_epc, AR_mealist[key].cyl_slp?1:0);
			
			if (   AR_mealist[key].hld_epc == 0
				&& AR_mealist[key].cyl_slp == false
				&& AR_mealist[key].sat_ele >= MIN_PIV_ELE)
			{
				double dis2 = dist+(PI/2-AR_mealist[key].sat_ele);
				tracepdeex(ARTRCLVL+2, trace, " %.2f\n", dis2);
				if(dis2 < mindist)
				{
					mindist = dis2;
					candidt  = sat;
				}
				
			}
		}
	}
	if(mindist < 9999.9)
	{
		dist = mindist;
		return candidt;
	}
	
	for (auto& [sat,amb] : RECpivlist[typ][sys][base].sat_amb)
	{
		if  (RECpivlist[typ][sys][base].pre_sat == sat)
			continue;
			
		for (auto& [rec2,amb2] : SATpivlist[typ][sys][sat].rec_amb)
		{
			if(rec2 == base)  
				continue;
		
			double dis2 = dist+1;
			SatSys sat2  = Find__sat( trace, sys, typ, rec2, rec, dis2);
			if(	dis2 < mindist)
			{
				mindist = dis2;
				candidt = sat2;
			}
		}
	}	
		
	dist = mindist;
	return candidt;
}

/** Reset satellite on network pivot */
int Reset_sat( 
	Trace& trace,	///< Debug trace
	E_Sys sys,		///< GNSS system
	E_AmbTyp typ,	///< Ambiguity type
	SatSys sat, 	///< GNSS satellite
	GTime time) 	///< Reset time
{
	SATbialist[typ].erase(sat);
	SatSys sat0 = {};
	KFKey  key = {KF::AMBIGUITY,sat,"",typ};
	string tmp;
	double maxel  = 0;
	double maxel2 = 0;
	int    intamb = 0;
	bool   is_raw = true;
	
	tracepdeex(ARTRCLVL+1, trace, "\n#ARES_PIV (Re) setting %s: ", sat.id().c_str());
	
	auto& AR_mealist = ARstations["NETWORK"].AR_meaMap;
	
	for (auto& [rec,piv] : RECpivlist[typ][sys])
	{
		key.str = rec;
		if ((rec != AR_reflist[sys]) && (piv.pre_sat == sat0))		continue;
		if (AR_mealist.find(key) == AR_mealist.end())				continue;
		if (AR_mealist[key].out_epc >  0  )							continue;
		if (AR_mealist[key].sat_ele <  MIN_PIV_ELE)					continue;
		if (AR_mealist[key].sat_ele <  maxel )						continue;
		
		if (AR_mealist[key].flt_var >= 0)
		{
			intamb = ROUND(AR_mealist[key].flt_amb);
			tmp = rec;
			is_raw = false;
			maxel = AR_mealist[key].sat_ele;
			continue;
		}
		
		if (!is_raw) 												continue;
		if (AR_mealist[key].sat_ele <  maxel2)						continue;
		
		double satbia = 0;
		double recbia = 0;
		if (RECbialist[typ][sys].find(rec) != RECbialist[typ][sys].end()) 
			recbia = RECbialist[typ][sys][rec].rawbias;
		if (SATbialist[typ].find(sat) != SATbialist[typ].end()) 
			satbia = SATbialist[typ][sat].rawbias;
		
		intamb = ROUND(AR_mealist[key].raw_amb-recbia-satbia);
		tmp = rec;
		maxel2 = AR_mealist[key].sat_ele;
	}
	
	if ( typ == +E_AmbTyp::WL12 ) 
		remov_WLsate(trace,typ,sat);
			
	if ((maxel  >= MIN_PIV_ELE) 
	 || (maxel2 >= MIN_PIV_ELE)) 
	 {
		Link_amb(typ,sat,tmp,true,intamb,time);
		
		if (is_raw) tracepdeex(ARTRCLVL+1, trace, " from raw %s -> %s  %d", tmp, sat.id().c_str(), intamb);
		else        tracepdeex(ARTRCLVL+1, trace, " from flt %s -> %s  %d", tmp, sat.id().c_str(), intamb);
		
		return 1;
	}
	tracepdeex(ARTRCLVL+1, trace, " failed");
	return 0;
}

/** Reset receiver on network pivot */
int Reset_rec( 
	Trace& trace,	///< Debug trace
	E_Sys sys,		///< GNSS system
	E_AmbTyp typ,	///< Ambiguity type
	string rec, 	///< Station/receiver name
	GTime time)		///< Reset time
{
	RECbialist[typ][sys].erase(rec);
	SatSys tmp    = {};
	KFKey  key    = {KF::AMBIGUITY,tmp,rec,typ};
	double maxel  = 0;
	double maxel2 = 0;
	int    intamb = 0;
	bool   is_raw = true;
	
	tracepdeex(ARTRCLVL+1, trace, "\n#ARES_PIV (Re) setting %s: ", rec);
	auto& AR_mealist = ARstations["NETWORK"].AR_meaMap;
	
	for (auto& [sat,piv] : SATpivlist[typ][sys])
	{
		key.Sat=sat;
		if ( piv.pre_rec == "")									continue;
		
		if (AR_mealist.find(key) == AR_mealist.end()) 			continue;
		if (AR_mealist[key].out_epc >  0  ) 					continue;
		if (AR_mealist[key].sat_ele <  MIN_PIV_ELE)				continue;
		if (AR_mealist[key].sat_ele <  maxel )					continue;
		
		if (AR_mealist[key].flt_var >= 0)
		{
			intamb = ROUND(AR_mealist[key].flt_amb);
			maxel = AR_mealist[key].sat_ele;
			tmp = sat;
			is_raw = false;
			continue;
		}
		
		if (!is_raw) 											continue;
		if (AR_mealist[key].sat_ele <  maxel2)					continue;
		
		double satbia = 0;
		double recbia = 0;
		if (RECbialist[typ][sys].find(rec) != RECbialist[typ][sys].end()) 
			recbia = RECbialist[typ][sys][rec].rawbias;
		if (SATbialist[typ].find(sat) != SATbialist[typ].end()) 
			satbia = SATbialist[typ][sat].rawbias;	
		
		intamb = ROUND(AR_mealist[key].raw_amb-recbia-satbia);
		tmp = sat;
		maxel2 = AR_mealist[key].sat_ele;
	}
	
	if ( typ == +E_AmbTyp::WL12 ) 
		remov_WLrecv(trace,typ,rec,sys);
	
	if ((maxel  >= MIN_PIV_ELE) 
	 || (maxel2 >= MIN_PIV_ELE))
	{
		Link_amb(typ,tmp,rec,false,intamb,time);
		
		if (is_raw) tracepdeex(ARTRCLVL+1, trace, " from raw %s  -> %s %d", tmp.id().c_str(), rec, intamb);
		else        tracepdeex(ARTRCLVL+1, trace, " from flt %s  -> %s %d", tmp.id().c_str(), rec, intamb);
		
		return 1;				
	}
	tracepdeex(ARTRCLVL+1, trace, " failed");
	return 0;	
}

/** Initilize end-user AR pivot */
void init_usr_pivot( 
	Trace& trace,		///< Debug trace
	E_Sys sys,			///< GNSS System
	GinAR_opt& opt, 	///< Ginan AR control options
	E_AmbTyp typ,		///< Ambiguity type
	GTime time)			///< Solution time
{
	string rec = opt.recv;
	
	tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV Initializing pivot for %s %s %s", rec,sys._to_string(), typ._to_string());
	double maxel = 0;
	SatSys pivsat;
	int    pivamb = -99999;
	
	auto& AR_mealist = ARstations[opt.recv].AR_meaMap;
	
	for (auto& [key, amb] : AR_mealist)
	{
		if (key.num 	!= typ) continue;
		if (key.Sat.sys != sys) continue;
		if (key.str     != rec) continue;
		if (amb.out_epc >  0  ) continue; 
		
		tracepdeex(ARTRCLVL+2, trace, "\n considering %s: %.4f  /  %.4f", key.Sat.id().c_str(), amb.sat_ele*R2D, maxel*R2D);
		
		if (amb.sat_ele > maxel)
		{
			maxel  = amb.sat_ele;
			pivsat = key.Sat;
			pivamb = ROUND(amb.flt_amb);
		}
	}
	
	GinAR_piv& recpiv = RECpivlist[typ][sys][rec];
	SatSys s0 = {};
	recpiv.pre_sat = s0;
	recpiv.pre_rec = "";
	recpiv.rec_amb.clear();
	recpiv.sat_amb.clear();
		
	if ( maxel < opt.MIN_Elev_piv )
	{
		tracepdeex(ARTRCLVL+1, trace, "#ARES_PIV ... no suitable candidate found");
		return;
	}
	
	Link_amb(typ,pivsat,rec,false,pivamb,time, true);
	
	KFKey key = {KF::AMBIGUITY,pivsat,rec,typ};
	AR_mealist[key].int_amb = pivamb;
	AR_mealist[key].fix_fin = time;
	AR_mealist[key].hld_epc = 0;
	
	tracepdeex(ARTRCLVL+1, trace, "... %s elev: %5.1f", pivsat.id().c_str(), maxel*R2D);
	return;
}

/** Update User Pivot */
void updt_usr_pivot ( 
	Trace& trace,		///< Debug trace
	GTime time, 		///< Solution time
	GinAR_opt& opt, 	///< Ginan AR control options
	E_AmbTyp typ )		///< Ambiguity type
{
	string rec = opt.recv;
	tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV Checking pivot for %s", rec);
	
	auto& AR_mealist = ARstations[rec].AR_meaMap;
	
	for (auto [sys,act] : sys_solve) 
	{
		if(!act)
			continue;
	
		GinAR_piv& recpiv = RECpivlist[typ][sys][rec];
		bool pivout = false;
		if ( recpiv.sat_amb.size() != 1)
		{
			if (typ == +E_AmbTyp::WL12) 
				reset_WLfilt(trace,typ,time,rec,sys);
				
			recpiv.sat_amb.clear();
			init_usr_pivot( trace, sys, opt, typ, time);
			continue;
		}
		
		SatSys pivsat = recpiv.sat_amb.begin()->first;
		KFKey key = {KF::AMBIGUITY,pivsat,rec,typ};
		
		if (AR_mealist.find(key) == AR_mealist.end()) 	pivout=true;
		if (AR_mealist[key].cyl_slp) 					pivout=true;
		if (AR_mealist[key].sat_ele < opt.MIN_Elev_AR) 		pivout=true;
		if (AR_mealist[key].out_epc>0) 					pivout=true;
		
		if(pivout)
		{
			Unlnk_amb(typ,pivsat,rec,false,true);
			AR_mealist[key].hld_epc = -1;
			double maxel  = 0;
			int    pivamb = 0;
			for (auto& [key2, amb] : AR_mealist)
			{
				if (key2.num		!= typ) 			continue;
				if (key2.Sat.sys != sys) 				continue;
				if (key2.str		!= rec) 			continue;
				if (amb.sat_ele 	< opt.MIN_Elev_piv)		continue;
				if (amb.out_epc  != 0  ) 				continue;
				if (amb.hld_epc  < 0  )  				continue;
				if (amb.hld_epc  > opt.Max_Hold_epc  ) 		continue;
				
				maxel  = amb.sat_ele;
				pivsat = key2.Sat;
				pivamb = amb.int_amb;
			}
			if( maxel < opt.MIN_Elev_piv )
			{
				if(typ == +E_AmbTyp::WL12) reset_WLfilt(trace,typ,time,rec,sys);
				tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV     Resetting pivot for %s in %s",sys._to_string(),rec);
				init_usr_pivot( trace, sys, opt, typ, time);
			}
			else
			{
				Link_amb(typ,pivsat,rec, false,pivamb,time,true);
				tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV     Changing pivot for %s in %s : %s",sys._to_string(),rec.c_str(),pivsat.id().c_str());
			}
		}
	}
}

/** Initialize Network pivot */
void init_net_pivot ( 
	Trace& trace,		///< Debug trace
	E_Sys sys,			///< GNSS system
	GinAR_opt& opt, 	///< Ginan AR control options
	E_AmbTyp typ,		///< Ambiguity type
	GTime time)			///< Solution time
{
	MIN_PIV_ELE = opt.MIN_Elev_piv;
	
	if ( typ == +E_AmbTyp::WL12 ) 
		reset_WLfilt(trace, typ, time, opt.recv, sys);
	
	auto& AR_mealist = ARstations["NETWORK"].AR_meaMap;
	
	RECpivlist[typ][sys].clear();
	SATpivlist[typ][sys].clear();
	
	map<string,int> nsat;
	map<SatSys,int> nrec;
	for (auto& [key, amb] : AR_mealist)
	{
		if (key.num 	   != typ)			continue;
		if (key.Sat.sys != sys)				continue;
		amb.flt_var = -1;
		if (amb.out_epc >  0  )				continue; 
		if (amb.sat_ele < opt.MIN_Elev_piv)	continue;
		
		nsat[key.str]++;
		nrec[key.Sat]++;

	}
	
	/* Select anchor/reference station, if not selected by the "config_AmbigResl" function, or the selected station has no measurments, it is selected among those with maximum number of available measurements */
	string sysref = AR_reflist[sys];
	if (nsat.find(sysref) == nsat.end())
	{
		int nmax=0;
		for (auto& [rec, n] : nsat)
		{
			if (n>nmax)
			{
				nmax = n;
				sysref = rec;
			}
		}
		AR_reflist[sys] = sysref;
		tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV Reselecting Anchor station");
	}
	tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV Initializing pivot from anchor %s minel: %.2f", sysref, MIN_PIV_ELE*R2D);
	
	SatSys sat0 = {};
	RECpivlist[typ][sys][sysref].pre_sat = sat0;
	RECbialist[typ][sys][sysref].rawbias = 0;																						/* Bias for anchor station is 0 */
	
	/* Include satellites visible by the anchor station (that meet the criteria to be included in the pivot */ 
	for (auto& [key, amb] : AR_mealist)
	{
		if (key.num 	   	!= typ) 		continue;
		if (key.Sat.sys 	!= sys) 		continue;
		if (key.str 	   	!= sysref) 		continue;
		if (amb.out_epc >  0  ) 			continue; 
		if (amb.sat_ele < MIN_PIV_ELE) 		continue;
		
		Link_amb(typ,key.Sat,sysref,true,ROUND(amb.raw_amb),time);
		tracepdeex(ARTRCLVL+1, trace, "\n#ARES_PIV Setting from raw %s -> %s  %d", sysref.c_str(), key.Sat.id().c_str(), ROUND(amb.raw_amb) );
		nrec.erase(key.Sat);
		SATbialist[typ][key.Sat].rawbias = amb.raw_amb - SATpivlist[typ][sys][key.Sat].rec_amb[sysref];								/* Bias for Satellites connected to anchor is initialized as (measurment - ambiguity) */
	}
	
	int nnew = 1;	
	while (nnew > 0)
	{
		nnew = 0;
		/* Station cycle: will try to connect receivers/stations to the network pivot */
		for (auto it = nsat.begin(); it !=nsat.end();)
		{
			string rec = it->first;			
			if (Reset_rec( trace,sys,typ,rec,time )>0)									/* Reset_rec will find satellites already part of the pivot that can see the station "rec" (return 0 if not found) */
			{
				SatSys sat = RECpivlist[typ][sys][rec].pre_sat;							/* The satellite with best metric is stored here */
				KFKey  key = {KF::AMBIGUITY,sat,rec,typ};
				if (SATbialist[typ].find(sat) != SATbialist[typ].end())
				{
					double rawmeas = AR_mealist[key].raw_amb;
					double sigambg = RECpivlist[typ][sys][rec].sat_amb[sat];
					double satbias = SATbialist[typ][sat].rawbias;
					
					RECbialist[typ][sys][rec].rawbias = rawmeas - sigambg - satbias;	/* The receiver bias is initialized as (measurment - ambiguity - satbias) */
				}
				nnew++;
				it = nsat.erase(it);
			}
			else it++;
		}
		
		/* Satellite Cycle */
		for (auto it = nrec.begin(); it !=nrec.end();)
		{
			SatSys sat = it->first;						
			if (Reset_sat(trace,sys,typ,sat,time)>0)									/* Reset_sat will find stations already part of the pivot that can see the satellite "sat" (return 0 if not found)  */
			{
				string rec = SATpivlist[typ][sys][sat].pre_rec;							/* The station with best metric is stored here */ 
				KFKey  key = {KF::AMBIGUITY,sat,rec,typ};
				
				if (RECbialist[typ][sys].find(rec) != RECbialist[typ][sys].end())
				{
					double rawmeas = AR_mealist[key].raw_amb;
					double sigambg = SATpivlist[typ][sys][sat].rec_amb[rec];
					double recbias = RECbialist[typ][sys][rec].rawbias;
					
					SATbialist[typ][sat].rawbias = rawmeas - sigambg - recbias;			/* The satellite bias is initialized as (measurment - ambiguity - recbias) */
				}
				nnew++;
				it = nrec.erase(it);
			}
			else it++;
		}		
	}	
}

/** Update network pivot */
void updt_net_pivot( 
	Trace& trace,		///< Debug trace 
	GTime time, 		///< Solution time
	GinAR_opt& opt, 	///< Gina AR control options
	E_AmbTyp typ)		///< Ambiguity type
{
	MIN_PIV_ELE = opt.MIN_Elev_piv;
	auto& AR_mealist = ARstations["NETWORK"].AR_meaMap;
	
	for (auto [sys,act] : sys_solve) 
	{
		if(!act)
			continue;
		
		auto& recpivlst = RECpivlist[typ][sys];
		auto& satpivlst = SATpivlist[typ][sys];
		
		tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV Checking network pivot for %s %d", sys._to_string(), recpivlst.size());
		
		/* Check anchor station */ 
		string sysref = AR_reflist[sys];
		if ( recpivlst.empty() )
		{
			init_net_pivot(trace,sys,opt,typ,time);
			Check_rec( trace,sys,typ,AR_reflist[sys],0);
			continue;
		} 
		else
		{
			bool ref_ok = false;
			for (auto& [sat, n] : recpivlst[sysref].sat_amb)
			{
				KFKey key = {KF::AMBIGUITY,sat,sysref,typ};
				if (AR_mealist.find(key) == AR_mealist.end()) 	continue;
				if (AR_mealist[key].cyl_slp) 					continue;
				if (AR_mealist[key].hld_epc != 0  ) 			continue;
				if (AR_mealist[key].sat_ele <  opt.MIN_Elev_AR)		continue;
				ref_ok = true;
				break;
			}
			if (!ref_ok)
			{
				init_net_pivot(trace,sys,opt,typ,time);
				continue;
			}
		}	
		
		aval_rec.clear();
		aval_sat.clear();
		disc_rec.clear();
		disc_sat.clear();

		for (auto& [key, amb] : AR_mealist)
		{
			if (key.Sat.sys != sys) 
				continue;
			
			if (amb.out_epc == 0 
			 && amb.sat_ele >  opt.MIN_Elev_piv)
			{
				aval_sat[key.Sat]++;
				aval_rec[key.str]++;
			}
			
			if (recpivlst.find(key.str) == recpivlst.end()) disc_rec[key.str] = E_SigWarning::SIG_OUTG;
			if (satpivlst.find(key.Sat) == satpivlst.end()) disc_sat[key.Sat] = E_SigWarning::SIG_OUTG;
		}
		Check_rec(trace,sys,typ,sysref,0);
		
		if (disc_sat.empty() 
		 && disc_rec.empty()) 
			continue;
		
		for (auto it = disc_sat.begin(); it != disc_sat.end();)
		{
			auto& [sat, mes] = *it;
			Unlnk_amb(typ,sat,satpivlst[sat].pre_rec,true);
			if (mes == E_SigWarning::MAJ_OUTG)
			{
				remov_WLsate(trace,typ,sat);
				SATbialist[typ].erase(sat);
				satpivlst.erase(sat);
				it = disc_sat.erase(it);
			}
			else it++;
		}
		for (auto it = disc_rec.begin(); it != disc_rec.end();)
		{
			auto& [rec, mes] = *it;
			Unlnk_amb(typ,recpivlst[rec].pre_sat,rec,false);
			if (mes == E_SigWarning::MAJ_OUTG)
			{
				remov_WLrecv(trace,typ,rec,sys);
				RECbialist[typ][sys].erase(rec);
				recpivlst.erase(rec);
				it = disc_rec.erase(it);
			}
			else it++;
		}

		tracepdeex(ARTRCLVL+2, trace, "\n   Reconnection cycle %d %d", disc_rec.size(), disc_sat.size());
		
		for (auto& [recs,mess] : disc_rec) tracepdeex(ARTRCLVL+2, trace, "\n     %s %d", recs.c_str(),      recpivlst[recs].sat_amb.size());
		for (auto& [sats,mess] : disc_sat) tracepdeex(ARTRCLVL+2, trace, "\n     %s %d", sats.id().c_str(), satpivlst[sats].rec_amb.size());

		while (!disc_rec.empty()
			|| !disc_sat.empty())
		{
			int nnew = 0;

			/* connecting disconnected stations */
			for (auto it = disc_rec.begin(); it != disc_rec.end();)
			{
				string rec = it->first;
				double dist = 0;
				SatSys sat = Find__sat(trace,sys,typ,sysref,rec,dist);
				if (dist < 9999.9)
				{
					KFKey key = {KF::AMBIGUITY,sat,rec,typ};
					tracepdeex(ARTRCLVL+1, trace, "\n Setting from fix %s -> %s  %d",rec.c_str(),sat.id().c_str(), AR_mealist[key].int_amb); 
					Link_amb(typ,sat,rec,false,AR_mealist[key].int_amb,time);
					nnew++;
					it = disc_rec.erase(it);
				}
				else it++;
			}
			
			/* reconnecting disconnected satellites */
			for (auto it = disc_sat.begin(); it != disc_sat.end();)
			{
				SatSys sat = it->first;
				tracepdeex(ARTRCLVL+2, trace, "\n Trying to connect %s:", sat.id().c_str());
				double dist = 0;
				string rec = Find__rec(trace,sys,typ,sysref,sat,dist);
				if (dist<9999.9)
				{
					KFKey key = {KF::AMBIGUITY,sat,rec,typ};
					tracepdeex(ARTRCLVL+1, trace, "\n#ARES_PIV Setting from fix %s  -> %s %d", sat.id().c_str(),rec.c_str(), AR_mealist[key].int_amb);
					Link_amb(typ,sat,rec,true,AR_mealist[key].int_amb,time);
					nnew++;
					it = disc_sat.erase(it);
				}
				else it++;
			}
			
			if (nnew>0) 
				continue;
			
			/* adding new satellite to diconnected list */
			for ( auto& [recs,mess] : disc_rec ) 
			{
				if (recpivlst[recs].sat_amb.empty())
					continue;
			
				SatSys sats = recpivlst[recs].sat_amb.begin()->first;
				tracepdeex(ARTRCLVL+1, trace, "\n Disconnecting %s:", sats.id().c_str());
				Unlnk_amb(typ,sats,recs,true);
				disc_sat[sats] = E_SigWarning::USR_DISC;
				nnew++;
				break;
			}
			
			if (nnew>0) 
				continue;
			
			/* adding new station to disconnected list */
			for ( auto& [sats,mess] : disc_sat ) 
			{
				if  (satpivlst[sats].rec_amb.empty())
					continue;
				
				string recs = satpivlst[sats].rec_amb.begin()->first;
				tracepdeex(ARTRCLVL+1, trace, "\n Disconnecting %s:", recs);
				Unlnk_amb(typ,sats,recs,false);
				disc_rec[recs] = E_SigWarning::USR_DISC;
				nnew++;
				break;
			}
			
			if (nnew>0) 
				continue;
			
			nnew=1;
			while (nnew>0)
			{
				nnew=0;
				/* resetting disconnected satellite/station */
				for (auto it = disc_sat.begin(); it != disc_sat.end();)
				{
					SatSys sats = it->first;
					if (Reset_sat(trace,sys,typ,sats,time) > 0)
					{
						nnew++;
						it = disc_sat.erase(it);
					}
					else it++;
				}
			
				for (auto it = disc_rec.begin(); it != disc_rec.end();)
				{
					string recs = it->first;
					if (Reset_rec(trace,sys,typ,recs,time) > 0)
					{
						nnew++;
						it = disc_rec.erase(it);
					}
					else it++;
				}
			}
			
			for ( auto& [recs,mess] : disc_rec ) 
			{
				tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV WARNING: pivot for %s could not be found, erasing", recs.c_str());
				RECbialist[typ][sys].erase(recs);
				recpivlst.erase(recs);
			}
			
			for ( auto& [sats,mess] : disc_sat ) 
			{
				tracepdeex(ARTRCLVL, trace, "\n#ARES_PIV WARNING: pivot for %s could not be found, erasing", sats.id().c_str());
				SATbialist[typ].erase(sats);
				satpivlst.erase(sats);
			}
				
			disc_rec.clear();
			disc_sat.clear();
			
			break;
		}
	}
}