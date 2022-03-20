#include "GNSSambres.hpp"
#include "biasSINEX.hpp"
#include <iomanip>

using std::setprecision;
using std::setw;
using std::setfill;

map<E_AmbTyp,map<E_Sys,map<string,GinAR_bia>>>	RECbialist;
map<E_AmbTyp,map<SatSys,GinAR_bia>>				SATbialist;

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

/** Output GPGGA or modified GPGGA messages */ 
void gpggaout(
	string		outfile,		///< File name to output GGA message
	KFState&	kfState,		///< KF containing the positioning solution
	string		recId,			///< Receiver ID
	int 		solStat,		///< Solution quiality (2: ambiguity float, 5: ambiguity fix)
	int			numSat,			///< # of satellites
	double      hdop,			///< Horizontal DOP
	bool		lng,			///< Modified GPGGA format (false: GPGGA format according to NMEA 0183)
	bool		print_header)	///< Print header line
{
	double ep[6];
	time2epoch(kfState.time, ep);

	std::ofstream fpar(outfile, std::ios::out | std::ios::app);

	if (print_header) 
		tracepdeex(1,fpar,"!GPGGA, UTC time, Latitude, E/W, Longitude, N/S, State, # Sat, HDOP, Height, , Geoid,\n");

	double xyz_[3];
	for (short i = 0; i < 3; i++)
		kfState.getKFValue({KF::REC_POS,		{}, recId,	i}, xyz_[i]);
	
	double lla_[3];
	ecef2pos(xyz_, lla_);
	lla_[0]*=R2D;
	lla_[1]*=R2D;
	
	double latint = floor(fabs(lla_[0]));
	double lonint = floor(fabs(lla_[1]));
	
	double latv = latint*100 + (fabs(lla_[0]) - latint)*60;
	double lonv = lonint*100 + (fabs(lla_[1]) - lonint)*60;
	char   latc = lla_[0]>0?'N':'S';
	char   lonc = lla_[1]>0?'E':'W';
	
	if (lng)
	{
		tracepdeex(1,fpar,"$GPGGALONG,%02.0f%02.0f%05.2f,",ep[3],ep[4],ep[5]);
		tracepdeex(1,fpar,"%012.7f,%c,%013.7f,%c,",	latv,latc, lonv,lonc);
		tracepdeex(1,fpar,"%1d,%02d,%3.1f,%09.3f",	solStat, numSat, hdop,lla_[2]);
	}
	else
	{
		tracepdeex(1,fpar,"$GPGGA,%02.0f%02.0f%05.2f,",ep[3],ep[4],ep[5]);
		tracepdeex(1,fpar,"%09.4f,%c,%010.4f,%c,",	latv,latc, lonv,lonc);
		tracepdeex(1,fpar,"%1d,%02d,%3.1f,%08.2f",	solStat, numSat, hdop,lla_[2]);
	}
	tracepdeex(1,fpar,",M,0.0,M,,,\n");
}

/** Output ambiguity measurments/states */
void artrcout( 
	Trace& trace,	///< Output stream 
	GTime time, 	///< Solution time
	string rec, 	///< Receiver ID ("NETWORK": all stations in network)
	GinAR_opt opt )	///< Ginan AR control options
{
	int week;
	double tow = time2gpst(time,&week);
	map<E_AmbTyp,int> Nmeas;
	map<E_AmbTyp,int> Nfixd;
	
	auto& AR_mealist = ARstations[opt.recv].AR_meaMap;
	for (auto& [key,amb] : AR_mealist)
	{
		if ( rec != "NETWORK" && key.str!=rec ) 
			continue;
		
		E_AmbTyp typs = E_AmbTyp::_from_integral(key.num);
		tracepdeex(ARTRCLVL+1, trace, "\n#ARES_MEAS, %s", amb.mea_fin.to_string(0));
		tracepdeex(ARTRCLVL+1, trace, ", %s, %s, %s", key.str.c_str(), key.Sat.id().c_str(), typs._to_string());
		tracepdeex(ARTRCLVL+1, trace, ", %6.2f, %4d, %4d, %1d, %10.5f", amb.sat_ele*R2D, amb.hld_epc, amb.out_epc, amb.cyl_slp?1:0, amb.raw_amb);
		
		double satbias = 0;
		double recbias = 0;
		if (RECbialist[typs][key.Sat.sys].find(key.str) != RECbialist[typs][key.Sat.sys].end()) 
			recbias = RECbialist[typs][key.Sat.sys][key.str].rawbias;
		if (SATbialist[typs].find(key.Sat) != SATbialist[typs].end()) 
			satbias = SATbialist[typs][key.Sat].rawbias;
		
		tracepdeex(ARTRCLVL+1, trace, ", %10.5f, %10.5f, %10.5f %4d %10.3e", amb.flt_amb, satbias, recbias, amb.int_amb, amb.flt_var);
		
		if ( amb.sat_ele < opt.MIN_Elev_AR ) 
			continue;
		
		Nmeas[typs]++;
		
		if ( amb.hld_epc >= 0 && amb.hld_epc <= opt.Max_Hold_epc ) 
			Nfixd[typs]++;
	}

	tracepdeex(ARTRCLVL,trace,"\n#ARES_FIX %4d, %8.1f", week, tow);
	for (auto& [typ,nm] : Nmeas)
	{
		tracepdeex(ARTRCLVL,trace,", %s, %4d, %4d", typ._to_string(), Nmeas[typ], Nfixd[typ]);
	}
	
	if (opt.endu)
	{
		double latLonHt[3];
		ecef2pos(ARstations[rec].snxPos_, latLonHt); 
			
		double dXYZ_FL[3];
		double dXYZ_AR[3];
		for(short i = 0; i < 3; i++)
		{
			dXYZ_AR[i] = ARstations[rec].fixPos_[i] - ARstations[rec].snxPos_[i];
			dXYZ_FL[i] = ARstations[rec].fltPos_[i] - ARstations[rec].snxPos_[i];
		}
	
		double dENU_FL[3];
		double dENU_AR[3];
		ecef2enu(latLonHt, dXYZ_AR, dENU_AR);
		ecef2enu(latLonHt, dXYZ_FL, dENU_FL);
	
		int week_;
		double tow_ = time2gpst(time,&week_);
		tracepdeex(2,trace, "\n#ARES_DPOS,%4d,%8.1f,",week_,tow_);
		tracepdeex(2,trace, "%8.4f,%8.4f,%8.4f,",dENU_AR[0],dENU_AR[1],dENU_AR[2]);
		tracepdeex(2,trace, "%8.4f,%8.4f,%8.4f,",dENU_FL[0],dENU_FL[1],dENU_FL[2]);
	}
	
	return;	
}

/** Output phase biases */
void arbiaout( 
	Trace& trace,	///< Output stream 
	GTime time, 	///< Solution time
	double tupdt )	///< Update interval (sec)
{
	int week;
	double tow=time2gpst(time,&week);
	for (auto& [typ,lst] : SATbialist)
	for (auto& [sat,bia] : lst)
	{
		/* reflect the biases here (maybe save a time series) */
		tracepdeex(ARTRCLVL+1,trace,"\n#ARES_BIA %4d, %8.1f, %s, %s       %8.4f, %8.4f, %11.4e", week, tow, typ._to_string(), sat.id().c_str(),bia.rawbias, bia.outbias, sqrt(bia.outvari));
		
		E_ObsCode non  = E_ObsCode::NONE;
		E_ObsCode def1 = defCodesL1[sat.sys];
		E_ObsCode def2 = defCodesL2[sat.sys];
		E_ObsCode def3 = defCodesL3[sat.sys];
		auto btyp =  E_BiasType::OSB;
		
		if (typ == +E_AmbTyp::UCL1) outp_bias(trace,time,btyp,"", sat,def1,non,bia.outbias,bia.outvari,tupdt,PHAS);
		if (typ == +E_AmbTyp::UCL2) outp_bias(trace,time,btyp,"", sat,def2,non,bia.outbias,bia.outvari,tupdt,PHAS);
		if (typ == +E_AmbTyp::UCL3) outp_bias(trace,time,btyp,"", sat,def3,non,bia.outbias,bia.outvari,tupdt,PHAS);
		if (typ == +E_AmbTyp::NL12)
		{
			if(SATbialist[E_AmbTyp::WL12].find(sat) == SATbialist[E_AmbTyp::WL12].end()) 
				continue;
				
			double NLbia = bia.outbias;
			double NLvar = bia.outvari;
			double WLbia = SATbialist[E_AmbTyp::WL12][sat].outbias;
			double WLvar = SATbialist[E_AmbTyp::WL12][sat].outvari;
		
			double lam1  = lambdas[def1];
			double lam2  = lambdas[def2];
		
			double c1	 = lam1 / (lam2 - lam1);
			double c2	 = lam2 / (lam2 - lam1);
			double L1bia = lam1 * (NLbia - c1 * WLbia);
			double L2bia = lam2 * (NLbia - c2 * WLbia);
			double L1var = lam1*lam1 * (NLvar + c1*c1 * WLvar);
			double L2var = lam2*lam2 * (NLvar + c2*c2 * WLvar);
		
			outp_bias(trace,time,btyp,"",sat,def1,non,L1bia,L1var,tupdt,PHAS);
			outp_bias(trace,time,btyp,"",sat,def2,non,L2bia,L2var,tupdt,PHAS);
		}
	}

	SatSys sat0 = {};	
	for (auto& [typ,lst1] : RECbialist)
	for (auto& [sys,lst2] : lst1)
	for (auto& [rec,bia]  : lst2)
	{
		/* reflect the biases here (maybe save a time series) */
		tracepdeex(ARTRCLVL+1,trace,"\n#ARES_BIA %4d, %8.1f, %s, %s,      %s, %8.4f, %8.4f, %11.4e", week, tow, typ._to_string(), sys._to_string(), rec, bia.rawbias, bia.outbias, sqrt(bia.outvari));
		
		E_ObsCode non  = E_ObsCode::NONE;
		E_ObsCode def1 = defCodesL1[sys];
		E_ObsCode def2 = defCodesL2[sys];
		E_ObsCode def3 = defCodesL3[sys];
		auto btyp =  E_BiasType::OSB;
		
		if (typ == +E_AmbTyp::UCL1) outp_bias(trace,time,btyp,rec,sat0,def1,non,bia.outbias,bia.outvari,tupdt,PHAS);
		if (typ == +E_AmbTyp::UCL2) outp_bias(trace,time,btyp,rec,sat0,def2,non,bia.outbias,bia.outvari,tupdt,PHAS);
		if (typ == +E_AmbTyp::UCL3) outp_bias(trace,time,btyp,rec,sat0,def3,non,bia.outbias,bia.outvari,tupdt,PHAS);
		if (typ == +E_AmbTyp::NL12)
		{
			if (RECbialist[E_AmbTyp::WL12].find(sys)      == RECbialist[E_AmbTyp::WL12].end())			continue;
			if (RECbialist[E_AmbTyp::WL12][sys].find(rec) == RECbialist[E_AmbTyp::WL12][sys].end()) 	continue;
			
			double NLbia = bia.outbias;
			double NLvar = bia.outvari;
			double WLbia = RECbialist[E_AmbTyp::WL12][sys][rec].outbias;
			double WLvar = RECbialist[E_AmbTyp::WL12][sys][rec].outvari;
			
			double lam1  = lambdas[def1];
			double lam2  = lambdas[def2];
			
			double c1	 = lam1 / (lam2 - lam1);
			double c2	 = lam2 / (lam2 - lam1);
			double L1bia = lam1 * (NLbia - c1 * WLbia);
			double L2bia = lam2 * (NLbia - c2 * WLbia);
			double L1var = lam1*lam1 * (NLvar + c1*c1 * WLvar);
			double L2var = lam2*lam2 * (NLvar + c2*c2 * WLvar);
			
			outp_bias(trace,time,btyp,rec,sat0,def1,non,L1bia,L1var,tupdt,PHAS);
			outp_bias(trace,time,btyp,rec,sat0,def2,non,L2bia,L2var,tupdt,PHAS);
		}
	}
	return;
}

/** Export AR Ionosphere measurments */
int arionout(	
	Trace& trace,		///< Debug stream
	KFState& kfState,	///< KF containing GNSS solution
	ObsList& obsList,	///< GNSS observation list (with field for ionosphere output)
	GinAR_opt opt )		///< Ginan AR control options
{
	int nion=0;
	
	for (auto& obs : obsList)
	{
		KFKey key;
		if ( opt.ionmod == +E_IonoMode::ESTIMATE )
		{
			key = {KF::IONO_STEC,obs.Sat, obs.mount, 0};
			double val;
			double var;
			if	(!kfState.getKFValue(key,val,&var)) 
			continue;
			
			obs.STECsmth = val;
			obs.STECsmvr = var;
			obs.STECtype = 2;
			nion++;
		}
		else if ( opt.ionmod == +E_IonoMode::IONO_FREE_LINEAR_COMBO )
		{
			E_FType frq1 = F1;
			E_FType frq2 = F2;
			
			int wlamb =  retrv_WLambg(trace, E_AmbTyp::WL12,kfState.time,obs.mount,obs.Sat);
			
			if (opt.endu) key = {KF::PHASE_BIAS,obs.Sat, obs.mount, E_FType::FTYPE_IF12};
			else          key = {KF::AMBIGUITY ,obs.Sat, obs.mount, E_FType::FTYPE_IF12};
			
			double val;
			double var;
			
			if (!kfState.getKFValue(key,val,&var))
			{
				key.num = E_FType::FTYPE_IF15;
				if (!kfState.getKFValue(key,val,&var))		continue;
				frq2 = F5;
			}
			
			if ( wlamb == INVALID_WLVAL) 					continue;
			
			double lam1 = obs.satNav_ptr->lamMap[frq1];
			double lam2 = obs.satNav_ptr->lamMap[frq2];
			if ( lam1 == 0 || lam2 == 0)					continue;
			
			double L1 = obs.Sigs[frq1].L_corr_m;
			double L2 = obs.Sigs[frq2].L_corr_m;
			if	( L1 == 0 || L2 == 0 )						continue;
			double V1 = obs.Sigs[frq1].phasVar;
			double V2 = obs.Sigs[frq2].phasVar;
			
			double wlbias = 0;
			double wlvari = 0;
			if (!opt.endu)
			{
				auto& satbias = SATbialist[E_AmbTyp::WL12][key.Sat];
				auto& recbias = RECbialist[E_AmbTyp::WL12][key.Sat.sys][key.str];
				
				wlbias = satbias.rawbias + recbias.rawbias;
				wlvari = satbias.outvari + recbias.outvari;
			}
			
			double c2 = lam1*lam1/(lam2-lam1);
			double c1 = c2/(lam2+lam1);
			double c3 = lam1/lam2;
			double s1 = c1*c1;
			double s2 = c2*c2;
			double s3 = c3*c3;
			
			obs.STECsmth = c1*(L2-L1) + c2*(wlamb + wlbias) - c3*val;
			obs.STECsmvr = s1*(V2+V1) + s2*(        wlvari) + s3*val;
			obs.STECtype = 2;
			nion++;
		}
		else break;
	}
	return nion;
}
