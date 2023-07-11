
#include "coordinates.hpp"
#include "GNSSambres.hpp"
#include "biasSINEX.hpp"

#include <iomanip>

using std::setprecision;
using std::setw;
using std::setfill;

map<E_AmbTyp,map<E_Sys,map<string,GinAR_bia>>>	RECbialist;
map<E_AmbTyp,map<SatSys,GinAR_bia>>				SATbialist;

/** Output GPGGA or modified GPGGA messages
 */ 
void gpggaout(
	string		outfile,		///< File name to output GGA message
	KFState&	kfState,		///< KF containing the positioning solution
	string		recId,			///< Receiver ID
	int 		solStat,		///< Solution quiality (2: ambiguity float, 5: ambiguity fix)
	int			numSat,			///< Number of satellites
	double      hdop,			///< Horizontal DOP
	bool		lng)			///< Modified GPGGA format (false: GPGGA format according to NMEA 0183)
{
	GEpoch ep = kfState.time;

	std::ofstream fpar(outfile, std::ios::out | std::ios::app);

	if (fpar.tellp() == 0) 
		tracepdeex(1,fpar,"!GPGGA, UTC time, Latitude, N/S, Longitude, E/W, State, # Sat, HDOP, Height, , Geoid,\n");

	VectorEcef ecef;
	for (short i = 0; i < 3; i++)
		kfState.getKFValue({KF::REC_POS,		{}, recId,	i}, ecef[i]);
	
	VectorPos pos = ecef2pos(ecef);
	
	double latint = floor(fabs(pos.latDeg()));
	double lonint = floor(fabs(pos.lonDeg()));
	
	double latv = latint*100 + (fabs(pos.latDeg()) - latint)*60;
	double lonv = lonint*100 + (fabs(pos.lonDeg()) - lonint)*60;
	char   latc = pos.latDeg()>0?'N':'S';
	char   lonc = pos.lonDeg()>0?'E':'W';
	
	if (lng)
	{
		tracepdeex(1,fpar,"$GPGGALONG,%02.0f%02.0f%05.2f,",ep[3],ep[4],ep[5]);
		tracepdeex(1,fpar,"%012.7f,%c,%013.7f,%c,",	latv,latc, lonv,lonc);
		tracepdeex(1,fpar,"%1d,%02d,%3.1f,%09.3f",	solStat, numSat, hdop,pos.hgt());
	}
	else
	{
		tracepdeex(1,fpar,"$GPGGA,%02.0f%02.0f%05.2f,",ep[3],ep[4],ep[5]);
		tracepdeex(1,fpar,"%09.4f,%c,%010.4f,%c,",	latv,latc, lonv,lonc);
		tracepdeex(1,fpar,"%1d,%02d,%3.1f,%08.2f",	solStat, numSat, hdop,pos.hgt());
	}
	tracepdeex(1,fpar,",M,0.0,M,,,\n");
}

/** Output ambiguity measurments/states */
void artrcout( 
	Trace& trace,	///< Output stream 
	GTime time, 	///< Solution time
	GinAR_opt opt )	///< Ginan AR control options
{
	GTow	tow		= time;
	GWeek	week	= time;
	map<E_AmbTyp,int> Nmeas;
	map<E_AmbTyp,int> Nfixd;
	
	auto& AR_mealist = ARstations[opt.recv].AR_meaMap;
	for (auto& [key,amb] : AR_mealist)
	{
		if ( opt.recv != "NETWORK" && key.str!=opt.recv ) 
			continue;
		
		E_AmbTyp typs = E_AmbTyp::_from_integral(key.num);
		tracepdeex(ARTRCLVL+1, trace, "\n#ARES_MEAS, %s", amb.mea_fin.to_string().c_str());
		tracepdeex(ARTRCLVL+1, trace, ", %s, %s, %s", key.str.c_str(), key.Sat.id().c_str(), typs._to_string());
		tracepdeex(ARTRCLVL+1, trace, ", %6.2f, %4d, %4d, %1d, %10.5f", amb.sat_ele*R2D, amb.hld_epc, amb.out_epc, amb.cyl_slp?1:0, amb.raw_amb);
		
		double satbias = 0;
		double recbias = 0;
		
		if (RECbialist.find(typs) != RECbialist.end())
		if (RECbialist[typs].find(key.Sat.sys) !=RECbialist[typs].end())
		if (RECbialist[typs][key.Sat.sys].find(key.str) != RECbialist[typs][key.Sat.sys].end()) 
			recbias = RECbialist[typs][key.Sat.sys][key.str].rawbias;
		
		if (SATbialist.find(typs) != SATbialist.end()) 
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
		VectorPos pos = ecef2pos(ARstations[opt.recv].snxPos_); 
			
		VectorEcef	dXYZ_FL = ARstations[opt.recv].fltPos_ - ARstations[opt.recv].snxPos_;
		VectorEcef	dXYZ_AR = ARstations[opt.recv].fixPos_ - ARstations[opt.recv].snxPos_;
		
		VectorEnu	dENU_FL = ecef2enu(pos, dXYZ_FL);
		VectorEnu	dENU_AR = ecef2enu(pos, dXYZ_AR);
		
		tracepdeex(2,trace, "\n#ARES_DPOS,%4d,%8.1f,",week,tow);
		tracepdeex(2,trace, "%8.4f,%8.4f,%8.4f,",dENU_AR[0],dENU_AR[1],dENU_AR[2]);
		tracepdeex(2,trace, "%8.4f,%8.4f,%8.4f,",dENU_FL[0],dENU_FL[1],dENU_FL[2]);
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
	
	for (auto& obs : only<GObs>(obsList))
	{
		KFKey key;
		if ( opt.ionmod == +E_IonoMode::ESTIMATE )
		{
			key = {KF::IONO_STEC,obs.Sat, obs.mount, 0};
			double val;
			double var;
			if	(!kfState.getKFValue(key,val,&var)) 
				continue;
			
			obs.stecVal	= val;
			obs.stecVar	= var;
			obs.STECtype = 2;
			nion++;
		}
		else if ( opt.ionmod == +E_IonoMode::IONO_FREE_LINEAR_COMBO )
		{
			E_FType frq1;
			E_FType frq2;
			E_FType frq3;
			if (!satFreqs(obs.Sat.sys, frq1, frq2, frq3))
				continue;
			
			int wlamb =  retrv_WLambg(trace, E_AmbTyp::WL12,kfState.time,obs.mount,obs.Sat);
			if ( wlamb == INVALID_WLVAL) 
				continue;
			
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
			
			double alpha = SQR(CLIGHT) / TEC_CONSTANT;
			double c2 = alpha/(lam1-lam2);
			double c1 = -c2/(lam1+lam2);
			double c3 = alpha/lam1/lam2;
			double s1 = SQR(c1);
			double s2 = SQR(c2);
			double s3 = SQR(c3);
			
			obs.stecVal = c1*(L1-L2) + c2*(wlamb + wlbias) + c3*val;
			obs.stecVar = s1*(V2+V1) + s2*(        wlvari) + s3*val;
			obs.STECtype = 2;
			nion++;
		}
		else break;
	}
	return nion;
}

/** Querry Ginan 1.0 AR algorithms for phase biases, it does not check for specific codes just carrier frequencies 
returns false if bias is not found */
bool queryBiasWLNL(
	Trace& trace,		///< debug stream 
	SatSys sat,			///< satellite (for receiver biases, sat.sys needs to be set to the appropriate system, and sat.prn must be 0)
	string rec,			///< receiver  (for satellite biases nees to be "")
	E_FType ft, 		///< Signal frequency
	double& bias,		///< signal bias
	double& vari)		///< bias variance	
{
	E_Sys sys = sat.sys;
	
	E_FType frq1;
	E_FType frq2;
	E_FType frq3;
	if (!satFreqs(sys, frq1, frq2, frq3))
		return false;
				
	if	(  ft != frq1
		&& ft != frq2)
	{
		return false;
	}
	
	double lam_ = nav.satNavMap[sat].lamMap[ft];
	double lam1 = nav.satNavMap[sat].lamMap[frq1];
	double lam2 = nav.satNavMap[sat].lamMap[frq2];
	
	/* satellite bias */
	if (rec.empty())
	{
		if (SATbialist[E_AmbTyp::WL12].find(sat) == SATbialist[E_AmbTyp::WL12].end())					return false;
		if (SATbialist[E_AmbTyp::NL12].find(sat) == SATbialist[E_AmbTyp::NL12].end())					return false;
		
		double WLbia = SATbialist[E_AmbTyp::WL12][sat].outbias;
		double WLvar = SATbialist[E_AmbTyp::WL12][sat].outvari;
		double NLbia = SATbialist[E_AmbTyp::NL12][sat].outbias;
		double NLvar = SATbialist[E_AmbTyp::NL12][sat].outvari;
		
		double c1	 = lam_ / (lam2 - lam1);
		bias		 = lam_ * (NLbia - c1 * WLbia);
		vari		 = lam_*lam_ * (NLvar + c1*c1 * WLvar);
		return true;
	}
	
	/* receiver bias */ 
	if (sat.prn==0)
	{
		if (RECbialist[E_AmbTyp::WL12].find(sat.sys)      == RECbialist[E_AmbTyp::WL12].end())			return false;
		if (RECbialist[E_AmbTyp::WL12][sat.sys].find(rec) == RECbialist[E_AmbTyp::WL12][sys].end()) 	return false;
		if (RECbialist[E_AmbTyp::NL12].find(sat.sys)      == RECbialist[E_AmbTyp::NL12].end())			return false;
		if (RECbialist[E_AmbTyp::NL12][sat.sys].find(rec) == RECbialist[E_AmbTyp::NL12][sys].end()) 	return false;
		
		double WLbia = RECbialist[E_AmbTyp::WL12][sat.sys][rec].outbias;
		double WLvar = RECbialist[E_AmbTyp::WL12][sat.sys][rec].outvari;
		double NLbia = RECbialist[E_AmbTyp::NL12][sat.sys][rec].outbias;
		double NLvar = RECbialist[E_AmbTyp::NL12][sat.sys][rec].outvari;
		
		double c1	 = lam_ / (lam2 - lam1);
		bias		 = lam_ * (NLbia - c1 * WLbia);
		vari		 = lam_*lam_ * (NLvar + c1*c1 * WLvar);
		return true;
	}
	
	return false; /* Ginan 1.0 does not support hybrid biases */
}	
