#include "GNSSambres.hpp"

/** Comapring two Z-transformed ambiguities */
bool comp_Zamb(
	Z_Amb Z1,		///< Z-ambiguity 1
	Z_Amb Z2)		///< Z-ambiguity 2
{
	if ( Z1.size() != Z2.size())			return false;
	for( auto& [key,coef] : Z1)
	{
		if ( Z2.find(key) == Z2.end() ) 	return false;
		if ( Z2[key] != coef )				return false;
	}
	
	return true;
}

/** Check Z-transformed ambiguities and remove old entries */
void chk_arch( 
	Trace& trace,		///< Debug trace
	GTime time,			///< Time of solution
	GinAR_opt& opt )	///< AR control options
{
	auto& AR_mealist = ARstations[opt.recv].AR_meaMap;
	auto& ZAmb_list  = ARstations[opt.recv].ZAmb_archive;
	for ( auto it=ZAmb_list.begin(); it!= ZAmb_list.end(); )
	{
		bool keep = true;
		bool meas = true;
		Z_Amb      Zambigty = it->first;
		GinAR_amb& archived = it->second;
		
		if (archived.hld_epc < 0) keep = false;
		
		for (auto& [key,coef] : Zambigty )
		{
			E_AmbTyp typ = E_AmbTyp::NL12;
			if  ( opt.ionmod == +E_IonoMode::ESTIMATE )
			{
				E_FType frq1;
				E_FType frq2;
				E_FType frq3;
				sys_frq(key.Sat.sys, frq1, frq2, frq3);
				
				if (key.num == frq1)	typ = E_AmbTyp::UCL1;
				if (key.num == frq2)	typ = E_AmbTyp::UCL2;
				if (key.num == frq3)	typ = E_AmbTyp::UCL3;
				
			}
			KFKey key2 = {KF::AMBIGUITY,key.Sat,key.str,typ};
			
			if (AR_mealist.find(key2) == AR_mealist.end())		{ keep = false; break; }
			if (AR_mealist[key2].cyl_slp)						{ keep = false; break; }
			if (fabs(time == AR_mealist[key2].mea_fin) > DTTOL)	{ meas = false; }
		}
		
		if (meas)												archived.mea_fin = time;
		else if ((time-archived.mea_fin) > opt.Max_Hold_tim )			keep = false;
		
		if (archived.hld_epc++ > opt.Max_Hold_epc )					keep = false;
		
		if (keep)
		{
			it++;
		}
		else
		{
			tracepdeex(ARTRCLVL,trace, "\nARES_NLAR Discarding Zamb"); 
			it = ZAmb_list.erase(it);
		}
	}
}

/** Add Z-transformed ambiguity to archive */
void incl_Zamb( 
	Trace& trace,	///< Debug Trace
	GTime time, 	///< Solution time
	Z_Amb& Zin, 	///< Z-transformed ambiguity
	double amb,		///< (Integer) Value of ambiguity
	string recv)	///< receiver ID ("NETWORK" if processing the whole network)
{
	bool found = false;
	int ncoef = 0;
	
	auto& Zamb_list = ARstations[recv].ZAmb_archive;
	
	for (auto& [Zlst,lstamb] : Zamb_list)
	{
		if (comp_Zamb(Zin,Zlst))
		{
			/* it is assumed that the ambiguity has been validated, if not, add checks here */
			
			tracepdeex(ARTRCLVL+1,trace,"\n#ARES_NLAR Updating Zamb:" );
			for (auto& [key,coef] : Zin)
				tracepdeex(ARTRCLVL+1,trace," %+7.4f AMB(%s,%s)",coef,key.Sat.id().c_str(),key.str.c_str());
			
			double damb = fabs(amb-lstamb.flt_amb);
			if (damb > 0.1) 
				tracepdeex(ARTRCLVL,trace,"\n ... WARNING: ambiguity changed %.4f -> %.4f", lstamb.flt_amb, amb);
			
			lstamb.mea_fin = time;
			lstamb.fix_fin = time;
			lstamb.flt_amb = amb;
			lstamb.hld_epc = 0;
			lstamb.out_epc = 0;
			
			return;
		}
	}
	
	tracepdeex(ARTRCLVL+1,trace,"\n#ARES_NLAR Initializing Zamb:" );
	for (auto& [key,coef] : Zin)
		tracepdeex(ARTRCLVL+1,trace," %+7.4f AMB(%s,%s)",coef,key.Sat.id(),key.str);
	
	GinAR_amb newamb;
	newamb.sec_ini = time;
	newamb.mea_fin = time;
	newamb.fix_fin = time;
	newamb.flt_amb = amb;
	newamb.hld_epc = 0;	
	newamb.out_epc = 0;	
	Zamb_list[Zin] = newamb;
	
	return;
}

/** Update float ambiguity estimates */
int updat_ambigt( 
	Trace& trace,		///< Debug trace
	KFState& kfState,	///< KF struct containing raw ambiguity measurements
	GinAR_opt opt )		///< Ginan AR control options 
{
	tracepdeex(ARTRCLVL,trace,"\n#ARES_NLAR Resolving ambiguities %s", kfState.time.to_string(0));
	auto& AR_mealist = ARstations[opt.recv].AR_meaMap;
	
	chk_arch( trace, kfState.time, opt );
	
	VectorXd KF_x =  kfState.x;
	MatrixXd KF_P =  kfState.P;
	
	vector<int> 		AmbReadindx;
	map <KFKey, int>	AmbList;
	map <KFKey, int>	RecList;
	map <KFKey, int>	SatList;
	map <KFKey, double> WLcorrMap;
	GinAR_mtx   ambState;
		
	int nstates = 0;
	SatSys  sat0;
	for (auto& [key,indx] : kfState.kfIndexMap)
	{
		SatSys  sat = key.Sat;
		string  rec = key.str;
		E_Sys	sys = sat.sys;
		E_FType frq1;
		E_FType frq2;
		E_FType frq3;
		E_AmbTyp typ = E_AmbTyp::NL12;
		
		if (!sys_solve[sys])													continue;	
		if (!sys_frq(sys, frq1, frq2, frq3))									continue;
		if ( opt.ionmod == +E_IonoMode::ESTIMATE )
		{
			if(key.num == frq1) typ = E_AmbTyp::UCL1;
			if(key.num == frq2) typ = E_AmbTyp::UCL2;
			if(key.num == frq3) typ = E_AmbTyp::UCL3;
		}
		
		if ( opt.endu )
		{
			if (key.type != KF::PHASE_BIAS) 									continue;
		}
		else
		{
			if (key.type != KF::AMBIGUITY)  									continue;
			if (SATpivlist[typ][sys].find(sat) == SATpivlist[typ][sys].end())	continue;
		}
		if (RECpivlist[typ][sys].find(rec) == RECpivlist[typ][sys].end())		continue;
		
		KFKey key2 = {KF::AMBIGUITY, sat, rec, typ};
		if ( AR_mealist.find(key2) == AR_mealist.end() )						continue;
		if ( AR_mealist[key2].sat_ele < opt.MIN_Elev_AR )							continue;
		
		if ( opt.ionmod == +E_IonoMode::IONO_FREE_LINEAR_COMBO ){
			int WLamb = retrv_WLambg(trace, E_AmbTyp::WL12,kfState.time,rec,sat);
			if ( WLamb == INVALID_WLVAL )
			{
				tracepdeex(ARTRCLVL+2,trace,"\n#ARES_NLAR Failed to retrieve WL ambiguities for %s %s %s", kfState.time.to_string(0),rec,sat.id().c_str());
				continue;
			}
			double wlfct = opt.wlfact[sys];
			tracepdeex(ARTRCLVL+2,trace,"\n#ARES_NLAR Retrieved WL amb %s %s %s %d %f", kfState.time.to_string(0).c_str(),rec,sat.id().c_str(),WLamb,wlfct);
			WLcorrMap[key2] = wlfct*WLamb;
		}
		else WLcorrMap[key2] = 0;
		
		AmbReadindx.push_back(indx);
		ambState.ambmap[nstates] = key;
		AmbList[key2] = nstates++;
		
		sat0.sys = sys;
		key2 = {KF::REC_SYS_BIAS, sat0, rec, typ};
		RecList[key2] = 0;
		
		if ( !opt.endu )
		{
			key2 = {KF::PHASE_BIAS, sat, "", typ};
			SatList[key2] = 0;
		}
	}
	
	tracepdeex(ARTRCLVL,trace,"\n#ARES_NLAR Number of NL measurments: %d", nstates);
	
	if (nstates <= 0)
		return 0;
	
	int nambig = nstates;
	
	if (nambig <= (SatList.size()+RecList.size())) 
		return 0;
	
	if (!opt.endu) for(auto& [key,ind] : SatList)
		ind = nstates++;
		
	for (auto& [key,ind] : RecList) 
		ind = nstates++;
	
	tracepdeex(ARTRCLVL,trace,"\n#ARES_NLAR attempting AR with %4d measurements, %4d amb, %3d sat, %4d rec",nstates, nambig, SatList.size(), RecList.size());
	
	VectorXd raw_xmea = kfState.x(AmbReadindx);
	MatrixXd raw_Pmea = kfState.P(AmbReadindx,AmbReadindx);

	VectorXd xmea = VectorXd::Zero(nstates);
	MatrixXd Pmea = MatrixXd::Zero(nstates, nstates);
	MatrixXd Hinv = MatrixXd::Zero(nstates, nstates);
	
	VectorXd wmea = VectorXd::Zero(nambig);

	/* ambiguity measurements */
	xmea.head(nambig)                 = raw_xmea;
	Pmea.topLeftCorner(nambig,nambig) = raw_Pmea;
	for (auto& [key,ind] : AmbList)
	{
		xmea(ind) -= WLcorrMap[key];
		wmea(ind)  = WLcorrMap[key];
		
		E_Sys sys = key.Sat.sys;
		double NLwavlng = opt.wavlen[sys];
		
		Hinv(ind,ind) = NLwavlng;
		
		sat0.sys = key.Sat.sys;
		KFKey key2 = {KF::REC_SYS_BIAS, sat0, key.str, key.num};
		Hinv(ind,RecList[key2]) = NLwavlng;
		
		if (!opt.endu)
		{
			key2 = {KF::PHASE_BIAS, key.Sat, "", key.num};
			Hinv(ind,SatList[key2]) = NLwavlng;
		}
	}
	
	/* Pseudo-measurements from satellite pivots (one fixed ambiguity per satellite) */
	if (!opt.endu) 
	for (auto& [key,ind] : SatList)
	{
		E_AmbTyp typ = E_AmbTyp::_from_integral(key.num);
		string   rec = SATpivlist[typ][key.Sat.sys][key.Sat].pre_rec;
		double   amb = SATpivlist[typ][key.Sat.sys][key.Sat].rec_amb[rec];
		xmea(ind) = amb;
		Pmea(ind,ind) = FIXED_AMB_VAR;
		KFKey key2 = {KF::AMBIGUITY, key.Sat, rec, key.num};
		Hinv(ind,AmbList[key2]) = 1;
	}
	
	/* Pseudo-measurements from station pivots */
	for (auto& [key,ind] : RecList)
	{
		E_AmbTyp typ = E_AmbTyp::_from_integral(key.num);
		E_Sys sys = key.Sat.sys;
																				/* Rec bias = 0 for Anchor station */ 
		if ( key.str == AR_reflist[sys] )
		{
			xmea(ind) = 0;
			Pmea(ind,ind) = FIXED_AMB_VAR;
			Hinv(ind,ind) = 1;
		}
		else																	/* One fixed ambiguity per other stations */ 
		{
			SatSys   sat = RECpivlist[typ][sys][key.str].pre_sat;
			double   amb = RECpivlist[typ][sys][key.str].sat_amb[sat];
			
			KFKey key2 = {KF::AMBIGUITY, sat, key.str, key.num};
			xmea(ind) = amb;
			Pmea(ind,ind) = FIXED_AMB_VAR;
			Hinv(ind,AmbList[key2]) = 1;
		}
	}
	
	if (AR_VERBO)
	{
		for (auto& [key,ind] : AmbList)
			trace << std::endl << "#ARES_NLAR > " << key << " " << ind;
		
		if (!opt.endu)
		for(auto& [key,ind] : SatList)
			trace << std::endl << "#ARES_NLAR > " << key << " " << ind;
		
		for(auto& [key,ind] : RecList)
			trace << std::endl << "#ARES_NLAR > " << key << " " << ind;
		
		trace << std::endl << "Pmea: " << std::endl << Pmea 			<< std::endl;
		trace << std::endl << "xmea: " << std::endl << xmea.transpose() << std::endl;
		trace << std::endl << "wmea: " << std::endl << wmea.transpose() << std::endl;
		trace << std::endl << "Hinv: " << std::endl << Hinv 			<< std::endl;
	}
	
	MatrixXd Hflt = Hinv.inverse();
	VectorXd xflt = Hflt * xmea;												/* float solutions: ambiguities(1-namb), satellite bias, and receiver bias */
	MatrixXd Pflt = Hflt * Pmea * Hflt.transpose();								/* Variance of float solutions */
	
	for (auto& [keyd,ind] : AmbList)
	{
		AR_mealist[keyd].flt_amb = xflt(ind);
		AR_mealist[keyd].flt_var = Pflt(ind,ind);
	}
	
	ambState.aflt = xflt.head(nambig);											/* float ambiguities */
	ambState.Paflt= Pflt.topLeftCorner(nambig,nambig);							/* Variance of float ambiguities */
	
	if (AR_VERBO)
	{
		trace << std::endl << "Hflt: " << std::endl << Hflt << std::endl;
		trace << std::endl << "Pflt: " << std::endl << Pflt << std::endl;
		trace << std::endl << "xflt: " << std::endl << xflt.transpose() << std::endl;
	}
	
	int nfix = GNSS_AR(trace, ambState, opt);									/* AMBIGUITY RESOLUTION */
	
	if (nfix <= 0) 
		return 0;

	tracepdeex(ARTRCLVL, trace, "\n#ARES_NLAR %s Solved %4d ambiguities out of %4d", kfState.time.to_string(0), nfix, nambig);

	MatrixXd Ztrs = ambState.Ztrs;
	MatrixXd Sfix = Ztrs * ambState.Paflt * Ztrs.transpose();					/* Variance of z-transform of float ambiguities */
	
	for (int i = 0; i < nfix; i++) 
		Sfix(i, i) = Sfix(i, i) + FIXED_AMB_VAR; 								/* Avoiding negative variances */
	
	VectorXd zfix = ambState.zfix;												/* z-transform of fixed solutions */
	VectorXd yfix = zfix - Ztrs * ambState.aflt;
	MatrixXd Qfix = Sfix.inverse();

	MatrixXd Hfix = MatrixXd::Zero(nfix, nstates);
	Hfix.leftCols(nambig) = Ztrs;
	MatrixXd Kfix = Pflt * Hfix.transpose() * Qfix;
	
	VectorXd xfix = xflt + Kfix * yfix;											/* fixed solutions: ambiguities(1-namb), satellite bias, and receiver bias */
	MatrixXd Ista = MatrixXd::Identity(nstates,nstates);
	MatrixXd Pfix = (Ista - Kfix * Hfix).eval() * Pflt;							/* Variance of float solutions */

	if (AR_VERBO)
	{
		trace << std::endl << "Ztrs: " << std::endl << Ztrs << std::endl;
		trace << std::endl << "zfix: " << std::endl << zfix.transpose() << std::endl;
		trace << std::endl << "xfix: " << std::endl << xfix.transpose() << std::endl;
	}
	
	/* Store satellite biases */
	if (!opt.endu) for(auto& [key,ind] : SatList)
	{
		E_AmbTyp typ = E_AmbTyp::_from_integral(key.num);
		GinAR_bia& bia = SATbialist[typ][key.Sat];
		double val = xfix(ind);
	
		if (bia.numsamp == 0)	bia.intlevl = ROUND(val);
		else					bia.intlevl+= ROUND(val-bia.rawbias);
			
		bia.numsamp++;
		bia.rawbias = val;
		bia.outbias = val - bia.intlevl;
		bia.outvari = Pfix(ind,ind);
	}
	
	/* Store station biases */
	for (auto& [key,ind] : RecList)
	{
		E_AmbTyp typ = E_AmbTyp::_from_integral(key.num);
		GinAR_bia& bia = RECbialist[typ][key.Sat.sys][key.str];
		
		double val = xfix(ind);
	
		if (bia.numsamp == 0)	bia.intlevl = ROUND(val);
		else					bia.intlevl+= ROUND(val-bia.rawbias);
			
		bia.numsamp++;
		bia.rawbias = val;
		bia.outbias = val - bia.intlevl;
		bia.outvari = Pfix(ind,ind);
	}
	
	/* Store ambiguties */
	for(auto& [key,ind] : AmbList)
	{
		double val = xfix(ind);
		double var = Pfix(ind,ind);
		double vali = ROUND(val);
		
		if ( var < POSTAR_VAR && fabs(val-vali)<0.1 )
		{
			AR_mealist[key].fix_fin = kfState.time;
			AR_mealist[key].int_amb = (int) vali;
			AR_mealist[key].hld_epc = 0;
		}
	}

	MatrixXd Hsto = Ztrs * (Hflt.topRows(nambig)).eval();
	MatrixXd Asto = Hsto.leftCols(nambig);
	MatrixXd Bsto = Hsto.rightCols(nstates-nambig);

	VectorXd zsto = zfix - Bsto * (xmea.tail(nstates-nambig)).eval();
	if ( opt.ionmod == +E_IonoMode::IONO_FREE_LINEAR_COMBO ) 
		zsto += Asto*wmea;
	
	if (AR_VERBO)
	{
		trace << std::endl << "Asto: " << std::endl << Asto << std::endl;
		trace << std::endl << "wmea: " << std::endl << wmea.transpose() << std::endl;
		trace << std::endl << "zsto: " << std::endl << zsto.transpose() << std::endl;
	}
	
	for(int i = 0; i<nfix; i++)
	{
		Z_Amb Zsto;
		for(int j = 0; j<nambig; j++)
		{
			KFKey key = ambState.ambmap[j];
			double lam = opt.wavlen[key.Sat.sys];
			double Zcoef=ROUND(lam*Asto(i,j))/lam;
			
			if (Zcoef != 0)
				Zsto[key] = Zcoef;
		}
		if ( Zsto.empty() ) 
			continue;
		incl_Zamb( trace, kfState.time, Zsto, zsto(i), opt.recv );
	}
	return nfix;
}

/** Apply archived Z-ambiguities to float solution */
int  apply_ambigt( 
	Trace& trace,			///< Debug trace
	KFState& kfState,		///< KF containing float/fixed solutions
	GinAR_opt opt)			///< Ginan AR control options 
{
	auto& Zamb_list = ARstations[opt.recv].ZAmb_archive;
	tracepdeex( ARTRCLVL+1, trace, "\n#ARES_NLAR Retrieving stored ambiguities %d", Zamb_list.size() );
	KFMeasEntryList	measList;
	InitialState init;
	init.x = 0;
	init.P = 0;
	init.Q = 0;
	
	SatSys sat0 ={};
	int nind=0;
	
	map<Z_Amb, double>	Zapplied;
	vector<int> 		AmbReadindx;
	map<KFKey,int>		AmbList;
	int indH=0;
	
	for (auto& [Zamb, amb] : Zamb_list )
	{
		if ( (kfState.time-amb.mea_fin) > opt.Max_Hold_tim ) 
			continue;
		
		ObsKey obsKey = { sat0, "COMBIN", "Zamb", nind };
		KFMeasEntry	AmbMeas(&kfState, obsKey);
		
		AmbMeas.setValue(amb.flt_amb);
		AmbMeas.setNoise(FIXED_AMB_VAR);
		
		bool use = true;
		tracepdeex( ARTRCLVL, trace, "\n#ARES_NLAR Applying: " );
	
		for (auto& [key,coef] : Zamb )
		{
			if (kfState.kfIndexMap.find(key) == kfState.kfIndexMap.end()) 
			{
				use=false; 
				break;
			}
			
			AmbMeas.addDsgnEntry(key, coef, init);
			tracepdeex(ARTRCLVL, trace, "%+.2f x (%s,%s) ", coef, key.str.c_str(), key.Sat.id().c_str());
		}
		
		if (use)
		{
			measList.push_back(AmbMeas);
			
			Zapplied[Zamb] = amb.flt_amb;
			for (auto& [key,coef] : Zamb )
			if (AmbList.find(key)==AmbList.end())
			{
				int indKF = kfState.kfIndexMap[key];
				AmbReadindx.push_back(indKF);
				AmbList[key] = indH++;	
			}
			
			nind++;
			tracepdeex(ARTRCLVL, trace, "= %.2f", amb.flt_amb);
		}
	}
	
	if (nind<=0) 
		return 0;
	
	KFMeas combAmb = kfState.combineKFMeasList(measList);
	kfState.filterKalman(trace, combAmb, false);
	
	if(nind<5) return nind;
	
	/* Post fit residual check */	
	VectorXd x_post = kfState.x(AmbReadindx);
	VectorXd y_post = VectorXd::Zero(nind);
	MatrixXd H_post = MatrixXd::Zero(nind, indH);
	int indZ = 0;
	for (auto& [Zamb, amb] : Zapplied )
	{
		y_post(indZ) = amb;
		for (auto& [key,coef] : Zamb )
			H_post(indZ,AmbList[key]) = coef;
		indZ++;
	}
	VectorXd v_post = y_post-H_post*x_post;
	
	if (AR_VERBO)
	{
		trace << std::endl << "Hpost: " << std::endl << y_post << std::endl;
		trace << std::endl << "ypost: " << std::endl << y_post.transpose() << std::endl;
		trace << std::endl << "vpost: " << std::endl << v_post.transpose() << std::endl;
	}
	
	indZ = 0;
	if(opt.endu)
	for (auto& [Zamb, amb] : Zapplied )
	{
		if(fabs(v_post(indZ)) > 0.5)
		{
			tracepdeex( ARTRCLVL, trace, "\n#ARES_NLAR High postfit residual for: " );
			for (auto& [key,coef] : Zamb )
				tracepdeex(ARTRCLVL, trace, "%+.2f x (%s,%s) ", coef, key.str.c_str(), key.Sat.id().c_str());
			tracepdeex( ARTRCLVL, trace, "removing from list" );
		
			Zamb_list.erase(Zamb);
		}
		indZ++;
	}
	
	return nind;
}