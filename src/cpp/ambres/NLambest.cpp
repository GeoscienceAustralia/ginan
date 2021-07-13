#include "GNSSambres.hpp"

int nltrclvl=3;
int NLambEstm( Trace& trace, KFState& kfState, ARState& ambState)
{
	int epoc = kfState.time.time;
	int nfix = 0;

	VectorXd xKF = kfState.x;
	MatrixXd PKF = kfState.P;

	list<KFKey> Keylist;
	vector<int> indices;
	list<KFKey> amblist;
	map<KFKey, int> satlist;
	map<KFKey, int> stalist;

	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		E_Sys sys = kfKey.Sat.sys;
		auto& sysdat = StatAmbMap_list[sys];
		if ( ambState.endu && kfKey.type != KF::PHASE_BIAS) 			continue;
		if (!ambState.endu && kfKey.type != KF::AMBIGUITY)  			continue;
		if (!sys_solve[sys]) 											continue;
		if (sysdat.find(kfKey.str) == sysdat.end()) 					continue;

		auto& stadat = sysdat[kfKey.str];

		if (stadat.SignList.find(kfKey.Sat) == stadat.SignList.end()) 	continue;

		SignAmbg& sigamb = stadat.SignList[kfKey.Sat];

		if (sigamb.state < 2) 											continue;

		Keylist.push_back(kfKey);
		indices.push_back(index);

		if (sigamb.pivot)
		{

			if (!ambState.endu)
			{
				KFKey satKey = { KF::PHASE_BIAS, kfKey.Sat, "",	kfKey.num};
				satlist[satKey]++;

			}

			if (ambState.endu || kfKey.str != ARrefsta)
			{
				SatSys sat0;
				sat0.sys = sys;
				sat0.prn = 0;
				KFKey recKey = {KF::REC_SYS_BIAS, sat0, kfKey.str, kfKey.num};
				stalist[recKey]++;
			}
		}
		else
		{
			KFKey ambkey = {KF::AMBIGUITY,	kfKey.Sat, kfKey.str, kfKey.num};
			amblist.push_back(ambkey);
		}
	}

	VectorXd NLmeas    = xKF(indices);
	MatrixXd NLmeasVar = PKF(indices, indices);
	int siz = NLmeas.size();
	int ntot = xKF.size();

	MatrixXd Hsel = MatrixXd::Zero(siz, ntot);
	MatrixXd Hflt = MatrixXd::Zero(siz, siz);

	map<KFKey, int> NLambind;
	int nmea = 0;

	for (auto& kfKey : amblist) 
		NLambind[kfKey] = nmea++;

	int namb = nmea;

	if (namb == 0) 
		return 0;

	for (auto& [kfKey, stanum] : satlist)
		NLambind[kfKey] = nmea++;

	int nsat = nmea - namb;

	for (auto& [kfKey, satnum] : stalist) 
		NLambind[kfKey] = nmea++;

	int nsta = nmea - namb - nsat;

	if (siz != nmea)
	{
		tracepdeex(2, trace, "#ARES_NLR WARNING ambiguity modelling matrix is not invertible %d %d %d %d %d\n", siz, nmea, namb, nsat, nsta);
		return 0;
	}

	tracepdeex(nltrclvl, trace, "#ARES_NLR ambiguity modelling matrix built %d %d %d %d\n", nmea, namb, nsat, nsta);

	int i = 0;

	for (auto& kfKey : Keylist)
	{
		StatAmbg& stadat = StatAmbMap_list[kfKey.Sat.sys][kfKey.str];
		SignAmbg& sigamb = stadat.SignList[kfKey.Sat];

		Hsel(i, indices[i]) = 1.0;
		NLmeas(i) -= satpiv[kfKey.Sat].WLinIF * sigamb.fix.WL12;
		sigamb.raw.NL12 = NLmeas(i) / satpiv[kfKey.Sat].NLwav;

		if (sigamb.pivot)
		{
			NLmeas(i) -= satpiv[kfKey.Sat].NLwav * sigamb.fix.NL12;
		}
		else
		{
			KFKey ambkey = {KF::AMBIGUITY,	kfKey.Sat, kfKey.str, kfKey.num};
			Hflt(i, NLambind[ambkey]) = satpiv[kfKey.Sat].NLwav;
		}

		if (!ambState.endu)
		{
			KFKey satKey = {KF::PHASE_BIAS,	kfKey.Sat, {},	kfKey.num};
			Hflt(i, NLambind[satKey]) = satpiv[kfKey.Sat].NLwav;
		}

		SatSys sat0;
		sat0.sys = kfKey.Sat.sys;
		sat0.prn = 0;

		if (kfKey.str != ARrefsta)
		{
			KFKey recKey = {KF::REC_SYS_BIAS, sat0, kfKey.str, kfKey.num};
			Hflt(i, NLambind[recKey]) = satpiv[kfKey.Sat].NLwav;
		}

		tracepdeex(nltrclvl, trace, "#ARES_NLR, meas, %10d, %s, %s, %d, %12.4f, %12.4f, %12.4f, %12.4f\n", epoc, kfKey.str, kfKey.Sat.id().c_str(), sigamb.pivot ? 1 : 0,
				sigamb.raw.NL12, satpiv[kfKey.Sat].WLinIF * sigamb.fix.WL12, satpiv[kfKey.Sat].NLwav * sigamb.fix.NL12, NLmeas(i));
		i++;
	}

	MatrixXd Hinv = Hflt.inverse();
	VectorXd xflt = Hinv * NLmeas;
	MatrixXd Pxflt = Hinv * NLmeasVar * Hinv.transpose();

	vector<int> indexA;
	vector<int> indexB;
	indexA.reserve(namb);
	indexB.reserve(nmea - namb);
	map<int, KFKey> biasmap_out;
	map<int, KFKey> ambmap_out;

	ambState.ambmap.clear();
	i = 0;

	for (auto& [kfKey, index] : NLambind)
	{
		double val = xflt(index);
		double var = Pxflt(index, index);

		if (kfKey.type == KF::AMBIGUITY)
		{
			StatAmbg& stadat = StatAmbMap_list[kfKey.Sat.sys][kfKey.str];
			stadat.SignList[kfKey.Sat].flt.NL12 = val;
			stadat.SignList[kfKey.Sat].flt.NL12var = var;
			ambState.ambmap[i++] = kfKey;
			indexA.push_back(index);
		}

		if (kfKey.type == KF::PHASE_BIAS)
		{
			satpiv[kfKey.Sat].satbias.NL12 = val;
			satpiv[kfKey.Sat].satbias.NL12var = var;
			indexB.push_back(index);
		}

		if (kfKey.type == KF::REC_SYS_BIAS)
		{
			StatAmbg& stadat = StatAmbMap_list[kfKey.Sat.sys][kfKey.str];
			stadat.stabias.NL12 = val;
			stadat.stabias.NL12var = var;
			indexB.push_back(index);
		}

		string type	= KF::_from_integral(kfKey.type)._to_string();
		tracepdeex(nltrclvl, trace, "\n#ARES_NLR, float, %4d, %14s, %5s, %3s, %3d, %13.4f, %.9e", index, type, kfKey.str, kfKey.Sat.id().c_str(), kfKey.num, val, var);
	}

	ambState.aflt = xflt(indexA);
	ambState.Paflt = Pxflt(indexA, indexA);
	nfix = GNSS_AR(trace, &ambState);

	if (nfix <= 0) return 0;

	tracepdeex(2, trace, "\n#ARES_NLR Number of fixed ambiguities: %d\n", nfix);

	VectorXd zfix = ambState.zfix;
	MatrixXd Ztrs = ambState.Ztrs;

	VectorXd yfix = zfix - Ztrs * ambState.aflt;
	MatrixXd Sfix = Ztrs * ambState.Paflt * Ztrs.transpose();

	for (int i = 0; i < nfix; i++) Sfix(i, i) = Sfix(i, i) + FIXED_AMB_VAR; /* avoiding negative variances */

	MatrixXd Qfix = Sfix.inverse();

	MatrixXd zer  = MatrixXd::Zero(nfix, nmea - namb);
	MatrixXd Hfix(nfix, nmea);
	Hfix << Ztrs, zer;
	MatrixXd Kfix = Pxflt * Hfix.transpose() * Qfix;
	VectorXd xfix = xflt + Kfix * yfix;
	MatrixXd Pxfix = (MatrixXd::Identity(nmea, nmea) - Kfix * Hfix).eval() * Pxflt;

	for (auto& [kfKey, index] : NLambind)
	{
		double val = xfix(index);
		double var = Pxfix(index, index);

		if (kfKey.type == KF::AMBIGUITY)
		{
			SignAmbg& sigamb = StatAmbMap_list[kfKey.Sat.sys][kfKey.str].SignList[kfKey.Sat];
			sigamb.fix.NL12 = val;
			sigamb.fix.NL12var = var;
			double vali = 1.0 * ROUND(val);

			if (var < POSTAR_VAR && fabs(val - vali) < 0.01)
			{
				sigamb.state |= 4;
				sigamb.fix.NL12 = vali;
				sigamb.fix.NL12var = 0.0;
			}
			else 
				sigamb.state &= 3;
		}

		if (kfKey.type == KF::PHASE_BIAS)
		{
			satpiv[kfKey.Sat].satbias_fix.NL12    = val;
			satpiv[kfKey.Sat].satbias_fix.NL12var = var;
		}

		if (kfKey.type == KF::REC_SYS_BIAS)
		{
			StatAmbMap_list[kfKey.Sat.sys][kfKey.str].stabias_fix.NL12    = val;
			StatAmbMap_list[kfKey.Sat.sys][kfKey.str].stabias_fix.NL12var = var;
		}

		string type	= KF::_from_integral(kfKey.type)._to_string();
		tracepdeex(nltrclvl, trace, "#ARES_NLR, fixed, %4d, %20s, %5s, %3s, %3d, %13.4f, %.9e\n", index, type, kfKey.str, kfKey.Sat.id().c_str(), kfKey.num, val, var);
	}

	MatrixXd Hout = Hfix * Hinv;
	VectorXd yout = zfix - Hout * NLmeas;
	MatrixXd Sout = Hout * NLmeasVar * Hout.transpose();

	for (int i = 0; i < nfix; i++) 
		Sout(i, i) = Sout(i, i) + FIXED_AMB_VAR; /* avoiding negative variances */

	MatrixXd Qout = Sout.inverse();
	Hout = Hout * Hsel;
	MatrixXd Kout = PKF *  Hout.transpose() * Qout;
	xKF = (xKF + Kout * yout).eval();
	PKF = ((MatrixXd::Identity(ntot, ntot) - Kout * Hout).eval() * PKF).eval() ;

	kfState.x = xKF;
	kfState.P = PKF;
	return nfix;
}
