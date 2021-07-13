#include "GNSSambres.hpp"
#include <math.h>

#define LOG_PI          1.14472988584940017
#define SQRT2           1.41421356237309510
#define AMB_RANG		10

int artrclvl = 4;

double round_perr (double dx, double var)
{
	if (var < 1e-20) return 0.0;

	double p0 = 0.0;
	double fact = -1.0 / 4.0 / var;

	for (int i = 1; i < AMB_RANG; i++)
	{
		p0 += exp((i + 2.0 * dx) * i * fact);
		p0 += exp((i - 2.0 * dx) * i * fact);
	}

	return p0 / (1.0 + p0);
}

int simple_round(Trace& trace, ARState* ambc)
{
	MatrixXd P = ambc->Paflt;
	VectorXd ret = ambc->aflt;
	int namb = ret.size(), nfix = 0;

	ambc->Ztrs.resize(0, 0);
	ambc->zfix.resize(0);

	if (namb <= 0) return 0;

	double ratthr = 1.0 / (ambc->ratthr + 1.0);
	double sucthr = 1 - pow(ambc->sucthr, 1.0 / namb);
	tracepdeex(artrclvl+1, trace, "\n#ARES_RND Using integer rounding ... %.4e  %.4f", sucthr, ratthr);

	vector <int> zind;
	vector <int> xind;
	xind.reserve(namb);

	for (int i = 0; i < namb; i++)
	{
		xind.push_back(i);
		double dv  = ret(i) - ROUND(ret(i));
		double perr = round_perr (dv, P(i, i));

		tracepdeex(artrclvl+1, trace, "\n#ARES_RND AR for %s %s : %6.3f %.4e %.4e", ambc->ambmap[i].str, ambc->ambmap[i].Sat.id().c_str(), dv, perr, P(i, i) );

		if (fabs(dv) < ratthr && perr < sucthr)
		{
			tracepdeex(4, trace, " ...  fixed");
			ret(i) = ret(i) - dv;
			nfix++;
			zind.push_back(i);
		}
	}

	MatrixXd Z = MatrixXd::Identity(namb, namb);

	if (nfix == 0) return 0;

	ambc->Ztrs = Z(zind, xind);
	ambc->zfix = ret(zind);

	return nfix;
}

int interat_round(Trace& trace, ARState* ambc)
{
	MatrixXd P = ambc->Paflt;
	VectorXd x = ambc->aflt;
	int namb = x.size();

	ambc->Ztrs.resize(0, 0);
	ambc->zfix.resize(0);

	if (namb <= 0) return 0;

	double sucthr = 1 - pow(ambc->sucthr, 1.0 / namb);
	double ratthr = 1.0 / (ambc->ratthr + 1.0);
	tracepdeex(artrclvl+1, trace, "\n#ARES_RND Using iterative rounding ... %.4e  %.4f", sucthr, ratthr);

	vector <int> zind;
	vector <int> xind;
	xind.reserve(namb);

	for (int i = 0; i < namb; i++) xind.push_back(i);

	MatrixXd I = MatrixXd::Identity(namb, namb);
	MatrixXd Ztrs;
	VectorXd xfix;
	VectorXd dvvct = VectorXd::Zero(namb);

	int nfix = 0, nnew = 0;

	for (int iter = 0; iter < ambc->nitr; iter++)
	{
		zind.clear();
		nnew = 0;

		for (int i = 0; i < namb; i++)
		{
			double dv  = x(i) - ROUND(x(i));
			double perr = round_perr (dv, P(i, i));

			tracepdeex(artrclvl+1, trace, "\n#ARES_RND %1d AR for %s %s : %6.3f %12.4e %12.4e", iter, ambc->ambmap[i].str, ambc->ambmap[i].Sat.id().c_str(), dv, perr, P(i, i));

			if ((fabs(dv) < ratthr) && (perr < sucthr))
			{
				tracepdeex(4, trace, " ...  fixed");
				nnew++;
				zind.push_back(i);
				dvvct(i) = dv;
			}
		}

		if (nnew <= nfix) break;

		VectorXd dx = dvvct(zind);

		Ztrs = I(zind, xind);
		xfix = x(zind) - dx;

		MatrixXd Psel = P(zind, zind) + (FIXED_AMB_VAR * I(zind, zind));
		MatrixXd K = P(xind, zind) * Psel.inverse();
		MatrixXd S = K * P(zind, xind);
		x = x - K * dx;
		P = P - S;
		nfix = nnew;
	}

	if (nfix == 0)return 0;

	ambc->Ztrs = Ztrs;
	ambc->zfix = xfix;

	return nfix;
}


int Ztrans_reduction(Trace& trace, ARState* ambc)
{
	int siz = ambc->aflt.size();
	int info = 0;
	int nhigh = 0;
	MatrixXd modul = MatrixXd::Identity(siz, siz);


	LDLT<MatrixXd> ldlt_;
	ldlt_.compute(ambc->Paflt);

	if (ldlt_.isPositive() == false)
	{
		tracepdeex(1, trace, "WARNING: LD decomposition error, ambiguity matrix may not positive definite\n");
		return -1;
	}

	MatrixXd L_mtrx = ldlt_.matrixL();
	VectorXd D_vect = ldlt_.vectorD();

	auto tr = ldlt_.transpositionsP ();
	MatrixXd Ztrans = tr * modul;
	VectorXd z_vect = tr * ambc->aflt;

	if (artrclvl < 3)
	{
		trace << std::endl << "x =" << std::endl << ambc->aflt.transpose() << std::endl;
		trace << std::endl << "Px=" << std::endl << ambc->Paflt            << std::endl;
		trace << std::endl << "Lx=" << std::endl << L_mtrx                 << std::endl;
		trace << std::endl << "D =" << std::endl << D_vect.transpose()     << std::endl;
		trace << std::endl << "T= " << std::endl << Ztrans                 << std::endl;

	}

	for (int j = siz - 2; j >= 0; j--)
	{
		for (int i = j + 1; i < siz; i++)
		{
			double mu = ROUND(L_mtrx(i, j));

			if (mu != 0)
			{
				nhigh++;
				L_mtrx.row(i) -= mu * L_mtrx.row(j);
				Ztrans.row(i) -= mu * Ztrans.row(j);
				z_vect    (i) -= mu * z_vect(j);
			}
		}

	}

	ambc->zflt = z_vect;
	ambc->Ztrs = Ztrans;
	ambc->Ltrs = L_mtrx;
	ambc->Dtrs = D_vect;

	if (artrclvl < 3)
	{
		trace << std::endl << "Zt=" << std::endl << Ztrans                 << std::endl;
		trace << std::endl << "Lz=" << std::endl << L_mtrx                 << std::endl;
		trace << std::endl << "z =" << std::endl << ambc->zflt.transpose() << std::endl;
	}

	return nhigh;
}

int integer_bootst(Trace& trace, ARState* ambc)
{
	tracepdeex(artrclvl, trace, "\n#WLR Using integer bootstrapping ... ");

	int info = Ztrans_reduction(trace, ambc);

	if (info < 0)
		return 0;

	double ratt = 1.0 / (ambc->ratthr + 1);
	double suct = ambc->sucthr;

	tracepdeex(artrclvl, trace, " %.4e %.4e",ratt,1.0-suct);

	MatrixXd Z = ambc->Ztrs;
	MatrixXd L = ambc->Ltrs;
	VectorXd D = ambc->Dtrs;
	VectorXd zflt = ambc->zflt;

	vector<int> xind;

	for (int j = 0; j < zflt.size(); j++) 
		xind.push_back(j);

	int zsiz = zflt.size();
	double succ = 1;
	int i = zsiz - 1;
	VectorXd zfix = VectorXd::Zero(zsiz);
	VectorXd zadj = zflt;

	vector<int> zind;

	while (succ > suct && i >= 0)
	{
		succ *= erf(sqrt(1 / (8 * D(i))));

		if (succ < suct)
			break;

		zfix(i) = ROUND(zadj(i));

		if (fabs(zfix(i) - zadj(i)) > ratt) /* skip ambiguities with high biases */
		{
			i--;
			continue;
		}

		zind.push_back(i);

		if (i == 0) 
			break;

		zadj.head(i - 1) += (zfix(i) - zadj(i)) * L.block(i, 0, 1, i - 1);
	}

	ambc->Ztrs = Z(zind, xind);
	ambc->zfix = zfix(zind);

	return zind.size();
}


int lambda_search(Trace& trace, ARState* ambc, int opt)
{
	tracepdeex(artrclvl, trace, "\n Using lambda search ... %.4f ",1.0-ambc->sucthr);
	int info = Ztrans_reduction(trace, ambc);

	if (info < 0)
	{
		tracepdeex(2, trace, "\n Matrix decorrelation failed ... ");
		return 0;
	}

	int nmax = ambc->Dtrs.size();
	int k = nmax - 1;

	double succ = erf(sqrt(1 / (8 * ambc->Dtrs(k--))));
	
	if (succ < ambc->sucthr) 
		return 0;

	int zsiz = 0;

	while (k >= 0 && succ >= ambc->sucthr)
	{
		succ *= erf(sqrt(1 / (8 * ambc->Dtrs(k--))));
		zsiz++;
	}

	tracepdeex(artrclvl, trace, " %d ambiguities passed success the rate test ... ", zsiz);

	if (zsiz < 4) 
		return 0;

	/*auto& zfixList = ambc->biemap;
	zfixList.clear();*/

	map<double, VectorXd> zfixList;

	MatrixXd Z = ambc->Ztrs.bottomRows(zsiz);
	MatrixXd L = ambc->Ltrs.bottomRightCorner(zsiz, zsiz);
	VectorXd D = ambc->Dtrs.tail(zsiz);
	VectorXd zflt = ambc->zflt.tail(zsiz);
	vector<int> xind;

	for (int i = 0; i < nmax; i++) 
		xind.push_back(i);

	MatrixXd Sadj = MatrixXd::Zero(zsiz, zsiz);
	VectorXd dist = VectorXd::Zero(zsiz);
	VectorXd zadj = VectorXd::Zero(zsiz);
	VectorXd zfix = VectorXd::Zero(zsiz);
	VectorXd step = VectorXd::Zero(zsiz);

	k = zsiz - 1;
	zadj(k) = zflt(k);
	zfix(k) = ROUND(zadj(k));
	double zdif = zadj(k) - zfix(k);
	step(k) = zdif < 0 ? -1 : 1;
	bool search = true;
	double maxdist = 1e99;
	int ncand = 0;
	
	while (search)
	{
		double newdist = dist(k) + zdif * zdif / D(k);

		if (newdist < maxdist)
		{
			if (k != 0)
			{
				k--;
				dist(k) = newdist;
				Sadj.block(k, 0, 1, k + 1) = (Sadj.block(k + 1, 0, 1, k + 1)).eval() + (zfix(k + 1) - zadj(k + 1)) * L.block(k + 1, 0, 1, k + 1);
				zadj(k) = zflt(k) + Sadj(k, k);
				zfix(k) = ROUND(zadj(k));
				zdif = zadj(k) - zfix(k);
				step(k) = zdif < 0 ? -1 : 1;
			}
			else
			{
				//trace << std::endl << "Candidate found:" << zfix.transpose() << ";   dist= " << newdist;
				zfixList[newdist] = zfix;
				ncand = zfixList.size();
				double maxd = newdist * ambc->ratthr;

				if (maxd < maxdist) maxdist = maxd;

				if (ambc->nset>0 && (ncand >= ambc->nset))
				{
					if (opt < 2)
					{
						int ntot = 0;
						maxd = 0;

						for ( auto& [dis, zcand] : zfixList )
						{
							if (ambc->nset>0 && (ntot++ < ambc->nset)) 
								if 		(dis > maxd ) 		maxd = dis;
								else if (dis < maxd) 		maxd = dis;
						}

						if (maxd < maxdist) maxdist = maxd;
					}

					for (auto it = zfixList.begin(); it != zfixList.end(); )
					{
						if (it->first > maxdist)
							it = zfixList.erase(it);
						else 
							++it;
					}

					ncand = zfixList.size();
				}

				zfix(0) += step(0);
				zdif = zadj(0) - zfix(0);
				step(0) = -step(0) + (zdif < 0 ? -1 : 1);
			}
		}
		else
		{
			if (k == (zsiz - 1)) break;
			else
			{
				k++;
				zfix(k) += step(k);
				zdif = zadj(k) - zfix(k);
				step(k) = -step(k) + (zdif < 0 ? -1 : 1);
			}
		}
	}

	if (zfixList.size() < 1) 
		return 0;

	double mindist = zfixList.begin()->first;
	VectorXd zfix0 = zfixList.begin()->second;
	ambc->zfix = zfix0;
	ambc->Ztrs = Z;

	if (zfixList.size() == 1)
		return zfix0.size();

	if ( opt == 0 )
		return 0;

	/*
		{
			auto it=zfixList.begin();
			it++;
			if((it->first/mindist)<ambc->ratthr) return 0;
			else return zfix0.size();
		}
	*/
	if (opt == 1 && (maxdist / mindist) < ambc->ratthr) 
		return 0;

	if (opt == 3)
	{
		MatrixXd Pbie = Z * ambc->Paflt * Z.transpose();
		MatrixXd Qbie = Pbie.inverse();
		double acum = 0;

		for (auto& [dis, fixvec] : zfixList)
		{
			VectorXd ybie = fixvec - zflt;
			double dbie = ybie.transpose() * Qbie * ybie;
			double fct  = exp(-0.5 * sqrt(dbie));
			acum += fct;
		}

		VectorXd zbie = VectorXd::Zero(zsiz);

		for (auto& [dis, fixvec] : zfixList)
		{
			VectorXd ybie = fixvec - zflt;
			double dbie = ybie.transpose() * Qbie * ybie;
			double fct  = exp(-0.5 * sqrt(dbie)) / acum;
			zbie += fct * fixvec;
		}

		ambc->zfix = zbie;
		
		/*int nsolv_bie=0;
		for(int l=0;l<zbie.size();l++)
		{
			double zreal=zbie(l);
			double zintg=ROUND(zreal);
			if(fabz(zreal-zintg)<0.1)nsolv_bie++;
		}
		return nsolv_bie; */
		return zbie.size();
	}

	for (auto& [dis, fixvec] : zfixList)
	{
		//trace << std::endl << " zcand: " << fixvec.transpose() << "; dist= " << dis << "/" <<maxdist;
		if (mindist < 0)
		{
			mindist = dis;
			zfix0 = fixvec;
		}
		else if ((dis / mindist) > ambc->ratthr) 
			break;
		else
		{
			for (int l = 0; l < zfix0.size(); l++)
			{
				if (zfix0(l) == -99999.5) 
					continue;

				if (zfix0(l) != fixvec(l))
					zfix0(l) = -99999.5;
			}
		}
	}

	tracepdeex(2, trace, "\n %d candidates selected ",	zfixList.size());
	vector<int> zind;

	for (int k = 0; k < zfix0.size(); k++) 
	if (zfix0(k) != -99999.5)
	{
		zind.push_back(k);

	}

	tracepdeex(2, trace, "... %d ambiguties in common\n", zind.size());


	ambc->zfix = zfix0(zind);
	ambc->Ztrs = Z(zind, xind);

	return zind.size();

}

int GNSS_AR(Trace& trace, ARState* ambc)
{
	switch (ambc->mode)
	{
		case E_ARmode::ROUND:				return simple_round		(trace, ambc);
		case E_ARmode::ITER_RND:			return interat_round	(trace, ambc);
		case E_ARmode::BOOTST:				return integer_bootst	(trace, ambc);
		case E_ARmode::LAMBDA:				return lambda_search	(trace, ambc, 0);
		case E_ARmode::LAMBDA_ALT:			return lambda_search	(trace, ambc, 2);
		case E_ARmode::LAMBDA_AL2:			return lambda_search	(trace, ambc, 1);
		case E_ARmode::LAMBDA_BIE:			return lambda_search	(trace, ambc, 3);
		default:							tracepdeex(1, trace, "\n AR mode not supported \n");
	}

	return 0;
}
