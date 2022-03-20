#include "GNSSambres.hpp"
#include <math.h>

#define LOG_PI          1.14472988584940017
#define SQRT2           1.41421356237309510
#define AMB_RANG		10

bool 	AR_VERBO = false;

/** Probability of error (assuming normal distribution) */
double round_perr (
	double dx,		///< Distance between value and mean
	double var)		///< Variance
{
	if (var < 1e-20) 
		return 0;

	double p0 = 0;
	double fact = -0.25 / var;

	for (int i = 1; i < AMB_RANG; i++)
	{
		p0 += exp((i + 2 * dx) * i * fact);
		p0 += exp((i - 2 * dx) * i * fact);
	}

	return p0 / (p0 + 1);
}

/** Simple integer Rounding */
int simple_round(
	Trace& trace,		///< Debug trace
	GinAR_mtx& mtrx,	///< Reference to structure containing float values and covariance
	GinAR_opt opt)		///< Object containing processing options
{
	MatrixXd P = mtrx.Paflt;
	VectorXd ret = mtrx.aflt;
	int namb = ret.size(), nfix = 0;

	mtrx.Ztrs.resize(0, 0);
	mtrx.zfix.resize(0);

	if (namb <= 0) 
		return 0;

	double ratthr = 1 / (opt.ratthr + 1);
	double sucthr = 1 - pow(opt.sucthr, 1.0 / namb);
	tracepdeex(ARTRCLVL+1, trace, "\n#ARES_RND Using integer rounding ... %.4e  %.4f", sucthr, ratthr);

	vector <int> zind;
	vector <int> xind;
	xind.reserve(namb);

	for (int i = 0; i < namb; i++)
	{
		xind.push_back(i);
		double dv  = ret(i) - ROUND(ret(i));
		double perr = round_perr (dv, P(i, i));

		if (fabs(dv) < ratthr && perr < sucthr)
		{
			tracepdeex(4, trace, " ...  fixed");
			ret(i) = ret(i) - dv;
			nfix++;
			zind.push_back(i);
		}
	}

	MatrixXd Z = MatrixXd::Identity(namb, namb);

	if (nfix == 0) 
		return 0;

	mtrx.Ztrs = Z(zind, xind);
	mtrx.zfix = ret(zind);

	return nfix;
}

/** Iterative Rounding */
int interat_round(
	Trace& trace,		///< Debug trace
	GinAR_mtx& mtrx,	///< Reference to structure containing float values and covariance
	GinAR_opt opt)		///< Object containing processing options
{
	MatrixXd P = mtrx.Paflt;
	VectorXd x = mtrx.aflt;
	int namb = x.size();

	mtrx.Ztrs.resize(0, 0);
	mtrx.zfix.resize(0);

	if (namb <= 0) 
		return 0;

	double sucthr = 1 - pow(opt.sucthr, 1.0 / namb);
	double ratthr = 1 / (opt.ratthr + 1);
	
	vector <int> zind;
	vector <int> xind;
	xind.reserve(namb);

	for (int i = 0; i < namb; i++) 
		xind.push_back(i);

	MatrixXd I = MatrixXd::Identity(namb, namb);
	MatrixXd Ztrs;
	VectorXd xfix;
	VectorXd dvvct = VectorXd::Zero(namb);

	int nfix = 0;
	int nnew = 0;

	for (int iter = 0; iter < opt.nitr; iter++)
	{
		zind.clear();
		nnew = 0;

		for (int i = 0; i < namb; i++)
		{
			double dv  = x(i) - ROUND(x(i));
			double perr = round_perr (dv, P(i, i));

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

	if (nfix == 0)
		return 0;

	mtrx.Ztrs = Ztrs;
	mtrx.zfix = xfix;

	return nfix;
}

/** Lambda decorrelation (trough Z transform) */
int Ztrans_reduction(
	Trace& trace,		///< Debug trace
	GinAR_mtx& mtrx)	///< Reference to structure containing float values and covariance
{
	int siz = mtrx.aflt.size();
	int nhigh = 0;
	MatrixXd modul = MatrixXd::Identity(siz, siz);

	LDLT<MatrixXd> ldlt_;
	ldlt_.compute(mtrx.Paflt);

	if (ldlt_.isPositive() == false)
	{
		tracepdeex(1, trace, "WARNING: LD decomposition error, ambiguity matrix may not positive definite\n");
		return -1;
	}

	MatrixXd L_mtrx = ldlt_.matrixL();
	VectorXd D_vect = ldlt_.vectorD();

	auto tr = ldlt_.transpositionsP ();
	MatrixXd Ztrans = tr * modul;
	VectorXd z_vect = tr * mtrx.aflt;

	if (AR_VERBO)
	{
		trace << std::endl << "x =" << std::endl << mtrx.aflt.transpose() << std::endl;
		trace << std::endl << "Px=" << std::endl << mtrx.Paflt            << std::endl;
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

	mtrx.zflt = z_vect;
	mtrx.Ztrs = Ztrans;
	mtrx.Ltrs = L_mtrx;
	mtrx.Dtrs = D_vect;

	if (AR_VERBO)
	{
		trace << std::endl << "Zt=" << std::endl << Ztrans                 << std::endl;
		trace << std::endl << "Lz=" << std::endl << L_mtrx                 << std::endl;
		trace << std::endl << "z =" << std::endl << mtrx.zflt.transpose() << std::endl;
		trace << std::endl << "Dz=" << std::endl << mtrx.Dtrs.transpose() << std::endl;
	}

	return nhigh;
}

/** Integer bootstrapping */
int integer_bootst(
	Trace& trace,		///< Debug trace
	GinAR_mtx& mtrx,	///< Reference to structure containing float values and covariance
	GinAR_opt opt)		///< Object containing processing options
{
	int info = Ztrans_reduction(trace, mtrx);

	if (info < 0)
		return 0;

	double ratt = 1 / (opt.ratthr + 1);
	double suct = opt.sucthr;

	MatrixXd Z = mtrx.Ztrs;
	MatrixXd L = mtrx.Ltrs;
	VectorXd D = mtrx.Dtrs;
	VectorXd zflt = mtrx.zflt;

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

	mtrx.Ztrs = Z(zind, xind);
	mtrx.zfix = zfix(zind);

	return zind.size();
}

/** Lambda algorithm and its variations (ILQ, Common set, BIE) */
int lambda_search(
	Trace& trace,		///< Debug trace
	GinAR_mtx& mtrx,	///< Reference to structure containing float values and covariance
	GinAR_opt opt)		///< Object containing processing options
{
	int info = Ztrans_reduction(trace, mtrx);

	if (info < 0)
	{
		tracepdeex(2, trace, "\n Matrix decorrelation failed ... ");
		return 0;
	}

	
	int nmax = mtrx.Dtrs.size();
	int k = nmax - 1;
	int kmax = k;

	double succ = erf(sqrt(1 / (8 * mtrx.Dtrs(k--))));
	
	if (succ < opt.sucthr) 
		return 0;

	int zsiz = 1;

	while (k >= 0)
	{
		succ *= erf(sqrt(1 / (8 * mtrx.Dtrs(k--))));
		if (succ < opt.sucthr)
			break;
		zsiz++;
	}
	if(zsiz<3)
		return 0;
	
	int kmin=kmax-zsiz+1;

	map<double, VectorXd> zfixList;

	MatrixXd L = mtrx.Ltrs;
	VectorXd D = mtrx.Dtrs;
	VectorXd zflt = mtrx.zflt;
	
//	MatrixXd Sadj = MatrixXd::Zero(nmax, nmax);
	VectorXd dist = VectorXd::Zero(nmax);
	VectorXd zadj = VectorXd::Zero(nmax);
	VectorXd zfix = VectorXd::Zero(nmax);
	VectorXd zdif = VectorXd::Zero(nmax);
	VectorXd step = VectorXd::Zero(nmax);
	
	k = kmax;
	zadj(k) = zflt(k);
	zfix(k) = ROUND(zadj(k));
	zdif(k) = zadj(k) - zfix(k);
	step(k) = zdif(k) < 0 ? -1 : 1;
	bool search = true;
	double maxdist = 1e99;
	int ncand = 0;
	
		trace << std::endl << "L: "    << std::endl << L 				<< std::endl;
		trace << std::endl << "dist: " << std::endl << dist.transpose() << std::endl;
		trace << std::endl << "zflt: " << std::endl << zflt.transpose() << std::endl;
		trace << std::endl << "D: " << std::endl	<< D 				<< std::endl;
		
		trace << std::endl << "  zsiz: " << zsiz << ": " << kmin << " - " << kmax << std::endl; 
	
	trace << k << ": " << zadj(k) << ", " <<  zfix(k);
	while (search)
	{
		double newdist = dist(k) + zdif(k) * zdif(k) / D(k);

		if (newdist < maxdist)
		{
			if (k != kmin)
			{
				k--;
				dist(k) = newdist;
				// Sadj.block(k, 0, 1, k-kmin + 1) = (Sadj.block(k + 1, 0, 1, k-kmin+1)).eval() - zdif(k + 1) * L.block(k + 1, 0, 1, k-kmin+1);
				// zadj(k) = zflt(k) + Sadj(k,k);
				
				zadj(k) = zflt(k);
				for(int j = k+1; j<nmax; j++)
					zadj(k)-= zdif(j)*L(j,k);
				
				zfix(k) = ROUND(zadj(k));
				zdif(k) = zadj(k) - zfix(k);
				step(k) = zdif(k) < 0 ? -1 : 1;
				
				trace << "; " << k << ": " << zadj(k) << ", " <<  zfix(k);
			}
			else
			{
				VectorXd zcut = zfix.tail(zsiz);
				zfixList[newdist] = zcut;
				ncand = zfixList.size();
				double maxd = newdist * opt.ratthr;
				
				if (ncand > 1
				 && maxd  < maxdist) 
					maxdist = maxd;

				// trace << std::endl << "Candidate found:" << zfixList[newdist].transpose() << ";   dist: " << newdist << " / " << maxdist << ";  #cand: " << ncand << " / " << opt.nset << ", zsiz: " << zsiz;
				
				if(ncand > opt.nset)
					break;
				
				if (opt.nset>0
				&& (ncand >= opt.nset))
				{
					int ntot = 0;
					for (auto it = zfixList.begin(); it != zfixList.end(); )
					{
						if (ntot++ >= opt.nset)
							it = zfixList.erase(it);
						else
						{
							maxd = it->first;
							++it;
						}
					}
					
					if(maxd < maxdist) 
						maxdist = maxd;
			
					ncand = zfixList.size();
				}

				zfix(kmin)+= step(kmin);
				zdif(kmin) = zadj(kmin) - zfix(kmin);
				step(kmin) =-step(kmin) + (step(kmin) < 0 ? 1 : -1);
			}
		}
		else
		{
			if (k == kmax) break;
			else
			{
				k++;
				zfix(k)+= step(k);
				zdif(k) = zadj(k) - zfix(k);
				step(k) =-step(k) + (step(k) < 0 ? 1 : -1);
			}
		}
	}

	if (zfixList.size() < 1) 
		return 0;

	double mindist = zfixList.begin()->first;
	VectorXd zfix0 = zfixList.begin()->second;
	mtrx.zfix = zfix0;
	MatrixXd Z = mtrx.Ztrs.bottomRows(zsiz);
	mtrx.Ztrs = Z;

	if ( opt.mode == E_ARmode::LAMBDA )
		return zfix0.size();
		
	if ( opt.mode == E_ARmode::LAMBDA_ALT )
	{
		double first  = 0;
		double second = 0;
		for (auto& [dis, fixvec] : zfixList)
		{
			if (first == 0) 		first = dis;
			else if(second == 0)	second =dis;
			else					break;
		}
		
		if ((second/first) < opt.ratthr)	return 0;
		else                                return zfix0.size();
	}
	
	if ( opt.mode == E_ARmode::LAMBDA_AL2 )
	{
		for (auto& [dis, fixvec] : zfixList)
		{
			if ((dis / mindist) > opt.ratthr) 
				break;
			
			for (int l = 0; l < zfix0.size(); l++)
			{
				if (zfix0(l) == -99999.5) 
					continue;

				if (zfix0(l) != fixvec(l))
					zfix0(l) = -99999.5;
			}
		}
		
		vector<int> zind;
		for (int k = 0; k < zfix0.size(); k++) 
		if (zfix0(k) != -99999.5)
			zind.push_back(k);
		tracepdeex(2, trace, "... %d ambiguties in common\n", zind.size());

		vector<int> xind;
		for (int k = 0; k < nmax; k++) 
			xind.push_back(k);
		
		mtrx.zfix = zfix0(zind);
		mtrx.Ztrs = Z(zind, xind);
		
		return zind.size();
	}
	
	if (opt.mode == E_ARmode::LAMBDA_BIE)
	{
		double acum = 0;

		for (auto& [dis, fixvec] : zfixList)
		{
			double fct  = exp(-0.5 * (dis-mindist));
			acum += fct;
		}

		VectorXd zbie = VectorXd::Zero(zsiz);

		for (auto& [dis, fixvec] : zfixList)
		{
			double fct  = exp(-0.5 * (dis-mindist)) / acum;
			if (AR_VERBO) 
				trace << std::endl << "Candidate found:" << fixvec.transpose() << ";   fact= " << fct;
			zbie += fct * fixvec;
		}

		mtrx.zfix = zbie;
		
		return zbie.size();
	}

	// should never be hit ...
	return 0;
	
}

/** Ambiguity resolution function for Ginan */
int GNSS_AR(
	Trace& trace,		///< Debug trace
	GinAR_mtx& mtrx,	///< Reference to structure containing float values and covariance
	GinAR_opt opt)		///< Object containing processing options
{
	switch (opt.mode)
	{
		case E_ARmode::OFF:					return 0;
		case E_ARmode::ROUND:				return simple_round		(trace, mtrx, opt);
		case E_ARmode::ITER_RND:			return interat_round	(trace, mtrx, opt);
		case E_ARmode::BOOTST:				return integer_bootst	(trace, mtrx, opt);
		case E_ARmode::LAMBDA:				return lambda_search	(trace, mtrx, opt);
		case E_ARmode::LAMBDA_ALT:			return lambda_search	(trace, mtrx, opt);
		case E_ARmode::LAMBDA_AL2:			return lambda_search	(trace, mtrx, opt);
		case E_ARmode::LAMBDA_BIE:			return lambda_search	(trace, mtrx, opt);
		// default:							tracepdeex(1, trace, "\n AR mode not supported \n");
	}

	return 0;
}
