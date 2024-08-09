
#include "GNSSambres.hpp"
#include "testUtils.hpp"

#include <math.h>

#define LOG_PI          1.14472988584940017
#define SQRT2           1.41421356237309510
#define AMB_RANG		10

bool 	AR_VERBO = false;
double  FIXED_AMB_VAR = 1e-8;
/** Probability of error (assuming normal distribution) */
double round_perr(
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
	tracepdeex(4, trace, "\n#ARES_RND Using integer rounding ... %.4e  %.4f", sucthr, ratthr);

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
	GinAR_opt& opt)		///< Object containing processing options
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


bool LTDL_factorization(
	GinAR_mtx& mtrx)		///< Reference to structure containing float values and covariance
{
	int n = mtrx.aflt.size();
	MatrixXd P = mtrx.Paflt;

	MatrixXd L = MatrixXd::Zero(n,n);
	VectorXd D = VectorXd::Zero(n);

	for (int i=n-1; i>=0; i--)
	{
		if (P(i,i)<=0)
			return false;

		D(i) = P(i,i);
		double a=sqrt(P(i,i));
		L.block(i, 0, 1, i+1) = P.block(i, 0, 1, i+1)/a;
		for (int j=0;j<i;j++)
			P.block(j, 0, 1, j+1)-= L(i,j)*L.block(i, 0, 1, j+1);
		L.block(i, 0, 1, i+1)/= L(i,i);
	}

	mtrx.Ltrs = L;
	mtrx.Dtrs = D;
	return true;
}

/** Lambda decorrelation (trough Z transform) */
int Ztrans_reduction(
	Trace& trace,		///< Debug trace
	GinAR_mtx& mtrx)	///< Reference to structure containing float values and covariance
{
	int n = mtrx.aflt.size();
	if (n<1)
		return -1;

	if (LTDL_factorization(mtrx) == false)
	{
		tracepdeex(1, trace, "WARNING: LD decomposition error, ambiguity matrix may not positive definite\n");
		return -1;
	}

	VectorXd x = mtrx.aflt;
	VectorXd D = mtrx.Dtrs;
	MatrixXd L = mtrx.Ltrs;
	MatrixXd Z = MatrixXd::Identity(n, n);

	if (AR_VERBO)
	{
		trace << std::setprecision(8);
		trace << "\n" << "x =" << "\n" << x.transpose() << "\n";
		trace << "\n" << "Px=" << "\n" << mtrx.Paflt    << "\n";
		trace << "\n" << "Lx=" << "\n" << L             << "\n";
		trace << "\n" << "Dx=" << "\n" << D.transpose() << "\n";
	}

	int k=n-2;
	int j=n-2;
	while (j>=0)
	{
		if (j<=k)
		for (int i=j+1; i<n; i++)
		{
			double mu = ROUND(L(i, j));
			if (mu != 0)
			{
				L.col(j) -= mu * L.col(i);
				Z.col(j) -= mu * Z.col(i);
			}
		}

		double del = D(j) + L(j+1,j)*L(j+1,j)*D(j+1);
		if ((del+1E-6)<D(j+1))
		{
			double eta = D(j) / del;
			double lam = D(j+1) * L(j+1,j) / del;

			D(j)   = eta * D(j+1);
			D(j+1) = del;

			MatrixXd a0 = L.block(j,   0, 1, j);
    		MatrixXd a1 = L.block(j+1, 0, 1, j);
    		L.block(j,   0, 1, j) = a1 - L(j+1,j)*a0;
			L.block(j+1, 0, 1, j) = lam*a1 + eta*a0;
			L(j+1,j) = lam;

			VectorXd Ltmp = L.block(j+2, j, n-j-2, 1);
			L.block(j+2, j,   n-j-2, 1) = L.block(j+2, j+1, n-j-2, 1);
			L.block(j+2, j+1, n-j-2, 1) = Ltmp;

			VectorXd Ztmp = Z.col(j);
			Z.col(j)   = Z.col(j+1);
			Z.col(j+1) = Ztmp;

			k=j;
			j=n-2;
		}
		else
			j--;
	}

	mtrx.Ztrs = Z.transpose();
	mtrx.zflt = mtrx.Ztrs * x;
	mtrx.Ltrs = L;
	mtrx.Dtrs = D;

	if (AR_VERBO)
	{
		trace << std::setprecision(8);
		trace << "\n" << "z =" << "\n" << mtrx.zflt.transpose()	<< "\n";
		trace << "\n" << "Zt=" << "\n" << mtrx.Ztrs				<< "\n";
		trace << "\n" << "Lz=" << "\n" << L						<< "\n";
		trace << "\n" << "Dz=" << "\n" << D.transpose()			<< "\n";
	}

	return n;
}

/** Integer bootstrapping */
// int integer_bootst(
// 	Trace& trace,		///< Debug trace
// 	GinAR_mtx& mtrx,	///< Reference to structure containing float values and covariance
// 	GinAR_opt opt)		///< Object containing processing options
// {
// 	int info = Ztrans_reduction(trace, mtrx);

// 	if (info < 0)
// 		return 0;



// 	GinAR_mtx mtrx2;
// 	mtrx2.aflt = mtrx.zflt;

// 	MatrixXd Z   = mtrx.Ztrs;
// 	mtrx2.Paflt	 = Z*mtrx.Paflt*Z.transpose();

// 	int nfix = interat_round (trace, mtrx2, opt);

// 	mtrx.Ztrs = mtrx2.Ztrs * Z;
// 	mtrx.zfix = mtrx2.zfix;

// 	return nfix;
// }

int integer_bootst(
	Trace& trace,		///< Debug trace
	GinAR_mtx& mtrx,	///< Reference to structure containing float values and covariance
	GinAR_opt& opt)		///< Object containing processing options
{
	LDLT<MatrixXd> ldlt_;
	ldlt_.compute(mtrx.Paflt);

	if (ldlt_.isPositive() == false)
	{
		tracepdeex(1, trace, "WARNING: LD decomposition error, ambiguity matrix may not positive definite\n");
		return 0;
	}

	MatrixXd L_ = ldlt_.matrixL();
	auto     tr = ldlt_.transpositionsP ();

	int siz = mtrx.aflt.size();
	MatrixXd I0 = MatrixXd::Identity(siz, siz);
	MatrixXd Zt = tr * I0;
	VectorXd z_ = tr * mtrx.aflt;

	for (int j = siz - 2; j >= 0; j--)
	{
		for (int i = j + 1; i < siz; i++)
		{
			double mu = ROUND(L_(i, j));

			if (mu != 0)
			{
				L_.row(i) -= mu * L_.row(j);
				Zt.row(i) -= mu * Zt.row(j);
				z_    (i) -= mu * z_    (j);
			}
		}
	}

	MatrixXd Pz = Zt*mtrx.Paflt*Zt.transpose();

	GinAR_mtx mtrx2;
	mtrx2.aflt  = z_;
	mtrx2.Paflt	= Pz;

	if (AR_VERBO)
	{
		trace << "\n" << "x_=" << "\n" << mtrx.aflt.transpose() 	<< "\n";
		trace << "\n" << "Px=" << "\n" << mtrx.Paflt 				<< "\n";
		trace << "\n" << "Zt=" << "\n" << Zt 						<< "\n";
		trace << "\n" << "z_=" << "\n" << z_.transpose() 			<< "\n";
		trace << "\n" << "Pz=" << "\n" << Pz						<< "\n";
	}

	int nfix = interat_round(trace, mtrx2, opt);

	mtrx.Ztrs = mtrx2.Ztrs * Zt;
	mtrx.zfix = mtrx2.zfix;

	return nfix;
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

	while (search)
	{
		double newdist = dist(k) + zdif(k) * zdif(k) / D(k);

		if (newdist < maxdist)
		{
			if (k != kmin)
			{
				k--;
				dist(k) = newdist;

				zadj(k) = zflt(k);
				for(int j = k+1; j<nmax; j++)
					zadj(k)-= zdif(j)*L(j,k);

				zfix(k) = ROUND(zadj(k));
				zdif(k) = zadj(k) - zfix(k);
				step(k) = zdif(k) < 0 ? -1 : 1;
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

	switch (opt.mode)
	{
		case E_ARmode::LAMBDA:
			return zfix0.size();

		case E_ARmode::LAMBDA_ALT:
		{
			double first  = 0;
			double second = 0;
			for (auto& [dis, fixvec] : zfixList)
			{
				if		(first	== 0)	first	= dis;
				else if	(second	== 0)	second	= dis;
				else					break;
			}

			if ((second/first) < opt.ratthr)	return 0;
			else                                return zfix0.size();
		}

		case E_ARmode::LAMBDA_AL2:
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

		case E_ARmode::LAMBDA_BIE:
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
					trace << "\n" << "BIE Candidate found:" << fixvec.transpose() << ";   dist= " << dis << ";   fact= " << fct;
				zbie += fct * fixvec;
			}

			mtrx.zfix = zbie;

			return zbie.size();
		}
	}

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
