
#include <iostream>
#include <vector>

#include "observations.hpp"
#include "streamTrace.hpp"
#include "acsConfig.hpp"
#include "testUtils.hpp"
#include "satStat.hpp"
#include "constants.h"
#include "algebra.hpp"
#include "common.hpp"
#include "acsQC.hpp"
#include "lambda.h"


#define		THRES_MW_JUMP		10.0

#define     PDEGAP  			60              /* (s) */
#define     PDESLIPTHRESHOLD	0.5    			/* triple-frequency (cycle) */
#define     NRESET  			20              /* reset number for averaging MW */

#define     TEST    			0


/** Detect cycle slip by reported loss of lock
*/
void detslp_ll(
	ObsList&  obsList)	///< List of observations to detect slips within
{
	BOOST_LOG_TRIVIAL(debug) << "detslp_ll: n=" << obsList.size();

	for (auto& obs			: obsList)
	for (auto& [ft, sig]	: obs.Sigs)
	{
		if (obs.exclude)
		{
			continue;
		}

		int f = ft;
		if	( sig.L == 0
			|| !(sig.LLI & 0x03))
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(debug) << "detslp_ll: slip detected sat=" << obs.Sat.id() << " f=" << f + 1;

		obs.satStat_ptr->sigStatMap[ft].slip.LLI = true;
	}
}

/** Detect cycle slip by geometry free phase jump
*/
void detslp_gf(
	ObsList&  obsList)	///< List of observations to detect slips within
{
	BOOST_LOG_TRIVIAL(debug) << "detslp_gf: n=" << obsList.size();

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}
		E_FType frq2=F2;
		if(obs.Sat.sys == +E_Sys::GAL) frq2=F5;
		
		S_LC& lc = getLC(obs.satStat_ptr->lc_new, F1, frq2);

		double gf1 = lc.GF_Phas_m;
		if	( lc.valid	== false
			||gf1		== 0)
		{
			continue;
		}

		double gf0 = obs.satStat_ptr->gf;
		obs.satStat_ptr->gf = gf1;

		if (gf0 == 0)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(debug) << "detslip_gf: sat=" << obs.Sat.id() << " gf0=" << gf0 << " gf1=" << gf1;

		if (fabs(gf1 - gf0) > acsConfig.thres_slip)
		{
			BOOST_LOG_TRIVIAL(debug) << "detslip_gf: slip detected sat=" << obs.Sat.id() << " gf=" << gf0 << "->" << gf1;

			for (auto& [ft, sigStat] : obs.satStat_ptr->sigStatMap)
			{
				sigStat.slip.GF = true;
			}
		}
	}
}

/** Detect slip by Melbourne-Wubbena linear combination jump
*/
void detslp_mw(
	ObsList&  obsList)	///< List of observations to detect slips within
{
	BOOST_LOG_TRIVIAL(debug) << "detslp_mw: n=" << obsList.size();

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}
		E_FType frq2=F2;
		if(obs.Sat.sys == +E_Sys::GAL) frq2=F5;
		
		S_LC& lc = getLC(obs.satStat_ptr->lc_new, F1, frq2);

		double mw1 = lc.MW_c;
		if	( lc.valid	== false
			||mw1		== 0)
		{
			continue;
		}

		double mw0 = obs.satStat_ptr->mw;
		obs.satStat_ptr->mw = mw1;

		if (mw0 == 0)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(debug) << "detslip_mw: sat=" << obs.Sat.id() << " mw0=" << mw0 << " mw1=" << mw1;

		if (fabs(mw1 - mw0) > THRES_MW_JUMP)
		{
			BOOST_LOG_TRIVIAL(debug) << "detslip_mw: slip detected sat=" << obs.Sat.id() << " mw=" << mw0 << "->" << mw1;

			for (auto& [ft, sigStat] : obs.satStat_ptr->sigStatMap)
			{
				sigStat.slip.MW = true;
			}
		}
	}
}




/** Melbourne-Wenbunna (MW) measurement noise (m)
*/
double mwnoise(
	double sigcode,		///< Code noise
	double sigphase,	///< Phase noise
	double lam1,		///< L1 wavelength
	double lam2)		///< L2 wavelength
{
	double a = lam2 * lam2 / (lam2 + lam1) / (lam2 + lam1) + lam1 * lam1 / (lam2 + lam1) / (lam2 + lam1);
	double b = lam2 * lam2 / (lam2 - lam1) / (lam2 - lam1) + lam1 * lam1 / (lam2 - lam1) / (lam2 - lam1);
	return SQRT(a*SQR(sigcode) + b*SQR(sigphase));
}

/** Melbourne-Wenbunna (MW) measurement noise average over time
*/
void mwaverage(
	double	mw,			///< Calculated mw combination
	double	mw_pre,		///< Previous mw combination
	MWSlip&	mwSlip)		///< MW Slip object
{
	mwSlip.num++;

	double exp = SQR(mwSlip.mean) + 1 / mwSlip.num * (SQR(mw) - SQR(mwSlip.mean));

	/* averaged MW measurement and noise */
	if (mwSlip.num == 1)
	{
		/* first epoch */
		mwSlip.mean		= 1 / 2 * (mw + mw_pre);
		mwSlip.sigma	= 0.5; /* initial 0.5 cycle */
	}
	else if (mwSlip.num == NRESET)
	{
		/* reset */
		mwSlip.mean += 1 / 2 * (mw - mwSlip.mean);
		mwSlip.sigma = SQRT(SQR(mwSlip.sigma) + 1 / 2 * (SQR(mw - mw_pre) - SQR(mwSlip.sigma)));
	}
	else
	{
		mwSlip.mean += 1 / mwSlip.num * (mw - mwSlip.mean);
		mwSlip.sigma = SQRT(SQR(mwSlip.sigma) + 1 / mwSlip.num * (SQR(mw - mw_pre) - SQR(mwSlip.sigma)));
	}
	//todo aaron, change to use sliding window rather than reset? looks faster considering the sqrts and low reset value

#if (0)
	/* close formula */
	mwSlip.sigma = SQRT(fabs(exp - SQR(mwSlip.mean)));
#endif
}


/** Single channel detection–identification–adaptation (DIA) for integer cycle slips
*/
void scdia(
	Trace&				trace,		///< Trace to output to
	SatStat&			satStat,	///< Persistant satellite status parameters
	lc_t&				lc,			///< Linear combinations
	map<int, double>&	lam,		///< Signal wavelength map
	double				sigmaPhase,	///< Phase noise
	double				sigmaCode,	///< Code noise
	int					nf,			///< Number of frequencies
	int					sys,		///< Satellite system
	int					flag,		///< LSQ/Kalman filter flag (0-KF)
	double*				timeStamp)	///< Timestamp
{
	TestStack ts(__FUNCTION__);

	double *H, *P, *Z, *N, *x, *Px, *N1, *a, *Qa, *F, *xp, *Pp, *H1, *I, *Z1, *Hlom, *v;
	double lam1, lam2, lam5, s[2], coef;
	int i, j, m, n, index, ind = 0; /* m-rows measurements, n-cols unknowns */

#ifdef DEBUGLOM
	GTime t1={0};
#endif

	E_FType frq2=F2,frq3=F5;
	if(sys == E_Sys::GAL)
	{
		frq2=F5; frq3=F7;
	}

	if (nf == 0)
		return;
	m = 2 * nf + 1;
	n = 2 + nf; /* row=2*nf+1, col=2+nf */

	H = zeros(m, n);
	P = eye(m);
	Z = mat(m, 1);
	Z1 = mat(m, 1);
	N = mat(n, m);
	F = mat(nf, 2);
	x = zeros(n, 1);
	Px = zeros(n, n);
	N1 = mat(n, 1);
	a = mat(nf, 1);
	Qa = mat(nf, nf);
	xp = mat(n, 1);
	Pp = mat(n, n);
	H1 = zeros(n, m);
	I = eye(m);
	Hlom = zeros(m, 2);
	v = mat(m, 1);

#ifdef DEBUGLOM
	t1=utc2gpst(timeget());
#endif

	/* wavelength */
	lam1 = lam[F1];
	lam2 = lam[frq2];
	lam5 = lam[frq3];

	coef = (lam2 * lam2) / (lam1 * lam1);

	/* single frequency, prepare H, R, Z, not supported in current PDE */
	if (nf == 1 && 0)
	{
		/* success-rate is so low that ambiguity validation is not passed */
		Z[0] = lc.L_m[F1]	- satStat.lc_pre.L_m[F1];
		Z[1] = lc.P  [F1]	- satStat.lc_pre.P  [F1];
		Z[2] = satStat.dIono;

		P[0]			= 1 / (2 * SQR(sigmaPhase));
		P[1 + m]		= 1 / (2 * SQR(sigmaCode));
		P[2 + 2 * m]	= 1 / SQR(satStat.sigmaIono);

		H[0]		= 1;
		H[1]		= 1;
		H[1 + m]	= 1;
		H[2 + m]	= 1;
		H[m]		= -1;
		H[2 * m]	= lam1;
	}
	/* dual frequency, prepare H, R, Z */
	else if (nf == 2)
	{
		if (flag)
		{
			/* observed minus computed */
			Z[0] = lc.L_m[F1]	- satStat.lc_pre.L_m[F1];
			Z[1] = lc.L_m[frq2]	- satStat.lc_pre.L_m[frq2];

			Z[2] = lc.P[F1]		- satStat.lc_pre.P[F1];
			Z[3] = lc.P[frq2]	- satStat.lc_pre.P[frq2];
		}
		else
		{
			/* observed minus computed */
			Z[0] = lc.L_m[F1]	- satStat.flt.lc_pre.L_m[F1];
			Z[1] = lc.L_m[frq2]	- satStat.flt.lc_pre.L_m[frq2];

			Z[2] = lc.P[F1]		- satStat.flt.lc_pre.P[F1];
			Z[3] = lc.P[frq2]	- satStat.flt.lc_pre.P[frq2];
		}
		Z[4] = satStat.dIono; /* note here */
		/* weight matrix */

		P[0]			= 1 / (2 * SQR(sigmaPhase));
		P[1 + m]		= 1 / (2 * SQR(sigmaPhase));
		P[2 + 2 * m]	= 1 / (2 * SQR(sigmaCode));
		P[3 + 3 * m]	= 1 / (2 * SQR(sigmaCode));
		P[4 + 4 * m]	= 1 / SQR(satStat.sigmaIono);
		/* design matrix */
		H[0]			= 1;
		H[1]			= 1;
		H[2]			= 1;
		H[3]			= 1;
		H[m + 4]		= 1;
		H[m]			= -1;
		H[m + 1]		= -SQR(lam2) / SQR(lam1);
		H[m + 2]		= 1;
		H[m + 3]		= SQR(lam2) / SQR(lam1);
		H[2 * m]		= lam1;
		H[3 * m + 1]	= lam2;
	}
	/* triple-frequency, prepare H, R, Z */
	else
	{
		if (flag)
		{
			/* observed minus computed */
			Z[0] = lc.L_m[F1]	- satStat.lc_pre.L_m[F1];
			Z[1] = lc.L_m[frq2]	- satStat.lc_pre.L_m[frq2];
			Z[2] = lc.L_m[frq3]	- satStat.lc_pre.L_m[frq3];

			Z[3] = lc.P[F1]		- satStat.lc_pre.P[F1];
			Z[4] = lc.P[frq2]	- satStat.lc_pre.P[frq2];
			Z[5] = lc.P[frq3]	- satStat.lc_pre.P[frq3];
		}
		else
		{
			/* observed minus computed */
			Z[0] = lc.L_m[F1]	- satStat.flt.lc_pre.L_m[F1];
			Z[1] = lc.L_m[frq2]	- satStat.flt.lc_pre.L_m[frq2];
			Z[2] = lc.L_m[frq3]	- satStat.flt.lc_pre.L_m[frq3];

			Z[3] = lc.P[F1]		- satStat.flt.lc_pre.P[F1];
			Z[4] = lc.P[frq2]	- satStat.flt.lc_pre.P[frq2];
			Z[5] = lc.P[frq3]	- satStat.flt.lc_pre.P[frq3];
		}
		Z[6] = satStat.dIono; /* note here */
		/* weight matrix */
		P[0]			= 1 / (2 * SQR(sigmaPhase));
		P[1 + m]		= 1 / (2 * SQR(sigmaPhase));
		P[2 + 2 * m]	= 1 / (2 * SQR(sigmaPhase));
		P[3 + 3 * m]	= 1 / (2 * SQR(sigmaCode));
		P[4 + 4 * m]	= 1 / (2 * SQR(sigmaCode));
		P[5 + 5 * m]	= 1 / (2 * SQR(sigmaCode));
		P[6 + 6 * m]	= 1 / SQR(satStat.sigmaIono);
		/* design matrix */
		H[0]			= 1;
		H[1] 			= 1;
		H[2]			= 1;
		H[3]			= 1;
		H[4]			= 1;
		H[5]			= 1;
		H[m + 6]		= 1;
		H[m]			= -1;
		H[m + 1]		= -SQR(lam2) / SQR(lam1);
		H[m + 2]		= -SQR(lam5) / SQR(lam1);
		H[m + 3]		= 1;
		H[m + 4]		= SQR(lam2) / SQR(lam1);
		H[m + 5]		= SQR(lam5) / SQR(lam1);
		H[2 * m]		= lam1;
		H[3 * m + 1]	= lam2;
		H[4 * m + 2]	= lam5;
	}

	/* design matrix for LOM test */
	matcpy(Hlom, H, m, 2);

	/* perform LOM test for outlier detection */
#ifdef DEBUGLOM
				TestStack::testMat("Hlom", Hlom, m*2);
				TestStack::testMat("P", P, m*m);
	ind=lsqqc(trace,Hlom,P,Z,v,NULL,NULL,m,2,0,0);
	*timeStamp=timediff(utc2gpst(timeget()),t1);
	if (ind==0) tracepdeex(2,trace,"\n");
#else
	ind = 1;
#endif
	if (ind)
	{
		satStat.sigStatMap[F1].slip.SCDIA = true;
		satStat.sigStatMap[frq2].slip.SCDIA = true;
		if (nf == 3)
			satStat.sigStatMap[frq3].slip.SCDIA = true;
		if (flag)
		{ /* least-squares */
			matmul("TN", n, m, m, 1.0, H, P, 0.0, N); /* H'*P */
			matmul("NN", n, n, m, 1.0, N, H, 0.0, Pp); /* H'*P*H */
			matmul("NN", n, 1, m, 1.0, N, Z, 0.0, N1); /* Nl=H'*P*Z */
			if (!matinv(Pp, n))
			{
				matmul("NN", n, 1, n, 1.0, Pp, N1, 0.0, xp);
			}
			/* store float solution and vc matrix */
			matcpy(satStat.flt.a, xp + 2, 1, nf);

			for (i = 0; i < nf; i++)
			for (j = 0; j < nf; j++)
				satStat.flt.Qa[i][j] = Pp[(i + 2) * n + j + 2];
		}
		else
		{ /* kalman filter */
			satStat.flt.ne++;
			if (satStat.flt.ne > 2)
			{
				satStat.flt.slip = 0;
				satStat.flt.ne = 0;
				tracepdeex(2, trace, "\n");
				free(H);
				free(P);
				free(Z);
				free(Px);
				free(x);
				free(xp);
				free(H1);
				free(Z1);
				free(a);
				free(Qa);
				free(N1);
				free(N);
				free(F);
				free(Pp);
				free(I);
				free(Hlom);
				return;
			}

			matcpy(x + 2, satStat.flt.a, 1, nf);
			/* time update */
			for (i = 0; i < nf; i++)
			for (j = 0; j < nf; j++)
				Px[(i + 2) * n + j + 2] = satStat.flt.Qa[i][j];

			Px[0]		= 1E6;
			Px[1 + n]	= 1E6;
			matcpy(Z1, Z, m, 1);
			/* measurement-prediction */
			matmul("NN", m, 1, n, -1.0, H, x, 1.0, Z);
			/* transpose of desgin matrix */
			matmul("TN", n, m, m, 1.0, H, I, 0.0, H1);
			/* measurement update */
			if (!matinv(P, m))
				filter_(x, Px, H1, Z, P, n, m, xp, Pp);

			matcpy(satStat.flt.a, xp + 2, 1, nf);

			for (i = 0; i < nf; i++)
			for (j = 0; j < nf; j++)
				satStat.flt.Qa[i][j] = Pp[(i + 2) * n + j + 2];
		}

		/* ambiguity vector and its variance */
		matcpy(a, xp + n - nf, nf, 1);

		for (i = 0; i < nf; i++)
		for (j = 0; j < nf; j++)
		{
			Qa[i * nf + j] = Pp[(n - nf + i) * n + j + n - nf];
		}

		/* integer cycle slip estimation */
		lambda(trace, nf, 2, a, Qa, F, s, acsConfig.predefined_fail, &index);
		if (flag)
		{
			/* least-squares */
			satStat.amb[0] = 0;
			satStat.amb[1] = 0;
			satStat.amb[2] = 0;
			tracepdeex(2, trace, "(freq=%d) ", nf);
			if (index)
			{
				tracepdeex(2, trace, "fixed ");
// 				tracematpde(2, trace, F, 1, nf, 4, 1);
				for (auto& [key, sigStat] : satStat.sigStatMap)
				{
					satStat.amb[i] = ROUND(F[i]);
					sigStat.slip.SCDIA = true;
				}
			}
			tracepdeex(2, trace, "\n");
		}
		else
		{
			/* kalman filter */
			satStat.flt.amb[0] = 0;
			satStat.flt.amb[1] = 0;
			satStat.flt.amb[2] = 0;
			if (index)
			{
				memset(satStat.flt.a, 0, 3);
				memset(satStat.flt.Qa, 0, 9);
				satStat.flt.slip |= 2;
				tracepdeex(1, trace, "     ACC fixed ");
// 				tracematpde(1, trace, F, 1, nf, 4, 1);
				for (i = 0; i < nf; i++)
				{
					satStat.flt.amb[i] = ROUND(F[i]);
				}
			}
			tracepdeex(2, trace, "\n");
			tracepde(1, trace, "ACC epoch used=%2d\n", satStat.flt.ne);
			if (index)
				satStat.flt.ne = 0;
		}

#ifdef DEBUGCSACC
		if (flag) 
		{
			tracepde(1,trace,"ILS H=\n");	tracematpde(1,trace,H,m,n,14,7);
			tracepde(1,trace,"ILS R=\n");	tracematpde(1,trace,P,m,m,14,7);
			tracepde(1,trace,"ILS Z=  ");	tracematpde(1,trace,Z,1,m,14,7);
		}
		else 
		{
			tracepde(1,trace,"ACC H=\n");	tracematpde(1,trace,H,m,n,14,7);
			tracepde(1,trace,"ACC R=\n");	tracematpde(1,trace,P,m,m,14,7);
			tracepde(1,trace,"ACC Z=  ");	tracematpde(1,trace,Z1,1,m,14,7);
		}
		tracepde(1,trace,"ACC x0=\n");	tracematpde(1,trace,x,1,n,14,7);
		tracepde(1,trace,"ACC P0=\n");	tracematpde(1,trace,Px,n,n,14,7);
		tracepde(1,trace,"ACC xp=\n");	tracematpde(1,trace,xp,1,n,14,7);
		tracepde(1,trace,"ACC Pp=\n");	tracematpde(1,trace,Pp,n,n,14,7);
		tracepde(1,trace,"ACC a=\n"); 	tracematpde(2,trace,a,1,nf,14,4);
		tracepde(1,trace,"ACC Qa=\n");	tracematpde(2,trace,Qa,nf,nf,14,4);
#endif
	}
	free(H);
	free(P);
	free(Z);
	free(Px);
	free(x);
	free(xp);
	free(H1);
	free(Z1);
	free(a);
	free(Qa);
	free(N1);
	free(N);
	free(F);
	free(Pp);
	free(I);
	free(Hlom);
	free(v);
}

/** Cycle slip detection and repair for dual-frequency
*/
void cycleslip2(
	Trace&		trace,		///< Trace to output to
	SatStat&	satStat,	///< Persistant satellite status parameters
	lc_t&		lcBase,		///< Linear combinations
	Obs&		obs,		///< Navigation object for this satellite
	double*		timeStamp)	///< Timestamp
{
	int week;
	double sec	= time2gpst(lcBase.time, &week);
	double dt	= timediff(lcBase.time, satStat.lc_pre.time);

	if	( dt < 20
		||dt > PDEGAP)
	{
		// small interval or reset

		satStat.dIono = 0;
		// approximation of ionosphere residual

		satStat.sigmaIono = acsConfig.proc_noise_iono * SQRT(dt);
	}
	else
	{
		// medium interval ~30s

		if (satStat.dIono == 0)
		{
			satStat.sigmaIono = acsConfig.proc_noise_iono * SQRT(dt);
		}
	}

	if (satStat.sigmaIono == 0)
	{
		satStat.sigmaIono = 0.001;
	}

	char id[32];
	lcBase.Sat.getId(id);
	int sys = lcBase.Sat.sys;
	E_FType frq2=F2;
	if(sys == E_Sys::GAL) frq2=F5;
	
	auto&	lam = obs.satNav_ptr->lamMap;

	double lam1 = lam[F1];
	double lam2 = lam[frq2];

	double lamw = lam1 * lam2 / (lam2 - lam1);	//todo aaron, rename

	/* ionosphere coefficient */
	double coef = SQR(CLIGHT / lam1) / SQR(CLIGHT / lam2) - 1;

	/* elevation dependent noise */
	double sigmaCode	= sqrt(obs.Sigs.begin()->second.codeVar);
	double sigmaPhase	= sqrt(obs.Sigs.begin()->second.phasVar);

	if (acsConfig.debug_cs)
	{
		sigmaCode	= 2;
		sigmaPhase	= 0.02;
	}

	double sigmaGF = 2 * sigmaPhase;

	S_LC lc12new = getLC(lcBase, 			F1, frq2);
	S_LC lc12old = getLC(satStat.lc_pre,	F1, frq2);

	double mwNoise = mwnoise(sigmaCode, sigmaPhase, lam1, lam2);

	/* averaged MW measurement and noise */
	double fNw;
	double sigmaMW;
	if (satStat.mwSlip.num >= 15 && 0)
	{
		fNw = lc12new.MW_c - satStat.mwSlip.mean;
		sigmaMW = SQRT(2) * satStat.mwSlip.sigma * lamw;
	}
	else
	{
		fNw = lc12new.MW_c - lc12old.MW_c; /* Eq (6) in TN */
		sigmaMW = SQRT(2) * mwNoise;
	}

	if (acsConfig.debug_cs)
	{
		sigmaMW = SQRT(2 * SQR(mwNoise) + 0.05 + 0.5);
	}

	/* clock jump */
	if (fabs(fNw * lamw) > 10e-3 * CLIGHT)
	{
		fprintf(stdout,		"Potential clock jump rather than cycle slip -cs2\n");
		tracepde(1, trace,	"Potential clock jump rather than cycle slip -cs2\n");
	}

	double deltaGF	= lc12new.GF_Phas_m
					- lc12old.GF_Phas_m; 	/* Eq (9) in TN */

	tracepde(2, trace, "PDE-CS GPST DUAL  %4d %8.1f %4s %5.2f %5.3f %8.4f %7.4f %8.4f %6.3f                         ",
			week, sec, id, satStat.el * R2D, lamw, deltaGF, fNw, sigmaGF, sigmaMW);

	/* cycle slip detection */
	if (satStat.el >= acsConfig.elevation_mask)
	{
#ifdef DEBUGLOM
		/* cycle slip detection by MW */
		if (TEST&&fabs(fNw) > 0.6)
		{
			tracepdeex(2,trace,"detected by MW12 ");
			satStat.sigStatMap[F1].slip.MW = true;
			satStat.sigStatMap[frq2].slip.MW = true;
		}
		else
			scdia(trace, satStat, lcBase, lam, sigmaPhase, sigmaCode, 2, sys, 1, timeStamp);
#else
		double t1 = utc2gpst(timeget());

		/* cycle slip detection by MW */
		if (fabs(fNw) > (4 * sigmw / lamw))
		{
			tracepdeex(2, trace, "detected by MW12 ");
			satStat->slip[0] |= 1;
			satStat->slip[1] |= 1;
		}
		/* cycle slip detection by GF when identical slips on both frequency */
		else if (fabs(gf) > (4 * siggf + coef * sigiono))
		{
			tracepdeex(2, trace, "detected by GF12 ");
			satStat->slip[0] |= 1;
			satStat->slip[1] |= 1;
		}

		*timeStamp = timediff(utc2gpst(timeget()), t1);

		/* integer cycle slip resolution by Ps-LAMBDA */
		if (satStat->slip[0]
			|| satStat->slip[1])
		{
			scdia(trace, satStat, lc, lam, sigphase, sigcode, 2, sys, 1, timeStamp);
		}

#ifdef DEBUGCSACC
		else if (satStat->flt.slip == 1)	//todo need to and
		{
			tracepdeex(2,trace,"                 ");

		}
#endif
		else
			tracepdeex(2, trace, "\n");
#endif
	}
	else
		tracepdeex(2, trace, "\n");

	/* update TD ionosphere residual */
	if	( satStat.sigStatMap[F1].slip.any == 0
		&&satStat.sigStatMap[frq2].slip.any == 0)
	{
		satStat.dIono		= deltaGF	/ coef;
		satStat.sigmaIono	= sigmaGF	/ coef;
	}

#ifdef DEBUGCSACC
	if (config.debug_csacc)
	{
		if	( satStat.slip[0]==2
			&&satStat.slip[1]==2)
		{
			satStat.flt.slip=0;
			satStat.flt.ne	=0;
		}
		/* lc information update */
		if	( satStat.slip[0]==1
			&&satStat.slip[1]==1)
		{
			/* detected but not repaired */
			satStat.flt.lc_pre=satStat.lc;
			satStat.flt.slip=1;
			satStat.flt.ne	=1;
		}
		/* multi-epoch cycle slip repair */
		if	( satStat.slip[0]==0
			&&satStat.slip[1]==0
			&&satStat.flt.slip==1)
		{
			scdia(trace, satStat, lc, lam, sigphase, sigcode, 2, sys, 0);
		}
	}
#endif

	return;
}

/** Cycle slip detection and repair for triple-frequency
*/
void cycleslip3(
	Trace&		trace,			///< Trace to output to
	SatStat&	satStat,		///< Persistant satellite status parameters
	lc_t&		lc,				///< Linear combinations
	Obs&		obs,			///< Navigation object for this satellite
	double*		timeStamp)		///< Timestamp
{
	int week;
	double sec	= time2gpst(lc.time, &week);
	double dt	= timediff(lc.time, satStat.lc_pre.time);

	/* small interval */
	if (dt < 20)
	{
		satStat.dIono = 0;

		/* approximation of ionosphere residual */
		satStat.sigmaIono = acsConfig.proc_noise_iono * SQRT(dt);
	}
	else
	{
		/* large interval */
		if (satStat.sigmaIono == 0)
		{
			satStat.sigmaIono = acsConfig.proc_noise_iono * SQRT(dt);
		}
	}

	if (satStat.sigmaIono == 0)
	{
		satStat.sigmaIono = 0.001;
	}

	int sys = lc.Sat.sys;

	E_FType frq2=F2,frq3=F5;
	if(obs.Sat.sys == +E_Sys::GAL)
	{
		frq2=F5; frq3=F7;
	}

	auto&	lam = obs.satNav_ptr->lamMap;
	double lam1 = lam[F1];
	double lam2 = lam[frq2];
	double lam5 = lam[frq3];
	
	/* TD MW noise (m) */
	double lamew = lam2 * lam5 / (lam5 - lam2);
	if (lamew < 0)
		lamew *= -1;

	/* elevation dependent noise */
	double sigmaCode	= sqrt(obs.Sigs.begin()->second.codeVar);
	double sigmaPhase	= sqrt(obs.Sigs.begin()->second.phasVar);

	double mwNoise12 = mwnoise(sigmaCode, sigmaPhase, lam1, lam2);
	double mwNoise15 = mwnoise(sigmaCode, sigmaPhase, lam1, lam5);
	double mwNoise25 = mwnoise(sigmaCode, sigmaPhase, lam2, lam5);

	double sigmaEMW;
	if (acsConfig.debug_cs)
	{
		sigmaCode	= 2;
		sigmaPhase	= 0.02;
		sigmaEMW	= SQRT(2 * SQR(mwNoise25) + 0.05 + 0.5);
	}

	double sigmaGF = 2 * sigmaPhase; /* TD GF noise */

	S_LC lc25new = getLC(lc, 				frq2, frq3);
	S_LC lc25pre = getLC(satStat.lc_pre,	frq2, frq3);

	/* averaged EMW measurement and noise */
	double fNew;
	if (satStat.emwSlip.num >= 15 && 0)
	{
		fNew = lc25new.MW_c- satStat.emwSlip.mean;
		sigmaEMW = SQRT(2) * satStat.emwSlip.sigma * lamew;
	}
	else
	{
		fNew = lc25new.MW_c - lc25pre.MW_c; /* Eq (13) in TN */
		sigmaEMW = SQRT(2) * mwNoise25;
	}

	double deltaGF25	= lc25new.GF_Phas_m
						- lc25pre.GF_Phas_m;

	/* clock jump */
	if (fabs(fNew * lamew) > 10e-3 * CLIGHT)
	{
		fprintf(stdout, "Potential clock jump rather than cycle slip -cs3\n");
		return;
	}

	/* ionosphere coefficient for L2 & L5 */
	double coef1 = SQR(CLIGHT / lam1) / SQR(CLIGHT / lam5) - SQR(CLIGHT / lam1) / SQR(CLIGHT / lam2);
	if (coef1 < 0)
		coef1 = -coef1;

	S_LC lc15new = getLC(lc, 				F1, frq3);
	S_LC lc15pre = getLC(satStat.lc_pre,	F1, frq3);
	S_LC lc12new = getLC(lc, 				F1, frq2);
	S_LC lc12pre = getLC(satStat.lc_pre,	F1, frq2);

	double deltaGF;
	double fNw;
	double coef;
	double sigmaMW;
	double lamw;
	/* wide-lane with longer wavelength, note for the IGNSS paper */
	if (sys == E_Sys::CMP && 1)
	{
		fNw		= lc15new.MW_c
				- lc15pre.MW_c;

		deltaGF	= lc15new.GF_Phas_m
				- lc15pre.GF_Phas_m;

		lamw = lam1 * lam5 / (lam5 - lam1);

		/* ionosphere coefficient for L1 & L5 */
		coef = SQR(CLIGHT / lam1) / SQR(CLIGHT / lam5) - 1;
		if (satStat.mwSlip.num >= 15 && 0)
		{
			sigmaMW = SQRT(2) * satStat.mwSlip.sigma * lamw;
		}
		else
		{
			sigmaMW = SQRT(2) * mwNoise12;
		}

		if (acsConfig.debug_cs)
		{
			sigmaMW = SQRT(2 * SQR(mwNoise15) + 0.05 + 0.5);
		}
	}
	else
	{
		deltaGF	= lc12new.GF_Phas_m
				- lc12pre.GF_Phas_m;

		lamw = lam1 * lam2 / (lam2 - lam1);

		/* averaged MW measurement and noise */
		if (satStat.mwSlip.num >= 15 && 0)
		{
			fNw = lc12new.MW_c - satStat.mwSlip.mean;
			sigmaMW = SQRT(2) * satStat.mwSlip.sigma * lamw;
		}
		else
		{
			fNw = lc12new.MW_c - lc12pre.MW_c; /* Eq (6) in TN */
			sigmaMW = SQRT(2) * mwNoise12;
		}

		if (acsConfig.debug_cs)
		{
			sigmaMW = SQRT(2 * SQR(mwNoise12) + 0.05 + 0.5);
		}

		/* ionosphere coefficient for L1 & L2 */
		coef = SQR(CLIGHT / lam1) / SQR(CLIGHT / lam2) - 1;
	}

	tracepde(2, trace, "PDE-CS GPST TRIP  %4d %8.1f %4s %5.2f %5.3f %8.4f %7.4f %8.4f %6.3f %6.2f %8.4f %7.4f ", week,
			sec, lc.Sat.id().c_str(), satStat.el * R2D, lamw, deltaGF, fNw, sigmaGF, sigmaMW, lamew, deltaGF25, fNew);

	if (satStat.el >= acsConfig.elevation_mask)
	{
#ifdef DEBUGLOM
		/* extra wide-lane */
		if (TEST&&fabs(fNew)>0.5)
		{
			tracepdeex(2,trace,"detected by EMW  ");
			satStat.sigStatMap[F1].slip.EMW = true;
			satStat.sigStatMap[frq2].slip.EMW = true;
			satStat.sigStatMap[frq3].slip.EMW = true;
		}
		/* wide-lane */
		else if (TEST&&fabs(fNw)>0.6)
		{
			tracepdeex(2,trace,"detected by MW12 ");
			satStat.sigStatMap[F1].slip.MW = true;
			satStat.sigStatMap[frq2].slip.MW = true;
			satStat.sigStatMap[frq3].slip.MW = true;
		}
		else
			scdia(trace, satStat, lc, lam, sigmaPhase, sigmaCode, 3, sys, 1, timeStamp);
#else
		t1 = utc2gpst(timeget());
		/* extra wide-lane */
#ifndef DEBUGCS
		if (fabs(fNew) > PDESLIPTHRESHOLD)
		{
#else
		if (fabs(fNew)>4*sigemw/lamew) 
		{
#endif
			tracepdeex(2, trace, "detected by EMW  ");
			satStat.sigStatMap[F1].slip.EMW = true;
			satStat.sigStatMap[frq2].slip.EMW = true;
			satStat.sigStatMap[frq3].slip.EMW = true;
		}
		else if (fabs(gf25) > 4 * siggf + coef1 * sigiono)
		{
			tracepdeex(2, trace, "detected by GF25 ");
			satStat.sigStatMap[F1].slip.GF = true;
			satStat.sigStatMap[frq2].slip.GF = true;
			satStat.sigStatMap[frq3].slip.GF = true;
		}
		/* wide-lane */
		else if (fabs(fNw) > 4 * sigmw / lamw)
		{
			tracepdeex(2, trace, "detected by MW12 ");
			satStat.sigStatMap[F1].slip.MW = true;
			satStat.sigStatMap[frq2].slip.MW = true;
			satStat.sigStatMap[frq3].slip.MW = true;
		}
		else if (fabs(gf) > 4 * siggf + coef * sigiono)
		{
			tracepdeex(2, trace, "detected by GF12 ");
			satStat.sigStatMap[F1].slip.GF = true;
			satStat.sigStatMap[frq2].slip.GF = true;
			satStat.sigStatMap[frq3].slip.GF = true;
		}

		*timeStamp = timediff(utc2gpst(timeget()), t1);

		/* integer cycle slip resolution by Ps-LAMBDA */
		if	( satStat->slip[0].byte
			||satStat->slip[1].byte
			||satStat->slip[2].byte)
		{
			scdia(trace, satStat, lc, lam, sigphase, sigcode, 3, sys, 1, timeStamp);
		}

#ifdef DEBUGCSACC
		else if (ssat->flt.slip==1)
		{
			tracepdeex(2,trace,"                 ");
		}
#endif
		else
			tracepdeex(2, trace, "\n");

		/* further check the cycle slip repair results */
		if	( satStat.sigStatMap[F1].slip == 2
			&&satStat.sigStatMap[frq2].slip == 2
			&&satStat.sigStatMap[frq3].slip == 2)
		{
			/* false detection, set the slip to 0 (no cycle slip) */
			if	( satStat->amb[0] == 0
				&&satStat->amb[1] == 0
				&&satStat->amb[2] == 0)
			{
				satStat.sigStatMap[F1].slip.any = 0;
				satStat.sigStatMap[frq2].slip.any = 0;
				satStat.sigStatMap[frq3].slip.any = 0;
			}
		}
#endif
	}
	else
		tracepdeex(2, trace, "\n");

	/* update TD ionosphere residual */
	if	( satStat.sigStatMap[F1].slip.any == 0
		&&satStat.sigStatMap[frq2].slip.any == 0
		&&satStat.sigStatMap[frq3].slip.any == 0)
	{
		satStat.dIono		= deltaGF	/ coef;
		satStat.sigmaIono	= sigmaGF	/ coef;
	}

#ifdef DEBUGCSACC
	if (config.debug_csacc)
	{
		if	( satStat.slip[0]==2
			&&satStat.slip[1]==2
			&&satStat.slip[2]==2)
		{
			satStat.flt.slip=0;
			satStat.flt.ne	=0;
		}
		/* lc information update */
		if	( satStat.slip[0]==1
			&&satStat.slip[1]==1
			&&satStat.slip[2]==1)
		{
			/* detected but not repaired */
			satStat.flt.lc_pre = satStat.lc;
			satStat.flt.slip=1;
		}
		/* multi-epoch cycle slip repair */
		if	( satStat.slip[0]==0
			&&satStat.slip[1]==0
			&&satStat.slip[2]==0
			&&satStat.flt.slip==1)
		{
			scdia(trace, satStat, lc, lam, sigphase, sigcode, 3, sys, 0);
		}
	}
#endif

	return;
}

/** Cycle slip detection and repair
*/
void detectslip(
			Trace&		trace,		///< Trace to output to
			SatStat&	satStat,	///< Persistant satellite status parameters
			lc_t&		lc_new,		///< Linear combination for this epoch
			lc_t&		lc_old,		///< Linear combination from previous epoch
			Obs&		obs)	///< Navigation object for this satellite
{
	/* Note satellite elevation & lli */
	char id[32];
	double ts = 0;

	bool dualFreq = false;
	int sys = lc_new.Sat.sys;
	lc_new.Sat.getId(id);

	int week;
	double sec = time2gpst(lc_new.time, &week);

	E_FType frq2=F2,frq3=F5;
	if(obs.Sat.sys == +E_Sys::GAL)
	{
		frq2=F5; frq3=F7;
	}
	
	
	/* SBS and LEO are not included */
	if  ( acsConfig.process_sys[sys] == false
		||sys == E_Sys::SBS
		||sys == E_Sys::LEO)
	{
		return;
	}

	/* initialize the amb parameter each epoch */
	for (auto& [key, sigStat] : satStat.sigStatMap)
	{
		if (key < 3)
			satStat.amb[key] = 0;	//todo aaron, yuk
	}

	/* first epoch or large gap or low elevation */			//todo aaron initialisation stuff, remove
	if  ( satStat.lc_pre.time.time == 0
		||satStat.el < acsConfig.elevation_mask
		||timediff(lc_new.time, lc_old.time) > PDEGAP)
	{
		satStat.mwSlip	= {};
		satStat.emwSlip	= {};

		if (timediff(lc_new.time, lc_old.time) > PDEGAP)	tracepde(1, trace, "PDE-CS GPST       %4d %8.1f %4s %5.2f --time gap --\n", 				week, sec, id, satStat.el * R2D);
		if (satStat.el < acsConfig.elevation_mask)			tracepde(1, trace, "PDE-CS GPST       %4d %8.1f %4s %5.2f --low_elevation --\n", 			week, sec, id, satStat.el * R2D);
		if (satStat.el > acsConfig.elevation_mask)			tracepde(1, trace, "PDE-CS GPST       %4d %8.1f %4s %5.2f --satStat.lc_pre.time.time --\n", week, sec, id, satStat.el * R2D);

		return;
	}

	if  ( acsConfig.csfreq == 3
		&&lc_new.L_m[F1] != 0
		&&lc_new.L_m[frq2] != 0
		&&lc_new.L_m[frq3] == 0)
	{
		dualFreq = true;
	}

	if  ( acsConfig.csfreq != 3
		&&lc_new.L_m[F1] != 0
		&&lc_new.L_m[frq2] != 0)
	{
		/* using dual-frequency only */
		dualFreq = true;
	}

	if  ( dualFreq
		&&lc_old.L_m[F1] != 0
		&&lc_old.L_m[frq2] != 0)
	{
		cycleslip2(trace, satStat, lc_new, obs, &ts);

		/* update averaged MW noise when no cycle slip */
		if	( satStat.sigStatMap[F1].slip.any == 0
			&&satStat.sigStatMap[frq2].slip.any == 0)
		{
			S_LC& lc12new = getLC(lc_new, F1, frq2);
			S_LC& lc12old = getLC(lc_old, F1, frq2);
			mwaverage(lc12new.MW_c, lc12old.MW_c, satStat.mwSlip);
		}
		else
		{
			satStat.mwSlip = {};
		}
	}
	/* track L5 again */
	else if ( lc_new.L_m[F1]   != 0
			&&lc_new.L_m[frq2] != 0
			&&lc_new.L_m[frq3] != 0
			&&lc_old.L_m[F1]   != 0
			&&lc_old.L_m[frq2] != 0
			&&lc_old.L_m[frq3] == 0)	//was zero, now not.
	{
		/* set slip flag for L5 (introduce new ambiguity for L5) */
		satStat.sigStatMap[frq3].slip.LLI = true;
		cycleslip2(trace, satStat, lc_new, obs, &ts);

		/* update averaged MW noise when no cycle slip */
		if	( satStat.sigStatMap[F1].slip.any == 0
			&&satStat.sigStatMap[frq2].slip.any == 0)
		{
			S_LC& lc12new = getLC(lc_new, F1, frq2);
			S_LC& lc12old = getLC(lc_old, F1, frq2);
			mwaverage(lc12new.MW_c, lc12old.MW_c, satStat.mwSlip);
		}
		else
		{
			satStat.mwSlip = {};
		}
	}
	/* Triple-frequency */
	else if ( lc_new.L_m[F1]   != 0
			&&lc_new.L_m[frq2] != 0
			&&lc_new.L_m[frq3] != 0
			&&lc_old.L_m[F1]   != 0
			&&lc_old.L_m[frq2] != 0
			&&lc_old.L_m[frq3] != 0)
	{
		cycleslip3(trace, satStat, lc_new, obs, &ts);

		//todo aaron, recent addition by mike? dont know why
		if (satStat.el * R2D > 30)
		{
			if	( satStat.sigStatMap[F1].slip.any	== 2	//todo aaron, check the 2
				&&satStat.amb[0]				== 0
				&&satStat.amb[1]				== 0
				&&satStat.amb[2]				== 0)
			{
				satStat.sigStatMap[F1]  .slip.any = 0;
				satStat.sigStatMap[frq2].slip.any = 0;
				satStat.sigStatMap[frq3].slip.any = 0;
			}
		}

		/*update averaged MW25 noise when no cycle slip */
		if	( satStat.sigStatMap[F1]  .slip.any == 0
			&&satStat.sigStatMap[frq2].slip.any == 0
			&&satStat.sigStatMap[frq3].slip.any == 0)
		{
			S_LC& lc25new = getLC(lc_new, frq2, frq3);
			S_LC& lc25old = getLC(lc_old, frq2, frq3);
			mwaverage(lc25new.MW_c, lc25old.MW_c, satStat.emwSlip);
		}
		else
		{
			satStat.emwSlip = {};
		}
	}
	/* track L1 or L2 again, new rising satellite */
	else if ( dualFreq
			&&( lc_old.L_m[F1] == 0
			||lc_old.L_m[frq2] == 0))
	{
		satStat.flt.slip	= 0;
		satStat.flt.ne		= 0;
		for (auto& [key, sigStat] : satStat.sigStatMap)
		{
			sigStat.slip.LLI = true;
		}

		tracepde(1, trace, "PDE-CS GPST       %4d %8.1f %4s %5.2f --  re-tracking   --\n", week, sec, id, satStat.el * R2D);
	}
	else
	{
		satStat.flt.slip	= 0;
		satStat.flt.ne		= 0;
		for (auto& [key, sigStat] : satStat.sigStatMap)
		{
			sigStat.slip.LLI = true;
		}

		tracepde(1, trace, "PDE-CS GPST       %4d %8.1f %4s %5.2f --single frequency--\n", week, sec, id, satStat.el * R2D);
	}
}

/** Detect slips for multiple observations
*/
void detectslips(
	Trace&		trace,		///< Trace to output to
	ObsList&	obsList)	///< List of observations to detect slips within
{
	//clear non-persistent status values.
	for (auto& obs					: obsList)
	for (auto& [sigKey, sigStat]	: obs.satStat_ptr->sigStatMap)
	{
		sigStat.slip.any = 0;
	}

	tracepdeex(2, trace, "\n   *-------- PDE cycle slip detection & repair --------*\n");
	tracepde  (2, trace, "PDE-CS GPST       week      sec  prn   el   lamw     gf12    mw12    siggf  sigmw  lamew     gf25    mw25               LC                   N1   N2   N5\n");

	/* detect cycle slip by LLI */
	detslp_ll(obsList);

	/* detect cycle slip by geometry-free phase jump */
	detslp_gf(obsList);

	/* detect slip by Melbourne-Wubbena linear combination jump */
	detslp_mw(obsList);

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		TestStack ts(obs.Sat);
		SatStat& satStat = *(obs.satStat_ptr);
		detectslip(trace, satStat, satStat.lc_new, satStat.lc_pre, obs);
	}
}

/** Clock jump detection and repair
*/
void detectjump(
	Trace&		trace,		///< Trace to output to
	ObsList&	obsList,	///< List of observations to detect jumps within
	double		elmin,		///< Minimum elevation (rad)
	ClockJump&	cj)			///< Clock jump object
{
	int		nj		= 0;		//todo aaron, this function is repeated in ppp?
	double 	sig		= 5;
	double 	sum		= 0;
	double 	sump	= 0;
	double 	suml	= 0;
	double 	nk		= 0;

	tracepdeex(3, trace, "\n   *-------- PDE Clock jump detection & repair --------*\n");
	int nsat = obsList.size();
	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		int sys = obs.Sat.sys;
		E_FType frq2=F2;
		if(sys == E_Sys::GAL) frq2=F5;

		auto& satStat = *(obs.satStat_ptr);
		lc_t& lc_pre = satStat.lc_pre;
		lc_t& lc_new = satStat.lc_new;
		int week;
		int ind = 0;

		/*------PDE clock jump-----low elevation */
		if (satStat.el < elmin)
		{
			nsat--;
			continue;
		}

		/*------PDE clock jump-----First epoch */
		if  ( lc_pre.time.time == 0
			||sys != E_Sys::GPS
			||timediff(lc_new.time, lc_pre.time) > PDEGAP)
		{
			nsat--;
			continue;
		}
		else
		{
			double 	dL = 0;
			int 	nf = 0;

			/* only cycle slip free satellites */
			if  ( satStat.sigStatMap[F1].slip.any == 0
				&&satStat.sigStatMap[frq2].slip.any == 0)
			{
				/* L1 */
				if  ( lc_pre.L_m[F1] != 0
					&&lc_new.L_m[F1] != 0)
				{
					dL += lc_pre.L_m[F1]
						- lc_new.L_m[F1];
					nf++;										//todo aaron, put in loop?
				}
				/* L2 */
				if  ( lc_pre.L_m[frq2] != 0
					&&lc_new.L_m[frq2] != 0)
				{
					dL += lc_pre.L_m[frq2]
						- lc_new.L_m[frq2];			//todo aaron, should this do L5?
					nf++;
				}
			}
			else
			{
				nsat--;
				ind = 1;
				continue;
			}

			if (nf == 0 && ind == 0)
			{
				nsat--;
				continue;
			}

			dL /= nf;				//average difference for all frequencies

			double 	dP = 0;
					nf = 0;
			/* P1 */
			if	( lc_pre.P[F1] != 0
				&&lc_new.P[F1] != 0)
			{
				dP += lc_pre.P[F1]
					- lc_new.P[F1];
				nf++;
			}
			/* P2 */
			if	( lc_pre.P[frq2] != 0
				&&lc_new.P[frq2] != 0)
			{
				dP += lc_pre.P[frq2]
					- lc_new.P[frq2];
				nf++;
			}

			dP /= nf;		//average difference for all frequencies

			double S = dP - dL;

			nk++;
			sump += dP;
			suml += dL;

			double k = 1e-3 * CLIGHT - 3 * sig;
			tracepde(3, trace, "PDE-CJ DIFF=%10.2f %10.2f %10.2f \n", fabs(S), dP, dL);
			if (fabs(S) > k)
			{
				nj++;
				sum += S;
			}
		}
	}

	tracepde(3, trace, "PDE-CJ valid=%d used=%d\n", nj, nsat);

	double Js = 0;
	if 	( nj != 0
		&&nj == nsat)
	{
		/* Clock jump occurred */
		double M = sum / nsat * 1e3 / CLIGHT;
		tracepde(3, trace, "PDE-CJ float value, %lf\n", M);
		if (fabs(M - ROUND(M) <= 1e-5))
		{
			Js = ROUND(M);
			tracepde(2, trace, "PDE-CJ clock jump = %4d (ms)  type = 0 \n", Js);
		}
		else
			Js = 0;

		cj.type = 0;
	}
	else
	{
		if (nk != 0)
		{
			double t1 = ROUND(sump/nk/(CLIGHT/1000));
			double t2 = ROUND(suml/nk/(CLIGHT/1000));

			if (t1 != 0 && t2 != 0)
			{
				Js = t1;
				tracepde(2, trace, "PDE-CJ clock jump = %4d (ms)  type = 1 \n", Js);
			}
		}

		cj.type = 1;
	}

	cj.msJump += Js;

	return;
}
