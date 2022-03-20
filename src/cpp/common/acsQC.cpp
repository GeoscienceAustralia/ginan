
// #pragma GCC optimize ("O0")

#include <iostream>
#include <vector>

#include "observations.hpp"
#include "streamTrace.hpp"
#include "acsConfig.hpp"
#include "testUtils.hpp"
#include "constants.hpp"
#include "satStat.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "acsQC.hpp"
#include "lambda.h"
#include "enums.h"

#define		THRES_MW_JUMP		10.0
#define     PDEGAP  			60.0
#define     PDESLIPTHRESHOLD	0.5

/** Detect cycle slip by reported loss of lock
*/
void detslp_ll(
	Trace&		trace,		///< Trace to output to
	ObsList&	obsList)	///< List of observations to detect slips within
{
	tracepdeex(5, trace, "detslp_ll: n=%d", obsList.size());

	for (auto& obs			: obsList)
	for (auto& [ft, sig]	: obs.Sigs)
	{
		if (obs.exclude)
		{
			continue;
		}

		int f = ft;
		if	( sig.L == 0
			|| (sig.LLI & 0x03) == 0)
		{
			continue;
		}

		tracepdeex(3, trace, "detslp_ll: slip detected sat=%s f=F%d\n", obs.Sat.id().c_str(), ft);

		obs.satStat_ptr->sigStatMap[ft].slip.LLI = true;
	}
}

/** Detect cycle slip by geometry free phase jump
*/
void detslp_gf(
	Trace&		trace,		///< Trace to output to
	ObsList&	obsList)	///< List of observations to detect slips within
{
	tracepdeex(5, trace, "detslp_gf: n=%d", obsList.size());

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}
		
		E_FType	frq1 = F1;
		E_FType	frq2;
		E_FType frq3;
		if (obs.Sat.sys == +E_Sys::GPS && acsConfig.ionoOpts.iflc_freqs == +E_LinearCombo::L1L5_ONLY) 	{	frq2=F5;	frq3=F7;	}
		if (obs.Sat.sys == +E_Sys::GAL)																	{	frq2=F5;	frq3=F7;	}
		else																							{	frq2=F2;	frq3=F5;	}

		S_LC& lc = getLC(obs.satStat_ptr->lc_new, frq1, frq2);

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

		tracepdeex(5, trace, "detslip_gf: sat=%s gf0=%f gf1=%f", obs.Sat.id().c_str(), gf0, gf1);

		if (fabs(gf1 - gf0) > acsConfig.thres_slip)
		{
			tracepdeex(3, trace, "detslip_gf: slip detected: sat=%s gf0=%f gf1=%f", obs.Sat.id().c_str(), gf0, gf1);

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
	Trace&		trace,		///< Trace to output to
	ObsList&	obsList)	///< List of observations to detect slips within
{
	tracepdeex(5, trace, "detslp_mw: n=%d",  obsList.size());

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}
		
		E_FType	frq1 = F1;
		E_FType	frq2;
		E_FType frq3;
		if (obs.Sat.sys == +E_Sys::GPS && acsConfig.ionoOpts.iflc_freqs == +E_LinearCombo::L1L5_ONLY) 	{	frq2=F5;	frq3=F7;	}
		if (obs.Sat.sys == +E_Sys::GAL)																	{	frq2=F5;	frq3=F7;	}
		else																							{	frq2=F2;	frq3=F5;	}
		
		S_LC& lc = getLC(obs.satStat_ptr->lc_new, frq1, frq2);

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

		tracepdeex(5, trace, "detslip_mw: sat=%s mw0=%f mw1=%f", obs.Sat.id().c_str(), mw0, mw1);

		if (fabs(mw1 - mw0) > THRES_MW_JUMP)
		{
			tracepdeex(3, trace, "detslip_mw: slip detected: sat=%s mw0=%f mw1=%f", obs.Sat.id().c_str(), mw0, mw1);

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
	E_FilterMode		filterMode)	///< LSQ/Kalman filter flag
{
	E_FType	frq1 = F1;
	E_FType	frq2;
	E_FType frq3;
	if (sys == +E_Sys::GPS && acsConfig.ionoOpts.iflc_freqs == +E_LinearCombo::L1L5_ONLY) 	{	frq2=F5;	frq3=F7;	}
	if (sys == +E_Sys::GAL)																	{	frq2=F5;	frq3=F7;	}
	else																					{	frq2=F2;	frq3=F5;	}

	if (nf == 0)
		return;
	

	lc_t* lc_pre_ptr;
	
	if (filterMode == +E_FilterMode::LSQ)	lc_pre_ptr = &satStat.		lc_pre;
	else									lc_pre_ptr = &satStat.flt.	lc_pre;
	if (nf == 1)							lc_pre_ptr = &satStat.flt.	lc_pre;
	
	auto& lc_pre = *lc_pre_ptr;
	
	/* single frequency not supported in current PDE */
	if (nf == 1)
	{
		return;
	}
	
	E_FType ftypes[3] = {frq1, frq2, frq3};
	
	/* m-rows measurements, n-cols unknowns */
	int m = 2 * nf + 1;
	int n = 2 + nf;
	VectorXd Z = VectorXd::Zero		(m);
	MatrixXd R = MatrixXd::Identity	(m, m);
	MatrixXd H = MatrixXd::Zero		(m, n);
	
	double	lam1	= lam[frq1];
	int		i		= 0;
	
	//phase and code
	for (int f = 0; f < nf; f++)
	{
		E_FType	frqX = ftypes[f];
		double	lamX = lam[frqX];
		
		Z[i]	= lc	.L_m[frqX] 
				- lc_pre.L_m[frqX];		R(i,i) = 1 / (2 * SQR(sigmaPhase));		H(i,0)		= 1;
																				H(i,1)		= -SQR(lamX) / SQR(lam1);	
																				H(i,2 + f)	= lamX;							i++;	
																								
		Z[i]	= lc	.P  [frqX]	
				- lc_pre.P	[frqX];		R(i,i) = 1 / (2 * SQR(sigmaCode));		H(i,0)		= 1;
																				H(i,1)		= +SQR(lamX) / SQR(lam1);		i++;
	}
	
	//ionosphere
	{		
		Z[i] = satStat.dIono;			R(i,i) = 1 / SQR(satStat.sigmaIono);	H(i,1)		= 1;							i++;
	}

	/* perform LOM test for outlier detection */
	/* design matrix for LOM test */
	MatrixXd Hlom = H.leftCols(2);
	VectorXd v = VectorXd::Zero	(m);
	int ind = lsqqc(trace, Hlom.data(), R.data(), Z.data(), v.data(), m, 2, 0, 0);
	if (ind == 0)
	{
		tracepdeex(2,trace,"\n");
		return;
	}
	
					satStat.sigStatMap[frq1].slip.SCDIA = true;
					satStat.sigStatMap[frq2].slip.SCDIA = true;
	if (nf == 3)	satStat.sigStatMap[frq3].slip.SCDIA = true;
	
	VectorXd xp	= VectorXd::Zero(n);
	MatrixXd Pp	= MatrixXd::Zero(n, n);
	
	if (filterMode == +E_FilterMode::LSQ)
	{
		MatrixXd N		= MatrixXd::Zero	(n, m);
		VectorXd N1		= VectorXd::Zero	(n);
		matmul("TN", n, m, m, 1, H.data(), R.data(), 0, N.data());	/* H'*R */
		matmul("NN", n, n, m, 1, N.data(), H.data(), 0, Pp.data());	/* H'*R*H */
		matmul("NN", n, 1, m, 1, N.data(), Z.data(), 0, N1.data());	/* Nl=H'*R*Z */
		if (!matinv(Pp.data(), n))
		{
			matmul("NN", n, 1, n, 1, Pp.data(), N1.data(), 0, xp.data());
		}
		/* store float solution and vc matrix */
		matcpy(satStat.flt.a, xp.data() + 2, 1, nf);

		for (int i = 0; i < nf; i++)
		for (int j = 0; j < nf; j++)
			satStat.flt.Qa[i][j] = Pp.data()[(i + 2) * n + j + 2];
	}
	else
	{
		satStat.flt.ne++;
		if (satStat.flt.ne < 2)	
		{
			satStat.flt.slip	= 0;
			satStat.flt.ne		= 0;
			tracepdeex(2, trace, "\n");
			return;
		}

		VectorXd x		= VectorXd::Zero	(n);
		matcpy(x.data() + 2, satStat.flt.a, 1, nf);
		
		/* time update */
		MatrixXd Px = MatrixXd::Zero(n, n);
		for (int i = 0; i < nf; i++)
		for (int j = 0; j < nf; j++)
			Px.data()[(i + 2) * n + j + 2] = satStat.flt.Qa[i][j];

		Px.data()[0]		= 1E6;
		Px.data()[1 + n]	= 1E6;
		
		/* measurement-prediction */
		matmul("NN", m, 1, n, -1, H.data(), x.data(), 1, Z.data());
		
		/* transpose of desgin matrix */
		MatrixXd I		= MatrixXd::Identity(m, m);
		MatrixXd H1		= MatrixXd::Zero	(n, m);
		matmul("TN", n, m, m, +1, H.data(), I.data(), 0, H1.data());
		
		/* measurement update */
		if (!matinv(R.data(), m))
			filter_(x.data(), Px.data(), H1.data(), Z.data(), R.data(), n, m, xp.data(), Pp.data());

		matcpy(satStat.flt.a, xp.data() + 2, 1, nf);

		for (int i = 0; i < nf; i++)
		for (int j = 0; j < nf; j++)
			satStat.flt.Qa[i][j] = Pp.data()[(i + 2) * n + j + 2];
	}

	/* ambiguity vector and its variance */
	VectorXd a = VectorXd::Zero(nf);
	matcpy(a.data(), xp.data() + n - nf, nf, 1);

	MatrixXd Qa	= MatrixXd::Zero(nf, nf);
	for (int i = 0; i < nf; i++)
	for (int j = 0; j < nf; j++)
	{
		Qa.data()[i * nf + j] = Pp.data()[(n - nf + i) * n + j + n - nf];
	}

	/* integer cycle slip estimation */
	MatrixXd F	= MatrixXd::Zero(nf, 2);
	bool pass;
	double s[2];
	lambda(trace, nf, 2, a.data(), Qa.data(), F.data(), s, acsConfig.predefined_fail, pass);
	
	if (filterMode == +E_FilterMode::LSQ)
	{
		/* least-squares */
		satStat.amb[0] = 0;
		satStat.amb[1] = 0;
		satStat.amb[2] = 0;
		tracepdeex(2, trace, "(freq=%d) ", nf);
		if (pass)
		{
			tracepdeex(2, trace, "fixed ");
// 				tracematpde(2, trace, F, 1, nf, 4, 1);
			for (int i = 0; i < 3; i++)
				satStat.amb[i] = ROUND(F.data()[i]);
			
			for (auto& [key, sigStat] : satStat.sigStatMap)
			{
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
		if (pass)
		{
			memset(satStat.flt.a, 0, 3 * sizeof (double));
			memset(satStat.flt.Qa, 0, 9);				//todo aaron, looks sketchy
			satStat.flt.slip |= 2;
			tracepdeex(1, trace, "     ACC fixed ");
// 				tracematpde(1, trace, F, 1, nf, 4, 1);
			for (int i = 0; i < nf; i++)
			{
				satStat.flt.amb[i] = ROUND(F.data()[i]);
			}
		}
		tracepdeex(2, trace, "\n");
		tracepde(1, trace, "ACC epoch used=%2d\n", satStat.flt.ne);
		if (pass)
			satStat.flt.ne = 0;
	}
}

/** Cycle slip detection and repair for dual-frequency
*/
void cycleslip2(
	Trace&		trace,		///< Trace to output to
	SatStat&	satStat,	///< Persistant satellite status parameters
	lc_t&		lcBase,		///< Linear combinations
	Obs&		obs)		///< Navigation object for this satellite
{
	int week;
	double sec	= time2gpst(lcBase.time, &week);
	double dt	= lcBase.time - satStat.lc_pre.time;

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

	int sys = lcBase.Sat.sys;
		
	E_FType	frq1 = F1;
	E_FType	frq2;
	E_FType frq3;
	if (obs.Sat.sys == +E_Sys::GPS && acsConfig.ionoOpts.iflc_freqs == +E_LinearCombo::L1L5_ONLY) 	{	frq2=F5;	frq3=F7;	}
	if (obs.Sat.sys == +E_Sys::GAL)																	{	frq2=F5;	frq3=F7;	}
	else																							{	frq2=F2;	frq3=F5;	}
	
	auto&	lam = obs.satNav_ptr->lamMap;

	double lam1 = lam[frq1];
	double lam2 = lam[frq2];

	double lamw = lam1 * lam2 / (lam2 - lam1);	//todo aaron, rename

	/* ionosphere coefficient */
	double coef = SQR(lam2) / SQR(lam1) - 1;

	/* elevation dependent noise */
	double sigmaCode	= sqrt(obs.Sigs.begin()->second.codeVar);
	double sigmaPhase	= sqrt(obs.Sigs.begin()->second.phasVar);

	double sigmaGF = 2 * sigmaPhase;

	S_LC lcNew = getLC(lcBase, 			frq1, frq2);
	S_LC lcPre = getLC(satStat.lc_pre,	frq1, frq2);

	double mwNoise = mwnoise(sigmaCode, sigmaPhase, lam1, lam2);

	/* averaged MW measurement and noise */
	double fNw;
	if (acsConfig.mw_proc_noise)	{	fNw = lcNew.MW_c - satStat.mwSlip.mean;	}	
	else							{	fNw = lcNew.MW_c - lcPre.MW_c;			}	/* Eq (6) in TN */	

	/* clock jump */
	if (fabs(fNw * lamw) > 10e-3 * CLIGHT)
	{
		fprintf(stdout,		"Potential clock jump rather than cycle slip -cs2\n");
		tracepde(1, trace,	"Potential clock jump rather than cycle slip -cs2\n");
	}

	double deltaGF	= lcNew.GF_Phas_m
					- lcPre.GF_Phas_m; 	/* Eq (9) in TN */

	tracepde(2, trace, "PDE-CS GPST DUAL  %4d %8.1f %4s %5.2f %5.3f %8.4f %7.4f %8.4f                                ",
			week, sec, lcBase.Sat.id().c_str(), satStat.el * R2D, lamw, deltaGF, fNw, sigmaGF);

	/* cycle slip detection */
	if (satStat.el >= acsConfig.elevation_mask)
	{
		scdia(trace, satStat, lcBase, lam, sigmaPhase, sigmaCode, 2, sys, E_FilterMode::LSQ);
	}
	else
		tracepdeex(2, trace, "\n");

	/* update TD ionosphere residual */
	if	( satStat.sigStatMap[frq1].slip.any == 0
		&&satStat.sigStatMap[frq2].slip.any == 0)
	{
		satStat.dIono		= deltaGF	/ coef;
		satStat.sigmaIono	= sigmaGF	/ coef;
	}
}

/** Cycle slip detection and repair for triple-frequency
*/
void cycleslip3(
	Trace&		trace,			///< Trace to output to
	SatStat&	satStat,		///< Persistant satellite status parameters
	lc_t&		lc,				///< Linear combinations
	Obs&		obs)			///< Navigation object for this satellite
{
	int week;
	double sec	= time2gpst(lc.time, &week);
	double dt	= lc.time - satStat.lc_pre.time;

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

	E_FType	frq1 = F1;
	E_FType	frq2;
	E_FType frq3;
																									{	frq2=F2;	frq3=F5;	}
	if (obs.Sat.sys == +E_Sys::GAL)																	{	frq2=F5;	frq3=F7;	}
	if (obs.Sat.sys == +E_Sys::GPS && acsConfig.ionoOpts.iflc_freqs == +E_LinearCombo::L1L5_ONLY) 	{	frq2=F5;	frq3=F7;	}

	auto&	lam = obs.satNav_ptr->lamMap;
	double lam1 = lam[frq1];
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


	double sigmaGF = 2 * sigmaPhase; /* TD GF noise */

	S_LC lc25new = getLC(lc, 				frq2, frq3);
	S_LC lc25pre = getLC(satStat.lc_pre,	frq2, frq3);

	/* averaged EMW measurement and noise */
	double fNew;
// 	double sigmaEMW;
	if (acsConfig.mw_proc_noise)	{	fNew = lc25new.MW_c - satStat.emwSlip.mean;		}	
	else							{	fNew = lc25new.MW_c - lc25pre.MW_c; 			}	/* Eq (13) in TN */

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

	/* wide-lane with longer wavelength, note for the IGNSS paper */
	E_FType	frqX;
	double	lamX;
	if (sys == E_Sys::BDS)		{	frqX = frq3;		lamX = lam5;	}	
	else						{	frqX = frq2;		lamX = lam2;	}
		
	S_LC	lcNew = getLC(lc, 				frq1, frqX);
	S_LC	lcPre = getLC(satStat.lc_pre,	frq1, frqX);

	double	lamw = lam1 * lamX / (lamX - lam1);

	double	coef = SQR(lamX) / SQR(lam1) - 1;	// ionosphere coefficient for L1 & LX
	
	/* averaged MW measurement and noise */
	double	fNw;
	if (acsConfig.mw_proc_noise)		{	fNw = lcNew.MW_c - satStat.mwSlip.mean;	}	
	else								{	fNw = lcNew.MW_c - lcPre.MW_c;			} /* Eq (6) in TN */	

	double	deltaGF	= lcNew.GF_Phas_m
					- lcPre.GF_Phas_m;
			
	tracepde(2, trace, "PDE-CS GPST TRIP  %4d %8.1f %4s %5.2f %5.3f %8.4f %7.4f %8.4f        %6.2f %8.4f %7.4f ", week,
			sec, lc.Sat.id().c_str(), satStat.el * R2D, lamw, deltaGF, fNw, sigmaGF, lamew, deltaGF25, fNew);

	if (satStat.el >= acsConfig.elevation_mask)
	{
		scdia(trace, satStat, lc, lam, sigmaPhase, sigmaCode, 3, sys, E_FilterMode::LSQ);
	}
	else
		tracepdeex(2, trace, "\n");

	/* update TD ionosphere residual */
	if	( satStat.sigStatMap[frq1].slip.any == 0
		&&satStat.sigStatMap[frq2].slip.any == 0
		&&satStat.sigStatMap[frq3].slip.any == 0)
	{
		satStat.dIono		= deltaGF	/ coef;
		satStat.sigmaIono	= sigmaGF	/ coef;
	}
}

/** Cycle slip detection and repair
*/
void detectslip(
			Trace&		trace,		///< Trace to output to
			SatStat&	satStat,	///< Persistant satellite status parameters
			lc_t&		lc_new,		///< Linear combination for this epoch
			lc_t&		lc_old,		///< Linear combination from previous epoch
			Obs&		obs)		///< Navigation object for this satellite
{
	bool dualFreq = false;
	E_Sys sys = lc_new.Sat.sys;
	
	char id[32];
	lc_new.Sat.getId(id);

	int week;
	double sec = time2gpst(lc_new.time, &week);

	E_FType	frq1 = F1;
	E_FType	frq2;
	E_FType frq3;
	if (obs.Sat.sys == +E_Sys::GPS && acsConfig.ionoOpts.iflc_freqs == +E_LinearCombo::L1L5_ONLY) 	{	frq2=F5;	frq3=F7;	}
	if (obs.Sat.sys == +E_Sys::GAL)																	{	frq2=F5;	frq3=F7;	}
	else																							{	frq2=F2;	frq3=F5;	}

	/* SBS and LEO are not included */
	if  ( acsConfig.process_sys[sys] == false
		||sys == +E_Sys::SBS
		||sys == +E_Sys::LEO)
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
	if  (  satStat.lc_pre.time.time == 0
		|| satStat.el	< acsConfig.elevation_mask
		|| lc_new.time	> lc_old.time + PDEGAP)
	{
		satStat.mwSlip	= {};
		satStat.emwSlip	= {};

		if (lc_new.time	> lc_old.time + PDEGAP)				tracepde(1, trace, "PDE-CS GPST       %4d %8.1f %4s %5.2f --time gap --\n", 				week, sec, id, satStat.el * R2D);
		if (satStat.el	< acsConfig.elevation_mask)			tracepde(1, trace, "PDE-CS GPST       %4d %8.1f %4s %5.2f --low_elevation --\n", 			week, sec, id, satStat.el * R2D);
		if (satStat.el	> acsConfig.elevation_mask)			tracepde(1, trace, "PDE-CS GPST       %4d %8.1f %4s %5.2f --satStat.lc_pre.time.time --\n", week, sec, id, satStat.el * R2D);

		return;
	}

	if  ( acsConfig.csfreq == 3
		&&lc_new.L_m[frq1] != 0
		&&lc_new.L_m[frq2] != 0
		&&lc_new.L_m[frq3] == 0)
	{
		dualFreq = true;
	}

	if  ( acsConfig.csfreq != 3
		&&lc_new.L_m[frq1] != 0
		&&lc_new.L_m[frq2] != 0)
	{
		dualFreq = true;
	}

	if  ( dualFreq
		&&lc_old.L_m[frq1] != 0
		&&lc_old.L_m[frq2] != 0)
	{
		cycleslip2(trace, satStat, lc_new, obs);

		/* update averaged MW noise when no cycle slip */
		if	( satStat.sigStatMap[frq1].slip.any == 0
			&&satStat.sigStatMap[frq2].slip.any == 0)
		{
			S_LC& lc12 = getLC(lc_new, frq1, frq2);
			lowPassFilter(satStat.mwSlip, lc12.MW_c, acsConfig.mw_proc_noise);
		}
		else
		{
			satStat.mwSlip = {};
		}
	}
	/* track L5 again */
	else if ( lc_new.L_m[frq1] != 0
			&&lc_new.L_m[frq2] != 0
			&&lc_new.L_m[frq3] != 0
			&&lc_old.L_m[frq1] != 0
			&&lc_old.L_m[frq2] != 0
			&&lc_old.L_m[frq3] == 0)	//was zero, now not.
	{
		/* set slip flag for L5 (introduce new ambiguity for L5) */
		satStat.sigStatMap[frq3].slip.LLI = true;
		cycleslip2(trace, satStat, lc_new, obs);

		/* update averaged MW noise when no cycle slip */
		if	( satStat.sigStatMap[frq1].slip.any == 0
			&&satStat.sigStatMap[frq2].slip.any == 0)
		{
			S_LC& lc12 = getLC(lc_new, frq1, frq2);
			lowPassFilter(satStat.mwSlip, lc12.MW_c, acsConfig.mw_proc_noise);
		}
		else
		{
			satStat.mwSlip = {};
		}
	}
	/* Triple-frequency */
	else if ( lc_new.L_m[frq1] != 0
			&&lc_new.L_m[frq2] != 0
			&&lc_new.L_m[frq3] != 0
			&&lc_old.L_m[frq1] != 0
			&&lc_old.L_m[frq2] != 0
			&&lc_old.L_m[frq3] != 0)
	{
		cycleslip3(trace, satStat, lc_new, obs);

		//todo aaron, recent addition by mike? dont know why
		if (satStat.el * R2D > 30)
		{
			if	( satStat.sigStatMap[frq1].slip.any	== 2	//todo aaron, check the 2
				&&satStat.amb[0]				== 0
				&&satStat.amb[1]				== 0
				&&satStat.amb[2]				== 0)
			{
				satStat.sigStatMap[frq1].slip.any = 0;
				satStat.sigStatMap[frq2].slip.any = 0;
				satStat.sigStatMap[frq3].slip.any = 0;
			}
		}

		/*update averaged MW25 noise when no cycle slip */
		if	( satStat.sigStatMap[frq1].slip.any == 0
			&&satStat.sigStatMap[frq2].slip.any == 0
			&&satStat.sigStatMap[frq3].slip.any == 0)
		{
			S_LC& lc25 = getLC(lc_new, frq2, frq3);
			lowPassFilter(satStat.emwSlip, lc25.MW_c, acsConfig.mw_proc_noise);
		}
		else
		{
			satStat.emwSlip = {};
		}
	}
	/* track L1 or L2 again, new rising satellite */
	else if ( dualFreq
			&&( lc_old.L_m[frq1] == 0
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
	
	detslp_ll(trace, obsList);
	detslp_gf(trace, obsList);
	detslp_mw(trace, obsList);
	
	tracepde  (2, trace, "PDE-CS GPST       week      sec  prn   el   lamw     gf12    mw12    siggf  sigmw  lamew     gf25    mw25               LC                   N1   N2   N5\n");

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
	double		elmin)		///< Minimum elevation (rad)
{
// 	int		nj		= 0;		//todo aaron, this function is repeated in ppp?
// 	double 	sig		= 5;
// 	double 	sum		= 0;
// 	double 	sump	= 0;
// 	double 	suml	= 0;
// 	double 	nk		= 0;
// 
// 	tracepdeex(3, trace, "\n   *-------- PDE Clock jump detection & repair --------*\n");
// 	int nsat = obsList.size();
// 	for (auto& obs : obsList)
// 	{
// 		if (obs.exclude)
// 		{
// 			continue;
// 		}
// 
// 		int sys = obs.Sat.sys;
// 		E_FType					frq2 = F2;
// 		if (sys == E_Sys::GAL)	frq2 = F5;
// 		if (sys == E_Sys::GPS && acsConfig.ionoOpts.iflc_freqs == +E_LinearCombo::L1L5_ONLY) frq2 = F5;
// 
// 		auto& satStat = *(obs.satStat_ptr);
// 		lc_t& lc_pre = satStat.lc_pre;
// 		lc_t& lc_new = satStat.lc_new;
// 		int ind = 0;
// 
// 		/*------PDE clock jump-----low elevation */
// 		if (satStat.el < elmin)
// 		{
// 			nsat--;
// 			continue;
// 		}
// 
// 		/*------PDE clock jump-----First epoch */
// 		if  ( lc_pre.time.time == 0
// 			||sys != E_Sys::GPS
// 			||(lc_new.time - lc_pre.time) > PDEGAP)
// 		{
// 			nsat--;
// 			continue;
// 		}
// 		else
// 		{
// 			double 	dL = 0;
// 			int 	nf = 0;
// 
// 			/* only cycle slip free satellites */
// 			if  ( satStat.sigStatMap[F1].slip.any == 0
// 				&&satStat.sigStatMap[frq2].slip.any == 0)
// 			{
// 				/* L1 */
// 				if  ( lc_pre.L_m[F1] != 0
// 					&&lc_new.L_m[F1] != 0)
// 				{
// 					dL += lc_pre.L_m[F1]
// 						- lc_new.L_m[F1];
// 					nf++;										//todo aaron, put in loop?
// 				}
// 				/* L2 */
// 				if  ( lc_pre.L_m[frq2] != 0
// 					&&lc_new.L_m[frq2] != 0)
// 				{
// 					dL += lc_pre.L_m[frq2]
// 						- lc_new.L_m[frq2];			//todo aaron, should this do L5?
// 					nf++;
// 				}
// 			}
// 			else
// 			{
// 				nsat--;
// 				ind = 1;
// 				continue;
// 			}
// 
// 			if (nf == 0 && ind == 0)
// 			{
// 				nsat--;
// 				continue;
// 			}
// 
// 			dL /= nf;				//average difference for all frequencies
// 
// 			double 	dP = 0;
// 					nf = 0;
// 			/* P1 */
// 			if	( lc_pre.P[F1] != 0
// 				&&lc_new.P[F1] != 0)
// 			{
// 				dP += lc_pre.P[F1]
// 					- lc_new.P[F1];
// 				nf++;
// 			}
// 			/* P2 */
// 			if	( lc_pre.P[frq2] != 0
// 				&&lc_new.P[frq2] != 0)
// 			{
// 				dP += lc_pre.P[frq2]
// 					- lc_new.P[frq2];
// 				nf++;
// 			}
// 
// 			dP /= nf;		//average difference for all frequencies
// 
// 			double S = dP - dL;
// 
// 			nk++;
// 			sump += dP;
// 			suml += dL;
// 
// 			double k = 1e-3 * CLIGHT - 3 * sig;
// 			tracepde(3, trace, "PDE-CJ DIFF=%10.2f %10.2f %10.2f \n", fabs(S), dP, dL);
// 			if (fabs(S) > k)
// 			{
// 				nj++;
// 				sum += S;
// 			}
// 		}
// 	}
// 
// 	tracepde(3, trace, "PDE-CJ valid=%d used=%d\n", nj, nsat);
// 
// 	double Js = 0;
// 	if 	( nj != 0
// 		&&nj == nsat)
// 	{
// 		/* Clock jump occurred */
// 		double M = sum / nsat * 1e3 / CLIGHT;
// 		tracepde(3, trace, "PDE-CJ float value, %lf\n", M);
// 		if (fabs(M - ROUND(M) <= 1e-5))
// 		{
// 			Js = ROUND(M);
// 			tracepde(2, trace, "PDE-CJ clock jump = %4d (ms)  type = 0 \n", Js);
// 		}
// 		else
// 			Js = 0;
// 	}
// 	else
// 	{
// 		if (nk != 0)
// 		{
// 			double t1 = ROUND(sump/nk/(CLIGHT/1000));
// 			double t2 = ROUND(suml/nk/(CLIGHT/1000));
// 
// 			if (t1 != 0 && t2 != 0)
// 			{
// 				Js = t1;
// 				tracepde(2, trace, "PDE-CJ clock jump = %4d (ms)  type = 1 \n", Js);
// 			}
// 		}
// 
// 		cj.type = 1;
// 	}
// 
// 	cj.msJump += Js;
// 
// 	return;
}
