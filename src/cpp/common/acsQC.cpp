
// #pragma GCC optimize ("O0")

#include <iostream>
#include <vector>

#include "observations.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "testUtils.hpp"
#include "constants.hpp"
#include "satStat.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "acsQC.hpp"
#include "trace.hpp"
#include "lambda.h"
#include "enums.h"

#define		THRES_MW_JUMP		10.0
#define     PDEGAP  			60.0
#define     PDESLIPTHRESHOLD	0.5
#define		PROC_NOISE_IONO		0.001


bool satFreqs(
	E_Sys		sys,
	E_FType&	ft1,
	E_FType&	ft2,
	E_FType&	ft3)
{
	bool ft1Ready = false;
	bool ft2Ready = false;

	//Add defaults in case someone forgets to initialise them...
	ft1 = F1;
	ft2 = F2;
	ft3 = F5;

	if (acsConfig.code_priorities.find(sys) == acsConfig.code_priorities.end())
		return false;

	for (auto& code : acsConfig.code_priorities[sys])
	{
		E_FType ft = code2Freq[sys][code];

		if (ft1Ready == false)
		{
			ft1 = ft;			ft1Ready = true;
			continue;
		}

		if (ft == ft1)
			continue;

		if (ft2Ready == false)
		{
			ft2 = ft;			ft2Ready = true;
			continue;
		}

		if (ft == ft2)
			continue;

		{
			ft3 = ft;
			break;
		}
	}

	return true;
}
/** Detect cycle slip by reported loss of lock
*/
void detslp_ll(
	Trace&		trace,		///< Trace to output to
	ObsList&	obsList)	///< List of observations to detect slips within
{
	tracepdeex(5, trace, "\n%s: n=%d", __FUNCTION__, obsList.size());

// 	auto begin_iter = boost::make_filter_iterator([]

	for (auto& obs			: only<GObs>(obsList))
	for (auto& [ft, sig]	: obs.sigs)
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

		tracepdeex(3, trace, "\n%s: slip detected sat=%s f=F%d\n", __FUNCTION__, obs.Sat.id().c_str(), ft);

		obs.satStat_ptr->sigStatMap[ft2string(ft)].slip.		LLI = true;
		obs.satStat_ptr->sigStatMap[ft2string(ft)].savedSlip.	LLI = true;
	}
}

/** Detect cycle slip by geometry free phase jump
*/
void detslp_gf(
	Trace&		trace,		///< Trace to output to
	ObsList&	obsList)	///< List of observations to detect slips within
{
	tracepdeex(5, trace, "\n%s: n=%d", __FUNCTION__, obsList.size());

	for (auto& obs : only<GObs>(obsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		E_FType	frq1;
		E_FType	frq2;
		E_FType frq3;
		bool pass = satFreqs(obs.Sat.sys, frq1, frq2, frq3);
		if (pass == false)
			continue;

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

		tracepdeex(5, trace, "\n%s: sat=%s gf0=%f gf1=%f", __FUNCTION__, obs.Sat.id().c_str(), gf0, gf1);

		if (fabs(gf1 - gf0) > acsConfig.preprocOpts.slip_threshold)
		{
			tracepdeex(3, trace, "\n%s: slip detected: sat=%s gf0=%f gf1=%f", __FUNCTION__, obs.Sat.id().c_str(), gf0, gf1);

			obs.satStat_ptr->sigStatMap[ft2string(frq1)].slip.		GF = true;
			obs.satStat_ptr->sigStatMap[ft2string(frq2)].slip.		GF = true;
			obs.satStat_ptr->sigStatMap[ft2string(frq1)].savedSlip.	GF = true;
			obs.satStat_ptr->sigStatMap[ft2string(frq2)].savedSlip.	GF = true;
		}
	}
}

/** Detect slip by Melbourne-Wubbena linear combination jump
*/
void detslp_mw(
	Trace&		trace,		///< Trace to output to
	ObsList&	obsList)	///< List of observations to detect slips within
{
	tracepdeex(5, trace, "\n%s: n=%d", __FUNCTION__, obsList.size());

	for (auto& obs : only<GObs>(obsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		E_FType	frq1;
		E_FType	frq2;
		E_FType frq3;
		bool pass = satFreqs(obs.Sat.sys, frq1, frq2, frq3);
		if (pass == false)
			continue;

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

		tracepdeex(5, trace, "\n%s: sat=%s mw0=%f mw1=%f", __FUNCTION__, obs.Sat.id().c_str(), mw0, mw1);

		if (fabs(mw1 - mw0) > THRES_MW_JUMP)
		{
			tracepdeex(3, trace, "\n%s: slip detected: sat=%s mw0=%f mw1=%f", __FUNCTION__, obs.Sat.id().c_str(), mw0, mw1);

			obs.satStat_ptr->sigStatMap[ft2string(frq1)].slip.		MW = true;
			obs.satStat_ptr->sigStatMap[ft2string(frq2)].slip.		MW = true;
			obs.satStat_ptr->sigStatMap[ft2string(frq1)].savedSlip.	MW = true;
			obs.satStat_ptr->sigStatMap[ft2string(frq2)].savedSlip.	MW = true;
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
	E_Sys				sys,		///< Satellite system
	E_FilterMode		filterMode)	///< LSQ/Kalman filter flag
{
	if (nf == 0)
		return;

	E_FType	frq1;
	E_FType	frq2;
	E_FType frq3;
	bool pass = satFreqs(sys, frq1, frq2, frq3);
	if (pass == false)
	{
		return;
	}

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

	E_FType freqs[3] = {frq1, frq2, frq3};

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
		E_FType	frqX = freqs[f];
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
		return;
	}

					satStat.sigStatMap[ft2string(frq1)].slip.		SCDIA = true;
					satStat.sigStatMap[ft2string(frq2)].slip.		SCDIA = true;
	if (nf == 3)	satStat.sigStatMap[ft2string(frq3)].slip.		SCDIA = true;
					satStat.sigStatMap[ft2string(frq1)].savedSlip.	SCDIA = true;
					satStat.sigStatMap[ft2string(frq2)].savedSlip.	SCDIA = true;
	if (nf == 3)	satStat.sigStatMap[ft2string(frq3)].savedSlip.	SCDIA = true;

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
			for (int i = 0; i < 3; i++)
				satStat.amb[i] = ROUND(F.data()[i]);

			for (auto& [key, sigStat] : satStat.sigStatMap)
			{
				sigStat.slip.SCDIA = true;
			}
		}
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
			for (int i = 0; i < nf; i++)
			{
				satStat.flt.amb[i] = ROUND(F.data()[i]);
			}
		}
		tracepdeex(1, trace, "ACC epoch used=%2d\n", satStat.flt.ne);
		if (pass)
			satStat.flt.ne = 0;
	}
}

/** Cycle slip detection for dual-frequency
*/
void cycleslip2(
	Trace&		trace,		///< Trace to output to
	SatStat&	satStat,	///< Persistant satellite status parameters
	lc_t&		lcBase,		///< Linear combinations
	GObs&		obs)		///< Navigation object for this satellite
{
	GWeek	week	= lcBase.time;
	GTow	tow		= lcBase.time;

	auto& recOpts = acsConfig.getRecOpts(obs.mount);

	double dt	= (lcBase.time - satStat.lc_pre.time).to_double();

	if	( dt < 20
		||dt > PDEGAP)
	{
		// small interval or reset

		satStat.dIono = 0;
		// approximation of ionosphere residual

		satStat.sigmaIono = PROC_NOISE_IONO * SQRT(dt);
	}
	else
	{
		// medium interval ~30s

		if (satStat.dIono == 0)
		{
			satStat.sigmaIono = PROC_NOISE_IONO * SQRT(dt);
		}
	}

	if (satStat.sigmaIono == 0)
	{
		satStat.sigmaIono = 0.001;
	}

	auto sys = lcBase.Sat.sys;

	E_FType	frq1;
	E_FType	frq2;
	E_FType frq3;
	bool pass = satFreqs(obs.Sat.sys, frq1, frq2, frq3);
	if (pass == false)
	{
		return;
	}

	auto&	lam = obs.satNav_ptr->lamMap;

	double lam1 = lam[frq1];
	double lam2 = lam[frq2];

	double lamw = lam1 * lam2 / (lam2 - lam1);	//todo aaron, rename

	/* ionosphere coefficient */
	double coef = SQR(lam2) / SQR(lam1) - 1;

	/* elevation dependent noise */
	double sigmaCode	= sqrt(obs.sigs.begin()->second.codeVar);
	double sigmaPhase	= sqrt(obs.sigs.begin()->second.phasVar);

	double sigmaGF = 2 * sigmaPhase;

	S_LC lcNew = getLC(lcBase, 			frq1, frq2);
	S_LC lcPre = getLC(satStat.lc_pre,	frq1, frq2);

	double mwNoise = mwnoise(sigmaCode, sigmaPhase, lam1, lam2);

	/* averaged MW measurement and noise */
	double fNw;
	if (acsConfig.preprocOpts.mw_proc_noise)	{	fNw = lcNew.MW_c - satStat.mwSlip.mean;	}
	else										{	fNw = lcNew.MW_c - lcPre.MW_c;			}	/* Eq (6) in TN */

	/* clock jump */
	if (fabs(fNw * lamw) > 10e-3 * CLIGHT)
	{
		tracepdeex(1, trace,	"Potential clock jump rather than cycle slip -cs2\n");
	}

	double deltaGF	= lcNew.GF_Phas_m
					- lcPre.GF_Phas_m; 	/* Eq (9) in TN */

	tracepdeex(2, trace, "\nPDE-CS GPST DUAL  %4d %8.1f %4s %5.2f %5.3f %8.4f %7.4f %8.4f                                ",
			week, tow, lcBase.Sat.id().c_str(), satStat.el * R2D, lamw, deltaGF, fNw, sigmaGF);

	/* cycle slip detection */
	if (satStat.el >= recOpts.elevation_mask_deg * D2R)
	{
		scdia(trace, satStat, lcBase, lam, sigmaPhase, sigmaCode, 2, sys, E_FilterMode::LSQ);
	}

	/* update TD ionosphere residual */
	if	( satStat.sigStatMap[ft2string(frq1)].slip.any == 0
		&&satStat.sigStatMap[ft2string(frq2)].slip.any == 0)
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
	GObs&		obs)			///< Navigation object for this satellite
{
	GWeek	week	= lc.time;
	GTow	tow		= lc.time;

	auto& recOpts = acsConfig.getRecOpts(obs.mount);

	double dt	= (lc.time - satStat.lc_pre.time).to_double();

	/* small interval */
	if (dt < 20)
	{
		satStat.dIono = 0;

		/* approximation of ionosphere residual */
		satStat.sigmaIono = PROC_NOISE_IONO * SQRT(dt);
	}
	else
	{
		/* large interval */
		if (satStat.sigmaIono == 0)
		{
			satStat.sigmaIono = PROC_NOISE_IONO * SQRT(dt);
		}
	}

	if (satStat.sigmaIono == 0)
	{
		satStat.sigmaIono = 0.001;
	}

	auto sys = lc.Sat.sys;

	E_FType	frq1;
	E_FType	frq2;
	E_FType frq3;
	bool pass = satFreqs(obs.Sat.sys, frq1, frq2, frq3);
	if (pass == false)
		return;

	auto&	lam = obs.satNav_ptr->lamMap;
	double lam1 = lam[frq1];
	double lam2 = lam[frq2];
	double lam5 = lam[frq3];

	/* TD MW noise (m) */
	double lamew = lam2 * lam5 / (lam5 - lam2);
	if (lamew < 0)
		lamew *= -1;

	/* elevation dependent noise */
	double sigmaCode	= sqrt(obs.sigs.begin()->second.codeVar);
	double sigmaPhase	= sqrt(obs.sigs.begin()->second.phasVar);

	double mwNoise12 = mwnoise(sigmaCode, sigmaPhase, lam1, lam2);
	double mwNoise15 = mwnoise(sigmaCode, sigmaPhase, lam1, lam5);
	double mwNoise25 = mwnoise(sigmaCode, sigmaPhase, lam2, lam5);


	double sigmaGF = 2 * sigmaPhase; /* TD GF noise */

	S_LC lc25new = getLC(lc, 				frq2, frq3);
	S_LC lc25pre = getLC(satStat.lc_pre,	frq2, frq3);

	/* averaged EMW measurement and noise */
	double fNew;
// 	double sigmaEMW;
	if (acsConfig.preprocOpts.mw_proc_noise)	{	fNew = lc25new.MW_c - satStat.emwSlip.mean;		}
	else										{	fNew = lc25new.MW_c - lc25pre.MW_c; 			}	/* Eq (13) in TN */

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

	S_LC	lcNew = getLC(lc, 				frq1, frq2);
	S_LC	lcPre = getLC(satStat.lc_pre,	frq1, frq2);

	double	lamw = lam1 * lam2 / (lam2 - lam1);

	double	coef = SQR(lam2) / SQR(lam1) - 1;	// ionosphere coefficient for L1 & LX

	/* averaged MW measurement and noise */
	double	fNw;
	if (acsConfig.preprocOpts.mw_proc_noise)		{	fNw = lcNew.MW_c - satStat.mwSlip.mean;	}
	else											{	fNw = lcNew.MW_c - lcPre.MW_c;			} /* Eq (6) in TN */

	double	deltaGF	= lcNew.GF_Phas_m
					- lcPre.GF_Phas_m;

	tracepdeex(2, trace, "\nPDE-CS GPST TRIP  %4d %8.1f %4s %5.2f %5.3f %8.4f %7.4f %8.4f        %6.2f %8.4f %7.4f ", week,
			tow, lc.Sat.id().c_str(), satStat.el * R2D, lamw, deltaGF, fNw, sigmaGF, lamew, deltaGF25, fNew);

	if (satStat.el >= recOpts.elevation_mask_deg * D2R)
	{
		scdia(trace, satStat, lc, lam, sigmaPhase, sigmaCode, 3, sys, E_FilterMode::LSQ);
	}

	/* update TD ionosphere residual */
	if	( satStat.sigStatMap[ft2string(frq1)].slip.any == 0
		&&satStat.sigStatMap[ft2string(frq2)].slip.any == 0
		&&satStat.sigStatMap[ft2string(frq3)].slip.any == 0)
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
			GObs&		obs)		///< Navigation object for this satellite
{
	bool dualFreq = false;
	E_Sys sys = lc_new.Sat.sys;

	char id[32];
	lc_new.Sat.getId(id);

	GWeek	week	= lc_new.time;
	GTow	tow		= lc_new.time;

	auto& recOpts = acsConfig.getRecOpts(obs.mount);

	if (acsConfig.process_sys[sys] == false)
		return;

	E_FType	frq1;
	E_FType	frq2;
	E_FType frq3;
	bool pass = satFreqs(obs.Sat.sys, frq1, frq2, frq3);
	if (pass == false)
	{
		return;
	}

	/* first epoch or large gap or low elevation */			//todo aaron initialisation stuff, remove
	if  (  satStat.lc_pre.time.bigTime == 0
		|| satStat.el	< recOpts.elevation_mask_deg * D2R
		|| lc_new.time	> lc_old.time + PDEGAP)
	{
		satStat.mwSlip	= {};
		satStat.emwSlip	= {};

		if (lc_new.time	> lc_old.time + PDEGAP)					tracepdeex(1, trace, "\nPDE-CS GPST       %4d %8.1f %4s %5.2f --time gap --", 					week, tow, id, satStat.el * R2D);
		if (satStat.el	< recOpts.elevation_mask_deg * D2R)		tracepdeex(1, trace, "\nPDE-CS GPST       %4d %8.1f %4s %5.2f --low_elevation --", 				week, tow, id, satStat.el * R2D);
		else													tracepdeex(1, trace, "\nPDE-CS GPST       %4d %8.1f %4s %5.2f --satStat.lc_pre.time.time --",	week, tow, id, satStat.el * R2D);

		return;
	}

	if  ( lc_new.L_m[frq1] != 0
		&&lc_new.L_m[frq2] != 0
		&&lc_new.L_m[frq3] == 0)
	{
		dualFreq = true;
	}

	if  ( dualFreq
		&&lc_old.L_m[frq1] != 0
		&&lc_old.L_m[frq2] != 0)
	{
		cycleslip2(trace, satStat, lc_new, obs);

		/* update averaged MW noise when no cycle slip */
		if	( satStat.sigStatMap[ft2string(frq1)].slip.any == 0
			&&satStat.sigStatMap[ft2string(frq2)].slip.any == 0)
		{
			S_LC& lc12 = getLC(lc_new, frq1, frq2);
			lowPassFilter(satStat.mwSlip, lc12.MW_c, acsConfig.preprocOpts.mw_proc_noise);
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
		satStat.sigStatMap[ft2string(frq3)].slip.LLI = true;
		cycleslip2(trace, satStat, lc_new, obs);

		/* update averaged MW noise when no cycle slip */
		if	( satStat.sigStatMap[ft2string(frq1)].slip.any == 0
			&&satStat.sigStatMap[ft2string(frq2)].slip.any == 0)
		{
			S_LC& lc12 = getLC(lc_new, frq1, frq2);
			lowPassFilter(satStat.mwSlip, lc12.MW_c, acsConfig.preprocOpts.mw_proc_noise);
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

		if (satStat.el * R2D > 30)
		{
			if	( satStat.sigStatMap[ft2string(frq1)].slip.any	== 2	//todo aaron, check the 2
				&&satStat.amb[0]				== 0
				&&satStat.amb[1]				== 0
				&&satStat.amb[2]				== 0)
			{
				satStat.sigStatMap[ft2string(frq1)].slip.any = 0;
				satStat.sigStatMap[ft2string(frq2)].slip.any = 0;
				satStat.sigStatMap[ft2string(frq3)].slip.any = 0;
			}
		}

		/*update averaged MW25 noise when no cycle slip */
		if	( satStat.sigStatMap[ft2string(frq1)].slip.any == 0
			&&satStat.sigStatMap[ft2string(frq2)].slip.any == 0
			&&satStat.sigStatMap[ft2string(frq3)].slip.any == 0)
		{
			S_LC& lc25 = getLC(lc_new, frq2, frq3);
			lowPassFilter(satStat.emwSlip, lc25.MW_c, acsConfig.preprocOpts.mw_proc_noise);
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

		tracepdeex(1, trace, "\nPDE-CS GPST       %4d %8.1f %4s %5.2f --  re-tracking   --\n", week, tow, id, satStat.el * R2D);
	}
	else
	{
		satStat.flt.slip	= 0;
		satStat.flt.ne		= 0;
		for (auto& [key, sigStat] : satStat.sigStatMap)
		{
			sigStat.slip.LLI = true;
		}

		tracepdeex(1, trace, "\nPDE-CS GPST       %4d %8.1f %4s %5.2f --single frequency--\n", week, tow, id, satStat.el * R2D);
	}
}

void clearSlips(
	ObsList&	obsList)
{
	//clear non-persistent status values.
	for (auto& obs					: only<GObs>(obsList))
	{
		if (acsConfig.process_sys[obs.Sat.sys] == false)
		{
			continue;
		}

		auto& satOpts = acsConfig.getSatOpts(obs.Sat);

		if (satOpts.exclude)
		{
			continue;
		}

		for (auto& [sigKey, sigStat]	: obs.satStat_ptr->sigStatMap)
		{
			SatStat& satStat = *(obs.satStat_ptr);

			satStat.slip		= false;		//todo aaron, is this used?
			sigStat.slip.any	= 0;
		}
	}
}

/** Detect slips for multiple observations
*/
void detectslips(
	Trace&		trace,		///< Trace to output to
	ObsList&	obsList)	///< List of observations to detect slips within
{
	Instrument instrument(__FUNCTION__);

	tracepdeex(2, trace, "\n   *-------- PDE cycle slip detection & repair --------*\n");

	detslp_ll(trace, obsList);
	detslp_gf(trace, obsList);
	detslp_mw(trace, obsList);

	tracepdeex(2, trace, "\nPDE-CS GPST       week      sec  prn   el   lamw     gf12    mw12    siggf  sigmw  lamew     gf25    mw25               LC                   N1   N2   N5\n");

	for (auto& obs : only<GObs>(obsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		SatStat& satStat = *(obs.satStat_ptr);

		detectslip(trace, satStat, satStat.lc_new, satStat.lc_pre, obs);

		for (auto& [ft, sig] : obs.sigs)
		{
			auto& sigStat = obs.satStat_ptr->sigStatMap[ft2string(ft)];

			if (sigStat.slip.any)
			{
				satStat.slip = true;
			}
		}
	}
}
