
// #pragma GCC optimize ("O0")

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "streamTrace.hpp"
#include "linearCombo.hpp"
#include "corrections.hpp"
#include "navigation.hpp"
#include "ephemeris.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "satStat.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "antenna.hpp"
#include "common.hpp"
#include "wancorr.h"
#include "tides.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "vmf3.h"
#include "trop.h"



/** PPP observed-minus-computed 
 */
void pppomc(
	Trace&		trace,			
	ObsList&	obsList,		
	gptgrid_t&	gptg,			 
	Station&	rec,			
	vmf3_t*		vmf3,			 
	double*		orography)		 
{
	TestStack ts(__FUNCTION__);

	if (obsList.empty())
	{
		std::cout << "No measurements for " << rec.id;
		return;
	}
	
	GTime	time = obsList.front().time;

	char str[32];
	time2str(time, str, 2);
// 	tracepde(3, trace, "pppos   : time=%s nx=%d n=%d\n", str, rtk.nx, obsList.size());

	/* satellite positions and clocks */
	satposs(trace, time, obsList, nav, acsConfig.model.sat_pos.ephemeris_source, E_OffsetType::APC);

	/* earth tides correction */
	Vector3d dTide = Vector3d::Zero();
	if	( acsConfig.model.tides.enable)
	{
		tidedisp(trace, gpst2utc(time), rec.sol.sppRRec, nav.erp, rec.otlDisplacement, dTide);
	}

	int lv		= 2;

	map<int, double>	r2;
	map<int, Vector3d>	rr2;

	for (auto& obs			: obsList)
	for (auto& [ft, sig]	: obs.Sigs)
	{
		sig.vsig = false;		//todo aaron, move somewhere else
	}

	bool sppInit;
	selectAprioriSource(rec, sppInit); 
	
	// receiver position with tide and delta enu
	Vector3d rRec	= rec.aprioriPos
					+ dTide;

	double pos[3];
	ecef2pos(rRec, pos);

	Vector3d antDelta;
	enu2ecef(pos, rec.antDelta, antDelta);
	rRec += antDelta;

	tracepdeex(lv, trace, "\n   *-------- Observed minus computed           --------*\n");

	int nv = 0;
	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		TestStack ts(obs.Sat.id());

		double ep[6];
		time2epoch(obs.time, ep);
		double jd	= ymdhms2jd(ep);
		double mjd	= jd - JD2MJD;

		char id[8];
		obs.Sat.getId(id);
		
		E_FType frq2=F2;
		if(obs.Sat.sys==+E_Sys::GAL) frq2=F5;
		
		tracepdeex(lv, trace, "*---------------------------------------------------*\n");
		tracepdeex(lv, trace, " %.6f %s  recpos               = %14.4f %14.4f %14.4f\n", mjd, id, rec.aprioriPos[0], rec.aprioriPos[1], rec.aprioriPos[2]);

											TestStack::testMat("recpos", rec.aprioriPos, 1e-3);

		auto& satStat = *(obs.satStat_ptr);


		Vector3d e;
		double r = geodist(obs.rSat, rRec, e);
		if	( r <= 0
			||satazel(pos, e.data(), satStat.azel) < acsConfig.elevation_mask)
		{
			obs.excludeElevation = true;
			continue;
		}

		satStat.e = e;

		if	( acsConfig.raim
			&&epoch > 1)
		{
			if (acsConfig.excludeSlip.LLI	&& satStat.sigStatMap[F1].slip.LLI)		{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.GF	&& satStat.sigStatMap[F1].slip.GF)		{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.MW	&& satStat.sigStatMap[F1].slip.MW)		{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.EMW	&& satStat.sigStatMap[F1].slip.EMW)		{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.CJ	&& satStat.sigStatMap[F1].slip.CJ)		{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.SCDIA	&& satStat.sigStatMap[F1].slip.SCDIA)	{	obs.excludeSlip = true;		continue;   }
			
			if (acsConfig.excludeSlip.LLI	&& satStat.sigStatMap[frq2].slip.LLI)	{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.GF	&& satStat.sigStatMap[frq2].slip.GF)	{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.MW	&& satStat.sigStatMap[frq2].slip.MW)	{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.EMW	&& satStat.sigStatMap[frq2].slip.EMW)	{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.CJ	&& satStat.sigStatMap[frq2].slip.CJ)	{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.SCDIA	&& satStat.sigStatMap[frq2].slip.SCDIA)	{	obs.excludeSlip = true;		continue;   }
		}

		if	(satexclude(obs.Sat, obs.svh)) 		//can move to front end todo aaron?
		{
			obs.exclude = 1;
			continue;
		}

		double zd = PI/2 - satStat.el;
		double zhd = 0;
		double zwd = 0;
		double mf[2] = {};
		if	( vmf3
			&&vmf3->m		!= 2
			&&orography[0]	!= 0)
		{
			tropvmf3(vmf3->vmf3g, orography, jd, pos[0], pos[1], pos[2], zd, vmf3->m, &zhd, &zwd, mf);
		}
		else
		{
			/* tropospheric model gpt2+vmf1 */
			zhd = tropztd(gptg, pos, mjd, satStat.el, 0, mf, zwd);
		}

		double dtrp	= mf[0] * zhd
					+ mf[1] * zwd;
		if (dtrp == 0)
		{
			obs.excludeTrop = true;
			continue;
		}

		double mg = sin(satStat.el) * tan(satStat.el) + 0.0032;
		satStat.mapWet			= mf[1];
		satStat.mapWetGrads[0]	= cos(satStat.az) / mg; /* N grad */
		satStat.mapWetGrads[1]	= sin(satStat.az) / mg; /* E grad */

		/* receiver pco correction to the coordinates */
		for (auto& [ft, sig] : obs.Sigs)
		{
			rr2[ft] = rRec;

			Vector3d pco_r = antPco(rec.antId, obs.Sat.sys, ft, obs.time);
											//check map, continue if null
			Vector3d dr2;
			enu2ecef(pos, pco_r, dr2);  /* convert enu to xyz */

			if (ft <= F2)
			{
				tracepdeex(lv, trace, " %.6f %s  rec pco%d (enu)       = %14.4f %14.4f %14.4f\n", mjd, id, ft, pco_r[0],	pco_r[1],	pco_r[2]);
				tracepdeex(lv, trace, " %.6f %s  rec pco%d             = %14.4f %14.4f %14.4f\n", mjd, id, ft, dr2[0],	dr2[1],		dr2[2]);
			}

			/* get rec position and geometric distance for each frequency */
			rr2[ft] += dr2;

			r2[ft] = geodist(obs.rSat, rr2[ft], e);
		}

		/* phase windup model */
		if (acsConfig.model.phase_windup)
		{
			bool pass = model_phw(rec.sol.time, obs, rRec, satStat.phw);
			if (pass == false)
			{
				continue;
			}
		}

		tracepdeex(lv, trace, " %.6f %s  tide                 = %14.4f %14.4f %14.4f\n",	mjd, id, dTide(0), dTide(1), dTide(2));
		tracepdeex(lv, trace, " %.6f %s  delta xyz            = %14.4f %14.4f %14.4f\n",	mjd, id, antDelta[0], antDelta[1], antDelta[2]);
		tracepdeex(lv, trace, " %.6f %s  enu                  = %14.4f %14.4f %14.4f\n",	mjd, id, rec.antDelta[0], rec.antDelta[1], rec.antDelta[2]);
		tracepdeex(lv, trace, " %.6f %s  recpos+tide+enu+pco1 = %14.4f %14.4f %14.4f\n",	mjd, id, rr2[F1][0], rr2[F1][1], rr2[F1][2]);
		tracepdeex(lv, trace, " %.6f %s  recpos+tide+enu+pco2 = %14.4f %14.4f %14.4f\n",	mjd, id, rr2[F2][0], rr2[F2][1], rr2[F2][2]);
		tracepdeex(lv, trace, " %.6f %s  satpos+pco           = %14.4f %14.4f %14.4f\n",	mjd, id, obs.rSat[0], obs.rSat[1], obs.rSat[2]);
		tracepdeex(lv, trace, " %.6f %s  dist1                = %14.4f\n",					mjd, id, r2[F1]);
		tracepdeex(lv, trace, " %.6f %s  dist2                = %14.4f\n",					mjd, id, r2[F2]);

// 		tracepdeex(lv, trace, " %.6f %s  satpcv               = %14.4f %14.4f\n",			mjd, id, dAntSat[F1], dAntSat[F2]);
// 		tracepdeex(lv, trace, " %.6f %s  recpcv               = %14.4f %14.4f\n",			mjd, id, dAntRec[F1], dAntRec[F2]);
		tracepdeex(lv, trace, " %.6f %s  az, el, nadir        = %14.4f %14.4f %14.4f\n",	mjd, id, satStat.az*R2D, satStat.el*R2D, satStat.nadir*R2D);
		tracepdeex(lv, trace, " %.6f %s  trop zenith dry (m)  = %14.4f\n",					mjd, id, zhd);
		tracepdeex(lv, trace, " %.6f %s  trop dry mf          = %14.4f\n",					mjd, id, mf[0]);
		tracepdeex(lv, trace, " %.6f %s  trop zenith wet (m)  = %14.4f\n",					mjd, id, zwd);
		tracepdeex(lv, trace, " %.6f %s  trop wet mf          = %14.4f\n",					mjd, id, mf[1]);
		tracepdeex(lv, trace, " %.6f %s  trop                 = %14.4f\n",					mjd, id, dtrp);

		tracepdeex(lv, trace, " %.6f %s  phw(cycle)           = %14.4f \n",					mjd, id, satStat.phw);

		/* corrected phase and code measurements */

		satStat.nadir = satNadir(obs.rSat, rRec);
		
		for (auto& [ft, sig] : obs.Sigs)
		{
			double recPcv = antPcv(rec.antId,		obs.Sat.sys, ft, obs.time, PI/2 - satStat.el, satStat.az);
			double satPcv = antPcv(obs.Sat.id(),	obs.Sat.sys, ft, obs.time, satStat.nadir);
			corr_meas(trace, obs, ft, recPcv, satPcv, satStat.phw, rec, mjd);
		}

		double c1;
		double c2;
		E_FType firstFT = FTYPE_NONE;

		for (int o = 0; o < 1; o++)   //todo aaron, separate as before
		{
			/* iono-free LC */
			E_FType ft;
			if ((obs.Sat.sys == +E_Sys::GAL)|| (obs.Sat.sys == +E_Sys::SBS))	ft = F5;
			else																ft = F2;

			Sig sig1 = obs.Sigs[F1];
			Sig sig2 = obs.Sigs[ft];

			auto& lam = obs.satNav_ptr->lamMap;

			if	( lam[F1] == 0
				||lam[ft] == 0)
				break;

			if	( (sig1.L_corr_m == 0)
				||(sig1.P_corr_m == 0)
				||(sig2.L_corr_m == 0)
				||(sig2.P_corr_m == 0))
			{
				continue;
			}
			S_LC lc = getLC(sig1.L_corr_m,	sig2.L_corr_m,			//todo aaron, move this out
							sig1.P_corr_m,	sig2.P_corr_m,
							lam[F1],		lam[ft],
							&c1, 			&c2);

			if (lc.valid == false)
			{
				continue;
			}

			if (firstFT == FTYPE_NONE)
			{
				firstFT = ft;
			}
			
			Sig lcSig = {};
			lcSig.L_corr_m = lc.IF_Phas_m;
			lcSig.P_corr_m = lc.IF_Code_m;

			lcSig.codeVar	= POW4(lam[F1]) * sig1.codeVar / POW2(POW2(lam[F1]) - POW2(lam[ft]))
							+ POW4(lam[ft]) * sig2.codeVar / POW2(POW2(lam[F1]) - POW2(lam[ft]));

			lcSig.phasVar 	= POW4(lam[F1]) * sig1.phasVar / POW2(POW2(lam[F1]) - POW2(lam[ft]))
							+ POW4(lam[ft]) * sig2.phasVar / POW2(POW2(lam[F1]) - POW2(lam[ft]));

			E_FType ifType;
			if (ft == F2)	ifType = FTYPE_IF12;
			if (ft == F5)	ifType = FTYPE_IF15;

			obs.Sigs[ifType] = lcSig;
			obs.satStat_ptr->sigStatMap[ifType].slip.any	= obs.satStat_ptr->sigStatMap[F1].slip.any
															| obs.satStat_ptr->sigStatMap[ft].slip.any;
		}

		/* dual-frequency are required, omit first epoch with single freq */
		E_FType ft = (E_FType) firstFT;
		if	( (obs.Sigs[F1].L_corr_m	== 0)
			||(obs.Sigs[ft].L_corr_m	== 0)
			||(obs.Sigs[F1].P_corr_m	== 0)
			||(obs.Sigs[ft].P_corr_m	== 0))
		{
			continue;
		}

		/* note that relativity effect to estimate sat clock */
		double dtrel	= relativity1(obs.rSat, obs.satVel) * CLIGHT;

		/* secondary relativity effect (Shapiro effect) */
		double ln		= log((obs.rSat.norm() + rr2[F1].norm() + r2[F1])
							/ (obs.rSat.norm() + rr2[F1].norm() - r2[F1]));
		double dtrel2 	= 2 * MU * ln / CLIGHT / CLIGHT;

		/* geometric distance (UD IF or UD UC, to be refine) */
		if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
		{
			r	= r2[F1] * c1
				- r2[ft] * c2;
		}

		double expected	= r
						+ dtrp
						+ dtrel
						+ dtrel2;

		tracepdeex(lv, trace, " %.6f %s  L(cycle)             = %14.4f %14.4f \n",			mjd, id, obs.Sigs[F1].L, obs.Sigs[ft].L);
		tracepdeex(lv, trace, " %.6f %s  P(m)                 = %14.4f %14.4f \n",			mjd, id, obs.Sigs[F1].P, obs.Sigs[ft].P);
		tracepdeex(lv, trace, " %.6f %s  relativity on clock  = %14.4f\n",					mjd, id, dtrel);
		tracepdeex(lv, trace, " %.6f %s  relativity (shapiro) = %14.4f\n",					mjd, id, dtrel2);
		tracepdeex(lv, trace, " %.6f %s  Calclated Geo (m)    = %14.4f\n",					mjd, id, expected);
		tracepdeex(lv, trace, " %.6f %s  OMC Li(m)            = %14.4f %14.4f %14.4f\n",	mjd, id, obs.Sigs[F1].L_corr_m - expected, obs.Sigs[ft].L_corr_m - expected, obs.Sigs[FTYPE_IF12].L_corr_m - expected);
		tracepdeex(lv, trace, " %.6f %s  OMC Pi(m)            = %14.4f %14.4f %14.4f\n",	mjd, id, obs.Sigs[F1].P_corr_m - expected, obs.Sigs[ft].P_corr_m - expected, obs.Sigs[FTYPE_IF12].P_corr_m - expected);
		tracepdeex(lv, trace, " %.6f %s  LOS                  = %14.4f %14.4f %14.4f\n",	mjd, id, e[0], e[1], e[2]);

		//do residuals for all frequencies, plain and linear combinations.
		for (auto& [ft, sig] : obs.Sigs)
		{
			TestStack ts("F" + std::to_string(ft));

			if	( (sig.L_corr_m == 0)
				||(sig.P_corr_m == 0))
				continue;

			sig.phasRes	= sig.L_corr_m - expected;
			sig.codeRes	= sig.P_corr_m - expected;

			sig.vsig	= true;
			nv++;
		}
	}

	if (nv == 0)
	{
		tracepdeex(2, trace, "%s ppp no valid obs data\n", str);
	}
}
