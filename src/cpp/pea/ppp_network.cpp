
#include "observations.hpp"
#include "streamTrace.hpp"
#include "linearCombo.hpp"
#include "corrections.hpp"
#include "navigation.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "constants.h"
#include "satStat.hpp"
#include "preceph.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "antenna.hpp"
#include "constants.h"
#include "common.hpp"
#include "wancorr.h"
#include "tides.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "vmf3.h"
#include "trop.h"

#include "eigenIncluder.hpp"

#define MU          3.986004415E14  	// gravitational constant Handbook GNSS



/* phase and code residuals --------------------------------------------------*/
int resomc(
	Trace&		trace,
	ObsList&	obsList,
	Vector3d&	dTide,
	rtk_t&		rtk,
	gptgrid_t&	gptg,
	ClockJump&	cj,
	Station&	rec,
	vmf3_t*		vmf3,
	double*		orography)
{
	TestStack ts(__FUNCTION__);

	prcopt_t& opt = rtk.opt;

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

	Vector3d dr1;
	enu2ecef(pos, opt.antdel.data(), dr1.data());
	rRec += dr1;

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
		
		tracepde(lv, trace, "*---------------------------------------------------*\n");
		tracepde(lv, trace, " %.6f %s  recpos               = %14.4f %14.4f %14.4f\n", mjd, id, rec.aprioriPos[0], rec.aprioriPos[1], rec.aprioriPos[2]);

											TestStack::testMat("recpos", rec.aprioriPos, 1e-3);

		SatStat&	satStat	= *(obs.satStat_ptr);


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
			&&epoch > 1					//todo aaron, moved this here, because combined satstats?
			&&( satStat.sigStatMap[F1].slip.any	!= 0
			||satStat.sigStatMap[frq2].slip.any	!= 0))
		{
			/* remove satellites with cycle slips */
			obs.excludeSlip = true;			//todo aaron probably not doing somethin it should because it just excluded it rather than completing the msision.
			continue;						//todo aaron, issue here?
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
		if	( vmf3 			!= NULL
			&&vmf3->m		!= 2
			&&orography[0]	!= 0)
		{
			tropvmf3(vmf3->vmf3g, orography, jd, pos[0], pos[1], pos[2], zd, vmf3->m, &zhd, &zwd, mf);
		}
		else
		{
			/* tropospheric model gpt2+vmf1 */
			zhd = tropztd(gptg, pos, mjd, satStat.el, 0, mf, &zwd);
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
		map<int, double> dAntRec;
		for (auto& [ft, sig] : obs.Sigs)
		{
			rr2[ft] = rRec;

			if (rtk.pcvrec)
			{
				Vector3d pco_r;
				recpco(rtk.pcvrec, ft, pco_r);
												//check map, continue if null
				Vector3d dr2;
				enu2ecef(pos, pco_r.data(), dr2.data());  /* convert enu to xyz */

				if (ft <= F2)
				{
					tracepde(lv, trace, " %.6f %s  rec pco%d (enu)       = %14.4f %14.4f %14.4f\n", mjd, id, ft, pco_r[0],    pco_r[1],    pco_r[2]);
					tracepde(lv, trace, " %.6f %s  rec pco%d             = %14.4f %14.4f %14.4f\n", mjd, id, ft, dr2[0],    dr2[1],        dr2[2]);
					if (ft == F1)
					{
						TestStack::testMat("rec pco1 (enu)",     pco_r,    1e-3);
						TestStack::testMat("rec pco1",            dr2,    1e-3);
					}
					else
					{
						TestStack::testMat("rec pco2 (enu)",     pco_r,    1e-3);
						TestStack::testMat("rec pco2",            dr2,    1e-3);
					}
				}

				/* get rec position and geometric distance for each frequency */
				rr2[ft] += dr2;

				/* calculate pcv */
				double azDeg = satStat.az * R2D;
				double elDeg = satStat.el * R2D;
				recpcv(rtk.pcvrec, ft, elDeg, azDeg, dAntRec[ft]);
			}

			r2[ft] = geodist(obs.rSat, rr2[ft], e);
		}

// 		if (acsConfig.antexacs == 0)
// 		{
// // 			std::cout << " Using antmodel() " << std::endl;			//todo aaron, broke this.
// // 			antmodel(opt->pcvr, opt->antdel, obs.azel, opt->posopt[1], dAntRec);
// 		}

		/* satellite and receiver antenna model */
		double nadir = 0;
		map<int, double> dAntSat;
		if	(acsConfig.sat_pcv)
		{
			pcvacs_t* pcvsat = findAntenna(id, ep, nav);
			if (pcvsat)
			{
				satantpcv(obs.rSat, rRec, *pcvsat, dAntSat, &nadir);
			}
			else
			{
				tracepde(1, trace,	"Warning: no satellite (%s) pcv information\n",	id);
				continue;
			}
		}

		/* phase windup model */
		if (acsConfig.phase_windup)
		{
			bool pass = model_phw(rtk.sol.time, obs, rRec, satStat.phw);
			if (pass == false)
			{
				continue;
			}
		}

		if (acsConfig.antexacs == 0)
		{
			tracepde(lv, trace, " %.6f %s  tide                 = %14.4f %14.4f %14.4f\n",  mjd, id, dTide(0), dTide(1), dTide(2));
			tracepde(lv, trace, " %.6f %s  recpos+tide          = %14.4f %14.4f %14.4f\n",  mjd, id, rRec[0], rRec[1], rRec[2]);
			tracepde(lv, trace, " %.6f %s  satpos+pco           = %14.4f %14.4f %14.4f\n",	mjd, id, obs.rSat[0], obs.rSat[1], obs.rSat[2]);
			tracepde(lv, trace, " %.6f %s  dist                 = %14.4f\n",				mjd, id, r);
											TestStack::testMat("tide", 		dTide,		1e-3);
											TestStack::testMat("recpos+tide",	rRec,		1e-3);
											TestStack::testMat("satpos+pco",	obs.rSat,	1e-3);
											TestStack::testMat("dist", 		r,			1e-3);
		}
		else
		{
			tracepde(lv, trace, " %.6f %s  tide                 = %14.4f %14.4f %14.4f\n", 	mjd, id, dTide(0), dTide(1), dTide(2));
			tracepde(lv, trace, " %.6f %s  delta xyz            = %14.4f %14.4f %14.4f\n", 	mjd, id, dr1[0], dr1[1], dr1[2]);
			tracepde(lv, trace, " %.6f %s  enu                  = %14.4f %14.4f %14.4f\n", 	mjd, id, opt.antdel[0], opt.antdel[1], opt.antdel[2]);
			tracepde(lv, trace, " %.6f %s  recpos+tide+enu+pco1 = %14.4f %14.4f %14.4f\n", 	mjd, id, rr2[F1][0], rr2[F1][1], rr2[F1][2]);
			tracepde(lv, trace, " %.6f %s  recpos+tide+enu+pco2 = %14.4f %14.4f %14.4f\n", 	mjd, id, rr2[F2][0], rr2[F2][1], rr2[F2][2]);
			tracepde(lv, trace, " %.6f %s  satpos+pco           = %14.4f %14.4f %14.4f\n", 	mjd, id, obs.rSat[0], obs.rSat[1], obs.rSat[2]);
			tracepde(lv, trace, " %.6f %s  dist1                = %14.4f\n",	         	mjd, id, r2[F1]);
			tracepde(lv, trace, " %.6f %s  dist2                = %14.4f\n",	         	mjd, id, r2[F2]);
											TestStack::testMat("tide",					dTide,			1e-3);
											TestStack::testMat("delta xyz",			dr1,			1e-3);
											TestStack::testMat("enu",					opt.antdel,		1e-3);
											TestStack::testMat("recpos+tide+enu+pco1", rr2[F1], 		1e-3);
											TestStack::testMat("recpos+tide+enu+pco2", rr2[F2], 		1e-3);
											TestStack::testMat("satpos+pco",			obs.rSat,		1e-3);
											TestStack::testMat("dist1,",				r2[F1], 		1e-3);
											TestStack::testMat("dist2",				r2[F2], 		1e-3);
		}

		tracepde(lv, trace, " %.6f %s  satpcv               = %14.4f %14.4f\n",	        mjd, id, dAntSat[F1], dAntSat[F2]);
		tracepde(lv, trace, " %.6f %s  recpcv               = %14.4f %14.4f\n",	        mjd, id, dAntRec[F1], dAntRec[F2]);
		tracepde(lv, trace, " %.6f %s  az, el, nadir        = %14.4f %14.4f %14.4f\n",	mjd, id, obs.satStat_ptr->az*R2D, obs.satStat_ptr->el*R2D, nadir);
		tracepde(lv, trace, " %.6f %s  trop zenith dry (m)  = %14.4f\n",	         	mjd, id, zhd);
		tracepde(lv, trace, " %.6f %s  trop dry mf          = %14.4f\n",	         	mjd, id, mf[0]);
		tracepde(lv, trace, " %.6f %s  trop zenith wet (m)  = %14.4f\n",	         	mjd, id, zwd);
		tracepde(lv, trace, " %.6f %s  trop wet mf          = %14.4f\n",	         	mjd, id, mf[1]);
		tracepde(lv, trace, " %.6f %s  trop                 = %14.4f\n",	         	mjd, id, dtrp);

		tracepde(lv, trace, " %.6f %s  phw(cycle)           = %14.4f \n",	         	mjd, id, satStat.phw);

											TestStack::testMat("satpcv",				dAntSat[F1],			1e-3);
											TestStack::testMat("recpcv",				dAntRec[F1],			1e-3);
											TestStack::testMat("az",					obs.satStat_ptr->az,	1e-3);
											TestStack::testMat("el",					obs.satStat_ptr->el,	1e-3);
											TestStack::testMat("nadir",					nadir,					1e-3);
											TestStack::testMat("trop zenith dry (m)",	zhd,					1e-3);
											TestStack::testMat("trop dry mf",			mf[0],					1e-3);
											TestStack::testMat("trop zenith wet (m)",	zwd,					1e-3);
											TestStack::testMat("trop wet mf",			mf[1],					1e-3);
											TestStack::testMat("trop", 					dtrp,					1e-3);
											TestStack::testMat("phw(cycle)",			satStat.phw,			1e-3);

		/* corrected phase and code measurements */

		for (auto& [ft, sig] : obs.Sigs)
		{
			corr_meas(trace, obs, ft, obs.satStat_ptr->el, dAntRec[ft], dAntSat[ft], satStat.phw, cj, rec);
		}

		double c1;
		double c2;
		int index;

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

			if (acsConfig.antexacs)
			{
				/* used for the calculation of geometric distance */
				index = ft;
			}

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
		TestStack::testMat("dAntRec",		dAntRec[F1]);
		TestStack::testMat("dAntSat",		dAntSat[F1]);
		TestStack::testMat("satStat.phw",	&satStat.phw);
		TestStack::testMat("c1",			&c1);
		TestStack::testInt("cjump",			cj.msJump);

		/* dual-frequency are required, omit first epoch with single freq */
		E_FType ft = (E_FType) index;
		if	( (obs.Sigs[F1].L_corr_m	== 0)
			||(obs.Sigs[ft].L_corr_m	== 0)
			||(obs.Sigs[F1].P_corr_m	== 0)
			||(obs.Sigs[ft].P_corr_m	== 0))
		{
			obs.excludeMissingSig = true;
			continue;
		}

		/* note that relativity effect to estimate sat clock */
		double dtrel	= 2 * obs.rSat.dot(obs.satVel) / CLIGHT;

		/* secondary relativity effect (Shapiro effect) */
		double ln		= log((obs.rSat.norm() + rr2[F1].norm() + r2[F1])
							/ (obs.rSat.norm() + rr2[F1].norm() - r2[F1]));
		double dtrel2 	= 2 * MU * ln / CLIGHT / CLIGHT;                                         //todo aaron, does standard ppp have these included? are they in the precise clocks already?

		if (acsConfig.antexacs)
		{
			/* geometric distance (UD IF or UD UC, to be refine) */
			if (acsConfig.ionoOpts.corr_mode == E_IonoMode::IONO_FREE_LINEAR_COMBO)
			{
				r	= r2[F1] * c1
					- r2[ft] * c2;
			}
		}

		double expected	= r
						+ dtrp
						+ dtrel
						+ dtrel2;

		tracepde(lv, trace, " %.6f %s  L(cycle)             = %14.4f %14.4f \n",     	mjd, id, obs.Sigs[F1].L, obs.Sigs[ft].L);
		tracepde(lv, trace, " %.6f %s  P(m)                 = %14.4f %14.4f \n",     	mjd, id, obs.Sigs[F1].P, obs.Sigs[ft].P);
		tracepde(lv, trace, " %.6f %s  relativity on clock  = %14.4f\n",		        mjd, id, dtrel);
		tracepde(lv, trace, " %.6f %s  relativity (shapiro) = %14.4f\n",		        mjd, id, dtrel2);
		tracepde(lv, trace, " %.6f %s  Calclated Geo (m)    = %14.4f\n",		        mjd, id, expected);
		tracepde(lv, trace, " %.6f %s  OMC Li(m)            = %14.4f %14.4f %14.4f\n",	mjd, id, obs.Sigs[F1].L_corr_m - expected, obs.Sigs[ft].L_corr_m - expected, obs.Sigs[FTYPE_IF12].L_corr_m - expected);
		tracepde(lv, trace, " %.6f %s  OMC Pi(m)            = %14.4f %14.4f %14.4f\n",	mjd, id, obs.Sigs[F1].P_corr_m - expected, obs.Sigs[ft].P_corr_m - expected, obs.Sigs[FTYPE_IF12].P_corr_m - expected);
		tracepde(lv, trace, " %.6f %s  LOS                  = %14.4f %14.4f %14.4f\n",	mjd, id, e[0], e[1], e[2]);

												TestStack::testMat("L(cycle)(F1)",				obs.Sigs[F1].L,					1e-3);
												TestStack::testMat("L(cycle)(F2)",				obs.Sigs[F2].L,					1e-3);
												TestStack::testMat("P(m)(F1)",					obs.Sigs[F1].P,					1e-3);
												TestStack::testMat("P(m)(F2)",					obs.Sigs[F2].P,					1e-3);
												TestStack::testMat("relativity on clock",		dtrel,							1e-3);
												TestStack::testMat("relativity (shapiro)",		dtrel2,							1e-3);
												TestStack::testMat("Calclated Geo (m)",			expected,						1e-3);
												TestStack::testMat("OMC Li(m)(F1)",				obs.Sigs[F1].L_corr_m, 			1e-3);
												TestStack::testMat("OMC Li(m)(F2)",				obs.Sigs[F2].L_corr_m, 			1e-3);
												TestStack::testMat("OMC Li(m)(FTYPE_LC12)",		obs.Sigs[FTYPE_IF12].L_corr_m, 	1e-3);
												TestStack::testMat("OMC Pi(m)(F1)",				obs.Sigs[F1].P_corr_m,  		1e-3);
												TestStack::testMat("OMC Pi(m)(F2)",				obs.Sigs[F2].P_corr_m,  		1e-3);
												TestStack::testMat("OMC Pi(m)(FTYPE_LC12)",		obs.Sigs[FTYPE_IF12].P_corr_m,  1e-3);
												TestStack::testMat("LOS",						e,								1e-3);

		//do residuals for all frequencies, plain and linear combinations.
		for (auto& [ft, sig] : obs.Sigs)
		{
			TestStack ts("F" + std::to_string(ft));

			if	( (sig.L_corr_m == 0)
				||(sig.P_corr_m == 0))
				continue;

			sig.phasRes	= sig.L_corr_m - expected;
			sig.codeRes	= sig.P_corr_m - expected;

												TestStack::testMat("sig.phasRes",	sig.phasRes);
												TestStack::testMat("sig.codeRes",	sig.codeRes);
												TestStack::testMat("L_corr",		sig.L_corr_m);
												TestStack::testMat("P_corr",		sig.P_corr_m);
												TestStack::testMat("L",				sig.L);
												TestStack::testMat("P",				sig.P);
												TestStack::testStr("Code",			sig.code._to_string());
												TestStack::testMat("expected",		expected);

			sig.vsig	= true;
			nv++;
		}
	}

	return nv;
}

/* observed-minus-computed -------------------------------------------------- */
void pppomc(
	Trace&		trace,			///< [in]		Trace for output
	rtk_t&		rtk,            ///< [in/out]
	ObsList&	obsList,        ///< [in/out]	List of observations
	gptgrid_t&	gptg,           ///< [in/out]
	ClockJump&	cj,             ///< [in/out]
	Station&	rec,        	///< [in/out]
	vmf3_t*		vmf3,           ///< [in/out]
	double*		orography)      ///< [in/out]
{
	TestStack ts(__FUNCTION__);

	const prcopt_t* opt = &rtk.opt;
	char str[32];

	GTime	time = obsList.front().time;
	time2str(time, str, 2);
// 	tracepde(3, trace, "pppos   : time=%s nx=%d n=%d\n", str, rtk.nx, obsList.size());

	/* satellite positions and clocks */
	satposs(trace, time, obsList, nav, acsConfig.ppp_ephemeris);

																					TestStack::testMat("sat0", obsList.front().rSat.data(), 3);
																					TestStack::testMat("dts0", obsList.front().dtSat, 1);

	/* earth tides correction */
	Vector3d dTide = Vector3d::Zero();
	if	( acsConfig.tide_solid
		||acsConfig.tide_otl
		||acsConfig.tide_pole)
	{
		tidedisp(trace, gpst2utc(time), rtk.sol.sppRRec, &nav.erp, opt->odisp[0], dTide);
	}

	/* prefit residuals */
	int nv = resomc(trace, obsList, dTide, rtk, gptg, cj, rec, vmf3, orography);
	if (nv == 0)
	{
		tracepde(2, trace, "%s ppp no valid obs data\n", str);
	}
}
