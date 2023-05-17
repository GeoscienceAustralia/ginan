
// #pragma GCC optimize ("O0")

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "coordinates.hpp"
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
#include "trace.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "vmf3.h"
#include "trop.h"



/** PPP observed-minus-computed 
 */
void pppomc(
	Trace&			trace,			
	ObsList&		obsList,		
	gptgrid_t&		gptg,			 
	Station&		rec,			
	Vmf3&			vmf3)		 
{
	if (obsList.empty())
	{
		std::cout << "No measurements for " << rec.id;
		return;
	}
	
	GTime time = obsList.front()->time;

// 	tracepde(3, trace, "%s   : time=%s nx=%d n=%d\n", __FUNCTION__, str, rtk.nx, obsList.size());

	// satellite positions and clocks
	for (auto& obs : only<GObs>(obsList))
		satPosClk(trace, time, obs, nav, acsConfig.model.sat_pos.ephemeris_sources, acsConfig.model.sat_clock.ephemeris_sources, E_OffsetType::APC);

	// earth tides correction
	Vector3d dTide = Vector3d::Zero();
	if (acsConfig.model.tides.enable)
	{
		tideDisp(trace, time, rec.sol.sppRRec, nav.erp, rec.otlDisplacement, dTide);
	}

	int lv = 2;

	map<int, double>	r2;
	map<int, Vector3d>	rr2;

	Vector3d recAntVector = body2ecef(rec.attStatus, rec.antDelta);
	
	// receiver position with tide and delta enu
	Vector3d rRec	= rec.aprioriPos
					+ dTide
					+ recAntVector;

	auto& pos = rec.pos;
	
	pos = ecef2pos(rRec);


	tracepdeex(lv, trace, "\n   *-------- Observed minus computed --------*");

	for (auto& obs : only<GObs>(obsList))
	{
		if (obs.exclude)
		{
			continue;
		}
		
		string timeStr = obs.time.to_string();
		
		MjDateTT mjd	= obs.time;

		char id[8];
		obs.Sat.getId(id);
		
		E_FType frq1 = F1;
		E_FType frq2 = F2;
		if (obs.Sat.sys == +E_Sys::GAL) 		frq2 = F5;
		if (obs.Sat.sys == +E_Sys::GLO)
		{
												frq1 = G1;
												frq2 = G2;
		}
		
		tracepdeex(lv, trace, "\n*---------------------------------------------------*");
		tracepdeex(lv, trace, "\n%s %s  recpos               = %14.4f %14.4f %14.4f", timeStr.c_str(), id, rec.aprioriPos[0], rec.aprioriPos[1], rec.aprioriPos[2]);

		auto& satStat	= *(obs.satStat_ptr);
		auto& satNav	= *(obs.satNav_ptr);


		Vector3d e;
		double r = geodist(obs.rSat, rRec, e);
		if	( r <= 0
			||satazel(pos, e, satStat.azel) < acsConfig.elevation_mask)
		{
			obs.excludeElevation = true;
			continue;
		}

		satStat.e = e;

		if	( acsConfig.raim
			&&epoch > 1)
		{
			if (acsConfig.excludeSlip.LLI	&& satStat.sigStatMap[ft2string(frq1)].slip.LLI)	{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.GF	&& satStat.sigStatMap[ft2string(frq1)].slip.GF)		{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.MW	&& satStat.sigStatMap[ft2string(frq1)].slip.MW)		{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.SCDIA	&& satStat.sigStatMap[ft2string(frq1)].slip.SCDIA)	{	obs.excludeSlip = true;		continue;   }
			
			if (acsConfig.excludeSlip.LLI	&& satStat.sigStatMap[ft2string(frq2)].slip.LLI)	{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.GF	&& satStat.sigStatMap[ft2string(frq2)].slip.GF)		{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.MW	&& satStat.sigStatMap[ft2string(frq2)].slip.MW)		{	obs.excludeSlip = true;		continue;   }
			if (acsConfig.excludeSlip.SCDIA	&& satStat.sigStatMap[ft2string(frq2)].slip.SCDIA)	{	obs.excludeSlip = true;		continue;   }
		}

		if	( obs.ephPosValid == false
// 			||obs.ephClkValid == false
			)
		{
			obs.excludeSVH = 1;
			continue;
		}

		double zhd = 0;
		double zwd = 0;
		double mf[2] = {};
		if	( vmf3			.size()
			&&vmf3.orography.size())
		{
			tropvmf3(vmf3, time, pos, satStat.el, zhd, zwd, mf);
		}
		else
		{
			// tropospheric model gpt2+vmf1
			zhd = tropztd(gptg, pos, mjd.to_double(), satStat.el, 0, mf, zwd);
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

		// corrected phase and code measurements 

		// phase windup model
		if (acsConfig.model.phase_windup)
		{
			phaseWindup(obs, rec, satStat.phw);
		}
		
		for (auto& [ft, sig] : obs.Sigs)
		{
			auto& sigStat = satStat.sigStatMap[ft2string(ft)];
			
			sigStat.recPcv = antPcv(rec.antennaId,	obs.Sat.sys, ft, obs.time, rec.attStatus,		satStat.e * +1);
			sigStat.satPcv = antPcv(obs.Sat,		obs.Sat.sys, ft, obs.time, satNav.attStatus,	satStat.e * -1);
			
			corr_meas(trace, obs, ft, sigStat.recPcv, sigStat.satPcv, satStat.phw);
			
			rr2[ft] = rRec;

			Vector3d	pcoEnu	= antPco(rec.antennaId,	obs.Sat.sys, ft, obs.time, E_Radio::RECEIVER);
											//todo check map, continue if null
			
			VectorEcef	dr2		= antenna2ecef(rec.attStatus, pcoEnu);

			if (ft <= F2)
			{
				tracepdeex(lv, trace, "\n%s %s  rec pco%d (enu)       = %14.4f %14.4f %14.4f", timeStr.c_str(), id, ft, pcoEnu[0],	pcoEnu[1],	pcoEnu[2]);
				tracepdeex(lv, trace, "\n%s %s  rec pco%d             = %14.4f %14.4f %14.4f", timeStr.c_str(), id, ft, dr2[0],		dr2[1],		dr2[2]);
			}
			tracepdeex(lv, trace, "\n%s %sL%d satpcv              = %14.4f",				timeStr.c_str(), obs.Sat.id().c_str(), ft, sigStat.satPcv);
			tracepdeex(lv, trace, "\n%s %sL%d recpcv              = %14.4f",				timeStr.c_str(), obs.Sat.id().c_str(), ft, sigStat.recPcv);

			// get rec position and geometric distance for each frequency
			rr2[ft] += dr2;

			r2[ft] = geodist(obs.rSat, rr2[ft], e);
		}

		tracepdeex(lv, trace, "\n%s %s  tide                 = %14.4f %14.4f %14.4f",	timeStr.c_str(), id, dTide(0), dTide(1), dTide(2));
		tracepdeex(lv, trace, "\n%s %s  delta xyz            = %14.4f %14.4f %14.4f",	timeStr.c_str(), id, recAntVector[0], recAntVector[1], recAntVector[2]);
		tracepdeex(lv, trace, "\n%s %s  enu                  = %14.4f %14.4f %14.4f",	timeStr.c_str(), id, rec.antDelta[0], rec.antDelta[1], rec.antDelta[2]);
		tracepdeex(lv, trace, "\n%s %s  recpos+tide+enu+pco1 = %14.4f %14.4f %14.4f",	timeStr.c_str(), id, rr2[F1][0], rr2[F1][1], rr2[F1][2]);
		tracepdeex(lv, trace, "\n%s %s  recpos+tide+enu+pco2 = %14.4f %14.4f %14.4f",	timeStr.c_str(), id, rr2[F2][0], rr2[F2][1], rr2[F2][2]);
		tracepdeex(lv, trace, "\n%s %s  rSat+pco             = %14.4f %14.4f %14.4f",	timeStr.c_str(), id, obs.rSat[0], obs.rSat[1], obs.rSat[2]);
		tracepdeex(lv, trace, "\n%s %s  dist1                = %14.4f",					timeStr.c_str(), id, r2[F1]);
		tracepdeex(lv, trace, "\n%s %s  dist2                = %14.4f",					timeStr.c_str(), id, r2[F2]);

		tracepdeex(lv, trace, "\n%s %s  az, el               = %14.4f %14.4f",			timeStr.c_str(), id, satStat.az*R2D, satStat.el*R2D);
		tracepdeex(lv, trace, "\n%s %s  trop zenith dry (m)  = %14.4f",					timeStr.c_str(), id, zhd);
		tracepdeex(lv, trace, "\n%s %s  trop dry mf          = %14.4f",					timeStr.c_str(), id, mf[0]);
		tracepdeex(lv, trace, "\n%s %s  trop zenith wet (m)  = %14.4f",					timeStr.c_str(), id, zwd);
		tracepdeex(lv, trace, "\n%s %s  trop wet mf          = %14.4f",					timeStr.c_str(), id, mf[1]);
		tracepdeex(lv, trace, "\n%s %s  trop                 = %14.4f",					timeStr.c_str(), id, dtrp);

		tracepdeex(lv, trace, "\n%s %s  phw(cycle)           = %14.4f",					timeStr.c_str(), id, satStat.phw);

		double c1;
		double c2;
		
		for (int o = 0; o < 1; o++) 
		{
			// iono-free LC
			Sig sig1 = obs.Sigs[frq1];
			Sig sig2 = obs.Sigs[frq2];

			auto& lam = obs.satNav_ptr->lamMap;

			if	( lam[frq1] == 0
				||lam[frq2] == 0)
			{
				break;
			}
			
			if	( (sig1.L_corr_m == 0)
				||(sig1.P_corr_m == 0)
				||(sig2.L_corr_m == 0)
				||(sig2.P_corr_m == 0))
			{
				continue;
			}
			S_LC lc = getLC(sig1.L_corr_m,	sig2.L_corr_m,
							sig1.P_corr_m,	sig2.P_corr_m,
							lam[frq1],		lam[frq2],
							&c1, 			&c2);

			if (lc.valid == false)
			{
				continue;
			}

			Sig lcSig = {};
			lcSig.L_corr_m = lc.IF_Phas_m;
			lcSig.P_corr_m = lc.IF_Code_m;

			lcSig.codeVar	= POW4(lam[frq1]) * sig1.codeVar / SQR(SQR(lam[frq1]) - SQR(lam[frq2]))
							+ POW4(lam[frq2]) * sig2.codeVar / SQR(SQR(lam[frq1]) - SQR(lam[frq2]));

			lcSig.phasVar 	= POW4(lam[frq1]) * sig1.phasVar / SQR(SQR(lam[frq1]) - SQR(lam[frq2]))
							+ POW4(lam[frq2]) * sig2.phasVar / SQR(SQR(lam[frq1]) - SQR(lam[frq2]));

			E_FType ifType;
			if (frq2 == F2 || frq2 == G2)	ifType = FTYPE_IF12;
			if (frq2 == F5)					ifType = FTYPE_IF15;

			obs.Sigs[ifType] = lcSig;
			obs.satStat_ptr->sigStatMap[ft2string(ifType)].slip.any	= obs.satStat_ptr->sigStatMap[ft2string(frq1)].slip.any
																	| obs.satStat_ptr->sigStatMap[ft2string(frq2)].slip.any;
		}

		// dual-frequency are required, omit first epoch with single freq
		if	( (obs.Sigs[frq1].L_corr_m	== 0)
			||(obs.Sigs[frq2].L_corr_m	== 0)
			||(obs.Sigs[frq1].P_corr_m	== 0)
			||(obs.Sigs[frq2].P_corr_m	== 0))
		{
			continue;
		}

		// note that relativity effect to estimate sat clock
		double dtrel	= relativity1(obs.rSat, obs.satVel) * CLIGHT;

		// secondary relativity effect (Shapiro effect)
		double ln		= log((obs.rSat.norm() + rr2[frq1].norm() + r2[frq1])
							/ (obs.rSat.norm() + rr2[frq1].norm() - r2[frq1]));
		double dtrel2 	= 2 * MU * ln / CLIGHT / CLIGHT;

		// geometric distance (UD IF or UD UC, to be refined)
		if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
		{
			r	= r2[frq1] * c1
				- r2[frq2] * c2;
		}

		double expected	= r
						+ dtrp
						+ dtrel
						+ dtrel2;

		tracepdeex(lv, trace, "\n%s %s  L(cycle)             = %14.4f %14.4f",			timeStr.c_str(), id, obs.Sigs[frq1].L, obs.Sigs[frq2].L);
		tracepdeex(lv, trace, "\n%s %s  P(m)                 = %14.4f %14.4f",			timeStr.c_str(), id, obs.Sigs[frq1].P, obs.Sigs[frq2].P);
		tracepdeex(lv, trace, "\n%s %s  relativity on clock  = %14.4f",					timeStr.c_str(), id, dtrel);
		tracepdeex(lv, trace, "\n%s %s  relativity (shapiro) = %14.4f",					timeStr.c_str(), id, dtrel2);
		tracepdeex(lv, trace, "\n%s %s  Calclated Geo (m)    = %14.4f",					timeStr.c_str(), id, expected);
		tracepdeex(lv, trace, "\n%s %s  OMC Li(m)            = %14.4f %14.4f %14.4f",	timeStr.c_str(), id, obs.Sigs[frq1].L_corr_m - expected, obs.Sigs[frq2].L_corr_m - expected, obs.Sigs[FTYPE_IF12].L_corr_m - expected);
		tracepdeex(lv, trace, "\n%s %s  OMC Pi(m)            = %14.4f %14.4f %14.4f",	timeStr.c_str(), id, obs.Sigs[frq1].P_corr_m - expected, obs.Sigs[frq2].P_corr_m - expected, obs.Sigs[FTYPE_IF12].P_corr_m - expected);
		tracepdeex(lv, trace, "\n%s %s  LOS                  = %14.4f %14.4f %14.4f",	timeStr.c_str(), id, e[0], e[1], e[2]);

		//do residuals for all frequencies, plain and linear combinations.
		for (auto& [ft, sig] : obs.Sigs)
		{
			if	( (sig.L_corr_m == 0)
				||(sig.P_corr_m == 0))
				continue;

			sig.phasRes	= sig.L_corr_m - expected;
			sig.codeRes	= sig.P_corr_m - expected;
		}
	}
}
