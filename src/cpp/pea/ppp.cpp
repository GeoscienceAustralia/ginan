
// #pragma GCC optimize ("O0")

/** \file
* Precise point positioning
*
* ###References:
* 
* 1.  D.D.McCarthy, IERS Technical Note 21, IERS Conventions 1996, July 1996
* 2.  D.D.McCarthy and G.Petit, IERS Technical Note 32, IERS Conventions 2003, November 2003
* 3.  D.A.Vallado, Fundamentals of Astrodynamics and Applications 2nd ed, Space Technology Library, 2004
* 4.  J.Kouba, A Guide to using International GNSS Service (IGS) products, May 2009
* 5.  RTCM Paper, April 12, 2010, Proposed SSR Messages for SV Orbit Clock, Code Biases, URA
* 6.  MacMillan et al., Atmospheric gradients and the VLBI terrestrial and celestial reference frames, Geophys. Res. Let., 1997
* 7.  G.Petit and B.Luzum (eds), IERS Technical Note No. 36, IERS Conventions (2010), 2010
* 8.  J.Kouba, A simplified yaw-attitude model for eclipsing GPS satellites, GPS Solutions, 13:1-12, 2009
* 9.  F.Dilssner, GPS IIF-1 satellite antenna phase center and attitude modeling, InsideGNSS, September, 2010
* 10. F.Dilssner, The GLONASS-M satellite yaw-attitude model, Advances in Space Research, 2010
* 11. IGS MGEX (http://igs.org/mgex)
* 12. U.Hugentobler, S.Schaer, G.Beutler, H.Bock, R.Dach, A.Jäggi, M.Meindl, C.Urschl, L.Mervart, M.Rothacher & U.Wild, CODE IGS analysis center technical report 2002, 2002.
*/


#include <boost/log/trivial.hpp>

#include <vector>

using std::vector;


#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "coordinates.hpp"
#include "linearCombo.hpp"
#include "corrections.hpp"
#include "binaryStore.hpp"
#include "ephPrecise.hpp"
#include "navigation.hpp"
#include "instrument.hpp"
#include "mongoWrite.hpp"
#include "testUtils.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "biasSINEX.hpp"
#include "constants.hpp"
#include "satStat.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "planets.hpp"
#include "antenna.hpp"
#include "common.hpp"
#include "wancorr.h"
#include "tides.hpp"
#include "trace.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "vmf3.h"
#include "trop.h"


#define VAR_IONO    	SQR(60.0)       // init variance iono-delay
#define VAR_IONEX   	SQR(0.0)
#define ERR_BRDCI   	0.5             // broadcast iono model error factor

/** get meterological parameters
*/
void getmet(
	double	lat,
	double*	met)
{
	const double metprm[][10] = /* lat=15,30,45,60,75 */
	{
		{1013.25, 299.65, 26.31, 6.30E-3, 2.77,  0.00, 0.00, 0.00, 0.00E-3, 0.00},
		{1017.25, 294.15, 21.79, 6.05E-3, 3.15, -3.75, 7.00, 8.85, 0.25E-3, 0.33},
		{1015.75, 283.15, 11.66, 5.58E-3, 2.57, -2.25, 11.00, 7.24, 0.32E-3, 0.46},
		{1011.75, 272.15, 6.78, 5.39E-3, 1.81, -1.75, 15.00, 5.36, 0.81E-3, 0.74},
		{1013.00, 263.65, 4.11, 4.53E-3, 1.55, -0.50, 14.50, 3.39, 0.62E-3, 0.30}
	};

	lat = fabs(lat);

	if      (lat <= 15) for (int i = 0; i < 10; i++) met[i] = metprm[0][i];
	else if (lat >= 75) for (int i = 0; i < 10; i++) met[i] = metprm[4][i];
	else
	{
		int j = (int)(lat / 15);
		double a = (lat - j * 15) / 15.0;

		for (int i = 0; i < 10; i++)
		{
			met[i] = (1 - a) * metprm[j - 1][i] + a * metprm[j][i];
		}
	}
}


/* tropospheric delay correction -----------------------------------------------
* compute sbas tropospheric delay correction (mops model)
* args   : gtime_t time     I   time
*          double   *pos    I   receiver position {lat,lon,height} (rad/m)
*          double   *azel   I   satellite azimuth/elavation (rad)
*          double   *var    O   variance of troposphric error (m^2)
* return : slant tropospheric delay (m)
*-----------------------------------------------------------------------------*/
double sbstropcorr(
	GTime			time,			///< Time
	VectorEcef&		rRec,			///< Receiver position (ECEF)
	double			el,				///< Satellite elevation
	double*			var)			///< Optional variance output
{
	VectorPos pos = ecef2pos(rRec);
	const double k1	= 77.604;
	const double k2	= 382000;
	const double rd	= 287.054;
	const double gm	= 9.784;
	const double g	= 9.80665;

// 	trace(4, "sbstropcorr: pos=%.3f %.3f azel=%.3f\n",
// 			pos.latDeg(),
// 			pos.lonDeg(),
// 			el*R2D);

	if	( pos.hgt()	< -100
		||pos.hgt()	> +10000
		||el		<= 0)
	{
		if (var)
			*var = 0;

		return 0;
	}

	double met[10];
	getmet(pos.latDeg(), met);

	UYds yds = time;
	double c = cos(2 * PI * (yds.doy - (pos.lat() >= 0 ? 28 : 211)) / 365.25);
	for (int i = 0; i < 5; i++)
	{
		met[i] -= met[i + 5] * c;
	}
	double zh = 1E-6 * k1 * rd * met[0] / gm;
	double zw = 1E-6 * k2 * rd / (gm * (met[4] + 1) - met[3] * rd) * met[2] / met[1];

	double h = pos.hgt();
	zh *= pow(1 - met[3] * h / met[1], g / (rd * met[3]));
	zw *= pow(1 - met[3] * h / met[1], (met[4] + 1) * g / (rd * met[3]) - 1);

	double sinel = sin(el);
	double m = 1.001 / sqrt(0.002001 + sinel * sinel);
	if (var)
		*var = SQR(0.12 * m);
	return (zh + zw) * m;
}

/** Antenna corrected measurement
*/
void corr_meas(
	Trace&		trace,		///< Trace file to output to
	GObs&		obs,		///< Observation to correct measurements of
	E_FType		ft,			///< Frequency type to correct
	double		dAntRec,	///< Delta for antenna offset of receiver
	double		dAntSat,	///< Delta for antenna offset of satellite
	double		phw,		///< Phase wind up
	bool		oldSchool)	///< Option to apply 'corrections' here, rather than where they should be
{
	Instrument	instrument(__FUNCTION__);

	Sig& sig = obs.Sigs[ft];

	double lam = obs.satNav_ptr->lamMap[ft];
	
	if	(  lam		== 0 
		|| sig.L	== 0 
		|| sig.P	== 0)
	{
		return;
	}
	
	double bvar[2] = {};
	
	bool pass;
	pass			= getBiasSinex(trace, obs.time, obs.Sat.id(), obs.Sat, sig.code, CODE, sig.biases[CODE], bvar[CODE]);
	sig.phaseBias	= getBiasSinex(trace, obs.time, obs.Sat.id(), obs.Sat, sig.code, PHAS, sig.biases[PHAS], bvar[PHAS]);
	
	tracepdeex(3, trace, "\n%s %sL%d biases for %3s      = %14.4f %14.4f", obs.time.to_string().c_str(), obs.Sat.id().c_str(), ft, sig.code._to_string(), sig.biases[CODE], sig.biases[PHAS]);
			
	sig.P_corr_m = sig.P;
	sig.L_corr_m = sig.L * lam;
	
	if (oldSchool)
	{
		sig.P_corr_m += - dAntSat - dAntRec;
		sig.L_corr_m += - dAntSat - dAntRec;
		
		if (pass)				sig.P_corr_m += - sig.biases[CODE];
		if (sig.phaseBias)		sig.L_corr_m += - sig.biases[PHAS] - phw * lam;
		
	}
}

/** Returns gradient mapping function m_az(el)
* Valid for 0 < el < 0.9999 * PI/2; returns 0 otherwise
* Ref: https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/97JB01739
*/
double gradMapFn(
	double	el)		///< Elevation (rad)
{
	if	( el < 0
		||el > 0.9999 * PI/2)
	{
		return 0;
	}

	double c = 0.0031;
	return 1 / (sin(el) * tan(el) + c);
}

/* precise tropospheric model */
double trop_model_prec(
	GTime		time,
	VectorPos&	pos,
	double*		azel,
	double*		tropStates,
	double*		dTropDx,
	double&		var)
{
	double map[2] = {};

	/* zenith hydrostatic delay */
	double zhd = tropacs(pos, azel, map);
	double zwd = tropStates[0] - zhd;
	
	if	( acsConfig.process_user
		||acsConfig.process_ppp)
	{
		/* mapping function */
		double m_w;
		double m_h = tropmapf(time, pos, azel, &m_w);

		double& az = azel[0];
		double& el = azel[1];
		
		double m_az = gradMapFn(el);
		
		double grad_n = m_az * cos(az);
		double grad_e = m_az * sin(az);

		var			= SQR(0.01);		//todo aaron, move this somewhere else, should use trop state variance?
		
		double value	= m_h		* zhd
						+ m_w		* zwd
						+ grad_n	* tropStates[1]
						+ grad_e	* tropStates[2];
						
		dTropDx[0] = m_w;
		dTropDx[1] = grad_n;
		dTropDx[2] = grad_e;
		
		return value;
	}
	else
	{
		/* wet mapping function */
		dTropDx[0]	= map[1];
		var			= SQR(0.01);

		return map[0] * zhd;
	}
}

/** ionospheric model
 */
int ionoModel(
	GTime		time,
	VectorPos&	pos,
	double*		azel,
	double		ionoState,
	double&		dion,
	double&		var)
{
	switch (acsConfig.ionoOpts.corr_mode)
	{
		case E_IonoMode::TOTAL_ELECTRON_CONTENT:
		{
			int pass = iontec(time, &nav, pos, azel, acsConfig.ionoOpts.mapping_function, E_IonoFrame::SUN_FIXED, dion, var);
			if (pass)	var +=	VAR_IONEX;			// adding some extra errors to reflect modelling errors
			else		var =	VAR_IONO;

			return pass;
		}
		case E_IonoMode::BROADCAST:	// only GPS Klobuchar model implemented
		{
			E_Sys			sys		= E_Sys::GPS;
			E_NavMsgType	type	= defNavMsgType[sys];

			auto ion_ptr = seleph<ION>(std::cout, time, sys, type, nav);
			
			double* vals = nullptr;
			if (ion_ptr != nullptr)
				vals = ion_ptr->vals;
			
			dion	= ionmodel(time, vals, pos, azel);
			var		= SQR(dion * ERR_BRDCI);

			return 1;
		}
		case E_IonoMode::ESTIMATE:
		{
			dion	= ionoState;
			var		= 0;

			return 1;
		}
		case E_IonoMode::IONO_FREE_LINEAR_COMBO:
		{
			dion	= 0;
			var		= 0;

			return 1;
		}
		default:
		{
			return 0;
		}
	}
}

void combinator(
	GObs& obs)
{
	auto&	satStat	= *obs.satStat_ptr;
	auto&	lam		= obs.satNav_ptr->lamMap;
	
	E_FType ft1;
	if (obs.Sat.sys == +E_Sys::GLO)			ft1 = G1;
	else									ft1 = F1;
	
	for (E_FType ft : {F2, F5})
	{
		E_FType ft2;
		if (obs.Sat.sys == +E_Sys::GLO && ft == F2)		ft2 = G2;
		else											ft2 = ft;
		
		if (obs.Sat.sys == +E_Sys::GLO && ft == F5)	
			continue;
		 
		/* iono-free LC */
		Sig sig1 = obs.Sigs[ft1];
		Sig sig2 = obs.Sigs[ft2];

		if	( lam[ft1] == 0
			||lam[ft2] == 0)
		{
			continue;
		}
		
		if	( (sig1.L_corr_m == 0)
			||(sig1.P_corr_m == 0)
			||(sig2.L_corr_m == 0)
			||(sig2.P_corr_m == 0))
		{
			continue;
		}

		double c1;
		double c2;
		S_LC lc = getLC(sig1.L_corr_m,	sig2.L_corr_m,
						sig1.P_corr_m,	sig2.P_corr_m,
						lam[ft1],		lam[ft2],
						&c1, 			&c2);

		if (lc.valid == false)
		{
			continue;
		}

		Sig lcSig = {};
		lcSig.L_corr_m = lc.IF_Phas_m;
		lcSig.P_corr_m = lc.IF_Code_m;

		E_FType newType;
		switch (ft)
		{
			case F2: newType = FTYPE_IF12;	break;
			case F5: newType = FTYPE_IF15;	break;
			default: continue;
		}
		
		auto& sigStat_ = satStat.sigStatMap[ft2string(newType)];
		auto& sigStat1 = satStat.sigStatMap[ft2string(ft1)];
		auto& sigStat2 = satStat.sigStatMap[ft2string(ft2)];
		
		//update distance measurement for iflc
		lcSig.Range	= sig1.Range * c1
					- sig2.Range * c2;

		sigStat_.lambda	= sigStat1.lambda * c1
						- sigStat2.lambda * c2; 

		sigStat_.satPcv	= sigStat1.satPcv * c1
						- sigStat2.satPcv * c2; 

		sigStat_.recPcv	= sigStat1.recPcv * c1
						- sigStat2.recPcv * c2; 

		sigStat_.recPco	= sigStat1.recPco * c1
						- sigStat2.recPco * c2; 

		
		lcSig.biases[CODE]	= sig1.biases[CODE] * c1
							- sig2.biases[CODE] * c2;
	
		lcSig.biases[PHAS]	= sig1.biases[PHAS] * c1
							- sig2.biases[PHAS] * c2;

		lcSig.codeVar	= POW4(lam[ft1]) * sig1.codeVar / SQR(SQR(lam[ft1]) - SQR(lam[ft2]))
						+ POW4(lam[ft2]) * sig2.codeVar / SQR(SQR(lam[ft1]) - SQR(lam[ft2]));

		lcSig.phasVar 	= POW4(lam[ft1]) * sig1.phasVar / SQR(SQR(lam[ft1]) - SQR(lam[ft2]))
						+ POW4(lam[ft2]) * sig2.phasVar / SQR(SQR(lam[ft1]) - SQR(lam[ft2]));

		obs.Sigs[newType] = lcSig;

		sigStat_.slip.any	= sigStat1.slip.any | sigStat2.slip.any;
	}
}

void pppCorrections(
	Trace&		trace,
	ObsList&	obsList,
	Vector3d&	rRec,
	Station&	rec)
{
	int lv = 3;

	GTime time = obsList.front()->time;

	string timeString = time.to_string();
	
	auto& pos = rec.pos;
	
	pos = ecef2pos(rRec);

	tracepdeex(3,trace, "\n%-10s: n=%d", __FUNCTION__, obsList.size());

	for (auto& obs : only<GObs>(obsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		SatStat&	satStat	= *obs.satStat_ptr;
		SatNav&		satNav	= *obs.satNav_ptr;

		double r = geodist(obs.rSat, rRec, satStat.e);
		
		if 	( r <= 0
			||satStat.el < acsConfig.elevation_mask)
		{
			obs.excludeElevation = true;
			continue;
		}

		if	( obs.ephPosValid == false
// 			||obs.ephClkValid == false
			)
		{
			obs.excludeSVH = true;
			continue;
		}

		// phase windup model
		if (acsConfig.model.phase_windup)
		{ 
			phaseWindup(obs, rec, satStat.phw);
		}
		
		for (auto& [ft, sig] : obs.Sigs)
		{
			auto& sigStat = satStat.sigStatMap[ft2string(ft)];
		
			/* receiver pco correction to the coordinates */
			double varDummy = 0;
			Vector3d pcoEnu	= antPco(rec.antennaId, obs.Sat.sys, ft, time, varDummy, E_Radio::RECEIVER, acsConfig.interpolate_rec_pco);
			
			Vector3d dr2	= antenna2ecef(rec.attStatus, pcoEnu);

			sigStat.recPco 	= dr2.dot(satStat.e);

			sig.Range = (obs.rSat - rRec).norm();

			sigStat.satPcv = antPcv(obs.Sat.id(),	obs.Sat.sys, ft, time, satNav.attStatus,	satStat.e * -1);
			sigStat.recPcv = antPcv(rec.antennaId,	obs.Sat.sys, ft, time, rec.attStatus,		satStat.e * +1);
			
			// corrected phase and code measurements
			tracepdeex(lv, trace, "\n*---------------------------------------------------*");
			corr_meas(trace, obs, ft, sigStat.recPcv, sigStat.satPcv, satStat.phw, false);

			tracepdeex(lv, trace, "\n%s %sL%d satpcv              = %14.4f",				timeString.c_str(), obs.Sat.id().c_str(), ft, sigStat.satPcv);
			tracepdeex(lv, trace, "\n%s %sL%d recpcv              = %14.4f",				timeString.c_str(), obs.Sat.id().c_str(), ft, sigStat.recPcv);

			tracepdeex(lv, trace, "\n%s %sL%d az, el              = %14.4f %14.4f",			timeString.c_str(), obs.Sat.id().c_str(), ft, satStat.az*R2D, satStat.el*R2D);
			tracepdeex(lv, trace, "\n%s %sL%d phw(cycle)          = %14.4f",				timeString.c_str(), obs.Sat.id().c_str(), ft, satStat.phw);
			tracepdeex(lv, trace, "\n%s %sL%d rSat+pco            = %14.4f %14.4f %14.4f",	timeString.c_str(), obs.Sat.id().c_str(), ft, obs.rSat[0], obs.rSat[1], obs.rSat[2]);
			tracepdeex(lv, trace, "\n%s %sL%d dist                = %14.4f",				timeString.c_str(), obs.Sat.id().c_str(), ft, r);
		}

		if (acsConfig.ionoOpts.corr_mode == +E_IonoMode::IONO_FREE_LINEAR_COMBO)
			combinator(obs);
	}
}

void outputDeltaClocks(
	StationMap& stationMap)
{
	KFState aprioriState;
	
	for (auto& [id,		rec]	: stationMap)
	for (auto& [kfKey,	index]	: rec.pppState.kfIndexMap)
	{
		if (kfKey.type != KF::REC_CLOCK)
		{
			continue;
		}
		
		double c_dtRecEst = 0;
		rec.pppState.getKFValue(kfKey, c_dtRecEst);
		
		double dtRecPrec = 0;
		int ret = pephclk(std::cout, tsync, id, nav, dtRecPrec);
		if (ret == 1)
		{
			double c_dtRecPrec = CLIGHT * dtRecPrec;
			aprioriState.addKFState(kfKey, {.x = c_dtRecEst - c_dtRecPrec});
		}
	}
	
	aprioriState.stateTransition(nullStream, tsync);
	
	mongoStates(aprioriState, "_delta");
}

void outputApriori(
	StationMap& stationMap)
{
	if	(  acsConfig.localMongo	.output_states	== false
		&& acsConfig.remoteMongo.output_states	== false
		&& acsConfig.store_binary_states		== false)
	{
		return;
	}
	
	KFState aprioriState;
	for (auto& [id, rec] : stationMap)
	{
		KFKey kfKey;
		kfKey.str	= id;
		kfKey.type	= KF::REC_POS;
		
		for (int i = 0; i < 3; i++)
		{
			kfKey.num = i;
			double apriori = rec.aprioriPos[i];
			
			if (apriori == 0)
			{
				continue;
			}
			
			aprioriState.addKFState(kfKey, {.x = apriori});
		}
	}
	
	for (auto& [id, rec] : stationMap)
	{
		KFKey kfKey;
		kfKey.str	= id;
		kfKey.Sat	= SatSys(E_Sys::GPS);
		kfKey.type	= KF::REC_CLOCK;
			
		double dtRec = 0;
		int ret = pephclk(std::cout, tsync, id, nav, dtRec);
		if (ret == 1)
		{
			aprioriState.addKFState(kfKey, {.x = CLIGHT * dtRec});
		}
	}
	
	for (auto& [Sat, satNav] : nav.satNavMap)
	{
		if (acsConfig.process_sys[Sat.sys] == false)
		{
			continue;
		}
			
		auto& satOpts = acsConfig.getSatOpts(Sat);
			
		if (satOpts.exclude)
		{
			continue;
		}
		
		KFKey kfKey;
		kfKey.Sat	= Sat;
		kfKey.type	= KF::SAT_CLOCK;
		
		double dtSat = 0;
		int ret = pephclk(std::cout, tsync, kfKey.Sat.id(), nav, dtSat);
		if (ret == 1)
		{
			aprioriState.addKFState(kfKey, {.x = CLIGHT * dtSat});
		}
		
		for (int i = 0; i < 3; i++)
		{
			KFKey kfKey;
			kfKey.Sat	= Sat;
			kfKey.num	= i;
			kfKey.type	= KF::ORBIT;
			
			aprioriState.addKFState(kfKey, {.x = satNav.aprioriPos(i)});
		}
	}
	
	aprioriState.stateTransition(nullStream, tsync);
	
	mongoStates(aprioriState, "_apriori");
	storeStates(aprioriState, "apriori");
}

/** Compare estimated station position with benchmark in SINEX file
 */
void outputPPPSolution(
	string		filename,
	Station&	rec)
{
	VectorEcef&	snxPos		= rec.snx.pos;
	
	auto& recOpts = acsConfig.getRecOpts(rec.id);
	if (recOpts.apriori_pos.isZero() == false)
		snxPos	= recOpts.apriori_pos;
	
	VectorEcef&	estPos		= rec.sol.pppRRec;
	VectorEcef	diffEcef	= snxPos - estPos;
	
	auto& pos = rec.pos;
	
	pos = ecef2pos(snxPos);
	
	VectorEnu diffEnu = ecef2enu(pos, diffEcef);

	std::ofstream fout(filename, std::ios::out | std::ios::app);
	
	if (!fout)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Could not open trace file for PPP solution at \"" << filename << "\"";
		
		return;
	}
	
	if (fout.tellp() == 0)
	{
		tracepdeex(1,fout,"  Date       UTC time  Sta.   A priori X    A priori Y    A priori Z    Estimated X   Estimated Y   Estimated Z    Dif. X  Dif. Y  Dif. Z   Dif. E  Dif. N  Dif. U\n");
	}

	fout << rec.sol.time.to_string(2) << " ";
	fout << rec.id << " ";
	fout << std::fixed << std::setprecision(4);
	fout << snxPos.transpose() << "  ";
	fout << estPos.transpose() << "  ";
	fout << diffEcef.transpose() << "  ";
	fout << diffEnu.transpose() << "  ";
	fout << std::endl;
}

void selectAprioriSource(
	Station&	rec,
	bool&		sppUsed)
{
	sppUsed = false;
	
	auto& recOpts = acsConfig.getRecOpts(rec.id);
	
	if (recOpts.apriori_pos.isZero() == false)
	{
		rec.aprioriPos		= recOpts.apriori_pos;
	}
	else if (rec.snx.pos(2) != 0)
	{
		rec.aprioriPos		= rec.snx.pos;
		rec.primaryApriori	= rec.snx.primary;
		for (int i = 0; i < 3; i++)
		{
			rec.aprioriTime[i] = rec.snx.start[i];
		}
	}
	else
	{
		rec.aprioriTime = rec.sol.time;
		rec.aprioriPos	= rec.sol.sppRRec;
		
		sppUsed			= true;
	}
	
	Vector3d delta	= rec.aprioriPos 
					- rec.sol.sppRRec;
	
	double distance = delta.norm();
	
	if	( distance > 20
		&&rec.sol.sppRRec.norm() > 0)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Apriori for " << rec.id << " is " << distance << "m from SPP estimate";
	}
}

string ft2string(
	E_FType ft)
{
	return "F" + std::to_string(ft);
}

