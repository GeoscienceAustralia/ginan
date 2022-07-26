
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
*/


#include <boost/log/trivial.hpp>

#include <vector>

using std::vector;


#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "streamTrace.hpp"
#include "linearCombo.hpp"
#include "corrections.hpp"
#include "navigation.hpp"
#include "instrument.hpp"
#include "mongoWrite.hpp"
#include "testUtils.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "biasSINEX.hpp"
#include "constants.hpp"
#include "preceph.hpp"
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


#define VAR_IONO    	SQR(60.0)       // init variance iono-delay
#define VAR_IONEX   	SQR(0.0)
#define ERR_BRDCI   	0.5             // broadcast iono model error factor


/** exclude meas of eclipsing satellite (block IIA)
*/
void testeclipse(
	ObsList&	obsList)
{
	/* unit vector of sun direction (ecef) */
	Vector3d rsun;
	ERPValues erpv;
	sunmoonpos(gpst2utc(obsList.front().time), erpv, &rsun);
	Vector3d esun = rsun.normalized();

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		double r = obs.rSat.norm();
		if (r <= 0)
			continue;

		/* only block IIA */
// 		if (obs.satNav_ptr->pcv.type == "BLOCK IIA")		//todo take from the satSys object
// 			continue;

		/* sun-earth-satellite angle */
		double cosa = obs.rSat.dot(esun) / r;
		
		if (cosa < -1)		cosa = -1;
		if (cosa > +1)		cosa = +1;
		
		double ang = acos(cosa);

		/* test eclipse */
		if	( ang < PI / 2
			|| r * sin(ang) > RE_WGS84)
			continue;

// 		trace(3, "eclipsing sat excluded %s sat=%s\n", obs.time.to_string(0).c_str(), obs.Sat.id().c_str());

		obs.excludeEclipse = true;
	}
}

/** Calculate nominal yaw-angle
*/
double yaw_nominal(
	double	beta,
	double	mu)
{
	if	( fabs(beta)	< 1E-12
		&&fabs(mu)		< 1E-12)
		return PI;

	return atan2(-tan(beta), sin(mu)) + PI;
}

/** Satellite attitude model
*/
double sat_yaw(
	GTime		time,		///< Time of calculated yaw
	Obs&		obs,		///< Satellite observation
	Vector3d&	exs,		///< Output unit vector XS
	Vector3d&	eys)		///< Output unit vector YS
{
	Vector3d&	rSat = obs.rSat;
	
	Vector3d	rSun;
	ERPValues	erpv;
	sunmoonpos(gpst2utc(time), erpv, &rSun);

	Vector3d vSatPrime = obs.satVel;
	vSatPrime[0] -= OMGE * rSat[1];
	vSatPrime[1] += OMGE * rSat[0];

	Vector3d n = rSat.cross(vSatPrime);						//orbit-axis
	Vector3d p = rSun.cross(n);								//ascension?

	Vector3d eSat	= rSat.	normalized();
	Vector3d eSun	= rSun.	normalized();
	Vector3d en		= n.	normalized();					//orbit-axis
	Vector3d ep		= p.	normalized();					//ascension?

	/* beta and orbit angle */
	double beta	= PI / 2 - acos(eSun.dot(en));				//angle between sun and orbital plane
	
	double E	= acos(eSat.dot(ep));						//angle between sat and ascension node?
	
	double mu;
	if (eSat.dot(eSun) <= 0)	mu = PI/2 - E;				//sat on dark side
	else						mu = PI/2 + E;				//sat on noon side
// 	double mu	= PI / 2 + (eSat.dot(eSun) <= 0 ? -E : E);
	if      (mu < -PI / 2)		mu += 2 * PI;
	else if (mu >= PI / 2)		mu -= 2 * PI;

	/* yaw-angle of satellite */
	double yaw = yaw_nominal(beta, mu);

	/* satellite fixed x,y-vector */
	Vector3d ex = en.cross(eSat);

	double cosy = cos(yaw);
	double siny = sin(yaw);

	eys = -cosy * en - siny * ex;
	exs = -siny * en + cosy * ex;
	
// 	obs.satStat_ptr->beta	= beta;
// 	obs.satStat_ptr->yaw	= yaw;
// 	obs.satStat_ptr->mu		= mu;
	
	return yaw;
}

/** phase windup model
*/
int model_phw(
	GTime		time,	///< Time
	Obs&		obs,	///< Observation detailing the satellite to apply model to
	Vector3d&	rRec,	///< Position of receiver (ECEF)
	double&		phw)	///< Output of phase windup result
{
	/* satellite yaw attitude model */
	Vector3d eXsat;
	Vector3d eYsat;
	Vector3d eZsat;
	double yaw = sat_yaw(time, obs, eXsat, eYsat);
	
	/* non block IIR satellites, to be refined for other constellations */
	/* if ((!strstr(type,"BLOCK IIR"))&&(sys!=SYS_GAL)) { */

	/* all the satellite orientation follow block II-A, 21/03/2019*/
	if (1)
	{
		eXsat *= -1;
		eYsat *= -1;
	}
	
	eZsat = eXsat.cross(eYsat);

	/* unit vector satellite to receiver */
	Vector3d k = rRec - obs.rSat;
	Vector3d eK = k.normalized();

	/* unit vectors of receiver antenna */
	double E[9];
	double pos[3];
	ecef2pos(rRec.data(), pos);
	xyz2enu(pos, E);
	Vector3d eXrec;
	eXrec(0) =  E[1];
	eXrec(1) =  E[4];
	eXrec(2) =  E[7]; /* x = north */

	Vector3d eYrec;
	eYrec(0) = -E[0];
	eYrec(1) = -E[3];
	eYrec(2) = -E[6]; /* y = west  */

	Vector3d eZrec;
	eZrec(0) = -E[2];
	eZrec(1) = -E[5];
	eZrec(2) = -E[8]; /* z = up  */
	
	if (1)
	{
		//Get axis of rotation between antenna boresight and look vector
		Vector3d recAxis	= eZrec		.cross(eK)		.normalized();
		Vector3d satAxis	= eZsat		.cross(eK)		.normalized();
		
		//We dont need 1,2 for the first axis, because they are common
		
		//Get another unit vector to finish the boresight, axis, coordinate set
		Vector3d recQuad1	= recAxis	.cross(eZrec)	.normalized();
		Vector3d satQuad1	= satAxis	.cross(eZrec)	.normalized();
		
		//Get another unit vector to finish the look vector, axis, coordinate set
		Vector3d recQuad2	= recAxis	.cross(eK)		.normalized();
		Vector3d satQuad2	= satAxis	.cross(eK)		.normalized();
		
		//Apply a zero-twist rotation to the antenna to align it with the look vector.
		//Get projection of x axis on coordinate set1, then apply to coordinate set 2
		Vector3d recXnew	= eXrec.dot(recAxis) * recAxis		+ eXrec.dot(recQuad1) * recQuad2;
		Vector3d recYnew	= eYrec.dot(recAxis) * recAxis		+ eYrec.dot(recQuad1) * recQuad2;
							
		Vector3d satXnew	= eXsat.dot(satAxis) * satAxis		+ eXsat.dot(satQuad1) * satQuad2;
		Vector3d satYnew	= eYsat.dot(satAxis) * satAxis		+ eYsat.dot(satQuad1) * satQuad2;
		
		//Get angle offset by looking at receiver's new x component's alignment with satellite's new components
		double angleOffset	= atan2(satXnew.dot(recXnew), -satXnew.dot(recYnew));
		
		//Convert to a fraction of cycles (apply an offset to match old code)
		double phaseFraction = angleOffset / (2*PI) - 0.25;

		//keep phase windup continuous from previous result across cycles
		phw = phaseFraction + floor(phw - phaseFraction + 0.5);		
	}
	else
	{
		/* phase windup effect */
		Vector3d eKsat = eK.cross(eYsat);		//approximate X vector (perpendicular) for sat
		Vector3d eKrec = eK.cross(eYrec);		//approximate X vector (perpendicular) for rec
		
		Vector3d ds = eXsat - eKsat - eK * eK.dot(eXsat);
		Vector3d dr = eXrec + eKrec - eK * eK.dot(eXrec);
		Vector3d es = ds.normalized();
		Vector3d er = dr.normalized();
		
		double cosp = es.dot(er);				//projection of 
		if (isfinite(cosp) == false)
		{
			return 0;
		}
		if      (cosp < -1)		cosp = -1;
		else if (cosp > +1)		cosp = +1;
		
		double ph = acos(cosp) / (2*PI);		//windup in fractional cycles
		
		Vector3d drs = ds.cross(dr);

		if (eK.dot(drs) < 0)
		{
			ph *= -1;
// 			printf("\n-ve on %s", obs.Sat.id().c_str());
		}
		else
		{
// 			printf("\n+ve on %s", obs.Sat.id().c_str());
		}

		phw = ph + floor(phw - ph + 0.5);		//keep phase windup continuous from previous result across cycles
	}
	
	return 1;
}

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
	Vector3d&		rRec,			///< Receiver position (ECEF)
	double			el,				///< Satellite elevation
	double*			var)			///< Optional variance output
{
	double pos[3];
	ecef2pos(rRec.data(), pos);
	const double k1	= 77.604;
	const double k2	= 382000;
	const double rd	= 287.054;
	const double gm	= 9.784;
	const double g	= 9.80665;

// 	trace(4, "sbstropcorr: pos=%.3f %.3f azel=%.3f\n",
// 			pos[0]*R2D,
// 			pos[1]*R2D,
// 			el*R2D);

	if	( pos[2]	< -100
		||pos[2]	> +10000
		||el		<= 0)
	{
		if (var)
			*var = 0;

		return 0;
	}

	double met[10];
	getmet(pos[0] * R2D, met);

	double c = cos(2 * PI * (time2doy(time) - (pos[0] >= 0 ? 28 : 211)) / 365.25);
	for (int i = 0; i < 5; i++)
	{
		met[i] -= met[i + 5] * c;
	}
	double zh = 1E-6 * k1 * rd * met[0] / gm;
	double zw = 1E-6 * k2 * rd / (gm * (met[4] + 1.0) - met[3] * rd) * met[2] / met[1];

	double h = pos[2];
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
	Obs&		obs,		///< Observation to correct measurements of
	E_FType		ft,			///< Frequency type to correct
	double		dAntRec,	///< Delta for antenna offset of receiver
	double		dAntSat,	///< Delta for antenna offset of satellite
	double		phw,		///< Phase wind up
	Station&	rec,		///< Receiver
	double		mjd,		///< Modified julian date
	bool		oldSchool)	///< Option to apply 'corrections' here, rather than where they should be
{
	TestStack	ts			(__FUNCTION__);
	Instrument	instrument	(__FUNCTION__);

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
	
	tracepdeex(3, trace, " %.6f %sL%d biases for %3s      = %14.4f %14.4f\n", mjd, obs.Sat.id().c_str(), ft, sig.code._to_string(), sig.biases[CODE], sig.biases[PHAS]);
			
	sig.P_corr_m = sig.P;
	sig.L_corr_m = sig.L * lam;
	
	if (oldSchool)
	{
		sig.P_corr_m += - dAntSat - dAntRec - sig.biases[CODE];
		sig.L_corr_m += - dAntSat - dAntRec - sig.biases[PHAS] - phw * lam;
	}
}

double satNadir(
	Vector3d&			rs,
	Vector3d&			rr)
{
	Vector3d ru = rr - rs;			Vector3d eu = ru.normalized();
	Vector3d rz = -rs;				Vector3d ez = rz.normalized();
	
	double cosa = eu.dot(ez);
	if (cosa < -1)	cosa = -1;
	if (cosa > +1)	cosa = +1;

	double nadir = acos(cosa);
	
	return nadir;
}

/* precise tropospheric model ------------------------------------------------*/
double trop_model_prec(
	GTime		time,
	double*		pos,
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
		
		double m_az = 0;
		
		if (el > 0)
		if (el < 0.9999 * PI/2)
		{
			double c = 0.0031;
			m_az = 1 / (sin(el) * tan(el) + c);
		}
		
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
int model_iono(
	GTime		time,
	double*		pos,
	double*		azel,
	double		ionoState,
	double&		dion,
	double&		var)
{
	switch (acsConfig.ionoOpts.corr_mode)
	{
		case E_IonoMode::TOTAL_ELECTRON_CONTENT:
		{
			int pass = iontec(time, &nav, pos, azel, 1, dion, var);
			if (pass)	var +=	VAR_IONEX;			// adding some extra errors to reflect modelling errors
			else		var =	VAR_IONO;

			return pass;
		}
		case E_IonoMode::BROADCAST:	// only GPS Klobuchar model implemented
		{
			E_Sys sys = E_Sys::GPS;
			E_NavMsgType type = defNavMsgType[sys];

			ION* ion_ptr = seleph<ION>(std::cout, time, sys, type, nav);
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
	Obs& obs)
{
	auto&	satStat	= *obs.satStat_ptr;
	auto&	lam		= obs.satNav_ptr->lamMap;
		
	for (E_FType ft : {F2, F5})
	{
		/* iono-free LC */
		Sig sig1 = obs.Sigs[F1];
		Sig sig2 = obs.Sigs[ft];

		if	( lam[F1] == 0
			||lam[ft] == 0)
			continue;

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
						lam[F1],		lam[ft],
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
		
		auto& sigStat_ = satStat.sigStatMap[newType];
		auto& sigStat1 = satStat.sigStatMap[F1];
		auto& sigStat2 = satStat.sigStatMap[ft];
		
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

		double AA = POW4(CLIGHT/lam[F1]) / POW2(POW2(CLIGHT/lam[F1]) - POW2(CLIGHT/lam[ft]));
		double BB = POW4(CLIGHT/lam[ft]) / POW2(POW2(CLIGHT/lam[F1]) - POW2(CLIGHT/lam[ft]));

		double A = POW4(lam[F1]) / POW2(POW2(lam[F1]) - POW2(lam[ft]));
		double B = POW4(lam[ft]) / POW2(POW2(lam[F1]) - POW2(lam[ft]));
// 			printf("\n%f %f\n", A, B);
// 			printf("\n%f %f\n", AA, BB);
		lcSig.codeVar	= POW4(lam[F1]) * sig1.codeVar / POW2(POW2(lam[F1]) - POW2(lam[ft]))
						+ POW4(lam[ft]) * sig2.codeVar / POW2(POW2(lam[F1]) - POW2(lam[ft]));

		lcSig.phasVar 	= POW4(lam[F1]) * sig1.phasVar / POW2(POW2(lam[F1]) - POW2(lam[ft]))
						+ POW4(lam[ft]) * sig2.phasVar / POW2(POW2(lam[F1]) - POW2(lam[ft]));

		obs.Sigs[newType] = lcSig;

		obs.satStat_ptr->sigStatMap[newType].slip.any	= obs.satStat_ptr->sigStatMap[F1].slip.any
														| obs.satStat_ptr->sigStatMap[ft].slip.any;
	}
}

void pppCorrections(
	Trace&		trace,
	ObsList&	obsList,
	Vector3d&	rRec,
	Station&	rec)
{
	TestStack ts(__FUNCTION__);

	int lv = 3;

	GTime time = obsList.front().time;
	double ep[6];
	time2epoch(time, ep);
	double jd	= ymdhms2jd(ep);
	double mjd	= jd - JD2MJD;

	
	double pos[3];
	ecef2pos(rRec.data(), pos);

	tracepdeex(3,trace, "pppCorrections  : n=%d\n", obsList.size());

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		TestStack ts(obs.Sat);
		SatStat&	satStat	= *(obs.satStat_ptr);

		double r = geodist(obs.rSat, rRec, satStat.e);
		
		//TestStack::testMat("r",		r);
		if 	( r <= 0
			||satStat.el < acsConfig.elevation_mask)
		{
			obs.excludeElevation = true;
			continue;
		}

		if	(satexclude(obs.Sat, obs.svh))
		{
			obs.exclude = true;
			continue;
		}

		// phase windup model
		if (acsConfig.model.phase_windup)
		{
			bool pass = model_phw(rec.sol.time, obs, rRec, satStat.phw);
			if (pass == false)
			{
				continue;
			}
		}
		
		for (auto& [ft, sig] : obs.Sigs)
		{
			TestStack ts("F" + std::to_string(ft));
			
			auto& sigStat = satStat.sigStatMap[ft];
		
			/* receiver pco correction to the coordinates */
			Vector3d dr2;

			Vector3d pco_r = antPco(rec.antId, ft, time, acsConfig.interpolate_rec_pco);
			
			enu2ecef(pos, pco_r, dr2);    /* convert enu to xyz */

			sigStat.recPco = dr2.dot(satStat.e);

			sig.Range = (obs.rSat - rRec).norm();

			/* calculate pcv */
			satStat.nadir = satNadir(obs.rSat, rRec);	//todo aaron move up
			sigStat.satPcv = antPcv(obs.Sat.id(),	ft, time, satStat.nadir);
			sigStat.recPcv = antPcv(rec.antId,		ft, time, PI/2 - satStat.el, satStat.az);
			
												TestStack::testMat("obs.rSat",	obs.rSat);
// 												TestStack::testMat("rRecFreq",	rRecFreq);
// 												TestStack::testMat("rRec",	    rRec);
// 												TestStack::testMat("dr2",	     dr2);
// 												TestStack::testMat("sig.Range",	sig.Range);
			// corrected phase and code measurements
			tracepdeex(lv, trace, "*---------------------------------------------------*\n");
			corr_meas(trace, obs, ft, sigStat.recPcv, sigStat.satPcv, satStat.phw, rec, mjd, false);

			tracepdeex(lv, trace, " %.6f %sL%d satpcv              = %14.4f\n",                       mjd, obs.Sat.id().c_str(), ft, sigStat.satPcv);
			tracepdeex(lv, trace, " %.6f %sL%d recpcv              = %14.4f\n",                       mjd, obs.Sat.id().c_str(), ft, sigStat.recPcv);

			tracepdeex(lv, trace, " %.6f %sL%d az, el              = %14.4f %14.4f\n",                mjd, obs.Sat.id().c_str(), ft, satStat.az*R2D, satStat.el*R2D);
			tracepdeex(lv, trace, " %.6f %sL%d phw(cycle)          = %14.4f \n",                      mjd, obs.Sat.id().c_str(), ft, satStat.phw);
			tracepdeex(lv, trace, " %.6f %sL%d satpos+pco          = %14.4f %14.4f %14.4f\n",         mjd, obs.Sat.id().c_str(), ft, obs.rSat[0], obs.rSat[1], obs.rSat[2]);
			tracepdeex(lv, trace, " %.6f %sL%d dist                = %14.4f\n",                       mjd, obs.Sat.id().c_str(), ft, r);


// 								TestStack::testMat("dAntRec",	sigStat.recPcv);
								TestStack::testMat("dAntSat",	sigStat.satPcv);
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
		int ret = pephclk(tsync, id, nav, dtRecPrec);
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
	KFState aprioriState;
	for (auto& [id, rec] : stationMap)
	{
		KFKey kfKey;
		kfKey.str	= id;
		kfKey.type	= KF::REC_POS;
		
		for (int i = 0; i < 3; i++)
		{
			kfKey.num = i;
			
			aprioriState.addKFState(kfKey, {.x = rec.aprioriPos[i]});
		}
	}
	
	for (auto& [id, rec] : stationMap)
	{
		KFKey kfKey;
		kfKey.str	= id;
		kfKey.type	= KF::REC_CLOCK;
			
		double dtRec = 0;
		int ret = pephclk(tsync, id, nav, dtRec);
		if (ret == 1)
		{
			aprioriState.addKFState(kfKey, {.x = CLIGHT * dtRec});
		}
	}
	
	for (auto& [satId, satNav] : nav.satNavMap)
	{
		KFKey kfKey;
		kfKey.Sat.fromHash(satId);
		kfKey.type	= KF::SAT_CLOCK;
			
		if (acsConfig.process_sys[kfKey.Sat.sys] == false)
		{
			continue;
		}
		
		double dtSat = 0;
		int ret = pephclk(tsync, kfKey.Sat.id(), nav, dtSat);
		if (ret == 1)
		{
			aprioriState.addKFState(kfKey, {.x = CLIGHT * dtSat});
		}
	}
	
	aprioriState.stateTransition(nullStream, tsync);
	
	mongoStates(aprioriState, "_0");
}

/** Compare estimated station position with benchmark in SINEX file
 */
void outputPPPSolution(
	string		filename,
	Station&	rec)
{
	Vector3d snxPos		= rec.snx.pos;
	Vector3d estPos		= rec.sol.pppRRec;
	Vector3d diffEcef	= snxPos - estPos;
	
	double latLonHt[3];
	ecef2pos(snxPos, latLonHt); // rad,rad,m
	
	Vector3d diffEnu;
	ecef2enu(latLonHt, diffEcef.data(), diffEnu.data());

	std::ofstream fout(filename, std::ios::out | std::ios::app);
	
	if (!fout)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Could not open trace file for PPP solution at " << filename;
		
		return;
	}
	
	if (fout.tellp() == 0)
	{
		tracepdeex(1,fout,"  Date       UTC time  Sta.   A priory X    A priory Y    A priory Z    Estimated X   Estimated Y   Estimated Z    Dif. X  Dif. Y  Dif. Z   Dif. E  Dif. N  Dif. U\n");
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
	
	if (rec.aprioriPos(2) != 0)
	{
		//already has apriori
		return;
	}
	
	if (rec.snx.pos(2) != 0)
	{
		rec.aprioriPos		= rec.snx.pos;
		rec.primaryApriori	= rec.snx.primary;
		for (int i = 0; i < 3; i++)
		{
			rec.aprioriTime[i] = rec.snx.start[i];
		}
		
		Vector3d delta = rec.snx.pos - rec.sol.sppRRec;
		
		double distance = delta.norm();
		
		if	( distance > 20
			&&rec.sol.sppRRec.norm() > 0)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: SINEX apriori for " << rec.id << " is " << distance << "m from SPP estimate";
		}
		
		return;
	}
	else
	{
		double ep[6];
		time2epoch(rec.sol.time, ep);
		epoch2yds(ep, rec.aprioriTime);
		rec.aprioriPos		= rec.sol.sppRRec;
		
		sppUsed				= true;
	}
}

/** Deweight worst measurement
 */
bool deweightMeas(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index)
{
	trace << std::endl << "Deweighting " << kfMeas.obsKeys[index] << std::endl;

	kfMeas.R.row(index) *= acsConfig.deweight_factor;
	kfMeas.R.col(index) *= acsConfig.deweight_factor;
	
	return true;
}

/** Deweight measurement and its relatives
 */
bool deweightStationMeas(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index)
{
	string id = kfMeas.obsKeys[index].str;
	
	for (int i = 0; i < kfMeas.obsKeys.size(); i++)
	{
		auto& obsKey = kfMeas.obsKeys[i];
		
		if (obsKey.str != id)
		{
			continue;
		}
		
		trace << std::endl << "Deweighting " << kfMeas.obsKeys[i] << std::endl;

		kfMeas.R.row(i) *= acsConfig.deweight_factor;
		kfMeas.R.col(i) *= acsConfig.deweight_factor;
		
		map<string, void*>& metaDataMap = kfMeas.metaDataMaps[i];

		bool* used_ptr = (bool*) metaDataMap["used_ptr"];
		
		if (used_ptr)
		{
			*used_ptr = false;	
		}
	}
	return true;
}

/** Count worst measurement
 */
bool incrementPhaseSignalError(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	unsigned int* PhaseRejectCount_ptr = (unsigned int*) metaDataMap["PhaseRejectCount_ptr"];

	if (PhaseRejectCount_ptr == nullptr)
	{
		return true;
	}

	unsigned int&	phaseRejectCount	= *PhaseRejectCount_ptr;

	//increment counter, and clear the pointer so it cant be reset to zero in subsequent operations (because this is a failure)
	phaseRejectCount++;
	metaDataMap["PhaseRejectCount_ptr"] = nullptr;
	
	trace << std::endl << "Incrementing phaseRejectCount on " << kfMeas.obsKeys[index].Sat.id() << " to " << phaseRejectCount;
	
	return true;
}

bool countSignalErrors(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	Obs* obs_ptr = (Obs*) metaDataMap["obs_ptr"];

	if (obs_ptr == nullptr)
	{
		return true;
	}

	ObsKey&		obsKey	= kfMeas.obsKeys[index];
	Obs&		obs		= *obs_ptr;

	if (obsKey.type == "L")
	{
		//this is a phase observation
		obs.Sigs[(E_FType)obsKey.num].phaseError = true;
	}

	return true;
}

bool resetPhaseSignalError(
	KFMeas&		kfMeas,
	int			index)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	//these will have been set to null if there was an error after adding the measurement to the list
	for (auto suffix : {"", "_alt"})
	{
		unsigned int* PhaseRejectCount_ptr = (unsigned int*) metaDataMap[(string)"PhaseRejectCount_ptr" + suffix];

		if (PhaseRejectCount_ptr == nullptr)
		{
			return true;
		}

		unsigned int&	phaseRejectCount	= *PhaseRejectCount_ptr;

		phaseRejectCount = 0;
	}
	
	return true;
}

bool resetPhaseSignalOutage(
	KFMeas&		kfMeas,
	int			index)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	for (auto suffix : {"", "_alt"})
	{
		unsigned int* PhaseOutageCount_ptr = (unsigned int*) metaDataMap[(string)"PhaseOutageCount_ptr" + suffix];

		if (PhaseOutageCount_ptr == nullptr)
		{
			return true;
		}

		unsigned int&	phaseOutageCount	= *PhaseOutageCount_ptr;

		phaseOutageCount = 0;
	}
	
	return true;
}



/** Deweight measurements attached to worst state
 */
bool deweightByState(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	KFKey&		kfKey)
{
	if (acsConfig.deweight_on_state_error == false)
	{
		return true;
	}
	
	trace << std::endl << "Bad state detected " << kfKey << " - deweighting all referencing measurements" << std::endl;

	int stateIndex = kfState.getKFIndex(kfKey);
	
	for (int meas = 0; meas < kfMeas.H.rows(); meas++)
	{
		if (kfMeas.H(meas, stateIndex))
		{
			trace << "- Deweighting " << kfMeas.obsKeys[meas] << std::endl;
			
			kfMeas.R.row(meas) *= acsConfig.deweight_factor;
			kfMeas.R.col(meas) *= acsConfig.deweight_factor;
		}
	}
	
	return true;
}

/** Remove any states connected to a bad clock if it glitches
 */
bool clockGlitchReaction(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	KFKey&		kfKey)
{
	if	(  kfKey.type != KF::SAT_CLOCK
		&& kfKey.type != KF::REC_SYS_BIAS)
	{
		return true;
	}
	
	if (acsConfig.reinit_on_clock_error == false)
	{
		return true;
	}
	
	trace << std::endl << "Bad clock detected " << kfKey << " - resetting linked states" << std::endl;
	
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	(  kfKey.type	== KF::SAT_CLOCK
			&& kfKey.Sat	== key.Sat
			&&( key	.type	== KF::AMBIGUITY
			  ||key	.type	== KF::SAT_CLOCK))
		{
			//remove the satellite clock, and any ambiguities that are connected to it.
			trace << "- Removing " << key << std::endl;
			
			kfState.removeState(key);
		}
		
		if	(  kfKey.type	== KF::REC_SYS_BIAS
			&& kfKey.str	== key.str
			&&( key	.type	== KF::AMBIGUITY
			  ||key	.type	== KF::REC_SYS_BIAS))
		{
			//remove the satellite clock, and any ambiguities that are connected to it.
			trace << "- Removing " << key << std::endl;
			
			kfState.removeState(key);
			
			if (kfKey.rec_ptr)
			{
				//make sure receiver clock corrections get reset too.
				trace << "- Resetting clock adjustment" << std::endl;
				
				auto& rec = *kfKey.rec_ptr;
				
				rec.sol.deltaDt_net_old[0] = 0;
			}
		}
	}
	
	return true;
}
