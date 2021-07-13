/** \file
* ppp.c : precise point positioning
*
* references :
*    [1] D.D.McCarthy, IERS Technical Note 21, IERS Conventions 1996, July 1996
*    [2] D.D.McCarthy and G.Petit, IERS Technical Note 32, IERS Conventions
*        2003, November 2003
*    [3] D.A.Vallado, Fundamentals of Astrodynamics and Applications 2nd ed,
*        Space Technology Library, 2004
*    [4] J.Kouba, A Guide to using International GNSS Service (IGS) products,
*        May 2009
*    [5] RTCM Paper, April 12, 2010, Proposed SSR Messages for SV Orbit Clock,
*        Code Biases, URA
*    [6] MacMillan et al., Atmospheric gradients and the VLBI terrestrial and
*        celestial reference frames, Geophys. Res. Let., 1997
*    [7] G.Petit and B.Luzum (eds), IERS Technical Note No. 36, IERS
*         Conventions (2010), 2010
*    [8] J.Kouba, A simplified yaw-attitude model for eclipsing GPS satellites,
*        GPS Solutions, 13:1-12, 2009
*    [9] F.Dilssner, GPS IIF-1 satellite antenna phase center and attitude
*        modeling, InsideGNSS, September, 2010
*    [10] F.Dilssner, The GLONASS-M satellite yaw-attitude model, Advances in
*        Space Research, 2010
*    [11] IGS MGEX (http://igs.org/mgex)
*/


#include <boost/log/trivial.hpp>

#include <vector>

using std::vector;

#include "observations.hpp"
#include "streamTrace.hpp"
#include "linearCombo.hpp"
#include "corrections.hpp"
#include "navigation.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "biasSINEX.hpp"
#include "constants.h"
#include "satStat.hpp"
#include "preceph.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "constants.h"
#include "antenna.hpp"
#include "common.hpp"
#include "wancorr.h"
#include "tides.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "vmf3.h"
#include "trop.h"

#include "eigenIncluder.hpp"

#define VAR_IONO    	SQR(60.0)       // init variance iono-delay
#define VAR_IONEX   	SQR(0.0)
#define ERR_BRDCI   	0.5             // broadcast iono model error factor


/** exclude meas of eclipsing satellite (block IIA)
*/
void testeclipse(
	ObsList&	obsList)
{
	double erpv[5] = {0};

	/* unit vector of sun direction (ecef) */
	Vector3d rsun;
	sunmoonpos(gpst2utc(obsList.front().time), erpv, rsun.data(), NULL, NULL);
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
		if (cosa < -1)	cosa = -1;
		if (cosa > +1)	cosa = +1;
		double ang = acos(cosa);

		/* test eclipse */
		if (ang < PI / 2
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
int sat_yaw(
	GTime		time,		///< Time of calculated yaw
	Vector3d&	rSat,		///< Satellite position (ECEF)
	Vector3d&	vSat,		///< Satellite velocity (ECEF)
	Vector3d&	exs,		///< Output unit vector XS
	Vector3d&	eys)		///< Output unit vector YS
{
	Vector3d	rSun;
	double		erpv[5] = {};
	sunmoonpos(gpst2utc(time), erpv, rSun.data(), NULL, NULL);

	Vector3d vSatPrime = vSat;

	/* beta and orbit angle */
	vSatPrime[0] -= OMGE * rSat[1];
	vSatPrime[1] += OMGE * rSat[0];

	Vector3d n = rSat.	cross(vSatPrime);
	Vector3d p = rSun.	cross(n);

	Vector3d es		= rSat.	normalized();
	Vector3d eSun	= rSun.	normalized();
	Vector3d en		= n.	normalized();
	Vector3d ep		= p.	normalized();

	double beta	= PI / 2 - acos(eSun.dot(en));
	double E	= acos(es.dot(ep));
	double mu	= PI / 2 + (es.dot(eSun) <= 0 ? -E : E);
	if      (mu < -PI / 2) mu += 2 * PI;
	else if (mu >= PI / 2) mu -= 2 * PI;

	/* yaw-angle of satellite */
	double yaw = yaw_nominal(beta, mu);

	/* satellite fixed x,y-vector */
	Vector3d ex = en.cross(es);

	double cosy = cos(yaw);
	double siny = sin(yaw);

	eys = -cosy * en - siny * ex;
	exs = -siny * en + cosy * ex;

	return 1;
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
	Vector3d exs;
	Vector3d eys;
	if (!sat_yaw(time, obs.rSat, obs.satVel, exs, eys))
		return 0;

	/* non block IIR satellites, to be refined for other constellations */
	/* if ((!strstr(type,"BLOCK IIR"))&&(sys!=SYS_GAL)) { */

	/* all the satellite orientation follow block II-A, 21/03/2019*/
	if (1)
	{
		exs *= -1;
		eys *= -1;
	}

	/* unit vector satellite to receiver */
	Vector3d r = rRec - obs.rSat;
	Vector3d ek = r.normalized();

	/* unit vectors of receiver antenna */
	double E[9];
	double pos[3];
	ecef2pos(rRec.data(), pos);
	xyz2enu(pos, E);
	Vector3d exr;
	Vector3d eyr;
	exr(0) =  E[1];
	exr(1) =  E[4];
	exr(2) =  E[7]; /* x = north */

	eyr(0) = -E[0];
	eyr(1) = -E[3];
	eyr(2) = -E[6]; /* y = west  */

	/* phase windup effect */
	Vector3d eks = ek.cross(eys);
	Vector3d ekr = ek.cross(eyr);

	Vector3d ds = exs - ek * ek.dot(exs) - eks;
	Vector3d dr = exr - ek * ek.dot(exr) + ekr;
	double cosp = ds.dot(dr) / ds.norm() / dr.norm();
	if      (cosp < -1) cosp = -1;
	else if (cosp > +1) cosp = +1;
	double ph = acos(cosp) / 2 / PI;
	Vector3d drs = ds.cross(dr);

	if (ek.dot(drs) < 0)
		ph *= -1;

	phw = ph + floor(phw - ph + 0.5); /* in cycle */

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
	double		el,			///< Satellite elevation
	double		dAntRec,	///< Delta for antenna offset of receiver
	double		dAntSat,	///< Delta for antenna offset of satellite
	double		phw,		///< Phase wind up
	ClockJump&	cj,			///< Clock jump
	Station&	rec)		///< Receiver
{
	TestStack ts(__FUNCTION__);

	Sig& sig = obs.Sigs[ft];

	double lam = obs.satNav_ptr->lamMap[ft];
	
	if	(  lam		== 0 
		|| sig.L	== 0 
		|| sig.P	== 0)
	{
		return;
	}
	
	double bias[2] = {};
	double bvar[2] = {};
	
	bias_io_opt biaopt;
	biaopt.OSB_biases = acsConfig.ambrOpts.readOSB;
	biaopt.DSB_biases = acsConfig.ambrOpts.readDSB;
	biaopt.SSR_biases = acsConfig.ambrOpts.readSSRbias;
	biaopt.SAT_biases = acsConfig.ambrOpts.readSATbias;
	biaopt.REC_biases = acsConfig.ambrOpts.readRecBias;
	biaopt.HYB_biases = acsConfig.ambrOpts.readHYBbias;
	biaopt.COD_biases = true;
	biaopt.PHS_biases = true;	
	
	inpt_hard_bias(trace, obs, sig.code, bias, bvar, biaopt);

	if (acsConfig.ssrOpts.calculate_ssr)
	{
		double dummyVal = 0; // Set dummy code biases L1C & L2W to zero (not currently used)
		double dummyVar = 0; // Set dummy code biases L1C & L2W to zero (not currently used)
		if	( (ft == F1 && sig.code == +E_ObsCode::L1C) 
			||(ft == F2 && sig.code == +E_ObsCode::L2W))
		{
			obs.satNav_ptr->ssrOut.ssrCodeBias.canExport		= false;
			obs.satNav_ptr->ssrOut.ssrCodeBias.bias	[sig.code]	= dummyVal;
			obs.satNav_ptr->ssrOut.ssrCodeBias.var	[sig.code]	= dummyVar;
			obs.satNav_ptr->ssrOut.ssrCodeBias.isSet			= true;
		}
	}

	
#if 0 /* this should not be done. Once all bias messages follow the SINEX format properly, could be activated temporaly in case Galileo L5 biases are absent */ 	
	if(bias[1]==0.0){
		if(obs.Sat.sys == +E_Sys::GPS) {
			if(ft == F1 && sig.code != +E_ObsCode::L1C) inpt_hard_bias(trace,obs,  1, bs, bsv, station, biaopt);
			if(ft == F1 && sig.code != +E_ObsCode::L2W) inpt_hard_bias(trace,obs, 20, bs, bsv, station, biaopt);
		}
		
		if( obs.Sat.sys == +E_Sys::GAL ){
			if(ft == F1 && sig.code != +E_ObsCode::L1X) inpt_hard_bias(trace,obs, 12, bs, bsv, station, biaopt);
			if(ft == F5 && sig.code != +E_ObsCode::L5X) inpt_hard_bias(trace,obs, 26, bs, bsv, station, biaopt);
			if(ft == F5 && sig.code != +E_ObsCode::L5Q) inpt_hard_bias(trace,obs, 25, bs, bsv, station, biaopt);
			if(ft == F5 && bs[1]    ==        0.0     )	inpt_hard_bias(trace,obs, 24, bs, bsv, station, biaopt);
		}
		bias[1]=bs[1];
		bvar[1]=bsv[1];
	}
#endif
	
	tracepdeex(3, trace, "\n %s  Biases for code %3d:   %9.4f %9.4f, vari: %10.4e %10.4e", obs.Sat.id().c_str(), sig.code, bias[0], bias[1], bvar[0], bvar[1]);
	
	sig.P_corr_m = sig.P       	- dAntSat - dAntRec - bias[0];
	sig.L_corr_m = sig.L * lam	- dAntSat - dAntRec - bias[1] - phw * lam;

#if 0
	double jump = cj.msJump * CLIGHT * 1e-3;
	sig.P_corr_m+=jump;
	sig.L_corr_m+=jump;
#endif
	
}


/* satellite antenna phase center variation ----------------------------------*/
void satantpcv(
	Vector3d&			rs,
	Vector3d&			rr,
	pcvacs_t&			pcv,
	map<int, double>&	dAntSat,
	double*				nad = nullptr)
{
	Vector3d ru = rr - rs;
	Vector3d rz = -rs;
	Vector3d eu = ru.normalized();
	Vector3d ez = rz.normalized();

	double cosa = eu.dot(ez);
	if (cosa < -1)	cosa = -1;
	if (cosa > +1)	cosa = +1;

	double nadir = acos(cosa);
	if (nad)
		*nad = nadir * R2D;

	interp_satantmodel(pcv, nadir, dAntSat);
	//antmodel_s(pcv,nadir,dAntSat);
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

	if (acsConfig.process_user)
	{
		/* mapping function */
		double m_w;
		double m_h = tropmapf(time, pos, azel, &m_w);

		if (azel[1] > 0)
		{
			/* m_w=m_0+m_0*cot(el)*(Gn*cos(az)+Ge*sin(az)): ref [6] */
			double cotz = 1 / tan(azel[1]);
			double grad_n = m_w * cotz * cos(azel[0]);
			double grad_e = m_w * cotz * sin(azel[0]);

			m_w += grad_n * tropStates[1];
			m_w	+= grad_e * tropStates[2];

			dTropDx[1] = grad_n * (tropStates[0] - zhd);
			dTropDx[2] = grad_e * (tropStates[0] - zhd);
		}

		dTropDx[0]	= m_w;
		var			= SQR(0.01);		//todo aaron, move this somewhere else, should use trop state variance?
		double value	= m_h * zhd
						+ m_w * (tropStates[0] - zhd);
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

/* tropospheric model ---------------------------------------------------------*/
int model_trop(
	GTime		time,
	double*		pos,
	double*		azel,
	double*		tropStates,
	double*		dTropDx,
	double&		dTrp,
	double&		var)
{
	dTrp = trop_model_prec(time, pos, azel, tropStates, dTropDx, var);

	return 1;
}

/* ionospheric model ---------------------------------------------------------*/
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
			int res = iontec(time, &nav, pos, azel, 1, dion, var);
			if (res)	var +=	VAR_IONEX;			// adding some extra errors to reflect modelling errors
			else		var =	VAR_IONO;

			return res;
		}
		case E_IonoMode::BROADCAST:
		{
			dion	= ionmodel(time, nav.ion_gps, pos, azel);
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
	}

	return 0;
}

void pppCorrections(
	Trace&		trace,
	ObsList&	obsList,
	Vector3d&	rRec,
	rtk_t&		rtk,
	Station&	rec)
{
	TestStack ts(__FUNCTION__);

	int lv = 3;

	double ep[6];
	time2epoch(obsList.front().time, ep);
	double jd	= ymdhms2jd(ep);
	double mjd	= jd - JD2MJD;


	double pos[3];
	ecef2pos(rRec.data(), pos);

	tracepde(3,trace, "pppCorrections  : n=%d\n", obsList.size());

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		TestStack ts(obs.Sat);
		SatStat&	satStat	= *(obs.satStat_ptr);
		auto&		lam		= obs.satNav_ptr->lamMap;

		double r = geodist(obs.rSat, rRec, satStat.e);						TestStack::testMat("r",		r);
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

// 		if (acsConfig.antexacs == 0)
// 		{
// 			std::cout << " Using antmodel() " << std::endl;			//todo aaron, broke this.
// 			antmodel(opt->pcvr, opt->antdel, obs.azel, opt->posopt[1], dAntRec);
// 		}

		//satellite and receiver antenna model
		map<int, double> dAntSat;
		if	(acsConfig.sat_pcv)
		{
			double ep[6];
			time2epoch(obs.time, ep);

			pcvacs_t* pcvsat = findAntenna(obs.Sat.id(), ep, nav);
			if (pcvsat)
			{
				satantpcv(obs.rSat, rRec, *pcvsat, dAntSat);
			}
			else
			{
				tracepde(1, trace,	"Warning: no satellite (%s) pcv information\n",	obs.Sat.id().c_str());
				continue;
			}
		}

		// phase windup model
		if (acsConfig.phase_windup)
		{
			bool pass = model_phw(rtk.sol.time, obs, rRec, satStat.phw);
			if (pass == false)
			{
				continue;
			}
		}

		map<int, double> dAntRec;

		for (auto& [ft, sig] : obs.Sigs)
		{
			TestStack ts("F" + std::to_string(ft));

			sig.Range = r;

			double rpcv = 0;

			/* receiver pco correction to the coordinates */
			if (rtk.pcvrec)
			{
				Vector3d pco_r;
				Vector3d dr2;

				recpco(rtk.pcvrec, ft, pco_r);
				enu2ecef(pos, pco_r.data(), dr2.data());    /* convert enu to xyz */

				/* get rec position and geometric distance for each frequency */
				Vector3d rRecFreq = rRec + dr2;

				sig.Range = geodist(obs.rSat, rRecFreq, satStat.e);

				/* calculate pcv */
				double azDeg = satStat.az * R2D;
				double elDeg = satStat.el * R2D;
				recpcv(rtk.pcvrec, ft, elDeg, azDeg, rpcv);
				dAntRec[ft] = rpcv;

												TestStack::testMat("obs.rSat",	obs.rSat);
												TestStack::testMat("rRecFreq",	rRecFreq);
												TestStack::testMat("rRec",	    rRec);
												TestStack::testMat("dr2",	     dr2);
												TestStack::testMat("sig.Range",	sig.Range);
			}
			// corrected phase and code measurements
			ClockJump cj = {};
			corr_meas(trace, obs, ft, satStat.el, dAntRec[ft], dAntSat[ft], satStat.phw, cj, rec);

			tracepde(lv, trace, "*---------------------------------------------------*\n");
			tracepde(lv, trace, " %.6f %sL%d satpcv              = %14.4f\n",                       mjd, obs.Sat.id().c_str(), ft, dAntSat[ft]);
			tracepde(lv, trace, " %.6f %sL%d recpcv              = %14.4f\n",                       mjd, obs.Sat.id().c_str(), ft, dAntRec[ft]);
			tracepde(lv, trace, " %.6f %sL%d az, el              = %14.4f %14.4f\n",                mjd, obs.Sat.id().c_str(), ft, satStat.az*R2D, satStat.el*R2D);
			tracepde(lv, trace, " %.6f %sL%d phw(cycle)          = %14.4f \n",                      mjd, obs.Sat.id().c_str(), ft, satStat.phw);
			tracepde(lv, trace, " %.6f %sL%d satpos+pco          = %14.4f %14.4f %14.4f\n",         mjd, obs.Sat.id().c_str(), ft, obs.rSat[0], obs.rSat[1], obs.rSat[2]);
			tracepde(lv, trace, " %.6f %sL%d dist                = %14.4f\n",                       mjd, obs.Sat.id().c_str(), ft, r);


								TestStack::testMat("dAntRec",	dAntRec[ft]);
								TestStack::testMat("dAntSat",	dAntSat[ft]);
		}

		if (acsConfig.ionoOpts.corr_mode == E_IonoMode::IONO_FREE_LINEAR_COMBO)
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

			//update distance measurement for iflc
			lcSig.Range	= sig1.Range * c1
						- sig2.Range * c2;

			lcSig.codeVar	= POW4(lam[F1]) * sig1.codeVar / POW2(POW2(lam[F1]) - POW2(lam[ft]))
							+ POW4(lam[ft]) * sig2.codeVar / POW2(POW2(lam[F1]) - POW2(lam[ft]));

			lcSig.phasVar 	= POW4(lam[F1]) * sig1.phasVar / POW2(POW2(lam[F1]) - POW2(lam[ft]))
							+ POW4(lam[ft]) * sig2.phasVar / POW2(POW2(lam[F1]) - POW2(lam[ft]));

			obs.Sigs[newType] = lcSig;

			obs.satStat_ptr->sigStatMap[newType].slip.any	= obs.satStat_ptr->sigStatMap[F1].slip.any
															| obs.satStat_ptr->sigStatMap[ft].slip.any;
		}
	}
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
		
		Vector3d delta = rec.snx.pos - rec.rtk.sol.sppRRec;
		
		double distance = delta.norm();
		
		if	( distance > 20
			&&rec.rtk.sol.sppRRec.norm() > 0)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "SINEX apriori for " << rec.id << " is " << distance << "m from SPP estimate";
		}
		
		return;
	}
	else
	{
		rec.aprioriPos		= rec.rtk.sol.sppRRec;
		sppUsed				= true;
	}
}

bool deweightMeas(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index)
{
	trace << std::endl << "Deweighting " << kfMeas.obsKeys[index] << std::endl;

	kfMeas.R[index] *= SQR(acsConfig.deweight_factor);

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

bool incrementPhaseSignalError(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	unsigned int* phaseRejectCount_ptr = (unsigned int*) metaDataMap["phaseRejectCount_ptr"];

	if (phaseRejectCount_ptr == nullptr)
	{
		return true;
	}

	unsigned int&	phaseRejectCount	= *phaseRejectCount_ptr;

	//increment counter, and clear the pointer so it cant be reset to zero in subsequent operations (because this is a failure)
	phaseRejectCount++;
	metaDataMap["phaseRejectCount_ptr"] = nullptr;

	return true;
}

bool resetPhaseSignalError(
	KFMeas&		kfMeas,
	int			index)
{
	map<string, void*>& metaDataMap = kfMeas.metaDataMaps[index];

	unsigned int* phaseRejectCount_ptr = (unsigned int*) metaDataMap["phaseRejectCount_ptr"];

	if (phaseRejectCount_ptr == nullptr)
	{
		return true;
	}

	unsigned int&	phaseRejectCount	= *phaseRejectCount_ptr;

	phaseRejectCount = 0;

	return true;
}


