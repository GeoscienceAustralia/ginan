
#include <math.h>

#include "observations.hpp"
#include "streamTrace.hpp"
#include "acsConfig.hpp"
#include "constants.h"
#include "satStat.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "common.hpp"
#include "enums.h"


void obsVariances(
	ObsList& obsList)
{
	for (auto& obs			: obsList)
	for (auto& [ft, Sig]	: obs.Sigs)
	{
		auto& receiverOpts = acsConfig.getRecOpts(obs.mount);

		//get the sigma for this frequency, (or the last one in the list)
		int freqTypeCode = ft;
		int freqTypePhas = ft;
		if (freqTypeCode >= receiverOpts.code_sigmas.size())		freqTypeCode = receiverOpts.code_sigmas.size() - 1;
		if (freqTypePhas >= receiverOpts.phas_sigmas.size())		freqTypePhas = receiverOpts.phas_sigmas.size() - 1;

		double sigmaCode = receiverOpts.code_sigmas[freqTypeCode];
		double sigmaPhas = receiverOpts.phas_sigmas[freqTypePhas];

		double el = obs.satStat_ptr->el;
		if (el == 0)
			el = PI/4;

		double elevationScaling = 1;
		switch (receiverOpts.error_model)
		{
			case E_NoiseModel::UNIFORM:					{	elevationScaling = 1;				break;	}
			case E_NoiseModel::ELEVATION_DEPENDENT:		{	elevationScaling = 1 / sin(el);		break;	}
// 			case E_NoiseModel::ELEVATION_DEPENDENT2:	{	elevationScaling = break;	}
		}

		sigmaCode *= elevationScaling;
		sigmaPhas *= elevationScaling;

		Sig.codeVar = sigmaCode * sigmaCode;
		Sig.phasVar = sigmaPhas * sigmaPhas;
	}
}

/* ionosphere model ------------------------------------------------------------
* compute ionospheric delay by broadcast ionosphere model (klobuchar model)
* args   : gtime_t t        I   time (gpst)
*          double *ion      I   iono model parameters {a0,a1,a2,a3,b0,b1,b2,b3}
*          double *pos      I   receiver position {lat,lon,h} (rad,m)
*          double *azel     I   azimuth/elevation angle {az,el} (rad)
* return : ionospheric delay (L1) (m)
*-----------------------------------------------------------------------------*/
double ionmodel(
	GTime			t,
	const double*	ion,
	const double*	pos,
	const double*	azel)
{
	const double ion_default[] = /* 2004/1/1 */
	{
		0.1118E-07, -0.7451E-08, -0.5961E-07, 0.1192E-06,
		0.1167E+06, -0.2294E+06, -0.1311E+06, 0.1049E+07
	};

	if	( pos[2]	< -1E3
		||azel[1]	<= 0)
	{
		return 0;
	}

	if (norm(ion, 8) <= 0)
		ion = ion_default;

	/* earth centered angle (semi-circle) */
	double psi = 0.0137 / (azel[1] / PI + 0.11) - 0.022;

	/* subionospheric latitude/longitude (semi-circle) */
	double phi = pos[0] / PI + psi * cos(azel[0]);

	if      (phi > +0.416)		phi = +0.416;
	else if (phi < -0.416)		phi = -0.416;

	double lam = pos[1] / PI + psi * sin(azel[0]) / cos(phi * PI);

	/* geomagnetic latitude (semi-circle) */
	phi += 0.064 * cos((lam - 1.617) * PI);

	/* local time (s) */
	int week;
	double tt = 43200 * lam + time2gpst(t, &week);
	tt -= floor(tt / 86400) * 86400; /* 0<=tt<86400 */

	/* slant factor */
	double f = 1 + 16 * pow(0.53 - azel[1] / PI, 3);

	/* ionospheric delay */
	double amp = ion[0] + phi * (ion[1] + phi * (ion[2] + phi * ion[3]));
	double per = ion[4] + phi * (ion[5] + phi * (ion[6] + phi * ion[7]));
	amp = amp < 0 		? 0		: amp;
	per = per < 72000	? 72000	: per;
	double x = 2 * PI * (tt - 50400) / per;

	return CLIGHT * f * (fabs(x) < 1.57 ? 5E-9 + amp * (1 + x * x * (-0.5 + x * x / 24)) : 5E-9);
}
/* ionosphere mapping function -------------------------------------------------
* compute ionospheric delay mapping function by single layer model
* args   : double *pos      I   receiver position {lat,lon,h} (rad,m)
*          double *azel     I   azimuth/elevation angle {az,el} (rad)
* return : ionospheric mapping function
*-----------------------------------------------------------------------------*/
double ionmapf(Vector3d& rr, const double el)
{
	double pos[3];
	ecef2pos(rr.data(), pos);

	if (pos[2] >= HION)
		return 1;

	double rRec = RE_WGS84 + pos[2];
	double rIon = RE_WGS84 + HION;
	return 1 / cos(		asin(	  rRec / rIon	* cos(el))	);
}
/* ionospheric pierce point position -------------------------------------------
* compute ionospheric pierce point (ipp) position and slant factor
* args   : double *pos      I   receiver position {lat,lon,h} (rad,m)
*          double *azel     I   azimuth/elevation angle {az,el} (rad)
*          double re        I   earth radius (km)
*          double hion      I   altitude of ionosphere (km)
*          double *posp     O   pierce point position {lat,lon,h} (rad,m)
* return : slant factor
* notes  : see ref [2], only valid on the earth surface
*          fixing bug on ref [2] A.4.4.10.1 A-22,23
*-----------------------------------------------------------------------------*/
double ionppp(const double* pos, const double* azel, double re,
					double hion, double* posp)
{
	double cosaz, rp, ap, sinap, tanap;

	rp = re / (re + hion) * cos(azel[1]);
	ap = PI / 2 - azel[1] - asin(rp);
	sinap = sin(ap);
	tanap = tan(ap);
	cosaz = cos(azel[0]);
	posp[0] = asin(sin(pos[0]) * cos(ap) + cos(pos[0]) * sinap * cosaz);

	if	( (pos[0] > +70 * D2R && +tanap * cosaz > tan(PI / 2 - pos[0]))
		||(pos[0] < -70 * D2R && -tanap * cosaz > tan(PI / 2 + pos[0])))
	{
		posp[1] = pos[1] + PI - asin(sinap * sin(azel[0]) / cos(posp[0]));
	}
	else
	{
		posp[1] = pos[1] + asin(sinap * sin(azel[0]) / cos(posp[0]));
	}

	return 1 / sqrt(1 - rp * rp);
}
/* troposphere model -----------------------------------------------------------
* compute tropospheric delay by standard atmosphere and saastamoinen model
* args   : gtime_t time     I   time
*          double *pos      I   receiver position {lat,lon,h} (rad,m)
*          double *azel     I   azimuth/elevation angle {az,el} (rad)
*          double humi      I   relative humidity
* return : tropospheric delay (m)
*-----------------------------------------------------------------------------*/
double tropmodel(GTime time, const double* pos, const double* azel, double humi)
{
	const double temp0 = 15; /* temparature at sea level */
	double hgt, pres, temp, e, z, trph, trpw;

	if	(  pos[2] < -100
		|| 1E4 < pos[2]
		|| azel[1] <= 0)
		return 0;

	/* standard atmosphere */
	hgt = pos[2] < 0 ? 0 : pos[2];

	pres = 1013.25 * pow(1 - 2.2557E-5 * hgt, 5.2568);
	temp = temp0 - 6.5E-3 * hgt + 273.16;
	e = 6.108 * humi * exp((17.15 * temp - 4684) / (temp - 38.45));

	/* saastamoninen model */
	z = PI / 2 - azel[1];
	trph = 0.0022768 * pres / (1 - 0.00266 * cos(2 * pos[0]) - 0.00028 * hgt / 1E3) / cos(z);
	trpw = 0.002277 * (1255 / temp + 0.05) * e / cos(z);

	return trph + trpw;
}
/* troposphere zhd for acs------------------------------------------------------
* compute tropospheric delay by standard atmosphere and saastamoinen model
* args   : gtime_t time     I   time
*          double *pos      I   receiver position {lat,lon,h} (rad,m)
*          double *azel     I   azimuth/elevation angle {az,el} (rad)
*          double humi      I   relative humidity
* return : tropospheric delay (m)
*-----------------------------------------------------------------------------*/
double tropacs(const double* pos, const double* azel, double* map)
{
	const double temp0 = 15.0; /* temparature at sea level */
	double hgt, pres, temp, e, z, a[2], b[2], c[2];
	int i;

	if	( pos[2]	< -100
		||pos[2]	> 1E4
		||azel[1]	<= 0)
	{
		return 0;
	}

	/* standard atmosphere */
	hgt = pos[2] < 0 ? 0 : pos[2];

	/* consider the ellipsoid or geoid height */
	hgt = pos[2];

	/* standard atmosphere temperature, pressure */
	temp = temp0 - 6.5E-3 * hgt + 273.15;
	pres = 1013.25 * pow(288.15 / temp, -5.255877);
	e = 0.5 * exp(24.3702 - 6162.3496 / temp);

	/* parameters of dry mapping function */
	a[0]	= 0.1237 * pow(10, -2)
			+ 0.1316 * pow(10, -6) * (pres - 1000)
			+ 0.8057 * pow(10, -5) * sqrt(e)
			+ 0.1378 * pow(10, -5) * (temp - 288.15);

	b[0]	= 0.3333 * pow(10, -2)
			+ 0.1946 * pow(10, -6) * (pres - 1000)
			+ 0.1747 * pow(10, -6) * sqrt(e)
			+ 0.1040 * pow(10, -6) * (temp - 288.15);

	c[0]	= 0.078;

	/* parameters of wet mapping function */
	a[1]	= 0.5236 * pow(10, -3)
			+ 0.2471 * pow(10, -6) * (pres - 1000)
			- 0.1328 * pow(10, -4) * sqrt(e)
			+ 0.1724 * pow(10, -6) * (temp - 288.15);

	b[1]	= 0.1705 * pow(10, -2)
			+ 0.7384 * pow(10, -6) * (pres - 1000)
			+ 0.2147 * pow(10, -4) * sqrt(e)
			+ 0.3767 * pow(10, -6) * (temp - 288.15);

	c[1]	= 0.05917;

	z = PI / 2 - azel[1];

	/* mapping function */
	for (i = 0; i < 2; i++)
		map[i] = (1 + a[i] / (1 + b[i] / (1 + c[i]))) / (cos(z) + a[i] / (cos(z) + b[i] / (cos(z) + c[i])));

#if (0)
	return 0.002277 * (map[0] * pres	/ (1 - 0.00266 * cos(2 * pos[0]) - 0.00028 * hgt / 1E3) + map[1] * (1255 / temp + 0.05) * e);
#elif (0)
	return 0.002277 * (map[0] * pres	/ (1 - 0.00266 * cos(2 * pos[0]) - 0.00028 * hgt / 1E3));
#else
	return 0.002277 * (pres				/ (1 - 0.00266 * cos(2 * pos[0]) - 0.00028 * hgt / 1E3));
#endif
}
#ifndef IERS_MODEL

double interpc(const double coef[], double lat)
{
	int i = (int)(lat / 15);

	if		(i < 1) 		return coef[0];
	else if (i > 4) 		return coef[4];

	return coef[i - 1] * (1 - lat / 15 + i) + coef[i] * (lat / 15 - i);
}

double mapf(double el, double a, double b, double c)
{
	double sinel = sin(el);
	return	(1 + a /
				(1 + b /
					(1 + c))) / (sinel + (a /
										(sinel + b /
												(sinel + c))));
}
double nmf(
	GTime			time,
	const double	pos[],
	const double	azel[],
	double*			mapfw)
{
	/* ref [5] table 3 */
	/* hydro-ave-a,b,c, hydro-amp-a,b,c, wet-a,b,c at latitude 15,30,45,60,75 */
	const double coef[][5] =
	{
		{ 1.2769934E-3, 1.2683230E-3, 1.2465397E-3, 1.2196049E-3, 1.2045996E-3},
		{ 2.9153695E-3, 2.9152299E-3, 2.9288445E-3, 2.9022565E-3, 2.9024912E-3},
		{ 62.610505E-3, 62.837393E-3, 63.721774E-3, 63.824265E-3, 64.258455E-3},

		{ 0.0000000E-0, 1.2709626E-5, 2.6523662E-5, 3.4000452E-5, 4.1202191E-5},
		{ 0.0000000E-0, 2.1414979E-5, 3.0160779E-5, 7.2562722E-5, 11.723375E-5},
		{ 0.0000000E-0, 9.0128400E-5, 4.3497037E-5, 84.795348E-5, 170.37206E-5},

		{ 5.8021897E-4, 5.6794847E-4, 5.8118019E-4, 5.9727542E-4, 6.1641693E-4},
		{ 1.4275268E-3, 1.5138625E-3, 1.4572752E-3, 1.5007428E-3, 1.7599082E-3},
		{ 4.3472961E-2, 4.6729510E-2, 4.3908931E-2, 4.4626982E-2, 5.4736038E-2}
	};
	const double aht[] = { 2.53E-5, 5.49E-3, 1.14E-3}; /* height correction */

	double el	= azel[1];
	double lat	= pos[0] * R2D;
	double hgt	= pos[2];

	if (el <= 0)
	{
		if (mapfw)
			*mapfw = 0;

		return 0;
	}

	/* year from doy 28, added half a year for southern latitudes */
	double y = (time2doy(time) - 28) / 365.25 + (lat < 0 ? 0.5 : 0);

	double cosy = cos(2 * PI * y);
	lat = fabs(lat);

	double ah[3];
	double aw[3];
	for (int i = 0; i < 3; i++)
	{
		ah[i] = interpc(coef[i    ], lat) - interpc(coef[i + 3], lat) * cosy;
		aw[i] = interpc(coef[i + 6], lat);
	}

	/* ellipsoidal height is used instead of height above sea level */
	double dm = (1 / sin(el) - mapf(el, aht[0], aht[1], aht[2])) * hgt / 1E3;

	if (mapfw)
		*mapfw = mapf(el, aw[0], aw[1], aw[2]);

	return mapf(el, ah[0], ah[1], ah[2]) + dm;
}
#endif /* !IERS_MODEL */

/* troposphere mapping function ------------------------------------------------
* compute tropospheric mapping function by NMF
* args   : gtime_t t        I   time
*          double *pos      I   receiver position {lat,lon,h} (rad,m)
*          double *azel     I   azimuth/elevation angle {az,el} (rad)
*          double *mapfw    IO  wet mapping function (NULL: not output)
* return : dry mapping function
* note   : see ref [5] (NMF) and [9] (GMF)
*          original JGR paper of [5] has bugs in eq.(4) and (5). the corrected
*          paper is obtained from:
*          ftp://web.haystack.edu/pub/aen/nmf/NMF_JGR.pdf
*-----------------------------------------------------------------------------*/
double tropmapf(
	GTime			time,
	const double	pos[],
	const double	azel[],
	double*			mapfw)
{
#ifdef IERS_MODEL
	const double ep[] = {2000, 1, 1, 12, 0, 0};
	double mjd, lat, lon, hgt, zd, gmfh, gmfw;
#endif
// 	trace(4, "tropmapf: pos=%10.6f %11.6f %6.1f azel=%5.1f %4.1f\n",
// 	      pos[0]*R2D,
// 	      pos[1]*R2D,
// 	      pos[2],
// 	      azel[0]*R2D,
// 	      azel[1]*R2D);

	if	( pos[2] < -1000
		||pos[2] > 20000)
	{
		if (mapfw)
			*mapfw = 0;

		return 0;
	}

#ifdef IERS_MODEL
	mjd = 51544.5 + (timediff(time, epoch2time(ep))) / 86400;
	lat = pos[0];
	lon = pos[1];
	hgt = pos[2] - geoidh(pos); /* height in m (mean sea level) */
	zd  = PI / 2 - azel[1];

	/* call GMF */
	gmf_(&mjd, &lat, &lon, &hgt, &zd, &gmfh, &gmfw);

	if (mapfw)
		*mapfw = gmfw;

	return gmfh;
#else
	return nmf(time, pos, azel, mapfw); /* NMF */
#endif
}
