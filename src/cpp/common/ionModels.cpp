
// #pragma GCC optimize ("O0")

#include <math.h>

#include "observations.hpp"
#include "navigation.hpp"
#include "coordinates.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "satStat.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "enums.h"


#define VAR_IONO    	SQR(60.0)       // init variance iono-delay
#define VAR_IONEX   	SQR(0.0)
#define ERR_BRDCI   	0.5             // broadcast iono model error factor

#define VAR_NOTEC   SQR(30.0)   /* variance of no tec */
#define MIN_EL      0.0         /* min elevation angle (rad) */
#define MIN_HGT     -1000.0     /* min user height (m) */


int dataindex(
	int i,
	int j,
	int k,
	const int* ndata);

/* ionosphere model ------------------------------------------------------------
* compute ionospheric delay by broadcast ionosphere model (klobuchar model)
* args   : gtime_t t        I   time (gpst)
*          double *ion      I   iono model parameters {a0,a1,a2,a3,b0,b1,b2,b3}
*          double *pos      I   receiver position {lat,lon,h} (rad,m)
*          double *azel     I   azimuth/elevation angle {az,el} (rad)
* return : ionospheric delay (L1) (m)
*-----------------------------------------------------------------------------*/
double ionmodel(
	GTime				t,
	const double*		ion,
	const VectorPos&	pos,
	const AzEl&			azel)
{
	const double ion_default[] = /* 2004/1/1 */
	{
		0.1118E-07, -0.7451E-08, -0.5961E-07, 0.1192E-06,
		0.1167E+06, -0.2294E+06, -0.1311E+06, 0.1049E+07
	};

	if	( pos.hgt()	< -1000
		||azel.el	<= 0)
	{
		return 0;
	}

	if	( ion == nullptr
		||norm(ion, 8) <= 0)
	{
		ion = ion_default;
	}

	/* earth centered angle (semi-circle) */
	double psi = 0.0137 / (azel.el / PI + 0.11) - 0.022;

	/* subionospheric latitude/longitude (semi-circle) */
	double phi = pos.lat() / PI + psi * cos(azel.az);

	if      (phi > +0.416)		phi = +0.416;
	else if (phi < -0.416)		phi = -0.416;

	double lam = pos.lon() / PI + psi * sin(azel.az) / cos(phi * PI);

	/* geomagnetic latitude (semi-circle) */
	phi += 0.064 * cos((lam - 1.617) * PI);

	/* local time (s) */
	// int week;
	// double tt = 43200 * lam + time2gpst(t, &week);
	double tt = GTow(t) + 43200.0 * lam;
	tt -= floor(tt / 86400) * 86400; /* 0<=tt<86400 */

	/* slant factor */
	double f = 1 + 16 * pow(0.53 - azel.el / PI, 3);

	/* ionospheric delay */
	double amp = ion[0] + phi * (ion[1] + phi * (ion[2] + phi * ion[3]));
	double per = ion[4] + phi * (ion[5] + phi * (ion[6] + phi * ion[7]));
	amp = amp < 0 		? 0		: amp;
	per = per < 72000	? 72000	: per;
	double x = 2 * PI * (tt - 50400) / per;

	return CLIGHT * f * (fabs(x) < 1.57 ? 5E-9 + amp * (1 + x * x * (-0.5 + x * x / 24)) : 5E-9);
}

/** ionosphere mapping function
*/
double ionmapf(
	const VectorPos&	pos,	///< receiver position in geocentric spherical coordinates
	const AzEl&			azel,	///< satellite azimuth/elevation angle (rad)
	E_IonoMapFn			mapFn,	///< model of mapping function
	double				hion)	///< layer height (km)
{
	double alpha = 1;
	switch (mapFn)
	{
		case E_IonoMapFn::SLM:		// fallthrough
		case E_IonoMapFn::MLM:							break;	// same to SLM but need to call the function multiple times
		case E_IonoMapFn::MSLM:			alpha = 0.9782;	break;
		case E_IonoMapFn::KLOBUCHAR:	return 1 + 16 * pow(0.53 - azel.el / PI, 3);
	}

	double rp = RE_MEAN / (RE_MEAN + hion) * sin(alpha * (PI / 2 - azel.el));

	return 1 / sqrt(1 - SQR(rp));
}
/* ionospheric pierce point position -------------------------------------------
* compute ionospheric pierce point (ipp) position and slant factor
* args   : double *pos      I   receiver position {lat,lon,h} (rad,m)
*          double *azel     I   azimuth/elevation angle {az,el} (rad)
*          double re        I   earth radius (km)
*          double hion      I   altitude of ionosphere (km)
*          double *posp     O   pierce point position {lat,lon,r} (rad,m) in geocentric spherical coordinate system
* return : slant factor
* notes  : see ref [2], only valid on the earth surface
*          fixing bug on ref [2] A.4.4.10.1 A-22,23
*-----------------------------------------------------------------------------*/
double ionppp(
	const VectorPos&	pos,
	const AzEl&			azel,
	double				re,
	double				hion,
	VectorPos&			posp)
{
	double ri = re + hion;
	double rp = re / ri * cos(azel.el);
	double ap = PI / 2 - azel.el - asin(rp);
	double sinap = sin(ap);
	double tanap = tan(ap);
	double cosaz = cos(azel.az);
	posp[0] = asin(sin(pos.lat()) * cos(ap) + cos(pos.lat()) * sinap * cosaz);

	if	( (pos.lat() > +70 * D2R && +tanap * cosaz > tan(PI / 2 - pos.lat()))
		||(pos.lat() < -70 * D2R && -tanap * cosaz > tan(PI / 2 + pos.lat())))
	{
		posp.lon() = pos.lon() + PI	- asin(sinap * sin(azel.az) / cos(posp.lat()));
	}
	else
	{
		posp.lon() = pos.lon()		+ asin(sinap * sin(azel.az) / cos(posp.lat()));
	}

	posp[2] = ri * 1000;	// geocentric radius

	return 1 / sqrt(1 - SQR(rp));
}

/** interpolate tec grid data
 */
int interpTec(
	const	TEC&		tec,
			int			k,
	const	VectorPos&	posp,
			double&		value,
			double&		rms)
{
	tracepdeex(6,std::cout, "%s: k=%d posp=%.2f %.2f\n",__FUNCTION__, k, posp[0]*R2D, posp[1]*R2D);

	value	= 0;
	rms		= 0;

	if	( tec.lats[2] == 0
		||tec.lons[2] == 0)
	{
		return 0;
	}

	double dlat = posp.latDeg() - tec.lats[0];
	double dlon = posp.lonDeg() - tec.lons[0];

	if (tec.lons[2] > 0)	dlon -= floor( dlon / 360) * 360; /*  0<=dlon<360 */
	else					dlon += floor(-dlon / 360) * 360; /* -360<dlon<=0 */

	double a = dlat / tec.lats[2];
	double b = dlon / tec.lons[2];
	int i = (int) floor(a); 			a -= i;
	int j = (int) floor(b); 			b -= j;

	/* get gridded tec data */
	double d[4] = {};
	double r[4] = {};
	for (int n = 0; n < 4; n++)
	{
		int index = dataindex(i + (n % 2), j + (n < 2 ? 0 : 1), k, tec.ndata);
		if (index < 0)
			continue;

		auto& tecPoint = tec.tecPointVector[index];

		d[n] = tecPoint.data;
		r[n] = tecPoint.rms;
	}

	if	(  d[0] > 0
		&& d[1] > 0
		&& d[2] > 0
		&& d[3] > 0)
	{
		/* bilinear interpolation (inside of grid) */
		value	= (1 - a) * (1 - b) * d[0] 		+ a * (1 - b) * d[1] 		+ (1 - a) * b * d[2] 		+ a * b * d[3];
		rms		= (1 - a) * (1 - b) * r[0] 		+ a * (1 - b) * r[1] 		+ (1 - a) * b * r[2] 		+ a * b * r[3];

		// if (fdebug)
		tracepdeex(6,std::cout, "  gridpoints: %8.2f %8.2f %8.2f %8.2f -> %9.3f\n", d[0], d[1], d[2], d[3], value);
	}
	/* nearest-neighbour extrapolation (outside of grid) */
	else if (a <=	0.5 && b <= 0.5 && d[0] > 0) 	{	value = d[0];		rms = r[0];		}
	else if (a >	0.5 && b <= 0.5 && d[1] > 0) 	{	value = d[1];		rms = r[1];		}
	else if (a <=	0.5 && b >	0.5 && d[2] > 0) 	{	value = d[2];		rms = r[2];		}
	else if (a >	0.5 && b >	0.5 && d[3] > 0) 	{	value = d[3];		rms = r[3];		}
	else
	{
		i = 0;

		for (int n = 0; n < 4; n++)
		if (d[n] > 0)
		{
			i++;
			value	+= d[n];
			rms		+= r[n];
		}

		if (i == 0)
			return 0;

		value	/= i;
		rms		/= i;
	}

	return 1;
}

/** ionosphere delay by tec grid data
 */
bool ionDelay(
	GTime				time,			///< Time
	const TEC&			tec,			///< Input electron content data
	const VectorPos&	pos,			///< Position of receiver
	const AzEl&			azel,			///< Azimuth and elevation of signal path
	E_IonoMapFn			mapFn,			///< model of mapping function
	double				layerHeight,	///< Mapping function layer height
	E_IonoFrame			frame,			///< reference frame
	double&				delay,			///< Delay in meters
	double&				var)			///< Variance
{
	// if (fdebug)
	// 	fprintf(fdebug, "%s: time=%s pos=%.1f %.1f azel=%.1f %.1f\n", __FUNCTION__, time.to_string(0).c_str(), pos[0]*R2D, pos[1]*R2D, azel[0]*R2D, azel[1]*R2D);

	delay	= 0;
	var		= 0;

	for (int i = 0; i < tec.ndata[2]; i++)
	{
		double hion = tec.hgts[0] + tec.hgts[2] * i;

		/* ionospheric pierce point position */
		VectorPos posp;
		ionppp(pos, azel, tec.rb, hion, posp);
		double fs = ionmapf(pos, azel, mapFn, layerHeight);

		if (frame == +E_IonoFrame::SUN_FIXED)
		{
			/* earth rotation correction (sun-fixed coordinate) */
			posp[1] += 2 * PI * (time - tec.time).to_double() / 86400;
		}

		/* interpolate tec grid data */
		double rms;
		double vtec;
		if (interpTec(tec, i, posp, vtec, rms) == false)
			return false;

		const double fact = TEC_CONSTANT / SQR(FREQ1); /* tecu->L1 iono (m) */
		delay	+= fact * fs * vtec;
		var		+= SQR(fact * fs * rms);
	}

	tracepdeex(6,std::cout, "%s: delay=%7.2f std=%6.2f\n",__FUNCTION__, delay, sqrt(var));

	return true;
}


/** ionosphere model by tec grid data
 * Before calling the function, read tec grid data by calling readTec()
*          return ok with delay=0 and var=VAR_NOTEC if el < MIN_EL or h < MIN_HGT
*/
bool iontec(
	GTime				time,			///< time (gpst)
	const Navigation*	nav,			///< navigation data
	const VectorPos&	pos,			///< receiver position {lat,lon,h} (rad,m)
	const AzEl&			azel,			///< azimuth/elevation angle {az,el} (rad)
	E_IonoMapFn			mapFn,			///< model of mapping function
	double				layerHeight,	///< Mapping function layer height
	E_IonoFrame			frame,			///< reference frame
	double&				delay,			///< ionospheric delay (L1) (m)
	double&				var)			///< ionospheric dealy (L1) variance (m^2)
{
	// if (fdebug)
	// 	fprintf(fdebug, "iontec  : time=%s pos=%.1f %.1f azel=%.1f %.1f nt=%ld\n", time.to_string(0).c_str(), pos[0]*R2D, pos[1]*R2D, azel[0]*R2D, azel[1]*R2D, nav->tecList.size());

	delay	= 0;
	var		= VAR_NOTEC;

	if	(  azel.el		< MIN_EL
		|| pos.hgt()	< MIN_HGT)
	{
		return true;
	}

	auto it = nav->tecMap.lower_bound(time);
	if (it == nav->tecMap.end())
	{
		// if (fdebug)
		// 	fprintf(fdebug, "%s: tec grid out of period\n", time.to_string(0).c_str());

		return true;
	}

	bool pass[2] = {};
	double dels[2];
	double vars[2];

	auto& [t0, tec0] = *it;
	pass[0] = ionDelay(time, tec0, pos, azel, mapFn, layerHeight, frame, dels[0], vars[0]);

	if (it == nav->tecMap.begin())
	{
		delay	= dels[0];
		var		= vars[0];
		return pass[0];
	}

	//go forward and get the next timestep if available
	it--;

	auto& [t1, tec1] = *it;
	pass[1] = ionDelay(time, tec1, pos, azel, mapFn, layerHeight, frame, dels[1], vars[1]);


	if	(  pass[0]
		&& pass[1])
	{
		/* linear interpolation by time */
		double tt	= (tec1.time	- tec0.time).to_double();
		double a	= (time			- tec0.time).to_double() / tt;

		delay	= dels[0] * (1 - a) + dels[1] * a;
		var		= vars[0] * (1 - a) + vars[1] * a;
	}
	else if (pass[0])   /* nearest-neighbour extrapolation by time */
	{
		delay	= dels[0];
		var		= vars[0];
	}
	else if (pass[1])
	{
		delay	= dels[1];
		var		= vars[1];
	}
	else
	{
		// if (fdebug)
		// 	fprintf(fdebug, "%s: tec grid out of area pos=%6.2f %7.2f azel=%6.1f %5.1f\n",  time.to_string(0).c_str(), pos[0]*R2D, pos[1]*R2D, azel[0]*R2D, azel[1]*R2D);

		return false;
	}

	// if (fdebug)
	// 	fprintf(fdebug, "iontec  : delay=%5.2f std=%5.2f\n", delay, sqrt(var));

	return true;
}


/** ionospheric model
 */
bool ionoModel(
	GTime&		time,
	VectorPos&	pos,
	AzEl&		azel,
	E_IonoMapFn	mapFn,
	E_IonoMode	mode,
	double		layerHeight,
	double		ionoState,
	double&		dion,
	double&		var)
{
	switch (mode)
	{
		case E_IonoMode::TOTAL_ELECTRON_CONTENT:
		{
			int pass = iontec(time, &nav, pos, azel, mapFn, layerHeight, E_IonoFrame::SUN_FIXED, dion, var);
			if (pass)	var +=	VAR_IONEX;			// adding some extra errors to reflect modelling errors
			else		var =	VAR_IONO;

			return pass;
		}
		case E_IonoMode::BROADCAST:	// only GPS Klobuchar model implemented
		{
			E_Sys			sys		= E_Sys::GPS;
			E_NavMsgType	type	= defNavMsgType[sys];

			auto ion_ptr = seleph<ION>(nullStream, time, sys, type, nav);

			double* vals = nullptr;
			if (ion_ptr != nullptr)
				vals = ion_ptr->vals;

			dion	= ionmodel(time, vals, pos, azel);
			var		= SQR(dion * ERR_BRDCI);

			return true;
		}
		case E_IonoMode::ESTIMATE:
		{
			dion	= ionoState;
			var		= 0;

			return true;
		}
		case E_IonoMode::IONO_FREE_LINEAR_COMBO:
		{
			dion	= 0;
			var		= 0;

			return true;
		}
		default:
		{
			return false;
		}
	}
}
