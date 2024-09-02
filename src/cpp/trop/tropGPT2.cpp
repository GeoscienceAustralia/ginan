
#include <fstream>
#include <cstring>
#include <math.h>
#include <array>

using std::ifstream;
using std::array;


#include "tropModels.hpp"

struct GPTVals
{
	double pressure		= 0;	///< pressure (hPa)
	double temperature	= 0;	///< temperature (celsius)
	double deltaT		= 0;	///< delta temp (deg/km)
	double waterVp		= 0;	///< water vp (hPa)
	double hydroCoef	= 0;	///< hydrostatic coef at 0m
	double wetCoef		= 0;	///< wet coef
	double undulation	= 0;	///< undulation (m)
};

/** gpt grid file contents
 */
struct GptGrid
{
	double			lat		= 0;		///< lat grid (degree)
	double			lon		= 0;		///< lon grid (degree)
	array<double,5>	pres;				///< pressure										a0 A1 B1 A2 B2 (pascal)
	array<double,5>	temp;				///< temperature									a0 A1 B1 A2 B2 (kelvin)
	array<double,5>	humid;				///< humidity										a0 A1 B1 A2 B2 (kg/kg)
	array<double,5>	tlaps;				///< elapse rate									a0 A1 B1 A2 B2 (kelvin/m)
	array<double,5>	ah;					///< hydrostatic	mapping function coefficient	a0 A1 B1 A2 B2
	array<double,5>	aw;					///< wet			mapping function coefficient	a0 A1 B1 A2 B2
	double			undu	= 0;		///< geoid undulation (m)
	double			hgt		= 0;		///< orthometric height (m)
};

vector<GptGrid>		globalGPT2Grids			= {};		///< gpt grid information
bool				globalGPT2GridsReady	= false;	///< gpt grid information read


/* sign function ---------------------------------------------------------------
* args     :       double x                I       input number
*
* return   :       1, 0, or -1
* ---------------------------------------------------------------------------*/
int sign(double x)
{
	if		(x >  0)	return +1;
	else if	(x == 0)	return  0;
	else				return -1;
}
/** vienna mapping function.
 * coefficients coming from either GPT2 or from vmf file
 */
void vmf1(
	const double	ah,		///< vmf1 dry coefficients
	const double	aw,		///< vmf1 wet coefficients
	double			mjd,	///< modified julian date
	double			lat,	///< ellipsoidal lat (rad)
	double			hgt,	///< height (m)
	double			elev,	///< zenith distance (rad)
	double&			dryMap,
	double&			wetMap)
{
	double doy = mjd - 44239 + 1 - 28;

	/* hydrostatic mapping function */
	double c0h		= 0.062;
	double c11h;
	double c10h;
	double phh;
	if (lat < 0)	{	phh = PI;	c11h = 0.007;	c10h = 0.002;	}	/* southern hemisphere */
	else			{	phh = 0;	c11h = 0.005;	c10h = 0.001;	}	/* northern hemisphere */

	double bh		= 0.0029;
	double ch		= c0h + ((cos(doy / 365.25 * 2*PI + phh) + 1) * c11h / 2 + c10h) * (1 - cos(lat));
	dryMap = mapHerring(elev, ah, bh, ch);

	/* height correction */
	double aht = 2.53e-5;
	double bht = 5.49e-3;
	double cht = 1.14e-3;
	dryMap+= (1 / sin(elev) - mapHerring(elev, aht, bht, cht)) * hgt / 1000;

	/* wet mapping function */
	double bw	= 0.00146;
	double cw	= 0.04391;
	wetMap = mapHerring(elev, aw, bw, cw);
}

/** read GPT grid file
 */
void readgrid(
	string		filepath)	///< vmf1 coefficients filename
{
	int i = 0;

	auto& gptGrids = globalGPT2Grids;

	ifstream fileStream(filepath);
	if (!fileStream)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error opening gpt grid file" << filepath << "\n";
		return;
	}

	while (fileStream)
	{
		string line;

		getline(fileStream, line);

		char* buff = &line[0];

		/* ignore the first line */
		if (strchr(buff,'%'))
			continue;

		int j = 0;

		/* loop over each line */
		char* p = strtok(buff," ");
		double val[40];
		while (p)
		{
			sscanf(p,"%lf", &val[j]);
			j++;
			p = strtok(nullptr," ");
		}

		GptGrid gptGridPoint;

		/* assign lat lon */
		gptGridPoint.lat = val[0];
		gptGridPoint.lon = val[1];

		/* assign pres, temp, humid, tlaps, ah, aw */
		for (int k = 0; k < 5; k++)
		{
			gptGridPoint.pres	[k] = val[k+2];			/* pressure */
			gptGridPoint.temp	[k] = val[k+7];			/* temperature */
			gptGridPoint.humid	[k] = val[k+12]/1000;	/* humidity */
			gptGridPoint.tlaps	[k] = val[k+17]/1000;	/* temperature elapse rate */
			gptGridPoint.ah		[k] = val[k+24]/1000;	/* ah */
			gptGridPoint.aw		[k] = val[k+29]/1000;	/* aw */
		}

		/* assign undulation and height */
		gptGridPoint.undu	= val[22];
		gptGridPoint.hgt	= val[23];

		gptGrids.push_back(gptGridPoint);
	}

	globalGPT2GridsReady = true;
}

/** coefficients multiplication
 */
double coef(
	const array<double,5>&	a,
	double					cosfy,
	double					sinfy,
	double					coshy,
	double					sinhy)
{
	return	+ a[0]
			+ a[1] * cosfy
			+ a[2] * sinfy
			+ a[3] * coshy
			+ a[4] * sinhy;
}

double coefr(
	double p1,
	double p2,
	double l1,
	double l2,
	double a[4])
{
	double r[2];

	r[0] = p2 * a[0] + p1 * a[1];
	r[1] = p2 * a[2] + p1 * a[3];

	return	+ l2 * r[0]
			+ l1 * r[1];
}

/** global pressure and temperature
 */
GPTVals gpt2(
	const vector<GptGrid>&		gptg,		///< gpt grid information
	double						mjd,		///< modified julian date
	double						lat,		///< ellipsoidal lat (rad)
	double						lon,		///< ellipsoidal lon (rad)
	double						hell)		///< ellipsoidal height (m)
{
	/* change reference epoch to 1/1/2000 */
	double mjd1 = mjd - MJD_j2000;

	/* factors for amplitudes */
	double cosfy = cos(mjd1 / 365.25 * 2 * PI);
	double coshy = cos(mjd1 / 365.25 * 4 * PI);
	double sinfy = sin(mjd1 / 365.25 * 2 * PI);
	double sinhy = sin(mjd1 / 365.25 * 4 * PI);

	/* positive longitude in degrees */
	double plon;
	if (lon < 0)	plon = (lon+2*PI)	* 180 / PI;
	else        	plon = lon			* 180 / PI;

	/* transform to polar distance in degrees */
	double pdist = (-lat + PI/2) * 180 / PI;

	/* find the index of the nearest point */
	int ipod = floor((pdist	+ 5) / 5);
	int ilon = floor((plon	+ 5) / 5);

	/* normalized (to one) differences */
	double dpod = (pdist	- (ipod * 5 - 2.5)) / 5;
	double dlon = (plon		- (ilon * 5 - 2.5)) / 5;

	if (ipod == 37)
		ipod = 36;

	int index[4] = {};
	index[0] = (ipod - 1) * 72 + ilon;

	/* near the pole: nearest neighbour interpolation, otherwise, bilinear */
	int bl=0;
	if 	(  pdist > 2.5
		&& pdist < 177.5)
	{
		bl=1;
	}

	GPTVals gptVals;

	if (bl == 0)
	{
		/* near the pole */
		int i = index[0] - 1;

		auto& gptGridPoint = gptg[i];

		gptVals.undulation = gptGridPoint.undu;

		double hgt = hell - gptVals.undulation;

		/* pressure, temperature at the height of grid */
		double t0	= coef(gptGridPoint.temp,	cosfy, sinfy, coshy, sinhy);
		double p0	= coef(gptGridPoint.pres,	cosfy, sinfy, coshy, sinhy);
		double q0	= coef(gptGridPoint.humid,	cosfy, sinfy, coshy, sinhy);
		double dt0	= coef(gptGridPoint.tlaps,	cosfy, sinfy, coshy, sinhy);

		double con = GRAVITY * MOLARDRY / (UGAS * t0 * (1 + 0.6077 * q0));

		/* pressure in hPa */
		gptVals.pressure	= (p0 * exp(-con * (hgt - gptGridPoint.hgt))) / 100;

		/* temperature at station height in celsius */
		gptVals.temperature	= t0 + dt0 * (hgt - gptGridPoint.hgt) - ZEROC;
		gptVals.deltaT		= dt0 * 1000;

		/* water vapour pressure in hPa */
		gptVals.waterVp		= (q0 * gptVals.pressure) / (0.622 + 0.378 * q0);

		/* dry and wet coefficients */
		gptVals.hydroCoef	= coef(gptGridPoint.ah, cosfy, sinfy, coshy, sinhy);
		gptVals.wetCoef		= coef(gptGridPoint.aw, cosfy, sinfy, coshy, sinhy);
	}
	else
	{
		double t[4];
		double p[4];
		double q[4];
		double dt[4];
		double ah[4];
		double aw[4];
		double undu[4];

		double ipod1 = ipod + sign(dpod);
		double ilon1 = ilon + sign(dlon);

		if (ilon1 == 73) 		ilon1 = 1;
		if (ilon1 == 0) 		ilon1 = 72;

		index[0] = index[0] - 1;                /* starting from 0 */
		index[1] = (ipod1	- 1)	* 72 + ilon	 - 1;
		index[2] = (ipod	- 1)	* 72 + ilon1 - 1;
		index[3] = (ipod1	- 1)	* 72 + ilon1 - 1;

		for (int k = 0; k < 4; k++)
		{
			auto& gptGridPointK = gptg[index[k]];

			undu[k] = gptGridPointK.undu;

			double hgt = hell - undu[k];

			/* pressure, temperature at the height of the grid */
			double t0		= coef(gptGridPointK.temp,	cosfy, sinfy, coshy, sinhy);
			double p0		= coef(gptGridPointK.pres,	cosfy, sinfy, coshy, sinhy);
			q[k]			= coef(gptGridPointK.humid,	cosfy, sinfy, coshy, sinhy);
			dt[k]			= coef(gptGridPointK.tlaps,	cosfy, sinfy, coshy, sinhy);

			t[k] = t0 + dt[k] * (hgt - gptGridPointK.hgt) - ZEROC;
			double con = GRAVITY * MOLARDRY / (UGAS * t0 * (1 + 0.6077 * q[k]));

			p[k]	= (p0 * exp(-con * (hgt - gptGridPointK.hgt))) / 100;

			ah[k]			= coef(gptGridPointK.ah,	cosfy, sinfy, coshy, sinhy);
			aw[k]			= coef(gptGridPointK.aw,	cosfy, sinfy, coshy, sinhy);
		}
		double dnpod1 = fabs(dpod);
		double dnpod2 = 1 - dnpod1;
		double dnlon1 = fabs(dlon);
		double dnlon2 = 1 - dnlon1;

		gptVals.pressure	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, p);
		gptVals.temperature	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, t);
		gptVals.deltaT		= coefr(dnpod1, dnpod2, dnlon1, dnlon2, dt) * 1000;

		/* humidity */
		double tp			= coefr(dnpod1, dnpod2, dnlon1, dnlon2, q);
		gptVals.waterVp	= tp * gptVals.pressure / (0.622 + 0.378 * tp);

		gptVals.hydroCoef	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, ah);
		gptVals.wetCoef		= coefr(dnpod1, dnpod2, dnlon1, dnlon2, aw);
		gptVals.undulation	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, undu);
	}

	return gptVals;
}

/** Troposphere zenith hydrastatic delay and mapping function.
 * gpt2 is used to get pressure, temperature, water vapor pressure and mapping function coefficients and then vmf1 is used to derive dry and wet mapping function.
 */
double tropGPT2(
	Trace&		trace,
	GTime		time,
	VectorPos&	pos,
	double		el,
	double&		dryZTD,
	double&		dryMap,
	double&		wetZTD,
	double&		wetMap,
	double&		var)
{
	var = -1;
	MjDateTT mjd_= time;
	double   mjd = mjd_.to_double();

	/* standard atmosphere */
	double lat = pos.lat();
	double lon = pos.lon();
	double hgt = pos.hgt();

	/* pressure, temperature, water vapor at station height */
	double pres	= 1013.25 * pow((1 - 0.0000226 * hgt), 5.225);
	double tp	= 15 - 6.5E-3 * hgt + ZEROC;
	double ew	= 0.5 / 100 * exp(	- 37.2465
									+ 0.213166		* tp
									- 0.000256908	* tp * tp);
	double gm	= 1 - 0.00266 * cos(2 * lat) - 0.00028 * hgt / 1E3;

	if (globalGPT2GridsReady == false)
	{
		return 0;
	}

	/* use GPT2 model */

	/* get pressure and mapping coefficients from gpts */
	GPTVals gptVals = gpt2(globalGPT2Grids, mjd, lat, lon, hgt);

	pres	= gptVals.pressure;
	tp		= gptVals.temperature + ZEROC;    /* celcius to kelvin */
	ew		= gptVals.waterVp;

	/* get mapping function */
	vmf1(gptVals.hydroCoef, gptVals.wetCoef, mjd, lat, hgt, el, dryMap, wetMap);

	/* zenith wet delay (m) */
	dryZTD = 0.002277 * pres / gm;
	wetZTD = 0.002277 * (1255 / tp + 0.05) * ew * (1 / gm);

	var = 0;
	double delay	= dryMap * dryZTD
					+ wetMap * wetZTD;

	return delay;
}

