/*------------------------------------------------------------------------------
* trop.c : troposphere functions
*
*							CRCSI-GA ACS Project
*
* reference:
*			[1] Troposphere mapping functions for GPS and very long baseline
*               interferometry from European Centre for Medium-Range Weather
*               Forecasts operational analysis data, J. Geoph. Res.,
*               Boehm, et al, 2006
*           [2] Global Mapping Functions (GMF): A new empirical mapping function
*               based on numerical weather model data, Geoph. Res. Letters, Vol.
*               33, L07304, doi:10.1029/2005GL025545.
*           [3] vmf1.m file from http://ggosatm.hg.tuwien.ac.at/DELAY/SOURCE/
*           [4] gpt2 file from http://ggosatm.hg.tuwien.ac.at/DELAY/SOURCE/
*           [5] gmf.f file from http://ggosatm.hg.tuwien.ac.at/DELAY/SOURCE/
*-----------------------------------------------------------------------------*/

#include <cstring>
#include <math.h>

#include "trop.h"
#include "constants.hpp"

#define     MIN(x,y)    (x<y?x:y)           /* minimum of two numbers */

const double ah_mean[55]={      /* coefficients for GMF */
	+1.2517E+02, +8.503E-01, +6.936E-02, -6.760E+00, +1.771E-01,
	+1.130E-02, +5.963E-01, +1.808E-02, +2.801E-03, -1.414E-03,
	-1.212E+00, +9.300E-02, +3.683E-03, +1.095E-03, +4.671E-05,
	+3.959E-01, -3.867E-02, +5.413E-03, -5.289E-04, +3.229E-04,
	+2.067E-05, +3.000E-01, +2.031E-02, +5.900E-03, +4.573E-04,
	-7.619E-05, +2.327E-06, +3.845E-06, +1.182E-01, +1.158E-02,
	+5.445E-03, +6.219E-05, +4.204E-06, -2.093E-06, +1.540E-07,
	-4.280E-08, -4.751E-01, -3.490E-02, +1.758E-03, +4.019E-04,
	-2.799E-06, -1.287E-06, +5.468E-07, +7.580E-08, -6.300E-09,
	-1.160E-01, +8.301E-03, +8.771E-04, +9.955E-05, -1.718E-06,
	-2.012E-06, +1.170E-08, +1.790E-08, -1.300E-09, +1.000E-10};
const double bh_mean[55]={
	+0.000E+00, +0.000E+00, +3.249E-02, +0.000E+00, +3.324E-02,
	+1.850E-02, +0.000E+00, -1.115E-01, +2.519E-02, +4.923E-03,
	+0.000E+00, +2.737E-02, +1.595E-02, -7.332E-04, +1.933E-04,
	+0.000E+00, -4.796E-02, +6.381E-03, -1.599E-04, -3.685E-04,
	+1.815E-05, +0.000E+00, +7.033E-02, +2.426E-03, -1.111E-03,
	-1.357E-04, -7.828E-06, +2.547E-06, +0.000E+00, +5.779E-03,
	+3.133E-03, -5.312E-04, -2.028E-05, +2.323E-07, -9.100E-08,
	-1.650E-08, +0.000E+00, +3.688E-02, -8.638E-04, -8.514E-05,
	-2.828E-05, +5.403E-07, +4.390E-07, +1.350E-08, +1.800E-09,
	+0.000E+00, -2.736E-02, -2.977E-04, +8.113E-05, +2.329E-07,
	+8.451E-07, +4.490E-08, -8.100E-09, -1.500E-09, +2.000E-10};
const double ah_amp[55]={
	-2.738E-01, -2.837E+00, +1.298E-02, -3.588E-01, +2.413E-02,
	+3.427E-02, -7.624E-01, +7.272E-02, +2.160E-02, -3.385E-03,
	+4.424E-01, +3.722E-02, +2.195E-02, -1.503E-03, +2.426E-04,
	+3.013E-01, +5.762E-02, +1.019E-02, -4.476E-04, +6.790E-05,
	+3.227E-05, +3.123E-01, -3.535E-02, +4.840E-03, +3.025E-06,
	-4.363E-05, +2.854E-07, -1.286E-06, -6.725E-01, -3.730E-02,
	+8.964E-04, +1.399E-04, -3.990E-06, +7.431E-06, -2.796E-07,
	-1.601E-07, +4.068E-02, -1.352E-02, +7.282E-04, +9.594E-05,
	+2.070E-06, -9.620E-08, -2.742E-07, -6.370E-08, -6.300E-09,
	+8.625E-02, -5.971E-03, +4.705E-04, +2.335E-05, +4.226E-06,
	+2.475E-07, -8.850E-08, -3.600E-08, -2.900E-09, +0.000E+00};
const double bh_amp[55]={
	+0.000E+00, +0.000E+00, -1.136E-01, +0.000E+00, -1.868E-01,
	-1.399E-02, +0.000E+00, -1.043E-01, +1.175E-02, -2.240E-03,
	+0.000E+00, -3.222E-02, +1.333E-02, -2.647E-03, -2.316E-05,
	+0.000E+00, +5.339E-02, +1.107E-02, -3.116E-03, -1.079E-04,
	-1.299E-05, +0.000E+00, +4.861E-03, +8.891E-03, -6.448E-04,
	-1.279E-05, +6.358E-06, -1.417E-07, +0.000E+00, +3.041E-02,
	+1.150E-03, -8.743E-04, -2.781E-05, +6.367E-07, -1.140E-08,
	-4.200E-08, +0.000E+00, -2.982E-02, -3.000E-03, +1.394E-05,
	-3.290E-05, -1.705E-07, +7.440E-08, +2.720E-08, -6.600E-09,
	+0.000E+00, +1.236E-02, -9.981E-04, -3.792E-05, -1.355E-05,
	+1.162E-06, -1.789E-07, +1.470E-08, -2.400E-09, -4.000E-10};
const double aw_mean[55]={
	+5.640E+01, +1.555E+00, -1.011E+00, -3.975E+00, +3.171E-02,
	+1.065E-01, +6.175E-01, +1.376E-01, +4.229E-02, +3.028E-03,
	+1.688E+00, -1.692E-01, +5.478E-02, +2.473E-02, +6.059E-04,
	+2.278E+00, +6.614E-03, -3.505E-04, -6.697E-03, +8.402E-04,
	+7.033E-04, -3.236E+00, +2.184E-01, -4.611E-02, -1.613E-02,
	-1.604E-03, +5.420E-05, +7.922E-05, -2.711E-01, -4.406E-01,
	-3.376E-02, -2.801E-03, -4.090E-04, -2.056E-05, +6.894E-06,
	+2.317E-06, +1.941E+00, -2.562E-01, +1.598E-02, +5.449E-03,
	+3.544E-04, +1.148E-05, +7.503E-06, -5.667E-07, -3.660E-08,
	+8.683E-01, -5.931E-02, -1.864E-03, -1.277E-04, +2.029E-04,
	+1.269E-05, +1.629E-06, +9.660E-08, -1.015E-07, -5.000E-10};
const double bw_mean[55]={
	+0.000E+00, +0.000E+00, +2.592E-01, +0.000E+00, +2.974E-02,
	-5.471E-01, +0.000E+00, -5.926E-01, -1.030E-01, -1.567E-02,
	+0.000E+00, +1.710E-01, +9.025E-02, +2.689E-02, +2.243E-03,
	+0.000E+00, +3.439E-01, +2.402E-02, +5.410E-03, +1.601E-03,
	+9.669E-05, +0.000E+00, +9.502E-02, -3.063E-02, -1.055E-03,
	-1.067E-04, -1.130E-04, +2.124E-05, +0.000E+00, -3.129E-01,
	+8.463E-03, +2.253E-04, +7.413E-05, -9.376E-05, -1.606E-06,
	+2.060E-06, +0.000E+00, +2.739E-01, +1.167E-03, -2.246E-05,
	-1.287E-04, -2.438E-05, -7.561E-07, +1.158E-06, +4.950E-08,
	+0.000E+00, -1.344E-01, +5.342E-03, +3.775E-04, -6.756E-05,
	-1.686E-06, -1.184E-06, +2.768E-07, +2.730E-08, +5.700E-09};
const double aw_amp[55]={
	+1.023E-01, -2.695E+00, +3.417E-01, -1.405E-01, +3.175E-01,
	+2.116E-01, +3.536E+00, -1.505E-01, -1.660E-02, +2.967E-02,
	+3.819E-01, -1.695E-01, -7.444E-02, +7.409E-03, -6.262E-03,
	-1.836E+00, -1.759E-02, -6.256E-02, -2.371E-03, +7.947E-04,
	+1.501E-04, -8.603E-01, -1.360E-01, -3.629E-02, -3.706E-03,
	-2.976E-04, +1.857E-05, +3.021E-05, +2.248E+00, -1.178E-01,
	+1.255E-02, +1.134E-03, -2.161E-04, -5.817E-06, +8.836E-07,
	-1.769E-07, +7.313E-01, -1.188E-01, +1.145E-02, +1.011E-03,
	+1.083E-04, +2.570E-06, -2.140E-06, -5.710E-08, +2.000E-08,
	-1.632E+00, -6.948E-03, -3.893E-03, +8.592E-04, +7.577E-05,
	+4.539E-06, -3.852E-07, -2.213E-07, -1.370E-08, +5.800E-09};
const double bw_amp[55]={
	+0.000E+00, +0.000E+00, -8.865E-02, +0.000E+00, -4.309E-01,
	+6.340E-02, +0.000E+00, +1.162E-01, +6.176E-02, -4.234E-03,
	+0.000E+00, +2.530E-01, +4.017E-02, -6.204E-03, +4.977E-03,
	+0.000E+00, -1.737E-01, -5.638E-03, +1.488E-04, +4.857E-04,
	-1.809E-04, +0.000E+00, -1.514E-01, -1.685E-02, +5.333E-03,
	-7.611E-05, +2.394E-05, +8.195E-06, +0.000E+00, +9.326E-02,
	-1.275E-02, -3.071E-04, +5.374E-05, -3.391E-05, -7.436E-06,
	+6.747E-07, +0.000E+00, -8.637E-02, -3.807E-03, -6.833E-04,
	-3.861E-05, -2.268E-05, +1.454E-06, +3.860E-07, -1.068E-07,
	+0.000E+00, -2.658E-02, -1.947E-03, +7.131E-04, -3.506E-05,
	+1.885E-07, +5.792E-07, +3.990E-08, +2.000E-08, -5.700E-09};

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

/* vienna mapping function -----------------------------------------------------
* args     :       const FILE *fp          I       VMF1 coefficients file
*                  const char *staid       I       station id
*                  double a[7]             I       dry and wet coefficient
*                                                  a[0]=dry
*                                                  a[1]=wet
*                                                  a[2]=zhd
*                                                  a[3]=zwd
*                                                  a[4]=p
*                                                  a[5]=t
*                                                  a[6]=wvp
*
* return   :       0-fail, 1-successful
*
* note     :       format --> http://ggosatm.hg.tuwien.ac.at/DELAY/SITE/header4
* ---------------------------------------------------------------------------*/
int readvmf(const char *file, const char *staid, double a[7])
{
	char buff[128];
	FILE *fp;
	double tmp;
	int info=0;

	fp=fopen(file,"r");
	if (fp == nullptr) 
	{
		fprintf(stdout,"Warning: no VMF1 coefficients file provided\n");
		return info;
	}

	while (fgets(buff,128,fp)!=nullptr) 
	{
		if (strstr(buff,staid)) 
		{
			/* dry a, wet a, zhd, zwd, pressure, temperature, water vapor */
			sscanf(buff+18,"%lf %lf %lf %lf %lf %lf %lf %lf",
				&a[0],&a[1],&a[2],&a[3],&tmp,&a[4],&a[5],&a[6]);

			info=1;
			break;
		}
	}

	if (info==0) 
		fprintf(stdout,"Warning: no station (%s) found in trop.c\n", staid);
	fclose(fp);

	return info;
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
	double			zd,		///< zenith distance (rad)
	int				id,		///< 0-no height, 1-with height
	double			mf[2])	///< mapping function (dry, wet)
{
	double doy = mjd - 44239 + 1 - 28;

	double bh	= 0.0029;
	double c0h	= 0.062;
	double bw	= 0.00146;
	double cw	= 0.04391;

	/* height correction */
	double aht = 2.53e-5;
	double bht = 5.49e-3;
	double cht = 1.14e-3;
	double hkm = hgt / 1000;

	double phh;
	double c11h;
	double c10h;
	if (lat < 0)	{	phh = PI;	c11h = 0.007;	c10h = 0.002;	}	/* southern hemisphere */
	else			{	phh = 0;	c11h = 0.005;	c10h = 0.001;	}	/* northern hemisphere */

	/* hydrostatic mapping function */
	double ch		= c0h + ((cos(doy / 365.25 * 2*PI + phh) + 1) * c11h / 2 + c10h) * (1 - cos(lat));
	double sine		= sin(PI/2 - zd);
	double beta		= bh / (sine + ch);
	double gamma	= ah / (sine + beta);
	double topcon	= (1 + ah / (1 + bh / (1 + ch)));
	
	mf[0] = topcon / (sine + gamma);

	/* height correction */
	if (id == 1)
	{
		beta	= bht / (sine + cht);
		gamma	= aht / (sine + beta);
		topcon	= (1 + aht / (1 + bht / (1 + cht)));
		
		mf[0] += (1 / sine - topcon / (sine + gamma)) * hkm;
	}

	/* wet mapping function */
	beta	= bw / (sine + cw);
	gamma	= aw / (sine + beta);
	topcon	= (1 + aw / (1 + bw / (1 + cw)));
	
	mf[1] = topcon / (sine + gamma);
}

/** read GPT grid file
 */
int readgrid(
	string		file,	///< vmf1 coefficients filename
	gptgrid_t*	gptg)	///< gpt grid information
{
	char buff[256];
	int i = 0;

	FILE* fp = fopen(file.c_str(), "r");
	if (fp == nullptr)
	{
		gptg->ind = 0;

		fprintf(stdout,"Warning: no gpt grid file provided\n");
		return 0;
	}

	gptg->ind = 1;
	
	while (fgets(buff, 256, fp))
	{
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

		/* assign lat lon */
		gptg->lat[i] = val[0];
		gptg->lon[i] = val[1];

		/* assign pres, temp, humid, tlaps, ah, aw */
		for (int k=0;k<5;k++)
		{
			gptg->pres	[i][k]	= val[k+2];          /* pressure */
			gptg->temp	[i][k]	= val[k+7];          /* temperature */
			gptg->humid	[i][k]	= val[k+12]/1000;   /* humidity */
			gptg->tlaps	[i][k]	= val[k+17]/1000;   /* temperature elapse rate */
			gptg->ah	[i][k]	= val[k+24]/1000;      /* ah */
			gptg->aw	[i][k]	= val[k+29]/1000;      /* aw */
		}

		/* assign undulation and height */
		gptg->undu[i]	= val[22];
		gptg->hgt[i]	= val[23];

		i++;
	}

	fclose(fp);

	return 1;
}

/** coefficients multiplication
 */
double coef(
	const double a[5],
	double cosfy,
	double sinfy,
	double coshy,
	double sinhy)
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

/* global pressure and temperature ---------------------------------------------
* args     :       const gptgrid_t *gptg   I       gpt grid information
*                  double mjd              I       modified julian date
*                  double lat              I       ellipsoidal lat (rad)
*                  double lon              I       ellipsoidal lon (rad)
*                  double hgt              I       ellipsoidal height (m)
*                  int it                  I       1: no time variation
*                                                  0: with time variation
*                  double gptval[7]        O       values of
*                                                  [0] pressure (hPa)
*                                                  [1] temperature (celsius)
*                                                  [2] delta temp (deg/km)
*                                                  [3] water vp (hPa)
*                                                  [4] hydrostatic coef at 0m
*                                                  [5] wet coef
*                                                  [6] undulation (m)
* ---------------------------------------------------------------------------*/
void gpt2(
	const gptgrid_t&	gptg,
	double				mjd,
	double				lat,
	double				lon,
	double				hell,
	int					it,
	double				gptval[7])
{
	/* change reference epoch to 1/1/2000 */
	double mjd1 = mjd - 51544.5;

	/* factors for amplitudes */
	double cosfy = 0;
	double coshy = 0;
	double sinfy = 0;
	double sinhy = 0;
	if (it != 1)
	{
		cosfy = cos(mjd1 / 365.25 * 2 * PI);
		coshy = cos(mjd1 / 365.25 * 4 * PI);
		sinfy = sin(mjd1 / 365.25 * 2 * PI);
		sinhy = sin(mjd1 / 365.25 * 4 * PI);
	}

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

	int index[4];
	index[0] = (ipod - 1) * 72 + ilon;

	/* near the pole: nearest neighbour interpolation, otherwise, bilinear */
	int bl=0;
	if 	(  pdist > 2.5
		&& pdist < 177.5)
	{
		bl=1;
	}

	if (bl == 0)
	{
		/* near the pole */
		int i = index[0] - 1;

		gptval[6]	= gptg.undu[i];
		double hgt	= hell - gptval[6];

		/* pressure, temperature at the height of grid */
		double t0	= coef(gptg.temp[i],	cosfy, sinfy, coshy, sinhy);
		double p0	= coef(gptg.pres[i],	cosfy, sinfy, coshy, sinhy);
		double q0	= coef(gptg.humid[i],	cosfy, sinfy, coshy, sinhy);
		double dt0	= coef(gptg.tlaps[i],	cosfy, sinfy, coshy, sinhy);

		/* temperature at station height in celsius */
		gptval[1]= t0+dt0*(hgt-gptg.hgt[i]) - ZEROC;
		gptval[2]= dt0 * 1000;

		double con=GRAVITY*MOLARDRY/(UGAS*t0*(1+0.6077*q0));

		/* pressure in hPa */
		gptval[0]=(p0*exp(-con*(hgt-gptg.hgt[i])))/100;

		/* water vapour pressure in hPa */
		gptval[3]=(q0*gptval[0])/(0.622+0.378*q0);

		/* dry and wet coefficients */
		gptval[4] = coef(gptg.ah[i],cosfy,sinfy,coshy,sinhy);
		gptval[5] = coef(gptg.aw[i],cosfy,sinfy,coshy,sinhy);
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
			undu[k] = gptg.undu[index[k]];
			double hgt = hell - undu[k];

			/* pressure, temperature at the height of the grid */
			double t0		= coef(gptg.temp	[index[k]], cosfy, sinfy, coshy, sinhy);
			double p0		= coef(gptg.pres	[index[k]], cosfy, sinfy, coshy, sinhy);
			q[k]			= coef(gptg.humid	[index[k]], cosfy, sinfy, coshy, sinhy);
			dt[k]			= coef(gptg.tlaps	[index[k]], cosfy, sinfy, coshy, sinhy);

			t[k] = t0 + dt[k] * (hgt - gptg.hgt[index[k]]) - ZEROC;
			double con = GRAVITY * MOLARDRY / (UGAS * t0 * (1 + 0.6077 * q[k]));

			p[k]	= (p0*exp(-con*(hgt-gptg.hgt[index[k]])))/100;
			
			ah[k]			= coef(gptg.ah		[index[k]], cosfy, sinfy, coshy, sinhy);
			aw[k]			= coef(gptg.aw		[index[k]], cosfy, sinfy, coshy, sinhy);
		}
		double dnpod1 = fabs(dpod);
		double dnpod2 = 1 - dnpod1;
		double dnlon1 = fabs(dlon);
		double dnlon2 = 1 - dnlon1;


		gptval[0]	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, p); /* pressure */
		gptval[1]	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, t);  /* temperature */
		gptval[2]	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, dt) * 1000;/* temperature elapse per km */

		/* humidity */
		double tp	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, q);
		gptval[3]	= tp * gptval[0] / (0.622 + 0.378 * tp);


		gptval[4]	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, ah);   /* hydrostatic coefficient */
		gptval[5]	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, aw);   /* wet coefficient */
		gptval[6]	= coefr(dnpod1, dnpod2, dnlon1, dnlon2, undu);   /* undulation */
	}
}

/* Global mapping functions (GMF).
 * **** this function needs to be verifed ****
 */
void gmf(
	const double	mjd,	///< modified julian date
	const double	lat,	///< ellipsoidal lat (rad)
	const double	lon,	///< ellipsoidal lon (rad)
	const double	hgt,	///< ellipsoidal height (m)
	const double	zd,		///< zenith distance (rad)
	double			mf[2])	///< mapping function (dry, wet)
{
	const int n = 9;
	const int m = 9;
	

	/* reference day 28, Jan */
	double doy = mjd - 44239.0 + 1 - 28;

	double t = sin(lat);

	/* determine factorial n! */
	double dfac[20];
	dfac[0] = 1;
	for (int i = 0; i < 2 * n + 1; i++)
		dfac[i + 1] = dfac[i] * i;

	/* determine Legendre functions, NOTE, to be verified for the arrays */
	double P[10][10];
	for (int i = 0; i <= n; i++)
	{
		for (int j = 0; j <= MIN(i, m) + 1; j++)
		{
			int ir = (int)((i - j) / 2);
			double sum = 0;
			for (int k = 0; k < ir + 1; k++)
			{
				sum += pow(-1, k) * dfac[2 * i - 2 * k] / dfac[k] / dfac[i - k] / dfac[i - j - 2 * k] * pow(t, i - j - 2 * k);
			}
			P[i][j] = pow(0.5, i) * sqrt(pow((1 - pow(t, 2)), j)) * sum;
		}
	}

	/* spherical harmonics */
	int i = 0;
	double ap[55];
	double bp[55];
	for (int n = 0; n <= 9; n++)
	{
		for (int m = 0; m <= n; m++)
		{
			i = i + 1;
			ap[i - 1] = P[n][m] * cos(m * lon);
			bp[i - 1] = P[n][m] * sin(m * lon);
		}
	}

	/* hydrostatic */
	double bh = 0.0029;
	double c0h = 0.062;
	double phh;
	double c11h;
	double c10h;
	if (lat < 0)	{	phh = PI;	c11h = 0.007;	c10h = 0.002;	} /* southern hemisphere */	
	else			{	phh = 0;	c11h = 0.005;	c10h = 0.001;	} /* northern hemisphere */
		
	double ch = c0h + ((cos(doy / 365.25 * 2*PI + phh) + 1) * c11h / 2 + c10h) * (1 - cos(lat));

	double ahm = 0;
	double aha = 0;
	for (int i = 0; i < 55; i++)
	{
		ahm += (ah_mean[i]	* ap[i] + bh_mean[i]	* bp[i]) * 1E-5;
		aha += (ah_amp[i]	* ap[i] + bh_amp[i]		* bp[i]) * 1E-5;
	}
	double ah = ahm + aha * cos(doy / 365.35 * 2*PI);

	double sine		= sin(PI/2 - zd);
	double beta		= bh / (sine + ch);
	double gamma	= ah / (sine + beta);
	double topcon	= (1 + ah / (1 + bh / (1 + ch)));
	
	mf[0] = topcon / (sine + gamma);

	/* height correction for dry mapping function from Niell (1996) */
	double a_ht = 2.53E-5;
	double b_ht = 5.49E-3;
	double c_ht = 1.14E-3;
	double hs_km = hgt / 1000.0;

	beta	= b_ht / (sine + c_ht);
	gamma	= a_ht / (sine + beta);
	topcon	= (1 + a_ht / (1 + b_ht / (1 + c_ht)));
	
	double ht_corr = (1 / sine - topcon / (sine + gamma)) * hs_km;
	
	mf[0] = mf[0] + ht_corr;

	/* wet mapping function */
	double bw	= 0.00146; 
	double cw	= 0.04391; 
	double awm	= 0; 
	double awa	= 0;

	for (int i = 0; i < 55; i++)
	{
		awm += (aw_mean[i]	* ap[i] + bw_mean[i]	* bp[i]) * 1E-5;
		awa += (aw_amp[i]	* ap[i] + bw_amp[i]		* bp[i]) * 1E-5;
	}
	double aw = awm + awa * cos(doy / 365.25 * 2*PI);

	beta	= bw / (sine + cw);
	gamma	= aw / (sine + beta);
	topcon	= (1 + aw / (1 + bw / (1 + cw)));
	
	mf[1] = topcon / (sine + gamma);
}

/** emipirical troposphere mapping function
 */
double tropemp(
	const double	pres,	///< pressure (millibar)
	const double	temp,	///< temperature (kelvin)
	const double	e,		///< water vp (millibar)
	const double	lat,	///< latitude (rad)
	const double	hgt,	///< height (m)
	const double	el,	    ///< ele angle (rad)
	double*			map)	///< wet mapping function
{
	double a[2];
	double b[2];
	double c[2];

	/* parameters of dry mapping function */
	a[0]	= 0.1237 * pow(10, -2) 
			+ 0.1316 * pow(10, -6) * (pres - 1000) 
			+ 0.8057 * pow(10, -5) * sqrt(e)
			+ 0.1378 * pow(10, -5) * (temp - 288.15);
			
	b[0]	= 0.3333 * pow(10, -2) 
			+ 0.1946 * pow(10, -6) * (pres - 1000) 
			+ 0.1747 * pow(10, -6) * sqrt(e)
			+ 0.1040 * pow(10, -6) * (temp - 288.15);
			
	c[0] = 0.078;

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

	double z = PI/2 - el;

	/* mapping function */
	if (map)
	for (int i = 0; i < 2; i++)
	{
		map[i] =	(1		+ a[i] / 
									(1		+ b[i] /
													(1		+ c[i]))) /
					(cos(z)	+ a[i] / 
									(cos(z)	+ b[i] / 
													(cos(z)	+ c[i])));
	}
	
	double zhd = 0.002277 * (pres / (1 - 0.00266 * cos(2 * lat) - 0.00028 * hgt / 1E3));
	
	return zhd;
}

/** Troposphere zenith hydrastatic delay and mapping function.
 * gpt2 is used to get pressure, temperature, water vapor pressure and mapping function coefficients and then vmf1 is used to derive dry and wet mapping function.
 */
double tropztd(
	const gptgrid_t&	gptg,	///< gpt grid information
	double				pos[3],	///< lat,lon,hgt (rad,rad,m)
	double				mjd,	///< modified julian date
	double				el,		///< elevation (rad)
	int					it,		///< 1: no time variation, 0: with time variation
	double				mf[2],	///< mapping function (dry, wet)
	double&				zwd)	///< zenith wet delay
{
	double a[7] = {};

	/* standard atmosphere */
	double lat = pos[0];
	double lon = pos[1];
	double hgt = pos[2];
	
	double zd = PI/2 - el;

	/* pressure, temperature, water vapor at station height */
	double pres	= 1013.25 * pow((1 - 0.00000226 * hgt), 5.225);
	double tp	= 15 - 6.5E-3 * hgt + ZEROC;
	double ew	= 0.5 / 100 * exp(	- 37.2465 
									+ 0.213166		* tp 
									- 0.000256908	* tp * tp);
	double gm	= 1 - 0.00266 * cos(2 * lat) - 0.00028 * hgt / 1E3;

	double zhd = 0;
	if (gptg.ind != 0)
	{
		/* use GPT2 model */
		
		/* get pressure and mapping coefficients from gpts */
		double gptval[7] = {};
		gpt2(gptg,mjd, lat, lon, hgt, it, gptval);
		
		pres	= gptval[0];
		tp		= gptval[1] + ZEROC;    /* celcius to kelvin */
		ew		= gptval[3];

		/* get mapping function */
		vmf1(gptval[4], gptval[5], mjd, lat, hgt, zd, 1, mf);

		/* zenith hydrostatic delay */
		zhd = 0.002277 * pres / gm;
	}
	else if (0 && readvmf(nullptr, nullptr, a))/* currently omitted */
	{
		vmf1(a[0], a[1], mjd, lat, hgt, zd, 0, mf);
	}
	else
	{
		/* use empirical mapping functions */
		
		/* zenith hydrostatic delay */
		zhd = tropemp(pres, tp, ew, lat, hgt, el, mf);
	}

	/* zenith wet delay (m) */
	zwd = 0.002277 * (1255 / tp + 0.05) * ew * (1 / gm);

	return zhd;
}

