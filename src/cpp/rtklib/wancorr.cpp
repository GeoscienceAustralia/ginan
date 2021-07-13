/*------------------------------------------------------------------------------
* reference:
*			[1] BeiDou satellite-induced code pseudorange vairations: diagnosis
*               and therapy, GPS Solutions, 2015
*-----------------------------------------------------------------------------*/

#include "wancorr.h"

#define             NIGSO           8       /* IGSO satellite number */
#define             NMEO            6       /* MEO satellite number */
#define             NELEV           10      /* interval */

/* BeiDou IGSO satellite PRNs, Table 1 in Ref [1] */
int igsosat[NIGSO] = {6, 7, 8, 9, 10, 15, 31, 32};

/* BeiDou MEO satellite PRNs, Table 1 in Ref [1] */
int meosat[NMEO] = {11, 12, 13, 14, 33, 34};

/* elevation range, fixed dimension, Table 1 in Ref [1] */
double elerange[NELEV] = {0, 10, 20, 30, 40, 50, 60, 70, 80, 90};

/* IGSO correction table, fixed dimension, Table 1 in Ref [1] */
double igsobias[3][NELEV] =
{
	{ -0.55, -0.40, -0.34, -0.23, -0.15, -0.04, 0.09, 0.19, 0.27, 0.35}, /* B1 (C2I) */
	{ -0.71, -0.36, -0.33, -0.19, -0.14, -0.03, 0.08, 0.17, 0.24, 0.33}, /* B2 (C7I) */
	{ -0.27, -0.23, -0.21, -0.15, -0.11, -0.04, 0.05, 0.14, 0.19, 0.32} /* B3 (C6I) */
};

/* MEO correction table, fixed dimension, Table 1 in Ref [1] */
double meobias[3][NELEV] =
{
	{ -0.47, -0.38, -0.32, -0.23, -0.11, 0.06, 0.34, 0.69, 0.97, 1.05}, /* B1 (C2I) */
	{ -0.40, -0.31, -0.26, -0.18, -0.06, 0.09, 0.28, 0.48, 0.64, 0.69}, /* B2 (C7I) */
	{ -0.22, -0.15, -0.13, -0.10, -0.04, 0.05, 0.14, 0.27, 0.36, 0.47} /* B3 (C6I) */
};

/* linear interpolation function -----------------------------------------------
* args :   double *bias        I       satellite bias table (freq dependent)
*          const double el     I       satellite elevation (deg)
*
* return : satellite-induced code bias (m)
*----------------------------------------------------------------------------*/
double interp(double* bias, const double el)
{
	int j = 0;

	for (int i = 0; i < NELEV - 1; i++)
	{
		if (el >= elerange[i] && el < elerange[i + 1])
		{
			j = i + 1;
			break;
		}
	}

	/* linear interpolate y=(y2-y1)/(x2-x1)*(x-x1)+y1 */
	double d1 = (bias[j] - bias[j - 1]);
	double d2 = (elerange[j] - elerange[j - 1]);
	double corr = d1 / d2 * (el - elerange[j - 1]) + bias[j - 1];

	return corr;
}
/* beidou satellite code induced bias correction -------------------------------
* args :   const int prn       I       beidou satellite prn
*          const double el     I       satellite elevation (deg)
*          const int freq      I       frequency number
*
* return : satellite-induced code bias (m)
*----------------------------------------------------------------------------*/
double wancorr(const int prn, const double el, const int freq)
{
	int id = 0;

	double* p1 = igsobias[0];
	double* p2 = meobias[0];

	/* find IGSO satellite */
	for (int i = 0; i < NIGSO; i++)
	{
		if (prn == igsosat[i])
		{
			id = 1;
			break;
		}
	}

	/* or find MEO satellite */
	if (id == 0)
	{
		for (int i = 0; i < NMEO; i++)
		{
			if (prn == meosat[i])
			{
				id = 2;
				break;
			}
		}
	}

	double corr = 0;
	/* IGSO satellite */
	if (id == 1)
	{
		corr = interp(p1 + (freq - 1) * NELEV, el); /* interpolate the bias */
	}
	/* MEO satellite */
	else if (id == 2)
	{
		corr = interp(p2 + (freq - 1) * NELEV, el); /* interpolate the bias */
	}

	return corr;
}

#if (0)
/* for unit testing */
int wancorrmain()
{
	int i, j, freq = 1;
	double el = 25, corr = 0.0;

	for (i = 1; i <= 35; i++)
	{
		for (j = 1; j < 4; j++)
		{
			corr = wancorr(i, el, j);
			fprintf(stdout, "sat=%3d freq=%3d %3.2f \n", i, j, corr);
		}
	}

	return 0;
}
#endif
