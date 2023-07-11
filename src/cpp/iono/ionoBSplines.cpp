
#include "ionoModel.hpp"
#include "observations.hpp"
#include "common.hpp"
#include "acsConfig.hpp"

struct Bsp_Basis
{
	int hind;						/* layer number */
	double latit;					/* latitude */
	double longi;					/* longitude */
};

map<int, Bsp_Basis>  Bsp_Basis_list;

static double BSPLINE_LATCEN = 0;
static double BSPLINE_LONCEN = 0;
static double BSPLINE_LATWID = 0;
static double BSPLINE_LONWID = 0;
static double BSPLINE_LATINT = 0;
static double BSPLINE_LONINT = 0;


/** Initializes grid map model
	The following configursation parameters are used
	-  acsConfig.ionFilterOpts.lat_center:    latitude of map centre
	-  acsConfig.ionFilterOpts.lon_center:    longitude of map centre
	-  acsConfig.ionFilterOpts.lat_width:	  latitude width of maps
	-  acsConfig.ionFilterOpts.lon_width:	  longitude width of maps
	-  acsConfig.ionFilterOpts.lat_res:		  latitude resolution of gridmap
	-  acsConfig.ionFilterOpts.lon_res:		  longitude resolution of gridmap
	-  acsConfig.ionFilterOpts.layer_heights: Ionosphere layer Heights
----------------------------------------------------------------------------*/
int configIonModelBsplin()
{
	if	(  acsConfig.ionexGrid.lat_width > 180
		|| acsConfig.ionexGrid.lat_width < 0
		|| acsConfig.ionexGrid.lon_width > 360
		|| acsConfig.ionexGrid.lon_width < 0)
	{
		std::cout << "Wrongly sized gridmaps revise lat and lon width parameters...";
		return 0;
	}

	BSPLINE_LATCEN = acsConfig.ionexGrid.lat_center	* D2R;
	BSPLINE_LONCEN = acsConfig.ionexGrid.lon_center	* D2R;
	BSPLINE_LATINT = acsConfig.ionexGrid.lat_res	* D2R;
	BSPLINE_LONINT = acsConfig.ionexGrid.lon_res	* D2R;

	int latnum = (int)(acsConfig.ionexGrid.lat_width / acsConfig.ionexGrid.lat_res) + 1;
	int lonnum = (int)(acsConfig.ionexGrid.lon_width / acsConfig.ionexGrid.lon_res) + 1;

	BSPLINE_LATWID = BSPLINE_LATINT * (latnum - 1) / 2;
	BSPLINE_LONWID = BSPLINE_LONINT * (lonnum - 1) / 2;

	double latmin = BSPLINE_LATCEN - BSPLINE_LATWID;

	if ((latmin < -PI / 2) || ((BSPLINE_LATCEN + BSPLINE_LATWID) > PI / 2))
	{
		std::cout << "Gridmap model does not work on polar regions, select other mapping methods";
		return 0;
	}

	double lonmin = BSPLINE_LONCEN - BSPLINE_LONWID;

	Bsp_Basis basis;
	int ind = 0;

	for (int lay = 0; lay < acsConfig.ionModelOpts.layer_heights.size(); lay++)
	{
		basis.hind = lay;

		for (int lat = 0; lat < latnum; lat++)
		{
			basis.latit = latmin + lat * BSPLINE_LATINT;

			for (int lon = 0; lon < lonnum; lon++)
			{
				double lonmap = lonmin + lon * BSPLINE_LONINT;

				if (lonmap < -PI)	lonmap += 2 * PI;
				if (lonmap >  PI)	lonmap -= 2 * PI;

				basis.longi = lonmap;
				Bsp_Basis_list[ind++] = basis;
			}
		}
	}

	acsConfig.ionModelOpts.numBasis = ind;

	for (int j = 0; j < acsConfig.ionModelOpts.numBasis; j++)
	{
		Bsp_Basis& basis2 = Bsp_Basis_list[j];
// 		fprintf(fp_iondebug, "GRD_BASIS %3d %2d %8.4f %8.4f ", j, basis2.hind, basis.latit * R2D, basis.longi * R2D);
// 		fprintf(fp_iondebug, "\n");
	}

	return ind;
}

/** checks if the Ionosphere Piercing Point falls in area of coverage
time:  		I 		time of observations (not used)
IPP: 			I 		Ionospheric piercing point to be updated
returns 1 if the IPP is within the area of coverage
-----------------------------------------------------
Author: Ken Harima @ RMIT 04 August 2020
-----------------------------------------------------*/
bool ippCheckBsplin(GTime time, VectorPos& Ion_pp)
{
	if (fabs(BSPLINE_LATCEN - Ion_pp[0]) > BSPLINE_LATWID) 
		return false;

	double londiff = BSPLINE_LONCEN - Ion_pp[1];

	if (londiff < -PI)	londiff += 2 * PI;
	if (londiff >  PI)	londiff -= 2 * PI;

	if (fabs(londiff) > BSPLINE_LONWID)
		return false;

	return true;
}

/** Evaluates B-splines basis functions
	int ind			I		Basis function number
	meas			I		Ionosphere measurement struct
		latIPP				- Latitude of Ionosphere Piercing Point
		lonIPP				- Longitude of Ionosphere Piercing Point
		angIPP				- Angular gain for Ionosphere Piercing Point
	bool slant		I		state to delay gain; false: state to VTEC gain

	BSPLINE_LATINT and BSPLINE_LONINT needs to be set before calling this function
----------------------------------------------------------------------------*/
double ionCoefBsplin(int ind, IonoObs& obs, bool slant) 
{
	if (ind >= Bsp_Basis_list.size()) 
		return 0;

	Bsp_Basis& basis = Bsp_Basis_list[ind];

	double latdiff = (obs.ippMap[basis.hind].lat - basis.latit) / BSPLINE_LATINT;

	if	(  latdiff <= -1 
		|| latdiff >= +1) 
	{
		return 0;
	}
	
	double londiff = (obs.ippMap[basis.hind].lon - basis.longi);

	if (londiff < -PI)	londiff += 2 * PI;
	if (londiff >  PI)	londiff -= 2 * PI;

	londiff /= BSPLINE_LATINT;

	if	(  londiff <= -1 
		|| londiff >= +1)
	{
		return 0;
	}
	
	double out = latdiff < 0 ? (1 + latdiff) : (1 - latdiff);

	if (londiff < 0)		out *= 1 + londiff;
	else					out *= 1 - londiff;

	if (slant)
	{
		out *= obs.ippMap[basis.hind].slantFactor * obs.stecToDelay;
	}

	return out;
}

/** Estimate Ionosphere VTEC using Ionospheric gridmaps
	Ion_pp				I		Ionosphere Piercing Point
	layer				I 		Layer number
	vari				O		variance of VTEC
returns: VETC at piercing point
*/
double ionVtecBsplin(
	GTime		time,
	VectorPos&	ionPP,
	int			layer,
	double&		vari,
	KFState&	kfState)
{
	vari = 0;
	
	if (ippCheckBsplin(time, ionPP) == false)
	{
		return 0;
	}

	double iono = 0;
	GObs tmpobs;
	tmpobs.ippMap[layer].lat			= ionPP.lat();
	tmpobs.ippMap[layer].lon			= ionPP.lon();
	tmpobs.ippMap[layer].slantFactor	= 1;

	for (int ind = 0; ind < acsConfig.ionModelOpts.numBasis; ind++)
	{
		Bsp_Basis& basis = Bsp_Basis_list[ind];

		if (basis.hind != layer) 
			continue;

		double coef = ionCoefBsplin(ind, tmpobs, false);

		KFKey keyC;
		keyC.type	= KF::IONOSPHERIC;
		keyC.num	= ind;

		double staval = 0;
		double stastd = 0;
		kfState.getKFValue(keyC, staval, &stastd);

		iono += 	coef * staval;
		vari += SQR(coef * stastd);
	}

	return iono;
}
