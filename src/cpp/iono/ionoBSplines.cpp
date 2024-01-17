
#include "ionoModel.hpp"
#include "observations.hpp"
#include "common.hpp"
#include "acsConfig.hpp"

struct BspBasis
{
	int ind;						/* layer number */
	double latDeg;					/* latitude */
	double lonDeg;					/* longitude */
};

map<int, BspBasis> bspBasisMap;

static double BSPLINE_LAT_CENTRE	= 0;
static double BSPLINE_LON_CENTRE	= 0;
static double BSPLINE_LAT_WIDTH		= 0;
static double BSPLINE_LON_WIDTH		= 0;
static double BSPLINE_LAT_INTERVAL	= 0;
static double BSPLINE_LON_INTERVAL	= 0;

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
int configIonModelBsplin(
	Trace& trace)
{
	bspBasisMap.clear();
	if	(  acsConfig.ionexGrid.lat_width > 180
		|| acsConfig.ionexGrid.lat_width < 0
		|| acsConfig.ionexGrid.lon_width > 360
		|| acsConfig.ionexGrid.lon_width < 0)
	{
		std::cout << "Wrongly sized gridmaps revise lat and lon width parameters...";
		return 0;
	}

	BSPLINE_LAT_CENTRE		= acsConfig.ionexGrid.lat_centre;
	BSPLINE_LON_CENTRE		= acsConfig.ionexGrid.lon_centre;
	BSPLINE_LAT_INTERVAL	= acsConfig.ionexGrid.lat_res;
	BSPLINE_LON_INTERVAL	= acsConfig.ionexGrid.lon_res;

	int latnum = (int)(acsConfig.ionexGrid.lat_width / acsConfig.ionexGrid.lat_res) + 1;
	int lonnum = (int)(acsConfig.ionexGrid.lon_width / acsConfig.ionexGrid.lon_res) + 1;

	BSPLINE_LAT_WIDTH = BSPLINE_LAT_INTERVAL * (latnum - 1) / 2;
	BSPLINE_LON_WIDTH = BSPLINE_LON_INTERVAL * (lonnum - 1) / 2;

	double latmin = BSPLINE_LAT_CENTRE - BSPLINE_LAT_WIDTH;
	double lonmin = BSPLINE_LON_CENTRE - BSPLINE_LON_WIDTH;

	if	( (latmin < -90)
		||((BSPLINE_LAT_CENTRE + BSPLINE_LAT_WIDTH) > 90))
	{
		std::cout << "Gridmap model does not work on polar regions, select other mapping methods";
		return 0;
	}


	BspBasis basis;
	int ind = 0;

	for (int layN = 0; layN < acsConfig.ionModelOpts.layer_heights.size();	layN++)
	for (int latN = 0; latN < latnum;										latN++)
	for (int lonN = 0; lonN < lonnum;										lonN++)
	{
		basis.ind		= layN;
		basis.latDeg	= latmin + latN * BSPLINE_LAT_INTERVAL;
		double lonmap	= lonmin + lonN * BSPLINE_LON_INTERVAL;

		if (lonmap < -180)		lonmap += 360;
		if (lonmap >  180)		lonmap -= 360;

		basis.lonDeg	= lonmap;

		bspBasisMap[ind] = basis;
		ind++;
	}

	acsConfig.ionModelOpts.numBasis = ind;

	tracepdeex(2,trace, "\nIONO_BASIS ind lay latitude longitude");

	for (auto& [j, basis2] : bspBasisMap)
		tracepdeex(2, trace, "\nIONO_BASIS %3d %3d %8.4f %8.4f ", j, basis.ind, basis.latDeg, basis.lonDeg);

	tracepdeex(2,trace, "\n");

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
	if (fabs(BSPLINE_LAT_CENTRE - Ion_pp.latDeg()) > BSPLINE_LAT_WIDTH)
		return false;

	double londiff = BSPLINE_LON_CENTRE - Ion_pp.lonDeg();

	if (londiff < -180)		londiff += 360;
	if (londiff >  180)		londiff -= 360;

	if (fabs(londiff) > BSPLINE_LON_WIDTH)
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
double ionCoefBsplin(
	Trace&		trace,
	int			ind,
	IonoObs&	obs,
	bool		slant)
{
	if (ind >= bspBasisMap.size())
		return 0;

	auto& basis = bspBasisMap[ind];

	double latdiff = (obs.ippMap[basis.ind].latDeg - basis.latDeg) / BSPLINE_LAT_INTERVAL;

	if	(  latdiff <= -1
		|| latdiff >= +1)
	{
		return 0;
	}

	double londiff = (obs.ippMap[basis.ind].lonDeg - basis.lonDeg);

	if (londiff < -180)		londiff += 360;
	if (londiff >  180)		londiff -= 360;

	londiff /= BSPLINE_LAT_INTERVAL;

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
		out *= obs.ippMap[basis.ind].slantFactor * obs.stecToDelay;
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
	Trace&		trace,
	GTime		time,
	VectorPos&	ionPP,
	int			layer,
	double&		var,
	KFState&	kfState)
{
	var = 0;

	if (ippCheckBsplin(time, ionPP) == false)
	{
		return 0;
	}

	double iono = 0;
	GObs tmpobs;
	tmpobs.ippMap[layer].latDeg			= ionPP.latDeg();
	tmpobs.ippMap[layer].lonDeg			= ionPP.lonDeg();
	tmpobs.ippMap[layer].slantFactor	= 1;

	for (int ind = 0; ind < acsConfig.ionModelOpts.numBasis; ind++)
	{
		auto& basis = bspBasisMap[ind];

		if (basis.ind != layer)
			continue;

		double coef = ionCoefBsplin(trace, ind, tmpobs, false);

		KFKey keyC;
		keyC.type	= KF::IONOSPHERIC;
		keyC.num	= ind;

		double kfval = 0;
		double kfvar = 0;
		kfState.getKFValue(keyC, kfval, &kfvar);

		iono += 	coef	* kfval;
		var += SQR(coef)	* kfvar;
	}

	return iono;
}
