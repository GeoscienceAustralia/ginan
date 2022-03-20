
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


/*-------------------------------------------------------------------------
configure_iono_model_bsplin: Initializes grid map model
	The following configursation parameters are used
	-  acsConfig.ionFilterOpts.lat_center:    latitude of map centre
	-  acsConfig.ionFilterOpts.lon_center:    longitude of map centre
	-  acsConfig.ionFilterOpts.lat_width:	  latitude width of maps
	-  acsConfig.ionFilterOpts.lon_width:	  longitude width of maps
	-  acsConfig.ionFilterOpts.lat_res:		  latitude resolution of gridmap
	-  acsConfig.ionFilterOpts.lon_res:		  longitude resolution of gridmap
	-  acsConfig.ionFilterOpts.layer_heights: Ionosphere layer Heights
----------------------------------------------------------------------------*/
int configure_iono_model_bsplin(void)
{
	if (acsConfig.ionFilterOpts.lat_width > 180.0 || 0.0 < acsConfig.ionFilterOpts.lat_width ||
	        acsConfig.ionFilterOpts.lon_width > 360.0 || 0.0 < acsConfig.ionFilterOpts.lon_width)
	{
		std::cout << "Wrongly sized gridmaps revise lat and lon width parameters...";
		return 0;
	}

	BSPLINE_LATCEN = acsConfig.ionFilterOpts.lat_center * D2R;
	BSPLINE_LONCEN = acsConfig.ionFilterOpts.lon_center * D2R;
	BSPLINE_LATINT = acsConfig.ionFilterOpts.lat_res * D2R;
	BSPLINE_LONINT = acsConfig.ionFilterOpts.lon_res * D2R;

	int latnum = (int)(acsConfig.ionFilterOpts.lat_width / acsConfig.ionFilterOpts.lat_res) + 1;
	int lonnum = (int)(acsConfig.ionFilterOpts.lon_width / acsConfig.ionFilterOpts.lon_res) + 1;

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

	for (int lay = 0; lay < acsConfig.ionFilterOpts.layer_heights.size(); lay++)
	{
		basis.hind = lay;

		for (int lat = 0; lat < latnum; lat++)
		{
			basis.latit = latmin + lat * BSPLINE_LATINT;

			for (int lon = 0; lon < lonnum; lon++)
			{
				double lonmap = lonmin + lon * BSPLINE_LONINT;

				if (lonmap < -PI) lonmap += 2 * PI;

				if (lonmap >  PI) lonmap -= 2 * PI;

				basis.longi = lonmap;
				Bsp_Basis_list[ind++] = basis;
			}
		}
	}

	acsConfig.ionFilterOpts.NBasis = ind;

	for (int j = 0; j < acsConfig.ionFilterOpts.NBasis; j++)
	{
		Bsp_Basis& basis2 = Bsp_Basis_list[j];
		fprintf(fp_iondebug, "GRD_BASIS %3d %2d %8.4f %8.4f ", j, basis2.hind, basis.latit * R2D, basis.longi * R2D);
		fprintf(fp_iondebug, "\n");
	}

	return ind;
}

/*-----------------------------------------------------
Ipp_check_bsplin (time,IPP) checks if the Ionosphere
Piercing Point falls in area of coverage
time:  		I 		time of observations (not used)
IPP: 			I 		Ionospheric piercing point to be updated
returns 1 if the IPP is within the area of coverage
-----------------------------------------------------
Author: Ken Harima @ RMIT 04 August 2020
-----------------------------------------------------*/
int Ipp_check_bsplin(GTime time, double* Ion_pp)
{
	if (fabs(BSPLINE_LATCEN - Ion_pp[0]) > BSPLINE_LATWID) return 0;

	double londiff = BSPLINE_LONCEN - Ion_pp[1];

	if (londiff < -PI) londiff += 2 * PI;

	if (londiff >  PI) londiff -= 2 * PI;

	if (fabs(londiff) > BSPLINE_LONWID) return 0;

	return 1;
}

/*-------------------------------------------------------------------------
ion_coef_bsplin: Evaluates B-splines basis functions
	int ind			I		Basis function number
	meas			I		Ionosphere measurement struct
		latIPP				- Latitude of Ionosphere Piercing Point
		lonIPP				- Longitude of Ionosphere Piercing Point
		angIPP				- Angular gain for Ionosphere Piercing Point
	bool slant		I		state to delay gain; false: state to VTEC gain

	BSPLINE_LATINT and BSPLINE_LONINT needs to be set before calling this function
----------------------------------------------------------------------------*/
double ion_coef_bsplin(int ind, Obs& obs, bool slant)
{
	if (ind >= Bsp_Basis_list.size()) return 0.0;

	Bsp_Basis& basis = Bsp_Basis_list[ind];

	double latdiff = (obs.latIPP[basis.hind] - basis.latit) / BSPLINE_LATINT;

	if (latdiff <= -1.0 || latdiff >= 1.0) return 0.0;

	double londiff = (obs.lonIPP[basis.hind] - basis.longi);

	if (londiff < -PI) londiff += 2 * PI;

	if (londiff >  PI) londiff -= 2 * PI;

	londiff /= BSPLINE_LATINT;

	if (londiff <= -1.0 || londiff >= 1.0) return 0.0;

	double out = latdiff < 0.0 ? (1.0 + latdiff) : (1.0 - latdiff);

	if (londiff < 0.0)out *= 1.0 + londiff;
	else out *= 1.0 - londiff;

	if (slant)
	{
		out *= obs.angIPP[basis.hind] * obs.STECtoDELAY;
	}

	return out;
}

/*-------------------------------------------------------------------------
ion_vtec_bsplin: Estimate Ionosphere VTEC using Ionospheric gridmaps
	gtime_t  time		I		time of solutions (not useful for this one
	Ion_pp				I		Ionosphere Piercing Point
	layer				I 		Layer number
	vari				O		variance of VTEC
returns: VETC at piercing point
----------------------------------------------------------------------------*/
double ion_vtec_bsplin(
    GTime time,
    double* Ion_pp,
    int layer,
    double& vari,
    KFState& kfState)
{
	if (!Ipp_check_bsplin(time, Ion_pp))
	{
		vari = 0.0;
		return 0.0;
	}

	vari = 0;
	double iono = 0;
	Obs tmpobs;
	tmpobs.latIPP[layer] = Ion_pp[0];
	tmpobs.lonIPP[layer] = Ion_pp[1];
	tmpobs.angIPP[layer] = 1.0;

	for (int ind = 0; ind < acsConfig.ionFilterOpts.NBasis; ind++)
	{
		Bsp_Basis& basis = Bsp_Basis_list[ind];

		if (basis.hind != layer) continue;

		double coef = ion_coef_bsplin(ind, tmpobs, false);

		KFKey keyC;
		keyC.type	= KF::IONOSPHERIC;
		keyC.num	= ind;

		double staval = 0, stastd = 0;
		kfState.getKFValue(keyC, staval, &stastd);

		iono += 	coef * staval;
		vari += SQR(coef * stastd);
	}

	return iono;
}
