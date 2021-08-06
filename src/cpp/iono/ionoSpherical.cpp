
#include "ionoModel.hpp"
#include "observations.hpp"
#include "common.hpp"
#include "acsConfig.hpp"
#include "tides.hpp"

#define SQR(x)  ((x)*(x))

struct leg_elem    /* coef x sin(lat)^sind x cos(lat)^cosd */
{
	int sind;
	int cosd;
	double coef;
};

struct Sph_Basis
{
	int hind;					/* layer number */
	int degree;					/* degree of the function */
	int order;					/* order of the legendre function */
	list<leg_elem> legpoly;		/* Legendre polynomial (polynomial of trigonometric)*/
	bool parity;				/* longitude function: false=cosine, true=sine */
	double norm;
};

map<int, Sph_Basis>  Sph_Basis_list;

double shar_rotmtx[9];				/* Rotation matrix (to centre of map) */
GTime shar_time = {0};
double shar_valid = 10.0;

/*-----------------------------------------------------
configure_iono_model_sphhar() configures the spherical harmonics model.
Specifically it initializes:
shar_valid				time validity of a rotation matrix (the rotation matrix will chase the sun position)
Sph_Basis_list			List of ionosphere basis
time:  		I 		time of observations (to update the rotation matrix)
IPP: 			I 		Ionospheric piercing point to be updated
Since the spherical harmonic model has global validity, the check always return 1
-----------------------------------------------------
Author: Ken Harima @ RMIT 29 July 2020
-----------------------------------------------------*/
int configure_iono_model_sphhar()
{

	for (int i = 0; i < 9; i++)
	{
		shar_rotmtx[i] = 0.0;
	}

	shar_valid = DTTOL;
	shar_time.time = 0;

	int Kmax = acsConfig.ionFilterOpts.func_order + 1;
	int nlay = acsConfig.ionFilterOpts.layer_heights.size();
	int ind = 0;

	for (int j = 0; j < nlay; j++)
	{
		map<int, std::pair<int, int>> basismap;

		for (int k = 0; k < Kmax; k++)
		{
			if (k == 0)
			{
				/* legendre 0,0 */
				Sph_Basis basis;
				basis.hind = j;
				basis.order = 0;
				basis.degree = 0;
				basis.norm = (0.5 * basis.degree + 0.25) / PI;
				leg_elem elem;
				elem.sind = 0;
				elem.cosd = 0;
				elem.coef = 1.0;
				basis.legpoly.push_back(elem);
				basis.parity = false;
				basismap[k].first = ind;
				Sph_Basis_list[ind++] = basis;
			}
			else
			{
				Sph_Basis basis = Sph_Basis_list[basismap[k - 1].first];
				/* leg(k-1,k)  =  (2k-1) * coslat * leg(k-1,k-1) */
				basis.degree = k;
				basis.norm = (0.5 * k + 0.25) / PI;

				for (int l = (1 - basis.order); l <= basis.order; l++) basis.norm /= 1.0 * (basis.degree + l);

				basis.legpoly.front().cosd++;
				basis.legpoly.front().coef *= 1.0 * (2 * k - 1);
				basis.parity = false;
				basismap[k - 1].second = ind;
				Sph_Basis_list[ind++] = basis;

				if (k > 1)
				{
					basis.parity = true;
					Sph_Basis_list[ind++] = basis;
				}

				/* leg(k,k)  =  -(2k-1) * sinlat * leg(k-1,k-1) */
				basis.order = k;
				basis.norm = (0.5 * basis.degree + 0.25) / PI;

				for (int l = (1 - basis.order); l <= basis.order; l++) basis.norm /= 1.0 * (basis.degree + l);

				basis.legpoly.front().cosd--;
				basis.legpoly.front().sind++;
				basis.legpoly.front().coef *= -1.0;
				basis.parity = false;
				basismap[k].first = ind;
				Sph_Basis_list[ind++] = basis;
				basis.parity = true;
				Sph_Basis_list[ind++] = basis;
			}
		}

		if (Kmax < 3)
		{
			std::cout << "WARNING,Are you sure you want a 4 parameter Ionosphere?";
			continue;
		}

		for (int m = 0; m < Kmax - 2; m++)
		{
			Sph_Basis basis1 = Sph_Basis_list[basismap[m].first];
			Sph_Basis basis2 = Sph_Basis_list[basismap[m].second];

			for (int n = m + 2; n < Kmax; n++)
			{
				Sph_Basis basisnew;
				basisnew.hind = j;
				basisnew.order = m;
				basisnew.degree = n;
				basisnew.norm = (0.5 * basisnew.degree + 0.25) / PI;

				for (int l = (1 - basisnew.order); l <= basisnew.order; l++) basisnew.norm /= 1.0 * (basisnew.degree + l);

				basisnew.parity = false;
				basisnew.legpoly.clear();

				for (auto& elem : basis1.legpoly)
				{
					leg_elem elemnew = elem;
					elemnew.coef *= -1.0 * (n + m - 1) / (n - m);
					basisnew.legpoly.push_back(elemnew);
				}

				for (auto& elem : basis2.legpoly)
				{
					leg_elem elemnew = elem;
					elemnew.cosd++;
					elemnew.coef *= 1.0 * (2 * n - 1) / (n - m);
					basisnew.legpoly.push_back(elemnew);
				}

				basis1 = basis2;
				basis2 = basisnew;
				Sph_Basis_list[ind++] = basisnew;

				if (m > 0)
				{
					basisnew.parity = true;
					Sph_Basis_list[ind++] = basisnew;
				}
			}
		}
	}

	acsConfig.ionFilterOpts.NBasis = ind;

	if (fp_iondebug)  for (int j = 0; j < acsConfig.ionFilterOpts.NBasis; j++)
		{
			Sph_Basis& basis = Sph_Basis_list[j];
			fprintf(fp_iondebug, "SPH_BASIS %3d %2d %2d %2d %12.4e %1d %3lu ", j, basis.hind, basis.order, basis.degree, basis.norm, basis.parity, basis.legpoly.size());

			for (auto& item : basis.legpoly)
			{
				fprintf(fp_iondebug, "- %2d %2d %8.4f ", item.sind, item.cosd, item.coef);
			}

			fprintf(fp_iondebug, "\n");
		}

	return ind;
}

/*-----------------------------------------------------
Ipp_check_sphhar (time,IPP) rotates the Ionosphere piercing point
time:  		I 		time of observations (to update the rotation matrix)
IPP: 			I 		Ionospheric piercing point to be updated
Since the spherical harmonic model has global validity, the check always return 1
-----------------------------------------------------
Author: Ken Harima @ RMIT 29 July 2020
-----------------------------------------------------*/
extern int Ipp_check_sphhar(GTime time, double* Ion_pp)
{

	if ( time.time == 0 ) return 0;

	if (	shar_time.time == 0 ||
			fabs(timediff(time, shar_time)) > shar_valid )
	{

		double erpv[5] = {0}, rsun[3] = {0}, rmon[3] = {0}, gmst, sunpos[3] = {0};
		sunmoonpos(gpst2utc(time), erpv, rsun, rmon, &gmst);
		ecef2pos(rsun, sunpos);

		shar_rotmtx[0] = cos(sunpos[0]) * cos(sunpos[1]);
		shar_rotmtx[1] = cos(sunpos[0]) * sin(sunpos[1]);
		shar_rotmtx[2] = sin(sunpos[0]);
		shar_rotmtx[3] = -sin(sunpos[1]);
		shar_rotmtx[4] = cos(sunpos[1]);
		shar_rotmtx[5] = 0.0;
		shar_rotmtx[6] = -sin(sunpos[0]) * cos(sunpos[1]);
		shar_rotmtx[7] = -sin(sunpos[0]) * sin(sunpos[1]);
		shar_rotmtx[8] = cos(sunpos[0]);

		shar_time = time;

		if (fp_iondebug)
		{
			fprintf(fp_iondebug, "SPH_ROTMX %s\n", time.to_string(6).c_str());
			fprintf(fp_iondebug, "SPH_ROTMX %.5e,%.5e,%.5e; %.5e,%.5e,%.5e; %.5e,%.5e,%.5e\n",
					shar_rotmtx[0], shar_rotmtx[1], shar_rotmtx[2],
					shar_rotmtx[3], shar_rotmtx[4], shar_rotmtx[5],
					shar_rotmtx[6], shar_rotmtx[7], shar_rotmtx[8]);
		}
	}

	double pos[3], rpp[3], rrot[3];
	pos[0] = Ion_pp[0];
	pos[1] = Ion_pp[1];
	pos[2] = acsConfig.ionFilterOpts.layer_heights[0];
	pos2ecef(pos, rpp);
	matmul("NN", 3, 1, 3, 1, shar_rotmtx, rpp, 0, rrot);
	ecef2pos(rrot, pos);

	Ion_pp[0] = pos[0] + PI / 2;			/* colatitude for spherical harmonics */
	Ion_pp[1] = pos[1];
	return 1;
}

/*-----------------------------------------------------
P=leg(basis,lat) Returns the legendre polynimial evaluated at lat
The basis contein the legendre polynomial at basis.legpoly
latitude: argument (rad)
Truncation is set up by the resolution parameter eps0
-----------------------------------------------------
Author: Ken Harima @ RMIT 20 May 2020
-----------------------------------------------------*/
double legendre_poly(Sph_Basis& basis, double lat)
{
	double sinlat = sin(lat);
	double coslat = cos(lat);
	double tot = 0;

	for (auto& elem : basis.legpoly)
	{
		double sincmp = pow(sinlat, elem.sind);
		double coscmp = pow(coslat, elem.cosd);
		tot += elem.coef * sincmp * coscmp;
	}

	return tot;
}

/*-------------------------------------------------------------------------
ion_coef_sphcap: Evaluates spherical cap harmonics basis functions
	int ind			I		Basis function number
	obs				I		Ionosphere measurement struct
		latIPP				- Latitude of Ionosphere Piercing Point
		lonIPP				- Longitude of Ionosphere Piercing Point
		angIPP				- Angular gain for Ionosphere Piercing Point
	int slant		I		0: coefficient for Vtec, 1: coefficient for slant delay
----------------------------------------------------------------------------*/
double ion_coef_sphhar(int ind, Obs& obs, bool slant)
{
	if (ind >= Sph_Basis_list.size())
		return 0;

	Sph_Basis& basis = Sph_Basis_list[ind];

	double legr	= legendre_poly(basis, obs.latIPP[basis.hind]);		// Legendre function

	double out = basis.norm * legr;

	if (basis.parity)	out *= sin(basis.order * obs.lonIPP[basis.hind]);
	else				out *= cos(basis.order * obs.lonIPP[basis.hind]);

	if (slant)
	{
		out *= obs.angIPP[basis.hind] * obs.STECtoDELAY;
	}

	return out;
}

/*-------------------------------------------------------------------------
ion_vtec_sphcap: Estimate Ionosphere VTEC using Spherical Cap Harmonic models
	gtime_t  time		I		time of solutions (not useful for this one
	Ion_pp				I		Ionosphere Piercing Point
	layer				I 		Layer number
	vari				O		variance of VTEC
returns: VETC at piercing point
----------------------------------------------------------------------------*/
double ion_vtec_sphhar(
	GTime time,
	double* Ion_pp,
	int layer,
	double& vari,
	KFState& kfState)
{
	double ionpp_cpy[3];
	int ix, m;

	ionpp_cpy[0] = Ion_pp[0];
	ionpp_cpy[1] = Ion_pp[1];
	ionpp_cpy[2] = acsConfig.ionFilterOpts.layer_heights[layer];

	Ipp_check_sphhar(time, ionpp_cpy);

	vari = 0;
	double iono = 0;
	Obs tmpobs;
	tmpobs.latIPP[layer] = ionpp_cpy[0];
	tmpobs.lonIPP[layer] = ionpp_cpy[1];
	tmpobs.angIPP[layer] = 1;

	for (int ind = 0; ind < acsConfig.ionFilterOpts.NBasis; ind++)
	{
		Sph_Basis& basis = Sph_Basis_list[ind];

		if (basis.hind != layer) continue;

		double coef = ion_coef_sphhar(ind, tmpobs, false);

		KFKey keyC;
		keyC.type	= KF::IONOSPHERIC;
		keyC.num	= ind;

		double staval = 0, stastd = 0;
		kfState.getKFValue(keyC, staval, &stastd);

		if (fp_iondebug)
		{
			fprintf(fp_iondebug, "VTEC_COEF  %9.5f %10.5f  %9.5f %10.5f  %3d  %9.5f %10.5f %12.5f\n",
					Ion_pp[0]*R2D, Ion_pp[1]*R2D, ionpp_cpy[0]*R2D, ionpp_cpy[1]*R2D, ind, coef, staval, iono);
		}

		iono += 	coef * staval;
		vari += SQR(coef)* stastd;
	}

	return iono;
}

