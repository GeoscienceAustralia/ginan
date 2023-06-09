
#include "acceleration.hpp"
#include "observations.hpp"
#include "coordinates.hpp"
#include "corrections.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "ionoModel.hpp"
#include "planets.hpp"
#include "common.hpp"


Legendre ionLeg;                              		
double ionSPHLastColatitude = -100;

struct SphBasis
{
	int hind;					/* layer number */
	int degree;					/* degree of the function */
	int order;					/* order of the legendre function */
	bool parity;				/* longitude function: false=cosine, true=sine */
};

map<int, SphBasis>  sphBasisMap;
map<int, map<int, map<int, map<bool, int>>>>  sphBasisIndxMaps;

Matrix3d sphRotMatrix;				/* Rotation matrix (to centre of map) */
GTime	sphTime		= {};
double	sphValid	= 10;

/** configures the spherical harmonics model.
Specifically it initializes:
shar_valid				time validity of a rotation matrix (the rotation matrix will chase the sun position)
Sph_Basis_list			List of ionosphere basis
time:  		I 		time of observations (to update the rotation matrix)
IPP: 			I 		Ionospheric piercing point to be updated
Since the spherical harmonic model has global validity, the check always return 1
-----------------------------------------------------
Author: Ken Harima @ RMIT 29 July 2020
-----------------------------------------------------*/
int configIonModelSphhar()
{
	sphRotMatrix.setZero();

	sphValid		= DTTOL;
	sphTime.bigTime	= 0;

	if		(acsConfig.ionModelOpts.function_degree == 0)										acsConfig.ionModelOpts.function_degree	= acsConfig.ionModelOpts.function_order;	
	else if (acsConfig.ionModelOpts.function_order > acsConfig.ionModelOpts.function_degree)	acsConfig.ionModelOpts.function_order	= acsConfig.ionModelOpts.function_degree;
	
	int Nmax = acsConfig.ionModelOpts.function_degree + 1;
	int nlay = acsConfig.ionModelOpts.layer_heights.size();
	if (nlay == 0) 
	{
		acsConfig.ionModelOpts.layer_heights.push_back(350);
		nlay = 1;
	}
	
	ionLeg.setNmax(Nmax);

	int ind = 0;
	for (int layer	= 0;		layer	< nlay;										layer++)
	for (int order	= 0;		order	< acsConfig.ionModelOpts.function_order;	order++)
	for (int degree	= order;	degree	< acsConfig.ionModelOpts.function_degree;	degree++)
	{
		SphBasis basis;
		basis.hind		= layer;
		basis.order		= order;
		basis.degree	= degree;
		basis.parity	= false;
			
		sphBasisIndxMaps[layer][degree][order][false] = ind;
		sphBasisMap[ind] = basis;
		ind++;
		
		if (order > 0)
		{
			basis.parity = true;
			sphBasisIndxMaps[layer][degree][order][true] = ind;
			sphBasisMap[ind] = basis;
			ind++;
		}
	}

	acsConfig.ionModelOpts.NBasis = ind;

	return ind;
}

/** rotates the Ionosphere piercing point
time:  		I 		time of observations (to update the rotation matrix)
IPP: 			I 		Ionospheric piercing point to be updated
Since the spherical harmonic model has global validity, the check always return 1
-----------------------------------------------------
Author: Ken Harima @ RMIT 29 July 2020
-----------------------------------------------------*/
bool ippCheckSphhar(
	GTime		time, 
	VectorPos&	ionPP)
{
	if 	(time.bigTime == 0) 
		return false;

	if (acsConfig.ionModelOpts.use_rotation_mtx)
	{
		if	(  sphTime.bigTime == 0 
			|| fabs((time - sphTime).to_double()) > sphValid )
		{
			VectorEcef	rSun;
			planetPosEcef(time, E_ThirdBody::SUN, rSun);
		
			VectorPos sunpos = ecef2pos(rSun);

			sphRotMatrix(0,0) =  cos(sunpos[0]) * cos(sunpos[1]);	sphRotMatrix(0,1) = -sin(sunpos[1]);	sphRotMatrix(0,2) = -sin(sunpos[0]) * cos(sunpos[1]);		
			sphRotMatrix(1,0) =  cos(sunpos[0]) * sin(sunpos[1]);	sphRotMatrix(1,1) =  cos(sunpos[1]);	sphRotMatrix(1,2) = -sin(sunpos[0]) * sin(sunpos[1]);		
			sphRotMatrix(2,0) =  sin(sunpos[0]);					sphRotMatrix(2,1) =  0;					sphRotMatrix(2,2) =  cos(sunpos[0]);

			sphTime = time;
		}
		
		VectorPos pos;
		pos.hgt() = ionPP.hgt();
		pos.lon() = ionPP.lon();
		pos.hgt() = acsConfig.ionModelOpts.layer_heights[0];
		
		VectorEcef rpp = pos2ecef(pos);
		
		VectorEcef rrot = (Vector3d)(sphRotMatrix * rpp);
		pos = ecef2pos(rrot);
	
		ionPP.lat() = pos.lat() + PI/2;			/* colatitude for spherical harmonics */
		ionPP.lon() = pos.lon();
	}
	else
	{
		// int week;
		// double tow = time2gpst(time,&week);
		double tow = GTow(time);
		
		ionPP.lat() = ionPP.lat() - PI/2;
		ionPP.lon()+= (tow-50400)*PI/43200;
		double day= floor(ionPP.lon()/(2*PI))*2*PI;
																		
		ionPP.lon()-= day;
	}
	return true;
}

/*-----------------------------------------------------
P=leg(basis,lat) Returns the legendre polynimial evaluated at lat
The basis contein the legendre polynomial at basis.legpoly
latitude: argument (rad)
Truncation is set up by the resolution parameter eps0
-----------------------------------------------------
Author: Ken Harima @ RMIT 20 May 2020
-----------------------------------------------------*/
// double legendre_poly(Sph_Basis& basis, double lat)
// {
// 	double sinlat = sin(lat);
// 	double coslat = cos(lat);
// 	double tot = 0;

// 	for (auto& elem : basis.legpoly)
// 	{
// 		double sincmp = pow(sinlat, elem.sind);
// 		double coscmp = pow(coslat, elem.cosd);
// 		tot += elem.coef * sincmp * coscmp;
// 	}

// 	return tot;
// }

/** Evaluates spherical cap harmonics basis functions
	int ind			I		Basis function number
	obs				I		Ionosphere measurement struct
		latIPP				- Latitude of Ionosphere Piercing Point
		lonIPP				- Longitude of Ionosphere Piercing Point
		angIPP				- Angular gain for Ionosphere Piercing Point
	int slant		I		0: coefficient for VTEC, 1: coefficient for STEC
----------------------------------------------------------------------------*/
double ionCoefSphhar(int ind, GObs& obs, bool slant)
{
	if (ind >= sphBasisMap.size())
		return 0;

	auto& basis = sphBasisMap[ind];

	if (basis.order		> acsConfig.ionModelOpts.function_order)
		return 0;

	if (basis.degree	> acsConfig.ionModelOpts.function_degree)
		return 0;

	double colat = obs.ippMap[basis.hind].lat;
	
	if (fabs(ionSPHLastColatitude - colat) > 0.01)
		ionLeg.calculate(cos(colat));

	double out = pow(-1,basis.order)*ionLeg.Pnm(basis.degree, basis.order);

	if (basis.parity)	out *= sin(basis.order * obs.ippMap[basis.hind].lon);
	else				out *= cos(basis.order * obs.ippMap[basis.hind].lon);

	if (slant)
	{
		out *= obs.ippMap[basis.hind].ang;
	}

	return out;
}

/** Estimate Ionosphere VTEC using Spherical Cap Harmonic models
	gtime_t  time		I		time of solutions (not useful for this one
	Ion_pp				I		Ionosphere Piercing Point
	layer				I 		Layer number
	vari				O		variance of VTEC
returns: VETC at piercing point
----------------------------------------------------------------------------*/
double ionVtecSphhar(
	GTime time,
	VectorPos& ionPP,
	int layer,
	double& vari,
	KFState& kfState)
{
	VectorPos ionpp_cpy;

	ionpp_cpy[0] = ionPP[0];
	ionpp_cpy[1] = ionPP[1];
	ionpp_cpy[2] = acsConfig.ionModelOpts.layer_heights[layer];

	ippCheckSphhar(time, ionpp_cpy);

	vari = 0;
	double iono = 0;
	GObs tmpobs;
	tmpobs.ippMap[layer].lat = ionpp_cpy[0];
	tmpobs.ippMap[layer].lon = ionpp_cpy[1];
	tmpobs.ippMap[layer].ang = 1;

	for (int ind = 0; ind < acsConfig.ionModelOpts.NBasis; ind++)
	{
		auto& basis = sphBasisMap[ind];

		if (basis.hind != layer) 
			continue;

		double coef = ionCoefSphhar(ind, tmpobs, false);

		KFKey keyC;
		keyC.type	= KF::IONOSPHERIC;
		keyC.num	= ind;

		double staval = 0;
		double stastd = 0;
		kfState.getKFValue(keyC, staval, &stastd);

		iono += 	coef * staval;
		vari += SQR(coef)* stastd;
	}

	return iono;
}

void ionOutputSphcal(
	Trace&		trace, 
	KFState&	kfState)
{
	SSRAtmGlobal atmGlob;
	atmGlob.numberLayers = acsConfig.ionModelOpts.layer_heights.size();
	for (int j = 0; j < atmGlob.numberLayers; j++)
	{
		atmGlob.layers[j].height	= acsConfig.ionModelOpts.layer_heights[j] / 1000;
		atmGlob.layers[j].maxOrder	= acsConfig.ionModelOpts.function_order;
		atmGlob.layers[j].maxDegree	= acsConfig.ionModelOpts.function_degree;
	}
	
	tracepdeex (4, trace, "\n#IONO_MODL  tow   indx hght order degr part    value       variance");
	for (auto [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::IONOSPHERIC)
			continue;
		
		SphBasis basis		= sphBasisMap[key.num];
		auto& iono_record	= atmGlob.layers[basis.hind].sphHarmonic[key.num];
		iono_record.hind	= basis.hind;
		iono_record.order	= basis.order;
		iono_record.degree	= basis.degree;
		iono_record.parity	= basis.parity;
		
		double val;
		double var;
		kfState.getKFValue(key,val,&var);
		iono_record.coeffc		= val;
		iono_record.variance	= var;
		
		GTow tow = kfState.time;
		tracepdeex (4, trace, "\nIONO_MODL %6d %3d  %4.0f  %2d   %2d   %2d  %10.4f %12.5e",
		         (int)tow, key.num, atmGlob.layers[basis.hind].height, 
		         basis.order, basis.degree, basis.parity?1:0, val, sqrt(var));
		
	}
	tracepdeex (4, trace, "\n");
	
	atmGlob.time= kfState.time;
	
	nav.ssrAtm.atmosGlobalMap[kfState.time] = atmGlob;
}

bool getEpcSsrIono(
	GTime		time,		///< time of ionosphere correction
	SSRAtmGlobal& atmGlob,	///< SSR atmospheric correction
	Vector3d&	rSat,		///< Satellite position
	Vector3d&	rRec,		///< receiver position
	double& 	iono,		///< Ionosphere delay (in TECu)
	double& 	vari)		///< Ionosphere variance
{
	vari = -1;
	iono =  0;
	
	if (fabs((time - atmGlob.time).to_double())> acsConfig.ssrInOpts.global_vtec_valid_time)
		return false;
	
	if (sphBasisIndxMaps.size() < atmGlob.layers.size())
	{
		sphBasisIndxMaps.clear();
		int maxdeg = 0;
		int maxord = 0;
		for (auto& [hind,atmLay]: atmGlob.layers)
		{
			if (maxdeg < atmLay.maxDegree)		maxdeg = atmLay.maxDegree;
			if (maxord < atmLay.maxOrder)		maxord = atmLay.maxOrder;
			
			acsConfig.ionModelOpts.layer_heights[hind] = atmLay.height;
		}
		
		acsConfig.ionModelOpts.function_degree	= maxdeg;
		acsConfig.ionModelOpts.function_order	= maxord;
	
		configIonModelSphhar();
	}
	
	VectorPos pos = ecef2pos(rRec);
	
	Vector3d e;
	double r = geodist(rSat, rRec, e);
	double azel[2];
	satazel(pos, e, azel);
			
	double ivar	 = 0;
	for (auto& [hind, atmLay]: atmGlob.layers)
	{
		VectorPos posp;
		double angIPP = ionppp(pos, azel, RE_WGS84 / 1000, atmLay.height, posp);
	
		if (ippCheckSphhar(time, posp) == false)
			return 0;
		
		GObs tmpobs;
		tmpobs.ippMap[hind].lat = posp[0];
		tmpobs.ippMap[hind].lon = posp[1];
		tmpobs.ippMap[hind].ang = angIPP;
		
		for (auto& [ind,Harmn] : atmLay.sphHarmonic)
		{
			int reindex = sphBasisIndxMaps[Harmn.hind][Harmn.degree][Harmn.order][Harmn.parity];
			double comp = ionCoefSphhar(reindex, tmpobs, true);
			iono +=		comp	* Harmn.coeffc;
			ivar += SQR(comp)	* Harmn.variance;
		}
	}
	
	vari = atmGlob.vtecQuality + ivar;
	return iono;
}


bool getIGSSSRIono(
	GTime		time,	///< time of ionosphere correction
	SSRAtm&		ssrAtm,	///< SSR atmospheric correction
	Vector3d&	rSat,	///< Satellite position
	Vector3d&	rRec,	///< receiver position
	double& 	iono,	///< Ionosphere delay (in TECu)
	double& 	var)	///< Ionosphere variance
{
	var = -1;
	iono =  0;
	
	auto it = ssrAtm.atmosGlobalMap.lower_bound(time);
	if (it == ssrAtm.atmosGlobalMap.end())
		return 0;

	auto& [t0, ssrAtm0] = *it;
	
	double iono0	= 0;
	double var0		= 0;
	bool pass0		= getEpcSsrIono(t0, ssrAtm0, rRec, rSat, iono0, var0);
	
	double iono1	= 0;
	double var1		= 0;
	bool pass1;
	double a;
	if (it == ssrAtm.atmosGlobalMap.begin())
	{
		pass1 = false;
	}
	else
	{
		it--;
		auto& [t1, ssrAtm1] = *it;
		pass1		= getEpcSsrIono(t1, ssrAtm1, rRec, rSat, iono1, var1);
		if (pass1)
		{
			// double tt	= (t1 - t0).to_double();
			// a	= (time	- t0).to_double() / tt;
			
			a = (time - t0)/(t1 - t0);
		}
	}
	
	if (!pass0 && !pass1) {	var = -1;		return 0;}
	if ( pass0 && !pass1) {	var = var0;		return iono0;}
	if (!pass0 &&  pass1) {	var = var1;		return iono1;}

	var = var0		* SQR(1-a)	+ var1	* SQR(a);
	return iono0	*	 (1-a)	+ iono1	*	  a;
}
