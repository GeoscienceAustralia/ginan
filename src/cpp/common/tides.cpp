
// #pragma GCC optimize ("O0")

/** \file
* ###References:
*
* 1.  G.Petit and B.Luzum (eds), IERS Technical Note No. 36, IERS Conventions (2010), 2010
* 2.1 IERS Conventions (2010) Working Version 1.3.0 — Most Recent, https://iers-conventions.obspm.fr/conventions_material.php
* 2.2 IERS Conventions (2010) Working Version 1.3.0 — Chapter 7, https://iers-conventions.obspm.fr/content/chapter7/icc7.pdf
* 3.  DEHANTTIDEINEL, https://iers-conventions.obspm.fr/content/chapter7/software/dehanttideinel
* 4.  ARG2.F, https://iers-conventions.obspm.fr/content/chapter7/software/ARG2.F
* 5.  HARDISP, https://iers-conventions.obspm.fr/content/chapter7/software/hardisp/
* 6.  H.-G. Scherneck, Explanatory Supplement to the Section “Local Site Displacement due to Ocean loading” of the IERS Conventions (1996), 1999
* 7.  libiers10++, https://github.com/xanthospap/iers2010
*/

#include <boost/algorithm/string.hpp>
#include <iostream>
#include <fstream>
#include <math.h>

using boost::algorithm::to_lower_copy;
using std::ifstream;

#include "coordinates.hpp"
#include "ephPrecise.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "iers2010.hpp"
#include "receiver.hpp"
#include "planets.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "tides.hpp"
#include "trace.hpp"
#include "erp.hpp"

using iers2010::hisp::ntin;

const double		mjd1975	= 42413;		///< MJD of 1975.0

OceanPoleGrid		oceanPoleGrid;			///< Grid map of ocean pole load tide

map<E_TidalComponent, int> blqSignMap = 	///< Map of signs of BLQ records (positive east, north and upwards)
{
	{E_TidalComponent::EAST,	+1},
	{E_TidalComponent::WEST,	-1},
	{E_TidalComponent::NORTH,	+1},
	{E_TidalComponent::SOUTH,	-1},
	{E_TidalComponent::UP,		+1},
	{E_TidalComponent::DOWN,	-1},
};

map<E_TidalComponent, int> blqIndexMap = 	///< Map of indexes of BLQ records
{
	{E_TidalComponent::EAST,	0},
	{E_TidalComponent::WEST,	0},
	{E_TidalComponent::NORTH,	1},
	{E_TidalComponent::SOUTH,	1},
	{E_TidalComponent::UP,		2},
	{E_TidalComponent::DOWN,	2},
};

/** Read BLQ record for a single station
*/
bool readBlqRecord(
	ifstream&					fileStream,			///< Stream to read content from
	vector<E_TidalConstituent>	waveList,			///< List of tidal constituents
	vector<E_TidalComponent>	componentList,		///< List of components {E/W, N/S, U/D}
	TideMap&					tideLoading)		///< Map of ocean/atmospheric tide loading displacements in amplitude and phase
{
	double v[11];
	int n = 0;

	while (fileStream)
	{
		string line;
		getline(fileStream, line);

		if (line.substr(0, 2) == "$$")
			continue;

		char* buff = &line[0];
		int found = sscanf(buff, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
			&v[0],
			&v[1],
			&v[2],
			&v[3],
			&v[4],
			&v[5],
			&v[6],
			&v[7],
			&v[8],
			&v[9],
			&v[10]);

		auto component	= componentList[n % 3];
		int index		= blqIndexMap[component];
		int sign		= blqSignMap [component];

		int i = 0;
		for (auto wave : waveList)
		{
			if (n < 3)	tideLoading[wave].amplitude	[index] = v[i] * sign;	// Positive east, north and upwards
			else		tideLoading[wave].phase		[index] = v[i];			// Phase signs won't change
			i++;
		}

		n++;

		if (n == 6)
			return true;
	}

	return false;
}

/** Read BLQ ocean/atmospheric tide loading parameters
*/
bool readBlq(
	string			file,		///< BLQ ocean tide loading parameter file
	Receiver&		rec,		///< Receiver
	E_LoadingType	type)		///< Type of loading (ocean, atmospheric)
{
	ifstream fileStream(file);
	if (!fileStream)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "BLQ file open error " << file;

		return false;
	}

	vector<E_TidalConstituent>	waveList;

	if		(type == +E_LoadingType::OCEAN)			{	waveList = acsConfig.otl_blq_col_order;	}
	else if	(type == +E_LoadingType::ATMOSPHERIC)	{	waveList = acsConfig.atl_blq_col_order;	}

	// station ID to upper case
	string id = rec.id;
	boost::to_upper(id);

	bool columnOrderFound = false;
	while (fileStream)
	{
		string line;
		getline(fileStream, line);

		char* buff = &line[0];

		if		(line.substr(0, 16) == "$$ COLUMN ORDER:")
		{
			string str = line.substr(16);
			boost::trim(str);
			vector<string> waveNames;
			boost::split(waveNames, str, boost::is_any_of(" "), boost::token_compress_on);

			waveList.clear();

			for (auto& waveName : waveNames)
			{
				try
				{
					E_TidalConstituent constituent = E_TidalConstituent::_from_string(waveName.c_str());
					waveList.push_back(constituent);
				}
				catch (...)
				{
					BOOST_LOG_TRIVIAL(warning)
					<< "Warning: Unknown tidal constituent in BLQ file header: " << waveName << "\n";
				}
			}

			columnOrderFound = true;

			continue;
		}
		else if	(line.substr(0, 2) == "$$"
				||line.size() < 2)
		{
			continue;
		}

		if (!columnOrderFound)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: Column order information not found in BLQ file header, (default) config is used" << "\n";
		}

		string name = line.substr(2, 4);
		boost::to_upper(name);
		if (name != id)
			continue;

		// read BLQ record for the station
		if		(type == +E_LoadingType::OCEAN)			{	auto componentList = acsConfig.otl_blq_row_order;	return readBlqRecord(fileStream, waveList, componentList, rec.otlDisplacement);		}
		else if	(type == +E_LoadingType::ATMOSPHERIC)	{	auto componentList = acsConfig.atl_blq_row_order;	return readBlqRecord(fileStream, waveList, componentList, rec.atlDisplacement);		}
		else
		{
			BOOST_LOG_TRIVIAL(error)
			<< __FUNCTION__ << ": Unspported file type" << "\n";

			return false;
		}
	}

	BOOST_LOG_TRIVIAL(warning)
	<< "Warning: No otl parameters found for " << rec.id << " in " << file;

	return false;
}

/** Read ocean pole load tide coefficients
*/
bool readOceanPoleCoeff(
	string			file)		///< Ocean pole tide coefficient file
{
	ifstream fileStream(file);
	if (!fileStream)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Ocean pole tide coefficient file open error " << file;

		return false;
	}

	double v[11];
	OceanPoleCoeff	oceanPoleCoeff;

	oceanPoleGrid.grid.clear();

	bool headerDone = false;

	while (fileStream)
	{
		string line;
		getline(fileStream, line);

		char* buff = &line[0];

		if (headerDone == false)
		{
			if		(to_lower_copy(line.substr(0, 30)) == "number_longitude_grid_points =")	{	oceanPoleGrid.numLonGrid	= str2num(buff, 30, 10);	continue;	}
			else if	(to_lower_copy(line.substr(0, 30)) == "first_longitude_degrees      =")	{	oceanPoleGrid.firstLonDeg	= str2num(buff, 30, 10);	continue;	}
			else if	(to_lower_copy(line.substr(0, 30)) == "last_longitude_degrees       =")	{	oceanPoleGrid.lastLonDeg	= str2num(buff, 30, 10);	continue;	}
			else if	(to_lower_copy(line.substr(0, 30)) == "longitude_step_degrees       =")	{	oceanPoleGrid.lonStepDeg	= str2num(buff, 30, 10);	continue;	}
			else if	(to_lower_copy(line.substr(0, 30)) == "number_latitude_grid_points  =")	{	oceanPoleGrid.numLatGrid	= str2num(buff, 30, 10);	continue;	}
			else if	(to_lower_copy(line.substr(0, 30)) == "first_latitude_degrees       =")	{	oceanPoleGrid.firstLatDeg	= str2num(buff, 30, 10);	continue;	}
			else if	(to_lower_copy(line.substr(0, 30)) == "last_latitude_degrees        =")	{	oceanPoleGrid.lastLatDeg	= str2num(buff, 30, 10);	continue;	}
			else if	(to_lower_copy(line.substr(0, 30)) == "latitude_step_degrees        =")	{	oceanPoleGrid.latStepDeg	= str2num(buff, 30, 10);	continue;	}
		}

		int found = sscanf(buff, "%lf %lf %lf %lf %lf %lf %lf %lf",
			&v[0],
			&v[1],
			&v[2],
			&v[3],
			&v[4],
			&v[5],
			&v[6],
			&v[7]);

		if (found != 8)
			continue;

		headerDone = true;

		oceanPoleCoeff.lon		= v[0];
		oceanPoleCoeff.lat		= v[1];
		oceanPoleCoeff.uR.u()	= v[2];
		oceanPoleCoeff.uI.u()	= v[3];
		oceanPoleCoeff.uR.n()	= v[4];
		oceanPoleCoeff.uI.n()	= v[5];
		oceanPoleCoeff.uR.e()	= v[6];
		oceanPoleCoeff.uI.e()	= v[7];

		oceanPoleGrid.grid.push_back(oceanPoleCoeff);
	}

	return true;
}

/** Step 1 of solid Earth tide computation: Corrections to be computed in the time domain (solar/lunar tides)
* See ref [1] 7.1.1
*/
Vector3d tideTimeDomain(
	Trace&				trace,		///< Trace to output to
	const Vector3d&		eu,			///< Unit vector of Up component
	const Vector3d&		rp,			///< Sun/Moon position vector in ECEF (m)
	double				GMp,		///< Sun/Moon gravitational constant
	const VectorPos&	pos)		///< Geodetic position of station {lat,lon} (rad)
{
	const double H3 = 0.292;
	const double L3 = 0.015;

	tracepdeex(4, trace, "\n%s: pos=%.3f %.3f\n", __FUNCTION__, pos.latDeg(), pos.lonDeg());

	double r = rp.norm();
	if (r == 0)
		return Vector3d::Zero();

	Vector3d ep = rp.normalized();

	double K2 = GMp / GM_Earth * SQR(RE_WGS84) * SQR(RE_WGS84) / (r * r * r);
	double K3 = K2 * RE_WGS84 / r;

	double latp = asin(ep[2]);
	double lonp = atan2(ep[1], ep[0]);

	double cosp = cos(latp);
	double sinl = sin(pos.lat());
	double cosl = cos(pos.lat());

	// step 1: in phase (degree 2)
	double p = (3 * SQR(sinl) - 1) / 2;
	double H2 = 0.6078 - 0.0006 * p;
	double L2 = 0.0847 + 0.0002 * p;
	double a = ep.dot(eu);
	double dp = K2 * 3 * L2 * a;
	double du = K2 * (H2 * (1.5 * a * a - 0.5) - 3 * L2 * SQR(a));

	// step 1: in phase (degree 3)
	dp += K3 * L3 * (7.5 * a * a - 1.5);
	du += K3 * (H3 * (2.5 * a * a * a - 1.5 * a) - L3 * (7.5 * a * a - 1.5) * a);

	// step 1: out-of-phase (only radial)
	du += 3.0 / 4.0 * 0.0025 * K2 * sin(2 * latp) * sin(2 * pos.lat())	* sin(		pos.lon() - lonp);
	du += 3.0 / 4.0 * 0.0022 * K2 * cosp * cosp * cosl * cosl			* sin(2 * (	pos.lon() - lonp));

	Vector3d dr	= dp * ep
				+ du * eu;

	tracepdeex(5, trace, "%s: dr=%.3f %.3f %.3f\n", __FUNCTION__, dr[0], dr[1], dr[2]);

	return dr;
}

/** Displacement by solid Earth tide
* See ref [1] 7.1.1
* Note: permanent deformation not removed, i.e. the tidal model "in principle contains a time-independent part so that
* the coordinates obtained by taking into account this model in the analysis will be 'conventional tide free' values."
* See ref above for details
*/
Vector3d tideSolidEarth(
	Trace&				trace,		///< Trace to output to
	GTime				time,		///< GPS time
	MjDateUt1			mjdUt1,		///< UT1 time in MJD
	const Vector3d&		rsun,		///< Sun position vector in ECEF (m)
	const Vector3d&		rmoon,		///< Moon position vector in ECEF (m)
	const VectorPos&	pos)		///< Geodetic position of station {lat,lon} (rad)
{
	tracepdeex(4, trace, "\n%s: pos=%.3f %.3f\n", __FUNCTION__, pos.latDeg(), pos.lonDeg());

	// step 1: time domain
	Matrix3d E;
	pos2enu(pos, E.data());

	Vector3d eu = E.row(2);

	Vector3d dr1	= tideTimeDomain(trace, eu, rsun,  GM_Sun,  pos);
	Vector3d dr2	= tideTimeDomain(trace, eu, rmoon, GM_Moon, pos);

	// step 2: frequency domain, only K1 radial
	double sin2l	= sin(2 * pos.lat());
	double gmst		= Sofa::iauGmst(mjdUt1, time);
	double du		= -0.012 * sin2l * sin(gmst + pos.lon());

	Vector3d dr		= dr1 + dr2 + du * eu;

	// permanent deformation not removed

	tracepdeex(5, trace, "%s: dr=%.3f %.3f %.3f\n", __FUNCTION__, dr[0], dr[1], dr[2]);

	return dr;
}

/** Displacement by solid Earth tide with DEHANTTIDEINEL
 * See ref [1] 7.1.1, [3], [7]
*/
Vector3d tideSolidEarthDehant(
	Trace&				trace,		///< Trace to output to
	GTime				time,		///< GPS time
	const Vector3d&		rsun,		///< Sun position vector in ECEF (m)
	const Vector3d&		rmoon,		///< Moon position vector in ECEF (m)
	const Vector3d&		recPos)		///< Receiver position in ECEF (m)
{
	tracepdeex(4, trace, "\n%s:\n", __FUNCTION__);

	MjDateUtc mjdUtc(time);
	MjDateTT mjdTT(time);
	double mjd = mjdUtc.to_double();
	int mjdInt = (int) mjd;
	const double fhr = (mjd - mjdInt) * 24;
	const double t = mjdTT.to_j2000() / 36525e0;

	vector<Vector3d>	recPosVec;
	recPosVec.push_back(recPos);

	vector<Vector3d>	drVec;

	iers2010::dehanttideinel_impl(t, fhr, rsun, rmoon, recPosVec, drVec);

	Vector3d dr = drVec.front();

	tracepdeex(5, trace, "%s: dr=%.3f %.3f %.3f\n", __FUNCTION__, dr[0], dr[1], dr[2]);

	return dr;
}

/** Displacement by ocean tide loading
* See ref [1] 7.1.2, [4]
*/
VectorEnu tideOceanLoad(
	Trace&		trace,				///< Trace to output to
	MjDateUt1	mjdUt1,				///< UT1 time in MJD
	TideMap&	otlDisplacement)	///< OTL displacements in amplitude and phase
{
	map<E_TidalConstituent, vector<double>> args =
	{
		{E_TidalConstituent::M2,	{1.40519E-4,  2.0, -2.0,  0.0,  0.00}},		// M2
		{E_TidalConstituent::S2,	{1.45444E-4,  0.0,  0.0,  0.0,  0.00}},		// S2
		{E_TidalConstituent::N2,	{1.37880E-4,  2.0, -3.0,  1.0,  0.00}},		// N2
		{E_TidalConstituent::K2,	{1.45842E-4,  2.0,  0.0,  0.0,  0.00}},		// K2
		{E_TidalConstituent::K1,	{0.72921E-4,  1.0,  0.0,  0.0,  0.25}},		// K1
		{E_TidalConstituent::O1,	{0.67598E-4,  1.0, -2.0,  0.0, -0.25}},		// O1
		{E_TidalConstituent::P1,	{0.72523E-4, -1.0,  0.0,  0.0, -0.25}},		// P1
		{E_TidalConstituent::Q1,	{0.64959E-4,  1.0, -3.0,  1.0, -0.25}},		// Q1
		{E_TidalConstituent::MF,	{0.53234E-5,  0.0,  2.0,  0.0,  0.00}},		// Mf
		{E_TidalConstituent::MM,	{0.26392E-5,  0.0,  1.0, -1.0,  0.00}},		// Mm
		{E_TidalConstituent::SSA,	{0.03982E-5,  2.0,  0.0,  0.0,  0.00}}		// Ssa
	};

	tracepdeex(4, trace, "\n%s:\n", __FUNCTION__);

	// angular argument, see ref [4] ARG2.F

	double mjd	= mjdUt1.to_double();
	int mjdInt	= (int) mjd;
	double sod	= (mjd - mjdInt) * S_IN_DAY;

	double days = mjdInt - mjd1975 + 1;
	double t	= (27392.500528 + 1.000000035 * days) / 36525.0;
	double t2	= t * t;
	double t3	= t * t * t;

	double a[5];
	a[0]= sod;
	a[1]= (279.69668	+ 36000.768930485	* t	+ 3.03E-4	* t2)						* D2R;	// H0
	a[2]= (270.434358	+ 481267.88314137	* t	- 0.001133	* t2	+ 1.9E-6	* t3)	* D2R;	// S0
	a[3]= (334.329653	+ 4069.0340329577	* t	- 0.010325	* t2	- 1.2E-5	* t3)	* D2R;	// P0
	a[4]= 2 * PI;


	// displacements by 11 constituents

	VectorEnu denu;
	for (auto& [wave, disp] : otlDisplacement)
	{
		double ang = 0;
		for (int i = 0; i < 5; i++)		ang		+= a[i] * args[wave][i];
		for (int j = 0; j < 3; j++)		denu[j]	+= disp.amplitude[j] * cos(ang - disp.phase[j] * D2R);
	}

	tracepdeex(5, trace, "%s: denu=%.3f %.3f %.3f\n", __FUNCTION__, denu[0], denu[1], denu[2]);

	return denu;
}

/** Displacement by ocean tide loading - adjustments
* See ref [1] eq 7.16, [6]
* Note: This model/function does not work well
*/
VectorEnu tideOceanLoadAdjusted(
	Trace&		trace,				///< Trace to output to
	GTime		time,				///< GPS time
	MjDateUt1	mjdUt1,				///< UT1 time in MJD
	TideMap&	otlDisplacement)	///< OTL displacements in amplitude and phase
{
	map<E_TidalConstituent, vector<double>> q =
	{
		{E_TidalConstituent::M2,	{ 2,  0,  0,  0,  0,  0,  0}},	// M2
		{E_TidalConstituent::S2,	{ 2,  2, -2,  0,  0,  0,  0}},	// S2
		{E_TidalConstituent::N2,	{ 2, -1,  0,  1,  0,  0,  0}},	// N2
		{E_TidalConstituent::K2,	{ 2,  2,  0,  0,  0,  0,  0}},	// K2
		{E_TidalConstituent::K1,	{ 1,  1,  0,  0,  0,  0,  1}},	// K1
		{E_TidalConstituent::O1,	{ 1, -1,  0,  0,  0,  0, -1}},	// O1
		{E_TidalConstituent::P1,	{ 1,  1, -2,  0,  0,  0, -1}},	// P1
		{E_TidalConstituent::Q1,	{ 1, -2,  0,  1,  0,  0, -1}},	// Q1
		{E_TidalConstituent::MF,	{ 0,  2,  0,  0,  0,  0,  0}},	// Mf
		{E_TidalConstituent::MM,	{ 0,  1,  0, -1,  0,  0,  0}},	// Mm
		{E_TidalConstituent::SSA,	{ 0,  0,  2,  0,  0,  0,  0}}	// Ssa
	};

	map<E_TidalConstituent, vector<double>> q5 =
	{
		{E_TidalConstituent::M2,	{-1}},		// M2
		{E_TidalConstituent::S2,	{ 0}},		// S2
		{E_TidalConstituent::N2,	{ 0}},		// N2
		{E_TidalConstituent::K2,	{ 1}},		// K2
		{E_TidalConstituent::K1,	{ 1, -1}},	// K1
		{E_TidalConstituent::O1,	{-1}},		// O1
		{E_TidalConstituent::P1,	{ 0}},		// P1
		{E_TidalConstituent::Q1,	{-1}},		// Q1
		{E_TidalConstituent::MF,	{ 1}},		// Mf
		{E_TidalConstituent::MM,	{ 0}},		// Mm
		{E_TidalConstituent::SSA,	{ 0}}		// Ssa
	};

	map<E_TidalConstituent, vector<double>> Q =
	{
		{E_TidalConstituent::M2,	{-0.037}},			// M2
		{E_TidalConstituent::S2,	{ 0.000}},			// S2
		{E_TidalConstituent::N2,	{ 0.000}},			// N2
		{E_TidalConstituent::K2,	{ 0.298}},			// K2
		{E_TidalConstituent::K1,	{ 0.136, -0.020}},	// K1
		{E_TidalConstituent::O1,	{ 0.189}},			// O1
		{E_TidalConstituent::P1,	{ 0.000}},			// P1
		{E_TidalConstituent::Q1,	{ 0.189}},			// Q1
		{E_TidalConstituent::MF,	{ 0.415}},			// Mf
		{E_TidalConstituent::MM,	{ 0.000}},			// Mm
		{E_TidalConstituent::SSA,	{ 0.000}}			// Ssa
	};

	tracepdeex(4, trace, "\n%s:\n", __FUNCTION__);

	double mjd	= mjdUt1.to_double();
	int mjdInt	= (int) mjd;
	double sod	= (mjd - mjdInt) * S_IN_DAY;

	MjDateUtc mjdUtc(time);
	double U	= mjdUtc.to_double() / 36525.0;
	double U2	= U * U;
	double U3	= U * U * U;

	MjDateTT mjdTT(time);
	double T	= mjdTT.to_double() / 36525.0;
	double T2	= T * T;

	double a[7];
	a[1] = (218.3166560 + 481267.881342 * T - 0.00133000 * T2 + 0.0040 * cos((133 * T +  29) * D2R))	* D2R;	// s
	a[2] = (280.4664490 +  36000.769822 * T + 0.00030360 * T2 + 0.0018 * cos(( 19 * T + 159) * D2R))	* D2R;	// h
	a[3] = ( 83.3532430 +   4069.013711 * T - 0.01032400 * T2)											* D2R;	// p
	a[4] = (234.9554440 +   1934.136185 * T - 0.00207600 * T2)											* D2R;	// N
	a[5] = (282.9373480 +      1.719533 * T + 0.00045970 * T2)											* D2R;	// ps
	a[6] = PI / 2;
	a[0] = (280.4606184 + 36000.7700536 * U + 0.00038793 * U2 - 0.0000000258 * U3 + sod / 240.0)		* D2R - a[1];	// tao

	VectorEnu denu;
	for (auto& [wave, disp] : otlDisplacement)
	{
		int n = Q[wave].size();
		double f	= 1;
		double u	= 0;
		double ang	= 0;

		for (int i = 0; i < 7; i++)		ang		+= fmod(a[i], PI2) * q[wave][i];
		for (int j = 0; j < n; j++)	{	f		*= (1 + Q[wave][j] * cos(a[4]));
										u		+= Q[wave][j] * sin(q5[wave][j] * a[4]);	}
		for (int k = 0; k < 3; k++)		denu[k]	+= disp.amplitude[k] * f * cos(ang + u - disp.phase[k] * D2R);
	}

	tracepdeex(5, trace, "%s: denu=%.3f %.3f %.3f\n", __FUNCTION__, denu[0], denu[1], denu[2]);

	return denu;
}

/** Displacement by ocean tide loading with HARDISP
* See ref [1] 7.1.2, [5], [7]
*/
VectorEnu tideOceanLoadHardisp(
	Trace&		trace,				///< Trace to output to
	GTime		time,				///< GPS time
	TideMap&	otlDisplacement)	///< OTL displacements in amplitude and phase
{
	tracepdeex(4, trace, "\n%s:\n", __FUNCTION__);

	double tamp	[3][ntin];
	double tph	[3][ntin];

	int i = 0;
	for (auto [wave, disp] : otlDisplacement)
	{
		tamp[0][i] =  otlDisplacement[wave].amplitude	.u();
		tph [0][i] = -otlDisplacement[wave].phase		.u();	// HARDISP.F: Change sign for phase, to be negative for lags
		tamp[1][i] =  otlDisplacement[wave].amplitude	.e();
		tph [1][i] = -otlDisplacement[wave].phase		.e();
		tamp[2][i] =  otlDisplacement[wave].amplitude	.n();
		tph [2][i] = -otlDisplacement[wave].phase		.n();

		if (i++ >= ntin)
			break;
	}

	double du[1];
	double dn[1];
	double de[1];

	iers2010::hisp::hardisp_impl(1, 0, tamp, tph, time, du, dn, de);

	VectorEnu denu;
	denu.e() = de[0];
	denu.n() = dn[0];
	denu.u() = du[0];

	tracepdeex(5, trace, "%s: denu=%.3f %.3f %.3f\n", __FUNCTION__, denu[0], denu[1], denu[2]);

	return denu;
}

/** Displacement by atmospheric tide loading
* See ref [1] 7.1.3
*/
VectorEnu tideAtmosLoad(
	Trace&		trace,				///< Trace to output to
	MjDateUt1	mjdUt1,				///< UT1 time in MJD
	TideMap&	atlDisplacement)	///< ATL displacements in amplitude and phase
{
	map<E_TidalConstituent, double> args =
	{
		{E_TidalConstituent::S2,	1.45444E-4},  // S2: 2 cycles/day == 4*PI/86400
		{E_TidalConstituent::S1,	0.72722E-4},  // S1: 1 cycle /day == 2*PI/86400
	};

	tracepdeex(4, trace, "\n%s:\n", __FUNCTION__);

	double mjd	= mjdUt1.to_double();
	int mjdInt	= (int) mjd;
	double sod	= (mjd - mjdInt) * S_IN_DAY;

	// displacements by 2 constituents - equation rederived w/ amplitude and phase

	VectorEnu denu = {};
	for (auto& [wave, disp] : atlDisplacement)
	{
		double ang = sod * args[wave];
		for (int j = 0; j < 3; j++)
		{
			denu[j]	+= disp.amplitude[j] * cos(ang - disp.phase[j] * D2R);
		}
	}

	tracepdeex(5, trace, "%s: denu=%.3f %.3f %.3f\n", __FUNCTION__, denu[0], denu[1], denu[2]);

	return denu;
}

/** IERS mean pole
* See ref [2.2] eq.21
* Eugene: This function will be gone in the future as IERS2010::meanPole() does the same thing
*/
void iersMeanPole(
	MjDateUt1	mjdUt1,				///< UT1 time in MJD
	double&		xp_bar,				///< Mean pole xp (mas)
	double&		yp_bar)				///< Mean pole yp (mas)
{
	double y	= mjdUt1.to_j2000() / 365.25;

	//* Note: The cubic + linear mean pole model is obsolete, and a secular polar motion is adopted as recommended by IERS Conventions (2010) Working Version 1.3.0
	if (0)
	{
		double y2	= y * y;
		double y3	= y * y * y;

		if (y < 3653.0 / 365.25)
		{
			/* until 2010.0 */
			xp_bar =  55.974	+ 1.8243 * y	+ 0.18413 * y2	+ 0.007024 * y3;	// (mas)
			yp_bar = 346.346	+ 1.7896 * y	- 0.10729 * y2	- 0.000908 * y3;
		}
		else
		{
			/* after 2010.0 */
			xp_bar =  23.513	+ 7.6141 * y;	// (mas)
			yp_bar = 358.891	- 0.6287 * y;
		}
	}

	xp_bar =  55.0	+ 1.677 * y;
	yp_bar = 320.5	- 3.460 * y;
}

/** Displacement by solid Earth pole tide
* See ref [1] 7.1.4
*/
VectorEnu tideSolidPole(
	Trace&				trace,		///< Trace to output to
	MjDateUt1			mjdUt1,		///< UT1 time in MJD
	const VectorPos&	pos,		///< Geodetic position of station {lat,lon} (rad)
	ERPValues&			erpv)		///< ERP values
{
	tracepdeex(4, trace, "\n%s: pos=%.3f %.3f\n", __FUNCTION__, pos.latDeg(), pos.lonDeg());

	// IERS mean pole (mas)
	double xp_bar;
	double yp_bar;
	iersMeanPole(mjdUt1, xp_bar, yp_bar);

	// ref [1] eq.7.24
	double m1 = + erpv.xp / AS2R	- xp_bar * 1E-3;	// (arcsec)
	double m2 = - erpv.yp / AS2R	+ yp_bar * 1E-3;

	double theta	= PI / 2 - pos.lat();	// co-latitude
	double cosl		= cos(pos.lon());
	double sinl		= sin(pos.lon());

	VectorEnu denu;
	denu[0] = + 9E-3 * cos(1 * theta) * (m1 * sinl - m2 * cosl);	// de = +Slambda (m), Slambda positive east
	denu[1] = + 9E-3 * cos(2 * theta) * (m1 * cosl + m2 * sinl);	// dn = -Stheta  (m), Stheta positive south
	denu[2] = -33E-3 * sin(2 * theta) * (m1 * cosl + m2 * sinl);	// du = +Sr      (m), Sr positive upwards

	tracepdeex(5, trace, "%s: denu=%.3f %.3f %.3f\n", __FUNCTION__, denu[0], denu[1], denu[2]);

	return denu;
}

/** Displacement by ocean pole tide
* See ref [1] 7.1.5
*/
VectorEnu tideOceanPole(
	Trace&				trace,		///< Trace to output to
	MjDateUt1			mjdUt1,		///< UT1 time in MJD
	const VectorPos&	pos,		///< Geodetic position of station {lat,lon} (rad)
	ERPValues&			erpv)		///< ERP values
{
	double latDeg = pos.latDeg();
	double lonDeg = pos.lonDeg();
	tracepdeex(4, trace, "\n%s: pos=%.3f %.3f\n", __FUNCTION__, latDeg, lonDeg);

	if (oceanPoleGrid.grid.empty())
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Ocean pole tide coefficients not available" << "\n";

		return VectorEnu();
	}

	int ilat = floor((latDeg - oceanPoleGrid.firstLatDeg) / oceanPoleGrid.latStepDeg);
	int ilon = floor((lonDeg - oceanPoleGrid.firstLonDeg) / oceanPoleGrid.lonStepDeg);
	double lat1 = ilat * oceanPoleGrid.latStepDeg + oceanPoleGrid.firstLatDeg;
	double lat2 = lat1 + oceanPoleGrid.latStepDeg;
	double lon1 = ilon * oceanPoleGrid.lonStepDeg + oceanPoleGrid.firstLonDeg;
	double lon2 = lon1 + oceanPoleGrid.lonStepDeg;

	vector<double> dlat;
	vector<double> dlon;
	dlat.push_back(lat1 - latDeg);		dlon.push_back(lon1 - lonDeg);
	dlat.push_back(lat2 - latDeg);		dlon.push_back(lon2 - lonDeg);

	double index[4] = {};
	index[0] = ilat * oceanPoleGrid.numLonGrid + ilon;
	index[1] = index[0] + 1;
	index[2] = index[0] + oceanPoleGrid.numLonGrid;
	index[3] = index[2] + 1;

	// wrap grid points along longitude (outside of grid)
	if	(lon1 < oceanPoleGrid.firstLonDeg)	{	index[0] += oceanPoleGrid.numLonGrid;	index[2] += oceanPoleGrid.numLonGrid;	}
	if	(lon2 > oceanPoleGrid. lastLonDeg)	{	index[1] -= oceanPoleGrid.numLonGrid;	index[3] -= oceanPoleGrid.numLonGrid;	}

	// nearest-neighbour extrapolation along latitude (outside of grid)
	if	(lat1 < oceanPoleGrid.firstLatDeg)	{	index[0] = index[2];					index[1] = index[3];					}
	if	(lat2 > oceanPoleGrid. lastLatDeg)	{	index[2] = index[0];					index[3] = index[1];					}

	vector<VectorEnu> uRLon1;
	vector<VectorEnu> uILon1;
	vector<VectorEnu> uRLon2;
	vector<VectorEnu> uILon2;
	for (int i = 0; i < 2; i++)
	{
		OceanPoleCoeff coeff1 = oceanPoleGrid.grid.at(index[i]);
		OceanPoleCoeff coeff2 = oceanPoleGrid.grid.at(index[i + 2]);

		uRLon1.push_back(coeff1.uR);		uRLon2.push_back(coeff2.uR);
		uILon1.push_back(coeff1.uI);		uILon2.push_back(coeff2.uI);
	}

	// linear interpolation along longitude
	vector<VectorEnu> uRLat;
	vector<VectorEnu> uILat;
	uRLat.push_back(interpolate(dlon, uRLon1));		uILat.push_back(interpolate(dlon, uILon1));
	uRLat.push_back(interpolate(dlon, uRLon2));		uILat.push_back(interpolate(dlon, uILon2));

	// linear interpolation along latitude
	VectorEnu uR = interpolate(dlat, uRLat);
	VectorEnu uI = interpolate(dlat, uILat);

	// IERS mean pole (mas)
	double xp_bar;
	double yp_bar;
	iersMeanPole(mjdUt1, xp_bar, yp_bar);

	// ref [1] eq.7.24
	double m1 = + erpv.xp / AS2R	- xp_bar * 1E-3;	// (arcsec)
	double m2 = - erpv.yp / AS2R	+ yp_bar * 1E-3;

	double K = 5.3394043696E+03;	// K = 4 * pi * G * aE * rhow * Hp / (3 * ge), Hp = sqrt(8 * pi / 15) * OmgE^2 * aE^4 / GM;
	double gamma2R = 0.6870;
	double gamma2I = 0.0036;


	VectorEnu denu =	(	uR * (m1 * gamma2R + m2 * gamma2I)
						+	uI * (m2 * gamma2R - m1 * gamma2I)) * AS2R * K;

	tracepdeex(5, trace, "%s: denu=%.3f %.3f %.3f\n", __FUNCTION__, denu[0], denu[1], denu[2]);

	return denu;
}

/* Tidal displacement by Earth tides
* See ref [1] 7.1
*/
void tideDisp(
	Trace&			trace,			///< Trace to output to
	GTime			time,			///< GPS time
	Receiver&		rec,			///< Receiver
	Vector3d&		recPos,			///< Receiver position in ECEF (m)
	Vector3d&		solid,			///< Displacement by solid Earth tide
	Vector3d&		otl,			///< Displacement by ocean tide
	Vector3d&		atl,			///< Displacement by atmospheric tide
	Vector3d&		spole,			///< Displacement by solid Earth pole tide
	Vector3d&		opole)			///< Displacement by ocean pole tide
{
	int lv = 3;

	string timeStr = time.to_string();

	tracepdeex(lv, trace, "\n\n%s: time=%s", __FUNCTION__, time.to_string().c_str());

	ERPValues erpv = getErp(nav.erp, time);

	MjDateUt1 mjdUt1(time, erpv.ut1Utc);

	if (recPos.isZero())
		return;

	VectorPos pos = ecef2pos(recPos);

	auto& recOpts = acsConfig.getRecOpts(rec.id);

	VectorEcef	rSun;
	VectorEcef	rMoon;

	if (recOpts.tideModels.solid)
	{
		// Sun and Moon positions in ECEF
		planetPosEcef(time, E_ThirdBody::MOON,	rMoon,	erpv);
		planetPosEcef(time, E_ThirdBody::SUN,	rSun,	erpv);
	}

	if (recOpts.tideModels.solid)											{																						solid	= tideSolidEarthDehant(trace, time, rSun, rMoon, recPos);	}
	if (recOpts.tideModels.otl		&&rec.otlDisplacement.empty() == false)	{	VectorEnu	denu	= tideOceanLoadHardisp	(trace, time,	rec.otlDisplacement);	otl		= (Vector3d) enu2ecef(pos, denu);							}
	if (recOpts.tideModels.atl		&&rec.atlDisplacement.empty() == false)	{	VectorEnu	denu	= tideAtmosLoad			(trace, mjdUt1,	rec.atlDisplacement);	atl		= (Vector3d) enu2ecef(pos, denu);							}
	if (recOpts.tideModels.spole)											{	VectorEnu	denu	= tideSolidPole			(trace, mjdUt1, pos, erpv);				spole	= (Vector3d) enu2ecef(pos, denu);							}
	if (recOpts.tideModels.opole)											{	VectorEnu	denu	= tideOceanPole			(trace, mjdUt1, pos, erpv);				opole	= (Vector3d) enu2ecef(pos, denu);							}

	tracepdeex(lv, trace, "\n%s   SOLID       %14.6f %14.6f %14.6f", timeStr.c_str(), solid	[0], solid	[1], solid	[2]);
	tracepdeex(lv, trace, "\n%s   OCEAN       %14.6f %14.6f %14.6f", timeStr.c_str(), otl	[0], otl	[1], otl	[2]);
	tracepdeex(lv, trace, "\n%s   ATMOSPHERIC %14.6f %14.6f %14.6f", timeStr.c_str(), atl	[0], atl	[1], atl	[2]);
	tracepdeex(lv, trace, "\n%s   SOLID POLE  %14.6f %14.6f %14.6f", timeStr.c_str(), spole	[0], spole	[1], spole	[2]);
	tracepdeex(lv, trace, "\n%s   OCEAN POLE  %14.6f %14.6f %14.6f", timeStr.c_str(), opole	[0], opole	[1], opole	[2]);
}
