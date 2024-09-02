
#pragma once

#include <string>
#include <map>

using std::string;
using std::map;

#include "eigenIncluder.hpp"
#include "gTime.hpp"

/** Structure for variables related to the International Geomagnetic Reference Field
* 13th Generation International Geomagnetic Reference Field Schmidt semi-normalised spherical harmonic coefficients, degree n=1,13
* in units nanoTesla for IGRF and definitive DGRF main-field models (degree n=1,8 nanoTesla/year for secular variation (SV))
*/
struct GeomagMainField
{
	GeomagMainField()
	{

	};

    int			year;										///< model epoch
    int			maxDegree		= 13;						///< maximum degree and order
	MatrixXd	gnm				= MatrixXd::Zero(14, 14);	///< SH coefcients gnm
	MatrixXd	hnm				= MatrixXd::Zero(14, 14);	///< SH coefcients hnm
};

/** Structure for secular variation (SV) inherited from GeomagMainField
*/
struct GeomagSecularVariation : GeomagMainField
{
    int			yearEnd;									///< end of validity period == year+5
};

void readIGRF(
	string filename);

double decimalYear(
	GTime time);

bool getSHCoef(
	GTime				time,
	GeomagMainField&	igrfMF);

VectorEnu getGeomagIntensity(
	GTime				time,
	const VectorPos&	pos);

VectorEcef getGeomagIntensityEcef(
	GTime				time,
	const VectorPos&	pos);
