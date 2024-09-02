
#pragma once

#include "eigenIncluder.hpp"
#include "gTime.hpp"
#include "trace.hpp"


//forward declaration
struct ERP;
struct TideMap;

/** Structure for grid points of ocean pole load tide coefficients
*/
struct OceanPoleCoeff
{
	double		lat	= 0;		///< latitude of the grid point
	double		lon	= 0;		///< longitude of the grid point
	VectorEnu	uR	= {};		///< the real part of the ocean pole load tide coefficients
	VectorEnu	uI	= {};		///< the imaginary part of the ocean pole load tide coefficients
};

/** Structure for grid map of ocean pole load tide coefficients
*/
struct OceanPoleGrid
{
	int						numLonGrid	= 0;	///< number of longitude grids
	int						numLatGrid	= 0;	///< number of latitude grids
	double					firstLonDeg	= 0;	///< longitude of first grid point (degree)
	double					firstLatDeg	= 0;	///< latitude of first grid point (degree)
	double					lastLonDeg	= 0;	///< longitude of last grid point (degree)
	double					lastLatDeg	= 0;	///< latitude of last grid point (degree)
	double					lonStepDeg	= 0;	///< step size of longitude grids
	double					latStepDeg	= 0;	///< step size of latitude grids
	vector<OceanPoleCoeff>	grid		= {};	///< grid map of ocean pole load tide coefficients
};

bool readBlq(
	string			file,
	Receiver&		rec,
	E_LoadingType	type);

bool readOceanPoleCoeff(
	string			file);

Vector3d tideSolidEarth(
	Trace&				trace,
	GTime				time,
	MjDateUt1			mjdUt1,
	const Vector3d&		rsun,
	const Vector3d&		rmoon,
	const VectorPos&	pos);

Vector3d tideSolidEarthDehant(
	Trace&				trace,
	GTime				time,
	const Vector3d&		rsun,
	const Vector3d&		rmoon,
	const Vector3d&		recPos);

VectorEnu tideOceanLoad(
	Trace&				trace,
	MjDateUt1			mjdUt1,
	TideMap&			otlDisplacement);

VectorEnu tideOceanLoadAdjusted(
	Trace&				trace,
	GTime				time,
	MjDateUt1			mjdUt1,
	TideMap&			otlDisplacement);

VectorEnu tideOceanLoadHardisp(
	Trace&				trace,
	GTime				time,
	TideMap&			otlDisplacement);

VectorEnu tideAtmosLoad(
	Trace&				trace,
	MjDateUt1			mjdUt1,
	TideMap&			atlDisplacement);

VectorEnu tideSolidPole(
	Trace&				trace,
	MjDateUt1			mjdUt1,
	const VectorPos&	pos,
	ERPValues&			erpv);

VectorEnu tideOceanPole(
	Trace&				trace,
	MjDateUt1			mjdUt1,
	const VectorPos&	pos,
	ERPValues&			erpv);

void tideDisp(
	Trace&		trace,
	GTime		time,
	Receiver&	rec,
	Vector3d&	recPos,
	Vector3d&	solid,
	Vector3d&	olt,
	Vector3d&	alt,
	Vector3d&	spole,
	Vector3d&	opole);


