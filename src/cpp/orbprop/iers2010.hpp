
#pragma once

#include "eigenIncluder.hpp"
#include "gTime.hpp"

struct FundamentalNutationArgs : Array6d
{
	double& gmst;
	double& l;	
	double& l_prime;	
	double& f;	
	double& d;	
	double& omega;
	
	FundamentalNutationArgs(
		GTime	time,
		double	ut1_utc);
};

struct IERS2010
{
	static void PMGravi(
		GTime			time,
		double			ut1_utc,
		double&			x, 
		double&			y, 
		double&			ut1, 
		double&			lod);

	static void PMUTOcean(
		GTime			time,
		double			ut1_utc,
		double&			x, 
		double&			y, 
		double&			ut);
	
	static Array6d doodson(
		GTime			time,
		double			ut1_utc);

	static void solidEarthTide1(
		const Vector3d&	ITRFSun, 
		const Vector3d&	ITRFMoon, 
		MatrixXd&		Cnm, 
		MatrixXd&		Snm);
	
	static void solidEarthTide2(
		GTime			time,
		double			ut1_utc,
		MatrixXd&		Cnm, 
		MatrixXd&		Snm);

	static void poleSolidEarthTide(
		MjDateTT		mjdTT,
		const double	xp, 
		const double	yp, 
		MatrixXd&		Cnm, 
		MatrixXd&		Snm);
	
	static void poleOceanTide(
		MjDateTT		mjdTT,
		const double	xp, 
		const double	yp, 
		MatrixXd&		Cnm, 
		MatrixXd&		Snm);

	static Vector3d relativity(
		const	Vector3d&	posSat,
		const	Vector3d&	velSat,
		const	Vector3d&	posSun,
		const	Vector3d&	velSun,
		const	Matrix3d&	U,
		const	Matrix3d&	dU);
};

namespace iers2010
{
/// Compute the global total FCULa mapping function.
double fcul_a(double, double, double, double) noexcept;
/*
/// Computes the global total FCULb mapping function.
double fcul_b(double, double, double, double) noexcept;
*/
// Determine the total zenith delay following Mendes and Pavlis, 2004.
int fcul_zd_hpa(double, double, double, double, double, double &, double &, double &) noexcept;
};
