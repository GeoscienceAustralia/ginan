#ifndef __GRAVITY_HPP__
#define __GRAVITY_HPP__

#include "eigenIncluder.hpp"
#include "streamTrace.hpp"
#include "jplEph.hpp"
#include "enums.h"
#include "erp.hpp"

#include <string>

using std::string;

struct IERS;

/** Struct to save Earth gravity coefficients
*/
struct EGMCoef
{
	MatrixXd	smn = MatrixXd::Zero(361, 361);
	MatrixXd	cmn = MatrixXd::Zero(361, 361);
};


MatrixXd Legendre(
	int 	n,		
	int 	m,		
	double 	phi);

MatrixXd LegendreD(
	int             m,		///< Maximum degree
	int             n,		///< Maximum order
	MatrixXd        pnm,    ///< Normalised Legendre polinomial matrix
	double          phi);   ///< Geocentric latitude in radian

struct EarthGravityDeg
{
	int		mMax = 15;
	int		nMax = 15;
};

struct EarthGravMdlOpt
{
	E_GravMdl			earthGravMdl = E_GravMdl::GGM03S;
	EarthGravityDeg		earthGravAccDeg;
	EarthGravityDeg		earthGravSTMDeg;		
};

/** The gravity model of the central body
*/
struct GravityModel
{
	GravityModel(){}
	
	GravityModel(
		EarthGravMdlOpt         gravMdlOpt,
		EGMCoef         		egmCoef);
	
	EarthGravityDeg mEarthGravAccDeg = {15, 15};
	EarthGravityDeg mEarthGravSTMDeg = { 2,  2};

	/* Computes the acceleration due to the central body gravity field of the central body
	*
	*/
	Vector3d centralBodyGravityAcc(
		Trace&                  trace,          ///< Trace to output to (similar to cout)
		const double            mjdUTC,
		ERPValues&				 erpv,
		const Vector3d&         rSat,           ///< Satellite position vector in the inertial system
		const Matrix3d&         mECI2BF,        ///< Transformation matrix from ECI to central body fixed system
		bool				 	bVarEq);		///< bVarEq = 1 if for the variational equation


	void solidEarthTidesCorrection(
		Trace&                  trace,
		double				    mjdUTC,			    ///< 
		EGMCoef&             	egmCoef,			///< Struct of Earth gravity coefficients
		IERS&		 		    iers,				///< Instance of IERS class
		const Vector3d&		    vecRAESun,			///< Rho, azimuth and altitude information of Sun
		const Vector3d&		    vecRAEMoon);		///< Rho, azimuth and altitude information of Moon
	
	void oceanTidesCorrection(
		Trace&                  trace,
		double				    mjdUTC,			    ///< 
		EGMCoef&             	egmCoef,			///< Struct of Earth gravity coefficients
		const Vector3d&		    vecRAESun,			///< Rho, azimuth and altitude information of Sun
		const Vector3d&		    vecRAEMoon);		///< Rho, azimuth and altitude information of Moon
	
	void correctEgmCoefficients(
		Trace&					trace,
		const double 			mjdUTC,
		ERPValues&				erpv,			///< xp, yp, ut1_utc, lod and leapsecond
		const Matrix3d&			mECI2BF);
	

	Vector3d relativityEffectsAcc(
		Trace&    				trace,	 		///< Trace to output to (similar to cout)
		const Vector3d& 		rSat,			///< Inertial position of satellite (m)
		const Vector3d& 		vSat);			///< Inertial velocity of satellite (m/s)	    

	E_GravMdl			gravModelType;
	
	EGMCoef				uncorrectedEgmCoef;
	EGMCoef				correctedEgmCoef;
};


Vector3d accelPointMassGravity(
	Trace&				trace,
	const double 		mjdTT,		
	const Vector3d& 	pos,
	E_ThirdBody			thirdBody,
	Vector3d&			thirdBodyPos);	


#endif
