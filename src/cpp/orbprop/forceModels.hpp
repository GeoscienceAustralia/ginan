
#ifndef __FORCE_MODELS_HPP__
#define __FORCE_MODELS_HPP__

#include <string>
#include <map>

using std::string;
using std::map;

#include "eigenIncluder.hpp"
#include "streamTrace.hpp"
#include "satRefSys.hpp"
#include "acsConfig.hpp"
#include "gravity.hpp"
#include "algebra.hpp"
#include "jplEph.hpp"
#include "common.hpp"
#include "enums.h"

struct SRPPara
{
	double 					satMass = 0;						 ///< Satellite mass, unit: kg
	double					srpArea = 0;						 ///< SRP projected area, unit: m^2
	double					srpCoef = 0;						 ///< SRP coefficient (cannonball model)
	E_SRPModels				srpMdlName = E_SRPModels::CANNONBALL;
};

/*
* Class type: The class of solar radiation pressure
*/
struct SolarRadPressure
{
	SolarRadPressure(){}
	
	SolarRadPressure(
		SRPPara					paraSRP);
	
	SRPPara			mSRPPara;

	/*
	* Calculation of direct solar radiation pressure acceleration
	*/
	Vector3d directSolarRadiationAcc(
		Trace&				trace,		  ///< Trace to output to (similar to cout)
		const double 		mjdTT,		  ///< Terrestrial time (modified Julian date)	
		const Vector3d& 	rSat);        ///< Satellite position vector, unit: m, m/s
	
	/*
	* Calculation of indirect solar radiation pressure acceleration
	*/		
	Vector3d indirectSolarRadiationAcc(
		Trace&				trace,		  ///< Trace to output to (similar to cout)
		const double 		mjdTT,		  ///< Terrestrial time (modified Julian date)	
		const Vector3d& 	rSat);        ///< Satellite position vector, unit: m, m/s
};

Vector3d empiricalAcc();
Vector3d manoeuvreAcc();

struct PropOpt
{
	EarthGravMdlOpt			optEarthGravMdl;
	SRPPara					paraSRP;
};

struct OrbitPropagator
{
	OrbitPropagator(){}
	
	void operator()(
		Vector6d&		 inertialState,
		Vector6d&		dInertialState,	
		const double	mjdUTCinSec);
	
	string		traceFilename;
	string		id;
	
	PropOpt		propOpt;
	
	double 		mMJDUTC;		///< UTC modified Julian date
	
	Vector6d	inertialState;
// 	Vector6d	stateDelta;
// 	Matrix6d	stateDeltaNoise;
	
// 	GTime		stateTime;
	
	IERS		iers;	 		///< Instance of IERS class
	ERP*		erp;			///< Earth rotational parameters
	ERPValues 	erpv;			///< Earth rotational parameters (xp, yp, ut1_utc, lod, leapsec) for the current epoch
	EGMCoef		egmCoef;		///< Earth model coefficients

	Matrix3d mECI2ECEF	= Matrix3d::Identity();
	Matrix3d mdECI2ECEF;
	
	void setPropOption(
		ForceModels				forceMdl);	

	void init(
		Vector6d				rSatECI,
		double 					mjdUTC				= 0,
		double					leapSec				= 0,
		ERP*					erpSrc				= nullptr,
		EGMCoef					egmCoe				= {});

	void update(
		double					mjdUTC);		///< mjdUTC at the current moment

	Vector3d  calculateAcceleration(
		Trace&    				trace,	 		///< Trace to output to (similar to cout)
		const Vector3d& 		rSat,		 	///< Inertial position of satellite (m)
		const Vector3d& 		vSat,		 	///< Inertial velocity of satellite (m/s)
		const Matrix3d& 		mECI2ECEF); 	///< Transformation matrix from ECI coordinate to ECEF

	static GravityModel			gravityModel;
	
	SolarRadPressure		solarRadPressure;
	
	map<E_ThirdBody, Vector3d>	thirdBodyPositionMap;
};

struct OrbitVEQPropagator
{
	VectorXd	xSTMSM = VectorXd::Zero(48);			///< State, stm and sm
	GravityModel			gravityModel;
	IERS		mIERS;	 		///< Instance of IERS class
	ERP*		mERP;			///< Earth rotational parameters
	ERPData		mERPv;			///< Earth rotational parameters (xp, yp, ut1_utc, lod, leapsec) for the current epoch
	
	VectorXd	mRVPhiS;		///< Concencated vector of position, velocity, state transition matrix and sensitivity matrix
	
	Vector3d  calculateAccelNGradient(
		Trace&					trace,	 		///< Trace to output to (similar to cout)
		const Vector3d& 		rSat,			///< Inertial position of satellite (m)
		const Vector3d& 		vSat,			///< Inertial velocity of satellite (m/s)
		Matrix3d&				mGradient,		///< Gradient (G=da/dr) in the ICRF/J2000 system
		Vector3d&				dadCR,			///< Partials of acceleration w.r.t. to the solar radiation coefficient
		const Matrix3d& 		mECI2ECEF); 	///< Transformation matrix from ECI coordinate to ECEF			
};


void updateOrbits(
	Trace&			trace,
	KFState&		kfState,
	GTime           time);

extern map<int, OrbitPropagator>	orbitPropagatorMap;

#endif
