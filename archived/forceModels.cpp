

// #pragma GCC optimize ("O0")

#include <boost/numeric/odeint.hpp>       // odeint function definitions 

using namespace boost::numeric::odeint;


#include "eigenIncluder.hpp"
#include "forceModels.hpp"
#include "instrument.hpp"
#include "navigation.hpp"
#include "satRefSys.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "gravity.hpp"
#include "common.hpp"
#include "jplEph.hpp"
#include "trace.hpp"
#include "tides.hpp"
#include "enums.h"
#include "coordinates.hpp"

typedef Eigen::Vector<double, 48> VectorXSTMS;			//todo aaron, delete this

map<int, OrbitPropagator>	orbitPropagatorMap;

GravityModel OrbitPropagator::gravityModel;


/* Computes the fractional illumination of a spacecraft in the vicinity of the Earth assuming a cylindrical shadow model
*
*/
double Illumination(
	const Vector3d& 		rSat,			///< Spacecraft position vector [m]
	const Vector3d& 		rSun )			///< Sun position vector [m]
{                      
	Vector3d eSun = rSun / rSun.norm();    // Sun direction unit vector
	double s     	= rSat.dot(eSun);      // Projection of s/c position 

	/* Illumination factor:
	*  1: Spacecraft fully illuminated by the Sun
	*  0: Spacecraft in Earth shadow
	*/
	return ( ( s > 0 || (rSat - s * eSun).norm() > RE_WGS84 ) ?  1 : 0 );
}

/* Class type: The class of solar radiation pressure
*
*/
SolarRadPressure::SolarRadPressure(
	SRPPara 				paraSRP)
{
	mSRPPara		= paraSRP;
}

Vector3d SolarRadPressure::directSolarRadiationAcc(
	Trace&				trace,		 				///< Trace to output to
	double 				mjdTT,		 				///< Terrestrial time (modified Julian date)	
	const Vector3d& 	rSat)        				///< Satellite position vector, unit: m, m/s
{

	/* Relative position vector of spacecraft w.r.t. Sun
	*/
	Vector3d rSun;
	jplEphPos(nav.jplEph_ptr, mjdTT + JD2MJD, E_ThirdBody::SUN, rSun);

	// trace << "Calculated sun position: " << std::setw(14) << mjdTT << std::setw(14) << rSun.transpose() << std::endl;

	Vector3d rDis = rSat - rSun;
	double illumination = Illumination(rSat, rSun);
	
	switch (mSRPPara.srpMdlName)
	{
		case E_SRPModels::CANNONBALL:		return illumination * mSRPPara.srpCoef * (mSRPPara.srpArea / mSRPPara.satMass) * PSOL * (AU * AU) * rDis / pow(rDis.norm(), 3);	
		case E_SRPModels::BOXWING:			return Vector3d::Zero();			
		case E_SRPModels::ECOM:				return Vector3d::Zero();					
		case E_SRPModels::ECOM2:			return Vector3d::Zero();					
		default:							return Vector3d::Zero();
	}
}

Vector3d SolarRadPressure::indirectSolarRadiationAcc(
	Trace&				trace,		 					///< Trace to output to (similar to cout)
	double 				mjdTT,		 					///< Terrestrial time (modified Julian date)	
	const Vector3d& 	rSat)        					///< Satellite position vector, unit: m, m/s
{
	return Vector3d::Zero();
}

Vector3d antennarThrustAcc()
{
	return Vector3d::Zero();
}

Vector3d empiricalAcc()
{
	return Vector3d::Zero();
}

Vector3d manoeuvreAcc()
{
	return Vector3d::Zero();
}


/* Set options for force models in the propagator
*
*/
void OrbitPropagator::setPropOption(
	ForceModels				forceMdl)			
{
	/* Options from yaml file */
	propOpt.optEarthGravMdl.earthGravMdl		= E_GravMdl::GGM03S;
	
	/* Parameters from yaml file */
	propOpt.optEarthGravMdl.earthGravAccDeg.mMax = forceMdl.egmAccDeg;
	propOpt.optEarthGravMdl.earthGravAccDeg.nMax = forceMdl.egmAccOrd;
	propOpt.optEarthGravMdl.earthGravSTMDeg.mMax = forceMdl.egmSTMDeg;
	propOpt.optEarthGravMdl.earthGravSTMDeg.nMax = forceMdl.egmSTMOrd;

	propOpt.paraSRP.srpMdlName	= forceMdl.srp_model;
	propOpt.paraSRP.satMass		= forceMdl.sat_mass;
	propOpt.paraSRP.srpArea		= forceMdl.srp_area;
	propOpt.paraSRP.srpCoef		= forceMdl.srp_coef;
}	

/* initialise the propagator with time, state and necessary parameters
*
*/
void OrbitPropagator::init(
	Vector6d				rvSatECI,	
	double 					mjdUTC,
	double					leapSec,
	ERP*					erpSrc,
	EGMCoef					egmCoef)
{
	inertialState 		= rvSatECI;
	mMJDUTC 			= mjdUTC;
	erp 				= erpSrc;
	
	geterp(*erp, mMJDUTC, erpv);

	gravityModel = GravityModel(propOpt.optEarthGravMdl, egmCoef);

	if	(acsConfig.forceModels.solar_radiation_pressure)
	{
		solarRadPressure = SolarRadPressure(propOpt.paraSRP);
	}
}

void OrbitPropagator::update(
	double				mjdUTC)
{
	// update time
	mMJDUTC = mjdUTC;
	
	// update erp
	geterp(*erp, mMJDUTC, erpv);
	double dUTC_TAI	= -(19 + erpv.leaps);
	double xp		= erpv.xp;
	double yp		= erpv.yp;
	double dUT1_UTC	= erpv.ut1_utc;
	double lod		= erpv.lod;
	
	// update iers
	iers = IERS(dUT1_UTC, dUTC_TAI, xp, yp, lod);
	
	//update srp parameters
	solarRadPressure.mSRPPara = propOpt.paraSRP;
	
	//update third body positions
	double mjdTT = mMJDUTC + iers.TT_UTC() / 86400.0;
	for (int i = 0; i < E_ThirdBody::_size(); i++)
	{
		E_ThirdBody	body 		= E_ThirdBody::_values()[i];
		string		bodyName	= E_ThirdBody::_names()[i];
		
		if (acsConfig.forceModels.process_third_body[body] == false)
		{
			continue;
		}
		
		// Relative position vector of satellite w.r.t. point mass
		Vector3d thirdBodyPos;
		jplEphPos(nav.jplEph_ptr, mjdTT + JD2MJD, body, thirdBodyPos);

		thirdBodyPositionMap[body] = thirdBodyPos;
	}
}
	
/** Propagate orbits by ODE functor.
 * Used by RK - input is a 6 element inertial state and time, and output is the derivative of the state at the provided time.
 * This function requires the update function to be called first to set planetary ephemerides, erp values, etc.
 */
void OrbitPropagator::operator ()(
	Vector6d&		 inertialState,				 ///< Inertial position and velocity of satellite (m, m/s)
	Vector6d&		dInertialState,  		
	const double	mjdUTCinSec)
{
	Instrument instrument(__FUNCTION__);
	
	//Get sub vectors from the state
	Vector3d rSat = inertialState.head(3);
	Vector3d vSat = inertialState.tail(3);

	auto& trace = std::cout;
	
	double mjdUTC = mjdUTCinSec / 86400;


	//calculate acceleration components
	Vector3d aSat = Vector3d::Zero();
	
	
	bool bVarEq = false;
	if (acsConfig.forceModels.earth_gravity)
	{
		Instrument instrument("Grav");
		
		Vector3d earthgravityAcc = -GM_Earth * rSat.normalized() / rSat.squaredNorm();// = gravityModel.centralBodyGravityAcc(trace, mMJDUTC, erpv, rSat, mECI2ECEF, bVarEq);
		
// 		trace << "Calculated accleration due to the Earth's central body gravity: " << std::setw(14) << mMJDUTC << std::setw(14) << earthgravityAcc.transpose() << std::endl;

		aSat += earthgravityAcc;		
	}

	
	double mjdTT = iers.TT_UTC() / 86400.0 + mMJDUTC;
	for (int i = 0; i < E_ThirdBody::_size(); i++)
	{
		E_ThirdBody	body 		= E_ThirdBody::_values()[i];
		string		bodyName	= E_ThirdBody::_names()[i];
		
		if (acsConfig.forceModels.process_third_body[body] == false)
		{
			continue;
		}
		
		Vector3d& bodyPos = thirdBodyPositionMap[body];
		
		
		Vector3d thirdbodyAcc = accelPointMassGravity(trace, mjdTT, rSat, body, bodyPos);
		
// 		trace << "Calculated acceleration due to " << bodyName << "'s attraction: " << std::setw(14) << mMJDUTC << std::setw(14) << thirdbodyAcc.transpose() << std::endl;
		
		aSat += thirdbodyAcc;
	}
	
	if (acsConfig.forceModels.relativity_effect)
	{
		Instrument instrument("Rel");
		
		Vector3d relativityAcc = gravityModel.relativityEffectsAcc(trace, rSat, vSat);

// 		trace << "Calculated acceleration due to the relativity effect: " << std::setw(14) << mMJDUTC << std::setw(14) << relativityAcc.transpose() << std::endl;
		
		aSat += relativityAcc;
	}	

	if (acsConfig.forceModels.solar_radiation_pressure)
	{
		Instrument instrument("SRP");
		
		Vector3d directSRPAcc = solarRadPressure.directSolarRadiationAcc(trace, mMJDUTC, rSat);

// 		trace << "Calculated acceleration due to the direct solar radiation: " << std::setw(14) << mMJDUTC << std::setw(14) << directSRPAcc.transpose() << std::endl;
		
		aSat += directSRPAcc;
	}		
	
	// std::cout << "Total accelerations: " << mjdUTC << "  " << std::setw(14) << aSat.transpose() << std::endl;

	//set ODE output from sub vectors
	dInertialState.head(3) = vSat;
	dInertialState.tail(3) = aSat;
	
	
	// std::cout << "\n" << (aSat.dot(rSat.normalized()));
}

/** Observer, prints time and state when called (during integration)
*
*/
void obsvrSatMotion( 
	const Vector6d& x, 
	const double mjdUTC)
{
	// std::cout << std::endl;
	// std::cout << std::setw(8) << mjdUTC << std::setw(14) << x[0] << std::setw(14) << x[1] << std::setw(14) << x[2]
	// 		  << std::setw(14) << x[3] << std::setw(14) << x[4] << std::setw(14) << x[5] << std::setw(14) <<  std::endl;
}


void updateOrbits(
	Trace&			trace,
	KFState&		kfState,
	GTime           time)
{
	auto& dd  =  orbitPropagatorMap ;

	{
		//get current inertial states from the kfState
		for (auto& [kfKey, index] : kfState.kfIndexMap)
		{
			if	( kfKey.type	!= KF::SAT_POS
				||kfKey.num		!= 0)
			{
				continue;
			}
			
			auto& orbitPropagator = orbitPropagatorMap[kfKey.Sat];
			
			Vector6d rvECI;
			for (int i = 0; i < 3; i++)
			{
				KFKey posKey = kfKey;
				
				posKey.num = i;
				kfState.getKFValue(posKey, rvECI(0 + i));				
				
				posKey.type = KF::SAT_POS_RATE;
				kfState.getKFValue(posKey, rvECI(3 + i));		
			}
			
			double mjdUTC	= gpst2mjd(tsync);
			double leapSec	= nav.leaps;
			orbitPropagator.setPropOption(acsConfig.forceModels);
			orbitPropagator.init(rvECI, mjdUTC, leapSec, &nav.erp, nav.egm);
		}
	}
	


	// New function to update;

	for (auto& [satId, orbitPropagator] : orbitPropagatorMap)
	{
		SatSys Sat;
		Sat.fromHash(satId);
		
		orbitPropagator.dt = time - kfState.time;			//time indicates the current epoch, kfState.time indicates the last epoch
		
		double t0 = gpst2mjd(kfState.time)	* 86400;
		double t1 = gpst2mjd(time)			* 86400;
		
		if (orbitPropagator.dt == 0)
		{
			return;
		}
		
// 		orbitPropagator.stateTime = time;
		
		orbitPropagator.oldState = orbitPropagator.inertialState;
		
		double t_mid = (t0 + t1) / 2 / 86400;
		
	// 	orbitPropagator.updPropagator(t0);//time epoch before the orbital propagation

		std::cout << std::setprecision(2) << std::fixed;
		std::cout
		<< "ICRF coordinates before the orbital propagation step: " 
		<< std::setprecision(14) << orbitPropagator.inertialState.transpose() <<  std::endl;
		
		orbitPropagator.update(t_mid); //time epoch inside the integrator
		
			
		
		//Run the propagator using the functor
		
		if (acsConfig.forceModels.ode_integrator == +E_Integrator::RKF78)
		{
			typedef runge_kutta_fehlberg78<Vector6d>	rkf78;			// Error stepper, used to create the controlled stepper

			double errAbs = 1.0e-16; // Error bounds
			double errRel = 1.0e-13;
			
			auto controller = make_controlled(errAbs, errRel, rkf78());
			integrate_adaptive(controller, orbitPropagator, orbitPropagator.inertialState, t0, t1, orbitPropagator.dt);
		}
		
		
		
		std::cout 
		<< "ICRF coordinates after the orbital propagation step:  " 
		<< std::setprecision(14) << orbitPropagator.inertialState.transpose() <<  std::endl;


		orbitPropagator.update(gpst2mjd(time));//time epoch after the orbital propagation;

	}



	for (auto& [satId, orbitPropagator] : orbitPropagatorMap)
	{
		SatSys Sat;
		Sat.fromHash(satId);

		Vector3d rECI = orbitPropagator.inertialState.head(3);
		Vector3d vECI = orbitPropagator.inertialState.tail(3);
		
		Matrix6d stateTransition = Matrix6d::Identity();
		stateTransition.topRightCorner(3,3) = Matrix3d::Identity() * orbitPropagator.dt;
		
		Vector6d transitionedState = stateTransition * orbitPropagator.oldState;
		
		Vector6d missingDynamics = orbitPropagator.inertialState - transitionedState;
		
		for (int i = 0; i < 3; i++)
		{
			KFKey oneKey = {.type = KF::ONE};
			
			KFKey kfKey;
			kfKey.Sat = Sat;
			kfKey.num = i;
			
			kfKey.type = KF::SAT_POS;
			
			kfState.setKFTrans(kfKey, oneKey, missingDynamics(0 + i));
			
			kfKey.type = KF::SAT_POS_RATE;
			
			kfState.setKFTrans(kfKey, oneKey, missingDynamics(3 + i));
		}
	}
}
