

// #pragma GCC optimize ("O0")

#include <boost/numeric/odeint.hpp>       // odeint function definitions 

using namespace boost::numeric::odeint;


#include "eigenIncluder.hpp"
#include "forceModels.hpp"
#include "streamTrace.hpp"
#include "instrument.hpp"
#include "navigation.hpp"
#include "satRefSys.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "gravity.hpp"
#include "common.hpp"
#include "jplEph.hpp"
#include "tides.hpp"
#include "enums.h"

typedef Eigen::Vector<double, 48> VectorXSTMS;			//todo aaron, delete this

map<int, OrbitPropagator>	orbitPropagatorMap;

GravityModel OrbitPropagator::gravityModel;

/* Frac: Gives the fractional part of a number
*/
double Frac (double x)
{
	return x - floor(x);
}

/* Computes the Sun's geocentric position using a low precision analytical series
*/
Vector3d SunPos(
	double mjdTT)		///< Terrestrial time: modified Julian date
{
	/* Constants
	*/
	const double eps	= 23.43929111 * D2R;				// Obliquity of J2000 ecliptic
	const double T		= (mjdTT - mjdJ2000) / 36525.0;		// Julian cent. since J2000

	/* Mean anomaly, ecliptic longitude and radius
	*/
	double M = PI2 * Frac ( 0.9931267 + 99.9973583 * T);										// [rad]
	double L = PI2 * Frac ( 0.7859444 + M/PI2 + ( 6892 * sin(M) + 72 * sin(2 * M) ) / 1296e3 );	// [rad]
	double R = 149.619e9 - 2.499e9 * cos(M) - 0.021e9 * cos(2 * M);								// [m]

	/* Solar position vector [m] with respect to the mean equator and equinox of J2000 (EME2000, ICRF)
	*/
	return R_x(-eps) * R * Vector3d(cos(L), sin(L), 0);
}

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
	
	//update reference frame matrices
	eci2ecef_sofa(mjdUTC, iers, mECI2ECEF, mdECI2ECEF);
	
	//update egm coefficients for tides
	gravityModel.correctEgmCoefficients(std::cout, mjdUTC, erpv, mECI2ECEF);
}
	
Vector3d  OrbitVEQPropagator::calculateAccelNGradient(
	Trace&    				trace,	 			///< Trace to output to (similar to cout)
	const Vector3d& 		rSat,		 		///< Inertial position of satellite (m)
	const Vector3d& 		vSat,		 		///< Inertial velocity of satellite (m/s)
	Matrix3d&				mGradient,			///< Gradient (G=da/dr) in the ICRF/J2000 system
	Vector3d&				dadCr,       		///< Partials of acceleration w.r.t. to the solar radiation coefficient
	const Matrix3d& 		mECI2ECEF)	 		///< Transformation matrix from ECI coordinate to ECEF			
{
// 	bool bVarEq = true;
// 	Vector3d earthGravityAcc = gravityModel.centralBodyGravityAcc(trace, mMJDUTC, mERPv, rSat, mECI2ECEF, bVarEq);
// 	Vector3d drSat;
// 	const double dinc = 1;   // Position increment [m]
// 	Vector3d daSat;
// 
// 	/* Gradient
// 	*/
//   	for (int i = 0; i < 3; i++)
// 	{
//     	drSat = Vector3d::Zero();	
// 		drSat(i) = dinc;	// Set offset in i-th component of the position vector
//     	daSat = gravityModel.centralBodyGravityAcc (trace, mMJDUTC, mERPv, rSat + drSat, mECI2ECEF, bVarEq) -  earthGravityAcc;	// Acceleration difference
//     	mGradient.col(i) = daSat / dinc;	// Derivative with respect to i-th component
//   	}
// 
//   	/* Radiation pressure coefficient partials
// 	*/  
// 	double mjdTT = mIERS.TT_UTC(mMJDUTC) / 86400.0 + mMJDUTC;
// 	solarRadPressure.mSRPPara.srpCoef = 1;	
// 	dadCr = solarRadPressure.directSolarRadiationAcc(trace, mjdTT, rSat);
	Vector3d acc;
	return acc;
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

	// double erpv[4] = {};
	// geterp_from_utc(&nav.erp, mjdUTC, erpv);
	// double leapSec	= nav.leaps;
	// double dUT1_UTC = erpv[2];
	// double dUTC_TAI	= -(19 + leapSec);
	// double xp = erpv[0];
	// double yp = erpv[1];
	// double lod = erpv[3];
	// clIERS iersInstance;
	// iersInstance.Set(dUT1_UTC, dUTC_TAI, xp, yp, lod);
	// double mjdTT = mjdUTC + iersInstance.TT_UTC(mjdUTC) / 86400;


	// Matrix3d mECI2ECEF = Matrix3d::Identity();
	// Matrix3d mdECI2ECEF = Matrix3d::Identity();
	// eci2ecef_sofa(mjdUTC, iersInstance, mECI2ECEF, mdECI2ECEF);
	// std::cout << "mECI2ECEF: " << mjdUTC << "  " << std::setw(14) << mECI2ECEF << std::endl;
	// Vector3d aSat = calculateAcceleration(std::cout, mjdTT, rSat, vSat, mECI2ECEF, egm.cmn, egm.smn, 12, 12);
	// std::cout << "Accelerations: " << mjdUTC << "  " << std::setw(14) << aSat.transpose() << std::endl;
	
	
	// std::cout << "mECI2ECEF: " << orbitProp.mMJDUTC << "  " << std::setw(14) << mECI2ECEF << std::endl;

	
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
	
	
	std::cout << "\n" << (aSat.dot(rSat.normalized()));
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

/** ODE of variational equation to be solved, i.e. the derivative of the state vector and the state transition matrix
*
*/
// struct VarEqPropagator
// {
// 	void operator()( 
// 		const	VectorXSTMS&	rvPhiS, 
// 				VectorXSTMS&	drvPhiSdt, 
// 		const	double			mjdUTCinSec)
// 	{
// 		double mjdUTC = mjdUTCinSec / 86400;	// Time
// 
// 		Vector3d rSat = rvPhiS.segment(0, 3);	// Position components
// 		Vector3d vSat = rvPhiS.segment(3, 3);	// Velocity components
// 
// 		MatrixXd Phi = MatrixXd::Identity(6, 6);	// State transition matrix
// 		// for (int j = 0; j < 6; j++)
// 		// {
// 		// 	Phi.col(j) = rvPhiS.segment(6 * (j + 1), 6);
// 		// }
// 
// 		MatrixXd S = MatrixXd::Zero(6, 1);	// Sensitivity matrix
// 		// for (int j = 0; j < 1; j++)
// 		// {
// 		// 	S.col(j) = rvPhiS.segment(6 * (j + 7), 6);
// 		// }
// 
// 		Matrix3d mECI2ECEF	= Matrix3d::Identity();
// 		Matrix3d mdECI2ECEF	= Matrix3d::Identity();
// 		updPropagator(mjdUTC); //time epoch inside the integrator
// 		eci2ecef_sofa(mjdUTC, mIERS, mECI2ECEF, mdECI2ECEF);
// 
// 		Matrix3d mGradient;
// 		Vector3d dadCr;
// 		Vector3d aSat = calculateAccelNGradient(std::cout, rSat, vSat, mGradient, dadCr, mECI2ECEF);
// 
// 		/* Time derivative of state transition matrix
// 		*
// 		*/
// 		MatrixXd dfdy = MatrixXd::Zero(6, 6);
// 		for (int i = 0; i < 3; i++)
// 		for (int j = 0; j < 3; j++)
// 		{
// 			dfdy(i, 	j	 ) = 0;						// dv/dr(i, j)
// 			dfdy(i + 3, j	 ) = mGradient(i, j);		// da/dr(i, j)
// 			dfdy(i,     j + 3) = ( i == j ? 1 : 0 );	// dv/dv(i, j)
// 			dfdy(i + 3, j + 3) = 0;						// da/dv(i, j)
// 		}
// 		MatrixXd dPhi = dfdy * Phi;	// Time derivative of state transition matrix
// 
// 
// 		/* Time derivative of sensitivity matrix
// 		*
// 		*/
// 		MatrixXd dfdp(6, 1);
// 		for (int i = 0; i < 3; i++)
// 		{
// 			dfdp(i    ) = 0;							// dv/dCr(i)
// 			dfdp(i + 3) = dadCr(i);						// da/dCr(i)
// 		}
// 
// 		MatrixXd dS = MatrixXd::Zero(6, 1);
// 		dS = dfdy * S + dfdp;
// 
// 		/* Derivative of combined state vector and state transition matrix
// 		*
// 		*/
// 		for (int i = 0; i < 3; i++)
// 		{
// 			drvPhiSdt(i    ) = vSat(i);                    // dr/dt(i)
// 			drvPhiSdt(i + 3) = aSat(i);                    // dv/dt(i)
// 		}
// 		
// 		for (int i = 0; i < 6; i++)
// 		for (int j = 0; j < 6; j++)
// 		{
// 			drvPhiSdt(6 * (j + 1) + i  ) = dPhi(i, j);     // dPhi/dt(i,j)
// 		}
// 
// 		for (int i = 0; i < 6; i++)
// 		for (int j = 6; j < 7; j++)
// 		{
// 			drvPhiSdt(6 * (j + 1) + i) = dS(i, j - 6);     // dS/dt(i,j)
// 		}
// 	}
// };

void updateOrbits(
	Trace&			trace,
	KFState&		kfState,
	GTime           time)
{
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
	
	for (auto& [satId, orbitPropagator] : orbitPropagatorMap)
	{
		SatSys Sat;
		Sat.fromHash(satId);
		
		double dt = time - kfState.time;			//time indicates the current epoch, kfState.time indicates the last epoch
		
		double t0 = gpst2mjd(kfState.time)	* 86400;
		double t1 = gpst2mjd(time)			* 86400;
		
		if (dt == 0)
		{
			return;
		}
		
// 		orbitPropagator.stateTime = time;
		
		VectorXd oldState = orbitPropagator.inertialState;
		
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
// 			typedef controlled_runge_kutta<rkf78>		ctrl_rkf78;		// Controlled stepper: it's built on an error stepper and allows us to have the output at each internally defined (refined) timestep, via integrate_adaptive call

			double errAbs = 1.0e-16; // Error bounds
			double errRel = 1.0e-13;
			
			//integrate_adaptive(ctrl_rkf78(), OrbitPropagator(), rvECI, t0, t1, dt);
			auto controller = make_controlled(errAbs, errRel, rkf78());
			integrate_adaptive(controller, orbitPropagator, orbitPropagator.inertialState, t0, t1, dt);
		}
		
		
		
		std::cout 
		<< "ICRF coordinates after the orbital propagation step:  " 
		<< std::setprecision(14) << orbitPropagator.inertialState.transpose() <<  std::endl;
	// 	VectorXSTMS	rvPhiS	= kfState.xSTMSM;

	// 	if (0)
	// 	{
	// 		// typedef runge_kutta4<VectorXSTMS> rk4;
	// 		// integrate_const(rk4(), odeVarEquation, rvPhiS, t0, t1, dt);
	// 		integrate(VarEqPropagator(), rvPhiS, t0, t1, dt);
	// 		// typedef runge_kutta_dopri5<Vector6d> rk5;
	// 		// typedef controlled_runge_kutta<rk5> ctrl_rk5;
	// 		// double errAbs = 1.0e-10; // Error bounds
	// 		// double errRel = 1.0e-8;
	// 		// integrate_adaptive(ctrl_rk5(), odeVarEquation, rvPhiS, t0, t1, dt);
	// 		
	// 		std::cout << "ICRF coordinates before the variatioanal equation propagation: " << std::setprecision(14) << rvPhiS.head(3).transpose() <<  std::endl;
	// 		kfState.xSTMSM = rvPhiS;
	// 	}

	// 	kfState.x		= rvECI;
	// 	kfState.time	= time;


		// ECI to ECEF transformation

		// double erpv[4] = {};
		// geterp(&nav.erp, time + dt, erpv);
		// double mjdUTC = gpst2mjd(time + dt);
		// double leapSec	= nav.leaps;
		// double dUT1_UTC = erpv[2];
		// double dUTC_TAI	= -(19 + orbitProp.mLeapSec);
		// double xp = erpv[0];
		// double yp = erpv[1];
		// double lod = erpv[3];
		// clIERS iersInstance;
		// iersInstance.Set(dUT1_UTC, dUTC_TAI, xp, yp, lod);
		// double mjdTT = mjdUTC + iersInstance.TT_UTC(mjdUTC) / 86400;
		// Matrix3d mECI2ECEF = Matrix3d::Identity();
		// Matrix3d mdECI2ECEF = Matrix3d::Identity();
		// Vector6d rvECEF;
		// eci2ecefVec_sofa(mjdUTC, iersInstance, rvECI, rvECEF);
		// std::cout << "ITRF coordinates: " << std::endl << std::setprecision(14) << rvECEF.transpose() <<  std::endl;


	// 	Matrix3d mECI2ECEF	= Matrix3d::Identity();
	// 	Matrix3d mdECI2ECEF	= Matrix3d::Identity();
		orbitPropagator.update(gpst2mjd(time));//time epoch after the orbital propagation;
		
		Vector3d rECI = orbitPropagator.inertialState.head(3);
		Vector3d vECI = orbitPropagator.inertialState.tail(3);
// 		std::cout 
// 		<< "ICRF coordinates after the orbital propagation step:  " 
// 		<< std::setprecision(14) << rECI.transpose() << " " << vECI.transpose() <<  std::endl;
		
		Vector3d rECEF2;
		Vector3d vECEF2;
		eci2ecef_sofa(orbitPropagator.mMJDUTC, orbitPropagator.iers, rECI, vECI, rECEF2, vECEF2);
// 		std::cout 
// 		<< "ITRF coordinates after the orbital propagation step:  " 
// 		<< std::setprecision(14) << rECEF2.transpose() << " " << vECEF2.transpose() <<  std::endl;

// 		std::ofstream fileOPResults("./ex01/OPResults.txt", std::ios_base::app | std::ios_base::in);
// 		if (!fileOPResults)
// 		{
// 			std::cout << "Error openinng results file!\n";
// 			return;
// 		}
		
// 		fileOPResults << std::setprecision(6) << std::fixed;
		
		
		// fileOPResults << setw(16) << orbitProp.mMJDUTC << setw(16) << rvECI.transpose() << setw(16) << rvECEF2.transpose() << endl;
// 		fileOPResults << std::setw(16) << orbitPropagator.mMJDUTC << " " << rECEF2.transpose() << vECEF2.transpose() << std::endl;
		
		//get the change in state ready for use in the kalman filter's state transition
// 		orbitPropagator.stateDelta		= orbitPropagator.inertialState - oldState;
		
		//get the noise in the state delta according to uncertainty in the inputs, and propagation uncertainty for use in the filters' covariance transition
// 		orbitPropagator.stateDeltaNoise	= Matrix6d::Zero();
		
		Matrix6d stateTransition = Matrix6d::Identity();
		stateTransition.topRightCorner(3,3) = Matrix3d::Identity() * dt;
		
		Vector6d transitionedState = stateTransition * oldState;
		
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
