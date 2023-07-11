

// #pragma GCC optimize ("O0")

#include <boost/algorithm/string.hpp>
#include <deque>
#include <map>

using boost::algorithm::to_lower;
using std::deque;
using std::map;

#include "eigenIncluder.hpp"
#include "acceleration.hpp"
#include "coordinates.hpp"
#include "instrument.hpp"
#include "navigation.hpp"
#include "orbitProp.hpp"
#include "constants.hpp"
#include "iers2010.hpp"
#include "planets.hpp"
#include "mongo.hpp"
#include "sinex.hpp"

using bsoncxx::builder::stream::open_array;
using bsoncxx::builder::stream::close_array;
using bsoncxx::builder::stream::document;



void OrbitIntegrator::computeCommon(
	const double dt)
{
	GTime		time = timeInit	+ dt;

	ERPValues	erpv = getErp(nav.erp, time);
	
	eci2ecef(time, erpv, eci2ecf, &deci2ecf);

	for (auto& body : E_ThirdBody::_values())
	{
		jplEphPos(nav.jplEph_ptr, time, body, planetsPosMap[body], &planetsVelMap[body]);
	}

	//Spherical Harmonics
	if (propagationOptions.egm_field)
	for (auto& once : {1})
	{
		if (egm.initialised == false)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: EGM data not initialised, check for valid egm file.";
			
			break;
		}
		
		Cnm = egm.gfctC;
		Snm = egm.gfctS;

		if (propagationOptions.solid_earth_tide)
		{
			MatrixXd Cnm_solid = MatrixXd::Zero(5, 5);
			MatrixXd Snm_solid = MatrixXd::Zero(5, 5);

			IERS2010::solidEarthTide1(
				eci2ecf * planetsPosMap[E_ThirdBody::SUN],
				eci2ecf * planetsPosMap[E_ThirdBody::MOON], 
				Cnm_solid, 
				Snm_solid);
			
			IERS2010::solidEarthTide2(time, erpv.ut1Utc, Cnm_solid, Snm_solid);

			Cnm.topLeftCorner(5, 5) += Cnm_solid;
			Snm.topLeftCorner(5, 5) += Snm_solid;
		}

		if (propagationOptions.pole_tide_ocean)
		{
			IERS2010::poleOceanTide(time, erpv.xp, erpv.yp, Cnm, Snm);
		}

		if (propagationOptions.pole_tide_solid)
		{
			IERS2010::poleSolidEarthTide(time, erpv.xp, erpv.yp, Cnm, Snm);
		}

		if (propagationOptions.ocean_tide)
		{
			Vector6d dood_arr = IERS2010::doodson(time, erpv.ut1Utc);

			MatrixXd Cnm_ocean = MatrixXd::Zero(Cnm.rows(), Cnm.cols());
			MatrixXd Snm_ocean = MatrixXd::Zero(Snm.rows(), Snm.cols());

			tide.getSPH(dood_arr, Cnm_ocean, Snm_ocean);

			Cnm += Cnm_ocean;
			Snm += Snm_ocean;
		}
	}
}


void OrbitIntegrator::computeAcceleration(
	const	OrbitState&	orbInit,
			Vector3d&	acc,
			Matrix3d&	dAdPos,
			Matrix3d&	dAdVel,
			MatrixXd&	dAdParam)
{
	Vector3d rsat = orbInit.pos;
	Vector3d vsat = orbInit.vel;
	
	const double posOffset = 1e-3;
	const double velOffset = 1e-6;

 	const	Vector3d satToSun	= planetsPosMap[E_ThirdBody::SUN] - rsat;
 	const	Vector3d ed 		= satToSun		.normalized();
 	const	Vector3d er 		= rsat			.normalized();
 	const	Vector3d ey 		= (ed.cross(er)).normalized();
 	const	Vector3d eb			= ed.cross(ey);

	if (propagationOptions.central_force)
	{
		Vector3d accCF = accelCentralForce(rsat, GM_values[E_ThirdBody::EARTH], &dAdPos);
	
// 		orbInit.componentsMap[E_Component::CENTRAL_FORCE] = accCF.norm();
		
		acc += accCF;
	}

	if (propagationOptions.planetary_perturbation)
	for (const auto& [planet, planetPos] : planetsPosMap)
	{
		if (planet == +E_ThirdBody::EARTH)
		{
			continue;
		}
		
		Vector3d accPlanet = accelSourcePoint(rsat, planetPos, GM_values[planet], &dAdPos);
	
// 		orbInit.componentsMap[E_Component::PLANETARY_PERTURBATION] = accPlanet.norm();
		
		acc += accPlanet;
	}

	if (propagationOptions.egm_field)
	{
		Vector3d rsatE	= eci2ecf * rsat;
		Vector3d accSPH	= accelSPH(rsatE, Cnm, Snm, propagationOptions.degree_max, egm.earthGravityConstant);
		
		for (int i = 0; i < 3; i++)
		{
			Vector3d offset = Vector3d::Zero();
			offset(i) = posOffset;
			
			Vector3d posPerturbed = rsatE + offset;
			Vector3d accPerturbed = accelSPH(posPerturbed, Cnm, Snm, propagationOptions.degree_max, egm.earthGravityConstant);
			
			dAdPos.col(i) += eci2ecf.transpose() * (accPerturbed - accSPH) / posOffset;
		}
	
// 		orbInit.componentsMap[E_Component::EGM] = accSPH.norm();
		
		acc += eci2ecf.transpose() * accSPH;
	}

	if (propagationOptions.indirect_J2)
	for (const auto body : {E_ThirdBody::SUN, E_ThirdBody::MOON})
	{
		Vector3d accJ2 = accelJ2(Cnm(2,0), eci2ecf, planetsPosMap[body], GM_values[body]);
	
// 		orbInit.componentsMap[E_Component::INDIRECT_J2] = accJ2.norm();
		
		acc += eci2ecf.transpose() * accJ2;
	}

	if (propagationOptions.general_relativity)
	{
		Vector3d accRel = IERS2010::relativity(		rsat,
													vsat,
													planetsPosMap[E_ThirdBody::SUN],
													planetsVelMap[E_ThirdBody::SUN],
													eci2ecf,
													deci2ecf);

		for (int i = 0; i < 3; i++)
		{
			Vector3d offset			= Vector3d::Zero();
			Vector3d acc_rel_part	= Vector3d::Zero();

			offset(i) = posOffset;
			acc_rel_part = IERS2010::relativity(	rsat + offset,
													vsat,
													planetsPosMap[E_ThirdBody::SUN],
													planetsVelMap[E_ThirdBody::SUN],
													eci2ecf,
													deci2ecf);

			dAdPos.col(i) += (acc_rel_part - accRel) / posOffset;

			offset(i) = velOffset;
			acc_rel_part = IERS2010::relativity(	rsat,
													vsat + offset,
													planetsPosMap[E_ThirdBody::SUN],
													planetsVelMap[E_ThirdBody::SUN],
													eci2ecf,
													deci2ecf);

			dAdVel.col(i) += (acc_rel_part - accRel) / velOffset;
		}
	
// 		orbInit.componentsMap[E_Component::GENERAL_RELATIVITY] = accRel.norm();

		acc += accRel;
	};

	if (propagationOptions.antenna_thrust)
	{
		Vector3d accAnt = orbInit.satPower / (orbInit.satMass * CLIGHT) * rsat.normalized();

		for (int i = 0; i < 3; i++)
		{
			Vector3d offset = Vector3d::Zero();
			offset(i) = posOffset;

			Vector3d acc_pert = orbInit.satPower / (orbInit.satMass * CLIGHT) * (rsat + offset).normalized();

			dAdPos.col(i) += (acc_pert - accAnt) / posOffset;
		}
	
// 		orbInit.componentsMap[E_Component::ANTENNA_THRUST] = accAnt.norm();

		acc += accAnt;
	}

	if (propagationOptions.solar_pressure_radiation)
	{
		double P0			= 4.56e-6;
		double Cr			= propagationOptions.srp_cr;
		double A			= propagationOptions.sat_area;
		double m			= orbInit.satMass;

		double eclipseFrac	= sunVisibility(rsat, planetsPosMap[E_ThirdBody::SUN], planetsPosMap[E_ThirdBody::MOON]);
		double scalar		= P0 * Cr * A / m * SQR(AU) * eclipseFrac / satToSun.squaredNorm();
		double R			= satToSun.norm();

		dAdPos += scalar * (1 / pow(R, 3) * Matrix3d::Identity() - 3 * satToSun * (satToSun.transpose() / pow(R, 5)));

		Vector3d accSrp = -1 * scalar * ed;
	
// 		orbInit.componentsMap[E_Component::SRP] = accSrp.norm();

		acc += accSrp;
	}

	if (propagationOptions.albedo)
	{
		double A			= propagationOptions.sat_area;
		double m			= orbInit.satMass;
		double E 			= 1367;
		double cBall		= 0.8;
		double alpha		= 0.3;
		double Ae			= M_PI * SQR(RE_WGS84);
		Vector3d rSun		= planetsPosMap[E_ThirdBody::SUN];

		double E_IR 		= (1 - alpha) / (4 * PI) * Ae * E / rsat.squaredNorm();

		double cos_		= rsat.dot(rSun) / (rsat.norm() * rSun.norm());
		double phi		= acos(cos_);
		double sin_		= sin(phi);
		double E_Vis	= 2 * alpha / (3 * SQR(PI)) * Ae * E / rsat.squaredNorm() * ((PI - phi) * cos_ + sin_);
	
		Vector3d accAlbedo = A / m * (E_Vis + E_IR)  / CLIGHT * cBall * rsat.normalized();
		
// 		orbInit.componentsMap[E_Component::ALBEDO] = accAlbedo.norm();
		
		acc += accAlbedo;
	}

	if (propagationOptions.empirical_dyb)
	{
		double scalef		= SQR(AU) / satToSun.squaredNorm();
		double eclipseFrac	= sunVisibility(rsat, planetsPosMap[E_ThirdBody::SUN], planetsPosMap[E_ThirdBody::MOON]);

		double srpScalar	= eclipseFrac * scalef;
		
		vector<Vector3d> axis = {ed, ey, eb};

		Vector3d& rSun = planetsPosMap[E_ThirdBody::SUN];
		Vector3d z = (rsat	.cross(vsat))	.normalized();
		Vector3d y = (z		.cross(rSun))	.normalized();
		Vector3d x = (y		.cross(z))		.normalized();

		double du = atan2(rsat.transpose() * y, rsat.transpose() * x);

		Vector3d accEmp = Vector3d::Zero();
	
		for (int i = 0; i < orbInit.empInput.size(); i++)
		{
			auto& empdata = orbInit.empInput[i];
			
			double scalar = 1;
			
			if (empdata.srpScaled)
			{
				scalar = srpScalar;
			}
			
			dAdParam.col(i) = axis[empdata.axisId] * scalar;
			
			if		(empdata.type == +E_TrigType::COS)		dAdParam.col(i) *= cos(empdata.deg * du);
			else if	(empdata.type == +E_TrigType::SIN)		dAdParam.col(i) *= sin(empdata.deg * du);
			
			accEmp += empdata.value * dAdParam.col(i);
		}
		
// 		orbInit.componentsMap[E_Component::EMPIRICAL] = accEmp.norm();

 		acc += accEmp;
 	}
}

void OrbitIntegrator::operator()(
	const	Orbits&	orbInits,
			Orbits&	orbUpdates,
	const	double	timeOffset)
{
	computeCommon(timeOffset);

	for (int i = 0; i < orbInits.size(); i++)
	{
		Matrix6d A			= Matrix6d::Zero();
		A.block<3,3>(0,3)	= Matrix3d::Identity();
	
		auto& orbInit	= orbInits	[i];
		auto& orbUpdate	= orbUpdates[i];
		
		int nparam = orbInit.empnum;

		Vector3d acc		= Vector3d::Zero();
		Matrix3d dAdPos		= Matrix3d::Zero();
		Matrix3d dAdVel		= Matrix3d::Zero();
 		MatrixXd dAdParam	= MatrixXd::Zero(3, nparam);

		computeAcceleration(orbInit, acc, dAdPos, dAdVel, dAdParam);
		
		A.block<3,3>(3,0) = dAdPos;
		A.block<3,3>(3,3) = dAdVel;
		
// 		orbUpdate.componentsMap	= orbInit.componentsMap;
		orbUpdate.pos			= orbInit.vel;
		orbUpdate.vel			= acc;
		orbUpdate.posVelSTM		= A * orbInit.posVelSTM;
		
      	orbUpdate.posVelSTM.bottomRightCorner(3, nparam) += dAdParam;
	}
};


void integrateOrbits(
	OrbitIntegrator&	orbitPropagator,
	Orbits&				orbits,
	double				integrationPeriod,
	double 				dtRequested)
{
	Instrument instrument(__FUNCTION__);
	
	if	( orbits.empty()
		||integrationPeriod == 0)
	{
		return;
	}
	
	double	dt			= dtRequested;
	int		steps		= round(integrationPeriod / dt);
	double	remainder	= fmod (integrationPeriod, dtRequested);
	
	if (remainder != 0) 
	{
		double newDt = integrationPeriod / steps;
		
		BOOST_LOG_TRIVIAL(warning) << "Warning: Time step adjusted from " << dt << " to " << newDt ;
		
		dt = newDt;
	}
	
	for (int i = 0; i < steps; i++)
	{
		double initTime	= i * dt;
		
		Orbits errors;
		orbitPropagator.odeIntegrator.do_step(boost::ref(orbitPropagator), orbits, initTime, dt, errors);
		
		for (auto error : errors)
		{
			double errorMag = error.pos.norm();
			if (errorMag > 0.001)
			{
				BOOST_LOG_TRIVIAL(warning) << " Integrator error " << errorMag << " greater than 1mm for " << error.Sat << " " << error.str;
			}
		}
	}
}

/** Get the estimated elements for a single satellite's orbit
 */
KFState getOrbitFromState(
	Trace&			trace,
	SatSys			Sat,
	string			str,
	const KFState&	kfState)
{
	map<KFKey, int> kfKeyMap;
	
	int index = 0;
	
	//find all satellite orbit related states related to this sat/str pair
	for (auto& [kfKey, unused] : kfState.kfIndexMap)
	{
		if	( kfKey.Sat != Sat
			||kfKey.str != str)
		{
			continue;
		}
		
		if	(  kfKey.type >= KF::FIRST_ORBIT_STATE
			&& kfKey.type <= KF::LAST_ORBIT_STATE)
		{
			kfKeyMap[kfKey] = index;
			index++;
		}
	}
	
	KFState subState;
	kfState.getSubState(kfKeyMap, subState);
	
	return subState;
}

Orbits prepareOrbits(
	Trace&			trace,
	const KFState&	kfState)
{
	Instrument instrument(__FUNCTION__);
	
	Orbits orbits;

	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		if	(  kfKey.type	!= KF::ORBIT
			|| kfKey.num	!= 0)
		{
			continue;
		}
		
		orbits.emplace_back(OrbitState{.Sat = kfKey.Sat, .str = kfKey.str});
	}

	for (auto& orbit : orbits)
	{
		auto& Sat		= orbit.Sat;
		auto& subState	= orbit.subState;
		
		subState = getOrbitFromState(trace, Sat, orbit.str, kfState);
		
		for (auto& [subKey, index] : subState.kfIndexMap)
		{
			if (subKey.type == KF::ORBIT)
			{
				continue;
			}
			
			switch (subKey.type)
			{
				case KF::EMP_DYB_0:		{ orbit.empInput.emplace_back(EMP{false,	0, subKey.num, E_TrigType::CONSTANT,	subState.x(index)}); break;}
				case KF::EMP_DYB_1C:	{ orbit.empInput.emplace_back(EMP{false,	1, subKey.num, E_TrigType::COS,			subState.x(index)}); break;}
				case KF::EMP_DYB_1S:	{ orbit.empInput.emplace_back(EMP{false,	1, subKey.num, E_TrigType::SIN,			subState.x(index)}); break;}
				case KF::EMP_DYB_2C:	{ orbit.empInput.emplace_back(EMP{false,	2, subKey.num, E_TrigType::COS,			subState.x(index)}); break;}
				case KF::EMP_DYB_2S:	{ orbit.empInput.emplace_back(EMP{false,	2, subKey.num, E_TrigType::SIN,			subState.x(index)}); break;}
				case KF::EMP_DYB_3C:	{ orbit.empInput.emplace_back(EMP{false,	3, subKey.num, E_TrigType::COS,			subState.x(index)}); break;}
				case KF::EMP_DYB_3S:	{ orbit.empInput.emplace_back(EMP{false,	3, subKey.num, E_TrigType::SIN,			subState.x(index)}); break;}
				case KF::EMP_DYB_4C:	{ orbit.empInput.emplace_back(EMP{false,	4, subKey.num, E_TrigType::COS,			subState.x(index)}); break;}
				case KF::EMP_DYB_4S:	{ orbit.empInput.emplace_back(EMP{false,	4, subKey.num, E_TrigType::SIN,			subState.x(index)}); break;}
				
				case KF::SRP_DYB_0:		{ orbit.empInput.emplace_back(EMP{true,		0, subKey.num, E_TrigType::CONSTANT,	subState.x(index)}); break;}
				case KF::SRP_DYB_1C:	{ orbit.empInput.emplace_back(EMP{true,		1, subKey.num, E_TrigType::COS,			subState.x(index)}); break;}
				case KF::SRP_DYB_1S:	{ orbit.empInput.emplace_back(EMP{true,		1, subKey.num, E_TrigType::SIN,			subState.x(index)}); break;}
				case KF::SRP_DYB_2C:	{ orbit.empInput.emplace_back(EMP{true,		2, subKey.num, E_TrigType::COS,			subState.x(index)}); break;}
				case KF::SRP_DYB_2S:	{ orbit.empInput.emplace_back(EMP{true,		2, subKey.num, E_TrigType::SIN,			subState.x(index)}); break;}
				case KF::SRP_DYB_3C:	{ orbit.empInput.emplace_back(EMP{true,		3, subKey.num, E_TrigType::COS,			subState.x(index)}); break;}
				case KF::SRP_DYB_3S:	{ orbit.empInput.emplace_back(EMP{true,		3, subKey.num, E_TrigType::SIN,			subState.x(index)}); break;}
				case KF::SRP_DYB_4C:	{ orbit.empInput.emplace_back(EMP{true,		4, subKey.num, E_TrigType::COS,			subState.x(index)}); break;}
				case KF::SRP_DYB_4S:	{ orbit.empInput.emplace_back(EMP{true,		4, subKey.num, E_TrigType::SIN,			subState.x(index)}); break;}
				default:				{ break;}
			}
		}
		
		orbit.pos		= orbit.subState.x.head	(3);
		orbit.vel		= orbit.subState.x.segment(3, 3);
		orbit.posVelSTM	= MatrixXd::Identity(6, orbit.subState.kfIndexMap.size());
		
		orbit.empnum	= orbit.empInput.size();
		
		string svn		= Sat.svn();

		orbit.satMass	= acsConfig.orbitPropagation.sat_mass;
		orbit.satPower	= acsConfig.orbitPropagation.sat_power;
		orbit.satArea	= acsConfig.orbitPropagation.sat_area;
		
		auto findPower_it = theSinex.map_satpowers.find(svn);
		if (findPower_it != theSinex.map_satpowers.end())
		{
			auto& [svn, satPowerMap] = *findPower_it;
			auto& [time, firstPower] = *satPowerMap.begin();
			
			orbit.satPower = firstPower.power;
		}

		auto findMass_it = theSinex.map_satmasses.find(svn);
		if (findMass_it != theSinex.map_satmasses.end())
		{
			auto& [svn, satMassMap] = *findMass_it;
			auto& [time, firstMass] = *satMassMap.begin();
			
			orbit.satMass = firstMass.mass;
		}
		
	}

	return orbits;
}

/** Apply the prediction using the filter's state transition
 */
void applyOrbits(
	Trace&			trace,
	Orbits&			orbits,
	const KFState&	kfState,
	GTime			time,
	double			tgap)
{
	Instrument instrument(__FUNCTION__);
	
	for (auto& orbit : orbits)
	{
		auto& subState = orbit.subState;
		
		Vector6d inertialState;
		inertialState << orbit.pos, orbit.vel;

		//Convert the absolute transition matrix to an identity matrix (already populated elsewhere) and stm-per-time matrix
		MatrixXd transition = orbit.posVelSTM - MatrixXd::Identity(orbit.posVelSTM.rows(), orbit.posVelSTM.cols());
		transition /= tgap;

		KFKey keyI;
		keyI.type	= KF::ORBIT;
		keyI.Sat	= orbit.Sat;
		keyI.str	= orbit.str;
		
		//use the calculated transition matrix for the main filter, and a also a test filter (substate)
		for (int i = 0; i < 6; i++)
		for (auto& [keyJ, indexJ] : subState.kfIndexMap)
		{
			keyI.num = i;
			
			double transferIJ = transition(i, indexJ);

			kfState	.setKFTransRate(keyI, keyJ, transferIJ);
			subState.setKFTransRate(keyI, keyJ, transferIJ);
		}
		
		//run a test run of the transition on the substate to see how much we miss by
		subState.stateTransition(trace, time);

		//calculate the state error between the linearly transitioned filter state, and the orbit propagated states.
		Vector6d deltaState			= inertialState 
									- subState.x.head(6);
		
		//We should ensure that the state transition is smooth, without bulk adjustments or 'setting' the state
		//add per-time adjusting state transitions to implement an addition of the shortfall delta calculated above
		Vector6d deltaStatePerSec	= deltaState / tgap;
		
		KFKey kfKey;
		kfKey.type	= KF::ORBIT;
		kfKey.Sat	= orbit.Sat;
		kfKey.str	= orbit.str;
		
		for (int i = 0; i < 6; i++)
		{
			kfKey.num	= i;
		
			kfState.setKFTransRate(kfKey, KFState::oneKey, deltaStatePerSec(i));
		}
	}
}

/** Use models to predict orbital motion and prepare state transition equations to implement those predictions in the filter
 */
void predictOrbits(
	Trace&			trace,
	const KFState&	kfState,
	GTime           time)
{
	Instrument instrument(__FUNCTION__);
	
	double tgap = (time - kfState.time).to_double();

	if (tgap == 0)
	{
		return;
	}
	
	Orbits orbits = prepareOrbits(trace, kfState);
	
	OrbitIntegrator integrator;
	integrator.timeInit				= kfState.time;
	integrator.propagationOptions	= acsConfig.orbitPropagation;

	integrateOrbits(integrator, orbits, tgap, acsConfig.orbitPropagation.integrator_time_step);

	applyOrbits(trace, orbits, kfState, time, tgap);
};



void addKFSatEMPStates( 
			KalmanModel&	model, 
	const	KFState&		kfState,
			KF				kfType,
			string			id)
{
	for (int i = 0; i < 3; i++)
	{
		InitialState init = initialStateFromConfig(model, i);
		
		if (init.estimate)
		{
			KFKey empKey;
			empKey.type		= kfType;
			empKey.Sat		= id.c_str();
			empKey.num		= i;
			empKey.comment	= init.comment;
			
			kfState.addKFState(empKey, init);
		}
	}
}

void outputOrbitConfig(
		KFState&	kfState,
		string		suffix)
{
	document satellites;
	
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	( key.type	!= KF::ORBIT
			||key.num	!= 0)
		{
			continue;
		}
		
		document satellite;
		
		for (KF type : 
			{	
				KF::ORBIT,
				KF::EMP_DYB_0,
				KF::EMP_DYB_1C,
				KF::EMP_DYB_1S,
				KF::EMP_DYB_2C,
				KF::EMP_DYB_2S,
				KF::EMP_DYB_3C,
				KF::EMP_DYB_3S,
				KF::EMP_DYB_4C,
				KF::EMP_DYB_4S,	
			})
		{
			int n = 3;
			if (type == +KF::ORBIT)
				n = 6;
			
			vector<double>	aprioriVec	(n);
			vector<double>	sigmaVec	(n);
			deque<bool>		estimatedVec(n);
			
			for (int i = 0; i < n; i++)
			{
				KFKey kfKey = key;
				kfKey.type	= type;
				kfKey.num	= i;
				
				double val = 0;
				double var = 0;
				bool found = kfState.getKFValue(kfKey, val, &var);
				
				aprioriVec	[i]	= val;
				sigmaVec	[i]	= sqrt(var);
				estimatedVec[i]	= found;
			}
			
			document state;
			
			{auto arr = state << "apriori_val"	<< open_array;	for (auto& val : aprioriVec)	arr	<< val;	arr << close_array;}
			{auto arr = state << "sigma"		<< open_array;	for (auto& val : sigmaVec)		arr	<< val;	arr << close_array;}
			{auto arr = state << "estimated"	<< open_array;	for (auto& val : estimatedVec)	arr	<< val;	arr << close_array;}
			
			string typeStr = type._to_string();
			to_lower(typeStr);
			
			satellite << typeStr << state;
		}
		
		satellites << key.Sat.id() << satellite;
	}
	
	document epoch_control;				epoch_control			<< "start_epoch"			<< kfState.time.to_string();
	document processing_options;		processing_options		<< "epoch_control"			<< epoch_control;
	
	document estimation_parameters;		estimation_parameters	<< "satellites"				<< satellites;
	
	document json;						json					<< "processing_options"		<< processing_options;
										json					<< "estimation_parameters"	<< estimation_parameters;
	
	string filename = acsConfig.orbit_ics_filename + suffix;
	
	PTime logtime = tsync;
	
	boost::posix_time::ptime	logptime	= boost::posix_time::from_time_t((time_t)logtime.bigTime);
	
	if ((GTime)logtime == GTime::noTime())
	{
		logptime = boost::posix_time::not_a_date_time;
	}
	
	replaceTimes(filename, logptime);
	
	std::ofstream output(filename);
	
	if (!output)
	{
		return;
	}
	
	output << bsoncxx::to_json(json) << std::endl;
}



