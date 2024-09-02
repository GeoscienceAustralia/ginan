
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

/**
 */
Architecture Orbit_Integrator__()
{

}

#include <boost/algorithm/string.hpp>
#include <deque>
#include <map>

#ifdef ENABLE_PARALLELISATION
	#include "omp.h"
#endif

using boost::algorithm::to_lower;
using std::deque;
using std::map;

#include "interactiveTerminal.hpp"
#include "inputsOutputs.hpp"
#include "oceanPoleTide.hpp"
#include "eigenIncluder.hpp"
#include "acceleration.hpp"
#include "coordinates.hpp"
#include "staticField.hpp"
#include "navigation.hpp"
#include "orbitProp.hpp"
#include "constants.hpp"
#include "tideCoeff.hpp"
#include "attitude.hpp"
#include "iers2010.hpp"
#include "boxwing.hpp"
#include "planets.hpp"
#include "mongo.hpp"
#include "sinex.hpp"
#include "enums.h"
#include "aod.hpp"

#include "ubxDecoder.hpp"

using bsoncxx::builder::stream::open_array;
using bsoncxx::builder::stream::close_array;
using bsoncxx::builder::stream::document;

bool queryVectorElement(
	vector<bool>&	vec,
	size_t			index)
{
	if (index < vec.size())	{		return vec[index];		}
	else 					{		return vec.back();		}
}


void OrbitIntegrator::computeCommon(
	GTime		time)
{
	ERPValues	erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapper(time, erpv);
	eci2ecf		= frameSwapper.i2t_mat;
	deci2ecf	= frameSwapper.di2t_mat;	//todo aaron, just fs this instead of matrices?

	for (auto& body : E_ThirdBody::_values())
	{
		jplEphPos(nav.jplEph_ptr, time, body, planetsPosMap[body], &planetsVelMap[body]);
	}

	Array6d dood_arr = IERS2010::doodson(time, erpv.ut1Utc);

	if (acsConfig.propagationOptions.egm_field)
	for (auto& once : {1})
	{
		if (egm.initialised == false)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: EGM data not initialised, check for valid egm file";

			break;
		}

		Cnm = egm.gfctC;
		Snm = egm.gfctS;
		if (acsConfig.propagationOptions.solid_earth_tide)
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

		if (acsConfig.propagationOptions.pole_tide_ocean)
		{
			MatrixXd Cnm_poleTide = MatrixXd::Zero(Cnm.rows(), Cnm.cols());
			MatrixXd Snm_poleTide = MatrixXd::Zero(Snm.rows(), Snm.cols());

			if (oceanPoleTide.initialized)
			{
				double xpv;
				double ypv;
				IERS2010::meanPole(time, xpv, ypv);

				double m1 = +(erpv.xp / AS2R - xpv / 1000);
				double m2 = -(erpv.yp / AS2R - ypv / 1000);

				oceanPoleTide.estimate(m1, m2, Cnm_poleTide, Snm_poleTide);
			}
			else
			{
				IERS2010::poleOceanTide(time, erpv.xp, erpv.yp, Cnm_poleTide, Snm_poleTide);
			}

			Cnm += Cnm_poleTide;
			Snm += Snm_poleTide;
		}

		if (acsConfig.propagationOptions.aod)
		{
			MatrixXd Cnm_aod;
			MatrixXd Snm_aod;

			aod.interpolate(time, Cnm_aod, Snm_aod);

			Cnm += Cnm_aod;
			Snm += Snm_aod;
		}

		if (acsConfig.propagationOptions.pole_tide_solid)
		{
			IERS2010::poleSolidEarthTide(time, erpv.xp, erpv.yp, Cnm, Snm);
		}

		if (acsConfig.propagationOptions.ocean_tide)
		{
			MatrixXd Cnm_ocean = MatrixXd::Zero(Cnm.rows(), Cnm.cols());
			MatrixXd Snm_ocean = MatrixXd::Zero(Snm.rows(), Snm.cols());

			oceanTide.getSPH(dood_arr, Cnm_ocean, Snm_ocean);

			Cnm += Cnm_ocean;
			Snm += Snm_ocean;
		}

		if (acsConfig.propagationOptions.atm_tide)
		{
			MatrixXd Cnm_atm = MatrixXd::Zero(Cnm.rows(), Cnm.cols());
			MatrixXd Snm_atm = MatrixXd::Zero(Snm.rows(), Snm.cols());

			atmosphericTide.getSPH(dood_arr, Cnm_atm, Snm_atm);

			Cnm += Cnm_atm;
			Snm += Snm_atm;
		}
	}
}


void OrbitIntegrator::computeAcceleration(
	const	OrbitState&	orbInit,
			Vector3d&	acc,
			Matrix3d&	dAdPos,
			Matrix3d&	dAdVel,
			MatrixXd&	dAdParam,
	const	GTime		time)
{
	auto trace = getTraceFile(nav.satNavMap[orbInit.Sat]);

	trace << "\n" << "Computing accelerations at " << time;

	Vector3d rSat = orbInit.pos;
	Vector3d vSat = orbInit.vel;

	const double posOffset = 1e-3;
	const double velOffset = 1e-6;

	const	Vector3d satToSun	= planetsPosMap[E_ThirdBody::SUN] - rSat;
	const	Vector3d ed			= satToSun						.normalized();
	const	Vector3d er			= rSat							.normalized();
	const	Vector3d ey			= ed.cross(er)					.normalized();
	const	Vector3d eb			= ed.cross(ey)					.normalized();
	const	Vector3d en			= er.cross(vSat)				.normalized();
	const	Vector3d et			= en.cross(er)					.normalized();
	const	Vector3d ep			= ed.cross(Vector3d::UnitZ())	.normalized();
	const	Vector3d eq			= ed.cross(ep)					.normalized();

	if (acsConfig.propagationOptions.central_force)
	{
		Vector3d accCF = accelCentralForce(rSat, GM_values[E_ThirdBody::EARTH], &dAdPos);
        acc += accCF;

		orbInit.componentsMap[E_Component::CENTRAL_FORCE] = accCF.norm();
	}

	{
		Vector3d accPlanets = Vector3d::Zero();

		for (auto& planet : orbInit.planetary_perturbations)
		{
			if (planet == +E_ThirdBody::EARTH)
			{
				continue;
			}

			auto& planetPos = planetsPosMap[planet];

			Vector3d accPlanet = accelSourcePoint(rSat, planetPos, GM_values[planet], &dAdPos);

			accPlanets += accPlanet;
		}
        acc += accPlanets;

		orbInit.componentsMap[E_Component::PLANETARY_PERTURBATION] = accPlanets.norm();
	}

	if (acsConfig.propagationOptions.egm_field)
	{
		Vector3d rsatE	= eci2ecf * rSat;
		Vector3d accSPH	= accelSPH(rsatE, Cnm, Snm, acsConfig.propagationOptions.egm_degree, egm.earthGravityConstant);

		for (int i = 0; i < 3; i++)
		{
			Vector3d offset = Vector3d::Zero();
			offset(i) = posOffset;

			Vector3d posPerturbed = rsatE + offset;
			Vector3d accPerturbed = accelSPH(posPerturbed, Cnm, Snm, acsConfig.propagationOptions.egm_degree, egm.earthGravityConstant);

			dAdPos.col(i) += eci2ecf.transpose() * (accPerturbed - accSPH) / posOffset;
		}
        acc += eci2ecf.transpose() * accSPH;

		orbInit.componentsMap[E_Component::EGM] = accSPH.norm();
	}



	if	( acsConfig.propagationOptions.egm_field
		&&acsConfig.propagationOptions.indirect_J2)
	for (const auto body : {E_ThirdBody::SUN, E_ThirdBody::MOON})
	{
		Vector3d accJ2 = accelJ2(Cnm(2,0), eci2ecf, planetsPosMap[body], GM_values[body]);
        acc += eci2ecf.transpose() * accJ2;

		orbInit.componentsMap[E_Component::INDIRECT_J2] = accJ2.norm();
	}

	if (acsConfig.propagationOptions.general_relativity)
	{
		Vector3d accRel = IERS2010::relativity(		rSat,
													vSat,
													planetsPosMap[E_ThirdBody::SUN],
													planetsVelMap[E_ThirdBody::SUN],
													eci2ecf,
													deci2ecf);

		for (int i = 0; i < 3; i++)
		{
			Vector3d offset			= Vector3d::Zero();
			Vector3d acc_rel_part	= Vector3d::Zero();

			offset(i) = posOffset;
			acc_rel_part = IERS2010::relativity(	rSat + offset,
													vSat,
													planetsPosMap[E_ThirdBody::SUN],
													planetsVelMap[E_ThirdBody::SUN],
													eci2ecf,
													deci2ecf);

			dAdPos.col(i) += (acc_rel_part - accRel) / posOffset;

			offset(i) = velOffset;
			acc_rel_part = IERS2010::relativity(	rSat,
													vSat + offset,
													planetsPosMap[E_ThirdBody::SUN],
													planetsVelMap[E_ThirdBody::SUN],
													eci2ecf,
													deci2ecf);

			dAdVel.col(i) += (acc_rel_part - accRel) / velOffset;
		}
        acc += accRel;

		orbInit.componentsMap[E_Component::GENERAL_RELATIVITY] = accRel.norm();
	};

	if (orbInit.antenna_thrust)
	{
		Vector3d accAnt = orbInit.power / (orbInit.mass * CLIGHT) * rSat.normalized();

		for (int i = 0; i < 3; i++)
		{
			Vector3d offset = Vector3d::Zero();
			offset(i) = posOffset;

			Vector3d acc_pert = orbInit.power / (orbInit.mass * CLIGHT) * (rSat + offset).normalized();

			dAdPos.col(i) += (acc_pert - accAnt) / posOffset;
		}
        acc += accAnt;

		orbInit.componentsMap[E_Component::ANTENNA_THRUST] = accAnt.norm();
	}

	switch (orbInit.solar_radiation_pressure)
	{
		case E_SRPModel::CANNONBALL:
		{
			double P0			= 4.56e-6;
			double Cr			= orbInit.srp_cr;
			double A			= orbInit.area;
			double m			= orbInit.mass;

			double eclipseFrac	= sunVisibility(rSat, planetsPosMap[E_ThirdBody::SUN], planetsPosMap[E_ThirdBody::MOON]);
			double scalar		= P0 * Cr * A / m * SQR(AU) * eclipseFrac / satToSun.squaredNorm();
			double R			= satToSun.norm();

			dAdPos += scalar * (1 / pow(R, 3) * Matrix3d::Identity() - 3 * satToSun * (satToSun.transpose() / pow(R, 5)));
			Vector3d accSrp = -1 * scalar * ed;
			acc += accSrp;

            orbInit.componentsMap[E_Component::SRP_CANNONBALL] = accSrp.norm();

            break;
		}

		case E_SRPModel::BOXWING:
		{
			SatPos satPos;
			satPos.rSatCom		= eci2ecf * rSat;
			satPos.satVel		= eci2ecf * vSat + deci2ecf * rSat;
			satPos.posTime		= timeInit;
			satPos.Sat			= orbInit.Sat;
			satPos.satNav_ptr 	= &nav.satNavMap[orbInit.Sat];

			updateSatAtts(satPos);

			Vector3d eX = eci2ecf.transpose() * satPos.satNav_ptr->attStatus.eXBody;
			Vector3d eY = eci2ecf.transpose() * satPos.satNav_ptr->attStatus.eYBody;
			Vector3d eZ = eci2ecf.transpose() * satPos.satNav_ptr->attStatus.eZBody;

			Vector3d accSolarBoxwing = applyBoxwingSrp(orbInit, ed, eX, eY, eZ);

			double eclipseFrac = sunVisibility(rSat, planetsPosMap[E_ThirdBody::SUN], planetsPosMap[E_ThirdBody::MOON]);
			accSolarBoxwing *= eclipseFrac;
			acc +=  accSolarBoxwing;

            orbInit.componentsMap[E_Component::SRP_BOXWING] = accSolarBoxwing.norm();

            break;
		}
	}

	if (orbInit.albedo != +E_SRPModel::NONE)
	{
		double A		= orbInit.area;
		double m		= orbInit.mass;
		double E 		= 1367;
		double cBall	= 0.8 * E / CLIGHT;
		double alpha	= 0.3;
		double Ae		= M_PI * SQR(RE_WGS84);
		Vector3d rSun	= planetsPosMap[E_ThirdBody::SUN];

		double factor 	= Ae / rSat.squaredNorm();
		double E_IR 	= (1 - alpha) / (4 * PI);

		double cos_		= rSat.dot(rSun) / (rSat.norm() * rSun.norm());
		double phi		= acos(cos_);
		double sin_		= sin(phi);
		double E_Vis	= 2 * alpha / (3 * SQR(PI)) * ((PI - phi) * cos_ + sin_);

		E_Vis	*= factor;
		E_IR 	*= factor;

		switch (orbInit.albedo)
		{
			case E_SRPModel::CANNONBALL:
			{
				Vector3d accAlbedo = A / m * (E_Vis + E_IR) * cBall * rSat.normalized();

				orbInit.componentsMap[E_Component::ALBEDO] = accAlbedo.norm();

				acc += accAlbedo;

				break;
			}
			case E_SRPModel::BOXWING:
			{
				SatPos satPos;
				satPos.rSatCom		= eci2ecf * rSat;
				satPos.satVel		= eci2ecf * vSat + deci2ecf * rSat;
				satPos.posTime		= timeInit;
				satPos.Sat			= orbInit.Sat;
				satPos.satNav_ptr 	= &nav.satNavMap[orbInit.Sat];

				updateSatAtts(satPos);

				Vector3d eX = eci2ecf.transpose() * satPos.satNav_ptr->attStatus.eXBody;
				Vector3d eY = eci2ecf.transpose() * satPos.satNav_ptr->attStatus.eYBody;
				Vector3d eZ = eci2ecf.transpose() * satPos.satNav_ptr->attStatus.eZBody;

				Vector3d accAlbedoBoxwing = applyBoxwingAlbedo(orbInit, E_Vis, E_IR, rSat*-1.0, ed, eX, eY, eZ);

				orbInit.componentsMap[E_Component::ALBEDO_BOXWING] = accAlbedoBoxwing.norm();

				acc +=  accAlbedoBoxwing;

				break;
			}
		}
	}

	if (0)
	for (auto once : {1})
	{
		auto it = UbxDecoder::acclDataMaps[orbInit.Sat.id()].lower_bound(time);
		if (it == UbxDecoder::acclDataMaps[orbInit.Sat.id()].end())
		{
			return;
		}

		auto& [foundTime, accl] = *it;

		Vector3d accBody = ((accl - orbInit.acclBias).array() * orbInit.acclScale.array()).matrix();

		VectorEcef accEcef = body2ecef(orbInit.attStatus, accBody);

		Vector3d accAccl = eci2ecf.transpose() * accEcef;

// 		std::cout << "\n" << "vSat: " << vSat.normalized().transpose();
// 		std::cout << "\n" << "R: " << er.transpose();
// 		std::cout << "\n" << "T: " << et.transpose();
// 		std::cout << "\n" << "N: " << en.transpose();

		Vector3d afX = eci2ecf.transpose() * body2ecef(orbInit.attStatus, Vector3d::UnitX());
		Vector3d afY = eci2ecf.transpose() * body2ecef(orbInit.attStatus, Vector3d::UnitY());
		Vector3d afZ = eci2ecf.transpose() * body2ecef(orbInit.attStatus, Vector3d::UnitZ());

// 		std::cout << "\n" << "x: " << afX.transpose();
// 		std::cout << "\n" << "y: " << afY.transpose();
// 		std::cout << "\n" << "z: " << afZ.transpose();

// 		dAdParam.col(orbInit.numEmp + 0) = afX;
// 		dAdParam.col(orbInit.numEmp + 1) = afY;
// 		dAdParam.col(orbInit.numEmp + 2) = afZ;

		acc += accAccl;
	}

	if (orbInit.empirical)
	{
		double scalef		= SQR(AU) / satToSun.squaredNorm();
		double eclipseFrac	= sunVisibility(rSat, planetsPosMap[E_ThirdBody::SUN], planetsPosMap[E_ThirdBody::MOON]);

		double scaling = 1e-9;

		vector<Vector3d> axis =
		{
			Vector3d::Zero(),
			ed * scaling * scalef,
			ey * scaling * scalef,
			eb * scaling * scalef,
			er * scaling,
			et * scaling,
			en * scaling,
			ep * scaling,
			eq * scaling
		};

		Vector3d& rSun = planetsPosMap[E_ThirdBody::SUN];
		Vector3d z = (rSat	.cross(vSat))	.normalized();
		Vector3d y = (z		.cross(rSun))	.normalized();
		Vector3d x = (y		.cross(z))		.normalized();

		double du = atan2(rSat.dot(y), rSat.dot(x));

		Vector3d accEmp = Vector3d::Zero();

		for (int i = 0; i < orbInit.empInput.size(); i++)
		{
			auto& empdata = orbInit.empInput[i];
			double ecl = 1;
			if (empdata.scaleEclipse)
			{
				ecl = eclipseFrac;
			}
			dAdParam.col(i) = axis[empdata.axisId] * ecl;

			if		(empdata.type == +E_TrigType::COS)		dAdParam.col(i) *= cos(empdata.deg * du);
			else if	(empdata.type == +E_TrigType::SIN)		dAdParam.col(i) *= sin(empdata.deg * du);

			accEmp += empdata.value * dAdParam.col(i);
		}

		orbInit.componentsMap[E_Component::EMPIRICAL] = accEmp.norm();

		acc += accEmp;
	}
}

void OrbitIntegrator::operator()(
	const	Orbits&	orbInits,
			Orbits&	orbUpdates,
	const	double	timeOffset)
{
	computeCommon(timeInit + timeOffset);

#	ifdef ENABLE_PARALLELISATION
		Eigen::setNbThreads(1);
#		pragma omp parallel for
#	endif
	for (int i = 0; i < orbInits.size(); i++)
	{
		Matrix6d A			= Matrix6d::Zero();
		A.block<3,3>(0,3)	= Matrix3d::Identity();

		auto& orbInit	= orbInits	[i];
		auto& orbUpdate	= orbUpdates[i];

		if (orbInit.exclude)
		{
			continue;
		}

		int numParam = orbInit.numEmp + orbInit.numParam;

		Vector3d acc		= Vector3d::Zero();
		Matrix3d dAdPos		= Matrix3d::Zero();
		Matrix3d dAdVel		= Matrix3d::Zero();
		MatrixXd dAdParam	= MatrixXd::Zero(3, numParam);

		computeAcceleration(orbInit, acc, dAdPos, dAdVel, dAdParam, timeInit + timeOffset);

		A.block<3,3>(3,0) = dAdPos;
		A.block<3,3>(3,3) = dAdVel;

		orbUpdate.componentsMap	= orbInit.componentsMap;
		orbUpdate.pos			= orbInit.vel;
		orbUpdate.vel			= acc;
		orbUpdate.posVelSTM		= A * orbInit.posVelSTM;

		orbUpdate.posVelSTM.bottomRightCorner(3, numParam) += dAdParam;
	}
	Eigen::setNbThreads(0);
};


void integrateOrbits(
	OrbitIntegrator&	orbitPropagator,
	Orbits&				orbits,
	double				integrationPeriod,
	double 				dtRequested)
{
	if	( orbits.empty()
		||integrationPeriod == 0)
	{
		return;
	}

	double	dt			= dtRequested;
	int		steps		= round(integrationPeriod / dt);
	double	remainder	= fmod (integrationPeriod, dtRequested);

	if (steps == 0)
	{
		steps = 1;
	}

	if (remainder != 0)
	{
		double newDt = integrationPeriod / steps;

		BOOST_LOG_TRIVIAL(warning) << "Warning: Time step adjusted from " << dt << " to " << newDt;

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

	for (auto& orbit : orbits)
	{
		auto& satNav	= nav.satNavMap[orbit.Sat];
		auto& satPos0	= satNav.satPos0;

		auto satTrace = getTraceFile(satNav);

		satTrace << "\n" << "Propagated orbit from " << orbitPropagator.timeInit << " for " << integrationPeriod;

		for (auto& [component, value] : orbit.componentsMap)
		{
			tracepdeex(0, satTrace, "\n");
			tracepdeex(4, satTrace, "%s",				orbitPropagator.timeInit.to_string().c_str());
			tracepdeex(0, satTrace, " %-25s %+14.4e",	component._to_string(), value);
		}
	}
}

/** Get the estimated elements for a single satellite's orbit
*/
shared_ptr<KFState> getOrbitFromState(
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

		if	( (  kfKey.type >= KF::BEGIN_ORBIT_STATES
			  && kfKey.type <= KF::END_ORBIT_STATES)
			||(  kfKey.type >= KF::BEGIN_INERTIAL_STATES
			  && kfKey.type <= KF::END_INERTIAL_STATES))
		{
			kfKeyMap[kfKey] = index;
			index++;
		}
	}

	KFState subState;
	kfState.getSubState(kfKeyMap, subState);

	return make_shared<KFState>(subState);
}

Orbits prepareOrbits(
	Trace&			trace,
	const KFState&	kfState)
{
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
		auto& satOpts	= acsConfig.getSatOpts(Sat);

		orbit.subState_ptr = getOrbitFromState(trace, Sat, orbit.str, kfState);

		auto& subState	= *orbit.subState_ptr;

		vector<bool> eclipse;
		for (int i = 0; i < 3; i++)		{	eclipse.push_back(queryVectorElement(satOpts.empirical_dyb_eclipse, i));	}
		for (int i = 0; i < 3; i++)		{	eclipse.push_back(queryVectorElement(satOpts.empirical_rtn_eclipse, i));	}

		for (auto& [subKey, index] : subState.kfIndexMap)
		{
			if (subKey.type == KF::ORBIT)
			{
				continue;
			}

			E_TrigType trigType;
			if		(subKey.num == 0)		trigType = E_TrigType::COS;
			else							trigType = E_TrigType::SIN;

			double stateValue = subState.x(index);

			switch (subKey.type)
			{
				case KF::GYRO_SCALE:	{ orbit.gyroScale	(subKey.num) = stateValue;	break;}
				case KF::ACCL_SCALE:	{ orbit.acclScale	(subKey.num) = stateValue;	break;}
				case KF::GYRO_BIAS:		{ orbit.gyroBias	(subKey.num) = stateValue;	break;}
				case KF::ACCL_BIAS:		{ orbit.acclBias	(subKey.num) = stateValue;	break;}

				case KF::EMP_D_0:		{ orbit.empInput.push_back({eclipse[0],	0, E_EmpAxis::D, trigType,	stateValue});	break;}
				case KF::EMP_D_1:		{ orbit.empInput.push_back({eclipse[0],	1, E_EmpAxis::D, trigType,	stateValue});	break;}
				case KF::EMP_D_2:		{ orbit.empInput.push_back({eclipse[0],	2, E_EmpAxis::D, trigType,	stateValue});	break;}
				case KF::EMP_D_3:		{ orbit.empInput.push_back({eclipse[0],	3, E_EmpAxis::D, trigType,	stateValue});	break;}
				case KF::EMP_D_4:		{ orbit.empInput.push_back({eclipse[0],	4, E_EmpAxis::D, trigType,	stateValue});	break;}

				case KF::EMP_Y_0:		{ orbit.empInput.push_back({eclipse[1],	0, E_EmpAxis::Y, trigType,	stateValue});	break;}
				case KF::EMP_Y_1:		{ orbit.empInput.push_back({eclipse[1],	1, E_EmpAxis::Y, trigType,	stateValue});	break;}
				case KF::EMP_Y_2:		{ orbit.empInput.push_back({eclipse[1],	2, E_EmpAxis::Y, trigType,	stateValue});	break;}
				case KF::EMP_Y_3:		{ orbit.empInput.push_back({eclipse[1],	3, E_EmpAxis::Y, trigType,	stateValue});	break;}
				case KF::EMP_Y_4:		{ orbit.empInput.push_back({eclipse[1],	4, E_EmpAxis::Y, trigType,	stateValue});	break;}

				case KF::EMP_B_0:		{ orbit.empInput.push_back({eclipse[2],	0, E_EmpAxis::B, trigType,	stateValue});	break;}
				case KF::EMP_B_1:		{ orbit.empInput.push_back({eclipse[2],	1, E_EmpAxis::B, trigType,	stateValue});	break;}
				case KF::EMP_B_2:		{ orbit.empInput.push_back({eclipse[2],	2, E_EmpAxis::B, trigType,	stateValue});	break;}
				case KF::EMP_B_3:		{ orbit.empInput.push_back({eclipse[2],	3, E_EmpAxis::B, trigType,	stateValue});	break;}
				case KF::EMP_B_4:		{ orbit.empInput.push_back({eclipse[2],	4, E_EmpAxis::B, trigType,	stateValue});	break;}

				case KF::EMP_R_0:		{ orbit.empInput.push_back({eclipse[3],	0, E_EmpAxis::R, trigType,	stateValue});	break;}
				case KF::EMP_R_1:		{ orbit.empInput.push_back({eclipse[3],	1, E_EmpAxis::R, trigType,	stateValue});	break;}
				case KF::EMP_R_2:		{ orbit.empInput.push_back({eclipse[3],	2, E_EmpAxis::R, trigType,	stateValue});	break;}
				case KF::EMP_R_3:		{ orbit.empInput.push_back({eclipse[3],	3, E_EmpAxis::R, trigType,	stateValue});	break;}
				case KF::EMP_R_4:		{ orbit.empInput.push_back({eclipse[3],	4, E_EmpAxis::R, trigType,	stateValue});	break;}

				case KF::EMP_T_0:		{ orbit.empInput.push_back({eclipse[4],	0, E_EmpAxis::T, trigType,	stateValue});	break;}
				case KF::EMP_T_1:		{ orbit.empInput.push_back({eclipse[4],	1, E_EmpAxis::T, trigType,	stateValue});	break;}
				case KF::EMP_T_2:		{ orbit.empInput.push_back({eclipse[4],	2, E_EmpAxis::T, trigType,	stateValue});	break;}
				case KF::EMP_T_3:		{ orbit.empInput.push_back({eclipse[4],	3, E_EmpAxis::T, trigType,	stateValue});	break;}
				case KF::EMP_T_4:		{ orbit.empInput.push_back({eclipse[4],	4, E_EmpAxis::T, trigType,	stateValue});	break;}

				case KF::EMP_N_0:		{ orbit.empInput.push_back({eclipse[5],	0, E_EmpAxis::N, trigType,	stateValue});	break;}
				case KF::EMP_N_1:		{ orbit.empInput.push_back({eclipse[5],	1, E_EmpAxis::N, trigType,	stateValue});	break;}
				case KF::EMP_N_2:		{ orbit.empInput.push_back({eclipse[5],	2, E_EmpAxis::N, trigType,	stateValue});	break;}
				case KF::EMP_N_3:		{ orbit.empInput.push_back({eclipse[5],	3, E_EmpAxis::N, trigType,	stateValue});	break;}
				case KF::EMP_N_4:		{ orbit.empInput.push_back({eclipse[5],	4, E_EmpAxis::N, trigType,	stateValue});	break;}

				case KF::EMP_P_0:		{ orbit.empInput.push_back({false,		0, E_EmpAxis::P, trigType,	stateValue});	break;}
				case KF::EMP_P_1:		{ orbit.empInput.push_back({false,		1, E_EmpAxis::P, trigType,	stateValue});	break;}
				case KF::EMP_P_2:		{ orbit.empInput.push_back({false,		2, E_EmpAxis::P, trigType,	stateValue});	break;}
				case KF::EMP_P_3:		{ orbit.empInput.push_back({false,		3, E_EmpAxis::P, trigType,	stateValue});	break;}
				case KF::EMP_P_4:		{ orbit.empInput.push_back({false,		4, E_EmpAxis::P, trigType,	stateValue});	break;}

				case KF::EMP_Q_0:		{ orbit.empInput.push_back({false,		0, E_EmpAxis::Q, trigType,	stateValue});	break;}
				case KF::EMP_Q_1:		{ orbit.empInput.push_back({false,		1, E_EmpAxis::Q, trigType,	stateValue});	break;}
				case KF::EMP_Q_2:		{ orbit.empInput.push_back({false,		2, E_EmpAxis::Q, trigType,	stateValue});	break;}
				case KF::EMP_Q_3:		{ orbit.empInput.push_back({false,		3, E_EmpAxis::Q, trigType,	stateValue});	break;}
				case KF::EMP_Q_4:		{ orbit.empInput.push_back({false,		4, E_EmpAxis::Q, trigType,	stateValue});	break;}

				default:				{ break;}
			}
		}

		orbit.pos		= subState.x.head		(3);
		orbit.vel		= subState.x.segment	(3, 3);

		if (orbit.pos.isZero())
		{
			orbit.exclude = true;
			continue;
		}

		orbit.posVelSTM	= MatrixXd::Identity(6, subState.kfIndexMap.size());

		orbit.attStatus	= nav.satNavMap[Sat].attStatus;

		orbit.numEmp	= orbit.empInput.size();

		orbit.OrbitOptions::operator=(satOpts);

		string svn		= Sat.svn();

		auto findPower_it = theSinex.mapsatpowers.find(svn);
		if (findPower_it != theSinex.mapsatpowers.end())
		{
			auto& [svn, satPowerMap] = *findPower_it;
			auto& [time, firstPower] = *satPowerMap.begin();

			orbit.power = firstPower.power;
		}

		auto findMass_it = theSinex.mapsatmasses.find(svn);
		if (findMass_it != theSinex.mapsatmasses.end())
		{
			auto& [svn, satMassMap] = *findMass_it;
			auto& [time, firstMass] = *satMassMap.begin();

			orbit.mass = firstMass.mass;
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
	for (auto& orbit : orbits)
	{
		if (orbit.exclude)
		{
			continue;
		}

		auto& subState = *orbit.subState_ptr;

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
	GTime			time)
{
	double tgap = (time - kfState.time).to_double();

	if (tgap == 0)
	{
		return;
	}

	Orbits orbits = prepareOrbits(trace, kfState);

	if (orbits.empty())
	{
		return;
	}

	InteractiveTerminal::setMode(E_InteractiveMode::PropagatingOrbits);
	BOOST_LOG_TRIVIAL(info) << " ------- PROPAGATING ORBITS           --------" << "\n";

	OrbitIntegrator integrator;
	integrator.timeInit				= kfState.time;

	integrateOrbits(integrator, orbits, tgap, acsConfig.propagationOptions.integrator_time_step);

	applyOrbits(trace, orbits, kfState, time, tgap);

	for (auto& orbit : orbits)
	{
		auto& satNav	= nav.satNavMap[orbit.Sat];
		auto& satPos0	= satNav.satPos0;

		satPos0.posTime		= time;
		satPos0.rSatEci0	= orbit.pos;
		satPos0.vSatEci0	= orbit.vel;
	}
};

void addNilDesignStates(
	const	KalmanModel&	model,
	const	KFState&		kfState,
	const	KF&				kfType,
			int				num,
	const	string&			id)
{
	for (int i = 0; i < num; i++)
	{
		InitialState init = initialStateFromConfig(model, i);

		if (init.estimate)
		{
			KFKey key;
			key.type	= kfType;
			key.num		= i;
			key.comment	= init.comment;

			SatSys Sat = id.c_str();
			if (Sat.prn)	key.Sat		= id.c_str();
			else			key.str		= id;

			kfState.addKFState(key, init);
		}
	}
}

void addEmpStates(
	const EmpKalmans&	satOpts,
	const KFState&		kfState,
	const string&		id)
{
	addNilDesignStates(satOpts.emp_d_0,		kfState,	KF::EMP_D_0,	1, id);
	addNilDesignStates(satOpts.emp_d_1,		kfState,	KF::EMP_D_1,	2, id);
	addNilDesignStates(satOpts.emp_d_2,		kfState,	KF::EMP_D_2,	2, id);
	addNilDesignStates(satOpts.emp_d_3,		kfState,	KF::EMP_D_3,	2, id);
	addNilDesignStates(satOpts.emp_d_4,		kfState,	KF::EMP_D_4,	2, id);

	addNilDesignStates(satOpts.emp_y_0,		kfState,	KF::EMP_Y_0,	1, id);
	addNilDesignStates(satOpts.emp_y_1,		kfState,	KF::EMP_Y_1,	2, id);
	addNilDesignStates(satOpts.emp_y_2,		kfState,	KF::EMP_Y_2,	2, id);
	addNilDesignStates(satOpts.emp_y_3,		kfState,	KF::EMP_Y_3,	2, id);
	addNilDesignStates(satOpts.emp_y_4,		kfState,	KF::EMP_Y_4,	2, id);

	addNilDesignStates(satOpts.emp_b_0,		kfState,	KF::EMP_B_0,	1, id);
	addNilDesignStates(satOpts.emp_b_1,		kfState,	KF::EMP_B_1,	2, id);
	addNilDesignStates(satOpts.emp_b_2,		kfState,	KF::EMP_B_2,	2, id);
	addNilDesignStates(satOpts.emp_b_3,		kfState,	KF::EMP_B_3,	2, id);
	addNilDesignStates(satOpts.emp_b_4,		kfState,	KF::EMP_B_4,	2, id);

	addNilDesignStates(satOpts.emp_r_0,		kfState,	KF::EMP_R_0,	1, id);
	addNilDesignStates(satOpts.emp_r_1,		kfState,	KF::EMP_R_1,	2, id);
	addNilDesignStates(satOpts.emp_r_2,		kfState,	KF::EMP_R_2,	2, id);
	addNilDesignStates(satOpts.emp_r_3,		kfState,	KF::EMP_R_3,	2, id);
	addNilDesignStates(satOpts.emp_r_4,		kfState,	KF::EMP_R_4,	2, id);

	addNilDesignStates(satOpts.emp_t_0,		kfState,	KF::EMP_T_0,	1, id);
	addNilDesignStates(satOpts.emp_t_1,		kfState,	KF::EMP_T_1,	2, id);
	addNilDesignStates(satOpts.emp_t_2,		kfState,	KF::EMP_T_2,	2, id);
	addNilDesignStates(satOpts.emp_t_3,		kfState,	KF::EMP_T_3,	2, id);
	addNilDesignStates(satOpts.emp_t_4,		kfState,	KF::EMP_T_4,	2, id);

	addNilDesignStates(satOpts.emp_n_0,		kfState,	KF::EMP_N_0,	1, id);
	addNilDesignStates(satOpts.emp_n_1,		kfState,	KF::EMP_N_1,	2, id);
	addNilDesignStates(satOpts.emp_n_2,		kfState,	KF::EMP_N_2,	2, id);
	addNilDesignStates(satOpts.emp_n_3,		kfState,	KF::EMP_N_3,	2, id);
	addNilDesignStates(satOpts.emp_n_4,		kfState,	KF::EMP_N_4,	2, id);

	addNilDesignStates(satOpts.emp_p_0,		kfState,	KF::EMP_P_0,	1, id);
	addNilDesignStates(satOpts.emp_p_1,		kfState,	KF::EMP_P_1,	2, id);
	addNilDesignStates(satOpts.emp_p_2,		kfState,	KF::EMP_P_2,	2, id);
	addNilDesignStates(satOpts.emp_p_3,		kfState,	KF::EMP_P_3,	2, id);
	addNilDesignStates(satOpts.emp_p_4,		kfState,	KF::EMP_P_4,	2, id);

	addNilDesignStates(satOpts.emp_q_0,		kfState,	KF::EMP_Q_0,	1, id);
	addNilDesignStates(satOpts.emp_q_1,		kfState,	KF::EMP_Q_1,	2, id);
	addNilDesignStates(satOpts.emp_q_2,		kfState,	KF::EMP_Q_2,	2, id);
	addNilDesignStates(satOpts.emp_q_3,		kfState,	KF::EMP_Q_3,	2, id);
	addNilDesignStates(satOpts.emp_q_4,		kfState,	KF::EMP_Q_4,	2, id);
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

				KF::EMP_D_0,	KF::EMP_D_1,	KF::EMP_D_2,	KF::EMP_D_3,	KF::EMP_D_4,
				KF::EMP_Y_0,	KF::EMP_Y_1,	KF::EMP_Y_2,	KF::EMP_Y_3,	KF::EMP_Y_4,
				KF::EMP_B_0,	KF::EMP_B_1,	KF::EMP_B_2,	KF::EMP_B_3,	KF::EMP_B_4,
				KF::EMP_R_0,	KF::EMP_R_1,	KF::EMP_R_2,	KF::EMP_R_3,	KF::EMP_R_4,
				KF::EMP_T_0,	KF::EMP_T_1,	KF::EMP_T_2,	KF::EMP_T_3,	KF::EMP_T_4,
				KF::EMP_N_0,	KF::EMP_N_1,	KF::EMP_N_2,	KF::EMP_N_3,	KF::EMP_N_4,
				KF::EMP_P_0,	KF::EMP_P_1,	KF::EMP_P_2,	KF::EMP_P_3,	KF::EMP_P_4,
				KF::EMP_Q_0,	KF::EMP_Q_1,	KF::EMP_Q_2,	KF::EMP_Q_3,	KF::EMP_Q_4,
			})
		{
			int n = 2;
			if (type == +KF::ORBIT)
				n = 6;

			vector<double>	aprioriVec	(n);
			vector<double>	sigmaVec	(n);
			deque<bool>		estimatedVec(n);

			bool anyFound = false;
			for (int i = 0; i < n; i++)
			{
				KFKey kfKey = key;
				kfKey.type	= type;
				kfKey.num	= i;

				double val = 0;
				double var = 0;
				bool found = kfState.getKFValue(kfKey, val, &var);

				aprioriVec	[i]	 = val;
				sigmaVec	[i]	 = sqrt(var);
				estimatedVec[i]	 = found;
				anyFound		|= found;
			}

			if (anyFound == false)
			{
				continue;
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

	PTime logtime = kfState.time;

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

	output << bsoncxx::to_json(json) << "\n";
}



