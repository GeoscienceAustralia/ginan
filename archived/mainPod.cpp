//
// Created by Sébastien Allgeyer on 11/3/22.
//
#include <boost/log/core.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/expressions.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/numeric/odeint.hpp>       // odeint function definitions

using namespace boost::numeric::odeint;


#include "mainPod.hpp"
#include "orbit.hpp"
#include "math_tools.hpp"
#include "coordinates.hpp"
#include "jpl_eph.hpp"
#include "jplEph.hpp"
#include "constants.hpp"
#include "iers2010.hpp"

atomic<mainPod*> mainPod::pInstance {nullptr};
mutex mainPod::m_;

mainPod* mainPod::Instance(int argc, char* argv[]) {
	if (pInstance == nullptr){
		lock_guard<mutex> lock(m_);
		if (pInstance == nullptr){
			pInstance = new mainPod(argc, argv);
		}
	}
	return pInstance;
}

void mainPod::init(int argv, char **argc) {
	// orbit_data.egm = StaticField("EGM2008.gfc", 15);
	orbit_data.egm = StaticField("GOCO05s.gfc",15);
	orbit_data.tide = OceanTide("fes2004_Cnm-Snm.dat", 15);
	orbit_data.startTime = GTime(boost::posix_time::time_from_string("2007-04-12 00:00:00"));
	orbit_data.jplEph_ptr = (struct jpl_eph_data*)jpl_init_ephemeris("DE436.1950.2050", nullptr, nullptr);
	readerp("igs96p02.erp", orbit_data.erpData);
	currentTime = orbit_data.startTime;
    currentTime_d = gpst2mjd(currentTime)*86400.0;
	satellite["G01"] = stateSV();
	satellite["G02"] = stateSV();
	//From sp3 test file.
	satellite["G01"].posI << 1120.484500,  15079.297000,  22047.746500 ;
	satellite["G01"].velI << -37784.29000,  -5001.870000 ,  5331.295000 ;

	satellite["G02"].posI << -4632.723000, -25495.815500 ,  6219.951000;
	satellite["G02"].velI << 24185.510000,   3236.255000,  29921.925000;   


	Matrix3d U, dU;
	ERPValues erps;
	geterp(orbit_data.erpData, currentTime, erps);
	eci2ecef_sofa(gpst2mjd(currentTime), erps ,U, dU);
	BOOST_LOG_TRIVIAL(debug) << "\n"<< U << "\n" << dU ; 
	eci2ecef_sofa_old(gpst2mjd(currentTime), erps ,U, dU);
	BOOST_LOG_TRIVIAL(debug) << "\n\n" << U << "\n" << dU ; 
	// exit(0);

	// BOOST_LOG_TRIVIAL(info) << "inittial pos";
	for (auto & [name, sat]: satellite) {
		sat.posI *= 1000.0 ; // km -> m
		sat.velI /= 10.0; // dm/s-> m/s
		sat.posVelI.head(3) = sat.posI;
		sat.posVelI.tail(3) = sat.velI;
		// BOOST_LOG_TRIVIAL(info) << name << " " << sat.posVelI.transpose();
		sat.posE = U * sat.posI;
		sat.velE = U * sat.velI + dU * sat.posI;
	}

	
}


void mainPod::oncePerEpoch()
{
	/**
	 * Generate the eci 2 ecef
	 * */
	//Eigen::Matrix3d U, dU;
	//eci2ecef_sofa(gpst2mjd(currentTime), U,dU);

	/**
	 * Generate the position of the celestial bodies
	 * This is the first step as the positin of Earth and Moon are required for the solidEarth tides.
	 */
	planets.clear();
	for (int i = 0; i < E_ThirdBody::_size(); i++) 
	{
		E_ThirdBody body = E_ThirdBody::_values()[i];
		string bodyName = E_ThirdBody::_names()[i];
		Vector3d pos;
		Vector3d vel;
		jplEphPos(orbit_data.jplEph_ptr, gpst2mjd(currentTime) +(33+32.186)/86400.0+ JD2MJD, body, pos, &vel);
		Vector6d posvel;
		posvel.head(3) = pos;
		posvel.tail(3) = vel;
		planets[body] = posvel;
	}


	/**
	 * Generate the spherical harmonic field
	 */


}

void mainPod::oncePerEpochPerSV()
{
	/**
	 * Acceleration from the spherical harmonics
	 */

	/**
	 * Acceleration from the celestial bodies
	 */

	/**
	 * Solar radiation pressure
	 */

	/**
	 * Relativistic effects
	 */

}
void mainPod::run() {
// 	typedef runge_kutta_fehlberg78<Vector6d> rkf78;
// 	typedef controlled_runge_kutta<rkf78> ctrl_rkf78;

// 	double errAbs = 1.0e-16;
// 	double errRel = 1.0e-13;

// 	auto controller = make_controlled(errAbs, errRel, rkf78() );

	runge_kutta4< Vector6d > stepper;

//	BOOST_LOG_TRIVIAL(info) << "time step MJD  " << gpst2mjd(currentTime) << " ... " << gpst2mjd(orbit_data.startTime);
//	currentTime = currentTime + 86400.0;
//	BOOST_LOG_TRIVIAL(info) << "time step MJD  " << gpst2mjd(currentTime) << " ... " << gpst2mjd(orbit_data.startTime);
	// Get satellite independant values
	oncePerEpoch();
	for (auto & [name, sat]: satellite)
	{
		Vector6d old = sat.posVelI;
		double dt = 5.0;
		stepper.do_step(sat, sat.posVelI, currentTime_d, dt);
		// BOOST_LOG_TRIVIAL(debug) << "CHANGES "<< (old.head(3)-sat.posVelI.head(3)).norm() ;
		sat.posI = sat.posVelI.head(3);
		sat.velI = sat.posVelI.tail(3);
	}
//	oncePerEpochPerSV();
	currentTime += 5.0;
    currentTime_d += 5.0;
	Matrix3d U, dU;
	ERPValues erps;
	geterp(orbit_data.erpData, currentTime, erps);
	// BOOST_LOG_TRIVIAL(debug) << "Erps " << erps.xp;
	eci2ecef_sofa(gpst2mjd(currentTime), erps ,U, dU);
	// BOOST_LOG_TRIVIAL(debug) << (U.transpose() - U.inverse()).norm() ;
	// BOOST_LOG_TRIVIAL(debug) << (U.transpose().array() - U.inverse().array())/U.transpose().array() ;

	for (auto & [name, sat]: satellite)
	{
		sat.posE = U * sat.posI;
		sat.velE = U * sat.velI + dU * sat.posI;
		// BOOST_LOG_TRIVIAL(info) << name << " ** " << sat.posE.transpose() << " -- " << sat.posI.transpose();
	}




}