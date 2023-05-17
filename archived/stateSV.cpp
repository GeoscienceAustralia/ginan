//
// Created by Sébastien Allgeyer on 15/3/22.
//

#include "stateSV.hpp"
#include "mainPod.hpp"
#include "jplEph.hpp"
#include "math_tools.hpp"
#include "coordinates.hpp"
#include "erp.hpp"
#include "iers2010.hpp"
#include <boost/log/core.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/expressions.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include <iostream>

stateSV::stateSV() {
	name="blah";
};

void stateSV::computeAcceleration(Vector6d & init, const double mjd)
{
	IERS2010 iers;
	Vector3d acc = Vector3d::Zero();
	Vector3d rSat = init.head(3);
	Vector3d vSat = init.tail(3);

	Matrix3d eci2ecf, deci2ecf;
	ERPValues erps ;
	geterp(mainPod::Instance()->orbit_data.erpData, mjd/86400.0, erps);
	eci2ecef_sofa(mjd/86400.0, erps, eci2ecf, deci2ecf);

	if(true)
	{
		for (auto  & [name, vecPosition] : mainPod::Instance()->planets){
			if (name != orbiting)
			{
				Vector3d acc_ = accelSourcePoint(rSat, vecPosition.head(3), GM_values[name]);
                acc += acc_
                BOOST_LOG_TRIVIAL(debug) << name << " " << acc_;
			}
		}
		accI = acc;
	}

	//central body force
	if(false)
	{
		accI += accelCentralForce(rSat, mainPod::Instance()->orbit_data.egm.earthGravityConstant);
	}


	//Acceleration due to the spherical field.
	if(true)
	{

		MatrixXd Cnm, Snm;
		Cnm = mainPod::Instance()->orbit_data.egm.gfctC;
		Snm = mainPod::Instance()->orbit_data.egm.gfctS;

		Eigen::Array<double, 1, 6> dood_arr = iers.doodson((mjd+33+32.184)/86400.0, erps.ut1_utc);

		if (false)
		{
			MatrixXd Cnm_solid = MatrixXd::Zero(5,5);  /* correction for solid earth tide*/
			MatrixXd Snm_solid = MatrixXd::Zero(5,5);
			iers.SolidEarthTide1(eci2ecf*mainPod::Instance()->planets[E_ThirdBody::SUN].head(3), eci2ecf*mainPod::Instance()->planets[E_ThirdBody::MOON].head(3),
								Cnm_solid, Snm_solid);		
			iers.SolidEarthTide2((mjd+33+32.184)/86400.0, erps.ut1_utc, Cnm_solid, Snm_solid);
			Cnm.block(0,0,5,5) += Cnm_solid;
			Snm.block(0,0,5,5) += Snm_solid;
		}

		// ocean tide
		if(false)
		{
			Eigen::Array<double, 1, 6> dood_arr = iers.doodson((mjd+33+32.184)/86400.0, erps.ut1_utc);
			MatrixXd Cnm_ocean = MatrixXd::Zero(Cnm.cols(), Cnm.cols());  /* correction for solid earth tide*/
			MatrixXd Snm_ocean = MatrixXd::Zero(Cnm.cols(), Cnm.rows());
			mainPod::Instance()->orbit_data.tide.getSPH(dood_arr, Cnm_ocean, Snm_ocean);
			Cnm = Cnm + Cnm_ocean;
			Snm = Snm + Snm_ocean;
		}

		//pole tides
		if(false)
		{
			iers.PoleOceanTide(mjd/86400, erps.xp, erps.yp, Cnm, Snm);
			iers.PoleSolidEarthTide(mjd/86400, erps.xp, erps.yp, Cnm, Snm);
		}


		posE = eci2ecf * rSat;
		velE = eci2ecf * vSat + deci2ecf * rSat;
		acc = accelSPH(posE,
				Cnm, Snm, 
				mainPod::Instance()->orbit_data.egm.degMax, 
				mainPod::Instance()->orbit_data.egm.earthGravityConstant );
		accI += eci2ecf.transpose()*acc;
        BOOST_LOG_TRIVIAL(debug) << " SPH " << 
	}

	// Relativistic effects;
	if (true)
	{
		Vector3d acc_rel;
		iers.relativity(init, mainPod::Instance()->planets[E_ThirdBody::SUN], eci2ecf, deci2ecf,  acc_rel);
		accI += acc_rel;
		// BOOST_LOG_TRIVIAL(debug) << acc_rel.transpose();

	}

	//Indirect J2.
	if(true)
	{
        /**
         *  @brief "The indirect J2 effect". Only for Sun and Moon
         *  @ref GOCE standards GO-TN-HPF-GS-0111
         * 
         */
        Vector3d accJ2 = Vector3d::Zero();
        for (auto body : { E_ThirdBody::SUN, E_ThirdBody::MOON})
	    {
		double		GM_Body;
		double		rho;
		Vector3d pos_ecef = eci2ecf * mainPod::Instance()->planets[body].head(3);
		double dist = pos_ecef.norm();
		double term = (GM_values[body] / pow(dist,3)) * pow(RE_WGS84 / dist, 2);
		Vector3d vec = term * (5.0 * pow(pos_ecef.z()/dist,2) - 1.0) * pos_ecef;
		vec.z()      = term * (5.0 * pow(pos_ecef.z()/dist,2) - 3.0) * pos_ecef.z();

		accJ2 += -1.0 * ( ( 3.0 * sqrt(5.0) ) / 2.0 ) * mainPod::Instance()->orbit_data.egm.gfctC(2,0) * (vec);
        }
		accI += eci2ecf.transpose() * accJ2;

	}


};


void stateSV::operator()(Vector6d & init, Vector6d &update, const double mjd)
{
	Vector3d rSat = init.head(3);
	Vector3d vSat = init.tail(3);
	computeAcceleration(init, mjd);
	update.head(3) = vSat;
	update.tail(3) = accI;
};

