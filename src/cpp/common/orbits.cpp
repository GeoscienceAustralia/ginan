
// #pragma GCC optimize ("O0")

#include <algorithm>
#include <iostream>
#include <fstream>
#include <chrono>
#include <string>
#include <ctime>
#include <cmath>

using std::chrono::system_clock;
using std::chrono::time_point;
using std::string;


#include "peaCommitStrings.hpp"
#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "ephPrecise.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "orbitProp.hpp"
#include "algebra.hpp"
#include "orbits.hpp"
#include "satSys.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "enums.h"


#define RTOL_KEPLER			1E-14		///< relative tolerance for Kepler equation
#define MAX_ITER_KEPLER		30			///< max number of iteration of Kelpler


bool inertial2Keplers(
			Trace&		trace,
	const	VectorEci&	r,
	const	VectorEci&	v,
			Vector6d&	keplers)
{
	Vector3d e_r = r.normalized();

	//Calculate orbital momentum vector (perpendicular to both position and velocity)
	Vector3d L = r.cross(v);

	//Obtain the eccentricity vector
	Vector3d e	= v.cross(L) / GM_Earth - e_r;

	double L_x = L(0);
	double L_y = L(1);
	double L_z = L(2);

	L.normalize();

	//Determine the vector n pointing towards the ascending node (point on arc crossing xy plane?)
	Vector3d n0 = Vector3d(0,0,1).cross(L);
	if (n0.norm() < 0.0001)
	{
		trace << "\n fixingKKKKKKK";
		n0 = Vector3d(1,0,0);
	}
	n0.normalize();

	//get another handy vector
	Vector3d n1 = L.cross(n0).normalized();

	double e_X = e.dot(n0);
	double e_Y = e.dot(n1);

	//Determine the orbit eccentricity, which is simply the magnitude of the eccentricity vector e,
	double e_ = e.norm();

	if (e_ < 0.000001)
	{
		//if its too small, point it at a valid place and use a normalised version of it.
		e = n0;
		trace << "\n fixingBBB";
	}
	e.normalize();

	//Determine the true anomaly (angle between e and r)
	double nu;
	if (r.dot(v) >= 0)	nu = 		+ acos(e.dot(e_r));
	else				nu = 2*PI	- acos(e.dot(e_r));

	//Determine the eccentric anomaly
	double E = 2 * atan2(	sqrt(1-e_) * sin(nu/2),
							sqrt(1+e_) * cos(nu/2));


	//Compute the mean anomaly with help of Kepler’s Equation from the eccentric anomaly E and the eccentricity e
	double M = E - e_ * sin(E);

	bool error = false;
	if (isnan(nu))	{		std::cout << "nu is nan\n";		error = true;	}
	if (isnan(e_))	{		std::cout << "e_ is nan\n";		error = true;	}
	if (isnan(E))	{		std::cout << "E is nan\n";		error = true;	}
	if (isnan(M))	{		std::cout << "M is nan\n";		error = true;	}

	if (error)
	{
		std::cout << "\n" << "n0 " << n0.transpose();
		std::cout << "\te " << e.transpose();
		std::cout << "\tn1 " << n1.transpose();
		std::cout << "\tnu " << nu;
		std::cout << "\tE " << E;
		std::cout << "\tM " << M;

		return false;
	}

	keplers(KEPLER::LX)	= L_x;
	keplers(KEPLER::LY)	= L_y;
	keplers(KEPLER::LZ)	= L_z;
	keplers(KEPLER::EU)	= e_X;
	keplers(KEPLER::EV)	= e_Y;
	keplers(KEPLER::M )	= M;

	return true;
}


VectorEci keplers2Inertial(
			Trace&		trace,
	const	Vector6d&	keplers0)
{
	Vector3d L		= Vector3d::Zero();
	Vector2d eee	= Vector2d::Zero();

	L[0]		= keplers0[KEPLER::LX];
	L[1]		= keplers0[KEPLER::LY];
	L[2]		= keplers0[KEPLER::LZ];
	eee[0]		= keplers0[KEPLER::EU];
	eee[1]		= keplers0[KEPLER::EV];
	double M	= keplers0[KEPLER::M ];


	double e_ = eee.norm();


	double E		= M;
	double Eprev	= 0;
	int n;
	for (n = 0; n < MAX_ITER_KEPLER; n++)
	{
// 		std::cout << "\nE: " << n << " " << E << " " << Eprev;
		Eprev = E;
		E -= (E - e_ * sin(E) - M) / (1 - e_ * cos(E));

		if (fabs(E - Eprev) < RTOL_KEPLER)
		{
			break;
		}
	}

	if (n >= MAX_ITER_KEPLER)
	{
		std::cout << "iteratios";

		return VectorEci();
	}


	double nu	= 2 * atan2(	sqrt(1 + e_) * sin(E/2),
								sqrt(1 - e_) * cos(E/2));

	double r	= L.squaredNorm() / GM_Earth / (1 + e_ * cos(nu));

	L.normalize();

	//Determine the vector n pointing towards the ascending node (point on arc crossing xy plane?)
	Vector3d n0 = Vector3d(0,0,1).cross(L);
	if (n0.norm() < 0.0001)
	{
		trace << "\n fixingKKKKKKK";
		n0 = Vector3d(1,0,0);
	}
	n0.normalize();


	//get another handy vector
	Vector3d n1 = L.cross(n0).normalized();

	Vector3d e	= eee[0] * n0
				+ eee[1] * n1;

	if (e.norm() < 0.000001)
	{
		//if its too small, point it at a valid place and use a normalised version of it.
		e = n0;
		trace << "\n fixingBBB";
	}
	e.normalize();

	double x = r * cos(nu);
	double y = r * sin(nu);

	double cos_w = n0.dot(e);
	double cos_i = L(2);

	double cos_W = n0(0);
	double sin_W = n0(1);					//dont unify, needs sign

	if		(cos_w > +1)		cos_w = +1;
	else if (cos_w < -1)		cos_w = -1;
	if		(cos_i > +1)		cos_i = +1;
	else if (cos_i < -1)		cos_i = -1;


	double sin_w = sqrt(1 - SQR(cos_w));
	double sin_i = sqrt(1 - SQR(cos_i));

	if (n0.dot(e.cross(L)) < 0)
	{
		sin_w *= -1;
	}

// 	std::cout << "\tcosw " << cos_w;
// 	std::cout << "\tcosi " << cos_i;
// 	std::cout << "\tcosW " << cos_W;
// 	std::cout << "\tsinw " << sin_w;
// 	std::cout << "\tsini " << sin_i;
// 	std::cout << "\tsinW " << sin_W;

	if (isnan(nu))		{		std::cout << "nu is NAN\n";		}
	if (isnan(r))		{		std::cout << "r is NAN\n";		}
	if (isnan(sin_w))	{		std::cout << "sin_w is NAN\n";	}
	if (isnan(sin_i))	{		std::cout << "sin_i is NAN\n";	}
	if (isnan(sin_W))	{		std::cout << "sin_W is NAN\n";	}

	VectorEci rSat;
	rSat.x() = x * (cos_w * cos_W 	- sin_w * cos_i * sin_W	)	- y * (cos_w * cos_i * sin_W	+ sin_w * cos_W);
	rSat.y() = x * (cos_w * sin_W 	+ sin_w * cos_i * cos_W	)	+ y * (cos_w * cos_i * cos_W	- sin_w * sin_W);
	rSat.z() = x * (				+ sin_w * sin_i			)	+ y * (cos_w * sin_i);

	return rSat;
// 	std::cout << "\te " << e.transpose();
// 	std::cout << "\n" << "n0 " << n0.transpose();
// 	std::cout << "\tn1 " << n1.transpose();

// 	std::cout << "\tnu " << nu;

// 	std::cout << "\tE " << E;
// 	std::cout << "\tM " << M;

// 	double A = r / (1 - e_ * cos(E));
//
// 	dM = sqrt(GM_Earth /A/A/A);
}

void getKeplerPartials(
	Trace&		trace,
	VectorXd&	keplers0,
	MatrixXd&	partials)
{
	partials = MatrixXd::Zero(3, 6);

	double deltas[6] =
	{
		1, 1, 1, 0.00001, 0.00001, PI/180/1000
	};

	//get base position
	VectorEci pos0 = keplers2Inertial(trace, keplers0);

	for (int i = 0; i < 6; i++)
	{
		VectorXd keplers1 = keplers0;

		keplers1[i] += deltas[i];

		VectorEci pos1 = keplers2Inertial(trace, keplers1);

		partials.col(i) = (pos1 - pos0) / deltas[i];
	}
}


void getKeplerInversePartials(
	Trace&		trace,
	VectorEci&	pos,
	VectorEci&	vel,
	MatrixXd&	partials)
{
	double deltas[6] =
	{
		20000, 20000, 20000, 10, 10, 10
	};

	partials = MatrixXd::Zero(6, 6);

	//get base keplers
	Vector6d keplers0;
	bool pass = inertial2Keplers(std::cout, pos, vel, keplers0);

	for (int i = 0; i < 6; i++)
	{
		VectorEci pos1 = pos;
		VectorEci vel1 = vel;

		if (i < 3)		pos1[i]		+= deltas[i];
		else			vel1[i-3]	+= deltas[i];

		Vector6d keplers1;
		pass = inertial2Keplers(std::cout, pos1, vel1, keplers1);

		partials.col(i) = (keplers1 - keplers0) / deltas[i];
	}
}

VectorEci propagateEllipse(
			Trace&		trace,
			GTime		time,
			double		dt,
	const	VectorEci&	rSat,
	const	VectorEci&	vSat,
			SatPos&		satPos,
			bool		j2)
{
	ERPValues erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapper(time + dt, erpv);

	auto& ecef		= satPos.rSatCom;
	auto& vSatEcef	= satPos.satVel;

	if (dt == 0)
	{
		ecef = frameSwapper(rSat, &vSat, &vSatEcef);

		return rSat;
	}

	Vector6d keplers0;

	bool pass = inertial2Keplers(trace, rSat, vSat, keplers0);

	if (pass == false)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Failed to determine keplers for " << satPos.Sat.id() << " , "
		<< rSat.transpose().format(heavyFmt)
		<< vSat.transpose().format(heavyFmt);

		VectorEci newPos	= rSat
							+ vSat * dt;

		ecef = frameSwapper(newPos, &vSat, &vSatEcef);

		return newPos;
	}


	Vector3d L = Vector3d(
		keplers0[KEPLER::LX],
		keplers0[KEPLER::LY],
		keplers0[KEPLER::LZ]
	);

	double h = L.norm();

	Vector2d E = Vector2d(
		keplers0[KEPLER::EU],
		keplers0[KEPLER::EV]
	);

	double e = E.norm();

	double n = SQR(GM_Earth) / pow(h / sqrt(1 - SQR(e)), 3);		//2.82

	keplers0[KEPLER::M] += n * dt;

	VectorEci newPos1 = keplers2Inertial(trace, keplers0);

	if (j2)
	{
		double J2 = 0.00108263;

		VectorEci a;

		for (auto& rSat : {rSat, newPos1})
		{
			double R = rSat.norm();

			double x = rSat.x();
			double y = rSat.y();
			double z = rSat.z();

			double commonTerm = 5 * SQR(z/R);

			Vector3d vec = Vector3d(
				x * (commonTerm - 1),
				y * (commonTerm - 1),
				z * (commonTerm - 3)
			);

			a += 1.5 * J2 * GM_Earth * SQR(RE_MEAN) / (R * R * R * R * R) * vec;
		}

		a /= 2;

		newPos1 += 0.5 * a * SQR(dt);
	}

	double dtVel = 1e-4;

	keplers0[KEPLER::M] += n * dtVel;

	VectorEci velEci;
	if (1)
	{
		VectorEci newPos2 = keplers2Inertial(trace, keplers0);

		velEci = ((Vector3d) newPos2 - newPos1) / dtVel;
	}

// 	std::cout << "\nrSatInertial:       " << 		newPos.transpose();

	ecef = frameSwapper(newPos1, &velEci, &vSatEcef);

	return newPos1;
}

VectorEci propagateFull(
	Trace&		trace,
	GTime		time,
	double		dt,
	VectorEci&	rSat,
	VectorEci&	vSat,
	SatPos&		satPos)
{
	ERPValues erpv = getErp(nav.erp, time + dt);

	FrameSwapper frameSwapper(time + dt, erpv);

	auto& ecef		= satPos.rSatCom;
	auto& vSatEcef	= satPos.satVel;

	if (dt == 0)
	{
		ecef = frameSwapper(rSat, &vSat, &vSatEcef);

		return rSat;
	}

	OrbitState orbit;
	orbit.Sat = satPos.Sat;
	orbit.pos = rSat;
	orbit.vel = vSat;
	orbit.posVelSTM = MatrixXd::Identity(6, 6);

	Orbits orbits;
	orbits.push_back(orbit);

	OrbitIntegrator integrator;
	integrator.timeInit	= time;

	integrateOrbits(integrator, orbits, dt, acsConfig.propagationOptions.integrator_time_step);

	VectorEci newPos;
	VectorEci velEci;
	newPos	= orbits[0].pos;
	velEci	= orbits[0].vel;

	ecef = frameSwapper(newPos, &velEci, &vSatEcef);

	return newPos;
}
