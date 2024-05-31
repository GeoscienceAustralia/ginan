
// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>
#include <iostream>

#include "common.hpp"
#include "eigenIncluder.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "boxwing.hpp"

Vector3d getNormal(
		const Vector3d &eD,
		const Vector3d &eX,
		const Vector3d &eY,
		const Vector3d &eZ,
		const SurfaceDetails &surf)
{
	Vector3d eN = surf.normal[0] * eX
				+ surf.normal[1] * eY
				+ surf.normal[2] * eZ;

	if (surf.rotation_axis.empty())
		return eN;

	double panelOrientation	= surf.normal[0]
							+ surf.normal[1]
							+ surf.normal[2]; //will be +/- 1

	Vector3d rotAxis	= surf.rotation_axis[0] * eX
						+ surf.rotation_axis[1] * eY
						+ surf.rotation_axis[2] * eZ;

	Vector3d eT = rotAxis.cross(eN);

	Vector3d neweN	= eD.dot(eN) * eN
					+ eD.dot(eT) * eT;

	eN = panelOrientation * neweN.normalized();

	return eN;
}

Vector3d calculateAcceleration(
		const Vector3d&	direction,
		const Vector3d&	eN,
		const double	mass,
		const double	area,
		const double	alpha,
		const double	delta,
		const double	rho,
		const double	kappa,
		const double	s)
{
	Vector3d acc = Vector3d::Zero();

	double S0 = 1367; // Solar flux constant

	double costheta = direction.dot(eN);
	if (costheta < 0)
	{
		costheta = 0;
	}

	acc += -area / mass * S0 / CLIGHT * costheta *
		((alpha + delta) * direction
			+ (M_PI / 6 * s + 2.0 / 3.0 * (1 - s) * (delta + kappa * alpha)) * eN
			+ (4.0 / 3.0 * s + 2.0 * (1 - s)) * rho * costheta * eN
		);

	return acc;
}

Vector3d applyBoxwingSrp(
		const OrbitOptions&	orbitOptions,
		const Vector3d&		eD,
		const Vector3d&		eX,
		const Vector3d&		eY,
		const Vector3d&		eZ)
{
	Vector3d acceleration = Vector3d::Zero();
	for (auto& surf : orbitOptions.surface_details)
	{
		double alpha	= surf.absorption_visible;
		double delta	= surf.diffusion_visible;
		double rho		= surf.reflection_visible;
		double kappa	= surf.thermal_reemission;
		double s		= surf.shape;

		Vector3d eN = getNormal(eD, eX, eY, eZ, surf);

		acceleration += calculateAcceleration(eD, eN, orbitOptions.mass, surf.area, alpha, delta, rho, kappa, s);
	}
	return acceleration;
}

Vector3d applyBoxwingAlbedo(
		const OrbitOptions&	orbitOptions,
		const double		E_Vis,
		const double		E_IR,
		const Vector3d&		rsat,
		const Vector3d&		eD,
		const Vector3d&		eX,
		const Vector3d&		eY,
		const Vector3d&		eZ)
{
	Vector3d acceleration = Vector3d::Zero();
	Vector3d er = rsat.normalized();

	for (auto& surf : orbitOptions.surface_details)
	{
		double alpha	= surf.absorption_visible	* E_Vis + surf.absorption_infrared	* E_IR;
		double delta	= surf.diffusion_visible	* E_Vis + surf.diffusion_infrared	* E_IR;
		double rho		= surf.reflection_visible	* E_Vis + surf.reflection_infrared	* E_IR;
		double kappa	= surf.thermal_reemission;
		double s		= surf.shape;

		Vector3d eN = getNormal(eD, eX, eY, eZ, surf);

		acceleration += calculateAcceleration(er, eN, orbitOptions.mass, surf.area, alpha, delta, rho, kappa, s);
	}

	return acceleration;
}
