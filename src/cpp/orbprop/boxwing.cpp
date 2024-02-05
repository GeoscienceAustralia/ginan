
// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>
#include <iostream>

#include "eigenIncluder.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "boxwing.hpp"

Vector3d  applyBoxwingSrp(
		const OrbitOptions&	orbitOptions,	///< Orbit options detailing surfaces
		const Vector3d&		eD,				///< Direction of the sun (ECI)
		const Vector3d&		eX,				///< X axis of the spacecraft (ECI)
		const Vector3d&		eY,				///< Y axis of the spacecraft (ECI)
		const Vector3d&		eZ)				///< Z axis of the spacecraft (ECI)
{
	Vector3d acc = Vector3d::Zero();

	double S0 = 1367; /// Solar flux constant

	for (auto& surf : orbitOptions.surface_details)
	{
		double alpha	= surf.absorption_visible;
		double delta	= surf.diffusion_visible;
		double rho		= surf.reflection_visible;
		double kappa	= surf.thermal_reemission;
		double s		= surf.shape;
		double A		= surf.area;

		Vector3d eN = surf.normal[0] * eX
					+ surf.normal[1] * eY
					+ surf.normal[2] * eZ;

		if (surf.rotation_axis.size() == 3)
		{
			double panelOrientation = surf.normal[0] + surf.normal[1] + surf.normal[2];	//will be +/- 1

			Vector3d rotAxis	= surf.rotation_axis[0] * eX
								+ surf.rotation_axis[1] * eY
								+ surf.rotation_axis[2] * eZ;

			Vector3d eT = rotAxis.cross(eN);

			Vector3d neweN = eD.dot(eN) * eN + eD.dot(eT) * eT;

			eN = panelOrientation * neweN.normalized();
		}

		double costheta = eD.dot(eN);
		if (costheta < 0)
		{
			costheta = 0;
		}

		acc += -A / orbitOptions.mass * S0 / CLIGHT * costheta *
			((alpha + delta)														* eD
				+ (M_PI / 6 * s + 2.0 / 3.0 * (1 - s) * (delta + kappa * alpha))	* eN
				+ (4.0 / 3.0 * s + 2.0 * (1 - s)) * rho * costheta					* eN
			);
	}

	return acc;
}
