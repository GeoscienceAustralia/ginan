
// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>
#include <iostream>

#include "eigenIncluder.hpp"
#include "constants.hpp"
#include "boxwing.hpp"

#include "yaml-cpp/yaml.h"

Boxwing boxwing;

Vector3d  Boxwing::apply(
		string			blockType,	///< Block type string name
		const double	mass,		///< Mass of the spacecraft
		const Vector3d&	eD,			///< Direction of the sun (ECI)
		const Vector3d&	eX,			///< X axis of the spacecraft (ECI)
		const Vector3d&	eY,			///< Y axis of the spacecraft (ECI)
		const Vector3d&	eZ)			///< Z axis of the spacecraft (ECI)
{
	Vector3d acc = Vector3d::Zero();

	double S0 = 1367; /// Solar flux constant

	// trim trailing spaces of blockType
	blockType.erase(blockType.find_last_not_of(" \n\r\t") + 1);

	auto boxGeom_it = surfaces.find(blockType);
	if (boxGeom_it == surfaces.end())
	{
		return Vector3d::Zero();
	}

	auto [key, boxGeometry] = *boxGeom_it;

	for (auto& surf : boxGeometry)
	{
		double alpha	= surf.absorptionVisible;
		double delta	= surf.diffusionVisible;
		double rho		= surf.reflectionVisible;
		double kappa	= surf.hasThermalReemission;
		double s		= surf.shape;
		double A		= surf.area;

		Vector3d eN = surf.normal[0] * eX
					+ surf.normal[1] * eY
					+ surf.normal[2] * eZ;

		if (surf.rotationAxis.size() == 3)
		{
			double panelOrientation = surf.normal[0] + surf.normal[1] + surf.normal[2];	//will be +/- 1

			Vector3d rotAxis	= surf.rotationAxis[0] * eX
								+ surf.rotationAxis[1] * eY
								+ surf.rotationAxis[2] * eZ;

			Vector3d eT = rotAxis.cross(eN);

			Vector3d neweN = eD.dot(eN) * eN + eD.dot(eT) * eT;

			eN = panelOrientation * neweN.normalized();
		}

		double costheta = eD.dot(eN);
		if (costheta < 0)
		{
			costheta = 0;
		}

		acc += -A / mass * S0 / CLIGHT * costheta *
			((alpha + delta)														* eD
				+ (M_PI / 6 * s + 2.0 / 3.0 * (1 - s) * (delta + kappa * alpha))	* eN
				+ (4.0 / 3.0 * s + 2.0 * (1 - s)) * rho * costheta					* eN
			);
	}

	return acc;
}

void Boxwing::read(
	const string& filename)
{
	YAML::Node yamlFile = YAML::LoadFile(filename);

	for (const auto& entry : yamlFile)
	{
		string key = entry.first.as<string>();

		const YAML::Node& surfaceDataList = entry.second;

		// Loop through the list of surface data for each key
		for (const auto& surfaceNode : surfaceDataList)
		{
			SurfaceData surface;

			// Read surface data from YAML node
			surface.normal					= surfaceNode["normal"				].as<vector<double>>();
			surface.shape					= surfaceNode["shape"				].as<double>();
			surface.area					= surfaceNode["area"				].as<double>();
			surface.reflectionVisible		= surfaceNode["reflectionVisible"	].as<double>();
			surface.diffusionVisible		= surfaceNode["diffusionVisible"	].as<double>();
			surface.absorptionVisible		= surfaceNode["absorptionVisible"	].as<double>();
			surface.hasThermalReemission	= surfaceNode["hasThermalReemission"].as<double>();

			if (surfaceNode["rotationAxis"])
			{
				surface.rotationAxis = surfaceNode["rotationAxis"].as<vector<double>>();
				if (surface.rotationAxis.size() != 3)
				{
					BOOST_LOG_TRIVIAL(warning) << "Error: boxwing surface.rotationAxis is not a vector of size 3 for surface " << key << " in file " << filename << "!";
					continue;
				}
			}

			if (surface.normal.size() != 3)
			{
				BOOST_LOG_TRIVIAL(warning) << "Error: boxwing surface.normal is not a vector of size 3 for surface " << key << " in file " << filename << "!";
				continue;
			}

			surfaces[key].push_back(surface);
		}
	}
}
