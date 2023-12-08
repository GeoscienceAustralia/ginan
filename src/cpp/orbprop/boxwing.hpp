
#pragma once

#include <vector>
#include <string>
#include <map>

#include "eigenIncluder.hpp"

using std::vector;
using std::string;
using std::map;

struct SurfaceData
{
	vector<double>	normal					= {0,0,0};
	vector<double>	rotationAxis			= {}; // Optional field
	double			shape					= 0;
	double			area					= 0;
	double			reflectionVisible		= 0;
	double			diffusionVisible		= 0;
	double			absorptionVisible		= 0;
	double			hasThermalReemission	= 0;
};

struct Boxwing
{
	Vector3d  apply(
		string			blockType,
		const double	mass,
		const Vector3d&	eD,
		const Vector3d&	eX,
		const Vector3d&	eY,
		const Vector3d&	eZ);

	void read(
		const string& filename);

	map<string, vector<SurfaceData>> surfaces;

};

extern Boxwing boxwing;
