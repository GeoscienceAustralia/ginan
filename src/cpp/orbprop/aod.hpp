#pragma once

#include "common/eigenIncluder.hpp"
#include "common/gTime.hpp"

#include <map>

using std::map;

struct AodData
{
	MatrixXd Cnm;
	MatrixXd Snm;
};

struct Aod
{
	void read(
		const string&	filename,
		int				maxDeg);
	
	void interpolate(
		GTime		time,
		MatrixXd&	Cnm, 
		MatrixXd&	Snm);
	
	std::map<GTime, AodData> data;
};

extern Aod aod;

