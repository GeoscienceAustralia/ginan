#pragma once

#include <string>

using std::string;

#include "eigenIncluder.hpp"


struct OceanPoleTide 
{
	void read(
		const string&	filename,
		int				maxDeg);
	
	void estimate(
		double		m1,
		double		m2,
		MatrixXd&	Cnm, 
		MatrixXd&	Snm);

	bool initialized = false;
	MatrixXd cnmp;
	MatrixXd cnmm;
	MatrixXd snmp;
	MatrixXd snmm;

	const double gamma2_i = 0.0036;
	const double gamma2_r = 0.6870;
};

extern OceanPoleTide oceanPoleTide;


