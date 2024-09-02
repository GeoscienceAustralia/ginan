
#include <boost/log/trivial.hpp>

#include <fstream>
#include <sstream>
#include <iostream>

#include "oceanPoleTide.hpp"

OceanPoleTide oceanPoleTide;


void OceanPoleTide::read(
	const string&	filename,
	int				maxDeg)
{
	if (filename.empty())
	{
		return;
	}

	std::ifstream infile(filename);
	if (!infile)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Ocean pole tide file open error " << filename;

		return;
	}

	cnmp = MatrixXd::Zero(maxDeg + 1, maxDeg + 1);
	cnmm = MatrixXd::Zero(maxDeg + 1, maxDeg + 1);
	snmp = MatrixXd::Zero(maxDeg + 1, maxDeg + 1);
	snmm = MatrixXd::Zero(maxDeg + 1, maxDeg + 1);

	string line;
	std::getline(infile, line);
	while (std::getline(infile, line))
	{
		if (line[0] == '#')
		{
			continue;
		}

		std::istringstream iss(line);
		int n;
		int m;
		double cnmp_;
		double cnmm_;
		double snmp_;
		double snmm_;
		iss >> n >> m >> cnmp_ >> cnmm_ >> snmp_ >> snmm_;
		if (n <= maxDeg)
		{
			cnmp(n, m) = cnmp_;
			cnmm(n, m) = cnmm_;
			snmp(n, m) = snmp_;
			snmm(n, m) = snmm_;
		}
	}
	initialized = true;
}

void OceanPoleTide::estimate(
	double		m1,
	double		m2,
	MatrixXd&	Cnm,
	MatrixXd&	Snm)
{
	double coeff1 = m1 * gamma2_r + m2 * gamma2_i;
	double coeff2 = m2 * gamma2_r - m1 * gamma2_i;
	coeff1 *= M_PI / 180 / 3600;
	coeff2 *= M_PI / 180 / 3600;
	Cnm += coeff1 * cnmp + coeff2 * cnmm;
	Snm += coeff1 * snmp + coeff2 * snmm;
}
