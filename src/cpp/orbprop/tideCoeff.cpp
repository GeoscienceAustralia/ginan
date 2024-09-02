
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using std::string;
using std::vector;

#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

#include "coordinates.hpp"
#include "tideCoeff.hpp"
#include "constants.hpp"
#include "iers2010.hpp"
#include "sofa.h"

Tide oceanTide;
Tide atmosphericTide;

void Tide::read(
	const string&	filename,
	int				degMax)
{
	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << " Reading tide";

	if (filename.empty())
	{
		return;
	}

	std::ifstream infile(filename);
	if (!infile)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Tide file open error " << filename;

		return;
	}

	string line;
	// Skip the 4 lines of header
	for (int i = 0; i < 4; i++)
		std::getline(infile, line);

	while (std::getline(infile, line))
	{
		std::istringstream iss(line);

		string wave_;
		string doodson_;
		int n_;
		int m_;
		double cnmp_;
		double snmp_;
		double cnmm_;
		double snmm_;
		iss >> doodson_ >> wave_ >> n_ >> m_ >> cnmp_ >> snmp_ >> cnmm_ >> snmm_;
		if (n_ <= degMax)
		{
			bool isnew = true;
			for (auto& wave : tidalWaves)
			{
				if (wave.waveName == wave_)
				{
					wave.CnmP(n_, m_) = cnmp_;
					wave.SnmP(n_, m_) = snmp_;
					wave.CnmM(n_, m_) = cnmm_;
					wave.SnmM(n_, m_) = snmm_;
					isnew = false;
				}
			}

			if (isnew)
			{
				tidalWaves.push_back(TidalWave(wave_, doodson_, degMax));
				tidalWaves.back().CnmP(n_, m_) = cnmp_;
				tidalWaves.back().CnmM(n_, m_) = cnmm_;
				tidalWaves.back().SnmP(n_, m_) = snmp_;
				tidalWaves.back().SnmM(n_, m_) = snmm_;
			}
		}
	}

	for (auto& wave : tidalWaves)
	{
		wave.C1 = wave.CnmP + wave.CnmM;
		wave.C2 = wave.SnmP + wave.SnmM;
		wave.S1 = wave.SnmP - wave.SnmM;
		wave.S2 = wave.CnmP - wave.CnmM;
	}
}

TidalWave::TidalWave(
	string	name,
	string	darw,
	int		degmax)
:	waveName(name)
{
	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << " Creating new wave " << waveName << " degmax " << degmax;

	size_t dot = darw.find(".");

	doodson = Array6d::Zero();
	if (darw.length() >= 7)			doodson(0) = std::stoi(darw.substr(dot - 3, 1));
	if (darw.length() >= 6)			doodson(1) = std::stoi(darw.substr(dot - 2, 1));
	if (darw.length() >= 5)			doodson(2) = std::stoi(darw.substr(dot - 1, 1));
									doodson(3) = std::stoi(darw.substr(dot + 1, 1));
									doodson(4) = std::stoi(darw.substr(dot + 2, 1));
									doodson(5) = std::stoi(darw.substr(dot + 3, 1));

	for (int i = 1; i < 6; i++)		doodson(i) -= 5;

	CnmM = MatrixXd::Zero(degmax + 1, degmax + 1);
	CnmP = MatrixXd::Zero(degmax + 1, degmax + 1);
	SnmM = MatrixXd::Zero(degmax + 1, degmax + 1);
	SnmP = MatrixXd::Zero(degmax + 1, degmax + 1);
}

/** Generates the Beta angles from the given a modified julian day.
 */
void Tide::setBeta(
	GTime	time,
	double	ut1_utc)
{
	FundamentalArgs fundArgs(time, ut1_utc);

	beta(0) = fundArgs.gmst - fundArgs.f - fundArgs.omega;		//todo aaron, swap with doodson?
	beta(1) = fundArgs.f + fundArgs.omega;
	beta(2) = beta(1) - fundArgs.d;
	beta(3) = beta(1) - fundArgs.l;
	beta(4) = -1 * fundArgs.omega;
	beta(5) = beta(1) - fundArgs.d - fundArgs.l_prime;
}

void Tide::getSPH(
	Array6d&	dood,
	MatrixXd&	Cnm,
	MatrixXd&	Snm)
{
	for (auto& wave : tidalWaves)
	{
		double thetaf = (dood * wave.doodson).sum();
		double cosThetaf = cos(thetaf);
		double sinThetaf = sin(thetaf);

		Cnm += (wave.C1 * cosThetaf + wave.C2 * sinThetaf) * 1e-11;
		Snm += (wave.S1 * cosThetaf - wave.S2 * sinThetaf) * 1e-11;
	}
	Snm.col(0).setZero();
	Snm.row(0).setZero();
}
