
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
#include "oceanTide.hpp"
#include "constants.hpp"
#include "iers2010.hpp"
#include "sofa.h"

OceanTide tide;

void OceanTide::readocetide()
{
	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << " Reading OCE";
	
	if (filename.empty())
	{
		return;
	}
	
	std::ifstream infile(filename);
	if (!infile)
	{
		return;
	}
	
	string line;
	/// Quick and dirty skip the 4 lines of header (@todo need to do better)
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
			for (auto& wave : TidalWaves)
			{
				if (wave.WaveName.compare(wave_) == 0)
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
				TidalWaves.push_back(TidalWave(wave_, doodson_, degMax));
				TidalWaves.back().CnmP(n_, m_) = cnmp_;
				TidalWaves.back().CnmM(n_, m_) = cnmm_;
				TidalWaves.back().SnmP(n_, m_) = snmp_;
				TidalWaves.back().SnmM(n_, m_) = snmm_;
			}
		}
	}
}

TidalWave::TidalWave(
	string	name, 
	string	darw, 
	int		degmax) 
:	WaveName(name)
{
	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << " Creating new wave " << WaveName << " degmax " << degmax;
	
	size_t dot = darw.find(".");
	
	Doodson = Array6d::Zero();	
	if (darw.length() >= 7)			Doodson(0) = std::stoi(darw.substr(dot - 3, 1));
	if (darw.length() >= 6)			Doodson(1) = std::stoi(darw.substr(dot - 2, 1));
	if (darw.length() >= 5)			Doodson(2) = std::stoi(darw.substr(dot - 1, 1));
									Doodson(3) = std::stoi(darw.substr(dot + 1, 1));
									Doodson(4) = std::stoi(darw.substr(dot + 2, 1));
									Doodson(5) = std::stoi(darw.substr(dot + 3, 1));
		
	for (int i = 1; i < 6; i++)		Doodson(i) -= 5;
	
	CnmM = MatrixXd::Zero(degmax + 1, degmax + 1);
	CnmP = MatrixXd::Zero(degmax + 1, degmax + 1);
	SnmM = MatrixXd::Zero(degmax + 1, degmax + 1);
	SnmP = MatrixXd::Zero(degmax + 1, degmax + 1);
}

/** Generates the Beta angles from the given a modified julian day.
 */
void OceanTide::setBeta(
	GTime	time,
	double	ut1_utc)
{
	FundamentalArgs fundArgs(time, ut1_utc);
	
	Beta(0) = fundArgs.gmst - fundArgs.f - fundArgs.omega;
	Beta(1) = fundArgs.f + fundArgs.omega;
	Beta(2) = Beta(1) - fundArgs.d;
	Beta(3) = Beta(1) - fundArgs.l;
	Beta(4) = -1 * fundArgs.omega;
	Beta(5) = Beta(1) - fundArgs.d - fundArgs.l_prime;
}

void OceanTide::getSPH(
	Array6d		dood, 
	MatrixXd&	Cnm, 
	MatrixXd&	Snm)
{
	for (auto& wave : TidalWaves)
	{
		double thetaf = (dood * wave.Doodson).sum();
		double cosThetaf = cos(thetaf);
		double sinThetaf = sin(thetaf);
		
		Cnm += ((wave.CnmP + wave.CnmM) * cosThetaf + (wave.SnmP + wave.SnmM) * sinThetaf) * 1e-11;
		Snm += ((wave.SnmP - wave.SnmM) * cosThetaf - (wave.CnmP - wave.CnmM) * sinThetaf) * 1e-11;
	}
	Snm.col(0).setZero();
	Snm.row(0).setZero();
}
