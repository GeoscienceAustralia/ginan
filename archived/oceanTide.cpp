/**
 * @file oceanTide.cpp
 * @author Sébastien Allgeyer (sallgeyer@frontiersi.com.au)
 * @brief 
 * @version 0.1
 * @date 2022-03-23
 * @ingroup orbprop
 * 
 * @copyright Copyright (c) 2022
 * 
 */

#include "oceanTide.hpp"
#include <boost/log/trivial.hpp>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string.hpp>

#include "sofa.h"

OceanTide::OceanTide(std::string filename_, int degmax_): filename(filename_), degMax(degmax_)
{
	readocetide();
	summary();
};

void OceanTide::readocetide() {
	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << " Reading OCE" << __PRETTY_FUNCTION__ ;
	std::ifstream infile(filename);
	std::string wave_, doodson_, line;
	int n_,m_;
	double cnmp_, snmp_, cnmm_, snmm_;
	/// Quick and dirty skip the 4 lines of header (@todo need to do better)
	for (int i=0; i<4; i++)
		std::getline(infile, line);
	while (std::getline(infile, line))
	{
//		std::cout << line << std::endl;
		std::istringstream iss(line);
		iss >> doodson_ >> wave_ >> n_ >> m_ >> cnmp_ >> snmp_ >> cnmm_ >> snmm_;
		// cout << doodson_ << wave_ << "\n";
		if (n_ <= degMax) {
			bool isnew = true;
//			std::cout << wave_ << std::endl;
			for (auto & wave: TidalWaves) {
				// BOOST_LOG_TRIVIAL(debug) <<  wave.WaveName.compare(wave_) << " " << wave.WaveName << " .vs. " << wave_ ;
				if (wave.WaveName.compare(wave_) == 0) {
					wave.CnmP(n_, m_) = cnmp_;
					wave.SnmP(n_, m_) = snmp_;
					wave.CnmM(n_, m_) = cnmm_;
					wave.SnmM(n_, m_) = snmm_;
					isnew = false;
				}

			}
			if (isnew) {
				TidalWaves.push_back(TidalWave(wave_, doodson_, degMax));
				TidalWaves.back().CnmP(n_, m_) = cnmp_;
				TidalWaves.back().CnmM(n_, m_) = cnmm_;
				TidalWaves.back().SnmP(n_, m_) = snmp_;
				TidalWaves.back().SnmM(n_, m_) = snmm_;				
			}
		}
	}

}

void OceanTide::summary() {
	BOOST_LOG_TRIVIAL(debug) << "write summary here";
}


TidalWave::TidalWave(std::string name, std::string darw, int degmax) : WaveName(name)
{
	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << " Creating new wave " << WaveName << " degmax " << degmax;
	size_t dot = darw.find(".");
	Doodson = Eigen::Array<double, 1, 6>::Zero(6);
	Doodson(5) = std::stoi(darw.substr(dot+3, 1));
	Doodson(4) = std::stoi(darw.substr(dot+2, 1));
	Doodson(3) = std::stoi(darw.substr(dot+1, 1));
	if (darw.length()>=5)
		Doodson(2) = std::stoi(darw.substr(dot-1, 1));
	if (darw.length()>=6)
		Doodson(1) =  std::stoi(darw.substr(dot-2, 1));
	if (darw.length()>=7)
		Doodson(0) =  std::stoi(darw.substr(dot-3, 1));
	for (int i=1; i<6; i++)
		Doodson(i) -= 5;
	BOOST_LOG_TRIVIAL(debug) << "\t" << Doodson.transpose();

	CnmM = Eigen::MatrixXd::Zero(degmax+1, degmax+1);
	CnmP = Eigen::MatrixXd::Zero(degmax+1, degmax+1);
	SnmM = Eigen::MatrixXd::Zero(degmax+1, degmax+1);
	SnmP = Eigen::MatrixXd::Zero(degmax+1, degmax+1);

}

/**
 * @brief Generates the Beta angles from the given a modified julian day.
 * @todo implement the ut1_utc.
 */
void OceanTide::setBeta(double mjd)//, double ut1_utc)
{
	
	double jd = mjd + 2400000.5;
	double teph = ( jd - 2451545.0 ) / 36525.0;
	Eigen::Array<double,6,1> fundArguments;
	fundArguments(0) = iauFal03(teph);
	fundArguments(1) = iauFalp03(teph);
	fundArguments(2) = iauFaf03(teph);
	fundArguments(3) = iauFad03(teph);
	fundArguments(4) = iauFaom03(teph);

	//theta
	double uta, utb, tta, ttb;
	uta = 2400000.5;
	tta = 2400000.5;
	int iy, im, id;
	double fd;
	int err;
	err = iauJd2cal(2400000.5, mjd, &iy, &im, &id, &fd);
	double tai_utc;
	err = iauDat(iy, im, id, fd, &tai_utc);
	ttb = mjd;
	double mjd_utc = mjd + 32.184 + tai_utc;
	utb = mjd_utc ; //+ ut1_utc / 86400.0; //Should have the ut1 utc value, probably too small
	double gmst00 = iauGmst00(uta, utb, tta, ttb);
	Beta(0) = gmst00 + M_PI - fundArguments(2) - fundArguments(4);
	Beta(1) = fundArguments(2) + fundArguments(4);
	Beta(2) = Beta(1) - fundArguments(3);
	Beta(3) = Beta(1) - fundArguments(0);
	Beta(4) = -1 * fundArguments(4);
	Beta(5) = Beta(1) - fundArguments(3) - fundArguments(1);

}

void OceanTide::getSPH(Eigen::Array<double, 1, 6> dood, MatrixXd & Cnm, MatrixXd & Snm)
{
	for (auto & wave : TidalWaves )
	{
		double thetaf = (dood * wave.Doodson).sum();
		Cnm += ((wave.CnmP + wave.CnmM) * cos(thetaf) + (wave.SnmP + wave.SnmM) * sin(thetaf))*1e-11;
		Snm += ((wave.SnmP - wave.SnmM) * cos(thetaf) - (wave.CnmP - wave.CnmM) * sin(thetaf))*1e-11;
		// BOOST_LOG_TRIVIAL(debug) << "doods " << dood;
		// BOOST_LOG_TRIVIAL(debug) << "mult  " << wave.Doodson;
		// BOOST_LOG_TRIVIAL(debug) << wave.CnmP ;
		// BOOST_LOG_TRIVIAL(debug) << "thetaf" << thetaf;
		// BOOST_LOG_TRIVIAL(debug) << "\n" << Cnm << "\n" << Snm ;
		// exit(0);
	}
	Snm.col(0) *= 0.0;
	Snm.row(0) *= 0.0;
}
