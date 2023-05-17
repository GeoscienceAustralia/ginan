//
// Created by Sébastien Allgeyer on 28/2/22.
//

#ifndef GINAN_OCEANTIDE_HPP
#define GINAN_OCEANTIDE_HPP

#include <string>
#include <vector>
// #include <eigen3/Eigen/Core>
// #include <eigen3/Eigen/Dense>
#include "eigenIncluder.hpp"

struct TidalWave{
	TidalWave();
	TidalWave(
			std::string name,
			std::string doodson,
			int degmax
			);
	std::string WaveName;
	Eigen::MatrixXd CnmP;
	Eigen::MatrixXd CnmM;
	Eigen::MatrixXd SnmP;
	Eigen::MatrixXd SnmM;
	Eigen::ArrayXd coeff;
	Eigen::Array<double, 1, 6> Doodson;

};

struct OceanTide {
	OceanTide(){};
	OceanTide(
			std::string     filename_,
			int             degmax_ );
	void readocetide();
	void summary();
	void setBeta(double);
	std::string filename;
	int degMax;
	std::vector<TidalWave> TidalWaves;
	Eigen::Array<double, 1, 6> Beta;
	void getSPH(Eigen::Array<double, 1, 6> beta, MatrixXd & Cnm, MatrixXd & Snm);

};


#endif //GINAN_OCEANTIDE_HPP
