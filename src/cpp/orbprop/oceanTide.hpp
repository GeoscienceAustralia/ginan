
#pragma once

#include <string>
#include <vector>

using std::string;
using std::vector;

#include "eigenIncluder.hpp"

struct TidalWave
{
	TidalWave();

	TidalWave(
		string name,
		string doodson,
		int degmax
	);

	string		WaveName;
	MatrixXd	CnmP;
	MatrixXd	CnmM;
	MatrixXd	SnmP;
	MatrixXd	SnmM;
	ArrayXd		coeff;
	Array6d		Doodson;
};

struct OceanTide
{
	string				filename;
	int					degMax;
	
	vector<TidalWave>	TidalWaves;
	Vector6d			Beta;

	OceanTide() 
	{
	}
	
	void readocetide();
	
	void setBeta(
		GTime	time,
		double	ut1_utc = 0);

	void getSPH(
		Array6d		beta, 
		MatrixXd&	Cnm, 
		MatrixXd&	Snm);
};

extern OceanTide tide;
