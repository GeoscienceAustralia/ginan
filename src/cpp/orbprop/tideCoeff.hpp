
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
		string	name,
		string	doodson,
		int		degmax
	);

	string		waveName;
	MatrixXd	CnmP;
	MatrixXd	CnmM;
	MatrixXd	SnmP;
	MatrixXd	SnmM;
	MatrixXd	C1;
	MatrixXd	C2;
	MatrixXd	S1;
	MatrixXd	S2;
	ArrayXd		coeff;
	Array6d		doodson;
};

struct Tide
{
	string				filename;
	int					degMax;

	vector<TidalWave>	tidalWaves;
	Vector6d			beta;

	void read(
		const string&	filename,
		int				degMax);

	void setBeta(
		GTime	time,
		double	ut1_utc = 0);

	void getSPH(
		Array6d&	beta,
		MatrixXd&	Cnm,
		MatrixXd&	Snm);
};

extern Tide oceanTide;
extern Tide atmosphericTide;
