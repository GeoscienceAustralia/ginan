
#pragma once

#include <iostream>
#include <string>

using std::string;

#include "satSys.hpp"
#include "gTime.hpp"
#include "trace.hpp"

//forward declarations
struct Navigation;
struct SatPos;
struct GObs;
struct Peph;

int readdcb(string file);

void readSp3ToNav(
	string&		file,
	Navigation&	nav,
	int			opt);

bool readsp3(
	std::istream&	fileStream,
	vector<Peph>&	pephList,
	int				opt,
	E_TimeSys&		tsys,
	double*			bfact);

double	interpolate(const double *x, double *y, int n);

/** polynomial interpolation by Neville's algorithm.
 *  Sketchy formatting to only require +, * operators on TYPE
 */
template<typename TYPE>
TYPE interpolate(
	vector<double>&		x,
	vector<TYPE>&		y)
{
	for (int j = 1; j < x.size();		j++)
	for (int i = 0; i < x.size() - j;	i++)
	{
		y[i] = (y[i] * x[i+j] + y[i+1] * x[i] * -1) * (1 / (x[i+j] - x[i]));
	}

	return y[0];
}

bool pephclk(
	Trace&		trace,
	GTime		time,
	string		id,
	Navigation&	nav,
	double&		dtSat,
	double*		varc = nullptr);

bool pephpos(
	Trace&		trace,
	GTime		time,
	SatSys		Sat,
	Navigation&	nav,
	Vector3d&	rSat,
	double*		vare = nullptr);
