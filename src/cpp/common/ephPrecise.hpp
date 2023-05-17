
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
	Navigation*	nav, 
	int			opt);

bool readsp3(
	std::istream&	fileStream, 
	vector<Peph>&	pephList,	
	int				opt,		
	E_TimeSys&		tsys,
	double*			bfact);

double	interpolate(const double *x, double *y, int n);
void	orb2sp3(Navigation& nav);


bool pephPos(
	Trace&		trace,
	GTime		time,
	SatPos&		satPos,
	Navigation&	nav);

bool pephClk(
	Trace&		trace,
	GTime		time,
	SatPos&		satPos,
	Navigation&	nav);

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
