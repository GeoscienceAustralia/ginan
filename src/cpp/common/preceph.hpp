
#ifndef PRECEPH_HPP__
#define PRECEPH_HPP__

#include <iostream>
#include <string>
#include <list>

using std::string;
using std::list;

#include "streamTrace.hpp"
#include "antenna.hpp"
#include "satSys.hpp"
#include "gTime.hpp"

//forward declarations
struct nav_t;
struct Obs;
struct Peph;

int readdcb(string file, nav_t *nav);

bool peph2pos(
	Trace&		trace,
	GTime		time,
	SatSys&		Sat,
	Obs&		obs,
	nav_t& 		nav,
	bool		applyRelativity	= true);

void readSp3ToNav(
	string&	file, 
	nav_t*	nav, 
	int		opt);

bool readsp3(
	std::istream&	fileStream, 
	list<Peph>&		pephList,	
	int				opt,		
	bool&			isUTC,
	double*			bfact);

double	interppol(const double *x, double *y, int n);
void	orb2sp3(nav_t& nav);

int		pephclk(
	GTime	time,
	string	id,
	nav_t&	nav,
	double&	dtSat,
	double*	varc = nullptr);

#endif
