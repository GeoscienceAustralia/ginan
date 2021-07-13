
#ifndef PRECEPH_HPP__
#define PRECEPH_HPP__

#include <string>
#include <list>

using std::string;
using std::list;

#include "antenna.hpp"
#include "gTime.hpp"
#include "satSys.hpp"

//forward declarations
struct nav_t;
struct Obs;

int readdcb(string file, nav_t *nav);
int peph2pos(
	Trace&		trace,
	GTime		time,
	SatSys&		Sat,
	nav_t&		nav,
	int			opt,
	Obs&		obs,
	PcoMapType*	pcoMap_ptr);


void 	readsp3(string& file, nav_t *nav, int opt);
int  	readsap(string& file, GTime time, nav_t *nav);
// int  	readfcb(string& file, nav_t *nav);
double	interppol(const double *x, double *y, int n);
void	orb2sp3(nav_t& nav);

int		pephclk(GTime time, string id, nav_t& nav, double *dts, double *varc);

#endif
