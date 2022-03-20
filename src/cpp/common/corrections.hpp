#ifndef __CORRECTIONS__HPP__
#define __CORRECTIONS__HPP__

#include <string>

using std::string;

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "gTime.hpp"


struct nav_t;

/* atmosphere models ---------------------------------------------------------*/
double	ionmodel(GTime t, const double *ion, const double *pos, const double *azel);
double	ionmapf(Vector3d&rr, const double el);
double	ionppp(const double *pos, const double *azel, double re, double hion, double *pppos);
int		iontec(GTime time, const nav_t *nav, const double *pos, const double *azel, int opt, double&delay, double&var);
void	readtec(string file, nav_t *nav);

double relativity1(
	Vector3d& rSat,
	Vector3d& satVel);
	
void obsVariances(
	ObsList& obsList);

#endif
