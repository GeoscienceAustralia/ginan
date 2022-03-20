/*------------------------------------------------------------------------------
* trop.c : troposphere functions
*
*							CRCSI-GA ACS Project
*
* reference:
*			[1] Troposphere mapping functions for GPS and very long baseline
*               interferometry from European Centre for Medium-Range Weather
*               Forecasts operational analysis data, J. Geoph. Res.,
*               Boehm, et al, 2006
*           [2] vmf1.m file from http://ggosatm.hg.tuwien.ac.at/DELAY/SOURCE/
*           [3] gpt2 file from http://ggosatm.hg.tuwien.ac.at/DELAY/SOURCE/
*-----------------------------------------------------------------------------*/
#ifndef TROP_H
#define TROP_H

#include "gTime.hpp"

#include <string>

using std::string;

#define     NGPT        2592                /* grid number */

/** gpt grid file contents
 */
struct gptgrid_t
{
	double lat	[NGPT];				///< lat grid (degree) 
	double lon	[NGPT];				///< lon grid (degree) 
	double pres	[NGPT][5];			///< pressure										a0 A1 B1 A2 B2 (pascal) 
	double temp	[NGPT][5];			///< temperature									a0 A1 B1 A2 B2 (kelvin) 
	double humid[NGPT][5];			///< humidity										a0 A1 B1 A2 B2 (kg/kg) 
	double tlaps[NGPT][5];			///< elapse rate									a0 A1 B1 A2 B2 (kelvin/m) 
	double ah	[NGPT][5];			///< hydrostatic	mapping function coefficient	a0 A1 B1 A2 B2 
	double aw	[NGPT][5];			///< wet			mapping function coefficient	a0 A1 B1 A2 B2 
	double undu	[NGPT];				///< geoid undulation (m) 
	double hgt	[NGPT];				///< orthometric height (m) 
	int ind;						///< indicator, 0-fail, 1-success 
};


void	gpt2(const gptgrid_t *gptg, double mjd, double lat, double lon, double hell, int it, double gptval[7]);

double tropztd(
	const gptgrid_t&	gptg,
	double				pos[3],
	double				mjd,
	double				el,
	int					it,
	double				mf[2],
	double&				zwd);

void	vmf1(const double ah, const double aw, double mjd, double lat, double hgt, double zd, int id, double mf[2]);
int		readgrid(string file, gptgrid_t *gptg);

double	tropmodel(GTime time, const double *pos, const double *azel, double humi);

double	tropacs(
	const double*	pos, 
	const double*	azel,
	double*			map = nullptr);

double	tropmapf(GTime time, const double *pos, const double *azel,double *mapfw);
int		tropcorr(GTime time, const double *pos, const double *azel, int tropopt, double *trp, double *var);


#endif
