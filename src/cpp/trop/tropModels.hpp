

#pragma once

#include "eigenIncluder.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "enums.h"

#include <string>
using std::string;

#define 	ERR_TROP	3.0			///< tropspheric delay std (m) 
#define     NGPT        2592                /* grid number */

struct TropMapping
{
	double wetMap	= 0;
	double northMap	= 0;
	double eastMap	= 0;
	double dryMap	= 0;
};

double mapHerring(
	double el, 
	double a, 
	double b, 
	double c);

void readvmf3(
	string			filepath);

void readorog(
	string			filepath);

void readgrid(
	string			filepath);

double tropSAAS( 
	GTime			time,
	VectorPos&		pos,
	double			el,
	double&			dryZTD,
	double&			dryMap,
	double&			wetZTD,
	double&			wetMap,
	double&			var);
	
double tropSBAS( 
	GTime			time,
	VectorPos&		pos,
	double			el,
	double&			dryZTD,
	double&			dryMap,
	double&			wetZTD,
	double&			wetMap,
	double&			var);

double tropVMF3( 
	GTime			time,
	VectorPos&		pos,
	double			el,
	double&			dryZTD,
	double&			dryMap,
	double&			wetZTD,
	double&			wetMap);

double tropGPT2( 
	GTime			time,
	VectorPos&		pos,
	double			el,
	double&			dryZTD,
	double&			dryMap,
	double&			wetZTD,
	double&			wetMap);

double gradMapFn(
	double			el);

double tropModel(
	Trace&			trace,
	E_TropModel 	model,
	GTime			time,
	VectorPos&		pos,
	double*			azel,
	double*			tropStates,
	TropMapping&	dTropDx,
	double&			var);

double tropDryZTD(
	E_TropModel 	model,
	GTime			time,
	VectorPos&		pos);

double tropModelCoef(
	int ind, 
	VectorPos&		pos);

void defineLocalTropBasis();

double heightAdjustWet( 
	double hgt);

double heightAdjustDry( 
	double hgt,
	double lat);
