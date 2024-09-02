

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
#define     NGPT        2592		///< grid number

struct TropMapping
{
	double dryMap	= 0;
	double wetMap	= 0;
	double northMap	= 0;
	double eastMap	= 0;
};

struct TropStates
{
	double zenith	= 0;
	double grads[2]	= {};
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
	Trace&			trace,
	GTime			time,
	VectorPos&		pos,
	double			el,
	double&			dryZTD,
	double&			dryMap,
	double&			wetZTD,
	double&			wetMap,
	double&			var);

double tropSBAS(
	Trace&			trace,
	GTime			time,
	VectorPos&		pos,
	double			el,
	double&			dryZTD,
	double&			dryMap,
	double&			wetZTD,
	double&			wetMap,
	double&			var);

double tropVMF3(
	Trace&			trace,
	GTime			time,
	VectorPos&		pos,
	double			el,
	double&			dryZTD,
	double&			dryMap,
	double&			wetZTD,
	double&			wetMap,
	double&			var);

double tropGPT2(
	Trace&			trace,
	GTime			time,
	VectorPos&		pos,
	double			el,
	double&			dryZTD,
	double&			dryMap,
	double&			wetZTD,
	double&			wetMap,
	double&			var);

double tropCSSR(
	Trace&			trace,
	GTime			time,
	VectorPos&		pos,
	double			elv,
	double&			dryZTD,
	double&			dryMap,
	double&			wetZTD,
	double&			wetMap,
	double&			var);

double gradMapFn(
	double			el);

double tropModel(
	Trace&					trace,
	vector<E_TropModel> 	models,
	GTime					time,
	VectorPos&				pos,
	AzEl&					azel,
	TropStates&				tropStates,
	TropMapping&			dTropDx,
	double&					var);

double tropDryZTD(
	Trace&					trace,
	vector<E_TropModel> 	models,
	GTime					time,
	VectorPos&				pos);

double tropModelCoef(
	int ind,
	VectorPos&		pos);

void defineLocalTropBasis();

double heightAdjustWet(
	double hgt);

double heightAdjustDry(
	double hgt,
	double lat);
