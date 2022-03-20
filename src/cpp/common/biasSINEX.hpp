#ifndef BIAS_SINEX_HPP
#define BIAS_SINEX_HPP

#include "navigation.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "satSys.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "enums.h"


#include <string>
#include <array>
#include <map>
#include <set>

using std::string;
using std::array;
using std::map;
using std::set;

struct SinexBias
{
	GTime		tini;
	GTime		tfin;
	E_BiasType	biasType	= E_BiasType::NONE;
	E_MeasType	measType	= CODE;					/* Measurement type */
	E_ObsCode	cod1		= E_ObsCode::NONE;		/* Measurment code 1 */
	E_ObsCode	cod2		= E_ObsCode::NONE;		/* Measurment code 2 */
	double		bias		= 0;					/* hardware bias in meters */
	double		slop		= 0;					/* hardware bias slope in meters/second */
	double		var			= 0;					/* hardware bias variance in meters^2 */
	double		slpv		= 0;					/* hardware bias slope variance in (meters/second)^2 */
	string		name;
	SatSys		Sat;
};

struct Deriv_OSB
{
	GTime	tini;
	GTime	tfin;
	GTime	updated;
	int		iod			= 0;
	double	Lbias		= 0;			/* bias in meters */
	double	Pbias		= 0;			/* bias in meters */
	double	Lvar		= 0;			/* bias variance in meters^2 */
	double	Pvar		= 0;			/* bias variance in meters^2 */
	bool	LValid	= false;
	bool	PValid	= false;
};

struct bias_io_opt
{
	bool SSR_biases = false;
	bool OSB_biases = false;
	bool DSB_biases = false;
	
	bool REC_biases = false;
	bool SAT_biases = false;
	bool HYB_biases = false;
	
	bool COD_biases = false;
	bool PHS_biases = false;
	
	bool LC12_correction = true;
};


int readBiasSinex(
	string& file);

void inpt_hard_bias(
	Trace& 			trace,	
	GTime			time,
	string			id,
	SatSys			sys,
	E_ObsCode 		obsCode,
	double* 		bias,	
	double* 		var,	
	bias_io_opt 	opt,	
	SatNav*			satNav_ptr = nullptr);



void outp_bias(
	Trace&		trace,
	GTime		time,
	E_BiasType	type, 
	string		receiver, 
	SatSys		Sat, 
	E_ObsCode	code1,
	E_ObsCode	code2, 
	double		bias, 
	double		variance,
	double		updat, 
	E_MeasType	measType);

int writeBiasSinex(
	Trace&		trace,
	GTime		time,
	string&		biasfile,
	bias_io_opt	opt);



extern map<string, map<E_Sys, array<double, 3>>> stationRBiasMap;
extern map<KFKey, map<int,SinexBias>> SINEXBiases_out;

//extern array<map<string, map<E_ObsCode, map<E_ObsCode, map<GTime, SinexBias, std::greater<GTime>>>>>, NUM_MEAS> SINEXBiases_out;	///< Multi dimensional map, as SINEXBiases[measType][id][code1][code2][time]
#endif
