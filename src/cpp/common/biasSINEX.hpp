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

/** Struct for SINEX bias record
*-------------------------------------------------------------------------------
+BIAS/SOLUTION
*BIAS SVN_ PRN STATION__ OBS1 OBS2 BIAS_START____ BIAS_END______ UNIT __ESTIMATED_VALUE____ _STD_DEV___ __ESTIMATED_SLOPE____ _STD_DEV___
*/
struct SinexBias
{
	GTime		tini;								///< start time
	GTime		tfin;								///< end time
	E_MeasType	measType	= CODE;					///< Measurement type
	E_ObsCode	cod1		= E_ObsCode::NONE;		///< Measurement code 1
	E_ObsCode	cod2		= E_ObsCode::NONE;		///< Measurement code 2
	double		bias		= 0;					///< hardware bias in meters
	double		slop		= 0;					///< hardware bias slope in meters/second
	double		var			= 0;					///< hardware bias variance in meters^2
	double		slpv		= 0;					///< hardware bias slope variance in (meters/second)^2
	string		name		= "";					///< receiver name for receiver bias
	SatSys		Sat;								///< satellite prn for satellite bias / satellite system for receiver bias
	string		source		= "X";
};

struct Deriv_OSB
{
	GTime	tini;
	GTime	tfin;
	GTime	updated;
	int		iod			= 0;
	double	Lbias		= 0;			///< bias in meters
	double	Pbias		= 0;			///< bias in meters
	double	Lvar		= 0;			///< bias variance in meters^2
	double	Pvar		= 0;			///< bias variance in meters^2
	bool	LValid	= false;
	bool	PValid	= false;
};

struct bias_opt
{
	bool SSR_biases		= false;
	bool non_SSR_biases	= true;
};

E_ObsCode str2code(
	string&		input,
	E_MeasType&	measType,
	double&		lam);

string code2str(
	E_ObsCode	code, 
	E_MeasType	opt);

void pushBiasSinex(
	string		id,
	SinexBias	entry);

void initialiseBiasSinex();

void addDefaultBiasSinex();

bool decomposeDSBBias(
	SinexBias&	DSB,
	SinexBias&	OSB1,
	SinexBias&	OSB2);

bool decomposeTGDBias(
	SatSys		Sat,
	double		tgd);

bool decomposeBGDBias(
	SatSys		Sat,
	double		bgd1,
	double		bgd2);
	
int readBiasSinex(
	string& file);

bool getBiasSinex(
	Trace& 			trace,	
	GTime			time,
	string			id,
	SatSys			sys,
	E_ObsCode		obsCode1,
	E_ObsCode		obsCode2,
	E_MeasType		measType,
	double&			bias,	
	double&			var);

bool getBiasSinex(
	Trace& 			trace,
	GTime			time,
	string			id,
	SatSys			Sat,
	E_ObsCode 		obsCode1,
	E_MeasType		measType,
	double& 		bias,
	double& 		var);

void outp_bias(
	Trace&		trace,
	GTime		time,
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
	string		biasfile);



extern array<map<string, map<E_ObsCode, map<E_ObsCode, map<GTime, SinexBias, std::greater<GTime>>>>>, NUM_MEAS_TYPES> SINEXBiases;		///< Multi dimensional map, as SINEXBiases[measType][id][code1][code2][time]

extern map<KFKey, map<int,SinexBias>> SINEXBiases_out;		///< Multi dimensional map, as SINEXBiases_out[key][ind]


#endif
