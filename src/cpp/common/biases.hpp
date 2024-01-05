
#pragma once

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

struct ReceiverMap;

struct BiasEntry
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

	long int	posInOutFile =-1;					///< Position this entry is written in biasSINEX file
};

E_ObsCode str2code(
	string&		input,
	E_MeasType&	measType);

void pushBiasEntry(
	string		id,
	BiasEntry	entry);

void initialiseBias();

void addDefaultBias();

void cullOldBiases(
	GTime		time);

bool decomposeDSBBias(
	string		id,
	BiasEntry&	DSB);

bool decomposeTGDBias(
	SatSys		Sat,
	double		tgd);

bool decomposeBGDBias(
	SatSys		Sat,
	double		bgd1,
	double		bgd2);

bool readBiasSinex(
	string& file);

bool getBias(
	Trace& 			trace,
	GTime			time,
	string			id,
	SatSys			Sat,
	E_ObsCode 		obsCode1,
	E_MeasType		measType,
	double& 		bias,
	double& 		var,
	KFState*		kfState_ptr = nullptr);

void writeBiasSinex(
	Trace&			trace,
	GTime			time,
	KFState&		kfState,
	KFState&		ionState,
	string			biasfile,
	ReceiverMap&	receiverMap);

bool queryBiasOutput(
	Trace&		trace,
	GTime		time,
	KFState&	kfState,
	KFState&	ionState,
	SatSys		Sat,
	string		Rec,
	E_ObsCode	obsCode,
	double& 	bias_out,
	double& 	variance,
	E_MeasType	type);

void loadStateBiases(
	KFState&	kfState);

extern array<map<string, map<E_ObsCode, map<E_ObsCode, map<GTime, BiasEntry, std::greater<GTime>>>>>, NUM_MEAS_TYPES> biasMaps;
