
// #pragma GCC optimize ("O0")

#include <functional>

#include "constants.hpp"
#include "acsConfig.hpp"
#include "biases.hpp"
#include "enums.h"


array<map<string, map<E_ObsCode, map<E_ObsCode, map<GTime, BiasEntry, std::greater<GTime>>>>>, NUM_MEAS_TYPES> biasMaps;		///< Multi dimensional map, as biasMaps[measType][id][code1][code2][time]


/** Initialise satellite DSBs between default signals, e.g. P1-P2 DCBs, with 0 values
*/
void initialiseBias()
{
	BiasEntry entry;
	entry.bias		= 0;
	entry.var		= 0;
	entry.measType	= CODE;
	entry.name		= "";
	entry.source	= "init";

	for (E_Sys sys : E_Sys::_values())
	{
		auto sats = getSysSats(sys);
		if (acsConfig.process_sys[sys])
		for (auto Sat : sats)
		{
			string id	= Sat.id() + ":" + string(1,Sat.sysChar());
			entry.Sat	= Sat;
// 			entry.cod1	= acsConfig.clock_codesL1[sys];
// 			entry.cod2	= acsConfig.clock_codesL2[sys];

			pushBiasEntry(id, entry);
		}
	}
}

/** Add default 0 values to DSBs between default signals, e.g. P1-P2 DCBs, for each bias ID
*/
void addDefaultBias()
{
	BiasEntry entry;
	entry.bias		= 0;
	entry.var		= 0;
	entry.measType	= CODE;

	for (auto& [id, obsObsBiasMap] : biasMaps[entry.measType])
	{
		// get Sat and receiver name from the first entry in the CODE bias map
		auto obsBiasMap	= obsObsBiasMap	.begin()->second;
		auto biasMap	= obsBiasMap	.begin()->second;
		auto bias		= biasMap		.begin()->second;

		entry.Sat		= bias.Sat;
		entry.name		= bias.name;
// 		entry.cod1		= acsConfig.clock_codesL1[bias.Sat.sys];
// 		entry.cod2		= acsConfig.clock_codesL2[bias.Sat.sys];
		entry.source	= "def1";

// 		pushBiasEntry(id, entry);	//todo aaron, disabled
	}

	for (auto& Sat : getSysSats(E_Sys::GPS))
	{
		entry.cod1		= E_ObsCode::L1W;
		entry.cod2		= E_ObsCode::L1C;
		entry.Sat		= Sat;
		entry.source	= "def2";

		string id	= Sat.id() + ":" + Sat.sysChar();

		pushBiasEntry(id, entry);
	}
}

void loadStateBiases(
	KFState&	kfState)
{
	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		if	( kfKey.type != +KF::CODE_BIAS
			&&kfKey.type != +KF::PHASE_BIAS)
		{
			continue;
		}

		BiasEntry entry;
		if (kfKey.type == +KF::CODE_BIAS)		entry.measType = CODE;
		if (kfKey.type == +KF::PHASE_BIAS)		entry.measType = PHAS;

		entry.tini		= kfState.time;
		entry.cod1		= E_ObsCode::_from_integral(kfKey.num);
		entry.bias		= kfState.x(index);
		entry.var		= kfState.P(index,index);
		entry.name		= kfKey.str;
		entry.Sat		= kfKey.Sat;
		entry.source	= "KALMAN";

		string id;
		if (entry.name.empty() == false)	id = entry.name;
		else								id = entry.Sat.id();
		id.push_back(':');
		id.push_back(entry.Sat.sysChar());

		pushBiasEntry(id, entry);
	}
}

/** Push forward and reverse bias entry into biasMaps
*/
void pushBiasEntry(
	string		id,			///< Device ID
	BiasEntry	entry)		///< Bias entry to push into biasMaps
{
	//add forward bias to maps
	biasMaps	[entry.measType]
				[id]
				[entry.cod1]
				[entry.cod2]
				[entry.tini] = entry;

	decomposeDSBBias(id, entry);

	//create reverse bias and add to maps
	entry.bias *= -1;
	entry.slop *= -1;

	E_ObsCode swap = entry.cod1;
	entry.cod1 = entry.cod2;
	entry.cod2 = swap;

	biasMaps	[entry.measType]
				[id]
				[entry.cod1]
				[entry.cod2]
				[entry.tini] = entry;

	decomposeDSBBias(id, entry);
}

void cullOldBiases(
	GTime time)
{
	for (auto& typeBiasMap				: biasMaps)
	for (auto& [dummy1, idBiasMap]		: typeBiasMap)
	for (auto& [dummy2, code1BiasMap]	: idBiasMap)
	for (auto& [dummy3, code2BiasMap]	: code1BiasMap)
	{
		auto foundIt = code2BiasMap.lower_bound(time);

		if (foundIt == code2BiasMap.end())
		{
			continue;
		}

		foundIt++;

		//delete all before this found one (after since its reversed ordering)
		code2BiasMap.erase(foundIt, code2BiasMap.end());
	}
}

/** Decompose DSB or DCB into OSBs
*/
bool decomposeDSBBias(
	string		id,			///< ID of the bias
	BiasEntry&	DSB)		///< DSB to be decomposed
{
	auto& Sat = DSB.Sat;

// 	if	( DSB.cod1 != acsConfig.clock_codesL1[Sat.sys]
// 		||DSB.cod2 != acsConfig.clock_codesL2[Sat.sys])
// 	{
// 		return false;
// 	}

	E_FType ft1 = code2Freq[Sat.sys][DSB.cod1];
	E_FType ft2 = code2Freq[Sat.sys][DSB.cod2];
	double lam1	= genericWavelength[ft1];
	double lam2	= genericWavelength[ft2];

	if	(  lam1 == 0
		|| lam2 == 0)
	{
		return false;
	}

	if (lam1 == lam2)
	{
		return false;
	}

	double c2	= -SQR(lam1) / (SQR(lam2) - SQR(lam1));
	double c1	= c2 - 1;

	BiasEntry entry = DSB;
	entry.cod2		= E_ObsCode::NONE;
	entry.source	+= "*";

	entry.cod1		= DSB.cod1;
	entry.bias		=     c2  * DSB.bias;
	entry.var		= SQR(c2) * DSB.var;

	pushBiasEntry(id, entry);

	entry.cod1		= DSB.cod2;
	entry.bias		=     c1  * DSB.bias;
	entry.var		= SQR(c1) * DSB.var;

	pushBiasEntry(id, entry);

	return true;
}

/** Convert GPS/QZS TGD into OSBs & DSB
*/
bool decomposeTGDBias(
	SatSys		Sat,	///< The satellite to decompose the bias of
	double		tgd)	///< GPS or QZS TGD to be decomposed
{
	auto sys = Sat.sys;

	E_ObsCode cod1 = E_ObsCode::NONE;
	E_ObsCode cod2 = E_ObsCode::NONE;
	if		(sys == +E_Sys::GPS)		{	cod1 = E_ObsCode::L1W;	cod2 = E_ObsCode::L2W;	}
	else if	(sys == +E_Sys::QZS)		{	cod1 = E_ObsCode::L1C;	cod2 = E_ObsCode::L2L;	}
	else									return false;

	string	id		= Sat.id() + ":" + Sat.sysChar();
	double	bias	= tgd * CLIGHT;
	double	gamma	= SQR(FREQ1) / SQR(FREQ2);

	BiasEntry entry;
	entry.tini.bigTime	= 1;
	entry.measType		= CODE;
	entry.Sat			= Sat;
	entry.name			= Sat.id();

	//covert TGD to P1-P2 DCB
	entry.cod1		= cod1;
	entry.cod2		= cod2;
	entry.bias		= bias * (1 - gamma);
	entry.var		= 0;
	entry.source	= "tgd";

	pushBiasEntry(id, entry);

	return true;
}

/** Convert GAL BGDs into OSBs & DSB
*/
bool decomposeBGDBias(
	SatSys		Sat,	///< The satellite to decompose the bias of
	double		bgd1,	///< BGD E5a/E1 to be decomposed
	double		bgd2)	///< BGD E5b/E1 to be decomposed
{
	E_ObsCode cod1 = E_ObsCode::L1C;
	E_ObsCode cod2 = E_ObsCode::L5Q;
	E_ObsCode cod3 = E_ObsCode::L7Q;

	string	id			= Sat.id() + ":" + Sat.sysChar();
	double	bgdE1E5a	= bgd1 * CLIGHT;
	double	bgdE1E5b	= bgd2 * CLIGHT;
	double	gammaE1E5a	= SQR(FREQ1) / SQR(FREQ5);
	double	gammaE1E5b	= SQR(FREQ1) / SQR(FREQ7);

	BiasEntry entry;
	entry.tini.bigTime	= 1;
	entry.measType		= CODE;
	entry.Sat			= Sat;
	entry.name			= "";
	entry.source		= "bgd";

	//store BGD E5b/E1 as C1C-IF OSB
	entry.cod1		= cod1;
	entry.cod2		= E_ObsCode::NONE;
	entry.bias		= bgdE1E5a;
	entry.var		= 0;

	pushBiasEntry(id, entry);		//todo aaron, check which of these match the clock_codes and only create those.

	//covert BGD E5a/E1 to C5Q-IF OSB
	entry.cod1		= cod2;
	entry.cod2		= E_ObsCode::NONE;
	entry.bias		= bgdE1E5a * gammaE1E5a;
	entry.var		= 0;

	pushBiasEntry(id, entry);

	//covert BGD E5b/E1 to C7Q-IF OSB
	entry.cod1		= cod3;
	entry.cod2		= E_ObsCode::NONE;
	entry.bias		= bgdE1E5a - bgdE1E5b * (1 - gammaE1E5b);
	entry.var		= 0;

	pushBiasEntry(id, entry);

	//covert BGD E5b/E1 to C1C-C7Q DSB
	entry.cod1		= cod1;
	entry.cod2		= cod3;
	entry.bias		= bgdE1E5b * (1 - gammaE1E5b);
	entry.var		= 0;

	pushBiasEntry(id, entry);

	return true;
}


void setRestrictiveStartTime(
	GTime& current,
	GTime& potential)
{
	if (current > potential)
	{
		current = potential;
	}
}

void setRestrictiveEndTime(
	GTime& current,
	GTime& potential)
{
	if	( current.bigTime == 0
		||current < potential)
	{
		current = potential;
	}
}

/** Recurser of bias chaining, i.e. searching the path between base code and secondary code
*/
bool biasRecurser(
			Trace&			trace,				///< Trace to output to
			GTime&			time,				///< Time of bias to look up
			BiasEntry&		output,				///< The bias entry retrieved
	const	E_ObsCode&		obsCode1,			///< Base code of observation to find biases for
	const	E_ObsCode&		obsCode2,			///< Secondary code of observation to find biases for
	const	map<E_ObsCode,
			map<E_ObsCode,
			map<GTime,
				BiasEntry, std::greater<GTime>>>>& obsObsBiasMap,	///< Bias map for given measrement type & device, as obsObsBiasMap[code1][code2][time]
			set<E_ObsCode>&	checkedObscodes)	///< A list of all checked observation codes
{
	checkedObscodes.insert(obsCode1);

	//try to find the base key in the big map
	auto it1 = obsObsBiasMap.find(obsCode1);
	if (it1 == obsObsBiasMap.end())
	{
		//the obscode was not found, we have no hope
		return false;
	}
	auto& [dummy, obsBiasMap] = *it1;

	//try to find the secondary key in the sub map
	auto it2 = obsBiasMap.find(obsCode2);
	if (it2 != obsBiasMap.end())
	{
		//the obscode was found, use it

		auto& [dummy, timeBiasMap] = *it2;


		//find the last bias in that map that comes before the desired time
		auto biasIt = timeBiasMap.lower_bound(time);
		if (biasIt != timeBiasMap.end())
		{
			//a valid time entry was found, use it

			auto& [dummy, bias] = *biasIt;

			output = bias;
			return true;
		}
	}

	//we didnt find what we were looking for with this set of obscodes,
	//try to find a different path to the destination
	//use the base key we found, and check all of it's siblings
	for (auto& [secondaryKey, primarySecondaryTimeBiasMap] : obsBiasMap)
	{
		if (checkedObscodes.count(secondaryKey))
		{
			//already checked
			continue;
		}

		BiasEntry pathB;
		bool pass = biasRecurser(trace, time, pathB, secondaryKey, obsCode2, obsObsBiasMap, checkedObscodes);
		if (pass == false)
			continue;

		//a valid secondary path was found, now get the first half at the correct time

		//find the last bias in that map that comes before the desired time
		auto biasIt = primarySecondaryTimeBiasMap.lower_bound(time);
		if (biasIt == primarySecondaryTimeBiasMap.end())
		{
			//a valid time entry was not found, try the next path
			continue;
		}

		//we've found both sides of the path, join them
		auto& [dummy, pathA] = *biasIt;
		output = pathA;
		output.bias		+= pathB.bias;
		output.var		+= pathB.var;
		output.slop		+= pathB.slop;
		output.slpv		+= pathB.slpv;
		output.source	+= "," + pathB.source;
		output.cod2		=  pathB.cod2;

// 		printf("\nTraversing %s %s %s %f %f %f",
// 			   pathA.cod1._to_string(),
// 			   pathA.cod2._to_string(),
// 			   pathB.cod2._to_string(),
// 			   pathA.bias,
// 			   pathB.bias,
// 			   output.bias);

		setRestrictiveStartTime	(output.tini, pathB.tini);
		setRestrictiveEndTime	(output.tfin, pathB.tfin);

		return true;
	}

	return false;
}

/** Search for hardware bias for given measurmenet type
*/
bool getBiasEntry(
			Trace&		trace,				///< Trace to output to
			GTime		time,				///< Time of bias to look up
	const	string&		id,					///< The id of the device to retrieve the bias of
			E_MeasType	measType,			///< The measurement type to retrieve the bias of
			BiasEntry&	output,				///< The bias entry retrieved
			E_ObsCode	obsCode1,			///< Base code of observation to find biases for
			E_ObsCode	obsCode2)			///< Secondary code of observation to find biases for
{
	//get the basic map of biases for this ID and measurmenet type
	try
	{
		auto& biasMap = biasMaps[measType].at(id);

		set<E_ObsCode> checkedObscodes;

		bool pass = biasRecurser(trace, time, output, obsCode1, obsCode2, biasMap, checkedObscodes);

		return pass;
	}
	catch (...)
	{
		return false;
	}
}

/** Search for hardware biases in phase and code
*/
bool getBias(
	Trace& 			trace,					///< Trace to output to
	GTime			time,					///< Time of bias to look up
	string			id,						///< The id of the device to retrieve the bias of
	SatSys			Sat,					///< The satellite to retrieve the bias of
	E_ObsCode 		obsCode1,				///< Base code of observation to find biases for
	E_MeasType		measType,				///< Type of bias to retreive (code/phase)
	double&			bias,					///< Hardware bias
	double&			var,					///< Hardware bias variance
	KFState*		kfState_ptr)			///< Optional filter to search for biases
{
	E_ObsCode obsCode2 = E_ObsCode::NONE;

	if	(  acsConfig.ssrInOpts.one_freq_phase_bias
		&& measType == PHAS)
	{
		E_FType freq = code2Freq[Sat.sys][obsCode1];

		obsCode1 = freq2CodeHax(Sat.sys, freq);
	}

	if (kfState_ptr)
	{
		auto& kfState = *kfState_ptr;

		KFKey kfKey;
		if (measType == CODE)	kfKey.type		= KF::CODE_BIAS;
		if (measType == PHAS)	kfKey.type		= KF::PHASE_BIAS;

// 		kfKey.str		= id;
// 		kfKey.Sat		= SatSys(Sat.sys);
		kfKey.Sat		= Sat;
		kfKey.num		= obsCode1;

		bool found = kfState.getKFValue(kfKey, bias, &var);

		if (found)
		{
			return true;
		}
	}

	if (Sat.sys == +E_Sys::GLO)	id = id + ":" + Sat.id();
	else						id = id + ":" + Sat.sysChar();

	string type;
	if (measType == CODE)	type = "CODE";
	if (measType == PHAS)	type = "PHAS";

	tracepdeex(3, trace, "\nReading %s bias for %6s, %4s-%4s ...", type.c_str(), id.c_str(), obsCode1._to_string(), obsCode2._to_string());

	BiasEntry foundBias;
	bool pass = getBiasEntry(trace, time, id, measType, foundBias, obsCode1, obsCode2);
	if (pass == false)
	{
		tracepdeex(3, trace, " Not found,          var: %5.1f", var);
		return false;
	}

	bias	= foundBias.bias;
	var		= foundBias.var;
	tracepdeex(3, trace, " Found: %11.4f, var: %5.1f from %s", bias, var, foundBias.source.c_str());

	return true;
}


