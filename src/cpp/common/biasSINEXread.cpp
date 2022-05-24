
// #pragma GCC optimize ("O0")

#include <functional>

#include "biasSINEX.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "enums.h"


array<map<string, map<E_ObsCode, map<E_ObsCode, map<GTime, SinexBias, std::greater<GTime>>>>>, NUM_MEAS_TYPES> SINEXBiases;		///< Multi dimensional map, as SINEXBiases[measType][id][code1][code2][time]

/** Convert observation code string to enum code
*/
E_ObsCode str2code(
	string&		input,		///< The input observation code string
	E_MeasType&	measType,	///< Measurement type of this observation - CODE/PHAS - as output
	double&		lam)		///< Wave length of this signal as output
{
	char cods[] = "Lxx";
	cods[1] = input[1];
	cods[2] = input[2];

	E_ObsCode code = E_ObsCode::NONE;

	if 		(input[0] == 'L') 	measType = PHAS;
	else if (input[0] == 'C') 	measType = CODE;
	else
	{
		measType = CODE;
		code = E_ObsCode::NONE;
	}

	try
	{
		code = E_ObsCode::_from_string(cods);
	}
	catch (...)
	{
		code = E_ObsCode::NONE;
	}

	lam = lambdas[code];

	return code;
}

/** Convert enum observation code to code string
*/
string code2str(
	E_ObsCode	code,		///< The input enum observation code
	E_MeasType	measType)	///< Measurement type of this observation - CODE/PHAS
{
	if (code == +E_ObsCode::NONE)
		return "";

	string outstr = code._to_string();
	
	char head;
	if (measType == PHAS)	head='L';	
	if (measType == CODE)	head='C';
	
	outstr[0] = head;
	
	return outstr;
}

/** Convert time string in bias SINEX to gtime struct
*/
GTime sinex_time_text(
	string& line)			///< Start/End time string in bias SINEX
{
	double year;
	double doy;
	double tod;
	double ep[6] = {2000, 1, 1, 0, 0, 0};
	GTime time = {};

	if (sscanf(line.c_str(), "%lf:%lf:%lf", &year, &doy, &tod) == 3)
	{
		ep[0] = year;
		time = epoch2time(ep);
		time = time + 86400 * (doy - 1) + tod;
	}

	return time;
}

/** Initialise satellite DSBs between default signals, e.g. P1-P2 DCBs, with 0 values
*/
void initialiseBiasSinex()
{
	SinexBias entry;
	entry.tini.sec	= 0;
	entry.bias		= 0;
	entry.var		= 0;
	entry.measType	= CODE;
	entry.name		= "";
	entry.source	= "init";

	for (int i = 0; i < E_Sys::_size(); i++)
	{
		E_Sys sys	= E_Sys::_values()[i];

		auto sats = getSysSats(sys);
		if (acsConfig.process_sys[sys])
		for (auto Sat : sats) 
		{
			string id	= Sat.id() + ":" + Sat.sysChar();
			entry.Sat	= Sat;
			entry.cod1	= acsConfig.clock_codesL1[sys];
			entry.cod2	= acsConfig.clock_codesL2[sys];
			
			pushBiasSinex(id, entry);
		}
	}
}

/** Add default 0 values to DSBs between default signals, e.g. P1-P2 DCBs, for each bias ID
*/
void addDefaultBiasSinex()
{
	SinexBias entry;
	entry.tini.sec	= 0;
	entry.bias		= 0;
	entry.var		= 0;
	entry.measType	= CODE;
	entry.source	= "def";

	for (auto& [id, obsObsBiasMap] : SINEXBiases[entry.measType])
	{
		// get Sat and receiver name from the first entry in the CODE bias map 
		auto obsBiasMap	= obsObsBiasMap	.begin()->second;
		auto biasMap	= obsBiasMap	.begin()->second;
		auto bias		= biasMap		.begin()->second;

		entry.Sat	= bias.Sat;
		entry.name	= bias.name;
		entry.cod1	= acsConfig.clock_codesL1[bias.Sat.sys];
		entry.cod2	= acsConfig.clock_codesL2[bias.Sat.sys];
		
		pushBiasSinex(id, entry);
	}
	
	for (auto& Sat : getSysSats(E_Sys::GPS))
	{
		entry.cod1	= E_ObsCode::L1W;
		entry.cod2	= E_ObsCode::L1C;
		entry.Sat	= Sat;
		
		string id	= Sat.id() + ":" + Sat.sysChar();

		pushBiasSinex(id, entry);
	}
}

/** Push forward and reverse bias entry into the SINEXBiases map
*/
void pushBiasSinex(
	string		id,			///< Device ID
	SinexBias	entry)		///< Bias entry to push into the SINEXBiases map
{
	//add forward bias to maps
	SINEXBiases	[entry.measType]
				[id]
				[entry.cod1]
				[entry.cod2]
				[entry.tini] = entry;

	//create reverse bias and add to maps
	entry.bias *= -1;
	entry.slop *= -1;

	E_ObsCode swap = entry.cod1;
	entry.cod1 = entry.cod2;
	entry.cod2 = swap;

	SINEXBiases	[entry.measType]
				[id]
				[entry.cod1]
				[entry.cod2]
				[entry.tini] = entry;
}

/** Decompose DSB or DCB into OSBs
*/
bool decomposeDSBBias(
	SinexBias&	DSB,		///< DSB to be decomposed
	SinexBias&	OSB1,		///< OSB on L1 as output
	SinexBias&	OSB2)		///< OSB on L2 as output
{
	if 	( DSB.Sat.sys != +E_Sys::GPS
		||DSB.cod1 != +E_ObsCode::L1W
		||DSB.cod2 != +E_ObsCode::L2W)	//only GPS P1-P2 DCB can be decomposed at the moment
	{
		string cod1 = code2str(DSB.cod1, DSB.measType);
		string cod2 = code2str(DSB.cod2, DSB.measType);
		// printf("DSB %s-%s could not be decomposed at the moment\n", cod1.c_str(), cod2.c_str());

		return false;
	}

	double lam1	= lambdas[DSB.cod1];
	double lam2	= lambdas[DSB.cod2];
	double c2	= -SQR(lam1) / (SQR(lam2) - SQR(lam1));
	double c1	= c2 - 1;

	OSB1 = DSB;
	OSB1.cod1		= DSB.cod1;
	OSB1.cod2		= E_ObsCode::NONE;
	OSB1.bias		=     c2  * DSB.bias;
	OSB1.var		= SQR(c2) * DSB.var;

	OSB2 = DSB;
	OSB2.cod1		= DSB.cod2;
	OSB2.cod2		= E_ObsCode::NONE;
	OSB2.bias		=     c1  * DSB.bias;
	OSB2.var		= SQR(c1) * DSB.var;

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
	
	SinexBias entry;
	entry.tini.sec	= 1;
	entry.measType	= CODE;
	entry.Sat		= Sat;
	entry.name		= "";

	//store TGD as C1P-IF OSB
	entry.cod1		= cod1;
	entry.cod2		= E_ObsCode::NONE;
	entry.bias		= bias;
	entry.var		= 0;
	entry.source	= "tgd";

	pushBiasSinex(id, entry);
	
	if	( cod1 == acsConfig.clock_codesL1[sys]
		&&cod2 == acsConfig.clock_codesL2[sys])
	{
		//covert TGD to C2P-IF OSB
		entry.cod1		= cod2;
		entry.cod2		= E_ObsCode::NONE;
		entry.bias		= bias * gamma;
		entry.var		= 0;
		entry.source	= "tgd1";

		pushBiasSinex(id, entry);

		//covert TGD to P1-P2 DCB
		entry.cod1		= cod1;
		entry.cod2		= cod2;
		entry.bias		= bias * (1 - gamma);
		entry.var		= 0;
		entry.source	= "tgd2";

		pushBiasSinex(id, entry);
	}

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
		
	SinexBias entry;
	entry.tini.sec	= 1;
	entry.measType	= CODE;
	entry.Sat		= Sat;
	entry.name		= "";
	entry.source	= "bgd";

	//store BGD E5b/E1 as C1C-IF OSB
	entry.cod1		= cod1;
	entry.cod2		= E_ObsCode::NONE;
	entry.bias		= bgdE1E5a;
	entry.var		= 0;

	pushBiasSinex(id, entry);		//todo aaron, check which of these match the clock_codes and only create those.

	//covert BGD E5a/E1 to C5Q-IF OSB
	entry.cod1		= cod2;
	entry.cod2		= E_ObsCode::NONE;
	entry.bias		= bgdE1E5a * gammaE1E5a;
	entry.var		= 0;

	pushBiasSinex(id, entry);

	//covert BGD E5b/E1 to C7Q-IF OSB
	entry.cod1		= cod3;
	entry.cod2		= E_ObsCode::NONE;
	entry.bias		= bgdE1E5a - bgdE1E5b * (1 - gammaE1E5b);
	entry.var		= 0;

	pushBiasSinex(id, entry);

	//covert BGD E5b/E1 to C1C-C7Q DSB
	entry.cod1		= cod1;
	entry.cod2		= cod3;
	entry.bias		= bgdE1E5b * (1 - gammaE1E5b);
	entry.var		= 0;

	pushBiasSinex(id, entry);

	return true;
}

/** Read header line in bias SINEX file
*/
void read_biasSINEX_head(
	const char* buff)	///< Line to read
{
	/* This is the function to read the header, but data in header is of no use at the moment */
	return;
}

/** Read data line in bias SINEX file
*/
int read_biasSINEX_line(
	char*		buff)	///< Line to read
{
	int size = strlen(buff);

	if (size < 91)
	{
		fprintf(stderr, " Short bias line in SINEX file (%3d): %s\n", size, buff);
		return 0;
	}

	SinexBias entry;
	entry.source = "bsx";

	string type		(buff + 1,  4);
	string svn		(buff + 6,  4);
	string sat		(buff + 11, 3);
	string name		(buff + 15, 4);
	string cod1str	(buff + 25, 3);
	string cod2str	(buff + 30, 3);
	string startTime(buff + 35, 14);
	string endTime	(buff + 50, 14);
	string units	(buff + 65, 3);
	string biasStr	(buff + 70, 21);

	if	( type != "DSB "
		&&type != "OSB ")
	{
		return 0;
	}

	SatSys Sat(sat.c_str());

	string id;
	if (name != "    ")
	{
		//this seems to be a receiver, but may have satellite dependency for glonass
		entry.Sat  = Sat;
		entry.name = name;
		id = name;
	}
	else if (sat != "   ")
	{
		//this should be a satellite, but check its valid		//todo aaron, system for receiver dcbs

		if	( Sat.prn == 0
			||Sat.sys == +E_Sys::NONE)
		{
			return 0;
		}

		entry.Sat  = Sat;
		entry.name = "";
		id = sat;
	}
	else
	{
		//no valid identifier
		return 0;
	}
	
	double lam1		= 0;
	E_MeasType dummy;
	double dummy2	= 0;
	entry.cod1 = str2code(cod1str, entry.measType, lam1);
	entry.cod2 = str2code(cod2str, dummy,			dummy2);

	/* decoding start/end times */
	entry.tini = sinex_time_text(startTime);
	entry.tfin = sinex_time_text(endTime);

	/* decoding units */
	double fact = 0;

	if		(units == "ns ")											fact = CLIGHT / 1e9;
	else if (units == "cyc" && entry.measType == PHAS && lam1 > 0)		fact = lam1;
	else
	{
		return 0;
	}

	/* decoding bias */
	try
	{
		entry.bias = stod(biasStr, nullptr) * fact;
	}
	catch (const std::invalid_argument& ia)
	{
		fprintf(stderr, " Invalid bias in SINEX file: %s\n", buff);
		return 0;
	}

	/* reading/decoding standard deviation */
	if (strlen(buff) >= 103)
	{
		string stdstr(buff + 92, 11);

		try
		{
			double stdv = 0;
			stdv = stod(stdstr, nullptr) * fact;
			entry.var	= SQR(stdv);
		}
		catch (const std::invalid_argument& ia)
		{
			entry.var	= 0;
		}
	}

	entry.slop	= 0;
	entry.slpv	= 0;

	if	( Sat.sys == +E_Sys::GLO
		&&Sat.prn == 0)
	{
		// this seems to be a receiver
		// for ambiguous GLO receiver bias id (i.e. PRN not specified), duplicate bias entry for each satellite
		for (int prn = MINPRNGLO; prn <= MAXPRNGLO; prn++)
		{
			Sat.prn	= prn;
			id = entry.name + ":" + Sat.id();
			// entry.Sat = Sat;
			pushBiasSinex(id, entry);
		}
	}
	else if	( Sat.sys == +E_Sys::GLO
			&&Sat.prn != 0)
	{
		// this can be a receiver or satellite
		id = id + ":" + Sat.id();
		pushBiasSinex(id, entry);
	}
	else
	{
		// this can be a receiver or satellite
		id = id + ":" + Sat.sysChar();
		pushBiasSinex(id, entry);
	}

	return 1;
}

/** Read single bias SINEX file
*/
int read_biasnx_fil(
	string&		filename)	///< File to read
{
	int nbia = 0;
	bool datasect = false;

	std::ifstream inputStream(filename);
	if (!inputStream)
	{
		printf("Warning: could not find bias SINEX file %s \n", filename.c_str());
		return -1;
	}

	//fprintf(stdout,"\nReading bias SINEX file: %s \n",biasfile);

	string line;
	while (std::getline(inputStream, line))
	{
		char* buff = &line[0];

		if (buff[0] == '*')
			continue;       /* comment line */

		if (strstr(buff, "%=BIA"))
		{
			read_biasSINEX_head(buff);
			continue;
		}

		if (strstr(buff, "%=ENDBIA"))
		{
			//fprintf(stdout,"\n ... %d biases read\n",nbia);
			return nbia;
		}

		if (strstr(buff, "%="))
		{
			printf(", Warning: erroneous bias SINEX file %s \n", filename.c_str());
			return -1;
		}

		if (strstr(buff, "+FILE/REFERENCE"))
		{
			/* read bias description here... */
			continue;
		}

		if (strstr(buff, "+BIAS/DESCRIPTION"))
		{
			/* read bias description here... */
			continue;
		}

		if (strstr(buff, "+BIAS/SOLUTION"))
		{
			datasect = true;
			continue;
		}

		if (strstr(buff, "-BIAS/SOLUTION"))
		{
			datasect = false;
			continue;
		}

		if (!datasect)
			continue;

		if	(  strstr(buff, " DSB ")
			|| strstr(buff, " OSB "))
		{
			if (read_biasSINEX_line(buff) > 0)
				nbia++;
		}
	}

	return nbia;
}

/** Read and store values from SINEX files
*/
int readBiasSinex(
	string& file)	///< File to read
{
	int nbia = read_biasnx_fil(file);

	return nbia;
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
	if	( current.time == 0
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
			SinexBias&		output,				///< The bias entry retrieved
	const	E_ObsCode&		obsCode1,			///< Base code of observation to find biases for
	const	E_ObsCode&		obsCode2,			///< Secondary code of observation to find biases for
	const	map<E_ObsCode,
			map<E_ObsCode,
			map<GTime, SinexBias, std::greater<GTime>>>>& obsObsBiasMap,	///< Bias map for given measrement type & device, as obsObsBiasMap[code1][code2][time]
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

		SinexBias pathB;
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
		output.bias	+= pathB.bias;
		output.var	+= pathB.var;
		output.slop	+= pathB.slop;
		output.slpv	+= pathB.slpv;
		output.cod2	=  pathB.cod2;

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
bool getBias(
			Trace&		trace,				///< Trace to output to
			GTime		time,				///< Time of bias to look up
	const	string&		id,					///< The id of the device to retrieve the bias of
			E_MeasType	measType,			///< The measurement type to retrieve the bias of
			SinexBias&	output,				///< The bias entry retrieved
			E_ObsCode	obsCode1,			///< Base code of observation to find biases for
			E_ObsCode	obsCode2)			///< Secondary code of observation to find biases for
{
	//get the basic map of biases for this ID and measurmenet type
	try
	{
		auto& biasMap = SINEXBiases[measType].at(id);

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
bool getBiasSinex(
	Trace& 			trace,					///< Trace to output to
	GTime			time,					///< Time of bias to look up
	string			id,						///< The id of the device to retrieve the bias of
	SatSys			Sat,					///< The satellite to retrieve the bias of
	E_ObsCode 		obsCode1,				///< Base code of observation to find biases for
	E_ObsCode 		obsCode2,				///< Secondary code of observation to find biases for
	E_MeasType		measType,				///< Type of bias to retreive (code/phase)
	double&			bias,					///< Hardware bias
	double&			var)					///< Hardware bias variance
{
	const int lv = 4;

	bias	= 0;
	var		= 0;
	
	if (Sat.sys == +E_Sys::GLO)	id = id + ":" + Sat.id();
	else						id = id + ":" + Sat.sysChar();
	
	string type;
	if (measType == CODE)	type = "CODE";
	if (measType == PHAS)	type = "PHAS";

	tracepdeex(lv, trace, "Reading bias for %s, %s %4s-%4s ...", id.c_str(), type.c_str(), obsCode1._to_string(), obsCode2._to_string());

	SinexBias foundBias;

	bool pass = getBias(trace, time, id, measType, foundBias, obsCode1, obsCode2);
	if (pass == false)
	{
		tracepdeex(lv, trace, " %s:   not found,            ", type.c_str());
		return false;
	}
	
	bias	= foundBias.bias;
	var		= foundBias.var;
	tracepdeex(lv, trace, " %s: %11.4f from %6s,", type.c_str(), foundBias.bias, foundBias.source.c_str());
	
	return true;
}

/** Search for absolute hardware biases in phase and code
*/
bool getBiasSinex(
	Trace& 			trace,		///< Trace to output to
	GTime			time,		///< Time of bias to look up
	string			id,			///< The id of the device to retrieve the bias of
	SatSys			Sat,		///< The satellite to retrieve the bias of
	E_ObsCode 		obsCode1,	///< Base code of observation to find biases for
	E_MeasType		measType,	///< Type of bias to retreive (code/phase)
	double& 		bias,		///< Hardware bias
	double& 		var)		///< Hardware bias variance
{
	return getBiasSinex(trace, time, id, Sat, obsCode1, E_ObsCode::NONE, measType, bias, var);
}
