
#include <functional>

#include "GNSSambres.hpp"
#include "biasSINEX.hpp"
#include "enums.h"



#define SSR_CBIA_VALID 3600.0
#define SSR_PBIA_VALID 300.0




map<E_Sys, E_ObsCode> defaultCodesL1 =
{
	{E_Sys::GPS, E_ObsCode::L1C},
	{E_Sys::GLO, E_ObsCode::L1C},
	{E_Sys::GAL, E_ObsCode::L1C},
	{E_Sys::CMP, E_ObsCode::L2I},
	{E_Sys::QZS, E_ObsCode::L1C}
};

map<E_Sys, E_ObsCode> defaultCodesL2 =
{
	{E_Sys::GPS, E_ObsCode::L2W},
	{E_Sys::GLO, E_ObsCode::L2C},
	{E_Sys::GAL, E_ObsCode::L5Q},
	{E_Sys::CMP, E_ObsCode::L7I},
	{E_Sys::QZS, E_ObsCode::L2L}
};

array<map<string, map<E_ObsCode, map<E_ObsCode, map<GTime, SinexBias, std::greater<GTime>>>>>, NUM_MEAS> SINEXBiases;		///< Multi dimensional map, as SINEXBiases[measType][id][code1][code2][time]
array<map<string, map<E_ObsCode, map<E_ObsCode, map<GTime, SinexBias, std::greater<GTime>>>>>, NUM_MEAS> SINEXBiases_out;	///< Multi dimensional map, as SINEXBiases[measType][id][code1][code2][time]

E_ObsCode str2code(
	string&		input,
	E_MeasType&	measType,
	double&		lam)
{
	const char* buff = input.c_str();
	char cods[] = "Lxx";
	cods[1] = buff[1];
	cods[2] = buff[2];

	E_ObsCode code;

	if 		(buff[0] == 'L') 	measType = PHAS;
	else if (buff[0] == 'C') 	measType = CODE;
	else
	{
		measType = CODE;
		code = E_ObsCode::NONE;
	}

	code = E_ObsCode::_from_string(cods);
	lam = lambdas[code];

	return code;
}

/* convert time string in bias SINEX to gtime struct */
GTime sinex_time_text(
	string& line)
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
		time = timeadd(time, 86400 * (doy - 1) + tod);
	}

	return time;
}

/* read header line in bias SINEX file */
void read_biasSINEX_head(
	const char* buff)
{
	/* This is the function to read the header, but data in header is of no use at the moment */
	return;
}

/* read data line in bias SINEX file */
int read_biasSINEX_line(
	char*		buff, 
	bias_io_opt	opt)
{
	int size = strlen(buff);

	if (size < 91)
	{
		fprintf(stderr, " Short bias line in SINEX file (%3d): %s\n", size, buff);
		return 0;
	}

	SinexBias entry;
	
	string type		(buff + 1,  4);
	string svn		(buff + 6,  4);
	string sat		(buff + 11, 3);
	string name		(buff + 15, 4);
	string code1str	(buff + 25, 3);
	string code2str	(buff + 30, 3);
	string startTime(buff + 35, 14);
	string endTime	(buff + 50, 14);
	string units	(buff + 65, 3);
	string biasStr	(buff + 70, 21);



	if		(type == "DSB ")	{	entry.biasType = E_BiasType::DSB;	}
	else if	(type == "OSB ")	{	entry.biasType = E_BiasType::OSB;	}
	else
	{
		return 0;
	}
	
	SatSys Sat(sat.c_str());
	
	string id; 
	if (name != "    ")
	{
		//this seems to be a receiver
		id = name;
		id += Sat.sysChar();
	}
	else if (sat != "   ")
	{
		//this should be a satellite, but check its valid		//todo aaron, system for receiver dcbs

		if	( Sat.prn == 0
			||Sat.sys == +E_Sys::NONE)
		{
				return 0;
		}
		
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
	entry.cod1 = str2code(code1str, entry.measType, lam1);
	entry.cod2 = str2code(code2str, dummy,			dummy2);

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

	return 1;
}

/* read single bias SINEX file */
int read_biasnx_fil(
	string&		filename,
	bias_io_opt	opt)
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
			if (read_biasSINEX_line(buff, opt) > 0) 
				nbia++;
		}
	}

	return nbia;
}

/* --------------- read and store values from SINEX files -----------------------
* args     :       char*   file    I   Files to read
*                  int     opt     I   bias I/O options
* ---------------------------------------------------------------------------*/
int read_bias_SINEX(
	string& file)
{
	bias_io_opt opt;
	opt.OSB_biases = acsConfig.ambrOpts.readOSB;
	opt.DSB_biases = acsConfig.ambrOpts.readDSB;
	opt.SSR_biases = false;
	opt.SAT_biases = acsConfig.ambrOpts.readSATbias;
	opt.REC_biases = acsConfig.ambrOpts.readRecBias;
	opt.HYB_biases = acsConfig.ambrOpts.readHYBbias;
	opt.COD_biases = true;
	opt.PHS_biases = true;

	int nbia = read_biasnx_fil(file, opt);

	return nbia;
}

bool getOSBBias(
			Trace&		trace,
			GTime		time,
	const	string&		id,
			E_MeasType	measType,
			SinexBias&	output,
			E_ObsCode	obsCode)
{	
	const int lvl = 4;
	
	//get the map of OSB biases for this ID and obsCode
	//OSB biases will only have one obsCode
	auto& biasMap = SINEXBiases[measType][id][obsCode][E_ObsCode::NONE];
	
	//find the last bias in that map that comes before the desired time
	auto biasIt = biasMap.lower_bound(time);
	if (biasIt == biasMap.end())
	{
		//not found
		return false;
	}
	
	auto& [startTime, bias] = *biasIt;
	
	if	( bias.tfin.time == 0 
		||timediff(bias.tfin, time) <= 0) 
	{
		//end time is satisfactory
		output = bias;
		tracepdeex(lvl, trace, "\nFound bias for %d %S  %s - %s", bias.cod1, measType == PHAS ? "phase" : "code ", bias.tini.to_string(0), bias.tfin.to_string(0));
		return true;
	}
	
	//end time was not satisfactory
	return false;
}

void setRestrictiveStartTime(
	GTime& current,
	GTime& potential)
{
	if (timediff(current, potential) > 0)
	{
		current = potential;
	}
}

void setRestrictiveEndTime(
	GTime& current,
	GTime& potential)
{
	if	( current.time == 0
		||timediff(current, potential) < 0)
	{
		current = potential;
	}		
}

bool dsbRecurser(
			Trace&			trace,
			GTime&			time,
			SinexBias&		output,
	const	E_ObsCode&		obsCode1,
	const	E_ObsCode&		obsCode2,
	const	map<E_ObsCode, 
			map<E_ObsCode, 
			map<GTime, SinexBias, std::greater<GTime>>>>& obsObsBiasMap,
			set<E_ObsCode>&	checkedObscodes)
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
		bool pass = dsbRecurser(trace, time, pathB, secondaryKey, obsCode2, obsObsBiasMap, checkedObscodes);
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

bool getDSBBias(
			Trace&		trace,
			GTime		time,
	const	string&		id,
			E_MeasType	measType,
			SinexBias&	output,
			E_ObsCode	obsCode1,
			E_ObsCode	obsCode2)
{
	const int lvl = 4;
	
	//get the basic map of DSB biases for this ID and measurmenet type
	auto& biasMap = SINEXBiases[measType][id];
	
	set<E_ObsCode> checkedObscodes;
	
	bool pass = dsbRecurser(trace, time, output, obsCode1, obsCode2, biasMap, checkedObscodes);
	
	return pass;
}
	
/** Search for hardware biases in phase and code
*/
void inpt_hard_bias(
	Trace& 			trace,		///< Trace to output to
	Obs& 			obs, 		///< Observation to find biases for
	E_ObsCode 		obsCode,	///< Specific code of observation to find biases for
	double* 		bias,		///< hardware bias
	double* 		var,		///< hardware bias variance
	bias_io_opt 	opt)		///< Specific biases to find
{
	const int lv = 3;
	
	GTime time = obs.time;
	SatNav& satNav	= *obs.satNav_ptr;

	bias[CODE] = 0;
	bias[PHAS] = 0;
	var [CODE] = 0;
	var [PHAS] = 0;
	
	if (opt.SSR_biases)
	{	
		auto codeIt = satNav.ssr.ssrCodeBias_map.lower_bound(time);
		if (codeIt != satNav.ssr.ssrCodeBias_map.end())
		{
			auto& [t, ssrbias] = *codeIt;
			
			if	(  opt.COD_biases 
				&& ssrbias.t0.time > 0
				&& fabs(timediff(time, ssrbias.t0)) < SSR_CBIA_VALID
				&& ssrbias.bias.find(obsCode) != ssrbias.bias.end())
			{
				bias[CODE] += -ssrbias.bias	[obsCode];
				var [CODE] +=  ssrbias.var	[obsCode];
			}
		}
		
		auto phasIt = satNav.ssr.ssrPhasBias_map.lower_bound(time);
		if (phasIt != satNav.ssr.ssrPhasBias_map.end())
		{
			auto& [t, ssrbias] = *phasIt;
			
			if	(  opt.PHS_biases 
				&& ssrbias.t0.time > 0
				&& fabs(timediff(time, ssrbias.t0)) < SSR_PBIA_VALID
				&& ssrbias.bias.find(obsCode) != ssrbias.bias.end())
			{
				bias[PHAS] += -ssrbias.bias	[obsCode];
				var [PHAS] +=  ssrbias.var	[obsCode];
			}
		}
	}

	if (opt.OSB_biases)
	{
		tracepdeex(lv, trace, "\nReading OSB bias for sat %s, code %s ... ", obs.Sat.id().c_str(), obsCode._to_string());
		
		if (opt.COD_biases)
		{
			SinexBias foundCodeBias;
			bool pass = getOSBBias(trace, time, obs.Sat.id(), CODE, foundCodeBias, obsCode);
			if (pass)
			{
				bias[CODE] += foundCodeBias.bias;
				var [CODE] += foundCodeBias.var;
				tracepdeex(lv, trace, "found CODE: %11.4f", foundCodeBias.bias);
			}
		}
		
		if (opt.PHS_biases)
		{
			SinexBias foundPhasBias;
			bool pass = getOSBBias(trace, time, obs.Sat.id(), PHAS, foundPhasBias, obsCode);
			if (pass)
			{
				bias[PHAS] += foundPhasBias.bias;
				var [PHAS] += foundPhasBias.var;
				tracepdeex(lv, trace, "found PHAS: %11.4f", foundPhasBias.bias);
			}
		}
	}

	if	( opt.DSB_biases
		&&opt.COD_biases)
	{
		tracepdeex(lv, trace, "\nReading DSB bias for sat %s, code %d ...", obs.Sat.id().c_str(), obsCode._to_string());
		
		auto sys = obs.Sat.sys;
		auto defaultCodeL1 = defaultCodesL1[sys];
		auto defaultCodeL2 = defaultCodesL2[sys];
		double lam1 = satNav.lamMap[ftypes[defaultCodeL1]];
		double lam2 = satNav.lamMap[ftypes[defaultCodeL2]];		//todo aaron remove later (LCs)
		
		double c2 = -SQR(lam1) / (SQR(lam2) - SQR(lam1));
		double c1 = c2 - 1;

		SinexBias foundCodeBias;
		bool pass = getDSBBias(trace, time, obs.Sat.id(), CODE, foundCodeBias, obsCode, defaultCodeL1);
		if (pass)
		{
			tracepdeex(lv, trace, "found CODE: %11.4f", foundCodeBias.bias);
			if (opt.LC12_correction)
			{
				SinexBias foundCodeBias2;
				pass = getDSBBias(trace, time, obs.Sat.id(), CODE, foundCodeBias2, defaultCodeL1, defaultCodeL2);
				
				foundCodeBias.bias	+=	   c2	* foundCodeBias2.bias;
				foundCodeBias.var	+= SQR(c2)	* foundCodeBias2.var;
// 				printf("\nadd %f, from %f", c2 * foundCodeBias2.bias, foundCodeBias2.bias);
			}
			
			bias[CODE] += foundCodeBias.bias;
			var [CODE] += foundCodeBias.var;
			tracepdeex(lv, trace, "found CODE: %11.4f", foundCodeBias.bias);
		}
		else
		{
			//biases not valid, try to generate
			auto ft = ftypes[obsCode];
			
			auto& rbias = stationRBiasMap[obs.mount][sys];
			
			tracepdeex(lv, trace, "DSB bias note found, looking for RINEX DCB ... ");
		
			if (satNav.cBias_P1_P2 != 0)
			{
				tracepdeex(lv, trace, "P1-P2 DCB found ... ");

				if (ft == F1)
				{
					if (opt.SAT_biases)			bias[CODE] += c2 * satNav.cBias_P1_P2;
					if (opt.REC_biases)			bias[CODE] += c2 * rbias[0];
				}

				if (ft == F2)
				{
					if (opt.SAT_biases)			bias[CODE] += c1 * satNav.cBias_P1_P2;
					if (opt.REC_biases)			bias[CODE] += c1 * rbias[0];
				}
			}
		
			if (obs.Sat.sys == +E_Sys::GPS)
			{
				if (obsCode == +E_ObsCode::L2C)
				{
					tracepdeex(lv, trace, "P2-C2 DCB found ... ");

					if (opt.SAT_biases)			bias[CODE] -= satNav.cBiasMap[F2];		//these are negative
					if (opt.REC_biases)			bias[CODE] -= rbias[1];
				}

				if (obsCode == +E_ObsCode::L1W)
				{
					tracepdeex(lv, trace, "P1-C1 DCB found ... ");

					if (opt.SAT_biases)			bias[CODE] += satNav.cBiasMap[F1];
					if (opt.REC_biases)			bias[CODE] += rbias[2];
				}
			}
			else if (obs.Sat.sys == +E_Sys::GLO)
			{
				if (obsCode == +E_ObsCode::L2P)
				{
					tracepdeex(lv, trace, "P2-C2 DCB found ... ");

					if (opt.SAT_biases)			bias[CODE] += satNav.cBiasMap[F2];
					if (opt.REC_biases)			bias[CODE] += rbias[1];
				}

				if (obsCode == +E_ObsCode::L1P)
				{
					tracepdeex(lv, trace, "P1-C1 DCB found ... ");

					if (opt.SAT_biases)			bias[CODE] += satNav.cBiasMap[F1];
					if (opt.REC_biases)			bias[CODE] += rbias[2];
				}
			}
		}
	}
}
