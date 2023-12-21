
// #pragma GCC optimize ("O0")

#include <functional>

#include "constants.hpp"
#include "acsConfig.hpp"
#include "biases.hpp"
#include "enums.h"

/** Convert observation code string to enum code
*/
E_ObsCode str2code(
	string&		input,		///< The input observation code string
	E_MeasType&	measType)	///< Measurement type of this observation - CODE/PHAS - as output
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

	return code;
}

/** Convert time string in bias SINEX to gtime struct
*/
GTime sinex_time_text(
	string&		line,		///< line to read
	E_TimeSys	tsys)		///< time system
{
	double yds[3];
	GTime time = {};
	if (sscanf(line.c_str(), "%lf:%lf:%lf", &yds[0], &yds[1], &yds[2]) == 3)
	{
		time = yds2time(yds, tsys);
	}

	return time;
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
	char*		buff,	///< Line to read
	E_TimeSys	tsys)	///< time system "UTC", "TAI", etc.
{
	int size = strlen(buff);

	if (size < 91)
	{
		fprintf(stderr, " Short bias line in SINEX file (%3d): %s\n", size, buff);
		return 0;
	}

	if (tsys == +E_TimeSys::NONE)
	{
		fprintf(stderr, " Unkown time system for bias SINEX file: %s\n", tsys._to_string());
		return 0;
	}

	BiasEntry entry;
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
	if (acsConfig.process_sys[Sat.sys] == false)
	{
		return 0;
	}

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

	E_MeasType dummy;
	entry.cod1 = str2code(cod1str, entry.measType);
	entry.cod2 = str2code(cod2str, dummy);


	SatSys lamSat = Sat;
	if (lamSat.prn == 0)
	{
		lamSat.prn++;
	}

	int ft1 = code2Freq[Sat.sys][entry.cod1];
	double lam1 = nav.satNavMap[lamSat].lamMap[ft1];

	/* decoding start/end times */
	entry.tini = sinex_time_text(startTime,	tsys);
	entry.tfin = sinex_time_text(endTime,	tsys);

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
		for (int prn = 1; prn <= NSATGLO; prn++)
		{
			Sat.prn	= prn;
			id = entry.name + ":" + Sat.id();
			// entry.Sat = Sat;
			pushBiasEntry(id, entry);
		}
	}
	else if	( Sat.sys == +E_Sys::GLO
			&&Sat.prn != 0)
	{
		// this can be a receiver or satellite
		id = id + ":" + Sat.id();
		pushBiasEntry(id, entry);
	}
	else
	{
		// this can be a receiver or satellite
		id = id + ":" + Sat.sysChar();
		pushBiasEntry(id, entry);
	}

	return 1;
}

/** Read single bias SINEX file
*/
bool readBiasSinex(
	string&		filename)	///< File to read
{
	int nbia = 0;
	bool datasect = false;
	E_TimeSys tsys = E_TimeSys::NONE;

	std::ifstream inputStream(filename);
	if (!inputStream)
	{
		printf("Warning: could not find bias SINEX file %s \n", filename.c_str());
		return false;
	}

	string line;
	while (std::getline(inputStream, line))
	{
		char* buff = &line[0];

		if (buff[0] == '*')
			continue;       /* comment line */

		if (strstr(buff, "TIME_SYSTEM"))
		{
			string timeSystem(buff + 41,  1);
			if (timeSystem != " ")
			{
				if		(timeSystem == "G")		tsys = E_TimeSys::GPST;
				else if	(timeSystem == "C")		tsys = E_TimeSys::BDT;
				else if	(timeSystem == "R")		tsys = E_TimeSys::GLONASST;
				else if	(timeSystem == "U")		tsys = E_TimeSys::UTC;
				else if	(timeSystem == "T")		tsys = E_TimeSys::TAI;
				else
				{
					BOOST_LOG_TRIVIAL(warning) << "Warning: unsupported time system: " << timeSystem;
					return false;
				}
			}
		}

		if (strstr(buff, "%=BIA"))
		{
			read_biasSINEX_head(buff);
			continue;
		}

		if (strstr(buff, "%=ENDBIA"))
		{
			//fprintf(stdout,"\n ... %d biases read\n",nbia);
			return true;
		}

		if (strstr(buff, "%="))
		{
			printf(", Warning: erroneous bias SINEX file %s \n", filename.c_str());
			return false;
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
			if (read_biasSINEX_line(buff, tsys) > 0)
				nbia++;
		}
	}

	return true;
}
