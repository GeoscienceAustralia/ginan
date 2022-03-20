#include <algorithm>
#include <fstream>
#include <vector>
#include <math.h>
#include <map>

// #pragma GCC optimize ("O0")

using std::vector;
using std::pair;

#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "observations.hpp"
#include "streamTrace.hpp"
#include "acsConfig.hpp"
#include "common.hpp"
#include "sinex.hpp"

struct RinexOutput
{
	string sysDesc;
	long headerObsPos	= 0;
	long headerTimePos	= 0;
	
	map<E_Sys, list<pair<E_ObsCode, E_ObsDesc>>> codesPerSys;
};

map<string, RinexOutput> filenameObsFileDataMap;


string rinexSysDesc(
	E_Sys sys)
{
	switch (sys)
	{
		case E_Sys::COMB:	//fallthrough
		case E_Sys::NONE:	return "M: Mixed";		
		case E_Sys::GPS:	return "G: GPS";		
		case E_Sys::GLO:	return "R: GLONASS";	
		case E_Sys::GAL:	return "E: Galileo";	
		case E_Sys::QZS:	return "J: QZSS";		
		case E_Sys::BDS:	return "C: BDS";		
			//case E_Sys::LEO:	return 'L';
		case E_Sys::IRN:	return "I: IRNSS";		
		case E_Sys::SBS:	return "S: SBAS payload";
		default:
			BOOST_LOG_TRIVIAL(error) << "Error writing RINEX Navigation header unsupported system type.";

	}
	return "M: Mixed";
}

void updateRinexObsHeader(
	RinexOutput&	rinexOutput,
	std::fstream&	rinexStream,
	GTime&			firstObsTime)
{
	rinexStream.seekp(rinexOutput.headerObsPos);
	const char label[] = "SYS / # / OBS TYPES";

	int numSysLines = 0;
	for (auto& [sys, obsCodeDesc] : rinexOutput.codesPerSys)
	{
		auto dummySat = SatSys(sys, 0);
		char sys_c = dummySat.sysChar();

		if (sys_c == '-')
		{
			BOOST_LOG_TRIVIAL(error) << "Writing RINEX file undefined system.";
			return;
		}

		tracepdeex(0, rinexStream, "%c  %3d", sys_c, obsCodeDesc.size());

		int obsCodeCnt = 0;
		for (auto& [obsCode, obsDesc] : obsCodeDesc)
		{
			obsCodeCnt++;
			auto obsDescStr = obsDesc._to_string();
			auto obsCodeStr = obsCode._to_string();
			char obsStr[4];
			obsStr[0] = obsDescStr[0];
			obsStr[1] = obsCodeStr[1];
			obsStr[2] = obsCodeStr[2];
			obsStr[3] = 0;

			if ( obsCodeCnt % 13 == 1
				&&obsCodeCnt 		!= 1)
			{
				tracepdeex(0, rinexStream, "      ");
			}

			tracepdeex(0, rinexStream, " %3s", obsStr);

			if (obsCodeCnt % 13 == 0)
			{
				// After 13 observations make a new line.
				tracepdeex(0, rinexStream, "  %-20s\n", label);
				numSysLines++;
			}
		}

		if (obsCodeCnt % 13 != 0)
		{
			// less than 13 entries and a new line is required.
			while (obsCodeCnt % 13 != 0)
			{
				obsCodeCnt++;
				tracepdeex(0, rinexStream, " %3s", "");
			}

			tracepdeex(0, rinexStream, "  %-20s\n", label);
			numSysLines++;
		}
	}

	while (numSysLines < 2 * E_Sys::_size())
	{
		//add some lines to be filled in later to allow for the maximum number expected
		tracepdeex(0, rinexStream, "%-60.60s%-20s\n", "", "COMMENT");
		numSysLines++;
	}

	if (rinexOutput.headerTimePos == 0)
	{
		string tsys = "GPS"; // PEA internal time is GPS.
		double	ep[6];
		time2epoch(firstObsTime, ep);

		tracepdeex(0, rinexStream, "%10.3f%50s%-20s\n",
			acsConfig.epoch_interval,
			"",
			"INTERVAL");

		tracepdeex(0, rinexStream, "  %04.0f%6.0f%6.0f%6.0f%6.0f%13.7f     %-12s%-20s\n",
			ep[0],
			ep[1],
			ep[2],
			ep[3],
			ep[4],
			ep[5],
			tsys,
			"TIME OF FIRST OBS");

		rinexOutput.headerTimePos = rinexStream.tellp();
	}
}

void writeRinexObsHeader(
	RinexOutput&		fileData,
	Sinex_stn_snx_t&	snx,
	std::fstream&		rinexStream,
	GTime&				firstObsTime)
{
	fileData.headerTimePos = 0;

	// Write the RINEX header.
	GTime now = utc2gpst(timeget());

	char tempStr[20];
	time2str(now, tempStr, 0);

	string timeDate(tempStr);
	boost::replace_all(timeDate, "/", "");
	boost::replace_all(timeDate, ":", "");
	timeDate += " UTC";

	double rnxver = 3.05;

	string prog = "PEA v1";
	string runby = "Geoscience Australia";

	tracepdeex(0, rinexStream, "%9.2f%-11s%-20s%-20s%-20s\n",
		rnxver,
		"",
		"OBSERVATION DATA",
		fileData.sysDesc,
		"RINEX VERSION / TYPE");

	tracepdeex(0, rinexStream, "%-20.20s%-20.20s%-20.20s%-20s\n",
		prog,
		runby,
		timeDate.c_str(),
		"PGM / RUN BY / DATE");

	tracepdeex(0, rinexStream, "%-60.60s%-20s\n",
		snx.sitecode,
		"MARKER NAME");

	tracepdeex(0, rinexStream, "%-20.20s%-40.40s%-20s\n",
		snx.monuid,
		"",
		"MARKER NUMBER");

	//TODO Add marker type as RINEX version is greater than 2.99
	//tracepdeex(0,rinexStream,"%-20.20s%-40.40s%-20s\n",rinexOutput.snx.,"","MARKER TYPE");

	tracepdeex(0, rinexStream, "%-20.20s%-40.40s%-20s\n",
		"",
		acsConfig.analysis_center,
		"OBSERVER / AGENCY");

	tracepdeex(0, rinexStream, "%-20.20s%-20.20s%-20.20s%-20s\n",
		snx.recsn,
		snx.rectype,
		snx.recfirm,
		"REC # / TYPE / VERS");

	tracepdeex(0, rinexStream, "%-20.20s%-20.20s%-20.20s%-20s\n",
		snx.antsn,
		snx.anttype,
		"",
		"ANT # / TYPE");

	tracepdeex(0, rinexStream, "%14.4f%14.4f%14.4f%-18s%-20s\n",
		snx.pos.x(),
		snx.pos.y(),
		snx.pos.z(),
		"",
		"APPROX POSITION XYZ");

	tracepdeex(0, rinexStream, "%14.4f%14.4f%14.4f%-18s%-20s\n",
		snx.ecc[2],
		snx.ecc[1],
		snx.ecc[0],
		"",
		"ANTENNA: DELTA H/E/N");

	fileData.headerObsPos = rinexStream.tellp();

	updateRinexObsHeader(fileData, rinexStream, firstObsTime);

	tracepdeex(0, rinexStream, "%-60.60s%-20s\n",
		"",
		"END OF HEADER");
}

void writeRinexObsBody(
	RinexOutput&		fileData,
	std::fstream&		rinexStream,
	ObsList&			obsList,
	GTime&				time,
	map<E_Sys, bool>&	sysMap)
{
	string tsys = "GPS"; // PEA internal time is GPS.
	double	ep[6];
	time2epoch(time, ep);

	rinexStream.seekp(fileData.headerTimePos);
	tracepdeex(0, rinexStream, "  %04.0f%6.0f%6.0f%6.0f%6.0f%13.7f     %-12s%-20s\n",
		ep[0],
		ep[1],
		ep[2],
		ep[3],
		ep[4],
		ep[5],
		tsys,
		"TIME OF LAST OBS");

	// Write the RINEX body.
	rinexStream.seekp(0, std::ios::end);

	// flag epoch flag (0:ok,1:power failure,>1:event flag)
	int		flag = 0;
	tracepdeex(0, rinexStream, "> %04.0f %02.0f %02.0f %02.0f %02.0f%11.7f  %d%3d%21s\n",
		ep[0],
		ep[1],
		ep[2],
		ep[3],
		ep[4],
		ep[5],
		flag,
		obsList.size(),
		"");

	for (auto& obs : obsList)
	{
		if (sysMap[obs.Sat.sys] == false)
		{
			continue;
		}
		
		tracepdeex(0, rinexStream, "%s", obs.Sat.id().c_str());

		auto& obsCodeDesc = fileData.codesPerSys[obs.Sat.sys];

		for (auto& [obsCode, obsDesc] : obsCodeDesc)
		{
			if (obsCode == +E_ObsCode::NONE)
				continue;

			bool foundObsPair = false;

			for (auto& [ftype, sigList] : obs.SigsLists)
			for (auto& sig : sigList)
			{
				if (sig.code != obsCode)
					continue;

				// if it locates the E_ObsCode then it will always locate E_ObsDesc.
				if (foundObsPair)
				{
					BOOST_LOG_TRIVIAL(error) << "Writing RINEX file duplicated observation.";
					break;
				}
				else
				{
					foundObsPair = true;
				}

				double sn_raw = (sig.snr - 0.5) / 4;
				int sn_rnx = std::min(std::max((int)std::round(sn_raw / 6.0), 1), 9);

				switch (obsDesc)
				{
					case E_ObsDesc::C:
						//tracepdeex(0,rinexStream,"%14.3f %d",sig.P,sSI);
						if (sig.P == 0)		tracepdeex(0, rinexStream, "%14.3s  ", "");
						else				tracepdeex(0, rinexStream, "%14.3f  ", sig.P);
						break;

					case E_ObsDesc::L:
						if (sig.L == 0)		tracepdeex(0, rinexStream, "%14.3s  ", "");
						else				tracepdeex(0, rinexStream, "%14.3f%d%d", sig.L, (uint)sig.LLI, sn_rnx);
						break;

					case E_ObsDesc::D:
						if (sig.D == 0)		tracepdeex(0, rinexStream, "%14.3s  ", "");
						else				tracepdeex(0, rinexStream, "%14.3f  ", sig.D);
						break;

					case E_ObsDesc::S:
						if (sn_raw == 0)	tracepdeex(0, rinexStream, "%14.3s  ", "");
						else				tracepdeex(0, rinexStream, "%14.3f  ", sn_raw);
						break;

					default:
						BOOST_LOG_TRIVIAL(error) << "Writing RINEX unknown/unused observation code.";
						break;
				}
			}

			if (foundObsPair == false)
			{
				// Observation code and description not in observation.
				tracepdeex(0, rinexStream, "%14.3s  ", "");
			}
		}
		rinexStream << "\n";
	}
}

bool updateRinexObsOutput(
	RinexOutput&		rinexOutput,	///< Information for writing file.
	ObsList&			obsList,		///< List of observation data
	map<E_Sys, bool>	sysMap)			///< Options to enable outputting specific systems
{
	bool foundNew = false;
	for (auto& obs : obsList)
	{
		E_Sys sys = obs.Sat.sys;
		
		if (sysMap[sys] == false)
		{
			continue;
		}

		auto& codes = rinexOutput.codesPerSys[sys];

		for (auto& [ftype, sigsList] : obs.SigsLists)
		for (auto& sig : sigsList)
		{
			if (sig.code == +E_ObsCode::NONE)
				continue;
			
			pair<E_ObsCode, E_ObsDesc> codePair;
			auto& [code, type] = codePair;
			
			code = sig.code;
			
			double sn_raw = (sig.snr - 0.5) / 4;	//todo aaron, check this
			code = sig.code;
			type = E_ObsDesc::C;	if (std::find(codes.begin(), codes.end(), codePair) == codes.end()	&&sig.P 	!= 0	&&acsConfig.rinex_obs_print_C_code)		{	foundNew = true;	codes.push_back(codePair);	}	
			type = E_ObsDesc::L;	if (std::find(codes.begin(), codes.end(), codePair) == codes.end()	&&sig.L		!= 0	&&acsConfig.rinex_obs_print_L_code)		{	foundNew = true;	codes.push_back(codePair);	}		
			type = E_ObsDesc::D;	if (std::find(codes.begin(), codes.end(), codePair) == codes.end()	&&sig.D		!= 0	&&acsConfig.rinex_obs_print_D_code)		{	foundNew = true;	codes.push_back(codePair);	}		
			type = E_ObsDesc::S;	if (std::find(codes.begin(), codes.end(), codePair) == codes.end()	&&sn_raw	!= 0	&&acsConfig.rinex_obs_print_S_code)		{	foundNew = true;	codes.push_back(codePair);	}	
		}

		if (codes.size() == 0)
			rinexOutput.codesPerSys.erase(rinexOutput.codesPerSys.find(sys));
	}
	return foundNew;
}


void writeRinexObsFile(
	RinexOutput&		fileData,
	Sinex_stn_snx_t&	snx,
	string				fileName,
	ObsList&			obsList,
	GTime&				time,
	map<E_Sys, bool>	sysMap)
{
	if (obsList.empty())
		return;

	std::fstream rinexStream(fileName);
	rinexStream.seekp(0, std::ios::end);
	long endFilePos = rinexStream.tellp();

	if (endFilePos == 0)
	{
		fileData = {};
		
		if (sysMap.size() == 1)		fileData.sysDesc = rinexSysDesc(sysMap.begin()->first);
		else						fileData.sysDesc = rinexSysDesc(E_Sys::COMB);
		
		updateRinexObsOutput(fileData, obsList, sysMap);
		writeRinexObsHeader(fileData, snx, rinexStream, time);
	}
	else
	{
		bool newVals = updateRinexObsOutput(fileData, obsList, sysMap);
		if (newVals)
			updateRinexObsHeader(fileData, rinexStream, time);
	}
	writeRinexObsBody(fileData, rinexStream, obsList, time, sysMap);
}

map<string, string> rinexObsFilenameMap;

void writeRinexObs(
	string&				id,
	Sinex_stn_snx_t&	snx,
	GTime&				time,
	ObsList&			obsList)
{
	string filename = acsConfig.rinex_obs_filename;
	
	auto filenameSysMap = getSysOutputFilenames(filename, time, id);
	
	for (auto [filename, sysMap] : filenameSysMap)
	{
		auto& fileData = filenameObsFileDataMap[filename];
		
		writeRinexObsFile(fileData, snx, filename, obsList, time, sysMap);
	}
}
