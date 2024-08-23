
#pragma once

#include "navigation.hpp"

#include "streamObs.hpp"

#include "rinex.hpp"


struct RinexParser : Parser, ObsLister
{
	char							ctype;
	double							version;
	E_Sys							nav_system;
	E_TimeSys						time_system;
	map<E_Sys, map<int, CodeType>>	sysCodeTypes;
	ObsList							tempObsList;
	RinexStation					rnxRec = {};

	void parse(
		std::istream& inputStream)
	{
		//read some of the input,(up to next epoch header?)
		//save outputs to member variables.
		//eg. header metadata
		//eg. list of (ObsLists) with multiple sats, signals combined for each epoch.

		int stat = 0;
		// account for rinex comment in the middle of the file
		while   (  stat <= 0
				&& inputStream)
		{
			stat = readRnx(inputStream, ctype, tempObsList, nav, rnxRec, version, nav_system, time_system, sysCodeTypes);
		}

		if (tempObsList.size() > 0)
		{
			obsListList.push_back(std::move(tempObsList));
		}
	}

	string parserType()
	{
		return "RinexParser";
	}
};
