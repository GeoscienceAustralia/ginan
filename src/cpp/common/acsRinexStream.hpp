
#ifndef __RINEX_STREAM__HPP
#define __RINEX_STREAM__HPP

#include "rinex.hpp"

/** Interface for rinex streams
*/
struct RinexStream : ObsStream, NavStream
{
	char							ctype;
	double							version;
	E_Sys							nav_system;
	int 							time_system;
	map<E_Sys, vector<CodeType>>	sysCodeTypes;
	ObsList							tempObsList;

	RinexStream()
	{

	}

	void parseRINEX(std::istream& inputStream)
	{
		//read some of the input,(up to next epoch header?)
		//save outputs to member variables.
		//eg. header metadata
		//eg. list of (ObsLists) with multiple sats, signals combined for each epoch.
		//dont parse all, just some.

		//this structure to match real-time architecture.
		int stat = 0;
		// account for rinex comment in the middle of the file
		while   ( stat<=0
				&&inputStream)
		{
			stat = readrnx(inputStream, ctype, tempObsList, nav, nullptr,	version, nav_system, time_system, sysCodeTypes);
		}

		if (tempObsList.size() > 0)
		{
			obsListList.push_back(std::move(tempObsList));
		}
	}

	int readRinexHeader(std::istream& inputStream)
	{
		bool pass = (bool) inputStream;
		
		if (pass == false)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "ERROR: Failed to get good inputStream for RINEX file ";

			return EXIT_FAILURE;
		}
		
		pass = readrnx(inputStream, ctype, tempObsList, nav, &rnxStation, version, nav_system, time_system, sysCodeTypes);
		if (pass == false)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "ERROR: Failed to read header from RINEX file ";

			return EXIT_FAILURE;
		}
		return EXIT_SUCCESS;
	}
};

#endif
