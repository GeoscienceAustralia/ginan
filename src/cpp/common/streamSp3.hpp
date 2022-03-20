
#ifndef __SP3_STREAM__HPP
#define __SP3_STREAM__HPP


#include "streamObs.hpp"
#include "ephemeris.hpp"

/** Interface for rinex streams
*/
struct SP3Stream : PseudoObsStream
{
	PseudoObsList	tempPseudoObsList;

	
	bool	isUTC		= false;
	double	bfact[2]	= {};
	
	SP3Stream()
	{

	}

	void parseSP3(
		std::istream& inputStream)
	{
// 		//read some of the input,(up to next epoch header?)
// 		//save outputs to member variables.
// 		//eg. header metadata
// 		//eg. list of (ObsLists) with multiple sats, signals combined for each epoch.
// 		//dont parse all, just some.
		list<Peph>	pephList;
	
		bool more = readsp3(inputStream, pephList, 0, isUTC, bfact);
		
		for (auto& peph : pephList)
		{
			PseudoObs pseudoObs;
			pseudoObs.pos	= peph.Pos;
			pseudoObs.vel	= peph.Vel;
			pseudoObs.time	= peph.time;
			pseudoObs.Sat	= peph.Sat;
			
			tempPseudoObsList.push_back(pseudoObs);
		}
		
		if (tempPseudoObsList.size() > 0)
		{
			obsListList.push_back(std::move(tempPseudoObsList));
		}
	}

	int readSP3Header(
		std::istream& inputStream)
	{
		return EXIT_SUCCESS;
	}
};

#endif
