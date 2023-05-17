
#pragma once

#include "streamObs.hpp"
#include "ephemeris.hpp"

struct Sp3Parser : Parser, ObsLister
{
	ObsList	tempObsList;

	
	E_TimeSys	tsys	= E_TimeSys::GPST;

	double	bfact[2]	= {};

	void parse(
		std::istream& inputStream)
	{
		vector<Peph>	pephList;
	
		bool more = readsp3(inputStream, pephList, 0, tsys, bfact);
		
		for (auto& peph : pephList)
		{
			PObs pObs;
			pObs.pos	= peph.pos;
			pObs.vel	= peph.vel;
			pObs.time	= peph.time;
			pObs.Sat	= peph.Sat;
			
			tempObsList.push_back((shared_ptr<PObs>)pObs);
		}
		
		if (tempObsList.size() > 0)
		{
			obsListList.push_back(std::move(tempObsList));
		}
	}
	
	string parserType()
	{
		return "Sp3Parser";
	}
};

