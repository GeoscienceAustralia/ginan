
#pragma once

#include "streamObs.hpp"
#include "slr.hpp"

/** Interface for slr streams
*/
struct SlrParser : Parser, ObsLister
{
	SlrParser()
	{

	}

	void parse(
		std::istream& inputStream)
	{
		ObsList obsList;
		//read some of the input
		//save outputs to member variables.
		//dont parse all, just some.

		//this structure to match real-time architecture.
		int stat = 0;
		while   ( stat<=0
				&&inputStream)
		{
			stat = readSlrObs(inputStream, obsList);
		}

		if (obsList.size() > 0)
		{
			obsListList.push_back(std::move(obsList));
		}
	}
	
	string parserType()
	{
		return "SlrParser";
	}
};

