#ifndef __SUMMARY_HPP__
#define __SUMMARY_HPP__

#include <iostream>
#include <map>

using std::map;

#include "station.hpp"

/** Output statistics from each station.
* Including observation counts, slips, beginning and ending epochs*/
void outputSummaries(
	map<string, Station>& stationMap)	///< Map of stations used throughout the program.
{
	std::cout << std::endl << "--------------- SUMMARIES ------------------- " << std::endl;

	for (auto& [id, rec] : stationMap)
	{
		std::cout << std::endl << "------------------- " << rec.id << " --------------------";
		auto a	= boost::posix_time::from_time_t(rec.firstEpoch.time);
		auto b	= boost::posix_time::from_time_t(rec.lastEpoch.time);
		auto ab	= b-a;

		std::cout << std::endl << "First Epoch : " << a;
		std::cout << std::endl << "Last  Epoch : " << b;
		std::cout << std::endl << "Epoch Count : " << rec.epochCount;
		if (rec.epochCount > 1)
			std::cout << std::endl << "Epoch Step  : " << ab / (rec.epochCount - 1);					//todo aaron, this is erroneously coming from config
		std::cout << std::endl << "Duration    : " << ab;
		std::cout << std::endl << "Observations: " << rec.obsCount;

		bool first = true;
		std::cout << std::endl << "By Code     : ";
		for (auto& [code, count] : rec.codeCount)
		{
			if (first)
				first = false;
			else
				std::cout << "  |  ";

			std::cout << code._to_string() << " : " << count;
		}

		first = true;
		std::cout << std::endl << "By Satellite: ";
		for (auto& [sat, count] : rec.satCount)
		{
			if (first)
				first = false;
			else
				std::cout << "  |  ";

			std::cout << sat << " : " << count;
		}
		std::cout << std::endl << "Obs/Slips   : " << rec.obsCount / (rec.slipCount + 1);
		std::cout << std::endl;
	}
}

#endif
