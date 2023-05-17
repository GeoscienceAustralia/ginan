
// #pragma GCC optimize ("O0")


#include "instrument.hpp"

#include <iostream>
#include <chrono>

map<string, size_t>		Instrument::timeMap;
map<string, size_t>		Instrument::callMap;


Instrument::Instrument(string desc, bool print)
{
#ifdef	ENABLE_UNIT_TESTS
	this->print	= print;
	start		= std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now().time_since_epoch()).count();
	description	= desc;
#endif
}


/** Pop a level from the stack for runtime tests
	*/
Instrument::~Instrument()
{
#ifdef	ENABLE_UNIT_TESTS
	size_t stop = std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now().time_since_epoch()).count();
	
	timeMap[description] += stop - start;
	callMap[description] += 1;
	if (print)
		printf("\n%40s took %15ld us", description.c_str(), stop - start);
#endif
}


/** Print the status of completed (passed/failed) and remaining tests
	*/
void Instrument::printStatus() 
{
#ifdef	ENABLE_UNIT_TESTS
	std::cout << std::endl << "Instrumentation:\n";
	
	map<size_t, string>	sortedTimes;
	
	for (auto& [desc, time] : timeMap)
	{
		auto calls = callMap[desc];
		
		char buff[1000];
		snprintf(buff, sizeof(buff), "%40s took %15ld us over %5ld calls, averaging %ld\n", desc.c_str(), time, calls, time/calls);
		sortedTimes[time] = buff;
	}
	
	for (auto& [time, thing] : sortedTimes)
	{
		std::cout << thing;
	}
#endif
}
