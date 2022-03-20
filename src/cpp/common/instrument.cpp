
// #pragma GCC optimize ("O0")


#include "instrument.hpp"

#include <iostream>
#include <chrono>

map<string, size_t>		Instrument::timeMap;
map<string, size_t>		Instrument::callMap;


Instrument::Instrument(string desc)
{
#ifdef	ENABLE_UNIT_TESTS
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
#endif
}


/** Print the status of completed (passed/failed) and remaining tests
	*/
void Instrument::printStatus() 
{
#ifdef	ENABLE_UNIT_TESTS
	std::cout << std::endl << "Instrumentation:\n";
	for (auto& [desc, time] : timeMap)
	{
		auto calls = callMap[desc];
		printf("%30s took %15ldus over %5ld calls, averaging %ld\n", desc.c_str(), time, calls, time/calls);
	}
#endif
}
