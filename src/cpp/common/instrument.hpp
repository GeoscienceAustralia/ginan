
#ifndef __INSTRUMENT_HPP__
#define __INSTRUMENT_HPP__


#include <string>
#include <map>

using std::string;
using std::map;


struct Instrument
{
	static map<string, size_t>		timeMap;
	static map<string, size_t>		callMap;

	size_t	start;
	string	description;
	
	Instrument(string desc);

	~Instrument();

	static void printStatus();
};

#endif
