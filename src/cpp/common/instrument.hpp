
#pragma once

#include <string>
#include <map>

using std::string;
using std::map;


struct Instrument
{
	static map<string, long int>	cpuMap;
	static map<string, long int>	timeMap;
	static map<string, long int>	callMap;
	
	bool print = false;

	long int	start;
	long int	start_cpu;
	string		description;
	
	Instrument(
		const string&	desc, 
		bool			print = false);

	~Instrument();

	static void printStatus(
		bool			clear = false);
};

