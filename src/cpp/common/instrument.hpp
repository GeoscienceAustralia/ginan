
#pragma once

#include <string>
#include <map>

using std::string;
using std::map;


struct Instrument
{
	static map<string, size_t>		timeMap;
	static map<string, size_t>		callMap;
	
	bool print = false;

	size_t	start;
	string	description;
	
	Instrument(
		string	desc, 
		bool	print = false);

	~Instrument();

	static void printStatus();
};

