
#pragma once

#include <unordered_map>
#include <string>
#include <map>

using std::unordered_map;
using std::string;

struct InstrumentEntry
{
	long int cpu	= 0;
	long int time	= 0;
	long int call	= 0;
};

struct Instrument
{
	static unordered_map<string, InstrumentEntry>	entries;

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

