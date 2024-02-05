
// #pragma GCC optimize ("O0")


#include <iostream>
#include <fstream>
#include <chrono>

using std::ifstream;

#include "instrument.hpp"
#include "acsConfig.hpp"
#include "gTime.hpp"

map<string, long int>		Instrument::timeMap;
map<string, long int>		Instrument::callMap;
map<string, long int>		Instrument::cpuMap;

long int userTime()
{
	auto pid = getpid();
	
	char buff[64];
	snprintf(buff, 64, "/proc/%d/stat", pid);
	ifstream pidStat(buff);
	
	if (!pidStat)
	{
		return 0;
	}
	
	string line;
	int			pid2;
	char		comm[64];
	char		state;
	int			ppid;
	int			pgrp;
	int			session;
	int			tty_nr;
	int			tpgid;
	int			flags;
	long int	minflt;
	long int	cminflt;
	long int	majflt;
	long int	cmajflt;
	long int	utime;
	long int	stime;			
	long int	cutime;
	long int	cstime;
	long int	priority;
	long int	nice;
	long int	num_threads;
	getline(pidStat, line);
// 					1 2 3  4  5  6  7  8  9  0    1  2    3   4    5   6 17
	sscanf(line.data(), "%d %s %c %d %d %d %d %d %u %lu %lu %lu %lu %lu %lu %ld %ld %ld %ld %ld",
		&	pid2,
			comm,
		&	state,
		&	ppid,
		&	pgrp,
		&	session,
		&	tty_nr,
		&	tpgid,
		&	flags,
		&	minflt,
		&	cminflt,
		&	majflt,
		&	cmajflt,
		&	utime,
		&	stime,
		&	cutime,
		&	cstime,
		&	priority,
		&	nice,
		&	num_threads);
	
	return utime;
}

Instrument::Instrument(
	const string&	desc, 
	bool			print)
{
	if (acsConfig.instrument == false)
	{
		return;
	}
	
	this->print	= print;
	start		= std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now().time_since_epoch()).count();
	start_cpu	= userTime();
	description	= desc;
}

Instrument::~Instrument()
{
	if (acsConfig.instrument == false)
	{
		return;
	}
	
	size_t		stop		= std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now().time_since_epoch()).count();
	long int	stop_cpu	= userTime();
	cpuMap	[description] += stop_cpu - start_cpu;
	timeMap	[description] += stop - start;
	callMap	[description] += 1;
	
	if (print)
		printf("\n%40s took %15ld us", description.c_str(), stop - start);
}

extern GTime tsync;

/** Print the status of completed (passed/failed) and remaining tests
	*/
void Instrument::printStatus(
	bool clear) 
{
	if (acsConfig.instrument == false)
	{
		return;
	}
	
// 	Block block(std::cout, "INSTRUMENTATION");
	
	map<size_t, string>	sortedTimes;
	
	for (auto& [desc, time] : timeMap)
	{
		auto& calls	= callMap	[desc];
		auto& user	= cpuMap	[desc];
		
		char buff[1000];
		snprintf(buff, sizeof(buff), "%40s took %15ld us over %5ld calls, averaging %-20ld user: %8ld cpu: %4.0f%%\n", desc.c_str(), time, calls, time/calls, user, user / (time * 1e-6) );
		sortedTimes[time] = buff;
	}
	
	for (auto& [time, thing] : sortedTimes)
	{
		if (trace_level >= 4)
		{
			std::cout << tsync << " ";
		}

		std::cout << thing;
	}
	
	if (clear)
	{
		cpuMap	.clear();
		timeMap	.clear();
		callMap	.clear();
	}
}
