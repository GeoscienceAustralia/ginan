

#pragma once

#include <iostream>
#include <fstream>
#include <iomanip>
#include <string>
#include <vector>

#include <boost/log/sinks/basic_sink_backend.hpp>
#include <boost/log/sinks/sync_frontend.hpp>
#include <boost/log/trivial.hpp>

namespace sinks = boost::log::sinks;


#include <boost/format.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/log/trivial.hpp>


using std::vector;
using std::string;

extern boost::iostreams::stream< boost::iostreams::null_sink> nullStream;


using Trace = std::ostream;

#include "eigenIncluder.hpp"

struct ConsoleLog : public sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>
{
	// The function consumes the log records that come from the frontend
	void consume(
		boost::log::record_view																	const&	rec,
		sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string);
};

extern int trace_level;

template<typename... Arguments>
void tracepdeex(int level, Trace& stream, string const& fmt, Arguments&&... args)
{
	if (level > trace_level)
		return;

	boost::format f(fmt);
	int unroll[] {0, (f % std::forward<Arguments>(args), 0)...};
	static_cast<void>(unroll);

	stream << boost::str(f);
}


template<typename T>
std::ofstream getTraceFile(
	T&		thing,
	bool	json = false)
{
	string traceFilename;
	if (json)		traceFilename = thing.jsonTraceFilename;
	else 			traceFilename = thing.traceFilename;

	if (traceFilename.empty())
	{
		return std::ofstream();
	}

	std::ofstream trace(traceFilename, std::ios::app);
	if (!trace)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error: Could not open trace file for " << thing.id << " at " << traceFilename;
	}

	return trace;
}

void printHex(
	Trace&					trace,
	vector<unsigned char>&	chunk);

void tracelevel(int level);
void traceFormatedFloat(Trace& trace, double val, string formatStr);

struct Block
{
	Trace& trace;
	string blockName;

	Block(
		Trace& trace,
		string blockName)
	:	trace		{trace},
		blockName	{blockName}
	{
		trace << std::endl << "+" << blockName << std::endl;
	}

	~Block()
	{
		trace << "-" << blockName << std::endl;
	}
};

struct ArbitraryKVP
{
	string		name;
	string		str;
	double		num		= 0;
	long int	integer	= 0;
	int			type	= 0;

	ArbitraryKVP(string name, string	str)		: name {name}, str		{str		}	{	type = 0;	}
	ArbitraryKVP(string name, double	num)		: name {name}, num		{num		}	{	type = 1;	}
	ArbitraryKVP(string name, int		integer)	: name {name}, integer	{integer	}	{	type = 2;	}
	ArbitraryKVP(string name, long int	integer)	: name {name}, integer	{integer	}	{	type = 2;	}

	string value()
	{
		if (isnan(num))
		{
			num = -1;
		}

		if		(type == 0)		return "\"" + str + "\"";
		else if	(type == 1)		return std::to_string(num);
		else if	(type == 2)		return std::to_string(integer);
		else					return "";
	}
};

void traceJson(
	int						level,
	Trace&					trace,
	string					time,
	vector<ArbitraryKVP>	id,
	vector<ArbitraryKVP>	val);


bool createNewTraceFile(
	const string				id,
	boost::posix_time::ptime	logptime,
	string  					new_path_trace,
	string& 					old_path_trace,
	bool						outputHeader = false,
	bool						outputConfig = false);
