
// #pragma GCC optimize ("O0")

#include <unordered_map>
#include <functional>
#include <stdarg.h>
#include <ctype.h>

using std::unordered_map;

#include <boost/iostreams/stream.hpp>
#include <boost/format.hpp>

#include "peaCommitStrings.hpp"
#include "observations.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "trace.hpp"

boost::iostreams::stream<boost::iostreams::null_sink> nullStream((boost::iostreams::null_sink()));



void ConsoleLog::consume(
	boost::log::record_view																	const&	rec,
	sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	logString)
{
	static unordered_map<size_t, bool> warnedMap;

	auto attrs = rec.attribute_values();
	auto sev = attrs[boost::log::trivial::severity].get();

	if	(  sev == boost::log::trivial::warning
		&& acsConfig.warn_once)
	{
		auto& warned = warnedMap[std::hash<string>{}(logString)];
		if (warned)
		{
			return;
		}

		warned = true;
	}

	std::cout << std::endl;
	if (acsConfig.colourise_terminal)
	{
		if (sev == boost::log::trivial::warning)	std::cout << "\x1B[1;93m";
		if (sev == boost::log::trivial::error)		std::cout << "\x1B[101m";
	}
	std::cout << logString;

	if (acsConfig.colourise_terminal)
	{
		std::cout << "\x1B[0m";
	}

	std::cout << std::flush;
}


int trace_level = 0;       ///< level of trace

void tracelevel(int level)
{
	trace_level = level;
}

void traceFormatedFloat(Trace& trace, double val, string formatStr)
{
	// If someone knows how to make C++ print with just one digit as exponent...
	int		exponent	= 0;
	double	base		= 0;

	if (val != 0)
	{
		exponent	= (int)floor(log10(fabs(val)));
		base		= val * pow(10, -1 * exponent);
	}
	tracepdeex(0, trace, formatStr.c_str(), base, exponent);
}

void tracepdeex(int level, FILE *fppde, const char *format, ...)
{
	va_list ap;

	if (!fppde||level>trace_level)
		return;

	va_start(ap,format);
		vfprintf(fppde,format,ap);
	va_end(ap);

	fflush(fppde);
}


void printHex(
	Trace&					trace,
	vector<unsigned char>&	chunk)
{
	trace << "\nHex Data : " << chunk.size();

	for (int i = 0; i < chunk.size(); i++)
	{
		if (i % 40 == 0)
			trace << std::endl;

		if (i % 10 == 0)
			trace << " ";
		char hex[3];
		snprintf(hex, sizeof(hex),"%02x", chunk[i]);
		tracepdeex(0, trace, "%s ", hex);
	}
	trace << std::endl;
}


void mongoTrace(string, bool);

void traceJson(
	int						level,
	Trace&					trace,
	string					time,
	vector<ArbitraryKVP>	id,
	vector<ArbitraryKVP>	val)
{
	if (level > trace_level)
		return;

	if	( acsConfig.output_json_trace		== false
		&&acsConfig.mongoOpts.output_trace	== false)
	{
		return;
	}

	string json = "{ \"Epoch\":\"" + time + "\", \"id\":{";
	for (auto& thing : id)
	{
		json += "\"" + thing.name + "\":" + thing.value() + ",";
	}
	json = json.substr(0, json.length() - 1);

	json += "}, \"val\":{";

	for (auto& thing : val)
	{
		json += "\"" + thing.name + "\":" + thing.value() + ",";
	}
	json = json.substr(0, json.length() - 1);
	json += "} }";

	if (acsConfig.output_json_trace)
	{
		trace << "\n - " + json;
	}
	if (acsConfig.mongoOpts.output_trace)
	{
		mongoTrace(json, acsConfig.mongoOpts.queue_outputs);
	}
}

bool createNewTraceFile(
	const string				id,
	boost::posix_time::ptime	logptime,
	string  					new_path_trace,
	string& 					old_path_trace,
	bool						outputHeader,
	bool						outputConfig)
{
	replaceString(new_path_trace, "<RECEIVER>", id);
	replaceTimes (new_path_trace, logptime);

	// Create the trace file if its a new filename, otherwise, keep the old one
	if	( new_path_trace == old_path_trace
		||new_path_trace.empty())
	{
		//the filename is the same, keep using the old ones
		return false;
	}

	old_path_trace = new_path_trace;

	BOOST_LOG_TRIVIAL(debug)
	<< "Creating new file for " << id << " at " << old_path_trace;

	std::ofstream trace(old_path_trace);
	if (!trace)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error: Could not create file for " << id << " at " << old_path_trace;

		return false;
	}

	// Trace file head
	if (outputHeader)
	{
		trace << "station    : " << id << std::endl;
		trace << "start_epoch: " << acsConfig.start_epoch			<< std::endl;
		trace << "end_epoch  : " << acsConfig.end_epoch				<< std::endl;
		trace << "trace_level: " << acsConfig.trace_level			<< std::endl;
		trace << "pea_version: " << ginanCommitVersion()			<< std::endl;
// 		trace << "rts_lag    : " << acsConfig.pppOpts.rts_lag		<< std::endl;
	}

	if (outputConfig)
	{
		dumpConfig(trace);
	}

	return true;
}
