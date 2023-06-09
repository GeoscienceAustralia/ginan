
// #pragma GCC optimize ("O0")

#include <stdarg.h>
#include <ctype.h>
#include <unordered_map>

using std::unordered_map;

#include <boost/iostreams/stream.hpp>
#include <boost/format.hpp>

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
	static unordered_map<string, bool> warnedMap;
	
	auto attrs = rec.attribute_values();
	auto sev = attrs[boost::log::trivial::severity].get();
	
	if (sev == boost::log::trivial::warning)
	{
		auto& warned = warnedMap[logString];
		if (warned)
		{
			return;
		}
		
		warned = true;
	}
	
	std::cout << std::endl << logString << std::flush;
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
	Trace&			trace,
	vector<char>	chunk)
{
	trace << "\nHex Data : ";

	for (int i = 0; i < chunk.size(); i++)
	{
		if (i % 10 == 0)
			trace << std::endl;       
		char hex[3];
		snprintf(hex, sizeof(hex),"%02x",(unsigned char) chunk[i]);
		tracepdeex(0, trace, "%s ", hex);
	}
	trace << std::endl; 
}


void mongoTrace(string);

void traceJson(
	int						level,
	Trace&					trace,
	string					time,
	vector<ArbitraryKVP>	id,
	vector<ArbitraryKVP>	val)
{
	if (level > trace_level)
		return;
	
	if	( acsConfig.output_json_trace == false
		&&acsConfig.localMongo.output_trace)
	{
		return;
	}
	
	string json = "{ \"time\":\"" + time + "\", \"id\":{";
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
	if (acsConfig.localMongo.output_trace)
	{
		mongoTrace(json);
	}
}
	
