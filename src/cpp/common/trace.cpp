
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

#include <unordered_map>
#include <functional>
#include <stdarg.h>
#include <ctype.h>

using std::unordered_map;

#include <boost/iostreams/stream.hpp>
#include <boost/format.hpp>

#include "interactiveTerminal.hpp"
#include "peaCommitStrings.hpp"
#include "inputsOutputs.hpp"
#include "observations.hpp"
#include "mongoWrite.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "trace.hpp"

boost::iostreams::stream<boost::iostreams::null_sink> nullStream((boost::iostreams::null_sink()));


bool ConsoleLog::useInteractive = false;



/** Semi-formatted text-based outputs.
 * Trace files are the best record of the processing that occurs within the Pea.
 *
 * The level of trace may be set numerically by configuration.
 * It will enable more or fewer lines of detail, and in some cases, the number of columns to be included in formatted sections.
 *
 * Receivers and Satellites may have trace file outputs configured, which will include details specific to their processing, such as any preprocessing,
 * and details about measurements that are being computed.
 *
 * Calculated satellite and receiver positions, and reasons for the exclusion of any measurements are recorded in the receiver trace files.
 *
 * When satellite orbit propagation is enabled, some details about modelled forces may be available in satellite trace files.
 *
 * The receiver files may be configured with residual chain outputs on, which will produce formatted output of each modelled component as they are subtracted from the measured quantity.
 * Depending on the possibility of chunking, receiver trace files may also include states that are usually only recorded in the network trace files.
 *
 * Once the list of measurements is aggregated and passed to the main filter, trace outputs are written to the 'network' trace file.
 * This contains any filter states, measurement residuals, state removals or reinitialsations, and iteration details.
 *
 * The filter outputs in the network trace file include blocks that are formatted with SINEX-style +BLOCK...-BLOCK sections,
 * which may allow for easier post-processing by eliminating any information outside of the required section.
 */
FileType Trace_Files__()
{

}

boost::log::trivial::severity_level acsSeverity = boost::log::trivial::info;

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


	string output;

	output += "\r\n";
	if (acsConfig.colourise_terminal)
	{
		if (sev == boost::log::trivial::warning)	output += "\x1B[1;93m";
		if (sev == boost::log::trivial::error)		output += "\x1B[101m";
	}
	output += logString;

	if (acsConfig.colourise_terminal)
	{
		output += "\x1B[0m";
	}

	if (useInteractive)
	{
		InteractiveTerminal::addString("Messages/All", logString);

		if		(sev == boost::log::trivial::info)		InteractiveTerminal::addString("Messages/Info",		logString);
		else if	(sev == boost::log::trivial::warning)	InteractiveTerminal::addString("Messages/Warnings",	logString);
		else if	(sev == boost::log::trivial::error)		InteractiveTerminal::addString("Messages/Errors",	logString);
		else if	(sev == boost::log::trivial::debug)		InteractiveTerminal::addString("Messages/Debug",	logString);

		// std::cerr << output << std::flush;
	}
	else
	{
		std::cout << output << std::flush;
	}
}


int traceLevel = 0;       ///< level of trace

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

void printHex(
	Trace&					trace,
	vector<unsigned char>&	chunk)
{
	trace << "\nHex Data : " << chunk.size();

	for (int i = 0; i < chunk.size(); i++)
	{
		if (i % 40 == 0)
			trace << "\n";

		if (i % 10 == 0)
			trace << " ";
		char hex[3];
		snprintf(hex, sizeof(hex),"%02x", chunk[i]);
		tracepdeex(0, trace, "%s ", hex);
	}
	trace << "\n";
}


void traceJson_(
	Trace&					trace,
	GTime&					time,
	vector<ArbitraryKVP>	id,
	vector<ArbitraryKVP>	val)
{
	GEpoch ep(time);

	char timeBuff[64];
	snprintf(timeBuff, sizeof(timeBuff),"%04.0f-%02.0f-%02.0fT%02.0f:%02.0f:%06.3fZ",
			ep.year,
			ep.month,
			ep.day,
			ep.hour,
			ep.min,
			ep.sec);

	string json = (string) "{ \"Epoch\":{ \"$date\":\"" + timeBuff + "\"}, \"id\":{";

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
		mongoTrace({json}, acsConfig.mongoOpts.queue_outputs);
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

	if (new_path_trace == acsConfig.pppOpts.rts_smoothed_suffix)
	{
		return false;
	}

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
		trace << "station    : " << id << "\n";
		trace << "start_epoch: " << acsConfig.start_epoch			<< "\n";
		trace << "end_epoch  : " << acsConfig.end_epoch				<< "\n";
		trace << "trace_level: " << acsConfig.trace_level			<< "\n";
		trace << "pea_version: " << ginanCommitVersion()			<< "\n";
// 		trace << "rts_lag    : " << acsConfig.pppOpts.rts_lag		<< "\n";
	}

	if (outputConfig)
	{
		dumpConfig(trace);
	}

	return true;
}
