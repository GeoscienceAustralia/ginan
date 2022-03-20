#ifndef FILELOG_H
#define FILELOG_H

#include "acsConfig.hpp"
#include "gTime.hpp"

#include <boost/log/sinks/basic_sink_backend.hpp>
#include <string>
#include <fstream> 

namespace sinks = boost::log::sinks;
using std::string;

struct FileLog : public sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>
{
	static string path_log;
	// The function consumes the log records that come from the frontend
	void consume(
		boost::log::record_view																	const&	rec,
		sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string);
};

void addFileLog();

#endif // FILELOG_H
