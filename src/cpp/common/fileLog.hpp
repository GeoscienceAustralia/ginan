
#pragma once

#include "common/acsConfig.hpp"
#include "common/gTime.hpp"

#include <boost/log/sinks/basic_sink_backend.hpp>

namespace sinks = boost::log::sinks;

#include <string>
#include <fstream>
using std::string;


struct FileLog : public sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>
{
	static string path_log;

	bool json = false;

	void consume(
		boost::log::record_view																	const&	rec,
		sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string);
};

void addFileLog(
	bool json);
