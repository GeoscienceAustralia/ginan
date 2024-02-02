
#pragma once

#include <boost/log/sinks/basic_sink_backend.hpp>
#include <boost/log/sinks/sync_frontend.hpp>
#include <boost/log/trivial.hpp>

namespace sinks = boost::log::sinks;

#include "eigenIncluder.hpp"

struct ErrorExit : public sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>
{
	// The function consumes the log records that come from the frontend
	void consume(
		boost::log::record_view																	const&	rec,
		sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string);
};

void exitOnErrors();

void stacktrace();

struct TempDisabler
{
	bool	oldVal = false;
	bool*	bool_ptr;

	TempDisabler(
		bool& disable)
	{
		oldVal		= disable;
		disable		= false;
		bool_ptr	= &disable;
	}

	~TempDisabler()
	{
		*bool_ptr = oldVal;
	}
};
