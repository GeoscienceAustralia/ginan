

#ifndef _STREAM_TRACE_HPP_
#define _STREAM_TRACE_HPP_


#include <iostream>
#include <fstream>
#include <iomanip>
#include <string>
#include <boost/format.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/log/trivial.hpp>

using std::string;

extern boost::iostreams::stream< boost::iostreams::null_sink > nullStream;


using Trace = std::ostream;

#include "eigenIncluder.hpp"

extern int trace_level;

void tracematpde(int lv, Trace& stream, MatrixXd& mat, int width, int precision);
void tracematpde(int lv, Trace& stream, VectorXd& vec, int width, int precision);
void tracematpde(int lv, Trace& stream, MatrixXd* mat, int width, int precision);
void tracematpde(int lv, Trace& stream, VectorXd* vec, int width, int precision);

template<typename... Arguments>
void tracepde(int level, Trace& stream, std::string const& fmt, Arguments&&... args)
{
	if (level > trace_level)
		return;

	stream << "*" << level << " ";
	boost::format f(fmt);
	int unroll[] {0, (f % std::forward<Arguments>(args), 0)...};
	static_cast<void>(unroll);

	stream << boost::str(f);

	stream.flush();
}

template<typename... Arguments>
void tracepdeex(int level, Trace& stream, std::string const& fmt, Arguments&&... args)
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
	T& thing)
{
	if (thing.traceFilename.empty())
	{
		return std::ofstream();
	}

	std::ofstream trace(thing.traceFilename, std::ios::app);
	if (!trace)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not open trace file for " << thing.id << " at " << thing.traceFilename;
	}

	return trace;
}

//forward declarations
struct Obs;

void tracepde(int level, FILE *fppde, const char *format,...);
void tracepdeex(int level, FILE *fppde, const char *format, ...);
void tracematpde(int level, FILE *fppde, const double *A, int n,
						int m, int p, int q);

/* debug trace functions -----------------------------------------------------*/
void tracelevel(int level);
void traceFormatedFloat(Trace& trace, double val, string formatStr);
void matfprint(const double *A, int n, int m, int p, int q, FILE *fp);

void fatalerr(const char *format, ...);


template<typename T>
std::ofstream getTraceFile(T& thing);

#endif

