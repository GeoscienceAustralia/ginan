

#ifndef _STREAM_TRACE_HPP_
#define _STREAM_TRACE_HPP_


#include <iostream>
#include <fstream>
#include <iomanip>
#include <boost/format.hpp>
#include <boost/iostreams/stream.hpp>


extern boost::iostreams::stream< boost::iostreams::null_sink > nullStream;


using Trace = std::ostream;

#include "eigenIncluder.hpp"

extern int level_trace;

void tracematpde(int lv, Trace& stream, MatrixXd& mat, int width, int precision);
void tracematpde(int lv, Trace& stream, VectorXd& vec, int width, int precision);
void tracematpde(int lv, Trace& stream, MatrixXd* mat, int width, int precision);
void tracematpde(int lv, Trace& stream, VectorXd* vec, int width, int precision);

template<typename... Arguments>
void tracepde(int level, Trace& stream, std::string const& fmt, Arguments&&... args)
{
	if (level > level_trace)
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
	if (level > level_trace)
		return;

	boost::format f(fmt);
	int unroll[] {0, (f % std::forward<Arguments>(args), 0)...};
	static_cast<void>(unroll);

	stream << boost::str(f);
}


//forward declarations
struct Obs;

void tracepde(int level, FILE *fppde, const char *format,...);
void tracepdeex(int level, FILE *fppde, const char *format, ...);
void tracematpde(int level, FILE *fppde, const double *A, int n,
						int m, int p, int q);

/* debug trace functions -----------------------------------------------------*/
void tracelevel(int level);

void matfprint(const double *A, int n, int m, int p, int q, FILE *fp);

void fatalerr(const char *format, ...);

#endif

