
#pragma once

#include <boost/log/sinks/basic_sink_backend.hpp>
#include <boost/log/sinks/sync_frontend.hpp>
#include <boost/log/trivial.hpp>

namespace sinks = boost::log::sinks;

#include <iostream>
#include <fstream>
#include <math.h>
#include <vector>
#include <tuple>
#include <list>
#include <map>

using std::vector;
using std::string;
using std::tuple;
using std::list;
using std::map;

#include "eigenIncluder.hpp"

struct TestThingy
{
	string			stack;
	vector<double>	data;
	int				status = 0;
	
	bool operator < (const TestThingy& rhs) const
	{
		return stack < rhs.stack;
	}
	
	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & stack;
		ar & data;
	}
};

/** Object to contain sets of tests to perform during runtime
*/
struct TestStack
{
	static list<string>				TestStackList;
	static vector<string>			RecordStackList;
	static vector<TestThingy>		TestDoubleData;
	static list<TestThingy>			RecordedTests;
	static map<string, string>		TestStringData;
	static map<string, string>		TestRedirect;
	static std::ofstream			TestOutputStream;
	static std::ofstream			TestNameStream;
	static bool						DontTest;
	static bool						NewData;

	TestStack(string desc);

	~TestStack();

	static void printStatus(
		bool		final = false);

	static int testStatus();

	static void openData();

	static void saveData();

	static string getStack(
		string		id,
		string&		original);

	static bool checkMat(
		string		id,
		double*		mat,
		int			n);

	static void testMat(
		string		id,
		double*		mat,
		int			n			= 1,
		double		precision	= 1e-4,
		double*		covariance	= nullptr);

	static void testMat(
		string		id,
		MatrixXd&	mat,
		double		precision = 1e-4);

	static void checkMat(
		string		id,
		MatrixXd&	mat);

	static void checkMat(
		string		id,
		VectorXd&	vec);

	static void testMat(
		string		id,
		VectorXd&	mat,
		double		precision	= 1e-4,
		MatrixXd*	covariance	= nullptr);

	static void testMat(
		string		id,
		Vector3d&	mat,
		double		precision	= 1e-4,
		MatrixXd*	covariance	= nullptr);

	static void testMat(
		string		id,
		double&		num,
		double		precision	= 1e-4);

	static void testInt(
		string		id,
		int			num);

	static void testStr(
		string 		id,
		string 		str);
};

struct ErrorExit : public sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>
{
	// The function consumes the log records that come from the frontend
	void consume(
		boost::log::record_view																	const&	rec,
		sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string);
};

void exitOnErrors();


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
