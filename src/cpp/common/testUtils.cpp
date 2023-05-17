
// #pragma GCC optimize ("O0")

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/map.hpp>

#include <algorithm>
#include <math.h>    

#include "instrument.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "common.hpp"

list<string>					TestStack::TestStackList;
vector<string>					TestStack::RecordStackList;
vector<TestThingy>				TestStack::TestDoubleData;
list<TestThingy>				TestStack::RecordedTests;
map<string, string>				TestStack::TestStringData;
map<string, string>				TestStack::TestRedirect;
std::ofstream					TestStack::TestOutputStream;
std::ofstream					TestStack::TestNameStream;
bool							TestStack::DontTest			= false;
bool							TestStack::NewData			= false;



/** Add a level to the stack for runtime tests
	*/
TestStack::TestStack(string desc)
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}

	TestStackList.push_back(desc + ">");
#endif
}


/** Pop a level from the stack for runtime tests
	*/
TestStack::~TestStack()
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}

	TestStackList.pop_back();
#endif
}


/** Print the status of completed (passed/failed) and remaining tests
	*/
void TestStack::printStatus(
	bool final)  		///< Option to print missing test cases as failed
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}

	int pass = 0;
	int fail = 0;

	for (auto& test : TestDoubleData)
	{
		if (test.status == +1) 	pass++;
		if (test.status == -1)	fail++;
	}

	TestOutputStream
	<< std::endl	<< std::endl
	<< "Tested "	<< (pass + fail)
	<< " of "		<< TestDoubleData.size()
	<< " tests."	<< std::endl
	<< "Passed: "	<< pass << std::endl
	<< "Failed: "	<< fail << std::endl;

	if (final)
	{
		for (auto& test : TestDoubleData)
		{
			if (test.status == 0)
			{
				TestOutputStream
				<< std::endl
				<< "Test missed: \""	<< test.stack << "\"";
			}
		}
	}
#endif
}

/** Get current status of test set
	*/
int TestStack::testStatus()
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return 0;
	}

// 	Instrument instrument(__FUNCTION__);
	
	int totalStatus = 1;
	bool complete = true;
	for (auto& test : TestDoubleData)
	{
		if (test.status == 0)
		{
			//incomplete
			totalStatus = 0;
			complete = false;
		}

		if (test.status == -1)	//fail
		{
			totalStatus = -1;
		}
	}

	if	( (acsConfig.testOpts.stop_on_done)
		&&(complete))
	{
		printStatus();
		std::cout << std::endl;
		std::cout << std::endl << "All tests completed. Exiting...";
		std::cout << std::endl << std::endl;

		if (totalStatus > 0)	exit(0);
		if (totalStatus < 0)	exit(-1);

	}
	return totalStatus;
#endif
	return true;
}

/** Open and read test data files
	*/
void TestStack::openData()
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}
	
// 	Instrument instrument(__FUNCTION__);

	TestOutputStream.	open(acsConfig.test_filename + ".out");
	TestNameStream.		open(acsConfig.test_filename + ".names");

	TestOutputStream << "Results for tests run on " << acsConfig.config_description << std::endl;
	
	std::cout << "Reading in test data from " << acsConfig.test_filename << "..." << std::endl;
	std::ifstream inputFilestream(acsConfig.test_filename + ".bin");
	try
	{
		boost::archive::binary_iarchive archive(inputFilestream);
		archive >> TestDoubleData;
		archive >> TestStringData;
	}
	catch (...) {}

	try
	{
		auto yaml = YAML::LoadFile(acsConfig.test_filename + ".redir");

		for (YAML::const_iterator it = yaml.begin(); it != yaml.end(); ++it)
		{
			string New = it->first.	as<string>();
			string Old = it->second.as<string>();
			TestRedirect[New] = Old;
		}
	}
	catch (...)
	{
//		std::cout << std::endl << "Error loading TestStack redir file" << std::endl;
	}

	string recordFilename = acsConfig.test_filename + ".record";
	
	int numTests = 0;
	try
	{
		auto yaml = YAML::LoadFile(recordFilename);

		for (YAML::const_iterator it = yaml.begin(); it != yaml.end(); ++it)
		{
			string New = it->first.as<string>();
			RecordStackList.push_back(std::move(New));
			
			numTests++;
		}
	}
	catch (...)
	{
		std::cout << std::endl << "Error loading TestStack record file" << recordFilename << std::endl;
	}
	

	TestOutputStream << numTests << " tests loaded from file" <<  std::endl << std::endl;

	std::sort(RecordStackList.begin(), RecordStackList.end());
#endif
}

/** Save test data.
	* Should be called regularly, but repeatedly calling this function will result in very poor (exponential) performance declines.
	*/
void TestStack::saveData()
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest)
		||(NewData == false))
	{
		return;
	}
	
// 	Instrument instrument(__FUNCTION__);
	
	TestDoubleData.reserve(TestDoubleData.size() + RecordedTests.size());
	
	for (auto& test : RecordedTests)
	{
		TestDoubleData.push_back(std::move(test));
	}
	
	std::sort(TestDoubleData.begin(), TestDoubleData.end());
	
	// create and open a character archive for output
	std::ofstream outputFilestream(acsConfig.test_filename + ".bin");

	// save data to archive
	{
		boost::archive::binary_oarchive archive(outputFilestream);
		archive << TestDoubleData;
		archive << TestStringData;
	}

	NewData = false;
#endif
}

/** Get the test stack as a string.
	* It also compares and replaces with a redirect string if available
	*/
string TestStack::getStack(
	string	id,       		///< Id to append to stack value
	string&	original)		///< Original stack value when redirected
{
// 	Instrument instrument(__FUNCTION__);
	
	string stack;
	for (auto& a : TestStackList)
	{
		stack += a;
	}
	stack += id;

	original = stack;
	TestNameStream << "\"" << stack << "\":"  << std::endl;

	auto redirectEntry = TestRedirect.find(stack);
	if (redirectEntry != TestRedirect.end())
	{
		stack = redirectEntry->second;
	}

	return stack;
}

/** Check matrix entries for invalid values.
	*/
bool TestStack::checkMat(
	string	id,		///< ID to append to stack value
	double*	mat,	///< Matrix data to check
	int		n)		///< Number of elements in matrix
{
#ifdef	ENABLE_UNIT_TESTS
	if	( acsConfig.testOpts.enable == false
// 		||(DontTest)
		)
	{
		return true;
	}
	
// 	Instrument instrument(__FUNCTION__);

	string	original;
	string	stack = getStack(id, original);

	bool fail = false;
	for (int i = 0; i < n; i++)
	{
		if (std::isnan(mat[i]))
		{
			TestOutputStream << std::endl << stack << " has NAN at " << i << std::endl;

			fail = true;
			break;
		}

		if (std::isinf(mat[i]))
		{
			TestOutputStream << std::endl << stack << " has INF at " << i << std::endl;

			fail = true;
			break;
		}
	}

	if (fail)
	{
		if (acsConfig.testOpts.stop_on_fail)
		{
			TestOutputStream << std::endl;

			std::cout << std::endl << "Test failed. Exiting..." << std::endl;
			exit(-1);
		}

		return false;
	}

#endif
	return true;
}

/** Compare matrix against test dataset
	*/
void TestStack::testMat(
	string	id,                   		///< ID value to append to stack
	double*	mat,                        ///< Matrix data to compare
	int		n,        				    ///< Number of elements in matrix
	double	precision,       			///< The threshold for failing a comparison
	double*	covariance)     			///< Optional covariance matrix for element-wise thresholds
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}
	
// 	Instrument instrument(__FUNCTION__);

	string	original;
	string	stack = getStack(id, original);

	TestThingy dummy = {stack};
	
	auto it = std::lower_bound(TestDoubleData.begin(), TestDoubleData.end(), dummy);
	if	(	it == TestDoubleData.end()
		||	it->stack != stack)
	{
		if (original != stack)
		{
			return;
		}
		
		auto it = std::lower_bound(RecordStackList.begin(), RecordStackList.end(), stack);
		if	(   it == RecordStackList.end()
			|| *it != stack)
		{
			return;
		}

		auto redirectEntry = TestRedirect.find(stack);
		if (redirectEntry != TestRedirect.end())
		{
			return;
		}

		TestThingy test = {stack};
		
		test.data.reserve(n);
		
		for (int i = 0; i < n; i++)
		{
			test.data.push_back(mat[i]);
		}
		
		RecordedTests.push_back(std::move(test));

		TestOutputStream << "TEST: Added  '" << stack << std::endl;
		NewData = true;

		return;
	}

	int		errorCount = 0;
	int		firstI	= -1;
	double	firstA	= -1;
	double	firstB	= -1;
	int		maxI	= 0;
	double	maxA	= 0;
	double	maxB	= 0;
	double	maxFrac = 0;
	bool	nan = false;
	bool	inf = false;

	auto& test = *it;

	bool sizeOk = true;

	if (n != test.data.size())
	{
		sizeOk = false;
		TestOutputStream << "Bad test length: got " << n << " but expected " << test.data.size() << " for " << stack << std::endl;

		test.status = -1;

		return;
	}

	map<int, std::tuple<double, double>> errors;

	for (int i = 0; i < n; i++)
	{
		double Old = test.data[i];
		double New = mat[i];

		double delta = fabs(New - Old);
		double sum = fabs(New) + fabs(Old);
		double frac = delta / sum;

		if (std::isnan(New))
		{
			nan = true;
			errorCount++;
			break;
		}
		if (std::isinf(New))
		{
			nan = true;
			errorCount++;
			break;
		}
		bool error = false;
		if (covariance)
		{
			if (SQR(delta * 4) > covariance[i + i * n])
			{
				error = true;
			}
		}
		else
		{
			if (1)
			{
				if (delta > precision)
				{
					error = true;
				}
			}
			else
			{
				if (frac > precision)
				{
					error = true;
				}
			}
		}

		if (error)
		{
			if (frac > maxFrac)
			{
				maxI = i;
				maxA = New;
				maxB = Old;
				maxFrac = frac;
			}

			errors[i] = {Old, New};

			errorCount++;
			if (errorCount == 1)
			{
				firstI = i;
				firstA = New;
				firstB = Old;
			}
		}
	}

	if	( (errorCount > 0)
		&&(acsConfig.testOpts.absorb_errors)
		&&(sizeOk))
	{
		for (int i = 0; i < n; i++)
		{
			mat[i] = test.data[i];
		}
		TestOutputStream << "ABSRB";
	}

	if (errorCount)		test.status = -1;
	else				test.status = +1;

	if 	( (errorCount						== 0)
		&&(acsConfig.testOpts.output_pass	== false))
	{
		return;
	}

	if (errorCount)		TestOutputStream << "---- ";
	else				TestOutputStream << "PASS ";

	if (original.size() > 60)
	{
		original = original.substr(0, 20) + ".." + original.substr(original.size() - 38);
	}
	else
	{
		original.append(60 - original.size(), ' ');
	}

	TestOutputStream << n << "\tof " << original << " \tmaxFrac:\t" << maxFrac*100<<"%\t" << (maxA - maxB);

	bool fail = false;

	if	(nan)
	{
		TestOutputStream
		<< "\tHad NANs~~~~~~~~~~~~~~~~~~~~~~.";

		fail = true;
	}
	else if	(inf)
	{
		TestOutputStream
		<< "\tHad INFs~~~~~~~~~~~~~~~~~~~~~~.";

		fail = true;
	}
	else if	(errorCount > 0)
	{
		TestOutputStream
		<< "\tHad " << errorCount << " errors."
		<< std::endl << "     First at \t" << firstI	<< "\tMax at \t" << maxI
		<< std::scientific
		<< std::endl << "     Expected \t" << firstB	<< "\t       \t" << maxB
		<< std::endl << "     Received \t" << firstA	<< "\t       \t" << maxA;

		if (acsConfig.testOpts.output_errors)
		{
			TestOutputStream << std::endl << "     Index    :";
// 				for (int i = 0; i < n; i++)
			for (auto& [index, vals] : errors)
			{
				TestOutputStream << "\t" << index;
			}

			TestOutputStream << std::endl << "     Expected :";
// 				for (int i = 0; i < n; i++)
			for (auto& [index, vals] : errors)
			{
				TestOutputStream << "\t" << std::scientific << std::get<0>(vals);
			}

			TestOutputStream << std::endl << "     Got      :";
// 				for (int i = 0; i < n; i++)
			for (auto& [index, vals] : errors)
			{
				TestOutputStream << "\t" << std::scientific << std::get<1>(vals);
			}

			TestOutputStream << std::endl << "     Error    :";
// 				for (int i = 0; i < n; i++)
			for (auto& [index, vals] : errors)
			{
				TestOutputStream << "\t" << std::scientific << std::get<1>(vals) - std::get<0>(vals);
			}
		}

		fail = true;
	}

	if	( fail
		&&acsConfig.testOpts.stop_on_fail)
	{
		TestOutputStream << std::endl;

		std::cout << std::endl << "Test failed. Exiting..." << std::endl;

		exit(-1);
	}

	TestOutputStream << std::endl;
#endif
}

/** Compare matrix against test dataset
	*/
void TestStack::testMat(
	string		id,					///< ID value to append to stack
	MatrixXd&	mat,				///< Matrix data to compare
	double		precision) 			///< The threshold for failing a comparison
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}

	testMat(id, mat.data(), mat.rows() * mat.cols(), precision);
#endif
}

/** Compare matrix against test dataset
	*/
void TestStack::checkMat(
	string		id,					///< ID value to append to stack
	MatrixXd&	mat)				///< Matrix data to compare
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}

	checkMat(id, mat.data(), mat.rows() * mat.cols());
#endif
}

/** Compare matrix against test dataset
	*/
void TestStack::checkMat(
	string		id,					///< ID value to append to stack
	VectorXd&	vec)				///< Matrix data to compare
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}

	checkMat(id, vec.data(), vec.rows() * vec.cols());
#endif
}

/** Compare vector against test dataset
	*/
void TestStack::testMat(
	string		id,						///< ID value to append to stack
	VectorXd&	mat,					///< Vector data to compare
	double		precision,				///< The threshold for failing a comparison
	MatrixXd*	covariance)				///< Optional covariance matrix for element-wise thresholds
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}

	void* ptr = nullptr;
	if (covariance)
		ptr = covariance->data();
	testMat(id, mat.data(), mat.rows(), precision, (double*) ptr);
#endif
}

/** Compare vector against test dataset
	*/
void TestStack::testMat(
	string		id,						///< ID value to append to stack
	Vector3d&	mat,					///< Vector data to compare
	double		precision,				///< The threshold for failing a comparison
	MatrixXd*	covariance)				///< Optional covariance matrix for element-wise thresholds
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}

	void* ptr = nullptr;
	if (covariance)
		ptr = covariance->data();
	testMat(id, mat.data(), mat.rows(), precision, (double*) ptr);
#endif
}

/** Compare single double against test data
	*/
void TestStack::testMat(
	string		id,							///< ID value to append to stack
	double&		num,                		///< Double value to compare
	double		precision)					///< The threshold for failing a comparison
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}

	testMat(id, &num, 1, precision);
#endif
}

void TestStack::testInt(
	string	id,			///< ID value to append to stack
	int		num)        ///< Integer value to compare
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.testOpts.enable == false)
		||(DontTest))
	{
		return;
	}

	double a = num;
	testMat(id, &a, 1, 1e-14);
#endif
}

/** Compare string against test data
	*/
void TestStack::testStr(
	string id,			///< ID value to append to stack
	string str)			///< String value to compare
{
// #ifdef	ENABLE_UNIT_TESTS
// 	if	( (acsConfig.testOpts.enable == false)
// 		||(DontTest))
// 	{
// 		return;
// 	}
// 
// 	string	original;
// 	string	stack = getStack(id, original);
// 
// 	auto entry = TestStringData.find(stack);
// 	if (entry == TestStringData.end())
// 	{
// 		if (stack != original)
// 		{
// 			return;
// 		}
// 
// 		volatile bool output = false;
// 		if	( (output)
// 			||(std::find(RecordStackList.begin(), RecordStackList.end(), stack) != RecordStackList.end()))
// 		{
// 			auto redirectEntry = TestRedirect.find(stack);
// 			if (redirectEntry != TestRedirect.end())
// 			{
// 				return;
// 			}
// 
// 			TestStringData[stack] = str;
// 
// 			TestOutputStream << "TEST: Added  '" << stack << std::endl;
// 		}
// 		return;
// 	}
// 
// 	auto& data = entry->second;
// 
// 	if 	( (data == str)
// 		&&(acsConfig.testOpts.output_pass	== false))
// 	{
// 		return;
// 	}
// 
// 	if (data != str)	{	test.status = -1;	TestOutputStream << "----"; }
// 	else				{	test.status = +1;	TestOutputStream << "PASS"; }
// 
// 	if (original.size() > 60)
// 	{
// 		original = original.substr(0, 20) + ".." + original.substr(original.size() - 38);
// 	}
// 	else
// 	{
// 		original.append(60 - original.size(), ' ');
// 	}
// 
// 	TestOutputStream << "\t   " << original;
// 
// 	if	(data != str)
// 	{
// 		TestOutputStream
// 		<< std::endl << "\texpected \"" << data	<< "\""
// 		<< std::endl << "\tbut got  \"" << str	<< "\""
// 		<< std::endl << "\t          ";
// 		for (int i = 0; i < data.size(); i++)
// 		{
// 			if (data[i] != str[i])
// 			{
// 				TestOutputStream << "!";
// 				break;
// 			}
// 			else
// 			{
// 				TestOutputStream << " ";
// 			}
// 		}
// 
// 		if (acsConfig.testOpts.stop_on_fail)
// 		{
// 			exit(-1);
// 		}
// 	}
// 	TestOutputStream << std::endl;
// #endif
}







void ErrorExit::consume(
	boost::log::record_view																	const&	rec,
	sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string)
{
	int logLevel = 0;
	auto attrs = rec.attribute_values();
	auto sev = attrs[boost::log::trivial::severity].get();
	switch (sev)
	{
		case boost::log::trivial::trace:			logLevel = 5;			break;
		case boost::log::trivial::debug:			logLevel = 4;			break;
		case boost::log::trivial::info:				logLevel = 3;			break;
		case boost::log::trivial::warning:			logLevel = 2;			break;
		case boost::log::trivial::error:			logLevel = 1;			break;
		case boost::log::trivial::fatal:			logLevel = 0;			break;
	}

	if (logLevel <= acsConfig.fatal_level)
	{
		std::cout << std::endl << "Message met fatal_message_level condition for exit.\nExiting...\n\n";
		exit(1);
	}
}


void exitOnErrors()
{
	// Construct the sink
	using LogSink = sinks::synchronous_sink<ErrorExit>;

	boost::shared_ptr<LogSink> logSink = boost::make_shared<LogSink>();

	// Register the sink in the logging core
	boost::log::core::get()->add_sink(logSink);
}



// #include <malloc.h>

// size_t bucket = 0;
// 
// static void* plumber_hook(size_t size, const void* caller);
// static void* plumber_hook(size_t size, const void* caller)
// {
// 	void*	result;
// 	
// 	/* Restore all old hooks */
// 	/* Call recursively */
// 	__malloc_hook		= 0;
// 	{
// 		result = malloc(size);
// 	}
// 	__malloc_hook		= plumber_hook;
// 
// 	bucket += size;
// 	
// 	return result;
// }
// 
// 
// template<typename T>
// size_t plumberTest(T& t)
// {
// 	//begin plumbing
// 	bucket = 0;
// 		
// 	__malloc_hook	= plumber_hook;
// 	{
// 		T newT = t;
// 	}
// 	__malloc_hook	= 0;
// 	
// 	return bucket;
// }

void plumber()
{
// 	static map<string, size_t>	plumberMap;
// 	
// 	size_t New;
// 	string v;
// 	
// 	printf("Checking plumbing:\n");
// 	v = "nav";			New = plumberTest(nav			);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "ephMap";		New = plumberTest(nav.ephMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "gephMap";		New = plumberTest(nav.gephMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "sephMap";		New = plumberTest(nav.sephMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "pephMap";		New = plumberTest(nav.pephMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "pclkMap";		New = plumberTest(nav.pclkMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "satNavMap";	New = plumberTest(nav.satNavMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "tecMap";		New = plumberTest(nav.tecMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "pcMap";		New = plumberTest(nav.pcMap		);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	
// 	printf("\n");
}

