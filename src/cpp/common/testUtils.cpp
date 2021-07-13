


#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/unordered_map.hpp>


#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "common.hpp"

list<string>							TestStack::TestStackList;
list<string>							TestStack::RecordStackList;
unordered_map<string, vector<double>>	TestStack::TestDoubleData;
unordered_map<string, string>			TestStack::TestStringData;
unordered_map<string, int>				TestStack::TestStatus;
unordered_map<string, string>			TestStack::TestRedirect;
std::ofstream							TestStack::TestOutputStream;
std::ofstream							TestStack::TestNameStream;
bool									TestStack::DontTest			= false;
bool									TestStack::NewData			= false;



/** Add a level to the stack for runtime tests
	*/
TestStack::TestStack(string desc)
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.process_tests == false)
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
	if	( (acsConfig.process_tests == false)
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
	if	( (acsConfig.process_tests == false)
		||(DontTest))
	{
		return;
	}

	int pass = 0;
	int fail = 0;

	for (auto& [test, status] : TestStatus)
	{
		if (status == +1) 	pass++;
		if (status == -1)	fail++;
	}

	TestOutputStream
	<< std::endl	<< std::endl
	<< "Tested "	<< (pass + fail)
	<< " of "		<< TestStatus.size()
	<< " tests."	<< std::endl
	<< "Passed: "	<< pass << std::endl
	<< "Failed: "	<< fail << std::endl;

	if (final)
	{
		for (auto& [test, status] : TestStatus)
		{
			if (status == 0)
				TestOutputStream
				<< std::endl
				<< "Test missed: \""	<< test << "\"";
		}
	}
#endif
}

/** Get current status of test set
	*/
int TestStack::testStatus()
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.process_tests == false)
		||(DontTest))
	{
		return 0;
	}

	int totalStatus = 1;
	bool complete = true;
	for (auto& [test, status] : TestStatus)
	{
		if (status == 0)
		{
			//incomplete
			totalStatus = 0;
			complete = false;
		}

		if (status == -1)	//fail
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
	if	( (acsConfig.process_tests == false)
		||(DontTest))
	{
		return;
	}

	std::ifstream inputFilestream(acsConfig.testOpts.filename + ".bin");
	try
	{
		boost::archive::binary_iarchive archive(inputFilestream);
		archive >> TestDoubleData;
		archive >> TestStringData;
	}
	catch (...) {}

	for (auto& [test, data] : TestDoubleData)
	{
		TestStatus[test] = 0;
	}
	for (auto& [test, data] : TestStringData)
	{
		TestStatus[test] = 0;
	}

	TestOutputStream.	open(acsConfig.testOpts.filename + ".out");
	TestNameStream.		open(acsConfig.testOpts.filename + ".names");

	try
	{
		auto yaml = YAML::LoadFile(acsConfig.testOpts.filename + ".redir");

		for (YAML::const_iterator it = yaml.begin(); it != yaml.end(); ++it)
		{
			string New = it->first.	as<string>();
			string Old = it->second.as<string>();
			TestRedirect[New] = Old;
		}
	}
	catch (...)
	{
// 			std::cout << std::endl << "Error loading TestStack redir file" << std::endl;
	}

	try
	{
		auto yaml = YAML::LoadFile(acsConfig.testOpts.filename + ".record");

		for (YAML::const_iterator it = yaml.begin(); it != yaml.end(); ++it)
		{
			string New = it->first.as<string>();
			RecordStackList.push_back(New);
		}
	}
	catch (...)
	{
		std::cout << std::endl << "Error loading TestStack record file" << std::endl;
	}

	TestOutputStream << "Results for tests run on " << acsConfig.config_description << std::endl << std::endl;
#endif
}

/** Save test data.
	* Should be called regularly, but repeatedly calling this function will result in very poor (exponential) performance declines.
	*/
void TestStack::saveData()
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.process_tests == false)
		||(DontTest)
		||(NewData == false))
	{
		return;
	}
	// create and open a character archive for output
	std::ofstream outputFilestream(acsConfig.testOpts.filename + ".bin");

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
	if	( (acsConfig.process_tests == false)
// 		||(DontTest)
		)
	{
		return true;
	}

	string	original;
	string	stack = getStack(id, original);

	bool fail = false;
	for (int i = 0; i < n; i++)
	{
		if (std::isnan(mat[i]))		//todo aaron, eigen has internal versions of these
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
	if	( (acsConfig.process_tests == false)
		||(DontTest))
	{
		return;
	}

	string	original;
	string	stack = getStack(id, original);

	auto entry = TestDoubleData.find(stack);
	if (entry == TestDoubleData.end())
	{
		if (original != stack)
		{
			return;
		}
		volatile bool output = false;
		if	( (output)
			||(std::find(RecordStackList.begin(), RecordStackList.end(), stack) != RecordStackList.end()))
		{
			auto redirectEntry = TestRedirect.find(stack);
			if (redirectEntry != TestRedirect.end())
			{
				return;
			}

			vector<double> data;
			for (int i = 0; i < n; i++)
			{
				data.push_back(mat[i]);
			}
			TestDoubleData[stack] = data;

			TestOutputStream << "TEST: Added  '" << stack << std::endl;
			NewData = true;
		}
		return;
	}

	int		errorCount = 0;
	int		firstI;
	double	firstA;
	double	firstB;
	int		maxI;
	double	maxA;
	double	maxB;
	double	maxFrac = 0;
	bool	nan = false;
	bool	inf = false;

	auto& data = entry->second;

	bool sizeOk = true;

	if (n != data.size())
	{
		sizeOk = false;
		TestOutputStream << "Bad test length: got " << n << " but expected " << data.size() << " for " << stack << std::endl;

		TestStatus[stack] = -1;

		return;
	}

	map<int, std::tuple<double, double>> errors;

	for (int i = 0; i < n; i++)
	{
		double Old = data[i];
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
			mat[i] = data[i];
		}
		TestOutputStream << "ABSRB";
	}

	if 	( (errorCount						== 0)
		&&(acsConfig.testOpts.output_pass	== false))
	{
		return;
	}

	if (errorCount > 0)	{	TestStatus[stack] = -1;	TestOutputStream << "---- "; }
	else				{	TestStatus[stack] = +1;	TestOutputStream << "PASS "; }

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
	if	( (acsConfig.process_tests == false)
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
	if	( (acsConfig.process_tests == false)
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
	if	( (acsConfig.process_tests == false)
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
	if	( (acsConfig.process_tests == false)
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
	if	( (acsConfig.process_tests == false)
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
	if	( (acsConfig.process_tests == false)
		||(DontTest))
	{
		return;
	}

	testMat(id, &num, 1, 1e-6);
#endif
}

void TestStack::testInt(
	string	id,			///< ID value to append to stack
	int		num)        ///< Integer value to compare
{
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.process_tests == false)
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
#ifdef	ENABLE_UNIT_TESTS
	if	( (acsConfig.process_tests == false)
		||(DontTest))
	{
		return;
	}

	string	original;
	string	stack = getStack(id, original);

	auto entry = TestStringData.find(stack);
	if (entry == TestStringData.end())
	{
		if (stack != original)
		{
			return;
		}

		volatile bool output = false;
		if	( (output)
			||(std::find(RecordStackList.begin(), RecordStackList.end(), stack) != RecordStackList.end()))
		{
			auto redirectEntry = TestRedirect.find(stack);
			if (redirectEntry != TestRedirect.end())
			{
				return;
			}

			TestStringData[stack] = str;

			TestOutputStream << "TEST: Added  '" << stack << std::endl;
		}
		return;
	}

	auto& data = entry->second;

	if 	( (data == str)
		&&(acsConfig.testOpts.output_pass	== false))
	{
		return;
	}

	if (data != str)	{	TestStatus[stack] = -1;	TestOutputStream << "----"; }
	else				{	TestStatus[stack] = +1;	TestOutputStream << "PASS"; }

	if (original.size() > 60)
	{
		original = original.substr(0, 20) + ".." + original.substr(original.size() - 38);
	}
	else
	{
		original.append(60 - original.size(), ' ');
	}

	TestOutputStream << "\t   " << original;

	if	(data != str)
	{
		TestOutputStream
		<< std::endl << "\texpected \"" << data	<< "\""
		<< std::endl << "\tbut got  \"" << str	<< "\""
		<< std::endl << "\t          ";
		for (int i = 0; i < data.size(); i++)
		{
			if (data[i] != str[i])
			{
				TestOutputStream << "!";
				break;
			}
			else
			{
				TestOutputStream << " ";
			}
		}

		if (acsConfig.testOpts.stop_on_fail)
		{
			exit(-1);
		}
	}
	TestOutputStream << std::endl;
#endif
}

