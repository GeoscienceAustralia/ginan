
#pragma once


#include <stdlib.h>
#include <ctype.h>
#include <string>
#include <tuple>

#include <boost/log/trivial.hpp>

#include "eigenIncluder.hpp"
#include "streamObs.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "trace.hpp"

using std::getline;
using std::ifstream;
using std::string;
using std::tuple;

struct DiscontinuityObject
{
	string		sitecode;			// site code
	string		monuid;				// monument identification
	int			solution = 0;	// valid solution number
	// a P letter, no idea what it stands for ...
	UYds		start;
	UYds		end;
	bool		isVelocity	= false; // position/velocity switch
};

struct SinexParser : Parser, ObsLister
{
	map<int, tuple<GTime, KFKey>>								parameterMap;
	map<GTime, map<KFKey, tuple<double, map<KFKey, double>>>>	valueMap;
	map<GTime, map<string, DiscontinuityObject>>				discontinuityMap;

	// Sinex 2.02 documentation indicates 2 digit years. >50 means 1900+N. <=50 means 2000+N
	// To achieve this, when we read years, if >50 add 1900 else add 2000. This source will
	// cease to work safely around 2045!
	// when we write years, write out modulo 100
	// This only applies to site data, for satellites it is using 4 digit years
	void nearestYear(
		double& year)
	{
		if (year > 50)	year += 1900;
		else			year += 2000;
	}

	void nullFunction(
		string& s)
	{

	}

	void parseSinexEstimates(
		string& s);

	void parseSinexEstimateMatrix(
		string&	s);

	void parseSinexDiscontinuities(
		string&	s);

	void parse(
		std::istream& inputStream)
	{
		string	closure;
		void	(SinexParser::*parseFunction)(string&) = &SinexParser::nullFunction;

		if (!inputStream)
		{
			return;
		}

		while (inputStream)
		{
			string line;

			getline(inputStream, line);

			// test below empty line (ie continue if something on the line)
			if	(!inputStream)
			{
				// error - did not find closure line. Report and clean up.
				BOOST_LOG_TRIVIAL(error)
				<< "Error: Closure line not found before end.";

				break;
			}

			if (line[0] == '*')
			{
				//comment
				continue;
			}

			if (line[0] == '-')
			{
				//end of block
				parseFunction = &SinexParser::nullFunction;

				if (line != closure)
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Error: Incorrect section closure line encountered: "
					<< closure << " != " << line;
				}

				closure = "";

				continue;
			}

			if (line[0] == ' ')
			{
				//this probably needs specialty parsing - use a prepared function pointer.
				(this->*parseFunction)(line);

				continue;
			}

			if (line[0] == '+')
			{
				string	mvs;

				//prepare closing line for comparison
				closure = line;
				closure[0] = '-';
				string lineName = line.substr(0, line.find(' '));
				if		(lineName == "+SOLUTION/ESTIMATE"				)	{ parseFunction = &SinexParser::parseSinexEstimates;			}
				else if	(lineName == "+SOLUTION/DISCONTINUITY"			)	{ parseFunction = &SinexParser::parseSinexDiscontinuities;		}
				else if	(lineName == "+SOLUTION/MATRIX_ESTIMATE"		)	{ parseFunction = &SinexParser::parseSinexEstimateMatrix;		}
				else
				{
	// 				BOOST_LOG_TRIVIAL(error)
	// 				<< "Error: error unknown header line: " << line;
				}

				continue;
			}

			if (line[0] == '%')
			{
				if (line.substr(0, 5) == "%=SNX")
				{
					continue;
				}

				if (line != "%ENDSNX")
				{
					// error in file. report it.
					BOOST_LOG_TRIVIAL(error)
					<< "Error: line starting '%' met not final line" << "\n" << line;

					return;
				}

				continue;
			}
		}

		//aggregate all maps into an obsList type entity
		for (auto& [time, someMap] : valueMap)
		{
			//make an Observation.
			FObs obs;
			obs.time = time;

			int index = 0;

			obs.obsState.kfIndexMap[KFState::oneKey] = index;

			index++;

			for (auto& [key, tuplet] : someMap)
			{
				obs.obsState.kfIndexMap[key]					= index;
				obs.obsState.stateTransitionMap[key][key][0]	= 1;

				index++;
			}

			obs.obsState.x = VectorXd::Zero(index);
// 			obs.obsState.Z = MatrixXd::Zero(index,index);
			obs.obsState.P = MatrixXd::Zero(index,index);

			for (auto& [keyA, tuplet] : someMap)
			{
				auto indexA = obs.obsState.kfIndexMap[keyA];

				auto& [value, covMap] = tuplet;

				obs.obsState.x(indexA) = value;

				for (auto& [keyB, cov] : covMap)
				{
					auto indexB = obs.obsState.kfIndexMap[keyB];

					obs.obsState.P(indexA, indexB) = cov;
				}
			}

			ObsList obsList;

			obsList.push_back((shared_ptr<FObs>)obs);

			std::cout << "Got obs for " << obs.time << "\n";
			obsListList.push_back(std::move(obsList));
		}
	}

	string parserType()
	{
		return "SinexParser";
	}
};
