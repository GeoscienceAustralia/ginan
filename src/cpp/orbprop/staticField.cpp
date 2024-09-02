

#include <iostream>
#include <fstream>
#include <vector>

using std::vector;

#include<boost/algorithm/string/split.hpp>
#include<boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

#include "staticField.hpp"
#include "gTime.hpp"

//todo aaron global
StaticField egm;

/** Read the static gravity field
* @todo include the time variable component of the gravity field.
*/
void StaticField::read(
	const string&	filename,
	int				degMax)
{
	if (filename.empty())
	{
		return;
	}

	std::ifstream infile(filename);
	if (!infile)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "EGM file open error " << filename;

		return;
	}

	this->filename = filename;

	bool header = true;
	gfctC	= MatrixXd::Zero(degMax + 1, degMax + 1);
	gfctS	= MatrixXd::Zero(degMax + 1, degMax + 1);

	string line;
	int maxDegreeRead = 0;
	while (std::getline(infile, line))
	{
		std::istringstream iss(line);
		if (header)
		{
			if (line.compare(0, 11, "end_of_head") == 0)
			{
				header = false;
			}
			else
			{
				if (line.length() != 0)
				{
					vector<string> split;
					boost::algorithm::split(split, line, boost::algorithm::is_any_of(" "), boost::token_compress_on);
					if		(split[0].compare("modelname")				== 0)			modelName				= 		split[1];
					else if	(split[0].compare("earth_gravity_constant")	== 0)			earthGravityConstant	= stod(	split[1]);
					else if	(split[0].compare("radius")					== 0)			earthRadius				= stod(	split[1]);
					else if	(split[0].compare("max_degree")				== 0)			maxDegree				= stoi(	split[1]);
					else if	(split[0].compare("tide_system")			== 0)
					{
						if	(split[1].compare("tide_free")			== 0)				isTideFree = true;
						else															isTideFree = false;
					}
				}
			}
		}
		else
		{
			int n;
			int m;
			double C;
			double S;
			double sigC;
			double sigs;
			string key;
			iss >> key >> n >> m >> C >> S >> sigC >> sigs ;
			if (n <= degMax)
			{
				maxDegreeRead = std::max(maxDegreeRead, n);
				if (key.compare(0, 3, "gfc") == 0)
				{
									gfctC(n, m) = C;
					if (m > 0)		gfctS(n, m) = S;
				}
			}
			else
			{
				break;
			}
		}
	}
	if (maxDegreeRead < degMax)
	{
		BOOST_LOG_TRIVIAL(warning) << "Maximum degree requested is higher than the model " << degMax << ">" << maxDegreeRead;
	}
	this->toTideFree();
	initialised = true;
}

void StaticField::toTideFree()
{
	double correction = -4.1736e-9;

	if (isTideFree == false)
	{
		gfctC(2, 0) -= correction;
		BOOST_LOG_TRIVIAL(info) << "Earth gravity field converted to tide free system (compatible with orbit propagation)";
		isTideFree = true;
	}
}

