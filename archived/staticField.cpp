//
// Created by Sébastien Allgeyer on 24/2/22.
//

#include "staticField.hpp"
#include <boost/log/trivial.hpp>
#include <cstdlib>
#include <iostream>
#include <fstream>

#include<vector>
#include<boost/algorithm/string/split.hpp>
#include<boost/algorithm/string.hpp>

StaticField::StaticField(std::string filename_, int degmax_): filename(filename_), degMax(degmax_)
{
	readegm();
	summary();
}


/**
 * Read the static gravity field
 * @todo include the time variable component of the gravity field.
*/
void StaticField::readegm()
{
	std::ifstream infile(filename);
	int n,m;
	double C, S, sigC, sigS, t0_;
	std::string key, line;
	bool header = true;
	gfct = Eigen::MatrixXd::Zero(degMax+1, degMax+1);
	gfctC = Eigen::MatrixXd::Zero(degMax+1, degMax+1);
	gfctS = Eigen::MatrixXd::Zero(degMax+1, degMax+1);

	while (std::getline(infile, line))
	{
		std::istringstream iss(line);
		if (header)
		{
			if (line.compare(0,11,"end_of_head")==0)
			{
				header = false;
			}
			else
			{
				if (line.length() != 0)
				{
					std::vector<std::string> split;
					boost::algorithm::split(split,line,boost::algorithm::is_any_of(" "),boost::token_compress_on);
					if (split[0].compare("modelname") == 0)
						modelName = split[1];
					else if (split[0].compare("earth_gravity_constant") == 0)
						earthGravityConstant = stod(split[1]);
					else if (split[0].compare("radius") == 0)
						earthRadius = stod(split[1]);
					else if (split[0].compare("max_degree") == 0)
						maxDegree = stoi(split[1]);
					else if (split[0].compare("tide_system")==0)
					{
						if (split[1].compare("tide_free") == 0)
							tideFree = true;
						else
							tideFree = false;
					}
				}
			}
		}
		else
		{
			iss >> key >> n >> m >> C >> S >> sigC >> sigS ;
			if (n <= degMax )
			{
				if (key.compare(0,3, "gfc") == 0 )
				{
					gfctC(n,m) = C;
					gfct (n,m) = C;
					if ( m > 0 ) {
						gfctS(n, m) = S;
						gfct  (m-1,n) = S;
					}
				}
			}
			else
			{
				break;
			}
		}
	}
	
    gfctC(2,0) -= -4.1736e-9; 
    // gfctC(2,0) -= -4.1736e-9;
    // gfctC(2,0) = -0.48416948e-3 - 4.1736e-9 + 11.6e-12 * 7.0; 
    // gfctC(3,0) = 0.9571612e-6 + 4.9e-12 * 7.0;
    // gfctC(4,0) = 0.5399659e-6  + 4.7e-12 * 7.0;
    // xp, yp are the mean pole. MAS to RAD
    // double xp = (23.513 + 7.6141 * 7 ) * 4.8481368E-9;
    // double yp = (358.891 - 0.6287 * 7) * 4.8481368E-9;
    // gfctC(2,1) =   sqrt(3) * xp * gfctC(2,0) - xp * gfctC(2,2) + yp * gfctS(2,2);
    // gfctS(2,1) = - sqrt(3) * xp * gfctC(2,0) - yp * gfctC(2,2) - xp * gfctS(2,2);

//	BOOST_LOG_TRIVIAL(debug) << "\n== coeff ==\n" << gfctC << "\n\n" << gfctS<< "\n\n" << gfct ;
}


void StaticField::summary() {
	BOOST_LOG_TRIVIAL(info) << "read file : " << filename;
	BOOST_LOG_TRIVIAL(info) << "    - modelName         " << modelName;
	BOOST_LOG_TRIVIAL(info) << "    - max degree model  " << maxDegree;
	if (maxDegree >= degMax )
		BOOST_LOG_TRIVIAL(info) << "        => read only the first " << degMax << " degrees";
	else
		BOOST_LOG_TRIVIAL(warning) << "        => Maximum degree requested is higher than the model " << degMax << ">" << maxDegree ;
	BOOST_LOG_TRIVIAL(info) << "    - is tide_free      " << tideFree ;

}