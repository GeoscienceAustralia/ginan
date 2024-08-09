
#include <boost/log/trivial.hpp>

#include <fstream>
#include <sstream>
#include <iostream>
#include <string>

#include "aod.hpp"

Aod aod;

void Aod::read(
	const string&	filename,
	int				maxDeg)
{
	std::ifstream infile(filename);
	if (!infile)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Aod file open error " << filename;

		return;
	}

	string line;
	while (std::getline(infile, line))
	{
		if (line.find("DATA SET") == std::string::npos)
		{
			continue;
		}
		int dataSetNumber;
		int totalCoefficients;
		int epoch[6];
		char type[4];
		int found = sscanf(line.c_str(), "DATA SET %d: %d COEFFICIENTS FOR %d-%d-%d %d:%d:%d OF TYPE %3s",
						&dataSetNumber,
						&totalCoefficients,
						&epoch[0],
						&epoch[1],
						&epoch[2],
						&epoch[3],
						&epoch[4],
						&epoch[5],
						type);

		if (found != 9)
		{
			continue;
		}

		if (string(type) == "glo")
		{
			AodData data_;
			data_.Cnm = MatrixXd::Zero(maxDeg+1, maxDeg+1);
			data_.Snm = MatrixXd::Zero(maxDeg+1, maxDeg+1);

			GEpoch epoch_f;
			for (int i = 0 ; i <6; i++)
				epoch_f[i] = (double)epoch[i];

			GTime time = epoch_f;
			for (int i = 0; i < totalCoefficients; i++)
			{
				int n;
				int m;
				double C;
				double S;
				std::getline(infile, line);

				int found = sscanf(line.c_str(), "%d %d %lf %lf", &n, &m, &C, &S);

				if (found != 4)
				{
					continue;
				}

				if (n <= maxDeg)
				{
					data_.Cnm(n, m) = C;
					data_.Snm(n, m) = S;
				}
			}
			data[time] = data_;
		}
	}
}

void Aod::interpolate(
	const GTime	time,
	MatrixXd&	Cnm,
	MatrixXd&	Snm)
{
	// Right now linear interpolation as it is recomanded by the technical note
	auto it = data.lower_bound(time);

	if (it == data.end())		{	it--;	}
	if (it == data.begin())		{	it++;	}

	auto it2 = it;
	it2--;

	double t1 = (time - it2->first)	.to_double();
	double t2 = (it->first - time)	.to_double();
	double t = t1 + t2;

	Cnm = (it->second.Cnm * t1 + it2->second.Cnm * t2) / t;
	Snm = (it->second.Snm * t1 + it2->second.Snm * t2) / t;
}
