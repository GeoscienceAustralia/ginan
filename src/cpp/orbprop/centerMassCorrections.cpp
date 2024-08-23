
#include <boost/log/trivial.hpp>

#include <fstream>
#include <iostream>

#include "centerMassCorrections.hpp"

CenterMassCorrections cmc;

void CenterMassCorrections::read(
	const string&	filename)
{
	if (filename.empty())
	{
		return;
	}

	std::ifstream infile(filename);
	if (!infile)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "CMC file open error " << filename;

		return;
	}

	string line;
	std::getline(infile, line);

	while (std::getline(infile, line))
	{
		std::istringstream iss(line);
		string wavename;
		string dummy;
		double zIn;
		double zOut;
		double xIn;
		double xOut;
		double yIn;
		double yOut;
		iss >> wavename >> dummy >> zIn >> zOut >> xIn >> xOut >> yIn >> yOut;
		data[wavename] << xIn, yIn, zIn, xOut, yOut, zOut;
	}

	for (auto& [wave, coeff] : data)
	{
		BOOST_LOG_TRIVIAL(debug) << wave << " " << coeff.transpose();
	}

//    DoodsonNumbers["K1"] = Array6d (6);
	doodsonNumbers["K1"]	<< 1, 1, 0, 0, 0, 0;	// 165.555
	doodsonNumbers["K2"]	<< 2, 2, 0, 0, 0, 0;	// 275.555
	doodsonNumbers["M2"]	<< 2, 0, 0, 0, 0, 0;	// 255.555
	doodsonNumbers["Mf"]	<< 0, 2, 0, 0, 0, 0;	// 75.555
	doodsonNumbers["Mm"]	<< 0, 1, -1, -1, 0, 0;	// 64.455
	doodsonNumbers["N2"]	<< 2, -2, 0, 2, 0, 0;	// 235.755
	doodsonNumbers["O1"]	<< 1, -1, 0, 0, 0, 0;	// 145.555
	doodsonNumbers["P1"]	<< 1, 1, -2, 0, 0, 0;	// 163.555
	doodsonNumbers["Q1"]	<< 1, -2, 0, 1, 0, 0;	// 135.655
	doodsonNumbers["S2"]	<< 2, 2, -2, 0, 0, 0;	// 273.555
	doodsonNumbers["Ssa"]	<< 0, 0, 2, 0, 0, 0;	// 57.555

	initialized = true;
}

Vector3d CenterMassCorrections::estimate(
	Array6d& dood)
{
	Vector3d cmcEstimate = Vector3d::Zero();
	for (auto& [wave, coeff] : data)
	{
		double theta = (dood * doodsonNumbers[wave]).sum();
		for (int i = 0; i < 3; i++ )
		{
			cmcEstimate(i) += coeff[i*2]	* cos(theta)
							+ coeff[i*2+1]	* sin(theta);	//todo aaron this would be better with 2 arrays or a matrix for cos/si
		}
	}
	return cmcEstimate;
}
