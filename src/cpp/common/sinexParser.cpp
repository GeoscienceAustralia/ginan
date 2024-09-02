
// #pragma GCC optimize ("O0")

#include "sinexParser.hpp"
#include "receiver.hpp"

#include <string>

using std::string;

void SinexParser::parseSinexEstimates(
	string& s)
{
	KFKey kfKey;

	string type		= s.substr(7,	6);

	if		(type == "STAX  ")	{	kfKey.num = 0;	kfKey.type = KF::REC_POS;	}
	else if (type == "STAY  ")	{	kfKey.num = 1;	kfKey.type = KF::REC_POS;	}
	else if (type == "STAZ  ")	{	kfKey.num = 2;	kfKey.type = KF::REC_POS;	}

	if (kfKey.type != KF::REC_POS)
	{
		return;
	}

	kfKey.str		= s.substr(14,	4);
	kfKey.rec_ptr	= &receiverMap[kfKey.str];

	int parameterNumber = atoi(s.substr(1, 5).c_str());

	UYds yds;
	int	readcount;
	readcount = sscanf(s.c_str() + 27, "%2lf:%3lf:%5lf",
						&yds[0],
						&yds[1],
						&yds[2]);

	if (readcount != 3)
	{
		return;
	}

	if 	( yds[0] != 0
		||yds[1] != 0
		||yds[2] != 0)
	{
		nearestYear(yds[0]);
	}

	double x;
	double P0;

	readcount = sscanf(s.c_str() + 47, "%21lf %11lf",
						&x,
						&P0);

	if (readcount != 2)
	{
		return;
	}

	GTime time = yds;

	parameterMap[parameterNumber] = {time, kfKey};

	auto& [value, covMap] = valueMap[time][kfKey];

	value = x;
	covMap[kfKey] = P0;
}

void SinexParser::parseSinexEstimateMatrix(
	string&	s)
{
	int row;
	int col0;
	double values[3];
	int readcount = sscanf(s.c_str(), " %5d %5d %21lf %21lf %21lf",
						&row,
						&col0,
						&values[0],
						&values[1],
						&values[2]);

	if (readcount < 3)
	{
		return;
	}

	int covars = readcount - 2;

	for (int i = 0; i < covars; i++)
	{
		auto& [timeA, kfKeyA] = parameterMap[row];
		auto& [timeB, kfKeyB] = parameterMap[col0 + i];

		if (row != col0 + i)
		{
// 			continue;
		}

		if	(  timeA.bigTime == 0
			|| timeB.bigTime == 0)
		{
			continue;
		}

		auto& [valueA, covMapA] = valueMap[timeA][kfKeyA];		covMapA[kfKeyB] = values[i];
		auto& [valueB, covMapB] = valueMap[timeB][kfKeyB];		covMapB[kfKeyA] = values[i];
	}
}

void SinexParser::parseSinexDiscontinuities(
	string&	s)
{
	//  0194  A    1 P 00:000:00000 03:160:00000 P - antenna change
	DiscontinuityObject dObj;

	dObj.sitecode = s.substr(1, 4);
	dObj.monuid = s[7];
	dObj.solution = atoi(s.substr(9, 4).c_str());
	dObj.isVelocity = s[42] == 'V';

	UYds startYds;
	UYds endYds;

	int	readcount;
	readcount = sscanf(s.c_str() + 16, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf",
						&startYds[0],
						&startYds[1],
						&startYds[2],
						&endYds[0],
						&endYds[1],
						&endYds[2]);

	if (readcount != 6)
	{
		return;
	}

	if 	( startYds[0] != 0
		||startYds[1] != 0
		||startYds[2] != 0)
	{
		nearestYear(startYds[0]);
	}

	if 	( endYds[0] != 0
		||endYds[1] != 0
		||endYds[2] != 0)
	{
		nearestYear(endYds[0]);
	}

	dObj.start	= startYds;
	dObj.end	= endYds;

	GTime time = startYds;

	discontinuityMap[time][dObj.sitecode] = dObj;
}
