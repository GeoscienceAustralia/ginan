
#pragma once

#include <vector>
#include <map>

using std::vector;
using std::map;

template<typename TYPE>
struct AzElMapData
{
	double	aziDelta;				///< azimuth increment (degree) 
	double	zenStart;
	double	zenStop;
	double	zenDelta;
	int		nz;						///< number of zenith intervals 
	int		naz;					///< number of non-azimuth dependent intervals 
	
				vector<TYPE>	elMap;
	map<int,	vector<TYPE>>	azElMap;
};
