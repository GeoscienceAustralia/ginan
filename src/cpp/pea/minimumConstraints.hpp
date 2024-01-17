
#pragma once

#include <string>
#include <map>

using std::string;
using std::map;

#include "trace.hpp"

struct ReceiverMap;
struct KFState;

struct SumCount
{
	double	sum		= 0;
	int		count	= 0;
};

struct MinconStatistics : map<string, map<string, SumCount[2]>>
{

};

void mincon(
	Trace&				trace,
	KFState&			kfStateStations,
	MinconStatistics*	minconStatistics_ptr0	= nullptr,
	MinconStatistics*	minconStatistics_ptr1	= nullptr,
	bool				commentSinex			= false,
	KFState*			kfStateTransform_ptr	= nullptr);

void outputMinconStatistics(
	Trace&				trace,
	MinconStatistics&	minconStatistics,
	const string&		suffix = "");

KFState minconOnly(
	Trace&		trace,
	ReceiverMap&	receiverMap);
