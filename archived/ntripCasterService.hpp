#ifndef NTRIPCASTERSERVICE_H
#define NTRIPCASTERSERVICE_H

#include <boost/date_time/posix_time/posix_time.hpp>
#include <memory>
#include <thread> 
#include <chrono> 

#include <map>
#include <string>
#include <vector>

using std::string;
using std::vector;
using std::multimap;


#include "ntripSourceTable.hpp"
#include "peaCommitVersion.h"
#include "acsStream.hpp"
#include "acsConfig.hpp"
#include "fileLog.hpp"

extern GTime tsync;
extern int epoch;
extern ACSConfig acsConfig;

struct NtripCasterService
{
	vector<SourceTableEntry>							sourceTableData;
	multimap<string, std::shared_ptr<NtripRtcmStream>>	downloadStreamMap;
	multimap<string, string>							traceFiles;
	string												caster_stream_root;
	
	void startPerformanceMonitoring();
	void makeTraceFiles();
};


#endif
