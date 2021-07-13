#ifndef NTRIPCASTERSERVICE_H
#define NTRIPCASTERSERVICE_H

#include <boost/date_time/posix_time/posix_time.hpp>
#include <memory>
#include <thread> 
#include <chrono> 


#include "ntripSourceTable.hpp"
#include "peaCommitVersion.h"
#include "acsStream.hpp"
#include "acsConfig.hpp"

extern GTime tsync;
extern int epoch;
extern ACSConfig acsConfig;

struct NtripCasterService
{
public:
	std::vector<SourceTableEntry> sourceTableData;
	std::multimap<std::string, std::shared_ptr<NtripRtcmStream>> downloadStreamMap;
	std::multimap<std::string,std::string> traceFiles;
	std::string caster_stream_root;
	
	void startPerformanceMonitoring();
	void makeTraceFiles();
};

#endif
