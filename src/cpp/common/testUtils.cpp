
// #pragma GCC optimize ("O0")

#include <algorithm>
#include <math.h>

#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "common.hpp"


void ErrorExit::consume(
	boost::log::record_view																	const&	rec,
	sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string)
{
	int logLevel = 0;
	auto attrs = rec.attribute_values();
	auto sev = attrs[boost::log::trivial::severity].get();
	switch (sev)
	{
		case boost::log::trivial::trace:			logLevel = 5;			break;
		case boost::log::trivial::debug:			logLevel = 4;			break;
		case boost::log::trivial::info:				logLevel = 3;			break;
		case boost::log::trivial::warning:			logLevel = 2;			break;
		case boost::log::trivial::error:			logLevel = 1;			break;
		case boost::log::trivial::fatal:			logLevel = 0;			break;
	}

	if (logLevel <= acsConfig.fatal_level)
	{
		std::cout << "\n" << "Message met fatal_message_level condition for exit.\nExiting...\n\n";
		exit(0);
	}
}


void exitOnErrors()
{
	// Construct the sink
	using LogSink = sinks::synchronous_sink<ErrorExit>;

	boost::shared_ptr<LogSink> logSink = boost::make_shared<LogSink>();

	// Register the sink in the logging core
	boost::log::core::get()->add_sink(logSink);
}


#ifdef PLUMBER

size_t bucket = 0;
#include <malloc.h>

#include "streamParser.hpp"
#include "streamNtrip.hpp"
#include "navigation.hpp"
#include "streamRtcm.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "biases.hpp"


static void* plumber_hook(size_t size, const void* caller);
static void* plumber_hook(size_t size, const void* caller)
{
	void*	result;

	/* Restore all old hooks */
	/* Call recursively */
	__malloc_hook		= 0;
	{
		result = malloc(size);
	}
	__malloc_hook		= plumber_hook;

	bucket += size;

	return result;
}


template<typename T>
size_t plumberTest(T& t)
{
	//begin plumbing
	bucket = 0;

	__malloc_hook	= plumber_hook;
	{
		T newT = t;
	}
	__malloc_hook	= 0;

	return bucket;
}

#endif

void plumber()
{
#ifdef PLUMBER
	static map<string, size_t>	plumberMap;

	size_t New;
	string v;

	printf("Checking plumbing:\n");
	for (auto& [id, satNav] : nav.satNavMap)
	{
		v = id.id();			New = plumberTest(satNav						);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	}

	v = "biasMaps";				New = plumberTest(biasMaps						);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "receiverMap";			New = plumberTest(receiverMap					);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "nav";					New = plumberTest(nav							);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "ephMap";				New = plumberTest(nav.ephMap					);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "gephMap";				New = plumberTest(nav.gephMap					);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "sephMap";				New = plumberTest(nav.sephMap					);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "pephMap";				New = plumberTest(nav.pephMap					);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "pclkMap";				New = plumberTest(nav.pclkMap					);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "satNavMap";			New = plumberTest(nav.satNavMap					);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "tecMap";				New = plumberTest(nav.tecMap					);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "pcoMap";				New = plumberTest(nav.pcoMap					);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "pcoMap";				New = plumberTest(nav.pcoMap					);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;

	v = "satOptsMap";			New = plumberTest(acsConfig.satOptsMap			);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "recOptsMap";			New = plumberTest(acsConfig.recOptsMap			);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "yamls";				New = plumberTest(acsConfig.yamls				);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "ionoOpts";				New = plumberTest(acsConfig.ionoOpts			);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "pppOpts";				New = plumberTest(acsConfig.pppOpts				);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "minconOpts";				New = plumberTest(acsConfig.minconOpts			);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "localMongo";			New = plumberTest(acsConfig.localMongo			);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "remoteMongo";			New = plumberTest(acsConfig.remoteMongo			);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "slrOpts";				New = plumberTest(acsConfig.slrOpts				);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "netOpts";				New = plumberTest(acsConfig.netOpts				);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
	v = "availableOptions";		New = plumberTest(acsConfig.availableOptions	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;

	printf("\n");
#endif
}
