
#ifndef ___MONGO_HPP__
#define ___MONGO_HPP__


#include <bsoncxx/builder/basic/document.hpp>

#include <bsoncxx/builder/stream/document.hpp>
#include <bsoncxx/builder/stream/helpers.hpp>
#include <bsoncxx/builder/stream/array.hpp>
#include <bsoncxx/json.hpp>

#include <mongocxx/instance.hpp>
#include <mongocxx/client.hpp>
#include <mongocxx/pool.hpp>
#include <mongocxx/stdx.hpp>
#include <mongocxx/uri.hpp>

#include <boost/log/sinks/basic_sink_backend.hpp>

#include <string>
#include <map>


using std::string;
using std::map;


#include "station.hpp"
#include "observations.hpp"
#include "algebra.hpp"
#include "networkEstimator.hpp"


struct DBEntry
{
	map<string, tuple<string,	bool>>		stringMap;
	map<string, tuple<GTime,	bool>>		timeMap;
	map<string, tuple<double,	bool>>		doubleMap;
	map<string, tuple<int,		bool>>		intMap;	
	map<string, tuple<Vector3d,	bool>>		vectorMap;	
};

using bsoncxx::builder::stream::close_array;
using bsoncxx::builder::stream::close_document;
using bsoncxx::builder::stream::document;
using bsoncxx::builder::stream::finalize;
using bsoncxx::builder::stream::open_array;
using bsoncxx::builder::stream::open_document;

namespace sinks = boost::log::sinks;


struct Mongo
{
	mongocxx::instance		instance; 	// This should be done only once.
	mongocxx::uri			uri;
	mongocxx::pool			pool;

	Mongo(string uriString) : uri{uriString}, pool{uri}
	{

	}
};

#define SSR_DATA		"Data"
#define SSR_PHAS_BIAS	"PBias"
#define SSR_CODE_BIAS	"CBias"
#define SSR_EPHEMERIS	"Eph"
#define SSR_CLOCK		"Clk"

#define SSR_DB			"SSRData"

#define SSR_EPOCH		"Epoch"
#define SSR_UPDATED		"Updated"
#define SSR_SAT	 		"Sat"
#define SSR_IODE		"Iode"
#define SSR_POS			"Pos"
#define SSR_VEL			"Vel"
#define SSR_OBSCODE		"ObsCode"
#define SSR_BIAS		"Bias"
#define SSR_VAR			"Var"

#define SSR_BRDC		"Brdc"
#define SSR_PREC		"Prec"



struct MongoLogSinkBackend : public sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>
{
	// The function consumes the log records that come from the frontend
	void consume(
		boost::log::record_view																	const&	rec,
		sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string);
};

void mongoooo();

extern Mongo*	mongo_ptr;


#define MONGO_NOT_INITIALISED_MESSAGE BOOST_LOG_TRIVIAL(warning)	<< "Mongo actions requested but mongo is not available - check it is enabled and connected correctly"

#endif

