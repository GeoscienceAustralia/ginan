
#ifndef ___MONGO_HPP__
#define ___MONGO_HPP__

#ifdef ENABLE_MONGODB

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

using bsoncxx::builder::stream::close_array;
using bsoncxx::builder::stream::close_document;
using bsoncxx::builder::stream::document;
using bsoncxx::builder::stream::finalize;
using bsoncxx::builder::stream::open_array;
using bsoncxx::builder::stream::open_document;

namespace sinks = boost::log::sinks;

using std::string;

#include "observations.hpp"
#include "algebra.hpp"

struct Mongo
{
	mongocxx::instance		instance; 	// This should be done only once.
	mongocxx::uri			uri;
	mongocxx::pool			pool;

	Mongo(string uriString) : uri{uriString}, pool{uri}
	{

	}
};

struct MongoLogSinkBackend : public sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>
{
	// The function consumes the log records that come from the frontend
	void consume(
		boost::log::record_view																	const&	rec,
		sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string);
};

void mongoMeasResiduals(
	vector<ObsKey>		obsKeys,
	VectorXd&			prefits,
	VectorXd&			postfits,
	VectorXd&			variance);

void mongoMeasSatStat(
	ObsList&			obsList);

void mongoStates(
	KFState&			kfState,
	string				prefix = "");

void mongoooo();

extern Mongo*	mongo_ptr;

#endif

#endif
