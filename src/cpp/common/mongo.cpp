
// #pragma GCC optimize ("O0")


#include "observations.hpp"
#include "rtcmEncoder.hpp"
#include "instrument.hpp"
#include "acsConfig.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "mongo.hpp"

#include <boost/log/trivial.hpp>
#include <boost/log/sinks/sync_frontend.hpp>


using bsoncxx::types::b_date;

namespace sinks = boost::log::sinks;

Mongo*	localMongo_ptr	= nullptr;
Mongo*	remoteMongo_ptr	= nullptr;


mongocxx::instance Mongo::instance;	//single static instance of the driver


void mongoooo()
{
	for (auto config_ptr : {&acsConfig.localMongo, &acsConfig.remoteMongo})
	{
		Mongo** mongo_ptr_ptr;
		
		if (config_ptr == &acsConfig.localMongo)	mongo_ptr_ptr = &localMongo_ptr;
		else										mongo_ptr_ptr = &remoteMongo_ptr;
		
		auto& mongo_ptr = *mongo_ptr_ptr;
			
		if (mongo_ptr)
			continue;

		auto& config = *config_ptr;
		
		if (config.enable == false)
		{
			continue;
		}
		
		try
		{
			mongo_ptr = new Mongo(config.uri);
		}
		catch (...) {} // just eat any exception

		if (mongo_ptr == nullptr)
		{
			continue;
		}

		Mongo& mongo = *mongo_ptr;

		auto c = mongo.pool.acquire();
		mongocxx::client&		client	= *c;
		mongocxx::database		db		= client[config.database];
		BOOST_LOG_TRIVIAL(info)  << "Mongo connecting to database : " << config.database << " @ " << config.uri;
		try
		{
			if (config.delete_history)
			{
				db.drop();
			}

			if (config.output_logs)
			{
				// Construct the sink
				using MongoLogSink = sinks::synchronous_sink<MongoLogSinkBackend>;

				boost::shared_ptr<MongoLogSink> mongoLogSink = boost::make_shared<MongoLogSink>();

				// Register the sink in the logging core
				boost::log::core::get()->add_sink(mongoLogSink);
			}
			
			BOOST_LOG_TRIVIAL(info)  << "Mongo connected  to database : " << config.database;
		}
		catch (...)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: Mongo connection failed - check if service is running at " << config.uri;
			exit(1);
		}
	}
}

void MongoLogSinkBackend::consume(
	boost::log::record_view																	const&	rec,
	sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string)
{
	if (localMongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *localMongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.localMongo.database];
	mongocxx::collection		coll	= db	["Console"];

	//add a azimuth to a chunk
	coll.insert_one(
		document{}
			<< "Epoch"			<< b_date {std::chrono::system_clock::from_time_t((time_t)((PTime)tsync).bigTime)}
			<< "Log"			<< log_string.c_str()
			<< finalize
		);
}
