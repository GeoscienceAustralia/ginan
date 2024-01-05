
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

array<Mongo*, 3>	mongo_ptr_arr	= {};


mongocxx::instance Mongo::instance;	//single static instance of the driver

vector<E_Mongo> mongoInstances(
	E_Mongo		selection)
{
	vector<E_Mongo>	instances;

	if (selection == +E_Mongo::PRIMARY		|| selection == +E_Mongo::BOTH)		instances.push_back(E_Mongo::PRIMARY);
	if (selection == +E_Mongo::SECONDARY	|| selection == +E_Mongo::BOTH)		instances.push_back(E_Mongo::SECONDARY);

	return instances;
}


void newMongoDatabase(
	E_Mongo		instance)
{
	auto& mongo_ptr	= mongo_ptr_arr[instance];

	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return;
	}

	auto& mongo		= *mongo_ptr;
	auto& config	= acsConfig.mongoOpts[instance];

	auto c = mongo.pool.acquire();
	mongocxx::client&		client	= *c;
	mongocxx::database		db		= client[mongo.database];

	BOOST_LOG_TRIVIAL(info)  << "Mongo connecting to database : " << mongo.database << " @ " << config.uri;
	try
	{
		auto dropInstances = mongoInstances(acsConfig.mongoOpts.delete_history);
		if (std::find(dropInstances.begin(), dropInstances.end(), instance) != dropInstances.end())
		{
			db.drop();
		}

		db[SSR_DB].create_index(
				document{}
					<< "Epoch"		<< 1
					<< "Sat"		<< 1
					<< "Type"		<< 1
					<< "Data"		<< 1
					<< "ObsCode"	<< 1
					<< finalize,
				{});

		auto logInstances = mongoInstances(acsConfig.mongoOpts.output_logs);
		if (std::find(logInstances.begin(), logInstances.end(), instance) != logInstances.end())
		{
			// Construct the sink
			using MongoLogSink = sinks::synchronous_sink<MongoLogSinkBackend>;

			boost::shared_ptr<MongoLogSink> mongoLogSink = boost::make_shared<MongoLogSink>();

			// Register the sink in the logging core
			boost::log::core::get()->add_sink(mongoLogSink);
		}

		BOOST_LOG_TRIVIAL(info)  << "Mongo connected  to database : " << mongo.database;
	}
	catch (...)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: Mongo connection failed - check if service is running at " << config.uri;
		exit(1);
	}
}

bool startNewMongoDb(
	const string&				id,
	boost::posix_time::ptime	logptime,
	string  					new_database,
	E_Mongo						instance)
{
	auto& mongo_ptr = mongo_ptr_arr[instance];

	if (mongo_ptr == nullptr)
	{
		return false;
	}

	auto& mongo = *mongo_ptr;

	replaceString(new_database, "<RECEIVER>", id);
	replaceTimes (new_database, logptime);

	// Create the database if its a new name, otherwise, keep the old one
	if	( new_database == mongo.database
		||new_database.empty())
	{
		//the filename is the same, keep using the old ones
		return false;
	}

	mongo.database = new_database;

	newMongoDatabase(instance);

	return true;
}

void mongoooo()
{
	auto instances = mongoInstances(acsConfig.mongoOpts.enable);

	for (auto instance : instances)
	{
		auto& mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr)
			continue;

		auto& config = acsConfig.mongoOpts[instance];

		try
		{
			BOOST_LOG_TRIVIAL(info)  << "Mongo connecting to database @ " << config.uri;
			mongo_ptr = new Mongo(config.uri);
		}
		catch (...) {} // just eat any exception
	}
}

void MongoLogSinkBackend::consume(
	boost::log::record_view																	const&	rec,
	sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string)
{
	for (auto instance : {E_Mongo::PRIMARY, E_Mongo::SECONDARY})
	{
		auto& mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		auto 						c		= mongo.pool.acquire();
		mongocxx::client&			client	= *c;
		mongocxx::database			db		= client[mongo.database];
		mongocxx::collection		coll	= db	["Console"];

		//add a azimuth to a chunk
		coll.insert_one(
			document{}
				<< "Epoch"			<< b_date {std::chrono::system_clock::from_time_t((time_t)((PTime)tsync).bigTime)}
				<< "Log"			<< log_string.c_str()
				<< finalize
			);
	}
}
