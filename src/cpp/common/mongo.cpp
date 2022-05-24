
// #pragma GCC optimize ("O0")



#include "observations.hpp"
#include "rtcmEncoder.hpp"
#include "acsConfig.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "mongo.hpp"

#include <boost/log/trivial.hpp>
#include <boost/log/sinks/sync_frontend.hpp>


namespace sinks = boost::log::sinks;

Mongo*	mongo_ptr = nullptr;

void mongoooo()
{
	if (mongo_ptr)
		return;

	if (acsConfig.enable_mongo == false)
	{
		return;
	}
	
	try
	{
		mongo_ptr = new Mongo(acsConfig.mongo_uri);
	}
	catch (...) {} // just eat any exception

	if (mongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto c = mongo.pool.acquire();
	mongocxx::client&		client	= *c;
	mongocxx::database		db		= client[acsConfig.mongo_database];
	
	BOOST_LOG_TRIVIAL(info)  << "Mongo connecting to database : " << acsConfig.config_description;
	try
	{
		if (acsConfig.delete_mongo_history)
		{
			db["Measurements"]	.drop();
			db["States"]		.drop();
			db["Console"]		.drop();
			db["SSRData"]		.drop();
		}

		//create compound and simple indicies for most useful types
		
		db["Measurements"]	.create_index(
								document{}
									<< "Epoch"	<< 1
									<< "Site"	<< 1
									<< "Sat"	<< 1
									<< finalize,
								{});

		db["States"]		.create_index(
								document{}
									<< "Epoch"	<< 1
									<< "Site"	<< 1
									<< "Sat"	<< 1
									<< finalize,
								{});

		db["SSRData"]		.create_index(
								document{}
									<< "Epoch"		<< 1
									<< "Sat"		<< 1
									<< "Type"		<< 1
									<< "Data"		<< 1
									<< "ObsCode"	<< 1
									<< finalize,
								{});

		db["Measurements"]	.create_index(	document{}	<< "Epoch"	<< 1	<< finalize,	{});
		db["Measurements"]	.create_index(	document{}	<< "Site"	<< 1	<< finalize,	{});
		db["Measurements"]	.create_index(	document{}	<< "Sat"	<< 1	<< finalize,	{});
		
		db["States"]		.create_index(	document{}	<< "Epoch"	<< 1	<< finalize,	{});
		db["States"]		.create_index(	document{}	<< "Sat"	<< 1	<< finalize,	{});
		db["States"]		.create_index(	document{}	<< "Site"	<< 1	<< finalize,	{});
		
		db["SSRData"]		.create_index(	document{}	<< "ObsCode"<< 1	<< finalize,	{});
		db["SSRData"]		.create_index(	document{}	<< "Data"	<< 1	<< finalize,	{});
		db["SSRData"]		.create_index(	document{}	<< "Type"	<< 1	<< finalize,	{});
		db["SSRData"]		.create_index(	document{}	<< "Sat"	<< 1	<< finalize,	{});
		db["SSRData"]		.create_index(	document{}	<< "Epoch"	<< 1	<< finalize,	{});
		

		if (acsConfig.output_mongo_logs)
		{
			// Construct the sink
			using MongoLogSink = sinks::synchronous_sink<MongoLogSinkBackend>;

			boost::shared_ptr<MongoLogSink> mongoLogSink = boost::make_shared<MongoLogSink>();

			// Register the sink in the logging core
			boost::log::core::get()->add_sink(mongoLogSink);
		}
		
		BOOST_LOG_TRIVIAL(info)  << "Mongo connected to database : " << acsConfig.config_description;
	}
	catch (...)
	{
		BOOST_LOG_TRIVIAL(error) << "Mongo connection failed - check if service is running at " << acsConfig.mongo_uri;
		exit(1);
	}
}

void MongoLogSinkBackend::consume(
	boost::log::record_view																	const&	rec,
	sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string)
{
	if (mongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.mongo_database];
	mongocxx::collection		coll	= db	["Console"];

	//add a azimuth to a chunk
	coll.insert_one(
		document{}
			<< "Epoch"			<< bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(tsync.time)}
			<< "Log"			<< log_string.c_str()
			<< finalize
		);
}
