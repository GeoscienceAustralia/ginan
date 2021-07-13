
#ifdef ENABLE_MONGODB


#include "observations.hpp"
#include "acsConfig.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "mongo.hpp"

#include <boost/log/trivial.hpp>
#include <boost/log/sinks/sync_frontend.hpp>

namespace sinks = boost::log::sinks;

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
	mongocxx::database			db		= client[acsConfig.config_description];
	mongocxx::collection		coll	= db	["Console"];

	//add a azimuth to a chunk
	auto result = coll.insert_one(
		document{}
			<< "Epoch"			<< (double) tsync.time
			<< "Log"			<< log_string.c_str()
			<< finalize
		);
}

void mongoMeasSatStat(
	ObsList&			obsList)
{
	if (mongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.config_description];
	mongocxx::collection		coll	= db	["Measurements"];
	mongocxx::options::update	options;
	options.upsert(true);

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		SatStat& satStat = *obs.satStat_ptr;

		//add a azimuth to a chunk
		auto result = coll.update_one(
			document{}
				<< "Epoch"			<< (double) tsync.time
				<< "Site"			<< obs.mount
				<< "Sat"			<< obs.Sat.id()
				<< finalize,

			document{}
				<< "$set"
				<< open_document
					<< "Azimuth"	<< satStat.az * R2D
					<< "Elevation"	<< satStat.el * R2D
				<< close_document
				<< finalize,

			options
			);
	}
}

void mongoMeasResiduals(
	vector<ObsKey>		obsKeys,
	VectorXd&			prefits,
	VectorXd&			postfits,
	VectorXd&			variance)
{
	if (mongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.config_description];
	mongocxx::collection		coll	= db	["Measurements"];
	mongocxx::options::update	options;
	options.upsert(true);

	for (int i = 0; i < prefits.rows(); i++)
	{
		ObsKey& obsKey = obsKeys[i];

		string name = obsKey.type + std::to_string(obsKey.num);

		//add residuals to a chunk
		auto result = coll.update_one(
			document{}
				<< "Epoch"		<< (double) tsync.time
				<< "Site"		<< obsKey.str
				<< "Sat"		<< obsKey.Sat.id()
				<< finalize,

			document{}
				<< "$set"
				<< open_document
					<< name + "-Prefit"		<< prefits(i)
					<< name + "-Postfit"	<< postfits(i)
					<< name + "-Variance"	<< variance(i)
				<< close_document
				<< finalize,

			options
			);
	}
}

void mongoStates(
	KFState&			kfState,
	string				prefix)
{
	if (mongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.config_description];
	mongocxx::collection		coll	= db	["States"];
	mongocxx::options::update	options;
	options.upsert(true);

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if (key.type == KF::ONE)
		{
			continue;
		}

		//add states to a chunk
		auto result = coll.update_one(
			document{}
				<< "Epoch"		<< (double) tsync.time
				<< "Site"		<< key.str
				<< "Sat"		<< key.Sat.id()
				<< "State"		<< KF::_from_integral_unchecked(key.type)._to_string()
				<< finalize,

			document{}
				<< "$set"
				<< open_document
					<< prefix + "x" + std::to_string(key.num)		<< kfState.x(index)
					<< prefix + "P" + std::to_string(key.num)		<< kfState.P(index,index)
				<< close_document
				<< finalize,

			options
			);
	}
}

void mongoooo()
{
	if (mongo_ptr != nullptr)
		return;

	try {
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
	mongocxx::database		db		= client[acsConfig.config_description];
// 	mongocxx::collection	coll	= db	["Measurements"];

	if (acsConfig.delete_mongo_history)
	{
		db["Measurements"]	.delete_many({});
		db["States"]		.delete_many({});
		db["Console"]		.delete_many({});
	}

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

	if (acsConfig.output_mongo_logs)
	{
		// Construct the sink
		using MongoLogSink = sinks::synchronous_sink<MongoLogSinkBackend>;

		boost::shared_ptr<MongoLogSink> mongoLogSink = boost::make_shared<MongoLogSink>();

		// Register the sink in the logging core
		boost::log::core::get()->add_sink(mongoLogSink);
	}

// 	auto builder = bsoncxx::builder::stream::document();
// 	bsoncxx::document::value doc_value = builder
// 	<< "epoch"		<< 1
// 	<< "site"		<< "ALIC"
// 	<< "sat"		<< i
// 	<< "versions"
// 		<< open_array
// 		<< "v3.2"
// 		<< "v3.0"
// 		<< "v2.6"
// 		<< close_array
// 	<< "info"
// 		<< open_document
// 		<< "x" << 203
// 		<< "y" << 102
// 		<< close_document
// 	<< finalize;
// 	bsoncxx::document::view view = doc_value.view();


// 	mongoMeasSatStat	(client);

// 	mongocxx::cursor cursor = coll.find({});
// 	for(auto doc : cursor)
// 	{
// 		std::cout << bsoncxx::to_json(doc) << "\n";
// 	}
}


#endif
