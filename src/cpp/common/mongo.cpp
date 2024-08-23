
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

/** Persistant formatted storage and inter-process communication.
 * A mongodb database is mainly used for storage of filter states and measurements.
 * This data is primarily used for later analysis and plotting using the python utility.
 *
 * Typically the data is stored in States and Measurements collections, with pseudo-indexes collections to allow other applications to see at-a-glance which elements are available for retrieval, without searching the whole db.
 *
 * The database's States collection is used as a method of inter-process communication, providing simple configuration of cross-network data passing, integrity and backup.
 * It is used for recording clocks and orbits from a POD process, which are then sampled and formatted as RTCM SSR outputs
 * - This allows separation of the estimation of parameters, and the generation and transmission of RTCM messages.
 *
 * The states collection may also be used to pass current or predicted values to another Pea instance, allowing several filters to run in a fast/slow configuration.
 * In this case the states entries are marked with the time of update/prediction, and other db entries are used to signify validity of complete sets of db entries using the 'updated' time.
 */
Database Mongo_Database__()
{

}



#include "inputsOutputs.hpp"
#include "observations.hpp"
#include "rtcmEncoder.hpp"
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
	auto mongo_ptr	= mongo_ptr_arr[instance];

	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return;
	}

	auto& mongo		= *mongo_ptr;

	auto& config	= acsConfig.mongoOpts[instance];

	getMongoCollection(mongo, "Content");

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

		auto opts = mongocxx::options::index();
		opts.sparse(true);

		db[STATES_DB].create_index(
				document{}
					<< MONGO_TYPE	<< 1
					<< finalize,
				opts);

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
		BOOST_LOG_TRIVIAL(fatal) << "Error: Mongo connection failed - check if service is running at " << config.uri;
	}
}

void checkValidDbname(
	string& new_database)
{
	if (new_database.empty())
	{
		BOOST_LOG_TRIVIAL(fatal) << "Error: Mongo database name is empty";
	}
	string old_database = new_database;
	bool invalid = false;
	for (string invalidChar : {"/", "\\", ".", "$", "*", "<", ">", ":", "|", "?"})
	{
		invalid |=  replaceString(new_database, invalidChar, "", false);
	}
	if (invalid)
	{
		BOOST_LOG_TRIVIAL(warning) << "Error: Mongo database name contains invalid characters, new database is: " << new_database << " previously: " << old_database;
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
	checkValidDbname(new_database);

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
	DOCS_REFERENCE(Mongo_Database__);

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

string formatSeries(const string series)
{
	string formatted = series;
	formatted[0] = '_';
	std::transform(formatted.begin(), formatted.end(), formatted.begin(), ::tolower);
	return formatted;
}

void MongoLogSinkBackend::consume(
	boost::log::record_view																	const&	rec,
	sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string)
{
	for (auto instance : {E_Mongo::PRIMARY, E_Mongo::SECONDARY})
	{
		auto mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		getMongoCollection(mongo, "Console");

		//add a azimuth to a chunk
		coll.insert_one(
			document{}
				<< "Epoch"			<< b_date {std::chrono::system_clock::from_time_t((time_t)((PTime)tsync).bigTime)}
				<< "Log"			<< log_string.c_str()
				<< finalize
			);
	}
}

