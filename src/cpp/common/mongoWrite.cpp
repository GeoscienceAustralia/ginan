
// #pragma GCC optimize ("O0")


#include "observations.hpp"
#include "rtcmEncoder.hpp"
#include "coordinates.hpp"
#include "mongoWrite.hpp"
#include "GNSSambres.hpp"
#include "orbitProp.hpp"
#include "rtcmTrace.hpp"
#include "acsConfig.hpp"
#include "ionoModel.hpp"
#include "satStat.hpp"
#include "biases.hpp"
#include "common.hpp"
#include "mongo.hpp"


#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/builder/stream/array.hpp>

#include <bsoncxx/json.hpp>

#include <optional>
#include <thread>

using bsoncxx::builder::stream::close_array;
using bsoncxx::builder::stream::close_document;
using bsoncxx::builder::stream::document;
using bsoncxx::builder::stream::finalize;
using bsoncxx::builder::stream::open_array;
using bsoncxx::builder::stream::open_document;
using bsoncxx::builder::basic::kvp;
using bsoncxx::types::b_date;


struct DBEntry
{
	map<string, tuple<string,			bool>>		stringMap;
	map<string, tuple<GTime,			bool>>		timeMap;
	map<string, tuple<double,			bool>>		doubleMap;
	map<string, tuple<int,				bool>>		intMap;
	map<string, tuple<Vector3d,			bool>>		vectorMap;
	map<string, tuple<vector<double>,	bool>>		doubleArrayMap;
	map<string, tuple<deque<bool>,		bool>>		boolArrayMap;
};

struct QueuedMongo
{
	std::optional<	KFState>			kfState_optl;
	std::optional<	KFMeas>				kfMeas_optl;
					vector<DBEntry>		dbEntryList;

	E_MongoType			mongoType;
	vector<E_Mongo>		instances;
	MongoStatesOptions	mongoStatesOpts;

	string				suffix;

	GTime				time;
};

void mongoOutput(
	vector<DBEntry>&	dbEntryList,
	bool				queue,
	vector<E_Mongo>		instances,
	string				collection);

list<QueuedMongo>	mongoQueue;
std::mutex			mongoQueueMutex;
bool				mongoQueueRunning = false;

void mongoQueueRun()
{
	BOOST_LOG_TRIVIAL(debug) << "Running mongo queue thread";

	while (1)
	{
		list<QueuedMongo> localQueue;
		//Guarding while we splice the shared queue to the local queue, which should be quick
		{
			lock_guard<mutex> guard(mongoQueueMutex);
			// We break when the queue is empty so the thread ends and we stop burning cycles
			// It will be restarted when more messages come in
			if (mongoQueue.empty())
			{
				break;
			}

			BOOST_LOG_TRIVIAL(debug) << "Mongo Queue: Splicing " << mongoQueue.size() << " entries for processing";
			localQueue.splice(localQueue.end(), mongoQueue);
		}
		BOOST_LOG_TRIVIAL(debug) << "Mongo Queue: Processing " << localQueue.size() << " entries";

		vector<string> traceJsons;
		for (auto& object : localQueue)
		{
			switch (object.mongoType)
			{
				case E_MongoType::STATES_AVAILABLE:	mongoStatesAvailable(object.kfState_optl->time,							object.mongoStatesOpts);
				case E_MongoType::STATES:			mongoStates			(							*object.kfState_optl,	object.mongoStatesOpts);						break;
				case E_MongoType::RESIDUALS:		mongoMeasResiduals	(object.time,				*object.kfMeas_optl,	false,						object.suffix);		break;
				case E_MongoType::TRACE:			traceJsons.push_back(std::move(object.suffix));																			break;
				case E_MongoType::LIST:				mongoOutput			(object.dbEntryList,								false, object.instances,	object.suffix);		break;
			}
		}
		mongoTrace(traceJsons, false);
	}

	lock_guard<mutex> guard(mongoQueueMutex);
	mongoQueueRunning = false;
}

void queueMongo(
	QueuedMongo& object)		///< Object to output
{
	BOOST_LOG_TRIVIAL(debug) << "Queueing mongo: " << object.mongoType;

	lock_guard<mutex> guard(mongoQueueMutex);

	mongoQueue.push_back(std::move(object));

	if (mongoQueueRunning == false)
	{
		mongoQueueRunning = true;

		std::thread(mongoQueueRun).detach();
	}
}

b_date bDate(
	const GTime& time)
{
	int fractionalMilliseconds = (time.bigTime - (long int) time.bigTime) * 1000;

	auto stdTime = std::chrono::system_clock::from_time_t((time_t)((PTime)time).bigTime);

	stdTime += std::chrono::milliseconds(fractionalMilliseconds);

	return b_date(stdTime);
}

void mongoTestStat(
	KFState&		kfState,
	TestStatistics&	testStatistics)
{
	auto instances = mongoInstances(acsConfig.mongoOpts.output_test_stats);

	if (instances.empty())
	{
		return;
	}

	map<string, double> entries;
	entries["StatsSumOfSquaresPre"		] = testStatistics.sumOfSquaresPre;
	entries["StatsAverageRatioPre"		] = testStatistics.averageRatioPre;
	entries["StatsSumOfSquaresPost"		] = testStatistics.sumOfSquaresPost;
	entries["StatsAverageRatioPost"		] = testStatistics.averageRatioPost;
	entries["StatsChiSquare"			] = testStatistics.chiSq;
	entries["StatsChiSquareThreshold"	] = testStatistics.qc;
	entries["StatsDegreeOfFreedom"		] = testStatistics.dof;
	entries["StatsChiSquarePerDOF"		] = testStatistics.chiSqPerDof;

	for (auto instance : instances)
	{
		auto mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		auto& config	= acsConfig.mongoOpts[instance];

		getMongoCollection(mongo, STATES_DB);

		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);

		auto bulk = coll.create_bulk_write(bulk_opts);

		bool update = false;

		for (auto& [state, value] : entries)
		{
			bsoncxx::builder::stream::document doc{};
			doc	<< REMOTE_EPOCH		<< bDate(kfState.time)
				<< MONGO_STR		<< kfState.id		+ config.suffix
				<< MONGO_SAT		<< ""				+ config.suffix
				<< MONGO_STATE		<< state
				<< MONGO_X			<< value;

			bsoncxx::document::value doc_val = doc << finalize;
			bulk.append(mongocxx::model::insert_one(doc_val.view()));
			update = true;
		}

		if (update)
		{
			bulk.execute();
		}
	}
}

void mongoTrace(
	const vector<string>&	jsons,
	bool					queue)
{
	auto instances = mongoInstances(acsConfig.mongoOpts.output_trace);
	if (instances.empty())
	{
		return;
	}

	if (queue)
	{
		for (auto& json : jsons)
		{
			QueuedMongo queueEntry;
			queueEntry.suffix		= json;
			queueEntry.mongoType	= E_MongoType::TRACE;

			queueMongo(queueEntry);
		}
		return;
	}

	for (auto instance : instances)
	{
		auto mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		getMongoCollection(mongo, MONGO_TRACE);

		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);

		auto bulk = coll.create_bulk_write(bulk_opts);
		for (auto& json : jsons)
		{
			auto doc = bsoncxx::from_json(json);

			bulk.append(mongocxx::model::insert_one(doc.view()));
		}

		bulk.execute();
	}
}

void mongoOutputConfig(
	string& config)
{
	auto instances = mongoInstances(acsConfig.mongoOpts.output_config);
	if (instances.empty())
	{
		return;
	}

	for (auto instance : instances)
	{
		auto mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		getMongoCollection(mongo, MONGO_CONFIG);

		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);

		auto bulk = coll.create_bulk_write(bulk_opts);

		try
		{
			auto doc = bsoncxx::from_json(config);

			bulk.append(mongocxx::model::insert_one(doc.view()));

			bulk.execute();
		}
		catch (...)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: Could not output config to mongo, likely due to empty entries in yaml.";
		}
	}
}

void mongoMeasSatStat(
	ReceiverMap& receiverMap)
{
	auto instances = mongoInstances(acsConfig.mongoOpts.output_measurements);
	if (instances.empty())
	{
		return;
	}

	for (auto instance : instances)
	{
		auto mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		auto& config	= acsConfig.mongoOpts[instance];

		getMongoCollection(mongo, MONGO_GEOMETRY);

		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);

		auto bulk = coll.create_bulk_write(bulk_opts);

		bool update = false;

		for (auto& [id, rec] : receiverMap)
		{
			for (auto& obs_ptr : rec.obsList)
			{
				auto& obs = *obs_ptr;

				try
				{
					auto& satPos  = dynamic_cast<SatPos&>(obs);

					if	(obs.exclude)
						continue;

					if (satPos.satStat_ptr == nullptr)
						continue;

					SatStat& satStat = *satPos.satStat_ptr;

					bsoncxx::builder::stream::document doc{};
					doc		<< MONGO_EPOCH		<< bDate(tsync)
							<< MONGO_STR		<< obs.mount
							<< MONGO_SAT		<< satPos.Sat.id()
							<< MONGO_SERIES		<< formatSeries(config.suffix)
							<< MONGO_AZIMUTH	<< satStat.az		* R2D
							<< MONGO_ELEVATION	<< satStat.el		* R2D
							<< MONGO_NADIR		<< satStat.nadir	* R2D;

					bsoncxx::document::value doc_val = doc << finalize;
					bulk.append(mongocxx::model::insert_one(doc_val.view()));
					update = true;
				}
				catch (...)
				{
				}
			}
		}

		if (update)
			bulk.execute();
	}
}

void mongoMeasResiduals(
	const	GTime&	time,
			KFMeas&	kfMeas,
			bool	queue,
			string	suffix,
			int		beg,
			int		num)
{
	auto instances = mongoInstances(acsConfig.mongoOpts.output_measurements);

	if (instances.empty())
	{
		return;
	}

	if (queue)
	{
		QueuedMongo queueEntry;
		queueEntry.kfMeas_optl	= kfMeas;
		queueEntry.suffix		= suffix;
		queueEntry.time			= time;
		queueEntry.mongoType	= E_MongoType::RESIDUALS;

		queueMongo(queueEntry);

		return;
	}

	for (auto instance : instances)
	{
		auto mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		auto& config	= acsConfig.mongoOpts[instance];

		getMongoCollection(mongo, MONGO_MEASUREMENTS);

		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);

		auto bulk = coll.create_bulk_write(bulk_opts);

		bool update = false;

		if (num < 0)
		{
			num = kfMeas.obsKeys.size();
		}

		map<tuple<string, string>, vector<int>> lookup;

		map<string, bool> indexSeries;
		map<string, bool> indexLabel;
		map<string, bool> indexSite;
		map<string, bool> indexSat;

		for (int i = beg; i < beg + num; i++)
		{
			KFKey& obsKey = kfMeas.obsKeys[i];

			string commentString;
			if (obsKey.comment.empty() == false)
				commentString = obsKey.comment + "-";

			string name = commentString + std::to_string(obsKey.num);

			lookup[{obsKey.str, obsKey.Sat.id()}].push_back(i);
		}

		for (auto& [description, index] : lookup)
		{
			bsoncxx::builder::stream::document doc{};
			auto& [site, sat] = description;
			auto series = formatSeries(config.suffix + suffix);
			doc		<< MONGO_EPOCH		<< bDate(time)
					<< MONGO_STR		<< site
					<< MONGO_SAT		<< sat
					<< MONGO_SERIES		<< series;

			indexSeries	[series]	= true;
			indexSite	[site]		= true;
			indexSat	[sat]		= true;

			for (int& i : index)
			{
				KFKey& obsKey = kfMeas.obsKeys[i];

				string commentString;
				if (obsKey.comment.empty() == false)
					commentString = obsKey.comment + "-";

				string name = commentString + std::to_string(obsKey.num);

				doc << name + "-Prefit"		<<			kfMeas.V	(i)
					<< name + "-Postfit"	<<			kfMeas.VV	(i)
					<< name + "-Sigma"		<<	sqrt(	kfMeas.R	(i,i));

				indexLabel[name + "-Prefit"]	= true;
				indexLabel[name + "-Postfit"]	= true;
				indexLabel[name + "-Sigma"]		= true;

				if	( (instance & acsConfig.mongoOpts.output_components) == +E_Mongo::NONE
					||kfMeas.componentsMaps.empty())
				{
					continue;
				}

				auto& componentsMap = kfMeas.componentsMaps[i];

				double cumulative = 0;

				for (auto& [component, details] : componentsMap)
				{
					auto& [value, desc, var] = details;

					string label	= name
									+ " " + KF::_from_integral_unchecked(obsKey.type)._to_string()
									+ " " + component._to_string();

					doc << label << value;

					indexLabel[label] = true;

					if (acsConfig.mongoOpts.output_cumulative == +E_Mongo::NONE)
					{
						continue;
					}

					cumulative += value;

					string resLabel;
					if (component._to_integral() >= 10)		resLabel = (string) "RES-"	+ std::to_string(component._to_integral());
					else									resLabel = (string) "RES-0"	+ std::to_string(component._to_integral());

					label			= name
									+ " " + KF::_from_integral_unchecked(obsKey.type)._to_string()
									+ "_" + resLabel
									+ " " + component._to_string();

					doc << label << cumulative;

					indexLabel[label] = true;
				}
			}

			bsoncxx::document::value doc_val = doc << finalize;

			bulk.append(mongocxx::model::insert_one(doc_val.view()));

			update = true;
		}

		if (update)
		{
			bulk.execute();

			auto addIndices = [&](string name, map<string, bool> index)
			{
				mongocxx::options::update	options;
				options.upsert(true);

				auto eachDoc = document{};

				auto arrayDoc = eachDoc << "$each" << open_array;
				for (auto &[indexName, unused]: index)
				{
					arrayDoc << indexName;
				}
				arrayDoc << close_array;

				auto findDoc	= document{}									<< MONGO_TYPE	<< name							<< finalize;
				auto updateDoc	= document{} << "$addToSet" << open_document	<< MONGO_VALUES << eachDoc << close_document	<< finalize;

				db[MONGO_CONTENT].update_one(findDoc.view(), updateDoc.view(), options);
			};

			addIndices(MONGO_MEASUREMENTS,					indexLabel);
			addIndices(MONGO_MEASUREMENTS  MONGO_SERIES,	indexSeries);
			addIndices(MONGO_STR,							indexSite);
			addIndices(MONGO_SAT,							indexSat);
		}
	}
}

void mongoStatesAvailable(
	GTime				time,
	MongoStatesOptions	opts)
{
	auto instances = mongoInstances(opts.instances);

	if (instances.empty())
	{
		return;
	}

	if (opts.queue)
	{
		opts.queue = false;

		QueuedMongo queueEntry;
		queueEntry.kfState_optl->time	= time;
		queueEntry.mongoType			= E_MongoType::STATES_AVAILABLE;
		queueEntry.mongoStatesOpts		= opts;

		queueMongo(queueEntry);

		return;
	}

	for (auto instance : instances)
	{
		Mongo* mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo		= *mongo_ptr;
		auto& config	= acsConfig.mongoOpts[instance];

		auto 						c		= mongo.pool.acquire();
		mongocxx::client&			client	= *c;
		mongocxx::database			db		= client[mongo.database];
		mongocxx::collection		coll	= db[opts.collection];

		auto findDoc	= document{}
						<< MONGO_TYPE		<< MONGO_AVAILABLE
						<< MONGO_EPOCH		<< bDate(time)
						<< MONGO_UPDATED	<< bDate(opts.updated)
						<< finalize;

		db[STATES_DB].insert_one(findDoc.view());
	}
}

void mongoStates(
	KFState&			kfState,
	MongoStatesOptions	opts)
{
	auto instances = mongoInstances(opts.instances);

	if (instances.empty())
	{
		return;
	}

	if (opts.queue)
	{
		opts.queue = false;

		QueuedMongo queueEntry;
		queueEntry.kfState_optl		= kfState;
		queueEntry.mongoType		= E_MongoType::STATES;
		queueEntry.mongoStatesOpts	= opts;

		queueMongo(queueEntry);

		return;
	}

	for (auto instance : instances)
	{
		auto mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		auto& config	= acsConfig.mongoOpts[instance];

		getMongoCollection(mongo, opts.collection);

		mongocxx::options::bulk_write bulk_opts;

		bulk_opts.ordered(false);

		auto bulk = coll.create_bulk_write(bulk_opts);

		map<tuple<string, string, string>, vector<tuple<int, int>>> lookup;
		map<string, bool> indexSeries;
		map<string, bool> indexState;
		map<string, bool> indexSite;
		map<string, bool> indexSat;

		for (auto& [key, index] : kfState.kfIndexMap)
		{
			if (key.type == KF::ONE)
			{
				continue;
			}

			lookup[{key.str, key.Sat.id(), KF::_from_integral_unchecked(key.type)._to_string()}].push_back({index, key.num});
		}

		vector<bsoncxx::document::value> documents;

		bool update = false;

		for (auto& [description, index] : lookup)
		{
			auto& [site, sat, state] = description;

			bsoncxx::builder::stream::document	keydoc{};
			bsoncxx::builder::stream::document	valdoc{};
			bsoncxx::builder::stream::document*	val_ptr;

			if (opts.upsert)	{	val_ptr = &valdoc;		valdoc << "$set" << open_document;	}
			else				{	val_ptr = &keydoc;											}

			auto& doc = *val_ptr;
			string series = formatSeries(config.suffix + opts.suffix);
			keydoc	<< MONGO_EPOCH		<< bDate(kfState.time)
					<< MONGO_STR		<< site
					<< MONGO_SAT		<< sat
					<< MONGO_STATE		<< state
					<< MONGO_SERIES		<< series;

			indexSeries	[series]	= true;
			indexState	[state]		= true;
			indexSat	[sat]		= true;
			indexSite	[site]		= true;

			if (opts.updated != GTime::noTime())
			{
				doc << MONGO_UPDATED << b_date{std::chrono::system_clock::from_time_t((time_t)((PTime)opts.updated).bigTime)};
			}

			auto	array_builder = doc << MONGO_X		<< open_array;	for (auto& [i, num]: index)		array_builder << 		kfState.x	(i);	array_builder << close_array;
					array_builder = doc << MONGO_DX		<< open_array;	for (auto& [i, num]: index)		array_builder << 		kfState.dx	(i);	array_builder << close_array;
					array_builder = doc << MONGO_SIGMA	<< open_array;	for (auto& [i, num]: index)		array_builder << sqrt(	kfState.P	(i,i));	array_builder << close_array;
					array_builder = doc << MONGO_NUM	<< open_array;	for (auto& [i, num]: index)		array_builder << num;						array_builder << close_array;

			if (instance & acsConfig.mongoOpts.output_state_covars)
			{
					array_builder = doc << MONGO_COVAR	<< open_array;	for (auto& [i, numI]: index)
																		for (auto& [j, numJ]: index)
																		if (j > i)						array_builder <<		kfState.P	(i,j);	array_builder << close_array;
			}

			if (opts.upsert)		{	valdoc << close_document; 	bulk.append(mongocxx::model::update_one(keydoc.view(), valdoc.view()).upsert(true));	}
			else					{								bulk.append(mongocxx::model::insert_one(keydoc.view()));								}

			update = true;
		}

		if (update)
		{
			bulk.execute();

			auto addIndices = [&](string name, map<string, bool> index)
			{
				mongocxx::options::update		options;
				options.upsert(true);

				auto eachDoc = document{};

				auto arrayDoc = eachDoc << "$each" << open_array;
				for (auto &[indexStr, unused]: index)
				{
					arrayDoc << indexStr;
				}
				arrayDoc << close_array;

				auto findDoc	= document{}									<< MONGO_TYPE	<< name							<< finalize;
				auto updateDoc	= document{} << "$addToSet" << open_document	<< MONGO_VALUES << eachDoc << close_document	<< finalize;

				db[MONGO_CONTENT].update_one(findDoc.view(), updateDoc.view(), options);
			};

			if (opts.index)
			{
				addIndices(MONGO_STATE,					indexState);
				addIndices(MONGO_STATE MONGO_SERIES,	indexSeries);
				addIndices(MONGO_STR,					indexSite);
				addIndices(MONGO_SAT,					indexSat);
			}
		}
	}
}


void mongoCull(
	GTime time)
{
	auto instances = mongoInstances(acsConfig.mongoOpts.cull_history);
	if (instances.empty())
	{
		return;
	}

	for (auto instance : instances)
	{
		Mongo* mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		auto& config	= acsConfig.mongoOpts[instance];

		for (auto collection: {SSR_DB, REMOTE_DATA_DB})
		{
			getMongoCollection(mongo, collection);

			b_date btime{std::chrono::system_clock::from_time_t((time_t)((PTime)(time - acsConfig.mongoOpts.min_cull_age)).bigTime)};

			// Remove all documents that match a condition.
			auto docSys		= document{}	<< SSR_EPOCH
												<< open_document
												<< "$lt" << btime
												<< close_document
											<< finalize;

			coll.delete_many(docSys.view());
		}
	}
}

document entryToDocument(
	DBEntry&	entry,
	bool		type)
{
	// builder::document builds an empty BSON document
	document doc = {};

	for (auto&[k,v]:entry.stringMap)		{auto& [e,b] = v; if (type == b)							doc << k << e;}
	for (auto&[k,v]:entry.intMap)			{auto& [e,b] = v; if (type == b)							doc << k << e;}
	for (auto&[k,v]:entry.doubleMap)		{auto& [e,b] = v; if (type == b)							doc << k << e;}
	for (auto&[k,v]:entry.timeMap)			{auto& [e,b] = v; if (type == b)							doc << k << b_date{std::chrono::system_clock::from_time_t((time_t)((PTime)e).bigTime)};}
	for (auto&[k,v]:entry.vectorMap)		{auto& [e,b] = v; if (type == b) for (int i=0;i<3;i++)		doc << k + std::to_string(i) << e[i];}
	for (auto&[k,v]:entry.doubleArrayMap)	{auto& [e,b] = v; if (type == b) { auto builder = doc << k << open_array; for (auto& a : e) builder << a; builder << close_array; }}
	for (auto&[k,v]:entry.boolArrayMap)		{auto& [e,b] = v; if (type == b) { auto builder = doc << k << open_array; for (auto& a : e) builder << a; builder << close_array; }}

	return doc;
}


void mongoOutput(
	vector<DBEntry>&	dbEntryList,
	bool				queue,
	vector<E_Mongo>		instances,
	string				collection)
{
	if (queue)
	{
		QueuedMongo queueEntry;
		queueEntry.dbEntryList		= dbEntryList;
		queueEntry.instances		= instances;
		queueEntry.suffix			= collection;
		queueEntry.mongoType		= E_MongoType::LIST;

		queueMongo(queueEntry);

		return;
	}

	for (auto instance : instances)
	{
		Mongo* mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		auto 						c		= mongo.pool.acquire();
		mongocxx::client&			client	= *c;
		mongocxx::database			db		= client[mongo.database];
		mongocxx::collection		coll	= db[collection];

		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);

		auto bulk = coll.create_bulk_write(bulk_opts);

		bool update = false;

		for (auto& entry : dbEntryList)
		{
			// builder::document builds an empty BSON document
			auto keys = entryToDocument(entry, true);
			auto vals = entryToDocument(entry, false);

			bsoncxx::builder::basic::document Vals = {};

			Vals.append(kvp("$set", vals));

	// 		std::cout << "\n" << bsoncxx::to_json(keys.view()) << bsoncxx::to_json(Vals.view()) << "\n";

			mongocxx::model::update_one  mongo_req{keys.view(), Vals.view()};

			update = true;
			mongo_req.upsert(true);
			bulk.append(mongo_req);
		}

		if (update)
			bulk.execute();
	}
}

/** Write states generating SSR corrections to Mongo DB
*/
void	prepareSsrStates(
	Trace&				trace,			///< Trace to output to
	KFState&			kfState,		///< Filter object to extract state elements from
	KFState&			ionState,		///< Filter object to extract state elements from
	GTime 				time)			///< Time of current epoch
{
	auto instances = mongoInstances(acsConfig.mongoOpts.output_ssr_precursors);

	if (instances.empty())
	{
		return;
	}

	BOOST_LOG_TRIVIAL(info)
	<< "Calculating SSR message precursors\n";

	vector<DBEntry>	dbEntryList;

 	time.bigTime = (long int) (time.bigTime + 0.5);	// time tags in mongo will be rounded up to whole sec

	for (E_Sys sys : E_Sys::_values())
	{
		string	sysName		= boost::algorithm::to_lower_copy((string) sys._to_string());

		if (acsConfig.process_sys[sys] == false)
			continue;

		auto sats = getSysSats(sys);

		// Calculate orbits + clocks at tsync & tsync+udi
		for (auto& Sat : sats)
		{
			// Create a dummy observation
			GObs obs;
			obs.Sat = Sat;

			auto& satNav = nav.satNavMap[obs.Sat];
			SatStat satStatDummy;

			obs.satStat_ptr	= &satStatDummy;
			obs.satNav_ptr	= &satNav;

			GTime teph = time;

			// Clock and orbit corrections (and predicted corrections)
			for (int tpredict = 0; tpredict <= acsConfig.ssrOpts.prediction_duration; tpredict += acsConfig.ssrOpts.prediction_interval)
			{
				GTime	pTime	= time + tpredict;
				bool	pass	= true;

				pass = true;
				pass &= satclk(nullStream, pTime, pTime, obs, acsConfig.ssrOpts.clock_sources,							nav, &kfState);
				pass &= satpos(nullStream, pTime, pTime, obs, acsConfig.ssrOpts.ephemeris_sources, E_OffsetType::APC,	nav, &kfState);			//todo aaron, ssra streams expect common_sat_pco to be true
				if (obs.satClk == INVALID_CLOCK_VALUE)
				{
					pass = false;
				}

				//precise clock
				double	precClkVal = obs.satClk * CLIGHT;

				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(info) << Sat.id() << " failed ssrprecise prediction.\n";

					continue;
				}

				if (obs.iodeClk != obs.iodePos)
				{
					BOOST_LOG_TRIVIAL(info)
					<< Sat.id() << " IODE clock" << obs.iodeClk << " mismatch IODE orbit " << obs.iodePos << ".\n";

					continue;
				}

				Vector3d	precPos = obs.rSatApc;
				Vector3d	precVel = obs.satVel;
				double		posVar	= obs.posVar;

				//broadcast values, must come after precise values to associate the correct IODE (when SSR in use)
				pass &= satClkBroadcast(nullStream, pTime, pTime, obs, nav, obs.iodeClk);
				pass &= satPosBroadcast(nullStream, pTime, pTime, obs, nav, obs.iodePos);
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(info)
					<< Sat.id() << " failed broadcast predictions.\n";

					continue;
				}

				int			iodeClk		= obs.iodeClk;
				int			iodePos		= obs.iodePos;
// 				int			iodcrc		= ?
				Vector3d	brdcPos		= obs.rSatApc;
				Vector3d	brdcVel		= obs.satVel;
				double		brdcClkVal	= obs.satClk * CLIGHT;

				//output the valid entries
				{
					DBEntry entry;
					entry.stringMap	[SSR_DATA			]	= {SSR_CLOCK,		true};
					entry.stringMap	[SSR_SAT			]	= {Sat.id(),		true};
					entry.timeMap	[SSR_EPOCH			]	= {pTime,			true};

					entry.doubleMap	[SSR_CLOCK SSR_BRDC	]	= {brdcClkVal,		false};
					entry.doubleMap	[SSR_CLOCK SSR_PREC	]	= {precClkVal,		false};
					entry.timeMap	[SSR_UPDATED		]	= {time,			false};
					entry.intMap	[SSR_IODE			]	= {iodeClk,			false};

					dbEntryList.push_back(entry);
				}

				{
					DBEntry entry;
					entry.stringMap	[SSR_DATA			]	= {SSR_EPHEMERIS,	true};
					entry.stringMap	[SSR_SAT			]	= {Sat.id(),		true};
					entry.timeMap	[SSR_EPOCH			]	= {pTime,			true};

					entry.vectorMap	[SSR_POS SSR_BRDC	]	= {brdcPos,			false};
					entry.vectorMap	[SSR_VEL SSR_BRDC	]	= {brdcVel,			false};
					entry.vectorMap	[SSR_POS SSR_PREC	]	= {precPos,			false};
					entry.vectorMap	[SSR_VEL SSR_PREC	]	= {precVel,			false};
					entry.doubleMap	[SSR_VAR			]	= {posVar,			false};
					entry.timeMap	[SSR_UPDATED		]	= {time,			false};
					entry.intMap	[SSR_IODE			]	= {iodePos,			false};

					dbEntryList.push_back(entry);
				}
			}

			E_Source source = acsConfig.ssrOpts.code_bias_sources.front();
			switch (source)
			{
				case E_Source::PRECISE:
				{
					for (auto& obsCode : acsConfig.code_priorities[Sat.sys])
					{
						double bias = 0;
						double bvar = 0;
						bool pass = getBias(trace, time, Sat.id(), Sat, obsCode, CODE, bias, bvar);
						if (pass == false)
						{
							continue;
						}

						DBEntry entry;
						entry.stringMap	[SSR_DATA		]	= {SSR_CODE_BIAS,			true};
						entry.stringMap	[SSR_SAT		]	= {Sat.id(),				true};
						entry.timeMap	[SSR_EPOCH		]	= {time,					true};
						entry.stringMap	[SSR_OBSCODE	]	= {obsCode._to_string(),	true};

						entry.doubleMap	[SSR_BIAS		]	= {bias,					false};
						entry.doubleMap	[SSR_VAR		]	= {bvar,					false};
						entry.timeMap	[SSR_UPDATED	]	= {time,					false};

						dbEntryList.push_back(entry);
					}

					break;
				}

				case E_Source::SSR:
				{
					auto it = satNav.receivedSSR.ssrCodeBias_map.upper_bound(time);
					if (it == satNav.receivedSSR.ssrCodeBias_map.end())
					{
						break;
					}

					auto& [dummy, ssrCodeBias] = *it;
					for (auto& [obsCode, biasVar] : ssrCodeBias.obsCodeBiasMap)
					{
						DBEntry entry;
						entry.stringMap	[SSR_DATA		]	= {SSR_CODE_BIAS,			true};
						entry.stringMap	[SSR_SAT		]	= {Sat.id(),				true};
						entry.timeMap	[SSR_EPOCH		]	= {ssrCodeBias.t0,			true};
						entry.stringMap	[SSR_OBSCODE	]	= {obsCode._to_string(),	true};

						entry.doubleMap	[SSR_BIAS		]	= {biasVar.bias,			false};
						entry.doubleMap	[SSR_VAR		]	= {biasVar.var,				false};
						entry.timeMap	[SSR_UPDATED	]	= {time,					false};

						dbEntryList.push_back(entry);
					}

					break;
				}

				case E_Source::KALMAN:
				{
					for (auto& obsCode : acsConfig.code_priorities[Sat.sys])
					{
						double bias = 0;
						double bvar = 0;

						bool pass = queryBiasOutput(trace, time, kfState, ionState, Sat,"", obsCode, bias, bvar, CODE);
						if (pass == false)
						{
							continue;
						}

						DBEntry entry;
						entry.stringMap	[SSR_DATA		]	= {SSR_CODE_BIAS,			true};
						entry.stringMap	[SSR_SAT		]	= {Sat.id(),				true};
						entry.timeMap	[SSR_EPOCH		]	= {time,					true};
						entry.stringMap	[SSR_OBSCODE	]	= {obsCode._to_string(),	true};

						entry.doubleMap	[SSR_BIAS		]	= {bias,					false};
						entry.doubleMap	[SSR_VAR		]	= {bvar,					false};
						entry.timeMap	[SSR_UPDATED	]	= {time,					false};

						dbEntryList.push_back(entry);
					}

					break;
				}
			}

			source = acsConfig.ssrOpts.phase_bias_sources.front();
			switch (source)
			{
				case E_Source::PRECISE:
				{
					for (auto& obsCode : acsConfig.code_priorities[Sat.sys])
					{
						double bias = 0;
						double bvar = 0;
						bool pass = getBias(trace, time, Sat.id(), Sat, obsCode, PHAS, bias, bvar);
						if (pass == false)
						{
							continue;
						}

						DBEntry entry;
						entry.stringMap	[SSR_DATA		]	= {SSR_PHAS_BIAS,			true};
						entry.stringMap	[SSR_SAT		]	= {Sat.id(),				true};
						entry.timeMap	[SSR_EPOCH		]	= {time,					true};
						entry.stringMap	[SSR_OBSCODE	]	= {obsCode._to_string(),	true};

						entry.doubleMap	[SSR_BIAS		]	= {bias,					false};
						entry.doubleMap	[SSR_VAR		]	= {bvar,					false};
						entry.timeMap	[SSR_UPDATED	]	= {time,					false};

						entry.intMap	["dispBiasConistInd"]	= {0,					false};
						entry.intMap	["MWConistInd"      ]	= {1,					false};
						entry.doubleMap	["yawAngle"         ]	= {0,					false};			/* To Do: reflect internal yaw calculation here */
						entry.doubleMap	["yawRate"          ]	= {0,					false};			/* To Do: reflect internal yaw calculation here */
						entry.intMap	["signalIntInd"     ]	= {1,					false};
						entry.intMap	["signalWLIntInd"   ]	= {2,					false};
						entry.intMap	["signalDisconCnt"  ]	= {0,					false};

						dbEntryList.push_back(entry);
					}

					break;
				}

				case E_Source::SSR:
				{
					auto it = satNav.receivedSSR.ssrPhasBias_map.upper_bound(time);
					if (it == satNav.receivedSSR.ssrPhasBias_map.end())
					{
						break;
					}
					auto& [dummy, ssrPhasBias] = *it;

					for (auto& [obsCode, biasVar] : ssrPhasBias.obsCodeBiasMap)
					{
						DBEntry entry;
						entry.stringMap	[SSR_DATA		]	= {SSR_PHAS_BIAS,			true};
						entry.stringMap	[SSR_SAT		]	= {Sat.id(),				true};
						entry.timeMap	[SSR_EPOCH		]	= {ssrPhasBias.t0,			true};
						entry.stringMap	[SSR_OBSCODE	]	= {obsCode._to_string(),	true};

						entry.doubleMap	[SSR_BIAS		]	= {biasVar.bias,			false};
						entry.doubleMap	[SSR_VAR		]	= {biasVar.var,				false};
						entry.timeMap	[SSR_UPDATED	]	= {time,					false};

						auto& ssrPhase		= ssrPhasBias.ssrPhase;
						auto& ssrPhaseChs	= ssrPhasBias.ssrPhaseChs;
						entry.intMap	["dispBiasConistInd"]	= {ssrPhase.dispBiasConistInd,				false};
						entry.intMap	["MWConistInd"      ]	= {ssrPhase.MWConistInd,					false};
						entry.doubleMap	["yawAngle"         ]	= {ssrPhase.yawAngle,						false};
						entry.doubleMap	["yawRate"          ]	= {ssrPhase.yawRate,						false};
						entry.intMap	["signalIntInd"     ]	= {ssrPhaseChs[obsCode].signalIntInd,		false};
						entry.intMap	["signalWLIntInd"   ]	= {ssrPhaseChs[obsCode].signalWLIntInd,		false};
						entry.intMap	["signalDisconCnt"  ]	= {ssrPhaseChs[obsCode].signalDisconCnt,	false};

						dbEntryList.push_back(entry);
					}

					break;
				}

				case E_Source::KALMAN:
				{
					double bias;
					double bvar;

					for (auto& obsCode : acsConfig.code_priorities[sys])
					if (queryBiasOutput(trace, time, kfState, ionState, Sat, "", obsCode, bias, bvar, PHAS))
					{
						DBEntry entry;
						entry.stringMap	[SSR_DATA		]	= {SSR_PHAS_BIAS,			true};
						entry.stringMap	[SSR_SAT		]	= {Sat.id(),				true};
						entry.timeMap	[SSR_EPOCH		]	= {time,					true};
						entry.stringMap	[SSR_OBSCODE	]	= {obsCode._to_string(),	true};

						entry.doubleMap	[SSR_BIAS		]	= {bias,					false};
						entry.doubleMap	[SSR_VAR		]	= {bvar,					false};
						entry.timeMap	[SSR_UPDATED	]	= {time,					false};

						entry.intMap	["dispBiasConistInd"]	= {0,					false};
						entry.intMap	["MWConistInd"      ]	= {1,					false};
						entry.doubleMap	["yawAngle"         ]	= {0,					false};			/* To Do: reflect internal yaw calculation here */
						entry.doubleMap	["yawRate"          ]	= {0,					false};			/* To Do: reflect internal yaw calculation here */
						entry.intMap	["signalIntInd"     ]	= {1,					false};
						entry.intMap	["signalWLIntInd"   ]	= {2,					false};
						entry.intMap	["signalDisconCnt"  ]	= {0,					false};

						dbEntryList.push_back(entry);
					}

					break;
				}
			}
		}
	}

	E_Source atmSource = acsConfig.ssrOpts.atmosphere_sources.front();
	switch (atmSource)
	{
		case E_Source::SSR:
		case E_Source::KALMAN:
		{
			// IGS SSR Ionosphere
			bool igsIonoDetected = false;
			auto it = nav.ssrAtm.atmosGlobalMap.lower_bound(time);
			if (it != nav.ssrAtm.atmosGlobalMap.end())
			{
				auto& [t0, atmGlob] = *it;

				double dTime = fabs((atmGlob.time - time).to_double());

				if	(dTime < acsConfig.ssrInOpts.global_vtec_valid_time
					&& atmGlob.layers.empty() == false)
				{
					igsIonoDetected = true;
				}
			}

			if (igsIonoDetected)
			{
				auto it = nav.ssrAtm.atmosGlobalMap.lower_bound(time);
				auto& [t0, atmGlob] = *it;
				int nbasis = 0;
				for (auto& [hind, laydata]: atmGlob.layers)
				for (auto& [bind, basdata]: laydata.sphHarmonic)
				{
					DBEntry entry;
					entry.stringMap	[SSR_DATA		]	= {IGS_ION_ENTRY,			true};
					entry.timeMap	[SSR_EPOCH		]	= {atmGlob.time,			true};
					entry.intMap	[SSR_ION_IND	]	= {nbasis++,				true};

					entry.intMap	[IGS_ION_HGT	]	= {basdata.layer,			false};
		 			entry.intMap	[IGS_ION_DEG	]	= {basdata.degree,			false};
		 			entry.intMap	[IGS_ION_ORD	]	= {basdata.order,			false};
					entry.intMap	[IGS_ION_PAR	]	= {basdata.trigType,		false};
					entry.doubleMap	[IGS_ION_VAL	]	= {basdata.value,			false};

					dbEntryList.push_back(entry);
				}

				{
					DBEntry entry;
					entry.stringMap	[SSR_DATA		]	= {IGS_ION_META,			true};
					entry.timeMap	[SSR_EPOCH		]	= {atmGlob.time,			true};

					entry.intMap	[IGS_ION_NLAY	]	= {atmGlob.numberLayers,	false};
					entry.intMap	[IGS_ION_NBAS	]	= {nbasis,					false};
					entry.doubleMap	[IGS_ION_QLTY	]	= {atmGlob.vtecQuality,		false};
					for (auto& [layer, laydata]: atmGlob.layers)
					{
						string hghStr = "Height_"+std::to_string(layer);
						entry.doubleMap	[hghStr		]	= {laydata.height,			false};
					}
					dbEntryList.push_back(entry);
				}
			}

			// CMP SSR Atmosphere
			for (auto& [regInd, regData] : nav.ssrAtm.atmosRegionsMap)
			{
				GTime stecTime = regData.stecUpdateTime;
				map<int, SatSys> stecFound;
				int sInd = 0;
				for (auto& [sat,satData] : regData.stecData)
				{
					if (satData.find(stecTime) == satData.end())
						continue;

					stecFound[sInd] = sat;
					sInd++;
				}

				if (stecFound.empty())
					continue;

				double dTime = fabs((stecTime - time).to_double());
				if (dTime > acsConfig.ssrInOpts.local_stec_valid_time)
					continue;

				// Region Metadata
				{
					DBEntry regMT;
					regMT.stringMap	[SSR_DATA		]	= {CMP_ATM_META,				true};
					regMT.intMap	["RegionID"		]	= {regInd,						true};

					regMT.intMap	["RegionIOD"	]	= {regData.regionDefIOD,		false};
					regMT.doubleMap	["minLat"		]	= {regData.minLatDeg,			false};
					regMT.doubleMap	["maxLat"		]	= {regData.maxLatDeg,			false};
					regMT.doubleMap	["intLat"		]	= {regData.intLatDeg,			false};
					regMT.doubleMap	["minLon"		]	= {regData.minLonDeg,			false};
					regMT.doubleMap	["maxLon"		]	= {regData.maxLonDeg,			false};
					regMT.doubleMap	["intLon"		]	= {regData.intLonDeg,			false};

					regMT.intMap	["gridType"		]	= {regData.gridType,			false};
					regMT.intMap	["tropPoly"		]	= {regData.tropPolySize,		false};
					regMT.intMap	["ionoPoly"		]	= {regData.ionoPolySize,		false};
					regMT.intMap	["tropGrid"		]	= {regData.tropGrid ? 1 : 0,	false};
					regMT.intMap	["ionoGrid"		]	= {regData.ionoGrid ? 1 : 0,	false};

					regMT.doubleMap	["grdLat_0"		]	= {regData.gridLatDeg[0],		false};
					regMT.doubleMap	["grdLon_0"		]	= {regData.gridLonDeg[0],		false};

					if (regData.gridType==0)
					{
						regMT.intMap	["gridNum"	]	= {regData.gridLatDeg.size(),	false};
						string keyStr;
						for (int i = 1; i < regData.gridLatDeg.size(); i++)
						{
							keyStr = "grdLat_" + std::to_string(i);
							regMT.doubleMap	[keyStr]	= {regData.gridLatDeg[i],		false};

							keyStr = "grdLon_" + std::to_string(i);
							regMT.doubleMap	[keyStr]	= {regData.gridLonDeg[i],		false};
						}
					}
					dbEntryList.push_back(regMT);
				}

				// Troposphere entry
				{
					auto trop_ptr = regData.tropData.begin();
					if (trop_ptr != regData.tropData.end())
					{
						GTime trpTime = trop_ptr->first;
						auto& trpData = trop_ptr->second;

						DBEntry trpDT;
						trpDT.stringMap	[SSR_DATA		]	= {CMP_TRP_ENTRY,		true};
						trpDT.intMap	["RegionID"		]	= {regInd,				true};
						trpDT.timeMap	[SSR_EPOCH		]	= {trpTime,				true};

						trpDT.doubleMap	["trpAcc"		]	= {trpData.sigma,		false};
						for (int i=0; i<regData.ionoPolySize; i++)
						{
							string keyStr = "tropPoly_"+std::to_string(i);
							double polyVal = -9999;
							if (trpData.polyDry.find(i) == trpData.polyDry.end())
								polyVal = trpData.polyDry[i];
							trpDT.doubleMap	[keyStr]	= {polyVal,					false};
						}
						if (regData.tropGrid)
						for (auto& [ind,lat] : regData.gridLatDeg)
						{
							string keyStr = "tropDry_"+std::to_string(ind);
							double tropDry = -9999;
							if (trpData.gridDry.find(ind) == trpData.gridDry.end())
								tropDry = trpData.gridDry[ind];
							trpDT.doubleMap	[keyStr]	= {tropDry,					false};

							keyStr = "tropWet_"+std::to_string(ind);
							double tropWet = -9999;
							if (trpData.gridWet.find(ind) == trpData.gridWet.end())
								tropWet = trpData.gridWet[ind];
							trpDT.doubleMap	[keyStr]	= {tropWet,					false};
						}
					}
				}

				// Ionosphere entries
				{
					// Ionosphere Metadata
					DBEntry ionMT;
					ionMT.stringMap	[SSR_DATA		]	= {CMP_ION_META,			true};
					ionMT.intMap	["RegionID"		]	= {regInd,					true};
					ionMT.timeMap	[SSR_EPOCH		]	= {stecTime,				true};

					ionMT.intMap	["satNumb"		]	= {stecFound.size(),		false};
					for (auto& [satInd,sat] : stecFound)
					{
						string keyStr					= "regSat_"+std::to_string(satInd);
						ionMT.stringMap [keyStr		]	= {sat.id(),				false};

						// Ionosphere Data
						DBEntry ionDT;
						ionDT.stringMap	[SSR_DATA		]	= {CMP_ION_ENTRY,		true};
						ionDT.intMap	["RegionID"		]	= {regInd,				true};
						ionDT.stringMap [SSR_SAT		]	= {sat.id(),			true};
						ionDT.timeMap	[SSR_EPOCH		]	= {stecTime,			true};

						auto& stecData = regData.stecData[sat][stecTime];
						ionDT.doubleMap	["ionAcc"		]	= {stecData.sigma,		false};

						for (int i=0; i<regData.ionoPolySize; i++)
						{
							string keyStr = "ionoPoly_"+std::to_string(i);
							double polyVal = -9999;
							if (stecData.poly.find(i) != stecData.poly.end())
								polyVal = stecData.poly[i];
							ionDT.doubleMap	[keyStr]	= {polyVal,					false};
						}
						if (regData.ionoGrid)
						for (auto& [ind,lat] : regData.gridLatDeg)
						{
							string keyStr = "ionoGrid_"+std::to_string(ind);
							double gridVal = -9999;
							if (stecData.grid.find(ind) != stecData.grid.end())
								gridVal = stecData.grid[ind];
							ionDT.doubleMap	[keyStr]	= {gridVal,					false};
						}
						dbEntryList.push_back(ionDT);
					}
					dbEntryList.push_back(ionMT);
				}
			}
		}
	}

	mongoOutput(dbEntryList, acsConfig.mongoOpts.queue_outputs, instances,	SSR_DB);
}
