
// #pragma GCC optimize ("O0")


#include "observations.hpp"
#include "rtcmEncoder.hpp"
#include "coordinates.hpp"
#include "instrument.hpp"
#include "mongoWrite.hpp"
#include "GNSSambres.hpp"
#include "orbitProp.hpp"
#include "rtcmTrace.hpp"
#include "acsConfig.hpp"
#include "satStat.hpp"
#include "biases.hpp"
#include "common.hpp"
#include "mongo.hpp"


#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/builder/stream/array.hpp>

#include <bsoncxx/json.hpp>

using bsoncxx::builder::stream::close_array;
using bsoncxx::builder::stream::close_document;
using bsoncxx::builder::stream::document;
using bsoncxx::builder::stream::finalize;
using bsoncxx::builder::stream::open_array;
using bsoncxx::builder::stream::open_document;
using bsoncxx::builder::basic::kvp;
using bsoncxx::types::b_date;

using std::make_pair;

b_date bDate(
	GTime time)
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
	auto& mongo_ptr = localMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		
		acsConfig.localMongo.output_test_stats = false;
		
		return;
	}
	
	Instrument instrument(__FUNCTION__);

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.localMongo.database];
	mongocxx::collection		coll	= db[STATES_DB];

	mongocxx::options::bulk_write bulk_opts;
	bulk_opts.ordered(false);

	auto bulk = coll.create_bulk_write(bulk_opts);

	bool update = false;

	map<string, double> entries;
	entries["StatsSumOfSquaresPre"		] = testStatistics.sumOfSquaresPre;
	entries["StatsAverageRatioPre"		] = testStatistics.averageRatioPre;
	entries["StatsSumOfSquaresPost"		] = testStatistics.sumOfSquaresPost;
	entries["StatsAverageRatioPost"		] = testStatistics.averageRatioPost;
	entries["StatsChiSquare"			] = testStatistics.chiSq;
	entries["StatsChiSquareThreshold"	] = testStatistics.qc;
	entries["StatsDegreeOfFreedom"		] = testStatistics.dof;
	entries["StatsChiSquarePerDOF"		] = testStatistics.chiSqPerDof;
	
	for (auto& [state, value] : entries)
	{
		bsoncxx::builder::stream::document doc{};
		doc	<< "Epoch"		<< bDate(kfState.time)
			<< "Site"		<< kfState.id		+ acsConfig.localMongo.suffix
			<< "Sat"		<< ""				+ acsConfig.localMongo.suffix
			<< "State"		<< state
			<< "x"			<< value;
		
		bsoncxx::document::value doc_val = doc << finalize;
		bulk.append(mongocxx::model::insert_one(doc_val.view()));
		update = true;
	}

	if (update)
	{
		bulk.execute();
	}
}

void mongoTrace(
	string json)
{	
	auto& mongo_ptr = localMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		
		acsConfig.localMongo.output_trace = false;
		
		return;
	}
	
	Instrument instrument(__FUNCTION__);

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.localMongo.database];
	

	string collectionName = "Trace";
	
	mongocxx::collection		coll	= db[collectionName];

	mongocxx::options::bulk_write bulk_opts;
	bulk_opts.ordered(false);

	auto bulk = coll.create_bulk_write(bulk_opts);

	bool update = false;

	auto doc = bsoncxx::from_json(json);
	
	bulk.append(mongocxx::model::insert_one(doc.view()));
	
	bulk.execute();
}

void mongoOutputConfig(
	string& config)
{
	auto& mongo_ptr = localMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		
		return;
	}
	
	Instrument instrument(__FUNCTION__);

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.localMongo.database];
	

	string collectionName = "Config";
	
	mongocxx::collection		coll	= db[collectionName];

	mongocxx::options::bulk_write bulk_opts;
	bulk_opts.ordered(false);

	auto bulk = coll.create_bulk_write(bulk_opts);

	bool update = false;
	
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

void mongoMeasSatStat(
	StationMap& stationMap)
{	
	auto& mongo_ptr = localMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		
		acsConfig.localMongo.output_measurements = false;
		
		return;
	}
	
	Instrument instrument(__FUNCTION__);

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.localMongo.database];
	

	string collectionName = "Geometry";
	
	mongocxx::collection		coll	= db[collectionName];

	mongocxx::options::bulk_write bulk_opts;
	bulk_opts.ordered(false);

	auto bulk = coll.create_bulk_write(bulk_opts);

	bool update = false;

	for (auto& [id, rec] : stationMap)
	for (auto& obs : only<GObs>(rec.obsList))
	{
		if (obs.exclude)
			continue;

		if (obs.satStat_ptr == nullptr)
			continue;

		SatStat& satStat = *obs.satStat_ptr;

		bsoncxx::builder::stream::document doc{};
		doc		<< "Epoch"		<< bDate(tsync)
				<< "Site"		<< obs.mount	
				<< "Sat"		<< obs.Sat.id()
				<< "Series"		<< acsConfig.localMongo.suffix
				<< "Azimuth"	<< satStat.az		* R2D		
				<< "Elevation"	<< satStat.el		* R2D		
				<< "Nadir"		<< satStat.nadir	* R2D;

		bsoncxx::document::value doc_val = doc << finalize;
		bulk.append(mongocxx::model::insert_one(doc_val.view()));
		update = true;
	}

	if (update)
		bulk.execute();
}



void mongoMeasResiduals(
	GTime				time,
	KFMeas&				kfMeas,
	string				suffix,
	int					beg,
	int					num)
{
	Instrument instrument(__FUNCTION__);
	
	for (auto mongo_ptr_ptr : {&localMongo_ptr, &remoteMongo_ptr})
	{
		auto& mongo_ptr = *mongo_ptr_ptr;
		
		MongoOptions*	config_ptr;
		if (mongo_ptr_ptr == &localMongo_ptr)		config_ptr = &acsConfig.localMongo;
		else										config_ptr = &acsConfig.remoteMongo;
		
		auto& config = *config_ptr;
		
		if (config.output_measurements == false)
		{
			continue;
		}
	
		if (mongo_ptr == nullptr)
		{
			MONGO_NOT_INITIALISED_MESSAGE;
			
			config.output_measurements = false;
			
			continue;
		}
			
		Mongo& mongo = *mongo_ptr;

		auto 						c		= mongo.pool.acquire();
		mongocxx::client&			client	= *c;
		mongocxx::database			db		= client[config.database];

		
		string collectionName = "Measurements";
		
		mongocxx::collection		coll	= db[collectionName];

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
			
			string commentString = "";
			if (obsKey.comment.empty() == false)
				commentString = obsKey.comment + "-";
			
			string name = commentString + std::to_string(obsKey.num);
			
			lookup[make_tuple(obsKey.str, obsKey.Sat.id())].push_back(i);
		}

		for (auto& [description, index] : lookup)
		{
			bsoncxx::builder::stream::document doc{};
			auto& [site, sat] = description;
			doc		<< "Epoch"		<< bDate(time)
					<< "Site"		<< site
					<< "Sat"		<< sat
					<< "Series"		<< config.suffix + suffix;
					
			indexSeries	[config.suffix + suffix]	= true;
			indexSite	[site]						= true;
			indexSat	[sat]						= true;
			
			for (int& i : index)
			{
				KFKey& obsKey = kfMeas.obsKeys[i];
			
				string commentString = "";
				if (obsKey.comment.empty() == false)
					commentString = obsKey.comment + "-";
				
				string name = commentString + std::to_string(obsKey.num);
				
				doc << name + "-Prefit"		<<			kfMeas.V	(i)	
					<< name + "-Postfit"	<<			kfMeas.VV	(i)	
					<< name + "-Sigma"		<<	sqrt(	kfMeas.R	(i,i));
					
				indexLabel[name + "-Prefit"]	= true;
				indexLabel[name + "-Postfit"]	= true;
				indexLabel[name + "-Sigma"]		= true;
				
				if	( config.output_components == false
					||kfMeas.componentLists.empty())
				{
					continue;
				}
				
				auto& componentList = kfMeas.componentLists[i];
				
				for (auto& component : componentList)
				{
					auto& [comp, value, desc, var] = component;
				
					string label = name + " " + KF::_from_integral_unchecked(obsKey.type)._to_string() + " " + comp._to_string();
					
					doc << label << value;
					
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
				
				auto findDoc	= document{}									<< "type"	<< name							<< finalize;
				auto updateDoc	= document{} << "$addToSet" << open_document	<< "Values" << eachDoc << close_document	<< finalize;
				
				db["Content"].update_one(findDoc.view(), updateDoc.view(), options);
			};
			
			addIndices("Measurements",	indexLabel);
			addIndices("Series",		indexSeries);
			addIndices("Site",			indexSite);
			addIndices("Sat",			indexSat);
		}
	}
}

void mongoStates(
	KFState&			kfState,
	string				suffix)
{
	Instrument instrument(__FUNCTION__);
	
	for (auto mongo_ptr_ptr : {&localMongo_ptr, &remoteMongo_ptr})
	{
		auto& mongo_ptr = *mongo_ptr_ptr;
		
		MongoOptions*	config_ptr;
		if (mongo_ptr_ptr == &localMongo_ptr)		config_ptr = &acsConfig.localMongo;
		else										config_ptr = &acsConfig.remoteMongo;
		
		auto& config = *config_ptr;
		
		if (config.output_states == false)
		{
			continue;
		}
		
		if (mongo_ptr == nullptr)
		{
			MONGO_NOT_INITIALISED_MESSAGE;
			
			config.output_states = false;
			
			return;
		}
		
		Instrument instrument(__FUNCTION__);
		
		Mongo& mongo = *mongo_ptr;

		auto 						c		= mongo.pool.acquire();
		mongocxx::client&			client	= *c;
		mongocxx::database			db		= client[config.database];
		mongocxx::collection		coll	= db[STATES_DB];

		mongocxx::options::update	options;
		options.upsert(true);
		
		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);
		
		auto bulk = coll.create_bulk_write(bulk_opts);

		map<tuple<string, string, string>, vector<pair<int, int>>> lookup;
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
			
			lookup[make_tuple(key.str, key.Sat.id(), KF::_from_integral_unchecked(key.type)._to_string())].push_back(make_pair(index, key.num));
		}

		vector<bsoncxx::document::value> documents;

		bool update = false;
		
		for (auto &[description, index] : lookup)
		{
			auto &[site, sat, state] = description;

			bsoncxx::builder::stream::document doc{};
			doc		<< "Epoch"		<< bDate(kfState.time)
					<< "Site"		<< site
					<< "Sat"		<< sat
					<< "State"		<< state
					<< "Series"		<< config.suffix + suffix;

			indexSeries	[config.suffix + suffix]	= true;
			indexState	[state]						= true;
			indexSat	[sat]						= true;
			indexSite	[site]						= true;

			auto	array_builder = doc << "x"		<< open_array;	for (auto &[i, num]: index)		array_builder << 		kfState.x	(i);	array_builder << close_array;
					array_builder = doc << "dx"		<< open_array;	for (auto &[i, num]: index)		array_builder << 		kfState.dx	(i);	array_builder << close_array;
					array_builder = doc << "sigma"	<< open_array;	for (auto &[i, num]: index)		array_builder << sqrt(	kfState.P	(i,i));	array_builder << close_array;
					array_builder = doc << "Num"	<< open_array;	for (auto &[i, num]: index)		array_builder << num;						array_builder << close_array;
			
			bsoncxx::document::value doc_val = doc << finalize;
			
			bulk.append(mongocxx::model::insert_one(doc_val.view()));	
			
			update = true;
		}
		
		if (update)
		{
			bulk.execute();

			auto addIndices = [&](string name, map<string, bool> index)
			{
				auto eachDoc = document{};
				
				auto arrayDoc = eachDoc << "$each" << open_array;
				for (auto &[indexStr, unused]: index)
				{
					arrayDoc << indexStr;
				}
				arrayDoc << close_array;
				
				auto findDoc	= document{}									<< "type"	<< name							<< finalize;
				auto updateDoc	= document{} << "$addToSet" << open_document	<< "Values" << eachDoc << close_document	<< finalize;
				
				db["Content"].update_one(findDoc.view(), updateDoc.view(), options);
			};
			
			addIndices("Series",indexSeries);
			addIndices("State",	indexState);
			addIndices("Site",	indexSite);
			addIndices("Sat",	indexSat);
		}
	}
}


void mongoCull(
	GTime time)
{
	for (auto mongo_ptr_ptr : {&localMongo_ptr, &remoteMongo_ptr})
	{
		auto& mongo_ptr = *mongo_ptr_ptr;
		
		MongoOptions*	config_ptr;
		if (mongo_ptr_ptr == &localMongo_ptr)		config_ptr = &acsConfig.localMongo;
		else										config_ptr = &acsConfig.remoteMongo;
		
		auto& config = *config_ptr;
		
		if (config.cull_history == false)
		{
			continue;
		}
		
		if (mongo_ptr == nullptr)
		{
			MONGO_NOT_INITIALISED_MESSAGE;
			return;
		}

		Mongo& mongo = *mongo_ptr;

		auto 						c		= mongo.pool.acquire();
		mongocxx::client&			client	= *c;
		mongocxx::database			db		= client[config.database];
		
		for (auto collection: {SSR_DB, REMOTE_DATA_DB})
		{
			mongocxx::collection		coll	= db[collection];

			using bsoncxx::builder::basic::kvp;
			
			b_date btime{std::chrono::system_clock::from_time_t((time_t)((PTime)(time - config.min_cull_age)).bigTime)};
			
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
	MongoOptions&		config,
	string				collection)
{	
	Mongo* mongo_ptr;
	
	if (config.local)		mongo_ptr = localMongo_ptr;
	else					mongo_ptr = remoteMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[config.database];
	mongocxx::collection		coll	= db[collection];

	mongocxx::options::update	options;
	options.upsert(true);

	mongocxx::options::bulk_write bulk_opts;
	bulk_opts.ordered(false);

	auto bulk = coll.create_bulk_write(bulk_opts);

	bool update = false;

	using bsoncxx::builder::basic::kvp;
	
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

/** Write states generating SSR corrections to Mongo DB
*/
void	prepareSsrStates(
	Trace&				trace,			///< Trace to output to
	KFState&			kfState,		///< Filter object to extract state elements from
	GTime 				time)			///< Time of current epoch
{
	if	( acsConfig.localMongo.	output_ssr_precursors == false
		&&acsConfig.remoteMongo.output_ssr_precursors == false)
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
				pass &= satpos(nullStream, pTime, pTime, obs, acsConfig.ssrOpts.ephemeris_sources, E_OffsetType::APC,	nav, &kfState);
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
						bool pass = queryBiasOutput(trace, time, kfState, Sat,"", obsCode, bias, bvar, CODE);
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
					if (queryBiasOutput(trace, time, kfState, Sat, "", obsCode, bias, bvar, PHAS))
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
	
	// IGS SSR Ionosphere
	E_Source source = acsConfig.ssrOpts.ionosphere_sources.front();
	switch (source)
	{
		case E_Source::SSR:
		case E_Source::KALMAN:
		{
			auto it = nav.ssrAtm.atmosGlobalMap.lower_bound(time);
			if (it == nav.ssrAtm.atmosGlobalMap.end())
				break;
			
			auto& [t0, atmGlob] = *it;
			
			if (fabs((atmGlob.time - time).to_double()) > acsConfig.ssrInOpts.global_vtec_valid_time)
				break;
			
			if (atmGlob.layers.size()==0)
				break;
			
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
	}
	
	if (acsConfig.localMongo.	output_ssr_precursors)		mongoOutput(dbEntryList, acsConfig.localMongo,	SSR_DB);
	if (acsConfig.remoteMongo.	output_ssr_precursors)		mongoOutput(dbEntryList, acsConfig.remoteMongo,	SSR_DB);
}

void	outputMongoPredictions(
	Trace&			trace,		///< Trace to output to
	Orbits&			orbits,		///< Filter object to extract state elements from
	GTime 			time,		///< Time of current epoch
	MongoOptions&	config)		///< Set of options for the mongo instance to be used
{
	vector<DBEntry>	dbEntryList;

 	time.bigTime = (long int) (time.bigTime + 0.5);	// time tags in mongo will be rounded up to whole sec
	
	for (auto& orbit : orbits)
	{	
		if	( orbit.pos.isZero()
			&&orbit.vel.isZero())
		{
			continue;
		}
		
		DBEntry entry;
		entry.stringMap	[REMOTE_DATA		]	= {REMOTE_ORBIT,		true};
		entry.stringMap	[REMOTE_SAT			]	= {orbit.Sat.id(),		true};
		entry.timeMap	[REMOTE_EPOCH		]	= {time,				true};
		
		entry.timeMap	[REMOTE_UPDATED		]	= {tsync,				false};
		entry.vectorMap	[REMOTE_POS			]	= {orbit.pos,			false};
		entry.vectorMap	[REMOTE_VEL			]	= {orbit.vel,			false};
		entry.doubleMap	[REMOTE_VAR			]	= {orbit.posVar,		false};
		
		dbEntryList.push_back(entry);
	}
	
	BOOST_LOG_TRIVIAL(debug)
	<< "Writing to mongo\n";
	
	mongoOutput(dbEntryList, config, REMOTE_DATA_DB);
}

