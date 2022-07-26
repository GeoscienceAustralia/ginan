
// #pragma GCC optimize ("O0")


#include "observations.hpp"
#include "rtcmEncoder.hpp"
#include "instrument.hpp"
#include "mongoWrite.hpp"
#include "GNSSambres.hpp"
#include "biasSINEX.hpp"
#include "acsConfig.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "mongo.hpp"


#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/json.hpp>

using bsoncxx::builder::basic::kvp;

void mongoTestStat(
	KFState&		kfState,
	TestStatistics&	testStatistics)
{
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		
		acsConfig.output_mongo_test_stats = false;
		
		return;
	}
	
	Instrument instrument(__FUNCTION__);

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.mongo_database];
	mongocxx::collection		coll	= db	["States"];

	mongocxx::options::update	options;
	options.upsert(true);

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
		mongocxx::model::update_one  mongo_req(
			document{}
				<< "Epoch"		<< bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(kfState.time.time)}
				<< "Site"		<< kfState.id		+ acsConfig.mongo_suffix
				<< "Sat"		<< ""				+ acsConfig.mongo_suffix
				<< "State"		<< state
				<< finalize,

			document{}
				<< "$set"
				<< open_document
					<< "x0"		<< value
				<< close_document
				<< finalize
			);
		
		mongo_req.upsert(true);
		bulk.append(mongo_req);
		update = true;
	}

	if (update)
	{
		bulk.execute();
	}
}

void mongoMeasSatStat(
	StationMap& stationMap)
{	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		
		acsConfig.output_mongo_measurements = false;
		
		return;
	}
	
	Instrument instrument(__FUNCTION__);

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.mongo_database];
	
	for (auto pseudoIndex : {false, true})
	{
		string collectionName = "Measurements";
		if (pseudoIndex)		
			collectionName += "Index";
		
		mongocxx::collection		coll	= db[collectionName];

		mongocxx::options::update	options;
		options.upsert(true);

		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);

		auto bulk = coll.create_bulk_write(bulk_opts);

		bool update = false;

		for (auto& [id, rec] : stationMap)
		for (auto& obs : rec.obsList)
		{
			if (obs.exclude)
				continue;

			if (obs.satStat_ptr == nullptr)
				continue;

			SatStat& satStat = *obs.satStat_ptr;

			bsoncxx::builder::basic::document keyDoc = {};
			if (pseudoIndex == false)
			{
				//only add epoch data to full collection, not the pseudoIndex subset
				keyDoc.append(kvp("Epoch",	bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(tsync.time)}	));
			}
			
			{
				keyDoc.append(kvp("Site",	obs.mount	+ acsConfig.mongo_suffix							));
				keyDoc.append(kvp("Sat",	obs.Sat.id()													));
			}
			
			bsoncxx::builder::basic::document valDoc = {};
			valDoc.append(kvp("Azimuth",		pseudoIndex ? true : satStat.az		* R2D		));
			valDoc.append(kvp("Elevation",		pseudoIndex ? true : satStat.el		* R2D		));
			valDoc.append(kvp("Nadir",			pseudoIndex ? true : satStat.nadir	* R2D		));
			valDoc.append(kvp("sunDotSat",		pseudoIndex ? true : satStat.sunDotSat			));
			valDoc.append(kvp("sunCrossSat",	pseudoIndex ? true : satStat.sunCrossSat		));
			valDoc.append(kvp("slip",			pseudoIndex ? true : satStat.slip				));
			valDoc.append(kvp("Phase Windup",	pseudoIndex ? true : satStat.phw				));
			
			mongocxx::model::update_one  mongo_req(
				keyDoc.extract(),

				document{}
					<< "$set" << valDoc
					<< finalize
				);

			mongo_req.upsert(true);
			bulk.append(mongo_req);
			update = true;
		}

		if (update)
			bulk.execute();
	}
}


void mongoMeasResiduals(
	GTime				time,
	vector<ObsKey>&		obsKeys,
	VectorXd&			prefits,
	VectorXd&			postfits,
	MatrixXd&			variance,
	string				suffix,
	int					beg,
	int					num)
{
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		
		acsConfig.output_mongo_measurements = false;
		
		return;
	}
	
	Instrument instrument(__FUNCTION__);

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.mongo_database];
	
	for (auto pseudoIndex : {false, true})
	{
		string collectionName = "Measurements";
		if (pseudoIndex)		
			collectionName += "Index";
		
		mongocxx::collection		coll	= db[collectionName];

		mongocxx::options::update	options;
		options.upsert(true);

		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);

		auto bulk = coll.create_bulk_write(bulk_opts);

		bool update = false;

		if (num < 0)
		{
			num = prefits.rows();
		}
		
		options.upsert(true);
		for (int i = beg; i < beg + num; i++)
		{
			ObsKey& obsKey = obsKeys[i];

			string name = obsKey.type + std::to_string(obsKey.num);

			bsoncxx::builder::basic::document keyDoc = {};
			if (pseudoIndex == false)
			{
				//only add epoch data to full collection, not the pseudoIndex subset
				keyDoc.append(kvp("Epoch",	bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(time.time)}	));
			}
			
			{
				keyDoc.append(kvp("Site",	obsKey.str	+ acsConfig.mongo_suffix	+ suffix							));
				keyDoc.append(kvp("Sat",	obsKey.Sat.id()							+ suffix							));
			}
			
			bsoncxx::builder::basic::document valDoc = {};
			valDoc.append(kvp(name + "-Prefit",		pseudoIndex ? true : prefits(i)		));
			valDoc.append(kvp(name + "-Postfit",	pseudoIndex ? true : postfits(i)	));
			valDoc.append(kvp(name + "-Variance",	pseudoIndex ? true : variance(i,i)	));
			
			mongocxx::model::update_one  mongo_req(
				keyDoc.extract(),

				document{}
					<< "$set" << valDoc
					<< finalize
				);

			mongo_req.upsert(true);
			
			bulk.append(mongo_req);
			update = true;
		}

		if (update)
		{
			bulk.execute();
		}
	}
}

void mongoMeasComponents(
	KFMeas&		kfMeas)
{
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		
		acsConfig.output_mongo_measurements = false;
		
		return;
	}
	
	Instrument instrument(__FUNCTION__);
	
	Mongo& mongo = *mongo_ptr;
	
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.mongo_database];
	
	for (auto pseudoIndex : {false, true})
	{
		string collectionName = "Measurements";
		if (pseudoIndex)		
			collectionName += "Index";
		
		mongocxx::collection		coll	= db[collectionName];
	
		mongocxx::options::update	options;
		options.upsert(true);
		
		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);
		
		auto bulk = coll.create_bulk_write(bulk_opts);
		
		bool update = false;
		
		options.upsert(true);
		for (int i = 0; i < kfMeas.obsKeys.size(); i++)
		{
			ObsKey&	obsKey			= kfMeas.obsKeys[i];
			auto&	componentList	= kfMeas.componentLists[i];
			
			string name = obsKey.type + std::to_string(obsKey.num);

			bsoncxx::builder::basic::document keyDoc = {};
			if (pseudoIndex == false)
			{
				//only add epoch data to full collection, not the pseudoIndex subset
				keyDoc.append(kvp("Epoch",	bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(tsync.time)}	));
			}
			
			{
				keyDoc.append(kvp("Site",	obsKey.str	+ acsConfig.mongo_suffix							));
				keyDoc.append(kvp("Sat",	obsKey.Sat.id()													));
			}
			
			bsoncxx::builder::basic::document valDoc = {};
			for (auto& component : componentList)
			{
				auto& [comp, value, desc] = component;
			
				valDoc.append(kvp(name + "-" + comp,		pseudoIndex ? true : value		));
			}
			
			mongocxx::model::update_one  mongo_req(
				keyDoc.extract(),

				document{}
					<< "$set" << valDoc
					<< finalize
				);
			
			mongo_req.upsert(true);
			bulk.append(mongo_req);
			update = true;
		}
		
		if (update)
		{
			bulk.execute();
		}
	}
}

void mongoStates(
	KFState&			kfState,
	string				suffix)
{
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		
		acsConfig.output_mongo_states = false;
		
		return;
	}

	Instrument instrument(__FUNCTION__);
	
	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.mongo_database];
	
	for (auto pseudoIndex : {false, true})
	{
		string collectionName = "States";
		if (pseudoIndex)		
			collectionName += "Index";
		
		mongocxx::collection		coll	= db[collectionName];

		mongocxx::options::update	options;
		options.upsert(true);

		mongocxx::options::bulk_write bulk_opts;
		bulk_opts.ordered(false);

		auto bulk = coll.create_bulk_write(bulk_opts);

		bool update = false;

		for (auto& [key, index] : kfState.kfIndexMap)
		{
			if (key.type == KF::ONE)
			{
				continue;
			}

			bsoncxx::builder::basic::document keyDoc = {};
			if (pseudoIndex == false)
			{
				//only add epoch data to full collection, not the index subset
				keyDoc.append(kvp("Epoch",	bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(kfState.time.time)}	));
			}
			
			{
				keyDoc.append(kvp("Site",	key.str			+ acsConfig.mongo_suffix + suffix		));
				keyDoc.append(kvp("Sat",	key.Sat.id()	+ acsConfig.mongo_suffix + suffix		));
				keyDoc.append(kvp("State",	KF::_from_integral_unchecked(key.type)._to_string()		));
			}
			
			bsoncxx::builder::basic::document valDoc = {};
			valDoc.append(kvp("x"	+ std::to_string(key.num),		pseudoIndex ? true : kfState.x	(index)			));
			valDoc.append(kvp("dx"	+ std::to_string(key.num),		pseudoIndex ? true : kfState.dx	(index)			));
			valDoc.append(kvp("P"	+ std::to_string(key.num),		pseudoIndex ? true : kfState.P	(index,index)	));
			
			string setType;
			
			if (pseudoIndex)	setType = "$setOnInsert";
			else				setType = "$set";
			
			bsoncxx::builder::basic::document valThing = {};
			valThing.append(kvp(setType,		valDoc));
			
			mongocxx::model::update_one  mongo_req(
				keyDoc	.extract(),
				valThing.extract());
			
			update = true;

			mongo_req.upsert(true);
			bulk.append(mongo_req);
			
			
		}

		if (update)
		{
			Instrument instrument("execution");
			bulk.execute();
		}
	}
}

void mongoCull(
	GTime cullTime)
{
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.mongo_database];
	mongocxx::collection		coll	= db[SSR_DB];

    using bsoncxx::builder::basic::kvp;
	
	bsoncxx::types::b_date btime{std::chrono::system_clock::from_time_t(cullTime.time)};
	
    // Remove all documents that match a condition.
	auto docSys		= document{}	<< SSR_EPOCH	
										<< open_document
										<< "$lt" << btime
										<< close_document 
									<< finalize;
									
    coll.delete_many(docSys.view());
}

void mongoOutput(
	list<DBEntry>& dbEntryList)
{	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.mongo_database];
	mongocxx::collection		coll	= db[SSR_DB];

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
		bsoncxx::builder::basic::document keys = {};
		bsoncxx::builder::basic::document vals = {};
		bsoncxx::builder::basic::document Vals = {};
		
		for (bool key : {false, true})
		{
			auto* ptr = &keys;
			if (key)	ptr = &keys;
			else		ptr = &vals;
			auto& doc = *ptr;
			
			for (auto&[k,v]:entry.stringMap)	{auto& [e,b] = v; if (key == b)							doc.append(kvp(k,						e));}
			for (auto&[k,v]:entry.intMap)		{auto& [e,b] = v; if (key == b)							doc.append(kvp(k,						e));}
			for (auto&[k,v]:entry.doubleMap)	{auto& [e,b] = v; if (key == b)							doc.append(kvp(k,						e));}
			for (auto&[k,v]:entry.timeMap)		{auto& [e,b] = v; if (key == b)							doc.append(kvp(k, bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(e.time)}));}
			for (auto&[k,v]:entry.vectorMap)	{auto& [e,b] = v; if (key == b) for (int i=0;i<3;i++)	doc.append(kvp(k + std::to_string(i),	e[i]));}
		}
		
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

void	prepareSsrStates(
	Trace&				trace,			///< Trace to output to
	KFState&			kfState,		///< Filter object to extract state elements from
	GTime 				time)			///< Time of current epoch
{
	list<DBEntry>	dbEntryList;
	
	for (int i = 0; i < E_Sys::_size(); i++)
	{
		E_Sys	sys			= E_Sys::_values()[i];
		string	sysName		= E_Sys::_names() [i];		boost::algorithm::to_lower(sysName);

		if (acsConfig.process_sys[sys] == false)
			continue;
		
		auto sats = getSysSats(sys);
		
		// Calculate orbits + clocks at tsync & tsync+udi
		for (auto& Sat : sats)
		{
			// Create a dummy observation
			Obs obs;
			obs.Sat = Sat;
			
			auto& satNav = nav.satNavMap[obs.Sat];
			SatStat satStatDummy;
			
			obs.satStat_ptr	= &satStatDummy;
			obs.satNav_ptr	= &satNav; // for satpos_ssr()

			GTime teph = time;
			
			// Clock and orbit corrections (and predicted corrections)
			for (int tpredict = 0; tpredict <= acsConfig.ssrOpts.prediction_duration; tpredict += acsConfig.ssrOpts.prediction_interval)
			{
				GTime	pTime	= time + tpredict;
				bool	pass	= false;
				
				//broadcast values
				
				pass = satpos(trace, pTime, pTime, obs, E_Ephemeris::BROADCAST,	E_OffsetType::APC, nav, false);
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(info) 
					<< Sat.id() << " failed broadcast predictions.\n";
					
					continue;
				}
				
				int			iode		= obs.iode;
				Vector3d	brdcPos		= obs.rSat;
				Vector3d	brdcVel		= obs.satVel;
				double		brdcClkVal	= obs.dtSat[0] * CLIGHT;
				
				//precise clock
				double	precClkVal	= 0;
				switch (acsConfig.ssrOpts.clock_source)
				{
					case E_Ephemeris::BROADCAST:
					case E_Ephemeris::PRECISE:
					case E_Ephemeris::SSR:
					{
						pass = satpos(trace, pTime, pTime, obs, acsConfig.ssrOpts.clock_source,	E_OffsetType::APC, nav, false);
						
						precClkVal = obs.dtSat[0] * CLIGHT;
						
						break;
					}
					case E_Ephemeris::KALMAN:
					{
						//currently can't predict clocks
						if (tpredict == 0)
						{
							pass = kfState.getKFValue({.type = KF::SAT_CLOCK, .Sat = Sat}, precClkVal);
						}
						
						break;
					}
				}
				
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(info) 
					<< Sat.id() << " failed ssrClk precise prediction.\n";
					
					continue;
				}
				
				//precise orbit
				switch (acsConfig.ssrOpts.ephemeris_source)
				{
					case E_Ephemeris::BROADCAST:
					case E_Ephemeris::PRECISE:
					case E_Ephemeris::SSR:
					{
						pass = satpos(trace, pTime, pTime, obs, acsConfig.ssrOpts.ephemeris_source,	E_OffsetType::APC, nav, false);
						
						break;
					}
				}
				
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(info) 
					<< Sat.id() << " failed ssrEph precise prediction.\n";
					break;
				}
			
				Vector3d precPos = obs.rSat;
				Vector3d precVel = obs.satVel;
				
				
				//output the valid entries
				{
					DBEntry entry;
					entry.stringMap	[SSR_DATA			]	= {SSR_CLOCK,		true};
					entry.stringMap	[SSR_SAT			]	= {Sat.id(),		true};
					entry.timeMap	[SSR_EPOCH			]	= {pTime,			true};
					
					entry.doubleMap	[SSR_CLOCK SSR_BRDC	]	= {brdcClkVal,		false};
					entry.doubleMap	[SSR_CLOCK SSR_PREC	]	= {precClkVal,		false};
					entry.timeMap	[SSR_UPDATED		]	= {time,			false};
					entry.intMap	[SSR_IODE			]	= {iode,			false};
					
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
					entry.timeMap	[SSR_UPDATED		]	= {time,			false};
					entry.intMap	[SSR_IODE			]	= {iode,			false};
				
					dbEntryList.push_back(entry);
				}
			}
			
			switch (acsConfig.ssrOpts.code_bias_source)
			{
				case E_Ephemeris::PRECISE:
				{
					for (auto& obsCode : acsConfig.code_priorities[Sat.sys])
					{
						double bias = 0;
						double bvar = 0;
						bool pass = getBiasSinex(trace, time, Sat.id(), Sat, obsCode, CODE, bias, bvar);
						if (pass == false)
						{
							continue;
						}
						
						DBEntry entry;
						entry.stringMap	[SSR_DATA		]	= {SSR_CODE_BIAS,			true};
						entry.stringMap	[SSR_SAT		]	= {Sat.id(),				true};
						entry.timeMap	[SSR_EPOCH		]	= {time,					true};
						entry.stringMap	[SSR_OBSCODE	]	= {obsCode._to_string(),	true};

						entry.doubleMap	[SSR_BIAS		]	= {-bias,					false};
						entry.doubleMap	[SSR_VAR		]	= {bvar,					false};
						entry.timeMap	[SSR_UPDATED	]	= {time,					false};
					
						dbEntryList.push_back(entry);
					}
					
					break;
				}
				case E_Ephemeris::SSR:
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
			}
			
			switch (acsConfig.ssrOpts.phase_bias_source)
			{
				case E_Ephemeris::SSR:
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
						entry.intMap	["signalWidIntInd"  ]	= {ssrPhaseChs[obsCode].signalWidIntInd,	false};
						entry.intMap	["signalDisconCnt"  ]	= {ssrPhaseChs[obsCode].signalDisconCnt,	false};
					
						dbEntryList.push_back(entry);
					}
					
					break;
				}
				
				case E_Ephemeris::KALMAN:
				{
					double bias;
					double bvar;
				
					if (queryBiasOutput(trace, Sat, E_AmbTyp::UCL1, bias, bvar))
					{
						E_ObsCode obsCode = acsConfig.clock_codesL1[sys];
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
						entry.intMap	["signalWidIntInd"  ]	= {2,					false};
						entry.intMap	["signalDisconCnt"  ]	= {0,					false};
						
						dbEntryList.push_back(entry);
					}
				
					if (queryBiasOutput(trace, Sat, E_AmbTyp::UCL2, bias, bvar))
					{
						E_ObsCode obsCode = acsConfig.clock_codesL2[sys];
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
						entry.intMap	["signalWidIntInd"  ]	= {2,					false};
						entry.intMap	["signalDisconCnt"  ]	= {0,					false};
						
						dbEntryList.push_back(entry);
					}
					
					break;
				}
			}
		}
	}
	
	if (acsConfig.output_mongo_rtcm_messages)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "Writing to mongo\n";
		
		mongoCull(time - 300.0);
		mongoOutput(dbEntryList);
	}
}

