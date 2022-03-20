
// #pragma GCC optimize ("O0")

#ifdef ENABLE_MONGODB

#include "observations.hpp"
#include "rtcmEncoder.hpp"
#include "mongoWrite.hpp"
#include "acsConfig.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "mongo.hpp"


void mongoTestStat(
	KFState&			kfState,
	double				prefitSumOfSqTestStat,
	double				postfitSumOfSqTestStat,
	double				chiSq,
	double				qc,
	int					dof,
	double				chiSqPerDof)
{
	if (mongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.mongo_database];
	mongocxx::collection		coll	= db	["TestStatistics"];

	mongocxx::options::update	options;
	options.upsert(true);

	mongocxx::options::bulk_write bulk_opts;
	bulk_opts.ordered(false);

	auto bulk = coll.create_bulk_write(bulk_opts);

	bool update = false;

	{
		mongocxx::model::update_one  mongo_req(
			document{}
				<< "Epoch"			<< bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(tsync.time)}
				<< finalize,

			document{}
				<< "$set"
				<< open_document
					<< "prefitSumOfSqTestStat"	<< prefitSumOfSqTestStat
					<< "postfitSumOfSqTestStat"	<< postfitSumOfSqTestStat
					<< "ChiSqaureMode"			<< kfState.chi_square_mode._to_string()
					<< "ChiSquare"				<< chiSq
					<< "ChiSquareThreshold"		<< qc
					<< "DegreeOfFreedom"		<< dof
					<< "ChiSquarePerDOF"		<< chiSqPerDof
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

void mongoMeasSatStat_all(
	StationMap& stationMap)
{	
	if (mongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.config_description];
	mongocxx::collection		coll	= db["Measurements"];

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

		mongocxx::model::update_one  mongo_req{
			document{}
				<< "Epoch"			<< bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(tsync.time)}
				<< "Site"			<< obs.mount + acsConfig.mongo_suffix
				<< "Sat"			<< obs.Sat.id()
				<< finalize,

			document()
				<< "$set"
				<< open_document
					<< "Azimuth"	<< satStat.az * R2D
					<< "Elevation"	<< satStat.el * R2D
				<< close_document
				<< finalize
			};

		mongo_req.upsert(true);
		bulk.append(mongo_req);
		update = true;
	}

	if (update)
		bulk.execute();
}


void mongoMeasResiduals(
	vector<ObsKey>		obsKeys,
	VectorXd&			prefits,
	VectorXd&			postfits,
	MatrixXd&			variance,
	int					beg,
	int					num)
{
	if (mongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.mongo_database];
	mongocxx::collection		coll	= db["Measurements"];

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

		mongocxx::model::update_one  mongo_req(
			document{}
				<< "Epoch"		<< bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(tsync.time)}
				<< "Site"		<< obsKey.str	+ acsConfig.mongo_suffix
				<< "Sat"		<< obsKey.Sat.id()
				<< finalize,

			document{}
				<< "$set"
				<< open_document
					<< name + "-Prefit"		<< prefits(i)
					<< name + "-Postfit"	<< postfits(i)
					<< name + "-Variance"	<< variance(i,i)
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

void mongoStates(
	KFState&			kfState,
	string				suffix)
{
	if (mongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.config_description];
	mongocxx::collection		coll	= db["States"];

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

		mongocxx::model::update_one  mongo_req(
			document{}
				<< "Epoch"		<< bsoncxx::types::b_date {std::chrono::system_clock::from_time_t(kfState.time.time)}
				<< "Site"		<< key.str		+ acsConfig.mongo_suffix + suffix
				<< "Sat"		<< key.Sat.id()	+ acsConfig.mongo_suffix + suffix
				<< "State"		<< KF::_from_integral_unchecked(key.type)._to_string()
				<< finalize,

			document{}
				<< "$set"
				<< open_document
					<< "x" + std::to_string(key.num)		<< kfState.x(index)
					<< "P" + std::to_string(key.num)		<< kfState.P(index,index)
				<< close_document
				<< finalize
			);

		update = true;

		mongo_req.upsert(true);
		bulk.append(mongo_req);
	}

	if (update)
	{
		bulk.execute();
	}
}

void mongoCull(
	GTime cullTime)
{
	if (mongo_ptr == nullptr)
	{
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.config_description];
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
		return;
	}

	Mongo& mongo = *mongo_ptr;

	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.config_description];
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
			
			obs.satNav_ptr = &satNav; // for satpos_ssr()

			GTime teph = time;
			
			for (int tpredict = 0; tpredict <= acsConfig.ssrOpts.prediction_duration; tpredict += acsConfig.ssrOpts.prediction_interval)
			{
				//std::cout << "Time offset : " << tpredict << std::endl;
				GTime pTime = time + tpredict;
			
				/* grep satellite antenna information */
				PcoMapType* pcoMap_ptr = nullptr;
				{
					PhaseCenterData* satPCD = findAntenna(Sat.id(), pTime, nav);

					if (satPCD == nullptr)
					{
						if (Sat.prn < MINPRNSBS)
						{
							tracepde(1, trace,	"Warning: no satellite (%s) pco information\n", Sat.id().c_str());
							printf(				"Warning: no satellite (%s) pco information\n", Sat.id().c_str());
						}
					}
					else
					{
						pcoMap_ptr = &satPCD->pcoMap;
					}
				}
				
				bool pass = satpos(trace, pTime, teph, obs, acsConfig.ssrOpts.ephemeris_source,	E_OffsetType::APC, nav, pcoMap_ptr, false);
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(info) 
					<< Sat.id() << " failed ssrEph precise prediction.\n";
					break;
				}
				
				{
					DBEntry entry;
					entry.stringMap	[SSR_DATA		]	= {SSR_EPHEMERIS,	true};
					entry.stringMap	[SSR_TYPE		]	= {SSR_PREC,		true};
					entry.stringMap	[SSR_SAT		]	= {Sat.id(),		true};
					entry.timeMap	[SSR_EPOCH		]	= {pTime,			true};
					
					entry.vectorMap	[SSR_POS		]	= {obs.rSat,		false};
					entry.timeMap	[SSR_UPDATED	]	= {time,			false};
					
					dbEntryList.push_back(entry);
				}
				
				{
					DBEntry entry;
					entry.stringMap	[SSR_DATA		]	= {SSR_SAT_CLOCK,			true};
					entry.stringMap	[SSR_TYPE		]	= {SSR_PREC,				true};
					entry.stringMap	[SSR_SAT		]	= {Sat.id(),				true};
					entry.timeMap	[SSR_EPOCH		]	= {pTime,					true};
					
					entry.doubleMap	[SSR_SAT_CLOCK	]	= {obs.dtSat[0] * CLIGHT,	false};
					entry.timeMap	[SSR_UPDATED	]	= {time,					false};
					
					dbEntryList.push_back(entry);
				}
			}
			
			if (1)
			{
				
			}
			else
			{
				double clkVal = 0;
				bool pass = kfState.getKFValue({.type = KF::SAT_CLOCK, .Sat = Sat}, clkVal);	//todo aaron, no source here...?
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(info) 
					<< Sat.id() << " failed ssrClk precise prediction.\n";
	// 				break;
				}
				
				DBEntry entry;
				entry.stringMap	[SSR_DATA		]	= {SSR_SAT_CLOCK,	true};
				entry.stringMap	[SSR_TYPE		]	= {SSR_PREC,		true};
				entry.stringMap	[SSR_SAT		]	= {Sat.id(),		true};
				entry.timeMap	[SSR_EPOCH		]	= {time,			true};
				
				entry.doubleMap	[SSR_SAT_CLOCK	]	= {clkVal,			false};
				entry.timeMap	[SSR_UPDATED	]	= {time,			false};
				
				dbEntryList.push_back(entry);
			}
			
			for (int tpredict = 0; tpredict <= acsConfig.ssrOpts.prediction_duration; tpredict += acsConfig.ssrOpts.prediction_interval)
			{
				//std::cout << "Time offset : " << tpredict << std::endl;
				GTime pTime = time + tpredict;

				// Calculate broadcast & precise orb & clock at tsync
				bool pass = satpos(trace, pTime, teph, obs, E_Ephemeris::BROADCAST, E_OffsetType::APC, nav);
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(info) 
					<< Sat.id() << " failed ssrEph broadcast prediction.\n";
					break;
				}
				
				Eph* eph 	= seleph<Eph>(trace, teph, Sat, -1, nav);
				int iode	= eph->iode;
				
				{
					DBEntry entry;
					entry.stringMap	[SSR_DATA		]	= {SSR_EPHEMERIS,	true};
					entry.stringMap	[SSR_TYPE		]	= {SSR_BRDC,		true};
					entry.stringMap	[SSR_SAT		]	= {Sat.id(),		true};
					entry.timeMap	[SSR_EPOCH		]	= {pTime,			true};
					
					entry.intMap	[SSR_IODE		]	= {iode,			false};
					entry.vectorMap	[SSR_POS		]	= {obs.rSat,		false};
					entry.vectorMap	[SSR_VEL		]	= {obs.satVel,		false};
					entry.timeMap	[SSR_UPDATED	]	= {time,			false};
				
					dbEntryList.push_back(entry);
				}
				
				double tk 	= pTime - eph->toc;
				double	clkVal	= (eph->f0 + eph->f1 * tk + eph->f2 * tk * tk) * CLIGHT;
				
				{
					DBEntry entry;
					entry.stringMap	[SSR_TYPE		]	= {SSR_BRDC,		true};
					entry.stringMap	[SSR_DATA		]	= {SSR_SAT_CLOCK,	true};
					entry.stringMap	[SSR_SAT		]	= {Sat.id(),		true};
					entry.timeMap	[SSR_EPOCH		]	= {pTime,			true};
					
					entry.intMap	[SSR_IODE		]	= {iode,			false};
					entry.doubleMap	[SSR_SAT_CLOCK	]	= {clkVal,			false};
					entry.timeMap	[SSR_UPDATED	]	= {time,			false};
					
					dbEntryList.push_back(entry);
				}
			}
			
			for (auto once : {1})
			{
				auto it = satNav.receivedSSR.ssrCodeBias_map.upper_bound(time);			//get same biases
				if (it == satNav.receivedSSR.ssrCodeBias_map.end())
				{
					continue;
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
			}
			
			for (auto once : {1})
			{
				auto it = satNav.receivedSSR.ssrPhasBias_map.upper_bound(time);
				if (it == satNav.receivedSSR.ssrPhasBias_map.end())
				{
					continue;
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
					entry.intMap	["MWConistInd"		]	= {ssrPhase.MWConistInd,					false};
					entry.doubleMap	["yawAngle"			]	= {ssrPhase.yawAngle,						false};
					entry.doubleMap	["yawRate"			]	= {ssrPhase.yawRate,						false};
					entry.intMap	["signalIntInd"		]	= {ssrPhaseChs[obsCode].signalIntInd,		false};
					entry.intMap	["signalWidIntInd"	]	= {ssrPhaseChs[obsCode].signalWidIntInd,	false};
					entry.intMap	["signalDisconCnt"	]	= {ssrPhaseChs[obsCode].signalDisconCnt,	false};
					
					dbEntryList.push_back(entry);
				}
			}
		}
	}
	
#	ifdef ENABLE_MONGODB
	{
		if (acsConfig.output_mongo_rtcm_messages)
		{
			BOOST_LOG_TRIVIAL(info)
			<< "Writing to mongo\n";
			mongoCull(time - 300.0);
			mongoOutput(dbEntryList);
		}
	}
#	endif
}

#endif
