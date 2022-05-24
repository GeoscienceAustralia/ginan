
// #pragma GCC optimize ("O0")

#include <deque>

using std::deque;

#include "streamRtcm.hpp"
#include "acsConfig.hpp"
#include "mongoRead.hpp"
#include "common.hpp"

template <typename RETTYPE, typename INTYPE>
RETTYPE getStraddle(
	GTime			targetTime,
	deque<INTYPE>&	ssrVec)
{
	RETTYPE ssr;
	
	ssr.valid = true;
	
	//try to find a set of things that straddle the target time, with the same iode
	
	int bestI = -1;
	int bestJ = -1;
	
	for (int j = 1; j < ssrVec.size(); j++)
	{
		int i = j - 1;
		
		auto& entryI = ssrVec[i];
		auto& entryJ = ssrVec[j];
		
		if (entryI.iode != entryJ.iode)
		{
			//no good, iodes dont match
			continue;
		}
	
// 			if	( fabs(bestI.time.time - targetTime.time) > acsConfig.
// 				||fabs(bestJ.time.time - targetTime.time) > acsConfig.
// 			{
// 				continue;
// 			}
		
		//these are acceptable - store them for later
		bestI = i;
		bestJ = j;
	
		if (entryJ.time > targetTime)
		{
			//this is as close as we will come to a straddle
			break;
		}
	}
	
	if (bestJ < 0)
	{
		//nothing found, dont use
		ssr.valid = false;
		
		return RETTYPE();
	}
	
	ssr.vals[0] = ssrVec[bestI];
	ssr.vals[1]	= ssrVec[bestJ];	
	
	return ssr;
}

map<SatSys, SSROut> mongoReadSSRData(
	GTime	targetTime,
	SSRMeta	ssrMeta,
	int		masterIod,
	E_Sys	targetSys)
{
	map<SatSys, SSROut> ssrOutMap;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return ssrOutMap;
	}
	
	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.config_description];
	mongocxx::collection		coll	= db[SSR_DB];

// 	targetTime.time += 300;
	
// 	std::cout << "\nTrying to get things for " << targetTime.to_string(0) << std::endl;
	bsoncxx::types::b_date btime{std::chrono::system_clock::from_time_t(targetTime.time)};
	
	auto sats = getSysSats(targetSys);
	for (auto Sat : sats) 
	{
		deque<EphValues> ephVec;
		deque<ClkValues> clkVec;
		
		//try to get up to two entries from either side of the desired time
		for (string	data		: {SSR_EPHEMERIS, SSR_CLOCK})
		for (bool	less		: {false, true})
		{
			if (data == SSR_EPHEMERIS	&& 0)	continue;
			if (data == SSR_CLOCK		&& 0)	continue;
			
			string	moreLess;
			int		sortDir;
			if (less)		{	moreLess = "$lte";	sortDir = -1;	}
			else			{	moreLess = "$gt";	sortDir = +1;	}
		
			// Find the latest document according to t0_time.
			auto docSys		= document{}	<< SSR_SAT		<< Sat.id()
											<< SSR_DATA		<< data
											<< SSR_EPOCH	
												<< open_document
												<< moreLess << btime
												<< close_document 
											<< finalize;
											
			auto docSort	= document{}	<< SSR_EPOCH 		<< sortDir 
											<< finalize;
			
// 			fout << bsoncxx::to_json(doc) << std::endl;
			
			auto findOpts 	= mongocxx::options::find{};
			findOpts.limit(2);
			findOpts.sort(docSort.view());
			
			auto cursor  	= coll.find(docSys.view(), findOpts);

			for (auto doc : cursor)
			{
				GTime timeUpdate;
				GTime timeEpoch;
				
				auto tp		= doc[SSR_UPDATED	].get_date();
				timeUpdate.time	= std::chrono::system_clock::to_time_t(tp);
				
				tp			= doc[SSR_EPOCH		].get_date();	
				timeEpoch.time	= std::chrono::system_clock::to_time_t(tp);
				
				if (data == SSR_EPHEMERIS)
				{
					EphValues ephValues;
					ephValues.time.time 		= std::chrono::system_clock::to_time_t(tp);
					
					for (int i = 0; i < 3; i++)
					{
						ephValues.brdcPos(i) = doc[SSR_POS SSR_BRDC + std::to_string(i)].get_double();
						ephValues.precPos(i) = doc[SSR_POS SSR_PREC + std::to_string(i)].get_double();
						ephValues.brdcVel(i) = doc[SSR_VEL SSR_BRDC + std::to_string(i)].get_double();
						ephValues.precVel(i) = doc[SSR_VEL SSR_PREC + std::to_string(i)].get_double();
					}
					
					ephValues.iode	= doc[SSR_IODE].get_int32();
					
					if (less)	ephVec	.push_front	(ephValues);
					else		ephVec	.push_back	(ephValues);
					
					continue;
				}
				
				if (data == SSR_CLOCK)
				{
					ClkValues clkValues;
					clkValues.time.time 		= std::chrono::system_clock::to_time_t(tp);
					
					clkValues.brdcClk 	= doc[SSR_CLOCK SSR_BRDC].get_double();
					clkValues.precClk 	= doc[SSR_CLOCK SSR_PREC].get_double();
				
					clkValues.iode	= doc[SSR_IODE		].get_int32();
					
// 					std::cout << Sat.id() << " less:" << less << " brdc:" << broadcast << "   " << clkValues.time.to_string(0) << std::endl;
					
					if (less)	clkVec	.push_front	(clkValues);
					else		clkVec	.push_back	(clkValues);
					
					continue;
				}
			}
		}
		
// 	for (auto& a : clkBroadcastVec)
// 	{
// 		std::cout << Sat.id() << "Final cbrdcs:" << " iode: " << a.iode <<  " "<< a.time.to_string(0) << std::endl;
// 	}
// 	for (auto& a : clkPreciseVec)
// 	{
// 		std::cout << Sat.id() << "Final cprecs:" << " iode: " << a.iode <<  " "<< a.time.to_string(0) << std::endl;
// 	}
// 	for (auto& a : ephBroadcastVec)
// 	{
// 		std::cout << Sat.id() << "Final ebrdcs:" << " iode: " << a.iode <<  " "<< a.time.to_string(0) << std::endl;
// 	}
// 	for (auto& a : ephPreciseVec)
// 	{
// 		std::cout << Sat.id() << "Final eprecs:" << " iode: " << a.iode <<  " "<< a.time.to_string(0) << std::endl;
// 	}
		
		//try to find a set of things that straddle the target time, with the same iode
		//do for both broadcast and precise values
		SSROut ssrOut;
		ssrOut.ephInput = getStraddle<SSREphInput>(targetTime, ephVec);
		ssrOut.clkInput = getStraddle<SSRClkInput>(targetTime, clkVec);
		
		if	(ssrOut.ephInput.valid == false)
		{
			tracepdeex(3, std::cout, "Could not retrieve valid ephemeris for %s\n", Sat.id().c_str()); 
			continue;
		}
		if	( ssrOut.clkInput.valid == false)
		{
			tracepdeex(3, std::cout, "Could not retrieve valid clock     for %s\n", Sat.id().c_str()); 
			continue;
		}

		ssrOutMap[Sat] = ssrOut;
	}

	return ssrOutMap;
}

SsrPBMap mongoReadPhaseBias(
	GTime	time,
	SSRMeta	ssrMeta,
	int		masterIod,
	E_Sys	targetSys)
{
	SsrPBMap ssrPBMap;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return ssrPBMap;
	}

	auto sats = getSysSats(targetSys);

	Mongo&						mongo 	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.config_description];
	mongocxx::collection		coll	= db[SSR_DB];

	bsoncxx::types::b_date btime{std::chrono::system_clock::from_time_t(time.time)};
	
	for (auto Sat : sats) 
	{
		SSRPhasBias ssrPhasBias;
		ssrPhasBias.ssrMeta = ssrMeta;
		ssrPhasBias.iod 	= masterIod;

		// Find the latest document according to t0_time.
		auto docSys		= document{}	<< SSR_SAT		<< Sat.id()
										<< SSR_DATA		<< SSR_PHAS_BIAS
										<< finalize;
										
		auto docSort	= document{}	<< SSR_EPOCH 		<< -1 
										<< finalize; // Get newest entry.

		auto findOpts 		= mongocxx::options::find{};
		findOpts.sort(docSort.view());
		
		auto cursor  	= coll.find(docSys.view(), findOpts);
	
		ssrPhasBias.t0.time = 0;
		for (auto satDoc : cursor)
		{
			GTime t0;
			auto tp									= satDoc[SSR_EPOCH			].get_date();
			t0.time 								= std::chrono::system_clock::to_time_t(tp);
			
			BiasVar biasVar;
			ssrPhasBias.t0	= t0;
			ssrPhasBias.ssrPhase.dispBiasConistInd	= satDoc["dispBiasConistInd"].get_int32();
			ssrPhasBias.ssrPhase.MWConistInd		= satDoc["MWConistInd"		].get_int32();
			ssrPhasBias.ssrPhase.yawAngle			= satDoc["yawAngle"			].get_double();
			ssrPhasBias.ssrPhase.yawRate			= satDoc["yawRate"			].get_double();

			SSRPhaseCh ssrPhaseCh;
			ssrPhaseCh.signalIntInd 				= satDoc["signalIntInd"		].get_int32();
			ssrPhaseCh.signalWidIntInd 				= satDoc["signalWidIntInd"	].get_int32();
			ssrPhaseCh.signalDisconCnt 				= satDoc["signalDisconCnt"	].get_int32();

			auto strView							= satDoc[SSR_OBSCODE		].get_utf8().value;
			string obsStr 	= strView.to_string();
			E_ObsCode obsCode = E_ObsCode::_from_string(obsStr.c_str());

			biasVar.bias							= satDoc[SSR_BIAS			].get_double();
			biasVar.var								= satDoc[SSR_VAR			].get_double();

			ssrPhasBias.obsCodeBiasMap	[obsCode]	= biasVar;
			ssrPhasBias.ssrPhaseChs		[obsCode]	= ssrPhaseCh;
		}
		
		if (ssrPhasBias.t0.time != 0)
		{
			// std::cout << "Phase Bias frame . ";
			// std::cout << "satId : " << Sat.id();
			// std::cout << ". Num Observation codes : " << ssrPhasBias.obsCodeBiasMap.size() << std::endl;
			//std::cout << "Num : " << targetSys._to_string() << " Bias read " << ssrPBMap[ssrPhasBias.t0].size() << std::endl;
			ssrPBMap[Sat] = ssrPhasBias;
		}
	}
	
	return ssrPBMap;
}

SsrCBMap mongoReadCodeBias(
	GTime	time,
	SSRMeta	ssrMeta,
	int		masterIod,
	E_Sys	targetSys)
{
	SsrCBMap ssrCBMap;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return ssrCBMap;
	}

	auto sats = getSysSats(targetSys);

	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.config_description];
	mongocxx::collection		coll	= db[SSR_DB];

	bsoncxx::types::b_date btime{std::chrono::system_clock::from_time_t(time.time)};
	
	for (auto Sat : sats) 
	{
		SSRCodeBias ssrCodeBias;
		ssrCodeBias.ssrMeta = ssrMeta;
		ssrCodeBias.iod 	= masterIod;
		ssrCodeBias.t0.time = 0;

		// Find the latest document according to t0_time.
		auto docSys		= document{}	<< SSR_SAT		<< Sat.id()
										<< SSR_DATA		<< SSR_CODE_BIAS
										<< SSR_EPOCH	
											<< open_document
											<< "$lt" << btime
											<< close_document 
										<< finalize;
										
		auto docSort	= document{}	<< SSR_EPOCH 		<< 1 
										<< finalize;
		
		auto findOpts 	= mongocxx::options::find{};
		findOpts.limit(1);
		
		auto cursor  	= coll.find(docSys.view(), findOpts);

		for (auto satDoc : cursor)
		{
			GTime t0;
			auto tp				= satDoc[SSR_EPOCH		].get_date();
			t0.time 			= std::chrono::system_clock::to_time_t(tp);
			
			ssrCodeBias.t0		= t0;
			auto strView		= satDoc[SSR_OBSCODE	].get_utf8().value;
			string obsStr 		= strView.to_string();
			E_ObsCode obsCode 	= E_ObsCode::_from_string(obsStr.c_str());

			BiasVar biasVar;
			biasVar.bias 		= satDoc[SSR_BIAS		].get_double();
			biasVar.var 		= satDoc[SSR_VAR		].get_double();
			ssrCodeBias.obsCodeBiasMap[obsCode] = biasVar;
		}
		
		if (ssrCodeBias.t0.time != 0)
		{
			//std::cout << "Code Bias.\n"
			//std::cout << "satId : " << sat.id() << std::endl;
			//std::cout << "Num Observation codes : " << ssrCodeBias.codeBias_map.size() << std::endl;
			//std::cout << "Num : " << targetSys._to_string() << " Bias read " << ssrCBMap[ssrCodeBias.t0].size() << std::endl;
			ssrCBMap[Sat] = ssrCodeBias;
		}
	}
	
	return ssrCBMap;
}
