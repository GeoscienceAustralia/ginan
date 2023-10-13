
// #pragma GCC optimize ("O0")

#include <deque>

using std::deque;

#include "streamRtcm.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "mongoRead.hpp"
#include "common.hpp"

using bsoncxx::types::b_date;

short int			currentSSRIod = 0;	//todo aaron, sketchy global?
map<SatSys, int>	lastBrdcIode;

template <typename RETTYPE, typename INTYPE>
RETTYPE getStraddle(
	GTime			referenceTime,
	deque<INTYPE>&	ssrVec)
{
	RETTYPE ssr;
	
	ssr.valid = true;
	
	//try to find a set of things that straddle the reference time, with the same iode
	
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
		
		//these are acceptable - store them for later
		bestI = i;
		bestJ = j;
	
		if (entryJ.time > referenceTime)
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

GTime mongoReadLastClock()		//todo aaron delete me
{
	GTime outTime;
	
	auto& mongo_ptr = remoteMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return outTime;
	}
	
	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.remoteMongo.database];
	mongocxx::collection		coll	= db[SSR_DB];
	
	auto docClk		= document{}	<< SSR_DATA		<< SSR_CLOCK
									<< finalize;
											
	auto docSort	= document{}	<< SSR_EPOCH 		<< -1 
									<< finalize;
	
	auto findOpts 	= mongocxx::options::find{};
	findOpts.limit(1);
	findOpts.sort(docSort.view());
	
	auto cursor  	= coll.find(docClk.view(), findOpts);
	for (auto doc : cursor)
	{
		PTime timeEpoch;
		auto tp			= doc[SSR_EPOCH		].get_date();	
		timeEpoch.bigTime	= std::chrono::system_clock::to_time_t(tp);
		outTime = timeEpoch;			
	}
	return outTime;
}

/** Read orbits and clocks from Mongo DB
*/
SsrOutMap mongoReadOrbClk(
	GTime		referenceTime,		///< reference time (t0) of SSR correction
	SSRMeta&	ssrMeta,			///< SSR message metadata
	int			masterIod,			///< IOD SSR
	E_Sys		targetSys)			///< target system
{
	SsrOutMap ssrOutMap;
	
	auto& mongo_ptr = remoteMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return ssrOutMap;
	}
	
	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.remoteMongo.database];
	mongocxx::collection		coll	= db[SSR_DB];
	
// 	std::cout << "\nTrying to get things for " << targetTime.to_string(0) << std::endl;
	b_date btime{std::chrono::system_clock::from_time_t((time_t)((PTime)referenceTime).bigTime)};
	
	bool changeIod = false;
	auto sats = getSysSats(targetSys);
	for (auto Sat : sats) 
	{
		deque<EphValues> ephVec;
		deque<ClkValues> clkVec;
		
		//try to get up to two entries from either side of the desired time
		for (string	data		: {SSR_EPHEMERIS, SSR_CLOCK})
		for (bool	less		: {false, true})
		{
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
				PTime timeUpdate;
				auto tp		= doc[SSR_UPDATED	].get_date();
				timeUpdate.bigTime	= std::chrono::system_clock::to_time_t(tp);
				
				PTime timeEpoch;
				tp			= doc[SSR_EPOCH		].get_date();	
				timeEpoch.bigTime	= std::chrono::system_clock::to_time_t(tp);
				
				if (data == SSR_EPHEMERIS)
				{
					EphValues ephValues;
					ephValues.time = timeEpoch;
					
					for (int i = 0; i < 3; i++)
					{
						ephValues.brdcPos(i) = doc[SSR_POS SSR_BRDC + std::to_string(i)].get_double();
						ephValues.precPos(i) = doc[SSR_POS SSR_PREC + std::to_string(i)].get_double();
						ephValues.brdcVel(i) = doc[SSR_VEL SSR_BRDC + std::to_string(i)].get_double();
						ephValues.precVel(i) = doc[SSR_VEL SSR_PREC + std::to_string(i)].get_double();
					}
					
					ephValues.ephVar	= doc[SSR_VAR	].get_double();
					ephValues.iode		= doc[SSR_IODE	].get_int32();
					
					if (less)	ephVec	.push_front	(ephValues);
					else		ephVec	.push_back	(ephValues);
					
					continue;
				}
				
				if (data == SSR_CLOCK)
				{
					ClkValues clkValues;
					clkValues.time = timeEpoch;
					
					clkValues.brdcClk 	= doc[SSR_CLOCK SSR_BRDC].get_double();
					clkValues.precClk 	= doc[SSR_CLOCK SSR_PREC].get_double();
				
					clkValues.iode		= doc[SSR_IODE		].get_int32();
					
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
		
		//try to find a set of things that straddle the reference time, with the same iode
		//do for both broadcast and precise values
		SSROut ssrOut;
		ssrOut.ephInput = getStraddle<SSREphInput>(referenceTime, ephVec);
		ssrOut.clkInput = getStraddle<SSRClkInput>(referenceTime, clkVec);
		
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
		
		if (ssrOut.ephInput.vals[0].iode != lastBrdcIode[Sat])
		{
			changeIod = true;
			lastBrdcIode[Sat] = ssrOut.ephInput.vals[0].iode;
		}
	}

	if (changeIod)
	{
		currentSSRIod++;
		
		if (currentSSRIod > 15)
			currentSSRIod = 0;
	}
	
	masterIod = currentSSRIod;

	return ssrOutMap;
}

/** Group and sort biases in Mongo DB to avoid unnecessary requests
*/
void mongoGroupSortBias(
	bsoncxx::builder::stream::document&	doc)
{
	doc << "_id" 
		<< open_document 
			<< SSR_SAT		<< "$Sat"
			<< SSR_OBSCODE	<< "$ObsCode"
		<< close_document
		
		<< "lastEpoch"
		<< open_document
			<< "$max"
			<< open_document
				<< "$mergeObjects"
				<< open_array
					<< open_document
						<< SSR_EPOCH	<< "$Epoch"
					<< close_document
					<< "$$ROOT"
				<< close_array
			<< close_document
		<< close_document;
}

/** Read phase biases from Mongo DB
*/
SsrPBMap mongoReadPhaseBias(
	SSRMeta&	ssrMeta,			///< SSR message metadata
	int			masterIod,			///< IOD SSR
	E_Sys		targetSys)			///< target system
{
	SsrPBMap ssrPBMap;
	
	auto& mongo_ptr = remoteMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return ssrPBMap;
	}

	auto sats = getSysSats(targetSys);

	Mongo&						mongo 	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.remoteMongo.database];
	mongocxx::collection		coll	= db[SSR_DB];

	mongocxx::pipeline p;
	p.match(bsoncxx::builder::basic::make_document(bsoncxx::builder::basic::kvp(SSR_DATA, SSR_PHAS_BIAS)));
	// p.sort (bsoncxx::builder::basic::make_document(bsoncxx::builder::basic::kvp(SSR_EPOCH, 1)));

	bsoncxx::builder::stream::document doc = {};

	mongoGroupSortBias(doc);
	p.group(doc.view());
	
	auto cursor = coll.aggregate(p, mongocxx::options::aggregate{});
	
	for (auto resultDoc : cursor)
	{
		auto entry			= resultDoc["lastEpoch"];
		auto strView		= entry[SSR_SAT			].get_utf8().value;
		string satStr 		= strView.to_string();
		SatSys Sat(satStr.c_str());

		if (Sat.sys != targetSys)
			continue;

		SSRPhasBias& ssrPhasBias	= ssrPBMap[Sat];
		ssrPhasBias.ssrMeta			= ssrMeta;
		ssrPhasBias.iod				= masterIod;

		auto tp									= entry[SSR_EPOCH			].get_date();
		PTime t0;
		t0.bigTime 								= std::chrono::system_clock::to_time_t(tp);

		if (!t0.bigTime)
			continue;
		
		ssrPhasBias.t0							= t0;
		ssrPhasBias.ssrPhase.dispBiasConistInd	= entry["dispBiasConistInd"	].get_int32();
		ssrPhasBias.ssrPhase.MWConistInd		= entry["MWConistInd"		].get_int32();
		ssrPhasBias.ssrPhase.yawAngle			= entry["yawAngle"			].get_double();
		ssrPhasBias.ssrPhase.yawRate			= entry["yawRate"			].get_double();

		SSRPhaseCh ssrPhaseCh;
		ssrPhaseCh.signalIntInd 				= entry["signalIntInd"		].get_int32();
		ssrPhaseCh.signalWLIntInd 				= entry["signalWLIntInd"	].get_int32();
		ssrPhaseCh.signalDisconCnt 				= entry["signalDisconCnt"	].get_int32();

		strView									= entry[SSR_OBSCODE			].get_utf8().value;
		string obsStr							= strView.to_string();
		E_ObsCode obsCode						= E_ObsCode::_from_string(obsStr.c_str());

		BiasVar biasVar;
		biasVar.bias							= entry[SSR_BIAS			].get_double();
		biasVar.var								= entry[SSR_VAR				].get_double();

		ssrPhasBias.obsCodeBiasMap	[obsCode]	= biasVar;			// last entry wins
		ssrPhasBias.ssrPhaseChs		[obsCode]	= ssrPhaseCh;
	}
		
	return ssrPBMap;
}

/** Read code biases from Mongo DB
*/
SsrCBMap mongoReadCodeBias(
	SSRMeta&	ssrMeta,			///< SSR message metadata
	int			masterIod,			///< IOD SSR
	E_Sys		targetSys)			///< target system
{
	SsrCBMap ssrCBMap;
	
	auto& mongo_ptr = remoteMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return ssrCBMap;
	}

	auto sats = getSysSats(targetSys);

	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.remoteMongo.database];
	mongocxx::collection		coll	= db[SSR_DB];

	mongocxx::pipeline p;
	p.match(bsoncxx::builder::basic::make_document(bsoncxx::builder::basic::kvp(SSR_DATA, SSR_CODE_BIAS)));
	// p.sort (bsoncxx::builder::basic::make_document(bsoncxx::builder::basic::kvp(SSR_EPOCH, 1)));

	bsoncxx::builder::stream::document doc = {};

	mongoGroupSortBias(doc);
	p.group(doc.view());
	
	auto cursor = coll.aggregate(p, mongocxx::options::aggregate{});
	
	for (auto resultDoc : cursor)
	{
		auto entry			= resultDoc["lastEpoch"];
		auto strView		= entry[SSR_SAT		].get_utf8().value;
		string satStr 		= strView.to_string();
		SatSys Sat(satStr.c_str());

		if (Sat.sys != targetSys)
			continue;

		SSRCodeBias& ssrCodeBias	= ssrCBMap[Sat];
		ssrCodeBias.ssrMeta			= ssrMeta;
		ssrCodeBias.iod				= masterIod;

		auto tp				= entry[SSR_EPOCH	].get_date();
		PTime t0;
		t0.bigTime 			= std::chrono::system_clock::to_time_t(tp);

		if (!t0.bigTime)
			continue;
		
		ssrCodeBias.t0		= t0;

		strView				= entry[SSR_OBSCODE	].get_utf8().value;
		string obsStr 		= strView.to_string();
		E_ObsCode obsCode 	= E_ObsCode::_from_string(obsStr.c_str());

		BiasVar biasVar;
		biasVar.bias 		= entry[SSR_BIAS	].get_double();
		biasVar.var 		= entry[SSR_VAR		].get_double();

		ssrCodeBias.obsCodeBiasMap[obsCode] = biasVar;		// last entry wins
	}

	return ssrCBMap;
}

/** Read GPS/GAL/BDS/QZS ephemeris from Mongo DB
*/
Eph	mongoReadEphemeris(
	GTime			targetTime,			///< target system
	SatSys			Sat,				///< satellite to read ephemeris of
	RtcmMessageType	rtcmMessCode)		///< RTCM message code to read ephemeris of
{
	Eph				eph;
	E_NavMsgType	type;

	auto& mongo_ptr = remoteMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return eph;
	}
	
	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.remoteMongo.database];
	mongocxx::collection		coll	= db["Ephemeris"];

	b_date btime{std::chrono::system_clock::from_time_t((time_t)((PTime)targetTime).bigTime)};

	switch (rtcmMessCode)
	{
		case +RtcmMessageType:: GPS_EPHEMERIS:		// fallthrough
		case +RtcmMessageType:: QZS_EPHEMERIS:		type	= E_NavMsgType::LNAV;	break;
		case +RtcmMessageType:: BDS_EPHEMERIS:		type	= E_NavMsgType::D1;		break;
		case +RtcmMessageType:: GAL_FNAV_EPHEMERIS:	type	= E_NavMsgType::FNAV;	break;
		case +RtcmMessageType:: GAL_INAV_EPHEMERIS:	type	= E_NavMsgType::INAV;	break;
		default:
			BOOST_LOG_TRIVIAL(error) << "Error, attempting to upload incorrect message type.\n";
			return eph;
	}
	
	// Find the latest document according to t0_time.
	auto docSys		= document{}	<< "Sat"		<< Sat.id()
									<< "Type"		<< type._to_string()
									<< finalize;
									
	auto docSort	= document{}	<< "ToeGPST" 	<< -1	// Newest entry comes first
									<< finalize;

	auto findOpts 	= mongocxx::options::find{};
	findOpts.sort(docSort.view());
	findOpts.limit(1);	// Only get the first entry

	auto cursor  	= coll.find(docSys.view(), findOpts);

	for (auto satDoc : cursor)
	{
		eph.Sat		= Sat;
		eph.type	= type;

		PTime ptoe;
		auto toe		= satDoc["ToeGPST"	].get_date();
		ptoe.bigTime	= std::chrono::system_clock::to_time_t(toe);
		eph.toe			= ptoe;

		PTime ptoc;
		auto toc		= satDoc["TocGPST"	].get_date();
		ptoc.bigTime	= std::chrono::system_clock::to_time_t(toc);
		eph.toc			= ptoc;

		eph.weekRollOver	= satDoc["WeekDecoded"	].get_int32();
		eph.week			= satDoc["WeekAdjusted"	].get_int32();
		eph.toes			= satDoc["ToeSecOfWeek"	].get_double();
		eph.tocs			= satDoc["TocSecOfWeek"	].get_double();

		eph.aode	= satDoc["AODE"		].get_int32();
		eph.aodc	= satDoc["AODC"		].get_int32();
		eph.iode	= satDoc["IODE"		].get_int32();
		eph.iodc	= satDoc["IODC"		].get_int32();
	
		eph.f0		= satDoc["f0"		].get_double();
		eph.f1		= satDoc["f1"		].get_double();
		eph.f2		= satDoc["f2"		].get_double();

		eph.sqrtA	= satDoc["SqrtA"	].get_double();
		eph.A		= satDoc["A"		].get_double();
		eph.e		= satDoc["e"		].get_double();
		eph.i0		= satDoc["i0"		].get_double();
		eph.idot	= satDoc["iDot"		].get_double();
		eph.omg		= satDoc["omg"		].get_double();
		eph.OMG0	= satDoc["OMG0"		].get_double();
		eph.OMGd	= satDoc["OMGDot"	].get_double();
		eph.M0		= satDoc["M0"		].get_double();
		eph.deln	= satDoc["DeltaN"	].get_double();
		eph.crc		= satDoc["Crc"		].get_double();
		eph.crs		= satDoc["Crs"		].get_double();
		eph.cic		= satDoc["Cic"		].get_double();
		eph.cis		= satDoc["Cis"		].get_double();
		eph.cuc		= satDoc["Cuc"		].get_double();
		eph.cus		= satDoc["Cus"		].get_double();

		eph.tgd[0]	= satDoc["TGD0"		].get_double();
		eph.tgd[1]	= satDoc["TGD1"		].get_double();
		eph.sva		= satDoc["URAIndex"	].get_int32();
	
		if	( eph.Sat.sys == +E_Sys::GPS
			||eph.Sat.sys == +E_Sys::QZS)
		{
			eph.ura[0]	= satDoc["URA"				].get_double();
			int svh		= satDoc["SVHealth"			].get_int32();		eph.svh	= (E_Svh)svh;
			eph.code	= satDoc["CodeOnL2"			].get_int32();
			eph.flag	= satDoc["L2PDataFlag"		].get_int32();
			eph.fitFlag	= satDoc["FitFlag"			].get_int32();
			eph.fit		= satDoc["FitInterval"		].get_double();
		}
		else if (eph.Sat.sys == +E_Sys::GAL)
		{
			eph.ura[0]	= satDoc["SISA"				].get_double();
			int svh		= satDoc["SVHealth"			].get_int32();		eph.svh	= (E_Svh)svh;
			eph.e5a_hs	= satDoc["E5aHealth"		].get_int32();
			eph.e5a_dvs	= satDoc["E5aDataValidity"	].get_int32();
			eph.e5b_hs	= satDoc["E5bHealth"		].get_int32();
			eph.e5b_dvs	= satDoc["E5bDataValidity"	].get_int32();
			eph.e1_hs	= satDoc["E1Health"			].get_int32();
			eph.e1_dvs	= satDoc["E1DataValidity"	].get_int32();
			eph.code	= satDoc["DataSource"		].get_int32();
		}
		else if (eph.Sat.sys == +E_Sys::BDS)
		{
			eph.ura[0]	= satDoc["URA"				].get_double();
			int svh		= satDoc["SVHealth"			].get_int32();		eph.svh	= (E_Svh)svh;
		}
	}
	
	return eph;
}

/** Read GLO ephemeris from Mongo DB
*/
Geph mongoReadGloEphemeris(
	GTime	targetTime,			///< target system
	SatSys	Sat)				///< satellite to read ephemeris of
{
	Geph geph;
	
	auto& mongo_ptr = remoteMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return geph;
	}

	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.remoteMongo.database];
	mongocxx::collection		coll	= db["Ephemeris"];

	b_date btime{std::chrono::system_clock::from_time_t((time_t)((PTime)targetTime).bigTime)};

	// Find the latest document according to t0_time.
	auto docSys		= document{}	<< "Sat"		<< Sat.id()
									<< finalize;
									
	auto docSort	= document{}	<< "ToeGPST" 	<< -1	// Newest entry comes first
									<< finalize;

	auto findOpts 	= mongocxx::options::find{};
	findOpts.sort(docSort.view());
	findOpts.limit(1);	// Only get the first entry

	auto cursor  	= coll.find(docSys.view(), findOpts);

	for (auto satDoc : cursor)
	{
		geph.Sat	= Sat;
		geph.type	= E_NavMsgType::FDMA;

		PTime ptoe;
		auto toe		= satDoc["ToeGPST"	].get_date();
		ptoe.bigTime	= std::chrono::system_clock::to_time_t(toe);
		geph.toe		= ptoe; 

		PTime ptof;
		auto tof		= satDoc["TofGPST"	].get_date();
		ptof.bigTime	= std::chrono::system_clock::to_time_t(tof);
		geph.tof	 	= ptof;

		geph.tb			= satDoc["ToeSecOfDay"	].get_int32();
		geph.tk_hour	= satDoc["TofHour"		].get_int32();
		geph.tk_min		= satDoc["TofMin"		].get_int32();
		geph.tk_sec		= satDoc["TofSec"		].get_double();
		
		geph.iode		= satDoc["IODE"			].get_int32();
		
		geph.taun		= satDoc["TauN"			].get_double();
		geph.gammaN		= satDoc["GammaN"		].get_double();
		geph.dtaun		= satDoc["DeltaTauN"	].get_double();
		
		geph.pos[0]		= satDoc["PosX"			].get_double();
		geph.pos[1]		= satDoc["PosY"			].get_double();
		geph.pos[2]		= satDoc["PosZ"			].get_double();
		geph.vel[0]		= satDoc["VelX"			].get_double();
		geph.vel[1]		= satDoc["VelY"			].get_double();
		geph.vel[2]		= satDoc["VelZ"			].get_double();
		geph.acc[0]		= satDoc["AccX"			].get_double();
		geph.acc[1]		= satDoc["AccY"			].get_double();
		geph.acc[2]		= satDoc["AccZ"			].get_double();

		geph.frq		= satDoc["FrquencyNumber"	].get_int32();
		int svh			= satDoc["SVHealth"			].get_int32();		geph.svh	= (E_Svh)svh;
		geph.age		= satDoc["Age"				].get_int32();

		geph.glonassM	= satDoc["GLONASSM"				].get_int32();
		geph.NT			= satDoc["NumberOfDayIn4Year"	].get_int32();
		geph.moreData	= satDoc["AdditionalData"		].get_bool();
		geph.N4			= satDoc["4YearIntervalNumber"	].get_int32();
	}

	return geph;
}
	
	
SSRAtm mongoReadIGSIonosphere(
			GTime		time,
	const	SSRMeta&	ssrMeta,
			int			masterIod)
{
	SSRAtm ssrAtm;
	
	auto& mongo_ptr = remoteMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return ssrAtm;
	}
	ssrAtm.ssrMeta = ssrMeta;
	
	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[acsConfig.remoteMongo.database];
	mongocxx::collection		coll	= db[SSR_DB];

	b_date btime{std::chrono::system_clock::from_time_t((time_t)((PTime)time).bigTime)};
	
	// Find the latest document according to t0_time.
	auto docSys		= document{}	<< SSR_DATA		<< IGS_ION_META
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
	
	SSRAtmGlobal atmGlob;
	int nbasis;
	for (auto atmDoc : cursor)
	{
		PTime t0;
		auto tp					= atmDoc[SSR_EPOCH		].get_date();
		t0.bigTime 				= std::chrono::system_clock::to_time_t(tp);
	
		atmGlob.time			= t0;
		
		atmGlob.numberLayers	= atmDoc[IGS_ION_NLAY	].get_int32();
		nbasis					= atmDoc[IGS_ION_NBAS	].get_int32();
		atmGlob.vtecQuality		= atmDoc[IGS_ION_QLTY	].get_double();
		for (int i=0; i < atmGlob.numberLayers; i++)
		{
			string hghStr = "Height_"+std::to_string(i);
			atmGlob.layers[i].height = atmDoc[hghStr	].get_double();
		}
	}
	
	auto timobj = b_date {std::chrono::system_clock::from_time_t(atmGlob.time.bigTime)};
	auto docEntr    = document{}    << SSR_DATA     << IGS_ION_ENTRY
                                	<< SSR_EPOCH    << timobj 
                                	<< finalize;
	auto cursor2     = coll.find(docEntr.view(), mongocxx::options::find{});
	
	map<int,int> maxBasis;
	for (auto atmDoc : cursor2)
	{
		SphComp	sphComp;
		sphComp.layer			= atmDoc[IGS_ION_HGT	].get_int32();
		sphComp.degree			= atmDoc[IGS_ION_DEG	].get_int32();
		sphComp.order			= atmDoc[IGS_ION_ORD	].get_int32();
		int trigType			= atmDoc[IGS_ION_PAR	].get_int32();
		
		sphComp.trigType		= E_TrigType::_from_integral(trigType);
		
		sphComp.value			= atmDoc[IGS_ION_VAL	].get_double();
		sphComp.variance		= 0;
		
		SSRVTEClayer& laydata	= atmGlob.layers[sphComp.layer];
		
		laydata.sphHarmonic[maxBasis[sphComp.layer]] = sphComp;
		maxBasis[sphComp.layer]++;
		
		if (laydata.maxDegree	< sphComp.degree)			laydata.maxDegree	= sphComp.degree;
		if (laydata.maxOrder	< sphComp.order)			laydata.maxOrder	= sphComp.order;
	}
	
	atmGlob.iod = masterIod;
	
	ssrAtm.atmosGlobalMap[atmGlob.time] = atmGlob;
	
	return ssrAtm;
}

KFState mongoReadFilter(
	const	GTime&		time,
	const	SatSys&		Sat,
	const	string&		str,
	const	vector<KF>&	types,
			bool		remote)
{
	KFState kfState;
	
	Mongo* mongo_ptr;
	
	if (remote)	mongo_ptr = remoteMongo_ptr;
	else		mongo_ptr = localMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return kfState;
	}
	
	string database;
	if (remote)	database = acsConfig.remoteMongo.database;
	else		database = acsConfig.localMongo	.database;
	
	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[database];
	mongocxx::collection		coll	= db[REMOTE_DATA_DB];
	
	b_date btime{std::chrono::system_clock::from_time_t((time_t)((PTime)time).bigTime)};
	
	for (auto& type : types)
	{
		// Find the latest document according to t0_time.
		auto docSys		= document{};
		
										docSys << REMOTE_DATA		<< type._to_string();
		if (str.empty() == false)		docSys << REMOTE_STR		<< str;
		if (Sat.prn)					docSys << REMOTE_SAT		<< Sat.id();
		if (time != GTime::noTime())	docSys << REMOTE_EPOCH		<< btime;
						
		auto findOpts 	= mongocxx::options::find{};
// 		if	( Sat.prn	!= 0
// 			&&time		!= GTime::noTime())
// 		{
// 			findOpts.limit(1);
// 		}
			
		auto cursor  	= coll.find(docSys.view(), findOpts);
		
		for (auto doc : cursor)
		{
	// 		PTime updated;
	// 		auto tp					= doc[REMOTE_UPDATED		].get_date();
	// 		updated.bigTime 		= std::chrono::system_clock::to_time_t(tp);
			
			PTime time;
			auto tp2				= doc[REMOTE_EPOCH			].get_date();
			time.bigTime			= std::chrono::system_clock::to_time_t(tp2);
			
			
// 			Vector6d inertialState = Vector6d::Zero();
			
// 			for (int i = 0; i < 3; i++)
			{
// 				inertialState(i + 0) = doc[REMOTE_POS + std::to_string(i)].get_double();
// 				inertialState(i + 3) = doc[REMOTE_VEL + std::to_string(i)].get_double();
			}
			
			string sat = doc[REMOTE_SAT].get_utf8().value.to_string();
			
			SatSys Sat(sat.c_str());
// 			
// 			predictedPosMap[Sat][time] = inertialState;
		}
	}
	
	return kfState;
}

map<SatSys, map<GTime, Vector6d>> mongoReadOrbits(
	GTime	time,
	SatSys	Sat,
	bool	remote,
	double*	var_ptr)
{
	map<SatSys, map<GTime, Vector6d>> predictedPosMap;
	
	Mongo* mongo_ptr;
	
	if (remote)	mongo_ptr = remoteMongo_ptr;
	else		mongo_ptr = localMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return predictedPosMap;
	}
	
	string database;
	if (remote)	database = acsConfig.remoteMongo.database;
	else		database = acsConfig.localMongo	.database;
	
	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[database];
	mongocxx::collection		coll	= db[REMOTE_DATA_DB];
	
	b_date btime{std::chrono::system_clock::from_time_t((time_t)((PTime)time).bigTime)};
	
	// Find the latest document according to t0_time.
	auto docSys		= document{};
	
									docSys << REMOTE_DATA		<< REMOTE_ORBIT;
	if (Sat.prn)					docSys << REMOTE_SAT		<< Sat.id();
	if (time != GTime::noTime())	docSys << REMOTE_EPOCH		<< btime;
					
	auto findOpts 	= mongocxx::options::find{};
	if	( Sat.prn	!= 0
		&&time		!= GTime::noTime())
	{
		findOpts.limit(1);
	}
		
	auto cursor  	= coll.find(docSys.view(), findOpts);
	
	for (auto doc : cursor)
	{
// 		PTime updated;
// 		auto tp					= doc[REMOTE_UPDATED		].get_date();
// 		updated.bigTime 		= std::chrono::system_clock::to_time_t(tp);
		
		PTime time;
		auto tp2				= doc[REMOTE_EPOCH			].get_date();
		time.bigTime			= std::chrono::system_clock::to_time_t(tp2);
		
		Vector6d inertialState = Vector6d::Zero();
		
		for (int i = 0; i < 3; i++)
		{
			inertialState(i + 0) = doc[REMOTE_POS + std::to_string(i)].get_double();
			inertialState(i + 3) = doc[REMOTE_VEL + std::to_string(i)].get_double();
		}
		
		if (var_ptr)
		{
			*var_ptr = doc[REMOTE_VAR].get_double();
		}
		
		string sat = doc[REMOTE_SAT].get_utf8().value.to_string();
		
		SatSys Sat(sat.c_str());
		
		predictedPosMap[Sat][time] = inertialState;
	}
	
	return predictedPosMap;
}

map<string, map<GTime, tuple<double, double>>> mongoReadClocks(
	GTime	time,
	string	str,
	bool	remote)
{
	map<string, map<GTime, tuple<double, double>>> predictedClkMap;
	
	Mongo* mongo_ptr;
	
	if (remote)	mongo_ptr = remoteMongo_ptr;
	else		mongo_ptr = localMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return predictedClkMap;
	}
	
	string database;
	if (remote)	database = acsConfig.remoteMongo.database;
	else		database = acsConfig.localMongo	.database;
	
	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[database];
	mongocxx::collection		coll	= db[REMOTE_DATA_DB];
	
	b_date btime{std::chrono::system_clock::from_time_t((time_t)((PTime)time).bigTime)};
	
	// Find the latest document according to t0_time.
	auto docSys		= document{};
	
									docSys << REMOTE_DATA		<< REMOTE_CLOCK;
	if (str.empty() == false)		docSys << REMOTE_SAT		<< str;
	if (time != GTime::noTime())	docSys << REMOTE_EPOCH		<< btime;
					
	auto findOpts 	= mongocxx::options::find{};
	if	( str.empty()	== false
		&&time			!= GTime::noTime())
	{
		findOpts.limit(1);
	}
		
	auto cursor  	= coll.find(docSys.view(), findOpts);
	
	for (auto doc : cursor)
	{
// 		PTime updated;
// 		auto tp					= doc[REMOTE_UPDATED		].get_date();
// 		updated.bigTime 		= std::chrono::system_clock::to_time_t(tp);
		PTime time;
		auto tp2				= doc[REMOTE_EPOCH			].get_date();
		time.bigTime			= std::chrono::system_clock::to_time_t(tp2);
		
		
		tuple<double, double> clocks;
		
		auto& [clock, drift] = clocks;
		
		clock = doc[REMOTE_CLK]			.get_double();
		drift = doc[REMOTE_CLK_DRIFT]	.get_double();
		
		string str = doc[REMOTE_STR].get_utf8().value.to_string();
		
		predictedClkMap[str][time] = clocks;
	}
	
	return predictedClkMap;
}

void mongoReadFilter(
	KFState&				kfState,
	GTime					time,
	const vector<KF>&		types,
	const string&			Sat,
	const string&			str,
	bool					remote)
{
	Mongo* mongo_ptr;
	
	if (remote)	mongo_ptr = remoteMongo_ptr;
	else		mongo_ptr = localMongo_ptr;
	
	if (mongo_ptr == nullptr)
	{
		MONGO_NOT_INITIALISED_MESSAGE;
		return;
	}
	
	string database;
	if (remote)	database = acsConfig.remoteMongo.database;
	else		database = acsConfig.localMongo	.database;
	
	Mongo&						mongo	= *mongo_ptr;
	auto 						c		= mongo.pool.acquire();
	mongocxx::client&			client	= *c;
	mongocxx::database			db		= client[database];
	mongocxx::collection		coll	= db[STATES_DB];
	
	b_date btime{std::chrono::system_clock::from_time_t((time_t)((PTime)time).bigTime)};
	
	// Find the latest document according to t0_time.
	auto docMatch		= document{};
	auto docGroup		= document{};
	
	if (types.empty() == false)
	{
		auto array = docMatch << "$or" 
								<< open_array;
		
		for (auto& type : types)
		{
			array	<< open_document
						<< "State"	<< type._to_string()
					<< close_document;
		}
		
		array	<< close_array;
	}
									docMatch << "Series"		<< "";
	if (str.empty() == false)		docMatch << "Site"			<< str;
	if (Sat.empty() == false)		docMatch << "Sat"			<< Sat;
	if (time != GTime::noTime())	docMatch << "Epoch"					//todo aaron, convert these to #defines
												<< open_document
													<< "$lte" << btime
												<< close_document;
		
	docGroup	<< "_id"
					<< open_document
						<< "State"	<< "$State"
						<< "Sat"	<< "$Sat"
						<< "Site"	<< "$Site"
					<< close_document
				<< "x"		<< open_document << "$last" << "$x"		<< close_document
				<< "sigma"	<< open_document << "$last" << "$sigma" << close_document
				<< "Num"	<< open_document << "$last" << "$Num"	<< close_document;
				
	mongocxx::pipeline p;
	p.match(docMatch.view());
	p.group(docGroup.view());
	
// 	std::cout <<  std::endl << bsoncxx::to_json(docMatch);
// 	std::cout <<  std::endl << bsoncxx::to_json(docGroup);
	
	auto cursor = coll.aggregate(p, mongocxx::options::aggregate{});
	
	int index = 1;
	vector<double> x = {1};
	vector<double> P = {0};
	
	for (auto doc : cursor)
	{
// 		std::cout <<  std::endl << bsoncxx::to_json(doc);
		
		KFKey kfKey;
		kfKey.type	= KF::_from_string(	doc["_id"]["State"]	.get_utf8().value.to_string().c_str());
		kfKey.Sat	= SatSys(			doc["_id"]["Sat"]	.get_utf8().value.to_string().c_str());
		kfKey.str	= 					doc["_id"]["Site"]	.get_utf8().value.to_string();
		
		int i = 0;
		for (auto thing : doc["Num"].get_array().value)
		{
			kfKey.num	= 		doc["Num"]	.get_array().value[i].get_int32();
			
			x.push_back(		doc["x"]	.get_array().value[i].get_double());
			P.push_back(SQR(	doc["sigma"].get_array().value[i].get_double()));
			
			kfState.kfIndexMap[kfKey] = index;
			index++;
			i++;
		}
	}
	
	kfState.x	= VectorXd		(x.size());
	kfState.dx	= VectorXd::Zero(x.size());
	kfState.P	= MatrixXd::Zero(P.size(), P.size());
	
	for (int i = 0; i < x.size(); i++)
	{
		kfState.x(i)	= x[i];
		kfState.P(i,i)	= P[i];
	}
	
// 	kfState.outputStates(std::cout);
}
