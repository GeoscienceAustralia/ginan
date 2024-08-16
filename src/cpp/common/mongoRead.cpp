
// #pragma GCC optimize ("O0")

#include <deque>

using std::deque;

#include "streamRtcm.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "mongoRead.hpp"
#include "common.hpp"

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

/** Read orbits and clocks from Mongo DB
*/
SsrOutMap mongoReadOrbClk(
	GTime		referenceTime,		///< reference time (t0) of SSR correction
	SSRMeta&	ssrMeta,			///< SSR message metadata
	int			masterIod,			///< IOD SSR
	E_Sys		targetSys)			///< target system
{
	SsrOutMap ssrOutMap;

	for (auto instance : {E_Mongo::PRIMARY, E_Mongo::SECONDARY})
	{
		Mongo* mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		getMongoCollection(mongo, SSR_DB);

	// 	std::cout << "\nTrying to get things for " << targetTime.to_string(0) << "\n";

		b_date btime = bDate(referenceTime);

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

	// 			fout << bsoncxx::to_json(doc) << "\n";

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

	// 					std::cout << Sat.id() << " less:" << less << " brdc:" << broadcast << "   " << clkValues.time.to_string(0) << "\n";

						if (less)	clkVec	.push_front	(clkValues);
						else		clkVec	.push_back	(clkValues);

						continue;
					}
				}
			}

	// 	for (auto& a : clkBroadcastVec)
	// 	{
	// 		std::cout << Sat.id() << "Final cbrdcs:" << " iode: " << a.iode <<  " "<< a.time.to_string(0) << "\n";
	// 	}
	// 	for (auto& a : clkPreciseVec)
	// 	{
	// 		std::cout << Sat.id() << "Final cprecs:" << " iode: " << a.iode <<  " "<< a.time.to_string(0) << "\n";
	// 	}
	// 	for (auto& a : ephBroadcastVec)
	// 	{
	// 		std::cout << Sat.id() << "Final ebrdcs:" << " iode: " << a.iode <<  " "<< a.time.to_string(0) << "\n";
	// 	}
	// 	for (auto& a : ephPreciseVec)
	// 	{
	// 		std::cout << Sat.id() << "Final eprecs:" << " iode: " << a.iode <<  " "<< a.time.to_string(0) << "\n";
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
	}

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

	for (auto instance : {E_Mongo::PRIMARY, E_Mongo::SECONDARY})
	{
		auto mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		auto sats = getSysSats(targetSys);

		getMongoCollection(mongo, SSR_DB);

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

	for (auto instance : {E_Mongo::PRIMARY, E_Mongo::SECONDARY})
	{
		Mongo* mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		auto sats = getSysSats(targetSys);

		getMongoCollection(mongo, SSR_DB);

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

	for (auto instance : {E_Mongo::PRIMARY, E_Mongo::SECONDARY})
	{
		Mongo* mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		getMongoCollection(mongo, "Ephemeris");

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

	for (auto instance : {E_Mongo::PRIMARY, E_Mongo::SECONDARY})
	{
		Mongo* mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		getMongoCollection(mongo, "Ephemeris");

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
	}

	return geph;
}

SSRAtm mongoReadCmpAtmosphere(
	GTime		time,
	SSRMeta		ssrMeta)
{
	SSRAtm ssrAtm;

	for (auto instance : {E_Mongo::PRIMARY, E_Mongo::SECONDARY})
	{
		Mongo* mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		ssrAtm.ssrMeta = ssrMeta;

		getMongoCollection(mongo, SSR_DB);

		b_date btime = bDate(time);

		auto regDoc		= document{}	<< SSR_DATA		<< CMP_ATM_META
										<< finalize;

		auto regSort	= document{}	<< "RegionID" 		<< 1
										<< finalize;

		auto regnOpts 	= mongocxx::options::find{};
		regnOpts.sort(regSort.view());

		auto cursor  	= coll.find(regDoc.view(), regnOpts);

		for (auto regDoc : cursor)
		{
			int reg = regDoc["RegionID"	].get_int32();
			if (reg < 0 || reg > 31)
				continue;

			SSRAtmRegion& atmRegion = ssrAtm.atmosRegionsMap[reg];
			atmRegion.regionDefIOD	= regDoc["RegionIOD"	].get_int32();

			atmRegion.minLatDeg		= regDoc["minLat"	].get_double();
			atmRegion.maxLatDeg		= regDoc["maxLat"	].get_double();
			atmRegion.intLatDeg		= regDoc["intLat"	].get_double();
			atmRegion.minLonDeg		= regDoc["minLon"	].get_double();
			atmRegion.maxLonDeg		= regDoc["maxLon"	].get_double();
			atmRegion.intLonDeg		= regDoc["intLon"	].get_double();

			atmRegion.gridType		= regDoc["gridType"	].get_int32();
			atmRegion.tropPolySize	= regDoc["tropPoly"	].get_int32();
			atmRegion.ionoPolySize	= regDoc["ionoPoly"	].get_int32();
			atmRegion.tropGrid		= regDoc["tropGrid"	].get_int32();
			atmRegion.ionoGrid		= regDoc["ionoGrid"	].get_int32();

			atmRegion.gridLatDeg[0] = regDoc["grdLat_0"].get_double();
			atmRegion.gridLonDeg[0] = regDoc["grdLon_0"].get_double();

			if (atmRegion.gridType==0)
			{
				int nGrid			= regDoc["gridNum"	].get_int32();
				string keyStr;
				for (int i=0; i < nGrid; i++)
				{
					keyStr = "grdLat_" + std::to_string(i);
					atmRegion.gridLatDeg[i] = regDoc[keyStr].get_double();

					keyStr = "grdLon_" + std::to_string(i);
					atmRegion.gridLonDeg[i] = regDoc[keyStr].get_double();
				}
			}
			if (atmRegion.gridType==1)
			{
				int latNgrid = ROUND((atmRegion.maxLatDeg - atmRegion.minLatDeg)/atmRegion.intLatDeg);
				int lonNgrid = ROUND((atmRegion.maxLonDeg - atmRegion.minLonDeg)/atmRegion.intLonDeg);
				int nind=0;
				for (int i = 0; i <= latNgrid; i++)
				for (int j = 0; j <= lonNgrid; j++)
				{
					atmRegion.gridLatDeg[nind] = atmRegion.gridLatDeg[0] - atmRegion.intLatDeg*i;
					atmRegion.gridLonDeg[nind] = atmRegion.gridLonDeg[0] + atmRegion.intLonDeg*j;
					nind++;
				}
			}
			if (atmRegion.gridType==2)
			{
				int latNgrid = ROUND((atmRegion.maxLatDeg - atmRegion.minLatDeg)/atmRegion.intLatDeg);
				int lonNgrid = ROUND((atmRegion.maxLonDeg - atmRegion.minLonDeg)/atmRegion.intLonDeg);
				int nind=0;
				for (int j = 0; j <= lonNgrid; j++)
				for (int i = 0; i <= latNgrid; i++)
				{
					atmRegion.gridLatDeg[nind] = atmRegion.gridLatDeg[0] + atmRegion.intLatDeg*i;
					atmRegion.gridLonDeg[nind] = atmRegion.gridLonDeg[0] + atmRegion.intLonDeg*j;
					nind++;
				}
			}
		}

		if (ssrAtm.atmosRegionsMap.empty())
			return ssrAtm;

		for (auto& [regId,regData] : ssrAtm.atmosRegionsMap)
		{
			auto trpSel		= document{}	<< SSR_DATA		<< CMP_TRP_ENTRY
											<< "RegionID"	<< regId
											<< SSR_EPOCH
											<< open_document
											<< "$lt" << btime
											<< close_document
											<< finalize;

			auto trpSort	= document{}	<< SSR_EPOCH 		<< 1
											<< finalize;

			auto trpOpts 	= mongocxx::options::find{};
			trpOpts.sort(trpSort.view());
			trpOpts.limit(1);

			auto trpDocs  	= coll.find(trpSel.view(), trpOpts);

			for (auto atmDoc : trpDocs)
			{
				PTime t0;
				auto tp					= atmDoc[SSR_EPOCH		].get_date();
				t0.bigTime 				= std::chrono::system_clock::to_time_t(tp);

				GTime tatm = t0;
				if (abs((time-tatm).to_double()) > 600)
					continue;

				regData.tropData[tatm].sigma = atmDoc["trpAcc"].get_double();

				for (int i = 0; i < regData.tropPolySize; i++)
				{
					string keyStr = "tropPoly_"+std::to_string(i);
					regData.tropData[tatm].polyDry[i] = atmDoc[keyStr].get_double();
				}

				string keyStr;
				if (regData.tropGrid)
				for (auto& [ind, lat] : regData.gridLatDeg)
				{
					keyStr = "tropDry_" + std::to_string(ind);
					regData.tropData[tatm].gridDry[ind] = atmDoc[keyStr].get_double();

					keyStr = "tropWet_" + std::to_string(ind);
					regData.tropData[tatm].gridWet[ind] = atmDoc[keyStr].get_double();
				}
			}

			auto ionSel		= document{}	<< SSR_DATA		<< CMP_ION_META
											<< "RegionID"	<< regId
											<< SSR_EPOCH
											<< open_document
											<< "$lt" << btime
											<< close_document
											<< finalize;

			auto ionSort	= document{}	<< SSR_EPOCH 		<< 1
											<< finalize;

			auto ionOpts 	= mongocxx::options::find{};
			ionOpts.sort(ionSort.view());
			ionOpts.limit(1);

			auto ionDocs  	= coll.find(ionSel.view(), trpOpts);
			map<int,SatSys> regSat;
			for (auto atmDoc : ionDocs)
			{
				PTime t0;
				auto tp					= atmDoc[SSR_EPOCH		].get_date();
				t0.bigTime 				= std::chrono::system_clock::to_time_t(tp);

				GTime tatm = t0;
				if (abs((time-tatm).to_double()) > 600)
					continue;

				int nSat			= atmDoc["satNumb"	].get_int32();
				for (int i=0; i<nSat; i++)
				{
					string keyStr		= "regSat_" + std::to_string(i);
					auto strView		= atmDoc[keyStr].get_utf8().value;
					string satStr 		= strView.to_string();
					SatSys Sat(satStr.c_str());

					regSat[i] = Sat;
				}
			}

			for (auto& [iSat,sat] : regSat)
			{
				auto satSel		= document{}	<< SSR_DATA		<< CMP_ION_ENTRY
												<< "RegionID"	<< regId
												<< SSR_SAT		<< sat.id()
												<< SSR_EPOCH
												<< open_document
												<< "$lt" << btime
												<< close_document
												<< finalize;

				auto satSort	= document{}	<< SSR_EPOCH 	<< 1
												<< finalize;

				auto satOpts 	= mongocxx::options::find{};
				satOpts.sort(satSort.view());
				satOpts.limit(1);

				auto satDocs  	= coll.find(satSel.view(), satOpts);
				for (auto satDoc : satDocs)
				{
					PTime t0;
					auto tp					= satDoc[SSR_EPOCH		].get_date();
					t0.bigTime 				= std::chrono::system_clock::to_time_t(tp);

					GTime tatm = t0;
					if (abs((time-tatm).to_double()) > 600)
						continue;

					regData.stecData[sat][tatm].iod		= regData.regionDefIOD;
					regData.stecData[sat][tatm].sigma	= satDoc["ionAcc"].get_double();

					for (int i = 0; i < regData.ionoPolySize; i++)
					{
						string keyStr = "ionoPoly_" + std::to_string(i);
						regData.stecData[sat][tatm].poly[i] = satDoc[keyStr].get_double();
						tracepdeex (6,std::cout,"\n   Mongo_ionP  %s %2d %s %1d %8.4f", tatm.to_string().c_str(), regId, sat.id().c_str(), i, regData.stecData[sat][tatm].poly[i]);
					}

					if (regData.ionoGrid)
					for (auto& [ind, lat] : regData.gridLatDeg)
					{
						string keyStr = "ionoGrid_" + std::to_string(ind);
						regData.stecData[sat][tatm].grid[ind] = satDoc[keyStr].get_double();
						tracepdeex (6,std::cout,"\n   Mongo_ionG  %s %2d %s %1d %8.4f", tatm.to_string().c_str(), regId, sat.id().c_str(), ind, regData.stecData[sat][tatm].grid[ind]);
					}
				}
			}
		}
	}

	return ssrAtm;
}

SSRAtm mongoReadIGSIonosphere(
			GTime		time,
	const	SSRMeta&	ssrMeta,
			int			masterIod)
{
	SSRAtm ssrAtm;

	for (auto instance : {E_Mongo::PRIMARY, E_Mongo::SECONDARY})
	{
		Mongo* mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		ssrAtm.ssrMeta = ssrMeta;

		getMongoCollection(mongo, SSR_DB);

		b_date btime = bDate(time);

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
	}

	return ssrAtm;
}

void mongoReadFilter(
	KFState&				kfState,
	GTime					time,
	const vector<KF>&		types,
	const string&			Sat,
	const string&			str)
{
	for (auto instance : {E_Mongo::PRIMARY, E_Mongo::SECONDARY})
	{
		Mongo* mongo_ptr = mongo_ptr_arr[instance];

		if (mongo_ptr == nullptr)
			continue;

		auto& mongo = *mongo_ptr;

		getMongoCollection(mongo, STATES_DB);

		b_date btime = bDate(time);

		// get latest available from collection before next step

		auto docMatch	= document();
		auto docSort	= document();
		auto docProject	= document();
		auto docGroup	= document{};

										docMatch	<< MONGO_TYPE		<< MONGO_AVAILABLE;
		if (time != GTime::noTime())	docMatch	<< MONGO_EPOCH
													<< open_document
														<< "$lte" << btime
													<< close_document;


		docSort		<< MONGO_EPOCH		<< -1;
		docSort		<< MONGO_UPDATED	<< -1;

		docProject	<< "_id"		<< 0;
		docProject	<< MONGO_TYPE	<< 0;

		auto findOpts 	= mongocxx::options::find();
		findOpts.sort		(docSort	.view());
		findOpts.projection	(docProject	.view());
//
		auto matchTemplate = coll.find_one(docMatch.view(), findOpts);

		if (!matchTemplate)
		{
			continue;
		}

		//start a new match
		auto docMatch2		= document();
		auto docProject2	= document();

		docProject2	<< MONGO_EPOCH		<< 0;
		docProject2	<< MONGO_UPDATED	<< 0;
		docProject2	<< MONGO_SERIES		<< 0;
		docProject2	<< MONGO_DX			<< 0;
		docProject2	<< "_id"			<< 0;

		if (std::find(types.begin(), types.end(), +KF::ALL) == types.end())
		{
			auto array = docMatch2 << "$or"
									<< open_array;

			for (auto& type : types)
			{
				array	<< open_document
							<< MONGO_STATE	<< type._to_string()
						<< close_document;
			}

			array	<< close_array;
		}

		auto updateDoc = matchTemplate->view();


		PTime pTime;
		auto time		= updateDoc[MONGO_EPOCH].get_date();
		pTime.bigTime	= std::chrono::system_clock::to_time_t(time);

		kfState.time = pTime;

		time			= updateDoc[MONGO_UPDATED].get_date();
		pTime.bigTime	= std::chrono::system_clock::to_time_t(time);

		std::cout <<  "\n" << bsoncxx::to_json(updateDoc) << "\n";

										docMatch2 << MONGO_EPOCH		<< updateDoc[MONGO_EPOCH]	.get_date();
										docMatch2 << MONGO_UPDATED		<< updateDoc[MONGO_UPDATED]	.get_date();
										docMatch2 << MONGO_SERIES		<< "_predicted";
		if (str.empty() == false)		docMatch2 << MONGO_STR			<< str;
		if (Sat.empty() == false)		docMatch2 << MONGO_SAT			<< Sat;

		// std::cout <<  "\n" << bsoncxx::to_json(docMatch2.view()) << "\n";

		mongocxx::pipeline p;
		p.match		(docMatch2	.view());
		p.project	(docProject2.view());
		// p.group	(docGroup	.view());

	// 	std::cout <<  "\n" << bsoncxx::to_json(docMatch);
	// 	std::cout <<  "\n" << bsoncxx::to_json(docGroup);

		auto cursor = coll.aggregate(p);

		vector<double> x;
		vector<double> P;
		for (int i = 0; i < kfState.x.rows(); i++)
		{
			x.push_back(kfState.x(i));
			P.push_back(kfState.P(i,i));
		}

		int index = x.size();

		for (auto doc : cursor)
		{
			// std::cout << bsoncxx::to_json(doc) <<  "\n";

			KFKey kfKey;
			kfKey.type	= KF::_from_string(	doc[MONGO_STATE].get_utf8().value.to_string().c_str());
			kfKey.Sat	= SatSys(			doc[MONGO_SAT]	.get_utf8().value.to_string().c_str());
			kfKey.str	= 					doc[MONGO_STR]	.get_utf8().value.to_string();

			int i = 0;
			for (auto thing : doc[MONGO_NUM].get_array().value)
			{
				kfKey.num	= 		doc[MONGO_NUM]	.get_array().value[i].get_int32();

				x.push_back(		doc[MONGO_X]	.get_array().value[i].get_double());
				P.push_back(SQR(	doc[MONGO_SIGMA].get_array().value[i].get_double()));

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
	}
}
