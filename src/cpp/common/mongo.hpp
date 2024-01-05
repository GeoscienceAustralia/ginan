
#pragma once


#include <bsoncxx/builder/basic/document.hpp>

#include <bsoncxx/builder/stream/document.hpp>
#include <bsoncxx/builder/stream/helpers.hpp>
#include <bsoncxx/builder/stream/array.hpp>
#include <bsoncxx/json.hpp>

#include <mongocxx/instance.hpp>
#include <mongocxx/client.hpp>
#include <mongocxx/pool.hpp>
#include <mongocxx/stdx.hpp>
#include <mongocxx/uri.hpp>

#include <boost/log/sinks/basic_sink_backend.hpp>

#include <string>
#include <vector>
#include <deque>
#include <tuple>
#include <array>
#include <map>


using std::string;
using std::vector;
using std::deque;
using std::tuple;
using std::map;


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

using bsoncxx::builder::stream::close_array;
using bsoncxx::builder::stream::close_document;
using bsoncxx::builder::stream::document;
using bsoncxx::builder::stream::finalize;
using bsoncxx::builder::stream::open_array;
using bsoncxx::builder::stream::open_document;

namespace sinks = boost::log::sinks;


struct Mongo
{
	static mongocxx::instance		instance; 	// This should be done only once.
	mongocxx::uri					uri;
	mongocxx::pool					pool;
	string							database;

	Mongo(string uriString) : uri{uriString}, pool{uri}
	{

	}
};

#define SSR_DB				"SSRData"

#define SSR_DATA			"Data"
#define SSR_PHAS_BIAS		"PBias"
#define SSR_CODE_BIAS		"CBias"
#define SSR_EPHEMERIS		"Eph"
#define SSR_CLOCK			"Clk"

#define IGS_ION_META		"igsSSRMeta"
#define IGS_ION_ENTRY		"igsSSREntry"
#define IGS_ION_DCB			"igsSSRDCB"
#define CMP_ATM_META		"cmpSSRMeta"
#define CMP_ION_META		"cmpIonMeta"
#define CMP_TRP_ENTRY		"cmpTrpEntry"
#define CMP_ION_ENTRY		"cmpIonEntry"

#define SSR_EPOCH			"Epoch"
#define SSR_UPDATED			"Updated"
#define SSR_SAT	 			"Sat"
#define SSR_IODE			"Iode"
#define SSR_POS				"Pos"
#define SSR_VEL				"Vel"
#define SSR_OBSCODE			"ObsCode"
#define SSR_BIAS			"Bias"
#define SSR_VAR				"Var"


#define IGS_ION_NLAY		"ionoMetNlay"
#define IGS_ION_NBAS		"ionoMetNbas"
#define IGS_ION_QLTY		"ionoMetQlty"
#define SSR_ION_IND			"ionoBasInd"
#define IGS_ION_HGT			"ionoBasHgt"
#define IGS_ION_DEG			"ionoBasDeg"
#define IGS_ION_ORD			"ionoBasOrd"
#define IGS_ION_PAR			"ionoBasPar"
#define IGS_ION_VAL			"ionoBasVal"

#define SSR_BRDC			"Brdc"
#define SSR_PREC			"Prec"

#define REMOTE_DATA_DB		"Remote"
#define STATES_DB			"States"

#define REMOTE_DATA			"Data"
#define REMOTE_EPOCH		"Epoch"
#define REMOTE_ORBIT		"Orbit"
#define REMOTE_CLOCK		"Clock"
#define REMOTE_SAT			"Sat"
#define REMOTE_UPDATED		"Updated"
#define REMOTE_POS			"Pos"
#define REMOTE_VEL			"Vel"
#define REMOTE_VAR			"Var"
#define REMOTE_CLK			"Clk"
#define REMOTE_CLK_DRIFT	"ClkRate"
#define REMOTE_STR			"Str"

struct MongoLogSinkBackend : public sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>
{
	// The function consumes the log records that come from the frontend
	void consume(
		boost::log::record_view																	const&	rec,
		sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string);
};

void mongoooo();

vector<E_Mongo> mongoInstances(
	E_Mongo		selection);

bool startNewMongoDb(
	const string&				id,
	boost::posix_time::ptime	logptime,
	string  					new_database,
	E_Mongo						instance);

document entryToDocument(
	DBEntry&	entry,
	bool		type);


extern array<Mongo*, 3>	mongo_ptr_arr;


#define MONGO_NOT_INITIALISED_MESSAGE BOOST_LOG_TRIVIAL(warning)	<< "Mongo actions requested but mongo is not available - check it is enabled and connected correctly"


