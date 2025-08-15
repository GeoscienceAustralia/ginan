#pragma once

#include <array>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/log/sinks/basic_sink_backend.hpp>
#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/builder/stream/array.hpp>
#include <bsoncxx/builder/stream/document.hpp>
#include <bsoncxx/builder/stream/helpers.hpp>
#include <bsoncxx/json.hpp>
#include <deque>
#include <map>
#include <mongocxx/client.hpp>
#include <mongocxx/instance.hpp>
#include <mongocxx/pool.hpp>
#include <mongocxx/uri.hpp>
#include <string>
#include <tuple>
#include <vector>
#include "common/enums.h"

namespace sinks = boost::log::sinks;

using bsoncxx::builder::stream::close_array;
using bsoncxx::builder::stream::close_document;
using bsoncxx::builder::stream::document;
using bsoncxx::builder::stream::finalize;
using bsoncxx::builder::stream::open_array;
using bsoncxx::builder::stream::open_document;
using bsoncxx::types::b_date;
using std::array;
using std::deque;
using std::map;
using std::string;
using std::tuple;
using std::vector;

struct DBEntry;
struct GTime;

struct Mongo
{
    static mongocxx::instance instance;  // This should be done only once.
    mongocxx::uri             uri;
    mongocxx::pool            pool;
    string                    database;

    Mongo(string uriString) : uri{uriString}, pool{uri} {}
};

#define SSR_DB "SSRData"

#define SSR_DATA "Data"
#define SSR_PHAS_BIAS "PBias"
#define SSR_CODE_BIAS "CBias"
#define SSR_EPHEMERIS "Eph"
#define SSR_CLOCK "Clk"

#define IGS_ION_META "igsSSRMeta"
#define IGS_ION_ENTRY "igsSSREntry"
#define IGS_ION_DCB "igsSSRDCB"
#define CMP_ATM_META "cmpSSRMeta"
#define CMP_ION_META "cmpIonMeta"
#define CMP_TRP_ENTRY "cmpTrpEntry"
#define CMP_ION_ENTRY "cmpIonEntry"

#define SSR_EPOCH "Epoch"
#define SSR_UPDATED "Updated"
#define SSR_SAT "Sat"
#define SSR_IODE "Iode"
#define SSR_POS "Pos"
#define SSR_VEL "Vel"
#define SSR_OBSCODE "ObsCode"
#define SSR_BIAS "Bias"
#define SSR_VAR "Var"

#define IGS_ION_NLAY "ionoMetNlay"
#define IGS_ION_NBAS "ionoMetNbas"
#define IGS_ION_QLTY "ionoMetQlty"
#define SSR_ION_IND "ionoBasInd"
#define IGS_ION_HGT "ionoBasHgt"
#define IGS_ION_DEG "ionoBasDeg"
#define IGS_ION_ORD "ionoBasOrd"
#define IGS_ION_PAR "ionoBasPar"
#define IGS_ION_VAL "ionoBasVal"

#define SSR_BRDC "Brdc"
#define SSR_PREC "Prec"

#define REMOTE_DATA_DB "Remote"

#define REMOTE_DATA "Data"
#define REMOTE_EPOCH "Epoch"
#define REMOTE_ORBIT "Orbit"
#define REMOTE_CLOCK "Clock"
#define REMOTE_SAT "Sat"
#define REMOTE_POS "Pos"
#define REMOTE_VEL "Vel"
#define REMOTE_VAR "Var"
#define REMOTE_CLK "Clk"
#define REMOTE_CLK_DRIFT "ClkRate"
#define REMOTE_STR "Str"

#define MONGO_UPDATED "Updated"

#define MONGO_AVAILABLE "Available"

// @todo seb put all define as const char* in a namespace

namespace Constants
{
/**
 * @namespace Mongo
 * @brief Contains constant string literals used for MongoDB database and field names.
 *
 * This namespace provides a collection of constant string literals that represent
 * database names and variable/field names commonly used in MongoDB operations.
 *
 */
namespace Mongo
{
constexpr const char* EDITING_DB      = "Editing";
constexpr const char* MEASUREMENTS_DB = "Measurements";
constexpr const char* STATE_DB        = "State";
constexpr const char* STATES_DB       = "States";  ///@todo are STATE_DB and STATES_DB the same?
constexpr const char* CONFIG_DB       = "Config";
constexpr const char* TRACE_DB        = "Trace";
constexpr const char* GEOMETRY_DB     = "Geometry";
constexpr const char* CONTENT_DB      = "Content";

constexpr const char* DX_VAR        = "dx";
constexpr const char* NUM_VAR       = "Num";
constexpr const char* X_VAR         = "x";
constexpr const char* SIGMA_VAR     = "sigma";
constexpr const char* COVAR_VAR     = "Covar";
constexpr const char* AZIMUTH_VAR   = "Azimuth";
constexpr const char* ELEVATION_VAR = "Elevation";
constexpr const char* NADIR_VAR     = "Nadir";
constexpr const char* SERIES_VAR    = "Series";
constexpr const char* EPOCH_VAR     = "Epoch";
constexpr const char* SAT_VAR       = "Sat";
constexpr const char* STR_VAR       = "Site";
constexpr const char* TYPE_VAR      = "Type";
constexpr const char* VALUE_VAR     = "Values";

};  // namespace Mongo
};  // namespace Constants

/**
 * @brief Converts a C-style string (const char*) to a std::string.
 *
 * @param cstr A pointer to a null-terminated C-style string.
 * @return A std::string object containing the same characters as the input C-style string.
 */
inline std::string toString(const char* cstr)
{
    return std::string(cstr);
}

b_date bDate(const GTime& time);

struct MongoLogSinkBackend
    : public sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>
{
    // The function consumes the log records that come from the frontend
    void consume(
        boost::log::record_view const& rec,
        sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type const&
            log_string
    );
};

void mongoooo();

vector<E_Mongo> mongoInstances(E_Mongo selection);

bool startNewMongoDb(
    const string&            id,
    boost::posix_time::ptime logptime,
    string                   new_database,
    E_Mongo                  instance
);

string formatSeries(string series);

document entryToDocument(DBEntry& entry, bool type);

#define getMongoCollection(MONGO, COLLECTION)           \
    auto               c      = MONGO.pool.acquire();   \
    mongocxx::client&  client = *c;                     \
    mongocxx::database db     = client[MONGO.database]; \
    auto               coll   = db[COLLECTION];

extern array<Mongo*, 3> mongo_ptr_arr;

#define MONGO_NOT_INITIALISED_MESSAGE                                                              \
    BOOST_LOG_TRIVIAL(warning) << "Mongo actions requested but mongo is not available - check it " \
                                  "is enabled and connected correctly"
