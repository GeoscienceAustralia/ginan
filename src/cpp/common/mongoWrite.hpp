#pragma once

#ifdef ENABLE_MONGODB

#include <set>
#include "common/gTime.hpp"
#include "common/mongo.hpp"
#include "common/satSys.hpp"
#include "common/trace.hpp"

using std::set;

struct MongoOptions;
struct ReceiverMap;
struct OrbitState;
struct KFState;
struct KFMeas;
struct Geph;
struct Eph;

struct TestStatistics
{
    int    numMeas          = 0;
    double sumOfSquaresLsq  = 0;
    double sumOfSquaresPre  = 0;
    double sumOfSquaresPost = 0;
    double averageRatioLsq  = 0;
    double averageRatioPre  = 0;
    double averageRatioPost = 0;
    double chiSq            = 0;
    double dof              = 0;
    double chiSqPerDof      = 0;
    double qc               = 0;
};

void mongoMeasResiduals(
    const GTime& time,
    KFMeas&      kfMeas,
    bool         queue  = false,
    string       suffix = "",
    int          beg    = 0,
    int          num    = -1
);

void mongoTrace(const vector<string>& jsons, bool queue = false);

void mongoOutputConfig(string& config);

struct MongoStatesOptions
{
    string  suffix;
    string  collection = Constants::Mongo::STATES_DB;
    E_Mongo instances;
    bool    force  = false;
    bool    upsert = false;
    bool    queue  = false;
    bool    index  = true;
    GTime   updated;
};

void mongoStatesAvailable(GTime time, MongoStatesOptions opts = {});

void mongoStates(KFState& kfState, MongoStatesOptions opts = {});

void mongoMeasSatStat(ReceiverMap& receiverMap);

void mongoTestStat(KFState& kfState, TestStatistics& statistics);

void mongoCull(GTime time);

void mongoEditing(
    const std::string& sat,
    const std::string& site,
    const GTime&       time,
    const std::string& type,
    const std::string& signal,
    const double&      values,
    const std::string& message = ""
);

#else  // !ENABLE_MONGODB

// Stub declarations when MongoDB is disabled
#include <string>
#include <vector>
#include "common/gTime.hpp"
#include "common/mongo.hpp"  // For mongoooo() and other mongo functions
#include "enums.h"

struct ReceiverMap;
struct KFState;
struct KFMeas;

struct TestStatistics
{
    int    numMeas          = 0;
    double sumOfSquaresLsq  = 0;
    double sumOfSquaresPre  = 0;
    double sumOfSquaresPost = 0;
    double averageRatioLsq  = 0;
    double averageRatioPre  = 0;
    double averageRatioPost = 0;
    double chiSq            = 0;
    double dof              = 0;
    double chiSqPerDof      = 0;
    double qc               = 0;
};

struct MongoStatesOptions
{
    std::string suffix;
    std::string collection = "States";
    E_Mongo     instances;
    bool        force  = false;
    bool        upsert = false;
    bool        queue  = false;
    bool        index  = true;
    GTime       updated;
};

inline void mongoMeasResiduals(
    const GTime& time,
    KFMeas&      kfMeas,
    bool         queue  = false,
    std::string  suffix = "",
    int          beg    = 0,
    int          num    = -1
)
{
}

inline void mongoTrace(const std::vector<std::string>& jsons, bool queue = false) {}

inline void mongoOutputConfig(std::string& config) {}

inline void mongoStatesAvailable(GTime time, MongoStatesOptions opts = {}) {}

inline void mongoStates(KFState& kfState, MongoStatesOptions opts = {}) {}

inline void mongoMeasSatStat(ReceiverMap& receiverMap) {}

inline void mongoTestStat(KFState& kfState, TestStatistics& statistics) {}

inline void mongoCull(GTime time) {}

inline void mongoEditing(
    const std::string& sat,
    const std::string& site,
    const GTime&       time,
    const std::string& type,
    const std::string& signal,
    const int&         values,
    const std::string& message = ""
)
{
}

inline void mongoEditing(
    const std::string& sat,
    const std::string& site,
    const GTime&       time,
    const std::string& type,
    const std::string& signal,
    const double&      values,
    const std::string& message = ""
)
{
}

// Stub for prepareSsrStates which is declared in ssr.hpp but defined in mongoWrite.cpp
#include "common/trace.hpp"  // For Trace typedef
inline void prepareSsrStates(Trace& trace, KFState& kfState, KFState& ionState, GTime time)
{
    // No-op when MongoDB is disabled
}

#endif                       // ENABLE_MONGODB
