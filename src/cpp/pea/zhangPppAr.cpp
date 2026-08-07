#include "pea/zhangPppAr.hpp"

#include <algorithm>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <limits>
#include <map>
#include <set>
#include <sstream>
#include <tuple>
#include <vector>
#include <boost/log/trivial.hpp>
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/constants.hpp"
#include "common/observations.hpp"
#include "common/phaseClockOsb.hpp"
#include "common/receiver.hpp"
#include "common/satStat.hpp"
#include "common/trace.hpp"
#include "common/zhangPhaseContinuity.hpp"
#include "common/zhangIntegerAudit.hpp"
#include "common/zhangSatelliteDatum.hpp"
#include "common/zhangFactorCapture.hpp"
#include "common/zhangIntegerTargets.hpp"
#include "common/zhangUserTarget.hpp"
#include "orbprop/coordinates.hpp"
#include "pea/zhangReference.hpp"
#include "rtklib/lambda.h"

using std::map;
using std::set;
using std::string;
using std::tuple;
using std::vector;

namespace
{
struct ProductKey
{
    SatSys    satellite;
    E_ObsCode observable = E_ObsCode::NONE;

    bool operator<(const ProductKey& other) const
    {
        return std::tie(satellite, observable) <
               std::tie(other.satellite, other.observable);
    }
};

struct GlobalContinuityState
{
    int    counter = 0;
    int    datumVersion = 0;
    GTime  validFrom;
    int    iod = 0;
    string resetReason = "initial";
    int    stabilizationRemaining = 0;
};

map<ProductKey, ZhangPhaseContinuityState> continuityMap;
map<std::pair<E_Sys, E_ObsCode>, GlobalContinuityState> globalContinuityMap;
map<std::pair<E_Sys, E_ObsCode>, ZhangSatelliteDatumManager>
    satelliteDatumManagers;
map<const KFState*, ZhangFactorCaptureBuffer> e18FactorCaptureBuffers;
set<const KFState*> e18ConfiguredFactorCaptureStates;
map<const KFState*, ZhangPersistentProductDatumRegistry>
    e18PersistentProductDatumRegistries;

struct PromotionEvidenceKey
{
    E_Sys      system = E_Sys::NONE;
    E_ObsCode  observable = E_ObsCode::NONE;
    SatSys     a;
    int        segmentA = 0;
    SatSys     b;
    int        segmentB = 0;

    bool operator<(const PromotionEvidenceKey& other) const
    {
        return std::tie(system, observable, a, segmentA, b, segmentB) <
               std::tie(
                   other.system, other.observable,
                   other.a, other.segmentA, other.b, other.segmentB
               );
    }
};

struct PromotionEvidence
{
    long long difference = 0;
    long int  lastEpoch = 0;
    int       confirmations = 0;
};

map<PromotionEvidenceKey, PromotionEvidence> promotionEvidence;
map<PromotionEvidenceKey, PromotionEvidence> relinkEvidence;

ZhangSatelliteDatumManager& satelliteDatumManager(E_Sys sys, E_ObsCode code)
{
    auto key = std::make_pair(sys, code);
    auto found = satelliteDatumManagers.find(key);
    if (found == satelliteDatumManagers.end())
    {
        found = satelliteDatumManagers.emplace(
            key, ZhangSatelliteDatumManager(sys, code)
        ).first;
    }
    return found->second;
}

struct ProductLookupKey
{
    long int   epoch = 0;
    SatSys     satellite;
    E_ObsCode  observable = E_ObsCode::NONE;
    string     solution;

    bool operator<(const ProductLookupKey& other) const
    {
        return std::tie(epoch, satellite, observable, solution) <
               std::tie(other.epoch, other.satellite, other.observable, other.solution);
    }
};

map<ProductLookupKey, ZhangInternalProduct> productMap;
string loadedProductFilename;

struct ProductHistoryKey
{
    string     solution;
    SatSys     satellite;
    E_ObsCode  observable = E_ObsCode::NONE;

    bool operator<(const ProductHistoryKey& other) const
    {
        return std::tie(solution, satellite, observable) <
               std::tie(other.solution, other.satellite, other.observable);
    }
};

struct ProductHistory
{
    GTime  time;
    double correction = 0;
    int    discontinuityCounter = 0;
    int    datumVersion = 0;
};

map<ProductHistoryKey, ProductHistory> productHistoryMap;

struct UserReferenceKey
{
    const KFState* state = nullptr;
    string         receiver;
    E_Sys          sys = E_Sys::NONE;
    E_ObsCode      observable = E_ObsCode::NONE;

    bool operator<(const UserReferenceKey& other) const
    {
        return std::tie(state, receiver, sys, observable) <
               std::tie(other.state, other.receiver, other.sys, other.observable);
    }
};

struct UserReferenceState
{
    SatSys reference;
    int    productCounter = -1;
    int    datumVersion = -1;
    map<SatSys, std::pair<int, int>> satelliteDatum;
};

map<UserReferenceKey, UserReferenceState> userReferenceMap;

double wavelength(E_Sys sys, E_ObsCode code)
{
    auto sysIt = code2Freq.find(sys);
    if (sysIt == code2Freq.end())
    {
        return 0;
    }

    auto frequencyIt = sysIt->second.find(code);
    if (frequencyIt == sysIt->second.end())
    {
        return 0;
    }

    auto wavelengthIt = genericWavelength.find(frequencyIt->second);
    if (wavelengthIt == genericWavelength.end())
    {
        return 0;
    }

    return wavelengthIt->second;
}

KFKey userAmbiguityKey(const string& receiver, const SatSys& satellite, E_ObsCode code)
{
    KFKey key;
    key.type = KF::AMBIGUITY;
    key.str  = receiver;
    key.Sat  = satellite;
    key.num  = static_cast<int>(code);
    return key;
}

bool slipIsExcluded(const SigStat::SlipStat& slip)
{
    if (!slip.any)
    {
        return false;
    }

    return
        (acsConfig.exclude.LLI         && slip.LLI)        ||
        (acsConfig.exclude.GF          && slip.GF)         ||
        (acsConfig.exclude.MW          && slip.MW)         ||
        (acsConfig.exclude.SCDIA       && slip.SCDIA)      ||
        (acsConfig.exclude.retrack     && slip.retrack)    ||
        (acsConfig.exclude.single_freq && slip.singleFreq);
}

bool signalUsable(const GObs& obs, E_ObsCode code)
{
    for (const auto& [frequency, signal] : obs.sigs)
    {
        if (signal.code != code || signal.P == 0 || signal.L == 0 || signal.invalid)
        {
            continue;
        }

        if (obs.satStat_ptr)
        {
            auto slipIt = obs.satStat_ptr->sigStatMap.find(ft2string(frequency));
            if (slipIt != obs.satStat_ptr->sigStatMap.end() &&
                slipIsExcluded(slipIt->second.slip))
            {
                continue;
            }
        }

        return true;
    }

    return false;
}

void initialiseContinuityState(
    const ProductKey& key,
    ZhangPhaseContinuityState& state
)
{
    if (state.validFrom != GTime::noTime())
    {
        return;
    }

    auto& global = globalContinuityMap[{key.satellite.sys, key.observable}];
    if (global.validFrom == GTime::noTime())
    {
        global.counter = acsConfig.zhangPppAr.initial_discontinuity_counter;
    }

    state.counter                   = global.counter;
    state.datumVersion              = global.datumVersion;
    state.validFrom                 = global.validFrom;
    state.iod                       = global.iod;
    state.resetReason               = global.resetReason;
    state.stabilizationRemaining    = global.stabilizationRemaining;
}

void ensureProductFileHeader()
{
    static string initializedFilename;

    const string& filename = acsConfig.zhangPppAr.product_filename;
    if (filename.empty() || initializedFilename == filename)
    {
        return;
    }

    std::filesystem::path path(filename);
    if (path.has_parent_path())
    {
        std::filesystem::create_directories(path.parent_path());
    }

    std::ofstream output(filename, std::ios::trunc);
    output
        << "gpst_seconds,solution,satellite,observable,clock_m,clock_sigma_m,"
           "phase_m,phase_sigma_m,clock_phase_covariance_m2,correction_m,"
           "correction_sigma_m,discontinuity_counter,integer_shift_cycles,"
           "fractional_shift_cycles,datum_version,valid_from_gpst_seconds,"
           "product_iod,reset_reason,persistent_relation_known,"
           "current_alignment_state,integer_structure_valid,"
           "integer_datum_continuous,integer_precision_valid,integer_valid,"
           "integer_component_id,"
           "integer_datum_id,solution_interval_start_gpst_seconds,"
           "solution_interval_end_gpst_seconds,numeric_valid,branch_valid,"
           "continuity_valid,ppp_usable,pppar_usable,invalid_reason\n";

    initializedFilename = filename;
}

void appendProduct(const ZhangInternalProduct& product)
{
    ensureProductFileHeader();

    std::ofstream output(acsConfig.zhangPppAr.product_filename, std::ios::app);
    output << std::setprecision(17)
           << static_cast<double>(product.time.bigTime) << ","
           << product.solution << ","
           << product.satellite.id() << ","
           << enum_to_string(product.observable) << ","
           << product.clock_m << ","
           << product.clock_sigma_m << ","
           << product.phase_m << ","
           << product.phase_sigma_m << ","
           << product.clock_phase_covariance_m2 << ","
           << product.correction_m << ","
           << product.correction_sigma_m << ","
           << product.discontinuity_counter << ","
           << product.integer_shift_cycles << ","
           << product.fractional_shift_cycles << ","
           << product.datum_version << ","
           << static_cast<double>(product.valid_from.bigTime) << ","
           << product.product_iod << ","
           << product.reset_reason << ","
           << product.persistent_relation_known << ","
           << product.current_alignment_state << ","
           << product.integer_structure_valid << ","
           << product.integer_datum_continuous << ","
           << product.integer_precision_valid << ","
           << product.integer_valid << ","
           << product.integer_component_id << ","
           << product.integer_datum_id << ","
           << static_cast<double>(product.valid_from.bigTime) << ","
           << static_cast<double>(product.time.bigTime) << ","
           << product.numeric_valid << ","
           << product.branch_valid << ","
           << product.continuity_valid << ","
           << product.ppp_usable << ","
           << product.pppar_usable << ","
           << product.invalid_reason << "\n";
}

struct ProductCovarianceParameter
{
    KFKey       key;
    SatSys      satellite;
    string      parameter;
    E_ObsCode   observable = E_ObsCode::NONE;
    int         stateIndex = -1;
};

void ensureProductCovarianceFileHeader()
{
    static string initializedFilename;

    const string& filename =
        acsConfig.zhangPppAr.product_covariance_filename;
    if (filename.empty() || initializedFilename == filename)
    {
        return;
    }

    std::filesystem::path path(filename);
    if (path.has_parent_path())
    {
        std::filesystem::create_directories(path.parent_path());
    }

    std::ofstream output(filename, std::ios::trunc);
    output
        << "gpst_seconds,solution,row_satellite,row_parameter,row_observable,"
           "column_satellite,column_parameter,column_observable,covariance_m2\n";

    initializedFilename = filename;
}

void appendProductCovariance(
    const KFState& state,
    const string&  solution,
    const KFState& graphState
)
{
    const string& filename =
        acsConfig.zhangPppAr.product_covariance_filename;
    if (filename.empty())
    {
        return;
    }

    vector<ProductCovarianceParameter> parameters;
    set<SatSys> satellites;
    for (const auto& [key, index] : state.kfIndexMap)
    {
        if (key.type == KF::PHASE_BIAS &&
            key.Sat.prn > 0 &&
            key.str.empty() &&
            zhangGraphProductSatelliteActive(graphState, key.Sat) &&
            zhangPppArUsesObservable(
                key.Sat.sys,
                static_cast<E_ObsCode>(key.num)
            ))
        {
            satellites.insert(key.Sat);
        }
    }

    for (const SatSys& satellite : satellites)
    {
        KFKey clockKey;
        clockKey.type = KF::SAT_CLOCK;
        clockKey.Sat  = satellite;
        auto clockIt = state.kfIndexMap.find(clockKey);
        if (clockIt == state.kfIndexMap.end())
        {
            continue;
        }
        parameters.push_back(
            {clockKey, satellite, "CLOCK", E_ObsCode::NONE, clockIt->second}
        );

        const auto& observables =
            acsConfig.zhangPppAr.baseline_observables[satellite.sys];
        for (E_ObsCode observable : observables)
        {
            KFKey phaseKey;
            phaseKey.type = KF::PHASE_BIAS;
            phaseKey.Sat  = satellite;
            phaseKey.num  = static_cast<int>(observable);
            auto phaseIt = state.kfIndexMap.find(phaseKey);
            if (phaseIt == state.kfIndexMap.end())
            {
                continue;
            }
            parameters.push_back(
                {phaseKey, satellite, "PHASE", observable, phaseIt->second}
            );
        }
    }

    ensureProductCovarianceFileHeader();
    std::ofstream output(filename, std::ios::app);
    output << std::setprecision(17);
    for (size_t row = 0; row < parameters.size(); row++)
    {
        for (size_t column = row; column < parameters.size(); column++)
        {
            const auto& left  = parameters[row];
            const auto& right = parameters[column];
            output
                << static_cast<double>(state.time.bigTime) << ","
                << solution << ","
                << left.satellite.id() << ","
                << left.parameter << ","
                << enum_to_string(left.observable) << ","
                << right.satellite.id() << ","
                << right.parameter << ","
                << enum_to_string(right.observable) << ","
                << state.P(left.stateIndex, right.stateIndex) << "\n";
        }
    }
}

vector<string> splitCsv(const string& line)
{
    vector<string> fields;
    std::stringstream stream(line);
    string field;
    while (std::getline(stream, field, ','))
    {
        fields.push_back(field);
    }
    return fields;
}

bool loadProducts()
{
    const string& filename = acsConfig.zhangPppAr.product_filename;
    if (filename.empty())
    {
        return false;
    }
    if (loadedProductFilename == filename && !productMap.empty())
    {
        return true;
    }

    std::ifstream input(filename);
    if (!input)
    {
        BOOST_LOG_TRIVIAL(error)
            << "Unable to open Zhang internal product file " << filename;
        return false;
    }

    productMap.clear();
    string line;
    std::getline(input, line);
    while (std::getline(input, line))
    {
        auto fields = splitCsv(line);
        if (fields.size() != 19 && fields.size() != 23 &&
            fields.size() != 26 && fields.size() != 28 &&
            fields.size() != 33 && fields.size() != 34)
        {
            continue;
        }

        ZhangInternalProduct product;
        product.time.bigTime                  = std::stold(fields[0]);
        product.solution                      = fields[1];
        product.satellite                     = SatSys(fields[2].c_str());
        product.observable                    = string_to_enum<E_ObsCode>(fields[3]);
        product.clock_m                       = std::stod(fields[4]);
        product.clock_sigma_m                 = std::stod(fields[5]);
        product.phase_m                       = std::stod(fields[6]);
        product.phase_sigma_m                 = std::stod(fields[7]);
        product.clock_phase_covariance_m2     = std::stod(fields[8]);
        product.correction_m                  = std::stod(fields[9]);
        product.correction_sigma_m            = std::stod(fields[10]);
        product.discontinuity_counter         = std::stoi(fields[11]);
        product.integer_shift_cycles          = std::stoll(fields[12]);
        product.fractional_shift_cycles       = std::stod(fields[13]);
        product.datum_version                 = std::stoi(fields[14]);
        product.valid_from.bigTime            = std::stold(fields[15]);
        product.product_iod                   = std::stoi(fields[16]);
        product.reset_reason                  = fields[17];
        if (fields.size() == 28 || fields.size() == 33 || fields.size() == 34)
        {
            product.persistent_relation_known = std::stoi(fields[18]) != 0;
            product.current_alignment_state   = fields[19];
            product.integer_structure_valid  = std::stoi(fields[20]) != 0;
            product.integer_datum_continuous = std::stoi(fields[21]) != 0;
            product.integer_precision_valid  = std::stoi(fields[22]) != 0;
            product.integer_valid            = std::stoi(fields[23]) != 0;
            product.integer_component_id     = fields[24];
            product.integer_datum_id          = fields[25];
            product.valid_from.bigTime        = std::stold(fields[26]);
            if (fields.size() == 33)
            {
                product.numeric_valid = std::stoi(fields[28]) != 0;
                product.branch_valid  = std::stoi(fields[29]) != 0;
                product.ppp_usable    = std::stoi(fields[30]) != 0;
                product.pppar_usable  = std::stoi(fields[31]) != 0;
                product.invalid_reason = fields[32];
                product.continuity_valid = product.ppp_usable;
            }
            else if (fields.size() == 34)
            {
                product.numeric_valid = std::stoi(fields[28]) != 0;
                product.branch_valid  = std::stoi(fields[29]) != 0;
                product.continuity_valid = std::stoi(fields[30]) != 0;
                product.ppp_usable    = std::stoi(fields[31]) != 0;
                product.pppar_usable  = std::stoi(fields[32]) != 0;
                product.invalid_reason = fields[33];
            }
        }

        else if (fields.size() == 26)
        {
            product.integer_structure_valid  = std::stoi(fields[18]) != 0;
            product.integer_datum_continuous = std::stoi(fields[19]) != 0;
            product.integer_precision_valid  = std::stoi(fields[20]) != 0;
            product.integer_valid            = std::stoi(fields[21]) != 0;
            product.integer_component_id     = fields[22];
            product.integer_datum_id          = fields[23];
            product.valid_from.bigTime        = std::stold(fields[24]);
            product.persistent_relation_known =
                product.integer_component_id != "UNRESOLVED";
            product.current_alignment_state =
                product.integer_datum_continuous
                    ? "CURRENT_ALIGNMENT_VALID"
                    : (product.persistent_relation_known
                           ? "CURRENT_ALIGNMENT_LOST"
                           : "CURRENT_ALIGNMENT_PENDING");
        }
        else
        {
            product.integer_valid             = std::stoi(fields[18]) != 0;
            product.persistent_relation_known = product.integer_valid;
            product.current_alignment_state   = product.integer_valid
                ? "CURRENT_ALIGNMENT_VALID"
                : "CURRENT_ALIGNMENT_PENDING";
            product.integer_structure_valid   = product.integer_valid;
            product.integer_datum_continuous  = product.integer_valid;
            product.integer_precision_valid   = product.integer_valid;
            if (fields.size() == 23)
            {
                product.integer_component_id  = fields[19];
                product.integer_datum_id      = fields[20];
                product.valid_from.bigTime    = std::stold(fields[21]);
            }
        }

        if (fields.size() != 33 && fields.size() != 34)
        {
            product.numeric_valid =
                std::isfinite(product.clock_m) &&
                std::isfinite(product.phase_m) &&
                std::isfinite(product.correction_m) &&
                std::isfinite(product.clock_sigma_m) &&
                std::isfinite(product.phase_sigma_m) &&
                std::isfinite(product.correction_sigma_m);
            product.branch_valid = product.numeric_valid;
            product.continuity_valid = product.numeric_valid;
            product.ppp_usable = product.numeric_valid;
            product.pppar_usable =
                product.ppp_usable && product.integer_valid;
            product.invalid_reason = product.numeric_valid
                ? "LEGACY_PRODUCT"
                : "LEGACY_NUMERIC_FAILURE";
        }

        ProductLookupKey key{
            static_cast<long int>(std::llround(product.time.bigTime)),
            product.satellite,
            product.observable,
            product.solution
        };
        productMap[key] = product;
    }

    loadedProductFilename = filename;
    BOOST_LOG_TRIVIAL(info)
        << "Loaded " << productMap.size()
        << " Zhang internal product records from " << filename;
    return !productMap.empty();
}

bool resetUserPhaseBlock(
    Trace&       trace,
    KFState&     kfState,
    const string& receiver,
    E_Sys        sys,
    E_ObsCode    code,
    const string& reason
)
{
    map<KFKey, map<KFKey, double>> transform;
    bool removed = false;

    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        bool targetReceiverPhase =
            key.type == KF::PHASE_BIAS &&
            key.str == receiver &&
            key.Sat.sys == sys &&
            key.num == static_cast<int>(code);
        bool targetAmbiguity =
            key.type == KF::AMBIGUITY &&
            key.str == receiver &&
            key.Sat.sys == sys &&
            key.num == static_cast<int>(code);

        if (targetReceiverPhase || targetAmbiguity)
        {
            removed = true;
            continue;
        }
        transform[key][key] = 1;
    }

    if (!removed)
    {
        return true;
    }

    return kfState.applyStateTransform(
        trace,
        transform,
        "Zhang held-out user phase reset: " + reason
    );
}

bool resetUserAmbiguity(
    Trace&        trace,
    KFState&      kfState,
    const string& receiver,
    const SatSys& satellite,
    E_ObsCode     code,
    const string& reason
)
{
    KFKey ambiguityKey =
        userAmbiguityKey(receiver, satellite, code);
    if (kfState.kfIndexMap.find(ambiguityKey) == kfState.kfIndexMap.end())
    {
        return true;
    }

    map<KFKey, map<KFKey, double>> transform;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        if (key == ambiguityKey)
        {
            continue;
        }
        transform[key][key] = 1;
    }

    return kfState.applyStateTransform(
        trace,
        transform,
        "Zhang held-out user ambiguity reset: " + reason
    );
}

bool transformUserReference(
    Trace&        trace,
    KFState&      kfState,
    const string& receiver,
    E_Sys         sys,
    E_ObsCode     code,
    const SatSys& oldReference,
    const SatSys& newReference
)
{
    if (oldReference == newReference)
    {
        return true;
    }

    const double lambda = wavelength(sys, code);
    KFKey newReferenceAmbiguity =
        userAmbiguityKey(receiver, newReference, code);
    if (lambda <= 0 ||
        kfState.kfIndexMap.find(newReferenceAmbiguity) == kfState.kfIndexMap.end())
    {
        return false;
    }

    map<KFKey, map<KFKey, double>> transform;
    for (const auto& [key, index] : kfState.kfIndexMap)
    {
        bool targetReceiverPhase =
            key.type == KF::PHASE_BIAS &&
            key.str == receiver &&
            key.Sat.sys == sys &&
            key.num == static_cast<int>(code);
        bool targetAmbiguity =
            key.type == KF::AMBIGUITY &&
            key.str == receiver &&
            key.Sat.sys == sys &&
            key.num == static_cast<int>(code);

        if (!targetReceiverPhase && !targetAmbiguity)
        {
            transform[key][key] = 1;
            continue;
        }

        if (targetReceiverPhase)
        {
            transform[key][key] = 1;
            transform[key][newReferenceAmbiguity] = lambda;
            continue;
        }

        if (key.Sat == newReference)
        {
            continue;
        }

        if (key.Sat == oldReference)
        {
            KFKey oldReferenceDestination =
                userAmbiguityKey(receiver, oldReference, code);
            transform[oldReferenceDestination][newReferenceAmbiguity] = -1;
            continue;
        }

        transform[key][key] = 1;
        transform[key][newReferenceAmbiguity] = -1;
    }

    return kfState.applyStateTransform(
        trace,
        transform,
        "Zhang held-out user ambiguity-reference exchange"
    );
}
}  // namespace

ZhangCanonicalRelationSelection selectZhangE18CanonicalProductRelations(
    const KFState& captureOwner,
    E_Sys system,
    const vector<ZhangCanonicalSatelliteRelation>& bootstrapCandidates,
    const set<SatSys>& availableSatellites,
    int maximumRelations)
{
    return e18PersistentProductDatumRegistries[&captureOwner].selectRelations(
        system, bootstrapCandidates, availableSatellites, maximumRelations);
}

ZhangPersistentProductDatumObservation observeZhangE18PersistentProductDatum(
    const KFState& captureOwner,
    E_Sys system,
    E_ObsCode observable,
    const ZhangCanonicalSatelliteRelation& relation,
    int anchorPhaseSegment,
    int satellitePhaseSegment,
    int anchorDatumVersion,
    int satelliteDatumVersion,
    bool absoluteAvailable)
{
    return e18PersistentProductDatumRegistries[&captureOwner].observe(
        system, observable, relation,
        anchorPhaseSegment, satellitePhaseSegment,
        anchorDatumVersion, satelliteDatumVersion,
        absoluteAvailable);
}

void configureZhangE18FactorCapture(KFState& kfState)
{
    const KFState* owner = &kfState;
    if (!acsConfig.zhangPppAr.fixed_lag_factor_capture_shadow)
    {
        if (e18ConfiguredFactorCaptureStates.erase(owner) > 0)
        {
            kfState.acceptedMeasurementFactorCallback = {};
            kfState.stateTransitionFactorCallback = {};
            kfState.exactStateTransformCallback = {};
            e18FactorCaptureBuffers.erase(owner);
        }
        return;
    }
    if (e18ConfiguredFactorCaptureStates.find(owner) !=
        e18ConfiguredFactorCaptureStates.end())
    {
        return;
    }

    auto& buffer = e18FactorCaptureBuffers[owner];
    buffer.clear();
    buffer.setMaximumEvents(
        acsConfig.zhangPppAr.fixed_lag_factor_capture_max_events
    );

    kfState.acceptedMeasurementFactorCallback =
        [owner](const KFState& state,
                const KFMeas& measurement,
                const string& suffix,
                const VectorXd& posteriorMean,
                const MatrixXd& posteriorCovariance)
        {
            if (&state != owner || suffix != "/PPP")
            {
                return;
            }
            auto capture = e18FactorCaptureBuffers.find(owner);
            if (capture == e18FactorCaptureBuffers.end())
            {
                return;
            }
            bool accepted = capture->second.recordMeasurement(
                measurement.time,
                zhangKeysByIndex(state.kfIndexMap),
                state.x,
                state.P,
                measurement,
                suffix,
                posteriorMean,
                posteriorCovariance
            );
            ZhangFactorCaptureSummary summary = capture->second.summary();
            BOOST_LOG_TRIVIAL(info)
                << "ZHANG_E18_FACTOR_CAPTURE time="
                << measurement.time.to_string(0)
                << " event=MEASUREMENT"
                << " status=" << (accepted ? "ACCEPTED" : "REJECTED")
                << " events=" << summary.events
                << " measurements=" << summary.measurements
                << " transitions=" << summary.transitions
                << " exact_transforms=" << summary.coordinateTransforms
                << " measurement_rows=" << summary.measurementRows
                << " measurement_nnz=" << summary.measurementNonZeros
                << " covariance_nnz=" << summary.covarianceNonZeros
                << " replay_prior_mean_relative_error="
                << summary.maximumReplayPriorMeanRelativeError
                << " replay_prior_covariance_relative_error="
                << summary.maximumReplayPriorCovarianceRelativeError
				<< " raw_square_root_mean_relative_error="
				<< summary.maximumRawSquareRootMeanRelativeError
				<< " raw_square_root_covariance_relative_error="
				<< summary.maximumRawSquareRootCovarianceRelativeError
                << " failure_reason="
                << (summary.failureReason.empty() ? "NONE" : summary.failureReason)
                << " feedback=0";
        };

    kfState.stateTransitionFactorCallback =
        [owner](const KFState& state,
                GTime time,
                const map<KFKey, int>& source,
                const map<KFKey, int>& destination,
                const SparseMatrix<double>& transition,
                const MatrixXd& processCovariance,
                const string& label)
        {
            if (&state != owner)
            {
                return;
            }
            auto capture = e18FactorCaptureBuffers.find(owner);
            if (capture == e18FactorCaptureBuffers.end())
            {
                return;
            }
            bool accepted = capture->second.recordTransition(
                time,
                zhangKeysByIndex(source),
                zhangKeysByIndex(destination),
                transition,
                processCovariance,
                label
            );
            if (!accepted)
            {
                auto summary = capture->second.summary();
                BOOST_LOG_TRIVIAL(error)
                    << "ZHANG_E18_FACTOR_CAPTURE time=" << time.to_string(0)
                    << " event=STATE_TRANSITION status=REJECTED"
                    << " failure_reason=" << summary.failureReason
                    << " feedback=0";
            }
        };

    kfState.exactStateTransformCallback =
        [owner](const KFState& state,
                GTime time,
                const map<KFKey, int>& source,
                const map<KFKey, int>& destination,
                const SparseMatrix<double>& transform,
                const string& label)
        {
            if (&state != owner)
            {
                return;
            }
            auto capture = e18FactorCaptureBuffers.find(owner);
            if (capture == e18FactorCaptureBuffers.end())
            {
                return;
            }
            bool accepted = capture->second.recordCoordinateTransform(
                time,
                zhangKeysByIndex(source),
                zhangKeysByIndex(destination),
                transform,
                label
            );
			auto transformSummary = capture->second.summary();
			const string transformFailure = transformSummary.failureReason;
			const bool localPhysicalReinitialisation =
				label.find("local phase-coordinate reinitialisation")
					!= string::npos;
			const bool physicalFunctionalRetired =
				transformFailure.find(
					"PERSISTENT_FUNCTIONAL_NOT_TRANSPORTABLE_") == 0;
			bool physicalArcReset = false;
			if (!accepted
			 && localPhysicalReinitialisation
			 && physicalFunctionalRetired)
			{
				// This projection removed a direction used by the physical target.
				// It is a real arc/version boundary, not an S-basis exchange.  Close
				// the old chronology and re-anchor at the next accepted measurement.
				capture->second.resetForPhysicalArcChange();
				physicalArcReset = true;
			}
            BOOST_LOG_TRIVIAL(info)
                << "ZHANG_E18_FACTOR_CAPTURE time=" << time.to_string(0)
                << " event=EXACT_COORDINATE_TRANSFORM"
                << " label=" << label
                << " source_states=" << source.size()
                << " destination_states=" << destination.size()
                << " transform_nnz=" << transform.nonZeros()
				<< " status=" << (accepted
					? "ACCEPTED"
					: physicalArcReset ? "RESET" : "REJECTED")
				<< " physical_arc_reset=" << physicalArcReset
				<< " failure_reason="
				<< (transformFailure.empty() ? "NONE" : transformFailure)
                << " feedback=0";
        };
    e18ConfiguredFactorCaptureStates.insert(owner);
}

bool recordZhangE18IntegerDatumTarget(
    Trace&              trace,
    const KFState&      captureOwner,
    const KFState&      state,
    E_Sys               system,
    const string&       targetFamily,
    const SatSys&       anchor,
    const SatSys&       satellite,
    const VectorXd&     currentCoordinateRow,
    double              persistentDatumOffsetCycles,
    bool                exactDatumTransportValid,
    const string&       canonicalCoordinateIdentity,
    const string&       productDatumIdentity,
    int                 productDatumVersion,
    const string&       topologyKey,
    const string&       gaugeComponentIdentity,
    const string&       phaseSegmentIdentity,
    const string&       physicalArcSignature,
    const vector<std::pair<string, int>>& physicalArcVersions,
    GTime               time)
{
    if (!acsConfig.zhangPppAr.fixed_lag_factor_capture_shadow)
    {
        return false;
    }
    auto capture = e18FactorCaptureBuffers.find(&captureOwner);
    if (capture == e18FactorCaptureBuffers.end())
    {
        return false;
    }
	if (currentCoordinateRow.size() != state.x.size()
	 || !currentCoordinateRow.allFinite()
	 || !std::isfinite(persistentDatumOffsetCycles))
	{
        trace << "\nZHANG_E18_INTEGER_DATUM_TARGET time=" << time.to_string(0)
              << " system=" << enum_to_string(system)
              << " topology_key=" << topologyKey
              << " anchor=" << anchor.id()
              << " satellite=" << satellite.id()
              << " status=REJECTED reason="
			  << "INVALID_INTEGER_DATUM_FUNCTIONAL"
              << " feedback=0";
        return false;
    }
    const VectorXd& row = currentCoordinateRow;
	// An unresolved z_T is an integer translation, not a continuous random
	// state.  Retaining Gk modulo Z preserves the fractional likelihood and
	// perr while still blocking absolute product publication.
	const double offset = exactDatumTransportValid
		? persistentDatumOffsetCycles
		: 0.0;
	const int unresolvedGaugeRank = exactDatumTransportValid ? 0 : 1;
	const string integerGaugeIdentity = exactDatumTransportValid
		? ""
		: enum_to_string(system) + ":" + targetFamily + ":"
			+ gaugeComponentIdentity;
	const string identity = enum_to_string(system) + ":" + targetFamily + ":" +
		anchor.id() + ":" + satellite.id();
	vector<ZhangCapturedPhysicalArcVersion> capturedArcVersions;
	for (const auto& [arc, version] : physicalArcVersions)
	{
		capturedArcVersions.push_back({arc, version});
	}
	double targetMean = offset;
	double targetVariance = 0;
	vector<std::pair<int, double>> nonZeros;
	for (int index = 0; index < row.size(); index++)
	{
		if (row(index) != 0)
		{
			nonZeros.push_back({index, row(index)});
			targetMean += row(index) * state.x(index);
		}
	}
	for (const auto& [left, leftCoefficient] : nonZeros)
	for (const auto& [right, rightCoefficient] : nonZeros)
	{
		targetVariance += leftCoefficient * rightCoefficient
			* state.P(left, right);
	}
    const bool accepted = capture->second.recordPhysicalTarget(
        time,
        identity,
        physicalArcSignature,
		phaseSegmentIdentity,
		capturedArcVersions,
        zhangKeysByIndex(state.kfIndexMap),
        row,
        offset,
        state.x,
		state.P,
		unresolvedGaugeRank,
		integerGaugeIdentity,
		canonicalCoordinateIdentity,
		productDatumIdentity,
		productDatumVersion
    );
	if (accepted && !capture->second.capturedPhysicalTargets().empty())
	{
		const auto& persisted = capture->second.capturedPhysicalTargets().back();
		targetMean = persisted.mean;
		targetVariance = persisted.variance;
	}
    const auto summary = capture->second.summary();
	const auto& retainedBlock = capture->second.currentRetainedBlock();
	std::ostringstream whitenedResiduals;
	for (int index = 0; index < retainedBlock.whitenedResidual.size(); index++)
	{
		if (index > 0)
		{
			whitenedResiduals << ";";
		}
		whitenedResiduals << retainedBlock.whitenedResidual(index);
	}
	trace << "\nZHANG_E18_INTEGER_DATUM_TARGET time=" << time.to_string(0)
		  << " system=" << enum_to_string(system)
		  << " target_family=" << targetFamily
          << " topology_key=" << topologyKey
          << " anchor=" << anchor.id()
          << " satellite=" << satellite.id()
		  << " phase_segment_identity=" << phaseSegmentIdentity
          << " physical_signature=" << physicalArcSignature
          << " mean=" << targetMean
          << " variance=" << targetVariance
		  << " datum_offset_cycles="
		  << (accepted
			&& !capture->second.capturedPhysicalTargets().empty()
				? capture->second.capturedPhysicalTargets().back().offset
				: offset)
		  << " canonical_coordinate_id=" << canonicalCoordinateIdentity
		  << " product_datum_id=" << productDatumIdentity
		  << " product_datum_version=" << productDatumVersion
		  << " coordinate=PRIMITIVE_BASE_INTEGER_TARGET"
		  << " quotient_valid=" << accepted
		  << " absolute_datum_valid="
		  << (accepted && exactDatumTransportValid)
		  << " unresolved_gauge_rank=" << unresolvedGaugeRank
          << " targets=" << summary.physicalTargets
		  << " physical_identity_resets="
		  << summary.physicalTargetIdentityResets
		  << " coordinate_continuations="
		  << summary.physicalTargetCoordinateContinuations
          << " target_mean_replay_relative_error="
          << summary.maximumTargetMeanRelativeError
          << " target_variance_replay_relative_error="
          << summary.maximumTargetVarianceRelativeError
		  << " retained_block_targets=" << retainedBlock.targetCount
		  << " retained_block_rank=" << retainedBlock.informationRank
		  << " retained_block_residual_domain=PREFIT_INNOVATION"
		  << " retained_block_residual_dof=" << retainedBlock.residualDof
		  << " retained_block_projected_gauge_rank="
		  << retainedBlock.projectedGaugeRank
		  << " retained_block_whitened_squared_norm="
		  << retainedBlock.whitenedSquaredNorm
		  << " retained_block_whitened_residuals="
		  << (whitenedResiduals.str().empty() ? "NONE" : whitenedResiduals.str())
		  << " retained_block_valid=" << retainedBlock.valid
		  << " retained_block_reason="
		  << (retainedBlock.failureReason.empty()
				? "NONE" : retainedBlock.failureReason)
		  << " status=" << (accepted
				? (exactDatumTransportValid
					? "ACCEPTED_ABSOLUTE_DATUM"
					: "ACCEPTED_INTEGER_QUOTIENT")
				: "REJECTED")
          << " reason="
		  << (!accepted
				? (!capture->second.lastTargetReason().empty()
					? capture->second.lastTargetReason()
					: summary.failureReason.empty()
						? "UNKNOWN" : summary.failureReason)
				: (exactDatumTransportValid
					? "NONE" : "INTEGER_GAUGE_UNRESOLVED"))
          << " feedback=0";
    return accepted;
}

namespace
{
struct ZhangOperationalLambdaResult
{
	bool valid = false;
	bool validationPass = false;
	VectorXd best;
	VectorXd second;
	MatrixXd decorrelation;
	MatrixXd reducedCovariance;
	VectorXd conditionalVariances;
	VectorXd conditionalSuccessRates;
	VectorXd reducedBest;
	VectorXd reducedSecond;
	double bestDistance = std::numeric_limits<double>::quiet_NaN();
	double secondDistance = std::numeric_limits<double>::quiet_NaN();
	double bootstrappedSuccessRate = std::numeric_limits<double>::quiet_NaN();
	double bootstrapImplementationConsistencyError =
		std::numeric_limits<double>::quiet_NaN();
	double ambiguityDilutionOfPrecision =
		std::numeric_limits<double>::quiet_NaN();
	double covarianceTransformMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	double conditionalDeterminantLogError =
		std::numeric_limits<double>::quiet_NaN();
	double bestCandidateBackTransformMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	double secondCandidateBackTransformMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	double reducedCandidateIntegerMaximumError =
		std::numeric_limits<double>::quiet_NaN();
	bool transformUnimodular = false;
	bool candidateBackTransformConsistent = false;
	std::string failureReason;
};

ZhangOperationalLambdaResult runZhangOperationalLambda(
	Trace& trace,
	const VectorXd& mean,
	const MatrixXd& covariance)
{
	ZhangOperationalLambdaResult result;
	if (mean.size() == 0
	 || covariance.rows() != mean.size()
	 || covariance.cols() != mean.size()
	 || !mean.allFinite() || !covariance.allFinite())
	{
		result.failureReason = "INVALID_OPERATIONAL_LAMBDA_DIMENSIONS";
		return result;
	}
	const MatrixXd symmetric = 0.5 * (covariance + covariance.transpose());
	Eigen::SelfAdjointEigenSolver<MatrixXd> spectrum(symmetric);
	if (spectrum.info() != Eigen::Success
	 || spectrum.eigenvalues().minCoeff() <= 0)
	{
		result.failureReason = "NON_POSITIVE_OPERATIONAL_LAMBDA_COVARIANCE";
		return result;
	}
	std::vector<double> candidates(mean.size() * 2);
	double distances[2] = {};
	const int status = lambdaWithTransform(
		trace, mean.size(), 2, mean.data(), symmetric.data(),
		candidates.data(), distances, acsConfig.predefined_fail,
		result.validationPass, result.decorrelation,
		result.reducedCovariance, result.conditionalVariances,
		result.conditionalSuccessRates,
		result.bootstrappedSuccessRate);
	if (status != 0)
	{
		result.failureReason = "OPERATIONAL_LAMBDA_FAILED_"
			+ std::to_string(status);
		return result;
	}
	result.best = Eigen::Map<VectorXd>(candidates.data(), mean.size());
	result.second = Eigen::Map<VectorXd>(
		candidates.data() + mean.size(), mean.size());
	result.bestDistance = distances[0];
	result.secondDistance = distances[1];
	const auto reductionAudit = zhangAuditLambdaReduction(
		symmetric, result.decorrelation, result.reducedCovariance,
		result.conditionalVariances, result.best, result.second);
	result.conditionalSuccessRates = reductionAudit.conditionalSuccessRates;
	result.reducedBest = reductionAudit.reducedBestCandidate;
	result.reducedSecond = reductionAudit.reducedSecondCandidate;
	result.ambiguityDilutionOfPrecision =
		reductionAudit.ambiguityDilutionOfPrecision;
	result.covarianceTransformMaximumError =
		reductionAudit.covarianceTransformMaximumError;
	result.conditionalDeterminantLogError =
		reductionAudit.conditionalDeterminantLogError;
	result.bestCandidateBackTransformMaximumError =
		reductionAudit.bestCandidateBackTransformMaximumError;
	result.secondCandidateBackTransformMaximumError =
		reductionAudit.secondCandidateBackTransformMaximumError;
	result.reducedCandidateIntegerMaximumError =
		reductionAudit.reducedCandidateIntegerMaximumError;
	result.transformUnimodular = reductionAudit.transformUnimodular;
	result.candidateBackTransformConsistent =
		reductionAudit.candidateBackTransformConsistent;
	result.bootstrapImplementationConsistencyError = std::abs(
		result.bootstrappedSuccessRate
		- reductionAudit.jointBootstrappedSuccessRate);
	constexpr double bootstrapAuditTolerance = 5e-7;
	result.valid = result.best.allFinite() && result.second.allFinite()
		&& result.decorrelation.allFinite()
		&& result.reducedCovariance.allFinite()
		&& result.conditionalVariances.allFinite()
		&& result.conditionalSuccessRates.allFinite()
		&& std::isfinite(result.bootstrappedSuccessRate)
		&& std::isfinite(result.ambiguityDilutionOfPrecision)
		&& reductionAudit.valid
		&& result.bootstrapImplementationConsistencyError
			<= bootstrapAuditTolerance;
	if (!result.valid)
	{
		if (!reductionAudit.valid)
		{
			result.failureReason = reductionAudit.failureReason;
		}
		else if (result.bootstrapImplementationConsistencyError
			> bootstrapAuditTolerance)
		{
			result.failureReason =
				"OPERATIONAL_LAMBDA_BOOTSTRAP_AUDIT_MISMATCH";
		}
		else
		{
			result.failureReason = "NONFINITE_OPERATIONAL_LAMBDA_RESULT";
		}
	}
	return result;
}

ZhangIntegerVector zhangIntegerCandidate(const VectorXd& candidate)
{
	ZhangIntegerVector integer(candidate.size());
	for (int index = 0; index < candidate.size(); index++)
	{
		integer(index) = std::llround(candidate(index));
	}
	return integer;
}

std::vector<int> zhangSelectOperationalParSubset(
	Trace& trace,
	const VectorXd& mean,
	const MatrixXd& covariance,
	double successThreshold,
	double& achievedSuccess)
{
	std::vector<int> retained(mean.size());
	std::iota(retained.begin(), retained.end(), 0);
	auto evaluate = [&](const std::vector<int>& indices)
	{
		VectorXd subsetMean(indices.size());
		MatrixXd subsetCovariance(indices.size(), indices.size());
		for (int row = 0; row < static_cast<int>(indices.size()); row++)
		{
			subsetMean(row) = mean(indices[row]);
			for (int column = 0;
				 column < static_cast<int>(indices.size()); column++)
			{
				subsetCovariance(row, column) =
					covariance(indices[row], indices[column]);
			}
		}
		return runZhangOperationalLambda(trace, subsetMean, subsetCovariance);
	};
	auto current = evaluate(retained);
	achievedSuccess = current.valid
		? current.bootstrappedSuccessRate
		: std::numeric_limits<double>::quiet_NaN();
	while (retained.size() > 1
		&& (!current.valid || achievedSuccess < successThreshold))
	{
		double bestSuccess = -1;
		std::vector<int> bestSubset;
		for (int removed = 0;
			 removed < static_cast<int>(retained.size()); removed++)
		{
			std::vector<int> candidate = retained;
			candidate.erase(candidate.begin() + removed);
			const auto candidateResult = evaluate(candidate);
			if (candidateResult.valid
			 && candidateResult.bootstrappedSuccessRate > bestSuccess)
			{
				bestSuccess = candidateResult.bootstrappedSuccessRate;
				bestSubset = std::move(candidate);
			}
		}
		if (bestSubset.empty())
		{
			retained.clear();
			break;
		}
		retained = std::move(bestSubset);
		current = evaluate(retained);
		achievedSuccess = current.valid
			? current.bootstrappedSuccessRate
			: std::numeric_limits<double>::quiet_NaN();
	}
	if (!current.valid || achievedSuccess < successThreshold)
	{
		retained.clear();
	}
	return retained;
}

MatrixXd zhangProductRelationIncidence(
	const std::vector<std::string>& relations)
{
	std::map<std::string, int> nodeIndex;
	for (const auto& relation : relations)
	{
		const auto delimiter = relation.find("->");
		if (delimiter == std::string::npos)
		{
			continue;
		}
		nodeIndex.emplace(
			relation.substr(0, delimiter), nodeIndex.size());
		nodeIndex.emplace(
			relation.substr(delimiter + 2), nodeIndex.size());
	}
	MatrixXd incidence = MatrixXd::Zero(relations.size(), nodeIndex.size());
	for (int row = 0; row < static_cast<int>(relations.size()); row++)
	{
		const auto delimiter = relations[row].find("->");
		if (delimiter == std::string::npos)
		{
			continue;
		}
		incidence(row, nodeIndex.at(relations[row].substr(0, delimiter))) = -1;
		incidence(row, nodeIndex.at(relations[row].substr(delimiter + 2))) = 1;
	}
	return incidence;
}

void traceZhangIntegerDiagnostic(
	Trace& trace,
	GTime time,
	const std::string& strategy,
	const VectorXd& mean,
	const MatrixXd& covariance,
	const std::vector<std::string>& labels,
	const std::vector<std::string>& relations,
	int quotientRank,
	int absoluteRank,
	bool transformUnimodular,
	const std::vector<int>& sourceIndices = {})
{
	const auto solution = runZhangOperationalLambda(trace, mean, covariance);
	const MatrixXd productIncidence = zhangProductRelationIncidence(relations);
	const MatrixXd noRedundantCycles(0, mean.size());
	ZhangLambdaParDiagnostics diagnostics;
	if (solution.valid)
	{
		diagnostics = zhangEvaluateLambdaParCandidates(
			mean, covariance,
			zhangIntegerCandidate(solution.best),
			zhangIntegerCandidate(solution.second),
			quotientRank, absoluteRank, productIncidence,
			noRedundantCycles, 0.999);
	}
	const int conditionalDirectionPassCount = solution.valid
		? (solution.conditionalSuccessRates.array() >= 0.999).count() : 0;
	const bool jointReliabilityPass = solution.valid
		&& solution.validationPass
		&& solution.bootstrappedSuccessRate >= 0.999;
	trace << "\nZHANG_E18_INTEGER_DIAGNOSTIC time=" << time.to_string(0)
		<< " strategy=" << strategy
		<< " valid=" << (solution.valid && diagnostics.valid)
		<< " target_count=" << mean.size()
		<< " quotient_valid_rank=" << quotientRank
		<< " absolute_valid_rank=" << absoluteRank
		<< " product_relation_graph_rank="
		<< diagnostics.productRelationGraphRank
		<< " conditional_direction_pass_count="
		<< conditionalDirectionPassCount
		<< " recoverable_satellite_count="
		<< diagnostics.recoverableSatelliteCount
		<< " best_candidate_distance=" << solution.bestDistance
		<< " second_candidate_distance=" << solution.secondDistance
		<< " second_to_best_distance_ratio="
		<< (solution.bestDistance > 0
			? solution.secondDistance / solution.bestDistance
			: std::numeric_limits<double>::infinity())
		<< " joint_bootstrapped_success_rate="
		<< solution.bootstrappedSuccessRate
		<< " bootstrap_implementation_consistency_error="
		<< solution.bootstrapImplementationConsistencyError
		<< " lambda_validation_pass=" << solution.validationPass
		<< " joint_reliability_pass=" << jointReliabilityPass
		<< " reliability_gate=JOINT_BOOTSTRAP_AND_FFRT"
		<< " ambiguity_dilution_of_precision="
		<< solution.ambiguityDilutionOfPrecision
		<< " lambda_transform_unimodular="
		<< solution.transformUnimodular
		<< " candidate_back_transform_consistent="
		<< solution.candidateBackTransformConsistent
		<< " covariance_transform_maximum_error="
		<< solution.covarianceTransformMaximumError
		<< " conditional_determinant_log_error="
		<< solution.conditionalDeterminantLogError
		<< " best_candidate_back_transform_maximum_error="
		<< solution.bestCandidateBackTransformMaximumError
		<< " second_candidate_back_transform_maximum_error="
		<< solution.secondCandidateBackTransformMaximumError
		<< " reduced_candidate_integer_maximum_error="
		<< solution.reducedCandidateIntegerMaximumError
		<< " maximum_cycle_closure_error="
		<< diagnostics.maximumCycleClosureError
		<< " cycle_constraint_count=0"
		<< " transform_unimodular=" << transformUnimodular
		<< " target_labels=";
	for (int index = 0; index < static_cast<int>(labels.size()); index++)
	{
		if (index) trace << ";";
		trace << labels[index];
	}
	trace << " best_candidate=";
	for (int index = 0; index < solution.best.size(); index++)
	{
		if (index) trace << ";";
		trace << std::llround(solution.best(index));
	}
	trace << " second_candidate=";
	for (int index = 0; index < solution.second.size(); index++)
	{
		if (index) trace << ";";
		trace << std::llround(solution.second(index));
	}
	trace << " lambda_Z=";
	for (int row = 0; row < solution.decorrelation.rows(); row++)
	for (int column = 0; column < solution.decorrelation.cols(); column++)
	{
		if (row || column) trace << ";";
		trace << std::llround(solution.decorrelation(row, column));
	}
	trace << " reduced_covariance=";
	for (int row = 0; row < solution.reducedCovariance.rows(); row++)
	for (int column = 0; column < solution.reducedCovariance.cols(); column++)
	{
		if (row || column) trace << ";";
		trace << solution.reducedCovariance(row, column);
	}
	trace << " conditional_variances=";
	for (int index = 0; index < solution.conditionalVariances.size(); index++)
	{
		if (index) trace << ";";
		trace << solution.conditionalVariances(index);
	}
	trace << " conditional_success_rates=";
	for (int index = 0; index < solution.conditionalSuccessRates.size(); index++)
	{
		if (index) trace << ";";
		trace << solution.conditionalSuccessRates(index);
	}
	trace << " reduced_best_candidate=";
	for (int index = 0; index < solution.reducedBest.size(); index++)
	{
		if (index) trace << ";";
		trace << std::llround(solution.reducedBest(index));
	}
	trace << " reduced_second_candidate=";
	for (int index = 0; index < solution.reducedSecond.size(); index++)
	{
		if (index) trace << ";";
		trace << std::llround(solution.reducedSecond(index));
	}
	trace << " source_indices=";
	if (sourceIndices.empty())
	{
		trace << "ALL";
	}
	else
	{
		for (int index = 0; index < static_cast<int>(sourceIndices.size()); index++)
		{
			if (index) trace << ";";
			trace << sourceIndices[index];
		}
	}
	trace << " status=" << (solution.valid && diagnostics.valid
			? "EVALUATED" : "REJECTED")
		<< " reason=" << (!solution.valid ? solution.failureReason
			: !diagnostics.valid ? diagnostics.failureReason : "NONE")
		<< " feedback=0";
}

template<typename Marginal>
void traceZhangE18IntegerDiagnostics(
	Trace& trace,
	GTime time,
	const Marginal& marginal,
	const std::string& strategyPrefix)
{
	if (!marginal.valid)
	{
		return;
	}
	const auto quotient = zhangBuildIntegerQuotientCoordinates(
		marginal.identities, marginal.gaugeIdentities,
		marginal.absoluteValidity, marginal.mean, marginal.covariance);
	if (!quotient.valid)
	{
		trace << "\nZHANG_E18_INTEGER_DIAGNOSTIC time=" << time.to_string(0)
			<< " strategy=" << strategyPrefix
			<< "QUOTIENT_CONSTRUCTION valid=0 status=REJECTED reason="
			<< quotient.failureReason << " feedback=0";
		return;
	}
	traceZhangIntegerDiagnostic(
		trace, time, strategyPrefix + "DIRECT_JOINT",
		quotient.mean, quotient.covariance,
		quotient.labels, quotient.relations, marginal.quotientValidRank,
		marginal.absoluteValidRank, true);

	const auto wideLane = zhangBuildWideLaneL1BlockCoordinates(quotient);
	if (wideLane.valid)
	{
		const MatrixXd transform = wideLane.transform.template cast<double>();
		traceZhangIntegerDiagnostic(
			trace, time, strategyPrefix + "WL_L1_UNIMODULAR",
			transform.transpose() * quotient.mean,
			transform.transpose() * quotient.covariance * transform,
			wideLane.labels, quotient.relations,
			marginal.quotientValidRank, marginal.absoluteValidRank, true);
	}
	else
	{
		trace << "\nZHANG_E18_INTEGER_DIAGNOSTIC time=" << time.to_string(0)
			<< " strategy=" << strategyPrefix
			<< "WL_L1_UNIMODULAR valid=0 status=REJECTED reason="
			<< wideLane.failureReason << " feedback=0";
	}

	double parSuccess = std::numeric_limits<double>::quiet_NaN();
	const std::vector<int> par = zhangSelectOperationalParSubset(
		trace, quotient.mean, quotient.covariance, 0.999, parSuccess);
	if (!par.empty())
	{
		VectorXd parMean(par.size());
		MatrixXd parCovariance(par.size(), par.size());
		std::vector<std::string> parLabels;
		std::vector<std::string> parRelations;
		for (int row = 0; row < static_cast<int>(par.size()); row++)
		{
			parMean(row) = quotient.mean(par[row]);
			parLabels.push_back(quotient.labels[par[row]]);
			parRelations.push_back(quotient.relations[par[row]]);
			for (int column = 0; column < static_cast<int>(par.size()); column++)
			{
				parCovariance(row, column) =
					quotient.covariance(par[row], par[column]);
			}
		}
		traceZhangIntegerDiagnostic(
			trace, time, strategyPrefix + "PAR_OPERATIONAL_SUBSET",
			parMean, parCovariance,
			parLabels, parRelations, par.size(), 0, true, par);
	}
	else
	{
		trace << "\nZHANG_E18_INTEGER_DIAGNOSTIC time=" << time.to_string(0)
			<< " strategy=" << strategyPrefix
			<< "PAR_OPERATIONAL_SUBSET valid=0 target_count=0"
			<< " joint_bootstrapped_success_rate=" << parSuccess
			<< " status=REJECTED reason=NO_SUBSET_REACHES_0.999 feedback=0";
	}
}
}

void traceZhangE18RawIntegerDatumWindow(
    Trace& trace,
    const KFState& captureOwner,
    GTime time)
{
    if (!acsConfig.zhangPppAr.fixed_lag_factor_capture_shadow)
    {
        return;
    }
    auto capture = e18FactorCaptureBuffers.find(&captureOwner);
    if (capture == e18FactorCaptureBuffers.end())
    {
        return;
    }
	const auto summary = capture->second.summary();
	const int evaluationStride = std::max(
		1, acsConfig.zhangPppAr.fixed_lag_factor_capture_evaluation_stride);
	if (summary.measurements == 0
	 || summary.measurements % evaluationStride != 0)
	{
		return;
	}
	const ZhangRawSquareRootTargetMarginal rawMarginal =
		capture->second.currentRawSquareRootTargetMarginal();
	trace << "\nZHANG_E18_RAW_SQUARE_ROOT_WINDOW time="
		<< time.to_string(0)
		<< " valid=" << rawMarginal.valid
		<< " quotient_valid="
		<< (rawMarginal.valid && rawMarginal.quotientValidRank > 0)
		<< " absolute_datum_valid="
		<< (rawMarginal.valid
			&& rawMarginal.absoluteValidRank
				== rawMarginal.requestedTargetCount)
		<< " requested_targets=" << rawMarginal.requestedTargetCount
		<< " unresolved_gauge_rank=" << rawMarginal.unresolvedGaugeRank
		<< " information_rank=" << rawMarginal.informationRank
		<< " quotient_valid_rank=" << rawMarginal.quotientValidRank
		<< " absolute_valid_rank=" << rawMarginal.absoluteValidRank
		<< " batch_orthogonal_residual_dof="
		<< rawMarginal.batchOrthogonalDof
		<< " batch_orthogonal_residual_squared_norm="
		<< rawMarginal.batchOrthogonalSquaredNorm
		<< " boundary_rows=" << rawMarginal.storedRows
		<< " boundary_columns=" << rawMarginal.storedColumns
		<< " maximum_boundary_rows=" << rawMarginal.maximumStoredRows
		<< " maximum_boundary_columns=" << rawMarginal.maximumStoredColumns
		<< " target_identities=";
	for (int index = 0;
		 index < static_cast<int>(rawMarginal.identities.size()); index++)
	{
		if (index) trace << ";";
		trace << rawMarginal.identities[index];
	}
	if (rawMarginal.identities.empty()) trace << "NONE";
	trace << " target_gauge_identities=";
	for (int index = 0;
		 index < static_cast<int>(rawMarginal.gaugeIdentities.size()); index++)
	{
		if (index) trace << ";";
		trace << (rawMarginal.gaugeIdentities[index].empty()
			? "ABSOLUTE" : rawMarginal.gaugeIdentities[index]);
	}
	if (rawMarginal.gaugeIdentities.empty()) trace << "NONE";
	trace << " target_absolute_valid=";
	for (int index = 0;
		 index < static_cast<int>(rawMarginal.absoluteValidity.size()); index++)
	{
		if (index) trace << ";";
		trace << rawMarginal.absoluteValidity[index];
	}
	if (rawMarginal.absoluteValidity.empty()) trace << "NONE";
	trace << " target_covariance_row_major=";
	for (int row = 0; row < rawMarginal.covariance.rows(); row++)
	for (int column = 0; column < rawMarginal.covariance.cols(); column++)
	{
		if (row || column) trace << ";";
		trace << rawMarginal.covariance(row, column);
	}
	if (rawMarginal.covariance.rows() == 0) trace << "NONE";
	trace << " target_mean=";
	for (int index = 0; index < rawMarginal.mean.size(); index++)
	{
		if (index) trace << ";";
		trace << rawMarginal.mean(index);
	}
	if (rawMarginal.mean.size() == 0) trace << "NONE";
	trace << " status=" << (rawMarginal.valid ? "ACCEPTED" : "REJECTED")
		<< " reason=" << (rawMarginal.failureReason.empty()
			? "NONE" : rawMarginal.failureReason)
		<< " source=FINAL_ACCEPTED_H_R_F_Q_SQUARE_ROOT feedback=0";
	traceZhangE18IntegerDiagnostics(
		trace, time, rawMarginal, "RAW_SQUARE_ROOT_");

	const ZhangRawSquareRootTargetMarginal persistentMarginal =
		capture->second.currentPersistentRawTargetMarginal();
	trace << "\nZHANG_E19_PERSISTENT_RAW_TARGET_WINDOW time="
		<< time.to_string(0)
		<< " valid=" << persistentMarginal.valid
		<< " requested_targets=" << persistentMarginal.requestedTargetCount
		<< " information_rank=" << persistentMarginal.informationRank
		<< " unresolved_gauge_rank="
		<< persistentMarginal.unresolvedGaugeRank
		<< " quotient_valid_rank=" << persistentMarginal.quotientValidRank
		<< " absolute_valid_rank=" << persistentMarginal.absoluteValidRank
		<< " exact_constraints_applied="
		<< persistentMarginal.exactConstraintsApplied
		<< " batch_orthogonal_residual_dof="
		<< persistentMarginal.batchOrthogonalDof
		<< " batch_orthogonal_residual_squared_norm="
		<< persistentMarginal.batchOrthogonalSquaredNorm
		<< " boundary_rows=" << persistentMarginal.storedRows
		<< " boundary_columns=" << persistentMarginal.storedColumns
		<< " target_identities=";
	for (int index = 0;
		 index < static_cast<int>(persistentMarginal.identities.size()); index++)
	{
		if (index) trace << ";";
		trace << persistentMarginal.identities[index];
	}
	if (persistentMarginal.identities.empty()) trace << "NONE";
	trace << " target_gauge_identities=";
	for (int index = 0;
		 index < static_cast<int>(persistentMarginal.gaugeIdentities.size()); index++)
	{
		if (index) trace << ";";
		trace << (persistentMarginal.gaugeIdentities[index].empty()
			? "ABSOLUTE" : persistentMarginal.gaugeIdentities[index]);
	}
	if (persistentMarginal.gaugeIdentities.empty()) trace << "NONE";
	trace << " target_absolute_valid=";
	for (int index = 0;
		 index < static_cast<int>(persistentMarginal.absoluteValidity.size()); index++)
	{
		if (index) trace << ";";
		trace << persistentMarginal.absoluteValidity[index];
	}
	if (persistentMarginal.absoluteValidity.empty()) trace << "NONE";
	trace << " target_covariance_row_major=";
	for (int row = 0; row < persistentMarginal.covariance.rows(); row++)
	for (int column = 0;
		 column < persistentMarginal.covariance.cols(); column++)
	{
		if (row || column) trace << ";";
		trace << persistentMarginal.covariance(row, column);
	}
	if (persistentMarginal.covariance.size() == 0) trace << "NONE";
	trace << " target_mean=";
	for (int index = 0; index < persistentMarginal.mean.size(); index++)
	{
		if (index) trace << ";";
		trace << persistentMarginal.mean(index);
	}
	if (persistentMarginal.mean.size() == 0) trace << "NONE";
	trace << " status=" << (persistentMarginal.valid
			? "ACCEPTED" : "REJECTED")
		<< " reason=" << (persistentMarginal.failureReason.empty()
			? "NONE" : persistentMarginal.failureReason)
		<< " source=PERSISTENT_RAW_TARGET_EXACT_CONSTRAINT feedback=0";
	traceZhangE18IntegerDiagnostics(
		trace, time, persistentMarginal, "PERSISTENT_RAW_TARGET_");
	for (const auto& scale :
		capture->second.innovationScaleDiagnostics())
	{
		trace << "\nZHANG_E19_INNOVATION_SCALE_GROUP time="
			<< time.to_string(0)
			<< " group=" << scale.identity
			<< " blocks=" << scale.blocks
			<< " marginal_samples=" << scale.samples
			<< " marginal_standardised_squared_sum="
			<< scale.marginalStandardisedSquaredSum
			<< " predictive_covariance_scale_mle="
			<< scale.predictiveCovarianceScaleMle()
			<< " maximum_absolute_prefit_ratio="
			<< scale.maximumAbsoluteRatio
			<< " statistic=MARGINAL_PREFIT_RATIO_NOT_JOINT_CHI_SQUARE"
			<< " role=TRAINING_HOLDOUT_DIAGNOSTIC_ONLY feedback=0";
	}

	const ZhangIncrementalTargetMarginal marginal =
		capture->second.currentIncrementalTargetMarginal();
	trace << "\nZHANG_E18_INCREMENTAL_INTEGER_WINDOW time="
		  << time.to_string(0)
		  << " valid=" << marginal.valid
		  << " quotient_valid="
		  << (marginal.valid && marginal.quotientValidRank > 0)
		  << " absolute_datum_valid="
		  << (marginal.valid
			&& marginal.absoluteValidRank == marginal.requestedTargetCount)
		  << " requested_targets=" << marginal.requestedTargetCount
		  << " unresolved_gauge_rank=" << marginal.unresolvedGaugeRank
		  << " information_rank=" << marginal.informationRank
		  << " quotient_valid_rank=" << marginal.quotientValidRank
		  << " absolute_valid_rank=" << marginal.absoluteValidRank
		  << " orthogonal_residual_dof=" << marginal.orthogonalResidualDof
		  << " orthogonal_residual_squared_norm="
		  << marginal.orthogonalResidualSquaredNorm
		  << " separator_rows=" << marginal.storedRows
		  << " separator_columns=" << marginal.storedColumns
		  << " maximum_separator_rows=" << marginal.maximumStoredRows
		  << " maximum_separator_columns=" << marginal.maximumStoredColumns
		  << " target_identities=";
	if (marginal.identities.empty())
	{
		trace << "NONE";
	}
	else
	{
		for (int index = 0; index < static_cast<int>(marginal.identities.size()); index++)
		{
			if (index > 0) trace << ";";
			trace << marginal.identities[index];
		}
	}
	trace << " target_gauge_identities=";
	if (marginal.gaugeIdentities.empty())
	{
		trace << "NONE";
	}
	else
	{
		for (int index = 0;
			 index < static_cast<int>(marginal.gaugeIdentities.size());
			 index++)
		{
			if (index > 0) trace << ";";
			trace << (marginal.gaugeIdentities[index].empty()
				? "ABSOLUTE" : marginal.gaugeIdentities[index]);
		}
	}
	trace << " target_absolute_valid=";
	if (marginal.absoluteValidity.empty())
	{
		trace << "NONE";
	}
	else
	{
		for (int index = 0;
			 index < static_cast<int>(marginal.absoluteValidity.size());
			 index++)
		{
			if (index > 0) trace << ";";
			trace << marginal.absoluteValidity[index];
		}
	}
	trace << " target_coordinate_offsets=";
	if (marginal.coordinateOffsets.empty())
	{
		trace << "NONE";
	}
	else
	{
		for (int index = 0;
			 index < static_cast<int>(marginal.coordinateOffsets.size());
			 index++)
		{
			if (index > 0) trace << ";";
			trace << marginal.coordinateOffsets[index];
		}
	}
	trace << " target_covariance_row_major=";
	if (marginal.covariance.rows() == 0)
	{
		trace << "NONE";
	}
	else
	{
		for (int row = 0; row < marginal.covariance.rows(); row++)
		for (int column = 0; column < marginal.covariance.cols(); column++)
		{
			if (row > 0 || column > 0) trace << ";";
			trace << marginal.covariance(row, column);
		}
	}
	trace
		  << " target_mean=";
    if (marginal.mean.size() == 0)
    {
        trace << "NONE";
    }
    else
    {
        for (int index = 0; index < marginal.mean.size(); index++)
        {
            if (index > 0) trace << ";";
            trace << marginal.mean(index);
        }
    }
    trace << " target_variance_diagonal=";
    if (marginal.covariance.rows() == 0)
    {
        trace << "NONE";
    }
	else
	{
		for (int index = 0; index < marginal.covariance.rows(); index++)
		{
			if (index > 0) trace << ";";
			trace << marginal.covariance(index, index);
		}
	}
	trace << " target_fractional_mean=";
	if (marginal.fractionalMean.size() == 0)
	{
		trace << "NONE";
	}
	else
	{
		for (int index = 0; index < marginal.fractionalMean.size(); index++)
		{
			if (index > 0) trace << ";";
			trace << marginal.fractionalMean(index);
		}
	}
	trace << " status=" << (marginal.valid ? "ACCEPTED" : "REJECTED")
		  << " reason="
		  << (marginal.failureReason.empty() ? "NONE" : marginal.failureReason)
		  << " source=INCREMENTAL_TARGET_SEPARATOR feedback=0";

	// Compare only identical physical coordinates.  A covariance difference is
	// otherwise contaminated by a datum or coordinate change and cannot
	// distinguish stochastic scaling from information discarded by the
	// epoch-local separator.
	std::vector<int> rawCommon;
	std::vector<int> incrementalCommon;
	std::vector<std::string> commonIdentities;
	if (rawMarginal.valid && marginal.valid)
	{
		for (int rawIndex = 0;
			 rawIndex < static_cast<int>(rawMarginal.identities.size());
			 rawIndex++)
		{
			for (int incrementalIndex = 0;
				 incrementalIndex < static_cast<int>(marginal.identities.size());
				 incrementalIndex++)
			{
				if (rawMarginal.identities[rawIndex]
						!= marginal.identities[incrementalIndex]
				 || rawMarginal.gaugeIdentities[rawIndex]
						!= marginal.gaugeIdentities[incrementalIndex]
				 || rawMarginal.absoluteValidity[rawIndex]
						!= marginal.absoluteValidity[incrementalIndex])
				{
					continue;
				}
				rawCommon.push_back(rawIndex);
				incrementalCommon.push_back(incrementalIndex);
				commonIdentities.push_back(rawMarginal.identities[rawIndex]);
				break;
			}
		}
	}
	const int commonCount = rawCommon.size();
	VectorXd rawCommonMean(commonCount);
	VectorXd incrementalCommonMean(commonCount);
	MatrixXd rawCommonCovariance(commonCount, commonCount);
	MatrixXd incrementalCommonCovariance(commonCount, commonCount);
	for (int row = 0; row < commonCount; row++)
	{
		rawCommonMean(row) = rawMarginal.mean(rawCommon[row]);
		incrementalCommonMean(row) = marginal.mean(incrementalCommon[row]);
		for (int column = 0; column < commonCount; column++)
		{
			rawCommonCovariance(row, column) = rawMarginal.covariance(
				rawCommon[row], rawCommon[column]);
			incrementalCommonCovariance(row, column) = marginal.covariance(
				incrementalCommon[row], incrementalCommon[column]);
		}
	}
	auto informationMatrix = [](const MatrixXd& covariance,
		MatrixXd& information, int& rank)
	{
		rank = 0;
		information = MatrixXd::Zero(covariance.rows(), covariance.cols());
		if (covariance.rows() == 0 || covariance.rows() != covariance.cols())
		{
			return false;
		}
		Eigen::SelfAdjointEigenSolver<MatrixXd> spectrum(
			0.5 * (covariance + covariance.transpose()));
		if (spectrum.info() != Eigen::Success)
		{
			return false;
		}
		const double maximum = spectrum.eigenvalues().cwiseAbs().maxCoeff();
		const double threshold = std::max(1e-14, maximum * 1e-12);
		VectorXd inverse = VectorXd::Zero(covariance.rows());
		for (int index = 0; index < covariance.rows(); index++)
		{
			if (spectrum.eigenvalues()(index) > threshold)
			{
				inverse(index) = 1 / spectrum.eigenvalues()(index);
				rank++;
			}
		}
		information = spectrum.eigenvectors() * inverse.asDiagonal()
			* spectrum.eigenvectors().transpose();
		information = 0.5 * (information + information.transpose());
		return information.allFinite() && rank > 0;
	};
	MatrixXd rawInformation;
	MatrixXd incrementalInformation;
	int rawInformationRank = 0;
	int incrementalInformationRank = 0;
	const bool comparisonValid = commonCount > 0
		&& informationMatrix(rawCommonCovariance,
			rawInformation, rawInformationRank)
		&& informationMatrix(incrementalCommonCovariance,
			incrementalInformation, incrementalInformationRank);
	const MatrixXd covarianceDifference = comparisonValid
		? incrementalCommonCovariance - rawCommonCovariance : MatrixXd();
	const MatrixXd informationDifference = comparisonValid
		? incrementalInformation - rawInformation : MatrixXd();
	const VectorXd meanDifference = comparisonValid
		? incrementalCommonMean - rawCommonMean : VectorXd();
	const double covarianceRelativeDifference = comparisonValid
		? covarianceDifference.norm()
			/ std::max(1e-30, rawCommonCovariance.norm())
		: std::numeric_limits<double>::quiet_NaN();
	const double informationRelativeDifference = comparisonValid
		? informationDifference.norm()
			/ std::max(1e-30, rawInformation.norm())
		: std::numeric_limits<double>::quiet_NaN();
	trace << "\nZHANG_E19_TARGET_INFORMATION_COMPARISON time="
		<< time.to_string(0)
		<< " valid=" << comparisonValid
		<< " common_target_count=" << commonCount
		<< " raw_information_rank=" << rawInformationRank
		<< " incremental_information_rank=" << incrementalInformationRank
		<< " covariance_relative_difference="
		<< covarianceRelativeDifference
		<< " information_relative_difference="
		<< informationRelativeDifference
		<< " covariance_trace_ratio="
		<< (comparisonValid && rawCommonCovariance.trace() > 0
			? incrementalCommonCovariance.trace()
				/ rawCommonCovariance.trace()
			: std::numeric_limits<double>::quiet_NaN())
		<< " information_trace_ratio="
		<< (comparisonValid && rawInformation.trace() > 0
			? incrementalInformation.trace() / rawInformation.trace()
			: std::numeric_limits<double>::quiet_NaN())
		<< " common_target_identities=";
	for (int index = 0; index < static_cast<int>(commonIdentities.size()); index++)
	{
		if (index) trace << ";";
		trace << commonIdentities[index];
	}
	if (commonIdentities.empty()) trace << "NONE";
	auto traceVector = [&trace](const VectorXd& vector)
	{
		for (int index = 0; index < vector.size(); index++)
		{
			if (index) trace << ";";
			trace << vector(index);
		}
		if (vector.size() == 0) trace << "NONE";
	};
	auto traceMatrix = [&trace](const MatrixXd& matrix)
	{
		for (int row = 0; row < matrix.rows(); row++)
		for (int column = 0; column < matrix.cols(); column++)
		{
			if (row || column) trace << ";";
			trace << matrix(row, column);
		}
		if (matrix.size() == 0) trace << "NONE";
	};
	trace << " raw_mean=";
	traceVector(rawCommonMean);
	trace << " incremental_mean=";
	traceVector(incrementalCommonMean);
	trace << " mean_difference=";
	traceVector(meanDifference);
	trace << " raw_covariance=";
	traceMatrix(rawCommonCovariance);
	trace << " incremental_covariance=";
	traceMatrix(incrementalCommonCovariance);
	trace << " covariance_difference=";
	traceMatrix(covarianceDifference);
	trace << " raw_information=";
	traceMatrix(rawInformation);
	trace << " incremental_information=";
	traceMatrix(incrementalInformation);
	trace << " information_difference=";
	traceMatrix(informationDifference);
	trace << " status=" << (comparisonValid ? "EVALUATED" : "REJECTED")
		<< " reason=" << (comparisonValid ? "NONE" : "NO_COMMON_COORDINATE")
		<< " incremental_role=DIAGNOSTIC_ONLY feedback=0";
	traceZhangE18IntegerDiagnostics(
		trace, time, marginal, "TARGET_INCREMENT_");
}

bool zhangPppArUsesObservable(E_Sys sys, E_ObsCode code)
{
    auto it = acsConfig.zhangPppAr.baseline_observables.find(sys);
    if (it == acsConfig.zhangPppAr.baseline_observables.end())
    {
        return false;
    }
    return std::find(it->second.begin(), it->second.end(), code) != it->second.end();
}

void recordZhangExactPhaseTransform(
    GTime         time,
    E_Sys         sys,
    E_ObsCode     code,
    const SatSys& satellite,
    double        correctionChangeMetres
)
{
    recordZhangExactPhaseTransforms(
        time, sys, code, {{satellite, correctionChangeMetres}}
    );
}

void recordZhangExactPhaseTransforms(
    GTime                          time,
    E_Sys                          sys,
    E_ObsCode                      code,
    const map<SatSys, double>&     correctionChangesMetres
)
{
    if (!acsConfig.zhangPppAr.output_products ||
        correctionChangesMetres.empty())
    {
        return;
    }
    const double lambda = wavelength(sys, code);
    if (lambda <= 0)
    {
        return;
    }

    map<SatSys, double> cycleChanges;
    for (const auto& [satellite, metres] : correctionChangesMetres)
    {
        cycleChanges[satellite] = metres / lambda;
    }
    auto preserved = satelliteDatumManager(sys, code).applyDynamicTreeTransform(
        cycleChanges
    );
    const bool houOsbLike =
        acsConfig.zhangPppAr.product_mode == "HOU_OSB_LIKE";
    for (const auto& [satellite, cycleChange] : cycleChanges)
    {
        ProductKey key{satellite, code};
        auto& state = continuityMap[key];
        initialiseContinuityState(key, state);
        if (houOsbLike)
        {
            // The complete affine offset defines the fixed Hou product
            // coordinate.  A pure internal-tree exchange is never a product
            // discontinuity, even when its current float value is fractional.
            state.applyHouProductTransform(cycleChange);
        }
        else if (preserved[satellite])
        {
            state.resetReason = "component_gauge_s_transform";
            state.fractionalShiftCycles +=
                cycleChange - std::llround(cycleChange);
        }
        else
        {
            state.applyExactTransform(
                time,
                cycleChange,
                acsConfig.zhangPppAr.stabilization_epochs
            );
        }
    }
}

void recordZhangPhaseReinitialisation(
    GTime                         time,
    E_Sys                         sys,
    const vector<E_ObsCode>&      observables,
    const string&                 reason,
    const set<SatSys>&            affectedSatellites
)
{
    if (!acsConfig.zhangPppAr.output_products)
    {
        return;
    }

    for (E_ObsCode code : observables)
    {
        satelliteDatumManager(sys, code).markDynamicAlignmentUnknown(
            affectedSatellites
        );
        for (auto& [key, state] : continuityMap)
        {
            if (key.satellite.sys != sys || key.observable != code)
            {
                continue;
            }
            if (affectedSatellites.find(key.satellite) == affectedSatellites.end())
            {
                continue;
            }
            // This is a dynamic estimation-coordinate reset, not a satellite
            // product phase discontinuity.  Preserve the product counter,
            // version, integer shift and promoted satellite relations.
            state.resetReason = "dynamic_alignment_unknown:" + reason;
            state.hasFixedDatum = false;
            state.stabilizationRemaining =
                acsConfig.zhangPppAr.stabilization_epochs;
        }
    }
}

bool promoteZhangSatelliteProductRelation(
    GTime              time,
    E_Sys              sys,
    E_ObsCode          code,
    const SatSys&      a,
    const SatSys&      b,
    long long          integerDifferenceCycles,
    const string&      provenance
)
{
    return promoteZhangSatelliteProductRelationDetailed(
        time, sys, code, a, b, integerDifferenceCycles, provenance
    ).accepted;
}

ZhangProductRelationEvent promoteZhangSatelliteProductRelationDetailed(
    GTime              time,
    E_Sys              sys,
    E_ObsCode          code,
    const SatSys&      a,
    const SatSys&      b,
    long long          integerDifferenceCycles,
    const string&      provenance
)
{
    auto& manager = satelliteDatumManager(sys, code);
    long long existingDifference = 0;
    bool relationKnown = manager.relation(a, b, existingDifference);
    ZhangProductRelationEvent event;

    if (relationKnown && existingDifference != integerDifferenceCycles &&
        acsConfig.zhangPppAr.conflict_quarantine)
    {
        SatSys trustedAnchor;
        if (provenance.rfind("G_sat_", 0) == 0)
        {
            trustedAnchor = a;
        }
        event = manager.quarantineCurrentAlignment(a, b, trustedAnchor);
    }
    else if (!relationKnown &&
             acsConfig.zhangPppAr.promotion_confirmation_epochs > 1)
    {
        SatSys canonicalA = a;
        SatSys canonicalB = b;
        int segmentA = manager.status(a, false).phaseSegment;
        int segmentB = manager.status(b, false).phaseSegment;
        long long canonicalDifference = integerDifferenceCycles;
        if (canonicalB < canonicalA)
        {
            std::swap(canonicalA, canonicalB);
            std::swap(segmentA, segmentB);
            canonicalDifference = -canonicalDifference;
        }
        PromotionEvidenceKey key{
            sys, code, canonicalA, segmentA, canonicalB, segmentB
        };
        auto& evidence = promotionEvidence[key];
        long int epoch = static_cast<long int>(
            std::llround(time.bigTime)
        );
        double maxGap =
            acsConfig.zhangPppAr.promotion_confirmation_max_gap_seconds;
        bool sameSequence =
            evidence.confirmations > 0 &&
            evidence.difference == canonicalDifference &&
            epoch != evidence.lastEpoch &&
            (maxGap <= 0 || epoch - evidence.lastEpoch <= maxGap);
        if (!sameSequence && epoch != evidence.lastEpoch)
        {
            evidence.confirmations = 0;
        }
        if (epoch != evidence.lastEpoch)
        {
            evidence.difference = canonicalDifference;
            evidence.lastEpoch = epoch;
            evidence.confirmations++;
        }
        event.type = ZhangProductRelationEventType::PENDING_CONFIRMATION;
        event.confirmationCount = evidence.confirmations;
        event.confirmationRequired =
            acsConfig.zhangPppAr.promotion_confirmation_epochs;
        if (evidence.confirmations >= event.confirmationRequired)
        {
            promotionEvidence.erase(key);
            event = manager.promoteRelationDetailed(
                a, b, integerDifferenceCycles, provenance, true
            );
        }
    }
    else
    {
        event = manager.promoteRelationDetailed(
            a, b, integerDifferenceCycles, provenance, true
        );
    }

    const char* status = "REJECTED_INCONSISTENT";
    if (event.accepted)
    {
        status = "ACCEPTED";
    }
    else if (event.type ==
             ZhangProductRelationEventType::PENDING_CONFIRMATION)
    {
        status = "PENDING_CONFIRMATION";
    }
    else if (event.type ==
             ZhangProductRelationEventType::CURRENT_ALIGNMENT_QUARANTINED)
    {
        status = "QUARANTINED_CURRENT_ALIGNMENT";
    }
    std::ostringstream message;
    message << "ZHANG_PRODUCT_RELATION_PROMOTION time=" << time.to_string(0)
            << " system=" << enum_to_string(sys)
            << " observable=" << enum_to_string(code)
            << " satellite_a=" << a.id()
            << " satellite_b=" << b.id()
            << " integer_difference=" << integerDifferenceCycles
            << " status=" << status
            << " event_type=" << zhangProductRelationEventName(event.type)
            << " old_component_size_a=" << event.oldComponentSizeA
            << " old_component_size_b=" << event.oldComponentSizeB
            << " new_component_size=" << event.newComponentSize
            << " confirmation_count=" << event.confirmationCount
            << " confirmation_required=" << event.confirmationRequired
            << " quarantined_satellite="
            << (event.quarantinedSatellite.sys == E_Sys::NONE
                    ? "NONE" : event.quarantinedSatellite.id())
            << " provenance=" << provenance;
    if (event.accepted)
    {
        BOOST_LOG_TRIVIAL(info) << message.str();
    }
    else if (event.type == ZhangProductRelationEventType::CONFLICT_REJECTED)
    {
        BOOST_LOG_TRIVIAL(error) << message.str();
    }
    else if (event.type ==
             ZhangProductRelationEventType::CURRENT_ALIGNMENT_QUARANTINED)
    {
        BOOST_LOG_TRIVIAL(warning) << message.str();
    }
    else
    {
        BOOST_LOG_TRIVIAL(info) << message.str();
    }
    return event;
}

ZhangProductRelationEvent relinkZhangSatelliteProductRelation(
    GTime              time,
    E_Sys              sys,
    E_ObsCode          code,
    const SatSys&      anchor,
    const SatSys&      satellite,
    long long          currentDifferenceCycles,
    const string&      provenance
)
{
    auto& manager = satelliteDatumManager(sys, code);
    int segmentA = manager.status(anchor, false).phaseSegment;
    int segmentB = manager.status(satellite, false).phaseSegment;
    PromotionEvidenceKey key{
        sys, code, anchor, segmentA, satellite, segmentB
    };
    auto& evidence = relinkEvidence[key];
    long int epoch = static_cast<long int>(std::llround(time.bigTime));
    double maxGap =
        acsConfig.zhangPppAr.promotion_confirmation_max_gap_seconds;
    bool sameSequence =
        evidence.confirmations > 0 &&
        evidence.difference == currentDifferenceCycles &&
        epoch != evidence.lastEpoch &&
        (maxGap <= 0 || epoch - evidence.lastEpoch <= maxGap);
    if (!sameSequence && epoch != evidence.lastEpoch)
    {
        evidence.confirmations = 0;
    }
    if (epoch != evidence.lastEpoch)
    {
        evidence.difference = currentDifferenceCycles;
        evidence.lastEpoch = epoch;
        evidence.confirmations++;
    }

    ZhangProductRelationEvent event;
    event.type = ZhangProductRelationEventType::PENDING_CONFIRMATION;
    event.confirmationCount = evidence.confirmations;
    event.confirmationRequired = std::max(
        1, acsConfig.zhangPppAr.promotion_confirmation_epochs
    );
    if (evidence.confirmations >= event.confirmationRequired)
    {
        relinkEvidence.erase(key);
        event = manager.realignRelation(
            anchor, satellite, currentDifferenceCycles, provenance
        );
    }
    BOOST_LOG_TRIVIAL(info)
        << "ZHANG_PRODUCT_RELATION_PROMOTION time=" << time.to_string(0)
        << " system=" << enum_to_string(sys)
        << " observable=" << enum_to_string(code)
        << " satellite_a=" << anchor.id()
        << " satellite_b=" << satellite.id()
        << " integer_difference=" << currentDifferenceCycles
        << " status="
        << (event.accepted
                ? "ACCEPTED"
                : event.type == ZhangProductRelationEventType::PENDING_CONFIRMATION
                    ? "PENDING_CONFIRMATION"
                    : "REJECTED_INCONSISTENT")
        << " event_type=" << zhangProductRelationEventName(event.type)
        << " old_component_size_a=" << event.oldComponentSizeA
        << " old_component_size_b=" << event.oldComponentSizeB
        << " new_component_size=" << event.newComponentSize
        << " confirmation_count=" << event.confirmationCount
        << " confirmation_required=" << event.confirmationRequired
        << " provenance=" << provenance;
    return event;
}

std::size_t quarantineZhangSatelliteProductAlignments(
    GTime                   time,
    E_Sys                   sys,
    E_ObsCode               code,
    const std::set<SatSys>& satellites,
    const SatSys&           trustedAnchor,
    const std::string&      reason
)
{
    auto quarantined = satelliteDatumManager(sys, code)
        .quarantineCurrentAlignments(satellites, trustedAnchor);
    BOOST_LOG_TRIVIAL(info)
        << "ZHANG_HELD_PRODUCT_QUARANTINE time=" << time.to_string(0)
        << " system=" << enum_to_string(sys)
        << " observable=" << enum_to_string(code)
        << " support_satellites=" << satellites.size()
        << " quarantined_satellites=" << quarantined
        << " trusted_anchor="
        << (trustedAnchor.sys == E_Sys::NONE ? "NONE" : trustedAnchor.id())
        << " reason=" << reason;
    return quarantined;
}

vector<ZhangSatelliteDatumComponent> zhangSatelliteDatumComponents(
    E_Sys sys,
    E_ObsCode code
)
{
    return satelliteDatumManager(sys, code).components();
}

ZhangCurrentAlignmentState zhangSatelliteAlignmentState(
    E_Sys sys,
    E_ObsCode code,
    const SatSys& satellite
)
{
    return satelliteDatumManager(sys, code).alignmentState(satellite);
}

ZhangSatelliteDatumStatus zhangSatelliteDatumStatus(
    E_Sys sys,
    E_ObsCode code,
    const SatSys& satellite
)
{
    return satelliteDatumManager(sys, code).status(satellite, false);
}

bool queryZhangSatelliteProductRelation(
    E_Sys sys,
    E_ObsCode code,
    const SatSys& a,
    const SatSys& b,
    long long& differenceCycles
)
{
    return satelliteDatumManager(sys, code).relation(
        a, b, differenceCycles
    );
}

void recordZhangSatellitePhaseDiscontinuity(
    GTime                         time,
    E_Sys                         sys,
    const vector<E_ObsCode>&      observables,
    const SatSys&                 satellite,
    const string&                 reason
)
{
    for (E_ObsCode code : observables)
    {
        satelliteDatumManager(sys, code).recordSatelliteDiscontinuity(satellite);
        ProductKey key{satellite, code};
        auto& state = continuityMap[key];
        initialiseContinuityState(key, state);
        state.reinitialise(
            time,
            "satellite_phase_discontinuity:" + reason,
            acsConfig.zhangPppAr.stabilization_epochs
        );
    }
}

void writeZhangInternalProducts(
    Trace&         trace,
    const KFState& floatState,
    const KFState& fixedState,
    int            newlyFixed,
    bool           integerDatumComplete,
    bool           fixedBranchValid,
    bool           networkIntegerReady
)
{
    if (!acsConfig.zhangPppAr.output_products)
    {
        return;
    }
	const bool houOsbLike =
		acsConfig.zhangPppAr.product_mode == "HOU_OSB_LIKE";
	if (houOsbLike && acsConfig.zhangPppAr.output_diagnostics)
	{
		trace << "\nZHANG_HOU_OSB_LIKE_PRODUCT_MODEL time="
			<< fixedState.time.to_string(0)
			<< " correction_definition=CLOCK_MINUS_PHASE"
			<< " integer_source=NETWORK_CYCLE_LATTICE"
			<< " product_datum=RELATIVE_PER_SYSTEM_SIGNAL"
			<< " absolute_satellite_integer_required=0"
			<< " user_ambiguity_datum=ONE_REFERENCE_PER_SYSTEM_SIGNAL"
			<< " network_integer_ready=" << networkIntegerReady
			<< " fixed_branch_transactional="
			<< acsConfig.zhangPppAr.transactional_integer_fixing;
	}

    vector<ZhangInternalProduct> epochProducts;
    auto writeSolution = [&](const KFState& state, const string& solution)
    {
        for (const auto& [phaseKey, phaseIndex] : state.kfIndexMap)
        {
            if (phaseKey.type != KF::PHASE_BIAS ||
                phaseKey.Sat.prn <= 0 ||
                !phaseKey.str.empty() ||
                !zhangGraphProductSatelliteActive(fixedState, phaseKey.Sat) ||
                !zhangPppArUsesObservable(
                    phaseKey.Sat.sys,
                    static_cast<E_ObsCode>(phaseKey.num)
                ))
            {
                continue;
            }

            KFKey clockKey;
            clockKey.type = KF::SAT_CLOCK;
            clockKey.Sat  = phaseKey.Sat;
            auto clockIt = state.kfIndexMap.find(clockKey);
            if (clockIt == state.kfIndexMap.end())
            {
                continue;
            }

            E_ObsCode code = static_cast<E_ObsCode>(phaseKey.num);
            double lambda = wavelength(phaseKey.Sat.sys, code);
            if (lambda <= 0)
            {
                continue;
            }

            ProductKey productKey{phaseKey.Sat, code};
            auto& continuity = continuityMap[productKey];
            initialiseContinuityState(productKey, continuity);
            continuity.advanceEpoch(state.time);

            ZhangGraphIntegerContext graphContext;
            bool structureValid =
                zhangGraphIntegerContext(
                    fixedState, phaseKey.Sat.sys, graphContext
                ) &&
                zhangCanonicalIntegerAudit(graphContext.basis).valid;
            ZhangSatelliteDatumStatus datumStatus =
                satelliteDatumManager(phaseKey.Sat.sys, code).status(
                    phaseKey.Sat, structureValid
                );
            int clockIndex = clockIt->second;
            double clock = state.x(clockIndex);
            double rawPhase = state.x(phaseIndex);
            double houAlignmentCycles =
                static_cast<double>(continuity.integerShiftCycles) +
                continuity.fractionalShiftCycles;
            double productAlignmentCycles = houOsbLike
                ? houAlignmentCycles
                : static_cast<double>(datumStatus.alignmentCycles);
            double emittedPhase =
                rawPhase + productAlignmentCycles * lambda;
            double covariance = state.P(clockIndex, phaseIndex);
            double clockVariance = state.P(clockIndex, clockIndex);
            double phaseVariance = state.P(phaseIndex, phaseIndex);
            double correction = zhangUserPhaseCorrectionValue(
                clock,
                rawPhase,
                lambda,
                datumStatus.alignmentCycles
            );
            double correctionVariance =
                clockVariance + phaseVariance - 2 * covariance;
			if (houOsbLike)
			{
				const auto target = zhangHouOsbLikePhaseCorrectionTarget(
					state.x.size(), clockIndex, phaseIndex, lambda,
					productAlignmentCycles);
				correction = target.value(state.x);
				correctionVariance = target.variance(state.P);
			}

            ZhangInternalProduct product;
            product.time = state.time;
            product.satellite = phaseKey.Sat;
            product.observable = code;
            product.solution = solution;
            product.clock_m = clock;
            product.clock_sigma_m =
                std::sqrt(std::max(0.0, state.P(clockIndex, clockIndex)));
            product.phase_m = emittedPhase;
            product.phase_sigma_m =
                std::sqrt(std::max(0.0, state.P(phaseIndex, phaseIndex)));
            product.clock_phase_covariance_m2 = covariance;
            product.correction_m = correction;
            product.correction_sigma_m =
                std::sqrt(std::max(0.0, correctionVariance));
            bool productPrecisionValid =
                acsConfig.zhangPppAr.maximum_pppar_correction_sigma_m <= 0 ||
                product.correction_sigma_m <=
                    acsConfig.zhangPppAr.maximum_pppar_correction_sigma_m;
            bool productIntegerCoordinateReady = houOsbLike
                ? networkIntegerReady && structureValid
                : datumStatus.integerValid;
            if (solution == "FIXED" && fixedBranchValid &&
                productIntegerCoordinateReady && productPrecisionValid)
            {
                // Integer validity is a per-satellite, per-signal property.
                // A newly precise product must also pass the configured
                // stabilization window before it can be consumed by PPP-AR.
                continuity.markFixed(
                    state.time, acsConfig.zhangPppAr.stabilization_epochs
                );
            }
            else if (solution == "FIXED" && !productPrecisionValid)
            {
                continuity.markIntegerPrecisionUnavailable(
                    "product_correction_sigma_exceeded",
                    acsConfig.zhangPppAr.stabilization_epochs
                );
            }
            else if (solution == "FIXED" && houOsbLike &&
                     !productIntegerCoordinateReady)
            {
                continuity.markIntegerPrecisionUnavailable(
                    "network_cycle_phase_not_ready",
                    acsConfig.zhangPppAr.stabilization_epochs
                );
            }
            product.discontinuity_counter = houOsbLike
                ? continuity.counter
                : datumStatus.discontinuityCounter;
            product.integer_shift_cycles = houOsbLike
                ? continuity.integerShiftCycles
                : datumStatus.alignmentCycles;
            product.fractional_shift_cycles = continuity.fractionalShiftCycles;
            product.datum_version = houOsbLike
                ? continuity.datumVersion
                : datumStatus.datumVersion;
            product.valid_from = continuity.validFrom;
            product.product_iod = continuity.iod;
            product.reset_reason = continuity.resetReason;
            ZhangCurrentAlignmentState alignmentState = houOsbLike
                ? ZhangCurrentAlignmentState::CURRENT_ALIGNMENT_VALID
                : satelliteDatumManager(
                    phaseKey.Sat.sys, code).alignmentState(phaseKey.Sat);
            // Hou products intentionally do not claim a conventional
            // satellite-only persistent integer relation.  Their integer
            // compatibility comes from the fixed network cycle lattice and
            // the explicitly transported product S-coordinate instead.
            product.persistent_relation_known = houOsbLike
                ? false
                : datumStatus.componentSize >= 2;
            product.current_alignment_state =
                zhangCurrentAlignmentStateName(alignmentState);
            product.integer_structure_valid =
                houOsbLike
                    ? structureValid
                    : datumStatus.integerStructureValid;
            product.integer_datum_continuous =
                houOsbLike
                    ? structureValid
                    : datumStatus.integerDatumContinuous;
            product.integer_precision_valid =
                solution == "FIXED" && fixedBranchValid &&
                (houOsbLike
                    ? networkIntegerReady
                    : datumStatus.integerPrecisionValid) &&
                productPrecisionValid &&
                continuity.integerValid();
            product.integer_valid =
                product.integer_structure_valid &&
                product.integer_datum_continuous &&
                product.integer_precision_valid;
            product.branch_valid =
                solution == "FLOAT" || fixedBranchValid;
            double covarianceScale = std::max(
                1.0,
                std::max(std::abs(clockVariance), std::abs(phaseVariance))
            );
            bool finite =
                std::isfinite(clock) && std::isfinite(rawPhase) &&
                std::isfinite(emittedPhase) && std::isfinite(covariance) &&
                std::isfinite(clockVariance) &&
                std::isfinite(phaseVariance) &&
                std::isfinite(correctionVariance);
            bool covarianceValid =
                clockVariance >= -1e-10 * covarianceScale &&
                phaseVariance >= -1e-10 * covarianceScale &&
                correctionVariance >= -1e-10 * covarianceScale &&
                covariance * covariance <=
                    clockVariance * phaseVariance +
                    1e-10 * covarianceScale * covarianceScale;
            product.numeric_valid = finite && covarianceValid;
            product.continuity_valid = true;
            product.ppp_usable = product.numeric_valid && product.branch_valid;
            product.pppar_usable =
                product.ppp_usable && product.integer_valid;
            product.invalid_reason = !product.branch_valid
                ? "FIXED_TRANSACTION_ABORTED"
                : (!finite
                       ? "NONFINITE_PRODUCT"
                       : (!covarianceValid
                              ? "INVALID_PRODUCT_COVARIANCE"
                              : "NONE"));
            product.integer_component_id = houOsbLike
                ? "HOU-" + enum_to_string(phaseKey.Sat.sys) + "-" +
                    enum_to_string(code) + "-NETWORK-CYCLE"
                : datumStatus.componentId;
            product.integer_datum_id = houOsbLike
                ? "HOU-" + enum_to_string(phaseKey.Sat.sys) + "-" +
                    enum_to_string(code) + "-V" +
                    std::to_string(continuity.datumVersion)
                : enum_to_string(phaseKey.Sat.sys) + "-" +
                    enum_to_string(code) + "-V" +
                    std::to_string(datumStatus.datumVersion) + "-SEG" +
                    std::to_string(datumStatus.phaseSegment);

            epochProducts.push_back(product);

            if (acsConfig.zhangPppAr.output_diagnostics)
            {
                trace << "\nZHANG_CONTINUITY_PRODUCT time=" << state.time.to_string(0)
                      << " solution=" << solution
					  << " product_mode="
					  << acsConfig.zhangPppAr.product_mode
					  << " absolute_satellite_integer_required="
					  << (!houOsbLike)
                      << " satellite=" << phaseKey.Sat.id()
                      << " observable=" << enum_to_string(code)
                      << " counter=" << continuity.counter
                      << " integer_shift_cycles=" << continuity.integerShiftCycles
                      << " fractional_shift_cycles=" << continuity.fractionalShiftCycles
                      << " datum_version=" << continuity.datumVersion
                      << " iod=" << continuity.iod
                      << " newly_fixed=" << newlyFixed
                      << " integer_datum_complete=" << integerDatumComplete
                      << " network_integer_ready=" << networkIntegerReady
                      << " persistent_relation_known="
                      << product.persistent_relation_known
                      << " current_alignment_state="
                      << product.current_alignment_state
                      << " integer_structure_valid="
                      << product.integer_structure_valid
                      << " integer_datum_continuous="
                      << product.integer_datum_continuous
                      << " integer_precision_valid="
                      << product.integer_precision_valid
                      << " integer_component_size="
                      << (houOsbLike ? 0 : datumStatus.componentSize)
                      << " integer_valid=" << product.integer_valid
                      << " numeric_valid=" << product.numeric_valid
                      << " branch_valid=" << product.branch_valid
                      << " ppp_usable=" << product.ppp_usable
                      << " pppar_usable=" << product.pppar_usable
                      << " invalid_reason=" << product.invalid_reason
                      << " reason=" << continuity.resetReason;
            }
        }
    };

    writeSolution(floatState, "FLOAT");
    writeSolution(fixedState, "FIXED");

    // Reject satellite-dependent correction jumps after removing the robust
    // per-signal common mode.  A common clock-datum change can be absorbed by
    // the user's receiver clock; a non-common jump cannot.
    map<pair<string, E_ObsCode>, vector<int>> continuityGroups;
    for (int index = 0; index < static_cast<int>(epochProducts.size()); index++)
    {
        continuityGroups[
            {epochProducts[index].solution, epochProducts[index].observable}
        ].push_back(index);
    }
    const double maximumGap = std::max(120.0, 2.5 * acsConfig.epoch_interval);
    const double maximumResidualStep =
        acsConfig.zhangPppAr.maximum_product_residual_step_m;
    for (const auto& [group, indices] : continuityGroups)
    {
        map<int, double> deltas;
        vector<double> commonModeCandidates;
        for (int index : indices)
        {
            const auto& product = epochProducts[index];
            ProductHistoryKey key{
                product.solution, product.satellite, product.observable
            };
            auto previous = productHistoryMap.find(key);
            if (previous == productHistoryMap.end() ||
                !product.numeric_valid ||
                previous->second.discontinuityCounter !=
                    product.discontinuity_counter ||
                previous->second.datumVersion != product.datum_version)
            {
                continue;
            }
            double gap = (product.time - previous->second.time).to_double();
            if (!(gap > 0) || gap > maximumGap)
            {
                continue;
            }
            double delta = product.correction_m - previous->second.correction;
            if (std::isfinite(delta))
            {
                deltas[index] = delta;
                commonModeCandidates.push_back(delta);
            }
        }

        double commonMode = 0;
        if (!commonModeCandidates.empty())
        {
            auto middle = commonModeCandidates.begin() +
                commonModeCandidates.size() / 2;
            std::nth_element(
                commonModeCandidates.begin(), middle, commonModeCandidates.end()
            );
            commonMode = *middle;
        }
        for (int index : indices)
        {
            auto& product = epochProducts[index];
            auto delta = deltas.find(index);
            double residualStep = 0;
            if (delta != deltas.end())
            {
                residualStep = std::abs(delta->second - commonMode);
                if (!std::isfinite(residualStep) ||
                    (maximumResidualStep > 0 &&
                     residualStep > maximumResidualStep))
                {
                    product.continuity_valid = false;
                    product.invalid_reason =
                        "COMMON_MODE_REMOVED_STEP_EXCEEDED";
                }
            }
            product.ppp_usable =
                product.numeric_valid && product.branch_valid &&
                product.continuity_valid;
            product.pppar_usable =
                product.ppp_usable && product.integer_valid;

            ProductHistoryKey key{
                product.solution, product.satellite, product.observable
            };
            if (product.ppp_usable)
            {
                productHistoryMap[key] = {
                    product.time,
                    product.correction_m,
                    product.discontinuity_counter,
                    product.datum_version
                };
            }
            appendProduct(product);
            if (acsConfig.zhangPppAr.output_diagnostics)
            {
                trace << "\nZHANG_PRODUCT_NUMERIC_GATE time="
                      << product.time.to_string(0)
                      << " solution=" << product.solution
                      << " satellite=" << product.satellite.id()
                      << " observable=" << enum_to_string(product.observable)
                      << " common_mode_step_m=" << commonMode
                      << " residual_step_m=" << residualStep
                      << " numeric_valid=" << product.numeric_valid
                      << " branch_valid=" << product.branch_valid
                      << " continuity_valid=" << product.continuity_valid
                      << " ppp_usable=" << product.ppp_usable
                      << " pppar_usable=" << product.pppar_usable
                      << " reason=" << product.invalid_reason;
            }
        }
    }
    appendProductCovariance(floatState, "FLOAT", fixedState);
    appendProductCovariance(fixedState, "FIXED", fixedState);
}

bool queryZhangInternalProduct(
    GTime                 time,
    const SatSys&         satellite,
    E_ObsCode             observable,
    ZhangInternalProduct& product
)
{
    if (!acsConfig.zhangPppAr.user_adapter || !loadProducts())
    {
        return false;
    }

    ProductLookupKey key{
        static_cast<long int>(std::llround(time.bigTime)),
        satellite,
        observable,
        acsConfig.zhangPppAr.product_solution
    };
    auto it = productMap.find(key);
    if (it == productMap.end())
    {
        return false;
    }
    product = it->second;
    return product.ppp_usable;
}

void updateZhangPppArUserReferences(
    Trace&       trace,
    ReceiverMap& receiverMap,
    KFState&     kfState
)
{
    if (!acsConfig.zhangPppAr.user_adapter)
    {
        return;
    }

    for (auto& [receiverId, receiver] : receiverMap)
    {
        if (!receiver.ready || receiver.obsList.empty())
        {
            continue;
        }

        for (const auto& [sys, observables] : acsConfig.zhangPppAr.baseline_observables)
        {
            for (E_ObsCode code : observables)
            {
                map<SatSys, double> candidates;
                map<SatSys, ZhangInternalProduct> products;

                for (const auto& obs : only<GObs>(receiver.obsList))
                {
                    if (obs.Sat.sys != sys || !signalUsable(obs, code))
                    {
                        continue;
                    }

                    ZhangInternalProduct product;
                    if (!queryZhangInternalProduct(kfState.time, obs.Sat, code, product))
                    {
                        continue;
                    }

                    double elevation =
                        obs.satStat_ptr ? obs.satStat_ptr->el : 0;
                    candidates[obs.Sat] = elevation;
                    products[obs.Sat] = product;
                }

                if (candidates.empty())
                {
                    continue;
                }

                UserReferenceKey key{&kfState, receiverId, sys, code};
                auto& runtime = userReferenceMap[key];

                auto bestIt = std::max_element(
                    candidates.begin(),
                    candidates.end(),
                    [](const auto& left, const auto& right)
                    { return left.second < right.second; }
                );
                SatSys selected = bestIt->first;
                if (runtime.reference.prn > 0 &&
                    candidates.find(runtime.reference) != candidates.end())
                {
                    selected = runtime.reference;
                }

                const ZhangInternalProduct& selectedProduct =
                    products.at(selected);
                bool productDatumChanged =
                    runtime.productCounter >= 0 &&
                    runtime.reference == selected &&
                    (runtime.productCounter !=
                         selectedProduct.discontinuity_counter ||
                     runtime.datumVersion !=
                         selectedProduct.datum_version);
                if (productDatumChanged)
                {
                    resetUserPhaseBlock(
                        trace,
                        kfState,
                        receiverId,
                        sys,
                        code,
                        "product discontinuity counter changed"
                    );
                    runtime.reference = {};
                }

                if (runtime.reference.prn > 0 && runtime.reference != selected)
                {
                    if (!transformUserReference(
                            trace,
                            kfState,
                            receiverId,
                            sys,
                            code,
                            runtime.reference,
                            selected
                        ))
                    {
                        resetUserPhaseBlock(
                            trace,
                            kfState,
                            receiverId,
                            sys,
                            code,
                            "ambiguity reference exchange unavailable"
                        );
                    }
                }

                for (const auto& [satellite, product] : products)
                {
                    auto datum = std::make_pair(
                        product.discontinuity_counter,
                        product.datum_version
                    );
                    auto oldDatumIt =
                        runtime.satelliteDatum.find(satellite);
                    bool satelliteDatumChanged =
                        oldDatumIt != runtime.satelliteDatum.end() &&
                        oldDatumIt->second != datum;
                    if (satelliteDatumChanged &&
                        satellite != selected)
                    {
                        resetUserAmbiguity(
                            trace,
                            kfState,
                            receiverId,
                            satellite,
                            code,
                            "satellite product datum changed"
                        );
                    }
                    runtime.satelliteDatum[satellite] = datum;
                }

                if (runtime.reference != selected ||
                    productDatumChanged)
                {
                    trace << "\nZHANG_USER_REFERENCE time=" << kfState.time.to_string(0)
                          << " receiver=" << receiverId
                          << " sys=" << enum_to_string(sys)
                          << " observable=" << enum_to_string(code)
                          << " old="
                          << (runtime.reference.prn > 0
                                  ? runtime.reference.id()
                                  : string("NONE"))
                          << " new=" << selected.id()
                          << " product_counter="
                          << selectedProduct.discontinuity_counter
                          << " datum_version="
                          << selectedProduct.datum_version
                          << " product_datum_changed=" << productDatumChanged;
                }

                runtime.reference = selected;
                runtime.productCounter =
                    selectedProduct.discontinuity_counter;
                runtime.datumVersion =
                    selectedProduct.datum_version;
            }
        }
    }
}

bool zhangPppArUserReferenceAmbiguity(
    const KFState&     kfState,
    const string&      receiver,
    const SatSys&      satellite,
    E_ObsCode          observable
)
{
    if (!acsConfig.zhangPppAr.user_adapter)
    {
        return false;
    }

    UserReferenceKey key{
        &kfState,
        receiver,
        satellite.sys,
        observable
    };
    auto it = userReferenceMap.find(key);
    return it != userReferenceMap.end() &&
           it->second.reference == satellite;
}

bool zhangPppArUserAmbiguityIntegerValid(
    const KFState&     kfState,
    const string&      receiver,
    const SatSys&      satellite,
    E_ObsCode          observable
)
{
    if (!acsConfig.zhangPppAr.user_adapter)
    {
        return true;
    }

    UserReferenceKey key{
        &kfState,
        receiver,
        satellite.sys,
        observable
    };
    auto referenceIt = userReferenceMap.find(key);
    if (referenceIt == userReferenceMap.end() ||
        referenceIt->second.reference.prn <= 0)
    {
        return false;
    }

    ZhangInternalProduct satelliteProduct;
    ZhangInternalProduct referenceProduct;
    if (!queryZhangInternalProduct(
            kfState.time,
            satellite,
            observable,
            satelliteProduct
        ) ||
        !queryZhangInternalProduct(
            kfState.time,
            referenceIt->second.reference,
            observable,
            referenceProduct
        ))
    {
        return false;
    }

    return satelliteProduct.pppar_usable &&
           referenceProduct.pppar_usable;
}

void traceZhangPppArUserDiagnostics(
    Trace&       trace,
    ReceiverMap& receiverMap,
    KFState&     kfState
)
{
    if (!acsConfig.zhangPppAr.user_adapter ||
        !acsConfig.zhangPppAr.output_diagnostics)
    {
        return;
    }

    for (auto& [receiverId, receiver] : receiverMap)
    {
        vector<double> fractions;
        int ambiguityCount = 0;
        int integerValidCount = 0;
        int maxCounter = -1;
        int maxDatumVersion = -1;

        for (const auto& [key, index] : kfState.kfIndexMap)
        {
            if (key.type != KF::AMBIGUITY ||
                key.str != receiverId ||
                !zhangPppArUsesObservable(
                    key.Sat.sys,
                    static_cast<E_ObsCode>(key.num)
                ))
            {
                continue;
            }

            ambiguityCount++;
            double value = kfState.x(index);
            fractions.push_back(std::abs(value - std::round(value)));

            E_ObsCode code = static_cast<E_ObsCode>(key.num);
            ZhangInternalProduct product;
            if (queryZhangInternalProduct(
                    kfState.time,
                    key.Sat,
                    code,
                    product
                ))
            {
                maxCounter = std::max(
                    maxCounter,
                    product.discontinuity_counter
                );
                maxDatumVersion = std::max(
                    maxDatumVersion,
                    product.datum_version
                );
            }

            bool integerValid = zhangPppArUserAmbiguityIntegerValid(
                kfState,
                receiverId,
                key.Sat,
                code
            );
            integerValidCount += integerValid;

            UserReferenceKey referenceKey{
                &kfState,
                receiverId,
                key.Sat.sys,
                code
            };
            auto referenceIt = userReferenceMap.find(referenceKey);
            string reference =
                referenceIt != userReferenceMap.end() &&
                referenceIt->second.reference.prn > 0
                    ? referenceIt->second.reference.id()
                    : "NONE";

            trace << "\nZHANG_USER_AMBIGUITY time=" << kfState.time.to_string(0)
                  << " receiver=" << receiverId
                  << " satellite=" << key.Sat.id()
                  << " observable=" << enum_to_string(code)
                  << " reference=" << reference
                  << " value_cycles=" << value
                  << " rounded_cycles=" << std::llround(value)
                  << " fractional_cycle="
                  << std::abs(value - std::round(value))
                  << " integer_valid=" << integerValid
                  << " product_counter="
                  << (product.satellite.prn > 0
                          ? product.discontinuity_counter
                          : -1)
                  << " datum_version="
                  << (product.satellite.prn > 0
                          ? product.datum_version
                          : -1);
        }

        std::sort(fractions.begin(), fractions.end());
        double median = fractions.empty()
                            ? std::numeric_limits<double>::quiet_NaN()
                            : fractions[fractions.size() / 2];
        double p90 = fractions.empty()
                         ? std::numeric_limits<double>::quiet_NaN()
                         : fractions[static_cast<size_t>(
                               0.9 * (fractions.size() - 1)
                           )];

        Vector3d estimate = Vector3d::Zero();
        bool positionFound = true;
        for (int axis = 0; axis < 3; axis++)
        {
            KFKey positionKey;
            positionKey.type = KF::REC_POS;
            positionKey.str  = receiverId;
            positionKey.num  = axis;
            if (kfState.getKFValue(positionKey, estimate(axis)) == E_Source::NONE)
            {
                positionFound = false;
            }
        }

        Vector3d errorEnu = Vector3d::Constant(
            std::numeric_limits<double>::quiet_NaN()
        );
        if (positionFound && !receiver.aprioriPos.isZero())
        {
            Matrix3d rotation;
            pos2enu(ecef2pos(receiver.aprioriPos), rotation.data());
            errorEnu = rotation * (estimate - receiver.aprioriPos);
        }

        trace << "\nZHANG_USER_DIAGNOSTIC time=" << kfState.time.to_string(0)
              << " receiver=" << receiverId
              << " ambiguities=" << ambiguityCount
              << " integer_valid_ambiguities=" << integerValidCount
              << " product_counter=" << maxCounter
              << " datum_version=" << maxDatumVersion
              << " median_fractional_cycle=" << median
              << " p90_fractional_cycle=" << p90
              << " east_error_m=" << errorEnu(0)
              << " north_error_m=" << errorEnu(1)
              << " up_error_m=" << errorEnu(2);
    }
}
