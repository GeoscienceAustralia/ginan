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
#include "orbprop/coordinates.hpp"

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
           "product_iod,reset_reason,integer_valid,integer_component_id,"
           "integer_datum_id,solution_interval_start_gpst_seconds,"
           "solution_interval_end_gpst_seconds\n";

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
           << product.integer_valid << ","
           << product.integer_component_id << ","
           << product.integer_datum_id << ","
           << static_cast<double>(product.valid_from.bigTime) << ","
           << static_cast<double>(product.time.bigTime) << "\n";
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
    const string&  solution
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
        if (fields.size() != 19 && fields.size() != 23)
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
        product.integer_valid                 = std::stoi(fields[18]) != 0;
        if (fields.size() == 23)
        {
            product.integer_component_id      = fields[19];
            product.integer_datum_id          = fields[20];
            product.valid_from.bigTime        = std::stold(fields[21]);
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
    if (!acsConfig.zhangPppAr.output_products)
    {
        return;
    }

    const double lambda = wavelength(sys, code);
    if (lambda <= 0)
    {
        return;
    }

    ProductKey key{satellite, code};
    auto& state = continuityMap[key];
    initialiseContinuityState(key, state);

    state.applyExactTransform(
        time,
        correctionChangeMetres / lambda,
        acsConfig.zhangPppAr.stabilization_epochs
    );
}

void recordZhangPhaseReinitialisation(
    GTime                         time,
    E_Sys                         sys,
    const vector<E_ObsCode>&      observables,
    const string&                 reason
)
{
    if (!acsConfig.zhangPppAr.output_products)
    {
        return;
    }

    for (E_ObsCode code : observables)
    {
        auto& global = globalContinuityMap[{sys, code}];
        global.counter++;
        global.datumVersion++;
        global.iod++;
        global.validFrom = time;
        global.resetReason = reason;
        global.stabilizationRemaining = acsConfig.zhangPppAr.stabilization_epochs;

        for (auto& [key, state] : continuityMap)
        {
            if (key.satellite.sys != sys || key.observable != code)
            {
                continue;
            }
            state.reinitialise(
                time,
                reason,
                acsConfig.zhangPppAr.stabilization_epochs
            );
        }
    }
}

void writeZhangInternalProducts(
    Trace&         trace,
    const KFState& floatState,
    const KFState& fixedState,
    int            newlyFixed,
    bool           integerDatumComplete
)
{
    if (!acsConfig.zhangPppAr.output_products)
    {
        return;
    }

    auto writeSolution = [&](const KFState& state, const string& solution)
    {
        for (const auto& [phaseKey, phaseIndex] : state.kfIndexMap)
        {
            if (phaseKey.type != KF::PHASE_BIAS ||
                phaseKey.Sat.prn <= 0 ||
                !phaseKey.str.empty() ||
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
            if (solution == "FIXED")
            {
                if (integerDatumComplete)
                {
                    continuity.markFixed();
                }
                else
                {
                    continuity.invalidateIntegerDatum(
                        state.time,
                        "integer_datum_incomplete",
                        acsConfig.zhangPppAr.stabilization_epochs
                    );
                }
            }

            int clockIndex = clockIt->second;
            double clock = state.x(clockIndex);
            double rawPhase = state.x(phaseIndex);
            double emittedPhase =
                rawPhase + continuity.integerShiftCycles * lambda;
            double covariance = state.P(clockIndex, phaseIndex);
            double correctionVariance =
                state.P(clockIndex, clockIndex) +
                state.P(phaseIndex, phaseIndex) -
                2 * covariance;

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
            product.correction_m = clock - emittedPhase;
            product.correction_sigma_m =
                std::sqrt(std::max(0.0, correctionVariance));
            product.discontinuity_counter = continuity.counter;
            product.integer_shift_cycles = continuity.integerShiftCycles;
            product.fractional_shift_cycles = continuity.fractionalShiftCycles;
            product.datum_version = continuity.datumVersion;
            product.valid_from = continuity.validFrom;
            product.product_iod = continuity.iod;
            product.reset_reason = continuity.resetReason;
            product.integer_valid =
                solution == "FIXED" &&
                continuity.integerValid();
            product.integer_component_id =
                product.integer_valid
                    ? enum_to_string(phaseKey.Sat.sys) + "-" +
                          enum_to_string(code) + "-MAIN"
                    : "UNRESOLVED";
            product.integer_datum_id =
                enum_to_string(phaseKey.Sat.sys) + "-" +
                enum_to_string(code) + "-V" +
                std::to_string(continuity.datumVersion) + "-IOD" +
                std::to_string(continuity.iod);

            appendProduct(product);

            if (acsConfig.zhangPppAr.output_diagnostics)
            {
                trace << "\nZHANG_CONTINUITY_PRODUCT time=" << state.time.to_string(0)
                      << " solution=" << solution
                      << " satellite=" << phaseKey.Sat.id()
                      << " observable=" << enum_to_string(code)
                      << " counter=" << continuity.counter
                      << " integer_shift_cycles=" << continuity.integerShiftCycles
                      << " fractional_shift_cycles=" << continuity.fractionalShiftCycles
                      << " datum_version=" << continuity.datumVersion
                      << " iod=" << continuity.iod
                      << " newly_fixed=" << newlyFixed
                      << " integer_datum_complete=" << integerDatumComplete
                      << " integer_valid=" << product.integer_valid
                      << " reason=" << continuity.resetReason;
            }
        }
    };

    writeSolution(floatState, "FLOAT");
    writeSolution(fixedState, "FIXED");
    appendProductCovariance(floatState, "FLOAT");
    appendProductCovariance(fixedState, "FIXED");
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
    return true;
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

    return satelliteProduct.integer_valid &&
           referenceProduct.integer_valid;
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
