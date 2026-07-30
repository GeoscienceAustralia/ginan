#pragma once

#include <iosfwd>
#include <string>
#include <vector>
#include "common/enums.h"
#include "common/gTime.hpp"
#include "common/satSys.hpp"

struct KFState;
struct ReceiverMap;
using Trace = std::ostream;

struct ZhangInternalProduct
{
    GTime     time;
    SatSys    satellite;
    E_ObsCode observable = E_ObsCode::NONE;
    std::string solution;

    double clock_m = 0;
    double clock_sigma_m = 0;
    double phase_m = 0;
    double phase_sigma_m = 0;
    double clock_phase_covariance_m2 = 0;
    double correction_m = 0;
    double correction_sigma_m = 0;

    int         discontinuity_counter = 0;
    long long   integer_shift_cycles = 0;
    double      fractional_shift_cycles = 0;
    int         datum_version = 0;
    GTime       valid_from;
    int         product_iod = 0;
    std::string reset_reason;
    bool        integer_valid = false;
    std::string integer_component_id = "UNRESOLVED";
    std::string integer_datum_id;
};

bool zhangPppArUsesObservable(E_Sys sys, E_ObsCode code);

/** Notify the continuity manager that an exact S-transform changed a raw
 * satellite phase correction.  The manager removes integer branch changes
 * from the emitted product and increments the discontinuity metadata for
 * fractional changes.
 */
void recordZhangExactPhaseTransform(
    GTime          time,
    E_Sys          sys,
    E_ObsCode      code,
    const SatSys&  satellite,
    double         correctionChangeMetres
);

/** Notify the continuity manager that the old phase coordinate system could
 * not be transformed exactly and was reinitialised.
 */
void recordZhangPhaseReinitialisation(
    GTime                         time,
    E_Sys                         sys,
    const std::vector<E_ObsCode>& observables,
    const std::string&            reason
);

/** Write both pre-fix and post-feedback internal products. */
void writeZhangInternalProducts(
    Trace&         trace,
    const KFState& floatState,
    const KFState& fixedState,
    int            newlyFixed,
    bool           integerDatumComplete
);

/** Read and interpolate an exact-epoch internal product for a held-out user. */
bool queryZhangInternalProduct(
    GTime                 time,
    const SatSys&         satellite,
    E_ObsCode             observable,
    ZhangInternalProduct& product
);

/** Select the held-out user's ambiguity reference per signal, perform exact
 * single-difference state/covariance transforms on reference changes, and
 * reset the independent user phase block when the product counter changes.
 */
void updateZhangPppArUserReferences(
    Trace&       trace,
    ReceiverMap& receiverMap,
    KFState&     kfState
);

bool zhangPppArUserReferenceAmbiguity(
    const KFState&     kfState,
    const std::string& receiver,
    const SatSys&      satellite,
    E_ObsCode          observable
);

bool zhangPppArUserAmbiguityIntegerValid(
    const KFState&     kfState,
    const std::string& receiver,
    const SatSys&      satellite,
    E_ObsCode          observable
);

/** Emit per-user ambiguity clustering and position-error diagnostics. */
void traceZhangPppArUserDiagnostics(
    Trace&       trace,
    ReceiverMap& receiverMap,
    KFState&     kfState
);
