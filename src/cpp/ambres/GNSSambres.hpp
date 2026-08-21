#pragma once

#include <cstddef>
#include <cstdint>
#include <cmath>
#include <string>

#include "common/algebra.hpp"
#include "common/common.hpp"
#include "common/constants.hpp"
#include "common/eigenIncluder.hpp"
#include "common/observations.hpp"
#include "common/receiver.hpp"
#include "common/satSys.hpp"
#include "common/trace.hpp"

extern double FIXED_AMB_VAR;
extern bool   AR_VERBO;

struct KFState;
struct ZhangCheckpointSnapshotReferenceSummary;

/** Bootstrapped success probability for the suffix actually retained by
 * LAMBDA/PAR.  Keeping this calculation independent of the initial proposal
 * rank prevents a shrunken NIS-accepted subset from inheriting stale risk. */
inline double lambdaSelectedSuffixBootstrapSuccess(
    const VectorXd& conditionalVariances,
    int selectedCount)
{
    if (selectedCount <= 0 || selectedCount > conditionalVariances.size())
        return 0;
    double success = 1;
    for (int index = conditionalVariances.size() - selectedCount;
         index < conditionalVariances.size(); index++)
    {
        if (!(conditionalVariances(index) > 0)) return 0;
        success *= std::erf(std::sqrt(1 / (8 * conditionalVariances(index))));
    }
    return success;
}

inline constexpr std::uint32_t ZHANG_AMBRES_CHECKPOINT_SCHEMA_VERSION = 2;
inline constexpr const char* ZHANG_AMBRES_CHECKPOINT_SECTION_NAME =
    "zhang_ambres_runtime";

struct ZhangAmbresCheckpointResult
{
    bool        valid = false;
    std::string failureReason;
    std::size_t e27Runtimes = 0;
    std::size_t e27Factors = 0;
    std::size_t heldLattices = 0;
    std::size_t heldRows = 0;
    std::size_t heldEvidenceRows = 0;
    std::size_t activeTemporalTransitions = 0;
    std::size_t relinkPriorMoments = 0;
    std::size_t relinkScalarIncrements = 0;
    std::size_t relinkJointIncrements = 0;
    std::size_t whitenedWlObservations = 0;
    std::size_t productRelationAdmissionStates = 0;
    std::size_t productRelationPendingCandidates = 0;
    std::size_t temporalCertificateConfirmations = 0;
    std::size_t heldUserWideLaneStates = 0;
    std::size_t heldUserWideLaneIntegers = 0;
};

/** Pointer-free, versioned checkpoint for the cross-epoch runtime owned by
 * ppp_ambres.cpp.  The caller must invoke this only at the post-epoch E29
 * barrier.  validateOnly performs complete decode/reconstruction validation
 * without mutating live state; commit replaces the requested runtime family
 * only after every section object has passed validation.  A bundle containing
 * temporal transitions must additionally pass the two snapshot-reference
 * inspection calls and validateZhangCheckpointSnapshotReferences() before
 * this section is committed. */
ZhangAmbresCheckpointResult exportZhangAmbresCheckpointSection(
    const KFState&     owner,
    const std::string& runtimeId,
    std::string&       payload);

ZhangAmbresCheckpointResult importZhangAmbresCheckpointSection(
    KFState&           owner,
    const std::string& runtimeId,
    const std::string& payload,
    bool               validateOnly = false);

/** Fully decode and validate the AMBRES payload and report its active
 * transition snapshot references.  The returned section-local counts must be
 * merged with the PPP-AR pending counts by
 * validateZhangCheckpointSnapshotReferences() before bundle commit. */
ZhangAmbresCheckpointResult inspectZhangAmbresCheckpointSnapshotReferences(
    const std::string& runtimeId,
    const std::string& payload,
    ZhangCheckpointSnapshotReferenceSummary& summary);

double round_perr(double dx, double var);

struct GinAR_mtx
{
    map<int, KFKey> ambmap;
    VectorXd        aflt;
    MatrixXd        Paflt;

    MatrixXd Ztrs;
    MatrixXd Ltrs;
    VectorXd Dtrs;

    VectorXd zflt;
    VectorXd zfix;

    VectorXd afix;
    MatrixXd Pafix;

    int    lambda_initial_fix_count          = 0;
    /** Bootstrapped success probability of the suffix actually returned by
     * lambda_search(), after any NIS-driven rank reduction. */
    double lambda_selected_bootstrap_success = 0;
    double lambda_candidate_nis              = 0;
    double lambda_candidate_nis_threshold    = 0;
    bool   lambda_candidate_nis_valid        = false;
    int    lambda_candidate_tested_fix_count = 0;
    double lambda_candidate_rms_innovation   = 0;
    double lambda_candidate_max_innovation   = 0;
    double lambda_candidate_min_sigma        = 0;
    double lambda_candidate_max_sigma        = 0;
    double lambda_candidate_max_marginal_nis = 0;
    /** Exact reliable suffix actually tested by the absolute NIS gate.  These
     * rows and integers are retained even when the candidate is rejected so
     * a frozen-posterior causal replay can evaluate the identical physical
     * integer functionals instead of allowing every ablation to select a
     * different LAMBDA basis. */
    MatrixXd lambda_candidate_tested_rows;
    VectorXd lambda_candidate_tested_integers;
    int    lambda_dominant_whitened_mode      = -1;
    int    lambda_whitened_effective_rank     = 0;
    double lambda_dominant_whitened_residual  = 0;
    double lambda_dominant_whitened_nis       = 0;
    double lambda_dominant_whitened_share     = 0;
    double lambda_second_whitened_share       = 0;
    double lambda_whitened_condition_number   = 0;
    double lambda_whitened_nis_closure        = 0;
    VectorXd lambda_dominant_original_loading;
    int    lambda_ablation_input_rows          = 0;
    int    lambda_ablation_support_rows        = 0;
    int    lambda_ablation_removed_rows        = 0;
    int    lambda_ablation_retained_rows       = 0;
    double lambda_ablation_target_mean_sigma   = 0;
    double lambda_ablation_removed_mean_sigma  = 0;
    double lambda_ablation_max_log_var_mismatch = 0;
    string lambda_ablation_status;
};

struct GinAR_opt
{
    string           recv;
    map<E_Sys, bool> sys_solve;

    bool     endu = false;
    E_ARmode mode = E_ARmode::OFF;  /* AR mode */

    int nset = 0;                   /* candidate set size for lambda */
    int nitr = 3;                   /* number of iterations for iter_rnd */

    /** Optional cap on the nested LAMBDA PAR dimension.  Zero leaves the
     * bootstrap-success selection unchanged.  A positive value makes LAMBDA
     * re-solve the corresponding reliable suffix rather than truncating a
     * candidate obtained in a larger integer search space.
     */
    int max_lambda_fix_count = 0;

    /** Minimum reliable suffix dimension accepted by LAMBDA/PAR.  The legacy
     * network ambiguity path keeps three as its default.  Direct named
     * ProductRelation branches may set this to one because their original
     * integer coordinates are subsequently subjected to exact named-row,
     * scalar-risk and joint-NIS gates. */
    int min_lambda_fix_count = 3;

    /** Optional absolute candidate-consistency gate applied inside the nested
     * LAMBDA PAR search.  Zero disables it.  When enabled, the decorrelation
     * is performed once and successively smaller reliable suffixes are solved
     * until the ILS distance passes the requested chi-square upper-tail gate.
     */
    double lambda_candidate_nis_alpha = 0;

    /** Optional shadow-only removal of exact integer rows after the first
     * LAMBDA decorrelation.  Target columns refer to the physical ambiguity
     * coordinates on input; retained rows are re-solved as an integer
     * sublattice without changing the float filter or WL stage.
     */
    string      lambda_candidate_row_ablation = "NONE";
    vector<int> lambda_candidate_ablation_target_columns;
    std::uint64_t lambda_candidate_ablation_seed = 0;

    double MIN_Elev_prc = D2R * 10; /* min elevation for processing */
    double MIN_Elev_AR  = D2R * 15; /* min elevation for AR */
    double MIN_Elev_piv = D2R * 20; /* min elevation for pivot */

    double sucthr = 0.9999;         /* success rate threshold */
    double ratthr = 3;              /* ratio test threshold */

    bool   clear_old_amb = false;
    int    Max_Hold_epc  = 0;   /* max hold (epoch) */
    double Max_Hold_tim  = 600; /* max hold (seconds) */
};

struct GinAR_lambda_beam_options
{
    int core_max_dimension = 80;
    int reserve_dimension  = 40;
    int branch_factor      = 4;
    int beam_width         = 12;
    int maximum_depth      = 10;
    int minimum_rank       = 3;
    double fixed_failure_rate = 0.001;
    bool prefer_product_gain = false;
    string context;
};

struct GinAR_lambda_beam_result
{
    bool     nis_compatible_found = false;
    int      initial_core_rank    = 0;
    int      initial_pool_rank    = 0;
    int      explored_nodes       = 0;
    int      unique_nodes         = 0;
    int      duplicate_nodes      = 0;
    int      nis_compatible_nodes = 0;
    int      selected_rank        = 0;
    int      selected_depth       = 0;
    double   selected_bootstrap_success = 0;
    double   selected_bootstrap_log_failure = 0;
    double   selected_nis               = 0;
    double   selected_nis_threshold     = 0;
    double   selected_candidate_ratio   = 0;
    bool     selected_ffrt_pass          = false;
    double   selected_product_gain      = 0;
    bool     product_gain_available     = false;
    string   selected_hnf_fingerprint;
    MatrixXd selected_integer_rows;
    VectorXd selected_integer_values;
};

struct GinAR_lambda_subset_oracle_options
{
    int pool_dimension = 10;
    int minimum_rank = 3;
    int maximum_rank = 5;
    int maximum_subsets = 5000;
    double fixed_failure_rate = 0.001;
    string context;
};

struct GinAR_lambda_subset_oracle_result
{
    bool     feasible_found = false;
    int      dictionary_rank = 0;
    int      enumerated_subsets = 0;
    int      unique_sublattices = 0;
    int      feasible_sublattices = 0;
    int      selected_rank = 0;
    double   selected_product_gain = 0;
    double   selected_nis = 0;
    double   selected_nis_threshold = 0;
    string   selected_hnf_fingerprint;
    MatrixXd selected_integer_rows;
    VectorXd selected_integer_values;
};

int GNSS_AR(Trace& trace, GinAR_mtx& mtrx, GinAR_opt opt);

/** Expose the unimodular LAMBDA decorrelation rows for diagnostics and
 * bounded shadow candidate dictionaries.  Calling this function does not
 * perform fixing or estimator feedback. */
int Ztrans_reduction(Trace& trace, GinAR_mtx& mtrx);

GinAR_lambda_beam_result GNSS_AR_LAMBDA_BEAM_SHADOW(
    Trace&                         trace,
    const GinAR_mtx&               source,
    const GinAR_opt&               options,
    const GinAR_lambda_beam_options& beamOptions,
    const MatrixXd&                productCrossCovariance = MatrixXd(),
    double                         productVarianceTrace = 0
);

GinAR_lambda_subset_oracle_result
GNSS_AR_LAMBDA_SUBSET_ORACLE_SHADOW(
    Trace&                                  trace,
    const GinAR_mtx&                        source,
    const GinAR_opt&                        options,
    const GinAR_lambda_subset_oracle_options& oracleOptions,
    const MatrixXd&                         productCrossCovariance,
    double                                  productVarianceTrace
);
