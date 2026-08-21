#include "ambres/GNSSambres.hpp"
#include <algorithm>
#include <limits>
#include <math.h>
#include <random>
#include <set>
#include <boost/math/distributions/chi_squared.hpp>
#include "common/zhangLambdaBeam.hpp"
#include "rtklib/lambda.h"

#define LOG_PI 1.14472988584940017
#define SQRT2 1.41421356237309510
#define AMB_RANG 10

bool   AR_VERBO      = false;
double FIXED_AMB_VAR = 1e-8;
/** Probability of error (assuming normal distribution) */
double round_perr(
    double dx,  ///< Distance between value and mean
    double var  ///< Variance
)
{
    if (var < 1e-20)
        return 0;

    double p0   = 0;
    double fact = -0.25 / var;

    for (int i = 1; i < AMB_RANG; i++)
    {
        p0 += exp((i + 2 * dx) * i * fact);
        p0 += exp((i - 2 * dx) * i * fact);
    }

    return p0 / (p0 + 1);
}

/** Simple integer Rounding */
int simple_round(
    Trace&     trace,  ///< Debug trace
    GinAR_mtx& mtrx,   ///< Reference to structure containing float values and covariance
    GinAR_opt  opt     ///< Object containing processing options
)
{
    MatrixXd P    = mtrx.Paflt;
    VectorXd ret  = mtrx.aflt;
    int      namb = ret.size(), nfix = 0;

    mtrx.Ztrs.resize(0, 0);
    mtrx.zfix.resize(0);

    if (namb <= 0)
        return 0;

    double ratthr = 1 / (opt.ratthr + 1);
    double sucthr = 1 - pow(opt.sucthr, 1.0 / namb);
    tracepdeex(4, trace, "\n#ARES_RND Using integer rounding ... %.4e  %.4f", sucthr, ratthr);

    vector<int> zind;
    vector<int> xind;
    xind.reserve(namb);

    for (int i = 0; i < namb; i++)
    {
        xind.push_back(i);
        double dv   = ret(i) - ROUND(ret(i));
        double perr = round_perr(dv, P(i, i));

        if (fabs(dv) < ratthr && perr < sucthr)
        {
            ret(i) = ret(i) - dv;
            nfix++;
            zind.push_back(i);
        }
    }

    MatrixXd Z = MatrixXd::Identity(namb, namb);

    if (nfix == 0)
        return 0;

    mtrx.Ztrs = Z(zind, xind);
    mtrx.zfix = ret(zind);

    return nfix;
}

/** Iterative Rounding */
int interat_round(
    Trace&     trace,  ///< Debug trace
    GinAR_mtx& mtrx,   ///< Reference to structure containing float values and covariance
    GinAR_opt& opt     ///< Object containing processing options
)
{
    MatrixXd P    = mtrx.Paflt;
    VectorXd x    = mtrx.aflt;
    int      namb = x.size();

    mtrx.Ztrs.resize(0, 0);
    mtrx.zfix.resize(0);

    if (namb <= 0)
        return 0;

    double sucthr = 1 - pow(opt.sucthr, 1.0 / namb);
    double ratthr = 1 / (opt.ratthr + 1);

    vector<int> zind;
    vector<int> xind;
    xind.reserve(namb);

    for (int i = 0; i < namb; i++)
        xind.push_back(i);

    MatrixXd I = MatrixXd::Identity(namb, namb);
    MatrixXd Ztrs;
    VectorXd xfix;
    VectorXd dvvct = VectorXd::Zero(namb);

    int nfix = 0;
    int nnew = 0;

    for (int iter = 0; iter < opt.nitr; iter++)
    {
        zind.clear();
        nnew = 0;

        for (int i = 0; i < namb; i++)
        {
            double dv   = x(i) - ROUND(x(i));
            double perr = round_perr(dv, P(i, i));

            if ((fabs(dv) < ratthr) && (perr < sucthr))
            {
                nnew++;
                zind.push_back(i);
                dvvct(i) = dv;
            }
        }

        if (nnew <= nfix)
            break;

        VectorXd dx = dvvct(zind);

        Ztrs = I(zind, xind);
        xfix = x(zind) - dx;

        MatrixXd Psel = P(zind, zind) + (FIXED_AMB_VAR * I(zind, zind));
        MatrixXd K    = P(xind, zind) * Psel.inverse();
        MatrixXd S    = K * P(zind, xind);
        x             = x - K * dx;
        P             = P - S;
        nfix          = nnew;
    }

    if (nfix == 0)
        return 0;

    mtrx.Ztrs = Ztrs;
    mtrx.zfix = xfix;

    return nfix;
}

bool LTDL_factorization(
    GinAR_mtx& mtrx  ///< Reference to structure containing float values and covariance
)
{
    int      n = mtrx.aflt.size();
    MatrixXd P = mtrx.Paflt;

    MatrixXd L = MatrixXd::Zero(n, n);
    VectorXd D = VectorXd::Zero(n);

    for (int i = n - 1; i >= 0; i--)
    {
        if (P(i, i) <= 0)
            return false;

        D(i)                    = P(i, i);
        double a                = sqrt(P(i, i));
        L.block(i, 0, 1, i + 1) = P.block(i, 0, 1, i + 1) / a;
        for (int j = 0; j < i; j++)
            P.block(j, 0, 1, j + 1) -= L(i, j) * L.block(i, 0, 1, j + 1);
        L.block(i, 0, 1, i + 1) /= L(i, i);
    }

    mtrx.Ltrs = L;
    mtrx.Dtrs = D;
    return true;
}

/** Lambda decorrelation (trough Z transform) */
int Ztrans_reduction(
    Trace&     trace,  ///< Debug trace
    GinAR_mtx& mtrx    ///< Reference to structure containing float values and covariance
)
{
    int n = mtrx.aflt.size();
    if (n < 1)
        return -1;

    if (LTDL_factorization(mtrx) == false)
    {
        tracepdeex(
            1,
            trace,
            "WARNING: LD decomposition error, ambiguity matrix may not positive definite\n"
        );
        return -1;
    }

    VectorXd x = mtrx.aflt;
    VectorXd D = mtrx.Dtrs;
    MatrixXd L = mtrx.Ltrs;
    MatrixXd Z = MatrixXd::Identity(n, n);

    if (AR_VERBO)
    {
        trace << std::setprecision(8);
        trace << "\n"
              << "x =" << "\n"
              << x.transpose() << "\n";
        trace << "\n"
              << "Px=" << "\n"
              << mtrx.Paflt << "\n";
        trace << "\n"
              << "Lx=" << "\n"
              << L << "\n";
        trace << "\n"
              << "Dx=" << "\n"
              << D.transpose() << "\n";
    }

    int k = n - 2;
    int j = n - 2;
    while (j >= 0)
    {
        if (j <= k)
            for (int i = j + 1; i < n; i++)
            {
                double mu = ROUND(L(i, j));
                if (mu != 0)
                {
                    L.col(j) -= mu * L.col(i);
                    Z.col(j) -= mu * Z.col(i);
                }
            }

        double del = D(j) + L(j + 1, j) * L(j + 1, j) * D(j + 1);
        if ((del + 1E-6) < D(j + 1))
        {
            double eta = D(j) / del;
            double lam = D(j + 1) * L(j + 1, j) / del;

            D(j)     = eta * D(j + 1);
            D(j + 1) = del;

            MatrixXd a0             = L.block(j, 0, 1, j);
            MatrixXd a1             = L.block(j + 1, 0, 1, j);
            L.block(j, 0, 1, j)     = a1 - L(j + 1, j) * a0;
            L.block(j + 1, 0, 1, j) = lam * a1 + eta * a0;
            L(j + 1, j)             = lam;

            VectorXd Ltmp                       = L.block(j + 2, j, n - j - 2, 1);
            L.block(j + 2, j, n - j - 2, 1)     = L.block(j + 2, j + 1, n - j - 2, 1);
            L.block(j + 2, j + 1, n - j - 2, 1) = Ltmp;

            VectorXd Ztmp = Z.col(j);
            Z.col(j)      = Z.col(j + 1);
            Z.col(j + 1)  = Ztmp;

            k = j;
            j = n - 2;
        }
        else
            j--;
    }

    mtrx.Ztrs = Z.transpose();
    mtrx.zflt = mtrx.Ztrs * x;
    mtrx.Ltrs = L;
    mtrx.Dtrs = D;

    if (AR_VERBO)
    {
        trace << std::setprecision(8);
        trace << "\n"
              << "z =" << "\n"
              << mtrx.zflt.transpose() << "\n";
        trace << "\n"
              << "Zt=" << "\n"
              << mtrx.Ztrs << "\n";
        trace << "\n"
              << "Lz=" << "\n"
              << L << "\n";
        trace << "\n"
              << "Dz=" << "\n"
              << D.transpose() << "\n";
    }

    return n;
}

/** Integer bootstrapping */
// int integer_bootst(
// 	Trace& trace,		///< Debug trace
// 	GinAR_mtx& mtrx,	///< Reference to structure containing float values and covariance
// 	GinAR_opt opt)		///< Object containing processing options
// {
// 	int info = Ztrans_reduction(trace, mtrx);

// 	if (info < 0)
// 		return 0;

// 	GinAR_mtx mtrx2;
// 	mtrx2.aflt = mtrx.zflt;

// 	MatrixXd Z   = mtrx.Ztrs;
// 	mtrx2.Paflt	 = Z*mtrx.Paflt*Z.transpose();

// 	int nfix = interat_round (trace, mtrx2, opt);

// 	mtrx.Ztrs = mtrx2.Ztrs * Z;
// 	mtrx.zfix = mtrx2.zfix;

// 	return nfix;
// }

int integer_bootst(
    Trace&     trace,  ///< Debug trace
    GinAR_mtx& mtrx,   ///< Reference to structure containing float values and covariance
    GinAR_opt& opt     ///< Object containing processing options
)
{
    LDLT<MatrixXd> ldlt_;
    ldlt_.compute(mtrx.Paflt);

    if (ldlt_.isPositive() == false)
    {
        tracepdeex(
            1,
            trace,
            "WARNING: LD decomposition error, ambiguity matrix may not positive definite\n"
        );
        return 0;
    }

    MatrixXd L_ = ldlt_.matrixL();
    auto     tr = ldlt_.transpositionsP();

    int      siz = mtrx.aflt.size();
    MatrixXd I0  = MatrixXd::Identity(siz, siz);
    MatrixXd Zt  = tr * I0;
    VectorXd z_  = tr * mtrx.aflt;

    for (int j = siz - 2; j >= 0; j--)
    {
        for (int i = j + 1; i < siz; i++)
        {
            double mu = ROUND(L_(i, j));

            if (mu != 0)
            {
                L_.row(i) -= mu * L_.row(j);
                Zt.row(i) -= mu * Zt.row(j);
                z_(i) -= mu * z_(j);
            }
        }
    }

    MatrixXd Pz = Zt * mtrx.Paflt * Zt.transpose();

    GinAR_mtx mtrx2;
    mtrx2.aflt  = z_;
    mtrx2.Paflt = Pz;

    if (AR_VERBO)
    {
        trace << "\n"
              << "x_=" << "\n"
              << mtrx.aflt.transpose() << "\n";
        trace << "\n"
              << "Px=" << "\n"
              << mtrx.Paflt << "\n";
        trace << "\n"
              << "Zt=" << "\n"
              << Zt << "\n";
        trace << "\n"
              << "z_=" << "\n"
              << z_.transpose() << "\n";
        trace << "\n"
              << "Pz=" << "\n"
              << Pz << "\n";
    }

    int nfix = interat_round(trace, mtrx2, opt);

    mtrx.Ztrs = mtrx2.Ztrs * Zt;
    mtrx.zfix = mtrx2.zfix;

    return nfix;
}

static map<double, VectorXd> lambdaSearchReducedSuffix(
    const GinAR_mtx& mtrx,
    int               zsiz,
    const GinAR_opt&  opt
)
{
    int nmax = mtrx.Dtrs.size();
    int kmax = nmax - 1;
    int kmin = kmax - zsiz + 1;
    map<double, VectorXd> zfixList;
    VectorXd dist = VectorXd::Zero(nmax);
    VectorXd zadj = VectorXd::Zero(nmax);
    VectorXd zfix = VectorXd::Zero(nmax);
    VectorXd zdif = VectorXd::Zero(nmax);
    VectorXd step = VectorXd::Zero(nmax);

    int k          = kmax;
    zadj(k)        = mtrx.zflt(k);
    zfix(k)        = ROUND(zadj(k));
    zdif(k)        = zadj(k) - zfix(k);
    step(k)        = zdif(k) < 0 ? -1 : 1;
    bool   search  = true;
    double maxdist = 1e99;
    int    ncand   = 0;

    while (search)
    {
        double newdist = dist(k) + zdif(k) * zdif(k) / mtrx.Dtrs(k);

        if (newdist < maxdist)
        {
            if (k != kmin)
            {
                k--;
                dist(k) = newdist;

                zadj(k) = mtrx.zflt(k);
                for (int j = k + 1; j < nmax; j++)
                    zadj(k) -= zdif(j) * mtrx.Ltrs(j, k);

                zfix(k) = ROUND(zadj(k));
                zdif(k) = zadj(k) - zfix(k);
                step(k) = zdif(k) < 0 ? -1 : 1;
            }
            else
            {
                VectorXd zcut     = zfix.tail(zsiz);
                zfixList[newdist] = zcut;
                ncand             = zfixList.size();
                double maxd       = newdist * opt.ratthr;

                if (ncand > 1 && maxd < maxdist)
                    maxdist = maxd;

                if (ncand > opt.nset)
                    break;

                if (opt.nset > 0 && (ncand >= opt.nset))
                {
                    int ntot = 0;
                    for (auto it = zfixList.begin(); it != zfixList.end();)
                    {
                        if (ntot++ >= opt.nset)
                            it = zfixList.erase(it);
                        else
                        {
                            maxd = it->first;
                            ++it;
                        }
                    }

                    if (maxd < maxdist)
                        maxdist = maxd;

                    ncand = zfixList.size();
                }

                zfix(kmin) += step(kmin);
                zdif(kmin) = zadj(kmin) - zfix(kmin);
                step(kmin) = -step(kmin) + (step(kmin) < 0 ? 1 : -1);
            }
        }
        else
        {
            if (k == kmax)
                break;
            else
            {
                k++;
                zfix(k) += step(k);
                zdif(k) = zadj(k) - zfix(k);
                step(k) = -step(k) + (step(k) < 0 ? 1 : -1);
            }
        }
    }

    return zfixList;
}

/** Lambda algorithm and its variations (ILQ, Common set, BIE) */
int lambda_search(
    Trace&     trace,  ///< Debug trace
    GinAR_mtx& mtrx,   ///< Reference to structure containing float values and covariance
    GinAR_opt  opt     ///< Object containing processing options
)
{
	const int minimumFixCount = std::max(1, opt.min_lambda_fix_count);
    mtrx.lambda_initial_fix_count       = 0;
    mtrx.lambda_selected_bootstrap_success = 0;
    mtrx.lambda_candidate_nis           = 0;
    mtrx.lambda_candidate_nis_threshold = 0;
    mtrx.lambda_candidate_nis_valid     = false;
    mtrx.lambda_candidate_tested_fix_count = 0;
    mtrx.lambda_candidate_rms_innovation   = 0;
    mtrx.lambda_candidate_max_innovation   = 0;
    mtrx.lambda_candidate_min_sigma        = 0;
    mtrx.lambda_candidate_max_sigma        = 0;
    mtrx.lambda_candidate_max_marginal_nis = 0;
    mtrx.lambda_candidate_tested_rows.resize(0, mtrx.aflt.size());
    mtrx.lambda_candidate_tested_integers.resize(0);
    mtrx.lambda_dominant_whitened_mode      = -1;
    mtrx.lambda_whitened_effective_rank     = 0;
    mtrx.lambda_dominant_whitened_residual  = 0;
    mtrx.lambda_dominant_whitened_nis       = 0;
    mtrx.lambda_dominant_whitened_share     = 0;
    mtrx.lambda_second_whitened_share       = 0;
    mtrx.lambda_whitened_condition_number   = 0;
    mtrx.lambda_whitened_nis_closure        = 0;
    mtrx.lambda_dominant_original_loading.resize(0);
    mtrx.lambda_ablation_input_rows          = 0;
    mtrx.lambda_ablation_support_rows        = 0;
    mtrx.lambda_ablation_removed_rows        = 0;
    mtrx.lambda_ablation_retained_rows       = 0;
    mtrx.lambda_ablation_target_mean_sigma   = 0;
    mtrx.lambda_ablation_removed_mean_sigma  = 0;
    mtrx.lambda_ablation_max_log_var_mismatch = 0;
    mtrx.lambda_ablation_status.clear();

    int info = Ztrans_reduction(trace, mtrx);
    if (info < 0)
    {
        tracepdeex(2, trace, "\n Matrix decorrelation failed ... ");
        return 0;
    }

    mtrx.lambda_ablation_input_rows = mtrx.Ztrs.rows();
    mtrx.lambda_ablation_retained_rows = mtrx.Ztrs.rows();
    mtrx.lambda_ablation_status = "BASELINE";

    if (opt.lambda_candidate_row_ablation != "NONE")
    {
        const MatrixXd baseRows = mtrx.Ztrs;
        const VectorXd baseConditionalVariance = mtrx.Dtrs;
        const int inputRows = baseRows.rows();
        const set<int> targetColumns(
            opt.lambda_candidate_ablation_target_columns.begin(),
            opt.lambda_candidate_ablation_target_columns.end()
        );
        vector<int> supportRows;
        vector<int> controlRows;
        for (int row = 0; row < inputRows; row++)
        {
            bool hasTargetSupport = false;
            for (int column : targetColumns)
            {
                if (column >= 0 && column < baseRows.cols() &&
                    std::abs(baseRows(row, column)) > 0.5)
                {
                    hasTargetSupport = true;
                    break;
                }
            }
            (hasTargetSupport ? supportRows : controlRows).push_back(row);
        }

        vector<int> removedRows;
        double maximumLogVarianceMismatch = 0;
        string status = "APPLIED";
        auto conditionalVariance = [&](int row)
        {
            return std::max(
                baseConditionalVariance(row),
                std::numeric_limits<double>::min()
            );
        };
        if (opt.lambda_candidate_row_ablation == "PHYSICAL_SUPPORT")
        {
            removedRows = supportRows;
        }
        else if (opt.lambda_candidate_row_ablation == "MATCHED_RANDOM")
        {
            if (controlRows.size() < supportRows.size())
            {
                status = "INSUFFICIENT_CONTROL_ROWS";
            }
            else
            {
                vector<int> targets = supportRows;
                std::mt19937_64 generator(
                    opt.lambda_candidate_ablation_seed
                );
                std::shuffle(targets.begin(), targets.end(), generator);
                for (int targetRow : targets)
                {
                    const double targetLogVariance =
                        std::log(conditionalVariance(targetRow));
                    std::sort(
                        controlRows.begin(),
                        controlRows.end(),
                        [&](int left, int right)
                        {
                            const double leftDistance = std::abs(
                                std::log(conditionalVariance(left)) -
                                targetLogVariance
                            );
                            const double rightDistance = std::abs(
                                std::log(conditionalVariance(right)) -
                                targetLogVariance
                            );
                            if (leftDistance != rightDistance)
                            {
                                return leftDistance < rightDistance;
                            }
                            return left < right;
                        }
                    );
                    const int matchedWindow = std::min(
                        4,
                        static_cast<int>(controlRows.size())
                    );
                    std::uniform_int_distribution<int> select(
                        0,
                        matchedWindow - 1
                    );
                    const int selectedIndex = select(generator);
                    const int selectedRow = controlRows[selectedIndex];
                    removedRows.push_back(selectedRow);
                    maximumLogVarianceMismatch = std::max(
                        maximumLogVarianceMismatch,
                        std::abs(
                            std::log(conditionalVariance(selectedRow)) -
                            targetLogVariance
                        )
                    );
                    controlRows.erase(controlRows.begin() + selectedIndex);
                }
            }
        }

        set<int> removedSet(removedRows.begin(), removedRows.end());
        vector<int> retainedRows;
        for (int row = 0; row < inputRows; row++)
        {
            if (removedSet.count(row) == 0)
            {
                retainedRows.push_back(row);
            }
        }
        if (status != "APPLIED" ||
			static_cast<int>(retainedRows.size()) < minimumFixCount)
        {
            status = status == "APPLIED"
                ? "INSUFFICIENT_RETAINED_ROWS"
                : status;
            retainedRows.clear();
        }

        auto meanSigma = [&](const vector<int>& rows)
        {
            double sum = 0;
            for (int row : rows)
            {
                sum += std::sqrt(conditionalVariance(row));
            }
            return rows.empty() ? 0 : sum / rows.size();
        };
        const double targetMeanSigma = meanSigma(supportRows);
        const double removedMeanSigma = meanSigma(removedRows);

        trace << "\nZHANG_LAMBDA_ROW_ABLATION"
              << " mode=" << opt.lambda_candidate_row_ablation
              << " input_rows=" << inputRows
              << " support_rows=" << supportRows.size()
              << " removed_rows=" << removedRows.size()
              << " retained_rows=" << retainedRows.size()
              << " target_mean_conditional_sigma_cycles="
              << targetMeanSigma
              << " removed_mean_conditional_sigma_cycles="
              << removedMeanSigma
              << " maximum_log_variance_mismatch="
              << maximumLogVarianceMismatch
              << " status=" << status;

        if (retainedRows.empty())
        {
            mtrx.Ztrs.resize(0, baseRows.cols());
            mtrx.zfix.resize(0);
            mtrx.lambda_ablation_input_rows = inputRows;
            mtrx.lambda_ablation_support_rows = supportRows.size();
            mtrx.lambda_ablation_removed_rows = removedRows.size();
            mtrx.lambda_ablation_retained_rows = 0;
            mtrx.lambda_ablation_target_mean_sigma = targetMeanSigma;
            mtrx.lambda_ablation_removed_mean_sigma = removedMeanSigma;
            mtrx.lambda_ablation_max_log_var_mismatch =
                maximumLogVarianceMismatch;
            mtrx.lambda_ablation_status = status;
            return 0;
        }

        MatrixXd retainedIntegerRows = baseRows(retainedRows, Eigen::all);
        GinAR_mtx ablated;
        ablated.aflt = retainedIntegerRows * mtrx.aflt;
        ablated.Paflt = retainedIntegerRows * mtrx.Paflt *
            retainedIntegerRows.transpose();
        GinAR_opt ablatedOptions = opt;
        ablatedOptions.lambda_candidate_row_ablation = "NONE";
        ablatedOptions.lambda_candidate_ablation_target_columns.clear();
        const int fixed = lambda_search(trace, ablated, ablatedOptions);

        if (ablated.Ztrs.rows() > 0)
        {
            mtrx.Ztrs = ablated.Ztrs * retainedIntegerRows;
        }
        else
        {
            mtrx.Ztrs.resize(0, baseRows.cols());
        }
        mtrx.zfix = ablated.zfix;
        mtrx.lambda_initial_fix_count = ablated.lambda_initial_fix_count;
        mtrx.lambda_selected_bootstrap_success =
            ablated.lambda_selected_bootstrap_success;
        mtrx.lambda_candidate_nis = ablated.lambda_candidate_nis;
        mtrx.lambda_candidate_nis_threshold =
            ablated.lambda_candidate_nis_threshold;
        mtrx.lambda_candidate_nis_valid =
            ablated.lambda_candidate_nis_valid;
        mtrx.lambda_candidate_tested_fix_count =
            ablated.lambda_candidate_tested_fix_count;
        mtrx.lambda_candidate_rms_innovation =
            ablated.lambda_candidate_rms_innovation;
        mtrx.lambda_candidate_max_innovation =
            ablated.lambda_candidate_max_innovation;
        mtrx.lambda_candidate_min_sigma =
            ablated.lambda_candidate_min_sigma;
        mtrx.lambda_candidate_max_sigma =
            ablated.lambda_candidate_max_sigma;
        mtrx.lambda_candidate_max_marginal_nis =
            ablated.lambda_candidate_max_marginal_nis;
        mtrx.lambda_candidate_tested_rows =
            ablated.lambda_candidate_tested_rows * retainedIntegerRows;
        mtrx.lambda_candidate_tested_integers =
            ablated.lambda_candidate_tested_integers;
        mtrx.lambda_dominant_whitened_mode =
            ablated.lambda_dominant_whitened_mode;
        mtrx.lambda_whitened_effective_rank =
            ablated.lambda_whitened_effective_rank;
        mtrx.lambda_dominant_whitened_residual =
            ablated.lambda_dominant_whitened_residual;
        mtrx.lambda_dominant_whitened_nis =
            ablated.lambda_dominant_whitened_nis;
        mtrx.lambda_dominant_whitened_share =
            ablated.lambda_dominant_whitened_share;
        mtrx.lambda_second_whitened_share =
            ablated.lambda_second_whitened_share;
        mtrx.lambda_whitened_condition_number =
            ablated.lambda_whitened_condition_number;
        mtrx.lambda_whitened_nis_closure =
            ablated.lambda_whitened_nis_closure;
        if (ablated.lambda_dominant_original_loading.size() ==
            retainedIntegerRows.rows())
        {
            mtrx.lambda_dominant_original_loading =
                retainedIntegerRows.transpose() *
                ablated.lambda_dominant_original_loading;
        }
        mtrx.lambda_ablation_input_rows = inputRows;
        mtrx.lambda_ablation_support_rows = supportRows.size();
        mtrx.lambda_ablation_removed_rows = removedRows.size();
        mtrx.lambda_ablation_retained_rows = retainedRows.size();
        mtrx.lambda_ablation_target_mean_sigma = targetMeanSigma;
        mtrx.lambda_ablation_removed_mean_sigma = removedMeanSigma;
        mtrx.lambda_ablation_max_log_var_mismatch =
            maximumLogVarianceMismatch;
        mtrx.lambda_ablation_status = status;
        return fixed;
    }

    const int nmax = mtrx.Dtrs.size();
    int k = nmax - 1;
    double succ = erf(sqrt(1 / (8 * mtrx.Dtrs(k--))));
    if (succ < opt.sucthr)
    {
        return 0;
    }

    int zsiz = 1;
    while (k >= 0 &&
           (opt.max_lambda_fix_count <= 0 ||
            zsiz < opt.max_lambda_fix_count))
    {
        succ *= erf(sqrt(1 / (8 * mtrx.Dtrs(k--))));
        if (succ < opt.sucthr)
        {
            break;
        }
        zsiz++;
    }
    if (zsiz < minimumFixCount)
    {
        return 0;
    }
    mtrx.lambda_initial_fix_count = zsiz;

    map<double, VectorXd> zfixList;
    while (zsiz >= minimumFixCount)
    {
        zfixList = lambdaSearchReducedSuffix(mtrx, zsiz, opt);
        if (zfixList.empty())
        {
            break;
        }
        const double mindist = zfixList.begin()->first;
        if (!(opt.lambda_candidate_nis_alpha > 0 &&
              opt.lambda_candidate_nis_alpha < 1))
        {
            break;
        }

        boost::math::chi_squared distribution(zsiz);
        const double threshold = quantile(complement(
            distribution,
            opt.lambda_candidate_nis_alpha
        ));
        mtrx.lambda_candidate_nis           = mindist;
        mtrx.lambda_candidate_nis_threshold = threshold;
        mtrx.lambda_candidate_nis_valid =
            std::isfinite(mindist) && std::isfinite(threshold);
        const bool accepted = mtrx.lambda_candidate_nis_valid &&
            mindist <= threshold;
        if (accepted || zsiz == minimumFixCount)
        {
            VectorXd innovation = zfixList.begin()->second -
                mtrx.zflt.tail(zsiz);
            MatrixXd rows = mtrx.Ztrs.bottomRows(zsiz);
            MatrixXd covariance =
                rows * mtrx.Paflt * rows.transpose();
            mtrx.lambda_candidate_tested_rows = rows;
            mtrx.lambda_candidate_tested_integers =
                zfixList.begin()->second;
            covariance = 0.5 * (covariance + covariance.transpose());
            VectorXd variances = covariance.diagonal();
            mtrx.lambda_candidate_tested_fix_count = zsiz;
            mtrx.lambda_candidate_rms_innovation =
                std::sqrt(innovation.squaredNorm() / zsiz);
            mtrx.lambda_candidate_max_innovation =
                innovation.cwiseAbs().maxCoeff();
            mtrx.lambda_candidate_min_sigma =
                std::sqrt(std::max(0.0, variances.minCoeff()));
            mtrx.lambda_candidate_max_sigma =
                std::sqrt(std::max(0.0, variances.maxCoeff()));
            mtrx.lambda_candidate_max_marginal_nis = 0;
            for (int index = 0; index < zsiz; index++)
            {
                if (variances(index) > 0)
                {
                    mtrx.lambda_candidate_max_marginal_nis = std::max(
                        mtrx.lambda_candidate_max_marginal_nis,
                        innovation(index) * innovation(index) /
                            variances(index)
                    );
                }
            }

            Eigen::SelfAdjointEigenSolver<MatrixXd> eigenSolver(covariance);
            if (eigenSolver.info() == Eigen::Success &&
                eigenSolver.eigenvalues().allFinite())
            {
                const double largestEigenvalue =
                    eigenSolver.eigenvalues().maxCoeff();
                const double tolerance = std::max(
                    1e-14,
                    1e-12 * std::max(0.0, largestEigenvalue)
                );
                VectorXd projected =
                    eigenSolver.eigenvectors().transpose() * innovation;
                vector<pair<double, int>> contributions;
                double minimumPositiveEigenvalue =
                    std::numeric_limits<double>::infinity();
                double whitenedNis = 0;
                for (int mode = 0; mode < zsiz; mode++)
                {
                    const double eigenvalue =
                        eigenSolver.eigenvalues()(mode);
                    if (eigenvalue <= tolerance)
                    {
                        continue;
                    }
                    const double contribution =
                        projected(mode) * projected(mode) / eigenvalue;
                    contributions.push_back({contribution, mode});
                    whitenedNis += contribution;
                    minimumPositiveEigenvalue = std::min(
                        minimumPositiveEigenvalue,
                        eigenvalue
                    );
                }
                std::sort(
                    contributions.begin(),
                    contributions.end(),
                    [](const auto& left, const auto& right)
                    {
                        return left.first > right.first;
                    }
                );
                mtrx.lambda_whitened_effective_rank = contributions.size();
                mtrx.lambda_whitened_nis_closure =
                    std::abs(whitenedNis - mindist);
                if (minimumPositiveEigenvalue <
                    std::numeric_limits<double>::infinity())
                {
                    mtrx.lambda_whitened_condition_number =
                        largestEigenvalue / minimumPositiveEigenvalue;
                }
                if (!contributions.empty() && whitenedNis > 0)
                {
                    const auto [dominantNis, dominantMode] =
                        contributions.front();
                    const double eigenvalue =
                        eigenSolver.eigenvalues()(dominantMode);
                    mtrx.lambda_dominant_whitened_mode = dominantMode;
                    mtrx.lambda_dominant_whitened_residual =
                        projected(dominantMode) / std::sqrt(eigenvalue);
                    mtrx.lambda_dominant_whitened_nis = dominantNis;
                    mtrx.lambda_dominant_whitened_share =
                        dominantNis / whitenedNis;
                    if (contributions.size() > 1)
                    {
                        mtrx.lambda_second_whitened_share =
                            contributions[1].first / whitenedNis;
                    }
                    mtrx.lambda_dominant_original_loading =
                        rows.transpose() *
                        eigenSolver.eigenvectors().col(dominantMode) /
                        std::sqrt(eigenvalue);
                }
            }
        }
        if (accepted)
        {
            break;
        }
        zsiz--;
    }

    if (zsiz < minimumFixCount || zfixList.empty())
    {
        mtrx.Ztrs.resize(0, nmax);
        mtrx.zfix.resize(0);
        return 0;
    }

    double   mindist = zfixList.begin()->first;
    VectorXd zfix0   = zfixList.begin()->second;
    mtrx.zfix        = zfix0;
    MatrixXd Z       = mtrx.Ztrs.bottomRows(zsiz);
    mtrx.Ztrs        = Z;
    mtrx.lambda_selected_bootstrap_success =
        lambdaSelectedSuffixBootstrapSuccess(mtrx.Dtrs, zsiz);

    switch (opt.mode)
    {
        case E_ARmode::LAMBDA:
            return zfix0.size();

        case E_ARmode::LAMBDA_ALT:
        {
            double first  = 0;
            double second = 0;
            for (auto& [dis, fixvec] : zfixList)
            {
                if (first == 0)
                    first = dis;
                else if (second == 0)
                    second = dis;
                else
                    break;
            }

            if ((second / first) < opt.ratthr)
                return 0;
            else
                return zfix0.size();
        }

        case E_ARmode::LAMBDA_AL2:
        {
            for (auto& [dis, fixvec] : zfixList)
            {
                if ((dis / mindist) > opt.ratthr)
                    break;

                for (int l = 0; l < zfix0.size(); l++)
                {
                    if (zfix0(l) == -99999.5)
                        continue;

                    if (zfix0(l) != fixvec(l))
                        zfix0(l) = -99999.5;
                }
            }

            vector<int> zind;
            for (int k = 0; k < zfix0.size(); k++)
                if (zfix0(k) != -99999.5)
                    zind.push_back(k);
            tracepdeex(2, trace, "... %d ambiguties in common\n", zind.size());

            vector<int> xind;
            for (int k = 0; k < nmax; k++)
                xind.push_back(k);

            mtrx.zfix = zfix0(zind);
            mtrx.Ztrs = Z(zind, xind);

            return zind.size();
        }

        case E_ARmode::LAMBDA_BIE:
        {
            double acum = 0;

            for (auto& [dis, fixvec] : zfixList)
            {
                double fct = exp(-0.5 * (dis - mindist));
                acum += fct;
            }

            VectorXd zbie = VectorXd::Zero(zsiz);

            for (auto& [dis, fixvec] : zfixList)
            {
                double fct = exp(-0.5 * (dis - mindist)) / acum;
                if (AR_VERBO)
                    trace << "\n"
                          << "BIE Candidate found:" << fixvec.transpose() << ";   dist= " << dis
                          << ";   fact= " << fct;
                zbie += fct * fixvec;
            }

            mtrx.zfix = zbie;

            return zbie.size();
        }
    }

    return 0;
}

namespace
{
struct LambdaBeamNode
{
    MatrixXd poolRows;
    MatrixXd rows;
    VectorXd values;
    int      depth = 0;
    double   bootstrapSuccess = 0;
    double   bootstrapLogFailure = 0;
    double   nis = std::numeric_limits<double>::infinity();
    double   nisThreshold = 0;
    double   candidateRatio = 0;
    double   productGain = std::numeric_limits<double>::quiet_NaN();
    bool     nisCompatible = false;
    bool     ffrtPass = false;
    bool     ffrtCandidateConsistent = false;
    string   hnfCanonicalKey;
    string   hnfFingerprint;
    double   branchPriority = 0;
};

double lambdaBootstrapSuccess(const VectorXd& conditionalVariances)
{
    double success = 1;
    for (double variance : conditionalVariances)
    {
        if (!(variance > 0) || !std::isfinite(variance))
        {
            return 0;
        }
        success *= erf(std::sqrt(1 / (8 * variance)));
    }
    return success;
}

int lambdaReliableCoreRank(
    const VectorXd& conditionalVariances,
    double          successThreshold,
    int             maximumDimension
)
{
    int rank = 0;
    double cumulativeSuccess = 1;
    for (int row = conditionalVariances.size() - 1; row >= 0; row--)
    {
        const double variance = conditionalVariances(row);
        if (!(variance > 0) || !std::isfinite(variance))
        {
            break;
        }
        const double trial = cumulativeSuccess *
            erf(std::sqrt(1 / (8 * variance)));
        if (trial < successThreshold)
        {
            break;
        }
        cumulativeSuccess = trial;
        rank++;
        if (maximumDimension > 0 && rank >= maximumDimension)
        {
            break;
        }
    }
    return rank;
}

bool evaluateLambdaBeamNode(
    Trace&                          trace,
    const GinAR_mtx&                source,
    const GinAR_opt&                options,
    const GinAR_lambda_beam_options& beamOptions,
    const MatrixXd&                 productCrossCovariance,
    double                          productVarianceTrace,
    LambdaBeamNode&                 node
)
{
    GinAR_mtx local;
    local.aflt = node.poolRows * source.aflt;
    local.Paflt = node.poolRows * source.Paflt * node.poolRows.transpose();
    if (Ztrans_reduction(trace, local) < 0)
    {
        return false;
    }

    const int coreRank = lambdaReliableCoreRank(
        local.Dtrs,
        options.sucthr,
        beamOptions.core_max_dimension
    );
    if (coreRank < beamOptions.minimum_rank)
    {
        return false;
    }
    node.bootstrapSuccess = lambdaBootstrapSuccess(
        local.Dtrs.tail(coreRank));
    node.bootstrapLogFailure = zhangBootstrapLogFailure(
        local.Dtrs.tail(coreRank));
    map<double, VectorXd> candidates = lambdaSearchReducedSuffix(
        local,
        coreRank,
        options
    );
    if (candidates.empty())
    {
        return false;
    }

    node.poolRows = local.Ztrs * node.poolRows;
    node.rows = node.poolRows.bottomRows(coreRank);
    node.values = candidates.begin()->second;
    node.hnfCanonicalKey = zhangIntegerRowHnfCanonicalKey(node.rows);
    node.hnfFingerprint = zhangIntegerRowHnfFingerprint(node.rows);
    if (node.hnfCanonicalKey == "NON_INTEGER" ||
        node.hnfCanonicalKey == "INCONSISTENT")
    {
        return false;
    }

    auto candidate = candidates.begin();
    const double firstDistance = candidate->first;
    if (++candidate != candidates.end() && firstDistance > 0)
    {
        node.candidateRatio = candidate->first / firstDistance;
    }

    VectorXd innovation = node.values - node.rows * source.aflt;
    MatrixXd covariance = node.rows * source.Paflt * node.rows.transpose();
    VectorXd nodeMean = node.rows * source.aflt;
    vector<double> ffrtCandidates(node.rows.rows() * 2);
    double ffrtDistances[2] = {};
    MatrixXd ffrtTransform;
    MatrixXd ffrtReducedCovariance;
    VectorXd ffrtConditionalVariances;
    VectorXd ffrtConditionalSuccessRates;
    double ffrtBootstrapSuccess = 0;
    bool ffrtValidationPass = false;
    const int ffrtStatus = lambdaWithTransform(
        trace,
        node.rows.rows(),
        2,
        nodeMean.data(),
        covariance.data(),
        ffrtCandidates.data(),
        ffrtDistances,
        beamOptions.fixed_failure_rate,
        ffrtValidationPass,
        ffrtTransform,
        ffrtReducedCovariance,
        ffrtConditionalVariances,
        ffrtConditionalSuccessRates,
        ffrtBootstrapSuccess
    );
    if (ffrtStatus != 0 || !std::isfinite(ffrtBootstrapSuccess))
    {
        return false;
    }
    const VectorXd ffrtBest = Eigen::Map<VectorXd>(
        ffrtCandidates.data(), node.rows.rows());
    node.ffrtCandidateConsistent =
        (ffrtBest - node.values).cwiseAbs().maxCoeff() <= 1e-7;
    node.ffrtPass = ffrtValidationPass && node.ffrtCandidateConsistent;
    node.bootstrapSuccess = std::min(
        node.bootstrapSuccess,
        ffrtBootstrapSuccess
    );
    if (ffrtBootstrapSuccess < 1)
    {
        node.bootstrapLogFailure = std::max(
            node.bootstrapLogFailure,
            std::log1p(-ffrtBootstrapSuccess));
    }
    if (ffrtDistances[0] > 0 && std::isfinite(ffrtDistances[1]))
    {
        node.candidateRatio = ffrtDistances[1] / ffrtDistances[0];
    }
    ZhangConstraintNisLeverage leverage =
        zhangConstraintNisLeverage(innovation, covariance);
    if (!leverage.valid)
    {
        return false;
    }
    node.nis = leverage.nis;
    boost::math::chi_squared distribution(node.rows.rows());
    node.nisThreshold = quantile(complement(
        distribution,
        options.lambda_candidate_nis_alpha
    ));
    node.nisCompatible =
        node.rows.rows() >= beamOptions.minimum_rank &&
        node.bootstrapSuccess >= options.sucthr &&
        node.ffrtPass &&
        std::isfinite(node.nisThreshold) &&
        node.nis <= node.nisThreshold;
    node.productGain = zhangConstraintProductInformationGain(
        productCrossCovariance,
        productVarianceTrace,
        source.Paflt,
        node.rows
    );
    return true;
}

vector<LambdaBeamNode> branchLambdaBeamNode(
    Trace&                          trace,
    const GinAR_mtx&                source,
    const GinAR_lambda_beam_options& beamOptions,
    const MatrixXd&                 productCrossCovariance,
    double                          productVarianceTrace,
    const LambdaBeamNode&           node
)
{
    vector<LambdaBeamNode> branches;
    if (node.rows.rows() <= beamOptions.minimum_rank ||
        node.values.size() != node.rows.rows())
    {
        return branches;
    }

    const VectorXd innovation = node.values - node.rows * source.aflt;
    const MatrixXd covariance =
        node.rows * source.Paflt * node.rows.transpose();
    ZhangConstraintNisLeverage leverage =
        zhangConstraintNisLeverage(innovation, covariance);
    if (!leverage.valid)
    {
        return branches;
    }

    struct RankedDeletion
    {
        int    row = -1;
        double nisReduction = 0;
        double productLoss = 0;
        double score = 0;
    };
    vector<RankedDeletion> ranked;
    const bool hasProductGain = std::isfinite(node.productGain);
    for (int removed = 0; removed < node.rows.rows(); removed++)
    {
        MatrixXd retained(node.rows.rows() - 1, node.rows.cols());
        if (removed > 0)
        {
            retained.topRows(removed) = node.rows.topRows(removed);
        }
        if (removed + 1 < node.rows.rows())
        {
            retained.bottomRows(node.rows.rows() - removed - 1) =
                node.rows.bottomRows(node.rows.rows() - removed - 1);
        }
        double loss = 0;
        if (hasProductGain)
        {
            const double retainedGain =
                zhangConstraintProductInformationGain(
                    productCrossCovariance,
                    productVarianceTrace,
                    source.Paflt,
                    retained
                );
            if (std::isfinite(retainedGain))
            {
                loss = std::max(0.0, node.productGain - retainedGain);
            }
        }
        const double score = leverage.deletionReduction(removed) /
            (1e-12 + loss);
        ranked.push_back({
            removed,
            leverage.deletionReduction(removed),
            loss,
            score
        });
    }
    std::sort(
        ranked.begin(), ranked.end(),
        [](const RankedDeletion& left, const RankedDeletion& right)
        {
            if (left.score != right.score)
            {
                return left.score > right.score;
            }
            return left.row < right.row;
        }
    );

    const int branchCount = std::min(
        beamOptions.branch_factor,
        static_cast<int>(ranked.size())
    );
    for (int branch = 0; branch < branchCount; branch++)
    {
        const RankedDeletion& deletion = ranked[branch];
        LambdaBeamNode child;
        child.depth = node.depth + 1;
        child.branchPriority = deletion.score;
        const int poolRemoved = node.poolRows.rows() - node.rows.rows() +
            deletion.row;
        child.poolRows.resize(
            node.poolRows.rows() - 1, node.poolRows.cols());
        if (poolRemoved > 0)
        {
            child.poolRows.topRows(poolRemoved) =
                node.poolRows.topRows(poolRemoved);
        }
        if (poolRemoved + 1 < node.poolRows.rows())
        {
            child.poolRows.bottomRows(
                node.poolRows.rows() - poolRemoved - 1) =
                node.poolRows.bottomRows(
                    node.poolRows.rows() - poolRemoved - 1);
        }
        trace << "\nZHANG_LAMBDA_BEAM_BRANCH"
              << " context=" << beamOptions.context
              << " parent_depth=" << node.depth
              << " parent_rank=" << node.rows.rows()
              << " parent_pool_rank=" << node.poolRows.rows()
              << " removed_integer_row=" << deletion.row
              << " exact_nis_reduction=" << deletion.nisReduction
              << " product_information_loss=" << deletion.productLoss
              << " branch_score=" << deletion.score
              << " score_definition=DELTA_NIS_OVER_PRODUCT_LOSS";
        branches.push_back(std::move(child));
    }
    return branches;
}
}

GinAR_lambda_beam_result GNSS_AR_LAMBDA_BEAM_SHADOW(
    Trace&                           trace,
    const GinAR_mtx&                 source,
    const GinAR_opt&                 options,
    const GinAR_lambda_beam_options& beamOptions,
    const MatrixXd&                  productCrossCovariance,
    double                           productVarianceTrace
)
{
    GinAR_lambda_beam_result result;
    if (source.aflt.size() < beamOptions.minimum_rank ||
        source.Paflt.rows() != source.aflt.size() ||
        !(options.lambda_candidate_nis_alpha > 0) ||
        !(options.lambda_candidate_nis_alpha < 1))
    {
        return result;
    }

    GinAR_mtx reduced = source;
    if (Ztrans_reduction(trace, reduced) < 0)
    {
        return result;
    }
    const int coreRank = lambdaReliableCoreRank(
        reduced.Dtrs,
        options.sucthr,
        beamOptions.core_max_dimension
    );
    result.initial_core_rank = coreRank;
    if (coreRank < beamOptions.minimum_rank)
    {
        return result;
    }

    LambdaBeamNode root;
    const int poolRank = std::min(
        static_cast<int>(reduced.Ztrs.rows()),
        coreRank + beamOptions.reserve_dimension
    );
    result.initial_pool_rank = poolRank;
    root.poolRows = reduced.Ztrs.bottomRows(poolRank);
    vector<LambdaBeamNode> frontier = {root};
    set<string> visited;
    set<string> evaluatedCandidateSublattices;
    vector<LambdaBeamNode> feasible;
    for (int depth = 0;
         depth <= beamOptions.maximum_depth && !frontier.empty();
         depth++)
    {
        vector<LambdaBeamNode> next;
        for (LambdaBeamNode node : frontier)
        {
            const string inputFingerprint =
                zhangIntegerRowHnfFingerprint(node.poolRows);
            if (!visited.insert(inputFingerprint).second)
            {
                result.duplicate_nodes++;
                continue;
            }
            result.unique_nodes++;
            result.explored_nodes++;
            if (!evaluateLambdaBeamNode(
                    trace,
                    source,
                    options,
                    beamOptions,
                    productCrossCovariance,
                    productVarianceTrace,
                    node))
            {
                trace << "\nZHANG_LAMBDA_BEAM_NODE"
                      << " context=" << beamOptions.context
                      << " depth=" << node.depth
                      << " pool_rank=" << node.poolRows.rows()
                      << " status=NUMERIC_OR_INTEGER_FAILURE"
                      << " feedback=SHADOW_NONE";
                continue;
            }

            trace << "\nZHANG_LAMBDA_BEAM_NODE"
                  << " context=" << beamOptions.context
                  << " depth=" << node.depth
                  << " rank=" << node.rows.rows()
                  << " pool_rank=" << node.poolRows.rows()
                  << " bootstrap_success=" << node.bootstrapSuccess
                  << " bootstrap_log_failure="
                  << node.bootstrapLogFailure
                  << " candidate_ratio=" << node.candidateRatio
                  << " fixed_failure_rate="
                  << beamOptions.fixed_failure_rate
                  << " ffrt_candidate_consistent="
                  << node.ffrtCandidateConsistent
                  << " ffrt_status="
                  << (node.ffrtPass ? "PASSED" : "REJECTED")
                  << " nis=" << node.nis
                  << " nis_threshold=" << node.nisThreshold
                  << " nis_pass=" << node.nisCompatible
                  << " product_information_gain_fraction="
                  << node.productGain
                  << " product_gain_status="
                  << (std::isfinite(node.productGain)
                        ? "EXACT_TRACE_REDUCTION_FRACTION"
                        : "NOT_AVAILABLE_NO_PRODUCT_FUNCTIONAL")
                  << " hnf=" << node.hnfFingerprint
                  << " ar_authorized=0"
                  << " feedback=SHADOW_NONE";
            if (!evaluatedCandidateSublattices.insert(
                    node.hnfCanonicalKey).second)
            {
                result.duplicate_nodes++;
                trace << "\nZHANG_LAMBDA_BEAM_DEDUP"
                      << " context=" << beamOptions.context
                      << " depth=" << node.depth
                      << " rank=" << node.rows.rows()
                      << " hnf=" << node.hnfFingerprint
                      << " status=DUPLICATE_CANDIDATE_SUBLATTICE";
                continue;
            }
            if (node.nisCompatible)
            {
                feasible.push_back(std::move(node));
                continue;
            }
            vector<LambdaBeamNode> branches = branchLambdaBeamNode(
                trace,
                source,
                beamOptions,
                productCrossCovariance,
                productVarianceTrace,
                node
            );
            next.insert(
                next.end(),
                std::make_move_iterator(branches.begin()),
                std::make_move_iterator(branches.end())
            );
        }
        if (!feasible.empty() && !beamOptions.prefer_product_gain)
        {
            break;
        }
        std::sort(
            next.begin(), next.end(),
            [&](const LambdaBeamNode& left, const LambdaBeamNode& right)
            {
                if (left.branchPriority != right.branchPriority)
                {
                    return left.branchPriority > right.branchPriority;
                }
                return zhangIntegerRowHnfFingerprint(left.poolRows) <
                    zhangIntegerRowHnfFingerprint(right.poolRows);
            }
        );
        if (static_cast<int>(next.size()) > beamOptions.beam_width)
        {
            next.resize(beamOptions.beam_width);
        }
        frontier = std::move(next);
    }

    result.nis_compatible_nodes = feasible.size();
    if (feasible.empty())
    {
        return result;
    }
    std::sort(
        feasible.begin(), feasible.end(),
        [&](const LambdaBeamNode& left, const LambdaBeamNode& right)
        {
            const double leftGain = std::isfinite(left.productGain)
                ? left.productGain : -1;
            const double rightGain = std::isfinite(right.productGain)
                ? right.productGain : -1;
            if (beamOptions.prefer_product_gain && leftGain != rightGain)
            {
                return leftGain > rightGain;
            }
            if (left.rows.rows() != right.rows.rows())
            {
                return left.rows.rows() > right.rows.rows();
            }
            if (leftGain != rightGain)
            {
                return leftGain > rightGain;
            }
            return left.nis < right.nis;
        }
    );
    const LambdaBeamNode& selected = feasible.front();
    result.nis_compatible_found = true;
    result.selected_rank = selected.rows.rows();
    result.selected_depth = selected.depth;
    result.selected_bootstrap_success = selected.bootstrapSuccess;
    result.selected_bootstrap_log_failure = selected.bootstrapLogFailure;
    result.selected_nis = selected.nis;
    result.selected_nis_threshold = selected.nisThreshold;
    result.selected_candidate_ratio = selected.candidateRatio;
    result.selected_ffrt_pass = selected.ffrtPass;
    result.selected_product_gain = selected.productGain;
    result.product_gain_available = std::isfinite(selected.productGain);
    result.selected_hnf_fingerprint = selected.hnfFingerprint;
    result.selected_integer_rows = selected.rows;
    result.selected_integer_values = selected.values;
    return result;
}

GinAR_lambda_subset_oracle_result
GNSS_AR_LAMBDA_SUBSET_ORACLE_SHADOW(
    Trace&                                   trace,
    const GinAR_mtx&                         source,
    const GinAR_opt&                         options,
    const GinAR_lambda_subset_oracle_options& oracleOptions,
    const MatrixXd&                          productCrossCovariance,
    double                                   productVarianceTrace
)
{
    GinAR_lambda_subset_oracle_result result;
    if (source.aflt.size() < oracleOptions.minimum_rank ||
        oracleOptions.minimum_rank < 1 ||
        oracleOptions.maximum_rank < oracleOptions.minimum_rank ||
        oracleOptions.pool_dimension < oracleOptions.minimum_rank ||
        oracleOptions.maximum_subsets < 1)
    {
        return result;
    }

    GinAR_mtx reduced = source;
    if (Ztrans_reduction(trace, reduced) < 0)
    {
        return result;
    }
    result.dictionary_rank = std::min(
        oracleOptions.pool_dimension,
        static_cast<int>(reduced.Ztrs.rows()));
    const MatrixXd dictionary = reduced.Ztrs.bottomRows(
        result.dictionary_rank);
    set<string> exactSublattices;
    LambdaBeamNode best;
    bool stop = false;
    for (int rank = oracleOptions.minimum_rank;
         rank <= std::min(
            oracleOptions.maximum_rank, result.dictionary_rank) && !stop;
         rank++)
    {
        vector<int> selected;
        std::function<void(int)> enumerate = [&](int next)
        {
            if (stop)
            {
                return;
            }
            if (static_cast<int>(selected.size()) == rank)
            {
                if (result.enumerated_subsets >= oracleOptions.maximum_subsets)
                {
                    stop = true;
                    return;
                }
                result.enumerated_subsets++;
                LambdaBeamNode node;
                node.poolRows = MatrixXd::Zero(rank, source.aflt.size());
                for (int row = 0; row < rank; row++)
                {
                    node.poolRows.row(row) = dictionary.row(selected[row]);
                }
                GinAR_lambda_beam_options nodeOptions;
                nodeOptions.core_max_dimension = rank;
                nodeOptions.reserve_dimension = 0;
                nodeOptions.minimum_rank = rank;
                nodeOptions.fixed_failure_rate =
                    oracleOptions.fixed_failure_rate;
                nodeOptions.context = oracleOptions.context;
                if (!evaluateLambdaBeamNode(
                        trace,
                        source,
                        options,
                        nodeOptions,
                        productCrossCovariance,
                        productVarianceTrace,
                        node))
                {
                    return;
                }
                if (!exactSublattices.insert(node.hnfCanonicalKey).second)
                {
                    return;
                }
                result.unique_sublattices++;
                if (!node.nisCompatible)
                {
                    return;
                }
                result.feasible_sublattices++;
                if (!result.feasible_found ||
                    node.productGain > best.productGain ||
                    (node.productGain == best.productGain &&
                     node.rows.rows() > best.rows.rows()))
                {
                    best = std::move(node);
                    result.feasible_found = true;
                }
                return;
            }
            const int needed = rank - selected.size();
            for (int index = next;
                 index <= result.dictionary_rank - needed; index++)
            {
                selected.push_back(index);
                enumerate(index + 1);
                selected.pop_back();
                if (stop)
                {
                    return;
                }
            }
        };
        enumerate(0);
    }
    if (result.feasible_found)
    {
        result.selected_rank = best.rows.rows();
        result.selected_product_gain = best.productGain;
        result.selected_nis = best.nis;
        result.selected_nis_threshold = best.nisThreshold;
        result.selected_hnf_fingerprint = best.hnfFingerprint;
        result.selected_integer_rows = std::move(best.rows);
        result.selected_integer_values = std::move(best.values);
    }
    trace << "\nZHANG_LAMBDA_SUBSET_ORACLE"
          << " context=" << oracleOptions.context
          << " dictionary_rank=" << result.dictionary_rank
          << " rank_range=" << oracleOptions.minimum_rank << "-"
          << oracleOptions.maximum_rank
          << " enumerated_subsets=" << result.enumerated_subsets
          << " unique_sublattices=" << result.unique_sublattices
          << " feasible_sublattices=" << result.feasible_sublattices
          << " selected_rank=" << result.selected_rank
          << " product_information_gain_fraction="
          << result.selected_product_gain
          << " nis=" << result.selected_nis
          << " nis_threshold=" << result.selected_nis_threshold
          << " hnf=" << result.selected_hnf_fingerprint
          << " oracle_scope=BOUNDED_SUBSETS_OF_LAMBDA_RELIABLE_DICTIONARY"
          << " ar_authorized=0 feedback=SHADOW_NONE";
    return result;
}

/** Ambiguity resolution function for Ginan */
int GNSS_AR(
    Trace&     trace,  ///< Debug trace
    GinAR_mtx& mtrx,   ///< Reference to structure containing float values and covariance
    GinAR_opt  opt     ///< Object containing processing options
)
{
    switch (opt.mode)
    {
        case E_ARmode::OFF:
            return 0;
        case E_ARmode::ROUND:
            return simple_round(trace, mtrx, opt);
        case E_ARmode::ITER_RND:
            return interat_round(trace, mtrx, opt);
        case E_ARmode::BOOTST:
            return integer_bootst(trace, mtrx, opt);
        case E_ARmode::LAMBDA:
            return lambda_search(trace, mtrx, opt);
        case E_ARmode::LAMBDA_ALT:
            return lambda_search(trace, mtrx, opt);
        case E_ARmode::LAMBDA_AL2:
            return lambda_search(trace, mtrx, opt);
        case E_ARmode::LAMBDA_BIE:
            return lambda_search(trace, mtrx, opt);
            // default:							tracepdeex(1, trace, "\n AR mode not supported \n");
    }

    return 0;
}
