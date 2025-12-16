#include "common/kalmanBlas.hpp"
#include <boost/log/trivial.hpp>
#include <cstring>

// Compute H*P using BLAS
void KalmanFilterBLAS::computeHP(const MatrixXd& H, const MatrixXd& P, MatrixXd& HP)
{
    int numH = H.rows();
    int numX = H.cols();

    // Resize output if needed
    if (HP.rows() != numH || HP.cols() != numX)
    {
        HP.resize(numH, numX);
    }

    // HP = H * P
    // C = alpha*A*B + beta*C
    LapackWrapper::dgemm(
        LapackWrapper::CblasColMajor,  // Eigen uses column-major storage
        LapackWrapper::CblasNoTrans,   // Don't transpose H
        LapackWrapper::CblasNoTrans,   // Don't transpose P
        numH,                          // Rows of H (and HP)
        numX,                          // Cols of P (and HP)
        numX,                          // Cols of H / rows of P
        1.0,                           // alpha = 1.0
        H.data(),                      // Matrix H
        H.rows(),                      // Leading dimension of H
        P.data(),                      // Matrix P
        P.rows(),                      // Leading dimension of P
        0.0,                           // beta = 0.0 (don't accumulate)
        HP.data(),                     // Output matrix HP
        HP.rows()                      // Leading dimension of HP
    );
}

// Compute innovation covariance Q = H*P*H' + R using BLAS
void KalmanFilterBLAS::computeInnovationCovariance(
    const MatrixXd& H,
    const MatrixXd& P,
    const MatrixXd& R,
    MatrixXd&       Q
)
{
    int numH = H.rows();
    int numX = H.cols();

    // Resize output if needed
    if (Q.rows() != numH || Q.cols() != numH)
    {
        Q.resize(numH, numH);
    }

    // Step 1: Compute HP = H * P
    MatrixXd HP(numH, numX);
    computeHP(H, P, HP);

    // Step 2: Copy R into Q (Q will be HP*H' + R)
    std::memcpy(Q.data(), R.data(), numH * numH * sizeof(double));

    // Step 3: Q = HP * H' + Q (Q already contains R)
    // C = alpha*A*B' + beta*C
    LapackWrapper::dgemm(
        LapackWrapper::CblasColMajor,  // Eigen uses column-major storage
        LapackWrapper::CblasNoTrans,   // Don't transpose HP
        LapackWrapper::CblasTrans,     // Transpose H
        numH,                          // Rows of HP (and Q)
        numH,                          // Rows of H (cols of H')
        numX,                          // Cols of HP / cols of H
        1.0,                           // alpha = 1.0
        HP.data(),                     // Matrix HP
        HP.rows(),                     // Leading dimension of HP
        H.data(),                      // Matrix H (will be transposed)
        H.rows(),                      // Leading dimension of H
        1.0,                           // beta = 1.0 (accumulate with R)
        Q.data(),                      // Output matrix Q
        Q.rows()                       // Leading dimension of Q
    );
}

// Solve linear system Q*X = B using LAPACKE with fallback chain
bool KalmanFilterBLAS::solveLinearSystem(MatrixXd& Q, MatrixXd& B, E_Inverter inverter)
{
    int n    = Q.rows();
    int nrhs = B.cols();
    int info;

    // Always keep backup for fallback chain
    MatrixXd Q_backup = Q;
    MatrixXd B_backup = B;

    bool      repeat       = true;
    int       attempts     = 0;
    const int max_attempts = 10;  // Prevent infinite loops

    while (repeat && attempts < max_attempts)
    {
        repeat = false;
        attempts++;

        switch (inverter)
        {
            case E_Inverter::LLT:
            {
                // Try 1: Cholesky factorization (dposv) - fastest but requires positive definite
                info = LapackWrapper::dposv(
                    LapackWrapper::COL_MAJOR,
                    'U',       // Upper triangle
                    n,         // Order of matrix
                    nrhs,      // Number of right-hand sides
                    Q.data(),  // Matrix (modified on output)
                    n,         // Leading dimension
                    B.data(),  // RHS on input, solution on output
                    n          // Leading dimension of B
                );

                if (info != 0)
                {
                    BOOST_LOG_TRIVIAL(warning) << "LapackWrapper::dposv failed with info = " << info
                                               << ", falling back to LDLT";

                    // Restore from backup and retry with LDLT
                    Q        = Q_backup;
                    B        = B_backup;
                    inverter = E_Inverter::LDLT;
                    repeat   = true;
                    continue;
                }

                break;
            }

            case E_Inverter::LDLT:
            {
                // Try 2: Symmetric indefinite factorization (dsysv)
                std::vector<int> ipiv(n);

                info = LapackWrapper::dsysv(
                    LapackWrapper::COL_MAJOR,
                    'U',          // Upper triangle
                    n,            // Order of matrix
                    nrhs,         // Number of right-hand sides
                    Q.data(),     // Matrix (modified on output)
                    n,            // Leading dimension
                    ipiv.data(),  // Pivot indices
                    B.data(),     // RHS on input, solution on output
                    n             // Leading dimension of B
                );

                if (info != 0)
                {
                    BOOST_LOG_TRIVIAL(warning) << "LapackWrapper::dsysv failed with info = " << info
                                               << ", falling back to LU factorization";

                    // Restore from backup and retry with LU
                    Q        = Q_backup;
                    B        = B_backup;
                    inverter = E_Inverter::INV;
                    repeat   = true;
                    continue;
                }

                break;
            }

            case E_Inverter::INV:
            {
                // Try 3: General LU factorization (dgesv)
                std::vector<int> ipiv(n);

                info = LapackWrapper::dgesv(
                    LapackWrapper::COL_MAJOR,
                    n,            // Order of matrix
                    nrhs,         // Number of right-hand sides
                    Q.data(),     // Matrix (modified on output)
                    n,            // Leading dimension
                    ipiv.data(),  // Pivot indices
                    B.data(),     // RHS on input, solution on output
                    n             // Leading dimension of B
                );

                if (info != 0)
                {
                    BOOST_LOG_TRIVIAL(error) << "LapackWrapper::dgesv failed with info = " << info
                                             << " - all solver methods exhausted";
                    return false;
                }

                break;
            }

            default:
            {
                BOOST_LOG_TRIVIAL(error) << "Unknown inverter type: " << inverter;
                return false;
            }
        }
    }

    if (attempts >= max_attempts)
    {
        BOOST_LOG_TRIVIAL(
            error
        ) << "Maximum solver attempts reached - possible configuration error";
        return false;
    }

    return true;
}

// Compute Kalman gain K = P*H'*Q^-1 using BLAS/LAPACKE
bool KalmanFilterBLAS::computeKalmanGain(
    const MatrixXd& H,
    const MatrixXd& P,
    MatrixXd&       Q,
    MatrixXd&       K,
    MatrixXd*       Qinv,
    E_Inverter      inverter
)
{
    int numH = H.rows();
    int numX = H.cols();

    // Resize outputs if needed
    if (K.rows() != numX || K.cols() != numH)
    {
        K.resize(numX, numH);
    }

    // Step 1: Compute HP = H * P
    MatrixXd HP(numH, numX);
    computeHP(H, P, HP);

    // Step 2: Solve Q * K' = HP for K'
    // This gives us K' = Q^-1 * HP, then K = (K')' = HP' * Q^-1 = P*H'*Q^-1
    MatrixXd KT = HP;  // Copy HP, will be overwritten with solution

    if (!solveLinearSystem(Q, KT, inverter))
    {
        return false;
    }

    // Step 3: Transpose to get K = (K')'
    K = KT.transpose();

    // Step 4: Optionally compute Q inverse
    if (Qinv != nullptr)
    {
        // Solve Q * I = I for Q^-1
        Qinv->resize(numH, numH);
        Qinv->setIdentity();

        MatrixXd Q_copy = Q;  // Need copy since Q was modified by previous solve

        if (!solveLinearSystem(Q_copy, *Qinv, inverter))
        {
            return false;
        }
    }

    return true;
}

// Update state vector using BLAS
void KalmanFilterBLAS::updateStateVector(
    const VectorXd& x,
    const MatrixXd& K,
    const VectorXd& v,
    VectorXd&       xp,
    VectorXd&       dx,
    int             begX,
    int             numX,
    int             begH,
    int             numH
)
{
    // Extract subvectors
    auto v_sub = v.segment(begH, numH);

    // Compute dx = K * v for the substate
    // y = alpha*A*x + beta*y
    LapackWrapper::dgemv(
        LapackWrapper::CblasColMajor,
        LapackWrapper::CblasNoTrans,
        numX,              // Rows of K
        numH,              // Cols of K
        1.0,               // alpha
        K.data(),          // Matrix K
        K.rows(),          // Leading dimension
        v_sub.data(),      // Vector v
        1,                 // Stride of v
        0.0,               // beta
        dx.data() + begX,  // Output vector dx (starting at begX)
        1                  // Stride of dx
    );

    // Compute xp = x + dx
    LapackWrapper::dcopy(numX, x.data() + begX, 1, xp.data() + begX, 1);
    LapackWrapper::daxpy(numX, 1.0, dx.data() + begX, 1, xp.data() + begX, 1);
}

// Update covariance using standard form: Pp = P - K*H*P
void KalmanFilterBLAS::updateCovarianceStandard(
    const MatrixXd& P,
    const MatrixXd& K,
    const MatrixXd& H,
    const MatrixXd& HP,
    MatrixXd&       Pp
)
{
    int numX = P.rows();
    int numH = H.rows();

    // Resize output if needed
    if (Pp.rows() != numX || Pp.cols() != numX)
    {
        Pp.resize(numX, numX);
    }

    // Step 1: Copy P into Pp
    std::memcpy(Pp.data(), P.data(), numX * numX * sizeof(double));

    // Step 2: Compute KHP = K * HP
    MatrixXd KHP(numX, numX);
    LapackWrapper::dgemm(
        LapackWrapper::CblasColMajor,
        LapackWrapper::CblasNoTrans,
        LapackWrapper::CblasNoTrans,
        numX,        // Rows of K (and KHP)
        numX,        // Cols of HP (and KHP)
        numH,        // Cols of K / rows of HP
        1.0,         // alpha
        K.data(),    // Matrix K
        K.rows(),    // Leading dimension of K
        HP.data(),   // Matrix HP
        HP.rows(),   // Leading dimension of HP
        0.0,         // beta
        KHP.data(),  // Output matrix KHP
        KHP.rows()   // Leading dimension of KHP
    );

    // Step 3: Pp = Pp - KHP = P - K*H*P
    LapackWrapper::daxpy(numX * numX, -1.0, KHP.data(), 1, Pp.data(), 1);

    // Symmetrize for numerical stability
    symmetrizeMatrix(Pp);
}

// Update covariance using Joseph form: Pp = (I-K*H)*P*(I-K*H)' + K*R*K'
void KalmanFilterBLAS::updateCovarianceJoseph(
    const MatrixXd& P,
    const MatrixXd& K,
    const MatrixXd& H,
    const MatrixXd& R,
    MatrixXd&       Pp
)
{
    int numX = P.rows();
    int numH = H.rows();

    // Resize output if needed
    if (Pp.rows() != numX || Pp.cols() != numX)
    {
        Pp.resize(numX, numX);
    }

    // Step 1: Compute IKH = I - K*H
    MatrixXd IKH = MatrixXd::Identity(numX, numX);
    LapackWrapper::dgemm(
        LapackWrapper::CblasColMajor,
        LapackWrapper::CblasNoTrans,
        LapackWrapper::CblasNoTrans,
        numX,        // Rows of K (and IKH)
        numX,        // Cols of H (and IKH)
        numH,        // Cols of K / rows of H
        -1.0,        // alpha = -1 (subtract)
        K.data(),    // Matrix K
        K.rows(),    // Leading dimension of K
        H.data(),    // Matrix H
        H.rows(),    // Leading dimension of H
        1.0,         // beta = 1 (add to identity)
        IKH.data(),  // Output matrix IKH
        IKH.rows()   // Leading dimension of IKH
    );

    // Step 2: Compute temp = IKH * P
    MatrixXd temp(numX, numX);
    LapackWrapper::dgemm(
        LapackWrapper::CblasColMajor,
        LapackWrapper::CblasNoTrans,
        LapackWrapper::CblasNoTrans,
        numX,         // Rows
        numX,         // Cols
        numX,         // Inner dimension
        1.0,          // alpha
        IKH.data(),   // Matrix IKH
        IKH.rows(),   // Leading dimension
        P.data(),     // Matrix P
        P.rows(),     // Leading dimension
        0.0,          // beta
        temp.data(),  // Output
        temp.rows()   // Leading dimension
    );

    // Step 3: Compute Pp = temp * IKH' = IKH * P * IKH'
    LapackWrapper::dgemm(
        LapackWrapper::CblasColMajor,
        LapackWrapper::CblasNoTrans,
        LapackWrapper::CblasTrans,
        numX,         // Rows
        numX,         // Cols
        numX,         // Inner dimension
        1.0,          // alpha
        temp.data(),  // Matrix temp
        temp.rows(),  // Leading dimension
        IKH.data(),   // Matrix IKH (will be transposed)
        IKH.rows(),   // Leading dimension
        0.0,          // beta
        Pp.data(),    // Output
        Pp.rows()     // Leading dimension
    );

    // Step 4: Add K*R*K' term
    // temp2 = K * R
    MatrixXd temp2(numX, numH);
    LapackWrapper::dgemm(
        LapackWrapper::CblasColMajor,
        LapackWrapper::CblasNoTrans,
        LapackWrapper::CblasNoTrans,
        numX,          // Rows of K
        numH,          // Cols of R
        numH,          // Cols of K / rows of R
        1.0,           // alpha
        K.data(),      // Matrix K
        K.rows(),      // Leading dimension
        R.data(),      // Matrix R
        R.rows(),      // Leading dimension
        0.0,           // beta
        temp2.data(),  // Output
        temp2.rows()   // Leading dimension
    );

    // Pp += temp2 * K' = Pp + K*R*K'
    LapackWrapper::dgemm(
        LapackWrapper::CblasColMajor,
        LapackWrapper::CblasNoTrans,
        LapackWrapper::CblasTrans,
        numX,          // Rows
        numX,          // Cols
        numH,          // Inner dimension
        1.0,           // alpha
        temp2.data(),  // Matrix temp2
        temp2.rows(),  // Leading dimension
        K.data(),      // Matrix K (will be transposed)
        K.rows(),      // Leading dimension
        1.0,           // beta = 1 (accumulate)
        Pp.data(),     // Output
        Pp.rows()      // Leading dimension
    );

    // Symmetrize for numerical stability
    symmetrizeMatrix(Pp);
}

// Symmetrize matrix: A = (A + A') / 2
void KalmanFilterBLAS::symmetrizeMatrix(MatrixXd& A)
{
    int n = A.rows();

    for (int i = 0; i < n; i++)
    {
        for (int j = i + 1; j < n; j++)
        {
            double avg = (A(i, j) + A(j, i)) / 2.0;
            A(i, j)    = avg;
            A(j, i)    = avg;
        }
    }
}
