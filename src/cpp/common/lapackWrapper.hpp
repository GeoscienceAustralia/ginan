#pragma once

#include <vector>

namespace LapackWrapper
{
// Matrix layout enum to match LAPACKE interface
enum class Layout
{
    ColMajor = 102,
    RowMajor = 101
};

// Forward declare Fortran LAPACK/BLAS functions
extern "C"
{
    void dposv_(
        const char* uplo,
        const int*  n,
        const int*  nrhs,
        double*     a,
        const int*  lda,
        double*     b,
        const int*  ldb,
        int*        info
    );
    void dsysv_(
        const char* uplo,
        const int*  n,
        const int*  nrhs,
        double*     a,
        const int*  lda,
        int*        ipiv,
        double*     b,
        const int*  ldb,
        double*     work,
        const int*  lwork,
        int*        info
    );
    void dgetrf_(const int* m, const int* n, double* a, const int* lda, int* ipiv, int* info);
    void dgetrs_(
        const char*   trans,
        const int*    n,
        const int*    nrhs,
        const double* a,
        const int*    lda,
        const int*    ipiv,
        double*       b,
        const int*    ldb,
        int*          info
    );
    void dpotrf_(const char* uplo, const int* n, double* a, const int* lda, int* info);
    void dpotrs_(
        const char*   uplo,
        const int*    n,
        const int*    nrhs,
        const double* a,
        const int*    lda,
        double*       b,
        const int*    ldb,
        int*          info
    );
    void dpotri_(const char* uplo, const int* n, double* a, const int* lda, int* info);
    void dsytrf_(
        const char* uplo,
        const int*  n,
        double*     a,
        const int*  lda,
        int*        ipiv,
        double*     work,
        const int*  lwork,
        int*        info
    );
    void dsytrs_(
        const char*   uplo,
        const int*    n,
        const int*    nrhs,
        const double* a,
        const int*    lda,
        const int*    ipiv,
        double*       b,
        const int*    ldb,
        int*          info
    );
    void dsytri_(
        const char* uplo,
        const int*  n,
        double*     a,
        const int*  lda,
        const int*  ipiv,
        double*     work,
        int*        info
    );
    void dgetri_(
        const int* n,
        double*    a,
        const int* lda,
        const int* ipiv,
        double*    work,
        const int* lwork,
        int*       info
    );
}

// BLAS function declarations - handle different environments
// Eigen/OpenBLAS declares these with int return, standard BLAS uses void
#if !defined(EIGEN_USE_BLAS) && !defined(EIGEN_BLAS_H)
// Standard BLAS declarations (void return, Fortran style)
extern "C"
{
    void dgemm_(
        const char*   transa,
        const char*   transb,
        const int*    m,
        const int*    n,
        const int*    k,
        const double* alpha,
        const double* a,
        const int*    lda,
        const double* b,
        const int*    ldb,
        const double* beta,
        double*       c,
        const int*    ldc
    );
    void dgemv_(
        const char*   trans,
        const int*    m,
        const int*    n,
        const double* alpha,
        const double* a,
        const int*    lda,
        const double* x,
        const int*    incx,
        const double* beta,
        double*       y,
        const int*    incy
    );
    void dcopy_(const int* n, const double* x, const int* incx, double* y, const int* incy);
    void daxpy_(
        const int*    n,
        const double* alpha,
        const double* x,
        const int*    incx,
        double*       y,
        const int*    incy
    );
}
#endif
// Note: When EIGEN_USE_BLAS/EIGEN_BLAS_H is defined, these are already declared by Eigen headers

// Cholesky factorization (positive definite)
inline int dpotrf(Layout layout, char uplo, int n, double* a, int lda)
{
    if (layout != Layout::ColMajor)
        return -1;  // Only column-major supported with Fortran LAPACK

    int info = 0;
    dpotrf_(&uplo, &n, a, &lda, &info);
    return info;
}  // Solve using Cholesky factorization
inline int
dpotrs(Layout layout, char uplo, int n, int nrhs, const double* a, int lda, double* b, int ldb)
{
    if (layout != Layout::ColMajor)
        return -1;

    int info = 0;
    dpotrs_(&uplo, &n, &nrhs, a, &lda, b, &ldb, &info);
    return info;
}  // Inverse using Cholesky factorization
inline int dpotri(Layout layout, char uplo, int n, double* a, int lda)
{
    if (layout != Layout::ColMajor)
        return -1;

    int info = 0;
    dpotri_(&uplo, &n, a, &lda, &info);
    return info;
}  // Symmetric indefinite factorization
inline int dsytrf(Layout layout, char uplo, int n, double* a, int lda, int* ipiv)
{
    if (layout != Layout::ColMajor)
        return -1;

    // Query workspace size
    int    lwork = -1;
    double work_query;
    int    info = 0;

    dsytrf_(&uplo, &n, a, &lda, ipiv, &work_query, &lwork, &info);

    if (info != 0)
        return info;

    // Allocate workspace and perform factorization
    lwork = static_cast<int>(work_query);
    std::vector<double> work(lwork);
    dsytrf_(&uplo, &n, a, &lda, ipiv, work.data(), &lwork, &info);
    return info;
}

// Solve using symmetric indefinite factorization
inline int dsytrs(
    Layout        layout,
    char          uplo,
    int           n,
    int           nrhs,
    const double* a,
    int           lda,
    const int*    ipiv,
    double*       b,
    int           ldb
)
{
    if (layout != Layout::ColMajor)
        return -1;

    int info = 0;
    dsytrs_(&uplo, &n, &nrhs, a, &lda, ipiv, b, &ldb, &info);
    return info;
}  // Inverse using symmetric indefinite factorization
inline int dsytri(Layout layout, char uplo, int n, double* a, int lda, int* ipiv)
{
    if (layout != Layout::ColMajor)
        return -1;

    // Allocate workspace (dsytri requires work array of size n)
    std::vector<double> work(n);
    int                 info = 0;

    dsytri_(&uplo, &n, a, &lda, ipiv, work.data(), &info);
    return info;
}

// LU factorization
inline int dgetrf(Layout layout, int m, int n, double* a, int lda, int* ipiv)
{
    if (layout != Layout::ColMajor)
        return -1;

    int info = 0;
    dgetrf_(&m, &n, a, &lda, ipiv, &info);
    return info;
}

// Solve using LU factorization
inline int dgetrs(
    Layout        layout,
    char          trans,
    int           n,
    int           nrhs,
    const double* a,
    int           lda,
    const int*    ipiv,
    double*       b,
    int           ldb
)
{
    if (layout != Layout::ColMajor)
        return -1;

    int info = 0;
    dgetrs_(&trans, &n, &nrhs, a, &lda, ipiv, b, &ldb, &info);
    return info;
}  // Inverse using LU factorization
inline int dgetri(Layout layout, int n, double* a, int lda, int* ipiv)
{
    if (layout != Layout::ColMajor)
        return -1;

    // Query workspace size
    int    lwork = -1;
    double work_query;
    int    info = 0;

    dgetri_(&n, a, &lda, ipiv, &work_query, &lwork, &info);

    if (info != 0)
        return info;

    // Allocate workspace and perform inversion
    lwork = static_cast<int>(work_query);
    std::vector<double> work(lwork);
    dgetri_(&n, a, &lda, ipiv, work.data(), &lwork, &info);

    return info;
}

// Combined solve with Cholesky (factorize + solve)
inline int dposv(Layout layout, char uplo, int n, int nrhs, double* a, int lda, double* b, int ldb)
{
    if (layout != Layout::ColMajor)
        return -1;

    int info = 0;
    dposv_(&uplo, &n, &nrhs, a, &lda, b, &ldb, &info);
    return info;
}  // Combined solve with symmetric indefinite (factorize + solve)
inline int
dsysv(Layout layout, char uplo, int n, int nrhs, double* a, int lda, int* ipiv, double* b, int ldb)
{
    if (layout != Layout::ColMajor)
        return -1;

    // Query workspace size
    int    lwork = -1;
    double work_query;
    int    info = 0;

    dsysv_(&uplo, &n, &nrhs, a, &lda, ipiv, b, &ldb, &work_query, &lwork, &info);

    if (info != 0)
        return info;

    // Allocate workspace and perform solve
    lwork = static_cast<int>(work_query);
    std::vector<double> work(lwork);
    dsysv_(&uplo, &n, &nrhs, a, &lda, ipiv, b, &ldb, work.data(), &lwork, &info);
    return info;
}

// Combined solve with LU (factorize + solve) - general matrix
inline int dgesv(Layout layout, int n, int nrhs, double* a, int lda, int* ipiv, double* b, int ldb)
{
    if (layout != Layout::ColMajor)
        return -1;

    int info = 0;
    int m    = n;  // For square matrix
    // dgesv requires factorization first
    dgetrf_(&m, &n, a, &lda, ipiv, &info);
    if (info == 0)
    {
        char trans = 'N';
        dgetrs_(&trans, &n, &nrhs, a, &lda, ipiv, b, &ldb, &info);
    }
    return info;
}  // Define constants to match LAPACKE/CBLAS
constexpr Layout COL_MAJOR = Layout::ColMajor;
constexpr Layout ROW_MAJOR = Layout::RowMajor;

// =============================================================================
// BLAS Wrappers (using pure Fortran BLAS instead of CBLAS)
// =============================================================================

// BLAS functions are declared by Eigen with int return type on some platforms
// We don't need to declare them - just use them directly

// Transpose enum to match CBLAS
enum class Transpose
{
    NoTrans   = 111,
    Trans     = 112,
    ConjTrans = 113
};

constexpr Transpose CblasNoTrans   = Transpose::NoTrans;
constexpr Transpose CblasTrans     = Transpose::Trans;
constexpr Transpose CblasConjTrans = Transpose::ConjTrans;
constexpr Layout    CblasColMajor  = Layout::ColMajor;
constexpr Layout    CblasRowMajor  = Layout::RowMajor;

// Helper to convert Transpose enum to char
inline char transpose_to_char(Transpose trans)
{
    switch (trans)
    {
        case Transpose::NoTrans:
            return 'N';
        case Transpose::Trans:
            return 'T';
        case Transpose::ConjTrans:
            return 'C';
        default:
            return 'N';
    }
}

// Matrix-matrix multiply: C = alpha*op(A)*op(B) + beta*C
inline void dgemm(
    Layout        layout,
    Transpose     transa,
    Transpose     transb,
    int           m,
    int           n,
    int           k,
    double        alpha,
    const double* a,
    int           lda,
    const double* b,
    int           ldb,
    double        beta,
    double*       c,
    int           ldc
)
{
    if (layout != Layout::ColMajor)
    {
        // For row-major, swap and transpose
        // This is a simplified version - full implementation would need more work
        return;
    }

    char ta = transpose_to_char(transa);
    char tb = transpose_to_char(transb);
    dgemm_(
        &ta,
        &tb,
        &m,
        &n,
        &k,
        &alpha,
        const_cast<double*>(a),
        &lda,
        const_cast<double*>(b),
        &ldb,
        &beta,
        c,
        &ldc
    );
}

// Matrix-vector multiply: y = alpha*op(A)*x + beta*y
inline void dgemv(
    Layout        layout,
    Transpose     trans,
    int           m,
    int           n,
    double        alpha,
    const double* a,
    int           lda,
    const double* x,
    int           incx,
    double        beta,
    double*       y,
    int           incy
)
{
    if (layout != Layout::ColMajor)
    {
        return;
    }

    char t = transpose_to_char(trans);
    dgemv_(
        &t,
        &m,
        &n,
        &alpha,
        const_cast<double*>(a),
        &lda,
        const_cast<double*>(x),
        &incx,
        &beta,
        y,
        &incy
    );
}

// Vector copy: y = x
inline void dcopy(int n, const double* x, int incx, double* y, int incy)
{
    dcopy_(&n, const_cast<double*>(x), &incx, y, &incy);
}

// Vector addition: y = alpha*x + y
inline void daxpy(int n, double alpha, const double* x, int incx, double* y, int incy)
{
    daxpy_(&n, &alpha, const_cast<double*>(x), &incx, y, &incy);
}

}  // namespace LapackWrapper
