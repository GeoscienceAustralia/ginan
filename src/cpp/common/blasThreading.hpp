#pragma once

namespace BlasThreading
{
#ifdef GINAN_USE_OPENBLAS
extern "C"
{
    int  openblas_get_num_threads();
    void openblas_set_num_threads(int num_threads);
}
#endif

inline bool openblasThreadControlAvailable()
{
#ifdef GINAN_USE_OPENBLAS
    return true;
#else
    return false;
#endif
}

/**
 * Temporarily limits OpenBLAS worker threads.
 *
 * OpenBLAS builds that use pthreads warn, and can hang, if they are called from
 * an active OpenMP parallel region:
 *
 *   OpenBLAS Warning : Detect OpenMP Loop and this application may hang.
 *
 * Ginan keeps some high-level chunk loops parallel with OpenMP. For those loops
 * we want each chunk's BLAS/LAPACK calls to run single-threaded instead of
 * nesting another thread pool inside every OpenMP worker. This RAII guard sets
 * OpenBLAS to the requested thread count for the current scope and restores the
 * previous OpenBLAS setting when it leaves the scope.
 *
 * Passing numThreads <= 0 disables the guard. If the target is linked against
 * a non-OpenBLAS BLAS/LAPACK library, the guard is compiled as a no-op.
 */
class ScopedOpenBlasThreadLimit
{
public:
    explicit ScopedOpenBlasThreadLimit(int numThreads)
    {
#ifdef GINAN_USE_OPENBLAS
        if (numThreads <= 0 || openblasThreadControlAvailable() == false)
        {
            return;
        }

        previousNumThreads = openblas_get_num_threads();
        if (previousNumThreads != numThreads)
        {
            openblas_set_num_threads(numThreads);
            changed = true;
        }
#endif
    }

    ~ScopedOpenBlasThreadLimit()
    {
#ifdef GINAN_USE_OPENBLAS
        if (changed && previousNumThreads > 0 && openblasThreadControlAvailable())
        {
            openblas_set_num_threads(previousNumThreads);
        }
#endif
    }

    ScopedOpenBlasThreadLimit(const ScopedOpenBlasThreadLimit&)            = delete;
    ScopedOpenBlasThreadLimit& operator=(const ScopedOpenBlasThreadLimit&) = delete;

#ifdef GINAN_USE_OPENBLAS
private:
    int  previousNumThreads = 0;
    bool changed            = false;
#endif
};
}  // namespace BlasThreading
