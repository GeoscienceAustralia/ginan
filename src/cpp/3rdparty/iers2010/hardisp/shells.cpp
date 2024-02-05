// #include "hardisp.hpp"
#include "iers2010.hpp"
#include <algorithm>
#include <vector>

/// @details This function sorts an array x, of length n, sorting upward,
///          and returns an array k which may be used to key another array
///          to the sorted pattern (i.e., if we had an array f to which x
///          corresponded before sorting, then after calling SHELLS,
///          f(k(1)) will be the element of f corresponding to the
///          smallest x, f(k(2)) the next smallest, and so on).
///
/// @param[in]  x  Array to be sorted (Note 1)
/// @param[in]  n  Length of input array \c x.
/// @param[out] k  Sorted array that may be used to key another array.
/// @return        Zero on sucess, 1 for failure.
///
/// @note
///     -# See the subroutine ADMINT header comments for detailed information.
///     -# The original fortran routine used a shell sorting algorithm (hence
///        the name). This function does tha same as the original, but via a
///        completely different way.
///     -# Status: Canonical model
///
/// @version 2009 August 19
int iers2010::hisp::shells(double *x, int *k, int n) noexcept {
  std::vector<std::pair<double, int>> vec;
  vec.reserve(n);
  for (int i = 0; i < n; i++)
    vec.emplace_back(std::make_pair(x[i], i));

  std::sort(vec.begin(), vec.end(),
            [](auto &l, auto &r) { return l.first < r.first; });

  for (int i = 0; i < n; i++) {
    x[i] = vec[i].first;
    k[i] = vec[i].second;
  }

  return 0;
}
