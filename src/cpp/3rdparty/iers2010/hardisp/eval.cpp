// #include "hardisp.hpp"
#include "iers2010.hpp"
#include <algorithm>
#ifdef DEBUG
#include <cassert>
#endif

/// @details This function performs cubic spline interpolation of a given
///          function sampled at unequally spaced intervals. The function spline
///          needs to be called beforehand to set up the array s.
///
/// @param[in]  y    The coordinate at which a function value is desired (Note
/// 1)
/// @param[in]  nn   Number of samples of the original function
/// @param[in]  x    Array containing sample coordinates x(1),x(2),...x(nn)
///                  (Note 2)
/// @param[in]  s    Array containing the 2nd derivatives at the sample points
///                  (Note 3)
/// @param[in]  u    Array containing samples of a function at the coordinates
///                  x(1),x(2),...x(nn)
/// @return          The interpolated value of the function at y.
///
/// @note
///     -# If y falls outside the range (x(1),x(nn)), the value at the nearest
///        endpoint of the series is used.
///     -# The sequence x(1),x(2),...x(nn) must be strictly increasing.
///     -# This array is found by the function SPLINE, which must be called
///        once before beginning this interpolation.
///
/// @version 19.08.2009
double iers2010::hisp::eval(double y, int nn, const double *x, const double *u,
                            const double *s) {
  std::size_t k;
  int k1, k2;

  if (y <= x[0]) {
    return u[0];
  } else if (y >= x[nn - 1]) {
    return u[nn - 1];
  } else {
    k = std::distance(x, std::lower_bound(x, x + nn, y));
    k2 = k;
    k1 = k - 1;
#ifdef DEBUG
    assert((int)k > 0 && (int)k <= nn - 1);
#endif
  }

  //  Evaluate and then interpolate.
  //+ Note that this can fail if dk is ~0
  const double dy{x[k2] - y};
  const double dy1{y - x[k1]};
  const double dk{x[k2] - x[k1]};
  const double deli{1e0 / (6e0 * dk)};
  const double ff1{s[k1] * dy * dy * dy};
  const double ff2{s[k2] * dy1 * dy1 * dy1};
  const double f1{(ff1 + ff2) * deli};
  const double f2{dy1 * ((u[k2] / dk) - (s[k2] * dk) / 6e0)};
  const double f3{dy * ((u[k1] / dk) - (s[k1] * dk) / 6e0)};

  return f1 + f2 + f3;
}
