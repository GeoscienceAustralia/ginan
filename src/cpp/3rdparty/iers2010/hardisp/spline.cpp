// #include "hardisp.hpp"
#include "iers2010.hpp"

inline double q(double u1, double x1, double u2, double x2) noexcept {
  return (u1 / (x1 * x1) - u2 / (x2 * x2)) / (1e0 / x1 - 1e0 / x2);
}

/// @details The purpose of this function is to find an array s for the spline
///          interpolator function EVAL.
///
/// @param[in]  nn   Number of data points supplied, which may be
///                  negative (Note 1)
/// @param[in]  x    Array containing x-coordinates where function
///                  is sampled (Note 2)
/// @param[in]  u    Array containing sample values that are to be
///                  interpolated
/// @param[in]  a    Working space array of dimension at least nn
/// @param[out] s    Output array of 2nd derivative at sample points
/// @return          Always zero
///
/// @note
///   -# If the user wishes to force the derivatives at the ends of the series
///      to assume specified values, he or she should put du(1)/dx and du(n)/dx
///      in the variables s1 and s2 and call the subroutine with nn = -(number
///      of terms in the series).  Normally a parabola is fitted through the
///      1st and last 3 points to find the slopes.  If less than 4 points are
///      supplied, straight lines are fitted.
///   -# The sequence xx(1), xx(2), ... xx(nn) must be strictly increasing.
///   -# Status: Canonical model
///
/// @version 26.08.2009
int iers2010::hisp::spline(int nn, const double *x, const double *u, double *s,
                           double *a) {
  const int n = std::abs(nn);

  if (n <= 3) {
    // series too short for cubic spline - use straight lines.
    for (int i = 0; i < n; i++)
      s[i] = 0e0;
    return 0;
  }

  double q1 = q(u[1] - u[0], x[1] - x[0], u[2] - u[0], x[2] - x[0]);
  double qn = q(u[n - 2] - u[n - 1], x[n - 2] - x[n - 1], u[n - 3] - u[n - 1],
                x[n - 3] - x[n - 1]);

  if (nn <= 0) {
    q1 = s[0];
    qn = s[1];
  }

  s[0] = 6e0 * ((u[1] - u[0]) / (x[1] - x[0]) - q1);
  int n1{n - 1};

  for (int i = 1; i < n1; i++) {
    s[i] = (u[i - 1] / (x[i] - x[i - 1]) -
            u[i] * (1e0 / (x[i] - x[i - 1]) + 1e0 / (x[i + 1] - x[i])) +
            u[i + 1] / (x[i + 1] - x[i])) *
           6e0;
  }

  s[n - 1] = 6e0 * (qn + (u[n1 - 1] - u[n - 1]) / (x[n - 1] - x[n1 - 1]));
  a[0] = 2e0 * (x[1] - x[0]);
  a[1] = 1.5e0 * (x[1] - x[0]) + 2e0 * (x[2] - x[1]);
  s[1] = s[1] - 0.5e0 * s[0];

  double c;
  for (int i = 2; i < n1; i++) {
    c = (x[i] - x[i - 1]) / a[i - 1];
    a[i] = 2e0 * (x[i + 1] - x[i - 1]) - c * (x[i] - x[i - 1]);
    s[i] -= (c * s[i - 1]);
  }

  c = (x[n - 1] - x[n1 - 1]) / a[n1 - 1];
  a[n - 1] = (2e0 - c) * (x[n - 1] - x[n1 - 1]);
  s[n - 1] -= (c * s[n1 - 1]);

  // Back substitute
  s[n - 1] /= a[n - 1];

  int i;
  for (int j = 1; j <= n1; j++) {
    i = n - j - 1;
    s[i] = (s[i] - (x[i + 1] - x[i]) * s[i + 1]) / a[i];
  }

  // Finished
  return 0;
}
