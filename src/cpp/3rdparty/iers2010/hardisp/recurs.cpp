// #include "hardisp.hpp"
#include "iers2010.hpp"

/// @details The purpose of the function is to perform sine and cosine recursion
///          to fill in data x, of length n, for nf sines and cosines with
///          frequencies om.
///
/// @param[in]  x    Data provided from a file given as standard input from the
///                  MAIN  program HARDISP.F (Note 1). This array will change!
/// @param[in]  n    Length of the data file x
/// @param[in]  hc   Array containing alternating cosine and sine coefficients
/// @param[in]  nf   Number of sine and cosine terms
/// @param[in]  om   Sine and cosine frequencies (Note 2)
/// @param[out] scr  Scratch array of length 3 times nf which is returned as
///                  the recursion cr
/// @return          Always returns 0
///
/// @note
///   -# See the MAIN program HARDISP header comments for detailed information.
///   -# The frequencies are normalized so that the Nyquist frequency is pi.
///   -# Status: Canonical model
///
/// @version 19.08.2009
int iers2010::hisp::recurs(double *x, int n, const double *hc, int nf,
                           const double *om, double *scr) {
  //  Set up for start of recursion by computing harmonic values
  //+ at starting point and just before it
  double cos_omi, sin_omi;
  for (int i = 0; i < nf; i++) {
    cos_omi = std::cos(om[i]);
    sin_omi = std::sin(om[i]);
    scr[3 * i] = hc[2 * i];
    scr[3 * i + 1] = hc[2 * i] * cos_omi - hc[2 * i + 1] * sin_omi;
    scr[3 * i + 2] = 2e0 * cos_omi;
#ifdef DEBUG
    assert(3 * i + 2 < 3 * nf && 2 * i + 1 < 2 * nf);
#endif
  }

  // Do recursion over data
  double sc;
  for (int i = 0; i < n; i++) {
    x[i] = .0e0;
    // Then do recursive computation for each harmonic
    for (int j = 0; j < nf; j++) {
      sc = scr[3 * j];
      x[i] += sc;
      scr[3 * j] = scr[3 * j + 2] * sc - scr[3 * j + 1];
      scr[3 * j + 1] = sc;
#ifdef DEBUG
      assert(3 * j + 2 < 3 * nf);
#endif
    }
  }

  // Finished
  return 0;
}
