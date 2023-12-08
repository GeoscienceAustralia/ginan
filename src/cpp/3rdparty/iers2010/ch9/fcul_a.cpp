#include "iers2010.hpp"
#ifdef USE_EXTERNAL_CONSTS
#include "iersc.hpp"
#endif

/// @details  This function computes the global total FCULa mapping function
///           (Mendes et al. 2002). It is dependent on latitude, height, and
///           surface temperature.
///           This function is a translation/wrapper for the fortran FCUL_A
///           subroutine, found here :
///           http://maia.usno.navy.mil/conv2010/software.html
///
/// @param[in]  dlat  Latitude given in degrees (North Latitude)
/// @param[in]  dhgt  Height given in meters (mean sea level)
/// @param[in]  t     Surface temperature given in Kelvin
/// @param[in]  elev  Elevation angle in degrees
/// @return           Mapping function to scale total delay (Note 1)
///
/// @note
///    -# These coefficients are based on a LS adjustment of 87766 (cleaned)
///    set of traces, based on Ciddor routines to compute refractivity,
///    according to IUGG recommendations (1999).
///    -# Status: Class 1 model
///
/// @version 13.08.2009
///
/// @cite iers2010
/// Mendes, V.B., G. Prates, E.C. Pavlis, D.E. Pavlis,
/// and R.B. Langley (2002). "Improved mapping functions for
/// atmospheric refraction correction in SLR", Geophysical
/// Res. Lett., 29(10), 1414, doi:10.1029/2001GL014394, 2002
double iers2010::fcul_a(double dlat, double dhgt, double t,
                        double elev) noexcept {
#ifdef USE_EXTERNAL_CONSTS
  constexpr double PI(iers2010::DPI);
#else
  constexpr double PI(3.14159265358979323846e0);
#endif

  // Convert elevation angle to radians
  const double epsilon = elev * (PI / 180e0);
  const double sine = std::sin(epsilon);
  // Convert temperature to Celsius
  const double t_c = t - 273.15e0;
  const double cosphi = std::cos(dlat * (PI / 180e0));

  // Define coefficients used in the model
  constexpr double a10 = 0.121008e-02;
  constexpr double a11 = 0.17295e-05;
  constexpr double a12 = 0.3191e-04;
  constexpr double a13 = -0.18478e-07;

  constexpr double a20 = 0.304965e-02;
  constexpr double a21 = 0.2346e-05;
  constexpr double a22 = -0.1035e-03;
  constexpr double a23 = -0.1856e-07;

  constexpr double a30 = 0.68777e-01;
  constexpr double a31 = 0.1972e-04;
  constexpr double a32 = -0.3458e-02;
  constexpr double a33 = 0.1060e-06;

  // a, b, and c in Marini continued fraction (Eq. 5)
  double a1{a10 + a11 * t_c + a12 * cosphi + a13 * dhgt};
  double a2{a20 + a21 * t_c + a22 * cosphi + a23 * dhgt};
  double a3{a30 + a31 * t_c + a32 * cosphi + a33 * dhgt};

  // numerator in continued fraction
  double map_zen = (1e0 + a1 / (1e0 + a2 / (1e0 + a3)));

  // result
  return map_zen / (sine + a1 / (sine + a2 / (sine + a3)));
}
