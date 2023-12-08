
#pragma once

#include "eigenIncluder.hpp"
#include "gTime.hpp"

namespace iers2010
{
/// @brief Equatorial radius of the Earth [m].
/// @see Table 1.1: IERS numerical standards, IERS 2010
constexpr const double Re = 6'378'136.6e0;

/// Compute the global total FCULa mapping function.
double fcul_a(double, double, double, double) noexcept;
/*
/// Computes the global total FCULb mapping function.
double fcul_b(double, double, double, double) noexcept;
*/
// Determine the total zenith delay following Mendes and Pavlis, 2004.
int fcul_zd_hpa(double, double, double, double, double, double &, double &, double &) noexcept;

/// @brief Compute tidal corrections of station displacements caused by lunar
/// and solar gravitational attraction. This is just the implementation, use the
/// generic template function instead.
int dehanttideinel_impl(
    double julian_centuries_tt, double fhr_ut,
    const Eigen::Matrix<double, 3, 1> &xsun,
    const Eigen::Matrix<double, 3, 1> &xmon,
    const std::vector<Eigen::Matrix<double, 3, 1>> &xsta_vec,
    std::vector<Eigen::Matrix<double, 3, 1>> &xcor_vec) noexcept;

namespace hisp {

// Parameters below set the buffer size for computing the tides recursively

// nl: the number of harmonics used in the prediction
constexpr int nl{600};

// nt: this must also be set in the subroutine admint
constexpr int nt{342};

// ntin: the number of harmonics read in
constexpr int ntin{11};

double eval(double, int, const double *, const double *, const double *);

int recurs(double *, int, const double *, int, const double *, double *);

int shells(double *, int *, int) noexcept;

int spline(int, const double *, const double *, double *, double *);

// int tdfrph(const int *, dso::datetime<dso::seconds>, double &, double &);
int tdfrph(const int *, GTime, double &, double &);

// int admint(const double *, const double *, dso::datetime<dso::seconds>,
int admint(const double *, const double *, GTime,
           double *, double *, double *, int, int &);

int read_hardisp_args(double tamp[3][ntin], double tph[3][ntin],
                      const char *filename = nullptr);

int hardisp_impl(int, double, double tamp[3][ntin], double tph[3][ntin],
                //  dso::datetime<dso::seconds> epoch, double *du, double *ds,
                 GTime epoch, double *du, double *ds,
                 double *dw);

} // hisp

};
