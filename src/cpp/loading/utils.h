/*!
 * Definition of utility functions
 * @author Sébastien Allgeyer
 * @date 5/3/21
 *
 */
#pragma once

// Template function replacing SQR macro
template <typename T>
constexpr T SQR(const T& x)
{
    return x * x;
}

// Mathematical constants
constexpr double PI  = 3.141592653589793238462643383279502884197169399375105820974;
constexpr double D2R = (PI / 180.0); /* deg to rad */
constexpr double R2D = (180.0 / PI); /* rad to deg */
void             calcDistanceBearing(
                float*  lat1,
                float*  lon1,
                float*  lat2,
                float*  lon2,
                double* dist,
                double* brng
            );
void ecef2pos(const double* r, double* pos);