/*!
 * Definition of utility functions
 * @author Sébastien Allgeyer
 * @date 5/3/21
 *
 */

#pragma once

double rad_to_deg(double rad);

double deg_to_rad(double rad);

void calcDistanceBearing(float *lat1, float *lon1, float * lat2, float *lon2, double *dist, double *brng);

void ecef2pos(const double *r, double* pos);
