/*!
 *
 * @author Sébastien Allgeyer
 * @date 5/3/21
 *
 */
#include <cmath>

#include "utils.h"

using namespace std;

const double PI = atan(1.0)*4;


double rad_to_deg(double rad)
{
	return rad * 180/PI;
};


double deg_to_rad(double rad)
{
	return rad / 180*PI;
};

void calcDistanceBearing(float *lat1, float *lon1, float * lat2, float *lon2, double *dist, double *brng)
{
	double lat1_r = (double)  (*lat1 * PI/180.0);
	double lat2_r = (double)  (*lat2 * PI/180.0);
	double lon1_r = (double)  (*lon1 * PI/180.0);
	double lon2_r = (double)  (*lon2 * PI/180.0);
	double deltalon = lon2_r - lon1_r;
	double deltalat = lat2_r - lat1_r;
	double a = sin(deltalat/2) * sin(deltalat/2) +
			   cos(lat1_r) * cos(lat2_r) *
			   sin(deltalon/2) * sin(deltalon/2);
	double y = sin(deltalon) * cos(lat2_r);
	double x = cos(lat1_r)*sin(lat2_r) -
			   sin(lat1_r)*cos(lat2_r)*cos(deltalon);

	*brng = atan2(y, x);
	*dist = 2 * atan2(sqrt(a), sqrt(1-a));
	if (*dist != *dist) *dist = M_PI;
}
