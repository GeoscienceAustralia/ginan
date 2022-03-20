/*!
 *
 * @author Sébastien Allgeyer
 * @date 5/3/21
 *
 */
#include <cmath>

#include "utils.h"

using namespace std;

#define RE_WGS84    6378137.0           /* earth semimajor axis (WGS84) (m) */
#define FE_WGS84    (1.0/298.257223563) /* earth flattening (WGS84) */

const double PI = atan(1.0)*4;


/* transform ecef to geodetic postion ------------------------------------------
* Copied from RTKLIB 
* transform ecef position to geodetic position
* args   : double *r        I   ecef position {x,y,z} (m)
*          double *pos      O   geodetic position {lat,lon,h} (rad,m)
* return : none
* notes  : WGS84, ellipsoidal height
*-----------------------------------------------------------------------------*/
void ecef2pos(const double *r, double *pos)
{
	double e2=FE_WGS84*(2.0-FE_WGS84);
	double r2=r[0]*r[0] + r[1]*r[1];
	double z;
	double zk;
	double v=RE_WGS84;
	double sinp;

	for (z=r[2],zk=0.0;fabs(z-zk)>=1E-4;)
	{
		zk=z;
		sinp=z/sqrt(r2+z*z);
		v=RE_WGS84/sqrt(1.0-e2*sinp*sinp);
		z=r[2]+v*e2*sinp;
	}
	pos[0]=r2>1E-12?atan(z/sqrt(r2)):(r[2]>0.0?PI/2.0:-PI/2.0);
	pos[1]=r2>1E-12?atan2(r[1],r[0]):0.0;
	pos[2]=sqrt(r2+z*z)-v;
}


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
