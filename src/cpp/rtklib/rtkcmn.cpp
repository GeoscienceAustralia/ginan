
// #pragma GCC optimize ("O0")

#include <stdarg.h>
#include <ctype.h>
#ifndef WIN32
#	include <dirent.h>
#	include <time.h>
#	include <sys/time.h>
#	include <sys/stat.h>
#	include <sys/types.h>
#endif

#include <boost/log/trivial.hpp>

#include <algorithm>
#include <iostream>
#include <fstream>
#include <string>
#include <array>

#include "eigenIncluder.hpp"
#include "streamTrace.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "satRefSys.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "sofam.hpp"
#include "sofa.hpp"
#include "enums.h"


/* test to exclude satellite
*/
int satexclude(
	SatSys&	Sat, 		///< Satellite
	E_Svh	svh)		///< SV Health
{
	int svh_i = svh;
	
	if (svh_i < 0)
		return 1; /* ephemeris unavailable */

	if (Sat.sys == +E_Sys::QZS)
		svh_i &= 0xFE; /* mask QZSS LEX health */

	if (svh_i > 0)
	{
//         trace(3,"unhealthy satellite: sat=%3d svh=%02X\n", Sat, svh);
		return 1;
	}
	return 0;
}





/** Transform geodetic postion to ecef
*/
void pos2ecef(
	const double*	pos,	///< geodetic position {lat,lon,h} (rad,m)
	Vector3d&		r)		///< ecef position {x,y,z} (m)
{
	double sinp	= sin(pos[0]);
	double cosp	= cos(pos[0]);
	double sinl	= sin(pos[1]);
	double cosl	= cos(pos[1]);
	double e2	= FE_WGS84 * (2 - FE_WGS84);
	double v	= RE_WGS84 / sqrt(1 - e2 * sinp * sinp);

	r[0] = (v+pos[2])			* cosp * cosl;
	r[1] = (v+pos[2])			* cosp * sinl;
	r[2] = (v*(1-e2)+pos[2])	* sinp;
}

/* transform ecef to geodetic postion ------------------------------------------
* transform ecef position to geodetic position
* args   : double *r        I   ecef position {x,y,z} (m)
*          double *pos      O   geodetic position {lat,lon,h} (rad,m)
* return : none
* notes  : WGS84, ellipsoidal height
*-----------------------------------------------------------------------------*/
void ecef2pos(
	const double *r, 
	double *pos)
{
	double e2	= FE_WGS84 * (2 - FE_WGS84);
	double r2	= dot(r,r,2);
	double v	= RE_WGS84;
	double z;
	double zk;
	double sinp;

	for (z = r[2], zk = 0; fabs(z-zk) >= 1E-4; )
	{
		zk		= z;
		sinp	= z / sqrt(r2 + SQR(z));
		v		= RE_WGS84 / sqrt(1 - e2 * SQR(sinp));
		z		= r[2] + v * e2 * sinp;
	}
	pos[0] = r2 > 1E-12 ? atan(z/sqrt(r2)) : (r[2] > 0 ? PI/2: -PI/2);
	pos[1] = r2 > 1E-12 ? atan2(r[1],r[0]) : 0;
	pos[2] = sqrt(r2 + SQR(z)) - v;
}

void ecef2pos(
	Vector3d& r, 
	double *pos)
{
	double e2	= FE_WGS84 * (2-FE_WGS84);
	double r2	= SQR(r(0)) +SQR(r(1));
	double v	= RE_WGS84;
	double z;
	double zk;
	double sinp;

	for (z = r[2], zk = 0; fabs(z-zk) >= 1E-4; )
	{
		zk		= z;
		sinp	= z / sqrt(r2 + SQR(z));
		v		= RE_WGS84 / sqrt(1 - e2 * SQR(sinp));
		z		= r[2] + v * e2 * sinp;
	}
	pos[0] = r2 > 1E-12 ? atan(z/sqrt(r2)) : (r[2] > 0 ? PI/2: -PI/2);
	pos[1] = r2 > 1E-12 ? atan2(r[1],r[0]) : 0;
	pos[2] = sqrt(r2 + SQR(z)) - v;
}

/* ecef to local coordinate transfromation matrix ------------------------------
* compute ecef to local coordinate transfromation matrix
* args   : double *pos      I   geodetic position {lat,lon} (rad)
*          double *E        O   ecef to local coord transformation matrix (3x3)
* return : none
* notes  : matirix stored by column-major order (fortran convention)
*-----------------------------------------------------------------------------*/
void xyz2enu(
	const double *pos,
	double *E)
{
	double sinp = sin(pos[0]);
	double cosp = cos(pos[0]);
	double sinl = sin(pos[1]);
	double cosl = cos(pos[1]);

	E[0] = -sinl;			E[3] = +cosl;			E[6] = 0;
	E[1] = -sinp * cosl;	E[4] = -sinp * sinl;	E[7] = +cosp;
	E[2] = +cosp * cosl;	E[5] = +cosp * sinl;	E[8] = +sinp;
}

/* transform ecef vector to local tangental coordinate -------------------------
* transform ecef vector to local tangental coordinate
* args   : double *pos      I   geodetic position {lat,lon} (rad)
*          double *r        I   vector in ecef coordinate {x,y,z}
*          double *e        O   vector in local tangental coordinate {e,n,u}
* return : none
*-----------------------------------------------------------------------------*/
void ecef2enu(
	const double *pos,
	const double *r, 
	double *e)
{
	double E[9];

	xyz2enu(pos,E);
	matmul("NN",3,1,3,1.0,E,r,0.0,e);
}

/* transform local vector to ecef coordinate -----------------------------------
* transform local tangental coordinate vector to ecef
* args   : double *pos      I   geodetic position {lat,lon} (rad)
*          double *e        I   vector in local tangental coordinate {e,n,u}
*          double *r        O   vector in ecef coordinate {x,y,z}
* return : none
*-----------------------------------------------------------------------------*/
void enu2ecef(const double *pos, const double *e, double *r)
{
	double E[9];

	xyz2enu(pos,E);
	matmul("TN",3,1,3,1.0,E,e,0.0,r);
}

/* coordinate rotation matrix ------------------------------------------------*/
//todo aaron delete
#define Rx(t,X) do {				\
	double sint = sin(t);			\
	(X)[0]=1;						\
	(X)[1]=(X)[2]=(X)[3]=(X)[6]=0; 	\
	(X)[4]=(X)[8]=cos(t);			\
	(X)[7]=+sin(t); 				\
	(X)[5]=-sint; 					\
} while (0)

#define Ry(t,X) do {				\
	double sint = sin(t);			\
	(X)[4]=1;						\
	(X)[1]=(X)[3]=(X)[5]=(X)[7]=0;	\
	(X)[0]=(X)[8]=cos(t); 			\
	(X)[2]=+sint;					\
	(X)[6]=-sint;					\
} while (0)

#define Rz(t,X) do {				\
	double sint = sin(t);			\
	(X)[8]=1;						\
	(X)[2]=(X)[5]=(X)[6]=(X)[7]=0;	\
	(X)[0]=(X)[4]=cos(t);			\
	(X)[3]=+sint;					\
	(X)[1]=-sint;					\
} while (0)



/* Elementary rotations
*/
Matrix3d R_x(	
    double    Angle)  ///< Angle in radian		
{
    const double C = cos(Angle);
    const double S = sin(Angle);
    Matrix3d U = Matrix3d::Zero();
    U(0,0) = 1.0;  U(0,1) = 0.0;  U(0,2) = 0.0;
    U(1,0) = 0.0;  U(1,1) =  +C;  U(1,2) =  +S;
    U(2,0) = 0.0;  U(2,1) =  -S;  U(2,2) =  +C;
    return U;
}

Matrix3d R_y(
    double    Angle)  ///< Angle in radian
{
    const double C = cos(Angle);
    const double S = sin(Angle);
    Matrix3d U = Matrix3d::Zero();
    U(0,0) =  +C;  U(0,1) = 0.0;  U(0,2) =  -S;
    U(1,0) = 0.0;  U(1,1) = 1.0;  U(1,2) = 0.0;
    U(2,0) =  +S;  U(2,1) = 0.0;  U(2,2) =  +C;
    return U;
}

Matrix3d R_z(
    double    Angle)  ///< Angle in radian
{
    const double C = cos(Angle);
    const double S = sin(Angle);
    Matrix3d U = Matrix3d::Zero();
    U(0,0) =  +C;  U(0,1) =  +S;  U(0,2) = 0.0;
    U(1,0) =  -S;  U(1,1) =  +C;  U(1,2) = 0.0;
    U(2,0) = 0.0;  U(2,1) = 0.0;  U(2,2) = 1.0;
    return U;
}

/* astronomical arguments: f={l,l',F,D,OMG} (rad) ----------------------------*/
void ast_args(double t, double *f)
{
	const double fc[][5]=
	{	
		/* coefficients for iau 1980 nutation */
		{ 134.96340251, 1717915923.2178,  31.8792,  0.051635, -0.00024470},
		{ 357.52910918,  129596581.0481,  -0.5532,  0.000136, -0.00001149},
		{  93.27209062, 1739527262.8478, -12.7512, -0.001037,  0.00000417},
		{ 297.85019547, 1602961601.2090,  -6.3706,  0.006593, -0.00003169},
		{ 125.04455501,   -6962890.2665,   7.4722,  0.007702, -0.00005939}
	};
	double tt[4];
	
	int i;
	for (tt[0] = t, i = 1; i < 4; i++)
		tt[i] = tt[i-1] * t;
	
	for (int i = 0; i < 5; i++)
	{
		f[i] = fc[i][0] * 3600;
		
		for (int j = 0; j < 4; j++)
			f[i] += fc[i][j+1] * tt[j];
		
		f[i] = fmod(f[i] * AS2R, 2 * PI);
	}
}
/* iau 1980 nutation ---------------------------------------------------------*/
void nut_iau1980(
	double			t, 
	const double*	f, 
	double&			dpsi,
	double&			deps)
{
	const double nut[106][10] =
	{
		{   0,   0,   0,   0,   1, -6798.4, -171996, -174.2, 92025,   8.9},
		{   0,   0,   2,  -2,   2,   182.6,  -13187,   -1.6,  5736,  -3.1},
		{   0,   0,   2,   0,   2,    13.7,   -2274,   -0.2,   977,  -0.5},
		{   0,   0,   0,   0,   2, -3399.2,    2062,    0.2,  -895,   0.5},
		{   0,  -1,   0,   0,   0,  -365.3,   -1426,    3.4,    54,  -0.1},
		{   1,   0,   0,   0,   0,    27.6,     712,    0.1,    -7,   0.0},
		{   0,   1,   2,  -2,   2,   121.7,    -517,    1.2,   224,  -0.6},
		{   0,   0,   2,   0,   1,    13.6,    -386,   -0.4,   200,   0.0},
		{   1,   0,   2,   0,   2,     9.1,    -301,    0.0,   129,  -0.1},
		{   0,  -1,   2,  -2,   2,   365.2,     217,   -0.5,   -95,   0.3},
		{  -1,   0,   0,   2,   0,    31.8,     158,    0.0,    -1,   0.0},
		{   0,   0,   2,  -2,   1,   177.8,     129,    0.1,   -70,   0.0},
		{  -1,   0,   2,   0,   2,    27.1,     123,    0.0,   -53,   0.0},
		{   1,   0,   0,   0,   1,    27.7,      63,    0.1,   -33,   0.0},
		{   0,   0,   0,   2,   0,    14.8,      63,    0.0,    -2,   0.0},
		{  -1,   0,   2,   2,   2,     9.6,     -59,    0.0,    26,   0.0},
		{  -1,   0,   0,   0,   1,   -27.4,     -58,   -0.1,    32,   0.0},
		{   1,   0,   2,   0,   1,     9.1,     -51,    0.0,    27,   0.0},
		{  -2,   0,   0,   2,   0,  -205.9,     -48,    0.0,     1,   0.0},
		{  -2,   0,   2,   0,   1,  1305.5,      46,    0.0,   -24,   0.0},
		{   0,   0,   2,   2,   2,     7.1,     -38,    0.0,    16,   0.0},
		{   2,   0,   2,   0,   2,     6.9,     -31,    0.0,    13,   0.0},
		{   2,   0,   0,   0,   0,    13.8,      29,    0.0,    -1,   0.0},
		{   1,   0,   2,  -2,   2,    23.9,      29,    0.0,   -12,   0.0},
		{   0,   0,   2,   0,   0,    13.6,      26,    0.0,    -1,   0.0},
		{   0,   0,   2,  -2,   0,   173.3,     -22,    0.0,     0,   0.0},
		{  -1,   0,   2,   0,   1,    27.0,      21,    0.0,   -10,   0.0},
		{   0,   2,   0,   0,   0,   182.6,      17,   -0.1,     0,   0.0},
		{   0,   2,   2,  -2,   2,    91.3,     -16,    0.1,     7,   0.0},
		{  -1,   0,   0,   2,   1,    32.0,      16,    0.0,    -8,   0.0},
		{   0,   1,   0,   0,   1,   386.0,     -15,    0.0,     9,   0.0},
		{   1,   0,   0,  -2,   1,   -31.7,     -13,    0.0,     7,   0.0},
		{   0,  -1,   0,   0,   1,  -346.6,     -12,    0.0,     6,   0.0},
		{   2,   0,  -2,   0,   0, -1095.2,      11,    0.0,     0,   0.0},
		{  -1,   0,   2,   2,   1,     9.5,     -10,    0.0,     5,   0.0},
		{   1,   0,   2,   2,   2,     5.6,      -8,    0.0,     3,   0.0},
		{   0,  -1,   2,   0,   2,    14.2,      -7,    0.0,     3,   0.0},
		{   0,   0,   2,   2,   1,     7.1,      -7,    0.0,     3,   0.0},
		{   1,   1,   0,  -2,   0,   -34.8,      -7,    0.0,     0,   0.0},
		{   0,   1,   2,   0,   2,    13.2,       7,    0.0,    -3,   0.0},
		{  -2,   0,   0,   2,   1,  -199.8,      -6,    0.0,     3,   0.0},
		{   0,   0,   0,   2,   1,    14.8,      -6,    0.0,     3,   0.0},
		{   2,   0,   2,  -2,   2,    12.8,       6,    0.0,    -3,   0.0},
		{   1,   0,   0,   2,   0,     9.6,       6,    0.0,     0,   0.0},
		{   1,   0,   2,  -2,   1,    23.9,       6,    0.0,    -3,   0.0},
		{   0,   0,   0,  -2,   1,   -14.7,      -5,    0.0,     3,   0.0},
		{   0,  -1,   2,  -2,   1,   346.6,      -5,    0.0,     3,   0.0},
		{   2,   0,   2,   0,   1,     6.9,      -5,    0.0,     3,   0.0},
		{   1,  -1,   0,   0,   0,    29.8,       5,    0.0,     0,   0.0},
		{   1,   0,   0,  -1,   0,   411.8,      -4,    0.0,     0,   0.0},
		{   0,   0,   0,   1,   0,    29.5,      -4,    0.0,     0,   0.0},
		{   0,   1,   0,  -2,   0,   -15.4,      -4,    0.0,     0,   0.0},
		{   1,   0,  -2,   0,   0,   -26.9,       4,    0.0,     0,   0.0},
		{   2,   0,   0,  -2,   1,   212.3,       4,    0.0,    -2,   0.0},
		{   0,   1,   2,  -2,   1,   119.6,       4,    0.0,    -2,   0.0},
		{   1,   1,   0,   0,   0,    25.6,      -3,    0.0,     0,   0.0},
		{   1,  -1,   0,  -1,   0, -3232.9,      -3,    0.0,     0,   0.0},
		{  -1,  -1,   2,   2,   2,     9.8,      -3,    0.0,     1,   0.0},
		{   0,  -1,   2,   2,   2,     7.2,      -3,    0.0,     1,   0.0},
		{   1,  -1,   2,   0,   2,     9.4,      -3,    0.0,     1,   0.0},
		{   3,   0,   2,   0,   2,     5.5,      -3,    0.0,     1,   0.0},
		{  -2,   0,   2,   0,   2,  1615.7,      -3,    0.0,     1,   0.0},
		{   1,   0,   2,   0,   0,     9.1,       3,    0.0,     0,   0.0},
		{  -1,   0,   2,   4,   2,     5.8,      -2,    0.0,     1,   0.0},
		{   1,   0,   0,   0,   2,    27.8,      -2,    0.0,     1,   0.0},
		{  -1,   0,   2,  -2,   1,   -32.6,      -2,    0.0,     1,   0.0},
		{   0,  -2,   2,  -2,   1,  6786.3,      -2,    0.0,     1,   0.0},
		{  -2,   0,   0,   0,   1,   -13.7,      -2,    0.0,     1,   0.0},
		{   2,   0,   0,   0,   1,    13.8,       2,    0.0,    -1,   0.0},
		{   3,   0,   0,   0,   0,     9.2,       2,    0.0,     0,   0.0},
		{   1,   1,   2,   0,   2,     8.9,       2,    0.0,    -1,   0.0},
		{   0,   0,   2,   1,   2,     9.3,       2,    0.0,    -1,   0.0},
		{   1,   0,   0,   2,   1,     9.6,      -1,    0.0,     0,   0.0},
		{   1,   0,   2,   2,   1,     5.6,      -1,    0.0,     1,   0.0},
		{   1,   1,   0,  -2,   1,   -34.7,      -1,    0.0,     0,   0.0},
		{   0,   1,   0,   2,   0,    14.2,      -1,    0.0,     0,   0.0},
		{   0,   1,   2,  -2,   0,   117.5,      -1,    0.0,     0,   0.0},
		{   0,   1,  -2,   2,   0,  -329.8,      -1,    0.0,     0,   0.0},
		{   1,   0,  -2,   2,   0,    23.8,      -1,    0.0,     0,   0.0},
		{   1,   0,  -2,  -2,   0,    -9.5,      -1,    0.0,     0,   0.0},
		{   1,   0,   2,  -2,   0,    32.8,      -1,    0.0,     0,   0.0},
		{   1,   0,   0,  -4,   0,   -10.1,      -1,    0.0,     0,   0.0},
		{   2,   0,   0,  -4,   0,   -15.9,      -1,    0.0,     0,   0.0},
		{   0,   0,   2,   4,   2,     4.8,      -1,    0.0,     0,   0.0},
		{   0,   0,   2,  -1,   2,    25.4,      -1,    0.0,     0,   0.0},
		{  -2,   0,   2,   4,   2,     7.3,      -1,    0.0,     1,   0.0},
		{   2,   0,   2,   2,   2,     4.7,      -1,    0.0,     0,   0.0},
		{   0,  -1,   2,   0,   1,    14.2,      -1,    0.0,     0,   0.0},
		{   0,   0,  -2,   0,   1,   -13.6,      -1,    0.0,     0,   0.0},
		{   0,   0,   4,  -2,   2,    12.7,       1,    0.0,     0,   0.0},
		{   0,   1,   0,   0,   2,   409.2,       1,    0.0,     0,   0.0},
		{   1,   1,   2,  -2,   2,    22.5,       1,    0.0,    -1,   0.0},
		{   3,   0,   2,  -2,   2,     8.7,       1,    0.0,     0,   0.0},
		{  -2,   0,   2,   2,   2,    14.6,       1,    0.0,    -1,   0.0},
		{  -1,   0,   0,   0,   2,   -27.3,       1,    0.0,    -1,   0.0},
		{   0,   0,  -2,   2,   1,  -169.0,       1,    0.0,     0,   0.0},
		{   0,   1,   2,   0,   1,    13.1,       1,    0.0,     0,   0.0},
		{  -1,   0,   4,   0,   2,     9.1,       1,    0.0,     0,   0.0},
		{   2,   1,   0,  -2,   0,   131.7,       1,    0.0,     0,   0.0},
		{   2,   0,   0,   2,   0,     7.1,       1,    0.0,     0,   0.0},
		{   2,   0,   2,  -2,   1,    12.8,       1,    0.0,    -1,   0.0},
		{   2,   0,  -2,   0,   1,  -943.2,       1,    0.0,     0,   0.0},
		{   1,  -1,   0,  -2,   0,   -29.3,       1,    0.0,     0,   0.0},
		{  -1,   0,   0,   1,   1,  -388.3,       1,    0.0,     0,   0.0},
		{  -1,  -1,   0,   2,   1,    35.0,       1,    0.0,     0,   0.0},
		{   0,   1,   0,   1,   0,    27.3,       1,    0.0,     0,   0.0}
	};
	
	dpsi = 0;
	deps = 0;

	for (int i = 0; i < 106; i++)
	{
		double ang = 0;
		for (int j = 0; j < 5; j++)
			ang += nut[i][j] * f[j];
		
		dpsi += (nut[i][6] + nut[i][7] * t) * sin(ang);
		deps += (nut[i][8] + nut[i][9] * t) * cos(ang);
	}
	
	dpsi *= 1E-4 * AS2R; /* 0.1 mas -> rad */
	deps *= 1E-4 * AS2R;
}

/** eci to ecef transformation matrix
*/
void eci2ecef(
	const GTime		tutc,		///< time in utc
	ERPValues&		erpv,		///< erp values {xp,yp,ut1_utc,lod} (rad,rad,s,s/d)
	Matrix3d&		U,			///< eci to ecef transformation matrix (3 x 3)
	double*			gmst_ptr,	///< greenwich mean sidereal time (rad)
	Matrix3d*		dU_ptr)		///< eci to ecef transformation matrix (3 x 3)
{
	const double ep2000[] = {2000, 1, 1, 12, 0, 0};

//	trace(4,"%s: tutc=%s\n", __FUNCTION__, tutc.to_string(0).c_str());

	/* terrestrial time */
	GTime	tutc_	= tutc;
	GTime	tgps	= utc2gpst(tutc_);
	double	t		= (tgps - epoch2time(ep2000) + 19 + 32.184) / 86400 / 36525;	//measured in centuries
	double	t2 = t	* t; 
	double	t3 = t2	* t;

	/* astronomical arguments */
	double f[5];
	ast_args(t, f);

	/* iau 1976 precession */ //Yang comments: needs updates
	double ze	= (				+ 2306.2181	* t + 0.30188 * t2 + 0.017998 * t3) * AS2R;
	double th	= (				+ 2004.3109	* t - 0.42665 * t2 - 0.041833 * t3) * AS2R;
	double z	= (				+ 2306.2181	* t + 1.09468 * t2 + 0.018203 * t3) * AS2R;
	double eps	= (84381.448	-   46.8150	* t	- 0.00059 * t2 + 0.001813 * t3) * AS2R;
	
	Matrix3d R1;
	Matrix3d R2;
	Matrix3d R3;
	
	Rz(-z,	R1.data());
	Ry(th,	R2.data());
	Rz(-ze,	R3.data());
	
	Matrix3d P = R1 * R2 * R3;		// P = Rz(-z) * Ry(th) * Rz(-ze)
	// std::cout << "Precession matrix: " << std::endl << std::setw(14) << P << std::endl;

	/* iau 1980 nutation */  //Yang comments: needs updates
	double dpsi;
	double deps;
	nut_iau1980(t, f, dpsi, deps);
	
	Rx(-eps-deps,	R1.data());
	Rz(-dpsi,		R2.data());
	Rx(eps,			R3.data());
	
	Matrix3d N = R1 * R2 * R3;		// N = Rx(-eps) * Rz(-dspi) * Rx(eps)
	// std::cout << "Nutation matrix: " << std::endl << std::setw(14) << N << std::endl;

	/* greenwich aparent sidereal time (rad) */
	double gmst_	= utc2gmst(tutc_, erpv.ut1_utc);
	double gast		= gmst_ + dpsi * cos(eps)
					+ (   0.00264	* sin(f[4]) 
						+ 0.000063	* sin(2 * f[4])) * AS2R;

	/* eci to ecef transformation matrix */
	Ry(-erpv.xp,	R1.data());
	Rx(-erpv.yp,	R2.data());
	Rz(gast,		R3.data());

	Matrix3d theta	= R3;
	// std::cout << "Earth rotation matrix: " << std::endl << std::setw(14) << theta << std::endl;
	Matrix3d phi		= R1 * R2;
	// std::cout << "Polar motion matrix: " << std::endl << std::setw(14) << phi << std::endl;
	
	/* eci to ecef transformation matrix */
	U = phi * theta * N * P;				

	Matrix3d S = Matrix3d::Zero();
	S(0, 1) = +1; 
	S(1, 0) = -1;              			 // Derivative of Earth rotation
	
	// double Omega  = 7292115.8553e-11+4.3e-15*( (MJD_UTC-mjdJ2000)/36525 ); % [rad/s]
	
	if (dU_ptr)
	{
		double		Omega	= OMGE - 0.843994809 * 1e-9 * erpv.lod;  // IERS
		Matrix3d	dTheta	= Omega * S * theta;						// matrix [1/s]
		
		*dU_ptr		= phi * dTheta * N * P;           // Derivative [1/s]
	}
	
	if (gmst_ptr) 
		*gmst_ptr	= gmst_;
}


/** eci to ecef transformation matrix using the sofa library
*/
void eci2ecef_sofa(
	const double	mjdUTC, ///< UTC, modified Julian date 
	IERS&			iers,///< IERS instance
	Matrix3d&		U,		///< eci to ecef transformation matrix (3 x 3)
	Matrix3d&		dU)		///< eci to ecef transformation matrix (3 x 3)
{
	double mjdTT	= mjdUTC + iers.TT_UTC	() / 86400;
	double mjdUT1	= mjdUTC + iers.UT1_UTC	() / 86400;

	/* Form bias-precession-nutation matrix */
	double arrNPB[3][3] = {};
	iauPnm06a(JD2MJD, mjdTT, arrNPB);
	
	/* Form Earth rotation matrix */
	double arrTheta[3][3] = {1,0,0,0,1,0,0,0,1};
	double gst = iauGst06(JD2MJD, mjdUT1, JD2MJD, mjdTT, arrNPB);
	iauRz(gst, arrTheta);
	
	/* Polar motion matrix (TIRS->ITRS, IERS 2003) */
	double xp	= iers.xp;
	double yp	= iers.yp;
	double lod	= iers.lod;
	
	double arrPi[3][3] = {};
	iauPom00(xp, yp, iauSp00(JD2MJD, mjdTT), arrPi);

	/* ICRS to ITRS transformation matrix and derivative */	
	Matrix3d matS = Matrix3d::Zero();
	matS(0, 1) = +1;
	matS(1, 0) = -1;       //Derivative of Earth rotation
	
	double Omega	= OMGE - 0.843994809*1e-9 * lod;  //IERS, [rad/s]
	double Omega2	= 7292115.8553e-11 + 4.3e-15 * ( (mjdUTC - mjdJ2000) / 36525 ); // [rad/s]
	
	auto matNPB		= Map<Matrix<double,3,3,Eigen::RowMajor>>(&arrNPB	[0][0]);
	auto matTheta	= Map<Matrix<double,3,3,Eigen::RowMajor>>(&arrTheta	[0][0]);
	auto matPi		= Map<Matrix<double,3,3,Eigen::RowMajor>>(&arrPi	[0][0]);
	
	Matrix3d matdTheta = Omega * matS * matTheta;   // matrix [1/s]

	U	= matPi * matTheta	* matNPB;
	dU	= matPi * matdTheta	* matNPB;
}

/** vector transformed from eci to ecef using the sofa library
*/
void eci2ecef_sofa(
	const double	mjdUTC,		///< UTC, modified Julian date
	IERS&			iers,		///< IERS instance	
	Vector3d&		rSat_eci,	///< satellite state vector in eci
	Vector3d&		vSat_eci,	///< satellite state vector in eci
	Vector3d&		rSat_ecef,	///< satellite state vector in ecef
	Vector3d&		vSat_ecef)	///< satellite state vector in ecef
{
	Matrix3d  ECI2ECEF;
	Matrix3d dECI2ECEF;
	eci2ecef_sofa(mjdUTC, iers, ECI2ECEF, dECI2ECEF);

	rSat_ecef	=  ECI2ECEF * rSat_eci;
	vSat_ecef	=  ECI2ECEF * vSat_eci
				+ dECI2ECEF * rSat_eci;
}

/** vector transformed from ecef to eci using the sofa library
*/
void ecef2eci_sofa(
	const double	mjdUTC,		///< UTC, modified Julian date
	IERS&			iers,		///< IERS instance	
	Vector3d&		rSat_ecef,	///< satellite state vector in ecef
	Vector3d&		vSat_ecef,	///< satellite state vector in ecef
	Vector3d&		rSat_eci,	///< satellite state vector in eci
	Vector3d&		vSat_eci)	///< satellite state vector in eci
{
	Matrix3d  ECI2ECEF;
	Matrix3d dECI2ECEF;
	eci2ecef_sofa(mjdUTC, iers, ECI2ECEF, dECI2ECEF);

	rSat_eci	=  ECI2ECEF.transpose() * rSat_ecef;
	vSat_eci	=  ECI2ECEF.transpose() * vSat_ecef 
				+ dECI2ECEF.transpose() * rSat_ecef;
}

/* read blq record -----------------------------------------------------------*/
int readblqrecord(FILE *fp, double *otlDisplacement)
{
	double v[11];
	char buff[256];
	int i,n=0;

	while (fgets(buff,sizeof(buff),fp))
	{
		if (!strncmp(buff,"$$",2))
			continue;
		
		if (sscanf(buff,"%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", v,v+1,v+2,v+3,v+4,v+5,v+6,v+7,v+8,v+9,v+10)<11)
			continue;
		
		for (i=0;i<11;i++)
			otlDisplacement[n+i*6]=v[i];
		
		if (++n==6)
			return 1;
	}
	return 0;
}

/* read blq ocean tide loading parameters --------------------------------------
* read blq ocean tide loading parameters
* args   : char   *file       I   BLQ ocean tide loading parameter file
*          char   *sta        I   station name
*          double *odisp      O   ocean tide loading parameters
* return : status (1:ok,0:file open error)
*-----------------------------------------------------------------------------*/
int readblq(
	string file,
	const char *sta, 
	double *otlDisplacement)
{
	FILE *fp;
	char buff[256],staname[32]="",name[32],*p;

	/* station name to upper case */
	sscanf(sta,"%16s",staname);
	for (p=staname;(*p=(char)toupper((int)(*p)));p++) ;

	if (!(fp=fopen(file.c_str(),"r")))
	{
//         trace(2,"blq file open error: file=%s\n",file);
		return 0;
	}
	
	while (fgets(buff,sizeof(buff),fp))
	{
		if (!strncmp(buff,"$$",2)||strlen(buff)<2)
			continue;

		if (sscanf(buff+2,"%16s",name)<1)
			continue;
		
		for (p=name;(*p=(char)toupper((int)(*p)));p++)
			;
		
		if (strcmp(name,staname))
			continue;

		/* read blq record */
		if (readblqrecord(fp,otlDisplacement))
		{
			fclose(fp);
			return 1;
		}
	}
	fclose(fp);
//     trace(2,"no otl parameters: sta=%s file=%s\n",sta,file);
	return 0;
}


void updatenav(
	Obs& obs)
{
	int sys = obs.Sat.sys;
	if (obs.satNav_ptr == nullptr)
	{
		return;
	}

	if		( (sys == +E_Sys::GLO)
			&&(obs.satNav_ptr->geph_ptr != nullptr))
	{
		obs.satNav_ptr->lamMap[F1]		= CLIGHT / (FREQ1_GLO + DFRQ1_GLO * obs.satNav_ptr->geph_ptr->frq);
		obs.satNav_ptr->lamMap[F2]		= CLIGHT / (FREQ2_GLO + DFRQ2_GLO * obs.satNav_ptr->geph_ptr->frq);
		obs.satNav_ptr->lamMap[F5]		= CLIGHT / (FREQ3_GLO);
	}
	else if	(sys == +E_Sys::BDS)
	{
		obs.satNav_ptr->lamMap[F1]		= CLIGHT / FREQ1_CMP; /* B1 */
		obs.satNav_ptr->lamMap[F2]		= CLIGHT / FREQ2_CMP; /* B2 */
		obs.satNav_ptr->lamMap[F5]		= CLIGHT / FREQ3_CMP; /* B3 */
	}
	else
	{
		obs.satNav_ptr->lamMap[F1]		= CLIGHT / FREQ1; /* L1/E1 */
		obs.satNav_ptr->lamMap[F2]		= CLIGHT / FREQ2; /* L2 */
		obs.satNav_ptr->lamMap[F5]		= CLIGHT / FREQ5; /* L5/E5a */
		obs.satNav_ptr->lamMap[F6]		= CLIGHT / FREQ6; /* L6/LEX */
		obs.satNav_ptr->lamMap[E5B]		= CLIGHT / FREQ7; /* E5b */
		obs.satNav_ptr->lamMap[E5AB]	= CLIGHT / FREQ8; /* E5a+b */
	}
}

/* satellite carrier wave length -----------------------------------------------
* get satellite carrier wave lengths
* args   : int    sat       I   satellite number
*          int    frq       I   frequency index (0:L1,1:L2,2:L5/3,...)
*          nav_t  *nav      I   navigation messages
* return : carrier wave length (m) (0.0: error)
*-----------------------------------------------------------------------------*/
double satwavelen(SatSys Sat, int frq, SatNav* satNav_ptr)
{
	const double freq_glo[] = { FREQ1_GLO, FREQ2_GLO, FREQ3_GLO};
	const double dfrq_glo[] = { DFRQ1_GLO, DFRQ2_GLO, 0};

	int sys = Sat.sys;

	if		(sys == +E_Sys::GLO)
	{
		if	( frq >= 0
			&&frq <= 2)
		{
			if (satNav_ptr->geph_ptr)		return CLIGHT / (freq_glo[frq] + dfrq_glo[frq] * satNav_ptr->geph_ptr->frq);
			else							return 0;
		}
	}
	else if	(sys == +E_Sys::BDS)
	{
		if      (frq == 0) return CLIGHT / FREQ1_CMP; /* B1 */
		else if (frq == 1) return CLIGHT / FREQ2_CMP; /* B2 */
		else if (frq == 2) return CLIGHT / FREQ3_CMP; /* B3 */
	}
	else
	{
		if      (frq == 0) return CLIGHT / FREQ1; /* L1/E1 */
		else if (frq == 1) return CLIGHT / FREQ2; /* L2 */
		else if (frq == 2) return CLIGHT / FREQ5; /* L5/E5a */
		else if (frq == 3) return CLIGHT / FREQ6; /* L6/LEX */
		else if (frq == 4) return CLIGHT / FREQ7; /* E5b */
		else if (frq == 5) return CLIGHT / FREQ8; /* E5a+b */
	}
	return 0;
}

/** geometric distance
* compute geometric distance and receiver-to-satellite unit vector
* notes  : distance includes sagnac effect correction
*/
double geodist(
	const double *rs,	///< satellilte position (ecef at transmission) (m)
	const double *rr,	///< receiver position (ecef at reception) (m)
	double *e)			///< line-of-sight vector (ecef)
{
	int i;

	if (norm(rs, 3) < RE_WGS84)
		return -1;

	for (i = 0; i < 3; i++)
	{
		e[i] = rs[i] - rr[i];
		//printf("geodist e %f = sat %f - rec %f \n",e[i],rs[i],rr[i]);
	}

	double r = norm(e, 3);

	for (i = 0; i < 3; i++)
		e[i] /= r;

	return r + OMGE * (rs[0] * rr[1] - rs[1] * rr[0]) / CLIGHT;
}

/** geometric distance
* compute geometric distance and receiver-to-satellite unit vector
* notes  : distance includes sagnac effect correction
*/
double geodist(
	Vector3d& rs,	///< satellilte position (ecef at transmission) (m)
	Vector3d& rr,	///< receiver position (ecef at reception) (m)
	Vector3d& e)	///< line-of-sight vector (ecef)
{
	if (rs.norm() < RE_WGS84)
		return -1;

	e = rs - rr;
	double r = e.norm();
	e.normalize();
	return r + OMGE * (rs(0) * rr(1) - rs(1) * rr(0)) / CLIGHT;
}

double sagnac(
	Vector3d& rSource,
	Vector3d& rDest)
{
	return OMGE * (rSource(0) * rDest(1) - rSource(1) * rDest(0)) / CLIGHT;
}

/* satellite azimuth/elevation angle -------------------------------------------
* compute satellite azimuth/elevation angle
* args   : double *pos      I   geodetic position {lat,lon,h} (rad,m)
*          double *e        I   receiver-to-satellilte unit vevtor (ecef)
*          double *azel     IO  azimuth/elevation {az,el} (rad) (nullptr: no output)
*                               (0.0<=azel[0]<2*pi,-pi/2<=azel[1]<=pi/2)
* return : elevation angle (rad)
*-----------------------------------------------------------------------------*/
double satazel(
	const double *pos,
	const double *e,
	double *azel)
{
	double az = 0;
	double el = PI/2;
	double enu[3];
	//printf("pos %f %f %f e: %f enu: %f %f %f\n",pos[0],pos[1],pos[2],e,enu[0],enu[1],enu[2]);
	if (pos[2] > -RE_WGS84)
	{
		ecef2enu(pos, e, enu);
		az = dot(enu, enu, 2) < 1E-12 ? 0.0 : atan2(enu[0], enu[1]);

		if (az < 0)
			az += 2 * PI;

		el = asin(enu[2]);
	}
	if (azel)
	{
		azel[0] = az;
		azel[1] = el;
	}
	return el;
}

double satazel(
	const double*	pos,
	Vector3d&		e,
	double*			azel)
{
	double az = 0;
	double el = PI/2;
	double enu[3];
	//printf("pos %f %f %f e: %f enu: %f %f %f\n",pos[0],pos[1],pos[2],e,enu[0],enu[1],enu[2]);
	if (pos[2] > -RE_WGS84)
	{
		ecef2enu(pos, e.data(), enu);
		az = dot(enu, enu, 2) < 1E-12 ? 0.0 : atan2(enu[0], enu[1]);

		if (az < 0)
			az += 2 * PI;

		el = asin(enu[2]);
	}
	if (azel)
	{
		azel[0] = az;
		azel[1] = el;
	}
	return el;
}

/* compute dops ----------------------------------------------------------------
* compute DOP (dilution of precision)
* args   : int    ns        I   number of satellites
*          double *azel     I   satellite azimuth/elevation angle (rad)
*          double elmin     I   elevation cutoff angle (rad)
*          double *dop      O   DOPs {GDOP,PDOP,HDOP,VDOP}
* notes  : dop[0]-[3] return 0 in case of dop computation error
*-----------------------------------------------------------------------------*/
void dops(
	int				ns,
	const double*	azel,
	double			elmin,
	double*			dop)
{
	vector<double> H;
	H.reserve(64);
	int n = 0;

	for (int i = 0; i < 4; i++)
		dop[i] = 0;

	for (int i = 0; i < ns; i++)
	{
		double az = azel[0+i*2];
		double el = azel[1+i*2];
		
		if (el * R2D < elmin)
		{
			fprintf(stderr,"dops(): below elevation mask azel %f elmin %f \n", el * R2D, elmin);
			continue;
		}
		
		if (el <= 0)
		{
			printf("dops(): el below zero\n");
			continue;
		}
		
		double cosel = cos(el);
		double sinel = sin(el);
		
		H.push_back(cosel * sin(az));
		H.push_back(cosel * cos(az));
		H.push_back(sinel);
		H.push_back(1);
		n++;
	}

	if (n < 4)
	{
		fprintf(stderr,"dops(): Can not calculate the dops less than 4 sats\n");
		return;
	}

	auto H_mat = MatrixXd::Map(H.data(), 4, n).transpose();
	auto Q_mat = H_mat.transpose() * H_mat;
	
	auto Q_inv = Q_mat.inverse();
	
	dop[0] = SQRT(Q_inv(0,0) + Q_inv(1,1) + Q_inv(2,2) + Q_inv(3,3)	);	/* GDOP */
	dop[1] = SQRT(Q_inv(0,0) + Q_inv(1,1) + Q_inv(2,2)				);	/* PDOP */
	dop[2] = SQRT(Q_inv(0,0) + Q_inv(1,1)							);	/* HDOP */
	dop[3] = SQRT(							Q_inv(2,2)				);	/* VDOP */
	
//     {
//         fprintf(stderr,"dops(): error could not calculate the inverse\n");
//     }
}

void sunmoonpos_eci(
	GTime		tut,
	Vector3d*	rsun_ptr,
	Vector3d*	rmoon_ptr)
{
	const double ep2000[] = {2000, 1, 1, 12, 0, 0};

//	trace(4,"%s: tut=%s\n", __FUNCCTION__, tut.to_string(3).c_str());

	double t = (tut - epoch2time(ep2000)) / 86400.0 / 36525.0;

	/* astronomical arguments */
	double f[5];
	ast_args(t, f);

	/* obliquity of the ecliptic */
	double eps	= 23.439291 
				- 0.0130042 * t;
				
	double sine	= sin(eps * D2R); 
	double cose	= cos(eps * D2R);

	/* sun position in eci */
	if (rsun_ptr)
	{
		auto& rsun = *rsun_ptr;
		
		double Ms	= 357.5277233
					+ 35999.05034	* t;
					
		double ls	= 280.460
					+ 36000.770		* t
					+ 1.914666471	* sin(Ms*D2R)
					+ 0.019994643	* sin(Ms*D2R*2);
					
		double rs	= 1.000140612	
					- 0.016708617	* cos(Ms*D2R)
					- 0.000139589	* cos(Ms*D2R*2);
		rs *= AU;
		
		double sinl	= sin(ls*D2R); 
		double cosl	= cos(ls*D2R);
		
		rsun[0] = cosl;
		rsun[1] = cose * sinl;
		rsun[2] = sine * sinl;
		rsun *= rs;

//		trace(5,"rsun =%.3f %.3f %.3f\n",rsun[0],rsun[1],rsun[2]);
	}
	
	/* moon position in eci */
	if (rmoon_ptr)
	{
		auto& rmoon = *rmoon_ptr;
		
		double lm	= 218.32
					+ 481267.883	* t
					+ 6.29			* sin(f[0])
					- 1.27			* sin(f[0]	-f[3]*2)
					+ 0.66			* sin(f[3]*2)
					+ 0.21			* sin(f[0]*2)
					- 0.19			* sin(f[1])
					- 0.11			* sin(f[2]*2);
		
		double pm	= 5.13			* sin(f[2])
					+ 0.28			* sin(f[0]	+f[2])
					- 0.28			* sin(f[2]	-f[0])
					- 0.17			* sin(f[2]	-f[3]*2);
		
		double temp	= 0.9508 
					+ 0.0518		* cos(f[0]) 
					+ 0.0095		* cos(f[0]	-f[3]*2) 
					+ 0.0078		* cos(f[3]*2)
					+ 0.0028		* cos(f[0]*2);
		
		double rm	= RE_WGS84 / sin(temp * D2R);
		
		double sinl	= sin(lm*D2R); 
		double cosl	= cos(lm*D2R);
		
		double sinp	= sin(pm*D2R); 
		double cosp	= cos(pm*D2R);
		
		rmoon[0] = cosp * cosl;
		rmoon[1] = cose * cosp * sinl - sine * sinp;
		rmoon[2] = sine * cosp * sinl + cose * sinp;
		rmoon *= rm;
		
//		trace(5,"rmoon=%.3f %.3f %.3f\n",rmoon[0],rmoon[1],rmoon[2]);
	}
}
/** get sun and moon position in ecef
*/
void sunmoonpos(
	GTime			tutc,	///< time in ut1
	ERPValues&		erpv,	///< erp value {xp,yp,ut1_utc,lod} (rad,rad,s,s/d)
	Vector3d*		rsun_ptr,	///< sun position in ecef  (m)
	Vector3d*		rmoon_ptr,	///< moon position in ecef (m) 
	double*			gmst_ptr)	///< gmst (rad)
{
//	trace(4,"%s: tutc=%s\n",__FUNCTION__, tutc.to_string(3).c_str());

	GTime tut = tutc + erpv.ut1_utc;

	/* sun and moon position in eci */
	Vector3d rs;
	Vector3d rm;
	sunmoonpos_eci(tut, &rs, &rm);

	/* eci to ecef transformation matrix */
	Matrix3d U;
	eci2ecef(tutc, erpv, U, gmst_ptr);

	/* sun and moon postion in ecef */
	if (rsun_ptr )		*rsun_ptr	= U * rs;
	if (rmoon_ptr)		*rmoon_ptr	= U * rm;
}

/** Low pass filter values
*/
void lowPassFilter(
	Average&	avg,
	double		meas,
	double		procNoise,
	double		measVar)
{
	if (avg.var == 0)
	{
		avg.mean	= meas;
		avg.var		= measVar;
		return;
	}
	
	avg.var += SQR(procNoise);
	
	double delta	= meas - avg.mean;
	double varFrac	= 1 / (measVar + avg.var);
	
	avg.mean += delta * varFrac;
	
	avg.var = 1 / (1 / measVar + 1 / avg.var);
}

