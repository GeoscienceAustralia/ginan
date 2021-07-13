
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
#include "constants.h"
#include "algebra.hpp"
#include "common.hpp"
#include "enums.h"


/* test excluded satellite -----------------------------------------------------
* test excluded satellite
* args   : int    sat       I   satellite number
*          int    svh       I   sv health flag
*          prcopt_t *opt    I   processing options (NULL: not used)
* return : status (1:excluded,0:not excluded)
*-----------------------------------------------------------------------------*/
extern int satexclude(SatSys& Sat, int svh)
{
	if (svh < 0)
		return 1; /* ephemeris unavailable */

	if (Sat.sys == +E_Sys::QZS)
		svh&=0xFE; /* mask QZSS LEX health */

	if (svh > 0)
	{
//         trace(3,"unhealthy satellite: sat=%3d svh=%02X\n", Sat, svh);
		return 1;
	}
	return 0;
}

/* YMDHMS to Julian Day --------------------------------------------------------
*
* args     :       const double time[6]   I       civil date time [YMDHMS]
*
* return   :       julian day
*
* reference:       [3]
*----------------------------------------------------------------------------*/
double ymdhms2jd(const double time[6])
{
	double i,j;

	double yr	= time[0];
	double mon	= time[1];
// 	double day	= time[2];
	double hr	= time[3];
	double min	= time[4];
	double sec	= time[5];

	if (yr<=0||yr>=2099) return 0;

	if (mon>2)	{	i = yr;		j = mon;    	}
	else		{	i = yr - 1;	j = mon + 12;	}

	double day	= time[2]
				+ hr	/ 24
				+ min	/ 24/ 60
				+ sec	/ 24/ 60 / 60;

	double jd = (floor(365.25*i)+floor(30.6001*(j+1))+day+1720981.5);

// 	BOOST_LOG_TRIVIAL(debug) << "YMDH to JD: year=" << yr << " mon=" << mon << " day=" << day << " hour=" << hr << ", jd=" << jd;
	return jd;
}

/* crc-24q parity --------------------------------------------------------------
* compute crc-24q parity for sbas, rtcm3
* args   : unsigned char *buff I data
*          int    len    I      data length (bytes)
* return : crc-24Q parity
* notes  : see reference [2] A.4.3.3 Parity
*-----------------------------------------------------------------------------*/
unsigned int crc24q(const unsigned char *buff, int len)
{
	unsigned int crc=0;
	int i;

//     trace(4,"crc24q: len=%d\n",len);

	for (i=0;i<len;i++) crc=((crc<<8)&0xFFFFFF)^tbl_CRC24Q[(crc>>16)^buff[i]];
	return crc;
}

/* encode unsigned/signed bits, I have chosen to use readable code
* above compact or efficient code, Alex.
*/
void setbitu(unsigned char *buff, int pos, int len, unsigned int data)
{
	unsigned int mask=1u<<(len-1);
	int i;
	if (len<=0||32<len) return;
	for (i=pos;i<pos+len;i++,mask>>=1) {
		if (data&mask) buff[i/8]|=1u<<(7-i%8); else buff[i/8]&=~(1u<<(7-i%8));
	}
}
void setbits(unsigned char *buff, int pos, int len, int data)
{
	if (data<0) data|=1<<(len-1); else data&=~(1<<(len-1)); /* set sign bit */
	setbitu(buff,pos,len,(unsigned int)data);
}

int setbituInc(
	unsigned char *buff,
	int pos,
	int len,
	unsigned int var)
{   
	setbitu(buff,pos,len,var);
	return pos+len;
}

int setbitsInc(
	unsigned char *buff,
	int pos,
	int len,
	int var)
{
	setbits(buff,pos,len,var);
	return pos+len;
}


/* extract unsigned/signed bits ------------------------------------------------
* extract unsigned/signed bits from byte data
* args   : unsigned char *buff I byte data
*          int    pos    I      bit position from start of data (bits)
*          int    len    I      bit length (bits) (len<=32)
* return : extracted unsigned/signed bits
*-----------------------------------------------------------------------------*/
unsigned int getbitu(
	const unsigned char *buff,
	int pos,
	int len)
{
	unsigned int bits=0;
	int i;
	for (i=pos;i<pos+len;i++) bits=(bits<<1)+((buff[i/8]>>(7-i%8))&1u);
	return bits;
}

int getbits(
	const unsigned char *buff,
	int pos,
	int len)
{
	unsigned int bits=getbitu(buff,pos,len);
	if (len<=0||32<=len||!(bits&(1u<<(len-1)))) return (int)bits;
	return (int)(bits|(~0u<<len)); /* extend sign */
}

unsigned int getbituInc(
	const unsigned char *buff,
	int& pos,
	int len)
{
	unsigned int ans = getbitu(buff, pos, len);
	pos += len;
	return ans;
}

int getbitsInc(
	const unsigned char *buff,
	int& pos,
	int len)
{
	int ans = getbits(buff, pos, len);
	pos += len;
	return ans;
}

void pos2ecef(const double *pos, double *r)
{
	double sinp=sin(pos[0]);
	double cosp=cos(pos[0]);
	double sinl=sin(pos[1]);
	double cosl=cos(pos[1]);
	double e2=FE_WGS84*(2.0-FE_WGS84);
	double v=RE_WGS84/sqrt(1.0-e2*sinp*sinp);

	r[0]=(v+pos[2])*cosp*cosl;
	r[1]=(v+pos[2])*cosp*sinl;
	r[2]=(v*(1.0-e2)+pos[2])*sinp;
}

/* transform ecef to geodetic postion ------------------------------------------
* transform ecef position to geodetic position
* args   : double *r        I   ecef position {x,y,z} (m)
*          double *pos      O   geodetic position {lat,lon,h} (rad,m)
* return : none
* notes  : WGS84, ellipsoidal height
*-----------------------------------------------------------------------------*/
extern void ecef2pos(const double *r, double *pos)
{
	double e2=FE_WGS84*(2.0-FE_WGS84);
	double r2=dot(r,r,2);
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
extern void ecef2pos(Vector3d& r, double *pos)
{
	double e2=FE_WGS84*(2.0-FE_WGS84);
	double r2=SQR(r(0))+SQR(r(1));
	double z;
	double zk;
	double v=RE_WGS84;
	double sinp;

	for (z=r(2),zk=0.0;fabs(z-zk)>=1E-4;)
	{
		zk=z;
		sinp=z/sqrt(r2+z*z);
		v=RE_WGS84/sqrt(1.0-e2*sinp*sinp);
		z=r(2)+v*e2*sinp;
	}
	pos[0]=r2>1E-12?atan(z/sqrt(r2)):(r(2)>0.0?PI/2.0:-PI/2.0);
	pos[1]=r2>1E-12?atan2(r[1],r[0]):0.0;
	pos[2]=sqrt(r2+z*z)-v;
}
/* ecef to local coordinate transfromation matrix ------------------------------
* compute ecef to local coordinate transfromation matrix
* args   : double *pos      I   geodetic position {lat,lon} (rad)
*          double *E        O   ecef to local coord transformation matrix (3x3)
* return : none
* notes  : matirix stored by column-major order (fortran convention)
*-----------------------------------------------------------------------------*/
extern void xyz2enu(const double *pos, double *E)
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
extern void ecef2enu(const double *pos, const double *r, double *e)
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
extern void enu2ecef(const double *pos, const double *e, double *r)
{
	double E[9];

	xyz2enu(pos,E);
	matmul("TN",3,1,3,1.0,E,e,0.0,r);
}
/* coordinate rotation matrix ------------------------------------------------*/
#define Rx(t,X) do { \
	(X)[0]=1.0; (X)[1]=(X)[2]=(X)[3]=(X)[6]=0.0; \
	(X)[4]=(X)[8]=cos(t); (X)[7]=sin(t); (X)[5]=-(X)[7]; \
} while (0)

#define Ry(t,X) do { \
	(X)[4]=1.0; (X)[1]=(X)[3]=(X)[5]=(X)[7]=0.0; \
	(X)[0]=(X)[8]=cos(t); (X)[2]=sin(t); (X)[6]=-(X)[2]; \
} while (0)

#define Rz(t,X) do { \
	(X)[8]=1.0; (X)[2]=(X)[5]=(X)[6]=(X)[7]=0.0; \
	(X)[0]=(X)[4]=cos(t); (X)[3]=sin(t); (X)[1]=-(X)[3]; \
} while (0)

/* astronomical arguments: f={l,l',F,D,OMG} (rad) ----------------------------*/
void ast_args(double t, double *f)
{
	const double fc[][5]=
	{	/* coefficients for iau 1980 nutation */
		{ 134.96340251, 1717915923.2178,  31.8792,  0.051635, -0.00024470},
		{ 357.52910918,  129596581.0481,  -0.5532,  0.000136, -0.00001149},
		{  93.27209062, 1739527262.8478, -12.7512, -0.001037,  0.00000417},
		{ 297.85019547, 1602961601.2090,  -6.3706,  0.006593, -0.00003169},
		{ 125.04455501,   -6962890.2665,   7.4722,  0.007702, -0.00005939}
	};
	double tt[4];
	int i,j;

	for (tt[0] = t, i = 1; i < 4; i++)
		tt[i] = tt[i-1] * t;
	for (i = 0; i < 5; i++)
	{
		f[i] = fc[i][0] * 3600;
		for (j=0;j<4;j++)
			f[i] += fc[i][j+1] * tt[j];
		f[i] = fmod(f[i] * AS2R, 2 * PI);
	}
}
/* iau 1980 nutation ---------------------------------------------------------*/
void nut_iau1980(double t, const double *f, double *dpsi, double *deps)
{
	const double nut[106][10]=
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
	double ang;
	int i,j;

	*dpsi = 0;
	*deps = 0;

	for (i = 0; i < 106; i++)
	{
		ang=0.0;
		for (j=0;j<5;j++)
			ang+=nut[i][j]*f[j];
		*dpsi += (nut[i][6] + nut[i][7] * t) * sin(ang);
		*deps += (nut[i][8] + nut[i][9] * t) * cos(ang);
	}
	*dpsi *= 1E-4 * AS2R; /* 0.1 mas -> rad */
	*deps *= 1E-4 * AS2R;
}
/* eci to ecef transformation matrix -------------------------------------------
* compute eci to ecef transformation matrix
* args   : gtime_t tutc     I   time in utc
*          double *erpv     I   erp values {xp,yp,ut1_utc,lod} (rad,rad,s,s/d)
*          double *U        O   eci to ecef transformation matrix (3 x 3)
*          double *gmst     IO  greenwich mean sidereal time (rad)
*                               (NULL: no output)
* return : none
* note   : see ref [3] chap 5
*          not thread-safe
*-----------------------------------------------------------------------------*/
void eci2ecef(GTime tutc, const double *erpv, double *U, double *gmst)
{
	const double ep2000[]={2000,1,1,12,0,0};
	GTime tutc_;
	double U_[9],gmst_;
	GTime tgps;
	double eps,ze,th,z,t,t2,t3,dpsi,deps,gast,f[5];
	double R1[9],R2[9],R3[9],R[9],W[9],N[9],P[9],NP[9];
	int i;

//     trace(4,"eci2ecef: tutc=%s\n", tutc.to_string(0).c_str());

	if (0&&fabs(timediff(tutc,tutc_))<0.01)
	{ /* read cache */
		for (i=0;i<9;i++) U[i]=U_[i];
		if (gmst) *gmst=gmst_;
		return;
	}
	tutc_=tutc;

	/* terrestrial time */
	tgps=utc2gpst(tutc_);
	t=(timediff(tgps,epoch2time(ep2000))+19.0+32.184)/86400.0/36525.0;
	t2=t*t; t3=t2*t;

	/* astronomical arguments */
	ast_args(t,f);

	/* iau 1976 precession */
	ze=(2306.2181*t+0.30188*t2+0.017998*t3)*AS2R;
	th=(2004.3109*t-0.42665*t2-0.041833*t3)*AS2R;
	z =(2306.2181*t+1.09468*t2+0.018203*t3)*AS2R;
	eps=(84381.448-46.8150*t-0.00059*t2+0.001813*t3)*AS2R;
	Rz(-z,R1); Ry(th,R2); Rz(-ze,R3);
	matmul("NN",3,3,3,1.0,R1,R2,0.0,R);
	matmul("NN",3,3,3,1.0,R, R3,0.0,P); /* P=Rz(-z)*Ry(th)*Rz(-ze) */

	/* iau 1980 nutation */
	nut_iau1980(t,f,&dpsi,&deps);
	Rx(-eps-deps,R1); Rz(-dpsi,R2); Rx(eps,R3);
	matmul("NN",3,3,3,1.0,R1,R2,0.0,R);
	matmul("NN",3,3,3,1.0,R ,R3,0.0,N); /* N=Rx(-eps)*Rz(-dspi)*Rx(eps) */

	/* greenwich aparent sidereal time (rad) */
	gmst_=utc2gmst(tutc_,erpv[2]);
	gast=gmst_+dpsi*cos(eps);
	gast+=(0.00264*sin(f[4])+0.000063*sin(2.0*f[4]))*AS2R;

	/* eci to ecef transformation matrix */
	Ry(-erpv[0],R1); Rx(-erpv[1],R2); Rz(gast,R3);
	matmul("NN",3,3,3,1.0,R1,R2,0.0,W );
	matmul("NN",3,3,3,1.0,W ,R3,0.0,R ); /* W=Ry(-xp)*Rx(-yp) */
	matmul("NN",3,3,3,1.0,N ,P ,0.0,NP);
	matmul("NN",3,3,3,1.0,R ,NP,0.0,U_); /* U=W*Rz(gast)*N*P */

	for (i=0;i<9;i++) U[i]=U_[i];
	if (gmst) *gmst=gmst_;

//     trace(5,"gmst=%.12f gast=%.12f\n",gmst_,gast);
//     trace(5,"P=\n"); tracemat(5,P,3,3,15,12);
//     trace(5,"N=\n"); tracemat(5,N,3,3,15,12);
//     trace(5,"W=\n"); tracemat(5,W,3,3,15,12);
//     trace(5,"U=\n"); tracemat(5,U,3,3,15,12);
}

/* read blq record -----------------------------------------------------------*/
int readblqrecord(FILE *fp, double *odisp)
{
	double v[11];
	char buff[256];
	int i,n=0;

	while (fgets(buff,sizeof(buff),fp))
	{
		if (!strncmp(buff,"$$",2))
			continue;
		if (sscanf(buff,"%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
				v,v+1,v+2,v+3,v+4,v+5,v+6,v+7,v+8,v+9,v+10)<11)
			continue;
		for (i=0;i<11;i++)
			odisp[n+i*6]=v[i];
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
int readblq(string file, const char *sta, double *odisp)
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
		if (readblqrecord(fp,odisp))
		{
			fclose(fp);
			return 1;
		}
	}
	fclose(fp);
//     trace(2,"no otl parameters: sta=%s file=%s\n",sta,file);
	return 0;
}

/* read earth rotation parameters ----------------------------------------------
* read earth rotation parameters
* args   : char   *file       I   IGS ERP file (IGS ERP ver.2)
*          erp_t  *erp        O   earth rotation parameters
* return : status (1:ok,0:file open error)
*-----------------------------------------------------------------------------*/
int readerp(string file, erp_t *erp)
{
	FILE *fp;
	erpd_t *erp_data;
	double v[14]={0};
	char buff[256];

//     trace(3,"readerp: file=%s\n",file);

	if (!(fp=fopen(file.c_str(), "r"))) {
//         trace(2,"erp file open error: file=%s\n",file);
		return 0;
	}
	while (fgets(buff,sizeof(buff),fp))
	{
		if (sscanf(buff,"%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
				v,v+1,v+2,v+3,v+4,v+5,v+6,v+7,v+8,v+9,v+10,v+11,v+12,v+13)<5)
		{
			continue;
		}
		if (erp->n>=erp->nmax)
		{
			erp->nmax=erp->nmax<=0?128:erp->nmax*2;
			erp_data=(erpd_t *)realloc(erp->data,sizeof(erpd_t)*erp->nmax);
			if (!erp_data)
			{
				free(erp->data); erp->data=NULL; erp->n=erp->nmax=0;
				fclose(fp);
				return 0;
			}
			erp->data=erp_data;
		}
		erp->data[erp->n].mjd=v[0];
		erp->data[erp->n].xp=v[1]*1E-6*AS2R;
		erp->data[erp->n].yp=v[2]*1E-6*AS2R;
		erp->data[erp->n].ut1_utc=v[3]*1E-7;
		erp->data[erp->n].lod=v[4]*1E-7;
		erp->data[erp->n].xpr=v[12]*1E-6*AS2R;
		erp->data[erp->n++].ypr=v[13]*1E-6*AS2R;
	}
	fclose(fp);
	return 1;
}
/* get earth rotation parameter values -----------------------------------------
* get earth rotation parameter values
* args   : erp_t  *erp        I   earth rotation parameters
*          gtime_t time       I   time (gpst)
*          double *erpv       O   erp values {xp,yp,ut1_utc,lod} (rad,rad,s,s/d)
* return : status (1:ok,0:error)
*-----------------------------------------------------------------------------*/
int geterp(const erp_t *erp, GTime time, double *erpv)
{
	const double ep[]={2000,1,1,12,0,0};
	double mjd,day,a;
	int i,j,k;

//     trace(4,"geterp:\n");

	if (erp->n<=0) return 0;

	mjd=51544.5+(timediff(gpst2utc(time),epoch2time(ep)))/86400.0;

	if (mjd<=erp->data[0].mjd)
	{
		day=mjd-erp->data[0].mjd;
		erpv[0]=erp->data[0].xp     +erp->data[0].xpr*day;
		erpv[1]=erp->data[0].yp     +erp->data[0].ypr*day;
		erpv[2]=erp->data[0].ut1_utc-erp->data[0].lod*day;
		erpv[3]=erp->data[0].lod;
		return 1;
	}
	if (mjd>=erp->data[erp->n-1].mjd)
	{
		day=mjd-erp->data[erp->n-1].mjd;
		erpv[0]=erp->data[erp->n-1].xp     +erp->data[erp->n-1].xpr*day;
		erpv[1]=erp->data[erp->n-1].yp     +erp->data[erp->n-1].ypr*day;
		erpv[2]=erp->data[erp->n-1].ut1_utc-erp->data[erp->n-1].lod*day;
		erpv[3]=erp->data[erp->n-1].lod;
		return 1;
	}
	for (j=0,k=erp->n-1;j<k-1;)
	{
		i=(j+k)/2;
		if (mjd<erp->data[i].mjd) k=i; else j=i;
	}
	if (erp->data[j].mjd==erp->data[j+1].mjd)
	{
		a=0.5;
	}
	else
	{
		a=(mjd-erp->data[j].mjd)/(erp->data[j+1].mjd-erp->data[j].mjd);
	}
	erpv[0]=(1.0-a)*erp->data[j].xp     +a*erp->data[j+1].xp;
	erpv[1]=(1.0-a)*erp->data[j].yp     +a*erp->data[j+1].yp;
	erpv[2]=(1.0-a)*erp->data[j].ut1_utc+a*erp->data[j+1].ut1_utc;
	erpv[3]=(1.0-a)*erp->data[j].lod    +a*erp->data[j+1].lod;
	return 1;
}

void updatenav(Obs& obs)
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
	else if	(sys == +E_Sys::CMP)
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
	const double dfrq_glo[] = { DFRQ1_GLO, DFRQ2_GLO, 0.0};

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
	else if	(sys == +E_Sys::CMP)
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

/* geometric distance ----------------------------------------------------------
* compute geometric distance and receiver-to-satellite unit vector
* args   : double *rs       I   satellilte position (ecef at transmission) (m)
*          double *rr       I   receiver position (ecef at reception) (m)
*          double *e        O   line-of-sight vector (ecef)
* return : geometric distance (m) (0>:error/no satellite position)
* notes  : distance includes sagnac effect correction
*-----------------------------------------------------------------------------*/
double geodist(
	const double *rs,
	const double *rr,
	double *e)
{
	int i;

	if (norm(rs, 3) < RE_WGS84)
		return -1.0;

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

double geodist(
	Vector3d& rs,
	Vector3d& rr,
	Vector3d& e)
{
	if (rs.norm() < RE_WGS84)
		return -1.0;

	e = rs - rr;
	double r = e.norm();
	e.normalize();
	return r + OMGE * (rs(0) * rr(1) - rs(1) * rr(0)) / CLIGHT;
}

/* satellite azimuth/elevation angle -------------------------------------------
* compute satellite azimuth/elevation angle
* args   : double *pos      I   geodetic position {lat,lon,h} (rad,m)
*          double *e        I   receiver-to-satellilte unit vevtor (ecef)
*          double *azel     IO  azimuth/elevation {az,el} (rad) (NULL: no output)
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
	GTime tut,
	double *rsun,
	double *rmoon)
{
	const double ep2000[]={2000,1,1,12,0,0};
	double t,f[5],eps,Ms,ls,rs,lm,pm,rm,sine,cose,sinp,cosp,sinl,cosl;

//     trace(4,"sunmoonpos_eci: tut=%s\n", tut.to_string(3).c_str());

	t=timediff(tut,epoch2time(ep2000))/86400.0/36525.0;

	/* astronomical arguments */
	ast_args(t,f);

	/* obliquity of the ecliptic */
	eps=23.439291-0.0130042*t;
	sine=sin(eps*D2R); cose=cos(eps*D2R);

	/* sun position in eci */
	if (rsun) {
		Ms=357.5277233+35999.05034*t;
		ls=280.460+36000.770*t+1.914666471*sin(Ms*D2R)+0.019994643*sin(2.0*Ms*D2R);
		rs=AU*(1.000140612-0.016708617*cos(Ms*D2R)-0.000139589*cos(2.0*Ms*D2R));
		sinl=sin(ls*D2R); cosl=cos(ls*D2R);
		rsun[0]=rs*cosl;
		rsun[1]=rs*cose*sinl;
		rsun[2]=rs*sine*sinl;

//         trace(5,"rsun =%.3f %.3f %.3f\n",rsun[0],rsun[1],rsun[2]);
	}
	/* moon position in eci */
	if (rmoon) {
		lm=218.32+481267.883*t+6.29*sin(f[0])-1.27*sin(f[0]-2.0*f[3])+
		0.66*sin(2.0*f[3])+0.21*sin(2.0*f[0])-0.19*sin(f[1])-0.11*sin(2.0*f[2]);
		pm=5.13*sin(f[2])+0.28*sin(f[0]+f[2])-0.28*sin(f[2]-f[0])-
		0.17*sin(f[2]-2.0*f[3]);
		rm=RE_WGS84/sin((0.9508+0.0518*cos(f[0])+0.0095*cos(f[0]-2.0*f[3])+
				0.0078*cos(2.0*f[3])+0.0028*cos(2.0*f[0]))*D2R);
		sinl=sin(lm*D2R); cosl=cos(lm*D2R);
		sinp=sin(pm*D2R); cosp=cos(pm*D2R);
		rmoon[0]=rm*cosp*cosl;
		rmoon[1]=rm*(cose*cosp*sinl-sine*sinp);
		rmoon[2]=rm*(sine*cosp*sinl+cose*sinp);

//         trace(5,"rmoon=%.3f %.3f %.3f\n",rmoon[0],rmoon[1],rmoon[2]);
	}
}
/* sun and moon position -------------------------------------------------------
* get sun and moon position in ecef
* args   : gtime_t tut      I   time in ut1
*          double *erpv     I   erp value {xp,yp,ut1_utc,lod} (rad,rad,s,s/d)
*          double *rsun     IO  sun position in ecef  (m) (NULL: not output)
*          double *rmoon    IO  moon position in ecef (m) (NULL: not output)
*          double *gmst     O   gmst (rad)
* return : none
*-----------------------------------------------------------------------------*/
void sunmoonpos(
	GTime			tutc,
	const double *	erpv,
	double *		rsun,
	double *		rmoon,
	double *		gmst)
{
	GTime tut;
	double rs[3],rm[3],U[9],gmst_;

//     trace(4,"sunmoonpos: tutc=%s\n",tutc.to_string(3).c_str());

	tut=timeadd(tutc,erpv[2]); /* utc -> ut1 */

	/* sun and moon position in eci */
	sunmoonpos_eci(tut,rsun?rs:NULL,rmoon?rm:NULL);

	/* eci to ecef transformation matrix */
	eci2ecef(tutc,erpv,U,&gmst_);

	/* sun and moon postion in ecef */
	if (rsun ) matmul("NN",3,1,3,1.0,U,rs,0.0,rsun );
	if (rmoon) matmul("NN",3,1,3,1.0,U,rm,0.0,rmoon);
	if (gmst ) *gmst=gmst_;
}

/* replace string ------------------------------------------------------------*/
int repstr(char *str, const char *pat, const char *rep)
{
	int len=strlen(pat);
	char buff[1024],*p,*q,*r;

	for (p=str,r=buff;*p;p=q+len) {
		if (!(q=strstr(p,pat))) break;
		strncpy(r,p,q-p);
		r+=q-p;
		r+=sprintf(r,"%s",rep);
	}
	if (p<=str) return 0;
	strcpy(r,p);
	strcpy(str,buff);
	return 1;
}
