
// #pragma GCC optimize ("O0")

#include <stdarg.h>
#include <ctype.h>

#include <boost/log/trivial.hpp>

#include <algorithm>
#include <iostream>
#include <fstream>
#include <string>
#include <array>

#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "enums.h"


/** read blq record 
 */
int readblqrecord(
	FILE *fp, 
	double *otlDisplacement)
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
	SatPos& satPos)
{
	int sys = satPos.Sat.sys;
	if (satPos.satNav_ptr == nullptr)
	{
		return;
	}
	
	auto& satNav = *satPos.satNav_ptr;

	if	( (sys == +E_Sys::GLO)
		&&(satNav.eph_ptr != nullptr))
	{
		auto& geph = *static_cast<Geph*>(satNav.eph_ptr);
		
		satNav.lamMap[G1]	= CLIGHT / (FREQ1_GLO + DFRQ1_GLO * geph.frq);
		satNav.lamMap[G2]	= CLIGHT / (FREQ2_GLO + DFRQ2_GLO * geph.frq);
		satNav.lamMap[G3]	= CLIGHT / (FREQ3_GLO);
		satNav.lamMap[G4]	= CLIGHT / (FREQ4_GLO);
		satNav.lamMap[G6]	= CLIGHT / (FREQ6_GLO);
	}
	else 
	{
		satNav.lamMap[F1]	= CLIGHT / FREQ1; /* L1/E1/B1 */
		satNav.lamMap[F2]	= CLIGHT / FREQ2; /* L2 */
		satNav.lamMap[F5]	= CLIGHT / FREQ5; /* L5/E5a/B2a */
		satNav.lamMap[F6]	= CLIGHT / FREQ6; /* E6/L6 */
		satNav.lamMap[F7]	= CLIGHT / FREQ7; /* E5b/B2/B2b */
		satNav.lamMap[F8]	= CLIGHT / FREQ8; /* E5a+b/B2a+b */
		
		if	(sys == +E_Sys::BDS)
		{
			satNav.lamMap[B1]	= CLIGHT / FREQ1_CMP; /* B2-1 */
			satNav.lamMap[B3]	= CLIGHT / FREQ3_CMP; /* B3 */
		}
	}
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
	if (rs.norm() < RE_WGS84 * 0.9)
		return -1;

	e = rs - rr;
	double r = e.norm();
	e.normalize();
	return r + sagnac(rs, rr);
}

double sagnac(
	Vector3d&	rSource,	//inertial position of source
	Vector3d&	rDest,		//inertial position of destination
	Vector3d	vel)		//inertial velocity
{
	if (vel.isZero())
	{
		Vector3d omega = Vector3d::Zero();
	
		omega.z() = OMGE;
		
		vel = omega.cross(rDest);
	}
	
	//todo aaron, check which vel is required for slr things, still dest on outward journey?
	return (rDest - rSource).dot(vel) / CLIGHT;
}	

/** satellite azimuth/elevation angle
 */
double satazel(
	const	VectorPos&	pos,	///< geodetic position {lat,lon,h} (rad,m)
	const	VectorEcef&	e,		///< receiver-to-satellilte unit vector (ecef)
			double*		azel)	///< azimuth/elevation {az,el} (rad) (nullptr: no output)
{
	double az = 0;
	double el = PI/2;
	
	//printf("pos %f %f %f e: %f enu: %f %f %f\n",pos[0],pos[1],pos[2],e,enu[0],enu[1],enu[2]);
	if (pos.hgt() > -RE_WGS84)
	{
		VectorEnu enu = ecef2enu(pos, e);
		
		az = dot(enu.data(), enu.data(), 2) < 1E-12 ? 0.0 : atan2(enu.e(), enu.n());

		if (az < 0)
			az += 2 * PI;

		el = asin(enu.u());
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

