
// #pragma GCC optimize ("O0")

#include <math.h>

#include "coordinates.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "planets.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "tides.hpp"
#include "trace.hpp"
#include "erp.hpp"
#include "vmf3.h"

#define AS2R        (D2R/3600.0)    /* arc sec to radian */
#define GME         3.986004415E+14 /* earth gravitational constant */
#define GMS         1.327124E+20    /* sun gravitational constant */
#define GMM         4.902801E+12    /* moon gravitational constant */

/* solar/lunar tides (ref [2] 7) 
 */
void tide_pl(
	const Vector3d&		eu, 
	const Vector3d&		rp,
	double				GMp,
	const VectorPos&	pos, 
	Vector3d&			dr)
{
	const double H3 = 0.292;
	const double L3 = 0.015;
	
//     trace(4,"tide_pl : pos=%.3f %.3f\n",pos.latDeg(),pos.lonDeg());
	double r = rp.norm();
	if (r == 0)
		return;

	Vector3d  ep = rp.normalized();

	double K2 = GMp / GME * SQR(RE_WGS84) * SQR(RE_WGS84) / (r * r * r);
	double K3 = K2 * RE_WGS84 / r;
	
	double latp = asin(ep[2]); 
	double lonp = atan2(ep[1], ep[0]);
	
	double cosp = cos(latp); 
	double sinl = sin(pos.lat()); 
	double cosl = cos(pos.lat());

	/* step1 in phase (degree 2) */
	double p = (3 * SQR(sinl) - 1) / 2;
	double H2 = 0.6078 - 0.0006 * p;
	double L2 = 0.0847 + 0.0002 * p;
	double a = ep.dot(eu);
	double dp = K2 * 3 * L2 * a;
	double du = K2 * (H2 * (1.5 * a * a - 0.5) - 3 * L2 * SQR(a));

	/* step1 in phase (degree 3) */
	dp += K3 * L3 * (7.5 * a * a - 1.5);
	du += K3 * (H3 * (2.5 * a * a * a - 1.5 * a) - L3 * (7.5 * a * a - 1.5) * a);

	/* step1 out-of-phase (only radial) */
	du += 3.0 / 4.0 * 0.0025 * K2 * sin(2 * latp) * sin(2 * pos.lat())	* sin(		pos.lon() - lonp);
	du += 3.0 / 4.0 * 0.0022 * K2 * cosp * cosp * cosl * cosl			* sin(2 * (	pos.lon() - lonp));

	dr	= dp * ep 
		+ du * eu;
//     trace(5,"tide_pl : dr=%.3f %.3f %.3f\n",dr[0],dr[1],dr[2]);
}

/* displacement by solid earth tide (ref [2] 7)
 */
void tideSolid(
	const Vector3d&		rsun,
	const Vector3d&		rmoon,
	const VectorPos&	pos,
	const double*		E,
	double				gmst,
	Vector3d&			dr)
{
//     trace(3,"tide_solid: pos=%.3f %.3f opt=%d\n",pos.latDeg(),pos.lonDeg(),opt);

	/* step1: time domain */
	Vector3d eu;
	
	eu[0] = E[2]; 
	eu[1] = E[5]; 
	eu[2] = E[8];
	
	Vector3d dr1;
	Vector3d dr2;
	tide_pl(eu,rsun, GMS,pos,dr1);
	tide_pl(eu,rmoon,GMM,pos,dr2);

	/* step2: frequency domain, only K1 radial */
	double sin2l	= sin(2*pos.lat());
	double du		= -0.012 * sin2l * sin(gmst + pos.lon());

	dr[0] = dr1[0] + dr2[0] + du * E[2];
	dr[1] = dr1[1] + dr2[1] + du * E[5];
	dr[2] = dr1[2] + dr2[2] + du * E[8];

	/* eliminate permanent deformation */
//     if (opt&8) {			//todo aaron, never enabled? add back in?
//         sinl=sin(pos[0]);
//         du=0.1196*(1.5*sinl*sinl-0.5);
//         dn=0.0247*sin2l;
//         dr[0]+=du*E[2]+dn*E[1];
//         dr[1]+=du*E[5]+dn*E[4];
//         dr[2]+=du*E[8]+dn*E[7];
//     }
//     trace(5,"__FUNCTION__: dr=%.3f %.3f %.3f\n",dr[0],dr[1],dr[2]);
}

/* displacement by ocean tide loading (ref [2] 7) 
 */
void tideOLoad(GTime tut, const double *otlDisplacement, double *denu)
{
	const double args[][5] =
	{
		{1.40519E-4, 2.0,-2.0, 0.0, 0.00},  /* M2 */
		{1.45444E-4, 0.0, 0.0, 0.0, 0.00},  /* S2 */
		{1.37880E-4, 2.0,-3.0, 1.0, 0.00},  /* N2 */
		{1.45842E-4, 2.0, 0.0, 0.0, 0.00},  /* K2 */
		{0.72921E-4, 1.0, 0.0, 0.0, 0.25},  /* K1 */
		{0.67598E-4, 1.0,-2.0, 0.0,-0.25},  /* O1 */
		{0.72523E-4,-1.0, 0.0, 0.0,-0.25},  /* P1 */
		{0.64959E-4, 1.0,-3.0, 1.0,-0.25},  /* Q1 */
		{0.53234E-5, 0.0, 2.0, 0.0, 0.00},  /* Mf */
		{0.26392E-5, 0.0, 1.0,-1.0, 0.00},  /* Mm */
		{0.03982E-5, 2.0, 0.0, 0.0, 0.00}   /* Ssa */
	};
	
	const double ep1975[]={1975,1,1,0,0,0};

//     trace(3,"tide_oload:\n");

	/* angular argument: see subroutine arg.f for reference [1] */
	
	GEpoch ep = tut;
	
	double fday	= ep[3]	* 60 * 60
				+ ep[4]	* 60
				+ ep[5];
				
	ep[3]=ep[4]=ep[5]=0;
	
	double days = (epoch2time(ep.data()) - epoch2time(ep1975)).to_double()/86400+1;		//todo aaron, yds?
	double t	= (27392.500528+1.000000035*days)/36525.0;
	double t2	= t*t; 
	double t3	= t*t*t;

	double a[5];
	a[0]= fday;
	a[1]= (279.69668	+ 36000.768930485	* t	+3.03E-4	*t2)					* D2R;	/* H0 */
	a[2]= (270.434358	+ 481267.88314137	* t	-0.001133	*t2	+ 1.9E-6	* t3)	* D2R;	/* S0 */
	a[3]= (334.329653	+ 4069.0340329577	* t	-0.010325	*t2	- 1.2E-5	* t3)	* D2R;	/* P0 */
	a[4]= 2*PI;

	/* displacements by 11 constituents */
	double dp[3] = {};
	for (int i = 0; i < 11; i++) 
	{
		double ang = 0;
		for (int j = 0; j < 5; j++)		ang		+= a[j]*args[i][j];
		for (int j = 0; j < 3; j++)		dp[j]	+= otlDisplacement[j+i*6]*cos(ang-otlDisplacement[j+3+i*6]*D2R);
	}
	denu[0]=-dp[1];
	denu[1]=-dp[2];
	denu[2]= dp[0];

//     trace(5,"tide_oload: denu=%.3f %.3f %.3f\n",denu[0],denu[1],denu[2]);
}

/* iers mean pole (ref [7] eq.7.25)
 */
void iers_mean_pole(
	MjDateUt1	mjdUt1, 
	double&		xp_bar, 
	double&		yp_bar)
{
	double y	= mjdUt1.to_j2000() / 365.25;
	double y2	= y*y; 
	double y3	= y*y*y;

	if (y < 3653.0 / 365.25)
	{ 
		/* until 2010.0 */
		xp_bar =  55.974	+ 1.8243 * y	+ 0.18413 * y2	+ 0.007024 * y3; /* (mas) */
		yp_bar = 346.346	+ 1.7896 * y	- 0.10729 * y2	- 0.000908 * y3;
	}
	else
	{ 
		/* after 2010.0 */
		xp_bar =  23.513	+ 7.6141 * y; /* (mas) */
		yp_bar = 358.891	- 0.6287 * y;
	}
}

/* displacement by pole tide (ref [7] eq.7.26)
 */
void tidePole(
	GTime				time,
	const VectorPos&	pos, 
	ERPValues&			erpv,
	double*				denu)
{
//     trace(3,"tide_pole: pos=%.3f %.3f\n",pos[0]*R2D,pos[1]*R2D);

	/* iers mean pole (mas) */
	double xp_bar;
	double yp_bar;
	MjDateUt1 mjdUt1(time, erpv.ut1Utc);
	iers_mean_pole(mjdUt1, xp_bar, yp_bar);

	/* ref [7] eq.7.24 */
	double m1 = + erpv.xp/AS2R	- xp_bar*1E-3; /* (as) */
	double m2 = - erpv.yp/AS2R	+ yp_bar*1E-3;

	/* sin(2*theta) = sin(2*phi), cos(2*theta)=-cos(2*phi) */
	double cosl = cos(pos.lon());
	double sinl = sin(pos.lon());
	denu[0]=  9E-3*sin(1*pos.lat())*(m1*sinl-m2*cosl); /* de= Slambda (m) */
	denu[1]= -9E-3*cos(2*pos.lat())*(m1*cosl+m2*sinl); /* dn=-Stheta  (m) */
	denu[2]=-33E-3*sin(2*pos.lat())*(m1*cosl+m2*sinl); /* du= Sr      (m) */

//     trace(5,"tide_pole : denu=%.3f %.3f %.3f\n",denu[0],denu[1],denu[2]);
}

/* tidal displacement by earth tides
* args   : gtime_t tutc     I   time in utc
*          double *erp      I   earth rotation parameters 
*          double *odisp    I   ocean loading parameters  (nullptr: not used)
*                                 odisp[0+i*6]: consituent i amplitude radial(m)
*                                 odisp[1+i*6]: consituent i amplitude west  (m)
*                                 odisp[2+i*6]: consituent i amplitude south (m)
*                                 odisp[3+i*6]: consituent i phase radial  (deg)
*                                 odisp[4+i*6]: consituent i phase west    (deg)
*                                 odisp[5+i*6]: consituent i phase south   (deg)
*                                (i=0:M2,1:S2,2:N2,3:K2,4:K1,5:O1,6:P1,7:Q1,
*                                   8:Mf,9:Mm,10:Ssa)
*          double *dr       O   displacement by earth tides (ecef) (m)
* notes  : see ref [1], [2] chap 7
*          see ref [4] 5.2.1, 5.2.2, 5.2.3
*          ver.2.4.0 does not use ocean loading and pole tide corrections
*/
void tideDisp(
	Trace&			trace,
	GTime			time,
	Vector3d&		recPos,
	ERP&			erp,
	const double*	otlDisplacement,
	Vector3d&		dr,
	Vector3d*		solid_ptr,
	Vector3d*		otl_ptr,
	Vector3d*		pole_ptr)
{
	int lv = 3;
	
	string timeStr = time.to_string();

	tracepdeex(3,trace,"\n\n%s: time=%s", __FUNCTION__, time.to_string(2).c_str());

	ERPValues erpv = getErp(erp, time);
	
	dr = Vector3d::Zero();

	if (recPos.isZero())
		return;

	VectorPos pos;
	pos.lat() = asin(recPos[2] / recPos.norm());		//todo aaron, use other function
	pos.lon() = atan2(recPos[1], recPos[0]);
	
	double E[9];
	pos2enu(pos,E);

	if (acsConfig.model.tides.solid)
	{
		/* solid earth tides */

		/* sun and moon position in ecef */
		Vector3d	drt		= Vector3d::Zero();
		MjDateUt1	mjdUt1(time, erpv.ut1Utc);
		double		gmst	= Sofa::iauGmst(mjdUt1, time);
		
		VectorEcef	rSun;
		VectorEcef	rMoon;
		planetPosEcef(time, E_ThirdBody::MOON,	rMoon,	erpv);
		planetPosEcef(time, E_ThirdBody::SUN,	rSun,	erpv);
		
		tideSolid(rSun, rMoon, pos, E, gmst, drt);
		
		dr += drt;
		
		if (solid_ptr)
		{
			*solid_ptr = drt;
		}

		tracepdeex(lv, trace,"\n%s   SOLID %14.4f %14.4f %14.4f", timeStr.c_str(), drt[0], drt[1], drt[2]);
	}
	
	if	( acsConfig.model.tides.otl
		&&otlDisplacement)
	{
		/* ocean tide loading */
		double denu	[3] = {};
		Vector3d drt	= Vector3d::Zero();
		tideOLoad(time, otlDisplacement, denu);
		matmul("TN", 3, 1, 3, 1, E, denu, 0, drt.data());

		dr += drt;
		
		if (otl_ptr)
		{
			*otl_ptr = drt;
		}

		tracepdeex(lv, trace, "\n%s   OCEAN %14.4f %14.4f %14.4f", timeStr.c_str(), drt[0], drt[1], drt[2]);
	}
	
	if 	(acsConfig.model.tides.pole)
	{
		/* pole tide */
		double denu	[3] = {};
		Vector3d drt	= Vector3d::Zero();
		tidePole(time, pos, erpv, denu);
		matmul("TN", 3, 1, 3, 1, E, denu, 0, drt.data());

		dr += drt;
		
		if (pole_ptr)
		{
			*pole_ptr = drt;
		}

		tracepdeex(lv, trace, "\n%s   POLE  %14.4f %14.4f %14.4f", timeStr.c_str(), drt[0], drt[1], drt[2]);
	}
	tracepdeex(3,trace,"\n-TIDEDISP\n");
}
