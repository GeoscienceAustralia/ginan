/*------------------------------------------------------------------------------
* preceph.c : precise ephemeris and clock functions
*
*          Copyright (C) 2007-2013 by T.TAKASU, All rights reserved.
*
* references :
*     [1] S.Hilla, The Extended Standard Product 3 Orbit Format (SP3-c),
*         12 February, 2007
*     [2] J.Ray, W.Gurtner, RINEX Extensions to Handle Clock Information,
*         27 August, 1998
*     [3] D.D.McCarthy, IERS Technical Note 21, IERS Conventions 1996, July 1996
*     [4] D.A.Vallado, Fundamentals of Astrodynamics and Applications 2nd ed,
*         Space Technology Library, 2004
*-----------------------------------------------------------------------------*/

#include <unordered_map>
#include <iostream>
#include <string>
#include <array>
#include <map>
#include <ctype.h>

using std::string;
using std::array;
using std::map;

#include <boost/log/trivial.hpp>


#include "eigenIncluder.hpp"
#include "streamTrace.hpp"
#include "navigation.hpp"
#include "constants.h"
#include "station.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "common.hpp"
#include "tides.hpp"
#include "enums.h"

map<string, map<E_Sys, array<double, 3>>> stationRBiasMap;

//     double rbias[2][3]		= {}; /* receiver dcb (0:p1-p2, 1:p1-c1, 2:p2-c2) (m) */
	
#define NMAX        10              /* order of polynomial interpolation */
#define MAXDTE      900.0           /* max time difference to ephem time (s) */
#define EXTERR_CLK  1E-3            /* extrapolation error for clock (m/s) */
#define EXTERR_EPH  5E-7            /* extrapolation error for ephem (m/s^2) */

/* read satellite antenna parameters -------------------------------------------
* read satellite antenna parameters
* args   : char   *file       I   antenna parameter file
*          gtime_t time       I   time
*          nav_t  *nav        IO  navigation data
* return : status (1:ok,0:error)
* notes  : only support antex format for the antenna parameter file
*-----------------------------------------------------------------------------*/
/*extern int readsap(char *file, gtime_t time, nav_t *nav)
{
	pcvs_t pcvs={0};
	pcv_t pcv0={0},*pcv;
	int i;

	trace(3,"readsap : file=%s time=%s\n",file,time.to_string(0).c_str());

	if (!readpcv(file,&pcvs)) return 0;

	for (i=0;i<MAXSAT;i++) {
		pcv=searchpcv(i+1,"",time,&pcvs);
		nav->pcvs[i]=pcv?*pcv:pcv0;
	}
	free(pcvs.pcv);
	return 1;
}*/
/* read dcb parameters file --------------------------------------------------*/
int readdcb(string file, nav_t *nav)
{
	//todo aaron, use maps for these rather than arrays,
	FILE *fp;
	double cbias;
	char buff[256],str1[32],str2[32]="";
	int type=0;
	SatSys Sat;
//     trace(3,"readdcbf: file=%s\n",file);

	if (!(fp=fopen(file.c_str(), "r")))
	{
//         trace(2,"dcb parameters file open error: %s\n",file);
		return 0;
	}
	while (fgets(buff,sizeof(buff),fp))
	{
		if      (strstr(buff,"DIFFERENTIAL (P1-P2) CODE BIASES"))	type=1;
		else if (strstr(buff,"DIFFERENTIAL (P1-C1) CODE BIASES"))	type=2;
		else if (strstr(buff,"DIFFERENTIAL (P2-C2) CODE BIASES"))	type=3;

		if	(!type
			||sscanf(buff,"%s %s",str1,str2) < 1)
			continue;

		if ((cbias = str2num(buff,26,9)) == 0)
			continue;

		if  ( !strcmp(str1,"G")
			||!strcmp(str1,"R"))
		{
			E_Sys sys;
			if (str1[0] == 'G')		sys = E_Sys::GPS;
			if (str1[0] == 'R')		sys = E_Sys::GLO;
			
			auto& rbias = stationRBiasMap[str2][sys];
			
			/* receiver dcb */
			rbias[type-1] = cbias * 1E-9 * CLIGHT; /* ns -> m */ //todo aaron, this looks like it had issues to begin with
		
		}
		else if (Sat = SatSys(str1), Sat)
		{
			/* satellite dcb */
			if (type == 1)		nav->satNavMap[Sat].cBias_P1_P2		= cbias * 1E-9 * CLIGHT; /* ns -> m */
			if (type == 2)		nav->satNavMap[Sat].cBiasMap[F1]	= cbias * 1E-9 * CLIGHT; /* ns -> m */
			if (type == 3)		nav->satNavMap[Sat].cBiasMap[F2]	= cbias * 1E-9 * CLIGHT; /* ns -> m */
		}
	}
	fclose(fp);

	return 1;
}

/* add satellite fcb ---------------------------------------------------------*/
// int addfcb(nav_t *nav, gtime_t ts, gtime_t te, int sat,
// 				const double *bias, const double *std)	//todo aaron, is this broken?
// {
//     fcbd_t *nav_fcb;
//     int i,j;
//
//     if (nav->nf>0&&fabs(timediff(ts,nav->fcb[nav->nf-1].ts))<=1e-3) {
//         for (i=0;i<3;i++) {
//             nav->fcb[nav->nf-1].bias[sat-1][i]=bias[i];
//             nav->fcb[nav->nf-1].std [sat-1][i]=std [i];
//         }
//         return 1;
//     }
//     if (nav->nf>=nav->nfmax) {
//         nav->nfmax=nav->nfmax<=0?2048:nav->nfmax*2;
//         if (!(nav_fcb=(fcbd_t *)realloc(nav->fcb,sizeof(fcbd_t)*nav->nfmax))) {
//             free(nav->fcb); nav->nf=nav->nfmax=0;
//             return 0;
//         }
//         nav->fcb=nav_fcb;
//     }
//     for (i=0;i<MAXSAT;i++) for (j=0;j<3;j++) {
//         nav->fcb[nav->nf].bias[i][j]=nav->fcb[nav->nf].std[i][j]=0.0;
//     }
//     for (i=0;i<3;i++) {
//         nav->fcb[nav->nf].bias[sat-1][i]=bias[i];
//         nav->fcb[nav->nf].std [sat-1][i]=std [i];
//     }
//     nav->fcb[nav->nf  ].ts=ts;
//     nav->fcb[nav->nf++].te=te;
// 	return 1;
// }
/* read satellite fcb file ---------------------------------------------------*/
// int readfcbf(const char *file, nav_t *nav)
// {
// 	FILE *fp;
// 	gtime_t ts,te;
// 	double ep1[6],ep2[6],bias[3]={0},std[3]={0};
// 	char buff[1024],str[32],*p;
// 	int sat;
// 
// //     trace(3,"readfcbf: file=%s\n",file);
// 
// 	if (!(fp=fopen(file,"r"))) {
// //         trace(2,"fcb parameters file open error: %s\n",file);
// 		return 0;
// 	}
// 	while (fgets(buff,sizeof(buff),fp)) {
// 		if ((p=strchr(buff,'#'))) *p='\0';
// 		if (sscanf(buff,"%lf/%lf/%lf %lf:%lf:%lf %lf/%lf/%lf %lf:%lf:%lf %s"
// 				"%lf %lf %lf %lf %lf %lf",ep1,ep1+1,ep1+2,ep1+3,ep1+4,ep1+5,
// 				ep2,ep2+1,ep2+2,ep2+3,ep2+4,ep2+5,str,bias,std,bias+1,std+1,
// 				bias+2,std+2)<17) continue;
// 		if (!(sat=SatSys(str))) continue;
// 		ts=epoch2time(ep1);
// 		te=epoch2time(ep2);
// 		if (!addfcb(nav,ts,te,sat,bias,std)) return 0;
// 	}
// 	fclose(fp);
// 	return 1;
// }
/* compare satellite fcb -----------------------------------------------------*/
// bool cmpfcb(fcbd_t&p1, fcbd_t&p2)
// {
// 	fcbd_t *q1=&p1,*q2=&p2;
// 	double tt=timediff(q1->ts,q2->ts);
// 	return tt<-1E-3?-1:(tt>1E-3?1:0);
// }
/* read satellite fcb data -----------------------------------------------------
* read satellite fractional cycle bias (dcb) parameters
* args   : char   *file       I   fcb parameters file (wild-card * expanded)
*          nav_t  *nav        IO  navigation data
* return : status (1:ok,0:error)
* notes  : fcb data appended to navigation data
*-----------------------------------------------------------------------------*/
// int readfcb(string& file, nav_t *nav)
// {
// 	readfcbf(file.c_str(), nav);
// // 	nav->fcbList.sort(cmpfcb);
// 	return 1;
// }

/* polynomial interpolation by Neville's algorithm ---------------------------*/
double interppol(const double *x, double *y, int n)
{
	for (int j=1; j < n;		j++)
	for (int i=0; i < n - j;	i++)
	{
		y[i] = (x[i+j] * y[i] - x[i] * y[i+1]) / (x[i+j] - x[i]);
	}

	return y[0];
}

/* satellite position by precise ephemeris -----------------------------------*/
int pephpos(
	GTime time,
	SatSys Sat,
	nav_t& nav,
	double *rs,
	double *dts,
	double *vare,
	double *varc)
{
	double t[NMAX+1],p[3][NMAX+1],c[2],s[3];

	char id[4];
	Sat.getId(id);
//     trace(4,"pephpos : time=%s sat=%s\n",time.to_string(3).c_str(),id);

	rs[0]	= 0;
	rs[1]	= 0;
	rs[2]	= 0;
	*dts	= 0;

	PephList pephList = nav.pephMap[Sat];

	if	( (pephList.size()							< NMAX + 1)
		||(timediff(time, pephList.begin()->first)	< -MAXDTE)
		||(timediff(time, pephList.rbegin()->first)	> +MAXDTE))
	{
//         trace(3,"no prec ephem %s sat=%s\n",time.to_string(0).c_str(),id);
		return 0;
	}

// 	//search for the ephemeris in the list

	auto peph_it = pephList.lower_bound(time);
	if (peph_it == pephList.end())
	{
		peph_it--;
	}

	auto middle0 = peph_it;

	//go forward a few steps to make sure we're far from the end of the list.
	for (int i = 0; i < NMAX/2; i++)
	{
		peph_it++;
		if (peph_it == pephList.end())
		{
			break;
		}
	}

	//go backward a few steps to make sure we're far from the beginning of the list
	for (int i = 0; i <= NMAX; i++)	//todo aaron, needs +1 to go back an extra step due to end()?
	{
		peph_it--;
		if (peph_it == pephList.begin())
		{
			break;
		}
	}

	auto begin = peph_it;

	//get interpolation parameters and check all ephemerides have values.
	peph_it = begin;
	for (int i = 0; i <= NMAX; i++, peph_it++)
	{
		Peph& peph = peph_it->second;
		if (peph.Pos.norm() <= 0)
		{
//             trace(3,"prec ephem outage %s sat=%s\n",time.to_string(0).c_str(), id);
			return 0;
		}

		t[i] = timediff(peph.time, time);
		auto& pos = peph.Pos;
#if 0
		p[0][i]=pos[0];
		p[1][i]=pos[1];
#else
		/* correciton for earh rotation ver.2.4.0 */
		double sinl = sin(OMGE * t[i]);
		double cosl = cos(OMGE * t[i]);
		p[0][i] = cosl * pos[0] - sinl * pos[1];
		p[1][i] = sinl * pos[0] + cosl * pos[1];
#endif
		p[2][i] = pos[2];
	}

	for (int i = 0; i < 3; i++)
	{
		rs[i] = interppol(t, p[i], NMAX + 1);
	}
	double std = 0;
	if (vare)
	{
		for (int i = 0; i < 3; i++)
			s[i] = middle0->second.PosStd[i];
		std = norm(s, 3);

		/* extrapolation error for orbit */
		if      (t[0   ] > 0) std += EXTERR_EPH * SQR(t[0   ]) / 2;		//todo aaron, needs straigtening as below?
		else if (t[NMAX] < 0) std += EXTERR_EPH * SQR(t[NMAX]) / 2;

		*vare = SQR(std);
	}

	/* linear interpolation for clock */
	auto middle1 = middle0;
	if (middle0 != pephList.begin())
	{
		middle0--;
	}
	t[0] = timediff(time, middle0->second.time);
	t[1] = timediff(time, middle1->second.time);
	c[0] = middle0->second.Clk;
	c[1] = middle1->second.Clk;

	if 		(t[0] <= 0)
	{
		*dts = c[0];

		if (*dts != 0)
			std = middle0->second.ClkStd * CLIGHT	+ EXTERR_CLK * fabs(t[0]);
	}
	else if (t[1] >= 0)
	{
		*dts = c[1];

		if (*dts != 0)
			std = middle1->second.ClkStd * CLIGHT	+ EXTERR_CLK * fabs(t[1]);
	}
	else if ( c[0] != 0
			&&c[1] != 0)
	{
		*dts = (c[1] * t[0] - c[0] * t[1]) / (t[0] - t[1]);

		double inv0 = 1 / middle0->second.ClkStd * CLIGHT + EXTERR_CLK * fabs(t[0]);
		double inv1 = 1 / middle1->second.ClkStd * CLIGHT + EXTERR_CLK * fabs(t[1]);
		std			= 1 / (inv0 + inv1);
	}
	else
	{
		*dts = 0;
	}
	if (varc)
		*varc=SQR(std);
	return 1;
}

/* satellite clock by precise clock ------------------------------------------*/
int pephclk(GTime time, string id, nav_t& nav, double *dtSat, double *varc)
{
//     BOOST_LOG_TRIVIAL(debug)
// 	<< "pephclk : time=" << time.to_string(3)
// 	<< " id=" << id;

	PclkList pclkList = nav.pclkMap[id];

	if	( (pclkList.size()							< 2)
		||(timediff(time, pclkList.front().	time)	< -MAXDTE)
		||(timediff(time, pclkList.back().	time)	> +MAXDTE))
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "no prec clock " << time.to_string(0)
		<< " id=" << id;

		return -1;	//non zero for pass, negative for no result
	}

	//search for the ephemeris in the list
	auto pclk_it = pclkList.begin();
	while (pclk_it->time < time)
	{
		pclk_it++;
		if (pclk_it == pclkList.end())
		{
			pclk_it--;
			break;
		}
	}
	auto middle1 = pclk_it;
	auto middle0 = middle1;
	if (middle0 != pclkList.begin())
	{
		middle0--;
	}

	/* linear interpolation for clock */
	double t[2];
	double c[2];
	t[0] = timediff(time, middle0->time);
	t[1] = timediff(time, middle1->time);
	c[0] = middle0->clk;
	c[1] = middle1->clk;

	double std = 0;

	if		(t[0] <= 0)
	{
		*dtSat = c[0];

		if (*dtSat == 0)
			return 0;

		std	= middle0->std * CLIGHT	+ EXTERR_CLK * fabs(t[0]);
	}
	else if (t[1] >= 0)
	{
		*dtSat = c[1];

		if (*dtSat == 0)
			return 0;

		std	= middle1->std * CLIGHT	+ EXTERR_CLK * fabs(t[1]);
	}
	else if	( c[0] != 0
			&&c[1] != 0)
	{
		*dtSat = (c[1] * t[0] - c[0] * t[1]) / (t[0] - t[1]);

		double inv0 = 1 / middle0->std * CLIGHT + EXTERR_CLK * fabs(t[0]);
		double inv1 = 1 / middle1->std * CLIGHT + EXTERR_CLK * fabs(t[1]);
		std			= 1 / (inv0 + inv1);
	}
	else
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "prec clock outage " << time.to_string(0)
		<< " sat=" << id;

		return 0;
	}

	if (varc)
		*varc = SQR(std);

	return 1;
}
/* satellite antenna phase center offset ---------------------------------------
* compute satellite antenna phase center offset in ecef
* args   : gtime_t time       I   time (gpst)
*          double *rs         I   satellite position and velocity (ecef)
*                                 {x,y,z,vx,vy,vz} (m|m/s)
*          int    sat         I   satellite number
*          nav_t  *nav        I   navigation data
*          double *dant       I   satellite antenna phase center offset (ecef)
*                                 {dx,dy,dz} (m) (iono-free LC value)
* return : none
*-----------------------------------------------------------------------------*/
void satantoff(
	Trace&		trace,
	GTime		time,
	Vector3d&	rs,
	SatSys& 	Sat,
	SatNav*		satNav_ptr,
	Vector3d&	dant,
	PcoMapType*	pcoMap_ptr)
{
	auto&	lam = satNav_ptr->lamMap;
	double gmst,erpv[5]={0};
	E_FType j = F1;
	E_FType k = F2;
	tracepde(4,trace, "satantoff: time=%s sat=%2d\n",time.to_string(3).c_str() ,Sat);

	/* sun position in ecef */
	Vector3d rsun;
	sunmoonpos(gpst2utc(time),erpv,rsun.data(),NULL,&gmst);

	/* unit vectors of satellite fixed coordinates */
	Vector3d r = -rs;
	Vector3d ez = r.normalized();
	r = rsun - rs;
	Vector3d es = r.normalized();
	r = ez.cross(es);
	Vector3d ey = r.normalized();
	Vector3d ex = ey.cross(ez);


	int sys = Sat.sys;
	if 	( sys == E_Sys::GAL
		||sys == E_Sys::SBS)
	{
		k = F5;
	}

	if 	( lam[j] == 0
		||lam[k] == 0)
	{
		return;
	}

	double gamma	= SQR(lam[k]) / SQR(lam[j]);		//todo aaron, can use obs2lc?
	double C1		= gamma	/ (gamma - 1);
	double C2		= -1	/ (gamma - 1);

	if (pcoMap_ptr == nullptr)
	{
		return;
	}
	auto& pcoMap = *pcoMap_ptr;

	/* iono-free LC */
	for (int i = 0; i < 3; i++)
	{
		/* ENU to NEU */
		Vector3d pcoJ;
		Vector3d pcoK;
		if (pcoMap.find(j) == pcoMap.end())		pcoJ = Vector3d::Zero();
		else									pcoJ = pcoMap[j];
		if (pcoMap.find(k) == pcoMap.end())		pcoK = Vector3d::Zero();
		else									pcoK = pcoMap[k];
		double dant1	= pcoJ[1] * ex(i)
						+ pcoJ[0] * ey(i)
						+ pcoJ[2] * ez(i);	//todo aaron, matrix
		double dant2	= pcoK[1] * ex(i)
						+ pcoK[0] * ey(i)
						+ pcoK[2] * ez(i);

		dant(i)	= C1 * dant1
				+ C2 * dant2;
	}
}

/* satellite position/clock by precise ephemeris/clock -------------------------
* compute satellite position/clock with precise ephemeris/clock
* args   : gtime_t time       I   time (gpst)
*          int    sat         I   satellite number
*          nav_t  *nav        I   navigation data
*          int    opt         I   sat postion option
*                                 (0: center of mass, 1: antenna phase center)
*          double *rs         O   sat position and velocity (ecef)
*                                 {x,y,z,vx,vy,vz} (m|m/s)
*          double *dts        O   sat clock {bias,drift} (s|s/s)
*          double *var        IO  sat position and clock error variance (m)
*                                 (NULL: no output)
* return : status (1:ok,0:error or data outage)
* notes  : clock includes relativistic correction but does not contain code bias
*          before calling the function, nav->peph, nav->ne, nav->pclk and
*          nav->nc must be set by calling readsp3(), readrnx() or readrnxt()
*          if precise clocks are not set, clocks in sp3 are used instead
*-----------------------------------------------------------------------------*/
int peph2pos(
	Trace&		trace,
	GTime		time,
	SatSys&		Sat,
	nav_t& 		nav,
	int			opt,
	Obs&		obs,
	PcoMapType*	pcoMap_ptr)
{
	Vector3d dAnt	= Vector3d::Zero();
	Vector3d rst	= Vector3d::Zero();

	double dtss1 = 0,dtss2 = 0,dtst,vare=0,varc=0,tt=1E-3;

	tracepde(4,trace, "peph2pos: time=%s sat=%2d opt=%d\n", time.to_string(3).c_str(), Sat, opt);

	/* satellite position and clock bias */
	if 	( !pephpos(time, Sat, nav, obs.rSat.data(),	&dtss1,	&vare,	&varc)
		||!pephclk(time, Sat, nav, 					&dtss2,			&varc))
	{
		return 0;
	}

	if (dtss2 != 0)
	{
		double delta = dtss1 - dtss2;
		dtss1 = dtss2;
	}

	time = timeadd(time,tt);

	if 	( !pephpos(time, Sat, nav, rst.data(),		&dtst,	NULL,	NULL)
		||!pephclk(time, Sat, nav, 					&dtst,			NULL))
		return 0;

	/* satellite antenna offset correction */
	if (opt)
	{
		satantoff(trace, time, obs.rSat, Sat, &nav.satNavMap[Sat], dAnt, pcoMap_ptr);
	}

	obs.satVel = (rst - obs.rSat) / tt;
	obs.rSat += dAnt;

	/* relativistic effect correction */
	if (dtss1 != 0)
	{
		double deltaDt = dtst-dtss1;
		double relativisticAdj = 2 * obs.rSat.dot(obs.satVel) / CLIGHT / CLIGHT;
		obs.dtSat[0] = dtss1 - relativisticAdj;
		obs.dtSat[1] = deltaDt / tt;
	}
	else
	{
		/* no precise clock */
		obs.dtSat[0] = 0;
		obs.dtSat[1] = 0;
	}

	obs.var = vare + varc;

// 	obs.svh = 1;
	return 1;
}

