
// #pragma GCC optimize ("O0")

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
#include "corrections.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "ephemeris.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "common.hpp"
#include "tides.hpp"
#include "enums.h"

map<string, map<E_Sys, array<double, 3>>> stationRBiasMap;
	
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
/* int readsap(char *file, gtime_t time, nav_t *nav)
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
	E_DCBPair type = E_DCBPair::NONE;
	SatSys Sat;
//     trace(3,"readdcbf: file=%s\n",file);

	if (!(fp=fopen(file.c_str(), "r")))
	{
//         trace(2,"dcb parameters file open error: %s\n",file);
		return 0;
	}
	while (fgets(buff,sizeof(buff),fp))
	{
		if      (strstr(buff,"DIFFERENTIAL (P1-P2) CODE BIASES"))	type = E_DCBPair::P1_P2;
		else if (strstr(buff,"DIFFERENTIAL (P1-C1) CODE BIASES"))	type = E_DCBPair::P1_C1;
		else if (strstr(buff,"DIFFERENTIAL (P2-C2) CODE BIASES"))	type = E_DCBPair::P2_C2;

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
			if (type == +E_DCBPair::P1_P2)		nav->satNavMap[Sat].cBias_P1_P2		= cbias * 1E-9 * CLIGHT; /* ns -> m */
			if (type == +E_DCBPair::P1_C1)		nav->satNavMap[Sat].cBiasMap[F1]	= cbias * 1E-9 * CLIGHT; /* ns -> m */
			if (type == +E_DCBPair::P2_C2)		nav->satNavMap[Sat].cBiasMap[F2]	= cbias * 1E-9 * CLIGHT; /* ns -> m */
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

double interpolate(
	double*	t, 
	double* x,
	int		n)
{
	KFState kfState;

// 	kfState.output_residuals = true;

	KFMeasEntryList kfMeasEntryList;
	
	for (int i = 0; i < n; i++)
	{
		double T = t[i];
		
		KFMeasEntry kfMeasEntry(&kfState);
		
		double tt = 1;
		
		for (short int j = 0; j < n; j++)
		{
			kfMeasEntry.addDsgnEntry({.num = j}, tt);
			
			tt *= T;
		}
		
		kfMeasEntry.setValue(x[i]);
		kfMeasEntry.setNoise(10 );
		
		kfMeasEntryList.push_back(kfMeasEntry);
	}
	
	//use state transition to initialise states
	kfState.stateTransition(std::cout, GTime::noTime());

// 	kfState.outputStates(std::cout);
	
	//combine the measurement list into a single matrix
	KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

	kfState.leastSquareInitStates(std::cout, combinedMeas, false);

	double value;
	kfState.getKFValue({}, value);
	
	return value;
}

/* satellite position by precise ephemeris -----------------------------------*/
int pephpos(
	GTime		time,
	SatSys		Sat,
	nav_t&		nav,
	Vector3d&	rSat,
	double&		dtSat,
	double*		vare = nullptr,
	double*		varc = nullptr)
{
	double t[NMAX+1];
	double p[3][NMAX+1];
	double c[2];
	double s[3];

	char id[4];
	Sat.getId(id);
//     trace(4,"%s : time=%s sat=%s\n",__FUNCTION__, time.to_string(3).c_str(),id);

	rSat = Vector3d::Zero();
	dtSat	= 0;

	if (nav.pephMap.empty())
	{
		tracepdeex(3, std::cout, "\nLooking for precise position, but no precise ephemerides found");
		return 0;
	}
	
	auto it = nav.pephMap.find(Sat);
	if (it == nav.pephMap.end())
	{
		return 0;
	}
	
	PephList& pephList = nav.pephMap[Sat];

	if	( (pephList.size()	< NMAX + 1)
		||(time	< pephList.begin()	->first - MAXDTE)
		||(time	> pephList.rbegin()	->first + MAXDTE))
	{
        tracepdeex(3, std::cout, "\nno prec ephem %s sat=%s",time.to_string(0).c_str(),id);
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
	for (int i = 0; i <= NMAX; i++)
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

		t[i] = peph.time - time;
		auto& pos = peph.Pos;
#if 0
		p[0][i]=pos[0];
		p[1][i]=pos[1];
#else
		/* correciton for earh rotation ver.2.4.0 */		//todo aaron change to vector
		double sinl = sin(OMGE * t[i]);
		double cosl = cos(OMGE * t[i]);
		p[0][i] = cosl * pos[0] - sinl * pos[1];
		p[1][i] = sinl * pos[0] + cosl * pos[1];
#endif
		p[2][i] = pos[2];
	}

// 	Vector3d rSat2;
	
	for (int i = 0; i < 3; i++)
	{
// 		rSat2	[i] = interpolate	(t, p[i], NMAX + 1);
		rSat	[i] = interppol		(t, p[i], NMAX + 1);
	}
	
// 	rSat = rSat2;
// 	std::cout << std::endl << "diff  " << (rSat2 - rSat).transpose();
	
	if (vare)
	{
		for (int i = 0; i < 3; i++)
			s[i] = middle0->second.PosStd[i];
		double std = norm(s, 3);

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
	t[0] = time - middle0->second.time;
	t[1] = time - middle1->second.time;
	c[0] = middle0->second.Clk;
	c[1] = middle1->second.Clk;

	double std = 0;
	if 		(t[0] <= 0)
	{
		dtSat = c[0];

		if (dtSat != 0)
			std = middle0->second.ClkStd * CLIGHT	+ EXTERR_CLK * fabs(t[0]);
	}
	else if (t[1] >= 0)
	{
		dtSat = c[1];

		if (dtSat != 0)
			std = middle1->second.ClkStd * CLIGHT	+ EXTERR_CLK * fabs(t[1]);
	}
	else if ( c[0] != 0
			&&c[1] != 0)
	{
		dtSat = (c[1] * t[0] - c[0] * t[1]) / (t[0] - t[1]);

		double inv0 = 1 / middle0->second.ClkStd * CLIGHT + EXTERR_CLK * fabs(t[0]);
		double inv1 = 1 / middle1->second.ClkStd * CLIGHT + EXTERR_CLK * fabs(t[1]);
		std			= 1 / (inv0 + inv1);
	}
	else
	{
		dtSat = 0;
	}
	
	if (varc)
		*varc = SQR(std);
		
	return 1;
}

/* satellite clock by precise clock ------------------------------------------*/
int pephclk(
	GTime	time,
	string	id,
	nav_t&	nav,
	double&	dtSat,
	double*	varc)
{
//     BOOST_LOG_TRIVIAL(debug)
// 	<< "pephclk : time=" << time.to_string(3)
// 	<< " id=" << id;
	
	auto it = nav.pclkMap.find(id);
	if (it == nav.pclkMap.end())
	{
		return -1;
	}
	
	auto& [key, pclkList] = *it;
	
	if	( (pclkList.size() < 2)
		||(time	< pclkList.front().	time - MAXDTE)
		||(time	> pclkList.back().	time + MAXDTE))
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
	t[0] = time - middle0->time;
	t[1] = time - middle1->time;
	c[0] = middle0->clk;
	c[1] = middle1->clk;

	double std = 0;

	if		(t[0] <= 0)
	{
		dtSat = c[0];

		if (dtSat == 0)
			return 0;

		std	= middle0->std * CLIGHT	+ EXTERR_CLK * fabs(t[0]);
	}
	else if (t[1] >= 0)
	{
		dtSat = c[1];

		if (dtSat == 0)
			return 0;

		std	= middle1->std * CLIGHT	+ EXTERR_CLK * fabs(t[1]);
	}
	else if	( c[0] != 0
			&&c[1] != 0)
	{
		dtSat = (c[1] * t[0] - c[0] * t[1]) / (t[0] - t[1]);

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
	Trace&				trace,
	GTime				time,
	Vector3d&			rSat,
	SatSys& 			Sat,
	map<int, double>&	lamMap,
	Vector3d&			dAnt,
	PcoMapType*			pcoMap_ptr)
{
	ERPValues erpv;
	E_FType j = F1;
	E_FType k = F2;
	tracepde(4, trace, "%s: time=%s sat=%2d\n", __FUNCTION__, time.to_string(3).c_str() ,Sat);

	/* sun position in ecef */
	Vector3d rsun;
	sunmoonpos(gpst2utc(time), erpv, &rsun);

	/* unit vectors of satellite fixed coordinates */
	Vector3d z = -rSat;			Vector3d ez = z.normalized();			//todo aaron, this is probably everywhere
	Vector3d s = rsun - rSat;	Vector3d es = s.normalized();
	Vector3d y = ez.cross(es);	Vector3d ey = y.normalized();
								Vector3d ex = ey.cross(ez);


	int sys = Sat.sys;
	if 	( sys == E_Sys::GAL
		||sys == E_Sys::SBS)
	{
		k = F5;
	}
	
	/* WARNING: In principle there should be a 

	if (sys == E_Sys::GPS && acsConfig.ionoOpts.iflc_freqs == +E_LinearCombo::L1L5_ONLY) k = F5;

	here, but (as of Nov. 2021) ANTEX files do not have the L5 antenna parameters for GPS */

	if 	( lamMap[j] == 0
		||lamMap[k] == 0)
	{
		return;
	}

	double gamma	= SQR(lamMap[k]) / SQR(lamMap[j]);	
	double C1		= gamma	/ (gamma - 1);
	double C2		= -1	/ (gamma - 1);

	if (pcoMap_ptr == nullptr)
	{
		return;
	}
	auto& pcoMap = *pcoMap_ptr;

	/* iono-free LC */
	Vector3d pcoJ;
	Vector3d pcoK;
	if (pcoMap.find(j) == pcoMap.end())		pcoJ = Vector3d::Zero();
	else									pcoJ = pcoMap[j];
	
	if (pcoMap.find(k) == pcoMap.end())		pcoK = Vector3d::Zero();
	else									pcoK = pcoMap[k];
	
	for (int i = 0; i < 3; i++)
	{
		/* ENU to NEU */
		double dant1	= pcoJ[1] * ex(i)
						+ pcoJ[0] * ey(i)
						+ pcoJ[2] * ez(i);	//todo aaron, matrix
						
		double dant2	= pcoK[1] * ex(i)
						+ pcoK[0] * ey(i)
						+ pcoK[2] * ez(i);

		dAnt(i)	= C1 * dant1
				+ C2 * dant2;
	}
}

void satantoff(
	Trace&				trace,
	GTime				time,
	Vector3d&			rSat,
	E_FType 			ft,
	Vector3d&			dAnt,
	PcoMapType*			pcoMap_ptr)
{
	tracepde(4, trace, "%s: time=%s\n", __FUNCTION__, time.to_string(3).c_str());

	/* sun position in ecef */
	Vector3d rsun;
	ERPValues erpv;
	sunmoonpos(gpst2utc(time), erpv, &rsun);

	/* unit vectors of satellite fixed coordinates */
	Vector3d z = -rSat;			Vector3d ez = z.normalized();			//todo aaron, this is probably everywhere
	Vector3d s = rsun - rSat;	Vector3d es = s.normalized();
	Vector3d y = ez.cross(es);	Vector3d ey = y.normalized();
								Vector3d ex = ey.cross(ez);

	if (pcoMap_ptr == nullptr)
	{
		return;
	}
	auto& pcoMap = *pcoMap_ptr;

	/* iono-free LC */
	Vector3d pco;
	if (pcoMap.find(ft) == pcoMap.end())		pco = Vector3d::Zero();
	else										pco = pcoMap[ft];
	
	for (int i = 0; i < 3; i++)
	{
		/* ENU to NEU */
		dAnt(i)	= pco[1] * ex(i)
				+ pco[0] * ey(i)
				+ pco[2] * ez(i);	//todo aaron, matrix
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
*                                 (nullptr: no output)
* return : pass */
bool peph2pos(
	Trace&		trace,
	GTime		time,
	SatSys&		Sat,
	Vector3d&	rSat,
	Vector3d&	satVel,
	double*		dtSat,
	double&		ephVar,
	E_Svh&		svh,
	nav_t& 		nav,
	bool		applyRelativity)
{
	double dtss1	= 0;
	double dtss2	= 0;
	double varPos	= 0;
	double varClk	= 0;

	tracepde(4, trace, "%s: time=%s sat=%s\n", __FUNCTION__, time.to_string(3).c_str(), Sat.id().c_str());

	svh = SVH_UNHEALTHY;
	
	/* satellite position and clock bias */
	if 	( !pephpos(time, Sat, nav, rSat,	dtss1,	&varPos,	&varClk)
		||!pephclk(time, Sat, nav, 			dtss2,				&varClk))
	{
		return false;
	}

	if (dtss2 != 0)
	{
		double delta = dtss1 - dtss2;
		dtss1 = dtss2;
	}

	double tt		= 1E-3;
	time = time + tt;

	double dtst		= 0;
	Vector3d rSat2	= Vector3d::Zero();
	if 	( !pephpos(time, Sat, nav, rSat2,		dtst)
		||!pephclk(time, Sat, nav, 				dtst))
	{	
		return false;
	}

	satVel = (rSat2 - rSat) / tt;
	
	if (dtss1 != 0)
	{
		double deltaDt = dtst - dtss1;
		
		dtSat[0] = dtss1;
		dtSat[1] = deltaDt / tt;
		
		if (applyRelativity)
		{
			dtSat[0] -= relativity1(rSat, satVel);
		}
	}
	else
	{
		/* no precise clock */
		dtSat[0] = 0;
		dtSat[1] = 0;
	}

	ephVar	= varPos 
			+ varClk;
				
// 	printf("%14.6f %14.6f %14.6f\n", vare, varc, obs.var);
	svh = SVH_OK;
	return true;
}

bool peph2pos(
	Trace&		trace,
	GTime		time,
	SatSys&		Sat,
	Obs&		obs,
	nav_t& 		nav,
	bool		applyRelativity)
{
	return peph2pos(trace, time, Sat, obs.rSat, obs.satVel, obs.dtSat, obs.ephVar, obs.svh, nav, applyRelativity);
}

