
#include <unordered_map>

#include "eigenIncluder.hpp"

#include "acsConfig.hpp"
#include "constants.h"
#include "common.hpp"
#include "enums.h"
#include "algebra.hpp"
#include "satSys.hpp"
#include "navigation.hpp"
#include "preceph.hpp"
#include "streamTrace.hpp"
#include "testUtils.hpp"

#define 	NMAX		10

/* constants and macros ------------------------------------------------------*/

#define RE_GLO   6378136.0        /* radius of earth (m)            ref [2] */
#define MU_GPS   3.9860050E14     /* gravitational constant         ref [1] */
#define MU_GLO   3.9860044E14     /* gravitational constant         ref [2] */
#define MU_GAL   3.986004418E14   /* earth gravitational constant   ref [7] */
#define MU_CMP   3.986004418E14   /* earth gravitational constant   ref [9] */
#define J2_GLO   1.0826257E-3     /* 2nd zonal harmonic of geopot   ref [2] */

#define OMGE_GLO 7.292115E-5      /* earth angular velocity (rad/s) ref [2] */
#define OMGE_GAL 7.2921151467E-5  /* earth angular velocity (rad/s) ref [7] */
#define OMGE_CMP 7.292115E-5      /* earth angular velocity (rad/s) ref [9] */

#define SIN_5 -0.0871557427476582 /* sin(-5.0 deg) */
#define COS_5  0.9961946980917456 /* cos(-5.0 deg) */

#define ERREPH_GLO 5.0            /* error of glonass ephemeris (m) */
#define TSTEP    60.0             /* integration step glonass ephemeris (s) */
#define RTOL_KEPLER 1E-14         /* relative tolerance for Kepler equation */

#define DEFURASSR 0.03            /* default accurary of ssr corr (m) */
#define MAXECORSSR 10.0           /* max orbit correction of ssr (m) */
#define MAXCCORSSR (1E-6*CLIGHT)  /* max clock correction of ssr (m) */
#define MAXAGESSR 90.0            /* max age of ssr orbit and clock (s) */
#define MAXAGESSR_HRCLK 10.0      /* max age of ssr high-rate clock (s) */
#define STD_BRDCCLK 30.0          /* error of broadcast clock (m) */

#define MAX_ITER_KEPLER 30        /* max number of iteration of Kelpler */

/* variance by ura ephemeris (ref [1] 20.3.3.3.1.1) --------------------------*/
double var_uraeph(int ura)
{
	const double ura_value[] =
	{
		2.4, 3.4, 4.85, 6.85, 9.65, 13.65, 24.0, 48.0, 96.0, 192.0, 384.0, 768.0, 1536.0, 3072.0, 6144.0
	};
	return ura < 0 || 15 < ura ? SQR(6144.0) : SQR(ura_value[ura]);
}

/* variance by ura ssr (ref [4]) ---------------------------------------------*/
double var_urassr(int ura)
{
	if (ura <= 0)		return SQR(DEFURASSR);
	if (ura >= 63)		return SQR(5.4665);

	double std = (pow(3, (ura >> 3) & 7) * (1.0 + (ura & 7) / 4.0) - 1.0) * 1E-3;
	return SQR(std);
}

/* broadcast ephemeris to satellite clock bias ---------------------------------
* compute satellite clock bias with broadcast ephemeris (gps, galileo, qzss)
* args   : gtime_t time     I   time by satellite clock (gpst)
*          Eph *eph       I   broadcast ephemeris
* return : satellite clock bias (s) without relativeity correction
* notes  : see ref [1],[7],[8]
*          satellite clock does not include relativity correction and tdg
*-----------------------------------------------------------------------------*/
double eph2clk(GTime      time, Eph* eph)
{
//     trace(4,"eph2clk : time=%s sat=%2d\n",time.to_string(3).c_str(),eph->Sat);
	double t = timediff(time, eph->toc);

	for (int i = 0; i < 2; i++)
	{
		t	-= eph->f0
			+  eph->f1 * t
			+  eph->f2 * t * t;
	}

	double ans	= eph->f0
				+ eph->f1 * t
				+ eph->f2 * t * t;
	return ans;
}

/* broadcast ephemeris to satellite position and clock bias --------------------
* compute satellite position and clock bias with broadcast ephemeris (gps,
* galileo, qzss)
* args   : gtime_t time     I   time (gpst)
*          Eph *eph       I   broadcast ephemeris
*          double *rs       O   satellite position (ecef) {x,y,z} (m)
*          double *dts      O   satellite clock bias (s)
*          double *var      O   satellite position and clock variance (m^2)
* return : none
* notes  : see ref [1],[7],[8]
*          satellite clock includes relativity correction without code bias
*          (tgd or bgd)
*-----------------------------------------------------------------------------*/
void eph2pos(
	GTime		time,
	Eph*		eph,
	double*		rs,
	double*		dts,
	double*		var)
{
//     trace(4,"eph2pos : time=%s sat=%2d\n",time.to_string(3).c_str(),eph->Sat);

	if (eph->A <= 0)
	{
		rs[0]	= 0;
		rs[1]	= 0;
		rs[2]	= 0;
		*dts	= 0;
		*var	= 0;
		return;
	}

	double tk = timediff(time, eph->toe);
	int prn = eph->Sat.prn;
	int sys = eph->Sat.sys;

	double mu;
	double omge;
	switch (sys)
	{
		case E_Sys::GAL: mu = MU_GAL; omge = OMGE_GAL; break;
		case E_Sys::CMP: mu = MU_CMP; omge = OMGE_CMP; break;		//todo aaron, lookup table
		default:     	 mu = MU_GPS; omge = OMGE;     break;
	}

	double M = eph->M0 + (sqrt(mu / (eph->A * eph->A * eph->A)) + eph->deln) * tk;

	double E	= M;
	double Ek	= 0;
	int n;
	for (n = 0; fabs(E - Ek) > RTOL_KEPLER && n < MAX_ITER_KEPLER; n++)
	{
		Ek = E;
		E -= (E - eph->e * sin(E) - M) / (1 - eph->e * cos(E));
	}

	if (n >= MAX_ITER_KEPLER)
	{
//         trace(2,"kepler iteration overflow sat=%2d\n",eph->Sat);
		return;
	}

	double sinE = sin(E);
	double cosE = cos(E);

//     trace(4,"kepler: sat=%2d e=%8.5f n=%2d del=%10.3e\n",eph->Sat,eph->e,n,E-Ek);

	double u = atan2(sqrt(1 - eph->e * eph->e) * sinE, cosE - eph->e) + eph->omg;
	double r = eph->A * (1 - eph->e * cosE);
	double i	= eph->i0
				+ eph->idot * tk;
	double sin2u = sin(2 * u);
	double cos2u = cos(2 * u);

	u += eph->cus * sin2u + eph->cuc * cos2u;
	r += eph->crs * sin2u + eph->crc * cos2u;
	i += eph->cis * sin2u + eph->cic * cos2u;

	double x	= r * cos(u);
	double y	= r * sin(u);
	double cosi = cos(i);

	/* beidou geo satellite (ref [9]) */
	if (sys == +E_Sys::CMP && prn <= 5)
	{
		double O	= eph->OMG0
					+ eph->OMGd * tk
					- omge * eph->toes;
		double sinO = sin(O);
		double cosO = cos(O);
		double xg = x * cosO - y * cosi * sinO;
		double yg = x * sinO + y * cosi * cosO;
		double zg = y * sin(i);
		double sino = sin(omge * tk);
		double coso = cos(omge * tk);
		rs[0] = +xg * coso + yg * sino * COS_5 + zg * sino * SIN_5;
		rs[1] = -xg * sino + yg * coso * COS_5 + zg * coso * SIN_5;
		rs[2] = -yg * SIN_5 + zg * COS_5;
	}
	else
	{
		double O	= eph->OMG0
					+ (eph->OMGd - omge) * tk
					- omge * eph->toes;
		double sinO = sin(O);
		double cosO = cos(O);
		rs[0] = x * cosO - y * cosi * sinO;
		rs[1] = x * sinO + y * cosi * cosO;
		rs[2] = y * sin(i);
	}

	tk = timediff(time, eph->toc);
	*dts	= eph->f0
			+ eph->f1 * tk
			+ eph->f2 * tk * tk;

	/* relativity correction */
	*dts	-= 2 * sqrt(mu * eph->A) * eph->e * sinE / SQR(CLIGHT); //is equivalent to - 2 * obs.rSat.dot(obs.satVel) / CLIGHT; 

	/* position and clock error variance */
	if (var)
		*var = var_uraeph(eph->sva);
}

/* glonass orbit differential equations --------------------------------------*/
void deq(const double* x, double* xdot, const double* acc)
{
	double a, b, c, r2 = dot(x, x, 3), r3 = r2 * sqrt(r2), omg2 = SQR(OMGE_GLO);

	if (r2 <= 0.0)
	{
		xdot[0] = xdot[1] = xdot[2] = xdot[3] = xdot[4] = xdot[5] = 0.0;
		return;
	}

	/* ref [2] A.3.1.2 with bug fix for xdot[4],xdot[5] */
	a = 1.5 * J2_GLO * MU_GLO * SQR(RE_GLO) / r2 / r3; /* 3/2*J2*mu*Ae^2/r^5 */
	b = 5.0 * x[2] * x[2] / r2;            /* 5*z^2/r^2 */
	c = -MU_GLO / r3 - a * (1.0 - b);      /* -mu/r^3-a(1-b) */
	xdot[0] = x[3];
	xdot[1] = x[4];
	xdot[2] = x[5];
	xdot[3] = (c + omg2) * x[0] + 2.0 * OMGE_GLO * x[4] + acc[0];
	xdot[4] = (c + omg2) * x[1] - 2.0 * OMGE_GLO * x[3] + acc[1];
	xdot[5] = (c - 2.0 * a) * x[2] + acc[2];
}
/* glonass position and velocity by numerical integration --------------------*/
void glorbit(double t, double* x, const double* acc)
{
	double k1[6], k2[6], k3[6], k4[6], w[6];
	int i;

	deq(x, k1, acc); for (i = 0; i < 6; i++) w[i] = x[i] + k1[i] * t / 2;
	deq(w, k2, acc); for (i = 0; i < 6; i++) w[i] = x[i] + k2[i] * t / 2;
	deq(w, k3, acc); for (i = 0; i < 6; i++) w[i] = x[i] + k3[i] * t;
	deq(w, k4, acc);

	for (i = 0; i < 6; i++)
		x[i] += (k1[i] + 2 * k2[i] + 2 * k3[i] + k4[i]) * t / 6;
}

/* glonass ephemeris to satellite clock bias -----------------------------------
* compute satellite clock bias with glonass ephemeris
* args   : gtime_t time     I   time by satellite clock (gpst)
*          Geph *geph     I   glonass ephemeris
* return : satellite clock bias (s)
* notes  : see ref [2]
*-----------------------------------------------------------------------------*/
double geph2clk(GTime time, Geph* geph)
{
	double t;
	int i;

//     trace(4,"geph2clk: time=%s sat=%2d\n",time.to_string(3).c_str(),geph->Sat);

	t = timediff(time, geph->toe);

	for (i = 0; i < 2; i++)
	{
		t -= -geph->taun + geph->gamn * t;
	}

	return -geph->taun + geph->gamn * t;
}

/* glonass ephemeris to satellite position and clock bias ----------------------
* compute satellite position and clock bias with glonass ephemeris
* args   : gtime_t time     I   time (gpst)
*          Geph *geph     I   glonass ephemeris
*          double *rs       O   satellite position {x,y,z} (ecef) (m)
*          double *dts      O   satellite clock bias (s)
*          double *var      O   satellite position and clock variance (m^2)
* return : none
* notes  : see ref [2]
*-----------------------------------------------------------------------------*/
void geph2pos(GTime time, Geph* geph, double* rs, double* dts, double* var)
{
	double t, tt, x[6];
	int i;

//     trace(4,"geph2pos: time=%s sat=%2d\n",time.to_string(3).c_str(),geph->Sat);

	t = timediff(time, geph->toe);

	*dts = -geph->taun + geph->gamn * t;

	for (i = 0; i < 3; i++)
	{
		x[i    ] = geph->pos[i];
		x[i + 3] = geph->vel[i];
	}

	for (tt = t < 0 ? -TSTEP : TSTEP; fabs(t) > 1E-9; t -= tt)
	{
		if (fabs(t) < TSTEP)
			tt = t;

		glorbit(tt, x, geph->acc);
	}

	for (i = 0; i < 3; i++)
		rs[i] = x[i];

	if (var)
		*var = SQR(ERREPH_GLO);
}

/* sbas ephemeris to satellite clock bias --------------------------------------
* compute satellite clock bias with sbas ephemeris
* args   : gtime_t time     I   time by satellite clock (gpst)
*          Seph *seph     I   sbas ephemeris
* return : satellite clock bias (s)
* notes  : see ref [3]
*-----------------------------------------------------------------------------*/
extern double seph2clk(GTime time, Seph* seph)
{
	double t;
	int i;

//     trace(4,"seph2clk: time=%s sat=%2d\n",time.to_string(3).c_str(),seph->Sat);

	t = timediff(time, seph->t0);

	for (i = 0; i < 2; i++)
	{
		t -= seph->af0 + seph->af1 * t;
	}

	return seph->af0 + seph->af1 * t;
}

/* sbas ephemeris to satellite position and clock bias -------------------------
* compute satellite position and clock bias with sbas ephemeris
* args   : gtime_t time     I   time (gpst)
*          Seph  *seph    I   sbas ephemeris
*          double  *rs      O   satellite position {x,y,z} (ecef) (m)
*          double  *dts     O   satellite clock bias (s)
*          double  *var     O   satellite position and clock variance (m^2)
* return : none
* notes  : see ref [3]
*-----------------------------------------------------------------------------*/
extern void seph2pos(GTime time, Seph* seph, double* rs, double* dts, double* var)
{
	double t;
	int i;

//     trace(4,"seph2pos: time=%s sat=%2d\n",time.to_string(3).c_str(),seph->Sat);

	t = timediff(time, seph->t0);

	for (i = 0; i < 3; i++)
	{
		rs[i]	= seph->pos[i]
				+ seph->vel[i] * t
				+ seph->acc[i] * t * t / 2;
	}

	*dts	= seph->af0
			+ seph->af1 * t;

	if (var)
		*var = var_uraeph(seph->sva);
}

/* select ephememeris --------------------------------------------------------*/
Eph* seleph(
	GTime	time, 
	SatSys	Sat, 
	int		iode, 
	nav_t&	nav)
{
	double t;
	double tmax;
//	double tmin;
	
	Eph* chosen = nullptr;
//     trace(4,"seleph  : time=%s sat=%2d iode=%d\n",time.to_string(3).c_str(),Sat,iode);

	switch (Sat.sys)
	{
		case E_Sys::QZS:	tmax = MAXDTOE_QZS	+ 1; break;
		case E_Sys::GAL:	tmax = MAXDTOE_GAL	+ 1; break;
		case E_Sys::CMP:	tmax = MAXDTOE_CMP	+ 1; break;
		default: 			tmax = MAXDTOE		+ 1; break;
	}

//	tmin = tmax + 1;

	auto& ephList = nav.ephMap[Sat];

	GTime latestToe = GTime::noTime();
	for (auto& eph : ephList)
	{
		if 	( iode >= 0
			&&iode != eph.iode)
			continue;

		t = fabs(timediff(eph.toe, time));

		if (t > tmax)
			continue;

		if (iode >= 0)
			return &eph;

//		if (t <= tmin)
//		{
//			/* toe closest to time */
//			chosen	= &eph;
//			tmin	= t;
//		}
		if (eph.toe > latestToe)
		{
			/* latest ephem with toe within +-MAXDTOE of current epoch */
			chosen	= &eph;
			latestToe = eph.toe;
		}
	}

	if	(  iode >= 0
		|| chosen == nullptr)
	{
//         trace(2,"no broadcast ephemeris: %s sat=%2d iode=%3d\n",time.to_string(0).c_str(), Sat,iode);
		return nullptr;
	}

	return chosen;
}

/* select glonass ephememeris ------------------------------------------------*/
Geph* selgeph(GTime time, SatSys Sat, int iode, nav_t& nav)
{
	double t, tmax = MAXDTOE_GLO, tmin = tmax + 1.0;

	Geph* closest = NULL;
//     trace(4,"selgeph : time=%s sat=%2d iode=%2d\n",time.to_string(3).c_str(),Sat,iode);

	auto& gephList = nav.gephMap[Sat];

	for (auto& geph : gephList)
	{
		if (iode >= 0 && geph.iode != iode) 				continue;
		if ((t = fabs(timediff(geph.toe, time))) > tmax)	continue;
		if (iode >= 0)										return &geph;
		if (t <= tmin)
		{
			/* toe closest to time */
			closest = &geph;
			tmin = t;
		}
	}

	if (iode >= 0 || closest == NULL)
	{
//         trace(3,"no glonass ephemeris  : %s sat=%2d iode=%2d\n",time.to_string(0).c_str(),Sat,iode);
		return NULL;
	}

	return closest;
}

/* select sbas ephememeris ---------------------------------------------------*/
Seph* selseph(GTime time, SatSys Sat, nav_t& nav)
{
	double t, tmax = MAXDTOE_SBS, tmin = tmax + 1.0;
	Seph* closest = NULL;

//     trace(4,"selseph : time=%s sat=%2d\n",time.to_string(3).c_str(),Sat);

	auto& sephList = nav.sephMap[Sat];

	for (auto& seph : sephList)
	{
		if ((t = fabs(timediff(seph.t0, time))) > tmax)
			continue;

		if (t <= tmin)
		{
			closest = &seph;
			tmin = t;

		} /* toe closest to time */
	}

	if (closest == NULL)
	{
//         trace(3,"no sbas ephemeris     : %s sat=%2d\n",time.to_string(0).c_str(),Sat);
		return NULL;
	}

	return closest;
}

/* satellite clock with broadcast ephemeris ----------------------------------*/
int ephclk(GTime time, GTime teph, Obs& obs, double& dts)
{
	int sys;

//     trace(4,"ephclk  : time=%s sat=%2d\n",time.to_string(3).c_str(),obs.Sat);

	sys = obs.Sat.sys;

	//todo aaron, need to check ephemeris timeout. with teph
	switch (sys)
	{
		case E_Sys::GPS:
		case E_Sys::GAL:
		case E_Sys::QZS:
		case E_Sys::CMP:	{	if (!obs.satNav_ptr->eph_ptr)	return 0;	dts = eph2clk (time, obs.satNav_ptr->eph_ptr);	break;	}
		case E_Sys::GLO:	{	if (!obs.satNav_ptr->geph_ptr)	return 0;	dts = geph2clk(time, obs.satNav_ptr->geph_ptr);	break;	}
		case E_Sys::SBS:	{	if (!obs.satNav_ptr->seph_ptr)	return 0;	dts = seph2clk(time, obs.satNav_ptr->seph_ptr);	break;	}
		default:
		{
			return 0;
		}
	}

	return 1;
}

/* satellite position and clock by broadcast ephemeris -----------------------*/
int ephpos(
GTime	time,
GTime	teph,
Obs&	obs,
nav_t&	nav,
int		iode)
{
	Vector3d rst;
	double dtst, tt = 1E-3;

//     trace(4,"ephpos  : time=%s sat=%2d iode=%d\n",time.to_string(3).c_str(),obs.Sat,iode);

	SatSys Sat = obs.Sat;
	int sys = Sat.sys;

	obs.svh = -1;

	if	(  sys == +E_Sys::GPS
		|| sys == +E_Sys::GAL
		|| sys == +E_Sys::QZS
		|| sys == +E_Sys::CMP)
	{
		Eph* eph = seleph(teph, Sat, iode, nav);

		if (!eph)
			return 0;

										eph2pos(time, 	eph, 	obs.rSat.data(), 	obs.dtSat, 	&obs.var);
		time = timeadd(time, tt);		eph2pos(time,	eph, 	rst.data(),			&dtst,		nullptr);
		
		obs.svh		= eph->svh;
		obs.iode	= eph->iode;
	}
	else if (sys == +E_Sys::GLO)
	{
		Geph* geph = selgeph(teph, Sat, iode, nav);

		if (!geph)
			return 0;

										geph2pos(time,	geph,	obs.rSat.data(),	obs.dtSat,	&obs.var);
		time = timeadd(time, tt);		geph2pos(time,	geph,	rst.data(),			&dtst,		nullptr);
		
		obs.svh = geph->svh;
	}
	else if (sys == +E_Sys::SBS)
	{
		Seph* seph = selseph(teph, Sat, nav);

		if (!seph)
			return 0;

										seph2pos(time,	seph,	obs.rSat.data(),	obs.dtSat,	&obs.var);
		time = timeadd(time, tt);		seph2pos(time,	seph,	rst.data(),			&dtst,		nullptr);
		
		obs.svh = seph->svh;
	}
	else
		return 0;

	/* satellite velocity and clock drift by differential approx */
	obs.satVel		= (rst	- obs.rSat) 	/ tt;
	obs.dtSat[1]	= (dtst	- obs.dtSat[0])	/ tt;

	return 1;
}
#if (0)
/* satellite position and clock with sbas correction -------------------------*/
int satpos_sbas(gtime_t time, gtime_t teph, SatSys Sat, const nav_t* nav,
				double* rs, double* dtSat, double* var, int* svh)
{

	const sbssatp_t* sbs;
	int i;

	trace(4, "satpos_sbas: time=%s sat=%2d\n", time.to_string(3).c_str(), Sat);

	/* search sbas satellite correciton */
	for (i = 0; i < nav->sbssat.nsat; i++)
	{
		sbs = nav->sbssat.sat + i;

		if (sbs->Sat == Sat)
			break;
	}

	if (i >= nav->sbssat.nsat)
	{
		trace(2, "no sbas correction for orbit: %s sat=%2d\n", time.to_string(0).c_str(), Sat);
		ephpos(time, teph, Sat, nav, -1, rs, dts, var, svh);
		*svh = -1;

		return 0;
	}

	/* satellite postion and clock by broadcast ephemeris */
	if (!ephpos(time, teph, Sat, nav, sbs->lcorr.iode, rs, dts, var, svh))
		return 0;

	/* sbas satellite correction (long term and fast) */

	if (sbssatcorr(time, Sat, nav, rs, dts, var))
		return 1;

	*svh = -1;

	return 0;
}
#endif

Vector3d ecef2rac(
	Vector3d vecToRotate,	// ECEF vector to transform to RAC
	Vector3d r,				// Sat position (ECEF)
	Vector3d v)				// Sat velocity (ECEF)
{
	// Ref: RTCM c10403.3, equation (3.12-5), p188 (this rotation matrix performs RAC->ECEF, so ECEF->RAC is simply the transpose of this)
	Vector3d ea = v.normalized();
	Vector3d rv = r.cross(v);
	Vector3d ec = rv.normalized();
	Vector3d er = ea.cross(ec);

	Matrix3d Rt;
	Rt.row(0) = er;
	Rt.row(1) = ea;
	Rt.row(2) = ec;

	// RAC = Rt * ECEF
	return Rt * vecToRotate;
}

/* satellite position and clock with ssr correction --------------------------*/
int satpos_ssr(
	GTime		time,
	GTime		teph,
	Obs&		obs,
	nav_t&		nav,
	int			opt,
	PcoMapType*	pcoMap_ptr)
{
//     trace(4, __FUNCTION__ ": time=%s sat=%2d\n",time.to_string(3).c_str(),obs.Sat);

	const ssr_t& ssr = obs.satNav_ptr->ssr;
	
	//get 'price is right' closest ssr components to ephemeris time.
	auto ephIt = ssr.ssrEph_map		.lower_bound(time);			if (ephIt == ssr.ssrEph_map		.end())		return 0;
	auto clkIt = ssr.ssrClk_map		.lower_bound(time);			if (clkIt == ssr.ssrClk_map		.end())		return 0;
	auto uraIt = ssr.ssrUra_map		.lower_bound(time);		//	if (uraIt == ssr.ssrUra_map		.end())		return 0;	//check these later
	auto hrcIt = ssr.ssrHRClk_map	.lower_bound(time);		//	if (hrcIt == ssr.ssrHRClk_map	.end())		return 0;
	
	auto& [t_e, ssrEph] = *ephIt;
	auto& [t_c, ssrClk] = *clkIt;

	/* inconsistency between orbit and clock correction */
	if (ssrEph.iod != ssrClk.iod)
	{
//         trace(2,"inconsist ssr correction: %s sat=%2d iod=%d %d\n",
//               time.to_string(0).c_str(),obs.Sat,ssr->iod[0],ssr->iod[1]);
		obs.svh = -1;
		return 0;
	}

	double tEph = timediff(time, ssrEph.t0);
	double tClk = timediff(time, ssrClk.t0);

	/* ssr orbit and clock correction (ref [4]) */
	if 	( fabs(tEph) > MAXAGESSR
		||fabs(tClk) > MAXAGESSR)
	{
//         trace(2,"age of ssr error: %s sat=%2d t=%.0f %.0f\n",time.to_string(0).c_str(),
//               obs.Sat,t1,t2);
		obs.svh = -1;
		return 0;
	}

	if (ssrEph.udi >= 1)		tEph -= ssrEph.udi / 2;
	if (ssrClk.udi >= 1)		tClk -= ssrClk.udi / 2;

	double deph[3];

	for (int i = 0; i < 3; i++)
	{
		deph[i]	= ssrEph.deph [i]
				+ ssrEph.ddeph[i] * tEph;
	}

	double dclk = ssrClk.dclk[0]
				+ ssrClk.dclk[1] * tClk
				+ ssrClk.dclk[2] * tClk * tClk;

	/* ssr highrate clock correction (ref [4]) */
	if (hrcIt != ssr.ssrHRClk_map.end())	
	{
		auto& [t_h, ssrHrc] = *hrcIt;
		
		double tHrc = timediff(time, ssrHrc.t0);
		
		if 	(  ssrEph.iod == ssrHrc.iod
			&& fabs(tHrc) < MAXAGESSR_HRCLK)
		{
			dclk += ssrHrc.hrclk;
		}
	}

	if 	( norm(deph, 3)	> MAXECORSSR
		||fabs(dclk)	> MAXCCORSSR)
	{
//         trace(3,"invalid ssr correction: %s deph=%.1f dclk=%.1f\n",
//               time.to_string(0).c_str(),norm(deph,3),dclk);
		obs.svh = -1;
		return 0;
	}

	/* satellite postion and clock by broadcast ephemeris */
	bool pass = ephpos(time, teph, obs, nav, ssrEph.iode);

	if (pass == false)
		return 0;

	/* satellite clock for gps, galileo and qzss */
	int sys = obs.Sat.sys;

	if	(  sys == +E_Sys::GPS
		|| sys == +E_Sys::GAL
		|| sys == +E_Sys::QZS
		|| sys == +E_Sys::CMP)
	{
		Eph* eph = seleph(teph, obs.Sat, ssrEph.iode, nav);

		if (eph == nullptr)
			return 0;

		/* satellite clock by clock parameters */
		double tk = timediff(time, eph->toc);
		obs.dtSat[0] 	= eph->f0
						+ eph->f1 * tk
						+ eph->f2 * tk * tk;

		obs.dtSat[1] 	= eph->f1
						+ eph->f2 * tk * 2;

		/* relativity correction */
		obs.dtSat[0] -= 2 * obs.rSat.dot(obs.satVel) / CLIGHT / CLIGHT;
	}

	/* radial-along-cross directions in ecef */
												Vector3d ea = obs.satVel.normalized();
	Vector3d rc = obs.rSat.cross(obs.satVel);	Vector3d ec = rc.normalized();
												Vector3d er = ea.cross(ec);

	Vector3d dant = Vector3d::Zero();

	/* satellite antenna offset correction */
	if (opt)
	{
		satantoff(nullStream, time, obs.rSat, obs.Sat, obs.satNav_ptr, dant, pcoMap_ptr);
	}

	obs.rSat += -(	  er * deph[0] 
					+ ea * deph[1] 
					+ ec * deph[2]) + dant;		//todo aaron, change to rotation matrix and generalise

	/* t_corr = t_sv - (dtSat(brdc) + dclk(ssr) / CLIGHT) (ref [10] eq.3.12-7) */
	obs.dtSat[0] += dclk / CLIGHT;

	/* variance by ssr ura */
	double ura = -1;
	if (uraIt != ssr.ssrUra_map.end())
	{
		auto& [t_u, ssrUra] = *uraIt;
		
		ura = ssrUra.ura;
	}
	obs.var = var_urassr(ura);

//     trace(5,"satpos_ssr: %s sat=%2d deph=%6.3f %6.3f %6.3f er=%6.3f %6.3f %6.3f dclk=%6.3f var=%6.3f\n",
//           time.to_string(2).c_str(),obs.Sat,deph[0],deph[1],deph[2],er[0],er[1],er[2],dclk,obs.var);

	return 1;
}


/* satellite position and clock ------------------------------------------------
* compute satellite position, velocity and clock
* args   : gtime_t time     I   time (gpst)
*          gtime_t teph     I   time to select ephemeris (gpst)
*          int    sat       I   satellite number
*          nav_t  *nav      I   navigation data
*          int    ephopt    I   ephemeris option (EPHOPT_???)
*          double *rs       O   sat position and velocity (ecef)
*                               {x,y,z,vx,vy,vz} (m|m/s)
*          double *dts      O   sat clock {bias,drift} (s|s/s)
*          double *var      O   sat position and clock error variance (m^2)
*          int    *svh      O   sat health flag (-1:correction not available)
* return : status (1:ok,0:error)
* notes  : satellite position is referenced to antenna phase center
*          satellite clock does not include code bias correction (tgd or bgd)
*-----------------------------------------------------------------------------*/
int satpos(
	Trace&		trace,
	GTime		time,
	GTime		teph,
	Obs&		obs,
	int			ephopt,
	nav_t&		nav,
	PcoMapType* pcoMap_ptr)
{
	tracepde(4, trace, "satpos  : time=%s sat=%2d ephopt=%d\n", time.to_string(3).c_str(), obs.Sat, ephopt);
	//printf("satpos  : time=%s sat=%2d ephopt=%d\n",time.to_string(3).c_str(),sat,ephopt);

	obs.svh = 0;

	switch (ephopt)
	{
		//todo aaron, remove options, just pass or not the pco pointer.
		//todo aaron, pass through as obs. 
		case E_Ephemeris::BROADCAST:	return ephpos		(time, teph, obs, nav, -1);
		case E_Ephemeris::SSR_APC:		return satpos_ssr	(time, teph, obs, nav,  0, pcoMap_ptr);
		case E_Ephemeris::SSR_COM: 		return satpos_ssr	(time, teph, obs, nav, +1, pcoMap_ptr);
		case E_Ephemeris::PRECISE:
			if (!peph2pos(trace, time, obs.Sat, nav, +1, obs, pcoMap_ptr)) break;
			else return 1;
		case E_Ephemeris::PRECISE_COM:
			if (!peph2pos(trace, time, obs.Sat, nav,  0, obs, pcoMap_ptr)) break;
			else return 1;

//             if (!peph2pos(fp,time,obs.Sat,nav,0,obs,pcoMap_ptr)) break; else return 1;	//todo aaron, turned off pco for debuggin
	}

	obs.svh = -1;
	return 0;
}

/* satellite positions and clocks ----------------------------------------------
* compute satellite positions, velocities and clocks
* args   : gtime_t teph     I   time to select ephemeris (gpst)
*          obsd_t *obs      I   observation data
*          int    n         I   number of observation data
*          nav_t  *nav      I   navigation data
*          int    ephopt    I   ephemeris option (EPHOPT_???)
* return : none
* notes  : if no navigation data, set 0 to rs[], dts[], var[] and svh[]
*          satellite position and clock are values at signal transmission time
*          satellite position is referenced to antenna phase center
*          satellite clock does not include code bias correction (tgd or bgd)
*          any pseudorange and broadcast ephemeris are always needed to get
*          signal transmission time
*-----------------------------------------------------------------------------*/
void satposs(
	Trace&		trace,
	GTime		teph,
	ObsList&	obsList,
	nav_t&		nav,
	int			ephopt)
{
	TestStack ts(__FUNCTION__);

	tracepde(3, trace, "satposs : teph=%s n=%d ephopt=%d\n", teph.to_string(3).c_str(), obsList.size(), ephopt);

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		char id[5];
		obs.Sat.getId(id);

		/* search any psuedorange */
		if (obs.Sigs.size() == 0)
		{
			tracepde(2, trace, "no pseudorange %s sat=%s\n", obs.time.to_string(3).c_str(), id);
			continue;
		}

		double pr = 0;

		for (auto& [a, sig] : obs.Sigs)
		{
			pr = sig.P;
			break;
		}

		/* transmission time by satellite clock */
		GTime time = timeadd(obs.time, -pr / CLIGHT);

		/* satellite clock bias by broadcast ephemeris */
		double dt;
		bool pass = ephclk(time, teph, obs, dt);
		if (pass == false)
		{
			tracepde(2, trace, "no broadcast clock %s sat=%s\n", time.to_string(3).c_str(), id);
//             printf( "no broadcast clock %s sat=%s\n",time.to_string(3).c_str(), id);
			continue;
		}

		time = timeadd(time, -dt);

		/* satellite antenna information */
		PcoMapType* pcoMap_ptr = NULL;
		{
			double ep[6];
			time2epoch(time, ep);
			pcvacs_t* pcsat = findAntenna(id, ep, nav);

			if (pcsat == NULL)
			{
				if (obs.Sat < MINPRNSBS)
				{
					tracepde(1, trace,	"Warning: no satellite (%s) pco information\n", id);
					printf(				"Warning: no satellite (%s) pco information\n", id);
				}
			}
			else
			{
				pcoMap_ptr = &pcsat->pcoMap;
			}
		}
		
		//todo aaron, send through satNav_ptrs rather than the whole set of options
		/* satellite position and clock at transmission time */
		pass = satpos(trace, time, teph, obs, ephopt, nav, pcoMap_ptr);

		if (pass == false)
		{
			tracepde(3, trace, "no ephemeris %s sat=%s\n", time.to_string(3).c_str(), id);

			continue;
		}

		/* if no precise clock available, use broadcast clock instead */
		if (obs.dtSat[0] == 0)
		{
			pass = ephclk(time, teph, obs, obs.dtSat[0]);

			if (pass == false)
			{
				continue;
			}

			obs.dtSat[1]	= 0;
			obs.var			= SQR(STD_BRDCCLK);
		}
	}

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		TestStack ts(obs.Sat);

		char id[5];
		obs.Sat.getId(id);
		tracepde(4, trace, "%s sat=%s rs=%13.3f %13.3f %13.3f dtSat=%12.3f var=%7.3f svh=%02X\n",
				obs.time.to_string(6).c_str(),
				id,
				obs.rSat[0],
				obs.rSat[1],
				obs.rSat[2],
				obs.dtSat[0] * 1E9,
				obs.var,
				obs.svh);

		TestStack::testMat("obs.rSat", obs.rSat.data(), obs.rSat.rows());
	}
}
