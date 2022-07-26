
// #pragma GCC optimize ("O0")


#include "enums.h"
#include "common.hpp"
#include "satSys.hpp"
#include "algebra.hpp"
#include "preceph.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "ephemeris.hpp"
#include "testUtils.hpp"
#include "navigation.hpp"
#include "corrections.hpp"
#include "streamTrace.hpp"
#include "eigenIncluder.hpp"


#define 	NMAX		10

/* constants and macros ------------------------------------------------------*/

#define J2_GLO   1.0826257E-3     ///< 2nd zonal harmonic of geopot   ref [2]

#define OMGE_GLO 7.292115E-5      ///< earth angular velocity (rad/s) ref [2]
#define OMGE_CMP 7.292115E-5      ///< earth angular velocity (rad/s) ref [9]
#define OMGE_GAL 7.2921151467E-5  ///< earth angular velocity (rad/s) ref [7]

#define SIN_5 -0.0871557427476582 ///< sin(-5.0 deg)
#define COS_5  0.9961946980917456 ///< cos(-5.0 deg)

#define ERREPH_GLO 5.0            ///< error of glonass ephemeris (m)
#define TSTEP    60.0             ///< integration step glonass ephemeris (s)
#define RTOL_KEPLER 1E-14         ///< relative tolerance for Kepler equation

#define DEFURASSR 0.03            ///< default accurary of ssr corr (m)
#define MAXECORSSR 10.0           ///< max orbit correction of ssr (m) 
#define MAXCCORSSR (1E-6*CLIGHT)  ///< max clock correction of ssr (m) 
#define STD_BRDCCLK 30.0          ///< error of broadcast clock (m)

#define MAX_ITER_KEPLER 30        ///< max number of iteration of Kelpler

/** variance by ura ephemeris (ref [1] 20.3.3.3.1.1)
 */
double var_uraeph(
	int ura)
{
	const double ura_value[] =
	{
		2.4, 3.4, 4.85, 6.85, 9.65, 13.65, 24.0, 48.0, 96.0, 192.0, 384.0, 768.0, 1536.0, 3072.0, 6144.0
	};
	
	return ura < 0 || 15 < ura ? SQR(6144.0) : SQR(ura_value[ura]);
}

/** variance by ura ssr (ref [4])
 */
double var_urassr(
	int ura)
{
	if (ura <= 0)		return SQR(DEFURASSR);
	if (ura >= 63)		return SQR(5.4665);

	double std = (pow(3, (ura >> 3) & 7) * (1.0 + (ura & 7) / 4.0) - 1.0) * 1E-3;
	return SQR(std);
}

/** broadcast ephemeris to satellite clock bias 
* compute satellite clock bias with broadcast ephemeris (gps, galileo, qzss)
* notes  : see ref [1],[7],[8]
*          satellite clock does not include relativity correction and tdg
*/
double eph2clk(
	GTime	time, 		///< time by satellite clock (gpst)
	Eph&	eph)		///< broadcast ephemeris
{
//     trace(4,__FUNCTION__ " : time=%s sat=%2d\n",time.to_string(3).c_str(),eph->Sat);
	double t = time - eph.toc;

	for (int i = 0; i < 2; i++)
	{
		t	-= eph.f0
			+  eph.f1 * t
			+  eph.f2 * t * t;
	}

	double ans	= eph.f0
				+ eph.f1 * t
				+ eph.f2 * t * t;
	return ans;
}

/** broadcast ephemeris to satellite position and clock bias 
* compute satellite position and clock bias with broadcast ephemeris (gps, galileo, qzss)
* 
* notes  : see ref [1],[7],[8]
*          satellite clock includes relativity correction without code bias
*          (tgd or bgd)
*/
void eph2pos(
	GTime		time,						///< time (gpst)
	Eph&		eph,						///< broadcast ephemeris
	Vector3d&	rSat,						///< satellite position (ecef) {x,y,z} (m)
	double&		dtSat,						///< satellite clock bias (s)
	double*		var_ptr			= nullptr,	///< satellite position and clock variance (m^2)
	bool		applyRelativity	= true)		///< apply relativity to clock
{
//     trace(4, __FUNCTION__ " : time=%s sat=%2d\n",time.to_string(3).c_str(),eph->Sat);

	if (eph.A <= 0)
	{
		rSat 	= Vector3d::Zero();
		dtSat	= 0;
		
		if (var_ptr)
			*var_ptr = 0;
			
		return;
	}

	double tk = time - eph.toe;
	int prn = eph.Sat.prn;
	int sys = eph.Sat.sys;

	double mu;
	double omge;
	switch (sys)
	{
		case E_Sys::GAL:	mu = MU_GAL;	omge = OMGE_GAL;	break;
		case E_Sys::BDS:	mu = MU_CMP;	omge = OMGE_CMP;	break;
		default:			mu = MU_GPS;	omge = OMGE;		break;
	}

	double M = eph.M0 + (sqrt(mu / (eph.A * eph.A * eph.A)) + eph.deln) * tk;

	double E	= M;
	double Ek	= 0;
	int n;
	for (n = 0; fabs(E - Ek) > RTOL_KEPLER && n < MAX_ITER_KEPLER; n++)
	{
		Ek = E;
		E -= (E - eph.e * sin(E) - M) / (1 - eph.e * cos(E));
	}

	if (n >= MAX_ITER_KEPLER)
	{
//         trace(2,"kepler iteration overflow sat=%2d\n",eph->Sat);
		return;
	}

	double sinE = sin(E);
	double cosE = cos(E);

//     trace(4,"kepler: sat=%2d e=%8.5f n=%2d del=%10.3e\n",eph->Sat,eph->e,n,E-Ek);

	double u	= atan2(sqrt(1 - eph.e * eph.e) * sinE, cosE - eph.e) + eph.omg;
	double r	= eph.A * (1 - eph.e * cosE);
	double i	= eph.i0
				+ eph.idot * tk;
				
	double sin2u = sin(2 * u);
	double cos2u = cos(2 * u);

	u += eph.cus * sin2u + eph.cuc * cos2u;		//argument of latitude
	r += eph.crs * sin2u + eph.crc * cos2u;		//radius
	i += eph.cis * sin2u + eph.cic * cos2u;		//inclination

	double x	= r * cos(u);
	double y	= r * sin(u);
	double cosi = cos(i);

	/* beidou geo satellite (ref [9]), prn range may change in the future */
	if	( ( sys == +E_Sys::BDS)
		&&( prn <= 5
		  ||prn >= 59))
	{
		double O	= eph.OMG0
					+ eph.OMGd * tk
					- omge * eph.toes;
					
		double sinO = sin(O);
		double cosO = cos(O);
		
		double xg = x * cosO - y * cosi * sinO;
		double yg = x * sinO + y * cosi * cosO;
		double zg = y * sin(i);
		
		double sino = sin(omge * tk);
		double coso = cos(omge * tk);
		
		rSat[0] = +xg * coso	+ yg * sino * COS_5	+ zg * sino * SIN_5;
		rSat[1] = -xg * sino	+ yg * coso * COS_5	+ zg * coso * SIN_5;
		rSat[2] = -yg * SIN_5						+ zg * COS_5;
	}
	else
	{
		double O	=  eph.OMG0
					+ (eph.OMGd - omge) * tk
					- omge * eph.toes;
						
		double sinO = sin(O);
		double cosO = cos(O);
		
		rSat[0] = x * cosO - y * cosi * sinO;
		rSat[1] = x * sinO + y * cosi * cosO;
		rSat[2] = y * sin(i);
	}

	tk = time - eph.toc;
	dtSat	= eph.f0
			+ eph.f1 * tk
			+ eph.f2 * tk * tk;

	/* relativity correction */
	if (applyRelativity)
	{
		dtSat	-= 2 * sqrt(mu * eph.A) * eph.e * sinE / SQR(CLIGHT); //is equivalent to - 2 * obs.rSat.dot(obs.satVel) / CLIGHT; 
	}

	/* position and clock error variance */
	if (var_ptr)
		*var_ptr = var_uraeph(eph.sva);
}

/* glonass orbit differential equations --------------------------------------*/
void deq(
	const	double* x, 
			double* xdot, 
	Vector3d& acc)
{
	double r2	= dot(x, x, 3);
	double r3	= r2 * sqrt(r2);
	double omg2	= SQR(OMGE_GLO);

	if (r2 <= 0)
	{
		xdot[0] = 0;
		xdot[1] = 0;
		xdot[2] = 0;
		xdot[3] = 0;
		xdot[4] = 0;
		xdot[5] = 0;
		
		return;
	}

	/* ref [2] A.3.1.2 with bug fix for xdot[4],xdot[5] */
	double a = 1.5 * J2_GLO * MU_GLO * SQR(RE_GLO) / r2 / r3;	/* 3/2*J2*mu*Ae^2/r^5 */
	double b = 5.0 * SQR(x[2]) / r2;							/* 5*z^2/r^2 */
	double c = -MU_GLO / r3 - a * (1 - b);						/* -mu/r^3-a(1-b) */
	
	xdot[0] = x[3];
	xdot[1] = x[4];
	xdot[2] = x[5];
	
	xdot[3] = (c + omg2) * x[0] + 2 * OMGE_GLO * x[4] + acc[0];
	xdot[4] = (c + omg2) * x[1] - 2 * OMGE_GLO * x[3] + acc[1];
	xdot[5] = (c - 2 * a) * x[2] + acc[2];
}

/* glonass position and velocity by numerical integration --------------------*/
void glorbit(
	double t, 
	double* x, 
	Vector3d& acc)
{
	double k1[6];
	double k2[6];
	double k3[6];
	double k4[6];
	double w [6];

	deq(x, k1, acc);	for (int i = 0; i < 6; i++) 		w[i] = x[i] + k1[i] * t / 2;
	deq(w, k2, acc);	for (int i = 0; i < 6; i++) 		w[i] = x[i] + k2[i] * t / 2;
	deq(w, k3, acc);	for (int i = 0; i < 6; i++) 		w[i] = x[i] + k3[i] * t;
	deq(w, k4, acc);

	for (int i = 0; i < 6; i++)
		x[i] += (k1[i] + 2 * k2[i] + 2 * k3[i] + k4[i]) * t / 6;
}

/* glonass ephemeris to satellite clock bias -----------------------------------
* compute satellite clock bias with glonass ephemeris
* args   : gtime_t time     I   time by satellite clock (gpst)
*          Geph *geph     I   glonass ephemeris
* return : satellite clock bias (s)
* notes  : see ref [2]
*-----------------------------------------------------------------------------*/
double geph2clk(
	GTime time, 
	Geph& geph)
{
//     trace(4,__FUNCTION__ ": time=%s sat=%2d\n",time.to_string(3).c_str(),geph->Sat);

	double t = time - geph.toe;

	for (int i = 0; i < 2; i++)
	{
		t -=  geph.taun 
			+ geph.gamn * t;
	}

	return	  geph.taun 
			+ geph.gamn * t;
}

/** glonass ephemeris to satellite position and clock bias.
* compute satellite position and clock bias with glonass ephemeris
* -*/
void geph2pos(
	GTime		time,			///< time (gpst)
	Geph&		geph, 			///< glonass ephemeris
	Vector3d&	rSat,			///< satellite position {x,y,z} (ecef) (m)
	double&		dts, 			///< satellite clock bias (s)
	double*		var = nullptr)	///< satellite position and clock variance (m^2)
{
//     trace(4, __FUNCTION__ ": time=%s sat=%2d\n",time.to_string(3).c_str(),geph->Sat);

	double t = time - geph.toe;

	dts = geph.taun 
		+ geph.gamn * t;

	double x[6];
	for (int i = 0; i < 3; i++)
	{
		x[i    ] = geph.pos[i];
		x[i + 3] = geph.vel[i];
	}

	for (double tt = t < 0 ? -TSTEP : TSTEP; fabs(t) > 1E-9; t -= tt)
	{
		if (fabs(t) < TSTEP)
			tt = t;

		glorbit(tt, x, geph.acc);
	}

	for (int i = 0; i < 3; i++)
		rSat[i] = x[i];

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
double seph2clk(
	GTime time,
	Seph& seph)
{
//     trace(4,__FUNCTION__ ": time=%s sat=%2d\n",time.to_string(3).c_str(),seph->Sat);

	double t = time - seph.t0;

	for (int i = 0; i < 2; i++)
	{
		t -=  seph.af0
			+ seph.af1 * t;
	}

	return	  seph.af0
			+ seph.af1 * t;
}

/** sbas ephemeris to satellite position and clock bias -------------------------
* compute satellite position and clock bias with sbas ephemeris
*/
void seph2pos(
	GTime		time,			///< time (gpst)
	Seph&		seph,			///< sbas ephemeris
	Vector3d&	rSat,			///< satellite position {x,y,z} (ecef) (m)
	double&		dtSat,			///< satellite clock bias (s)
	double*		var = nullptr)	///< satellite position and clock variance (m^2)
{
//     trace(4, __FUNCTION__ ": time=%s sat=%2d\n",time.to_string(3).c_str(),seph->Sat);

	double t = time - seph.t0;

	for (int i = 0; i < 3; i++)
	{
		rSat[i]	= seph.pos[i]
				+ seph.vel[i] * t
				+ seph.acc[i] * t * t / 2;
	}

	dtSat	= seph.af0
			+ seph.af1 * t;

	if (var)
		*var = var_uraeph(seph.sva);
}

template<typename TYPE>
void cullEphMap(
	GTime	time,
	TYPE&	map)
{
	for (auto& [satid, satEphMap] : map)
	{
		SatSys Sat;
		Sat.fromHash(satid);
		
		double tmax;
		switch (Sat.sys)
		{
			case E_Sys::QZS:	tmax = MAXDTOE_QZS	+ 1; break;
			case E_Sys::GAL:	tmax = MAXDTOE_GAL	+ 1; break;
			case E_Sys::BDS:	tmax = MAXDTOE_CMP	+ 1; break;
			default: 			tmax = MAXDTOE		+ 1; break;
		}
		
		for (auto it = satEphMap.begin(); it != satEphMap.end(); )
		{
			auto& [ephtime, eph] = *it;
			
			if (ephtime < time - tmax)
			{
				it = satEphMap.erase(it);
			}
			else
			{
				++it;
			}
		}
	}
}

template<typename TYPE>
void cullSSRMap(
	GTime	time,
	TYPE&	map)
{
	for (auto it = map.begin(); it != map.end(); )
	{
		auto& [ssrtime, ssr] = *it;
		
		if (ssr.t0 < time - ssr.udi * acsConfig.validity_interval_factor)
		{
			it = map.erase(it);
		}
		else
		{
			++it;
		}
	}
}

void cullOldSSRs(
	GTime	time)
{
	for (auto& [satid, satNav] : nav.satNavMap)
	{
		cullSSRMap(time, satNav.receivedSSR.ssrCodeBias_map);
		cullSSRMap(time, satNav.receivedSSR.ssrPhasBias_map);
		cullSSRMap(time, satNav.receivedSSR.ssrClk_map);
		cullSSRMap(time, satNav.receivedSSR.ssrEph_map);
		cullSSRMap(time, satNav.receivedSSR.ssrHRClk_map);
		cullSSRMap(time, satNav.receivedSSR.ssrUra_map);
	}
}

void cullOldEphs(
	GTime	time)
{
	cullEphMap (time, nav.ephMap);
	cullEphMap (time, nav.gephMap);
	cullEphMap (time, nav.sephMap);
	for (auto& [a,b] : nav.cephMap)
	{
		cullEphMap (time, b);
	}
}

/** select ephememeris
 */
template<typename EPHTYPE>
EPHTYPE* seleph(
	Trace&														trace,
	GTime														time, 
	SatSys														Sat, 
	int															iode, 
	map<int, map<GTime, EPHTYPE,	std::greater<GTime>>>&		ephMap)
{
//     trace(4,__FUNCTION__ " : time=%s sat=%2d iode=%d\n",time.to_string(3).c_str(),Sat,iode);

	double tmax;
	switch (Sat.sys)
	{
		case E_Sys::QZS:	tmax = MAXDTOE_QZS	+ 1; break;
		case E_Sys::GAL:	tmax = MAXDTOE_GAL	+ 1; break;
		case E_Sys::BDS:	tmax = MAXDTOE_CMP	+ 1; break;
		case E_Sys::GLO:	tmax = MAXDTOE_GLO	+ 1; break;
		case E_Sys::SBS:	tmax = MAXDTOE_SBS	+ 1; break;
		default: 			tmax = MAXDTOE		+ 1; break;
	}

	auto& satEphMap = ephMap[Sat];

	if (iode >= 0)
	{
		for (auto& [dummy, eph] : satEphMap)
		{
			if (iode != eph.iode)
			{
				continue;
			}
			
			return &eph;
		}
	
        tracepdeex(5, trace, "no broadcast ephemeris: %s sat=%s with iode=%3d\n", time.to_string(0).c_str(), Sat.id().c_str(), iode);
		return nullptr;
	}
	
	auto it = satEphMap.lower_bound(time + tmax);
	if (it == satEphMap.end())
	{
        tracepdeex(5, trace, "no broadcast ephemeris: %s sat=%s within MAXDTOE+ ", time.to_string(0).c_str(), Sat.id().c_str());
    	if (satEphMap.empty() == false)
    	{
    		tracepdeex(5, trace, " last is %s", satEphMap.begin()->first.to_string(0).c_str());
    	}
    	tracepdeex(5, trace, "\n");
		return nullptr;
	}
	
	auto& [ephTime, eph] = *it;
	
	if (fabs(eph.toe - time) > tmax)
	{
        tracepdeex(5, trace, "no broadcast ephemeris: %s sat=%s within MAXDTOE-\n", time.to_string(0).c_str(), Sat.id().c_str());
		return nullptr;
	}
	
	return &eph;
}

/** select CNVX ephememeris
 */
template<typename EPHTYPE>
EPHTYPE* seleph(
	Trace&																	trace,
	GTime																	time, 
	SatSys																	Sat, 
	E_NavMsgType															type,
	int																		iode, 
	map<int, map<E_NavMsgType,	map<GTime, EPHTYPE,	std::greater<GTime>>>>&	ephMap)
{
//     trace(4,__FUNCTION__ " : time=%s sat=%2d iode=%d\n",time.to_string(3).c_str(),Sat,iode);

	if	( Sat.sys != +E_Sys::GPS
		&&Sat.sys != +E_Sys::QZS
		&&Sat.sys != +E_Sys::BDS)
	{
		tracepdeex(5, trace, "invalid satellite system for CNVX message type: sys=%s type=%s\n", Sat.sys._to_string(), type._to_string());
		return nullptr;
	}

	double tmax;
	switch (Sat.sys)
	{
		case E_Sys::QZS:	tmax = MAXDTOE_QZS	+ 1; break;
		case E_Sys::BDS:	tmax = MAXDTOE_CMP	+ 1; break;
		default: 			tmax = MAXDTOE		+ 1; break;
	}

	auto& satEphMap = ephMap[Sat][type];

	if (iode >= 0)
	{
		for (auto& [dummy, eph] : satEphMap)
		{
			if (iode != eph.iode)
			{
				continue;
			}
			
			return &eph;
		}
	
        tracepdeex(5, trace, "no broadcast ephemeris (CNVX): %s sat=%s with iode=%3d\n", time.to_string(0).c_str(), Sat.id().c_str(), iode);
		return nullptr;
	}
	
	auto it = satEphMap.lower_bound(time + tmax);
	if (it == satEphMap.end())
	{
        tracepdeex(5, trace, "no broadcast ephemeris (CNVX): %s sat=%s within MAXDTOE+ ", time.to_string(0).c_str(), Sat.id().c_str());
    	if (satEphMap.empty() == false)
    	{
    		tracepdeex(5, trace, " last is %s", satEphMap.begin()->first.to_string(0).c_str());
    	}
    	tracepdeex(5, trace, "\n");
		return nullptr;
	}
	
	auto& [ephTime, eph] = *it;
	
	if (fabs(eph.toe - time) > tmax)
	{
        tracepdeex(5, trace, "no broadcast ephemeris (CNVX): %s sat=%s within MAXDTOE-\n", time.to_string(0).c_str(), Sat.id().c_str());
		return nullptr;
	}
	
	return &eph;
}

/** select EOP/ION messages
 */
template<typename EPHTYPE>
EPHTYPE* seleph(
	Trace&																		trace,
	GTime																		time, 
	E_Sys																		sys, 
	E_NavMsgType																type,
	map<E_Sys, map<E_NavMsgType,	map<GTime, EPHTYPE,	std::greater<GTime>>>>&	ephMap)
{
//     trace(4,__FUNCTION__ " : time=%s sat=%2d iode=%d\n",time.to_string(3).c_str(),Sat,iode);

	auto& satEphMap = ephMap[sys][type];

	auto it = satEphMap.lower_bound(time);
	if (it == satEphMap.end())
	{
        tracepdeex(5, trace, "no broadcast ephemeris (EOP/ION): %s sys=%s", time.to_string(0).c_str(), sys._to_string());
    	if (satEphMap.empty() == false)
    	{
    		tracepdeex(5, trace, " last is %s", satEphMap.begin()->first.to_string(0).c_str());
    	}
    	tracepdeex(5, trace, "\n");
		return nullptr;
	}
	
	auto& [ephTime, eph] = *it;
	
	return &eph;
}

template<>
Eph* seleph<Eph>(
	Trace&		trace,
	GTime		time, 
	SatSys		Sat, 
	int			iode, 
	Navigation&	nav)
{
	return seleph(trace, time, Sat, iode, nav.ephMap);
}

template<>
Geph* seleph<Geph>(
	Trace&		trace,
	GTime		time, 
	SatSys		Sat, 
	int			iode, 
	Navigation&	nav)
{
	return seleph(trace, time, Sat, iode, nav.gephMap);
}

template<>
Seph* seleph<Seph>(
	Trace&		trace,
	GTime		time, 
	SatSys		Sat, 
	int			iode, 
	Navigation&	nav)
{
	return seleph(trace, time, Sat, -1, nav.sephMap);
}

template<>
Ceph* seleph<Ceph>(
	Trace&			trace,
	GTime			time, 
	SatSys			Sat, 
	E_NavMsgType	type,
	int				iode, 
	Navigation&		nav)
{
	return seleph(trace, time, Sat, type, iode, nav.cephMap);
}

template<>
ION* seleph<ION>(
	Trace&			trace,
	GTime			time, 
	E_Sys			sys, 
	E_NavMsgType	type,
	Navigation&		nav)
{
	return seleph(trace, time, sys, type, nav.ionMap);
}

template<>
EOP* seleph<EOP>(
	Trace&			trace,
	GTime			time, 
	E_Sys			sys, 
	E_NavMsgType	type,
	Navigation&		nav)
{
	return seleph(trace, time, sys, type, nav.eopMap);
}

/* satellite clock with broadcast ephemeris ----------------------------------*/
int ephclk(
	GTime	time,
	GTime	teph,
	Obs&	obs,
	double&	dts)
{
	int sys;

//     trace(4,__FUNCTION__ "  : time=%s sat=%2d\n",time.to_string(3).c_str(),obs.Sat);

	sys = obs.Sat.sys;

	//todo aaron, need to check ephemeris timeout. with teph
	switch (sys)
	{
		case E_Sys::GPS:
		case E_Sys::GAL:
		case E_Sys::QZS:
		case E_Sys::BDS:	{	if (!obs.satNav_ptr->eph_ptr)	return 0;	dts = eph2clk (time, *obs.satNav_ptr->eph_ptr);		break;	}
		case E_Sys::GLO:	{	if (!obs.satNav_ptr->geph_ptr)	return 0;	dts = geph2clk(time, *obs.satNav_ptr->geph_ptr);	break;	}
		case E_Sys::SBS:	{	if (!obs.satNav_ptr->seph_ptr)	return 0;	dts = seph2clk(time, *obs.satNav_ptr->seph_ptr);	break;	}
		default:
		{
			return 0;
		}
	}

	return 1;
}



/* satellite position and clock by broadcast ephemeris -----------------------*/
bool ephpos(
	Trace&		trace,
	GTime		time,
	GTime		teph,
	SatSys		Sat,
	Vector3d&	rSat,
	Vector3d&	satVel,
	double*		dtSat,
	double&		ephVar,
	E_Svh&		svh,
	int&		obsIode,
	Navigation&	nav,
	int			iode,
	bool		applyRelativity = true)
{
	Vector3d	rSat_1;
	double		dtSat_1;
	double		tt = 1E-3;

//     trace(4, "%s: time=%s sat=%2d iode=%d\n",__FUNCTION__,time.to_string(3).c_str(),obs.Sat,iode);

	int sys = Sat.sys;

	svh = SVH_UNHEALTHY;

	if	(  sys == +E_Sys::GPS
		|| sys == +E_Sys::GAL
		|| sys == +E_Sys::QZS
		|| sys == +E_Sys::BDS)
	{
		Eph* eph_ptr	= seleph<Eph>(trace, teph, Sat, iode, nav);

		if (eph_ptr == nullptr)
			return false;
		
		auto& eph = *eph_ptr;

									eph2pos(time, 	eph, 	rSat, 	dtSat[0], 	&ephVar,	applyRelativity);
		time = time + tt;			eph2pos(time,	eph, 	rSat_1,	dtSat_1,	nullptr,	applyRelativity);
		
		svh		= eph.svh;
		obsIode	= eph.iode;
	}
	else if (sys == +E_Sys::GLO)
	{
		Geph* geph_ptr	= seleph<Geph>(trace, teph, Sat, iode, nav);

		if (geph_ptr == nullptr)
			return false;
		
		auto& geph = *geph_ptr;

									geph2pos(time,	geph,	rSat,	dtSat[0],	&ephVar);
		time = time + tt;			geph2pos(time,	geph,	rSat_1,	dtSat_1);
		
		svh		= geph.svh;
		obsIode	= geph.iode;
	}
	else if (sys == +E_Sys::SBS)
	{
		Seph* seph_ptr	= seleph<Seph>(trace, teph, Sat, -1, nav);

		if (seph_ptr == nullptr)
			return 0;
		
		auto& seph = *seph_ptr;

									seph2pos(time,	seph,	rSat,	dtSat[0],	&ephVar);
		time = time + tt;			seph2pos(time,	seph,	rSat_1,	dtSat_1);
		
		svh = seph.svh;
	}
	else
		return false;

	/* satellite velocity and clock drift by differential approx */
	satVel		= (rSat_1	- rSat) 	/ tt;
	dtSat[1]	= (dtSat_1	- dtSat[0])	/ tt;

	return true;
}

bool ephpos(
	Trace&		trace,
	GTime		time,
	GTime		teph,
	Obs&		obs,
	Navigation&	nav,
	int			iode,
	bool		applyRelativity = true)
{
	return ephpos(
		trace,
		time, 
		teph,
		obs.Sat, 
		obs.rSat, 
		obs.satVel,
		obs.dtSat,
		obs.ephVar, 
		obs.svh,
		obs.iode,
		nav, 
		iode,
		applyRelativity);
}


Matrix3d ecef2rac(
	Vector3d& rSat,				// Sat position (ECEF)
	Vector3d& satVel)			// Sat velocity (ECEF)
{
	// Ref: RTCM c10403.3, equation (3.12-5), p188 (this rotation matrix performs RAC->ECEF, so ECEF->RAC is simply the transpose of this)
											Vector3d ea = satVel.normalized();
	Vector3d rv = rSat.cross(satVel);		Vector3d ec = rv.normalized();
											Vector3d er = ea.cross(ec);

	Matrix3d Rt;
	Rt.row(0) = er;
	Rt.row(1) = ea;
	Rt.row(2) = ec;

	return Rt;
}

Matrix3d rac2ecef(
	Vector3d& rSat,				// Sat position (ECEF)
	Vector3d& satVel)			// Sat velocity (ECEF)
{
	Matrix3d ecef2racMat = ecef2rac(rSat, satVel);
	
	return ecef2racMat.transpose();
}


double relativity1(
	Vector3d& rSat,
	Vector3d& satVel)
{
	return 2 * rSat.dot(satVel) / CLIGHT / CLIGHT;
}

bool ssrPosDelta(
	GTime			time,
	const SSRMaps&	ssrMaps,
	Vector3d&		dPos,
	int&			iodPos,
	int&			iodEph)
{
	//get 'price is right' closest ssr components to ephemeris time.
	auto ephIt = ssrMaps.ssrEph_map.lower_bound(time);
	if (ephIt == ssrMaps.ssrEph_map.end())	
	{
		return false;
	}
	
	auto& [t_e, ssrEph] = *ephIt;
	
	iodPos = ssrEph.iod;
	iodEph = ssrEph.iode;
	
	double tEph = time - ssrEph.t0;

	/* ssr orbit and clock correction (ref [4]) */
	if (fabs(tEph) > ssrEph.udi * acsConfig.validity_interval_factor)
	{
        tracepdeex(2, std::cout, "age of ssr error: %s t=%.0f > %.0f\n",time.to_string(0).c_str(), tEph, ssrEph.udi * acsConfig.validity_interval_factor);
		return false;
	}
	
	if (ssrEph.udi >= 1)	
		tEph -= ssrEph.udi / 2;

	for (int i = 0; i < 3; i++)
	{
		dPos[i]	= ssrEph.deph [i]
				+ ssrEph.ddeph[i] * tEph;
	}

	if (dPos.norm() > MAXECORSSR)
	{
        tracepdeex(2, std::cout,"invalid ssr correction: %s deph=%.1f\n", time.to_string(0).c_str(), dPos.norm());
		return false;
	}
	
	return true;
}

bool ssrClkDelta(
	GTime			time,
	const SSRMaps&	ssrMaps,
	double&			dclk, 
	int&			iodClk)
{
	//get 'price is right' closest ssr components to ephemeris time.
	auto clkIt = ssrMaps.ssrClk_map.lower_bound(time);	
	if (clkIt == ssrMaps.ssrClk_map.end())	
	{
		return false;
	}
	
	auto& [t_c, ssrClk] = *clkIt;
	
	iodClk = ssrClk.iod;
	
	double tClk = time - ssrClk.t0;

	/* ssr orbit and clock correction (ref [4]) */
	if (fabs(tClk) > ssrClk.udi * acsConfig.validity_interval_factor)
	{
//         trace(2,"age of ssr error: %s sat=%2d t=%.0f %.0f\n", time.to_string(0).c_str(), obs.Sat,t1,t2);
		return false;
	}
	
	if (ssrClk.udi >= 1)	
		tClk -= ssrClk.udi / 2;

	dclk	= ssrClk.dclk[0]
			+ ssrClk.dclk[1] * tClk
			+ ssrClk.dclk[2] * tClk * tClk;

	/* ssr highrate clock correction (ref [4]) */
	auto hrcIt = ssrMaps.ssrHRClk_map.lower_bound(time);
	if (hrcIt != ssrMaps.ssrHRClk_map.end())
	{
		auto& [t_h, ssrHrc] = *hrcIt;
		
		double tHrc = time - ssrHrc.t0;
		
		if 	(  ssrClk.iod == ssrHrc.iod
			&& fabs(tHrc) < ssrClk.udi * acsConfig.validity_interval_factor)
		{
			dclk += ssrHrc.hrclk;
		}
	}

	if (fabs(dclk) > MAXCCORSSR)
	{
//         trace(3,"invalid ssr correction: %s dclk=%.1f\n", time.to_string(0).c_str(), dclk);
		return 0;
	}

	return true;
}

/* satellite position and clock with ssr correction --------------------------*/
bool satpos_ssr(
	Trace&			trace,		
	GTime			time,
	GTime			teph,
	Navigation&		nav,
	SSRMaps&		ssrMaps,
	SatSys&			Sat,
	Vector3d&		rSat,
	Vector3d&		satVel,
	double*			dtSat,
	E_Svh&			svh,
	int&			obsIode,
	double&			ephVar,
	bool			applyRelativity = true)
{
//     tracepdeex(4,trace, __FUNCTION__ ": time=%s sat=%2d\n",time.to_string(3).c_str(),obs.Sat);
	svh = SVH_UNHEALTHY;
		
	int			iodPos;
	int			iodEph;
	int			iodClk;
	Vector3d	dPos;
	double		dClk;
	bool posDeltaPass = ssrPosDelta(time, ssrMaps, dPos, iodPos, iodEph);
	bool clkDeltaPass = ssrClkDelta(time, ssrMaps, dClk, iodClk);
	if	(  posDeltaPass == false
		|| clkDeltaPass == false)
	{
		return false;
	}
	
	if (iodClk != iodPos)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: IOD inconsistent." << iodClk << " " << iodPos;
// 		std::cout << "Bad SSR Delta function" << std::endl;
		return false;
	}
	
	/* satellite postion and clock by broadcast ephemeris */
	bool pass = ephpos(trace, time, teph, Sat, rSat, satVel, dtSat, ephVar, svh, obsIode, nav, iodEph);
	
	if (pass == false)
	{
		svh = SVH_UNHEALTHY;
		return false;
	}

	/* satellite clock for gps, galileo and qzss */
	int sys = Sat.sys;

	if	(  sys == +E_Sys::GPS
		|| sys == +E_Sys::GAL
		|| sys == +E_Sys::QZS
		|| sys == +E_Sys::BDS)
	{
		Eph* eph = seleph<Eph>(trace, teph, Sat, iodEph, nav);

		if (eph == nullptr)
		{
			return false;
		}

		/* satellite clock by clock parameters */
		double tk = time - eph->toc;
		
		dtSat[0] 	= eph->f0
					+ eph->f1 * tk
					+ eph->f2 * tk * tk;

		dtSat[1] 	= eph->f1
					+ eph->f2 * tk * 2;
	}

	Matrix3d rac2ecefMat = rac2ecef(rSat, satVel);
	
	Vector3d dPosECEF = rac2ecefMat * dPos;
	
	rSat -= dPosECEF;

	/* t_corr = t_sv - (dtSat(brdc) + dClk(ssr) / CLIGHT) (ref [10] eq.3.12-7) */
	dtSat[0] += dClk / CLIGHT;

	/* variance by ssr ura */
	double ura = -1;
	
	auto uraIt = ssrMaps.ssrUra_map.lower_bound(time);
	if (uraIt != ssrMaps.ssrUra_map.end())
	{
		auto& [t_u, ssrUra] = *uraIt;
		
		ura = ssrUra.ura;
	}
	
	ephVar = var_urassr(ura);

	/* relativity correction */
	if	(applyRelativity)
	{
		dtSat[0] -= relativity1(rSat, satVel);
	}
	
	tracepdeex(5, trace, "%s: %s sat=%2d deph=%6.3f %6.3f %6.3f dclk=%6.3f var=%6.3f\n",
		__FUNCTION__, time.to_string(2).c_str(), Sat, dPos[0], dPos[1], dPos[2], dClk, ephVar);

	svh = SVH_OK;
	return true;
}

bool satpos_ssr(
	Trace&		trace,
	GTime		time,
	GTime		teph,
	Obs&		obs,
	Navigation&	nav,
	bool		applyRelativity = true)
{
	return satpos_ssr(
		trace,		
		time,
		teph,
		nav,
		obs.satNav_ptr->receivedSSR,
		obs.Sat,
		obs.rSat,
		obs.satVel,
		obs.dtSat,
		obs.svh,
		obs.iode,
		obs.ephVar,
		applyRelativity);
}

bool kalmanPos(
	Trace&		trace,
	GTime		time,
	SatSys		Sat,
	Obs&		obs,
	KFState*	kfState_ptr)
{
	if (kfState_ptr == nullptr)
	{
		return false;
	}
	
	auto& kfState = *kfState_ptr;
	
	bool pass = true;
	
	for (short int i = 0; i < 3; i++)
	{
		pass &= kfState.getKFValue(KFKey{.type = KF::SAT_POS,		.Sat = Sat,	.num = i},	obs.rSat[i]);
		pass &= kfState.getKFValue(KFKey{.type = KF::SAT_POS_RATE,	.Sat = Sat, .num = i},	obs.satVel[i]);
	}
	
	return pass;
}

/* satellite position and clock ------------------------------------------------
* compute satellite position, velocity and clock
* return : status (1:ok,0:error)
*          satellite clock does not include code bias correction (tgd or bgd)
*-----------------------------------------------------------------------------*/
int satpos(
	Trace&			trace,				///< Trace to output to
	GTime			time,				///< time (gpst)
	GTime			teph,				///< time to select ephemeris (gpst)
	Obs&			obs,				///< Observation to determine satellite etc, and store answers
	E_Ephemeris		ephType,			///< Source of ephemeris
	E_OffsetType	offsetType,			///< Type of antenna offset to apply
	Navigation&		nav,				///< navigation data
	bool			applyRelativity,	///< Apply relativity
	KFState*		kfState_ptr)		///< Optional pointer to a kalman filter to take values from
{
	tracepdeex(4, trace, "%s: time=%s sat=%s ephType=%d offsetType=%d\n", __FUNCTION__, time.to_string(3).c_str(), obs.Sat.id().c_str(), ephType, offsetType);

	obs.svh = SVH_UNHEALTHY;

	int returnValue = 0;
	switch (ephType)
	{
		case E_Ephemeris::BROADCAST:	returnValue = ephpos		(trace, time, teph,		obs, 	nav,	ANY_IODE,	applyRelativity);	break;
		case E_Ephemeris::SSR:			returnValue = satpos_ssr	(trace, time, teph,		obs, 	nav,				applyRelativity);	break;
		case E_Ephemeris::PRECISE:		returnValue = peph2pos		(trace, time, obs.Sat,	obs, 	nav,				applyRelativity);	break;
		case E_Ephemeris::KALMAN:		returnValue = kalmanPos		(trace, time, obs.Sat,	obs,	kfState_ptr);							break;
		default:						return false;
	}
	
	if 	(ephType == +E_Ephemeris::SSR											&& acsConfig.ssr_input_antenna_offset == +E_OffsetType::UNSPECIFIED)
		BOOST_LOG_TRIVIAL(error) << "Error: ssr_input_antenna_offset has not been set in config.";
	
	double antennaScalar = 0;

	if	(ephType == +E_Ephemeris::SSR		&& offsetType == +E_OffsetType::APC	&& acsConfig.ssr_input_antenna_offset == +E_OffsetType::COM)		antennaScalar = +1;
	if	(ephType == +E_Ephemeris::SSR		&& offsetType == +E_OffsetType::COM	&& acsConfig.ssr_input_antenna_offset == +E_OffsetType::APC)		antennaScalar = -1;
	if	(ephType == +E_Ephemeris::PRECISE	&& offsetType == +E_OffsetType::APC)																	antennaScalar = +1;
	if	(ephType == +E_Ephemeris::KALMAN	&& offsetType == +E_OffsetType::APC)																	antennaScalar = +1;
	if	(ephType == +E_Ephemeris::BROADCAST	&& offsetType == +E_OffsetType::COM)																	antennaScalar = -1;
		
	/* satellite antenna offset correction */
	if	(antennaScalar)
	{
		Vector3d dAnt = Vector3d::Zero();
		
		if (acsConfig.if_antenna_phase_centre)
			     satAntOff(trace, time, obs.rSat, obs.Sat, nav.satNavMap[obs.Sat].lamMap, dAnt, obs.satStat_ptr);
		else
			dAnt=satAntOff(trace, time, obs.rSat, obs.Sat, F1, obs.satStat_ptr);

		obs.rSat += dAnt * antennaScalar;	
	}
	
	return returnValue;
}

/** satellite positions and clocks.
 * satellite position and clock are values at signal transmission time.
 * satellite clock does not include code bias correction (tgd or bgd).
 * any pseudorange and broadcast ephemeris are always needed to get signal transmission time.
 */
void satposs(
	Trace&			trace,				///< Trace to output to
	GTime			teph,				///< time to select ephemeris (gpst)
	ObsList&		obsList,			///< List of observations to complete with satellite positions
	Navigation&		nav,				///< Navigation data
	E_Ephemeris		ephType,			///< Source of ephemeris data
	E_OffsetType	offsetType,			///< Point of satellite to output position of
	bool			applyRelativity,	///< Option to apply relativistic correction to clock
	bool			applyFlightTime)	///< Option to apply offset due to flight time of light signal
{
	TestStack ts(__FUNCTION__);

	tracepdeex(3, trace, "%s: teph=%s n=%d ephType=%d\n", __FUNCTION__, teph.to_string(3).c_str(), obsList.size(), ephType);

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		/* search any pseudorange */
		if (obs.Sigs.empty())
		{
			tracepdeex(2, trace, "no pseudorange %s sat=%s\n", obs.time.to_string(3).c_str(), obs.Sat.id().c_str());
			continue;
		}

		double pr = 0;

		if (applyFlightTime)
		for (auto& [a, sig] : obs.Sigs)
		{
			pr = sig.P;
			break;
		}

		/* transmission time by satellite clock */
		GTime time = obs.time - pr / CLIGHT;

		// satellite clock bias by precise/broadcast ephemeris
		double dt;
		double tvar;
		bool pass = false;
		int ret = pephclk(time, obs.Sat.id(), nav, dt, &tvar);
		
		if (ret > 0)
			pass = true;
		else
			pass = ephclk(time, teph, obs, dt);
			
		if (pass == false)
		{
			tracepdeex(2, trace, "no satellite clock %s sat=%s\n", time.to_string(3).c_str(), obs.Sat.id().c_str());
			continue;
		}

		time = time - dt;

		/* satellite position and clock at transmission time */
		pass = satpos(trace, time, teph, obs, ephType, offsetType, nav, applyRelativity);

		if (pass == false)
		{
			tracepdeex(3, trace, "satpos failed (no ephemeris?) %s sat=%s\n", time.to_string(3).c_str(), obs.Sat.id().c_str());

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
			obs.ephVar		= SQR(STD_BRDCCLK);
		}
	}

	for (auto& obs : obsList)
	{
		if (obs.exclude)
		{
			continue;
		}

		TestStack ts(obs.Sat);

		tracepdeex(4, trace, "%s sat=%s rs=%13.3f %13.3f %13.3f dtSat=%12.3f var=%7.3f svh=%02X\n",
				obs.time.to_string(6).c_str(),
				obs.Sat.id().c_str(),
				obs.rSat[0],
				obs.rSat[1],
				obs.rSat[2],
				obs.dtSat[0] * 1E9,
				obs.ephVar,
				obs.svh);

		TestStack::testMat("obs.rSat", obs.rSat);
	}
}


#if (0)
/* satellite position and clock with sbas correction -------------------------*/
// int satpos_sbas(gtime_t time, gtime_t teph, SatSys Sat, const nav_t* nav,
// 				double* rs, double* dtSat, double* var, int* svh)
// {
// 	const sbssatp_t* sbs;
// 	int i;
// 
// 	trace(4, __FUNCTION__ ": time=%s sat=%2d\n", time.to_string(3).c_str(), Sat);
// 
// 	/* search sbas satellite correciton */
// 	for (i = 0; i < nav->sbssat.nsat; i++)
// 	{
// 		sbs = nav->sbssat.sat + i;
// 
// 		if (sbs->Sat == Sat)
// 			break;
// 	}
// 
// 	if (i >= nav->sbssat.nsat)
// 	{
// 		trace(2, "no sbas correction for orbit: %s sat=%2d\n", time.to_string(0).c_str(), Sat);
// 		ephpos(time, teph, Sat, nav, -1, rs, dts, var, svh);
// 		*svh = -1;
// 
// 		return 0;
// 	}
// 
// 	/* satellite postion and clock by broadcast ephemeris */
// 	if (!ephpos(time, teph, Sat, nav, sbs->lcorr.iode, rs, dts, var, svh))
// 		return 0;
// 
// 	/* sbas satellite correction (long term and fast) */
// 	if (sbssatcorr(time, Sat, nav, rs, dts, var))
// 		return 1;
// 
// 	*svh = -1;
// 
// 	return 0;
// }
#endif
