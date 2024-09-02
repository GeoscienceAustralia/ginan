
#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "trace.hpp"
#include "gTime.hpp"


#define STD_BRDCCLK		30.0				///< error of broadcast clock (m)

#define J2_GLO			1.0826257E-3		///< 2nd zonal harmonic of geopot   ref [2]

#define SIN_5			-0.0871557427476582	///< sin(-5.0 deg)
#define COS_5			+0.9961946980917456	///< cos(-5.0 deg)

#define ERREPH_GLO		5.0					///< error of glonass ephemeris (m)
#define TSTEP			60.0				///< integration step glonass ephemeris (s)
#define RTOL_KEPLER		1E-14				///< relative tolerance for Kepler equation

#define MAX_ITER_KEPLER	30					///< max number of iteration of Kelpler

#define OMGE_GLO		7.292115E-5			///< earth angular velocity (rad/s) ref [2]
#define OMGE_CMP		7.292115E-5			///< earth angular velocity (rad/s) ref [9]
#define OMGE_GAL		7.2921151467E-5		///< earth angular velocity (rad/s) ref [7]

/** variance by ura ephemeris (ref [1] 20.3.3.3.1.1)
*/
double var_uraeph(
	int ura)
{
	const double ura_value[] =
	{
		2.4, 3.4, 4.85, 6.85, 9.65, 13.65, 24.0, 48.0, 96.0, 192.0, 384.0, 768.0, 1536.0, 3072.0, 6144.0
	};

	return ura < 0 || 15 <= ura ? SQR(6144.0) : SQR(ura_value[ura]);
}

/** select EOP/ION messages
*/
template<typename EPHTYPE>
EPHTYPE* selSysEphFromMap(
	Trace&																			trace,
	GTime																			time,
	E_Sys																			sys,
	E_NavMsgType																	type,
	map<E_Sys, map<E_NavMsgType,	map<GTime, EPHTYPE,	std::greater<GTime>>>>&		ephMap)
{
//	trace(4,__FUNCTION__ " : time=%s sat=%2d iode=%d\n",time.to_string(3).c_str(),Sat,iode);

	if	(ephMap.find(sys)	== ephMap.end())		return nullptr;			auto& sysMap	= ephMap[sys];
	if	(sysMap.find(type)	== sysMap.end())		return nullptr;			auto& sysEphMap = sysMap[type];

	auto it = sysEphMap.lower_bound(time);
	if (it == sysEphMap.end())
	{
		tracepdeex(5, trace, "\nno broadcast ephemeris (EOP/ION): %s sys=%s", time.to_string().c_str(), sys._to_string());
		if (sysEphMap.empty() == false)
		{
			tracepdeex(5, trace, " last is %s", sysEphMap.begin()->first.to_string().c_str());
		}
		return nullptr;
	}

	auto& [ephTime, eph] = *it;

	return &eph;
}

/** select ephememeris
*/
template<typename EPHTYPE>
EPHTYPE* selSatEphFromMap(
	Trace&																		trace,
	GTime																		time,
	SatSys																		Sat,
	E_NavMsgType																type,
	int&																		iode,
	map<SatSys, map<E_NavMsgType,	map<GTime, EPHTYPE,	std::greater<GTime>>>>&	ephMap)
{
//	trace(4,__FUNCTION__ " : time=%s sat=%2d iode=%d\n",time.to_string(3).c_str(),Sat,iode);

	double tmax;
	switch (Sat.sys)
	{
		case E_Sys::GAL:	tmax = MAXDTOE_GAL;	break;
		case E_Sys::QZS:	tmax = MAXDTOE_QZS;	break;
		case E_Sys::BDS:	tmax = MAXDTOE_CMP;	break;
		case E_Sys::GLO:	tmax = MAXDTOE_GLO;	break;
		case E_Sys::SBS:	tmax = MAXDTOE_SBS;	break;
		default: 			tmax = MAXDTOE;		break;
	}

	auto& satEphMap = ephMap[Sat][type];

	if (iode >= 0)
	{
		for (auto& [dummy, eph] : satEphMap)
		{
			if	( iode != eph.iode
				||fabs((eph.toe - time).to_double()) > tmax)
			{
				continue;
			}

			return &eph;
		}

		tracepdeex(5, trace, "\nno broadcast ephemeris: %s sat=%s with iode=%3d", time.to_string().c_str(), Sat.id().c_str(), iode);

		return nullptr;
	}

	auto it = satEphMap.lower_bound(time + tmax);
	if (it == satEphMap.end())
	{
		tracepdeex(5, trace, "\nno broadcast ephemeris: %s sat=%s within MAXDTOE+ ", time.to_string().c_str(), Sat.id().c_str());
		if (satEphMap.empty() == false)
		{
			tracepdeex(5, trace, " last is %s", satEphMap.begin()->first.to_string().c_str());
		}

		return nullptr;
	}

	auto& [ephTime, eph] = *it;

	if (fabs((eph.toe - time).to_double()) > tmax)
	{
		tracepdeex(5, trace, "\nno broadcast ephemeris: %s sat=%s within MAXDTOE-", time.to_string().c_str(), Sat.id().c_str());

		return nullptr;
	}

	iode = eph.iode;

	return &eph;
}


template<>	Eph*	seleph<Eph>	(Trace& trace, GTime time, SatSys Sat,	E_NavMsgType type, int iode,	Navigation& nav){	return selSatEphFromMap(trace, time, Sat, type,	iode,	nav.ephMap);	}
template<>	Geph*	seleph<Geph>(Trace& trace, GTime time, SatSys Sat,	E_NavMsgType type, int iode,	Navigation& nav){	return selSatEphFromMap(trace, time, Sat, type,	iode,	nav.gephMap);	}
template<>	Seph*	seleph<Seph>(Trace& trace, GTime time, SatSys Sat,	E_NavMsgType type, int iode,	Navigation& nav){	return selSatEphFromMap(trace, time, Sat, type,	iode,	nav.sephMap);	}
template<>	ION*	seleph<ION>	(Trace& trace, GTime time, E_Sys sys,	E_NavMsgType type,				Navigation& nav){	return selSysEphFromMap(trace, time, sys, type,			nav.ionMap);	}
template<>	EOP*	seleph<EOP>	(Trace& trace, GTime time, E_Sys sys,	E_NavMsgType type,				Navigation& nav){	return selSysEphFromMap(trace, time, sys, type,			nav.eopMap);	}



/** glonass orbit differential equations
 */
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









/** broadcast ephemeris to satellite clock bias
* compute satellite clock bias with broadcast ephemeris (gps, galileo, qzss)
* satellite clock does not include relativity correction and tdg
*/
double eph2Clk(GTime time, Eph& eph,	int recurse = 0)
{
	double t = (time - eph.toe).to_double();					t -= recurse ? eph2Clk(time - t, eph,	recurse - 1) : 0;

	return 	+ eph.f0
			+ eph.f1		* t
			+ eph.f2		* t * t;
}

/** glonass ephemeris to satellite clock bias
 * satellite clock includes relativity correction
 */
double eph2Clk(GTime time, Geph& geph,	int recurse = 0)
{
	double t = (time - geph.toe).to_double();					t -= recurse ? eph2Clk(time - t, geph,	recurse - 1) : 0;

	return	- geph.taun
			+ geph.gammaN	* t;
}

/** sbas ephemeris to satellite clock bias
*/
double eph2Clk(GTime time, Seph& seph,	int recurse = 0)
{
	double t = (time - seph.toe).to_double();					t -= recurse ? eph2Clk(time - t, seph,	recurse - 1) : 0;

	return	+ seph.af0
			+ seph.af1		* t;
}





/** broadcast ephemeris to satellite position and clock bias
* compute satellite position and clock bias with broadcast ephemeris (gps, galileo, qzss)
* satellite clock includes relativity correction without code bias (tgd or bgd)
*/
void eph2Pos(
	GTime		time,						///< time (gpst)
	Eph&		eph,						///< broadcast ephemeris
	Vector3d&	rSat,						///< satellite position (ecef) {x,y,z} (m)
	double*		var_ptr			= nullptr)	///< satellite position and clock variance (m^2)
{
//	trace(4, __FUNCTION__ " : time=%s sat=%2d\n",time.to_string(3).c_str(),eph->Sat);

	if (eph.A <= 0)
	{
		rSat 	= Vector3d::Zero();

		if (var_ptr)
			*var_ptr = 0;

		return;
	}

	double tk = (time - eph.toe).to_double();
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
        printf("kepler iteration overflow sat=%s\n", eph.Sat.id().c_str());
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

	/* position and clock error variance */
	if (var_ptr)
		*var_ptr = var_uraeph(eph.sva);
}


/** glonass ephemeris to satellite position and clock bias.
* compute satellite position and clock bias with glonass ephemeris
*/
void eph2Pos(
	GTime		time,			///< time (gpst)
	Geph&		geph, 			///< glonass ephemeris
	Vector3d&	rSat,			///< satellite position {x,y,z} (ecef) (m)
	double*		var = nullptr)	///< satellite position and clock variance (m^2)
{
//	trace(4, __FUNCTION__ ": time=%s sat=%2d\n",time.to_string(3).c_str(),geph->Sat);

	double t = (time - geph.toe).to_double();

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

/** sbas ephemeris to satellite position and clock bias
* compute satellite position and clock bias with sbas ephemeris
*/
void eph2Pos(
	GTime		time,			///< time (gpst)
	Seph&		seph,			///< sbas ephemeris
	Vector3d&	rSat,			///< satellite position {x,y,z} (ecef) (m)
	double*		var = nullptr)	///< satellite position and clock variance (m^2)
{
//	trace(4, __FUNCTION__ ": time=%s sat=%2d\n",time.to_string(3).c_str(),seph->Sat);

	double t = (time - seph.t0).to_double();

	for (int i = 0; i < 3; i++)
	{
		rSat[i]	= seph.pos[i]
				+ seph.vel[i] * t
				+ seph.acc[i] * t * t / 2;
	}

	if (var)
		*var = var_uraeph(seph.sva);
}






/* satellite clock by broadcast ephemeris
 */
template<typename TYPE>
bool satClkBroadcast(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatSys			Sat,
	double&			satClk,
	double&			satClkVel,
	double&			ephVar,
	bool&			ephClkValid,
	int&			iode,
	Navigation&		nav)
{
	double		satClk1;
	double		tt = 1E-3;

	ephVar = SQR(STD_BRDCCLK);

//	trace(4, "%s: time=%s sat=%2d iode=%d\n",__FUNCTION__,time.to_string(3).c_str(),obs.Sat,iode);

	int sys = Sat.sys;

	ephClkValid = false;

	auto type = acsConfig.used_nav_types[Sat.sys];

	auto eph_ptr = seleph<TYPE>(trace, teph, Sat, type, iode, nav);

	if (eph_ptr == nullptr)
	{
		tracepdeex(2, trace, "Could not find Broadcast Ephemeris for sat: %s, %s\n", Sat.id().c_str(), teph.to_string().c_str());
		return false;
	}

	auto& eph = *eph_ptr;

	satClk	= eph2Clk(time, 		eph);
	satClk1	= eph2Clk(time + tt,	eph);

	if (eph.svh == E_Svh::SVH_OK)
	{
		ephClkValid = true;
	}

	iode	= eph.iode;

	/* satellite velocity and clock drift by differential approx */
	satClkVel = (satClk1 - satClk) / tt;

	return true;
}



/* satellite position by broadcast ephemeris
 */
template<typename TYPE>
bool satPosBroadcast(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatSys			Sat,
	Vector3d&		rSat,
	Vector3d&		satVel,
	double&			ephVar,
	bool&			ephPosValid,
	int&			iode,
	Navigation&		nav)
{
	Vector3d	rSat1;
	Vector3d	rSat2;
	double		tt = 10e-3;

//	trace(4, "%s: time=%s sat=%2d iode=%d\n",__FUNCTION__,time.to_string(3).c_str(),obs.Sat,iode);

	int sys = Sat.sys;

	auto type = acsConfig.used_nav_types[Sat.sys];

	auto eph_ptr = seleph<TYPE>(trace, teph, Sat, type, iode, nav);

	if (eph_ptr == nullptr)
	{
		tracepdeex(2, trace, "Could not find Broadcast Ephemeris for sat: %s, %s\n", Sat.id().c_str(), teph.to_string().c_str());
		return false;
	}

	auto& eph = *eph_ptr;

	eph2Pos(time - tt, eph, rSat1, &ephVar);
	eph2Pos(time + tt, eph, rSat2);

	if (eph.svh == E_Svh::SVH_OK)
	{
		ephPosValid = true;
	}

	iode	= eph.iode;

	/* satellite velocity and clock drift by differential approx */
	rSat	= (rSat2 + rSat1) /  2;
	satVel	= (rSat2 - rSat1) / (2 * tt);

	return true;
}

/* satellite clock by broadcast ephemeris
 */
bool satClkBroadcast(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatSys			Sat,
	double&			satClk,
	double&			satClkVel,
	double&			ephVar,
	bool&			ephClkValid,
	int&			iode,
	Navigation&		nav)
{
	int sys = Sat.sys;

	ephClkValid = false;

	if		(  sys == +E_Sys::GPS
			|| sys == +E_Sys::GAL
			|| sys == +E_Sys::QZS
			|| sys == +E_Sys::BDS)	{	return satClkBroadcast<Eph>		(trace, time, teph, Sat, satClk,	satClkVel,	ephVar, ephClkValid, iode, nav);	}
	else if (  sys == +E_Sys::GLO)	{	return satClkBroadcast<Geph>	(trace, time, teph, Sat, satClk,	satClkVel,	ephVar, ephClkValid, iode, nav);	}
	else if (  sys == +E_Sys::SBS)	{	return satClkBroadcast<Seph>	(trace, time, teph, Sat, satClk,	satClkVel,	ephVar, ephClkValid, iode, nav);	}
	else							{	return false;																										}
}


/* satellite position by broadcast ephemeris
 */
bool satPosBroadcast(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatSys			Sat,
	Vector3d&		rSat,
	Vector3d&		satVel,
	double&			ephVar,
	bool&			ephPosValid,
	int&			iode,
	Navigation&		nav)
{
	int sys = Sat.sys;

	ephPosValid = false;

	if		(  sys == +E_Sys::GPS
			|| sys == +E_Sys::GAL
			|| sys == +E_Sys::QZS
			|| sys == +E_Sys::BDS)	{	return satPosBroadcast<Eph>		(trace, time, teph, Sat, rSat,		satVel,		ephVar, ephPosValid, iode, nav);	}
	else if (  sys == +E_Sys::GLO)	{	return satPosBroadcast<Geph>	(trace, time, teph, Sat, rSat,		satVel,		ephVar, ephPosValid, iode, nav);	}
	else if (  sys == +E_Sys::SBS)	{	return satPosBroadcast<Seph>	(trace, time, teph, Sat, rSat,		satVel,		ephVar, ephPosValid, iode, nav);	}
	else							{	return false;																										}

}

bool satClkBroadcast(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatPos&			satPos,
	Navigation&		nav,
	int				iode)
{
	satPos.iodeClk = iode;

	return satClkBroadcast(
		trace,
		time,
		teph,
		satPos.Sat,
		satPos.satClk,
		satPos.satClkVel,
		satPos.satClkVar,
		satPos.ephClkValid,
		satPos.iodeClk,
		nav);
}

bool satPosBroadcast(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatPos&			satPos,
	Navigation&		nav,
	int				iode)
{
	satPos.iodePos = iode;

	return satPosBroadcast(
		trace,
		time,
		teph,
		satPos.Sat,
		satPos.rSatApc,
		satPos.satVel,
		satPos.posVar,
		satPos.ephPosValid,
		satPos.iodePos,
		nav);
}
