
#ifndef __EPHEMERIS_HPP__
#define __EPHEMERIS_HPP__


#include <map>

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "streamTrace.hpp"
#include "constants.hpp"
#include "antenna.hpp"
#include "satSys.hpp"
#include "gTime.hpp"
#include "enums.h"


#include <iostream>
#include <string>
#include <list>

using std::string;
using std::list;
using std::map;


//forward declarations
struct Navigation;
struct Obs;
struct Peph;

#define ANY_IODE -1

/** GPS/QZS/GAL/BDS broadcast ephemeris
 */
struct Eph
{    
	E_NavMsgType	type = E_NavMsgType::NONE;	///< message type
	SatSys	Sat;		///< satellite number
	int		iode;		///< IODE
	int		iodc;		///< IODC
	int		sva;		///< SV accuracy (URA index)
	E_Svh	svh;		///< SV health
	int		week;		///< GPS/QZS: gps week, GAL: galileo week
	int		code;		///< GPS/QZS: code on L2, GAL/CMP: data sources
	int		flag;		///< GPS/QZS: L2 P data flag, CMP: nav type
	GTime	toc;		///< time of clock
	GTime	toe;		///< time of ephemeris
	GTime	ttm;		///< transmission time
	
						
	double A;			///< semi major axis
	double e;			///< eccentricity
	double i0;			///< inclination
	double OMG0;		///< right ascension of ascending node
	double omg;			///< argument of perigee
	double M0;			///< mean anomoly
	double deln;		///< correction mean motion
	double OMGd;		///< rate of OMG
	double idot;		///< rate of inclination
	double crc;			///< correction radial		cosine
	double crs;			///< correction radial		  sine
	double cuc;			///< correction lattitude	cosine
	double cus;			///< correction lattitude	  sine
	double cic;			///< correction inclination cosine
	double cis;			///< correction inclination   sine
	
	double toes;        ///< TOE (s) in week
	double fit;         ///< fit interval (h)
	double f0;	///< SV clock parameter (af0)
	double f1;	///< SV clock parameter (af1)
	double f2;	///< SV clock parameter (af2)
	double tgd[4];      ///< group delay parameters
						///< GPS/QZS:tgd[0]=TGD
						///< GAL    :tgd[0]=BGD E5a/E1,tgd[1]=BGD E5b/E1
						///< CMP    :tgd[0]=BGD1,tgd[1]=BGD2

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & iode;
		ar & iodc;
		ar & sva;
		ar & svh;
		ar & week;
		ar & code;
		ar & flag;
		ar & toe;
		ar & toc;
		ar & ttm;
		ar & A;
		ar & e;
		ar & i0;
		ar & OMG0;
		ar & omg;
		ar & M0;
		ar & deln;
		ar & OMGd;
		ar & idot;
		ar & crc;
		ar & crs;
		ar & cuc;
		ar & cus;
		ar & cic;
		ar & cis;
		ar & toes;
		ar & fit;
		ar & f0;
		ar & f1;
		ar & f2;
		ar & tgd[0];
		ar & tgd[1];
		ar & tgd[2];
		ar & tgd[3];
		ar & Sat;
	}
};

/** GLONASS broadcast ephemeris
 */
struct Geph
{       
	E_NavMsgType	type = E_NavMsgType::NONE;	///< message type
	SatSys		Sat;		///< satellite number 
	int			iode;		///< IODE (0-6 bit of tb field) 
	int			frq;		///< satellite frequency number
	E_Svh		svh;		///< satellite health
	int			sva;		///< satellite accuracy
	int			age;		///< age of operation 
	GTime		toe;		///< epoch of epherides (gpst) 
	GTime		tof;		///< message frame time (gpst) 
	Vector3d	pos;		///< satellite position (ecef) (m) 
	Vector3d	vel;		///< satellite velocity (ecef) (m/s) 
	Vector3d	acc;	 	///< satellite acceleration (ecef) (m/s^2) 
	double		taun,gamn;	///< SV clock bias (s)/relative freq bias 
	double		dtaun;		///< delay between L1 and L2 (s) 
};

/** precise clock
 */
struct Pclk
{ 			
	double	clk;		///< satellite clock (s)
	double	std;		///< satellite clock std (s)
	GTime	time;		///< time (GPST)
	int		index;		///< clock index for multiple files
};

/** precise ephemeris
 */
struct Peph
{
	SatSys		Sat;							///< satellite number 
	GTime		time;							///< time (GPST) 
	int			index;							///< ephemeris index for multiple files 
	Vector3d	Pos		= Vector3d::Zero();		///< satellite position (ecef) (m)
	Vector3d	PosStd	= Vector3d::Zero();		///< satellite position std (m) 
	Vector3d	Vel		= Vector3d::Zero();		///< satellite velocity/clk-rate (m/s) 
	Vector3d	VelStd	= Vector3d::Zero();		///< satellite velocity/clk-rate std (m/s) 
	double		Clk		= 0;
	double		ClkStd	= 0;
	double		dCk		= 0;
	double		dCkStd	= 0;
};

/** SBAS ephemeris
 */
struct Seph
{        
	E_NavMsgType	type = E_NavMsgType::NONE;	///< message type
	SatSys		Sat;			///< satellite number
	GTime		t0;				///< reference epoch time (GPST) 
	GTime		tof;			///< time of message frame (GPST) 
	int			sva;			///< SV accuracy (URA index) 
	E_Svh		svh;			///< SV health 
	Vector3d	pos;			///< satellite position (m) (ecef) 
	Vector3d	vel;			///< satellite velocity (m/s) (ecef) 
	Vector3d	acc;			///< satellite acceleration (m/s^2) (ecef) 
	double		af0,af1;		///< satellite clock-offset/drift (s,s/s) 
	
	int			iode	= 0;	//unused, for templating only
	int			toe		= 0;	//unused, for templating only
};

/** GPS/QZS CNAV/CNAV-2 or BDS CNAV-1/CNAV-2/CNAV-3 ephemeris
*/
struct Ceph
{
	E_NavMsgType	type	= E_NavMsgType::NONE;	///< message type
	E_SatType		orb		= E_SatType::NONE;		///< BDS sat/orbit type
	SatSys	Sat;			///< satellite number
	int		iode	= 0;	///< BDS CNAV1/CNV2 IODE
	int		iodc	= 0;	///< BDS CNAV1/CNV2 IODC
	E_Svh	svh;			///< SV health
	int		wnop	= 0;	///< GPS/QZS: GPS week number (of prediction?) with AR
	int		flag	= 0;	///< BDS B1C/B2a+B1C/B2b integrity flags
	GTime	toc		= {};	///< time of clock
	GTime	toe		= {};	///< time of ephemeris, for GPS/QZS, TOE==TOC
	GTime	top		= {};	///< time of prediction
	GTime	ttm		= {};	///< transmission time
	
						
	double	A		= 0;	///< semi major axis
	double	Adot	= 0;	///< rate of A
	double	e		= 0;	///< eccentricity
	double	i0		= 0;	///< inclination
	double	OMG0	= 0;	///< right ascension of ascending node
	double	omg		= 0;	///< argument of perigee
	double	M0		= 0;	///< mean anomoly
	double	deln	= 0;	///< correction mean motion
	double	dn0d	= 0;	///< rate of correction mean motion
	double	OMGd	= 0;	///< rate of OMG
	double	idot	= 0;	///< rate of inclination
	double	crc		= 0;	///< correction radial		cosine
	double	crs		= 0;	///< correction radial		sine
	double	cuc		= 0;	///< correction lattitude	cosine
	double	cus		= 0;	///< correction lattitude	sine
	double	cic		= 0;	///< correction inclination cosine
	double	cis		= 0;	///< correction inclination sine
	
	double	ura[4]	= {};	///< user range accuracy
							///< GPS/QZS: ura[0]=URAI_NED0, ura[1]=URAI_NED1, ura[2]=URAI_NED2, ura[3]=URAI_ED
	double	isc[6]	= {};	///< inter-signal corrections
							///< GPS/QZS CNAV: isc[0]=ISC_L1CA, isc[1]=ISC_L2C, isc[2]=ISC_L5I5, isc[3]=ISC_L5Q5
							///< GPS/QZS CNV2: isc[0]=ISC_L1CA, isc[1]=ISC_L2C, isc[2]=ISC_L5I5, isc[3]=ISC_L5Q5, isc[4]=ISC_L1Cd, isc[5]=ISC_L1Cp
							///< BDS	 CNV1: isc[0]=ISC_B1Cd
							///< BDS	 CNV2: isc[1]=ISC_B2ad
	double	sis[5]	= {};	///< signal in space accuracy index
							///< BDS sis[0]=SISAI_oe, sis[1]=SISAI_ocb, sis[2]=SISAI_oc1, sis[3]=SISAI_oc2, sis[4]=SISMAI
	double	tops	= 0;	///< t_op (s) in week
	double	toes	= 0;	///< TOE (s) in week
	double	f0		= 0;	///< SV clock parameter (af0)
	double	f1		= 0;	///< SV clock parameter (af1)
	double	f2		= 0;	///< SV clock parameter (af2)
	double	tgd[4]	= {};	///< group delay parameters
							///< GPS/QZS:		tgd[0]=TGD
							///< BDS CNAV1/CNV2: tgd[0]=TGD_B1Cp, tgd[1]=TGD_B2ap
							///< BDS CNAV3:	tgd[2]=TGD_B2bI
};

/** system Time offset message
 */
struct STO
{
	E_NavMsgType	type = E_NavMsgType::NONE;	///< message type
	SatSys			Sat;	///< satellite number
	GTime			tot;	///< reference epoch for time offset information
	GTime			ttm;	///< transmission time
	E_StoCode		code = E_StoCode::NONE;	///< system Time offset code;
	E_SbasId		sid  = E_SbasId::NONE;	///< SBAS ID
	E_UtcId			uid  = E_UtcId::NONE;	///< UTC ID

	double A0;				///< (sec)
	double A1;				///< (sec/sec)
	double A2;				///< (sec/sec^2)
};

/** EOP message
 */
struct EOP
{
	E_NavMsgType	type = E_NavMsgType::NONE;	///< message type
	SatSys			Sat;		///< satellite number
	GTime			teop;		///< reference epoch of EOP data
	GTime			ttm;		///< transmission time

	double xp		= 0;		///< pole offset (rad)
	double xpr		= 0;		///< pole offset rate (rad/day)
	double xprr		= 0;		///< pole offset rate rate (rad/day^2)
	double yp		= 0;		///< pole offset (rad)
	double ypr		= 0;		///< pole offset rate (rad/day)
	double yprr		= 0;		///< pole offset rate rate (rad/day^2)
	double dut1		= 0;		///< ut1-utc or ut1-gpst (s)
	double dur		= 0;		///< delta ut1 rate (s/day)
	double durr		= 0;		///< delta ut1 rate rate (s/day^2)
};

/** ionosphere message
 */
struct ION
{
	E_NavMsgType	type = E_NavMsgType::NONE;	///< message type
	SatSys			Sat;		///< satellite number
	GTime			ttm;		///< transmission time
	int				code = 0;	///< rgion code for QZS
	int				flag = 0;	///< disturbance flags for GAL

	union 
	{
		double vals[9] = {};
		struct
		{
			// Klobuchar model: GPS/QZS LNAV/CNVX and BDS D1D2
			double a0;
			double a1;
			double a2;
			double a3;
			double b0;
			double b1;
			double b2;
			double b3;
		};
		struct
		{
			// NEQUICK-G model: GAL IFNV
			double ai0;
			double ai1;
			double ai2;
		};
		struct
		{
			// BDGIM model: BDS CNVX
			double alpha1;
			double alpha2;
			double alpha3;
			double alpha4;
			double alpha5;
			double alpha6;
			double alpha7;
			double alpha8;
			double alpha9;
		};
	};
};

struct Navigation;


template<typename EPHTYPE>
EPHTYPE* seleph(
	Trace&		trace,
	GTime		time, 
	SatSys		Sat, 
	int			iode, 
	Navigation&	nav);

template<>
Eph* seleph<Eph>(
	Trace&		trace,
	GTime		time, 
	SatSys		Sat, 
	int			iode, 
	Navigation&	nav);

template<>
Geph* seleph<Geph>(
	Trace&		trace,
	GTime		time, 
	SatSys		Sat, 
	int			iode, 
	Navigation&	nav);

template<>
Seph* seleph<Seph>(
	Trace&		trace,
	GTime		time, 
	SatSys		Sat, 
	int			iode, 
	Navigation&	nav);

template<typename EPHTYPE>
EPHTYPE* seleph(
	Trace&			trace,
	GTime			time, 
	SatSys			Sat, 
	E_NavMsgType	type,
	int				iode, 
	Navigation&		nav);

template<>
Ceph* seleph<Ceph>(
	Trace&			trace,
	GTime			time, 
	SatSys			Sat, 
	E_NavMsgType	type,
	int				iode, 
	Navigation&		nav);

template<typename EPHTYPE>
EPHTYPE* seleph(
	Trace&			trace,
	GTime			time, 
	E_Sys			sys, 
	E_NavMsgType	type,
	Navigation&		nav);

template<>
ION* seleph<ION>(
	Trace&			trace,
	GTime			time, 
	E_Sys			sys, 
	E_NavMsgType	type,
	Navigation&		nav);

template<>
EOP* seleph<EOP>(
	Trace&			trace,
	GTime			time, 
	E_Sys			sys, 
	E_NavMsgType	type,
	Navigation&		nav);


Vector3d ecef2rac(
	Vector3d& vecToRotate,
	Vector3d& r,
	Vector3d& v);

Matrix3d ecef2rac(
	Vector3d& rSat,		
	Vector3d& satVel);	


void cullOldEphs(
	GTime	time);

void cullOldSSRs(
	GTime	time);


struct KFState;

int satpos(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	Obs&			obs,
	E_Ephemeris		ephType,
	E_OffsetType	offsetType,
	Navigation&		nav,
	bool			applyRelativity	= true,
	KFState*		kfState_ptr		= nullptr);

void satposs(
	Trace&			trace,
	GTime			teph,
	ObsList&		obsList,
	Navigation&		nav,
	E_Ephemeris		ephType,
	E_OffsetType	offsetType = E_OffsetType::COM,
	bool			applyRelativity = true,
	bool			applyFlightTime = true);

void readSp3ToNav(
	string&		file, 
	Navigation*	nav, 
	int			opt);

bool readsp3(
	std::istream&	fileStream, 
	list<Peph>&		pephList,	
	int				opt,		
	bool&			isUTC,
	double*			bfact);

double	interppol(
	const double*	x,
	double*			y,
	int				n);

void	orb2sp3(
	Navigation&	nav);

#endif
