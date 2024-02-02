
#pragma once


#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "constants.hpp"
#include "satSys.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "enums.h"


#include <iostream>
#include <string>
#include <vector>
#include <map>

using std::string;
using std::vector;
using std::map;


//forward declarations
struct Navigation;
struct GObs;
struct Peph;

#define ANY_IODE				-1
#define NO_SP3_CLK				999999.999999
#define INVALID_CLOCK_VALUE		NO_SP3_CLK

struct KeplerEph
{
	double A	= 0;		///< semi major axis
	double e	= 0;		///< eccentricity
	double i0	= 0;		///< inclination
	double OMG0	= 0;		///< right ascension of ascending node
	double omg	= 0;		///< argument of perigee
	double M0	= 0;		///< mean anomoly
	double deln	= 0;		///< correction mean motion
	double OMGd	= 0;		///< rate of OMG
	double idot	= 0;		///< rate of inclination
	double crc	= 0;		///< correction radial		cosine
	double crs	= 0;		///< correction radial		  sine
	double cuc	= 0;		///< correction lattitude	cosine
	double cus	= 0;		///< correction lattitude	  sine
	double cic	= 0;		///< correction inclination cosine
	double cis	= 0;		///< correction inclination   sine
	double dn0d	= 0;		///< rate of correction mean motion
	double Adot	= 0;		///< rate of A
};

struct BrdcEph
{

};

/** GPS/QZS/GAL/BDS broadcast ephemeris
 */
struct Eph : BrdcEph, KeplerEph
{
	E_NavMsgType	type = E_NavMsgType::NONE;	///< message type
	SatSys	Sat;		///< satellite number
	int		iode = -1;	///< GPS/QZS: IODE, GAL: IODnav
	int		iodc = 0;	///< IODC
	int		aode;		///< BDS AODE
	int		aodc;		///< BDS AODC
	int		sva;		///< SV accuracy (URA index)
	E_Svh	svh;		///< SV health
	int		week;		///< GPS/QZS: gps week, GAL:gps week (i.e. galileo week + 1024), BDS: beidou week
	int		code = 0;	///< GPS/QZS: code on L2, GAL: data source
	int		flag = 0;	///< GPS L2 P data flag
	int		howTow;		///< Hand over word time
	GTime	toc;		///< time of clock
	GTime	toe;		///< time of ephemeris
	GTime	ttm;		///< transmission time


	double toes;        ///< TOE (s) in week
	double fit;         ///< fit interval (h)
	double f0;			///< SV clock parameter (af0)
	double f1;			///< SV clock parameter (af1)
	double f2;			///< SV clock parameter (af2)
	double tgd[4]	= {};	///< group delay parameters
							///< GPS/QZS:tgd[0]=TGD
							///< GAL    :tgd[0]=BGD E5a/E1,tgd[1]=BGD E5b/E1
							///< BDS    :tgd[0]=BGD1,tgd[1]=BGD2

	E_SatType		orb		= E_SatType::NONE;		///< BDS sat/orbit type
	GTime	top		= {};	///< time of prediction
	double	tops	= 0;	///< t_op (s) in week
	double	ura[4]	= {};	///< user range accuracy or GAL SISA
							///< GPS/QZS CNVX: ura[0]=URAI_NED0, ura[1]=URAI_NED1, ura[2]=URAI_NED2, ura[3]=URAI_ED
	double	isc[6]	= {};	///< inter-signal corrections
							///< GPS/QZS CNAV: isc[0]=ISC_L1CA, isc[1]=ISC_L2C, isc[2]=ISC_L5I5, isc[3]=ISC_L5Q5
							///< GPS/QZS CNV2: isc[0]=ISC_L1CA, isc[1]=ISC_L2C, isc[2]=ISC_L5I5, isc[3]=ISC_L5Q5, isc[4]=ISC_L1Cd, isc[5]=ISC_L1Cp
							///< BDS	 CNV1: isc[0]=ISC_B1Cd
							///< BDS	 CNV2: isc[1]=ISC_B2ad
	double	sis[5]	= {};	///< signal in space accuracy index
							///< BDS CNVX sis[0]=SISAI_oe, sis[1]=SISAI_ocb, sis[2]=SISAI_oc1, sis[3]=SISAI_oc2, sis[4]=SISMAI


	// original messages from stream/rinex for debugging
	double	tocs;			///< TOC (s) within week
	int		weekRollOver;	///< week number (rolled over)
	double	sqrtA;			///< sqrt A
	int		e5a_hs	= 0;	///< GAL E5a signal health status
	int		e5a_dvs	= 0;	///< GAL E5a data validity status
	int		e5b_hs	= 0;	///< GAL E5b signal health status
	int		e5b_dvs	= 0;	///< GAL E5b data validity status
	int		e1_hs	= 0;	///< GAL E1 signal health status
	int		e1_dvs	= 0;	///< GAL E1 data validity status
	double	ttms	= 0;	///< transmission time (s) within week
	int		fitFlag	= 0;	///< fit flag


	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & iode;
		ar & iodc;
		ar & aode;
		ar & aodc;
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
struct Geph : BrdcEph
{
	E_NavMsgType	type = E_NavMsgType::NONE;	///< message type
	SatSys		Sat;		///< satellite number
	int			iode = -1;	///< IODE (0-6 bit of tb field)
	int			frq;		///< satellite frequency number
	E_Svh		svh;		///< satellite health
	int			sva;		///< satellite accuracy
	int			age;		///< age of operation
	GTime		toe;		///< epoch of epherides (gpst)
	GTime		tof;		///< message frame time (gpst)
	Vector3d	pos;		///< satellite position (ecef) (m)
	Vector3d	vel;		///< satellite velocity (ecef) (m/s)
	Vector3d	acc;	 	///< satellite acceleration (ecef) (m/s^2)
	double		taun;		///< SV clock bias (s)
	double		gammaN;		///< SV relative freq bias
	double		dtaun;		///< delay between L1 and L2 (s)


	// original messages from stream/rinex for debugging
	double	tofs;			///< TOF (s) within the current day
    int		tk_hour;		///< number of hours of TOF
    int		tk_min;			///< number of minutes of TOF
    double	tk_sec;			///< seconds of TOF
	int		tb;				///< number of 15 min of TOE
	int		glonassM;		///< type of GLO satellites
	int		NT;				///< calender number of day within 4-year interval
	bool	moreData;		///< availability of additional data
	int		N4;				///< 4-year interval number
};

/** precise clock
 */
struct Pclk
{
	double		clk			= INVALID_CLOCK_VALUE;		///< satellite clock (s)
	double		clkStd		= 0;						///< satellite clock std (s)
	int			clkIndex;								///< clock index for multiple files
};

/** precise ephemeris
 */
struct Peph : Pclk
{
	SatSys		Sat;							///< satellite number
	GTime		time;							///< time (GPST)
	int			index;							///< ephemeris index for multiple files
	VectorEcef	pos;							///< satellite position					(m)
	Vector3d	posStd	= Vector3d::Zero();		///< satellite position std				(m)
	VectorEcef	vel;							///< satellite velocity/clk-rate		(m/s)
	Vector3d	velStd	= Vector3d::Zero();		///< satellite velocity/clk-rate std	(m/s)
};

/** Satellite attitude
 */
struct Att
{
	string		id;
	GTime		time;	///< time (GPST)
	int			index;	///< ephemeris index for multiple files
	E_ObxFrame	frame;
	Quaterniond	q = Eigen::Quaterniond::Identity();	///< satellite attitude represented w/ a quaternion
};

/** SBAS ephemeris
 */
struct Seph : BrdcEph
{
	E_NavMsgType	type = E_NavMsgType::NONE;	///< message type
	SatSys		Sat;			///< satellite number
	GTime		t0;				///< reference epoch time (GPST)
	GTime		tof;			///< time of message frame (GPST)
	int			sva;			///< SV accuracy (URA index)
	E_Svh		svh;			///< SV health
	VectorEcef	pos;			///< satellite position (m) (ecef)
	VectorEcef	vel;			///< satellite velocity (m/s) (ecef)
	VectorEcef	acc;			///< satellite acceleration (m/s^2) (ecef)
	double		af0		= 0;	///< satellite clock-offset/drift (s)
	double		af1		= 0;	///< satellite clock-drift (s/s)
	int			iode	= -1;	//unused, for templating only
	GTime		toe;			//unused, for templating only

	double		tofs;			///< TOF (s) within the week
 };

/** GPS/QZS CNAV/CNAV-2 or BDS CNAV-1/CNAV-2/CNAV-3 ephemeris
*/
struct Ceph : KeplerEph
{
	E_NavMsgType	type	= E_NavMsgType::NONE;	///< message type
	E_SatType		orb		= E_SatType::NONE;		///< BDS sat/orbit type
	SatSys	Sat;			///< satellite number
	int		iode	= -1;	///< BDS CNAV1/CNV2 IODE
	int		iodc	= -1;	///< BDS CNAV1/CNV2 IODC
	E_Svh	svh;			///< SV health
	int		wnop	= 0;	///< GPS/QZS: GPS week number (of prediction?) with AR
	int		flag	= 0;	///< BDS B1C/B2a+B1C/B2b integrity flags
	GTime	toc		= {};	///< time of clock
	GTime	toe		= {};	///< time of ephemeris, for GPS/QZS, TOE==TOC
	GTime	top		= {};	///< time of prediction
	GTime	ttm		= {};	///< transmission time

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

	double	ttms	= 0;	///< transmission time (s) within week
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

	double A0	= 0;	///< (sec)
	double A1	= 0;	///< (sec/sec)
	double A2	= 0;	///< (sec/sec^2)

	double ttms	= 0;	///< transmission time (s) within week
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

	double			ttms;		///< transmission time (s) within week
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

Matrix3d ecef2rac(
	Vector3d& rSat,
	Vector3d& satVel);


void cullOldEphs(
	GTime	time);

void cullOldSSRs(
	GTime	time);


struct KFState;


template<typename TYPE> TYPE*	seleph(Trace& trace, GTime time, SatSys Sat,	E_NavMsgType type, int iode,	Navigation& nav);
template<typename TYPE> TYPE*	seleph(Trace& trace, GTime time, E_Sys sys,		E_NavMsgType type, 				Navigation& nav);


bool satclk(
	Trace&				trace,
	GTime				time,
	GTime				teph,
	SatPos&				satPos,
	vector<E_Source>	ephTypes,
	Navigation&			nav,
	const KFState*		kfState_ptr		= nullptr,
	const KFState*		remote_ptr		= nullptr);

bool satpos(
	Trace&				trace,
	GTime				time,
	GTime				teph,
	SatPos&				satPos,
	vector<E_Source>	ephTypes,
	E_OffsetType		offsetType,
	Navigation&			nav,
	const KFState*		kfState_ptr		= nullptr,
	const KFState*		remote_ptr		= nullptr);

bool satPosClk(
	Trace&				trace,
	GTime				teph,
	GObs&				obs,
	Navigation&			nav,
	vector<E_Source>	posSources,
	vector<E_Source>	clkSources,
	const KFState*		kfState_ptr		= nullptr,
	const KFState*		remote_ptr		= nullptr,
	E_OffsetType		offsetType		= E_OffsetType::COM,
	E_Relativity		applyRelativity	= E_Relativity::ON);

void readSp3ToNav(
	string&		file,
	Navigation&	nav,
	int			opt);

bool readsp3(
	std::istream&	fileStream,
	vector<Peph>&	pephList,
	int				opt,
	E_TimeSys&		tsys,
	double*			bfact);

void readOrbex(
	string			filepath,
	Navigation&		nav);


bool satPosKalman(
	Trace&			trace,
	GTime			time,
	SatPos&			satPos,
	const KFState*	kfState_ptr);

bool satClkKalman(
	Trace&			trace,
	GTime			time,
	SatPos&			satPos,
	const KFState*	kfState_ptr);

bool satClkBroadcast(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatSys			Sat,
	double&			satClk,
	double&			satClkVel,
	double&			ephVar,
	bool&			ephClkValid,
	int&			obsIode,
	Navigation&		nav);

bool satPosBroadcast(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatSys			Sat,
	Vector3d&		rSat,
	Vector3d&		satVel,
	double&			ephVar,
	bool&			ephPosValid,
	int&			obsIode,
	Navigation&		nav);


bool satClkBroadcast(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatPos&			satPos,
	Navigation&		nav,
	int				iode = ANY_IODE);

bool satPosBroadcast(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatPos&			satPos,
	Navigation&		nav,
	int				iode = ANY_IODE);


bool satPosPrecise(
	Trace&			trace,
	GTime			time,
	SatPos&			satPos,
	Navigation&		nav);

bool satClkPrecise(
	Trace&			trace,
	GTime			time,
	SatPos&			satPos,
	Navigation&		nav);


bool satPosSSR(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatPos&			satPos,
	Navigation&		nav);

bool satClkSSR(
	Trace&			trace,
	GTime			time,
	GTime			teph,
	SatPos&			satPos,
	Navigation&		nav);

double relativity1(
	Vector3d&			rSat,
	Vector3d&			satVel);
