
#ifndef __EPHEMERIS_HPP__
#define __EPHEMERIS_HPP__


#include <map>

using std::map;

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


//forward declarations
struct nav_t;
struct Obs;
struct Peph;

/** GPS/QZS/GAL broadcast ephemeris
 */
struct Eph
{    
	SatSys	Sat;		///< satellite number
	int		iode;		///< IODE
	int		iodc;		///< IODC
	int		sva;		///< SV accuracy (URA index)
	E_Svh	svh;		///< SV health
	int		week;		///< GPS/QZS: gps week, GAL: galileo week
	int		code;		///< GPS/QZS: code on L2, GAL/CMP: data sources
	int		flag;		///< GPS/QZS: L2 P data flag, CMP: nav type
	GTime	toe;		///< Time of ephemeris
	GTime	toc;		///< TOC
	GTime	ttr;		///< T_trans
	
						
	double A;			///< semi major axis
	double e;			///< eccentricity
	double i0;			///< inclination
	double OMG0;		///< 
	double omg;			///< 
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
	
	double toes;        ///< Toe (s) in week
	double fit;         ///< fit interval (h)
	double f0,f1,f2;    ///< SV clock parameters (af0,af1,af2)
	double tgd[4];      ///< group delay parameters
						///< GPS/QZS:tgd[0]=TGD
						///< GAL    :tgd[0]=BGD E5a/E1,tgd[1]=BGD E5b/E1
						///< CMP    :tgd[0]=BGD1,tgd[1]=BGD2

	operator int() const
	{
		size_t hash = Sat;
		return hash;
	}

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
		ar & ttr;
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

struct Geph
{       
	/// GLONASS broadcast ephemeris type
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

	operator int() const
	{
		size_t hash = Sat;
		return hash;
	}
};

struct Pclk
{ 			
	/// precise clock type
	double	clk;		///< satellite clock (s)
	double	std;		///< satellite clock std (s)
	GTime	time;		///< time (GPST)
	int		index;		///< clock index for multiple files
};

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

struct Seph
{        
	/// SBAS ephemeris type 
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

	operator int() const
	{
		size_t hash = Sat;
		return hash;
	}
};


struct nav_t;


template<typename EPHTYPE>
EPHTYPE* seleph(
	Trace&	trace,
	GTime	time, 
	SatSys	Sat, 
	int		iode, 
	nav_t&	nav);

template<>
Eph* seleph<Eph>(
	Trace&	trace,
	GTime	time, 
	SatSys	Sat, 
	int		iode, 
	nav_t&	nav);

template<>
Geph* seleph<Geph>(
	Trace&	trace,
	GTime	time, 
	SatSys	Sat, 
	int		iode, 
	nav_t&	nav);

template<>
Seph* seleph<Seph>(
	Trace&	trace,
	GTime	time, 
	SatSys	Sat, 
	int		iode, 
	nav_t&	nav);


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
	nav_t&			nav,
	PcoMapType* 	pcoMap_ptr		= nullptr,
	bool			applyRelativity	= true,
	KFState*		kfState_ptr		= nullptr);

void satposs(
	Trace&			trace,
	GTime			teph,
	ObsList&		obsList,
	nav_t&			nav,
	E_Ephemeris		ephType,
	E_OffsetType	offsetType = E_OffsetType::COM,
	bool			applyRelativity = true,
	bool			applyFlightTime = true);

int readdcb(
	string	file, 
	nav_t*	nav);

bool peph2pos(
	Trace&		trace,
	GTime		time,
	SatSys&		Sat,
	Obs&		obs,
	nav_t& 		nav,
	bool		applyRelativity	= true);

void readSp3ToNav(
	string&	file, 
	nav_t*	nav, 
	int		opt);

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
	nav_t& nav);

int		pephclk(
	GTime	time,
	string	id,
	nav_t&	nav,
	double&	dtSat,
	double*	varc = nullptr);

#endif
