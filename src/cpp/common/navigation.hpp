#ifndef __NAVIGATION_HPP__
#define __NAVIGATION_HPP__

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/binary_object.hpp>
#include <boost/serialization/list.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/unordered_map.hpp>

#include <unordered_map>
#include <unordered_set>
#include <string>
#include <list>
#include <map>

using std::unordered_set;
using std::unordered_map;
using std::string;
using std::list;
using std::map;


#include "observations.hpp"
#include "streamTrace.hpp"
#include "constants.h"
#include "antenna.hpp"
#include "orbits.hpp"
#include "satSys.hpp"
#include "gTime.hpp"
#include "enums.h"

#define MAXDTE      900.0           /* max time difference to ephem time (s) */

// typedef struct
// {        /* satellite fcb data type */
// 	gtime_t ts,te;      /* start/end time (GPST) */
// 	double bias[MAXSAT][3]; /* fcb value   (cyc) */
// 	double std [MAXSAT][3]; /* fcb std-dev (cyc) */
// } fcbd_t;

struct Eph
{        /* GPS/QZS/GAL broadcast ephemeris type */
	SatSys Sat;            /* satellite number */
	int iode,iodc;      /* IODE,IODC */
	int sva;            /* SV accuracy (URA index) */
	int svh;            /* SV health (0:ok) */
	int week;           /* GPS/QZS: gps week, GAL: galileo week */
	int code;           /* GPS/QZS: code on L2, GAL/CMP: data sources */
	int flag;           /* GPS/QZS: L2 P data flag, CMP: nav type */
	GTime toe,toc,ttr; /* Toe,Toc,T_trans */
						/* SV orbit parameters */
	double A,e,i0,OMG0,omg,M0,deln,OMGd,idot;
	double crc,crs,cuc,cus,cic,cis;
	double toes;        /* Toe (s) in week */
	double fit;         /* fit interval (h) */
	double f0,f1,f2;    /* SV clock parameters (af0,af1,af2) */
	double tgd[4];      /* group delay parameters */
						/* GPS/QZS:tgd[0]=TGD */
						/* GAL    :tgd[0]=BGD E5a/E1,tgd[1]=BGD E5b/E1 */
						/* CMP    :tgd[0]=BGD1,tgd[1]=BGD2 */

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
{        /* GLONASS broadcast ephemeris type */
	SatSys Sat;            /* satellite number */
	int iode;           /* IODE (0-6 bit of tb field) */
	int frq;            /* satellite frequency number */
	int svh,sva,age;    /* satellite health, accuracy, age of operation */
	GTime toe;        /* epoch of epherides (gpst) */
	GTime tof;        /* message frame time (gpst) */
	double pos[3];      /* satellite position (ecef) (m) */
	double vel[3];      /* satellite velocity (ecef) (m/s) */
	double acc[3];      /* satellite acceleration (ecef) (m/s^2) */
	double taun,gamn;   /* SV clock bias (s)/relative freq bias */
	double dtaun;       /* delay between L1 and L2 (s) */

	operator int() const
	{
		size_t hash = Sat;
		return hash;
	}
};

struct Pclk
{ 			/* precise clock type */
	double clk; /* satellite clock (s) */
	double std; /* satellite clock std (s) */
	GTime time;       /* time (GPST) */
	int index;          /* clock index for multiple files */

	bool operator <(const Pclk &p2)
	{
		Pclk& p1 = *this;
		double tt = timediff(p1.time, p2.time);
		return tt<-1E-9?-1:(tt>1E-9?1:p1.index-p2.index);
	}
};


//forward declaration for use below
struct OrbitInfo;

struct Peph
{
	SatSys	Sat;            /* satellite number */
	GTime	time;       /* time (GPST) */
	int		index;          /* ephemeris index for multiple files */
	Vector3d	Pos		= Vector3d::Zero();		///< satellite position (ecef) (m)
	Vector3d	PosStd	= Vector3d::Zero();		///< satellite position std (m) 
	Vector3d	Vel		= Vector3d::Zero();		///< satellite velocity/clk-rate (m/s) 
	Vector3d	VelStd	= Vector3d::Zero();		///< satellite velocity/clk-rate std (m/s) 
	double		Clk		= 0;
	double		ClkStd	= 0;
	double		dCk		= 0;
	double		dCkStd	= 0;
	
	bool operator < (const Peph &p2) const
	{
		const Peph& p1 = *this;
		double tt = timediff(p1.time, p2.time);
		return tt < -1E-9 ? -1 : (tt > 1E-9 ? 1 : p1.index - p2.index);
	}
};

struct Seph
{        /* SBAS ephemeris type */
	SatSys Sat;            /* satellite number */
	GTime t0;         /* reference epoch time (GPST) */
	GTime tof;        /* time of message frame (GPST) */
	int sva;            /* SV accuracy (URA index) */
	int svh;            /* SV health (0:ok) */
	double pos[3];      /* satellite position (m) (ecef) */
	double vel[3];      /* satellite velocity (m/s) (ecef) */
	double acc[3];      /* satellite acceleration (m/s^2) (ecef) */
	double af0,af1;     /* satellite clock-offset/drift (s,s/s) */

	operator int() const
	{
		size_t hash = Sat;
		return hash;
	}
};

typedef list<Geph>			GephList;	//todo aaron, change these to maps too
typedef list<Seph>			SephList;
typedef map<GTime, Peph>	PephList;
typedef list<Pclk>			PclkList;
typedef list<Eph>			EphList;

struct tec_t
{
	/* TEC grid type */
	GTime time;       /* epoch time (GPST) */
	int ndata[3];       /* TEC grid data size {nlat,nlon,nhgt} */
	double rb;          /* earth radius (km) */
	double lats[3];     /* latitude start/interval (deg) */
	double lons[3];     /* longitude start/interval (deg) */
	double hgts[3];     /* heights start/interval (km) */
	unordered_map<int, double> 		data; /* TEC grid data (tecu) */
	unordered_map<int, double> 		rms; /* RMS values (tecu) */
};

struct erpd_t
{        /* earth rotation parameter data type */
	double mjd;         /* mjd (days) */
	double xp,yp;       /* pole offset (rad) */
	double xpr,ypr;     /* pole offset rate (rad/day) */
	double ut1_utc;     /* ut1-utc (s) */
	double lod;         /* length of day (s/day) */
};

struct erp_t
{        /* earth rotation parameter type */
	int n,nmax;         /* number and max number of data */
	erpd_t *data;       /* earth rotation parameter data */
};


// SSR message metadata
struct SSRMeta
{
	int				epochTime1s			= 0;
	int				ssrUpdateIntIndex	= -1;
	int				multipleMessage		= 0;
	unsigned int	referenceDatum		= 0;
	unsigned int	provider			= 0;
	unsigned int	solution			= 0;
};

struct SSREph
{
	SSRMeta ssrMeta;
	GTime	t0			= {};
	double	udi			= 0;		///< update interval
	int		iod			= -1;	
	int		iode		= -1;		///< issue of data
	int		iodcrc		= -1;
	double	deph [3]	= {};		///< delta orbit {radial,along,cross} (m)
	double	ddeph[3]	= {};		///< dot delta orbit {radial,along,cross} (m/s)
};

struct SSRClk
{
	SSRMeta ssrMeta	= {};
	GTime	t0		= {};
	double	udi		= 0;			///< update interval
	int		iod		= -1;
	double	dclk[3]	= {};			///< delta clock {c0,c1,c2} (m,m/s,m/s^2)
};

struct SSRHRClk
{
	SSRMeta ssrMeta	= {};
	GTime	t0		= {};
	double	udi		= 0;			///< update interval
	int		iod		= -1;
	double hrclk	= 0;       /* high-rate clock corection (m) */
};

struct SSRUra
{
	GTime	t0	= {};
	double	udi	= 0;				///< update interval
	int		iod	= -1;
	int		ura	= 0;            /* URA indicator */
};

struct SSRPhase
{
	int dispBiasConistInd	= -1;
	int MWConistInd			= -1;
	unsigned int nbias		= 0;
	double yawAngle			= 0;
	double yawRate			= 0;
};

struct SSRPhaseCh
{
	unsigned int signalIntInd		= -1;
	unsigned int signalWidIntInd	= -1;
	unsigned int signalDisconCnt	= -1;
};



struct SSRBias
{
	SSRMeta		ssrMeta;
	
	GTime	t0	= {};
	double	udi	= 0;						///< update interval
	int		iod	= -1;
	
	map<E_ObsCode, double> bias;			///< biases (m) 
	map<E_ObsCode, double> var;				///< biases variance (m^2) 
};

struct SSRCodeBias : SSRBias
{
	//just inherit.
};

struct SSRPhasBias : SSRBias
{
	SSRPhase	ssrPhase; 					///< Additional data for SSR phase messages
	map<E_ObsCode,SSRPhaseCh> ssrPhaseChs;	///< Additional data for SSR phase messages, for each channel
};

struct SSRCodeBiasOut : SSRCodeBias
{
	bool canExport	= false;
	bool isSet		= false;
};

struct SSRPhasBiasOut : SSRPhasBias
{
	bool canExport	= false;
	bool isSet		= false;
};

struct SSRClkOut : SSRClk
{
	double  broadcast		= 0;	// (m)
	double  precise			= 0;	// (m)
	bool    isBroadcastSet	= false;
	bool    isPreciseSet	= false;
	bool	canExport		= false;
};

struct SSREphOut : SSREph
{
	Vector3d	broadcast			= {};
	Vector3d	broadcastVel		= {};
	Vector3d	precise				= {};
	Vector3d	nextBroadcast		= {};
	Vector3d	nextBroadcastVel	= {};
	Vector3d	nextPrecise			= {};
	bool    	isBroadcastSet		= false;
	bool    	isPreciseSet		= false;
	bool    	isNextBroadcastSet	= false;
	bool    	isNextPreciseSet	= false;
	bool		canExport			= false;
};	

/* SSR correction type */
struct ssr_t
{
	map<GTime, SSRCodeBias,	std::greater<GTime>>	ssrCodeBias_map;
	map<GTime, SSRPhasBias,	std::greater<GTime>>	ssrPhasBias_map;
	map<GTime, SSRClk,		std::greater<GTime>>	ssrClk_map;
	map<GTime, SSREph,		std::greater<GTime>>	ssrEph_map;
	map<GTime, SSRHRClk,	std::greater<GTime>>	ssrHRClk_map;
	map<GTime, SSRUra,		std::greater<GTime>>	ssrUra_map;

	int refd_;					///< sat ref datum (0:ITRF,1:regional)
	unsigned char update_;		///<update flag (0:no update,1:update)
};

// map<string, map<GTime, pcvacs_t,	std::greater<GTime>>>	pcvMap;

struct SSROut
{
	SSRPhasBiasOut	ssrPhasBias;
	SSRCodeBiasOut	ssrCodeBias;
	SSRClkOut       ssrClk;
	SSREphOut       ssrEph;
	int             udiNumEpochs		= 0; /* UDI in epochs */
	int             epochsSinceUpdate	= 0;
	int             numObs				= 0; /* Number of observations for this sat */
};

struct SatNav
{
	map<int, double>	lamMap;
	double				cBias_P1_P2;	///< Satellite DCB - P1-P2 (m)
	map<int, double>	cBiasMap;		///< Satellite DCB - Px-Cx (m)
	double		wlbias;      	/* wide-lane bias (cycle) */
	ssr_t		ssr;  			/* SSR corrections */
	SSROut      ssrOut;         /* SSR corrections to export */
	Eph*		eph_ptr			= nullptr;
	Geph*		geph_ptr		= nullptr;
	Seph*		seph_ptr		= nullptr;
	PephList*	pephList_ptr	= nullptr;
};

struct nav_t
{
	/* navigation data type */

	map<string, map<GTime, pcvacs_t,	std::greater<GTime>>>	pcvMap;
	
	unordered_map<int, EphList> 		ephMap;        /* GPS/QZS/GAL ephemeris */
	unordered_map<int, GephList>		gephMap;       /* GLONASS ephemeris */
	unordered_map<int, SephList> 		sephMap;       /* SBAS ephemeris */
	unordered_map<int, PephList> 		pephMap;       /* precise ephemeris */
	unordered_map<string, PclkList> 	pclkMap;       /* precise clock */
	unordered_map<int, SatNav>			satNavMap;
	map<time_t, tec_t>	tecList;         /* tec grid data */
// 	list<fcbd_t> 		fcbList;        /* satellite fcb data */
	erp_t  	erp;         /* earth rotation parameters */
	double utc_gps[4];  /* GPS delta-UTC parameters {A0,A1,T,W} */
	double utc_glo[4];  /* GLONASS UTC GPS time parameters */
	double utc_gal[4];  /* Galileo UTC GPS time parameters */
	double utc_qzs[4];  /* QZS UTC GPS time parameters */
	double utc_cmp[4];  /* BeiDou UTC parameters */
	double utc_sbs[4];  /* SBAS UTC parameters */
	double ion_gps[8];  /* GPS iono model parameters {a0,a1,a2,a3,b0,b1,b2,b3} */
	double ion_gal[4];  /* Galileo iono model parameters {ai0,ai1,ai2,0} */
	double ion_qzs[8];  /* QZSS iono model parameters {a0,a1,a2,a3,b0,b1,b2,b3} */
	double ion_cmp[8];  /* BeiDou iono model parameters {a0,a1,a2,a3,b0,b1,b2,b3} */
	int leaps;          /* leap seconds (s) */
	char glo_fcn[MAXPRNGLO+1];  /* glonass frequency channel number + 8 */
	double glo_cpbias[4];       /* glonass code-phase bias {1C,1P,2C,2P} (m) */

	orbpod_t orbpod = {};


	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & ephMap;
		ar & satNavMap;
	}
};

/* ephemeris and clock functions ---------------------------------------------*/
double eph2clk (GTime time, Eph  *eph);
double geph2clk(GTime time, Geph *geph);
double seph2clk(GTime time, Seph *seph);
void eph2pos (GTime time, Eph  *eph,  double *rs, double *dts, double *var);
void geph2pos(GTime time, Geph *geph, double *rs, double *dts, double *var);
void seph2pos(GTime time, Seph *seph, double *rs, double *dts, double *var);

int satpos(
	Trace&		trace,
	GTime		time,
	GTime		teph,
	Obs&		obs,
	int			ephopt,
	nav_t&		nav,
	PcoMapType* pcoMap_ptr);

void satposs(
	Trace&		trace,
	GTime		teph,
	ObsList&	obsList,
	nav_t&		nav,
	int			ephopt);

Eph*	seleph	(GTime time, SatSys Sat, int iode, nav_t& nav);
Geph*	selgeph	(GTime time, SatSys Sat, int iode, nav_t& nav);
int		ephclk	(GTime time, GTime teph, Obs& obs, double& dts);



int orbPartials(
	Trace&		trace,
	GTime		time,
	Obs&		obs,
	MatrixXd&	interpPartials);

void	updatenav(Obs& obs);
int		outrnxnavbpde(FILE *fp, Eph *eph);


extern	nav_t	nav;

namespace boost::serialization
{
	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, nav_t& nav)
	{
		ar & nav.ephMap;
	}
}

void	initSsrOut();

void	calcSsrCorrections(
	Trace&				trace,
	std::set<SatSys>&	sats,
	GTime				iontime);

void	writeSsrOutToFile(
	int					epochNum,
	std::set<SatSys>	sats);

void	rtcmEncodeToFile			(int epochNum);
void	rtcmDecodeFromFile			(int epochNum);
void	exportSyncFile				(int epochNum);
void	waitForSyncFile				(int epochNum);

Vector3d ecef2rac(
	Vector3d vecToRotate,
	Vector3d r,
	Vector3d v);

#endif
