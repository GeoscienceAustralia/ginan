
#ifndef __CONSTANTS___HPP_
#define __CONSTANTS___HPP_


#include <map>

using std::map;

#include "enums.h"

#include <boost/bimap.hpp>

#define CLIGHT      299792458.0         /* speed of light (m/s) */

#define PSOL        1367/CLIGHT         /* solar radiation pressure at 1 AU, [N/m^2] (1367 W/m^2); IERS 96 */

#define FREQ1       1.57542E9           /* L1/E1  frequency (Hz) */
#define FREQ2       1.22760E9           /* L2     frequency (Hz) */
#define FREQ5       1.17645E9           /* L5/E5a frequency (Hz) */
#define FREQ6       1.27875E9           /* E6/LEX frequency (Hz) */
#define FREQ7       1.20714E9           /* E5b    frequency (Hz) */
#define FREQ8       1.191795E9          /* E5a+b  frequency (Hz) */
#define FREQ1_GLO   1.60200E9           /* GLONASS G1 base frequency (Hz) */
#define DFRQ1_GLO   0.56250E6           /* GLONASS G1 bias frequency (Hz/n) */
#define FREQ2_GLO   1.24600E9           /* GLONASS G2 base frequency (Hz) */
#define DFRQ2_GLO   0.43750E6           /* GLONASS G2 bias frequency (Hz/n) */
#define FREQ3_GLO   1.202025E9          /* GLONASS G3 frequency (Hz) */
#define FREQ1_CMP   1.561098E9          /* BeiDou B1 frequency (Hz) */
#define FREQ2_CMP   1.20714E9           /* BeiDou B2 frequency (Hz) */
#define FREQ3_CMP   1.26852E9           /* BeiDou B3 frequency (Hz) */



#define PI          3.141592653589793238462643383279502884197169399375105820974  /* pi */
#define PI2         (PI*2)              /* pi*2 */
#define D2R         (PI/180.0)          /* deg to rad */
#define R2D         (180.0/PI)          /* rad to deg */
#define SC2RAD      3.1415926535898     /* semi-circle to radian (IS-GPS) */
#define AU          149597870691.0      /* 1 AU (m) */
#define AS2R        (D2R/3600.0)        /* arc sec to radian */

#define OMGE        7.2921151467E-5     /* earth angular velocity (IS-GPS) (rad/s) */

#define G_CONST			6.67408E-11
#define RE_GLO   6378136.0        /* radius of earth (m)            ref [2] */
#define MU_GPS   3.9860050E14     /* gravitational constant         ref [1] */
#define MU_GLO   3.9860044E14     /* gravitational constant         ref [2] */
#define MU_GAL   3.986004418E14   /* earth gravitational constant   ref [7] */
#define MU_CMP   3.986004418E14   /* earth gravitational constant   ref [9] */

#define MU				MU_GPS
#define MOMENTUM_SCALE	10000000.0

#define RE_WGS84    6378137.0           /* earth semimajor axis (WGS84) (m) */
#define FE_WGS84    (1.0/298.257223563) /* earth flattening (WGS84) */

#define IONO_HEIGHT        350000.0            /* ionosphere height (m) */

#define JD2MJD      2400000.5           /* JD to MJD */
#define AUPerDay    (AU/8.64e04)       /* AU/Day (IAU 2009)[m/s] */

#define GM_Earth			3.986004418e14	///< Geocentric gravitation constant (WGS84) [m^3/s^2]
#define M_Earth				5.9722e24

#define RHO_w				1025

/*
// moon parameter 
*/
#define GM_Moon            4.9027949e12		///< moon gravitation constant [m^3/s^2]
#define MoonRadius         1738200.0		///< Equatorial radius of the Moon [m]
#define MoonMinRadius      1738200.0
#define Moon_J2            0.2027e-3

/*
// Jupiter parameter
*/
#define GM_Jupiter         1.26712000000000e+017
#define JupiterRadius      7.14920000000000e+007
#define JupiterMinRadius   6.68540000000000e+007
#define Jupiter_J2         0.01475

/*
// Mars parameter
*/
#define GM_Mars            4.28282868534000e+013
#define MarsRadius         3.39700000000000e+006
#define MarsMinRadius      3.37500000000000e+006
#define Mars_J2            1.96045e-3


/*
// Mercury parameter
*/
#define GM_Mercury         2.20320800000000e+013
#define MercuryRadius      2.43970000000000e+006
#define MercuryMinRadius   2.43970000000000e+006
#define Mercury_J2         50.3e-6

/*
// Neptune parameter 
*/
#define GM_Neptune         6.87130000000000e+015
#define NeptuneRadius      2.52690000000000e+007
#define NeptuneMinRadius   2.48000000000000e+007
#define Neptune_J2         3.411e-3
/*
// Pluto parameter
*/
#define GM_Pluto           1.00907600000000e+012
#define PlutoRadius        1.16200000000000e+006
#define PlutoMinRadius     1.16200000000000e+006
#define Pluto_J2           0.0
/*
// Saturn parameter
*/
#define GM_Saturn          3.79340000000000e+016
#define SaturnRadius       6.02680000000000e+007
#define SaturnMinRadius    5.43640000000000e+007
#define Saturn_J2          0.016298

/*
// Sun parameter
*/
#define GM_Sun             1.327122E20				///< Heliocentric gravitation constant [m^3/s^2]
#define SunRadius          695990000.0				///< Equatorial radius of the Sun [m], Seidelmann 1992
#define SunMinRadius       695990000.0
#define SolarRadPreAtAU    4.560E-6                 ///< Solar Radiation pressure at 1 AU [N/m^2] (~1367 W/m^2); IERS 96
#define Sun_J2             0.0
/*
// Uranus parameter
*/
#define GM_Uranus          5.80320000000000e+015
#define UranusRadius       2.55590000000000e+007
#define UranusMinRadius    2.49730000000000e+007
#define Uranus_J2          3.34343e-3

/*
* Venus parameter
*/
#define GM_Venus           3.24858800000000e+014
#define VenusRadius        6.05190000000000e+006
#define VenusMinRadius     6.05190000000000e+006
#define Venus_J2           4.458e-6

#define ZEROC		273.15

#define GRAVITY     9.80665             /* mean gravity (m/s^2) */
#define MOLARDRY    0.028965            /* molar mass dry air (kg/mol) */
#define UGAS        8.3143              /* universal gas constant (J/K/mol) */

#define TSYS_GPS    0                   /* time system: GPS time */
#define TSYS_UTC    1                   /* time system: UTC */
#define TSYS_GLO    2                   /* time system: GLONASS time */
#define TSYS_GAL    3                   /* time system: Galileo time */
#define TSYS_QZS    4                   /* time system: QZSS time */
#define TSYS_CMP    5                   /* time system: BeiDou time */

/* End of macro definitions for ACS PDE */

#define MAXOBSTYPE  256                  /* max number of obs type in RINEX */
#define DTTOL       0.005               /* tolerance of time difference (s) */
#define MAXDTOE     7200.0              /* max time difference to GPS Toe (s) */
#define MAXDTOE_QZS 3600.0              /* max time difference to QZS Toe (s) */
#define MAXDTOE_GAL 1800.0              /* max time difference to GAL Toe (s) */
#define MAXDTOE_CMP 3600.0              /* max time difference to BDS Toe (s) */
#define MAXDBDSTOE  3600.0              /* max time difference to ephem Toe (s) for BDS */
#define MAXDTOE_GLO 1800.0              /* max time difference to GLO Toe (s) */
#define MAXDTOE_SBS 360.0               /* max time difference to SBAS Toe (s) */

#define MAXLEAPS    64                  /* max number of leap seconds table */

#define SOLQ_NONE   0                   /* solution status: no solution */
#define SOLQ_FIX    1                   /* solution status: fix */
#define SOLQ_FLOAT  2                   /* solution status: float */
#define SOLQ_SBAS   3                   /* solution status: SBAS */
#define SOLQ_DGPS   4                   /* solution status: DGPS/DGNSS */
#define SOLQ_SINGLE 5                   /* solution status: single */
#define SOLQ_PPP    6                   /* solution status: PPP */


#define P2_5        0.03125             /* 2^-5 */
#define P2_6        0.015625            /* 2^-6 */
#define P2_10       0.0009765625          /* 2^-10 */
#define P2_11       4.882812500000000E-04 /* 2^-11 */
#define P2_15       3.051757812500000E-05 /* 2^-15 */
#define P2_17       7.629394531250000E-06 /* 2^-17 */
#define P2_19       1.907348632812500E-06 /* 2^-19 */
#define P2_20       9.536743164062500E-07 /* 2^-20 */
#define P2_21       4.768371582031250E-07 /* 2^-21 */
#define P2_23       1.192092895507810E-07 /* 2^-23 */
#define P2_24       5.960464477539063E-08 /* 2^-24 */
#define P2_27       7.450580596923828E-09 /* 2^-27 */
#define P2_28       3.725290298461914E-09 /* 2^-28 */
#define P2_29       1.862645149230957E-09 /* 2^-29 */
#define P2_30       9.313225746154785E-10 /* 2^-30 */
#define P2_31       4.656612873077393E-10 /* 2^-31 */
#define P2_32       2.328306436538696E-10 /* 2^-32 */
#define P2_33       1.164153218269348E-10 /* 2^-33 */
#define P2_34       5.820766091346740E-11 /* 2^-34 */
#define P2_35       2.910383045673370E-11 /* 2^-35 */
#define P2_38       3.637978807091710E-12 /* 2^-38 */
#define P2_39       1.818989403545856E-12 /* 2^-39 */
#define P2_40       9.094947017729280E-13 /* 2^-40 */
#define P2_41       4.547473508864641E-13 /* 2^-41 */
#define P2_43       1.136868377216160E-13 /* 2^-43 */
#define P2_46       1.421085471520200E-14 /* 2^-46 */
#define P2_48       3.552713678800501E-15 /* 2^-48 */
#define P2_50       8.881784197001252E-16 /* 2^-50 */
#define P2_55       2.775557561562891E-17 /* 2^-55 */
#define P2_59       1.734723475976810E-18 /* 2^-59 */
#define P2_66       1.355252715606880E-20 /* 2^-66 */


#define SMOOTHED_SUFFIX		"_smoothed"
#define FORWARD_SUFFIX		"_forward"
#define BACKWARD_SUFFIX		"_backward"

extern const E_FType		ftypes	[E_ObsCode::NUM_CODES];
extern const double			lambdas	[E_ObsCode::NUM_CODES];

#define mjdJ2000    51544.5 /* modified Julian date of J2000.0 */
#define Arcs        3600.0*180.0/PI     /* Arcseconds per radian */
extern const double gpst0[]; /* gps time reference */
extern const double gst0 []; /* galileo system time reference */
extern const double bdt0 []; /* beidou time reference */

extern const unsigned int tbl_CRC24Q[];
extern const double leaps[MAXLEAPS+1][7];

#define NUM_DAT_ORB_INTP 10 /* number of data for orbit interpolation */
#define NUM_ORD_ORB_INTP 10 /* orders of orbit interpolation */

extern map<int, double> lam_carr;		/* carrier wave length (m) {L1,L2,...} */ 

const unsigned char RTCM_PREAMBLE = 0xD3;

extern const boost::bimap<E_ObsCode,int> mCodes_gps;
extern const boost::bimap<E_ObsCode,int> mCodes_glo;
extern const boost::bimap<E_ObsCode,int> mCodes_gal;
extern const boost::bimap<E_ObsCode,int> mCodes_qzs;
extern const boost::bimap<E_ObsCode,int> mCodes_bds;
extern const boost::bimap<E_ObsCode,int> mCodes_sbs;

#endif
