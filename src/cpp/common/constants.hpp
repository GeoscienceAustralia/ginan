#pragma once

#include <boost/bimap.hpp>
#include <map>
#include <vector>
#include "common/enums.h"

struct SatSys;

using std::map;
using std::vector;

constexpr double PI     = 3.141592653589793238462643383279502884197169399375105820974; /* pi */
constexpr double PI2    = (PI * 2);                                                    /* pi*2 */
constexpr double D2R    = (PI / 180.0);    /* deg to rad */
constexpr double R2D    = (180.0 / PI);    /* rad to deg */
constexpr double SC2RAD = 3.1415926535898; /* semi-circle to radian (IS-GPS) */
constexpr double AU     = 149597870691.0;  /* 1 AU (m) */
constexpr double AS2R   = (D2R / 3600.0);  /* arc sec to radian */

constexpr double TEC_CONSTANT = 40.308193e16;
constexpr double R2AS         = (1 / AS2R);
constexpr double MAS2R        = (2 * PI / (360 * 60 * 60 * 1000));
constexpr double MTS2R        = (2 * PI / (24 * 60 * 60 * 1000));
constexpr double S2MTS        = (1000.0);
constexpr double R2MAS        = (1 / MAS2R);
constexpr double R2MTS        = (1 / MTS2R);
constexpr double MTS2S        = (1 / S2MTS);
constexpr double G_CONST      = 6.67408E-11;
constexpr double RE_MEAN      = 6371000.0;  ///< mean Earth radius (m)
constexpr double RE_IGRF =
    6371200.0;  ///< geomagnetic conventional mean Earth radius for IGRF model (m)
constexpr double GM_Earth = 3.986004418e14;  ///< Geocentric gravitation constant (WGS84) [m^3/s^2]
constexpr double M_Earth  = 5.9722e24;
constexpr double GM_Moon  = 4.9027949e12;    ///< moon gravitation constant [m^3/s^2]
constexpr double MoonRadius = 1738200.0;     ///< Equatorial radius of the Moon [m]
constexpr double GM_Sun     = 1.327122E20;   ///< Heliocentric gravitation constant [m^3/s^2]
constexpr double SunRadius  = 695990000.0;   ///< Equatorial radius of the Sun [m], Seidelmann 1992
constexpr double ZEROC      = 273.15;
constexpr const char* SMOOTHED_SUFFIX = "_smoothed";
constexpr const char* FORWARD_SUFFIX  = "_forward";
constexpr const char* BACKWARD_SUFFIX = "_backward";

constexpr double CLIGHT = 299792458.0;    /* speed of light (m/s) */

constexpr double FREQ1     = 1.57542E9;   /* L1/E1  frequency (Hz) */
constexpr double FREQ2     = 1.22760E9;   /* L2     frequency (Hz) */
constexpr double FREQ5     = 1.17645E9;   /* L5/E5a frequency (Hz) */
constexpr double FREQ6     = 1.27875E9;   /* E6/LEX frequency (Hz) */
constexpr double FREQ7     = 1.20714E9;   /* E5b    frequency (Hz) */
constexpr double FREQ8     = 1.191795E9;  /* E5a+b  frequency (Hz) */
constexpr double FREQ1_GLO = 1.60200E9;   /* GLONASS G1 base frequency (Hz) */
constexpr double DFRQ1_GLO = 0.56250E6;   /* GLONASS G1 bias frequency (Hz/n) */
constexpr double FREQ2_GLO = 1.24600E9;   /* GLONASS G2 base frequency (Hz) */
constexpr double DFRQ2_GLO = 0.43750E6;   /* GLONASS G2 bias frequency (Hz/n) */
constexpr double FREQ3_GLO = 1.202025E9;  /* GLONASS G3 frequency (Hz) */
constexpr double FREQ4_GLO = 1.600995E9;  /* GLONASS G4 (CDMA G1) frequency (Hz) */
constexpr double FREQ6_GLO = 1.248060E9;  /* GLONASS G6 (CDMA G2) frequency (Hz) */
constexpr double FREQ1_CMP = 1.561098E9;  /* BeiDou B1 frequency (Hz) */
constexpr double FREQ2_CMP = 1.207140E9;  /* BeiDou B2 frequency (Hz) */
constexpr double FREQ3_CMP = 1.268520E9;  /* BeiDou B3 frequency (Hz) */
constexpr double FREQ9_IRN = 2.492028E9;  /* NavIC / IRNSS S9 frequency (Hz) */

constexpr double OMGE = 7.2921151467E-5;  /* earth angular velocity (IS-GPS) (rad/s) */

constexpr double RE_GLO = 6378136.0;      /* radius of earth (m)            ref [2] */
constexpr double MU_GPS = 3.9860050E14;   /* gravitational constant         ref [1] */
constexpr double MU_GLO = 3.9860044E14;   /* gravitational constant         ref [2] */
constexpr double MU_GAL = 3.986004418E14; /* earth gravitational constant   ref [7] */
constexpr double MU_CMP = 3.986004418E14; /* earth gravitational constant   ref [9] */
constexpr double MU     = MU_GPS;

constexpr double RE_WGS84 = 6378137.0;             /* earth semimajor axis (WGS84) (m) */
constexpr double FE_WGS84 = (1.0 / 298.257223563); /* earth flattening (WGS84) */

constexpr double IONO_HEIGHT = 350000.0;           /* ionosphere height (m) */

constexpr double JD2MJD   = 2400000.5;             /* JD to MJD */
constexpr double S_IN_DAY = 86400.0;               /* Number of seconds in a day */
constexpr double AUPerDay = (AU / 8.64e04);        /* AU/Day (IAU 2009)[m/s] */

constexpr double GRAVITY  = 9.80665;               /* mean gravity (m/s^2) */
constexpr double MOLARDRY = 0.028965;              /* molar mass dry air (kg/mol) */
constexpr double UGAS     = 8.3143;                /* universal gas constant (J/K/mol) */

constexpr double DTTOL       = 0.005;              /* tolerance of time difference (s) */
constexpr double MAXDTOE     = 14400.0;            /* max time difference to GPS Toe (s) */
constexpr double MAXDTOE_QZS = 7200.0;             /* max time difference to QZS Toe (s) */
constexpr double MAXDTOE_GAL = 9600.0;             /* max time difference to GAL Toe (s) */
constexpr double MAXDTOE_CMP = 7200.0;             /* max time difference to BDS Toe (s) */
constexpr double MAXDTOE_GLO = 7200.0;             /* max time difference to GLO Toe (s) */
constexpr double MAXDTOE_SBS = 360.0;              /* max time difference to SBAS Toe (s) */

constexpr double P2_5  = 3.125000000000000E-02;    /* 2^-5 */
constexpr double P2_6  = 1.562500000000000E-02;    /* 2^-6 */
constexpr double P2_10 = 9.765625000000000E-04;    /* 2^-10 */
constexpr double P2_11 = 4.882812500000000E-04;    /* 2^-11 */
constexpr double P2_12 = 2.441406250000000E-04;    /* 2^-12 */
constexpr double P2_15 = 3.051757812500000E-05;    /* 2^-15 */
constexpr double P2_17 = 7.629394531250000E-06;    /* 2^-17 */
constexpr double P2_19 = 1.907348632812500E-06;    /* 2^-19 */
constexpr double P2_20 = 9.536743164062500E-07;    /* 2^-20 */
constexpr double P2_21 = 4.768371582031250E-07;    /* 2^-21 */
constexpr double P2_23 = 1.192092895507813E-07;    /* 2^-23 */
constexpr double P2_24 = 5.960464477539063E-08;    /* 2^-24 */
constexpr double P2_27 = 7.450580596923828E-09;    /* 2^-27 */
constexpr double P2_28 = 3.725290298461914E-09;    /* 2^-28 */
constexpr double P2_29 = 1.862645149230957E-09;    /* 2^-29 */
constexpr double P2_30 = 9.313225746154785E-10;    /* 2^-30 */
constexpr double P2_31 = 4.656612873077393E-10;    /* 2^-31 */
constexpr double P2_32 = 2.328306436538696E-10;    /* 2^-32 */
constexpr double P2_33 = 1.164153218269348E-10;    /* 2^-33 */
constexpr double P2_34 = 5.820766091346741E-11;    /* 2^-34 */
constexpr double P2_35 = 2.910383045673370E-11;    /* 2^-35 */
constexpr double P2_38 = 3.637978807091713E-12;    /* 2^-38 */
constexpr double P2_39 = 1.818989403545856E-12;    /* 2^-39 */
constexpr double P2_40 = 9.094947017729282E-13;    /* 2^-40 */
constexpr double P2_41 = 4.547473508864641E-13;    /* 2^-41 */
constexpr double P2_43 = 1.136868377216160E-13;    /* 2^-43 */
constexpr double P2_46 = 1.421085471520200E-14;    /* 2^-46 */
constexpr double P2_48 = 3.552713678800501E-15;    /* 2^-48 */
constexpr double P2_50 = 8.881784197001252E-16;    /* 2^-50 */
constexpr double P2_55 = 2.775557561562891E-17;    /* 2^-55 */
constexpr double P2_59 = 1.734723475976807E-18;    /* 2^-59 */
constexpr double P2_66 = 1.355252715606881E-20;    /* 2^-66 */

extern map<E_Sys, map<E_ObsCode, E_FType>> code2Freq;
extern map<E_FType, double>                genericWavelength;
extern map<E_Block, vector<E_FType>>       blockTypeFrequencies;

// Filter signal codes based on block type capabilities
// Returns true if the signal code is supported by the given block type
bool isSignalSupportedByBlockType(E_ObsCode code, E_Block blockType);

extern const unsigned int tbl_CRC24Q[];

const unsigned char RTCM_PREAMBLE = 0xD3;

extern const boost::bimap<E_ObsCode, int> mCodes_gps;
extern const boost::bimap<E_ObsCode, int> mCodes_glo;
extern const boost::bimap<E_ObsCode, int> mCodes_gal;
extern const boost::bimap<E_ObsCode, int> mCodes_qzs;
extern const boost::bimap<E_ObsCode, int> mCodes_bds;
extern const boost::bimap<E_ObsCode, int> mCodes_sbs;

extern E_ObsCode freq2CodeHax(E_Sys sys, E_FType ft);
