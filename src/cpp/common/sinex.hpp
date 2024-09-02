
#pragma once


#include <fstream>
#include <vector>
#include <string>
#include <list>
#include <map>

using std::vector;
using std::string;
using std::list;
using std::map;

#include "eigenIncluder.hpp"
#include "trace.hpp"
#include "gTime.hpp"
#include "enums.h"

//===============================================================================
/* history structure (optional but recommended)
* ------------------------------------------------------------------------------
+INPUT/HISTORY
*CSNX FMT_ AGC EPOCH_______ AGD START_______ STOP________ T EST__ C A B C D E F
+SNX 1.23 XXX YR:DOY:SOD.. YYY YR:DOY:SOD   YR:DOY:SOD   C 01234 D S O E T C A
*/
struct SinexInputHistory
{
	char	code; // '+' for input, '-' for output
	double	fmt; // format (d4.2)
	string	create_agency; // 3
	UYds	create_time;
	string	data_agency; // 3
	UYds	start;
	UYds	stop;
	char	obs_tech; //
	int		num_estimates;
	char	constraint;
	string	contents; // S/O/E/T/C/A separated characters may have trailing blanks
};

/* files structure (optional)
* ------------------------------------------------------------------------------
+INPUT/FILES
*AGC EPOCH_______ FILE_NAME____________________ _FILE_DESCRIPTION_______________
 XXX YR:DOY:SOD.. ABCDEFGHIJKLMNOPQRSTUVWXYZABC ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEF
*/
struct SinexInputFile
{
	string	agency; // 3
	UYds	yds;
	string	file; // 29 - name of file
	string	description; // 32 - description of file
};

/* acknowledgements structure (optional)
* ------------------------------------------------------------------------------
+INPUT/ACKNOWLEDGEMENTS
*AGC DESCRIPTION_________________________________________________________________
 XXX BLAH BLAH BLAH ...
*/
struct SinexAck
{
	string agency; // 3
	string description; // 75
};

/* nut-data structure (mandatory when VLBI)
* ------------------------------------------------------------------------------
+NUTATION/DATA
*CODE____ COMMENTS_______________________________________________________________
 NUTCODE  BLAH BLAH BLAH ...
*/
struct SinexNutCode
{
	string nutcode; // 8 - one of IAU1980/IERS1996/IAU2000a/IAU2000b
	string comment; // 70 - description of model
};

/* precess-data structure (mandatory when VLBI)
* ------------------------------------------------------------------------------
+PRECESSION/DATA
*CODE____ COMMENTS_______________________________________________________________
 PRECODE  BLAH BLAH BLAH ...
*/
struct SinexPreCode
{
	string precesscode; // 8 - one of IAU1976/IERS1996
	string comment; // 70 - description of model
};

/* source id structure (mandatory when VLBI)
+SOURCE/ID
*CODE IERSDESC ICRF_DESCRIPTION COMMENTS_________________________________________
 CCCC DESCRIPT DESCRIPTION      BLAH BLAH BLAH ...
*/
struct SinexSourceId
{
	string source; // 4 - call sign
	string iers;   // 8 - 8 char designation
	string icrf;   // 16 - 16 char designation
	string comments; // 58
};

// site/id block structurea (mandatory for GNSS)
/*
+SITE/ID
*CODE PT __DOMES__ T _STATION DESCRIPTION__ _LONGITUDE_ _LATITUDE__ HEIGHT_
*/
struct SinexSiteId
{
	string	sitecode;	// station (4)
	string	ptcode;		// physical monument used at the site (2)
	char	typecode;	// observation technique {C,D,L,M,P,or R}
	string	domes;		// domes number unique monument num (9)
	string	desc;		// site description eg town/city (22)
	int		lon_deg;	// longitude degrees (uint16_t) east is positive
	int		lon_min;	//
	double	lon_sec;	//
	int		lat_deg;	// latitude degrees north is positive
	int		lat_min;	// uint8_t
	double	lat_sec;	// float
	double	height;		//
	bool	used = false;
};

// site/data block structure (optional)
/*
+SITE/DATA
*CODE PT SOLN CODE PT SOLN O START___ STOP____ AGC CREATE___
*/
struct SinexSiteData
{
	string	site;       // 4 call sign for solved parameters
	string	station_pt; // 2 physical
	string	soln_id;    // 4 solution number to which this input is referred to (int?)
	string	sitecode;   // 4 call sign from input sinex file
	string	site_pt;    // 2 physical from above
	string	sitesoln;   // 4 solution number for site/pt from input sinex file
	char	obscode;    //
	UYds	start;
	UYds	stop;
	string	agency;     // 3 - code agency of creation
	UYds	create;
};

/* receiver block structre (mandatory for GNSS)
+SITE/RECEIVER
*CODE PT SOLN T _DATA START_ __DATA_END__ ___RECEIVER_TYPE____ _S/N_ _FIRMWARE__
 ALBH  A ---- C 93:327:70260 94:016:15540 ROGUE SNR-8C         313   Meenix 7.4
 ALBH  A ---- C 94:016:15540 95:011:80100 ROGUE SNR-8000       168   SFG2 0.0 lk
*/
struct SinexReceiver
{
	string	sitecode;   // station (4)
	string	ptcode;     // physical monument used at the site  (2)
	string	solnid;     // solution number (4) or '----'
	char	typecode;
	UYds	start;        // receiver start time
	UYds	end;          // receiver end time
	string	type;     // receiver type (20)
	string	sn;      // receiver serial number (5)
	string	firm;    // receiver firmware  (11)
	bool	used = false;
};

/* antenna block structure (mandatory for GNSS)
+SITE/ANTENNA
*CODE PT SOLN T _DATA START_ __DATA_END__ ____ANTENNA_TYPE____ _S/N_
 ALBH  A ---- C 92:125:00000 94:104:74100 AOAD/M_B        EMRA 91119
 ALBH  A ---- C 94:104:74100 95:011:80100 AOAD/M_T        EMRA 92172
*/
struct SinexAntenna
{
	string	sitecode;
	string	ptcode;   // physical monument used at the site (2)
	string	solnnum;
	string	calibModel;
	char	typecode;
	UYds	start;        /* antenna start time */
	UYds	end;          /* antenna end time  */
	string	type;    /* receiver type (20)*/
	string	sn;      /* receiver serial number (5)*/
	bool	used = false;
};

/* gps phase centre block structure (mandatory for GPS)
+SITE/GPS_PHASE_CENTER
*ANT_TYPE_AND_MODEL__ SERNO L1UOFF L1NOFF L1EOFF L2UOFF L2NOFF L2EOFF CALIBMODEL
*/
struct SinexGpsPhaseCenter
{
	string		antname;  // 20 name and model
	string		serialno; // 5
	Vector3d	L1;    // UNE d6.4*3
	Vector3d	L2;    // UNE d6.4*3
	string		calib;    // 10 calibration model
};

/* gal phase centre block structure (mandatory for Gallileo)
* NB 3 lines for each one!
+SITE/GAL_PHASE_CENTER
*ANT_TYPE_AND_MODEL__ SERNO L1UOFF L1NOFF L1EOFF L5UOFF L5NOFF L5EOFF CALIBMODEL
*ANT_TYPE_AND_MODEL__ SERNO L6UOFF L6NOFF L6EOFF L7UOFF L7NOFF L7EOFF CALIBMODEL
*ANT_TYPE_AND_MODEL__ SERNO L8UOFF L8NOFF L8EOFF BLANK_ BLANK_ BLANK_ CALIBMODEL
*/
struct SinexGalPhaseCenter
{
	string		antname;  // 20 name and model
	string		serialno; // 5
	Vector3d	L1;    // UNE d6.4*3
	Vector3d	L5;    // UNE d6.4*3
	Vector3d	L6;    // UNE d6.4*3
	Vector3d	L7;    // UNE d6.4*3
	Vector3d	L8;    // UNE d6.4*3
	string		calib;    // 10 calibration model
};

/*
+SITE/ECCENTRICITY
*CODE PT SOLN T _DATA START_ __DATA_END__ REF __DX_U__ __DX_N__ __DX_E__
 ALBH  A ---- C 92:146:00000 94:104:74100 UNE   0.1260   0.0000   0.0000
*/
struct SinexSiteEcc
{
	string		sitecode;		//4
	string		ptcode;			//2 - physical monument used at the site
	string		solnnum;
	char		typecode;
	UYds		start;			/* ecc start time */
	UYds		end;			/* ecc end time */
	string		rs;				/* 3 - reference system UNE (0) or XYZ (1) */
	VectorEnu	ecc;			/* eccentricity UNE or XYZ (m) d8.4*3 */
	bool		used = false;
};

/*
+SOLUTION/EPOCHS (mandatory) *OR*
+BIAS/EPOCHS (mandatory when biases are included)
*CODE PT SOLN T _DATA_START_ __DATA_END__ _MEAN_EPOCH_
 ALBH  A    1 C 94:002:00000 94:104:00000 94:053:00000
*/
struct SinexSolEpoch
{
	string	sitecode; //4
	string	ptcode;   //2 - physical monument used at the site
	string	solnnum;
	char	typecode;
	UYds	start;
	UYds	end;
	UYds	mean;
};

/*
+SOLUTION/STATISTICS
*STAT_NAME (30 chars) value (22char double)
*/
struct SinexSolStatistic
{
	string name;
	short  etype; // 0 = int, 1 = double
	union
	{
		int		ival;
		double	dval;
	} value;
};


/*
+SOLUTION/ESTIMATE
*INDEX _TYPE_ CODE PT SOLN _REF_EPOCH__ UNIT S ___ESTIMATED_VALUE___ __STD_DEV__
	1 STAX   ALBH  A    1 10:001:00000 m    2 -2.34133301687257e+06 5.58270e-04
	2 STAY   ALBH  A    1 10:001:00000 m    2 -3.53904951624333e+06 7.77370e-04
	3 STAZ   ALBH  A    1 10:001:00000 m    2  4.74579129951391e+06 8.98560e-04
	4 VELX   ALBH  A    1 10:001:00000 m/y  2 -9.92019926884722e-03 1.67050e-05
	5 VELY   ALBH  A    1 10:001:00000 m/y  2 -8.46787398931193e-04 2.12080e-05
	6 VELZ   ALBH  A    1 10:001:00000 m/y  2 -4.85721729753769e-03 2.39140e-05
*/
struct SinexSolEstimate
{
	int		index;
	string	type;  //6
	string	sitecode; //4
	string	ptcode;   //2 - physical monument used at the site
	string	solnnum;
	UYds	refepoch;
	string	unit; //4
	char	constraint;
	double	estimate;
	double	stddev;
	string	file;

	bool	used = false;
};

/*
+SOLUTION/APRIORI
*INDEX PARAMT SITE PT SOLN EPOCH_____ UNIT C PARAM________________ STD_DEV____
 12345 AAAAAA XXXX YY NNNN YR:DOY:SOD UUUU A 12345.123456789ABCDEF 1234.123456
*/
struct SinexSolApriori
{
	int		idx;
	string	param_type; // 6 - select from
	string	sitecode;   // 4
	string	ptcode;     // 2
	string	solnnum;
	UYds	epoch;
	string	unit;       // 4 - select from
	char	constraint; // for inner constraints, choose 1
	double	param;      // d21.15 apriori parameter
	double	stddev;     // std deviation of parameter
};

/*
+SOLUTION/NORMAL_EQUATION_VECTOR
*PARAM PTYPE_ SITE PT SOLN EPOCH_____ UNIT C NORMAL______________
 12345 AAAAAA XXXX YY NNNN YR:DOY:SOD UUUU A 12345.123456789ABCDEF
*/
struct SinexSolNeq
{
	int	param; // 5 index of estimated parameters
	string	ptype; // 6 - type of parameter
	string	site;  // 4 - station
	string	pt;    // 2 - point code
	string	solnnum;     // 4 solution number
	UYds	epoch;
	string	unit;  // 4
	char	constraint; //
	double	normal; // right hand side of normal equation
};

/*
+SOLUTION/MATRIX_ESTIMATE C TYPE (mandatory)
+SOLUTION/MATRIX_APRIORI C TYPE (recommended)
+SOLUTION/MATRIX_NORMAL_EQUATION C (mandatory for normal equations)
* (Not used until I understand it better)
* C must be L or U (matrix is always symmetric about main diagonal)
* TYPE must be one of CORR/COVA/INFO for correlation, covariance and info (covariance inverse)
* APRIORI VALUES are 21.16lf, estimates and normal_equations are 21.14lf!
*ROW__ COL__ ELEM1________________ ELEM2________________ ELEM3________________
*/
struct SinexSolMatrix
{
	int		row;   // 5 - must match the solution/estimate row
	int		col;   // 5 - must match the solution/estimate col
	int		numvals;
	double	value[3]; // each d21.14 cols col, col+1, col+2 of the row
};


//=============================================================================
/*
+SOLUTION/DATA_HANDLING
*CODE PT UNIT T _DATA_START_ __DATA_END__ M __E-VALUE___ STD_DEV _E-RATE__ CMNTS
 7090 -- ms   A 09:344:31560 09:345:70200 T      0.90920
 7840 -- %    A 95:358:00000 95:358:86400 H     -20.00                     HER
 7080 -- mB   A 95:065:00000 96:026:00000 P        -2.10
 1873 -- mm   A 95:001:00000 00:001:00000 R      -270.00
*/
//=============================================================================
struct SinexDataHandling
{
	string	sitecode;	//4 - CDP ID
	string	ptcode;		//2 - satellites these biases apply to (-- = all)
	string	solnnum;    //4 - solution number
	string	t;			//1
	UYds	epochstart;	//yr:doy:sod
	UYds	epochend;	//yr:doy:sod
	string	m;			//1
	double	estimate;
	double	stddev;
	double	estrate;
	string	unit;		//4 - units of estimate
	string	comments;	//4
} ;

typedef enum
{
	ESTIMATE,
	APRIORI,
	NORMAL_EQN,
	MAX_MATRIX_TYPE
} matrix_type;

typedef enum
{
	CORRELATION,
	COVARIANCE,
	INFORMATION,
	MAX_MATRIX_VALUE
} matrix_value;

typedef enum
{
	P_ANT,	// P: antenna //todo: check the meaning of 'P'
	L_LRA	// L: laser retroreflector array
} E_EccType;

//=============================================================================
// TODO: satid and satident/satprn need to be checked for consistency ...
/*
+SATELLITE/ID (recommmended for GNSS)
*SVN_ PR COSPAR_ID O TSL        TUD        ANTENNA_RCV_TYPE_______________
 G001 04 1978-020A A yy:doy:sod yy:doy:sod BLAH BLAH BLAH ...
*/
struct SinexSatId
{
	string	svn;     // 4
	string	prn;     // 2
	string	cospar;  // 9
	char	obsCode; //
	UYds	timeSinceLaunch; // yy:doy:sod a value of 0 everywhere means launched before file epoch start
	UYds	timeUntilDecom;  // yy:doy:sod a value of 0 everywhere mean still in commission after file epoch end
	string	antRcvType; // 20 - satellite antenna receiver type
};

/*
+SATELLITE/IDENTIFIER
*SVN_ COSPAR_ID SatCat Block__________ Comment__________________________________
 G001 1978-020A  10684 GPS-I           Launched 1978-02-22; NAVSTAR 1
*/
struct SinexSatIdentity
{
	string	svn; // 4
	string	cospar; // 9
	int		category; // 6
	string	blocktype; // 15
	string	comment; // 42
};

/*
+SATELLITE/PRN
*SVN_ Valid_From____ Valid_To______ PRN Comment_________________________________
 G001 1978:053:00000 1985:199:00000 G04
*/
struct SinexSatPrn
{
	string	svn; // 4
	UYds	start; //yr:doy:sod
	UYds	stop; //yr:doy:sod
	string	prn; //3
	string	comment; // 40
};

/* ONLY FOR GLONASS!
+SATELLITE/FREQUENCY_CHANNEL
*SVN_ Valid_From____ Valid_To______ chn Comment________________________________
 R701 2003:344:00000 2009:258:86399   1 [FC10]
*/
struct SinexSatFreqChn
{
	string	svn; // 4
	UYds	start;
	UYds	stop;
	int		channel;
	string	comment; // 40?
};

/*
+SATELLITE/MASS
*SVN_ Valid_From____ Valid_To______ Mass_[kg] Comment___________________________
 G001 1978:053:00000 0000:000:00000   455.000 GPS-I; [MA01]
*/
struct SinexSatMass
{
	string	svn; // 4
	UYds	start;
	UYds	stop;
	double	mass; // kg
	string	comment; // 40
};

/*
+SATELLITE/COM  (Centre of Mass)
*SVN_ Valid_From____ Valid_To______ ____X_[m] ____Y_[m] ___ Z_[m] Comment___________________________
 G001 1978:053:00000 0000:000:00000    0.0000    0.0000    0.0000 GPS-I; [CM02]
*/
struct SinexSatCom
{
	string		svn; // 4
	UYds		start;
	UYds		stop;
	Vector3d	com; //x/y/z (metres)
	string		comment; // 40
};

/*
+SATELLITE/ECCENTRICITY
*SVN_ Equipment___________ T ____X_[m] ____Y_[m] ____Z_[m] Comment___________________________
 G001 BLOCK I              P    0.0000    0.0000    0.0000 GPS-I; [EG02]
*/
struct SinexSatEcc
{
	string		svn;		// 4
	string		equip;		//  20
	char		type;		// L or P - both can exist for the same satellite
	VectorEnu	ecc;
	string		comment;	// 40
};

/*
+SATELLITE/TX_POWER
*SVN_ Valid_From____ Valid_To______ P[W] Comment________________________________
 G001 1978:053:00000 0000:000:00000   50  GPS-I; same as GPS-II/IIA; [TP04]
*/
struct SinexSatPower
{
	string	svn; // 4p
	UYds	start; //yr:doy:sod
	UYds	stop; //yr:doy:sod
	int		power;   // watts
	string	comment; // 40
};

/*
+SATELLITE/PHASE_CENTER
*NB Can have more than one line if satellite transmits on more than 2 frequencies
*SVN_ C ZZZZZZ XXXXXX YYYYYY C ZZZZZZ XXXXXX YYYYYY ANTENNA___ T M
*/
struct SinexSatPc
{
	string		svn;     // 4
	char		freq;    // 1/2/5 for GPS & GLONASS, 1/5/6/7/8 for Gallileo
	Vector3d	zxy;  // metres offset from COM in the order given 3* d6.4
	char		freq2;   // as above
	Vector3d	zxy2; // as above
	string		antenna; // 10 - model of antenna
	char		type;    // Phase Center Variation A(bsolute)/R(elative)
	char		model;   // F(ull)/E(levation model only)
};

/*
+SATELLITE/YAW_BIAS_RATE
*SVN_ Valid_From____ Valid_To______   YB Yaw Rate  Comment________________________________
 G001 1978:053:00000 1985:199:00000    U   0.1999  Launched 1978-02-22; NAVSTAR 1; mass 453800. in previous svnav.dat.allgnss
*/
struct SinexSatYawRate
{
	string	svn;		///< SVN
	UYds	start;	///< valid from (yr:doy:sod)
	UYds	stop;	///< valid until (yr:doy:sod)
	char	yawBias;	///< yaw bias
	double	maxYawRate;	///< maximum yaw rate for SV (rad/s)
	string	comment;	///< comment field
};

/*
+SATELLITE/ATTITUDE_MODE
*SVN_ DATE_TIME_START(UTC) END(UTC)___________ ATTITUDE_MODE
 J001 2011-02-16 10:32:00  2011-04-20 07:40:00 ON
*/
struct SinexSatAttMode
{
	string	svn;		///< SVN
	GEpoch	start;		///< valid from (yyyy-mm-dd hh-mm-ss)
	GEpoch	stop;		///< valid until (yyyy-mm-dd hh-mm-ss)
	string	attMode;	///< attitude mode
};

/*
+TROP/DESCRIPTION
*_________KEYWORD_____________ __VALUE(S)_______________________________________
 ELEVATION CUTOFF ANGLE                             7
 SAMPLING INTERVAL                                300
 SAMPLING TROP                                    300
 TROP MAPPING FUNCTION         WET GMF
 SOLUTION_FIELDS_1             TROTOT STDDEV TGNTOT STDDEV TGETOT STDDEV
*/
struct SinexTropDesc
{
	map<string, string>			strings;		//1X,A22
	map<string, int>			ints;			//1X,I22
	map<string, double>			doubles;		//1X,F22
	map<string, vector<string>>	vecStrings;		//1X,n(1X,A6)
	map<string, vector<double>>	vecDoubles;		//1X,F5.2,1X,F5.2,1X,F8.1
	bool						isEmpty = true;
};

/*
+TROP/SOLUTION
*SITE ____EPOCH___ TROTOT STDDEV  TGNTOT STDDEV  TGETOT STDDEV
 WIDC 21:014:00000 2270.0    2.8  -0.186  0.376   0.788  0.464
*/
struct SinexTropSol
{
	string	site;
	UYds	yds;
	struct TropSolutionEntry	//first Sinex_tropsol_t entry also used in TROP/DESCRIPTION block
	{
		string	type;
		double	value;
		double	units;
		int		width;
	};
	vector<TropSolutionEntry> solutions; //map not used b/c may have multiple STDDEV entries
};

struct Sinex
{
	string currentFile;

	/* header block */
	string	snxtype;				/* SINEX file type */
	double	ver;					/* version */
	string	createagc;				/* file creation agency */
	UYds	filedate;				/* file create date as yr:doy:sod */
	string	dataagc;				/* data source agency */
	UYds	solutionstartdate;	// start date of solution
	UYds	solutionenddate;
	char	obsCode;				/* observation code */
	int		numparam;				/* number of estimated parameters */
	char	constCode;				/* constraint code */
	string	solcont;				/* solution types S O E T C A */
	string	markerName;

	map<string, list<string>>		blockComments;
	list<string>					refstrings;
	list<SinexInputHistory>			inputHistory;
	list<SinexInputFile>			inputFiles;
	list<SinexAck>					acknowledgements;

	/* site stuff */
	map<string, SinexSiteId>										mapsiteids;
	list<SinexSiteData>												listsitedata;
	map<string, map<GTime, SinexReceiver,	std::greater<GTime>>>	mapreceivers;
	map<string, map<GTime, SinexAntenna,	std::greater<GTime>>>	mapantennas;
	map<string, map<GTime, SinexSiteEcc,	std::greater<GTime>>>	mapeccentricities;
	list<SinexGpsPhaseCenter>										listgpspcs;
	list<SinexGalPhaseCenter>										listgalpcs;

	/* solution stuff - tied to sites */
	bool																					epochshavebias;
	map<string, SinexSolEpoch>																solEpochMap;
// 	list<SinexSolEpoch>																		list_solepochs;
	list<SinexSolStatistic>																	liststatistics;
	map<string, map<string, map<GTime, SinexSolEstimate, std::greater<GTime>>>>				estimatesMap;
	map<int, SinexSolApriori>																apriorimap;
	list<SinexSolNeq>																		listnormaleqns;
	map<matrix_value,list<SinexSolMatrix>>													matrixmap[MAX_MATRIX_TYPE];
	map<string,	map<string,	map<char, map<GTime, SinexDataHandling, std::greater<GTime>>>>>	mapdatahandling;

	/* satellite stuff */
	list<SinexSatPc>						listsatpcs;
	list<SinexSatId>						listsatids;
	map<string, SinexSatIdentity>			satIdentityMap;

	map<string, map<GTime, SinexSatMass>>	mapsatmasses;
	map<string, map<GTime, SinexSatPower>>	mapsatpowers;

	list<SinexSatPrn>						listsatprns;
	list<SinexSatFreqChn>					listsatfreqchns;
	list<SinexSatCom>						listsatcoms;
	list<SinexSatEcc>						listsateccs;

	map<string, map<GTime, SinexSatYawRate,		std::greater<GTime>>>	satYawRateMap;
	map<string, map<GTime, SinexSatAttMode,		std::greater<GTime>>>	satAttModeMap;

	/* VLBI - ignored for now */
	list<SinexSourceId>		listsourceids;
	list<SinexNutCode>		listnutcodes;
	list<SinexPreCode>		listprecessions;

	// constructor
	Sinex(
		bool epochshavebias = false)
	:	epochshavebias(epochshavebias)
	{

	};

	// Troposphere Sinex data
	map<string, int>		tropSiteCoordBodyFPosMap;
	map<string, int>		tropSolFootFPosMap;
	SinexTropDesc			tropDesc = {};
	map<string, VectorEcef>	tropSiteCoordMapMap; //indexed by station ID, then axis #
	list<SinexTropSol>		tropSolList;
};

struct Sinex_stn_soln
{
	string 	type;					/* parameter type */
	string  unit;					/* parameter units */
	double  pos		= 0;			/* real position (ecef) (m)*/
	double  pstd	= 0;			/* position std (m) */
	UYds	yds;					/* epoch when valid */
};


extern SinexSatIdentity		dummySinexSatIdentity;
extern SinexSatEcc			dummySinexSatEcc;

/* satellite meta data */
struct SinexSatSnx
{
	string 				svn;
	string				prn;
	SinexSatIdentity*	id_ptr		= &dummySinexSatIdentity;
	SinexSatEcc*		ecc_ptrs[2]	={&dummySinexSatEcc, &dummySinexSatEcc};
	double 				mass;				/* kg */
	int					channel;			/* GLONASS ONLY */
	Vector3d			com;				/* centre of mass offsets (m) */
	int					power;				/* Tx Power (watts); */

	string				antenna;
	int					numfreqs;			/* number of phase center frequencies */
	char				freq[5];			/* 1/2/5/6/7/8 (up to 5 freqs allowed) */
	Vector3d			zxy[5];				/* phase offsets, order given by var name! */
	char				pctype;
	char				pcmodel;

	UYds				start;
	UYds				stop;
};


void nearestYear(
	double&	year);

bool readSinex(
	const string& filepath);

struct KFState;
struct Receiver;

void  writeSinex(
	string					filepath,
	KFState&				kfState,
	map<string, Receiver>&	receiverMap);

struct SinexRecData;

union GetSnxResult
{
	const unsigned int failure = 0;
	struct
	{
		unsigned failureSiteId			: 1;
		unsigned failureReceiver		: 1;
		unsigned failureAntenna			: 1;
		unsigned failureEccentricity	: 1;
		unsigned failurePhaseCentre		: 1;
		unsigned failureEstimate		: 1;
		unsigned failurePRN				: 1;
		unsigned failureSatId			: 1;
		unsigned failureCOM				: 1;
	};
};


GetSnxResult	getRecSnx		(string id,				GTime time, SinexRecData&		snx);
GetSnxResult	getSatSnx		(string prn,			GTime time, SinexSatSnx&		snx);
void			getSlrRecBias	(string id,	string prn,	GTime time, map<char, double>&	recBias);

void	sinexAddStatistic(const string& what, const int		value);
void	sinexAddStatistic(const string& what, const double	value);
int		sinexCheckAddGaReference(string solType, string peaVer, bool isTrop);
void	sinexAddAcknowledgement(const string& who, const string& description);
void	sinexAddComment(const string what);
void	sinexAddFiles(const string& who, const GTime& when, const vector<string>& filenames, const string& description);

void updateSinexHeader(
	string& 	create_agc,
	string&		data_agc, /* satellite meta data */
	UYds		soln_start,
	UYds		soln_end,
	const char	obsCode,
	const char	constCode,
	string&		contents,
	int			numParam,
	double		sinexVer);

void sinexPostProcessing(
	GTime					time,
	map<string, Receiver>&	receiverMap,
	KFState&				netKFState);

void sinexPerEpochPerStation(
	Trace&		trace,
	GTime		time,
	Receiver&	rec);


// Trop sinex
void outputTropSinex(
	string					filename,
	GTime					time,
	KFState&				netKfState,
	string					markerName = "MIX",
	bool					isSmoothed = false);

// snx.cpp fns used in tropSinex.cpp
void writeAsComments(
	Trace&			out,
	list<string>&	comments);

void writeSnxReference(
	std::ofstream&	out);

bool getSnxSatMaxYawRate(
	string	prn,
	GTime&	time,
	double&	maxYawRate);

bool getSnxSatBlockType(
	string	svn,
	string&	blockType);

bool getSnxSatAttMode(
	string	svn,
	GTime&	time,
	string&	attMode);

extern Sinex theSinex; // the one and only sinex object.

void getReceiversFromSinex(
	map<string, Receiver>&	receiverMap,
	KFState&				kfState);

