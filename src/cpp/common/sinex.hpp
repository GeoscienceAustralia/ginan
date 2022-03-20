
#ifndef NEWSNX_H
#define NEWSNX_H


#include <fstream>
#include <string>
#include <list>
#include <map>

using std::string;
using std::list;
using std::map;

#include "eigenIncluder.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "enums.h"


//===============================================================================
/* reference structure (mandatory)
+FILE/REFERENCE
<any>
*/
struct Sinex_ref_t
{
	string refline; //name of file used to construct the output
	// but store anything already there just in case
} ;

//===============================================================================
/* comment structure (optional but recommended)
+FILE/COMMENT
<any>
*/
struct Sinex_comment_t
{
	string cmtline; // some comment
} ;

//===============================================================================
/* history structure (optional but recommended)
* ------------------------------------------------------------------------------
+INPUT/HISTORY
*CSNX FMT_ AGC EPOCH_______ AGD START_______ STOP________ T EST__ C A B C D E F
+SNX 1.23 XXX YR:DOY:SOD.. YYY YR:DOY:SOD   YR:DOY:SOD   C 01234 D S O E T C A
*/
struct Sinex_input_history_t
{
	char    code; // '+' for input, '-' for output
	double  fmt; // format (d4.2)
	string  create_agency; // 3
	int     create_time[3]; // yr:doy:sod
	string  data_agency; // 3
	int     start[3]; //yr::doy:sod
	int     stop[3]; //yr:doy:sod
	char    obs_tech; //
	int     num_estimates;
	char    constraint;
	string  contents; // S/O/E/T/C/A separated characters may have trailing blanks
} ;

//===============================================================================
/* files structure (optional)
* ------------------------------------------------------------------------------
+INPUT/FILES
*AGC EPOCH_______ FILE_NAME____________________ _FILE_DESCRIPTION_______________
XXX YR:DOY:SOD.. ABCDEFGHIJKLMNOPQRSTUVWXYZABC ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEF
*/
struct Sinex_input_file_t
{
	string agency; // 3
	int    epoch[3]; // yr:doy:sod
	string file; // 29 - name of file
	string description; // 32 - description of file
} ;

//===============================================================================
/* acknowledgements structure (optional)
* ------------------------------------------------------------------------------
+INPUT/ACKNOWLEDGEMENTS
*AGC DESCRIPTION_________________________________________________________________
XXX BLAH BLAH BLAH ...
*/
struct Sinex_ack_t
{
	string agency; // 3
	string description; // 75
} ;

//===============================================================================
/* nut-data structure (mandatory when VLBI)
* ------------------------------------------------------------------------------
+NUTATION/DATA
*CODE____ COMMENTS_______________________________________________________________
NUTCODE  BLAH BLAH BLAH ...
*/
struct Sinex_nutcode_t
{
	string nutcode; // 8 - one of IAU1980/IERS1996/IAU2000a/IAU2000b
	string comment; // 70 - description of model
} ;

//===============================================================================
/* precess-data structure (mandatory when VLBI)
* ------------------------------------------------------------------------------
+PRECESSION/DATA
*CODE____ COMMENTS_______________________________________________________________
PRECODE  BLAH BLAH BLAH ...
*/
struct Sinex_precode_t
{
	string precesscode; // 8 - one of IAU1976/IERS1996
	string comment; // 70 - description of model
} ;

//===============================================================================
/* source id structure (mandatory when VLBI)
* ------------------------------------------------------------------------------
+SOURCE/ID
*CODE IERSDESC ICRF_DESCRIPTION COMMENTS_________________________________________
CCCC DESCRIPT DESCRIPTION      BLAH BLAH BLAH ...
*/
struct Sinex_source_id_t
{
	string source; // 4 - call sign
	string iers;   // 8 - 8 char designation
	string icrf;   // 16 - 16 char designation
	string comments; // 58
} ;

//===============================================================================
// site/id block structurea (mandatory for GNSS)
/*
*-------------------------------------------------------------------------------
+SITE/ID
*CODE PT __DOMES__ T _STATION DESCRIPTION__ _LONGITUDE_ _LATITUDE__ HEIGHT_
*/
//===============================================================================
struct Sinex_siteid_t
{
	string sitecode; // station (4)
	string ptcode;   // physical monument used at the site (2)
	char typecode;   // observation technique {C,D,L,M,P,or R}
	string domes;    // domes number unique monument num (9)
	string desc;     // site description eg town/city (22)
	int long_deg;  // longitude degrees (uint16_t) east is positive
	int long_min;   //
	double long_sec;  //
	int lat_deg;   // latitude degrees north is positive
	int lat_min;    // uint8_t
	double lat_sec;   // float
	double height;   //
	bool used = false;
} ;

//===============================================================================
// site/data block structure (optional)
/*
*-------------------------------------------------------------------------------
+SITE/DATA
*CODE PT SOLN CODE PT SOLN O START___ STOP____ AGC CREATE___
*/
//===============================================================================
struct Sinex_sitedata_t
{
	string site;       // 4 call sign for solved parameters
	string station_pt; // 2 physical
	string soln_id;    // 4 solution number to which this input is referred to (int?)
	string sitecode;   // 4 call sign from input sinex file
	string site_pt;    // 2 physical from above
	string sitesoln;   // 4 solution number for site/pt from input sinex file
	char   obscode;    //
	int    start[3];   // yr:doy:sod
	int    stop[3];    // yr:doy:sod
	string agency;     // 3 - code agency of creation
	int    create[3];  // yr:doy:sod
} ;

//=============================================================================
/* receiver block structre (mandatory for GNSS)
+SITE/RECEIVER
*CODE PT SOLN T _DATA START_ __DATA_END__ ___RECEIVER_TYPE____ _S/N_ _FIRMWARE__
ALBH  A ---- C 93:327:70260 94:016:15540 ROGUE SNR-8C         313   Meenix 7.4
ALBH  A ---- C 94:016:15540 95:011:80100 ROGUE SNR-8000       168   SFG2 0.0 lk
*/
//=============================================================================
struct Sinex_receiver_t
{
	string  sitecode;   // station (4)
	string  ptcode;     // physical monument used at the site  (2)
	string  solnid;     // solution number (4) or '----'
	char    typecode;
	int     recstart[3];        /* receiver start time YY:DOY:SOD */
	int     recend[3];          /* receiver end time YY:DOY:SOD */
	string  rectype;     // receiver type (20)
	string  recsn;      // receiver serial number (5)
	string  recfirm;    // receiver firmware  (11)
	bool	used = false;
} ;

//=============================================================================
/* antenna block structure (mandatory for GNSS)
+SITE/ANTENNA
*CODE PT SOLN T _DATA START_ __DATA_END__ ____ANTENNA_TYPE____ _S/N_
ALBH  A ---- C 92:125:00000 94:104:74100 AOAD/M_B        EMRA 91119
ALBH  A ---- C 94:104:74100 95:011:80100 AOAD/M_T        EMRA 92172
*/
//=============================================================================
struct Sinex_antenna_t
{
	string  sitecode;
	string  ptcode;   // physical monument used at the site (2)
	string  solnnum;
	string  calibModel;
	char    typecode;
	int     antstart[3];        /* antenna start time YY:DOY:SOD */
	int     antend[3];          /* antenna end time YY:DOY:SOD */
	string  anttype;    /* receiver type (20)*/
	string  antsn;      /* receiver serial number (5)*/
	bool	used = false;
} ;

//=============================================================================
/* gps phase centre block structure (mandatory for GPS)
+SITE/GPS_PHASE_CENTER
*ANT_TYPE_AND_MODEL__ SERNO L1UOFF L1NOFF L1EOFF L2UOFF L2NOFF L2EOFF CALIBMODEL
*/
struct Sinex_gps_phase_center_t
{
	string antname;  // 20 name and model
	string serialno; // 5
	double L1[3];    // UNE d6.4*3
	double L2[3];    // UNE d6.4*3
	string calib;    // 10 calibration model
} ;

//=============================================================================
/* gal phase centre block structure (mandatory for Gallileo)
* NB 3 lines for each one!
+SITE/GAL_PHASE_CENTER
*ANT_TYPE_AND_MODEL__ SERNO L1UOFF L1NOFF L1EOFF L5UOFF L5NOFF L5EOFF CALIBMODEL
*ANT_TYPE_AND_MODEL__ SERNO L6UOFF L6NOFF L6EOFF L7UOFF L7NOFF L7EOFF CALIBMODEL
*ANT_TYPE_AND_MODEL__ SERNO L8UOFF L8NOFF L8EOFF BLANK_ BLANK_ BLANK_ CALIBMODEL
*/
struct Sinex_gal_phase_center_t
{
	string antname;  // 20 name and model
	string serialno; // 5
	double L1[3];    // UNE d6.4*3
	double L5[3];    // UNE d6.4*3
	double L6[3];    // UNE d6.4*3
	double L7[3];    // UNE d6.4*3
	double L8[3];    // UNE d6.4*3
	string calib;    // 10 calibration model
} ;

//=============================================================================
/*
+SITE/ECCENTRICITY
*CODE PT SOLN T _DATA START_ __DATA_END__ REF __DX_U__ __DX_N__ __DX_E__
ALBH  A ---- C 92:146:00000 94:104:74100 UNE   0.1260   0.0000   0.0000
*/
//=============================================================================
struct Sinex_site_ecc_t
{
	string  sitecode; //4
	string  ptcode;   //2 - physical monument used at the site
	string  solnnum;
	char    typecode;
	int     eccstart[3];        /* ecc start time YY:DOY:SOD */
	int     eccend[3];          /* ecc end time YY:DOY:SOD */
	string  eccrs;              /* 3 - reference system UNE (0) or XYZ (1) */
	double  ecc[3];             /* eccentricity UNE or XYZ (m) d8.4*3 */
	bool	used = false;
} ;

//=============================================================================
/*
+SOLUTION/EPOCHS (mandatory) *OR*
+BIAS/EPOCHS (mandatory when biases are included)
*CODE PT SOLN T _DATA_START_ __DATA_END__ _MEAN_EPOCH_
ALBH  A    1 C 94:002:00000 94:104:00000 94:053:00000
*/
//=============================================================================
struct Sinex_solepoch_t
{
	string  sitecode; //4
	string  ptcode;   //2 - physical monument used at the site
	string  solnnum;
	char    typecode;
	int     start[3]; //yr:doy:sod
	int     end[3];   //yr:doy:sod
	int     mean[3];  //yr:doy:sod
} ;

//=============================================================================
/*
+SOLUTION/STATISTICS
*STAT_NAME (30 chars) value (22char double)
*/
struct Sinex_solstatistic_t
{
	string name;
	short  etype; // 0 = int, 1 = double
	union {
		int	ival;
		double dval;
	} value;
} ;


//=============================================================================
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
//=============================================================================
struct Sinex_solestimate_t
{
	int     index;
	string  type;  //6
	string  sitecode; //4
	string  ptcode;   //2 - physical monument used at the site
	string  solnnum;
	int     refepoch[3]; //yr:doy:sod
	string  unit; //4
	char    constraint;
	double  estimate;
	double  stddev;
	
	bool	primary;	///< First sinex file ever read is considered the primary source of apriori data, to be used for minimum constraints etc.
	bool	used = false;
} ;

//=============================================================================
/*
+SOLUTION/APRIORI
*INDEX PARAMT SITE PT SOLN EPOCH_____ UNIT C PARAM________________ STD_DEV____
12345 AAAAAA XXXX YY NNNN YR:DOY:SOD UUUU A 12345.123456789ABCDEF 1234.123456
*/
//=============================================================================
struct Sinex_solapriori_t
{
	int    idx;
	string param_type; // 6 - select from
	string sitecode;   // 4
	string ptcode;     // 2
	string solnnum;
	int    epoch[3];   // yr:doy:sod
	string unit;       // 4 - select from
	char   constraint; // for inner constraints, choose 1
	double param;      // d21.15 apriori parameter
	double stddev;     // std deviation of parameter
} ;

//=============================================================================
/*
+SOLUTION/NORMAL_EQUATION_VECTOR
*PARAM PTYPE_ SITE PT SOLN EPOCH_____ UNIT C NORMAL______________
12345 AAAAAA XXXX YY NNNN YR:DOY:SOD UUUU A 12345.123456789ABCDEF
*/
//=============================================================================
struct Sinex_solneq_t
{
	int    param; // 5 index of estimated parameters
	string ptype; // 6 - type of parameter
	string site;  // 4 - station
	string pt;    // 2 - point code
	string solnnum;     // 4 solution number
	int epoch[3]; // yr:doy:sod
	string unit;  // 4
	char constraint; //
	double normal; // right hand side of normal equation
} ;

//=============================================================================
/*
+SOLUTION/STATISTICS
*any string (not used currently)
*/
struct Sinex_solstat_t
{
	string statistic;
} ;

//=============================================================================
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
//=============================================================================
struct Sinex_solmatrix_t
{
	int    row;   // 5 - must match the solution/estimate row
	int    col;   // 5 - must match the solution/estimate col
	int	   numvals;
	double value[3]; // each d21.14 cols col, col+1, col+2 of the row
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

//=============================================================================
// TODO: satid and satident/satprn need to be checked for consistency ...
/*
+SATELLITE/ID (recommmended for GNSS)
*SVN_ PR COSPAR_ID O TSL        TUD        ANTENNA_RCV_TYPE_______________
G001 04 1978-020A A yy:doy:sod yy:doy:sod BLAH BLAH BLAH ...
*/
//=============================================================================
struct Sinex_satid_t
{
	string svn;     // 4
	string prn;     // 2
	string cospar;  // 9
	char   obsCode; //
	int    timeSinceLaunch[3]; // yy:doy:sod a value of 0 everywhere means launched before file epoch start
	int    timeUntilDecom[3];  // yy:doy:sod a value of 0 everywhere mean still in commission after file epoch end
	string antRcvType; // 20 - satellite antenna receiver type
} ;

//=============================================================================
/*
+SATELLITE/IDENTIFIER
*SVN_ COSPAR_ID SatCat Block__________ Comment__________________________________
G001 1978-020A  10684 GPS-I           Launched 1978-02-22; NAVSTAR 1
*/
//=============================================================================
struct Sinex_satident_t
{
	string svn; // 4
	string cospar; // 9
	int    category; // 6
	string blocktype; // 15
	string comment; // 42
} ;

//=============================================================================
/*
+SATELLITE/PRN
*SVN_ Valid_From____ Valid_To______ PRN Comment_________________________________
G001 1978:053:00000 1985:199:00000 G04
*/
//=============================================================================
struct Sinex_satprn_t
{
	string svn; // 4
	int start[3]; //yr:doy:sod
	int stop[3]; //yr:doy:sod
	string prn; //3
	string comment; // 40
} ;

//=============================================================================
/* ONLY FOR GLONASS!
+SATELLITE/FREQUENCY_CHANNEL
*SVN_ Valid_From____ Valid_To______ chn Comment________________________________
R701 2003:344:00000 2009:258:86399   1 [FC10]
*/
//=============================================================================
struct Sinex_satfreqchn_t
{
	string svn; // 4
	int start[3]; //yr:doy:sod
	int stop[3]; //yr:doy:sod
	int channel;
	string comment; // 40?
} ;

//=============================================================================
/*
+SATELLITE/MASS
*SVN_ Valid_From____ Valid_To______ Mass_[kg] Comment___________________________
G001 1978:053:00000 0000:000:00000   455.000 GPS-I; [MA01]
*/
//=============================================================================
struct Sinex_satmass_t
{
	string svn; // 4
	int start[3]; //yr:doy:sod
	int stop[3]; //yr/doy:sod
	double mass; // kg
	string comment; // 40
} ;

//=============================================================================
/*
+SATELLITE/COM  (Centre of Mass)
*SVN_ Valid_From____ Valid_To______ ____X_[m] ____Y_[m] ___ Z_[m] Comment___________________________
G001 1978:053:00000 0000:000:00000    0.0000    0.0000    0.0000 GPS-I; [CM02]
*/
//=============================================================================
struct Sinex_satcom_t
{
	string svn; // 4
	int start[3]; //yr:doy:sod
	int stop[3]; //yr:doy:sod
	double com[3]; //x/y/z (metres)
	string comment; // 40
} ;

//=============================================================================
/*
+SATELLITE/ECCENTRICITY
*SVN_ Equipment___________ T ____X_[m] ____Y_[m] ____Z_[m] Comment___________________________
G001 BLOCK I              P    0.0000    0.0000    0.0000 GPS-I; [EG02]
*/
//=============================================================================
struct Sinex_satecc_t
{
	string svn; // 4
	string equip; //  20
	char   type; // L or P - both can exist for the same satellite
	double ecc[3]; //x/y/z
	string comment; // 40
} ;

//=============================================================================
/*
+SATELLITE/TX_POWER
*SVN_ Valid_From____ Valid_To______ P[W] Comment________________________________
G001 1978:053:00000 0000:000:00000   50  GPS-I; same as GPS-II/IIA; [TP04]
*/
//=============================================================================
struct Sinex_satpower_t
{
	string svn; // 4
	int start[3]; //yr:doy:sod
	int stop[3]; //yr:doy:sod
	int power;   // watts
	string comment; // 40
} ;

//=============================================================================
/*
+SATELLITE/PHASE_CENTER
*NB Can have more than one line if satellite transmits on more than 2 frequencies
*SVN_ C ZZZZZZ XXXXXX YYYYYY C ZZZZZZ XXXXXX YYYYYY ANTENNA___ T M
*/
struct Sinex_satpc_t
{
	string svn;     // 4
	char   freq;    // 1/2/5 for GPS & GLONASS, 1/5/6/7/8 for Gallileo
	double zxy[3];  // metres offset from COM in the order given 3* d6.4
	char   freq2;   // as above
	double zxy2[3]; // as above
	string antenna; // 10 - model of antenna
	char   type;    // Phase Center Variation A(bsolute)/R(elative)
	char   model;   // F(ull)/E(levation model only)
} ;

//=============================================================================
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
	bool isEmpty = true;
};

//=============================================================================
/*
+TROP/SOLUTION
*SITE ____EPOCH___ TROTOT STDDEV  TGNTOT STDDEV  TGETOT STDDEV
WIDC 21:014:00000 2270.0    2.8  -0.186  0.376   0.788  0.464
*/
struct SinexTropSol
{
	string	site;
	int		epoch[3]; //yr:doy:sod
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
	/* header block */
	string  snxtype;    /* SINEX file type */
	double  ver;        /* version */
	string  create_agc;  /* file creation agency */
	int     filedate[3]; /* file create date as yr:doy:sod */
	string  data_agc;   /* data source agency */
	int     solution_start_date[3]; // start date of solution as yr:doy:sod
	int     solution_end_date[3];   // as yr:doy:sod
	char    ObsCode;    /* observation code */
	int     numparam;         /* number of estimated parameters */
	char    ConstCode;  /* constraint code */
	string  solcont;    /* solution types S O E T C A */
	string	markerName;

	KFState	kfState;
	KFState	tropKFState;

	list<string>                  		historyComments;
	list<string>                  		filesComments;
	list<string>                  		ackComments;
	list<Sinex_ref_t>                 	refstrings;
	list<Sinex_comment_t>             	commentstrings;
	list<Sinex_input_history_t>       	inputHistory;
	list<Sinex_input_file_t>          	inputFiles;
	list<Sinex_ack_t>                 	acknowledgements;

	/* site stuff */
	list<string>                  		siteIdcomments;
	list<string>                 		siteDatacomments;
	list<string>                  		receivercomments;
	list<string>                  		antennacomments;
	list<string>                  		site_ecc_comments;
	list<string>                  		gps_pc_comments;
	list<string>                  		gal_pc_comments;
	map<string, Sinex_siteid_t>		map_siteids;
	list<Sinex_sitedata_t>            	list_sitedata;
	map<string, map<GTime, Sinex_receiver_t,	std::greater<GTime>>>	map_receivers;
	map<string, map<GTime, Sinex_antenna_t,	std::greater<GTime>>>	map_antennas;
	map<string, map<GTime, Sinex_site_ecc_t,	std::greater<GTime>>>	map_eccentricities;
	list<Sinex_gps_phase_center_t>    	list_gps_pcs;
	list<Sinex_gal_phase_center_t>    	list_gal_pcs;

	/* solution stuff - tied to sites */
	bool                          		epochs_have_bias;
	list<string>                  		epochcomments;
	list<string>                 		statistics_comments;
	list<string>    					estimate_comments;
	list<string>    					apriori_comments;
	list<string>    					normal_eqns_comments;
	list<string>    					matrix_comments;
	list<Sinex_solepoch_t>            	list_solepochs;
	list<Sinex_solstatistic_t>       	list_statistics;
	map<string, map<string, map<GTime, Sinex_solestimate_t, std::greater<GTime>>>>	map_estimates_primary;
	map<string, map<string, map<GTime, Sinex_solestimate_t, std::greater<GTime>>>>	map_estimates;
	map<int, Sinex_solapriori_t> 		apriori_map;
	list<Sinex_solneq_t>   			list_normal_eqns;
	map<matrix_value,list<Sinex_solmatrix_t>>  matrix_map[MAX_MATRIX_TYPE];

	/* satellite stuff */
	list<string>    			satpc_comments;
	list<string>    			satid_comments;
	list<string>    			satident_comments;
	list<string>    			satprn_comments;
	list<string>    			satfreqchn_comments;
	list<string>    			satmass_comments;
	list<string>    			satcom_comments;
	list<string>    			satecc_comments;
	list<string>    			satpower_comments;
	list<Sinex_satpc_t>		list_satpcs;
	list<Sinex_satid_t>		list_satids;
	list<Sinex_satident_t>		list_satidents;
	list<Sinex_satprn_t>		list_satprns;
	list<Sinex_satfreqchn_t>	list_satfreqchns;
	list<Sinex_satmass_t>		list_satmasses;
	list<Sinex_satcom_t>		list_satcoms;
	list<Sinex_satecc_t>		list_sateccs;
	list<Sinex_satpower_t>		list_satpowers;

	/* VLBI - ignored for now */
	list<string>    			sourceid_comments;
	list<string>    			nutation_comments;
	list<string>   				precession_comments;
	list<Sinex_source_id_t> 	list_source_ids;
	list<Sinex_nutcode_t>  	list_nutcodes;
	list<Sinex_precode_t>  	list_precessions;

	// constructor
	Sinex(bool t = false) : epochs_have_bias(t)
	{
	};
	
	bool	primary = false;	///< Set true while a primary sinex file is being read.

	// Troposphere Sinex data
	map<string, int>				tropSiteCoordBodyFPosMap;
	map<string, int>				tropSolFootFPosMap;
	list<string>					tropSiteIdCommentList;
	list<string>					tropSiteRecCommentList;
	list<string>					tropSiteAntCommentList;
	list<string>					tropSiteCoordCommentList;
	list<string>					tropSiteEccCommentList;
	list<string>					tropDescCommentList;
	list<string>					tropSolCommentList;
	SinexTropDesc					tropDesc = {};
	map<string, map<int, double>>	tropSiteCoordMapMap; //indexed by station ID, then axis #
	list<SinexTropSol>				tropSolList;
};

struct Sinex_stn_soln_t
{
	string 	type	= "";		 	/* parameter type */
	string  unit	= "";           /* parameter units */
	double  pos		= 0;            /* real position (ecef) (m)*/
	double  pstd	= 0;           /* position std (m) */
	int		yds[3]	= {};			/* epoch when valid */
} ;

struct  Sinex_stn_snx_t    /* station-wise information */
{
	/* ID block */
	string		sitecode	= "";   /* site code */
	string		ptcode		= "";     /* point code */
	string		monuid		= "";     /* monument identification */
	char		typecode	= 0;
	string		desc		= "";     // site description eg town/city (22)
	int			long_deg	= 0;  // longitude degrees (uint16_t) east is positive
	int			long_min	= 0;   //
	double		long_sec	= 0;  //
	int			lat_deg		= 0;   // latitude degrees north is positive
	int			lat_min		= 0;    // uint8_t
	double		lat_sec		= 0;   // float
	double		height		= 0;   //

	int			start[3]	= {};					/* yr:doy:sod */
	int			stop[3]		= {-1,-1,-1};			/* yr:doy:sod */

	/* receiver block */
	string  rectype;    // receiver type (20)
	string  recsn;      // receiver serial number (5)
	string  recfirm;    // receiver firmware  (11)
	
	/* anntenna block */
	string  anttype	= "";    /* antenna type */
	string  antsn	= "";      /* antenna serial number */

	/* phase_center block (GPS/Galileo only) */
	bool has_gps_pc	= false;
	double  gpsl1[3]	= {};           /* GPS L1 PCO UNE (m) */
	double  gpsl2[3]	= {};           /* GPS L2 PCO UNE (m) */
	bool has_gal_pc	= false;
	double  gall1[3]	= {};           /* GAL L1 PCO UNE (m) */
	double  gall5[3]	= {};           /* GAL L5 PCO UNE (m) */
	double  gall6[3]	= {};           /* GAL L6 PCO UNE (m) */
	double  gall7[3]	= {};           /* GAL L7 PCO UNE (m) */
	double  gall8[3]	= {};           /* GAL L8 PCO UNE (m) */

	/* eccentricity block */
	bool has_ecc	= false;
	string  eccrs	= "";              /* reference system UNE  or XYZ */
	Vector3d	ecc		= Vector3d::Zero();             /* eccentricity UNE or XYZ (m) */


	bool		primary	= false;			///< this position estimate is considered to come from a primary source
	Vector3d	pos		= Vector3d::Zero();
};

struct Sinex_sat_snx_t /* satellite meta data */
{
	string 	svn;
	string	prn;
	string	cospar;
	string	blocktype;
	int		category;
	double 	mass;				/* kg */
	int		channel;			/* GLONASS ONLY */
	double	com[3];				/* centre of mass offsets (m) */
	int		power;				/* Tx Power (watts); */
	int		numeccs;			/* number of eccentricities */
	char	ecctype[2];			/* L or P - seems can have both */
	string	eccequip[2];		/* equipment can be diff for each one */
	double	eccentricity[2][3];	/* x/y/z in m */
	string	antenna;
	int		numfreqs;			/* number of phase center frequencies */
	char	freq[5];			/* 1/2/5/6/7/8 (up to 5 freqs allowed) */
	double	zxy[5][3];			/* phase offsets, order given by var name! */
	char	pctype;
	char	pcmodel;

	int		start[3];			/* yr:doy:sod */
	int		stop[3];			/* yr:doy:sod */
};

typedef map<string, list<Sinex_stn_snx_t>> station_map;
typedef map<string, list<Sinex_sat_snx_t>> satellite_map;

int read_sinex(
	string	filepath,
	bool	primary);

int  write_sinex(
	string filepath,
	map<string, Station>*		stationMap		= nullptr,
	Sinex_sat_snx_t*			psat			= nullptr,
	bool 						comm_override	= true);

E_SnxDataMissing getstnsnx	(string station, int yds[3], Sinex_stn_snx_t& snx);
int getsatsnx	(string prn, int yds[3], Sinex_sat_snx_t&snx);
void sinex_report(Trace& trace);
int sinex_sat_count();
int sinex_site_count();
long int time_compare(int left[3], int right[3]);
void sinex_update_estimate(const Sinex_stn_snx_t& station, E_Estimate e, double val, double std, int yds[3], int newidx, double defstd);
void sinex_update_matrix	(matrix_type mt, matrix_value mv, int row, int col, int nvals, double val[3]);
void sinex_add_statistic(const string& what, const int		value);
void sinex_add_statistic(const string& what, const double	value);
int sinex_check_add_ga_reference(string solType, string peaVer, bool isTrop);
void sinex_add_acknowledgement(const string& who, const string& description);
void sinex_add_comment(const string& what);
void sinex_add_file(const string& who, const GTime& when, const string& filename, const string& description);

void sinex_update_header(
	string& 	create_agc,
	int			create_date[3],
	string&		data_agc,
	int			soln_start[3],
	int			soln_end[3],
	const char	obsCode,
	const char	constCode,
	string&		contents,
	double		sinexVer);

void sinexPostProcessing(
	GTime&					tsync,
	map<string, Station>&	stationMap,
	KFState&				netKFState);

void sinexPerEpochPerStation(
	GTime&		tsync,
	Station&	rec);


// Trop sinex
void outputTropSinex(
	string					filename,
	GTime&					tsync,
	map<string, Station>&	stationMap,
	KFState&				netKFState,
	string					markerName = "MIX",
	bool					isSmoothed = false);

// snx.cpp fns used in tropSinex.cpp
void write_as_comments(
	std::ofstream&	out,
	list<string>&	comments);

void write_pretty_line(
	std::ofstream&	out);

int write_snx_reference(
	std::ofstream&	out);

extern Sinex theSinex; // the one and only sinex object.

#endif
