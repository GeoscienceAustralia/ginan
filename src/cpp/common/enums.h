

#ifndef __ENUMS_H__
#define __ENUMS_H__

#define BETTER_ENUMS_DEFAULT_CONSTRUCTOR(Enum) \
public:                                      \
	Enum() = default;

#include "enum.h"	//BETTER_ENUM

typedef enum
{
	FTYPE_NONE,

	F1 = 1,	L1 = F1,
	F2 = 2,	L2 = F2,
	F5 = 5,	L5 = F5,

	F6 = 6,
	F8 = 8,
	F3 = 3,
	F4 = 4,
	F7 = 7,

	FTYPE_IF12 = 12,
	FTYPE_IF15 = 15,
	FTYPE_IF25 = 25,

	G1 = 101,
	G2,
	E1   = F1,
	E2,
	E5A  = F5,
	E5B  = F7,
	E5AB = F8,
	E6   = F6,
	LEX  = F6,
	B1,
	B2,
	B3,
	NUM_FTYPES
} E_FType;

typedef enum
{
	CODE,
	PHAS,
	NUM_MEAS
} E_MeasType;

typedef enum
{
	SVH_OK,
	SVH_UNHEALTHY = -1
} E_Svh;


BETTER_ENUM(E_Sys,			short int,
			NONE,
			GPS,
			SBS,
			GLO,
			GAL,
			QZS,
			BDS,
			LEO,
			IRN,
			COMB)


BETTER_ENUM(E_DCBPair,		short int,
			NONE,
			P1_P2,
			P1_C1,
			P2_C2)

BETTER_ENUM(E_BiasGroup,	short int,
			GPS,
			GLO,
			GAL,
			BDS,
			NUM)


BETTER_ENUM(E_OffsetType,	short int,
			UNSPECIFIED,
			APC,
			COM)

BETTER_ENUM(KF,				short int,
	NONE,
	ONE,
	
	REC_POS,
	REC_POS_RATE,
	
	SAT_POS,
	SAT_POS_RATE,
	SAT_MOMENTUM,
	
	REF_SYS_BIAS,
	
	REC_CLOCK,			REC_SYS_BIAS			= REC_CLOCK,
	REC_CLOCK_RATE,		REC_SYS_BIAS_RATE		= REC_CLOCK_RATE,
	REC_CLOCK_RATE_GM,	REC_SYS_BIAS_RATE_GM	= REC_CLOCK_RATE_GM,
	
	SAT_CLOCK,
	SAT_CLOCK_RATE,
	SAT_CLOCK_RATE_GM,
	
	TROP,
	TROP_GM,
	
	ORBIT_PTS,
	SRP,
	
	KEPLERS,
	
	
	IONOSPHERIC,
	IONO_STEC,
	
	DCB,
	
	EOP,
	EOP_RATE,
	
	CALC,

	XFORM_XLATE,
	XFORM_RTATE,
	XFORM_SCALE,


	AMBIGUITY,
	PHASE_BIAS,
	CODE_BIAS
)


BETTER_ENUM(KEPLER,				short int,
			LX,
			LY,
			LZ,
			EU,
			EV,
			M,
			NUM
)

BETTER_ENUM(E_BiasType,				short int,
	NONE,
	OSB,
	DSB
)

//config file enums

BETTER_ENUM(E_ChiSqMode,	int,
			NONE,
			INNOVATION,
			MEASUREMENT,
			STATE)

BETTER_ENUM(E_TropModel,		int,
			VMF3,
			GPT2)

BETTER_ENUM(E_NoiseModel,		int,
			UNIFORM,
			ELEVATION_DEPENDENT)

BETTER_ENUM(E_LogLevel,			int,
			DEBUG,
			WARN,
			ERROR)

BETTER_ENUM(E_IonoModel,		int,
			NONE,
			MEAS_OUT,
			BSPLINE,
			SPHERICAL_CAPS,
			SPHERICAL_HARMONICS)

BETTER_ENUM(E_IonoMode,			int,
			OFF,                        /* ionosphere option: correction off */
			BROADCAST,                  /* ionosphere option: broadcast model */
			SBAS,                       /* ionosphere option: SBAS model */
			IONO_FREE_LINEAR_COMBO,		/* ionosphere option: L1/L2 or L1/L5 iono-free LC */
			ESTIMATE,                   /* ionosphere option: estimation */
			TOTAL_ELECTRON_CONTENT,     /* ionosphere option: IONEX TEC model */
			QZS,                        /* ionosphere option: QZSS broadcast model */
			LEX,                        /* ionosphere option: QZSS LEX ionospehre */
			STEC)                       /* ionosphere option: SLANT TEC model */

BETTER_ENUM(E_LinearCombo,		int,
			ANY,
			L1L2_ONLY,
			L1L5_ONLY)

BETTER_ENUM(E_Period,			int,
			SECOND	= 1, 				
			MINUTE	= 60, 		
			HOUR	= 60 * 60,		
			DAY		= 60 * 60 * 24, 	
			WEEK	= 60 * 60 * 24 * 7,	
			YEAR	= 60 * 60 * 24 * 365,
			
			SECONDS			= SECOND,	MINUTES			= MINUTE,	HOURS		= HOUR,	DAYS		= DAY,	WEEKS		= WEEK,	YEARS		= YEAR,
			SEC				= SECOND,	MIN				= MINUTE,	HR			= HOUR,	DY			= DAY,	WK			= WEEK,	YR			= YEAR,
			SECS			= SECOND,	MINS			= MINUTE,	HRS			= HOUR,	DYS			= DAY,	WKS			= WEEK,	YRS			= YEAR,
			SQRT_SEC		= SECOND,	SQRT_MIN		= MINUTE,	SQRT_HR		= HOUR,	SQRT_DY		= DAY,	SQRT_WK		= WEEK,	SQRT_YR		= YEAR,
			SQRT_SECS		= SECOND,	SQRT_MINS		= MINUTE,	SQRT_HRS	= HOUR,	SQRT_DYS	= DAY,	SQRT_WKS	= WEEK,	SQRT_YRS	= YEAR,
			SQRT_SECOND		= SECOND,	SQRT_MINUTE		= MINUTE,	SQRT_HOUR	= HOUR,	SQRT_DAY	= DAY,	SQRT_WEEK	= WEEK,	SQRT_YEAR	= YEAR,
			SQRT_SECONDS	= SECOND,	SQRT_MINUTES	= MINUTE,	SQRT_HOURS	= HOUR,	SQRT_DAYS	= DAY,	SQRT_WEEKS	= WEEK,	SQRT_YEARS	= YEAR)

BETTER_ENUM(E_PosFrame, int,
			NONE,
			XYZ,
			NED,
			RTN)

BETTER_ENUM(E_FilterMode, int,
			LSQ,
			KALMAN)

BETTER_ENUM(E_Inverter, int,
			LLT,
			LDLT,
			INV)

BETTER_ENUM(E_ObsDesc, int,
	C, // Code / Pseudorange
	L, // Phase
	D, // Doppler
	S, // Raw signal strength (carrier to noise ratio)
	X  // Receiver channel numbers
)

BETTER_ENUM(E_ObsCode, int,
	NONE  = 0 ,     		          /* none or unknown */
	L1C   = 1 ,						  /* L1C/A,G1C/A,E1C (GPS,GLO,GAL,QZS,SBS) */
	L1P   = 2 ,        		          /* L1P,G1P    (GPS,GLO) */
	L1W   = 3 ,        		          /* L1 Z-track (GPS) */
	L1Y   = 4 ,        		          /* L1Y        (GPS) */
	L1M   = 5 ,        		          /* L1M        (GPS) */
	L1N   = 6 ,        		          /* L1codeless (GPS) */
	L1S   = 7 ,        		          /* L1C(D)     (GPS,QZS) */
	L1L   = 8 ,        		          /* L1C(P)     (GPS,QZS) */
	L1E   = 9 ,        		          /* L1-SAIF    (QZS) */
	L1A   = 10,        		          /* E1A        (GAL) */
	L1B   = 11,        		          /* E1B        (GAL) */
	L1X   = 12,        		          /* E1B+C,L1C(D+P) (GAL,QZS) */
	L1Z   = 13,        		          /* E1A+B+C,L1SAIF (GAL,QZS) */
	L2C   = 14,        		          /* L2C/A,G1C/A (GPS,GLO) */
	L2D   = 15,        		          /* L2 L1C/A-(P2-P1) (GPS) */
	L2S   = 16,        		          /* L2C(M)     (GPS,QZS) */
	L2L   = 17,        		          /* L2C(L)     (GPS,QZS) */
	L2X   = 18,        		          /* L2C(M+L),B1I+Q (GPS,QZS,CMP) */
	L2P   = 19,        		          /* L2P,G2P    (GPS,GLO) */
	L2W   = 20,        		          /* L2 Z-track (GPS) */
	L2Y   = 21,        		          /* L2Y        (GPS) */
	L2M   = 22,        		          /* L2M        (GPS) */
	L2N   = 23,        		          /* L2codeless (GPS) */
	L5I   = 24,        		          /* L5/E5aI    (GPS,GAL,QZS,SBS) */
	L5Q   = 25,        		          /* L5/E5aQ    (GPS,GAL,QZS,SBS) */
	L5X   = 26,        		          /* L5/E5aI+Q  (GPS,GAL,QZS,SBS) */
	L7I   = 27,        		          /* E5bI,B2I   (GAL,CMP) */
	L7Q   = 28,        		          /* E5bQ,B2Q   (GAL,CMP) */
	L7X   = 29,        		          /* E5bI+Q,B2I+Q (GAL,CMP) */
	L6A   = 30,        		          /* E6A        (GAL) */
	L6B   = 31,        		          /* E6B        (GAL) */
	L6C   = 32,        		          /* E6C        (GAL) */
	L6X   = 33,        		          /* E6B+C,LEXS+L,B3I+Q (GAL,QZS,CMP) */
	L6Z   = 34,        		          /* E6A+B+C    (GAL) */
	L6S   = 35,        		          /* LEXS       (QZS) */
	L6L   = 36,        		          /* LEXL       (QZS) */
	L8I   = 37,        		          /* E5(a+b)I   (GAL) */
	L8Q   = 38,        		          /* E5(a+b)Q   (GAL) */
	L8X   = 39,        		          /* E5(a+b)I+Q (GAL) */
	L2I   = 40,        		          /* B1I        (CMP) */
	L2Q   = 41,        		          /* B1Q        (CMP) */
	L6I   = 42,        		          /* B3I        (CMP) */
	L6Q   = 43,        		          /* B3Q        (CMP) */
	L3I   = 44,        		          /* G3I        (GLO) */
	L3Q   = 45,        		          /* G3Q        (GLO) */
	L3X   = 46,        		          /* G3I+Q      (GLO) */
	L1I   = 47,        		          /* B1I        (BDS) */
	L1Q   = 48,         		      /* B1Q        (BDS) */
	MAXCODE = 48,
	NUM_CODES
)


BETTER_ENUM(E_Estimate, int,
	STAX,
	STAY,
	STAZ,
	VELX,
	VELY,
	VELZ,
	XGC,
	YGC,
	ZGC,
	RS_RAR,
	RS_DER,
	RS_RA,
	RS_DE,
	RS_PL,
	LOD,
	UT,
	XPOR,
	YPOR,
	XPO,
	YPO,
	NUT_LN,
	NUT_OB,
	NUTRLN,
	NUTROB,
	NUT_X,
	NUT_Y,
	NUTR_X,
	NUTR_Y,
	SAT__X,
	SAT__Y,
	SAT__Z,
	SAT_VX,
	SAT_VY,
	SAT_VZ,
	SAT_RP,
	SAT_GX,
	SAT_GZ,
	SATYBI,
	TROTOT,
	TRODRY,
	TROWET,
	TGNTOT,
	TGNDRY,
	TGNWET,
	TGETOT,
	TGEDRY,
	TGEWET,
	RBIAS,
	TBIAS,
	SBIAS,
	ZBIAS,
	AXI_OF,
	SATA_X,
	SATA_Y,
	SATA_Z,
	CN,
	SN)

BETTER_ENUM(E_ARmode,	short int,
			OFF,
			ROUND,
			ITER_RND,
			BOOTST,
			LAMBDA,
			LAMBDA_ALT,
			LAMBDA_AL2,
			LAMBDA_BIE) 

BETTER_ENUM(E_AmbTyp,	short int,
			NONE,
			NL12,
			WL12,
			WL23,
			UCL1,
			UCL2,
			UCL3)

BETTER_ENUM(RtcmMessageType, uint16_t,
		NONE 				= 0,
		
		GPS_EPHEMERIS		= 1019,

		//GPS_NETWORK_RTK_RESIDUAL = 1030,
		//RECEIVER_AND_ANTENNA_DESC = 1033,

		//BDS_EPHEMERIS = 1042,
		
		GAL_FNAV_EPHEMERIS	= 1045,
		GAL_INAV_EPHEMERIS	= 1046,
		
		GPS_SSR_ORB_CORR	= 1057,
		GPS_SSR_CLK_CORR	= 1058,
		GPS_SSR_CODE_BIAS	= 1059,
		GPS_SSR_COMB_CORR	= 1060,
		GPS_SSR_URA			= 1061,
		
		MSM4_GPS 			= 1074,
		MSM5_GPS 			= 1075,
		MSM6_GPS 			= 1076,
		MSM7_GPS 			= 1077,
		
		MSM4_GLONASS 		= 1084,
		MSM5_GLONASS 		= 1085,
		MSM6_GLONASS 		= 1086,
		MSM7_GLONASS 		= 1087,
		
		MSM4_GALILEO 		= 1094,
		MSM5_GALILEO 		= 1095,
		MSM6_GALILEO 		= 1096,
		MSM7_GALILEO 		= 1097,
		
		MSM4_QZSS 			= 1114,
		MSM5_QZSS 			= 1115,
		MSM6_QZSS 			= 1116,
		MSM7_QZSS 			= 1117,
		
		MSM4_BEIDOU 		= 1124,
		MSM5_BEIDOU 		= 1125,
		MSM6_BEIDOU 		= 1126,
		MSM7_BEIDOU 		= 1127,
		
		//GLONASS_AUX_OPERATION_INFO = 1230
		
		GAL_SSR_ORB_CORR	= 1240,
		GAL_SSR_CLK_CORR	= 1241,
		GAL_SSR_COMB_CORR	= 1243,
		GAL_SSR_CODE_BIAS	= 1242,
		
		GPS_SSR_PHASE_BIAS	= 1265,
		
		GAL_SSR_PHASE_BIAS	= 1267,
		
		CUSTOM				= 4082
)

BETTER_ENUM(E_Ephemeris, int,
		NONE,
		PRECISE,
		SSR,
		KALMAN,
		BROADCAST)


BETTER_ENUM(E_RTCMSubmessage,	short int,
		TIMESTAMP = 1
)

BETTER_ENUM(E_ObsWaitCode,	short int,
		OK,
		NO_DATA_WAIT,
		NO_DATA_EVER)

/* Options associated with solar radiation pressure models */
BETTER_ENUM(E_SRPModels,	int,
			CANNONBALL,
			BOXWING,
			ECOM,
			ECOM2)

BETTER_ENUM(E_GravMdl, 		short int,
			EGM08,
			GGM03S,
			GGM05S)


BETTER_ENUM(E_TidesMdl, 	short int,
			ELASTIC,
			ANELASTIC)			

BETTER_ENUM(E_SnxDataMissing,	short int,
	NONE_MISSING,
	SITE_ID,
	RECEIVER,
	ANTENNA,
	ECCENTRICITY,
	GPS_PHASE_CENTRE,
	ESTIMATE)

BETTER_ENUM(E_Integrator,		short int,
			RKF78)

BETTER_ENUM(E_ThirdBody,		short int,
			MERCURY		= 1,
			VENUS		= 2,
			EARTH		= 3,
			MARS		= 4,
			JUPITER		= 5,
			SATURN		= 6,
			URANUS		= 7,
			NEPTUNE		= 8,
			PLUTO		= 9,
			MOON		= 10,
			SUN			= 11)	//from jpl, do not modify


BETTER_ENUM(E_SigWarning, short int,
	SIG_OUTG	= 1,		// Minor (one signal) outage
	LOW_ELEV	= 2,		// Low elevation
	CYC_SLIP	= 3,		// Cycle slip
	MAJ_OUTG    = 4,		// Major (whole satellite/receiver) outage
	USR_DISC	= 5)		// User defined



#endif
