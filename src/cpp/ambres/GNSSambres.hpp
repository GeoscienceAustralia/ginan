#ifndef GNSS_AMB_RES_HPP
#define GNSS_AMB_RES_HPP

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "streamTrace.hpp"
#include "constants.hpp"
#include "algebra.hpp"
#include "station.hpp"
#include "satSys.hpp"
#include "common.hpp"

#define POSTAR_VAR			1e-6
#define FIXED_AMB_VAR		1e-8
#define INVALID_WLVAL		-999999
#define MAX_ARCH    604800.0
#define ARTRCLVL	2


struct GinAR_mtx
{
	map<int, KFKey> ambmap;
	VectorXd aflt;
	MatrixXd Paflt;

	MatrixXd Ztrs;
	MatrixXd Ltrs;
	VectorXd Dtrs;

	VectorXd zflt;
	VectorXd zfix;

	VectorXd afix;
	MatrixXd Pafix;
};

struct GinAR_opt
{
	string recv;
	bool   endu = false;
	int    mode = E_ARmode::OFF;	/* AR mode */
	int    nset = 0;	/* candidate set size for lambda */
	int    nitr = 3;	/* number of iterations for iter_rnd */
	
	double MIN_Elev_prc = D2R * 10;	/* min elevation for processing */
	double MIN_Elev_AR = D2R * 15;  /* min elevation for AR */
	double MIN_Elev_piv = D2R * 20;  /* min elevation for pivot */
	
	double wlsatp = 0.00001;
	double wlrecp = 0.0001;
	int    wlmxit = 2;
	int    wlmxrm = 0;
	
	double sucthr = 0.9999; 	/* success rate threshold */
	double ratthr = 3;			/* ratio test threshold */
	int    Max_Hold_epc = 0;	/* max hold (epoch) */
	double Max_Hold_tim = 600;		/* max hold (seconds) */
	
	map<E_Sys,double> wavlen;
	map<E_Sys,double> wlfact;
	
	E_IonoMode ionmod = E_IonoMode::IONO_FREE_LINEAR_COMBO;
	
};

struct GinAR_amb
{
	GTime	sec_ini = {};			/* Start of section */
	GTime	mea_fin = {};			/* Last sampled */
	GTime	fix_fin = {};			/* Last fixed */
	int 	int_amb =  0;
	bool 	cyl_slp = false;
	int 	hld_epc = -1;			/* -1:unfixed, 0:fixed this epoc, >0:held */
	int 	out_epc = 9999;			/* outage epocs */
	double 	sat_ele =  0;
	double	raw_amb =  0;
	double	raw_var = -1;			/* <0: float data invalid */
	double	flt_amb =  0;
	double	flt_var = -1;			/* <0: float data invalid */
};

struct GinAR_piv
{
	SatSys			pre_sat;
	string			pre_rec;
	map<string,int> rec_amb;
	map<SatSys,int> sat_amb;
};

struct GinAR_bia
{
	double rawbias = 0;
	int    intlevl = 0;
	int    numsamp = 0;
	double outbias = 0;
	double outvari = 0;
};

typedef map<KFKey,double> Z_Amb; 
typedef map<string,GinAR_piv> ARrecpivts;
typedef map<SatSys,GinAR_piv> ARsatpivts;
extern map<E_Sys, E_ObsCode> defCodesL1;
extern map<E_Sys, E_ObsCode> defCodesL2;
extern map<E_Sys, E_ObsCode> defCodesL3;

struct Station_AR_control
{
	string ID;
	
	map<E_AmbTyp,int>		AmbTypMap;
	map<KFKey,GinAR_amb>	AR_meaMap;
	
	map<Z_Amb,GinAR_amb>	ZAmb_archive;
	
	KFState kfState_fixed;
	
	Vector3d snxPos_;
	Vector3d fltPos_;
	Vector3d fixPos_;
	
	string  solutFilename;
};

/* global variables */
extern map<E_Sys, bool>									sys_solve;
extern map<E_Sys, string>								AR_reflist;
extern map<E_AmbTyp,map<E_Sys,ARsatpivts>>				SATpivlist;
extern map<E_AmbTyp,map<E_Sys,ARrecpivts>>				RECpivlist;
extern map<E_AmbTyp,map<E_Sys,map<string,GinAR_bia>>>	RECbialist;
extern map<E_AmbTyp,map<SatSys,GinAR_bia>>				SATbialist;
extern bool AR_VERBO;
extern map<string,Station_AR_control> ARstations;

/* main functions */
void config_AmbigResl( void );																				/* Configures the ambiguity resolution algorithms */
int  networkAmbigResl( Trace& trace, StationMap& stations, KFState& kfState);								/* Ambiguity resolution for network solutions */
int  enduserAmbigResl( Trace& trace, ObsList& obsList, KFState& kfState, Vector3d snxPos, double dop);		/* Ambiguity resolution for end user solutions */
int  smoothdAmbigResl( KFState& kfState );																	/* Ambiguity resolution on smoothed KF*/
bool sys_frq(short int sys, E_FType& frq1, E_FType& frq2, E_FType& frq3);
bool ARsol_ready(void);
KFState retrieve_last_ARcopy (void);
void init_station_AR ( string stationID );

/* Output fuctions */
void gpggaout( string outfile, KFState& KfState, string recId, int solStat, int numSat, double hdop, bool lng, bool print_header); /* Alternative end user aoutput for ambiguity resolved solutions */
void artrcout( Trace& trace, GTime time, string rec, GinAR_opt opt );
void arbiaout( Trace& trace, GTime time, double tupdt );
int  arionout( Trace& trace, KFState& KfState, ObsList& obsList, GinAR_opt opt );

/* WL ambiguity functions */
void reset_WLfilt( Trace& trace, E_AmbTyp typ, GTime time, string rec, E_Sys  sys);
int  retrv_WLambg( Trace& trace, E_AmbTyp typ, GTime time, string rec, SatSys sat);
int  updat_WLambg( Trace& trace, E_AmbTyp typ, GTime time,             E_Sys  sys, GinAR_opt opt);
void remov_WLrecv( Trace& trace, E_AmbTyp typ,             string rec, E_Sys  sys);
void remov_WLsate( Trace& trace, E_AmbTyp typ,                         SatSys sat);
void dump__WLambg( Trace& trace );

/* NL ambiguity functions */
int  updat_ambigt( Trace& trace, KFState& kfState, GinAR_opt opt );
int  apply_ambigt( Trace& trace, KFState& kfState, GinAR_opt opt );

/* Pivot functions */
void updt_usr_pivot ( Trace& trace, GTime time, GinAR_opt& opt, E_AmbTyp typ );
void updt_net_pivot ( Trace& trace, GTime time, GinAR_opt& opt, E_AmbTyp typ );

/* Core ambiguity resolution function */
int  GNSS_AR(Trace& trace, GinAR_mtx& mtrx, GinAR_opt opt);

/* KF function to be deprecated */
void removeUnmeasuredAmbiguities( Trace& trace,  KFState& kfState, map<KFKey, bool>	measuredStates);

#endif
