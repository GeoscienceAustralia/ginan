#ifndef GNSS_AMB_RES_HPP
#define GNSS_AMB_RES_HPP

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "streamTrace.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "satSys.hpp"
#include "common.hpp"

#define POSTAR_VAR		1e-12
#define MEAS_OUTAG		120.0
#define FIXED_AMB_VAR   1e-16

struct ARState
{
	bool   endu=false;	/* end user mode */
	string recv;
	int    mode = E_ARmode::OFF;	/* AR mode */
	int    nset = 0;	/* candidate set size for lambda */
	int    nitr = 3;	/* number of iterations for iter_rnd */
	double prcele = D2R * 10.0;	/* min elevation for processing */
	double arelev = D2R * 15.0;  /* min elevation for AR */
	double sucthr = 0.9999;  /* success rate threshold */
	double ratthr = 3;	/* ratio test threshold */

	double satprn = 0.00001;  /* process noise for satellite bias */
	double staprn =	0.0001;   /* process noise for station bias */

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

struct ambMeas
{
	double NL12 = 0;
	double WL12 = 0;
	double WL23 = 0;
	double NL12var = -1; 				/* -1.0: NL12 data invalid */
	double WL12var = -1;				/* -1.0: WL12 data invalid */
	double WL23var = -1;				/* -1.0: WL23 data invalid */
};

struct Amb_Sec
{
	short	sec_num = 0;
	GTime	sec_ini = {};			/* End of section */
	GTime	sec_fin = {};			/* Start of section */
	int		sec_am1 = 0;			/* NL or  L1 */
	int		sec_am2 = 0;			/* WL or  L2 or L1-L2 */
	int		sec_am3 = 0;			/* eWL or L3 or L2-L3 */
};

/* 0: all float, +1: eWL fixed, +2: WL fixed, +4: NL fixed, to be replaced by separate bool indicators */
struct SignAmbg
{
	bool pivot = false;		
	int state = 0;				/*0: all float, +1: eWL fixed, +2: WL fixed, +4: NL fixed */
	int nepc  = 0;
	int nfreq = 0;
	double elev=0;
	ambMeas raw;
	ambMeas flt;
	ambMeas fix;
	int outage = 0;
	double ref[3];

	Amb_Sec curr_sect;
};

struct StatAmbg
{
	GTime update;
	bool anchor;
	int npivot = 0;
	int dept = 5;
	map<SatSys, SignAmbg> SignList;
	ambMeas stabias;
	ambMeas stabias_fix;
	bool reset=false;
};

struct bias_out_state
{
	double rawbias = 0;
	int    intlevl = 0;
	int    numsamp = 0;
	double outbias = 0;
	double outvari = 0;
};

struct Satlpivt
{
	int npivot = 0;
	int dept = 5;
	string pivot_in;
	map<string, double> elev;
	ambMeas satbias;
	ambMeas satbias_fix;
	map<E_AmbTyp, bias_out_state> bias_out;
	bool reset;

	double rSat[3];
	double NLwav = 0;
	double WLinIF = 0;
};

/* global variables */
extern map<E_Sys, bool>						sys_solve;
extern string								ARrefsta;
extern map<SatSys, Satlpivt>				satpiv;
extern map<E_Sys, map<string, StatAmbg>>	StatAmbMap_list;
extern KFState								KF_ARcopy;

int networkAmbigResl( Trace& trace, StationList& stations, KFState& kfState, double tgap);
void Netwrk_ARoutput ( Trace& trace, StationList& stations, GTime time, bool ionout, double biaupdt, double arelev);
int enduserAmbigResl( Trace& trace, ObsList& obsList, KFState& kfState, double tgap);
int net_sect_out ( Trace& trace );
void Netwrk_trace_out(Trace& trace, double arelev,string recv);

void updt_usr_pivot ( Trace& trace, double arelev, string receiver );
void init_net_pivot ( Trace& trace, double arelev, string defref );
void updt_net_pivot ( Trace& trace, bool wlonly );
int  rese_net_NL ( Trace& trace, string sta, SatSys insat);

int WLambEstm(Trace& trace, GTime time, double tgap, ARState& ambState, bool wlonly);
int NLambEstm(Trace& trace, KFState& kfState, ARState& ambState);

int GNSS_AR(Trace& trace, ARState* ambc);
bool ARsol_ready (void);
void start_new_sect(Trace& trace, SatSys sat, string sta, GTime time);
#endif
