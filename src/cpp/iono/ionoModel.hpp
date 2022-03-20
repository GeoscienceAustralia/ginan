#ifndef ___IONOMODEL_HPP__
#define ___IONOMODEL_HPP__

#include <math.h>
#include <stdio.h>
#include "observations.hpp"
#include "navigation.hpp"
#include "biasSINEX.hpp"
#include "station.hpp"
#include "gTime.hpp"

#define MAX_LAYER_NUM 4
#define MAX_DCB_COD 1
#define STEC2DELAY  4.48397972589608
#define MAXBUFSIZ_IONO 4096


#define IONOMODEL_NAVSYS (SYS_GPS) /*(SYS_GPS + SYS_GLO + SYS_GAL + SYS_CMP)*/
#define MIN_NSAT_STA 		3
#define MAXSTANUM			512
#define MAXOUTCNT			10
#define MAXIONOUTS			120.0
#define PHNS_TO_PRNS		100
#define PRNG_NOISE_A		0.15
#define PRNG_NOISE_B		0.15
#define PDE_FAIL_RATE		0.001
#define DEF_LAYER_VAR		0.1

struct sinexbias_t
{
	SatSys Sat;
	char station[9];
	char cod1[4];
	char cod2[4];
	double bias;
	double biasstd;
	double slope;
	double slopestd;
	int tini[3];
	int tend[3];
};

typedef struct
{
	double rotmtx[9];				/* Rotation matrix (to centre of map) */
	GTime ion_time;
	double valid;

	int *m;
	double *n;

	map<E_Sys, Station> refStations;
} ionomodel_t;

extern FILE* fp_iondebug;

/* Global variables */
extern ionomodel_t  iono_modl;
extern KFState		iono_KFState;

/* General functions */
int  config_ionosph_model ();
int  update_receivr_measr (Trace& trace, Station& rec);
void update_ionosph_model (Trace& trace, StationMap& stationMap, GTime iontime);
int  ionex_file_write(Trace& trace, GTime time, bool end = false);
void write_receivr_measr(Trace& trace, std::map<string, Station> stations, GTime time);

/* Spherical Harmonics Model */
int configure_iono_model_sphhar (void);
int Ipp_check_sphhar(GTime time, double *Ion_pp);
double ion_coef_sphhar(int ind, Obs& obs, bool slant = true);
double ion_vtec_sphhar(GTime time, double *Ion_pp, int layer, double& vari, KFState& kfState);


/* Spherical Cap Model */
int configure_iono_model_sphcap (void);
int Ipp_check_sphcap(GTime time, double *Ion_pp);
double ion_coef_sphcap(int ind, Obs& obs, bool slant = true);
double ion_vtec_sphcap(GTime time, double *Ion_pp, int layer, double& vari, KFState& kfState);

/* Bspline Model */
int configure_iono_model_bsplin (void);
int Ipp_check_bsplin(GTime time, double *Ion_pp);
double ion_coef_bsplin(int ind, Obs& obs, bool slant = true);
double ion_vtec_bsplin(GTime time, double *Ion_pp, int layer, double& vari, KFState& kfState);

#endif



