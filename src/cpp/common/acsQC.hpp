
#ifndef ACSQC_HPP
#define ACSQC_HPP

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "eigenIncluder.hpp"

#include "enums.h"
#include "linearCombo.hpp"
#include "observations.hpp"
#include "navigation.hpp"

#define     MAXIGSSTA   1
#define     NTROP       2880        /* max daily trop solution number */
#define     MAXSTR      32          /* max string length */

/** Clock jump repair object
*/
struct ClockJump
{
	int msJump;
	int type;
};

/** Cycle slip repair filter
*/
struct flt_t
{
	double  a[3];       ///< cycle slip state vector
	double  Qa[3][3];   ///< cycle slip state variance-covariance matrix
	int     slip;       ///< cycle slip indicator for multi-epoch
	int     amb[3];     ///< repaired cycle slip
	int     ne;         ///< number of epochs involved
	lc_t    lc_pre;     ///< lc information used for cycle slip repair
};

/* trop sinex file */
struct mgex_tropcoord                 /* trop station coordinates block */
{
	char    sitecode[4];        /* site code */
	char    ptcode[2];          /* point code */
	char    solid[4];           /* solution ID */
	char    obscode;            /* observation code */
	double  x[3];               /* coordinates */
	char    sys[6];             /* system */
	char    remark[5];
	double  std[3];             /* (mm) */
	int     counter;
};

struct mgex_tropsol                 /* trop description block */
{
	char    marker[4];          /* marker name */
	double  ts[3];              /* solution time YDS */
	double  x[14];
	int     actak;
	int     acdel;
};

struct mgex_trop                 /* trop information */
{
	char    id[5];
	double  ver;
	char    agency[3];
	char    agencycode[3];
	int     tbc[3];             /* time created */
	int     tbs[3];             /* time started */
	int     tbe[3];             /* time end */
	char    obscode;            /* observation code */
	char    solcon[4];
	/* description block */
	char    solfield[14][6];    /* solution */
	int     inttrop;            /* trop solution sample rate */
	int     intdata;            /* data sampling interval */
	char    tropmap[22];        /* mapping function */
	double  el;                 /* elevation cut off */
	int     bstart;             /* bias start */
	int     bend;               /* bias end */
	double  factor;             /* delete factor */
	char    cfactor[22];        /* conversion factor */
	mgex_tropcoord  tcoord[MAXIGSSTA];
	mgex_tropsol    *tsol;
};

int lsqqc(
		FILE *fp,
		const double *H,
		const double *P,
		const double *Z,
		double *v,
		double *xo,
		double *Po,
		int m,
		int n,
		int ind,
		int norb);

int chiqc(
	Trace& trace,
	const double *H,
	const double *P,
	const double *Z,
	const double *xp,
	double *v,
	int m,
	int n,
	int ind);

void detectslips(
	Trace&		trace,
	ObsList&	obsList);

void detslp_gf(
	ObsList&	obsList);

void detslp_mw(
	ObsList&	obsList);

void detslp_ll(
	ObsList& obsList);

void detectjump(
	Trace&		trace,
	ObsList&	obsList,
	double		elmin,
	ClockJump&	cj);


#endif
