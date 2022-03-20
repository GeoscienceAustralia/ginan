
#ifndef ACSQC_HPP
#define ACSQC_HPP

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "linearCombo.hpp"
#include "navigation.hpp"
#include "enums.h"

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


/* trop sinex file */
struct mgex_tropcoord                 /* trop station coordinates block */
{
	char    sitecode[5] = {};        /* site code */
	char    ptcode[3] = {};          /* point code */
	char    solid[5] = {};           /* solution ID */
	char    obscode;            /* observation code */
	double  x[3];               /* coordinates */
	char    sys[7] = {};             /* system */
	char    remark[6] = {};
	double  std[3];             /* (mm) */
	int     counter;
};

struct mgex_tropsol                 /* trop description block */
{
	char    marker[5] = {};          /* marker name */
	double  ts[3];              /* solution time YDS */
	double  x[14];
	int     actak;
	int     acdel;
};

struct mgex_trop                 /* trop information */
{
	char    id[6] = {};
	double  ver;
	char    agency[4] = {};
	char    agencycode[4] = {};
	int     tbc[3];             /* time created */
	int     tbs[3];             /* time started */
	int     tbe[3];             /* time end */
	char    obscode;            /* observation code */
	char    solcon[5] = {};
	/* description block */
	char    solfield[14][6] = {};    /* solution */
	int     inttrop;            /* trop solution sample rate */
	int     intdata;            /* data sampling interval */
	char    tropmap[23] = {};        /* mapping function */
	double  el;                 /* elevation cut off */
	int     bstart;             /* bias start */
	int     bend;               /* bias end */
	double  factor;             /* delete factor */
	char    cfactor[23] = {};        /* conversion factor */
	mgex_tropcoord  tcoord[MAXIGSSTA];
	mgex_tropsol*	tsol = nullptr;
};

int lsqqc(
	Trace&	trace,
	const double *H,
	const double *P,
	const double *Z,
	double *v,
	int m,
	int n,
	int ind,
	int norb,
	double *xo = nullptr,
	double *Po = nullptr);

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
	double		elmin);


#endif
