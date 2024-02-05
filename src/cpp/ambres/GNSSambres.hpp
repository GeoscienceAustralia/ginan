
#pragma once

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "constants.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "satSys.hpp"
#include "common.hpp"
#include "trace.hpp"

extern double	FIXED_AMB_VAR;
extern bool 	AR_VERBO;

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
	map<E_Sys, bool> sys_solve;

	bool     endu     = false;
	int      mode     = E_ARmode::OFF;	/* AR mode */

	int    nset = 0;	/* candidate set size for lambda */
	int    nitr = 3;	/* number of iterations for iter_rnd */

	double MIN_Elev_prc = D2R * 10;	/* min elevation for processing */
	double MIN_Elev_AR  = D2R * 15;  /* min elevation for AR */
	double MIN_Elev_piv = D2R * 20;  /* min elevation for pivot */

	double sucthr       = 0.9999; 	/* success rate threshold */
	double ratthr       = 3;		/* ratio test threshold */

	bool  clear_old_amb = false;
	int    Max_Hold_epc = 0;		/* max hold (epoch) */
	double Max_Hold_tim = 600;		/* max hold (seconds) */

};

int		GNSS_AR(Trace& trace, GinAR_mtx& mtrx, GinAR_opt opt);
