
#pragma once

#include <map>

using std::map;

#include "satSys.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "enums.h"


struct S_LC
{
	bool	valid = false;

	double	GF_Phas_m;
	double	GF_Code_m;

	double	WL_Phas_m;
	double	WL_Phas_c;
	double	WL_Code_m;
	double	WL_Code_c;

	double	NL_Phas_m;
	double	NL_Phas_c;
	double	NL_Code_m;
	double	NL_Code_c;

	double	IF_Phas_m;
	double	IF_Code_m;

	double	MW_m;
	double	MW_c;

	double	lam_A;
	double	lam_B;
	double	lam_WL;
	double	lam_NL;
};

struct lc_t
{
	GTime	time;       			///< receiver sampling time (GPST) */
	SatSys	Sat;

	map<E_FType, double> L_m;
	map<E_FType, double> P;
	map<E_FType, double> mp;		///< Multipath info (m)

	map<std::pair<E_FType, E_FType>, S_LC> lcMap;	///< interfrequency linear combination parameters
};

//forward declarations
struct Navigation;
struct ObsList;
struct GObs;


S_LC	getLC(double L_A, double L_B, double P_A, double P_B, double lamA, double lamB, double* c1_out, double* c2_out);
S_LC&	getLC(lc_t& lcBase, E_FType fA, E_FType fB);
S_LC&	getLC(GObs& obs, lc_t& lcBase, E_FType fA, E_FType fB);

void obs2lcs(
	Trace&		trace,
	ObsList&	obsList);

