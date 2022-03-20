#ifndef __LINEAR_COMBO_H_
#define __LINEAR_COMBO_H_

#include <map>

using std::map;

#include "observations.hpp"
#include "streamTrace.hpp"
#include "gTime.hpp"
#include "satSys.hpp"
#include "enums.h"


typedef struct
{
	bool	valid;

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
// 	double	gf;		///< geometry-free LC for phase (m)
// 	double	gfc;  	///< geometry-free LC for code (m)
// 	double	wl;		///< wide-lane LC for phase (m)
// 	double	If;		///< ionosphere-free LC for phase (m)
// 	double	ifc;	///< ionosphere-free LC for code (m)
// 	double	mw;		///< Melbourne Wenbunna LC for phase (cycle)
// 	double	nl; 	///< Narrow-lane LC for code (m)
} S_LC;

typedef struct
{
	GTime	time;       			///< receiver sampling time (GPST) */
	SatSys	Sat;

	map<E_FType, double> L_m;
	map<E_FType, double> P;
	map<E_FType, double> mp;		///< Multipath info (m)

	map<std::pair<E_FType, E_FType>, S_LC> lcMap;	///< interfrequency linear combination parameters
} lc_t;

//forward declarations
struct nav_t;

S_LC	getLC(double L_A, double L_B, double P_A, double P_B, double lamA, double lamB, double* c1_out, double* c2_out);
S_LC&	getLC(lc_t& lcBase, E_FType fA, E_FType fB);
S_LC&	getLC(Obs& obs, lc_t& lcBase, E_FType fA, E_FType fB);

void obs2lc(FILE* fppde, Obs& obs, lc_t& lc, const nav_t* nav, int cscase, int csfreq);

void obs2lcs(
	Trace&		trace,
	ObsList&	obsList);

#endif
