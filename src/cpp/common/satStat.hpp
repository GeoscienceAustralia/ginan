
#pragma once

#include <map>

using std::map;

#include "eigenIncluder.hpp"

#include "linearCombo.hpp"
#include "common.hpp"
#include "acsQC.hpp"
#include "enums.h"

/** Object containing persistent status parameters of individual signals
*/
struct SigStat
{
	union SlipStat
	{
		unsigned int any	= 0;	///< Non zero value indicates a slip has been detected
		struct
		{
			unsigned LLI	: 1;	///< Slip detected by loss of lock indicator
			unsigned GF		: 1;	///< Slip detected by geometry free combination
			unsigned MW		: 1;	///< Slip detected by Melbourne Wubenna combination
			unsigned SCDIA	: 1;	///< Slip detected DIA
		};
	};

	SlipStat savedSlip;
	SlipStat slip;

	unsigned int	phaseRejectCount	= 0;
	GTime			lastPhaseTime;
};

struct IonoStat
{
	double	ambvar		= 0;
	double	gf_amb		= 0;
	GTime	lastObsTime = {};
	double	extiono		= 0;
	double	extionovar	= 0;
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

struct QC
{
	Average		mwSlip		= {};		///<
	Average		emwSlip		= {};		///<
	int			amb[3]		= {}; 		///< repaired integer cycle slip
	double		mw			= 0;		///< MW-LC (m)
	double		gf			= 0;
	flt_t		flt			= {};		///< cycle slip repair filter
	lc_t		lc_pre		= {};		///< lc information
	lc_t		lc_new		= {};		///< lc information
};

struct AzEl
{
	double	az = 0;		///< azimuth angle (rad)
	double	el = 0;		///< elevation angle (rad)
};

/** Object containing persistent status parameters of individual satellites
*/
struct SatStat : IonoStat, QC, AzEl
{
	double  	phw				= 0;		///< Phase windup (cycle)
	double  	mapWet			= 0;		///< troposphere wet mapping function
	double  	mapWetGrads[2]	= {};		///< troposphere wet mapping function
	VectorEcef	e;							///< Line-of-sight unit vector

	GTime lastIonTime;

	double		dIono		= 0;			///< TD ionosphere residual
	double		sigmaIono	= 0;			///< TD ionosphere residual noise
	double		prevSTEC	= 0;

	double		nadir		= 0;
	bool		slip		= false;

	map<string, SigStat>	sigStatMap;		///< Map for individual signal status for this SatStat object

};

string ft2string(E_FType ft);
