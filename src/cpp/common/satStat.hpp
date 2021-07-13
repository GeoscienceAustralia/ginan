

#ifndef __SATSTAT_HPP__
#define __SATSTAT_HPP__

#include <unordered_map>

using std::unordered_map;

#include "eigenIncluder.hpp"

#include "linearCombo.hpp"
#include "acsQC.hpp"
#include "enums.h"

/** Object containing persistant status parameters of individual signals
*/
struct SigStat
{
	union
	{
		unsigned int any	= 0;	///< Non zero value indicates a slip has been detected
		struct
		{
			unsigned LLI	: 1;	///< Slip detected by loss of lock indicator
			unsigned GF		: 1;	///< Slip detected by geometry free combination
			unsigned MW		: 1;	///< Slip detected by Melbourne Wubenna combination
			unsigned EMW	: 1;	///< Slip detected by extended MW combination
			unsigned CJ		: 1;	///< Slip detected as clock jump
			unsigned SCDIA	: 1;	///< Slip detected DIA
		};
	} slip;

	unsigned int	phaseRejectCount	= 0;
	unsigned int	phaseOutageCount	= 0;	/* obs outage counter of phase */
};

struct IonoStat
{
	double	ambvar		= 0;
	double	gf_amb		= 0;
	GTime	lastObsTime = {};
	double	extiono		= 0;
	double	extionovar	= 0;
};

struct MWSlip
{
	double	mean	= 0;
	double	sigma	= 0;
	int		num		= 0;
};

/** Object containing persistant status parameters of individual satellites
*/
struct SatStat : IonoStat
{
	double  	phw				= 0;					///< Phase windup (cycle)
	double  	mapWet			= 0;					///< troposphere wet mapping function
	double  	mapWetGrads[2]	= {};					///< troposphere wet mapping function
	Vector3d	e				= Vector3d::Zero();		///< Line-of-sight unit vector

	MWSlip	mwSlip		= {};		///<
	MWSlip	emwSlip		= {};		///<
	int		amb[3]		= {}; 		///< repaired integer cycle slip
	double	mw			= 0;		///< MW-LC (m)
	double	gf			= 0;
	flt_t	flt			= {};		///< cycle slip repair filter
	lc_t	lc_pre		= {};		///< lc information
	lc_t	lc_new		= {};		///< lc information

	double	dIono		= 0;      	///< TD ionosphere residual
	double	sigmaIono	= 0;      	///< TD ionosphere residual noise

	union
	{
		double		azel[2] = {};	///< azimuth/elevation angles as array(rad)
		struct
		{
			double	az;			///< azimuth angle (rad)
			double	el;			///< elevation angle (rad)
		};
	};

	unordered_map<E_FType, SigStat>	sigStatMap;	///< Map for individual signal status for this SatStat object
};

#endif
