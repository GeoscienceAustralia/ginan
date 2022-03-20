#ifndef __OBSERVATIONS_HPP_
#define __OBSERVATIONS_HPP_

#include <vector>
#include <string>
#include <list>
#include <map>

using std::vector;
using std::string;
using std::list;
using std::map;

#include "satSys.hpp"
#include "gTime.hpp"
#include "enums.h"

#include "eigenIncluder.hpp"

/** Raw observation data from a receiver for a single frequency. Not to be modified by processing functions
*/
struct RawSig
{
	E_ObsCode		code	= E_ObsCode::NONE;	///< Reported code type
	double			L		= 0;				///< Carrier phase (cycles)
	double			P		= 0;				///< Pseudorange (meters)
	double			D		= 0;				///< Doppler
	unsigned char	LLI		= 0;				///< Loss of lock indicator
	double			snr		= 0;				///< Signal to Noise ratio

	bool operator < (const RawSig& b) const
	{
		return (code < b.code);
	}
};

/** Per signal data that is calculated from the raw signals.
*/
struct Sig : RawSig
{
	Sig()
	{

	}

	Sig(RawSig raw) : RawSig(raw)
	{

	}

	bool	vsig		= 0;	///< Valid signal flag
	bool	phaseError	= 0;	///< Valid signal flag
	double	Range		= 0;	///< Corrected range
	double	L_corr_m	= 0;	///< Corrected carrier phase (meters)
	double	P_corr_m	= 0;	///< Corrected pseudorange (meters)
	double	codeRes		= 0;	///< Residuals of code measurement
	double	phasRes		= 0;	///< Residuals of phase measurement
	double	codeVar		= 0;	///< Variance of code measurement
	double	phasVar		= 0;	///< Variance of phase measurement
};

/** Raw observation data from a receiver. Not to be modified by processing functions
*/
struct RawObs
{
	GTime	 	time	= {};       		///> Receiver sampling time (GPST)
	SatSys		Sat		= {};				///> Satellite ID (system, prn)

	map<E_FType, Sig>			Sigs;		///> Map of signals available in this observation (one per frequency only)
	map<E_FType, list<RawSig>>	SigsLists;	///> Map of all signals available in this observation (may include multiple per frequency, eg L1X, L1C)
};

#define MAX_LAYER_NUM 4

struct IonoObs
{
	double STECtoDELAY;
	int    STECtype;
	double STECsmth;
	double STECsmvr;

	double latIPP[MAX_LAYER_NUM];
	double lonIPP[MAX_LAYER_NUM];
	double angIPP[MAX_LAYER_NUM];

	int ionExclude = false;
};

//forward declarations for pointers below
struct SatStat;
struct SatNav;
struct SatOrbit;

/** Observation, and data derived from it.
* All processing relevant for a single rec:sat:epoch should be stored here.
* For data that should persist across epochs: use satStat.
**/
struct Obs : RawObs, IonoObs
{
	Obs() : exclude(0)
	{

	}

	Obs(RawObs raw) : RawObs(raw), exclude(0)
	{

	}

	Vector3d	rSat			= Vector3d::Zero();	///< ECEF based vector to satellite
	Vector3d	satVel			= Vector3d::Zero();	///< ECEF based vector of satellite velocity
	double		ephVar			= 0;				///< Variance of ephemeris derived values
	
	SatStat* 	satStat_ptr		= 0;				///< Pointer to a status object for this satellite
	SatNav*		satNav_ptr		= 0;				///< Pointer to a navigation object for this satellite
	
	string 		mount			= "";				///< ID of the receiver that generated the observation
	double		dtSat[2]		= {};				///< Clock bias of the satellite
	E_Svh 		svh				= SVH_OK;			///< Satellite vehicle health
	int 		iode			= 0;				///< Issue of data ephemeris
	double 		rescode_v		= 0;				///< Residuals of code
	bool		vsat			= 0;				///< Valid satellite flag

	union
	{
		unsigned int exclude;
		struct
		{
			unsigned excludeElevation		: 1;
			unsigned excludeEclipse			: 1;
			unsigned excludeSystem			: 1;
			unsigned excludeSlip			: 1;
			unsigned excludeTrop			: 1;
			unsigned excludeMissingSig		: 1;
			unsigned excludeOutlier			: 1;
			unsigned excludeBadSPP			: 1;
		};
	};
};


struct PseudoObs
{
	GTime	 	time	= {};       		///> Receiver sampling time (GPST)
	SatSys		Sat		= {};				///> Satellite ID (system, prn)
	Vector3d	pos		= Vector3d::Zero();
	Vector3d	vel		= Vector3d::Zero();
};

typedef vector<Obs>			ObsList;		///< List of observations for an epoch
typedef vector<PseudoObs>	PseudoObsList;	///< List of observations for an epoch


#endif
