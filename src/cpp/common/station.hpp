#ifndef __STATION__HPP
#define __STATION__HPP

#include "eigenIncluder.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "sinex.hpp"
#include "ppp.hpp"
#include "vmf3.h"

struct RinexStation
{
	string		id;						///< marker name
	string		marker;					///< marker number
	string		antDesc;				///< antenna descriptor
	string		antSerial;				///< antenna serial number
	string		recType;				///< receiver type descriptor
	string		recFWVersion; 			///< receiver firmware version
	string		recSerial; 				///< receiver serial number
	Vector3d	del	= Vector3d::Zero();	///< antenna position delta (e/n/u or x/y/z) (m)
	Vector3d 	pos	= Vector3d::Zero();
};


struct StationLogs
{
	GTime	firstEpoch	= GTime::noTime();
	GTime	lastEpoch	= GTime::noTime();
	int		epochCount	= 0;
	int		obsCount	= 0;
	int		slipCount	= 0;
	map<E_ObsCode, int>	codeCount;
	map<string, int>	satCount;
};


struct Rtk
{
	KFState					pppState;
	Solution				sol;		///< RTK solution
	string					antType;
	string					antId;
	map<SatSys, SatStat>	satStatMap;	
	double		otlDisplacement[6*11] = {};		///< ocean tide loading parameters
	Vector3d	antDelta	= Vector3d::Zero();		///< antenna delta {rov_e,rov_n,rov_u}
};


/** Object to maintain receiver station data
*/
struct Station : StationLogs, Rtk
{
	RinexStation		rnxStation;
	Sinex_stn_snx_t		snx;						///< Antenna information

	ObsList				obsList;					///< Observations available for this station at this epoch
	PseudoObsList		pseudoObsList;				///< PseudoObservations available for this station at this epoch
	string				id;							///< Unique name for this station (4 characters)
	
	bool		primaryApriori	= false;
	int			aprioriTime[3]	= {};
	Vector3d	aprioriPos		= Vector3d::Zero();		///< station position (ecef) (m)
	Vector3d	aprioriVar		= Vector3d::Zero();
	bool		ready			= false;
	
	string		traceFilename;
	
	
	map<SatSys, GTime> savedSlips;
};

using StationMap	= map<string, Station>;		///< Map of all stations


struct Network
{
	string traceFilename;
	string id				= "Network";
	
	KFState kfState			= {};
};

#endif
