#ifndef __STATION__HPP
#define __STATION__HPP

#include <memory>

#include "constants.h"
#include "gTime.hpp"
#include "snx.hpp"
#include "ppp.hpp"
#include "vmf3.h"

#include "eigenIncluder.hpp"

struct RinexStation
{
	string id; 				///< marker name
	string marker; 			///< marker number
	string antDesc;			///< antenna descriptor
	string antSerial; 		///< antenna serial number
	string recType; 		///< receiver type descriptor
	string recFWVersion; 	///< receiver firmware version
	string recSerial; 		///< receiver serial number
	int antSetup	= 0;	///< antenna setup id
	int itrfYear	= 0;	///< ITRF realization year
	int deltaFrame	= 0;	///< antenna delta type (0:enu,1:xyz)
	double hgt		= 0;	///< antenna height (m)
	Vector3d	del	= Vector3d::Zero();		///< antenna position delta (e/n/u or x/y/z) (m)
	Vector3d 	pos	= Vector3d::Zero();
};

struct IonoStation
{
	double Rot_pos[3]	= {};
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

/** Object to maintain receiver station data
*/
struct Station : IonoStation, StationLogs
{
	RinexStation		rnxStation;
	rtk_t 				rtk;						///< Legacy rtk filter status
	Sinex_stn_snx_t		snx;						///< Antenna information
	vmf3_t				vmf3	= {.m = 1};

	ObsList				obsList;					///< Observations available for this station at this epoch
	string				id;							///< Unique name for this station (4 characters)

	double				mjd0[3]			= {}; 		// mjd time for vmf3
	ClockJump			cj				= {};

	string				traceFilename;
	
	
	bool		primaryApriori = false;
	GTime		aprioriTime	= {};
	Vector3d	aprioriPos	= Vector3d::Zero();		///< station position (ecef) (m)
	Vector3d	aprioriVar	= Vector3d::Zero();
};

using StationList = list<Station*>;			///< List of station pointers

struct Network
{
	string traceFilename;
	string clockFilename;
	string rtsClockFilename;
	string id				= "NET";
	KFState kfState			= {};
};

#endif
