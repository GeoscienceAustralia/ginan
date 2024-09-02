
#pragma once

#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "attitude.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "cache.hpp"
#include "gTime.hpp"
#include "ppp.hpp"

/** Solution of user mode processing functinos
*/
struct Solution
{
	/* solution type */
	GTime				sppTime;       							///< time (GPST)
	map<E_Sys, double>	dtRec_m; 								///< receiver clock bias to time systems (m)
	map<E_Sys, double>	dtRec_m_pppp_old; 						///< previous receiver clock bias to time systems (m)
	E_Solution			status;									///< solution status
	int					numMeas			= 0;					///< number of valid satellites
	KFState				sppState;								///< SPP filter object
	Dops				dops;									///< dilution of precision (GDOP,PDOP,HDOP,VDOP)
	VectorEcef			sppRRec;								///< Position vector from spp
};

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


struct ReceiverLogs
{
	PTime	firstEpoch	= GTime::noTime();
	PTime	lastEpoch	= GTime::noTime();
	int		epochCount	= 0;
	int		obsCount	= 0;
	int		slipCount	= 0;
	map<E_ObsCode, int>	codeCount;
	map<string, int>	satCount;

	int		receiverErrorEpochs	= 0;
	int		receiverErrorCount	= 0;
};


/** Structure of ocean/atmospheric tide loading displacements in amplitude and phase
*/
struct TidalDisplacement
{
	VectorEnu	amplitude;
	VectorEnu	phase;
};

/** Map of ocean/atmospheric tide loading displacements
*/
struct TideMap : map<E_TidalConstituent, TidalDisplacement>
{

};

struct Rtk
{
	Solution					sol;								///< RTK solution
	string						antennaType;
	string						receiverType;
	string						antennaId;
	map<SatSys, SatStat>		satStatMap;
	TideMap						otlDisplacement;					///< ocean tide loading parameters
	TideMap						atlDisplacement;					///< atmospheric tide loading parameters
	VectorEnu					antDelta;							///< antenna delta {rov_e,rov_n,rov_u}
	AttStatus					attStatus;
};

struct SinexSiteId;
struct SinexReceiver;
struct SinexAntenna;
struct SinexSiteEcc;

extern SinexSiteId			dummySiteid;
extern SinexReceiver		dummyReceiver;
extern SinexAntenna			dummyAntenna;
extern SinexSiteEcc			dummySiteEcc;

struct SinexRecData
{
	SinexSiteId*			id_ptr		= &dummySiteid;
	SinexReceiver*			rec_ptr		= &dummyReceiver;
	SinexAntenna*			ant_ptr		= &dummyAntenna;
	SinexSiteEcc*			ecc_ptr		= &dummySiteEcc;

	UYds		start;
	UYds		stop = UYds(-1,-1,-1);

	bool		primary		= false;			///< this position estimate is considered to come from a primary source
	VectorEcef	pos;
	VectorEcef	var;
	VectorEcef	vel;
	GTime		refEpoch	= {};
};

/** Object to maintain receiver station data
*/
struct Receiver : ReceiverLogs, Rtk
{
	bool				isPseudoRec	= false;
	bool				invalid		= false;
	SinexRecData		snx;						///< Antenna information

	map<string, string>					metaDataMap;
	ObsList								obsList;					///< Observations available for this station at this epoch
	string								id;							///< Unique name for this station (4 characters)

	bool		primaryApriori	= false;
	UYds		aprioriTime;
	double		aprioriClk		= 0;
	double		aprioriClkVar	= 0;
	Vector3d	aprioriPos		= Vector3d::Zero();		///< station position (ecef) (m)
	Matrix3d	aprioriVar		= Matrix3d::Zero();
	Vector3d	minconApriori	= Vector3d::Zero();

	VectorPos	pos;

	bool		ready			= false;

	Vector3d	antBoresight	= {0,0,1};
	Vector3d	antAzimuth		= {0,1,0};

	string		traceFilename;
	string		jsonTraceFilename;

	map<SatSys, GTime> savedSlips;

	union
	{
		const unsigned int failure = 0;
		struct
		{
			unsigned failureSinex			: 1;
			unsigned failureAprioriPos		: 1;
			unsigned failureEccentricity	: 1;
			unsigned failureAntenna			: 1;
		};
	};
	Cache<tuple<Vector3d, Vector3d, Vector3d, Vector3d, Vector3d>>	pppTideCache;
	Cache<tuple<Vector3d>>											pppEopCache;
};

struct ReceiverMap : map<string, Receiver>
{

};

extern ReceiverMap	receiverMap;

struct Network
{
	string traceFilename;
	string jsonTraceFilename;
	string id				= "Network";

	KFState kfState			= {};
};

void initialiseStation(
	string		id,
	Receiver&	rec);
