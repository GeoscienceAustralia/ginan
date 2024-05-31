
#pragma once

#include <memory>
#include <vector>
#include <string>
#include <list>
#include <map>

using std::make_shared;
using std::shared_ptr;
using std::vector;
using std::string;
using std::list;
using std::map;

#include "eigenIncluder.hpp"
#include "algebra.hpp"
#include "satSys.hpp"
#include "gTime.hpp"
#include "enums.h"
#include "crd.h"




struct GObs;
struct PObs;
struct FObs;
struct LObs;

struct ObsMeta
{
	ObsMeta() : exclude(0)
	{

	}

	union
	{
		const unsigned int exclude = 0;
		struct
		{
			unsigned excludeElevation		: 1;
			unsigned excludeEclipse			: 1;
			unsigned excludeSystem			: 1;
			unsigned excludeOutlier			: 1;
			unsigned excludeBadSPP			: 1;
			unsigned excludeConfig			: 1;
			unsigned excludeSVH				: 1;
			unsigned excludeBadRange		: 1;
			unsigned excludeDataHandling	: 1;
			unsigned excludeCom				: 1;
			unsigned excludeBadFlags		: 1;
			unsigned excludeAlert			: 1;
		};
	};
};

struct Observation : ObsMeta
{
	GTime	 	time	= {};       ///< Receiver sampling time (GPST)
	string 		mount;				///< ID of the receiver that generated the observation
	double		ephVar	= 0;

	virtual ~Observation() = default;

protected:
	GObs*	gObs_ptr = nullptr;
	PObs*	pObs_ptr = nullptr;
	FObs*	fObs_ptr = nullptr;
	LObs*	lObs_ptr = nullptr;
};

/** Raw observation data from a receiver for a single frequency. Not to be modified by processing functions
*/
struct RawSig
{
	E_ObsCode		code	= E_ObsCode::NONE;	///< Reported code type
	double			L		= 0;				///< Carrier phase (cycles)
	double			P		= 0;				///< Pseudorange (meters)
	double			D		= 0;				///< Doppler
	bool			LLI		= false;			///< Loss of lock indicator
	double			snr		= 0;				///< Signal to Noise ratio (dB-Hz)

	bool			invalid	= false;

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

	Sig(RawSig& raw) : RawSig(raw)
	{

	}

	double	codeVar		= 0;		///< Variance of code measurement
	double	phasVar		= 0;		///< Variance of phase measurement

	double	biases	[NUM_MEAS_TYPES] = {std::nan("")};
	double	biasVars[NUM_MEAS_TYPES] = {};
};



struct IonoPP
{
	double latDeg		= 0;
	double lonDeg		= 0;
	double slantFactor	= 1;
};

struct IonoObs
{
	IonoObs() : ionExclude(0)
	{

	}

	double	stecToDelay;
	int		stecType	= 0;
	double	stecVal;
	double	stecVar;
	int		stecCodeCombo;

	SatSys ionoSat;	//todo aaron, remove when possible

	map<int, IonoPP> ippMap;

	union
	{
		unsigned int ionExclude;
		struct
		{
			unsigned ionExcludeElevation	: 1;
			unsigned ionExcludeCode			: 1;
			unsigned ionExcludeLC			: 1;
			unsigned ionExcludeRange		: 1;
		};
	};
};

//forward declarations for pointers below
struct SatNav;
struct SatStat;
struct Receiver;


/** Observation metadata and data derived from it.
* All processing relevant for a single rec:sat:epoch should be stored here.
* For data that should persist across epochs: use SatStat.
**/
struct GObsMeta : IonoObs
{
	Receiver*	rec_ptr			= nullptr;

	double 		sppCodeResidual	= 0;				///< Residuals of code
	double		tropSlant		= 0;				///< Troposphere slant delay
	double		tropSlantVar	= 0;				///< Troposphere slant delay variance

};

/** Satellite position data - for determining and storing satellite positions/clocks
*/
struct SatPos
{
	SatPos() : failure(0)
	{

	}

	GTime				posTime;
	SatSys				Sat				= {};					///> Satellite ID (system, prn)
	SatNav*				satNav_ptr		= nullptr;				///< Pointer to a navigation object for this satellite
	SatStat* 			satStat_ptr		= nullptr;				///< Pointer to a status object for this satellite

	E_Source	posSource		= E_Source::NONE;
	E_Source	clkSource		= E_Source::NONE;

	VectorEcef	rSatCom;							///< ECEF based vector of satellite
	VectorEcef	rSatApc;							///< ECEF based vector of satellite
	VectorEcef	satVel;								///< ECEF based vector of satellite velocity
	VectorEci	rSatEciDt;							///< ECI  based vector of satellite          at transmission time
	VectorEci	vSatEciDt;							///< ECI  based vector of satellite velocity at transmission time
	VectorEci	rSatEci0;							///< ECI  based vector of satellite          at nominal epoch
	VectorEci	vSatEci0;							///< ECI  based vector of satellite velocity at nominal epoch

	double		posVar			= 0;				///< Variance of ephemeris derived values

	double		satClk			= 0;
	double		satClkVel		= 0;
	double		satClkVar		= 0;

	bool		sppValid		= 0;				///< Valid satellite flag

	int 		iodeClk			= -1;				///< Issue of data ephemeris
	int 		iodePos			= -1;				///< Issue of data ephemeris
	bool		ephPosValid		= false;
	bool		ephClkValid		= false;

	double		tof				= 0;				///< Estimated time of flight

	union
	{
		const unsigned int failure = 0;
		struct
		{
			unsigned failureExclude			: 1;
			unsigned failureNoSatPos		: 1;
			unsigned failureNoSatClock		: 1;
			unsigned failureNoPseudorange	: 1;
			unsigned failureIodeConsistency	: 1;
			unsigned failureBroadcastEph	: 1;
			unsigned failureSSRFail			: 1;
			unsigned failureSsrPosEmpty		: 1;
			unsigned failureSsrClkEmpty		: 1;
			unsigned failureSsrPosTime		: 1;
			unsigned failureSsrClkTime		: 1;
			unsigned failureSsrPosMag		: 1;
			unsigned failureSsrClkMag		: 1;
			unsigned failureSsrPosUdi		: 1;
			unsigned failureSsrClkUdi		: 1;
			unsigned failureGeodist			: 1;
			unsigned failureRSat			: 1;
			unsigned failureElevation		: 1;
			unsigned failurePrange			: 1;
			unsigned failureIonocorr		: 1;
		};
	};
};

/** Raw observation data from a receiver. Not to be modified by processing functions
*/
struct GObs : Observation, GObsMeta, SatPos
{
	map<E_FType, Sig>				sigs;		///> Map of signals available in this observation (one per frequency only)
	map<E_FType, list<Sig>>			sigsLists;	///> Map of all signals available in this observation (may include multiple per frequency, eg L1X, L1C)

	operator shared_ptr<GObs>()
	{
		auto pointer = make_shared<GObs>(*this);

		pointer->gObs_ptr = pointer.get();

		return pointer;
	}

	virtual ~GObs() = default;
};

struct PObs : Observation
{
	SatSys		Sat		= {};				///> Satellite ID (system, prn)
	Vector3d	pos		= Vector3d::Zero();
	Vector3d	vel		= Vector3d::Zero();

	operator shared_ptr<PObs>()
	{
		auto pointer = make_shared<PObs>(*this);

		pointer->pObs_ptr = pointer.get();

		return pointer;
	}

	virtual ~PObs() = default;
};

struct FObs : Observation
{
	KFState obsState;

	operator shared_ptr<FObs>()
	{
		auto pointer = make_shared<FObs>(*this);

		pointer->fObs_ptr = pointer.get();

		return pointer;
	}

	virtual ~FObs() = default;
};


/** List of observations for an epoch
*/
struct ObsList : vector<shared_ptr<Observation>>
{
	ObsList& operator+=(const ObsList& right)
	{
		this->insert(this->end(), right.begin(), right.end());
		return *this;
	}
};


//========================================================================================================
/*
#Mission Name            SP3c Code  ILRS ID    NORAD     Altitude [km] Inclination [deg]   Tracking Status
#                        (PRN)                           From       To
 GPS-MET                 L02        9501703    23547      740      740            69.900               Off
*/
//========================================================================================================
struct SatIdentity
{
	string	satName;	// Mission Name
	string	satId;		// SP3c Code (PRN)
	int		ilrsId;		// ILRS ID
	int		noradId;	// NORAD
	double	altitude[2];	// [km], 0: min, 1: max
	double	inclination;	// [deg]
	bool	tracking;	// Tracking Status
};

void	readSatId(string filepath);

extern map<int, SatIdentity> satIdMap;// index by ILRS ID

/** SLR normal point data from a session within a CRD file
*/
struct CrdSession
{
	CrdH1					h1;
	CrdH2					h2;
	CrdH3					h3;
	CrdH4					h4;
	CrdH5					h5;
	CrdC0					c0;
	CrdC1					c1;
	CrdC2					c2;
	CrdC3					c3;
	CrdC4					c4;
	CrdC5					c5;
	CrdC6					c6;
	CrdC7					c7;
	vector<struct	CrdD10>	d10;
	vector<struct	CrdD11>	d11;
	vector<struct	CrdD12>	d12;
	vector<struct	CrdD20>	d20;
	vector<struct	CrdD21>	d21;
	vector<struct	CrdD30>	d30;
	vector<struct	CrdD40>	d40;
	vector<struct	CrdD40>	d41;
	vector<struct	CrdD42>	d42;
	vector<struct	CrdD50>	d50;
	vector<struct	CrdD60>	d60;
	vector<struct	CrdD00>	d00;

	union
	{
		unsigned int read = 0;
		struct
		{
			unsigned readH1	: 1;
			unsigned readH2	: 1;
			unsigned readH3	: 1;
			unsigned readH4	: 1;
			unsigned readH5	: 1;
			unsigned readC0	: 1;
			unsigned readC1	: 1;
			unsigned readC2	: 1;
			unsigned readC3	: 1;
			unsigned readC4	: 1;
			unsigned readC5	: 1;
			unsigned readC6	: 1;
			unsigned readC7	: 1;
		};
	};
};


vector<CrdSession> readCrdFile(string filepath);

struct LObsMeta
{
	// Rec data
	string			recName		= {};
	int				recCdpId	= 0;

	// Sat data
	string			satName		= {};
	int				ilrsId		= 0;
	string			cosparId	= {};
};

struct LObs : Observation, LObsMeta, SatPos
{
	// Obs data
	E_CrdEpochEvent	epochEvent			= E_CrdEpochEvent::NONE;
	GTime			timeTx				= {};
	double			twoWayTimeOfFlight	= 0;

	double			pressure			= 0;	// hPa (mbar)
	double			temperature			= 0;	// K
	double			humidity			= 0;	// 0.00-1.00
	double			wavelengthNm		= 0;	// nm

	double			rangeBias			= 0;	// m
	double			timeBias			= 0;	// s
	double			pressureBias		= 0;	// hPa (mbar)
	double			humidityBias		= 0;	// 0.00-1.00

	operator shared_ptr<LObs>()
	{
		auto pointer = make_shared<LObs>(*this);

		pointer->lObs_ptr = pointer.get();

		return pointer;
	}

	virtual ~LObs() = default;
};

