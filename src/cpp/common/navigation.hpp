
#pragma once

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/binary_object.hpp>
#include <boost/serialization/map.hpp>

#include <string>
#include <map>
#include <set>

using std::string;
using std::map;
using std::set;

#include "azElMapData.hpp"
#include "ephemeris.hpp"
#include "attitude.hpp"
#include "antenna.hpp"
#include "orbits.hpp"
#include "satSys.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "enums.h"
#include "ssr.hpp"
#include "erp.hpp"

#define MAXDTE      900.0           /* max time difference to ephem time (s) */

struct TECPoint
{
	double data = 0;		///< TEC grid data (tecu)
	double rms	= 0;		///< RMS values (tecu)
};

struct TEC
{
	/// TEC grid type
	GTime				time;			///< epoch time (GPST)
	int					ndata[3];		///< TEC grid data size {nlat,nlon,nhgt}
	double				rb;				///< earth radius (km)
	double				lats[3];		///< latitude start/interval (deg)
	double				lons[3];		///< longitude start/interval (deg)
	double				hgts[3];		///< heights start/interval (km)
	vector<TECPoint>	tecPointVector;
};

struct SatNav
{
	map<int, double>	lamMap;

	SSRMaps				receivedSSR;				///< SSR corrections

	VectorEci			aprioriPos;					///< Inertial satellite position at epoch time
	double				aprioriClk		= 0;		///< Apriori clock value at epoch time

	AttStatus			attStatus		= {};		///< Persistent data for attitude model

	string				id;
	string				traceFilename;
	string				jsonTraceFilename;

	Vector3d			antBoresight	= {0,0,1};
	Vector3d			antAzimuth		= {0,1,0};

	SatPos				satPos0;					///< Satellite position when propagated to nominal time
};

/** navigation data type
 */
struct Navigation
{
	//these are interpolated between, dont need greater
	map<string,							map<GTime, Peph>> 																pephMap;	///< precise ephemeris
	map<string,							map<GTime, Pclk>> 																pclkMap;	///< precise clock
	map<string,							map<GTime, Att>>															 	attMapMap;	///< attitudes

	map<string,												map<GTime, AzElMapData<Vector3d>,	std::greater<GTime>>>	dragMap;
	map<string,												map<GTime, AzElMapData<Vector3d>,	std::greater<GTime>>>	reflectorMap;
	map<string, 	map<E_Sys,			map<E_FType, 		map<GTime, PhaseCenterOffset,		std::greater<GTime>>>>>	pcoMap;
	map<string, 	map<E_Sys,			map<E_FType, 		map<GTime, PhaseCenterData,			std::greater<GTime>>>>>	pcvMap;

	map<SatSys,		map<E_NavMsgType,	map<GTime, Eph,											std::greater<GTime>>>>	ephMap;			///< GPS/QZS/GAL/BDS ephemeris
	map<SatSys,		map<E_NavMsgType,	map<GTime, Geph,										std::greater<GTime>>>>	gephMap;		///< GLONASS ephemeris
	map<SatSys,		map<E_NavMsgType,	map<GTime, Seph,										std::greater<GTime>>>>	sephMap;		///< SBAS ephemeris
	map<SatSys,		map<E_NavMsgType,	map<GTime, Ceph,										std::greater<GTime>>>>	cephMap;		///< GPS/QZS/BDS CNVX ephemeris
	map<E_Sys,		map<E_NavMsgType,	map<GTime, ION,											std::greater<GTime>>>>	ionMap;			///< ION messages
	map<E_StoCode,	map<E_NavMsgType,	map<GTime, STO,											std::greater<GTime>>>>	stoMap;			///< STO messages
	map<E_Sys,		map<E_NavMsgType,	map<GTime, EOP,											std::greater<GTime>>>>	eopMap;			///< EOP messages
	map<GTime,		TEC,																		std::greater<GTime>>	tecMap;			///< tec grid data
	map<SatSys,		map<GTime, string,															std::greater<GTime>>>	svnMap;			///< Sat SVNs

	map<string,		string>																								blocktypeMap;	///< svn blocktypes

	map<SatSys,		SatNav>																								satNavMap;
	SSRAtm			ssrAtm;

	ERP		erp;						/* earth rotation parameters */
	int		leaps	= -1;				/* leap seconds (s) */
	char	glo_fcn[NSATGLO+1];			/* glonass frequency channel number + 8 */
	double	glo_cpbias[4];				/* glonass code-phase bias {1C,1P,2C,2P} (m) */


	struct jpl_eph_data*			jplEph_ptr = nullptr;

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & ephMap;
		ar & satNavMap;
	}
};


namespace boost::serialization
{
	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, Navigation& nav)
	{
// 		ar & nav.ephMap;
	}
}


extern map<E_Sys, E_NavMsgType> defNavMsgType;

extern	Navigation	nav;
