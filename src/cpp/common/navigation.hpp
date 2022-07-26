#ifndef __NAVIGATION_HPP__
#define __NAVIGATION_HPP__

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/binary_object.hpp>
#include <boost/serialization/list.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/unordered_map.hpp>

#include <unordered_map>
#include <unordered_set>
#include <string>
#include <list>
#include <map>
#include <set>

using std::unordered_set;
using std::unordered_map;
using std::string;
using std::list;
using std::map;
using std::set;


#include "streamTrace.hpp"
#include "ephemeris.hpp"
#include "antenna.hpp"
#include "gravity.hpp"
#include "orbits.hpp"
#include "satSys.hpp"
#include "gTime.hpp"
#include "enums.h"
#include "ssr.hpp"
#include "erp.hpp"
#include "trop.h"
#include "vmf3.h"

#define MAXDTE      900.0           /* max time difference to ephem time (s) */

typedef map<GTime, Peph>	PephList;
typedef list<Pclk>			PclkList;

struct TECPoint
{
	double data = 0;		///< TEC grid data (tecu)
	double rms	= 0;		///< RMS values (tecu)
};

struct tec_t
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
	double				wlbias;						///< wide-lane bias (cycle)
	
	SSRMaps				receivedSSR;				///< SSR corrections
	
	SatOrbit			satOrbit	 	= {};
	
	Eph*				eph_ptr			= nullptr;
	Geph*				geph_ptr		= nullptr;
	Seph*				seph_ptr		= nullptr;
	Ceph*				ceph_ptr		= nullptr;
	STO*				sto_ptr			= nullptr;
	EOP*				eop_ptr			= nullptr;
	ION*				ion_ptr			= nullptr;
	MatrixXd			satPartialMat;				///< Partial derivative matrices for orbits
};

struct Navigation
{
	///< navigation data type

	map<string, 	map<E_FType, 		map<GTime, Vector3d,		std::greater<GTime>>>>	 pcoMap;
	map<string, 	map<E_FType, 		map<GTime, PhaseCenterData,	std::greater<GTime>>>>	 pcvMap;
	
	map<int,							map<GTime, Eph,				std::greater<GTime>>>	 ephMap;	/* GPS/QZS/GAL/BDS ephemeris */
	map<int,							map<GTime, Geph,			std::greater<GTime>>>	gephMap;	/* GLONASS ephemeris */
	map<int,							map<GTime, Seph,			std::greater<GTime>>>	sephMap;	/* SBAS ephemeris */
	map<int,		map<E_NavMsgType,	map<GTime, Ceph,			std::greater<GTime>>>>	cephMap;	/* GPS/QZS/BDS CNVX ephemeris */
	map<E_Sys,		map<E_NavMsgType,	map<GTime, ION,				std::greater<GTime>>>>	 ionMap;	/* ION messages */
	map<E_StoCode,	map<E_NavMsgType,	map<GTime, STO,				std::greater<GTime>>>>	 stoMap;	/* STO messages */
	map<E_Sys,		map<E_NavMsgType,	map<GTime, EOP,				std::greater<GTime>>>>	 eopMap;	/* EOP messages */
	map<int,		PephList> 																pephMap;	/* precise ephemeris */
	map<string,		PclkList> 																pclkMap;	/* precise clock */
	map<GTime,		tec_t,											std::greater<GTime>>	 tecMap;	/* tec grid data */
	
	map<int,		SatNav>																  satNavMap;
	
// 	list<fcbd_t> 		fcbList;        /* satellite fcb data */
	vmf3_t	vmf3	= {.m = 1};
	ERP  	erp;         /* earth rotation parameters */
	int leaps;          /* leap seconds (s) */
	char glo_fcn[MAXPRNGLO+1];  /* glonass frequency channel number + 8 */
	double glo_cpbias[4];       /* glonass code-phase bias {1C,1P,2C,2P} (m) */

	
	EGMCoef			egm;
	struct jpl_eph_data*			jplEph_ptr = nullptr;
	
	list<string>					podInfoList;

	double		orography[NGRID]	= {};	 		/* vmf3 orography information, config->orography */
	gptgrid_t	gptg				= {};			/* gpt grid information */

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
		ar & nav.ephMap;
	}
}


extern map<E_Sys, E_NavMsgType> defNavMsgType;

extern	Navigation	nav;


#endif
