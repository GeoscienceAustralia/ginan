
// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>

#include <fstream>
#include <string>
#include <chrono>

using std::chrono::system_clock;
using std::string;

#include "peaCommitVersion.h"
#include "streamTrace.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "erp.hpp"


/** read earth rotation parameters
 */
int readerp(
	string	filename,	///< IGS ERP file (IGS ERP ver.2)
	ERP&	erp)			///< earth rotation parameters
{
	FILE*		fp;
	ERPData*	erp_data;

//	trace(3,"%s: file=%s\n",__FUNCTION__, file);

	std::ifstream filestream(filename);
	if (!filestream)
	{
//         trace(2,"erp file open error: file=%s\n",file);
		return 0;
	}
	
	while (filestream)
	{
		string line;
		
		getline(filestream, line);

		double v[14] = {};
		
		if (sscanf(line.c_str(),"%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
				v,v+1,v+2,v+3,v+4,v+5,v+6,v+7,v+8,v+9,v+10,v+11,v+12,v+13) < 5)
		{
			continue;
		}
	
		ERPData erpData;
		
		erpData.mjd		= v[0];
		erpData.xp		= v[1]	* 1E-6*AS2R;
		erpData.yp		= v[2]	* 1E-6*AS2R;
		erpData.ut1_utc	= v[3]	* 1E-7;
		erpData.lod		= v[4]	* 1E-7;
		erpData.xpr		= v[12]	* 1E-6*AS2R;
		erpData.ypr		= v[13]	* 1E-6*AS2R;
		
		erp.erpMap[erpData.mjd] = erpData;
	}
	
	return 1;
}


/** Get earth rotation parameter values
 */
int geterp(
	ERP&			erp,
	double			mjd,
	ERPValues&		erpv)
{
	if (erp.erpMap.empty())
		return 0;
	
	//find two erps to interpolate between (may be duplicate)
	ERPData* erp1_ptr;
	ERPData* erp2_ptr;
	
	auto it = erp.erpMap.lower_bound(mjd);
	
	if		(it == erp.erpMap.end())
	{
		auto lastIt = erp.erpMap.rbegin();
		
		auto& [dummy, last] = *lastIt;
		erp1_ptr = &last;
		erp2_ptr = &last;
	}
	else if (it == erp.erpMap.begin())
	{
		auto& [dummy, first] = *it;
		erp1_ptr = &first;
		erp2_ptr = &first;
	}
	else
	{
		auto& [dummy2, erp2] = *it;
		erp2_ptr = &erp2;
		it--;
		
		auto& [dummy1, erp1] = *it;
		erp1_ptr = &erp1;
	}
	
	ERPData& erp1 = *erp1_ptr;
	ERPData& erp2 = *erp2_ptr;
	
	//interpolate values between erps
	double a;
	if (erp2.mjd == erp1.mjd)	a = 0;	
	else						a = (mjd - erp1.mjd) / (erp2.mjd - erp1.mjd);
	
	erpv.xp			= (1-a) * erp1.xp		+ a * erp2.xp;
	erpv.yp			= (1-a) * erp1.yp		+ a * erp2.yp;
	erpv.ut1_utc	= (1-a) * erp1.ut1_utc	+ a * erp2.ut1_utc;
	erpv.lod		= (1-a) * erp1.lod		+ a * erp2.lod;
	
	return 1;
}

/** Get earth rotation parameter values
 */
int geterp(
	ERP&			erp, 
	GTime			time,
	ERPValues&		erpv)
{
	const double ep[] = {2000, 1, 1, 12, 0, 0};
	double day;

//	trace(4,"geterp:\n");

	double mjd = 51544.5 + (gpst2utc(time) - epoch2time(ep)) / 86400.0;		//todo aaron, convert to function

	return geterp(erp, mjd, erpv);
}

/* get earth rotation parameter values -----------------------------------------
* get earth rotation parameter values
* args   : erp_t  *erp        I   earth rotation parameters
*          double time        I   time (modified julian date)
*          double *erpv       O   erp values {xp,yp,ut1_utc,lod} (rad,rad,s,s/d)
* return : status (1:ok,0:error)
*-----------------------------------------------------------------------------*/
// int geterp_from_utc(
// 	const erp_t*	erp,
// 	double 			leapSec, 
// 	double			mjd,
// 	double*			erpv)
// {
// 	double day;
// 
// //	trace(4,"geterp:\n");
// 
// 	if (erp->n <= 0)
// 		return 0;
// 
// 	if (mjd <= erp->data[0].mjd)
// 	{
// 		day = mjd-erp->data[0].mjd;
// 		
// 		erpv[0]	= erp->data[0].xp     			+ erp->data[0].xpr * day;
// 		erpv[1]	= erp->data[0].yp     			+ erp->data[0].ypr * day;
// 		erpv[2]	= erp->data[0].ut1_utc			- erp->data[0].lod * day;
// 		erpv[3]	= erp->data[0].lod;
// 		erpv[4] = leapSec;
// 		
// 		return 1;
// 	}
// 	
// 	if (mjd >= erp->data[erp->n-1].mjd)
// 	{
// 		day = mjd-erp->data[erp->n-1].mjd;
// 		
// 		erpv[0] = erp->data[erp->n-1].xp     	+ erp->data[erp->n-1].xpr * day;
// 		erpv[1] = erp->data[erp->n-1].yp     	+ erp->data[erp->n-1].ypr * day;
// 		erpv[2] = erp->data[erp->n-1].ut1_utc	- erp->data[erp->n-1].lod * day;
// 		erpv[3] = erp->data[erp->n-1].lod;
// 		erpv[4] = leapSec;
// 
// 		return 1;
// 	}
// 	
// 	int i;
// 	int j = 0;
// 	int k = erp->n - 1;
// 	while (j < k-1)
// 	{
// 		i = (j+k) / 2;
// 		
// 		if (mjd < erp->data[i].mjd)
// 			k = i; 
// 		else 
// 			j = i;
// 	}
// 	
// 	double a;
// 	if (erp->data[j].mjd == erp->data[j+1].mjd)
// 	{
// 		a = 0.5;
// 	}
// 	else
// 	{
// 		a = (mjd - erp->data[j].mjd) / (erp->data[j+1].mjd - erp->data[j].mjd);
// 	}
// 	
// 	erpv[0] = (1-a) * erp->data[j].xp     + a * erp->data[j+1].xp;
// 	erpv[1] = (1-a) * erp->data[j].yp     + a * erp->data[j+1].yp;
// 	erpv[2] = (1-a) * erp->data[j].ut1_utc+ a * erp->data[j+1].ut1_utc;
// 	erpv[3] = (1-a) * erp->data[j].lod    + a * erp->data[j+1].lod;
// 	erpv[4] = leapSec;
// 	
// 	return 1;
// }

void writeERP(
	string		filename,
	ERPData&	erpAdj)
{
	std::ofstream erpStream(filename, std::ios::out);

	if (!erpStream)
	{
		BOOST_LOG_TRIVIAL(error) << "Error opening " << filename << " for ERP file.";
		
		return;
	}

	auto peaStopTime = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
	
	erpStream << "VERSION 2" << std::endl;
	erpStream << " Generated by GINAN " << GINAN_COMMIT_VERSION << " branch " << GINAN_BRANCH_NAME << " at " << peaStopTime << std::endl;
	
	erpStream << "--------------------------------------------------------------------------------------------------------------" << std::endl;
	erpStream << "     MJD    Xpole    Ypole  UT1-UTC      LOD     Xsig     Ysig    UTsig   LODsig  Nr  Nf  Nt      Xrt      Yrt" << std::endl;
	erpStream << "           1E-6as   1E-6as    1E-7s  1E-7s/d   1E-6as   1E-6as    1E-7s  1E-7s/d               1E-6/d   1E-6/d" << std::endl;
              
	
	int numRecs			= 0;
	int numFixedRecs	= 0;
	int numSats			= 0;
		
	for (auto& [dummy, olderp] : nav.erp.erpMap)
	{
		ERPData data = olderp;
		
		double dmjd = erpAdj.mjd - olderp.mjd;
		
		data.xp			+= erpAdj.xp		- dmjd * erpAdj.xpr;
		data.yp			+= erpAdj.yp		- dmjd * erpAdj.ypr;
		data.ut1_utc	+= erpAdj.ut1_utc	- dmjd * erpAdj.lod;
		
		data.xpr		+= erpAdj.xpr;	
		data.ypr		+= erpAdj.ypr;	
		data.lod		+= erpAdj.lod;
		
		tracepdeex(0, erpStream, "%8.2f %8d %8d %8d %8d %8d %8d %8d %8d %3d %3d %3d %8d %8d\n",
						data.mjd,
				(int)	(data.xp		* 1E6 / AS2R),
				(int)	(data.yp		* 1E6 / AS2R),
				(int)	(data.ut1_utc	* 1E7),
				(int)	(data.lod		* 1E7),
						0,                  
						0,                  
						0,                  
						0,                  
						numRecs,            
						numFixedRecs,       
						numSats,            
				(int)	(data.xpr		* 1E6 / AS2R),
				(int)	(data.ypr		* 1E6 / AS2R));
	}
}

void writeERPFromNetwork(
	KFState&	kfState,
	GTime		time)
{
	double ep[6];
	time2epoch(time, ep);
	double jd	= ymdhms2jd(ep);
	
	ERPData erpd;
	erpd.mjd	= jd - JD2MJD;
	
	kfState.getKFValue({.type = KF::EOP,		.str= "_XP"},	erpd.xp);				erpd.xp			*= 1e-3 * AS2R;
	kfState.getKFValue({.type = KF::EOP,		.str= "_YP"},	erpd.yp);				erpd.yp			*= 1e-3 * AS2R;
	kfState.getKFValue({.type = KF::EOP,		.str= "_UT1"},	erpd.ut1_utc);			erpd.ut1_utc	*= 1e-3;
	kfState.getKFValue({.type = KF::EOP_RATE,	.str= "_XP"},	erpd.xpr);				erpd.xpr		*= 1e-3 * AS2R;
	kfState.getKFValue({.type = KF::EOP_RATE,	.str= "_YP"},	erpd.ypr);				erpd.ypr		*= 1e-3 * AS2R;
	kfState.getKFValue({.type = KF::EOP_RATE,	.str= "_UT1"},	erpd.lod);				erpd.lod		*= 1e-3;
	
	writeERP(acsConfig.erp_filename, erpd);
}
