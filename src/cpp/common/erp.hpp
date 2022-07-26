
#ifndef __ERP_HPP__
#define __ERP_HPP__

#include <string>
#include <map>

using std::string;
using std::map;


constexpr char xp_str[]		= "_XP";
constexpr char yp_str[]		= "_YP";
constexpr char ut1_str[]	= "_UT1";

/** earth rotation parameter data type 
 */
struct ERPData
{        
	double mjd		= 0;		///< mjd (days) 
	double xp		= 0;		///< pole offset (rad)
	double yp		= 0;		///< pole offset (rad) 
	double xpr		= 0;		///< pole offset rate (rad/day) 
	double ypr		= 0;		///< pole offset rate (rad/day) 
	double ut1_utc	= 0;		///< ut1-utc (s) 
	double lod		= 0;		///< delta length of day (s/day) 
	
	double xp_sigma			= 0;
	double yp_sigma			= 0;
	double xpr_sigma		= 0;
	double ypr_sigma		= 0;
	double ut1_utc_sigma	= 0;
	double lod_sigma		= 0;
};

struct ERP
{        
	map<double, ERPData>	erpMap;
};

struct ERPValues
{
	union 
	{
		double vals[6] = {};
		struct 
		{
			double xp;			///< rads
			double yp;			///< rads
			double ut1_utc;		///< seconds
			double lod;			///< seconds/day
			double leaps;
		};
	};
};

struct GTime;
struct KFState;

int readerp(
	string	file,
	ERP&	erp);

int geterp(
	ERP&			erp,
	GTime			time,
	ERPValues&		erpv);

int geterp(
	ERP&			erp,
	double			mjd,
	ERPValues&		erpv);

void writeERPFromNetwork(
	string		filename,
	KFState&	kfState);

#endif
