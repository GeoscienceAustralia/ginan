
#ifndef __ERP_HPP__
#define __ERP_HPP__

#include <map>

using std::map;


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
