
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
	double xp		= 0;
	double yp		= 0;
	double ut1_utc	= 0;
	double lod		= 0;
	double leaps	= 0;
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
	KFState&	kfState,
	GTime		time);

#endif
