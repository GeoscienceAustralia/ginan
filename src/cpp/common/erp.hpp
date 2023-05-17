
#pragma once

#include <string>
#include <map>

using std::string;
using std::map;


constexpr char eopComments[][16] = {"XP (MAS)", "YP (MAS)", "UT1(MTS)"};

/** earth rotation parameter data type 
 */
struct ERPValues
{        
	GTime time;
	
	union 
	{
		double vals[4] = {};
		struct 
		{
			double xp;			///< pole offset (rad) 
			double yp;			///< pole offset (rad) 
			double ut1Utc;		///< ut1-utc (s) 
			double lod;			///< delta length of day (s/day) 
		};
	};
	
	double	xpr			= 0;		///< pole offset rate (rad/day) 
	double	ypr			= 0;		///< pole offset rate (rad/day) 
	
	double	xpSigma		= 0;
	double	ypSigma		= 0;
	double	xprSigma	= 0;
	double	yprSigma	= 0;
	double	ut1UtcSigma	= 0;
	double	lodSigma	= 0;
};

struct ERP
{        
	map<GTime, ERPValues>	erpMap;
};

struct KFState;

void readerp(
	string	file,
	ERP&	erp);

ERPValues geterp(
	ERP&			erp,
	GTime			time);

void writeERPFromNetwork(
	string		filename,
	KFState&	kfState);
