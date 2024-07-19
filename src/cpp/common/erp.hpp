
#pragma once

#include <string>
#include <vector>
#include <map>

using std::string;
using std::vector;
using std::map;

#include "common.hpp"


constexpr char eopComments[][16] = {"XP (MAS)", "YP (MAS)", "UT1(MTS)"};

/** earth rotation parameter data type
 */
struct ERPValues
{
	GTime time;

	double xp			= 0;		///< pole offset (rad)
	double yp			= 0;		///< pole offset (rad)
	double ut1Utc		= 0;		///< ut1-utc (s)
	double lod			= 0;		///< delta length of day (s/day)

	double	xpr			= 0;		///< pole offset rate (rad/day)
	double	ypr			= 0;		///< pole offset rate (rad/day)

	double	xpSigma		= 0;
	double	ypSigma		= 0;
	double	xprSigma	= 0;
	double	yprSigma	= 0;
	double	ut1UtcSigma	= 0;
	double	lodSigma	= 0;

	bool	isPredicted	= false;
	bool	isFiltered	= false;

	ERPValues operator +(const ERPValues& rhs)
	{
		ERPValues erpv = *this;

		erpv.time	+= rhs.time;
		erpv.xp		+= rhs.xp;
		erpv.yp		+= rhs.yp;
		erpv.ut1Utc	+= rhs.ut1Utc;
		erpv.lod	+= rhs.lod;

		erpv.xpr	+= rhs.xpr;
		erpv.ypr	+= rhs.ypr;

		erpv.xpSigma		= sqrt(SQR(erpv.xpSigma)		+ SQR(rhs.xpSigma));
		erpv.ypSigma		= sqrt(SQR(erpv.ypSigma)		+ SQR(rhs.ypSigma));
		erpv.xprSigma		= sqrt(SQR(erpv.xprSigma)		+ SQR(rhs.xprSigma));
		erpv.yprSigma		= sqrt(SQR(erpv.yprSigma)		+ SQR(rhs.yprSigma));
		erpv.ut1UtcSigma	= sqrt(SQR(erpv.ut1UtcSigma)	+ SQR(rhs.ut1UtcSigma));
		erpv.lodSigma		= sqrt(SQR(erpv.lodSigma)		+ SQR(rhs.lodSigma));

		erpv.isPredicted	|= rhs.isPredicted;

		return erpv;
	}

	bool operator ==(const ERPValues& rhs) const
	{
		bool equal	=	time	== rhs.time
					&&	xp		== rhs.xp
					&&	yp		== rhs.yp
					&&	ut1Utc	== rhs.ut1Utc
					&&	lod		== rhs.lod
					&&	xpr		== rhs.xpr
					&&	ypr		== rhs.ypr;

		return equal;
	}

	ERPValues operator *(const double scalar)
	{
		ERPValues erpv = *this;

		erpv.time.bigTime	*= scalar;
		erpv.xp				*= scalar;
		erpv.yp				*= scalar;
		erpv.ut1Utc			*= scalar;
		erpv.lod			*= scalar;

		erpv.xpr			*= scalar;
		erpv.ypr			*= scalar;

		erpv.xpSigma		*= scalar;
		erpv.ypSigma		*= scalar;
		erpv.xprSigma		*= scalar;
		erpv.yprSigma		*= scalar;
		erpv.ut1UtcSigma	*= scalar;
		erpv.lodSigma		*= scalar;

		return erpv;
	}

	string toString();
	string toReadableString();
};

struct ERP
{
	vector<map<GTime, ERPValues>>	erpMaps;

	ERPValues						filterValues;
};

struct KFState;

void readErp(
	string		filename,
	ERP&		erp);

ERPValues getErp(
	ERP&		erp,
	GTime		time,
	bool		useFilter = true);

void writeErp(
	string		filename,
	ERPValues&	erp);

void writeErpFromNetwork(
	string		filename,
	KFState&	kfState);

ERPValues getErpFromFilter(
	const KFState&	kfState);

Matrix3d stationEopPartials(
	Vector3d&	rRec);
