
#pragma once

#include <iostream>
#include <time.h>
#include <string>
#include <array>


#include <boost/date_time/posix_time/posix_time.hpp>

#include "enums.h"

using std::ostream;
using std::string;
using std::array;

struct PTime;
struct GTime;
struct UtcTime;
struct GEpoch;
struct GWeek;
struct GTow;
struct MjDateTT;
struct MjDateUtc;



#define GPS_SUB_UTC_2000	+13
#define GPS_SUB_UTC_2006	+14
#define GPS_SUB_TAI			-19

extern const GTime	GPS_t0;
extern const double	MJD_j2000;
extern const int	secondsInDay;

string	time2str(GTime t, int n);

GTime yds2time(
	const double*	yds,
	E_TimeSys		tsys = E_TimeSys::GPST);

void time2yds(
	GTime			time,
	double*			yds,
	E_TimeSys		tsys = E_TimeSys::GPST);

GTime epoch2time(
	const double*	ep,
	E_TimeSys		tsys = E_TimeSys::GPST);

void time2epoch(
	GTime			time,
	double*			ep,
	E_TimeSys		tsys = E_TimeSys::GPST);

double leapSeconds( GTime time );

struct Int
{
	int val = 0;

	Int()
	{

	}

	Int(
		const int& in)
	: val {in}
	{

	}

	operator int() const
	{
		return val;
	}

	Int& operator =(
		const int& in)
	{
		val = in;
		return *this;
	}
};

struct Double
{
	double val = 0;

	Double()
	{

	}

	Double(
		const double& in)
	: val {in}
	{

	}

	operator double() const
	{
		return val;
	}

	Double& operator =(
		const double& in)
	{
		val = in;
		return *this;
	}
};


struct GWeek	: Int
{
	GWeek(
		int in)
	{
		val = in;
	}
};

struct BWeek	: Int
{
	BWeek(
		int in)
	{
		val = in;
	}
};

struct GTow		: Double
{
	GTow(
		double in)
	{
		val = in;
	}
};

struct BTow		: Double
{
	BTow(
		double in)
	{
		val = in;
	}
};

struct RTod		: Double
{
	RTod(
		double in)
	{
		val = in;
	}
};

struct Duration
{
	long double bigTime	= 0;

	double to_double() const
	{
		return (double) bigTime;
	}

	long int to_int() const
	{
		return (long int) bigTime;
	}

	bool operator <		(const double& t2) const
	{
		if (this->bigTime	< t2)	return true;
		else						return false;
	}

	bool operator >		(const double& t2) const
	{
		if (this->bigTime	> t2)	return true;
		else						return false;
	}

	long double operator -		(const Duration& t2) const
	{
		return this->bigTime - t2.bigTime;
	}

	double operator /		(const Duration& t2) const
	{
		return this->bigTime / t2.bigTime;
	}


	friend ostream& operator<<(ostream& os, const Duration& time);
};

/** Time structure used throughout this software
*/
struct GTime
{
	mutable int			cacheN		=  0;
	mutable	long double	cacheTime	= -1;
	mutable string		cacheString;

	long double	bigTime = 0;


	/** Uninitialised time for comparisons
	*/
	static GTime noTime()
	{
		GTime nothing;
		return nothing;
	}

	GTime(
		GTow	tow,
		GTime	nearTime);

	GTime(
		BTow	tow,
		GTime	nearTime);

	GTime(
		RTod	tod,
		GTime	nearTime);

	string to_string(int n = 2) const;

	string to_ISOstring(int n = 2) const;

	double to_decYear() const;

	bool operator ==	(const GTime& t2) const
	{
		if (this->bigTime	!= t2.bigTime)	return false;
		else								return true;
	}

	bool operator !=	(const GTime& t2) const
	{
		return !(*this == t2);
	}

	bool operator <		(const GTime& t2) const
	{
		if (this->bigTime	< t2.bigTime)	return true;
		else								return false;
	}

	bool operator >		(const GTime& t2) const
	{
		if (this->bigTime	> t2.bigTime)	return true;
		else								return false;
	}

	bool operator >=	(const GTime& t2) const
	{
		if (*this		>	t2)		return true;
		if (*this		==	t2)		return true;
		else						return false;
	}

	friend ostream& operator<<(ostream& os, const GTime& time);

	GTime operator +(const double t) const
	{
		GTime gTime = *this;

		gTime.bigTime += t;

		return gTime;
	}

	GTime operator +(const int t) const
	{
		GTime gTime = *this;

		gTime.bigTime += t;

		return gTime;
	}

	GTime operator +(const Duration duration) const
	{
		GTime gTime = *this;

		gTime.bigTime += duration.bigTime;

		return gTime;
	}

	GTime& operator+=(const double rhs)
	{
		*this = *this + rhs;
		return *this;
	}

	GTime& operator-=(const double rhs)
	{
		*this = *this - rhs;
		return *this;
	}

	Duration operator -(const GTime t) const
	{
		Duration duration;
		duration.bigTime	= bigTime	- t.bigTime;

		return duration;
	}

	GTime operator -(const double t) const
	{
		GTime time = *this + (-t);
		return time;
	}

	GTime operator -(const Duration duration) const
	{
		GTime gTime = *this;
		gTime.bigTime -= duration.bigTime;
		return gTime;
	}

	GTime& operator++(int)
	{
		this->bigTime++;
		return *this;
	}

	GTime()
	{

	}

	GTime(
		GWeek	gpsWeek,
		GTow	tow);

	GTime(
		BWeek	bdsWeek,
		BTow	tow);

	GTime(
		MjDateTT mjdTT);

	GTime(
		MjDateUtc mjdUtc);


	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & bigTime;
	}

	GTime floorTime(
		double	period) const;

	string gregString();

	operator long double()		const;
	operator MjDateTT()			const;
	operator GEpoch()			const;
	operator UtcTime()			const;
	operator GWeek()			const;
	operator BWeek()			const;
	operator GTow()				const;
	operator BTow()				const;
	operator PTime()			const;
	operator string()			const;
	operator RTod()				const;
};

struct PTime
{
	long double bigTime	= 0;

	PTime()
	{

	}

	operator GTime() const;
};


GTime timeGet();


struct MjDateUtc
{
	long double val;

	MjDateUtc()
	{

	}

	MjDateUtc(
		GTime	time);

	double to_double() const
	{
		return (double) val;
	}
};


struct MjDateUt1
{
	long double val;

	MjDateUt1()
	{

	}

	MjDateUt1(
		GTime	time,
		double	ut1_utc);

	double to_double() const
	{
		return (double) val;
	}

	double to_j2000() const
	{
		return (double) (val - MJD_j2000);
	}
};

struct MjDateTT
{
	long double val;

	double to_double() const
	{
		return (double) val;
	}

	double to_j2000() const
	{
		return (double) (val - MJD_j2000);
	}
};

struct UtcTime
{
	long double bigTime;	// Eugene: bigTime can be ambiguous, e.g. 1167264000.5, never know if GPST is 2017-01-01 00:00:17.5 or 2017-01-01 00:00:18.5

	UtcTime operator +(const double t) const
	{
		UtcTime time = *this;

		time.bigTime += t;

		return time;
	}

	string to_string(int n = 2) const
	{
		GTime gTime;
		gTime.bigTime	= this->bigTime;
		string str = gTime.to_string(n);
		str += "Z";
		str[10]='T';
		return str;
	}

	string to_ISOstring(int n = 2) const
	{
		GTime gTime;
		gTime.bigTime	= this->bigTime;

		return gTime.to_ISOstring(n)+'Z';
	}

	UtcTime()
	{

	}

	operator GTime()	const;
};

struct GEpoch : array<double, 6>
{
	GTime toGTime() const;

	operator GTime() const;

	double& year;
	double& month;
	double& day;
	double& hour;
	double& min;
	double& sec;

	GEpoch(
		double yearVal	= 0,
		double monthVal	= 0,
		double dayVal	= 0,
		double hourVal	= 0,
		double minVal	= 0,
		double secVal	= 0)
	:	year	{ (*this)[0]},
		month	{ (*this)[1]},
		day 	{ (*this)[2]},
		hour	{ (*this)[3]},
		min 	{ (*this)[4]},
		sec		{ (*this)[5]}
	{
		year	= yearVal;
		month	= monthVal;
		day 	= dayVal;
		hour	= hourVal;
		min 	= minVal;
		sec		= secVal;
	}

	GEpoch(
		const GEpoch& other)
	:	year	{ (*this)[0]},
		month	{ (*this)[1]},
		day 	{ (*this)[2]},
		hour	{ (*this)[3]},
		min 	{ (*this)[4]},
		sec		{ (*this)[5]}
	{
		//special copy constructor to deal with aliases
		year	= other.year;
		month	= other.month;
		day 	= other.day;
		hour	= other.hour;
		min 	= other.min;
		sec		= other.sec;
	}

	GEpoch& operator = (
		const GEpoch& other)
	{
		year	= other.year;
		month	= other.month;
		day 	= other.day;
		hour	= other.hour;
		min 	= other.min;
		sec		= other.sec;

		return *this;
	}
};


struct UYds : array<double, 3>
{
	double& year;
	double& doy;
	double& sod;

	UYds(
		double yearval	= 0,
		double doyVal	= 0,
		double sodVal	= 0)
	:	year{ (*this)[0]},
		doy	{ (*this)[1]},
		sod { (*this)[2]}
	{
		year	= yearval;
		doy		= doyVal;
		sod		= sodVal;
	}

	UYds(
		const UYds& yds)
	:	year{ (*this)[0]},
		doy	{ (*this)[1]},
		sod { (*this)[2]}
    {
		//special copy constructor to deal with aliases
		year	= yds.year;
		doy		= yds.doy;
		sod		= yds.sod;
    }

	UYds& operator = (
		const UYds& other)
	{
		year	= other.year;
		doy		= other.doy;
		sod		= other.sod;

		return *this;
	}

	UYds(
		const GTime& time)
	:	year{ (*this)[0]},
		doy	{ (*this)[1]},
		sod { (*this)[2]}
	{
		time2yds(time, this->data(), E_TimeSys::UTC);
	}

	UYds& operator +=(
		const double offset)
	{
		sod += offset;
		while (sod > secondsInDay)		{	sod -= secondsInDay;		doy++;	}
		while (sod < 0)					{	sod += secondsInDay;		doy--;	}

		while (doy > 366)				{	doy -= 365;					year++;	}
		while (doy < 1)					{	doy += 365;					year--;	}

		return *this;
	}

	UYds& operator = (
		const GTime& time)
	{
		*this = UYds(time);

		return *this;
	}

	operator GTime()	const;
};

UtcTime gpst2utc (GTime t);
GTime utc2gpst (UtcTime t);

double  str2num(const char *s, int i, int n);

GTime	gpst2time(int week, double sec);
double  time2gpst(GTime t, int *week = nullptr);

GTime	bdt2time(int week, double sec);
double  time2bdt(GTime t, int *week = nullptr);

int str2time(
	const char*	s,
	int			i,
	int			n,
	GTime&		t,
	E_TimeSys	tsys = E_TimeSys::GPST);

void	jd2ymdhms(const double jd, double *ep);

double	ymdhms2jd(const double time[6]);


GTime nearestTime(
	GTime	referenceEpoch,
	double	tom,
	GTime	nearTime,
	int		mod);

boost::posix_time::ptime currentLogptime();
