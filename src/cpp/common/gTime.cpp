
// #pragma GCC optimize ("O0")

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <iostream>

using std::ostream;

#include "navigation.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "gTime.hpp"
#include "enums.h"


const GTime j2000TT		= GEpoch{2000, E_Month::JAN, 1,		11,	58,	55.816	+ GPS_SUB_UTC_2000};	// defined in utc 11:58:55.816
const GTime j2000Utc	= GEpoch{2000, E_Month::JAN, 1,		12,	0,	0};								// right answer, wrong reason? todo

const GTime GPS_t0		= GEpoch{1980, E_Month::JAN, 6,		0,	0,	0};								// gps time reference
const GTime GLO_t0		= GEpoch{1980, E_Month::JAN, 6,	   21,	0,	0};								// glo time reference (without leap seconds)
const GTime GAL_t0		= GEpoch{1999, E_Month::AUG, 22,	0,	0,	0};								// galileo system time reference as gps time -> 13 seconds before 0:00:00 UTC on Sunday, 22 August 1999 (midnight between 21 and 22 August)
const GTime BDS_t0		= GEpoch{2006, E_Month::JAN, 1,		0,	0,	0		+ GPS_SUB_UTC_2006};	// beidou time reference as gps time - defined in utc 11:58:55.816

const int		GPS_t0_sub_POSIX_t0	= 315964800;
const double	MJD_j2000			= 51544.5;

const auto POSIX_GPS_t0	= boost::posix_time::from_time_t(GPS_t0_sub_POSIX_t0);


const int			secondsInWeek	= 60 * 60 * 24 * 7;
const int			secondsInDay	= 60 * 60 * 24;
const long double	secondsInDayP	= 60 * 60 * 24;

map<GTime, int, std::greater<GTime>> leapSecondMap =
{
	{ GEpoch{2017,	1,	1,	0,	0,	18},	18},
	{ GEpoch{2015,	7,	1,	0,	0,	17},	17},
	{ GEpoch{2012,	7,	1,	0,	0,	16},	16},
	{ GEpoch{2009,	1,	1,	0,	0,	15},	15},
	{ GEpoch{2006,	1,	1,	0,	0,	14},	14},
	{ GEpoch{1999,	1,	1,	0,	0,	13},	13},
	{ GEpoch{1997,	7,	1,	0,	0,	12},	12},
	{ GEpoch{1996,	1,	1,	0,	0,	11},	11},
	{ GEpoch{1994,	7,	1,	0,	0,	10},	10},
	{ GEpoch{1993,	7,	1,	0,	0,	9},		9},
	{ GEpoch{1992,	7,	1,	0,	0,	8},		8},
	{ GEpoch{1991,	1,	1,	0,	0,	7},		7},
	{ GEpoch{1990,	1,	1,	0,	0,	6},		6},
	{ GEpoch{1988,	1,	1,	0,	0,	5},		5},
	{ GEpoch{1985,	7,	1,	0,	0,	4},		4},
	{ GEpoch{1983,	7,	1,	0,	0,	3},		3},
	{ GEpoch{1982,	7,	1,	0,	0,	2},		2},
	{ GEpoch{1981,	7,	1,	0,	0,	1},		1},
	{ GEpoch{1980,	1,	6,	0,	0,	0},		0}
};


ostream& operator <<(ostream& stream, const GTime& time)
{
	stream << time.to_string();
	return stream;
}

ostream& operator <<(ostream& stream, const Duration& duration)
{
	char buff[64];

	int decimal = (duration.bigTime - floor(duration.bigTime)) * 100;

	snprintf(buff, sizeof(buff), "%02d:%02d:%02d.%02d",
			(int)	duration.bigTime / 60 / 60,
			(int)	duration.bigTime / 60			% 60,
			(int)	duration.bigTime				% 60,
					decimal);

	stream << buff;

	return stream;
}

/* convert substring in string to number
* args   : char   *s        I   string ("... nnn.nnn ...")
*          int    i,n       I   substring position and width
* return : converted number (0.0:error)
*/
double str2num(const char *s, int i, int n)
{
	double value;
	char str[256],*p=str;

	if	( i<0
		||(int)strlen(s)<i
		||(int)sizeof(str)-1<n)
		return 0.;

	for (s+=i;*s&&--n>=0;s++)
		*p++ = *s == 'd' || *s == 'D' ? 'E' : *s;

	*p='\0';
	return sscanf(str, "%lf", &value) == 1 ? value : 0;
}

/* convert substring in string to GTime struct
* args   : char   *s        I   string ("... yyyy mm dd hh mm ss ...")
*          int    i,n       I   substring position and width
*          GTime *t       O   GTime struct
* return : status (0:ok,0>:error)*/
int str2time(
	const char*	s,
	int			i,
	int			n,
	GTime&		t,
	E_TimeSys	tsys)
{
	double ep[6];
	char str[256],*p=str;

	if	( i					< 0
		||(int)strlen(s)	< i
		||(int)sizeof(str)-1< i)
	{
		return -1;
	}

	for (s+=i;*s&&--n>=0;)
	{
		*p++=*s++;
	}
	*p='\0';

	int readCount = sscanf(str,"%lf %lf %lf %lf %lf %lf",
		ep,
		ep+1,
		ep+2,
		ep+3,
		ep+4,
		ep+5);

	if (readCount < 6)
	{
		return -1;
	}

	if	(ep[0] < 100)
		ep[0] += ep[0] < 80 ? 2000 : 1900;

	t = epoch2time(ep, tsys);

	return 0;
}

GTime yds2time(
	const double*	yds,
	E_TimeSys		tsys)
{
	int year	= (int) yds[0];
	int doy		= (int) yds[1];
	double sec	=       yds[2];

	if	( year	< 1970
		||doy	< 1
		||doy	> 366)
	{
		return GTime::noTime();
	}

	int leapDays	= (year-1968-1) / 4	// -1968 = last leap year before 1970; -1 = year must end before applying leap-day
					- (year-1900-1) / 100
					+ (year-1600-1) / 400;

	int days = (year-1970)*365 + leapDays + doy - 1;

	PTime pTime = {};
	pTime.bigTime = days * S_IN_DAY + sec;	//.0 to prevent eventual overflow

	GTime time = pTime;
	switch (tsys)
	{
		case E_TimeSys::GPST:																				break;	// nothing to do for now
		case E_TimeSys::GST:																				break;	// nothing to do for now
		case E_TimeSys::QZSST:																				break;	// nothing to do for now
		case E_TimeSys::BDT:		{ time += GPS_SUB_UTC_2006;											}	break;
		case E_TimeSys::GLONASST:	{ time -= 10800;													}			// fallthough to account for leap seconds further
		case E_TimeSys::UTC:		{ UtcTime utcTime;	utcTime.bigTime = time.bigTime;	time = utcTime;	}	break;
		case E_TimeSys::TAI:		{ time += GPS_SUB_TAI;												}	break;
		default:
		{
			BOOST_LOG_TRIVIAL(error) << "Unsupported / Unknown time system: " << tsys._to_string() << ", use GPST by default." << "\n";
		}
	}

	return time;
}

void time2yds(
	GTime			time,
	double*			yds,
	E_TimeSys		tsys)
{
	GEpoch gEpoch;
	time2epoch(time, gEpoch.data(), tsys);

	//make new time with only the year of the input one,
	GEpoch gEpoch0(2000, 1, 1, 0, 0, 0);
	gEpoch0[0] = gEpoch[0];

	//subtract off the years
	Duration toy = (GTime)gEpoch - (GTime)gEpoch0;

	yds[0] = gEpoch0[0];
	yds[1] = toy.to_int() / 86400 + 1;	//(doy in bias SINEX (where yds is common) starts at 1)
	yds[2] = fmod(toy.to_double(), 86400);
}



UYds::operator GTime() const
{
	return yds2time(this->data(), E_TimeSys::UTC);
}


GTime epoch2time(
	const double*	ep,
	E_TimeSys		tsys)
{
	int year	= (int) ep[0];
	int mon		= (int) ep[1];
	int day		= (int) ep[2];
	int hour	= (int) ep[3];
	int min		= (int) ep[4];
	double sec	= 	    ep[5];

	if	(  year	< 1970
		|| mon	< 1
		|| mon	> 12)
	{
		return GTime::noTime();
	}

	const int dayOffsetFromMonth[] = {0,31,59,90,120,151,181,212,243,273,304,334};
	int dayOfYear = dayOffsetFromMonth[mon-1] + day;

	if	( (	mon >= 3)				//after feb29
		&&(	year % 4	== 0)		//every 4 years
		&&( year % 100	!= 0
		  ||year % 400	== 0))
	{
		dayOfYear++;
	}

	double secOfDay	= hour	* 60 * 60
					+ min	* 60
					+ sec;

	double yds[3];
	yds[0] = year;
	yds[1] = dayOfYear;
	yds[2] = secOfDay;
	GTime time = yds2time(yds, tsys);

	return time;
}

void time2epoch(
	GTime			time,
	double*			ep,
	E_TimeSys		tsys)
{
	switch (tsys)
	{
		case E_TimeSys::GPST:																				break;	// nothing to do for now
		case E_TimeSys::GST:																				break;	// nothing to do for now
		case E_TimeSys::QZSST:																				break;	// nothing to do for now
		case E_TimeSys::BDT:		{ time -= GPS_SUB_UTC_2006;											}	break;
		case E_TimeSys::GLONASST:	{ time += 10800;													}			// fallthough to account for leap seconds further
		case E_TimeSys::UTC:		{ UtcTime utcTime = time;	time.bigTime = utcTime.bigTime;			}	break;
		case E_TimeSys::TAI:		{ time -= GPS_SUB_TAI;												}	break;
		default:
		{
			BOOST_LOG_TRIVIAL(error) << "Unsupported / Unknown time system: " << tsys._to_string() << ", use GPST by default." << "\n";
		}
	}

	PTime pTime = time;

	const int mday[] =
	{
		/* # of days in a month */
		31,28,31,30,31,30,31,31,30,31,30,31,
		31,28,31,30,31,30,31,31,30,31,30,31,
		31,29,31,30,31,30,31,31,30,31,30,31,
		31,28,31,30,31,30,31,31,30,31,30,31
	};

	/* leap year if year % 4 == 0 in 1901-2099 */

	int		days	= (int) (pTime.bigTime / secondsInDayP);
	double	remSecs	= pTime.bigTime - (time_t) days * secondsInDay;

	int doy = days % (365*4+1);
	int mon;
	for (mon = 0; mon < 48; mon++)
	{
		if (doy >= mday[mon])
			doy -= mday[mon];
		else
			break;
	}
	ep[0]		= 1970
				+ days	/ 1461 * 4 		//1461 = 365.25 * 4
				+ mon	/ 12;
	ep[1]		= mon % 12			+ 1;
	ep[2]		= doy				+ 1;

	ep[3]		= (int) (remSecs / 3600);		remSecs -= ep[3]	* 3600;
	ep[4]		= (int) (remSecs / 60);			remSecs -= ep[4]	* 60;
	ep[5]		= 		(remSecs);
}


/* convert week and tow in gps time to GTime struct
* args   : int    week      I   week number in gps time
*          double sec       I   time of week in gps time (s)
* return : GTime struct
*/
GTime gpst2time(int week, double sec)
{
	GTime t = GPS_t0;

	if 	( sec <-1E9
		||sec > 1E9)
	{
		sec = 0;
	}
	t.bigTime	+= secondsInWeek *	week;
	t.bigTime	+= sec;

	return t;
}

GTime timeGet()
{
	auto posixUtcNow	= boost::posix_time::microsec_clock::universal_time();
	auto duration		= posixUtcNow - POSIX_GPS_t0;	// UtcTime lines up w/ GTime at GPS_t0

	UtcTime utcTime;
	utcTime.bigTime	= duration.total_microseconds() * 1e-6;

	return utcTime;
}

/* GPStime - UTC */
double leapSeconds(
		GTime time)
{
	if (nav.leaps >= 0)
		return nav.leaps;

	if (acsConfig.leap_seconds >= 0)
		return acsConfig.leap_seconds;

	auto it = leapSecondMap.lower_bound(time);
	if (it == leapSecondMap.end())
	{
		return 0;
	}

	auto& [leapTime, seconds] = *it;

	return seconds;
}


UtcTime gpst2utc(
	GTime time)
{
	long double leaps = leapSeconds(time);

	UtcTime utcTime;
	utcTime.bigTime	= time.bigTime - leaps;

	return utcTime;
}

GTime utc2gpst(UtcTime utcTime)
{
	GTime gTime;
	gTime.bigTime	= utcTime.bigTime;

	double leaps	= leapSeconds(gTime);
	leaps			= leapSeconds(gTime + leaps);

	gTime.bigTime += leaps;

	return gTime;
}

string GTime::to_string(
	int n)
const
{
	if	( cacheTime == bigTime
		&&cacheN	== n)
	{
		return cacheString;
	}

	GTime t = *this;

	if		(n < 0) 		n = 0;
	else if (n > 12)		n = 12;

	double exper = pow(10, n);

	long double val = t.bigTime * exper;
	val -= (long int) val;

	if (val > 0.5)
	{
		t.bigTime += 0.5 / exper;
	};

	GEpoch ep(t);

	char buff[64];
	snprintf(buff, sizeof(buff),"%04.0f-%02.0f-%02.0f %02.0f:%02.0f:%0*.*f",
			ep.year,
			ep.month,
			ep.day,
			ep.hour,
			ep.min,
			n <=0?2:n+3,
			n,
			ep.sec);

	cacheString = buff;
	cacheTime	= bigTime;
	cacheN		= n;

	return cacheString;
}

string GTime::to_ISOstring(
	int n)
const
{
	string s = this->to_string(n);
	s[10] = 'T';
	return s;
}

double GTime::to_decYear() const
{
	UYds yds = *this;

	double year	= yds.year;
	double doy	= yds.doy;
	double sod	= yds.sod;

	// Determine if the year is a leap year
	bool isLeapYear = (static_cast<int>(year) % 4 == 0 && static_cast<int>(year) % 100 != 0) || (static_cast<int>(year) % 400 == 0);
	int totalDaysInYear = isLeapYear ? 366 : 365;

	return year + (doy + sod / secondsInDay) / totalDaysInYear;
}

GTime::operator GEpoch() const
{
	GEpoch gEpoch;
	time2epoch(*this, gEpoch.data());

	return gEpoch;
}

GTime GTime::floorTime(
	double	period) const
{
	GTime roundedTime = *this;

	//need separate functions for fractional / whole seconds
	//ignore fractions greater than one
	if (period < 1)
	{
		double fractionalSeconds = bigTime - (long int) bigTime;

		int wholePeriods = fractionalSeconds / period;

		fractionalSeconds = wholePeriods * period;

		roundedTime.bigTime = fractionalSeconds + (long int) bigTime;
	}
	else
	{
		//round to nearest chunk by integer arithmetic
		roundedTime.bigTime = ((long int) (roundedTime.bigTime / period)) * period;
	}

	return roundedTime;
}

/** Returns GTime in "dd-mmm-yyyy hh:mm:ss" format
*/
string GTime::gregString()
{
	GEpoch epoch = *this;
	char buffer[25];
	snprintf(buffer, 25, "%02d-%3s-%04d %02d:%02d:%02d",
								(int)epoch.day,
		E_Month::_from_integral((int)epoch.month)._to_string(),
								(int)epoch.year,
								(int)epoch.hour,
								(int)epoch.min,
								(int)epoch.sec);
	return buffer;
}



/** Use a time of modulus and recent time to calculate the new time
 */
GTime nearestTime(
	GTime	referenceEpoch,	//
	double	tom,
	GTime	nearTime,
	int		mod)
{
	time_t	seconds		= (time_t) (nearTime.bigTime - referenceEpoch.bigTime);
	int		nearMod		= seconds / mod;
	int		nearTom		= seconds % mod;

	int		deltaTom	= tom - nearTom;

	int		newMod		= nearMod;

	if		(deltaTom > + mod / 2)		{	newMod--;	}
	else if	(deltaTom < - mod / 2)		{	newMod++;	}

	GTime newTime	= referenceEpoch
					+ newMod * mod
					+ tom;

	return newTime;
}


GTime::operator MjDateTT() const
{
	long double thisDate = *this;
	long double thenDate = j2000TT;

	long double deltaDate = thisDate - thenDate;
	deltaDate /= secondsInDayP;

	MjDateTT mjd;
	mjd.val	= MJD_j2000
			+ deltaDate;

	return mjd;
}

GTime::GTime(
	MjDateTT mjdTT)
{
	long double deltaDays = mjdTT.val	- MJD_j2000;

	bigTime	= j2000TT.bigTime
			+ deltaDays * secondsInDayP;
}

GTime::GTime(
	MjDateUtc mjdUtc)
{
	long double deltaDays = mjdUtc.val	- MJD_j2000;

	bigTime	= j2000Utc.bigTime
			+ deltaDays * secondsInDayP;

	long double leaps = leapSeconds(*this);

	bigTime += leaps;
}

MjDateUt1::MjDateUt1(
	GTime	time,
	double	ut1_utc)
{
	MjDateUtc mjdUtc = time;

	val	= mjdUtc.val
		+ ut1_utc / secondsInDayP;
}

MjDateUtc::MjDateUtc(
	GTime	time)
{
	long double thisDate = time;
	long double thenDate = j2000Utc;

	long double deltaDate = thisDate - thenDate;
	deltaDate /= secondsInDayP;

	long double leaps = leapSeconds(time);

	val	= MJD_j2000
		+ deltaDate
		- leaps / secondsInDayP;
}

GTime::GTime(GWeek	gpsWeek,	GTow	tow)		{	*this	= GPS_t0	+ gpsWeek * secondsInWeek	+ tow;	}
GTime::GTime(BWeek	bdsWeek,	BTow	tow)		{	*this	= BDS_t0	+ bdsWeek * secondsInWeek	+ tow;	}

GEpoch	::operator GTime()		const{	return epoch2time(this->data());	}
UtcTime	::operator GTime()		const{	return utc2gpst(*this);				}
GTime	::operator UtcTime()	const{	return gpst2utc(*this);				}


GTime::GTime(GTow	tow,	GTime	nearTime)	{	*this = nearestTime(GPS_t0, tow, nearTime, secondsInWeek);	}
GTime::GTime(BTow	tow,	GTime	nearTime)	{	*this = nearestTime(BDS_t0, tow, nearTime, secondsInWeek);	}

GTime::GTime(RTod	tod,	GTime	nearTime)
{
	RTod nearTod = nearTime;

	double delta = tod - nearTod;

	while (delta > +secondsInDay / 2)			delta -= secondsInDay;
	while (delta < -secondsInDay / 2)			delta += secondsInDay;

	*this = nearTime + delta;
}


GTime::operator long double()	const{																													return bigTime;	}

GTime::operator GWeek()			const{	Duration seconds = *this - GPS_t0;		GWeek	gWeek	= seconds.to_int() / secondsInWeek;						return gWeek;	}
GTime::operator BWeek()			const{	Duration seconds = *this - BDS_t0;		BWeek	bWeek	= seconds.to_int() / secondsInWeek;						return bWeek;	}
GTime::operator GTow()			const{	Duration seconds = *this - GPS_t0;		GTow	gTow	= fmod(seconds.to_double(), 					(double)secondsInWeek);		return gTow;	}
GTime::operator BTow()			const{	Duration seconds = *this - BDS_t0;		BTow	bTow	= fmod(seconds.to_double(), 					(double)secondsInWeek);		return bTow;	}
GTime::operator RTod()			const{	Duration seconds = *this - GLO_t0;		RTod	rTod	= fmod(seconds.to_double()-leapSeconds(*this),	(double)secondsInDay);		return rTod;	}

PTime::operator GTime() 		const{	GTime gTime;	gTime.bigTime	= bigTime - GPS_t0_sub_POSIX_t0;												return gTime;}
GTime::operator PTime() 		const{	PTime pTime;	pTime.bigTime	= bigTime + GPS_t0_sub_POSIX_t0;												return pTime;}

GTime::operator string()		const{	return to_string();	}

/** Returns (posix) for current epoch
 */
boost::posix_time::ptime currentLogptime()
{
	PTime logtime = tsync.floorTime(acsConfig.rotate_period);

	boost::posix_time::ptime	logptime	= boost::posix_time::from_time_t((time_t)logtime.bigTime);

	if ((GTime)logtime == GTime::noTime())
	{
		logptime = boost::posix_time::not_a_date_time;
	}
	return logptime;
}
