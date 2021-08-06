
#ifndef __GATIME_HPP__
#define __GATIME_HPP__

#include <iostream>
#include <time.h>
#include <string>

using std::ostream;
using std::string;

struct GTime;

void    time2str(GTime t, char *str, int n);

/** Time structure used throughout this software
*/
struct GTime
{
	time_t time	= 0;		///< Time (s) expressed by standard time_t
	double sec	= 0;		///< Fractions of second ( 0 < sec < 1 )

	/** Uninitialised time for comparisons
	*/
	static GTime noTime()
	{
		GTime nothing = {};
		return nothing;
	}

	string to_string(int n)
	{
		char buff[64];
		time2str(*this, buff, n);
		string result = buff;
		return result;
	}

	bool operator ==(const GTime &t2) const
	{
		if (this->time	!= t2.time)	return false;
		if (this->sec	!= t2.sec)	return false;
		else						return true;
	}

	bool operator !=(const GTime &t2) const
	{
		return !(*this == t2);
	}

//     bool operator <(const GTime &t2) const
// 	{
// 		if (this->time	< t2.time)	return true;
// 		if (this->time	> t2.time)	return false;
// 		if (this->sec	< t2.sec)	return true;
// 		else						return false;
// 	}

	bool operator <(const GTime &t2) const
	{
		if (this->time	< t2.time)	return true;
		if (this->time	> t2.time)	return false;
		if (this->sec	< t2.sec)	return true;
		else						return false;
	}

	bool operator >(const GTime &t2) const
	{
		if (this->time	> t2.time)	return true;
		if (this->time	< t2.time)	return false;
		if (this->sec	> t2.sec)	return true;
		else						return false;
	}

	friend ostream& operator<<(ostream& os, const GTime& time);

	GTime operator +(const double t) const
	{
		GTime time = *this;

		time.sec += t;

		double fracSeconds	= time.sec - (int) time.sec;
		double intSeconds	= time.sec - fracSeconds;

		time.time	+= intSeconds;
		time.sec	-= intSeconds;

		if (time.sec < 0)
		{
			time.sec	+= 1;
			time.time	-= 1;
		}
		return time;
	}
	
	GTime& operator+=(const double rhs)
	{
		*this = *this + rhs;
		return *this;
	}

	GTime operator -(const GTime t) const
	{
		GTime time = *this;

		time.time	-= t.time;
		time.sec	-= t.sec;
		
		if (time.sec < 0)
		{
			time.time	-= 1;
			time.sec	+= 1;
		}
		
		return time;
	}

	GTime operator -(const double t) const
	{
		GTime time = *this + (-t);
		return time;
	}
	
	GTime& operator++(int)
	{
		this->time++;
		return *this;
	}


	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		long int time_int = time;
		ar & time_int;
		time = time_int;
	}
};
	
GTime timeadd  (GTime t, double sec);
double  timediff (GTime t1, GTime t2);
GTime gpst2utc (GTime t);
GTime utc2gpst (GTime t);
GTime gpst2bdt (GTime t);
GTime bdt2gpst (GTime t);
GTime timeget  (void);
double  time2doy(GTime t);
double  utc2gmst (GTime t, double ut1_utc);
int adjgpsweek(int week);

/* time and string functions -------------------------------------------------*/
double  str2num(const char *s, int i, int n);
GTime epoch2time(const double *ep);
GTime yds2time	(const int* yds);
void    time2epoch(GTime t, double *ep);
void	epoch2yds(double *ep, int *yds);
GTime gpst2time(int week, double sec);
double  time2gpst(GTime t, int *week);
GTime gst2time(int week, double sec);
double  time2gst(GTime t, int *week);
GTime bdt2time(int week, double sec);
double  time2bdt(GTime t, int *week);
char    *time_str(GTime t, int n);

int str2time(
	const char*	s,
	int			i,
	int			n,
	GTime&		t);


void	jd2ymdhms(const double jd, double *ep);


extern unsigned int tickget(void);
extern void sleepms(int ms);

#endif
