
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <iostream>

using std::ostream;

#include "constants.h"
#include "gTime.hpp"

ostream& operator <<(ostream& stream, const GTime& time)
{
	stream << time.time << ": " << time.sec;
	return stream;
}

/* string to number ------------------------------------------------------------
* convert substring in string to number
* args   : char   *s        I   string ("... nnn.nnn ...")
*          int    i,n       I   substring position and width
* return : converted number (0.0:error)
*-----------------------------------------------------------------------------*/
double str2num(const char *s, int i, int n)
{
	double value;
	char str[256],*p=str;

	if (i<0||(int)strlen(s)<i||(int)sizeof(str)-1<n) return 0.0;
	for (s+=i;*s&&--n>=0;s++) *p++=*s=='d'||*s=='D'?'E':*s; *p='\0';
	return sscanf(str,"%lf",&value)==1?value:0.0;
}

/* string to time --------------------------------------------------------------
* convert substring in string to GTime struct
* args   : char   *s        I   string ("... yyyy mm dd hh mm ss ...")
*          int    i,n       I   substring position and width
*          GTime *t       O   GTime struct
* return : status (0:ok,0>:error)
*-----------------------------------------------------------------------------*/
int str2time(
	const char*	s, 
	int			i, 
	int			n, 
	GTime&		t)
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
	
	t = epoch2time(ep);
	
	return 0;
}

/* convert calendar day/time to time -------------------------------------------
* convert calendar day/time to GTime struct
* args   : double *ep       I   day/time {year,month,day,hour,min,sec}
* return : GTime struct
* notes  : proper in 1970-2037 or 1970-2099 (64bit time_t)
*-----------------------------------------------------------------------------*/
GTime epoch2time(const double *ep)
{
	const int doy[]={1,32,60,91,121,152,182,213,244,274,305,335};
	GTime time={0};
	int days,sec,year=(int)ep[0],mon=(int)ep[1],day=(int)ep[2];

	if (year<1970||2099<year||mon<1||12<mon) return time;

	/* leap year if year%4==0 in 1901-2099 */
	days=(year-1970)*365+(year-1969)/4+doy[mon-1]+day-2+(year%4==0&&mon>=3?1:0);
	sec=(int)floor(ep[5]);
	time.time=(time_t)days*86400+(int)ep[3]*3600+(int)ep[4]*60+sec;
	time.sec=ep[5]-sec;
	return time;
}

GTime yds2time(const int* yds)
{
	int year	= yds[0];
	int doy		= yds[1];
	int sec		= yds[2];
	
	if (year<1970||2099<year||doy<1||356<doy) 
		return {};

	/* leap year if year%4==0 in 1901-2099 */
	int days=(year-1970)*365+(year-1969)/4 + doy - 2;	//todo aaron the 2 needed?
	
	GTime time = {};
	time.time = (time_t)days*86400+sec;
	
	return time;
}

/* time to calendar day/time ---------------------------------------------------
* convert GTime struct to calendar day/time
* args   : GTime t        I   GTime struct
*          double *ep       O   day/time {year,month,day,hour,min,sec}
* return : none
* notes  : proper in 1970-2037 or 1970-2099 (64bit time_t)
*-----------------------------------------------------------------------------*/
void time2epoch(GTime t, double* ep)
{
	const int mday[] =
	{
		/* # of days in a month */
		31,28,31,30,31,30,31,31,30,31,30,31,
		31,28,31,30,31,30,31,31,30,31,30,31,
		31,29,31,30,31,30,31,31,30,31,30,31,
		31,28,31,30,31,30,31,31,30,31,30,31
	};

	/* leap year if year % 4 == 0 in 1901-2099 */

	int days	= (int) (t.time / 86400);
	int sec		= (int) (t.time - (time_t) days * 86400);

	int doy = days % (365*4+1);
	int mon;
	for (mon = 0; mon < 48; mon++)
	{
		if (doy >= mday[mon])
			doy -= mday[mon];
		else
			break;
	}
	ep[0] = 1970 + days / 1461 * 4 + mon / 12;
	ep[1] = mon % 12 + 1;
	ep[2] = doy + 1;
	ep[3] = sec / 3600;
	ep[4] = sec % 3600 / 60;
	ep[5] = sec % 60 + t.sec;
}

/* epoch to year doy sod converter----------------------------------------------
* args    : double 	*ep		I day/time {year, mon, day, hour, min,sec}
*		  : int		*yds	O year, doy, sod
* return  : none
*------------------------------------------------------------------------------*/
void epoch2yds(double *ep, int *yds)
{
	const int mday[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

	int year	= ep[0];
	int month	= ep[1];
	int dom		= ep[2];
	int hour	= ep[3];
	int minute	= ep[4];
	int second	= ep[5];

	int doy = dom;

	//add days for all previous months
	for (int i = 1; i <= 12; i++)
	{
		if (month > i)
		{
			doy += mday[i];
		}
	}

	/* all years 1900-2099 divisible by 4 are leap years */
	if	( month		> 2
		&&year % 4	== 0)
	{
		doy++;
	}


	yds[0]	= year;
	yds[1]	= doy;
	yds[2]	= hour		* 60 * 60
			+ minute	* 60
			+ second;
}

/* gps time to time ------------------------------------------------------------
* convert week and tow in gps time to GTime struct
* args   : int    week      I   week number in gps time
*          double sec       I   time of week in gps time (s)
* return : GTime struct
*-----------------------------------------------------------------------------*/
GTime gpst2time(int week, double sec)
{
	GTime t = epoch2time(gpst0);

	if 	( sec <-1E9
		||sec > 1E9)
	{
		sec = 0;
	}
	t.time += 60*60*24*7*week + (int)sec;
	t.sec = sec - (int)sec;
	return t;
}

/* time to gps time ------------------------------------------------------------
* convert GTime struct to week and tow in gps time
* args   : GTime t        I   GTime struct
*          int    *week     IO  week number in gps time (NULL: no output)
* return : time of week in gps time (s)
*-----------------------------------------------------------------------------*/
double time2gpst(GTime t, int *week)
{
	GTime t0 = epoch2time(gpst0);

	time_t sec = t.time - t0.time;
	int w=(int)(sec/(86400*7));

	if (week) *week=w;
	return (double)(sec-w*86400*7)+t.sec;
}

/* galileo system time to time -------------------------------------------------
* convert week and tow in galileo system time (gst) to GTime struct
* args   : int    week      I   week number in gst
*          double sec       I   time of week in gst (s)
* return : GTime struct
*-----------------------------------------------------------------------------*/
GTime gst2time(int week, double sec)
{
	GTime t=epoch2time(gst0);

	if (sec<-1E9||1E9<sec) sec=0.0;
	t.time+=86400*7*week+(int)sec;
	t.sec=sec-(int)sec;
	return t;
}

/* time to galileo system time -------------------------------------------------
* convert GTime struct to week and tow in galileo system time (gst)
* args   : GTime t        I   GTime struct
*          int    *week     IO  week number in gst (NULL: no output)
* return : time of week in gst (s)
*-----------------------------------------------------------------------------*/
double time2gst(GTime t, int *week)
{
	GTime t0=epoch2time(gst0);
	time_t sec=t.time-t0.time;
	int w=(int)(sec/(86400*7));

	if (week) *week=w;
	return (double)(sec-w*86400*7)+t.sec;
}

/* beidou time (bdt) to time ---------------------------------------------------
* convert week and tow in beidou time (bdt) to GTime struct
* args   : int    week      I   week number in bdt
*          double sec       I   time of week in bdt (s)
* return : GTime struct
*-----------------------------------------------------------------------------*/
GTime bdt2time(int week, double sec)
{
	GTime t=epoch2time(bdt0);

	if (sec<-1E9||1E9<sec) sec=0.0;
	t.time+=86400*7*week+(int)sec;
	t.sec=sec-(int)sec;
	return t;
}

/* time to beidouo time (bdt) --------------------------------------------------
* convert GTime struct to week and tow in beidou time (bdt)
* args   : GTime t        I   GTime struct
*          int    *week     IO  week number in bdt (NULL: no output)
* return : time of week in bdt (s)
*-----------------------------------------------------------------------------*/
double time2bdt(GTime t, int *week)
{
	GTime t0=epoch2time(bdt0);
	time_t sec=t.time-t0.time;
	int w=(int)(sec/(86400*7));

	if (week) *week=w;
	return (double)(sec-w*86400*7)+t.sec;
}

/* add time --------------------------------------------------------------------
* add time to GTime struct
* args   : GTime t        I   GTime struct
*          double sec       I   time to add (s)
* return : GTime struct (t+sec)
*-----------------------------------------------------------------------------*/
GTime timeadd(GTime t, double sec)
{
	double tt;

	t.sec+=sec; tt=floor(t.sec); t.time+=(int)tt; t.sec-=tt;
	return t;
}

/* time difference -------------------------------------------------------------
* difference between GTime structs
* args   : GTime t1,t2    I   GTime structs
* return : time difference (t1-t2) (s)
*-----------------------------------------------------------------------------*/
double timediff(GTime t1, GTime t2)
{
	return difftime(t1.time,t2.time)+t1.sec-t2.sec;
}

/* get current time in utc -----------------------------------------------------
* get current time in utc
* args   : none
* return : current time in utc
*-----------------------------------------------------------------------------*/

GTime timeget(void)
{
	double ep[6]={0};
#ifdef WIN32
	SYSTEMTIME ts;

	GetSystemTime(&ts); /* utc */
	ep[0]=ts.wYear; ep[1]=ts.wMonth;  ep[2]=ts.wDay;
	ep[3]=ts.wHour; ep[4]=ts.wMinute; ep[5]=ts.wSecond+ts.wMilliseconds*1E-3;
#else
	struct timeval tv;
	struct tm *tt;

	if	(!gettimeofday(&tv,NULL)
		&&(tt=gmtime(&tv.tv_sec)))
	{
		ep[0]=tt->tm_year+1900;
		ep[1]=tt->tm_mon+1;
		ep[2]=tt->tm_mday;
		ep[3]=tt->tm_hour;
		ep[4]=tt->tm_min;
		ep[5]=tt->tm_sec+tv.tv_usec*1E-6;
	}
#endif
	return epoch2time(ep);
}


/* gpstime to utc --------------------------------------------------------------
* convert gpstime to utc considering leap seconds
* args   : GTime t        I   time expressed in gpstime
* return : time expressed in utc
* notes  : ignore slight time offset under 100 ns
*-----------------------------------------------------------------------------*/
GTime gpst2utc(GTime t)
{
	GTime tu;
	int i;

	for (i=0;leaps[i][0]>0;i++) {
		tu=timeadd(t,leaps[i][6]);
		if (timediff(tu,epoch2time(leaps[i]))>=0.0) return tu;
	}
	return t;
}

/* utc to gpstime --------------------------------------------------------------
* convert utc to gpstime considering leap seconds
* args   : GTime t        I   time expressed in utc
* return : time expressed in gpstime
* notes  : ignore slight time offset under 100 ns
*-----------------------------------------------------------------------------*/
GTime utc2gpst(GTime t)
{
	int i;

	for (i=0;leaps[i][0]>0;i++) {
		if (timediff(t,epoch2time(leaps[i]))>=0.0) return timeadd(t,-leaps[i][6]);
	}
	return t;
}

/* gpstime to bdt --------------------------------------------------------------
* convert gpstime to bdt (beidou navigation satellite system time)
* args   : GTime t        I   time expressed in gpstime
* return : time expressed in bdt
* notes  : ref [8] 3.3, 2006/1/1 00:00 BDT = 2006/1/1 00:00 UTC
*          no leap seconds in BDT
*          ignore slight time offset under 100 ns
*-----------------------------------------------------------------------------*/
GTime gpst2bdt(GTime t)
{
	return timeadd(t,-14.0);
}

/* bdt to gpstime --------------------------------------------------------------
* convert bdt (beidou navigation satellite system time) to gpstime
* args   : GTime t        I   time expressed in bdt
* return : time expressed in gpstime
* notes  : see gpst2bdt()
*-----------------------------------------------------------------------------*/
GTime bdt2gpst(GTime t)
{
	return timeadd(t,14.0);
}

/* time to day and sec -------------------------------------------------------*/
double time2sec(GTime time, GTime* day)
{
	double ep[6],sec;
	time2epoch(time,ep);
	sec=ep[3]*3600.0+ep[4]*60.0+ep[5];
	ep[3]=ep[4]=ep[5]=0.0;
	*day=epoch2time(ep);
	return sec;
}

/* utc to gmst -----------------------------------------------------------------
* convert utc to gmst (Greenwich mean sidereal time)
* args   : GTime t        I   time expressed in utc
*          double ut1_utc   I   UT1-UTC (s)
* return : gmst (rad)
*-----------------------------------------------------------------------------*/
double utc2gmst(GTime t, double ut1_utc)
{
	const double ep2000[]={2000,1,1,12,0,0};
	GTime tut,tut0;
	double ut,t1,t2,t3,gmst0,gmst;

	tut=timeadd(t,ut1_utc);
	ut=time2sec(tut,&tut0);
	t1=timediff(tut0,epoch2time(ep2000))/86400.0/36525.0;
	t2=t1*t1; t3=t2*t1;
	gmst0=24110.54841+8640184.812866*t1+0.093104*t2-6.2E-6*t3;
	gmst=gmst0+1.002737909350795*ut;

	return fmod(gmst,86400.0)*PI/43200.0; /* 0 <= gmst <= 2*PI */
}

/* time to string --------------------------------------------------------------
* convert GTime struct to string
* args   : GTime t        I   GTime struct
*          char   *s        O   string ("yyyy/mm/dd hh:mm:ss.ssss")
*          int    n         I   number of decimals
* return : none
*-----------------------------------------------------------------------------*/
void time2str(GTime t, char *s, int n)
{
	double ep[6];

	if (n<0) n=0; else if (n>12) n=12;
	if (1.0-t.sec<0.5/pow(10.0,n)) {t.time++; t.sec=0.0;};
	time2epoch(t,ep);
	sprintf(s,"%04.0f/%02.0f/%02.0f %02.0f:%02.0f:%0*.*f",ep[0],ep[1],ep[2],
			ep[3],ep[4],n<=0?2:n+3,n<=0?0:n,ep[5]);
}

/* time to day of year ---------------------------------------------------------
* convert time to day of year
* args   : GTime t        I   GTime struct
* return : day of year (days)
*-----------------------------------------------------------------------------*/
double time2doy(GTime t)
{
	double ep[6];

	time2epoch(t,ep);
	ep[1]=ep[2]=1.0; ep[3]=ep[4]=ep[5]=0.0;
	return timediff(t,epoch2time(ep))/86400.0+1.0;
}

/* adjust gps week number ------------------------------------------------------
* adjust gps week number using cpu time
* args   : int   week       I   not-adjusted gps week number
* return : adjusted gps week number
*-----------------------------------------------------------------------------*/
int adjgpsweek(int week)
{
	int w;
	(void)time2gpst(utc2gpst(timeget()),&w);
	if (w<1560) w=1560; /* use 2009/12/1 if time is earlier than 2009/12/1 */
	return week+(w-week+512)/1024*1024;
}

/* get tick time ---------------------------------------------------------------
* get current tick in ms
* args   : none
* return : current tick in ms
*-----------------------------------------------------------------------------*/
unsigned int tickget(void)
{
#ifdef WIN32
	return (unsigned int)timeGetTime();
#else
#if (0)
	struct timespec tp={0};
#endif
	struct timeval  tv={0};

#ifdef CLOCK_MONOTONIC_RAW
	/* linux kernel > 2.6.28 */
#if (1)
	gettimeofday(&tv,NULL);
	return tv.tv_sec*1000u+tv.tv_usec/1000u;
#endif
#if (0)
	if (!clock_gettime(CLOCK_MONOTONIC_RAW,&tp)) {
		return tp.tv_sec*1000u+tp.tv_nsec/1000000u;
	}
#endif
#else
	gettimeofday(&tv,NULL);
	return tv.tv_sec*1000u+tv.tv_usec/1000u;
#endif
#endif /* WIN32 */
}

/* sleep ms --------------------------------------------------------------------
* sleep ms
* args   : int   ms         I   miliseconds to sleep (<0:no sleep)
* return : none
*-----------------------------------------------------------------------------*/
void sleepms(int ms)
{
#ifdef WIN32
	if (ms<5) Sleep(1); else Sleep(ms);
#else
	struct timespec ts;
	if (ms<=0) return;
	ts.tv_sec=(time_t)(ms/1000);
	ts.tv_nsec=(long)(ms%1000*1000000);
	nanosleep(&ts,NULL);
#endif
}

/* set precision ---------------------------------------------------------------
*
* args     :       const double n          I       source number
*
* return   :       formatted number
*----------------------------------------------------------------------------*/
double setdigits(const double n)
{
	char str[128];
	double m;

	sprintf(str,"%.4f",n);
	sscanf(str,"%lf",&m);

	return m;
}
/* Julian day to YMDHMS --------------------------------------------------------
*
* args     :       const double jd         I       Julian day
*                  double ep[6]            O       Y,M,D,H,M,S
*
* return   :       none
*
* reference:       [3]
*----------------------------------------------------------------------------*/
extern void jd2ymdhms(const double jd, double *ep)
{
	int b,c,d,e;
	double t1,t2,i1;

	modf(jd+0.5,&i1);
	b=i1+1537;
	modf((b-122.1)/365.25,&i1);
	c=i1;
	modf(365.25*c,&i1);
	d=i1;
	modf((b-d)/30.6001,&i1);
	e=i1;

	t1=(jd+0.5-floor(jd+0.5))*24*3600;

	t2=modf(t1/3600,&ep[3]);            /* hour */
	ep[2]=b-d-floor(30.6001*e);         /* day */
	ep[1]=e-1-12*floor(e/14);           /* month */
	ep[0]=c-4715-floor((7+ep[1])/10);   /* year */
	t2=modf(t2*60,&ep[4]);              /* minute */
	ep[5]=t2*60;                        /* second */
	ep[5]=setdigits(ep[5]);

	if (setdigits(ep[5])==60)
	{
		ep[5]=0;
		ep[4]+=1;
		if (ep[4]==60)
		{
			ep[4]=0;
			ep[3]+=1;
		}
	}

	return;
}
