
#ifndef ___COMMON_HPP__
#define ___COMMON_HPP__

#include <memory>

/* constants/macros ----------------------------------------------------------*/
#define SQR(x)      ((x)*(x))
#define POW2(x)     ((x)*(x))
#define POW4(x)     ((x)*(x)*(x)*(x))
#define SQRT(x)     ((x)<=0.0?0.0:sqrt(x))
#define ROUND(x)    (int)floor((x)+0.5)
#define MAX(x,y)    ((x)>(y)?(x):(y))
#define MIN(x,y)    ((x)<(y)?(x):(y))
#define SWAP(x,y)   do {double tmp_; tmp_=x; x=y; y=tmp_;} while (0)
#define SGN(x)      ((x)<=0.0?-1.0:1.0)

#include "eigenIncluder.hpp"
#include "gTime.hpp"
#include "enums.h"


#ifdef WIN32
#define FILEPATHSEP '\\'
#else
#define FILEPATHSEP '/'
#endif


/* coordinates transformation ------------------------------------------------*/

void ecef2enu(const double *pos, const double *r, double *e);
void enu2ecef(const double *pos, const double *e, double *r);
void xyz2enu (const double *pos, double *E);
void eci2ecef(GTime tutc, const double *erpv, double *U, double *gmst);
void ecef2pos(const double *r, double *pos);
void ecef2pos(Vector3d& r, double *pos);
void pos2ecef(const double *pos, double *r);

double geodist(Vector3d& rs, Vector3d& rr, Vector3d& e);


//forward declarations
struct prcopt_t;
struct erp_t;
struct StationOptions;
struct Obs;
struct SatSys;

/* satellites, systems, codes functions --------------------------------------*/

double satazel(const double *pos, const double *e, double *azel);
double geodist(const double *rs, const double *rr, double *e);

unsigned int getbitu	(const unsigned char *buff, int  pos, int len);
int          getbits	(const unsigned char *buff, int  pos, int len);
unsigned int getbituInc	(const unsigned char *buff, int& pos, int len);
int          getbitsInc	(const unsigned char *buff, int& pos, int len);
int setbitsInc(unsigned char *buff,int pos,int len,const int var);
int setbituInc(unsigned char *buff,int pos,int len,const unsigned int var);

unsigned int crc24q (const unsigned char *buff, int len);

double ymdhms2jd(const double time[6]);

/* positioning models --------------------------------------------------------*/
void dops(int ns, const double *azel, double elmin, double *dop);

int  readblq(string file, const char *sta, double *odisp);
int  readerp(string file, erp_t *erp);
int  geterp (const erp_t *erp, GTime time, double *val);


int satexclude(SatSys& sat, int svh);


extern int		epoch;
extern GTime	tsync;


#endif
