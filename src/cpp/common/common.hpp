
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
#include "erp.hpp"
#include "enums.h"


struct Average
{
	double	mean	= 0;
	double	var		= 0;
};


void lowPassFilter(
	Average&	avg,
	double		meas,
	double		procNoise,
	double		measVar = 1);

/* coordinates transformation ------------------------------------------------*/

void ecef2enu(const double *pos, const double *r, double *e);
void enu2ecef(const double *pos, const double *e, double *r);
void xyz2enu (const double *pos, double *E);


//forward declarations
struct prcopt_t;
struct StationOptions;
struct Obs;
struct SatSys;
struct ERPValues;
struct IERS;

void eci2ecef(
	const GTime		tutc,	
	ERPValues&		erpv,	
	Matrix3d&		U,		
	double*			gmst	= nullptr,		
	Matrix3d*		dU		= nullptr);

void eci2ecef_sofa(
	const double	mjdUTC,
	IERS&			iers,	
	Matrix3d&		U,		
	Matrix3d&		dU);

void eci2ecef_sofa(
	const double	mjdUTC,		
	IERS&			iers,	
	Vector3d&		rSat_eci,	
	Vector3d&		vSat_eci,	
	Vector3d&		rSat_ecef,	
	Vector3d&		vSat_ecef);	

void ecef2eci_sofa(
	const double	mjdUTC,		
	IERS&			iers,	
	Vector3d&		rSat_ecef,	
	Vector3d&		vSat_ecef,	
	Vector3d&		rSat_eci,	
	Vector3d&		vSat_eci);	



Matrix3d R_x(
	double    Angle);

Matrix3d R_y(
	double    Angle);

Matrix3d R_z(
	double    Angle);


void ecef2pos(const double *r, double *pos);
void ecef2pos(Vector3d& r, double *pos);
void pos2ecef(const double *pos, Vector3d& r);

double geodist(Vector3d& rs, Vector3d& rr, Vector3d& e);

double sagnac(
	Vector3d& rSource,
	Vector3d& rDest);


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

/* positioning models --------------------------------------------------------*/
void dops(int ns, const double *azel, double elmin, double *dop);

int  readblq(string file, const char *sta, double *otlDisplacement);


int satexclude(SatSys& sat, E_Svh svh);

extern int		epoch;
extern GTime	tsync;

int sisaToSva(double sisa);
double svaToSisa(int sva);
double svaToUra(int sva);
void replaceTimes(
	string&						str,		///< String to replace macros within
	boost::posix_time::ptime	time_time);	///< Time to use for replacements

void updatenav(
	Obs&	obs);
#endif
