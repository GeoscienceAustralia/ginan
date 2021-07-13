
#ifndef VMF3_H
#define VMF3_H


#define     PIGA        3.1415926535897932  /* PI */
#define     D2RGA       (PIGA/180.0)        /* degree to radian */
#define     R2DGA       (180.0/PIGA)        /* radian to degree */

#ifndef     NGRID1
#define     NGRID       2592                /* grid number (5 degree) */
#define     NLAT        36                  /**/
#define     NLON        72                  /**/
#else
#define     NGRID       64800               /* grid number (1 degree) */
#define     NLAT        180                 /**/
#define     NLON        360                 /**/
#endif


struct vmf3grid_t
{
	/* vmf3 grid file contents */
	double lat	[NGRID];          	/* lat grid (degree) */
	double lon	[NGRID];          	/* lon grid (degree) */
	double ah	[NGRID];           	/* hydrostatic mapping coefficient */
	double aw	[NGRID];           	/* wet mapping coefficient */
	double zhd	[NGRID];          	/* zenith hydrastatic delay (m) */
	double zwd	[NGRID];			/* zenith wet delay */
	double resol;					/* grid resolution */
	double mjd;						/* modified julian date */
} ;

struct vmf3_t
{
	int m;                      /* NWM changing indicator */
	vmf3grid_t vmf3g[3];        /* vmf3 grid file info */
};

int		readorog(string file, double *orog);
void	readvmf3grids(const char *dir, vmf3_t *vmf3, const double jd);
int		udgrid(const char *dir, vmf3grid_t *vmf3g, const double jd, double mjd0[3], int& m);
int		tropvmf3(const vmf3grid_t *vmf3g, const double *orog,   const double jd, const double lat,  const double lon, const double hgt, const double zd,  const int mi, double *zhd, double *zwd, double mf[2]);
int		tropvmf3full(vmf3grid_t *vmf3g, const double *orog,  const double jd, const double lat, const double lon,     const double hgt, const double zd,   double mjd0[3], double delay[2], double mf[2]);

#endif
