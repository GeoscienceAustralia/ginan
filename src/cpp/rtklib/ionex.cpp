/*------------------------------------------------------------------------------
* ionex.c : ionex functions

* references:
*     [1] S.Schear, W.Gurtner and J.Feltens, IONEX: The IONosphere Map EXchange
*         Format Version 1, February 25, 1998
*     [2] S.Schaer, R.Markus, B.Gerhard and A.S.Timon, Daily Global Ionosphere
*         Maps based on GPS Carrier Phase Data Routinely producted by CODE
*         Analysis Center, Proceeding of the IGS Analysis Center Workshop, 1996
*-----------------------------------------------------------------------------*/
#include "corrections.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "common.hpp"


#define VAR_NOTEC   SQR(30.0)   /* variance of no tec */
#define MIN_EL      0.0         /* min elevation angle (rad) */
#define MIN_HGT     -1000.0     /* min user height (m) */

FILE* fdebug;

/* get index -----------------------------------------------------------------*/
int getindex(double value, const double* range)
{
	if (range[2] == 0)											return 0;
	if (range[1] > 0 && (value < range[0] || range[1] < value))	return -1;
	if (range[1] < 0 && (value < range[1] || range[0] < value))	return -1;

	return (int)floor((value - range[0]) / range[2] + 0.5);
}

/* get number of items -------------------------------------------------------*/
int nitem(const double* range)
{
	return getindex(range[1], range) + 1;
}

/* data index (i:lat,j:lon,k:hgt) --------------------------------------------*/
int dataindex(int i, int j, int k, const int* ndata)
{
	if	(  i < 0 || ndata[0] <= i
		|| j < 0 || ndata[1] <= j
		|| k < 0 || ndata[2] <= k)
		return -1;

	return i + ndata[0] * (j + ndata[1] * k);
}

/* read ionex dcb aux data ----------------------------------------------------*/
void readionexdcb(FILE* fp,  nav_t* navi)
{
	char buff[1024], id[32] = "", *label;

	if (fdebug)
		fprintf(fdebug, "readionexdcb:\n");

	for (auto& [key, satNav] : navi->satNavMap)
	{
		satNav.cBias_P1_P2 = 0;
	}

	while (fgets(buff, sizeof(buff), fp))
	{
		if (strlen(buff) < 60)
			continue;

		label = buff + 60;

		if (strstr(label, "PRN / BIAS / RMS") == label)
		{
			strncpy(id, buff + 3, 3); id[3] = '\0';

			SatSys Sat = SatSys(id);
			if (Sat)
			{
				navi->satNavMap[Sat].cBias_P1_P2 = str2num(buff, 6, 10) * CLIGHT * 1E-9;

				if (fdebug)
					fprintf(fdebug, "    %s   %.4f\n", id, navi->satNavMap[Sat].cBias_P1_P2);
			}
			else
			{
				if (fdebug)
					fprintf(fdebug, "ionex invalid satellite: %s\n", id);

				continue;
			}

			//dcb[sat-1]=str2num(buff, 6,10);
			//rms[sat-1]=str2num(buff,16,10);
		}
		else if (strstr(label, "END OF AUX DATA") == label)
			break;
	}
}

/* read ionex header ---------------------------------------------------------*/
double readionexh(FILE* fp, double* lats, double* lons, double* hgts, double& rb, double& nexp,  nav_t* navi)
{
	double ver = 0;
	char buff[1024], *label;

	if (fdebug)
		fprintf(fdebug, "readionexh:\n");

	while (fgets(buff, sizeof(buff), fp))
	{
		if (strlen(buff) < 60)
			continue;

		label = buff + 60;

		if (strstr(label, "IONEX VERSION / TYPE") == label)
		{
			if (buff[20] == 'I') ver = str2num(buff, 0, 8);

			if (fdebug)
				fprintf(fdebug, " ver= %.1f \n", ver);
		}
		else if (strstr(label, "BASE RADIUS") == label)
		{
			rb = str2num(buff, 0, 8);

			if (fdebug)
				fprintf(fdebug, " rad= %.2f \n", rb);
		}
		else if (strstr(label, "HGT1 / HGT2 / DHGT") == label)
		{
			hgts[0] = str2num(buff, 2, 6);
			hgts[1] = str2num(buff, 8, 6);
			hgts[2] = str2num(buff, 14, 6);

			if (fdebug)
				fprintf(fdebug, " heights= %.3f %.3f %.3f \n", hgts[0], hgts[1], hgts[2]);
		}
		else if (strstr(label, "LAT1 / LAT2 / DLAT") == label)
		{
			lats[0] = str2num(buff, 2, 6);
			lats[1] = str2num(buff, 8, 6);
			lats[2] = str2num(buff, 14, 6);

			if (fdebug)
				fprintf(fdebug, " lats= %.3f %.3f %.3f \n", lats[0], lats[1], lats[2]);
		}
		else if (strstr(label, "LON1 / LON2 / DLON") == label)
		{
			lons[0] = str2num(buff, 2, 6);
			lons[1] = str2num(buff, 8, 6);
			lons[2] = str2num(buff, 14, 6);

			if (fdebug)
				fprintf(fdebug, " lons= %.3f %.3f %.3f \n", lons[0], lons[1], lons[2]);
		}
		else if (strstr(label, "EXPONENT") == label)
		{
			nexp = str2num(buff, 0, 6);
		}
		else if (strstr(label, "START OF AUX DATA") == label &&
				strstr(buff, "DIFFERENTIAL CODE BIASES"))
		{
			readionexdcb(fp, navi);
		}
		else if (strstr(label, "END OF HEADER") == label)
		{
			if (fdebug)
				fprintf(fdebug, "\n");

			return ver;
		}
	}

	fprintf(fdebug, "\n");

	return 0;
}

/* read ionex body -----------------------------------------------------------*/
int readionexb(FILE* fp, const double* lats, const double* lons, const double* hgts, double rb, double nexp, nav_t* navi)
{
	GTime time = {0};
	double lat, lon[3], hgt, x;
	int i, j, k, n, m, index, type = 0;
	char buff[1024], *label = buff + 60;

	if (fdebug)
		fprintf(fdebug, "readionexb:\n");

	while (fgets(buff, sizeof(buff), fp))
	{
		if (strlen(buff) < 60)
			continue;

		if (strstr(label, "START OF TEC MAP") == label)
		{
			type = 1;
			time.time = 0;
		}
		else if (strstr(label, "END OF TEC MAP") == label)
		{
			if (fdebug)
				fprintf(fdebug, "%5ld data and %5ld rms entries for %s\n", navi->tecList[time.time].data.size(), navi->tecList[time.time].rms.size(), time.to_string(0).c_str());

			type = 0;
		}
		else if (strstr(label, "START OF RMS MAP") == label)
		{
			type = 2;
			time.time = 0;
		}
		else if (strstr(label, "END OF RMS MAP") == label)
		{
			if (fdebug)
				fprintf(fdebug, "%5ld data and %5ld rms entries for %s\n", navi->tecList[time.time].data.size(), navi->tecList[time.time].rms.size(), time.to_string(0).c_str());

			type = 0;
		}
		else if (strstr(label, "EPOCH OF CURRENT MAP") == label)
		{
			if (str2time(buff, 0, 36, time))
			{
				fprintf(fdebug, "ionex epoch invalid: %-36.36s\n", buff);
				continue;
			}

			if (type == 1)
			{
				navi->tecList[time.time].time = time;
				navi->tecList[time.time].ndata[0] = nitem(lats);
				navi->tecList[time.time].ndata[1] = nitem(lons);
				navi->tecList[time.time].ndata[2] = nitem(hgts);
				navi->tecList[time.time].rb = rb;

				for (i = 0; i < 3; i++)
				{
					navi->tecList[time.time].lats[i] = lats[i];
					navi->tecList[time.time].lons[i] = lons[i];
					navi->tecList[time.time].hgts[i] = hgts[i];
				}

				navi->tecList[time.time].data.clear();
				navi->tecList[time.time].rms.clear();
			}
		}
		else if (strstr(label, "LAT/LON1/LON2/DLON/H") == label && time.time && type)
		{
			lat		= str2num(buff, 2, 6);
			lon[0]	= str2num(buff, 8, 6);
			lon[1]	= str2num(buff, 14, 6);
			lon[2]	= str2num(buff, 20, 6);
			hgt		= str2num(buff, 26, 6);

			i = getindex(lat, lats);
			k = getindex(hgt, hgts);
			n = nitem(lon);

			for (m = 0; m < n; m++)
			{
				if (m % 16 == 0 && !fgets(buff, sizeof(buff), fp))
					break;

				j = getindex(lon[0] + lon[2] * m, lons);

				if ((index = dataindex(i, j, k, navi->tecList[time.time].ndata)) < 0)
					continue;

				if ((x = str2num(buff, m % 16 * 5, 5)) == 9999)
					continue;

				if (type == 1)
					navi->tecList[time.time].data[index] = x * pow(10, nexp);

				if (type == 2)
					navi->tecList[time.time].rms [index] = x * pow(10, nexp);
			}
		}
	}

	return 1;
}

/* read ionex tec grid file ----------------------------------------------------
* read ionex ionospheric tec grid file
* args   : char   *file       I   ionex tec grid file
*                                 (wind-card * is expanded)
*          nav_t  *nav        IO  navigation data
*                                 nav->nt, nav->ntmax and nav->tec are modified
*          int    opt         I   read option (1: no clear of tec data,0:clear)
* return : none
* notes  : see ref [1]
*-----------------------------------------------------------------------------*/
void readtec(string file, nav_t* navi, int opt, FILE* fpout)
{
	FILE* fp;
	double lats[3] = {};
	double lons[3] = {};
	double hgts[3] = {};

	if (fpout)
		fprintf(fpout, "readtec : file=%s\n", file.c_str());

	fdebug = fpout;

	/* clear of tec grid data option */
	if (!opt)
	{
		navi->tecList.clear();
	}

	if (!(fp = fopen(file.c_str(), "r")))
	{
		if (fpout)
			fprintf(fpout, "ionex file open error %s\n", file.c_str());

		return;
	}

	/* read ionex header */
	double nexp = -1;
	double rb	= 0;
	if (readionexh(fp, lats, lons, hgts, rb, nexp, navi) <= 0)
	{
		if (fpout)
			fprintf(fpout, "ionex file format error %s\n", file.c_str());

		return;
	}

	/* read ionex body */
	readionexb(fp, lats, lons, hgts, rb, nexp, navi);

	fclose(fp);
}

/* interpolate tec grid data -------------------------------------------------*/
int interptec(const tec_t* tec, int k, const double* posp, double& value, double& rms)
{
	double a, b, d[4] = {0}, r[4] = {0};
	int i, j, n, index;

	if (fdebug)
		fprintf(fdebug, "interptec: k=%d posp=%.2f %.2f\n", k, posp[0]*R2D, posp[1]*R2D);

	value	= 0;
	rms		= 0;

	if	( tec->lats[2] == 0
		||tec->lons[2] == 0)
		return 0;

	double dlat = posp[0] * R2D - tec->lats[0];
	double dlon = posp[1] * R2D - tec->lons[0];

	if (tec->lons[2] > 0)	dlon -= floor( dlon / 360) * 360; /*  0<=dlon<360 */
	else                  	dlon += floor(-dlon / 360) * 360; /* -360<dlon<=0 */

	a = dlat / tec->lats[2];
	b = dlon / tec->lons[2];
	i = (int)floor(a); 			a -= i;
	j = (int)floor(b); 			b -= j;

	/* get gridded tec data */
	for (n = 0; n < 4; n++)
	{
		index = dataindex(i + (n % 2), j + (n < 2 ? 0 : 1), k, tec->ndata);
		if (index < 0)
			continue;

		auto mapd = tec->data.find(index);
		auto mapr = tec->rms.find(index);

		if (mapd == tec->data.end())
			continue;

		if (mapr == tec->rms.end())
			continue;

		d[n] = mapd->second;
		r[n] = mapr->second;
	}

	if	(  d[0] > 0
		&& d[1] > 0
		&& d[2] > 0
		&& d[3] > 0)
	{
		/* bilinear interpolation (inside of grid) */
		value	= (1 - a) * (1 - b) * d[0] + a * (1 - b) * d[1] + (1 - a) * b * d[2] + a * b * d[3];
		rms		= (1 - a) * (1 - b) * r[0] + a * (1 - b) * r[1] + (1 - a) * b * r[2] + a * b * r[3];

		if (fdebug)
			fprintf(fdebug, "  gridpoints: %8.2f %8.2f %8.2f %8.2f -> %9.3f\n", d[0], d[1], d[2], d[3], value);
	}
	/* nearest-neighbour extrapolation (outside of grid) */
	else if (a <=	0.5 && b <= 0.5 && d[0] > 0) 	{	value = d[0];	rms = r[0];}
	else if (a >	0.5 && b <= 0.5 && d[1] > 0) 	{	value = d[1];	rms = r[1];}
	else if (a <=	0.5 && b >	0.5 && d[2] > 0) 	{	value = d[2];	rms = r[2];}
	else if (a >	0.5 && b >	0.5 && d[3] > 0) 	{	value = d[3];	rms = r[3];}
	else
	{
		i = 0;

		for (n = 0; n < 4; n++) if (d[n] > 0.0) {i++; 	value += d[n];	rms += r[n];}

		if (i == 0)
			return 0;

		value	/= i;
		rms		/= i;
	}

	return 1;
}

/* ionosphere delay by tec grid data -----------------------------------------*/
int iondelay(GTime time, const tec_t* tec, const double* pos, const double* azel, int opt, double& delay, double& var)
{
	const double fact = 40.30E16 / FREQ1 / FREQ1; /* tecu->L1 iono (m) */
	double fs, posp[3] = {0}, vtec, rms, hion, rp;
	int i;

	if (fdebug)
		fprintf(fdebug, "iondelay: time=%s pos=%.1f %.1f azel=%.1f %.1f\n", time.to_string(0).c_str(), pos[0]*R2D, pos[1]*R2D, azel[0]*R2D, azel[1]*R2D);

	delay	= 0;
	var		= 0;

	for (i = 0; i < tec->ndata[2]; i++) /* for a layer */
	{
		hion = tec->hgts[0] + tec->hgts[2] * i;

		/* ionospheric pierce point position */
		fs = ionppp(pos, azel, tec->rb, hion, posp);

		if (opt & 2)
		{
			/* modified single layer mapping function (M-SLM) ref [2] */
			rp = tec->rb / (tec->rb + hion) * sin(0.9782 * (PI / 2 - azel[1]));
			fs = 1 / sqrt(1 - rp * rp);
		}

		if (opt & 1)
		{
			/* earth rotation correction (sun-fixed coordinate) */
			posp[1] += 2 * PI * timediff(time, tec->time) / 86400;
		}

		/* interpolate tec grid data */
		if (!interptec(tec, i, posp, vtec, rms))
			return 0;

		delay	+= fact * fs * vtec;
		var		+= fact * fact * fs * fs * rms * rms;
	}

	if (fdebug)
		fprintf(fdebug, "iondelay: delay=%7.2f std=%6.2f\n", delay, sqrt(var));

	return 1;
}

/* ionosphere model by tec grid data -------------------------------------------
* compute ionospheric delay by tec grid data
* args   : gtime_t time     I   time (gpst)
*          nav_t  *nav      I   navigation data
*          double *pos      I   receiver position {lat,lon,h} (rad,m)
*          double *azel     I   azimuth/elevation angle {az,el} (rad)
*          int    opt       I   model option
*                                bit0: 0:earth-fixed,1:sun-fixed
*                                bit1: 0:single-layer,1:modified single-layer
*          double *delay    O   ionospheric delay (L1) (m)
*          double *var      O   ionospheric dealy (L1) variance (m^2)
* return : status (1:ok,0:error)
* notes  : before calling the function, read tec grid data by calling readtec()
*          return ok with delay=0 and var=VAR_NOTEC if el<MIN_EL or h<MIN_HGT
*-----------------------------------------------------------------------------*/
int iontec(GTime time, const nav_t* nav, const double* pos, const double* azel, int opt, double& delay, double& var)
{
	double dels[2], vars[2], a, tt;
	int stat[2] = {0};
	tec_t tec1, tec2;

	if (fdebug)
		fprintf(fdebug, "iontec  : time=%s pos=%.1f %.1f azel=%.1f %.1f nt=%ld\n", time.to_string(0).c_str(), pos[0]*R2D, pos[1]*R2D, azel[0]*R2D, azel[1]*R2D, nav->tecList.size());

	delay	= 0;
	var		= VAR_NOTEC;

	if	( azel[1] < MIN_EL
		|| pos[2] < MIN_HGT)
		return 1;

	if (nav->tecList.empty())
		return 1;

	auto it = nav->tecList.begin();

	if (it->first > time.time)
	{
		if (fdebug)
			fprintf(fdebug, "%s: tec grid out of period\n", time.to_string(0).c_str());

		return 0;
	}

	tec1 = it->second;

	for (; it != nav->tecList.end(); it++)
	{
		tec2 = it->second;

		if (it->first > time.time)
		{
			stat[0] = iondelay(time, &tec1, pos, azel, opt, dels[0], vars[0]);
			stat[1] = iondelay(time, &tec2, pos, azel, opt, dels[1], vars[1]);
			break;
		}

		tec1 = it->second;

		if (fdebug)
			fprintf(fdebug, "tec map %s\n", tec1.time.to_string(0).c_str());
	}

	if (!stat[0] && !stat[1])
	{
		if (fdebug)
			fprintf(fdebug, "%s: tec grid out of area pos=%6.2f %7.2f azel=%6.1f %5.1f\n",  time.to_string(0).c_str(), pos[0]*R2D, pos[1]*R2D, azel[0]*R2D, azel[1]*R2D);

		return 0;
	}

	if (stat[0] && stat[1]) /* linear interpolation by time */
	{
		tt	= timediff(tec2.time,	tec1.time);
		a	= timediff(time,		tec1.time) / tt;
		delay	= dels[0] * (1 - a) + dels[1] * a;
		var		= vars[0] * (1 - a) + vars[1] * a;
	}
	else if (stat[0])   /* nearest-neighbour extrapolation by time */
	{
		delay	= dels[0];
		var		= vars[0];
	}
	else
	{
		delay	= dels[1];
		var		= vars[1];
	}

	if (fdebug)
		fprintf(fdebug, "iontec  : delay=%5.2f std=%5.2f\n", delay, sqrt(var));

	return 1;
}
