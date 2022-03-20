/**------------------------------------------------------------------------------

* references:
*     [1] S.Schear, W.Gurtner and J.Feltens, IONEX: The IONosphere Map EXchange
*         Format Version 1, February 25, 1998
*     [2] S.Schaer, R.Markus, B.Gerhard and A.S.Timon, Daily Global Ionosphere
*         Maps based on GPS Carrier Phase Data Routinely producted by CODE
*         Analysis Center, Proceeding of the IGS Analysis Center Workshop, 1996
*-----------------------------------------------------------------------------*/
#include <boost/log/trivial.hpp>


#include "corrections.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "common.hpp"


#define VAR_NOTEC   SQR(30.0)   /* variance of no tec */
#define MIN_EL      0.0         /* min elevation angle (rad) */
#define MIN_HGT     -1000.0     /* min user height (m) */

/* get index -----------------------------------------------------------------*/
int getindex(
	double value, 
	const double* range)
{
	if (range[2] == 0)												return 0;
	if (range[1] > 0 && (value < range[0] || range[1] < value))		return -1;
	if (range[1] < 0 && (value < range[1] || range[0] < value))		return -1;

	return (int) floor((value - range[0]) / range[2] + 0.5);
}

/* get number of items -------------------------------------------------------*/
int nitem(
	const double* range)
{
	return getindex(range[1], range) + 1;
}

/* data index (i:lat,j:lon,k:hgt) --------------------------------------------*/
int dataindex(
	int i,
	int j,
	int k,
	const int* ndata)
{
	if	(  i < 0 || ndata[0] <= i
		|| j < 0 || ndata[1] <= j
		|| k < 0 || ndata[2] <= k)
	{
		return -1;
	}

	return i + ndata[0] * (j + ndata[1] * k);
}

/* read ionex dcb aux data ----------------------------------------------------*/
void readionexdcb(
	FILE* fp, 
	nav_t* navi)
{
	char buff[1024];
	char id[32] = "";

	BOOST_LOG_TRIVIAL(debug)
	<< "readionexdcb:";

	for (auto& [key, satNav] : navi->satNavMap)
	{
		satNav.cBias_P1_P2 = 0;
	}

	while (fgets(buff, sizeof(buff), fp))
	{
		if (strlen(buff) < 60)
			continue;

		char* label = buff + 60;

		if (strstr(label, "PRN / BIAS / RMS") == label)
		{
			strncpy(id, buff + 3, 3);
			id[3] = '\0';

			SatSys Sat = SatSys(id);
			if (Sat)
			{
				navi->satNavMap[Sat].cBias_P1_P2 = str2num(buff, 6, 10) * CLIGHT * 1E-9;

				BOOST_LOG_TRIVIAL(debug)
				<< id << navi->satNavMap[Sat].cBias_P1_P2;
			}
			else
			{
				BOOST_LOG_TRIVIAL(debug)
				<< "ionex invalid satellite: " << id;

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
double readionexh(
	FILE* fp,
	double* lats, 
	double* lons, 
	double* hgts, 
	double& rb, 
	double& nexp,
	nav_t* navi)
{
	double ver = 0;
	char buff[1024];

	BOOST_LOG_TRIVIAL(debug)
	<< "readionexh:";

	while (fgets(buff, sizeof(buff), fp))
	{
		if (strlen(buff) < 60)
			continue;

		char* label = buff + 60;

		if (strstr(label, "IONEX VERSION / TYPE") == label)
		{
			if (buff[20] == 'I')
				ver = str2num(buff, 0, 8);

			BOOST_LOG_TRIVIAL(debug)
			<< " ver= " << ver;
		}
		else if (strstr(label, "BASE RADIUS") == label)
		{
			rb = str2num(buff, 0, 8);

			BOOST_LOG_TRIVIAL(debug)
			<< " rad= " << rb;
		}
		else if (strstr(label, "HGT1 / HGT2 / DHGT") == label)
		{
			hgts[0] = str2num(buff, 2, 6);
			hgts[1] = str2num(buff, 8, 6);
			hgts[2] = str2num(buff, 14, 6);

			BOOST_LOG_TRIVIAL(debug)
			<< " heights= " << hgts[0] << " " << hgts[1] << " " << hgts[2];
		}
		else if (strstr(label, "LAT1 / LAT2 / DLAT") == label)
		{
			lats[0] = str2num(buff, 2, 6);
			lats[1] = str2num(buff, 8, 6);
			lats[2] = str2num(buff, 14, 6);

			BOOST_LOG_TRIVIAL(debug)
			<< " lats= " << lats[0] << " " << lats[1] << " " << lats[2];
		}
		else if (strstr(label, "LON1 / LON2 / DLON") == label)
		{
			lons[0] = str2num(buff, 2,  6);
			lons[1] = str2num(buff, 8,  6);
			lons[2] = str2num(buff, 14, 6);

			BOOST_LOG_TRIVIAL(debug)
			<< " lons= " << lons[0] << " " << lons[1] << " " << lons[2];
		}
		else if ( strstr(label, "EXPONENT") == label)
		{
			nexp = str2num(buff, 0, 6);
		}
		else if	( strstr(label,	"START OF AUX DATA") == label
				&&strstr(buff,	"DIFFERENTIAL CODE BIASES"))
		{
			readionexdcb(fp, navi);
		}
		else if (strstr(label, "END OF HEADER") == label)
		{
			return ver;
		}
	}

	return 0;
}

/* read ionex body -----------------------------------------------------------*/
int readionexb(
	FILE* fp,
	const double* lats,
	const double* lons,
	const double* hgts,
	double rb,
	double nexp,
	nav_t* navi)
{
	GTime time = {};
	int type = 0;
	char buff[1024];
	char* label = buff + 60;

	// if (fdebug)
	// 	fprintf(fdebug, "readionexb:\n");

	while (fgets(buff, sizeof(buff), fp))
	{
		if (strlen(buff) < 60)
			continue;

		if 		(strstr(label, "START OF TEC MAP")		== label)
		{
			type = 1;
			time.time = 0;
			
		}
		else if (strstr(label, "END OF TEC MAP")		== label)
		{
			// if (fdebug)
			// 	fprintf(fdebug, "%5ld data and %5ld rms entries for %s\n", navi->tecList[time.time].data.size(), navi->tecList[time.time].rms.size(), time.to_string(0).c_str());

			type = 0;
		}
		else if (strstr(label, "START OF RMS MAP")		== label)
		{
			type = 2;
			time.time = 0;
		}
		else if (strstr(label, "END OF RMS MAP")		== label)
		{
			// if (fdebug)
			// 	fprintf(fdebug, "%5ld data and %5ld rms entries for %s\n", navi->tecList[time.time].data.size(), navi->tecList[time.time].rms.size(), time.to_string(0).c_str());

			type = 0;
		}
		else if (strstr(label, "EPOCH OF CURRENT MAP")	== label)
		{
			if (str2time(buff, 0, 36, time))
			{
				// fprintf(fdebug, "ionex epoch invalid: %-36.36s\n", buff);
				continue;
			}

			auto& epochTec = navi->tecMap[time];
			
			if (type == 1)
			{
				epochTec.time		= time;
				epochTec.ndata[0]	= nitem(lats);
				epochTec.ndata[1]	= nitem(lons);
				epochTec.ndata[2]	= nitem(hgts);
				epochTec.rb			= rb;

				for (int i = 0; i < 3; i++)
				{
					epochTec.lats[i] = lats[i];
					epochTec.lons[i] = lons[i];
					epochTec.hgts[i] = hgts[i];
				}

				epochTec.tecPointVector.resize(epochTec.ndata[0] * epochTec.ndata[1] * epochTec.ndata[2]);
				
				std::fill(epochTec.tecPointVector.begin(), epochTec.tecPointVector.end(), TECPoint{});
			}
		}
		else if	( strstr(label, "LAT/LON1/LON2/DLON/H")	== label
				&& time.time
				&& type)
		{
			double lon[3];
			double	lat		= str2num(buff, 2,  6);
					lon[0]	= str2num(buff, 8,  6);
					lon[1]	= str2num(buff, 14, 6);
					lon[2]	= str2num(buff, 20, 6);
			double	hgt		= str2num(buff, 26, 6);

			int i = getindex(lat, lats);
			int k = getindex(hgt, hgts);
			int n = nitem(lon);

			auto& epochTec = navi->tecMap[time];
			
			for (int m = 0; m < n; m++)
			{
				if (m % 16 == 0 && !fgets(buff, sizeof(buff), fp))
					break;

				int j = getindex(lon[0] + lon[2] * m, lons);

				int index = dataindex(i, j, k, epochTec.ndata);
				if (index  < 0)
					continue;
				
				double x = str2num(buff, m % 16 * 5, 5);
				if (x == 9999)
					continue;

				if (type == 1)		epochTec.tecPointVector[index].data	= x * pow(10, nexp);
				if (type == 2)		epochTec.tecPointVector[index].rms	= x * pow(10, nexp);
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
* notes  : see ref [1]
*-----------------------------------------------------------------------------*/
void readtec(
	string file,
	nav_t* navi)
{
	BOOST_LOG_TRIVIAL(debug)
	<< "readtec : file=" << file;

	FILE* fp = fopen(file.c_str(), "r");
	if (fp == nullptr)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "ionex file open error " << file;

		return;
	}

	/* read ionex header */
	double nexp = -1;
	double rb	= 0;
	double lats[3] = {};
	double lons[3] = {};
	double hgts[3] = {};
	double version = readionexh(fp, lats, lons, hgts, rb, nexp, navi);
	if (version <= 0)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "ionex file format error " << file;

		return;
	}

	/* read ionex body */
	readionexb(fp, lats, lons, hgts, rb, nexp, navi);

	fclose(fp);
}

/* interpolate tec grid data -------------------------------------------------*/
int interptec(
	const tec_t& tec,
	int k, 
	const double* posp,
	double& value,
	double& rms)
{
	// if (fdebug)
	// 	fprintf(fdebug, "%s: k=%d posp=%.2f %.2f\n",__FUNCTION__, k, posp[0]*R2D, posp[1]*R2D);

	value	= 0;
	rms		= 0;

	if	( tec.lats[2] == 0
		||tec.lons[2] == 0)
	{
		return 0;
	}

	double dlat = posp[0] * R2D - tec.lats[0];
	double dlon = posp[1] * R2D - tec.lons[0];

	if (tec.lons[2] > 0)	dlon -= floor( dlon / 360) * 360; /*  0<=dlon<360 */
	else					dlon += floor(-dlon / 360) * 360; /* -360<dlon<=0 */

	double a = dlat / tec.lats[2];
	double b = dlon / tec.lons[2];
	int i = (int) floor(a); 			a -= i;
	int j = (int) floor(b); 			b -= j;

	/* get gridded tec data */
	double d[4] = {};
	double r[4] = {};
	for (int n = 0; n < 4; n++)
	{
		int index = dataindex(i + (n % 2), j + (n < 2 ? 0 : 1), k, tec.ndata);
		if (index < 0)
			continue;

		auto& tecPoint = tec.tecPointVector[index];
		
		d[n] = tecPoint.data;
		r[n] = tecPoint.rms;
	}

	if	(  d[0] > 0
		&& d[1] > 0
		&& d[2] > 0
		&& d[3] > 0)
	{
		/* bilinear interpolation (inside of grid) */
		value	= (1 - a) * (1 - b) * d[0] 		+ a * (1 - b) * d[1] 		+ (1 - a) * b * d[2] 		+ a * b * d[3];
		rms		= (1 - a) * (1 - b) * r[0] 		+ a * (1 - b) * r[1] 		+ (1 - a) * b * r[2] 		+ a * b * r[3];

		// if (fdebug)
		// 	fprintf(fdebug, "  gridpoints: %8.2f %8.2f %8.2f %8.2f -> %9.3f\n", d[0], d[1], d[2], d[3], value);
	}
	/* nearest-neighbour extrapolation (outside of grid) */
	else if (a <=	0.5 && b <= 0.5 && d[0] > 0) 	{	value = d[0];		rms = r[0];		}
	else if (a >	0.5 && b <= 0.5 && d[1] > 0) 	{	value = d[1];		rms = r[1];		}
	else if (a <=	0.5 && b >	0.5 && d[2] > 0) 	{	value = d[2];		rms = r[2];		}
	else if (a >	0.5 && b >	0.5 && d[3] > 0) 	{	value = d[3];		rms = r[3];		}
	else
	{
		i = 0;

		for (int n = 0; n < 4; n++)
		if (d[n] > 0)
		{
			i++;
			value	+= d[n];
			rms		+= r[n];
		}

		if (i == 0)
			return 0;

		value	/= i;
		rms		/= i;
	}

	return 1;
}

/* ionosphere delay by tec grid data -----------------------------------------*/
int iondelay(
	GTime time, 
	const tec_t& tec,
	const double* pos, 
	const double* azel,
	int opt,
	double& delay,
	double& var)
{
	// if (fdebug)
	// 	fprintf(fdebug, "%s: time=%s pos=%.1f %.1f azel=%.1f %.1f\n", __FUNCTION__, time.to_string(0).c_str(), pos[0]*R2D, pos[1]*R2D, azel[0]*R2D, azel[1]*R2D);

	delay	= 0;
	var		= 0;

	for (int i = 0; i < tec.ndata[2]; i++)
	{
		double hion = tec.hgts[0] + tec.hgts[2] * i;

		/* ionospheric pierce point position */
		double posp[3] = {};
		double fs = ionppp(pos, azel, tec.rb, hion, posp);

		if (opt & 2)
		{
			/* modified single layer mapping function (M-SLM) ref [2] */
			double rp = tec.rb / (tec.rb + hion) * sin(0.9782 * (PI / 2 - azel[1]));
			fs = 1 / sqrt(1 - rp * rp);
		}

		if (opt & 1)
		{
			/* earth rotation correction (sun-fixed coordinate) */
			posp[1] += 2 * PI * (time - tec.time) / 86400;
		}

		/* interpolate tec grid data */
		double rms;
		double vtec;
		if (!interptec(tec, i, posp, vtec, rms))
			return 0;

		const double fact = 40.30E16 / FREQ1 / FREQ1; /* tecu->L1 iono (m) */
		delay	+= fact * fs * vtec;
		var		+= SQR(fact * fs * rms);
	}

	// if (fdebug)
	// 	fprintf(fdebug, "%s: delay=%7.2f std=%6.2f\n",__FUNCTION__, delay, sqrt(var));

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
int iontec(
	GTime time,
	const nav_t*	nav,
	const double*	pos,
	const double*	azel,
	int				opt,
	double&			delay,
	double&			var)
{
	// if (fdebug)
	// 	fprintf(fdebug, "iontec  : time=%s pos=%.1f %.1f azel=%.1f %.1f nt=%ld\n", time.to_string(0).c_str(), pos[0]*R2D, pos[1]*R2D, azel[0]*R2D, azel[1]*R2D, nav->tecList.size());

	delay	= 0;
	var		= VAR_NOTEC;

	if	(  azel[1]	< MIN_EL
		|| pos[2]	< MIN_HGT)
	{
		return 1;
	}

	auto it = nav->tecMap.lower_bound(time);
	if (it == nav->tecMap.end())
	{
		// if (fdebug)
		// 	fprintf(fdebug, "%s: tec grid out of period\n", time.to_string(0).c_str());

		return 1;
	}
	
	int stat[2] = {};
	double dels[2];
	double vars[2];
	
	auto& [t0, tec0] = *it;
	stat[0] = iondelay(time, tec0, pos, azel, opt, dels[0], vars[0]);
		
	if (it == nav->tecMap.begin())
	{
		delay	= dels[0];
		var		= vars[0];
		return stat[0];
	}
	
	//go forward and get the next timestep if available
	it--;
	
	auto& [t1, tec1] = *it;
	stat[1] = iondelay(time, tec1, pos, azel, opt, dels[1], vars[1]);

	

	if	(  stat[0]
		&& stat[1])
	{
		/* linear interpolation by time */
		double tt	= (tec1.time	- tec0.time);
		double a	= (time			- tec0.time) / tt;
		
		delay	= dels[0] * (1 - a) + dels[1] * a;
		var		= vars[0] * (1 - a) + vars[1] * a;
	}
	else if (stat[0])   /* nearest-neighbour extrapolation by time */
	{
		delay	= dels[0];
		var		= vars[0];
	}
	else if (stat[1])
	{
		delay	= dels[1];
		var		= vars[1];
	}
	else
	{
		// if (fdebug)
		// 	fprintf(fdebug, "%s: tec grid out of area pos=%6.2f %7.2f azel=%6.1f %5.1f\n",  time.to_string(0).c_str(), pos[0]*R2D, pos[1]*R2D, azel[0]*R2D, azel[1]*R2D);

		return 0;
	}

	// if (fdebug)
	// 	fprintf(fdebug, "iontec  : delay=%5.2f std=%5.2f\n", delay, sqrt(var));

	return 1;
}
