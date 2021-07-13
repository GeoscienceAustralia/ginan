

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <iostream>

#include <boost/log/trivial.hpp>
#include <boost/algorithm/string.hpp>

#include "navigation.hpp"
#include "common.hpp"
#include "antenna.hpp"
#include "enums.h"

#include "eigenIncluder.hpp"

/** Compare two time tags (t2-t1)
* Returns julian day difference
*/
double timecomp(
	const double t1[6],		///< time tag in [YMDHMS]
	const double t2[6])		///< time tag in [YMDHMS]
{
	/* convert two time tags to julian day */
	double jd1 = ymdhms2jd(t1);
	double jd2 = ymdhms2jd(t2);

// 	BOOST_LOG_TRIVIAL(debug) << "time difference in julian day is "<< jd2-jd1;

	return jd2-jd1;
}

/* decode antenna field */
int decodef(char *p, int n, double *v)
{
	int i;
	for (i = 0; i < n; i++)
		v[i] = 0;

	for (i = 0, p = strtok(p," "); p && i < n; p = strtok(NULL, " "))
	{
		v[i] = atof(p) * 1E-3;
		i++;
	}
	return i;
}

pcvacs_t* findAntenna(
	string code,
	double tc[6],
	nav_t& nav)
{
// 	BOOST_LOG_TRIVIAL(debug)
// 	<< "Searching for " << type << ", " << code;

	auto it1 = nav.pcvMap.find(code);
	if (it1 == nav.pcvMap.end())
	{
		return nullptr;
	}
	
	auto& pcvTimeMap = it1->second;
	
	if (pcvTimeMap.size() == 0)
	{
		return nullptr;
	}
	
	GTime time = epoch2time(tc);
	
	auto it2 = pcvTimeMap.lower_bound(time);
	if (it2 == pcvTimeMap.end())
	{
		//just use the first chronologically, (last when sorted as they are) instead
		auto it3 = pcvTimeMap.rbegin();
		pcvacs_t& pcv = it3->second;
		
		return &pcv;
	}
	
	pcvacs_t& pcv = it2->second;
	
	return &pcv;
}
	
/* linear interpolate pcv ------------------------------------------------------
*
* args     :       double x1              I       x1 lower bound (degree)
*                  double x2              I       x2 upper bound (degree)
*                  double y1              I       y1 lower bound (m)
*                  double y2              I       y2 upper bound (m)
*                  double x               O       x current point (degree)
*
* return   :       interpolated pcv (m)
*----------------------------------------------------------------------------*/
double interp(double x1, double x2, double y1, double y2, double x)
{
#if (0)
	return (y2-y1)*(x-x1)/(x2-x1)+y1;
#endif
	return y2-(y2-y1)*(x2-x)/(x2-x1);
}

/* fetch rec pco ---------------------------------------------------------------
*
* args     :       const pcvacs_t *pc     I       antenna info
*                  const chat sys         I       satellite system
*                  const int freq         I       frequency 1 or 2
*                  double pco[3]          O       rec pco (m)
*
* return   :       none
*
* note     :       frequencies are sorted as GPS, GLONASS, Galileo and BeiDou
*----------------------------------------------------------------------------*/
void recpco(pcvacs_t *pc, int freq, Vector3d& pco)
{
	pcvacs_t& pcv = *pc;
	/* assign rec/sat pco */
	if (pcv.pcoMap.find((E_FType)freq) == pcv.pcoMap.end())		pco = Vector3d::Zero();
	else														pco = pcv.pcoMap[(E_FType) freq];
}

/* fetch rec pcv ---------------------------------------------------------------
*
* args     :       const pcvacs_t pc      I       antenna info
*                  const chat sys         I       satellite system
*                  const int freq         I       frequency 1, 2
*                  const double el        I       satellite elevation (degree)
*                  const double azi       I       azimuth (degree)
*                  double *pcv            O       rec pcv (m)
*
* return   :       none
*
* note     :       frequencies are sorted as GPS, GLONASS, Galileo and BeiDou
*----------------------------------------------------------------------------*/
void recpcv(
	pcvacs_t*	pc,
	int			freq,
	double		el,
	double		azi,
	double&		pcv)
{
	int		nz		= pc->nz;
	int		naz		= pc->naz;
	double	zen1	= pc->zenStart;
	double	dzen	= pc->zenDelta;
	double	dazi	= pc->aziDelta;
	double	zen		= 90 - el;

	if (pc->PCVMap1D.find(freq) == pc->PCVMap1D.end())
	{
		//frequency not found
		return;
	}
	auto& pcvMap1D = pc->PCVMap1D[freq];

	/* select zenith angle range */
	int zen_n;
	for (zen_n = 1; zen_n < nz; zen_n++)
	{
		if ((zen1 + dzen * zen_n) >= zen)
		{
			break;
		}
	}

	double xz1 = zen1 + dzen * (zen_n - 1);
	double xz2 = zen1 + dzen * (zen_n);

	if (naz == 0)
	{
		/* linear interpolate receiver pcv - non azimuth-dependent */
		/* interpolate */

		double	yz1 = pcvMap1D[zen_n - 1];		// lower bound
		double	yz2 = pcvMap1D[zen_n];			// upper bound
		pcv = interp(xz1, xz2, yz1, yz2, zen);
	}
	else
	{
		if (pc->PCVMap2D.find(freq) == pc->PCVMap2D.end())
		{
			//frequency not found
			return;
		}
		auto& pcvMap2D = pc->PCVMap2D[freq];

		/* bilinear interpolate receiver pcv - azimuth-dependent */
		/* select azimuth angle range */
		int az_n;
		for (az_n = 1; az_n < naz; az_n++)
		{
			if ((dazi * az_n) >= azi)
			{
				break;
			}
		}

		double xa1 = dazi * (az_n -1);
		double xa2 = dazi * (az_n);

		double yz3 = pcvMap2D[az_n-1]	[zen_n-1];		double yz1 = pcvMap2D[az_n-1]	[zen_n];
		double yz4 = pcvMap2D[az_n]		[zen_n-1];		double yz2 = pcvMap2D[az_n]		[zen_n];

		/* linear interpolation along zenith angle */
		double ya1	= interp(xz1, xz2, yz3, yz1, zen);
		double ya2 	= interp(xz1, xz2, yz4, yz2, zen);

		/* linear interpolation along azimuth angle */
		pcv	= interp(xa1, xa2, ya1, ya2, azi);
	}

	return;
}
//=============================================================================
// radomeNoneAntennaType = radome2none(antennaType)
//
//       e,g, "AOAD/M_T        JPLA" => "AOAD/M_T        NONE"
//
// Change the last four characters of antenna type to NONE
// This function is useful for when searching for an antenna model in ANTEX
//
// The IGS convention is to default to NONE for the radome if the calibration
// value is not available
//=============================================================================
//void radome2none(char *restrict antenna_type)
void radome2none(string& antenna_type)
{
	size_t length = antenna_type.size();
	if (length != 20)
	{
		printf("\n*** ERROR radome2none(): string length is less then 20 characters received %ld characters\n",length);
		return;
	}
	antenna_type.replace(length - 4, 4, "NONE");
}

map<string, E_FType> antexCodes =
{
	{"G01", L1    },
	{"G02", L2    },
	{"G05", L5    },
	{"R01", G1    },
	{"R02", G2    },
	{"E01", E1    },
	{"E05", E5A   },
	{"E07", E5B   },
	{"E08", E5AB  },
	{"E06", E6    },
	{"C01", E1    },
	{"C02", E2    },
	{"C07", E5B   },
	{"C06", E6    },
	{"J01", L1    },
	{"J02", L2    },
	{"J05", L5    },
	{"J06", LEX   },
	{"S01", L1    },
	{"S05", L5    }
};

/** Read antex file */
int readantexf(
	string	file,
	nav_t&	nav)
{
	int offset;
	int noazi_flag		= 0;
	int num_azi_rd		= 0;
	int new_antenna		= 0;
	int num_antennas	= 0;
	int irms			= 0;

	char tmp[10];
	char *p;

	FILE* fp = fopen(file.c_str(),"r");
	if (fp == nullptr)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: ANTEX file opening error";

		return 0;
	}

	const pcvacs_t pcv0 = {};
	pcvacs_t ds_pcv;

	E_FType	ft = FTYPE_NONE;

	char buff[512];
	while (fgets(buff, sizeof(buff), fp))
	{
		char* comment = buff + 60;

		if (irms) 
			continue;
		/* Read in the ANTEX header information */
		if (strlen(buff) < 60 )								{	continue;	}
		if (strstr(comment, "ANTEX VERSION / SYST"))		{	continue;	}
		if (strstr(comment, "PCV TYPE / REFANT")) 			{	continue;	}
		if (strstr(comment, "COMMENT")) 					{	continue;	}
		if (strstr(comment, "END OF HEADER"))				{	continue;	}
		/* Read in specific Antenna information now */
		
		if (strstr(comment, "START OF ANTENNA"))
		{
			num_antennas++;
			ds_pcv		= pcv0;
			new_antenna	= 1;        /* flag for new antenna */
			
			continue;
		}
		
		if (!new_antenna)
		{
			continue;
		}
		
		if (strstr(comment, "METH / BY / # / DATE"))
		{
// 			int num_calibrated;
// 			char cal_method[20];
// 			char cal_agency[20];
// 			char cal_date[10];
// 			strncpy(cal_method,	buff,		20);/* Should be CHAMBER or FIELD or ROBOT or COPIED ot CONVERTED */
// 			cal_method[19] = '\0';
// 			strncpy(cal_agency, buff + 20,	20);
// 			cal_agency[19] = '\0';
// 			strncpy(tmp,		buff + 40,	10);
// 			num_calibrated = atoi(tmp);
// 			strncpy(cal_date,	buff + 50,	10);
// 			cal_date[9] = '\0';
			
			continue;
		}
		
		if (strstr(comment, "DAZI"))
		{
			strncpy(tmp,buff   ,8);tmp[8] = '\0';
			ds_pcv.aziDelta = atof(tmp);

			if (ds_pcv.aziDelta < 0.0001)	ds_pcv.naz = 0;
			else                     		ds_pcv.naz = (360 / ds_pcv.aziDelta) + 1;
			
			continue;
		}
		
		if (strstr(comment, "END OF ANTENNA"))
		{
			/* reset the flags for the next antenna */
			new_antenna	= 0;

			/* stack antenna pco and pcv */
			string id;
			string& satId = ds_pcv.code;
			if (satId.find_first_not_of(' ') == satId.npos)		{ id = ds_pcv.type;	}
			else												{ id = ds_pcv.code;	}
		
			boost::trim_right(id);
			
			GTime time = epoch2time(ds_pcv.tf);
			nav.pcvMap[id][time] = ds_pcv;
			
			continue;
		}
		
		if (strstr(comment, "TYPE / SERIAL NO"))
		{
			ds_pcv.type		.assign(buff,		20);
			ds_pcv.code		.assign(buff+20,	20);
			ds_pcv.svn		.assign(buff+40,	4);
			ds_pcv.cospar	.assign(buff+50,	10);
			
			continue;
		}
		
		if (strstr(comment, "ZEN1 / ZEN2 / DZEN"))
		{
			strncpy(tmp, buff,		8);	tmp[8] = '\0'; 	ds_pcv.zenStart	= atof(tmp);
			strncpy(tmp, buff+8,	7);	tmp[8] = '\0'; 	ds_pcv.zenStop	= atof(tmp);
			strncpy(tmp, buff+16,	7);	tmp[8] = '\0'; 	ds_pcv.zenDelta	= atof(tmp);

			ds_pcv.nz = (ds_pcv.zenStop - ds_pcv.zenStart) / ds_pcv.zenDelta + 1 ;
			
			continue;
		}
		
		if (strstr(comment, "# OF FREQUENCIES"))
		{
//			strncpy(tmp, buff,		8);
// 			tmp[8] = '\0'; 	
// 			ds_pcv.nf		= atoi(tmp);
			
			continue;
		}
		
		if (strstr(comment, "VALID FROM"))
		{
			char valid_from[44];
			/* if (!str2time(buff,0,43,pcv.ts)) continue;*/
			strncpy(valid_from, buff, 43);
			valid_from[43] = '\0';
			p = strtok(valid_from, " ");
			int j = 0;
			while (p != NULL)
			{
				ds_pcv.tf[j] = (double) atoi(p);
				p = strtok(NULL, " ");
				j++;
			}
			
			continue;
		}
		
		if (strstr(comment, "VALID UNTIL"))
		{
			char valid_until[44];
			/* if (!str2time(buff,0,43,pcv.te)) continue;*/
			strncpy(valid_until, buff   ,43);
			valid_until[43] = '\0';
			p = strtok(valid_until, " ");
			int j = 0;
			while (p != nullptr)
			{
				ds_pcv.tu[j] = (double) atoi(p);
				p = strtok(NULL, " ");
				j++;
			}
			
			continue;
		}
		
		if (strstr(comment, "NORTH / EAST / UP"))
		{
			double neu[3];
			if (decodef(buff, 3, neu) < 3)
			{
				continue;
			}

			/* assign pco value in ENU */
			Vector3d& enu = ds_pcv.pcoMap[ft];
			enu[0] = neu[1];
			enu[1] = neu[0];
			enu[2] = neu[2];
			
			continue;
		}
		
		if (strstr(comment, "START OF FREQUENCY"))
		{
			num_azi_rd = 0;
			noazi_flag = 0;

			string antexFCode;
			antexFCode.assign(&buff[3], 3);

			ft = antexCodes[antexFCode];
			
			continue;
		}
		
		if (strstr(comment, "END OF FREQUENCY"))	{	noazi_flag	= 0;	continue;	}
		if (strstr(comment, "START OF FREQ RMS"))	{	irms		= 1;	continue;	}
		if (strstr(comment, "END OF FREQ RMS"))		{	irms		= 0;	continue;	}
		
		if (!irms && strstr(buff,"NOAZI"))
		{
			for (int i = 0; i < ds_pcv.nz; i++)
			{
				offset = i * 8 + 8;
				strncpy(tmp, buff + offset, 8);
				tmp[8]='\0';
				double pcv_val = atof(tmp);
				ds_pcv.PCVMap1D[ft]			.push_back(pcv_val * 1e-3);
			}
			noazi_flag = 1;
			
			continue;
		}
		
		if (!irms && noazi_flag == 1)
		{
			strncpy(tmp, buff, 8);
			tmp[8]='\0';

			for (int i = 0; i < ds_pcv.nz; i++)
			{
				offset = i * 8 + 8;
				strncpy(tmp, buff + offset, 8);
				tmp[8]='\0';
				double pcv_val = atof(tmp);
				ds_pcv.PCVMap2D[ft][num_azi_rd].push_back(pcv_val * 1e-3);
			}
			num_azi_rd++;
			
			continue;
		}
	}

	fclose(fp);

	return 1;
}

/** Satellite antenna model.
* Compute satellite antenna phase center parameters
*/
// inplace of antmodel_
// this will not work
// for galileo models
void interp_satantmodel(
	pcvacs_t&			pcv,	///< antenna phase center parameters
	double				nadir,	///< nadir angle for satellite (rad)
	map<int, double>&	dant)	///< range offsets for each frequency (m)
{
	for (auto& [ft, pcvVector] : pcv.PCVMap1D)
	{
		double	nadirDeg		= nadir * R2D;				// ang=0-90
		int		numSections		= pcvVector.size();
		double	startAngle		= pcv.zenStart;
		double	sectionWidth	= pcv.zenDelta;
		double	realSection		= ((nadirDeg - startAngle) / sectionWidth);
		double	intSection		= (int) realSection;
		double	fraction		= realSection - intSection;

		if		(intSection < 0)				{	dant[ft] = pcvVector[0];					}
		else if	(intSection >= numSections)		{	dant[ft] = pcvVector[numSections-1];		}
		else
		{
			double a = pcvVector[intSection];
			double b = pcvVector[intSection + 1];

			dant[ft] = a + fraction * (b - a);
		}
	}
}
