
// #pragma GCC optimize ("O0")


#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <iostream>

#include <boost/log/trivial.hpp>
#include <boost/algorithm/string.hpp>

#include "navigation.hpp"
#include "constants.hpp"
#include "antenna.hpp"
#include "common.hpp"
#include "enums.h"

#include "eigenIncluder.hpp"


/* decode antenna field */
int decodef(char *p, int n, double *v)
{
	int i;
	for (i = 0; i < n; i++)
		v[i] = 0;

	for (i = 0, p = strtok(p," "); p && i < n; p = strtok(nullptr, " "))
	{
		v[i] = atof(p) * 1E-3;
		i++;
	}
	return i;
}

bool findAntenna(
	string				code,
	GTime				time,
	Navigation&			nav,
	E_FType				ft,
	PhaseCenterData**	pcd_ptr_ptr)
{
// 	BOOST_LOG_TRIVIAL(debug)
// 	<< "Searching for " << type << ", " << code;

	auto it1 = nav.pcvMap.find(code);
	if (it1 == nav.pcvMap.end())
	{
		return false;
	}
	
	auto& [dummyCode, pcvFreqMap] = *it1;
	
	auto it2 = pcvFreqMap.find(ft);
	if (it2 == pcvFreqMap.end())
	{
		return false;
	}
	
	auto& [dummy2, pcvTimeMap] = *it2;
	
	auto it3 = pcvTimeMap.lower_bound(time);
	if (it3 == pcvTimeMap.end())
	{
		//just use the first chronologically, (last when sorted as they are) instead
		auto it4 = pcvTimeMap.rbegin();
		
		auto& [dummyTime, pcd] = *it4;
		
		if (pcd_ptr_ptr)
			*pcd_ptr_ptr = &pcd;
		
		return true;
	}
	
	auto& [dummyTime, pcd] = *it3;
		
	if (pcd_ptr_ptr)
		*pcd_ptr_ptr = &pcd;
	
	return true;
}
	
/** linearly interpolate
 */
double interp(double x1, double x2, double y1, double y2, double x)
{
	return y2-(y2-y1)*(x2-x)/(x2-x1);
}

Vector3d makeAntPco(
	string		id,
	E_FType		ft,
	GTime		time)
{
	if (ft == F1)		return Vector3d::Zero();
	if (ft == F2)		return Vector3d::Zero();
	
	Vector3d pco1 = antPco(id, F1, time);
	Vector3d pco2 = antPco(id, F2, time);
	
	if (pco1.isZero())	return Vector3d::Zero();
	if (pco2.isZero())	return Vector3d::Zero();
	
	double lam1 = lam_carr[F1];
	double lam2 = lam_carr[F2];
	double lamX = lam_carr[ft];
	
	if (lamX == 0)		return Vector3d::Zero();
	
	double k32 = (lamX-lam2)/(lam1-lam2);
	double k31 = (lamX-lam1)/(lam1-lam2);
	
	Vector3d pco	= k32 * pco1
					- k31 * pco2;
					
	return pco;
}
	
/** fetch pco
 */
Vector3d antPco(
	string		id,
	E_FType		ft,
	GTime		time,
	bool		interp)
{
	auto it1 = nav.pcoMap.find(id);
	if (it1 == nav.pcoMap.end())
	{
		return Vector3d::Zero();
	}
	
	auto& [dummy1, pcoFreqMap] = *it1;
	
	auto it2 = pcoFreqMap.find(ft);
	if (it2 == pcoFreqMap.end())
	{
		if (interp)		return makeAntPco(id, ft, time);
		else			return Vector3d::Zero();
	}
	
	auto& [dummy2, pcoTimeMap] = *it2;
	
	auto it3 = pcoTimeMap.lower_bound(time);
	if (it3 == pcoTimeMap.end())
	{
		return Vector3d::Zero();
	}
	
	auto& [dummy3, pco] = *it3;
	
	return pco;
}

/** find and interpolate antenna pcv
*/
double antPcv(
	string		id,		///< antenna id
	E_FType		ft,		///< frequency
	GTime		time,	///< time
	double		aCos,	///< angle between target and antenna axis (radians)
	double		azi)	///< azimuth angle (radians)
{
	auto it1 = nav.pcvMap.find(id);
	if (it1 == nav.pcvMap.end())
	{
		return 0;
	}
	
	auto& [dummy1, pcvFreqMap] = *it1;

	auto it2 = pcvFreqMap.find(ft);
	if (it2 == pcvFreqMap.end())
	{
		return 0;
	}
	
	auto& [dummy2, pcvTimeMap] = *it2;
	
	auto it3 = pcvTimeMap.lower_bound(time);
	if (it3 == pcvTimeMap.end())
	{
		return 0;
	}
	
	auto& [dummy3, pcd] = *it3;
	
	auto& pcvMap1D = pcd.PCVMap1D;
	auto& pcvMap2D = pcd.PCVMap2D;

	int		nz		= pcd.nz;
	int		naz		= pcd.naz;
	double	zen1	= pcd.zenStart;
	double	dzen	= pcd.zenDelta;
	double	dazi	= pcd.aziDelta;
	double	zen		= aCos * R2D;
	azi *= R2D;
	
	double	pcv;
	
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

	if	( naz == 0
		||azi == 0)
	{
		/* linear interpolate receiver pcv - non azimuth-dependent */
		/* interpolate */

		double	yz1 = pcvMap1D[zen_n - 1];		// lower bound
		double	yz2 = pcvMap1D[zen_n];			// upper bound
		pcv = interp(xz1, xz2, yz1, yz2, zen);
	}
	else
	{
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

	return pcv;
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
	string		file,
	Navigation&	nav)
{
	int offset;
	int noazi_flag		= 0;
	int num_azi_rd		= 0;
	int new_antenna		= 0;
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

	const PhaseCenterData pcv0 = {};
	PhaseCenterData recPcv;
	PhaseCenterData freqPcv;
	Vector3d	pco = Vector3d::Zero();
	string		id;
	GTime		time;
	
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
			recPcv	= pcv0;
			freqPcv	= pcv0;
			pco		= Vector3d::Zero();
			id		= "";
			
			continue;
		}
// 		if (strstr(comment, "END OF ANTENNA"))
// 		{
// 			GTime time = epoch2time(recPcv.tf);
// 			
// 			continue;
// 		}
		

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
			strncpy(tmp,buff   ,8);		tmp[8] = '\0';
			recPcv.aziDelta = atof(tmp);

			if (recPcv.aziDelta < 0.0001)	recPcv.naz = 0;
			else							recPcv.naz = (360 / recPcv.aziDelta) + 1;
			
			continue;
		}
		
		if (strstr(comment, "SINEX CODE"))
		{
			recPcv.calibModel	.assign(buff,		10);
			continue;
		}
		
		if (strstr(comment, "TYPE / SERIAL NO"))
		{
			recPcv.type			.assign(buff,		20);
			recPcv.code			.assign(buff+20,	20);
			recPcv.svn			.assign(buff+40,	4);
			recPcv.cospar		.assign(buff+50,	10);
			
			/* stack antenna pco and pcv */
			string satId = recPcv.code;
			if (satId.find_first_not_of(' ') == satId.npos)		{ id = recPcv.type;	}
			else												{ id = recPcv.code;	}
		
			boost::trim_right(id);
			
			continue;
		}
		
		if (strstr(comment, "ZEN1 / ZEN2 / DZEN"))
		{
			strncpy(tmp, buff,		8);	tmp[8] = '\0'; 	recPcv.zenStart	= atof(tmp);
			strncpy(tmp, buff+8,	7);	tmp[8] = '\0'; 	recPcv.zenStop	= atof(tmp);
			strncpy(tmp, buff+16,	7);	tmp[8] = '\0'; 	recPcv.zenDelta	= atof(tmp);

			recPcv.nz = (recPcv.zenStop - recPcv.zenStart) / recPcv.zenDelta + 1 ;
			
			continue;
		}
		
		if (strstr(comment, "# OF FREQUENCIES"))
		{
//			strncpy(tmp, buff,		8);
// 			tmp[8] = '\0'; 	
// 			pcv.nf		= atoi(tmp);
			
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
			while (p != nullptr)
			{
				recPcv.tf[j] = (double) atoi(p);
				p = strtok(nullptr, " ");
				j++;
			}
			
			time = epoch2time(recPcv.tf);
			
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
				recPcv.tu[j] = (double) atoi(p);
				p = strtok(nullptr, " ");
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
			Vector3d enu;
			enu[0] = neu[1];
			enu[1] = neu[0];
			enu[2] = neu[2];
			
			pco = enu;
			
			continue;
		}
		
		if (strstr(comment, "START OF FREQUENCY"))
		{
			num_azi_rd = 0;
			noazi_flag = 0;

			string antexFCode;
			antexFCode.assign(&buff[3], 3);

			ft = antexCodes[antexFCode];
			
			freqPcv = recPcv;
			
			continue;
		}
		
		if (strstr(comment, "END OF FREQUENCY"))
		{	
			noazi_flag	= 0;	
			
			nav.pcvMap[id][ft][time] = freqPcv;
			nav.pcoMap[id][ft][time] = pco;
			
			continue;
		}
		
		if (strstr(comment, "START OF FREQ RMS"))	{	irms		= 1;	continue;	}
		if (strstr(comment, "END OF FREQ RMS"))		{	irms		= 0;	continue;	}
		
		if	(  irms == 0 
			&& strstr(buff, "NOAZI"))
		{
			for (int i = 0; i < recPcv.nz; i++)
			{
				offset = i * 8 + 8;
				strncpy(tmp, buff + offset, 8);
				tmp[8]='\0';
				double pcv_val = atof(tmp);
				freqPcv.PCVMap1D			.push_back(pcv_val * 1e-3);
			}
			noazi_flag = 1;
			
			continue;
		}
		
		if	(  irms == 0
			&& noazi_flag == 1)
		{
			strncpy(tmp, buff, 8);
			tmp[8]='\0';

			for (int i = 0; i < recPcv.nz; i++)
			{
				offset = i * 8 + 8;
				strncpy(tmp, buff + offset, 8);
				tmp[8]='\0';
				double pcv_val = atof(tmp);
				freqPcv.PCVMap2D[num_azi_rd].push_back(pcv_val * 1e-3);
			}
			num_azi_rd++;
			
			continue;
		}
	}

	fclose(fp);

	return 1;
}
