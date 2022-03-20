
// #pragma GCC optimize ("O0")

#include <iostream>
#include <string>

using std::string;

#include <boost/log/trivial.hpp>

#include "eigenIncluder.hpp"

#include "streamTrace.hpp"
#include "navigation.hpp"
#include "gTime.hpp"
#include "common.hpp"
#include "enums.h"


/* satellite code to satellite system ----------------------------------------*/
E_Sys code2sys(char code)
{
	if (code=='G'||code==' ') return E_Sys::GPS;
	if (code=='R') return E_Sys::GLO;
	if (code=='E') return E_Sys::GAL; /* extension to sp3-c */
	if (code=='J') return E_Sys::QZS; /* extension to sp3-c */
	if (code=='C') return E_Sys::BDS; /* extension to sp3-c */
	if (code=='L') return E_Sys::LEO; /* extension to sp3-c */
	return E_Sys::NONE;
}


/** read an epoch of data from an sp3 precise ephemeris file
 */
bool readsp3(
	std::istream&	fileStream, 	///< stream to read content from
	list<Peph>&		pephList,		///< list of precise ephemerides for one epoch
	int				opt,			///< options options (1: only observed + 2: only predicted + 4: not combined)
	bool&			isUTC,			///< reported utc state in header
	double*			bfact)			///< bfact values from header
{
	GTime	time			= {};

	//fprintf(stdout,"\n SP3READ: Expanded  %s to %d files\n",file,n);

	//keep track of file number
	static int index = 0;
	index++;
	
	
	int hashCount	= 0;
	int cCount		= 0;
	int fCount		= 0;
	
	bool epochFound = false;
	string line;
	while (fileStream)
	{	
		//return early when an epoch is complete
		int peek = fileStream.peek();
		if	(  peek == '*'
			&& epochFound)
		{
			return true;
		}
		
		getline(fileStream, line);
		
		char* buff = &line[0];
		
		if (buff[0] == '*')
		{
			//epoch line
			epochFound = true;
			
			bool error = str2time(buff, 3, 28, time);
			if (error)
			{
				printf("\nInvalid epoch line in sp3 file %s\n", line.c_str());
				return false;
			}
			
			if (isUTC)
			{
				time = utc2gpst(time); /* utc->gpst */
			}
			continue;
		}
		
		if (buff[0] == 'P')
		{
			//position line
			bool pred_p = false;
			bool pred_c = false;
			
			E_Sys sys = code2sys(buff[1]);
			int prn = (int)str2num(buff, 2, 2);
			
			if		(sys == +E_Sys::SBS)	prn += 100;
			else if (sys == +E_Sys::QZS)	prn += 192; /* extension to sp3-c */

			SatSys Sat;
			Sat.sys = sys;
			Sat.prn = prn;
			if (!Sat)
				continue;

			Peph peph 	= {};
			peph.time 	= time;
			peph.index	= index;
			peph.Sat	= Sat;
			bool valid	= false;

			if (buff[0] == 'P')
			{
				pred_c = strlen(buff)>=76 && buff[75]=='P';
				pred_p = strlen(buff)>=80 && buff[79]=='P';
			}
			
			//positions/rates
			for (int j = 0; j < 3; j++)
			{
				/* read option for predicted value */
				if (j < 3 	&& (opt&1)	&&  pred_p)		continue;
				if (j < 3 	&& (opt&2)	&& !pred_p)		continue;

				double val = str2num(buff, 4	+ j * 14,	14);
				double std = str2num(buff,61	+ j * 3,	2);

				if (buff[0]=='P')
				{ 
					/* position */
					if	( val != 0
						&&fabs(val - 999999.999999) >= 1E-6)
					{
						peph.Pos[j] = val * 1000;
						valid = true;
					}
					
					double base = bfact[0];
					if	(  base	> 0
						&& std	> 0)
					{
						peph.PosStd[j] = pow(base, std) * 1E-3;
					}
				}
				else if (valid)
				{
// 					/* velocity */
// 					if	( val !=0
// 						&&fabs(val - 999999.999999) >= 1E-6)
// 					{
// 						peph.Vel[j] = val * 0.1;
// 					}
// 					
// 					double base = bfact[j < 3 ? 0 : 1];
// 					if	(  base	> 0
// 						&& std	> 0)
// 					{
// 						peph.VelStd[j] = pow(base, std) * 1E-7;
// 					}
				}
			}
			
			//clocks / rates
			for (int j = 3; j < 4; j++)
			{
				/* read option for predicted value */
				if (j == 3	&& (opt&1)	&&  pred_c)		continue;
				if (j == 3	&& (opt&2)	&& !pred_c)		continue;

				double val = str2num(buff, 4	+ j * 14,	14);
				double std = str2num(buff,61	+ j * 3,	3);

				if (buff[0] == 'P')
				{ 
					/* position */
					if	( val != 0
						&&fabs(val - 999999.999999) >= 1E-6)
					{
						peph.Clk = val * 1E-6;
						valid = 1; /* valid epoch */
					}
					
					double base = bfact[1];
					if	(  base	> 0
						&& std	> 0)
					{
						peph.ClkStd = pow(base, std) * 1E-12;
					}
				}
				else if (valid)
				{
// 					/* velocity */
// 					if	( val !=0
// 						&&fabs(val - 999999.999999) >= 1E-6)
// 					{
// 						peph.dCk = val * 1E-10;
// 					}
// 					
// 					double base = bfact[j < 3 ? 0 : 1];
// 					if	(  base	> 0
// 						&& std	> 0)
// 					{
// 						peph.dCkStd = pow(base, std) * 1E-16;
// 					}
				}
			}
			
			if (valid)
			{
				pephList.push_back(peph);
// 				nav->pephMap[peph.Sat][peph.time] = peph;
			}
			
			continue;
		}
		
		if (buff[0] == '#')
		{
			hashCount++;
			
			if (hashCount == 1) 
			{
				//first line is time and type
// 				type = buff[2];
				int error = str2time(buff, 3, 28, time);
				if (error) 
					return false;
				
				continue;
			}
		}
		
		string twoChars = line.substr(0,2);
		
// 		if (twoChars == "+ ")
// 		{
//			plusCount++;
// 			//number and list of satellites included in the file - information only, sat ids are included in epoch lines..
// 			if (lineNum == 2) 
// 			{
// 				ns = (int)str2num(buff,4,2);
// 			}
// 			for (int j = 0; j < 17 && k < ns; j++) 
// 			{
// 				E_Sys sys=code2sys(buff[9+3*j]);
// 				
// 				int prn = (int)str2num(buff,10+3*j,2);
// 				
// 				if (k < MAXSAT) 
// 					sats[k++] = SatSys(sys, prn);
// 			}
//			continue;
// 		}

// 		if (twoChars == "++")
// 		{
// 			pplusCount++;	
// 			continue;
// 		}

		if (twoChars == "%c")
		{
			cCount++;
			
			if (cCount == 1)
			{
				char tsys[4] = "";
				strncpy(tsys, buff+9,3); 
				tsys[3] = '\0';
				
				if (!strcmp(tsys, "UTC"))
				{
					isUTC = true;
// 					time = utc2gpst(time); /* utc->gpst */
				}
			}
			continue;
		}
		
		if (twoChars == "%f")
		{
			fCount++;
			
			if (fCount == 1)
			{
				bfact[0] = str2num(buff, 3,10);
				bfact[1] = str2num(buff,14,12);
			}
			continue;
		}
		
		if (line.substr(0,3) == "EOF")
		{
			//all done
			return true;
		}
	}
	
// 	printf("\nDidnt find eof in sp3 file\n");
	return false;
}


void readSp3ToNav(
	string&	file, 
	nav_t*	nav, 
	int		opt)
{
	std::ifstream fileStream(file);
	if (!fileStream)
	{
		printf("\nSp3 file open error %s\n", file.c_str());
	}
	
	list<Peph>	pephList;
	
	bool	isUTC		= false;
	double	bfact[2]	= {};
	while (readsp3(fileStream, pephList, opt, isUTC, bfact))
	{
		//keep reading until it fails
		for (auto& peph : pephList)
		{
			nav->pephMap[peph.Sat][peph.time] = peph;
		}
		pephList.clear();
	}
}


void orb2sp3(
	nav_t& nav)
{
	for (auto& [SatId,	satNav]	: nav.satNavMap)
	for (auto& [time,	orbitInfo]	: satNav.satOrbit.orbitInfoMap)
	{
		SatSys Sat;
		Sat.fromHash(SatId);
		
		Peph peph = {};

		peph.index			= 0;
		peph.time			= time;
		peph.Sat			= Sat;

		/* copy itrf coordinates */
		for (int k = 0; k < 3; k++)
		{
			peph.Pos[k] = orbitInfo.xtrf[k];
		}

		nav.pephMap[Sat][time] = peph;
	}
}
