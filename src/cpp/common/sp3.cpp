
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

/**
 */
FileType SP3__()
{

}

#include <iostream>
#include <string>

using std::string;

#include <boost/log/trivial.hpp>

#include "eigenIncluder.hpp"

#include "navigation.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "gTime.hpp"
#include "enums.h"


/** satellite code to satellite system
*/
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
	vector<Peph>&	pephList,		///< vector of precise ephemerides for one epoch
	int				opt,			///< options options (1: only observed + 2: only predicted + 4: not combined)
	E_TimeSys&		tsys,			///< time system
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

			bool error = str2time(buff, 3, 28, time, tsys);
			if (error)
			{
				printf("\nInvalid epoch line in sp3 file %s\n", line.c_str());
				return false;
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

			SatSys Sat(sys, prn);
			if (!Sat)
				continue;

			Peph peph 	= {};
			peph.time 	= time;
			peph.index	= index;
			peph.Sat	= Sat;
			bool valid	= true;

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
					if	( val != 0)
					{
						peph.pos[j] = val * 1000;
					}
					else
					{
						valid = false;
					}

					double base = bfact[0];
					if	(  base	> 0
						&& std	> 0)
					{
						peph.posStd[j] = pow(base, std) * 1E-3;
					}
				}
				else if (valid)
				{
// 					/* velocity */
// 					if	( val !=0)
// 					{
// 						peph.vel[j] = val * 0.1;
// 					}
//
// 					double base = bfact[j < 3 ? 0 : 1];
// 					if	(  base	> 0
// 						&& std	> 0)
// 					{
// 						peph.velStd[j] = pow(base, std) * 1E-7;
// 					}
				}
			}

			//clocks / rates
			for (int j = 3; j < 4; j++)
			{
				/* read option for predicted value */
				if (j == 3	&& (opt&1)	&&  pred_c)		continue;
				if (j == 3	&& (opt&2)	&& !pred_c)		continue;

				string checkValue;
				checkValue.assign(buff + 4 + j * 14, 7);
				double val = str2num(buff, 4	+ j * 14,	14);
				double std = str2num(buff,61	+ j * 3,	3);

				if (buff[0] == 'P')
				{
					/* clock */
					if	( val != 0
						&&checkValue != " 999999")
					{
						peph.clk = val * 1E-6;
					}
					else
					{
						peph.clk = INVALID_CLOCK_VALUE;
// 						valid = false;	//allow clocks to be invalid
					}

					double base = bfact[1];
					if	(  base	> 0
						&& std	> 0)
					{
						peph.clkStd = pow(base, std) * 1E-12;
					}
				}
				else if (valid)
				{
// 					/* clock rate */
// 					if	( val !=0
// 						&&fabs(val) < NO_SP3_CLK)
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
			}

			continue;
		}
		/*
	Quick and dirty read of the velocities
	@todo change later.
		*/
		if (buff[0] == 'V')
		{
			for (int i=0; i < 3; i++)
			{
				double val = str2num(buff, 4 + i * 14, 14);
				pephList.back().vel[i] = val * 0.1;
			}
		}
		if (buff[0] == '#')
		{
			hashCount++;

			if (hashCount == 1)
			{
				//first line is time and type
// 				type = buff[2];
				int error = str2time(buff, 3, 28, time);	// time system unknown at beginning but does not matter
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
				string timeSysStr = line.substr(9, 3);

				if		(timeSysStr == "GPS")	tsys = E_TimeSys::GPST;
				else if	(timeSysStr == "GLO")	tsys = E_TimeSys::GLONASST;
				else if	(timeSysStr == "GAL")	tsys = E_TimeSys::GST;
				else if	(timeSysStr == "QZS")	tsys = E_TimeSys::QZSST;
				else if	(timeSysStr == "TAI")	tsys = E_TimeSys::TAI;
				else if	(timeSysStr == "UTC")	tsys = E_TimeSys::UTC;
				else
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Unknown sp3 time system: " << timeSysStr;
					return false;
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
	string&		file,
	Navigation&	nav,
	int			opt)
{
	std::ifstream fileStream(file);
	if (!fileStream)
	{
		printf("\nSp3 file open error %s\n", file.c_str());
		return;
	}

	vector<Peph>	pephList;

	E_TimeSys	tsys	= E_TimeSys::NONE;
	double	bfact[2]	= {};
	while (readsp3(fileStream, pephList, opt, tsys, bfact))
	{
		//keep reading until it fails
		for (auto& peph : pephList)
		{
			nav.pephMap[peph.Sat.id()][peph.time] = peph;
		}
		pephList.clear();
	}
}
