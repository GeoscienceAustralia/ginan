
#include <iostream>
#include <string>

using std::string;
using std::ifstream;
using std::ofstream;

#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

#include "eigenIncluder.hpp"
#include "navigation.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "enums.h"


/** Satellite code to satellite system
*/
E_Sys code2sys(char code);

/** Read and check the two header lines from an orbex file
*/
int readOrbexHeader(
	ifstream&	fileStream,	///< Stream to read content from
	double&		ver)		///< ORBEX version
{
	ver = 0;

	string line;

	// first header line
	std::getline(fileStream, line);

	if (fileStream.eof())
	{
		BOOST_LOG_TRIVIAL(error) << "Empty file" << std::endl;
		return 1;
	}

	// verify document type
	if (line.substr(0, 7) != "%=ORBEX")
	{
		BOOST_LOG_TRIVIAL(error) << "Not an Orbex file" << std::endl;
		return 2;
	}
	
	char* buff = &line[0];
	ver = str2num(buff, 8, 5);

	// second header line
	std::getline(fileStream, line);

	if (line.substr(0, 2) != "%%")
	{
		BOOST_LOG_TRIVIAL(error) << "Incorrect format" << std::endl;
		return 3;
	}

	return 0;
}

/** Read necessary information, e.g. time system & frame type, from the FILE/DESCRIPTION block
*/
bool readOrbexFileDesc(
	ifstream&	fileStream,	///< Stream to read content from
	E_TimeSys&	tsys,		///< Time system
	E_ObxFrame&	frame)		///< Frame type
{
	tsys	= E_TimeSys::NONE;
	frame	= E_ObxFrame::OTHER;

	while (fileStream)
	{
		string line;
		
		getline(fileStream, line);
	
		if (line[0] == ' ')
		{
			if (line.substr(1, 11) == "TIME_SYSTEM")
			{
				string timeSysStr = line.substr(21, 3);
				if		(timeSysStr == "GPS")	tsys = E_TimeSys::GPST;
				else if	(timeSysStr == "UTC")	tsys = E_TimeSys::UTC;
				else if	(timeSysStr == "TAI")	tsys = E_TimeSys::TAI;
				else if	(timeSysStr == "GAL")	tsys = E_TimeSys::GST;
				else if	(timeSysStr == "GLO")	tsys = E_TimeSys::GLONASST;
				else if	(timeSysStr == "TT ")	tsys = E_TimeSys::TT;
				else
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Unknown Orbex time system: " << timeSysStr << std::endl;
					return false;
				}

				// currently only GPST and UTC are supported
				if	( tsys != +E_TimeSys::GPST
					&&tsys != +E_TimeSys::UTC)
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Unsupported time system: " << timeSysStr << std::endl;
					return false;
				}
			}
			else if (line.substr(1, 10) == "FRAME_TYPE")
			{
				string frameTypeStr = line.substr(21, 20);
				boost::trim(frameTypeStr);

				try
				{
					frame = E_ObxFrame::_from_string(frameTypeStr.c_str());
				}
				catch (...)
				{
					BOOST_LOG_TRIVIAL(debug)
					<< "Unknown Orbex frame type: " << frameTypeStr << std::endl;
				}

				if	( frame != +E_ObxFrame::ECEF
					&&frame != +E_ObxFrame::ECI)
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Unsupported Orbex frame type: " << frameTypeStr << std::endl;
					return false;
				}
			}
		}
		else if (line[0] == '-')
		{
			// end of block
			string closure = "-FILE/DESCRIPTION";
			if (line != closure)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Incorrect block closure line encountered: "
				<< line << " != " << closure << std::endl;
				return false;
			}
			else
			{
				return true;
			}
		}
	}

	return false;
}

/** Read SATELLITE/ID_AND_DESCRIPTION block
*/
bool readOrbexSatId(
	ifstream&	fileStream)	///< Stream to read content from
{
	while (fileStream)
	{
		string line;
		
		getline(fileStream, line);
	
		if (line[0] == '-')
		{
			// end of block
			string closure = "-SATELLITE/ID_AND_DESCRIPTION";
			if (line != closure)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Incorrect block closure line encountered: "
				<< line << " != " << closure << std::endl;
				return false;
			}
			else
			{
				return true;
			}
		}
	}

	return false;
}

/** Read EPHEMERIS/DATA block
*/
bool readOrbexEph(
	ifstream&	fileStream,						///< Stream to read content from
	Navigation&	nav,							///< Navigation data
	E_TimeSys	tsys	= E_TimeSys::GPST,		///< Time system
	E_ObxFrame	frame	= E_ObxFrame::ECEF)		///< Frame type
{
	GTime	time	= {};
	int		nsat	= 0;

	static int index = 0; // keep track of file number
	index++;
	
	while (fileStream)
	{
		string line;
		
		getline(fileStream, line);

		char* buff = &line[0];
	
		if	( line[0] == '#'
			&&line[1] == '#')
		{
			bool error = str2time(buff, 3, 32, time);
			if (error)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Invalid epoch line in Orbex file: " << line << std::endl;
				return false;
			}
			
			if (tsys == +E_TimeSys::UTC)
			{
				UtcTime utcTime;
				utcTime.bigTime	= time.bigTime;
				
				time = utcTime; 
			}

			nsat = str2num(buff, 36, 3);

			index++;
		}
		else if (line[0] == ' ')
		{
			if (nsat == 0)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Epoch line invalid or not found before data records" << std::endl;
				return false;
			}

			string recType = line.substr(1,3);
			if (recType == "ATT")
			{
				E_Sys sys = code2sys(buff[5]);
				int prn = (int)str2num(buff, 6, 2);
			
				SatSys Sat(sys, prn);
				if (!Sat)
					continue;

				Att att 	= {};
				att.time 	= time;
				att.index	= index;
				att.Sat		= Sat;
				att.frame	= frame;

				int nRec	= (int)str2num(buff, 22, 1);
				if (nRec != 4)
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Invalid number of data columns: " << nRec << std::endl;
					return false;
				}
				
				att.q.w() = str2num(buff, 24, 19);
				att.q.x() = str2num(buff, 44, 19);
				att.q.y() = str2num(buff, 64, 19);
				att.q.z() = str2num(buff, 84, 19);

				att.q.normalize();
				if (Sat.sys == +E_Sys::GAL)
				{
					// rotate from original GAL frame to ANTEX frame (180deg around local Z+) (https://www.gsc-europa.eu/support-to-developers/galileo-satellite-metadata#3.2)
					Eigen::Quaterniond rotZ(0, 0, 0, 1);
					att.q = rotZ * att.q;
				}

				nav.attMapMap[att.Sat][att.time] = att;
			}
			// other record types to be added here, e.g.
			/*
			else if (recType == "PCS")
			{
				...
			}
			*/
			else
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Unsupported record type: " << recType << std::endl;
				return false;
			}
		}
		else if (line[0] == '-')
		{
			// end of block
			string closure = "-EPHEMERIS/DATA";
			if (line != closure)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Incorrect block closure line encountered: "
				<< line << " != " << closure << std::endl;
				return false;
			}
			else
			{
				return true;
			}
		}
	}

	return false;
}

/** Read an ORBEX file into navigation data struct
*/
void  readOrbex(
	string			filepath,	///< File path to output file
	Navigation&		nav)		///< Navigation data
{
	ifstream fileStream(filepath);
	if (!fileStream)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Orbex file open error " << filepath << std::endl;
	}

	// header lines - each ORBEX file must begin with the two header lines
	double ver;
	int failure = readOrbexHeader(fileStream, ver);

	if (failure)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error reading Orbex header lines" << std::endl;
		return;
	}

	E_TimeSys	tsys	= E_TimeSys::NONE;
	E_ObxFrame	frame	= E_ObxFrame::OTHER;

	while (fileStream)
	{
		string line;
		getline(fileStream, line);
		line = line.substr(0, line.find(' '));
		if (line.back() == '\r')
			line.pop_back();
		bool pass = true;

		if	(!fileStream)
		{
			// unexpected end of file
			BOOST_LOG_TRIVIAL(error)
			<< "Closure line not found before end of Orbex file " << filepath << std::endl;
			break;
		}
		else if (line == "+FILE/DESCRIPTION")				pass = readOrbexFileDesc(fileStream,		tsys, frame	);
		else if (line == "+SATELLITE/ID_AND_DESCRIPTION")	pass = readOrbexSatId	(fileStream						);
		else if (line == "+EPHEMERIS/DATA")					pass = readOrbexEph		(fileStream, nav,	tsys, frame	);
		else if (line == "%END_ORBEX")						return; // end of file
		
		if (pass == false)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error reading Orbex " << line << " block" << std::endl;
			return;
		}
	}

	return;
}
