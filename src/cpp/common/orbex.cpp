
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

FileType OBX__()
{

}

#include <iostream>
#include <string>

using std::string;
using std::ifstream;
using std::ofstream;

#include <boost/algorithm/string/split.hpp>
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
		BOOST_LOG_TRIVIAL(error) << "Empty file";
		return 1;
	}

	// verify document type
	if (line.substr(0, 7) != "%=ORBEX")
	{
		BOOST_LOG_TRIVIAL(error) << "Not an Orbex file";
		return 2;
	}

	char* buff = &line[0];
	ver = str2num(buff, 8, 5);

	// second header line
	std::getline(fileStream, line);

	if (line.substr(0, 2) != "%%")
	{
		BOOST_LOG_TRIVIAL(error) << "Incorrect format";
		return 3;
	}

	return 0;
}

/** Read necessary information, e.g. time system & frame type, from the FILE/DESCRIPTION block
* Note: Only TIME_SYSTEM and FRAME_TYPE are read currently
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
			vector<string> split;
			boost::trim(line);
			boost::algorithm::split(split, line, boost::algorithm::is_space(), boost::token_compress_on);

			if (split[0] == "TIME_SYSTEM")
			{
				string timeSysStr = split[1];

				if		(timeSysStr == "GPS")	tsys = E_TimeSys::GPST;
				else if	(timeSysStr == "UTC")	tsys = E_TimeSys::UTC;
				else if	(timeSysStr == "TAI")	tsys = E_TimeSys::TAI;
				else if	(timeSysStr == "GAL")	tsys = E_TimeSys::GST;
				else if	(timeSysStr == "GLO")	tsys = E_TimeSys::GLONASST;
				else if	(timeSysStr == "TT" )	tsys = E_TimeSys::TT;
				else
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Unknown Orbex time system: " << timeSysStr;
					return false;
				}

				// currently only GPST and UTC are supported
				if	( tsys != +E_TimeSys::GPST
					&&tsys != +E_TimeSys::UTC)
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Unsupported time system: " << timeSysStr;
					return false;
				}
			}
			else if (split[0] == "FRAME_TYPE")
			{
				string frameTypeStr = split[1];

				try
				{
					frame = E_ObxFrame::_from_string(frameTypeStr.c_str());
				}
				catch (...)
				{
					BOOST_LOG_TRIVIAL(debug)
					<< "Unknown Orbex frame type: " << frameTypeStr;
				}

				if	( frame != +E_ObxFrame::ECEF
					&&frame != +E_ObxFrame::ECI)
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Unsupported Orbex frame type: " << frameTypeStr;
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
				<< line << " != " << closure;
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
* Note: This function is currently not fully implemented, satellite ID's are read from EPHEMERIS/DATA block
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
				<< line << " != " << closure;
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
* Note: Only ATT record type is supported currently
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
			bool error = str2time(buff, 3, 32, time, tsys);
			if (error)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Invalid epoch line in Orbex file: " << line;
				return false;
			}

			nsat = str2num(buff, 36, 3);

			index++;
		}
		else if (line[0] == ' ')
		{
			if (nsat == 0)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Epoch line invalid or not found before data records";
				return false;
			}

			string recType = line.substr(1,3);
			if (recType == "ATT")
			{
				string id = line.substr(5);
				id = id.substr(0, id.find(' '));

				Att att 	= {};
				att.time 	= time;
				att.index	= index;
				att.id		= id;
				att.frame	= frame;

				int nRec	= (int)str2num(buff, 22, 1);
				if (nRec != 4)
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Invalid number of data columns: " << nRec;
					return false;
				}

				double val[4];
				int found = sscanf(buff+24, "%lf %lf %lf %lf", &val[0], &val[1], &val[2], &val[3]);

				if (found < 4)
				{
					continue;
				}

				att.q.w() = val[0];
				att.q.x() = val[1];
				att.q.y() = val[2];
				att.q.z() = val[3];

				if (abs(att.q.norm() - 1) > 1E-6)
				{
					BOOST_LOG_TRIVIAL(warning)
					<< "The quaternion is not approximately unit norm";
					continue;
				}

				att.q.normalize();

				nav.attMapMap[att.id][att.time] = att;
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
				<< "Unsupported record type: " << recType;
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
				<< line << " != " << closure;
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
		<< "Orbex file open error " << filepath;

		return;
	}

	// header lines - each ORBEX file must begin with the two header lines
	double ver;
	int failure = readOrbexHeader(fileStream, ver);

	if (failure)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error reading Orbex header lines";
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
			<< "Closure line not found before end of Orbex file " << filepath;
			break;
		}
		else if (line == "+FILE/DESCRIPTION")				pass = readOrbexFileDesc(fileStream,		tsys, frame	);
		else if (line == "+SATELLITE/ID_AND_DESCRIPTION")	pass = readOrbexSatId	(fileStream						);
		else if (line == "+EPHEMERIS/DATA")					pass = readOrbexEph		(fileStream, nav,	tsys, frame	);
		else if (line == "%END_ORBEX")						return; // end of file

		if (pass == false)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error reading Orbex " << line << " block";
			return;
		}
	}
}
