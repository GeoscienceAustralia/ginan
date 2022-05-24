
#ifndef __SATSYS_HPP_
#define __SATSYS_HPP_

#include <string>
#include <vector>
#include <map>

using std::string;
using std::vector;
using std::map;


#include "enums.h"

#define MINPRNGPS   1                   /* min satellite PRN number of GPS */
#define MAXPRNGPS   32                  /* max satellite PRN number of GPS */
#define NSATGPS     (MAXPRNGPS-MINPRNGPS+1) /* number of GPS satellites */
#define NSYSGPS     1

#define MINPRNGLO   1                   /* min satellite slot number of GLONASS */
#define MAXPRNGLO   27                  /* max satellite slot number of GLONASS */
#define NSATGLO     (MAXPRNGLO-MINPRNGLO+1) /* number of GLONASS satellites */

#define MINPRNGAL   1                   /* min satellite PRN number of Galileo */
#define MAXPRNGAL   30                  /* max satellite PRN number of Galileo */
#define NSATGAL    (MAXPRNGAL-MINPRNGAL+1) /* number of Galileo satellites */

#define MINPRNQZS   193                 /* min satellite PRN number of QZSS */
#define MAXPRNQZS   199                 /* max satellite PRN number of QZSS */
#define MINPRNQZS_S 183                 /* min satellite PRN number of QZSS SAIF */
#define MAXPRNQZS_S 189                 /* max satellite PRN number of QZSS SAIF */
#define NSATQZS     (MAXPRNQZS-MINPRNQZS+1) /* number of QZSS satellites */

#define MINPRNBDS   1                   /* min satellite sat number of BeiDou */
#define MAXPRNBDS   35                  /* max satellite sat number of BeiDou */
#define NSATBDS     (MAXPRNBDS-MINPRNBDS+1) /* number of BeiDou satellites */

#define MINPRNLEO   1                   /* min satellite sat number of LEO */
#define MAXPRNLEO   10                  /* max satellite sat number of LEO */
#define NSATLEO     (MAXPRNLEO-MINPRNLEO+1) /* number of LEO satellites */

#define MINPRNSBS   120                 /* min satellite PRN number of SBAS */
#define MAXPRNSBS   142                 /* max satellite PRN number of SBAS */
#define NSATSBS     (MAXPRNSBS-MINPRNSBS+1) /* number of SBAS satellites */

/** Object holding satellite id, and providing related functions
*/
struct SatSys
{
	E_Sys		sys = E_Sys::NONE;		///< Satellite system
	short int	prn = 0;				///< PRN for this satellite

	/** Constructor using satellite system and prn
	*/
	SatSys(E_Sys _sys = E_Sys::NONE, int _prn = 0)
	: sys(_sys)
	, prn(_prn)
	{

	}

	struct SatData
	{
		string	block;
		string 	svn;
	};


	/** Returns the bias group associated with this satellite.
	* Receivers may combine multiple satellite systems on a single internal clock, each with their own bias.
	* The biases defined by this function are the basis for the different clocks calculated in this software
	*/
	short int biasGroup()
	{
		switch (sys)
		{
			default:			return E_BiasGroup::GPS;
			case E_Sys::GLO:	return E_BiasGroup::GLO;
			case E_Sys::GAL:	return E_BiasGroup::GAL;
			case E_Sys::BDS:	return E_BiasGroup::BDS;
		}
	}

#define SBAS_CHAR 's'
	/** Returns the character used as a prefix for this system.
	*/
	char sysChar()	const
	{
		switch (sys)
		{
			case E_Sys::GPS:	return 'G';
			case E_Sys::GLO:	return 'R';
			case E_Sys::GAL:	return 'E';
			case E_Sys::QZS:	return 'J';
			case E_Sys::BDS:	return 'C';
			case E_Sys::LEO:	return 'L';
			case E_Sys::IRN:	return 'I';
			case E_Sys::SBS:	return SBAS_CHAR;
			default:			return '-';
		}
	}

	/** Sets a 4 character c_string of this satellite's id.
	*/
	void getId(char* str) const
	{
		char sys_c = sysChar();
		if (sys_c == SBAS_CHAR)		sprintf(str, "%03d", 			prn);
		else						sprintf(str, "%c%02d",	sys_c,	prn);
	}

	/** Returns a unique id for this satellite (for use in hashes)
	*/
	operator int() const
	{
		int intval	= (sys			<< 16)
					+ (prn			<< 8);
		return intval;
	}

	static map<SatSys, SatData> SatDataMap;

	void setBlockType(
		string blockType)
	{
		SatDataMap[*this].block = blockType;
	}

	void setSvn(
		string svn)
	{
		SatDataMap[*this].svn = svn;
	}

	string blockType() const
	{
		auto iterator = SatDataMap.find(*this);
		if (iterator == SatDataMap.end())
		{
			return "";
		}
		else
		{
			auto& satData = iterator->second;
			return satData.block;
		}
	}

	string svn() const
	{
		auto iterator = SatDataMap.find(*this);
		if (iterator == SatDataMap.end())
		{
			return "";
		}
		else
		{
			auto& satData = iterator->second;
			return satData.svn;
		}
	}

	/** Constructs a SatSys object from it's hash uid
	*/
	void fromHash(int intval)
	{
		sys = E_Sys::_from_integral((intval >> 16)	& 0xFF);
		prn = 						(intval >> 8)	& 0xFF;
	}

	/** Returns a std::string of this satellite's id
	*/
	string id() const
	{
		char cstring[5];
		getId(cstring);
		string str = cstring;
		if (str != "-00")
			return str;
		else
			return "";
	}

	/** Returns a string of this satellite's id
	*/
	operator string() const
	{
		return id();
	}

	/** Constructs a SatSys object from a c_string id
	*/
	SatSys(const char *id)
	{
		char code;
		int prn_;

		if (sscanf(id,"%d",&prn_)==1)
		{
			prn = prn_;
			if      (MINPRNGPS <= prn && prn <= MAXPRNGPS) sys = E_Sys::GPS;
			else if (MINPRNSBS <= prn && prn <= MAXPRNSBS) sys = E_Sys::SBS;
			else if (MINPRNQZS <= prn && prn <= MAXPRNQZS) sys = E_Sys::QZS;
			else return;
			return;
		}
		int found = sscanf(id,"%c%d",&code,&prn_);
		if (found > 0)
		{
			switch (code)
			{
				case 'G': sys = E_Sys::GPS; break;
				case 'R': sys = E_Sys::GLO; break;
				case 'E': sys = E_Sys::GAL; break;
				case 'J': sys = E_Sys::QZS; break;
				case 'C': sys = E_Sys::BDS; break;
				case 'L': sys = E_Sys::LEO; break;
				case 'I': sys = E_Sys::IRN; break;
				case 'S': sys = E_Sys::SBS; prn+=100; break;
				default:  sys = E_Sys::NONE; return;	//todo aaron, if this returns none, bad things happen?
			}
		}
		if (found > 1)
			prn = prn_;
		return;
	}

	/* Returns a string of this satellite's system id
	*/
	string sysName() const
	{
		return sys._to_string();
	}


	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		int sysInt = sys;
		ar & sysInt;
		ar & prn;
		try
		{
			sys = E_Sys::_from_integral(sysInt);
		}
		catch (...) {}
	}
};

namespace std
{
	template<> struct hash<SatSys>
	{
		size_t operator()(SatSys const& Sat) const
		{
			size_t hashval	= hash<size_t>	{}(Sat);
			return hashval;
		}
	};
}

vector<SatSys> getSysSats(
	E_Sys	targetSys);

#endif
