
#pragma once

#include <string>
#include <vector>
#include <map>

using std::string;
using std::vector;
using std::map;


#include "enums.h"

#define RAW_SBAS_PRN_OFFSET	119
#define RAW_QZSS_PRN_OFFSET	192
#define QZS_SAIF_PRN_OFFSET	182

#define NSYSGPS     1
#define NSATGPS   	32						///< potential number of GPS satellites, PRN goes from 1 to this number
#define NSATGLO   	27						///< potential number of GLONASS satellites, PRN goes from 1 to this number
#define NSATGAL   	36						///< potential number of Galileo satellites, PRN goes from 1 to this number
#define NSATQZS   	7						///< potential number of QZSS satellites, PRN goes from 1 to this number
#define NSATLEO		78                  	///< potential number of LEO satellites, PRN goes from 1 to this number
#define NSATBDS   	62						///< potential number of Beidou satellites, PRN goes from 1 to this number
#define NSATSBS   	39						///< potential number of SBAS satellites, PRN goes from 1 to this number

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
			case E_Sys::SBS:	return 'S';
			default:			return '-';
		}
	}

	void getId(char* str) const;

	/** Returns a unique id for this satellite (for use in hashes)
	*/
	operator int() const
	{
		int intval	= (sys			<< 16)
					+ (prn			<< 8);
		return intval;
	}

	static map<SatSys, SatData> satDataMap;

	void setBlockType(
		string blockType)
	{
		satDataMap[*this].block = blockType;
	}

	void setSvn(
		string svn)
	{
		satDataMap[*this].svn = svn;
	}

	string blockType() const
	{
		return satDataMap[*this].block;
	}

	string svn() const
	{
		return satDataMap[*this].svn;
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
	
	static E_Sys sysFromChar(
		char sysChar)
	{
		switch (sysChar)
		{
			case 'G': return E_Sys::GPS;	
			case 'R': return E_Sys::GLO;	
			case 'E': return E_Sys::GAL;	
			case 'J': return E_Sys::QZS;	
			case 'C': return E_Sys::BDS;	
			case 'L': return E_Sys::LEO;	
			case 'I': return E_Sys::IRN;	
			case 'S': return E_Sys::SBS;	
			default:  return E_Sys::NONE;	
		}
	}

	/** Constructs a SatSys object from a c_string id
	*/
	SatSys(const char* id)
	{
		char code;
		int prn_;

		if (sscanf(id, "%d", &prn_) == 1)
		{
			prn = prn_;
			if (1 <= prn && prn <= NSATGPS) {	sys = E_Sys::GPS;	return;}
			
			prn = prn_ - RAW_SBAS_PRN_OFFSET;
			if (1 <= prn && prn <= NSATSBS) {	sys = E_Sys::SBS;	return;}
			
			prn = prn_ - RAW_QZSS_PRN_OFFSET;
			if (1 <= prn && prn <= NSATQZS) {	sys = E_Sys::QZS;	return;}
			
			prn = prn_;							sys = E_Sys::NONE;	return;
		}
		
		int found = sscanf(id, "%c%d", &code, &prn_);
		if (found > 0)
		{
			sys = sysFromChar(code);
		}
		
		if (found > 1)
			prn = prn_;
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

