
#ifndef __SATSYS_HPP_
#define __SATSYS_HPP_

#include <string>
#include <map>

using std::string;
using std::map;

#include "constants.h"
#include "enums.h"

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
	* Receivers may combine multiple satellite systems on a single internal clock, each wih their own bias.
	* The biases defined by this function are the basis for the different clocks calculated in this software
	*/
	short int biasGroup()
	{
		switch (sys)
		{
			default:			return BiasGroup::GPS;
			case E_Sys::GLO:	return BiasGroup::GLO;
			case E_Sys::GAL:	return BiasGroup::GAL;
			case E_Sys::CMP:	return BiasGroup::BDS;
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
			case E_Sys::CMP:	return 'C';
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
				case 'C': sys = E_Sys::CMP; break;
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
#endif
