#ifndef __WRITEPOSCLKSP3_HPP
#define __WRITEPOSCLKSP3_HPP

#include <string>
#include <set>

using std::string;
using std::set;

#include "rinexClkWrite.hpp"

struct GTime;
class E_Ephemeris;
struct KFState;

struct Sp3FileData
{
	set<SatSys> sats			= {};
	long 		numEpoch		= 0;
	long 		numEpoch_pos	= 0;
};

void writeSysSetSp3(
	string			filename,
	GTime			time,
	OutSys			outSys,
	Sp3FileData&	outFileDat,
	E_Ephemeris		sp3DataSrc,	
	KFState*		kfState_ptr = nullptr);

void outputSp3(
	GTime		time,
	E_Ephemeris	sp3DataSrc,	
	KFState*	kfState_ptr = nullptr);
#endif
