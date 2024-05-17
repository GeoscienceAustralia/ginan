
// #pragma GCC optimize ("O0")

#include "satSys.hpp"


map<SatSys, SatSys::SatData> SatSys::satDataMap =
{

};

/** Get all satellites of target system
*/
vector<SatSys> getSysSats(
	E_Sys	targetSys)			///< Target system
{
	vector<SatSys> sats;
	/* output satellite PRN*/
	if (targetSys == +E_Sys::GPS)	for (int prn = 1; prn <= NSATGPS; prn++)	{ sats.push_back(SatSys(E_Sys::GPS, prn)); }
	if (targetSys == +E_Sys::GLO)	for (int prn = 1; prn <= NSATGLO; prn++)	{ sats.push_back(SatSys(E_Sys::GLO, prn)); }
	if (targetSys == +E_Sys::GAL)	for (int prn = 1; prn <= NSATGAL; prn++)	{ sats.push_back(SatSys(E_Sys::GAL, prn)); }
	if (targetSys == +E_Sys::BDS)	for (int prn = 1; prn <= NSATBDS; prn++)	{ sats.push_back(SatSys(E_Sys::BDS, prn)); }
	if (targetSys == +E_Sys::QZS)	for (int prn = 1; prn <= NSATQZS; prn++)	{ sats.push_back(SatSys(E_Sys::QZS, prn)); }
	if (targetSys == +E_Sys::SBS)	for (int prn = 1; prn <= NSATSBS; prn++)	{ sats.push_back(SatSys(E_Sys::SBS, prn)); }
	if (targetSys == +E_Sys::LEO)	for (int prn = 1; prn <= NSATLEO; prn++)	{ sats.push_back(SatSys(E_Sys::LEO, prn)); }

	return sats;
}

void SatSys::getId(char* str) const
{	
	char sys_c = sysChar();
	
	if		(sys == +E_Sys::NONE)	sprintf(str, "");
	else if (prn == 0)				sprintf(str, "%c--",	sys_c);
	else							sprintf(str, "%c%02d",	sys_c,	prn);
}
