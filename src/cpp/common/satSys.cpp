

#include "satSys.hpp"


map<SatSys, SatSys::SatData> SatSys::SatDataMap =
{

};

/** Get all satellites of target system
*/
vector<SatSys> getSysSats(
	E_Sys	targetSys)			///< Target system
{
	vector<SatSys> sats;
	/* output satellite PRN*/
	if (targetSys == +E_Sys::GPS)	for (int prn = MINPRNGPS; prn <= MAXPRNGPS; prn++)	{ sats.push_back(SatSys(E_Sys::GPS, prn)); }
	if (targetSys == +E_Sys::GLO)	for (int prn = MINPRNGLO; prn <= MAXPRNGLO; prn++)	{ sats.push_back(SatSys(E_Sys::GLO, prn)); }
	if (targetSys == +E_Sys::GAL)	for (int prn = MINPRNGAL; prn <= MAXPRNGAL; prn++)	{ sats.push_back(SatSys(E_Sys::GAL, prn)); }
	if (targetSys == +E_Sys::BDS)	for (int prn = MINPRNBDS; prn <= MAXPRNBDS; prn++)	{ sats.push_back(SatSys(E_Sys::BDS, prn)); }

	return sats;
}