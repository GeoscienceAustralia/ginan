
// #pragma GCC optimize ("O0")


#include <fstream>


#include "eigenIncluder.hpp"
#include "navigation.hpp"
#include "orbexWrite.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "enums.h"
#include "ppp.hpp"

#define ORBEX_VER	0.09
#define NO_PV_STD	99999.9
#define NO_CLK		9999999.9999999
#define NO_CLK_STD	9999999.999

/** ORBEX entry to write out
*/
struct OrbexEntry
{
	SatSys 		Sat			= {};							///< Satellite ID
	Vector3d	pos			= Vector3d::Zero();				///< Satellite position (m)
	Vector3d	vel			= Vector3d::Zero();				///< Satellite velocity (m/s)
	double	 	clk			= NO_CLK;						///< Satellite clock (microsecond) & clock-rate (nanosecond/s)
	double		clkVel		= NO_CLK;
	Vector3d	posStd		= Vector3d::Ones() * NO_PV_STD;	///< Satellite position std (mm)
	Vector3d	velStd		= Vector3d::Ones() * NO_PV_STD;	///< Satellite velocity std (micrometer/s)
	double		clkStd		= NO_CLK_STD;					///< Satellite clock (picosecond)
	double		clkVelStd	= NO_CLK_STD;					///< Satellite clock-rate std (femtosecond/s)
	Quaterniond	q			= {0, 0, 0, 0};					///< Satellite attitude in quaternions

	// other record types, e.g. correlation coefficients, to be added in the future
};

OrbexFileData orbexCombinedFileData;		///< Combined file editing information for ORBEX writing

/** Write ORBEX header lines and header blocks including FILE/DESCRIPTION and SATELLITE/ID_AND_DESCRIPTION block
*/
void writeOrbexHeader(
	std::fstream& 		orbexStream,	///< Output stream
	GTime				time,			///< Epoch time (GPST)
	map<E_Sys, bool>&	outSys,			///< Systems to include in file
	OrbexFileData&		outFileDat)		///< File editing information for ORBEX writing
{
	GEpoch ep = time;

	tracepdeex(0, orbexStream, "%%=ORBEX %5.2f\n", ORBEX_VER);
	tracepdeex(0, orbexStream, "%%%%\n");

	tracepdeex(0, orbexStream, "+FILE/DESCRIPTION\n");
	tracepdeex(0, orbexStream, " DESCRIPTION         %s\n", "Satellite attitude quaternions");
	tracepdeex(0, orbexStream, " CREATED_BY          %s %s\n", acsConfig.analysis_agency.c_str(), acsConfig.analysis_software.c_str());
	tracepdeex(0, orbexStream, " CREATION_DATE       %s\n", timeGet().to_string(0).c_str());
	tracepdeex(0, orbexStream, " INPUT_DATA          %s\n", "");
	tracepdeex(0, orbexStream, " CONTACT             %s\n", "npi@ga.gov.au");
	tracepdeex(0, orbexStream, " TIME_SYSTEM         %s\n", "GPS");
	tracepdeex(0, orbexStream, " START_TIME          %4.0f %2.0f %2.0f %2.0f %2.0f %15.12f\n", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	outFileDat.headerTimePos = orbexStream.tellp();

	tracepdeex(0, orbexStream, " END_TIME            %4.0f %2.0f %2.0f %2.0f %2.0f %15.12f\n", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);
	tracepdeex(0, orbexStream, " EPOCH_INTERVAL      %9.3f\n", acsConfig.orbex_output_interval);
	tracepdeex(0, orbexStream, " COORD_SYSTEM        %s\n", "IGS14");
	tracepdeex(0, orbexStream, " FRAME_TYPE          %s\n", "ECEF");
	tracepdeex(0, orbexStream, " ORBIT_TYPE          %s\n", "");
	tracepdeex(0, orbexStream, " LIST_OF_REC_TYPES  ");
	for (auto& recType : acsConfig.orbex_record_types)
	{
		tracepdeex(0, orbexStream, " %s", recType._to_string());
	}
	tracepdeex(0, orbexStream, "\n");

	tracepdeex(0, orbexStream, "-FILE/DESCRIPTION\n");

	for (auto sys : {E_Sys::GPS, E_Sys::GLO, E_Sys::GAL, E_Sys::BDS})
	{
		if (outSys[sys])
		for (auto Sat : getSysSats(sys))
		{
			outFileDat.satList[Sat] = false;
		}
	}

	Block block(orbexStream, "SATELLITE/ID_AND_DESCRIPTION");

	outFileDat.satListPos = orbexStream.tellp();

	for (auto& [sat, dummy] : outFileDat.satList)
	{
		tracepdeex(0, orbexStream, "*   \n");	// placeholders for satellite list
	}

	// optional blocks to be added in the future (not yet available in existing ORBEX files)
}

/** Write PCS or VCS records
*/
bool writePVCS(
	std::fstream& 		orbexStream,	///< Output stream
	OrbexEntry&			entry,			///< ORBEX entry to write out
	E_OrbexRecord		recType)		///< Record type
{
	Vector3d	pv		= Vector3d::Zero();	// satellite position or velocity
	Vector3d	pvStd	= Vector3d::Zero();	// satellite position or velocity std
	double		clk		= 0;				// satellite clock or clock-rate
	double		clkStd	= 0;				// satellite clock or clock-rate std

	if		(recType == +E_OrbexRecord::PCS)		{	pv = entry.pos;		pvStd = entry.posStd;	clk = entry.clk;		clkStd	= entry.clkStd;		}
	else if	(recType == +E_OrbexRecord::VCS)		{	pv = entry.vel;		pvStd = entry.velStd;	clk = entry.clkVel;		clkStd	= entry.clkVelStd;	}
	else
	{
		return false;
	}

	if (pv.isZero())
		return false; // skip if no position entries

	if (fabs(clk) >= NO_CLK)			clk			= NO_CLK;
	if (pvStd.x() >= NO_PV_STD)			pvStd.x()	= NO_PV_STD;
	if (pvStd.y() >= NO_PV_STD)			pvStd.y()	= NO_PV_STD;
	if (pvStd.z() >= NO_PV_STD)			pvStd.z()	= NO_PV_STD;
	if (clkStd    >= NO_CLK_STD)		clkStd		= NO_CLK_STD;


	// satellite flags not available at the moment
	int nRec = 8;
	tracepdeex(0, orbexStream, " %s %s              %d %16.4f %16.4f %16.4f %16.7f %7.1f %7.1f %7.1f %11.3f\n",
			recType,
			entry.Sat.id().c_str(),
			nRec,
			pv.x(),
			pv.y(),
			pv.z(),
			clk,
			pvStd.x(),
			pvStd.y(),
			pvStd.z(),
			clkStd);

	return true;
}

/** Write POS or VEL records
*/
bool writePV(
	std::fstream& 		orbexStream,	///< Output stream
	OrbexEntry&			entry,			///< ORBEX entry to write out
	E_OrbexRecord		recType)		///< Record type
{
	Vector3d	pv		= Vector3d::Zero();	// satellite position or velocity

	if		(recType == +E_OrbexRecord::POS)		pv = entry.pos;
	else if	(recType == +E_OrbexRecord::VEL)		pv = entry.vel;
	else
	{
		return false;
	}

	if (pv.isZero())
	{
		return false; // skip if no position entries
	}

	int nRec = 3;
	tracepdeex(0, orbexStream, " %s %s              %d %16.4f %16.4f %16.4f\n",
			recType,
			entry.Sat.id().c_str(),
			nRec,
			pv.x(),
			pv.y(),
			pv.z());

	return true;
}

/** Write CLK or CRT records
*/
bool writeClk(
	std::fstream& 		orbexStream,	///< Output stream
	OrbexEntry&			entry,			///< ORBEX entry to write out
	E_OrbexRecord		recType)		///< Record type
{
	double		clk		= 0;				// satellite clock or clock-rate

	if		(recType == +E_OrbexRecord::CLK)		clk = entry.clk;
	else if (recType == +E_OrbexRecord::CRT)		clk = entry.clkVel;
	else
	{
		return false;
	}

	if (fabs(clk) > NO_CLK)
		return false;

	int nRec = 1;
	tracepdeex(0, orbexStream, " %s %s              %d %16.7f\n",
			recType,
			entry.Sat.id().c_str(),
			nRec,
			clk);

	return true;
}

/** Write ATT records
*/
bool writeAtt(
	std::fstream& 		orbexStream,	///< Output stream
	OrbexEntry&			entry,			///< ORBEX entry to write out
	E_OrbexRecord		recType)		///< Record type
{
	if (recType != +E_OrbexRecord::ATT)		return false;
	if (entry.q.norm() == 0)				return false;

	int nRec = 4;
	tracepdeex(0, orbexStream, " %s %s              %d %19.16f %19.16f %19.16f %19.16f\n",
			recType,
			entry.Sat.id().c_str(),
			nRec,
			entry.q.w(),
			entry.q.x(),
			entry.q.y(),
			entry.q.z());

	return true;
}

/** Write EPHEMERIS/DATA block and update END_TIME line
*/
void updateOrbexBody(
	string&					filename,		///< File path to output file
	map<int, OrbexEntry>&	entryList,		///< List of data to print
	GTime					time,			///< Epoch time (GPST)
	map<E_Sys, bool>&		outSys,			///< Systems to include in file
	OrbexFileData&			outFileDat)		///< Current file editing information
{
	GEpoch ep = time;

	// first create if non existing
	{
		std::fstream maker(filename, std::ios::app);
	}
	std::fstream orbexStream(filename);

	if (!orbexStream)
	{
		BOOST_LOG_TRIVIAL(error) << "Error opening " << filename << " for Orbex file.";
		return;
	}

	orbexStream.seekp(0, std::ios::end);

	long endFilePos = orbexStream.tellp();

	if (endFilePos == 0)
	{
		writeOrbexHeader(orbexStream, time, outSys, outFileDat);

		tracepdeex(0, orbexStream, "+EPHEMERIS/DATA\n");
		tracepdeex(0, orbexStream, "*PCS ID_    FLAGS_    N __X______(m)____ ______Y__(m)____ ______Z___(m)___ _SVCLK___(usec)_ _STD_X_ _STD_Y_ _STD_Z_ ____STD_CLK\n");
		tracepdeex(0, orbexStream, "*VCS ID_    FLAGS_    N __VX_____(m/s)__ ______VY_(m/s)__ ______VZ__(m/s)_ _CLKRATE_(ns/s)_ _STD_VX _STD_VY _STD_VZ ____STD_CLK\n");
		tracepdeex(0, orbexStream, "*POS ID_    FLAGS_    N __X______(m)____ ______Y__(m)____ ______Z___(m)___\n");
		tracepdeex(0, orbexStream, "*VEL ID_    FLAGS_    N __VX_____(m/s)__ ______VY_(m/s)__ ______VZ__(m/s)_\n");
		tracepdeex(0, orbexStream, "*CLK ID_    FLAGS_    N __SVCLK__(usec)_\n");
		tracepdeex(0, orbexStream, "*CRT ID_    FLAGS_    N __SVCLKR_(ns/s)_\n");
		tracepdeex(0, orbexStream, "*ATT ID_    FLAGS_    N __q0_______________ ___q1______________ ____q2_____________ ____q3_____________\n");
		tracepdeex(0, orbexStream, "*ATT RECORDS: TRANSFORMATION FROM TERRESTRIAL FRAME COORDINATES (T) TO SAT. BODY FRAME ONES (B) SUCH AS\n");
		tracepdeex(0, orbexStream, "*                                 (0,B) = q.(0,T).trans(q)\n");

		//tracepdeex(0, orbexStream, "*CPC ID_    FLAGS_    N ______xy_corr____ ______xz_corr____ ______xc_corr____ ______yz_corr____ ______yc_corr____ ______zc_corr____\n");
		//tracepdeex(0, orbexStream, "*CVC ID_    FLAGS_    N ____vxvy_corr____ ____vxvz_corr____ ____vxvc_corr____ ____vyvz_corr____ ____vyvc_corr____ ____vzvc_corr____\n");
	}
	else
	{
		orbexStream.seekp(outFileDat.headerTimePos);

		tracepdeex(0, orbexStream, " END_TIME            %4.0f %2.0f %2.0f %2.0f %2.0f %15.12f\n", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

		orbexStream.seekp(outFileDat.endDataPos);
	}

	tracepdeex(0, orbexStream, "## %4.0f %2.0f %2.0f %2.0f %2.0f %15.12f", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	long numSatPos = orbexStream.tellp();

	int nsat = 0;
	tracepdeex(0, orbexStream, " %3d\n", nsat);

	for (auto& [sat, isIncluded] : outFileDat.satList)
	{
		auto it = entryList.find(sat);
		if (it == entryList.end())
			continue;

		auto& [key, entry] = *it;
		isIncluded = false;
		for (auto& recType : acsConfig.orbex_record_types)
		switch (recType)
		{
			case E_OrbexRecord::PCS:	{	isIncluded |= writePVCS	(orbexStream, entry, recType);	break;	}
			case E_OrbexRecord::VCS:	{	isIncluded |= writePVCS	(orbexStream, entry, recType);	break;	}
			case E_OrbexRecord::CPC:	{															break;	}	// to be added if needed in the future
			case E_OrbexRecord::CVC:	{															break;	}	// to be added if needed in the future
			case E_OrbexRecord::POS:	{	isIncluded |= writePV	(orbexStream, entry, recType);	break;	}
			case E_OrbexRecord::VEL:	{	isIncluded |= writePV	(orbexStream, entry, recType);	break;	}
			case E_OrbexRecord::CLK:	{	isIncluded |= writeClk	(orbexStream, entry, recType);	break;	}
			case E_OrbexRecord::CRT:	{	isIncluded |= writeClk	(orbexStream, entry, recType);	break;	}
			case E_OrbexRecord::ATT:	{	isIncluded |= writeAtt	(orbexStream, entry, recType);	break;	}
		}

		if (isIncluded)
		{
			nsat++;
		}
	}

	outFileDat.endDataPos = orbexStream.tellp();

	tracepdeex(0, orbexStream, "-EPHEMERIS/DATA\n");
	tracepdeex(0, orbexStream, "%%END_ORBEX\n");

	orbexStream.seekp(numSatPos);

	tracepdeex(0, orbexStream, " %3d\n", nsat);

	orbexStream.seekp(outFileDat.satListPos);

	for (auto& [sat, isIncluded] : outFileDat.satList)
	{
		if (isIncluded)
		{
			tracepdeex(0, orbexStream, " %3s\n", sat.id().c_str());
		}
	}
}

/** Retrieve satellite orbits, clocks and attitudes for all included systems and write out to an ORBEX file
*/
void writeSysSetOrbex(
	string				filename,			///< File path to output file
	GTime				time,				///< Epoch time (GPST)
	map<E_Sys, bool>&	outSys,				///< Systems to include in file
	OrbexFileData&		outFileDat,			///< Current file editing information
	vector<E_Source>	orbDataSrcs,		///< Data source for satellite positions & velocities
	vector<E_Source>	clkDataSrcs,		///< Data source for satellite clocks
	vector<E_Source>	attDataSrcs,		///< Data source for satellite attitudes
	KFState*			kfState_ptr)		///< Pointer to a kalman filter to take values from
{
	map<int, OrbexEntry> entryList;

	for (auto& [Sat, satNav] : nav.satNavMap)
	{
		if (outSys[Sat.sys] == false)
			continue;

		OrbexEntry entry;
		entry.Sat = Sat;

		// Create a dummy observation
		GObs obs;
		obs.Sat			= Sat;
		obs.time		= time;
		obs.satNav_ptr	= &nav.satNavMap[Sat];

		GTime teph = time;

		// satellite orbit - position and velocity + satellite clock (for PCS and VCS record only)
		bool orbPass = true;
		orbPass &= satclk(nullStream, time, time, obs, clkDataSrcs,						nav, kfState_ptr);
		orbPass &= satpos(nullStream, time, time, obs, orbDataSrcs, E_OffsetType::COM,	nav, kfState_ptr);

		if (orbPass)
		{
			entry.pos		= obs.rSatCom;
			entry.vel		= obs.satVel;
			entry.clk		= obs.satClk	* 1E6;	// microsecond
			entry.clkVel	= obs.satClkVel	* 1E9;	// nanosecond
		}

		// satellite attitude
		bool attPass = false;
		Quaterniond quat;
		if (orbPass)
		{
			updateSatYaw(obs, obs.satNav_ptr->attStatus);
			attPass = satQuat(obs, attDataSrcs, quat);
		}

		if (attPass)
		{
			if (quat.w() < 0) // convention to have +ve w
			{
				quat.w()	*= -1;
				quat.x()	*= -1;
				quat.y()	*= -1;
				quat.z()	*= -1;
			}

			entry.q	= quat.conjugate(); // satQuat() returns transformation from body to ECEF, but Orbex req's ECEF to body - i.e. quat.conjugate()
		}

		if	( orbPass
			||attPass)
		{
			entryList[Sat] = entry;
		}
	}

	updateOrbexBody(filename, entryList, time, outSys, outFileDat);
}

/** Output ORBEX files
*/
void outputOrbex(
	string				filename,		///< File to write to
	GTime				time,			///< Epoch time (GPST)
	vector<E_Source>	orbDataSrcs,	///< Data source for satellite positions & velocities
	vector<E_Source>	clkDataSrcs,	///< Data source for satellite clocks
	vector<E_Source>	attDataSrcs,	///< Data source for satellite attitudes
	KFState*			kfState_ptr)	///< Pointer to a kalman filter to take values from
{
	auto sysFilenames = getSysOutputFilenames(filename, time);

	for (auto [filename, sysMap] : sysFilenames)
	{
		writeSysSetOrbex(filename, time, sysMap, orbexCombinedFileData, orbDataSrcs, clkDataSrcs, attDataSrcs, kfState_ptr);
	}
}
