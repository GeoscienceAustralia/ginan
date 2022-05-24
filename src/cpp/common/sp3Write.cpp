
// #pragma GCC optimize ("O0")


#include <fstream>


#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "GNSSambres.hpp"
#include "navigation.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "sp3Write.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "enums.h"

/* macro defintions */
#define             VERSION             3.00

struct Sp3Entry
{
	SatSys 		sat			= {};
	Vector3d	satPos		= Vector3d::Zero();		// Satellite position.
	Vector3d	satVel		= Vector3d::Zero();		// Satellite velocity.
	double	 	clock[2]	= {};					// Mean clock delta reference.
	double		sigma		= 0;
};

typedef map<int, Sp3Entry> Sp3SatList;


Sp3FileData sp3CombinedFileData;
map<E_Sys, Sp3FileData> sp3SytemFileData;

void writeSp3Header(
	std::fstream& 		sp3Stream,
	Sp3SatList&			entryList,
	GTime				time,
	OutSys				outSys,
	Sp3FileData&		outFileDat)
{
	double ep[6] = {};
	time2epoch(time, ep);
	
	outFileDat = {};
	outFileDat.numEpoch = 1;
	
	// note "#dV" for velocity and position.
	if (acsConfig.output_orbit_velocities)		tracepdeex(0, sp3Stream, "#dV%4.0f %2.0f %2.0f %2.0f %2.0f %11.8f ", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);
	else										tracepdeex(0, sp3Stream, "#dP%4.0f %2.0f %2.0f %2.0f %2.0f %11.8f ", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	outFileDat.numEpoch_pos = sp3Stream.tellp();

	//TODO Check, coordinate system and Orbit Type from example product file.
	tracepdeex(0, sp3Stream, "%7d ORBIT IGS14 HLM %4s\n",   outFileDat.numEpoch,   acsConfig.analysis_agency.c_str());

	int week;
	double tow_sec = time2gpst(time, &week);
	double mjdate = 7.0 * week + tow_sec / 86400.0 + 44244.0;
	tracepdeex(0, sp3Stream, "## %4d %15.8f %14.8f %5.0f %15.13f\n",
			   week,
			   tow_sec,
			   acsConfig.epoch_interval,
			   mjdate,
			   mjdate - floor(mjdate));

	for (auto sys : {E_Sys::GPS, E_Sys::GLO, E_Sys::GAL, E_Sys::BDS})
	{
		if (outSys[sys])	
		for (auto Sat : getSysSats(E_Sys::GPS))
		{
			outFileDat.sats.insert(Sat);
		}
	}

	int lineNumber	= 1;
	int lineEntries	= 0;
	tracepdeex(0, sp3Stream, "+  %3d   ", outFileDat.sats.size());
	for (auto& sat : outFileDat.sats)
	{
		if (lineEntries	== 17)
		{
			//start a new line
			tracepdeex(0, sp3Stream, "\n+        ");
			lineEntries = 0;
			lineNumber++;
		}
		tracepdeex(0, sp3Stream, "%3s", sat.id().c_str());
		
		lineEntries++;
	}

	while	( lineNumber	< 17 
			||lineEntries	< 17)
	{
		//keep adding entries and lines until we reach the minimum
		
		if (lineEntries	== 17)
		{
			//start a new line
			tracepdeex(0, sp3Stream, "\n+        ");
			lineEntries = 0;
			lineNumber++;
		}
		tracepdeex(0, sp3Stream, "%3s", "0");
		
		lineEntries++;
	}
	tracepdeex(0, sp3Stream, "\n");
	
	// ++ line entries one per satellite sigma = 2^val in millimeters.
	lineNumber	= 1;
	lineEntries	= 0;
	tracepdeex(0, sp3Stream, "++       ");
	for (auto& sat : outFileDat.sats)
	{
		if (lineEntries == 17)
		{
			//start a new line
			tracepdeex(0, sp3Stream, "\n++       ");
			lineEntries = 0;
		}

		auto it = entryList.find(sat);
		if (it == entryList.end())
		{
			// Accuracy unknown.
			tracepdeex(0, sp3Stream, "  0");
		}
		else
		{
			auto& [key, entry] = *it;
			
			if (entry.sigma == 0)
			{
				// Accuracy unknown.
				tracepdeex(0, sp3Stream, "  0");
			}
			else
			{
				// Accuracy sigma = 2^(Accuracy) in millimeters.
				//TODO Not sure if ceil or round is correct needs checking.
				double val = std::ceil(std::log2(entry.sigma * 1000));
				tracepdeex(0, sp3Stream, "%3.0f", val);
			}
		}
		
		lineEntries++;
	}

	while	( lineNumber	< 17 
			||lineEntries	< 17)
	{
		//keep adding entries and lines until we reach the minimum
		
		if (lineEntries	== 17)
		{
			//start a new line
			tracepdeex(0, sp3Stream, "\n++       ");
			lineEntries = 0;
			lineNumber++;
		}
		tracepdeex(0, sp3Stream, "%3s", "0");
		
		lineEntries++;
	}
	tracepdeex(0, sp3Stream, "\n");
	
	char syschar = 0;
	for (auto& sat : outFileDat.sats)
	{
		if (syschar == 0)					{			syschar = sat.sysChar();			continue;		}
		if (syschar != sat.sysChar())		{			syschar = 'M';						break;			}
	}
	if (syschar == 0)
	{
		BOOST_LOG_TRIVIAL(error) << "Writing RINEX clock file no systems in process_sys.";
		return;
	}

	// Using GPS time.
	tracepdeex(0, sp3Stream, "%%c %c  cc GPS ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc\n",   syschar);
	tracepdeex(0, sp3Stream, "%%c cc cc ccc ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc\n");

	// first variable is the base for the sigma of position, x, y, z, sigma = 1.25^val in millimeters.
	// second variable is the base for the sigma of time sigma = 1.025^val in picoseconds.
	tracepdeex(0, sp3Stream, "%%f  1.2500000  1.025000000  0.00000000000  0.000000000000000\n");
	tracepdeex(0, sp3Stream, "%%f  0.0000000  0.000000000  0.00000000000  0.000000000000000\n");

	// float variable lines, unused.
	tracepdeex(0, sp3Stream, "%%i    0    0    0    0      0      0      0      0         0\n");
	tracepdeex(0, sp3Stream, "%%i    0    0    0    0      0      0      0      0         0\n");

	// There is a minimum of four comment lines.
	tracepdeex(0, sp3Stream, "/* Created using Ginan at: %s.\n",   timeget().to_string(0).c_str());
	tracepdeex(0, sp3Stream, "/* WARNING: For Geoscience Australia's internal use only\n");
	tracepdeex(0, sp3Stream, "/*\n");
	tracepdeex(0, sp3Stream, "/*\n");
}

void updateSp3Body(
	string&				filename,		///< Path to output file.
	Sp3SatList&			entryList,		///< List of data to print.
	GTime				time,			///< Epoch time.
	OutSys				outSys,			///< Systems to include in file.
	Sp3FileData&		outFileDat)		///< Current file editing information. 
{
	double ep[6] = {};
	time2epoch(time, ep);

	//first create if non existing
	{
		std::fstream maker(filename, std::ios::app);
	}
	std::fstream sp3Stream(filename);

	if (!sp3Stream)
	{
		BOOST_LOG_TRIVIAL(error) << "Error opening " << filename << " for SP3 file.";
		
		return;
	}

	sp3Stream.seekp(0, std::ios::end);

	long endFilePos = sp3Stream.tellp();

	if (endFilePos == 0)
	{
		writeSp3Header(sp3Stream, entryList, time, outSys, outFileDat);
	}
	else
	{
		outFileDat.numEpoch++;
		
		sp3Stream.seekp(outFileDat.numEpoch_pos);
		
		tracepdeex(0, sp3Stream, "%7d", outFileDat.numEpoch);
		
		//go back to end of file (minus "EOF\n")
		sp3Stream.seekp(-4, std::ios::end);
	}

	tracepdeex(0, sp3Stream, "*  %4.0f %2.0f %2.0f %2.0f %2.0f %11.8f\n",   ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	// Note position is in kilometers and clock values microseconds.
	// There need to be one entry per satellite in the header for correct file format.
	for (auto& sat : outFileDat.sats)
	{
		auto it = entryList.find(sat);
		if (it != entryList.end())
		{			
			auto& [key, entry] = *it;
			
			{
				tracepdeex(0, sp3Stream, "P%s%14.6f%14.6f%14.6f%14.6f\n",
						entry.sat.id().c_str(),
						entry.satPos.x() / 1000,
						entry.satPos.y() / 1000,
						entry.satPos.z() / 1000,
						entry.clock[0] * 1e6);
			}
			
			if (acsConfig.output_orbit_velocities)
			{
				tracepdeex(0, sp3Stream, "V%s%14.6f%14.6f%14.6f%14.6f\n",
						entry.sat.id().c_str(),
						entry.satVel.x(),
						entry.satVel.y(),
						entry.satVel.z(),
						entry.clock[1] * 1e6);
			}
		}
		else
		{
			{
				tracepdeex(0, sp3Stream, "P%s%14.6f%14.6f%14.6f%14.6f\n",
						sat.id().c_str(), 0, 0, 0, 999999.999999);
			}
			
			if (acsConfig.output_orbit_velocities)
			{
				tracepdeex(0, sp3Stream, "V%s%14.6f%14.6f%14.6f%14.6f\n",
						sat.id().c_str(), 0, 0, 0, 999999.999999);
			}
		}
	}

	tracepdeex(0, sp3Stream, "EOF\n");
}

void writeSysSetSp3(
	string			filename,
	GTime			time,
	OutSys			outSys,
	Sp3FileData&	outFileDat,
	E_Ephemeris		sp3DataSrc,	
	KFState*		kfState_ptr)
{
	Sp3SatList entryList;

	for (auto& [satId, satNav] : nav.satNavMap)
	{
		SatSys Sat;
		Sat.fromHash(satId);

		if (!outSys[Sat.sys])
			continue;

		// Create a dummy observation
		Obs obs;
		obs.Sat = Sat;
		obs.satNav_ptr = &nav.satNavMap[Sat];

		GTime 		teph = time;

		bool pass = satpos(nullStream, time, teph, obs, acsConfig.orbits_data_source, E_OffsetType::COM, nav, false, kfState_ptr);
		if (pass == false)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: Writing SP3 file, failed to get data for satellite " << Sat.id();
			continue;
		}

		Sp3Entry entry;
		entry.sat = Sat;
		entry.satPos = obs.rSat;
		entry.satVel = obs.satVel;
		entry.clock[0] = obs.dtSat[0];
		entry.clock[1] = obs.dtSat[1];
		entry.sigma = sqrt(obs.ephVar);

		entryList[Sat] = entry;
	}

	updateSp3Body(filename, entryList, time, outSys, outFileDat);
}


void outputSp3(
	GTime		time,
	E_Ephemeris	sp3DataSrc,
	KFState*	kfState_ptr)
{
	auto sysFilenames = getSysOutputFilenames(acsConfig.orbits_filename, time);

	for (auto [filename, sysMap] : sysFilenames)
	{
		writeSysSetSp3(filename, time, sysMap, sp3CombinedFileData, sp3DataSrc, kfState_ptr);
	}
}
