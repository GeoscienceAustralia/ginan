
// #pragma GCC optimize ("O0")


#include <fstream>


#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "coordinates.hpp"
#include "GNSSambres.hpp"
#include "navigation.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "mongoRead.hpp"
#include "sp3Write.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "enums.h"
#include "erp.hpp"

/* macro defintions */
#define             VERSION             3.00

struct Sp3Entry
{
	SatSys 		Sat;
	Vector3d	satPos		= Vector3d::Zero();				// Satellite position.
	Vector3d	satVel		= Vector3d::Zero();				// Satellite velocity.
	double	 	satClk		= INVALID_CLOCK_VALUE / 1e6;
	double		satClkVel	= INVALID_CLOCK_VALUE / 1e6;
	double		sigma		= 0;
	bool		predicted	= false;
};


struct Sp3FileData
{
	map<SatSys, bool>	sats;
	long 				numEpoch		= 0;
	long 				numEpoch_pos	= 0;
};

map<string, Sp3FileData> sp3FileDataMap;

void writeSp3Header(
	std::fstream& 		sp3Stream,
	map<int, Sp3Entry>&	entryList,
	GTime				time,
	map<E_Sys, bool>&	outSys,
	Sp3FileData&		sp3FileData)
{
	GEpoch ep = time;

	sp3FileData = {};
	sp3FileData.numEpoch = 1;

	// note "#dV" for velocity and position.
	if (acsConfig.output_sp3_velocities)		tracepdeex(0, sp3Stream, "#dV%4.0f %2.0f %2.0f %2.0f %2.0f %11.8f ", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);
	else										tracepdeex(0, sp3Stream, "#dP%4.0f %2.0f %2.0f %2.0f %2.0f %11.8f ", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	sp3FileData.numEpoch_pos = sp3Stream.tellp();

	//TODO Check, coordinate system and Orbit Type from example product file.
	tracepdeex(0, sp3Stream, "%7d ORBIT IGS14 HLM %4s\n",   sp3FileData.numEpoch,   acsConfig.analysis_agency.c_str());

	GWeek	week	= time;
	GTow	tow		= time;
	double mjdate = 7.0 * week + tow / 86400.0 + 44244.0;	//todo aaron ew.
	tracepdeex(0, sp3Stream, "## %4d %15.8f %14.8f %5.0f %15.13f\n",
			   week,
			   tow,
			   acsConfig.sp3_output_interval,
			   mjdate,
			   mjdate - floor(mjdate));

	for (auto sys : {E_Sys::GPS, E_Sys::GLO, E_Sys::GAL, E_Sys::BDS, E_Sys::LEO})
	{
		if (outSys[sys])
		for (auto Sat : getSysSats(sys))
		{
			auto satOpts = acsConfig.getSatOpts(Sat);

			if (satOpts.exclude)
				continue;

			sp3FileData.sats[Sat] = true;
		}
	}

	int lineNumber	= 1;
	int lineEntries	= 0;
	tracepdeex(0, sp3Stream, "+  %3d   ", sp3FileData.sats.size());
	for (auto& [Sat, enable] : sp3FileData.sats)
	{
		if (lineEntries	== 17)
		{
			//start a new line
			tracepdeex(0, sp3Stream, "\n+        ");
			lineEntries = 0;
			lineNumber++;
		}
		tracepdeex(0, sp3Stream, "%3s", Sat.id().c_str());

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
	for (auto& [Sat, enable] : sp3FileData.sats)
	{
		if (lineEntries == 17)
		{
			//start a new line
			tracepdeex(0, sp3Stream, "\n++       ");
			lineEntries = 0;
		}

		auto it = entryList.find(Sat);
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
	for (auto& [Sat, enable] : sp3FileData.sats)
	{
		if (syschar == 0)					{			syschar = Sat.sysChar();			continue;		}
		if (syschar != Sat.sysChar())		{			syschar = 'M';						break;			}
	}
	if (syschar == 0)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: Writing RINEX clock file no systems in process_sys.";
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
	tracepdeex(0, sp3Stream, "/* Created using Ginan at: %s.\n",   timeGet().to_string().c_str());
	tracepdeex(0, sp3Stream, "/* WARNING: Not for operational use\n");
	tracepdeex(0, sp3Stream, "/*\n");
	tracepdeex(0, sp3Stream, "/*\n");
}

void updateSp3Body(
	string				filename,		///< Path to output file.
	map<int, Sp3Entry>&	entryList,		///< List of data to print.
	GTime				time,			///< Epoch time.
	map<E_Sys, bool>&	outSys)			///< Systems to include in file.
{
	//first create if non existing
	{
		std::fstream maker(filename, std::ios::app);
	}
	std::fstream sp3Stream(filename);

	if (!sp3Stream)
	{
		BOOST_LOG_TRIVIAL(warning) << "Error opening " << filename << " for SP3 file.";

		return;
	}

	sp3Stream.seekp(0, std::ios::end);

	long endFilePos = sp3Stream.tellp();

	auto& sp3FileData = sp3FileDataMap[filename];

	if (endFilePos == 0)
	{
		writeSp3Header(sp3Stream, entryList, time, outSys, sp3FileData);
	}
	else
	{
		sp3FileData.numEpoch++;

		sp3Stream.seekp(sp3FileData.numEpoch_pos);

		tracepdeex(0, sp3Stream, "%7d", sp3FileData.numEpoch);

		//go back to end of file (minus "EOF\n")
		sp3Stream.seekp(-4, std::ios::end);
	}

	GEpoch ep = time;
	tracepdeex(0, sp3Stream, "*  %4.0f %2.0f %2.0f %2.0f %2.0f %11.8f\n",   ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	// Note position is in kilometers and clock values microseconds.
	// There need to be one entry per satellite in the header for correct file format.
	for (auto& [Sat, enable] : sp3FileData.sats)
	{
		auto it = entryList.find(Sat);
		if (it != entryList.end())
		{
			auto& [key, entry] = *it;

			char predictedChar;
			if (entry.predicted)		predictedChar = 'P';
			else						predictedChar = ' ';

			{
				tracepdeex(0, sp3Stream, "P%s%14.6f%14.6f%14.6f%14.6f%15s%c%3s%c\n",
						entry.Sat.id().c_str(),
						entry.satPos.x() / 1000,
						entry.satPos.y() / 1000,
						entry.satPos.z() / 1000,
						entry.satClk * 1e6,
						"",
						predictedChar,
						"",
						predictedChar);
			}

			if (acsConfig.output_sp3_velocities)
			{
				tracepdeex(0, sp3Stream, "V%s%14.6f%14.6f%14.6f%14.6f%15s%c%3s%c\n",
						entry.Sat.id().c_str(),
						entry.satVel.x(),
						entry.satVel.y(),
						entry.satVel.z(),
						entry.satClkVel * 1e6,
						"",
						predictedChar,
						"",
						predictedChar);
			}
		}
		else
		{
			{
				tracepdeex(0, sp3Stream, "P%s%14.6f%14.6f%14.6f%14.6f\n",
						Sat.id().c_str(), 0, 0, 0, NO_SP3_CLK);
			}

			if (acsConfig.output_sp3_velocities)
			{
				tracepdeex(0, sp3Stream, "V%s%14.6f%14.6f%14.6f%14.6f\n",
						Sat.id().c_str(), 0, 0, 0, NO_SP3_CLK);
			}
		}
	}

	tracepdeex(0, sp3Stream, "EOF\n");
}

void writeSysSetSp3(
	string				filename,
	GTime				time,
	map<E_Sys, bool>&	outSys,
	vector<E_Source>	sp3OrbitSrcs,
	vector<E_Source>	sp3ClockSrcs,
	KFState*			kfState_ptr,
	bool				predicted)
{
	map<int, Sp3Entry> entryList;

	ERPValues filterErpv;
	if (kfState_ptr)
	{
		filterErpv = getErpFromFilter(*kfState_ptr);
	}

	ERPValues erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapperUndo(time, erpv);
	FrameSwapper frameSwapperRedo(time, filterErpv);

	for (auto& [Sat, satNav] : nav.satNavMap)
	{
		if (outSys[Sat.sys] == false)
			continue;

		// Create a dummy observation
		GObs obs;
		obs.Sat			= Sat;
		obs.satNav_ptr	= &nav.satNavMap[Sat];

		bool clkPass = satclk(nullStream, time, time, obs, sp3ClockSrcs,					nav, kfState_ptr);
		bool posPass = satpos(nullStream, time, time, obs, sp3OrbitSrcs, E_OffsetType::COM,	nav, kfState_ptr);

		if (posPass == false)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: Writing SP3 file, failed to get data for satellite " << Sat.id();
			continue;
		}

		Sp3Entry entry;
		entry.Sat = Sat;
		if (acsConfig.output_inertial_orbits)
		{
			obs.rSatEci0 = frameSwapperUndo(obs.rSatCom, &obs.satVel, &obs.vSatEci0);

			entry.satPos = obs.rSatEci0;
			entry.satVel = obs.vSatEci0;
		}
		else
		{
			VectorEcef posEcef = obs.rSatCom;
			VectorEcef velEcef = obs.satVel;

			if (filterErpv.time != GTime::noTime())
			{
				VectorEci velEci;
				VectorEci posEci;

				posEci	= frameSwapperUndo(posEcef,	&velEcef,	&velEci);
				posEcef = frameSwapperRedo(posEci,	&velEci,	&velEcef);
			}

			entry.satPos = posEcef;
			entry.satVel = velEcef;
		}

		if (clkPass)
		{
			entry.satClk	= obs.satClk;
			entry.satClkVel	= obs.satClkVel;
		}
		else
		{
			entry.satClk	= INVALID_CLOCK_VALUE / 1e6;
			entry.satClkVel	= INVALID_CLOCK_VALUE / 1e6;
		}

		entry.sigma		= sqrt(obs.satClkVar);
		entry.predicted	= predicted;

		entryList[Sat] = entry;
	}

	updateSp3Body(filename, entryList, time, outSys);
}


void outputSp3(
	string				filename,
	GTime				time,
	vector<E_Source>	sp3OrbitSrcs,
	vector<E_Source>	sp3ClockSrcs,
	KFState*			kfState_ptr,
	bool				predicted)
{
	time = time.floorTime(1);

	GTow tow = time;
	if (int(tow) % acsConfig.sp3_output_interval != 0)
		return;

	auto sysFilenames = getSysOutputFilenames(filename, time);

	for (auto [filename, sysMap] : sysFilenames)
	{
		writeSysSetSp3(filename, time, sysMap, sp3OrbitSrcs, sp3ClockSrcs, kfState_ptr, predicted);
	}
}

void outputMongoOrbits()
{
	map<GTime, map<int, Sp3Entry>> entryListMap;

	auto orbitMapMap = mongoReadOrbits();
	auto clockMapMap = mongoReadClocks();

	map<E_Sys, bool> outSys;

	auto sysFilenames = getSysOutputFilenames(acsConfig.predicted_sp3_filename, tsync);

	for (auto [filename, sysMap] : sysFilenames)
	{
		for (auto& [Sat,	timeMap]	: orbitMapMap)
		for (auto& [time,	state]		: timeMap)
		{
			ERPValues erpv = getErp(nav.erp, time);

			FrameSwapper frameSwapper(time, erpv);

			auto& entry = entryListMap[time][Sat];

			entry.Sat		= Sat;
			entry.predicted	= true;

			if (acsConfig.output_inertial_orbits)
			{
				entry.satPos = state.head(3);
				entry.satVel = state.tail(3);
			}
			else
			{
				VectorEci rSat = (Vector3d) state.head(3);
				VectorEci vSat = (Vector3d) state.tail(3);

				VectorEcef vSatEcef;
				entry.satPos = frameSwapper(rSat, &vSat, &vSatEcef);
				entry.satVel = vSatEcef;
			}

			auto timeMapIt = clockMapMap.find(Sat.id());
			if (timeMapIt == clockMapMap.end())
			{
				continue;
			}

			auto& [dummy1, timeClkMap] = *timeMapIt;

			auto timeIt = timeClkMap.find(time);
			if (timeIt == timeClkMap.end())
			{
				continue;
			}

			auto& [dummy2, clockTuple]	= *timeIt;
			auto& [clock, drift]		= clockTuple;

			entry.satClk	= clock;
			entry.satClkVel	= drift;
		}

		for (auto& [time, entryList] : entryListMap)
		{
			updateSp3Body(filename, entryList, time, sysMap);
		}
	}
}
