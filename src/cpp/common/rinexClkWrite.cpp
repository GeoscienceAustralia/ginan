
// #pragma GCC optimize ("O0")


#include <fstream>
#include <tuple>
#include <map>

using std::map;

#include "inputsOutputs.hpp"
#include "rinexNavWrite.hpp"
#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "ephPrecise.hpp"
#include "GNSSambres.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "ephemeris.hpp"
#include "constants.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "sinex.hpp"
#include "rinex.hpp"
#include "enums.h"

/* macro defintions */
#define             VERSION             3.00

struct ClockEntry
{
	string			id;					// Either station of satellite.
	string			monid;				// Monument identification, receiver.
	Vector3d		recPos	= {};		// Receiver position.
	double	 		clock	= 0;		// Mean clock delta reference.
	double	 		sigma	= 0;		// Standard deviation.
	bool	 		isRec	= true;		// If true is receiver clock data.
	vector<int>		clkIndices;
};

struct ClockList : vector<ClockEntry>
{

};

void outputRinexClocksBody(
	string&			filename,	    ///< Path to output file.
	ClockList&		clkEntryList,	///< List of data to print.
	const GTime&	time)		    ///< Epoch time.
{
	std::ofstream clockFile(filename, std::ofstream::app);

	if (!clockFile)
	{
		BOOST_LOG_TRIVIAL(error) << "Error opening " << filename << " for RINEX clock file.";
		return;
	}

	GEpoch ep = time;

	for (auto& clkEntry : clkEntryList)
	{
		string dataType;
		if (clkEntry.isRec)	dataType = "AR";	// Result for receiver clock.
		else				dataType = "AS";	// Result for satellite clock.

		int numData = 2; // Number of data values is 2, clock and sigma.
		tracepdeex(0,clockFile,"%2s %-4s %4d%3d%3d%3d%3d%10.6f%3d   %19.12E %19.12E\n",
			dataType.c_str(), clkEntry.id.c_str(),
			(int) ep[0],
			(int) ep[1],
			(int) ep[2],
			(int) ep[3],
			(int) ep[4],
				ep[5],
			numData,
			clkEntry.clock,
			clkEntry.sigma);
	}
}

void getKalmanSatClks(
	ClockList&			clkEntryList,
	map<E_Sys, bool>&	outSys,
	KFState&			kfState)
{
	map<string, ClockEntry> clockEntries;

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	( key.type				!= KF::SAT_CLOCK
			||outSys[key.Sat.sys]	== false)
		{
			continue;
		}

		double clk		= 0;
		double variance	= 0;
		kfState.getKFValue(key, clk, &variance);

		ClockEntry& clkEntry = clockEntries[key.Sat.id()];
		clkEntry.id		= key.Sat.id();
		clkEntry.isRec	= false;

		clkEntry.clkIndices.push_back(index);
	}

	for (auto& [id, clkEntry] : clockEntries)
	{
		clkEntry.clock = 		kfState.x(clkEntry.clkIndices						).sum()		/ CLIGHT;
		clkEntry.sigma = sqrt(	kfState.P(clkEntry.clkIndices, clkEntry.clkIndices	).sum())	/ CLIGHT;

		clkEntryList.push_back(clkEntry);
	}
}

void getKalmanRecClks(
	ClockList&	clkEntryList,
	ClockEntry&	referenceRec,
	KFState& 	kfState)
{
	map<string, ClockEntry> clockEntries;

	SatSys firstSys;
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if  ( (  key.type		== KF::REC_SYS_BIAS
			  || key.type		== KF::REC_CLOCK)
			&&(  firstSys	== E_Sys::NONE
			  || firstSys	== key.Sat))
		{
			firstSys = key.Sat;

			double clk		= 0;
			double variance	= 0;
			kfState.getKFValue(key, clk, &variance);

			ClockEntry& clkEntry = clockEntries[key.str];
			clkEntry.id		= key.str;
			clkEntry.isRec	= true;

			clkEntry.clkIndices.push_back(index);

			if (key.rec_ptr == nullptr)
			{
				BOOST_LOG_TRIVIAL(error) << "Error: Kalman RINEX file clock entry has no reference to receiver.";
// 				continue;
			}
			else
			{
				clkEntry.monid	= key.rec_ptr->snx.id_ptr->domes;
				clkEntry.recPos	= key.rec_ptr->snx.pos;
			}
		}

		if	(  key.type		== KF::REC_SYS_BIAS
			|| key.type		== KF::REC_CLOCK)
		{
			// Enter details for reference receiver if available.
			referenceRec.id		= key.str;
			referenceRec.isRec	= true;

			if (key.rec_ptr)
			{
				referenceRec.monid	= key.rec_ptr->snx.id_ptr->domes;
				referenceRec.recPos	= key.rec_ptr->snx.pos;
			}
		}
	}

	for (auto& [id, clkEntry] : clockEntries)
	{
		clkEntry.clock = 		kfState.x(clkEntry.clkIndices						).sum()		/ CLIGHT;
		clkEntry.sigma = sqrt(	kfState.P(clkEntry.clkIndices, clkEntry.clkIndices	).sum())	/ CLIGHT;

		clkEntryList.push_back(clkEntry);
	}
}

void getPreciseRecClks(
	ClockList&  	clkEntryList,
	ReceiverMap*	receiverMap_ptr,
	const GTime& 	time)
{
	if (receiverMap_ptr == nullptr)
	{
		return;
	}

	auto& receiverMap = *receiverMap_ptr;

	for (auto& [id, rec] : receiverMap)
	{
		double dt;
		double variance;
		int ret = pephclk(std::cout, time, rec.id, nav, dt, &variance);
		if (ret != 1)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: Receiver : " << rec.id
			<< ", precise clock entry not calculated.";

			continue;
		}

		ClockEntry clkEntry;
		clkEntry.id		= rec.id;
		clkEntry.clock	= dt;
		clkEntry.sigma	= sqrt(variance);
		clkEntry.isRec	= true;
		clkEntry.monid	= rec.snx.id_ptr->domes;
		clkEntry.recPos	= rec.snx.pos;

		clkEntryList.push_back(clkEntry);
	}
}

void getSatClksFromEph(
	ClockList&  		clkEntryList,
	const GTime& 		time,
	map<E_Sys, bool>&	outSys,
	vector<E_Source>	ephType)
{
	for (auto& [Sat, satNav] : nav.satNavMap)
	{
		if (outSys[Sat.sys] == false)
			continue;

		// Create a dummy observation
		GObs obs;
		obs.Sat			= Sat;
		obs.satNav_ptr	= &nav.satNavMap[Sat]; // for satpos_ssr()

		bool pass = true;
		pass &= satclk(nullStream, time, time, obs, ephType,					nav);
		pass &= satpos(nullStream, time, time, obs, ephType, E_OffsetType::COM,	nav);	//use both for now to get ssr clocks if required
		if (pass == false)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: Satellite : " << Sat.id()
			<< ",  clock entry not calculated.";

			continue;
		}

		ClockEntry clkEntry;
		clkEntry.id		= Sat.id();
		clkEntry.clock	= obs.satClk;
		clkEntry.sigma	= sqrt(obs.satClkVar);
		clkEntry.isRec	= false;

		clkEntryList.push_back(clkEntry);
	}
}

void outputRinexClocksHeader(
	string&				filename,			///< Path of tile to output to
	ClockList&			clkEntryList,		///< List of clock values to output
	ClockEntry&			referenceRec,		///< Entry for the reference receiver
	map<E_Sys, bool>&	sysMap,				///< Options to enable outputting of specific systems
	GTime				time)				///< Epoch time
{
	std::ofstream clockFile(filename, std::ofstream::app);

	if (!clockFile)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: Error opening " << filename << " for RINEX clock file.";
		return;
	}

	auto pos = clockFile.tellp();
	if (pos != 0)
		return;


	string sysDesc;
	if (sysMap.size() == 1)	sysDesc = rinexSysDesc(sysMap.begin()->first);
	else					sysDesc = rinexSysDesc(E_Sys::COMB);

	string clkRefStation = referenceRec.id;

	int numRecs = 0;
	for (auto clkEntry : clkEntryList)
	{
		if (clkEntry.isRec)
		{
			numRecs++;
		}
	}

	tracepdeex(0, clockFile, "%9.2f%-11s%-20s%-20s%-20s\n",
		VERSION,
		"",
		"C",
		sysDesc.c_str(),
		"RINEX VERSION / TYPE");

	GEpoch ep = time;

	tracepdeex(0,clockFile,"%-20s%-20s%4d%02d%02d %02d%02d%02d %4s%s\n",
		acsConfig.analysis_software	.c_str(),
		acsConfig.analysis_agency	.c_str(),
		(int)ep[0],
		(int)ep[1],
		(int)ep[2],
		(int)ep[3],
		(int)ep[4],
		(int)ep[5],
		"LCL","PGM / RUN BY / DATE");

	tracepdeex(0,clockFile,"%-60s%s\n","",																			"SYS / # / OBS TYPES");
	tracepdeex(0,clockFile,"%-60s%s\n","",																			"TIME SYSTEM ID");
	tracepdeex(0,clockFile,"%6d    %2s    %2s%-42s%s\n",           2,"AS","AR","",									"# / TYPES OF DATA");
	tracepdeex(0,clockFile,"%-60s%s\n","",																			"STATION NAME / NUM");
	tracepdeex(0,clockFile,"%-60s%s\n","",																			"STATION CLK REF");
	tracepdeex(0,clockFile,"%-3s  %-55s%s\n", acsConfig.analysis_agency.c_str(), acsConfig.analysis_centre.c_str(),	"ANALYSIS CENTER");
	tracepdeex(0,clockFile,"%6d%54s%s\n",1,"",																		"# OF CLK REF");

	// Note clkRefStation can be a zero length string.
	tracepdeex(0,clockFile,"%-4s %-20s%35s%s\n", "", clkRefStation.c_str(),"",										"ANALYSIS CLK REF");
	tracepdeex(0,clockFile,"%6d    %-50s%s\n", numRecs, "IGS14",													"# OF SOLN STA / TRF");
	// MM This line causes the clock combination software to crash to removing
	//tracepdeex(0,clockFile,"%-60s%s\n",acsConfig.rinex_comment,												"COMMENT");

	/* output receiver id and coordinates */

	for (auto& clkEntry : clkEntryList)
	{
		if (clkEntry.isRec == false)
		{
			continue;
		}

		string idStr  = clkEntry.id		.substr(0,4);
		string monuid = clkEntry.monid	.substr(0,20);

		tracepdeex(0,clockFile,"%-4s ",idStr.c_str());
		tracepdeex(0,clockFile,"%-20s",monuid.c_str());
		tracepdeex(0,clockFile,"%11.0f %11.0f %11.0f%s\n",
				clkEntry.recPos(0) * 1000,
				clkEntry.recPos(1) * 1000,
				clkEntry.recPos(2) * 1000,
				"SOLN STA NAME / NUM");
	}

	int num_sats = 0;
	if (sysMap[E_Sys::GPS])	num_sats += NSATGPS;
	if (sysMap[E_Sys::GLO])	num_sats += NSATGLO;
	if (sysMap[E_Sys::GAL])	num_sats += NSATGAL;
	if (sysMap[E_Sys::BDS])	num_sats += NSATBDS;
	if (sysMap[E_Sys::QZS])	num_sats += NSATQZS;


	/* output satellite PRN*/
	int k = 0;
	tracepdeex(0,clockFile,"%6d%54s%s\n",num_sats,"","# OF SOLN SATS");
	if (sysMap[E_Sys::GPS])	for (int prn = 1; prn <= NSATGPS; prn++)	{k++;	SatSys s(E_Sys::GPS,prn);	tracepdeex(0,clockFile,"%3s ",	s.id().c_str());	if (k % 15 == 0) tracepdeex(0,clockFile,"%s\n","PRN LIST");}
	if (sysMap[E_Sys::GLO])	for (int prn = 1; prn <= NSATGLO; prn++)	{k++;	SatSys s(E_Sys::GLO,prn);	tracepdeex(0,clockFile,"%3s ",	s.id().c_str());	if (k % 15 == 0) tracepdeex(0,clockFile,"%s\n","PRN LIST");}
	if (sysMap[E_Sys::GAL])	for (int prn = 1; prn <= NSATGAL; prn++)	{k++;	SatSys s(E_Sys::GAL,prn);	tracepdeex(0,clockFile,"%3s ",	s.id().c_str());	if (k % 15 == 0) tracepdeex(0,clockFile,"%s\n","PRN LIST");}
	if (sysMap[E_Sys::BDS])	for (int prn = 1; prn <= NSATBDS; prn++)	{k++;	SatSys s(E_Sys::BDS,prn);	tracepdeex(0,clockFile,"%3s ",	s.id().c_str());	if (k % 15 == 0) tracepdeex(0,clockFile,"%s\n","PRN LIST");}
	if (sysMap[E_Sys::QZS])	for (int prn = 1; prn <= NSATQZS; prn++)	{k++;	SatSys s(E_Sys::QZS,prn);	tracepdeex(0,clockFile,"%3s ",	s.id().c_str());	if (k % 15 == 0) tracepdeex(0,clockFile,"%s\n","PRN LIST");}
	/*finish the line*/						while (k % 15 != 0)					{k++;						tracepdeex(0,clockFile,"%3s ",	"");				if (k % 15 == 0) tracepdeex(0,clockFile,"%s\n","PRN LIST");}

	tracepdeex(0,clockFile,"%-60s%s\n","","END OF HEADER");
}


void outputClocksSet(
	string				filename,
	vector<E_Source>	clkDataRecSrcs,
	vector<E_Source>	clkDataSatSrcs,
	const GTime&		time,
	map<E_Sys, bool>&	outSys,
	KFState&			kfState,
	ReceiverMap*		receiverMap_ptr)
{
	ClockList	clkEntryList;
	ClockEntry	referenceRec;
	referenceRec.isRec = false;

	switch (clkDataSatSrcs.front())		//todo aaron, remove this function
	{
		case +E_Source::NONE:																				break;
		case +E_Source::KALMAN:				getKalmanSatClks(clkEntryList, outSys, kfState);				break;
		case +E_Source::PRECISE:			//fallthrough
		case +E_Source::BROADCAST:			//fallthrough
		case +E_Source::SSR:				getSatClksFromEph(clkEntryList, time, outSys, clkDataSatSrcs);	break;
		default:	BOOST_LOG_TRIVIAL(error) << "Error: Unknown / Undefined clock data source.";			return;
	}

	switch (clkDataRecSrcs.front())
	{
		case +E_Source::NONE:																			break;
		case +E_Source::KALMAN:				getKalmanRecClks(clkEntryList, referenceRec, kfState);		break;
		case +E_Source::PRECISE:			getPreciseRecClks(clkEntryList, receiverMap_ptr, time);		break;
		case +E_Source::SSR:				//fallthrough
		case +E_Source::BROADCAST:			//fallthrough
		default:	BOOST_LOG_TRIVIAL(error) << "Error: Printing receiver clocks for " << clkDataRecSrcs.front()._to_string() << " not implemented.";	return;
	}

	outputRinexClocksHeader	(filename, clkEntryList, referenceRec, outSys, time);
	outputRinexClocksBody	(filename, clkEntryList, time);
}

map<string, map<E_Sys, bool>> getSysOutputFilenames(
	string	filename,
	GTime	logtime,
	bool	replaceSys,
	string	id)
{
	logtime = logtime.floorTime(acsConfig.rotate_period);

	boost::posix_time::ptime	logptime	= boost::posix_time::from_time_t((time_t)((PTime)logtime).bigTime);

	if (logtime == GTime::noTime())
	{
		logptime = boost::posix_time::not_a_date_time;
	}

	replaceString(filename, "<RECEIVER>", id);
	replaceTimes (filename, logptime);

	map<string, map<E_Sys, bool>> fileOutputSysMap;

	if (replaceSys == false)
	{
		fileOutputSysMap[filename][E_Sys::NONE] = true;

		return fileOutputSysMap;
	}

	for (auto& [sys, output] : acsConfig.process_sys)
	{
		if (output == false)
			continue;

		SatSys t_Sat = SatSys(sys, 0);

		string sysChar;
		if (acsConfig.split_sys)	sysChar = string(1, t_Sat.sysChar());
		else						sysChar = "M";

		if (sysChar == "-")
			continue;

		string sysFilename = filename;
		replaceString(sysFilename, "<SYS>", sysChar);

		fileOutputSysMap[sysFilename][sys] = true;
	}

	return fileOutputSysMap;
}

void outputClocks(
	string				filename,
	const GTime&		time,
	vector<E_Source>	clkDataRecSrcs,
	vector<E_Source>	clkDataSatSrcs,
	KFState&			kfState,
	ReceiverMap*		receiverMap_ptr)
{
	auto filenameSysMap = getSysOutputFilenames(filename, time);

	for (auto [sysFilename, sysMap] : filenameSysMap)
	{
		outputClocksSet(sysFilename, clkDataRecSrcs, clkDataSatSrcs, time, sysMap, kfState, receiverMap_ptr);
	}
}
