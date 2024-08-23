
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

FileType COST__()
{

}

#include <fstream>

#include <boost/algorithm/string.hpp>

#include "coordinates.hpp"
#include "tropModels.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "sinex.hpp"
#include "trace.hpp"
#include "cost.hpp"
#include "EGM96.h"
#include "ppp.hpp"


static map<string, map<E_FilePos, int>>	filePosMap;
static map<string, GTime>				startTimeMap;
static map<string, int>					numSamplesMap;


/** Replaces first instance of string 'toReplace' with 'replaceWith' within 's'
*/
bool replaceStr(
	string&	s,				///< String to modify
	string	toReplace,		///< String to replace
	string	replaceWith)	///< String to replace with
{
	std::size_t pos = s.find(toReplace);
	if (pos == string::npos) return false;
	s.replace(pos, toReplace.length(), replaceWith);
	return true;
}

/** Outputs troposphere COST file
*/
void outputCost(
	string		filename,	///< Filename
	KFState&	kfState,	///< KF object containing positioning & trop solutions
	Receiver&	rec)		///< Receiver
{
	std::ofstream fout(filename, std::fstream::in | std::fstream::out);
	if (!fout)
	{
		return;
	}

	auto& recOpts = acsConfig.getRecOpts(rec.id);

	auto time = kfState.time;

	fout.seekp(0, fout.end);				// seek to end of file
	bool firstWrite = (fout.tellp() == 0);	// file is empty if current position is 0

	if (firstWrite)
		startTimeMap[filename] = time;
	long int duration = (time - startTimeMap[filename]).to_int();
	if (duration % acsConfig.cost_time_interval != 0)
		return;

	if (firstWrite)
	{
		tracepdeex(0, fout, "%-20s     %-20s     %-20s\n",
			acsConfig.cost_format,						// Format name & version number
			acsConfig.cost_project,						// Project name
			acsConfig.cost_status);						// File status

		string locationName = rec.snx.id_ptr->desc;
		boost::trim(locationName);

		bool commaReplaced = replaceStr(locationName, ", ", " (");
		if (commaReplaced)
			locationName.push_back(')');

		tracepdeex(0, fout, "%-4s %-9s           %-60s\n",
			rec.snx.id_ptr->sitecode	.c_str(),					// Rec ID
			rec.snx.id_ptr->domes		.c_str(),					// DOMES ID
			locationName				.c_str());					// Site name with country

		tracepdeex(0, fout, "%-20s     %-20s\n",
			rec.receiverType			.c_str(),					// Receiver type
			rec.antennaType				.c_str());					// Antenna type
	}

	if (firstWrite)	filePosMap[filename][E_FilePos::COORD] = fout.tellp();
	fout.seekp(		filePosMap[filename][E_FilePos::COORD]);	// Overwrite


	VectorEcef recPosEcef;
	bool kfFound = true;
	for (int i = 0; i < 3; i++)
	{
		kfFound &= kfState.getKFValue({KF::REC_POS, {}, rec.id, i}, recPosEcef(i));
	}

	if (kfFound == false)
	{
		recPosEcef = rec.aprioriPos;
	}

	VectorEcef	eccEcef = body2ecef(rec.attStatus, rec.snx.ecc_ptr->ecc);
	VectorPos	recPos	= ecef2pos(recPosEcef + eccEcef);

	if (recPos[1] < 0)
		recPos[1] += 2 * PI;

	double geoidOffset = egm96_compute_altitude_offset(recPos.latDeg(), recPos.lonDeg());

	tracepdeex(0, fout, "%12.6lf%12.6lf%12.3lf%12.3lf%12.3lf\n",
		recPos.latDeg(),
		recPos.lonDeg(),
		recPos.hgt(),									// ARP height above ellipsoid
		recPos.hgt() - geoidOffset,						// ARP height above geoid
		rec.snx.ecc_ptr->ecc.u());						// ARP height above benchmark

	if (firstWrite)
		tracepdeex(0, fout, "%-20s     ", time.gregString().c_str());	// Time of first sample

	if (firstWrite)	filePosMap[filename][E_FilePos::CURR_TIME] = fout.tellp();
	fout.seekp(		filePosMap[filename][E_FilePos::CURR_TIME]);
	tracepdeex(0, fout, "%-20s\n", time.gregString().c_str());	// Time of processing

	if (firstWrite)
	{
		tracepdeex(0, fout, "%-20s     %-20s     %-20s     %-20s\n",
			acsConfig.cost_centre,						// Processing centre
			acsConfig.cost_method,						// Processing method
			acsConfig.cost_orbit_type,					// Orbit type
			acsConfig.cost_met_source);					// Source of met. data

		tracepdeex(0, fout, "%5d%5d",
			acsConfig.cost_time_interval / 60,			// Nominal time increment between data samples (min)
			acsConfig.cost_time_interval / 60);			// Batch updating interval (min)
	}

	if (firstWrite)	filePosMap[filename][E_FilePos::TOTAL_TIME]	= fout.tellp();
	fout.seekp(		filePosMap[filename][E_FilePos::TOTAL_TIME]);
	tracepdeex(0, fout, "%5d\n", duration / 60);		// Total length of batch time series

	if (firstWrite)	filePosMap[filename][E_FilePos::PCDH]			= fout.tellp();
	fout.seekp(		filePosMap[filename][E_FilePos::PCDH]);

	bool isRealTime = (acsConfig.obs_rtcm_inputs.size() > 0);

	union
	{
		unsigned int all = 0;
		struct
		{
			unsigned isRealTime		: 1;	///< Data processed in near-real time [false: (re-)processed off-line]
			unsigned climate		: 1;	///< Data processed to climate quality [false: NRT quality]
			unsigned otl			: 1;	///< Ocean tide loading correction applied
			unsigned atc			: 1;	///< Atmospheric loading correction applied
			unsigned localMetData	: 1;	///< Local surface met. sensor data available
			unsigned centredTime	: 1;	///< Timestamps are at the centre of period [false: end of period]
			unsigned gpsUsed		: 1;	///< GPS satellite(s) used
			unsigned gloUsed		: 1;	///< GLONASS satellite(s) used
			unsigned galUsed		: 1;	///< Galileo satellite(s) used
			unsigned reserved		: 22;	///< Reserved
			unsigned invalid		: 1;	///< PCDH is missing or invalid
		};
	} pcdh;									///< Product Confidence Data - Header
	pcdh.isRealTime		= isRealTime;
	pcdh.climate		= false;
	pcdh.otl			= recOpts.tideModels.otl;
	pcdh.atc			= false;
	pcdh.localMetData	= false;
	pcdh.centredTime	= false;
	pcdh.gpsUsed		= acsConfig.process_sys[E_Sys::GPS];
	pcdh.gloUsed		= acsConfig.process_sys[E_Sys::GLO];
	pcdh.galUsed		= acsConfig.process_sys[E_Sys::GAL];
	pcdh.invalid		= false;

	tracepdeex(0, fout, "%08x\n", (uint32_t)pcdh.all);	// Product confidence data - header

	auto& numSamplesPos = filePosMap[filename][E_FilePos::NUM_SAMPLES];

	if (firstWrite)
		numSamplesPos = fout.tellp();

	fout.seekp(numSamplesPos);

	numSamplesMap[filename]++;

	tracepdeex(0, fout, "%4d\n", numSamplesMap[filename]);


	// Body
	if (firstWrite == false)
		fout.seekp(filePosMap[filename][E_FilePos::FOOTER]);


	int obsCount = 0;
	for (auto& obs : only<GObs>(rec.obsList))
	{
		if	( acsConfig.process_sys[obs.Sat.sys] == false
			||obs.exclude)
		{
			continue;
		}

		obsCount++;
	}

	union
	{
		unsigned int all = 0;
		struct
		{
			unsigned numSats		: 5;	///< Num GNSS sasts in solution [31 = missing]
			unsigned obsMetData		: 1;	///< Observed met. data used [false: NWP met data]
			unsigned ztdPoorQuality	: 1;	///< ZTD data quality is considered poor
			unsigned reserved		: 24;	///< Reserved
			unsigned invalid		: 1;	///< PCDD is missing or invalid
		};
	} pcdd;									///< Product Confidence Data - Data

	pcdd.numSats		= std::min(obsCount, 31);
	pcdd.obsMetData		= false;
	pcdd.ztdPoorQuality	= false;
	pcdd.invalid		= false;

	double	tropStates[3]	= {};
	double	tropVars  [3]	= {};
	bool	tropFound [3]	= {};

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	( key.type != KF::TROP
			&&key.type != KF::TROP_GRAD)
		{
			continue;
		}

		if	( acsConfig.pppOpts.equate_tropospheres	== false
			&&key.str								!= rec.id)
		{
			continue;
		}

		double state	= kfState.x(index);
		double var		= kfState.P(index,index);

		int num;
		if (key.type == KF::TROP)		num = 0;
		else							num = key.num + 1;

		tropFound	[num] =  true;
		tropStates	[num] += state;
		tropVars	[num] += var;
	}

	double ztd			= -9.9	/ 1000;
	double zwd			= -9.9	/ 1000;
	double ztdStd		= -9.9	/ 1000;
	double nsGrad		= -9.99	/ 1000;
	double ewGrad		= -9.99	/ 1000;
	double nsGradStd	= -9.99	/ 1000;
	double ewGradStd	= -9.99	/ 1000;

	if (tropFound[0])
	{
		ztd				= 		tropStates	[0];
		ztdStd			= sqrt(	tropVars	[0]);

		double zhd		= tropDryZTD(nullStream, recOpts.tropModel.models, time, recPos);
		zwd				= ztd - zhd;
	}

	double gradM	= gradMapFn(30 * D2R);

	if (tropFound[1])	{	nsGrad = gradM * tropStates[1];		nsGradStd = sqrt(tropVars[1]);	}
	if (tropFound[2])	{	ewGrad = gradM * tropStates[2];		ewGradStd = sqrt(tropVars[2]);	}

	GEpoch epoch = time;
	tracepdeex(0, fout, " %02d %02d %02d %08x%7.1lf%7.1lf%7.1lf%7.1lf%7.1lf%7.1lf%7.1lf%7.2lf%7.2lf%7.2lf%7.2lf%8.3lf\n",
		(int)epoch.hour,								// Timestamp (hr)
		(int)epoch.min,									// Timestamp (min)
		(int)epoch.sec,									// Timestamp (sec)
		(uint32_t)pcdd.all,								// Product confidence data - data
		ztd			* 1000,								// ZTD (mm)
		ztdStd		* 1000,								// ZTD std-dev (mm)
		zwd			* 1000,								// ZWD (mm)
		-9.9,											// IWV (kg.m^-2)
		-9.9,											// Pressure (hPa)
		-9.9,											// Temperature used for IWV (K)
		-9.9,											// Relative humidity used for IWV (%)
		nsGrad		* 1000,								// N/S delay gradient (mm)
		ewGrad		* 1000,								// E/W delay gradient (mm)
		nsGradStd	* 1000,								// N/S delay gradient std-dev (mm)
		ewGradStd	* 1000,								// E/W delay gradient std-dev (mm)
		-99.999);										// Vertically integrated TEC (TECU)

	tracepdeex(0, fout, "%4d\n", obsCount);				// Number of slant samples to follow

	for (auto& obs : only<GObs>(rec.obsList))
	{
		if	( acsConfig.process_sys[obs.Sat.sys] == false
			||obs.exclude
			||obs.tropSlant == 0)
		{
			continue;
		}

		SatStat& satStat = *obs.satStat_ptr;

		tracepdeex(0, fout, "%-4s%7.1lf%7.1lf%7.1lf%7.1lf\n",
			obs.Sat.id().c_str(),						// Sat ID
			obs.tropSlant			* 1000,				// Total slant delay (mm)
			sqrt(obs.tropSlantVar)	* 1000,				// Total slant delay std-dev (mm)
			satStat.az * R2D,							// Slant azi angle (CW from true North)
			satStat.el * R2D);							// Slant ele angle (from local horizon)
	}

	filePosMap[filename][E_FilePos::FOOTER] = fout.tellp();

	tracepdeex(0, fout, "----------------------------------------------------------------------------------------------------\n");
}
