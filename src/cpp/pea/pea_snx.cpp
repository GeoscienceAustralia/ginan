
// #pragma GCC optimize ("O0")

#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/utsname.h>

#include <boost/log/trivial.hpp>

#include "eigenIncluder.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "station.hpp"
#include "gTime.hpp"
#include "sinex.hpp"

void getStationsFromSinex(
	map<string, Station>&	stationMap,
	KFState&				kfState)
{
	
}

void sinexPostProcessing(
	GTime						time,
	map<string, Station>&		stationMap,
	KFState&					netKFState)
{
	theSinex.inputFiles.		clear();
	theSinex.acknowledgements.	clear();
	theSinex.inputHistory.		clear();

	// FIXME: network solution could be different?
	// TODO: replace 0.1 with some auto generated variable or config file entry, should be current version in bitbucket
	sinex_check_add_ga_reference("PPP Solution", "0.1", false);

	{
		list<KFState*> kfStatePointers;

		if (acsConfig.process_user)
		for (auto& [key, rec] : stationMap)
		{
			kfStatePointers.push_back(&rec.pppState);
		}
		
		KFState augmentedNetKFState;
		if (acsConfig.process_network)
		{
			augmentedNetKFState = netKFState;
			
			for (auto& [key, index] : augmentedNetKFState.kfIndexMap)
			{
				if (key.type != KF::REC_POS)
				{
					continue;
				}
				
				auto& rRec = stationMap[key.str].aprioriPos(key.num);
				
				auto& state = augmentedNetKFState.x(index);
				
				//augment the correction state with the value from the sinex
				state += rRec;
			}

			kfStatePointers.push_back(&augmentedNetKFState);
		}

		// push the covar matrix to the sinex object
		theSinex.kfState = mergeFilters(kfStatePointers);
	}

	// add in the files used to create the solution
	for (auto& rnxfile : acsConfig.rnx_files)	{	sinex_add_file(acsConfig.analysis_agency, time, rnxfile, "RINEX v3.x");	}
	for (auto& sp3file : acsConfig.sp3_files)	{	sinex_add_file(acsConfig.analysis_agency, time, sp3file, "SP3");		}
	for (auto& snxfile : acsConfig.snx_files)	{	sinex_add_file(acsConfig.analysis_agency, time, snxfile, "SINEX");		}

	// Add other statistics as they become available...
	sinex_add_statistic("SAMPLING INTERVAL (SECONDS)", acsConfig.epoch_interval);

	char obsCode	= 'P';	//GNSS measurements
	char constCode	= ' ';

	string solcont = "ST";
	// uncomment next bit once integrated
	// if (acsConfig.orbit_output) solcont += 'O';


	string data_agc = "";

	GTime start_time;
	start_time.time = static_cast<int>(boost::posix_time::to_time_t(acsConfig.start_epoch));
	start_time.sec	= 0;
	double	ep[6];
	int		start[3];
	int		end[3];
	time2epoch(start_time, ep);
	epoch2yds(ep, start);
	time2epoch(time, ep);
	epoch2yds(ep, end);
	struct timeval tv;
	struct tm* tmbuf;
	gettimeofday(&tv, nullptr);
	tmbuf = gmtime(&tv.tv_sec);
	int create_yds[3]; // create time for Sinex header
	create_yds[0]	= tmbuf->tm_year + 1900;
	create_yds[1]	= tmbuf->tm_yday;
	create_yds[2]	= tmbuf->tm_sec
					+ tmbuf->tm_min		* 60
					+ tmbuf->tm_hour	* 60 * 60;


	sinex_update_header(acsConfig.analysis_agency, create_yds, data_agc, start, end, obsCode, constCode, solcont, 2.02); //Change this if the sinex format gets updated

	string filename = acsConfig.sinex_filename;
	replaceTimes(filename, acsConfig.start_epoch);
	write_sinex(filename, &stationMap);
}

void sinexPerEpochPerStation(
	GTime		time,
	Station&	rec)
{
	// check the station data for currency. If later that the end time, refresh Sinex data
	double ep[6];
	int yds[3];
	int defaultStop[3] = {-1,-1,-1};

	time2epoch(time, ep);
	epoch2yds(ep, yds);

	if 	(  time_compare(rec.snx.stop, yds)			>  0
		&& time_compare(rec.snx.stop, defaultStop)	!= 0)
	{
		//already have valid data
		return;
	}
	
	int result = getstnsnx(rec.id, yds, rec.snx);

	if (result == E_SnxDataMissing::SITE_ID)
	{
// 		BOOST_LOG_TRIVIAL(error)
// 		<< "Station " << rec.id << " not found in sinex file";		//todo aaron useful eeror

		return;
	}

	if (result == E_SnxDataMissing::ESTIMATE)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "Station " << rec.id << " position not found in sinex file";

		return; // No current station position estimate!
	}

	rec.antDelta	= rec.snx.ecc;
	rec.antType		= rec.snx.anttype;
// 	rec.opt.antdel	= rec.rnxStation.del;		//todo aaron, this should come from sinex preferably, but what about monument codes....
	
	auto trace = getTraceFile(rec);
	
	// Initialise the receiver antenna information
	for (bool once : {1})
	{
		string nullstring	= "";
		string tmpant		= rec.antType;

		if (tmpant.empty())
		{
			trace
			<< "Antenna name not specified"
			<< rec.id << ": Antenna name not specified";

			break;
		}

		bool found;
		found = findAntenna(tmpant, E_Sys::GPS, time, nav, F1);
		if (found)
		{
			//all good, carry on
			rec.antId = tmpant;
			break;
		}

		// Try searching under the antenna type with DOME => NONE
		radome2none(tmpant);

		found = findAntenna(tmpant, E_Sys::GPS, time, nav, F1);
		if (found)
		{
			trace
			<< "Using \"" << tmpant
			<< "\" instead of: \"" << rec.antType
			<< "\" for radome of " << rec.id;

			rec.antId = tmpant;
			break;
		}
		else
		{
			trace
			<< "No information for antenna " << rec.antType;

			break;
		}
	}
}
