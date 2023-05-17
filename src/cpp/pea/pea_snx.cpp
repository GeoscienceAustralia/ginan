
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
		vector<KFState*> kfStatePointers;

		if (acsConfig.process_user)
		for (auto& [key, rec] : stationMap)
		{
			kfStatePointers.push_back(&rec.pppState);
		}
		
		KFState augmentedNetKFState;
		if	( acsConfig.process_network
			||acsConfig.process_ppp)
		{
			augmentedNetKFState = netKFState;
			
			if (acsConfig.process_network)
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
	for (auto& ubxinput : acsConfig.ubx_inputs)	{	sinex_add_file(acsConfig.analysis_agency, time, ubxinput, "UBX");			}
	for (auto& rnxinput : acsConfig.rnx_inputs)	{	sinex_add_file(acsConfig.analysis_agency, time, rnxinput, "RINEX v3.x");	}
	for (auto& sp3input : acsConfig.sp3_files)	{	sinex_add_file(acsConfig.analysis_agency, time, sp3input, "SP3");			}
	for (auto& snxinput : acsConfig.snx_files)	{	sinex_add_file(acsConfig.analysis_agency, time, snxinput, "SINEX");			}

	// Add other statistics as they become available...
	sinex_add_statistic("SAMPLING INTERVAL (SECONDS)", acsConfig.epoch_interval);

	char obsCode	= 'P';	//GNSS measurements
	char constCode	= ' ';

	string solcont = "ST";
	// uncomment next bit once integrated
	// if (acsConfig.orbit_output) solcont += 'O';

	string data_agc = "";

	PTime startTime;
	startTime.bigTime = boost::posix_time::to_time_t(acsConfig.start_epoch);		//todo aaron, make these constructors for ptime.

	updateSinexHeader(acsConfig.analysis_agency, data_agc, (GTime) startTime, time, obsCode, constCode, solcont, 2.02); //Change this if the sinex format gets updated

	string filename = acsConfig.sinex_filename;
	
	replaceTimes(filename, acsConfig.start_epoch);
	
	writeSinex(filename, stationMap);
}

void sinexPerEpochPerStation(
	GTime		time,
	Station&	rec)
{
	// check the station data for currency. If later that the end time, refresh Sinex data
	UYds yds = time;
	UYds defaultStop(-1,-1,-1);

	if 	(  time_compare(rec.snx.stop, yds)			>  0
		&& time_compare(rec.snx.stop, defaultStop)	!= 0)
	{
		//already have valid data
		return;
	}
	
	auto result = getStnSnx(rec.id, time, rec.snx);

	auto& recOpts = acsConfig.getRecOpts(rec.id);
	
	if (rec.antDelta	.isZero())		rec.antDelta		= recOpts.eccentricity;
	if (rec.antennaType	.empty())		rec.antennaType		= recOpts.antenna_type;
	if (rec.receiverType.empty())		rec.receiverType	= recOpts.receiver_type;
	
	if (rec.antDelta	.isZero())		rec.antDelta		= rec.snx.ecc_ptr->ecc;
	if (rec.antennaType	.empty())		rec.antennaType		= rec.snx.ant_ptr->type;
	if (rec.receiverType.empty())		rec.receiverType	= rec.snx.rec_ptr->type;
	
	auto trace = getTraceFile(rec);
	
	// Initialise the receiver antenna information
	for (bool once : {1})
	{
		string nullstring	= "";
		string tmpant		= rec.antennaType;

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
			rec.antennaId = tmpant;
			break;
		}

		// Try searching under the antenna type with DOME => NONE
		radome2none(tmpant);

		found = findAntenna(tmpant, E_Sys::GPS, time, nav, F1);
		if (found)
		{
			trace
			<< "Using '" << tmpant
			<< "' instead of: '" << rec.antennaType
			<< "' for radome of " << rec.id;

			rec.antennaId = tmpant;
			break;
		}
		else
		{
			trace
			<< "No information for antenna " << rec.antennaType;

			break;
		}
	}

	if	( result.failureEstimate
		&&recOpts.apriori_pos.isZero())
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Station " << rec.id << " position not found in sinex or yaml files";

		return; // No current station position estimate!
	}
	
	if (result.failureSiteId)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Station " << rec.id << " not found in sinex file";

		return;
	}
}
