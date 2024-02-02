
// #pragma GCC optimize ("O0")

#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

using boost::algorithm::to_lower_copy;

#include "eigenIncluder.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "sinex.hpp"

void getStationsFromSinex(
	map<string, Receiver>&	receiverMap,
	KFState&				kfState)
{

}

void sinexPostProcessing(
	GTime						time,
	map<string, Receiver>&		receiverMap,
	KFState&					netKFState)
{
	theSinex.inputFiles.		clear();
	theSinex.acknowledgements.	clear();
	theSinex.inputHistory.		clear();

	sinex_check_add_ga_reference("PPP Solution", "2.1", false);

	// add in the files used to create the solution
	for (auto& [id, ubxinput] : acsConfig.ubx_inputs)	{	sinex_add_files(acsConfig.analysis_agency, time, ubxinput,				"UBX");			}
	for (auto& [id, rnxinput] : acsConfig.rnx_inputs)	{	sinex_add_files(acsConfig.analysis_agency, time, rnxinput,				"RINEX v3.x");	}
														{	sinex_add_files(acsConfig.analysis_agency, time, acsConfig.sp3_files,	"SP3");			}
														{	sinex_add_files(acsConfig.analysis_agency, time, acsConfig.snx_files,	"SINEX");		}

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

	KFState sinexSubstate = mergeFilters({&netKFState}, {KF::ONE, KF::REC_POS, KF::REC_POS_RATE});

	updateSinexHeader(acsConfig.analysis_agency, data_agc, (GTime) startTime, time, obsCode, constCode, solcont, sinexSubstate.x.rows() - 1, 2.02); //Change this if the sinex format gets updated

	string filename = acsConfig.sinex_filename;

	replaceTimes(filename, acsConfig.start_epoch);

	writeSinex(filename, sinexSubstate, receiverMap);
}

void sinexPerEpochPerStation(
	GTime		time,
	Receiver&	rec)
{
	{
		auto& solEpoch = theSinex.solEpochMap[rec.id];

		solEpoch.sitecode 	= rec.id;
		solEpoch.typecode	= '-';
		solEpoch.ptcode		= "A";
		solEpoch.solnnum	= "0";
		if ((GTime) solEpoch.start == GTime::noTime())		solEpoch.start	= time;
															solEpoch.end	= time;
															solEpoch.mean	= (GTime) solEpoch.start
																			+ ((GTime)solEpoch.end - (GTime)solEpoch.start).to_double() / 2;
	}

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

	if (rec.antDelta	.isZero())		rec.antDelta		= recOpts.eccentricityModel.eccentricity;
	if (rec.antennaType	.empty())		rec.antennaType		= recOpts.antenna_type;
	if (rec.receiverType.empty())		rec.receiverType	= recOpts.receiver_type;

	if (rec.antDelta	.isZero())		rec.antDelta		= rec.snx.ecc_ptr->ecc;
	if (rec.antennaType	.empty())		rec.antennaType		= rec.snx.ant_ptr->type;
	if (rec.receiverType.empty())		rec.receiverType	= rec.snx.rec_ptr->type;

	if (rec.receiverType.empty() == false)
	{
		string receiverType = to_lower_copy(rec.receiverType);
		receiverType = receiverType.substr(0, receiverType.find(" "));

		auto [it, inserted] = acsConfig.customAliasesMap[rec.id].insert(receiverType);
		if (inserted)
		{
			auto& baseRecOpts = acsConfig.getRecOpts((string) "_" + rec.id);

			for (auto& [id, inheritor] : baseRecOpts.inheritors)
			{
				inheritor->_initialised = false;
			}
		}
	}

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
		<< "Receiver " << rec.id << " position not found in sinex or yaml files";

		return; // No current station position estimate!
	}

	if (result.failureSiteId)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Receiver " << rec.id << " not found in sinex file";

		return;
	}
}
