
// #pragma GCC optimize ("O0")

#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

using boost::algorithm::to_lower_copy;

#include "eigenIncluder.hpp"
#include "inputsOutputs.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "sinex.hpp"
#include "slr.hpp"

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

	sinexCheckAddGaReference("PPP Solution", "2.1", false);

	// add in the files used to create the solution
	for (auto& [id, ubxinput] : acsConfig.ubx_inputs)	{	sinexAddFiles(acsConfig.analysis_agency, time, ubxinput,			"UBX");			}
	for (auto& [id, rnxinput] : acsConfig.rnx_inputs)	{	sinexAddFiles(acsConfig.analysis_agency, time, rnxinput,			"RINEX v3.x");	}
														{	sinexAddFiles(acsConfig.analysis_agency, time, acsConfig.sp3_files,	"SP3");			}
														{	sinexAddFiles(acsConfig.analysis_agency, time, acsConfig.snx_files,	"SINEX");		}

	// Add other statistics as they become available...
	sinexAddStatistic("SAMPLING INTERVAL (SECONDS)", acsConfig.epoch_interval);

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
	Trace&		trace,
	GTime		time,
	Receiver&	rec)
{
	if (rec.id.empty())
	{
		return;
	}

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

	if 	(  rec.snx.stop	> yds
		&& rec.snx.stop	> defaultStop)
	{
		//already have valid data
		return;
	}

	string snxId = rec.id;

	if (cdpIdMap.find(rec.id) != cdpIdMap.end())
	{
		// need to use CDP ID for SLR stations if possible
		int cdpId = cdpIdMap.at(rec.id);
		assert(cdpId >= 1000);	// if fails, need to consider zero-padding in sinex files
		snxId = std::to_string(cdpId);
	}

	rec.failureEccentricity = rec.antDelta.isZero();

	auto& recOpts = acsConfig.getRecOpts(rec.id);
	{
		auto& eccModel = recOpts.eccentricityModel;
		if (rec.antDelta	.isZero()	&& eccModel.enable)				{	rec.antDelta		= recOpts.eccentricityModel.eccentricity;					rec.failureEccentricity = false;	}
		if (rec.antennaType	.empty())										rec.antennaType		= recOpts.antenna_type;
		if (rec.receiverType.empty())										rec.receiverType	= recOpts.receiver_type;
	}

	string refSys = "UNE";
	auto result = getRecSnx(snxId, time, rec.snx);
	if (!result.failureSiteId)
	{
		if (rec.antDelta	.isZero()	&& rec.snx.ecc_ptr	!= nullptr)	{	rec.antDelta		= rec.snx.ecc_ptr->ecc;		refSys = rec.snx.ecc_ptr->rs;	rec.failureEccentricity = false;	}
		if (rec.antennaType	.empty()	&& rec.snx.ant_ptr	!= nullptr)		rec.antennaType		= rec.snx.ant_ptr->type;
		if (rec.receiverType.empty()	&& rec.snx.rec_ptr	!= nullptr)		rec.receiverType	= rec.snx.rec_ptr->type;
	}

	if	( result.failureSiteId)
	{
		rec.failureSinex = true;
	}

	if	( result.failureEstimate
		&&recOpts.apriori_pos.isZero())
	{
		rec.failureAprioriPos = true;
	}

	if	( refSys != "UNE")
	{
		rec.failureEccentricity = true;

		BOOST_LOG_TRIVIAL(error)
		<< "Error: Receiver eccentricity referency system != UNE";	//todo aaron, this needs duplication elsewhere, rs unchecked
	}

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

			rec.failureAntenna = true;

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

		trace
		<< "No information for antenna " << rec.antennaType;

		rec.failureAntenna = true;
	}
}
