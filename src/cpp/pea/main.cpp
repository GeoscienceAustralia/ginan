
// #pragma GCC optimize ("O0")

#include <sys/time.h>
#include <filesystem>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <memory>
#include <chrono>
#include <thread>
#include <string>

#ifdef ENABLE_PARALLELISATION
	#include "omp.h"
#endif

using namespace std::literals::chrono_literals;
using std::this_thread::sleep_for;
using std::chrono::system_clock;
using std::chrono::time_point;
using std::make_unique;
using std::make_shared;
using std::string;

#include <boost/log/utility/setup/console.hpp>
#include <boost/system/error_code.hpp>
#include <boost/log/trivial.hpp>

#include "centerMassCorrections.hpp"
#include "minimumConstraints.hpp"
#include "peaCommitStrings.hpp"
#include "ntripBroadcast.hpp"
#include "rinexNavWrite.hpp"
#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "oceanPoleTide.hpp"
#include "algebraTrace.hpp"
#include "rtsSmoothing.hpp"
#include "preprocessor.hpp"
#include "streamCustom.hpp"
#include "ntripSocket.hpp"
#include "rtcmEncoder.hpp"
#include "sinexParser.hpp"
#include "streamRinex.hpp"
#include "coordinates.hpp"
#include "staticField.hpp"
#include "geomagField.hpp"
#include "binaryStore.hpp"
#include "orbexWrite.hpp"
#include "mongoWrite.hpp"
#include "GNSSambres.hpp"
#include "instrument.hpp"
#include "streamFile.hpp"
#include "streamRtcm.hpp"
#include "tropModels.hpp"
#include "ephPrecise.hpp"
#include "mongoRead.hpp"
#include "acsConfig.hpp"
#include "testUtils.hpp"
#include "streamUbx.hpp"
#include "streamSlr.hpp"
#include "streamSp3.hpp"
#include "tideCoeff.hpp"
#include "orbitProp.hpp"
#include "ionoModel.hpp"
#include "ionModels.hpp"
#include "ephemeris.hpp"
#include "sp3Write.hpp"
#include "metaData.hpp"
#include "attitude.hpp"
#include "receiver.hpp"
#include "posProp.hpp"
#include "summary.hpp"
#include "antenna.hpp"
#include "satStat.hpp"
#include "fileLog.hpp"
#include "jpl_eph.hpp"
#include "biases.hpp"
#include "common.hpp"
#include "orbits.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "debug.hpp"
#include "sinex.hpp"
#include "tides.hpp"
#include "cost.hpp"
#include "enums.h"
#include "aod.hpp"
#include "ppp.hpp"
#include "gpx.hpp"
#include "slr.hpp"
#include "api.hpp"

Navigation				nav		= {};
int						epoch	= 1;
GTime					tsync	= GTime::noTime();
map<int, SatIdentity>	satIdMap;
ReceiverMap				receiverMap;

void outputMqtt(KFState& kfState);

bool fileChanged(
	string filename)
{
	bool valid = checkValidFile(filename);
	if (valid == false)
	{
		return false;
	}

	std::filesystem::file_time_type modifyTime;
	try
	{
		modifyTime = std::filesystem::last_write_time(filename);
	}
	catch (...)
	{
		//failed despite just checking it exists.. sometimes happens apparently
		return false;
	}

	auto it = acsConfig.configModifyTimeMap.find(filename);
	if (it == acsConfig.configModifyTimeMap.end())
	{
		//the first time this file has been read,
		//update then return true
		acsConfig.configModifyTimeMap[filename] = modifyTime;

		return true;
	}

	auto& [dummy, readTime] = *it;
	if (readTime != modifyTime)
	{
		//has a different modification time, update then return true
		readTime = modifyTime;

		return true;
	}

	//has been read with this time before
	return false;
}

void removeInvalidFiles(
	vector<string>& files)
{
	for (auto it = files.begin(); it != files.end(); )
	{
		auto& filename = *it;
		bool valid = checkValidFile(filename);
		if (valid == false)
		{
			it = files.erase(it);
		}
		else
		{
			it++;
		}
	}
}

void initialiseStation(
	string		id,
	Receiver&	rec)
{
	if (rec.id.empty() == false)
	{
		//already initialised
		return;
	}

	BOOST_LOG_TRIVIAL(info)
	<< "Initialising station " << id;

	Instrument	instrument(__FUNCTION__);

	rec.id = id;

	// Read the OTL BLQ file
	bool found = false;
	for (auto& blqfile : acsConfig.ocean_tide_loading_blq_files)
	{
		found |= readBlq(blqfile, rec, E_LoadingType::OCEAN);
	}

	if (found == false)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: No OTL BLQ for " << id;
	}

	// Read the ATL BLQ file
	found = false;
	for (auto& blqfile : acsConfig.atmos_tide_loading_blq_files)
	{
		found |= readBlq(blqfile, rec, E_LoadingType::ATMOSPHERIC);
	}

	if (found == false)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: No ATL BLQ for " << id;
	}
}

/** Create a station object from an input
*/
void addStationData(
	string			stationId,			///< Id of station to add data for
	vector<string>	inputNames,			///< Filename to create station from
	string			inputFormat,		///< Type of data in file
	string			dataType)			///< Type of data
{
	for (auto& inputName : inputNames)
	{
		if (streamDOAMap.find(inputName) != streamDOAMap.end())
		{
			//this stream was already added, dont re-add
			continue;
		}

		string mountpoint;
		auto lastSlashPos = inputName.find_last_of('/');
		if (lastSlashPos == string::npos)		{	mountpoint = inputName;								}
		else									{	mountpoint = inputName.substr(lastSlashPos + 1);	};

		string id = stationId;

		if (id == "<AUTO>")
		{
			id = mountpoint.substr(0,4);
		}

		boost::algorithm::to_upper(id);

		auto& recOpts = acsConfig.getRecOpts(id);

		if	(  recOpts.exclude
			|| recOpts.kill)
		{
			return;
		}

		string protocol;
		string subInputName;
		auto protocolPos = inputName.find("://");
		if (protocolPos == string::npos)	{	protocol = "file";								subInputName = inputName;							}
		else								{	protocol = inputName.substr(0, protocolPos);	subInputName = inputName.substr(protocolPos + 3);	}

		std::unique_ptr<Stream>			stream_ptr;
		std::unique_ptr<Parser>			parser_ptr;

		if	( protocol == "file"
			||protocol == "serial")
		{
			if (checkValidFile(subInputName, dataType) == false)
			{
				return;
			}
		}

		if		(protocol == "file")		{	stream_ptr = make_unique<FileStream>	(subInputName);	}
		else if (protocol == "serial")		{	stream_ptr = make_unique<SerialStream>	(subInputName);	}
		else if (protocol == "http")		{	stream_ptr = make_unique<NtripStream>	(inputName);	}
		else if (protocol == "https")		{	stream_ptr = make_unique<NtripStream>	(inputName);	}
		else if (protocol == "ntrip")		{	stream_ptr = make_unique<NtripStream>	(inputName);	}
		else
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: Invalid protocol " << protocol;
		}

		if		(inputFormat == "RINEX")	{	parser_ptr = make_unique<RinexParser>	();	static_cast<RinexParser*>	(parser_ptr.get())->rnxRec.id		= id;			}
		else if	(inputFormat == "UBX")		{	parser_ptr = make_unique<UbxParser>		();	static_cast<UbxParser*>		(parser_ptr.get())->recId			= id;			}
		else if	(inputFormat == "CUSTOM")	{	parser_ptr = make_unique<CustomParser>	();	static_cast<CustomParser*>	(parser_ptr.get())->recId			= id;			}
		else if (inputFormat == "RTCM")		{	parser_ptr = make_unique<RtcmParser>	();	static_cast<RtcmParser*>	(parser_ptr.get())->rtcmMountpoint	= mountpoint;	if (id == "QZSL6") { static_cast<RtcmParser*> (parser_ptr.get())->qzssL6 = true;}}
		else if (inputFormat == "SP3")		{	parser_ptr = make_unique<Sp3Parser>		();	}
		else if (inputFormat == "SINEX")	{	parser_ptr = make_unique<SinexParser>	();	}
		else if (inputFormat == "SLR")		{	parser_ptr = make_unique<SlrParser>		();	}
		else
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: Invalid inputFormat " << inputFormat;
		}

		shared_ptr<StreamParser> streamParser_ptr;

		if		(dataType == "OBS")		streamParser_ptr = make_shared<ObsStream>	(std::move(stream_ptr), std::move(parser_ptr));
		else if	(dataType == "PSEUDO")	streamParser_ptr = make_shared<ObsStream>	(std::move(stream_ptr), std::move(parser_ptr));
		else							streamParser_ptr = make_shared<StreamParser>(std::move(stream_ptr), std::move(parser_ptr));

		if (dataType == "OBS")
		{
			auto& rec = receiverMap[id];

			initialiseStation(id, rec);
		}

		streamParser_ptr->stream.sourceString = inputName;

		streamParserMultimap.insert({id, std::move(streamParser_ptr)});

		streamDOAMap[inputName] = false;
	}
}

void reloadInputFiles()
{
	removeInvalidFiles(acsConfig.atx_files);
	for (auto& atxfile : acsConfig.atx_files)
	{
		if (fileChanged(atxfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading ATX file " << atxfile;

		readantexf(atxfile, nav);
	}

	removeInvalidFiles(acsConfig.sp3_files);
	for (auto& sp3file : acsConfig.sp3_files)
	{
		if (fileChanged(sp3file) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading SP3 file " << sp3file;

		readSp3ToNav(sp3file, nav, 0);
	}

	removeInvalidFiles(acsConfig.obx_files);
	for (auto& obxfile : acsConfig.obx_files)
	{
		if (fileChanged(obxfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading OBX file " << obxfile;

		readOrbex(obxfile, nav);
	}

	removeInvalidFiles(acsConfig.nav_files);
	for (auto& navfile : acsConfig.nav_files)
	{
		if (fileChanged(navfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading NAV file " << navfile;

		auto rinexStream = make_unique<StreamParser>(make_unique<FileStream>(navfile), make_unique<RinexParser>());

		rinexStream->parse();
	}

	removeInvalidFiles(acsConfig.erp_files);
	for (auto& erpfile : acsConfig.erp_files)
	{
		if (fileChanged(erpfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading ERP file " << erpfile;

		readErp(erpfile, nav.erp);
	}

	removeInvalidFiles(acsConfig.clk_files);
	for (auto& clkfile : acsConfig.clk_files)
	{
		if (fileChanged(clkfile) == false)
		{
			continue;
		}

		/* CLK file - RINEX 3 */
		BOOST_LOG_TRIVIAL(info)
		<< "Loading CLK file " << clkfile;

		auto rinexStream = make_unique<StreamParser>(make_unique<FileStream>(clkfile), make_unique<RinexParser>());

		rinexStream->parse();
	}

	removeInvalidFiles(acsConfig.dcb_files);
	for (auto& dcbfile : acsConfig.dcb_files)
	{
		if (fileChanged(dcbfile) == false)
		{
			continue;
		}

		/* DCB file */
		BOOST_LOG_TRIVIAL(info)
		<< "Loading DCB file " << dcbfile;

		readdcb(dcbfile);
	}

	removeInvalidFiles(acsConfig.bsx_files);
	for (auto& bsxfile : acsConfig.bsx_files)
	{
		if (fileChanged(bsxfile) == false)
		{
			continue;
		}

		/* BSX file*/
		BOOST_LOG_TRIVIAL(info)
		<< "Loading BSX file " << bsxfile;

		readBiasSinex(bsxfile);
	}

	removeInvalidFiles(acsConfig.ion_files);
	for (auto& ionfile : acsConfig.ion_files)
	{
		if (fileChanged(ionfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading ION file " << ionfile;

		readTec(ionfile, &nav);
	}

	removeInvalidFiles(acsConfig.igrf_files);
	for (auto& igrffile : acsConfig.igrf_files)
	{
		if (fileChanged(igrffile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading IGRF file " << igrffile;

		readIGRF(igrffile);
	}

	static bool once = true;
	removeInvalidFiles(acsConfig.snx_files);
	for (auto& snxfile : acsConfig.snx_files)
	{
		if (fileChanged(snxfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading SNX file " <<  snxfile;

		bool pass = readSinex(snxfile, once);
		if (pass == false)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error: Unable to load SINEX file " << snxfile;

			continue;
		}

		once = false;
	}

	removeInvalidFiles(acsConfig.vmf_files);
	for (auto& vmffile : acsConfig.vmf_files)
	{
		if (fileChanged(vmffile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading VMF file " << vmffile;

		readvmf3(vmffile);
	}

	removeInvalidFiles(acsConfig.orography_files);
	for (auto& orographyfile : acsConfig.orography_files)
	{
		if (fileChanged(orographyfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading ORO from " << orographyfile;

		readorog(orographyfile);
	}

	removeInvalidFiles(acsConfig.gpt2grid_files);
	for (auto& gpt2gridfile : acsConfig.gpt2grid_files)
	{
		if (fileChanged(gpt2gridfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading GPT file " << gpt2gridfile;

		readgrid(gpt2gridfile);
	}

	removeInvalidFiles(acsConfig.ocean_pole_tide_loading_files);
	for (auto& optfile : acsConfig.ocean_pole_tide_loading_files)
	{
		if (fileChanged(optfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading Ocean Pole Tide file " << optfile;

		readOceanPoleCoeff(optfile);
	}

	removeInvalidFiles(acsConfig.sid_files); // satellite ID (sp3c code) data
	for (auto& sidfile : acsConfig.sid_files)
	{
		if (fileChanged(sidfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading Sat ID file " << sidfile;

		readSatId(sidfile);
	}

	removeInvalidFiles(acsConfig.crd_files); // SLR observation data
	for (auto& crdfile : acsConfig.crd_files)
	{
		if (fileChanged(crdfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading CRD file " << crdfile;

		readCrd(crdfile);
	}

	if (acsConfig.output_slr_obs)
	{
		slrObsFiles = outputSortedSlrObs(); // CRD files need to be parsed before sorted .slr_obs files are exported
	}

	removeInvalidFiles(acsConfig.com_files); // centre-of-mass data
	for (auto& comfile : acsConfig.com_files)
	{
		if (fileChanged(comfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading CoM file " << comfile;

		readCom(comfile);
	}

	removeInvalidFiles(acsConfig.egm_files);
	for (auto& egmfile : acsConfig.egm_files)
	{
		if (fileChanged(egmfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading EGM file " << egmfile;

		egm.read(egmfile, acsConfig.propagationOptions.egm_degree);
	}

	removeInvalidFiles(acsConfig.ocean_tide_potential_files);
	for (auto& tidefile : acsConfig.ocean_tide_potential_files)
	{
		if (fileChanged(tidefile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading Tide file " << tidefile;

		oceanTide.read(tidefile, acsConfig.propagationOptions.egm_degree);
	}

	removeInvalidFiles(acsConfig.atmos_tide_potential_files);
	for (auto& tidefile : acsConfig.atmos_tide_potential_files)
	{
		if (fileChanged(tidefile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading Tide file " << tidefile;

		atmosphericTide.read(tidefile, acsConfig.propagationOptions.egm_degree);
	}

	removeInvalidFiles(acsConfig.cmc_files);
	for (auto& cmcfile : acsConfig.cmc_files)
	{
		if (fileChanged(cmcfile) == false)
		{
			continue;
		}
		BOOST_LOG_TRIVIAL(info) << "Loading CMC file " << cmcfile;

		cmc.read(cmcfile);
	}

	removeInvalidFiles(acsConfig.hfeop_files);
	for (auto& hfeopfile : acsConfig.hfeop_files)
	{
		if (fileChanged(hfeopfile) == false)
		{
			continue;
		}
		BOOST_LOG_TRIVIAL(info) << "Loading HFEOP file " << hfeopfile;

		hfEop.read(hfeopfile);
	}

	removeInvalidFiles(acsConfig.atmos_oceean_dealiasing_files);
	for (auto& aod1b_file : acsConfig.atmos_oceean_dealiasing_files)
	{
		if (fileChanged(aod1b_file) == false)
		{
			continue;
		}
		BOOST_LOG_TRIVIAL(info) << "Loading AOD file " << aod1b_file;

		aod.read(aod1b_file, acsConfig.propagationOptions.egm_degree);
	}

	removeInvalidFiles(acsConfig.ocean_pole_tide_potential_files);
	for (auto& poleocean_file : acsConfig.ocean_pole_tide_potential_files)
	{
		if (fileChanged(poleocean_file) == false)
		{
			continue;
		}
		BOOST_LOG_TRIVIAL(info) << "Loading Pole Ocean Tide file " << poleocean_file;

		oceanPoleTide.read(poleocean_file, acsConfig.propagationOptions.egm_degree);
	}

	removeInvalidFiles(acsConfig.planetary_ephemeris_files);
	for (auto& jplfile : acsConfig.planetary_ephemeris_files)
	{
		if (fileChanged(jplfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading planetary ephemeris file " << jplfile;

		nav.jplEph_ptr = (struct jpl_eph_data*) jpl_init_ephemeris(jplfile.c_str(), nullptr, nullptr);          // a Pointer to The jpl_eph_data Structure

		if (jpl_init_error_code())
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: planetary ephemeris file had error code " << jpl_init_error_code();
		}
	}

	for (auto& [id, slrinputs]			: slrObsFiles)					{	addStationData(id,		slrinputs,					"SLR",		"OBS");			}
	for (auto& [id, ubxinputs]			: acsConfig.ubx_inputs)			{	addStationData(id,		ubxinputs,					"UBX",		"OBS");			}
	for (auto& [id, custominputs]		: acsConfig.custom_inputs)		{	addStationData(id,		custominputs,				"CUSTOM",	"OBS");			}
	for (auto& [id, rnxinputs]			: acsConfig.rnx_inputs)			{	addStationData(id,		rnxinputs,					"RINEX",	"OBS");			}
	for (auto& [id, rtcminputs]			: acsConfig.obs_rtcm_inputs)	{	addStationData(id,		rtcminputs,					"RTCM",		"OBS");			}
	for (auto& [id, pseudosp3inputs]	: acsConfig.pseudo_sp3_inputs)	{	addStationData(id,		pseudosp3inputs,			"SP3",		"PSEUDO");		}
	for (auto& [id, pseudosnxinputs]	: acsConfig.pseudo_snx_inputs)	{	addStationData(id,		pseudosnxinputs,			"SINEX",	"PSEUDO");		}
																		{	addStationData("Nav",	acsConfig.nav_rtcm_inputs,	"RTCM",		"NAV");			}
																		{	addStationData("QZSL6",	acsConfig.qzs_rtcm_inputs,	"RTCM",		"NAV");			}
}

void configureUploadingStreams()
{
	for (auto& [outLabel, outStreamData] : acsConfig.netOpts.uploadingStreamData)
	{
		auto it = ntripBroadcaster.ntripUploadStreams.find(outLabel);

		// Create stream if it does not already exist.
		if (it == ntripBroadcaster.ntripUploadStreams.end())
		{
			auto outStream_ptr = std::make_shared<NtripUploader>(outStreamData.url);
			auto& outStream = *outStream_ptr.get();
			ntripBroadcaster.ntripUploadStreams[outLabel] = std::move(outStream_ptr);

			it = ntripBroadcaster.ntripUploadStreams.find(outLabel);
		}

		auto& [label, outStream_ptr]	= *it;
		auto& outStream					= *outStream_ptr;

		outStream.streamConfig.rtcmMsgOptsMap		= outStreamData.rtcmMsgOptsMap;
		outStream.streamConfig.itrf_datum 			= outStreamData.itrf_datum;
		outStream.streamConfig.provider_id 			= outStreamData.provider_id;
		outStream.streamConfig.solution_id 			= outStreamData.solution_id;
	}

	for (auto it = ntripBroadcaster.ntripUploadStreams.begin(); it != ntripBroadcaster.ntripUploadStreams.end();)
	{
		if (acsConfig.netOpts.uploadingStreamData.find(it->first) == acsConfig.netOpts.uploadingStreamData.end())
		{
			auto& [label, outStream_ptr]	= *it;
			auto& outStream					= *outStream_ptr;
			outStream.disconnect();
			it = ntripBroadcaster.ntripUploadStreams.erase(it);
		}
		else
		{
			it++;
		}
	}

	if	( acsConfig.process_ppp				== false
		&&acsConfig.process_spp				== false
		&&acsConfig.slrOpts.process_slr		== false
		&&acsConfig.process_preprocessor	== false
		&&acsConfig.process_ionosphere		== false)
	while (1)
	{
		BOOST_LOG_TRIVIAL(info) << "Running with no processing modes enabled";

		sleep_for(std::chrono::seconds(10));
	}
}


/** Create directories if required
*/
void createDirectories(
	boost::posix_time::ptime	logptime)
{
	// Ensure the output directories exist
	for (auto directory : {
								acsConfig.sp3_directory,
								acsConfig.erp_directory,
								acsConfig.gpx_directory,
								acsConfig.log_directory,
								acsConfig.cost_directory,
								acsConfig.ionex_directory,
								acsConfig.orbex_directory,
								acsConfig.sinex_directory,
								acsConfig.trace_directory,
								acsConfig.clocks_directory,
								acsConfig.slr_obs_directory,
								acsConfig.ionstec_directory,
								acsConfig.rtcm_nav_directory,
								acsConfig.rtcm_obs_directory,
								acsConfig.orbit_ics_directory,
								acsConfig.rinex_obs_directory,
								acsConfig.rinex_nav_directory,
								acsConfig.raw_custom_directory,
								acsConfig.trop_sinex_directory,
								acsConfig.bias_sinex_directory,
								acsConfig.pppOpts.rts_directory,
								acsConfig.decoded_rtcm_json_directory,
								acsConfig.encoded_rtcm_json_directory,
								acsConfig.network_statistics_json_directory
							})
	{
		replaceTimes(directory, logptime);

		if (directory == ".")	continue;
		if (directory == "./")	continue;
		if (directory.empty())	continue;

		try
		{
			std::filesystem::create_directories(directory);
		}
		catch (...)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: Could not create directory: \"" << directory << "\"";
		}
	}
}

map<string, string> fileNames;

/** Create new empty trace files only when required when the filename is changed
*/
void createTracefiles(
	map<string, Receiver>&	receiverMap,
	Network&				pppNet,
	Network&				ionNet)
{
	boost::posix_time::ptime logptime = currentLogptime();
	createDirectories(logptime);

	startNewMongoDb("PRIMARY",		logptime,	acsConfig.mongoOpts[E_Mongo::PRIMARY]	.database,	E_Mongo::PRIMARY);
	startNewMongoDb("SECONDARY",	logptime,	acsConfig.mongoOpts[E_Mongo::SECONDARY]	.database,	E_Mongo::SECONDARY);

	for (auto rts : {false, true})
	{
		if	(	rts
			&&(	acsConfig.process_rts		== false
			||acsConfig.pppOpts.rts_lag	== 0))
		{
			continue;
		}


		string suff		= "";
		string metaSuff	= "";

		if (rts)
		{
			suff		= acsConfig.pppOpts.rts_smoothed_suffix;
			metaSuff	= SMOOTHED_SUFFIX;

			if (acsConfig.process_ppp)
			{
				bool newTraceFile = createNewTraceFile(pppNet.id,	boost::posix_time::not_a_date_time,	acsConfig.pppOpts.rts_filename,		pppNet.kfState.rts_basename);

				if (newTraceFile)
				{
	// 				std::cout << std::endl << "new trace file";
					std::remove((pppNet.kfState.rts_basename					).c_str());
					std::remove((pppNet.kfState.rts_basename + FORWARD_SUFFIX	).c_str());
					std::remove((pppNet.kfState.rts_basename + BACKWARD_SUFFIX	).c_str());
				}
			}

			if (acsConfig.process_ionosphere)
			{
				bool newTraceFile = createNewTraceFile(ionNet.id,	boost::posix_time::not_a_date_time,	acsConfig.pppOpts.rts_filename,		ionNet.kfState.rts_basename);

				if (newTraceFile)
				{
	// 				std::cout << std::endl << "new trace file";
					std::remove((ionNet.kfState.rts_basename					).c_str());
					std::remove((ionNet.kfState.rts_basename + FORWARD_SUFFIX	).c_str());
					std::remove((ionNet.kfState.rts_basename + BACKWARD_SUFFIX	).c_str());
				}
			}
		}

		bool newTraceFile = false;

		for (auto& [Sat, satNav] : nav.satNavMap)
		{
			if	(  acsConfig.output_satellite_trace
				&& suff.empty())
			{
				newTraceFile |= createNewTraceFile(Sat,			logptime,	acsConfig.satellite_trace_filename		+ suff,	satNav.traceFilename,										true,	acsConfig.output_config);
			}
		}

		for (auto& [id, rec] : receiverMap)
		{
			if (acsConfig.output_receiver_trace)
			{
				newTraceFile |= createNewTraceFile(id,				logptime,	acsConfig.receiver_trace_filename	+ suff,	rec.metaDataMap[TRACE_FILENAME_STR			+ metaSuff],	true,	acsConfig.output_config);

				if (suff.empty())
				{
					rec.traceFilename = rec.metaDataMap[TRACE_FILENAME_STR];
				}
			}

			if (acsConfig.output_json_trace)
			{
				newTraceFile |= createNewTraceFile(id,				logptime,	acsConfig.receiver_trace_filename + "_json"	+ suff,	rec.metaDataMap[JSON_FILENAME_STR			+ metaSuff]);

				if (suff.empty())
				{
					rec.jsonTraceFilename = rec.metaDataMap[JSON_FILENAME_STR];
				}
			}

			if (acsConfig.output_cost)
			{
				newTraceFile |= createNewTraceFile(id,			logptime,	acsConfig.cost_filename				+ suff,	pppNet.kfState	.metaDataMap[COST_FILENAME_STR	+ id	+ metaSuff]);
			}

			if (acsConfig.output_gpx)
			{
				newTraceFile |= createNewTraceFile(id,			logptime,	acsConfig.gpx_filename				+ suff,	pppNet.kfState	.metaDataMap[GPX_FILENAME_STR	+ id	+ metaSuff]);
			}
		}

		if (acsConfig.output_network_trace)
		{
			newTraceFile |= createNewTraceFile(pppNet.id,	logptime,	acsConfig.network_trace_filename	+ suff,	pppNet.kfState.metaDataMap[TRACE_FILENAME_STR		+ metaSuff],	true,	acsConfig.output_config);

			if (suff.empty())
			{
				pppNet.traceFilename = pppNet.kfState.metaDataMap[TRACE_FILENAME_STR];
			}
		}

		if (acsConfig.output_ionosphere_trace)
		{
			newTraceFile |= createNewTraceFile("IONO",	logptime,	acsConfig.ionosphere_trace_filename	+ suff,	ionNet.kfState.metaDataMap[TRACE_FILENAME_STR	+ metaSuff],	true,	acsConfig.output_config);

			if (suff.empty())
			{
				ionNet.traceFilename = ionNet.kfState.metaDataMap[TRACE_FILENAME_STR];
			}
		}

		if (acsConfig.output_ionex)
		{
			newTraceFile |= createNewTraceFile("",			logptime,	acsConfig.ionex_filename			+ suff,	pppNet.kfState.metaDataMap[IONEX_FILENAME_STR	+ metaSuff]);
		}

		if (acsConfig.output_ionstec)
		{
			newTraceFile |= createNewTraceFile("",			logptime,	acsConfig.ionstec_filename			+ suff,	pppNet.kfState.metaDataMap[IONSTEC_FILENAME_STR	+ metaSuff]);
		}

		if (acsConfig.output_trop_sinex)
		{
			newTraceFile |= createNewTraceFile(pppNet.id,	logptime,	acsConfig.trop_sinex_filename		+ suff,	pppNet.kfState.metaDataMap[TROP_FILENAME_STR	+ metaSuff]);
		}

		if (acsConfig.output_bias_sinex)
		{
			newTraceFile |= createNewTraceFile(pppNet.id, 	logptime,	acsConfig.bias_sinex_filename		+ suff,	pppNet.kfState.metaDataMap[BSX_FILENAME_STR		+ metaSuff]);
			newTraceFile |= createNewTraceFile(pppNet.id, 	logptime,	acsConfig.bias_sinex_filename		+ suff,	ionNet.kfState.metaDataMap[BSX_FILENAME_STR		+ metaSuff]);
		}

		if (acsConfig.output_erp)
		{
			newTraceFile |= createNewTraceFile(pppNet.id,	logptime,	acsConfig.erp_filename				+ suff,	pppNet.kfState.metaDataMap[ERP_FILENAME_STR		+ metaSuff]);
		}

		if (acsConfig.output_clocks)
		{
			auto singleFilenameMap	= getSysOutputFilenames(acsConfig.clocks_filename,	tsync, false);
			auto filenameMap		= getSysOutputFilenames(acsConfig.clocks_filename,	tsync);
			for (auto& [filename, dummy] : filenameMap)
			{
				newTraceFile |= createNewTraceFile(pppNet.id,	logptime,	filename + suff,				fileNames[filename + metaSuff]);
			}

			pppNet.kfState.metaDataMap[CLK_FILENAME_STR	+ metaSuff] = singleFilenameMap.begin()->first + suff;
		}

		if (acsConfig.output_sp3)
		{
			auto singleFilenameMap	= getSysOutputFilenames(acsConfig.sp3_filename,	tsync, false);
			auto filenameMap		= getSysOutputFilenames(acsConfig.sp3_filename,	tsync);
			for (auto& [filename, dummy] : filenameMap)
			{
				newTraceFile |= createNewTraceFile(pppNet.id,	logptime,	filename + suff,				fileNames[filename + metaSuff]);
			}

			pppNet.kfState.metaDataMap[SP3_FILENAME_STR	+ metaSuff] = singleFilenameMap.begin()->first + suff;
		}

		if (acsConfig.output_orbex)
		{
			auto singleFilenameMap	= getSysOutputFilenames(acsConfig.orbex_filename,	tsync, false);
			auto filenameMap		= getSysOutputFilenames(acsConfig.orbex_filename,	tsync);
			for (auto& [filename, dummy] : filenameMap)
			{
				newTraceFile |= createNewTraceFile(pppNet.id,	logptime,	filename + suff,				fileNames[filename + metaSuff]);
			}

			pppNet.kfState.metaDataMap[ORBEX_FILENAME_STR	+ metaSuff] = singleFilenameMap.begin()->first + suff;
		}

		if	(  rts
			&& newTraceFile)
		{
			spitFilterToFile(pppNet.kfState.metaDataMap, E_SerialObject::METADATA, pppNet.kfState.rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
		}
	}

	if (acsConfig.output_log)
	{
		createNewTraceFile("",			logptime,	acsConfig.log_filename,								FileLog::path_log);
	}

	if (acsConfig.output_ntrip_log)
	{
		for (auto& [id, stream_ptr] : ntripBroadcaster.ntripUploadStreams)
		{
			auto& stream = *stream_ptr;

			createNewTraceFile(id,			logptime,	acsConfig.ntrip_log_filename,					stream.networkTraceFilename);
		}

		for (auto& [id, streamParser_ptr] : streamParserMultimap)
		try
		{
			auto& ntripStream = dynamic_cast<NtripStream&>(streamParser_ptr->stream);

			createNewTraceFile(id,			logptime,	acsConfig.ntrip_log_filename,					ntripStream.networkTraceFilename);
		}
		catch(...){}
	}

	if (acsConfig.output_rinex_obs)
	for (auto& [id, rec] : receiverMap)
	{
		auto filenameMap = getSysOutputFilenames(acsConfig.rinex_obs_filename,	tsync, true, id);
		for (auto& [filename, dummy] : filenameMap)
		{
			createNewTraceFile(id,		logptime,	filename,											fileNames[filename]);
		}
	}

	if (acsConfig.output_rinex_nav)
	{
		auto filenameMap = getSysOutputFilenames(acsConfig.rinex_nav_filename,	tsync);
		for (auto& [filename, dummy] : filenameMap)
		{
			createNewTraceFile("Navs",	logptime,	filename,											fileNames[filename]);
		}
	}

	for (auto& [id, streamParser_ptr] : streamParserMultimap)
	try
	{
		auto& rtcmParser = dynamic_cast<RtcmParser&>(streamParser_ptr->parser);

		if (acsConfig.output_decoded_rtcm_json)
		{
			string filename = acsConfig.decoded_rtcm_json_filename;

			replaceString(filename, "<STREAM>",		rtcmParser.rtcmMountpoint);

			createNewTraceFile(id, 		logptime,	filename,	rtcmParser.rtcmTraceFilename);
		}

		for (auto nav : {false, true})
		{
			bool isNav = true;
			try
			{
				auto& obsStream = dynamic_cast<ObsStream&>(*streamParser_ptr);

				isNav = false;
			}
			catch(...){}

			if	( (acsConfig.record_rtcm_nav && isNav == true	&& nav == true)
				||(acsConfig.record_rtcm_obs && isNav == false	&& nav == false))
			{
				string filename;

				if (nav)	filename = acsConfig.rtcm_nav_filename;
				else		filename = acsConfig.rtcm_obs_filename;

				replaceString(filename, "<STREAM>",		rtcmParser.rtcmMountpoint);

				createNewTraceFile(id, 		logptime,	filename,	rtcmParser.rtcm_filename);
			}
		}
	}
	catch(...){}

	for (auto& [id, streamParser_ptr] : streamParserMultimap)
	try
	{
		auto& ubxParser = dynamic_cast<UbxParser&>(streamParser_ptr->parser);

		if (acsConfig.record_raw_ubx)
		{
			string filename = acsConfig.raw_ubx_filename;

			createNewTraceFile(id, 		logptime,	filename,	ubxParser.raw_ubx_filename);
		}
	}
	catch(...){}

	for (auto& [id, streamParser_ptr] : streamParserMultimap)
	try
	{
		auto& customParser = dynamic_cast<CustomParser&>(streamParser_ptr->parser);

		if (acsConfig.record_raw_custom)
		{
			string filename = acsConfig.raw_custom_filename;

			createNewTraceFile(id, 		logptime,	filename,	customParser.raw_custom_filename);
		}
	}
	catch(...){}
}

void avoidCollisions(
	ReceiverMap&		receiverMap)
{
	for (auto& [id, rec] : receiverMap)
	{
		auto trace = getTraceFile(rec);
	}

	for (auto& [id, rec] : receiverMap)
	{
		//create sinex estimate maps
		theSinex.estimatesMap	[id];
		theSinex.solEpochMap	[id];

		auto& recOpts = acsConfig.getRecOpts(id);

		if (recOpts.sat_id.empty() == false)
		{
			auto& satNav = nav.satNavMap[recOpts.sat_id.c_str()];
		}
	}
}

/** Perform operations for each station
* This function occurs in parallel with other stations - ensure that any operations on global maps do not create new entries, as that will destroy the map for other processes.
* Variables within the rec object are ok to use, but be aware that pointers from the within the receiver often point to global variables.
* Prepare global maps by accessing the desired elements before calling this function.
*/
void mainOncePerEpochPerStation(
	Receiver&	rec,
	Network&	net,
	bool&		emptyEpoch,
	KFState&	remoteState)
{
	Instrument instrument(__FUNCTION__);

	auto trace = getTraceFile(rec);

	sinexPerEpochPerStation(tsync, rec);

	preprocessor(net, rec, true);

	//recalculate variances now that elevations are known due to satellite postions calculation above
	obsVariances(rec.obsList);

	if (acsConfig.process_spp)
	{
		SPP(trace, rec.obsList, rec.sol, rec.id, &net.kfState, &remoteState);
	}

	if	(  rec.ready == false
		|| rec.invalid)
	{
		return;
	}

	bool sppUsed;
	selectAprioriSource(rec, tsync, sppUsed);

	if (sppUsed)
	{
		if (acsConfig.require_apriori_positions)
		{
			trace << std::endl			<< "Warning: Receiver " << rec.id << " rejected due to lack of apriori position";
			BOOST_LOG_TRIVIAL(warning)	<< "Warning: Receiver " << rec.id << " rejected due to lack of apriori position";

			rec.invalid = true;
			return;
		}

		BOOST_LOG_TRIVIAL(warning)	<< "Warning: Apriori position not found for " << rec.id;
	}

	if (rec.antennaId.empty())
	{
		if (acsConfig.require_antenna_details)
		{
			trace << std::endl			<< "Warning: Receiver " << rec.id << " rejected due to lack of antenna details";
			BOOST_LOG_TRIVIAL(warning)	<< "Warning: Receiver " << rec.id << " rejected due to lack of antenna details";
			rec.invalid = true;
			return;
		}

		BOOST_LOG_TRIVIAL(warning)	<< "Warning: Antenna details not found for " << rec.id;
	}

	emptyEpoch = false;

	BOOST_LOG_TRIVIAL(trace)
	<< "Read " << rec.obsList.size()
	<< " observations for station " << rec.id;

	//calculate statistics
	{
		Instrument instrument("Statistics");
		if ((GTime) rec.firstEpoch	== GTime::noTime())		{	rec.firstEpoch	= rec.obsList.front()->time;		}
																rec.lastEpoch	= rec.obsList.front()->time;
		rec.epochCount++;
		rec.obsCount += rec.obsList.size();

		for (auto& obs				: only<GObs>(rec.obsList))
		for (auto& [ft, sigList]	: obs.sigsLists)
		for (auto& sig				: sigList)
		{
			rec.codeCount[sig.code]++;
		}

		for (auto& obs				: only<GObs>(rec.obsList))
		{
			rec.satCount[obs.Sat]++;
		}
	}

	if (acsConfig.process_ionosphere)
	{
		obsIonoData(trace, rec);
	}

	auto& recOpts = acsConfig.getRecOpts(rec.id);

	rec.antBoresight	= recOpts.antenna_boresight;
	rec.antAzimuth		= recOpts.antenna_azimuth;

	recAtt(rec, tsync, recOpts.attitudeModel.sources);

	testEclipse(rec.obsList);

	if (acsConfig.output_rinex_obs)
	{
		writeRinexObs(rec.id, rec.snx, tsync, rec.obsList, acsConfig.rinex_obs_version);
	}
}

void outputPredictedStates(
	Trace&			trace,
	KFState&		kfState)
{
	if (acsConfig.mongoOpts.output_predictions == +E_Mongo::NONE)
	{
		return;
	}

	BOOST_LOG_TRIVIAL(info) << " ------- PREDICTING STATES            --------" << std::endl;

	tuple<double, double>	forward = {+1, acsConfig.mongoOpts.forward_prediction_duration};
	tuple<double, double>	reverse = {-1, acsConfig.mongoOpts.reverse_prediction_duration};

	MongoStatesOptions mongoStatesOpts;
	mongoStatesOpts.force		= true;
	mongoStatesOpts.queue		= acsConfig.mongoOpts.queue_outputs;
	mongoStatesOpts.index		= false;
	mongoStatesOpts.instances	= acsConfig.mongoOpts.output_predictions;

	for (auto& duo : {forward, reverse})
	{
		auto& [sign, duration] = duo;

		if (duration < 0)
		{
			continue;
		}

		GTime	startTime	= tsync + acsConfig.mongoOpts.prediction_offset;
		GTime	stopTime	= tsync + acsConfig.mongoOpts.prediction_offset + sign * duration;
		double	timeDelta	= sign * acsConfig.mongoOpts.prediction_interval;

		{
			KFState subState = kfState.getSubState({KF::REC_POS, KF::CODE_BIAS, KF::PHASE_BIAS});

			mongoStates(subState, mongoStatesOpts);
		}

		//predict orbits
		for (auto& once : {1})
		{
			Orbits orbits = prepareOrbits(trace, kfState);

			if (orbits.empty())
			{
				continue;
			}

			GTime orbitsTime = tsync;

			for (GTime time = startTime; sign * (time - stopTime).to_double() <= 0; time += timeDelta)
			{
				OrbitIntegrator integrator;
				integrator.timeInit				= orbitsTime;

				double tgap = (time - orbitsTime).to_double();

				integrateOrbits(integrator, orbits, tgap, acsConfig.propagationOptions.integrator_time_step);

				orbitsTime = time;

				std::cout << std::endl << "Propagated to " << time.to_string();

				KFState mongoState;
				mongoState.time = time;
				int s = 6 * orbits.size();
				mongoState.x	.resize(s);
				mongoState.dx	.resize(s);
				mongoState.P	.resize(s, s);

				int index = 0;
				for (int o = 0; o < orbits.size(); o++)
				{
					auto& orbit = orbits[o];

					for (auto& [key, i] : orbit.subState.kfIndexMap)
					{
						if (key.type != KF::ORBIT)
						{
							continue;
						}

						if (key.num < 3)	mongoState.x(index)			= orbit.pos(i);
						else				mongoState.x(index)			= orbit.vel(i-3);

											mongoState.P(index,	index)	= orbit.posVelSTM(i, i);

						mongoState.kfIndexMap[key] = index;
						index++;
					}

// 					if	(acsConfig.output_orbit_ics)
// 					{
// 						outputOrbitConfig(orbit.subState, "_prop");			//this wont work for predictions, substate isnt updated,
// 					}
				}

				mongoStates(mongoState, mongoStatesOpts);

// 				outputMongoPredictions(trace, orbits, time, mongoOptions);
			}
		}
	}
}

void mainPerEpochPostProcessingAndOutputs(
	Network&		pppNet,
	Network&		ionNet,
	ReceiverMap&	receiverMap,
	KFState&		kfState,
	KFState&		ionState,
	GTime			time,
	bool			emptyEpoch)
{
// 	Instrument	instrument(__FUNCTION__);

	auto pppTrace = getTraceFile(pppNet);
	auto ionTrace = getTraceFile(ionNet);

	//todo aaron, check clocks output for switching on user mode

	if (acsConfig.process_ppp)
	{
		mongoStates(kfState,
					{
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});
	}

	if	(	acsConfig.process_ppp
		&&	acsConfig.ambrOpts.mode != +E_ARmode::OFF
		&&	acsConfig.ambrOpts.once_per_epoch)
	{
		fixAndHoldAmbiguities(pppTrace, kfState);

		mongoStates(kfState,
					{
						.suffix		= "_AR",
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});
	}

	if	(  acsConfig.ionModelOpts.model
		&& acsConfig.ssrOpts.atmosphere_sources.front() == +E_Source::KALMAN)
	{
		ionosphereSsrUpdate(ionTrace, kfState);
	}

	if (acsConfig.process_ionosphere)
	{
		obsIonoDataFromFilter(ionTrace, receiverMap, kfState);

		filterIonosphere(ionTrace, ionNet.kfState, receiverMap, time);

		if (acsConfig.ssrOpts.atmosphere_sources.front() == +E_Source::KALMAN)
		{
			ionosphereSsrUpdate(ionTrace, ionNet.kfState);
		}
	}

	//todo aaron check trop sinex for ppp is implemented

	KFState tempAugmentedKF = kfState;

	if (acsConfig.process_ppp)
	{
		if	(  acsConfig.process_minimum_constraints
			&& acsConfig.minconOpts.once_per_epoch)
		{
			BOOST_LOG_TRIVIAL(info) << " ------- PERFORMING MIN-CONSTRAINTS   --------" << std::endl;

			for (auto& [id, rec] : receiverMap)
			{
				rec.minconApriori = rec.aprioriPos;
			}

			MinconStatistics minconStatistics;

			mincon(pppTrace, tempAugmentedKF, &minconStatistics);				//todo aaron, orbits apriori need etting

			tempAugmentedKF.outputStates(pppTrace, "/CONSTRAINED");

			outputMinconStatistics(pppTrace, minconStatistics);

			mongoStates(tempAugmentedKF,
						{
							.suffix		= "_mincon",
							.instances	= acsConfig.mongoOpts.output_states,
							.queue		= acsConfig.mongoOpts.queue_outputs
						});
		}

		if (acsConfig.output_erp)
		{
			writeErpFromNetwork(kfState.metaDataMap[ERP_FILENAME_STR], kfState);
		}

		if (acsConfig.output_clocks)
		{
			outputClocks(acsConfig.clocks_filename, acsConfig.clocks_receiver_sources, acsConfig.clocks_satellite_sources, time, tempAugmentedKF, &receiverMap);
		}

		if (acsConfig.output_trop_sinex)
		{
			outputTropSinex(kfState.metaDataMap[TROP_FILENAME_STR], kfState.time, kfState, "MIX");
		}

		if	(  acsConfig.process_rts
			&& acsConfig.pppOpts.rts_lag > 0)
		{
			rtsSmoothing(pppNet.kfState, receiverMap);
		}


		for (auto& [recId, rec] : receiverMap)
		{
			auto trace = getTraceFile(rec);

			{
				outputPppNmea(trace, kfState, rec.id);
			}

			if (acsConfig.output_cost)		{	outputCost			(kfState.metaDataMap[COST_FILENAME_STR	+ recId], kfState,	rec);		}
			if (acsConfig.output_gpx)		{	writeGPX			(kfState.metaDataMap[GPX_FILENAME_STR	+ recId], kfState,	rec.id);	}
		}

		outputStatistics(pppTrace, pppNet.kfState.statisticsMap, pppNet.kfState.statisticsMapSum);
	}

	if (acsConfig.output_rinex_nav)
	{
		writeRinexNav(acsConfig.rinex_nav_version);
	}

	if (acsConfig.output_sp3)
	{
		outputSp3(acsConfig.sp3_filename, time, acsConfig.sp3_orbit_sources, acsConfig.sp3_clock_sources, &tempAugmentedKF, emptyEpoch);
	}


	if (acsConfig.output_orbex)
	{
		outputOrbex(acsConfig.orbex_filename, time, acsConfig.orbex_orbit_sources, acsConfig.orbex_clock_sources, acsConfig.orbex_attitude_sources, &kfState);
	}

	if (acsConfig.output_ionex)
	{
		if (acsConfig.process_ionosphere)			ionexFileWrite(ionTrace, kfState.metaDataMap[IONEX_FILENAME_STR], time, ionNet.kfState);
		else										ionexFileWrite(pppTrace, kfState.metaDataMap[IONEX_FILENAME_STR], time, kfState);
	}

	if (acsConfig.output_ionstec)
	{
		writeIonStec(kfState.metaDataMap[IONSTEC_FILENAME_STR], kfState);
	}

	if (acsConfig.output_bias_sinex)
	{
		writeBiasSinex(pppTrace, time, kfState, ionState, kfState.metaDataMap[BSX_FILENAME_STR], receiverMap);
	}

	if (acsConfig.output_orbit_ics)
	{
		outputOrbitConfig(kfState);
	}

	mongoMeasSatStat		(receiverMap);
	outputApriori			(receiverMap);
	outputPredictedStates	(pppTrace, tempAugmentedKF);
	prepareSsrStates		(pppTrace, tempAugmentedKF, ionState, time);

	if (acsConfig.instrument_once_per_epoch)
	{
		Instrument::printStatus(true);
	}
}

void mainOncePerEpochPerSatellite(
	Trace&	trace,
	GTime	time,
	SatSys	Sat)
{
	auto& satNav 	= nav.satNavMap[Sat];
	auto& satOpts	= acsConfig.getSatOpts(Sat);

	if (satOpts.exclude)
	{
		return;
	}

	//get svn and block type if possible
	if (Sat.svn().empty())
	{
		auto it = nav.svnMap[Sat].lower_bound(time);
		if (it == nav.svnMap[Sat].end())
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: SVN not found for " << Sat.id();

			Sat.setSvn("UNKNOWN");
		}
		else
		{
			Sat.setSvn(it->second);
		}

		//reinitialise the options with the updated values
		satOpts._initialised = false;
	}

	if (Sat.blockType().empty())
	{
		auto it = nav.blocktypeMap.find(Sat.svn());
		if (it == nav.blocktypeMap.end())
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: Block type not found for " << Sat.id() << ", attitude modelling etc may be affected, check sinex file";

			Sat.setBlockType("UNKNOWN");
		}
		else
		{
			Sat.setBlockType(it->second);
		}

		//reinitialise the options with the updated values
		satOpts._initialised = false;		//todo aaron, this is insufficient since the opts are inherited from the other initialised ones per file which are not reset
	}

	satOpts = acsConfig.getSatOpts(Sat);

	satNav.antBoresight	= satOpts.antenna_boresight;
	satNav.antAzimuth	= satOpts.antenna_azimuth;

	auto& satPos0 = satNav.satPos0;

	satPos0.Sat			= Sat;
	satPos0.satNav_ptr	= &satNav;

	bool pass =	satpos(nullStream, time, time, satPos0, satOpts.posModel.sources, E_OffsetType::COM, nav);
	if (pass == false)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No sat pos found for " << satPos0.Sat.id() << ".";
		return;
	}

	ERPValues erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapper(time, erpv);

// 	satPos0.rSatEci0	= frameSwapper(satPos0.rSatCom);		//dont uncomment this, does double propagation in spp
// 	satPos0.posTime		= time;

	satNav.aprioriPos	= satPos0.rSatEci0;

	updateSatAtts(satPos0);
}


void mainOncePerEpoch(
	Network&		pppNet,
	Network&		ionNet,
	ReceiverMap&	receiverMap,
	GTime			time)
{
	avoidCollisions(receiverMap);

	//reload any new or modified files
	reloadInputFiles();

	addDefaultBias();

	createTracefiles(receiverMap, pppNet, ionNet);

	auto pppTrace = getTraceFile(pppNet);

	//initialise mongo if not already done
	mongoooo();

	//integrate accelerations and early prepare satPos0 with propagated values
	predictOrbits	(pppTrace, pppNet.kfState, time);
	predictInertials(pppTrace, pppNet.kfState, time);

	//try to get svns & block types of all used satellites
	for (auto& [Sat, satNav] : nav.satNavMap)
	{
		if (acsConfig.process_sys[Sat.sys] == false)
			continue;

		mainOncePerEpochPerSatellite(pppTrace, time, Sat);
	}

	BOOST_LOG_TRIVIAL(info) << " ------- PREPROCESSING STATIONS       --------" << std::endl;

	KFState remoteState;
	if (acsConfig.mongoOpts.use_predictions)
	{
		vector<KF> remoteTypes;					//todo aaron, may modify this to get only desired states, but recOpts configs determine those per sat/station...
		remoteTypes.push_back(KF::ORBIT);
		remoteTypes.push_back(KF::REC_POS);
		remoteTypes.push_back(KF::CODE_BIAS);
		remoteTypes.push_back(KF::PHASE_BIAS);

		mongoReadFilter(remoteState, time, remoteTypes);

		loadStateBiases(remoteState);
	}

	//do per-station pre processing
	bool emptyEpoch = true;
#	ifdef ENABLE_PARALLELISATION
		Eigen::setNbThreads(1);
#		pragma omp parallel for
#	endif
	for (int i = 0; i < receiverMap.size(); i++)
	{
		auto rec_ptr_iterator = receiverMap.begin();
		std::advance(rec_ptr_iterator, i);

		auto& [id, rec] = *rec_ptr_iterator;
		mainOncePerEpochPerStation(rec, pppNet, emptyEpoch, remoteState);
	}
	Eigen::setNbThreads(0);


	if	(emptyEpoch)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Epoch " << epoch << " has no observations";
	}

	if (acsConfig.process_ppp)
	{
		PPP(pppTrace, receiverMap, pppNet.kfState, remoteState);
	}

	KFState* kfState_ptr;

	KFState tempKfState;

	if (acsConfig.ambrOpts.fix_and_hold)	{									kfState_ptr = &pppNet.kfState;		}
	else									{	tempKfState = pppNet.kfState;	kfState_ptr = &tempKfState;		}

	auto& kfState = *kfState_ptr;

	mainPerEpochPostProcessingAndOutputs(pppNet, ionNet, receiverMap, kfState, ionNet.kfState, time, emptyEpoch);

	if (acsConfig.delete_old_ephemerides)
	{
		cullOldEphs		(time);
		cullOldSSRs		(time);
		cullOldBiases	(time);
	}

	mongoCull(time);

	if (acsConfig.check_plumbing)
	{
		plumber();
	}

	callbacksOncePerEpoch();
}

void mainPostProcessing(
	Network&		pppNet,
	Network&		ionNet,
	ReceiverMap&	receiverMap)
{
	BOOST_LOG_TRIVIAL(info)
	<< "Post processing... ";

	auto pppTrace = getTraceFile(pppNet);

	if	( 	acsConfig.process_ppp
		&&	acsConfig.ambrOpts.mode != +E_ARmode::OFF
		&&	acsConfig.ambrOpts.once_per_epoch == false
		&&	acsConfig.ambrOpts.fix_and_hold)
	{
		BOOST_LOG_TRIVIAL(info)
		<< std::endl
		<< "---------------PERFORMING AMBIGUITY RESOLUTION ON NETWORK WITH FIX AND HOLD ------------- " << std::endl;

		fixAndHoldAmbiguities(pppTrace, pppNet.kfState);

		mongoStates(pppNet.kfState,
					{
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});
	}

	if	(	acsConfig.process_ppp
		&&	acsConfig.process_minimum_constraints
		&&	acsConfig.minconOpts.once_per_epoch == false)
	{
		BOOST_LOG_TRIVIAL(info) << " ------- PERFORMING MIN-CONSTRAINTS   --------" << std::endl;

		for (auto& [id, rec] : receiverMap)
		{
			rec.minconApriori = rec.aprioriPos;
		}

		{
			ERPValues erpv = getErp(nav.erp, pppNet.kfState.time);

			FrameSwapper frameSwapper(pppNet.kfState.time, erpv);

			for (auto& [Sat, satNav] : nav.satNavMap)
			{
				SatPos satPos;

				satPos.Sat			= Sat;
				satPos.satNav_ptr	= &satNav;

				bool pass =	satpos(nullStream, pppNet.kfState.time, pppNet.kfState.time, satPos, {E_Source::PRECISE, E_Source::BROADCAST}, E_OffsetType::COM, nav);
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(warning) << "Warning: No sat pos found for " << satPos.Sat.id() << ".";
					continue;
				}

				satNav.aprioriPos = frameSwapper(satPos.rSatCom);
			}
		}

		MinconStatistics minconStatistics;

		mincon(pppTrace, pppNet.kfState, &minconStatistics);

		pppNet.kfState.outputStates(pppTrace, "/CONSTRAINED");

		outputMinconStatistics(pppTrace, minconStatistics);

		mongoStates(pppNet.kfState,
					{
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});
	}

	if (acsConfig.output_sinex)
	{
		sinexPostProcessing(tsync, receiverMap, pppNet.kfState);
	}

	if (acsConfig.process_ppp)
	{
// 		outputMqtt	(pppNet.kfState);
	}

	outputPredictedStates(pppTrace, pppNet.kfState);

	if (acsConfig.output_predicted_orbits)
	{
		outputMongoOrbits();
	}

	if (acsConfig.process_rts)
	{
		while (spitQueueRunning)
		{
			sleep_for(std::chrono::milliseconds(acsConfig.sleep_milliseconds));
		}

		if	( acsConfig.process_ppp
			&&acsConfig.pppOpts.rts_lag < 0)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------PROCESSING PPP WITH RTS--------------------- " << std::endl;

			rtsSmoothing(pppNet.kfState, receiverMap, true);
		}

		if	( acsConfig.process_ionosphere
			&&acsConfig.pppOpts.rts_lag < 0)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "---------------PROCESSING IONOSPHERE WITH RTS------------------ " << std::endl;

			rtsSmoothing(ionNet.kfState, receiverMap, true);
		}
	}

	Instrument::printStatus();

	outputSummaries(pppTrace, receiverMap);

	outputStatistics(pppTrace, pppNet.kfState.statisticsMapSum, pppNet.kfState.statisticsMapSum);
}

int ginan(
	int		argc,
	char**	argv)
{
	tracelevel(5);

	// Register the sink in the logging core
	boost::log::core::get()->add_sink(boost::make_shared<sinks::synchronous_sink<ConsoleLog>>());
	boost::log::core::get()->set_filter(boost::log::trivial::severity >= boost::log::trivial::info);

	BOOST_LOG_TRIVIAL(info)
	<< "PEA starting... (" << ginanBranchName() << " " << ginanCommitVersion() << " from " << ginanCommitDate() << ")" << std::endl;

	GTime peaStartTime = timeGet();

	exitOnErrors();

	bool pass = configure(argc, argv);
	if (pass == false)
	{
		BOOST_LOG_TRIVIAL(error) 	<< "Error: Incorrect configuration";
		BOOST_LOG_TRIVIAL(info) 	<< "PEA finished";
		NtripSocket::io_service.stop();
		return EXIT_FAILURE;
	}

	if (acsConfig.output_log)
	{
		addFileLog();
	}

	BOOST_LOG_TRIVIAL(info)
	<< "Threading with max " << Eigen::nbThreads()
	<< " eigen threads";

#ifdef ENABLE_PARALLELISATION
	BOOST_LOG_TRIVIAL(info)
	<< "Threading with max " << omp_get_thread_limit()
	<< " omp threads";
#endif

	BOOST_LOG_TRIVIAL(info)
	<< "Logging with trace level:" << acsConfig.trace_level << std::endl << std::endl;

	//prepare the satNavMap so that it at least has entries for everything
	for (auto [sys, max] :	{	tuple<E_Sys, int>{E_Sys::GPS, NSATGPS},
								tuple<E_Sys, int>{E_Sys::GLO, NSATGLO},
								tuple<E_Sys, int>{E_Sys::GAL, NSATGAL},
								tuple<E_Sys, int>{E_Sys::QZS, NSATQZS},
								tuple<E_Sys, int>{E_Sys::LEO, NSATLEO},
								tuple<E_Sys, int>{E_Sys::BDS, NSATBDS},
								tuple<E_Sys, int>{E_Sys::SBS, NSATSBS}})
	for (int prn = 1; prn <= max; prn++)
	{
		SatSys Sat(sys, prn);

		if (acsConfig.process_sys[Sat.sys] == false)
			continue;

		auto& satOpts = acsConfig.getSatOpts(Sat);

		if (satOpts.exclude)
			continue;

		nav.satNavMap[Sat].id = Sat.id();
	}

	Network pppNet;
	{
		pppNet.kfState.FilterOptions::operator=(acsConfig.pppOpts);
		pppNet.kfState.id						= "Net";
		pppNet.kfState.output_residuals			= acsConfig.output_residuals;
		pppNet.kfState.outputMongoMeasurements	= acsConfig.mongoOpts.output_measurements;

		pppNet.kfState.measRejectCallbacks	.push_back(deweightMeas);
		pppNet.kfState.measRejectCallbacks	.push_back(incrementPhaseSignalError);
		pppNet.kfState.measRejectCallbacks	.push_back(pseudoMeasTest);

		pppNet.kfState.stateRejectCallbacks	.push_back(orbitGlitchReaction);	//this goes before reject by state
		pppNet.kfState.stateRejectCallbacks	.push_back(rejectByState);
	}

	Network ionNet;
	if (acsConfig.process_ionosphere)
	{
		ionNet.kfState.FilterOptions::operator=(acsConfig.ionModelOpts);
		ionNet.kfState.id						= "ION";
		ionNet.kfState.output_residuals			= acsConfig.output_residuals;
		ionNet.kfState.outputMongoMeasurements	= acsConfig.mongoOpts.output_measurements;
		ionNet.kfState.rts_basename				= "IONEX_RTS";

		ionNet.kfState.measRejectCallbacks	.push_back(deweightMeas);

		ionNet.kfState.stateRejectCallbacks	.push_back(rejectByState);
	}

	if	(  acsConfig.process_rts
		&& acsConfig.pppOpts.rts_lag)
	{
		pppNet.kfState.rts_lag		= acsConfig.pppOpts.rts_lag;
		ionNet.kfState.rts_lag		= acsConfig.pppOpts.rts_lag;
	}

	//initialise mongo
	mongoooo();

	for (auto once : {1})
	{
		if (acsConfig.yamls.empty())
		{
			continue;
		}

		YAML::Emitter emitter;
		emitter << YAML::DoubleQuoted << YAML::Flow << YAML::BeginSeq << acsConfig.yamls[0];

		string config(emitter.c_str() + 1);

		mongoOutputConfig(config);
	}


	if (acsConfig.rts_only)
	{
		pppNet.kfState.rts_lag = 4000;
		pppNet.kfState.rts_basename = acsConfig.pppOpts.rts_filename;

		rtsSmoothing(pppNet.kfState, receiverMap);

		exit(0);
	}

	initialiseBias();

	boost::posix_time::ptime logptime = currentLogptime();
	createDirectories(logptime);

	reloadInputFiles();

	addDefaultBias();

	NtripSocket::startClients();

	configureUploadingStreams();

	if (acsConfig.start_epoch.is_not_a_date_time() == false)
	{
		PTime startTime;
		startTime.bigTime = boost::posix_time::to_time_t(acsConfig.start_epoch);

		tsync = startTime;
	}

	createTracefiles(receiverMap, pppNet, ionNet);

	configAtmosRegions(std::cout, receiverMap);

	if (acsConfig.mincon_only)
	{
		minconOnly(std::cout, receiverMap);
	}

	doDebugs();

	BOOST_LOG_TRIVIAL(info)
	<< std::endl;
	BOOST_LOG_TRIVIAL(info)
	<< "Starting to process epochs...";
	BOOST_LOG_TRIVIAL(info)
	<< "Starting epoch #1";

	//============================================================================
	// MAIN PROCESSING LOOP														//
	//============================================================================

	// Read the observations for each station and do stuff
	bool	complete					= false;							// When all input files are empty the processing is deemed complete - run until then, or until something else breaks the loop
	int		loopEpochs					= 0;								// A count of how many loops of epoch_interval this loop used up (usually one, but may be more if skipping epochs)
	auto	nextNominalLoopStartTime	= system_clock::now() + 10s;		// The time the next loop is expected to start - if it doesnt start until after this, it may be skipped
	while (complete == false)
	{
		if (tsync != GTime::noTime())
		{
			tsync.bigTime			+= loopEpochs * acsConfig.epoch_interval;

			if (fabs(tsync.bigTime - round(tsync.bigTime)) < acsConfig.epoch_tolerance)
			{
				tsync.bigTime = round(tsync.bigTime);
			}
		}

		epoch						+= loopEpochs;
		nextNominalLoopStartTime	+= loopEpochs * std::chrono::milliseconds((int)(acsConfig.wait_next_epoch * 1000));

		// Calculate the time at which we will stop waiting for data to come in for this epoch
		auto breakTime	= nextNominalLoopStartTime
						+ std::chrono::milliseconds((int)(acsConfig.wait_all_receivers	* 1000));

		if (loopEpochs)
		{
			BOOST_LOG_TRIVIAL(info) << std::endl
			<< "Starting epoch #" << epoch;
		}

		for (auto& [id, rec] : receiverMap)
		{
			rec.ready = false;

			auto trace		= getTraceFile(rec);

			trace		<< std::endl << "------=============== Epoch " << epoch	<< " =============-----------" << std::endl;
			trace		<< std::endl << "------=============== Time  " << tsync	<< " =============-----------" << std::endl;
		}

		{
			auto pppTrace	= getTraceFile(pppNet);

			pppTrace	<< std::endl << "------=============== Epoch " << epoch	<< " =============-----------" << std::endl;
			pppTrace	<< std::endl << "------=============== Time  " << tsync	<< " =============-----------" << std::endl;
		}

		//get observations from streams (allow some delay between stations, and retry, to ensure all messages for the epoch have arrived)
		map<string, bool>	dataAvailableMap;
		bool 				foundFirst	= false;
		bool				repeat		= true;
		bool				atLeastOnce	= true;
		while	(   atLeastOnce
				||( repeat
				&&system_clock::now() < breakTime))
		{
			atLeastOnce = false;

			if (acsConfig.require_obs)
			{
				repeat = false;
			}

			//load any changes from the config
			bool newConfig = acsConfig.parse();

			//make any changes to streams.
			if (newConfig)
			{
				configureUploadingStreams();
			}

			//remove any dead streams
			for (auto iter = streamParserMultimap.begin(); iter != streamParserMultimap.end(); )
			{
				auto& [id, streamParser_ptr]	= *iter;
				auto& stream					= streamParser_ptr->stream;

				auto& recOpts = acsConfig.getRecOpts(id);

				if (recOpts.kill)
				{
					BOOST_LOG_TRIVIAL(info)
					<< "Removing " << stream.sourceString << " due to kill config" << std::endl;

					for (auto& [key, index] : pppNet.kfState.kfIndexMap)
					{
						if (key.str == id)
						{
							pppNet.kfState.removeState(key);

							auto nodeHandler = pppNet.kfState.kfIndexMap.extract(key);
							nodeHandler.key().rec_ptr = nullptr;
							pppNet.kfState.kfIndexMap.insert(std::move(nodeHandler));
						}
					}

					receiverMap.erase(id);

					iter = streamParserMultimap.erase(iter);

					continue;
				}

				try
				{
					auto& obsStream	= dynamic_cast<ObsStream&>(*streamParser_ptr);

					if (obsStream.hasObs())
					{
						iter++;
						continue;
					}
				}
				catch(...){}

				if (stream.isDead())
				{
					BOOST_LOG_TRIVIAL(info)
					<< "No more data available on " << stream.sourceString << std::endl;

					//record as dead and erase
					streamDOAMap[stream.sourceString] = true;

					receiverMap[id].obsList.clear();

					iter = streamParserMultimap.erase(iter);

					continue;
				}

				iter++;
			}

			if (streamParserMultimap.empty())
			{
				static bool once = true;
				if (once)
				{
					once = false;

					BOOST_LOG_TRIVIAL(info)
					<< std::endl;
					BOOST_LOG_TRIVIAL(info)
					<< "Inputs finished at epoch #" << epoch;
				}

				if (acsConfig.require_obs)
					complete = true;

				break;
			}

			//parse all non-observation streams
			for (auto& [id, streamParser_ptr] : streamParserMultimap)
			try
			{
				auto& obsStream = dynamic_cast<ObsStream&>(*streamParser_ptr);
			}
			catch (std::bad_cast& e)
			{
				streamParser_ptr->parse();
			}

			for (auto& [id, streamParser_ptr] : streamParserMultimap)
			{
				ObsStream* obsStream_ptr;

				try
				{
					obsStream_ptr = &dynamic_cast<ObsStream&>(*streamParser_ptr);
				}
				catch (std::bad_cast& e)
				{
					continue;
				}

				auto& obsStream = *obsStream_ptr;

				auto& recOpts = acsConfig.getRecOpts(id);

				if (recOpts.exclude)
				{
					continue;
				}

				auto& rec = receiverMap[id];

				//try to get some data (again)
				if (rec.ready == false)
				{
					bool moreData = true;
					while (moreData)
					{
						if (acsConfig.assign_closest_epoch)	rec.obsList = obsStream.getObs(tsync, acsConfig.epoch_interval / 2);
						else								rec.obsList = obsStream.getObs(tsync, acsConfig.epoch_tolerance);

						switch (obsStream.obsWaitCode)
						{
							case E_ObsWaitCode::EARLY_DATA:								preprocessor(pppNet, rec);	break;
							case E_ObsWaitCode::OK:					moreData = false;	preprocessor(pppNet, rec);	break;
							case E_ObsWaitCode::NO_DATA_WAIT:		moreData = false;								break;
							case E_ObsWaitCode::NO_DATA_EVER:		moreData = false;								break;
						}
					}
				}

				if (rec.obsList.empty())
				{
					//failed to get observations
					if (obsStream.obsWaitCode == +E_ObsWaitCode::NO_DATA_WAIT)
					{
						// try again later
						repeat = true;
						sleep_for(std::chrono::milliseconds(acsConfig.sleep_milliseconds));
					}

					continue;
				}

				if (tsync == GTime::noTime())
				{
					tsync = rec.obsList.front()->time.floorTime(acsConfig.epoch_interval);

					acsConfig.start_epoch	= boost::posix_time::from_time_t((time_t)((PTime)tsync).bigTime);

					if (tsync + acsConfig.epoch_tolerance < rec.obsList.front()->time)
					{
						repeat = true;
						continue;
					}
				}

				dataAvailableMap[rec.id] = true;

				if (foundFirst == false)
				{
					foundFirst = true;

					//first observation found for this epoch, give any other stations some time to get their observations too
					//only shorten waiting periods, never extend
					auto now = system_clock::now();

					auto alternateBreakTime = now + std::chrono::milliseconds((int)(acsConfig.wait_all_receivers	* 1000));
					auto alternateStartTime = now + std::chrono::milliseconds((int)(acsConfig.wait_next_epoch		* 1000));

					if (alternateBreakTime < breakTime)						{	breakTime					= alternateBreakTime;	}
					if (alternateStartTime < nextNominalLoopStartTime)		{	nextNominalLoopStartTime	= alternateStartTime;	}
				}

				rec.ready = true;
			}
		}

		if (complete)
		{
			break;
		}

		if (tsync == GTime::noTime())
		{
			if (acsConfig.require_obs)
				continue;

			tsync = timeGet();
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Synced " << dataAvailableMap.size() << " stations...";

		GTime epochStartTime	= timeGet();
		{
			mainOncePerEpoch(pppNet, ionNet, receiverMap, tsync);
		}
		GTime epochStopTime		= timeGet();



		GWeek	week	= tsync;
		GTow	tow		= tsync;

		int fractionalMilliseconds = (tsync.bigTime - (long int) tsync.bigTime) * 1000;
		auto boostTime = boost::posix_time::from_time_t((time_t)((PTime)tsync).bigTime) + boost::posix_time::millisec(fractionalMilliseconds);

		BOOST_LOG_TRIVIAL(info)
		<< "Processed epoch #" << epoch
		<< " - " << "GPS time: " << week << " " << std::setw(6) << tow << " - " << boostTime
		<< " (took " << (epochStopTime-epochStartTime) << ")";

		// Check end epoch
		if	(  acsConfig.end_epoch.is_not_a_date_time() == false
			&& boostTime >= acsConfig.end_epoch)
		{
			BOOST_LOG_TRIVIAL(info)
			<< "Exiting at epoch " << epoch << " (" << boostTime
			<< ") as end epoch " << acsConfig.end_epoch
			<< " has been reached";

			break;
		}

		// Check number of epochs
		if	(  acsConfig.max_epochs	> 0
			&& epoch					>= acsConfig.max_epochs)
		{
			BOOST_LOG_TRIVIAL(info)
			<< std::endl
			<< "Exiting at epoch " << epoch << " (" << boostTime
			<< ") as epoch count " << acsConfig.max_epochs
			<< " has been reached";

			break;
		}

		// Calculate how many loops need to be skipped based on when the next loop was supposed to begin
		auto loopStopTime		= system_clock::now();
		auto loopExcessDuration = loopStopTime - (nextNominalLoopStartTime + std::chrono::milliseconds((int)(acsConfig.wait_all_receivers * 1000)));
		int excessLoops			= loopExcessDuration / std::chrono::milliseconds((int)(acsConfig.wait_next_epoch * 1000));

		if (excessLoops < 0)		{	excessLoops = 0;	}
		if (excessLoops > 0)
		{
			BOOST_LOG_TRIVIAL(warning) << std::endl
			<< "Warning: Excessive time elapsed, skipping " << excessLoops
			<< " epochs to epoch " << epoch + excessLoops + 1
			<< ". Configuration 'wait_next_epoch' is " << acsConfig.wait_next_epoch;
		}

		loopEpochs = 1 + excessLoops;
	}

	// Disconnect the downloading clients and stop the io_service for clean shutdown.
	for (auto& [id, ntripStream] : only<NtripStream>(streamParserMultimap))
	{
		ntripStream.disconnect();
	}

	ntripBroadcaster.stopBroadcast();
	NtripSocket::io_service.stop();

	GTime peaInterTime = timeGet();
	BOOST_LOG_TRIVIAL(info)
	<< std::endl
	<< "PEA started  processing at : " << peaStartTime << std::endl
	<< "and finished processing at : " << peaInterTime << std::endl
	<< "Total processing duration  : " << (peaInterTime - peaStartTime) << std::endl << std::endl;

	BOOST_LOG_TRIVIAL(info)
	<< std::endl
	<< "Finalising streams and post processing...";

	mainPostProcessing(pppNet, ionNet, receiverMap);

	GTime peaStopTime = timeGet();
	BOOST_LOG_TRIVIAL(info)
	<< std::endl
	<< "PEA started  processing at : " << peaStartTime	<< std::endl
	<< "and finished processing at : " << peaStopTime	<< std::endl
	<< "Total processing duration  : " << (peaStopTime - peaStartTime) << std::endl << std::endl;

	std::cout << std::endl << "PEA finished" << std::endl;

	return EXIT_SUCCESS;
}

