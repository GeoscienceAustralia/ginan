
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

Input Input_Files__()
{
	DOCS_REFERENCE(UBX__);
	DOCS_REFERENCE(YAML__);
	DOCS_REFERENCE(IGS_Files__);
}


#include "centerMassCorrections.hpp"
#include "inputsOutputs.hpp"
#include "oceanPoleTide.hpp"
#include "streamCustom.hpp"
#include "streamSerial.hpp"
#include "sinexParser.hpp"
#include "streamRinex.hpp"
#include "streamNtrip.hpp"
#include "coordinates.hpp"
#include "staticField.hpp"
#include "geomagField.hpp"
#include "navigation.hpp"
#include "streamFile.hpp"
#include "streamRtcm.hpp"
#include "tropModels.hpp"
#include "ephPrecise.hpp"
#include "acsConfig.hpp"
#include "streamUbx.hpp"
#include "streamSlr.hpp"
#include "streamSp3.hpp"
#include "tideCoeff.hpp"
#include "ionModels.hpp"
#include "antenna.hpp"
#include "jpl_eph.hpp"
#include "biases.hpp"
#include "sisnet.hpp"
#include "sinex.hpp"
#include "tides.hpp"
#include "aod.hpp"


/** Check that filename is valid and the file exists
*/
bool checkValidFile(
	const string&	path,			///< Filename to check
	const string&	description)	///< Description for error messages
{
	if	( !path.empty()
		&&!std::filesystem::exists(path))
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error: Missing " << description << " file "
		<< path;

		return false;
	}
	return true;
}

bool checkValidFiles(
	vector<string>&	paths,
	const string&	description)
{
	bool pass = true;
	for (auto& path : paths)
	{
		pass &= checkValidFile(path, description);
	}
	return pass;
}


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



/** Create a station object from an input
*/
void addStationData(
	string			stationId,			///< Id of station to add data for
	vector<string>&	inputNames,			///< Filename to create station from
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
			continue;
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
				continue;
			}
		}

		if		(protocol == "file")		{	stream_ptr = make_unique<FileStream>	(subInputName);	}
		else if (protocol == "serial")		{	stream_ptr = make_unique<SerialStream>	(subInputName);	}
		else if (protocol == "http")		{	stream_ptr = make_unique<NtripStream>	(inputName);	}
		else if (protocol == "https")		{	stream_ptr = make_unique<NtripStream>	(inputName);	}
		else if (protocol == "ntrip")		{	stream_ptr = make_unique<NtripStream>	(inputName);	}
		else if (protocol == "sisnet")		{	stream_ptr = make_unique<SisnetStream>	(inputName);	}
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
		else if (inputFormat == "DS2DC")	{	parser_ptr = make_unique<DS2DCParser>	();	}
		else
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: Invalid inputFormat " << inputFormat;
		}

		shared_ptr<StreamParser> streamParser_ptr;

		if		(dataType == "OBS")		streamParser_ptr = make_shared<ObsStream>	(std::move(stream_ptr), std::move(parser_ptr));
		else if	(dataType == "PSEUDO")	streamParser_ptr = make_shared<ObsStream>	(std::move(stream_ptr), std::move(parser_ptr), true);
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
	DOCS_REFERENCE(Input_Files__);

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

	removeInvalidFiles(acsConfig.snx_files);
	for (auto& snxfile : acsConfig.snx_files)
	{
		if (fileChanged(snxfile) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading SNX file " <<  snxfile;

		bool pass = readSinex(snxfile);
		if (pass == false)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error: Unable to load SINEX file " << snxfile;

			continue;
		}
	}

	//dont remove invalid files for pseudo filters, they may exist only intermittently
	// removeInvalidFiles(acsConfig.pseudo_filter_files);
	for (auto& pseudo_filter_file : acsConfig.pseudo_filter_files)
	{
		if (fileChanged(pseudo_filter_file) == false)
		{
			continue;
		}

		BOOST_LOG_TRIVIAL(info)
		<< "Loading Pseudo filter file " << pseudo_filter_file;

		readPseudosFromFile(pseudo_filter_file);
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

	for (auto& [id, slrinputs]			: slrObsFiles)						{	addStationData(id,		slrinputs,					"SLR",		"OBS");			}
	for (auto& [id, ubxinputs]			: acsConfig.ubx_inputs)				{	addStationData(id,		ubxinputs,					"UBX",		"OBS");			}
	for (auto& [id, custominputs]		: acsConfig.custom_inputs)			{	addStationData(id,		custominputs,				"CUSTOM",	"OBS");			}
	for (auto& [id, rnxinputs]			: acsConfig.rnx_inputs)				{	addStationData(id,		rnxinputs,					"RINEX",	"OBS");			}
	for (auto& [id, rtcminputs]			: acsConfig.obs_rtcm_inputs)		{	addStationData(id,		rtcminputs,					"RTCM",		"OBS");			}
	for (auto& [id, pseudosp3inputs]	: acsConfig.pseudo_sp3_inputs)		{	addStationData(id,		pseudosp3inputs,			"SP3",		"PSEUDO");		}
	for (auto& [id, pseudosnxinputs]	: acsConfig.pseudo_snx_inputs)		{	addStationData(id,		pseudosnxinputs,			"SINEX",	"PSEUDO");		}
																			{	addStationData("Nav",	acsConfig.nav_rtcm_inputs,	"RTCM",		"NAV");			}
																			{	addStationData("QZSL6",	acsConfig.qzs_rtcm_inputs,	"RTCM",		"NAV");			}
																			{	addStationData("sisnet",acsConfig.sisnet_inputs,	"DS2DC",	"NAV");			}
}
