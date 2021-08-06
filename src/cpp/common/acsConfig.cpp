#include <sstream>
#include <iostream>
#include <memory>
#include <string>
// #include <filesystem>

using std::unique_ptr;
using std::string;

#include <boost/log/trivial.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>

#include <yaml-cpp/yaml.h>

#include "ntripSourceTable.hpp"
#include "acsNtripBroadcast.hpp"
#include "rtsSmoothing.hpp"
#include "streamTrace.hpp"
#include "acsConfig.hpp"
#include "acsStream.hpp"

ACSConfig acsConfig = {};

extern	std::multimap	<string, std::shared_ptr<NtripRtcmStream>>	ntripRtcmMultimap;
extern	std::multimap	<string, ACSObsStreamPtr>	obsStreamMultimap;
extern	std::multimap	<string, ACSNavStreamPtr>	navStreamMultimap;
extern	std::map		<string, bool>				streamDOAMap;
extern	NtripBroadcaster outStreamManager;

/** Set value according to variable map entry if found
*/
template<typename TYPE>
void trySetValFromVM(
	boost::program_options::variables_map&	vm,		///< Variable map to search in
	string									key, 	///< Variable name
	TYPE&									output)	///< Destination to set
{
	if (vm.count(key))
	{
		output = vm[key].as<TYPE>();
	}
}

/** Check that filename is valid and the file exists
*/
bool checkValidFile(
	string&	path,			///< Filename to check
	string	description)	///< Description for error messages
{
	if	( !path.empty()
		&&!boost::filesystem::exists(path))
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Invalid " << description << " file "
		<< path;

		return false;
	}
	return true;
}

bool checkValidFiles(
	vector<string>&	paths,
	string			description)
{
	bool pass = true;
	for (auto& path : paths)
	{
		pass &= checkValidFile(path, description);
	}
	return pass;
}

/** Remove any path from a fully qualified file
*/
void removePath(
	string &filepath)	// path_to_file
{
	size_t lastdirsep = filepath.rfind('/');

	if (lastdirsep == string::npos) return; // didn't find one ...

	filepath = filepath.substr(lastdirsep+1);

	return;
}

/** Add a root to paths that are not already absolutely defined
*/
void tryAddRootToPath(
	string& root,		///< Root path
	string& path)		///< Filename to prepend root path to
{
	if (path.empty())
	{
		return;
	}
	if (root[0] == '~')
	{
		string HOME = std::getenv("HOME");
		root.erase(0, 1);
		root.insert(0, HOME);
	}
	if (path[0] == '~')
	{
		string HOME = std::getenv("HOME");
		path.erase(0, 1);
		path.insert(0, HOME);
	}
	if (boost::filesystem::path(path).is_absolute())
	{
		return;
	}
	if (root.back() != '/')
	{
		root = root + '/';
	}
	path = root + path;

}

/** Add a root to paths that are not already absolutely defined
*/
void tryAddRootToPath(
	string&			root,		///< Root path
	vector<string>& paths)		///< Filename to prepend root path to
{
	for (auto& path : paths)
	{
		tryAddRootToPath(root, path);
	}
}

/** Create a station object from a file
*/
void ACSConfig::addDataFile(
	string fileName,			///< Filename to create station from
	string fileType,			///< Type of data in file
	string dataType)			///< Type of data
{
	if (streamDOAMap.find(fileName) != streamDOAMap.end())
	{
		//this stream was already added, dont re-add
		return;
	}
	
	boost::filesystem::path filePath(fileName);

	if (checkValidFile(fileName, dataType) == false)
	{
		return;
	}

	if (!boost::filesystem::is_regular_file(filePath))
	{
		return;
	}

	auto filename = filePath.filename();
	string extension = filename.extension().string();

	string stationId = filename.string().substr(0,4);
	boost::algorithm::to_upper(stationId);

	auto recOpts = getRecOpts(stationId);

	if (recOpts.exclude)
	{
		return;
	}
	
	if (fileType == "RINEX")
	{
		auto rinexStream_ptr = std::make_shared<FileRinexStream>(filePath.string());

		if		(dataType == "NAV")		navStreamMultimap.insert({stationId, std::move(rinexStream_ptr)});
		else if	(dataType == "OBS")		obsStreamMultimap.insert({stationId, std::move(rinexStream_ptr)});
		
		streamDOAMap[fileName] = false;

		rinexFiles.push_back(filename.string());
	}

	if (fileType == "RTCM")
	{
		auto rtcmStream_ptr = std::make_shared<FileRtcmStream>(filePath.string());

		if		(dataType == "NAV")		navStreamMultimap.insert({stationId, std::move(rtcmStream_ptr)});
		else if	(dataType == "OBS")		obsStreamMultimap.insert({stationId, std::move(rtcmStream_ptr)});
		
		streamDOAMap[fileName] = false;
	}
}

/** Prepare the configuration of the program
*/
bool configure(
	int argc, 		///< Passthrough calling argument count
	char **argv)	///< Passthrough calling argument list
{
	// Command line options
	boost::program_options::options_description desc{"Options"};

	// Do not set default values here, as this will overide the configuration file opitions!!!
	desc.add_options()
	("help", 																"Help")
	("quiet", 																"Less output")
	("verbose", 															"More output")
	("very-verbose", 														"Much more output")
	("config", 					boost::program_options::value<string>(), 	"Configuration file")
	("trace_level", 			boost::program_options::value<int>(), 		"Trace level")
	("antenna", 				boost::program_options::value<string>(), 	"ANTEX file")
	("navigation", 				boost::program_options::value<string>(), 	"Navigation file")
	("sinex", 					boost::program_options::value<string>(), 	"SINEX file")
	("sp3files", 				boost::program_options::value<string>(), 	"Orbit (SP3) file")
	("clkfiles", 				boost::program_options::value<string>(), 	"Clock (CLK) file")
	("dcbfiles", 				boost::program_options::value<string>(), 	"Code Bias (DCB) file")
	("bsxfiles", 				boost::program_options::value<string>(), 	"Bias Sinex (BSX) file")
	("ionfiles", 				boost::program_options::value<string>(), 	"Ionosphere (IONEX) file")
	("podfiles", 				boost::program_options::value<string>(), 	"Orbits (POD) file")
	("blqfiles", 				boost::program_options::value<string>(), 	"BLQ (Ocean loading) file")
	("erpfiles", 				boost::program_options::value<string>(), 	"ERP file")
	("elevation_mask", 			boost::program_options::value<float>(), 	"Elevation Mask")
	("max_epochs", 				boost::program_options::value<int>(), 		"Maximum Epochs")
	("epoch_interval", 			boost::program_options::value<float>(), 	"Epoch Interval")
	("rnx", 					boost::program_options::value<string>(),	"RINEX station file")
	("root_input_dir", 			boost::program_options::value<string>(),	"Directory containg the input data")
	("root_output_directory", 	boost::program_options::value<string>(),	"Output directory")
	("start_epoch", 			boost::program_options::value<string>(),	"Start date/time")
	("end_epoch", 				boost::program_options::value<string>(),	"Stop date/time")
	("run_rts_only", 			boost::program_options::value<string>(),	"RTS filename (without _xxxxx suffix)")
	("dump-config-only",													"Dump the configuration and exit")
	("input_persistance", 													"Begin with previously stored filter and navigation states")
	("output_persistance", 													"Store filter and navigation states for restarting")
	;

	boost::program_options::variables_map vm;

	boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);

	boost::program_options::notify(vm);

	if	( vm.count("help")
		||argc == 1)
	{
		BOOST_LOG_TRIVIAL(info) << desc;
		BOOST_LOG_TRIVIAL(info) << "PEA finished";

		exit(EXIT_SUCCESS);
	}

	if (vm.count("very-verbose"))	{	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::debug);		}
	if (vm.count("verbose"))		{	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::info);		}
	if (vm.count("quiet"))			{	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::warning);	}

	if (vm.count("run_rts_only"))
	{
		string forward = vm["run_rts_only"].as<string>();

		BOOST_LOG_TRIVIAL(info)
		<< std::endl << "Processing RTS smoothing only for file: " << forward << "_xxxxxx";

		KFState kfState;
		kfState.rts_lag = -1;
		kfState.rts_filename		= forward;
		acsConfig.output_clocks		= true;
		acsConfig.clocks_filename	= forward + SMOOTHED_SUFFIX + "_clk";

		RTS_Process	(kfState, true);
		RTS_Output	(kfState);

		exit(EXIT_SUCCESS);
	}

	if (vm.count("config"))
	{
		string config = vm["config"].as<string>();

		bool pass = acsConfig.parse(config, vm);

		if (!pass)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Failed to parse configuration file " << config;

			return false;
		}
	}

	if (vm.count("output_persistance"))
	{
		acsConfig.output_persistance	= true;
	}
	if (vm.count("input_persistance"))
	{
		acsConfig.input_persistance		= true;
	}

	if (vm.count("rnx"))
	{
		acsConfig.addDataFile(vm["rnx"].as<string>(), "RINEX", "OBS");
	}

	// Dump the configuration information
	acsConfig.info(std::cout);

	// Check the configuration
	bool valid = true;
	valid &= checkValidFiles(acsConfig.snxfiles, 			"sinex file (snx file)");
	valid &= checkValidFiles(acsConfig.navfiles, 			"navfiles");
	valid &= checkValidFiles(acsConfig.sp3files, 			"orbit");
	valid &= checkValidFiles(acsConfig.clkfiles,			"clock file (CLK file)");
	valid &= checkValidFiles(acsConfig.blqfiles, 			"ocean loading information (Blq file)");
	valid &= checkValidFiles(acsConfig.erpfiles,			"earth rotation parameter file (ERP file)");
	valid &= checkValidFiles(acsConfig.dcbfiles,			"code Biases file (DCB file)");
	valid &= checkValidFiles(acsConfig.bsxfiles,			"bias Sinex file (BSX file)");
	valid &= checkValidFiles(acsConfig.ionfiles,			"Ionosphere (IONEX file)");
	valid &= checkValidFiles(acsConfig.atxfiles, 			"antenna information (ANTEX file)");
	valid &= checkValidFiles(acsConfig.orbfiles, 			"orbit determination (pod file)");
	valid &= checkValidFile (acsConfig.tropOpts.gpt2grid,	"grid");

	if	( acsConfig.snxfiles.empty())
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Invalid SINEX file ";

		return false;
	}

	if (vm.count("dump-config-only"))
	{
		BOOST_LOG_TRIVIAL(info)
		<< "PEA finished";

		exit(EXIT_SUCCESS);
	}

	return valid;
}

/** Print out the configuration data that has been read in.
*/
void ACSConfig::info(
	Trace& ss)		///< Trace file to output to
{
	ss << "\n";
	ss << "===============================\n";
	ss << "Configuration...\n";
	ss << "===============================\n";
	ss << "Inputs:\n";
	ss << "\tnavfiles:  "; for (auto& a : navfiles) ss << a << " "; ss << "\n";
	ss << "\tsnxfiles:  "; for (auto& a : snxfiles) ss << a << " "; ss << "\n";
	ss << "\tatxfiles:  "; for (auto& a : atxfiles) ss << a << " "; ss << "\n";
	ss << "\tdcbfiles:  "; for (auto& a : dcbfiles) ss << a << " "; ss << "\n";
	ss << "\tbsxfiles:  "; for (auto& a : bsxfiles) ss << a << " "; ss << "\n";
	ss << "\tionfiles:  "; for (auto& a : ionfiles) ss << a << " "; ss << "\n";
	ss << "\tblqfiles:  "; for (auto& a : blqfiles) ss << a << " "; ss << "\n";
	ss << "\terpfiles:  "; for (auto& a : erpfiles) ss << a << " "; ss << "\n";
	ss << "\tsp3files:  "; for (auto& a : sp3files) ss << a << " "; ss << "\n";
	ss << "\torbfiles:  "; for (auto& a : orbfiles) ss << a << " "; ss << "\n";
	ss << "\tvmf3dir:   " << tropOpts.vmf3dir 			<< "\n";
	ss << "\torography: " << tropOpts.orography 		<< "\n";
	ss << "\tgrid:      " << tropOpts.gpt2grid 			<< "\n";
	ss << "\ttestfiles: " << testOpts.filename			<< "\n";
	ss << "\n";

	ss << "Outputs:\n";
	if (output_trace)		{	ss << "\tTrace level:        " << trace_level 				<< "\n"; }
	if (output_trace)		{	ss << "\ttrace_filename:     " << trace_filename 			<< "\n"; }
	if (output_summary)		{	ss << "\tsummary_filename:   " << summary_filename 			<< "\n"; }
	if (output_clocks)		{	ss << "\tclocks_filename:    " << clocks_filename 			<< "\n"; }
	if (output_ionex)		{	ss << "\tionex_filename:     " << ionex_filename 			<< "\n"; }
	if (output_ionstec)		{	ss << "\tionstec_filename:   " << ionstec_filename 			<< "\n"; }
	if (output_biasSINEX)	{	ss << "\tbiasSINEX_filename: " << biasSINEX_filename		<< "\n"; }

	ss << "\n";

	ss << "Process Modes:\n";
	ss << "\tUser:                " << process_user 				<< "\n";
	ss << "\tNetwork:             " << process_network 				<< "\n";
	ss << "\tMinimum Constraints: " << process_minimum_constraints 	<< "\n";
	ss << "\tInonsphereic:        " << process_ionosphere 			<< "\n";
	ss << "\tRTS Smoothing:       " << process_rts 					<< "\n";
	ss << "\tUnit tests:          " << process_tests				<< "\n";
	ss << "\n";

	ss << "Systems:\n";
	ss << "\tGPS:     " << process_sys[E_Sys::GPS] 		<< "\n";
	ss << "\tGLONASS: " << process_sys[E_Sys::GLO] 		<< "\n";
	ss << "\tGALILEO: " << process_sys[E_Sys::GAL] 		<< "\n";
	ss << "\tBEIDOU:  " << process_sys[E_Sys::CMP] 		<< "\n";
	ss << "\n";

	ss << "Elevation_mask: " << elevation_mask * R2D 	<< "\n";
	ss << "\n";

	ss << "Epochs:\n";
	if (epoch_interval	> 0)					{	ss << "\tepoch_interval: " << epoch_interval	<< "\n";    }
	if (max_epochs		> 0)					{	ss << "\tmax_epochs:     " << max_epochs		<< "\n";    }
	if (!start_epoch	.is_not_a_date_time())	{	ss << "\tepoch start:    " << start_epoch		<< "\n";    }
	if (!end_epoch		.is_not_a_date_time())	{	ss << "\tepoch end:      " << end_epoch			<< "\n";    }

	ss << "\n";
	ss << "Stations:\n";
	for (auto& filename : station_files)
	{
		ss << "\t" << root_stations_dir << filename << "\n";
	}

	ss << "\n";
	ss << "===============================\n";
	ss << "...End Configuration\n";
	ss << "===============================\n";
	ss << "\n";
}

/** Find a yaml node object using a list of strings
*/
YAML::Node stringsToYamlObject(
	YAML::Node		yamlBase, 			///< Yaml node to search within
	vector<string>	yamlNodeDescriptor)	///< List of strings of keys to trace hierarchy
{
	YAML::Node currentNode;
	YAML::Node nullNode;
	currentNode.reset(yamlBase);

	for (auto& desc : yamlNodeDescriptor)
	{
		auto test = currentNode[desc];
		if (!test)
		{
			return test;
		}

		currentNode.reset(currentNode[desc]);
	}
	return currentNode;
}

/** Set an output from yaml object if found
*/
template<typename TYPE>
bool trySetFromYaml(
	TYPE&			output,				///< Variable to output to
	YAML::Node&		yamlBase,			///< Yaml node to search within
	vector<string>	yamlNodeDescriptor)	///< List of strings of keys to trace hierarcy
{
	YAML::Node optNode = stringsToYamlObject(yamlBase, yamlNodeDescriptor);

	try
	{
		output = optNode.as<TYPE>();
		return true;
	}
	catch (...){}

	return false;
}

/** Set an output from command line options if found
*/
template<typename TYPE>
bool trySetFromOpts(
	TYPE&									output,			///< Variable to output to
	boost::program_options::variables_map&	commandOpts,	///< Command line object to search within
	vector<string>							nodeDescriptor)	///< List of strings of keys to trace hierarcy
{
	string name = nodeDescriptor.back();
	if (commandOpts.count(name))
	{
		try
		{
			output = commandOpts[name].as<TYPE>();
			return true;
		}
		catch (...) {}
	}
	return false;
}

/** Set an output from any config source if found
*/
template<typename TYPE>
bool trySetFromAny(
	TYPE&									output,				///< Variable to output to
	boost::program_options::variables_map&	commandOpts,		///< Command line object to search within
	YAML::Node&								yamlBase,			///< Yaml node to search within
	vector<string>							nodeDescriptor)		///< List of strings of keys to trace hierarcy
{
	bool found = false;
	found |= trySetFromYaml(output, yamlBase,	nodeDescriptor);
	found |= trySetFromOpts(output, commandOpts,nodeDescriptor);
	return found;
}

/** Set an enum from yaml, decoding strings to ints
*/
template <typename ENUM>
void trySetEnumOpt(
	int&			out,							///< Variable to output to
	YAML::Node&		yamlBase,						///< Yaml node to search within
	vector<string>	yamlNodeDescriptor,				///< List of strings of keys to trace hierarcy
	ENUM			(&_from_string)(const char*))	///< Function to decode enum strings
{
	YAML::Node optNode = stringsToYamlObject(yamlBase, yamlNodeDescriptor);

	try
	{
		out = _from_string(optNode.as<string>().c_str());
	}
	catch (...)	{}
}

/** Set the variables associated with kalman filter states from yaml
*/
void trySetKalmanFromYaml(
	KalmanModel&	output,	///< Variable to output to
	YAML::Node&		yaml,	///< Yaml node to search within
	string			key)	///< Key of yaml object
{
	auto entry = yaml[key];
	if (entry)
	{
		int proc_noise_dt = 1;
						trySetEnumOpt(proc_noise_dt,			entry, {"proc_noise_dt"	}, E_Period::_from_string_nocase);
						trySetFromYaml(output.estimate, 		entry, {"estimated"		});

						trySetFromYaml(output.sigma,			entry, {"sigma" 		});
						trySetFromYaml(output.clamp_max,		entry, {"clamp_max" 	});
						trySetFromYaml(output.clamp_min,		entry, {"clamp_min" 	});
						trySetFromYaml(output.apriori_val,		entry, {"apriori_val"	});
		bool found = 	trySetFromYaml(output.proc_noise,		entry, {"proc_noise" 	});
		bool foundTau =	trySetFromYaml(output.tau,				entry, {"tau"			});
		if (found)
		{
			for (auto& proc : output.proc_noise)
			{
				proc /= sqrt(proc_noise_dt);
			}
		}
		if (foundTau)
		{
			for (auto& tau : output.tau)
			{
				tau *= proc_noise_dt;
			}
		}
	}
}

/** Set satellite options from yaml
*/
void getFromYaml(
	SatelliteOptions&	satOpts,			///< Satellite options variable to output to
	YAML::Node&			yamlBase,			///< Yaml node to search within
	vector<string>		yamlNodeDescriptor)	///< List of strings of keys of yaml hierarchy
{
	YAML::Node satNode = stringsToYamlObject(yamlBase, yamlNodeDescriptor);

	trySetKalmanFromYaml(satOpts.clk,		satNode, "clk"		);
	trySetKalmanFromYaml(satOpts.clk_rate,	satNode, "clk_rate"	);
	trySetKalmanFromYaml(satOpts.pos,		satNode, "pos"		);
	trySetKalmanFromYaml(satOpts.pos_rate,	satNode, "pos_rate"	);
	trySetKalmanFromYaml(satOpts.orb,		satNode, "orb"		);

	trySetFromYaml(satOpts.exclude, 	satNode, {"exclude"});

	satOpts._initialised = true;
}

/** Set receiver options from yaml
*/
void getFromYaml(
	ReceiverOptions& 	recOpts, 			///< Receiver options variable to output to
	YAML::Node&			yamlBase,			///< Yaml node to search within
	vector<string>		yamlNodeDescriptor)	///< List of strings of keys of yaml hierarchy
{
	YAML::Node recNode = stringsToYamlObject(yamlBase, yamlNodeDescriptor);

	trySetKalmanFromYaml(recOpts.pos,			recNode, "pos"			);
	trySetKalmanFromYaml(recOpts.pos_rate,		recNode, "pos_rate"		);
	trySetKalmanFromYaml(recOpts.clk,			recNode, "clk"			);
	trySetKalmanFromYaml(recOpts.clk_rate,		recNode, "clk_rate"		);
	trySetKalmanFromYaml(recOpts.amb,			recNode, "amb"			);
	trySetKalmanFromYaml(recOpts.trop,			recNode, "trop"			);
	trySetKalmanFromYaml(recOpts.trop_grads,	recNode, "trop_grads"	);
	trySetKalmanFromYaml(recOpts.trop_gauss_markov,			recNode, "trop_gauss_markov"		);
	trySetKalmanFromYaml(recOpts.trop_grads_gauss_markov,	recNode, "trop_grads_gauss_markov"	);

	trySetEnumOpt	(recOpts.error_model,		recNode, {"error_model"	}, E_NoiseModel::_from_string_nocase);
	trySetFromYaml	(recOpts.code_sigmas,		recNode, {"code_sigmas"	});
	trySetFromYaml	(recOpts.phas_sigmas,		recNode, {"phase_sigmas"});
	trySetFromYaml	(recOpts.exclude, 			recNode, {"exclude"		});

	recOpts._initialised = true;
}

/** Set satellite options for a specific satellite using a hierarchy of sources
*/
SatelliteOptions& ACSConfig::getSatOpts(
	SatSys& Sat)	///< Satellite to search for options for
{
	auto& satOpts = satOptsMap[Sat.id()];

	//return early if possible
	if (satOpts._initialised)
		return satOpts;

	//initialise the options for this satellite
	auto& blockOpts = satOptsMap[Sat.blockType()];
	if (blockOpts._initialised == false)
	{
		//find it's parent
		auto& sysOpts = satOptsMap[Sat.sys._to_string()];
		if (sysOpts._initialised == false)
		{
			//find it's parent
			auto& globalOpts = satOptsMap["GLOBAL"];
			if (globalOpts._initialised == false)
			{
				//get specifics from config file
				getFromYaml(globalOpts, yaml, {"default_filter_parameters", "satellites"});
				globalOpts._initialised = true;
			}

			//initialise from its parent
			sysOpts = globalOpts;

			//get specifics from config file
			string sys = "SYS_" + Sat.sysName();
			getFromYaml(sysOpts, yaml, {"override_filter_parameters", "satellites", sys});
		}

		//initialise from its parent
		blockOpts = sysOpts;

		//get specifics from config file
		string block = Sat.blockType();
		getFromYaml(blockOpts, yaml, {"override_filter_parameters", "satellites", block});
	}

	//initialise from its parent
	satOpts = blockOpts;

	//get specifics from config file
	string prn = "PRN_" + Sat.id();
	string svn = "SVN_" + Sat.svn();
	getFromYaml(satOpts, yaml, {"override_filter_parameters", "satellites", prn});
	getFromYaml(satOpts, yaml, {"override_filter_parameters", "satellites", svn});

	return satOpts;
}
	//todo aaron these are just asking for it to crash in parallel
/** Set receiver options for a specific receiver using a hierarchy of sources
*/
ReceiverOptions& ACSConfig::getRecOpts(
	string id)		///< Receiver to search for options for
{
	auto& recOpts = recOptsMap[id];

	//return early if possible
	if (recOpts._initialised)
		return recOpts;

	//initialise the options for this reciever
	auto& globalOpts = recOptsMap[""];
	if (globalOpts._initialised == false)
	{
		//get specifics from config file
		getFromYaml(globalOpts, yaml, {"default_filter_parameters", "stations"});
		globalOpts._initialised = true;
	}

	//initialise from its parent
	recOpts = globalOpts;

	//get specifics from config file
	getFromYaml(recOpts, yaml, {"override_filter_parameters", "stations", id});

	return recOpts;
}

/** Set minimum constraint options for a specific receiver using a hierarchy of sources
*/
MinimumStationOptions& ACSConfig::getMinConOpts(
	string id)			///< Receiver to search for options for
{
	auto& recOpts = minCOpts.stationMap[id];

	//return early if possible
	if (recOpts._initialised)
		return recOpts;

	//initialise the options for this reciever
	auto& globalOpts = minCOpts.stationMap[""];
	if (globalOpts._initialised == false)
	{
		//get specifics from config file
		trySetFromYaml(globalOpts.noise, yaml, {"minimum_constraints", "station_default_noise"});
		globalOpts._initialised = true;
	}

	//initialise from its parent
	recOpts = globalOpts;

	//get specifics from config file
	trySetFromYaml(recOpts.noise, yaml, {"minimum_constraints", "station_noise", id});

	return recOpts;
}

/** Set and scale a variable according to yaml options
*/
template<typename ENUM>
void trySetScaledFromYaml(
	double&			output,							///< Variable to output to
	YAML::Node		node,							///< Yaml node to search within
	vector<string>	number_parameter,				///< List of keys of the hierarchy to the value to be set
	vector<string>	scale_parameter,				///< List of keys of the hierarchy to the scale to be applied
	ENUM			(&_from_string)(const char*))	///< Function to decode scale enum strings
{
	double	number			= 0;
	int		number_units	= 1;
	trySetFromYaml(number,			node, number_parameter);
	trySetEnumOpt(number_units, node, scale_parameter, _from_string);
	number *= number_units;
	if (number != 0)
	{
		output = number;
	}
}

/** search for and replace section of string
*/
void replaceString(
	string&	str,			///< String to search within
	string	subStr, 		///< String to replace
	string	replacement)	///< Replacement string
{
	while (true)
	{
		size_t index = str.find(subStr);
		if (index == -1)
			break;

		str.erase	(index, subStr.size());
		str.insert(index, replacement);
	}
}

/** Replace macros for times with numeric values.
* Available replacements are "<DDD> <D> <WWWW> <YYYY> <YY> <MM> <DD> <HH>"
*/
void replaceTimes(
	string&						str,		///< String to replace macros within
	boost::posix_time::ptime	time_time)	///< Time to use for replacements
{
	string DDD	= "";
	string D	= "";
	string WWWW	= "";
	string YYYY	= "";
	string YY	= "";
	string MM	= "";
	string DD	= "";
	string HH	= "";

	if (!time_time.is_not_a_date_time())
	{
	// 	string gpsWeek0 = "2019-04-07 00:00:00.000";
		string gpsWeek0 = "1980-01-06 00:00:00.000";
		auto gpsZero = boost::posix_time::time_from_string(gpsWeek0);
		string time_string = boost::posix_time::to_iso_string(time_time);

		auto tm = to_tm(time_time);
		std::ostringstream ss;
		ss << std::setw(3) << std::setfill('0') << tm.tm_yday+1;
		string ddd = ss.str();

		auto gpsWeek = (time_time - gpsZero);
		int weeks = gpsWeek.hours() / 24 / 7;		//todo aaron, this is garbage
		ss.str("");
		ss << std::setw(4) << std::setfill('0') << weeks;
		string wwww = ss.str();

		DDD	= ddd;
		D	= std::to_string(tm.tm_wday);
		WWWW	= wwww;
		YYYY	= time_string.substr(0, 4);
		YY	= time_string.substr(2, 2);
		MM	= time_string.substr(4, 2);
		DD	= time_string.substr(6, 2);
		HH	= time_string.substr(9, 2);
	}

	replaceString(str, "<DDD>",		DDD);
	replaceString(str, "<D>",		D);
	replaceString(str, "<WWWW>",	WWWW);
	replaceString(str, "<YYYY>",	YYYY);
	replaceString(str, "<YY>",		YY);
	replaceString(str, "<MM>",		MM);
	replaceString(str, "<DD>",		DD);
	replaceString(str, "<HH>",		HH);
	replaceString(str, "<CONFIG>",	acsConfig.config_description);
}

void replaceTimes(
	vector<string>&				strs,
	boost::posix_time::ptime	time_time)
{
	for (auto& str : strs)
	{
		replaceTimes(str, time_time);
	}
}

bool checkGlob(string str1, string str2)
{
	vector<string> tokens;

	std::stringstream strstream(str1);
	string bit;
	while (getline(strstream, bit, '*'))
	{
		tokens.push_back(bit);
	}

	bool	first	= true;
	int		start	= 0;
	for (auto& token : tokens)
	{
		auto pos = str2.find(token, start);

		if	( first
			&&pos != 0)
		{
			return false;
		}
		else if (pos == string::npos)
		{
			return false;
		}

		start = pos + token.size();
		first = false;
	}

	int strlen = str2.size();
	if (start != strlen)
	{
		return false;
	}
	return true;
}

void globber(
	vector<string>&	files)
{
	vector<string> newFiles;

	for (auto& fileName : files)
	{
		if	( fileName.find('*') != string::npos)
		{
			boost::filesystem::path		filePath(fileName);
			boost::filesystem::path		searchDir	= filePath.parent_path();
			string						searchGlob	= filePath.filename().string();

			if (boost::filesystem::is_directory(searchDir) == false)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Invalid input directory "
				<< searchDir;

				continue;
			}

			for (auto dir_file : boost::filesystem::directory_iterator(searchDir))
			{
				// Skip if not a file
				if (boost::filesystem::is_regular_file(dir_file) == false)
					continue;
				
				string dir_fileName = dir_file.path().filename().string();

				if (checkGlob(searchGlob, dir_fileName))
				{
					newFiles.push_back(dir_file.path().string());
				}
			}
		}
		else
		{
			newFiles.push_back(fileName);
		}
	}
	files = newFiles;
}

bool ACSConfig::parse()
{
	return parse(configFilename, commandOpts);
}

/** Parse options to set acsConfig values.
* Command line options will override any values set in config files, which will themselves override any program default values.
*/
bool ACSConfig::parse(
	string									filename,		///< Path to yaml based config file
	boost::program_options::variables_map&	newCommandOpts)	///< Variable map object of command line options
{
	configFilename = filename;

// 	std::filesystem::path filePath(filename);
	boost::filesystem::path filePath(filename);
// 	auto currentConfigModifyTime = std::filesystem::last_write_time(filePath);
	auto currentConfigModifyTime = boost::filesystem::last_write_time(filePath);

	if (currentConfigModifyTime == configModifyTimeMap["CONFIG"])
	{
		return false;
	}
	
	configModifyTimeMap["CONFIG"] = currentConfigModifyTime;
	commandOpts = newCommandOpts;

	BOOST_LOG_TRIVIAL(info)
	<< "Loading configuration from file " << filename;

	//clear old saved parameters
	satOptsMap.clear();
	recOptsMap.clear();
	
	try
	{
		yaml = YAML::LoadFile(filename);
	}
	catch (const YAML::BadFile &e)
	{
		BOOST_LOG_TRIVIAL(error) << "Failed to parse configuration file " << filename;
		BOOST_LOG_TRIVIAL(error) << e.msg;
		return false;
	}

	// General Files
	auto input_files = stringsToYamlObject(yaml, {"input_files"});
	{
		trySetFromAny(root_input_dir,	commandOpts, input_files, {"root_input_directory"	});
		trySetFromAny(atxfiles,			commandOpts, input_files, {"atxfiles"				});
		trySetFromAny(snxfiles,			commandOpts, input_files, {"snxfiles"				});
		trySetFromAny(blqfiles,			commandOpts, input_files, {"blqfiles"				});
		trySetFromAny(navfiles,			commandOpts, input_files, {"navfiles"				});
		trySetFromAny(sp3files,			commandOpts, input_files, {"sp3files"				});
		trySetFromAny(erpfiles,			commandOpts, input_files, {"erpfiles"				});
		trySetFromAny(dcbfiles,			commandOpts, input_files, {"dcbfiles"				});
		trySetFromAny(bsxfiles,			commandOpts, input_files, {"bsxfiles"				});
		trySetFromAny(ionfiles,			commandOpts, input_files, {"ionfiles"				});
		trySetFromAny(clkfiles,			commandOpts, input_files, {"clkfiles"				});
		trySetFromAny(orbfiles,			commandOpts, input_files, {"orbfiles"				});

		tryAddRootToPath(root_input_dir, atxfiles);
		tryAddRootToPath(root_input_dir, snxfiles);
		tryAddRootToPath(root_input_dir, blqfiles);
		tryAddRootToPath(root_input_dir, navfiles);
		tryAddRootToPath(root_input_dir, orbfiles);
		tryAddRootToPath(root_input_dir, sp3files);
		tryAddRootToPath(root_input_dir, erpfiles);
		tryAddRootToPath(root_input_dir, dcbfiles);
		tryAddRootToPath(root_input_dir, bsxfiles);
		tryAddRootToPath(root_input_dir, ionfiles);
		tryAddRootToPath(root_input_dir, clkfiles);

		globber(atxfiles);
		globber(snxfiles);
		globber(blqfiles);
		globber(navfiles);
		globber(orbfiles);
		globber(sp3files);
		globber(erpfiles);
		globber(dcbfiles);
		globber(bsxfiles);
		globber(ionfiles);
		globber(clkfiles);
	}

	vector<string> rnxfiles;
	vector<string> obs_rtcmfiles;
	vector<string> nav_rtcmfiles;
	auto station_data = stringsToYamlObject(yaml, {"station_data"});
	{
		trySetFromAny(root_stations_dir,	commandOpts, station_data,	{"root_stations_directory"	});
		trySetFromAny(rnxfiles,				commandOpts, station_data,	{"rnxfiles"					});
		trySetFromAny(obs_rtcmfiles,			commandOpts, station_data,	{"obs_rtcmfiles"				});
		trySetFromAny(nav_rtcmfiles,			commandOpts, station_data,	{"nav_rtcmfiles"				});
		tryAddRootToPath(root_stations_dir, rnxfiles);
		tryAddRootToPath(root_stations_dir, obs_rtcmfiles);
		tryAddRootToPath(root_stations_dir, nav_rtcmfiles);

		globber(rnxfiles);
		globber(obs_rtcmfiles);
		globber(nav_rtcmfiles);
	}


	string root_output_dir;
	auto output_files = stringsToYamlObject(yaml, 		{"output_files"});
	{
		trySetFromAny(trace_level,			commandOpts, output_files, {"trace_level"		});

		trySetFromYaml(root_output_dir,			output_files, {"root_output_directory"			});

		trySetFromYaml(output_trace,			output_files, {"output_trace"		});
		trySetFromYaml(trace_directory,			output_files, {"trace_directory"	});
		trySetFromYaml(trace_filename,			output_files, {"trace_filename"		});

		trySetFromYaml(record_rtcm,		     	output_files, {"record_rtcm"		});
		trySetFromYaml(rtcm_directory,			output_files, {"rtcm_directory"	    });
		trySetFromYaml(nav_rtcm_filename,		output_files, {"nav_rtcm_filename"	}); 
		trySetFromYaml(obs_rtcm_filename,		output_files, {"obs_rtcm_filename"	});        
        
		trySetFromYaml(output_residuals,		output_files, {"output_residuals"	});
		trySetFromYaml(output_config,			output_files, {"output_config"		});

		trySetFromYaml(output_summary,			output_files, {"output_summary"		});
		trySetFromYaml(summary_directory,		output_files, {"summary_directory"	});
		trySetFromYaml(summary_filename,		output_files, {"summary_filename"	});

		trySetFromYaml(output_clocks,			output_files, {"output_clocks"		});
		trySetFromYaml(clocks_directory,		output_files, {"clocks_directory"	});
		trySetFromYaml(clocks_filename,			output_files, {"clocks_filename"	});
		trySetFromYaml(output_AR_clocks,		output_files, {"output_AR_clocks"	});
		
		trySetFromYaml(output_ppp_sol,			output_files, {"output_ppp_sol"		});
		trySetFromYaml(ppp_sol_directory,		output_files, {"ppp_sol_directory"	});
		trySetFromYaml(ppp_sol_filename,		output_files, {"ppp_sol_filename"	});

		trySetFromYaml(output_ionex,			output_files, {"output_ionex"		});
		trySetFromYaml(ionex_directory,			output_files, {"ionex_directory"	});
		trySetFromYaml(ionex_filename,			output_files, {"ionex_filename"		});
		
		trySetFromYaml(output_ionstec,			output_files, {"output_ionstec"		});
		trySetFromYaml(ionstec_directory,		output_files, {"ionstec_directory"	});
		trySetFromYaml(ionstec_filename,		output_files, {"ionstec_filename"	});

		trySetFromYaml(output_biasSINEX,		output_files, {"output_biasSINEX"	});
		trySetFromYaml(biasSINEX_directory,		output_files, {"biasSINEX_directory"});
		trySetFromYaml(biasSINEX_filename,		output_files, {"biasSINEX_filename"	});

		trySetFromYaml(output_sinex,			output_files, {"output_sinex"		});
		trySetFromYaml(sinex_directory,			output_files, {"sinex_directory"	});
		trySetFromYaml(sinex_filename,			output_files, {"sinex_filename"		});

		trySetFromYaml(output_persistance,		output_files, {"output_persistance"		});
		trySetFromYaml(input_persistance,		output_files, {"input_persistance"		});
		trySetFromYaml(persistance_directory,	output_files, {"persistance_directory"	});
		trySetFromYaml(persistance_filename,	output_files, {"persistance_filename"	});

		trySetFromYaml(output_mongo_measurements,		output_files, {"output_mongo_measurements"	});
		trySetFromYaml(output_mongo_states,				output_files, {"output_mongo_states"		});
		trySetFromYaml(output_mongo_metadata,			output_files, {"output_mongo_metadata"		});
		trySetFromYaml(output_mongo_logs,				output_files, {"output_mongo_logs"			});
		trySetFromYaml(delete_mongo_history,			output_files, {"delete_mongo_history"		});
		trySetFromYaml(mongo_uri,						output_files, {"mongo_uri"					});

		trySetScaledFromYaml(trace_rotate_period,		output_files, {"trace_rotate_period"	},	{"trace_rotate_period_units"	},	E_Period::_from_string_nocase);
		trySetScaledFromYaml(rtcm_rotate_period,		output_files, {"rtcm_rotate_period"		},	{"rtcm_rotate_period_units"		},	E_Period::_from_string_nocase);
	}

	auto processing_options = stringsToYamlObject(yaml, {"processing_options"});
	{
		trySetFromAny(max_epochs,			commandOpts, processing_options, {"max_epochs"			});
		trySetFromAny(epoch_interval,		commandOpts, processing_options, {"epoch_interval"		});
		trySetFromAny(simulate_real_time,	commandOpts, processing_options, {"simulate_real_time"	});
		
		string startStr;
		string stopStr;
		trySetFromAny(startStr,			commandOpts, processing_options, {"start_epoch"		});
		trySetFromAny(stopStr,			commandOpts, processing_options, {"end_epoch"		});
		if (!startStr.empty())	start_epoch	= boost::posix_time::time_from_string(startStr);
		if (!stopStr .empty())	end_epoch	= boost::posix_time::time_from_string(stopStr);
		
		trySetFromYaml(process_minimum_constraints,	processing_options, {"process_modes","minimum_constraints"	});
		trySetFromYaml(process_network,				processing_options, {"process_modes","network"				});
		trySetFromYaml(process_user,				processing_options, {"process_modes","user"					});
		trySetFromYaml(process_ionosphere,			processing_options, {"process_modes","ionosphere"			});
		trySetFromYaml(process_rts,					processing_options, {"process_modes","rts"					});
		trySetFromYaml(process_tests,				processing_options, {"process_modes","unit_tests"			});

		for (int i = 0; i < E_Sys::_size(); i++)
		{
			int		index	= E_Sys::_values()[i];
			string	sys		= E_Sys::_names() [i];		boost::algorithm::to_lower(sys);

			trySetFromYaml(process_sys[index],	processing_options, {"process_sys", sys	});
		}

		bool found = trySetFromAny(elevation_mask,	commandOpts, processing_options, {"elevation_mask"	});
		if (found)
			elevation_mask *= D2R;

		trySetEnumOpt( ppp_ephemeris, 		processing_options,	{"ppp_ephemeris" 				}, E_Ephemeris::_from_string_nocase);

		trySetFromYaml(tide_solid,			processing_options, {"tide_solid"						});
		trySetFromYaml(tide_otl,			processing_options, {"tide_otl"							});
		trySetFromYaml(tide_pole,			processing_options, {"tide_pole"						});

		trySetFromYaml(phase_windup,		processing_options, {"phase_windup"						});
		trySetFromYaml(reject_eclipse,		processing_options, {"reject_eclipse"					});
		trySetFromYaml(raim,				processing_options, {"raim"								});
		trySetFromYaml(thres_slip,			processing_options, {"cycle_slip",		"thres_slip"	});
		trySetFromYaml(max_inno,			processing_options, {"max_inno"			});
		trySetFromYaml(deweight_factor,		processing_options, {"deweight_factor"	});
		trySetFromYaml(ratio_limit,			processing_options, {"ratio_limit"		});
		trySetFromYaml(max_gdop,			processing_options, {"max_gdop"			});
		trySetFromYaml(antexacs,			processing_options, {"antexacs"			});
		trySetFromYaml(sat_pcv,				processing_options, {"sat_pcv"			});

		trySetFromYaml(pivot_station,		processing_options, {"pivot_station"	});
		trySetFromYaml(pivot_satellite,		processing_options, {"pivot_satellite"	});

		trySetFromYaml(wait_next_epoch,		processing_options, {"wait_next_epoch"	});
		trySetFromYaml(wait_all_stations,	processing_options, {"wait_all_stations"});

		trySetFromYaml(debug_cs,			processing_options, {"debug_cs"			});
		trySetFromYaml(debug_lom,			processing_options, {"debug_lom"		});
		trySetFromYaml(debug_csacc,			processing_options, {"debug_csacc"		});

		vector<string> codePriorityStrings;
		found = trySetFromYaml(codePriorityStrings,	processing_options, {"code_priorities"	});
		if (found)
		{
			code_priorities.clear();
			
			for (auto& codePriorityString : codePriorityStrings)
			{
				try
				{
					auto a = E_ObsCode::_from_string(codePriorityString.c_str());
					code_priorities.push_back(a);
				}
				catch (...)
				{
					continue;
				}
			}
		}
		trySetFromYaml(joseph_stabilisation,		processing_options, {"joseph_stabilisation"						});
	}

	auto user_filter = stringsToYamlObject(yaml, {"user_filter_parameters"});
	{
		trySetEnumOpt( pppOpts.inverter, 				user_filter,	{"inverter" 				}, E_Inverter::_from_string_nocase);
		trySetFromYaml(pppOpts.max_filter_iter,			user_filter,	{"max_filter_iterations"	});
		trySetFromYaml(pppOpts.max_prefit_remv,			user_filter,	{"max_prefit_remvovals"		});
		trySetFromYaml(pppOpts.rts_lag,					user_filter,	{"rts_lag"					});
		trySetFromYaml(pppOpts.rts_directory,			user_filter,	{"rts_directory"			});
		trySetFromYaml(pppOpts.rts_filename,			user_filter,	{"rts_filename"				});
		trySetFromYaml(pppOpts.outage_reset_limit,		user_filter,	{"outage_reset_limit"		});
		trySetFromYaml(pppOpts.phase_reject_limit,		user_filter,	{"phase_reject_limit"		});
	}

	auto network_filter = stringsToYamlObject(yaml, {"network_filter_parameters"});
	{
		trySetEnumOpt( netwOpts.inverter, 			network_filter,	{"inverter" 				}, E_Inverter::_from_string_nocase);
		trySetEnumOpt( netwOpts.filter_mode, 		network_filter, {"process_mode" 			}, E_FilterMode::_from_string_nocase);
		trySetFromYaml(netwOpts.max_filter_iter,	network_filter, {"max_filter_iterations"	});
		trySetFromYaml(netwOpts.max_prefit_remv,	network_filter, {"max_prefit_remvovals"		});
		trySetFromYaml(netwOpts.rts_lag,			network_filter,	{"rts_lag"					});
		trySetFromYaml(netwOpts.rts_directory,		network_filter,	{"rts_directory"			});
		trySetFromYaml(netwOpts.rts_filename,		network_filter,	{"rts_filename"				});
		trySetFromYaml(netwOpts.outage_reset_limit,	network_filter,	{"outage_reset_limit"		});
		trySetFromYaml(netwOpts.phase_reject_limit,	network_filter,	{"phase_reject_limit"		});
	}

	auto troposphere = stringsToYamlObject(processing_options, {"troposphere"});
	{
		trySetEnumOpt( tropOpts.model, 		troposphere, {"model" 		}, E_TropModel::_from_string_nocase);
		trySetFromYaml(tropOpts.vmf3dir,	troposphere, {"vmf3dir"		});
		trySetFromYaml(tropOpts.orography,	troposphere, {"orography"	});
		trySetFromYaml(tropOpts.gpt2grid,	troposphere, {"gpt2grid"	});

		tryAddRootToPath(root_input_dir, tropOpts.vmf3dir);
		tryAddRootToPath(root_input_dir, tropOpts.orography);
		tryAddRootToPath(root_input_dir, tropOpts.gpt2grid);
	}

	auto ionosphere = stringsToYamlObject(processing_options, {"ionosphere"});
	{
		trySetEnumOpt( ionoOpts.corr_mode, 	ionosphere, {"corr_mode" 		}, E_IonoMode::_from_string_nocase);
		trySetEnumOpt( ionoOpts.iflc_freqs,	ionosphere, {"iflc_freqs" 		}, E_LinearCombo::_from_string_nocase);
	}

	auto unit_tests = stringsToYamlObject(yaml, {"unit_test_options"});
	{
		trySetFromYaml(testOpts.output_pass,	unit_tests, {"output_pass"		});
		trySetFromYaml(testOpts.stop_on_fail,	unit_tests, {"stop_on_fail"		});
		trySetFromYaml(testOpts.stop_on_done,	unit_tests, {"stop_on_done"		});
		trySetFromYaml(testOpts.output_errors,	unit_tests, {"output_errors"	});
		trySetFromYaml(testOpts.absorb_errors,	unit_tests, {"absorb_errors"	});
		trySetFromYaml(testOpts.directory,		unit_tests, {"directory"		});
		trySetFromYaml(testOpts.filename,		unit_tests, {"filename"			});

		tryAddRootToPath(root_input_dir, 	testOpts.directory);
	}

	auto ionFilter = stringsToYamlObject(yaml, {"ionosphere_filter_parameters"});
	{
		trySetEnumOpt( ionFilterOpts.model, 			ionFilter, {"model" 				}, E_IonoModel::_from_string_nocase);
		trySetFromYaml(ionFilterOpts.lat_center,		ionFilter, {"lat_center"			});
		trySetFromYaml(ionFilterOpts.lon_center,		ionFilter, {"lon_center"			});
		trySetFromYaml(ionFilterOpts.lat_width,			ionFilter, {"lat_width"				});
		trySetFromYaml(ionFilterOpts.lon_width,			ionFilter, {"lon_width"				});
		trySetFromYaml(ionFilterOpts.lat_res,			ionFilter, {"lat_res"				});
		trySetFromYaml(ionFilterOpts.lon_res,			ionFilter, {"lon_res"				});
		trySetFromYaml(ionFilterOpts.time_res,			ionFilter, {"time_res"				});
		trySetFromYaml(ionFilterOpts.func_order,		ionFilter, {"func_order"			});
		trySetFromYaml(ionFilterOpts.layer_heights,		ionFilter, {"layer_heights"			});
		trySetFromYaml(ionFilterOpts.model_noise,		ionFilter, {"model_noise"			});
		trySetFromYaml(ionFilterOpts.max_filter_iter,	ionFilter, {"max_filter_iterations"	});
		trySetFromYaml(ionFilterOpts.max_prefit_remv,	ionFilter, {"max_filter_removals"	});
		trySetFromYaml(ionFilterOpts.rts_lag,			ionFilter, {"rts_lag"				});
		trySetFromYaml(ionFilterOpts.rts_directory,		ionFilter, {"rts_directory"			});
		trySetFromYaml(ionFilterOpts.rts_filename,		ionFilter, {"rts_filename"			});

		trySetKalmanFromYaml(ionFilterOpts.ion,			ionFilter, "ion");

		for (auto& a : ionFilterOpts.layer_heights)
		{
			a *= 1000; //km to m
		}
	}

	auto ambres_options = stringsToYamlObject(yaml, {"ambiguity_resolution_options"});
	{
		trySetFromYaml(ambrOpts.min_el_AR		  ,	ambres_options, {"Min_elev_for_AR"			});
		trySetFromYaml(ambrOpts.lambda_set	      ,	ambres_options, {"Set_size_for_lambda"		});
		trySetFromYaml(ambrOpts.AR_max_itr	      ,	ambres_options, {"Max_round_iterat"			});
		trySetFromYaml(ambrOpts.solvGPS			  ,	ambres_options, {"GPS_amb_resol"			});
		trySetFromYaml(ambrOpts.solvGLO			  ,	ambres_options, {"GLO_amb_resol"			});
		trySetFromYaml(ambrOpts.solvGAL			  ,	ambres_options, {"GAL_amb_resol"			});
		trySetFromYaml(ambrOpts.solvBDS			  ,	ambres_options, {"BDS_amb_resol"			});
		trySetFromYaml(ambrOpts.solvQZS			  ,	ambres_options, {"QZS_amb_resol"			});
		
		trySetEnumOpt( ambrOpts.WLmode			  , ambres_options,	{"WL_mode" 					}, E_ARmode::_from_string_nocase);
		trySetFromYaml(ambrOpts.WLsuccsThres	  ,	ambres_options, {"WL_succ_rate_thres"		});
		trySetFromYaml(ambrOpts.WLratioThres	  ,	ambres_options,	{"WL_sol_ratio_thres"		});
		trySetFromYaml(ambrOpts.WLSatPrcNois	  ,	ambres_options,	{"WL_procs_noise_sat"		});
		trySetFromYaml(ambrOpts.WLStaPrcNois	  ,	ambres_options,	{"WL_procs_noise_sta"		});
		
		trySetEnumOpt( ambrOpts.NLmode			  , ambres_options,	{"NL_mode" 					}, E_ARmode::_from_string_nocase);
		trySetFromYaml(ambrOpts.NLsuccsThres	  ,	ambres_options, {"NL_succ_rate_thres"		});
		trySetFromYaml(ambrOpts.NLratioThres	  ,	ambres_options, {"NL_sol_ratio_thres"		});
		trySetFromYaml(ambrOpts.NLstarttime		  ,	ambres_options, {"NL_proc_start"			});
		
		trySetFromYaml(ambrOpts.readOSB			  ,	ambres_options, {"read_OSB"					});
		trySetFromYaml(ambrOpts.readDSB			  ,	ambres_options, {"read_DSB"					});
		trySetFromYaml(ambrOpts.readSSRbias		  ,	ambres_options, {"read_SSR"					});
		trySetFromYaml(ambrOpts.readSATbias		  ,	ambres_options, {"read_satellite_bias"		});
		trySetFromYaml(ambrOpts.readRecBias		  ,	ambres_options, {"read_station_bias"		});
		trySetFromYaml(ambrOpts.readHYBbias		  ,	ambres_options, {"read_GLONASS_IFB"			});
		
		trySetFromYaml(ambrOpts.writeOSB		  ,	ambres_options, {"write_OSB"				});
		trySetFromYaml(ambrOpts.writeDSB		  ,	ambres_options, {"write_DSB"				});
		trySetFromYaml(ambrOpts.writeSSRbias	  ,	ambres_options, {"write_SSR_bias"			});
		trySetFromYaml(ambrOpts.writeSATbias	  ,	ambres_options, {"write_satellite_bias"		});
		trySetFromYaml(ambrOpts.writeRecBias	  ,	ambres_options, {"write_station_bias"		});
		
		trySetFromYaml(ambrOpts.biasOutrate		  ,	ambres_options, {"bias_output_rate"			});
	}

	auto minCon = stringsToYamlObject(yaml, {"minimum_constraints"});
	{
		trySetEnumOpt( minCOpts.filter_mode, 			minCon, {"process_mode" 			}, E_FilterMode::_from_string_nocase);
		trySetFromYaml(minCOpts.estimate_scale,			minCon, {"estimate", "scale"		});
		trySetFromYaml(minCOpts.estimate_rotation,		minCon, {"estimate", "rotation"		});
		trySetFromYaml(minCOpts.estimate_translation,	minCon, {"estimate", "translation"	});
	}

	auto outputOptions = stringsToYamlObject(yaml, {"output_options"});
	{
		trySetFromYaml(config_description,	outputOptions, {"config_description"});
		trySetFromYaml(analysis_agency,		outputOptions, {"analysis_agency"	});
		trySetFromYaml(analysis_center,		outputOptions, {"analysis_center"	});
		trySetFromYaml(analysis_program,	outputOptions, {"analysis_program"	});
		trySetFromYaml(rinex_comment,		outputOptions, {"rinex_comment"		});
	}

	auto defaultFilter = stringsToYamlObject(yaml, {"default_filter_parameters"});
	{
		trySetKalmanFromYaml(netwOpts.eop,			defaultFilter, "eop");
	}

	//todo get layer heights and process noise

	auto troposhpere = stringsToYamlObject(processing_options, {"troposhpere"});
	{
		trySetEnumOpt( tropOpts.model, 			troposhpere,	{"model" 			}, E_TropModel::_from_string_nocase);
		trySetFromYaml(tropOpts.gpt2grid,		troposhpere,	{"gpt2grid"			});
		trySetFromYaml(tropOpts.vmf3dir,		troposhpere,	{"vmf3dir"			});
		trySetFromYaml(tropOpts.orography,		troposhpere,	{"orography"		});
	}

	auto cycle_slip = stringsToYamlObject(yaml, {"cycle_slip_filter_parameters"});
	{
		trySetFromYaml(csOpts.enable,							cycle_slip, {"enable"							});
		trySetFromYaml(csOpts.freq_l1,							cycle_slip, {"freq_l1"							});
		trySetFromYaml(csOpts.freq_l2,							cycle_slip, {"freq_l2"							});
		trySetFromYaml(csOpts.print_activity,					cycle_slip, {"print_activity"					});
		trySetFromYaml(csOpts.debug,							cycle_slip, {"debug"							});
		trySetFromYaml(csOpts.timer_debug,						cycle_slip, {"timer_debug"						});
		trySetFromYaml(csOpts.new_channel_break_in_duration,	cycle_slip, {"new_channel_break_in_duration"	});
		trySetFromYaml(csOpts.d_moving_ave_win,					cycle_slip, {"d_moving_ave_win"					});
		trySetFromYaml(csOpts.common_mode_method,				cycle_slip, {"common_mode_method"				});
		trySetFromYaml(csOpts.slip_classify_min_pts,			cycle_slip, {"slip_classify_min_pts"			});
		trySetFromYaml(csOpts.slip_classify_post_outlier_pts,	cycle_slip, {"slip_classify_post_outlier_pts"	});
		trySetFromYaml(csOpts.outlier_p_alpha,					cycle_slip, {"outlier_p_alpha"					});
		trySetFromYaml(csOpts.t_alpha,							cycle_slip, {"t_alpha"							});
		trySetFromYaml(csOpts.min_size_p_alpha,					cycle_slip, {"min_size_p_alpha"					});
		trySetFromYaml(csOpts.outlier_detect_min_pts,			cycle_slip, {"outlier_detect_min_pts"			});
		trySetFromYaml(csOpts.int_valid_pdf_thresh,				cycle_slip, {"int_valid_pdf_thresh"				});
		trySetFromYaml(csOpts.int_valid_outlier_alpha,			cycle_slip, {"int_valid_outlier_alpha"			});
		trySetFromYaml(csOpts.deweight_noise,					cycle_slip, {"deweight_noise"					});
		trySetFromYaml(csOpts.int_valid_combo_jump_alpha,		cycle_slip, {"int_valid_combo_jump_alpha"		});
	}

	auto ssr = stringsToYamlObject(yaml, {"ssr_corrections"});
	{
		trySetFromYaml(ssrOpts.calculate_ssr,			ssr, {"calculate_ssr"			});
		trySetFromYaml(ssrOpts.update_interval,			ssr, {"update_interval"			});
		trySetEnumOpt (ssrOpts.ssr_ephemeris, 			ssr, {"ssr_ephemeris" 			}, E_Ephemeris::_from_string_nocase);
		trySetFromYaml(ssrOpts.upload_to_caster,		ssr, {"upload_to_caster"		});
		trySetFromYaml(ssrOpts.sync_epochs,				ssr, {"sync_epochs"				});
		trySetFromYaml(ssrOpts.sync_epoch_offset,		ssr, {"sync_epoch_offset"		});
		trySetFromYaml(ssrOpts.settime_week_override,	ssr, {"settime_week_override"	});
		trySetFromYaml(ssrOpts.rtcm_directory,			ssr, {"rtcm_directory"			});
		trySetFromYaml(ssrOpts.read_from_files,			ssr, {"read_from_files"			});
	}

	tryAddRootToPath(root_output_dir, 				trace_directory);
	tryAddRootToPath(root_output_dir, 				rtcm_directory);    
	tryAddRootToPath(root_output_dir,				summary_directory);
	tryAddRootToPath(root_output_dir,				clocks_directory);
	tryAddRootToPath(root_output_dir,				ppp_sol_directory);
	tryAddRootToPath(root_output_dir,				ionex_directory);
	tryAddRootToPath(root_output_dir,				ionstec_directory);
	tryAddRootToPath(root_output_dir,				biasSINEX_directory);
	
	tryAddRootToPath(root_output_dir,				sinex_directory);
	tryAddRootToPath(root_output_dir,				persistance_directory);
	tryAddRootToPath(root_output_dir,				ionFilterOpts.rts_directory);
	tryAddRootToPath(root_output_dir,				netwOpts.rts_directory);
	tryAddRootToPath(root_output_dir,				pppOpts.rts_directory);
	tryAddRootToPath(root_output_dir,				ssrOpts.rtcm_directory);

	tryAddRootToPath(trace_directory, 				trace_filename);
    tryAddRootToPath(rtcm_directory, 				nav_rtcm_filename);
    tryAddRootToPath(rtcm_directory, 				obs_rtcm_filename);
	tryAddRootToPath(summary_directory,				summary_filename);
	tryAddRootToPath(clocks_directory,				clocks_filename);
	tryAddRootToPath(ppp_sol_directory,				ppp_sol_filename);
	tryAddRootToPath(ionex_directory,				ionex_filename);
	tryAddRootToPath(ionstec_directory,				ionstec_filename);
	tryAddRootToPath(biasSINEX_directory,			biasSINEX_filename);
	

	tryAddRootToPath(sinex_directory,				sinex_filename);
	tryAddRootToPath(persistance_directory,			persistance_filename);
	tryAddRootToPath(ionFilterOpts.rts_directory,	ionFilterOpts.rts_filename);
	tryAddRootToPath(netwOpts.rts_directory,		netwOpts.rts_filename);
	tryAddRootToPath(pppOpts.rts_directory,			pppOpts.rts_filename);

	//Try to change all filenames to replace <YYYY> etc with other values.
	replaceTimes(navfiles,				start_epoch);
	replaceTimes(orbfiles,				start_epoch);
	replaceTimes(sp3files,				start_epoch);
	replaceTimes(clkfiles,				start_epoch);
	replaceTimes(atxfiles,				start_epoch);
	replaceTimes(snxfiles,				start_epoch);
	replaceTimes(blqfiles,				start_epoch);
	replaceTimes(erpfiles,				start_epoch);
	replaceTimes(dcbfiles,				start_epoch);
	replaceTimes(bsxfiles,				start_epoch);
	replaceTimes(ionfiles,				start_epoch);
	replaceTimes(trace_directory,		start_epoch);
	replaceTimes(rtcm_directory,		start_epoch);
	replaceTimes(trace_filename,		start_epoch);
	replaceTimes(ionex_directory,		start_epoch);
	replaceTimes(ionex_filename,		start_epoch);
	replaceTimes(ionstec_directory,		start_epoch);
	replaceTimes(ionstec_filename,		start_epoch);
	replaceTimes(biasSINEX_directory,	start_epoch);
	replaceTimes(biasSINEX_filename,	start_epoch);
	replaceTimes(summary_directory,		start_epoch);
	replaceTimes(summary_filename,		start_epoch);
	replaceTimes(clocks_directory,		start_epoch);
	replaceTimes(clocks_filename,		start_epoch);
	replaceTimes(ppp_sol_directory,		start_epoch);
	replaceTimes(ppp_sol_filename,		start_epoch);
	replaceTimes(root_stations_dir,		start_epoch);
	replaceTimes(sinex_directory,		start_epoch);
	replaceTimes(sinex_filename,		start_epoch);
	replaceTimes(ssrOpts.rtcm_directory,		start_epoch);
	replaceTimes(persistance_directory,			start_epoch);
	replaceTimes(persistance_filename,			start_epoch);
	replaceTimes(pppOpts.rts_filename,			start_epoch);
	replaceTimes(netwOpts.rts_filename,			start_epoch);
	replaceTimes(ionFilterOpts.rts_filename,	start_epoch);
    
    // The network stream time is in UTC to avoid problems with local time.
    replaceTimes(nav_rtcm_filename,	 	boost::posix_time::second_clock::universal_time());
    replaceTimes(obs_rtcm_filename,	 	boost::posix_time::second_clock::universal_time());

	for (auto& station_file : station_files)
	{
		replaceTimes(station_file,		start_epoch);
	}



	trySetFromYaml(caster_test,	yaml,{"station_data", "caster_test"});
	trySetFromYaml(print_stream_statistics,	yaml,{"station_data", "print_stats"});

	

	
	string streamRoot = "";
	trySetFromYaml(streamRoot,		yaml,	{"station_data", "stream_root"	});
	if( caster_test )
		caster_stream_root = streamRoot;

	auto obsStreamNode = stringsToYamlObject(yaml, {"station_data","obs_streams"});
	auto navStreamNode = stringsToYamlObject(yaml, {"station_data","nav_streams"});

	
	// Unique list of host names.
	std::vector<std::string> hosts;
	for (auto nav : {false, true})
	{
		YAML::Node* streamNode_ptr;
		if (nav == false)	streamNode_ptr = &obsStreamNode;
		else				streamNode_ptr = &navStreamNode;
		
		map<string, string> ntripStreams;
		for (auto streamUrl : *streamNode_ptr)
		{
			string streamUrl_str = streamUrl.as<string>();
			string fullUrl = streamUrl_str;

			tryAddRootToPath(streamRoot, fullUrl);
			if (fullUrl.front() == '/')
				fullUrl.erase(fullUrl.begin()); // in case of full urls given in station_data.streams
			while (fullUrl.back() == '/')
				fullUrl.pop_back();				// in case of terminating '/'
			std::size_t slashPos = fullUrl.find_last_of("/");	// find first 4 characters after last '/'
			string hostname = fullUrl.substr(0,slashPos+1);		// e.g. http://user:pass@auscors.ga.gov.au:2101/BCEP00BKG0 --> BCEP

			if( std::find(hosts.begin(), hosts.end(), hostname) == hosts.end() )
			{
				hosts.push_back( hostname );
			}
		}
	}

	std::multimap<std::string,std::vector<std::string>> hostMap;
	

#ifndef	ENABLE_UNIT_TESTS
	NtripSocket::startClients();
#endif	
	for( auto host : hosts )
	{
		NtripSourceTable sourceTable(host);
		sourceTable.getSourceTable();
		hostMap.insert({host,sourceTable.getStreamMounts()});	
	}
	
	for (auto nav : {false, true})
	{
		YAML::Node* streamNode_ptr;
		if (nav == false)	streamNode_ptr = &obsStreamNode;
		else				streamNode_ptr = &navStreamNode;
		
		map<string, string> ntripStreams;
		for (auto streamUrl : *streamNode_ptr)
		{
			string streamUrl_str = streamUrl.as<string>();
			string fullUrl = streamUrl_str;

			tryAddRootToPath(streamRoot, fullUrl);
			if (fullUrl.front() == '/')
				fullUrl.erase(fullUrl.begin()); // in case of full urls given in station_data.streams
			while (fullUrl.back() == '/')
				fullUrl.pop_back();				// in case of terminating '/'
			std::size_t slashPos = fullUrl.find_last_of("/");	// find first 4 characters after last '/'
			string id		= fullUrl.substr(slashPos + 1, 4);		// e.g. http://user:pass@auscors.ga.gov.au:2101/BCEP00BKG0 --> BCEP
			string hostname = fullUrl.substr(0,slashPos + 1);		// e.g. http://user:pass@auscors.ga.gov.au:2101/BCEP00BKG0 --> BCEP
			string mount	= fullUrl.substr(slashPos+1, fullUrl.length()-slashPos+1);
			bool foundMount = false;
			auto host = hostMap.find(hostname);
			if (host != hostMap.end())
			{
				auto mounts = host->second;
				if (std::find(mounts.begin(), mounts.end(), mount) != mounts.end())
					foundMount = true;
			}
			
			if (foundMount)
			{
				ntripStreams[id] = fullUrl;
			}
			else
			{
				URL url = URL::parse(fullUrl);
				BOOST_LOG_TRIVIAL(warning) 
						<< "Stream, " << url.sanitised() << " not found in sourcetable, invalid host/mount combination.\n";
			}
		}

		for (auto& [id, streamUrl] : ntripStreams)
		{
			if (streamDOAMap.find(streamUrl) != streamDOAMap.end())
			{
				//this stream was already added, dont re-add
				continue;
			}

	
			auto ntripStream_ptr = std::make_shared<NtripRtcmStream>(streamUrl);
			ntripRtcmMultimap.insert({id, ntripStream_ptr}); // for network (internet) tracing
	
			
			if (record_rtcm)
			{
				NtripRtcmStream& downloadStream = *ntripStream_ptr;
				string filename;
				if (nav)		filename = nav_rtcm_filename;
				else			filename = obs_rtcm_filename;
				
				replaceString(filename, "<STATION>", id);	
				
				downloadStream.rtcm_filename = filename;
				downloadStream.createRtcmFile();
			}
			
			if (nav == false)		{	obsStreamMultimap.insert({id, std::move(ntripStream_ptr)});		}
			else					{	navStreamMultimap.insert({id, std::move(ntripStream_ptr)});		}			
			streamDOAMap[streamUrl] = false;
		}
	}


	

	boost::filesystem::path root_stations_path(root_stations_dir);

	if (boost::filesystem::is_directory(root_stations_path))
	{
		for (auto& rnxfile : rnxfiles)
		{
			addDataFile(rnxfile, "RINEX", "OBS");
		}

		for (auto& rtcmfile : obs_rtcmfiles)
		{
			addDataFile(rtcmfile, "RTCM", "OBS");
		}
		
		for (auto& rtcmfile : nav_rtcmfiles)
		{
			addDataFile(rtcmfile, "RTCM", "NAV");
		}		

		if (obsStreamMultimap.empty())
		{
			BOOST_LOG_TRIVIAL(error)
			<< "No station observation (rnx) files found in "
			<< root_stations_dir;
		}
	}
	
	string ouputStreamRoot = "";
	trySetFromYaml(ouputStreamRoot,	yaml,{"output_streams", "stream_root"});

	auto outStreamNode = stringsToYamlObject(yaml, {"output_streams","stream_label"});
	
	for (auto outLabelYaml : outStreamNode)
	{
		string outLabel = outLabelYaml.as<string>();
		auto outStreamsYaml = stringsToYamlObject(yaml, {"output_streams",outLabel});
		
		bool addOutStream = false;
		string fullUrl = "";
		trySetFromYaml(fullUrl,outStreamsYaml,{"streams"});
		tryAddRootToPath(ouputStreamRoot, fullUrl);
		auto messageNodes = stringsToYamlObject(outStreamsYaml, {"messages"});
		auto ntripOutStream_ptr = std::make_shared<NtripBroadcaster::NtripUploadClient>(fullUrl);
		NtripBroadcaster::NtripUploadClient& outStreamOb = *ntripOutStream_ptr.get();
		for (auto outMessage : messageNodes)
		{ 
			string messStr = outMessage.as<string>();
			try 
			{
				int messNum = (int)(std::stod(messStr));
				RtcmMessageType mess = RtcmMessageType::_from_integral(messNum);
				outStreamOb.rtcmMessagesTypes.push_back(mess);
			}
			catch (std::exception& e)
			{
				BOOST_LOG_TRIVIAL(error) << "Error defining output stream message for label : " << outLabel;
				exit(1);                
			}
			addOutStream = true;    
		}
		if( addOutStream )
			outStreamManager.ntripUploadStreams.insert({outLabel, std::move(ntripOutStream_ptr)});
	}
	

#	ifndef ENABLE_MONGODB
	if	( output_mongo_measurements
		||output_mongo_metadata
		||output_mongo_states)
	{
		std::cout << std::endl << "Error: Mongodb output requested by config but this is a non-Mongodb binary." << std::endl;
		std::cout << std::endl << "Mongodb can be enabled by building with $ cmake -D ENABLE_MONGODB=ON .." << std::endl;
		exit(1);
	}
#	endif

#	ifndef ENABLE_UNIT_TESTS
	if (process_tests)
	{
		std::cout << std::endl << "Error: Tests requested by config but this is a non-test binary." << std::endl;
		std::cout << std::endl << "Tests can be enabled by building with $ cmake -DENABLE_UNIT_TESTS=ON .." << std::endl;
		exit(1);
	}
#	endif

	return true;
}
