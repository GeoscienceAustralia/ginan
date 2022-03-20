
// #pragma GCC optimize ("O0")

#include <iostream>
#include <sstream>
#include <memory>
#include <string>
#include <tuple>
#include <map>

using std::unique_ptr;
using std::multimap;
using std::string;
using std::tuple;
using std::map;

#include <boost/log/utility/setup/console.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/program_options.hpp>
#include <boost/log/trivial.hpp>
#include <boost/filesystem.hpp>
#include <boost/regex.hpp>

#include <yaml-cpp/yaml.h>

#include "peaCommitVersion.h"
#include "constants.hpp"
#include "acsConfig.hpp"

ACSConfig acsConfig = {};



typedef tuple<YAML::Node, string> NodeStack;

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
	if (path.empty())	{		return;	}
	if (root == "./")	{		return;	}
	
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

void ACSConfig::outputDefaultConfigutation()
{
	std::cout << std::endl << "Default configuration values:\n\n";
	
	vector<string> yamlStack;
	
	for (auto& [defaultStack, defaultVals] : yamlDefaults)
	{
		auto& [defaultVal, comment] = defaultVals;
		
		size_t pos_start = 0;
		size_t pos_end;
		
		string token;
		vector<string> tokens;
	
		while ((pos_end = defaultStack.find(":", pos_start)) != string::npos) 
		{
			token = defaultStack.substr(pos_start, pos_end - pos_start);
			pos_start = pos_end + 1;
			tokens.push_back(token);
		}
		
		for (int i = 0; i < tokens.size(); i++)
		{
			while	( (i + 1 > yamlStack.size())
					||(tokens[i] != yamlStack[i]))
			{
				if		(i + 1 > yamlStack.size())
				{
					//need to add a new one
					yamlStack.push_back(tokens[i]);
					
					printf("%-30s", (tokens[i]+":").c_str());
// 						std::cout << tokens[i] << ":";
					
					if (i + 1 < tokens.size())
					{
						//new line and start indenting again
						std::cout << "\n";
						i = 0;
					}
				}
				else if	(tokens[i] != yamlStack[i])
				{
					yamlStack.pop_back();
				}
			}
			std::cout << "    ";
		}
		
		printf(" %-30s", (defaultVal).c_str());
		
		if (comment.empty() == false)
		{
			std::cout << "# " << comment;
		}
		std::cout << std::endl;
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
	("help,h", 																"Help")
	("quiet,q", 															"Less output")
	("verbose,v", 															"More output")
	("very-verbose,V", 														"Much more output")
	("yaml-defaults,Y",														"Print complete set of parsed parameters and their default values")
	("config,y", 				boost::program_options::value<string>(), 	"Configuration file")
	("trace_level,l", 			boost::program_options::value<int>(), 		"Trace level")
	("fatal_level,L", 			boost::program_options::value<int>(), 		"Fatal error level")
	("elevation_mask,e", 		boost::program_options::value<float>(), 	"Elevation Mask")
	("max_epochs,n", 			boost::program_options::value<int>(), 		"Maximum Epochs")
	("epoch_interval,i", 		boost::program_options::value<float>(), 	"Epoch Interval")
	("stream_user,u", 			boost::program_options::value<string>(),	"Username for RTCM streams")
	("stream_pass,p", 			boost::program_options::value<string>(),	"Password for RTCM streams")
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
	("rnx", 					boost::program_options::value<string>(),	"RINEX station file")
	("egmfiles", 				boost::program_options::value<string>(),	"Earth gravity model coefficients file")
	("jplfiles", 				boost::program_options::value<string>(),	"JPL planetary and lunar ephemerides file")
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

	
	if (vm.count("yaml-defaults"))
	{
		acsConfig.parse("", vm);
		
		exit(EXIT_SUCCESS);	
	}
	
	if (vm.count("config"))
	{
		string config = vm["config"].as<string>();

		bool pass = acsConfig.parse(config, vm);

		if (!pass)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Configuration from " << config << " aborted";

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
	valid &= checkValidFiles(acsConfig.egmfiles, 			"Earth gravity model coefficients (egm file)");
	valid &= checkValidFiles(acsConfig.jplfiles, 			"JPL planetary and lunar ephemerides (jpl file)");
	valid &= checkValidFile (acsConfig.tropOpts.gpt2grid,	"grid");

	if	(acsConfig.snxfiles.empty())
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Invalid SINEX file ";
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
	ss << "\tclkfiles:  "; for (auto& a : clkfiles) ss << a << " "; ss << "\n";
	ss << "\tbsxfiles:  "; for (auto& a : bsxfiles) ss << a << " "; ss << "\n";
	ss << "\tionfiles:  "; for (auto& a : ionfiles) ss << a << " "; ss << "\n";
	ss << "\tblqfiles:  "; for (auto& a : blqfiles) ss << a << " "; ss << "\n";
	ss << "\terpfiles:  "; for (auto& a : erpfiles) ss << a << " "; ss << "\n";
	ss << "\tsp3files:  "; for (auto& a : sp3files) ss << a << " "; ss << "\n";
	ss << "\torbfiles:  "; for (auto& a : orbfiles) ss << a << " "; ss << "\n";
	ss << "\tegmfiles:  "; for (auto& a : egmfiles) ss << a << " "; ss << "\n";
	ss << "\tegmfiles:  "; for (auto& a : jplfiles) ss << a << " "; ss << "\n";
	ss << "\tvmf3dir:   " << tropOpts.vmf3dir 			<< "\n";
	ss << "\torography: " << tropOpts.orography 		<< "\n";
	ss << "\tgrid:      " << tropOpts.gpt2grid 			<< "\n";
	ss << "\ttestfiles: " << testOpts.filename			<< "\n";
	ss << "\n";

	ss << "Outputs:\n";
	if (output_trace)				{	ss << "\tTrace level:                   " << trace_level 					<< "\n"; }
	if (output_trace)				{	ss << "\ttrace_filename:                " << trace_filename 				<< "\n"; }
	if (output_summary)				{	ss << "\tsummary_filename:              " << summary_filename 				<< "\n"; }
	if (output_clocks)				{	ss << "\tclocks_filename:               " << clocks_filename 				<< "\n"; }
	if (output_ionex)				{	ss << "\tionex_filename:                " << ionex_filename 				<< "\n"; }
	if (output_ionstec)				{	ss << "\tionstec_filename:              " << ionstec_filename 				<< "\n"; }
	if (output_bias_sinex)			{	ss << "\tbias_sinex_filename:           " << bias_sinex_filename			<< "\n"; }
	if (output_trop_sinex)			{	ss << "\ttrop_sinex_filename:           " << trop_sinex_filename			<< "\n"; }
	if (output_decoded_rtcm_json)	{	ss << "\tdecoded_rtcm_json_filename:    " << decoded_rtcm_json_filename		<< "\n"; }
	if (output_encoded_rtcm_json)	{	ss << "\tencoded_rtcm_json_filename:    " << encoded_rtcm_json_filename		<< "\n"; }

	ss << "\n";

	ss << "Process Modes:\n";
	ss << "\tPreprocessor:        " << process_preprocessor			<< "\n";
	ss << "\tUser:                " << process_user 				<< "\n";
	ss << "\tNetwork:             " << process_network 				<< "\n";
	ss << "\tMinimum Constraints: " << process_minimum_constraints 	<< "\n";
	ss << "\tIonospheric:         " << process_ionosphere 			<< "\n";
	ss << "\tRTS Smoothing:       " << process_rts 					<< "\n";
	ss << "\tUnit tests:          " << process_tests				<< "\n";
	ss << "\tPPP:                 " << process_ppp					<< "\n";
	ss << "\n";

	ss << "Systems:\n";
	ss << "\tGPS:     " << process_sys[E_Sys::GPS] 		<< "\n";
	ss << "\tGLONASS: " << process_sys[E_Sys::GLO] 		<< "\n";
	ss << "\tGALILEO: " << process_sys[E_Sys::GAL] 		<< "\n";
	ss << "\tBEIDOU:  " << process_sys[E_Sys::BDS] 		<< "\n";
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
NodeStack stringsToYamlObject(
	NodeStack		yamlBase, 						///< Yaml node to search within
	vector<string>	yamlNodeDescriptor)				///< List of strings of keys to trace hierarchy
{
	YAML::Node currentNode;

	auto [node, stack] = yamlBase;
	currentNode.reset(node);
	
	for (auto& desc : yamlNodeDescriptor)
	{
		auto test = currentNode[desc];
		if (!test)
		{
// 			return {test, ""};
		}

		currentNode.reset(currentNode[desc]);
		stack += desc + ":";
	}
	
	return {currentNode, stack};
}

string stringify(
	string value)
{
	return value;
}

template<typename TYPE>
string stringify(
	TYPE value)
{
	string output;
	output += std::to_string(value);
	return output;
}

template<typename TYPE>
string stringify(
	vector<TYPE>& vec)
{
	string output;
	output += "[";
	
	for (int i = 0; i < vec.size(); i++)
	{
		output += stringify(vec[i]);
		
		if (i < vec.size() - 1)
		{
			output += ", ";
		}
	}
	output += "]";
    return output;
}

// using namespace std;


/** Set an output from yaml object if found
*/
template<typename TYPE>
bool trySetFromYaml(
	TYPE&			output,				///< Variable to output to
	NodeStack		yamlBase,			///< Yaml node to search within
	vector<string>	yamlNodeDescriptor,	///< List of strings of keys to trace hierarcy
	string			comment = "")		///< Description to provide to user for automatic documentation
{
	auto [optNode, stack] = stringsToYamlObject(yamlBase,			yamlNodeDescriptor);
	
	if (acsConfig.yamlDefaults.find(stack) == acsConfig.yamlDefaults.end())
	{
		acsConfig.yamlDefaults[stack] = {stringify(output), comment};
	}
	
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
	NodeStack&								yamlBase,			///< Yaml node to search within
	vector<string>							nodeDescriptor,		///< List of strings of keys to trace hierarcy
	string									comment = "")		///< Description to provide to user for automatic documentation
{
	bool found = false;
	found |= trySetFromYaml(output, yamlBase,	nodeDescriptor, comment);
	found |= trySetFromOpts(output, commandOpts,nodeDescriptor);
	return found;
}

/** Set an enum from yaml, decoding strings to ints
*/
template <typename ENUM>
void trySetEnumOpt(
	ENUM&			out,							///< Variable to output to
	NodeStack		yamlBase,						///< Yaml node to search within
	vector<string>	yamlNodeDescriptor,				///< List of strings of keys to trace hierarcy
	ENUM			(&_from_string)(const char*),	///< Function to decode enum strings
	string			comment = "")					///< Description to provide to user for automatic documentation
{
	auto [optNode, stack] = stringsToYamlObject(yamlBase, yamlNodeDescriptor);

	if (acsConfig.yamlDefaults.find(stack) == acsConfig.yamlDefaults.end())
	{
		acsConfig.yamlDefaults[stack] = {out._to_string(), comment};
	}
	
	string value;
	try
	{
		value = optNode.as<string>();
	}
	catch (...) 
	{
		return;
	}
	
	try
	{
		out = _from_string(value.c_str());
	}
	catch (...)	
	{
		BOOST_LOG_TRIVIAL(error)
		<< "\nError: " << value << " is not a valid entry for option: " << yamlNodeDescriptor.back() << ".\n"
		<< "Valid options include:";
		
		for (const char* name : ENUM::_names())
		{
			BOOST_LOG_TRIVIAL(error) << name << std::endl;
		}
		exit(0);
	}
}

/** Set the variables associated with kalman filter states from yaml
*/
void trySetKalmanFromYaml(
	KalmanModel&	output,	///< Variable to output to
	NodeStack&		yaml,	///< Yaml node to search within
	string			key)	///< Key of yaml object
{
	auto [node, stack] = yaml;
	auto entry = node[key];
	if (entry)
	{
		NodeStack newYaml = {entry, stack + key + ":"};
		E_Period proc_noise_dt = E_Period::SECOND;
						trySetEnumOpt(proc_noise_dt,			newYaml, {"proc_noise_dt"	}, E_Period::_from_string_nocase, "(enum) Time unit for process noise - sqrt_sec, sqrt_day etc.");
						trySetFromYaml(output.estimate, 		newYaml, {"estimated"		}, "(bool) Estimate state in kalman filter");

						trySetFromYaml(output.sigma,			newYaml, {"sigma" 			}, "[floats] Apriori sigma values");
						trySetFromYaml(output.apriori_val,		newYaml, {"apriori_val"		}, "[floats] Apriori state values");
		bool found = 	trySetFromYaml(output.proc_noise,		newYaml, {"proc_noise" 		}, "[floats] Process noise sigmas");
		bool foundTau =	trySetFromYaml(output.tau,				newYaml, {"tau"				}, "[floats] Correlation times for gauss markov noise, defaults to -1 -> inf (Random Walk)");
						trySetFromYaml(output.mu,				newYaml, {"mu"				}, "[floats] Desired mean value for gauss markov states");
		if (found)
		{
			for (auto& proc : output.proc_noise)
			{
				proc /= sqrt((int)proc_noise_dt);
			}
		}
		if (foundTau)
		{
			for (auto& tau : output.tau)
			{
				tau *= proc_noise_dt;		//todo aaron, this should probably be renamed, with a release note
			}
		}
	}
}

/** Set satellite options from yaml
*/
void getFromYaml(
	SatelliteOptions&	satOpts,			///< Satellite options variable to output to
	NodeStack			yamlBase,			///< Yaml node to search within
	vector<string>		yamlNodeDescriptor)	///< List of strings of keys of yaml hierarchy
{
	auto satNode = stringsToYamlObject(yamlBase, yamlNodeDescriptor);

	trySetKalmanFromYaml(satOpts.clk,					satNode, "clk"						);
	trySetKalmanFromYaml(satOpts.clk_rate,				satNode, "clk_rate"					);
	trySetKalmanFromYaml(satOpts.clk_rate_gauss_markov,	satNode, "clk_rate_gauss_markov"	);
	trySetKalmanFromYaml(satOpts.pos,					satNode, "pos"						);
	trySetKalmanFromYaml(satOpts.pos_rate,				satNode, "pos_rate"					);
	trySetKalmanFromYaml(satOpts.orb,					satNode, "orb"						);
	trySetKalmanFromYaml(satOpts.srp,					satNode, "srp"						);
	trySetKalmanFromYaml(satOpts.pco,					satNode, "pco"						);
	trySetKalmanFromYaml(satOpts.ant,					satNode, "ant"						);
	trySetKalmanFromYaml(satOpts.code_bias,				satNode, "code_bias"				);
	trySetKalmanFromYaml(satOpts.phase_bias,			satNode, "phase_bias"				);

	trySetFromYaml(satOpts.exclude, 	satNode, {"exclude"});

	satOpts._initialised = true;
}

/** Set receiver options from yaml
*/
void getFromYaml(
	ReceiverOptions&	recOpts, 			///< Receiver options variable to output to
	NodeStack			yamlBase,			///< Yaml node to search within
	vector<string>		yamlNodeDescriptor)	///< List of strings of keys of yaml hierarchy
{
	auto recNode = stringsToYamlObject(yamlBase, yamlNodeDescriptor);

	trySetKalmanFromYaml(recOpts.clk,						recNode, "clk"			);
	trySetKalmanFromYaml(recOpts.clk_rate,					recNode, "clk_rate"		);
	trySetKalmanFromYaml(recOpts.clk_rate_gauss_markov,		recNode, "clk_rate_gauss_markov"	);
	trySetKalmanFromYaml(recOpts.pos,						recNode, "pos"			);
	trySetKalmanFromYaml(recOpts.pos_rate,					recNode, "pos_rate"		);
	trySetKalmanFromYaml(recOpts.keplers,					recNode, "keplers"		);
	trySetKalmanFromYaml(recOpts.amb,						recNode, "amb"			);
	trySetKalmanFromYaml(recOpts.pco,						recNode, "pco"			);
	trySetKalmanFromYaml(recOpts.ant,						recNode, "ant"			);
	trySetKalmanFromYaml(recOpts.code_bias,					recNode, "code_bias"	);
	trySetKalmanFromYaml(recOpts.phase_bias,				recNode, "phase_bias"	);
	trySetKalmanFromYaml(recOpts.ion,						recNode, "ion"			);
	trySetKalmanFromYaml(recOpts.trop,						recNode, "trop"			);
	trySetKalmanFromYaml(recOpts.trop_grads,				recNode, "trop_grads"	);
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
				getFromYaml(globalOpts, {yaml, ""}, {"default_filter_parameters", "satellites"});
				globalOpts._initialised = true;
			}

			//initialise from its parent
			sysOpts = globalOpts;

			//get specifics from config file
			string sys = "SYS_" + Sat.sysName();
			getFromYaml(sysOpts, {yaml, ""}, {"override_filter_parameters", "satellites", sys});
		}

		//initialise from its parent
		blockOpts = sysOpts;

		//get specifics from config file
		string block = Sat.blockType();
		getFromYaml(blockOpts, {yaml, ""}, {"override_filter_parameters", "satellites", block});
	}

	//initialise from its parent
	satOpts = blockOpts;

	//get specifics from config file
	string prn = "PRN_" + Sat.id();
	string svn = "SVN_" + Sat.svn();
	getFromYaml(satOpts, {yaml, ""}, {"override_filter_parameters", "satellites", prn});
	getFromYaml(satOpts, {yaml, ""}, {"override_filter_parameters", "satellites", svn});

	return satOpts;
}

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
		getFromYaml(globalOpts, {yaml, ""}, {"default_filter_parameters", "stations"});
		globalOpts._initialised = true;
	}

	//initialise from its parent
	recOpts = globalOpts;

	//get specifics from config file
	getFromYaml(recOpts, {yaml, ""}, {"override_filter_parameters", "stations", id});

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
		trySetFromYaml(globalOpts.noise, {yaml, ""}, {"minimum_constraints", "station_default_noise"});
		globalOpts._initialised = true;
	}

	//initialise from its parent
	recOpts = globalOpts;

	//get specifics from config file
	trySetFromYaml(recOpts.noise, {yaml, ""}, {"minimum_constraints", "station_noise", id});

	return recOpts;
}

/** Set and scale a variable according to yaml options
*/
template<typename ENUM>
void trySetScaledFromYaml(
	double&			output,							///< Variable to output to
	NodeStack		node,							///< Yaml node to search within
	vector<string>	number_parameter,				///< List of keys of the hierarchy to the value to be set
	vector<string>	scale_parameter,				///< List of keys of the hierarchy to the scale to be applied
	ENUM			(&_from_string)(const char*))	///< Function to decode scale enum strings
{
	double	number			= 0;
	ENUM	number_units	= ENUM::_from_integral(1);
	trySetFromYaml(number,			node, number_parameter);
	trySetEnumOpt(number_units, 	node, scale_parameter,	_from_string);
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

		if (replacement.empty())
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: " << subStr << " is used in config but is not defined";
		}

		str.erase	(index, subStr.size());
		str.insert(index, replacement);
	}
}

/** Replace macros for times with configured values.
* Available replacements are "<CONFIG> <USER> <PASS> <BRANCH>"
*/
void replaceTags(
	string&						str)		///< String to replace macros within
{
	replaceString(str, "<CONFIG>",	acsConfig.config_description);
	replaceString(str, "<USER>",	acsConfig.stream_user);
	replaceString(str, "<PASS>",	acsConfig.stream_pass);
	replaceString(str, "<BRANCH>",	GINAN_BRANCH_NAME);
}

/** Replace macros for times with numeric values.
* Available replacements are "<DDD> <D> <WWWW> <YYYY> <YY> <MM> <DD> <HH> <hh> <mm> <LOGTIME>"
*/
void replaceTimes(
	string&						str,		///< String to replace macros within
	boost::posix_time::ptime	time_time)	///< Time to use for replacements
{
	string DDD		= "";
	string D		= "";
	string WWWW		= "";
	string YYYY		= "";
	string YY		= "";
	string MM		= "";
	string DD		= "";
	string HH		= "";
	string mm		= "";

	if (!time_time.is_not_a_date_time())
	{
		string gpsWeek0 = "1980-01-06 00:00:00.000";
		auto gpsZero = boost::posix_time::time_from_string(gpsWeek0);
		string time_string = boost::posix_time::to_iso_string(time_time);

		auto tm = to_tm(time_time);
		std::ostringstream ss;
		ss << std::setw(3) << std::setfill('0') << tm.tm_yday+1;
		string ddd = ss.str();

		auto gpsWeek = (time_time - gpsZero);
		int weeks = gpsWeek.hours() / 24 / 7;
		ss.str("");
		ss << std::setw(4) << std::setfill('0') << weeks;
		string wwww = ss.str();

		DDD	= ddd;
		D	= std::to_string(tm.tm_wday);
		WWWW	= wwww;
		YYYY	= time_string.substr(0,		4);
		YY		= time_string.substr(2,		2);
		MM		= time_string.substr(4,		2);
		DD		= time_string.substr(6,		2);
		HH		= time_string.substr(9,		2);
		mm		= time_string.substr(11,	2);
	}

	replaceString(str, "<LOGTIME>",	"<YYYY>-<MM>-<DD>_<HH>:<mm>");
	replaceString(str, "<DDD>",		DDD);
	replaceString(str, "<D>",		D);
	replaceString(str, "<WWWW>",	WWWW);
	replaceString(str, "<YYYY>",	YYYY);
	replaceString(str, "<YY>",		YY);
	replaceString(str, "<MM>",		MM);
	replaceString(str, "<DD>",		DD);
	replaceString(str, "<HH>",		HH);
	replaceString(str, "<hh>",		HH);
	replaceString(str, "<mm>",		mm);
}

void replaceTags(
	vector<string>&				strs)
{
	for (auto& str : strs)
	{
		replaceTags(str);
	}
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

bool checkGlob(
	string str1, 
	string str2)
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

	if (filename != "")
	{
		boost::filesystem::path filePath(filename);
		auto currentConfigModifyTime = boost::filesystem::last_write_time(filePath);
		
		if (currentConfigModifyTime == configModifyTimeMap["CONFIG"])
		{
			return false;
		}
		
		configModifyTimeMap["CONFIG"] = currentConfigModifyTime;
	}
	

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
		if (commandOpts.count("yaml-defaults"))	
		{
			//we expect to break, continue parsing
		}
		else
		{
			BOOST_LOG_TRIVIAL(error) << "\nFailed to parse configuration file " << filename;
			BOOST_LOG_TRIVIAL(error) << e.msg << std::endl;
			return false;	
		}
	}
	catch (const YAML::ParserException& e)
	{
		BOOST_LOG_TRIVIAL(error) << "\nFailed to parse configuration. Check for errors as described near the below:\n";
		BOOST_LOG_TRIVIAL(error) << e.what() << std::endl << std::endl;
		return false;	
	}

	// General Files
	auto input_files	= stringsToYamlObject({yaml, ""},	{"input_files"});
	{
		trySetFromAny(root_input_dir,	commandOpts, input_files, {"root_input_directory"	}, "(string) Root path to be added to all other input files (unless they are absolute)");
		
		trySetFromAny(atxfiles,			commandOpts, input_files, {"atxfiles"				}, "[string] List of antex      files to use");
		trySetFromAny(snxfiles,			commandOpts, input_files, {"snxfiles"				}, "[string] List of sinex      files to use");
		trySetFromAny(blqfiles,			commandOpts, input_files, {"blqfiles"				}, "[string] List of blq        files to use");
		trySetFromAny(navfiles,			commandOpts, input_files, {"navfiles"				}, "[string] List of ephemeris  files to use");
		trySetFromAny(sp3files,			commandOpts, input_files, {"sp3files"				}, "[string] List of sp3        files to use");
		trySetFromAny(erpfiles,			commandOpts, input_files, {"erpfiles"				}, "[string] List of erp        files to use");
		trySetFromAny(dcbfiles,			commandOpts, input_files, {"dcbfiles"				}, "[string] List of dcb        files to use");
		trySetFromAny(bsxfiles,			commandOpts, input_files, {"bsxfiles"				}, "[string] List of biassinex  files to use");
		trySetFromAny(ionfiles,			commandOpts, input_files, {"ionfiles"				}, "[string] List of ion        files to use");
		trySetFromAny(clkfiles,			commandOpts, input_files, {"clkfiles"				}, "[string] List of clock      files to use");
		trySetFromAny(orbfiles,			commandOpts, input_files, {"orbfiles"				}, "[string] List of orbit(pod) files to use");
		trySetFromAny(egmfiles,			commandOpts, input_files, {"egmfiles"				}, "[string] List of egm        files to use");
		trySetFromAny(jplfiles,			commandOpts, input_files, {"jplfiles"				}, "[string] List of jpl        files to use");

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
		tryAddRootToPath(root_input_dir, egmfiles);
		tryAddRootToPath(root_input_dir, jplfiles);

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
		globber(egmfiles);
		globber(jplfiles);
	}

	auto station_data = stringsToYamlObject({yaml, ""}, {"station_data"});
	{
		trySetFromAny(root_stations_dir,		commandOpts, station_data,	{"root_stations_directory"		}, "(string) Root path to be added to all other station files (unless they are absolute)");
		trySetFromAny(rnxfiles,					commandOpts, station_data,	{"rnxfiles"						}, "[string] List of rinex      files to use");
		trySetFromAny(obs_rtcmfiles,			commandOpts, station_data,	{"obs_rtcmfiles"				}, "[string] List of rtcmfiles  files to use for observations");
		trySetFromAny(nav_rtcmfiles,			commandOpts, station_data,	{"nav_rtcmfiles"				}, "[string] List of rtcmfiles  files to use for corrections");
		trySetFromAny(pseudoobs_files,			commandOpts, station_data,	{"pseudoobs_files"				}, "[string] List of sp3        files to use for pseudoobservations");
		trySetFromAny(stream_user,				commandOpts, station_data,	{"stream_user"					}, "(string) Username for connecting to NTRIP casters");
		trySetFromAny(stream_pass,				commandOpts, station_data,	{"stream_pass"					}, "(string) Password for connecting to NTRIP casters");
		
		trySetFromYaml(stream_root,							 station_data,	{"stream_root"					}, "(string) Root path to be added to all other streams");
		
		tryAddRootToPath(root_stations_dir, rnxfiles);
		tryAddRootToPath(root_stations_dir, obs_rtcmfiles);
		tryAddRootToPath(root_stations_dir, nav_rtcmfiles);
		tryAddRootToPath(root_stations_dir, pseudoobs_files);

		globber(rnxfiles);
		globber(obs_rtcmfiles);
		globber(nav_rtcmfiles);
		globber(pseudoobs_files);
	}


	string root_output_dir = "./";
	auto output_files = stringsToYamlObject({yaml, ""}, 		{"output_files"});
	{
		trySetFromAny(trace_level, commandOpts,	output_files, {"trace_level"		}, "(int) Threshold level for printing messages (0-5)");
		trySetFromAny(fatal_level, commandOpts,	output_files, {"fatal_level"		}, "(int) Threshold level for exiting the program early (0-2)");

		trySetFromYaml(root_output_dir,			output_files, {"root_output_directory"			});

		trySetFromYaml(output_trace,			output_files, {"output_trace"				});
		trySetFromYaml(trace_directory,			output_files, {"trace_directory"			});
		trySetFromYaml(trace_filename,			output_files, {"trace_filename"				});

		trySetFromYaml(record_rtcm_nav,			output_files, {"record_rtcm_nav"			});
		trySetFromYaml(rtcm_nav_directory,		output_files, {"rtcm_nav_directory"	  		});
		trySetFromYaml(rtcm_nav_filename,		output_files, {"rtcm_nav_filename"			});

		trySetFromYaml(record_rtcm_obs,			output_files, {"record_rtcm_obs"			});
		trySetFromYaml(rtcm_obs_directory,		output_files, {"rtcm_obs_directory"	  		});
		trySetFromYaml(rtcm_obs_filename,		output_files, {"rtcm_obs_filename"			});

		trySetFromYaml(output_log,		     	output_files, {"output_log"					});
		trySetFromYaml(log_directory,			output_files, {"log_directory"	   	 		});
		trySetFromYaml(log_filename,			output_files, {"log_filename"				});

		trySetFromYaml(output_residual_chain,	output_files, {"output_residual_chain"		});
		trySetFromYaml(output_residuals,		output_files, {"output_residuals"			});
		trySetFromYaml(output_config,			output_files, {"output_config"				});

		trySetFromYaml(output_summary,			output_files, {"output_summary"				});
		trySetFromYaml(summary_directory,		output_files, {"summary_directory"			});
		trySetFromYaml(summary_filename,		output_files, {"summary_filename"			});

		trySetFromYaml(output_orbits,			output_files, {"output_orbits"				});
		trySetFromYaml(output_orbit_velocities,	output_files, {"output_orbit_velocities"	});
		trySetFromYaml(orbits_directory,		output_files, {"orbits_directory"			});
		trySetFromYaml(orbits_filename,			output_files, {"orbits_filename"			});
		trySetEnumOpt(orbits_data_source,		output_files, {"orbits_data_source" 		},E_Ephemeris::_from_string_nocase);

		trySetFromYaml(output_rinex_nav,		output_files, {"output_rinex_nav"			});
		trySetFromYaml(rinex_nav_directory,		output_files, {"rinex_nav_directory"		});
		trySetFromYaml(rinex_obs_filename,		output_files, {"rinex_nav_filename"			});

		trySetFromYaml(output_rinex_obs,		output_files, {"output_rinex_obs"			});
		trySetFromYaml(rinex_obs_directory,		output_files, {"rinex_obs_directory"		});
		trySetFromYaml(rinex_obs_print_C_code,	output_files, {"rinex_obs_print_C_code"		});
		trySetFromYaml(rinex_obs_print_L_code,	output_files, {"rinex_obs_print_L_code"		});
		trySetFromYaml(rinex_obs_print_D_code,	output_files, {"rinex_obs_print_D_code"		});
		trySetFromYaml(rinex_obs_print_S_code,	output_files, {"rinex_obs_print_S_code"		});
		trySetFromYaml(rinex_obs_filename,		output_files, {"rinex_obs_filename"			});

		trySetFromYaml(output_sys_combined,		output_files, { "output_sys_combined"		});

		trySetFromYaml(output_clocks,			output_files, {"output_clocks"		});
		trySetFromYaml(clocks_directory,		output_files, {"clocks_directory"	});
		trySetFromYaml(clocks_filename,			output_files, {"clocks_filename"	});
		trySetFromYaml(output_AR_clocks,		output_files, {"output_AR_clocks"	});
		trySetEnumOpt(clocks_receiver_source,	output_files, {"clocks_receiver_source" }, E_Ephemeris::_from_string_nocase);
		trySetEnumOpt(clocks_satellite_source,	output_files, {"clocks_satellite_source"}, E_Ephemeris::_from_string_nocase);

		trySetFromYaml(output_ppp_sol,			output_files, {"output_ppp_sol"		});
		trySetFromYaml(ppp_sol_directory,		output_files, {"ppp_sol_directory"	});
		trySetFromYaml(ppp_sol_filename,		output_files, {"ppp_sol_filename"	});

		trySetFromYaml(output_ionex,			output_files, {"output_ionex"		});
		trySetFromYaml(ionex_directory,			output_files, {"ionex_directory"	});
		trySetFromYaml(ionex_filename,			output_files, {"ionex_filename"		});

		trySetFromYaml(output_ionstec,			output_files, {"output_ionstec"		});
		trySetFromYaml(ionstec_directory,		output_files, {"ionstec_directory"	});
		trySetFromYaml(ionstec_filename,		output_files, {"ionstec_filename"	});

		trySetFromYaml(output_erp,				output_files, {"output_erp"		});
		trySetFromYaml(erp_directory,			output_files, {"erp_directory"	});
		trySetFromYaml(erp_filename,			output_files, {"erp_filename"	});

		trySetFromYaml(output_bias_sinex,		output_files, {"output_bias_sinex"		});
		trySetFromYaml(bias_sinex_directory,	output_files, {"bias_sinex_directory"	});
		trySetFromYaml(bias_sinex_filename,		output_files, {"bias_sinex_filename"	});

		trySetFromYaml(output_sinex,			output_files, {"output_sinex"			});
		trySetFromYaml(sinex_directory,			output_files, {"sinex_directory"		});
		trySetFromYaml(sinex_filename,			output_files, {"sinex_filename"			});

		trySetFromYaml(output_trop_sinex,		output_files, {"output_trop_sinex"		},										"(bool) Enable data exporting to troposphere SINEX file");
		trySetEnumOpt( trop_data_source,		output_files, {"trop_data_source" 		}, E_Ephemeris::_from_string_nocase,	"(enum) Source for troposphere delay data - KALMAN, etc.");
		trySetFromYaml(trop_sinex_directory,	output_files, {"trop_sinex_directory"	},										"(string) Directory to export troposphere SINEX file");
		trySetFromYaml(trop_sinex_filename,		output_files, {"trop_sinex_filename"	},										"(string) Troposphere SINEX filename");

		trySetFromYaml(output_decoded_rtcm_json,	output_files, {"output_decoded_rtcm_json"		},	"(bool) Enable exporting decoded RTCM data to file");
		trySetFromYaml(decoded_rtcm_json_directory,	output_files, {"decoded_rtcm_json_directory"	},	"(string) Directory to export decoded RTCM data");
		trySetFromYaml(decoded_rtcm_json_filename,	output_files, {"decoded_rtcm_json_filename"		},	"(string) Decoded RTCM data filename");

		trySetFromYaml(output_encoded_rtcm_json,	output_files, {"output_encoded_rtcm_json"		},	"(bool) Enable exporting encoded RTCM data to file");
		trySetFromYaml(encoded_rtcm_json_directory,	output_files, {"encoded_rtcm_json_directory"	},	"(string) Directory to export encoded RTCM data");
		trySetFromYaml(encoded_rtcm_json_filename,	output_files, {"encoded_rtcm_json_filename"		},	"(string) Encoded RTCM data filename");

		trySetFromYaml(output_network_statistics_json,		output_files, {"output_network_statistics_json"		},	"(bool) Enable exporting network statistics data to file");
		trySetFromYaml(network_statistics_json_directory,	output_files, {"network_statistics_json_directory"	},	"(string) Directory to export network statistics data");
		trySetFromYaml(network_statistics_json_filename,	output_files, {"network_statistics_json_filename"	},	"(string) Network statistics data filename");

		trySetFromYaml(output_persistance,			output_files, {"output_persistance"		});
		trySetFromYaml(input_persistance,			output_files, {"input_persistance"		});
		trySetFromYaml(persistance_directory,		output_files, {"persistance_directory"	});
		trySetFromYaml(persistance_filename,		output_files, {"persistance_filename"	});

		trySetFromYaml(enable_mongo,				output_files, {"enable_mongo"				}, "(bool)   Enable and connect to mongo database");
		trySetFromYaml(output_mongo_rtcm_messages,	output_files, {"output_mongo_rtcm_messages"	}, "(bool)   Output rtcm data to mongo");
		trySetFromYaml(output_mongo_measurements,	output_files, {"output_mongo_measurements"	}, "(bool)   Output measurements and their residuals");
		trySetFromYaml(output_mongo_states,			output_files, {"output_mongo_states"		}, "(bool)   Output states");
		trySetFromYaml(output_mongo_test_stats,		output_files, {"output_mongo_test_stats"	}, "(bool)   Output test statistics");
		trySetFromYaml(output_intermediate_rts,		output_files, {"output_intermediate_rts"	}, "(bool)   Output best available smoothed states when performing fixed-lag rts (slow, use only when needed)");
		trySetFromYaml(output_mongo_metadata,		output_files, {"output_mongo_metadata"		}, "(bool)   Output metadata from run");
		trySetFromYaml(output_mongo_logs,			output_files, {"output_mongo_logs"			}, "(bool)   Output console trace and warnings to mongo with timestamps and other metadata");
		trySetFromYaml(delete_mongo_history,		output_files, {"delete_mongo_history"		}, "(bool)   Drop the collection in the database at the beginning of the run to only show fresh data");
		trySetFromYaml(mongo_rts_suffix,			output_files, {"mongo_rts_suffix"			}, "(string) Suffix to append to database elements for reverse smoothed elements");
		trySetFromYaml(mongo_suffix,				output_files, {"mongo_suffix"				}, "(string) Suffix to append to database elements to make distinctions between runs for comparison");
		trySetFromYaml(mongo_database,				output_files, {"mongo_database"				}, "(string) ");
		trySetFromYaml(mongo_uri,					output_files, {"mongo_uri"					}, "(string) Location and port of the mongo database to connect to");

		trySetScaledFromYaml(trace_rotate_period,		output_files, {"trace_rotate_period"	},	{"trace_rotate_period_units"	},	E_Period::_from_string_nocase);
		trySetScaledFromYaml(rtcm_rotate_period,		output_files, {"rtcm_rotate_period"		},	{"rtcm_rotate_period_units"		},	E_Period::_from_string_nocase);
	}


	auto processing_options = stringsToYamlObject({ yaml, "" }, {"processing_options"});
	{
		trySetFromAny(max_epochs,			commandOpts, processing_options, {"max_epochs"			}, "(int)   Maximum number of epochs to process");
		trySetFromAny(epoch_interval,		commandOpts, processing_options, {"epoch_interval"		}, "(float) Desired time step between each processing epoch");
		trySetFromAny(simulate_real_time,	commandOpts, processing_options, {"simulate_real_time"	}, "(bool)  For RTCM playback - delay processing to match original data rate");

		string startStr;
		string stopStr;
		trySetFromAny(startStr,			commandOpts, processing_options, {"start_epoch"		}, "(date) The time of the first epoch to process (all observations before this will be skipped)");
		trySetFromAny(stopStr,			commandOpts, processing_options, {"end_epoch"		}, "(date) The time of the last epoch to process (all observations after this will be skipped)");
		if (!startStr.empty())	start_epoch	= boost::posix_time::time_from_string(startStr);
		if (!stopStr .empty())	end_epoch	= boost::posix_time::time_from_string(stopStr);

		trySetFromYaml(process_minimum_constraints,	processing_options, {"process_modes","minimum_constraints"	}, "(bool) Transform states by minimal constraints to selected station coordinates");
		trySetFromYaml(process_network,				processing_options, {"process_modes","network"				}, "(bool) Compute PPP corrections for a network of stations and satellites");
		trySetFromYaml(process_preprocessor,		processing_options, {"process_modes","preprocessor"			}, "(bool) Perform preprocessing for quality checks");
		trySetFromYaml(process_user,				processing_options, {"process_modes","user"					}, "(bool) Compute PPP for separate stations using provided satellite data");
		trySetFromYaml(process_ionosphere,			processing_options, {"process_modes","ionosphere"			}, "(bool) Compute ionosphere maps from a network of stations");
		trySetFromYaml(process_rts,					processing_options, {"process_modes","rts"					}, "(bool) Perform backward smoothing of states to improve precision of earlier states");
		trySetFromYaml(process_tests,				processing_options, {"process_modes","unit_tests"			}, "(bool) Perform unit tests while processing");
		trySetFromYaml(process_ppp,					processing_options, {"process_modes","ppp"					}, "(bool) Perform PPP on a network of stations and satellites");

		for (int i = 0; i < E_Sys::_size(); i++)
		{
			E_Sys	sys			= E_Sys::_values()[i];
			string	sysName		= E_Sys::_names() [i];		boost::algorithm::to_lower(sysName);

			trySetFromYaml(process_sys[sys],	processing_options, {"process_sys", sysName	});
			trySetFromYaml(solve_amb_for[sys],	processing_options, {"solve_amb_for", sysName	});
		}
		
		for (int i = 0; i < NUM_FTYPES; i++)
		{
			string	freq = "l" + std::to_string(i);

			trySetFromYaml(process_freq[i],		processing_options, {"process_freq", freq});
		}

		bool found = trySetFromAny(elevation_mask,	commandOpts, processing_options, {"elevation_mask"	});
		if (found)
			elevation_mask *= D2R;

		trySetEnumOpt( ppp_ephemeris, 		processing_options,	{"ppp_ephemeris" 					}, E_Ephemeris::_from_string_nocase);

		trySetFromYaml(tide_solid,			processing_options, {"tide_solid"						});
		trySetFromYaml(tide_otl,			processing_options, {"tide_otl"							});
		trySetFromYaml(tide_pole,			processing_options, {"tide_pole"						});

		trySetFromYaml(phase_windup,		processing_options, {"phase_windup"						}, "(bool) Model the effects of rotating satellites on phase");
		trySetFromYaml(reject_eclipse,		processing_options, {"reject_eclipse"					}, "(bool) Exclude satellites that are in eclipsing season");
		trySetFromYaml(raim,				processing_options, {"raim"								});
		
		trySetFromYaml(thres_slip,			processing_options, {"cycle_slip",		"thres_slip"	});
		trySetFromYaml(mw_proc_noise,		processing_options, {"cycle_slip",		"mw_proc_noise"	});
		trySetFromYaml(excludeSlip.LLI,		processing_options, {"cycle_slip",		"exclude_lli"	}, "(bool) Exclude measurements that fail LLI slip test in preprocessor");            
		trySetFromYaml(excludeSlip.GF,		processing_options, {"cycle_slip",		"exclude_gf"	}, "(bool) Exclude measurements that fail GF  slip test in preprocessor");            
		trySetFromYaml(excludeSlip.MW,		processing_options, {"cycle_slip",		"exclude_mw"	}, "(bool) Exclude measurements that fail MW  slip test in preprocessor");            
		trySetFromYaml(excludeSlip.EMW,		processing_options, {"cycle_slip",		"exclude_emw"	}, "(bool) Exclude measurements that fail EMW slip test in preprocessor");            
		trySetFromYaml(excludeSlip.CJ,		processing_options, {"cycle_slip",		"exclude_cj"	}, "(bool) Exclude measurements that fail clk jump test in preprocessor");            
		trySetFromYaml(excludeSlip.SCDIA,	processing_options, {"cycle_slip",		"exclude_scdia"	}, "(bool) Exclude measurements that fail SCDIA    test in preprocessor");   
		
		trySetFromYaml(max_inno,				processing_options, {"max_inno"						});
		trySetFromYaml(deweight_factor,			processing_options, {"deweight_factor"				}, "(float) Factor to downweight the variance of measurements with statistically detected errors");
		trySetFromYaml(ratio_limit,				processing_options, {"ratio_limit"					});
		trySetFromYaml(max_gdop,				processing_options, {"max_gdop"						});
		trySetFromYaml(antexacs,				processing_options, {"antexacs"						});
		trySetFromYaml(sat_pcv,					processing_options, {"sat_pcv"						});
		trySetFromYaml(reinit_on_all_slips,		processing_options, {"reinit_on_all_slips"			}, "(bool) Any detected slips cause removal and reinitialisation of ambiguities");
		trySetFromYaml(reinit_on_clock_error,	processing_options, {"reinit_on_clock_error"		}, "(bool) Any clock 'state' errors cause removal and reinitialisation of the clocks and all associated ambiguities");
		trySetFromYaml(deweight_on_state_error,	processing_options, {"deweight_on_state_error"		}, "(bool) Any 'state' errors cause deweighting of all measurements that reference the state");

		trySetFromYaml(pivot_station,		processing_options, {"pivot_station"	});

		wait_next_epoch = epoch_interval;
		trySetFromYaml(wait_next_epoch,		processing_options, {"wait_next_epoch"	}, "(float) Time to wait for next epoch's data before skipping the epoch (will default to epoch_interval as an appropriate minimum value for realtime)");		
		trySetFromYaml(wait_all_stations,	processing_options, {"wait_all_stations"}, "(float) Time to wait from the reception of the first data of an epoch before skipping stations with data still unreceived");
		trySetFromYaml(require_obs,			processing_options, {"require_obs"		}, "(bool) Exit the program if no observation sources are available");

		trySetFromYaml(check_plumbing,		processing_options, {"check_plumbing"	}, "(bool) Debugging option to show sizes of objects in memory to detect leaks");
		
		trySetFromYaml(delete_old_ephemerides,						processing_options, {"delete_old_ephemerides"});
		  
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
		trySetFromYaml(validity_interval_factor,	processing_options, {"validity_interval_factor"					});
	}
	
	auto force_models = stringsToYamlObject({ yaml, "" }, {"processing_options", "force_models"});
	{
		trySetFromYaml(forceModels.earth_gravity,						force_models, {"earth_gravity"				});  
		trySetFromYaml(forceModels.solid_earth_tides,					force_models, {"solid_earth_tides"			});   		
		trySetFromYaml(forceModels.ocean_tide_loading,					force_models, {"ocean_tide_loading"			});   		 		
		trySetFromYaml(forceModels.relativity_effect,					force_models, {"relativity_effect"			});   		
		trySetFromYaml(forceModels.solar_radiation_pressure,			force_models, {"solar_radiation_pressure"	});   		
		trySetFromYaml(forceModels.thermal_emission,					force_models, {"thermal_emission"			});   		
		trySetFromYaml(forceModels.earth_albedo,						force_models, {"earth_albedo"				});   		
		trySetFromYaml(forceModels.infrared_radiation,					force_models, {"infrared_radiation"			});   		
		trySetFromYaml(forceModels.antenna_thrust,						force_models, {"antenna_thrust"				});
		trySetFromYaml(forceModels.satellite_manoeuvre,					force_models, {"satellite_manoeuvre"		});   		
		trySetFromYaml(forceModels.empirical_acceleration,				force_models, {"empirical_acceleration"		});   		
		trySetFromYaml(forceModels.sat_mass,							force_models, {"satMass"					});   	
		trySetFromYaml(forceModels.srp_area,							force_models, {"srpArea"					});   		
		trySetFromYaml(forceModels.srp_coef,							force_models, {"srpCoef"					});   		
		trySetFromYaml(forceModels.egmAccDeg,							force_models, {"egmAccDeg"					});   		
		trySetFromYaml(forceModels.egmAccOrd,							force_models, {"egmAccOrd"					});   		
		trySetFromYaml(forceModels.egmSTMDeg,							force_models, {"egmSTMDeg"					});   		
		trySetFromYaml(forceModels.egmSTMOrd,							force_models, {"egmSTMOrd"					});
		trySetEnumOpt( forceModels.gravity_model,						force_models, {"gravity_model"				},	E_GravMdl::_from_string_nocase);   						
		trySetEnumOpt( forceModels.srp_model,							force_models, {"srp_model"					},	E_SRPModels::_from_string_nocase);   		
		trySetEnumOpt( forceModels.ode_integrator, 						force_models, {"ode_integrator" 			},	E_Integrator::_from_string_nocase);
		
		for (int i = 0; i < E_ThirdBody::_size(); i++)
		{
			E_ThirdBody	body			= E_ThirdBody::_values()[i];
			string		bodyName		= E_ThirdBody::_names() [i];		boost::algorithm::to_lower(bodyName);

			trySetFromYaml(forceModels.process_third_body[body],		force_models, {"process_third_body", bodyName	});
		}
	}

	auto user_filter = stringsToYamlObject({yaml, ""}, {"user_filter_parameters"});
	{
		trySetEnumOpt( pppOpts.inverter, 			user_filter,	{"inverter" 				}, E_Inverter::_from_string_nocase);
		trySetFromYaml(pppOpts.max_filter_iter,		user_filter,	{"max_filter_iterations"	}, "(int) Maximum number of times to attempt a full filter process, allows post-fit rejections for > 1");
		trySetFromYaml(pppOpts.rts_lag,				user_filter,	{"rts_lag"					}, "(int) Number of epochs to smooth backwards during RTS (-ve -> full reverse smoothing)");
		trySetFromYaml(pppOpts.rts_directory,		user_filter,	{"rts_directory"			});
		trySetFromYaml(pppOpts.rts_filename,		user_filter,	{"rts_filename"				});
		trySetFromYaml(pppOpts.outage_reset_limit,	user_filter,	{"outage_reset_limit"		});
		trySetFromYaml(pppOpts.phase_reject_limit,	user_filter,	{"phase_reject_limit"		});
		trySetFromYaml(pppOpts.sigma_check,			user_filter,	{"outlier_screening", "sigma_check"			},										"(bool)  Enable prefit and postfit sigma check");
		trySetFromYaml(pppOpts.w_test,				user_filter,	{"outlier_screening", "w_test"				},										"(bool)  Enable w-test");
		trySetFromYaml(pppOpts.chi_square_test,		user_filter,	{"outlier_screening", "chi_square_test"		},										"(bool)  Enable Chi-square test");
		trySetEnumOpt( pppOpts.chi_square_mode,		user_filter,	{"outlier_screening", "chi_square_mode"		}, E_ChiSqMode::_from_string_nocase,	"(enum)  Chi-square test mode - innovation, measurement, state");
		trySetFromYaml(pppOpts.sigma_threshold,		user_filter,	{"outlier_screening", "sigma_threshold"		},										"(float) sigma threshold");
		trySetFromYaml(pppOpts.max_prefit_remv,		user_filter,	{/*"outlier_screening", */"max_prefit_removals"	}, 										"(int) Maximum number of measurements to exclude using prefit checks before attempting to filter");
	}

	auto network_filter = stringsToYamlObject({yaml, ""}, {"network_filter_parameters"});
	{
		trySetEnumOpt( netwOpts.inverter, 			network_filter,	{"inverter" 				}, E_Inverter::_from_string_nocase);
		trySetFromYaml(netwOpts.max_filter_iter,	network_filter, {"max_filter_iterations"	});
		trySetFromYaml(netwOpts.rts_lag,			network_filter,	{"rts_lag"					});
		trySetFromYaml(netwOpts.rts_directory,		network_filter,	{"rts_directory"			});
		trySetFromYaml(netwOpts.rts_filename,		network_filter,	{"rts_filename"				});
		trySetFromYaml(netwOpts.outage_reset_limit,	network_filter,	{"outage_reset_limit"		});
		trySetFromYaml(netwOpts.phase_reject_limit,	network_filter,	{"phase_reject_limit"		});
		trySetFromYaml(netwOpts.sigma_check,		network_filter,	{"outlier_screening", "sigma_check"			},										"(bool)  Enable prefit and postfit sigma check");
		trySetFromYaml(netwOpts.w_test,				network_filter,	{"outlier_screening", "w_test"				},										"(bool)  Enable w-test");
		trySetFromYaml(netwOpts.chi_square_test,	network_filter,	{"outlier_screening", "chi_square_test"		},										"(bool)  Enable Chi-square test");
		trySetEnumOpt( netwOpts.chi_square_mode,	network_filter,	{"outlier_screening", "chi_square_mode"		}, E_ChiSqMode::_from_string_nocase,	"(enum)  Chi-square test mode - innovation, measurement, state");
		trySetFromYaml(netwOpts.sigma_threshold,	network_filter,	{"outlier_screening", "sigma_threshold"		},										"(float) sigma threshold");
		trySetFromYaml(netwOpts.max_prefit_remv,	network_filter, {/*"outlier_screening", */"max_prefit_removals"	}, 										"(int) Maximum number of measurements to exclude using prefit checks before attempting to filter");
		trySetFromYaml(netwOpts.chunk_size,			network_filter,	{"chunk_size"				});
		trySetFromYaml(netwOpts.chunk_stations,		network_filter,	{"chunk_stations"			});
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
		trySetEnumOpt( ionoOpts.corr_mode, 			ionosphere, {"corr_mode" 		}, E_IonoMode::_from_string_nocase);
		trySetEnumOpt( ionoOpts.iflc_freqs,			ionosphere, {"iflc_freqs" 		}, E_LinearCombo::_from_string_nocase);
		trySetFromYaml(ionoOpts.common_ionosphere,	ionosphere, {"common_ionosphere"});
		trySetFromYaml(ionoOpts.Auto_select_def_code, ionosphere, {"automatic_def_codes"}, "(bool) Automatically detect/select default GNSS codes to estimate the Ionosphere");
	}

	auto unit_tests = stringsToYamlObject({yaml, ""}, {"unit_test_options"});
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

	auto ionFilter = stringsToYamlObject({yaml, ""}, {"ionosphere_filter_parameters"});
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

	auto ambres_options = stringsToYamlObject({yaml, ""}, {"ambiguity_resolution_options"});
	{
		trySetFromYaml(ambrOpts.min_el_AR		  ,	ambres_options, {"Min_elev_for_AR"			});
		trySetFromYaml(ambrOpts.lambda_set	      ,	ambres_options, {"Set_size_for_lambda"		});
		trySetFromYaml(ambrOpts.AR_max_itr	      ,	ambres_options, {"Max_round_iterat"			});
		trySetFromYaml(ambrOpts.Max_Hold_epoc     ,	ambres_options, {"Max_amb_hold_epoc"		}, "Maximun number of epocs to hold ambiguities");
		trySetFromYaml(ambrOpts.Max_Hold_time	  ,	ambres_options, {"Max_amb_hold_time"		}, "Maximun amount of time (sec) to hold ambiguities");
		
		// trySetFromYaml(ambrOpts.solvGPS			  ,	ambres_options, {"GPS_amb_resol"			});
		// trySetFromYaml(ambrOpts.solvGLO			  ,	ambres_options, {"GLO_amb_resol"			});
		// trySetFromYaml(ambrOpts.solvGAL			  ,	ambres_options, {"GAL_amb_resol"			});
		// trySetFromYaml(ambrOpts.solvBDS			  ,	ambres_options, {"BDS_amb_resol"			});
		// trySetFromYaml(ambrOpts.solvQZS			  ,	ambres_options, {"QZS_amb_resol"			});

		trySetEnumOpt( ambrOpts.WLmode			  , ambres_options,	{"WL_mode" 					}, E_ARmode::_from_string_nocase);
		trySetFromYaml(ambrOpts.WLsuccsThres	  ,	ambres_options, {"WL_succ_rate_thres"		});
		trySetFromYaml(ambrOpts.WLratioThres	  ,	ambres_options,	{"WL_sol_ratio_thres"		});
		trySetFromYaml(ambrOpts.WLSatPrcNois	  ,	ambres_options,	{"WL_procs_noise_sat"		}, "WL bias process noise for satellite");
		trySetFromYaml(ambrOpts.WLRecPrcNois	  ,	ambres_options,	{"WL_procs_noise_sta"		}, "WL bias process noise for receivers");
		trySetFromYaml(ambrOpts.WL_filter_iter	  ,	ambres_options, {"WL_filtr_max_itert"	});
		trySetFromYaml(ambrOpts.WL_prefit_remv    ,	ambres_options, {"WL_filtr_max_remov"	});
		
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

	auto minCon = stringsToYamlObject({yaml, ""}, {"minimum_constraints"});
	{
		trySetFromYaml(minCOpts.estimate_scale,			minCon, {"estimate", "scale"		}, "(bool) Estimate scale       component of transformation");
		trySetFromYaml(minCOpts.estimate_rotation,		minCon, {"estimate", "rotation"		}, "(bool) Estimate rotation    component of transformation");
		trySetFromYaml(minCOpts.estimate_translation,	minCon, {"estimate", "translation"	}, "(bool) Estimate translation component of transformation");
	}

	auto outputOptions = stringsToYamlObject({yaml, ""}, {"output_options"});
	{
		trySetFromYaml(config_description,	outputOptions, {"config_description"}, "(string) ID for this config, used to replace <CONFIG> tags in other options");
		trySetFromYaml(analysis_agency,		outputOptions, {"analysis_agency"	});
		trySetFromYaml(analysis_center,		outputOptions, {"analysis_center"	});
		trySetFromYaml(analysis_program,	outputOptions, {"analysis_program"	});
		trySetFromYaml(rinex_comment,		outputOptions, {"rinex_comment"		});
		trySetFromYaml(reference_system,	outputOptions, {"reference_system"	},	"(string) Terrestrial Reference System Code");
		trySetFromYaml(time_system,			outputOptions, {"time_system"		},	"(string) Time system - e.g. 'G', 'UTC'");
		trySetFromYaml(ocean_tide_load,		outputOptions, {"ocean_tide_load"	},	"(string) Ocean tide loading model applied");
		trySetFromYaml(atmosph_tide_load,	outputOptions, {"atmosph_tide_load"	},	"(string) Atmospheric tide loading model applied");
		trySetFromYaml(geoid_model,			outputOptions, {"geoid_model"		},	"(string) Geoid model name for undulation values");
		trySetFromYaml(grads_mapping_fn,	outputOptions, {"grads_mapping_fn"	},	"(string) Name of mapping function used for mapping horizontal troposphere gradients");
	}

	auto defaultFilter = stringsToYamlObject({yaml, ""}, {"default_filter_parameters"});
	{
		trySetKalmanFromYaml(netwOpts.eop,			defaultFilter, "eop"		);
		trySetKalmanFromYaml(netwOpts.eop_rates,	defaultFilter, "eop_rates"	);
	}

	//todo get layer heights and process noise

	auto troposhpere = stringsToYamlObject(processing_options, {"troposhpere"});
	{
		trySetEnumOpt( tropOpts.model, 			troposhpere,	{"model" 			}, E_TropModel::_from_string_nocase);
		trySetFromYaml(tropOpts.gpt2grid,		troposhpere,	{"gpt2grid"			});
		trySetFromYaml(tropOpts.vmf3dir,		troposhpere,	{"vmf3dir"			});
		trySetFromYaml(tropOpts.orography,		troposhpere,	{"orography"		});
	}
	
	auto ssr = stringsToYamlObject({yaml, ""}, {"ssr_corrections"});
	{
		trySetEnumOpt (ssrOpts.ephemeris_source, 		ssr, {"ephemeris_source" 	}, E_Ephemeris::_from_string_nocase);
		trySetFromYaml(ssrOpts.calculate_ssr,			ssr, {"calculate_ssr"			});
		trySetFromYaml(ssrOpts.prediction_interval,		ssr, {"prediction_interval"		});
		trySetFromYaml(ssrOpts.prediction_duration,		ssr, {"prediction_duration"		});		
		trySetFromYaml(ssrOpts.save_to_mongo,			ssr, {"save_to_mongo"			});
		trySetFromYaml(ssrOpts.rtcm_directory,			ssr, {"rtcm_directory"			});
	}
	
	tryAddRootToPath(root_output_dir, 				trace_directory);
	tryAddRootToPath(root_output_dir, 				log_directory);
	tryAddRootToPath(root_output_dir,				summary_directory);
	tryAddRootToPath(root_output_dir,				orbits_directory);	
	tryAddRootToPath(root_output_dir,				clocks_directory);
	tryAddRootToPath(root_output_dir, 				rtcm_nav_directory);
	tryAddRootToPath(root_output_dir, 				rtcm_obs_directory);
	tryAddRootToPath(root_output_dir,				rinex_obs_directory);
	tryAddRootToPath(root_output_dir,				rinex_nav_directory);
	tryAddRootToPath(root_output_dir,				ppp_sol_directory);
	tryAddRootToPath(root_output_dir,				ionex_directory);
	tryAddRootToPath(root_output_dir,				erp_directory);
	tryAddRootToPath(root_output_dir,				ionstec_directory);
	tryAddRootToPath(root_output_dir,				bias_sinex_directory);

	tryAddRootToPath(root_output_dir,				sinex_directory);
	tryAddRootToPath(root_output_dir,				trop_sinex_directory);
	tryAddRootToPath(root_output_dir,				decoded_rtcm_json_directory);
	tryAddRootToPath(root_output_dir,				encoded_rtcm_json_directory);
	tryAddRootToPath(root_output_dir,				network_statistics_json_directory);
	tryAddRootToPath(root_output_dir,				persistance_directory);
	tryAddRootToPath(root_output_dir,				ionFilterOpts.rts_directory);
	tryAddRootToPath(root_output_dir,				netwOpts.rts_directory);
	tryAddRootToPath(root_output_dir,				pppOpts.rts_directory);
	tryAddRootToPath(root_output_dir,				ssrOpts.rtcm_directory);

	tryAddRootToPath(trace_directory, 				trace_filename);
	tryAddRootToPath(rtcm_nav_directory, 			rtcm_nav_filename);
	tryAddRootToPath(rtcm_obs_directory, 			rtcm_obs_filename);
	tryAddRootToPath(log_directory,					log_filename);
	tryAddRootToPath(summary_directory,				summary_filename);
	tryAddRootToPath(clocks_directory,				clocks_filename);
	tryAddRootToPath(orbits_directory,				orbits_filename);
	tryAddRootToPath(rinex_obs_directory,			rinex_obs_filename);	
	tryAddRootToPath(rinex_nav_directory,			rinex_nav_filename);
	tryAddRootToPath(ppp_sol_directory,				ppp_sol_filename);
	tryAddRootToPath(ionex_directory,				ionex_filename);
	tryAddRootToPath(ionstec_directory,				ionstec_filename);
	tryAddRootToPath(erp_directory,					erp_filename);
	tryAddRootToPath(bias_sinex_directory,			bias_sinex_filename);


	tryAddRootToPath(sinex_directory,					sinex_filename);
	tryAddRootToPath(trop_sinex_directory,				trop_sinex_filename);
	tryAddRootToPath(decoded_rtcm_json_directory,		decoded_rtcm_json_filename);
	tryAddRootToPath(encoded_rtcm_json_directory,		encoded_rtcm_json_filename);
	tryAddRootToPath(network_statistics_json_directory,	network_statistics_json_filename);
	tryAddRootToPath(pppOpts.rts_directory,				pppOpts.rts_filename);
	tryAddRootToPath(persistance_directory,				persistance_filename);
	tryAddRootToPath(netwOpts.rts_directory,			netwOpts.rts_filename);
	tryAddRootToPath(ionFilterOpts.rts_directory,		ionFilterOpts.rts_filename);

	//Try to change all filenames to replace <YYYY> etc with other values.
	replaceTags(navfiles);
	replaceTags(orbfiles);
	replaceTags(sp3files);
	replaceTags(clkfiles);
	replaceTags(atxfiles);
	replaceTags(snxfiles);
	replaceTags(blqfiles);
	replaceTags(erpfiles);
	replaceTags(dcbfiles);
	replaceTags(bsxfiles);
	replaceTags(ionfiles);
	replaceTags(egmfiles);
	replaceTags(jplfiles);
	replaceTags(root_stations_dir);

	
	replaceTags(erp_directory);
	replaceTags(log_directory);
	replaceTags(trace_directory);
	replaceTags(ionex_directory);
	replaceTags(sinex_directory);
	replaceTags(orbits_directory);
	replaceTags(clocks_directory);
	replaceTags(ionstec_directory);
	replaceTags(ppp_sol_directory);
	replaceTags(summary_directory);
	replaceTags(rtcm_obs_directory);
	replaceTags(rtcm_nav_directory);
	replaceTags(rinex_obs_directory);
	replaceTags(rinex_nav_directory);
	replaceTags(trop_sinex_directory);
	replaceTags(bias_sinex_directory);
	replaceTags(persistance_directory);
	replaceTags(pppOpts.rts_directory);
	replaceTags(ssrOpts.rtcm_directory);
	replaceTags(netwOpts.rts_directory);
	replaceTags(ionFilterOpts.rts_directory);
	replaceTags(decoded_rtcm_json_directory);
	replaceTags(encoded_rtcm_json_directory);
	replaceTags(network_statistics_json_directory);

	replaceTags(erp_filename);
	replaceTags(stream_root);
	replaceTags(mongo_suffix);
	replaceTags(log_filename);
	replaceTags(trace_filename);
	replaceTags(ionex_filename);
	replaceTags(sinex_filename);
	replaceTags(mongo_database);
	replaceTags(orbits_filename);
	replaceTags(clocks_filename);
	replaceTags(ionstec_filename);
	replaceTags(ppp_sol_filename);
	replaceTags(summary_filename);
	replaceTags(bias_sinex_filename);
	replaceTags(trop_sinex_filename);
	replaceTags(persistance_filename);
	replaceTags(pppOpts.rts_filename);
	replaceTags(netwOpts.rts_filename);
	replaceTags(ionFilterOpts.rts_filename);
	replaceTags(decoded_rtcm_json_filename);
	replaceTags(encoded_rtcm_json_filename);
	replaceTags(network_statistics_json_filename);

	// The network stream time is in UTC to avoid problems with local time.
	replaceTags(rtcm_nav_filename);
	replaceTags(rtcm_obs_filename);
	replaceTags(rinex_obs_filename);
	replaceTags(rinex_nav_filename);
	
	replaceTimes(ionstec_filename,	acsConfig.start_epoch);	//todo aaron delete
	replaceTimes(ionex_filename,	acsConfig.start_epoch);
	
	for (auto& station_file : station_files)
	{
		replaceTags(station_file);
	}

	trySetFromYaml(print_stream_statistics,	{yaml, ""},{"station_data", "print_stats"});

	
	trySetEnumOpt( ssr_input_antenna_offset, {yaml, ""},	{"station_data","ssr_input_antenna_offset" }, E_OffsetType::_from_string_nocase);
	auto [obsStreamNode, obsStreamString] = stringsToYamlObject({yaml, ""},	{"station_data","obs_streams"});
	auto [navStreamNode, navStreamString] = stringsToYamlObject({yaml, ""},	{"station_data","nav_streams"});


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

			tryAddRootToPath(stream_root, fullUrl);
			if (fullUrl.front() == '/')
				fullUrl.erase(fullUrl.begin()); // in case of full urls given in station_data.streams

			while (fullUrl.back() == '/')
				fullUrl.pop_back();				// in case of terminating '/'

			std::size_t slashPos = fullUrl.find_last_of("/");	// find first 4 characters after last '/'
			string hostname = fullUrl.substr(0,slashPos+1);		// e.g. http://user:pass@auscors.ga.gov.au:2101/BCEP00BKG0 --> BCEP

			netwOpts.download_hosts.insert(hostname);
			
			if (nav == false)			netwOpts.obs_mount_url.insert(fullUrl);
			else						netwOpts.nav_mount_url.insert(fullUrl);
		}
	}





	string outputStreamRoot = "";
	trySetFromYaml(outputStreamRoot,	{yaml, ""}, {"output_streams", "stream_root"});
	
	replaceTags(outputStreamRoot);

	auto [outStreamNode, outStreamString] = stringsToYamlObject({yaml, ""}, {"output_streams", "stream_labels"});

	for (auto outLabelYaml : outStreamNode)
	{
		SsrBroadcast outStreamData;
		string outLabel = outLabelYaml.as<string>();
		auto outStreamsYaml = stringsToYamlObject({yaml, ""}, {"output_streams", outLabel});

		string fullUrl = "";
		trySetFromYaml(fullUrl, outStreamsYaml, {"stream"});
		tryAddRootToPath(outputStreamRoot, fullUrl);
		outStreamData.target_url = fullUrl;

		auto [messagesNode, messagesString] = stringsToYamlObject(outStreamsYaml, {"messages"});

		bool addOutStream = false;
		set<RtcmMessageType> upMessType;
		for (auto outMessage : messagesNode)
		{
			try
			{
				int messNum = outMessage.as<int>();
				outStreamData.rtcmMessagesTypes.insert(RtcmMessageType::_from_integral(messNum));
			}
			catch (std::exception& e)
			{
				BOOST_LOG_TRIVIAL(error) << "Error defining output stream message for label : " << outLabel;
				continue;
			}
		}
		

		trySetFromYaml(outStreamData.message_timeout,	outStreamsYaml, {"message_timeout"	});
		trySetFromYaml(outStreamData.itrf_datum, 		outStreamsYaml, {"itrf_datum"		});
		trySetFromYaml(outStreamData.provider_id, 		outStreamsYaml, {"provider_id"		});
		trySetFromYaml(outStreamData.solution_id, 		outStreamsYaml, {"solution_id"		});
		trySetFromYaml(outStreamData.update_interval, 	outStreamsYaml, {"update_interval"	});
		trySetFromYaml(outStreamData.master_iod, 		outStreamsYaml, {"master_iod"		});

		uploadingStreamData[outLabel] = outStreamData;
	}

#	ifndef ENABLE_MONGODB
	if	(enable_mongo)
	{
		std::cout << std::endl << "Error: Mongodb output requested by config but this is a non-Mongodb binary."	<< std::endl;
		std::cout << std::endl << "Mongodb can be enabled by building with $ cmake -D ENABLE_MONGODB=ON .."		<< std::endl;
		exit(1);
	}
#	endif

#	ifndef ENABLE_UNIT_TESTS
	if (process_tests)
	{
		std::cout << std::endl << "Error: Tests requested by config but this is a non-test binary."				<< std::endl;
		std::cout << std::endl << "Tests can be enabled by building with $ cmake -DENABLE_UNIT_TESTS=ON .."		<< std::endl;
		exit(1);
	}
#	endif
	
	if (commandOpts.count("yaml-defaults"))
	{
		outputDefaultConfigutation();
	}
	
	return true;
}


void dumpConfig(
	Trace& trace)
{
	trace << "+ RAW CONFIG" << std::endl;

	std::ifstream config(acsConfig.configFilename);

	string str;
	while (std::getline(config, str))
	{
		trace << str << std::endl;
	}

	trace << "- RAW CONFIG" << std::endl;
}
