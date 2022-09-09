
// #pragma GCC optimize ("O0")

#include <iostream>
#include <sstream>
#include <memory>
#include <string>
#include <tuple>
#include <map>

using std::stringstream;
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
		<< "Error: Invalid " << description << " file "
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

void tryPatchPaths(
	string&			rootDir,
	string&			fileDir,
	string&			fileName)
{
	tryAddRootToPath(rootDir, fileDir);
	tryAddRootToPath(fileDir, fileName);
	
	replaceTags(fileDir);
	replaceTags(fileName);
}

void dumpConfig(
	Trace& trace)
{
	trace << "+FILE/RAW_CONFIG" << std::endl;

	std::ifstream config(acsConfig.configFilename);

	string str;
	while (std::getline(config, str))
	{
		trace << str << std::endl;
	}

	trace << "-FILE/RAW_CONFIG" << std::endl;
}


string stringify(
	string value)
{
	return ((string) "\"") + value + "\"";
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


string nonNumericStack(
	string			stack,
	bool			colon = true)
{
	string token;
	
	stringstream ss(stack);
	string newStack;

	while (getline(ss, token, ':'))
	{
		size_t found = token.find_first_not_of("0123456789: ");
		if (found != std::string::npos)
		{
			token = token.substr(found);
			newStack += token;
			if (colon)
				newStack += ":";
		}
	}
	
	return newStack;
}

struct Indentor
{
	int indentation = 0;
	
	Indentor operator++(int)
	{
		Indentor old = *this;
		indentation += 4;
		return old;
	}
	
	Indentor& operator--()
	{
		indentation -= 4;
		return *this;
	}
	
	operator string() const 
	{
		return string(indentation, ' ');
	}
	
    friend std::ostream& operator<<(std::ostream& os, const Indentor& dt);
};

std::ostream& operator<<(std::ostream& os, const Indentor& indentor)
{
    os << string(indentor.indentation, ' ');
    return os;
}

/** Recursive function to output the default values of all siblings with a common root.
 */
template<typename TYPE>
void outputDefaultSiblings(
	std::ostream&	html,			///< Html file stream to output configurator to
	TYPE&			it,				///< Iterator over the default values map
	Indentor&		indentor,		///< Helper to maintain and output indentation for default yaml output
	Indentor&		htmlIndentor,	///< Helper to maintain and output indentation for internal html output
	string			root = "")		///< Common root to determine extent of siblings relationship
{
	//keep going until the end of the file, or this function returns due to the next iterator not being a sibling
	while (it != acsConfig.yamlDefaults.end())
	{
		auto& [itStack, dummy] = *it;
		
		// Check the name of this parameter against the root
		bool itIsSibling = (itStack.substr(0, root.length()) == root);
		
		// Exit this level of recursion once a non-sibling is found
		if (itIsSibling == false)
		{
			return;
		}
		
		//do this one, and bump the iterator
		{
			auto& [stack,		defaultVals]	= *it;
			auto& [defaultVal,	comment]		= defaultVals;
			
			it++;
			
			//check if it has children - if the iterator allows it
			bool nextIsChild = false;
			if (it != acsConfig.yamlDefaults.end())
			{
				auto& [nextStack, dummy] = *it;
				
				nextIsChild	= (nextStack.substr(0, stack.length()) == stack);
			}
			
			//split the name into tokens so that ordering numerals can be removed
			size_t pos_start = 0;
			size_t pos_end;
			
			string token;
			string flatStack = "";
			//find each part of the stack for this entry and make a list of them
			while ((pos_end = stack.find(":", pos_start)) != string::npos) 
			{
				token = stack.substr(pos_start, pos_end - pos_start);
				pos_start = pos_end + 1;
				token = nonNumericStack(token);
				flatStack += token;
			}
			
			//output the boilerplate of the name, and comment up to the point where the children are nested
			tracepdeex(0, std::cout, "\n%s%s\t%-30s", ((string)indentor).c_str(), token.c_str(), (defaultVal).c_str());
			
			html << std::endl <<	htmlIndentor++		<< "<div class='element'>";
			html << std::endl <<	htmlIndentor		<< "<input type='checkbox' id='" << flatStack << "'>";
			html << std::endl <<	htmlIndentor++		<< "<div class='ident' data-indent='" <<  indentor << "'>"
					<< (nextIsChild ? "<b>" : "") << token  
					<< (nextIsChild ? " ⯆</b>" : "");
			
			if (comment.empty() == false)
			{
				std::cout << "\t# " << comment;
				html << std::endl <<	htmlIndentor		<< "<span class='tooltiptext'># " << comment << "</span>";
			}
			
			html << std::endl << --	htmlIndentor		<< "</div>";
					
			
			//
			bool firstChild = false;
			if (nextIsChild)
			{
				if (firstChild == false)
				{
					//initiate the section for embedding children nodes
					firstChild = true;
					
					html << std::endl <<	htmlIndentor++	<< "<div class='contents'>";
				}
				
				// recurse to do children of this node
				indentor++;
				outputDefaultSiblings(html, it, indentor, htmlIndentor, stack);
				--indentor;
			}
			
			if (firstChild)
			{
				//finalise the child section
				html << std::endl << --	htmlIndentor		<< "</div>";
			}
			else
			{
				//this has no children, output the default value of this parameter instead - according to its commented parameter type
				
				for (auto once : {1})
				{
					//booleans
					if (comment.find("(bool)") != string::npos)
					{
						
						html << std::endl <<	htmlIndentor++	<< "<select class='value'>";
						html << std::endl <<	htmlIndentor	<< "<option value='true' "	<< (defaultVal == "1" ? " selected" : "") << ">true</option>";
						html << std::endl <<	htmlIndentor	<< "<option value='false' "	<< (defaultVal == "0" ? " selected" : "") << ">false</option>";
						html << std::endl <<	htmlIndentor	<< "<option value='1' hidden>true</option>";
						html << std::endl <<	htmlIndentor	<< "<option value='0' hidden>false</option>";
						html << std::endl << --	htmlIndentor	<< "</select>";
						break;
					}
					
					auto begin	= comment.find('{');
					auto end	= comment.find('}', begin);
					
					//enums
					if	( begin != string::npos
						&&end	!= string::npos)
					{
						string enums = comment.substr(begin + 1, end - begin - 1);
						size_t pos_start = 0;
						size_t pos_end;
					
						html << std::endl <<	htmlIndentor++	<< "<select class='value'>";
						
						//find each part of the stack for this entry and make a list of them
						while ((pos_end = enums.find(',', pos_start)) != string::npos) 
						{
							string token = enums.substr(pos_start, pos_end - pos_start);
							pos_start = pos_end + 1;
							html << std::endl <<	htmlIndentor	<< "<option value='" << token << "'>" << token << "</option>";
						}
						//get last one
						string token = enums.substr(pos_start);
						html << std::endl <<	htmlIndentor	<< "<option value='" << token << "'>" << token << "</option>";
						
						html << std::endl << --	htmlIndentor	<< "</select>";
						
						break;
					}
					
					//general parameters
					{
						html << std::endl << htmlIndentor << "<input type='text' class='value' value='" << defaultVal << "'>";
					}
					
				}
			}
		
			html << std::endl << --	htmlIndentor		<< "</div>";
		}
	}
}

/** Outputs default configuration and a configurator
 * The default values, and descriptions of each parameter configured is output to the command line.
 * A configurator is generated that can be used to edit the default configuration via interactive html scripts
 */
void ACSConfig::outputDefaultConfiguration()
{
	std::cout << std::endl << "Default configuration values:\n\n";
	
	std::ofstream html("GinanYamlInspector.html");

	html << 
	#include "htmlHeaderTemplate.html"
	<< std::endl;
	
	auto it = acsConfig.yamlDefaults.begin();
	
	Indentor indentor;
	Indentor htmlIndentor;
	
	outputDefaultSiblings(html, it, indentor, htmlIndentor);

	html << 
	#include "htmlFooterTemplate.html"
	<< std::endl;
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
	("help,h",																"Help")
	("quiet,q",																"Less output")
	("verbose,v",															"More output")
	("very-verbose,V",														"Much more output")
	("yaml-defaults,Y",														"Print complete set of parsed parameters and their default values, and generate configurator.html for visual editing of yaml files")
	("config,y",				boost::program_options::value<string>(),	"Configuration file")
	("config_description,d",	boost::program_options::value<string>(),	"Configuration description")
	("trace_level,l",			boost::program_options::value<int>(),		"Trace level")
	("fatal_message_level,L",	boost::program_options::value<int>(),		"Fatal error level")
	("elevation_mask,e",		boost::program_options::value<float>(), 	"Elevation Mask")
	("max_epochs,n",			boost::program_options::value<int>(),		"Maximum Epochs")
	("epoch_interval,i",		boost::program_options::value<float>(), 	"Epoch Interval")
	("user,u",					boost::program_options::value<string>(),	"Username for RTCM streams")
	("pass,p",					boost::program_options::value<string>(),	"Password for RTCM streams")
	("atx_files",				boost::program_options::value<string>(),	"ANTEX file")
	("nav_files",				boost::program_options::value<string>(),	"Navigation file")
	("snx_files",				boost::program_options::value<string>(),	"SINEX files")
	("sp3_files",				boost::program_options::value<string>(),	"Orbit (SP3) files")
	("clk_files",				boost::program_options::value<string>(),	"Clock (CLK) files")
	("dcb_files",				boost::program_options::value<string>(),	"Code Bias (DCB) files")
	("bsx_files",				boost::program_options::value<string>(),	"Bias Sinex (BSX) files")
	("ion_files",				boost::program_options::value<string>(),	"Ionosphere (IONEX) files")
	("pod_files",				boost::program_options::value<string>(),	"Orbits (POD) files")
	("blq_files",				boost::program_options::value<string>(),	"BLQ (Ocean loading) files")
	("erp_files",				boost::program_options::value<string>(),	"ERP files")
	("rnx_files",				boost::program_options::value<string>(),	"RINEX station files")
	("rtcm_files",				boost::program_options::value<string>(),	"RTCM station files")
	("egm_files",				boost::program_options::value<string>(),	"Earth gravity model coefficients file")
// 	("jpl_files",				boost::program_options::value<string>(),	"JPL planetary and lunar ephemerides file")
	("root_input_directory",	boost::program_options::value<string>(),	"Directory containg the input data")
	("root_output_directory",	boost::program_options::value<string>(),	"Output directory")
	("start_epoch",				boost::program_options::value<string>(),	"Start date/time")
	("end_epoch",				boost::program_options::value<string>(),	"Stop date/time")
	("run_rts_only",			boost::program_options::value<string>(),	"RTS filename (without _xxxxx suffix)")
	("dump-config-only",													"Dump the configuration and exit")
	("input_persistance",													"Begin with previously stored filter and navigation states")
	("output_persistance",													"Store filter and navigation states for restarting")
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
			<< "Error: Configuration from " << config << " aborted";

			return false;
		}
	}

	// Dump the configuration information
	acsConfig.info(std::cout);

	// Check the configuration
	bool valid = true;
	valid &= checkValidFiles(acsConfig.snx_files, 			"sinex file (snx file)");
	valid &= checkValidFiles(acsConfig.nav_files, 			"navfiles");
	valid &= checkValidFiles(acsConfig.sp3_files, 			"orbit");
	valid &= checkValidFiles(acsConfig.clk_files,			"clock file (CLK file)");
	valid &= checkValidFiles(acsConfig.blq_files, 			"ocean loading information (Blq file)");
	valid &= checkValidFiles(acsConfig.erp_files,			"earth rotation parameter file (ERP file)");
	valid &= checkValidFiles(acsConfig.dcb_files,			"code Biases file (DCB file)");
	valid &= checkValidFiles(acsConfig.bsx_files,			"bias Sinex file (BSX file)");
	valid &= checkValidFiles(acsConfig.ion_files,			"Ionosphere (IONEX file)");
	valid &= checkValidFiles(acsConfig.atx_files, 			"antenna information (ANTEX file)");
	valid &= checkValidFiles(acsConfig.orb_files, 			"orbit determination (pod file)");
	valid &= checkValidFiles(acsConfig.egm_files, 			"Earth gravity model coefficients (egm file)");
	valid &= checkValidFiles(acsConfig.jpl_files, 			"JPL planetary and lunar ephemerides (jpl file)");
	valid &= checkValidFile (acsConfig.model.trop.gpt2grid,	"grid");

	if	(acsConfig.snx_files.empty())
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Invalid SINEX file ";
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
	ss << "\tnav_files:  "; for (auto& a : nav_files)		ss << a << " "; ss << "\n";
	ss << "\tsnx_files:  "; for (auto& a : snx_files)		ss << a << " "; ss << "\n";
	ss << "\tatx_files:  "; for (auto& a : atx_files)		ss << a << " "; ss << "\n";
	ss << "\tdcb_files:  "; for (auto& a : dcb_files)		ss << a << " "; ss << "\n";
	ss << "\tclk_files:  "; for (auto& a : clk_files)		ss << a << " "; ss << "\n";
	ss << "\tbsx_files:  "; for (auto& a : bsx_files)		ss << a << " "; ss << "\n";
	ss << "\tion_files:  "; for (auto& a : ion_files)		ss << a << " "; ss << "\n";
	ss << "\tblq_files:  "; for (auto& a : blq_files)		ss << a << " "; ss << "\n";
	ss << "\terp_files:  "; for (auto& a : erp_files)		ss << a << " "; ss << "\n";
	ss << "\tsp3_files:  "; for (auto& a : sp3_files)		ss << a << " "; ss << "\n";
	ss << "\torb_files:  "; for (auto& a : orb_files)		ss << a << " "; ss << "\n";
	ss << "\tegm_files:  "; for (auto& a : egm_files)		ss << a << " "; ss << "\n";
// 	ss << "\tjpl_files:  "; for (auto& a : jpl_files)		ss << a << " "; ss << "\n";
	ss << "\trnx_files:  "; for (auto& a : rnx_files)		ss << a << " "; ss << "\n";
	ss << "\trtcm_files: "; for (auto& a : obs_rtcm_files)	ss << a << " "; ss << "\n";
	ss << "\tvmf3dir:    " << model.trop.vmf3dir 		<< "\n";
	ss << "\torography:  " << model.trop.orography 		<< "\n";
	ss << "\tgrid:       " << model.trop.gpt2grid 		<< "\n";
	ss << "\ttestfiles:  " << test_filename				<< "\n";
	ss << "\n";

	ss << "Outputs:\n";
	if (1)							{	ss << "\ttrace level:                   " << trace_level 					<< "\n"; }
	if (output_station_trace)		{	ss << "\tstation trace filename:        " << station_trace_filename 		<< "\n"; }
	if (output_network_trace)		{	ss << "\tnetwork trace filename:        " << network_trace_filename 		<< "\n"; }
	if (output_clocks)				{	ss << "\tclocks filename:               " << clocks_filename 				<< "\n"; }
	if (output_ionex)				{	ss << "\tionex filename:                " << ionex_filename 				<< "\n"; }
	if (output_ionstec)				{	ss << "\tionstec filename:              " << ionstec_filename 				<< "\n"; }
	if (output_bias_sinex)			{	ss << "\tbias sinex filename:           " << bias_sinex_filename			<< "\n"; }
	if (output_trop_sinex)			{	ss << "\ttrop sinex filename:           " << trop_sinex_filename			<< "\n"; }
	if (output_gpx)					{	ss << "\tgpx filename:                  " << gpx_filename					<< "\n"; }
	if (output_decoded_rtcm_json)	{	ss << "\tdecoded rtcm json filename:    " << decoded_rtcm_json_filename		<< "\n"; }
	if (output_encoded_rtcm_json)	{	ss << "\tencoded rtcm json filename:    " << encoded_rtcm_json_filename		<< "\n"; }

	ss << "\n";

	ss << "Process Modes:\n";
	ss << "\tPreprocessor:        " << process_preprocessor			<< "\n";
	ss << "\tUser:                " << process_user 				<< "\n";
	ss << "\tNetwork:             " << process_network 				<< "\n";
	ss << "\tMinimum Constraints: " << process_minimum_constraints 	<< "\n";
	ss << "\tIonospheric:         " << process_ionosphere 			<< "\n";
	ss << "\tRTS Smoothing:       " << process_rts 					<< "\n";
	ss << "\tPPP:                 " << process_ppp					<< "\n";
	ss << "\tOrbits:              " << process_orbits				<< "\n";
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
	if (epoch_interval	> 0)					{	ss << "\tepoch_interval: " << epoch_interval	<< "\n";	}
	if (max_epochs		> 0)					{	ss << "\tmax_epochs:     " << max_epochs		<< "\n";	}
	if (!start_epoch	.is_not_a_date_time())	{	ss << "\tepoch start:    " << start_epoch		<< "\n";	}
	if (!end_epoch		.is_not_a_date_time())	{	ss << "\tepoch end:      " << end_epoch			<< "\n";	}

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

/** Get an object within a hierarchy of yaml structure using a vector of nodes.
 * This will also set default values and comments for the final object in the hierarchy as required.
 * The descriptors may have numeric prefixes attached for ordering parameters in the default output,
 * these are removed before searching for them in the hierarchy
 */
NodeStack stringsToYamlObject(
	NodeStack		yamlBase, 						///< Yaml node to search within
	vector<string>	yamlNodeDescriptor,				///< List of strings of keys to trace hierarchy
	string			comment			= "",			///< Optional comment to append to default values output
	string			defaultValue	= "")			///< Optional default value
{
	YAML::Node currentNode;

	auto [node, stack] = yamlBase;
	currentNode.reset(node);
	
	//this function is fiddly - re-ordering or simplifying will likely lead to default configuration output issues
	
	for (int i = 0; i < yamlNodeDescriptor.size(); i++)
	{
		auto& desc = yamlNodeDescriptor[i];
		
		string shortDesc = nonNumericStack(desc, false);

		currentNode.reset(currentNode[shortDesc]);
		stack += desc + ":";
		
		if (acsConfig.yamlDefaults.find(stack) == acsConfig.yamlDefaults.end())
		{
			if (i == yamlNodeDescriptor.size() - 1)		acsConfig.yamlDefaults[stack] = {defaultValue, comment};
			else										acsConfig.yamlDefaults[stack] = {"", ""};
		}
	}
	
	return {currentNode, stack};
}



/** Set an output from yaml object if found
*/
template<typename TYPE>
bool trySetFromYaml(
	TYPE&			output,				///< Variable to output to
	NodeStack		yamlBase,			///< Yaml node to search within
	vector<string>	yamlNodeDescriptor,	///< List of strings of keys to trace hierarcy
	string			comment = "")		///< Description to provide to user for automatic documentation
{
	auto [optNode, stack] = stringsToYamlObject(yamlBase, yamlNodeDescriptor, comment, stringify(output));
	
	acsConfig.availableOptions[nonNumericStack(stack)] = true;
	
	try
	{
		output = optNode.template as<TYPE>();
		return true;
	}
	catch (...)
	{
		if (optNode.Scalar().empty() == false)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: Yaml entry '" << stack << "' was found but its value is incorrectly formatted";
		}
	}

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
	name = nonNumericStack(name, false);
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
	string enumOptions = " {";

	for (int i = 0; i < ENUM::_size(); i++)
	{
		string		enumOption		= ENUM::_names() [i];		boost::algorithm::to_lower(enumOption);
		if (i != 0)
			enumOptions += ",";
		enumOptions += enumOption;
	}
	enumOptions += "}";

	auto [optNode, stack] = stringsToYamlObject(yamlBase, yamlNodeDescriptor, comment + enumOptions, out._to_string());

	acsConfig.availableOptions[nonNumericStack(stack)] = true;
	
	string value;
	try
	{
		value = optNode.template as<string>();
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
	KalmanModel&	output,			///< Variable to output to
	NodeStack&		yaml,			///< Yaml node to search within
	string			key,			///< Key of yaml object
	string			comment = "")	///< Description to provide to user for automatic documentation
{	
	auto newYaml = stringsToYamlObject(yaml, {key}, comment);
	
	E_Period proc_noise_dt = E_Period::SECOND;
					trySetFromYaml(output.estimate, 		newYaml, {"0 estimated"		}, "[bools] Estimate state in kalman filter");
					trySetEnumOpt(proc_noise_dt,			newYaml, {"2 proc_noise_dt"	}, E_Period::_from_string_nocase, "(enum) Time unit for process noise - sqrt_sec, sqrt_day etc.");

					trySetFromYaml(output.sigma,			newYaml, {"1 sigma" 		}, "[floats] Apriori sigma values - if zero, will be initialised using least squares");
					trySetFromYaml(output.apriori_val,		newYaml, {"3 apriori_val"	}, "[floats] Apriori state values");
	bool found = 	trySetFromYaml(output.proc_noise,		newYaml, {"2 proc_noise" 	}, "[floats] Process noise sigmas");
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
			tau *= proc_noise_dt;
		}
	}
}

/** Set the variables associated with an output stream
*/
void tryGetStreamFromYaml(
	SsrBroadcast&	outStreamData,					///< Variable to output to
	NodeStack		yamlBase,						///< Yaml node to search within
	string			id)								///< Label associated with the stream
{
	auto outStreamsYaml = stringsToYamlObject(yamlBase, {id});

	trySetFromYaml(outStreamData.url, outStreamsYaml, {"0 url"}, "(string) Url of caster to send messages to");
	
	auto [messagesNode, messagesString] = stringsToYamlObject(outStreamsYaml, {"1 messages"},	"[int] List of message types to output for this stream");

	for (auto outMessage : messagesNode)
	{
		try
		{
			int messNum = outMessage.as<int>();
			outStreamData.rtcmMessagesTypes.insert(RtcmMessageType::_from_integral(messNum));
		}
		catch (std::exception& e)
		{
			BOOST_LOG_TRIVIAL(error) << "Error defining output stream message for label : " << id;
			continue;
		}
	}
	
	trySetFromYaml(outStreamData.update_interval, 	outStreamsYaml, {"2 update_interval"	});
	trySetFromYaml(outStreamData.message_timeout,	outStreamsYaml, {"message_timeout"		});
	trySetFromYaml(outStreamData.itrf_datum, 		outStreamsYaml, {"itrf_datum"			});
	trySetFromYaml(outStreamData.provider_id, 		outStreamsYaml, {"provider_id"			});
	trySetFromYaml(outStreamData.solution_id, 		outStreamsYaml, {"solution_id"			});
	trySetFromYaml(outStreamData.master_iod, 		outStreamsYaml, {"master_iod"			});
}

/** Set satellite options from yaml
*/
void getFromYaml(
	SatelliteOptions&	satOpts,			///< Satellite options variable to output to
	NodeStack			yamlBase,			///< Yaml node to search within
	vector<string>		yamlNodeDescriptor)	///< List of strings of keys of yaml hierarchy
{
	auto satNode = stringsToYamlObject(yamlBase, yamlNodeDescriptor);

	trySetKalmanFromYaml(satOpts.clk,					satNode, "clk",						"Clocks");
	trySetKalmanFromYaml(satOpts.clk_rate,				satNode, "clk_rate",				"Clock rates");
	trySetKalmanFromYaml(satOpts.clk_rate_gauss_markov,	satNode, "clk_rate_gauss_markov",	"Clock rates (gauss markov dynamics)");
	trySetKalmanFromYaml(satOpts.pos,					satNode, "pos",						"Position (experimental, use orb)");
	trySetKalmanFromYaml(satOpts.pos_rate,				satNode, "pos_rate",				"Velocity (experimental, use orb)");
	trySetKalmanFromYaml(satOpts.orb,					satNode, "orb",						"Orbit corrections");
	trySetKalmanFromYaml(satOpts.srp,					satNode, "srp",						"Solar radiation pressure (experimental, use orb)");
	trySetKalmanFromYaml(satOpts.pco,					satNode, "pco",						"Phase Center Offsets (experimental)");
	trySetKalmanFromYaml(satOpts.ant,					satNode, "ant",						"Antenna offsets (experimental)");
	trySetKalmanFromYaml(satOpts.code_bias,				satNode, "code_bias",				"Code bias (experimental)");
	trySetKalmanFromYaml(satOpts.phase_bias,			satNode, "phase_bias",				"Phase bias (experiemental)");

	trySetFromYaml(satOpts.exclude, 	satNode, {"0 exclude"});

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

	trySetKalmanFromYaml(recOpts.clk,						recNode, "clk",						"Clocks");
	trySetKalmanFromYaml(recOpts.clk_rate,					recNode, "clk_rate",				"Clock rates");
	trySetKalmanFromYaml(recOpts.clk_rate_gauss_markov,		recNode, "clk_rate_gauss_markov",	"Clock rates (gauss markov dynamics)");
	trySetKalmanFromYaml(recOpts.pos,						recNode, "pos",						"Position corrections");
	trySetKalmanFromYaml(recOpts.pos_rate,					recNode, "pos_rate",				"Velocity");
	trySetKalmanFromYaml(recOpts.amb,						recNode, "amb",						"Integer phase ambiguities");
	trySetKalmanFromYaml(recOpts.pco,						recNode, "pco",						"Phase Center Offsets (experimental)");
	trySetKalmanFromYaml(recOpts.ant,						recNode, "ant",						"Antenna offsets (experimental)");
	trySetKalmanFromYaml(recOpts.code_bias,					recNode, "code_bias",				"Code bias (experimental)");
	trySetKalmanFromYaml(recOpts.phase_bias,				recNode, "phase_bias",				"Phase bias (experiemental)");
	trySetKalmanFromYaml(recOpts.ion,						recNode, "ion",						"Ionosphere (experimental)");
	trySetKalmanFromYaml(recOpts.trop,						recNode, "trop",					"Troposphere corrections");
	trySetKalmanFromYaml(recOpts.trop_grads,				recNode, "trop_grads",				"Troposphere gradients");
	trySetKalmanFromYaml(recOpts.trop_gauss_markov,			recNode, "trop_gauss_markov",		"Troposphere corrections (gauss markov dynamics)");
	trySetKalmanFromYaml(recOpts.trop_grads_gauss_markov,	recNode, "trop_grads_gauss_markov",	"Troposphere gradients (gauss markov dynamics)");

	trySetFromYaml	(recOpts.exclude, 			recNode, {"1 exclude"		});
	trySetEnumOpt	(recOpts.error_model,		recNode, {"0 error_model"	}, E_NoiseModel::_from_string_nocase);
	trySetFromYaml	(recOpts.code_sigmas,		recNode, {"0 code_sigmas"	},					"[floats] Standard deviation of code measurements");
	trySetFromYaml	(recOpts.phas_sigmas,		recNode, {"0 phase_sigmas"	},					"[floats] Standard deviation of phase measurmeents");

	recOpts._initialised = true;
}

const string estimation_parameters_str	= "4 estimation_parameters";
const string processing_options_str		= "2 processing_options";
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
				getFromYaml(globalOpts, {yaml, ""}, {estimation_parameters_str, "0 satellites"});
				globalOpts._initialised = true;
			}

			if (Sat.sys == +E_Sys::NONE)
			{
				return globalOpts;
			}
			
			//initialise from its parent
			sysOpts = globalOpts;

			//get specifics from config file
			string sys = "SYS_" + Sat.sysName();
			getFromYaml(sysOpts, {yaml, ""}, {estimation_parameters_str, "overrides", "satellites", sys});
		}

		//initialise from its parent
		blockOpts = sysOpts;

		//get specifics from config file
		string block = Sat.blockType();
		getFromYaml(blockOpts, {yaml, ""}, {estimation_parameters_str, "overrides", "satellites", block});
	}

	//initialise from its parent
	satOpts = blockOpts;

	//get specifics from config file
	string prn = Sat.id();
	string svn = "SVN_" + Sat.svn();
	getFromYaml(satOpts, {yaml, ""}, {estimation_parameters_str, "overrides", "satellites", svn});
	getFromYaml(satOpts, {yaml, ""}, {estimation_parameters_str, "overrides", "satellites", prn});

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

	//initialise the options for this receiver
	auto& globalOpts = recOptsMap[""];
	if (globalOpts._initialised == false)
	{
		//get specifics from config file
		getFromYaml(globalOpts, {yaml, ""}, {estimation_parameters_str, "0 stations"});
		globalOpts._initialised = true;
	}

	//initialise from its parent
	recOpts = globalOpts;

	//get specifics from config file
	getFromYaml(recOpts, {yaml, ""}, {estimation_parameters_str, "overrides", "stations", id});

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

	//initialise the options for this receiver
	auto& globalOpts = minCOpts.stationMap[""];
	if (globalOpts._initialised == false)
	{
		//get specifics from config file
		trySetFromYaml(globalOpts.noise, {yaml, ""}, {processing_options_str, "minimum_constraints", "default_station_noise"}, "(float) Sigma applied to all stations for weighting in transformation estimation. (Lower is stronger weighting, Negative is unweighted)");
		globalOpts._initialised = true;
	}

	//initialise from its parent
	recOpts = globalOpts;

	//get specifics from config file
	trySetFromYaml(recOpts.noise, {yaml, ""}, {processing_options_str, "minimum_constraints", "station_noise", id}, "(float) Override sigma to adjust weighting of transformation estimation");

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
bool replaceString(
	string&	str,			///< String to search within
	string	subStr, 		///< String to replace
	string	replacement,	///< Replacement string
	bool	warn)			///< Optional wanring about undefined values
{
	bool replaced = false;
	while (true)
	{
		size_t index = str.find(subStr);
		if (index == -1)
			break;

		if	(  replacement.empty()
			&& warn)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: " << subStr << " is used in config but is not defined";
		}

		str.erase	(index, subStr.size());
		str.insert(index, replacement);
		
		replaced = true;
	}
	
	return replaced;
}

/** Replace macros for times with configured values.
* Available replacements are "<CONFIG> <USER> <PASS> <BRANCH> <HASH>"
*/
void replaceTags(
	string&						str)		///< String to replace macros within
{
	replaceString(str, "<CONFIG>",	acsConfig.config_description);
	replaceString(str, "<USER>",	acsConfig.stream_user);
	replaceString(str, "<PASS>",	acsConfig.stream_pass);
	replaceString(str, "<BRANCH>",	GINAN_BRANCH_NAME);
	replaceString(str, "<HASH>",	GINAN_COMMIT_HASH);
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
	
	bool replaced = false;

	replaced |= replaceString(str, "<LOGTIME>",	"<YYYY>-<MM>-<DD>_<HH>:<mm>",	false);
	replaced |= replaceString(str, "<DDD>",		DDD,							false);
	replaced |= replaceString(str, "<D>",		D,								false);
	replaced |= replaceString(str, "<WWWW>",	WWWW,							false);
	replaced |= replaceString(str, "<YYYY>",	YYYY,							false);
	replaced |= replaceString(str, "<YY>",		YY,								false);
	replaced |= replaceString(str, "<MM>",		MM,								false);
	replaced |= replaceString(str, "<DD>",		DD,								false);
	replaced |= replaceString(str, "<HH>",		HH,								false);
	replaced |= replaceString(str, "<hh>",		HH,								false);
	replaced |= replaceString(str, "<mm>",		mm,								false);
	
	if	(  YY.empty()
		&& replaced)
	{
		//replacing with nothing here may cause issues - kill the entire string to prevent damage
		str = "";
	}
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
				<< "Error: Invalid input directory "
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

void ACSConfig::recurseYaml(
	YAML::Node	node,
	string		stack)
{
	for (YAML::const_iterator it = node.begin(); it != node.end(); it++)
	{
		string key = it->first.as<string>();
		
// 		std::cout << key << std::endl;
		
		string newStack = stack + key + ":";
		
		if (newStack == "estimation_parameters:overrides:")							continue;
		if (newStack == "processing_options:minimum_constraints:station_noise:")	continue;
		if (newStack == "outputs:streams:")											continue;
		
		if (node[key].IsMap() == false)
		{
			if (availableOptions.find(newStack) != availableOptions.end())
			{
// 				BOOST_LOG_TRIVIAL(debug)
// 				<< newStack << " is a valid yaml option";
				
				continue;
			}
			
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: " << newStack << " is not a valid yaml option";
			
			continue;
		}
		
		recurseYaml(node[key], newStack);
	}
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
	defaultOutputOptions();

	for (int i = 1; i < E_Sys::SUPPORTED; i++)
	{
		E_Sys	sys			= E_Sys::_values()[i];
		
		code_priorities[sys] = code_priorities_default;
	}
	
	try
	{
		yaml.reset();
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
			BOOST_LOG_TRIVIAL(error) << "Error: \nFailed to parse configuration file " << filename;
			BOOST_LOG_TRIVIAL(error) << e.msg << std::endl;
			return false;	
		}
	}
	catch (const YAML::ParserException& e)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: \nFailed to parse configuration. Check for errors as described near the below:\n";
		BOOST_LOG_TRIVIAL(error) << e.what() << std::endl << std::endl;
		return false;	
	}

	string root_output_directory = "./";
	{
		auto outputs = stringsToYamlObject({yaml, ""}, {"1 outputs"}, "Configuration for types of files to output");
		
		trySetFromYaml(root_output_directory,			outputs, {"0 root_directory"			});
		
		{
			auto metadata = stringsToYamlObject(outputs, 		{"metadata"});
			
			trySetFromAny (config_description,	commandOpts,	metadata, {"config_description"				}, "(string) ID for this config, used to replace <CONFIG> tags in other options");
			trySetFromAny (stream_user,			commandOpts,	metadata, {"user"							}, "(string) Username for connecting to NTRIP casters");
			trySetFromAny (stream_pass,			commandOpts,	metadata, {"pass"							}, "(string) Password for connecting to NTRIP casters");
			trySetFromYaml(analysis_agency,						metadata, {"analysis_agency"				});
			trySetFromYaml(analysis_center,						metadata, {"analysis_center"				});
			trySetFromYaml(analysis_program,					metadata, {"analysis_program"				});
			trySetFromYaml(rinex_comment,						metadata, {"rinex_comment"					});
			trySetFromYaml(reference_system,					metadata, {"reference_system"				},	"(string) Terrestrial Reference System Code");
			trySetFromYaml(time_system,							metadata, {"time_system"					},	"(string) Time system - e.g. \"G\", \"UTC\"");
			trySetFromYaml(ocean_tide_loading_model,			metadata, {"ocean_tide_loading_model"		},	"(string) Ocean tide loading model applied");
			trySetFromYaml(atmospheric_tide_loading_model,		metadata, {"atmospheric_tide_loading_model"	},	"(string) Atmospheric tide loading model applied");
			trySetFromYaml(geoid_model,							metadata, {"geoid_model"					},	"(string) Geoid model name for undulation values");
			trySetFromYaml(gradient_mapping_function,			metadata, {"gradient_mapping_function"		},	"(string) Name of mapping function used for mapping horizontal troposphere gradients");
		}
		
		{
			auto trace = stringsToYamlObject(outputs, {"0 trace"});
                                                      
			trySetFromYaml(output_station_trace,	trace, {"0 output_stations"			}, "(bool) ");
			trySetFromYaml(output_network_trace,	trace, {"0 output_network"			}, "(bool) ");
			trySetFromYaml(trace_directory,			trace, {"directory"					});
			trySetFromYaml(station_trace_filename,	trace, {"1 station_filename"			});
			trySetFromYaml(network_trace_filename,	trace, {"1 network_filename"			});
			trySetFromAny(trace_level, commandOpts,	trace, {"level"						}, "(int) Threshold level for printing messages (0-5)");
														
			trySetFromYaml(output_residual_chain,	trace, {"output_residual_chain"		}, "(bool) ");
			trySetFromYaml(output_residuals,		trace, {"output_residuals"			}, "(bool) ");
			trySetFromYaml(output_config,			trace, {"output_config"				}, "(bool) ");
		}
		
		{
			auto output_rotation = stringsToYamlObject(outputs, {"output_rotation"});
			
			trySetScaledFromYaml(rotate_period,		output_rotation, {"period"	},	{"period_units"	},	E_Period::_from_string_nocase);
		}
		
		{
			auto bias_sinex = stringsToYamlObject(outputs, {"bias_sinex"});
														
			trySetFromYaml(output_bias_sinex,		bias_sinex, {"0 output"			}, "(bool) ");
			trySetFromYaml(bias_sinex_directory,	bias_sinex, {"directory"		});
			trySetFromYaml(bias_sinex_filename,		bias_sinex, {"filename"			});
			trySetFromYaml(ambrOpts.biasOutrate,	bias_sinex, {"output_interval"	});
		}
		
		{
			auto clocks = stringsToYamlObject(outputs, {"clocks"});
			
			trySetFromYaml(output_clocks,			clocks, {"0 output"			}, "(bool) ");
			trySetFromYaml(clocks_directory,		clocks, {"directory"		});
			trySetFromYaml(clocks_filename,			clocks, {"filename"			});
			trySetFromYaml(output_ar_clocks,		clocks, {"output_ar_clocks"	}, "(bool) ");
			trySetEnumOpt(clocks_receiver_source,	clocks, {"receiver_source"	}, E_Ephemeris::_from_string_nocase);
			trySetEnumOpt(clocks_satellite_source,	clocks, {"satellite_source"	}, E_Ephemeris::_from_string_nocase);
		}
		
		{
			auto decoded_rtcm = stringsToYamlObject(outputs, {"decoded_rtcm"});
			
			trySetFromYaml(output_decoded_rtcm_json,	decoded_rtcm, {"0 output"		},	"(bool) Enable exporting decoded RTCM data to file");
			trySetFromYaml(decoded_rtcm_json_directory,	decoded_rtcm, {"directory"		},	"(string) Directory to export decoded RTCM data");
			trySetFromYaml(decoded_rtcm_json_filename,	decoded_rtcm, {"filename"		},	"(string) Decoded RTCM data filename");
		
		}
		
		{
			auto encoded_rtcm = stringsToYamlObject(outputs, {"encoded_rtcm"});
			
			trySetFromYaml(output_encoded_rtcm_json,	encoded_rtcm, {"0 output"		},	"(bool) Enable exporting encoded RTCM data to file");
			trySetFromYaml(encoded_rtcm_json_directory,	encoded_rtcm, {"directory"		},	"(string) Directory to export encoded RTCM data");
			trySetFromYaml(encoded_rtcm_json_filename,	encoded_rtcm, {"filename"		},	"(string) Encoded RTCM data filename");
		}
		
		{
			auto erp = stringsToYamlObject(outputs, {"erp"});
                                                      
			trySetFromYaml(output_erp,				erp, {"0 output"		}, "(bool) ");
			trySetFromYaml(erp_directory,			erp, {"directory"		});
			trySetFromYaml(erp_filename,			erp, {"filename"		});
		}
		
		{
			auto ionex = stringsToYamlObject(outputs, {"ionex"});
														
			trySetFromYaml(output_ionex,			ionex, {"0 output"						}, "(bool) ");
			trySetFromYaml(ionex_directory,			ionex, {"directory"						});
			trySetFromYaml(ionex_filename,			ionex, {"filename"						});
			trySetFromYaml(ionexGrid.lat_center,	ionex, {"grid", "lat_center"			});
			trySetFromYaml(ionexGrid.lon_center,	ionex, {"grid", "lon_center"			});
			trySetFromYaml(ionexGrid.lat_width,		ionex, {"grid", "lat_width"				});
			trySetFromYaml(ionexGrid.lon_width,		ionex, {"grid", "lon_width"				});
			trySetFromYaml(ionexGrid.lat_res,		ionex, {"grid", "lat_resolution"		});
			trySetFromYaml(ionexGrid.lon_res,		ionex, {"grid", "lon_resolution"		});
			trySetFromYaml(ionexGrid.time_res,		ionex, {"grid", "time_resolution"		});
				
		}
		
		{
			auto ionstec = stringsToYamlObject(outputs, {"ionstec"});
                                                      
			trySetFromYaml(output_ionstec,			ionstec, {"0 output"		}, "(bool) ");
			trySetFromYaml(ionstec_directory,		ionstec, {"directory"		});
			trySetFromYaml(ionstec_filename,		ionstec, {"filename"		});
		}
		
		{
			auto sinex = stringsToYamlObject(outputs, {"sinex"});
                                                      
			trySetFromYaml(output_sinex,			sinex, {"0 output"		}, "(bool) ");
			trySetFromYaml(sinex_directory,			sinex, {"directory"		});
			trySetFromYaml(sinex_filename,			sinex, {"filename"		});
		}
		
		{
			auto log = stringsToYamlObject(outputs, {"log"});                      	//todo
                                                      
			trySetFromYaml(output_log,		     	log, {"0 output"			}, "(bool) ");
			trySetFromYaml(log_directory,			log, {"directory"			});
			trySetFromYaml(log_filename,			log, {"filename"			});
		}
		
		{
			auto gpx = stringsToYamlObject(outputs, {"gpx"});
                                                      
			trySetFromYaml(output_gpx,		     	gpx, {"0 output"			}, "(bool) ");
			trySetFromYaml(gpx_directory,			gpx, {"directory"			});
			trySetFromYaml(gpx_filename,			gpx, {"filename"			});
		}
		
		{
			auto network_statistics = stringsToYamlObject(outputs, {"network_statistics"});
			
			trySetFromYaml(output_network_statistics_json,		network_statistics, {"0 output"		},	"(bool) Enable exporting network statistics data to file");
			trySetFromYaml(network_statistics_json_directory,	network_statistics, {"directory"	},	"(string) Directory to export network statistics data");
			trySetFromYaml(network_statistics_json_filename,	network_statistics, {"filename"		},	"(string) Network statistics data filename");
		}
		
		{
			auto sp3 = stringsToYamlObject(outputs, {"sp3"});
                           //sp3                           
			trySetFromYaml(output_orbits,			sp3, {"0 output"			}, "(bool) ");
			trySetFromYaml(output_orbit_velocities,	sp3, {"output_velocities"	}, "(bool) ");
			trySetFromYaml(orbits_directory,		sp3, {"directory"			});
			trySetFromYaml(orbits_filename,			sp3, {"filename"			});
			trySetEnumOpt(orbits_data_source,		sp3, {"data_source" 		},E_Ephemeris::_from_string_nocase);
		}
		
		{
			auto ppp_sol = stringsToYamlObject(outputs, {"ppp_sol"});                                                      
                                                      
			trySetFromYaml(output_ppp_sol,			ppp_sol, {"0 output"	}, "(bool) ");
			trySetFromYaml(ppp_sol_directory,		ppp_sol, {"directory"	});
			trySetFromYaml(ppp_sol_filename,		ppp_sol, {"filename"	});
		}
		
		{
			auto rinex_nav = stringsToYamlObject(outputs, {"rinex_nav"});
                                                      
			trySetFromYaml(output_rinex_nav,		rinex_nav, {"0 output"		}, "(bool) ");
			trySetFromYaml(rinex_nav_directory,		rinex_nav, {"directory"		});
			trySetFromYaml(rinex_nav_filename,		rinex_nav, {"filename"		});
			trySetFromYaml(rinex_nav_version,		rinex_nav, {"version"		});
		}
		
		{
			auto rinex_obs = stringsToYamlObject(outputs, {"rinex_obs"});
                                                      
			trySetFromYaml(output_rinex_obs,		rinex_obs, {"0 output"					}, "(bool) ");
			trySetFromYaml(rinex_obs_directory,		rinex_obs, {"directory"					});
			trySetFromYaml(rinex_obs_print_C_code,	rinex_obs, {"output_pseudorange"		}, "(bool) ");
			trySetFromYaml(rinex_obs_print_L_code,	rinex_obs, {"output_phase_range"		}, "(bool) ");
			trySetFromYaml(rinex_obs_print_D_code,	rinex_obs, {"output_doppler"			}, "(bool) ");
			trySetFromYaml(rinex_obs_print_S_code,	rinex_obs, {"output_signal_to_noise"	}, "(bool) ");
			trySetFromYaml(rinex_obs_filename,		rinex_obs, {"filename"					});
			trySetFromYaml(rinex_obs_version,		rinex_obs, {"version"					});
		}
		
		{
			auto rtcm_nav = stringsToYamlObject(outputs, {"rtcm_nav"});
                                                      
			trySetFromYaml(record_rtcm_nav,			rtcm_nav, {"0 output"			}, "(bool) ");
			trySetFromYaml(rtcm_nav_directory,		rtcm_nav, {"directory"		  	});
			trySetFromYaml(rtcm_nav_filename,		rtcm_nav, {"filename"			});
		}
		
		{
			auto rtcm_obs = stringsToYamlObject(outputs, {"rtcm_obs"});
                                                      
			trySetFromYaml(record_rtcm_obs,			rtcm_obs, {"0 output"			}, "(bool) ");
			trySetFromYaml(rtcm_obs_directory,		rtcm_obs, {"directory"			});
			trySetFromYaml(rtcm_obs_filename,		rtcm_obs, {"filename"			});
		}
		
		
		{
			auto trop_sinex = stringsToYamlObject(outputs, {"trop_sinex"});
			
			trySetFromYaml(output_trop_sinex,		trop_sinex, {"0 output"		},										"(bool) Enable data exporting to troposphere SINEX file");
			trySetEnumOpt( trop_data_source,		trop_sinex, {"source" 		}, E_Ephemeris::_from_string_nocase,	"(enum) Source for troposphere delay data - KALMAN, etc.");
			trySetFromYaml(trop_sinex_directory,	trop_sinex, {"directory"	},										"(string) Directory to export troposphere SINEX file");
			trySetFromYaml(trop_sinex_filename,		trop_sinex, {"filename"		},										"(string) Troposphere SINEX filename");
		}
		
		{
			auto streams = stringsToYamlObject(outputs, 		{"streams"});
			
			string root_stream_url = "";
			trySetFromYaml(root_stream_url, streams, {"0 root_url"}, "(string) Root url to be prepended to all other streams specified in this section. If the streams used have individually specified root urls, usernames, or passwords, this should not be used.");
			
			replaceTags(root_stream_url);

			SsrBroadcast	dummyStreamData;
			tryGetStreamFromYaml(dummyStreamData, streams, {"XMPL"});
			
			auto [outStreamNode, outStreamString] = stringsToYamlObject(streams, {"1 labels"},		"[string] List of output stream is with further information to be found in its own section, as per XMPL below");
			
			for (auto outLabelYaml : outStreamNode)
			{
				SsrBroadcast outStreamData;
				string outLabel = outLabelYaml.as<string>();
				
				tryGetStreamFromYaml(outStreamData, streams, {outLabel});
				
				tryAddRootToPath(root_stream_url, outStreamData.url);
	
				uploadingStreamData[outLabel] = outStreamData;
			}
		}
	}
	
	string root_input_directory = "./";
	string ephemeris_data_root_url;
	string gnss_data_root_url;
	vector<string> gnssDataStreams;
	vector<string> ephemerisDataStreams;
	{
		auto inputs	= stringsToYamlObject({yaml, ""},	{"0 inputs"});
		
		string root_stream_url;
		trySetFromAny(root_input_directory,	commandOpts,	inputs, {"0 root_directory"		}, "(string) Root path to be added to all other input files (unless they are absolute)");
		trySetFromYaml(root_stream_url,						inputs,	{"0 root_stream_url"	}, "(string) Root url to be prepended to all other streams specified in this section. If the streams used have individually specified root urls, usernames, or passwords, this should not be used.");
		
		trySetFromAny(atx_files,			commandOpts,	inputs, {"atx_files"				}, "[string] List of atx files to use");			
		trySetFromAny(snx_files,			commandOpts,	inputs, {"snx_files"				}, "[string] List of snx files to use");			
		trySetFromAny(blq_files,			commandOpts,	inputs, {"blq_files"				}, "[string] List of blq files to use");			
		trySetFromAny(erp_files,			commandOpts,	inputs, {"erp_files"				}, "[string] List of erp files to use");			
		trySetFromAny(ion_files,			commandOpts,	inputs, {"ion_files"				}, "[string] List of ion files to use");			
		trySetFromAny(egm_files,			commandOpts,	inputs, {"egm_files"				}, "[string] List of egm files to use");			
// 		trySetFromAny(jpl_files,			commandOpts,	inputs, {"jpl_files"				}, "[string] List of jpl files to use");			
                                                                                                                                                
		trySetFromYaml(model.trop.vmf3dir,		inputs, {"troposphere", "vmf3_directory"		});															
		trySetFromYaml(model.trop.orography,	inputs, {"troposphere", "orography_files"		});															
		trySetFromYaml(model.trop.gpt2grid,		inputs, {"troposphere", "gpt2grid_files"		});															

		{
			auto gnss_data = stringsToYamlObject(inputs, {"1 gnss_observations"});

			trySetFromAny(rnx_files,			commandOpts,	gnss_data,	{"rnx_files"			}, "[string] List of rinex      files to use");
			trySetFromAny(obs_rtcm_files,		commandOpts,	gnss_data,	{"rtcm_files"			}, "[string] List of rtcmfiles  files to use for observations");	//todo
					
			trySetFromYaml(gnssDataStreams,	 					gnss_data,	{"streams"				});
			
			string root_gnss_directory = "./";
			trySetFromAny(root_gnss_directory,	commandOpts,	gnss_data,	{"0 root_directory"		}, "(string) Root path to be added to all other gnss data files (unless they are absolute)");
			trySetFromYaml(gnss_data_root_url,					gnss_data,	{"0 root_stream_url"	});
			
			tryAddRootToPath(root_gnss_directory, rnx_files);
			tryAddRootToPath(root_gnss_directory, obs_rtcm_files);
		}
		
		{
			auto pseudo_observation_data = stringsToYamlObject(inputs, {"pseudo_observations"});
			
			trySetFromAny(pseudoobs_files,		commandOpts,	pseudo_observation_data,	{"sp3_files"			}, "[string] List of sp3 files to use for pseudoobservations");
			
			string root_stream_url;
			trySetFromAny(root_stations_dir,	commandOpts,	pseudo_observation_data,	{"0 root_directory"		}, "(string) Root path to be added to all other gnss data files (unless they are absolute)");
			trySetFromYaml(root_stream_url,						pseudo_observation_data,	{"0 root_stream_url"	});
			
			tryAddRootToPath(root_stations_dir, pseudoobs_files);
		}
		
		{
			auto satellite_data = stringsToYamlObject(inputs, {"0 satellite_data"});
			
			string root_ephemeris_directory = "./";
			trySetFromAny(root_ephemeris_directory,	commandOpts,	satellite_data,	{"0 root_directory"		}, "(string) Root path to be added to all other gnss data files (unless they are absolute)");
			trySetFromYaml(ephemeris_data_root_url,					satellite_data,	{"0 root_stream_url"	});
		
			trySetFromAny(nav_rtcm_files,			commandOpts,	satellite_data,	{"rtcm_files"			}, "[string] List of rtcmfiles  files to use for corrections");
			trySetFromAny(nav_files,				commandOpts,	satellite_data, {"nav_files"			}, "[string] List of ephemeris  files to use");			
			trySetFromAny(sp3_files,				commandOpts,	satellite_data, {"sp3_files"			}, "[string] List of sp3        files to use");			
			trySetFromAny(dcb_files,				commandOpts,	satellite_data, {"dcb_files"			}, "[string] List of dcb        files to use");			
			trySetFromAny(bsx_files,				commandOpts,	satellite_data, {"bsx_files"			}, "[string] List of biassinex  files to use");			
			trySetFromAny(clk_files,				commandOpts,	satellite_data, {"clk_files"			}, "[string] List of clock      files to use");			
			trySetFromAny(orb_files,				commandOpts,	satellite_data, {"orb_files"			}, "[string] List of orbit(pod) files to use");			
			
			trySetFromYaml(ephemerisDataStreams,					satellite_data,	{"streams"				}, "[strings] Streams to be processed separately from observations, and will typically be used for receiving SSR messages or other navigational data from an external service");
			
			tryAddRootToPath(root_ephemeris_directory, nav_rtcm_files);
			
		}
	}
		
	{
		auto processing_options = stringsToYamlObject({ yaml, "" }, {processing_options_str});
	
		{
			auto general = stringsToYamlObject(processing_options, {"2 gnss_general"});
			
			bool found = trySetFromAny(elevation_mask,	commandOpts,	general, {"0 elevation_mask"		});
			if (found)
				elevation_mask *= D2R;
			
			trySetFromYaml	(interpolate_rec_pco,						general, {"interpolate_rec_pco" 	}, "(bool) ");
			trySetFromYaml	(max_gdop,									general, {"max_gdop"				});
			trySetFromYaml	(reject_eclipse,							general, {"reject_eclipse"			}, "(bool) Exclude satellites that are in eclipsing season");
			trySetFromYaml	(raim,										general, {"raim"					}, "(bool) ");
			trySetEnumOpt	(recOptsMap[""].error_model,				general, {"error_model"				}, E_NoiseModel::_from_string_nocase);
			trySetFromYaml	(pivot_station,								general, {"pivot_station"			});
			trySetFromYaml	(common_atmosphere,							general, {"common_atmosphere"		}, "(bool) ");
			trySetFromYaml	(delete_old_ephemerides,					general, {"delete_old_ephemerides"	}, "(bool) ");
			trySetFromYaml	(if_antenna_phase_centre,					general, {"use_if_apc"				}, "(bool) ");
			
			trySetFromYaml	(process_meas[CODE],						general, {"1 code_measurements",		"process"	}, "(bool) ");
			trySetFromYaml	(recOptsMap[""].code_sigmas,				general, {"1 code_measurements",		"sigmas"	});
			
			trySetFromYaml	(process_meas[PHAS],						general, {"1 phase_measurements",		"process"	}, "(bool) ");
			trySetFromYaml	(recOptsMap[""].phas_sigmas,				general, {"1 phase_measurements",		"sigmas"	});
		
			vector<string> clockCodesStrings;
			for (int i = 1; i < E_Sys::SUPPORTED; i++)
			{
				E_Sys	sys			= E_Sys::_values()[i];
				string	sysName		= E_Sys::_names() [i];		boost::algorithm::to_lower(sysName);

				auto sys_options = stringsToYamlObject(general, {"1 sys_options", sysName});
				
				trySetFromYaml(process_sys[sys],				sys_options, {"0 process"				}, "(bool) ");
				trySetFromYaml(solve_amb_for[sys],				sys_options, {"ambiguity_resolution"	}, "(bool) ");
				
				for (int i = 1; i < NUM_FTYPES; i++)
				{
					string	freq = "l" + std::to_string(i);

					trySetFromYaml(process_freq[sys][i],		sys_options, {"process_freq", freq}, "(bool) ");
				}
				
				bool found = trySetFromYaml(clockCodesStrings,		sys_options, {"clock_codes"	}, "(string) Default observation codes on two frequencies for IF combination based satellite clocks");
				if (found)
				for (auto once : {1})
				{
					try
					{
						clock_codesL1[sys] = E_ObsCode::_from_string(clockCodesStrings.at(0).c_str());
						clock_codesL2[sys] = E_ObsCode::_from_string(clockCodesStrings.at(1).c_str());
					}

					catch (...)
					{
						BOOST_LOG_TRIVIAL(error)
						<< std::endl << "Error: Invalid clock codes for: " << sysName << ", there should be two codes for each system in the form [LXX, LXX]\n";

						continue;
					}
				}
				
				vector<string> codePriorityStrings;
				found = trySetFromYaml(codePriorityStrings,			sys_options, {"code_priorities"			});
				if (found)
				for (auto once : {1})
				{
					code_priorities[sys].clear();

					for (auto& codePriorityString : codePriorityStrings)
					{
						try
						{
							auto a = E_ObsCode::_from_string(codePriorityString.c_str());
							code_priorities[sys].push_back(a);
						}
						catch (...)
						{
							continue;
						}
					}
				}
			}
		}
		
		{
			auto epoch_control = stringsToYamlObject(processing_options, {"0 epoch_control"});
			
			int i = 0;
			
			string startStr;
			string stopStr;
			trySetFromAny(epoch_interval,		commandOpts,	epoch_control, {std::to_string(i++) + "epoch_interval"		}, "(float) Desired time step between each processing epoch");
			trySetFromAny(max_epochs,			commandOpts,	epoch_control, {std::to_string(i++) + "max_epochs"			}, "(int)   Maximum number of epochs to process");
			trySetFromAny(startStr,				commandOpts,	epoch_control, {std::to_string(i++) + "start_epoch"			}, "(date) The time of the first epoch to process (all observations before this will be skipped)");
			trySetFromAny(stopStr,				commandOpts,	epoch_control, {std::to_string(i++) + "end_epoch"			}, "(date) The time of the last epoch to process (all observations after this will be skipped)");
			
			if (!startStr.empty())	start_epoch	= boost::posix_time::time_from_string(startStr);
			if (!stopStr .empty())	end_epoch	= boost::posix_time::time_from_string(stopStr);
			
			trySetFromAny(fatal_level,			commandOpts,	epoch_control, {std::to_string(i++) + "fatal_message_level"			}, "(int) Threshold level for exiting the program early (0-2)");
		
			wait_next_epoch = epoch_interval + 0.01;
			trySetFromYaml(wait_next_epoch,						epoch_control, {std::to_string(i++) + "wait_next_epoch"		}, "(float) Time to wait for next epochs data before skipping the epoch (will default to epoch_interval as an appropriate minimum value for realtime)");		
			trySetFromYaml(wait_all_stations,					epoch_control, {std::to_string(i++) + "wait_all_stations"	}, "(float) Time to wait from the reception of the first data of an epoch before skipping stations with data still unreceived");
			trySetFromYaml(require_obs,							epoch_control, {std::to_string(i++) + "require_obs"			}, "(bool) Exit the program if no observation sources are available");
			trySetFromAny(simulate_real_time,	commandOpts,	epoch_control, {std::to_string(i++) + "simulate_real_time"	}, "(bool)  For RTCM playback - delay processing to match original data rate");
		}
		
		{
			auto gnss_modelling = stringsToYamlObject(processing_options, {"2 gnss_models"});
			
			trySetFromYaml(model.rec_pos.enable,					gnss_modelling, {"rec_pos",				"enable"	}, "(bool) ");
			trySetFromYaml(model.sat_pos.enable,					gnss_modelling, {"sat_pos",				"enable"	}, "(bool) ");
			trySetEnumOpt( model.sat_pos.ephemeris_source,	 		gnss_modelling,	{"sat_pos",				"source"	}, E_Ephemeris::_from_string_nocase);
		                                                                                            		
			trySetFromYaml(model.range,								gnss_modelling, {"range",				"enable"	}, "(bool) ");
			                                                                                        		
			trySetFromYaml(model.sat_clock.enable,					gnss_modelling, {"sat_clock",			"enable"	}, "(bool) ");
			trySetEnumOpt( model.sat_clock.ephemeris_source, 		gnss_modelling,	{"sat_clock",			"source"	}, E_Ephemeris::_from_string_nocase);
			
			trySetFromYaml(model.rec_clock.enable,					gnss_modelling, {"rec_clock",			"enable"	}, "(bool) ");
			trySetFromYaml(model.sat_code_bias,						gnss_modelling, {"sat_code_bias",		"enable"	}, "(bool) ");
			trySetFromYaml(model.rec_code_bias,						gnss_modelling, {"rec_code_bias",		"enable"	}, "(bool) ");
			trySetFromYaml(model.sat_phase_bias,					gnss_modelling, {"sat_phase_bias",		"enable"	}, "(bool) ");
			trySetFromYaml(model.rec_phase_bias,					gnss_modelling, {"rec_phase_bias",		"enable"	}, "(bool) ");
			                                                     
			trySetFromYaml(model.rec_ant_delta,						gnss_modelling, {"rec_ant_delta",		"enable"	}, "(bool) ");
			trySetFromYaml(model.sat_pco,							gnss_modelling, {"sat_pco",				"enable"	}, "(bool) ");
			trySetFromYaml(model.rec_pco,							gnss_modelling, {"rec_pco",				"enable"	}, "(bool) ");
			trySetFromYaml(model.sat_pcv,							gnss_modelling, {"sat_pcv",				"enable"	}, "(bool) ");
			trySetFromYaml(model.rec_pcv,							gnss_modelling, {"rec_pcv",				"enable"	}, "(bool) ");
			                                                     
			trySetFromYaml(model.tides.enable,						gnss_modelling, {"tides",				"enable"	}, "(bool) ");
			trySetFromYaml(model.tides.solid,						gnss_modelling, {"tides",				"solid"		}, "(bool) ");
			trySetFromYaml(model.tides.otl,							gnss_modelling, {"tides",				"otl"		}, "(bool) ");
			trySetFromYaml(model.tides.pole,						gnss_modelling, {"tides",				"pole"		}, "(bool) ");
			
			trySetFromYaml(model.relativity,						gnss_modelling, {"relativity",			"enable"	}, "(bool) ");
			trySetFromYaml(model.relativity2,						gnss_modelling, {"relativity2",			"enable"	}, "(bool) ");
			trySetFromYaml(model.sagnac,							gnss_modelling, {"sagnac",				"enable"	}, "(bool) ");
			
			{
				auto ionospheric_component = stringsToYamlObject(gnss_modelling, {"ionospheric_component"});
			
				trySetFromYaml(model.ionospheric_component,			ionospheric_component, {"0 enable"	}, "(bool) ");
	
				trySetEnumOpt( ionoOpts.corr_mode, 					ionospheric_component, {"corr_mode" 			}, E_IonoMode::_from_string_nocase);
				trySetEnumOpt( ionoOpts.iflc_freqs,					ionospheric_component, {"iflc_freqs" 			}, E_LinearCombo::_from_string_nocase);
				trySetFromYaml(ionoOpts.common_ionosphere,			ionospheric_component, {"common_ionosphere"		}, "(bool) ");
				trySetFromYaml(ionoOpts.use_if_combo,				ionospheric_component, {"use_if_combo"			}, "(bool) ");
				trySetFromYaml(ionoOpts.auto_select_default_code,	ionospheric_component, {"automatic_def_codes"	}, "(bool) Automatically detect/select default GNSS codes to estimate the Ionosphere");	
			}
			
			{
				auto ionospheric_model = stringsToYamlObject(gnss_modelling, {"ionospheric_model"});

				trySetFromYaml(process_ionosphere,			ionospheric_model, {"0 enable"				}, "(bool) Compute ionosphere maps from a network of stations");
				trySetEnumOpt( ionModelOpts.model, 			ionospheric_model, {"model" 				}, E_IonoModel::_from_string_nocase);
				trySetFromYaml(ionModelOpts.function_order,	ionospheric_model, {"function_order"		});
				trySetFromYaml(ionModelOpts.model_noise,	ionospheric_model, {"model_noise"			});
				trySetKalmanFromYaml(ionModelOpts.ion,		ionospheric_model, "ion");		
				
				bool found = trySetFromYaml(ionModelOpts.layer_heights,		ionospheric_model, {"layer_heights"			});
				if (found)
				for (auto& a : ionModelOpts.layer_heights)
				{
					a *= 1000; //km to m
				}
			}

			auto troposhpere = stringsToYamlObject(gnss_modelling, {"troposphere"});
			{
				trySetFromYaml(model.trop.enable,		troposhpere,	{"0 enable" 		});
				trySetEnumOpt( model.trop.model, 		troposhpere,	{"model" 			}, E_TropModel::_from_string_nocase);
			}
	
			trySetFromYaml(model.phase_windup,						gnss_modelling, {"phase_windup",		"enable"	}, "(bool) ");
			trySetFromYaml(model.integer_ambiguity,					gnss_modelling, {"integer_ambiguity",	"enable"	}, "(bool) ");
			trySetFromYaml(model.clock_definitions,					gnss_modelling, {"clock_definitions",	"enable"	}, "(bool) ");
		}
		
		{
			auto model_error_checking = stringsToYamlObject(processing_options, {"3 model_error_checking"});
			
			{
				auto deweighting = stringsToYamlObject(model_error_checking, {"deweighting"});
				
				trySetFromYaml(deweight_factor,				deweighting,	{"deweight_factor"			}, "(float) Factor to downweight the variance of measurements with statistically detected errors");
				trySetFromYaml(deweight_on_state_error,		deweighting,	{"deweight_on_state_error"	}, "(bool) Any \"state\" errors cause deweighting of all measurements that reference the state");
			}
			
			{
				auto ambiguities = stringsToYamlObject(model_error_checking, {"ambiguities"});
				
				trySetFromYaml(reinit_on_all_slips,			ambiguities,	{"reinit_on_all_slips"		}, "(bool) Any detected slips cause removal and reinitialisation of ambiguities");
				trySetFromYaml(pppOpts.outage_reset_limit,	ambiguities,	{"outage_reset_limit"		}, "(int) Maximum number of epochs with missed phase measurements before the ambiguity associated with the measurement is reset.");
				trySetFromYaml(pppOpts.phase_reject_limit,	ambiguities,	{"phase_reject_limit"		}, "(int) Maximum number of phase measurements to reject before the ambiguity associated with the measurement is reset.");
			}
			
			{
				auto clocks = stringsToYamlObject(model_error_checking, {"clocks"});
				
				trySetFromYaml(reinit_on_clock_error,		clocks,			{"reinit_on_clock_error"	}, "(bool) Any clock \"state\" errors cause removal and reinitialisation of the clocks and all associated ambiguities");
			}
		
			{
				auto cycle_slips = stringsToYamlObject(model_error_checking, {"cycle_slips"});
				
				trySetFromYaml(thres_slip,			cycle_slips, {"slip_threshold"		});
				trySetFromYaml(mw_proc_noise,		cycle_slips, {"mw_proc_noise"		});
				
				trySetFromYaml(excludeSlip.LLI,		cycle_slips, {"detect",		"lli"	}, "(bool) Exclude measurements that fail LLI slip test in preprocessor");            
				trySetFromYaml(excludeSlip.GF,		cycle_slips, {"detect",		"gf"	}, "(bool) Exclude measurements that fail GF  slip test in preprocessor");            
				trySetFromYaml(excludeSlip.MW,		cycle_slips, {"detect",		"mw"	}, "(bool) Exclude measurements that fail MW  slip test in preprocessor");            
				trySetFromYaml(excludeSlip.EMW,		cycle_slips, {"detect",		"emw"	}, "(bool) Exclude measurements that fail EMW slip test in preprocessor");            
				trySetFromYaml(excludeSlip.CJ,		cycle_slips, {"detect",		"cj"	}, "(bool) Exclude measurements that fail clk jump test in preprocessor");            
				trySetFromYaml(excludeSlip.SCDIA,	cycle_slips, {"detect",		"scdia"	}, "(bool) Exclude measurements that fail SCDIA    test in preprocessor");   
			}
		}
		
		{
			auto process_modes = stringsToYamlObject(processing_options, {"1 process_modes"});
			
			trySetFromYaml(process_user,				process_modes, {"user"				}, "(bool) Compute PPP for separate stations using provided satellite data");
			trySetFromYaml(process_network,				process_modes, {"network"			}, "(bool) Compute PPP corrections for a network of stations and satellites");
			trySetFromYaml(process_ionosphere,			process_modes, {"ionosphere"		}, "(bool) ");
			trySetFromYaml(process_preprocessor,		process_modes, {"preprocessor"		}, "(bool) Perform preprocessing for quality checks");
			trySetFromYaml(process_ppp,					process_modes, {"ppp"				}, "(bool) Perform PPP on a network of stations and satellites");
	// 		trySetFromYaml(process_orbits,				b, {"process_modes","orbits"				}, "(bool) Propagate an orbit using initial conditions");
		}
		
		{
			auto minimum_constraints = stringsToYamlObject(processing_options, {"minimum_constraints"});
			
			trySetFromYaml(process_minimum_constraints,		minimum_constraints,	{"enable"					}, "(bool) Transform states by minimal constraints to selected station coordinates");
		
			trySetKalmanFromYaml(minCOpts.scale,			minimum_constraints,	"scale");
			trySetKalmanFromYaml(minCOpts.rotation,			minimum_constraints,	"rotation");
			trySetKalmanFromYaml(minCOpts.translation,		minimum_constraints,	"translation");
			
			trySetFromYaml(minCOpts.full_vcv,				minimum_constraints,	{"full_vcv"					},	"(bool) ! experimental ! Use full VCV for measurement noise in minimum constraints filter");
			trySetFromYaml(minCOpts.scale_by_vcv,			minimum_constraints,	{"scale_by_vcv"				},	"(bool) Use variance of positions as additional scaling factor in minimum constraints weighting");
			                                            	
			trySetEnumOpt( minCOpts.inverter, 				minimum_constraints,	{"inverter" 				}, E_Inverter::_from_string_nocase);
			trySetFromYaml(minCOpts.max_filter_iter,		minimum_constraints,	{"max_filter_iterations"	});
			trySetFromYaml(minCOpts.max_prefit_remv,		minimum_constraints,	{"max_prefit_removals"		}, 														"(int) Maximum number of measurements to exclude using prefit checks before attempting to filter");
			trySetFromYaml(minCOpts.sigma_check,			minimum_constraints,	{"outlier_screening", "sigma_check"			},										"(bool)  Enable prefit and postfit sigma check");
			trySetFromYaml(minCOpts.w_test,					minimum_constraints,	{"outlier_screening", "w_test"				},										"(bool)  Enable w-test");
			trySetFromYaml(minCOpts.chi_square_test,		minimum_constraints,	{"outlier_screening", "chi_square_test"		},										"(bool)  Enable Chi-square test");
			trySetEnumOpt( minCOpts.chi_square_mode,		minimum_constraints,	{"outlier_screening", "chi_square_mode"		}, E_ChiSqMode::_from_string_nocase,	"(enum)  Chi-square test mode - innovation, measurement, state");
			trySetFromYaml(minCOpts.sigma_threshold,		minimum_constraints,	{"outlier_screening", "sigma_threshold"		},										"(float) sigma threshold");
		}

		{
			auto ambres_options = stringsToYamlObject(processing_options, {"ambiguity_resolution"});
		
			trySetFromYaml(ambrOpts.min_el_AR,			ambres_options, {"elevation_mask"				});
			trySetFromYaml(ambrOpts.lambda_set,			ambres_options, {"lambda_set_size"				});
			trySetFromYaml(ambrOpts.AR_max_itr,			ambres_options, {"max_rounding_iterations"		});
			trySetFromYaml(ambrOpts.Max_Hold_epoc,		ambres_options, {"max_hold_epochs"				}, "Maximun number of epocs to hold ambiguities");
			trySetFromYaml(ambrOpts.Max_Hold_time,		ambres_options, {"max_hold_time"				}, "Maximun amount of time (sec) to hold ambiguities");

			trySetEnumOpt( ambrOpts.WLmode,				ambres_options,	{"wide_lane", "mode" 						}, E_ARmode::_from_string_nocase);
			trySetFromYaml(ambrOpts.WLsuccsThres,		ambres_options, {"wide_lane", "success_rate_threshold"		});
			trySetFromYaml(ambrOpts.WLratioThres,		ambres_options,	{"wide_lane", "solution_ratio_threshold"	});
			trySetFromYaml(ambrOpts.WLSatPrcNois,		ambres_options,	{"wide_lane", "process_noise_sat"			}, "WL bias process noise for satellite");
			trySetFromYaml(ambrOpts.WLRecPrcNois,		ambres_options,	{"wide_lane", "process_noise_rec"			}, "WL bias process noise for receivers");
			trySetFromYaml(ambrOpts.WL_filter_iter,		ambres_options, {"wide_lane", "filter_max_iterations"		});
			trySetFromYaml(ambrOpts.WL_prefit_remv,		ambres_options, {"wide_lane", "filter_max_removals"			});
			
			trySetEnumOpt( ambrOpts.NLmode,				ambres_options,	{"narrow_lane", "mode" 						}, E_ARmode::_from_string_nocase);
			trySetFromYaml(ambrOpts.NLsuccsThres,		ambres_options, {"narrow_lane", "success_rate_threshold"	});
			trySetFromYaml(ambrOpts.NLratioThres,		ambres_options, {"narrow_lane", "solution_ratio_threshold"	});
			trySetFromYaml(ambrOpts.NLstarttime,		ambres_options, {"narrow_lane", "proc_start"				});

			trySetFromYaml(ambrOpts.reduction_limit,	ambres_options, {"reduction_limit"					});
		}
		
		{
			auto ssr_corrections = stringsToYamlObject(processing_options, {"ssr_corrections"});
		
			trySetEnumOpt (ssrOpts.ephemeris_source, 		ssr_corrections, {"ephemeris_source" 		}, E_Ephemeris::_from_string_nocase);
			trySetEnumOpt (ssrOpts.clock_source, 			ssr_corrections, {"clock_source" 			}, E_Ephemeris::_from_string_nocase);
			trySetEnumOpt (ssrOpts.code_bias_source, 		ssr_corrections, {"code_bias_source" 		}, E_Ephemeris::_from_string_nocase);
			trySetEnumOpt (ssrOpts.phase_bias_source, 		ssr_corrections, {"phase_bias_source" 		}, E_Ephemeris::_from_string_nocase);
			trySetFromYaml(ssrOpts.calculate_ssr,			ssr_corrections, {"calculate_precursors"	}, "(bool) ");
			trySetFromYaml(ssrOpts.prediction_interval,		ssr_corrections, {"prediction_interval"		});
			trySetFromYaml(ssrOpts.prediction_duration,		ssr_corrections, {"prediction_duration"		});
			trySetFromYaml(ssrOpts.extrapolate_corrections,	ssr_corrections, {"extrapolate_corrections"	}, "(bool) ");
		}	
		
		{
			auto ssr_inputs = stringsToYamlObject(processing_options, {"ssr_inputs"});
		
			trySetFromYaml(ssrInOpts.code_bias_valid_time,	ssr_inputs, {"code_bias_validity_time"	},	"(double) Valid time period of SSR code biases");		
			trySetFromYaml(ssrInOpts.phase_bias_valid_time,	ssr_inputs, {"phase_bias_validity_time"	},	"(double) Valid time period of SSR phase biases");		
			trySetFromYaml(validity_interval_factor,		ssr_inputs, {"validity_interval_factor"	});
			trySetEnumOpt(ssr_input_antenna_offset,			ssr_inputs,	{"ssr_antenna_offset"		}, E_OffsetType::_from_string_nocase, "Ephemeris type that is provided in the listed SSR stream, i.e. satellite antenna-phase-centre (APC) or centre-of-mass (COM). This information is listed in the NTRIP Caster's sourcetable");
		}
		
		auto filter_options = stringsToYamlObject(processing_options, {"filter_options"});
		{
			trySetFromYaml(joseph_stabilisation,	filter_options,	{"joseph_stabilisation"							});
			trySetEnumOpt( pppOpts.inverter, 		filter_options,	{"inverter" 									}, E_Inverter::_from_string_nocase, "Inverter to be used within the Kalman filter update stage, which may provide different performance outcomes in terms of processing time and accuracy and stability.");
			trySetFromYaml(process_rts,				filter_options,	{"rts", "0 enable"								}, "(bool) Perform backward smoothing of states to improve precision of earlier states");
			trySetFromYaml(pppOpts.rts_lag,			filter_options,	{"rts", "1 lag"									}, "(int) Number of epochs to use in RTS smoothing. Negative numbers indicate full reverse smoothing.");
			trySetFromYaml(pppOpts.rts_directory,	filter_options,	{"rts", "directory"								}, "(string) Directory for rts intermediate files");
			trySetFromYaml(pppOpts.rts_filename,	filter_options,	{"rts", "filename"								}, "(string) Base filename for rts intermediate files");
			trySetFromYaml(pppOpts.max_filter_iter,	filter_options,	{"outlier_screening", "max_filter_iterations"	});
			trySetFromYaml(pppOpts.sigma_check,		filter_options,	{"outlier_screening", "sigma_check"				},										"(bool)  Enable prefit and postfit sigma check");
			trySetFromYaml(pppOpts.w_test,			filter_options,	{"outlier_screening", "w_test"					},										"(bool)  Enable w-test");
			trySetFromYaml(pppOpts.chi_square_test,	filter_options,	{"outlier_screening", "chi_square_test"			},										"(bool)  Enable Chi-square test");
			trySetEnumOpt( pppOpts.chi_square_mode,	filter_options,	{"outlier_screening", "chi_square_mode"			}, E_ChiSqMode::_from_string_nocase,	"(enum)  Chi-square test mode - innovation, measurement, state");
			trySetFromYaml(pppOpts.sigma_threshold,	filter_options,	{"outlier_screening", "sigma_threshold"			},										"(float) sigma threshold");
			trySetFromYaml(pppOpts.max_prefit_remv,	filter_options,	{"outlier_screening", "max_prefit_removals"		}, 										"(int) Maximum number of measurements to exclude using prefit checks before attempting to filter");
			trySetFromYaml(pppOpts.chunk_size,		filter_options,	{"chunking", "size"								});
			trySetFromYaml(pppOpts.chunk_stations,	filter_options,	{"chunking", "enable"							}, "(bool) ");
		}
	}
	
#if 0
	auto force_models = stringsToYamlObject({ yaml, "" }, {processing_options_str, "force_models"});
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
					
                                                      
// 		trySetFromYaml(split_sys,				outputs, { "split_sys"		});
                                                      
// 		trySetFromAny (output_persistance,		commandOpts, 	output_files, {"output_persistance"		});
// 		trySetFromAny (input_persistance,		commandOpts,	output_files, {"input_persistance"		});
// 		trySetFromYaml(persistance_directory,					output_files, {"persistance_directory"	});
// 		trySetFromYaml(persistance_filename,					output_files, {"persistance_filename"	});

#endif
	
	auto estimation_parameters = stringsToYamlObject({yaml, ""}, {estimation_parameters_str});
	{
		trySetKalmanFromYaml(pppOpts.eop,		estimation_parameters, "eop"		);
		trySetKalmanFromYaml(pppOpts.eop_rates,	estimation_parameters, "eop_rates"	);
	}

	{
		auto mongo = stringsToYamlObject({yaml, ""}, {"5 mongo"});	
		
		trySetFromYaml(enable_mongo,				mongo, {"0 enable"					}, "(bool)   Enable and connect to mongo database");
		trySetFromYaml(output_mongo_rtcm_messages,	mongo, {"output_rtcm_messages"		}, "(bool)   Output rtcm data to mongo");
		trySetFromYaml(output_mongo_measurements,	mongo, {"output_measurements"		}, "(bool)   Output measurements and their residuals");
		trySetFromYaml(output_mongo_states,			mongo, {"output_states"				}, "(bool)   Output states");
		trySetFromYaml(output_mongo_test_stats,		mongo, {"output_test_stats"			}, "(bool)   Output test statistics");
		trySetFromYaml(output_intermediate_rts,		mongo, {"output_intermediate_rts"	}, "(bool)   Output best available smoothed states when performing fixed-lag rts (slow, use only when needed)");
		trySetFromYaml(output_mongo_logs,			mongo, {"output_logs"				}, "(bool)   Output console trace and warnings to mongo with timestamps and other metadata");
		trySetFromYaml(delete_mongo_history,		mongo, {"delete_history"			}, "(bool)   Drop the collection in the database at the beginning of the run to only show fresh data");
		trySetFromYaml(mongo_rts_suffix,			mongo, {"rts_suffix"				}, "(string) Suffix to append to database elements for reverse smoothed elements");
		trySetFromYaml(mongo_suffix,				mongo, {"suffix"					}, "(string) Suffix to append to database elements to make distinctions between runs for comparison");
		trySetFromYaml(mongo_database,				mongo, {"database"					}, "(string) ");
		trySetFromYaml(mongo_uri,					mongo, {"uri"						}, "(string) Location and port of the mongo database to connect to");
	}
	
	{
		auto debug = stringsToYamlObject({yaml, ""}, {"9 debug"});	
		
		trySetFromYaml(check_plumbing,		debug, {"check_plumbing"	}, "(bool) Debugging option to show sizes of objects in memory to detect leaks");
		trySetFromYaml(retain_rts_files,	debug, {"retain_rts_files"	}, "(bool) Debugging option to keep rts files for post processing");
		trySetFromYaml(rts_only,			debug, {"rts_only"			}, "(bool) Debugging option to only re-run rts from previous run");
		trySetFromYaml(mincon_only,			debug, {"mincon_only"		}, "(bool) Debugging option to only save and re-run minimum constraints code");
		
		auto unit_tests = stringsToYamlObject(debug, {"unit_tests"});
		{
			trySetFromYaml(testOpts.enable,			unit_tests, {"enable"			}, "(bool) Perform unit tests while processing");
			
			trySetFromYaml(testOpts.output_pass,	unit_tests, {"output_pass"		}, "(bool) ");
			trySetFromYaml(testOpts.stop_on_fail,	unit_tests, {"stop_on_fail"		}, "(bool) ");
			trySetFromYaml(testOpts.stop_on_done,	unit_tests, {"stop_on_done"		}, "(bool) ");
			trySetFromYaml(testOpts.output_errors,	unit_tests, {"output_errors"	}, "(bool) ");
			trySetFromYaml(testOpts.absorb_errors,	unit_tests, {"absorb_errors"	}, "(bool) ");
                                                      
			trySetFromYaml(test_directory,			unit_tests, {"directory"		});
			trySetFromYaml(test_filename,			unit_tests, {"filename"			});
		}
	}
	
	tryAddRootToPath(root_input_directory, atx_files);				globber(atx_files);
	tryAddRootToPath(root_input_directory, snx_files);				globber(snx_files);
	tryAddRootToPath(root_input_directory, blq_files);				globber(blq_files);
	tryAddRootToPath(root_input_directory, nav_files);				globber(nav_files);
	tryAddRootToPath(root_input_directory, sp3_files);				globber(sp3_files);
	tryAddRootToPath(root_input_directory, erp_files);				globber(erp_files);
	tryAddRootToPath(root_input_directory, dcb_files);				globber(dcb_files);
	tryAddRootToPath(root_input_directory, bsx_files);				globber(bsx_files);
	tryAddRootToPath(root_input_directory, ion_files);				globber(ion_files);
	tryAddRootToPath(root_input_directory, clk_files);				globber(clk_files);
	tryAddRootToPath(root_input_directory, orb_files);				globber(orb_files);
	tryAddRootToPath(root_input_directory, egm_files);				globber(egm_files);
// 	tryAddRootToPath(root_input_directory, jpl_files);				globber(jpl_files);
	tryAddRootToPath(root_input_directory, rnx_files);				globber(rnx_files);
	tryAddRootToPath(root_input_directory, obs_rtcm_files);			globber(obs_rtcm_files);
	tryAddRootToPath(root_input_directory, nav_rtcm_files);			globber(nav_rtcm_files);
	tryAddRootToPath(root_input_directory, pseudoobs_files);		globber(pseudoobs_files);

	tryAddRootToPath(root_input_directory, model.trop.vmf3dir);		
	tryAddRootToPath(root_input_directory, model.trop.orography);	
	tryAddRootToPath(root_input_directory, model.trop.gpt2grid);		
	
	string revert = trace_directory;
	
	tryPatchPaths(root_output_directory,	erp_directory,							erp_filename);
	tryPatchPaths(root_output_directory,	gpx_directory,							gpx_filename);
	tryPatchPaths(root_output_directory,	log_directory,							log_filename);
	tryPatchPaths(root_output_directory,	test_directory,							test_filename);
	tryPatchPaths(root_output_directory,	sinex_directory,						sinex_filename);
	tryPatchPaths(root_output_directory,	ionex_directory,						ionex_filename);
	tryPatchPaths(root_output_directory,	orbits_directory,						orbits_filename);
	tryPatchPaths(root_output_directory,	clocks_directory,						clocks_filename);
	tryPatchPaths(root_output_directory,	ionstec_directory,						ionstec_filename);
	tryPatchPaths(root_output_directory,	ppp_sol_directory,						ppp_sol_filename);
	tryPatchPaths(root_output_directory,	rtcm_nav_directory,						rtcm_nav_filename);
	tryPatchPaths(root_output_directory,	rtcm_obs_directory,						rtcm_obs_filename);
	tryPatchPaths(root_output_directory,	rinex_obs_directory,					rinex_obs_filename);
	tryPatchPaths(root_output_directory,	rinex_nav_directory,					rinex_nav_filename);
	tryPatchPaths(root_output_directory,	bias_sinex_directory,					bias_sinex_filename);
	tryPatchPaths(root_output_directory,	trop_sinex_directory,					trop_sinex_filename);
	tryPatchPaths(root_output_directory,	persistance_directory,					persistance_filename);
	tryPatchPaths(root_output_directory,	pppOpts.rts_directory,					pppOpts.rts_filename);
	tryPatchPaths(root_output_directory,	trace_directory,						station_trace_filename);	trace_directory = revert;
	tryPatchPaths(root_output_directory,	trace_directory,						network_trace_filename);
	tryPatchPaths(root_output_directory,	decoded_rtcm_json_directory,			decoded_rtcm_json_filename);
	tryPatchPaths(root_output_directory,	encoded_rtcm_json_directory,			encoded_rtcm_json_filename);
	tryPatchPaths(root_output_directory,	network_statistics_json_directory,		network_statistics_json_filename);
	
	//Try to change all filenames to replace <YYYY> etc with other values.
	replaceTags(nav_files);
	replaceTags(orb_files);
	replaceTags(sp3_files);
	replaceTags(clk_files);
	replaceTags(atx_files);
	replaceTags(snx_files);
	replaceTags(blq_files);
	replaceTags(erp_files);
	replaceTags(dcb_files);
	replaceTags(bsx_files);
	replaceTags(ion_files);
	replaceTags(egm_files);
	replaceTags(jpl_files);
	replaceTags(rnx_files);
	replaceTags(obs_rtcm_files);
	replaceTags(nav_rtcm_files);
	replaceTags(pseudoobs_files);

	replaceTags(root_stations_dir);
	replaceTags(gnss_data_root_url);
	replaceTags(ephemeris_data_root_url);
	
	replaceTags(mongo_suffix);
	replaceTags(mongo_database);
	
	for (auto& station_file : station_files)
	{
		replaceTags(station_file);
	}
	
	for (auto nav : {false, true})
	{
		vector<string>* streamNode_ptr;
		string stream_root;
		if (nav == false)	{	streamNode_ptr = &gnssDataStreams;			stream_root = gnss_data_root_url;		}
		else				{	streamNode_ptr = &ephemerisDataStreams;		stream_root = ephemeris_data_root_url;	}

		map<string, string> ntripStreams;
		for (auto streamUrl : *streamNode_ptr)
		{
			string fullUrl = streamUrl;

			if (fullUrl.find(':') == std::string::npos)
			{
				//only add root if it looks like it doesnt have one (eg https':'//)
				tryAddRootToPath(stream_root, fullUrl);
			}
			
			replaceTags(fullUrl);
			
			while (fullUrl.back() == '/')
				fullUrl.pop_back();				// in case of terminating '/'

			std::size_t slashPos = fullUrl.find_last_of("/");	// find first 4 characters after last '/'
			string hostname = fullUrl.substr(0,slashPos+1);		// e.g. http://user:pass@auscors.ga.gov.au:2101/BCEP00BKG0 --> BCEP

			pppOpts.download_hosts.insert(hostname);
			
			if (nav == false)			pppOpts.obs_mount_url.insert(fullUrl);
			else						pppOpts.nav_mount_url.insert(fullUrl);
		}
	}

	SatSys			dummySat;
				
	auto satOpts = getSatOpts(dummySat);
	auto recOpts = getRecOpts	("XMPL");
	auto minOpts = getMinConOpts("XMPL");
	
	
#	ifndef ENABLE_UNIT_TESTS
	if (testOpts.enable)
	{
		std::cout << std::endl << "Error: Tests requested by config but this is a non-test binary."				<< std::endl;
		std::cout << std::endl << "Tests can be enabled by building with $ cmake -DENABLE_UNIT_TESTS=ON .."		<< std::endl;
		exit(1);
	}
#	endif

	recurseYaml(yaml);
	
	if (commandOpts.count("yaml-defaults"))
	{
		outputDefaultConfiguration();
	}
	
	return true;
}
