
// #pragma GCC optimize ("O0")

#include <iostream>
#include <sstream>
#include <memory>
#include <string>
#include <tuple>
#include <mutex>  
#include <map>

using std::stringstream;
using std::lock_guard;
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

#include "peaCommitStrings.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "debug.hpp"

ACSConfig acsConfig = {};

struct EnumDetails
{
	vector<string> usingOptions;
	vector<string> enums;
};

map<string, EnumDetails> enumDetailsMap;

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
	if (path.empty())					{	return;	}
	if (root == "./")					{	return;	}
	if (path.find(':') != string::npos)	{	return;	}
	
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
		root += '/';
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

/** Add a root to paths that are not already absolutely defined
*/
void tryAddRootToPath(
	string&							root,		///< Root path
	map<string, vector<string>>&	paths)		///< Filename to prepend root path to
{
	for (auto& [id, path] : paths)
	{
		tryAddRootToPath(root, path);
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

	if (tokens.back() == "")
	{
		return true;
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
		if (fileName.find('*') == string::npos)
		{
			newFiles.push_back(fileName);
			continue;
		}

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

		vector<string> globFiles;

		for (auto dir_file : boost::filesystem::directory_iterator(searchDir))
		{
			// Skip if not a file
			if (boost::filesystem::is_regular_file(dir_file) == false)
				continue;

			string dir_fileName = dir_file.path().filename().string();

			if (checkGlob(searchGlob, dir_fileName))
			{
				globFiles.push_back(dir_file.path().string());
			}
		}

		std::sort(globFiles.begin(), globFiles.end());

		newFiles.insert(newFiles.end(), globFiles.begin(), globFiles.end());
	}

	files = newFiles;
}

void globber(
	map<string, vector<string>>&	files)
{
	for (auto& [id, file] : files)
	{
		globber(file);
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
	for (auto& filename : acsConfig.configFilenames)
	{
		Block block(trace, (string)"FILE/RAW_CONFIG " + filename);

		std::ifstream config(filename);

		string str;
		while (std::getline(config, str))
		{
			trace << str << std::endl;
		}
	}
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
	std::stringstream ss;
	ss << std::boolalpha << value;

	return ss.str();
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
	string&			cutstr,
	bool			colon = true)
{
	string token;
	
	stringstream ss(stack);
	string newStack;

	while (getline(ss, token, ':'))
	{
		size_t found = token.find_first_not_of("0123456789!@#: ");
		if (found != std::string::npos)
		{
			cutstr = token.substr(0, found);
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
	int 	indentation = 0;
	int		width;
	char	indent;
	
	Indentor(
		char	c		= ' ',
		int		width	= 4)
	:	width	{width},
		indent	{c}
	{
		
	}
	
	Indentor operator++(int)
	{
		Indentor old = *this;
		indentation += width;
		return old;
	}
	
	Indentor& operator--()
	{
		indentation -= width;
		return *this;
	}
	
	operator string() const 
	{
		return string(indentation, indent);
	}
	
    friend std::ostream& operator<<(std::ostream& os, const Indentor& dt);
};

std::ostream& operator<<(std::ostream& os, const Indentor& indentor)
{
    os << string(indentor.indentation, indentor.indent);
    return os;
}

struct TempStreamDisabler
{
	std::ostream&	stream;
	bool			disabled = false;
	
	TempStreamDisabler(
		std::ostream& stream)
	: stream {stream}
	{
		
	}
	
	void disable()
	{
		if (stream.fail() == false)
		{
			stream.setstate(std::ios_base::failbit);
			disabled = true;
		}
	}
	
	~TempStreamDisabler()
	{
		if (disabled)
			stream.clear();
	}
};

/** Recursive function to output the default values of all siblings with a common root.
 */
template<typename TYPE>
void outputDefaultSiblings(
	int				level,
	std::ostream&	html,			///< Html file stream to output configurator to
	std::ostream&	md,				///< Markdown file stream to output configurator to
	TYPE&			it,				///< Iterator over the default values map
	Indentor&		indentor,		///< Helper to maintain and output indentation for default yaml output
	Indentor&		htmlIndentor,	///< Helper to maintain and output indentation for internal html output
	Indentor&		mdIndentor, 	///< Helper to maintain and output indentation for markdown output
	string			root = "")		///< Common root to determine extent of siblings relationship
{
	//keep going until the end of the file, or this function returns due to the next iterator not being a sibling
	while (it != acsConfig.yamlDefaults.end())
	{
		auto& [itStack, defaults] = *it;
		
		// Check the name of this parameter against the root
		bool itIsSibling = (itStack.substr(0, root.length()) == root);
		
		// Exit this level of recursion once a non-sibling is found
		if (itIsSibling == false)
		{
			return;
		}
		
		//do this one, and bump the iterator
		{
			TempStreamDisabler disableCout	(std::cout);
			TempStreamDisabler disableHtml	(html);
			TempStreamDisabler disableMd	(md);
			
			auto& [stack,		defaultVals]	= *it;
			auto& defaultVal					= defaultVals.defaultValue;
			auto& comment						= defaultVals.comment;
			
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
				string cutstr;
				token = nonNumericStack(token, cutstr);
				flatStack += token;
				
				int optionLevel = 4;
				if		(cutstr.find('!') != string::npos)		optionLevel = 1;
				else if	(cutstr.find('@') != string::npos)		optionLevel = 2;
				else if	(cutstr.find('#') != string::npos)		optionLevel = 3;
				
				if (optionLevel > level)
				{
					disableCout	.disable();
					disableHtml	.disable();
					disableMd	.disable();
				}
			}
			
			//output the boilerplate of the name, and comment up to the point where the children are nested
			tracepdeex(0, std::cout, "\n%s%s\t%-30s", ((string)indentor).c_str(), token.c_str(), (defaultVal).c_str());
			
			html << std::endl <<	htmlIndentor++		<< "<div class='element'>";
			html << std::endl <<	htmlIndentor		<< "<input type='checkbox' id='" << flatStack << "'>";
			html << std::endl <<	htmlIndentor++		<< "<div class='ident' data-indent='" <<  indentor << "'>"
					<< (nextIsChild ? "<b>" : "") << token  
					<< (nextIsChild ? " ⯆</b>" : "");
			
			mdIndentor++;
			if (mdIndentor.indentation == 2)
			{
				md << std::endl << mdIndentor << " " << token << std::endl;
			}
			
			string link;
			
			if (defaults.enumName.empty() == false)
			{
				string linkName = defaults.enumName;
		
				if (defaultVal.find('[') == string::npos)	link += "[`"	+ linkName +  "`]";
				else										link += "[`["	+ linkName + "]`]";
					
				link += "(#" + boost::algorithm::to_lower_copy(linkName) + ") ";
				
			}
			
			md << std::endl << "###### **`" << flatStack << "`**";
			
			md << std::endl << " " << link << "`" << defaultVal << " `" << std::endl;
			
			if (comment.empty() == false)
			{
				std::cout << "\t# " << comment.substr(0, comment.find('.'));
				html << std::endl <<	htmlIndentor		<< "<span class='tooltiptext'># " << comment << "</span>";
				
				auto period = comment.find('.');
				md << std::endl << std::endl << "> " <<	comment.substr(0, period);
				if	( period != string::npos
					&&period + 2 < comment.size())
				{	
					md << std::endl << std::endl <<		comment.substr(period + 2);
				}
			}
			
			html << std::endl << --	htmlIndentor		<< "</div>";
			
			
			md		<< std::endl << std::endl << "---" << std::endl;
			
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
				outputDefaultSiblings(level, html, md, it, indentor, htmlIndentor, mdIndentor, stack);
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
						html << std::endl <<	htmlIndentor	<< "<option value='true' "	<< (defaultVal == "true"	? " selected" : "") << ">true</option>";
						html << std::endl <<	htmlIndentor	<< "<option value='false' "	<< (defaultVal == "false"	? " selected" : "") << ">false</option>";
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
		
			html	<< std::endl << --htmlIndentor		<< "</div>";
			
			--mdIndentor;
		}
	}
}

/** Outputs default configuration and a configurator
 * The default values, and descriptions of each parameter configured is output to the command line.
 * A configurator is generated that can be used to edit the default configuration via interactive html scripts
 */
void ACSConfig::outputDefaultConfiguration(
	int level)
{
	std::cout << std::endl << "Default configuration values:\n\n";
	
	std::ofstream html	("GinanYamlInspector.html");
	std::ofstream md	("defaultConfiguration.md");

	html << 
	#include "htmlHeaderTemplate.html"
	<< std::endl;
	
	auto it = acsConfig.yamlDefaults.begin();
	
	Indentor indentor;
	Indentor htmlIndentor;
	Indentor mdIndentor('#', 1);
	mdIndentor++;
	md << std::endl << mdIndentor << " Default Configuration" << std::endl;
	
	outputDefaultSiblings(level, html, md, it, indentor, htmlIndentor, mdIndentor);

	std::cout << std::endl << std::endl;
	
	html << 
	#include "htmlFooterTemplate.html"
	<< std::endl;
	
	md << std::endl << mdIndentor << " Enum Details" << std::endl;
	
	for (auto& [enumName, details] : enumDetailsMap)
	{
		md << std::endl << "---";
		
		md << std::endl << std::endl << "### " + enumName;
		
		md << std::endl << std::endl << "Valid enum values are:";
		for (auto& value : details.enums)
		{
			md << std::endl << "- `" << value << "`";
			
			if (docs[value].empty() == false)
				md << " : " << docs[value];
		}
		
		md << std::endl << std::endl << "For options:" << std::endl;
		for (auto& caller : details.usingOptions)
		{
			string dummy;
			md << std::endl << "- [`" << caller << "`](#" << nonNumericStack(caller, dummy, false) << ")";
		}
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

	("help,h",																					"Help")
	("quiet,q",																					"Less output")
	("verbose,v",																				"More output")
	("very-verbose,V",																			"Much more output")
	("yaml-defaults,Y",			boost::program_options::value<int>(),							"Print set of parsed parameters and their default values according to their priority level (1-3), and generate configurator.html for visual editing of yaml files")
	("config_description,d",	boost::program_options::value<string>(),						"Configuration description")
	("level,l",					boost::program_options::value<int>(),							"Trace level")
	("fatal_message_level,L",	boost::program_options::value<int>(),							"Fatal error level")
	("elevation_mask,e",		boost::program_options::value<float>(), 						"Elevation Mask")
	("max_epochs,n",			boost::program_options::value<int>(),							"Maximum Epochs")
	("epoch_interval,i",		boost::program_options::value<float>(), 						"Epoch Interval")
	("user,u",					boost::program_options::value<string>(),						"Username for RTCM streams")
	("pass,p",					boost::program_options::value<string>(),						"Password for RTCM streams")
	("config,y",				boost::program_options::value<vector<string>>()->multitoken(),	"Configuration file")
	("atx_files",				boost::program_options::value<vector<string>>()->multitoken(),	"ANTEX files")
	("nav_files",				boost::program_options::value<vector<string>>()->multitoken(),	"Navigation files")
	("snx_files",				boost::program_options::value<vector<string>>()->multitoken(),	"SINEX files")
	("sp3_files",				boost::program_options::value<vector<string>>()->multitoken(),	"Orbit (SP3) files")
	("clk_files",				boost::program_options::value<vector<string>>()->multitoken(),	"Clock (CLK) files")
	("obx_files",				boost::program_options::value<vector<string>>()->multitoken(),	"ORBEX (OBX) files")
	("dcb_files",				boost::program_options::value<vector<string>>()->multitoken(),	"Code Bias (DCB) files")
	("bsx_files",				boost::program_options::value<vector<string>>()->multitoken(),	"Bias Sinex (BSX) files")
	("ion_files",				boost::program_options::value<vector<string>>()->multitoken(),	"Ionosphere (IONEX) files")
	("igrf_files",				boost::program_options::value<vector<string>>()->multitoken(),	"Geomagnetic field coefficients (IGRF) file")
	("orb_files",				boost::program_options::value<vector<string>>()->multitoken(),	"Orbits (POD) files")
	("blq_files",				boost::program_options::value<vector<string>>()->multitoken(),	"BLQ (Ocean loading) files")
	("erp_files",				boost::program_options::value<vector<string>>()->multitoken(),	"ERP files")
// 	("rnx_inputs,r",			boost::program_options::value<vector<string>>()->multitoken(),	"RINEX station inputs")
// 	("ubx_inputs",				boost::program_options::value<vector<string>>()->multitoken(),	"UBX station inputs")
// 	("rtcm_inputs",				boost::program_options::value<vector<string>>()->multitoken(),	"RTCM station inputs")
	("egm_files",				boost::program_options::value<vector<string>>()->multitoken(),	"Earth gravity model coefficients file")
	("crd_files",				boost::program_options::value<vector<string>>()->multitoken(),	"SLR CRD file")
// 	("slr_inputs",				boost::program_options::value<vector<string>>()->multitoken(),	"Tabular SLR OBS station file")
	("jpl_files",				boost::program_options::value<vector<string>>()->multitoken(),	"JPL planetary and lunar ephemerides file")
// 	("root_input_directory",	boost::program_options::value<string>(),						"Directory containg the input data")
// 	("root_output_directory",	boost::program_options::value<string>(),						"Output directory")
	("start_epoch",				boost::program_options::value<string>(),						"Start date/time")
	("end_epoch",				boost::program_options::value<string>(),						"Stop date/time")
// 	("run_rts_only",			boost::program_options::value<string>(),						"RTS filename (without _xxxxx suffix)")
	("dump-config-only",																		"Dump the configuration and exit")
// 	("input_persistance",																		"Begin with previously stored filter and navigation states")
// 	("output_persistance",																		"Store filter and navigation states for restarting")
	("walkthrough",																				"Run demonstration code interactively with commentary")
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
	
	if (vm.count("walkthrough"))
	{
		walkthrough();
		exit(EXIT_SUCCESS);
	}

	if (vm.count("very-verbose"))	{	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::debug);		}
	if (vm.count("verbose"))		{	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::info);		}
	if (vm.count("quiet"))			{	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::warning);	}

	if (vm.count("yaml-defaults"))
	{
		acsConfig.parse({""}, vm);
		
		exit(EXIT_SUCCESS);	
	}
	
	if (vm.count("config"))
	{
		vector<string> configs = vm["config"].as<vector<string>>();

		globber(configs);

		bool pass = acsConfig.parse(configs, vm);

		if (!pass)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error: Configuration aborted";

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
	valid &= checkValidFiles(acsConfig.obx_files, 			"orbex file (OBX file)");
	valid &= checkValidFiles(acsConfig.blq_files, 			"ocean loading information (Blq file)");
	valid &= checkValidFiles(acsConfig.erp_files,			"earth rotation parameter file (ERP file)");
	valid &= checkValidFiles(acsConfig.dcb_files,			"code Biases file (DCB file)");
	valid &= checkValidFiles(acsConfig.bsx_files,			"bias Sinex file (BSX file)");
	valid &= checkValidFiles(acsConfig.ion_files,			"Ionosphere (IONEX file)");
	valid &= checkValidFiles(acsConfig.igrf_files,			"geomagnetic field coefficients (IGRF file)");
	valid &= checkValidFiles(acsConfig.atx_files, 			"antenna information (ANTEX file)");
	valid &= checkValidFiles(acsConfig.orb_files, 			"orbit determination (pod file)");
	valid &= checkValidFiles(acsConfig.egm_files, 			"Earth gravity model coefficients (egm file)");
	valid &= checkValidFiles(acsConfig.jpl_files, 			"JPL planetary and lunar ephemerides (jpl file)");
	valid &= checkValidFiles(acsConfig.tide_files, 			"Ocean tide file FES (tide file)");
	valid &= checkValidFiles(acsConfig.sid_files, 			"satellite ID list");
	valid &= checkValidFiles(acsConfig.com_files, 			"centre-of-mass (com file)");
	valid &= checkValidFiles(acsConfig.crd_files, 			"SLR observation files (crd file)");
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
	ss << "\n\n";
	ss << "===============================\n";
	ss << "Configuration...\n";
	ss << "===============================\n";
	ss << "Inputs:\n";
	if (!nav_files				.empty())	{	ss << "\tnav_files:   ";												for (auto& a : nav_files)		ss << a << " ";		ss << "\n";		}
	if (!snx_files				.empty())	{	ss << "\tsnx_files:   ";												for (auto& a : snx_files)		ss << a << " ";		ss << "\n";		}
	if (!atx_files				.empty())	{	ss << "\tatx_files:   ";												for (auto& a : atx_files)		ss << a << " ";		ss << "\n";		}
	if (!dcb_files				.empty())	{	ss << "\tdcb_files:   ";												for (auto& a : dcb_files)		ss << a << " ";		ss << "\n";		}
	if (!clk_files				.empty())	{	ss << "\tclk_files:   ";												for (auto& a : clk_files)		ss << a << " ";		ss << "\n";		}
	if (!bsx_files				.empty())	{	ss << "\tbsx_files:   ";												for (auto& a : bsx_files)		ss << a << " ";		ss << "\n";		}
	if (!ion_files				.empty())	{	ss << "\tion_files:   ";												for (auto& a : ion_files)		ss << a << " ";		ss << "\n";		}
	if (!igrf_files				.empty())	{	ss << "\tigrf_files:  ";												for (auto& a : igrf_files)		ss << a << " ";		ss << "\n";		}
	if (!blq_files				.empty())	{	ss << "\tblq_files:   ";												for (auto& a : blq_files)		ss << a << " ";		ss << "\n";		}
	if (!erp_files				.empty())	{	ss << "\terp_files:   ";												for (auto& a : erp_files)		ss << a << " ";		ss << "\n";		}
	if (!sp3_files				.empty())	{	ss << "\tsp3_files:   ";												for (auto& a : sp3_files)		ss << a << " ";		ss << "\n";		}
	if (!obx_files				.empty())	{	ss << "\tobx_files:   ";												for (auto& a : obx_files)		ss << a << " ";		ss << "\n";		}
	if (!orb_files				.empty())	{	ss << "\torb_files:   ";												for (auto& a : orb_files)		ss << a << " ";		ss << "\n";		}
	if (!egm_files				.empty())	{	ss << "\tegm_files:   ";												for (auto& a : egm_files)		ss << a << " ";		ss << "\n";		}
 	if (!jpl_files				.empty())	{	ss << "\tjpl_files:   ";												for (auto& a : jpl_files)		ss << a << " ";		ss << "\n";		}
	if (!tide_files				.empty())	{	ss << "\ttide_files:  ";												for (auto& a : tide_files)		ss << a << " ";		ss << "\n";		}
	if (!sid_files				.empty())	{	ss << "\tsid_files:   ";												for (auto& a : sid_files)		ss << a << " ";		ss << "\n";		}
	if (!vmf_files				.empty())	{	ss << "\tvmf_files:   ";												for (auto& a : vmf_files)		ss << a << " ";		ss << "\n";		}
	if (!com_files				.empty())	{	ss << "\tcom_files:   ";												for (auto& a : com_files)		ss << a << " ";		ss << "\n";		}
	if (!crd_files				.empty())	{	ss << "\tcrd_files:   ";												for (auto& a : crd_files)		ss << a << " ";		ss << "\n";		}
	if (!nav_rtcm_inputs		.empty())	{	ss << "\trtcm_inputs: ";												for (auto& a : nav_rtcm_inputs)	ss << a << " ";		ss << "\n";		}
	if (!rnx_inputs				.empty())	{	ss << "\trnx_inputs:  ";		for (auto& [z, A] : rnx_inputs)			for (auto& a : A)				ss << a << " ";		ss << "\n";		}
	if (!pseudo_sp3_inputs		.empty())	{	ss << "\tsp3_inputs:  ";		for (auto& [z, A] : pseudo_sp3_inputs)	for (auto& a : A)				ss << a << " ";		ss << "\n";		}
	if (!pseudo_snx_inputs		.empty())	{	ss << "\tsnx_inputs:  ";		for (auto& [z, A] : pseudo_snx_inputs)	for (auto& a : A)				ss << a << " ";		ss << "\n";		}
	if (!obs_rtcm_inputs		.empty())	{	ss << "\trtcm_inputs: ";		for (auto& [z, A] : obs_rtcm_inputs)	for (auto& a : A)				ss << a << " ";		ss << "\n";		}
								
	if (!model.trop.orography	.empty())		ss << "\torography:   " << model.trop.orography 	<< "\n";
	if (!model.trop.gpt2grid	.empty())		ss << "\tgrid:        " << model.trop.gpt2grid 		<< "\n";
	if (!test_filename			.empty())		ss << "\ttestfiles:   " << test_filename			<< "\n";
												ss << "\n";

	ss << "Outputs:\n";
	if (1)									{	ss << "\ttrace level:                   " << trace_level 					<< "\n"; }
	if (output_satellite_trace)				{	ss << "\tsatellite trace filename:      " << satellite_trace_filename 		<< "\n"; }
	if (output_station_trace)				{	ss << "\tstation trace filename:        " << station_trace_filename 		<< "\n"; }
	if (output_json_trace)					{	ss << "\tjson trace filename:           " << station_trace_filename 		<< "\n"; }
	if (output_network_trace)				{	ss << "\tnetwork trace filename:        " << network_trace_filename 		<< "\n"; }
	if (output_clocks)						{	ss << "\tclocks filename:               " << clocks_filename 				<< "\n"; }
	if (output_ionex)						{	ss << "\tionex filename:                " << ionex_filename 				<< "\n"; }
	if (output_ionstec)						{	ss << "\tionstec filename:              " << ionstec_filename 				<< "\n"; }
	if (output_bias_sinex)					{	ss << "\tbias sinex filename:           " << bias_sinex_filename			<< "\n"; }
	if (output_cost)						{	ss << "\tcost filename:                 " << cost_filename					<< "\n"; }
	if (output_trop_sinex)					{	ss << "\ttrop sinex filename:           " << trop_sinex_filename			<< "\n"; }
	if (output_gpx)							{	ss << "\tgpx filename:                  " << gpx_filename					<< "\n"; }
	if (output_decoded_rtcm_json)			{	ss << "\tdecoded rtcm json filename:    " << decoded_rtcm_json_filename		<< "\n"; }
	if (output_encoded_rtcm_json)			{	ss << "\tencoded rtcm json filename:    " << encoded_rtcm_json_filename		<< "\n"; }

	ss << "\n";

	ss << "Process Modes:\n";
	ss << "\tPreprocessor:        " << process_preprocessor			<< "\n";
	ss << "\tSPP                  " << process_spp					<< "\n";
	ss << "\tPPP:                 " << process_ppp					<< "\n";
	ss << "\tMinimum Constraints: " << process_minimum_constraints 	<< "\n";
	ss << "\tIonospheric:         " << process_ionosphere 			<< "\n";
	ss << "\tRTS Smoothing:       " << process_rts 					<< "\n";
	ss << "\n";

	ss << "Systems:\n";
	ss << "\tGPS:     " << process_sys[E_Sys::GPS] 		<< "\n";
	ss << "\tGLONASS: " << process_sys[E_Sys::GLO] 		<< "\n";
	ss << "\tGALILEO: " << process_sys[E_Sys::GAL] 		<< "\n";
	ss << "\tBEIDOU:  " << process_sys[E_Sys::BDS] 		<< "\n";
	ss << "\tQZSS:    " << process_sys[E_Sys::QZS] 		<< "\n";
	ss << "\tLEO:     " << process_sys[E_Sys::LEO] 		<< "\n";
	ss << "\n";
	
	ss << "Elevation_mask: " << elevation_mask * R2D 	<< "\n";
	ss << "\n";

	ss << "Epochs:\n";
	if (epoch_interval	> 0)					{	ss << "\tepoch_interval: " << epoch_interval	<< "\n";	}
	if (max_epochs		> 0)					{	ss << "\tmax_epochs:     " << max_epochs		<< "\n";	}
	if (!start_epoch	.is_not_a_date_time())	{	ss << "\tepoch start:    " << start_epoch		<< "\n";	}
	if (!end_epoch		.is_not_a_date_time())	{	ss << "\tepoch end:      " << end_epoch			<< "\n";	}

	ss << "\n";
	ss << "===============================\n";
	ss << "...End Configuration\n";
	ss << "===============================\n";
	ss << "\n";
	
	if (process_user)		BOOST_LOG_TRIVIAL(warning) << "Warning: 'process_modes: user:'    is being deprecated. Consider using ppp mode instead";
	if (process_network)	BOOST_LOG_TRIVIAL(warning) << "Warning: 'process_modes: network:' is being deprecated. Consider using ppp mode instead";
}

void addAvailableOptions(
	string stack)
{
	string dummy;
	auto& available = acsConfig.availableOptions[nonNumericStack(stack, dummy)];

	if (available)
	{
		//already recursed this bit
		return;
	}

	available = true;

	auto pos = stack.find_last_of(':', stack.size() - 2);

	if (pos == string::npos)
	{
		return;
	}

	addAvailableOptions(stack.substr(0, pos + 1));
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
	
		if (desc.empty())
		{
			continue;
		}
		
		string dummy;
		string shortDesc = nonNumericStack(desc, dummy, false);

		bool test = false;
		if (currentNode[shortDesc])
			test = true;

		currentNode.reset(currentNode[shortDesc]);
		stack += desc + ":";
		
// 		string test = currentNode.Scalar();

		if (acsConfig.yamlDefaults.find(stack) == acsConfig.yamlDefaults.end())
		{
			if (i == yamlNodeDescriptor.size() - 1)		acsConfig.yamlDefaults[stack] = {defaultValue, comment};
			else										acsConfig.yamlDefaults[stack] = {"", ""};
														acsConfig.yamlDefaults[stack].found = test;
		}
	}
	
	addAvailableOptions(stack);
	
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
	
	addAvailableOptions(stack);
	
	auto& yamlDefault = acsConfig.yamlDefaults[stack];
	try
	{
		yamlDefault.foundValue	= yamlDefault.defaultValue;
		
		output = optNode.template as<TYPE>();
		
		yamlDefault.foundValue	= stringify(output);
		yamlDefault.found		= true;
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
	string dummy;
	string name = nodeDescriptor.back();
	name = nonNumericStack(name, dummy, false);
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

template<typename ENUM>
void addEnumDetails(
	string& stack)
{
	string enumName = ENUM::_name();
	
	auto names = ENUM::_names();
	
	if (enumDetailsMap.find(enumName) == enumDetailsMap.end())
	for (int i = 0; i < ENUM::_size(); i++)
	{
		string enumOption = boost::algorithm::to_lower_copy((string) names[i]);
		enumDetailsMap[enumName].enums.push_back(enumOption);
	}
	
	string dummy;
	
	enumDetailsMap[enumName].usingOptions.push_back(nonNumericStack(stack, dummy));
	acsConfig.yamlDefaults[stack].enumName = enumName;
}

/** Set an enum from yaml, decoding strings to ints
*/
template <typename ENUM>
bool trySetEnumOpt(
	ENUM&			out,									///< Variable to output to
	NodeStack		yamlBase,								///< Yaml node to search within
	vector<string>	yamlNodeDescriptor,						///< List of strings of keys to trace hierarcy
	ENUM			(&_from_string_nocase)(const char*),	///< Function to decode enum strings
	string			comment = "")							///< Description to provide to user for automatic documentation
{
	string enumOptions = " {";

	auto names = ENUM::_names();
	for (int i = 0; i < ENUM::_size(); i++)
	{
		string enumOption = boost::algorithm::to_lower_copy((string) names[i]);
		
		if (i != 0)
			enumOptions += ",";
		enumOptions += enumOption;
	}
	
	enumOptions += "}";

	auto [optNode, stack] = stringsToYamlObject(yamlBase, yamlNodeDescriptor, comment + enumOptions, out._to_string());
	
	addAvailableOptions(stack);
	
	addEnumDetails<ENUM>(stack);
	
	string value;
	try
	{
		value = optNode.template as<string>();
	}
	catch (...) 
	{
// 		std::cout << stack << " not found\n";
		return false;
	}
	
	try
	{
		out = _from_string_nocase(value.c_str());
		return true;
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

template <typename ENUM>
bool trySetEnumVec(
	vector<ENUM>&	enumVector,
	NodeStack		yamlBase,						///< Yaml node to search within
	vector<string>	yamlNodeDescriptor,				///< List of strings of keys to trace hierarcy
	string			comment = "")					///< Description to provide to user for automatic documentation
{
	auto [optNode, stack] = stringsToYamlObject(yamlBase, yamlNodeDescriptor, comment, stringify(enumVector));
	
	vector<string> enumStrings;
	bool found = trySetFromYaml(enumStrings, yamlBase, yamlNodeDescriptor, comment);
	
	addEnumDetails<ENUM>(stack);
	
	if (found == false)
		return false;
	
	enumVector.clear();
	

	for (auto& enumString : enumStrings)
	{
		try
		{
			auto a = ENUM::_from_string_nocase(enumString.c_str());
			enumVector.push_back(a);
		}
		catch (...)
		{
			continue;
		}
	}
	
	return true;
}

/** Set the variables associated with kalman filter states from yaml
*/
void trySetKalmanFromYaml(
	KalmanModel&	output,					///< Variable to output to
	NodeStack&		yaml,					///< Yaml node to search within
	string			key,					///< Key of yaml object
	string			comment		= "",		///< Description to provide to user for automatic documentation
	bool			skippable	= false)	///< Optionally skip this when yaml object not found in file
{	
	auto newYaml = stringsToYamlObject(yaml, {key}, comment);
	
	auto& [optNode, stack] = newYaml;
	
	if	(  skippable
		&& !optNode)
	{
		return;
	}
	
	E_Period proc_noise_dt = E_Period::SECOND;
	setInited(output,	output.estimate,	trySetFromYaml(output.estimate, 	newYaml, {"0! estimated"		}, "[bools] Estimate state in kalman filter"));
											trySetEnumOpt(proc_noise_dt,		newYaml, {"2@ proc_noise_dt"	}, E_Period::_from_string_nocase, "(enum) Time unit for process noise - sqrt_sec, sqrt_day etc.");

	setInited(output,	output.sigma,		trySetFromYaml(output.sigma,		newYaml, {"1! sigma" 			}, "[floats] Apriori sigma values - if zero, will be initialised using least squares"));
	setInited(output,	output.apriori_val,	trySetFromYaml(output.apriori_val,	newYaml, {"3! apriori_val"		}, "[floats] Apriori state values"));
	setInited(output,	output.proc_noise,	trySetFromYaml(output.proc_noise,	newYaml, {"2! proc_noise"	 	}, "[floats] Process noise sigmas"));
	setInited(output,	output.proc_noise,	trySetFromYaml(output.proc_noise,	newYaml, {"2  process_noise" 	}, "[floats] Process noise sigmas"));
	setInited(output,	output.tau,			trySetFromYaml(output.tau,			newYaml, {"@ tau"				}, "[floats] Correlation times for gauss markov noise, defaults to -1 -> inf (Random Walk)"));
	setInited(output,	output.mu,			trySetFromYaml(output.mu,			newYaml, {"@ mu"				}, "[floats] Desired mean value for gauss markov states"));
	setInited(output,	output.comment,		trySetFromYaml(output.comment,		newYaml, {"@ comment"			}, "[strings] Comment to apply to the state"));
	
	if (isInited(output, output.proc_noise))
	{
		for (auto& proc : output.proc_noise)
		{
			proc /= sqrt((int)proc_noise_dt);
		}
	}
	
	if (isInited(output, output.tau))
	{
		for (auto& tau : output.tau)
		{
			tau *= (int)proc_noise_dt;
		}
	}
}

void tryGetMappedList(
	map<string, vector<string>>&	mappedList,
	NodeStack&						yaml,
	string							key,
	string							comment = "")
{
	auto [outStreamNode, outStreamString] = stringsToYamlObject(yaml, {key});

	for (auto outLabelYaml : outStreamNode)
	{
		if (outLabelYaml.IsScalar())
		{
			string value = outLabelYaml.as<string>();
			
			int keyPos = 0;
			int lastSlash = value.find_last_of('/');
			
			if (lastSlash != string::npos)
			{
				keyPos = lastSlash + 1;
			}
		
			mappedList["<AUTO>"].push_back(value);
		}
		if (outLabelYaml.IsMap())
		{
			for (auto it = outLabelYaml.begin(); it != outLabelYaml.end(); it++)
			{
				string key = it->first.as<string>();
				
				mappedList[key] = it->second.as<vector<string>>();
			}
		}
	}
}


/** Set the variables associated with an output stream
*/
void tryGetStreamFromYaml(
	SsrBroadcast&	outStreamData,				///< Variable to output to
	NodeStack&		yaml,						///< Yaml node to search within
	string			id)							///< Label associated with the stream
{
	auto outStreamsYaml = stringsToYamlObject(yaml, {id});

	trySetFromYaml(outStreamData.url, outStreamsYaml, {"0@ url"}, "(string) Url of caster to send messages to");
	
	for (auto msgType : RtcmMessageType::_values())
	{
		if (msgType == +RtcmMessageType::IGS_SSR)
		for (auto subType : IgsSSRSubtype::_values())
		{
			string str = (boost::format("@ rtcm_%4d_%03d") % msgType._to_integral() % subType._to_integral()).str();

			auto msgOptions = stringsToYamlObject(outStreamsYaml, {"0@ messages", str},	"(int) Message type to output");

			bool found = trySetFromYaml(outStreamData.rtcmMsgOptsMap[msgType].igs_udi[subType], msgOptions, {"0 udi"},	"(int) Update interval");
			if (found)
				outStreamData.rtcmMsgOptsMap[msgType].udi = 1;
		}

		else if (msgType == +RtcmMessageType::COMPACT_SSR)
		for (auto subType : CompactSSRSubtype::_values())
		{
			string str = (boost::format("@ rtcm_%4d_%02d") % msgType._to_integral() % subType._to_integral()).str();

			auto msgOptions = stringsToYamlObject(outStreamsYaml, {"0@ messages", str},	"(int) Message type to output");

			bool found = trySetFromYaml(outStreamData.rtcmMsgOptsMap[msgType].comp_udi[subType], msgOptions, {"0@ udi"},	"(int) Update interval");
			if (found)
				outStreamData.rtcmMsgOptsMap[msgType].udi = 1;
		}

		else
		{
			string str = "@ rtcm_" + std::to_string(msgType);

			auto msgOptions = stringsToYamlObject(outStreamsYaml, {"0@ messages", str},	"(int) Message type to output");

			trySetFromYaml(outStreamData.rtcmMsgOptsMap[msgType].udi, msgOptions, {"0@ udi"},	"(int) Update interval");
		}
	}
	
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

	trySetKalmanFromYaml(satOpts.clk,					satNode, "! clk",					"Clocks");
	trySetKalmanFromYaml(satOpts.clk_rate,				satNode, "! clk_rate",				"Clock rates");
	trySetKalmanFromYaml(satOpts.pos,					satNode, "! pos",					"Position (experimental, use orb)");
	trySetKalmanFromYaml(satOpts.pos_rate,				satNode, "! pos_rate",				"Velocity (experimental, use orb)");
	trySetKalmanFromYaml(satOpts.orb,					satNode, "@ orb",					"Orbit corrections");
	trySetKalmanFromYaml(satOpts.pco,					satNode, "pco",						"Phase Center Offsets (experimental)");
	trySetKalmanFromYaml(satOpts.ant,					satNode, "ant",						"Antenna offsets (experimental)");
	trySetKalmanFromYaml(satOpts.orbit,					satNode, "! orbit",					"Orbital state");
	trySetKalmanFromYaml(satOpts.ion_model,				satNode, "@ ion_model",				"Ionosphere models");
	trySetKalmanFromYaml(satOpts.code_bias,				satNode, "! code_bias",				"Code bias (experimental)");
	trySetKalmanFromYaml(satOpts.phase_bias,			satNode, "! phase_bias",			"Phase bias (experiemental)");
	
	trySetKalmanFromYaml(satOpts.emp_dyb_0,				satNode, "! emp_dyb_0",				"emp bias ");
	trySetKalmanFromYaml(satOpts.emp_dyb_1c,			satNode, "! emp_dyb_1c",			"emp 1 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_1s,			satNode, "! emp_dyb_1s",			"emp 1 per rev sine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_2c,			satNode, "! emp_dyb_2c",			"emp 2 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_2s,			satNode, "! emp_dyb_2s",			"emp 2 per rev sine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_3c,			satNode, "! emp_dyb_3c",			"emp 3 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_3s,			satNode, "! emp_dyb_3s",			"emp 3 per rev sine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_4c,			satNode, "! emp_dyb_4c",			"emp 4 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_4s,			satNode, "! emp_dyb_4s",			"emp 4 per rev sine term ");
	                                                               
	trySetKalmanFromYaml(satOpts.srp_dyb_0,				satNode, "! srp_dyb_0",				"srp bias ");
	trySetKalmanFromYaml(satOpts.srp_dyb_1c,			satNode, "! srp_dyb_1c",			"srp 1 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.srp_dyb_1s,			satNode, "! srp_dyb_1s",			"srp 1 per rev sine term ");
	trySetKalmanFromYaml(satOpts.srp_dyb_2c,			satNode, "! srp_dyb_2c",			"srp 2 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.srp_dyb_2s,			satNode, "! srp_dyb_2s",			"srp 2 per rev sine term ");
	trySetKalmanFromYaml(satOpts.srp_dyb_3c,			satNode, "! srp_dyb_3c",			"srp 3 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.srp_dyb_3s,			satNode, "! srp_dyb_3s",			"srp 3 per rev sine term ");
	trySetKalmanFromYaml(satOpts.srp_dyb_4c,			satNode, "! srp_dyb_4c",			"srp 4 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.srp_dyb_4s,			satNode, "! srp_dyb_4s",			"srp 4 per rev sine term ");

	setInited(satOpts,	satOpts.exclude,		trySetFromYaml(satOpts.exclude, 		satNode, {"0!  exclude"			}));
	setInited(satOpts,	satOpts.code_sigmas,	trySetFromYaml(satOpts.code_sigmas,		satNode, {"0!  code_sigmas"		},					"[floats] Standard deviation of code measurements"));
	setInited(satOpts,	satOpts.phas_sigmas,	trySetFromYaml(satOpts.phas_sigmas,		satNode, {"0!  phase_sigmas"	},					"[floats] Standard deviation of phase measurmeents"));
	setInited(satOpts,	satOpts.pseudo_sigmas,	trySetFromYaml(satOpts.pseudo_sigmas,	satNode, {"0@ pseudo_sigmas"	},					"[floats] Standard deviation of pseudo measurmeents"));
	setInited(satOpts,	satOpts.laser_sigmas,	trySetFromYaml(satOpts.laser_sigmas,	satNode, {"0@ laser_sigmas"		},					"[floats] Standard deviation of SLR laser measurements"));

	satOpts._initialised = true;
}

const string estimation_parameters_str	= "4! estimation_parameters";
const string processing_options_str		= "2! processing_options";

/** Set receiver options from yaml
*/
void getFromYaml(
	ReceiverOptions&	recOpts, 			///< Receiver options variable to output to
	NodeStack			yamlBase,			///< Yaml node to search within
	vector<string>		yamlNodeDescriptor)	///< List of strings of keys of yaml hierarchy
{
	auto recNode = stringsToYamlObject(yamlBase, yamlNodeDescriptor);

	trySetKalmanFromYaml(recOpts.clk,						recNode, "! clk",					"Clocks");
	trySetKalmanFromYaml(recOpts.clk_rate,					recNode, "! clk_rate",				"Clock rates");
	trySetKalmanFromYaml(recOpts.pos,						recNode, "! pos",					"Position");
	trySetKalmanFromYaml(recOpts.pos_rate,					recNode, "! pos_rate",				"Velocity");
	trySetKalmanFromYaml(recOpts.heading,					recNode, "@ heading",				"Heading");
	trySetKalmanFromYaml(recOpts.orbit,						recNode, "@ orbit",					"Orbital state");
	trySetKalmanFromYaml(recOpts.strain_rate,				recNode, "@ strain_rate",			"Velocity (large gain, for geodetic timescales)");
	trySetKalmanFromYaml(recOpts.amb,						recNode, "! amb",					"Integer phase ambiguities");
	trySetKalmanFromYaml(recOpts.pco,						recNode, "pco",						"Phase Center Offsets (experimental)");
	trySetKalmanFromYaml(recOpts.ant,						recNode, "ant",						"Antenna offsets (experimental)");
	trySetKalmanFromYaml(recOpts.code_bias,					recNode, "! code_bias",				"Code bias (experimental)");
	trySetKalmanFromYaml(recOpts.phase_bias,				recNode, "! phase_bias",			"Phase bias (experiemental)");
	trySetKalmanFromYaml(recOpts.ion_stec,					recNode, "! ion_stec",				"Ionosphere (experimental)");
	trySetKalmanFromYaml(recOpts.slr_range_bias,			recNode, "@ slr_range_bias",		"Satellite Laser Ranging range bias");
	trySetKalmanFromYaml(recOpts.slr_time_bias,				recNode, "@ slr_time_bias",			"Satellite Laser Ranging time bias");
	trySetKalmanFromYaml(recOpts.trop,						recNode, "! trop",					"Troposphere corrections");
	trySetKalmanFromYaml(recOpts.trop_grads,				recNode, "! trop_grads",			"Troposphere gradients");

	setInited(recOpts,	recOpts.exclude,			trySetFromYaml	(recOpts.exclude, 			recNode, {"1! exclude"			}));
	setInited(recOpts,	recOpts.error_model,		trySetEnumOpt	(recOpts.error_model,		recNode, {"0@ error_model"		}, E_NoiseModel::_from_string_nocase));
	setInited(recOpts,	recOpts.spp_sigma_scaling,	trySetFromYaml	(recOpts.spp_sigma_scaling,	recNode, {"@ spp_sigma_scaling"	},					"(floats) Amount to scale sigmas for SPP"));
	setInited(recOpts,	recOpts.code_sigmas,		trySetFromYaml	(recOpts.code_sigmas,		recNode, {"0! code_sigmas"		},					"[floats] Standard deviation of code measurements"));
	setInited(recOpts,	recOpts.phas_sigmas,		trySetFromYaml	(recOpts.phas_sigmas,		recNode, {"0! phase_sigmas"		},					"[floats] Standard deviation of phase measurmeents"));
	setInited(recOpts,	recOpts.laser_sigmas,		trySetFromYaml	(recOpts.laser_sigmas,		recNode, {"@ laser_sigmas"		},					"[floats] Standard deviation of SLR laser measurements"));
}

/** Set satellite options for a specific satellite using a hierarchy of sources
*/
SatelliteOptions& ACSConfig::getSatOpts(
	SatSys			Sat,		///< Satellite to search for options for
	vector<string>	suffixes)	///< Optional suffix to get more specific versions
{
	lock_guard<mutex> guard(configMutex);

	string fullId = Sat.id();
	for (auto& suffix : suffixes)
	{
		fullId += suffix;
	}
	
	auto& satOpts = satOptsMap[fullId];

	//return early if possible
	if (satOpts._initialised)
		return satOpts;
	
	satOpts._initialised = true;

	vector<string> aliases;
	
	aliases.push_back("");
	aliases.push_back("! global");
	
	for (int i = 0; i < yamls.size(); i++)
	{
		auto& yaml = yamls[i];
		
		vector<string> yamlAliases;
		trySetFromYaml(yamlAliases, {yaml, ""}, {"! satellite_options", Sat.id(), "@ aliases"}, "[string] Aliases for this satellite");
		
		for (auto& alias : yamlAliases)
		{
			aliases.push_back(alias);
		}
	}
	
	//add global and this id on either side of the aliases
											aliases	.push_back(Sat.sysName());
	if (Sat.blockType()	.empty() == false)	aliases	.push_back(Sat.blockType());
											aliases	.push_back(Sat.id());
	if (Sat.svn()		.empty() == false)	aliases	.push_back("SVN_" + Sat.svn());

	for (int i = 0; i < yamls		.size(); i++)	{		auto& yaml = yamls[i];
	for (auto& alias	: aliases)
	for (int S = 0; S <= suffixes	.size(); S++)
	{
		boost::trim_right(alias);
		
		string aliasName = std::to_string(i) + alias;
		
		for (int s = 0; s < S; s++)
		{
			aliasName += suffixes[s];
		}
		
		auto& aliasOpts = satOptsMap[aliasName];
		
		if (aliasOpts._initialised)
		{
			satOpts += aliasOpts;
			continue;
		}
		
		aliasOpts._initialised = true;
		
		vector<string> descriptorVec = {estimation_parameters_str, "0! satellites", alias};
		for (int s = 0; s < S; s++)
		{
			descriptorVec.push_back(suffixes[s]);
		}
		
		getFromYaml(aliasOpts, {yaml, ""}, descriptorVec);

		if (alias.empty() == false)
		{
			vector<double>	antenna_boresight;
			vector<double>	antenna_azimuth;
			
			descriptorVec = {"@ satellite_options", alias};
			for (int s = 0; s < S; s++)
			{
				descriptorVec.push_back(suffixes[s]);
			}
			
			{	auto vec = descriptorVec; vec.push_back("@ antenna_boresight");		trySetFromYaml	(antenna_boresight,			{yaml, ""}, vec,	"[floats] Antenna boresight (Up) in satellite body-fixed frame");	}
			{	auto vec = descriptorVec; vec.push_back("@ antenna_azimuth");		trySetFromYaml	(antenna_azimuth,			{yaml, ""}, vec,	"[floats] Antenna azimuth (North) in satellite body-fixed frame");	}
			
			if (antenna_boresight	.size()	== 3)	{	aliasOpts.antenna_boresight		= Vector3d(antenna_boresight.data());		setInited(aliasOpts,	aliasOpts.antenna_boresight);	}	
			if (antenna_azimuth		.size()	== 3)	{	aliasOpts.antenna_azimuth		= Vector3d(antenna_azimuth	.data());		setInited(aliasOpts,	aliasOpts.antenna_azimuth);	}	
		}
		
		satOpts += aliasOpts;
	}}
	
	return satOpts;
}

/** Set receiver options for a specific receiver using a hierarchy of sources
*/
ReceiverOptions& ACSConfig::getRecOpts(
	string			id,			///< Receiver to search for options for
	vector<string>	suffixes)	///< Optional suffix to get more specific versions
{
	lock_guard<mutex> guard(configMutex);

	string fullId = id;
	for (auto& suffix : suffixes)
	{
		fullId += suffix;
	}
	
	auto& recOpts = recOptsMap[fullId];

	//return early if possible
	if (recOpts._initialised)
		return recOpts;
	
	recOpts._initialised = true;

	vector<string> aliases;
	
	aliases.push_back("");
	aliases.push_back("! global");
	
	for (int i = 0; i < yamls.size(); i++)
	{
		auto& yaml = yamls[i];
		
		vector<string> yamlAliases;
		trySetFromYaml(yamlAliases, {yaml, ""}, {"! station_options", id, "@ aliases"}, "[string] Aliases for this station");
		
		for (auto& alias : yamlAliases)
		{
			aliases.push_back(alias);
		}
	}
	
	//add global and this id on either side of the aliases
	aliases.push_back(id);

	for (int i = 0; i < yamls		.size(); i++)	{						auto& yaml = yamls[i];
	for (auto& alias	: aliases)
	for (int S = 0; S <= suffixes	.size(); S++)
	{
		string aliasName = std::to_string(i) + alias;
		
		for (int s = 0; s < S; s++)
		{
			aliasName += suffixes[s];
		}
		
		auto& aliasOpts = recOptsMap[aliasName];
		
		if (aliasOpts._initialised)
		{
			recOpts += aliasOpts;
			continue;
		}
		
		aliasOpts._initialised = true;
		
		vector<string> descriptorVec = {estimation_parameters_str, "0! stations", alias};
		for (int s = 0; s < S; s++)
		{
			descriptorVec.push_back(suffixes[s]);
		}
		
		getFromYaml(aliasOpts, {yaml, ""}, descriptorVec);

		if (alias.empty() == false)
		{
			vector<double>	eccentricity;
			vector<double>	apriori_pos;
			vector<double>	antenna_boresight;
			vector<double>	antenna_azimuth;
			
			setInited(aliasOpts,	aliasOpts.minConNoise,		trySetFromYaml(aliasOpts.minConNoise, {yaml, ""}, {processing_options_str, "minimum_constraints", "station_noise", alias}, "(float) Sigma applied to all stations for weighting in transformation estimation. (Lower is stronger weighting, Negative is unweighted, in ENU frame)"));

			descriptorVec = {"@ station_options", alias};
			for (int s = 0; s < S; s++)
			{
				descriptorVec.push_back(suffixes[s]);
			}
			
			{	auto vec = descriptorVec; vec.push_back("@ sat_id");				setInited(aliasOpts,	aliasOpts.sat_id,			trySetFromYaml	(aliasOpts.sat_id,			{yaml, ""}, vec,	"(string) "));	}
			{	auto vec = descriptorVec; vec.push_back("@ receiver_type");			setInited(aliasOpts,	aliasOpts.receiver_type,	trySetFromYaml	(aliasOpts.receiver_type,	{yaml, ""}, vec,	"(string) "));	}
			{	auto vec = descriptorVec; vec.push_back("@ antenna_type");			setInited(aliasOpts,	aliasOpts.antenna_type,		trySetFromYaml	(aliasOpts.antenna_type,	{yaml, ""}, vec,	"(string) Antenna type and radome in 20 character string as per sinex"));	}				
			{	auto vec = descriptorVec; vec.push_back("@ eccentricity");																trySetFromYaml	(eccentricity,				{yaml, ""}, vec,	"[floats] Antenna offset in ENU frame");}
			{	auto vec = descriptorVec; vec.push_back("@ apriori_position");															trySetFromYaml	(apriori_pos,				{yaml, ""}, vec,	"[floats] Apriori position in XYZ ECEF frame");}
			{	auto vec = descriptorVec; vec.push_back("@ antenna_boresight");															trySetFromYaml	(antenna_boresight,			{yaml, ""}, vec,	"[floats] Antenna boresight (Up) in receiver body-fixed frame");}
			{	auto vec = descriptorVec; vec.push_back("@ antenna_azimuth");															trySetFromYaml	(antenna_azimuth,			{yaml, ""}, vec,	"[floats] Antenna azimuth (North) in receiver body-fixed frame");}
			
			if (eccentricity		.size()	== 3)	{	aliasOpts.eccentricity		= Vector3d(eccentricity		.data());		setInited(aliasOpts,	aliasOpts.eccentricity);		}
			if (apriori_pos			.size()	== 3)	{	aliasOpts.apriori_pos		= Vector3d(apriori_pos		.data());		setInited(aliasOpts,	aliasOpts.apriori_pos);			}
			if (antenna_boresight	.size()	== 3)	{	aliasOpts.antenna_boresight	= Vector3d(antenna_boresight.data());		setInited(aliasOpts,	aliasOpts.antenna_boresight);	}
			if (antenna_azimuth		.size()	== 3)	{	aliasOpts.antenna_azimuth	= Vector3d(antenna_azimuth	.data());		setInited(aliasOpts,	aliasOpts.antenna_azimuth);		}
					
			for (int i = E_Sys::GPS; E_Sys::_values()[i] < +E_Sys::SUPPORTED; i++)
			for (E_ObsCode2 obsCode2 : E_ObsCode2::_values())
			{
				E_Sys sys = E_Sys::_values()[i];

				auto& rinex3Code	= aliasOpts.rinex23Conv.codeConv[sys][obsCode2];
				auto& rinex3Phas	= aliasOpts.rinex23Conv.phasConv[sys][obsCode2];

				string sysName		= boost::algorithm::to_lower_copy((string) sys._to_string());

				{	auto vec = descriptorVec; vec.push_back("@ rnx_code_conversions");	trySetEnumOpt(rinex3Code, stringsToYamlObject(	{yaml, ""}, vec), {sysName, obsCode2._to_string()}, E_ObsCode::_from_string_nocase);}
				{	auto vec = descriptorVec; vec.push_back("@ rnx_phase_conversions");	trySetEnumOpt(rinex3Phas, stringsToYamlObject(	{yaml, ""}, vec), {sysName, obsCode2._to_string()}, E_ObsCode::_from_string_nocase);}
			}
		}
		
		recOpts += aliasOpts;
	}}
	
	return recOpts;
}

/** Set and scale a variable according to yaml options
*/
template<typename ENUM>
void trySetScaledFromYaml(
	double&			output,									///< Variable to output to
	NodeStack		node,									///< Yaml node to search within
	vector<string>	number_parameter,						///< List of keys of the hierarchy to the value to be set
	vector<string>	scale_parameter,						///< List of keys of the hierarchy to the scale to be applied
	ENUM			(&_from_string_nocase)(const char*))	///< Function to decode scale enum strings
{
	double	number			= output;
	ENUM	number_units	= ENUM::_from_integral(1);
	
	trySetFromYaml	(number,		node, number_parameter);
	trySetEnumOpt	(number_units, 	node, scale_parameter,	_from_string_nocase);
	
	number *= (int)number_units;
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
			<< "Warning: " << subStr << " is used in config but is not defined, clearing...";
			
			str.clear();
			return true;
		}

		str.erase	(index, subStr.size());
		str.insert(index, replacement);
		
		replaced = true;
	}
	
	return replaced;
}

/** Replace macros for times with configured values.
* Available replacements are "<CONFIG> <USER> <PASS> <BRANCH> <HASH> <AGENCY> <SOFTWARE>"
*/
void replaceTags(
	string&						str)		///< String to replace macros within
{
	replaceString(str, "<CONFIG>",		acsConfig.config_description);
	replaceString(str, "<USER>",		acsConfig.stream_user);
	replaceString(str, "<PASS>",		acsConfig.stream_pass);
	replaceString(str, "<BRANCH>",		ginanBranchName());
	replaceString(str, "<HASH>",		ginanCommitHash());
	replaceString(str, "<AGENCY>",		acsConfig.analysis_agency);
	replaceString(str, "<SOFTWARE>",	acsConfig.analysis_program.substr(0,3));
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
	vector<string>& strs)
{
	for (auto& str : strs)
	{
		replaceTags(str);
	}
}

void replaceTags(
	map<string, vector<string>>& strs)
{
	for (auto& [id, str] : strs)
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

void ACSConfig::recurseYaml(
	YAML::Node	node,
	string		stack,
	string		aliasStack)
{
	for (YAML::const_iterator it = node.begin(); it != node.end(); it++)
	{
		string key = it->first.as<string>();
		
// 		std::cout << key << std::endl;
		
		string newStack			= stack			+ key + ":";
		string newAliasStack	= aliasStack	+ key + ":";
		
		if (availableOptions.find(newAliasStack) == availableOptions.end())
		{
			if	( (stack == "estimation_parameters:stations:")
				||(stack == "estimation_parameters:satellites:")
				||(stack == "processing_options:minimum_constraints:station_noise:")
				||(stack == "outputs:streams:")
				||(stack == "station_options:"))
			{
				newAliasStack = stack + "global" + ":";

				if (availableOptions.find(newAliasStack) == availableOptions.end())
				{
					BOOST_LOG_TRIVIAL(warning)
					<< "Warning: " << newStack << " is not a valid yaml option";

					continue;
				}
			}
			else
			{
				BOOST_LOG_TRIVIAL(warning)
				<< "Warning: " << newStack << " is not a valid yaml option";

				continue;
			}
		}

//		BOOST_LOG_TRIVIAL(debug)
//		<< newStack << " is a valid yaml option";

		if (node[key].IsMap())
		{
			recurseYaml(node[key], newStack, newAliasStack);
		}
	}
}

bool ACSConfig::parse()
{
	return parse(configFilenames, commandOpts);
}

/** Parse options to set acsConfig values.
* Command line options will override any values set in config files, which will themselves override any program default values.
*/
bool ACSConfig::parse(
	vector<string>							filenames,		///< Path to yaml based config file
	boost::program_options::variables_map&	newCommandOpts)	///< Variable map object of command line options
{
	configFilenames = filenames;

	bool modified = false;

	for (auto& filename : filenames)
	if (filename != "")
	{
		boost::filesystem::path filePath(filename);
		auto currentConfigModifyTime = boost::filesystem::last_write_time(filePath);
		
		if (currentConfigModifyTime != configModifyTimeMap[filename])
		{
			modified = true;
		}

		configModifyTimeMap[filename] = currentConfigModifyTime;
	}
	else
	{
		modified = true;
	}

	if (modified == false)
	{
		return false;
	}

	commandOpts = newCommandOpts;

	//clear old saved parameters
	satOptsMap.clear();
	recOptsMap.clear();
	defaultOutputOptions();

	for (int i = E_Sys::GPS; i < E_Sys::SUPPORTED; i++)
	{
		E_Sys	sys			= E_Sys::_values()[i];
		
		code_priorities[sys] = code_priorities_default;
	}

	string root_output_directory	= "./";
	string root_input_directory		= "./";

	vector<string> yamlList;
	
	yamls.resize(filenames.size());
	
	for (int i = 0; i < filenames.size(); i++)
	{
		auto& filename	= filenames	[i];
		auto& yaml		= yamls		[i];

		BOOST_LOG_TRIVIAL(info)
		<< "Checking configuration file " << filename;

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
		
		vector<string> includes;
		
		auto inputs	= stringsToYamlObject({yaml, ""},	{"0 inputs"}, docs["inputs"]);

		trySetFromYaml(includes, inputs, {"include_yamls"}, "[string] List of yaml files to include before this one");
		
		for (auto& include : includes)
		{
			yamlList.push_back(include);
		}
	}
	
	for (auto& filename : filenames)
	{
		yamlList.push_back(filename);
	}
	
	yamls.resize(yamlList.size());
	
	for (int i = 0; i < yamlList.size(); i++)
	{
		auto& filename	= yamlList	[i];
		auto& yaml		= yamls		[i];

		BOOST_LOG_TRIVIAL(info)
		<< "Loading configuration from file " << filename;

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


		{
			auto outputs = stringsToYamlObject({yaml, ""}, {"1! outputs"}, docs["outputs"]);

			trySetFromYaml(root_output_directory,			outputs, {"0! root_directory"			});

			{
				auto metadata = stringsToYamlObject(outputs, 		{"! metadata"});

				trySetFromAny (config_description,	commandOpts,	metadata, {"! config_description"				}, "(string) ID for this config, used to replace <CONFIG> tags in other options");
				trySetFromAny (stream_user,			commandOpts,	metadata, {"! user"								}, "(string) Username for connecting to NTRIP casters");
				trySetFromAny (stream_pass,			commandOpts,	metadata, {"! pass"								}, "(string) Password for connecting to NTRIP casters");
				trySetFromYaml(analysis_agency,						metadata, {"@ analysis_agency"					});
				trySetFromYaml(analysis_center,						metadata, {"@ analysis_center"					});
				trySetFromYaml(ac_contact,							metadata, {"@ ac_contact"						});
				trySetFromYaml(analysis_program,					metadata, {"@ analysis_program"					});
				trySetFromYaml(analysis_program_version,			metadata, {"@ analysis_program_version"			});
				trySetFromYaml(rinex_comment,						metadata, {"@ rinex_comment"					});
				trySetFromYaml(reference_system,					metadata, {"@ reference_system"					},	"(string) Terrestrial Reference System Code");
				trySetFromYaml(time_system,							metadata, {"@ time_system"						},	"(string) Time system - e.g. \"G\", \"UTC\"");
				trySetFromYaml(bias_time_system,					metadata, {"@ bias_time_system"					},	"(string) Time system for bias SINEX \"G\", \"UTC\"");
				trySetFromYaml(ocean_tide_loading_model,			metadata, {"@ ocean_tide_loading_model"			},	"(string) Ocean tide loading model applied");
				trySetFromYaml(atmospheric_tide_loading_model,		metadata, {"@ atmospheric_tide_loading_model"	},	"(string) Atmospheric tide loading model applied");
				trySetFromYaml(geoid_model,							metadata, {"@ geoid_model"						},	"(string) Geoid model name for undulation values");
				trySetFromYaml(gradient_mapping_function,			metadata, {"@ gradient_mapping_function"		},	"(string) Name of mapping function used for mapping horizontal troposphere gradients");
			}

			{
				auto trace = stringsToYamlObject(outputs, {"0! trace"});

				trySetFromYaml(output_station_trace,		trace, {"0! output_stations"		}, "(bool) ");
				trySetFromYaml(output_network_trace,		trace, {"0! output_network"			}, "(bool) ");
				trySetFromYaml(output_satellite_trace,		trace, {"0! output_satellites"		}, "(bool) ");
				trySetFromYaml(trace_directory,				trace, {"! directory"				});
				trySetFromYaml(satellite_trace_filename,	trace, {"1! satellite_filename"		});
				trySetFromYaml(station_trace_filename,		trace, {"1! station_filename"		});
				trySetFromYaml(network_trace_filename,		trace, {"1! network_filename"		});
				trySetFromAny(trace_level, commandOpts,		trace, {"! level"					}, "(int) Threshold level for printing messages (0-5)");

				trySetFromYaml(output_residual_chain,		trace, {"! output_residual_chain"	}, "(bool) ");
				trySetFromYaml(output_residuals,			trace, {"! output_residuals"		}, "(bool) ");
				trySetFromYaml(output_config,				trace, {"! output_config"			}, "(bool) ");
				trySetFromYaml(output_json_trace,			trace, {"@ output_json"				}, "(bool) ");
			}

			{
				auto output_rotation = stringsToYamlObject(outputs, {"@ output_rotation"});

				trySetScaledFromYaml(rotate_period,		output_rotation, {"@ period"	},	{"@ period_units"	},	E_Period::_from_string_nocase);
			}

			{
				auto bias_sinex = stringsToYamlObject(outputs, {"! bias_sinex"});

				trySetFromYaml(output_bias_sinex,				bias_sinex, {"0 output"					}, "(bool) ");
				trySetFromYaml(bias_sinex_directory,			bias_sinex, {"@ directory"				});
				trySetFromYaml(bias_sinex_filename,				bias_sinex, {"@ filename"				});
				trySetFromYaml(ambrOpts.code_output_interval,	bias_sinex, {"@ code_output_interval"	}, "(double) Update interval for code  biases");
				trySetFromYaml(ambrOpts.phase_output_interval,	bias_sinex, {"@ phase_output_interval"	}, "(double) Update interval for phase biases");
				trySetFromYaml(ambrOpts.output_rec_bias,		bias_sinex, {"@ output_rec_bias"		}, "(bool) output receiver biases");
			}

			{
				auto clocks = stringsToYamlObject(outputs, {"! clocks"});

				trySetFromYaml(output_clocks,				clocks, {"0 output"					}, "(bool) ");
				trySetFromYaml(clocks_directory,			clocks, {"@ directory"				});
				trySetFromYaml(clocks_filename,				clocks, {"@ filename"				});
				trySetFromYaml(output_ar_clocks,			clocks, {"@ output_ar_clocks"		}, "(bool) ");
				trySetEnumVec (clocks_receiver_sources,		clocks, {"@ receiver_sources"		});
				trySetEnumVec (clocks_satellite_sources,	clocks, {"@ satellite_sources"		});
			}

			{
				auto decoded_rtcm = stringsToYamlObject(outputs, {"@ decoded_rtcm"});

				trySetFromYaml(output_decoded_rtcm_json,	decoded_rtcm, {"0@ output"		},	"(bool) Enable exporting decoded RTCM data to file");
				trySetFromYaml(decoded_rtcm_json_directory,	decoded_rtcm, {"@ directory"		},	"(string) Directory to export decoded RTCM data");
				trySetFromYaml(decoded_rtcm_json_filename,	decoded_rtcm, {"@ filename"		},	"(string) Decoded RTCM data filename");
			}

			{
				auto encoded_rtcm = stringsToYamlObject(outputs, {"@ encoded_rtcm"});

				trySetFromYaml(output_encoded_rtcm_json,	encoded_rtcm, {"0@ output"		},	"(bool) Enable exporting encoded RTCM data to file");
				trySetFromYaml(encoded_rtcm_json_directory,	encoded_rtcm, {"@ directory"		},	"(string) Directory to export encoded RTCM data");
				trySetFromYaml(encoded_rtcm_json_filename,	encoded_rtcm, {"@ filename"		},	"(string) Encoded RTCM data filename");
			}

			{
				auto erp = stringsToYamlObject(outputs, {"! erp"});

				trySetFromYaml(output_erp,				erp, {"0! output"		}, "(bool) ");
				trySetFromYaml(erp_directory,			erp, {"@ directory"		});
				trySetFromYaml(erp_filename,			erp, {"@ filename"		});
			}

			{
				auto ionex = stringsToYamlObject(outputs, {"! ionex"});

				trySetFromYaml(output_ionex,			ionex, {"0! output"						}, "(bool) ");
				trySetFromYaml(ionex_directory,			ionex, {"@ directory"						});
				trySetFromYaml(ionex_filename,			ionex, {"@ filename"						});
				trySetFromYaml(ionexGrid.lat_center,	ionex, {"@ grid", "@ lat_center"			});
				trySetFromYaml(ionexGrid.lon_center,	ionex, {"@ grid", "@ lon_center"			});
				trySetFromYaml(ionexGrid.lat_width,		ionex, {"@ grid", "@ lat_width"				});
				trySetFromYaml(ionexGrid.lon_width,		ionex, {"@ grid", "@ lon_width"				});
				trySetFromYaml(ionexGrid.lat_res,		ionex, {"@ grid", "@ lat_resolution"		});
				trySetFromYaml(ionexGrid.lon_res,		ionex, {"@ grid", "@ lon_resolution"		});
				trySetFromYaml(ionexGrid.time_res,		ionex, {"@ grid", "@ time_resolution"		});
			}

			{
				auto ionstec = stringsToYamlObject(outputs, {"! ionstec"});

				trySetFromYaml(output_ionstec,			ionstec, {"0! output"		}, "(bool) ");
				trySetFromYaml(ionstec_directory,		ionstec, {"@ directory"		});
				trySetFromYaml(ionstec_filename,		ionstec, {"@ filename"		});
			}

			{
				auto sinex = stringsToYamlObject(outputs, {"! sinex"});

				trySetFromYaml(output_sinex,			sinex, {"0! output"			}, "(bool) ");
				trySetFromYaml(sinex_directory,			sinex, {"@ directory"		});
				trySetFromYaml(sinex_filename,			sinex, {"@ filename"		});
			}

			{
				auto log = stringsToYamlObject(outputs, {"! log"});

				trySetFromYaml(output_log,		     	log, {"0! output"			}, "(bool) ");
				trySetFromYaml(log_directory,			log, {"@ directory"			});
				trySetFromYaml(log_filename,			log, {"@ filename"			});
			}

			{
				auto ntrip_log = stringsToYamlObject(outputs, {"@ ntrip_log"});

				trySetFromYaml(output_ntrip_log,		ntrip_log, {"0@ output"		}, "(bool) ");
				trySetFromYaml(ntrip_log_directory,		ntrip_log, {"@ directory"	});
				trySetFromYaml(ntrip_log_filename,		ntrip_log, {"@ filename"	});
			}

			{
				auto gpx = stringsToYamlObject(outputs, {"! gpx"});

				trySetFromYaml(output_gpx,		     	gpx, {"0! output"			}, "(bool) ");
				trySetFromYaml(gpx_directory,			gpx, {"@ directory"			});
				trySetFromYaml(gpx_filename,			gpx, {"@ filename"			});
			}

			{
				auto network_statistics = stringsToYamlObject(outputs, {"@ network_statistics"});

				trySetFromYaml(output_network_statistics_json,		network_statistics, {"0@ output"	},	"(bool) Enable exporting network statistics data to file");
				trySetFromYaml(network_statistics_json_directory,	network_statistics, {"@ directory"	},	"(string) Directory to export network statistics data");
				trySetFromYaml(network_statistics_json_filename,	network_statistics, {"@ filename"	},	"(string) Network statistics data filename");
			}

			{
				auto sp3 = stringsToYamlObject(outputs, {"! sp3"});

				trySetFromYaml(output_sp3,					sp3, {"0! output"					}, "(bool) ");
				trySetFromYaml(output_inertial_orbits, 		sp3, {"@ output_inertial"			}, "(bool) Output the entries using inertial positions and velocities");
				trySetFromYaml(output_predicted_orbits,		sp3, {"0@ output_predicted_orbits"	}, "(bool) ");
				trySetFromYaml(output_sp3_velocities,		sp3, {"@ output_velocities"			}, "(bool) ");
				trySetFromYaml(sp3_directory,				sp3, {"@ directory"					});
				trySetFromYaml(sp3_filename,				sp3, {"@ filename"					});
				trySetFromYaml(predicted_sp3_filename,		sp3, {"@ predicted_filename"		});
				trySetEnumVec (sp3_clock_sources,			sp3, {"@ clock_sources"				});
				trySetEnumVec (sp3_orbit_sources,			sp3, {"@ orbit_sources"				});
				trySetFromYaml(sp3_output_interval,			sp3, {"@ output_interval"			}, "(int) Update interval for sp3 records");
			}
			
			{
				auto orbit_ics = stringsToYamlObject(outputs, {"@ orbit_ics"});
				
				trySetFromYaml(output_orbit_ics,	orbit_ics, {"@ output"				}, "");
				trySetFromYaml(orbit_ics_filename,	orbit_ics, {"@ orbit_ics_filename"	}, "");
				trySetFromYaml(orbit_ics_directory,	orbit_ics, {"@ orbit_ics_directory"	}, "");
			}

			{
				auto orbex = stringsToYamlObject(outputs, {"@ orbex"});

				trySetFromYaml(output_orbex,			orbex, {"0@ output"				}, "(bool) Output orbex file");
				trySetFromYaml(orbex_directory,			orbex, {"@ directory"			}, "(string) Output orbex directory");
				trySetFromYaml(orbex_filename,			orbex, {"@ filename"			}, "(string) Output orbex filename");
				trySetEnumVec (orbex_orbit_sources,		orbex, {"@ orbit_sources" 		}, "[E_Source] Source for orbex orbits");
				trySetEnumVec (orbex_clock_sources,		orbex, {"@ clock_sources" 		}, "[E_Source] Source for orbex clocks");
				trySetEnumVec (orbex_attitude_sources,	orbex, {"@ attitude_sources" 	}, "[E_Source] Source for orbex attitudes");

				vector<string> recordTypeStrings;
				bool found = trySetFromYaml(recordTypeStrings,	orbex, {"@ record_types"	}, "[string] List of record types to output to ORBEX file");
				if (found)
				for (auto once : {1})
				{
					orbex_record_types.clear();

					for (auto& recordTypeString : recordTypeStrings)
					{
						try
						{
							orbex_record_types.push_back(recordTypeString);
						}
						catch (...)
						{
							continue;
						}
					}
				}
			}

			{
				auto ppp_sol = stringsToYamlObject(outputs, {"! ppp_sol"});

				trySetFromYaml(output_ppp_sol,			ppp_sol, {"0! output"	}, "(bool) ");
				trySetFromYaml(ppp_sol_directory,		ppp_sol, {"@ directory"	});
				trySetFromYaml(ppp_sol_filename,		ppp_sol, {"@ filename"	});
			}

			{
				auto cost = stringsToYamlObject(outputs, {"@ cost"});

				trySetFromYaml(output_cost,			cost, {"0@ output"			},	"(bool) Enable data exporting to troposphere COST file");
				trySetEnumVec (cost_data_sources,	cost, {"@ sources" 			},	"(enum) Source for troposphere delay data - KALMAN, etc.");
				trySetFromYaml(cost_directory,		cost, {"@ directory"		},	"(string) Directory to export troposphere COST file");
				trySetFromYaml(cost_filename,		cost, {"@ filename"			},	"(string) Troposphere COST filename");
				trySetFromYaml(cost_time_interval,	cost, {"@ time_interval"	},	"(int) Time interval between entries in troposphere COST file (sec)");
				trySetFromYaml(cost_format,			cost, {"@ cost_format"		},	"(string) Format name & version number");
				trySetFromYaml(cost_project,		cost, {"@ cost_project"		},	"(string) Project name");
				trySetFromYaml(cost_status,			cost, {"@ cost_status"		},	"(string) File status");
				trySetFromYaml(cost_centre,			cost, {"@ cost_centre"		},	"(string) Processing centre");
				trySetFromYaml(cost_method,			cost, {"@ cost_method"		},	"(string) Processing method");
				trySetFromYaml(cost_orbit_type,		cost, {"@ cost_orbit_type"	},	"(string) Orbit type");
				trySetFromYaml(cost_met_source,		cost, {"@ cost_met_sources"	},	"(string) Source of met. data");
			}

			{
				auto rinex_nav = stringsToYamlObject(outputs, {"@ rinex_nav"});

				trySetFromYaml(output_rinex_nav,		rinex_nav, {"0@ output"		}, "(bool) ");
				trySetFromYaml(rinex_nav_directory,		rinex_nav, {"@ directory"	});
				trySetFromYaml(rinex_nav_filename,		rinex_nav, {"@ filename"	});
				trySetFromYaml(rinex_nav_version,		rinex_nav, {"@ version"		});
			}

			{
				auto rinex_obs = stringsToYamlObject(outputs, {"@ rinex_obs"});

				trySetFromYaml(output_rinex_obs,		rinex_obs, {"0@ output"					}, "(bool) ");
				trySetFromYaml(rinex_obs_directory,		rinex_obs, {"@ directory"				});
				trySetFromYaml(rinex_obs_print_C_code,	rinex_obs, {"@ output_pseudorange"		}, "(bool) ");
				trySetFromYaml(rinex_obs_print_L_code,	rinex_obs, {"@ output_phase_range"		}, "(bool) ");
				trySetFromYaml(rinex_obs_print_D_code,	rinex_obs, {"@ output_doppler"			}, "(bool) ");
				trySetFromYaml(rinex_obs_print_S_code,	rinex_obs, {"@ output_signal_to_noise"	}, "(bool) ");
				trySetFromYaml(rinex_obs_filename,		rinex_obs, {"@ filename"				});
				trySetFromYaml(rinex_obs_version,		rinex_obs, {"@ version"					});
			}

			{
				auto rtcm_nav = stringsToYamlObject(outputs, {"@ rtcm_nav"});

				trySetFromYaml(record_rtcm_nav,			rtcm_nav, {"0@ output"			}, "(bool) ");
				trySetFromYaml(rtcm_nav_directory,		rtcm_nav, {"@ directory"		 	});
				trySetFromYaml(rtcm_nav_filename,		rtcm_nav, {"@ filename"			});
			}

			{
				auto rtcm_obs = stringsToYamlObject(outputs, {"@ rtcm_obs"});

				trySetFromYaml(record_rtcm_obs,			rtcm_obs, {"0@ output"			}, "(bool) ");
				trySetFromYaml(rtcm_obs_directory,		rtcm_obs, {"@ directory"		});
				trySetFromYaml(rtcm_obs_filename,		rtcm_obs, {"@ filename"			});
			}

			{
				auto raw_ubx = stringsToYamlObject(outputs, {"raw_ubx"});

				trySetFromYaml(record_raw_ubx,			raw_ubx, {"0 output"			}, "(bool) ");
				trySetFromYaml(raw_ubx_directory,		raw_ubx, {"directory"			});
				trySetFromYaml(raw_ubx_filename,		raw_ubx, {"filename"			});
			}

			{
				auto slr_obs = stringsToYamlObject(outputs, {"@ slr_obs"});

				trySetFromYaml(output_slr_obs,			slr_obs, {"0@ output"			}, 	"(bool) Enable data exporting to tabular SLR obs file");
				trySetFromYaml(slr_obs_directory,		slr_obs, {"@ directory"			}, 	"(string) Directory to export tabular SLR obs file");
				trySetFromYaml(slr_obs_filename,		slr_obs, {"@ filename"			},	"(string) Tabular SLR obs filename");
			}

			{
				auto trop_sinex = stringsToYamlObject(outputs, {"! trop_sinex"});

				trySetFromYaml(output_trop_sinex,		trop_sinex, {"0! output"		},	"(bool) Enable data exporting to troposphere SINEX file");
				trySetEnumVec (trop_sinex_data_sources,	trop_sinex, {"@ sources"		},	"(enum) Source for troposphere delay data - KALMAN, etc.");
				trySetFromYaml(trop_sinex_directory,	trop_sinex, {"@ directory"		},	"(string) Directory to export troposphere SINEX file");
				trySetFromYaml(trop_sinex_filename,		trop_sinex, {"@ filename"		},	"(string) Troposphere SINEX filename");
				trySetFromYaml(trop_sinex_sol_type,		trop_sinex, {"@ sol_type"		},	"(string) Troposphere SINEX solution type");
				trySetFromYaml(trop_sinex_obs_code,		trop_sinex, {"@ obs_code"		},	"(string) Troposphere SINEX observation code");
				trySetFromYaml(trop_sinex_const_code,	trop_sinex, {"@ const_code"		},	"(string) Troposphere SINEX const code");
				trySetFromYaml(trop_sinex_version,		trop_sinex, {"@ version"		},	"(string) Troposphere SINEX version");
			}

			{
				auto streams = stringsToYamlObject(outputs, 		{"! streams"});

				string root_stream_url = "";
				trySetFromYaml(root_stream_url, streams, {"0! root_url"}, "(string) Root url to be prepended to all other streams specified in this section. If the streams used have individually specified root urls, usernames, or passwords, this should not be used.");

				replaceTags(root_stream_url);

				SsrBroadcast	dummyStreamData;
				tryGetStreamFromYaml(dummyStreamData, streams, {"@ XMPL"});

				auto [outStreamNode, outStreamString] = stringsToYamlObject(streams, {"1@ labels"},		"[string] List of output stream is with further information to be found in its own section, as per XMPL below");

				for (auto outLabelYaml : outStreamNode)
				{
					string outLabel = outLabelYaml.as<string>();

					tryGetStreamFromYaml(netOpts.uploadingStreamData[outLabel], streams, {outLabel});

					tryAddRootToPath(root_stream_url, netOpts.uploadingStreamData[outLabel].url);
				}
			}
		}

		{
			auto inputs	= stringsToYamlObject({yaml, ""},	{"0! inputs"}, docs["inputs"]);

			string root_stream_url;
			trySetFromAny(root_input_directory,	commandOpts,	inputs, {"0! root_directory"		}, "(string) Root path to be added to all other input files (unless they are absolute)");
			trySetFromYaml(root_stream_url,						inputs,	{"0! root_stream_url"	}, "(string) Root url to be prepended to all other streams specified in this section. If the streams used have individually specified root urls, usernames, or passwords, this should not be used.");
			
			trySetFromAny(atx_files,			commandOpts,	inputs, {"! atx_files"				}, "[string] List of atx files to use");
			trySetFromAny(snx_files,			commandOpts,	inputs, {"! snx_files"				}, "[string] List of snx files to use");
			trySetFromAny(blq_files,			commandOpts,	inputs, {"! blq_files"				}, "[string] List of blq files to use");
			trySetFromAny(erp_files,			commandOpts,	inputs, {"! erp_files"				}, "[string] List of erp files to use");
			trySetFromAny(ion_files,			commandOpts,	inputs, {"! ion_files"				}, "[string] List of ion files to use");
			trySetFromAny(igrf_files,			commandOpts,	inputs, {"! igrf_files"				}, "[string] List of igrf files to use");
			trySetFromAny(egm_files,			commandOpts,	inputs, {"! egm_files"				}, "[string] List of egm files to use");
			trySetFromAny(jpl_files,			commandOpts,	inputs, {"! jpl_files"				}, "[string] List of jpl files to use");
			trySetFromAny(tide_files,			commandOpts,	inputs, {"! tide_files"});


			trySetFromYaml(vmf_files,				inputs, {"! troposphere", "! vmf_files"				}, "[string] List of vmf files to use");
			trySetFromYaml(model.trop.orography,	inputs, {"! troposphere", "! orography_files"		});
			trySetFromYaml(model.trop.gpt2grid,		inputs, {"! troposphere", "! gpt2grid_files"		});

			trySetFromYaml(atm_reg_definitions,		inputs, {"@ ionosphere" , "@ atm_reg_definitions"	}, "[string] List of files to define regions for compact SSR");
			trySetFromAny(ion_files,			commandOpts,	inputs, {"! ionosphere" , "! ion_files"	}, "[string] List of IONEX files for VTEC input");


			{
				auto gnss_data = stringsToYamlObject(inputs, {"1! gnss_observations"});

				string inputs_root = "./";
				trySetFromAny(inputs_root,	commandOpts,	gnss_data,	{"0! inputs_root"		}, "(string) Root path to be added to all other gnss data inputs (unless they are absolute)");

				tryGetMappedList(rnx_inputs,			gnss_data,	{"! rnx_inputs"			}, "[string] List of rinex      inputs to use");
				tryGetMappedList(ubx_inputs,			gnss_data,	{"ubx_inputs"			}, "[string] List of ubxfiles   inputs to use");
				tryGetMappedList(obs_rtcm_inputs,		gnss_data,	{"! rtcm_inputs"		}, "[string] List of rtcmfiles  inputs to use for observations");

				tryAddRootToPath(inputs_root, rnx_inputs);
				tryAddRootToPath(inputs_root, ubx_inputs);
				tryAddRootToPath(inputs_root, obs_rtcm_inputs);
			}

			{
				auto pseudo_observation_data = stringsToYamlObject(inputs, {"! pseudo_observations"});

				string inputs_root = "./";
				trySetFromYaml(inputs_root,	pseudo_observation_data,	{"0! inputs_root"		}, "(string) Root path to be added to all other pseudo obs data files (unless they are absolute)");

				tryGetMappedList(pseudo_sp3_inputs,	pseudo_observation_data,	{"! sp3_inputs"			}, "[string] List of sp3 inputs to use for pseudoobservations");
				tryGetMappedList(pseudo_snx_inputs,	pseudo_observation_data,	{"@ snx_inputs"			}, "[string] List of snx inputs to use for pseudoobservations");

				tryAddRootToPath(inputs_root, pseudo_sp3_inputs);
				tryAddRootToPath(inputs_root, pseudo_snx_inputs);
			}

			{
				auto satellite_data = stringsToYamlObject(inputs, {"0! satellite_data"});

				string inputs_root = "./";
				trySetFromYaml(inputs_root,	satellite_data,	{"0! inputs_root"		}, "(string) Root path to be added to all other satellite data files (unless they are absolute)");

				trySetFromAny(nav_files,				commandOpts,	satellite_data, {"! nav_files"			}, "[string] List of ephemeris  files to use");			
				trySetFromAny(sp3_files,				commandOpts,	satellite_data, {"! sp3_files"			}, "[string] List of sp3        files to use");			
				trySetFromAny(dcb_files,				commandOpts,	satellite_data, {"! dcb_files"			}, "[string] List of dcb        files to use");			
				trySetFromAny(bsx_files,				commandOpts,	satellite_data, {"! bsx_files"			}, "[string] List of biassinex  files to use");			
				trySetFromAny(clk_files,				commandOpts,	satellite_data, {"! clk_files"			}, "[string] List of clock      files to use");			
				trySetFromAny(sid_files,				commandOpts, 	satellite_data, {"@ sid_files"			}, "[string] List of sat ID     files to use - from https://cddis.nasa.gov/sp3c_satlist.html/");
				trySetFromAny(com_files,				commandOpts, 	satellite_data, {"@ com_files"			}, "[string] List of com        files to use - retroreflector offsets from centre-of-mass for spherical sats");
				trySetFromAny(crd_files,				commandOpts, 	satellite_data, {"@ crd_files"			}, "[string] List of crd        files to use - SLR observation data");
				trySetFromAny(obx_files,				commandOpts,	satellite_data, {"! obx_files"			}, "[string] List of orbex      files to use");			
				trySetFromAny(orb_files,				commandOpts,	satellite_data, {"@ orb_files"			}, "[string] List of orbit(pod) files to use");			
				trySetFromAny(nav_rtcm_inputs,			commandOpts,	satellite_data,	{"! rtcm_inputs"			}, "[string] List of rtcm       inputs to use for corrections");

// 				tryAddRootToPath(inputs_root, nav_files);
// 				tryAddRootToPath(inputs_root, sp3_files);
// 				tryAddRootToPath(inputs_root, dcb_files);
// 				tryAddRootToPath(inputs_root, bsx_files);
// 				tryAddRootToPath(inputs_root, clk_files);
// 				tryAddRootToPath(inputs_root, sid_files);
// 				tryAddRootToPath(inputs_root, com_files);
// 				tryAddRootToPath(inputs_root, crd_files);
// 				tryAddRootToPath(inputs_root, obx_files);
// 				tryAddRootToPath(inputs_root, orb_files);
				tryAddRootToPath(inputs_root, nav_rtcm_inputs);
			}
		}

		auto processing_options = stringsToYamlObject({ yaml, "" }, {processing_options_str});
		{
			auto general = stringsToYamlObject(processing_options, {"2! gnss_general"});
			
			bool found = trySetFromAny(elevation_mask,	commandOpts,	general, {"0!  elevation_mask"		});
			if (found)
				elevation_mask *= D2R;
			
			trySetFromYaml	(require_apriori_positions,					general, {"@ require_apriori_positions" }, "(bool) ");
			trySetFromYaml	(require_antenna_details,					general, {"@ require_antenna_details" 	}, "(bool) ");
			trySetFromYaml	(pivot_station,								general, {"@ pivot_station" 			}, "(string) ");
			trySetFromYaml	(interpolate_rec_pco,						general, {"@ interpolate_rec_pco" 		}, "(bool) ");
			trySetFromYaml	(max_gdop,									general, {"@ max_gdop"					});
			trySetFromYaml	(raim,										general, {"@ raim"						}, "(bool) ");
			trySetEnumOpt	(recOptsMap[""].error_model,				general, {"@ error_model"				}, E_NoiseModel::_from_string_nocase);
			trySetFromYaml	(pppOpts.common_atmosphere,					general, {"@ common_atmosphere"			}, "(bool) ");
			trySetFromYaml	(pppOpts.use_rtk_combo,						general, {"@ use_rtk_combo"				}, "(bool) ");
			trySetFromYaml	(delete_old_ephemerides,					general, {"@ delete_old_ephemerides"	}, "(bool) ");
			trySetFromYaml	(no_bias_sigma,								general, {"@ no_bias_sigma"				}, "(float) Sigma to use for biases that are not found");
			trySetFromYaml	(use_tgd_bias,								general, {"@ use_tgd_bias"				}, "(bool) Use TGD/BGD bias from ephemeris, DO NOT turn on unless using Klobuchar/NeQuick Ionospheres");
			trySetFromYaml	(common_sat_pco,							general, {"@ common_sat_pco"			}, "(bool) Use L1 satellite PCO values for all signals");
			trySetFromYaml	(common_rec_pco,							general, {"@ common_rec_pco"			}, "(bool) Use L1 receiver PCO values for all signals");
			trySetFromYaml	(leap_seconds,								general, {"@ gpst_utc_leap_seconds"		}, "(int) Difference between gps time and utc in leap seconds");

			trySetFromYaml	(process_meas[CODE],						general, {"1@ code_measurements",		"process"	}, "(bool) ");
			trySetFromYaml	(recOptsMap[""].code_sigmas,				general, {"1@ code_measurements",		"sigmas"	});
			
			trySetFromYaml	(process_meas[PHAS],						general, {"1@ phase_measurements",		"process"	}, "(bool) ");
			trySetFromYaml	(recOptsMap[""].phas_sigmas,				general, {"1@ phase_measurements",		"sigmas"	});		//todo aaron needed?

			trySetEnumOpt	(receiver_reference_clk,					general, {"@ rec_reference_system"		}, E_Sys::_from_string_nocase, "(String) Receiver will use this system as reference clock");
			trySetFromYaml	(fixed_phase_bias_var,						general, {"@ fixed_phase_bias_var"		}, "(double) variance of phase bias to be considered fixed/binded");
			trySetFromYaml	(sat_clk_definition,						general, {"@ sat_clk_definition"		}, "(bool) use satellite clock definition pseudorange ");
			

			for (int i = E_Sys::GPS; i < E_Sys::SUPPORTED; i++)
			{
				E_Sys	sys			= E_Sys::_values()[i];
				string	sysName		= boost::algorithm::to_lower_copy((string) sys._to_string());	

				auto sys_options = stringsToYamlObject(general, {"1! sys_options", sysName});

				trySetFromYaml(process_sys				[sys],		sys_options, {"0! process"				}, "(bool) Process this constellation");
				trySetFromYaml(solve_amb_for			[sys],		sys_options, {"! ambiguity_resolution"	}, "(bool) Solve carrier phase ambiguities for this constellation");
				trySetFromYaml(reject_eclipse			[sys],		sys_options, {"@ reject_eclipse"			}, "(bool) Exclude satellites that are in eclipsing region");
				trySetFromYaml(zero_satellite_dcb		[sys],		sys_options, {"@ zero_satellite_dcb"		}, "(bool) Constrain: satellite DCB for this system to zero");
				trySetFromYaml(zero_receiver_dcb		[sys],		sys_options, {"@ zero_receiver_dcb"		}, "(bool) Constrain: receiver DCB for this system to zero");
				trySetFromYaml(one_phase_bias			[sys],		sys_options, {"@ one_phase_bias"			}, "(bool) Constrain: assume satellite phase biases are common among frequencies");
				trySetFromYaml(receiver_amb_pivot		[sys],		sys_options, {"@ receiver_amb_pivot"		}, "(bool) Constrain: set of ambiguities, to eliminate receiver rank deficiencies");
				trySetFromYaml(network_amb_pivot		[sys],		sys_options, {"@ network_amb_pivot"		}, "(bool) Constrain: set of ambiguities, to eliminate network  rank deficiencies");
				trySetFromYaml(use_for_iono_model		[sys],		sys_options, {"@ use_for_iono_model"		}, "(bool) Use this constellation as part of Ionospheric model");
				trySetFromYaml(use_iono_corrections		[sys],		sys_options, {"@ use_iono_corrections"	}, "(bool) Use external ionosphere delay estimation for this constellation");
				trySetEnumOpt( used_nav_types			[sys],		sys_options, {"@ used_nav_type"			}, E_NavMsgType::_from_string_nocase);
				
				vector<string> clockCodesStrings;
				bool found = trySetFromYaml(clockCodesStrings,		sys_options, {"@ clock_codes"	}, "(string) Default observation codes on two frequencies for IF combination based satellite clocks");
				if (found)
				for (auto once : {1})
				{
					try
					{
						clock_codesL1[sys] = E_ObsCode::_from_string_nocase(clockCodesStrings.at(0).c_str());
						clock_codesL2[sys] = E_ObsCode::_from_string_nocase(clockCodesStrings.at(1).c_str());
					}

					catch (...)
					{
						BOOST_LOG_TRIVIAL(error)
						<< std::endl << "Error: Invalid clock codes for: " << sysName << ", there should be two codes for each system in the form [LXX, LXX]\n";

						continue;
					}
				}
				
				vector<string> codePriorityStrings;
				found = trySetFromYaml(codePriorityStrings,			sys_options, {"! code_priorities"			});
				if (found)
				for (auto once : {1})
				{
					code_priorities[sys].clear();

					for (auto& codePriorityString : codePriorityStrings)
					{
						try
						{
							auto a = E_ObsCode::_from_string_nocase(codePriorityString.c_str());
							code_priorities[sys].push_back(a);
						}
						catch (...)
						{
							continue;
						}
					}
				}
				
				vector<string> zeroAverageCodes;
				found = trySetFromYaml(zeroAverageCodes,			sys_options, {"@ zero_code_average"			});
				if (found)
				for (auto once : {1})
				{
					zero_code_average[sys].clear();

					for (auto& codeString : zeroAverageCodes)
					{
						try
						{
							auto a = E_ObsCode::_from_string(codeString.c_str());
							zero_code_average[sys].push_back(a);
						}
						catch (...)
						{
							continue;
						}
					}
				}
				
				vector<string> zeroAveragePhase;
				found = trySetFromYaml(zeroAveragePhase,			sys_options, {"@ zero_phas_average"			});
				if (found)
				for (auto once : {1})
				{
					zero_phas_average[sys].clear();

					for (auto& codeString : zeroAveragePhase)
					{
						try
						{
							auto a = E_ObsCode::_from_string(codeString.c_str());
							zero_phas_average[sys].push_back(a);
						}
						catch (...)
						{
							continue;
						}
					}
				}
			}

			{
				auto epoch_control = stringsToYamlObject(processing_options, {"0! epoch_control"});

				int i = 0;

				string startStr;
				string stopStr;
				trySetFromAny(epoch_interval,		commandOpts,	epoch_control, {std::to_string(i++) + "! epoch_interval"		}, "(float) Desired time step between each processing epoch");
				trySetFromAny(epoch_tolerance,		commandOpts,	epoch_control, {std::to_string(i++) + "@ epoch_tolerance"		}, "(float) Tolerance of times to add to an epoch (usually half of the original data's sample rate)");
				trySetFromAny(max_epochs,			commandOpts,	epoch_control, {std::to_string(i++) + "! max_epochs"			}, "(int)   Maximum number of epochs to process");
				trySetFromAny(startStr,				commandOpts,	epoch_control, {std::to_string(i++) + "! start_epoch"			}, "(date) The time of the first epoch to process (all observations before this will be skipped)");
				trySetFromAny(stopStr,				commandOpts,	epoch_control, {std::to_string(i++) + "! end_epoch"				}, "(date) The time of the last epoch to process (all observations after this will be skipped)");

				if (!startStr.empty())	start_epoch	= boost::posix_time::time_from_string(startStr);
				if (!stopStr .empty())	end_epoch	= boost::posix_time::time_from_string(stopStr);

				trySetFromAny(fatal_level,			commandOpts,	epoch_control, {std::to_string(i++) + "@ fatal_message_level"	}, "(int) Threshold level for exiting the program early (0-2)");

				wait_next_epoch = epoch_interval + 0.01;
				trySetFromYaml(wait_next_epoch,						epoch_control, {std::to_string(i++) + "@ wait_next_epoch"		}, "(float) Time to wait for next epochs data before skipping the epoch (will default to epoch_interval as an appropriate minimum value for realtime)");
				trySetFromYaml(wait_all_stations,					epoch_control, {std::to_string(i++) + "@ wait_all_stations"		}, "(float) Time to wait from the reception of the first data of an epoch before skipping stations with data still unreceived");
				trySetFromYaml(require_obs,							epoch_control, {std::to_string(i++) + "@ require_obs"			}, "(bool) Exit the program if no observation sources are available");
				trySetFromYaml(assign_closest_epoch,				epoch_control, {std::to_string(i++) + "@ assign_closest_epoch"	}, "(bool) Assign observations to the closest epoch - don't skip observations that fall between epochs");
				trySetFromAny(simulate_real_time,	commandOpts,	epoch_control, {std::to_string(i++) + "@ simulate_real_time"	}, "(bool)  For RTCM playback - delay processing to match original data rate");
			}

			{
				auto gnss_modelling = stringsToYamlObject(processing_options, {"2! gnss_models"});

				trySetFromYaml(model.rec_pos.enable,					gnss_modelling, {"@ rec_pos",			"enable"	}, "(bool) ");
				trySetFromYaml(model.sat_pos.enable,					gnss_modelling, {"@ sat_pos",			"enable"	}, "(bool) ");
				trySetEnumVec( model.sat_pos.ephemeris_sources,	 		gnss_modelling,	{"! sat_pos",			"sources"	});

				trySetFromYaml(model.range,								gnss_modelling, {"@ range",				"enable"	}, "(bool) ");

				trySetFromYaml(model.sat_clock.enable,					gnss_modelling, {"! sat_clock",			"enable"	}, "(bool) ");
				trySetEnumVec( model.sat_clock.ephemeris_sources, 		gnss_modelling,	{"! sat_clock",			"! sources"	});

				trySetFromYaml(model.rec_clock.enable,					gnss_modelling, {"@ rec_clock",			"enable"	}, "(bool) ");
				trySetFromYaml(model.sat_code_bias,						gnss_modelling, {"@ sat_code_bias",		"enable"	}, "(bool) ");
				trySetFromYaml(model.rec_code_bias,						gnss_modelling, {"@ rec_code_bias",		"enable"	}, "(bool) ");
				trySetFromYaml(model.sat_phase_bias,					gnss_modelling, {"@ sat_phase_bias",	"enable"	}, "(bool) ");
				trySetFromYaml(model.rec_phase_bias,					gnss_modelling, {"@ rec_phase_bias",	"enable"	}, "(bool) ");

				trySetFromYaml(model.rec_ant_delta,						gnss_modelling, {"@ rec_ant_delta",		"enable"	}, "(bool) ");
				trySetFromYaml(model.sat_pco,							gnss_modelling, {"@ sat_pco",			"enable"	}, "(bool) ");
				trySetFromYaml(model.rec_pco,							gnss_modelling, {"@ rec_pco",			"enable"	}, "(bool) ");
				trySetFromYaml(model.sat_pcv,							gnss_modelling, {"@ sat_pcv",			"enable"	}, "(bool) ");
				trySetFromYaml(model.rec_pcv,							gnss_modelling, {"@ rec_pcv",			"enable"	}, "(bool) ");

				trySetFromYaml(model.tides.enable,						gnss_modelling, {"@ tides",				"@ enable"	}, "(bool) ");
				trySetFromYaml(model.tides.solid,						gnss_modelling, {"@ tides",				"@ solid"	}, "(bool) ");
				trySetFromYaml(model.tides.otl,							gnss_modelling, {"@ tides",				"@ otl"		}, "(bool) ");
				trySetFromYaml(model.tides.pole,						gnss_modelling, {"@ tides",				"@ pole"	}, "(bool) ");

				trySetFromYaml(model.relativity,						gnss_modelling, {"@ relativity",		"@ enable"	}, "(bool) ");
				trySetFromYaml(model.relativity2,						gnss_modelling, {"@ relativity2",		"@ enable"	}, "(bool) ");
				trySetFromYaml(model.sagnac,							gnss_modelling, {"@ sagnac",			"@ enable"	}, "(bool) ");

				trySetFromYaml(model.sat_attitude.enable,				gnss_modelling, {"@ sat_attitude",		"@ enable"		}, "(bool) Enables non-nominal attitude types");
				trySetEnumVec( model.sat_attitude.sources, 				gnss_modelling,	{"@ sat_attitude",		"@ sources"		}, "[E_Source] Attitude type ");
				trySetFromYaml(model.sat_attitude.valid_var,			gnss_modelling, {"@ sat_attitude",		"@ valid_var"	}, "(double) Observation variance added when attitude is valid");
				trySetFromYaml(model.sat_attitude.invalid_var,			gnss_modelling, {"@ sat_attitude",		"@ invalid_var"	}, "(double) Observation variance added when attitude is invalid");

				trySetFromYaml(model.rec_attitude.enable,				gnss_modelling, {"@ rec_attitude",		"@ enable"		}, "(bool) Enables non-nominal attitude types");
				trySetEnumVec( model.rec_attitude.sources, 				gnss_modelling,	{"@ rec_attitude",		"@ sources"		}, "[E_Source] Attitude type");
				trySetFromYaml(model.rec_attitude.valid_var,			gnss_modelling, {"@ rec_attitude",		"@ valid_var"	}, "(double) Observation variance added when attitude is valid");
				trySetFromYaml(model.rec_attitude.invalid_var,			gnss_modelling, {"@ rec_attitude",		"@ invalid_var"	}, "(double) Observation variance added when attitude is invalid");


				{
					auto ionospheric_component = stringsToYamlObject(gnss_modelling, {"! ionospheric_component"});

					trySetFromYaml(model.ionospheric_component,				ionospheric_component, {"0@ enable"	}, "(bool) ");

					trySetEnumOpt( ionoOpts.corr_mode, 						ionospheric_component, {"@ corr_mode" 						}, E_IonoMode::_from_string_nocase);
					trySetEnumOpt( ionoOpts.mapping_function,				ionospheric_component, {"@ mapping_function" 				}, E_IonoMapFn::_from_string_nocase, "(E_IonoMapFn) mapping function if not specified in the data or model");
					trySetFromYaml(ionoOpts.pierce_point_layer_height,		ionospheric_component, {"@ pierce_point_layer_height"		}, "(double) ionospheric pierce point layer height if not specified in the data or model (km)");
					trySetFromYaml(ionoOpts.mapping_function_layer_height,	ionospheric_component, {"@ mapping_function_layer_height"	}, "(double) mapping function layer height if not specified in the data or model (km)");
					trySetFromYaml(ionoOpts.common_ionosphere,				ionospheric_component, {"! common_ionosphere"				}, "(bool) ");
					trySetFromYaml(ionoOpts.use_if_combo,					ionospheric_component, {"! use_if_combo"					}, "(bool) ");
					trySetFromYaml(ionoOpts.auto_select_default_code,		ionospheric_component, {"@ automatic_def_codes"				}, "(bool) Automatically detect/select default GNSS codes to estimate the Ionosphere");
				}

				{
					auto ionospheric_component2 = stringsToYamlObject(gnss_modelling, {"@ ionospheric_component2"});

					trySetFromYaml(model.ionospheric_component2,			ionospheric_component2, {"0@ enable"	}, "(bool) ");
				}

				{
					auto ionospheric_component3 = stringsToYamlObject(gnss_modelling, {"@ ionospheric_component3"});

					trySetFromYaml(model.ionospheric_component3,			ionospheric_component3, {"0@ enable"	}, "(bool) ");
				}

				{
					auto ionospheric_model = stringsToYamlObject(gnss_modelling, {"@ ionospheric_model"});

					trySetFromYaml(process_ionosphere,				ionospheric_model, {"0@ enable"				}, "(bool) Compute ionosphere maps from a network of stations");
					trySetEnumOpt( ionModelOpts.model, 				ionospheric_model, {"@ model" 				}, E_IonoModel::_from_string_nocase);
					trySetFromYaml(ionModelOpts.function_order,		ionospheric_model, {"@ function_order"		}, "Maximum order  of Spherical harmonics for Ionospheric mapping");
					trySetFromYaml(ionModelOpts.function_degree,	ionospheric_model, {"@ function_degree"		}, "Maximum degree of Spherical harmonics for Ionospheric mapping");
					trySetFromYaml(ionModelOpts.estimate_sat_dcb,	ionospheric_model, {"@ estimate_sat_dcb"	}, "(bool) Estimate satellite dcb alongside Ionosphere models, should be false for local STEC");
					trySetFromYaml(ionModelOpts.use_rotation_mtx,	ionospheric_model, {"@ use_rotation_mtx"	}, "(bool) Use 3D rotation matrix for spherical harmonics");

					bool found = trySetFromYaml(ionModelOpts.layer_heights,		ionospheric_model, {"@ layer_heights"			});
					if (found)
					for (auto& a : ionModelOpts.layer_heights)
					{
						a *= 1000; //km to m
					}
				}

				auto troposhpere = stringsToYamlObject(gnss_modelling, {"@ troposphere"});
				{
					trySetFromYaml(model.trop.enable,		troposhpere,	{"0@ enable" 		});
					trySetEnumOpt( model.trop.model, 		troposhpere,	{"@ model" 			}, E_TropModel::_from_string_nocase);
				}

				auto orbits = stringsToYamlObject(gnss_modelling, {"@ orbits"});
				{
					trySetFromYaml(model.orbits,			orbits,			{"0@ enable" 		});
				}

				trySetFromYaml(model.phase_windup,						gnss_modelling, {"@ phase_windup",		"enable"	}, "(bool) ");
				trySetFromYaml(model.heading,							gnss_modelling, {"@ heading",			"enable"	}, "(bool) ");
				trySetFromYaml(model.integer_ambiguity,					gnss_modelling, {"@ integer_ambiguity",	"enable"	}, "(bool) ");
				trySetFromYaml(model.clock_definitions,					gnss_modelling, {"@ clock_definitions",	"enable"	}, "(bool) ");
			}

			{
				auto model_error_checking = stringsToYamlObject(processing_options, {"3! model_error_checking"});

				{
					auto deweighting = stringsToYamlObject(model_error_checking, {"! deweighting"});

					trySetFromYaml(deweight_on_state_error,		deweighting,	{"@ deweight_on_state_error"	}, "(bool) Any \"state\" errors cause deweighting of all measurements that reference the state");

					trySetFromYaml(deweight_factor,				deweighting,	{"! deweight_factor"			}, "(float) Factor to downweight the variance of measurements with statistically detected errors");
					trySetFromYaml(deweight_on_state_error,		deweighting,	{"@ deweight_on_state_error"	}, "(bool) Any \"state\" errors cause deweighting of all measurements that reference the state");
				}

				{
					auto orbit_errors = stringsToYamlObject(model_error_checking, {"@ orbit_errors"});

					trySetFromYaml(orbit_pos_proc_noise,			orbit_errors,	{"@ orbit_pos_proc_noise"			});
					trySetFromYaml(orbit_vel_proc_noise,			orbit_errors,	{"@ orbit_vel_proc_noise"			});
					trySetFromYaml(orbit_vel_proc_noise_trail,		orbit_errors,	{"@ orbit_vel_proc_noise_trail"		});
					trySetFromYaml(orbit_vel_proc_noise_trail_tau,	orbit_errors,	{"@ orbit_vel_proc_noise_trail_tau"	});
				}

				{
					auto ambiguities = stringsToYamlObject(model_error_checking, {"! ambiguities"});

					trySetFromYaml(reinit_on_all_slips,			ambiguities,	{"! reinit_on_all_slips"	}, "(bool) Any detected slips cause removal and reinitialisation of ambiguities");
					trySetFromYaml(pppOpts.outage_reset_limit,	ambiguities,	{"! outage_reset_limit"		}, "(int) Maximum number of epochs with missed phase measurements before the ambiguity associated with the measurement is reset.");
					trySetFromYaml(pppOpts.phase_reject_limit,	ambiguities,	{"! phase_reject_limit"		}, "(int) Maximum number of phase measurements to reject before the ambiguity associated with the measurement is reset.");
				}

				{
					auto clocks = stringsToYamlObject(model_error_checking, {"@ clocks"});

					trySetFromYaml(reinit_on_clock_error,		clocks,			{"@ reinit_on_clock_error"	}, "(bool) Any clock \"state\" errors cause removal and reinitialisation of the clocks and all associated ambiguities");
				}

				{
					auto cycle_slips = stringsToYamlObject(model_error_checking, {"@ cycle_slips"});

					trySetFromYaml(thres_slip,			cycle_slips, {"@ slip_threshold"		});
					trySetFromYaml(mw_proc_noise,		cycle_slips, {"@ mw_proc_noise"			});

					trySetFromYaml(excludeSlip.LLI,		cycle_slips, {"@ exclude_on",	"@ lli"		}, "(bool) Exclude measurements that fail LLI slip test in preprocessor");
					trySetFromYaml(excludeSlip.GF,		cycle_slips, {"@ exclude_on",	"@ gf"		}, "(bool) Exclude measurements that fail GF  slip test in preprocessor");
					trySetFromYaml(excludeSlip.MW,		cycle_slips, {"@ exclude_on",	"@ mw"		}, "(bool) Exclude measurements that fail MW  slip test in preprocessor");
					trySetFromYaml(excludeSlip.SCDIA,	cycle_slips, {"@ exclude_on",	"@ scdia"	}, "(bool) Exclude measurements that fail SCDIA    test in preprocessor");

					trySetFromYaml(resetOnSlip.LLI,		cycle_slips, {"@ reset_on",		"@ lli"		}, "(bool) Reset ambiguities if LLI   test is detecting a slip");
					trySetFromYaml(resetOnSlip.GF,		cycle_slips, {"@ reset_on",		"@ gf"		}, "(bool) Reset ambiguities if GF    test is detecting a slip");
					trySetFromYaml(resetOnSlip.MW,		cycle_slips, {"@ reset_on",		"@ mw"		}, "(bool) Reset ambiguities if MW    test is detecting a slip");
					trySetFromYaml(resetOnSlip.SCDIA,	cycle_slips, {"@ reset_on",		"@ scdia"	}, "(bool) Reset ambiguities if SCDIA test is detecting a slip");
				}

			}

			{
				auto process_modes = stringsToYamlObject(processing_options, {"1 process_modes"});
			
				trySetFromYaml(process_user,				process_modes, {"@ user"			}, "(bool) Compute position/clock/atmosphere at autonomous receiver location using UD/IF PPP");
				trySetFromYaml(process_network,				process_modes, {"@ network"			}, "(bool) Compute GNSS corrections using UD/IF PPP on a CORS network");
				trySetFromYaml(process_ionosphere,			process_modes, {"@ ionosphere"		}, "(bool) Compute Ionosphere models based on GNSS measurements");
				trySetFromYaml(process_preprocessor,		process_modes, {"! preprocessor"	}, "(bool) Preprocessing and quality checks");
				trySetFromYaml(process_spp,					process_modes, {"@ spp"				}, "(bool) Perform SPP on station data");
				trySetFromYaml(process_ppp,					process_modes, {"! ppp"				}, "(bool) Perform PPP network or end user mode");
			}

			{
				auto minimum_constraints = stringsToYamlObject(processing_options, {"! minimum_constraints"});

				trySetFromYaml(process_minimum_constraints,		minimum_constraints,	{"! enable"					}, "(bool) Transform states by minimal constraints to selected station coordinates");

				trySetKalmanFromYaml(minCOpts.scale,			minimum_constraints,	"! scale");
				trySetKalmanFromYaml(minCOpts.rotation,			minimum_constraints,	"! rotation");
				trySetKalmanFromYaml(minCOpts.translation,		minimum_constraints,	"! translation");

				trySetFromYaml(minCOpts.once_per_epoch,			minimum_constraints,	{"@ once_per_epoch"			},	"(bool) Perform minimum constraints on a temporary filter and output results once per epoch");
				trySetFromYaml(minCOpts.full_vcv,				minimum_constraints,	{"@ full_vcv"				},	"(bool) ! experimental ! Use full VCV for measurement noise in minimum constraints filter");
				trySetFromYaml(minCOpts.scale_by_vcv,			minimum_constraints,	{"@ scale_by_vcv"			},	"(bool) Use variance of positions as additional scaling factor in minimum constraints weighting");

				trySetEnumOpt( minCOpts.inverter, 				minimum_constraints,	{"@ inverter" 				}, E_Inverter::_from_string_nocase);
				trySetFromYaml(minCOpts.max_filter_iter,		minimum_constraints,	{"@ max_filter_iterations"	});
				trySetFromYaml(minCOpts.max_prefit_remv,		minimum_constraints,	{"@ max_prefit_removals"	}, 														"(int) Maximum number of measurements to exclude using prefit checks before attempting to filter");
				trySetFromYaml(minCOpts.sigma_check,			minimum_constraints,	{"@ outlier_screening", "@ sigma_check"			},										"(bool)  Enable prefit and postfit sigma check");
				trySetFromYaml(minCOpts.w_test,					minimum_constraints,	{"@ outlier_screening", "@ w_test"				},										"(bool)  Enable w-test");
				trySetFromYaml(minCOpts.chi_square_test,		minimum_constraints,	{"@ outlier_screening", "@ chi_square_test"		},										"(bool)  Enable Chi-square test");
				trySetEnumOpt( minCOpts.chi_square_mode,		minimum_constraints,	{"@ outlier_screening", "@ chi_square_mode"		}, E_ChiSqMode::_from_string_nocase,	"(enum)  Chi-square test mode - innovation, measurement, state");
				trySetFromYaml(minCOpts.sigma_threshold,		minimum_constraints,	{"@ outlier_screening", "@ sigma_threshold"		},										"(float) sigma threshold");
			}

			auto filter_options = stringsToYamlObject(processing_options, {"! filter_options"});
			{
				trySetFromYaml(joseph_stabilisation,			filter_options,	{"joseph_stabilisation"							});
				trySetEnumOpt( pppOpts.inverter, 				filter_options,	{"inverter" 									}, E_Inverter::_from_string_nocase, "Inverter to be used within the Kalman filter update stage, which may provide different performance outcomes in terms of processing time and accuracy and stability.");
				trySetFromYaml(process_rts,						filter_options,	{"! rts", "0!  enable"								}, "(bool) Perform backward smoothing of states to improve precision of earlier states");
				trySetFromYaml(pppOpts.rts_lag,					filter_options,	{"! rts", "1 lag"									}, "(int) Number of epochs to use in RTS smoothing. Negative numbers indicate full reverse smoothing.");
				trySetFromYaml(pppOpts.rts_directory,			filter_options,	{"! rts", "directory"								}, "(string) Directory for rts intermediate files");
				trySetFromYaml(pppOpts.rts_filename,			filter_options,	{"! rts", "filename"								}, "(string) Base filename for rts intermediate files");
				trySetFromYaml(pppOpts.rts_smoothed_suffix,		filter_options,	{"! rts", "suffix"								}, "(string) Suffix to be applied to smoothed versions of files");
				trySetEnumOpt( pppOpts.rts_inverter, 			filter_options,	{"! rts", "inverter" 								}, E_Inverter::_from_string_nocase, "Inverter to be used within the rts processor, which may provide different performance outcomes in terms of processing time and accuracy and stability.");
				trySetFromYaml(pppOpts.output_intermediate_rts,	filter_options, {"! rts", "output_intermediates"					}, "(bool) Output best available smoothed states when performing fixed-lag rts (slow, use only when needed)");
				trySetFromYaml(pppOpts.simulate_filter_only,	filter_options,	{"simulate_filter_only"							}, "(bool) Residuals will be calculated, but no adjustments to state or covariances will be applied");
				
				trySetFromYaml(pppOpts.max_filter_iter,			filter_options,	{"! outlier_screening",		"! max_filter_iterations"	});
				trySetFromYaml(pppOpts.sigma_check,				filter_options,	{"@ outlier_screening",		"@ sigma_check"				},										"(bool)  Enable prefit and postfit sigma check");
				trySetFromYaml(pppOpts.w_test,					filter_options,	{"@ outlier_screening",		"@ w_test"					},										"(bool)  Enable w-test");
				trySetFromYaml(pppOpts.chi_square_test,			filter_options,	{"@ outlier_screening",		"@ chi_square_test"			},										"(bool)  Enable Chi-square test");
				trySetEnumOpt( pppOpts.chi_square_mode,			filter_options,	{"@ outlier_screening",		"@ chi_square_mode"			}, E_ChiSqMode::_from_string_nocase,	"(enum)  Chi-square test mode - innovation, measurement, state");
				trySetFromYaml(pppOpts.sigma_threshold,			filter_options,	{"@ outlier_screening",		"@ sigma_threshold"			},										"(float) sigma threshold");
				trySetFromYaml(pppOpts.max_prefit_remv,			filter_options,	{"@ outlier_screening",		"@ max_prefit_removals"		}, 										"(int) Maximum number of measurements to exclude using prefit checks before attempting to filter");
				trySetFromYaml(pppOpts.chunk_size,				filter_options,	{"@ chunking", "size"								});
				trySetFromYaml(pppOpts.station_chunking,		filter_options,	{"@ station_chunking",		"@ enable"				}, "(bool) ");
				trySetFromYaml(pppOpts.satellite_chunking,		filter_options,	{"@ satellite_chunking",	"@ enable"				}, "(bool) ");
			}

			auto slr_options = stringsToYamlObject(processing_options, {"@ slr_options"});
			{
				trySetFromYaml(slrOpts.process_slr,				slr_options, {"@ process_slr"					}, "(bool) Process SLR observations (in addition to GNSS)");
			}
		}


		auto orbit_propagation = stringsToYamlObject({ yaml, "" }, {processing_options_str, "@ orbit_propagation"});
		{
			trySetFromYaml(orbitPropagation.central_force,				orbit_propagation, {"@ central_force"				});
			trySetFromYaml(orbitPropagation.planetary_perturbation,		orbit_propagation, {"@ planetary_perturbation"		});
			trySetFromYaml(orbitPropagation.indirect_J2,				orbit_propagation, {"@ indirect_J2"					});
			trySetFromYaml(orbitPropagation.egm_field,					orbit_propagation, {"@ egm_field"					});
			trySetFromYaml(orbitPropagation.solid_earth_tide,			orbit_propagation, {"@ solid_earth_tide"			});
			trySetFromYaml(orbitPropagation.ocean_tide,					orbit_propagation, {"@ ocean_tide"					});
			trySetFromYaml(orbitPropagation.general_relativity,			orbit_propagation, {"@ general_relativity"			});
			trySetFromYaml(orbitPropagation.pole_tide_ocean,			orbit_propagation, {"@ pole_tide_ocean"				});
			trySetFromYaml(orbitPropagation.pole_tide_solid,			orbit_propagation, {"@ pole_tide_solid"				});
			trySetFromYaml(orbitPropagation.solar_pressure_radiation, 	orbit_propagation, {"@ solar_pressure_radiation"	});
			trySetFromYaml(orbitPropagation.empirical_dyb,				orbit_propagation, {"@ empirical_dyb"				});
			trySetFromYaml(orbitPropagation.antenna_thrust,				orbit_propagation, {"@ antenna_thrust"				});
			trySetFromYaml(orbitPropagation.albedo, 					orbit_propagation, {"@ albedo"						});
			trySetFromYaml(orbitPropagation.sat_mass,					orbit_propagation, {"@ sat_mass"					});
			trySetFromYaml(orbitPropagation.sat_area, 					orbit_propagation, {"@ sat_area"					});
			trySetFromYaml(orbitPropagation.sat_power, 					orbit_propagation, {"@ sat_power"					});
			trySetFromYaml(orbitPropagation.srp_cr,						orbit_propagation, {"@ srp_cr"						});
			trySetFromYaml(orbitPropagation.degree_max,					orbit_propagation, {"@ degree_max"					});
			trySetFromYaml(orbitPropagation.itrf_pseudoobs,				orbit_propagation, {"@ itrf_pseudoobs"				});
			trySetFromYaml(orbitPropagation.integrator_time_step,		orbit_propagation, {"@ integrator_time_step"		});
		}
					
                                                      
// 		trySetFromYaml(split_sys,				outputs, { "split_sys"		});
                                                      
// 		trySetFromAny (output_persistance,		commandOpts, 	output_files, {"output_persistance"		});
// 		trySetFromAny (input_persistance,		commandOpts,	output_files, {"input_persistance"		});
// 		trySetFromYaml(persistance_directory,					output_files, {"persistance_directory"	});
// 		trySetFromYaml(persistance_filename,					output_files, {"persistance_filename"	});


		{
			auto ambres_options = stringsToYamlObject(processing_options, {"@ ambiguity_resolution"});

			trySetFromYaml(ambrOpts.min_el_AR,			ambres_options, {"@ elevation_mask"				});
			trySetFromYaml(ambrOpts.lambda_set,			ambres_options, {"@ lambda_set_size"			});
			trySetFromYaml(ambrOpts.AR_max_itr,			ambres_options, {"@ max_rounding_iterations"	});
			trySetFromYaml(ambrOpts.Max_Hold_epoc,		ambres_options, {"@ max_hold_epochs"			}, "Maximun number of epocs to hold ambiguities");
			trySetFromYaml(ambrOpts.Max_Hold_time,		ambres_options, {"@ max_hold_time"				}, "Maximun amount of time (sec) to hold ambiguities");
			
			trySetEnumOpt( ambrOpts.WLmode,				ambres_options,	{"@ wide_lane", "@ mode" 						}, E_ARmode::_from_string_nocase);
			trySetFromYaml(ambrOpts.WLsuccsThres,		ambres_options, {"@ wide_lane", "@ success_rate_threshold"		});
			trySetFromYaml(ambrOpts.WLratioThres,		ambres_options,	{"@ wide_lane", "@ solution_ratio_threshold"	});
			trySetFromYaml(ambrOpts.WLSatPrcNois,		ambres_options,	{"@ wide_lane", "@ process_noise_sat"			}, "WL bias process noise for satellite");
			trySetFromYaml(ambrOpts.WLRecPrcNois,		ambres_options,	{"@ wide_lane", "@ process_noise_rec"			}, "WL bias process noise for receivers");
			trySetFromYaml(ambrOpts.WL_filter_iter,		ambres_options, {"@ wide_lane", "@ filter_max_iterations"		});
			trySetFromYaml(ambrOpts.WL_prefit_remv,		ambres_options, {"@ wide_lane", "@ filter_max_removals"			});

			trySetEnumOpt( ambrOpts.NLmode,				ambres_options,	{"@ narrow_lane", "@ mode" 						}, E_ARmode::_from_string_nocase);
			trySetFromYaml(ambrOpts.NLsuccsThres,		ambres_options, {"@ narrow_lane", "@ success_rate_threshold"	});
			trySetFromYaml(ambrOpts.NLratioThres,		ambres_options, {"@ narrow_lane", "@ solution_ratio_threshold"	});
			trySetFromYaml(ambrOpts.NLstarttime,		ambres_options, {"@ narrow_lane", "@ proc_start"				});

			trySetFromYaml(ambrOpts.reduction_limit,	ambres_options, {"@ reduction_limit"							});
		}

		{
			auto ssr_corrections = stringsToYamlObject(processing_options, {"ssr_corrections"});

			trySetEnumVec (ssrOpts.ephemeris_sources, 		ssr_corrections, {"ephemeris_sources" 		});
			trySetEnumVec (ssrOpts.clock_sources, 			ssr_corrections, {"clock_sources" 			});
			trySetEnumVec (ssrOpts.code_bias_sources, 		ssr_corrections, {"code_bias_sources" 		});
			trySetEnumVec (ssrOpts.phase_bias_sources, 		ssr_corrections, {"phase_bias_sources" 		});
			trySetEnumVec (ssrOpts.ionosphere_sources, 		ssr_corrections, {"ionosphere_sources" 		});
			// trySetEnumVec (ssrOpts.troposphere_sources, 		ssr_corrections, {"troposphere_sources" 		});
			trySetEnumOpt (ssrOpts.output_timing, 			ssr_corrections, {"output_timing" 			}, E_SSROutTiming::_from_string_nocase);
			trySetFromYaml(ssrOpts.prediction_interval,		ssr_corrections, {"prediction_interval"		});
			trySetFromYaml(ssrOpts.prediction_duration,		ssr_corrections, {"prediction_duration"		});
			trySetFromYaml(ssrOpts.extrapolate_corrections,	ssr_corrections, {"extrapolate_corrections"	}, "(bool) ");
		}

		{
			auto ssr_inputs = stringsToYamlObject(processing_options, {"@ ssr_inputs"});

			trySetFromYaml(ssrInOpts.code_bias_valid_time,	ssr_inputs, {"@ code_bias_validity_time"	},	"(double) Valid time period of SSR code biases");
			trySetFromYaml(ssrInOpts.phase_bias_valid_time,	ssr_inputs, {"@ phase_bias_validity_time"	},	"(double) Valid time period of SSR phase biases");
			trySetFromYaml(ssrInOpts.one_freq_phase_bias,	ssr_inputs, {"@ one_freq_phase_bias"		},	"(bool)   Used stream have one SSR phase bias per frequency");
			trySetFromYaml(ssrInOpts.global_vtec_valid_time,ssr_inputs, {"@ global_vtec_valid_time"		},	"(double) Valid time period of global VTEC maps");
			trySetFromYaml(ssrInOpts.local_stec_valid_time,	ssr_inputs, {"@ local_stec_valid_time"		},	"(double) Valid time period of local STEC corrections");
			trySetFromYaml(validity_interval_factor,		ssr_inputs, {"@ validity_interval_factor"	});
			trySetEnumOpt(ssr_input_antenna_offset,			ssr_inputs,	{"@ ssr_antenna_offset"			}, E_OffsetType::_from_string_nocase, "Ephemeris type that is provided in the listed SSR stream, i.e. satellite antenna-phase-centre (APC) or centre-of-mass (COM). This information is listed in the NTRIP Caster's sourcetable");
		}

		auto estimation_parameters = stringsToYamlObject({yaml, ""}, {estimation_parameters_str});
		{
			trySetKalmanFromYaml(pppOpts.eop,		estimation_parameters, "@ eop"			);
			trySetKalmanFromYaml(pppOpts.eop_rates,	estimation_parameters, "@ eop_rates"	);
			trySetKalmanFromYaml(ionModelOpts.ion,	estimation_parameters, "@ ion"			);
		}

		{
			auto mongo = stringsToYamlObject({yaml, ""}, {"5!  mongo"});

			trySetFromYaml(localMongo.enable,						mongo, {"0!  enable"					}, "(bool)   Enable and connect to mongo database");
			trySetFromYaml(localMongo.predict_states,				mongo, {"@ predict_states"				}, "(bool)   ");
			trySetFromYaml(localMongo.output_rtcm_messages,			mongo, {"@ output_rtcm_messages"		}, "(bool)   Output rtcm data to mongo");
			trySetFromYaml(localMongo.output_measurements,			mongo, {"! output_measurements"			}, "(bool)   Output measurements and their residuals");
			trySetFromYaml(localMongo.output_components,			mongo, {"! output_components"			}, "(bool)   Output components of measurements");
			trySetFromYaml(localMongo.output_states,				mongo, {"! output_states"				}, "(bool)   Output states");
			trySetFromYaml(localMongo.output_trace,					mongo, {"@ output_trace"				}, "(bool)   Output trace");
			trySetFromYaml(localMongo.output_test_stats,			mongo, {"@ output_test_stats"			}, "(bool)   Output test statistics");
			trySetFromYaml(localMongo.output_logs,					mongo, {"@ output_logs"					}, "(bool)   Output console trace and warnings to mongo with timestamps and other metadata");
			trySetFromYaml(localMongo.output_ssr_precursors,		mongo, {"@ output_ssr_precursors"		}, "(bool) ");
			trySetFromYaml(localMongo.delete_history,				mongo, {"! delete_history"				}, "(bool)   Drop the collection in the database at the beginning of the run to only show fresh data");
			trySetFromYaml(localMongo.cull_history,					mongo, {"@ cull_history"				}, "(bool)  ");
			trySetFromYaml(localMongo.min_cull_age,					mongo, {"@ min_cull_age"				}, "(float)  ");
			trySetFromYaml(localMongo.suffix,						mongo, {"@ suffix"						}, "(string) Suffix to append to database elements to make distinctions between runs for comparison");
			trySetFromYaml(localMongo.database,						mongo, {"@ database"					}, "(string) ");
			trySetFromYaml(localMongo.uri,							mongo, {"@ uri"							}, "(string) Location and port of the mongo database to connect to");
			
			trySetScaledFromYaml(localMongo.prediction_interval,			mongo, {"@ prediction_interval"			},	{"@ interval_units"	},	E_Period::_from_string_nocase);
			trySetScaledFromYaml(localMongo.forward_prediction_duration,	mongo, {"@ forward_prediction_duration"	},	{"@ duration_units"	},	E_Period::_from_string_nocase);
			trySetScaledFromYaml(localMongo.reverse_prediction_duration,	mongo, {"@ reverse_prediction_duration"	},	{"@ duration_units"	},	E_Period::_from_string_nocase);
		}

		{
			auto mongo = stringsToYamlObject({yaml, ""}, {"5@ remote_mongo"});

			trySetFromYaml(remoteMongo.enable,						mongo, {"0 enable"						}, "(bool)   Enable and connect to mongo database");
			trySetFromYaml(remoteMongo.predict_states,				mongo, {"@ predict_states"				}, "(bool)   ");
			trySetFromYaml(remoteMongo.output_rtcm_messages,		mongo, {"@ output_rtcm_messages"		}, "(bool)   Output rtcm data to mongo");
			trySetFromYaml(remoteMongo.output_measurements,			mongo, {"@ output_measurements"			}, "(bool)   Output measurements and their residuals");
			trySetFromYaml(remoteMongo.output_components,			mongo, {"@ output_components"			}, "(bool)   Output components of measurements");
			trySetFromYaml(remoteMongo.output_states,				mongo, {"@ output_states"				}, "(bool)   Output states");
			trySetFromYaml(remoteMongo.output_trace,				mongo, {"@ output_trace"				}, "(bool)   Output trace");
			trySetFromYaml(remoteMongo.output_test_stats,			mongo, {"@ output_test_stats"			}, "(bool)   Output test statistics");
			trySetFromYaml(remoteMongo.output_logs,					mongo, {"@ output_logs"					}, "(bool)   Output console trace and warnings to mongo with timestamps and other metadata");
			trySetFromYaml(remoteMongo.output_ssr_precursors,		mongo, {"@ output_ssr_precursors"		}, "(bool) ");
			trySetFromYaml(remoteMongo.delete_history,				mongo, {"@ delete_history"				}, "(bool)   Drop the collection in the database at the beginning of the run to only show fresh data");
			trySetFromYaml(remoteMongo.cull_history,				mongo, {"@ cull_history"				}, "(bool)  ");
			trySetFromYaml(remoteMongo.min_cull_age,				mongo, {"@ min_cull_age"				}, "(float)  ");
			trySetFromYaml(remoteMongo.suffix,						mongo, {"@ suffix"						}, "(string) Suffix to append to database elements to make distinctions between runs for comparison");
			trySetFromYaml(remoteMongo.database,					mongo, {"@ database"					}, "(string) ");
			trySetFromYaml(remoteMongo.uri,							mongo, {"@ uri"							}, "(string) Location and port of the mongo database to connect to");
		
			trySetScaledFromYaml(remoteMongo.prediction_interval,			mongo, {"@ prediction_interval"			},	{"@ interval_units"	},	E_Period::_from_string_nocase);
			trySetScaledFromYaml(remoteMongo.forward_prediction_duration,	mongo, {"@ forward_prediction_duration"	},	{"@ duration_units"	},	E_Period::_from_string_nocase);
			trySetScaledFromYaml(remoteMongo.reverse_prediction_duration,	mongo, {"@ reverse_prediction_duration"	},	{"@ duration_units"	},	E_Period::_from_string_nocase);
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
	}
	
	tryAddRootToPath(root_input_directory, vmf_files);				globber(vmf_files);
	tryAddRootToPath(root_input_directory, atx_files);				globber(atx_files);
	tryAddRootToPath(root_input_directory, snx_files);				globber(snx_files);
	tryAddRootToPath(root_input_directory, blq_files);				globber(blq_files);
	tryAddRootToPath(root_input_directory, erp_files);				globber(erp_files);
	tryAddRootToPath(root_input_directory, ion_files);				globber(ion_files);
	tryAddRootToPath(root_input_directory, nav_files);				globber(nav_files);
	tryAddRootToPath(root_input_directory, sp3_files);				globber(sp3_files);
	tryAddRootToPath(root_input_directory, dcb_files);				globber(dcb_files);
	tryAddRootToPath(root_input_directory, bsx_files);				globber(bsx_files);
	tryAddRootToPath(root_input_directory, igrf_files);				globber(igrf_files);
	tryAddRootToPath(root_input_directory, clk_files);				globber(clk_files);
	tryAddRootToPath(root_input_directory, obx_files);				globber(obx_files);
	tryAddRootToPath(root_input_directory, orb_files);				globber(orb_files);
	tryAddRootToPath(root_input_directory, sid_files);				globber(sid_files);
	tryAddRootToPath(root_input_directory, com_files);				globber(com_files);
	tryAddRootToPath(root_input_directory, crd_files);				globber(crd_files);
	tryAddRootToPath(root_input_directory, egm_files);				globber(egm_files);
	tryAddRootToPath(root_input_directory, jpl_files);				globber(jpl_files);
	tryAddRootToPath(root_input_directory, tide_files);				globber(tide_files);

																	globber(rnx_inputs);
																	globber(ubx_inputs);
																	globber(obs_rtcm_inputs);
																	globber(nav_rtcm_inputs);
																	globber(pseudo_sp3_inputs);
																	globber(pseudo_snx_inputs);

	tryAddRootToPath(root_input_directory, model.trop.orography);
	tryAddRootToPath(root_input_directory, model.trop.gpt2grid);
	
	string revert_trace		= trace_directory;
	string revert_sp3		= sp3_directory;
	
	tryPatchPaths(root_output_directory,	sp3_directory,							sp3_filename);			sp3_directory = revert_sp3;
	tryPatchPaths(root_output_directory,	erp_directory,							erp_filename);
	tryPatchPaths(root_output_directory,	gpx_directory,							gpx_filename);
	tryPatchPaths(root_output_directory,	log_directory,							log_filename);
	tryPatchPaths(root_output_directory,	cost_directory,							cost_filename);
	tryPatchPaths(root_output_directory,	test_directory,							test_filename);
	tryPatchPaths(root_output_directory,	sinex_directory,						sinex_filename);
	tryPatchPaths(root_output_directory,	ionex_directory,						ionex_filename);
	tryPatchPaths(root_output_directory,	orbex_directory,						orbex_filename);
	tryPatchPaths(root_output_directory,	clocks_directory,						clocks_filename);
	tryPatchPaths(root_output_directory,	slr_obs_directory,						slr_obs_filename);
	tryPatchPaths(root_output_directory,	ionstec_directory,						ionstec_filename);
	tryPatchPaths(root_output_directory,	ppp_sol_directory,						ppp_sol_filename);
	tryPatchPaths(root_output_directory,	raw_ubx_directory,						raw_ubx_filename);
	tryPatchPaths(root_output_directory,	rtcm_nav_directory,						rtcm_nav_filename);
	tryPatchPaths(root_output_directory,	rtcm_obs_directory,						rtcm_obs_filename);
	tryPatchPaths(root_output_directory,	orbit_ics_directory,					orbit_ics_filename);
	tryPatchPaths(root_output_directory,	ntrip_log_directory,					ntrip_log_filename);
	tryPatchPaths(root_output_directory,	rinex_obs_directory,					rinex_obs_filename);
	tryPatchPaths(root_output_directory,	rinex_nav_directory,					rinex_nav_filename);
	tryPatchPaths(root_output_directory,	sp3_directory,							predicted_sp3_filename);
	tryPatchPaths(root_output_directory,	bias_sinex_directory,					bias_sinex_filename);
	tryPatchPaths(root_output_directory,	trop_sinex_directory,					trop_sinex_filename);
	tryPatchPaths(root_output_directory,	persistance_directory,					persistance_filename);
	tryPatchPaths(root_output_directory,	pppOpts.rts_directory,					pppOpts.rts_filename);
	tryPatchPaths(root_output_directory,	trace_directory,						satellite_trace_filename);	trace_directory = revert_trace;
	tryPatchPaths(root_output_directory,	trace_directory,						station_trace_filename);	trace_directory = revert_trace;
	tryPatchPaths(root_output_directory,	trace_directory,						network_trace_filename);
	tryPatchPaths(root_output_directory,	decoded_rtcm_json_directory,			decoded_rtcm_json_filename);
	tryPatchPaths(root_output_directory,	encoded_rtcm_json_directory,			encoded_rtcm_json_filename);
	tryPatchPaths(root_output_directory,	network_statistics_json_directory,		network_statistics_json_filename);
	
	//Try to change all filenames to replace <YYYY> etc with other values.
	replaceTags(nav_files);
	replaceTags(orb_files);
	replaceTags(sp3_files);
	replaceTags(clk_files);
	replaceTags(obx_files);
	replaceTags(atx_files);
	replaceTags(snx_files);
	replaceTags(blq_files);
	replaceTags(erp_files);
	replaceTags(dcb_files);
	replaceTags(bsx_files);
	replaceTags(ion_files);
	replaceTags(egm_files);
	replaceTags(sid_files);
	replaceTags(com_files);
	replaceTags(crd_files);
	replaceTags(jpl_files);
	
	replaceTags(rnx_inputs);
	replaceTags(ubx_inputs);
	replaceTags(obs_rtcm_inputs);
	replaceTags(nav_rtcm_inputs);
	replaceTags(pseudo_sp3_inputs);
	replaceTags(pseudo_snx_inputs);
	
	replaceTags(localMongo.suffix);
	replaceTags(localMongo.database);
	
	replaceTags(remoteMongo.suffix);
	replaceTags(remoteMongo.database);
	
	SatSys dummySat;
				
	getSatOpts(dummySat);
	getRecOpts("global");
	getRecOpts("XMPL");
	
	
#	ifndef ENABLE_UNIT_TESTS
	if (testOpts.enable)
	{
		std::cout << std::endl << "Error: Tests requested by config but this is a non-test binary."				<< std::endl;
		std::cout << std::endl << "Tests can be enabled by building with $ cmake -DENABLE_UNIT_TESTS=ON .."		<< std::endl;
		exit(1);
	}
#	endif

	for (auto& yaml : yamls)
	{
		recurseYaml(yaml);
	}
	
	if (commandOpts.count("yaml-defaults"))
	{
		int level = commandOpts["yaml-defaults"].as<int>();
		outputDefaultConfiguration(level);
	}
	
	return true;
}
