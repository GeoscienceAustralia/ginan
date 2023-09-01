
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

void conditionalPrefix(
	string	prefix,
	string&	path,
	bool	condition = true)
{
	if (condition == false)
	{
		return;
	}
	
	if (path.empty())					{	return;	}
	if (path.find(':') != string::npos)	{	return;	}
	
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
	
	if (prefix.back() == '/')	path = prefix		+ path;
	else						path = prefix + "/"	+ path;
}

void conditionalPrefix(
	string			prefix,
	vector<string>& paths,
	bool			condition = true)
{
	for (auto& path : paths)
	{
		conditionalPrefix(prefix, path, condition);
	}
}

void conditionalPrefix(
	string							prefix,
	map<string, vector<string>>&	paths,
	bool							condition = true)
{
	for (auto& [id, path] : paths)
	{
		conditionalPrefix(prefix, path, condition);
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
		
		if (replacement.back() == '/')	str.insert(index, replacement.substr(0, replacement.size() -1));
		else							str.insert(index, replacement);
		
		replaced = true;
	}
	
	return replaced;
}

/** Replace macros for times with configured values.
* Available replacements are "<CONFIG> <USER> <PASS> <BRANCH> <HASH> <AGENCY> <SOFTWARE>"
*/
void replaceTags(
	string&			str,		///< String to replace macros within
	string			custom = "")
{
	replaceString(str, "<SAT_DATA_ROOT>",					acsConfig.sat_data_root);
	replaceString(str, "<GNSS_OBS_ROOT>",					acsConfig.gnss_obs_root);
	replaceString(str, "<PSEUDO_OBS_ROOT>",					acsConfig.pseudo_obs_root);
	replaceString(str, "<HASH>",							ginanCommitHash());
	replaceString(str, "<BRANCH>",							ginanBranchName());
	replaceString(str, "<AGENCY>",							acsConfig.analysis_agency);
	replaceString(str, "<SOFTWARE>",						acsConfig.analysis_program.substr(0,3));
	replaceString(str, "<INPUTS_ROOT>",						acsConfig.inputs_root);
	replaceString(str, "<TRACE_DIRECTORY>",					acsConfig.trace_directory);
	replaceString(str, "<BIAS_SINEX_DIRECTORY>",			acsConfig.bias_sinex_directory);
	replaceString(str, "<CLOCKS_DIRECTORY>",				acsConfig.clocks_directory);
	replaceString(str, "<DECODED_RTCM_DIRECTORY>",			acsConfig.decoded_rtcm_json_directory);
	replaceString(str, "<ENCODED_RTCM_DIRECTORY>",			acsConfig.encoded_rtcm_json_directory);
	replaceString(str, "<ERP_DIRECTORY>",					acsConfig.erp_directory);
	replaceString(str, "<IONEX_DIRECTORY>",					acsConfig.ionex_directory);
	replaceString(str, "<IONSTEC_DIRECTORY>",				acsConfig.ionstec_directory);
	replaceString(str, "<SINEX_DIRECTORY>",					acsConfig.sinex_directory);
	replaceString(str, "<LOG_DIRECTORY>",					acsConfig.log_directory);
	replaceString(str, "<GPX_DIRECTORY>",					acsConfig.gpx_directory);
	replaceString(str, "<NTRIP_LOG_DIRECTORY>",				acsConfig.ntrip_log_directory);
	replaceString(str, "<NETWORK_STATISTICS_DIRECTORY>",	acsConfig.network_statistics_json_directory);
	replaceString(str, "<SP3_DIRECTORY>",					acsConfig.sp3_directory);
	replaceString(str, "<ORBIT_ICS_DIRECTORY>",				acsConfig.orbit_ics_directory);
	replaceString(str, "<ORBEX_DIRECTORY>",					acsConfig.orbex_directory);
	replaceString(str, "<PPP_SOL_DIRECTORY>",				acsConfig.ppp_sol_directory);
	replaceString(str, "<COST_DIRECTORY>",					acsConfig.cost_directory);
	replaceString(str, "<RINEX_NAV_DIRECTORY>",				acsConfig.rinex_nav_directory);
	replaceString(str, "<RINEX_OBS_DIRECTORY>",				acsConfig.rinex_obs_directory);
	replaceString(str, "<RTCM_NAV_DIRECTORY>",				acsConfig.rtcm_nav_directory);
	replaceString(str, "<RTCM_OBS_DIRECTORY>",				acsConfig.rtcm_obs_directory);
	replaceString(str, "<UBX_DIRECTORY>",					acsConfig.raw_ubx_directory);
	replaceString(str, "<SLR_OBS_DIRECTORY>",				acsConfig.slr_obs_directory);
	replaceString(str, "<TROP_SINEX_DIRECTORY>",			acsConfig.trop_sinex_directory);
	replaceString(str, "<OUTPUTS_ROOT>",					acsConfig.outputs_root);
	replaceString(str, "<USER>",							acsConfig.stream_user);
	replaceString(str, "<PASS>",							acsConfig.stream_pass);
	replaceString(str, "<CONFIG>",							acsConfig.config_description);
}

void replaceTags(
	vector<string>&		strs,
	string				custom = "")
{
	for (auto& str : strs)
	{
		replaceTags(str);
	}
}

void replaceTags(
	map<string, vector<string>>&	strs,
	string							custom = "")
{
	for (auto& [id, str] : strs)
	{
		replaceTags(str);
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

void dumpConfig(
	Trace& trace)
{
	for (auto& filename : acsConfig.includedFilenames)
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
	int				level,			///< Level of complexity for outputs
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
				md << std::endl << std::endl;
				
				if (nextIsChild)
					md << "> ";
				
				md << comment;//.substr(0, period);
// 				if	( period != string::npos
// 					&&period + 2 < comment.size())
// 				{	
// 					md << std::endl << std::endl <<		comment.substr(period + 2);
// 				}
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
	
	md << std::endl << "This document outlines the major configuration options available in ginan that are most applicable to general users. "
	<< "For more advanced configuration options and their defaults, use the `-Y <level>` option at the command line to view increasing levels of advanced configurations.";
	
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

void defaultConfigs()
{
	acsConfig.recOptsMap["0"].rinex23Conv.codeConv = 
	{
		{E_Sys::GPS,{	
						{E_ObsCode2::P1, E_ObsCode::L1W},
						{E_ObsCode2::P2, E_ObsCode::L2W},
						{E_ObsCode2::C1, E_ObsCode::L1C},
						{E_ObsCode2::C2, E_ObsCode::L2C},
						{E_ObsCode2::C5, E_ObsCode::L5X},
						{E_ObsCode2::L1, E_ObsCode::L1W},
						{E_ObsCode2::L2, E_ObsCode::L2C},
						{E_ObsCode2::L5, E_ObsCode::L5X}}
		},
						
		{E_Sys::GAL,{	
						{E_ObsCode2::P1, E_ObsCode::L1P},
						{E_ObsCode2::P2, E_ObsCode::L2P},
						{E_ObsCode2::C1, E_ObsCode::L1C},
						{E_ObsCode2::C2, E_ObsCode::L2C},
						{E_ObsCode2::C5, E_ObsCode::L5I},
						{E_ObsCode2::C7, E_ObsCode::L7Q},
						{E_ObsCode2::C8, E_ObsCode::L8Q},
						{E_ObsCode2::L1, E_ObsCode::L1C},
						{E_ObsCode2::L2, E_ObsCode::L2P},
						{E_ObsCode2::L5, E_ObsCode::L5I},
						{E_ObsCode2::L7, E_ObsCode::L7Q},
						{E_ObsCode2::L8, E_ObsCode::L8Q}}
		},
						
		{E_Sys::GLO,{
						{E_ObsCode2::P1, E_ObsCode::L1P},
						{E_ObsCode2::P2, E_ObsCode::L2P},
						{E_ObsCode2::C1, E_ObsCode::L1C},
						{E_ObsCode2::C2, E_ObsCode::L2C},
						{E_ObsCode2::C3, E_ObsCode::L3Q},
						{E_ObsCode2::C5, E_ObsCode::L5I},
						{E_ObsCode2::L1, E_ObsCode::L1C},
						{E_ObsCode2::L2, E_ObsCode::L2P},
						{E_ObsCode2::L3, E_ObsCode::L3Q},
						{E_ObsCode2::L5, E_ObsCode::L5I}}
		},
						
		{E_Sys::BDS,{
						{E_ObsCode2::C2, E_ObsCode::L2I},
						{E_ObsCode2::C6, E_ObsCode::L6I},
						{E_ObsCode2::C7, E_ObsCode::L7I},
						{E_ObsCode2::C8, E_ObsCode::L8X},
						{E_ObsCode2::L2, E_ObsCode::L2X},
						{E_ObsCode2::L6, E_ObsCode::L6I},
						{E_ObsCode2::L7, E_ObsCode::L7I},
						{E_ObsCode2::L8, E_ObsCode::L8X}}
		},
						
		{E_Sys::QZS,{	
						{E_ObsCode2::C1, E_ObsCode::L1C},
						{E_ObsCode2::C2, E_ObsCode::L2C},
						{E_ObsCode2::C5, E_ObsCode::L5I},
						{E_ObsCode2::C6, E_ObsCode::L6X},
						{E_ObsCode2::L1, E_ObsCode::L1C},
						{E_ObsCode2::L2, E_ObsCode::L2C},
						{E_ObsCode2::L5, E_ObsCode::L5I},
						{E_ObsCode2::L6, E_ObsCode::L6X}}
		},
						
		{E_Sys::SBS,{
						{E_ObsCode2::C1, E_ObsCode::L1C},
						{E_ObsCode2::C2, E_ObsCode::L2C},
						{E_ObsCode2::C5, E_ObsCode::L5I},
						{E_ObsCode2::L1, E_ObsCode::L1C},
						{E_ObsCode2::L2, E_ObsCode::L2C},
						{E_ObsCode2::L5, E_ObsCode::L5I}}
		}
	};

	acsConfig.recOptsMap["0"].rinex23Conv.phasConv =
	{
		{E_Sys::GPS,{	
						{E_ObsCode2::P1, E_ObsCode::L1W},
						{E_ObsCode2::P2, E_ObsCode::L2W},
						{E_ObsCode2::C1, E_ObsCode::L1C},
						{E_ObsCode2::C2, E_ObsCode::L2C},
						{E_ObsCode2::C5, E_ObsCode::L5X},
						{E_ObsCode2::L1, E_ObsCode::L1W},
						{E_ObsCode2::L2, E_ObsCode::L2C},
						{E_ObsCode2::L5, E_ObsCode::L5X}}
		},
						
		{E_Sys::GAL,{	
						{E_ObsCode2::P1, E_ObsCode::L1P},
						{E_ObsCode2::P2, E_ObsCode::L2P},
						{E_ObsCode2::C1, E_ObsCode::L1C},
						{E_ObsCode2::C2, E_ObsCode::L2C},
						{E_ObsCode2::C5, E_ObsCode::L5I},
						{E_ObsCode2::C7, E_ObsCode::L7Q},
						{E_ObsCode2::C8, E_ObsCode::L8Q},
						{E_ObsCode2::L1, E_ObsCode::L1C},
						{E_ObsCode2::L2, E_ObsCode::L2P},
						{E_ObsCode2::L5, E_ObsCode::L5I},
						{E_ObsCode2::L7, E_ObsCode::L7Q},
						{E_ObsCode2::L8, E_ObsCode::L8Q}}
		},
						
		{E_Sys::GLO,{
						{E_ObsCode2::P1, E_ObsCode::L1P},
						{E_ObsCode2::P2, E_ObsCode::L2P},
						{E_ObsCode2::C1, E_ObsCode::L1C},
						{E_ObsCode2::C2, E_ObsCode::L2C},
						{E_ObsCode2::C3, E_ObsCode::L3Q},
						{E_ObsCode2::C5, E_ObsCode::L5I},
						{E_ObsCode2::L1, E_ObsCode::L1C},
						{E_ObsCode2::L2, E_ObsCode::L2P},
						{E_ObsCode2::L3, E_ObsCode::L3Q},
						{E_ObsCode2::L5, E_ObsCode::L5I}}
		},
						
		{E_Sys::BDS,{
						{E_ObsCode2::C2, E_ObsCode::L2I},
						{E_ObsCode2::C6, E_ObsCode::L6I},
						{E_ObsCode2::C7, E_ObsCode::L7I},
						{E_ObsCode2::C8, E_ObsCode::L8X},
						{E_ObsCode2::L2, E_ObsCode::L2I},
						{E_ObsCode2::L6, E_ObsCode::L6I},
						{E_ObsCode2::L7, E_ObsCode::L7I},
						{E_ObsCode2::L8, E_ObsCode::L8X}}
		},
						
		{E_Sys::QZS,{	
						{E_ObsCode2::C1, E_ObsCode::L1C},
						{E_ObsCode2::C2, E_ObsCode::L2C},
						{E_ObsCode2::C5, E_ObsCode::L5I},
						{E_ObsCode2::C6, E_ObsCode::L6X},
						{E_ObsCode2::L1, E_ObsCode::L1C},
						{E_ObsCode2::L2, E_ObsCode::L2C},
						{E_ObsCode2::L5, E_ObsCode::L5I},
						{E_ObsCode2::L6, E_ObsCode::L6X}}
		},
						
		{E_Sys::SBS,{
						{E_ObsCode2::C1, E_ObsCode::L1C},
						{E_ObsCode2::C2, E_ObsCode::L2C},
						{E_ObsCode2::C5, E_ObsCode::L5I},
						{E_ObsCode2::L1, E_ObsCode::L1C},
						{E_ObsCode2::L2, E_ObsCode::L2C},
						{E_ObsCode2::L5, E_ObsCode::L5I}}
		}
	};
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
	("blq_files",				boost::program_options::value<vector<string>>()->multitoken(),	"BLQ (Ocean loading) files")
	("erp_files",				boost::program_options::value<vector<string>>()->multitoken(),	"ERP files")
	("rnx_inputs,r",			boost::program_options::value<vector<string>>()->multitoken(),	"RINEX station inputs")
	("ubx_inputs",				boost::program_options::value<vector<string>>()->multitoken(),	"UBX station inputs")
	("rtcm_inputs",				boost::program_options::value<vector<string>>()->multitoken(),	"RTCM station inputs")
	("egm_files",				boost::program_options::value<vector<string>>()->multitoken(),	"Earth gravity model coefficients file")
	("crd_files",				boost::program_options::value<vector<string>>()->multitoken(),	"SLR CRD file")
	("slr_inputs",				boost::program_options::value<vector<string>>()->multitoken(),	"Tabular SLR OBS station file")
	("jpl_files",				boost::program_options::value<vector<string>>()->multitoken(),	"JPL planetary and lunar ephemerides file")
	("inputs_root",				boost::program_options::value<string>(),						"Root to apply to non-absolute input locations")
	("outputs_root",			boost::program_options::value<string>(),						"Root to apply to non-absolute output locations")
	("start_epoch",				boost::program_options::value<string>(),						"Start date/time")
	("end_epoch",				boost::program_options::value<string>(),						"Stop date/time")
// 	("run_rts_only",			boost::program_options::value<string>(),						"RTS filename (without _xxxxx suffix)")
	("dump-config-only",																		"Dump the configuration and exit")
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
	valid &= checkValidFiles(acsConfig.egm_files, 			"Earth gravity model coefficients (egm file)");
	valid &= checkValidFiles(acsConfig.jpl_files, 			"JPL planetary and lunar ephemerides (jpl file)");
	valid &= checkValidFiles(acsConfig.tide_files, 			"Ocean tide file FES (tide file)");
    valid &= checkValidFiles(acsConfig.cmc_files, 			"Center of Mass tide file corrections");
    valid &= checkValidFiles(acsConfig.hfeop_files,			"Subdaily EOP variations");

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
	if (!egm_files				.empty())	{	ss << "\tegm_files:   ";												for (auto& a : egm_files)		ss << a << " ";		ss << "\n";		}
 	if (!jpl_files				.empty())	{	ss << "\tjpl_files:   ";												for (auto& a : jpl_files)		ss << a << " ";		ss << "\n";		}
	if (!tide_files				.empty())	{	ss << "\ttide_files:  ";												for (auto& a : tide_files)		ss << a << " ";		ss << "\n";		}
    if (!cmc_files				.empty())	{	ss << "\tcmc_files:   ";							    				for (auto& a : cmc_files)		ss << a << " ";		ss << "\n";		}
    if (!hfeop_files			.empty())	{	ss << "\thfeop_files: ";							    				for (auto& a : hfeop_files)		ss << a << " ";		ss << "\n";		}

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
		
		//override comment if it hasnt been set yet
		if	( i == yamlNodeDescriptor.size() - 1
			&&acsConfig.yamlDefaults[stack].comment.empty()
			&&comment.empty() == false)
		{
			acsConfig.yamlDefaults[stack].comment = comment;
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
// 		std::cout << stack << " was found\n";
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
	vector<ENUM>&	enumVector,						///< Output vector for enum configurations
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
	setInited(output,	output.apriori_val,	trySetFromYaml(output.apriori_val,	newYaml, {"3  apriori_val"		}, "[floats] Apriori state values"));
	setInited(output,	output.apriori_val,	trySetFromYaml(output.apriori_val,	newYaml, {"3! apriori_value"	}, "[floats] Apriori state values"));
	setInited(output,	output.proc_noise,	trySetFromYaml(output.proc_noise,	newYaml, {"2  proc_noise"	 	}, "[floats] Process noise sigmas"));
	setInited(output,	output.proc_noise,	trySetFromYaml(output.proc_noise,	newYaml, {"2! process_noise" 	}, "[floats] Process noise sigmas"));
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

bool tryGetMappedList(
	map<string, vector<string>>&			mappedList,
	boost::program_options::variables_map&	commandOpts,	///< Command line object to search within
	NodeStack&								yaml,
	string									key,
	string									prefix,
	string									comment = "")
{
	auto [outStreamNode, outStreamString] = stringsToYamlObject(yaml, {key});

	bool found = false;
	
	for (auto outLabelYaml : outStreamNode)
	{
		found = true;
		
		if (outLabelYaml.IsScalar())
		{
			string value = outLabelYaml.as<string>();
			
			conditionalPrefix(prefix, value);	
			
			mappedList["<AUTO>"].push_back(value);
		}
		if (outLabelYaml.IsMap())
		{
			for (auto it = outLabelYaml.begin(); it != outLabelYaml.end(); it++)
			{
				string key = it->first.as<string>();
				
				mappedList[key] = it->second.as<vector<string>>();
				conditionalPrefix(prefix, mappedList[key]);
			}
		}
	}
	
	vector<string> optsList;
	found |= trySetFromOpts(optsList, commandOpts, {key});
	
	for (auto& value : optsList)
	{
		conditionalPrefix(prefix, value);
		
		if (std::find(mappedList["<AUTO>"].begin(), mappedList["<AUTO>"].end(), value) != mappedList["<AUTO>"].end())
		{
			continue;
		}
		
		mappedList["<AUTO>"].push_back(value);
	}
	
	
	
	
	return found;
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
	
	trySetFromYaml(outStreamData.itrf_datum, 		outStreamsYaml, {"itrf_datum"			});
	trySetFromYaml(outStreamData.provider_id, 		outStreamsYaml, {"provider_id"			});
	trySetFromYaml(outStreamData.solution_id, 		outStreamsYaml, {"solution_id"			});
}

/** Set satellite options from yaml
*/
void getFromYaml(
	SatelliteOptions&	satOpts,			///< Satellite options variable to output to
	NodeStack			yamlBase,			///< Yaml node to search within
	vector<string>		yamlNodeDescriptor)	///< List of strings of keys of yaml hierarchy
{
	auto satNode = stringsToYamlObject(yamlBase, yamlNodeDescriptor);

	trySetKalmanFromYaml(satOpts.clk,					satNode, "# clock",					"Clocks (duplicate)");
	trySetKalmanFromYaml(satOpts.clk_rate,				satNode, "# clock_rate",			"Clock rates (duplicate)");
	trySetKalmanFromYaml(satOpts.clk,					satNode, "! clk",					"Clocks");
	trySetKalmanFromYaml(satOpts.clk_rate,				satNode, "! clk_rate",				"Clock rates");
	trySetKalmanFromYaml(satOpts.pos,					satNode, "! pos",					"Position (prefer orbit)");
	trySetKalmanFromYaml(satOpts.pos_rate,				satNode, "! pos_rate",				"Velocity (prefer orbit)");
	trySetKalmanFromYaml(satOpts.pco,					satNode, "pco",						"Phase Center Offsets (experimental)");
	trySetKalmanFromYaml(satOpts.ant,					satNode, "ant",						"Antenna offsets (experimental)");
	trySetKalmanFromYaml(satOpts.orbit,					satNode, "! orbit",					"Orbital state");
	trySetKalmanFromYaml(satOpts.code_bias,				satNode, "! code_bias",				"Code bias");
	trySetKalmanFromYaml(satOpts.phase_bias,			satNode, "! phase_bias",			"Phase bias");
	
	trySetKalmanFromYaml(satOpts.emp_dyb_0,				satNode, "! emp_dyb_0",				"Empirical accleration bias ");
	trySetKalmanFromYaml(satOpts.emp_dyb_1c,			satNode, "! emp_dyb_1c",			"Empirical accleration 1 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_1s,			satNode, "! emp_dyb_1s",			"Empirical accleration 1 per rev sine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_2c,			satNode, "! emp_dyb_2c",			"Empirical accleration 2 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_2s,			satNode, "! emp_dyb_2s",			"Empirical accleration 2 per rev sine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_3c,			satNode, "! emp_dyb_3c",			"Empirical accleration 3 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_3s,			satNode, "! emp_dyb_3s",			"Empirical accleration 3 per rev sine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_4c,			satNode, "! emp_dyb_4c",			"Empirical accleration 4 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_dyb_4s,			satNode, "! emp_dyb_4s",			"Empirical accleration 4 per rev sine term ");
	                                                               
	trySetKalmanFromYaml(satOpts.emp_rtn_0,				satNode, "! emp_rtn_0",				"Empirical accleration bias ");
	trySetKalmanFromYaml(satOpts.emp_rtn_1c,			satNode, "! emp_rtn_1c",			"Empirical accleration 1 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_rtn_1s,			satNode, "! emp_rtn_1s",			"Empirical accleration 1 per rev sine term ");
	trySetKalmanFromYaml(satOpts.emp_rtn_2c,			satNode, "! emp_rtn_2c",			"Empirical accleration 2 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_rtn_2s,			satNode, "! emp_rtn_2s",			"Empirical accleration 2 per rev sine term ");
	trySetKalmanFromYaml(satOpts.emp_rtn_3c,			satNode, "! emp_rtn_3c",			"Empirical accleration 3 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_rtn_3s,			satNode, "! emp_rtn_3s",			"Empirical accleration 3 per rev sine term ");
	trySetKalmanFromYaml(satOpts.emp_rtn_4c,			satNode, "! emp_rtn_4c",			"Empirical accleration 4 per rev cosine term ");
	trySetKalmanFromYaml(satOpts.emp_rtn_4s,			satNode, "! emp_rtn_4s",			"Empirical accleration 4 per rev sine term ");

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

	trySetKalmanFromYaml(recOpts.clk,					recNode, "! clk",					"Clocks");
	trySetKalmanFromYaml(recOpts.clk_rate,				recNode, "! clk_rate",				"Clock rates");
	trySetKalmanFromYaml(recOpts.pos,					recNode, "! pos",					"Position");
	trySetKalmanFromYaml(recOpts.pos_rate,				recNode, "! pos_rate",				"Velocity");
	trySetKalmanFromYaml(recOpts.heading,				recNode, "@ heading",				"Heading");
	trySetKalmanFromYaml(recOpts.orbit,					recNode, "@ orbit",					"Orbital state");
	trySetKalmanFromYaml(recOpts.strain_rate,			recNode, "@ strain_rate",			"Velocity (large gain, for geodetic timescales)");
	trySetKalmanFromYaml(recOpts.amb,					recNode, "! amb",					"Integer phase ambiguities");
	trySetKalmanFromYaml(recOpts.pco,					recNode, "pco",						"Phase Center Offsets (experimental)");
	trySetKalmanFromYaml(recOpts.ant,					recNode, "ant",						"Antenna offsets (experimental)");
	trySetKalmanFromYaml(recOpts.code_bias,				recNode, "! code_bias",				"Code bias");
	trySetKalmanFromYaml(recOpts.phase_bias,			recNode, "! phase_bias",			"Phase bias");
	trySetKalmanFromYaml(recOpts.ion_stec,				recNode, "! ion_stec",				"Ionospheric slant delay");
	trySetKalmanFromYaml(recOpts.ion_model,				recNode, "! ion_model",				"Ionospheric mapping");
	trySetKalmanFromYaml(recOpts.slr_range_bias,		recNode, "@ slr_range_bias",		"Satellite Laser Ranging range bias");
	trySetKalmanFromYaml(recOpts.slr_time_bias,			recNode, "@ slr_time_bias",			"Satellite Laser Ranging time bias");
	trySetKalmanFromYaml(recOpts.trop,					recNode, "! trop",					"Troposphere corrections");
	trySetKalmanFromYaml(recOpts.trop_grads,			recNode, "! trop_grads",			"Troposphere gradients");
	trySetKalmanFromYaml(recOpts.trop_maps,				recNode, "! trop_maps",				"Troposphere ZWD mapping");
	
	trySetKalmanFromYaml(recOpts.emp_dyb_0,				recNode, "! emp_dyb_0",				"Empirical accleration bias ");
	trySetKalmanFromYaml(recOpts.emp_dyb_1c,			recNode, "! emp_dyb_1c",			"Empirical accleration 1 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_1s,			recNode, "! emp_dyb_1s",			"Empirical accleration 1 per rev sine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_2c,			recNode, "! emp_dyb_2c",			"Empirical accleration 2 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_2s,			recNode, "! emp_dyb_2s",			"Empirical accleration 2 per rev sine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_3c,			recNode, "! emp_dyb_3c",			"Empirical accleration 3 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_3s,			recNode, "! emp_dyb_3s",			"Empirical accleration 3 per rev sine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_4c,			recNode, "! emp_dyb_4c",			"Empirical accleration 4 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_4s,			recNode, "! emp_dyb_4s",			"Empirical accleration 4 per rev sine term ");
                                
	trySetKalmanFromYaml(recOpts.emp_dyb_0,				recNode, "! emp_dyb_0",				"Empirical accleration bias ");
	trySetKalmanFromYaml(recOpts.emp_dyb_1c,			recNode, "! emp_dyb_1c",			"Empirical accleration 1 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_1s,			recNode, "! emp_dyb_1s",			"Empirical accleration 1 per rev sine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_2c,			recNode, "! emp_dyb_2c",			"Empirical accleration 2 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_2s,			recNode, "! emp_dyb_2s",			"Empirical accleration 2 per rev sine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_3c,			recNode, "! emp_dyb_3c",			"Empirical accleration 3 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_3s,			recNode, "! emp_dyb_3s",			"Empirical accleration 3 per rev sine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_4c,			recNode, "! emp_dyb_4c",			"Empirical accleration 4 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_dyb_4s,			recNode, "! emp_dyb_4s",			"Empirical accleration 4 per rev sine term ");

	trySetKalmanFromYaml(recOpts.emp_rtn_0,				recNode, "! emp_rtn_0",				"Empirical accleration bias ");
	trySetKalmanFromYaml(recOpts.emp_rtn_1c,			recNode, "! emp_rtn_1c",			"Empirical accleration 1 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_rtn_1s,			recNode, "! emp_rtn_1s",			"Empirical accleration 1 per rev sine term ");
	trySetKalmanFromYaml(recOpts.emp_rtn_2c,			recNode, "! emp_rtn_2c",			"Empirical accleration 2 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_rtn_2s,			recNode, "! emp_rtn_2s",			"Empirical accleration 2 per rev sine term ");
	trySetKalmanFromYaml(recOpts.emp_rtn_3c,			recNode, "! emp_rtn_3c",			"Empirical accleration 3 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_rtn_3s,			recNode, "! emp_rtn_3s",			"Empirical accleration 3 per rev sine term ");
	trySetKalmanFromYaml(recOpts.emp_rtn_4c,			recNode, "! emp_rtn_4c",			"Empirical accleration 4 per rev cosine term ");
	trySetKalmanFromYaml(recOpts.emp_rtn_4s,			recNode, "! emp_rtn_4s",			"Empirical accleration 4 per rev sine term ");

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
		fullId += "." + suffix;
	}
	
	auto& satOpts = satOptsMap[fullId];

	//return early if possible
	if (satOpts._initialised)
		return satOpts;
	
	BOOST_LOG_TRIVIAL(debug) << "Getting config options for " << fullId;

	satOpts.id				= fullId;
	satOpts._initialised	= true;

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
		
		aliasOpts.id			= aliasName;
		aliasOpts._initialised	= true;
		
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
			
			stringsToYamlObject({yaml, ""}, {"! satellite_options"},	"Options to configure individual satellites, systems, or global configs");

			descriptorVec = {"! satellite_options", alias};
			for (int s = 0; s < S; s++)
			{
				descriptorVec.push_back(suffixes[s]);
			}			

			{	auto vec = descriptorVec; vec.insert(vec.end(), {"0!  exclude"							});	setInited(aliasOpts,	aliasOpts.exclude,							trySetFromYaml(aliasOpts.exclude, 							{yaml, ""}, vec, "(bool) Exclude satellite from processing"));						};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ pos",			"@ enable"			});	setInited(aliasOpts,	aliasOpts.sat_pos.enable,					trySetFromYaml(aliasOpts.sat_pos.enable,					{yaml, ""}, vec, "(bool) Enable modelling of position"));							};
            {	auto vec = descriptorVec; vec.insert(vec.end(), {"! pos",			"@ sources"			});	setInited(aliasOpts,	aliasOpts.sat_pos.ephemeris_sources,	 	trySetEnumVec( aliasOpts.sat_pos.ephemeris_sources,	 		{yaml, ""}, vec, "List of sources to use for position"));							}; 
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"! clock",			"@ enable"			});	setInited(aliasOpts,	aliasOpts.sat_clock.enable,					trySetFromYaml(aliasOpts.sat_clock.enable,					{yaml, ""}, vec, "(bool) Enable modelling of clocks"));								};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"! clock",			"! sources"			});	setInited(aliasOpts,	aliasOpts.sat_clock.ephemeris_sources, 		trySetEnumVec( aliasOpts.sat_clock.ephemeris_sources, 		{yaml, ""}, vec, "List of sources to use for clocks"));								};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"# clk",			"# enable"			});	setInited(aliasOpts,	aliasOpts.sat_clock.enable,					trySetFromYaml(aliasOpts.sat_clock.enable,					{yaml, ""}, vec, "(bool) Enable modelling of clocks (duplicate)"));					};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"# clk",			"# sources"			});	setInited(aliasOpts,	aliasOpts.sat_clock.ephemeris_sources, 		trySetEnumVec( aliasOpts.sat_clock.ephemeris_sources, 		{yaml, ""}, vec, "List of sources to use for clocks (duplicate)"));					};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ code_bias",		"@ enable"			});	setInited(aliasOpts,	aliasOpts.sat_code_bias.enable,				trySetFromYaml(aliasOpts.sat_code_bias.enable,				{yaml, ""}, vec, "(bool) Enable modelling of code biases"));						};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ phase_bias",	"@ enable"			});	setInited(aliasOpts,	aliasOpts.sat_phase_bias.enable,			trySetFromYaml(aliasOpts.sat_phase_bias.enable,				{yaml, ""}, vec, "(bool) Enable modelling of phase biases. Required for AR"));		};
            {	auto vec = descriptorVec; vec.insert(vec.end(), {"@ code_bias",		"@ default_bias"	});	setInited(aliasOpts,	aliasOpts.sat_code_bias.default_bias,		trySetFromYaml(aliasOpts.sat_code_bias.default_bias,		{yaml, ""}, vec, "(float) Bias to use when no code bias is found"));				};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ phase_bias",	"@ default_bias"	});	setInited(aliasOpts,	aliasOpts.sat_phase_bias.default_bias,		trySetFromYaml(aliasOpts.sat_phase_bias.default_bias,		{yaml, ""}, vec, "(float) Bias to use when no phase bias is found"));				};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ code_bias",		"@ undefined_sigma"	});	setInited(aliasOpts,	aliasOpts.sat_code_bias.undefined_sigma,	trySetFromYaml(aliasOpts.sat_code_bias.undefined_sigma,		{yaml, ""}, vec, "(float) Uncertainty sigma to apply to default code biases"));		};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ phase_bias",	"@ undefined_sigma"	});	setInited(aliasOpts,	aliasOpts.sat_phase_bias.undefined_sigma,	trySetFromYaml(aliasOpts.sat_phase_bias.undefined_sigma,	{yaml, ""}, vec, "(float) Uncertainty sigma to apply to default phase biases"));	};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ pco",			"@ enable"			});	setInited(aliasOpts,	aliasOpts.sat_pco,							trySetFromYaml(aliasOpts.sat_pco,							{yaml, ""}, vec, "(bool) Enable modelling of phase center offsets"));				};
            {	auto vec = descriptorVec; vec.insert(vec.end(), {"@ pcv",			"@ enable"			});	setInited(aliasOpts,	aliasOpts.sat_pcv,							trySetFromYaml(aliasOpts.sat_pcv,							{yaml, ""}, vec, "(bool) Enable modelling of phase center variations"));			};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ attitude",		"@ enable"			});	setInited(aliasOpts,	aliasOpts.sat_attitude.enable,				trySetFromYaml(aliasOpts.sat_attitude.enable,				{yaml, ""}, vec, "(bool) Enables non-nominal attitude types"));						};
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ attitude",		"@ sources"			});	setInited(aliasOpts,	aliasOpts.sat_attitude.sources, 			trySetEnumVec( aliasOpts.sat_attitude.sources, 				{yaml, ""}, vec, "List of sourecs to use for attitudes "));							};
			
			{	auto vec = descriptorVec; vec.push_back("@ antenna_boresight");		trySetFromYaml	(antenna_boresight,			{yaml, ""}, vec,	"[floats] Antenna boresight (Up) in satellite body-fixed frame");	}
			{	auto vec = descriptorVec; vec.push_back("@ antenna_azimuth");		trySetFromYaml	(antenna_azimuth,			{yaml, ""}, vec,	"[floats] Antenna azimuth (North) in satellite body-fixed frame");	}
			
			if (antenna_boresight	.size()	== 3)	{	aliasOpts.antenna_boresight		= Vector3d(antenna_boresight.data());		setInited(aliasOpts,	aliasOpts.antenna_boresight);	}	
			if (antenna_azimuth		.size()	== 3)	{	aliasOpts.antenna_azimuth		= Vector3d(antenna_azimuth	.data());		setInited(aliasOpts,	aliasOpts.antenna_azimuth);		}	
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
		fullId += "." + suffix;
	}
	
	auto& recOpts = recOptsMap[fullId];

	//return early if possible
	if (recOpts._initialised)
		return recOpts;
	
	BOOST_LOG_TRIVIAL(debug) << "Getting config options for " << fullId;
	
	recOpts.id				= fullId;
	recOpts._initialised	= true;

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
		
		aliasOpts.id			= aliasName;
		aliasOpts._initialised	= true;
		
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

			stringsToYamlObject({yaml, ""}, {"! station_options"},	"Options to configure individual stations or global configs");

			descriptorVec = {"! station_options", alias};
			for (int s = 0; s < S; s++)
			{
				descriptorVec.push_back(suffixes[s]);
			}

			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ kill"								});	setInited(aliasOpts,	aliasOpts.kill,								trySetFromYaml(aliasOpts.kill,							{yaml, ""}, vec, "(bool) Remove receiver from future processing"));					}                                                            
            {	auto vec = descriptorVec; vec.insert(vec.end(), {"@ exclude"							});	setInited(aliasOpts,	aliasOpts.exclude,							trySetFromYaml(aliasOpts.exclude,						{yaml, ""}, vec, "(bool) Exclude receiver from processing"));						}                                                            
            {	auto vec = descriptorVec; vec.insert(vec.end(), {"@ pos",			"@ enable"			});	setInited(aliasOpts,	aliasOpts.rec_pos.enable,					trySetFromYaml(aliasOpts.rec_pos.enable,				{yaml, ""}, vec, "(bool) Enable modelling of position"));							}                                                            
            {	auto vec = descriptorVec; vec.insert(vec.end(), {"@ clock",			"@ enable"			});	setInited(aliasOpts,	aliasOpts.rec_clock.enable,					trySetFromYaml(aliasOpts.rec_clock.enable,				{yaml, ""}, vec, "(bool) Enable modelling of clocks"));								}                                                            
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ clock",			"@ sources"			});	setInited(aliasOpts,	aliasOpts.rec_clock.ephemeris_sources, 		trySetEnumVec( aliasOpts.rec_clock.ephemeris_sources, 	{yaml, ""}, vec, "List of sources to use for clocks"));								}
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"# clk",			"# enable"			});	setInited(aliasOpts,	aliasOpts.rec_clock.enable,					trySetFromYaml(aliasOpts.rec_clock.enable,				{yaml, ""}, vec, "(bool) Enable modelling of clocks (duplicate)"));					}                                                            
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"# clk",			"# sources"			});	setInited(aliasOpts,	aliasOpts.rec_clock.ephemeris_sources, 		trySetEnumVec( aliasOpts.rec_clock.ephemeris_sources, 	{yaml, ""}, vec, "List of sources to use for clocks (duplicate)"));					}
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ code_bias",		"@ enable"			});	setInited(aliasOpts,	aliasOpts.rec_code_bias.enable,				trySetFromYaml(aliasOpts.rec_code_bias.enable,			{yaml, ""}, vec, "(bool) Enable modelling of code biases"));						}                                                            
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ phase_bias",	"@ enable"			});	setInited(aliasOpts,	aliasOpts.rec_phase_bias.enable,			trySetFromYaml(aliasOpts.rec_phase_bias.enable,			{yaml, ""}, vec, "(bool) Enable modelling of phase biases. Required for AR"));		}                                                            
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ code_bias",		"@ default_bias"	});	setInited(aliasOpts,	aliasOpts.rec_code_bias.default_bias,		trySetFromYaml(aliasOpts.rec_code_bias.default_bias,	{yaml, ""}, vec, "(float) Bias to use when no code bias is found"));				}                                                           
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ phase_bias",	"@ default_bias"	});	setInited(aliasOpts,	aliasOpts.rec_phase_bias.default_bias,		trySetFromYaml(aliasOpts.rec_phase_bias.default_bias,	{yaml, ""}, vec, "(float) Bias to use when no phase bias is found"));				}                                                           
            {	auto vec = descriptorVec; vec.insert(vec.end(), {"@ code_bias",		"@ undefined_sigma"	});	setInited(aliasOpts,	aliasOpts.rec_code_bias.undefined_sigma,	trySetFromYaml(aliasOpts.rec_code_bias.undefined_sigma,	{yaml, ""}, vec, "(float) Uncertainty sigma to apply to default code biases"));		}                                                           
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ phase_bias",	"@ undefined_sigma"	});	setInited(aliasOpts,	aliasOpts.rec_phase_bias.undefined_sigma,	trySetFromYaml(aliasOpts.rec_phase_bias.undefined_sigma,{yaml, ""}, vec, "(float) Uncertainty sigma to apply to default phase biases"));	}                                                           
            {	auto vec = descriptorVec; vec.insert(vec.end(), {"@ ant_delta",		"@ enable"			});	setInited(aliasOpts,	aliasOpts.rec_ant_delta,					trySetFromYaml(aliasOpts.rec_ant_delta,					{yaml, ""}, vec, "(bool) Enable modelling of antenna eccentricities"));				}                                                            
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ pco",			"@ enable"			});	setInited(aliasOpts,	aliasOpts.rec_pco,							trySetFromYaml(aliasOpts.rec_pco,						{yaml, ""}, vec, "(bool) Enable modelling of phase center offsets"));				}                                                            
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ pcv",			"@ enable"			});	setInited(aliasOpts,	aliasOpts.rec_pcv,							trySetFromYaml(aliasOpts.rec_pcv,						{yaml, ""}, vec, "(bool) Enable modelling of phase center variations"));			}                                                            
            {	auto vec = descriptorVec; vec.insert(vec.end(), {"@ attitude",		"@ enable"			});	setInited(aliasOpts,	aliasOpts.rec_attitude.enable,				trySetFromYaml(aliasOpts.rec_attitude.enable,			{yaml, ""}, vec, "(bool) Enables non-nominal attitude types"));						}                          
			{	auto vec = descriptorVec; vec.insert(vec.end(), {"@ attitude",		"@ sources"			});	setInited(aliasOpts,	aliasOpts.rec_attitude.sources, 			trySetEnumVec( aliasOpts.rec_attitude.sources, 			{yaml, ""}, vec, "List of sourecs to use for attitudes"));							}                                           
			
			
			{	auto vec = descriptorVec; vec.push_back("@ sat_id");				setInited(aliasOpts,	aliasOpts.sat_id,			trySetFromYaml	(aliasOpts.sat_id,			{yaml, ""}, vec,	"(string) Id for receivers that are also satellites"));						}
			{	auto vec = descriptorVec; vec.push_back("@ receiver_type");			setInited(aliasOpts,	aliasOpts.receiver_type,	trySetFromYaml	(aliasOpts.receiver_type,	{yaml, ""}, vec,	"(string) Type of gnss receiver hardware"));								}
			{	auto vec = descriptorVec; vec.push_back("@ antenna_type");			setInited(aliasOpts,	aliasOpts.antenna_type,		trySetFromYaml	(aliasOpts.antenna_type,	{yaml, ""}, vec,	"(string) Antenna type and radome in 20 character string as per sinex"));	}				
			{	auto vec = descriptorVec; vec.push_back("@ eccentricity");																trySetFromYaml	(eccentricity,				{yaml, ""}, vec,	"[floats] Antenna offset in ENU frame");									}
			{	auto vec = descriptorVec; vec.push_back("@ apriori_position");															trySetFromYaml	(apriori_pos,				{yaml, ""}, vec,	"[floats] Apriori position in XYZ ECEF frame");								}
			{	auto vec = descriptorVec; vec.push_back("@ antenna_boresight");															trySetFromYaml	(antenna_boresight,			{yaml, ""}, vec,	"[floats] Antenna boresight (Up) in receiver body-fixed frame");			}
			{	auto vec = descriptorVec; vec.push_back("@ antenna_azimuth");															trySetFromYaml	(antenna_azimuth,			{yaml, ""}, vec,	"[floats] Antenna azimuth (North) in receiver body-fixed frame");			}
			
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
                                                                                                                                                                                                                                                                          
				{	auto vec = descriptorVec;	vec.push_back("@ rnx_code_conversions");	trySetEnumOpt(rinex3Code, stringsToYamlObject(	{yaml, ""}, vec), {sysName, obsCode2._to_string()}, E_ObsCode::_from_string_nocase);}                                              
				{	auto vec = descriptorVec;	vec.push_back("@ rnx_phase_conversions");	trySetEnumOpt(rinex3Phas, stringsToYamlObject(	{yaml, ""}, vec), {sysName, obsCode2._to_string()}, E_ObsCode::_from_string_nocase);}
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
	ENUM			(&_from_string_nocase)(const char*),	///< Function to decode scale enum strings
	string			comment = "")							///< Description to use for documentation
{
	double	number			= output;
	ENUM	number_units	= ENUM::_from_integral(1);
	
	trySetFromYaml	(number,		node, number_parameter, comment);
	trySetEnumOpt	(number_units, 	node, scale_parameter,	_from_string_nocase);
	
	number *= (int)number_units;
	if (number != 0)
	{
		output = number;
	}
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
		
		bool altered = false;
		
		if (availableOptions.find(newAliasStack) == availableOptions.end())
		{
			//this yaml stack not found in the available options, check to see if it could have worked if it were an alias
			
			if	( (stack.find("estimation_parameters:stations:")						>= 0)
				||(stack.find("estimation_parameters:satellites:")						>= 0)
				||(stack.find("processing_options:minimum_constraints:station_noise:")	>= 0)
				||(stack.find("outputs:streams:")										>= 0)
				||(stack.find("station_options:")										>= 0)
				||(stack.find("satellite_options:")										>= 0))
			{
				int globalPos = aliasStack.find("global:");
				if (globalPos != aliasStack.size() - 7)
				{
					newAliasStack = aliasStack + "global" + ":";
				}
				else
				{
					newAliasStack = aliasStack.substr(0, globalPos + 7);
				}
				
				altered = true;

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

		if 		(node[key].IsMap())
		{
			recurseYaml(node[key], newStack, newAliasStack);
		}
		else if (node[key].IsNull())
		{
			//dont complain about things that dont do anything
		}
		else
		{
			//is a final value, this must pass on its own
			if (altered)
			{
				BOOST_LOG_TRIVIAL(warning)
				<< "Warning: " << newStack << " is not a valid yaml option";

				continue;
			}
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

	for (auto& filenameList : {filenames, includedFilenames})
	for (auto& filename : filenameList)
	if (filename != "")
	{
		boost::filesystem::path filePath(filename);
		auto currentConfigModifyTime = boost::filesystem::last_write_time(filePath);
		
		if (currentConfigModifyTime != configModifyTimeMap[filename])
		{
			modified = true;
		}
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
		
		auto inputs	= stringsToYamlObject({yaml, ""},	{"0! inputs"}, docs["inputs"]);

		trySetFromYaml(includes, inputs, {"! include_yamls"}, "[string] List of yaml files to include before this one");
		
		for (auto& include : includes)
		{
			yamlList.push_back(include);
		}
	}
	
	for (auto& filename : filenames)
	{
		yamlList.push_back(filename);
	}
	
	includedFilenames = yamlList;
	
	yamls.resize(yamlList.size());
	
	defaultConfigs();
	
	for (int i = 0; i < yamlList.size(); i++)
	{
		auto& filename	= yamlList	[i];
		auto& yaml		= yamls		[i];

		BOOST_LOG_TRIVIAL(info)
		<< "Loading configuration from file " << filename;
		
		try
		{
			boost::filesystem::path filePath(filename);
			auto currentConfigModifyTime = boost::filesystem::last_write_time(filePath);
		
			configModifyTimeMap[filename] = currentConfigModifyTime;
			
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
		catch (const boost::filesystem::filesystem_error &e)
		{
			if (commandOpts.count("yaml-defaults"))
			{
				//we expect to break, continue parsing
			}
			else
			{
				BOOST_LOG_TRIVIAL(error) << "Error: \nFailed to parse configuration file " << filename;
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

			trySetFromYaml(outputs_root,			outputs, {"0! root_directory"			}, "(string) Directory that outputs will be placed in");

			{
				auto metadata = stringsToYamlObject(outputs, 		{"! metadata"}, "Options for setting metadata for inputs and outputs");

				trySetFromAny (config_description,	commandOpts,	metadata, {"! config_description"				}, "(string) ID for this config, used to replace <CONFIG> tags in other options");
				trySetFromAny (stream_user,			commandOpts,	metadata, {"! user"								}, "(string) Username for connecting to NTRIP casters");
				trySetFromAny (stream_pass,			commandOpts,	metadata, {"! pass"								}, "(string) Password for connecting to NTRIP casters");
				trySetFromYaml(analysis_agency,						metadata, {"@ analysis_agency"					}, "(string) Agency for output files headers");
				trySetFromYaml(analysis_center,						metadata, {"@ analysis_center"					}, "(string) Analysis center for output files headers");
				trySetFromYaml(ac_contact,							metadata, {"@ ac_contact"						}, "(string) Contact person for output files headers");
				trySetFromYaml(analysis_program,					metadata, {"@ analysis_program"					}, "(string) Program for output files headers");
				trySetFromYaml(analysis_program_version,			metadata, {"@ analysis_program_version"			}, "(string) Version for output files headers");
				trySetFromYaml(rinex_comment,						metadata, {"@ rinex_comment"					}, "(string) Comment for output files headers");
				trySetFromYaml(reference_system,					metadata, {"@ reference_system"					}, "(string) Terrestrial Reference System Code");
				trySetFromYaml(time_system,							metadata, {"@ time_system"						}, "(string) Time system - e.g. \"G\", \"UTC\"");
				trySetFromYaml(ocean_tide_loading_model,			metadata, {"@ ocean_tide_loading_model"			}, "(string) Ocean tide loading model applied");
				trySetFromYaml(atmospheric_tide_loading_model,		metadata, {"@ atmospheric_tide_loading_model"	}, "(string) Atmospheric tide loading model applied");
				trySetFromYaml(geoid_model,							metadata, {"@ geoid_model"						}, "(string) Geoid model name for undulation values");
				trySetFromYaml(gradient_mapping_function,			metadata, {"@ gradient_mapping_function"		}, "(string) Name of mapping function used for mapping horizontal troposphere gradients");
			}

			{
				trySetFromYaml(colorize_terminal,			outputs, {"@ colorize_terminal"			}, "(bool) Use ascii command codes to highlight warnings and errors");
			}
			
			{
				auto trace = stringsToYamlObject(outputs, {"0! trace"}, docs["trace"]);

																					trySetFromYaml(output_station_trace,		trace, {"0! output_stations"		}, "(bool) Output trace files for individual stations processing");
																					trySetFromYaml(output_network_trace,		trace, {"0! output_network"			}, "(bool) Output trace files for complete network of stations, inclucing kalman filter results and statistics");
																					trySetFromYaml(output_satellite_trace,		trace, {"0! output_satellites"		}, "(bool) Output trace files for individual satellites processing");
				conditionalPrefix("<OUTPUTS_ROOT>",		trace_directory,			trySetFromYaml(trace_directory,				trace, {"! directory"				}, "(string) Directory to output trace files to"));
				conditionalPrefix("<TRACE_DIRECTORY>",	satellite_trace_filename,	trySetFromYaml(satellite_trace_filename,	trace, {"1! satellite_filename"		}, "(string) Template filename for satellite trace files"));
				conditionalPrefix("<TRACE_DIRECTORY>",	station_trace_filename,		trySetFromYaml(station_trace_filename,		trace, {"1! station_filename"		}, "(string) Template filename for station trace files"));
				conditionalPrefix("<TRACE_DIRECTORY>",	network_trace_filename,		trySetFromYaml(network_trace_filename,		trace, {"1! network_filename"		}, "(string) Template filename for network trace files"));
																					trySetFromAny(trace_level, commandOpts,		trace, {"! level"					}, "(int) Threshold level for printing messages (0-6). Increasing this increases the amount of data stored in all trace files");

																					trySetFromYaml(output_residual_chain,		trace, {"! output_residual_chain"	}, "(bool) Output component-wise details for measurement residuals");
																					trySetFromYaml(output_residuals,			trace, {"! output_residuals"		}, "(bool) Output measurements and residuals");
																					trySetFromYaml(output_config,				trace, {"! output_config"			}, "(bool) Output configuration files to top of trace files");
																					trySetFromYaml(output_json_trace,			trace, {"@ output_json"				}, "(bool) Output json formatted trace files");
			}

			{
				auto output_rotation = stringsToYamlObject(outputs, {"@ output_rotation"}, "Trace files can be rotated periodically by epoch interval. These options specify the period that applies to the <LOGTIME> template variables in filenames");

				trySetScaledFromYaml(rotate_period,		output_rotation, {"@ period"	},	{"@ period_units"	},	E_Period::_from_string_nocase, "Period that times will be rounded by to generate template variables in filenames");
			}

			{
				auto bias_sinex = stringsToYamlObject(outputs, {"! bias_sinex"}, "Rinex formatted bias sinex files");

																					trySetFromYaml(output_bias_sinex,				bias_sinex, {"0! output"				}, "(bool) Output bias sinex files");
				conditionalPrefix("<OUTPUTS_ROOT>",			bias_sinex_directory,	trySetFromYaml(bias_sinex_directory,			bias_sinex, {"@ directory"				}, "(string) Directory to output bias sinex files to"));
				conditionalPrefix("<BIAS_SINEX_DIRECTORY>",	bias_sinex_filename,	trySetFromYaml(bias_sinex_filename,				bias_sinex, {"@ filename"				}, "(string) Template filename for bias sinex files"));
																					trySetFromYaml(bias_time_system,				bias_sinex, {"@ bias_time_system"		}, "(string) Time system for bias SINEX \"G\", \"C\", \"R\", \"UTC\", \"TAI\"");
																					trySetFromYaml(ambrOpts.code_output_interval,	bias_sinex, {"@ code_output_interval"	}, "(double) Update interval for code  biases");
																					trySetFromYaml(ambrOpts.phase_output_interval,	bias_sinex, {"@ phase_output_interval"	}, "(double) Update interval for phase biases");
																					trySetFromYaml(ambrOpts.output_rec_bias,		bias_sinex, {"@ output_rec_bias"		}, "(bool) output receiver biases");
			}

			{
				auto clocks = stringsToYamlObject(outputs, {"! clocks"}, "Rinex formatted clock files");

																			trySetFromYaml(output_clocks,				clocks, {"0! output"				}, "(bool) Output clock files");
				conditionalPrefix("<OUTPUTS_ROOT>",		clocks_directory,	trySetFromYaml(clocks_directory,			clocks, {"@ directory"				}, "(string) Directory to output clock files to"));
				conditionalPrefix("<CLOCKS_DIRECTORY>",	clocks_filename,	trySetFromYaml(clocks_filename,				clocks, {"@ filename"				}, "(string) Template filename for clock files"));
																			trySetEnumVec (clocks_receiver_sources,		clocks, {"@ receiver_sources"		});
																			trySetEnumVec (clocks_satellite_sources,	clocks, {"@ satellite_sources"		});
			}

			{
				auto decoded_rtcm = stringsToYamlObject(outputs, {"@ decoded_rtcm"}, "RTCM messages that are received may be recorded to human-readable json files");

																								trySetFromYaml(output_decoded_rtcm_json,	decoded_rtcm, {"0@ output"		},	"(bool) Enable exporting decoded RTCM data to file");
				conditionalPrefix("<OUTPUTS_ROOT>",				decoded_rtcm_json_directory,	trySetFromYaml(decoded_rtcm_json_directory,	decoded_rtcm, {"@ directory"	},	"(string) Directory to export decoded RTCM data"));
				conditionalPrefix("<DECODED_RTCM_DIRECTORY>",	decoded_rtcm_json_filename,		trySetFromYaml(decoded_rtcm_json_filename,	decoded_rtcm, {"@ filename"		},	"(string) Decoded RTCM data filename"));
			}

			{
				auto encoded_rtcm = stringsToYamlObject(outputs, {"@ encoded_rtcm"}, "RTCM messages that are encoded and transmitted may be recorded to human-readable json files");

																								trySetFromYaml(output_encoded_rtcm_json,	encoded_rtcm, {"0@ output"		},	"(bool) Enable exporting encoded RTCM data to file");
				conditionalPrefix("<OUTPUTS_ROOT>",				encoded_rtcm_json_directory,	trySetFromYaml(encoded_rtcm_json_directory,	encoded_rtcm, {"@ directory"	},	"(string) Directory to export encoded RTCM data"));
				conditionalPrefix("<ENCODED_RTCM_DIRECTORY>",	encoded_rtcm_json_filename,		trySetFromYaml(encoded_rtcm_json_filename,	encoded_rtcm, {"@ filename"		},	"(string) Encoded RTCM data filename"));
			}

			{
				auto erp = stringsToYamlObject(outputs, {"! erp"}, "Earth rotation parameters can be output to file");

																		trySetFromYaml(output_erp,				erp, {"0! output"		}, "(bool) Enable exporting of erp data");
				conditionalPrefix("<OUTPUTS_ROOT>",		erp_directory,	trySetFromYaml(erp_directory,			erp, {"@ directory"		}, "(string) Directory to export erp data files"));
				conditionalPrefix("<ERP_DIRECTORY>",	erp_filename,	trySetFromYaml(erp_filename,			erp, {"@ filename"		}, "(string) ERP data output filename"));
			}

			{
				auto ionex = stringsToYamlObject(outputs, {"! ionex"}, "IONEX formatted ionospheric mapping and modelling outputs");

																			trySetFromYaml(output_ionex,			ionex, {"0! output"						}, "(bool) Enable exporting ionospheric model data");
				conditionalPrefix("<OUTPUTS_ROOT>",		ionex_directory,	trySetFromYaml(ionex_directory,			ionex, {"@ directory"					}, "(string) Directory to export ionex data"));
				conditionalPrefix("<IONEX_DIRECTORY>",	ionex_filename,		trySetFromYaml(ionex_filename,			ionex, {"@ filename"					}, "(string) Ionex data filename"));
																			trySetFromYaml(ionexGrid.lat_center,	ionex, {"@ grid", "@ lat_center"		}, "(float) Center lattitude for models");
																			trySetFromYaml(ionexGrid.lon_center,	ionex, {"@ grid", "@ lon_center"		}, "(float) Center longitude for models");
																			trySetFromYaml(ionexGrid.lat_width,		ionex, {"@ grid", "@ lat_width"			}, "(float) Total lattitudinal width of model");
																			trySetFromYaml(ionexGrid.lon_width,		ionex, {"@ grid", "@ lon_width"			}, "(float) Total longitudinal width of model");
																			trySetFromYaml(ionexGrid.lat_res,		ionex, {"@ grid", "@ lat_resolution"	}, "(float) Interval between lattitude outputs");
																			trySetFromYaml(ionexGrid.lon_res,		ionex, {"@ grid", "@ lon_resolution"	}, "(float) Interval between longitude outputs");
																			trySetFromYaml(ionexGrid.time_res,		ionex, {"@ grid", "@ time_resolution"	}, "(float) Interval between output epochs");
			}

			{
				auto ionstec = stringsToYamlObject(outputs, {"! ionstec"});

																				trySetFromYaml(output_ionstec,			ionstec, {"0! output"		}, "(bool) ");
				conditionalPrefix("<OUTPUTS_ROOT>",			ionstec_directory,	trySetFromYaml(ionstec_directory,		ionstec, {"@ directory"		}));
				conditionalPrefix("<IONSTEC_DIRECTORY>",	ionstec_filename,	trySetFromYaml(ionstec_filename,		ionstec, {"@ filename"		}));
			}

			{
				auto sinex = stringsToYamlObject(outputs, {"! sinex"});

																			trySetFromYaml(output_sinex,			sinex, {"0! output"			}, "(bool) ");
				conditionalPrefix("<OUTPUTS_ROOT>",		sinex_directory,	trySetFromYaml(sinex_directory,			sinex, {"@ directory"		}));
				conditionalPrefix("<SINEX_DIRECTORY>",	sinex_filename,		trySetFromYaml(sinex_filename,			sinex, {"@ filename"		}));
			}

			{
				auto log = stringsToYamlObject(outputs, {"! log"}, "Log files store console output in files");

																			trySetFromYaml(output_log,				log, {"0! output"			}, "(bool) Enable console output logging");
				conditionalPrefix("<OUTPUTS_ROOT>",		log_directory,		trySetFromYaml(log_directory,			log, {"@ directory"			}, "(string) Log output directory"));
				conditionalPrefix("<LOG_DIRECTORY>",	log_filename,		trySetFromYaml(log_filename,			log, {"@ filename"			}, "(string) Log output filename"));
			}

			{
				auto gpx = stringsToYamlObject(outputs, {"! gpx"}, "GPX files contain point data that may be easily viewed in GIS mapping software");

																			trySetFromYaml(output_gpx,		     	gpx, {"0! output"			}, "(bool) ");
				conditionalPrefix("<OUTPUTS_ROOT>",		gpx_directory,		trySetFromYaml(gpx_directory,			gpx, {"@ directory"			}));
				conditionalPrefix("<GPX_DIRECTORY>",	gpx_filename,		trySetFromYaml(gpx_filename,			gpx, {"@ filename"			}));
			}

			{
				auto ntrip_log = stringsToYamlObject(outputs, {"@ ntrip_log"});

																					trySetFromYaml(output_ntrip_log,		ntrip_log, {"0@ output"		}, "(bool) ");
				conditionalPrefix("<OUTPUTS_ROOT>",			ntrip_log_directory,	trySetFromYaml(ntrip_log_directory,		ntrip_log, {"@ directory"	}));
				conditionalPrefix("<NTRIP_LOG_DIRECTORY>",	ntrip_log_filename,		trySetFromYaml(ntrip_log_filename,		ntrip_log, {"@ filename"	}));
			}

			{
				auto network_statistics = stringsToYamlObject(outputs, {"@ network_statistics"});

																										trySetFromYaml(output_network_statistics_json,		network_statistics, {"0@ output"	},	"(bool) Enable exporting network statistics data to file");
				conditionalPrefix("<OUTPUTS_ROOT>",					network_statistics_json_directory,	trySetFromYaml(network_statistics_json_directory,	network_statistics, {"@ directory"	},	"(string) Directory to export network statistics data"));
				conditionalPrefix("<NETWORK_STATISTICS_DIRECTORY>",	network_statistics_json_filename,	trySetFromYaml(network_statistics_json_filename,	network_statistics, {"@ filename"	},	"(string) Network statistics data filename"));
			}

			{
				auto sp3 = stringsToYamlObject(outputs, {"! sp3"}, "SP3 files contain orbital and clock data of satellites and receivers");

																				trySetFromYaml(output_sp3,					sp3, {"0! output"					}, "(bool) Enable SP3 file outputs");
																				trySetFromYaml(output_inertial_orbits, 		sp3, {"@ output_inertial"			}, "(bool) Output the entries using inertial positions and velocities");
																				trySetFromYaml(output_predicted_orbits,		sp3, {"0@ output_predicted_orbits"	}, "(bool) Enable prediction and outputting of orbits past the end of processing");
																				trySetFromYaml(output_sp3_velocities,		sp3, {"@ output_velocities"			}, "(bool) Output velocity data to sp3 file");
				conditionalPrefix("<OUTPUTS_ROOT>",		sp3_directory,			trySetFromYaml(sp3_directory,				sp3, {"@ directory"					}, "(string) Directory to store SP3 outputs"));
				conditionalPrefix("<SP3_DIRECTORY>",	sp3_filename,			trySetFromYaml(sp3_filename,				sp3, {"@ filename"					}, "(string) SP3 output filename"));
				conditionalPrefix("<SP3_DIRECTORY>",	predicted_sp3_filename,	trySetFromYaml(predicted_sp3_filename,		sp3, {"@ predicted_filename"		}, "(string) Filename for predicted SP3 outputs"));
																				trySetEnumVec (sp3_clock_sources,			sp3, {"@ clock_sources"				}, "List of sources for clock data for SP3 outputs");
																				trySetEnumVec (sp3_orbit_sources,			sp3, {"@ orbit_sources"				}, "List of sources for orbit data for SP3 outputs");
																				trySetFromYaml(sp3_output_interval,			sp3, {"@ output_interval"			}, "(int) Update interval for sp3 records");
			}
			
			{
				auto orbit_ics = stringsToYamlObject(outputs, {"@ orbit_ics"}, "Orbital parameters can be output in a yaml that Ginan can later use as an initial condition for futher processing.");
				
																					trySetFromYaml(output_orbit_ics,	orbit_ics, {"@ output"		}, "(bool) Output orbital initial condition file");
				conditionalPrefix("<OUTPUTS_ROOT>",			orbit_ics_directory,	trySetFromYaml(orbit_ics_directory,	orbit_ics, {"@ directory"	}, "(string) Output orbital initial condition directory"));
				conditionalPrefix("<ORBIT_ICS_DIRECTORY>",	orbit_ics_filename,		trySetFromYaml(orbit_ics_filename,	orbit_ics, {"@ filename"	}, "(string) Output orbital initial condition filename"));
			}

			{
				auto orbex = stringsToYamlObject(outputs, {"@ orbex"});

																			trySetFromYaml(output_orbex,			orbex, {"0@ output"				}, "(bool) Output orbex file");
				conditionalPrefix("<OUTPUTS_ROOT>",		orbex_directory,	trySetFromYaml(orbex_directory,			orbex, {"@ directory"			}, "(string) Output orbex directory"));
				conditionalPrefix("<ORBEX_DIRECTORY>",	orbex_filename,		trySetFromYaml(orbex_filename,			orbex, {"@ filename"			}, "(string) Output orbex filename"));
																			trySetEnumVec (orbex_orbit_sources,		orbex, {"@ orbit_sources" 		}, "Sources for orbex orbits");
																			trySetEnumVec (orbex_clock_sources,		orbex, {"@ clock_sources" 		}, "Sources for orbex clocks");
																			trySetEnumVec (orbex_attitude_sources,	orbex, {"@ attitude_sources" 	}, "Sources for orbex attitudes");

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
				conditionalPrefix("<OUTPUTS_ROOT>",			ppp_sol_directory,	trySetFromYaml(ppp_sol_directory,		ppp_sol, {"@ directory"	}));
				conditionalPrefix("<PPP_SOL_DIRECTORY>",	ppp_sol_filename,	trySetFromYaml(ppp_sol_filename,		ppp_sol, {"@ filename"	}));
			}

			{
				auto cost = stringsToYamlObject(outputs, {"! cost"}, docs["cost"]);

																		trySetFromYaml(output_cost,			cost, {"0! output"			},	"(bool) Enable data exporting to troposphere COST file");
																		trySetEnumVec (cost_data_sources,	cost, {"@ sources" 			},	"Source for troposphere delay data - KALMAN, etc.");
				conditionalPrefix("<OUTPUTS_ROOT>",		cost_directory,	trySetFromYaml(cost_directory,		cost, {"@ directory"		},	"(string) Directory to export troposphere COST file"));
				conditionalPrefix("<COST_DIRECTORY>",	cost_filename,	trySetFromYaml(cost_filename,		cost, {"@ filename"			},	"(string) Troposphere COST filename"));
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
				conditionalPrefix("<OUTPUTS_ROOT>",			rinex_nav_directory,	trySetFromYaml(rinex_nav_directory,		rinex_nav, {"@ directory"	}));
				conditionalPrefix("<RINEX_NAV_DIRECTORY>",	rinex_nav_filename,		trySetFromYaml(rinex_nav_filename,		rinex_nav, {"@ filename"	}));
																					trySetFromYaml(rinex_nav_version,		rinex_nav, {"@ version"		});
			}

			{
				auto rinex_obs = stringsToYamlObject(outputs, {"@ rinex_obs"});

																					trySetFromYaml(output_rinex_obs,		rinex_obs, {"0@ output"					}, "(bool) ");
				conditionalPrefix("<OUTPUTS_ROOT>",			rinex_obs_directory,	trySetFromYaml(rinex_obs_directory,		rinex_obs, {"@ directory"				}));
				conditionalPrefix("<RINEX_OBS_DIRECTORY>",	rinex_obs_filename,		trySetFromYaml(rinex_obs_filename,		rinex_obs, {"@ filename"				}));
																					trySetFromYaml(rinex_obs_print_C_code,	rinex_obs, {"@ output_pseudorange"		}, "(bool) ");
																					trySetFromYaml(rinex_obs_print_L_code,	rinex_obs, {"@ output_phase_range"		}, "(bool) ");
																					trySetFromYaml(rinex_obs_print_D_code,	rinex_obs, {"@ output_doppler"			}, "(bool) ");
																					trySetFromYaml(rinex_obs_print_S_code,	rinex_obs, {"@ output_signal_to_noise"	}, "(bool) ");
																					trySetFromYaml(rinex_obs_version,		rinex_obs, {"@ version"					});
			}

			{
				auto rtcm_nav = stringsToYamlObject(outputs, {"@ rtcm_nav"});

																				trySetFromYaml(record_rtcm_nav,			rtcm_nav, {"0@ output"			}, "(bool) ");
				conditionalPrefix("<OUTPUTS_ROOT>",			rtcm_nav_directory,	trySetFromYaml(rtcm_nav_directory,		rtcm_nav, {"@ directory"		}));
				conditionalPrefix("<RTCM_NAV_DIRECTORY>",	rtcm_nav_filename,	trySetFromYaml(rtcm_nav_filename,		rtcm_nav, {"@ filename"			}));
			}

			{
				auto rtcm_obs = stringsToYamlObject(outputs, {"@ rtcm_obs"});

																				trySetFromYaml(record_rtcm_obs,			rtcm_obs, {"0@ output"			}, "(bool) ");
				conditionalPrefix("<OUTPUTS_ROOT>",			rtcm_obs_directory,	trySetFromYaml(rtcm_obs_directory,		rtcm_obs, {"@ directory"		}));
				conditionalPrefix("<RTCM_OBS_DIRECTORY>",	rtcm_obs_filename,	trySetFromYaml(rtcm_obs_filename,		rtcm_obs, {"@ filename"			}));
			}

			{
				auto raw_ubx = stringsToYamlObject(outputs, {"raw_ubx"});

																			trySetFromYaml(record_raw_ubx,			raw_ubx, {"0 output"			}, "(bool) ");
				conditionalPrefix("<OUTPUTS_ROOT>",		raw_ubx_directory,	trySetFromYaml(raw_ubx_directory,		raw_ubx, {"directory"			}));
				conditionalPrefix("<UBX_DIRECTORY>",	raw_ubx_filename,	trySetFromYaml(raw_ubx_filename,		raw_ubx, {"filename"			}));
			}

			{
				auto slr_obs = stringsToYamlObject(outputs, {"! slr_obs"}, docs["slr_obs"]);

																				trySetFromYaml(output_slr_obs,			slr_obs, {"0! output"			}, 	"(bool) Enable data exporting to tabular SLR obs file");
				conditionalPrefix("<OUTPUTS_ROOT>",			slr_obs_directory,	trySetFromYaml(slr_obs_directory,		slr_obs, {"@ directory"			}, 	"(string) Directory to export tabular SLR obs file"));
				conditionalPrefix("<SLR_OBS_DIRECTORY>",	slr_obs_filename,	trySetFromYaml(slr_obs_filename,		slr_obs, {"@ filename"			},	"(string) Tabular SLR obs filename"));
			}

			{
				auto trop_sinex = stringsToYamlObject(outputs, {"! trop_sinex"}, docs["trop_sinex"]);

																					trySetFromYaml(output_trop_sinex,		trop_sinex, {"0! output"		},	"(bool) Enable data exporting to troposphere SINEX file");
																					trySetEnumVec (trop_sinex_data_sources,	trop_sinex, {"@ sources"		},	"Source for troposphere delay data - KALMAN, etc.");
				conditionalPrefix("<OUTPUTS_ROOT>",			trop_sinex_directory,	trySetFromYaml(trop_sinex_directory,	trop_sinex, {"@ directory"		},	"(string) Directory to export troposphere SINEX file"));
				conditionalPrefix("<TROP_SINEX_DIRECTORY>",	trop_sinex_filename,	trySetFromYaml(trop_sinex_filename,		trop_sinex, {"@ filename"		},	"(string) Troposphere SINEX filename"));
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

			trySetFromAny(inputs_root,					commandOpts,	inputs, {"0! inputs_root"			}, "(string) Root path to be added to all other input files (unless they are absolute)");
			trySetFromAny(inputs_root,					commandOpts,	inputs, {"0! root_directory"		}, "(string) Root path to be added to all other input files (unless they are absolute)");
			
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! atx_files"				}, "[string] List of atx files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	atx_files	.insert(atx_files	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! snx_files"				}, "[string] List of snx files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	snx_files	.insert(snx_files	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! blq_files"				}, "[string] List of blq files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	blq_files	.insert(blq_files	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! erp_files"				}, "[string] List of erp files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	erp_files	.insert(erp_files	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! ion_files"				}, "[string] List of ion files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	ion_files	.insert(ion_files	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! igrf_files"				}, "[string] List of igrf files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	igrf_files	.insert(igrf_files	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! egm_files"				}, "[string] List of egm files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	egm_files	.insert(egm_files	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! jpl_files"				}, "[string] List of jpl files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	jpl_files	.insert(jpl_files	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! tide_files"				}, "[string] List of tide files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	tide_files	.insert(tide_files	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! cmc_files"				}, "[string] List of cmc files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	cmc_files	.insert(cmc_files	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny(vec,	commandOpts,	inputs, {"! hfeop_files"			}, "[string] List of hfeop files to use");		conditionalPrefix("<INPUTS_ROOT>", vec);	hfeop_files	.insert(hfeop_files	.end(), vec.begin(), vec.end()); }
			
			auto troposphere	= stringsToYamlObject(inputs,	{"0! troposphere"}, "Files specifying tropospheric model inputs");
			
			{ vector<string> vec;	trySetFromYaml(vec,					troposphere, {"! vmf_files"				}, "[string] List of vmf files to use");	conditionalPrefix("<INPUTS_ROOT>", vec);	vmf_files	.insert(vmf_files	.end(), vec.begin(), vec.end()); }
			
			conditionalPrefix("<INPUTS_ROOT>", 	model.trop.orography,	trySetFromYaml(model.trop.orography,troposphere, {"! orography_files"		}));
			conditionalPrefix("<INPUTS_ROOT>", 	model.trop.gpt2grid,	trySetFromYaml(model.trop.gpt2grid,	troposphere, {"! gpt2grid_files"		}));
			
			auto ionosphere		= stringsToYamlObject(inputs,	{"0! ionosphere"}, "Files specifying ionospheric model inputs");
		    { vector<string> vec;	trySetFromYaml(vec,					ionosphere, {"@ atm_reg_definitions"	}, "[string] List of files to define regions for compact SSR");	conditionalPrefix("<INPUTS_ROOT>", vec);	atm_reg_definitions	.insert(atm_reg_definitions	.end(), vec.begin(), vec.end()); }
			{ vector<string> vec;	trySetFromAny( vec,	commandOpts,	ionosphere, {"! ion_files"				}, "[string] List of IONEX files for VTEC input");             	conditionalPrefix("<INPUTS_ROOT>", vec);	ion_files			.insert(ion_files			.end(), vec.begin(), vec.end()); }

			{
				auto gnss_data = stringsToYamlObject(inputs, {"1! gnss_observations"}, "Signal observation data from gnss receivers to be used as measurements");

				conditionalPrefix("<INPUTS_ROOT>", gnss_obs_root,				trySetFromAny(gnss_obs_root,	commandOpts,	gnss_data,	{"0! inputs_root"	}, "(string) Root path to be added to all other gnss data inputs (unless they are absolute)"));

				tryGetMappedList(rnx_inputs,			commandOpts, gnss_data,	{"! rnx_inputs"			}, "<GNSS_OBS_ROOT>", "[string] List of rinex      inputs to use");
				tryGetMappedList(ubx_inputs,			commandOpts, gnss_data,	{"# ubx_inputs"			}, "<GNSS_OBS_ROOT>", "[string] List of ubxfiles   inputs to use");
				tryGetMappedList(obs_rtcm_inputs,		commandOpts, gnss_data,	{"! rtcm_inputs"		}, "<GNSS_OBS_ROOT>", "[string] List of rtcmfiles  inputs to use for observations");
			}

			{
				auto pseudo_observation_data = stringsToYamlObject(inputs, {"! pseudo_observations"}, "Use data from pre-processed data products as observations. Useful for combining and comparing datasets");

				conditionalPrefix("<INPUTS_ROOT>", pseudo_obs_root,		trySetFromYaml(pseudo_obs_root,	pseudo_observation_data,	{"0! inputs_root"		}, "(string) Root path to be added to all other pseudo obs data files (unless they are absolute)"));

				tryGetMappedList(pseudo_sp3_inputs,	commandOpts, pseudo_observation_data,	{"! sp3_inputs"			}, "<PSEUDO_OBS_ROOT>", "[string] List of sp3 inputs to use for pseudoobservations");
				tryGetMappedList(pseudo_snx_inputs,	commandOpts, pseudo_observation_data,	{"@ snx_inputs"			}, "<PSEUDO_OBS_ROOT>", "[string] List of snx inputs to use for pseudoobservations");
			}

			{
				auto satellite_data = stringsToYamlObject(inputs, {"0! satellite_data"});

				conditionalPrefix("<INPUTS_ROOT>", gnss_obs_root,				trySetFromYaml(sat_data_root,	satellite_data,	{"0! inputs_root"		}, "(string) Root path to be added to all other satellite data files (unless they are absolute)"));

				conditionalPrefix("<SAT_DATA_ROOT>", nav_files,			trySetFromAny(nav_files,				commandOpts,	satellite_data, {"! nav_files"			}, "[string] List of ephemeris  files to use"));			
				conditionalPrefix("<SAT_DATA_ROOT>", sp3_files,			trySetFromAny(sp3_files,				commandOpts,	satellite_data, {"! sp3_files"			}, "[string] List of sp3        files to use"));			
				conditionalPrefix("<SAT_DATA_ROOT>", dcb_files,			trySetFromAny(dcb_files,				commandOpts,	satellite_data, {"! dcb_files"			}, "[string] List of dcb        files to use"));			
				conditionalPrefix("<SAT_DATA_ROOT>", bsx_files,			trySetFromAny(bsx_files,				commandOpts,	satellite_data, {"! bsx_files"			}, "[string] List of biassinex  files to use"));			
				conditionalPrefix("<SAT_DATA_ROOT>", clk_files,			trySetFromAny(clk_files,				commandOpts,	satellite_data, {"! clk_files"			}, "[string] List of clock      files to use"));			
				conditionalPrefix("<SAT_DATA_ROOT>", sid_files,			trySetFromAny(sid_files,				commandOpts, 	satellite_data, {"@ sid_files"			}, "[string] List of sat ID     files to use - from https://cddis.nasa.gov/sp3c_satlist.html/"));
				conditionalPrefix("<SAT_DATA_ROOT>", com_files,			trySetFromAny(com_files,				commandOpts, 	satellite_data, {"@ com_files"			}, "[string] List of com        files to use - retroreflector offsets from centre-of-mass for spherical sats"));
				conditionalPrefix("<SAT_DATA_ROOT>", crd_files,			trySetFromAny(crd_files,				commandOpts, 	satellite_data, {"@ crd_files"			}, "[string] List of crd        files to use - SLR observation data"));
				conditionalPrefix("<SAT_DATA_ROOT>", obx_files,			trySetFromAny(obx_files,				commandOpts,	satellite_data, {"! obx_files"			}, "[string] List of orbex      files to use"));			
				conditionalPrefix("<SAT_DATA_ROOT>", nav_rtcm_inputs,	trySetFromAny(nav_rtcm_inputs,			commandOpts,	satellite_data,	{"! rtcm_inputs"		}, "[string] List of rtcm       inputs to use for corrections"));
			}
		}

		auto processing_options = stringsToYamlObject({ yaml, "" }, {processing_options_str}, "Various sections and parameters to specify how the observations are processed");
		{
			auto general = stringsToYamlObject(processing_options, {"2! gnss_general"}, "Options to specify the processing of gnss observations");
			
			bool found = trySetFromAny(elevation_mask,	commandOpts,	general, {"0!  elevation_mask"		}, "(float) Minimum elevation for satellites to be processed. Config in degrees, however default is displayed in radians");
			if (found)
				elevation_mask *= D2R;
			
			trySetFromYaml	(require_apriori_positions,					general, {"@ require_apriori_positions" }, "(bool) Restrict processing to stations that have apriori positions available");
			trySetFromYaml	(require_antenna_details,					general, {"@ require_antenna_details" 	}, "(bool) Restrict processing to stations that have antenna details");
			trySetFromYaml	(pivot_station,								general, {"@ pivot_station" 			}, "(string) Largely deprecated id of station to use for pivot constraints");
			trySetFromYaml	(interpolate_rec_pco,						general, {"@ interpolate_rec_pco" 		}, "(bool) Interpolate other known pco values to find pco for unknown frequencies");
			trySetFromYaml	(auto_fill_pco,								general, {"@ auto_fill_pco" 			}, "(bool) Use similar PCOs when requested values are not found");
			trySetFromYaml	(max_gdop,									general, {"@ max_gdop"					}, "(float) Maximum dilution of precision before error is flagged");
			trySetFromYaml	(raim,										general, {"@ raim"						}, "(bool) Enable Receiver Autonomous Integrity Monitoring. When SPP fails further SPP solutions are calculated with subsets of observations with the aim of eliminating a problem satellite");
			trySetEnumOpt	(recOptsMap[""].error_model,				general, {"@ error_model"				}, E_NoiseModel::_from_string_nocase);
			trySetFromYaml	(pppOpts.common_atmosphere,					general, {"@ common_atmosphere"			}, "(bool) ");
			trySetFromYaml	(pppOpts.use_rtk_combo,						general, {"@ use_rtk_combo"				}, "(bool) Combine applicable observations to simulate an rtk solution");
			trySetFromYaml	(delete_old_ephemerides,					general, {"@ delete_old_ephemerides"	}, "(bool) Remove old ephemerides that have accumulated over time from before far before the currently processing epoch");
			trySetFromYaml	(use_tgd_bias,								general, {"@ use_tgd_bias"				}, "(bool) Use TGD/BGD bias from ephemeris, DO NOT turn on unless using Klobuchar/NeQuick Ionospheres");
			trySetFromYaml	(common_sat_pco,							general, {"@ common_sat_pco"			}, "(bool) Use L1 satellite PCO values for all signals");
			trySetFromYaml	(common_rec_pco,							general, {"@ common_rec_pco"			}, "(bool) Use L1 receiver PCO values for all signals");
			trySetFromYaml	(leap_seconds,								general, {"@ gpst_utc_leap_seconds"		}, "(int) Difference between gps time and utc in leap seconds");

			trySetFromYaml	(process_meas[CODE],						general, {"1@ code_measurements",		"process"	}, "(bool) Process code measurements");
			trySetFromYaml	(recOptsMap[""].code_sigmas,				general, {"1@ code_measurements",		"sigmas"	}, "[floats] Sigmas for code observations");
			
			trySetFromYaml	(process_meas[PHAS],						general, {"1@ phase_measurements",		"process"	}, "(bool) Process phase measurements");
			trySetFromYaml	(recOptsMap[""].phas_sigmas,				general, {"1@ phase_measurements",		"sigmas"	}, "[floats] Sigmas for phase observations");		//todo aaron needed?

			trySetEnumOpt	(receiver_reference_clk,					general, {"@ rec_reference_system"		}, E_Sys::_from_string_nocase, "(String) Receiver will use this system as reference clock");
			trySetFromYaml	(fixed_phase_bias_var,						general, {"@ fixed_phase_bias_var"		}, "(double) variance of phase bias to be considered fixed/binded");
			trySetFromYaml	(sat_clk_definition,						general, {"@ sat_clk_definition"		}, "(bool) use satellite clock definition pseudorange ");
			

			for (int i = E_Sys::GPS; i < E_Sys::SUPPORTED; i++)
			{
				E_Sys	sys			= E_Sys::_values()[i];
				string	sysName		= "! " + boost::algorithm::to_lower_copy((string) sys._to_string());	

				auto sys_options = stringsToYamlObject(general, {"1! sys_options", sysName}, (string)"Options for the " + sys._to_string() + " constellation");

				trySetFromYaml(process_sys				[sys],		sys_options, {"0! process"				}, "(bool) Process this constellation");
				trySetFromYaml(solve_amb_for			[sys],		sys_options, {"! ambiguity_resolution"	}, "(bool) Solve carrier phase ambiguities for this constellation");
				trySetFromYaml(reject_eclipse			[sys],		sys_options, {"@ reject_eclipse"		}, "(bool) Exclude satellites that are in eclipsing region");
				trySetFromYaml(zero_satellite_dcb		[sys],		sys_options, {"@ zero_satellite_dcb"	}, "(bool) Constrain: satellite DCB for this system to zero");
				trySetFromYaml(zero_receiver_dcb		[sys],		sys_options, {"@ zero_receiver_dcb"		}, "(bool) Constrain: receiver DCB for this system to zero");
				trySetFromYaml(one_phase_bias			[sys],		sys_options, {"@ one_phase_bias"		}, "(bool) Constrain: assume satellite phase biases are common among frequencies");
				trySetFromYaml(receiver_amb_pivot		[sys],		sys_options, {"@ receiver_amb_pivot"	}, "(bool) Constrain: set of ambiguities, to eliminate receiver rank deficiencies");
				trySetFromYaml(network_amb_pivot		[sys],		sys_options, {"@ network_amb_pivot"		}, "(bool) Constrain: set of ambiguities, to eliminate network  rank deficiencies");
				trySetFromYaml(use_for_iono_model		[sys],		sys_options, {"@ use_for_iono_model"	}, "(bool) Use this constellation as part of Ionospheric model");
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
				found = trySetFromYaml(codePriorityStrings,			sys_options, {"! code_priorities"			}, "List of observation codes to use in processing");
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
				found = trySetFromYaml(zeroAveragePhase,			sys_options, {"@ zero_phase_average"		});
				if (found)
				for (auto once : {1})
				{
					zero_phase_average[sys].clear();

					for (auto& codeString : zeroAveragePhase)
					{
						try
						{
							auto a = E_ObsCode::_from_string(codeString.c_str());
							zero_phase_average[sys].push_back(a);
						}
						catch (...)
						{
							continue;
						}
					}
				}
			}

			{
				auto epoch_control = stringsToYamlObject(processing_options, {"0! epoch_control"}, "Specifies the rate and duration of data processing");

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

				if (wait_next_epoch < epoch_interval)
					wait_next_epoch = epoch_interval + 0.01;
				trySetFromYaml(sleep_milliseconds,					epoch_control, {std::to_string(i++) + "# sleep_milliseconds"	}, "(float) Time to sleep before checking for new data - lower numbers are associated with high idle cpu usage");
				trySetFromYaml(wait_next_epoch,						epoch_control, {std::to_string(i++) + "@ wait_next_epoch"		}, "(float) Time to wait for next epochs data before skipping the epoch (will default to epoch_interval as an appropriate minimum value for realtime)");
				trySetFromYaml(wait_all_stations,					epoch_control, {std::to_string(i++) + "@ wait_all_stations"		}, "(float) Time to wait from the reception of the first data of an epoch before skipping stations with data still unreceived");
				trySetFromYaml(require_obs,							epoch_control, {std::to_string(i++) + "@ require_obs"			}, "(bool) Exit the program if no observation sources are available");
				trySetFromYaml(assign_closest_epoch,				epoch_control, {std::to_string(i++) + "@ assign_closest_epoch"	}, "(bool) Assign observations to the closest epoch - don't skip observations that fall between epochs");
				trySetFromAny(simulate_real_time,	commandOpts,	epoch_control, {std::to_string(i++) + "@ simulate_real_time"	}, "(bool)  For RTCM playback - delay processing to match original data rate");
			}

			{
				auto gnss_modelling = stringsToYamlObject(processing_options, {"2! gnss_models"});

				trySetFromYaml(model.range,								gnss_modelling, {"@ range",				"enable"	}, "(bool) Enable modelling of signal time of flight time due to range");
				
				trySetFromYaml(model.tides.enable,						gnss_modelling, {"@ tides",				"@ enable"	}, "(bool) Enable modelling of tidal disaplacements");
				trySetFromYaml(model.tides.solid,						gnss_modelling, {"@ tides",				"@ solid"	}, "(bool) Enable solid earth tides");
				trySetFromYaml(model.tides.otl,							gnss_modelling, {"@ tides",				"@ otl"		}, "(bool) Enable ocean tide loading");
				trySetFromYaml(model.tides.pole,						gnss_modelling, {"@ tides",				"@ pole"	}, "(bool) Enable pole tides");

				trySetFromYaml(model.relativity,						gnss_modelling, {"@ relativity",		"@ enable"	}, "(bool) Enable modelling of relativistic effects");
				trySetFromYaml(model.relativity2,						gnss_modelling, {"@ relativity2",		"@ enable"	}, "(bool) Enable modelling of secondary relativistic effects");
				trySetFromYaml(model.sagnac,							gnss_modelling, {"@ sagnac",			"@ enable"	}, "(bool) Enable modelling of sagnac effect");

				{
					auto ionospheric_component = stringsToYamlObject(gnss_modelling, {"! ionospheric_component"}, "Ionospheric models produce frequency-dependent effects");

					trySetFromYaml(model.ionospheric_component,				ionospheric_component, {"0@ enable"	}, "(bool) Enable ionospheric modelling");

					trySetEnumOpt( ionoOpts.corr_mode, 						ionospheric_component, {"@ corr_mode" 						}, E_IonoMode::_from_string_nocase);
					trySetEnumOpt( ionoOpts.mapping_function,				ionospheric_component, {"@ mapping_function" 				}, E_IonoMapFn::_from_string_nocase, "(E_IonoMapFn) mapping function if not specified in the data or model");
					trySetFromYaml(ionoOpts.pierce_point_layer_height,		ionospheric_component, {"@ pierce_point_layer_height"		}, "(float) ionospheric pierce point layer height if not specified in the data or model (km)");
					trySetFromYaml(ionoOpts.mapping_function_layer_height,	ionospheric_component, {"@ mapping_function_layer_height"	}, "(float) mapping function layer height if not specified in the data or model (km)");
					trySetFromYaml(ionoOpts.iono_sigma_limit,				ionospheric_component, {"@ iono_sigma_limit"				}, "(float) Ionosphere states are removed when their sigma exceeds this value");
					trySetFromYaml(ionoOpts.common_ionosphere,				ionospheric_component, {"! common_ionosphere"				}, "(bool) Use the same ionosphere state for code and phase observations");
					trySetFromYaml(ionoOpts.use_if_combo,					ionospheric_component, {"! use_if_combo"					}, "(bool) Combine 'uncombined' measurements to simulate an ionosphere-free solution");
					trySetFromYaml(ionoOpts.use_gf_combo,					ionospheric_component, {"! use_gf_combo"					}, "(bool) Combine 'uncombined' measurements to simulate a geometry-free solution");
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
					auto ionospheric_model = stringsToYamlObject(gnss_modelling, {"@ ionospheric_model"}, "Coherent ionosphere models can improve estimation of biases and allow use with single frequency receivers");

					trySetFromYaml(model.ionospheric_model,			ionospheric_model, {"0@ enable"				}, "(bool) Compute ionosphere maps from a network of stations");
					trySetEnumOpt( ionModelOpts.model, 				ionospheric_model, {"@ model" 				}, E_IonoModel::_from_string_nocase);
					trySetFromYaml(ionModelOpts.function_order,		ionospheric_model, {"@ function_order"		}, "Maximum order  of Spherical harmonics for Ionospheric mapping");
					trySetFromYaml(ionModelOpts.function_degree,	ionospheric_model, {"@ function_degree"		}, "Maximum degree of Spherical harmonics for Ionospheric mapping");
					trySetFromYaml(ionModelOpts.estimate_sat_dcb,	ionospheric_model, {"@ estimate_sat_dcb"	}, "(bool) Estimate satellite dcb alongside Ionosphere models, should be false for local STEC");
					trySetFromYaml(ionModelOpts.use_rotation_mtx,	ionospheric_model, {"@ use_rotation_mtx"	}, "(bool) Use 3D rotation matrix for spherical harmonics to maintain orientation toward the sun");
					trySetFromYaml(ionModelOpts.basis_sigma_limit,	ionospheric_model, {"@ model_sigma_limit"	}, "(float) Ionosphere states are removed when their sigma exceeds this value");
					
					bool found = trySetFromYaml(ionModelOpts.layer_heights,		ionospheric_model, {"@ layer_heights"			}, "[floats] List of heights of ionosphere layers to estimate");
					if (found)
					for (auto& a : ionModelOpts.layer_heights)
					{
						a *= 1000; //km to m
					}
				}

				auto troposhpere = stringsToYamlObject(gnss_modelling, {"@ troposphere"}, "Tropospheric modelling accounts for delays due to refraction of light in water vapour");
				{
					trySetFromYaml(model.trop.enable,		troposhpere,	{"0@ enable" 		}, "(bool) Model tropospheric delays");
					trySetEnumOpt( model.trop.model, 		troposhpere,	{"@ model" 			}, E_TropModel::_from_string_nocase);
				}

				auto orbits = stringsToYamlObject(gnss_modelling, {"@ orbits"}, "Legacy orbital adjustment estimation code, largely deprecated");
				{
					trySetFromYaml(model.orbits,			orbits,			{"0@ enable" 		});
				}

				auto eop = stringsToYamlObject(gnss_modelling, {"@ eop"}, "Earth orientation parameters");
				{
					trySetFromYaml(model.eop,				eop,			{"0@ enable" 		});
				}

				trySetFromYaml(model.phase_windup,						gnss_modelling, {"@ phase_windup",		"enable"	}, "(bool) Model phase windup due to relative rotation of circularly polarised antennas");
				trySetFromYaml(model.heading,							gnss_modelling, {"@ heading",			"enable"	}, "(bool) Estimate heading using unmodelled phase windup residuals");
				trySetFromYaml(model.integer_ambiguity,					gnss_modelling, {"@ integer_ambiguity",	"enable"	}, "(bool) Model ambiguities due to unknown integer number of cycles in phase measurements");
			}

			{
				auto model_error_checking = stringsToYamlObject(processing_options, {"3! model_error_checking"}, "The kalman filter is capable of automatic statistical integrity modelling");

				{
					auto deweighting = stringsToYamlObject(model_error_checking, {"! deweighting"}, "Measurements that are outside the expected confidence bounds may be deweighted so that outliers do not contaminate the filtered solution");

					trySetFromYaml(deweight_factor,				deweighting,	{"! deweight_factor"		}, "(float) Factor to downweight the variance of measurements with statistically detected errors");
					trySetFromYaml(reject_on_state_error,		deweighting,	{"@ reject_on_state_error"	}, "(bool) Any \"state\" errors cause deweighting of all measurements that reference the state");
				}

				{
					auto orbit_errors = stringsToYamlObject(model_error_checking, {"@ orbit_errors"}, "Orbital states that are not consistent with measurements may be reinitialised to allow for dynamic maneuvers");

					trySetFromYaml(enable_orbit_proc_noise_impulses,	orbit_errors,	{"@ enable"							}, "(bool) Enable applying process noise impulses to orbits upon state errors");
					trySetFromYaml(orbit_pos_proc_noise,				orbit_errors,	{"@ orbit_pos_proc_noise"			}, "(float) Sigma to apply to orbital position states as reinitialisation");
					trySetFromYaml(orbit_vel_proc_noise,				orbit_errors,	{"@ orbit_vel_proc_noise"			}, "(float) Sigma to apply to orbital velocity states as reinitialisation");
					trySetFromYaml(orbit_vel_proc_noise_trail,			orbit_errors,	{"@ orbit_vel_proc_noise_trail"		}, "(float) Initial sigma for exponentially decaying noise to apply for subsequent epochs as soft reinitialisation");
					trySetFromYaml(orbit_vel_proc_noise_trail_tau,		orbit_errors,	{"@ orbit_vel_proc_noise_trail_tau"	}, "(float) Time constant for exponentially decauing noise");
				}

				{
					auto ambiguities = stringsToYamlObject(model_error_checking, {"! ambiguities"}, "Cycle slips in ambiguities are primary cause of incorrect gnss modelling and may be reinitialised");

					trySetFromYaml(reinit_on_all_slips,			ambiguities,	{"! reinit_on_all_slips"	}, "(bool) Any detected slips cause removal and reinitialisation of ambiguities");
					trySetFromYaml(pppOpts.outage_reset_limit,	ambiguities,	{"! outage_reset_limit"		}, "(int) Maximum number of epochs with missed phase measurements before the ambiguity associated with the measurement is reset.");
					trySetFromYaml(pppOpts.phase_reject_limit,	ambiguities,	{"! phase_reject_limit"		}, "(int) Maximum number of phase measurements to reject before the ambiguity associated with the measurement is reset.");
				}

				{
					auto clocks = stringsToYamlObject(model_error_checking, {"@ clocks"}, "Error responses specific to clock states");

					trySetFromYaml(reinit_on_clock_error,		clocks,			{"@ reinit_on_clock_error"	}, "(bool) Any clock \"state\" errors cause removal and reinitialisation of the clocks and all associated ambiguities");
				}

				{
					auto cycle_slips = stringsToYamlObject(model_error_checking, {"@ cycle_slips"}, "Cycle slips may be detected by the preprocessor and measurements rejected or ambiguities reinitialised");

					trySetFromYaml(thres_slip,			cycle_slips, {"@ slip_threshold"			}, "(float) Value used to determine when a slip has occurred");
					trySetFromYaml(mw_proc_noise,		cycle_slips, {"@ mw_proc_noise"				}, "(float) Process noise applied to filtered Melbourne-Wubenna measurements to detect cycle slips");

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
				auto process_modes = stringsToYamlObject(processing_options, {"1 process_modes"}, "Aspects of the processing flow may be enabled and disabled according to desired type of solutions");
			
				trySetFromYaml(process_ionosphere,			process_modes, {"@ ionosphere"		}, "(bool) Compute Ionosphere models based on GNSS measurements");
				trySetFromYaml(process_preprocessor,		process_modes, {"! preprocessor"	}, "(bool) Preprocessing and quality checks");
				trySetFromYaml(process_spp,					process_modes, {"@ spp"				}, "(bool) Perform SPP on station data");
				trySetFromYaml(process_ppp,					process_modes, {"! ppp"				}, "(bool) Perform PPP network or end user mode");
				trySetFromYaml(slrOpts.process_slr,			process_modes, {"! slr"				}, "(bool) Process SLR observations");
			}

			{
				auto minimum_constraints = stringsToYamlObject(processing_options, {"! minimum_constraints"}, "Station coodinates may be aligned to reference frames with minimal external constraints");

				trySetFromYaml(process_minimum_constraints,		minimum_constraints,	{"! enable"					}, "(bool) Transform states by minimal constraints to selected station coordinates");

				trySetKalmanFromYaml(minCOpts.scale,			minimum_constraints,	"! scale",						"Estimation and application of scaling factor");
				trySetKalmanFromYaml(minCOpts.rotation,			minimum_constraints,	"! rotation",					"Estimation and application of angular offsets");
				trySetKalmanFromYaml(minCOpts.translation,		minimum_constraints,	"! translation",				"Estimation and application of CoG offsets");

				trySetFromYaml(minCOpts.once_per_epoch,			minimum_constraints,	{"@ once_per_epoch"			},	"(bool) Perform minimum constraints on a temporary filter and output results once per epoch");
				trySetFromYaml(minCOpts.full_vcv,				minimum_constraints,	{"@ full_vcv"				},	"(bool) ! experimental ! Use full VCV for measurement noise in minimum constraints filter");
				trySetFromYaml(minCOpts.scale_by_vcv,			minimum_constraints,	{"@ scale_by_vcv"			},	"(bool) Use variance of positions as additional scaling factor in minimum constraints weighting");

				trySetEnumOpt( minCOpts.inverter, 				minimum_constraints,	{"@ inverter" 				}, E_Inverter::_from_string_nocase);
				trySetFromYaml(minCOpts.max_filter_iter,		minimum_constraints,	{"@ max_filter_iterations"	}, "(int) Number of times to run filter to test postfit adjustments against state and measurement confidence intervals. Should be set to limit the number of outliers to detect before allowing outlying measurements to affect the filter");
				trySetFromYaml(minCOpts.max_prefit_remv,		minimum_constraints,	{"@ max_prefit_removals"	}, 															"(int) Maximum number of measurements to exclude using prefit checks before attempting to filter");
				trySetFromYaml(minCOpts.sigma_check,			minimum_constraints,	{"@ outlier_screening", "@ sigma_check"			},										"(bool)  Enable prefit and postfit sigma check");
				trySetFromYaml(minCOpts.w_test,					minimum_constraints,	{"@ outlier_screening", "@ w_test"				},										"(bool)  Enable w-test");
				trySetFromYaml(minCOpts.chi_square_test,		minimum_constraints,	{"@ outlier_screening", "@ chi_square_test"		},										"(bool)  Enable Chi-square test");
				trySetEnumOpt( minCOpts.chi_square_mode,		minimum_constraints,	{"@ outlier_screening", "@ chi_square_mode"		}, E_ChiSqMode::_from_string_nocase,	"(enum)  Chi-square test mode - innovation, measurement, state");
				trySetFromYaml(minCOpts.sigma_threshold,		minimum_constraints,	{"@ outlier_screening", "@ sigma_threshold"		},										"(float) sigma threshold");
			}
			
			auto preprocessor_options = stringsToYamlObject(processing_options, {"! preprocessor_options"}, "Configurations for the kalman filter and its sub processes");
			{
				trySetFromYaml(preprocess_all_data,			preprocessor_options,	{"preprocess_all_data"							});
			}
			
			auto filter_options = stringsToYamlObject(processing_options, {"! filter_options"}, "Configurations for the kalman filter and its sub processes");
			{
				trySetFromYaml(joseph_stabilisation,			filter_options,	{"joseph_stabilisation"							});
				trySetEnumOpt( pppOpts.inverter, 				filter_options,	{"inverter" 									}, E_Inverter::_from_string_nocase, "Inverter to be used within the Kalman filter update stage, which may provide different performance outcomes in terms of processing time and accuracy and stability.");
			
				trySetFromYaml(pppOpts.simulate_filter_only,	filter_options,	{"simulate_filter_only"							}, "(bool) Residuals will be calculated, but no adjustments to state or covariances will be applied");
				
				trySetFromYaml(pppOpts.chunk_size,				filter_options,	{"@ chunking", "size"									});
				trySetFromYaml(pppOpts.station_chunking,		filter_options,	{"@ station_chunking",		"@ enable"					}, "(bool) Split large filter and measurement matrices blockwise by station ID to improve processing speed");
				trySetFromYaml(pppOpts.satellite_chunking,		filter_options,	{"@ satellite_chunking",	"@ enable"					}, "(bool) Split large filter and measurement matrices blockwise by satellite ID to improve processing speed");
				
				auto outlier_screening = stringsToYamlObject(filter_options, {"! outlier_screening"}, "Statistical checks allow for detection of outliers that exceed their confidence intervals.");
				trySetFromYaml(pppOpts.max_filter_iter,			outlier_screening,	{"! max_filter_iterations"	});
				trySetFromYaml(pppOpts.sigma_check,				outlier_screening,	{"@ sigma_check"			},										"(bool)  Enable prefit and postfit sigma check");
				trySetFromYaml(pppOpts.w_test,					outlier_screening,	{"@ w_test"					},										"(bool)  Enable w-test");
				trySetFromYaml(pppOpts.chi_square_test,			outlier_screening,	{"@ chi_square_test"		},										"(bool)  Enable Chi-square test");
				trySetEnumOpt( pppOpts.chi_square_mode,			outlier_screening,	{"@ chi_square_mode"		}, E_ChiSqMode::_from_string_nocase,	"(enum)  Chi-square test mode - innovation, measurement, state");
				trySetFromYaml(pppOpts.sigma_threshold,			outlier_screening,	{"@ sigma_threshold"		},										"(float) sigma threshold");
				trySetFromYaml(pppOpts.max_prefit_remv,			outlier_screening,	{"@ max_prefit_removals"	}, 										"(int) Maximum number of measurements to exclude using prefit checks before attempting to filter");
			
				auto rts = stringsToYamlObject(filter_options, {"! rts"}, "RTS allows reverse smoothing of estimates such that early estimates can make use of later data.");
				trySetFromYaml(process_rts,						rts,				{"0!  enable"				}, "(bool) Perform backward smoothing of states to improve precision of earlier states");
				trySetFromYaml(pppOpts.rts_lag,					rts,				{"1 lag"					}, "(int) Number of epochs to use in RTS smoothing. Negative numbers indicate full reverse smoothing.");
				trySetFromYaml(pppOpts.rts_directory,			rts,				{"directory"				}, "(string) Directory for rts intermediate files");
				trySetFromYaml(pppOpts.rts_filename,			rts,				{"filename"					}, "(string) Base filename for rts intermediate files");
				trySetFromYaml(pppOpts.rts_smoothed_suffix,		rts,				{"suffix"					}, "(string) Suffix to be applied to smoothed versions of files");
				trySetEnumOpt( pppOpts.rts_inverter, 			rts,				{"inverter" 				}, E_Inverter::_from_string_nocase, "Inverter to be used within the rts processor, which may provide different performance outcomes in terms of processing time and accuracy and stability.");
				trySetFromYaml(pppOpts.output_intermediate_rts,	rts,				{"output_intermediates"		}, "(bool) Output best available smoothed states when performing fixed-lag rts (slow, use only when needed)");
			}

			auto spp_options = stringsToYamlObject(processing_options, {"! spp_options"}, "Configurations for the kalman filter and its sub processes");
			{
				trySetFromYaml(sppOpts.max_lsq_iterations,		spp_options,		{"! max_lsq_iterations"		},	"(int) Maximum number of iterations of least squares allowed for convergence");
				trySetFromYaml(sppOpts.always_reinitialize,		spp_options,		{"! always_reinitialize"	},	"(bool) Reset SPP state to zero to avoid potential for lock-in of bad states");
				
				auto outlier_screening = stringsToYamlObject(filter_options, {"! outlier_screening"}, "Statistical checks allow for detection of outliers that exceed their confidence intervals.");
				trySetFromYaml(sppOpts.sigma_check,				outlier_screening,	{"@ sigma_check"			},										"(bool)  Enable sigma check on residuals after convergence");
				trySetFromYaml(sppOpts.sigma_threshold,			outlier_screening,	{"@ sigma_threshold"		},										"(float) sigma threshold");
				trySetFromYaml(sppOpts.max_removals,			outlier_screening,	{"@ max_removals"			}, 										"(int) Maximum number of measurements to exclude");
			}
		}


		auto orbit_propagation = stringsToYamlObject({ yaml, "" }, {processing_options_str, "@ orbit_propagation"});
		{
			trySetFromYaml(orbitPropagation.central_force,				orbit_propagation, {"@ central_force"				}, "(bool) Acceleration due to the central force");
			trySetFromYaml(orbitPropagation.planetary_perturbation,		orbit_propagation, {"@ planetary_perturbation"		}, "(bool) Acceleration due to third celestial bodies");
			trySetFromYaml(orbitPropagation.indirect_J2,				orbit_propagation, {"@ indirect_J2"					}, "(bool) J2 acceleration perturbation due to the Sun and Moon");
			trySetFromYaml(orbitPropagation.egm_field,					orbit_propagation, {"@ egm_field"					}, "(bool) Acceleration due to the high degree model of the Earth gravity model (exclude degree 0, made by central_force)");
			trySetFromYaml(orbitPropagation.solid_earth_tide,			orbit_propagation, {"@ solid_earth_tide"			}, "(bool) Model accelerations due to solid earth tides");
			trySetFromYaml(orbitPropagation.ocean_tide,					orbit_propagation, {"@ ocean_tide"					}, "(bool) Model accelerations due to ocean tides model");
			trySetFromYaml(orbitPropagation.general_relativity,			orbit_propagation, {"@ general_relativity"			}, "(bool) Model acceleration due general relativisty");
			trySetFromYaml(orbitPropagation.pole_tide_ocean,			orbit_propagation, {"@ pole_tide_ocean"				}, "(bool) Model accelerations due to ocean pole tide (degree 2 only)");
			trySetFromYaml(orbitPropagation.pole_tide_solid,			orbit_propagation, {"@ pole_tide_solid"				}, "(bool) Model accelerations due to solid pole tide (degree 2 only)");
			trySetFromYaml(orbitPropagation.solar_radiation_pressure, 	orbit_propagation, {"@ solar_radiation_pressure"	}, "(bool) Model accelerations due to solar radiation pressure (Cannonball model)");
			trySetFromYaml(orbitPropagation.empirical,					orbit_propagation, {"@ empirical"					}, "(bool) Model accelerations due to empirical accelerations");
			trySetFromYaml(orbitPropagation.antenna_thrust,				orbit_propagation, {"@ antenna_thrust"				}, "(bool) Model accelerations due to the emitted signal from the antenna");
			trySetFromYaml(orbitPropagation.albedo, 					orbit_propagation, {"@ albedo"						}, "(bool) Model accelerations due to the albedo effect from Earth (Visible and Infra-red)");

			trySetFromYaml(orbitPropagation.sat_mass,					orbit_propagation, {"@ sat_mass"					}, "(float) Satellite mass for use if not specified in the SINEX metadata file");
			trySetFromYaml(orbitPropagation.sat_area, 					orbit_propagation, {"@ sat_area"					}, "(float) Satellite area for use in solar radiation and albedo calculations");
			trySetFromYaml(orbitPropagation.sat_power, 					orbit_propagation, {"@ sat_power"					}, "(float) Transmission power use if not specified in the SINEX metadata file");
			trySetFromYaml(orbitPropagation.srp_cr,						orbit_propagation, {"@ srp_cr"						}, "(float) Coefficient of reflection of the satellite");
			trySetFromYaml(orbitPropagation.degree_max,					orbit_propagation, {"@ degree_max"					}, "(int) Maximum degree of spherical harmonics model");
			trySetFromYaml(orbitPropagation.itrf_pseudoobs,				orbit_propagation, {"@ itrf_pseudoobs"				}, "(bool) Pseudo observations are provided in ITRF frame rather than standard ECEF SP3 files");
			trySetFromYaml(orbitPropagation.integrator_time_step,		orbit_propagation, {"@ integrator_time_step"		}, "(float) Timestep for the integrator, must be smaller than the processing time step, might be adjusted if the processing time step isn't a integer number of time steps");

			trySetFromYaml(orbitPropagation.empirical_dyb_eclipse,		orbit_propagation, {"@ empirical_dyb_eclipse"		}, "[bool] turn on/off the eclipse on each axis (D, Y, B)");
			trySetFromYaml(orbitPropagation.empirical_rtn_eclipse,		orbit_propagation, {"@ empirical_rtn_eclipse"		}, "[bool] turn on/off the eclipse on each axis (R, T, N)");
	
		}
					
									
// 		trySetFromYaml(split_sys,				outputs, { "split_sys"		});

		{
			auto ambres_options = stringsToYamlObject(processing_options, {"@ ambiguity_resolution"});

			trySetFromYaml(ambrOpts.min_el_AR,			ambres_options, {"@ elevation_mask"				}, "Minimum satellite elevation to perform ambiguity resolution");
			trySetFromYaml(ambrOpts.lambda_set,			ambres_options, {"@ lambda_set_size"			}, "Maximum numer of candidate sets to be used in lambda_alt2 and lambda_bie modes");
			trySetFromYaml(ambrOpts.AR_max_itr,			ambres_options, {"@ max_rounding_iterations"	}, "Maximum number of rounding iterations performed in iter_rnd and bootst modes");

			trySetEnumOpt( ambrOpts.mode,				ambres_options,	{"@ mode" 						}, E_ARmode::_from_string_nocase);
			trySetFromYaml(ambrOpts.succsThres,			ambres_options, {"@ success_rate_threshold"		}, "Thresold for integer validation, success rate test.");
			trySetFromYaml(ambrOpts.ratioThres,			ambres_options, {"@ solution_ratio_threshold"	}, "Thresold for integer validation, distance ratio test.");
			
			trySetFromYaml(ambrOpts.once_per_epoch,		ambres_options,	{"@ once_per_epoch"				},	"(bool) Perform ambiguity resolution on a temporary filter and output results once per epoch");
			trySetFromYaml(ambrOpts.fix_and_hold,		ambres_options,	{"@ fix_and_hold"				},	"(bool) Perform ambiguity resolution and commit results to the main processing filter");
		}

		{
			auto ssr_corrections = stringsToYamlObject(processing_options, {"! ssr_corrections"}, docs["ssr_corrections"]);

			trySetEnumVec (ssrOpts.ephemeris_sources, 		ssr_corrections, {"! ephemeris_sources" 		}, "Sources for SSR ephemeris");
			trySetEnumVec (ssrOpts.clock_sources, 			ssr_corrections, {"! clock_sources" 			}, "Sources for SSR clocks");
			trySetEnumVec (ssrOpts.code_bias_sources, 		ssr_corrections, {"! code_bias_sources" 		}, "Sources for SSR code biases");
			trySetEnumVec (ssrOpts.phase_bias_sources, 		ssr_corrections, {"! phase_bias_sources" 		}, "Sources for SSR phase biases");
			trySetEnumVec (ssrOpts.ionosphere_sources, 		ssr_corrections, {"! ionosphere_sources" 		}, "Sources for SSR ionosphere");
			// trySetEnumVec (ssrOpts.troposphere_sources, 		ssr_corrections, {"@ troposphere_sources" 		});
			trySetEnumOpt (ssrOpts.output_timing, 			ssr_corrections, {"@ output_timing" 			}, E_SSROutTiming::_from_string_nocase);
			trySetFromYaml(ssrOpts.prediction_interval,		ssr_corrections, {"@ prediction_interval"		});
			trySetFromYaml(ssrOpts.prediction_duration,		ssr_corrections, {"@ prediction_duration"		});
			trySetFromYaml(ssrOpts.extrapolate_corrections,	ssr_corrections, {"@ extrapolate_corrections"	}, "(bool) ");
		
			{
				auto atmosphere = stringsToYamlObject(ssr_corrections, {"! atmpospheric"}, docs["atmpospheric"]);

				trySetFromYaml (ssrOpts.region_id, 			atmosphere, {"@ region_id" 				}, "Region ID for atmospheric corrections");
				trySetFromYaml (ssrOpts.npoly_trop, 		atmosphere, {"@ npoly_trop" 			}, "Number of polynomial coefficient for SSR trop corrections");
				trySetFromYaml (ssrOpts.npoly_iono, 		atmosphere, {"@ npoly_iono" 			}, "Number of polynomial coefficient for SSR STEC corrections");
				trySetFromYaml (ssrOpts.grid_type, 			atmosphere, {"@ grid_type" 				}, "Grid type for gridded atmospheric corrections");
				trySetFromYaml (ssrOpts.use_grid_iono, 		atmosphere, {"@ use_grid_iono" 			}, "Grid type for gridded atmospheric corrections");
				trySetFromYaml (ssrOpts.use_grid_trop, 		atmosphere, {"@ use_grid_trop" 			}, "Grid type for gridded atmospheric corrections");
				trySetFromYaml (ssrOpts.max_lat, 			atmosphere, {"@ max_lat" 				}, "Number of basis for SSR trop corrections");
				trySetFromYaml (ssrOpts.min_lat, 			atmosphere, {"@ min_lat" 				}, "Number of basis for SSR trop corrections");
				trySetFromYaml (ssrOpts.int_lat, 			atmosphere, {"@ int_lat" 				}, "Number of basis for SSR trop corrections");
				trySetFromYaml (ssrOpts.max_lon, 			atmosphere, {"@ max_lon" 				}, "Number of basis for SSR trop corrections");
				trySetFromYaml (ssrOpts.min_lon, 			atmosphere, {"@ min_lon" 				}, "Number of basis for SSR trop corrections");
				trySetFromYaml (ssrOpts.int_lon, 			atmosphere, {"@ int_lon" 				}, "Number of basis for SSR trop corrections");
			}
		}

		{
			auto ssr_inputs = stringsToYamlObject(processing_options, {"! ssr_inputs"}, docs["ssr_inputs"]);

			trySetFromYaml(ssrInOpts.code_bias_valid_time,	ssr_inputs, {"! code_bias_validity_time"	},	"(double) Valid time period of SSR code biases");
			trySetFromYaml(ssrInOpts.phase_bias_valid_time,	ssr_inputs, {"! phase_bias_validity_time"	},	"(double) Valid time period of SSR phase biases");
			trySetFromYaml(ssrInOpts.one_freq_phase_bias,	ssr_inputs, {"! one_freq_phase_bias"		},	"(bool)   Used stream have one SSR phase bias per frequency");
			trySetFromYaml(ssrInOpts.global_vtec_valid_time,ssr_inputs, {"! global_vtec_valid_time"		},	"(double) Valid time period of global VTEC maps");
			trySetFromYaml(ssrInOpts.local_stec_valid_time,	ssr_inputs, {"! local_stec_valid_time"		},	"(double) Valid time period of local STEC corrections");
			trySetFromYaml(validity_interval_factor,		ssr_inputs, {"@ validity_interval_factor"	});
			trySetEnumOpt(ssr_input_antenna_offset,			ssr_inputs,	{"! ssr_antenna_offset"			}, E_OffsetType::_from_string_nocase, "Ephemeris type that is provided in the listed SSR stream, i.e. satellite antenna-phase-centre (APC) or centre-of-mass (COM). This information is listed in the NTRIP Caster's sourcetable");
		}

		auto estimation_parameters = stringsToYamlObject({yaml, ""}, {estimation_parameters_str});
		{
			trySetKalmanFromYaml(pppOpts.eop,		estimation_parameters, "@ eop"			);
			trySetKalmanFromYaml(pppOpts.eop_rates,	estimation_parameters, "@ eop_rates"	);
			trySetKalmanFromYaml(ionModelOpts.ion,	estimation_parameters, "@ ion"			);
		}

		{
			auto mongo = stringsToYamlObject({yaml, ""}, {"5!  mongo"}, "Mongo is a database used to store results and intermediate values for later analysis and inter-process communication");

			trySetFromYaml(localMongo.enable,						mongo, {"0! enable"					}, "(bool) Enable and connect to mongo database");
			trySetFromYaml(localMongo.predict_states,				mongo, {"@ predict_states"			}, "(bool) ");
			trySetFromYaml(localMongo.output_rtcm_messages,			mongo, {"@ output_rtcm_messages"	}, "(bool) Output rtcm data to mongo");
			trySetFromYaml(localMongo.output_measurements,			mongo, {"! output_measurements"		}, "(bool) Output measurements and their residuals");
			trySetFromYaml(localMongo.output_components,			mongo, {"! output_components"		}, "(bool) Output components of measurements");
			trySetFromYaml(localMongo.output_states,				mongo, {"! output_states"			}, "(bool) Output states");
			trySetFromYaml(localMongo.output_trace,					mongo, {"@ output_trace"			}, "(bool) Output trace");
			trySetFromYaml(localMongo.output_test_stats,			mongo, {"@ output_test_stats"		}, "(bool) Output test statistics");
			trySetFromYaml(localMongo.output_logs,					mongo, {"@ output_logs"				}, "(bool) Output console trace and warnings to mongo with timestamps and other metadata");
			trySetFromYaml(localMongo.output_ssr_precursors,		mongo, {"@ output_ssr_precursors"	}, "(bool) Output orbits, clocks, and bias estimates to allow communication to ssr generating processes");
			trySetFromYaml(localMongo.delete_history,				mongo, {"! delete_history"			}, "(bool) Drop the collection in the database at the beginning of the run to only show fresh data");
			trySetFromYaml(localMongo.cull_history,					mongo, {"@ cull_history"			}, "(bool) Erase old database objects to limit the size and speed degredation over long runs");
			trySetFromYaml(localMongo.min_cull_age,					mongo, {"@ min_cull_age"			}, "(float) Age of which to cull history");
			trySetFromYaml(localMongo.suffix,						mongo, {"@ suffix"					}, "(string) Suffix to append to database elements to make distinctions between runs for comparison");
			trySetFromYaml(localMongo.database,						mongo, {"@ database"				}, "(string) ");
			trySetFromYaml(localMongo.uri,							mongo, {"@ uri"						}, "(string) Location and port of the mongo database to connect to");
			
			trySetScaledFromYaml(localMongo.prediction_offset,				mongo, {"@ prediction_offset"			},	{"@ interval_units"	},	E_Period::_from_string_nocase);
			trySetScaledFromYaml(localMongo.prediction_interval,			mongo, {"@ prediction_interval"			},	{"@ interval_units"	},	E_Period::_from_string_nocase);
			trySetScaledFromYaml(localMongo.forward_prediction_duration,	mongo, {"@ forward_prediction_duration"	},	{"@ duration_units"	},	E_Period::_from_string_nocase);
			trySetScaledFromYaml(localMongo.reverse_prediction_duration,	mongo, {"@ reverse_prediction_duration"	},	{"@ duration_units"	},	E_Period::_from_string_nocase);
		}

		{
			auto mongo = stringsToYamlObject({yaml, ""}, {"5@ remote_mongo"});

			trySetFromYaml(remoteMongo.enable,						mongo, {"0 enable"						}, "(bool) Enable and connect to mongo database");
			trySetFromYaml(remoteMongo.predict_states,				mongo, {"@ predict_states"				}, "(bool)");
			trySetFromYaml(remoteMongo.output_rtcm_messages,		mongo, {"@ output_rtcm_messages"		}, "(bool) Output rtcm data to mongo");
			trySetFromYaml(remoteMongo.output_measurements,			mongo, {"@ output_measurements"			}, "(bool) Output measurements and their residuals");
			trySetFromYaml(remoteMongo.output_components,			mongo, {"@ output_components"			}, "(bool) Output components of measurements");
			trySetFromYaml(remoteMongo.output_states,				mongo, {"@ output_states"				}, "(bool) Output states");
			trySetFromYaml(remoteMongo.output_trace,				mongo, {"@ output_trace"				}, "(bool) Output trace");
			trySetFromYaml(remoteMongo.output_test_stats,			mongo, {"@ output_test_stats"			}, "(bool) Output test statistics");
			trySetFromYaml(remoteMongo.output_logs,					mongo, {"@ output_logs"					}, "(bool) Output console trace and warnings to mongo with timestamps and other metadata");
			trySetFromYaml(remoteMongo.output_ssr_precursors,		mongo, {"@ output_ssr_precursors"		}, "(bool) ");
			trySetFromYaml(remoteMongo.delete_history,				mongo, {"@ delete_history"				}, "(bool) Drop the collection in the database at the beginning of the run to only show fresh data");
			trySetFromYaml(remoteMongo.cull_history,				mongo, {"@ cull_history"				}, "(bool) ");
			trySetFromYaml(remoteMongo.min_cull_age,				mongo, {"@ min_cull_age"				}, "(float) ");
			trySetFromYaml(remoteMongo.suffix,						mongo, {"@ suffix"						}, "(string) Suffix to append to database elements to make distinctions between runs for comparison");
			trySetFromYaml(remoteMongo.database,					mongo, {"@ database"					}, "(string) ");
			trySetFromYaml(remoteMongo.uri,							mongo, {"@ uri"							}, "(string) Location and port of the mongo database to connect to");
		
			trySetScaledFromYaml(remoteMongo.prediction_offset,				mongo, {"@ prediction_offset"			},	{"@ interval_units"	},	E_Period::_from_string_nocase);
			trySetScaledFromYaml(remoteMongo.prediction_interval,			mongo, {"@ prediction_interval"			},	{"@ interval_units"	},	E_Period::_from_string_nocase);
			trySetScaledFromYaml(remoteMongo.forward_prediction_duration,	mongo, {"@ forward_prediction_duration"	},	{"@ duration_units"	},	E_Period::_from_string_nocase);
			trySetScaledFromYaml(remoteMongo.reverse_prediction_duration,	mongo, {"@ reverse_prediction_duration"	},	{"@ duration_units"	},	E_Period::_from_string_nocase);
		}

		{
			auto debug = stringsToYamlObject({yaml, ""}, {"9 debug"}, "Debug options are designed for developers and should probably not be used by normal users");

			trySetFromYaml(instrument,					debug, {"instrument"				}, "(bool) Debugging option to show run times of functions");
			trySetFromYaml(instrument_once_per_epoch,	debug, {"instrument_once_per_epoch"	}, "(bool) Debugging option to show run times of functions every epoch");
			trySetFromYaml(check_plumbing,				debug, {"check_plumbing"			}, "(bool) Debugging option to show sizes of objects in memory to detect leaks");
			trySetFromYaml(explain_measurements,		debug, {"explain_measurements"		}, "(bool) Debugging option to show verbose measurement coefficients");
			trySetFromYaml(retain_rts_files,			debug, {"retain_rts_files"			}, "(bool) Debugging option to keep rts files for post processing");
			trySetFromYaml(rts_only,					debug, {"rts_only"					}, "(bool) Debugging option to only re-run rts from previous run");
			trySetFromYaml(mincon_only,					debug, {"mincon_only"				}, "(bool) Debugging option to only save and re-run minimum constraints code");
			trySetFromYaml(pseudo_pulses,				debug, {"pseudo_pulses"				}, "(bool) Debugging option to add this many bad measurements per day to force pseudo stochastic pulses in orbit estimates");
		}
	}
	
	//Try to change all filenames to replace <YYYY> etc with other values.
	replaceTags(gnss_obs_root);
	replaceTags(pseudo_obs_root);
	replaceTags(sat_data_root);
	
	replaceTags(vmf_files);				globber(vmf_files);
	replaceTags(atx_files);				globber(atx_files);
	replaceTags(snx_files);				globber(snx_files);
	replaceTags(blq_files);				globber(blq_files);
	replaceTags(erp_files);				globber(erp_files);
	replaceTags(ion_files);				globber(ion_files);
	replaceTags(nav_files);				globber(nav_files);
	replaceTags(sp3_files);				globber(sp3_files);
	replaceTags(dcb_files);				globber(dcb_files);
	replaceTags(bsx_files);				globber(bsx_files);
	replaceTags(igrf_files);			globber(igrf_files);
	replaceTags(clk_files);				globber(clk_files);
	replaceTags(obx_files);				globber(obx_files);
	replaceTags(sid_files);				globber(sid_files);
	replaceTags(com_files);				globber(com_files);
	replaceTags(crd_files);				globber(crd_files);
	replaceTags(egm_files);				globber(egm_files);
	replaceTags(jpl_files);				globber(jpl_files);
	replaceTags(tide_files);			globber(tide_files);
    replaceTags(cmc_files);				globber(cmc_files);
    replaceTags(hfeop_files);			globber(hfeop_files);

	replaceTags(rnx_inputs);			globber(rnx_inputs);
	replaceTags(ubx_inputs);			globber(ubx_inputs);                                                         	
	replaceTags(obs_rtcm_inputs);		globber(obs_rtcm_inputs);
	replaceTags(pseudo_sp3_inputs);		globber(pseudo_sp3_inputs);
	replaceTags(pseudo_snx_inputs);		globber(pseudo_snx_inputs);
	replaceTags(nav_rtcm_inputs);		globber(nav_rtcm_inputs);

	replaceTags(model.trop.orography);
	replaceTags(model.trop.gpt2grid);
	
	replaceTags(sp3_directory);							replaceTags(sp3_filename,						sp3_directory);						
	replaceTags(erp_directory);							replaceTags(erp_filename,						erp_directory);						
	replaceTags(gpx_directory);							replaceTags(gpx_filename,						gpx_directory);						
	replaceTags(log_directory);							replaceTags(log_filename,						log_directory);						
	replaceTags(cost_directory);						replaceTags(cost_filename,						cost_directory);					
	replaceTags(sinex_directory);						replaceTags(sinex_filename,						sinex_directory);					
	replaceTags(ionex_directory);						replaceTags(ionex_filename,						ionex_directory);					
	replaceTags(orbex_directory);						replaceTags(orbex_filename,						orbex_directory);					
	replaceTags(clocks_directory);						replaceTags(clocks_filename,					clocks_directory);					
	replaceTags(slr_obs_directory);						replaceTags(slr_obs_filename,					slr_obs_directory);					
	replaceTags(ionstec_directory);						replaceTags(ionstec_filename,					ionstec_directory);					
	replaceTags(ppp_sol_directory);						replaceTags(ppp_sol_filename,					ppp_sol_directory);					
	replaceTags(raw_ubx_directory);						replaceTags(raw_ubx_filename,					raw_ubx_directory);					
	replaceTags(rtcm_nav_directory);					replaceTags(rtcm_nav_filename,					rtcm_nav_directory);				
	replaceTags(rtcm_obs_directory);					replaceTags(rtcm_obs_filename,					rtcm_obs_directory);			
	replaceTags(orbit_ics_directory);					replaceTags(orbit_ics_filename,					orbit_ics_directory);				
	replaceTags(ntrip_log_directory);					replaceTags(ntrip_log_filename,					ntrip_log_directory);				
	replaceTags(rinex_obs_directory);					replaceTags(rinex_obs_filename,					rinex_obs_directory);				
	replaceTags(rinex_nav_directory);					replaceTags(rinex_nav_filename,					rinex_nav_directory);				
	replaceTags(sp3_directory);							replaceTags(predicted_sp3_filename,				sp3_directory);						
	replaceTags(bias_sinex_directory);					replaceTags(bias_sinex_filename,				bias_sinex_directory);				
	replaceTags(trop_sinex_directory);					replaceTags(trop_sinex_filename,				trop_sinex_directory);				
	replaceTags(pppOpts.rts_directory);					replaceTags(pppOpts.rts_filename,				pppOpts.rts_directory);				
	replaceTags(trace_directory);						replaceTags(satellite_trace_filename,			trace_directory);					
	replaceTags(trace_directory);						replaceTags(station_trace_filename,				trace_directory);					
	replaceTags(trace_directory);						replaceTags(network_trace_filename,				trace_directory);					
	replaceTags(decoded_rtcm_json_directory);			replaceTags(decoded_rtcm_json_filename,			decoded_rtcm_json_directory);		
	replaceTags(encoded_rtcm_json_directory);			replaceTags(encoded_rtcm_json_filename,			encoded_rtcm_json_directory);		
	replaceTags(network_statistics_json_directory);		replaceTags(network_statistics_json_filename,	network_statistics_json_directory);	

	
	replaceTags(localMongo.suffix);
	replaceTags(localMongo.database);
	
	replaceTags(remoteMongo.suffix);
	replaceTags(remoteMongo.database);
	
	SatSys dummySat("G01");
	getSatOpts(dummySat, {"L1W"});
	getRecOpts("global");
	getRecOpts("XMPL");

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
