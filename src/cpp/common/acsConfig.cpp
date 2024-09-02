
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

/** YAML based configuration.
 * The Pea uses one or more yaml files as its source of configuration. YAML files are simple heirarchical documents which are straightforward to edit in basic text editors.
 *
 *
 * The configuration parser performs many functions simultaneously, which may lead to issues if modifications are made without due care.
 * In most cases, finding a similar configuration and duplicating all references to it will be the most straightforward method of creating new parameters.
 *
 * The lines in this file can become very long, because they do perform so many functions at once, however they should be aligned such that columns of similar parameters form naturally on the screen.
 * This is preferable to more standard formatting because of the vast amount of duplication that would be required otherwise. The block-select tool in the IDE will come in handy.
 * There are cases where artificial scoping is used to allow dummy variables to be used such that rather than copying one variable name into multiple function calls, which is always a source of bugs,
 * a single common parameter name may be used across many unrelated lines. `thing` is one such variable name.
 *

 */
Architecture Config__()
{
	DOCS_REFERENCE(Globbing_And_Tags__);
	DOCS_REFERENCE(Aliases_And_Inheritance__);
	DOCS_REFERENCE(Default_Values_And_Configurator__);
}

/** Automatic expansion of configs.
 *
 */
Architecture Globbing_And_Tags__()
{

}

/** Some configuration structures are possible to be configured specific for eg a receiver or signal or combination of both.
 * *
 * Rather than creating multitudes of 'initialised' variables to correspond with config parameters, a map and pointer algebra is used to keep track of such things.
 * Typically, the base container, the member variable, and a boolean to signify initialisation are included in a parsing line.
 *
 * Inheritance is performed by using the overloaded += operator on an object, with any configurations that have been initialised in the secondary object overwriting those values in the first.
 */
Architecture Aliases_And_Inheritance__()
{

}

/**
 *
 * Each configuration parameter is defined with a default initialisation value which is most sensible for the majority of use-cases.
 * Parameters which are not assigned using the config will default to those values, allowing the size of yaml files to be minimised.
 *
 * While parsing the code
 */
Architecture Default_Values_And_Configurator__()
{

}



FileType YAML__()
{

}


#include <filesystem>
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

#include "interactiveTerminal.hpp"
#include "peaCommitStrings.hpp"
#include "inputsOutputs.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "compare.hpp"
#include "debug.hpp"


ACSConfig acsConfig = {};

/** Helper object to output the use-cases and potential values for enum options
 */
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
void tryGetValFromVM(
	boost::program_options::variables_map&	vm,		///< Variable map to search in
	const string&							key, 	///< Variable name
	TYPE&									output)	///< Destination to set
{
	if (vm.count(key))
	{
		output = vm[key].as<TYPE>();
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

	replaceString(prefix,	"<CWD>", std::filesystem::current_path());
	replaceString(path,		"<CWD>", std::filesystem::current_path());

	char* home = std::getenv("HOME");
	if	( prefix[0] == '~'
		&&home)
	{
		prefix.erase(0, 1);
		prefix.insert(0, home);
	}

	if	( path[0] == '~'
		&&home)
	{
		path.erase(0, 1);
		path.insert(0, home);
	}

	if (std::filesystem::path(path).is_absolute())
	{
		return;
	}

	if (prefix.back() == '/')	path = prefix		+ path;
	else						path = prefix + "/"	+ path;
}

void conditionalPrefix(
	const	string&			prefix,
			vector<string>& paths,
			bool			condition = true)
{
	for (auto& path : paths)
	{
		conditionalPrefix(prefix, path, condition);
	}
}

void conditionalPrefix(
	const	string&							prefix,
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
			<< "Warning: " << subStr << " is used in config near " << str << " but is not defined...";
		}

		str.erase	(index, subStr.size());

		if (replacement.back() == '/')	str.insert(index, replacement.substr(0, replacement.size() -1));
		else							str.insert(index, replacement);

		replaced = true;
	}

	return replaced;
}

/** Replace macros for times with configured values.
*/
void replaceTags(
	string& str)		///< String to replace macros within
{
	char* home = std::getenv("HOME");

					replaceString(str, "<SAT_DATA_ROOT>",					acsConfig.sat_data_root);
					replaceString(str, "<GNSS_OBS_ROOT>",					acsConfig.gnss_obs_root);
					replaceString(str, "<PSEUDO_OBS_ROOT>",					acsConfig.pseudo_obs_root);
					replaceString(str, "<RTCM_INPUTS_ROOT>",				acsConfig.rtcm_inputs_root);
					replaceString(str, "<SISNET_INPUTS_ROOT>",				acsConfig.sisnet_inputs_root);
					replaceString(str, "<ROOT_STREAM_URL>",					acsConfig.root_stream_url);
					replaceString(str, "<HASH>",							ginanCommitHash());
					replaceString(str, "<BRANCH>",							ginanBranchName());
					replaceString(str, "<AGENCY>",							acsConfig.analysis_agency);
					replaceString(str, "<SOFTWARE>",						acsConfig.analysis_software.substr(0,3));
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
					replaceString(str, "<POS_DIRECTORY>",					acsConfig.pos_directory);
					replaceString(str, "<NTRIP_LOG_DIRECTORY>",				acsConfig.ntrip_log_directory);
					replaceString(str, "<NETWORK_STATISTICS_DIRECTORY>",	acsConfig.network_statistics_json_directory);
					replaceString(str, "<SP3_DIRECTORY>",					acsConfig.sp3_directory);
					replaceString(str, "<ORBIT_ICS_DIRECTORY>",				acsConfig.orbit_ics_directory);
					replaceString(str, "<ORBEX_DIRECTORY>",					acsConfig.orbex_directory);
					replaceString(str, "<COST_DIRECTORY>",					acsConfig.cost_directory);
					replaceString(str, "<RINEX_NAV_DIRECTORY>",				acsConfig.rinex_nav_directory);
					replaceString(str, "<RINEX_OBS_DIRECTORY>",				acsConfig.rinex_obs_directory);
					replaceString(str, "<RTCM_NAV_DIRECTORY>",				acsConfig.rtcm_nav_directory);
					replaceString(str, "<RTCM_OBS_DIRECTORY>",				acsConfig.rtcm_obs_directory);
					replaceString(str, "<CUSTOM_DIRECTORY>",				acsConfig.raw_custom_directory);
					replaceString(str, "<UBX_DIRECTORY>",					acsConfig.raw_ubx_directory);
					replaceString(str, "<SLR_OBS_DIRECTORY>",				acsConfig.slr_obs_directory);
					replaceString(str, "<TROP_SINEX_DIRECTORY>",			acsConfig.trop_sinex_directory);
					replaceString(str, "<EMS_DIRECTORY>",					acsConfig.ems_directory);
					replaceString(str, "<RTS_DIRECTORY>",					acsConfig.pppOpts.rts_directory);
					replaceString(str, "<OUTPUTS_ROOT>",					acsConfig.outputs_root);
					replaceString(str, "<USER>",							acsConfig.stream_user);
					replaceString(str, "<PASS>",							acsConfig.stream_pass);
					replaceString(str, "<CONFIG>",							acsConfig.config_description);
					replaceString(str, "<CWD>",								std::filesystem::current_path());
	if (home)		replaceString(str, "~",									home);
}

void replaceTags(
	vector<string>&		strs)
{
	for (auto& str : strs)
	{
		replaceTags(str);
	}
}

void replaceTags(
	map<string, vector<string>>&	strs)
{
	for (auto& [id, str] : strs)
	{
		replaceTags(str);
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

		std::filesystem::path		filePath(fileName);
		std::filesystem::path		searchDir	= filePath.parent_path();
		string						searchGlob	= filePath.filename().string();

		if (std::filesystem::is_directory(searchDir) == false)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error: Invalid input directory "
			<< searchDir;

			continue;
		}

		vector<string> globFiles;

		for (auto dir_file : std::filesystem::directory_iterator(searchDir))
		{
			// Skip if not a file
			if (std::filesystem::is_regular_file(dir_file) == false)
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
			trace << str << "\n";
		}
	}
}

string stringify(
	string& value)
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
	vector<TYPE> vec)
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
	const string&	stack,
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

/** Helper object to allow push-pop style indentation in outputs
 */
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

/** Helper object to temporarily and automatically disable and reenable a stream when the object goes out of scope
 */
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
	const string&	root = "")		///< Common root to determine extent of siblings relationship
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
			TempStreamDisabler disableMd	(md);

			auto& [stack,		defaultVals]	= *it;
			auto& defaultVal					= defaultVals.defaultValue;
			auto& type							= defaultVals.typeName;
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
			string flatStack;
			int optionLevel = 4;
			//find each part of the stack for this entry and make a list of them
			while ((pos_end = stack.find(":", pos_start)) != string::npos)
			{
				token = stack.substr(pos_start, pos_end - pos_start);
				pos_start = pos_end + 1;
				string cutstr;
				token = nonNumericStack(token, cutstr);
				flatStack += token;

				if		(cutstr.find('!') != string::npos)		optionLevel = 1;
				else if	(cutstr.find('@') != string::npos)		optionLevel = 2;
				else if	(cutstr.find('#') != string::npos)		optionLevel = 3;

				if (optionLevel > level)
				{
					disableCout	.disable();
					disableMd	.disable();
				}
			}

			//output the boilerplate of the name, and comment up to the point where the children are nested
			tracepdeex(0, std::cout, "\n%s%s\t%-30s", ((string)indentor).c_str(), token.c_str(), (defaultVal).c_str());

			html << "\n" <<	htmlIndentor++		<< "<div class='element level" << optionLevel << "'>";
			html << "\n" <<	htmlIndentor		<< "<input type='checkbox' id='" << flatStack << "'>";
			html << "\n" <<	htmlIndentor++		<< "<div class='ident' data-indent='" <<  indentor << "'>"
					<< (nextIsChild ? "<b>" : "") << token
					<< (nextIsChild ? " ⯆</b>" : "");

			mdIndentor++;
			if (mdIndentor.indentation == 2)
			{
				md << "\n" << mdIndentor << " " << token << "\n";
			}

			string link;

			if (defaults.enumName.empty() == false)
			{
				string linkName = defaults.enumName;

				if (defaultVal.find('[') == string::npos)	link += "[`"	+ linkName +  "`]";
				else										link += "[`["	+ linkName + "]`]";

				link += "(#" + boost::algorithm::to_lower_copy(linkName) + ") ";

			}

			md << "\n" << "###### **`" << flatStack << "`**";

			md << "\n" << " " << link << "`" << defaultVal << " `" << "\n";

			if (comment.empty() == false)
			{
				std::cout << "\t# " << comment.substr(0, comment.find('.'));
				html << "\n" <<	htmlIndentor		<< "<span class='tooltiptext'># " << comment << "</span>";

				auto period = comment.find('.');
				md << "\n" << "\n";

				if (nextIsChild)
					md << "> ";

				md << comment;//.substr(0, period);
// 				if	( period != string::npos
// 					&&period + 2 < comment.size())
// 				{
// 					md << "\n" << "\n" <<		comment.substr(period + 2);
// 				}
			}

			html << "\n" << --	htmlIndentor		<< "</div>";


			md		<< "\n" << "\n" << "---" << "\n";

			//
			bool firstChild = false;
			if (nextIsChild)
			{
				if (firstChild == false)
				{
					//initiate the section for embedding children nodes
					firstChild = true;

					html << "\n" <<	htmlIndentor++	<< "<div class='contents'>";
				}

				// recurse to do children of this node
				indentor++;
				outputDefaultSiblings(level, html, md, it, indentor, htmlIndentor, mdIndentor, stack);
				--indentor;
			}

			if (firstChild)
			{
				//finalise the child section
				html << "\n" << --	htmlIndentor		<< "</div>";
			}
			else
			{
				//this has no children, output the default value of this parameter instead - according to its commented parameter type

				for (auto once : {1})
				{
					//booleans
					if (type == typeid(bool).name())
					{

						html << "\n" <<	htmlIndentor++	<< "<select class='value'>";
						html << "\n" <<	htmlIndentor	<< "<option value='true' "	<< (defaultVal == "true"	? " selected" : "") << ">true</option>";
						html << "\n" <<	htmlIndentor	<< "<option value='false' "	<< (defaultVal == "false"	? " selected" : "") << ">false</option>";
						html << "\n" <<	htmlIndentor	<< "<option value='1' hidden>true</option>";
						html << "\n" <<	htmlIndentor	<< "<option value='0' hidden>false</option>";
						html << "\n" << --	htmlIndentor	<< "</select>";
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

						html << "\n" <<	htmlIndentor++	<< "<select class='value'>";

						//find each part of the stack for this entry and make a list of them
						while ((pos_end = enums.find(',', pos_start)) != string::npos)
						{
							string token = enums.substr(pos_start, pos_end - pos_start);
							pos_start = pos_end + 1;
							html << "\n" <<	htmlIndentor	<< "<option value='" << token << "'>" << token << "</option>";
						}
						//get last one
						string token = enums.substr(pos_start);
						html << "\n" <<	htmlIndentor	<< "<option value='" << token << "'>" << token << "</option>";

						html << "\n" << --	htmlIndentor	<< "</select>";

						break;
					}

					//general parameters
					{
						html << "\n" << htmlIndentor << "<input type='text' class='value' value='" << defaultVal << "'>";
					}

				}
			}

			html	<< "\n" << --htmlIndentor		<< "</div>";

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
	std::cout << "\n" << "Default configuration values:\n\n";

	std::ofstream html	("GinanYamlInspector.html");
	std::ofstream md	("defaultConfiguration.md");

	html <<
	#include "htmlHeaderTemplate.html"
	<< "\n";

	auto it = acsConfig.yamlDefaults.begin();

	Indentor indentor;
	Indentor htmlIndentor;
	Indentor mdIndentor('#', 1);
	mdIndentor++;
	md << "\n" << mdIndentor << " Default Configuration" << "\n";

	md << "\n" << "This document outlines the major configuration options available in ginan that are most applicable to general users. "
	<< "For more advanced configuration options and their defaults, use the `-Y <level>` option at the command line to view increasing levels of advanced configurations.";

	outputDefaultSiblings(level, html, md, it, indentor, htmlIndentor, mdIndentor);

	std::cout << "\n" << "\n";

	std::cout << "An interactive configuration inspector has been generated and saved to GinanYamlInspector.html" << "\n";

	html <<
	#include "htmlFooterTemplate.html"
	<< "\n";

	md << "\n" << mdIndentor << " Enum Details" << "\n";

	for (auto& [enumName, details] : enumDetailsMap)
	{
		md << "\n" << "---";

		md << "\n" << "\n" << "### " + enumName;

		md << "\n" << "\n" << "Valid enum values are:";
		for (auto& value : details.enums)
		{
			md << "\n" << "- `" << value << "`";

			if (docs[value].empty() == false)
				md << " : " << docs[value];
		}

		md << "\n" << "\n" << "For options:" << "\n";
		for (auto& caller : details.usingOptions)
		{
			string dummy;
			md << "\n" << "- [`" << caller << "`](#" << nonNumericStack(caller, dummy, false) << ")";
		}
	}
}

void defaultConfigs()
{
// 	acsConfig.recOptsMap["GPS"].rinex23Conv.codeConv =
// 	{
// 		{E_Sys::GPS,{
// 						{E_ObsCode2::P1, E_ObsCode::L1W},
// 						{E_ObsCode2::P2, E_ObsCode::L2W},
// 						{E_ObsCode2::C1, E_ObsCode::L1C},
// 						{E_ObsCode2::C2, E_ObsCode::L2C},
// 						{E_ObsCode2::C5, E_ObsCode::L5X},
// 						{E_ObsCode2::L1, E_ObsCode::L1W},
// 						{E_ObsCode2::L2, E_ObsCode::L2C},
// 						{E_ObsCode2::L5, E_ObsCode::L5X}}
// 		},
//
// 		{E_Sys::GAL,{
// 						{E_ObsCode2::P1, E_ObsCode::L1P},
// 						{E_ObsCode2::P2, E_ObsCode::L2P},
// 						{E_ObsCode2::C1, E_ObsCode::L1C},
// 						{E_ObsCode2::C2, E_ObsCode::L2C},
// 						{E_ObsCode2::C5, E_ObsCode::L5I},
// 						{E_ObsCode2::C7, E_ObsCode::L7Q},
// 						{E_ObsCode2::C8, E_ObsCode::L8Q},
// 						{E_ObsCode2::L1, E_ObsCode::L1C},
// 						{E_ObsCode2::L2, E_ObsCode::L2P},
// 						{E_ObsCode2::L5, E_ObsCode::L5I},
// 						{E_ObsCode2::L7, E_ObsCode::L7Q},
// 						{E_ObsCode2::L8, E_ObsCode::L8Q}}
// 		},
//
// 		{E_Sys::GLO,{
// 						{E_ObsCode2::P1, E_ObsCode::L1P},
// 						{E_ObsCode2::P2, E_ObsCode::L2P},
// 						{E_ObsCode2::C1, E_ObsCode::L1C},
// 						{E_ObsCode2::C2, E_ObsCode::L2C},
// 						{E_ObsCode2::C3, E_ObsCode::L3Q},
// 						{E_ObsCode2::C5, E_ObsCode::L5I},
// 						{E_ObsCode2::L1, E_ObsCode::L1C},
// 						{E_ObsCode2::L2, E_ObsCode::L2P},
// 						{E_ObsCode2::L3, E_ObsCode::L3Q},
// 						{E_ObsCode2::L5, E_ObsCode::L5I}}
// 		},
//
// 		{E_Sys::BDS,{
// 						{E_ObsCode2::C2, E_ObsCode::L2I},
// 						{E_ObsCode2::C6, E_ObsCode::L6I},
// 						{E_ObsCode2::C7, E_ObsCode::L7I},
// 						{E_ObsCode2::C8, E_ObsCode::L8X},
// 						{E_ObsCode2::L2, E_ObsCode::L2X},
// 						{E_ObsCode2::L6, E_ObsCode::L6I},
// 						{E_ObsCode2::L7, E_ObsCode::L7I},
// 						{E_ObsCode2::L8, E_ObsCode::L8X}}
// 		},
//
// 		{E_Sys::QZS,{
// 						{E_ObsCode2::C1, E_ObsCode::L1C},
// 						{E_ObsCode2::C2, E_ObsCode::L2C},
// 						{E_ObsCode2::C5, E_ObsCode::L5I},
// 						{E_ObsCode2::C6, E_ObsCode::L6X},
// 						{E_ObsCode2::L1, E_ObsCode::L1C},
// 						{E_ObsCode2::L2, E_ObsCode::L2C},
// 						{E_ObsCode2::L5, E_ObsCode::L5I},
// 						{E_ObsCode2::L6, E_ObsCode::L6X}}
// 		},
//
// 		{E_Sys::SBS,{
// 						{E_ObsCode2::C1, E_ObsCode::L1C},
// 						{E_ObsCode2::C2, E_ObsCode::L2C},
// 						{E_ObsCode2::C5, E_ObsCode::L5I},
// 						{E_ObsCode2::L1, E_ObsCode::L1C},
// 						{E_ObsCode2::L2, E_ObsCode::L2C},
// 						{E_ObsCode2::L5, E_ObsCode::L5I}}
// 		}
// 	};
//
// 	acsConfig.recOptsMap["0"].rinex23Conv.phasConv =
// 	{
// 		{E_Sys::GPS,{
// 						{E_ObsCode2::P1, E_ObsCode::L1W},
// 						{E_ObsCode2::P2, E_ObsCode::L2W},
// 						{E_ObsCode2::C1, E_ObsCode::L1C},
// 						{E_ObsCode2::C2, E_ObsCode::L2C},
// 						{E_ObsCode2::C5, E_ObsCode::L5X},
// 						{E_ObsCode2::L1, E_ObsCode::L1W},
// 						{E_ObsCode2::L2, E_ObsCode::L2C},
// 						{E_ObsCode2::L5, E_ObsCode::L5X}}
// 		},
//
// 		{E_Sys::GAL,{
// 						{E_ObsCode2::P1, E_ObsCode::L1P},
// 						{E_ObsCode2::P2, E_ObsCode::L2P},
// 						{E_ObsCode2::C1, E_ObsCode::L1C},
// 						{E_ObsCode2::C2, E_ObsCode::L2C},
// 						{E_ObsCode2::C5, E_ObsCode::L5I},
// 						{E_ObsCode2::C7, E_ObsCode::L7Q},
// 						{E_ObsCode2::C8, E_ObsCode::L8Q},
// 						{E_ObsCode2::L1, E_ObsCode::L1C},
// 						{E_ObsCode2::L2, E_ObsCode::L2P},
// 						{E_ObsCode2::L5, E_ObsCode::L5I},
// 						{E_ObsCode2::L7, E_ObsCode::L7Q},
// 						{E_ObsCode2::L8, E_ObsCode::L8Q}}
// 		},
//
// 		{E_Sys::GLO,{
// 						{E_ObsCode2::P1, E_ObsCode::L1P},
// 						{E_ObsCode2::P2, E_ObsCode::L2P},
// 						{E_ObsCode2::C1, E_ObsCode::L1C},
// 						{E_ObsCode2::C2, E_ObsCode::L2C},
// 						{E_ObsCode2::C3, E_ObsCode::L3Q},
// 						{E_ObsCode2::C5, E_ObsCode::L5I},
// 						{E_ObsCode2::L1, E_ObsCode::L1C},
// 						{E_ObsCode2::L2, E_ObsCode::L2P},
// 						{E_ObsCode2::L3, E_ObsCode::L3Q},
// 						{E_ObsCode2::L5, E_ObsCode::L5I}}
// 		},
//
// 		{E_Sys::BDS,{
// 						{E_ObsCode2::C2, E_ObsCode::L2I},
// 						{E_ObsCode2::C6, E_ObsCode::L6I},
// 						{E_ObsCode2::C7, E_ObsCode::L7I},
// 						{E_ObsCode2::C8, E_ObsCode::L8X},
// 						{E_ObsCode2::L2, E_ObsCode::L2I},
// 						{E_ObsCode2::L6, E_ObsCode::L6I},
// 						{E_ObsCode2::L7, E_ObsCode::L7I},
// 						{E_ObsCode2::L8, E_ObsCode::L8X}}
// 		},
//
// 		{E_Sys::QZS,{
// 						{E_ObsCode2::C1, E_ObsCode::L1C},
// 						{E_ObsCode2::C2, E_ObsCode::L2C},
// 						{E_ObsCode2::C5, E_ObsCode::L5I},
// 						{E_ObsCode2::C6, E_ObsCode::L6X},
// 						{E_ObsCode2::L1, E_ObsCode::L1C},
// 						{E_ObsCode2::L2, E_ObsCode::L2C},
// 						{E_ObsCode2::L5, E_ObsCode::L5I},
// 						{E_ObsCode2::L6, E_ObsCode::L6X}}
// 		},
//
// 		{E_Sys::SBS,{
// 						{E_ObsCode2::C1, E_ObsCode::L1C},
// 						{E_ObsCode2::C2, E_ObsCode::L2C},
// 						{E_ObsCode2::C5, E_ObsCode::L5I},
// 						{E_ObsCode2::L1, E_ObsCode::L1C},
// 						{E_ObsCode2::L2, E_ObsCode::L2C},
// 						{E_ObsCode2::L5, E_ObsCode::L5I}}
// 		}
// 	};
}

/** Print out the configuration data that has been read in.
*/
void ACSConfig::info(
	Trace& s)		///< Trace file to output to
{
	std::stringstream ss;

	ss << "\n\n";
	ss << "===============================\n";
	ss << "Configuration Summary...\n";
	ss << "===============================\n";
	ss << "Inputs:\n";

	if (!nav_files						.empty())	{	ss << "\tnav_files:                       ";											for (auto& a : nav_files)						ss << a << " ";		ss << "\n";		}
	if (!snx_files						.empty())	{	ss << "\tsnx_files:                       ";											for (auto& a : snx_files)						ss << a << " ";		ss << "\n";		}
	if (!atx_files						.empty())	{	ss << "\tatx_files:                       ";											for (auto& a : atx_files)						ss << a << " ";		ss << "\n";		}
	if (!dcb_files						.empty())	{	ss << "\tdcb_files:                       ";											for (auto& a : dcb_files)						ss << a << " ";		ss << "\n";		}
	if (!clk_files						.empty())	{	ss << "\tclk_files:                       ";											for (auto& a : clk_files)						ss << a << " ";		ss << "\n";		}
	if (!bsx_files						.empty())	{	ss << "\tbsx_files:                       ";											for (auto& a : bsx_files)						ss << a << " ";		ss << "\n";		}
	if (!ion_files						.empty())	{	ss << "\tion_files:                       ";											for (auto& a : ion_files)						ss << a << " ";		ss << "\n";		}
	if (!igrf_files						.empty())	{	ss << "\tigrf_files:                      ";											for (auto& a : igrf_files)						ss << a << " ";		ss << "\n";		}
	if (!ocean_tide_loading_blq_files	.empty())	{	ss << "\tocean_tide_loading_blq_files:    ";											for (auto& a : ocean_tide_loading_blq_files)	ss << a << " ";		ss << "\n";		}
	if (!atmos_tide_loading_blq_files	.empty())	{	ss << "\tatmos_tide_loading_blq_files:    ";											for (auto& a : atmos_tide_loading_blq_files)	ss << a << " ";		ss << "\n";		}
	if (!ocean_pole_tide_loading_files	.empty())	{	ss << "\tocean_pole_tide_loading_files:   ";											for (auto& a : ocean_pole_tide_loading_files)	ss << a << " ";		ss << "\n";		}
	if (!pseudo_filter_files			.empty())	{	ss << "\tpseudo_filter_files:             ";											for (auto& a : pseudo_filter_files)				ss << a << " ";		ss << "\n";		}
	if (!erp_files						.empty())	{	ss << "\terp_files:                       ";											for (auto& a : erp_files)						ss << a << " ";		ss << "\n";		}
	if (!sp3_files						.empty())	{	ss << "\tsp3_files:                       ";											for (auto& a : sp3_files)						ss << a << " ";		ss << "\n";		}
	if (!obx_files						.empty())	{	ss << "\tobx_files:                       ";											for (auto& a : obx_files)						ss << a << " ";		ss << "\n";		}
	if (!egm_files						.empty())	{	ss << "\tegm_files:                       ";											for (auto& a : egm_files)						ss << a << " ";		ss << "\n";		}
	if (!planetary_ephemeris_files		.empty())	{	ss << "\tplanetary_ephemeris_files:       ";											for (auto& a : planetary_ephemeris_files)		ss << a << " ";		ss << "\n";		}
	if (!ocean_tide_potential_files		.empty())	{	ss << "\tocean_tide_potential_files:      ";											for (auto& a : ocean_tide_potential_files)		ss << a << " ";		ss << "\n";		}
	if (!atmos_tide_potential_files		.empty())	{	ss << "\tatmos_tide_potential_files:      ";											for (auto& a : atmos_tide_potential_files)		ss << a << " ";		ss << "\n";		}
	if (!cmc_files						.empty())	{	ss << "\tcmc_files:                       ";											for (auto& a : cmc_files)						ss << a << " ";		ss << "\n";		}
	if (!hfeop_files					.empty())	{	ss << "\thfeop_files:                     ";											for (auto& a : hfeop_files)						ss << a << " ";		ss << "\n";		}
	if (!atmos_oceean_dealiasing_files	.empty())	{	ss << "\tatmos_oceean_dealiasing_files:   ";											for (auto& a : atmos_oceean_dealiasing_files)	ss << a << " ";		ss << "\n";		}
	if (!ocean_pole_tide_potential_files.empty())	{	ss << "\tocean_pole_tide_potential_files: ";											for (auto& a : ocean_pole_tide_potential_files)	ss << a << " ";		ss << "\n";		}
	if (!sid_files						.empty())	{	ss << "\tsid_files:                       ";											for (auto& a : sid_files)						ss << a << " ";		ss << "\n";		}
	if (!vmf_files						.empty())	{	ss << "\tvmf_files:                       ";											for (auto& a : vmf_files)						ss << a << " ";		ss << "\n";		}
	if (!com_files						.empty())	{	ss << "\tcom_files:                       ";											for (auto& a : com_files)						ss << a << " ";		ss << "\n";		}
	if (!orography_files				.empty())	{	ss << "\torography_files:                 ";											for (auto& a : orography_files)					ss << a << " ";		ss << "\n";		}
	if (!gpt2grid_files					.empty())	{	ss << "\tgpt2grid_files:                  ";											for (auto& a : gpt2grid_files)					ss << a << " ";		ss << "\n";		}
	if (!nav_rtcm_inputs				.empty())	{	ss << "\trtcm_inputs:                     ";											for (auto& a : nav_rtcm_inputs)					ss << a << " ";		ss << "\n";		}
	if (!qzs_rtcm_inputs				.empty())	{	ss << "\tqzl6_inputs:                     ";											for (auto& a : qzs_rtcm_inputs)					ss << a << " ";		ss << "\n";		}
	if (!sisnet_inputs					.empty())	{	ss << "\tsisnet_inputs:                   ";											for (auto& a : sisnet_inputs)					ss << a << " ";		ss << "\n";		}
	if (!rnx_inputs						.empty())	{	ss << "\trnx_inputs:                      ";	for (auto& [z, A] : rnx_inputs)			for (auto& a : A)								ss << a << " ";		ss << "\n";		}
	if (!pseudo_sp3_inputs				.empty())	{	ss << "\tsp3_inputs:                      ";	for (auto& [z, A] : pseudo_sp3_inputs)	for (auto& a : A)								ss << a << " ";		ss << "\n";		}
	if (!pseudo_snx_inputs				.empty())	{	ss << "\tsnx_inputs:                      ";	for (auto& [z, A] : pseudo_snx_inputs)	for (auto& a : A)								ss << a << " ";		ss << "\n";		}
	if (!obs_rtcm_inputs				.empty())	{	ss << "\trtcm_inputs:                     ";	for (auto& [z, A] : obs_rtcm_inputs)	for (auto& a : A)								ss << a << " ";		ss << "\n";		}

														ss << "\n";

	ss << "Outputs:\n";
	if (1)									{	ss << "\ttrace level:                   " << trace_level 						<< "\n"; }
	if (output_satellite_trace)				{	ss << "\tsatellite trace filename:      " << satellite_trace_filename 			<< "\n"; }
	if (output_receiver_trace)				{	ss << "\treceiver trace filename:       " << receiver_trace_filename 			<< "\n"; }
	if (output_json_trace)					{	ss << "\tjson trace filename:           " << receiver_trace_filename + "_json"	<< "\n"; }
	if (output_network_trace)				{	ss << "\tnetwork trace filename:        " << network_trace_filename 			<< "\n"; }
	if (output_ionosphere_trace)			{	ss << "\tionosphere trace filename:     " << ionosphere_trace_filename 			<< "\n"; }
	if (output_clocks)						{	ss << "\tclocks filename:               " << clocks_filename 					<< "\n"; }
	if (output_ionex)						{	ss << "\tionex filename:                " << ionex_filename 					<< "\n"; }
	if (output_sinex)						{	ss << "\tsinex filename:                " << sinex_filename 					<< "\n"; }
	if (output_ionstec)						{	ss << "\tionstec filename:              " << ionstec_filename 					<< "\n"; }
	if (output_bias_sinex)					{	ss << "\tbias sinex filename:           " << bias_sinex_filename				<< "\n"; }
	if (output_cost)						{	ss << "\tcost filename:                 " << cost_filename						<< "\n"; }
	if (output_trop_sinex)					{	ss << "\ttrop sinex filename:           " << trop_sinex_filename				<< "\n"; }
	if (output_gpx)							{	ss << "\tgpx filename:                  " << gpx_filename						<< "\n"; }
	if (output_pos)							{	ss << "\tpos filename:                  " << pos_filename						<< "\n"; }
	if (output_sp3)							{	ss << "\tsp3 filename:                  " << sp3_filename						<< "\n"; }
	if (output_decoded_rtcm_json)			{	ss << "\tdecoded rtcm json filename:    " << decoded_rtcm_json_filename			<< "\n"; }
	if (output_encoded_rtcm_json)			{	ss << "\tencoded rtcm json filename:    " << encoded_rtcm_json_filename			<< "\n"; }
	if (output_sbas_ems)					{	ss << "\tSBAS EMS filename:             " << ems_filename 						<< "\n"; }

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

	ss << "Epochs:\n";
	if (epoch_interval	> 0)					{	ss << "\tepoch_interval: " << epoch_interval	<< "\n";	}
	if (max_epochs		> 0)					{	ss << "\tmax_epochs:     " << max_epochs		<< "\n";	}
	if (!start_epoch	.is_not_a_date_time())	{	ss << "\tepoch start:    " << start_epoch		<< "\n";	}
	if (!end_epoch		.is_not_a_date_time())	{	ss << "\tepoch end:      " << end_epoch			<< "\n";	}

	ss << "\n";
	ss << "===============================\n";
	ss << "...End Configuration Summary\n";
	ss << "===============================\n";
	ss << "\n";

	BOOST_LOG_TRIVIAL(info) << ss.str();
}

void addAvailableOptions(
	const string& stack)
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
	NodeStack				yamlBase, 						///< Yaml node to search within
	const vector<string>&	yamlNodeDescriptor,				///< List of strings of keys to trace hierarchy
	const string&			comment			= "",			///< Optional comment to append to default values output
	const string&			defaultValue	= "",			///< Optional default value
	const string&			type			= "")			///< Optional type of variable
{
	YAML::Node currentNode;

	auto [node, stack] = yamlBase;
	currentNode.reset(node);

	//this function is fiddly - re-ordering or simplifying will likely lead to default configuration output issues

	for (int i = 0; i < yamlNodeDescriptor.size(); i++)
	{
		auto desc = yamlNodeDescriptor[i];

		if (desc.empty())
		{
			continue;
		}

		boost::algorithm::to_lower(desc);

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
			if (i == yamlNodeDescriptor.size() - 1)		acsConfig.yamlDefaults[stack] = {defaultValue, comment, type};
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
bool tryGetFromYaml(
	TYPE&					output,				///< Variable to output to
	NodeStack				yamlBase,			///< Yaml node to search within
	const vector<string>&	yamlNodeDescriptor,	///< List of strings of keys to trace hierarcy
	const string&			comment = "")		///< Description to provide to user for automatic documentation
{
	auto [optNode, stack] = stringsToYamlObject(yamlBase, yamlNodeDescriptor, comment, stringify(output), typeid(output).name());

	addAvailableOptions(stack);

	string dummy;

	auto& yamlDefault = acsConfig.yamlDefaults[stack];
	try
	{
// 		yamlDefault.foundValue	= yamlDefault.defaultValue;

		output = optNode.template as<TYPE>();

// 		yamlDefault.foundValue	= stringify(output);

		if (stringify(output) == yamlDefault.defaultValue)
		{
			BOOST_LOG_TRIVIAL(debug)
			<< "Yaml entry " << nonNumericStack(stack, dummy) << " is configured with its default value, deleting it entirely would simplify the configuration file.";
		}

		yamlDefault.found		= true;
		return true;
	}
	catch (...)
	{
		if	( (optNode.IsSequence())
			||(optNode.IsScalar()	&& optNode.Scalar().	empty() == false))
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: Yaml entry '" << nonNumericStack(stack, dummy) << "' was found but its value is incorrectly formatted";
		}
	}

	return false;
}

/** Set an output from command line options if found
*/
template<typename TYPE>
bool tryGetFromOpts(
	TYPE&									output,			///< Variable to output to
	boost::program_options::variables_map&	commandOpts,	///< Command line object to search within
	const vector<string>&					nodeDescriptor)	///< List of strings of keys to trace hierarcy
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
bool tryGetFromAny(
	TYPE&									output,				///< Variable to output to
	boost::program_options::variables_map&	commandOpts,		///< Command line object to search within
	NodeStack&								yamlBase,			///< Yaml node to search within
	const vector<string>&					nodeDescriptor,		///< List of strings of keys to trace hierarcy
	const string&							comment = "")		///< Description to provide to user for automatic documentation
{
	bool found = false;
	found |= tryGetFromYaml(output, yamlBase,	nodeDescriptor, comment);
	found |= tryGetFromOpts(output, commandOpts,nodeDescriptor);
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

	string newStack = nonNumericStack(stack, dummy);
	enumDetailsMap[enumName].usingOptions.push_back(newStack);
	acsConfig.yamlDefaults[stack].enumName = enumName;
}

/** Get a list of available enum values as readable string
 */
template <typename ENUM>
string getEnumOpts(
	bool vec = false)
{
	string enumOptions;
	if (vec)	enumOptions = " [";
	else		enumOptions = " {";

	auto names = ENUM::_names();
	for (int i = 0; i < ENUM::_size(); i++)
	{
		string enumOption = boost::algorithm::to_lower_copy((string) names[i]);

		if (i != 0)
			enumOptions += ", ";
		enumOptions += enumOption;
	}

	if (vec)	enumOptions += "]";
	else		enumOptions += "}";

	return enumOptions;
}

template <typename ENUM>
void warnAboutEnum(
	const string& wrong,
	const string& option,
	ENUM	enumValue)
{
	BOOST_LOG_TRIVIAL(error)
	<< "\nError: " << wrong << " is not a valid entry for option: " << option << ".\n"
	<< "Valid options include:";

	for (const char* name : ENUM::_names())
	{
		BOOST_LOG_TRIVIAL(error) << name;
	}
}

/** Set an enum from yaml, decoding strings to ints
*/
template <typename ENUM>
bool tryGetEnumOpt(
	ENUM&					out,									///< Variable to output to
	NodeStack				yamlBase,								///< Yaml node to search within
	const vector<string>&	yamlNodeDescriptor,						///< List of strings of keys to trace hierarcy
	const string&			comment = "")							///< Description to provide to user for automatic documentation
{
	string enumOptions = getEnumOpts<ENUM>();

	auto [optNode, stack] = stringsToYamlObject(yamlBase, yamlNodeDescriptor, comment + enumOptions, out._to_string());

	addAvailableOptions(stack);

	addEnumDetails<ENUM>(stack);

	if	( optNode.IsSequence()
		||optNode.IsMap())
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error: Map or sequence found for scalar option " << yamlNodeDescriptor.back();
	}

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
		out = ENUM::_from_string_nocase(value.c_str());
		return true;
	}
	catch (...)
	{
		warnAboutEnum(value, yamlNodeDescriptor.back(), out);
		return false;
	}
}

template <typename ENUM>
bool tryGetEnumVec(
	vector<ENUM>&			enumVector,						///< Output vector for enum configurations
	NodeStack				yamlBase,						///< Yaml node to search within
	const vector<string>&	yamlNodeDescriptor,				///< List of strings of keys to trace hierarcy
	const string&			comment = "")					///< Description to provide to user for automatic documentation
{
	string enumOptions = getEnumOpts<ENUM>(true);

	auto [optNode, stack] = stringsToYamlObject(yamlBase, yamlNodeDescriptor, comment + enumOptions, stringify(enumVector)); //do this twice to populate the defaults before using strings

	vector<string> enumStrings;
	bool found = tryGetFromYaml(enumStrings, yamlBase, yamlNodeDescriptor, comment + enumOptions);

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
			ENUM enumValue;
			warnAboutEnum(enumString, yamlNodeDescriptor.back(), enumValue);
			continue;
		}
	}

	return true;
}

/** Use pointer arithmetic to keep track of variables that have been initialised
 */
template<typename BASE, typename COMP>
void setInited(
	BASE&	base,
	COMP&	comp,
	bool	init = true)
{
	if (init == false)
	{
		return;
	}

	int offset = (char*)(&comp) - (char*)(&base);

	base.initialisedMap[offset] = true;
}

/** Set the variables associated with kalman filter states from yaml
*/
void tryGetKalmanFromYaml(
	KalmanModel&	output,					///< Variable to output to
	NodeStack&		yaml,					///< Yaml node to search within
	const string&	key,					///< Key of yaml object
	const string&	comment		= "",		///< Description to provide to user for automatic documentation
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

	{
	}{auto& thing = output.estimate		 	;	setInited(output,	thing,	tryGetFromYaml(thing, newYaml, {"0! estimated"			}, "Estimate state in kalman filter"));
	}{auto& thing = output.use_remote_sigma	;	setInited(output,	thing,	tryGetFromYaml(thing, newYaml, {"4@ use_remote_sigma"	}, "Use remote filter sigma for initial sigma"));

	}{auto& thing = output.sigma			;	setInited(output,	thing,	tryGetFromYaml(thing, newYaml, {"1! sigma" 				}, "Apriori sigma values - if zero, will be initialised using least squares"));
	}{auto& thing = output.apriori_value	;	setInited(output,	thing,	tryGetFromYaml(thing, newYaml, {"3! apriori_value"		}, "Apriori state values"));
	}{auto& thing = output.process_noise	;	setInited(output,	thing,	tryGetFromYaml(thing, newYaml, {"2! process_noise" 		}, "Process noise sigmas"));
	}{auto& thing = output.tau				;	setInited(output,	thing,	tryGetFromYaml(thing, newYaml, {"@ tau"					}, "Correlation times for gauss markov noise, defaults to -1 -> inf (Random Walk)"));
	}{auto& thing = output.mu				;	setInited(output,	thing,	tryGetFromYaml(thing, newYaml, {"@ mu"					}, "Desired mean value for gauss markov states"));
	}{auto& thing = output.comment			;	setInited(output,	thing,	tryGetFromYaml(thing, newYaml, {"@ comment"				}, "Comment to apply to the state"));

	}{auto& thing = proc_noise_dt			;								tryGetEnumOpt( thing, newYaml, {"2@ process_noise_dt"	}, "Time unit for process noise");
	}

	if (isInited(output, output.process_noise))
	{
		for (auto& proc : output.process_noise)
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
	const string&							key,
	const string&							prefix,
	const string&							comment = "")
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
	found |= tryGetFromOpts(optsList, commandOpts, {key});

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
	const string&	id)							///< Label associated with the stream
{
	auto outStreamsYaml = stringsToYamlObject(yaml, {id});

	tryGetFromYaml(outStreamData.url, outStreamsYaml, {"0@ url"}, "Url of caster to send messages to");

	for (auto msgType : RtcmMessageType::_values())
	{
		if (msgType == +RtcmMessageType::IGS_SSR)
		for (auto subType : IgsSSRSubtype::_values())
		{
			string str = (boost::format("@ rtcm_%4d_%03d") % msgType._to_integral() % subType._to_integral()).str();

			auto msgOptions = stringsToYamlObject(outStreamsYaml, {"0@ messages", str},	"Message type to output");

			bool found = tryGetFromYaml(outStreamData.rtcmMsgOptsMap[msgType].igs_udi[subType],		msgOptions, {"0@ udi"},	"Update interval");
			if (found)
				outStreamData.rtcmMsgOptsMap[msgType].udi = 1;
		}

		else if (msgType == +RtcmMessageType::COMPACT_SSR)
		for (auto subType : CompactSSRSubtype::_values())
		{
			string str = (boost::format("@ rtcm_%4d_%02d") % msgType._to_integral() % subType._to_integral()).str();

			auto msgOptions = stringsToYamlObject(outStreamsYaml, {"0@ messages", str},	"Message type to output");

			bool found = tryGetFromYaml(outStreamData.rtcmMsgOptsMap[msgType].comp_udi[subType],	msgOptions, {"0@ udi"},	"Update interval");
			if (found)
				outStreamData.rtcmMsgOptsMap[msgType].udi = 1;
		}

		else
		{
			string str = "@ rtcm_" + std::to_string(msgType);

			auto msgOptions = stringsToYamlObject(outStreamsYaml, {"0@ messages", str},	"Message type to output");

			tryGetFromYaml(outStreamData.rtcmMsgOptsMap[msgType].udi, msgOptions, {"0@ udi"},	"Update interval");
		}
	}

	tryGetFromYaml(outStreamData.itrf_datum, 		outStreamsYaml, {"itrf_datum"			});
	tryGetFromYaml(outStreamData.provider_id, 		outStreamsYaml, {"provider_id"			});
	tryGetFromYaml(outStreamData.solution_id, 		outStreamsYaml, {"solution_id"			});
}


const string estimation_parameters_str	= "4! estimation_parameters";
const string processing_options_str		= "2! processing_options";


/** Copy one parameter to another, if it has been initialised.
 *
 * Use pointer arithmetic to determine the offset of another parameter within its parent structure, assuming it has the same layout as this parameter in its parent.
 */
template<
	typename CONTAINER,
	typename ELEMENT>
bool initIfNeeded(
			CONTAINER&	thisContainer,
	const	CONTAINER&	thatContainer,
			ELEMENT&	thisElement)
{
			CONTAINER*	thisContainer_ptr	= &thisContainer;
	const	CONTAINER*	thatContainer_ptr	= &thatContainer;
			ELEMENT*	thisElement_ptr		= &thisElement;
			ELEMENT*	thatElement_ptr		= (ELEMENT*)(((char*)thisElement_ptr) + ((char*)thatContainer_ptr - (char*)thisContainer_ptr));

	auto&		thatElement			= *thatElement_ptr;

	if (isInited(thatContainer, thatElement))
	{
		thisElement = thatElement;

		setInited(thisContainer, thisElement);

		return true;
	}

	return false;
}







CommonOptions& CommonOptions::operator+=(
	const CommonOptions& rhs)
{
	initIfNeeded(*this, rhs, exclude							);
	initIfNeeded(*this, rhs, pseudo_sigma						);
	initIfNeeded(*this, rhs, laser_sigma						);

	initIfNeeded(*this, rhs, clock_codes						);
	initIfNeeded(*this, rhs, apriori_sigma_enu					);
	initIfNeeded(*this, rhs, mincon_scale_apriori_sigma			);
	initIfNeeded(*this, rhs, mincon_scale_filter_sigma			);

	initIfNeeded(*this, rhs, antenna_boresight					);
	initIfNeeded(*this, rhs, antenna_azimuth					);

	initIfNeeded(*this, rhs, ellipse_propagation_time_tolerance	);

	initIfNeeded(*this, rhs, posModel.enable					);
	initIfNeeded(*this, rhs, posModel.sources					);
	initIfNeeded(*this, rhs, clockModel.enable					);
	initIfNeeded(*this, rhs, clockModel.sources					);

	initIfNeeded(*this, rhs, attitudeModel.enable				);
	initIfNeeded(*this, rhs, attitudeModel.sources				);
	initIfNeeded(*this, rhs, attitudeModel.model_dt				);

	initIfNeeded(*this, rhs, codeBiasModel.enable				);
	initIfNeeded(*this, rhs, codeBiasModel.default_bias			);
	initIfNeeded(*this, rhs, codeBiasModel.undefined_sigma		);
	initIfNeeded(*this, rhs, phaseBiasModel.enable				);
	initIfNeeded(*this, rhs, phaseBiasModel.default_bias		);
	initIfNeeded(*this, rhs, phaseBiasModel.undefined_sigma		);

	initIfNeeded(*this, rhs, pcoModel.enable					);
	initIfNeeded(*this, rhs, pcvModel.enable					);
	initIfNeeded(*this, rhs, phaseWindupModel.enable			);

	return *this;
}


OrbitOptions& OrbitOptions::operator+=(
	const OrbitOptions& rhs)
{
	initIfNeeded(*this, rhs, mass							);
	initIfNeeded(*this, rhs, area							);
	initIfNeeded(*this, rhs, power							);
	initIfNeeded(*this, rhs, srp_cr							);

	initIfNeeded(*this, rhs, planetary_perturbations		);
	initIfNeeded(*this, rhs, empirical						);
	initIfNeeded(*this, rhs, antenna_thrust					);
	initIfNeeded(*this, rhs, albedo							);
	initIfNeeded(*this, rhs, solar_radiation_pressure		);

	initIfNeeded(*this, rhs, empirical_dyb_eclipse			);
	initIfNeeded(*this, rhs, empirical_rtn_eclipse			);

	initIfNeeded(*this, rhs, surface_details				);

	initIfNeeded(*this, rhs, pseudoPulses.enable			);
	initIfNeeded(*this, rhs, pseudoPulses.interval			);
	initIfNeeded(*this, rhs, pseudoPulses.pos_proc_noise	);
	initIfNeeded(*this, rhs, pseudoPulses.vel_proc_noise	);

	return *this;
}


KalmanModel& KalmanModel::operator+=(
	const KalmanModel& rhs)
{
	initIfNeeded(*this, rhs,	sigma			);
	initIfNeeded(*this, rhs,	apriori_value	);
	initIfNeeded(*this, rhs,	process_noise	);
	initIfNeeded(*this, rhs,	tau				);
	initIfNeeded(*this, rhs,	mu				);
	initIfNeeded(*this, rhs,	use_remote_sigma);
	initIfNeeded(*this, rhs,	estimate		);
	initIfNeeded(*this, rhs,	comment			);

	return *this;
}


SatelliteOptions& SatelliteOptions::operator+=(
	const SatelliteOptions& rhs)
{
	SatelliteKalmans	::operator+=(rhs);
	CommonOptions		::operator+=(rhs);
	OrbitOptions		::operator+=(rhs);

	initIfNeeded(*this, rhs,	error_model						);
	initIfNeeded(*this, rhs,	code_sigma						);
	initIfNeeded(*this, rhs,	phase_sigma						);

	inheritedFrom[rhs.id] = (SatelliteOptions*) &rhs;

	return *this;
}


ReceiverOptions& ReceiverOptions::operator+=(
	const ReceiverOptions& rhs)
{
	ReceiverKalmans		::operator+=(rhs);
	CommonOptions		::operator+=(rhs);

	rinex23Conv		+= rhs.rinex23Conv;

	initIfNeeded(*this, rhs,	kill							);
	initIfNeeded(*this, rhs,	zero_dcb_codes					);
	initIfNeeded(*this, rhs,	apriori_pos						);
	initIfNeeded(*this, rhs,	antenna_type					);
	initIfNeeded(*this, rhs,	receiver_type					);
	initIfNeeded(*this, rhs,	sat_id							);
	initIfNeeded(*this, rhs,	elevation_mask_deg				);
	initIfNeeded(*this, rhs,	receiver_reference_system		);

	initIfNeeded(*this, rhs,	eccentricityModel.enable		);
	initIfNeeded(*this, rhs,	eccentricityModel.eccentricity	);

	initIfNeeded(*this, rhs,	tropModel.enable				);
	initIfNeeded(*this, rhs,	tropModel.models				);

	initIfNeeded(*this, rhs,	tideModels.enable				);
	initIfNeeded(*this, rhs,	tideModels.solid				);
	initIfNeeded(*this, rhs,	tideModels.otl					);
	initIfNeeded(*this, rhs,	tideModels.atl					);
	initIfNeeded(*this, rhs,	tideModels.spole				);
	initIfNeeded(*this, rhs,	tideModels.opole				);

	initIfNeeded(*this, rhs,	range							);
	initIfNeeded(*this, rhs,	relativity						);
	initIfNeeded(*this, rhs,	relativity2						);
	initIfNeeded(*this, rhs,	sagnac							);
	initIfNeeded(*this, rhs,	integer_ambiguity				);
	initIfNeeded(*this, rhs,	ionospheric_component			);
	initIfNeeded(*this, rhs,	ionospheric_component2			);
	initIfNeeded(*this, rhs,	ionospheric_component3			);
	initIfNeeded(*this, rhs,	ionospheric_model				);
	initIfNeeded(*this, rhs,	tropospheric_map				);
	initIfNeeded(*this, rhs,	eop								);

	initIfNeeded(*this, rhs,	mapping_function				);
	initIfNeeded(*this, rhs,	geomagnetic_field_height		);
	initIfNeeded(*this, rhs,	mapping_function_layer_height	);
	initIfNeeded(*this, rhs,	iono_sigma_limit				);

	initIfNeeded(*this, rhs,	error_model						);
	initIfNeeded(*this, rhs,	code_sigma						);
	initIfNeeded(*this, rhs,	phase_sigma						);

	inheritedFrom[rhs.id] = (ReceiverOptions*) &rhs;

	return *this;
}

/** Set inertial force options from yaml
*/
void tryGetKalmanFromYaml(
	InertialKalmans&		inertialOpts,		///< Inertail options variable to output to
	NodeStack				yamlBase,			///< Yaml node to search within
	const vector<string>&	descriptorVec)	///< List of strings of keys of yaml hierarchy
{
	auto inrNode = stringsToYamlObject(yamlBase, descriptorVec);

	tryGetKalmanFromYaml(inertialOpts.orientation,			inrNode, "7@ orientation");
	tryGetKalmanFromYaml(inertialOpts.gyro_bias,			inrNode, "7@ gyro_bias");
	tryGetKalmanFromYaml(inertialOpts.accelerometer_bias,	inrNode, "7@ accelerometer_bias");
	tryGetKalmanFromYaml(inertialOpts.gyro_scale,			inrNode, "7@ gyro_scale");
	tryGetKalmanFromYaml(inertialOpts.accelerometer_scale,	inrNode, "7@ accelerometer_scale");
	tryGetKalmanFromYaml(inertialOpts.imu_offset,			inrNode, "7@ imu_offset");
}

/** Set empirical force options from yaml
*/
void tryGetKalmanFromYaml(
	EmpKalmans&				empOpts,			///< Empirical options variable to output to
	NodeStack				yamlBase,			///< Yaml node to search within
	const vector<string>&	descriptorVec)	///< List of strings of keys of yaml hierarchy
{
	auto empNode = stringsToYamlObject(yamlBase, descriptorVec);

	tryGetKalmanFromYaml(empOpts.emp_d_0,					empNode, "6@ emp_d_0",				"Empirical accleration direct bias ");
	tryGetKalmanFromYaml(empOpts.emp_d_1,					empNode, "6@ emp_d_1",				"Empirical accleration direct 1 per rev");
	tryGetKalmanFromYaml(empOpts.emp_d_2,					empNode, "6@ emp_d_2",				"Empirical accleration direct 2 per rev");
	tryGetKalmanFromYaml(empOpts.emp_d_3,					empNode, "6@ emp_d_3",				"Empirical accleration direct 3 per rev");
	tryGetKalmanFromYaml(empOpts.emp_d_4,					empNode, "6@ emp_d_4",				"Empirical accleration direct 4 per rev");

	tryGetKalmanFromYaml(empOpts.emp_y_0,					empNode, "6@ emp_y_0",				"Empirical accleration Y bias ");
	tryGetKalmanFromYaml(empOpts.emp_y_1,					empNode, "6@ emp_y_1",				"Empirical accleration Y 1 per rev");
	tryGetKalmanFromYaml(empOpts.emp_y_2,					empNode, "6@ emp_y_2",				"Empirical accleration Y 2 per rev");
	tryGetKalmanFromYaml(empOpts.emp_y_3,					empNode, "6@ emp_y_3",				"Empirical accleration Y 3 per rev");
	tryGetKalmanFromYaml(empOpts.emp_y_4,					empNode, "6@ emp_y_4",				"Empirical accleration Y 4 per rev");

	tryGetKalmanFromYaml(empOpts.emp_b_0,					empNode, "6@ emp_b_0",				"Empirical accleration B bias ");
	tryGetKalmanFromYaml(empOpts.emp_b_1,					empNode, "6@ emp_b_1",				"Empirical accleration B 1 per rev");
	tryGetKalmanFromYaml(empOpts.emp_b_2,					empNode, "6@ emp_b_2",				"Empirical accleration B 2 per rev");
	tryGetKalmanFromYaml(empOpts.emp_b_3,					empNode, "6@ emp_b_3",				"Empirical accleration B 3 per rev");
	tryGetKalmanFromYaml(empOpts.emp_b_4,					empNode, "6@ emp_b_4",				"Empirical accleration B 4 per rev");

	tryGetKalmanFromYaml(empOpts.emp_r_0,					empNode, "6@ emp_r_0",				"Empirical accleration radial bias ");
	tryGetKalmanFromYaml(empOpts.emp_r_1,					empNode, "6@ emp_r_1",				"Empirical accleration radial 1 per rev");
	tryGetKalmanFromYaml(empOpts.emp_r_2,					empNode, "6@ emp_r_2",				"Empirical accleration radial 2 per rev");
	tryGetKalmanFromYaml(empOpts.emp_r_3,					empNode, "6@ emp_r_3",				"Empirical accleration radial 3 per rev");
	tryGetKalmanFromYaml(empOpts.emp_r_4,					empNode, "6@ emp_r_4",				"Empirical accleration radial 4 per rev");

	tryGetKalmanFromYaml(empOpts.emp_t_0,					empNode, "6@ emp_t_0",				"Empirical accleration tangential bias ");
	tryGetKalmanFromYaml(empOpts.emp_t_1,					empNode, "6@ emp_t_1",				"Empirical accleration tangential 1 per rev");
	tryGetKalmanFromYaml(empOpts.emp_t_2,					empNode, "6@ emp_t_2",				"Empirical accleration tangential 2 per rev");
	tryGetKalmanFromYaml(empOpts.emp_t_3,					empNode, "6@ emp_t_3",				"Empirical accleration tangential 3 per rev");
	tryGetKalmanFromYaml(empOpts.emp_t_4,					empNode, "6@ emp_t_4",				"Empirical accleration tangential 4 per rev");

	tryGetKalmanFromYaml(empOpts.emp_n_0,					empNode, "6@ emp_n_0",				"Empirical accleration normal bias ");
	tryGetKalmanFromYaml(empOpts.emp_n_1,					empNode, "6@ emp_n_1",				"Empirical accleration normal 1 per rev");
	tryGetKalmanFromYaml(empOpts.emp_n_2,					empNode, "6@ emp_n_2",				"Empirical accleration normal 2 per rev");
	tryGetKalmanFromYaml(empOpts.emp_n_3,					empNode, "6@ emp_n_3",				"Empirical accleration normal 3 per rev");
	tryGetKalmanFromYaml(empOpts.emp_n_4,					empNode, "6@ emp_n_4",				"Empirical accleration normal 4 per rev");

	tryGetKalmanFromYaml(empOpts.emp_p_0,					empNode, "6@ emp_p_0",				"Empirical accleration P bias ");
	tryGetKalmanFromYaml(empOpts.emp_p_1,					empNode, "6@ emp_p_1",				"Empirical accleration P 1 per rev");
	tryGetKalmanFromYaml(empOpts.emp_p_2,					empNode, "6@ emp_p_2",				"Empirical accleration P 2 per rev");
	tryGetKalmanFromYaml(empOpts.emp_p_3,					empNode, "6@ emp_p_3",				"Empirical accleration P 3 per rev");
	tryGetKalmanFromYaml(empOpts.emp_p_4,					empNode, "6@ emp_p_4",				"Empirical accleration P 4 per rev");

	tryGetKalmanFromYaml(empOpts.emp_q_0,					empNode, "6@ emp_q_0",				"Empirical accleration Q bias ");
	tryGetKalmanFromYaml(empOpts.emp_q_1,					empNode, "6@ emp_q_1",				"Empirical accleration Q 1 per rev");
	tryGetKalmanFromYaml(empOpts.emp_q_2,					empNode, "6@ emp_q_2",				"Empirical accleration Q 2 per rev");
	tryGetKalmanFromYaml(empOpts.emp_q_3,					empNode, "6@ emp_q_3",				"Empirical accleration Q 3 per rev");
	tryGetKalmanFromYaml(empOpts.emp_q_4,					empNode, "6@ emp_q_4",				"Empirical accleration Q 4 per rev");
}

/** Set common options from yaml
*/
void tryGetKalmanFromYaml(
	CommonKalmans&			comOpts, 			///< Receiver options variable to output to
	NodeStack				yamlBase,			///< Yaml node to search within
	const vector<string>&	descriptorVec)	///< List of strings of keys of yaml hierarchy
{
	auto comNode = stringsToYamlObject(yamlBase, descriptorVec);

	tryGetKalmanFromYaml(comOpts.clk,					comNode, "1! clock",				"Clocks");
	tryGetKalmanFromYaml(comOpts.clk_rate,				comNode, "1@ clock_rate",			"Clock rates");
	tryGetKalmanFromYaml(comOpts.pos,					comNode, "1! pos",					"Position");
	tryGetKalmanFromYaml(comOpts.pos_rate,				comNode, "1! pos_rate",				"Velocity");
	tryGetKalmanFromYaml(comOpts.orbit,					comNode, "2@ orbit",				"Orbital state");
	tryGetKalmanFromYaml(comOpts.pco,					comNode, "3# pco",					"Phase Center Offsets (experimental)");
	tryGetKalmanFromYaml(comOpts.code_bias,				comNode, "4! code_bias",			"Code bias");
	tryGetKalmanFromYaml(comOpts.phase_bias,			comNode, "4! phase_bias",			"Phase bias");
	tryGetKalmanFromYaml(comOpts.ant_delta,				comNode, "4! ant_delta",			"Antenna delta (body frame)");
}






/** Set satellite options from yaml
*/
void getKalmanFromYaml(
	SatelliteKalmans&		satOpts,		///< Satellite options variable to output to
	NodeStack				yamlBase,		///< Yaml node to search within
	const vector<string>&	descriptorVec)	///< List of strings of keys of yaml hierarchy
{
	auto satNode = stringsToYamlObject(yamlBase, descriptorVec);

	tryGetKalmanFromYaml((InertialKalmans&)	satOpts,	satNode, {});
	tryGetKalmanFromYaml((CommonKalmans&)	satOpts,	satNode, {});
	tryGetKalmanFromYaml((EmpKalmans&)		satOpts,	satNode, {});
}

/** Set receiver options from yaml
*/
void getKalmanFromYaml(
	ReceiverKalmans&		recOpts, 		///< Receiver options variable to output to
	NodeStack				yamlBase,		///< Yaml node to search within
	const vector<string>&	descriptorVec)	///< List of strings of keys of yaml hierarchy
{
	auto recNode = stringsToYamlObject(yamlBase, descriptorVec);

	tryGetKalmanFromYaml((InertialKalmans&)	recOpts,	recNode, {});
	tryGetKalmanFromYaml((CommonKalmans&)	recOpts,	recNode, {});
	tryGetKalmanFromYaml((EmpKalmans&)		recOpts,	recNode, {});

	tryGetKalmanFromYaml(recOpts.strain_rate,			recNode, "8@ strain_rate",			"Velocity (large gain, for geodetic timescales)");
	tryGetKalmanFromYaml(recOpts.ambiguity,				recNode, "1! ambiguities",			"Integer phase ambiguities");
	tryGetKalmanFromYaml(recOpts.pcv,					recNode, "3# pcv",					"Antenna phase center variations (experimental)");
	tryGetKalmanFromYaml(recOpts.ion_stec,				recNode, "1! ion_stec",				"Ionospheric slant delay");
	tryGetKalmanFromYaml(recOpts.ion_model,				recNode, "3@ ion_model",			"Ionospheric mapping");
	tryGetKalmanFromYaml(recOpts.slr_range_bias,		recNode, "9@ slr_range_bias",		"Satellite Laser Ranging range bias");
	tryGetKalmanFromYaml(recOpts.slr_time_bias,			recNode, "9@ slr_time_bias",		"Satellite Laser Ranging time bias");
	tryGetKalmanFromYaml(recOpts.trop,					recNode, "1! trop",					"Troposphere corrections");
	tryGetKalmanFromYaml(recOpts.trop_grads,			recNode, "1! trop_grads",			"Troposphere gradients");
	tryGetKalmanFromYaml(recOpts.trop_maps,				recNode, "1@ trop_maps",			"Troposphere ZWD mapping");
}










/** Set common options from yaml
*/
void getOptionsFromYaml(
	OrbitOptions&			orbOpts,		///< Satellite options variable to output to
	NodeStack				yamlBase,		///< Yaml node to search within
	const vector<string>&	descriptorVec)	///< List of strings of keys of yaml hierarchy
{
	auto comNode = stringsToYamlObject(yamlBase, descriptorVec);

	auto& [node, stack] = comNode;

	auto orbitsNode		= stringsToYamlObject(comNode,		{"@ orbit_propagation"	}, "Enable specific orbit propagation models");
	auto pseudo_pulses	= stringsToYamlObject(orbitsNode,	{"@ pseudo_pulses"		}, "Apply process noise to simulate pseudo-stochastic pulses commonly applied in least squares solutions");

	{
	}{	auto& thing = orbOpts.mass							;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing,	orbitsNode,	{"0! mass"						}, "Satellite mass for use if not specified in the SINEX metadata file"));
	}{	auto& thing = orbOpts.area							;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing,	orbitsNode,	{"0! area"						}, "Satellite area for use in solar radiation and albedo calculations"));
	}{	auto& thing = orbOpts.power							;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing,	orbitsNode,	{"0@ power"						}, "Transmission power use if not specified in the SINEX metadata file"));
	}{	auto& thing = orbOpts.srp_cr						;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing,	orbitsNode,	{"0@ srp_cr"					}, "Coefficient of reflection of the satellite"));


	}{	auto& thing = orbOpts.planetary_perturbations		;	setInited(orbOpts,	thing,	tryGetEnumVec	(thing, orbitsNode,	{"@ planetary_perturbations"	}, "Acceleration due to third celestial bodies"));
	}{	auto& thing = orbOpts.solar_radiation_pressure		;	setInited(orbOpts,	thing,	tryGetEnumOpt	(thing, orbitsNode,	{"@ solar_radiation_pressure"	}, "Model accelerations due to solar radiation pressure"));
	}{	auto& thing = orbOpts.empirical						;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing, orbitsNode,	{"@ empirical"					}, "Model accelerations due to empirical accelerations"));
	}{	auto& thing = orbOpts.antenna_thrust				;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing, orbitsNode,	{"@ antenna_thrust"				}, "Model accelerations due to the emitted signal from the antenna"));
	}{	auto& thing = orbOpts.albedo						;	setInited(orbOpts,	thing,	tryGetEnumOpt	(thing, orbitsNode,	{"@ albedo"						}, "Model accelerations due to the albedo effect from Earth (Visible and Infra-red)"));

	}{	auto& thing = orbOpts.empirical_dyb_eclipse			;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing, orbitsNode, {"@ empirical_dyb_eclipse"		}, "Turn on/off the eclipse on each axis (D, Y, B)"));
	}{	auto& thing = orbOpts.empirical_rtn_eclipse			;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing, orbitsNode, {"@ empirical_rtn_eclipse"		}, "Turn on/off the eclipse on each axis (R, T, N)"));

	}{	auto& thing = orbOpts.pseudoPulses.enable			;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing, pseudo_pulses,	{"@ enable"					}, "Enable applying process noise impulses to orbits upon state errors"));
	}{	auto& thing = orbOpts.pseudoPulses.interval			;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing, pseudo_pulses,	{"@ interval"				}, "Interval between applying pseudo pulses"));
	}{	auto& thing = orbOpts.pseudoPulses.pos_proc_noise	;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing, pseudo_pulses,	{"@ pos_process_noise"		}, "Sigma to add to orbital position states"));
	}{	auto& thing = orbOpts.pseudoPulses.vel_proc_noise	;	setInited(orbOpts,	thing,	tryGetFromYaml	(thing, pseudo_pulses,	{"@ vel_process_noise"		}, "Sigma to add to orbital velocity states"));
	}


	bool surfaceFound = false;
	vector<SurfaceDetails> surface_details;

	auto [surfacesNode, surfacesString] = stringsToYamlObject(comNode, {"6@ surface_details"},		"List of details for srp and drag surfaces");

	for (auto surfacesYaml : surfacesNode)
	{
		SurfaceDetails surface;

		tryGetFromYaml(surface.rotation_axis,			{surfacesYaml, ""}, {"rotation_axis"		});
		tryGetFromYaml(surface.normal,					{surfacesYaml, ""}, {"normal"				});
		tryGetFromYaml(surface.shape,					{surfacesYaml, ""}, {"shape"				});
		tryGetFromYaml(surface.area,					{surfacesYaml, ""}, {"area"					});
		tryGetFromYaml(surface.reflection_visible,		{surfacesYaml, ""}, {"reflection_visible"	});
		tryGetFromYaml(surface.diffusion_visible,		{surfacesYaml, ""}, {"diffusion_visible"	});
		tryGetFromYaml(surface.absorption_visible,		{surfacesYaml, ""}, {"absorption_visible"	});
		tryGetFromYaml(surface.thermal_reemission,		{surfacesYaml, ""}, {"thermal_reemission"	});
		tryGetFromYaml(surface.reflection_infrared,		{surfacesYaml, ""}, {"reflection_infrared"	});
		tryGetFromYaml(surface.diffusion_infrared,		{surfacesYaml, ""}, {"diffusion_infrared"	});
		tryGetFromYaml(surface.absorption_infrared,		{surfacesYaml, ""}, {"absorption_infrared"	});

		if	( surface.rotation_axis.empty() == false
			&&surface.rotation_axis.size() != 3)
		{

			BOOST_LOG_TRIVIAL(warning) << "Warning: rotation_axis is not a vector of size 3 for surface " << stack;
			continue;
		}

		if	( surface.normal.empty() == false
			&&surface.normal.size() != 3)
		{
			BOOST_LOG_TRIVIAL(warning) << "Error: boxwing surface.normal is not a vector of size 3 for surface " << stack;
			continue;
		}

		surface_details.push_back(surface);
		surfaceFound = true;
	}

	if (surfaceFound)
	{
		orbOpts.surface_details = surface_details;
		setInited(orbOpts, orbOpts.surface_details);
	}
}



/** Set common options from yaml
*/
void getOptionsFromYaml(
	CommonOptions&			comOpts,			///< Satellite options variable to output to
	NodeStack				yamlBase,			///< Yaml node to search within
	const vector<string>&	descriptorVec)	///< List of strings of keys of yaml hierarchy
{
	auto comNode = stringsToYamlObject(yamlBase, descriptorVec);

	auto modelsNode = stringsToYamlObject(comNode,		{"9@ models"					}, "Enable specific models");

	vector<double>	antenna_boresight;
	vector<double>	antenna_azimuth;
	{
	}{	auto& thing = comOpts.exclude							;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	comNode,	{"0! exclude"							}, "Exclude receiver from processing"));
	}{	auto& thing = comOpts.pseudo_sigma						;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	comNode,	{"0@ pseudo_sigma"						}, "Standard deviation of pseudo measurmeents"));
	}{	auto& thing = comOpts.laser_sigma						;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	comNode,	{"0@ laser_sigma"						}, "Standard deviation of SLR laser measurements"));
	}{	auto& thing = comOpts.clock_codes						;	setInited(comOpts,	thing,	tryGetEnumVec	(thing,	comNode,	{"3@ clock_codes"						}, "Codes for IF combination based clocks"));
	}{	auto& thing = comOpts.apriori_sigma_enu					;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	comNode,	{"4@ apriori_sigma_enu"					}, "Sigma applied for weighting in mincon transformation estimation. (Lower is stronger weighting, Negative is unweighted, ENU separation unsupported for satellites)"));
	}{	auto& thing = comOpts.mincon_scale_apriori_sigma		;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	comNode,	{"4@ mincon_scale_apriori_sigma"		}, "Scale applied to apriori sigmas while weighting in mincon transformation estimation"));
	}{	auto& thing = comOpts.mincon_scale_filter_sigma			;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	comNode,	{"4@ mincon_scale_filter_sigma"			}, "Scale applied to filter sigmas while weighting in mincon transformation estimation"));

	}{	auto& thing = antenna_boresight							;								tryGetFromYaml	(thing,	comNode,	{"@ antenna_boresight"					}, "Antenna boresight (Up) in satellite body-fixed frame");
	}{	auto& thing = antenna_azimuth							;								tryGetFromYaml	(thing,	comNode,	{"@ antenna_azimuth"					}, "Antenna azimuth (North) in satellite body-fixed frame");

	}{	auto& thing = comOpts.ellipse_propagation_time_tolerance;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	comNode,	{"@ ellipse_propagation_time_tolerance"	}, "Time gap tolerance under which the ellipse propagator can be used for orbit prediction"));

	}{	auto& thing = comOpts.posModel.enable					;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ pos",			"@ enable"			}, "Enable modelling of position"));
	}{	auto& thing = comOpts.posModel.sources					;	setInited(comOpts,	thing,	tryGetEnumVec	(thing,	modelsNode,	{"@ pos",			"@ sources"			}, "Enable modelling of position"));

	}{	auto& thing = comOpts.clockModel.enable					;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ clock",			"@ enable"			}, "Enable modelling of clocks"));
	}{	auto& thing = comOpts.clockModel.sources				; 	setInited(comOpts,	thing,	tryGetEnumVec	(thing,	modelsNode,	{"@ clock",			"@ sources"			}, "List of sources to use for clocks"));
	}{	auto& thing = comOpts.attitudeModel.enable				;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ attitude",		"@ enable"			}, "Enables non-nominal attitude types"));
	}{	auto& thing = comOpts.attitudeModel.sources				; 	setInited(comOpts,	thing,	tryGetEnumVec	(thing,	modelsNode,	{"@ attitude",		"@ sources"			}, "List of sourecs to use for attitudes"));
	}{	auto& thing = comOpts.attitudeModel.model_dt			;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ attitude",		"@ model_dt"		}, "Timestep used in modelling attitude"));
	}{	auto& thing = comOpts.codeBiasModel.enable				;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ code_bias",		"@ enable"			}, "Enable modelling of code biases"));
	}{	auto& thing = comOpts.codeBiasModel.default_bias		;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ code_bias",		"@ default_bias"	}, "Bias to use when no code bias is found"));
	}{	auto& thing = comOpts.codeBiasModel.undefined_sigma		;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ code_bias",		"@ undefined_sigma"	}, "Uncertainty sigma to apply to default code biases"));
	}{	auto& thing = comOpts.phaseBiasModel.enable				;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ phase_bias",	"@ enable"			}, "Enable modelling of phase biases. Required for AR"));
	}{	auto& thing = comOpts.phaseBiasModel.default_bias		;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ phase_bias",	"@ default_bias"	}, "Bias to use when no phase bias is found"));
	}{	auto& thing = comOpts.phaseBiasModel.undefined_sigma	;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ phase_bias",	"@ undefined_sigma"	}, "Uncertainty sigma to apply to default phase biases"));

	}{	auto& thing = comOpts.pcoModel.enable					;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ pco",			"@ enable"			}, "Enable modelling of phase center offsets"));
	}{	auto& thing = comOpts.pcvModel.enable					;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ pcv",			"@ enable"			}, "Enable modelling of phase center variations"));

	}{	auto& thing = comOpts.phaseWindupModel.enable			;	setInited(comOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ phase_windup",	"@ enable"			}, "Model phase windup due to relative rotation of circularly polarised antennas"));
	}

	if (antenna_boresight	.size()	== 3)	{	comOpts.antenna_boresight	= Vector3d(antenna_boresight.data());		setInited(comOpts,	comOpts.antenna_boresight);	}
	if (antenna_azimuth		.size()	== 3)	{	comOpts.antenna_azimuth		= Vector3d(antenna_azimuth	.data());		setInited(comOpts,	comOpts.antenna_azimuth);	}
}


/** Set satellite options from yaml
*/
void getOptionsFromYaml(
	SatelliteOptions&		satOpts,			///< Satellite options variable to output to
	NodeStack				yamlBase,			///< Yaml node to search within
	const vector<string>&	descriptorVec)	///< List of strings of keys of yaml hierarchy
{
	auto satNode = stringsToYamlObject(yamlBase, descriptorVec);

	getOptionsFromYaml((CommonOptions&)		satOpts, satNode, {});
	getOptionsFromYaml((OrbitOptions&)		satOpts, satNode, {});

	{
	}{	auto& thing = satOpts.error_model					;	setInited(satOpts,	thing,	tryGetEnumOpt	(thing,	satNode,	{"1@ error_model"						}));
	}{	auto& thing = satOpts.code_sigma					;	setInited(satOpts,	thing,	tryGetFromYaml	(thing,	satNode,	{"2@ code_sigma"						}, "Standard deviation of code measurements"));
	}{	auto& thing = satOpts.phase_sigma					;	setInited(satOpts,	thing,	tryGetFromYaml	(thing,	satNode,	{"2@ phase_sigma"						}, "Standard deviation of phase measurmeents"));
	}
}

/** Set receiver options from yaml
*/
void getOptionsFromYaml(
	ReceiverOptions&	recOpts, 			///< Receiver options variable to output to
	NodeStack			yamlBase,			///< Yaml node to search within
	vector<string>&		descriptorVec)	///< List of strings of keys of yaml hierarchy
{
	auto recNode = stringsToYamlObject(yamlBase, descriptorVec);

	getOptionsFromYaml((CommonOptions&)		recOpts, recNode, {});

	vector<double>	eccentricity;
	vector<double>	apriori_pos;

	auto modelsNode					= stringsToYamlObject(recNode,		{"9@ models"					}, "Enable specific models");

	//get option classes just to add comments
	{
		auto ionospheric_component	= stringsToYamlObject(modelsNode,	{"@ ionospheric_components"	}, "Ionospheric models produce frequency-dependent effects");
		auto ionospheric_model		= stringsToYamlObject(modelsNode,	{"@ ionospheric_model"		}, "Coherent ionosphere models can improve estimation of biases and allow use with single frequency receivers");
		auto troposhpere			= stringsToYamlObject(modelsNode,	{"@ troposphere"			}, "Tropospheric modelling accounts for delays due to refraction of light in water vapour");
		auto tides					= stringsToYamlObject(modelsNode,	{"@ tides"					});
		auto eop					= stringsToYamlObject(modelsNode,	{"@ eop"					});
	}

	{
	}{	auto& thing = recOpts.kill							;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	recNode,	{"0@ kill"										}, "Remove receiver from future processing"));
	}{	auto& thing = recOpts.zero_dcb_codes				;	setInited(recOpts,	thing,	tryGetEnumVec	(thing,	recNode,	{"3@ zero_dcb_codes"							}));
	}{	auto& thing = apriori_pos							;								tryGetFromYaml	(thing,	recNode,	{"4@ apriori_position"							}, "Apriori position in XYZ ECEF frame");
	}{	auto& thing = recOpts.antenna_type					;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	recNode,	{"4@ antenna_type"								}, "Antenna type and radome in 20 character string as per sinex"));
	}{	auto& thing = recOpts.receiver_type					;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	recNode,	{"4@ receiver_type"								}, "Type of gnss receiver hardware"));
	}{	auto& thing = recOpts.sat_id						;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	recNode,	{"4@ sat_id"									}, "Id for receivers that are also satellites"));
	}{	auto& thing = recOpts.elevation_mask_deg			;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	recNode,	{"0! elevation_mask"							}, "Minimum elevation for satellites to be processed"));
	}{	auto& thing = recOpts.receiver_reference_system		;	setInited(recOpts,	thing,	tryGetEnumOpt	(thing,	recNode,	{"@ rec_reference_system"						}, "Receiver will use this system as reference clock"));


	}{	auto& thing = recOpts.error_model					;	setInited(recOpts,	thing,	tryGetEnumOpt	(thing,	recNode,	{"1! error_model"						}));
	}{	auto& thing = recOpts.code_sigma					;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	recNode,	{"2! code_sigma"						}, "Standard deviation of code measurements"));
	}{	auto& thing = recOpts.phase_sigma					;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	recNode,	{"2! phase_sigma"						}, "Standard deviation of phase measurmeents"));


	}{	auto& thing = recOpts.eccentricityModel.enable		;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ eccentricity",				"enable"		}, "Enable antenna eccentrities"));
	}{	auto& thing = eccentricity							;								tryGetFromYaml	(thing,	modelsNode,	{"@ eccentricity",				"offset"		}, "Antenna offset in ENU frame");

	}{	auto& thing = recOpts.tropModel.enable				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ troposphere",				"enable" 		}, "Model tropospheric delays"));
	}{	auto& thing = recOpts.tropModel.models				;	setInited(recOpts,	thing,	tryGetEnumVec	(thing, modelsNode,	{"@ troposphere",				"models" 		}, "List of models to use for troposphere"));

	}{	auto& thing = recOpts.tideModels.enable				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ tides",						"@ enable"		}, "Enable modelling of tidal displacements"));
	}{	auto& thing = recOpts.tideModels.solid				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ tides",						"@ solid"		}, "Enable solid Earth tides"));
	}{	auto& thing = recOpts.tideModels.otl				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ tides",						"@ otl"			}, "Enable ocean tide loading"));
	}{	auto& thing = recOpts.tideModels.atl				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ tides",						"@ atl"			}, "Enable atmospheric tide loading"));
	}{	auto& thing = recOpts.tideModels.spole				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ tides",						"@ spole"		}, "Enable solid Earth pole tides"));
	}{	auto& thing = recOpts.tideModels.opole				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ tides",						"@ opole"		}, "Enable ocean pole tides"));

	}{	auto& thing = recOpts.range							;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ range",						"@ enable"		}, "Enable modelling of signal time of flight time due to range"));
	}{	auto& thing = recOpts.relativity					;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ relativity",				"@ enable"		}, "Enable modelling of relativistic effects"));
	}{	auto& thing = recOpts.relativity2					;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ relativity2",				"@ enable"		}, "Enable modelling of secondary relativistic effects"));
	}{	auto& thing = recOpts.sagnac						;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ sagnac",					"@ enable"		}, "Enable modelling of sagnac effect"));
	}{	auto& thing = recOpts.integer_ambiguity				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ integer_ambiguity",			"@ enable"		}, "Model ambiguities due to unknown integer number of cycles in phase measurements"));
	}{	auto& thing = recOpts.ionospheric_component			;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ ionospheric_components",	"enable"							}, "Enable ionospheric modelling"));
	}{	auto& thing = recOpts.ionospheric_component2		;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ ionospheric_components",	"use_2nd_order"						}));
	}{	auto& thing = recOpts.ionospheric_component3		;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ ionospheric_components",	"use_3rd_order"						}));
	}{	auto& thing = recOpts.mapping_function				;	setInited(recOpts,	thing,	tryGetEnumOpt	(thing,	modelsNode,	{"@ ionospheric_components",	"@ mapping_function" 				}, "Mapping function if not specified in the data or model"));
	}{	auto& thing = recOpts.geomagnetic_field_height		;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ ionospheric_components",	"@ geomagnetic_field_height"		}, "ionospheric pierce point layer height if not specified in the data or model (km)"));
	}{	auto& thing = recOpts.mapping_function_layer_height	;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ ionospheric_components",	"@ mapping_function_layer_height"	}, "mapping function layer height if not specified in the data or model (km)"));
	}{	auto& thing = recOpts.iono_sigma_limit				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ ionospheric_components",	"@ iono_sigma_limit"				}, "Ionosphere states are removed when their sigma exceeds this value"));
	}{	auto& thing = recOpts.ionospheric_model				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ ionospheric_model",			"enable"		}, "Compute ionosphere maps from a network of receivers"));
	}{	auto& thing = recOpts.tropospheric_map				;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ tropospheric_map",			"enable"		}, "Compute tropospheric maps from a network of receivers"));
	}{	auto& thing = recOpts.eop							;	setInited(recOpts,	thing,	tryGetFromYaml	(thing,	modelsNode,	{"@ eop",						"enable"		}, "Enable modelling of eops"));

	}{	auto& thing = recOpts.eccentricityModel.eccentricity;	if (eccentricity		.size()	== 3)	{	thing = Vector3d(eccentricity			.data());		setInited(recOpts,	thing);	}
	}{	auto& thing = recOpts.apriori_pos					;	if (apriori_pos			.size()	== 3)	{	thing = Vector3d(apriori_pos			.data());		setInited(recOpts,	thing);	}

	}


	for (E_ObsCode2 obsCode2 : E_ObsCode2::_values())
	{
		{
		}{	auto& thing = recOpts.rinex23Conv.codeConv[obsCode2]	;	setInited(recOpts,	thing,	tryGetEnumOpt(thing, recNode, {"@ rinex2", "@ rnx_code_conversions",	obsCode2._to_string()}));
		}{	auto& thing = recOpts.rinex23Conv.phasConv[obsCode2]	;	setInited(recOpts,	thing,	tryGetEnumOpt(thing, recNode, {"@ rinex2", "@ rnx_phase_conversions",	obsCode2._to_string()}));
		}
	}
}


/** Set satellite options for a specific satellite using a hierarchy of sources
*/
SatelliteOptions& ACSConfig::getSatOpts(
	SatSys					Sat,		///< Satellite to search for options for
	const vector<string>&	suffixes)	///< Optional suffix to get more specific versions
{
	DOCS_REFERENCE(Aliases_And_Inheritance__);

	string fullId = Sat.id();
	for (auto& suffix : suffixes)
	{
		fullId += ".";
		fullId += suffix;
	}

	lock_guard<mutex> guard(configMutex);

	auto& satOpts = satOptsMap[fullId];

	//return early if possible
	if (satOpts._initialised)
		return satOpts;

	satOpts.id				= fullId;

	BOOST_LOG_TRIVIAL(debug) << "Getting sat config for " << fullId;

	vector<string> aliases;

	aliases.push_back("! global");

	for (int i = 0; i < yamls.size(); i++)
	{
		auto& yaml = yamls[i];

		vector<string> yamlAliases;
		tryGetFromYaml(yamlAliases, {yaml, ""}, {"3! satellite_options", Sat.id(), "@ aliases"}, "Aliases for this satellite");

		for (auto& alias : yamlAliases)
		{
			aliases.push_back(alias);
		}
	}

	//add global and this id on either side of the aliases
											aliases.push_back(Sat.sysName());
	if (Sat.blockType()	.empty() == false)	aliases.push_back(Sat.blockType());
											aliases.push_back(Sat.id());
	if (Sat.svn()		.empty() == false)	aliases.push_back("SVN_" + Sat.svn());

	//prepare all the aliases or whatever using _names

	for (auto& alias : aliases)
	for (int S = 0; S <= suffixes.size(); S++)
	{
		boost::trim_right(alias);

		vector<string> estimationDescriptorVec	= {estimation_parameters_str, "0! satellites", alias};
		vector<string> optionsDescriptorVec		= {"3! satellite_options", alias};

		string suffixName = (string) "_" + alias;

		for (int s = 0; s < S; s++)
		{
			estimationDescriptorVec	.push_back(suffixes[s]);
			optionsDescriptorVec	.push_back(suffixes[s]);
			suffixName += suffixes[s];
		}

		auto& suffixOpts = satOptsMap[suffixName];

		if (suffixOpts._initialised)
		{
			continue;
		}

		suffixOpts.id = suffixName;

		for (auto& yaml : yamls)
		{
			stringsToYamlObject({yaml, ""}, {"3! receiver_options"},	"Options to configure individual satellites, systems, or global configs");

			getKalmanFromYaml	(suffixOpts, {yaml, ""}, estimationDescriptorVec);
			getOptionsFromYaml	(suffixOpts, {yaml, ""}, optionsDescriptorVec);
		}

		suffixOpts._initialised	= true;
	}

	//add up all the aliases or whatever
	for (int S = 0; S <= suffixes.size(); S++)
	for (auto& alias : aliases)
	{
		string suffixName = (string) "_" + alias;

		for (int s = 0; s < S; s++)
		{
			suffixName += suffixes[s];
		}

		auto& suffixOpts = satOptsMap[suffixName];

		if (suffixOpts.id != satOpts.id)
		{
			suffixOpts.inheritors[satOpts.id] = &satOpts;
		}

		BOOST_LOG_TRIVIAL(debug) << "   Inheriting sat config from " << suffixName;

		satOpts += suffixOpts;
	}

	satOpts._initialised	= true;
	return satOpts;
}


/** Set receiver options for a specific receiver using a hierarchy of sources
*/
ReceiverOptions& ACSConfig::getRecOpts(
	string					id,			///< Receiver to search for options for
	const vector<string>&	suffixes)	///< Optional suffix to get more specific versions
{
	DOCS_REFERENCE(Aliases_And_Inheritance__);

	string fullId = id;

	for (auto& suffix : suffixes)
	{
		fullId += ".";
		fullId += suffix;
	}

	lock_guard<mutex> guard(configMutex);

	auto& recOpts = recOptsMap[fullId];

	//return early if possible
	if (recOpts._initialised)
		return recOpts;

	BOOST_LOG_TRIVIAL(debug) << "Getting rec config for " << fullId;

	recOpts.id = fullId;

	vector<string> aliases;

	aliases.push_back("! global");

	for (auto& alias : customAliasesMap[id])
	{
		aliases.push_back(alias);
	}

	for (int i = 0; i < yamls.size(); i++)
	{
		auto& yaml = yamls[i];

		vector<string> yamlAliases;
		tryGetFromYaml(yamlAliases, {yaml, ""}, {"3! receiver_options", id, "@ aliases"}, "Aliases for this receiver");

		for (auto& alias : yamlAliases)
		{
			aliases.push_back(alias);
		}
	}

	//add global and this id on either side of the aliases
	aliases.push_back(id);

	//prepare all the aliases or whatever using _names

	for (auto& alias : aliases)
	for (int S = 0; S <= suffixes.size(); S++)
	{
		vector<string> estimationDescriptorVec	= {estimation_parameters_str, "0! receivers", alias};
		vector<string> optionsDescriptorVec		= {"3! receiver_options", alias};

		string suffixName = (string) "_" + alias;

		for (int s = 0; s < S; s++)
		{
			estimationDescriptorVec	.push_back(suffixes[s]);
			optionsDescriptorVec	.push_back(suffixes[s]);
			suffixName += suffixes[s];
		}

		auto& suffixOpts = recOptsMap[suffixName];

		if (suffixOpts._initialised)
		{
			continue;
		}

		suffixOpts.id = suffixName;

		for (auto& yaml : yamls)
		{
			stringsToYamlObject({yaml, ""}, {"3! receiver_options"},	"Options to configure individual receivers or global configs");

			getKalmanFromYaml	(suffixOpts, {yaml, ""}, estimationDescriptorVec);
			getOptionsFromYaml	(suffixOpts, {yaml, ""}, optionsDescriptorVec);
		}

		suffixOpts._initialised	= true;
	}

	//add up all the aliases or whatever
	for (int S = 0; S <= suffixes.size(); S++)
	for (auto& alias : aliases)
	{
		string suffixName = (string) "_" + alias;

		for (int s = 0; s < S; s++)
		{
			suffixName += suffixes[s];
		}

		auto& suffixOpts = recOptsMap[suffixName];

		if (suffixOpts.id != recOpts.id)
		{
			suffixOpts.inheritors[recOpts.id] = &recOpts;
		}

		BOOST_LOG_TRIVIAL(debug) << "   Inheriting rec config from " << suffixName;

		recOpts += suffixOpts;
	}

	recOpts._initialised	= true;
	return recOpts;
}

/** Set and scale a variable according to yaml options
*/
template<typename ENUM>
void tryGetScaledFromYaml(
	double&					output,									///< Variable to output to
	NodeStack				node,									///< Yaml node to search within
	const vector<string>&	number_parameter,						///< List of keys of the hierarchy to the value to be set
	const vector<string>&	scale_parameter,						///< List of keys of the hierarchy to the scale to be applied
	ENUM					(&_from_string_nocase)(const char*),	///< Function to decode scale enum strings
	const string&			comment = "")							///< Description to use for documentation
{
	double	number			= output;
	ENUM	number_units	= ENUM::_from_integral(1);

	tryGetFromYaml	(number,		node, number_parameter, comment);
	tryGetEnumOpt	(number_units, 	node, scale_parameter);

	number *= (int)number_units;
	if (number != 0)
	{
		output = number;
	}
}

void recurseLowerCase(
	YAML::Node& node)
{
	for (auto it = node.begin(); it != node.end(); ++it)
	try
	{
		it->first = boost::algorithm::to_lower_copy(it->first.as<std::string>());
		if	( it->second.IsSequence()
			||it->second.IsMap())
		{
			recurseLowerCase(it->second);
		}
	}
	catch (...)
	{
	}
}

void ACSConfig::recurseYaml(
	const string&	file,
	YAML::Node		node,
	const string&	stack,
	const string&	aliasStack)
{
	for (YAML::const_iterator it = node.begin(); it != node.end(); it++)
	{
		string key = it->first.as<string>();

// 		std::cout << key << "\n";

		string newStack			= stack			+ key + ":";
		string newAliasStack	= aliasStack	+ key + ":";

		bool altered = false;

		auto& found = foundOptions[file][newStack];
		if (found)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: Duplicate " << newStack << " entries found in config file: " << file;
		}
		found = true;

		if (availableOptions.find(newAliasStack) == availableOptions.end())
		{
			//this yaml stack not found in the available options, check to see if it could have worked if it were an alias

			for (auto str :
			{
				"estimation_parameters:receivers:",
				"estimation_parameters:satellites:",
				"outputs:streams:",
				"receiver_options:",
				"satellite_options:",
			})
			if (stack.find(str) != string::npos)
			{
				newAliasStack = str + (string)"global:";

				altered = true;

				if (availableOptions.find(newAliasStack) == availableOptions.end())
				{
					BOOST_LOG_TRIVIAL(warning)
					<< "Warning: " << newStack << " is not a valid yaml option";

					continue;
				}
				break;
			}

			if (altered == false)
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
			recurseYaml(file, node[key], newStack, newAliasStack);
		}
		else if (node[key].IsNull())
		{
			//dont complain about things that dont do anything
		}
		else
		{
			//is a final value, this must pass on its own - ie, dont let final leafs with dumb names get aliased away, only pass those that should

			if (altered)
			{
				BOOST_LOG_TRIVIAL(warning)
				<< "Warning: " << newStack << " is not a valid yaml option";

				continue;
			}
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

	("help,h",																							"Help")
	("quiet,q",																							"Less output")
	("very-quiet,Q",																					"Much less output")
	("verbose,v",																						"More output")
	("very-verbose,V",																					"Much more output")
	("interactive,I",																					"Use interactive terminal")
	("yaml-defaults,Y",					boost::program_options::value<int>(),							"Print set of parsed parameters and their default values according to their priority level (1-3), and generate configurator.html for visual editing of yaml files")
	("config_description,d",			boost::program_options::value<string>(),						"Configuration description")
	("level,l",							boost::program_options::value<int>(),							"Trace level")
	("fatal_message_level,L",			boost::program_options::value<int>(),							"Fatal error level")
	("elevation_mask,e",				boost::program_options::value<float>(), 						"Elevation Mask")
	("max_epochs,n",					boost::program_options::value<int>(),							"Maximum Epochs")
	("epoch_interval,i",				boost::program_options::value<float>(), 						"Epoch Interval")
	("user,u",							boost::program_options::value<string>(),						"Username for RTCM streams")
	("pass,p",							boost::program_options::value<string>(),						"Password for RTCM streams")
	("config,y",						boost::program_options::value<vector<string>>()->multitoken(),	"Configuration file")
	("atx_files",						boost::program_options::value<vector<string>>()->multitoken(),	"ANTEX files")
	("nav_files",						boost::program_options::value<vector<string>>()->multitoken(),	"Navigation files")
	("snx_files",						boost::program_options::value<vector<string>>()->multitoken(),	"SINEX files")
	("sp3_files",						boost::program_options::value<vector<string>>()->multitoken(),	"Orbit (SP3) files")
	("clk_files",						boost::program_options::value<vector<string>>()->multitoken(),	"Clock (CLK) files")
	("obx_files",						boost::program_options::value<vector<string>>()->multitoken(),	"ORBEX (OBX) files")
	("dcb_files",						boost::program_options::value<vector<string>>()->multitoken(),	"Code Bias (DCB) files")
	("bsx_files",						boost::program_options::value<vector<string>>()->multitoken(),	"Bias Sinex (BSX) files")
	("ion_files",						boost::program_options::value<vector<string>>()->multitoken(),	"Ionosphere (IONEX) files")
	("igrf_files",						boost::program_options::value<vector<string>>()->multitoken(),	"Geomagnetic field coefficients (IGRF) file")
	("ocean_tide_loading_blq_files",	boost::program_options::value<vector<string>>()->multitoken(),	"BLQ (Ocean tidal loading) files")
	("atmos_tide_loading_blq_files",	boost::program_options::value<vector<string>>()->multitoken(),	"BLQ (Atmospheric tidal loading) files")
	("erp_files",						boost::program_options::value<vector<string>>()->multitoken(),	"ERP files")
	("rnx_inputs,r",					boost::program_options::value<vector<string>>()->multitoken(),	"RINEX receiver inputs")
	("ubx_inputs",						boost::program_options::value<vector<string>>()->multitoken(),	"UBX receiver inputs")
	("rtcm_inputs",						boost::program_options::value<vector<string>>()->multitoken(),	"RTCM receiver inputs")
	("egm_files",						boost::program_options::value<vector<string>>()->multitoken(),	"Earth gravity model coefficients file")
	("crd_files",						boost::program_options::value<vector<string>>()->multitoken(),	"SLR CRD file")
	("slr_inputs",						boost::program_options::value<vector<string>>()->multitoken(),	"Tabular SLR OBS receiver file")
	("planetary_ephemeris_files",		boost::program_options::value<vector<string>>()->multitoken(),	"JPL planetary and lunar ephemerides file")
	("inputs_root",						boost::program_options::value<string>(),						"Root to apply to non-absolute input locations")
	("outputs_root",					boost::program_options::value<string>(),						"Root to apply to non-absolute output locations")
	("start_epoch",						boost::program_options::value<string>(),						"Start date/time")
	("end_epoch",						boost::program_options::value<string>(),						"Stop date/time")
// 	("run_rts_only",					boost::program_options::value<string>(),						"RTS filename (without _xxxxx suffix)")
	("dump-config-only",																				"Dump the configuration and exit")
	("walkthrough",																						"Run demonstration code interactively with commentary")
	("compare_clocks",																					"Compare clock files")
	("compare_orbits",																					"Compare sp3 files")
	("compare_attitudes",																				"Compare antex files")
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

	if (vm.count("very-verbose"))	{	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::trace);		acsSeverity = boost::log::trivial::trace;}
	if (vm.count("verbose"))		{	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::debug);		acsSeverity = boost::log::trivial::debug;}
	if (vm.count("quiet"))			{	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::warning);	acsSeverity = boost::log::trivial::warning;}
	if (vm.count("very-quiet"))		{	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::error);		acsSeverity = boost::log::trivial::error;}

	if (vm.count("interactive"))	{	InteractiveTerminal::enable();		}

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

	if	( acsConfig.compare_clocks
		||vm.count("compare_clocks"))
	{
		std::cout << "\n" << "----- Clock Comparator -----" << "\n";
		tryGetFromOpts(acsConfig.clk_files,			vm, {"clk_files"});
		compareClocks(acsConfig.clk_files);
		std::cout << "\n" << "----- Clock Comparator -----" << "\n";
		exit(EXIT_SUCCESS);
	}

	if	( acsConfig.compare_orbits
		||vm.count("compare_orbits"))
	{
		std::cout << "\n" << "----- Orbit Comparator -----" << "\n";
		tryGetFromOpts(acsConfig.sp3_files,			vm, {"sp3_files"});
		compareOrbits(acsConfig.sp3_files);
		std::cout << "\n" << "----- Orbit Comparator -----" << "\n";
		exit(EXIT_SUCCESS);
	}

	if	( acsConfig.compare_attitudes
		||vm.count("compare_attitudes"))
	{
		std::cout << "\n" << "----- Attitude Comparator -----" << "\n";
		tryGetFromOpts(acsConfig.obx_files,			vm, {"obx_files"});
		compareAttitudes(acsConfig.obx_files);
		std::cout << "\n" << "----- Attitude Comparator -----" << "\n";
		exit(EXIT_SUCCESS);
	}

	// Dump the configuration information
	acsConfig.info(std::cout);

	// Check the configuration
	bool valid = true;
	valid &= checkValidFiles(acsConfig.snx_files, 						"sinex file (snx file)");
	valid &= checkValidFiles(acsConfig.nav_files, 						"navfiles");
	valid &= checkValidFiles(acsConfig.sp3_files, 						"orbit");
	valid &= checkValidFiles(acsConfig.clk_files,						"clock file (CLK file)");
	valid &= checkValidFiles(acsConfig.obx_files, 						"orbex file (OBX file)");
	valid &= checkValidFiles(acsConfig.ocean_tide_loading_blq_files, 	"ocean loading information (Blq file)");
	valid &= checkValidFiles(acsConfig.atmos_tide_loading_blq_files, 	"atmospheric loading information (Blq file)");
	valid &= checkValidFiles(acsConfig.erp_files,						"earth rotation parameter file (ERP file)");
	valid &= checkValidFiles(acsConfig.dcb_files,						"code Biases file (DCB file)");
	valid &= checkValidFiles(acsConfig.bsx_files,						"bias Sinex file (BSX file)");
	valid &= checkValidFiles(acsConfig.ion_files,						"Ionosphere (IONEX file)");
	valid &= checkValidFiles(acsConfig.igrf_files,						"geomagnetic field coefficients (IGRF file)");
	valid &= checkValidFiles(acsConfig.atx_files, 						"antenna information (ANTEX file)");
	valid &= checkValidFiles(acsConfig.egm_files, 						"Earth gravity model coefficients (egm file)");
	valid &= checkValidFiles(acsConfig.planetary_ephemeris_files, 		"Planetary and lunar ephemerides");
	valid &= checkValidFiles(acsConfig.ocean_tide_potential_files,		"Ocean tide file (tide file)");
	valid &= checkValidFiles(acsConfig.atmos_tide_potential_files,		"Atmospheric tide file  (tide file)");
	valid &= checkValidFiles(acsConfig.cmc_files, 						"Center of Mass tide file corrections");
	valid &= checkValidFiles(acsConfig.hfeop_files,						"Subdaily EOP variations");
	valid &= checkValidFiles(acsConfig.atmos_oceean_dealiasing_files,	"Atmosphere and Ocean De-aliasing (AOD1B file)");
	valid &= checkValidFiles(acsConfig.ocean_pole_tide_potential_files,	"Pole ocean tide file");
	valid &= checkValidFiles(acsConfig.sid_files, 						"satellite ID list");
	valid &= checkValidFiles(acsConfig.com_files, 						"centre-of-mass (com file)");
	valid &= checkValidFiles(acsConfig.crd_files, 						"SLR observation files (crd file)");
	valid &= checkValidFiles(acsConfig.gpt2grid_files,					"grid");
	valid &= checkValidFiles(acsConfig.orography_files,					"orography");

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

void ACSConfig::sanityChecks()
{
	if (ambErrors.outage_reset_limit <	epoch_interval)		BOOST_LOG_TRIVIAL(warning) << "Warning: outage_reset_limit < epoch_interval, but it probably shouldnt be";
	if (ionErrors.outage_reset_limit <	epoch_interval)		BOOST_LOG_TRIVIAL(warning) << "Warning: outage_reset_limit < epoch_interval, but it probably shouldnt be";
}

bool ACSConfig::parse()
{
	return parse(configFilenames, commandOpts);
}

/** Parse options to set acsConfig values.
* Command line options will override any values set in config files, which will themselves override any program default values.
*/
bool ACSConfig::parse(
	const vector<string>&					filenames,		///< Path to yaml based config file
	boost::program_options::variables_map&	newCommandOpts)	///< Variable map object of command line options
{
	DOCS_REFERENCE(Config__);

	configFilenames = filenames;

	bool modified = false;

	for (auto& filenameList : {filenames, includedFilenames})
	for (auto& filename : filenameList)
	if (filename != "")
	{
		std::filesystem::path filePath(filename);
		auto currentConfigModifyTime = std::filesystem::last_write_time(filePath);

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
	foundOptions	.clear();
	satOptsMap		.clear();
	recOptsMap		.clear();
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
				BOOST_LOG_TRIVIAL(error) << e.msg << "\n";
				return false;
			}
		}
		catch (const YAML::ParserException& e)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: \nFailed to parse configuration. Check for errors as described near the below:\n";
			BOOST_LOG_TRIVIAL(error) << e.what() << "\n" << "\n";
			return false;
		}

		vector<string> includes;

		auto inputs	= stringsToYamlObject({yaml, ""},	{"0! inputs"}, docs["inputs"]);

		tryGetFromYaml(includes, inputs, {"1! include_yamls"}, "List of yaml files to include before this one");

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
			std::filesystem::path filePath(filename);
			auto currentConfigModifyTime = std::filesystem::last_write_time(filePath);

			configModifyTimeMap[filename] = currentConfigModifyTime;

			yaml.reset();
			yaml = YAML::LoadFile(filename);
			yaml["yaml_filename"]	= filename;
			yaml["yaml_number"]		= i;
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
				BOOST_LOG_TRIVIAL(error) << e.msg << "\n";
				return false;
			}
		}
		catch (const std::filesystem::filesystem_error &e)
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
			BOOST_LOG_TRIVIAL(error) << e.what() << "\n" << "\n";
			return false;
		}

		recurseLowerCase(yaml);

// 		outputs
		{
			auto outputs = stringsToYamlObject({yaml, ""}, {"1! outputs"}, docs["outputs"]);

			tryGetFromYaml(outputs_root,			outputs, {"0! outputs_root"			}, "Directory that outputs will be placed in");

			{
				auto metadata = stringsToYamlObject(outputs, 		{"2! metadata"}, "Options for setting metadata for inputs and outputs");

				tryGetFromAny (config_description,	commandOpts,	metadata, {"1! config_description"				}, "ID for this config, used to replace <CONFIG> tags in other options");
				tryGetFromAny (stream_user,			commandOpts,	metadata, {"1! user"							}, "Username for connecting to NTRIP casters");
				tryGetFromAny (stream_pass,			commandOpts,	metadata, {"1! pass"							}, "Password for connecting to NTRIP casters");
				tryGetFromYaml(analysis_agency,						metadata, {"@ analysis_agency"					}, "Agency for output files headers");
				tryGetFromYaml(config_details,						metadata, {"@ config_details"					}, "Comments and details specific to the config");
				tryGetFromYaml(analysis_centre,						metadata, {"@ analysis_centre"					}, "Analysis centre for output files headers");
				tryGetFromYaml(ac_contact,							metadata, {"@ ac_contact"						}, "Contact person for output files headers");
				tryGetFromYaml(analysis_software,					metadata, {"@ analysis_software"				}, "Program for output files headers");
				tryGetFromYaml(analysis_software_version,			metadata, {"@ analysis_software_version"		}, "Version for output files headers");
				tryGetFromYaml(rinex_comment,						metadata, {"@ rinex_comment"					}, "Comment for output files headers");
				tryGetFromYaml(reference_system,					metadata, {"@ reference_system"					}, "Terrestrial Reference System Code");
				tryGetFromYaml(time_system,							metadata, {"@ time_system"						}, "Time system - e.g. \"G\", \"UTC\"");
				tryGetFromYaml(ocean_tide_loading_model,			metadata, {"@ ocean_tide_loading_model"			}, "Ocean tide loading model applied");
				tryGetFromYaml(atmospheric_tide_loading_model,		metadata, {"@ atmospheric_tide_loading_model"	}, "Atmospheric tide loading model applied");
				tryGetFromYaml(geoid_model,							metadata, {"@ geoid_model"						}, "Geoid model name for undulation values");
				tryGetFromYaml(gradient_mapping_function,			metadata, {"@ gradient_mapping_function"		}, "Name of mapping function used for mapping horizontal troposphere gradients");
			}

			{
				tryGetFromYaml(colourise_terminal,			outputs, {"1! colourise_terminal"		}, "Use ascii command codes to highlight warnings and errors");
				tryGetFromYaml(warn_once,					outputs, {"1! warn_once"					}, "Print warnings once only");
			}

			{
				auto trace = stringsToYamlObject(outputs, {"2! trace"}, docs["trace"]);

																					tryGetFromYaml(output_receiver_trace,		trace, {"0! output_receivers"		}, "Output trace files for individual receivers processing");
																					tryGetFromYaml(output_network_trace,		trace, {"0! output_network"			}, "Output trace files for complete network of receivers, inclucing kalman filter results and statistics");
																					tryGetFromYaml(output_ionosphere_trace,		trace, {"0@ output_ionosphere"		}, "Output trace files for ionosphere processing, inclucing kalman filter results and statistics");
																					tryGetFromYaml(output_satellite_trace,		trace, {"0@ output_satellites"		}, "Output trace files for individual satellites processing");
				conditionalPrefix("<OUTPUTS_ROOT>",		trace_directory,			tryGetFromYaml(trace_directory,				trace, {"! directory"				}, "Directory to output trace files to"));
				conditionalPrefix("<TRACE_DIRECTORY>",	satellite_trace_filename,	tryGetFromYaml(satellite_trace_filename,	trace, {"1@ satellite_filename"		}, "Template filename for satellite trace files"));
				conditionalPrefix("<TRACE_DIRECTORY>",	receiver_trace_filename,	tryGetFromYaml(receiver_trace_filename,		trace, {"1! receiver_filename"		}, "Template filename for receiver trace files"));
				conditionalPrefix("<TRACE_DIRECTORY>",	ionosphere_trace_filename,	tryGetFromYaml(ionosphere_trace_filename,	trace, {"1@ ionosphere_filename"	}, "Template filename for ionosphere trace files"));
				conditionalPrefix("<TRACE_DIRECTORY>",	network_trace_filename,		tryGetFromYaml(network_trace_filename,		trace, {"1! network_filename"		}, "Template filename for network trace files"));
																					tryGetFromAny(trace_level, commandOpts,		trace, {"! level"					}, "Threshold level for printing messages (0-6). Increasing this increases the amount of data stored in all trace files");

				traceLevel = acsConfig.trace_level;

																					tryGetFromYaml(output_residual_chain,		trace, {"! output_residual_chain"		}, "Output component-wise details for measurement residuals");
																					tryGetFromYaml(output_predicted_states,		trace, {"! output_predicted_states"		}, "Output states after state transition 1");
																					tryGetFromYaml(output_initialised_states,	trace, {"! output_initialised_states"	}, "Output states after state transition 2");
																					tryGetFromYaml(output_residuals,			trace, {"! output_residuals"			}, "Output measurements and residuals");
																					tryGetFromYaml(output_config,				trace, {"! output_config"				}, "Output configuration files to top of trace files");
																					tryGetFromYaml(output_json_trace,			trace, {"@ output_json"					}, "Output json formatted trace files");
			}

			{
				auto output_rotation = stringsToYamlObject(outputs, {"2@ output_rotation"}, "Trace files can be rotated periodically by epoch interval. These options specify the period that applies to the <LOGTIME> template variables in filenames");

				tryGetScaledFromYaml(rotate_period,		output_rotation, {"@ period"	},	{"@ period_units"	},	E_Period::_from_string_nocase, "Period that times will be rounded by to generate template variables in filenames");
			}

			{
				auto bias_sinex = stringsToYamlObject(outputs, {"3@ bias_sinex"}, "Rinex formatted bias sinex files");

																					tryGetFromYaml(output_bias_sinex,				bias_sinex, {"0@ output"				}, "Output bias sinex files");
				conditionalPrefix("<OUTPUTS_ROOT>",			bias_sinex_directory,	tryGetFromYaml(bias_sinex_directory,			bias_sinex, {"@ directory"				}, "Directory to output bias sinex files to"));
				conditionalPrefix("<BIAS_SINEX_DIRECTORY>",	bias_sinex_filename,	tryGetFromYaml(bias_sinex_filename,				bias_sinex, {"@ filename"				}, "Template filename for bias sinex files"));
																					tryGetFromYaml(bias_time_system,				bias_sinex, {"@ bias_time_system"		}, "Time system for bias SINEX \"G\", \"C\", \"R\", \"UTC\", \"TAI\"");
																					tryGetFromYaml(ambrOpts.code_output_interval,	bias_sinex, {"@ code_output_interval"	}, "Update interval for code  biases");
																					tryGetFromYaml(ambrOpts.phase_output_interval,	bias_sinex, {"@ phase_output_interval"	}, "Update interval for phase biases");
																					tryGetFromYaml(ambrOpts.output_rec_bias,		bias_sinex, {"@ output_rec_bias"		}, "output receiver biases");
			}

			{
				auto clocks = stringsToYamlObject(outputs, {"3! clocks"}, "Rinex formatted clock files");

																			tryGetFromYaml(output_clocks,				clocks, {"0! output"				}, "Output clock files");
				conditionalPrefix("<OUTPUTS_ROOT>",		clocks_directory,	tryGetFromYaml(clocks_directory,			clocks, {"@ directory"				}, "Directory to output clock files to"));
				conditionalPrefix("<CLOCKS_DIRECTORY>",	clocks_filename,	tryGetFromYaml(clocks_filename,				clocks, {"@ filename"				}, "Template filename for clock files"));
																			tryGetEnumVec (clocks_receiver_sources,		clocks, {"@ receiver_sources"		});
																			tryGetEnumVec (clocks_satellite_sources,	clocks, {"@ satellite_sources"		});
																			tryGetFromYaml(clocks_output_interval,		clocks, {"@ output_interval"		}, "Update interval for clock records");
			}

			{
				auto decoded_rtcm = stringsToYamlObject(outputs, {"6@ decoded_rtcm"}, "RTCM messages that are received may be recorded to human-readable json files");

																								tryGetFromYaml(output_decoded_rtcm_json,	decoded_rtcm, {"0@ output"		},	"Enable exporting decoded RTCM data to file");
				conditionalPrefix("<OUTPUTS_ROOT>",				decoded_rtcm_json_directory,	tryGetFromYaml(decoded_rtcm_json_directory,	decoded_rtcm, {"@ directory"	},	"Directory to export decoded RTCM data"));
				conditionalPrefix("<DECODED_RTCM_DIRECTORY>",	decoded_rtcm_json_filename,		tryGetFromYaml(decoded_rtcm_json_filename,	decoded_rtcm, {"@ filename"		},	"Decoded RTCM data filename"));
			}

			{
				auto encoded_rtcm = stringsToYamlObject(outputs, {"6@ encoded_rtcm"}, "RTCM messages that are encoded and transmitted may be recorded to human-readable json files");

																								tryGetFromYaml(output_encoded_rtcm_json,	encoded_rtcm, {"0@ output"		},	"Enable exporting encoded RTCM data to file");
				conditionalPrefix("<OUTPUTS_ROOT>",				encoded_rtcm_json_directory,	tryGetFromYaml(encoded_rtcm_json_directory,	encoded_rtcm, {"@ directory"	},	"Directory to export encoded RTCM data"));
				conditionalPrefix("<ENCODED_RTCM_DIRECTORY>",	encoded_rtcm_json_filename,		tryGetFromYaml(encoded_rtcm_json_filename,	encoded_rtcm, {"@ filename"		},	"Encoded RTCM data filename"));
			}

			{
				auto erp = stringsToYamlObject(outputs, {"3@ erp"}, "Earth rotation parameters can be output to file");

																		tryGetFromYaml(output_erp,				erp, {"0@ output"		}, "Enable exporting of erp data");
				conditionalPrefix("<OUTPUTS_ROOT>",		erp_directory,	tryGetFromYaml(erp_directory,			erp, {"@ directory"		}, "Directory to export erp data files"));
				conditionalPrefix("<ERP_DIRECTORY>",	erp_filename,	tryGetFromYaml(erp_filename,			erp, {"@ filename"		}, "ERP data output filename"));
			}

			{
				auto ionex = stringsToYamlObject(outputs, {"4@ ionex"}, "IONEX formatted ionospheric mapping and modelling outputs");

																			tryGetFromYaml(output_ionex,			ionex, {"0@ output"						}, "Enable exporting ionospheric model data");
				conditionalPrefix("<OUTPUTS_ROOT>",		ionex_directory,	tryGetFromYaml(ionex_directory,			ionex, {"@ directory"					}, "Directory to export ionex data"));
				conditionalPrefix("<IONEX_DIRECTORY>",	ionex_filename,		tryGetFromYaml(ionex_filename,			ionex, {"@ filename"					}, "Ionex data filename"));
																			tryGetFromYaml(ionexGrid.lat_centre,	ionex, {"@ grid", "@ lat_centre"		}, "Center lattitude for models");
																			tryGetFromYaml(ionexGrid.lon_centre,	ionex, {"@ grid", "@ lon_centre"		}, "Center longitude for models");
																			tryGetFromYaml(ionexGrid.lat_width,		ionex, {"@ grid", "@ lat_width"			}, "Total lattitudinal width of model");
																			tryGetFromYaml(ionexGrid.lon_width,		ionex, {"@ grid", "@ lon_width"			}, "Total longitudinal width of model");
																			tryGetFromYaml(ionexGrid.lat_res,		ionex, {"@ grid", "@ lat_resolution"	}, "Interval between lattitude outputs");
																			tryGetFromYaml(ionexGrid.lon_res,		ionex, {"@ grid", "@ lon_resolution"	}, "Interval between longitude outputs");
																			tryGetFromYaml(ionexGrid.time_res,		ionex, {"@ grid", "@ time_resolution"	}, "Interval between output epochs");
			}

			{
				auto ionstec = stringsToYamlObject(outputs, {"4@ ionstec"});

																				tryGetFromYaml(output_ionstec,		ionstec, {"0@ output"		});
				conditionalPrefix("<OUTPUTS_ROOT>",			ionstec_directory,	tryGetFromYaml(ionstec_directory,	ionstec, {"@ directory"		}));
				conditionalPrefix("<IONSTEC_DIRECTORY>",	ionstec_filename,	tryGetFromYaml(ionstec_filename,	ionstec, {"@ filename"		}));
			}

			{
				auto sbas_ems = stringsToYamlObject(outputs, {"4@ sbas_ems"});

																		tryGetFromYaml(output_sbas_ems,	sbas_ems, {"0@ output"		});
				conditionalPrefix("<OUTPUTS_ROOT>",		ems_directory,	tryGetFromYaml(ems_directory,	sbas_ems, {"@ directory"	}));
				conditionalPrefix("<EMS_DIRECTORY>",	ems_filename,	tryGetFromYaml(ems_filename,	sbas_ems, {"@ filename"		}));
			}

			{
				auto sinex = stringsToYamlObject(outputs, {"3@ sinex"});

																			tryGetFromYaml(output_sinex,			sinex, {"0@ output"			});
				conditionalPrefix("<OUTPUTS_ROOT>",		sinex_directory,	tryGetFromYaml(sinex_directory,			sinex, {"@ directory"		}));
				conditionalPrefix("<SINEX_DIRECTORY>",	sinex_filename,		tryGetFromYaml(sinex_filename,			sinex, {"@ filename"		}));
			}

			{
				auto log = stringsToYamlObject(outputs, {"3! log"}, "Log files store console output in files");

																			tryGetFromYaml(output_log,				log, {"0! output"			}, "Enable console output logging");
				conditionalPrefix("<OUTPUTS_ROOT>",		log_directory,		tryGetFromYaml(log_directory,			log, {"@ directory"			}, "Log output directory"));
				conditionalPrefix("<LOG_DIRECTORY>",	log_filename,		tryGetFromYaml(log_filename,			log, {"@ filename"			}, "Log output filename"));
			}

			{
				auto gpx = stringsToYamlObject(outputs, {"3! gpx"}, "GPX files contain point data that may be easily viewed in GIS mapping software");

																			tryGetFromYaml(output_gpx,				gpx, {"0! output"			});
				conditionalPrefix("<OUTPUTS_ROOT>",		gpx_directory,		tryGetFromYaml(gpx_directory,			gpx, {"@ directory"			}));
				conditionalPrefix("<GPX_DIRECTORY>",	gpx_filename,		tryGetFromYaml(gpx_filename,			gpx, {"@ filename"			}));
			}
			{
				auto pos = stringsToYamlObject(outputs, {"3! pos"}, "POS files contain point data that may be easily viewed in GIS mapping software");

																			tryGetFromYaml(output_pos,				pos, {"0! output"			});
				conditionalPrefix("<OUTPUTS_ROOT>",		pos_directory,		tryGetFromYaml(pos_directory,			pos, {"@ directory"			}));
				conditionalPrefix("<POS_DIRECTORY>",	pos_filename,		tryGetFromYaml(pos_filename,			pos, {"@ filename"			}));
			}

			{
				auto ntrip_log = stringsToYamlObject(outputs, {"5@ ntrip_log"});

																					tryGetFromYaml(output_ntrip_log,		ntrip_log, {"0@ output"		});
				conditionalPrefix("<OUTPUTS_ROOT>",			ntrip_log_directory,	tryGetFromYaml(ntrip_log_directory,		ntrip_log, {"@ directory"	}));
				conditionalPrefix("<NTRIP_LOG_DIRECTORY>",	ntrip_log_filename,		tryGetFromYaml(ntrip_log_filename,		ntrip_log, {"@ filename"	}));
			}

			{
				auto network_statistics = stringsToYamlObject(outputs, {"5@ network_statistics"});

																										tryGetFromYaml(output_network_statistics_json,		network_statistics, {"0@ output"	},	"Enable exporting network statistics data to file");
				conditionalPrefix("<OUTPUTS_ROOT>",					network_statistics_json_directory,	tryGetFromYaml(network_statistics_json_directory,	network_statistics, {"@ directory"	},	"Directory to export network statistics data"));
				conditionalPrefix("<NETWORK_STATISTICS_DIRECTORY>",	network_statistics_json_filename,	tryGetFromYaml(network_statistics_json_filename,	network_statistics, {"@ filename"	},	"Network statistics data filename"));
			}

			{
				auto sp3 = stringsToYamlObject(outputs, {"3@ sp3"}, "SP3 files contain orbital and clock data of satellites and receivers");

																				tryGetFromYaml(output_sp3,					sp3, {"0@ output"					}, "Enable SP3 file outputs");
																				tryGetFromYaml(output_inertial_orbits, 		sp3, {"@ output_inertial"			}, "Output the entries using inertial positions and velocities");
																				tryGetFromYaml(output_sp3_velocities,		sp3, {"@ output_velocities"			}, "Output velocity data to SP3 file");
				conditionalPrefix("<OUTPUTS_ROOT>",		sp3_directory,			tryGetFromYaml(sp3_directory,				sp3, {"@ directory"					}, "Directory to store SP3 outputs"));
				conditionalPrefix("<SP3_DIRECTORY>",	sp3_filename,			tryGetFromYaml(sp3_filename,				sp3, {"@ filename"					}, "SP3 output filename"));
				conditionalPrefix("<SP3_DIRECTORY>",	predicted_sp3_filename,	tryGetFromYaml(predicted_sp3_filename,		sp3, {"@ predicted_filename"		}, "Filename for predicted SP3 outputs"));
																				tryGetEnumVec (sp3_clock_sources,			sp3, {"@ clock_sources"				}, "List of sources for clock data for SP3 outputs");
																				tryGetEnumVec (sp3_orbit_sources,			sp3, {"@ orbit_sources"				}, "List of sources for orbit data for SP3 outputs");
																				tryGetFromYaml(sp3_output_interval,			sp3, {"@ output_interval"			}, "Update interval for SP3 records");
			}

			{
				auto orbit_ics = stringsToYamlObject(outputs, {"4@ orbit_ics"}, "Orbital parameters can be output in a yaml that Ginan can later use as an initial condition for futher processing.");

																					tryGetFromYaml(output_orbit_ics,	orbit_ics, {"@ output"		}, "Output orbital initial condition file");
				conditionalPrefix("<OUTPUTS_ROOT>",			orbit_ics_directory,	tryGetFromYaml(orbit_ics_directory,	orbit_ics, {"@ directory"	}, "Output orbital initial condition directory"));
				conditionalPrefix("<ORBIT_ICS_DIRECTORY>",	orbit_ics_filename,		tryGetFromYaml(orbit_ics_filename,	orbit_ics, {"@ filename"	}, "Output orbital initial condition filename"));
			}

			{
				auto orbex = stringsToYamlObject(outputs, {"3@ orbex"});

																			tryGetFromYaml(output_orbex,			orbex, {"0@ output"				}, "Output orbex file");
				conditionalPrefix("<OUTPUTS_ROOT>",		orbex_directory,	tryGetFromYaml(orbex_directory,			orbex, {"@ directory"			}, "Output orbex directory"));
				conditionalPrefix("<ORBEX_DIRECTORY>",	orbex_filename,		tryGetFromYaml(orbex_filename,			orbex, {"@ filename"			}, "Output orbex filename"));
																			tryGetEnumVec (orbex_orbit_sources,		orbex, {"@ orbit_sources" 		}, "Sources for orbex orbits");
																			tryGetEnumVec (orbex_clock_sources,		orbex, {"@ clock_sources" 		}, "Sources for orbex clocks");
																			tryGetEnumVec (orbex_attitude_sources,	orbex, {"@ attitude_sources" 	}, "Sources for orbex attitudes");
																			tryGetEnumVec (orbex_record_types,		orbex, {"@ record_types"		}, "List of record types to output to orbex file");
																			tryGetFromYaml(orbex_output_interval,	orbex, {"@ output_interval"		}, "Update interval for orbex records (irregular epoch interval is currently NOT supported)");
			}

			{
				auto cost = stringsToYamlObject(outputs, {"3@ cost"}, docs["cost"]);

																		tryGetFromYaml(output_cost,			cost, {"0@ output"			},	"Enable data exporting to troposphere COST file");
																		tryGetEnumVec (cost_data_sources,	cost, {"@ sources" 			},	"Source for troposphere delay data - KALMAN, etc.");
				conditionalPrefix("<OUTPUTS_ROOT>",		cost_directory,	tryGetFromYaml(cost_directory,		cost, {"@ directory"		},	"Directory to export troposphere COST file"));
				conditionalPrefix("<COST_DIRECTORY>",	cost_filename,	tryGetFromYaml(cost_filename,		cost, {"@ filename"			},	"Troposphere COST filename"));
																		tryGetFromYaml(cost_time_interval,	cost, {"@ time_interval"	},	"Time interval between entries in troposphere COST file (sec)");
																		tryGetFromYaml(cost_format,			cost, {"@ cost_format"		},	"Format name & version number");
																		tryGetFromYaml(cost_project,		cost, {"@ cost_project"		},	"Project name");
																		tryGetFromYaml(cost_status,			cost, {"@ cost_status"		},	"File status");
																		tryGetFromYaml(cost_centre,			cost, {"@ cost_centre"		},	"Processing centre");
																		tryGetFromYaml(cost_method,			cost, {"@ cost_method"		},	"Processing method");
																		tryGetFromYaml(cost_orbit_type,		cost, {"@ cost_orbit_type"	},	"Orbit type");
																		tryGetFromYaml(cost_met_source,		cost, {"@ cost_met_sources"	},	"Source of met. data");
			}

			{
				auto rinex_nav = stringsToYamlObject(outputs, {"5@ rinex_nav"});

																					tryGetFromYaml(output_rinex_nav,		rinex_nav, {"0@ output"		});
				conditionalPrefix("<OUTPUTS_ROOT>",			rinex_nav_directory,	tryGetFromYaml(rinex_nav_directory,		rinex_nav, {"@ directory"	}));
				conditionalPrefix("<RINEX_NAV_DIRECTORY>",	rinex_nav_filename,		tryGetFromYaml(rinex_nav_filename,		rinex_nav, {"@ filename"	}));
																					tryGetFromYaml(rinex_nav_version,		rinex_nav, {"@ version"		});
			}

			{
				auto rinex_obs = stringsToYamlObject(outputs, {"5@ rinex_obs"});

																					tryGetFromYaml(output_rinex_obs,		rinex_obs, {"0@ output"					});
				conditionalPrefix("<OUTPUTS_ROOT>",			rinex_obs_directory,	tryGetFromYaml(rinex_obs_directory,		rinex_obs, {"@ directory"				}));
				conditionalPrefix("<RINEX_OBS_DIRECTORY>",	rinex_obs_filename,		tryGetFromYaml(rinex_obs_filename,		rinex_obs, {"@ filename"				}));
																					tryGetFromYaml(rinex_obs_print_C_code,	rinex_obs, {"@ output_pseudorange"		});
																					tryGetFromYaml(rinex_obs_print_L_code,	rinex_obs, {"@ output_phase_range"		});
																					tryGetFromYaml(rinex_obs_print_D_code,	rinex_obs, {"@ output_doppler"			});
																					tryGetFromYaml(rinex_obs_print_S_code,	rinex_obs, {"@ output_signal_to_noise"	});
																					tryGetFromYaml(rinex_obs_version,		rinex_obs, {"@ version"					});
			}

			{
				auto rtcm_nav = stringsToYamlObject(outputs, {"5@ rtcm_nav"});

																					tryGetFromYaml(record_rtcm_nav,			rtcm_nav, {"0@ output"			});
				conditionalPrefix("<OUTPUTS_ROOT>",			rtcm_nav_directory,		tryGetFromYaml(rtcm_nav_directory,		rtcm_nav, {"@ directory"		}));
				conditionalPrefix("<RTCM_NAV_DIRECTORY>",	rtcm_nav_filename,		tryGetFromYaml(rtcm_nav_filename,		rtcm_nav, {"@ filename"			}));
			}

			{
				auto rtcm_obs = stringsToYamlObject(outputs, {"5@ rtcm_obs"});

																					tryGetFromYaml(record_rtcm_obs,			rtcm_obs, {"0@ output"			});
				conditionalPrefix("<OUTPUTS_ROOT>",			rtcm_obs_directory,		tryGetFromYaml(rtcm_obs_directory,		rtcm_obs, {"@ directory"		}));
				conditionalPrefix("<RTCM_OBS_DIRECTORY>",	rtcm_obs_filename,		tryGetFromYaml(rtcm_obs_filename,		rtcm_obs, {"@ filename"			}));
			}

			{
				auto raw_ubx = stringsToYamlObject(outputs, {"6@ raw_ubx"});

																					tryGetFromYaml(record_raw_ubx,			raw_ubx, {"0 output"			});
				conditionalPrefix("<OUTPUTS_ROOT>",			raw_ubx_directory,		tryGetFromYaml(raw_ubx_directory,		raw_ubx, {"directory"			}));
				conditionalPrefix("<UBX_DIRECTORY>",		raw_ubx_filename,		tryGetFromYaml(raw_ubx_filename,		raw_ubx, {"filename"			}));
			}

			{
				auto raw_custom = stringsToYamlObject(outputs, {"6@ raw_custom"});

																					tryGetFromYaml(record_raw_custom,		raw_custom, {"0 output"			});
				conditionalPrefix("<OUTPUTS_ROOT>",			raw_custom_directory,	tryGetFromYaml(raw_custom_directory,	raw_custom, {"directory"		}));
				conditionalPrefix("<CUSTOM_DIRECTORY>",		raw_custom_filename,	tryGetFromYaml(raw_custom_filename,		raw_custom, {"filename"			}));
			}

			{
				auto slr_obs = stringsToYamlObject(outputs, {"7@ slr_obs"}, docs["slr_obs"]);

																					tryGetFromYaml(output_slr_obs,			slr_obs, {"0@ output"			}, 	"Enable data exporting to tabular SLR obs file");
				conditionalPrefix("<OUTPUTS_ROOT>",			slr_obs_directory,		tryGetFromYaml(slr_obs_directory,		slr_obs, {"@ directory"			}, 	"Directory to export tabular SLR obs file"));
				conditionalPrefix("<SLR_OBS_DIRECTORY>",	slr_obs_filename,		tryGetFromYaml(slr_obs_filename,		slr_obs, {"@ filename"			},	"Tabular SLR obs filename"));
			}

			{
				auto trop_sinex = stringsToYamlObject(outputs, {"3@ trop_sinex"}, docs["trop_sinex"]);

																					tryGetFromYaml(output_trop_sinex,		trop_sinex, {"0@ output"		},	"Enable data exporting to troposphere SINEX file");
																					tryGetEnumVec (trop_sinex_data_sources,	trop_sinex, {"@ sources"		},	"Source for troposphere delay data - KALMAN, etc.");
				conditionalPrefix("<OUTPUTS_ROOT>",			trop_sinex_directory,	tryGetFromYaml(trop_sinex_directory,	trop_sinex, {"@ directory"		},	"Directory to export troposphere SINEX file"));
				conditionalPrefix("<TROP_SINEX_DIRECTORY>",	trop_sinex_filename,	tryGetFromYaml(trop_sinex_filename,		trop_sinex, {"@ filename"		},	"Troposphere SINEX filename"));
																					tryGetFromYaml(trop_sinex_sol_type,		trop_sinex, {"@ sol_type"		},	"Troposphere SINEX solution type");
																					tryGetFromYaml(trop_sinex_obs_code,		trop_sinex, {"@ obs_code"		},	"Troposphere SINEX observation code");
																					tryGetFromYaml(trop_sinex_const_code,	trop_sinex, {"@ const_code"		},	"Troposphere SINEX const code");
																					tryGetFromYaml(trop_sinex_version,		trop_sinex, {"@ version"		},	"Troposphere SINEX version");
			}


// 			ssr_outputs
			{
				auto ssr_outputs = stringsToYamlObject(outputs, {"2@ ssr_outputs"}, docs["ssr_outputs"]);

				tryGetEnumVec (ssrOpts.ephemeris_sources, 		ssr_outputs, {"@ ephemeris_sources" 		}, "Sources for SSR ephemeris");
				tryGetEnumVec (ssrOpts.clock_sources, 			ssr_outputs, {"@ clock_sources" 			}, "Sources for SSR clocks");
				tryGetEnumVec (ssrOpts.code_bias_sources, 		ssr_outputs, {"2@ code_bias_sources" 		}, "Sources for SSR code biases");
				tryGetEnumVec (ssrOpts.phase_bias_sources, 		ssr_outputs, {"2@ phase_bias_sources" 		}, "Sources for SSR phase biases");
				tryGetFromYaml(ssrOpts.prediction_interval,		ssr_outputs, {"@ prediction_interval"		});
				tryGetFromYaml(ssrOpts.prediction_duration,		ssr_outputs, {"@ prediction_duration"		});
				tryGetFromYaml(ssrOpts.extrapolate_corrections,	ssr_outputs, {"@ extrapolate_corrections"	});
				tryGetFromYaml(ssrOpts.cmpssr_cell_mask,		ssr_outputs, {"@ cmpssr_cell_mask"			});
				tryGetFromYaml(ssrOpts.max_stec_sigma,			ssr_outputs, {"@ max_stec_sigma"			});

// 				atmospheric
				{
					auto atmospheric = stringsToYamlObject(ssr_outputs, {"@ atmospheric"}, docs["atmospheric"]);

					tryGetEnumVec (ssrOpts.atmosphere_sources, 	atmospheric, {"@ sources" 				}, "Sources for SSR ionosphere");
					tryGetFromYaml(ssrOpts.region_id, 			atmospheric, {"@ region_id" 			}, "Region ID for atmospheric corrections");
					tryGetFromYaml(ssrOpts.region_iod, 			atmospheric, {"@ region_iod" 			}, "Region IOD for atmospheric corrections (default: -1 for undefined)");
					tryGetFromYaml(ssrOpts.npoly_trop, 			atmospheric, {"@ npoly_trop" 			});
					tryGetFromYaml(ssrOpts.npoly_iono, 			atmospheric, {"@ npoly_iono" 			});
					tryGetFromYaml(ssrOpts.grid_type, 			atmospheric, {"@ grid_type" 			}, "Grid type for gridded atmospheric corrections");
					tryGetFromYaml(ssrOpts.use_grid_iono, 		atmospheric, {"@ use_grid_iono" 		}, "Grid type for gridded atmospheric corrections");
					tryGetFromYaml(ssrOpts.use_grid_trop, 		atmospheric, {"@ use_grid_trop" 		}, "Grid type for gridded atmospheric corrections");
					tryGetFromYaml(ssrOpts.lat_max, 			atmospheric, {"@ lat_max" 				});
					tryGetFromYaml(ssrOpts.lat_min, 			atmospheric, {"@ lat_min" 				});
					tryGetFromYaml(ssrOpts.lat_int, 			atmospheric, {"@ lat_int" 				});
					tryGetFromYaml(ssrOpts.lon_max, 			atmospheric, {"@ lon_max" 				});
					tryGetFromYaml(ssrOpts.lon_min, 			atmospheric, {"@ lon_min" 				});
					tryGetFromYaml(ssrOpts.lon_int, 			atmospheric, {"@ lon_int" 				});
					tryGetFromYaml(ssrOpts.cmpssr_stec_format,	atmospheric, {"@ cmpssr_stec_format"	}, "Format of STEC gridded corrections: 0:4bit(LSB=0.04) , 1:4bit(LSB=0.12), 2:5bit, 3:7bit, 4:16bit");
					tryGetFromYaml(ssrOpts.cmpssr_trop_format,	atmospheric, {"@ cmpssr_trop_format"	}, "Format of Trop. ZWD corrections: 0:8bit, 1:6bit");
				}
			}

			{
				auto streams = stringsToYamlObject(outputs, 		{"2@ streams"});

				tryGetFromYaml(root_stream_url, streams, {"0@ root_url"}, "Root url to be prepended to all other streams specified in this section. If the streams used have individually specified root urls, usernames, or passwords, this should not be used.");

				SsrBroadcast	dummyStreamData;
				tryGetStreamFromYaml(dummyStreamData, streams, {"@ XMPL"});

				auto [outStreamNode, outStreamString] = stringsToYamlObject(streams, {"1@ labels"},		"List of output stream is with further information to be found in its own section, as per XMPL below");

				for (auto outLabelYaml : outStreamNode)
				{
					string outLabel = outLabelYaml.as<string>();

					tryGetStreamFromYaml(netOpts.uploadingStreamData[outLabel], streams, {outLabel});

					conditionalPrefix("<ROOT_STREAM_URL>",	netOpts.uploadingStreamData[outLabel].url);

					replaceTags(netOpts.uploadingStreamData[outLabel].url);
				}
			}
		}

// 		inputs
		{
			auto inputs			= stringsToYamlObject({yaml, ""},	{"0! inputs"		},	docs["inputs"]);
			auto troposphere	= stringsToYamlObject(inputs,		{"2@ troposphere"	},	"Files specifying tropospheric model inputs");
			auto tides			= stringsToYamlObject(inputs,		{"2@ tides"			},	"Files specifying tidal loading and potential inputs");
			auto ionosphere		= stringsToYamlObject(inputs,		{"3@ ionosphere"	},	"Files specifying ionospheric model inputs");

			tryGetFromAny(inputs_root,					commandOpts,	inputs,	{"0! inputs_root"			}, "Root path to be added to all other input files (unless they are absolute)");

			auto getAppendFiles = [&](
				vector<string>&	output,
				NodeStack&		nodeStack,
				const string&	descriptor,
				const string&	comment)
			{
				vector<string> vec;

				tryGetFromAny(vec, commandOpts, nodeStack, {descriptor}, comment);

				conditionalPrefix("<INPUTS_ROOT>", vec);

				output.insert(output.end(), vec.begin(), vec.end());
			};

			getAppendFiles(atx_files						, inputs,			{"4! atx_files"						}, "List of atx files to use");
			getAppendFiles(snx_files						, inputs,			{"4@ snx_files"						}, "List of snx files to use");
			getAppendFiles(erp_files						, inputs,			{"4! erp_files"						}, "List of erp files to use");
			getAppendFiles(igrf_files						, inputs,			{"4@ igrf_files"					}, "List of igrf files to use");
			getAppendFiles(egm_files						, inputs,			{"4@ egm_files"						}, "List of egm files to use");
			getAppendFiles(planetary_ephemeris_files		, inputs,			{"4@ planetary_ephemeris_files"		}, "List of jpl files to use");
			getAppendFiles(cmc_files						, inputs,			{"4@ cmc_files"						}, "List of cmc files to use");
			getAppendFiles(hfeop_files						, inputs,			{"4@ hfeop_files"					}, "List of hfeop files to use");
			getAppendFiles(atm_reg_definitions				, ionosphere,		{"@ atm_reg_definitions"			}, "List of files to define regions for compact SSR");
			getAppendFiles(ion_files						, ionosphere,		{"@ ion_files"						}, "List of IONEX files for VTEC input");
			getAppendFiles(vmf_files						, troposphere,		{"@ vmf_files"						}, "List of vmf files to use");
			getAppendFiles(gpt2grid_files					, troposphere,		{"@ gpt2grid_files"					}, "List of gpt2 grid files to use");
			getAppendFiles(orography_files					, troposphere,		{"@ orography_files"				}, "List of orography files to use");
			getAppendFiles(ocean_tide_potential_files		, tides,			{"@ ocean_tide_potential_files"		}, "List of tide files to use");
			getAppendFiles(atmos_tide_potential_files		, tides,			{"@ atmos_tide_potential_files"		}, "List of tide files to use");
			getAppendFiles(ocean_tide_loading_blq_files		, tides, 			{"@ ocean_tide_loading_blq_files"	}, "List of otl blq files to use");
			getAppendFiles(atmos_tide_loading_blq_files		, tides, 			{"@ atmos_tide_loading_blq_files"	}, "List of atl blq files to use");
			getAppendFiles(ocean_pole_tide_loading_files	, tides, 			{"@ ocean_pole_tide_loading_files"	}, "List of opole files to use");
			getAppendFiles(atmos_oceean_dealiasing_files	, tides,			{"@ atmos_oceean_dealiasing_files"	}, "List of tide files to use");
			getAppendFiles(ocean_pole_tide_potential_files	, tides,			{"@ ocean_pole_tide_potential_files"}, "List of tide files to use");

			tryGetEnumVec (atl_blq_row_order				, tides,			{"@ atl_blq_row_order"				}, "Row order for amplitude and phase components in ATL BLQ files");
			tryGetEnumVec (otl_blq_row_order				, tides,			{"@ otl_blq_row_order"				}, "Row order for amplitude and phase components in OTL BLQ files");

			tryGetEnumVec (atl_blq_col_order				, tides,			{"@ atl_blq_col_order"				}, "Column order for amplitude and phase components in ATL BLQ files");
			tryGetEnumVec (otl_blq_col_order				, tides,			{"@ otl_blq_col_order"				}, "Column order for amplitude and phase components in OTL BLQ files");

			{
				auto gnss_data = stringsToYamlObject(inputs, {"2! gnss_observations"}, "Signal observation data from gnss receivers to be used as measurements");

				conditionalPrefix("<INPUTS_ROOT>", gnss_obs_root,				tryGetFromAny(gnss_obs_root,	commandOpts,	gnss_data,	{"0! gnss_observations_root"	}, "Root path to be added to all other gnss data inputs (unless they are absolute)"));

				tryGetMappedList(rnx_inputs,		commandOpts, gnss_data,					{"1! rnx_inputs"		}, "<GNSS_OBS_ROOT>", "List of rinex      inputs to use");
				tryGetMappedList(ubx_inputs,		commandOpts, gnss_data,					{"1# ubx_inputs"		}, "<GNSS_OBS_ROOT>", "List of ubxfiles   inputs to use");
				tryGetMappedList(custom_inputs,		commandOpts, gnss_data,					{"1# custom_inputs"		}, "<GNSS_OBS_ROOT>", "List of customfiles inputs to use");
				tryGetMappedList(obs_rtcm_inputs,	commandOpts, gnss_data,					{"1! rtcm_inputs"		}, "<GNSS_OBS_ROOT>", "List of rtcmfiles  inputs to use for observations");
			}

			{
				auto pseudo_observation_data = stringsToYamlObject(inputs, {"2@ pseudo_observations"}, "Use data from pre-processed data products as observations. Useful for combining and comparing datasets");

				conditionalPrefix("<INPUTS_ROOT>", pseudo_obs_root,		tryGetFromYaml(pseudo_obs_root,	pseudo_observation_data,	{"0@ pseudo_observations_root"		}, "Root path to be added to all other pseudo obs data files (unless they are absolute)"));

				tryGetMappedList(pseudo_sp3_inputs,		commandOpts,	pseudo_observation_data,	{"@@ sp3_inputs"		}, "<PSEUDO_OBS_ROOT>", "List of sp3 inputs to use for pseudoobservations");
				tryGetMappedList(pseudo_snx_inputs,		commandOpts,	pseudo_observation_data,	{"1@ snx_inputs"		}, "<PSEUDO_OBS_ROOT>", "List of snx inputs to use for pseudoobservations");
				conditionalPrefix("<PSEUDO_OBS_ROOT>", pseudo_filter_files,	tryGetFromAny(pseudo_filter_files,	commandOpts,	pseudo_observation_data,	{"1# filter_files"		}, "List of inputs to use for custom pseudoobservations"));

				tryGetFromYaml(eci_pseudoobs,						pseudo_observation_data,	{"@ eci_pseudoobs"			}, "Pseudo observations are provided in eci frame rather than standard ECEF SP3 files");
			}

			{
				auto satellite_data = stringsToYamlObject(inputs, {"2! satellite_data"});

				conditionalPrefix("<INPUTS_ROOT>",	sat_data_root,		tryGetFromYaml(sat_data_root,							satellite_data,	{"0! satellite_data_root"	}, "Root path to be added to all other satellite data files (unless they are absolute)"));
				conditionalPrefix("<SAT_DATA_ROOT>", nav_files,			tryGetFromAny(nav_files,				commandOpts,	satellite_data, {"1! nav_files"				}, "List of ephemeris  files to use"));
				conditionalPrefix("<SAT_DATA_ROOT>", sp3_files,			tryGetFromAny(sp3_files,				commandOpts,	satellite_data, {"1! sp3_files"				}, "List of sp3        files to use"));
				conditionalPrefix("<SAT_DATA_ROOT>", dcb_files,			tryGetFromAny(dcb_files,				commandOpts,	satellite_data, {"1! dcb_files"				}, "List of dcb        files to use"));
				conditionalPrefix("<SAT_DATA_ROOT>", bsx_files,			tryGetFromAny(bsx_files,				commandOpts,	satellite_data, {"1! bsx_files"				}, "List of biassinex  files to use"));
				conditionalPrefix("<SAT_DATA_ROOT>", clk_files,			tryGetFromAny(clk_files,				commandOpts,	satellite_data, {"1! clk_files"				}, "List of clock      files to use"));
				conditionalPrefix("<SAT_DATA_ROOT>", sid_files,			tryGetFromAny(sid_files,				commandOpts, 	satellite_data, {"2@ sid_files"				}, "List of sat ID     files to use - from https://cddis.nasa.gov/sp3c_satlist.html/"));
				conditionalPrefix("<SAT_DATA_ROOT>", com_files,			tryGetFromAny(com_files,				commandOpts, 	satellite_data, {"2@ com_files"				}, "List of com        files to use - retroreflector offsets from centre-of-mass for spherical sats"));
				conditionalPrefix("<SAT_DATA_ROOT>", crd_files,			tryGetFromAny(crd_files,				commandOpts, 	satellite_data, {"2@ crd_files"				}, "List of crd        files to use - SLR observation data"));
				conditionalPrefix("<SAT_DATA_ROOT>", obx_files,			tryGetFromAny(obx_files,				commandOpts,	satellite_data, {"1! obx_files"				}, "List of orbex      files to use"));

	// 			rtcm_inputs
				{
					auto rtcm_inputs = stringsToYamlObject(satellite_data, {"! rtcm_inputs"}, docs["rtcm_inputs"]);

					conditionalPrefix("<SAT_DATA_ROOT>",	rtcm_inputs_root,	tryGetFromYaml(rtcm_inputs_root,						rtcm_inputs,	{"0! rtcm_inputs_root"	}, "Root path to be added to all other rtcm inputs (unless they are absolute)"));

					conditionalPrefix("<RTCM_INPUTS_ROOT>", nav_rtcm_inputs,	tryGetFromAny(nav_rtcm_inputs,			commandOpts,	rtcm_inputs,	{"1! rtcm_inputs"			}, "List of rtcm       inputs to use for corrections"));
					conditionalPrefix("<RTCM_INPUTS_ROOT>", qzs_rtcm_inputs,	tryGetFromAny(qzs_rtcm_inputs,			commandOpts,	rtcm_inputs,	{"2@ qzl6_inputs"			}, "List of qzss L6    inputs to use for corrections"));

					tryGetFromYaml(ssrInOpts.code_bias_valid_time,	rtcm_inputs, {"@ code_bias_validity_time"	},	"Valid time period of SSR code biases");
					tryGetFromYaml(ssrInOpts.phase_bias_valid_time,	rtcm_inputs, {"@ phase_bias_validity_time"	},	"Valid time period of SSR phase biases");
					tryGetFromYaml(ssrInOpts.one_freq_phase_bias,	rtcm_inputs, {"@ one_freq_phase_bias"		},	"Used stream have one SSR phase bias per frequency");
					tryGetFromYaml(ssrInOpts.global_vtec_valid_time,rtcm_inputs, {"@ global_vtec_valid_time"	},	"Valid time period of global VTEC maps");
					tryGetFromYaml(ssrInOpts.local_stec_valid_time,	rtcm_inputs, {"@ local_stec_valid_time"		},	"Valid time period of local STEC corrections");
					tryGetFromYaml(ssrInOpts.local_trop_valid_time,	rtcm_inputs, {"@ local_trop_valid_time"		},	"Valid time period of local Troposphere corrections");
					tryGetFromYaml(validity_interval_factor,		rtcm_inputs, {"@ validity_interval_factor"	});
					tryGetEnumOpt(ssr_input_antenna_offset,			rtcm_inputs, {"1! ssr_antenna_offset"		},	"Ephemeris type that is provided in the listed SSR stream, i.e. satellite antenna-phase-centre (APC) or centre-of-mass (COM). This information is listed in the NTRIP Caster's sourcetable");
				}

				{
					auto sbas_inputs = stringsToYamlObject(satellite_data, {"! sisnet_inputs"}, "Configuration for SiSNet stream input. SiSNet broadcast SBAS messages");

					conditionalPrefix("<SAT_DATA_ROOT>",	sisnet_inputs_root,	tryGetFromYaml(sisnet_inputs_root,						sbas_inputs,	{"2@ sisnet_inputs_root"	}, "Root path to be added to all other sisnet inputs (unless they are absolute)"));

					conditionalPrefix("<SISNET_INPUTS_ROOT>", sisnet_inputs,	tryGetFromAny(sisnet_inputs,			commandOpts,	sbas_inputs,	{"2@ sisnet_inputs"			}, "List of sisnet inputs to use for corrections"));

					tryGetFromYaml(sbsInOpts.prn,	sbas_inputs, {"@ sbas_prn"					},	"PRN for SBAS satelite");
					tryGetFromYaml(sbsInOpts.freq,	sbas_inputs, {"@ sbas_carrier_frequency"	},	"Carrier frequency of SBAS channel");
				}
			}
		}

// 		processing_options
		{
			auto processing_options = stringsToYamlObject({ yaml, "" }, {processing_options_str}, "Various sections and parameters to specify how the observations are processed");

// 			process_modes
			{
				auto process_modes = stringsToYamlObject(processing_options, {"1! process_modes"}, "Aspects of the processing flow may be enabled and disabled according to desired type of solutions");

				tryGetFromYaml(process_ionosphere,			process_modes, {"@ ionosphere"		}, "Compute Ionosphere models based on GNSS measurements");
				tryGetFromYaml(process_preprocessor,		process_modes, {"! preprocessor"	}, "Preprocessing and quality checks");
				tryGetFromYaml(process_spp,					process_modes, {"! spp"				}, "Perform SPP on receiver data");
				tryGetFromYaml(process_ppp,					process_modes, {"! ppp"				}, "Perform PPP network or end user mode");
				tryGetFromYaml(slrOpts.process_slr,			process_modes, {"@ slr"				}, "Process SLR observations");
			}

// 			gnss_general
			{
				auto general = stringsToYamlObject(processing_options, {"0! gnss_general"}, "Options to specify the processing of gnss observations");

				tryGetFromYaml	(require_apriori_positions,					general, {"@ require_apriori_positions" 	}, "Restrict processing to receivers that have apriori positions available");
				tryGetFromYaml	(require_site_eccentricity,					general, {"@ require_site_eccentricity" 	}, "Restrict processing to receivers that have site eccentricity information");
				tryGetFromYaml	(require_sinex_data,						general, {"@ require_sinex_data" 			}, "Restrict processing to receivers that have sinex data available");
				tryGetFromYaml	(require_antenna_details,					general, {"@ require_antenna_details" 		}, "Restrict processing to receivers that have antenna details");
				tryGetFromYaml	(require_reflector_com,						general, {"@ require_reflector_com" 		}, "Restrict processing to SLR observations that have center of mass to laser retroreflector array offsets");
				tryGetFromYaml	(pivot_receiver,							general, {"@ pivot_receiver" 				}, "Largely deprecated id of receiver to use for pivot constraints");
				tryGetFromYaml	(pivot_satellite,							general, {"@ pivot_satellite" 				}, "Largely deprecated id of satellite to use for pivot constraints");
				tryGetFromYaml	(interpolate_rec_pco,						general, {"@ interpolate_rec_pco" 			}, "Interpolate other known pco values to find pco for unknown frequencies");
				tryGetFromYaml	(auto_fill_pco,								general, {"@ auto_fill_pco" 				}, "Use similar PCOs when requested values are not found");
				tryGetFromYaml	(pppOpts.equate_ionospheres,				general, {"@ equate_ionospheres"			}, "Use same STEC values for different receivers, useful for simulated rtk mode");
				tryGetFromYaml	(pppOpts.equate_tropospheres,				general, {"@ equate_tropospheres"			}, "Use same troposphere values for different receivers, useful for simulated rtk mode");
				tryGetFromYaml	(pppOpts.use_rtk_combo,						general, {"@ use_rtk_combo"					}, "Combine applicable observations to simulate an rtk solution");
				tryGetFromYaml	(pppOpts.add_eop_component,					general, {"@ add_eop_component"				}, "Add eop adjustments as a component in residual chain (for adjusting frames to match ecef ephemeris)");
				tryGetFromYaml	(delete_old_ephemerides,					general, {"@ delete_old_ephemerides"		}, "Remove old ephemerides that have accumulated over time from before far before the currently processing epoch");
				tryGetFromYaml	(use_tgd_bias,								general, {"@ use_tgd_bias"					}, "Use TGD/BGD bias from ephemeris, DO NOT turn on unless using Klobuchar/NeQuick Ionospheres");
				tryGetFromYaml	(common_sat_pco,							general, {"@ common_sat_pco"				}, "Use L1 satellite PCO values for all signals");
				tryGetFromYaml	(common_rec_pco,							general, {"@ common_rec_pco"				}, "Use L1 receiver PCO values for all signals");
				tryGetFromYaml	(leap_seconds,								general, {"@ gpst_utc_leap_seconds"			}, "Difference between gps time and utc in leap seconds");

				tryGetFromYaml	(process_meas[CODE],						general, {"1@ code_measurements",		"process"	}, "Process code measurements");
				tryGetFromYaml	(process_meas[PHAS],						general, {"1@ phase_measurements",		"process"	}, "Process phase measurements");

				tryGetFromYaml	(fixed_phase_bias_var,						general, {"@ fixed_phase_bias_var"			}, "Variance of phase bias to be considered fixed/binded");
				tryGetFromYaml	(adjust_rec_clocks_by_spp,					general, {"@ adjust_rec_clocks_by_spp"		}, "Adjust receiver clocks by spp values to minimise prefit residuals");
				tryGetFromYaml	(adjust_clocks_for_jumps_only,				general, {"@ adjust_clocks_for_jumps_only"	}, "Round clock adjustments from SPP to half milliseconds");
				tryGetFromYaml	(minimise_sat_clock_offsets,				general, {"@ minimise_sat_clock_offsets"	}, "Apply gauss-markov mu values to satellite clocks to minimise offsets with respect to broadcast values");
				tryGetFromYaml	(minimise_sat_orbit_offsets,				general, {"@ minimise_sat_orbit_offsets"	}, "Apply gauss-markov mu values to satellite orbits to minimise offsets with respect to broadcast values");
				tryGetFromYaml	(minimise_ionosphere_offsets,				general, {"@ minimise_ionosphere_offsets"	}, "Apply gauss-markov mu values to stec values to minimise offsets with respect to klobuchar values");

				for (int i = E_Sys::GPS; i < E_Sys::SUPPORTED; i++)
				{
					E_Sys	sys			= E_Sys::_values()[i];

					auto sys_options = stringsToYamlObject(general, {"1! sys_options", sys._to_string()}, (string)"Options for the " + sys._to_string() + " constellation");

					tryGetFromYaml(process_sys			[sys],		sys_options, {"0! process"				}, "Process this constellation");
					tryGetFromYaml(solve_amb_for		[sys],		sys_options, {"3! ambiguity_resolution"	}, "Solve carrier phase ambiguities for this constellation");
					tryGetFromYaml(reject_eclipse		[sys],		sys_options, {"2@ reject_eclipse"		}, "Exclude satellites that are in eclipsing region");
					tryGetFromYaml(receiver_amb_pivot	[sys],		sys_options, {"2@ receiver_amb_pivot"	}, "Constrain: set of ambiguities, to eliminate receiver rank deficiencies");
					tryGetFromYaml(network_amb_pivot	[sys],		sys_options, {"2@ network_amb_pivot"	}, "Constrain: set of ambiguities, to eliminate network  rank deficiencies");
					tryGetFromYaml(use_for_iono_model	[sys],		sys_options, {"2@ use_for_iono_model"	}, "Use this constellation as part of Ionospheric model");
					tryGetFromYaml(use_iono_corrections	[sys],		sys_options, {"2@ use_iono_corrections"	}, "Use external ionosphere delay estimation for this constellation");
					tryGetEnumOpt( used_nav_types		[sys],		sys_options, {"2@ used_nav_type"		});
					tryGetEnumVec (code_priorities		[sys], 		sys_options, {"2! code_priorities" 		}, "List of observation codes to use in processing");
				}
			}

// 			epoch_control
			{
				auto epoch_control = stringsToYamlObject(processing_options, {"0! epoch_control"}, "Specifies the rate and duration of data processing");

				int i = 0;

				string startStr;
				string stopStr;
				bool found =	tryGetFromAny(epoch_interval,		commandOpts,	epoch_control, {"! epoch_interval"		}, "Desired time step between each processing epoch");
								tryGetFromAny(epoch_tolerance,		commandOpts,	epoch_control, {"@ epoch_tolerance"		}, "Tolerance of times to add to an epoch (usually half of the original data's sample rate)");
								tryGetFromAny(max_epochs,			commandOpts,	epoch_control, {"! max_epochs"			}, "Maximum number of epochs to process");
								tryGetFromAny(startStr,				commandOpts,	epoch_control, {"! start_epoch"			}, "(YYYY-MM-DD hh:mm:ss) The time of the first epoch to process (all observations before this will be skipped)");
								tryGetFromAny(stopStr,				commandOpts,	epoch_control, {"! end_epoch"			}, "(YYYY-MM-DD hh:mm:ss) The time of the last epoch to process (all observations after this will be skipped)");

				if (!startStr.empty())	start_epoch	= boost::posix_time::time_from_string(startStr);
				if (!stopStr .empty())	end_epoch	= boost::posix_time::time_from_string(stopStr);

				if (found)
					wait_next_epoch = epoch_interval + 0.05;

								tryGetFromYaml(sleep_milliseconds,					epoch_control, {"# sleep_milliseconds"	}, "Time to sleep before checking for new data - lower numbers are associated with high idle cpu usage");
								tryGetFromYaml(wait_next_epoch,						epoch_control, {"@ wait_next_epoch"		}, "Time to wait for next epochs data before skipping the epoch (will default to epoch_interval as an appropriate minimum value for realtime)");
								tryGetFromYaml(max_rec_latency,						epoch_control, {"@ max_rec_latency"		}, "Time to wait from the reception of the first data of an epoch before skipping receivers with data still unreceived");
								tryGetFromYaml(require_obs,							epoch_control, {"@ require_obs"			}, "Exit the program if no observation sources are available");
								tryGetFromYaml(assign_closest_epoch,				epoch_control, {"@ assign_closest_epoch"}, "Assign observations to the closest epoch - don't skip observations that fall between epochs");
								tryGetFromAny(simulate_real_time,	commandOpts,	epoch_control, {"@ simulate_real_time"	}, "For RTCM playback - delay processing to match original data rate");
			}


// 			model_error_handling
			{
				auto model_error_handling = stringsToYamlObject(processing_options, {"5! model_error_handling"}, "The kalman filter is capable of automatic statistical integrity modelling");

				{
					auto meas_deweighting = stringsToYamlObject(model_error_handling, {"0! meas_deweighting"}, "Measurements that are outside the expected confidence bounds may be deweighted so that outliers do not contaminate the filtered solution");

					tryGetFromYaml(measErrors.enable,				meas_deweighting,	{"! enable"				}, "Enable deweighting of all rejected measurement");
					tryGetFromYaml(measErrors.deweight_factor,		meas_deweighting,	{"! deweight_factor"	}, "Factor to downweight the variance of measurements with statistically detected errors");
				}

				{
					auto state_deweighting = stringsToYamlObject(model_error_handling, {"0! state_deweighting"}, "Any \"state\" errors cause deweighting of all measurements that reference the state");

					tryGetFromYaml(stateErrors.enable,				state_deweighting,	{"! enable"				}, "Enable deweighting of all referencing measurements");
					tryGetFromYaml(stateErrors.deweight_factor,		state_deweighting,	{"! deweight_factor"	}, "Factor to downweight the variance of measurements with statistically detected errors");
				}

				{
					auto error_accumulation = stringsToYamlObject(model_error_handling, {"0! error_accumulation"}, "Any receivers that are consistently getting many measurement rejections may be reinitialiased");

					tryGetFromYaml(errorAccumulation.enable,							error_accumulation,	{"! enable"								}, "Enable reinitialisation of receivers upon many rejections");
					tryGetFromYaml(errorAccumulation.receiver_error_count_threshold,	error_accumulation,	{"! receiver_error_count_threshold"		}, "Number of errors for a receiver to be considered in error for a single epoch");
					tryGetFromYaml(errorAccumulation.receiver_error_epochs_threshold,	error_accumulation,	{"! receiver_error_epochs_threshold"	}, "Number of consecutive epochs with receiver in error before it is removed and reinitialised");
				}



				{
					auto orbit_errors = stringsToYamlObject(model_error_handling, {"2@ orbit_errors"}, "Orbital states that are not consistent with measurements may be reinitialised to allow for dynamic maneuvers");

					tryGetFromYaml(orbErrors.enable,					orbit_errors,	{"@ enable"							}, "Enable applying process noise impulses to orbits upon state errors");
					tryGetFromYaml(orbErrors.pos_proc_noise,			orbit_errors,	{"@ pos_process_noise"				}, "Sigma to apply to orbital position states as reinitialisation");
					tryGetFromYaml(orbErrors.vel_proc_noise,			orbit_errors,	{"@ vel_process_noise"				}, "Sigma to apply to orbital velocity states as reinitialisation");
					tryGetFromYaml(orbErrors.vel_proc_noise_trail,		orbit_errors,	{"@ vel_process_noise_trail"		}, "Initial sigma for exponentially decaying noise to apply for subsequent epochs as soft reinitialisation");
					tryGetFromYaml(orbErrors.vel_proc_noise_trail_tau,	orbit_errors,	{"@ vel_process_noise_trail_tau"	}, "Time constant for exponentially decauing noise");
				}

				{
					auto ambiguities = stringsToYamlObject(model_error_handling, {"1! ambiguities"}, "Cycle slips in ambiguities are primary cause of incorrect gnss modelling and may be reinitialised");

					tryGetFromYaml(ambErrors.outage_reset_limit,	ambiguities,	{"! outage_reset_limit"		}, "Maximum number of seconds without phase measurements before the ambiguity associated with the measurement is reset.");
					tryGetFromYaml(ambErrors.phase_reject_limit,	ambiguities,	{"! phase_reject_limit"		}, "Maximum number of phase measurements to reject before the ambiguity associated with the measurement is reset.");

					tryGetFromYaml(ambErrors.resetOnSlip.LLI,		ambiguities, {"@ reset_on",		"@ lli"		}, "Reset ambiguities if LLI   test is detecting a slip");
					tryGetFromYaml(ambErrors.resetOnSlip.GF,		ambiguities, {"@ reset_on",		"@ gf"		}, "Reset ambiguities if GF    test is detecting a slip");
					tryGetFromYaml(ambErrors.resetOnSlip.MW,		ambiguities, {"@ reset_on",		"@ mw"		}, "Reset ambiguities if MW    test is detecting a slip");
					tryGetFromYaml(ambErrors.resetOnSlip.SCDIA,		ambiguities, {"@ reset_on",		"@ scdia"	}, "Reset ambiguities if SCDIA test is detecting a slip");
				}

				{
					auto ionospheric_components = stringsToYamlObject(model_error_handling, {"1! ionospheric_components"});

					tryGetFromYaml(ionErrors.outage_reset_limit,	ionospheric_components,	{"! outage_reset_limit"		}, "Maximum number of seconds without measurements before the ionosphere associated with the measurement is reset.");
				}


				{
					auto exclusions = stringsToYamlObject(model_error_handling, {"1@ exclusions"}, "Cycle slips may be detected by the preprocessor and measurements rejected or ambiguities reinitialised");

					tryGetFromYaml(exclude.bad_spp,		exclusions, {"@ bad_spp"	}, "Exclude measurements that were associated with failed SPP");
					tryGetFromYaml(exclude.config,		exclusions, {"@ config"		}, "Exclude measurements that are configured as exclusions");
					tryGetFromYaml(exclude.eclipse,		exclusions, {"@ eclipse"	}, "Exclude measurements that are in eclipse");
					tryGetFromYaml(exclude.elevation,	exclusions, {"@ elevation"	}, "Exclude measurements that fall below elevation mask");
					tryGetFromYaml(exclude.outlier,		exclusions, {"@ outlier"	}, "Exclude measurements that were rejected as SPP outliers");
					tryGetFromYaml(exclude.system,		exclusions, {"@ system"		}, "Exclude measurements that have been excluded by system configs");
					tryGetFromYaml(exclude.svh, 		exclusions, {"@ svh"		}, "Exclude measurements that are not specified as healthy");
					tryGetFromYaml(exclude.LLI,			exclusions, {"@ lli"		}, "Exclude measurements that fail LLI slip test in preprocessor");
					tryGetFromYaml(exclude.GF,			exclusions, {"@ gf"			}, "Exclude measurements that fail GF  slip test in preprocessor");
					tryGetFromYaml(exclude.MW,			exclusions, {"@ mw"			}, "Exclude measurements that fail MW  slip test in preprocessor");
					tryGetFromYaml(exclude.SCDIA,		exclusions, {"@ scdia"		}, "Exclude measurements that fail SCDIA    test in preprocessor");
				}

// 				{
// 					auto clocks = stringsToYamlObject(model_error_handling, {"@ clocks"}, "Error responses specific to clock states");
//
// 					tryGetFromYaml(reinit_on_clock_error,		clocks,			{"@ reinit_on_clock_error"	}, "Any clock \"state\" errors cause removal and reinitialisation of the clocks and all associated ambiguities");
// 				}
			}

			auto getFilterOptions = [&](
				NodeStack&		nodeStack,
				FilterOptions&	filterOpts)
			{
				auto outlier_screening	= stringsToYamlObject(nodeStack,			{"! outlier_screening"},	"Statistical checks allow for detection of outliers that exceed their confidence intervals.");

				if (std::get<1>(nodeStack).find("spp") == string::npos)
				{
					tryGetFromYaml(filterOpts.joseph_stabilisation,			nodeStack,			{"@ joseph_stabilisation"						});
					tryGetEnumOpt( filterOpts.inverter, 					nodeStack,			{"@ inverter" 									}, "Inverter to be used within the Kalman filter update stage, which may provide different performance outcomes in terms of processing time and accuracy and stability.");
					tryGetFromYaml(filterOpts.advanced_postfits,			nodeStack,			{"# advanced_postfits"							}, "Use alternate calculation method to determine postfit residuals");
				}

				{
					auto prefit				= stringsToYamlObject(outlier_screening,	{"! prefit"});

									tryGetFromYaml(filterOpts.prefitOpts.max_iterations,			prefit,				{"! max_iterations"			},	"Maximum number of measurements to exclude using prefit checks before attempting to filter");
									tryGetFromYaml(filterOpts.prefitOpts.sigma_check,				prefit,				{"@ sigma_check"			},	"Enable sigma check");
					bool found =	tryGetFromYaml(filterOpts.prefitOpts.state_sigma_threshold,		prefit,				{"@ sigma_threshold"		},	"Sigma threshold");
									tryGetFromYaml(filterOpts.prefitOpts.meas_sigma_threshold,		prefit,				{"@ sigma_threshold"		},	"Sigma threshold");
									tryGetFromYaml(filterOpts.prefitOpts.state_sigma_threshold,		prefit,				{"@ state_sigma_threshold"	},	"Sigma threshold for states");
									tryGetFromYaml(filterOpts.prefitOpts.meas_sigma_threshold,		prefit,				{"@ meas_sigma_threshold"	},	"Sigma threshold for measurements");
									tryGetFromYaml(filterOpts.prefitOpts.omega_test,				prefit,				{"@ omega_test"				},	"Enable omega-test");

					if (found)
					{
						BOOST_LOG_TRIVIAL(warning) << "Warning: the yaml option 'prefit:sigma_threshold' is depreciated, better use 'prefit:state_sigma_threshold' and 'prefit:meas_sigma_threshold' instead";
					}
				}

				{
					auto postfit			= stringsToYamlObject(outlier_screening,	{"! postfit"});

									tryGetFromYaml(filterOpts.postfitOpts.max_iterations,			postfit,			{"! max_iterations"			},	"Maximum number of measurements to exclude using postfit checks while iterating filter");
									tryGetFromYaml(filterOpts.postfitOpts.sigma_check,				postfit,			{"@ sigma_check"			},	"Enable sigma check");
					bool found =	tryGetFromYaml(filterOpts.postfitOpts.state_sigma_threshold,	postfit,			{"@ sigma_threshold"		},	"Sigma threshold");
									tryGetFromYaml(filterOpts.postfitOpts.meas_sigma_threshold,		postfit,			{"@ sigma_threshold"		},	"Sigma threshold");
									tryGetFromYaml(filterOpts.postfitOpts.state_sigma_threshold,	postfit,			{"@ state_sigma_threshold"	},	"Sigma threshold for states");
									tryGetFromYaml(filterOpts.postfitOpts.meas_sigma_threshold,		postfit,			{"@ meas_sigma_threshold"	},	"Sigma threshold for measurements");
									tryGetFromYaml(filterOpts.chiSquareTest.sigma_threshold,		postfit,			{"@ sigma_threshold"		},	"Sigma threshold");

					if (found)
					{
						BOOST_LOG_TRIVIAL(warning) << "Warning: the yaml option 'postfit:sigma_threshold' is depreciated, better use 'postfit:state_sigma_threshold' and 'postfit:meas_sigma_threshold' instead";
					}
				}

				{
					auto chi_sqaure			= stringsToYamlObject(outlier_screening,	{"! chi_square"});

									tryGetFromYaml(filterOpts.chiSquareTest.enable,					chi_sqaure,			{"@ enable"					},	"Enable Chi-square test");
									tryGetEnumOpt( filterOpts.chiSquareTest.mode,					chi_sqaure,			{"@ mode"					},	"Chi-square test mode");
									tryGetFromYaml(filterOpts.chiSquareTest.sigma_threshold,		chi_sqaure,			{"@ sigma_threshold"		},	"Chi-square test threshold in terms of 'times of sigma'");
				}

				if (std::get<1>(nodeStack).find("spp") == string::npos)
				{
					auto rts				= stringsToYamlObject(nodeStack,			{"@ rts"},					"RTS allows reverse smoothing of estimates such that early estimates can make use of later data.");

																					tryGetFromYaml(process_rts,							rts,				{"0!  enable"				}, "Perform backward smoothing of states to improve precision of earlier states");
																					tryGetFromYaml(filterOpts.rts_lag,					rts,				{"@ 1 lag"					}, "Number of epochs to use in RTS smoothing. Negative numbers indicate full reverse smoothing.");
																					tryGetFromYaml(filterOpts.rts_interval,				rts,				{"# interval"				}, "Number of seconds to use between fixed lag in RTS smoothing.");
					conditionalPrefix("<OUTPUTS_ROOT>",		pppOpts.rts_directory,	tryGetFromYaml(filterOpts.rts_directory,			rts,				{"@ directory"				}, "Directory for rts intermediate files"));
					conditionalPrefix("<RTS_DIRECTORY>",	pppOpts.rts_filename,	tryGetFromYaml(filterOpts.rts_filename,				rts,				{"@ filename"				}, "Base filename for rts intermediate files"));
																					tryGetFromYaml(filterOpts.queue_rts_outputs,		rts,				{"@ queue_outputs"			}, "Queue rts outputs so that processing is not limited by IO bandwidth");
																					tryGetFromYaml(filterOpts.rts_smoothed_suffix,		rts,				{"@ suffix"					}, "Suffix to be applied to smoothed versions of files");
																					tryGetEnumOpt( filterOpts.rts_inverter, 			rts,				{"@ inverter" 				}, "Inverter to be used within the rts processor, which may provide different performance outcomes in terms of processing time and accuracy and stability.");
																					tryGetFromYaml(filterOpts.output_intermediate_rts,	rts,				{"@ output_intermediates"	}, "Output best available smoothed states when performing fixed-lag rts (slow, use only when needed)");
				}
			};

// 			minimum_constraints
			{
				auto minimum_constraints = stringsToYamlObject(processing_options, {"5! minimum_constraints"}, "Receiver coodinates may be aligned to reference frames with minimal external constraints");

				tryGetFromYaml(process_minimum_constraints,		minimum_constraints,	{"0! enable"					}, "Transform states by minimal constraints to selected receiver coordinates");

				tryGetKalmanFromYaml(minconOpts.delay,			minimum_constraints,	"1! delay",						"Estimation and application of clock delay adjustment");
				tryGetKalmanFromYaml(minconOpts.scale,			minimum_constraints,	"1! scale",						"Estimation and application of scaling factor");
				tryGetKalmanFromYaml(minconOpts.rotation,		minimum_constraints,	"1! rotation",					"Estimation and application of angular offsets");
				tryGetKalmanFromYaml(minconOpts.translation,	minimum_constraints,	"1! translation",				"Estimation and application of CoG offsets");

				tryGetFromYaml(minconOpts.once_per_epoch,		minimum_constraints,	{"2@ once_per_epoch"		},	"Perform minimum constraints on a temporary filter and output results once per epoch");
				tryGetFromYaml(minconOpts.full_vcv,				minimum_constraints,	{"2@ full_vcv"				},	"! experimental ! Use full VCV for measurement noise in minimum constraints filter");
				tryGetFromYaml(minconOpts.constrain_orbits,		minimum_constraints,	{"2@ constrain_orbits"		},	"Enforce rigid transformations of orbital states");
				tryGetEnumOpt( minconOpts.application_mode,		minimum_constraints,	{"2@ application_mode"		},	"Method of transforming positions ");
				tryGetFromYaml(minconOpts.transform_unweighted,	minimum_constraints,	{"2@ transform_unweighted"	},	"Add design entries for transformation of positions without weighting");

				getFilterOptions(minimum_constraints, minconOpts);
			}

// 			ppp_filter
			{
				auto ppp_filter = stringsToYamlObject(processing_options, {"4! ppp_filter"}, "Configurations for the kalman filter and its sub processes");

				tryGetFromYaml	(pppOpts.simulate_filter_only,	ppp_filter,	{"@ simulate_filter_only"				}, "Residuals will be calculated, but no adjustments to state or covariances will be applied");
				tryGetFromYaml	(pppOpts.assume_linearity,		ppp_filter,	{"@ assume_linearity"					}, "Residuals will be adjusted during measurement combination rather than performing 2 seperate state transitions");

				tryGetFromYaml	(pppOpts.chunk_size,			ppp_filter,	{"@ chunking", "@ size"					});
				tryGetFromYaml	(pppOpts.receiver_chunking,		ppp_filter,	{"@ chunking", "@ by_receiver"			}, "Split large filter and measurement matrices blockwise by receiver ID to improve processing speed");
				tryGetFromYaml	(pppOpts.satellite_chunking,	ppp_filter,	{"@ chunking", "@ by_satellite"			}, "Split large filter and measurement matrices blockwise by satellite ID to improve processing speed");

				tryGetFromYaml	(pppOpts.nuke_enable,			ppp_filter,	{"@ periodic_reset", "@ enable"			}, "Enable periodic reset of filter states");
				tryGetFromYaml	(pppOpts.nuke_interval,			ppp_filter,	{"@ periodic_reset", "@ interval"		}, "Interval between reset of filter states");
				tryGetEnumVec	(pppOpts.nuke_states,			ppp_filter,	{"@ periodic_reset", "@ states"			}, "States to remove for periodic reset");

// 				ionospheric_component
				{
					auto ionospheric_components = stringsToYamlObject(ppp_filter, {"! ionospheric_components"}, "Slant ionospheric components");

					tryGetEnumOpt( pppOpts.ionoOpts.corr_mode, 				ionospheric_components, {"@ corr_mode" 						});
					tryGetFromYaml(pppOpts.ionoOpts.common_ionosphere,		ionospheric_components, {"! common_ionosphere"				}, "Use the same ionosphere state for code and phase observations");
					tryGetFromYaml(pppOpts.ionoOpts.use_if_combo,			ionospheric_components, {"! use_if_combo"					}, "Combine 'uncombined' measurements to simulate an ionosphere-free solution");
					tryGetFromYaml(pppOpts.ionoOpts.use_gf_combo,			ionospheric_components, {"! use_gf_combo"					}, "Combine 'uncombined' measurements to simulate a geometry-free solution");
				}

				getFilterOptions(ppp_filter, pppOpts);
			}

// 			ion_filter
			{
				auto ion_filter = stringsToYamlObject(processing_options, {"5@ ion_filter"}, "Configurations for the ionospheric model kalman filter and its sub processes");

				tryGetEnumOpt( ionModelOpts.model, 				ion_filter, {"@ model" 				});
				tryGetFromYaml(ionModelOpts.function_order,		ion_filter, {"@ function_order"		}, "Maximum order  of Spherical harmonics for Ionospheric mapping");
				tryGetFromYaml(ionModelOpts.function_degree,	ion_filter, {"@ function_degree"	}, "Maximum degree of Spherical harmonics for Ionospheric mapping");
				tryGetFromYaml(ionModelOpts.estimate_sat_dcb,	ion_filter, {"@ estimate_sat_dcb"	}, "Estimate satellite dcb alongside Ionosphere models, should be false for local STEC");
				tryGetFromYaml(ionModelOpts.use_rotation_mtx,	ion_filter, {"@ use_rotation_mtx"	}, "Use 3D rotation matrix for spherical harmonics to maintain orientation toward the sun");
				tryGetFromYaml(ionModelOpts.basis_sigma_limit,	ion_filter, {"@ model_sigma_limit"	}, "Ionosphere states are removed when their sigma exceeds this value");

				bool found = tryGetFromYaml(ionModelOpts.layer_heights,		ion_filter, {"@ layer_heights"			}, "List of heights of ionosphere layers to estimate");
				if (found)
				for (auto& a : ionModelOpts.layer_heights)
				{
					a *= 1000; //km to m
				}

				getFilterOptions(ion_filter, ionModelOpts);
			}


// 			spp
			{
				auto spp = stringsToYamlObject(processing_options, {"1! spp"}, "Configurations for the kalman filter and its sub processes");

				tryGetFromYaml(sppOpts.max_lsq_iterations,		spp,		{"! max_lsq_iterations"		},	"Maximum number of iterations of least squares allowed for convergence");
				tryGetFromYaml(sppOpts.sigma_scaling,			spp,		{"! sigma_scaling"			},	"Scale applied to measurement noise for spp");
				tryGetFromYaml(sppOpts.always_reinitialise,		spp,		{"@ always_reinitialise"	},	"Reset SPP state to zero to avoid potential for lock-in of bad states");
				tryGetEnumOpt( sppOpts.iono_mode, 				spp,		{"@ iono_mode" 				});

				auto outlier_screening = stringsToYamlObject(spp, {"! outlier_screening"}, "Statistical checks allow for detection of outliers that exceed their confidence intervals.");

				tryGetFromYaml(sppOpts.max_gdop,				outlier_screening, {"@ max_gdop"				}, "Maximum dilution of precision before error is flagged");
				tryGetFromYaml(sppOpts.raim,					outlier_screening, {"@ raim"					}, "Enable Receiver Autonomous Integrity Monitoring. When SPP fails further SPP solutions are calculated with subsets of observations with the aim of eliminating a problem satellite");

				getFilterOptions(spp, sppOpts);
			}

// 			preprocessor
			{
				auto preprocessor = stringsToYamlObject(processing_options, {"1@ preprocessor"}, "Configurations for the kalman filter and its sub processes");

				tryGetFromYaml(preprocOpts.preprocess_all_data,			preprocessor,	{"@ preprocess_all_data"							});
				{
					auto cycle_slips = stringsToYamlObject(preprocessor, {"2@ cycle_slips"}, "Cycle slips may be detected by the preprocessor and measurements rejected or ambiguities reinitialised");

					tryGetFromYaml(preprocOpts.slip_threshold,		cycle_slips, {"@ slip_threshold"			}, "Value used to determine when a slip has occurred");
					tryGetFromYaml(preprocOpts.mw_proc_noise,		cycle_slips, {"@ mw_process_noise"			}, "Process noise applied to filtered Melbourne-Wubenna measurements to detect cycle slips");
				}
			}

// 				tryGetFromYaml(orbitOpts.degree_max,				orbit_propagation, {"@ degree_max"					}, "Maximum degree of spherical harmonics model");
// 			ambiguity_resolution
			{
				auto ambiguity_resolution = stringsToYamlObject(processing_options, {"5@ ambiguity_resolution"});

				tryGetFromYaml(ambrOpts.elevation_mask_deg,	ambiguity_resolution, {"@ elevation_mask"				}, "Minimum satellite elevation to perform ambiguity resolution");
				tryGetFromYaml(ambrOpts.lambda_set,			ambiguity_resolution, {"@ lambda_set_size"				}, "Maximum numer of candidate sets to be used in lambda_alt2 and lambda_bie modes");
				tryGetFromYaml(ambrOpts.AR_max_itr,			ambiguity_resolution, {"@ max_rounding_iterations"		}, "Maximum number of rounding iterations performed in iter_rnd and bootst modes");

				tryGetEnumOpt( ambrOpts.mode,				ambiguity_resolution, {"@ mode" 						});
				tryGetFromYaml(ambrOpts.succsThres,			ambiguity_resolution, {"@ success_rate_threshold"		}, "Thresold for integer validation, success rate test.");
				tryGetFromYaml(ambrOpts.ratioThres,			ambiguity_resolution, {"@ solution_ratio_threshold"		}, "Thresold for integer validation, distance ratio test.");

				tryGetFromYaml(ambrOpts.once_per_epoch,		ambiguity_resolution, {"@ once_per_epoch"				},	"Perform ambiguity resolution on a temporary filter and output results once per epoch");
				tryGetFromYaml(ambrOpts.fix_and_hold,		ambiguity_resolution, {"@ fix_and_hold"					},	"Perform ambiguity resolution and commit results to the main processing filter");
			}


// 			predictions
			{
				auto predictions = stringsToYamlObject(processing_options, {"5@ predictions"});

				tryGetScaledFromYaml(mongoOpts.prediction_offset,			predictions, {"4@ offset"			},	{"@ interval_units"	},	E_Period::_from_string_nocase);
				tryGetScaledFromYaml(mongoOpts.prediction_interval,			predictions, {"4@ interval"			},	{"@ interval_units"	},	E_Period::_from_string_nocase);
				tryGetScaledFromYaml(mongoOpts.forward_prediction_duration,	predictions, {"4@ forward_duration"	},	{"@ duration_units"	},	E_Period::_from_string_nocase);
				tryGetScaledFromYaml(mongoOpts.reverse_prediction_duration,	predictions, {"4@ reverse_duration"	},	{"@ duration_units"	},	E_Period::_from_string_nocase);
			}


// 			orbit_propagation
			{
				auto orbit_propagation = stringsToYamlObject(processing_options, {"5@ orbit_propagation"});

				tryGetFromYaml(propagationOptions.integrator_time_step		, orbit_propagation,	{"@ integrator_time_step"		}, "Timestep for the integrator, must be smaller than the processing time step, might be adjusted if the processing time step isn't a integer number of time steps");
				tryGetFromYaml(propagationOptions.egm_degree				, orbit_propagation,	{"@ egm_degree"					}, "Degree of spherical harmonics gravity model");
				tryGetFromYaml(propagationOptions.indirect_J2				, orbit_propagation,	{"@ indirect_J2"				}, "J2 acceleration perturbation due to the Sun and Moon");
				tryGetFromYaml(propagationOptions.egm_field					, orbit_propagation,	{"@ egm_field"					}, "Acceleration due to the high degree model of the Earth gravity model (exclude degree 0, made by central_force)");
				tryGetFromYaml(propagationOptions.solid_earth_tide			, orbit_propagation,	{"@ solid_earth_tide"			}, "Model accelerations due to solid earth tides");
				tryGetFromYaml(propagationOptions.ocean_tide				, orbit_propagation,	{"@ ocean_tide"					}, "Model accelerations due to ocean tides model");
				tryGetFromYaml(propagationOptions.atm_tide					, orbit_propagation,	{"@ atm_tide"					}, "Model accelerations due to atmospheric tides model");
				tryGetFromYaml(propagationOptions.pole_tide_ocean			, orbit_propagation,	{"@ pole_tide_ocean"			}, "Model accelerations due to ocean pole tide (degree 2 only)");
				tryGetFromYaml(propagationOptions.pole_tide_solid			, orbit_propagation,	{"@ pole_tide_solid"			}, "Model accelerations due to solid pole tide (degree 2 only)");
				tryGetFromYaml(propagationOptions.aod						, orbit_propagation,	{"@ aod"						}, "Model Atmospheric and Oceanic non tidal accelerations");
				tryGetFromYaml(propagationOptions.central_force				, orbit_propagation,	{"@ central_force"				}, "Acceleration due to the central force");
				tryGetFromYaml(propagationOptions.general_relativity		, orbit_propagation,	{"@ general_relativity"			}, "Model acceleration due general relativisty");
			}
		}

// 		estimation_parameters
		{
			auto estimation_parameters	= stringsToYamlObject({yaml, ""},				{estimation_parameters_str});
			auto global_models			= stringsToYamlObject(estimation_parameters,	{"@ global_models"});

			tryGetKalmanFromYaml(pppOpts.eop,		global_models, "@ eop"			);
			tryGetKalmanFromYaml(pppOpts.eop_rates,	global_models, "@ eop_rates"	);
			tryGetKalmanFromYaml(ionModelOpts.ion,	global_models, "@ ion"			);
		}

// 		mongo
		{
			auto mongo = stringsToYamlObject({yaml, ""}, {"5!  mongo"}, "Mongo is a database used to store results and intermediate values for later analysis and inter-process communication");

			tryGetEnumOpt (mongoOpts.enable,							mongo, {"0! enable"					}, "Enable and connect to mongo database");
			tryGetEnumOpt (mongoOpts.output_measurements,				mongo, {"1! output_measurements"	}, "Output measurements and their residuals");
			tryGetEnumOpt (mongoOpts.output_components,					mongo, {"1! output_components"		}, "Output components of measurements");
			tryGetEnumOpt (mongoOpts.output_cumulative,					mongo, {"1! output_cumulative"		}, "Output cumulative residuals of components of measurements");
			tryGetEnumOpt (mongoOpts.output_states,						mongo, {"1! output_states"			}, "Output states");
			tryGetEnumOpt (mongoOpts.output_state_covars,				mongo, {"1! output_state_covars"	}, "Output covariance values of related states");
			tryGetEnumOpt (mongoOpts.output_config,						mongo, {"2@ output_config"			}, "Output config");
			tryGetEnumOpt (mongoOpts.output_trace,						mongo, {"2@ output_trace"			}, "Output trace");
			tryGetEnumOpt (mongoOpts.output_test_stats,					mongo, {"2@ output_test_stats"		}, "Output test statistics");
			tryGetEnumOpt (mongoOpts.output_logs,						mongo, {"2@ output_logs"			}, "Output console trace and warnings to mongo with timestamps and other metadata");
			tryGetEnumOpt (mongoOpts.output_ssr_precursors,				mongo, {"2@ output_ssr_precursors"	}, "Output orbits, clocks, and bias estimates to allow communication to ssr generating processes");
			tryGetEnumOpt (mongoOpts.delete_history,					mongo, {"1! delete_history"			}, "Drop the collection in the database at the beginning of the run to only show fresh data");
			tryGetEnumOpt (mongoOpts.cull_history,						mongo, {"1@ cull_history"			}, "Erase old database objects to limit the size and speed degredation over long runs");
			tryGetEnumOpt (mongoOpts.use_predictions,					mongo, {"2@ use_predictions"		});
			tryGetEnumOpt (mongoOpts.output_predictions,				mongo, {"2@ output_predictions"		});
			tryGetFromYaml(mongoOpts.queue_outputs,						mongo, {"2@ queue_outputs"			}, "Output data in a separate thread - may reduce latency");
			tryGetFromYaml(mongoOpts.min_cull_age,						mongo, {"2@ min_cull_age"			}, "Age of which to cull history");

			tryGetEnumVec (mongoOpts.used_predictions,					mongo, {"@ used_predictions"		}, "Filter states to retrieve from mongo");
			tryGetEnumVec (mongoOpts.sent_predictions,					mongo, {"@ sent_predictions"		}, "Filter states to predict and send to mongo");

			tryGetFromYaml(mongoOpts[E_Mongo::PRIMARY].suffix,			mongo, {"3@ primary_suffix"					}, "Suffix to append to database elements to make distinctions between runs for comparison");
			tryGetFromYaml(mongoOpts[E_Mongo::PRIMARY].database,		mongo, {"3@ primary_database"				});
			tryGetFromYaml(mongoOpts[E_Mongo::PRIMARY].uri,				mongo, {"3@ primary_uri"					}, "Location and port of the mongo database to connect to");

			tryGetFromYaml(mongoOpts[E_Mongo::SECONDARY].suffix,		mongo, {"3@ secondary_suffix"				}, "Suffix to append to database elements to make distinctions between runs for comparison");
			tryGetFromYaml(mongoOpts[E_Mongo::SECONDARY].database,		mongo, {"3@ secondary_database"				});
			tryGetFromYaml(mongoOpts[E_Mongo::SECONDARY].uri,			mongo, {"3@ secondary_uri"					}, "Location and port of the mongo database to connect to");
		}

// 		debug
		{
			auto debug = stringsToYamlObject({yaml, ""}, {"9@ debug"}, "Debug options are designed for developers and should probably not be used by normal users");

			tryGetFromAny(fatal_level,			commandOpts,	debug, {"# fatal_message_level"			}, "Threshold level for exiting the program early (0-2)");
			tryGetFromYaml(check_plumbing,						debug, {"# check_plumbing"				}, "Debugging option to show sizes of objects in memory to detect leaks");
			tryGetFromYaml(explain_measurements,				debug, {"# explain_measurements"		}, "Debugging option to show verbose measurement coefficients");
			tryGetFromYaml(retain_rts_files,					debug, {"# retain_rts_files"			}, "Debugging option to keep rts files for post processing");
			tryGetFromYaml(rts_only,							debug, {"# rts_only"					}, "Debugging option to only re-run rts from previous run");
			tryGetFromYaml(mincon_only,							debug, {"# mincon_only"					}, "Debugging option to re-run minimum constraints code");
			tryGetFromYaml(output_mincon,						debug, {"# output_mincon"				}, "Debugging option to only save pre-minimum constraints filter state");
			tryGetFromYaml(mincon_filename,						debug, {"# mincon_filename"				}, "Filename of pre-mincon filter state for backup/loading");
			tryGetFromYaml(check_broadcast_differences,			debug, {"@ check_broadcast_differences"	});
			tryGetFromAny (compare_orbits,		commandOpts,	debug, {"@ compare_orbits"				});
			tryGetFromAny (compare_clocks,		commandOpts,	debug, {"@ compare_clocks"				});
			tryGetFromAny (compare_attitudes,	commandOpts,	debug, {"@ compare_attitudes"			});
		}
	}

// 		tryGetFromYaml(split_sys,				outputs, { "split_sys"		});


// 	Try to change all filenames to replace <YYYY> etc with other values.
	{
		replaceTags(config_description);
		replaceTags(gnss_obs_root);
		replaceTags(pseudo_obs_root);
		replaceTags(sat_data_root);
		replaceTags(rtcm_inputs_root);

		replaceTags(vmf_files);									globber(vmf_files);
		replaceTags(atx_files);									globber(atx_files);
		replaceTags(snx_files);									globber(snx_files);
		replaceTags(erp_files);									globber(erp_files);
		replaceTags(ion_files);									globber(ion_files);
		replaceTags(nav_files);									globber(nav_files);
		replaceTags(sp3_files);									globber(sp3_files);
		replaceTags(dcb_files);									globber(dcb_files);
		replaceTags(bsx_files);									globber(bsx_files);
		replaceTags(clk_files);									globber(clk_files);
		replaceTags(obx_files);									globber(obx_files);
		replaceTags(sid_files);									globber(sid_files);
		replaceTags(cmc_files);									globber(cmc_files);
		replaceTags(com_files);									globber(com_files);
		replaceTags(crd_files);									globber(crd_files);
		replaceTags(egm_files);									globber(egm_files);
		replaceTags(igrf_files);								globber(igrf_files);
		replaceTags(hfeop_files);								globber(hfeop_files);
		replaceTags(gpt2grid_files);							globber(gpt2grid_files);
		replaceTags(orography_files);							globber(orography_files);
		replaceTags(atm_reg_definitions);						globber(atm_reg_definitions);
		replaceTags(pseudo_filter_files);						globber(pseudo_filter_files);
		replaceTags(planetary_ephemeris_files);					globber(planetary_ephemeris_files);
		replaceTags(ocean_tide_potential_files);				globber(ocean_tide_potential_files);
		replaceTags(atmos_tide_potential_files);				globber(atmos_tide_potential_files);
		replaceTags(ocean_tide_loading_blq_files);				globber(ocean_tide_loading_blq_files);
		replaceTags(atmos_tide_loading_blq_files);				globber(atmos_tide_loading_blq_files);
		replaceTags(ocean_pole_tide_loading_files);				globber(ocean_pole_tide_loading_files);
		replaceTags(atmos_oceean_dealiasing_files);				globber(atmos_oceean_dealiasing_files);
		replaceTags(ocean_pole_tide_potential_files);			globber(ocean_pole_tide_potential_files);

		replaceTags(rnx_inputs);								globber(rnx_inputs);
		replaceTags(ubx_inputs);								globber(ubx_inputs);
		replaceTags(custom_inputs);								globber(custom_inputs);
		replaceTags(obs_rtcm_inputs);							globber(obs_rtcm_inputs);
		replaceTags(nav_rtcm_inputs);							globber(nav_rtcm_inputs);
		replaceTags(qzs_rtcm_inputs);							globber(qzs_rtcm_inputs);
		replaceTags(pseudo_sp3_inputs);							globber(pseudo_sp3_inputs);
		replaceTags(pseudo_snx_inputs);							globber(pseudo_snx_inputs);


		replaceTags(sp3_directory);							replaceTags(sp3_filename);
		replaceTags(erp_directory);							replaceTags(erp_filename);
		replaceTags(gpx_directory);							replaceTags(gpx_filename);
		replaceTags(pos_directory);							replaceTags(pos_filename);
		replaceTags(log_directory);							replaceTags(log_filename);
		replaceTags(cost_directory);						replaceTags(cost_filename);
		replaceTags(sinex_directory);						replaceTags(sinex_filename);
		replaceTags(ionex_directory);						replaceTags(ionex_filename);
		replaceTags(orbex_directory);						replaceTags(orbex_filename);
		replaceTags(clocks_directory);						replaceTags(clocks_filename);
		replaceTags(slr_obs_directory);						replaceTags(slr_obs_filename);
		replaceTags(ionstec_directory);						replaceTags(ionstec_filename);
		replaceTags(raw_ubx_directory);						replaceTags(raw_ubx_filename);
		replaceTags(rtcm_nav_directory);					replaceTags(rtcm_nav_filename);
		replaceTags(rtcm_obs_directory);					replaceTags(rtcm_obs_filename);
		replaceTags(orbit_ics_directory);					replaceTags(orbit_ics_filename);
		replaceTags(ntrip_log_directory);					replaceTags(ntrip_log_filename);
		replaceTags(rinex_obs_directory);					replaceTags(rinex_obs_filename);
		replaceTags(rinex_nav_directory);					replaceTags(rinex_nav_filename);
		replaceTags(raw_custom_directory);					replaceTags(raw_custom_filename);
		replaceTags(bias_sinex_directory);					replaceTags(bias_sinex_filename);
		replaceTags(trop_sinex_directory);					replaceTags(trop_sinex_filename);
		replaceTags(pppOpts.rts_directory);					replaceTags(pppOpts.rts_filename);
		replaceTags(sp3_directory);							replaceTags(predicted_sp3_filename);
		replaceTags(ems_directory);							replaceTags(ems_filename);
		replaceTags(trace_directory);						replaceTags(receiver_trace_filename);
		replaceTags(trace_directory);						replaceTags(network_trace_filename);
		replaceTags(trace_directory);						replaceTags(satellite_trace_filename);
		replaceTags(trace_directory);						replaceTags(ionosphere_trace_filename);
		replaceTags(decoded_rtcm_json_directory);			replaceTags(decoded_rtcm_json_filename);
		replaceTags(encoded_rtcm_json_directory);			replaceTags(encoded_rtcm_json_filename);
		replaceTags(network_statistics_json_directory);		replaceTags(network_statistics_json_filename);

		replaceTags(mongoOpts[E_Mongo::PRIMARY]		.uri);
		replaceTags(mongoOpts[E_Mongo::PRIMARY]		.suffix);
		replaceTags(mongoOpts[E_Mongo::PRIMARY]		.database);
		replaceTags(mongoOpts[E_Mongo::SECONDARY]	.uri);
		replaceTags(mongoOpts[E_Mongo::SECONDARY]	.suffix);
		replaceTags(mongoOpts[E_Mongo::SECONDARY]	.database);
	}

    sanityChecks();

// 	get template options
	{
		SatSys dummySat("G00");
		getSatOpts(dummySat, {"@ L1W"});
		getRecOpts("! global");
		getRecOpts("@ XMPL", {"@GPS", "@ L1W"});
	}

	for (auto& yaml : yamls)
	{
		string filename;
		if (yaml["yaml_filename"])
			filename = yaml["yaml_filename"].as<string>();

		recurseYaml(filename, yaml);
	}

	for (auto& [stack, defaults] : acsConfig.yamlDefaults)
	{
		if (defaults.comment.empty())
		{
			string dummy;
			string str = nonNumericStack(stack, dummy);
			BOOST_LOG_TRIVIAL(debug) << "Dev: " << str << " has no documentation comment";
		}
	}

	if (commandOpts.count("yaml-defaults"))
	{
		int level = commandOpts["yaml-defaults"].as<int>();
		outputDefaultConfiguration(level);
	}

	return true;
}
