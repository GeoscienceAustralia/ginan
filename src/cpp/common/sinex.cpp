
// #pragma GCC optimize ("O0")

#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <cstring>
#include <map>
#include <sys/utsname.h>

#include <boost/log/trivial.hpp>

#include "eigenIncluder.hpp"
#include "navigation.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "sinex.hpp"
#include "trace.hpp"

using std::endl;
using std::getline;
using std::ifstream;
using std::ofstream;




Sinex theSinex(false); // the one and only sinex object.


// Sinex 2.02 documentation indicates 2 digit years. >50 means 1900+N. <=50 means 2000+N
// To achieve this, when we read years, if >50 add 1900 else add 2000. This source will
// cease to work safely around 2045!
// when we write years, write out modulo 100
// This only applies to site data, for satellites it is using 4 digit years
void nearestYear(double& year)
{
	if (year > 50)	year += 1900;
	else			year += 2000;
}

/** Trims leading & trailing whitespace
 */
string trim(
	const string& ref)	///< string to trim
{
	int start = 0, stop = ref.length() - 1, len;

	while (start	!= stop		&& isspace(ref[start]))			start++;
	while (stop		!= start	&& isspace(ref[stop]))			stop--;

	len = stop - start + 1;

	return ref.substr(start, len);
}

/** Cuts string after first space, deletes trailing carriage return
 */
void trimCut(
	string& line) ///< string to trim
{
	line = line.substr(0, line.find(' '));
	if (line.back() == '\r')
		line.pop_back();
}

/** seconds diff bewteen left and right
 * If left < right the value is negative
 */
long int time_compare(UYds& left, UYds& right)			//todo aaron, delete this
{
	long int leftfull	= (left[0]	* 365 + left[1])	* 86400 + left[2];
	long int rightfull	= (right[0] * 365 + right[1])	* 86400 + right[2];

	return leftfull - rightfull;
}

bool compare(string& one, 	string& two)
{
	if (one.compare(two) == 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_input_file_t& one, 	Sinex_input_file_t& two)
{
	if 	(  one.yds[0]	== two.yds[0]
		&& one.yds[1]	== two.yds[1]
		&& one.yds[2]	== two.yds[2]
		&& one.agency		.compare(two.agency)		== 0
		&& one.file			.compare(two.file)			== 0
		&& one.description	.compare(two.description)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_solstatistic_t& one, 	Sinex_solstatistic_t& two)
{
	if (one.name.compare(two.name) == 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_satpc_t& one, 	Sinex_satpc_t& two)
{
	if 	( one.svn.compare(two.svn) == 0
		&&one.freq	== two.freq
		&&one.freq2	== two.freq2)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_satecc_t& one, 	Sinex_satecc_t& two)
{
	if 	( one.svn.compare(two.svn) == 0
		&&one.type == two.type)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_satmass_t& one, 	Sinex_satmass_t& two)
{
	if 	( one.svn.compare(two.svn) == 0
		&&one.start[0]	== two.start[0]
		&&one.start[1]	== two.start[1]
		&&one.start[2]	== two.start[2]
		&&one.stop[0]	== two.stop[0]
		&&one.stop[1]	== two.stop[1]
		&&one.stop[2]	== two.stop[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_satfreqchn_t& one, 	Sinex_satfreqchn_t& two)
{
	if 	( one.svn.compare(two.svn) == 0
		&&one.start[0]	== two.start[0]
		&&one.start[1]	== two.start[1]
		&&one.start[2]	== two.start[2]
		&&one.stop[0]	== two.stop[0]
		&&one.stop[1]	== two.stop[1]
		&&one.stop[2]	== two.stop[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_satid_t& one, 	Sinex_satid_t& two)
{
	if 	( one.svn.compare(two.svn) == 0
		&&one.prn.compare(two.prn) == 0
		&&one.timeSinceLaunch[0]	== two.timeSinceLaunch[0]
		&&one.timeSinceLaunch[1]	== two.timeSinceLaunch[1]
		&&one.timeSinceLaunch[2]	== two.timeSinceLaunch[2]
		&&one.timeUntilDecom[0]		== two.timeUntilDecom[0]
		&&one.timeUntilDecom[1]		== two.timeUntilDecom[1]
		&&one.timeUntilDecom[2]		== two.timeUntilDecom[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_precode_t& one, 	Sinex_precode_t& two)
{
	if 	(one.precesscode.compare(two.precesscode) == 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_source_id_t& one, 	Sinex_source_id_t& two)
{
	if 	(one.source.compare(two.source) == 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_nutcode_t& one, 	Sinex_nutcode_t& two)
{
	if 	(one.nutcode.compare(two.nutcode) == 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_satprn_t& one, 	Sinex_satprn_t& two)
{
	if 	( one.svn.compare(two.svn) == 0
		&&one.prn.compare(two.prn) == 0
		&&one.start[0] == two.start[0]
		&&one.start[1] == two.start[1]
		&&one.start[2] == two.start[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_satpower_t& one, 	Sinex_satpower_t& two)
{
	if 	( one.svn.compare(two.svn) == 0
		&&one.start[0]	== two.start[0]
		&&one.start[1]	== two.start[1]
		&&one.start[2]	== two.start[2]
		&&one.stop[0]	== two.stop[0]
		&&one.stop[1]	== two.stop[1]
		&&one.stop[2]	== two.stop[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_satcom_t& one, 	Sinex_satcom_t& two)
{
	if 	( one.svn.compare(two.svn) == 0
		&&one.start[0]	== two.start[0]
		&&one.start[1]	== two.start[1]
		&&one.start[2]	== two.start[2]
		&&one.stop[0]	== two.stop[0]
		&&one.stop[1]	== two.stop[1]
		&&one.stop[2]	== two.stop[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_ack_t& one, 	Sinex_ack_t& two)
{
	if 	( one.agency		.compare(two.agency)		== 0
		&&one.description	.compare(two.description)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_input_history_t& one, 	Sinex_input_history_t& two)
{
	if 	(  one.code				== two.code
		&& one.fmt				== two.fmt
		&& one.create_time[0]	== two.create_time[0]
		&& one.create_time[1]	== two.create_time[1]
		&& one.create_time[2]	== two.create_time[2]
		&& one.start[0]			== two.start[0]
		&& one.start[1]			== two.start[1]
		&& one.start[2]			== two.start[2]
		&& one.stop[0]			== two.stop[0]
		&& one.stop[1]			== two.stop[2]
		&& one.stop[2]			== two.stop[2]
		&& one.obs_tech			== two.obs_tech
		&& one.num_estimates	== two.num_estimates
		&& one.constraint		== two.constraint
		&& one.contents		.compare(two.contents) 		== 0
		&& one.data_agency	.compare(two.data_agency) 	== 0
		&& one.create_agency.compare(two.create_agency) == 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_siteid_t& one, 	Sinex_siteid_t& two)
{
	if 	(one.sitecode.compare(two.sitecode) == 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_sitedata_t& one, 	Sinex_sitedata_t& two)
{
	if 	( one.site.		compare(two.site)		== 0
		&&one.sitecode.	compare(two.sitecode)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_receiver_t& one, 	Sinex_receiver_t& two)
{
	if 	( one.sitecode.compare(two.sitecode) == 0
		&&one.start[0]	== two.start[0]
		&&one.start[1]	== two.start[1]
		&&one.start[2]	== two.start[2]
		&&one.end[0]	== two.end[0]
		&&one.end[1]	== two.end[1]
		&&one.end[2]	== two.end[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_antenna_t& one, 	Sinex_antenna_t& two)
{
	if 	( one.sitecode.compare(two.sitecode) == 0
		&&one.start[0]	== two.start[0]
		&&one.start[1]	== two.start[1]
		&&one.start[2]	== two.start[2]
		&&one.end[0]	== two.end[0]
		&&one.end[1]	== two.end[1]
		&&one.end[2]	== two.end[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_gps_phase_center_t& one, 	Sinex_gps_phase_center_t& two)
{
	if 	( one.antname	.compare(two.antname)	== 0
		&&one.serialno	.compare(two.serialno)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_gal_phase_center_t& one, 	Sinex_gal_phase_center_t& two)
{
	if 	( one.antname	.compare(two.antname)	== 0
		&&one.serialno	.compare(two.serialno)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_site_ecc_t& one, 	Sinex_site_ecc_t& two)
{
	if 	( one.sitecode.compare(two.sitecode) == 0
		&&one.start[0]	== two.start[0]
		&&one.start[1]	== two.start[1]
		&&one.start[2]	== two.start[2]
		&&one.end[0]	== two.end[0]
		&&one.end[1]	== two.end[1]
		&&one.end[2]	== two.end[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_solepoch_t& one, 	Sinex_solepoch_t& two)
{
	if 	( one.sitecode.compare(two.sitecode) == 0
		&&one.start[0]	== two.start[0]
		&&one.start[1]	== two.start[1]
		&&one.start[2]	== two.start[2]
		&&one.end[0]	== two.end[0]
		&&one.end[1]	== two.end[1]
		&&one.end[2]	== two.end[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_solestimate_t& one, 	Sinex_solestimate_t& two)
{
	if 	( one.sitecode.compare(two.sitecode)		== 0
		&&one.type.compare(two.type)				== 0
		&&time_compare(one.refepoch, two.refepoch)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_solapriori_t& one, 	Sinex_solapriori_t& two)
{
	if 	( one.sitecode.compare(two.sitecode)		== 0
		&&one.param_type.compare(two.param_type)	== 0
		&&time_compare(one.epoch, two.epoch)		== 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_solneq_t& one, 	Sinex_solneq_t& two)
{
	if 	( one.site.compare(two.site)			== 0
		&&one.ptype.compare(two.ptype)			== 0
		&&time_compare(one.epoch, two.epoch)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_solmatrix_t& one, 	Sinex_solmatrix_t& two)
{
	if 	( one.row == two.row
		&&one.col == two.col)
	{
		return true;
	}
	return false;
}

template<typename TYPE>
void dedupe(list<TYPE>& source)
{
	list<TYPE> copy;

	for (auto it = source.begin(); it != source.end();   )
	{
		bool found = false;

		for (auto it2 = copy.begin(); it2 != copy.end(); it2++)
		{
			if (compare(*it, *it2))
			{
				found = true;
				break;
			}
		}

		if (found)
		{
			it = source.erase(it);
		}
		else
		{
			copy.push_back(*it);
			it++;
		}
	}
}

template<typename TYPE>
void dedupeB(list<TYPE>& source)
{
	TYPE previous;
	bool first = true;

	for (auto it = source.begin(); it != source.end();   )
	{
		bool found = false;

		if (!first)
		{
			if (compare(*it, previous))
			{
				found = true;
			}
		}

		if (found)
		{
			it = source.erase(it);
		}
		else
		{
			previous = *it;
			it++;
			first = false;
		}
	}
}

// each of the lists is parsed for duplicates. When a dup is found it is erased. At the end of each loop the _copy list should contain the same stuff
// as the original
void dedupe_sinex()
{
	// do the lists which are not sorted first

	// general stuff
	dedupe(theSinex.refstrings);
	dedupe(theSinex.inputHistory);
	dedupe(theSinex.inputFiles);
	dedupe(theSinex.acknowledgements);
	dedupe(theSinex.list_nutcodes);
	dedupe(theSinex.list_precessions);
	dedupe(theSinex.list_source_ids);
	dedupe(theSinex.list_satids);
	dedupe(theSinex.list_satprns);
	dedupe(theSinex.list_satfreqchns);
	dedupe(theSinex.list_satcoms);
	dedupe(theSinex.list_sateccs);
	dedupe(theSinex.list_satpcs);
	dedupe(theSinex.list_statistics);

	// 	// TODO: need to make sure sitecode & type match on index
	// site stuff
	// all data is sorted before coming in here, so it suffices to just check against the previous value
	dedupeB(theSinex.list_sitedata);
	dedupeB(theSinex.list_gps_pcs);
	dedupeB(theSinex.list_gal_pcs);
	dedupeB(theSinex.list_normal_eqns);

	for (matrix_type	t = ESTIMATE;		t < MAX_MATRIX_TYPE;	t = static_cast<matrix_type>	(static_cast<int>(t) + 1))
	for (matrix_value	v = CORRELATION;	v < MAX_MATRIX_VALUE;	v = static_cast<matrix_value>	(static_cast<int>(v) + 1))
	{
		if (theSinex.matrix_map[t][v].empty())
			continue;

		dedupeB(theSinex.matrix_map[t][v]);
	}

	return;
}

// TODO; What if we are reading a second file. What wins?
bool read_snx_header(std::ifstream& in)
{
	string s;

	std::getline(in, s);

	if (in.eof())
	{
		BOOST_LOG_TRIVIAL(error) << "Error: empty file" << endl;
		return false;
	}

	// verify line contents
	if 	(  s[0] != '%'
		|| s[1] != '='
		|| s[2] != 'S'
		|| s[3] != 'N'
		|| s[4] != 'X')
	{
		// error. not a sinex file
		BOOST_LOG_TRIVIAL(error) << "Error: Not a sinex file" << endl;
		return false;
	}

	// remaining characters indiciate properties of the file
	if (s.length() > 5)
	{
		const char* p = s.c_str();
		char create_agc[4];
		char data_agc[4];
		char solcontents[7];

		int  readcount = sscanf(p + 6, "%4lf %3s %2lf:%3lf:%5lf %3s %2lf:%3lf:%5lf %2lf:%3lf:%5lf %c %5d %c %c %c %c %c %c %c",
						&theSinex.ver,
						create_agc,
						&theSinex.filedate[0],
						&theSinex.filedate[1],
						&theSinex.filedate[2],
						data_agc,
						&theSinex.solution_start_date[0],
						&theSinex.solution_start_date[1],
						&theSinex.solution_start_date[2],
						&theSinex.solution_end_date[0],
						&theSinex.solution_end_date[1],
						&theSinex.solution_end_date[2],
						&theSinex.obsCode,
						&theSinex.numparam,
						&theSinex.constCode,
						&solcontents[0],
						&solcontents[1],
						&solcontents[2],
						&solcontents[3],
						&solcontents[4],
						&solcontents[5]);

		if (readcount < 15)
		{
			// error, not enough parameters
			BOOST_LOG_TRIVIAL(error) << "Error: Not enough parameters on header line (expected min 15), got " << readcount << endl;
			return false;
		}

		while (readcount < 21)
		{
			solcontents[readcount - 15] = ' ';
			readcount++;
		}

		solcontents[6] = '\0';

		theSinex.create_agc	= create_agc;
		theSinex.data_agc	= data_agc;
		theSinex.solcont	= solcontents;

		nearestYear(theSinex.filedate[0]);
		nearestYear(theSinex.solution_start_date[0]);
		nearestYear(theSinex.solution_end_date[0]);
	}

	return true;
}

void updateSinexHeader(
	string& 	create_agc,
	string&		data_agc,
	UYds		soln_start,
	UYds		soln_end,
	const char	obsCode,
	const char	constCode,
	string&		contents,
	int			numParam,
	double		sinexVer)
{
	Sinex_input_history_t siht;

	siht.code			= '+';
	siht.fmt			= theSinex.ver;
	siht.create_agency	= theSinex.create_agc;
	siht.data_agency	= theSinex.data_agc;
	siht.obs_tech		= theSinex.obsCode;
	siht.constraint		= theSinex.constCode;
	siht.num_estimates	= theSinex.numparam;
	siht.contents		= theSinex.solcont;
	siht.create_time	= theSinex.filedate;
	siht.start			= theSinex.solution_start_date;
	siht.stop			= theSinex.solution_end_date;

	if (theSinex.inputHistory.empty())
		theSinex.inputHistory.push_back(siht);

	theSinex.ver = sinexVer;

	if (data_agc.size() > 0)		theSinex.data_agc = data_agc;
	else							theSinex.data_agc = theSinex.create_agc;

	theSinex.create_agc				= create_agc;
	theSinex.solcont				= contents;
	theSinex.filedate				= timeGet();
	theSinex.solution_start_date	= soln_start;
	theSinex.solution_end_date		= soln_end;

	if (obsCode		!= ' ')
		theSinex.obsCode	= obsCode;

	if (constCode 	!= ' ')
		theSinex.constCode	= constCode;

	theSinex.numparam = numParam;
}

void write_snx_header(std::ofstream& out)
{
	char line[81];
	char c;
	int  i;

	int offset = 0;
	offset += snprintf(line + offset, sizeof(line) - offset, "%%=SNX %4.2lf %3s %2.2d:%3.3d:%5.5d %3s %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %c %5d %c",
			theSinex.ver,
			theSinex.create_agc.c_str(),
			(int)theSinex.filedate[0] % 100,
			(int)theSinex.filedate[1],
			(int)theSinex.filedate[2],
			theSinex.data_agc.c_str(),
			(int)theSinex.solution_start_date[0] % 100,
			(int)theSinex.solution_start_date[1],
			(int)theSinex.solution_start_date[2],
			(int)theSinex.solution_end_date[0] % 100,
			(int)theSinex.solution_end_date[1],
			(int)theSinex.solution_end_date[2],
			theSinex.obsCode,
			theSinex.numparam,
			theSinex.constCode);

	i = 0;
	c = theSinex.solcont[0];

	while (c != ' ')
	{
		snprintf(line + offset, sizeof(line) - offset, " %c", c);

		i++;

		if (i <= theSinex.solcont.length())			c = theSinex.solcont[i];
		else										c = ' ';
	}

	out << line << endl;
}

void parseReference(string& s)
{
	theSinex.refstrings.push_back(s);
}

void write_as_comments(
	Trace&			out,
	list<string>&	comments)
{
	for (auto& comment : comments)
	{
		string s = comment;

		// just make sure it starts with * as required by format
		s[0] = '*';

		out << s << endl;
	}
}

void comments_override()
{
	// overriding only those that can be found in IGS/CODE/GRG SINEX files
	theSinex.blockComments["FILE/REFERENCE"].					push_back("*OWN __CREATION__ ___________FILENAME__________ ___________DESCRIPTION__________");		// INPUT/FILES
	theSinex.blockComments["INPUT/HISTORY"].					push_back("*_VERSION_ CRE __CREATION__ OWN _DATA_START_ __DATA_END__ T PARAM S ____TYPE____");		// INPUT/HISTORY
	theSinex.blockComments["INPUT/ACKNOWLEDGEMENTS"].			push_back("*AGY ______________________________FULL_DESCRIPTION_____________________________");		// INPUT/ACKNOWLEDGEMENTS
	theSinex.blockComments["SITE/ID"].							push_back("*CODE PT __DOMES__ T _STATION DESCRIPTION__ _LONGITUDE_ _LATITUDE__ HEIGHT_");			// SITE/ID
	theSinex.blockComments["SITE/DATA"].						push_back("*CODE PT SOLN CODE PT SOLN T _DATA START_ _DATA END___ OWN _FILE TIME__");				// SITE/DATA
	theSinex.blockComments["SITE/RECEIVER"].					push_back("*CODE PT SOLN T _DATA START_ _DATA END___ _RECEIVER TYPE______ _S/N_ _FIRMWARE__");		// SITE/RECEIVER
	theSinex.blockComments["SITE/ANTENNA"].						push_back("*CODE PT SOLN T _DATA START_ __DATA END__ __ANTENNA TYPE______ _S/N_");					// SITE/ANTENNA
	theSinex.blockComments["SITE/GPS_PHASE_CENTER"].			push_back("________TYPE________ _S/N_ _L1_U_ _L1_N_ _L1_E_ _L2_U_ _L2_N_ _L2_E_ __MODEL___");		// SITE/GPS_PHASE_CENTER
	theSinex.blockComments["SITE/ECCENTRICITY"].				push_back("*                                             _UP_____ _NORTH__ _EAST___\n*CODE PT SOLN T _DATA START_ __DATA END__ TYP __ARP-BENCHMARK (M)_______"); // SITE/ECCENTRICITY
	theSinex.blockComments["SOLUTION/ESTIMATE"].				push_back("*INDEX _TYPE_ CODE PT SOLN _REF_EPOCH__ UNIT S ___ESTIMATED_VALUE___ __STD_DEV__");		// BIAS/EPOCHS|SOLUTION/EPOCHS|SOLUTION/ESTIMATE
	theSinex.blockComments["SOLUTION/STATISTICS"].				push_back("*_STATISTICAL PARAMETER________ __VALUE(S)____________");								// SOLUTION/STATISTICS
	theSinex.blockComments["SOLUTION/APRIORI"].					push_back("*INDEX _TYPE_ CODE PT SOLN _REF_EPOCH__ UNIT S __APRIORI VALUE______ _STD_DEV___");		// SOLUTION/APRIORI
	theSinex.blockComments["SOLUTION/NORMAL_EQUATION_VECTOR"].	push_back("*INDEX TYPE__ CODE PT SOLN _REF_EPOCH__ UNIT S __RIGHT_HAND_SIDE____");					// SOLUTION/NORMAL_EQUATION_VECTOR
	theSinex.blockComments["SOLUTION/MATRIX_ESTIMATE"].			push_back("*PARA1 PARA2 _______PARA2+0_______ _______PARA2+1_______ _______PARA2+2_______");		// SOLUTION/MATRIX_ESTIMATE|SOLUTION/MATRIX_APRIORI|SOLUTION/NORMAL_EQUATION_MATRIX
	theSinex.blockComments["SATELLITE/PHASE_CENTER"].			push_back("*SITE L SATA_Z SATA_X SATA_Y L SATA_Z SATA_X SATA_Y MODEL_____ T M");					// SATELLITE/PHASE_CENTER
	theSinex.blockComments["SAT/ID"].							push_back("SATELLITE/ID *SITE PR COSPAR___ T DATA_START__ DATA_END____ ANTENNA_____________");		// SATELLITE/ID
}

void write_snx_reference(ofstream& out)
{
	Block block(out, "FILE/REFERENCE");

	for (auto& refString : theSinex.refstrings)
	{
		out << refString << endl;
	}
}

void write_snx_comments(ofstream& out)
{
	Block block(out, "FILE/COMMENT");

	for (auto& commentstring : theSinex.blockComments[block.blockName])
	{
		out << commentstring << endl;
	}
}

void parseInputHistory(string& s)
{
	Sinex_input_history_t siht;
	// remaining characters indiciate properties of the history

	if (s.length() > 5)
	{
		const char* p = s.c_str();
		char create_agc[4];
		char data_agc[4];
		char solcontents[7];
		int  readcount;

		siht.code = s[1];

		readcount = sscanf(p + 6, "%4lf %3s %2lf:%3lf:%5lf %3s %2lf:%3lf:%5lf %2lf:%3lf:%5lf %c %5d %c %c %c %c %c %c %c",
							&siht.fmt,
							create_agc,
							&siht.create_time[0],
							&siht.create_time[1],
							&siht.create_time[2],
							data_agc, &siht.start[0],
							&siht.start[1],
							&siht.start[2],
							&siht.stop[0],
							&siht.stop[1],
							&siht.stop[2],
							&siht.obs_tech,
							&siht.num_estimates,
							&siht.constraint,
							&solcontents[0],
							&solcontents[1],
							&solcontents[2],
							&solcontents[3],
							&solcontents[4],
							&solcontents[5]);

		if (readcount >= 15)
		{
			while (readcount < 21)
			{
				solcontents[readcount - 15] = ' ';
				readcount++;
			}

			solcontents[6] = '\0';

			siht.create_agency = create_agc;
			siht.data_agency = data_agc;
			siht.contents = solcontents;

			nearestYear(siht.create_time[0]);
			nearestYear(siht.start[0]);
			nearestYear(siht.stop[0]);

			theSinex.inputHistory.push_back(siht);
		}
	}
}

void write_snx_input_history(ofstream& out)
{
	Block block(out, "INPUT/HISTORY");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto it = theSinex.inputHistory.begin(); it != theSinex.inputHistory.end(); it++)
	{
		char	line[81] = {};
		int		offset = 0;
		Sinex_input_history_t siht = *it;
		int i = 0;

		offset += snprintf(line + offset, sizeof(line) - offset, " %cSNX %4.2lf %3s %2.2d:%3.3d:%5.5d %3s %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %c %5d %c",
				siht.code,
				siht.fmt,
				siht.create_agency.c_str(),
				(int)siht.create_time[0] % 100,
				(int)siht.create_time[1],
				(int)siht.create_time[2],
				siht.data_agency.c_str(),
				(int)siht.start[0] % 100,
				(int)siht.start[1],
				(int)siht.start[2],
				(int)siht.stop[0] % 100,
				(int)siht.stop[1],
				(int)siht.stop[2],
				siht.obs_tech,
				siht.num_estimates,
				siht.constraint);

		char c = siht.contents[i];

		while (c != ' ')
		{
			offset += snprintf(line + offset, sizeof(line) - offset, " %c", c);
			i++;

			if (siht.contents.length() >= i)			c = siht.contents[i];
			else										c = ' ';
		}

		out << line << endl;
	}
}

void parseInputFiles(string& s)
{
	Sinex_input_file_t sif;
	char agency[4];
	const char* p	= s.c_str();
	sif.file		= s.substr(18, 29);
	sif.description	= s.substr(48, 32);

	int  readcount = sscanf(p + 1, "%3s %2lf:%3lf:%5lf",
							agency,
							&sif.yds[0],
							&sif.yds[1],
							&sif.yds[2]);

	if (readcount == 4)
	{
		sif.agency = agency;

		nearestYear(sif.yds[0]);

		theSinex.inputFiles.push_back(sif);
	}
}

void write_snx_input_files(ofstream& out)
{
	Block block(out, "INPUT/FILES");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& inputFile : theSinex.inputFiles)
	{
		Sinex_input_file_t& sif = inputFile;

		char line[81];
		int len;
		snprintf(line, sizeof(line), " %3s %02d:%03d:%05d ",
				sif.agency.c_str(),
				(int)sif.yds[0] % 100,
				(int)sif.yds[1],
				(int)sif.yds[2]);

		// if the filename length is greater than 29 (format spec limit) make into a comment line
		if (sif.file.length() > 29)
			line[0] = '*';
		// pad short filenames to 29 characters
		if ((len = sif.file.length()) < 29)
		{
			for (int i = len; i<29; i++)
				sif.file += ' ';
		}
		out << line << sif.file << " " << sif.description << endl;
	}
}

void parseAcknowledgements(string& s)
{
	Sinex_ack_t sat;

	sat.description	= s.substr(5);
	sat.agency		= s.substr(1, 3);

	theSinex.acknowledgements.push_back(sat);
}

void write_snx_acknowledgements(ofstream& out)
{
	Block block(out, "INPUT/ACKNOWLEDGEMENTS");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& acknowledgement : theSinex.acknowledgements)
	{
		Sinex_ack_t& ack = acknowledgement;

		char line[81];
		snprintf(line, sizeof(line), " %3s %s", ack.agency.c_str(), ack.description.c_str());

		out << line << endl;
	}
}

void parseSiteIds(string& s)
{
	const char* p = s.c_str();
	Sinex_siteid_t sst;

	sst.sitecode	= trim(s.substr(1, 4));
	sst.ptcode		= s.substr(6, 2);
	sst.domes		= s.substr(9, 9);
	sst.typecode 	= s[19];
	sst.desc		= s.substr(21, 22);


	int    readcount = sscanf(p + 44, "%3d %2d %4lf %3d %2d %4lf %7lf",
						&sst.lon_deg,
						&sst.lon_min,
						&sst.lon_sec,
						&sst.lat_deg,
						&sst.lat_min,
						&sst.lat_sec,
						&sst.height);

	if (readcount == 7)
	{
		theSinex.map_siteids[sst.sitecode] = sst;
	}
}

void write_snx_siteids(ofstream& out)
{
	Block block(out, "SITE/ID");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& [id, ssi] : theSinex.map_siteids)
	{
		if (ssi.used == false)
		{
			continue;
		}

		tracepdeex(0, out, " %4s %2s %9s %c %22s %3d %2d %4.1lf %3d %2d %4.1lf %7.1lf\n",
				ssi.sitecode.c_str(),
				ssi.ptcode.c_str(),
				ssi.domes.c_str(),
				ssi.typecode,
				ssi.desc.c_str(),
				ssi.lon_deg,
				ssi.lon_min,
				ssi.lon_sec,
				ssi.lat_deg,
				ssi.lat_min,
				ssi.lat_sec,
				ssi.height);
	}
}

// compare by the 2 station ids only.
bool compare_sitedata(const Sinex_sitedata_t& left, const Sinex_sitedata_t& right)
{
	int sitec = left.site.compare(right.site);

	if (sitec == 0)
		sitec = left.sitecode.compare(right.sitecode);

	return (sitec < 0);
}

void parseSiteData(string& s)
{
	const char* p = s.c_str();

	Sinex_sitedata_t sst;

	sst.site		= s.substr(1, 4);
	sst.station_pt	= s.substr(6, 2);
	sst.soln_id		= s.substr(9, 4);
	sst.sitecode	= s.substr(14, 4);
	sst.site_pt		= s.substr(18, 2);
	sst.sitesoln	= s.substr(20, 4);

	sst.obscode		= s[24];
	UYds    start;
	UYds    end;
	UYds    create;
	char   agency[4];

	int    readcount;

	readcount = sscanf(p + 28, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf %3s %2lf:%3lf:%5lf",
						&start[0],
						&start[1],
						&start[2],
						&end[0],
						&end[1],
						&end[2],
						agency,
						&create[0],
						&create[1],
						&create[2]);

	if (readcount == 10)
	{
		sst.agency = agency;
		sst.start	= start;
		sst.stop	= end;
		sst.create	= create;

		// see comment at top of file
		if 	(  sst.start[0] != 0
			|| sst.start[1] != 0
			|| sst.start[2] != 0)
		{
			nearestYear(sst.start[0]);
		}

		if 	(  sst.stop[0] != 0
			|| sst.stop[1] != 0
			|| sst.stop[2] != 0)
		{
			nearestYear(sst.stop[0]);
		}

		nearestYear(sst.create[0]);

		theSinex.list_sitedata.push_back(sst);
	}
}

void write_snx_sitedata(ofstream& out, list<SinexRecData>* pstns)
{
	Block block(out, "SITE/DATA");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& sitedata : theSinex.list_sitedata)
	{
		Sinex_sitedata_t& ssd = sitedata;
		bool doit = false;

		char line[81];
		snprintf(line, sizeof(line), " %4s %2s %4s %4s %2s %4s %c %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %3s %2.2d:%3.3d:%5.5d",
				ssd.site.c_str(),
				ssd.station_pt.c_str(),
				ssd.soln_id.c_str(),
				ssd.sitecode.c_str(),
				ssd.site_pt.c_str(),
				ssd.sitesoln.c_str(),
				ssd.obscode,
				(int)ssd.start[0] % 100,
				(int)ssd.start[1],
				(int)ssd.start[2],
				(int)ssd.stop[0] % 100,
				(int)ssd.stop[1],
				(int)ssd.stop[2],
				ssd.agency.c_str(),
				(int)ssd.create[0] % 100,
				(int)ssd.create[1],
				(int)ssd.create[2]);

		if (pstns == nullptr)
			doit = true;
		else
		{
			for (auto& stn : *pstns)
			{
				if (ssd.site.compare(stn.id_ptr->sitecode) == 0)
				{
					doit = true;
					break;
				}
			}
		}

		if (doit)
			out << line << endl;
	}
}

void parseReceivers(string& s)
{
	const char* p = s.c_str();

	Sinex_receiver_t srt;

	srt.sitecode	= trim(s.substr(1, 4));
	srt.ptcode		= s.substr(6, 2);
	srt.solnid		= s.substr(9, 4);
	srt.typecode	= s[14];
	srt.type		= s.substr(42, 20);
	srt.sn			= s.substr(63, 5);
	srt.firm 		= trim(s.substr(69, 11));
	int readcount;

	readcount = sscanf(p + 16, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf",
						&srt.start[0],
						&srt.start[1],
						&srt.start[2],
						&srt.end[0],
						&srt.end[1],
						&srt.end[2]);

	if (readcount == 6)
	{
		// see comment at top of file
		if 	(  srt.start[0] != 0
			|| srt.start[1] != 0
			|| srt.start[2] != 0)
		{
			nearestYear(srt.start[0]);
		}

		if	(  srt.end[0] != 0
			|| srt.end[1] != 0
			|| srt.end[2] != 0)
		{
			nearestYear(srt.end[0]);
		}

		theSinex.map_receivers[srt.sitecode][srt.start] = srt;
	}
}

void write_snx_receivers(ofstream& out)
{
	Block block(out, "SITE/RECEIVER");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& [site, timemap] : theSinex.map_receivers)
	for (auto it = timemap.rbegin(); it != timemap.rend(); it++)
	{
		auto& [time, receiver] = *it;

		if (receiver.used == false)
		{
			continue;
		}

		tracepdeex(0, out, " %4s %2s %4s %c %02d:%03d:%05d %02d:%03d:%05d %20s %5s %s\n",
					receiver.sitecode	.c_str(),
					receiver.ptcode		.c_str(),
					receiver.solnid		.c_str(),
					receiver.typecode,
					(int)receiver.start[0] % 100,
					(int)receiver.start[1],
					(int)receiver.start[2],
					(int)receiver.end[0] % 100,
					(int)receiver.end[1],
					(int)receiver.end[2],
					receiver.type	.c_str(),
					receiver.sn		.c_str(),
					receiver.firm	.c_str());
	}
}

void parseAntennas(string& s)
{
	const char* p = s.c_str();

	Sinex_antenna_t ant;

	ant.sitecode	= trim(s.substr(1, 4));
	ant.ptcode		= s.substr(6, 2);
	ant.solnnum		= s.substr(9, 4);
	ant.typecode	= s[14];
	ant.type		= s.substr(42, 20);
	ant.sn			= trim(s.substr(63, 5));

	int    readcount = sscanf(p + 16, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf",
								&ant.start[0],
								&ant.start[1],
								&ant.start[2],
								&ant.end[0],
								&ant.end[1],
								&ant.end[2]);

	if (readcount == 6)
	{
		// see comment at top of file
		if 	(  ant.start[0] != 0
			|| ant.start[1] != 0
			|| ant.start[2] != 0)
		{
			nearestYear(ant.start[0]);
		}

		if 	(  ant.end[0] != 0
			|| ant.end[1] != 0
			|| ant.end[2] != 0)
		{
			nearestYear(ant.end[0]);
		}

		theSinex.map_antennas[ant.sitecode][ant.start] = ant;
// 				theSinex.list_antennas.push_back(ant);
	}
}

void write_snx_antennas(ofstream& out)
{
	Block block(out, "SITE/ANTENNA");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& [site, antmap]	: theSinex.map_antennas)
	for (auto it = antmap.rbegin(); it != antmap.rend(); it++)
	{
		auto& [time, ant] = *it;

		if (ant.used == false)
		{
			continue;
		}

		tracepdeex(0, out, " %4s %2s %4s %c %02d:%03d:%05d %02d:%03d:%05d %20s %s\n",
					ant.sitecode	.c_str(),
					ant.ptcode		.c_str(),
					ant.solnnum		.c_str(),
					ant.typecode,
					(int)ant.start[0] % 100,
					(int)ant.start[1],
					(int)ant.start[2],
					(int)ant.end[0] % 100,
					(int)ant.end[1],
					(int)ant.end[2],
					ant.type	.c_str(),
					ant.sn		.c_str());
	}
}

// compare by antenna type and serial number.
bool compare_gps_pc(Sinex_gps_phase_center_t& left, Sinex_gps_phase_center_t& right)
{
	int comp = left.antname.compare(right.antname);

	if (!comp)
		comp = left.serialno.compare(right.serialno);

	return (comp < 0);
}

void parseGpsPhaseCenters(string& s)
{
	const char* p = s.c_str();
	Sinex_gps_phase_center_t sgpct;

	sgpct.antname	= s.substr(1, 20);
	sgpct.serialno	= s.substr(22, 5);
	sgpct.calib		= s.substr(70, 10);

	int readcount = sscanf(p + 28, "%6lf %6lf %6lf %6lf %6lf %6lf",
								&sgpct.L1[0],
								&sgpct.L1[1],
								&sgpct.L1[2],
								&sgpct.L2[0],
								&sgpct.L2[1],
								&sgpct.L2[2]);

	if (readcount == 6)
	{
		theSinex.list_gps_pcs.push_back(sgpct);
	}
}

void truncateSomething(char* buf)
{
	if 	(  strlen(buf) == 7
		&& buf[1] == '0'
		&& buf[0] == '-')
	{
		for (int j = 2; j < 8; j++)
		{
			buf[j - 1] = buf[j];
		}
	}
}


void write_snx_gps_pcs(ofstream& out, list<SinexRecData>* pstns)
{
	Block block(out, "SITE/GPS_PHASE_CENTER");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& gps_pc : theSinex.list_gps_pcs)
	{
		Sinex_gps_phase_center_t& sgt = gps_pc;
		char buf[8];
		bool doit = false;

		char	line[81];
		int		offset = 0;

		offset += snprintf(line + offset, sizeof(line) - offset, " %20s %5s ",
				sgt.antname.c_str(),
				sgt.serialno.c_str());

		for (int i = 0; i < 3; i++)
		{
			snprintf(buf, sizeof(buf), "%6.4lf", sgt.L1[i]);
			truncateSomething(buf);
			offset += snprintf(line + offset, sizeof(line) - offset, "%s", buf);
		}

		for (int i = 0; i < 3; i++)
		{
			snprintf(buf, sizeof(buf), "%6.4lf", sgt.L2[i]);
			truncateSomething(buf);
			offset += snprintf(line + offset, sizeof(line) - offset, "%s ", buf);
		}

		offset += snprintf(line + offset, sizeof(line) - offset, "%s", sgt.calib.c_str());

		if (pstns == nullptr)
		{
			doit = true;
		}
		else
		{
			for (auto& stn : *pstns)
			{
				if (sgt.antname == stn.ant_ptr->type)
				{
					doit = true;
					break;
				}
			}
		}

		if (doit)
		{
			out << line << endl;
		}
	}
}

// compare by antenna type and serial number. return true0 if left < right
bool compare_gal_pc(Sinex_gal_phase_center_t& left, Sinex_gal_phase_center_t& right)
{
	int comp = left.antname.compare(right.antname);

	if (!comp)
		comp = left.serialno.compare(right.serialno);

	return (comp < 0);
}

// Gallileo phase centers take three line each!
void parseGalPhaseCenters(string& s_x)
{
	static int lineNum = 0;
	static string lines[3];
	lines[lineNum] = s_x;

	lineNum++;
	if (lineNum != 3)
	{
		//wait for 3 lines.
		return;
	}

	lineNum = 0;

	auto& s = lines[0];
	auto& t = lines[1];
	auto& u = lines[2];

	Sinex_gal_phase_center_t sgpct;

	sgpct.antname	= s.substr(1, 20);
	sgpct.serialno	= s.substr(22, 5);
	sgpct.calib		= s.substr(69, 10);

	int readcount1 = sscanf(s.c_str() + 28, "%6lf %6lf %6lf %6lf %6lf %6lf",
							&sgpct.L1[0],
							&sgpct.L1[1],
							&sgpct.L1[2],
							&sgpct.L5[0],
							&sgpct.L5[1],
							&sgpct.L5[2]);

		// Do we need to check the antenna name and serial each time? I am going to assume not
	int	readcount2 = sscanf(t.c_str() + 28, "%6lf %6lf %6lf %6lf %6lf %6lf",
							&sgpct.L6[0],
							&sgpct.L6[1],
							&sgpct.L6[2],
							&sgpct.L7[0],
							&sgpct.L7[1],
							&sgpct.L7[2]);
	int	readcount3 = sscanf(u.c_str() + 28, "%6lf %6lf %6lf",
							&sgpct.L8[0],
							&sgpct.L8[1],
							&sgpct.L8[2]);

	if	(  readcount1 == 6
		&& readcount2 == 6
		&& readcount3 == 3)
	{
		theSinex.list_gal_pcs.push_back(sgpct);
	}
}


void write_snx_gal_pcs(ofstream& out, list<SinexRecData>* pstns)
{
	Block block(out, "SITE/GAL_PHASE_CENTER");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& gal_pc : theSinex.list_gal_pcs)
	{
		Sinex_gal_phase_center_t& sgt = gal_pc;
		char buf[8];
		bool doit = false;

		if (pstns == nullptr)
			doit = true;
		else
		{
			for (auto& stn : *pstns)
			{
				if (sgt.antname == stn.ant_ptr->type)
				{
					doit = true;
					break;
				}
			}
		}

		if (!doit)
			continue;

		{
			char	line[81];
			int		offset = 0;

			offset += snprintf(line + offset, sizeof(line) - offset, " %20s %5s ",
					sgt.antname.c_str(),
					sgt.serialno.c_str());

			for (int i = 0; i < 3; i++)
			{
				snprintf(buf, sizeof(buf), "%6.4lf", sgt.L1[i]);
				truncateSomething(buf);
				offset += snprintf(line + offset, sizeof(line) - offset, "%s ", buf);
			}

			for (int i = 0; i < 3; i++)
			{
				snprintf(buf, sizeof(buf), "%6.4lf", sgt.L5[i]);
				truncateSomething(buf);
				offset += snprintf(line + offset, sizeof(line) - offset, "%s ", buf);
			}

			offset += snprintf(line + offset, sizeof(line) - offset, "%s", sgt.calib.c_str());
			out << line << endl;
		}

		{
			char	line[81];
			int		offset = 0;

			offset += snprintf(line + offset, sizeof(line) - offset, " %20s %5s ",
					sgt.antname.c_str(),
					sgt.serialno.c_str());

			for (int i = 0; i < 3; i++)
			{
				snprintf(buf, sizeof(buf), "%6.4lf", sgt.L6[i]);
				truncateSomething(buf);
				offset += snprintf(line + offset, sizeof(line) - offset, "%s ", buf);
			}

			for (int i = 0; i < 3; i++)
			{
				snprintf(buf, sizeof(buf), "%6.4lf", sgt.L7[i]);
				truncateSomething(buf);
				offset += snprintf(line + offset, sizeof(line) - offset, "%s ", buf);
			}

			offset += snprintf(line + offset, sizeof(line) - offset, "%s", sgt.calib.c_str());

			out << line << endl;
		}

		{
			char	line[81];
			int		offset = 0;

			offset += snprintf(line, sizeof(line), " %20s %5s ",
					sgt.antname.c_str(), sgt.serialno.c_str());

			for (int i = 0; i < 3; i++)
			{
				snprintf(buf, sizeof(buf), "%6.4lf", sgt.L8[i]);
				truncateSomething(buf);
				offset += snprintf(line + offset, sizeof(line) - offset, "%s ", buf);
			}

			offset += snprintf(line + offset, sizeof(line) - offset, "                    ");
			offset += snprintf(line + offset, sizeof(line) - offset, "%s", sgt.calib.c_str());
			out << line << endl;
		}
	}
}

void parseSiteEccentricity(string& s)
{
	const char* p = s.c_str();
	Sinex_site_ecc_t sset;

	sset.sitecode 	= trim(s.substr(1, 4));
	sset.ptcode		= s.substr(6, 2);
	sset.solnnum	= s.substr(9, 4);
	sset.typecode	= s[14];
	sset.rs			= s.substr(42, 3);
	char   junk[4];

	int readcount = sscanf(p + 16, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf %3s %8lf %8lf %8lf",
						&sset.start[0],
						&sset.start[1],
						&sset.start[2],
						&sset.end[0],
						&sset.end[1],
						&sset.end[2],
						junk,
						&sset.ecc.u(),
						&sset.ecc.n(),
						&sset.ecc.e());

	if (readcount == 10)
	{
		// see comment at top of file
		if	(  sset.start[0] != 0
			|| sset.start[1] != 0
			|| sset.start[2] != 0)
		{
			nearestYear(sset.start[0]);
		}

		if	(  sset.end[0] != 0
			|| sset.end[1] != 0
			|| sset.end[2] != 0)
		{
			nearestYear(sset.end[0]);
		}

		theSinex.map_eccentricities[sset.sitecode][sset.start] = sset;
	}
}

void write_snx_site_eccs(ofstream& out)
{
	Block block(out, "SITE/ECCENTRICITY");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& [id, setMap] : theSinex.map_eccentricities)
	for (auto it = setMap.rbegin(); it != setMap.rend(); it++)
	{
		auto& [time, set] = *it;

		if (set.used == false)
		{
			continue;
		}

		tracepdeex(0, out, " %4s %2s %4s %c %02d:%03d:%05d %02d:%03d:%05d %3s %8.4lf %8.4lf %8.4lf\n",
					set.sitecode.c_str(),
					set.ptcode.c_str(),
					set.solnnum.c_str(),
					set.typecode,
					(int)set.start[0] % 100,
					(int)set.start[1],
					(int)set.start[2],
					(int)set.end[0] % 100,
					(int)set.end[1],
					(int)set.end[2],
					set.rs.c_str(),
					set.ecc.u(),
					set.ecc.n(),
					set.ecc.e());
	}
}

bool compare_site_epochs(Sinex_solepoch_t& left, Sinex_solepoch_t& right)
{
	int comp = left.sitecode.compare(right.sitecode);
	int i = 0;

	while (!comp && i < 3)
	{
		comp = left.start[i] - right.start[i];
		i++;
	}

	return (comp < 0);
}

void parseEpochs(string& s)
{
	const char* p = s.c_str();

	Sinex_solepoch_t sst;

	sst.sitecode	= trim(s.substr(1, 4));
	sst.ptcode		= s.substr(6, 2);
	sst.solnnum		= s.substr(9, 4);
	sst.typecode	= s[14];

	int readcount = sscanf(p + 16, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf %2lf:%3lf:%5lf",
								&sst.start[0],
								&sst.start[1],
								&sst.start[2],
								&sst.end[0],
								&sst.end[1],
								&sst.end[2],
								&sst.mean[0],
								&sst.mean[1],
								&sst.mean[2]);

	if (readcount == 9)
	{
		// see comment at top of file
		if 	(  sst.start[0] != 0
			|| sst.start[1] != 0
			|| sst.start[2] != 0)
		{
			nearestYear(sst.start[0]);
		}

		if 	(  sst.end[0] != 0
			|| sst.end[1] != 0
			|| sst.end[2] != 0)
		{
			nearestYear(sst.end[0]);
		}

		if 	(  sst.mean[0] != 0
			|| sst.mean[1] != 0
			|| sst.mean[2] != 0)
		{
			nearestYear(sst.mean[0]);
		}
	}
}

void write_snx_epochs(
	Trace& out)
{
	string blockName;
	if (theSinex.epochs_have_bias)		blockName = "BIAS/EPOCHS";
	else								blockName = "SOLUTION/EPOCHS";

	Block block(out, blockName);

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& [id, sst] : theSinex.solEpochMap)
	{
		tracepdeex(0, out, " %4s %2s %4s %c %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d",
					sst.sitecode.c_str(),
					sst.ptcode	.c_str(),
					sst.solnnum	.c_str(),
					sst.typecode,
					(int)sst.start[0] % 100,
					(int)sst.start[1],
					(int)sst.start[2],
					(int)sst.end[0] % 100,
					(int)sst.end[1],
					(int)sst.end[2],
					(int)sst.mean[0] % 100,
					(int)sst.mean[1],
					(int)sst.mean[2]);
	}
}

void parseStatistics(string& s)	//todo aaron, is this type stuff really necessary
{
	string  stat = s.substr(1, 30);
	double  dval;
	int		ival;
	short	etype;

	if (s.substr(33).find(".") != string::npos)
	{
		dval = (double)atof(s.c_str() + 33);
		etype = 1;
	}
	else
	{
		ival = atoi(s.c_str() + 33);
		etype = 0;
	}

	Sinex_solstatistic_t sst;
	sst.name = trim(stat);
	sst.etype = etype;

	if (etype == 0)
		sst.value.ival = ival;

	if (etype == 1)
		sst.value.dval = dval;

	theSinex.list_statistics.push_back(sst);
}

void write_snx_statistics(ofstream& out)
{
	Block block(out, "SOLUTION/STATISTICS");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& statistic : theSinex.list_statistics)
	{
		char line[81];

		if (statistic.etype == 0) // int
			snprintf(line, sizeof(line), " %-30s %22d", statistic.name.c_str(), statistic.value.ival);

		if (statistic.etype == 1) // double
			snprintf(line, sizeof(line), " %-30s %22.15lf", statistic.name.c_str(), statistic.value.dval);

		out << line << endl;
	}
}


void parseSolutionEstimates(
	string& s)
{
	Sinex_solestimate_t sst;

	sst.file		= theSinex.currentFile;
	sst.type		= s.substr(7,	6);
	sst.sitecode	= s.substr(14,	4);
	sst.ptcode		= s.substr(19,	2);
	sst.solnnum 	= s.substr(22,	4);

	sst.index		= atoi(s.substr(1, 5).c_str());

	int	readcount	= sscanf(s.c_str() + 27, "%2lf:%3lf:%5lf",
						&sst.refepoch[0],
						&sst.refepoch[1],
						&sst.refepoch[2]);

	sst.unit		= s.substr(40,	4);

	sst.constraint	= s[45];

	readcount		+= sscanf(s.c_str() + 47, "%21lf %11lf",
						&sst.estimate,
						&sst.stddev);

	if (readcount == 5)
	{
		// see comment at top of file
		if 	( sst.refepoch[0] != 0
			||sst.refepoch[1] != 0
			||sst.refepoch[2] != 0)
		{
			nearestYear(sst.refepoch[0]);
		}

		auto it = theSinex.estimatesMap.find(sst.sitecode);
		if (it != theSinex.estimatesMap.end())
		{
			auto& firstEntry = it->second.begin()->second.begin()->second;

			if (firstEntry.file != sst.file)
			{
				BOOST_LOG_TRIVIAL(debug) << "Clearing sinex data for " << firstEntry.sitecode << " from " << firstEntry.file << " as it is being overwritten by " << theSinex.currentFile;
				theSinex.estimatesMap[sst.sitecode].clear();
			}
		}
		theSinex.estimatesMap[sst.sitecode][sst.type][sst.refepoch] = sst;
	}
}

void write_snx_estimates_from_filter(
	ofstream&	out,
	KFState&	kfState)
{
	Block block(out, "SOLUTION/ESTIMATE");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	(  key.type != KF::REC_POS
			&& key.type != KF::REC_POS_RATE
			&& key.type != KF::STRAIN_RATE)
		{
			continue;
		}

		string type;
		if		(key.type	== KF::REC_POS)			type = "STA?";
		else if (key.type	== KF::REC_POS_RATE)	type = "VEL?";
		else if (key.type	== KF::STRAIN_RATE)		type = "VEL?";			//todo aaron, scale is wrong, actually entirely untested

		if		(key.num	== 0)					type[3] = 'X';
		else if	(key.num	== 1)					type[3] = 'Y';
		else if	(key.num	== 2)					type[3] = 'Z';

		string ptcode = theSinex.map_siteids[key.str].ptcode;

		tracepdeex(0, out, " %5d %-6s %4s %2s %4d %02d:%03d:%05d %-4s %c %21.14le %11.5le\n",
				index,
				type.c_str(),
				key.str.c_str(),
				ptcode.c_str(),
				1,
				(int)theSinex.solution_end_date[0] % 100,
				(int)theSinex.solution_end_date[1],
				(int)theSinex.solution_end_date[2],
				"m",
				'9',	// TODO: replace with sst.constraint when fixed
						kfState.x(index),
				sqrt(	kfState.P(index,index)));
	}
}

// void write_snx_estimates(
// 	ofstream& out,
// 	std::list<Sinex_stn_snx_t>* pstns = nullptr)
// {
// 	out << "+SOLUTION/ESTIMATE" << endl;
//
// 	write_as_comments(out, theSinex.estimate_comments);
//
// 	for (auto& [index, sst] : theSinex.estimates_map)
// 	{
// 		bool doit = (pstns == nullptr);
//
// 		if (pstns != nullptr)
// 		{
// 			for (auto& stn : *pstns)
// 			{
// 				if (sst.sitecode.compare(stn.sitecode) == 0)
// 				{
// 					doit = true;
// 					break;
// 				}
// 			}
// 		}
//
// 		if (!doit)
// 			continue;
//
// 		char line[82];
//
// 		snprintf(line, sizeof(line), " %5d %6s %4s %2s %4s %2.2d:%3.3d:%5.5d %-4s %c %21.14le %11.5le",
// 		        sst.index,
// 		        sst.type.c_str(),
// 		        sst.sitecode.c_str(),
// 		        sst.ptcode.c_str(),
// 		        sst.solnnum.c_str(),
// 		        sst.refepoch[0] % 100,
// 		        sst.refepoch[1],
// 		        sst.refepoch[2],
// 		        sst.unit.c_str(),
// 		        sst.constraint,
// 		        sst.estimate,
// 		        sst.stddev);
//
// 		out << line << endl;
// 	}
//
// 	out << "-SOLUTION/ESTIMATE" << endl;
// }


void parseApriori(string& s)
{
	Sinex_solapriori_t sst = {};

	string index	= s.substr(1, 5);
	sst.idx			= atoi(index.c_str());
	sst.param_type	= s.substr(7, 6);
	sst.sitecode	= s.substr(14, 4);
	sst.ptcode		= s.substr(19, 2);
	sst.solnnum		= s.substr(22, 4);

	char   unit[5];

	unit[4] = '\0';

	int    readcount = sscanf(s.c_str() + 27, "%2lf:%3lf:%5lf %4s %c %21lf %11lf",
								&sst.epoch[0],
								&sst.epoch[1],
								&sst.epoch[2],
								unit,
								&sst.constraint,
								&sst.param,
								&sst.stddev);

	if (readcount == 7)
	{
		sst.unit		= unit;

		// see comment at top of file
		if 	( sst.epoch[0] != 0
			||sst.epoch[1] != 0
			||sst.epoch[2] != 0)
		{
			nearestYear(sst.epoch[0]);
		}

		theSinex.apriori_map[sst.idx] = sst;
	}
}

void write_snx_apriori(ofstream& out, list<SinexRecData>* pstns = nullptr)
{
	Block block(out, "SOLUTION/APRIORI");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& [index, apriori] : theSinex.apriori_map)
	{
		Sinex_solapriori_t& sst = apriori;
		bool doit = (pstns == nullptr);

		if (pstns)
		for (auto& stn : *pstns)
		{
			if (sst.sitecode.compare(stn.id_ptr->sitecode) == 0)
			{
				doit = true;
				break;
			}
		}

		if (!doit)
			continue;

		char line[82];

		snprintf(line, sizeof(line), " %5d %6s %4s %2s %4s %2.2d:%3.3d:%5.5d %-4s %c %21.14le %11.5le",
				sst.idx,
				sst.param_type.c_str(),
				sst.sitecode.c_str(),
				sst.ptcode.c_str(),
				sst.solnnum.c_str(),
				(int)sst.epoch[0] % 100,
				(int)sst.epoch[1],
				(int)sst.epoch[2],
				sst.unit.c_str(),
				sst.constraint,
				sst.param,
				sst.stddev);

		out << line << endl;
	}
}

void write_snx_apriori_from_stations(
	ofstream& out,
	map<string, Receiver>&		receiverMap)
{
	Block block(out, "SOLUTION/APRIORI");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	int index = 1;
	for (auto& [id, rec] : receiverMap)
	{
		if (rec.invalid)
		{
			continue;
		}

		auto& sst = rec.snx;

		for (int i = 0; i < 3; i++)
		{
			string type = "STA?";
			type[3] = 'X' + i;

			tracepdeex(0, out, " %5d %-6s %4s %2d %4s %02d:%03d:%05d %-4s %c %21.14le %11.5le\n",
					index,
					type.c_str(),
					id.c_str(),
					sst.id_ptr->ptcode.c_str(),
					1, //sst.solnnum.c_str(),
					(int)rec.aprioriTime[0] % 100,
					(int)rec.aprioriTime[1],
					(int)rec.aprioriTime[2],
					"m", //sst.unit.c_str(),
					'3',//sst.constraint,
					rec.aprioriPos(i),// sst.param,
					rec.aprioriVar(i));

			index++;
		}
	}
}

void parseNormals(string& s)
{
	Sinex_solneq_t sst;

	string parmnum	= s.substr(2, 5);
	sst.param		= atoi(parmnum.c_str());
	sst.ptype		= s.substr(7, 6);
	sst.site		= s.substr(14, 4);
	sst.pt			= s.substr(19, 2);
	sst.solnnum		= s.substr(22, 4);
	char   unit[5];

	unit[4] = '\0';

	int    readcount = sscanf(s.c_str() + 27, "%2lf:%3lf:%5lf %4s %c %21lf",
						&sst.epoch[0],
						&sst.epoch[1],
						&sst.epoch[2],
						unit,
						&sst.constraint,
						&sst.normal);

	if (readcount == 6)
	{
		sst.unit = unit;

		// see comment at top of file
		if (sst.epoch[0] != 0 || sst.epoch[1] != 0 || sst.epoch[2] != 0)
		{
			nearestYear(sst.epoch[0]);
		}

		theSinex.list_normal_eqns.push_back(sst);
	}
}

void write_snx_normal(ofstream& out, list<SinexRecData>* pstns = nullptr)
{
	Block block(out, "SOLUTION/NORMAL_EQUATION_VECTOR");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& sst : theSinex.list_normal_eqns)
	{
		bool doit = (pstns == nullptr);

		if (pstns)
		for (auto& stn : *pstns)
		{
			if (sst.site.compare(stn.id_ptr->sitecode) != 0)
			{
				doit = true;
				break;
			}
		}

		if (!doit)
			continue;

		char line[81];

		snprintf(line, sizeof(line), " %5d %6s %4s %2s %4s %2.2d:%3.3d:%5.5d %-4s %c %21.15lf",
				sst.param,
				sst.ptype.c_str(),
				sst.site.c_str(),
				sst.pt.c_str(),
				sst.solnnum.c_str(),
				(int)sst.epoch[0] % 100,
				(int)sst.epoch[1],
				(int)sst.epoch[2],
				sst.unit.c_str(),
				sst.constraint,
				sst.normal);

		out << line << endl;
	}
}

matrix_type		mat_type;
matrix_value	mat_value;

void parseMatrix(string& s)//, matrix_type type, matrix_value value)
{
// 	//todo aaron, this is only half complete, the maxrow/col arent used but should be with multiple input matrices.
	int		maxrow = 0;
	int		maxcol = 0;
	Sinex_solmatrix_t smt;

	int readcount = sscanf(s.c_str(), " %5d %5d %21lf %21lf %21lf",
						&smt.row,
						&smt.col,
						&smt.value[0],
						&smt.value[1],
						&smt.value[2]);

	if (readcount > 2)
	{
		if (smt.row < smt.col)
		{
			//xor swap
			smt.row ^= smt.col;
			smt.col ^= smt.row;
			smt.row ^= smt.col;
		}

		int covars = readcount - 2;

		for (int i = readcount - 2; i < 3; i++)
			smt.value[i] = -1;

		smt.numvals = readcount - 2;

		if (smt.row > maxrow)		maxrow = smt.row;
		if (smt.col > maxcol)		maxcol = smt.col;

		theSinex.matrix_map[mat_type][mat_value].push_back(smt);
	}
}

void parseSinexEstimates(
	string& s)
{

}

void parseSinexEstimateMatrix(
	string&	s)
{

}

void write_snx_matrices_from_filter(
	ofstream&	out,
	KFState&	kfState)
{
	const char* type_strings	[MAX_MATRIX_TYPE];
	const char* value_strings	[MAX_MATRIX_VALUE];

	type_strings[ESTIMATE]		= "SOLUTION/MATRIX_ESTIMATE";
	type_strings[APRIORI]		= "SOLUTION/MATRIX_APRIORI";
	type_strings[NORMAL_EQN]	= "SOLUTION/NORMAL_EQUATION_MATRIX";

	value_strings[CORRELATION]	= "CORR";
	value_strings[COVARIANCE]	= "COVA";
	value_strings[INFORMATION]	= "INFO";

    // just check we have some values to play with first
	if (kfState.P.rows() == 0)
		return;

	for (auto& mt : {ESTIMATE})
	for (auto& mv : {COVARIANCE})
	{
		//print header
		char header[128];
		snprintf(header, sizeof(header), "%s %c %s", type_strings[mt], 'L', mt == NORMAL_EQN ? "" : value_strings[mv]);

		Block block(out, header);

		write_as_comments(out, theSinex.blockComments[block.blockName]);

		MatrixXd& P = kfState.P;

		for (int i = 1; i <  P.rows();	i++)
		for (int j = 1; j <= i;			   )
		{
			if (P(i,j) == 0)
			{
				j++;
				continue;
			}

			//start printing a line
			tracepdeex(0, out, " %5d %5d %21.14le",	i,	j,	P(i,j));
			j++;

			for (int k = 0; k < 2; k++)
			{
				if	( (j > i)
					||(P(i,j) == 0))
				{
					break;
				}

				tracepdeex(0, out, " %21.14le", P(i,j));
				j++;
			}

			tracepdeex(0, out, "\n");
		}
	}
}


void parseDataHandling(string& s)
{
	Sinex_datahandling_t sdt;

	sdt.sitecode	= trim(s.substr(1, 4));	//4 - CDP ID
	sdt.ptcode		= s.substr(6, 2);	//2 - physical monument used at the site
	sdt.unit		= s.substr(9, 4);	//4 - units of estimate
	sdt.t			= s.substr(14, 1);	//1
	sdt.m			= s.substr(42, 1);	//1
	if (s.size() >= 75+4)
		sdt.comments	= s.substr(75, 4);	//4

	int	readcount	= sscanf(s.c_str() + 16, "%2lf:%3lf:%5lf",
					&sdt.epochstart[0],
					&sdt.epochstart[1],
					&sdt.epochstart[2]);
	readcount		+= sscanf(s.c_str() + 29, "%2lf:%3lf:%5lf",
					&sdt.epochend[0],
					&sdt.epochend[1],
					&sdt.epochend[2]);

	sscanf(s.c_str() + 44, "%12lf %7lf %9lf",
					&sdt.estimate,
					&sdt.stddev,
					&sdt.estrate);

	if (readcount >= 6) //just need a start & stop time
	{
		// see comment at top of file
		if 	( sdt.epochstart[0] != 0
			||sdt.epochstart[1] != 0
			||sdt.epochstart[2] != 0)
		{
			nearestYear(sdt.epochstart[0]);
		}
		if 	( sdt.epochend[0] != 0
			||sdt.epochend[1] != 0
			||sdt.epochend[2] != 0)
		{
			nearestYear(sdt.epochend[0]);
		}

		GTime time = sdt.epochstart;

		assert(sdt.m.size() == 1);		//todo, aaron whats this doing
		theSinex.map_data_handling[sdt.sitecode][sdt.m.front()][time] = sdt;
	}
}

void parsePrecode(string& s)
{
	Sinex_precode_t snt;

	snt.precesscode	= s.substr(1, 8);
	snt.comment		= s.substr(10);

	theSinex.list_precessions.push_back(snt);
}

void write_snx_precodes(ofstream& out)
{
	Block block(out, "PRECESSION/DATA");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& spt : theSinex.list_precessions)
	{
		char line[81];

		snprintf(line, sizeof(line), " %8s %s", spt.precesscode.c_str(), spt.comment.c_str());

		out << line << endl;
	}
}

void parseNutcode(string& s)
{
	Sinex_nutcode_t snt;

	snt.nutcode = s.substr(1, 8);
	snt.comment = s.substr(10);

	theSinex.list_nutcodes.push_back(snt);
}

void write_snx_nutcodes(ofstream& out)
{
	Block block(out, "NUTATION/DATA");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& nutcode : theSinex.list_nutcodes)
	{
		Sinex_nutcode_t& snt = nutcode;

		char line[81];

		snprintf(line, sizeof(line), " %8s %s", snt.nutcode.c_str(), snt.comment.c_str());

		out << line << endl;
	}
}

void parseSourceIds(string& s)
{
	Sinex_source_id_t ssi;

	ssi.source		= s.substr(1, 4);
	ssi.iers		= s.substr(6, 8);
	ssi.icrf		= s.substr(15, 16);
	ssi.comments	= s.substr(32);

	theSinex.list_source_ids.push_back(ssi);
}

void write_snx_sourceids(ofstream& out)
{
	Block block(out, "SOURCE/ID");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& source_id : theSinex.list_source_ids)
	{
		Sinex_source_id_t& ssi = source_id;

		char line[101];

		snprintf(line, sizeof(line), " %4s %8s %16s %s", ssi.source.c_str(), ssi.iers.c_str(), ssi.icrf.c_str(), ssi.comments.c_str());

		out << line << endl;
	}
}

bool compare_satids(Sinex_satid_t& left, Sinex_satid_t& right)
{
	char	constleft	= left.svn[0];
	char    constright	= right.svn[0];
	int     nleft	= atoi(left.svn.substr(1).c_str());
	int     nright	= atoi(right.svn.substr(1).c_str());
	int		comp;

	if (constleft == constright)
		comp = nleft - nright;
	else
		comp = constleft - constright;

	return (comp < 0);
}

void parseSatelliteIds(string& s)
{
	Sinex_satid_t sst;

	sst.svn			= s.substr(1, 4);
	sst.prn			= sst.svn[0] + s.substr(6, 2);
	sst.cospar		= s.substr(9, 9);
	sst.obsCode		= s[18];
	sst.antRcvType	= s.substr(47);

	const char* p = s.c_str() + 21;

	int 	readcount = sscanf(p, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf",
						&sst.timeSinceLaunch[0],
						&sst.timeSinceLaunch[1],
						&sst.timeSinceLaunch[2],
						&sst.timeUntilDecom[0],
						&sst.timeUntilDecom[1],
						&sst.timeUntilDecom[2]);

	if (readcount == 6)
	{
		// TODO: make the following adjustements
		// TSL if 0 is Sinex file start date
		// TUD if 0 is Sinex file end date

		theSinex.list_satids.push_back(sst);
	}
}

void write_snx_satids(ofstream& out)
{
	Block block(out, "SATELLITE/ID");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& ssi : theSinex.list_satids)
	{
		char line[101];

		snprintf(line, sizeof(line), " %4s %2s %9s %c %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %20s",
				ssi.svn.c_str(),
				ssi.prn.c_str() + 1,
				ssi.cospar.c_str(),
				ssi.obsCode,
				(int)ssi.timeSinceLaunch[0],
				(int)ssi.timeSinceLaunch[1],
				(int)ssi.timeSinceLaunch[2],
				(int)ssi.timeUntilDecom[0],
				(int)ssi.timeUntilDecom[1],
				(int)ssi.timeUntilDecom[2],
				ssi.antRcvType.c_str());

		out << line << endl;
	}
}

void parseSatelliteIdentifiers(string& s)
{
	SinexSatIdentity sst;

	sst.svn			= s.substr(1, 4);
	sst.cospar		= s.substr(6, 9);
	sst.category	= atoi(s.substr(16, 6).c_str());
	sst.blocktype	= trim(s.substr(23, 15));
	sst.comment		= s.substr(39);

	theSinex.satIdentityMap[sst.svn] = sst;

	nav.blocktypeMap[sst.svn] = sst.blocktype;
}

void write_snx_satidents(ofstream& out)
{
	Block block(out, "SATELLITE/IDENTIFIER");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& [svn, ssi] : theSinex.satIdentityMap)
	{
		char line[101];

		snprintf(line, sizeof(line), " %4s %9s %6d %-15s %s",
				ssi.svn.c_str(),
				ssi.cospar.c_str(),
				ssi.category,
				ssi.blocktype.c_str(),
				ssi.comment.c_str());

		out << line << endl;
	}
}

// NB this DOES not compare by PRN!!
bool compare_satprns(Sinex_satprn_t& left, Sinex_satprn_t& right)
{
	char	constleft	= left.svn[0];
	char    constright	= right.svn[0];
	int     nleft		= atoi(left.svn.substr(1).c_str());
	int     nright		= atoi(right.svn.substr(1).c_str());
	int		comp;

	if (constleft == constright)
		comp = nleft - nright;
	else
		comp = constleft - constright;

	return (comp < 0);
}

void parseSatPrns(string& s)
{
	Sinex_satprn_t spt;

	spt.svn			= s.substr(1, 4);
	spt.prn			= s.substr(36, 3);
	spt.comment		= s.substr(40);

	int readcount = sscanf(s.c_str() + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf",
						&spt.start[0],
						&spt.start[1],
						&spt.start[2],
						&spt.stop[0],
						&spt.stop[1],
						&spt.stop[2]);

	if (readcount == 6)
	{
		// No need to adjust years since for satellites the year is 4 digits ...
		theSinex.list_satprns.push_back(spt);

		nav.svnMap[SatSys(spt.prn.c_str())][spt.start] = spt.svn;
	}
}

void write_snx_satprns(ofstream& out)
{
	Block block(out, "SATELLITE/PRN");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	char line[101];

	for (auto& spt : theSinex.list_satprns)
	{
		snprintf(line, sizeof(line), " %4s %4.4d:%3.3d:%5.5d %4.4d:%3.3d:%5.5d %3s %s",
				spt.svn.c_str(),
				(int)spt.start[0],
				(int)spt.start[1],
				(int)spt.start[2],
				(int)spt.stop[0],
				(int)spt.stop[1],
				(int)spt.stop[2],
				spt.prn.c_str(),
				spt.comment.c_str());

		out << line << endl;
	}
}

bool compare_freq_channels(Sinex_satfreqchn_t& left, Sinex_satfreqchn_t& right)
{
	// start by comparing SVN...
	char	constleft	= left	.svn[0];
	char    constright	= right	.svn[0];
	int     nleft		= atoi(left	.svn.substr(1).c_str());
	int     nright		= atoi(right.svn.substr(1).c_str());
	int		result;

	if (constleft == constright)		result = nleft		- nright;
	else								result = constleft	- constright;

	// then by start time if the same space vehicle
	for (int i = 0; i < 3; i++)
		if (result == 0)
			result = left.start[i] - right.start[i];

	return (result < 0);
}

void parseSatFreqChannels(string& s)
{
	Sinex_satfreqchn_t	sfc;

	sfc.svn		= s.substr(1, 4);
	sfc.comment	= s.substr(40);

	int readcount = sscanf(s.c_str() + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf %3d",
						&sfc.start[0],
						&sfc.start[1],
						&sfc.start[2],
						&sfc.stop[0],
						&sfc.stop[1],
						&sfc.stop[2],
						&sfc.channel);

	if (readcount == 7)
	{
		// No need to adjust years since for satellites the year is 4 digits ...
		theSinex.list_satfreqchns.push_back(sfc);
	}
}

void write_snx_satfreqchn(ofstream& out)
{
	Block block(out, "SATELLITE/FREQUENCY_CHANNEL");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& sfc : theSinex.list_satfreqchns)
	{
		char line[101];

		snprintf(line, sizeof(line), " %4s %4.4d:%3.3d:%5.5d %4.4d:%3.3d:%5.5d %3d %s",
				sfc.svn.c_str(),
				(int)sfc.start[0],
				(int)sfc.start[1],
				(int)sfc.start[2],
				(int)sfc.stop[0],
				(int)sfc.stop[1],
				(int)sfc.stop[2],
				sfc.channel,
				sfc.comment.c_str());

		out << line << endl;
	}
}

void parseSatelliteMass(string& s)
{
	Sinex_satmass_t	ssm;

	ssm.svn		= s.substr(1, 4);
	ssm.comment	= s.substr(46);

	int readcount = sscanf(s.c_str() + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf %9lf",
						&ssm.start[0],
						&ssm.start[1],
						&ssm.start[2],
						&ssm.stop[0],
						&ssm.stop[1],
						&ssm.stop[2],
						&ssm.mass);

	if (readcount == 7)
	{
		// No need to adjust years since for satellites the year is 4 digits ...
		theSinex.map_satmasses[ssm.svn][ssm.start] = ssm;
	}
}

void write_snx_satmass(ofstream& out)
{
	Block block(out, "SATELLITE/MASS");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& [svn,	ssmMap]	: theSinex.map_satmasses)
	for (auto& [time,	ssm]	: ssmMap)
	{
		char line[101];

		snprintf(line, sizeof(line), " %4s %4.4d:%3.3d:%5.5d %4.4d:%3.3d:%5.5d %9.3lf %s",
				ssm.svn.c_str(),
				(int)ssm.start[0],
				(int)ssm.start[1],
				(int)ssm.start[2],
				(int)ssm.stop[0],
				(int)ssm.stop[1],
				(int)ssm.stop[2],
				ssm.mass,
				ssm.comment.c_str());

		out << line << endl;
	}
}

bool compare_satcom(Sinex_satcom_t& left, Sinex_satcom_t& right)
{
	// start by comparing SVN...
	char	constleft		= left.svn[0];
	char    constright		= right.svn[0];
	int     nleft			= atoi(left.svn.substr(1).c_str());
	int     nright			= atoi(right.svn.substr(1).c_str());
	int		result;

	if (constleft == constright)
		result = nleft - nright;
	else
		result = constleft - constright;

	// then by start time if the same space vehicle
	for (int i = 0; i < 3; i++)
		if (result == 0)
			result = left.start[i] - right.start[i];

	return (result < 0);
}

void parseSatelliteComs(string& s)
{
	Sinex_satcom_t	sct;

	sct.svn		= s.substr(1, 4);
	sct.comment	= s.substr(66);

	int readcount = sscanf(s.c_str() + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf %9lf %9lf %9lf",
						&sct.start[0],
						&sct.start[1],
						&sct.start[2],
						&sct.stop[0],
						&sct.stop[1],
						&sct.stop[2],
						&sct.com[0],
						&sct.com[1],
						&sct.com[2]);

	if (readcount == 9)
	{
		// No need to adjust years since for satellites the year is 4 digits ...
		theSinex.list_satcoms.push_back(sct);
	}
}

void write_snx_satcom(ofstream& out)
{
	Block block(out, "SATELLITE/COM");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& sct : theSinex.list_satcoms)
	{
		char line[101];

		snprintf(line, sizeof(line), " %4s %4.4d:%3.3d:%5.5d %4.4d:%3.3d:%5.5d %9.4lf %9.4lf %9.4lf %s",
				sct.svn.c_str(),
				(int)sct.start[0],
				(int)sct.start[1],
				(int)sct.start[2],
				(int)sct.stop[0],
				(int)sct.stop[1],
				(int)sct.stop[2],
				sct.com[0],
				sct.com[1],
				sct.com[2],
				sct.comment.c_str());

		out << line << endl;
	}
}

bool compare_satecc(Sinex_satecc_t& left, Sinex_satecc_t& right)
{
	// start by comparing SVN...
	char	constleft	= left.svn[0];
	char    constright	= right.svn[0];
	int     nleft	= atoi(left.svn.substr(1).c_str());
	int     nright	= atoi(right.svn.substr(1).c_str());
	int		result;

	if (constleft == constright)
		result = nleft - nright;
	else
		result = constleft - constright;

	// then by type (P or L)
	if (result == 0)
		result = static_cast<int>(left.type) - static_cast<int>(right.type);

	return (result < 0);
}

void parseSatelliteEccentricities(string& s)
{
	Sinex_satecc_t	set;

	set.svn		= s.substr(1, 4);
	set.equip	= s.substr(6, 20);
	set.type	= s[27];
	set.comment	= s.substr(59);

	int readcount = sscanf(s.c_str() + 29, "%9lf %9lf %9lf",
						&set.ecc[0],
						&set.ecc[1],
						&set.ecc[2]);

	if (readcount == 3)
	{
		theSinex.list_sateccs.push_back(set);
	}
}

void write_snx_satecc(ofstream& out)
{
	Block block(out, "SATELLITE/ECCENTRICITY");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& set : theSinex.list_sateccs)
	{
		char line[101];

		snprintf(line, sizeof(line), " %4s %-20s %c %9.4lf %9.4lf %9.4lf %s",
				set.svn.c_str(),
				set.equip.c_str(),
				set.type,
				set.ecc[0],
				set.ecc[1],
				set.ecc[2],
				set.comment.c_str());

		out << line << endl;
	}
}

void parseSatellitePowers(string& s)
{
	Sinex_satpower_t	spt;

	spt.svn		= s.substr(1, 4);
	spt.comment	= s.substr(41);

	int readcount = sscanf(s.c_str() + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf %4d",
						&spt.start[0],
						&spt.start[1],
						&spt.start[2],
						&spt.stop[0],
						&spt.stop[1],
						&spt.stop[2],
						&spt.power);

	if (readcount == 7)
	{
		// No need to adjust years since for satellites the year is 4 digits ...
		theSinex.map_satpowers[spt.svn][spt.start] = spt;
	}
}

void write_snx_satpower(ofstream& out)
{
	Block block(out, "SATELLITE/TX_POWER");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& [svn,	sptmap]	: theSinex.map_satpowers)
	for (auto& [time,	spt]	: sptmap)
	{
		char line[101];

		snprintf(line, sizeof(line), " %4s %4.4d:%3.3d:%5.5d %4.4d:%3.3d:%5.5d %4d %s",
				spt.svn.c_str(),
				(int)spt.start[0],
				(int)spt.start[1],
				(int)spt.start[2],
				(int)spt.stop[0],
				(int)spt.stop[1],
				(int)spt.stop[2],
				spt.power,
				spt.comment.c_str());

		out << line << endl;
	}
}

bool compare_satpc(Sinex_satpc_t& left, Sinex_satpc_t& right)
{
	// start by comparing SVN...
	char	constleft	= left.svn[0];
	char    constright	= right.svn[0];
	int     nleft	= atoi(left.svn.substr(1).c_str());
	int     nright	= atoi(right.svn.substr(1).c_str());
	int		result;

	if (constleft == constright)
		result = nleft - nright;
	else
		result = constleft - constright;

	// then by the first freq number
	if (result == 0)
		result = static_cast<int>(left.freq) - static_cast<int>(right.freq);

	return (result < 0);
}

void parseSatellitePhaseCenters(string& s)
{
	Sinex_satpc_t		spt;

	int				readcount2;

	spt.svn		= s.substr(1, 4);
	spt.freq	= s[6];
	spt.freq2	= s[29];
	spt.antenna	= s.substr(52, 10);
	spt.type	= s[63];
	spt.model	= s[65];

	int readcount = sscanf(s.c_str() + 6, "%6lf %6lf %6lf",
						&spt.zxy[0],
						&spt.zxy[1],
						&spt.zxy[2]);

	if (spt.freq2 != ' ')
	{
		readcount2 = sscanf(s.c_str() + 31, "%6lf %6lf %6lf",
						&spt.zxy2[0],
						&spt.zxy2[1],
						&spt.zxy2[2]);
	}

	if 	(   readcount	== 3
		&&( spt.freq2	== ' '
			||readcount2	== 3))
	{
		theSinex.list_satpcs.push_back(spt);
	}
}

void write_snx_satpc(ofstream& out)
{
	Block block(out, "SATELLITE/PHASE_CENTER");

	write_as_comments(out, theSinex.blockComments[block.blockName]);

	for (auto& spt : theSinex.list_satpcs)
	{
		char line[101];
		char freq2line[23];

		memset(freq2line, ' ', sizeof(freq2line));
		freq2line[22] = '\0';

		if (spt.freq2 != ' ')
			snprintf(freq2line, sizeof(freq2line), "%c %6.4lf %6.4lf %6.4lf",
					spt.freq2,
					spt.zxy2[0],
					spt.zxy2[1],
					spt.zxy2[2]);

		snprintf(line, sizeof(line), " %4s %c %6.4lf %6.4lf %6.4lf %22s %-10s %c %c",
				spt.svn.c_str(),
				spt.freq,
				spt.zxy[0],
				spt.zxy[1],
				spt.zxy[2],
				freq2line,
				spt.antenna.c_str(),
				spt.type,
				spt.model);

		out << line << endl;
	}
}

void parseSinexSatYawRates(string& line)
{
	SinexSatYawRate entry;

	entry.svn			= line.substr(1, 4);
	entry.comment		= line.substr(51);

	int readCount = sscanf(line.c_str() + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf    %c %8lf",
						&entry.start[0],
						&entry.start[1],
						&entry.start[2],
						&entry.stop[0],
						&entry.stop[1],
						&entry.stop[2],
						&entry.yawBias,
						&entry.maxYawRate);

	entry.maxYawRate *= D2R;

	if (readCount == 8)
	{
		theSinex.satYawRateMap[entry.svn][entry.start] = entry;
	}
}

void parseSinexSatAttMode(string& line)
{
	SinexSatAttMode entry;
	entry.svn			= line.substr(1, 4);
	int readCount = sscanf(line.c_str() + 6, "%4lf-%2lf-%2lf %2lf:%2lf:%2lf  %4lf-%2lf-%2lf %2lf:%2lf:%2lf ",
						&entry.start[0],
						&entry.start[1],
						&entry.start[2],
						&entry.start[3],
						&entry.start[4],
						&entry.start[5],
						&entry.stop[0],
						&entry.stop[1],
						&entry.stop[2],
						&entry.stop[3],
						&entry.stop[4],
						&entry.stop[5]);
	entry.attMode = line.substr(47);

	if (readCount == 12)
	{
		theSinex.satAttModeMap[entry.svn][entry.start] = entry;
	}
}

void nullFunction(string& s)
{

}

bool readSinex(
	string filepath,
	bool primary)
{
// 	BOOST_LOG_TRIVIAL(info)
// 	<< "reading " << filepath << std::endl;

	ifstream filestream(filepath);
	if (!filestream)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error opening sinex file" << filepath << endl;
		return false;
	}

	bool pass = read_snx_header(filestream);
	if (pass == false)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error reading header line." << endl;

		return false;
	}

	theSinex.currentFile = filepath;

	void (*parseFunction)(string&) = nullFunction;

	string			closure = "";

	bool failure = false;

	while (filestream)
	{
		string line;

		getline(filestream, line);

		// test below empty line (ie continue if something on the line)
		if	(!filestream)
		{
			// error - did not find closure line. Report and clean up.
			BOOST_LOG_TRIVIAL(error)
			<< "Error: Closure line not found before end." << endl;

			failure = true;
			break;
		}
		else if (line[0] == '*')
		{
			//comment
		}
		else if (line[0] == '-')
		{
			//end of block
			parseFunction = nullFunction;

			if (line != closure)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Error: Incorrect section closure line encountered: "
				<< closure << " != " << line << endl;
			}
		}
		else if (line[0] == ' ')
		{
			//this probably needs specialty parsing - use a prepared function pointer.
			parseFunction(line);
		}
		else if (line[0] == '+')
		{
			string	mvs;

			//prepare closing line for comparison
			closure = line;
			closure[0] = '-';

			trimCut(line);
			if		(line == "+FILE/REFERENCE"   				)	{ parseFunction = parseReference;				}
			else if	(line == "+FILE/COMMENT"					)	{ parseFunction = nullFunction;					}
			else if	(line == "+INPUT/HISTORY"					)	{ parseFunction = parseInputHistory;			}
			else if	(line == "+INPUT/FILES"						)	{ parseFunction = parseInputFiles;				}
			else if	(line == "+INPUT/ACKNOWLEDGEMENTS"			)	{ parseFunction = parseAcknowledgements;		}
			else if	(line == "+INPUT/ACKNOWLEDGMENTS"			)	{ parseFunction = parseAcknowledgements;		}
			else if	(line == "+NUTATION/DATA"					)	{ parseFunction = parseNutcode;					}
			else if	(line == "+PRECESSION/DATA"					)	{ parseFunction = parsePrecode;					}
			else if	(line == "+SOURCE/ID"						)	{ parseFunction = parseSourceIds;				}
			else if	(line == "+SITE/ID"							)	{ parseFunction = parseSiteIds;					}
			else if	(line == "+SITE/DATA"						)	{ parseFunction = parseSiteData;				}
			else if	(line == "+SITE/RECEIVER"					)	{ parseFunction = parseReceivers;				}
			else if	(line == "+SITE/ANTENNA"					)	{ parseFunction = parseAntennas;				}
			else if	(line == "+SITE/GPS_PHASE_CENTER"			)	{ parseFunction = parseGpsPhaseCenters;			}
			else if	(line == "+SITE/GAL_PHASE_CENTER"			)	{ parseFunction = parseGalPhaseCenters;			}
			else if	(line == "+SITE/ECCENTRICITY"				)	{ parseFunction = parseSiteEccentricity;		}
			else if	(line == "+BIAS/EPOCHS"						)	{ parseFunction = parseEpochs;					}
			else if	(line == "+SOLUTION/EPOCHS"					)	{ parseFunction = parseEpochs;					}
			else if	(line == "+SOLUTION/STATISTICS"				)	{ parseFunction = parseStatistics;				}
			else if	(line == "+SOLUTION/ESTIMATE"				)	{ parseFunction = parseSolutionEstimates;		}
			else if	(line == "+SOLUTION/APRIORI"				)	{ parseFunction = parseApriori;					}
			else if	(line == "+SOLUTION/NORMAL_EQUATION_VECTOR"	)	{ parseFunction = parseNormals;					}
			else if	(line == "+SOLUTION/MATRIX_ESTIMATE"		)	{ parseFunction = parseMatrix;					}
			else if	(line == "+SOLUTION/MATRIX_APRIORI"			)	{ parseFunction = parseMatrix;					}
			else if	(line == "+SOLUTION/NORMAL_EQUATION_MATRIX"	)	{ parseFunction = parseMatrix;					}
			else if	(line == "+SOLUTION/DATA_HANDLING"			)	{ parseFunction = parseDataHandling;			}
			else if	(line == "+SATELLITE/IDENTIFIER"			)	{ parseFunction = parseSatelliteIdentifiers;	}
			else if	(line == "+SATELLITE/PRN"					)	{ parseFunction = parseSatPrns;					}
			else if	(line == "+SATELLITE/MASS"					)	{ parseFunction = parseSatelliteMass;			}
			else if	(line == "+SATELLITE/FREQUENCY_CHANNEL"		)	{ parseFunction = parseSatFreqChannels;			}
			else if	(line == "+SATELLITE/TX_POWER"				)	{ parseFunction = parseSatellitePowers;			}
			else if	(line == "+SATELLITE/COM"					)	{ parseFunction = parseSatelliteComs;			}
			else if	(line == "+SATELLITE/ECCENTRICITY"			)	{ parseFunction = parseSatelliteEccentricities;	}
			else if	(line == "+SATELLITE/PHASE_CENTER"			)	{ parseFunction = parseSatellitePhaseCenters;	}
			else if	(line == "+SATELLITE/ID"					)	{ parseFunction = parseSatelliteIds;			}
			else if	(line == "+SATELLITE/YAW_BIAS_RATE"			)	{ parseFunction = parseSinexSatYawRates;		}
			else if	(line == "+SATELLITE/ATTITUDE_MODE"			)	{ parseFunction = parseSinexSatAttMode;			}
			else
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Error: unknown header line: " << line << endl;

				failure = true;
			}

// 			int 	i;
// 									failure = read_snx_matrix			(filestream, NORMAL_EQN, INFORMATION, c);			break;
// 				case 15:
// 					if (!theSinex.epochs_have_bias && !theSinex.list_solepochs.empty())
// 					{
// 						BOOST_LOG_TRIVIAL(error)
// 						<< "cannot combine BIAS/EPOCHS and SOLUTION/EPOCHS blocks." << endl;
//
// 						failure = true;
// 						break;
// 					}
//
// 					theSinex.epochs_have_bias = true;
// 					theSinex.epochcomments.insert(theSinex.epochcomments.end(), comments.begin(), comments.end());
// 					comments.clear();
// 					failure = read_snx_epochs(filestream, true);
// 					break;
//
// 				case 16:
// 					if (theSinex.epochs_have_bias && !theSinex.list_solepochs.empty())
// 					{
// 						BOOST_LOG_TRIVIAL(error)
// 						<< "cannot combine BIAS/EPOCHS and SOLUTION/EPOCHS blocks." << endl;
//
// 						failure = true;
// 						break;
// 					}
//
// 					theSinex.epochs_have_bias = false;
// 					theSinex.epochcomments			.insert(theSinex.epochcomments.end(), comments.begin(), comments.end());
// 					comments.clear();
//
// 					failure = read_snx_epochs(filestream, false);
// 					break;
//
// 				case 21:
// 					theSinex.matrix_comments.insert(theSinex.matrix_comments.end(), comments.begin(), comments.end());
// 					comments.clear();
// 					c = line[headers[i].length() + 2];
// 					mvs = line.substr(headers[i].length() + 4, 4);
//
// 					if 		(!mvs.compare("CORR"))	mv = CORRELATION;
// 					else if (!mvs.compare("COVA"))	mv = COVARIANCE;
// 					else if (!mvs.compare("INFO"))	mv = INFORMATION;
//
// 					failure = read_snx_matrix(filestream, ESTIMATE, mv, c);
// 					break;
//
// 				case 22:
// 					theSinex.matrix_comments.insert(theSinex.matrix_comments.end(), comments.begin(), comments.end());
// 					comments.clear();
// 					c = line[headers[i].length() + 2];
// 					mvs = line.substr(headers[i].length() + 4, 4);
//
// 					if 		(!mvs.compare("CORR"))	mv = CORRELATION;
// 					else if (!mvs.compare("COVA"))	mv = COVARIANCE;
// 					else if (!mvs.compare("INFO"))	mv = INFORMATION;
//
// 					failure = read_snx_matrix(filestream, APRIORI, mv, c);
// 					break;
//
// 				default:
// 					break;
// 			}
		}
		else if (line[0] == '%')
		{
			trimCut(line);
			if (line != "%ENDSNX")
			{
				// error in file. report it.
				BOOST_LOG_TRIVIAL(error)
				<< "Error: line starting '%' met not final line" << endl << line << endl;

				failure = true;
			}

			break;
		}

		if (failure)
			break;
	}

	theSinex.list_satpcs.		sort(compare_satpc);
	theSinex.list_sateccs.		sort(compare_satecc);
	theSinex.list_sitedata.		sort(compare_sitedata);
	theSinex.list_gps_pcs.		sort(compare_gps_pc);
	theSinex.list_satids.		sort(compare_satids);
	theSinex.list_satfreqchns.	sort(compare_freq_channels);
	theSinex.list_satprns.		sort(compare_satprns);
	theSinex.list_satcoms.		sort(compare_satcom);
	theSinex.list_gal_pcs.		sort(compare_gal_pc);

// 	theSinex.matrix_map[type][value].sort(compare_matrix_entries);
	dedupe_sinex();

	return failure == false;
}


void writeSinex(
	string						filepath,
	KFState&					kfState,
	map<string, Receiver>&		receiverMap)
{
	ofstream filestream(filepath);

	if (!filestream)
	{
		return;
	}

	comments_override();

	write_snx_header(filestream);

	if (!theSinex.refstrings.					empty())	{	write_snx_reference				(filestream);}
	if (!theSinex.blockComments["FILE/COMMENT"].empty())	{	write_snx_comments				(filestream);}
	if (!theSinex.inputHistory.					empty())	{	write_snx_input_history			(filestream);}
	if (!theSinex.inputFiles.					empty())	{	write_snx_input_files			(filestream);}
	if (!theSinex.acknowledgements.				empty())	{	write_snx_acknowledgements		(filestream);}

	if (!theSinex.map_siteids.					empty())	{	write_snx_siteids				(filestream);}
//	if (!theSinex.list_sitedata.				empty())	{	write_snx_sitedata				(filestream);}
	if (!theSinex.map_receivers.				empty())	{	write_snx_receivers				(filestream);}
	if (!theSinex.map_antennas.					empty())	{	write_snx_antennas				(filestream);}
//	if (!theSinex.list_gps_pcs.					empty())	{	write_snx_gps_pcs				(filestream);}
//	if (!theSinex.list_gal_pcs.					empty())	{	write_snx_gal_pcs				(filestream);}
	if (!theSinex.map_eccentricities.			empty())	{	write_snx_site_eccs				(filestream);}
	if (!theSinex.solEpochMap.					empty())	{	write_snx_epochs				(filestream);}
//	if (!theSinex.list_statistics.				empty())	{	write_snx_statistics			(filestream);}
//	if (!theSinex.estimates_map.				empty())		write_snx_estimates				(filestream);
																write_snx_estimates_from_filter	(filestream, kfState);
//	if (!theSinex.apriori_map.					empty())	{	write_snx_apriori				(filestream);}
																write_snx_apriori_from_stations (filestream, receiverMap);
// 		if (!theSinex.list_normal_eqns.			empty())	{	write_snx_normal				(filestream);}

	{
// 																write_snx_matrices				(filestream, stationListPointer);
																write_snx_matrices_from_filter	(filestream, kfState);
	}

//	if (!theSinex.list_source_ids.				empty())	{	write_snx_sourceids				(filestream);}
//	if (!theSinex.list_nutcodes.				empty())	{	write_snx_nutcodes				(filestream);}
//	if (!theSinex.list_precessions.				empty())	{	write_snx_precodes				(filestream);}

	filestream << "%ENDSNX" << endl;
}


void sinex_add_statistic(
	const string& what,
	const int val)
{
	Sinex_solstatistic_t sst;

	sst.name		= what;
	sst.etype		= 0;
	sst.value.ival	= val;

	theSinex.list_statistics.push_back(sst);
}

void sinex_add_statistic(
	const string& what,
	const double val)
{
	Sinex_solstatistic_t sst;

	sst.name		= what;
	sst.etype		= 1;
	sst.value.dval	= val;

	theSinex.list_statistics.push_back(sst);
}

int sinex_check_add_ga_reference(string solType, string peaVer, bool isTrop)
{
	// step 1: check it is not already there
	for (auto it = theSinex.refstrings.begin(); it != theSinex.refstrings.end(); it++)
	{
		if (it->find("Geoscience Australia") != string::npos)
		{
			return 1;
		}
	}

	// step 2: remove any other provider's details
	// NB we do not increment the iterator in the loop because the erase if found will do it for us
	for (auto it = theSinex.refstrings.begin(); it != theSinex.refstrings.end(); )
	{
		string	s = *it;

		if 	( s.find("DESCRIPTION") != string::npos
			||s.find("OUTPUT") 		!= string::npos
			||s.find("CONTACT") 	!= string::npos
			||s.find("SOFTWARE") 	!= string::npos
			||s.find("HARDWARE") 	!= string::npos
			||s.find("INPUT") 		!= string::npos)
		{
			it = theSinex.refstrings.erase(it);
		}
		else
		{
			it++;
		}
	}

	// step 3: put in the Geoscience reference
	struct utsname	buf;
	char 	line[81];

	snprintf(line, sizeof(line), " %-18s %s", "DESCRIPTION", "Geoscience Australia");					theSinex.refstrings.push_back(line);
	snprintf(line, sizeof(line), " %-18s %s", "OUTPUT", solType.c_str());								theSinex.refstrings.push_back(line);
	snprintf(line, sizeof(line), " %-18s %s", "CONTACT", "npi@ga.gov.au");								theSinex.refstrings.push_back(line);
	snprintf(line, sizeof(line), " %-18s %s", "SOFTWARE", ("Ginan PEA Version " + peaVer).c_str());		theSinex.refstrings.push_back(line);

	int result = uname(&buf);

	if (result == 0)
	{
		int offset = 0;

		offset += snprintf(line + offset, sizeof(line) - offset, " %-18s ", "HARDWARE");

		offset += snprintf(line + offset, sizeof(line) - offset, "%s ", buf.sysname);
		offset += snprintf(line + offset, sizeof(line) - offset, "%s ", buf.release);
		offset += snprintf(line + offset, sizeof(line) - offset, "%s ", buf.version);

		theSinex.refstrings.push_back(line);
	}

	snprintf(line, sizeof(line), " %-18s %s", "INPUT", "RINEX");
	theSinex.refstrings.push_back(line);

	if (isTrop)
	{
		snprintf(line, sizeof(line), " %-18s %03d", "VERSION NUMBER", 1); //note: increment if the processing is modified in a way that might lead to a different error characteristics of the product - see trop snx specs
		theSinex.refstrings.push_back(line);
	}
	return 0;
}

void sinex_add_comment(const string what)
{
	theSinex.blockComments["FILE/COMMENT"].push_back(what);
}

void sinex_add_files(
	const string&			who,
	const GTime&			time,
	const vector<string>&	filenames,
	const string&			description)
{
	for (auto& filename : filenames)
	{
		Sinex_input_file_t	sif;

		sif.yds			= time;
		sif.agency		= who;
		sif.file		= filename;
		sif.description	= description;

		theSinex.inputFiles.push_back(sif);
	}
}

void setRestrictiveEndTime(
	UYds& current,
	UYds& potential)
{
	UYds zeros;

	if (time_compare(current,	zeros)		== 0)	{	current = potential;	return;	}	//current is zero, just use the new version for the end time
	if (time_compare(potential,	zeros)		== 0)	{							return;	}	//potential time is zero, thats not restrictive, keep the current time
	if (time_compare(potential,	current)	<  0)	{	current = potential;	return;	}	//potential end time is more restrictive
}

GetSnxResult getStnSnx(
	string			station,
	GTime			time,
	SinexRecData&	recSnx)
{
	recSnx = SinexRecData();
	recSnx.start = time;

	GetSnxResult result;

	bool found = false;

	// search siteids for station (not time dependent)
	auto siteIdIt = theSinex.map_siteids.find(station);
	if (siteIdIt != theSinex.map_siteids.end())
	{
		auto& [dummy, siteId] = *siteIdIt;

		recSnx.id_ptr = &siteId;

		siteId.used = true;
	}
	else
	{
		result.failureSiteId = true;
	}

	auto receiverIt = theSinex.map_receivers.find(station);
	if (receiverIt != theSinex.map_receivers.end())
	{
		auto& [dummy, recTimeMap] = *receiverIt;

		auto timeRecIt = recTimeMap.lower_bound(time);
		if (timeRecIt != recTimeMap.end())
		{
			auto& [dummy, receiver] = *timeRecIt;

			receiver.used = true;

			recSnx.rec_ptr = &receiver;

			found = true;

			// get next next start time as end time for this aspect
			if (timeRecIt != recTimeMap.begin())
			{
				timeRecIt--;
				auto& nextReceiver = timeRecIt->second;

				setRestrictiveEndTime(receiver.end,	nextReceiver.start);
			}

			setRestrictiveEndTime(recSnx.start,	receiver.end);
		}
	}

	if (!found)
		result.failureReceiver = true;

	found = false;

	auto antIt = theSinex.map_antennas.find(station);
	if (antIt != theSinex.map_antennas.end())
	{
		auto& [dummy, antTimeMap] = *antIt;

		auto antIt2 = theSinex.map_antennas[station].lower_bound(time);
		if (antIt2 != theSinex.map_antennas[station].end())
		{
			auto& [dummy, antenna] = *antIt2;

			found = true;
			antenna.used = true;

			recSnx.ant_ptr = &antenna;

			// get next next start time as end time for this aspect
			if (antIt2 != theSinex.map_antennas[station].begin())
			{
				antIt2--;
				auto& [dummy, nextAntenna] = *antIt2;

				setRestrictiveEndTime(antenna.end,	nextAntenna.start);
			}

			setRestrictiveEndTime(recSnx.start,	antenna.end);
		}
	}

	if (!found)
		result.failureAntenna = true;

	found = false;

	auto eccIt = theSinex.map_eccentricities.find(station);
	if (eccIt != theSinex.map_eccentricities.end())
	{
		auto& [dummy, eccMap] = *eccIt;

		auto eccIt2 = eccMap.lower_bound(time);
		if (eccIt2 != theSinex.map_eccentricities[station].end())
		{
			auto& [dummy, ecc] = *eccIt2;

			found = true;

			ecc.used = true;

			recSnx.ecc_ptr = &ecc;

			// get next next start time as end time for this aspect
			if (eccIt2 != theSinex.map_eccentricities[station].begin())
			{
				eccIt2--;
				auto& [dummy, nextEcc] = *eccIt2;

				setRestrictiveEndTime(ecc.end,	nextEcc.start);
			}

			setRestrictiveEndTime(recSnx.stop,		ecc.end);
		}
	}

	if (!found)
		result.failureEccentricity = true;

	found = false;
//
// 	for (auto& gps_pcs : theSinex.list_gps_pcs)
// 	{
// 		if 	( (stn_snx.anttype.compare(gps_pcs.antname) == 0)
// 			&&(stn_snx.antsn.compare(gps_pcs.serialno)	== 0))
// 		{
// 			for (int i = 0; i < 3; i++)
// 			{
// 				stn_snx.gpsl1[i] = gps_pcs.L1[i];
// 				stn_snx.gpsl2[i] = gps_pcs.L2[i];
// 			}
//
// 			stn_snx.has_gps_pc	= true;
// 			found				= true;
// 			break;
// 		}
// 	}

	if (!found)
		result.failurePhaseCentre = true;

	found = true;

	for (string type : {"STA?  ", "VEL?  "})
	for (int i = 0; i < 3; i++)
	{
		type[3] = 'X' + i;

		auto& estMap = theSinex.estimatesMap[station][type];

		Sinex_solestimate_t* estimate_ptr = nullptr;

		auto est_it = estMap.lower_bound(time);
		GTime refEpoch = {};
		if (est_it != estMap.end())
		{
			estimate_ptr = &est_it->second;
			refEpoch = est_it->first;

			// get next next start time as end time for this aspect
			if (est_it != estMap.begin())
			{
				est_it--;
				auto& nextEst = est_it->second;

				setRestrictiveEndTime(recSnx.stop,		nextEst.refepoch);
			}
		}
		else
		{
			//just use the first chronologically, (last when sorted as they are) instead
			auto est_Rit = estMap.rbegin();
			if (est_Rit == estMap.rend())
			{
				//actually theres no estimate for this thing
				if (type.substr(0,3) == "STA")
					found = false;
				break;
			}

			estimate_ptr = &est_Rit->second;
			refEpoch = est_Rit->first;
		}

		auto& estimate = *estimate_ptr;

		estimate.used = true;

		if		(type.substr(0,3) == "STA")
		{
			recSnx.pos(i)	= 		estimate.estimate;
			recSnx.var(i)	= SQR(	estimate.stddev);
			recSnx.refEpoch= refEpoch;
		}
		else if	(type.substr(0,3) == "VEL")
		{
			recSnx.vel(i)	= estimate.estimate;
		}
	}

	recSnx.pos += recSnx.vel * (time - recSnx.refEpoch).to_double() / 86400 / 365.25;	//meters per year

	if (found == false)
	{
		result.failureEstimate = true;
	}

	return result;
}

GetSnxResult getSatSnx(
	string			prn,
	GTime			time,
	SinexSatSnx&	satSnx)
{
	bool found = false;

	GetSnxResult result;

	satSnx = SinexSatSnx();
	satSnx.start = time;

	// search satprns for prn and svn (not time dependent)
	for (auto& satPrn : theSinex.list_satprns)
	{
		GTime startTime	= satPrn.start;
		GTime stopTime	= satPrn.stop;

		if 	(  satPrn.prn == prn
			&&	(time - startTime)	.to_double()	>= 0
			&&(	(time - stopTime)	.to_double()	<= 0
			  ||satPrn.stop[0] == 0))
		{
			satSnx.prn	= prn;
			satSnx.svn	= satPrn.svn;
			//todo: start and stop time
			found = true;
			break;
		}
	}

	if (!found)
		result.failurePRN = true;

	// sat identifiers
	auto itr = theSinex.satIdentityMap.find(satSnx.svn);
	if (itr != theSinex.satIdentityMap.end())
	{
		auto& [dummy, satId] = *itr;

		satSnx.id_ptr = &satId;
	}
	else
	{
		result.failureSatId = true;
	}


	//todo: add other sections for satellite in theSinex

	// sat com
	found = false;
	for (auto& satCom : theSinex.list_satcoms)
	{
		GTime startTime	= satCom.start;
		GTime stopTime	= satCom.stop;

		if 	(  satCom.svn == satSnx.svn
			&& 	(time - startTime)	.to_double()	>= 0
			&&(	(time - stopTime)	.to_double()	<= 0
			  ||satCom.stop[0] == 0))
		{
			for (int i = 0; i < 3; i++)
				satSnx.com[i] = satCom.com[i];
			//todo: start and stop time
			found = true;
		}
	}

	if (!found)
		result.failureCOM = true;;

	found = false;

	// sat eccentricities
	for (auto& satEcc : theSinex.list_sateccs)
	{
		if (satEcc.svn == satSnx.svn)
		{
			E_EccType eccType;
			switch (satEcc.type)
			{
				case 'P':	{	found = true;	satSnx.ecc_ptrs[E_EccType::P_ANT] = &satEcc;	break;		}
				case 'L':	{	found = true;	satSnx.ecc_ptrs[E_EccType::L_LRA] = &satEcc;	break;		}
				default:
				{
					BOOST_LOG_TRIVIAL(error) << "Unknown satellite eccentricity type";
					break;
				}
			}
		}
	}

	if (!found)
		result.failureEccentricity = true;

	//todo: add other sections for satellite in theSinex

	return result;
}

void getRecBias(
			string				station,
	const	UYds&				yds,
			map<char, double>&	stationBias)
{
	GTime time = yds;

	// Loop through "M" models codes - Ref: https://ilrs.dgfi.tum.de/fileadmin/data_handling/ILRS_Data_Handling_File.snx
	const std::vector<char> codes = {'R', 'T', 'X', 'E', 'H', 'P', 'U', 'N', 'Q', 'V'};
	for (auto code : codes)
	{
		bool excludeFlag = false;
		auto it = theSinex.map_data_handling[station][code].lower_bound(time);
		if (it == theSinex.map_data_handling[station][code].end())
		{
			continue;
		}

		double unitsFactor = 1;
		auto& dataHandling = it->second;		//todo aaron

		const GTime stopTime = dataHandling.epochend;

		if (time >= stopTime)
		{
			continue;
		}

		switch (code)
		{
			case 'R': // Range bias to be applied, no estimation of bias
				if (dataHandling.unit == "mm  ")				unitsFactor = 1e-3;
				else
					BOOST_LOG_TRIVIAL(error) << "Error: unhandled units in " << __FUNCTION__ << ", model code R : " << dataHandling.unit << endl;
				break;
			case 'T': // Time bias in ms or µs & µs/d (T2L2) to be applied, NOT estimated
				if		(dataHandling.unit == "ms  ")			unitsFactor = 1e-3;
				else if (dataHandling.unit == "us  ")			unitsFactor = 1e-6;
				else
					BOOST_LOG_TRIVIAL(error) << "Error: unhandled units in " << __FUNCTION__ << ", model code T: " << dataHandling.unit << endl;
				break;
			case 'E': // Estimation of range bias, known a priori values are given
				if		(dataHandling.unit == "mm  ")			unitsFactor = 1e-3;
				else
					BOOST_LOG_TRIVIAL(error) << "Error: unhandled units in " << __FUNCTION__ << ", model code R: " << dataHandling.unit << endl;
				break;
			case 'H': // humidity error (correction in %)
				if		(dataHandling.unit == "%   ")			unitsFactor = 1e-3;
				else
					BOOST_LOG_TRIVIAL(error) << "Error: unhandled units in " << __FUNCTION__ << ", model code H: " << dataHandling.unit << endl;
				break;
			case 'P': // pressure bias (correction in mB)
				if		(dataHandling.unit == "mB  ")			unitsFactor = 1;
				else
					BOOST_LOG_TRIVIAL(error) << "Error: unhandled units in " << __FUNCTION__ << ", model code P: " << dataHandling.unit << endl;
				break;
			case 'U': // Estimation of time bias in ms
				if		(dataHandling.unit == "ms  ")			unitsFactor = 1e-3;
				else if (dataHandling.unit == "us  ")			unitsFactor = 1e-6;
				else
					BOOST_LOG_TRIVIAL(error) << "Error: unhandled units in " << __FUNCTION__ << ", model code U: " << dataHandling.unit << endl;
				break;
			// Equivalent to 'E':
			case 'C': // Target signature bias, correction different from standard
			case 'S': // Stanford event counter bias
				unitsFactor = 0;
				break;
			// Data to be excluded:
			case 'X': // Exclude/delete data
			case 'N': // unreliable station, should not be used in routine processing
			case 'Q': // Receiver with data in quarantine, not to be used in official products
			case 'V': // Receiver with not validated coordinates, not solving for biases
				excludeFlag = true;
				break;
		}

		if (excludeFlag)
		{
			stationBias[code] = 1; // non-zero = true
		}
		else if (dataHandling.ptcode == "--")
		{
			stationBias[code] = dataHandling.estimate * unitsFactor;

			if	( code == 'T'
				&&dataHandling.comments == "drif")	// comments only read in first 4 characters
			{
				GTime end	= dataHandling.epochend;
				GTime start	= dataHandling.epochstart;

				double interval = (end - start).to_double();
				GTime midInterval = start + interval / 2;

				double numDays = (time - midInterval).to_double() / S_IN_DAY;

				stationBias[code] += dataHandling.estrate * numDays * 1e-6; // estrate units in us/day
			}
		}
		else
		{
			BOOST_LOG_TRIVIAL(error) << "Error: unhandled ptcode in getRecBias: " << dataHandling.ptcode << endl;
		}
	}
}


/** Get yaw rate sinex entry for sat
 */
bool getSnxSatMaxYawRate(
	string	svn,
	GTime&	time,
	double&	maxYawRate)
{
	auto itr = theSinex.satYawRateMap[svn].lower_bound(time);
	if (itr == theSinex.satYawRateMap[svn].end())
		return false;

	auto& [dummy, entry] = *itr;
	maxYawRate = entry.maxYawRate;

	return true;
}

/** Get attitude mode for sat
 */
bool getSnxSatAttMode(
	string	svn,
	GTime&	time,
	string&	attMode)
{
	auto itr = theSinex.satAttModeMap[svn].lower_bound(time);
	if (itr == theSinex.satAttModeMap[svn].end())
		return false;

	auto& [dummy, entry] = *itr;
	attMode = entry.attMode;
	GTime stop = entry.stop;

	if	( stop != GTime::noTime()
		&&stop < time)
	{
		return false;
	}

	return true;
}
