#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/utsname.h>

#include <boost/log/trivial.hpp>

#include "eigenIncluder.hpp"
#include "streamTrace.hpp"
#include "algebra.hpp"
#include "station.hpp"
#include "gTime.hpp"
#include "snx.hpp"

using std::endl;
using std::getline;
using std::ifstream;
using std::ofstream;
using std::cout;			//todo aaron, should be using boost log or other



// Sinex 2.02 documentation indicates 2 digit years. >50 means 1900+N. <=50 means 2000+N
// To achieve this, when we read years, if >50 add 1900 else add 2000. This source will
// cease to work safely around 2045!
// when we write years, write out modulo 100
// This only applies to site data, for satellites it is using 4 digit years
void nearestYear(int& year)
{
	if (year > 50)	year += 1900;
	else			year += 2000;
}

Sinex theSinex(false); // the one and only sinex object.


string trim(const string& ref)
{
	int start = 0, stop = ref.length() - 1, len;

	while (start != stop && isspace(ref[start]))
		start++;

	while (stop != start && isspace(ref[stop]))
		stop--;

	len = stop - start + 1;
	return ref.substr(start, len);
}

// return seconds diff bewteen left and right. If left < right the value is negative
// each argument is given as year/doy/sod
long int time_compare(int left[3], int right[3])
{
	long int leftfull	= (left[0]	* 365 + left[1])	* 86400 + left[2];
	long int rightfull	= (right[0] * 365 + right[1])	* 86400 + right[2];

	return leftfull - rightfull;
}

bool compare(Sinex_ref_t& one, 	Sinex_ref_t& two)
{
	if (one.refline.compare(two.refline) == 0)
	{
		return true;
	}
	return false;
}

bool compare(Sinex_input_file_t& one, 	Sinex_input_file_t& two)
{
	if 	(  one.epoch[0]	== two.epoch[0]
		&& one.epoch[1]	== two.epoch[1]
		&& one.epoch[2]	== two.epoch[2]
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

bool compare(Sinex_satident_t& one, 	Sinex_satident_t& two)
{
	if 	(one.svn.compare(two.svn) == 0)
	{
		return true;
	}
	return false;
}
bool compare(Sinex_comment_t& one, 	Sinex_comment_t& two)
{
	if 	(one.cmtline.compare(two.cmtline) == 0)
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
		&&one.recstart[0]	== two.recstart[0]
		&&one.recstart[1]	== two.recstart[2]
		&&one.recstart[2]	== two.recstart[2]
		&&one.recend[0]		== two.recend[0]
		&&one.recend[1]		== two.recend[1]
		&&one.recend[2]		== two.recend[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_antenna_t& one, 	Sinex_antenna_t& two)
{
	if 	( one.sitecode.compare(two.sitecode) == 0
		&&one.antstart[0]	== two.antstart[0]
		&&one.antstart[1]	== two.antstart[2]
		&&one.antstart[2]	== two.antstart[2]
		&&one.antend[0]		== two.antend[0]
		&&one.antend[1]		== two.antend[1]
		&&one.antend[2]		== two.antend[2])
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
		&&one.eccstart[0]	== two.eccstart[0]
		&&one.eccstart[1]	== two.eccstart[2]
		&&one.eccstart[2]	== two.eccstart[2]
		&&one.eccend[0]		== two.eccend[0]
		&&one.eccend[1]		== two.eccend[1]
		&&one.eccend[2]		== two.eccend[2])
	{
		return true;
	}
	return false;
}

bool compare(Sinex_solepoch_t& one, 	Sinex_solepoch_t& two)
{
	if 	( one.sitecode.compare(two.sitecode) == 0
		&&one.start[0]	== two.start[0]
		&&one.start[1]	== two.start[2]
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
	dedupe(theSinex.commentstrings);
	dedupe(theSinex.inputHistory);
	dedupe(theSinex.inputFiles);
	dedupe(theSinex.acknowledgements);
	dedupe(theSinex.list_nutcodes);
	dedupe(theSinex.list_precessions);
	dedupe(theSinex.list_source_ids);
	dedupe(theSinex.list_satids);
	dedupe(theSinex.list_satidents);
	dedupe(theSinex.list_satprns);
	dedupe(theSinex.list_satfreqchns);
	dedupe(theSinex.list_satmasses);
	dedupe(theSinex.list_satcoms);
	dedupe(theSinex.list_satpowers);
	dedupe(theSinex.list_sateccs);
	dedupe(theSinex.list_satpcs);
	dedupe(theSinex.list_statistics);

	// 	// TODO: need to make sure sitecode & type match on index
	// site stuff
	// all data is sorted before coming in here, so it suffices to just check against the previous value
	dedupeB(theSinex.list_sitedata);
	dedupeB(theSinex.list_gps_pcs);
	dedupeB(theSinex.list_gal_pcs);
	dedupeB(theSinex.list_solepochs);
	dedupeB(theSinex.list_normal_eqns);
	Sinex_solmatrix_t me_copy;

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
int read_snx_header(std::ifstream& in)
{
	string s;

	std::getline(in, s);

	if (in.eof())
	{
		BOOST_LOG_TRIVIAL(error) << "empty file" << endl;
		return 1;
	}

	// verify line contents
	if 	(  s[0] != '%'
		|| s[1] != '='
		|| s[2] != 'S'
		|| s[3] != 'N'
		|| s[4] != 'X')
	{
		// error. not a sinex file
		BOOST_LOG_TRIVIAL(error) << "Not a sinex file" << endl;
		return 2;
	}

	// remaining characters indiciate properties of the file
	if (s.length() > 5)
	{
		const char* p = s.c_str();
		char create_agc[4];
		char data_agc[4];
		char solcontents[7];

		int  readcount = sscanf(p + 6, "%4lf %3s %2d:%3d:%5d %3s %2d:%3d:%5d %2d:%3d:%5d %c %5d %c %c %c %c %c %c %c",
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
						&theSinex.ObsCode,
						&theSinex.numparam,
						&theSinex.ConstCode,
						&solcontents[0],
						&solcontents[1],
						&solcontents[2],
						&solcontents[3],
						&solcontents[4],
						&solcontents[5]);

		if (readcount < 15)
		{
			// error, not enough parameters
			BOOST_LOG_TRIVIAL(error) << "Not enough parameters on header line (expected min 15), got " << readcount << endl;
			return 3;
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

	// Only one line for header - no sort required
	return 0;
}

void sinex_update_header(
	string& 	create_agc,
	int			create_date[3],
	string&		data_agc,
	int			soln_start[3],
	int			soln_end[3],
	const char	obsCode,
	const char	constCode,
	string&		contents)
{
	Sinex_input_history_t siht;

	siht.code			= '+';
	siht.fmt			= theSinex.ver;
	siht.create_agency	= theSinex.create_agc;
	siht.data_agency	= theSinex.data_agc;
	siht.obs_tech		= theSinex.ObsCode;
	siht.constraint		= theSinex.ConstCode;
	siht.num_estimates	= theSinex.numparam;
	siht.contents		= theSinex.solcont;

	memcpy(siht.create_time,	theSinex.filedate,				sizeof(siht.create_time));
	memcpy(siht.start,			theSinex.solution_start_date,	sizeof(siht.start));
	memcpy(siht.stop,			theSinex.solution_end_date,		sizeof(siht.stop));

	theSinex.inputHistory.push_back(siht);

	theSinex.ver = 2.02;	// Fix this if the sinex format gets updated!

	if (data_agc.size() > 0)
		theSinex.data_agc = data_agc;
	else
		theSinex.data_agc = theSinex.create_agc;

	theSinex.create_agc	= create_agc;
	theSinex.solcont	= contents;


	memcpy(theSinex.filedate,				create_date,	sizeof(theSinex.filedate));
	memcpy(theSinex.solution_start_date,	soln_start,		sizeof(theSinex.solution_start_date));
	memcpy(theSinex.solution_end_date,		soln_end,		sizeof(theSinex.solution_end_date));

	if (obsCode		!= ' ')
		theSinex.ObsCode = obsCode;

	if (constCode 	== ' ')
		theSinex.ConstCode = constCode;

	theSinex.numparam = theSinex.kfState.x.rows()-1;
}

void write_snx_header(std::ofstream& out)
{
	char line[80];
	char c;
	int  i;

	memset(line, 0, 80);

	sprintf(line, "%%=SNX %4.2lf %3s %2.2d:%3.3d:%5.5d %3s %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %c %5d %c",
			theSinex.ver,
			theSinex.create_agc.c_str(),
			theSinex.filedate[0] % 100,
			theSinex.filedate[1],
			theSinex.filedate[2],
			theSinex.data_agc.c_str(),
			theSinex.solution_start_date[0] % 100,
			theSinex.solution_start_date[1],
			theSinex.solution_start_date[2],
			theSinex.solution_end_date[0] % 100,
			theSinex.solution_end_date[1],
			theSinex.solution_end_date[2],
			theSinex.ObsCode,
			theSinex.numparam,
			theSinex.ConstCode);

	i = 0;
	c = theSinex.solcont[0];

	while (c != ' ')
	{
		char s[3];

		sprintf(s, " %c", c);
		strcat(line, s);

		++i;

		if (i <= static_cast<int>(theSinex.solcont.length()))
			c = theSinex.solcont[i];
		else
			c = ' ';
	}

	out << line << endl;
}

void parse_snx_reference(string& s)
{
	Sinex_ref_t srt;
	srt.refline = s;
	theSinex.refstrings.push_back(srt);
}

void write_as_comments(ofstream& out, list<string>& comments)
{
	for (auto& comment : comments)
	{
		string s = comment;

		// just make sure it starts with * as required by format
		s[0] = '*';

		out << s << endl;
	}
}

void write_pretty_line(ofstream& out)
{
	out << '*'+string(79, '-') << endl;
}

void comments_override()
{
	// overriding only those that can be found in IGS/CODE/GRG SINEX files
	if (!theSinex.inputHistory.			empty()) {theSinex.historyComments.		clear(); 	theSinex.historyComments.		push_back("*_VERSION_ CRE __CREATION__ OWN _DATA_START_ __DATA_END__ T PARAM S ____TYPE____");	}	// INPUT/HISTORY
	if (!theSinex.inputFiles.			empty()) {theSinex.filesComments.		clear(); 	theSinex.filesComments.			push_back("*OWN __CREATION__ ___________FILENAME__________ ___________DESCRIPTION__________");	}	// INPUT/FILES
	if (!theSinex.acknowledgements.		empty()) {theSinex.ackComments.			clear(); 	theSinex.ackComments.			push_back("*AGY ______________________________FULL_DESCRIPTION_____________________________");	}	// INPUT/ACKNOWLEDGEMENTS
	if (!theSinex.map_siteids.			empty()) {theSinex.siteIdcomments.		clear();	theSinex.siteIdcomments.		push_back("*CODE PT __DOMES__ T _STATION DESCRIPTION__ _LONGITUDE_ _LATITUDE__ HEIGHT_");		}	// SITE/ID
	if (!theSinex.list_sitedata.		empty()) {theSinex.siteDatacomments.	clear(); 	theSinex.siteDatacomments.		push_back("*CODE PT SOLN CODE PT SOLN T _DATA START_ _DATA END___ OWN _FILE TIME__");			}	// SITE/DATA
	if (!theSinex.map_receivers.		empty()) {theSinex.receivercomments.	clear(); 	theSinex.receivercomments.		push_back("*CODE PT SOLN T _DATA START_ _DATA END___ _RECEIVER TYPE______ _S/N_ _FIRMWARE__");	}	// SITE/RECEIVER
	if (!theSinex.map_antennas.			empty()) {theSinex.antennacomments.		clear(); 	theSinex.antennacomments.		push_back("*CODE PT SOLN T _DATA START_ __DATA END__ __ANTENNA TYPE______ _S/N_");				}	// SITE/ANTENNA
	if (!theSinex.list_gps_pcs.			empty()) {theSinex.gps_pc_comments.		clear();	theSinex.gps_pc_comments.		push_back("________TYPE________ _S/N_ _L1_U_ _L1_N_ _L1_E_ _L2_U_ _L2_N_ _L2_E_ __MODEL___");	}	// SITE/GPS_PHASE_CENTER
	if (!theSinex.map_eccentricities.	empty()) {theSinex.site_ecc_comments.	clear(); 	theSinex.site_ecc_comments.		push_back("*                                             _UP_____ _NORTH__ _EAST___\n*CODE PT SOLN T _DATA START_ __DATA END__ TYP __ARP-BENCHMARK (M)_______"); }// SITE/ECCENTRICITY
												{theSinex.estimate_comments.	clear(); 	theSinex.estimate_comments.		push_back("*INDEX _TYPE_ CODE PT SOLN _REF_EPOCH__ UNIT S ___ESTIMATED_VALUE___ __STD_DEV__");	}	// BIAS/EPOCHS|SOLUTION/EPOCHS|SOLUTION/ESTIMATE
	if (!theSinex.list_statistics.		empty()) {theSinex.statistics_comments.	clear();	theSinex.statistics_comments.	push_back("*_STATISTICAL PARAMETER________ __VALUE(S)____________");						}	// SOLUTION/STATISTICS
	if (!theSinex.apriori_map.			empty()) {theSinex.apriori_comments.	clear(); 	theSinex.apriori_comments.		push_back("*INDEX _TYPE_ CODE PT SOLN _REF_EPOCH__ UNIT S __APRIORI VALUE______ _STD_DEV___");	}	// SOLUTION/APRIORI
	if (!theSinex.list_normal_eqns.		empty()) {theSinex.normal_eqns_comments.clear();	theSinex.normal_eqns_comments.	push_back("*INDEX TYPE__ CODE PT SOLN _REF_EPOCH__ UNIT S __RIGHT_HAND_SIDE____");		}	// SOLUTION/NORMAL_EQUATION_VECTOR
												{theSinex.matrix_comments.		clear(); 	theSinex.matrix_comments.		push_back("*PARA1 PARA2 _______PARA2+0_______ _______PARA2+1_______ _______PARA2+2_______");	}	// SOLUTION/MATRIX_ESTIMATE|SOLUTION/MATRIX_APRIORI|SOLUTION/NORMAL_EQUATION_MATRIX
	if (!theSinex.list_satpcs.			empty()) {theSinex.satpc_comments.		clear(); 	theSinex.satpc_comments.		push_back("*SITE L SATA_Z SATA_X SATA_Y L SATA_Z SATA_X SATA_Y MODEL_____ T M");				}	// SATELLITE/PHASE_CENTER
	if (!theSinex.list_satids.			empty()) {theSinex.satid_comments.		clear(); 	theSinex.satid_comments.		push_back("SATELLITE/ID *SITE PR COSPAR___ T DATA_START__ DATA_END____ ANTENNA_____________");	}	// SATELLITE/ID
}

int write_snx_reference(ofstream& out)
{
	out << "+FILE/REFERENCE" << endl;

	for (auto& refString : theSinex.refstrings)
	{
		out << refString.refline << endl;
	}

	out << "-FILE/REFERENCE" << endl;

	return 0;
}

void parse_snx_comment(string& s)
{
	Sinex_comment_t sct;
	sct.cmtline = s;
	theSinex.commentstrings.push_back(sct);
}

int write_snx_comments(ofstream& out)
{
	out << "+FILE/COMMENT" << endl;

	for (auto& commentstring : theSinex.commentstrings)
	{
		out << commentstring.cmtline << endl;
	}

	out << "-FILE/COMMENT" << endl;

	return 0;
}

void parse_snx_inputHistory(string& s)
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

		readcount = sscanf(p + 6, "%4lf %3s %2d:%3d:%5d %3s %2d:%3d:%5d %2d:%3d:%5d %c %5d %c %c %c %c %c %c %c",
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
		else
		{
			// add to comment line, user will spot it and fix the error
			theSinex.historyComments.push_back(s);
		}
	}
}

int write_snx_input_history(ofstream& out)
{
	out << "+INPUT/HISTORY" << endl;

	write_as_comments(out, theSinex.historyComments);

	for (auto it = theSinex.inputHistory.begin(); it != theSinex.inputHistory.end(); it++)
	{
		char line[80];
		Sinex_input_history_t siht = *it;
		int i = 0;
		char c;

		memset (line, 0, 80);

		sprintf(line, " %cSNX %4.2lf %3s %2.2d:%3.3d:%5.5d %3s %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %c %5d %c",
				siht.code,
				siht.fmt,
				siht.create_agency.c_str(),
				siht.create_time[0] % 100,
				siht.create_time[1],
				siht.create_time[2],
				siht.data_agency.c_str(),
				siht.start[0] % 100,
				siht.start[1],
				siht.start[2],
				siht.stop[0] % 100,
				siht.stop[1],
				siht.stop[2],
				siht.obs_tech,
				siht.num_estimates,
				siht.constraint);

		c = siht.contents[i];

		while (c != ' ')
		{
			char s[3];

			s[0] = ' ';
			s[1] = c;
			s[2] = '\0';

			strcat(line, s);
			i++;

			if (static_cast<int>(siht.contents.length()) >= i)
				c = siht.contents[i];
			else
				c = ' ';
		}

		out << line << endl;
	}

	out << "-INPUT/HISTORY" << endl;

	return 0;
}

void parse_snx_inputFiles(string& s)
{
	Sinex_input_file_t sif;
	char agency[4];
	const char* p	= s.c_str();
	sif.file		= s.substr(18, 29);
	sif.description	= s.substr(48, 32);

	int  readcount = sscanf(p + 1, "%3s %2d:%3d:%5d",
							agency,
							&sif.epoch[0],
							&sif.epoch[1],
							&sif.epoch[2]);

	if (readcount == 4)
	{
		sif.agency = agency;

		nearestYear(sif.epoch[0]);

		theSinex.inputFiles.push_back(sif);
	}
	else
	{
		// error on line. Add to comments? user can fix it if they write it out again?
		theSinex.filesComments.push_back(s);
	}
}

int write_snx_input_files(ofstream& out)
{
	out << "+INPUT/FILES" << endl;

	write_as_comments(out, theSinex.filesComments);

	for (auto& inputFile : theSinex.inputFiles)
	{
		Sinex_input_file_t& sif = inputFile;

		char line[81];
		int len;
		sprintf(line, " %3s %2.2d:%3.3d:%5.5d ",
				sif.agency.c_str(),
				sif.epoch[0] % 100,
				sif.epoch[1],
				sif.epoch[2]);

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

	out << "-INPUT/FILES" << endl;

	return 0;
}

void parse_snx_acknowledgements(string& s)
{
	Sinex_ack_t sat;

	sat.description	= s.substr(5);
	sat.agency		= s.substr(1, 3);

	theSinex.acknowledgements.push_back(sat);
}

int write_snx_acknowledgements(ofstream& out)
{
	out << "+INPUT/ACKNOWLEDGEMENTS" << endl;

	write_as_comments(out, theSinex.ackComments);

	for (auto& acknowledgement : theSinex.acknowledgements)
	{
		Sinex_ack_t& ack = acknowledgement;

		char line[81];
		sprintf(line, " %3s %s", ack.agency.c_str(), ack.description.c_str());

		out << line << endl;
	}

	out << "-INPUT/ACKNOWLEDGEMENTS" << endl;

	return 0;
}

// compare by sitecode only.
static bool compare_siteids(const Sinex_siteid_t& left, const Sinex_siteid_t& right)
{
	return left.sitecode.compare(right.sitecode) < 0;
}

void parse_snx_siteIds(string& s)
{
	const char* p = s.c_str();
	Sinex_siteid_t sst;

	sst.sitecode	= s.substr(1, 4);
	sst.ptcode		= s.substr(6, 2);
	sst.domes		= s.substr(9, 9);
	sst.typecode 	= s[19];
	sst.desc		= s.substr(21, 22);


	int    readcount = sscanf(p + 44, "%3d %2d %4lf %3d %2d %4lf %7lf",
						&sst.long_deg,
						&sst.long_min,
						&sst.long_sec,
						&sst.lat_deg,
						&sst.lat_min,
						&sst.lat_sec,
						&sst.height);

	if (readcount == 7)
	{
		theSinex.map_siteids[sst.sitecode] = sst;
	}
	else
	{
		theSinex.siteIdcomments.push_back(s);
	}
}

int write_snx_siteids(ofstream& out)
{
	out << "+SITE/ID" << endl;

	write_as_comments(out, theSinex.siteIdcomments);
	
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
				ssi.long_deg,
				ssi.long_min,
				ssi.long_sec,
				ssi.lat_deg,
				ssi.lat_min,
				ssi.lat_sec,
				ssi.height);
	}

	out << "-SITE/ID" << endl;

	return 0;
}

// compare by the 2 station ids only. 
static bool compare_sitedata(const Sinex_sitedata_t& left, const Sinex_sitedata_t& right)
{
	int sitec = left.site.compare(right.site);

	if (sitec == 0)
		sitec = left.sitecode.compare(right.sitecode);

	return (sitec < 0);
}

void parse_snx_siteData(string& s)
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
	int    start[3]; // yr:doy:sod
	int    end[3]; //yr:doy:sod
	int    create[3]; //yr:doy:sod
	char   agency[4];

	int    readcount;

	readcount = sscanf(p + 28, "%2d:%3d:%5d %2d:%3d:%5d %3s %2d:%3d:%5d",
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

		for (int i = 0; i < 3; i++)
		{
			sst.start[i]	= start[i];
			sst.stop[i]		= end[i];
			sst.create[i]	= create[i];
		}

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
	else
	{
		// treat line as a comment. User can fix it if they write it out
		theSinex.siteDatacomments.push_back(s);
	}
}

int write_snx_sitedata(ofstream& out, std::list<Sinex_stn_snx_t>* pstns)
{
	out << "+SITE/DATA" << endl;

	write_as_comments(out, theSinex.siteDatacomments);

	for (auto& sitedata : theSinex.list_sitedata)
	{
		Sinex_sitedata_t& ssd = sitedata;
		bool doit = false;

		char line[81];
		sprintf(line, " %4s %2s %4s %4s %2s %4s %c %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %3s %2.2d:%3.3d:%5.5d",
				ssd.site.c_str(),
				ssd.station_pt.c_str(),
				ssd.soln_id.c_str(),
				ssd.sitecode.c_str(),
				ssd.site_pt.c_str(),
				ssd.sitesoln.c_str(),
				ssd.obscode,
				ssd.start[0] % 100,
				ssd.start[1],
				ssd.start[2],
				ssd.stop[0] % 100,
				ssd.stop[1],
				ssd.stop[2],
				ssd.agency.c_str(),
				ssd.create[0] % 100,
				ssd.create[1],
				ssd.create[2]);

		if (pstns == NULL)
			doit = true;
		else
		{
			for (auto& stn : *pstns)
			{
				if (ssd.site.compare(stn.sitecode) == 0)
				{
					doit = true;
					break;
				}
			}
		}

		if (doit)
			out << line << endl;
	}

	out << "-SITE/DATA" << endl;
	return 0;
}

// compare by site code and start time. 
static bool compare_receivers(const Sinex_receiver_t& left, const Sinex_receiver_t& right)
{
	// Always upper case so no need to case compare
	int comp = left.sitecode.compare(right.sitecode);

	int i = 0;

	while (comp == 0 && i < 3)
	{
		comp = left.recstart[i] - right.recstart[i];
		i++;
	}

	return (comp < 0);
}

void parse_snx_receivers(string& s)
{
	const char* p = s.c_str();
	
	Sinex_receiver_t srt;

	srt.sitecode	= s.substr(1, 4);
	srt.ptcode		= s.substr(6, 2);
	srt.solnid		= s.substr(9, 4);
	srt.typecode	= s[14];
	srt.rectype		= s.substr(42, 20);
	srt.recsn		= s.substr(63, 5);
	srt.recfirm 	= trim(s.substr(69, 11));
	int start[3];
	int stop[3];
	int readcount;

	readcount = sscanf(p + 16, "%2d:%3d:%5d %2d:%3d:%5d",
						&srt.recstart[0],
						&srt.recstart[1],
						&srt.recstart[2],
						&srt.recend[0],
						&srt.recend[1],
						&srt.recend[2]);

	if (readcount == 6)
	{
		// see comment at top of file
		if 	(  srt.recstart[0] != 0
			|| srt.recstart[1] != 0
			|| srt.recstart[2] != 0)
		{
			nearestYear(srt.recstart[0]);
		}

		if	(  srt.recend[0] != 0 
			|| srt.recend[1] != 0 
			|| srt.recend[2] != 0)
		{
			nearestYear(srt.recend[0]);
		}
		
		GTime time = yds2time(srt.recstart);
		theSinex.map_receivers[srt.sitecode][time] = srt;
	}
	else
	{
		// did not read properly. Treat as a comment
		theSinex.receivercomments.push_back(s);
	}
}

int write_snx_receivers(ofstream& out)
{
	out << "+SITE/RECEIVER" << endl;

	write_as_comments(out, theSinex.receivercomments);

	for (auto& [site, timemap] : theSinex.map_receivers)
	for (auto it = timemap.rbegin(); it != timemap.rend(); it++)
	{
		auto& [time, receiver] = *it;
		
		if (receiver.used == false)
		{
			continue;
		}

		tracepdeex(0, out, " %4s %2s %4s %c %02d:%03d:%05d %02d:%03d:%05d %20s %5s %s\n",
					receiver.sitecode.c_str(),
					receiver.ptcode.c_str(),
					receiver.solnid.c_str(),
					receiver.typecode,
					receiver.recstart[0] % 100,
					receiver.recstart[1],
					receiver.recstart[2],
					receiver.recend[0] % 100,
					receiver.recend[1],
					receiver.recend[2],
					receiver.rectype.c_str(),
					receiver.recsn.c_str(),
					receiver.recfirm.c_str());
	}

	out << "-SITE/RECEIVER" << endl;
	return 0;
}

// compare by sitecode and start time.
static bool compare_antennas(const Sinex_antenna_t& left, Sinex_antenna_t& right)
{
	int comp = left.sitecode.compare(right.sitecode);

	int i = 0;

	while (!comp && i < 3)
	{
		comp = left.antstart[i] - right.antstart[i];
		i++;
	}

	return (comp < 0);
}

void parse_snx_antennas(string& s)
{
	const char* p = s.c_str();
	
	Sinex_antenna_t ant;

	ant.sitecode	= s.substr(1, 4);
	ant.ptcode		= s.substr(6, 2);
	ant.solnnum		= s.substr(9, 4);
	ant.typecode	= s[14];
	ant.anttype		= s.substr(42, 20);
	ant.antsn		= trim(s.substr(63, 5));

	int    readcount = sscanf(p + 16, "%2d:%3d:%5d %2d:%3d:%5d",
								&ant.antstart[0],
								&ant.antstart[1],
								&ant.antstart[2],
								&ant.antend[0],
								&ant.antend[1],
								&ant.antend[2]);

	if (readcount == 6)
	{
		// see comment at top of file
		if 	(  ant.antstart[0] != 0
			|| ant.antstart[1] != 0
			|| ant.antstart[2] != 0)
		{
			nearestYear(ant.antstart[0]);
		}

		if 	(  ant.antend[0] != 0
			|| ant.antend[1] != 0
			|| ant.antend[2] != 0)
		{
			nearestYear(ant.antend[0]);
		}
		GTime time = yds2time(ant.antstart);
		theSinex.map_antennas[ant.sitecode][time] = ant;
// 				theSinex.list_antennas.push_back(ant);
	}
	else
	{
		// treat as comment.
		theSinex.antennacomments.push_back(s);
	}
}

int write_snx_antennas(ofstream& out)
{
	out << "+SITE/ANTENNA" << endl;

	write_as_comments(out, theSinex.antennacomments);

	for (auto& [site, antmap]	: theSinex.map_antennas)
	for (auto it = antmap.rbegin(); it != antmap.rend(); it++)
	{
		auto& [time, ant] = *it;
		
		if (ant.used == false)
		{
			continue;
		}
		
		tracepdeex(0, out, " %4s %2s %4s %c %02d:%03d:%05d %02d:%03d:%05d %20s %s\n",
					ant.sitecode.c_str(),
					ant.ptcode.c_str(),
					ant.solnnum.c_str(),
					ant.typecode,
					ant.antstart[0] % 100,
					ant.antstart[1],
					ant.antstart[2],
					ant.antend[0] % 100,
					ant.antend[1],
					ant.antend[2],
					ant.anttype.c_str(),
					ant.antsn.c_str());
	}

	out << "-SITE/ANTENNA" << endl;
	return 0;
}

// compare by antenna type and serial number.
static bool compare_gps_pc(Sinex_gps_phase_center_t& left, Sinex_gps_phase_center_t& right)
{
	int comp = left.antname.compare(right.antname);

	if (!comp)
		comp = left.serialno.compare(right.serialno);

	return (comp < 0);
}

void parse_gps_phaseCenters(string& s)
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
	else
	{
		// treat as comment line
		theSinex.gps_pc_comments.push_back(s);
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


int write_snx_gps_pcs(ofstream& out, std::list<Sinex_stn_snx_t>* pstns)
{
	out << "+SITE/GPS_PHASE_CENTER" << endl;

	write_as_comments(out, theSinex.gps_pc_comments);

	for (auto& gps_pc : theSinex.list_gps_pcs)
	{
		Sinex_gps_phase_center_t& sgt = gps_pc;
		char buf[8];
		bool doit = false;

		char line[81];

		sprintf(line, " %20s %5s ",
				sgt.antname.c_str(), sgt.serialno.c_str());

		for (int i = 0; i < 3; i++)
		{
			sprintf(buf, "%6.4lf", sgt.L1[i]);
			truncateSomething(buf);
			strcat(line, buf);
			strcat(line, " ");
		}

		for (int i = 0; i < 3; i++)
		{
			sprintf(buf, "%6.4lf", sgt.L2[i]);
			truncateSomething(buf);
			strcat(line, buf);
			strcat(line, " ");
		}

		strcat(line, sgt.calib.c_str());

		if (pstns == NULL)
		{
			doit = true;
		}
		else
		{
			for (auto& stn : *pstns)
			{
				if (sgt.antname.compare(stn.anttype) == 0)
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

	out << "-SITE/GPS_PHASE_CENTER" << endl;
	return 0;
}

// compare by antenna type and serial number. return true0 if left < right
static bool compare_gal_pc(Sinex_gal_phase_center_t& left, Sinex_gal_phase_center_t& right)
{
	int comp = left.antname.compare(right.antname);

	if (!comp)
		comp = left.serialno.compare(right.serialno);

	return (comp < 0);
}

// Gallileo phase centers take three line each!
void parse_gal_phaseCenters(string& s_x)
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
	else
	{
		theSinex.gal_pc_comments.push_back(s);
	}
}


int write_snx_gal_pcs(ofstream& out, std::list<Sinex_stn_snx_t>* pstns)
{
	out << "+SITE/GAL_PHASE_CENTER" << endl;

	write_as_comments(out, theSinex.gal_pc_comments);

	for (auto& gal_pc : theSinex.list_gal_pcs)
	{
		Sinex_gal_phase_center_t& sgt = gal_pc;
		char buf[8];
		bool doit = false;

		if (pstns == NULL)
			doit = true;
		else
		{
			for (auto& stn : *pstns)
			{
				if (sgt.antname.compare(stn.anttype) == 0)
				{
					doit = true;
					break;
				}
			}
		}

		if (!doit)
			continue;

		char line[81];

		sprintf(line, " %20s %5s ",
				sgt.antname.c_str(), sgt.serialno.c_str());

		for (int i = 0; i < 3; i++)
		{
			sprintf(buf, "%6.4lf", sgt.L1[i]);
			truncateSomething(buf);
			strcat(line, buf);
			strcat(line, " ");
		}

		for (int i = 0; i < 3; i++)
		{
			sprintf(buf, "%6.4lf", sgt.L5[i]);
			truncateSomething(buf);
			strcat(line, buf);
			strcat(line, " ");
		}

		strcat(line, sgt.calib.c_str());
		out << line << endl;

		sprintf(line, " %20s %5s ",
				sgt.antname.c_str(),
				sgt.serialno.c_str());

		for (int i = 0; i < 3; i++)
		{
			sprintf(buf, "%6.4lf", sgt.L6[i]);
			truncateSomething(buf);
			strcat(line, buf);
			strcat(line, " ");
		}

		for (int i = 0; i < 3; i++)
		{
			sprintf(buf, "%6.4lf", sgt.L7[i]);
			truncateSomething(buf);
			strcat(line, buf);
			strcat(line, " ");
		}

		strcat(line, sgt.calib.c_str());
		out << line << endl;

		sprintf(line, " %20s %5s ",
				sgt.antname.c_str(), sgt.serialno.c_str());

		for (int i = 0; i < 3; i++)
		{
			sprintf(buf, "%6.4lf", sgt.L8[i]);
			truncateSomething(buf);
			strcat(line, buf);
			strcat(line, " ");
		}

		strcat(line, "                    ");
		strcat(line, sgt.calib.c_str());
		out << line << endl;
	}

	out << "-SITE/GAL_PHASE_CENTER" << endl;
	return 0;
}

static bool compare_eccentricity(Sinex_site_ecc_t& left, Sinex_site_ecc_t& right)
{
	int comp = left.sitecode.compare(right.sitecode);
	int i = 0;

	while (!comp && i < 3)
	{
		comp = left.eccstart[i] - right.eccstart[i];
		i++;
	}

	return (comp < 0);
}

void parse_snx_siteEccentricity(string& s)
{
	const char* p = s.c_str();
	Sinex_site_ecc_t sset;

	sset.sitecode 	= s.substr(1, 4);
	sset.ptcode		= s.substr(6, 2);
	sset.solnnum	= s.substr(9, 4);
	sset.typecode	= s[14];
	sset.eccrs		= s.substr(42, 3);
	char   junk[4];

	int readcount = sscanf(p + 16, "%2d:%3d:%5d %2d:%3d:%5d %3s %8lf %8lf %8lf",
						&sset.eccstart[0],
						&sset.eccstart[1],
						&sset.eccstart[2],
						&sset.eccend[0],
						&sset.eccend[1],
						&sset.eccend[2],
						junk,
						&sset.ecc[2],
						&sset.ecc[1],
						&sset.ecc[0]);

	if (readcount == 10)
	{
		// see comment at top of file
		if	(  sset.eccstart[0] != 0 
			|| sset.eccstart[1] != 0 
			|| sset.eccstart[2] != 0)
		{
			nearestYear(sset.eccstart[0]);
		}

		if	(  sset.eccend[0] != 0
			|| sset.eccend[1] != 0 
			|| sset.eccend[2] != 0)
		{
			nearestYear(sset.eccend[0]);
		}

		GTime time = yds2time(sset.eccstart);
		theSinex.map_eccentricities[sset.sitecode][time] = sset;
	}
	else
	{
		theSinex.site_ecc_comments.push_back(s);
	}
}

int write_snx_site_eccs(ofstream& out)
{
	out << "+SITE/ECCENTRICITY" << endl;

	write_as_comments(out, theSinex.site_ecc_comments);

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
					set.eccstart[0] % 100,
					set.eccstart[1],
					set.eccstart[2],
					set.eccend[0] % 100,
					set.eccend[1],
					set.eccend[2],
					set.eccrs.c_str(),
					set.ecc[2],
					set.ecc[1],
					set.ecc[0]);
	}

	out << "-SITE/ECCENTRICITY" << endl;
	return 0;
}

static bool compare_site_epochs(Sinex_solepoch_t& left, Sinex_solepoch_t& right)
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

void parse_snx_epochs(string& s)
{
	const char* p = s.c_str();
	
	Sinex_solepoch_t sst;

	sst.sitecode	= s.substr(1, 4);
	sst.ptcode		= s.substr(6, 2);
	sst.solnnum		= s.substr(9, 4);
	sst.typecode	= s[14];

	int readcount = sscanf(p + 16, "%2d:%3d:%5d %2d:%3d:%5d %2d:%3d:%5d",
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
	else
	{
		theSinex.epochcomments.push_back(s);
	}
}

int write_snx_epochs(ofstream& out, std::list<Sinex_stn_snx_t>* pstns)
{
	if (theSinex.epochs_have_bias)
		out << "+BIAS/EPOCHS" << endl;
	else
		out << "+SOLUTION/EPOCHS" << endl;

	write_as_comments(out, theSinex.epochcomments);

	for (auto& solepoch : theSinex.list_solepochs)
	{
		Sinex_solepoch_t& sst = solepoch;
		bool doit = false;
		char line[81];


		sprintf(line, " %4s %2s %4s %c %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d",
					sst.sitecode.c_str(),
					sst.ptcode.c_str(),
					sst.solnnum.c_str(),
					sst.typecode,
					sst.start[0] % 100,
					sst.start[1],
					sst.start[2],
					sst.end[0] % 100,
					sst.end[1],
					sst.end[2],
					sst.mean[0] % 100,
					sst.mean[1],
					sst.mean[2]);

		if (pstns == NULL)
			doit = true;
		else
		{
			for (auto& stn : *pstns)
			{
				if (sst.sitecode.compare(stn.sitecode) == 0)
				{
					doit = true;
					break;
				}
			}
		}

		if (doit)
			out << line << endl;
	}

	if (theSinex.epochs_have_bias)
		out << "-BIAS/EPOCHS" << endl;
	else
		out << "-SOLUTION/EPOCHS" << endl;

	return 0;
}

void parse_snx_statistics(string& s)
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

int write_snx_statistics(ofstream& out)
{
	out << "+SOLUTION/STATISTICS" << endl;

	write_as_comments(out, theSinex.statistics_comments);

	for (auto& statistic : theSinex.list_statistics)
	{
		char line[81];

		if (statistic.etype == 0) // int
			sprintf(line, " %-30s %22d", statistic.name.c_str(), statistic.value.ival);

		if (statistic.etype == 1) // double
			sprintf(line, " %-30s %22.15lf", statistic.name.c_str(), statistic.value.dval);

		out << line << endl;
	}

	out << "-SOLUTION/STATISTICS" << endl;

	return 0;
}

static bool compare_estimates(Sinex_solestimate_t& left, Sinex_solestimate_t& right)
{
	int comp = left.sitecode.compare(right.sitecode);

	if (!comp)
	{
		// compare first on type, then on epoch
		if (left.type.compare(right.type) == 0)
		{
			comp = time_compare(left.refepoch, right.refepoch);
		}
		else
		{
			int ltype = 0;
			int rtype = 0;
			string s = trim(left.type);

			try
			{
				ltype = E_Estimate::_from_string(s.c_str());
			}
			catch (...)			{			}

			s = trim(right.type);

			try
			{
				rtype = E_Estimate::_from_string(s.c_str());
			}
			catch (...)			{			}

			comp = ltype - rtype;
		}
	}

	if (!comp)
		comp = left.index - right.index;

	return (comp < 0);
}

void parse_snx_solutionEstimates(string& s)
{
	Sinex_solestimate_t sst;
	
	sst.primary		= theSinex.primary;

	sst.type		= s.substr(7,	6);
	sst.sitecode	= s.substr(14,	4);
	sst.ptcode		= s.substr(19,	2);
	sst.solnnum 	= s.substr(22,	4);

	sst.index		= atoi(s.substr(1, 5).c_str());
	
	int	readcount	= sscanf(s.c_str() + 27, "%2d:%3d:%5d",
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

		GTime time = yds2time(sst.refepoch);
		if (theSinex.primary)		theSinex.map_estimates_primary	[sst.sitecode][sst.type][time] = sst;
		else						theSinex.map_estimates			[sst.sitecode][sst.type][time] = sst;
		
		return;
	}
	else
	{
		theSinex.estimate_comments.push_back(s);
	}
}

int write_snx_estimates_from_filter(ofstream& out)
{
	out << "+SOLUTION/ESTIMATE" << endl;

	write_as_comments(out, theSinex.estimate_comments);

	for (auto& [key, index] : theSinex.kfState.kfIndexMap)
	{
		if (key.type != KF::REC_POS)
		{
			continue;
		}

		string type;
		if		(key.num == 0) type = "STAX";
		else if	(key.num == 1) type = "STAY";
		else if	(key.num == 2) type = "STAZ";

		string ptcode = theSinex.map_siteids[key.str].ptcode;

		char line[83];
		sprintf(line, " %5d %-6s %4s %2s %4d %02d:%03d:%05d %-4s %c %21.14le %11.5le",
				index,
				type.c_str(),
				key.str.c_str(),
				ptcode.c_str(),
				1,
				theSinex.solution_end_date[0] % 100,
				theSinex.solution_end_date[1],
				theSinex.solution_end_date[2],
				"m",
				'9',	// TODO: replace with sst.constraint when fixed
						theSinex.kfState.x(index),
				sqrt(	theSinex.kfState.P(index,index)));

		out << line << endl;
	}

	out << "-SOLUTION/ESTIMATE" << endl;

	return 0;
}

// int write_snx_estimates(
// 	ofstream& out,
// 	std::list<Sinex_stn_snx_t>* pstns = NULL)
// {
// 	out << "+SOLUTION/ESTIMATE" << endl;
// 
// 	write_as_comments(out, theSinex.estimate_comments);
// 
// 	for (auto& [index, sst] : theSinex.estimates_map)
// 	{
// 		bool doit = (pstns == NULL);
// 
// 		if (pstns != NULL)
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
// 		sprintf(line, " %5d %6s %4s %2s %4s %2.2d:%3.3d:%5.5d %-4s %c %21.14le %11.5le",
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
// 
// 	return 0;
// }

// return true if left is less than right
static bool compare_apriori(Sinex_solapriori_t& left, Sinex_solapriori_t& right)
{
	int comp = left.sitecode.compare(right.sitecode);

	if (!comp)
	{
		// compare first on type, then on epoch
		if (left.param_type.compare(right.param_type) == 0)
		{
			comp = time_compare(left.epoch, right.epoch);
		}
		else
		{
			int ltype = 0;
			int rtype = 0;
			string s = trim(left.param_type);

			try
			{
				ltype = E_Estimate::_from_string(s.c_str());
			}
			catch (...)			{			}

			s = trim(right.param_type);

			try
			{
				rtype = E_Estimate::_from_string(s.c_str());
			}
			catch (...)			{			}

			comp = ltype - rtype;
		}
	}

	if (!comp)
		comp = left.idx - right.idx;

	return (comp < 0);
}

void parse_snx_apriori(string& s)
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

	int    readcount = sscanf(s.c_str() + 27, "%2d:%3d:%5d %4s %c %21lf %11lf",
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
	else
	{
		theSinex.apriori_comments.push_back(s);
	}
}

int write_snx_apriori(ofstream& out, std::list<Sinex_stn_snx_t>* pstns = NULL)
{
	out << "+SOLUTION/APRIORI" << endl;

	write_as_comments(out, theSinex.apriori_comments);

	for (auto& [index, apriori] : theSinex.apriori_map)
	{
		Sinex_solapriori_t& sst = apriori;
		bool doit = (pstns == NULL);

		if (pstns != NULL)
		{
			for (auto& stn : *pstns)
			{
				if (sst.sitecode.compare(stn.sitecode) == 0)
				{
					doit = true;
					break;
				}
			}
		}

		if (!doit)
			continue;

		char line[82];

		sprintf(line, " %5d %6s %4s %2s %4s %2.2d:%3.3d:%5.5d %-4s %c %21.14le %11.5le",
				sst.idx,
				sst.param_type.c_str(),
				sst.sitecode.c_str(),
				sst.ptcode.c_str(),
				sst.solnnum.c_str(),
				sst.epoch[0] % 100,
				sst.epoch[1],
				sst.epoch[2],
				sst.unit.c_str(),
				sst.constraint,
				sst.param,
				sst.stddev);

		out << line << endl;
	}

	out << "-SOLUTION/APRIORI" << endl;

	return 0;
}

int write_snx_apriori_from_stations(
	ofstream& out, 
	map<string, Station>&		stationMap)
{
	out << "+SOLUTION/APRIORI" << endl;

// 	write_as_comments(out, theSinex.apriori_comments);

	int index = 1;
	for (auto& [id, rec] : stationMap)
	{
		auto& sst = rec.snx;
		int		yds[3]		= {};
		double	epoch[6]	= {};
		time2epoch(rec.aprioriTime, epoch);
		epoch2yds(epoch, yds);
		
		for (int i = 0; i < 3; i++)
		{
			string type = "STA?";
			type[3] = 'X' + i;
			
			tracepdeex(0, out, " %5d %-6s %4s %2d %4s %02d:%03d:%05d %-4s %c %21.14le %11.5le\n",
					index,
					type.c_str(),
					id.c_str(),
					sst.ptcode.c_str(),
					1, //sst.solnnum.c_str(),
					((int)epoch[0]) % 100,
					epoch[1],
					epoch[2],
					"m", //sst.unit.c_str(),
					'3',//sst.constraint,
					rec.aprioriPos(i),// sst.param,
					rec.aprioriVar(i));
			
			index ++;
		}
	}

	out << "-SOLUTION/APRIORI" << endl;

	return 0;
}

static bool compare_normals(Sinex_solneq_t& left, Sinex_solneq_t& right)
{
	int comp = left.site.compare(right.site);

	if (!comp)
	{
		// compare first on type, then on epoch
		if (left.ptype.compare(right.ptype) == 0)
		{
			comp = time_compare(left.epoch, right.epoch);
		}
		else
		{
			int ltype = 0;
			int rtype = 0;
			string s = trim(left.ptype);

			try
			{
				ltype = E_Estimate::_from_string(s.c_str());
			}
			catch (...)			{			}

			s = trim(right.ptype);

			try
			{
				rtype = E_Estimate::_from_string(s.c_str());
			}
			catch (...)			{			}


			comp = ltype - rtype;
		}
	}

	if (!comp)
		comp = left.param - right.param;

	return (comp < 0);
}

void parse_snx_normals(string& s)
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

	int    readcount = sscanf(s.c_str() + 27, "%2d:%3d:%5d %4s %c %21lf",
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
	else
	{
		theSinex.normal_eqns_comments.push_back(s);
	}
}

int write_snx_normal(ofstream& out, std::list<Sinex_stn_snx_t>* pstns = NULL)
{
	out << "+SOLUTION/NORMAL_EQUATION_VECTOR" << endl;

	write_as_comments(out, theSinex.normal_eqns_comments);

	for (auto& sst : theSinex.list_normal_eqns)
	{
		bool doit = (pstns == NULL);

		if (pstns != NULL)
		{
			for (auto& stn : *pstns)
			{
				if (sst.site.compare(stn.sitecode) != 0)
				{
					doit = true;
					break;
				}
			}
		}

		if (!doit)
			continue;

		char line[81];

		sprintf(line, " %5d %6s %4s %2s %4s %2.2d:%3.3d:%5.5d %-4s %c %21.15lf",
				sst.param,
				sst.ptype.c_str(),
				sst.site.c_str(),
				sst.pt.c_str(),
				sst.solnnum.c_str(),
				sst.epoch[0] % 100,
				sst.epoch[1],
				sst.epoch[2],
				sst.unit.c_str(),
				sst.constraint,
				sst.normal);

		out << line << endl;
	}

	out << "-SOLUTION/NORMAL_EQUATION_VECTOR" << endl;

	return 0;
}

// Just use indices of row and col for the comparison
int compare_matrix_entries(Sinex_solmatrix_t& left, Sinex_solmatrix_t& right)
{
	int comp;

	if (left.row == right.row)
		comp = left.col - right.col;
	else
		comp = left.row - right.row;

	return (comp < 0);
}

void parse_snx_matrix(string& s)//, matrix_type type, matrix_value value)
{
// 	//todo aaron, this is only half complete, the maxrow/col arent used but should be with multiple input matrices.
// 	int		maxrow = 0;
// 	int		maxcol = 0;
// 	Sinex_solmatrix_t smt;
// 
// 	int readcount = sscanf(s.c_str(), " %5d %5d %21lf %21lf %21lf",
// 						&smt.row,
// 						&smt.col,
// 						&smt.value[0],
// 						&smt.value[1],
// 						&smt.value[2]);
// 
// 	if (readcount > 2)
// 	{
// 		if (smt.row < smt.col)
// 		{
// 			//xor swap
// 			smt.row ^= smt.col;
// 			smt.col ^= smt.row;
// 			smt.row ^= smt.col;
// 		}
// 		
// 		for (int i = readcount - 2; i < 3; i++)
// 			smt.value[i] = -1;
// 
// 		smt.numvals = readcount - 2;
// 
// 		if (smt.row > maxrow) maxrow = smt.row;
// 		if (smt.col > maxcol) maxcol = smt.col;
// 
// 		theSinex.matrix_map[type][value].push_back(smt);
// 	}
// 	else
// 	{
// 		theSinex.matrix_comments.push_back(s);
// 	}
}

void write_snx_matrices_from_filter(
	ofstream& out)
{
	const char* type_strings	[MAX_MATRIX_TYPE];
	const char* value_strings	[MAX_MATRIX_VALUE];

	type_strings[ESTIMATE]		= "SOLUTION/MATRIX_ESTIMATE";
	type_strings[APRIORI]		= "SOLUTION/MATRIX_APRIORI";
	type_strings[NORMAL_EQN]	= "SOLUTION/NORMAL_EQUATION_MATRIX";

	value_strings[CORRELATION]	= "CORR";
	value_strings[COVARIANCE]	= "COVA";
	value_strings[INFORMATION]	= "INFO";

	for (auto& mt : {ESTIMATE})
	for (auto& mv : {COVARIANCE})
	{
		//print header
		tracepdeex(0, out, "+%s %c %s\n",
			type_strings[mt],
			'L',
			mt == NORMAL_EQN ? "" : value_strings[mv]);

		write_as_comments(out, theSinex.matrix_comments);

		MatrixXd& P = theSinex.kfState.P;


		for (int i = 1; i < P.rows();	i++)
		for (int j = 1; j <= i;		)
		{
			if (P(i,j) == 0)
			{
				j++;
				continue;
			}

			//start printing a line
			tracepdeex(0, out, " %5d %5d %21.14le",
					i,
					j,
					P(i,j));
			j++;

			for (int k = 0; k < 2; k++)
			{
				if	( (P(i,j) == 0)
					||(j > i))
				{
					break;
				}

				tracepdeex(0, out, " %21.14le", P(i,j));
				j++;
			}

			tracepdeex(0, out, "\n");
		}

		//print footer
		tracepdeex(0, out, "-%s %c %s\n",
			type_strings[mt],
			'L',
			mt == NORMAL_EQN ? "" : value_strings[mv]);
	}
}

void parse_snx_precode(string& s)
{
	Sinex_precode_t snt;

	snt.precesscode	= s.substr(1, 8);
	snt.comment		= s.substr(10);

	theSinex.list_precessions.push_back(snt);
}

int write_snx_precodes(ofstream& out)
{
	out << "+PRECESSION/DATA" << endl;

	write_as_comments(out, theSinex.precession_comments);

	for (auto& spt : theSinex.list_precessions)
	{
		char line[81];

		sprintf(line, " %8s %s", spt.precesscode.c_str(), spt.comment.c_str());

		out << line << endl;
	}

	out << "-PRECESSION/DATA" << endl;

	return 0;
}

void parse_snx_nutcode(string& s)
{
	Sinex_nutcode_t snt;

	snt.nutcode = s.substr(1, 8);
	snt.comment = s.substr(10);

	theSinex.list_nutcodes.push_back(snt);
}

int write_snx_nutcodes(ofstream& out)
{
	out << "+NUTATION/DATA" << endl;

	write_as_comments(out, theSinex.nutation_comments);

	for (auto& nutcode : theSinex.list_nutcodes)
	{
		Sinex_nutcode_t& snt = nutcode;

		char line[81];

		sprintf(line, " %8s %s", snt.nutcode.c_str(), snt.comment.c_str());

		out << line << endl;
	}

	out << "-NUTATION/DATA" << endl;

	return 0;
}

void parse_snx_sourceids(string& s)
{
	Sinex_source_id_t ssi;

	ssi.source		= s.substr(1, 4);
	ssi.iers		= s.substr(6, 8);
	ssi.icrf		= s.substr(15, 16);
	ssi.comments	= s.substr(32);

	theSinex.list_source_ids.push_back(ssi);
}

int write_snx_sourceids(ofstream& out)
{
	out << "+SOURCE/ID" << endl;

	write_as_comments(out, theSinex.sourceid_comments);

	for (auto& source_id : theSinex.list_source_ids)
	{
		Sinex_source_id_t& ssi = source_id;

		char line[101];

		sprintf(line, " %4s %8s %16s %s", ssi.source.c_str(), ssi.iers.c_str(), ssi.icrf.c_str(), ssi.comments.c_str());

		out << line << endl;
	}

	out << "-SOURCE/ID" << endl;

	return 0;
}

static bool compare_satids(Sinex_satid_t& left, Sinex_satid_t& right)
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

void parse_snx_satelliteIds(string& s)
{
	Sinex_satid_t sst;

	sst.svn			= s.substr(1, 4);
	sst.prn			= sst.svn[0] + s.substr(6, 2);
	sst.cospar		= s.substr(9, 9);;
	sst.obsCode		= s[18];
	sst.antRcvType	= s.substr(47);

	const char* p = s.c_str() + 21;

	int 	readcount = sscanf(p, "%2d:%3d:%5d %2d:%3d:%5d",
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
	else
	{
		theSinex.satid_comments.push_back(s);
	}
}

int write_snx_satids(ofstream& out)
{
	out << "+SATELLITE/ID" << endl;

	write_as_comments(out, theSinex.satid_comments);

	for (auto& ssi : theSinex.list_satids)
	{
		char line[101];

		sprintf(line, " %4s %2s %9s %c %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %20s",
				ssi.svn.c_str(),
				ssi.prn.c_str() + 1,
				ssi.cospar.c_str(),
				ssi.obsCode,
				ssi.timeSinceLaunch[0],
				ssi.timeSinceLaunch[1],
				ssi.timeSinceLaunch[2],
				ssi.timeUntilDecom[0],
				ssi.timeUntilDecom[1],
				ssi.timeUntilDecom[2],
				ssi.antRcvType.c_str());

		out << line << endl;
	}

	out << "-SATELLITE/ID" << endl;

	return 0;
}

static bool compare_satidents(Sinex_satident_t& left, Sinex_satident_t& right)
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

	return (comp <= 0);
}

void parse_snx_satelliteIdentifiers(string& s)
{
	Sinex_satident_t sst;

	sst.svn			= s.substr(1, 4);
	sst.cospar		= s.substr(6, 9);
	sst.category	= atoi(s.substr(16, 6).c_str());
	sst.blocktype	= s.substr(23, 15);
	sst.comment		= s.substr(39);

	theSinex.list_satidents.push_back(sst);
}

int write_snx_satidents(ofstream& out)
{
	out << "+SATELLITE/IDENTIFIER" << endl;

	write_as_comments(out, theSinex.satident_comments);

	for (auto& ssi : theSinex.list_satidents)
	{
		char line[101];

		sprintf(line, " %4s %9s %6d %-15s %s",
				ssi.svn.c_str(),
				ssi.cospar.c_str(),
				ssi.category,
				ssi.blocktype.c_str(),
				ssi.comment.c_str());

		out << line << endl;
	}

	out << "-SATELLITE/IDENTIFIER" << endl;

	return 0;
}

// NB this DOES not compare by PRN!!
static bool compare_satprns(Sinex_satprn_t& left, Sinex_satprn_t& right)
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

void parse_snx_satprns(string& s)
{
	Sinex_satprn_t spt;

	spt.svn			= s.substr(1, 4);
	spt.prn			= s.substr(36, 3);
	spt.comment		= s.substr(40);

	int readcount = sscanf(s.c_str() + 6, "%4d:%3d:%5d %4d:%3d:%5d",
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
	}
	else
	{
		theSinex.satprn_comments.push_back(s);
	}
}

int write_snx_satprns(ofstream& out)
{
	out << "+SATELLITE/PRN" << endl;

	write_as_comments(out, theSinex.satprn_comments);

	char line[101];

	for (auto& spt : theSinex.list_satprns)
	{
		sprintf(line, " %4s %4.4d:%3.3d:%5.5d %4.4d:%3.3d:%5.5d %3s %s",
				spt.svn.c_str(),
				spt.start[0],
				spt.start[1],
				spt.start[2],
				spt.stop[0],
				spt.stop[1],
				spt.stop[2],
				spt.prn.c_str(),
				spt.comment.c_str());

		out << line << endl;
	}

	out << "-SATELLITE/PRN" << endl;

	return 0;
}

static bool compare_freq_channels(Sinex_satfreqchn_t& left, Sinex_satfreqchn_t& right)
{
	// start by comparing SVN...
	char	constleft	= left.svn[0];
	char    constright	= right.svn[0];
	int     nleft		= atoi(left.svn.substr(1).c_str());
	int     nright		= atoi(right.svn.substr(1).c_str());
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

void parse_snx_satfreqchannels(string& s)
{
	Sinex_satfreqchn_t	sfc;

	sfc.svn		= s.substr(1, 4);
	sfc.comment	= s.substr(40);

	int readcount = sscanf(s.c_str() + 6, "%4d:%3d:%5d %4d:%3d:%5d %3d",
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
	else
	{
		theSinex.satfreqchn_comments.push_back(s);
	}
}

int write_snx_satfreqchn(ofstream& out)
{
	out << "+SATELLITE/FREQUENCY_CHANNEL" << endl;

	write_as_comments(out, theSinex.satfreqchn_comments);

	for (auto& sfc : theSinex.list_satfreqchns)
	{
		char line[101];

		sprintf(line, " %4s %4.4d:%3.3d:%5.5d %4.4d:%3.3d:%5.5d %3d %s",
				sfc.svn.c_str(),
				sfc.start[0],
				sfc.start[1],
				sfc.start[2],
				sfc.stop[0],
				sfc.stop[1],
				sfc.stop[2],
				sfc.channel,
				sfc.comment.c_str());

		out << line << endl;
	}

	out << "-SATELLITE/FREQUENCY_CHANNEL" << endl;

	return 0;
}

static bool compare_satmass(Sinex_satmass_t& left, Sinex_satmass_t& right)
{
	// start by comparing SVN...
	char	constleft	= left.svn[0];
	char    constright	= right.svn[0];
	int     nleft		= atoi(left.svn.substr(1).c_str());
	int     nright		= atoi(right.svn.substr(1).c_str());
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

void parse_snx_satelliteMass(string& s)
{
	Sinex_satmass_t	ssm;

	ssm.svn		= s.substr(1, 4);
	ssm.comment	= s.substr(46);

	int readcount = sscanf(s.c_str() + 6, "%4d:%3d:%5d %4d:%3d:%5d %9lf",
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
		theSinex.list_satmasses.push_back(ssm);
	}
	else
	{
		theSinex.satmass_comments.push_back(s);
	}
}

int write_snx_satmass(ofstream& out)
{
	out << "+SATELLITE/MASS" << endl;

	write_as_comments(out, theSinex.satmass_comments);

	for (auto& ssm : theSinex.list_satmasses)
	{
		char line[101];

		sprintf(line, " %4s %4.4d:%3.3d:%5.5d %4.4d:%3.3d:%5.5d %9.3lf %s",
				ssm.svn.c_str(),
				ssm.start[0],
				ssm.start[1],
				ssm.start[2],
				ssm.stop[0],
				ssm.stop[1],
				ssm.stop[2],
				ssm.mass,
				ssm.comment.c_str());

		out << line << endl;
	}

	out << "-SATELLITE/MASS" << endl;

	return 0;
}

static bool compare_satcom(Sinex_satcom_t& left, Sinex_satcom_t& right)
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

void parse_snx_satelliteComs(string& s)
{
	Sinex_satcom_t	sct;

	sct.svn		= s.substr(1, 4);
	sct.comment	= s.substr(66);

	int readcount = sscanf(s.c_str() + 6, "%4d:%3d:%5d %4d:%3d:%5d %9lf %9lf %9lf",
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
	else
	{
		theSinex.satcom_comments.push_back(s);
	}
}

int write_snx_satcom(ofstream& out)
{
	out << "+SATELLITE/COM" << endl;

	write_as_comments(out, theSinex.satcom_comments);

	for (auto& sct : theSinex.list_satcoms)
	{
		char line[101];

		sprintf(line, " %4s %4.4d:%3.3d:%5.5d %4.4d:%3.3d:%5.5d %9.4lf %9.4lf %9.4lf %s",
				sct.svn.c_str(),
				sct.start[0],
				sct.start[1],
				sct.start[2],
				sct.stop[0],
				sct.stop[1],
				sct.stop[2],
				sct.com[0],
				sct.com[1],
				sct.com[2],
				sct.comment.c_str());

		out << line << endl;
	}

	out << "-SATELLITE/COM" << endl;

	return 0;
}

static bool compare_satecc(Sinex_satecc_t& left, Sinex_satecc_t& right)
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

void parse_snx_satelliteEccentricities(string& s)
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
	else
	{
		theSinex.satecc_comments.push_back(s);
	}
}

int write_snx_satecc(ofstream& out)
{
	out << "+SATELLITE/ECCENTRICITY" << endl;

	write_as_comments(out, theSinex.satecc_comments);

	for (auto& set : theSinex.list_sateccs)
	{
		char line[101];

		sprintf(line, " %4s %-20s %c %9.4lf %9.4lf %9.4lf %s",
				set.svn.c_str(),
				set.equip.c_str(),
				set.type,
				set.ecc[0],
				set.ecc[1],
				set.ecc[2],
				set.comment.c_str());

		out << line << endl;
	}

	out << "-SATELLITE/ECCENTRICITY" << endl;

	return 0;
}

static bool compare_satpower(Sinex_satpower_t& left, Sinex_satpower_t& right)
{
	// start by comparing SVN...
	char	constleft	= left.svn[0];
	char    constright	= right.svn[0];
	int     nleft		= atoi(left.svn.substr(1).c_str());
	int     nright		= atoi(right.svn.substr(1).c_str());
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

void parse_snx_satellitePowers(string& s)
{
	Sinex_satpower_t	spt;

	spt.svn		= s.substr(1, 4);
	spt.comment	= s.substr(41);

	int readcount = sscanf(s.c_str() + 6, "%4d:%3d:%5d %4d:%3d:%5d %4d",
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
		theSinex.list_satpowers.push_back(spt);
	}
	else
	{
		theSinex.satpower_comments.push_back(s);
	}
}

int write_snx_satpower(ofstream& out)
{
	out << "+SATELLITE/TX_POWER" << endl;

	write_as_comments(out, theSinex.satpower_comments);

	for (auto& spt : theSinex.list_satpowers)
	{
		char line[101];

		sprintf(line, " %4s %4.4d:%3.3d:%5.5d %4.4d:%3.3d:%5.5d %4d %s",
				spt.svn.c_str(),
				spt.start[0],
				spt.start[1],
				spt.start[2],
				spt.stop[0],
				spt.stop[1],
				spt.stop[2],
				spt.power,
				spt.comment.c_str());

		out << line << endl;
	}

	out << "-SATELLITE/TX_POWER" << endl;

	return 0;
}

static bool compare_satpc(Sinex_satpc_t& left, Sinex_satpc_t& right)
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

void parse_snx_satellitePhaseCenters(string& s)
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
	else
	{
		theSinex.satpc_comments.push_back(s);
	}
}

int write_snx_satpc(ofstream& out)
{
	out << "+SATELLITE/PHASE_CENTER" << endl;

	write_as_comments(out, theSinex.satpc_comments);

	for (auto& spt : theSinex.list_satpcs)
	{
		char line[101];
		char freq2line[23];

		memset(freq2line, ' ', sizeof(freq2line));
		freq2line[22] = '\0';

		if (spt.freq2 != ' ')
			sprintf(freq2line, "%c %6.4lf %6.4lf %6.4lf",
					spt.freq2,
					spt.zxy2[0],
					spt.zxy2[1],
					spt.zxy2[2]);

		sprintf(line, " %4s %c %6.4lf %6.4lf %6.4lf %22s %-10s %c %c",
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

	out << "-SATELLITE/PHASE_CENTER" << endl;

	return 0;
}

void nullFunction(string& s)
{
	
}

int read_sinex(
	string filepath, 
	bool primary)
{
	theSinex.primary = primary;
	
// 	BOOST_LOG_TRIVIAL(info)
// 	<< "reading " << filepath << std::endl;

	ifstream filestream(filepath);
	if (!filestream)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error opening sinex file" << filepath << endl;
		return 1;
	}

	int failure = read_snx_header(filestream);
	if (failure)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error reading header line." << endl;

		return failure;
	}

	void (*parseFunction)(string&) = nullFunction;
	
	list<string>*	com_ptr = nullptr;
	string			closure = "";
	list<string>	comments;
	
	while (filestream)
	{
		string line;
		
		getline(filestream, line);

		// test below empty line (ie continue if something on the line)
		if	(!filestream)
		{
			// error - did not find closure line. Report and clean up.
			BOOST_LOG_TRIVIAL(error)
			<< "Closure line not found before end." << endl;

			failure = 1;
			break;
		}
		else if (line[0] == '*')
		{
			comments.push_back(line);
		}
		else if (line[0] == '-')
		{
			//end of block
			parseFunction = nullFunction;
			
			if (line != closure)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Incorrect section closure line encountered: "
				<< closure << " != " << line << endl;
			}
			
			if (com_ptr)
			{
				//merge all comments that came during the block into the block's comment list.
				com_ptr->merge(comments);
				comments.clear();
			}
		}
		else if (line[0] == ' ')
		{
			//this probably needs specialty parsing - use a prepared function pointer.
			parseFunction(line);
		}
		else if (line[0] == '+')
		{
			char	c;
			matrix_value mv;
			string	mvs;
			
			//prepare closing line for comparison
			closure = line;
			closure[0] = '-';
			string lineName = line.substr(0, line.find(' '));
			if		(lineName == "+FILE/REFERENCE"   				)	{ parseFunction = parse_snx_reference;					com_ptr = nullptr;							}
			else if	(lineName == "+FILE/COMMENT"					)	{ parseFunction = parse_snx_comment;					com_ptr = nullptr;							}
			else if	(lineName == "+INPUT/HISTORY"					)	{ parseFunction = parse_snx_inputHistory;				com_ptr = &theSinex.historyComments;		}
			else if	(lineName == "+INPUT/FILES"						)	{ parseFunction = parse_snx_inputFiles;					com_ptr = &theSinex.filesComments;			}
			else if	(lineName == "+INPUT/ACKNOWLEDGEMENTS"			)	{ parseFunction = parse_snx_acknowledgements;			com_ptr = &theSinex.ackComments;			}
			else if	(lineName == "+INPUT/ACKNOWLEDGMENTS"			)	{ parseFunction = parse_snx_acknowledgements;			com_ptr = &theSinex.ackComments;			}
			else if	(lineName == "+NUTATION/DATA"					)	{ parseFunction = parse_snx_nutcode;					com_ptr = &theSinex.nutation_comments;		}
			else if	(lineName == "+PRECESSION/DATA"					)	{ parseFunction = parse_snx_precode;					com_ptr = &theSinex.precession_comments;	}
			else if	(lineName == "+SOURCE/ID"						)	{ parseFunction = parse_snx_sourceids;					com_ptr = &theSinex.sourceid_comments;		}
			else if	(lineName == "+SITE/ID"							)	{ parseFunction = parse_snx_siteIds;					com_ptr = &theSinex.siteIdcomments;			}
			else if	(lineName == "+SITE/DATA"						)	{ parseFunction = parse_snx_siteData;					com_ptr = &theSinex.siteDatacomments;		}
			else if	(lineName == "+SITE/RECEIVER"					)	{ parseFunction = parse_snx_receivers;					com_ptr = &theSinex.receivercomments;		}
			else if	(lineName == "+SITE/ANTENNA"					)	{ parseFunction = parse_snx_antennas;					com_ptr = &theSinex.antennacomments;		}
			else if	(lineName == "+SITE/GPS_PHASE_CENTER"			)	{ parseFunction = parse_gps_phaseCenters;				com_ptr = &theSinex.gps_pc_comments;		}
			else if	(lineName == "+SITE/GAL_PHASE_CENTER"			)	{ parseFunction = parse_gal_phaseCenters;				com_ptr = &theSinex.gal_pc_comments;		}
			else if	(lineName == "+SITE/ECCENTRICITY"				)	{ parseFunction = parse_snx_siteEccentricity;			com_ptr = &theSinex.site_ecc_comments;		}
			else if	(lineName == "+BIAS/EPOCHS"						)	{ parseFunction = parse_snx_epochs;						com_ptr = &theSinex.epochcomments;			}
			else if	(lineName == "+SOLUTION/EPOCHS"					)	{ parseFunction = parse_snx_epochs;						com_ptr = &theSinex.epochcomments;			}
			else if	(lineName == "+SOLUTION/STATISTICS"				)	{ parseFunction = parse_snx_statistics;					com_ptr = &theSinex.statistics_comments;	}
			else if	(lineName == "+SOLUTION/ESTIMATE"				)	{ parseFunction = parse_snx_solutionEstimates;			com_ptr = &theSinex.estimate_comments;		}
			else if	(lineName == "+SOLUTION/APRIORI"				)	{ parseFunction = parse_snx_apriori;					com_ptr = &theSinex.apriori_comments;		}
			else if	(lineName == "+SOLUTION/NORMAL_EQUATION_VECTOR"	)	{ parseFunction = parse_snx_normals;					com_ptr = &theSinex.normal_eqns_comments;	}
			else if	(lineName == "+SOLUTION/MATRIX_ESTIMATE"		)	{ parseFunction = parse_snx_matrix;						com_ptr = nullptr;							}
			else if	(lineName == "+SOLUTION/MATRIX_APRIORI"			)	{ parseFunction = parse_snx_matrix;						com_ptr = nullptr;							}
			else if	(lineName == "+SOLUTION/NORMAL_EQUATION_MATRIX"	)	{ parseFunction = parse_snx_matrix;						com_ptr = nullptr;							}
			else if	(lineName == "+SATELLITE/IDENTIFIER"			)	{ parseFunction = parse_snx_satelliteIdentifiers;		com_ptr = &theSinex.satident_comments;		}
			else if	(lineName == "+SATELLITE/PRN"					)	{ parseFunction = parse_snx_satprns;					com_ptr = &theSinex.satprn_comments;		}
			else if	(lineName == "+SATELLITE/MASS"					)	{ parseFunction = parse_snx_satelliteMass;				com_ptr = &theSinex.satmass_comments;		}
			else if	(lineName == "+SATELLITE/FREQUENCY_CHANNEL"		)	{ parseFunction = parse_snx_satfreqchannels;			com_ptr = &theSinex.satfreqchn_comments;	}
			else if	(lineName == "+SATELLITE/TX_POWER"				)	{ parseFunction = parse_snx_satellitePowers;			com_ptr = &theSinex.satpower_comments;		}
			else if	(lineName == "+SATELLITE/COM"					)	{ parseFunction = parse_snx_satelliteComs;				com_ptr = &theSinex.satcom_comments;		}
			else if	(lineName == "+SATELLITE/ECCENTRICITY"			)	{ parseFunction = parse_snx_satelliteEccentricities;	com_ptr = &theSinex.satecc_comments;		}
			else if	(lineName == "+SATELLITE/PHASE_CENTER"			)	{ parseFunction = parse_snx_satellitePhaseCenters;		com_ptr = &theSinex.satpc_comments;			}
			else if	(lineName == "+SATELLITE/ID"					)	{ parseFunction = parse_snx_satelliteIds;				com_ptr = &theSinex.satid_comments;			}
			else
			{
				BOOST_LOG_TRIVIAL(error)
				<< "error unknown header line: " << line << endl;

				failure = 1;	
			}
			
			if (com_ptr)
			{
				//merge all comments that came before the block into the block's comment list.
				com_ptr->merge(comments);
				comments.clear();
			}
				
// 			int 	i;
// 									failure = read_snx_matrix			(filestream, NORMAL_EQN, INFORMATION, c);			break;
// 				case 15:
// 					if (!theSinex.epochs_have_bias && !theSinex.list_solepochs.empty())
// 					{
// 						BOOST_LOG_TRIVIAL(error)
// 						<< "cannot combine BIAS/EPOCHS and SOLUTION/EPOCHS blocks." << endl;
// 
// 						failure = 1;
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
// 						failure = 1;
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
			if (line != "%ENDSNX")
			{
				// error in file. report it.
				BOOST_LOG_TRIVIAL(error)
				<< "line starting '%' met not final line" << endl << line << endl;

				failure = 1;
			}

			break;
		}

		if (failure)
			break;
	}

	theSinex.list_satpcs.		sort(compare_satpc);
	theSinex.list_satpowers.	sort(compare_satpower);
	theSinex.list_sateccs.		sort(compare_satecc);
	theSinex.list_solepochs.	sort(compare_site_epochs);
	theSinex.list_normal_eqns.	sort(compare_normals);
	theSinex.list_sitedata.		sort(compare_sitedata);
	theSinex.list_gps_pcs.		sort(compare_gps_pc);
	theSinex.list_satids.		sort(compare_satids);
	theSinex.list_satidents.	sort(compare_satidents);
	theSinex.list_satfreqchns.	sort(compare_freq_channels);
	theSinex.list_satmasses.	sort(compare_satmass);
	theSinex.list_satprns.		sort(compare_satprns);
	theSinex.list_satcoms.		sort(compare_satcom);
	theSinex.list_gal_pcs.		sort(compare_gal_pc);
	
// 	theSinex.matrix_map[type][value].sort(compare_matrix_entries);
	dedupe_sinex();

	return failure;
}





int  write_sinex(
	string filepath,
	map<string, Station>*		stationMap_ptr,
	Sinex_sat_snx_t*			psat,
	bool 						comm_override)
{
	ofstream 	filestream(filepath);

	if (!filestream)
	{
		return 1;
	}
	int 		failure = 0;
	bool		domatrices = false;
	// theSinex.estimate_comments.	 ;

	// bool 		comm_override = true;

	if (comm_override)
		comments_override();
	write_snx_header(filestream); if (comm_override) write_pretty_line(filestream);

	if (!theSinex.refstrings.		empty())		{failure += write_snx_reference			(filestream);	if (comm_override) write_pretty_line(filestream);}
	if (!theSinex.commentstrings.	empty())		{failure += write_snx_comments			(filestream);	if (comm_override) write_pretty_line(filestream);}
	if (!theSinex.inputHistory.		empty())		{failure += write_snx_input_history		(filestream);	if (comm_override) write_pretty_line(filestream);}
	if (!theSinex.inputFiles.		empty())		{failure += write_snx_input_files		(filestream);	if (comm_override) write_pretty_line(filestream);}
	if (!theSinex.acknowledgements.	empty())		{failure += write_snx_acknowledgements	(filestream);	if (comm_override) write_pretty_line(filestream);}

	if (psat == NULL)
	{
		if (!theSinex.map_siteids.			empty())	{failure += write_snx_siteids	(filestream);	if (comm_override) write_pretty_line(filestream);}
// 		if (!theSinex.list_sitedata.		empty())	{failure += write_snx_sitedata	(filestream);	if (comm_override) write_pretty_line(filestream);}
		if (!theSinex.map_receivers.		empty())	{failure += write_snx_receivers	(filestream);	if (comm_override) write_pretty_line(filestream);}
		if (!theSinex.map_antennas.			empty())	{failure += write_snx_antennas	(filestream);	if (comm_override) write_pretty_line(filestream);}
// 		if (!theSinex.list_gps_pcs.			empty())	{failure += write_snx_gps_pcs	(filestream);	if (comm_override) write_pretty_line(filestream);}
// 		if (!theSinex.list_gal_pcs.			empty())	{failure += write_snx_gal_pcs	(filestream);	if (comm_override) write_pretty_line(filestream);}
		if (!theSinex.map_eccentricities.	empty())	{failure += write_snx_site_eccs	(filestream);	if (comm_override) write_pretty_line(filestream);}
// 		if (!theSinex.list_solepochs.		empty())	{failure += write_snx_epochs	(filestream);	if (comm_override) write_pretty_line(filestream);}
// 		if (!theSinex.list_statistics.		empty())	{failure += write_snx_statistics(filestream);					if (comm_override) write_pretty_line(filestream);}
// 		if (!theSinex.estimates_map.		empty())		failure += write_snx_estimates	(filestream);
																	write_snx_estimates_from_filter	(filestream);		if (comm_override) write_pretty_line(filestream);
// 		if (!theSinex.apriori_map.			empty())	{failure += write_snx_apriori	(filestream);if (comm_override) write_pretty_line(filestream);}
																	write_snx_apriori_from_stations (filestream, *stationMap_ptr);
// 		if (!theSinex.list_normal_eqns.		empty())	{failure += write_snx_normal	(filestream);	if (comm_override) write_pretty_line(filestream);}


		for (int i = 0; i < 3; i++)
			domatrices |= !theSinex.matrix_map[i].empty();

		if (domatrices)
// 			write_snx_matrices(filestream, stationListPointer);
			write_snx_matrices_from_filter(filestream); if (comm_override) write_pretty_line(filestream);
	}

	if	(psat == nullptr)
	{
// 		if (!theSinex.list_source_ids.	empty())	{failure += write_snx_sourceids	(filestream);if (comm_override) write_pretty_line(filestream);}
// 		if (!theSinex.list_nutcodes.	empty())	{failure += write_snx_nutcodes	(filestream);if (comm_override) write_pretty_line(filestream);}
// 		if (!theSinex.list_precessions.	empty())	{failure += write_snx_precodes	(filestream);if (comm_override) write_pretty_line(filestream);}
	}
	
	filestream << "%ENDSNX" << endl;

	return failure;
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

int sinex_check_add_ga_reference()
{
	// step 1: check it is not already there
	for (auto it = theSinex.refstrings.begin(); it != theSinex.refstrings.end(); it++)
	{
		if (it->refline.find("Geoscience Australia") != string::npos)
		{
			return 1;
		}
	}

	// step 2: remove any other provider's details
	// NB we do not increment the iterator in the loop because the erase if found will do it for us
	for (auto it = theSinex.refstrings.begin(); it != theSinex.refstrings.end(); )
	{
		string	s = it->refline;

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
	Sinex_ref_t srt;
	char 	line[81];

	sprintf(line, " %-18s %s", "DESCRIPTION", "Geoscience Australia");
	srt.refline = line;
	theSinex.refstrings.push_back(srt);

	// FIXME: network solution could be different?
	sprintf(line, " %-18s %s", "OUTPUT", "PPP Solution");
	srt.refline = line;
	theSinex.refstrings.push_back(srt);

	sprintf(line, " %-18s %s", "CONTACT", "npi@ga.gov.au");
	srt.refline = line;
	theSinex.refstrings.push_back(srt);

	// TODO: replace 0.1 with some auto generated variable or config file entry, should be current version in bitbucket
	sprintf(line, " %-18s %s", "SOFTWARE", "Ginan PEA Version 0.1");
	srt.refline = line;
	theSinex.refstrings.push_back(srt);

	int result = uname(&buf);

	if (result == 0)
	{
		int len;
		sprintf(line, " %-18s ", "HARDWARE");
		len = strlen(line);

		if ((len + strlen(buf.sysname)) < 80)
		{
			strcat(line, buf.sysname);
			strcat(line, " ");
			len = strlen(line);
		}

		if ((len + strlen(buf.release)) < 80)
		{
			strcat(line, buf.release);
			strcat(line, " ");
			len = strlen(line);
		}

		if ((len + strlen(buf.version)) < 80)
		{
			strcat(line, buf.version);
			strcat(line, " ");
		}

		srt.refline = line;
		theSinex.refstrings.push_back(srt);
	}

	sprintf(line, " %-18s %s", "INPUT", "RINEX");
	srt.refline = line;
	theSinex.refstrings.push_back(srt);

	return 0;
}

void sinex_add_acknowledgement(const string& who, const string& description)
{
	Sinex_ack_t 	sat;

	sat.agency = who.substr(0, 3);
	sat.description = description;

	theSinex.acknowledgements.push_back(sat);
}

void sinex_add_comment(const string& what)
{
	Sinex_comment_t sct;

	sct.cmtline = what;

	theSinex.commentstrings.push_back(sct);
}

void sinex_add_file(const string& who, const GTime& when, const string& filename, const string& description)
{
	double 				ep[6];
	int					yds[3];
	Sinex_input_file_t	sif;

	time2epoch(when, ep);
	epoch2yds(ep, yds);


	for (int i = 0; i < 3; i++)
		sif.epoch[i] = yds[i];

	sif.agency			= who;
	sif.file			= filename;
	sif.description		= description;

	theSinex.inputFiles.push_back(sif);
}

void sinex_report()
{
	cout << "count of refstrings = " 					<< theSinex.refstrings			.size() << endl;
	cout << "count of comments = " 						<< theSinex.commentstrings		.size() << endl;
	cout << "count of history comments = " 				<< theSinex.historyComments		.size() << endl;
	cout << "count of input histories = " 				<< theSinex.inputHistory		.size() << endl;
	cout << "count of file comments = " 				<< theSinex.filesComments		.size() << endl;
	cout << "count of input files = " 					<< theSinex.inputFiles			.size() << endl;
	cout << "count of acknowledgement comments = " 		<< theSinex.ackComments			.size() << endl;
	cout << "count of acknowledgements = " 				<< theSinex.acknowledgements	.size() << endl;

	cout << "count of site id comments = " 				<< theSinex.siteIdcomments		.size() << endl;
	cout << "count of site ids = " 						<< theSinex.map_siteids			.size() << endl;
	cout << "count of site data comments = " 			<< theSinex.siteDatacomments	.size() << endl;
	cout << "count of site datas = " 					<< theSinex.list_sitedata		.size() << endl;
	cout << "count of receiver comments = " 			<< theSinex.receivercomments	.size() << endl;
	cout << "count of receivers = " 					<< theSinex.map_receivers		.size() << endl;
	cout << "count of antenna comments = " 				<< theSinex.antennacomments		.size() << endl;
	cout << "count of antenna sites = " 				<< theSinex.map_antennas		.size() << endl;
	cout << "count of site eccentricity comments = " 	<< theSinex.site_ecc_comments	.size() << endl;
	cout << "count of site eccentricities = " 			<< theSinex.map_eccentricities	.size() << endl;
	cout << "count of GPS phase centre comments = " 	<< theSinex.gps_pc_comments		.size() << endl;
	cout << "count of GPS phase centres = " 			<< theSinex.list_gps_pcs		.size() << endl;
	cout << "count of GAL phase cnetre comments = " 	<< theSinex.gal_pc_comments		.size() << endl;
	cout << "count of GAL phase centres = " 			<< theSinex.list_gal_pcs		.size() << endl;

	cout << "solutions have bias = " 					<< theSinex.epochs_have_bias << endl;
	cout << "count of epoch comments = " 				<< theSinex.epochcomments		.size() << endl;
	cout << "count of solution epochs = " 				<< theSinex.list_solepochs		.size() << endl;
	cout << "count of statistics comments = " 			<< theSinex.statistics_comments	.size() << endl;
	cout << "count of statistics = " 					<< theSinex.list_statistics		.size() << endl;
	cout << "count of estimate comments = " 			<< theSinex.estimate_comments	.size() << endl;
	cout << "count of estimates = " 					<< theSinex.map_estimates		.size() << endl;
	cout << "count of apriori comments = " 				<< theSinex.apriori_comments	.size() << endl;
	cout << "count of aprioris = " 						<< theSinex.apriori_map			.size() << endl;
	cout << "count of normal equation comments = " 		<< theSinex.normal_eqns_comments.size() << endl;
	cout << "count of normal equations = " 				<< theSinex.list_normal_eqns	.size() << endl;
	cout << "count of matrix comments = " 				<< theSinex.matrix_comments		.size() << endl;

	cout << "count of satid comments = " 				<< theSinex.satid_comments		.size() << endl;
	cout << "count of sat IDs = " 						<< theSinex.list_satids			.size() << endl;
	cout << "count of satident comments = " 			<< theSinex.satident_comments	.size() << endl;
	cout << "count of sat idents = " 					<< theSinex.list_satidents		.size() << endl;
	cout << "count of prn comments = " 					<< theSinex.satprn_comments		.size() << endl;
	cout << "count of prns = " 							<< theSinex.list_satprns		.size() << endl;
	cout << "count of freq channel comments = " 		<< theSinex.satfreqchn_comments	.size() << endl;
	cout << "count of freq channels = " 				<< theSinex.list_satfreqchns	.size() << endl;
	cout << "count of mass comments = " 				<< theSinex.satmass_comments	.size() << endl;
	cout << "count of satmasses = " 					<< theSinex.list_satmasses		.size() << endl;
	cout << "count of COM comments = " 					<< theSinex.satcom_comments		.size() << endl;
	cout << "count of sat COMs = "			 			<< theSinex.list_satcoms		.size() << endl;
	cout << "count of sat ecc comments = " 				<< theSinex.satecc_comments		.size() << endl;
	cout << "count of sat eccentricities = " 			<< theSinex.list_sateccs		.size() << endl;
	cout << "count of sat power comments = " 			<< theSinex.satpower_comments	.size() << endl;
	cout << "count of sat powers = " 					<< theSinex.list_satpowers		.size() << endl;
	cout << "count of sat phase centre comments = " 	<< theSinex.satpc_comments		.size() << endl;
	cout << "count of sat phase centres = " 			<< theSinex.list_satpcs			.size() << endl;
	cout << "count of source ID comments = " 			<< theSinex.sourceid_comments	.size() << endl;
	cout << "count of source IDs = " 					<< theSinex.list_source_ids		.size() << endl;
	cout << "count of nutation comments = " 			<< theSinex.nutation_comments	.size() << endl;
	cout << "count of nutations = " 					<< theSinex.list_nutcodes		.size() << endl;
	cout << "count of precession_comments = "	 		<< theSinex.precession_comments	.size() << endl;
	cout << "count of precessions = " 					<< theSinex.list_precessions	.size() << endl;
}

int sinex_site_count()
{
	return theSinex.map_siteids.size();
}

int sinex_sat_count()
{
	int result = theSinex.list_satidents.size();

	if (result == 0)
		result = theSinex.list_satids.size();

	return result;
}

void setRestrictiveEndTime(
	int current[3],
	int potential[3])
{
	static int zeros[3] = {};
		
	if (time_compare(current, zeros) == 0)
	{
		//current is zero, just use the new version for the end time
		for (int i = 0; i < 3; i++)
		{
			current[i] = potential[i];
		}
		
		return;
	}
	
	if (time_compare(potential, zeros) == 0)
	{
		//potential time is zero, thats not restrictive, keep the current time
		
		return;
	}
	
	if (time_compare(potential, current) < 0)
	{
		//potential end time is more restrictive
		for (int i = 0; i < 3; i++)
		{
			current[i] = potential[i];
		}
		
		return;
	}
}

// return value is 0 for success. Otherwise the value indicates the section where data was not found.
// 1 = siteid
// 2 = recevier
// 3 = antenna
// 4 = eccentricity
// 5 = gps phase center
// 6 = estimate
int getstnsnx(
	string				station,
	int					yds[3],
	Sinex_stn_snx_t&	stn_snx)
{
	bool	found 		= false;
	int		retval 		= 0;

	stn_snx = {};
	
	for (int i = 0; i < 3; i++)
	{
		stn_snx.start[i]	= yds[i];
	}

	// search siteids for station (not time dependent)
	auto siteIdIt = theSinex.map_siteids.find(station);
	if (siteIdIt != theSinex.map_siteids.end())
	{
		auto& siteId = siteIdIt->second;
		siteId.used = true;
		stn_snx.sitecode	= station;
		stn_snx.ptcode		= siteId.ptcode;
		stn_snx.monuid		= siteId.domes;
		stn_snx.typecode	= siteId.typecode;
		stn_snx.desc		= siteId.desc;
		stn_snx.long_deg	= siteId.long_deg;
		stn_snx.long_min	= siteId.long_min;
		stn_snx.long_sec	= siteId.long_sec;
		stn_snx.lat_deg		= siteId.lat_deg;
		stn_snx.lat_min		= siteId.lat_min;
		stn_snx.lat_sec		= siteId.lat_sec;
		stn_snx.height		= siteId.height;
		found = true;
	}

	if (!found)
		return 1;

	found = false;
	
	
	GTime time = yds2time(yds);
	
	auto receiverIt = theSinex.map_receivers.find(station);
	if (receiverIt != theSinex.map_receivers.end())
	{
		auto& recTimeMap = receiverIt->second;

		auto timeRec_it = recTimeMap.lower_bound(time);
		if (timeRec_it != recTimeMap.end())
		{
			auto& receiver = timeRec_it->second;
		
			receiver.used = true;
			
// 			stn_snx.rectype			= receiver.rectype;
// 			stn_snx.recsn			= receiver.recsn;
// 			stn_snx.recfirm			= receiver.recfirm;
			found = true;
		
			// get next next start time as end time for this aspect
			if (timeRec_it != recTimeMap.begin())
			{
				timeRec_it--;
				auto& nextReceiver = timeRec_it->second;
				
				setRestrictiveEndTime(receiver.recend,	nextReceiver.recstart);
			}

			setRestrictiveEndTime(stn_snx.start,	receiver.recend);
		}
	}
	
	if (!found)
		retval = 2;

	found = false;

	auto ant_it = theSinex.map_antennas[station].lower_bound(time);
	if (ant_it != theSinex.map_antennas[station].end())
	{
		auto& antenna = ant_it->second;
		found = true;
		antenna.used = true;
		
		stn_snx.anttype = antenna.anttype;
		stn_snx.antsn	= antenna.antsn;
		
		// get next next start time as end time for this aspect
		if (ant_it != theSinex.map_antennas[station].begin())
		{
			ant_it--;
			auto& nextAntenna = ant_it->second;
			
			setRestrictiveEndTime(antenna.antend,	nextAntenna.antstart);
		}
		
		setRestrictiveEndTime(stn_snx.start,	antenna.antend);
	}

	if (!found)
		retval = 3;

	found = false;

	auto ecc_it = theSinex.map_eccentricities[station].lower_bound(time);
	if (ecc_it != theSinex.map_eccentricities[station].end())
	{
		auto& eccentricity = ecc_it->second;
		found = true;
		eccentricity.used = true;
		
		stn_snx.eccrs = eccentricity.eccrs;

		for (int i = 0; i < 3; i++)
			stn_snx.ecc[i] = eccentricity.ecc[i];
		
		// get next next start time as end time for this aspect
		if (ecc_it != theSinex.map_eccentricities[station].begin())
		{
			ecc_it--;
			auto& nextEcc = ecc_it->second;
			
			setRestrictiveEndTime(eccentricity.eccend,	nextEcc.eccstart);
		}
		
		setRestrictiveEndTime(stn_snx.stop,		eccentricity.eccend);
	}

	if (!found)
		retval = 4;

// 	found = false;
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
		retval = 5;

	for (auto estMap_ptr :	{
								&theSinex.map_estimates_primary,
								&theSinex.map_estimates
							})
	{
		found = true;
		auto& estMap_ = *estMap_ptr;
		
		for (int i = 0; i < 3; i++)
		{
			string type = "STA?  ";
			type[3] = 'X' + i;
			
			auto& estMap	= estMap_[station][type];
			
			Sinex_solestimate_t* estimate_ptr = nullptr;
			
			auto est_it = estMap.lower_bound(time);
			if (est_it != estMap.end())
			{
				estimate_ptr = &est_it->second;
		
				// get next next start time as end time for this aspect
				if (est_it != estMap.begin())
				{
					est_it--;
					auto& nextEst = est_it->second;
			
					setRestrictiveEndTime(stn_snx.stop,		nextEst.refepoch);
				}
			}
			else
			{
				//just use the first chronologically, (last when sorted as they are) instead
				auto est_Rit = estMap.rbegin();
				if (est_Rit == estMap.rend())
				{
					//actually theres no estimate for this thing
					found = false;
					break;
				}
				
				estimate_ptr = &est_Rit->second;
			}
			estimate_ptr->used = true;
			
			stn_snx.pos(i)	= estimate_ptr->estimate;
			stn_snx.primary	= estimate_ptr->primary;
		}
		
		if (found)
		{
			//if found in primary map, skip the secondary map
			break;
		}
	}
	

	if (found == false)
	{
		retval = 6;
	}

	return retval;
}
