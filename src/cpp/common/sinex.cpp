
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

/**
 */
FileType SNX__()
{

}

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

bool compare(
	string& one,
	string& two)
{
	if (one.compare(two) == 0)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexInputFile& one,
	SinexInputFile& two)
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

bool compare(
	SinexSolStatistic& one,
	SinexSolStatistic& two)
{
	if (one.name.compare(two.name) == 0)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexSatPc& one,
	SinexSatPc& two)
{
	if 	( one.svn.compare(two.svn) == 0
		&&one.freq	== two.freq
		&&one.freq2	== two.freq2)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexSatEcc& one,
	SinexSatEcc& two)
{
	if 	( one.svn.compare(two.svn) == 0
		&&one.type == two.type)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexSatMass& one,
	SinexSatMass& two)
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

bool compare(
	SinexSatFreqChn& one,
	SinexSatFreqChn& two)
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

bool compare(
	SinexSatId& one,
	SinexSatId& two)
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

bool compare(
	SinexPreCode& one,
	SinexPreCode& two)
{
	if 	(one.precesscode.compare(two.precesscode) == 0)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexSourceId& one,
	SinexSourceId& two)
{
	if 	(one.source.compare(two.source) == 0)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexNutCode& one,
	SinexNutCode& two)
{
	if 	(one.nutcode.compare(two.nutcode) == 0)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexSatPrn& one,
	SinexSatPrn& two)
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

bool compare(
	SinexSatPower& one,
	SinexSatPower& two)
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

bool compare(
	SinexSatCom& one,
	SinexSatCom& two)
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

bool compare(
	SinexAck& one,
	SinexAck& two)
{
	if 	( one.agency		.compare(two.agency)		== 0
		&&one.description	.compare(two.description)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexInputHistory& one,
	SinexInputHistory& two)
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

bool compare(
	SinexSiteId& one,
	SinexSiteId& two)
{
	if 	(one.sitecode.compare(two.sitecode) == 0)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexSiteData& one,
	SinexSiteData& two)
{
	if 	( one.site.		compare(two.site)		== 0
		&&one.sitecode.	compare(two.sitecode)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexReceiver& one,
	SinexReceiver& two)
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

bool compare(
	SinexAntenna& one,
	SinexAntenna& two)
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

bool compare(
	SinexGpsPhaseCenter& one,
	SinexGpsPhaseCenter& two)
{
	if 	( one.antname	.compare(two.antname)	== 0
		&&one.serialno	.compare(two.serialno)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexGalPhaseCenter& one,
	SinexGalPhaseCenter& two)
{
	if 	( one.antname	.compare(two.antname)	== 0
		&&one.serialno	.compare(two.serialno)	== 0)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexSiteEcc& one,
	SinexSiteEcc& two)
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

bool compare(
	SinexSolEpoch& one,
	SinexSolEpoch& two)
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

bool compare(
	SinexSolEstimate& one,
	SinexSolEstimate& two)
{
	if 	( one.sitecode.compare(two.sitecode)	== 0
		&&one.type.compare(two.type)			== 0
		&&one.refepoch == two.refepoch)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexSolApriori& one,
	SinexSolApriori& two)
{
	if 	( one.sitecode.compare(two.sitecode)		== 0
		&&one.param_type.compare(two.param_type)	== 0
		&&one.epoch == two.epoch)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexSolNeq& one,
	SinexSolNeq& two)
{
	if 	( one.site.compare(two.site)	== 0
		&&one.ptype.compare(two.ptype)	== 0
		&&one.epoch == two.epoch)
	{
		return true;
	}
	return false;
}

bool compare(
	SinexSolMatrix& one,
	SinexSolMatrix& two)
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
void dedupeSinex()
{
	// do the lists which are not sorted first

	// general stuff
	dedupe(theSinex.refstrings);
	dedupe(theSinex.inputHistory);
	dedupe(theSinex.inputFiles);
	dedupe(theSinex.acknowledgements);
	dedupe(theSinex.listnutcodes);
	dedupe(theSinex.listprecessions);
	dedupe(theSinex.listsourceids);
	dedupe(theSinex.listsatids);
	dedupe(theSinex.listsatprns);
	dedupe(theSinex.listsatfreqchns);
	dedupe(theSinex.listsatcoms);
	dedupe(theSinex.listsateccs);
	dedupe(theSinex.listsatpcs);
	dedupe(theSinex.liststatistics);

	// 	// TODO: need to make sure sitecode & type match on index
	// site stuff
	// all data is sorted before coming in here, so it suffices to just check against the previous value
	dedupeB(theSinex.listsitedata);
	dedupeB(theSinex.listgpspcs);
	dedupeB(theSinex.listgalpcs);
	dedupeB(theSinex.listnormaleqns);

	for (matrix_type	t = ESTIMATE;		t < MAX_MATRIX_TYPE;	t = static_cast<matrix_type>	(static_cast<int>(t) + 1))
	for (matrix_value	v = CORRELATION;	v < MAX_MATRIX_VALUE;	v = static_cast<matrix_value>	(static_cast<int>(v) + 1))
	{
		if (theSinex.matrixmap[t][v].empty())
			continue;

		dedupeB(theSinex.matrixmap[t][v]);
	}

	return;
}

// TODO; What if we are reading a second file. What wins?
bool readSnxHeader(std::ifstream& in)
{
	string line;

	std::getline(in, line);

	if (in.eof())
	{
		BOOST_LOG_TRIVIAL(error) << "Error: empty file";
		return false;
	}

	// verify line contents
	if 	(  line[0] != '%'
		|| line[1] != '='
		|| line[2] != 'S'
		|| line[3] != 'N'
		|| line[4] != 'X')
	{
		// error. not a sinex file
		BOOST_LOG_TRIVIAL(error) << "Error: Not a sinex file";
		return false;
	}

	// remaining characters indiciate properties of the file
	if (line.length() > 5)
	{
		const char* buff = line.c_str();
		char create_agc[4];
		char data_agc[4];
		char solcontents[7];

		int  readcount = sscanf(buff + 6, "%4lf %3s %2lf:%3lf:%5lf %3s %2lf:%3lf:%5lf %2lf:%3lf:%5lf %c %5d %c %c %c %c %c %c %c",
						&theSinex.ver,
						create_agc,
						&theSinex.filedate[0],
						&theSinex.filedate[1],
						&theSinex.filedate[2],
						data_agc,
						&theSinex.solutionstartdate[0],
						&theSinex.solutionstartdate[1],
						&theSinex.solutionstartdate[2],
						&theSinex.solutionenddate[0],
						&theSinex.solutionenddate[1],
						&theSinex.solutionenddate[2],
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
			BOOST_LOG_TRIVIAL(error) << "Error: Not enough parameters on header line (expected min 15), got " << readcount;
			return false;
		}

		while (readcount < 21)
		{
			solcontents[readcount - 15] = ' ';
			readcount++;
		}

		solcontents[6] = '\0';

		theSinex.createagc	= create_agc;
		theSinex.dataagc	= data_agc;
		theSinex.solcont	= solcontents;

		nearestYear(theSinex.filedate[0]);
		nearestYear(theSinex.solutionstartdate[0]);
		nearestYear(theSinex.solutionenddate[0]);
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
	SinexInputHistory siht;

	siht.code			= '+';
	siht.fmt			= theSinex.ver;
	siht.create_agency	= theSinex.createagc;
	siht.data_agency	= theSinex.dataagc;
	siht.obs_tech		= theSinex.obsCode;
	siht.constraint		= theSinex.constCode;
	siht.num_estimates	= theSinex.numparam;
	siht.contents		= theSinex.solcont;
	siht.create_time	= theSinex.filedate;
	siht.start			= theSinex.solutionstartdate;
	siht.stop			= theSinex.solutionenddate;

	if (theSinex.inputHistory.empty())
		theSinex.inputHistory.push_back(siht);

	theSinex.ver = sinexVer;

	if (data_agc.size() > 0)		theSinex.dataagc = data_agc;
	else							theSinex.dataagc = theSinex.createagc;

	theSinex.createagc				= create_agc;
	theSinex.solcont				= contents;
	theSinex.filedate				= timeGet();
	theSinex.solutionstartdate	= soln_start;
	theSinex.solutionenddate		= soln_end;

	if (obsCode		!= ' ')
		theSinex.obsCode	= obsCode;

	if (constCode 	!= ' ')
		theSinex.constCode	= constCode;

	theSinex.numparam = numParam;
}

void writeSnxHeader(std::ofstream& out)
{
	char line[81];
	char c;
	int  i;

	int offset = 0;
	offset += snprintf(line + offset, sizeof(line) - offset, "%%=SNX %4.2lf %3s %2.2d:%3.3d:%5.5d %3s %2.2d:%3.3d:%5.5d %2.2d:%3.3d:%5.5d %c %5d %c",
			theSinex.ver,
			theSinex.createagc.c_str(),
			(int)theSinex.filedate[0] % 100,
			(int)theSinex.filedate[1],
			(int)theSinex.filedate[2],
			theSinex.dataagc.c_str(),
			(int)theSinex.solutionstartdate[0] % 100,
			(int)theSinex.solutionstartdate[1],
			(int)theSinex.solutionstartdate[2],
			(int)theSinex.solutionenddate[0] % 100,
			(int)theSinex.solutionenddate[1],
			(int)theSinex.solutionenddate[2],
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

	out << line << "\n";
}

void parseReference(string& line)
{
	theSinex.refstrings.push_back(line);
}

void writeAsComments(
	Trace&			out,
	list<string>&	comments)
{
	for (auto& comment : comments)
	{
		string line = comment;

		// just make sure it starts with * as required by format
		line[0] = '*';

		out << line << "\n";
	}
}

void commentsOverride()
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

void writeSnxReference(ofstream& out)
{
	Block block(out, "FILE/REFERENCE");

	for (auto& refString : theSinex.refstrings)
	{
		out << refString << "\n";
	}
}

void writeSnxComments(ofstream& out)
{
	Block block(out, "FILE/COMMENT");

	for (auto& commentstring : theSinex.blockComments[block.blockName])
	{
		out << commentstring << "\n";
	}
}

void parseInputHistory(string& line)
{
	SinexInputHistory siht;
	// remaining characters indiciate properties of the history

	if (line.length() > 5)
	{
		const char* buff = line.c_str();
		char create_agc[4];
		char data_agc[4];
		char solcontents[7];
		int  readcount;

		siht.code = line[1];

		readcount = sscanf(buff + 6, "%4lf %3s %2lf:%3lf:%5lf %3s %2lf:%3lf:%5lf %2lf:%3lf:%5lf %c %5d %c %c %c %c %c %c %c",
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

void writeSnxInputHistory(ofstream& out)
{
	Block block(out, "INPUT/HISTORY");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto it = theSinex.inputHistory.begin(); it != theSinex.inputHistory.end(); it++)
	{
		char	line[81] = {};
		int		offset = 0;
		SinexInputHistory siht = *it;
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

		out << line << "\n";
	}
}

void parseInputFiles(string& line)
{
	SinexInputFile sif;
	char agency[4];
	const char* buff	= line.c_str();
	sif.file		= line.substr(18, 29);
	sif.description	= line.substr(48, 32);

	int  readcount = sscanf(buff + 1, "%3s %2lf:%3lf:%5lf",
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

void writeSnxInputFiles(ofstream& out)
{
	Block block(out, "INPUT/FILES");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& inputFile : theSinex.inputFiles)
	{
		SinexInputFile& sif = inputFile;

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
		out << line << sif.file << " " << sif.description << "\n";
	}
}

void parseAcknowledgements(string& line)
{
	SinexAck sat;

	sat.description	= line.substr(5);
	sat.agency		= line.substr(1, 3);

	theSinex.acknowledgements.push_back(sat);
}

void writeSnxAcknowledgements(ofstream& out)
{
	Block block(out, "INPUT/ACKNOWLEDGEMENTS");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& acknowledgement : theSinex.acknowledgements)
	{
		SinexAck& ack = acknowledgement;

		char line[81];
		snprintf(line, sizeof(line), " %3s %s", ack.agency.c_str(), ack.description.c_str());

		out << line << "\n";
	}
}

void parseSiteIds(string& line)
{
	const char* buff = line.c_str();
	SinexSiteId sst;

	sst.sitecode	= trim(line.substr(1, 4));
	sst.ptcode		= line.substr(6, 2);
	sst.domes		= line.substr(9, 9);
	sst.typecode 	= line[19];
	sst.desc		= line.substr(21, 22);


	int    readcount = sscanf(buff + 44, "%3d %2d %4lf %3d %2d %4lf %7lf",
						&sst.lon_deg,
						&sst.lon_min,
						&sst.lon_sec,
						&sst.lat_deg,
						&sst.lat_min,
						&sst.lat_sec,
						&sst.height);

	if (readcount == 7)
	{
		theSinex.mapsiteids[sst.sitecode] = sst;
	}
}

void writeSnxSiteids(ofstream& out)
{
	Block block(out, "SITE/ID");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [id, ssi] : theSinex.mapsiteids)
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
bool compareSiteData(
	const SinexSiteData& left,
	const SinexSiteData& right)
{
	int comp = left.site.compare(right.site);

	if (comp == 0)
		comp = left.sitecode.compare(right.sitecode);

	return (comp < 0);
}

void parseSiteData(string& line)
{
	const char* buff = line.c_str();

	SinexSiteData sst;

	sst.site		= line.substr(1, 4);
	sst.station_pt	= line.substr(6, 2);
	sst.soln_id		= line.substr(9, 4);
	sst.sitecode	= line.substr(14, 4);
	sst.site_pt		= line.substr(18, 2);
	sst.sitesoln	= line.substr(20, 4);

	sst.obscode		= line[24];
	UYds    start;
	UYds    end;
	UYds    create;
	char   agency[4];

	int    readcount;

	readcount = sscanf(buff + 28, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf %3s %2lf:%3lf:%5lf",
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

		theSinex.listsitedata.push_back(sst);
	}
}

void writeSnxSitedata(ofstream& out, list<SinexRecData>* pstns)
{
	Block block(out, "SITE/DATA");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& sitedata : theSinex.listsitedata)
	{
		SinexSiteData& ssd = sitedata;
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
			out << line << "\n";
	}
}

void parseReceivers(string& line)
{
	const char* buff = line.c_str();

	SinexReceiver srt;

	srt.sitecode	= trim(line.substr(1, 4));
	srt.ptcode		= line.substr(6, 2);
	srt.solnid		= line.substr(9, 4);
	srt.typecode	= line[14];
	srt.type		= line.substr(42, 20);
	srt.sn			= line.substr(63, 5);
	srt.firm 		= trim(line.substr(69, 11));
	int readcount;

	readcount = sscanf(buff + 16, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf",
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

		theSinex.mapreceivers[srt.sitecode][srt.start] = srt;
	}
}

void writeSnxReceivers(ofstream& out)
{
	Block block(out, "SITE/RECEIVER");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [site, timemap] : theSinex.mapreceivers)
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

void parseAntennas(string& line)
{
	const char* buff = line.c_str();

	SinexAntenna ant;

	ant.sitecode	= trim(line.substr(1, 4));
	ant.ptcode		= line.substr(6, 2);
	ant.solnnum		= line.substr(9, 4);
	ant.typecode	= line[14];
	ant.type		= line.substr(42, 20);
	ant.sn			= trim(line.substr(63, 5));

	int    readcount = sscanf(buff + 16, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf",
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

		theSinex.mapantennas[ant.sitecode][ant.start] = ant;
// 				theSinex.list_antennas.push_back(ant);
	}
}

void writeSnxAntennas(ofstream& out)
{
	Block block(out, "SITE/ANTENNA");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [site, antmap]	: theSinex.mapantennas)
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
bool compareGpsPc(
	SinexGpsPhaseCenter& left,
	SinexGpsPhaseCenter& right)
{
	int comp = left.antname.compare(right.antname);

	if (comp == 0)
		comp = left.serialno.compare(right.serialno);

	return (comp < 0);
}

void parseGpsPhaseCenters(string& line)
{
	const char* buff = line.c_str();
	SinexGpsPhaseCenter sgpct;

	sgpct.antname	= line.substr(1, 20);
	sgpct.serialno	= line.substr(22, 5);
	sgpct.calib		= line.substr(70, 10);

	int readcount = sscanf(buff + 28, "%6lf %6lf %6lf %6lf %6lf %6lf",
								&sgpct.L1[0],
								&sgpct.L1[1],
								&sgpct.L1[2],
								&sgpct.L2[0],
								&sgpct.L2[1],
								&sgpct.L2[2]);

	if (readcount == 6)
	{
		theSinex.listgpspcs.push_back(sgpct);
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


void writeSnxGpsPcs(ofstream& out, list<SinexRecData>* pstns)
{
	Block block(out, "SITE/GPS_PHASE_CENTER");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& gps_pc : theSinex.listgpspcs)
	{
		SinexGpsPhaseCenter& sgt = gps_pc;
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
			out << line << "\n";
		}
	}
}

// compare by antenna type and serial number. return true0 if left < right
bool compareGalPc(
	SinexGalPhaseCenter& left,
	SinexGalPhaseCenter& right)
{
	int comp = left.antname.compare(right.antname);

	if (comp == 0)
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

	auto& line1 = lines[0];
	auto& line2 = lines[1];
	auto& line3 = lines[2];

	SinexGalPhaseCenter sgpct;

	sgpct.antname	= line1.substr(1, 20);
	sgpct.serialno	= line1.substr(22, 5);
	sgpct.calib		= line1.substr(69, 10);

	int readcount1 = sscanf(line1.c_str() + 28, "%6lf %6lf %6lf %6lf %6lf %6lf",
							&sgpct.L1[0],
							&sgpct.L1[1],
							&sgpct.L1[2],
							&sgpct.L5[0],
							&sgpct.L5[1],
							&sgpct.L5[2]);

		// Do we need to check the antenna name and serial each time? I am going to assume not
	int	readcount2 = sscanf(line2.c_str() + 28, "%6lf %6lf %6lf %6lf %6lf %6lf",
							&sgpct.L6[0],
							&sgpct.L6[1],
							&sgpct.L6[2],
							&sgpct.L7[0],
							&sgpct.L7[1],
							&sgpct.L7[2]);
	int	readcount3 = sscanf(line3.c_str() + 28, "%6lf %6lf %6lf",
							&sgpct.L8[0],
							&sgpct.L8[1],
							&sgpct.L8[2]);

	if	(  readcount1 == 6
		&& readcount2 == 6
		&& readcount3 == 3)
	{
		theSinex.listgalpcs.push_back(sgpct);
	}
}


void writeSnxGalPcs(ofstream& out, list<SinexRecData>* pstns)
{
	Block block(out, "SITE/GAL_PHASE_CENTER");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& gal_pc : theSinex.listgalpcs)
	{
		SinexGalPhaseCenter& sgt = gal_pc;
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
			out << line << "\n";
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

			out << line << "\n";
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
			out << line << "\n";
		}
	}
}

void parseSiteEccentricity(string& line)
{
	const char* buff = line.c_str();
	SinexSiteEcc sset;

	sset.sitecode 	= trim(line.substr(1, 4));
	sset.ptcode		= line.substr(6, 2);
	sset.solnnum	= line.substr(9, 4);
	sset.typecode	= line[14];
	sset.rs			= line.substr(42, 3);
	char   junk[4];

	int readcount = sscanf(buff + 16, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf %3s %8lf %8lf %8lf",
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

		theSinex.mapeccentricities[sset.sitecode][sset.start] = sset;
	}
}

void writeSnxSiteEccs(ofstream& out)
{
	Block block(out, "SITE/ECCENTRICITY");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [id, setMap] : theSinex.mapeccentricities)
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

bool compareSiteEpochs(
	SinexSolEpoch& left,
	SinexSolEpoch& right)
{
	int comp = left.sitecode.compare(right.sitecode);
	int i = 0;

	while (comp == 0 && i < 3)
	{
		comp = left.start[i] - right.start[i];
		i++;
	}

	return (comp < 0);
}

void parseEpochs(string& line)
{
	const char* buff = line.c_str();

	SinexSolEpoch sst;

	sst.sitecode	= trim(line.substr(1, 4));
	sst.ptcode		= line.substr(6, 2);
	sst.solnnum		= line.substr(9, 4);
	sst.typecode	= line[14];

	int readcount = sscanf(buff + 16, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf %2lf:%3lf:%5lf",
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

void writeSnxEpochs(
	Trace& out)
{
	string blockName;
	if (theSinex.epochshavebias)		blockName = "BIAS/EPOCHS";
	else								blockName = "SOLUTION/EPOCHS";

	Block block(out, blockName);

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [id, sst] : theSinex.solEpochMap)
	{
		tracepdeex(0, out, " %4s %2s %4s %c %02d:%03d:%05d %02d:%03d:%05d %02d:%03d:%05d\n",
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

void parseStatistics(string& line)	//todo aaron, is this type stuff really necessary
{
	const char* buff = line.c_str();

	string  stat = line.substr(1, 30);
	double  dval;
	int		ival;
	short	etype;

	if (line.substr(33).find(".") != string::npos)
	{
		dval = (double)atof(buff + 33);
		etype = 1;
	}
	else
	{
		ival = atoi(buff + 33);
		etype = 0;
	}

	SinexSolStatistic sst;
	sst.name = trim(stat);
	sst.etype = etype;

	if (etype == 0)
		sst.value.ival = ival;

	if (etype == 1)
		sst.value.dval = dval;

	theSinex.liststatistics.push_back(sst);
}

void writeSnxStatistics(ofstream& out)
{
	Block block(out, "SOLUTION/STATISTICS");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& statistic : theSinex.liststatistics)
	{
		char line[81];

		if (statistic.etype == 0) // int
			snprintf(line, sizeof(line), " %-30s %22d", statistic.name.c_str(), statistic.value.ival);

		if (statistic.etype == 1) // double
			snprintf(line, sizeof(line), " %-30s %22.15lf", statistic.name.c_str(), statistic.value.dval);

		out << line << "\n";
	}
}


void parseSolutionEstimates(
	string& line)
{
	const char* buff = line.c_str();

	SinexSolEstimate sst;

	sst.file		= theSinex.currentFile;
	sst.type		= line.substr(7,	6);
	sst.sitecode	= line.substr(14,	4);
	sst.ptcode		= line.substr(19,	2);
	sst.solnnum 	= line.substr(22,	4);

	sst.index		= (int)str2num(buff, 1, 5);

	int	readcount	= sscanf(buff + 27, "%2lf:%3lf:%5lf",
						&sst.refepoch[0],
						&sst.refepoch[1],
						&sst.refepoch[2]);

	sst.unit		= line.substr(40,	4);

	sst.constraint	= line[45];

	readcount		+= sscanf(buff + 47, "%21lf %11lf",
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

void writeSnxEstimatesFromFilter(
	ofstream&	out,
	KFState&	kfState)
{
	Block block(out, "SOLUTION/ESTIMATE");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

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

		string ptcode = theSinex.mapsiteids[key.str].ptcode;

		tracepdeex(0, out, " %5d %-6s %4s %2s %4d %02d:%03d:%05d %-4s %c %21.14le %11.5le\n",
				index,
				type.c_str(),
				key.str.c_str(),
				ptcode.c_str(),
				1,
				(int)theSinex.solutionenddate[0] % 100,
				(int)theSinex.solutionenddate[1],
				(int)theSinex.solutionenddate[2],
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
// 	out << "+SOLUTION/ESTIMATE" << "\n";
//
// 	writeAsComments(out, theSinex.estimate_comments);
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
// 		out << line << "\n";
// 	}
//
// 	out << "-SOLUTION/ESTIMATE" << "\n";
// }


void parseApriori(string& line)
{
	const char* buff = line.c_str();

	SinexSolApriori sst = {};

	sst.idx			= (int)str2num(buff, 1, 5);
	sst.param_type	= line.substr(7, 6);
	sst.sitecode	= line.substr(14, 4);
	sst.ptcode		= line.substr(19, 2);
	sst.solnnum		= line.substr(22, 4);

	char   unit[5];

	unit[4] = '\0';

	int    readcount = sscanf(buff + 27, "%2lf:%3lf:%5lf %4s %c %21lf %11lf",
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

		theSinex.apriorimap[sst.idx] = sst;
	}
}

void writeSnxApriori(ofstream& out, list<SinexRecData>* pstns = nullptr)
{
	Block block(out, "SOLUTION/APRIORI");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [index, apriori] : theSinex.apriorimap)
	{
		SinexSolApriori& sst = apriori;
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

		out << line << "\n";
	}
}

void writeSnxAprioriFromReceivers(
	ofstream& out,
	map<string, Receiver>&		receiverMap)
{
	Block block(out, "SOLUTION/APRIORI");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

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

void parseNormals(string& line)
{
	const char* buff = line.c_str();

	SinexSolNeq sst;

	sst.param		= (int)str2num(buff, 2, 5);
	sst.ptype		= line.substr(7, 6);
	sst.site		= line.substr(14, 4);
	sst.pt			= line.substr(19, 2);
	sst.solnnum		= line.substr(22, 4);
	char   unit[5];

	unit[4] = '\0';

	int    readcount = sscanf(buff + 27, "%2lf:%3lf:%5lf %4s %c %21lf",
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

		theSinex.listnormaleqns.push_back(sst);
	}
}

void writeSnxNormal(ofstream& out, list<SinexRecData>* pstns = nullptr)
{
	Block block(out, "SOLUTION/NORMAL_EQUATION_VECTOR");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& sst : theSinex.listnormaleqns)
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

		out << line << "\n";
	}
}

matrix_type		mat_type;
matrix_value	mat_value;

void parseMatrix(string& line)//, matrix_type type, matrix_value value)
{
	const char* buff = line.c_str();

// 	//todo aaron, this is only half complete, the maxrow/col arent used but should be with multiple input matrices.
	int		maxrow = 0;
	int		maxcol = 0;
	SinexSolMatrix smt;

	int readcount = sscanf(buff, " %5d %5d %21lf %21lf %21lf",
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

		theSinex.matrixmap[mat_type][mat_value].push_back(smt);
	}
}

void parseSinexEstimates(
	string& line)
{

}

void parseSinexEstimateMatrix(
	string&	line)
{

}

void writeSnxMatricesFromFilter(
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

		writeAsComments(out, theSinex.blockComments[block.blockName]);

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


void parseDataHandling(string& line)
{
	SinexDataHandling sdt;

	const char* buff = line.c_str();

	sdt.sitecode	= trim(line.substr(1, 4));	//4 - CDP ID
	sdt.ptcode		= line.substr(6, 2);	//2 - satellites these biases apply to (-- = all)
	sdt.solnnum		= line.substr(9, 4);	//4 - solution number
	sdt.t			= line.substr(14, 1);	//1

	int	readcount	= sscanf(buff + 16, "%2lf:%3lf:%5lf",
					&sdt.epochstart[0],
					&sdt.epochstart[1],
					&sdt.epochstart[2]);
	readcount		+= sscanf(buff + 29, "%2lf:%3lf:%5lf",
					&sdt.epochend[0],
					&sdt.epochend[1],
					&sdt.epochend[2]);

	sdt.m			= line.substr(42, 1);	//1

	if (line.size() >= 79)
	{
		sdt.estimate	= str2num(buff, 44, 12);
		sdt.stddev		= str2num(buff, 57,  7);
		sdt.estrate		= str2num(buff, 65,  9);
		sdt.unit		= line.substr(75, 4);	//4 - units of estimate
	}
	if (line.size() > 82)
	{
		sdt.comments	= line.substr(82);
	}

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

		theSinex.mapdatahandling[sdt.sitecode][sdt.ptcode][sdt.m.front()][time] = sdt;
	}
}

void parsePrecode(string& line)
{
	SinexPreCode snt;

	snt.precesscode	= line.substr(1, 8);
	snt.comment		= line.substr(10);

	theSinex.listprecessions.push_back(snt);
}

void writeSnxPreCodes(ofstream& out)
{
	Block block(out, "PRECESSION/DATA");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& spt : theSinex.listprecessions)
	{
		char line[81];

		snprintf(line, sizeof(line), " %8s %s", spt.precesscode.c_str(), spt.comment.c_str());

		out << line << "\n";
	}
}

void parseNutcode(string& line)
{
	SinexNutCode snt;

	snt.nutcode = line.substr(1, 8);
	snt.comment = line.substr(10);

	theSinex.listnutcodes.push_back(snt);
}

void writeSnxNutCodes(ofstream& out)
{
	Block block(out, "NUTATION/DATA");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& nutcode : theSinex.listnutcodes)
	{
		SinexNutCode& snt = nutcode;

		char line[81];

		snprintf(line, sizeof(line), " %8s %s", snt.nutcode.c_str(), snt.comment.c_str());

		out << line << "\n";
	}
}

void parseSourceIds(string& line)
{
	SinexSourceId ssi;

	ssi.source		= line.substr(1, 4);
	ssi.iers		= line.substr(6, 8);
	ssi.icrf		= line.substr(15, 16);
	ssi.comments	= line.substr(32);

	theSinex.listsourceids.push_back(ssi);
}

void writeSnxSourceIds(ofstream& out)
{
	Block block(out, "SOURCE/ID");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& source_id : theSinex.listsourceids)
	{
		SinexSourceId& ssi = source_id;

		char line[101];

		snprintf(line, sizeof(line), " %4s %8s %16s %s", ssi.source.c_str(), ssi.iers.c_str(), ssi.icrf.c_str(), ssi.comments.c_str());

		out << line << "\n";
	}
}

bool compareSatIds(
	SinexSatId& left,
	SinexSatId& right)
{
	int comp = left.svn.compare(right.svn);

	return (comp < 0);
}

void parseSatelliteIds(string& line)
{
	const char* buff = line.c_str();

	SinexSatId sst;

	sst.svn			= line.substr(1, 4);
	sst.prn			= sst.svn[0] + line.substr(6, 2);
	sst.cospar		= line.substr(9, 9);
	sst.obsCode		= line[18];
	sst.antRcvType	= line.substr(47);

	int 	readcount = sscanf(buff + 21, "%2lf:%3lf:%5lf %2lf:%3lf:%5lf",
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

		theSinex.listsatids.push_back(sst);
	}
}

void writeSnxSatIds(ofstream& out)
{
	Block block(out, "SATELLITE/ID");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& ssi : theSinex.listsatids)
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

		out << line << "\n";
	}
}

void parseSatelliteIdentifiers(string& line)
{
	const char* buff = line.c_str();

	SinexSatIdentity sst;

	sst.svn			= line.substr(1, 4);
	sst.cospar		= line.substr(6, 9);
	sst.category	= (int)str2num(buff, 16, 6);
	sst.blocktype	= trim(line.substr(23, 15));
	sst.comment		= line.substr(39);

	theSinex.satIdentityMap[sst.svn] = sst;

	nav.blocktypeMap[sst.svn] = sst.blocktype;
}

void writeSnxSatIdents(ofstream& out)
{
	Block block(out, "SATELLITE/IDENTIFIER");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [svn, ssi] : theSinex.satIdentityMap)
	{
		char line[101];

		snprintf(line, sizeof(line), " %4s %9s %6d %-15s %s",
				ssi.svn.c_str(),
				ssi.cospar.c_str(),
				ssi.category,
				ssi.blocktype.c_str(),
				ssi.comment.c_str());

		out << line << "\n";
	}
}

bool compareSatPrns(
	SinexSatPrn& left,
	SinexSatPrn& right)
{
	int comp = left.prn.compare(right.prn);

	return (comp < 0);
}

void parseSatPrns(string& line)
{
	const char* buff = line.c_str();

	SinexSatPrn spt;

	spt.svn			= line.substr(1, 4);
	spt.prn			= line.substr(36, 3);
	spt.comment		= line.substr(40);

	int readcount = sscanf(buff + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf",
						&spt.start[0],
						&spt.start[1],
						&spt.start[2],
						&spt.stop[0],
						&spt.stop[1],
						&spt.stop[2]);

	if (readcount == 6)
	{
		// No need to adjust years since for satellites the year is 4 digits ...
		theSinex.listsatprns.push_back(spt);

		nav.svnMap[SatSys(spt.prn.c_str())][spt.start] = spt.svn;
	}
}

void writeSnxSatPrns(ofstream& out)
{
	Block block(out, "SATELLITE/PRN");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	char line[101];

	for (auto& spt : theSinex.listsatprns)
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

		out << line << "\n";
	}
}

bool compareFreqChannels(
	SinexSatFreqChn& left,
	SinexSatFreqChn& right)
{
	// start by comparing SVN...
	int comp = left.svn.compare(right.svn);

	// then by start time if the same space vehicle
	for (int i = 0; i < 3; i++)
		if (comp == 0)
			comp = left.start[i] - right.start[i];

	return (comp < 0);
}

void parseSatFreqChannels(string& line)
{
	const char* buff = line.c_str();

	SinexSatFreqChn	sfc;

	sfc.svn		= line.substr(1, 4);
	sfc.comment	= line.substr(40);

	int readcount = sscanf(buff + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf %3d",
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
		theSinex.listsatfreqchns.push_back(sfc);
	}
}

void writeSnxSatFreqChn(ofstream& out)
{
	Block block(out, "SATELLITE/FREQUENCY_CHANNEL");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& sfc : theSinex.listsatfreqchns)
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

		out << line << "\n";
	}
}

void parseSatelliteMass(string& line)
{
	const char* buff = line.c_str();

	SinexSatMass	ssm;

	ssm.svn		= line.substr(1, 4);
	ssm.comment	= line.substr(46);

	int readcount = sscanf(buff + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf %9lf",
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
		theSinex.mapsatmasses[ssm.svn][ssm.start] = ssm;
	}
}

void writESnxSatMass(ofstream& out)
{
	Block block(out, "SATELLITE/MASS");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [svn,	ssmMap]	: theSinex.mapsatmasses)
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

		out << line << "\n";
	}
}

bool compareSatCom(
	SinexSatCom& left,
	SinexSatCom& right)
{
	// start by comparing SVN...
	int comp = left.svn.compare(right.svn);

	// then by start time if the same space vehicle
	for (int i = 0; i < 3; i++)
		if (comp == 0)
			comp = left.start[i] - right.start[i];

	return (comp < 0);
}

void parseSatelliteComs(string& line)
{
	const char* buff = line.c_str();

	SinexSatCom	sct;

	sct.svn		= line.substr(1, 4);
	sct.comment	= line.substr(66);

	int readcount = sscanf(buff + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf %9lf %9lf %9lf",
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
		theSinex.listsatcoms.push_back(sct);
	}
}

void writeSnxSatCom(ofstream& out)
{
	Block block(out, "SATELLITE/COM");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& sct : theSinex.listsatcoms)
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

		out << line << "\n";
	}
}

bool compareSatEcc(
	SinexSatEcc& left,
	SinexSatEcc& right)
{
	// start by comparing SVN...
	int comp = left.svn.compare(right.svn);

	// then by type (P or L)
	if (comp == 0)
		comp = static_cast<int>(left.type) - static_cast<int>(right.type);

	return (comp < 0);
}

void parseSatelliteEccentricities(string& line)
{
	const char* buff = line.c_str();

	SinexSatEcc	set;

	set.svn		= line.substr(1, 4);
	set.equip	= line.substr(6, 20);
	set.type	= line[27];
	set.comment	= line.substr(59);

	int readcount = sscanf(buff + 29, "%9lf %9lf %9lf",
						&set.ecc[0],
						&set.ecc[1],
						&set.ecc[2]);

	if (readcount == 3)
	{
		theSinex.listsateccs.push_back(set);
	}
}

void writeSnxSatEcc(ofstream& out)
{
	Block block(out, "SATELLITE/ECCENTRICITY");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& set : theSinex.listsateccs)
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

		out << line << "\n";
	}
}

void parseSatellitePowers(string& line)
{
	const char* buff = line.c_str();

	SinexSatPower	spt;

	spt.svn		= line.substr(1, 4);
	spt.comment	= line.substr(41);

	int readcount = sscanf(buff + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf %4d",
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
		theSinex.mapsatpowers[spt.svn][spt.start] = spt;
	}
}

void writeSnxSatPower(ofstream& out)
{
	Block block(out, "SATELLITE/TX_POWER");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [svn,	sptmap]	: theSinex.mapsatpowers)
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

		out << line << "\n";
	}
}

bool compareSatPc(
	SinexSatPc& left,
	SinexSatPc& right)
{
	// start by comparing SVN...
	int comp = left.svn.compare(right.svn);

	// then by the first freq number
	if (comp == 0)
		comp = static_cast<int>(left.freq) - static_cast<int>(right.freq);

	return (comp < 0);
}

void parseSatellitePhaseCenters(string& line)
{
	const char* buff = line.c_str();

	SinexSatPc		spt;

	int				readcount2;

	spt.svn		= line.substr(1, 4);
	spt.freq	= line[6];
	spt.freq2	= line[29];
	spt.antenna	= line.substr(52, 10);
	spt.type	= line[63];
	spt.model	= line[65];

	int readcount = sscanf(buff + 6, "%6lf %6lf %6lf",
						&spt.zxy[0],
						&spt.zxy[1],
						&spt.zxy[2]);

	if (spt.freq2 != ' ')
	{
		readcount2 = sscanf(buff + 31, "%6lf %6lf %6lf",
						&spt.zxy2[0],
						&spt.zxy2[1],
						&spt.zxy2[2]);
	}

	if 	(   readcount	== 3
		&&( spt.freq2	== ' '
			||readcount2	== 3))
	{
		theSinex.listsatpcs.push_back(spt);
	}
}

void writeSnxSatPc(ofstream& out)
{
	Block block(out, "SATELLITE/PHASE_CENTER");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& spt : theSinex.listsatpcs)
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

		out << line << "\n";
	}
}

void parseSinexSatYawRates(string& line)
{
	const char* buff = line.c_str();

	SinexSatYawRate entry;

	entry.svn			= line.substr(1, 4);
	entry.comment		= line.substr(51);

	int readCount = sscanf(buff + 6, "%4lf:%3lf:%5lf %4lf:%3lf:%5lf    %c %8lf",
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
	const char* buff = line.c_str();

	SinexSatAttMode entry;
	entry.svn			= line.substr(1, 4);
	int readCount = sscanf(buff + 6, "%4lf-%2lf-%2lf %2lf:%2lf:%2lf  %4lf-%2lf-%2lf %2lf:%2lf:%2lf ",
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

void nullFunction(string& line)
{

}

bool readSinex(
	const string& filepath)
{
// 	BOOST_LOG_TRIVIAL(info)
// 	<< "reading " << filepath;

	ifstream filestream(filepath);
	if (!filestream)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error opening sinex file" << filepath;
		return false;
	}

	bool pass = readSnxHeader(filestream);
	if (pass == false)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error reading header line.";

		return false;
	}

	theSinex.currentFile = filepath;

	void (*parseFunction)(string&) = nullFunction;

	string closure;

	bool failure = false;

	int lineNumber = 0;

	while (filestream)
	{
		string line;

		getline(filestream, line);

		lineNumber++;

		// test below empty line (ie continue if something on the line)
		if	(!filestream)
		{
			// error - did not find closure line. Report and clean up.
			BOOST_LOG_TRIVIAL(error)
			<< "Error: Closure line not found before end.";

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
				<< "Error: Incorrect section closure line encountered on line " << lineNumber << ": "
				<< closure << " != " << line;
			}
		}
		else if (line[0] == ' ')
		{
			try
			{
				//this probably needs specialty parsing - use a prepared function pointer.
				parseFunction(line);
			}
			catch (std::out_of_range& e)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Error: Sinex line width error on line "		<< lineNumber << ": '" << line << "'";
			}
			catch (...)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Error: Sinex parsing error on line "		<< lineNumber << ": '" << line << "'";
			}
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
			else if	(line == "+MODEL/RANGE_BIAS"				)	{ parseFunction = parseDataHandling;			}	// Same format w/ SOLUTION/DATA_HANDLING
			else if	(line == "+MODEL/TIME_BIAS"					)	{ parseFunction = parseDataHandling;			}	// Same format w/ SOLUTION/DATA_HANDLING
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
				<< "Error: unknown header line: " << line;

				failure = true;
			}

// 			int 	i;
// 									failure = read_snx_matrix			(filestream, NORMAL_EQN, INFORMATION, c);			break;
// 				case 15:
// 					if (!theSinex.epochs_have_bias && !theSinex.list_solepochs.empty())
// 					{
// 						BOOST_LOG_TRIVIAL(error)
// 						<< "cannot combine BIAS/EPOCHS and SOLUTION/EPOCHS blocks.";
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
// 						<< "cannot combine BIAS/EPOCHS and SOLUTION/EPOCHS blocks.";
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
				<< "Error: line starting '%' met not final line" << "\n" << line;

				failure = true;
			}

			break;
		}

		if (failure)
			break;
	}

	theSinex.listsatpcs.		sort(compareSatPc);
	theSinex.listsateccs.		sort(compareSatEcc);
	theSinex.listsitedata.		sort(compareSiteData);
	theSinex.listgpspcs.		sort(compareGpsPc);
	theSinex.listsatids.		sort(compareSatIds);
	theSinex.listsatfreqchns.	sort(compareFreqChannels);
	theSinex.listsatprns.		sort(compareSatPrns);
	theSinex.listsatcoms.		sort(compareSatCom);
	theSinex.listgalpcs.		sort(compareGalPc);

// 	theSinex.matrix_map[type][value].sort(compare_matrix_entries);
	dedupeSinex();

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

	commentsOverride();

	writeSnxHeader(filestream);

	if (!theSinex.refstrings.					empty())	{	writeSnxReference			(filestream);}
	if (!theSinex.blockComments["FILE/COMMENT"].empty())	{	writeSnxComments			(filestream);}
	if (!theSinex.inputHistory.					empty())	{	writeSnxInputHistory		(filestream);}
	if (!theSinex.inputFiles.					empty())	{	writeSnxInputFiles			(filestream);}
	if (!theSinex.acknowledgements.				empty())	{	writeSnxAcknowledgements	(filestream);}

	if (!theSinex.mapsiteids.					empty())	{	writeSnxSiteids				(filestream);}
//	if (!theSinex.listsitedata.					empty())	{	writeSnxSitedata			(filestream);}
	if (!theSinex.mapreceivers.					empty())	{	writeSnxReceivers			(filestream);}
	if (!theSinex.mapantennas.					empty())	{	writeSnxAntennas			(filestream);}
//	if (!theSinex.listgpspcs.					empty())	{	writeSnxGps_pcs				(filestream);}
//	if (!theSinex.listgalpcs.					empty())	{	writeSnxGal_pcs				(filestream);}
	if (!theSinex.mapeccentricities.			empty())	{	writeSnxSiteEccs			(filestream);}
	if (!theSinex.solEpochMap.					empty())	{	writeSnxEpochs				(filestream);}
//	if (!theSinex.liststatistics.				empty())	{	writeSnxStatistics			(filestream);}
//	if (!theSinex.estimatesmap.					empty())		writeSnxEstimates			(filestream);
																writeSnxEstimatesFromFilter	(filestream, kfState);
//	if (!theSinex.apriori_map.					empty())	{	writeSnxApriori				(filestream);}
																writeSnxAprioriFromReceivers(filestream, receiverMap);
// 		if (!theSinex.list_normal_eqns.			empty())	{	writeSnxNormal				(filestream);}

	{
// 																writeSnxMatrices			(filestream, stationListPointer);
																writeSnxMatricesFromFilter	(filestream, kfState);
	}

//	if (!theSinex.listsourceids.				empty())	{	writeSnxSourceIds				(filestream);}
//	if (!theSinex.listnutcodes.					empty())	{	writeSnxNutCodes				(filestream);}
//	if (!theSinex.listprecessions.				empty())	{	writeSnxPreCodes				(filestream);}

	filestream << "%ENDSNX" << "\n";
}


void sinexAddStatistic(
	const string& what,
	const int val)
{
	SinexSolStatistic sst;

	sst.name		= what;
	sst.etype		= 0;
	sst.value.ival	= val;

	theSinex.liststatistics.push_back(sst);
}

void sinexAddStatistic(
	const string& what,
	const double val)
{
	SinexSolStatistic sst;

	sst.name		= what;
	sst.etype		= 1;
	sst.value.dval	= val;

	theSinex.liststatistics.push_back(sst);
}

int sinexCheckAddGaReference(string solType, string peaVer, bool isTrop)
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
		string	line = *it;

		if 	( line.find("DESCRIPTION")	!= string::npos
			||line.find("OUTPUT") 		!= string::npos
			||line.find("CONTACT") 		!= string::npos
			||line.find("SOFTWARE") 	!= string::npos
			||line.find("HARDWARE") 	!= string::npos
			||line.find("INPUT") 		!= string::npos)
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

void sinexAddComment(const string what)
{
	theSinex.blockComments["FILE/COMMENT"].push_back(what);
}

void sinexAddFiles(
	const string&			who,
	const GTime&			time,
	const vector<string>&	filenames,
	const string&			description)
{
	for (auto& filename : filenames)
	{
		SinexInputFile	sif;

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

	if (current		== zeros)	{	current = potential;	return;	}	//current is zero, just use the new version for the end time
	if (potential	== zeros)	{							return;	}	//potential time is zero, thats not restrictive, keep the current time
	if (potential	== current)	{	current = potential;	return;	}	//potential end time is more restrictive
}

GetSnxResult getRecSnx(
	string			id,
	GTime			time,
	SinexRecData&	recSnx)
{
	recSnx = SinexRecData();
	recSnx.start = time;

	GetSnxResult result;

	bool found = false;

	// search siteids for receiver (not time dependent)
	auto siteIdIt = theSinex.mapsiteids.find(id);
	if (siteIdIt != theSinex.mapsiteids.end())
	{
		auto& [dummy, siteId] = *siteIdIt;

		recSnx.id_ptr = &siteId;

		siteId.used = true;
	}
	else
	{
		result.failureSiteId = true;
	}

	auto receiverIt = theSinex.mapreceivers.find(id);
	if (receiverIt != theSinex.mapreceivers.end())
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

	auto antIt = theSinex.mapantennas.find(id);
	if (antIt != theSinex.mapantennas.end())
	{
		auto& [dummy, antTimeMap] = *antIt;

		auto antIt2 = theSinex.mapantennas[id].lower_bound(time);
		if (antIt2 != theSinex.mapantennas[id].end())
		{
			auto& [dummy, antenna] = *antIt2;

			found = true;
			antenna.used = true;

			recSnx.ant_ptr = &antenna;

			// get next next start time as end time for this aspect
			if (antIt2 != theSinex.mapantennas[id].begin())
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

	auto eccIt = theSinex.mapeccentricities.find(id);
	if (eccIt != theSinex.mapeccentricities.end())
	{
		auto& [dummy, eccMap] = *eccIt;

		auto eccIt2 = eccMap.lower_bound(time);
		if (eccIt2 != theSinex.mapeccentricities[id].end())
		{
			auto& [dummy, ecc] = *eccIt2;

			found = true;

			ecc.used = true;

			recSnx.ecc_ptr = &ecc;

			// get next next start time as end time for this aspect
			if (eccIt2 != theSinex.mapeccentricities[id].begin())
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

		auto& estMap = theSinex.estimatesMap[id][type];

		SinexSolEstimate* estimate_ptr = nullptr;

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
	for (auto& satPrn : theSinex.listsatprns)
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
	for (auto& satCom : theSinex.listsatcoms)
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
		result.failureCOM = true;

	found = false;

	// sat eccentricities
	for (auto& satEcc : theSinex.listsateccs)
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

void getSlrRecBias(
	string				id,
	string				prn,
	GTime				time,
	map<char, double>&	recBias)
{
	string ptcode;
	if (prn[1] == 'L')		ptcode = prn.substr(1, 2);	// Eugene to confirm prn only applied for 'L' sats for ptcode
	else					ptcode = "--";

	// Loop through "M" models codes - Ref: https://ilrs.gsfc.nasa.gov/docs/2024/ILRS_Data_Handling_File_2024.02.13.snx
	for (auto code : {'R', 'T', 'X', 'E', 'H', 'P', 'U', 'N', 'Q', 'V'})
	{
		auto it = theSinex.mapdatahandling[id][ptcode][code].lower_bound(time);
		if (it == theSinex.mapdatahandling[id][ptcode][code].end())
		{
			if (ptcode == "--")	// have already searched for "--" (prn not applied for ptcode)
			{
				continue;
			}

			it = theSinex.mapdatahandling[id]["--"][code].lower_bound(time);	// not found for prn number, furthur search for "--"
			if (it == theSinex.mapdatahandling[id]["--"][code].end())
			{
				continue;
			}
		}

		double unitsFactor = -1;
		auto& [dummy, dataHandling] = *it;

		GTime stopTime = dataHandling.epochend;

		if	( stopTime != GTime::noTime()
			&&stopTime < time)
		{
			continue;
		}

		switch (code)
		{
			case 'R': // Range bias to be applied, no estimation of bias
				if		(dataHandling.unit == "mm  ")			unitsFactor = 1e-3;
				break;
			case 'T': // Time bias in ms or µs & µs/d (T2L2) to be applied, NOT estimated
				if		(dataHandling.unit == "ms  ")			unitsFactor = 1e-3;
				else if (dataHandling.unit == "us  ")			unitsFactor = 1e-6;
				break;
			case 'E': // Estimation of range bias, known a priori values are given
				if		(dataHandling.unit == "mm  ")			unitsFactor = 1e-3;
				break;
			case 'H': // humidity error (correction in %)
				if		(dataHandling.unit == "%   ")			unitsFactor = 1e-3;
				break;
			case 'P': // pressure bias (correction in mB)
				if		(dataHandling.unit == "mB  ")			unitsFactor = 1;
				break;
			case 'U': // Estimation of time bias in ms
				if		(dataHandling.unit == "ms  ")			unitsFactor = 1e-3;
				else if (dataHandling.unit == "us  ")			unitsFactor = 1e-6;
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
				recBias[code] = true;	// recBias['X'] == recBias['N'] == recBias['Q'] == recBias['V'] == true
				continue;
		}

		if (unitsFactor < 0)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: unhandled units in " << __FUNCTION__ << ", model code " << code << " : " << dataHandling.unit;
			continue;
		}

		recBias[code] = dataHandling.estimate * unitsFactor;

		if	( code == 'T'
			&&dataHandling.comments == "drift")
		{
			GTime end	= dataHandling.epochend;
			GTime start	= dataHandling.epochstart;

			double interval = (end - start).to_double();
			GTime midInterval = start + interval / 2;

			double numDays = (time - midInterval).to_double() / S_IN_DAY;

			recBias[code] += dataHandling.estrate * numDays * 1e-6; // estrate units in us/day
		}

		continue;
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
