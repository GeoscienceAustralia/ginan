
// #pragma GCC optimize ("O0")

#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>
#include <iostream>
#include <assert.h>
#include <fstream>
#include <sstream>
#include <string>

#include "observations.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "common.hpp"
#include "sinex.hpp"
#include "slr.hpp"

using std::ifstream;
using std::ofstream;
using std::string;

using namespace boost::algorithm;

constexpr int MIN_LINE_LEN_SATID	= 106;
constexpr int MIN_LINE_LEN_SLROBS	= 162;

map<string, int>	cdpIdMap;

/** Read sat ID file
*/
void readSatId(
	string	filepath)	///< Filepath to sat ID file
{
	std::ifstream satIdFile(filepath);
	if (!satIdFile)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: could not read in satellite ID file " << filepath;
		return;
	}

	string line;
	while (std::getline(satIdFile, line))
	{
		if	( line.size() < MIN_LINE_LEN_SATID
			||line.at(0) == '#')
		{
			continue;
		}

		// Getting all the values from the file
		SatIdentity newSat;
		string satName		= line.substr( 1, 24);	boost::algorithm::trim(satName);
		newSat.satName 		= satName;
		string satId		= line.substr(25,  9);	boost::algorithm::trim(satId);
		newSat.satId		= satId;
		newSat.ilrsId		= std::stoi(line.substr(34,  9)); // todo: check input strings are compatible with stoi() and stod(), e.g. white spaces
		newSat.noradId		= std::stoi(line.substr(43,  9));
		newSat.altitude[0]	= std::stod(line.substr(52,  9));
		newSat.altitude[1]	= std::stod(line.substr(61,  9));
		newSat.inclination	= std::stod(line.substr(70, 18));
		string trackStatus	= line.substr(88, 18);	boost::algorithm::trim(trackStatus);
		newSat.tracking		= (trackStatus == "On" ? true : false);

		satIdMap[newSat.ilrsId] = newSat;
	}
}

/** Parses a CRD file
 */
vector<CrdSession> readCrdFile(
	string	filepath)	///< Filepath to CRD file
{
	vector<CrdSession> crdSessions; // One CrdSession per session (pass); May be multiple passes per file

	ifstream fileStream(filepath);
	if (!fileStream)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error opening crd file " << filepath << "\n";

		return crdSessions;
	}

	CrdSession crdSession;

	while (fileStream)
	{
		string line;

		getline(fileStream, line);

		char* str = &line[0];

		string recordType = str;
		recordType = recordType.substr(0,2);

		for (auto& c: recordType)
			c = toupper((unsigned char)c);

		if		(recordType ==	"H1") {	CrdH1	h1;		read_h1(str, &h1);	crdSession.h1			= h1;	crdSession.readH1 = true;	}
		else if	(recordType ==	"H2") {	CrdH2	h2;		read_h2(str, &h2);	crdSession.h2			= h2;	crdSession.readH2 = true;	}
		else if	(recordType ==	"H3") {	CrdH3	h3;		read_h3(str, &h3);	crdSession.h3			= h3;	crdSession.readH3 = true;	}
		else if	(recordType ==	"H4") {	CrdH4	h4;		read_h4(str, &h4);	crdSession.h4			= h4;	crdSession.readH4 = true;	}
		else if	(recordType ==	"H5") {	CrdH5	h5;		read_h5(str, &h5);	crdSession.h5			= h5;	crdSession.readH5 = true;	}
		else if	(recordType ==	"C0") {	CrdC0	c0;		read_c0(str, &c0);	crdSession.c0			= c0;	crdSession.readC0 = true;	}
		else if	(recordType ==	"C1") {	CrdC1	c1;		read_c1(str, &c1);	crdSession.c1			= c1;	crdSession.readC1 = true;	}
		else if	(recordType ==	"C2") {	CrdC2	c2;		read_c2(str, &c2);	crdSession.c2			= c2;	crdSession.readC2 = true;	}
		else if	(recordType ==	"C3") {	CrdC3	c3;		read_c3(str, &c3);	crdSession.c3			= c3;	crdSession.readC3 = true;	}
		else if	(recordType ==	"C4") {	CrdC4	c4;		read_c4(str, &c4);	crdSession.c4			= c4;	crdSession.readC4 = true;	}
		else if	(recordType ==	"C5") {	CrdC5	c5;		read_c5(str, &c5);	crdSession.c5			= c5;	crdSession.readC5 = true;	}
		else if	(recordType ==	"C6") {	CrdC6	c6;		read_c6(str, &c6);	crdSession.c6			= c6;	crdSession.readC6 = true;	}
		else if	(recordType ==	"C7") {	CrdC7	c7;		read_c7(str, &c7);	crdSession.c7			= c7;	crdSession.readC7 = true;	}
		else if	(recordType ==	"10") {	CrdD10	d10;	read_10(str, &d10);	crdSession.d10.	push_back(d10);								}
		else if	(recordType ==	"11") {	CrdD11	d11;	read_11(str, &d11);	crdSession.d11.	push_back(d11);								}
		else if	(recordType ==	"12") {	CrdD12	d12;	read_12(str, &d12);	crdSession.d12.	push_back(d12);								}
		else if	(recordType ==	"20") {	CrdD20	d20;	read_20(str, &d20);	crdSession.d20.	push_back(d20);								}
		else if	(recordType ==	"21") {	CrdD21	d21;	read_21(str, &d21);	crdSession.d21.	push_back(d21);								}
		else if	(recordType ==	"30") {	CrdD30	d30;	read_30(str, &d30);	crdSession.d30.	push_back(d30);								}
		else if	(recordType ==	"40") {	CrdD40	d40;	read_40(str, &d40);	crdSession.d40.	push_back(d40);								}
		else if	(recordType ==	"41") {	CrdD40	d41;	read_41(str, &d41);	crdSession.d41.	push_back(d41);								}
		else if	(recordType ==	"42") {	CrdD42	d42;	read_42(str, &d42);	crdSession.d42.	push_back(d42);								}
		else if	(recordType ==	"50") {	CrdD50	d50;	read_50(str, &d50);	crdSession.d50.	push_back(d50);								}
		else if	(recordType ==	"60") {	CrdD60	d60;	read_60(str, &d60);	crdSession.d60.	push_back(d60);								}
		else if	(recordType ==	"00") {	CrdD00	d00;	read_00(str, &d00);	crdSession.d00.	push_back(d00);								}
		else if	(recordType ==	"H8") {	crdSessions.push_back(crdSession);	crdSession = {};											} // End of session
		else if	(recordType ==	"H9") {																									} // End of file (ignore)
		else if	(recordType[0]=='9' ) {																									} // User-defined (ignore)
		else BOOST_LOG_TRIVIAL(warning) << "Warning: Unrecognised CRD recordType in file " << filepath << " - '" << recordType << "'";
	}

	for (auto it = crdSessions.begin(); it != crdSessions.end();  )
	{
		auto session = *it;
		if	( session.readH2 == false
			||session.readH3 == false
			||session.readH4 == false
			||session.readC0 == false
			||session.d20.empty()
			||session.d11.empty())
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: CRD file " << filepath << " has a session with insufficient data; ignoring data";
			it = crdSessions.erase(it);
		}
		else
			++it;
	}

	return crdSessions;
}

/** Converts from ILRS ID (condensed version of COSPAR ID) to COSPAR ID
COSPAR ID to ILRS Satellite Identification Algorithm:
COSPAR ID Format: (YYYY-XXXA)
	YYYY is the four-digit year of when the launch vehicle was put in orbit
	XXX is the sequential launch vehicle number for that year
	A is the alpha numeric sequence number within a launch
ILRS Satellite Identification Format: (YYXXXAA), based on the COSPAR ID
	YY is the two-digit year of when the launch vehicle was put in orbit
	XXX is the sequential launch vehicle number for that year
	AA is the numeric sequence number within a launch
*/
string ilrsIdToCosparId(
	int	ilrsId)	///< Filepath to
{
	string YYXXXAA = std::to_string(ilrsId);
	int YY	= stoi(YYXXXAA.substr(0,2));
	int XXX	= stoi(YYXXXAA.substr(2,3));
	int AA	= stoi(YYXXXAA.substr(5,2));
	double year = YY;
	nearestYear(year);
	int YYYY = (int)year;

	if (AA > 26)
		BOOST_LOG_TRIVIAL(error) << "Error converting IlrsId to CosparId - AA = " << AA;

	char A = 'A' + AA - 1;
	string XXXStr = std::to_string(XXX);
	string XXXStrZeroPad = string(3 - XXXStr.length(), '0') + XXXStr;
	string cosparId = std::to_string(YYYY) + '-' + XXXStrZeroPad + A;
	return cosparId;
}

/** Converts from COSPAR ID to ILRS ID (condensed version of COSPAR ID)
 */
int cosparIdToIlrsId(
	string	cosparId)	///< COSPAR ID to convert
{
	assert(cosparId.size() == 9);
	string YY	= cosparId.substr(2,2);
	string XXX	= cosparId.substr(5,3);
	char A		= cosparId.back();
	int AA = 'A' - A + 1;
	string AAStr = std::to_string(AA);
	string AAStrZeroPad = string(2 - AAStr.length(), '0') + AAStr;
	string ilrsId = YY + XXX + AAStrZeroPad;
	return stoi(ilrsId);
}

/** Convert seconds-of-day of an event and the start of the session encompassing the event to a GTime
 * Assumes session duration is <24hrs
 */
GTime sessionSod2Time(
	double	eventSod,				///< Seconds-of-day of the event in UTC time
	GTime	startSession)			///< session start time
{
	UYds startYds = startSession;

	double delta = eventSod - startYds.sod;

	if (delta > +secondsInDay / 2)	delta -= secondsInDay;
	if (delta < -secondsInDay / 2)	delta += secondsInDay;

	GTime recordTime = startSession + delta;

	return recordTime;
}

/** Converts from ILRS ID to SatSys object
*/
SatSys ilrsIdToSatSys(
	int	ilrsId)	///< ILRS ID
{
	SatSys Sat;
	auto it = satIdMap.find(ilrsId);
	if (it == satIdMap.end())
	{
		BOOST_LOG_TRIVIAL(error) << "Error - SatId not found in " << __FUNCTION__ << ": " << ilrsId;
		return Sat;
	}

	auto& [dummy, satData] = *it;

	string satId	= satData.satId;

	Sat = SatSys(satData.satId.c_str());

	return Sat;
}

/** Extracts observations from a given CRD (.npt) file
 */
void readCrd(
	string 	filepath)	///< CRD file to read
{
	// Read fields from CRD file
	vector<CrdSession> crdSessions = readCrdFile(filepath);

	// Extract relevant CRD data
	for (auto& crdSession : crdSessions)
	{
		LObs obsHeader;
		obsHeader.recName	= to_upper_copy((string)	crdSession.h2.stn_name);
		obsHeader.recCdpId	=							crdSession.h2.cdp_pad_id;

		obsHeader.satName	= to_lower_copy((string)	crdSession.h3.target_name);
		obsHeader.ilrsId	=							crdSession.h3.ilrs_id;
		obsHeader.cosparId		= ilrsIdToCosparId	(obsHeader.ilrsId);
		obsHeader.Sat			= ilrsIdToSatSys	(obsHeader.ilrsId);
		obsHeader.wavelengthNm	= crdSession.c0.xmit_wavelength;

		if	( crdSession.h4.refraction_app_ind		!= false
			||crdSession.h4.CofM_app_ind			!= false
// 			||crdSession.h4.xcv_amp_app_ind			!= false
			||crdSession.h4.stn_sysdelay_app_ind	!= true
			||crdSession.h4.SC_sysdelay_app_ind		!= false
			||crdSession.h4.range_type_ind			!= E_SlrRangeType::TWO_WAY)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: unexpected H4 flags!";

			obsHeader.excludeBadFlags = true;
		}

		if (crdSession.h4.data_qual_alert_ind != 0)
			obsHeader.excludeAlert = true;

		double startEp[6] =
		{
			crdSession.h4.start_year,
			crdSession.h4.start_mon,
			crdSession.h4.start_day,
			crdSession.h4.start_hour,
			crdSession.h4.start_min,
			crdSession.h4.start_sec
		};

		double endEp[6] =
		{
			crdSession.h4.end_year,
			crdSession.h4.end_mon,
			crdSession.h4.end_day,
			crdSession.h4.end_hour,
			crdSession.h4.end_min,
			crdSession.h4.end_sec
		};

		GTime startSession	= epoch2time(startEp,	E_TimeSys::UTC);
		GTime endSession	= epoch2time(endEp,		E_TimeSys::UTC);

		double sessionLength = (endSession - startSession).to_double();
		if (sessionLength > S_IN_DAY/2)
			BOOST_LOG_TRIVIAL(error) << "Error: CRD session spans more than 12hrs";

		for (auto& record : crdSession.d11)
		{
			LObs obs = obsHeader; // copy over header info
			GTime obsTime = sessionSod2Time(record.sec_of_day, startSession);

			// Find closest weather data entry
			double minDeltaSec = S_IN_DAY;
			for (auto& weather : crdSession.d20)
			{
				GTime weatherTime = sessionSod2Time(weather.sec_of_day, startSession);

				double deltaSec = fabs((obsTime - weatherTime).to_double());
				if (deltaSec < minDeltaSec)
				{
					minDeltaSec = deltaSec;
					obs.pressure	= weather.pressure;
					obs.temperature	= weather.temperature;
					obs.humidity	= weather.humidity / 100;
				}
			}

			// Calculate tx & bounce times
			obs.epochEvent = E_CrdEpochEvent::_from_integral(record.epoch_event);
			switch(obs.epochEvent)
			{
				case E_CrdEpochEvent::REC_TX:
					obs.timeTx.bigTime		= obsTime.bigTime;
					obs.twoWayTimeOfFlight	= record.time_of_flight;
					break;
				case E_CrdEpochEvent::REC_RX:
					obs.timeTx.bigTime		= obsTime.bigTime - record.time_of_flight;
					obs.twoWayTimeOfFlight	= record.time_of_flight;
					break;
				case E_CrdEpochEvent::SAT_BN:
					obs.timeTx.bigTime		= obsTime.bigTime - record.time_of_flight / 2;
					obs.twoWayTimeOfFlight	= record.time_of_flight;
				case E_CrdEpochEvent::SAT_RX:
				case E_CrdEpochEvent::SAT_TX:
				case E_CrdEpochEvent::REC_TX_SAT_RX:
				case E_CrdEpochEvent::SAT_TX_REC_RX:
				default:
					BOOST_LOG_TRIVIAL(warning)
					<< "Warning: Unexpected epoch event found: " << obs.epochEvent << ", discarding";
					continue;
			}

			slrSiteObsMap[obs.recName][obs.timeTx] = (shared_ptr<LObs>)obs;
		}
	}
}

/** Outputs a tabular file with SLR observations in time-order, for 1 receiver
*/
void outputSortedSlrObsPerRec(
	string							filepath,	///< slr_obs file to write
	map<GTime, shared_ptr<LObs>>&	obsMap)		///< Output observation list
{
	ofstream 	filestream(filepath);
	if (!filestream)
		return;

	tracepdeex(0, filestream, "%21s  %-7s %7s %27s  %-11s %8s %4s %18s %13s %12s %9s %12s\n",
			"DateTime",
			"Site_ID",
			"CDP_ID",
			"BigTime",
			"Sat_Name",
			"ILRS_ID",
			"PRN",
			"2-Way_Measurement",
			"Pressure",
			"Temperature",
			"Humidity",
			"Wave_Length");
	tracepdeex(0, filestream, "%21s  %-7s %7s %27s  %-11s %8s %4s %18s %13s %12s %9s %12s\n",
			" ",
			" ",
			" ",
			"[s (GPS)]",
			" ",
			" ",
			" ",
			"[s]",
			"[hPa (mbar)]",
			"[K]",
			"[%]",
			"[nm]");

	for (auto& [time, slrObs_ptr] : obsMap)
	{
		auto& obs = *slrObs_ptr;

		tracepdeex(0, filestream, "%21s  %-7s %7d %27.12lf  %-11s %8d %4s %18.12f %13.2f %12.2f %9.1f %12.3f\n",
			obs.timeTx.to_string(1),
			obs.recName,
			obs.recCdpId,
			obs.timeTx.bigTime,
			obs.satName,
			obs.ilrsId,
			obs.Sat.id().c_str(),
			obs.twoWayTimeOfFlight,
			obs.pressure,
			obs.temperature,
			obs.humidity * 100,
			obs.wavelengthNm);
	}
}

/** Outputs a tabular file with SLR observations in time-order
*/
map<string, vector<string>> outputSortedSlrObs()
{
	map<string, vector<string>> slrObsFiles;

	for (auto& [recId, slrObsList] : slrSiteObsMap)
	{
		string filename	= acsConfig.slr_obs_filename;

		replaceString(filename, "<RECEIVER>", recId);

		outputSortedSlrObsPerRec(filename, slrObsList);

		slrObsFiles[recId].push_back(filename);
	}

	return slrObsFiles;
}

/** Read one obs record from tabular slr obs file
*/
int readSlrObs(
	std::istream&	inputStream,	///< File input stream
	ObsList&		obsList)		///< List of SLR observations
{
	string line;

	// read slr obs file header if at beginning of file
	if (inputStream.tellg() == 0)
	{
		std::getline(inputStream, line);
		std::getline(inputStream, line);
	}

	std::getline(inputStream, line);

	if (line.empty())
		return 0;

	if (line.size() < MIN_LINE_LEN_SLROBS)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: incorrect SLR obs file format, line skipped";
		return 0;
	}

	LObs obs;
	obs.recName				= 		line.substr(23,		4);
	obs.recCdpId			= stoi(	line.substr(31,		7));
	obs.timeTx.bigTime		= stod(	line.substr(39,		27));
	obs.satName				= 		line.substr(68,		11);
	obs.ilrsId				= stoi(	line.substr(80,		8));
	obs.twoWayTimeOfFlight	= stod(	line.substr(94,		18));
	obs.pressure			= stod(	line.substr(113,	13));
	obs.temperature			= stod(	line.substr(127,	12));
	obs.humidity			= stod(	line.substr(140,	9)) / 100;
	obs.wavelengthNm		= stod(	line.substr(150,	12));

	obs.cosparId			= ilrsIdToCosparId	(obs.ilrsId);
	obs.Sat					= ilrsIdToSatSys	(obs.ilrsId);
	obs.time				= obs.timeTx + obs.twoWayTimeOfFlight / 2;

	cdpIdMap[obs.recName]	= obs.recCdpId;

	obsList.push_back((shared_ptr<LObs>)obs);

	return 1;
}
