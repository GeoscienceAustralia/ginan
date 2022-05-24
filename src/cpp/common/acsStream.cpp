
// #pragma GCC optimize ("O0")
#include <map>

using std::multimap;
using std::map;

#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/json.hpp>

using bsoncxx::builder::basic::kvp;

#include "rtcmEncoder.hpp"
#include "navigation.hpp"
#include "streamRtcm.hpp"
#include "ephemeris.hpp"
#include "constants.hpp"
#include "acsStream.hpp"
#include "acsConfig.hpp"
#include "fileLog.hpp"
#include "enums.h"


GTime RtcmStream::rtcmDeltaTime = {};


// Used for stream specific tracing.
multimap<string, std::shared_ptr<NtripRtcmStream>>	ntripRtcmMultimap;
multimap<string, ACSObsStreamPtr>					obsStreamMultimap;
multimap<string, ACSNavStreamPtr>					navStreamMultimap;
multimap<string, ACSPseudoObsStreamPtr>				pseudoObsStreamMultimap;
map		<string, bool>								streamDOAMap;


ObsList ObsStream::getObs()
{
	if (obsListList.empty())
	{
		return ObsList();
	}
	ObsList& obsList = obsListList.front();

	std::sort(obsList.begin(), obsList.end(), [](Obs& a, Obs& b)
		{
			if (a.Sat.sys < b.Sat.sys)		return true;
			if (a.Sat.sys > b.Sat.sys)		return false;
			if (a.Sat.prn < b.Sat.prn)		return true;
			else							return false;
		});

	for (auto& obs					: obsList)
	for (auto& [ftype, sigsList]	: obs.SigsLists)
	{
		
		if (obs.Sat.sys == +E_Sys::GPS)
		{
			double dirty_C1W_phase = 0;
			for(auto& sig : sigsList)
			if ( sig.code == +E_ObsCode::L1C)
			{
				dirty_C1W_phase = sig.L;
				break;
			}
		
			for(auto& sig : sigsList)
			if	(  sig.code	== +E_ObsCode::L1W 
				&& sig.L	== 0)
			{
				sig.L = dirty_C1W_phase;
				break;
			}
		}
		
		sigsList.sort([](RawSig& a, RawSig& b)
			{
				auto iterA = std::find(acsConfig.code_priorities.begin(), acsConfig.code_priorities.end(), a.code);
				auto iterB = std::find(acsConfig.code_priorities.begin(), acsConfig.code_priorities.end(), b.code);

				if (a.L == 0)		return false;
				if (b.L == 0)		return true;
				if (a.P == 0)		return false;
				if (b.P == 0)		return true;
				if (iterA < iterB)	return true;
				else				return false;
			});

		if (sigsList.empty())
		{
			continue;
		}
		
		RawSig firstOfType = sigsList.front();

		//use first of type as representative if its in the priority list
		auto iter = std::find(acsConfig.code_priorities.begin(), acsConfig.code_priorities.end(), firstOfType.code);
		if (iter != acsConfig.code_priorities.end())
		{
			obs.Sigs[ftype] = Sig(firstOfType);
		}
	}

	return obsList;
}


PseudoObsList PseudoObsStream::getObs()
{
	if (obsListList.empty())
	{
		return PseudoObsList();
	}
	
	PseudoObsList& pseudoObsList = obsListList.front();

	return pseudoObsList;
}


void RtcmStream::createRtcmFile()
{
	GTime curTime;
	time(&curTime.time);
	long int roundTime = curTime.time;
	roundTime /= acsConfig.rtcm_rotate_period;
	roundTime *= acsConfig.rtcm_rotate_period;
	curTime.time = roundTime;

	string logtime = curTime.to_string(0);
	std::replace( logtime.begin(), logtime.end(), '/', '-');

	string path_rtcm = rtcm_filename;
	replaceString(path_rtcm, "<LOGTIME>", logtime);

	std::ofstream ofs( path_rtcm,std::ofstream::out | std::ofstream::ate);
}




/** Calculates average of given vector
*/
double	calcAve(vector<double> vec)
{
	if (vec.empty())
	{
		return 0;
	}
	
	double accum = 0;
	for (auto val : vec)
	{
		accum += val;
	}
	return accum / vec.size();
}

/** Encodes nav.satNavMap[].ssrOut to file containing RTCM messages
*/
void rtcmEncodeToFile()
{
	string filename = acsConfig.ssrOpts.rtcm_directory + "rtcmDataEpoch.dat";
	
	std::ofstream ofRtcmSsr(filename, std::ios::out | std::ios::binary);
	if (!ofRtcmSsr)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not open file for exporting SSR corrections at " << filename;
		return;
	}
	
	RtcmEncoder rtcmSsrEnc;
	
// 	SsrCombMap	ssrCombMap;
// 	SsrEphMap	ssrEphMap;
// 	SsrClkMap 	ssrClkMap;
// 	ssrEphMap  = collateSsrEphem (E_Sys::GPS, true);
// 	ssrClkMap  = collateSsrClocks(E_Sys::GPS, true);
// 	ssrCombMap = collateSsrComb(ssrEphMap, ssrClkMap);
// 	rtcmSsrEnc.encodeSsrComb(ssrCombMap);
// 	
// 	ssrEphMap  = collateSsrEphem (E_Sys::GAL, true);
// 	ssrClkMap  = collateSsrClocks(E_Sys::GAL, true);
// 	ssrCombMap = collateSsrComb(ssrEphMap, ssrClkMap);
// 	rtcmSsrEnc.encodeSsrComb(ssrCombMap);
	
// 	SsrCBMap ssrCBMap;
// 	ssrCBMap = collateSsrCode(E_Sys::GPS, true);
// 	rtcmSsrEnc.encodeSsrCode(ssrCBMap);
// 	
// 	ssrCBMap = collateSsrCode(E_Sys::GAL, true);
// 	rtcmSsrEnc.encodeSsrCode(ssrCBMap);	
// 	
// 	SsrPBMap ssrPBMap;
// 	ssrPBMap = collateSsrPhase(E_Sys::GPS, true);
// 	rtcmSsrEnc.encodeSsrPhase(ssrPBMap);
// 	
// 	ssrPBMap = collateSsrPhase(E_Sys::GAL, true);
//     rtcmSsrEnc.encodeSsrPhase(ssrPBMap);	
// 	
// 	rtcmSsrEnc.encodeWriteMessages(ofRtcmSsr);
}


/** Writes nav.satNavMap[].ssrOut to a human-readable file
*/
void	writeSsrOutToFile(
	int						epochNum,
	map<SatSys, SSROut>&	ssrOutMap)
{
	string filename = "ssrOut.dbg";
	std::ofstream out(filename, std::ios::app);

	if (!out)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Could not open trace file for SSR messages at " << filename;
		return;
	}
	out.precision(17);

	// Header
	out << "epochNum" 			<< "\t";
	out << "satId" 				<< "\t";
	
// 	out << "SSREph.canExport"	<< "\t";
	out << "SSREph.t0"			<< "\t";
	out << "SSREph.udi"			<< "\t";
	out << "SSREph.iod"			<< "\t";
	out << "SSREph.iode"		<< "\t";
	out << "SSREph.deph[0]"		<< "\t";
	out << "SSREph.deph[1]"		<< "\t";
	out << "SSREph.deph[2]"		<< "\t";
	out << "SSREph.ddeph[0]"	<< "\t";
	out << "SSREph.ddeph[1]"	<< "\t";
	out << "SSREph.ddeph[2]"	<< "\t";

// 	out << "SSRClk.canExport"	<< "\t";
	out << "SSRClk.t0"			<< "\t";
	out << "SSRClk.udi"			<< "\t";
	out << "SSRClk.iod"			<< "\t";
	out << "SSRClk.dclk[0]"		<< "\t";
	out << "SSRClk.dclk[1]"		<< "\t";
	out << "SSRClk.dclk[2]"		<< "\t";
	
	out << "SSRBias.t0_code"	<< "\t";
	out << "SSRBias.t0_phas"	<< "\t";
	out << "SSRBias.udi_code"	<< "\t";
	out << "SSRBias.udi_phas"	<< "\t";
	out << "SSRBias.iod_code"	<< "\t";
	out << "SSRBias.iod_phas"	<< "\t";
	for (int i=0; i<2; ++i)	out << "ssrBias.cbias_"<< i << "\t";
	for (int i=0; i<2; ++i)	out << "ssrBias.cvari_"<< i << "\t";
	for (int i=0; i<2; ++i)	out << "ssrBias.pbias_"<< i << "\t";
	for (int i=0; i<2; ++i)	out << "ssrBias.pvari_"<< i << "\t";
	for (int i=0; i<2; ++i)
	{
		out << "ssrBias.ssrPhaseCh.signalIntInd_"		<< i <<	"\t";
		out << "ssrBias.ssrPhaseCh.signalWidIntInd_"	<< i <<	"\t";
		out << "ssrBias.ssrPhaseCh.signalDisconCnt_"	<< i << "\t";
	}
	out << std::endl;

	// Body
	for (auto& [Sat, ssrOut] : ssrOutMap)
	{
		out << epochNum << "\t";
		out << Sat.id() << "\t";
// 		out << ssrOut.ssrEph.canExport<< "\t";
		out << ssrOut.ssrEph.t0			<< "\t";
		out << ssrOut.ssrEph.udi		<< "\t";
		out << ssrOut.ssrEph.iod		<< "\t";
		out << ssrOut.ssrEph.iode		<< "\t";
		out << ssrOut.ssrEph.deph[0]	<< "\t";
		out << ssrOut.ssrEph.deph[1]	<< "\t";
		out << ssrOut.ssrEph.deph[2]	<< "\t";
		out << ssrOut.ssrEph.ddeph[0]	<< "\t";
		out << ssrOut.ssrEph.ddeph[1]	<< "\t";
		out << ssrOut.ssrEph.ddeph[2]	<< "\t";

// 		out << ssrOut.ssrClk.canExport<< "\t";
		out << ssrOut.ssrClk.t0			<< "\t";
		out << ssrOut.ssrClk.udi		<< "\t";
		out << ssrOut.ssrClk.iod		<< "\t";
		out << ssrOut.ssrClk.dclk[0]	<< "\t";
		out << ssrOut.ssrClk.dclk[1]	<< "\t";
		out << ssrOut.ssrClk.dclk[2]	<< "\t";
		
		out << ssrOut.ssrCodeBias.t0			<< "\t";
		out << ssrOut.ssrPhasBias.t0			<< "\t";
		out << ssrOut.ssrCodeBias.udi			<< "\t";
		out << ssrOut.ssrPhasBias.udi			<< "\t";
		out << ssrOut.ssrCodeBias.iod			<< "\t";
		out << ssrOut.ssrPhasBias.iod			<< "\t";
		for (auto& [key, val] : ssrOut.ssrCodeBias.obsCodeBiasMap)	out << val.bias << "\t" << val.var << "\t";
		for (auto& [key, val] : ssrOut.ssrPhasBias.obsCodeBiasMap)	out << val.bias << "\t" << val.var << "\t";

		for (auto& [key, ssrPhaseCh] : ssrOut.ssrPhasBias.ssrPhaseChs)
		{
			out << ssrPhaseCh.signalIntInd		<< "\t";
			out << ssrPhaseCh.signalWidIntInd	<< "\t";
			out << ssrPhaseCh.signalDisconCnt	<< "\t";
		}
		out << std::endl;
	}
	out << std::endl;
}

void NtripRtcmStream::connectionError(const boost::system::error_code& err, std::string operation)
{
	if (acsConfig.output_log == false)
		return;

	std::ofstream logStream(acsConfig.log_filename, std::ofstream::app);

	if (!logStream)
	{
		BOOST_LOG_TRIVIAL(warning) << "Error opening log file.\n";
		return;
	}
	
	auto time = std::chrono::system_clock::to_time_t(system_clock::now());

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("label", 			"connectionError"));
	doc.append(kvp("Stream", 			url.path.substr(1,url.path.length())));
// 	doc.append(kvp("Time", 				time));
	doc.append(kvp("BoostSysErrCode",	err.value()));
	doc.append(kvp("BoostSysErrMess",	err.message()));
	doc.append(kvp("SocketOperation",	operation));
	
	logStream << bsoncxx::to_json(doc) << std::endl;
}

void NtripRtcmStream::serverResponse(
	unsigned int	status_code,
	string			http_version)
{
	if (acsConfig.output_log == false)
		return;

	std::ofstream logStream(FileLog::path_log, std::ofstream::app);
	
	if (!logStream)
	{
		BOOST_LOG_TRIVIAL(warning) << "Error opening log file.\n";
		return;
	}

	auto time = std::chrono::system_clock::to_time_t(system_clock::now());

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("label", 		"serverResponse"));
	doc.append(kvp("Stream", 		url.path.substr(1,url.path.length())));
// 	doc.append(kvp("Time", 			time));
	doc.append(kvp("ServerStatus", 	(int)status_code));
	doc.append(kvp("VersionHTTP",	http_version));
	
	logStream << bsoncxx::to_json(doc) << std::endl;
}

void recordNetworkStatistics(
	std::multimap<string, std::shared_ptr<NtripRtcmStream>> downloadStreamMap)
{
	for (auto& [id, s] : downloadStreamMap )
	{
		auto& downStream = *s;

// 		std::cout << downStream.getNetworkStatistics(tsync, id) << std::endl;	//todo aaron useless
	}
}

void writeNetworkTraces(
	StationMap& stationMap)
{
	// Observations or not provide trace information on the downloading station stream.

	for (auto& [id, rec] : stationMap)
	{
		auto down_it = ntripRtcmMultimap.find(rec.id);
		if (down_it == ntripRtcmMultimap.end())
		{
			continue;
		}
	
// 		auto trace = getTraceFile(rec);
// 		
// 		trace << std::endl << "<<<<<<<<<<< Network Trace : Epoch " << epoch << " >>>>>>>>>>>" << std::endl;
// 
// 		auto& [dummyId, downStream] = *down_it;
	}

	recordNetworkStatistics(ntripRtcmMultimap);
}
