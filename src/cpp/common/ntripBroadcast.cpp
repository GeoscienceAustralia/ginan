
// #pragma GCC optimize ("O0")

#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/json.hpp>

#include <chrono>

using std::chrono::system_clock;
using std::chrono::time_point;

using bsoncxx::builder::basic::kvp;

#include "ntripBroadcast.hpp"
#include "mongoRead.hpp"
#include "otherSSR.hpp"
#include "fileLog.hpp"
#include "gTime.hpp"


NtripBroadcaster ntripBroadcaster;

namespace bp = boost::asio::placeholders;


using boost::posix_time::ptime;
using boost::posix_time::time_duration;

void debugSSR(GTime t0, GTime targetTime, E_Sys sys, SsrOutMap& ssrOutMap);

void NtripBroadcaster::startBroadcast()
{
	TcpSocket::startClients();
}

void NtripBroadcaster::stopBroadcast()
{
	for (auto [label, outStream] : ntripUploadStreams)
	{
		outStream->disconnect();
	}

	ntripUploadStreams.clear();
}

void NtripUploader::serverResponse(
	unsigned int	status_code,
	string 			http_version)
{
	if (acsConfig.output_ntrip_log == false)
		return;

	std::ofstream logStream(networkTraceFilename, std::ofstream::app);

	if (!logStream)
	{
		BOOST_LOG_TRIVIAL(warning) << "Error opening log file.\n";
		return;
	}

	GTime time = timeGet();

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("label", 			__FUNCTION__));
	doc.append(kvp("Stream", 			url.path.substr(1, url.path.length())));
	doc.append(kvp("Time", 				time.to_string()));
	doc.append(kvp("ServerStatus", 		(int)status_code));
	doc.append(kvp("VersionHTTP",		http_version));

	logStream << bsoncxx::to_json(doc) << "\n";
}


void NtripUploader::writeHandler(
	const boost::system::error_code& err)
{
	if (err)
	{
		outMessages.consume(outMessages.size());
		outMessagesMtx.unlock();

		ERROR_OUTPUT_RECONNECT_AND_RETURN;
	}

	onChunkSentStatistics();

	outMessagesMtx.unlock();
}

void NtripUploader::messageTimeoutHandler(
	const boost::system::error_code& err)
{
	// BOOST_LOG_TRIVIAL(debug) << "started " << __FUNCTION__ << "\n";
	if (err)
	{
		ERROR_OUTPUT_RECONNECT_AND_RETURN;
	}

	//fire this callback again in the future
	{
		sendTimer.expires_from_now(boost::posix_time::milliseconds(500));		// check uploader twice a second to account for aliasing
		sendTimer.async_wait(boost::bind(&NtripUploader::messageTimeoutHandler, this, bp::error));
	}

	SSRMeta		ssrMeta;
	SsrOutMap	ssrOutMap;

	GTime	latestTime = timeGet();

	if (latestTime == GTime::noTime())
		return;

	GTime	targetTime = latestTime.floorTime(1);

	if (targetTime == previousTargetTime)
	{
		//already did this epoch
		return;
	}

	BOOST_LOG_TRIVIAL(debug) << "SSR OUT Targeting epoch: " << targetTime.to_string() << "\n";

	ssrMeta.receivedTime		= targetTime;	// for rtcmTrace (debugging)
	ssrMeta.multipleMessage 	= 1; // We assume there will be more messages.

	if (streamConfig.itrf_datum)		ssrMeta.referenceDatum 	=  0; // Orbit corrections, 0 - ITRF, 1 - Regional.
	else								ssrMeta.referenceDatum 	=  1;

	ssrMeta.provider			= streamConfig.provider_id;
	ssrMeta.solution			= streamConfig.solution_id;

	for (auto [messCode, msgOpts] : streamConfig.rtcmMsgOptsMap)
	{
		int updateInterval = msgOpts.udi;

		if	(  messCode == +RtcmMessageType::IGS_SSR
			|| messCode == +RtcmMessageType::COMPACT_SSR)
		{
			updateInterval = 1;
		}

		if	( updateInterval == 0
			||((long int)targetTime) % updateInterval != 0)
		{
			continue;
		}

		int udiIndex = getUdiIndex(updateInterval);

		ssrMeta.updateIntIndex	= udiIndex;

		if (ssrMeta.updateIntIndex == -1)
			BOOST_LOG_TRIVIAL(error) << "Error: ssrMeta.updateIntIndex is not valid :" << ssrMeta.updateIntIndex << ").";

		if (messCode == streamConfig.rtcmMsgOptsMap.rbegin()->first)
			ssrMeta.multipleMessage = 0;

		E_Sys sys = rtcmMessageSystemMap[messCode];

		if (sys == +E_Sys::NONE)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: invalid message code system :" << messCode;
			continue;
		}

		if		(sys == +E_Sys::GLO)	{	RTod tod = targetTime;			ssrMeta.epochTime1s		= (int)tod;		}
		else if	(sys == +E_Sys::BDS)	{	BTow tow = targetTime;			ssrMeta.epochTime1s		= (int)tow;		}
		else							{	GTow tow = targetTime;			ssrMeta.epochTime1s		= (int)tow;		}

		GTime t0;
		if (ssrMeta.updateIntIndex == 0)	t0 = targetTime;
		else								t0 = targetTime + updateInterval / 2.0;

		BOOST_LOG_TRIVIAL(debug) << "SSR message type: " << messCode._to_string() << ", udi: " << updateInterval;
		switch (messCode)
		{
			case +RtcmMessageType::GPS_SSR_PHASE_BIAS:
			case +RtcmMessageType::GLO_SSR_PHASE_BIAS:
			case +RtcmMessageType::GAL_SSR_PHASE_BIAS:
			case +RtcmMessageType::QZS_SSR_PHASE_BIAS:
			case +RtcmMessageType::BDS_SSR_PHASE_BIAS:
			case +RtcmMessageType::SBS_SSR_PHASE_BIAS:
			{
				auto ssrPBMap = mongoReadPhaseBias(ssrMeta, masterIod, sys);

				auto buffer = encodeSsrPhase(ssrPBMap, messCode);
				bool write = encodeWriteMessageToBuffer(buffer);

				if (write == false)
				{
					std::cout << "RtcmMessageType::" << messCode._to_string() << " was not written" << "\n";
				}

				break;
			}
			case +RtcmMessageType::GPS_SSR_CODE_BIAS:
			case +RtcmMessageType::GLO_SSR_CODE_BIAS:
			case +RtcmMessageType::GAL_SSR_CODE_BIAS:
			case +RtcmMessageType::QZS_SSR_CODE_BIAS:
			case +RtcmMessageType::BDS_SSR_CODE_BIAS:
			case +RtcmMessageType::SBS_SSR_CODE_BIAS:
			{
				auto ssrCBMap = mongoReadCodeBias(ssrMeta, masterIod, sys);

				auto buffer = encodeSsrCode(ssrCBMap, messCode);
				bool write = encodeWriteMessageToBuffer(buffer);

				if (write == false)
				{
					std::cout << "RtcmMessageType::" << messCode._to_string() << " was not written" << "\n";
				}

				break;
			}
			case +RtcmMessageType::GPS_SSR_COMB_CORR:
			case +RtcmMessageType::GLO_SSR_COMB_CORR:
			case +RtcmMessageType::GAL_SSR_COMB_CORR:
			case +RtcmMessageType::QZS_SSR_COMB_CORR:
			case +RtcmMessageType::BDS_SSR_COMB_CORR:
			case +RtcmMessageType::SBS_SSR_COMB_CORR:
			case +RtcmMessageType::GPS_SSR_ORB_CORR:
			case +RtcmMessageType::GLO_SSR_ORB_CORR:
			case +RtcmMessageType::GAL_SSR_ORB_CORR:
			case +RtcmMessageType::QZS_SSR_ORB_CORR:
			case +RtcmMessageType::BDS_SSR_ORB_CORR:
			case +RtcmMessageType::SBS_SSR_ORB_CORR:
			case +RtcmMessageType::GPS_SSR_CLK_CORR:
			case +RtcmMessageType::GLO_SSR_CLK_CORR:
			case +RtcmMessageType::GAL_SSR_CLK_CORR:
			case +RtcmMessageType::QZS_SSR_CLK_CORR:
			case +RtcmMessageType::BDS_SSR_CLK_CORR:
			case +RtcmMessageType::SBS_SSR_CLK_CORR:
			case +RtcmMessageType::GPS_SSR_HR_CLK_CORR:
			case +RtcmMessageType::GLO_SSR_HR_CLK_CORR:
			case +RtcmMessageType::GAL_SSR_HR_CLK_CORR:
			case +RtcmMessageType::QZS_SSR_HR_CLK_CORR:
			case +RtcmMessageType::BDS_SSR_HR_CLK_CORR:
			case +RtcmMessageType::SBS_SSR_HR_CLK_CORR:
			{
				ssrOutMap 	= mongoReadOrbClk(t0, ssrMeta, masterIod, sys);

				calculateSsrComb(t0, updateInterval, ssrMeta, masterIod, ssrOutMap);

				auto buffer = encodeSsrOrbClk(ssrOutMap, messCode);
				bool write = encodeWriteMessageToBuffer(buffer);

				if (traceLevel > 5)
				{
// 					debugSSR(t0, targetTime, sys, ssrOutMap);

					if (write == false)
					{
						std::cout << "RtcmMessageType::" << messCode._to_string() << " was not written" << "\n";
					}
				}

				break;
			}
			case +RtcmMessageType::GPS_SSR_URA:
			case +RtcmMessageType::GLO_SSR_URA:
			case +RtcmMessageType::GAL_SSR_URA:
			case +RtcmMessageType::QZS_SSR_URA:
			case +RtcmMessageType::BDS_SSR_URA:
			case +RtcmMessageType::SBS_SSR_URA:
			{
				auto buffer = encodeSsrUra(ssrOutMap, messCode);
				bool write = encodeWriteMessageToBuffer(buffer);

				if (write == false)
				{
					std::cout << "RtcmMessageType::" << messCode._to_string() << " was not written" << "\n";
				}

				break;
			}
			case +RtcmMessageType::GPS_EPHEMERIS:
			case +RtcmMessageType::BDS_EPHEMERIS:
			case +RtcmMessageType::QZS_EPHEMERIS:
			case +RtcmMessageType::GAL_FNAV_EPHEMERIS:
			case +RtcmMessageType::GAL_INAV_EPHEMERIS:
			{
				bool write = false;

				for (auto& sat : getSysSats(sys))
				{
					auto eph = mongoReadEphemeris(targetTime, sat, messCode);

					if (eph.toe == GTime::noTime())
						continue;

					auto buffer = encodeEphemeris(eph, messCode);
					write |= encodeWriteMessageToBuffer(buffer);
				}

				if (write == false)
				{
					std::cout << "RtcmMessageType::" << messCode._to_string() << " was not written" << "\n";
				}

				break;
			}
			case +RtcmMessageType::GLO_EPHEMERIS:
			{
				bool write = false;

				for (auto& sat : getSysSats(sys))
				{
					auto geph = mongoReadGloEphemeris(targetTime, sat);

					if (geph.toe == GTime::noTime())
						continue;

					auto buffer = encodeEphemeris(geph, messCode);
					write |= encodeWriteMessageToBuffer(buffer);
				}

				if (write == false)
				{
					std::cout << "RtcmMessageType::" << messCode._to_string() << " was not written" << "\n";
				}

				break;
			}

			case +RtcmMessageType::IGS_SSR:
			{
				SSRAtm							ssrAtm;
				map<E_Sys, map<SatSys, SSROut>> ssrOutMaps;
				map<E_Sys, SsrCBMap>			ssrCodMaps;
				map<E_Sys, SsrPBMap>			ssrPhsMaps;
				map<E_Sys, SsrUraMap>			ssrUraMaps;

				IgsSSRSubtype					lastSubType = IgsSSRSubtype::NONE;
				map<IgsSSRSubtype, bool>		approvedMessages;

				BOOST_LOG_TRIVIAL(debug) << "IGS SSR: ";

				E_Sys sys;
				for (auto [subType, subUdi] : msgOpts.igs_udi)
				{
					BOOST_LOG_TRIVIAL(debug) << "message type: " << subType._to_string() << ", udi: " << subUdi;

					if	(  subUdi == 0
						|| ((long int)targetTime) % subUdi != 0)
					{
						continue;
					}
					int subUdiIndex = getUdiIndex(subUdi);
					ssrMeta.updateIntIndex	= subUdiIndex;
					if (ssrMeta.updateIntIndex == 0)	t0 = targetTime;
					else								t0 = targetTime + subUdi/2.0;

					auto group = IGS_SSR_group(subType, sys);
					switch (group)
					{
						case 1:
						case 2:
						case 3:
						case 4:
						{
							auto sysOutMap = mongoReadOrbClk(t0, ssrMeta, masterIod, sys);

							calculateSsrComb(t0, subUdi, ssrMeta, masterIod, sysOutMap);

							if (sysOutMap.empty() == false)
							{
								lastSubType					= subType;
								ssrOutMaps[sys]				= sysOutMap;
								approvedMessages[subType]	= true;
							}
							break;
						}
						case 5:
						{
							auto sysCBMap  = mongoReadCodeBias(ssrMeta, masterIod, sys);

							if (sysCBMap.empty() == false)
							{
								lastSubType					= subType;
								ssrCodMaps[sys]				= sysCBMap;
								approvedMessages[subType]	= true;
							}
							break;
						}
						case 6:
						{
							auto sysPBMap  = mongoReadPhaseBias(ssrMeta, masterIod, sys);

							if (sysPBMap.empty() == false)
							{
								lastSubType					= subType;
								ssrPhsMaps[sys]				= sysPBMap;
								approvedMessages[subType]	= true;
							}
							break;
						}
						case 7:
						{
							// auto sysUraMap = mongoReadUra(t0, ssrMeta, masterIod, sys);	// Eugene: use sysOutMap?
							// if (sysUraMap.empty() == false)
							// {
							// 	lastSubType					= subType;
							// 	ssrUraMaps[sys]				= sysUraMap;
							// 	approvedMessages[subType]	= true;
							// }
							break;
						}
						case 8:
						{
							ssrAtm = mongoReadIGSIonosphere(targetTime, ssrMeta, masterIod);

							if (ssrAtm.atmosGlobalMap.empty() == false)
							{
								lastSubType					= subType;
								approvedMessages[subType]	= true;
							}
							break;
						}
					}
				}

				for (auto [subType, approved] : approvedMessages)
				{
					bool last = (lastSubType == subType);

					switch (IGS_SSR_group(subType, sys))
					{
						case 1: {auto buffer = encodeIGS_ORB(ssrOutMaps[sys], sys,	last);		encodeWriteMessageToBuffer(buffer);		break;}
						case 2: {auto buffer = encodeIGS_CLK(ssrOutMaps[sys], sys,	last);		encodeWriteMessageToBuffer(buffer);		break;}
						case 3: {auto buffer = encodeIGS_CMB(ssrOutMaps[sys], sys,	last);		encodeWriteMessageToBuffer(buffer);		break;}
						case 4: {auto buffer = encodeIGS_HRC(ssrOutMaps[sys], sys,	last);		encodeWriteMessageToBuffer(buffer);		break;}
						case 5: {auto buffer = encodeIGS_COD(ssrCodMaps[sys], sys,	last);		encodeWriteMessageToBuffer(buffer);		break;}
						case 6: {auto buffer = encodeIGS_PHS(ssrPhsMaps[sys], sys,	last);		encodeWriteMessageToBuffer(buffer);		break;}
						case 7: {auto buffer = encodeIGS_URA(ssrUraMaps[sys], sys,	last);		encodeWriteMessageToBuffer(buffer);		break;}
						case 8: {auto buffer = encodeIGS_ATM(ssrAtm,				last);		encodeWriteMessageToBuffer(buffer);		break;}
					}
				}
				break;
			}

			case +RtcmMessageType::COMPACT_SSR:
			{
				SSRAtm							ssrAtm;
				map<SatSys, SSROut> 			ssrOutMap;
				map<SatSys, SSRCodeBias>		ssrCodMap;
				map<SatSys, SSRPhasBias>		ssrPhsMap;

				map<CompactSSRSubtype, int>	approvedMessages;

				bool new_mask = false;
				for (auto [subType, subUdi] : msgOpts.comp_udi)
				{
					if	(  subUdi == 0 )
						continue;
					if (((long int)targetTime) % subUdi != 0)
					{
						if (!new_mask
							&& subType != +CompactSSRSubtype::SRV)
						{
							continue;
						}
					}

					int subUdiIndex = 0;
					if (subType != +CompactSSRSubtype::SRV)
						subUdiIndex = getUdiIndex(subUdi);

					ssrMeta.updateIntIndex	= subUdiIndex;
					if (ssrMeta.updateIntIndex == 0)		t0 = targetTime;
					else									t0 = targetTime + subUdi / 2.0;

					switch (subType)
					{
						case +CompactSSRSubtype::MSK:
							new_mask					= true;
							approvedMessages[subType]	= subUdiIndex;
							break;
						case +CompactSSRSubtype::ORB:
						case +CompactSSRSubtype::CLK:
						case +CompactSSRSubtype::CMB:
						case +CompactSSRSubtype::URA:
							for (auto [sys, proc] : acsConfig.process_sys)			//todo aaron, this is all just copying stuff from one map to another
							if (proc)
							{
								auto sysOutMap = mongoReadOrbClk(t0, ssrMeta, masterIod, sys);			for (auto [sat, data] : sysOutMap)	{	ssrOutMap[sat] = data;		approvedMessages[subType] = subUdiIndex;	}
							}
							calculateSsrComb(t0, subUdi, ssrMeta, masterIod, ssrOutMap);
							break;
						case +CompactSSRSubtype::COD:
							for (auto [sys, proc] : acsConfig.process_sys)
							if (proc)
							{
								auto sysCBMap  = mongoReadCodeBias(ssrMeta, masterIod, sys);			for (auto [sat, data] : sysCBMap)	{	ssrCodMap[sat] = data;		approvedMessages[subType] = subUdiIndex;	}
							}
							break;
						case +CompactSSRSubtype::PHS:
							for (auto [sys, proc] : acsConfig.process_sys)
							if (proc)
							{
								auto sysPBMap  = mongoReadPhaseBias(ssrMeta, masterIod, sys);			for (auto [sat, data] : sysPBMap)	{	ssrPhsMap[sat] = data;		approvedMessages[subType] = subUdiIndex;	}
							}
							break;
						case +CompactSSRSubtype::BIA:
							for (auto [sys, proc] : acsConfig.process_sys)
							if (proc)
							{
								auto sysCBMap  = mongoReadCodeBias	(ssrMeta, masterIod, sys);			for (auto [sat, data] : sysCBMap)	{	ssrCodMap[sat] = data;		approvedMessages[subType] = subUdiIndex;	}
								auto sysPBMap  = mongoReadPhaseBias	(ssrMeta, masterIod, sys);			for (auto [sat, data] : sysPBMap)	{	ssrPhsMap[sat] = data;		approvedMessages[subType] = subUdiIndex;	}
							}
							break;
						case +CompactSSRSubtype::TEC:
						case +CompactSSRSubtype::GRD:
						case +CompactSSRSubtype::ATM:
						case +CompactSSRSubtype::SRV:
						{
							ssrAtm = mongoReadCmpAtmosphere(targetTime, ssrMeta);

							if (ssrAtm.atmosRegionsMap.empty() == false)
								approvedMessages[subType] = subUdiIndex;

							break;
						}
						// default:
						// 	BOOST_LOG_TRIVIAL(error) << "Error, attempting to upload incorrect compacr SSR type: " << subType.to_integral << "\n";

					}
				}

				if (approvedMessages.empty())
					break;

				CompactSSRSubtype lastSubType = CompactSSRSubtype::NONE;

				for (auto [subType, udi] : approvedMessages)
				{
					if	(  subType == +CompactSSRSubtype::SRV
						|| subType == +CompactSSRSubtype::MSK)
					{
						continue;
					}

					lastSubType = subType;
				}

				int lastReg = -1;
				if	(  lastSubType == +CompactSSRSubtype::GRD
					|| lastSubType == +CompactSSRSubtype::TEC
					|| lastSubType == +CompactSSRSubtype::ATM)
				for (auto& [regId, regData] : ssrAtm.atmosRegionsMap)
					lastReg = regId;

				for (auto [subType, udi] : approvedMessages)
				{
					bool last = (lastSubType == subType);

					switch (subType)
					{
						case +CompactSSRSubtype::SRV:															{auto buffer = encodecompactSRV(									ssrAtm);			encodeWriteMessageToBuffer(buffer);		break;}
						case +CompactSSRSubtype::MSK:															{auto buffer = encodecompactMSK( ssrOutMap,	ssrCodMap,	ssrPhsMap,	ssrAtm, udi);		encodeWriteMessageToBuffer(buffer);		break;}
						case +CompactSSRSubtype::ORB:															{auto buffer = encodecompactORB( ssrOutMap,									udi, last);	encodeWriteMessageToBuffer(buffer);		break;}
						case +CompactSSRSubtype::CLK:															{auto buffer = encodecompactCLK( ssrOutMap,									udi, last);	encodeWriteMessageToBuffer(buffer);		break;}
						case +CompactSSRSubtype::CMB:															{auto buffer = encodecompactCMB( ssrOutMap,									udi, last);	encodeWriteMessageToBuffer(buffer);		break;}
						case +CompactSSRSubtype::URA:															{auto buffer = encodecompactURA( ssrOutMap,									udi, last);	encodeWriteMessageToBuffer(buffer);		break;}
						case +CompactSSRSubtype::COD:															{auto buffer = encodecompactCOD(			ssrCodMap,						udi, last);	encodeWriteMessageToBuffer(buffer);		break;}
						case +CompactSSRSubtype::PHS:															{auto buffer = encodecompactPHS(						ssrPhsMap,			udi, last);	encodeWriteMessageToBuffer(buffer);		break;}
						case +CompactSSRSubtype::BIA: 															{auto buffer = encodecompactBIA(			ssrCodMap,	ssrPhsMap,			udi, last);	encodeWriteMessageToBuffer(buffer);		break;}
						case +CompactSSRSubtype::TEC:	for (auto& [regId, regData] : ssrAtm.atmosRegionsMap)	{auto buffer = encodecompactTEC(ssrAtm.ssrMeta, regId, regData,	udi, regId==lastReg);	encodeWriteMessageToBuffer(buffer);	}	break;
						case +CompactSSRSubtype::GRD:	for (auto& [regId, regData] : ssrAtm.atmosRegionsMap)	{auto buffer = encodecompactGRD(ssrAtm.ssrMeta, regId, regData, udi, regId==lastReg);	encodeWriteMessageToBuffer(buffer);	}	break;
						case +CompactSSRSubtype::ATM:	for (auto& [regId, regData] : ssrAtm.atmosRegionsMap)	{auto buffer = encodecompactATM(ssrAtm.ssrMeta, regId, regData, udi, regId==lastReg);	encodeWriteMessageToBuffer(buffer);	}	break;
					}
				}
				break;
			}
			default:
				BOOST_LOG_TRIVIAL(error) << "Error, attempting to upload incorrect message type: " << messCode << "\n";
		}
	}

	std::stringstream messStream;
	encodeWriteMessages(messStream);

	messStream.seekg(0, messStream.end);
	int length = messStream.tellg();
	messStream.seekg(0, messStream.beg);

	BOOST_LOG_TRIVIAL(debug) << "Called " << __FUNCTION__ << " MessageLength : " << length << "\n";
	if (length != 0)
	{
		vector<char> data;
		data.resize(length);

		outMessagesMtx.lock();
		std::ostream chunkedStream(&outMessages);
		chunkedStream << std::uppercase << std::hex << length << "\r\n";

		messStream		.read	(&data[0], length);
		chunkedStream	.write	(&data[0], length);
		chunkedStream << "\r\n";

		if (url.protocol == "https")	{	boost::asio::async_write(*_sslsocket,	outMessages, boost::bind(&NtripUploader::writeHandler, this, bp::error));}
		else							{	boost::asio::async_write(*_socket,		outMessages, boost::bind(&NtripUploader::writeHandler, this, bp::error));}

		previousTargetTime = targetTime;
	}
}

void NtripUploader::startBroadcast()
{
	// BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << " Starting Send Loop.\n";

	sendTimer.expires_from_now(boost::posix_time::seconds(1));
	sendTimer.async_wait(boost::bind(&NtripUploader::messageTimeoutHandler, this, bp::error));
}

void NtripUploader::connected()
{
	// BOOST_LOG_TRIVIAL(info) << "Uploader connected.\n";

	// Although there should be no downloading attempting to download monitors the socket connection.
	startRead(true);

	startBroadcast();
}



