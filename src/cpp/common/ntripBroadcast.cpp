
// #pragma GCC optimize ("O0")

#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/json.hpp>

using bsoncxx::builder::basic::kvp;

#include "ntripBroadcast.hpp"
#include "acsStream.hpp"
#include "mongoRead.hpp"
#include "fileLog.hpp"
#include "gTime.hpp"

NtripBroadcaster ntripBroadcaster;

namespace bp = boost::asio::placeholders;


using boost::posix_time::ptime;
using boost::posix_time::time_duration;

ptime round_to(
	ptime t,
	time_duration period)
{
    auto units = (t.time_of_day() + period/2).total_seconds() / period.total_seconds();
	
    return { t.date(), period*units };
}

void NtripBroadcaster::startBroadcast()
{
	NtripSocket::startClients();
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
	doc.append(kvp("label", 			"serverResponse"));
	doc.append(kvp("Stream", 			url.path.substr(1,url.path.length())));
// 	doc.append(kvp("Time", 				std::put_time(std::localtime( &time ), "%F %X")));
	doc.append(kvp("ServerStatus", 		(int)status_code));
	doc.append(kvp("VersionHTTP",		http_version));
	
	logStream << bsoncxx::to_json(doc) << std::endl;
}


void NtripUploader::write_handler(
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

void NtripUploader::messageTimeout_hanlder(
	const boost::system::error_code& err)
{
// 	BOOST_LOG_TRIVIAL(info) << "started " << __FUNCTION__ << "\n";
	if (err)
	{
		ERROR_OUTPUT_RECONNECT_AND_RETURN;
	}

	int period		= streamConfig.update_interval;
	int udiIndex	= getUdiIndex(period);
	
	
	//fire this callback again in the future
	{
		boost::posix_time::ptime now = boost::posix_time::second_clock::universal_time();
		auto then = round_to(now, boost::posix_time::seconds(period));
		then += boost::posix_time::seconds(period);
		sendTimer.expires_at(then);
		sendTimer.async_wait(boost::bind(&NtripUploader::messageTimeout_hanlder, this, bp::error));
	}
	
	
	SSRMeta ssrMeta;

	ssrMeta.ssrUpdateIntIndex 	= udiIndex;
	if (ssrMeta.ssrUpdateIntIndex == -1)
		BOOST_LOG_TRIVIAL(error) << "Error: ssrMeta.ssrUpdateIntIndex is not valid :" << ssrMeta.ssrUpdateIntIndex << ").";
	
	GTime nowTime = timeget();
	GTime targetTime;
	{
		int		week;
		double	tow			= time2gpst(timeget() + period / 2, &week);
		double	roundedTow	= ((int)(tow / period)) * period;
		
		ssrMeta.epochTime1s			= roundedTow;
		targetTime					= gpst2time(week, roundedTow);
	}
	
	ssrMeta.multipleMessage 	= 1; // We assume there will be more messages.
	
	if (streamConfig.itrf_datum)		ssrMeta.referenceDatum 	=  1; // Orbit corrections, 0 - ITRF, 1 - Regional.
	else								ssrMeta.referenceDatum 	=  0;
	
	ssrMeta.provider			= streamConfig.provider_id;
	ssrMeta.provider			= streamConfig.solution_id;
	int masterIod 				= streamConfig.master_iod;
	
	for (auto RtcmMess : streamConfig.rtcmMessagesTypes)
	{
		if (RtcmMess == *streamConfig.rtcmMessagesTypes.rbegin())
			ssrMeta.multipleMessage = 0;
		
		E_Sys sys;
		switch (RtcmMess)
		{
			case +RtcmMessageType::GPS_SSR_COMB_CORR:
			case +RtcmMessageType::GPS_SSR_PHASE_BIAS:
			case +RtcmMessageType::GPS_SSR_CODE_BIAS:
				sys = E_Sys::GPS;
				break;
			case +RtcmMessageType::GAL_SSR_COMB_CORR:
			case +RtcmMessageType::GAL_SSR_CODE_BIAS:
			case +RtcmMessageType::GAL_SSR_PHASE_BIAS:
				sys = E_Sys::GAL;
				break;
		}
		
		switch (RtcmMess)
		{
			case +RtcmMessageType::GAL_SSR_PHASE_BIAS:
			case +RtcmMessageType::GPS_SSR_PHASE_BIAS:
			{
				auto ssrPBMap = mongoReadPhaseBias(targetTime, ssrMeta, masterIod, sys);
				
				auto buffer = encodeSsrPhase(ssrPBMap);	
				bool write = encodeWriteMessageToBuffer(buffer);
				
				if (write == false)
				{
					std::cout << "RtcmMessageType::" << RtcmMess._to_string() << " was not written" << std::endl;
				}
				
				break;			
			}
			case +RtcmMessageType::GAL_SSR_CODE_BIAS:
			case +RtcmMessageType::GPS_SSR_CODE_BIAS:
			{
				auto ssrCBMap = mongoReadCodeBias(targetTime, ssrMeta, masterIod, sys);	
				
				auto buffer = encodeSsrCode(ssrCBMap);
				bool write = encodeWriteMessageToBuffer(buffer);
				
				if (write == false)
				{
					std::cout << "RtcmMessageType::" << RtcmMess._to_string() << " was not written" << std::endl;
				}
				
				break;
			}
			case +RtcmMessageType::GAL_SSR_COMB_CORR:
			case +RtcmMessageType::GPS_SSR_COMB_CORR:
			{
				auto ssrOutMap 	= mongoReadSSRData(targetTime, ssrMeta, masterIod, sys);

				calculateSsrComb(targetTime, period, ssrMeta, masterIod, ssrOutMap);
				
				auto buffer = encodeSsrComb(ssrOutMap);
				bool write = encodeWriteMessageToBuffer(buffer);
				
				if (write == false)
				{
					std::cout << "RtcmMessageType::" << RtcmMess._to_string() << " was not written" << std::endl;
				}
				
				break;
			}
			default:
				BOOST_LOG_TRIVIAL(error) << "Error, attempting to upload incorrect message type.\n";
		}
	}
	
	std::stringstream messStream;
	encodeWriteMessages(messStream);

	messStream.seekg(0, messStream.end);
	int length = messStream.tellg();
	messStream.seekg(0, messStream.beg);
			
	BOOST_LOG_TRIVIAL(debug) << "Called " << __FUNCTION__ << " MessageLength : " << length << std::endl;
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
	
		if (url.protocol == "https")	{	boost::asio::async_write(*_sslsocket,	outMessages, boost::bind(&NtripUploader::write_handler, this, bp::error));}
		else							{	boost::asio::async_write(*_socket,		outMessages, boost::bind(&NtripUploader::write_handler, this, bp::error));}
	}
}

void NtripUploader::startBroadcast()
{
// 	BOOST_LOG_TRIVIAL(info) << __FUNCTION__ << " Starting Send Loop.\n";

	sendTimer.expires_from_now(boost::posix_time::seconds(1));
	sendTimer.async_wait(boost::bind(&NtripUploader::messageTimeout_hanlder, this, bp::error));
}

void NtripUploader::connected()
{
	BOOST_LOG_TRIVIAL(info) << "Uploader connected.\n";
	
	// Although there should be no downloading attempting to download monitors the socket connection.
	start_read(true);
	
	startBroadcast();
}



