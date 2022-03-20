
#ifndef NTRIPSSRBROADCASTER_H
#define NTRIPSSRBROADCASTER_H


#include <mutex>

#include "streamNtrip.hpp"
#include "rtcmEncoder.hpp"
#include "ntripSocket.hpp"
#include "ntripTrace.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "enums.h"


struct NtripUploader : NtripSocket, RtcmEncoder
{
	boost::posix_time::ptime		timeNextMessage;
	
	// This mutex ensures that the main thread and the io_service thread do
	// not alter the outMessages buffer at the same time.
	std::mutex						outMessagesMtx;
	boost::asio::streambuf			outMessages;
	boost::asio::deadline_timer		sendTimer;
	int 	numberChunksSent 		= 0;
	
	
	bool 	print_stream_statistics = false;
	string	ntripStr 				= "";
	string	id						= "NtripUploader";

	SsrBroadcast			streamConfig;
	
	NtripUploader(
		const string& url_str) 
								:	NtripSocket(url_str), 
									sendTimer(io_service)
	{
		rtcmMountPoint = url.path;
		
		if (acsConfig.output_encoded_rtcm_json)
		{
			rtcmTraceFilename = acsConfig.encoded_rtcm_json_filename;
		}
		
		std::stringstream request_stream;
									request_stream	<< "POST " 		<< url.path << " HTTP/1.1"				<< "\r\n";
									request_stream	<< "Host: " 	<< url.host								<< "\r\n";
									request_stream	<< "Ntrip-Version: Ntrip/2.0"							<< "\r\n";
		if (!url.user.empty())
		{
									request_stream	<< "Authorization: Basic "
													<< Base64::encode(string(url.user + ":" + url.pass))	<< "\r\n";
		}
									request_stream	<< "User-Agent: NTRIP ACS/1.0"							<< "\r\n";
		if (!ntripStr.empty())		request_stream	<< "Ntrip-STR: " << ntripStr							<< "\r\n";
									request_stream	<< "Connection: keep-alive"								<< "\r\n";
									request_stream  << "Transfer-Encoding: chunked"							<< "\r\n";
									request_stream	<< "\r\n";
		
		request_string = request_stream.str();

		connect();
	};

	void startBroadcast();

	void connected() 
	override;

	void messageTimeout_hanlder(
		const boost::system::error_code& err);
	
	void write_handler(
		const boost::system::error_code& err);


	void serverResponse(
		unsigned int status_code,
		string		 http_version)
	override;

	void getJsonNetworkStatistics(
		GTime now);

};

struct NtripBroadcaster
{
	void startBroadcast();
	void stopBroadcast();

	map<string, std::shared_ptr<NtripUploader>> ntripUploadStreams;
};

extern	NtripBroadcaster ntripBroadcaster;

#endif
