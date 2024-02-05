
#pragma once

#include "streamSerial.hpp"
#include "ntripSocket.hpp"


#include <mutex>



struct NtripStream : NtripSocket, SerialStream
{

private:

	std::mutex				receivedDataBufferMtx;	//< This mutex ensures that the main thread and the io_service thread dont alter the receivedDataBuffer buffer at the same time.
	vector<vector<char>>	chunkList;

public:

	NtripStream(
		const string& url_str) :
		NtripSocket(url_str)
	{
		std::stringstream	request_stream;
							request_stream	<< "GET " 		<< url.path << " HTTP/1.1"				<< "\r\n";
							request_stream	<< "Host: " 	<< url.host								<< "\r\n";
							request_stream	<< "Ntrip-Version: Ntrip/2.0"							<< "\r\n";
							request_stream	<< "User-Agent: NTRIP ACS/1.0"							<< "\r\n";
		if (!url.user.empty())
		{
							request_stream	<< "Authorization: Basic "
											<< Base64::encode(string(url.user + ":" + url.pass))	<< "\r\n";
		}
							request_stream	<< "Connection: close"									<< "\r\n";
							request_stream	<< "\r\n";

		request_string = request_stream.str();

		connect();
	}

	/** Retrieve data from the stream and store it for later removal
	*/
	void getData()
	override;

	void connected()
	override;

	void dataChunkDownloaded(
		vector<char>& dataChunk)
	override;


	/*
	* Due to boost::asio::io_service::work being added to boost::asio::io_service
	* io_service.run() blocks the thread in NtripStream::connect() and uses it in
	* the background to perform ansyncronous operations until io_service.stop()
	* is called at which time the thread exits.
	* As it is detached there is no need for a join, there is one worker thread
	* for all the NtripStream objects.
	*/

	virtual ~NtripStream() = default;
};

