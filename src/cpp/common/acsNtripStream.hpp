
#ifndef __ACS_NTRIPSTREAM_HPP
#define __ACS_NTRIPSTREAM_HPP

#include "ntripTrace.hpp"
#include "ntripSocket.hpp"





struct NtripStream : NtripSocket
{
private:
	// This mutex ensures that the main thread and the io_service thread do
	// not alter the receivedDataBuffer buffer at the same time.
	std::mutex receivedDataBufferMtx; 
		
	vector<char> receivedDataBuffer;

	

public:
	vector<char>                receivedData;
	
	
	NtripStream(const string& url_str) : 
		NtripSocket(url_str)
	{
		std::stringstream request_stream;
									request_stream	<< "GET " 		<< url.path << " HTTP/1.1\r\n";
									request_stream	<< "Host: " 	<< url.host << "\r\n";
									request_stream	<< "Ntrip-Version: Ntrip/2.0\r\n";
									request_stream	<< "User-Agent: NTRIP ACS/1.0\r\n";
		if (!url.username.empty())
		{               
									request_stream	<< "Authorization: Basic " 
													<< Base64::encode(string(url.username + ":" + url.password)) 
													<< "\r\n";
		}
									request_stream	<< "Connection: close\r\n";    
									request_stream	<< "\r\n";        
		
		request_string = request_stream.str();
		chunked_message_length = 0;
	
		connect();
	}

	/** Retrieve data from the stream and store it for later removal
	*/
	void getData();
	
	void connected() override;
	bool dataChunkDownloaded(vector<char> dataChunk) override;
	virtual void messageChunkLog(std::string message) override {}
	virtual void networkLog(std::string message) override {}
	virtual void connectionError(const boost::system::error_code& err, std::string operation) override {}
	virtual void serverResponse(unsigned int status_code, std::string http_version) override {}	
	/*
	* Due to boost::asio::io_service::work being added to boost::asio::io_service
	* io_service.run() blocks the thread in NtripStream::connect() and uses it in 
	* the background to perform ansyncronous operations until io_service.stop()
	* is called at which time the thread exits. 
	* As it is detached there is no need for a join, there is one worker thread
	* for all the NtripStream objects.
	*/ 
};

#endif
