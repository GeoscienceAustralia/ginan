
#pragma once

#include <iostream>
#include <string>
#include <vector>

using std::string;
using std::vector;

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/iostreams/device/array.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/system/error_code.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/trivial.hpp>
#include <boost/asio/buffer.hpp>
#include <boost/thread.hpp>
#include <boost/format.hpp>
#include <boost/regex.hpp>
#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>

#include "streamSerial.hpp"
#include "ntripTrace.hpp"


/* Interface to be used for NTRIP version 2, streams for downloading messages.
* The NtripSteam was ugraded by Alex and now connects and download ansyncronously
* in the background on a boost service with it's own thread created in the constuctor.
* The message chunking of NTRIP version 2 has been implemented and receivedDataBuffer
* is filled with valid RCTM messages in the background without the chunked encoding data.
* During testing there is some data downloading that is invalid, being not complient with
* the chunked protocol. This data is not transfered to receivedDataBuffer.
* As in the previous implementation getData() is called once per epoch of the main loop
* and the completed and checked RTCM messages are placed in receivedData in a thread safe
* way. A destructor was added to correctly close socket when the program exits.
* Addition ansyncronous code was added to connect reconnect and establish a data stream.
* An example for the boost library documenation was used;
* https://www.boost.org/doc/libs/1_40_0/doc/html/boost_asio/example/http/client/async_client.cpp
*/
namespace B_asio    = boost::asio;
namespace ip		= B_asio::ip;
namespace ssl		= B_asio::ssl;
using tcp			= ip::tcp;
using error_code	= boost::system::error_code;
using ssl_socket	= ssl::stream<tcp::socket>;


#define ERROR_OUTPUT_RECONNECT_AND_RETURN 																						\
{																																\
	string message = err.message();																								\
	std::erase(message, '\r');																									\
	std::erase(message, '\n');																									\
	onErrorStatistics(err, __FUNCTION__);																						\
	BOOST_LOG_TRIVIAL(error) << "Error: NTRIP - " << message << " in " << __FUNCTION__ << " for " << url.sanitised();			\
																																\
	if (err != boost::asio::error::operation_aborted)																			\
		delayedReconnect();																									\
																																\
	return;																														\
}


struct Base64
{
	static string encode(string in)
	{
		return encode(in.c_str(), in.length());
	}

	static string encode(const char * in, std::size_t len)
	{
		string out;
		const char constexpr tab[] =
		{
			"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			"abcdefghijklmnopqrstuvwxyz"
			"0123456789+/"
		};

		for (auto n = len / 3; n--; )
		{
			out += tab[ (in[0] & 0xfc) >> 2];
			out += tab[((in[0] & 0x03) << 4) + ((in[1] & 0xf0) >> 4)];
			out += tab[((in[2] & 0xc0) >> 6) + ((in[1] & 0x0f) << 2)];
			out += tab[  in[2] & 0x3f];
			in += 3;
		}

		switch (len % 3)
		{
			case 2:
				out += tab[ (in[0] & 0xfc) >> 2];
				out += tab[((in[0] & 0x03) << 4) + ((in[1] & 0xf0) >> 4)];
				out += tab[                         (in[1] & 0x0f) << 2];
				out += '=';
				break;

			case 1:
				out += tab[ (in[0] & 0xfc) >> 2];
				out += tab[((in[0] & 0x03) << 4)];
				out += '=';
				out += '=';
				break;

			case 0:
				break;
		}

		return out;
	}
};



struct URL
{
	string				url;
	string				protocol;
	string				portStr;
	string				user;
	string				pass;
	string				host;
	int					port;
	string				path;
	map<string, string>	params;

	URL()
	{

	}

	URL(const string& url)
	{
		this->url = url;

		bool fail = false;

		string subUrl;
		auto protocolPos	= url		.find("://");
		if (protocolPos == string::npos)	{	protocol	= "file";								subUrl		= url;									}
		else								{	protocol	= url		.substr(0, protocolPos);	subUrl		= url		.substr(protocolPos	+ 3);	}

		string serverStr;
		string pathNParams;
		auto slashPos		= subUrl	.find("/");
		if (slashPos == string::npos)		{	fail		= true;																						}
		else								{	serverStr	= subUrl	.substr(0, slashPos);		pathNParams	= subUrl	.substr(slashPos	+ 1);	}

		string passNUrl;
		auto colonPos		= serverStr	.find(":");
		if (colonPos == string::npos)		{		}
		else								{	user		= serverStr	.substr(0, colonPos);		passNUrl	= serverStr	.substr(colonPos	+ 1);	}

		string justUrl;
		auto atPos			= passNUrl	.find("@");
		if (atPos == string::npos)			{		}
		else								{	pass		= passNUrl	.substr(0, atPos);			justUrl		= passNUrl	.substr(atPos		+ 1);	}


		colonPos			= justUrl	.find(":");
		if (colonPos == string::npos)		{	host		= justUrl;																					}
		else								{	host		= justUrl	.substr(0, colonPos);		portStr		= justUrl	.substr(colonPos	+ 1);	}

		size_t questionPos = pathNParams.find("?");
		path = ((string)"/") + pathNParams.substr(0, questionPos);

		while (questionPos != string::npos)
		{
			pathNParams = pathNParams.substr(questionPos	+ 1);		colonPos	= pathNParams.find(":");	string paramName	= pathNParams.substr(0, colonPos);
			pathNParams = pathNParams.substr(colonPos		+ 1);		questionPos	= pathNParams.find("?");	string paramValue	= pathNParams.substr(0, questionPos);

			params[paramName] = paramValue;
		}

		if (fail)
		{
			BOOST_LOG_TRIVIAL(debug) << "Invalid URL [" << url << "]";
			*this = URL();
			return;
		}

		if (protocol.empty())
			protocol = "http";

		if (portStr.empty())
		{
			if (protocol == "https")	portStr = "443";
			else						portStr = "2101";
		}
		port = std::stoi(portStr);
	}

	string sanitised()
	{
		return protocol + ":" + "//" + host + (port > 0 ? (":" + std::to_string(port)) : "") + path;
	}
};

struct TcpSocket : NetworkStatistics, SerialStream
{
protected:
	std::shared_ptr<tcp::socket>	_socket;
	tcp::socket*					socket_ptr;
	std::shared_ptr<ssl_socket>		_sslsocket;
	std::shared_ptr<tcp::resolver>	_resolver;

	boost::asio::deadline_timer		timer;

	string readUntilString;
	string requestString;
	string responseString;

	boost::asio::streambuf			request;
	boost::asio::streambuf			downloadBuf;

	vector<char>					receivedTcpData;

public:
	URL		url;

	double	reconnectDelay			= 1;
	int		disconnectionCount		= 0;
	bool	isConnected				= false;

	int		numberErroredChunks		= 0;
	bool	logHttpSentReceived		= false;

	unsigned int content_length = 0;

	TcpSocket(const string& url_str,
			  const string& readUntil = "\r\n\r\n")
	:	timer			(ioService),
		readUntilString	(readUntil),
		sslContext		(ssl::context::sslv23_client)
	{
		url			= URL(url_str);
		streamName	= url.path;
	}

	void setUrl(const string& url_str)
	{
		url			= URL(url_str);
		streamName	= url.path;
	}

	void connect();
	void disconnect();


	void startRead(bool chunked);

			void timeoutHandler			(const boost::system::error_code& err);
			void delayedReconnect();
			void connectHandler			(const boost::system::error_code& err, tcp::resolver::iterator endpoint_iterator);
	virtual	void requestResponseHandler	(const boost::system::error_code& err) {};

private:

	std::mutex				receivedDataBufferMtx;	//< This mutex ensures that the main thread and the io_service thread dont alter the receivedDataBuffer buffer at the same time.
	vector<vector<char>>	chunkList;

	// These functions manage the connection using the boost service and
	// asyncronous function calls.
	void resolveHandler			(const boost::system::error_code& err, tcp::resolver::iterator endpoint_iterator);
	void sslHandshakeHandler	(const boost::system::error_code& err);
	void reconnectTimerHandler	(const boost::system::error_code& err);

	void readHandlerContent		(const boost::system::error_code& err);
	void readHandlerChunked		(const boost::system::error_code& err);
	void writeRequestHandler	(const boost::system::error_code& err);

public:
	void logChunkError();


	void dataChunkDownloaded(
		vector<char>& dataChunk);

	//content from a one-shot request has been received
	virtual void readContentDownloaded(
		vector<char> content)
	{

	}

	virtual void connected()
	{
		startRead(true);
	}

	virtual void messageChunkLog(
		string message)
	{

	}

	virtual void networkLog(
		string message)
	{

	}

	void getData()
	override;

	void connectionError(
		const boost::system::error_code&	err,
		string								operation);

	B_asio::ssl::context sslContext;

	static B_asio::io_service ioService;

	static void runService()
	{
		B_asio::io_service::work work(ioService);
		ioService.run();
	}

	static void startClients()
	{
		std::thread(TcpSocket::runService).detach();
	}

	virtual ~TcpSocket() = default;
};


