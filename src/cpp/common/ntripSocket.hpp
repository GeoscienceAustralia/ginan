
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
		delayed_reconnect();																									\
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
	string  url;
	string  protocol;
	string  user;
	string  pass;
	string  host;
	string  port_str;
	int     port;
	string  path;

	static URL parse(string url)
	{
		boost::regex re (R"((https?)://((.+):(.+)@)?([^:@]+)(:(\d+))?(/.*)$)", boost::regex::extended);

		boost::smatch matches;

		if (!boost::regex_match(url, matches, re))
		{
			BOOST_LOG_TRIVIAL(debug) << "Invalid URL [" << url << "]";
			return URL();
		}

		BOOST_LOG_TRIVIAL(debug)
		<< "Valid URL ["    << url << "]";

		BOOST_LOG_TRIVIAL(debug)
		<< "protocol=["     << matches[1]

		<< "] username=["   << matches[3]
		<< "] password=["   << matches[4]
		<< "] host=["       << matches[5]
		<< "] port=["       << matches[7]
		<< "] path=["       << matches[8] << "]";

		URL out;

		out.url = url;

		string protocol	= matches[1];

		out.user		= matches[3];
		out.pass		= matches[4];
		out.host		= matches[5];
		out.port_str	= matches[7];
		out.path		= matches[8];

		out.protocol	= protocol.empty() ? "http"    : protocol;
		if (out.port_str.empty())
		{
			if (out.protocol == "https")    out.port_str = "443";
			else                            out.port_str = "2101";
		}
		out.port		= std::stoi(out.port_str);

		return out;
	}

	string sanitised()
	{
		return protocol + ":" + "//" + host + (port > 0 ? (":" + std::to_string(port)) : "") + path;
	}
};

struct NtripSocket : NetworkStatistics
{
protected:
	std::shared_ptr<tcp::socket>	_socket;
	tcp::socket*					socket_ptr;
	std::shared_ptr<ssl_socket>		_sslsocket;
	std::shared_ptr<tcp::resolver>	_resolver;

	string request_string;
	string response_string;

	boost::asio::deadline_timer		timer;

	boost::asio::streambuf			request;
	boost::asio::streambuf			downloadBuf;

	vector<char>					receivedHttpData;

public:
	URL		url;

	double	reconnectDelay		= 1;
	int		disconnectionCount	= 0;
	bool	isConnected			= false;

	int				numberErroredChunks		= 0;
	bool			logHttpSentReceived		= false;

	unsigned int content_length = 0;

	NtripSocket(const string& url_str) :
		timer(io_service),
		ssl_context(ssl::context::sslv23_client)
	{
		url			= URL::parse(url_str);
		streamName	= url.path;
	}

	void setUrl(const string& url_str)
	{
		url			= URL::parse(url_str);
		streamName	= url.path;
	}

	void connect();
	void disconnect();


	void start_read(bool chunked);

protected:
	void delayed_reconnect();

private:
	// These functions manage the connection using the boost service and
	// asyncronous function calls.
	void resolve_handler			(const boost::system::error_code& err, tcp::resolver::iterator endpoint_iterator);
	void connect_handler			(const boost::system::error_code& err, tcp::resolver::iterator endpoint_iterator);
	void sslhandshake_handler		(const boost::system::error_code& err);
	void write_request_handler		(const boost::system::error_code& err);
	void request_response_handler	(const boost::system::error_code& err);
	void reconnect_timer_handler	(const boost::system::error_code& err);
	void timeout_handler			(const boost::system::error_code& err);

	void read_handler_content		(const boost::system::error_code& err);
	void read_handler_chunked		(const boost::system::error_code& err);

public:
	void logChunkError();


	//content from a stream has been received - process it in virtual functions from other classes
	virtual void dataChunkDownloaded(
		vector<char>& dataChunk)
	{
	}

	//content from a one-shot request has been received - process it in virtual functions from other classes
	virtual void readContentDownloaded(
		vector<char> content)
	{

	}

	virtual void connected()
	{

	}

	virtual void messageChunkLog(
		string message)
	{

	}

	virtual void networkLog(
		string message)
	{

	}

	virtual void connectionError(
		const boost::system::error_code&	err,
		string								operation);

	virtual void serverResponse(
		unsigned int	status_code,
		string			http_version);

	B_asio::ssl::context ssl_context;

	static B_asio::io_service io_service;
	static void runService()
	{
		B_asio::io_service::work work(io_service);
		io_service.run();
	}

	static void startClients()
	{
		std::thread(NtripSocket::runService).detach();
	}
};

