
// #pragma GCC optimize ("O0")

// #define BSONCXX_POLY_USE_MNMLSTC
// #define BSONCXX_POLY_USE_SYSTEM_MNMLSTC

#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/json.hpp>

#include <boost/system/error_code.hpp>

#include <chrono>


#include "ntripSocket.hpp"
#include "acsConfig.hpp"


using std::chrono::system_clock;
using bsoncxx::builder::basic::kvp;

namespace bp = boost::asio::placeholders;

B_asio::io_service NtripSocket::io_service;


void NtripSocket::logChunkError()
{
// 	if (numberValidChunks == 0)
// 		return;

	numberErroredChunks++;

	std::stringstream message;
	message << "HTTP chunk error, number of errors : ";
	message << numberErroredChunks;
// 	message << ", ratio (error/total) : " << (double)numberErroredChunks /(double)(numberErroredChunks+numberValidChunks);

	std::cout << message.str() << std::endl;
	messageChunkLog(message.str());

		//todo aaron
// 		std::ofstream outStream(rtcmTraceFilename, std::ios::app);
// 		if (!outStream)
// 		{
// 			std::cout << "Error opening " << rtcmTraceFilename << " in " << __FUNCTION__ << std::endl;
// 			return;
// 		}
//
// 		outStream << timeGet();
// 		outStream << " messageChunkLog" << message << std::endl;
}




void NtripSocket::start_read(
	bool chunked)
{
	auto function_ptr = &NtripSocket::read_handler_content;

	if (chunked)	function_ptr = &NtripSocket::read_handler_chunked;
	else			function_ptr = &NtripSocket::read_handler_content;

	//BOOST_LOG_TRIVIAL(debug) << "NtripSocket::start_read\n";
	//BOOST_LOG_TRIVIAL(debug) << "Downloading, length : " << content_length << std::endl;
	//BOOST_LOG_TRIVIAL(debug) << "downloadBuf.size() : " << downloadBuf.size() << std::endl;

	// Start reading remaining data until EOF.
	if (url.protocol == "https")
	{
		boost::asio::async_read(*_sslsocket,	downloadBuf, boost::asio::transfer_at_least(1), boost::bind(function_ptr, this, bp::error));
	}
	else
	{
		boost::asio::async_read(*_socket,		downloadBuf, boost::asio::transfer_at_least(1), boost::bind(function_ptr, this, bp::error));
	}
}


void NtripSocket::read_handler_content(
	const boost::system::error_code& err)
{
	if (err)
	{
		ERROR_OUTPUT_RECONNECT_AND_RETURN;
	}

	//BOOST_LOG_TRIVIAL(debug) << __FUNCTION__;
	//BOOST_LOG_TRIVIAL(debug) << "Downloading, length : " << content_length << std::endl;
	//BOOST_LOG_TRIVIAL(debug) << "downloadBuf.size() : " << downloadBuf.size() << std::endl;
	if (downloadBuf.size() == content_length)
	{
		vector<char> content(downloadBuf.size());
		buffer_copy(boost::asio::buffer(content), downloadBuf.data());

		readContentDownloaded(content);

		return;
	}

	start_read(false);
}


void NtripSocket::read_handler_chunked(
	const boost::system::error_code& err)
{
	if (err)
	{
		ERROR_OUTPUT_RECONNECT_AND_RETURN;
	}

	// Note downloadBuf can be filled with data past the "\r\n" termination.
	// We read whole lines only into receivedBuffer.
	// Messages are chuncked NTRIP Version 2 see RCTM NTRIP document.

	// The message should begin with the message header containing the length in hexadecimal
	// ascii charaters followed by carriage return and line feed.
	// "AE<CR><LF>", this routine recurses ancyronously searching the stream for a valid header.
	// If there is error it starts searching again. Once a valided header is found it places
	// the RTCM message in the buffer provided this can be done without error.

	// The async_read invokes this function every time it reads "<CR><LF>", although
	// it can read more data as so downloadBuf may not end with "<CR><LF>".

	// If there is a problem with the chunking attempt is made to pass the message to the
	// RTCM parser. The RTCM parser has further error checking and may recover some messages.

	onChunkReceivedStatistics();

// 	std::istream messStream(&downloadBuf);
// 	unsigned int sz = downloadBuf.size();

// 	std::cout << " size" << downloadBuf.size() << std::endl;
	int oldSize		= receivedHttpData	.size();
	int extraSize	= downloadBuf		.size();

	receivedHttpData.resize(oldSize + extraSize);
	auto destination = boost::asio::buffer(&receivedHttpData[oldSize], extraSize);
	buffer_copy(destination, downloadBuf.data());
	downloadBuf.consume(extraSize);

	int last = receivedHttpData.size();

	int start = 0;
	if (receivedHttpData.empty() == false)
	while (true)
	{
		int endOfLength = 0;
		int endOfHeader;
		for (endOfHeader = start + 1; endOfHeader < last; endOfHeader++)
		{
			unsigned char c1 = receivedHttpData[endOfHeader - 1];
			unsigned char c2 = receivedHttpData[endOfHeader];

			if	( c1 == ';')
			{
				endOfLength = endOfHeader - 1;
			}

			if	( c1 == '\r'
				&&c2 == '\n')
			{
				break;
			}
		}

		if (endOfHeader >= last)
		{
			break;
		}

		if (endOfLength == 0)
		{
			endOfLength = endOfHeader - 2;
		}

		if (endOfLength < 0)
		{
			start = 2;
			continue;
		}

		string hexLength(&receivedHttpData[start], &receivedHttpData[endOfLength + 1]);
// 		std::cout << "\nhexLength: " << hexLength << "\n";

		int messageLength;
		try
		{
			messageLength = std::stoi(hexLength, 0, 16);
		}
		catch (std::exception& e)
		{
			BOOST_LOG_TRIVIAL(warning) << "\nError Message Header, string not an integer: " << hexLength;
			logChunkError();
			start = endOfHeader;
			continue;
		}

		if (messageLength > 10000)
		{
			BOOST_LOG_TRIVIAL(warning) << "\nError Message Header, Body too Long: " << messageLength;
			logChunkError();
			start = endOfHeader;
			continue;
		}

		int startOfMessage	= endOfHeader + 1;
		int endOfMessage	= startOfMessage + messageLength - 1;

		if (endOfMessage + 2 >= receivedHttpData.size())
		{
			//not enough data, continue from this start later
			break;
		}


		char  postAmble1	= receivedHttpData[endOfMessage + 1];
		char  postAmble2	= receivedHttpData[endOfMessage + 2];

		if	( postAmble1 != '\r'
			||postAmble2 != '\n')
		{
			BOOST_LOG_TRIVIAL(warning) << "\nMissing Termination of Chunk Message.\n";
			start = endOfHeader;
			continue;
		}
// 		printf("\npostamble: %02x %02x\n", postAmble1, postAmble2);

		vector<char> chunk(&receivedHttpData[startOfMessage], &receivedHttpData[endOfMessage] + 1);
// 		printHex(std::cout, chunk);
		dataChunkDownloaded(chunk);

		start = endOfMessage + 2;
	}

	if (start > 0)
	{
		receivedHttpData.erase(receivedHttpData.begin(), receivedHttpData.begin() + start);
	}

	start_read(true);
}


void NtripSocket::reconnect_timer_handler(
	const boost::system::error_code& err)
{
	if (err)
	{
		ERROR_OUTPUT_RECONNECT_AND_RETURN;
	}

	connect();
}

void NtripSocket::timeout_handler(
	const boost::system::error_code& err)
{
	if (err)
	{
// 		ERROR_OUTPUT_RECONNECT_AND_RETURN;
		return;
	}

	if (isConnected == false)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: " << url.sanitised() <<" connection timed out, check paths, usernames + passwords, and ports";
	}
}

void NtripSocket::delayed_reconnect()
{
	if (isConnected)
	{
		isConnected = false;
		disconnectionCount++;

		std::stringstream message;

		networkLog(message.str());
	}
	else
	{
		// If the network does not connect after 10 seconds print the HTTP request and receive.
		if (logHttpSentReceived == false)
		{
			logHttpSentReceived = true;
		}
	}

	disconnect();

	//BOOST_LOG_TRIVIAL(debug) << " " << __FUNCTION__ << " " << url.sanitised() << " Started Timer.\n";

	// Delay and attempt reconnect, this prevents server abuse.
	timer.expires_from_now(boost::posix_time::seconds((int)reconnectDelay));

	//wait a little longer next time;
	reconnectDelay *= 2;

	timer.async_wait(boost::bind(&NtripSocket::reconnect_timer_handler, this, bp::error));
}

void NtripSocket::request_response_handler(
	const boost::system::error_code& err)
{
	if (err)
	{
		ERROR_OUTPUT_RECONNECT_AND_RETURN;
	}

	onChunkReceivedStatistics();

	vector<char> responseVec;
	responseVec.resize(downloadBuf.size());
	buffer_copy(boost::asio::buffer(responseVec), downloadBuf.data());

	response_string = "";
	response_string.assign(responseVec.begin(), responseVec.end());
	std::size_t pos = response_string.find("\r\n\r\n");
	if (pos == string::npos)
	{
		BOOST_LOG_TRIVIAL(error) << "Error handle_request_response : Invalid Server Response";
		response_string = "";
		delayed_reconnect();
		return;
	}
	response_string = response_string.substr(0,pos);
	response_string += "\r\n\r\n";
	// Note read buffer can be longer than the supplied delimiter.
	downloadBuf.consume(pos+4);


	// Check that response is OK.
	string bufStr = response_string;
	std::stringstream response_stream(bufStr);

	string			http_version;							response_stream >> http_version;
	unsigned int	status_code;							response_stream >> status_code;
	string			status_message;			std::getline(	response_stream, status_message);

	serverResponse(status_code, http_version);

	if	( !response_stream
		|| http_version.substr(0, 5) != "HTTP/"
		|| status_code != 200)
	{
		if (logHttpSentReceived)
		{
			std::stringstream message;
			message << "\nHTTP Sent     : ";
			message << request_string;
			message << "\nHTTP Received : ";
			message << response_string;

			networkLog(message.str());
		}

		BOOST_LOG_TRIVIAL(error) << "Error: NTRIP - " << status_code << " " << status_message << " in " << __FUNCTION__ << " for " << url.sanitised() << ", reconnecting in " << reconnectDelay;

		delayed_reconnect();

		return;
	}


	//BOOST_LOG_TRIVIAL(debug) << "**********************************************\n";
	BOOST_LOG_TRIVIAL(debug) << "Connected " << url.sanitised() << std::endl;

	//conneccted, turn the delay back down.
	reconnectDelay = 1;

	isConnected = true;

	if (disconnectionCount == 0)
	{
		networkLog("Initial Connection.");
	}
	else
	{
		std::stringstream message;

		networkLog(message.str());

		//BOOST_LOG_TRIVIAL(debug) << message.str() << std::endl;
	}

	boost::asio::socket_base::keep_alive option(true);
	socket_ptr->set_option(option);

	connected();
}


void NtripSocket::write_request_handler(
	const boost::system::error_code& err)
{
	if (err)
	{
		ERROR_OUTPUT_RECONNECT_AND_RETURN;
	}

	onChunkSentStatistics();

	//prepare a timeout because the read_until call doesnt seem to return on bad requests.
	timer.expires_from_now(boost::posix_time::seconds(10));
	timer.async_wait(boost::bind(&NtripSocket::timeout_handler, this, bp::error));

	// Read the response status line.
	if (url.protocol == "https")
	{
		boost::asio::async_read_until(*_sslsocket,	downloadBuf, "\r\n\r\n", boost::bind(&NtripSocket::request_response_handler, this, bp::error));
	}
	else
	{
		boost::asio::async_read_until(*_socket,		downloadBuf, "\r\n\r\n", boost::bind(&NtripSocket::request_response_handler, this, bp::error));
	}
}

void NtripSocket::sslhandshake_handler(
	const boost::system::error_code& err)
{
	if (err)
	{
		ERROR_OUTPUT_RECONNECT_AND_RETURN;
	}

	//BOOST_LOG_TRIVIAL(debug) << "SSL Handshake Completed.\n";

	// The connection was successful. Send the request.
	boost::asio::async_write(*_sslsocket, request, boost::bind(&NtripSocket::write_request_handler, this, bp::error));
}

void NtripSocket::connect_handler(
	const boost::system::error_code&	err,
	tcp::resolver::iterator				endpoint_iterator)
{
	if (err)
	{
		if (endpoint_iterator != tcp::resolver::iterator())
		{
			// The connection failed. Try the next endpoint in the list.
			tcp::endpoint endpoint = *endpoint_iterator;
			socket_ptr->async_connect(*endpoint_iterator, boost::bind(&NtripSocket::connect_handler, this, bp::error, ++endpoint_iterator));

			return;
		}
		else
		{
			ERROR_OUTPUT_RECONNECT_AND_RETURN;
		}
	}

	onConnectedStatistics();

	//BOOST_LOG_TRIVIAL(debug) << "Connect Completed.\n";

	if (url.protocol == "https")
	{
		_sslsocket->async_handshake(boost::asio::ssl::stream_base::client, boost::bind(&NtripSocket::sslhandshake_handler, this, bp::error));

		return;
	}

	// The connection was successful. Send the request.
	boost::asio::async_write(*_socket, request, boost::bind(&NtripSocket::write_request_handler, this, bp::error));
}


void NtripSocket::resolve_handler(
	const boost::system::error_code&	err,
	tcp::resolver::iterator				endpoint_iterator)
{
	if (err)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: check url and any usernames/passwords";
		ERROR_OUTPUT_RECONNECT_AND_RETURN;
	}

	//BOOST_LOG_TRIVIAL(debug) << "Resolve Completed.\n";

	// Attempt a connection to the first endpoint in the list. Each endpoint will be tried until we successfully establish a connection.

	tcp::endpoint endpoint = *endpoint_iterator;
	socket_ptr->async_connect(endpoint, boost::bind(&NtripSocket::connect_handler, this, bp::error, ++endpoint_iterator));
}

void NtripSocket::connect()
{
	// Pointers are required as objects may need to be destroyed to full recover
	// socket error.
	_socket		= std::make_shared<tcp::socket>		(io_service);
	_sslsocket	= std::make_shared<ssl_socket>		(io_service, ssl_context);
	_resolver	= std::make_shared<tcp::resolver>	(io_service);

	BOOST_LOG_TRIVIAL(debug) << "(Re)connecting " << url.sanitised() << std::endl;

	// The socket_ptr reduces some code, although the async_read and async_right
	// must be called using _sslsocket in order to work correctly.
	if (url.protocol == "https")	socket_ptr = &_sslsocket->next_layer();
	else 							socket_ptr = _socket.get();

	std::ostream				request_stream(&request);
								request_stream << request_string;

	//tcp::resolver::query query(url.host, url.port_str, boost::asio::ip::resolver_query_base::numeric_service);
	tcp::resolver::query		query(boost::asio::ip::tcp::v4(), url.host, url.port_str);

	_resolver->async_resolve(query, boost::bind(&NtripSocket::resolve_handler, this, bp::error, bp::iterator));
}

void NtripSocket::disconnect()
{
	onDisconnectedStatistics();

	try
	{
		socket_ptr->shutdown(boost::asio::ip::tcp::socket::shutdown_both);
		socket_ptr->close();
	}
	catch (...)
	{

	}

	// Clear the buffers except receivedBuffer that could possibly retain valid RTCM messages.
	request		.consume(request	.size());
	downloadBuf	.consume(downloadBuf.size());
}


void NtripSocket::connectionError(
	const boost::system::error_code&	err,
	string								operation)
{
	if (acsConfig.output_ntrip_log == false)
		return;

	std::ofstream logStream(networkTraceFilename, std::ofstream::app);

	if (!logStream)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: Error opening log file.\n";
		return;
	}

	GTime time = timeGet();

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("label", 			"connectionError"));
	doc.append(kvp("Stream", 			url.path.substr(1,url.path.length())));
	doc.append(kvp("Time", 				time.to_string()));
	doc.append(kvp("BoostSysErrCode",	err.value()));
	doc.append(kvp("BoostSysErrMess",	err.message()));
	doc.append(kvp("SocketOperation",	operation));

	logStream << bsoncxx::to_json(doc) << std::endl;
}

void NtripSocket::serverResponse(
	unsigned int	status_code,
	string			http_version)
{
	if (acsConfig.output_ntrip_log == false)
		return;

	std::ofstream logStream(networkTraceFilename, std::ofstream::app);

	if (!logStream)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: Error opening log file.\n";
		return;
	}

	GTime time = timeGet();

	bsoncxx::builder::basic::document doc = {};
	doc.append(kvp("label", 		"serverResponse"));
	doc.append(kvp("Stream", 		url.path.substr(1,url.path.length())));
	doc.append(kvp("Time", 			time.to_string()));
	doc.append(kvp("ServerStatus", 	(int)status_code));
	doc.append(kvp("VersionHTTP",	http_version));

	logStream << bsoncxx::to_json(doc) << std::endl;
}

