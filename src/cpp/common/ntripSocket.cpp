
// #pragma GCC optimize ("O0")

#include "ntripSocket.hpp"

#include <boost/system/error_code.hpp>
#include <list>

namespace bp = boost::asio::placeholders;

//B_asio::ssl::context NtripSocket::ssl_context(ssl::context::sslv23_client);
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
	
	messageChunkLog(message.str());      
}


void NtripSocket::printChunkHex(
	vector<char> chunk)
{
	BOOST_LOG_TRIVIAL(debug) << "HTTP Chunk Data : ";

	for (int i = 0; i < chunk.size(); i++)
	{
		if (i % 10 == 0)
			BOOST_LOG_TRIVIAL(debug) << std::endl;       
		
		printf("%02x ",(unsigned char)chunk[i]);
	}
	BOOST_LOG_TRIVIAL(debug) << std::endl; 
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
	
	vector<char> chunk;
	std::istream messStream(&downloadBuf);
	unsigned int sz = downloadBuf.size();
	
	char c_i;
	char c_j;
	unsigned int i = 0;
	
	onChunkReceivedStatistics();
	
	while (true)
	{
		if (chunked_message_length == 0)
		{
			chunk.clear();
			messStream.read(&c_i,1);
			chunk.push_back(c_i);
			for (i++; i < sz; i++)
			{
				messStream.read(&c_j,1);
				chunk.push_back(c_j);
				if	(  c_i == '\r' 
					&& c_j == '\n')
				{
					break;
				}
				c_i = c_j;
			}
		
			if (i >= sz)
			{
				//BOOST_LOG_TRIVIAL(debug) << "\nEnd of Buffer Reached Searching for Message Header Chunk.\n";
				//BOOST_LOG_TRIVIAL(debug) << "downloadBuf.size() : " << downloadBuf.size() << std::endl;
				//BOOST_LOG_TRIVIAL(debug) << "chunk.size()      : " << chunk.size() << std::endl;
				//printChunkHex(chunk);
				std::ostream outStream(&downloadBuf);
				outStream.write(&chunk[0],chunk.size());
				//BOOST_LOG_TRIVIAL(debug) << "messBuffer.size() : " << downloadBuf.size() << std::endl;
				break;
			}            

			// Read the first chunk with the message length.
			
			// Put termination charater on string.
			chunk.push_back(0);
			string messStr(&chunk[0]);
			messStr = messStr.substr(0, messStr.length()-2);
			// NTRIP 2 allows for an extended header.
			std::size_t ext = messStr.find(";");
			if (ext != string::npos)
			{
				//BOOST_LOG_TRIVIAL(debug) << "Found Extension in Message Length : " << messStr << std::endl;
				messStr = messStr.substr(0,ext);
			}
			
			try
			{
				// Two is added for the termination charaters.
				chunked_message_length = (unsigned int)std::stoi(messStr, 0, 16); 
				//BOOST_LOG_TRIVIAL(debug) << "\nValid Message Length Chunk Found.\n";
				//BOOST_LOG_TRIVIAL(debug) << "message chunk  : " << messStr << std::endl;
				//BOOST_LOG_TRIVIAL(debug) << "message_length : " << chunked_message_length << std::endl;
				//BOOST_LOG_TRIVIAL(debug) << "chunk.size()      : " << chunk.size() << std::endl;
				//printChunkHex(chunk);                    
				
				
				// Check if message is too long and set to zero.
				if (chunked_message_length > 1000000)
				{
					//BOOST_LOG_TRIVIAL(warning) << "\nError Message Header, Body too Long.\n";
					//printChunkHex(chunk);
					logChunkError();
					chunked_message_length = 0;
				}
				
			}
			catch (std::exception& e)
			{  
				//BOOST_LOG_TRIVIAL(warning) << "\nError Message Header, string not an integer.\n";
				//BOOST_LOG_TRIVIAL(warning) << "chunk.size() : " << chunk.size() << std::endl;
				//printChunkHex(chunk);
				logChunkError();
				chunked_message_length = 0;
			}            
		}
		else
		{
			// Read the second chunk with the message.		//todo aaron, roll into the first one and clean up
			
			// Buffer position for the end of the message body chunk.
			int bodyPos = i+chunked_message_length+2;
			
			// There is not enough data to read the message body.
			if (sz < bodyPos)
			{
				//BOOST_LOG_TRIVIAL(warning) << "\nBuffer sz : " << sz << std::endl;
				//BOOST_LOG_TRIVIAL(warning) << "Message end pos : " << bodyPos << std::endl;
				//BOOST_LOG_TRIVIAL(warning) << "Message buffer not large enough exiting.\n";
				break;
			}
				
			// Copy message out of receive buffer.
			chunk.clear();
			for (; i < bodyPos; i++)
			{
				messStream.read(&c_i,1);
				chunk.push_back(c_i);
			}                
			
			// Check the last two characters are '\r' and '\n' to complete chunk.
			if	(  chunk[chunked_message_length]	== '\r' 
				&& chunk[chunked_message_length+1]	== '\n')
			{
				// Remove the '\r\n' from the data vector.
				chunk.pop_back();
				chunk.pop_back();
				
				finishedReadingStream = dataChunkDownloaded(chunk);
				if (finishedReadingStream)
				{
					disconnect();
					
					return;
				}
			}
			else
			{
				logChunkError();
				
				/*
				BOOST_LOG_TRIVIAL(warning) << "\nMissing Termination of Chunk Message.\n";
				BOOST_LOG_TRIVIAL(warning) << "message_length : " << message_length << std::endl;
				BOOST_LOG_TRIVIAL(warning) << "chunk.size() : " << chunk.size() << std::endl;
				printChunkHex(chunk);
				BOOST_LOG_TRIVIAL(warning) << "chunk[message_length]   : ";
				printf("%02x\n",(unsigned char)chunk[message_length]);
				BOOST_LOG_TRIVIAL(warning) << "chunk[message_length+1] : ";
				printf("%02x\n",(unsigned char)chunk[message_length+1]);
				*/
			}
			
			//BOOST_LOG_TRIVIAL(debug) << "\nProcessed Chunk Body.\n";
			//BOOST_LOG_TRIVIAL(debug) << "chunk.size() : " << chunk.size() << std::endl;
			chunked_message_length = 0;           
		}
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

void NtripSocket::delayed_reconnect()
{
	if (isConnected)
	{
		isConnected = false;
		disconnectionCount++;
// 		disconnectedTime = boost::posix_time::microsec_clock::local_time();
		
// 		boost::posix_time::time_duration upTime		= disconnectedTime - connectedTime;
// 		boost::posix_time::time_duration totalTime	= disconnectedTime - startTime;
// 		connectedDuration += upTime;
		
		std::stringstream message;
// 		message << "disconnected, up time minutes : ";
// 		message << (double)upTime.total_milliseconds()/(60.0*1000.0);
		
// 		double ratio = (double)connectedDuration.total_milliseconds() /(double)totalTime.total_milliseconds();
// 		message << ", ratio (uptime_total/total) : " << ratio;
// 		message << ", total number of disconnections : " << disconnectionCount;
		
// 		double meanConn = (double)connectedDuration.total_milliseconds()/(60.0*1000.0*disconnectionCount);
// 		message << ", mean connection duration minutes : " << meanConn;

		networkLog(message.str());
	}
	else
	{
		// If the network does not connect after 10 seconds print the HTTP request and receive.
		if (logHttpSentReceived == false)
		{
// 			auto curTime = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
			
// 			boost::posix_time::time_duration totalTime = curTime - startTime;
			
// 			if (totalTime.total_milliseconds() > (10.0*1000.0))
				logHttpSentReceived = true;
		}
	}
	
	disconnect();
	
	//BOOST_LOG_TRIVIAL(debug) << " " << __FUNCTION__ << " " << url.sanitised() << " Started Timer.\n";

	// Delay and attempt reconnect, this prevents server abuse.
	timer.expires_from_now(boost::posix_time::seconds(1));
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
			message << "HTTP Sent :\n";
			message << request_string;
			message << "HTTP Reveived :\n";
			message << response_string;
			
			networkLog(message.str());
		}
		
		delayed_reconnect();
		
		return;
	}

		
	//BOOST_LOG_TRIVIAL(debug) << "**********************************************\n";
	BOOST_LOG_TRIVIAL(debug) << "Connected " << url.sanitised() << std::endl;
	
// 		connectedTime = boost::posix_time::from_time_t(std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()));
	isConnected = true;

	if (disconnectionCount == 0)
	{
		networkLog("Initial Connection.");
	}
	else
	{
// 			boost::posix_time::time_duration downTime = connectedTime - disconnectedTime;
// 			boost::posix_time::time_duration totalTime = connectedTime - startTime;
// 			disconnectedDuration += downTime;
		
		std::stringstream message;
// 			message << "connected, down time minutes : ";
// 			message << downTime.total_milliseconds()/(60.0*1000);
		
// 			double ratio = (double)connectedDuration.total_milliseconds() /(double)totalTime.total_milliseconds();
// 			message << ", ratio (uptime_total/total) : " << ratio;
// 			message << ", total number of disconnections : " << disconnectionCount;
		
// 			double meanReconn = (double)disconnectedDuration.total_milliseconds()/(60.0*1000.0*disconnectionCount);
// 			message << ", mean re-connection duration minutes : " << meanReconn;
		
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
	
	//BOOST_LOG_TRIVIAL(debug) << "Client Write Request Completed.\n";
	
	if (url.protocol == "https")
	{
		// Read the response status line.
		boost::asio::async_read_until(*_sslsocket,	downloadBuf, "\r\n\r\n", boost::bind(&NtripSocket::request_response_handler, this, bp::error));            
	}
	else
	{
		// Read the response status line.
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
	
	// Attempt a connection to the first endpoint in the list. Each endpoint
	// will be tried until we successfully establish a connection.

	tcp::endpoint endpoint = *endpoint_iterator;
	socket_ptr->async_connect(endpoint, boost::bind(&NtripSocket::connect_handler, this, bp::error, ++endpoint_iterator));
}


void NtripSocket::connect()
{
	chunked_message_length = 0;
	
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
		
	// Clear the buffers except receivedBuffer that could possibly retain
	// valid RTCM messages.
	request		.consume(request	.size());
	downloadBuf	.consume(downloadBuf.size());
}

