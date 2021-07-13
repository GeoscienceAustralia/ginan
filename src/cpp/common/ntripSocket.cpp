#include "ntripSocket.hpp"

#include <boost/system/error_code.hpp>
#include <list>




//B_asio::ssl::context NtripSocket::ssl_context(ssl::context::sslv23_client);
B_asio::io_service NtripSocket::io_service;

void NtripSocket::connect()
{
	chunked_message_length = 0;
	
	// Pointers are required as objects may need to be destroyed to full recover
	// socket error.
	_socket    = std::make_shared<tcp::socket>(io_service);
	_sslsocket = std::make_shared<ssl_socket>(io_service, ssl_context);
	_resolver  = std::make_shared<tcp::resolver>(io_service);    
	
	
	//BOOST_LOG_TRIVIAL(debug) << "(Re)connecting " << url.sanitised() << std::endl;     
	
	// The socket_ptr reduces some code, although the async_read and async_right
	// must be called using _sslsocket in order to work correctly.
	if (url.protocol == "https")	socket_ptr = &_sslsocket->next_layer();
	else 							socket_ptr = _socket.get();

	std::ostream				request_stream(&request);
								request_stream << request_string;

	//tcp::resolver::query query(url.host, url.port_str, boost::asio::ip::resolver_query_base::numeric_service);
	tcp::resolver::query		query(boost::asio::ip::tcp::v4(), url.host, url.port_str);
	
	_resolver->async_resolve(query,
		boost::bind(&NtripSocket::handle_resolve, this,
		boost::asio::placeholders::error,
		boost::asio::placeholders::iterator));
}

void NtripSocket::disconnect()
{
	try
	{
		socket_ptr->shutdown(boost::asio::ip::tcp::socket::shutdown_both);
		socket_ptr->close();
	}         
	catch (std::exception& e){}
		
	// Clear the buffers except receivedBuffer that could possibly retain
	// valid RTCM messages.
	request.consume(request.size());
	downloadBuf.consume(downloadBuf.size());
}

void NtripSocket::delayed_reconnect()
{
	if( isConnected )
	{
		disconnectedTime = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
		isConnected = false;
		disconnectionCount++;
		
		boost::posix_time::time_duration upTime = disconnectedTime - connectedTime;
		boost::posix_time::time_duration totalTime = disconnectedTime - startTime;
		connectedDuration += upTime;
		
		std::stringstream message;
		message << "disconnected, up time minutes : ";
		message << (double)upTime.total_milliseconds()/(60.0*1000.0);
		double ratio = (double)connectedDuration.total_milliseconds() /(double)totalTime.total_milliseconds();
		message << ", ratio (uptime_total/total) : " << ratio;
		message << ", total number of disconnections : " << disconnectionCount;
		double meanConn = (double)connectedDuration.total_milliseconds()/(60.0*1000.0*disconnectionCount);
		message << ", mean connection duration minutes : " << meanConn;

		networkLog(message.str());
	}
	else
	{
		// If the network does not connect after 10 seconds print the HTTP request and receive.
		if (!logHttpSentReceived)
		{
			auto curTime = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
			boost::posix_time::time_duration totalTime = curTime - startTime;
			if( totalTime.total_milliseconds() > (10.0*1000.0) )
				logHttpSentReceived = true;
		}
	}
	
	disconnect();
	
	//BOOST_LOG_TRIVIAL(debug) << "delayed_reconnect " << url.sanitised() << " Started Timer.\n";

	// Delay and attempt reconnect, this prevents server abuse.
	timer.expires_from_now(boost::posix_time::seconds(1));
	timer.async_wait(boost::bind(&NtripSocket::handle_reconnect, this,
										boost::asio::placeholders::error));       
}



void NtripSocket::handle_reconnect(const boost::system::error_code& err)
{
	if (!err)
		connect();
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error " << url.sanitised() << " handle_reconnect : " << err.message() << "\n";
		if ( err != boost::asio::error::operation_aborted )
			delayed_reconnect();   
	} 
}



void NtripSocket::handle_resolve(const boost::system::error_code& err,
	tcp::resolver::iterator endpoint_iterator)
{
	//BOOST_LOG_TRIVIAL(debug) << "Resolve Completed.\n";
	if (!err)
	{
		// Attempt a connection to the first endpoint in the list. Each endpoint
		// will be tried until we successfully establish a connection.

		tcp::endpoint endpoint = *endpoint_iterator;
		socket_ptr->async_connect(endpoint,
			boost::bind(&NtripSocket::handle_connect, this,
				boost::asio::placeholders::error, ++endpoint_iterator));

	}
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error " << url.sanitised() << " handle_resolve : " << err.message() << "\n";
		if ( err != boost::asio::error::operation_aborted )
			delayed_reconnect();   
	}    
}

void NtripSocket::handle_connect(const boost::system::error_code& err,
	tcp::resolver::iterator endpoint_iterator)
{
	//BOOST_LOG_TRIVIAL(debug) << "Connect Completed.\n";
	if (!err)
	{
		if (url.protocol == "https")
		{
			_sslsocket->async_handshake(boost::asio::ssl::stream_base::client,
								boost::bind(&NtripSocket::handle_sslhandshake, this,
								boost::asio::placeholders::error));
			return;    
		}
	
		// The connection was successful. Send the request.
		boost::asio::async_write(*_socket, request,
			boost::bind(&NtripSocket::handle_write_request, this,
			boost::asio::placeholders::error));
	}
	else if (endpoint_iterator != tcp::resolver::iterator())
	{
		// The connection failed. Try the next endpoint in the list.
		tcp::endpoint endpoint = *endpoint_iterator;
		socket_ptr->async_connect(endpoint,
			boost::bind(&NtripSocket::handle_connect, this,
			boost::asio::placeholders::error, ++endpoint_iterator));
	}
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error " << url.sanitised() << " handle_connect : " << err.message() << "\n";
		if ( err != boost::asio::error::operation_aborted )
			delayed_reconnect();  
	} 
}

void NtripSocket::handle_sslhandshake(const boost::system::error_code& err)
{
	//BOOST_LOG_TRIVIAL(debug) << "SSL Handshake Completed.\n";

	if (!err)
	{
		// The connection was successful. Send the request.
		boost::asio::async_write(*_sslsocket, request,
			boost::bind(&NtripSocket::handle_write_request, this,
			boost::asio::placeholders::error));            
	}
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error " << url.sanitised() << " handle_sslhandshake : " << err.message() << "\n";
		if ( err != boost::asio::error::operation_aborted )
			delayed_reconnect();        
	}      
}

void NtripSocket::handle_write_request(const boost::system::error_code& err)
{
	//BOOST_LOG_TRIVIAL(debug) << "Client Write Request Completed.\n";
	
	if (!err)
	{
		if (url.protocol == "https")
		{
			// Read the response status line.
			boost::asio::async_read_until(*_sslsocket, downloadBuf, "\r\n\r\n",
				boost::bind(&NtripSocket::handle_request_response, this,
				boost::asio::placeholders::error));            
		}
		else
		{
			// Read the response status line.
			boost::asio::async_read_until(*_socket, downloadBuf, "\r\n\r\n",
				boost::bind(&NtripSocket::handle_request_response, this,
				boost::asio::placeholders::error));
		}
	}
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error " << url.sanitised() << " handle_write_request : " << err.message() << "\n";
		if ( err != boost::asio::error::operation_aborted )
			delayed_reconnect();    
	}
}  

void NtripSocket::handle_request_response(const boost::system::error_code& err)
{  
	//BOOST_LOG_TRIVIAL(debug) << "Client Read Request Completed.\n";
	if (!err)
	{
		std::vector<char> responseVec;
		responseVec.resize(downloadBuf.size());
		buffer_copy(boost::asio::buffer(responseVec), downloadBuf.data());        
		
		response_string = "";
		response_string.assign(responseVec.begin(), responseVec.end());
		std::size_t pos = response_string.find("\r\n\r\n");
		if (pos == std::string::npos)
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
		std::string bufStr = response_string;
		std::stringstream response_stream(bufStr);
		std::string http_version;
		response_stream >> http_version;
		unsigned int status_code;
		response_stream >> status_code;
		std::string status_message;
		std::getline(response_stream, status_message);
		if (!response_stream || http_version.substr(0, 5) != "HTTP/")
		{
			if( logHttpSentReceived )
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
		if (status_code != 200)
		{
			if( logHttpSentReceived )
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
		
		connectedTime = boost::posix_time::from_time_t(std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()));
		isConnected = true;

		if ( disconnectionCount == 0 )
		{
			networkLog("Initial Connection.");
		}
		else
		{
			boost::posix_time::time_duration downTime = connectedTime - disconnectedTime;
			boost::posix_time::time_duration totalTime = connectedTime - startTime;
			disconnectedDuration += downTime;
			
			std::stringstream message;
			message << "connected, down time minutes : ";
			message << downTime.total_milliseconds()/(60.0*1000);
			double ratio = (double)connectedDuration.total_milliseconds() /(double)totalTime.total_milliseconds();
			message << ", ratio (uptime_total/total) : " << ratio;
			message << ", total number of disconnections : " << disconnectionCount;
			double meanReconn = (double)disconnectedDuration.total_milliseconds()/(60.0*1000.0*disconnectionCount);
			message << ", mean re-connection duration minutes : " << meanReconn;
			networkLog(message.str());
			//BOOST_LOG_TRIVIAL(debug) << message.str() << std::endl;
		}
	
		boost::asio::socket_base::keep_alive option(true);
		socket_ptr->set_option(option); 
		connected();

	}
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error " << url.sanitised() << " handle_request_response : " << err.message() << "\n";
		if ( err != boost::asio::error::operation_aborted )
			delayed_reconnect();      
	} 
}

void NtripSocket::handle_read_message(const boost::system::error_code& err)
{
	if (!err )
	{
		//readMessage();
	}
	else
	{
		BOOST_LOG_TRIVIAL(error) << "Error " << url.sanitised() << " handle_read_message : " << err.message() << "\n";
		if ( err != boost::asio::error::operation_aborted )
			delayed_reconnect();  
	}
}

void NtripSocket::logChunkError()
{
	if( numberValidChunks == 0 )
		return;
	
	numberErroredChunks++;
	std::stringstream message;
	message << "HTTP chunk error, number of errors : ";
	message << numberErroredChunks;
	message << ", number without error : " << numberValidChunks;
	message << ", ratio (error/total) : " << (double)numberErroredChunks /(double)(numberErroredChunks+numberValidChunks);
	messageChunkLog(message.str());      
}

void NtripSocket::printChunkHex(std::vector<char> chunk)
{
	BOOST_LOG_TRIVIAL(debug) << "HTTP Chunk Data : ";

	for (int i = 0; i < chunk.size(); i++)
	{
		if( i % 10 == 0 )
			BOOST_LOG_TRIVIAL(debug) << std::endl;            
		printf("%02x ",(unsigned char)chunk[i]);
	}
	BOOST_LOG_TRIVIAL(debug) << std::endl; 
}

void NtripSocket::start_read()
{
	//BOOST_LOG_TRIVIAL(debug) << "NtripSocket::start_read\n";
	//BOOST_LOG_TRIVIAL(debug) << "Downloading, length : " << content_length << std::endl;
	//BOOST_LOG_TRIVIAL(debug) << "downloadBuf.size() : " << downloadBuf.size() << std::endl; 
	if (url.protocol == "https")
	{
			// Start reading remaining data until EOF.
		boost::asio::async_read(*_sslsocket, downloadBuf,
			boost::asio::transfer_at_least(1),
			boost::bind(&NtripSocket::read_content, this,
				boost::asio::placeholders::error));          
	}
	else
	{
		// Start reading remaining data until EOF.
		boost::asio::async_read(*_socket, downloadBuf,
			boost::asio::transfer_at_least(1),
			boost::bind(&NtripSocket::read_content, this,
				boost::asio::placeholders::error));
	}     
}

void NtripSocket::read_content(const boost::system::error_code& err)
{
	//BOOST_LOG_TRIVIAL(debug) << "NtripSocket::read_content\n";
	//BOOST_LOG_TRIVIAL(debug) << "Downloading, length : " << content_length << std::endl;
	//BOOST_LOG_TRIVIAL(debug) << "downloadBuf.size() : " << downloadBuf.size() << std::endl;     
	if( downloadBuf.size() == content_length )
	{
		std::vector<char> content(downloadBuf.size());
		buffer_copy(boost::asio::buffer(content), downloadBuf.data());
		readContentDownloaded(content);
		return;
	}
	
	if ( err )
	{
		BOOST_LOG_TRIVIAL(error) << "Error " << url.sanitised() << " read_chunked_stream : " << err.message() << "\n";
		if ( err != boost::asio::error::operation_aborted )
			delayed_reconnect();
		return;
	}    
	
	if (url.protocol == "https")
	{
			// Start reading remaining data until EOF.
		boost::asio::async_read(*_sslsocket, downloadBuf,
			boost::asio::transfer_at_least(1),
			boost::bind(&NtripSocket::read_content, this,
				boost::asio::placeholders::error));          
	}
	else
	{
		// Start reading remaining data until EOF.
		boost::asio::async_read(*_socket, downloadBuf,
			boost::asio::transfer_at_least(1),
			boost::bind(&NtripSocket::read_content, this,
				boost::asio::placeholders::error));
	}     
}

void NtripSocket::start_read_stream()
{
	if (url.protocol == "https")
	{
			// Start reading remaining data until EOF.
		boost::asio::async_read(*_sslsocket, downloadBuf,
			boost::asio::transfer_at_least(1),
			boost::bind(&NtripSocket::read_chunked_stream, this,
				boost::asio::placeholders::error));          
	}
	else
	{
		// Start reading remaining data until EOF.
		boost::asio::async_read(*_socket, downloadBuf,
			boost::asio::transfer_at_least(1),
			boost::bind(&NtripSocket::read_chunked_stream, this,
				boost::asio::placeholders::error));
	}     
}


void NtripSocket::read_chunked_stream(const boost::system::error_code& err)
{
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
	

	
	
	std::vector<char> chunk;
	std::istream messStream(&downloadBuf);
	unsigned int sz = downloadBuf.size();
	
	char c_i, c_j;
	unsigned int i = 0;
	
	while(true)
	{
		
		if ( chunked_message_length == 0 )
		{
			chunk.clear();
			messStream.read(&c_i,1);
			chunk.push_back(c_i);
			for (i++; i < sz; i++)
			{
				messStream.read(&c_j,1);
				chunk.push_back(c_j);
				if( c_i == '\r' && c_j == '\n')
				{
					break;
				}
				c_i = c_j;
			}
		
			if ( i >= sz )
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
			std::string messStr(&chunk[0]);
			messStr = messStr.substr(0,messStr.length()-2);
			// NTRIP 2 allows for an extended header.
			std::size_t ext = messStr.find(";");
			if ( ext != string::npos )
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
				if( chunked_message_length > 1000000 )
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
			// Read the second chunk with the message.
			
			// Buffer position for the end of the message body chunk.
			int bodyPos = i+chunked_message_length+2;
			
			// There is not enough data to read the message body.
			if( sz < bodyPos )
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
			if( chunk[chunked_message_length]  == '\r' && chunk[chunked_message_length+1] == '\n' )
			{
				numberValidChunks++;
				
				// Remove the '\r\n' from the data vector.
				chunk.pop_back();
				chunk.pop_back();
				
				finishedReadingStream = dataChunkDownloaded(chunk);
				if( finishedReadingStream )
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
	
	if ( err )
	{
		BOOST_LOG_TRIVIAL(error) << "Error " << url.sanitised() << " read_chunked_stream : " << err.message() << "\n";
		if ( err != boost::asio::error::operation_aborted )
			delayed_reconnect();
		return;
	}    
	
	if (url.protocol == "https")
	{
			// Start reading remaining data until EOF.
		boost::asio::async_read(*_sslsocket, downloadBuf,
			boost::asio::transfer_at_least(1),
			boost::bind(&NtripSocket::read_chunked_stream, this,
				boost::asio::placeholders::error));          
	}
	else
	{
		// Start reading remaining data until EOF.
		boost::asio::async_read(*_socket, downloadBuf,
			boost::asio::transfer_at_least(1),
			boost::bind(&NtripSocket::read_chunked_stream, this,
				boost::asio::placeholders::error));
	}    
}
