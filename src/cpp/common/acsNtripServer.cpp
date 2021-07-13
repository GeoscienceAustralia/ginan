#include "acsNtripServer.hpp"

void NtripServer::NtripClient::negotiationError()
{
	char response[] = "HTTP/1.0 403 Forbidden\r\n";
	boost::asio::write(*this, boost::asio::buffer(&response[0], strlen(response)));
	this->shutdown();
	throw std::runtime_error( "Error in socket negociation.");
}

void NtripServer::NtripClient::initialize()
{
const int max_length = 2048;  
try
{
		BOOST_LOG_TRIVIAL(debug) << "Incomming connection, handshake.\n";
		parentServer.listenMtx.unlock();
	
		handshake(boost::asio::ssl::stream_base::server);      
	
		boost::system::error_code error;
		char data[max_length];
		
		// Read first line of connection negociation.
		size_t length = read_some(boost::asio::buffer(data), error);
		if (error)
		{
			throw boost::system::system_error(error);
		}
		
		string lineStr;
		vector<std::string> tokens;
		std::stringstream cResStream(data);
		
		// Check the first line of the connection negociation.
		std::getline(cResStream,lineStr);
		BOOST_LOG_TRIVIAL(debug) << "Line 1 : " << lineStr << std::endl;
		boost::split(tokens,lineStr,boost::is_any_of(" \r"),boost::token_compress_on);

		if( tokens.size() < 3)
			negotiationError();
		
		if( tokens[0] == "GET" )
			negotiationError();
		
		if( tokens[1] == parentServer.mountPoint )
			negotiationError();
		
		if( tokens[2] == "HTTP/1.0" )
			negotiationError();    
		tokens.clear();
		
		// Check the second line of the connection negociation.
		std::getline(cResStream,lineStr);
		BOOST_LOG_TRIVIAL(debug) << "Line 2 : " << lineStr << std::endl;
		boost::split(tokens,lineStr,boost::is_any_of(" \r"),boost::token_compress_on);
		if( tokens.size() < 3)
			negotiationError();
		
		if( tokens[0].compare("User-Agent:") != 0 )
			negotiationError();
		
		if( tokens[1].compare("NTRIP") != 0 )
			negotiationError();
		
		if( tokens[2].compare("ACS/1.0") != 0 )
			negotiationError();     
		tokens.clear();       
		
		// Check the third line of the connection negociation.
		std::getline(cResStream,lineStr);
		BOOST_LOG_TRIVIAL(debug) << "Line 3 : " << lineStr << std::endl;
		boost::split(tokens,lineStr,boost::is_any_of(" \r"),boost::token_compress_on);
		if( tokens.size() < 2)
			negotiationError();
		
		if( tokens[0].compare("Host:") != 0 )
			negotiationError();
		
		// Doesn't currently check the host name.
		//if( tokens[1].compare(<This Host Name>) != 0 )
		//   throw std::runtime_error( "Error in socket negociation.");
		tokens.clear();       
		
		// Check the fourth line of the connection negociation.
		std::getline(cResStream,lineStr);
		BOOST_LOG_TRIVIAL(debug) << "Line 4 : " << lineStr << std::endl;
		boost::split(tokens,lineStr,boost::is_any_of(" \n"),boost::token_compress_on);

		if( tokens.size() < 3)
			negotiationError();
		
		if( tokens[0].compare("Authorization:") != 0 )
			negotiationError();

		if( tokens[1].compare("Basic") != 0 )
			negotiationError();
		
		//TODO:Add user name and password.
		string encodedUserPass = tokens[2];
		BOOST_LOG_TRIVIAL(debug) << "Decoding and testing of user name and password not done yet!\n";
		tokens.clear(); 
		
		char response[] = "ICY 200 OK\r\n";
		boost::asio::write(*this, boost::asio::buffer(&response[0], strlen(response)));
				
		
		BOOST_LOG_TRIVIAL(debug) << "Handshake complete connection negociated.\n";
	}
	catch (std::exception& e)
	{
		std::cerr << "Error in Socket Client: " << e.what() << "\n";
		deleteClient = true;
		parentServer.listenMtx.unlock();
	}           

	readyForPrint = true;
}

void NtripServer::NtripClient::printMessage(std::vector<uint8_t> data)
{
	try
	{    
		printMtx.lock();
		readyForPrint = false;
		boost::asio::write(*this, boost::asio::buffer(&data[0], data.size()));
		readyForPrint = true;
		printMtx.unlock();
	}
	catch (std::exception& e)
	{
		std::cerr << "Error in Socket Client: " << e.what() << "\n";
		deleteClient = true;       
	}               
}


void NtripServer::sendMessages(std::istream& inputStream)
{
	auto it = clients.begin();
	while( it != clients.end())
	{
		auto client = *it;
		if(client->deleteClient)
			it = clients.erase(it);
		else
			it++;
	}
	
	inputStream.seekg (0, inputStream.end);
	int length = inputStream.tellg();
	
	if( length == 0)
	{
		BOOST_LOG_TRIVIAL(debug) << "Message length zero.\n";
		return;
	}
	
	inputStream.seekg (0, inputStream.beg);    
	std::vector<uint8_t> data;
	data.resize(length);
	inputStream.read((char *)&data[0],length);
	for(auto client : clients)
	{
		if(client->readyForPrint)
		{
			BOOST_LOG_TRIVIAL(debug) << "Sending message to client.\n";
			std::thread(&NtripServer::NtripClient::printMessage,client,data).detach();
		}   
	}
}

void NtripServer::broadcastLoop()
{
	using std::chrono::system_clock;
	int cnt = 0;
	int milliSec = (int)round(loopDelaySeconds*1000);
	auto pause = std::chrono::milliseconds(milliSec);
	system_clock::time_point DelayTill = system_clock::now();
	DelayTill += pause;
	
	while (!shutDownServer)
	{
		try 
		{
			cnt++;
			std::stringstream messStream;
	
			RtcmEncoder::SSREncoder rtcmSsrEnc;
			//rtcmSsrEnc.encodeSsrComb(nav, E_Sys::GPS, false);
			//rtcmSsrEnc.encodeSsrPhase(nav, E_Sys::GPS, false);
			//rtcmSsrEnc.encodeSsrCode(nav, E_Sys::GPS, false);
			rtcmSsrEnc.encodeWriteMessages(messStream);

			sendMessages(messStream);
			
			std::this_thread::sleep_until(DelayTill);
			DelayTill += pause;
		}
		catch (std::exception& e)
		{
			std::cerr << "Error in Sending Message : " << e.what() << "\n";
		}
	}
}

void NtripServer::startServer()
{
	// The acceptor must be started prior to starting io_service.
	// io_service is required for async_accept method of acceptor.
	// async_accept is required for a clean error free shutdown.
	
	// For the server to run transparently in the background the
	// thread that starts it must continue.
	
	// listenMtx, mutex ensure async_accept is called once per client
	//            or incomming connection.
	// shutDownMtx, mutex ensures the listening and broadcasting threads
	//              join prior to object destruction.
	// startMtx, mutex ensures the acceptor has been called prior to starting
	//           io_service as this is a requirement of boost.   
	
	// Mutex required for imediate transparent clean shutdown.
	listenMtx.lock();
	shutDownMtx.lock();
	startMtx.lock();
	std::thread(&NtripServer::monitorThreads,this).detach();
}

void NtripServer::startService()
{
	// This method captures the thread.
	io_service.run();
}

void NtripServer::monitorThreads()
{
	// Required for clean transparent shutdown.
	std::thread listenThrd(&NtripServer::broadcastLoop,this);
	std::thread broadcastThrd(&NtripServer::startListening,this);
	startMtx.lock();
	std::thread serviceThrd(&NtripServer::startService,this);

	serviceThrd.join();
	listenThrd.join();
	broadcastThrd.join();
	shutDownMtx.unlock();
}
	
void NtripServer::startListening()
{   
	using boost::asio::ip::tcp;    
	try
	{
		boost::asio::ssl::context ssl_context(boost::asio::ssl::context::sslv23);

		ssl_context.set_options(
			boost::asio::ssl::context::default_workarounds
			| boost::asio::ssl::context::no_sslv2);

		// Use the following command to connect to port using SSL.
		// openssl s_client -connect 127.0.0.1:<port>
		
		// use the following to determine range of ports availble without altering firewall.
		// cat /proc/sys/net/ipv4/ip_local_port_range

		// Use the following linux command to generate the certificate and key file.
		// There is no password on the SSL certificate.
		// openssl req -x509 -newkey  rsa:4096 -keyout key.pem -out cert.pem -days 365 -nodes -subj '/CN=localhost'
		ssl_context.use_certificate_chain_file(sslCertifateFileName);
		ssl_context.use_private_key_file(sslPrivateKeyFileName, boost::asio::ssl::context::pem);

		tcp::acceptor a(io_service, tcp::endpoint(tcp::v4(), portNumber));

		bool firstPass = true;
		for (;;)
		{
			BOOST_LOG_TRIVIAL(debug) << "Listening on port : " << portNumber << std::endl;
			std::shared_ptr<NtripClient> client (new NtripClient(*this,io_service, ssl_context));
			
			BOOST_LOG_TRIVIAL(debug) << "Listen, accept started.\n";
			a.async_accept(client->next_layer(),boost::bind( &NtripClient::initialize, client));
			
			if ( firstPass )
				startMtx.unlock();
			
			BOOST_LOG_TRIVIAL(debug) << "Listen, passed accept.\n";
			
			// Using this method the thread gets locked in accept method and does not shutdown clean.
			// a.accept(client->next_layer());
			// std::thread(&NtripServer::NtripClient::initialize,client.get()).detach();
			
			// Thread control required for clean shutdown.
			listenMtx.lock();
			BOOST_LOG_TRIVIAL(debug) << "Listen passed mutex.\n";
			if( shutDownServer )
				break;

			
			clients.push_back(std::move(client));
		}
	}
	catch (std::exception& e)
	{
		BOOST_LOG_TRIVIAL(debug) << "NtripServer::startListening(), Error\n";
		std::cerr << "Exception: " << e.what() << "\n";
	}    
	
}

NtripServer::~NtripServer()
{
	// Required for clean shutdown.
	BOOST_LOG_TRIVIAL(debug) << "Test 1.\n";
	io_service.stop();
	BOOST_LOG_TRIVIAL(debug) << "Test 2.\n";
	shutDownServer = true;
	listenMtx.unlock();
	BOOST_LOG_TRIVIAL(debug) << "Test 3.\n";
	shutDownMtx.lock();
	BOOST_LOG_TRIVIAL(debug) << "Test 4.\n";
	auto it = clients.begin();
	while( it != clients.end())
	{
		BOOST_LOG_TRIVIAL(debug) << "Test 5.\n";
		auto client = *it;
		if(!client->deleteClient)
			client->shutdown();
		it = clients.erase(it);
	}
	BOOST_LOG_TRIVIAL(debug) << "Exit.\n";
}
