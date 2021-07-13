#include "acsNtripBroadcast.hpp"


void NtripBroadcaster::stopBroadcast()
{
	for ( auto[label,outStream] : ntripUploadStreams )
		outStream->disconnect();
	
	ntripUploadStreams.clear();
}

std::string NtripBroadcaster::NtripUploadClient::getJsonNetworkStatistics(system_clock::time_point epochTime)
{
	auto time = std::chrono::system_clock::to_time_t(epochTime);
	
	std::stringstream networkJson;
	networkJson << "{";
	networkJson << "\"Station\": \"" << url.path.substr(1,url.path.length()) << "\",";
	networkJson << "\"Epoch\": \"" << std::put_time( std::localtime( &time ), "%F %X" ) << "\","; 
	networkJson << "\"Network\": \"" <<  acsConfig.analysis_agency << "\",";
	networkJson << "\"Downloading\": \"false\",";
	auto timeNow = boost::posix_time::from_time_t(system_clock::to_time_t(system_clock::now()));
	boost::posix_time::time_duration totalTime = timeNow - startTime;
	double connRatio = (double)connectedDuration.total_milliseconds() /(double)totalTime.total_milliseconds();
	
	double meanReconn = 0;
	if ( disconnectionCount != 0 )
		meanReconn = (double)disconnectedDuration.total_milliseconds()/(60.0*1000.0*disconnectionCount);

	networkJson << "\"Disconnects\": " << disconnectionCount << ",";
	networkJson << "\"MeanDowntime\": " << meanReconn << ",";
	networkJson << "\"Connected ratio\": " << connRatio << ",";

	double chunkRatio = 0;
	numberErroredChunks = numberChunksSent - numberValidChunks;
	if ( numberChunksSent != 0 )
		chunkRatio = (double)numberErroredChunks/(double)(numberChunksSent);

	networkJson << "\"Chunks\": " << numberChunksSent << ",";
	networkJson << "\"ChunkErrors\": " <<  numberErroredChunks << ",";
	networkJson << "\"Chunk error ratio\": " << chunkRatio << ",";

	networkJson << "\"RtcmExtraBytes\": " << 0 << ",";
	networkJson << "\"RtcmFailCrc\": " << 0 << ",";
	networkJson << "\"RtcmPassedCrc\": " << 0 << ",";
	networkJson << "\"RtcmDecoded\": " << 0 << ",";
	networkJson << "\"RtcmPreamble\": " << 0 << ",";
	

	networkJson << "\"RtcmFailedCrcToPreambleRatio\": " << 0.0;
	networkJson << "}";
	
	return networkJson.str();
}

void NtripBroadcaster::NtripUploadClient::sendMessages(bool useSsrOut)
{
	for ( auto RtcmMess : rtcmMessagesTypes )
	{
		switch( RtcmMess )
		{
			case +RtcmMessageType::GPS_SSR_COMB_CORR :
				encodeSsrComb(E_Sys::GPS,useSsrOut);
				break;
			case +RtcmMessageType::GAL_SSR_COMB_CORR :
				encodeSsrComb(E_Sys::GAL,useSsrOut);
				break;
			case +RtcmMessageType::GPS_SSR_PHASE_BIAS :
				encodeSsrPhase(E_Sys::GPS,useSsrOut);
				break;
			case +RtcmMessageType::GAL_SSR_PHASE_BIAS :
				encodeSsrPhase(E_Sys::GAL,useSsrOut);
				break;
			case +RtcmMessageType::GPS_SSR_CODE_BIAS :
				encodeSsrCode(E_Sys::GPS,useSsrOut);
				break;
			case +RtcmMessageType::GAL_SSR_CODE_BIAS :
				encodeSsrCode(E_Sys::GAL,useSsrOut);
				break; 
			default:
				BOOST_LOG_TRIVIAL(error) << "Error, attempting to upload incorrect message type.\n";
		}
}
	std::stringstream messStream;
	encodeWriteMessages(messStream);

	messStream.seekg (0, messStream.end);
	int length = messStream.tellg();
			
	//BOOST_LOG_TRIVIAL(debug) << "\nCalled, NtripBroadcaster::NtripUploadClient::sendMessages(), MessageLength : " << length << std::endl;
	if( length != 0)
	{    
		outMessagesMtx.lock();
		std::ostream chunkedStream(&outMessages);
		chunkedStream << std::uppercase << std::hex << length << "\r\n";
		
		messStream.seekg (0, messStream.beg);
		messStream.seekg (0, messStream.beg);    
		std::vector<char> data;
		data.resize(length);
		messStream.read(&data[0],length);
		chunkedStream.write(&data[0],length);
		chunkedStream << "\r\n";        
	
		if (url.protocol == "https")
		{
			boost::asio::async_write(*_sslsocket, outMessages,
				boost::bind(&NtripBroadcaster::NtripUploadClient::writeResponse, this,
				boost::asio::placeholders::error));          
		}
		else
		{
			boost::asio::async_write(*_socket, outMessages,
				boost::bind(&NtripBroadcaster::NtripUploadClient::writeResponse, this,
				boost::asio::placeholders::error));
		}
		numberValidChunks++;
	}
}

void NtripBroadcaster::NtripUploadClient::connected()
{
	// Although there should be no downloading attempting to download monitors the socket connection.
	start_read_stream();
}


void NtripBroadcaster::NtripUploadClient::writeResponse(const boost::system::error_code& err)
{
	//BOOST_LOG_TRIVIAL(debug) << "RTCM, NtripUploadClient::writeResponse\n";
	if (err)
	{
		outMessages.consume(outMessages.size());
		outMessagesMtx.unlock();
		BOOST_LOG_TRIVIAL(error) << "Error " << url.sanitised() << " NtripUploadClient::writeResponse : " << err.message() << "\n";
		delayed_reconnect();
		return;
	}
	else
	{
		numberChunksSent++;
		outMessagesMtx.unlock();
	}
}


void NtripBroadcaster::sendMessages(bool useSsrOut)
{
	for ( auto[label,outStream] : ntripUploadStreams )
		outStream->sendMessages(useSsrOut);    
}


void NtripBroadcaster::NtripUploadClient::traceMakeNetworkOverview(Trace& trace)
{
	bool printToTerminal = false;
	std::stringstream message;
	message << url.path;
	if ( !isConnected && disconnectionCount == 0 && (numberValidChunks+disconnectionCount) == 0 )
	{
		message << ", Network, Has Not Connected.";
		message << std::endl;
		message << std::endl;            
	}
	else
	{
		
		message << ", Network, Number Disconnects : " << disconnectionCount;
		double connRatio = 1;
		if( disconnectionCount != 0 )
		{
			boost::posix_time::time_duration totalTime = connectedTime - startTime;
			double connRatio = (double)connectedDuration.total_milliseconds() /(double)totalTime.total_milliseconds();
			double meanReconn = (double)disconnectedDuration.total_milliseconds()/(60.0*1000.0*disconnectionCount);
			
			message << ", Mean Re-connection Time (min) : " << meanReconn;
			message << ", Ratio Connectioned Time : " << connRatio;
		}
		if( connRatio < 0.99 )
			printToTerminal = true;

		
		message << std::endl;
		message << url.path;
		numberErroredChunks = numberChunksSent - numberValidChunks;
		message << ", Number Chunks : " << numberValidChunks;
		message << ", Number Errored Chunks : " << numberErroredChunks;
		
		double chunkRatio = 0;
		if ( numberValidChunks != 0 )
		{
			chunkRatio = (double)numberErroredChunks/(double)(numberValidChunks);
			message << ", Ratio Error Chunks : " << chunkRatio;
		}
		if( chunkRatio > 0.01 )
			printToTerminal = true;        
		
		message << std::endl;
		message << std::endl;
	}
	
	// The variable reduces the amount of printing to the terminal to only
	// streams with high error.
	if ( printToTerminal && print_stream_statistics)
		BOOST_LOG_TRIVIAL(debug) << message.str();
	
	
	std::string messLine;
	while(std::getline(message, messLine)) 
		tracepde(3,trace,messLine+"\n"); 
}
