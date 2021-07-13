
#include "acsNtripStream.hpp"

#include <boost/system/error_code.hpp>
#include <list>



void NtripStream::getData()
{
	receivedDataBufferMtx.lock();
	//if ( receivedDataBuffer.size() > 0 )
	//    BOOST_LOG_TRIVIAL(debug) << "NtripStream::getData(), receivedDataBuffer.size() = " << receivedDataBuffer.size() << std::endl;
	receivedData.insert(receivedData.end(), receivedDataBuffer.begin(), receivedDataBuffer.end());
	receivedDataBuffer.clear();
	receivedDataBufferMtx.unlock();
}

void NtripStream::connected()
{
	start_read_stream();   
}

bool NtripStream::dataChunkDownloaded(vector<char> dataChunk)
{
	receivedDataBufferMtx.lock();
	for ( int j = 0; j < chunked_message_length; j++ )
		receivedDataBuffer.push_back(dataChunk[j]);
	receivedDataBufferMtx.unlock(); 
	return false;
}
