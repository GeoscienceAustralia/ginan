
// #pragma GCC optimize ("O0")

#include "streamNtrip.hpp"


void NtripStream::getData()
{
	receivedDataBufferMtx.lock();
	{
		//if ( receivedDataBuffer.size() > 0 )
		//    BOOST_LOG_TRIVIAL(debug) << "NtripStream::getData(), receivedDataBuffer.size() = " << receivedDataBuffer.size() << std::endl;
		receivedData.insert(receivedData.end(), receivedDataBuffer.begin(), receivedDataBuffer.end());
		receivedDataBuffer.clear();
	}
	receivedDataBufferMtx.unlock();
}

bool NtripStream::dataChunkDownloaded(
	vector<char> dataChunk)
{
	receivedDataBufferMtx.lock();
	{
		receivedDataBuffer.insert(receivedDataBuffer.end(), dataChunk.begin(), dataChunk.end());
	}
	receivedDataBufferMtx.unlock(); 
	
	return false;
}

void NtripStream::connected()
{
	start_read(true);
}
