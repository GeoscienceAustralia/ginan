#ifndef NTRIPSOURCETABLE_H
#define NTRIPSOURCETABLE_H

#include <mutex>
#include <vector>

#include <boost/algorithm/string/split.hpp>

#include "ntripSocket.hpp"
#include "enums.h"

struct SourceTableEntry
{
	struct RtcmMessageData
	{
		//RtcmMessageType messageType;
		int messageType;
		int broadcastFrequency;
	};
	
	string mountPoint;
	string location;
	vector<RtcmMessageData> messageData;
};


struct NtripSourceTable :  NtripSocket
{
	NtripSourceTable(
		string url_str);
	
	// This function will block until source table has been read.
	void getSourceTable();
	
	string sourceTableString;
	vector<SourceTableEntry> sourceTableData;
	vector<string> getStreamMounts();
private:
	std::mutex getSourceTableMtx; 
	
	void connected() override;
	
	bool dataChunkDownloaded(
		vector<char> dataChunk) override; 
		
	void readContentDownloaded(
		vector<char> content) override;
};

#endif
