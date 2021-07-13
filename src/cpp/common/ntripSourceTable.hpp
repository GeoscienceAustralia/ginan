#ifndef NTRIPSOURCETABLE_H
#define NTRIPSOURCETABLE_H

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
	
	std::string mountPoint;
	std::string location;
	std::vector<RtcmMessageData> messageData;
};


struct NtripSourceTable :  NtripSocket
{
	
public:
	NtripSourceTable(const std::string& url_str);
	
	// This function will block until source table has been read.
	void getSourceTable();
	
	std::string sourceTableString;
	std::vector<SourceTableEntry> sourceTableData;
	
private:
	std::mutex getSourceTableMtx; 
	void connected() override;
	bool dataChunkDownloaded(vector<char> dataChunk) override; 
	void readContentDownloaded(std::vector<char> content) override;
	
};

#endif // NTRIPSOURCETABLE_H
