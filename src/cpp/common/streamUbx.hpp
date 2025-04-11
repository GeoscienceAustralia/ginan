
#pragma once


#include "common/packetStatistics.hpp"
#include "common/streamParser.hpp"
#include "common/ubxDecoder.hpp"
#include "common/streamObs.hpp"

#define UBX_PREAMBLE1 0xB5
#define UBX_PREAMBLE2 0x62

struct UbxParser : Parser, UbxDecoder, PacketStatistics
{
	void parse(
		std::istream& inputStream);
	
	string parserType()
	{
		return "UbxParser";
	}
};

