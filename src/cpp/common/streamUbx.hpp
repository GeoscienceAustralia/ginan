
#pragma once


#include "packetStatistics.hpp"
#include "streamParser.hpp"
#include "ubxDecoder.hpp"
#include "streamObs.hpp"

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

