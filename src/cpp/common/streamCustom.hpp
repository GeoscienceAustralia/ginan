
#pragma once


#include "packetStatistics.hpp"
#include "customDecoder.hpp"
#include "streamParser.hpp"
#include "streamObs.hpp"

#define CUSTOM_PREAMBLE 0xAC

struct CustomParser : Parser, CustomDecoder, PacketStatistics
{
	void parse(
		std::istream& inputStream);

	string parserType()
	{
		return "CustomParser";
	}
};

