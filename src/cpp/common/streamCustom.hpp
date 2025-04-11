
#pragma once


#include "common/packetStatistics.hpp"
#include "common/customDecoder.hpp"
#include "common/streamParser.hpp"
#include "common/streamObs.hpp"

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

