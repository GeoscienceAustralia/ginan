#pragma once

#include "common/customDecoder.hpp"
#include "common/packetStatistics.hpp"
#include "common/streamObs.hpp"
#include "common/streamParser.hpp"
#define CUSTOM_PREAMBLE 0xAC

struct CustomParser : Parser, CustomDecoder, PacketStatistics
{
    void parse(std::istream& inputStream);

    string parserType() { return "CustomParser"; }
};
