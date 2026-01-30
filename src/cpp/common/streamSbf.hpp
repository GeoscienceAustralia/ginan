#pragma once

#include "common/packetStatistics.hpp"
#include "common/sbfDecoder.hpp"
#include "common/streamObs.hpp"
#include "common/streamParser.hpp"

struct SbfParser : Parser, SbfDecoder, PacketStatistics
{
    void parse(std::istream& inputStream);

    string parserType() { return "SbfParser"; }
};
