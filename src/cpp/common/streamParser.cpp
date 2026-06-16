// #pragma GCC optimize ("O0")

#include "common/streamParser.hpp"
#include <map>

multimap<string, StreamParserPtr> streamParserMultimap;
map<string, bool>                 streamDOAMap;

long int streamPos(std::istream& stream)
{
    // 			std::cout << "Closed" << "\n";

    if (stream.bad())
    {
        BOOST_LOG_TRIVIAL(debug) << "Bad input stream";
        return -1;
    }
    if (stream.eof())
    {
        BOOST_LOG_TRIVIAL(debug) << "Input stream has reached the end of file";
        return -1;
    }
    if (stream.fail())
    {
        BOOST_LOG_TRIVIAL(debug) << "Failed to read input stream";
        return -1;
    }

    long int streamPos = stream.tellg();

    if (streamPos < 0)
    {
        BOOST_LOG_TRIVIAL(error) << "Error telling in stream";
        return -1;
    }

    return streamPos;
}
