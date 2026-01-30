#include "common/streamSbf.hpp"
#include <map>
#include "common/packetStatistics.hpp"
#define CLEAN_UP_AND_RETURN_ON_FAILURE \
                                       \
    if (inputStream.fail())            \
    {                                  \
        inputStream.clear();           \
        inputStream.seekg(pos);        \
        return;                        \
    }

void SbfParser::parse(std::istream& inputStream)
{
    // std::cout << "Parsing sbf" << "\n";

    while (inputStream)
    {
        int           pos;
        unsigned char c1 = 0;
        unsigned char c2 = 0;
        while (true)
        {
            pos = inputStream.tellg();

            // move c2 back one and replace, (check one byte at a time, not in pairs)
            c1 = c2;
            inputStream.read((char*)&c2, 1);

            if (inputStream)
            {
                if (c1 == SBF_PREAMBLE1 && c2 == SBF_PREAMBLE2)
                {
                    break;
                }
            }
            else
            {
                return;
            }
            nonFrameByteFound(c2);
        }
        CLEAN_UP_AND_RETURN_ON_FAILURE;

        preambleFound();

        unsigned short int crcRead = 0;
        inputStream.read((char*)&crcRead, 2);
        CLEAN_UP_AND_RETURN_ON_FAILURE;

        unsigned short int id = 0;
        inputStream.read((char*)&id, 2);
        CLEAN_UP_AND_RETURN_ON_FAILURE;
        id = id & 0x1FFF;

        unsigned short int payload_length = 0;
        inputStream.read((char*)&payload_length, 2);
        CLEAN_UP_AND_RETURN_ON_FAILURE;

        vector<unsigned char> payload(payload_length - 8);
        inputStream.read((char*)payload.data(), payload_length - 8);
        CLEAN_UP_AND_RETURN_ON_FAILURE;  // Read the frame data (include the header)

        // todo: calculate crcRead

        if (0)
        {
            checksumFailure();

            inputStream.seekg(pos);

            continue;
        }

        checksumSuccess();

        decode(id, payload);

        if (obsListList.size() > 2)
        {
            return;
        }
    }
    inputStream.clear();
}
