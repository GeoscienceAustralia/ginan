#pragma once
// #pragma GCC optimize ("O0")

#include <map>
#include <vector>
#include "common/enums.h"
#include "common/gTime.hpp"
#include "common/icdDecoder.hpp"
#include "common/streamObs.hpp"

using std::map;
using std::vector;

#define SBF_PREAMBLE1 0x24
#define SBF_PREAMBLE2 0x40

struct SbfDecoder : ObsLister, IcdDecoder
{
    unsigned int lastTimeTag = 0;
    GTime        lastTime;

    string recId;

    string raw_sbf_filename;

    ObsList sbfObsList;
    GTime   lastObstime;

    void decode(unsigned short int id, vector<unsigned char>& data);
    void decodeMeasEpoch(GTime time, vector<unsigned char>& data);
    void decodeEndOfMeas(GTime time);

    void recordFrame(unsigned short int id, vector<unsigned char>& data, unsigned short int crcRead)
    {
        if (raw_sbf_filename.empty())
        {
            return;
        }

        std::ofstream ofs(raw_sbf_filename, std::ofstream::app);

        if (!ofs)
        {
            return;
        }

        // copy the message to the output file
        unsigned char      c1            = SBF_PREAMBLE1;
        unsigned char      c2            = SBF_PREAMBLE2;
        unsigned short int payloadLength = data.size();

        ofs.write((char*)&c1, 1);
        ofs.write((char*)&c2, 1);
        ofs.write((char*)&crcRead, 2);
        ofs.write((char*)&id, 2);
        ofs.write((char*)&payloadLength, 2);
        ofs.write((char*)data.data(), data.size());
    }
};
