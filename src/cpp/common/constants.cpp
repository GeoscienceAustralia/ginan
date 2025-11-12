#include "common/constants.hpp"
#include <array>
#include <boost/assign.hpp>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include "common/enums.h"
#include "common/satSys.hpp"

using std::map;
using std::set;

map<E_FType, double> genericWavelength = {
    {F1, CLIGHT / FREQ1},
    {F2, CLIGHT / FREQ2},
    {F5, CLIGHT / FREQ5},
    {F6, CLIGHT / FREQ6},
    {F7, CLIGHT / FREQ7},
    {F8, CLIGHT / FREQ8},
    {B1, CLIGHT / FREQ1_CMP},
    {B3, CLIGHT / FREQ3_CMP},
    {G1, CLIGHT / FREQ1_GLO},
    {G2, CLIGHT / FREQ2_GLO},
    {G3, CLIGHT / FREQ3_GLO},
    {G4, CLIGHT / FREQ4_GLO},
    {G6, CLIGHT / FREQ6_GLO}
};

map<E_Sys, map<E_ObsCode, E_FType>> code2Freq = {
    {E_Sys::GPS,
     {{E_ObsCode::NONE, NONE}, {E_ObsCode::L1C, F1}, {E_ObsCode::L1S, F1}, {E_ObsCode::L1L, F1},
      {E_ObsCode::L1X, F1},    {E_ObsCode::L1P, F1}, {E_ObsCode::L1W, F1}, {E_ObsCode::L1Y, F1},
      {E_ObsCode::L1M, F1},    {E_ObsCode::L1N, F1},

      {E_ObsCode::L2C, F2},    {E_ObsCode::L2D, F2}, {E_ObsCode::L2S, F2}, {E_ObsCode::L2L, F2},
      {E_ObsCode::L2X, F2},    {E_ObsCode::L2P, F2}, {E_ObsCode::L2W, F2}, {E_ObsCode::L2Y, F2},
      {E_ObsCode::L2M, F2},    {E_ObsCode::L2N, F2},

      {E_ObsCode::L5I, F5},    {E_ObsCode::L5Q, F5}, {E_ObsCode::L5X, F5}}},

    {E_Sys::GLO,
     {{E_ObsCode::NONE, NONE},
      {E_ObsCode::L1C, G1},
      {E_ObsCode::L1P, G1},

      {E_ObsCode::L2C, G2},
      {E_ObsCode::L2P, G2},

      {E_ObsCode::L3I, G3},
      {E_ObsCode::L3Q, G3},
      {E_ObsCode::L3X, G3},

      {E_ObsCode::L4A, G4},
      {E_ObsCode::L4B, G4},
      {E_ObsCode::L4X, G4},

      {E_ObsCode::L6A, G6},
      {E_ObsCode::L6B, G6},
      {E_ObsCode::L6X, G6}}},

    {E_Sys::GAL, {{E_ObsCode::NONE, NONE}, {E_ObsCode::L1A, F1}, {E_ObsCode::L1B, F1},
                  {E_ObsCode::L1C, F1},    {E_ObsCode::L1X, F1}, {E_ObsCode::L1Z, F1},

                  {E_ObsCode::L5I, F5},    {E_ObsCode::L5Q, F5}, {E_ObsCode::L5X, F5},

                  {E_ObsCode::L6A, F6},    {E_ObsCode::L6B, F6}, {E_ObsCode::L6C, F6},
                  {E_ObsCode::L6X, F6},    {E_ObsCode::L6Z, F6},

                  {E_ObsCode::L7I, F7},    {E_ObsCode::L7Q, F7}, {E_ObsCode::L7X, F7},

                  {E_ObsCode::L8I, F8},    {E_ObsCode::L8Q, F8}, {E_ObsCode::L8X, F8}}},

    {E_Sys::BDS,
     {{E_ObsCode::NONE, NONE}, {E_ObsCode::L1D, F1}, {E_ObsCode::L1P, F1}, {E_ObsCode::L1X, F1},
      {E_ObsCode::L1S, F1},    {E_ObsCode::L1L, F1}, {E_ObsCode::L1Z, F1}, {E_ObsCode::L1A, F1},
      {E_ObsCode::L1N, F1},

      {E_ObsCode::L2I, B1},    {E_ObsCode::L2Q, B1}, {E_ObsCode::L2X, B1},

      {E_ObsCode::L5D, F5},    {E_ObsCode::L5P, F5}, {E_ObsCode::L5X, F5},

      {E_ObsCode::L6I, B3},    {E_ObsCode::L6Q, B3}, {E_ObsCode::L6X, B3}, {E_ObsCode::L6D, B3},
      {E_ObsCode::L6P, B3},    {E_ObsCode::L6Z, B3}, {E_ObsCode::L6A, B3},

      {E_ObsCode::L7I, F7},    {E_ObsCode::L7Q, F7}, {E_ObsCode::L7X, F7}, {E_ObsCode::L7D, F7},
      {E_ObsCode::L7P, F7},    {E_ObsCode::L7Z, F7},

      {E_ObsCode::L8D, F8},    {E_ObsCode::L8P, F8}, {E_ObsCode::L8X, F8}}},

    {E_Sys::QZS,
     {{E_ObsCode::NONE, NONE}, {E_ObsCode::L1C, F1}, {E_ObsCode::L1E, F1}, {E_ObsCode::L1S, F1},
      {E_ObsCode::L1L, F1},    {E_ObsCode::L1X, F1}, {E_ObsCode::L1Z, F1}, {E_ObsCode::L1B, F1},

      {E_ObsCode::L2S, F2},    {E_ObsCode::L2L, F2}, {E_ObsCode::L2X, F2},

      {E_ObsCode::L5I, F5},    {E_ObsCode::L5Q, F5}, {E_ObsCode::L5X, F5}, {E_ObsCode::L5D, F5},
      {E_ObsCode::L5P, F5},    {E_ObsCode::L5Z, F5},

      {E_ObsCode::L6S, F6},    {E_ObsCode::L6L, F6}, {E_ObsCode::L6X, F6}, {E_ObsCode::L6E, F6},
      {E_ObsCode::L6Z, F6}}},

    {E_Sys::IRN,
     {/* NavIC F1 in the works... */

      {E_ObsCode::NONE, NONE},
      {E_ObsCode::L5A, F5},
      {E_ObsCode::L5B, F5},
      {E_ObsCode::L5C, F5},
      {E_ObsCode::L5X, F5},

      {E_ObsCode::L9A, I9},
      {E_ObsCode::L9B, I9},
      {E_ObsCode::L9C, I9},
      {E_ObsCode::L9X, I9}
     }},

    {E_Sys::SBS,
     {{E_ObsCode::NONE, NONE},
      {E_ObsCode::L1C, F1},

      {E_ObsCode::L5I, F5},
      {E_ObsCode::L5Q, F5},
      {E_ObsCode::L5X, F5}}},

    {E_Sys::LEO,
     {{E_ObsCode::NONE, NONE}, {E_ObsCode::L1C, F1}, {E_ObsCode::L1S, F1}, {E_ObsCode::L1L, F1},
      {E_ObsCode::L1X, F1},    {E_ObsCode::L1P, F1}, {E_ObsCode::L1W, F1}, {E_ObsCode::L1Y, F1},
      {E_ObsCode::L1M, F1},    {E_ObsCode::L1N, F1},

      {E_ObsCode::L2C, F2},    {E_ObsCode::L2D, F2}, {E_ObsCode::L2S, F2}, {E_ObsCode::L2L, F2},
      {E_ObsCode::L2X, F2},    {E_ObsCode::L2P, F2}, {E_ObsCode::L2W, F2}, {E_ObsCode::L2Y, F2},
      {E_ObsCode::L2M, F2},    {E_ObsCode::L2N, F2},

      {E_ObsCode::L5I, F5},    {E_ObsCode::L5Q, F5}, {E_ObsCode::L5X, F5}}}
};

// Map satellite block types to their broadcast frequency bands
// Based on RINEX 4.02 specification and satellite constellation signal plans
map<E_Block, vector<E_FType>> blockTypeFrequencies = {
    // GPS Block Types
    {E_Block::GPS_I,      {F1, F2}},           // L1, L2
    {E_Block::GPS_II,     {F1, F2}},           // L1, L2
    {E_Block::GPS_IIA,    {F1, F2}},           // L1, L2
    {E_Block::GPS_IIR_A,  {F1, F2}},           // L1, L2
    {E_Block::GPS_IIR_B,  {F1, F2}},           // L1, L2
    {E_Block::GPS_IIR_M,  {F1, F2}},           // L1, L2
    {E_Block::GPS_IIF,    {F1, F2, F5}},       // L1, L2, L5
    {E_Block::GPS_IIIA,   {F1, F2, F5}},       // L1, L2, L5

    // GLONASS Block Types
    {E_Block::GLO_M,      {G1, G2}},             // G1, G2
    {E_Block::GLO,        {G1, G2}},             // G1, G2
    {E_Block::GLO_K1A,    {G1, G2}},             // G1, G2
    {E_Block::GLO_K1B,    {G1, G2, G3}},         // G1, G2, G3
    {E_Block::GLO_K2,     {G1, G2, G3, G4, G6}}, // G1, G2, G3, G4, G6
    {E_Block::GLO_MP,     {G1, G2, G3}},         // G1, G2, G3

    // Galileo Block Types
    {E_Block::GAL_0A,     {F1, F5, F7, F8, F6}},  // E1, E5a, E5b, E5, E6
    {E_Block::GAL_0B,     {F1, F5, F7, F8, F6}},  // E1, E5a, E5b, E5, E6
    {E_Block::GAL_1,      {F1, F5, F7, F8, F6}},  // E1, E5a, E5b, E5, E6
    {E_Block::GAL_2,      {F1, F5, F7, F8, F6}},  // E1, E5a, E5b, E5, E6

    // BeiDou Block Types
    // BDS-2: B1I, B2I, B3I
    // Note: RINEX uses L2 designation for B1I (B1=1561.098 MHz), L7 for B2I, L6 for B3I
    {E_Block::BDS_2M,     {B1, F7, B3}},
    {E_Block::BDS_2G,     {B1, F7, B3}},
    {E_Block::BDS_2I,     {B1, F7, B3}},

    // BDS-3: B1I, B1C, B2a, B2I, B2(a+b), B3I
    // Note: RINEX uses L2 for B1I, L1 for B1C, L5 for B2a, L7 for B2I, L8 for B2(a+b), L6 for B3I
    {E_Block::BDS_3SI_SECM,  {B1, F1, F5, F7, F8, B3}},
    {E_Block::BDS_3SM_CAST,  {B1, F1, F5, F7, F8, B3}},
    {E_Block::BDS_3SI_CAST,  {B1, F1, F5, F7, F8, B3}},
    {E_Block::BDS_3SM_SECM,  {B1, F1, F5, F7, F8, B3}},
    {E_Block::BDS_3M_CAST,   {B1, F1, F5, F7, F8, B3}},
    {E_Block::BDS_3M_SECM_A, {B1, F1, F5, F7, F8, B3}},
    {E_Block::BDS_3G,        {B1, F1, F5, F7, F8, B3}},
    {E_Block::BDS_3I,        {B1, F1, F5, F7, F8, B3}},
    {E_Block::BDS_3M_SECM_B, {B1, F1, F5, F7, F8, B3}},

    // QZSS Block Types
    {E_Block::QZS_1,      {F1, F2, F5}},       // L1, L2, L5
    {E_Block::QZS_2I,     {F1, F2, F5, F6}},   // L1, L2, L5, L6
    {E_Block::QZS_2G,     {F1, F2, F5, F6}},   // L1, L2, L5, L6
    {E_Block::QZS_2A,     {F1, F2, F5, F6}},   // L1, L2, L5, L6

    // NavIC/IRNSS Block Types
    {E_Block::IRS_1I,     {F5, I9}},           // L5, S9
    {E_Block::IRS_1G,     {F5, I9}},           // L5, S9
    {E_Block::IRS_2G,     {F5, I9}},           // L5, S9

    // SBAS Block Types
    {E_Block::SBS,        {F1, F5}},           // L1, L5

    // LEO Block Types
    {E_Block::LEO,        {F1, F2, F5}},       // L1, L2, L5
};

// Map of block types to their unsupported signal codes
// Background: GPS satellites have evolved their signal capabilities over time:
// - Older blocks (I, II, IIA, IIR-A, IIR-B): Only legacy L2 signals (L2P, L2W, L2Y)
// - IIR-M and newer (IIF, IIIA): Added modernised L2C signals (L2C, L2S, L2L, L2X)
//
// This map lists signals that are NOT supported by each block type.
// If a block type is not in this map, all signals are supported (default behavior).
static const map<E_Block, set<E_ObsCode>> unsupportedSignalsByBlockType = {
    // Older GPS blocks do not support modernised L2C signals
    {E_Block::GPS_I,     {E_ObsCode::L2C, E_ObsCode::L2S, E_ObsCode::L2L, E_ObsCode::L2X}},
    {E_Block::GPS_II,    {E_ObsCode::L2C, E_ObsCode::L2S, E_ObsCode::L2L, E_ObsCode::L2X}},
    {E_Block::GPS_IIA,   {E_ObsCode::L2C, E_ObsCode::L2S, E_ObsCode::L2L, E_ObsCode::L2X}},
    {E_Block::GPS_IIR_A, {E_ObsCode::L2C, E_ObsCode::L2S, E_ObsCode::L2L, E_ObsCode::L2X}},
    {E_Block::GPS_IIR_B, {E_ObsCode::L2C, E_ObsCode::L2S, E_ObsCode::L2L, E_ObsCode::L2X}},
    // IIR-M and newer support L2C signals, so they're not in this list
};

// Filter signal codes based on block type capabilities
// Returns true if the signal code is supported by the given block type
bool isSignalSupportedByBlockType(E_ObsCode code, E_Block blockType)
{
    auto it = unsupportedSignalsByBlockType.find(blockType);
    if (it != unsupportedSignalsByBlockType.end())
    {
        // Block type has restrictions - check if this signal is unsupported
        return it->second.find(code) == it->second.end();
    }

    // Block type not in map - all signals supported (default behavior)
    return true;
}

const unsigned int tbl_CRC24Q[] = {
    0x000000, 0x864CFB, 0x8AD50D, 0x0C99F6, 0x93E6E1, 0x15AA1A, 0x1933EC, 0x9F7F17, 0xA18139,
    0x27CDC2, 0x2B5434, 0xAD18CF, 0x3267D8, 0xB42B23, 0xB8B2D5, 0x3EFE2E, 0xC54E89, 0x430272,
    0x4F9B84, 0xC9D77F, 0x56A868, 0xD0E493, 0xDC7D65, 0x5A319E, 0x64CFB0, 0xE2834B, 0xEE1ABD,
    0x685646, 0xF72951, 0x7165AA, 0x7DFC5C, 0xFBB0A7, 0x0CD1E9, 0x8A9D12, 0x8604E4, 0x00481F,
    0x9F3708, 0x197BF3, 0x15E205, 0x93AEFE, 0xAD50D0, 0x2B1C2B, 0x2785DD, 0xA1C926, 0x3EB631,
    0xB8FACA, 0xB4633C, 0x322FC7, 0xC99F60, 0x4FD39B, 0x434A6D, 0xC50696, 0x5A7981, 0xDC357A,
    0xD0AC8C, 0x56E077, 0x681E59, 0xEE52A2, 0xE2CB54, 0x6487AF, 0xFBF8B8, 0x7DB443, 0x712DB5,
    0xF7614E, 0x19A3D2, 0x9FEF29, 0x9376DF, 0x153A24, 0x8A4533, 0x0C09C8, 0x00903E, 0x86DCC5,
    0xB822EB, 0x3E6E10, 0x32F7E6, 0xB4BB1D, 0x2BC40A, 0xAD88F1, 0xA11107, 0x275DFC, 0xDCED5B,
    0x5AA1A0, 0x563856, 0xD074AD, 0x4F0BBA, 0xC94741, 0xC5DEB7, 0x43924C, 0x7D6C62, 0xFB2099,
    0xF7B96F, 0x71F594, 0xEE8A83, 0x68C678, 0x645F8E, 0xE21375, 0x15723B, 0x933EC0, 0x9FA736,
    0x19EBCD, 0x8694DA, 0x00D821, 0x0C41D7, 0x8A0D2C, 0xB4F302, 0x32BFF9, 0x3E260F, 0xB86AF4,
    0x2715E3, 0xA15918, 0xADC0EE, 0x2B8C15, 0xD03CB2, 0x567049, 0x5AE9BF, 0xDCA544, 0x43DA53,
    0xC596A8, 0xC90F5E, 0x4F43A5, 0x71BD8B, 0xF7F170, 0xFB6886, 0x7D247D, 0xE25B6A, 0x641791,
    0x688E67, 0xEEC29C, 0x3347A4, 0xB50B5F, 0xB992A9, 0x3FDE52, 0xA0A145, 0x26EDBE, 0x2A7448,
    0xAC38B3, 0x92C69D, 0x148A66, 0x181390, 0x9E5F6B, 0x01207C, 0x876C87, 0x8BF571, 0x0DB98A,
    0xF6092D, 0x7045D6, 0x7CDC20, 0xFA90DB, 0x65EFCC, 0xE3A337, 0xEF3AC1, 0x69763A, 0x578814,
    0xD1C4EF, 0xDD5D19, 0x5B11E2, 0xC46EF5, 0x42220E, 0x4EBBF8, 0xC8F703, 0x3F964D, 0xB9DAB6,
    0xB54340, 0x330FBB, 0xAC70AC, 0x2A3C57, 0x26A5A1, 0xA0E95A, 0x9E1774, 0x185B8F, 0x14C279,
    0x928E82, 0x0DF195, 0x8BBD6E, 0x872498, 0x016863, 0xFAD8C4, 0x7C943F, 0x700DC9, 0xF64132,
    0x693E25, 0xEF72DE, 0xE3EB28, 0x65A7D3, 0x5B59FD, 0xDD1506, 0xD18CF0, 0x57C00B, 0xC8BF1C,
    0x4EF3E7, 0x426A11, 0xC426EA, 0x2AE476, 0xACA88D, 0xA0317B, 0x267D80, 0xB90297, 0x3F4E6C,
    0x33D79A, 0xB59B61, 0x8B654F, 0x0D29B4, 0x01B042, 0x87FCB9, 0x1883AE, 0x9ECF55, 0x9256A3,
    0x141A58, 0xEFAAFF, 0x69E604, 0x657FF2, 0xE33309, 0x7C4C1E, 0xFA00E5, 0xF69913, 0x70D5E8,
    0x4E2BC6, 0xC8673D, 0xC4FECB, 0x42B230, 0xDDCD27, 0x5B81DC, 0x57182A, 0xD154D1, 0x26359F,
    0xA07964, 0xACE092, 0x2AAC69, 0xB5D37E, 0x339F85, 0x3F0673, 0xB94A88, 0x87B4A6, 0x01F85D,
    0x0D61AB, 0x8B2D50, 0x145247, 0x921EBC, 0x9E874A, 0x18CBB1, 0xE37B16, 0x6537ED, 0x69AE1B,
    0xEFE2E0, 0x709DF7, 0xF6D10C, 0xFA48FA, 0x7C0401, 0x42FA2F, 0xC4B6D4, 0xC82F22, 0x4E63D9,
    0xD11CCE, 0x575035, 0x5BC9C3, 0xDD8538
};

const boost::bimap<E_ObsCode, int> mCodes_gps =
    boost::assign::list_of<
        boost::bimap<E_ObsCode, int>::
            relation>(E_ObsCode::L1C, 0)(E_ObsCode::L1P, 1)(E_ObsCode::L1W, 2)(E_ObsCode::L1S, 3)(E_ObsCode::L1L, 4)(E_ObsCode::L2C, 5)(E_ObsCode::L2D, 6)(E_ObsCode::L2S, 7)(E_ObsCode::L2L, 8)(E_ObsCode::L2X, 9)(E_ObsCode::L2P, 10)(E_ObsCode::L2W, 11)(E_ObsCode::L2Y, 12)(E_ObsCode::L2M, 13)(E_ObsCode::L5I, 14)(E_ObsCode::L5Q, 15)(
        E_ObsCode::L5X,
        16
    );

const boost::bimap<E_ObsCode, int> mCodes_glo =
    boost::assign::list_of<boost::bimap<E_ObsCode, int>::
                               relation>(E_ObsCode::L1C, 0)(E_ObsCode::L1P, 1)(E_ObsCode::L2C, 2)(
        E_ObsCode::L2P,
        3
    );

const boost::bimap<E_ObsCode, int> mCodes_gal =
    boost::assign::list_of<
        boost::bimap<E_ObsCode, int>::
            relation>(E_ObsCode::L1A, 0)(E_ObsCode::L1B, 1)(E_ObsCode::L1C, 2)(E_ObsCode::L1X, 3)(E_ObsCode::L1Z, 4)(E_ObsCode::L5I, 5)(E_ObsCode::L5Q, 6)(E_ObsCode::L5X, 7)(E_ObsCode::L7I, 8)(E_ObsCode::L7Q, 9)(E_ObsCode::L7X, 10)(E_ObsCode::L8I, 11)(E_ObsCode::L8Q, 12)(E_ObsCode::L8X, 13)(E_ObsCode::L6A, 14)(E_ObsCode::L6B, 15)(E_ObsCode::L6C, 16)(E_ObsCode::L6X, 17)(
        E_ObsCode::L6Z,
        18
    );

const boost::bimap<E_ObsCode, int> mCodes_qzs =
    boost::assign::list_of<
        boost::bimap<E_ObsCode, int>::
            relation>(E_ObsCode::L1C, 0)(E_ObsCode::L1S, 1)(E_ObsCode::L1L, 2)(E_ObsCode::L2S, 3)(E_ObsCode::L2L, 4)(E_ObsCode::L2X, 5)(E_ObsCode::L5I, 6)(E_ObsCode::L5Q, 7)(E_ObsCode::L5X, 8)(E_ObsCode::L6S, 9)(E_ObsCode::L6L, 10)(E_ObsCode::L6X, 11)(
        E_ObsCode::L1X,
        12
    );

const boost::bimap<E_ObsCode, int> mCodes_bds = boost::assign::list_of<
    boost::bimap<E_ObsCode, int>::relation>(E_ObsCode::L2I, 0)(E_ObsCode::L2Q, 1)(E_ObsCode::L2X, 2)(E_ObsCode::L6I, 3)(E_ObsCode::L6Q, 4)(E_ObsCode::L6X, 5)(E_ObsCode::L7I, 6)(E_ObsCode::L7Q, 7)(E_ObsCode::L7X, 8)(E_ObsCode::L1D, 9)(E_ObsCode::L1P, 10)(E_ObsCode::NONE, 11)(E_ObsCode::L5D, 12)(E_ObsCode::L5P, 13)(E_ObsCode::NONE, 14)(
    E_ObsCode::L1A,
    15
);

const boost::bimap<E_ObsCode, int> mCodes_sbs = boost::assign::list_of<
    boost::bimap<E_ObsCode, int>::relation>(E_ObsCode::L1C, 0)(E_ObsCode::L5I, 1)(
    E_ObsCode::L5Q,
    2
);

map<E_Sys, map<E_FType, E_ObsCode>> codeHax = {
    {E_Sys::GPS, {{F1, E_ObsCode::L1C}, {F2, E_ObsCode::L2W}, {F5, E_ObsCode::L5I}}},

    {E_Sys::GLO,
     {{G1, E_ObsCode::NONE},
      {G2, E_ObsCode::NONE},
      {G3, E_ObsCode::L3I},
      {G4, E_ObsCode::L4B},
      {G6, E_ObsCode::L6B}}},

    {E_Sys::GAL,
     {{F1, E_ObsCode::L1C},
      {F5, E_ObsCode::L5I},
      {F6, E_ObsCode::L6C},
      {F7, E_ObsCode::L7I},
      {F8, E_ObsCode::L8I}}},

    {E_Sys::BDS,
     {
         {F1, E_ObsCode::L1P},
         {B1, E_ObsCode::L2I},
         {F5, E_ObsCode::L5P},
         {B3, E_ObsCode::L6I},
         {F7, E_ObsCode::L7P},
         {F8, E_ObsCode::L8P},
     }},

    {E_Sys::QZS,
     {{F1, E_ObsCode::L1C}, {F2, E_ObsCode::L2L}, {F5, E_ObsCode::L5I}, {F6, E_ObsCode::L6S}}},

    {E_Sys::IRN, {{F1, E_ObsCode::L1A}, {I9, E_ObsCode::L9A}}},

    {E_Sys::SBS, {{F1, E_ObsCode::L1C}, {F5, E_ObsCode::L5I}}}
};

E_ObsCode freq2CodeHax(E_Sys sys, E_FType ft)
{
    if (codeHax.find(sys) == codeHax.end())
        return E_ObsCode::NONE;

    if (codeHax[sys].find(ft) == codeHax[sys].end())
        return E_ObsCode::NONE;

    return codeHax[sys][ft];
}
