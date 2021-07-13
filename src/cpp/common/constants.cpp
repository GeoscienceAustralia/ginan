
#include <iostream>
#include <sstream>
#include <string>
#include <array>
#include <map>

using std::map;

#include "constants.h"
#include "enums.h"
#include <boost/utility/binary.hpp>


const E_FType ftypes[E_ObsCode::NUM_CODES] =
{
	[E_ObsCode::NONE] = FTYPE_NONE,
	[E_ObsCode::L1C] = F1,
	[E_ObsCode::L1P] = F1,
	[E_ObsCode::L1W] = F1,
	[E_ObsCode::L1Y] = F1,
	[E_ObsCode::L1M] = F1,
	[E_ObsCode::L1N] = F1,
	[E_ObsCode::L1S] = F1,
	[E_ObsCode::L1L] = F1,
	[E_ObsCode::L1E] = F1,
	[E_ObsCode::L1A] = F1,
	[E_ObsCode::L1B] = F1,
	[E_ObsCode::L1X] = F1,
	[E_ObsCode::L1Z] = F1,
	[E_ObsCode::L2C] = F2,
	[E_ObsCode::L2D] = F2,
	[E_ObsCode::L2S] = F2,
	[E_ObsCode::L2L] = F2,
	[E_ObsCode::L2X] = F2,
	[E_ObsCode::L2P] = F2,
	[E_ObsCode::L2W] = F2,
	[E_ObsCode::L2Y] = F2,
	[E_ObsCode::L2M] = F2,
	[E_ObsCode::L2N] = F2,
	[E_ObsCode::L5I] = F5,
	[E_ObsCode::L5Q] = F5,
	[E_ObsCode::L5X] = F5,
	[E_ObsCode::L7I] = F7,
	[E_ObsCode::L7Q] = F7,
	[E_ObsCode::L7X] = F7,
	[E_ObsCode::L6A] = F6,
	[E_ObsCode::L6B] = F6,
	[E_ObsCode::L6C] = F6,
	[E_ObsCode::L6X] = F6,
	[E_ObsCode::L6Z] = F6,
	[E_ObsCode::L6S] = F6,
	[E_ObsCode::L6L] = F6,
	[E_ObsCode::L8I] = F8,
	[E_ObsCode::L8Q] = F8,
	[E_ObsCode::L8X] = F8,
	[E_ObsCode::L2I] = F2,
	[E_ObsCode::L2Q] = F2,
	[E_ObsCode::L6I] = F6,
	[E_ObsCode::L6Q] = F6,
	[E_ObsCode::L3I] = F3,
	[E_ObsCode::L3Q] = F3,
	[E_ObsCode::L3X] = F3,
	[E_ObsCode::L1I] = F1,
	[E_ObsCode::L1Q] = F1
};


const double lambdas[E_ObsCode::NUM_CODES] =
{
	[E_ObsCode::NONE] = 0,
	[E_ObsCode::L1C] = CLIGHT/FREQ1,
	[E_ObsCode::L1P] = CLIGHT/FREQ1,
	[E_ObsCode::L1W] = CLIGHT/FREQ1,
	[E_ObsCode::L1Y] = CLIGHT/FREQ1,
	[E_ObsCode::L1M] = CLIGHT/FREQ1,
	[E_ObsCode::L1N] = CLIGHT/FREQ1,
	[E_ObsCode::L1S] = CLIGHT/FREQ1,
	[E_ObsCode::L1L] = CLIGHT/FREQ1,
	[E_ObsCode::L1E] = CLIGHT/FREQ1,
	[E_ObsCode::L1A] = CLIGHT/FREQ1,
	[E_ObsCode::L1B] = CLIGHT/FREQ1,
	[E_ObsCode::L1X] = CLIGHT/FREQ1,
	[E_ObsCode::L1Z] = CLIGHT/FREQ1,
	[E_ObsCode::L2C] = CLIGHT/FREQ2,
	[E_ObsCode::L2D] = CLIGHT/FREQ2,
	[E_ObsCode::L2S] = CLIGHT/FREQ2,
	[E_ObsCode::L2L] = CLIGHT/FREQ2,
	[E_ObsCode::L2X] = CLIGHT/FREQ2,
	[E_ObsCode::L2P] = CLIGHT/FREQ2,
	[E_ObsCode::L2W] = CLIGHT/FREQ2,
	[E_ObsCode::L2Y] = CLIGHT/FREQ2,
	[E_ObsCode::L2M] = CLIGHT/FREQ2,
	[E_ObsCode::L2N] = CLIGHT/FREQ2,
	[E_ObsCode::L5I] = CLIGHT/FREQ5,
	[E_ObsCode::L5Q] = CLIGHT/FREQ5,
	[E_ObsCode::L5X] = CLIGHT/FREQ5,
	[E_ObsCode::L7I] = CLIGHT/FREQ7,
	[E_ObsCode::L7Q] = CLIGHT/FREQ7,
	[E_ObsCode::L7X] = CLIGHT/FREQ7,
	[E_ObsCode::L6A] = CLIGHT/FREQ6,
	[E_ObsCode::L6B] = CLIGHT/FREQ6,
	[E_ObsCode::L6C] = CLIGHT/FREQ6,
	[E_ObsCode::L6X] = CLIGHT/FREQ6,
	[E_ObsCode::L6Z] = CLIGHT/FREQ6,
	[E_ObsCode::L6S] = CLIGHT/FREQ6,
	[E_ObsCode::L6L] = CLIGHT/FREQ6,
	[E_ObsCode::L8I] = CLIGHT/FREQ8,
	[E_ObsCode::L8Q] = CLIGHT/FREQ8,
	[E_ObsCode::L8X] = CLIGHT/FREQ8,
	[E_ObsCode::L2I] = CLIGHT/FREQ2,
	[E_ObsCode::L2Q] = CLIGHT/FREQ2,
	[E_ObsCode::L6I] = CLIGHT/FREQ6,
	[E_ObsCode::L6Q] = CLIGHT/FREQ6,
	[E_ObsCode::L3I] = CLIGHT/FREQ2,
	[E_ObsCode::L3Q] = CLIGHT/FREQ2,
	[E_ObsCode::L3X] = CLIGHT/FREQ2,
	[E_ObsCode::L1I] = CLIGHT/FREQ1,
	[E_ObsCode::L1Q] = CLIGHT/FREQ1
};


const double gpst0[]={1980,1, 6,0,0,0}; /* gps time reference */
const double gst0 []={1999,8,22,0,0,0}; /* galileo system time reference */
const double bdt0 []={2006,1, 1,0,0,0}; /* beidou time reference */

const double leaps[MAXLEAPS+1][7]={ /* leap seconds (y,m,d,h,m,s,utc-gpst) */
	{2017,1,1,0,0,0,-18},
	{2015,7,1,0,0,0,-17},
	{2012,7,1,0,0,0,-16},
	{2009,1,1,0,0,0,-15},
	{2006,1,1,0,0,0,-14},
	{1999,1,1,0,0,0,-13},
	{1997,7,1,0,0,0,-12},
	{1996,1,1,0,0,0,-11},
	{1994,7,1,0,0,0,-10},
	{1993,7,1,0,0,0, -9},
	{1992,7,1,0,0,0, -8},
	{1991,1,1,0,0,0, -7},
	{1990,1,1,0,0,0, -6},
	{1988,1,1,0,0,0, -5},
	{1985,7,1,0,0,0, -4},
	{1983,7,1,0,0,0, -3},
	{1982,7,1,0,0,0, -2},
	{1981,7,1,0,0,0, -1},
	{0}
};
const double chisqr[100]={      /* chi-sqr(n) (alpha=0.001) */
	10.8,13.8,16.3,18.5,20.5,22.5,24.3,26.1,27.9,29.6,
	31.3,32.9,34.5,36.1,37.7,39.3,40.8,42.3,43.8,45.3,
	46.8,48.3,49.7,51.2,52.6,54.1,55.5,56.9,58.3,59.7,
	61.1,62.5,63.9,65.2,66.6,68.0,69.3,70.7,72.1,73.4,
	74.7,76.0,77.3,78.6,80.0,81.3,82.6,84.0,85.4,86.7,
	88.0,89.3,90.6,91.9,93.3,94.7,96.0,97.4,98.7,100 ,
	101 ,102 ,103 ,104 ,105 ,107 ,108 ,109 ,110 ,112 ,
	113 ,114 ,115 ,116 ,118 ,119 ,120 ,122 ,123 ,125 ,
	126 ,127 ,128 ,129 ,131 ,132 ,133 ,134 ,135 ,137 ,
	138 ,139 ,140 ,142 ,143 ,144 ,145 ,147 ,148 ,149
};

map<int, double> lam_carr =
{
	{F1, CLIGHT/FREQ1},
	{F2, CLIGHT/FREQ2},
	{F5, CLIGHT/FREQ5},
	{F6, CLIGHT/FREQ6},
	{F8, CLIGHT/FREQ8}
};

const unsigned int tbl_CRC24Q[]=
{
	0x000000,0x864CFB,0x8AD50D,0x0C99F6,0x93E6E1,0x15AA1A,0x1933EC,0x9F7F17,
	0xA18139,0x27CDC2,0x2B5434,0xAD18CF,0x3267D8,0xB42B23,0xB8B2D5,0x3EFE2E,
	0xC54E89,0x430272,0x4F9B84,0xC9D77F,0x56A868,0xD0E493,0xDC7D65,0x5A319E,
	0x64CFB0,0xE2834B,0xEE1ABD,0x685646,0xF72951,0x7165AA,0x7DFC5C,0xFBB0A7,
	0x0CD1E9,0x8A9D12,0x8604E4,0x00481F,0x9F3708,0x197BF3,0x15E205,0x93AEFE,
	0xAD50D0,0x2B1C2B,0x2785DD,0xA1C926,0x3EB631,0xB8FACA,0xB4633C,0x322FC7,
	0xC99F60,0x4FD39B,0x434A6D,0xC50696,0x5A7981,0xDC357A,0xD0AC8C,0x56E077,
	0x681E59,0xEE52A2,0xE2CB54,0x6487AF,0xFBF8B8,0x7DB443,0x712DB5,0xF7614E,
	0x19A3D2,0x9FEF29,0x9376DF,0x153A24,0x8A4533,0x0C09C8,0x00903E,0x86DCC5,
	0xB822EB,0x3E6E10,0x32F7E6,0xB4BB1D,0x2BC40A,0xAD88F1,0xA11107,0x275DFC,
	0xDCED5B,0x5AA1A0,0x563856,0xD074AD,0x4F0BBA,0xC94741,0xC5DEB7,0x43924C,
	0x7D6C62,0xFB2099,0xF7B96F,0x71F594,0xEE8A83,0x68C678,0x645F8E,0xE21375,
	0x15723B,0x933EC0,0x9FA736,0x19EBCD,0x8694DA,0x00D821,0x0C41D7,0x8A0D2C,
	0xB4F302,0x32BFF9,0x3E260F,0xB86AF4,0x2715E3,0xA15918,0xADC0EE,0x2B8C15,
	0xD03CB2,0x567049,0x5AE9BF,0xDCA544,0x43DA53,0xC596A8,0xC90F5E,0x4F43A5,
	0x71BD8B,0xF7F170,0xFB6886,0x7D247D,0xE25B6A,0x641791,0x688E67,0xEEC29C,
	0x3347A4,0xB50B5F,0xB992A9,0x3FDE52,0xA0A145,0x26EDBE,0x2A7448,0xAC38B3,
	0x92C69D,0x148A66,0x181390,0x9E5F6B,0x01207C,0x876C87,0x8BF571,0x0DB98A,
	0xF6092D,0x7045D6,0x7CDC20,0xFA90DB,0x65EFCC,0xE3A337,0xEF3AC1,0x69763A,
	0x578814,0xD1C4EF,0xDD5D19,0x5B11E2,0xC46EF5,0x42220E,0x4EBBF8,0xC8F703,
	0x3F964D,0xB9DAB6,0xB54340,0x330FBB,0xAC70AC,0x2A3C57,0x26A5A1,0xA0E95A,
	0x9E1774,0x185B8F,0x14C279,0x928E82,0x0DF195,0x8BBD6E,0x872498,0x016863,
	0xFAD8C4,0x7C943F,0x700DC9,0xF64132,0x693E25,0xEF72DE,0xE3EB28,0x65A7D3,
	0x5B59FD,0xDD1506,0xD18CF0,0x57C00B,0xC8BF1C,0x4EF3E7,0x426A11,0xC426EA,
	0x2AE476,0xACA88D,0xA0317B,0x267D80,0xB90297,0x3F4E6C,0x33D79A,0xB59B61,
	0x8B654F,0x0D29B4,0x01B042,0x87FCB9,0x1883AE,0x9ECF55,0x9256A3,0x141A58,
	0xEFAAFF,0x69E604,0x657FF2,0xE33309,0x7C4C1E,0xFA00E5,0xF69913,0x70D5E8,
	0x4E2BC6,0xC8673D,0xC4FECB,0x42B230,0xDDCD27,0x5B81DC,0x57182A,0xD154D1,
	0x26359F,0xA07964,0xACE092,0x2AAC69,0xB5D37E,0x339F85,0x3F0673,0xB94A88,
	0x87B4A6,0x01F85D,0x0D61AB,0x8B2D50,0x145247,0x921EBC,0x9E874A,0x18CBB1,
	0xE37B16,0x6537ED,0x69AE1B,0xEFE2E0,0x709DF7,0xF6D10C,0xFA48FA,0x7C0401,
	0x42FA2F,0xC4B6D4,0xC82F22,0x4E63D9,0xD11CCE,0x575035,0x5BC9C3,0xDD8538
};

const boost::bimap<E_ObsCode,int> mCodes_gps = boost::assign::list_of<boost::bimap<E_ObsCode,int>::relation>
	(E_ObsCode::L1C,0)
	(E_ObsCode::L1P,1)
	(E_ObsCode::L1W,2)
	(E_ObsCode::L1Y,3)
	(E_ObsCode::L1M,4)
	(E_ObsCode::L2C,5)
	(E_ObsCode::L2D,6)
	(E_ObsCode::L2S,7)
	(E_ObsCode::L2L,8)
	(E_ObsCode::L2X,9)
	(E_ObsCode::L2P,10)
	(E_ObsCode::L2W,11)
	(E_ObsCode::L2Y,12)
	(E_ObsCode::L2M,13)
	(E_ObsCode::L5I,14)
	(E_ObsCode::L5Q,15)
	(E_ObsCode::L5X,16);
	
const boost::bimap<E_ObsCode,int> mCodes_glo = boost::assign::list_of<boost::bimap<E_ObsCode,int>::relation>
	(E_ObsCode::L1C,0)
	(E_ObsCode::L1P,1)
	(E_ObsCode::L2C,2)
	(E_ObsCode::L2P,3);

const boost::bimap<E_ObsCode,int> mCodes_gal = boost::assign::list_of<boost::bimap<E_ObsCode,int>::relation>
	(E_ObsCode::L1A,0)
	(E_ObsCode::L1B,1)
	(E_ObsCode::L1C,2)
	(E_ObsCode::L1X,3)
	(E_ObsCode::L1Z,4)
	(E_ObsCode::L5I,5)
	(E_ObsCode::L5Q,6)
	(E_ObsCode::L5X,7)
	(E_ObsCode::L7I,8)
	(E_ObsCode::L7Q,9)
	(E_ObsCode::L7X,10)
	(E_ObsCode::L8I,11)
	(E_ObsCode::L8Q,12)
	(E_ObsCode::L8X,13)
	(E_ObsCode::L6A,14)
	(E_ObsCode::L6B,15)
	(E_ObsCode::L6C,16)
	(E_ObsCode::L6X,17)
	(E_ObsCode::L6Z,18);


const boost::bimap<E_ObsCode,int> mCodes_qzs = boost::assign::list_of<boost::bimap<E_ObsCode,int>::relation>
	(E_ObsCode::L1C,0)
	(E_ObsCode::L1S,1)
	(E_ObsCode::L1L,2)
	(E_ObsCode::L2S,3)
	(E_ObsCode::L2L,4)
	(E_ObsCode::L2X,5)
	(E_ObsCode::L5I,6)
	(E_ObsCode::L5Q,7)
	(E_ObsCode::L5X,8)
	(E_ObsCode::L6S,9)
	(E_ObsCode::L6L,10)
	(E_ObsCode::L6X,11)
	(E_ObsCode::L1X,12);

const boost::bimap<E_ObsCode,int> mCodes_bds = boost::assign::list_of<boost::bimap<E_ObsCode,int>::relation>
	(E_ObsCode::L1I,0)
	(E_ObsCode::L1Q,1)
	(E_ObsCode::L1X,2)
	(E_ObsCode::L7I,3)
	(E_ObsCode::L7Q,4)
	(E_ObsCode::L7X,5)
	(E_ObsCode::L6I,6)
	(E_ObsCode::L6Q,7)
	(E_ObsCode::L6X,8);

const boost::bimap<E_ObsCode,int> mCodes_sds = boost::assign::list_of<boost::bimap<E_ObsCode,int>::relation>
	(E_ObsCode::L1C,0)
	(E_ObsCode::L5I,1)
	(E_ObsCode::L5Q,2)
	(E_ObsCode::L5X,3);



