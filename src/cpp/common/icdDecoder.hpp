
#pragma once

#include "rtcmDecoder.hpp"
#include "ephemeris.hpp"


signed int gpsBitSFromWord(
	vector<int>&	words,
	int				word,
	int				offset,
	int				len);

unsigned int gpsBitUFromWord(
	vector<int>&	words,
	int				word,
	int				offset,
	int				len);

struct IcdDecoder
{
	map<SatSys, map<int, vector<int>>>	subframeMap;
	
	/* decode Galileo I/NAV ephemeris ----------------------------------------------
	* decode Galileo I/NAV (ref [5] 4.3)
	* args   : unsigned char *buff I Galileo I/NAV subframe bits
	*                                  buff[ 0-15]: I/NAV word type 0 (128 bit)
	*                                  buff[16-31]: I/NAV word type 1
	*                                  buff[32-47]: I/NAV word type 2
	*                                  buff[48-63]: I/NAV word type 3
	*                                  buff[64-79]: I/NAV word type 4
	*                                  buff[80-95]: I/NAV word type 5
	*          eph_t    *eph    IO  ephemeris structure
	* return : status (1:ok,0:error)
	*-----------------------------------------------------------------------------*/
// 	int decode_gal_inav(
// 		const unsigned char* buff, 
// 		eph_t* eph)
// 	{
// 		double tow, toc, tt, sqrtA;
// 		int i, time_f, week, svid, e5b_hs, e1b_hs, e5b_dvs, e1b_dvs, type[6], iod_nav[4];
// 
// 		i = 0; /* word type 0 */
// 		type[0]		= getbitu(buff, i, 6);              i += 6;
// 		time_f		= getbitu(buff, i, 2);              i += 2 + 88;
// 		week		= getbitu(buff, i, 12);              i += 12; /* gst-week */
// 		tow			= getbitu(buff, i, 20);
// 
// 		i = 128; /* word type 1 */
// 		type[1]		= getbitu(buff, i, 6);              i += 6;
// 		iod_nav[0]	= getbitu(buff, i, 10);              i += 10;
// 		eph->toes	= getbitu(buff, i, 14) * 60.0;         i += 14;
// 		eph->M0		= getbits(buff, i, 32) * P2_31 * SC2RAD; i += 32;
// 		eph->e		= getbitu(buff, i, 32) * P2_33;        i += 32;
// 		sqrtA		= getbitu(buff, i, 32) * P2_19;
// 
// 		i = 128 * 2; /* word type 2 */
// 		type[2]		= getbitu(buff, i, 6);              i += 6;
// 		iod_nav[1]	= getbitu(buff, i, 10);              i += 10;
// 		eph->OMG0	= getbits(buff, i, 32) * P2_31 * SC2RAD; i += 32;
// 		eph->i0		= getbits(buff, i, 32) * P2_31 * SC2RAD; i += 32;
// 		eph->omg	= getbits(buff, i, 32) * P2_31 * SC2RAD; i += 32;
// 		eph->idot	= getbits(buff, i, 14) * P2_43 * SC2RAD;
// 
// 		i = 128 * 3; /* word type 3 */
// 		type[3]		= getbitu(buff, i, 6);              i += 6;
// 		iod_nav[2]	= getbitu(buff, i, 10);              i += 10;
// 		eph->OMGd	= getbits(buff, i, 24) * P2_43 * SC2RAD; i += 24;
// 		eph->deln	= getbits(buff, i, 16) * P2_43 * SC2RAD; i += 16;
// 		eph->cuc	= getbits(buff, i, 16) * P2_29;        i += 16;
// 		eph->cus	= getbits(buff, i, 16) * P2_29;        i += 16;
// 		eph->crc	= getbits(buff, i, 16) * P2_5;         i += 16;
// 		eph->crs	= getbits(buff, i, 16) * P2_5;         i += 16;
// 		eph->sva	= getbitu(buff, i, 8);
// 
// 		i = 128 * 4; /* word type 4 */
// 		type[4]		= getbitu(buff, i, 6);              i += 6;
// 		iod_nav[3]	= getbitu(buff, i, 10);              i += 10;
// 		svid		= getbitu(buff, i, 6);              i += 6;
// 		eph->cic	= getbits(buff, i, 16) * P2_29;        i += 16;
// 		eph->cis	= getbits(buff, i, 16) * P2_29;        i += 16;
// 		toc			= getbitu(buff, i, 14) * 60.0;         i += 14;
// 		eph->f0		= getbits(buff, i, 31) * P2_34;        i += 31;
// 		eph->f1		= getbits(buff, i, 21) * P2_46;        i += 21;
// 		eph->f2		= getbits(buff, i, 6) * P2_59;
// 
// 		i = 128 * 5; /* word type 5 */
// 		type[5]		= getbitu(buff, i, 6);              i += 6 + 41;
// 		eph->tgd[0]	= getbits(buff, i, 10) * P2_32;        i += 10; /* BGD E5a/E1 */
// 		eph->tgd[1]	= getbits(buff, i, 10) * P2_32;        i += 10; /* BGD E5b/E1 */
// 		e5b_hs		= getbitu(buff, i, 2);              i += 2;
// 		e1b_hs		= getbitu(buff, i, 2);              i += 2;
// 		e5b_dvs		= getbitu(buff, i, 1);              i += 1;
// 		e1b_dvs		= getbitu(buff, i, 1);
// 
// 		/* test word types */
// 		if	(  type[0] != 0 
// 			|| type[1] != 1 
// 			|| type[2] != 2 
// 			|| type[3] != 3 
// 			|| type[4] != 4)
// 		{
// 			trace(3, "decode_gal_inav error: type=%d %d %d %d %d\n", type[0], type[1], type[2], type[3], type[4]);
// 			return 0;
// 		}
// 		
// 		/* test word type 0 time field */
// 		if (time_f != 2)
// 		{
// 			trace(3, "decode_gal_inav error: word0-time=%d\n", time_f);
// 			return 0;
// 		}
// 		
// 		/* test consistency of iod_nav */
// 		if	(  iod_nav[0] != iod_nav[1] 
// 			|| iod_nav[0] != iod_nav[2] 
// 			|| iod_nav[0] != iod_nav[3])
// 		{
// 			trace(3, "decode_gal_inav error: ionav=%d %d %d %d\n", iod_nav[0], iod_nav[1], iod_nav[2], iod_nav[3]);
// 			return 0;
// 		}
// 		
// 		if (!(eph->sat = satno(SYS_GAL, svid)))
// 		{
// 			trace(2, "decode_gal_inav svid error: svid=%d\n", svid);
// 			return 0;
// 		}
// 		
// 		eph->A		= sqrtA * sqrtA;
// 		eph->iode	= eph->iodc = iod_nav[0];
// 		eph->svh	= (e5b_hs << 7) | (e5b_dvs << 6) | (e1b_hs << 1) | e1b_dvs;
// 		eph->ttr	= gst2time(week, tow);
// 		tt			= timediff(gst2time(week, eph->toes), eph->ttr); /* week complient to toe */
// 		
// 		if      (tt > +302400.0)	week--;
// 		else if (tt < -302400.0)	week++;
// 		
// 		eph->toe = gst2time(week, eph->toes);
// 		eph->toc = gst2time(week, toc);
// 		eph->week = week + 1024; /* gal-week = gst-week + 1024 */
// 		eph->code = 1;       /* data source = I/NAV E1B */
// 
// 		return 1;
// 	}
	
	bool decodeGpsTlmWord(
		vector<int>&	words,
		Eph&			eph)
	{
		return true;
	}
	
	bool decodeGpsHowWord(
		vector<int>&	words,
		Eph&			eph)
	{
		eph.howTow	= gpsBitUFromWord(words, 1, 1, 17) * 6;
		
		return true;
	}
	
	/* decode gps/qzss navigation data subframe 1 
	 */
	bool decodeGPSSubframe1(
		vector<int>&	words,
		Eph&			eph)
	{
		decodeGpsTlmWord(words, eph);
		decodeGpsHowWord(words, eph);
		
		eph.weekRollOver	= gpsBitUFromWord(words, 3,		61,		10);          	//todo aaron, these all need scaling
		eph.code			= gpsBitUFromWord(words, 3,		71,		2);      
		eph.sva				= gpsBitUFromWord(words, 3,		73,		4);     
		int svh				= gpsBitUFromWord(words, 3,		77,		6);      
		int iodc_1			= gpsBitUFromWord(words, 3,		83,		2)	<< 8; 
		
		eph.flag			= gpsBitUFromWord(words, 4,		91,		1);
		
		int tgd				= gpsBitSFromWord(words, 7,		197,	8);    
		
		int iodc_2			= gpsBitUFromWord(words, 8,		211,	8);      
		eph.tocs			= gpsBitUFromWord(words, 8,		219,	16)	* (1 << 4); 
		
		eph.f2				= gpsBitSFromWord(words, 9,		241,	8)	* P2_55; 
		eph.f1				= gpsBitSFromWord(words, 9,		249,	16)	* P2_43;
		
		eph.f0				= gpsBitSFromWord(words, 10,	271,	22)	* P2_31;

		eph.svh		= (E_Svh) svh;
		eph.tgd[0]	= tgd == -128 ? 0 : tgd * P2_31; /* ref [4] */
		eph.iodc	= iodc_1 | iodc_2;
		
		GTime nearTime	= timeGet();		//todo aaron rtcmTime()
		
		//adjgpsweek()
		{
			GWeek nowWeek = nearTime;		
			
			int dWeeks		= nowWeek - eph.weekRollOver;
			int roundDWeeks	= (dWeeks + 512) / 1024 * 1024;
			
			eph.week	= eph.weekRollOver + roundDWeeks;
		}	
		
		return true;
	}
	
	/* decode gps/qzss navigation data subframe 2 
	 */
	bool decodeGPSSubframe2(
		vector<int>&	words,
		Eph&			eph)
	{
		decodeGpsTlmWord(words, eph);
		decodeGpsHowWord(words, eph);
		
		eph.iode				= gpsBitUFromWord(words, 3,		61,		8);         
		eph.crs					= gpsBitSFromWord(words, 3,		69,		16)	* P2_5; 
		
		eph.deln				= gpsBitSFromWord(words, 4,		91,		16)	* P2_43 * SC2RAD; 
		int M0_1				= gpsBitSFromWord(words, 4,		107,	8)	<< 24; 
		
		unsigned int M0_2		= gpsBitUFromWord(words, 5,		121,	24); 
		
		eph.cuc					= gpsBitSFromWord(words, 6,		151,	16)	* P2_29;
		unsigned int e_1		= gpsBitUFromWord(words, 6,		167,	8)	<< 24;
		
		unsigned int e_2		= gpsBitUFromWord(words, 7,		181,	24);
		
		eph.cus					= gpsBitSFromWord(words, 8,		211,	16)	* P2_29;
		unsigned int sqrtA_1	= gpsBitUFromWord(words, 8,		227,	8)	<< 24;
		
		unsigned int sqrtA_2	= gpsBitUFromWord(words, 9,		241,	24);
		
		eph.toes				= gpsBitUFromWord(words, 10,	271,	16)	* (1 << 4);   
		eph.fit					= gpsBitUFromWord(words, 10,	287,	1) ? 0 : 4; /* 0:4hr,1:>4hr */
		int aodo				= gpsBitUFromWord(words, 10,	288,	5);   	//todo aaron

		eph.sqrtA	= 		(sqrtA_1	| sqrtA_2)	* P2_19;
		eph.M0		= 		(M0_1		| M0_2)		* P2_31 * SC2RAD;
		eph.e		= 		(e_1		| e_2)		* P2_33;
		eph.A		= SQR(eph.sqrtA);

		return true;
	}
	
	/* decode gps/qzss navigation data subframe 3
	 */
	bool decodeGPSSubframe3(
		vector<int>&	words,	
		Eph&			eph)
	{
		decodeGpsTlmWord(words, eph);
		decodeGpsHowWord(words, eph);
		
		eph.cic				= gpsBitSFromWord(words, 3,		61,		16)	* P2_29;
		signed int OMG0_1	= gpsBitSFromWord(words, 3,		77,		8)	<< 24; 
		
		signed int OMG0_2	= gpsBitUFromWord(words, 4,		91,		24); 
		
		eph.cis				= gpsBitSFromWord(words, 5,		121,	16)	* P2_29;
		signed int i0_1 	= gpsBitSFromWord(words, 5,		137,	8)	<< 24; 
		
		signed int i0_2 	= gpsBitUFromWord(words, 6,		151,	24); 
		
		eph.crc				= gpsBitSFromWord(words, 7,		181,	16)	* P2_5;
		signed int omg_1	= gpsBitSFromWord(words, 7,		197,	8)	<< 24; 
		
		signed int omg_2	= gpsBitUFromWord(words, 8,		211,	24); 
		
		eph.OMGd			= gpsBitSFromWord(words, 9,		241,	24)	* P2_43 * SC2RAD; 
		
		int iode			= gpsBitUFromWord(words, 10,	271,	8);
		eph.idot			= gpsBitSFromWord(words, 10,	279,	14)	* P2_43 * SC2RAD;

		eph.OMG0	= (OMG0_1	| OMG0_2)	* P2_31 * SC2RAD;
		eph.i0		= (i0_1		| i0_2)		* P2_31 * SC2RAD;
		eph.omg		= (omg_1	| omg_2)	* P2_31 * SC2RAD;
		
		/* check iode and iodc consistency */
		if	(  iode !=  eph.iode 
			|| iode != (eph.iodc & 0xFF))
		{
			return false;
		}
		
		eph.ttm		= GTime(GWeek(eph.week), GTow(eph.howTow));
		
		eph.toc		= GTime(GTow(eph.tocs), eph.ttm);
		eph.toe		= GTime(GTow(eph.toes), eph.toc);
		
		
// 		std::cout << "\n" << eph.ttm;
// 		std::cout << "\n" << eph.toe;
// 		std::cout << "\n" << eph.toc;
// 		std::cout << "\n";
		
		return true;
	}
	
	/* decode gps/qzss navigation data frame */
	bool decodeGpsSubframe(
		vector<int>&	words,		///< words[0-29]: 30 bits x 10 words
		Eph&			eph)		///< output ephemeris
	{
		if (words.size() < 10)
		{
			return false;
		}
		
		int id	= gpsBitUFromWord(words, 2,		20,		3);  

		switch (id)
		{
			case 1: return decodeGPSSubframe1(words, eph);
			case 2: return decodeGPSSubframe2(words, eph);
			case 3: return decodeGPSSubframe3(words, eph);
		}
		return false;
	}
};
