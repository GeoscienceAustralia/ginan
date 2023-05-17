
#pragma once

#include "rtcmDecoder.hpp"

struct icdDecoder
{
	map<SatSys, map<int, vector<unsigned char>>>	subframeMap;
	
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
	
	/* decode gps/qzss navigation data subframe 1 
	 */
	bool decodeGPSSubframe1(
		vector<unsigned char>&	data,
		Eph&					eph)
	{
		int i = 24;

		double tow	= getbituInc(data, i, 17) * 6.0;     				i = 48;
		int week	= getbituInc(data, i, 10);     
		eph.code	= getbituInc(data, i, 2);      
		eph.sva		= getbituInc(data, i, 4);     
		int svh		= getbituInc(data, i, 6);      
		int iodc0	= getbituInc(data, i, 2);      
		eph.flag	= getbituInc(data, i, 1);    					  	i += 87;
		int tgd		= getbitsInc(data, i, 8);      
		int iodc1	= getbituInc(data, i, 8);      
		double toc	= getbituInc(data, i, 16) * 16.0; 
		eph.f2		= getbitsInc(data, i, 8) * P2_55; 
		eph.f1		= getbitsInc(data, i, 16) * P2_43;
		eph.f0		= getbitsInc(data, i, 22) * P2_31;

		eph.svh		= (E_Svh) svh;
		eph.tgd[0]	= tgd == -128 ? 0.0 : tgd * P2_31; /* ref [4] */
		eph.iodc	= (iodc0 << 8) + iodc1;
// 		eph.week	= adjgpsweek(week); /* week of tow */	//todo aaron
// 		eph.ttm		= gpst2time(eph.week, tow);
// 		eph.toc		= gpst2time(eph.week, toc);

		return true;
	}
	
	/* decode gps/qzss navigation data subframe 2 
	 */
	bool decodeGPSSubframe2(
		vector<unsigned char>&	data,
		Eph&					eph)
	{
		double sqrtA;
		int i = 48;

		eph.iode	= getbituInc(data, i, 8);         
		eph.crs		= getbitsInc(data, i, 16) * P2_5; 
		eph.deln	= getbitsInc(data, i, 16) * P2_43 * SC2RAD; 
		eph.M0		= getbitsInc(data, i, 32) * P2_31 * SC2RAD; 
		eph.cuc		= getbitsInc(data, i, 16) * P2_29;
		eph.e		= getbituInc(data, i, 32) * P2_33;
		eph.cus		= getbitsInc(data, i, 16) * P2_29;
		sqrtA		= getbituInc(data, i, 32) * P2_19;
		eph.toes	= getbituInc(data, i, 16) * 16.0;   
		eph.fit		= getbituInc(data, i, 1) ? 0.0 : 4.0; /* 0:4hr,1:>4hr */

		eph.A		= SQR(sqrtA);

		return true;
	}
	
	/* decode gps/qzss navigation data subframe 3
	 */
	bool decodeGPSSubframe3(
		vector<unsigned char>&	data,	
		Eph&					eph)
	{
		int i = 48;

		eph.cic		= getbitsInc(data, i, 16) * P2_29;        
		eph.OMG0	= getbitsInc(data, i, 32) * P2_31 * SC2RAD; 
		eph.cis		= getbitsInc(data, i, 16) * P2_29;       
		eph.i0 		= getbitsInc(data, i, 32) * P2_31 * SC2RAD; 
		eph.crc		= getbitsInc(data, i, 16) * P2_5;       
		eph.omg		= getbitsInc(data, i, 32) * P2_31 * SC2RAD; 
		eph.OMGd	= getbitsInc(data, i, 24) * P2_43 * SC2RAD; 
		int iode	= getbituInc(data, i, 8);             
		eph.idot	= getbitsInc(data, i, 14) * P2_43 * SC2RAD;

		/* check iode and iodc consistency */
		if	(  iode != eph.iode 
			|| iode != (eph.iodc & 0xFF))
		{
			return false;
		}
		
		/* adjustment for week handover */
		// double tow		= time2gpst(eph.ttm, &eph.week);
		// double toc		= time2gpst(eph.toc);
		double tow  = GTow(eph.ttm);
		double toc  = GTow(eph.toc);
		eph.week	= GWeek(eph.ttm);
		
		if		(eph.toes < tow - 302400.0) {eph.week++; tow -= 604800.0;}
		else if	(eph.toes > tow + 302400.0) {eph.week--; tow += 604800.0;}
		
		eph.toe = gpst2time(eph.week, eph.toes);
		eph.toc = gpst2time(eph.week, toc);
		eph.ttm = gpst2time(eph.week, tow);

		return true;
	}
	
	/* decode gps/qzss navigation data frame */
	int decodeGpsSubframe(
		vector<unsigned char>&	data,		///< data[0-29]: 24 bits x 10 words=
		Eph&					eph)
	{
		if (data.empty())
		{
			return 0;
		}
		
		int id = getbitu(data, 43, 3); /* subframe id */

// 		trace(3, "decodefrm: id=%d\n", id);

		switch (id)
		{
			case 1: return decodeGPSSubframe1(data, eph);
			case 2: return decodeGPSSubframe2(data, eph);
			case 3: return decodeGPSSubframe3(data, eph);
		}
		return 0;
	}

};
