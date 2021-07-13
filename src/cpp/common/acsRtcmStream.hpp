
#ifndef __ACSRTCMSTREAM__HPP
#define __ACSRTCMSTREAM__HPP

#include <boost/date_time/posix_time/posix_time.hpp>

#include <map>

using std::map;

#include "acsStream.hpp"
#include "ntripTrace.hpp"

struct LockTimeInfo
{
	boost::posix_time::ptime	epoch;

	map<pair<uint8_t, uint8_t>, uint16_t> lock_time_indicators;

	LockTimeInfo(const boost::posix_time::ptime&				epoch,
				const map<pair<uint8_t, uint8_t>, uint16_t>&	lockTimeIndicators)
	:	epoch(epoch),
		lock_time_indicators(lockTimeIndicators)
	{

	}

	LockTimeInfo()
	{

	}

	pair<uint32_t, uint32_t> get_lock_time_and_coefficient(pair<uint8_t, uint8_t> cell)
	{
		uint32_t b;
		uint32_t n;

		if (lock_time_indicators.count(cell) > 0)
		{
			auto lock = lock_time_indicators.at(cell);

			if		(lock < 	64)	{b = 1;			n = b * lock;               }
			else if (lock < 	96)	{b = 2;			n = b * lock - 64;          }
			else if (lock < 	128){b = 4;			n = b * lock - 256;         }
			else if (lock < 	160){b = 8;			n = b * lock - 768;         }
			else if (lock < 	192){b = 16;		n = b * lock - 2048;        }
			else if (lock < 	224){b = 32;		n = b * lock - 5120;        }
			else if (lock < 	256){b = 64;		n = b * lock - 12288;       }
			else if (lock < 	288){b = 128;		n = b * lock - 28672;       }
			else if (lock < 	320){b = 256;		n = b * lock - 65536;       }
			else if (lock < 	352){b = 512;		n = b * lock - 147456;      }
			else if (lock < 	384){b = 1024;		n = b * lock - 327680;      }
			else if (lock < 	416){b = 2048;		n = b * lock - 720896;      }
			else if (lock < 	448){b = 4096;		n = b * lock - 1572864;     }
			else if (lock < 	480){b = 8192;		n = b * lock - 3407872;     }
			else if (lock < 	512){b = 16384;		n = b * lock - 7340032;     }
			else if (lock < 	544){b = 32768;		n = b * lock - 15728640;    }
			else if (lock < 	576){b = 65536;		n = b * lock - 33554432;    }
			else if (lock < 	608){b = 131072;    n = b * lock - 71303168;    }
			else if (lock < 	640){b = 262144;    n = b * lock - 150994944;   }
			else if (lock <		672){b = 524288; 	n = b * lock - 318767104;   }
			else if (lock < 	704){b = 1048576;	n = b * lock - 671088640;   }
			else if (lock ==	704){b = 2097152;	n = b * lock - 1409286144;  }
		}

		return pair<uint32_t, uint32_t>(n, b);
	}

	static bool LOC(LockTimeInfo&			current,
					LockTimeInfo& 			previous,
					pair<uint8_t, uint8_t>	cell)
	{
		// TODO should we assume continuity or loss of lock when no previous epoch info???

		if (previous.	epoch.is_not_a_date_time()) 	return false;
		if (current.	epoch.is_not_a_date_time()) 	return true;
		// From the logic in 3.5.16.3.3 Lock Time Indicator in the RTCM 10403.3 spec

		auto current_lock_time_and_coefficient = current.get_lock_time_and_coefficient(cell);

		auto n = current_lock_time_and_coefficient.first;
		auto b = current_lock_time_and_coefficient.second;

		auto previous_lock_time_and_coefficient = previous.get_lock_time_and_coefficient(cell);

		auto p = previous_lock_time_and_coefficient.first;
		auto a = previous_lock_time_and_coefficient.second;

		auto dt = (current.epoch - previous.epoch).total_milliseconds();

		// NOTE : I've explicitly coded the rules from the spec as they are to make it clear the logic matches
		// the spec.  The tests could be simplified.

		if 		(p >  n)											{ return true;	}
		else if (p == n							&& dt >= a)			{ return true;	}
		else if (p == n							&& dt <  a)			{ return false;	}
		else if (p <  n	&& b >  p				&& dt >= (n+b-p))	{ return true;	}
		// Do we need the possible LOC???
		else if (p <  n	&& b >  p	&& n < dt	&& dt <  (n+b-p))	{ return true;  }
		else if (p <  n	&& b >  p				&& dt <= n)			{ return false; }
		else if (p <  n	&& b <= p				&& dt >  n)			{ return true;  }
		else if (p <  n	&& b <= p				&& dt <= n)			{ return false; }

		// Should never get here
		return true;
	}
};


// From the RTCM spec...
//  - table 3.5-91 (GPS)
//  - table 3.5-96 (GLONASS)
//  - table 3.5-99 (GALILEO)
//  - table 3.5-105 (QZSS)
//  - table 3.5-108 (BEIDOU)
struct SignalInfo
{
	uint8_t		signal_id;
	E_FType		ftype;
	E_ObsCode	rinex_observation_code;
};



struct RtcmDecoder
{
	static void setTime(GTime& time, double tow);
	static uint16_t message_length(char header[2]);
	static RtcmMessageType message_type(const uint8_t message[]);
	
	
	struct SSRDecoder
	{
		constexpr static int updateInterval[16] =
		{
			1, 2, 5, 10, 15, 30, 60, 120, 240, 300, 600, 900, 1800, 3600, 7200, 10800
		};
		
		static int getUdiIndex(int udi);
		
		// decodes data from Decoder struct and places in global nav_t struct.
		void decodeSSR(uint8_t* data, unsigned int message_length);
		
		// Optional virtual message functions for trace.
		virtual void traceSsrEph(SatSys Sat,SSREph ssrEph){}
		virtual void traceSsrClk(SatSys Sat,SSRClk ssrClk){}
		virtual void traceSsrCodeB(SatSys Sat,E_ObsCode mode, SSRCodeBias ssrBias){}
		virtual void traceSsrPhasB(SatSys Sat,E_ObsCode mode, SSRPhasBias ssrBias){}
	};
	
	struct EphemerisDecoder
	{
		// decodes data from Decoder struct and places in global nav_t struct.
		void decodeEphemeris(uint8_t* data, unsigned int message_length);
		
		// Optional virtual message function for trace.
		virtual void traceBroEph(Eph eph,E_Sys sys){}
	};
	
	struct MSM7Decoder
	{ 
		E_FType code_to_ftype(E_Sys sys, E_ObsCode code);
		boost::optional<SignalInfo> get_signal_info(E_Sys sys, uint8_t signal);
		E_ObsCode signal_to_code(E_Sys sys, uint8_t signal);
		
		ObsList decodeMSM7(
			uint8_t* data,
			unsigned int message_length,
			map<SatSys,map<E_ObsCode,int>> MSM7_lock_time);
	};
};


struct RtcmStream : ObsStream, NavStream, 
					RtcmDecoder::SSRDecoder,
					RtcmDecoder::EphemerisDecoder,
					RtcmDecoder::MSM7Decoder 
{
	LockTimeInfo lock_time_info_current;
	LockTimeInfo lock_time_info_previous;

	ObsList SuperList;
	map<SatSys,map<E_ObsCode,int>> MSM7_lock_time;

	int numPreambleFound	= 0;
	int numFramesFailedCRC	= 0;
	int numFramesPassCRC	= 0;
	int numFramesDecoded	= 0;
	int numNonMessBytes		= 0;
	
	void parseRTCM(std::istream& inputStream);
	
	// Optional virtual message functions for trace.
	virtual void messageRtcmLog(std::string message){}
	virtual void messageRtcmByteLog(std::string message){}
	virtual void traceBroEph(Eph eph,E_Sys sys){}
// 	virtual void traceSsrEph(SatSys Sat,SSREph ssrEph){}
// 	virtual void traceSsrClk(SatSys Sat,SSRClk ssrClk){}
// 	virtual void traceSsrCodeB(SatSys Sat,E_ObsCode mode, SSRCodeBias ssrBias){}
// 	virtual void traceSsrPhasB(SatSys Sat,E_ObsCode mode, SSRPhasBias ssrBias){}
};


#endif
