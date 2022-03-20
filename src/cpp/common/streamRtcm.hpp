
#ifndef __ACSRTCMSTREAM__HPP
#define __ACSRTCMSTREAM__HPP

#include <boost/date_time/posix_time/posix_time.hpp>

#include <map>

using std::map;
using std::pair;

#include "rtcmDecoder.hpp"
#include "ntripTrace.hpp"
#include "streamObs.hpp"
#include "streamNav.hpp"

struct LockTimeInfo
{
	boost::posix_time::ptime	epoch;

	map<pair<uint8_t, uint8_t>, uint16_t> lock_time_indicators;

	LockTimeInfo(){}
	LockTimeInfo(const boost::posix_time::ptime&				epoch,
				const map<pair<uint8_t, uint8_t>, uint16_t>&	lockTimeIndicators)
	:	epoch(epoch),
		lock_time_indicators(lockTimeIndicators)
	{

	}
	
	pair<uint32_t, uint32_t> get_lock_time_and_coefficient(
		pair<uint8_t, uint8_t> cell)
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


struct RtcmStream : ObsStream, NavStream, RtcmDecoder
{
	LockTimeInfo lock_time_info_current;
	LockTimeInfo lock_time_info_previous;

	ObsList SuperList;
	
	static GTime	rtcmDeltaTime;
	
	string			rtcm_filename;
	
	bool record_rtcm = false;
	
	void createRtcmFile();
	
	void parseRTCM(
		std::istream& inputStream);
};


#endif
