#ifndef RTCMENCODER_H
#define RTCMENCODER_H

#include "navigation.hpp"
#include "satSys.hpp"
#include "observations.hpp"
#include "enums.h"
#include "common.hpp"
#include "ntripTrace.hpp"


struct RtcmEncoder
{
	struct Encoder
	{
		std::vector<uint8_t> data;
        
        void encodeWriteMessages(std::ostream& outputStream);
        void encodeWriteMessageToBuffer(unsigned char * buf, int messLength);
    };
 
    struct CustomEndcoder : Encoder
    {
        void encodeTimeStampRTCM();
    };
    
	struct SSREncoder : Encoder
	{
		void encodeSsr		(bool useSSROut);
        void encodeSsrComb	(E_Sys targetSys, bool useSSROut);
        void encodeSsrPhase	(E_Sys targetSys, bool useSSROut);
        void encodeSsrCode	(E_Sys targetSys, bool useSSROut);
        
        virtual void traceSsrEph(SatSys Sat,SSREph ssrEph){}
        virtual void traceSsrClk(SatSys Sat,SSRClk ssrClk){}
        virtual void traceSsrCodeB(SatSys Sat,E_ObsCode mode, SSRBias ssrBias){}
        virtual void traceSsrPhasB(SatSys Sat,E_ObsCode mode, SSRBias ssrBias){}        
    };
};

#endif
