
#pragma once

#include <boost/date_time/posix_time/posix_time.hpp>


#include "streamParser.hpp"
#include "rtcmDecoder.hpp"
#include "streamObs.hpp"


#define CLEAN_UP_AND_RETURN_ON_FAILURE	\
										\
	if (inputStream.fail())				\
	{									\
		inputStream.clear();			\
		inputStream.seekg(pos);			\
		return;							\
	}					



struct RtcmParser : Parser, RtcmDecoder
{
	void parse(
		std::istream& inputStream)
	{
// 		std::cout << "Parsing rtcm" << std::endl;
		
		while (inputStream)
		{
			int byteCnt = 0;
			int pos;
			while (true)
			{
				// Skip to the start of the frame - marked by preamble character 0xD3
				pos = inputStream.tellg();

				unsigned char c;
				inputStream.read((char*)&c, 1);																			CLEAN_UP_AND_RETURN_ON_FAILURE;
				
				if (inputStream)
				{
					if (c == RTCM_PREAMBLE)
					{
						break;
					}
				}
				else
				{
					printf(".");
					return;
				}
				
				nonFrameByteFound(c);
			}
			CLEAN_UP_AND_RETURN_ON_FAILURE;
			
			preambleFound();
			
			// Read the frame length - 2 bytes big endian only want 10 bits
			char buf[2];									inputStream.read((char*)buf,				2);				CLEAN_UP_AND_RETURN_ON_FAILURE;
			
			// Message length is 10 bits starting at bit 6
			auto messageLength		= getbitu((uint8_t*)buf, 6,	10);
			auto dataFrameLength	= messageLength + 3;

			// Read the frame data (include the header)
			vector<unsigned char> data(dataFrameLength);	inputStream.read((char*)data.data() + 3,	messageLength);	CLEAN_UP_AND_RETURN_ON_FAILURE;

			// Read the frame CRC
			unsigned int crcRead = 0;						inputStream.read((char*)&crcRead,			3);				CLEAN_UP_AND_RETURN_ON_FAILURE;

			data[0] = RTCM_PREAMBLE;
			data[1] = buf[0];
			data[2] = buf[1];
			unsigned int crcCalc = crc24q(data.data(), data.size());

			if	( (((char*)&crcCalc)[0] != ((char*)&crcRead)[2])
				||(((char*)&crcCalc)[1] != ((char*)&crcRead)[1])
				||(((char*)&crcCalc)[2] != ((char*)&crcRead)[0]))
			{
				checksumFailure(rtcmMountpoint);
				
				inputStream.seekg(pos + 1);
				
				continue;
			}
			
			checksumSuccess();

			recordFrame(data, crcRead);
			
			//remove the header to get to the meat of the message
			auto& message = data;
			message.erase(message.begin(), message.begin()+3);
			
			auto rtcmReturnType = decode(message);
			
			if (rtcmReturnType == E_ReturnType::GOT_OBS)		{								return;		}
			if (rtcmReturnType == E_ReturnType::WAIT)			{	inputStream.seekg(pos);		return;		}
			
		}
	}
	
	string parserType()
	{
		return "RtcmParser";
	}
};

