
#include <map>

#include "packetStatistics.hpp"
#include "streamUbx.hpp"


#define CLEAN_UP_AND_RETURN_ON_FAILURE	\
										\
	if (inputStream.fail())				\
	{									\
		inputStream.clear();			\
		inputStream.seekg(pos);			\
		return;							\
	}					


void UbxParser::parse(
	std::istream& inputStream)
{
// 		std::cout << "Parsing ubx" << "\n";
	
	while (inputStream)
	{
		int pos;
		unsigned char c1 = 0;
		unsigned char c2 = 0;
		while (true)
		{
			pos = inputStream.tellg();

			//move c2 back one and replace, (check one byte at a time, not in pairs)
			c1 = c2;
			inputStream.read((char*)&c2, 1);

			if (inputStream)
			{
				if	( c1 == UBX_PREAMBLE1
					&&c2 == UBX_PREAMBLE2)
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

		//account for 2 byte preamble
		pos--;
		
		preambleFound();

		unsigned char			ubxClass		= 0;		inputStream.read((char*)&ubxClass,			1);					CLEAN_UP_AND_RETURN_ON_FAILURE;
		unsigned char			id				= 0;		inputStream.read((char*)&id,				1);					CLEAN_UP_AND_RETURN_ON_FAILURE;
		unsigned short int		payload_length	= 0;		inputStream.read((char*)&payload_length,	2);					CLEAN_UP_AND_RETURN_ON_FAILURE;
		vector<unsigned char>	payload(payload_length);	inputStream.read((char*)payload.data(),		payload_length);	CLEAN_UP_AND_RETURN_ON_FAILURE;	// Read the frame data (include the header)
		unsigned short int		crcRead			= 0;		inputStream.read((char*)&crcRead,			2);					CLEAN_UP_AND_RETURN_ON_FAILURE;
		
		//todo aaron calculate crcRead
		if (0)
		{
			checksumFailure();
			
			inputStream.seekg(pos + 1);
			
			continue;
		}
		
		checksumSuccess();
		
		recordFrame(ubxClass, id, payload, crcRead);
		
		decode(ubxClass, id, payload);
		
		if (obsListList.size() > 2)
		{
			return;
		}
	}
	inputStream.clear();
}
	
