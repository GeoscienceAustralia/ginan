
#pragma once

#include <boost/date_time/posix_time/posix_time.hpp>


#include "streamParser.hpp"
#include "rtcmDecoder.hpp"
#include "streamObs.hpp"
#include "constants.hpp"
#include "otherSSR.hpp"


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
	bool qzssL6 = false;
	vector<unsigned char> qzsL6buff;
	int qzsL6BitsLeft = 0;

	void parse(
		std::istream& inputStream)
	{
// 		std::cout << "Parsing rtcm" << "\n";

		if (qzssL6)			//todo aaron move to own decoder type
		{
			int pos;

			int mess_state = 0;
			const unsigned char L6header[4] = {0x1A,0xCF,0xFC,0x1D};

			vector<unsigned char> frame;
			frame.resize(250,0x00);
			unsigned char* data	= frame.data();


			while (inputStream)
			{
				if (mess_state == 0)
					pos = inputStream.tellg();

				if (mess_state < 4) 			//todo aaron, change to fifo for preamble
				{
					unsigned char c;
					inputStream.read((char*)&c, 1); 	CLEAN_UP_AND_RETURN_ON_FAILURE;
					if (c == L6header[mess_state])
						data[mess_state++] = c;
					else
						mess_state = 0;
				}

				if (mess_state < 4)
					continue;

				inputStream.read((char*)data+4,  2);		CLEAN_UP_AND_RETURN_ON_FAILURE;
				inputStream.read((char*)data+6,244);		CLEAN_UP_AND_RETURN_ON_FAILURE;

				// consider a RS decoder here


				int sz = (qzsL6BitsLeft + 1695) / 8 + 1;
				qzsL6buff.resize(sz);

				unsigned char* buf	= qzsL6buff.data();

				int j = 49;

				for (int i = 0; i < 113; i++)
				{
					unsigned int tmp  = getbituInc(data, j, 15);
					qzsL6BitsLeft = setbituInc(buf, qzsL6BitsLeft, 15, tmp);
				}

				tracepdeex(6, std::cout,"\n New QZSS L6 frame\n");

				int frameBits = 1;
				while (frameBits > 0)
				{
					for (auto& byte : qzsL6buff)
						tracepdeex(6, std::cout,"%02X", static_cast<int>(byte));
					tracepdeex(6, std::cout,"\n");

					int i = 0;
					int messType = getbituInc(buf, i, 12);

					if (messType == 4073)		//todo aaron enum
					{
						frameBits = decodecompactSSR(qzsL6buff, rtcmTime());
					}
					else
					{
						frameBits = 0;

						if (messType > 0)
						{
							tracepdeex(1, std::cout,"  WARNING: Error decoding QZSS L6 messages");

							while	(  messType != 4073
									&& (frameBits+13) < qzsL6BitsLeft)
							{
								frameBits++;
								int j = frameBits;
								messType    = getbituInc(buf, j, 12);
								if (messType == 4073) //todo aaron enum
								{
									int subtype = getbituInc(buf, j, 4);
									if	(  subtype != 6
										&& subtype != 12)
									{
										messType = 0;
									}
								}
							}
							if (messType != 4073)
								frameBits = 0;
						}
					}

					if (frameBits == -2)
					{
						inputStream.seekg(pos);
						tracepdeex(4, std::cout,"    Future frame detected, waiting");
						return;
					}

					if (frameBits == -1)
						tracepdeex(4, std::cout,"    Incomplete frame detected, topping up ");

					if (frameBits == 0)
					{
						qzsL6buff.clear();
						qzsL6BitsLeft = 0;
					}

					if (frameBits > 0)
					{
						int i = frameBits;
						qzsL6BitsLeft -= frameBits;

						tracepdeex(4, std::cout,"    Valid frame detected (%d Bits)", frameBits);

						int nByte = frameBits / 8 + 1;
						int nbit  = nByte * 8 - frameBits;
						int tmp   = getbituInc(buf, i, nbit);

						j = setbituInc(buf, 0, nbit, tmp);

						nbit  = qzsL6BitsLeft - nbit;
						i = nByte;
						while (nbit > 8)
						{
							j     = setbituInc(buf, j, 8, qzsL6buff[i++]);
							nbit -= 8;
						}
						j = setbituInc(buf, j, 8, qzsL6buff[i]);

						nByte = qzsL6BitsLeft / 8 + 1;
						qzsL6buff.resize(nByte);
						buf = qzsL6buff.data();

						if (nByte<5)
							frameBits=-1;
					}
				}
				mess_state=0;
			}
		}
		else
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

// 				printHex(std::cout, data);

				inputStream.seekg(pos + 1);

				continue;
			}

			checksumSuccess(crcRead);

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

