#include "acsConfig.hpp"
#include "common.hpp"
#include "sisnet.hpp"
#include "sbas.hpp"

#include <mutex>

using std::lock_guard;
using std::mutex;


#define CLEAN_UP_AND_RETURN_ON_FAILURE	\
										\
	if (inputStream.fail())				\
	{									\
		inputStream.clear();			\
		inputStream.seekg(pos);			\
		return;							\
	}

namespace bp = boost::asio::placeholders;

void DS2DCParser::parse(
	std::istream& inputStream)
{
	string buff;

    while (inputStream)
	{
    	std::getline(inputStream, buff);

		if (buff.empty())
			continue;

		auto strStart = buff.find("*MSG");

		if (strStart == string::npos)
		{
			BOOST_LOG_TRIVIAL(error) << "Invalid SISNET message:  " << buff;

			continue;
		}

		string	tmpBuff	= buff.substr(strStart + 5);
		int		week	= std::stoi(tmpBuff, &strStart);

		tmpBuff	= buff.substr(strStart + 6);

		auto strEnd = tmpBuff.find(",");

		double tow = std::stod(tmpBuff.substr(0,strEnd));

		string mess = tmpBuff.substr(strEnd + 1);

		size_t index;

		for (auto expander : {'|', '/'})
		while (index = mess.find(expander), index != string::npos)
		{
			int digits = 0;
			if (expander == '|')	digits = 1;
			if (expander == '/')	digits = 2;

			char character = mess[index - 1];

			int repeat = std::stoi(mess.substr(index+1, digits), nullptr, 16)-1;

			string replacement(repeat, character);

			mess.replace(index, digits + 1, replacement);
		}

		auto tail = mess.find("*");

		SBASMessage sbas;

		sbas.prn		= acsConfig.sbsInOpts.prn;
		sbas.freq		= acsConfig.sbsInOpts.freq;
		sbas.message	= mess.substr(0, tail);

		if (sbas.freq == 1)			sbas.type = std::stoi(mess.substr(2,2), nullptr, 16) >> 2;
		else						sbas.type = std::stoi(mess.substr(1,2), nullptr, 16) >> 2;

		BOOST_LOG_TRIVIAL(debug) << "SISNET message: " << sbas.message;

		/* CRC check */
		unsigned char	frame[29]	= {};
		unsigned int	frameCRC	= 0;

		for (int i = 0; i < 32; i++)
		{
			unsigned char byte = (unsigned char) std::stoi(sbas.message.substr(2*i,2), nullptr, 16);

			sbas.data[i] = byte;

			if 		(i <  28)	{ frame[i]+=((byte>>6) & 0xFFFF);	frame[i+1]	+=((byte<< 2) & 0x00FFFF);	}
			else if (i == 28)	{ frame[i]+=((byte>>6) & 0xFFFF);	frameCRC	+=((byte<<18) & 0xFC0000);	}
			else if (i == 29)	{									frameCRC	+=((byte<<10) & 0x03FC00);	}
			else if (i == 30)	{									frameCRC	+=((byte<< 2) & 0x0003FC);	}
			else if (i == 31)	{									frameCRC	+=((byte>> 6) & 0x000003);	}
		}

		unsigned int calcCRC = crc24q(frame, 29);

		if (calcCRC != frameCRC)
		{
			BOOST_LOG_TRIVIAL(error) << "CRC error in SISNET channel";
			continue;
		}

		GTime frametime = gpst2time(week, tow);

		{
			lock_guard<mutex> guard(sbasMessagesMutex);

			for (auto it = sbasMessages.begin(); it != sbasMessages.end(); )
			{
				auto& [time, sbasData] = *it;
				if ((frametime - time).to_double() > 90)
				{
					it = sbasMessages.erase(it);
				}
				else
				{
					it++;
				}
			}

			sbasMessages[frametime] = sbas;
		}
	}
	inputStream.clear();
}

void SisnetStream::sisnetHandler(
	const boost::system::error_code& err)
{
    if (err)
	{
		delayedReconnect();
		return;
	}

	vector<char> responseVec;
	int nread = downloadBuf.size();
	responseVec.resize(nread);
	buffer_copy(boost::asio::buffer(responseVec), downloadBuf.data());
	downloadBuf.consume(nread);

	dataChunkDownloaded(responseVec);

	boost::asio::async_read_until(*_socket,		downloadBuf, "\n", boost::bind(&SisnetStream::sisnetHandler, this, bp::error));
}

void SisnetStream::sisnetResponseHandler(
	const boost::system::error_code& err)
{
    if (err)
	{
		return;
		// delayed_reconnect();
	}

	vector<char> responseVec;

	int nread = downloadBuf.size();
	responseVec.resize(nread);

	buffer_copy(boost::asio::buffer(responseVec), downloadBuf.data());
	downloadBuf.consume(nread);

	string private_string;
	private_string.assign(responseVec.begin(), responseVec.end());

	size_t pos = private_string.find("*START");
	if (pos == string::npos)
	{
		BOOST_LOG_TRIVIAL(error) << "Error in starting string : Invalid Server Response:  " << private_string;
		delayedReconnect();
		return;
	}

	isConnected = true;

	boost::asio::socket_base::keep_alive option(true);
	socket_ptr->set_option(option);

	// The connection was successful. Send the request.
	boost::asio::async_read_until(*_socket,		downloadBuf, "\n", boost::bind(&SisnetStream::sisnetHandler, this, bp::error));
}

void SisnetStream::sisnetStartHandler(
	const boost::system::error_code& err)
{
    if (err)
	{
    	delayedReconnect();
    	return;
	}

	timer.expires_from_now(boost::posix_time::seconds(10));
	timer.async_wait(boost::bind(&SisnetStream::timeoutHandler, this, boost::asio::placeholders::error));

	boost::asio::async_read_until(*_socket, downloadBuf, "\n", boost::bind(&SisnetStream::sisnetResponseHandler, this, boost::asio::placeholders::error));
}

void SisnetStream::requestResponseHandler(
	const boost::system::error_code& err)
{
    if (err)
	{
    	delayedReconnect();
    	return;
	}

	vector<char> responseVec;

	int nread = downloadBuf.size();
	responseVec.resize(nread);

	buffer_copy(boost::asio::buffer(responseVec), downloadBuf.data());
	downloadBuf.consume(nread);

	string privateString;
	privateString.assign(responseVec.begin(), responseVec.end());

	size_t pos = privateString.find("*AUTH");
	if (pos == string::npos)
	{
		BOOST_LOG_TRIVIAL(error) << "Error in Authentication : Invalid Server Response:  " << privateString;
		delayedReconnect();
		return;
	}

	// boost::this_thread::sleep( boost::posix_time::milliseconds(100));

	int n = request.size();
	request.consume(n);

    std::ostream    requestStream(&request);
					requestStream << "START\n";

	// The connection was successful. Send the request.
	boost::asio::async_write(*_socket, request, boost::bind(&SisnetStream::sisnetStartHandler, this, boost::asio::placeholders::error));
}
