
#pragma once

#include <iostream>
#include <string>
#include <vector>

using std::string;
using std::vector;

#include "streamSerial.hpp"
#include "tcpSocket.hpp"


#include <mutex>

struct DS2DCParser: Parser
{
    void parse(
		std::istream& inputStream);

	string parserType()
	{
		return "DS2DCParser";
	}
};

struct SisnetStream : TcpSocket
{
    SisnetStream(
		const string& path)
		:	TcpSocket(path, "\n")
	{
		std::stringstream	requestStream;
							requestStream << "AUTH,";
							requestStream << url.user;
							requestStream << ',';
							requestStream << url.pass;
							requestStream << '\n';

		requestString = requestStream.str();

		connect();
	}

	void sisnetHandler			(const boost::system::error_code& err);
	void sisnetResponseHandler	(const boost::system::error_code& err);
	void sisnetStartHandler		(const boost::system::error_code& err);
	void requestResponseHandler	(const boost::system::error_code& err)	override;

	~SisnetStream(){};
};
