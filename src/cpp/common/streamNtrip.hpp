
#pragma once

#include "streamSerial.hpp"
#include "tcpSocket.hpp"


#include <mutex>



struct NtripStream : TcpSocket
{
	NtripStream(
		const string& url_str)
		:	TcpSocket(url_str)
	{
		std::stringstream	requestStream;
							requestStream	<< "GET " 		<< url.path << " HTTP/1.1"				<< "\r\n";
							requestStream	<< "Host: " 	<< url.host								<< "\r\n";
							requestStream	<< "Ntrip-Version: Ntrip/2.0"							<< "\r\n";
							requestStream	<< "User-Agent: NTRIP ACS/1.0"							<< "\r\n";
		if (!url.user.empty())
		{
							requestStream	<< "Authorization: Basic "
											<< Base64::encode(string(url.user + ":" + url.pass))	<< "\r\n";
		}
							requestStream	<< "Connection: close"									<< "\r\n";
							requestStream	<< "\r\n";

		requestString = requestStream.str();

		connect();
	}

	void requestResponseHandler(
		const boost::system::error_code& err)
	override;

	void serverResponse(
		unsigned int	statusCode,
		string			httpVersion);

	~NtripStream(){};
};

