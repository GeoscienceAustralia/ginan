#pragma once

#include <mutex>
#include "common/streamSerial.hpp"
#include "common/tcpSocket.hpp"
struct NtripResponder : TcpSocket
{
    NtripResponder(const string& url_str) : TcpSocket(url_str) {}

    void requestResponseHandler(const boost::system::error_code& err) override;

    virtual void serverResponse(unsigned int statusCode, string httpVersion)
    {
        std::cout << "Code Error: No server response defined" << std::endl;
    };
};

struct NtripStream : NtripResponder
{
    NtripStream(const string& url_str) : NtripResponder(url_str)
    {
        std::stringstream requestStream;
        requestStream << "GET " << url.path << " HTTP/1.1" << "\r\n";
        requestStream << "Host: " << url.host << "\r\n";
        requestStream << "Ntrip-Version: Ntrip/2.0" << "\r\n";
        requestStream << "User-Agent: NTRIP ACS/1.0" << "\r\n";
        if (!url.user.empty())
        {
            requestStream << "Authorization: Basic "
                          << Base64::encode(string(url.user + ":" + url.pass)) << "\r\n";
        }
        requestStream << "Connection: close" << "\r\n";
        requestStream << "\r\n";

        requestString = requestStream.str();

        connect();
    }

    void serverResponse(unsigned int statusCode, string httpVersion) override;

    ~NtripStream(){};
};
