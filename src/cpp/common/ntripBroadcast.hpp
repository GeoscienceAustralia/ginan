#pragma once

#include <mutex>
#include "common/acsConfig.hpp"
#include "common/enums.h"
#include "common/navigation.hpp"
#include "common/ntripTrace.hpp"
#include "common/rtcmEncoder.hpp"
#include "common/streamNtrip.hpp"
#include "common/tcpSocket.hpp"

struct NtripUploader : NtripResponder, RtcmEncoder
{
    boost::posix_time::ptime timeNextMessage;

    // This mutex ensures that the main thread and the io_service thread do
    // not alter the outMessages buffer at the same time.
    std::mutex                  outMessagesMtx;
    boost::asio::streambuf      outMessages;
    boost::asio::deadline_timer sendTimer;
    int                         numberChunksSent = 0;

    GTime previousTargetTime;  ///< Time to prevent aliasing

    string ntripStr;
    string id = "NtripUploader";

    SsrBroadcast streamConfig;

    NtripUploader(const string& url_str) : NtripResponder(url_str), sendTimer(ioContext)
    {
        if (url.path.empty())
        {
            BOOST_LOG_TRIVIAL(error) << "Ntrip uploader created with empty url";

            return;
        }

        rtcmMountpoint = url.path.substr(1);  // remove '/'

        if (acsConfig.output_encoded_rtcm_json)
        {
            rtcmTraceFilename = acsConfig.encoded_rtcm_json_filename;
        }

        std::stringstream requestStream;
        requestStream << "POST " << url.path << " HTTP/1.1" << "\r\n";
        requestStream << "Host: " << url.host << "\r\n";
        requestStream << "Ntrip-Version: Ntrip/2.0" << "\r\n";
        if (!url.user.empty())
        {
            requestStream << "Authorization: Basic "
                          << Base64::encode(string(url.user + ":" + url.pass)) << "\r\n";
        }
        requestStream << "User-Agent: NTRIP ACS/1.0" << "\r\n";
        if (!ntripStr.empty())
            requestStream << "Ntrip-STR: " << ntripStr << "\r\n";
        requestStream << "Connection: keep-alive" << "\r\n";
        requestStream << "Transfer-Encoding: chunked" << "\r\n";
        requestStream << "\r\n";

        requestString = requestStream.str();

        connect();
    };

    void startBroadcast();

    void connected() override;

    void messageTimeoutHandler(const boost::system::error_code& err);

    void writeHandler(const boost::system::error_code& err);

    void serverResponse(unsigned int status_code, string http_version) override;

    void getJsonNetworkStatistics(GTime now);
};

struct NtripBroadcaster
{
    void startBroadcast();
    void stopBroadcast();

    map<string, std::shared_ptr<NtripUploader>> ntripUploadStreams;
};

extern NtripBroadcaster ntripBroadcaster;
