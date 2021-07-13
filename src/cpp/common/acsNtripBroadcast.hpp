#ifndef NTRIPSSRBROADCASTER_H
#define NTRIPSSRBROADCASTER_H



#include "acsNtripStream.hpp"
#include "rtcmEncoder.hpp"
#include "ntripSocket.hpp"
#include "ntripTrace.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "enums.h"



template<typename T>
extern std::ofstream getTraceFile(T& thing);

struct NtripBroadcaster
{
	struct NtripUploadClient : NtripSocket, RtcmEncoder::SSREncoder
	{
	private:

		// This mutex ensures that the main thread and the io_service thread do
		// not alter the outMessages buffer at the same time.
		std::mutex outMessagesMtx;
		boost::asio::streambuf outMessages;
		
		int numberChunksSent = 0;
	public:
		bool print_stream_statistics = false;
		
		string	ntripStr = "";
		
		// Message Type and Broadcast Interval in milliseconds.
		vector<RtcmMessageType> rtcmMessagesTypes;
		
		
		NtripTrace	ntripTrace;
		//std::shared_ptr<std::ofstream> trace;
		string traceFilename;
		string		id = "NtripUploadClient";
		
		NtripUploadClient(
			const string& url_str) 
		: NtripSocket(url_str)
		{
			std::stringstream request_stream;
										request_stream	<< "POST " 		<< url.path << " HTTP/1.1\r\n";
										request_stream	<< "Host: " 	<< url.host << "\r\n";
										request_stream	<< "Ntrip-Version: Ntrip/2.0\r\n";
			if (!url.username.empty())
			{
										request_stream	<< "Authorization: Basic "
														<< Base64::encode(string(url.username + ":" + url.password))
														<< "\r\n";
			}
										request_stream	<< "User-Agent: NTRIP ACS/1.0\r\n";
			if (!ntripStr.empty())	    request_stream	<< "Ntrip-STR: " << ntripStr << "\r\n";
										request_stream	<< "Connection: close\r\n";
										request_stream	<< "\r\n";
			request_string = request_stream.str();
			
			connect();
		};
		
		void connected() override;
		void sendMessages(bool useSsrOut);
		void writeResponse(const boost::system::error_code& err);
		
		string getJsonNetworkStatistics(system_clock::time_point epochTime);
		
		void makeOutputTrace()
		{
			auto trace = getTraceFile((*this));
			traceWriteEpoch(trace);
		}
	
		void traceSsrEph(
			SatSys Sat, 
			SSREph ssrEph)
		override
		{
			ntripTrace.traceSsrEph(Sat,ssrEph);
		}

		void traceSsrClk(
			SatSys Sat, 
			SSRClk ssrClk) 
		override
		{
			ntripTrace.traceSsrClk(Sat,ssrClk);
		}

		void traceSsrCodeB(
			SatSys Sat,
			E_ObsCode mode, 
			SSRCodeBias ssrBias)
		override
		{
			ntripTrace.traceSsrCodeB(Sat, mode, ssrBias);
		}

		void traceSsrPhasB(
			SatSys Sat,
			E_ObsCode mode, 
			SSRPhasBias ssrBias)
		override
		{
			ntripTrace.traceSsrPhasB(Sat, mode, ssrBias);
		}

		void messageChunkLog(
			string message) 
		override 
		{
			ntripTrace.messageChunkLog(message);
		}
		
		void networkLog(
			string message)
		override
		{
			ntripTrace.networkLog(message);
		}

		void traceMakeNetworkOverview(
			Trace& trace);

		void traceWriteEpoch(
			Trace& trace)
		{
			traceMakeNetworkOverview(trace);
			ntripTrace.traceWriteEpoch(trace);
		}
	};
	
	void sendMessages(bool useSsrOut);
	void stopBroadcast();
	
	std::multimap<string, std::shared_ptr<NtripUploadClient>> ntripUploadStreams;
	
};

#endif
