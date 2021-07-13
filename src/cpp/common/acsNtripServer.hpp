#ifndef NTRIPSERVER_H
#define NTRIPSERVER_H

#include <iostream>
#include <thread>
#include <utility>
#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

#include "navigation.hpp"
#include "rtcmEncoder.hpp"

extern nav_t nav;

struct NtripServer
{
	using ssl_socket = boost::asio::ssl::stream<boost::asio::ip::tcp::socket>;
	
	
	struct NtripClient : ssl_socket
	{
		NtripClient(NtripServer& t_parentServer, boost::asio::io_context& io_context,boost::asio::ssl::context& ssl_context)
			:ssl_socket(io_context, ssl_context),parentServer(t_parentServer){};
		void initialize();
		
		void printMessage(std::vector<uint8_t> data);
		void negotiationError();
		
		bool deleteClient = false;
		bool readyForPrint = false;
		std::mutex printMtx;
		NtripServer& parentServer;
	};
	
public:
	void startServer();
	~NtripServer();
	std::mutex listenMtx;
	
	int portNumber;
	std::string sslCertifateFileName;
	std::string sslPrivateKeyFileName;
	std::string mountPoint;
	float loopDelaySeconds;
	std::map<std::string,std::string> userNamePasswords;
	
private:
	void monitorThreads();
	void startListening();
	void broadcastLoop();
	void sendMessages(std::istream& inputStream);
	void startService();
	
	boost::asio::io_service io_service;
	std::mutex shutDownMtx;
	std::mutex startMtx;
	bool shutDownServer = false;
	std::vector<std::shared_ptr<NtripClient>> clients;
};

#endif // NTRIPSERVER_H
