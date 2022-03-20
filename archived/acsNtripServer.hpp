#ifndef NTRIPSERVER_H
#define NTRIPSERVER_H

#include <iostream>
#include <utility>
#include <thread>
#include <vector>

using std::vector;

#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

#include "navigation.hpp"
#include "rtcmEncoder.hpp"

extern nav_t nav;

struct NtripServer;

using ssl_socket = boost::asio::ssl::stream<boost::asio::ip::tcp::socket>;

struct NtripClient : ssl_socket
{
	NtripClient(
		NtripServer&				t_parentServer,
		boost::asio::io_context&	io_context,
		boost::asio::ssl::context& 	ssl_context)
													:	ssl_socket(io_context, ssl_context),
														parentServer(t_parentServer){};
		
	void initialize();
	
	void printMessage(
		vector<uint8_t> data);
	
	void negotiationError();
	
	bool 			deleteClient	= false;
	bool 			readyForPrint	= false;
	std::mutex 		printMtx;
	NtripServer& 	parentServer;
};

struct NtripServer
{
	void startServer();
	
	~NtripServer();
	
	std::mutex				listenMtx;

	int 					portNumber;
	string					sslCertifateFileName;
	string					sslPrivateKeyFileName;
	string					mountPoint;
	float					loopDelaySeconds;
	
private:
	void monitorThreads();

	void startListening();

	void broadcastLoop();

	void sendMessages(
		std::istream& inputStream);

	void startService();

	boost::asio::io_service	io_service;
	std::mutex				shutDownMtx;
	std::mutex				startMtx;
	bool					shutDownServer = false;
	vector<std::shared_ptr<NtripClient>> clients;			//todo aaron, this is a weird structure
};

#endif 
