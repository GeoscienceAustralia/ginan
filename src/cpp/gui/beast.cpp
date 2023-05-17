
#include "server_certificate.hpp"

#include <boost/beast/core.hpp>
#include <boost/beast/websocket.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/ssl.hpp>
#include <boost/beast/version.hpp>
#include <boost/asio/dispatch.hpp>
#include <boost/asio/strand.hpp>
#include <boost/config.hpp>
#include <algorithm>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <thread>
#include <vector>
#include <string>
#include <list>
#include <map>

// #pragma GCC optimize ("O0")


using std::string;
using std::vector;
using std::list;
using std::map;


namespace beast		= boost::beast;				// from <boost/beast.hpp>
namespace websocket	= beast::websocket;			// from <boost/beast/websocket.hpp>
namespace http		= beast::http;				// from <boost/beast/http.hpp>
namespace net		= boost::asio;				// from <boost/asio.hpp>
namespace ssl		= boost::asio::ssl;			// from <boost/asio/ssl.hpp>
using tcp			= boost::asio::ip::tcp;		// from <boost/asio/ip/tcp.hpp>

// Return a reasonable mime type based on the extension of a file.
beast::string_view
mime_type(beast::string_view path)
{
	using beast::iequals;

	auto const ext = [&path]
	{
		auto const pos = path.rfind(".");

		if (pos == beast::string_view::npos)
			return beast::string_view{};

		return path.substr(pos);
	}();

	if (iequals(ext, ".htm"))	return "text/html";
	if (iequals(ext, ".html"))	return "text/html";
	if (iequals(ext, ".php"))	return "text/html";
	if (iequals(ext, ".css"))	return "text/css";
	if (iequals(ext, ".txt"))	return "text/plain";
	if (iequals(ext, ".js"))	return "application/javascript";
	if (iequals(ext, ".json"))	return "application/json";
	if (iequals(ext, ".xml"))	return "application/xml";
	if (iequals(ext, ".swf"))	return "application/x-shockwave-flash";
	if (iequals(ext, ".flv"))	return "video/x-flv";
	if (iequals(ext, ".png"))	return "image/png";
	if (iequals(ext, ".jpe"))	return "image/jpeg";
	if (iequals(ext, ".jpeg"))	return "image/jpeg";
	if (iequals(ext, ".jpg"))	return "image/jpeg";
	if (iequals(ext, ".gif"))	return "image/gif";
	if (iequals(ext, ".bmp"))	return "image/bmp";
	if (iequals(ext, ".ico"))	return "image/vnd.microsoft.icon";
	if (iequals(ext, ".tiff"))	return "image/tiff";
	if (iequals(ext, ".tif"))	return "image/tiff";
	if (iequals(ext, ".svg"))	return "image/svg+xml";
	if (iequals(ext, ".svgz"))	return "image/svg+xml";

	return "application/text";
}

// Append an HTTP rel-path to a local filesystem path.
// The returned path is normalized for the platform.
string path_cat(
	beast::string_view base,
	beast::string_view path)
{
	if (base.empty())
		return string(path);
	string result(base);
#ifdef BOOST_MSVC
	char constexpr path_separator = '\\';
	if (result.back() == path_separator)
		result.resize(result.size() - 1);
	result.append(path.data(), path.size());
	for (auto& c : result)
		if (c == '/')
			c = path_separator;
#else
	char constexpr path_separator = '/';
	if (result.back() == path_separator)
		result.resize(result.size() - 1);
	result.append(path.data(), path.size());
#endif
	return result;
}

// This function produces an HTTP response for the given
// request. The type of the response object depends on the
// contents of the request, so the interface requires the
// caller to pass a generic lambda for receiving the response.
template <
	class Body, class Allocator,
	class Send >
void
handle_request(
	beast::string_view doc_root,
	http::request<Body, http::basic_fields<Allocator>>&& req,
	Send&& send)
{
	// Returns a bad request response
	auto const bad_request =
		[&req](beast::string_view why)
	{
		http::response<http::string_body> res{http::status::bad_request, req.version()};
		res.set(http::field::server, BOOST_BEAST_VERSION_STRING);
		res.set(http::field::content_type, "text/html");
		res.keep_alive(req.keep_alive());
		res.body() = string(why);
		res.prepare_payload();
		return res;
	};

	// Returns a not found response
	auto const not_found =
		[&req](beast::string_view target)
	{
		http::response<http::string_body> res{http::status::not_found, req.version()};
		res.set(http::field::server, BOOST_BEAST_VERSION_STRING);
		res.set(http::field::content_type, "text/html");
		res.keep_alive(req.keep_alive());
		res.body() = "The resource '" + string(target) + "' was not found.";
		res.prepare_payload();
		return res;
	};

	// Returns a server error response
	auto const server_error =
		[&req](beast::string_view what)
	{
		http::response<http::string_body> res{http::status::internal_server_error, req.version()};
		res.set(http::field::server, BOOST_BEAST_VERSION_STRING);
		res.set(http::field::content_type, "text/html");
		res.keep_alive(req.keep_alive());
		res.body() = "An error occurred: '" + string(what) + "'";
		res.prepare_payload();
		return res;
	};

	// Make sure we can handle the method
	if ( req.method() != http::verb::get &&
			req.method() != http::verb::head)
		return send(bad_request("Unknown HTTP-method"));

	// Request path must be absolute and not contain "..".
	if ( req.target().empty() ||
			req.target()[0] != '/' ||
			req.target().find("..") != beast::string_view::npos)
		return send(bad_request("Illegal request-target"));

	// Build the path to the requested file
	string path = path_cat(doc_root, req.target());
	if (req.target().back() == '/')
		path.append("index.html");

	// Attempt to open the file
	beast::error_code ec;
	http::file_body::value_type body;
	body.open(path.c_str(), beast::file_mode::scan, ec);

	// Handle the case where the file doesn't exist
	if (ec == beast::errc::no_such_file_or_directory)
		return send(not_found(req.target()));

	// Handle an unknown error
	if (ec)
		return send(server_error(ec.message()));

	// Cache the size since we need it after the move
	auto const size = body.size();

	// Respond to HEAD request
	if (req.method() == http::verb::head)
	{
		http::response<http::empty_body> res{http::status::ok, req.version()};
		res.set(http::field::server, BOOST_BEAST_VERSION_STRING);
		res.set(http::field::content_type, mime_type(path));
		res.content_length(size);
		res.keep_alive(req.keep_alive());
		return send(std::move(res));
	}

	// Respond to GET request
	http::response<http::file_body> res
	{
		std::piecewise_construct,
		std::make_tuple(std::move(body)),
		std::make_tuple(http::status::ok, req.version())};
	res.set(http::field::server, BOOST_BEAST_VERSION_STRING);
	res.set(http::field::content_type, mime_type(path));
	res.content_length(size);
	res.keep_alive(req.keep_alive());
	return send(std::move(res));
}

//------------------------------------------------------------------------------

// Report a failure
void fail(beast::error_code ec, char const* what)
{
	std::cerr << what << ": " << ec.message() << "\n";
}

// Handles an HTTP server connection
class httpsession : public std::enable_shared_from_this<httpsession>
{
	// This is the C++11 equivalent of a generic lambda.
	// The function object is used to send an HTTP message.
	struct send_lambda
	{
		httpsession& self_;

		explicit
		send_lambda(httpsession& self)
			: self_(self)
		{
		}

		template<bool isRequest, class Body, class Fields>
		void
		operator()(http::message<isRequest, Body, Fields>&& msg) const
		{
			// The lifetime of the message has to extend
			// for the duration of the async operation so
			// we use a shared_ptr to manage it.
			auto sp = std::make_shared <
					  http::message<isRequest, Body, Fields >> (std::move(msg));

			// Store a type-erased version of the shared
			// pointer in the class to keep it alive.
			self_.res_ = sp;

			// Write the response
			http::async_write(
				self_.stream_,
				*sp,
				beast::bind_front_handler(
					&httpsession::on_write,
					self_.shared_from_this(),
					sp->need_eof()));
		}
	};

	beast::tcp_stream stream_;
	beast::flat_buffer buffer_;
	std::shared_ptr<string const> doc_root_;
	http::request<http::string_body> req_;
	std::shared_ptr<void> res_;
	send_lambda lambda_;

public:
	// Take ownership of the stream
	httpsession(
		tcp::socket&& socket,
		std::shared_ptr<string const> const& doc_root)
		: stream_(std::move(socket))
		, doc_root_(doc_root)
		, lambda_(*this)
	{
	}

	// Start the asynchronous operation
	void run()
	{
		// We need to be executing within a strand to perform async operations
		// on the I/O objects in this session. Although not strictly necessary
		// for single-threaded contexts, this example code is written to be
		// thread-safe by default.
		net::dispatch(stream_.get_executor(),
					  beast::bind_front_handler(
						  &httpsession::do_read,
						  shared_from_this()));
	}

	void do_read()
	{
		// Make the request empty before reading,
		// otherwise the operation behavior is undefined.
		req_ = {};

		// Set the timeout.
		stream_.expires_after(std::chrono::seconds(30));

		// Read a request
		http::async_read(stream_, buffer_, req_,
						 beast::bind_front_handler(
							 &httpsession::on_read,
							 shared_from_this()));
	}

	void on_read(
		beast::error_code ec,
		std::size_t bytes_transferred)
	{
		boost::ignore_unused(bytes_transferred);

		// This means they closed the connection
		if (ec == http::error::end_of_stream)
			return do_close();

		if (ec)
			return fail(ec, "read");

		// Send the response
		handle_request(*doc_root_, std::move(req_), lambda_);
	}

	void on_write(
		bool close,
		beast::error_code ec,
		std::size_t bytes_transferred)
	{
		boost::ignore_unused(bytes_transferred);

		if (ec)
			return fail(ec, "write");

		if (close)
		{
			// This means we should close the connection, usually because
			// the response indicated the "Connection: close" semantic.
			return do_close();
		}

		// We're done with the response so delete it
		res_ = nullptr;

		// Read another request
		do_read();
	}

	void do_close()
	{
		// Send a TCP shutdown
		beast::error_code ec;
		stream_.socket().shutdown(tcp::socket::shutdown_send, ec);

		// At this point the connection is closed gracefully
	}
};

//------------------------------------------------------------------------------

// Accepts incoming connections and launches the sessions
class httplistener : public std::enable_shared_from_this<httplistener>
{
	net::io_context& ioc_;
	tcp::acceptor acceptor_;
	std::shared_ptr<string const> doc_root_;

public:
	httplistener(
		net::io_context& ioc,
		tcp::endpoint endpoint,
		std::shared_ptr<string const> const& doc_root)
		: ioc_(ioc)
		, acceptor_(net::make_strand(ioc))
		, doc_root_(doc_root)
	{
		beast::error_code ec;

		// Open the acceptor
		acceptor_.open(endpoint.protocol(), ec);
		if (ec)
		{
			fail(ec, "open");
			return;
		}

		// Allow address reuse
		acceptor_.set_option(net::socket_base::reuse_address(true), ec);
		if (ec)
		{
			fail(ec, "set_option");
			return;
		}

		// Bind to the server address
		acceptor_.bind(endpoint, ec);
		if (ec)
		{
			fail(ec, "bind");
			return;
		}

		// Start listening for connections
		acceptor_.listen(
			net::socket_base::max_listen_connections, ec);
		if (ec)
		{
			fail(ec, "listen");
			return;
		}
	}

	// Start accepting incoming connections
	void runListener()
	{
		do_accept();
	}

private:
	void do_accept()
	{
		// The new connection gets its own strand
		acceptor_.async_accept(net::make_strand(ioc_), beast::bind_front_handler(&httplistener::on_accept, shared_from_this()));
	}

	void
	on_accept(beast::error_code ec, tcp::socket socket)
	{
		if (ec)
		{
			fail(ec, "accept");
		}
		else
		{
			// Create the session and run it
			std::make_shared<httpsession>(
				std::move(socket),
				doc_root_)->run();
		}

		// Accept another connection
		do_accept();
	}
};







#include "binaryStore.hpp"




#include "anode.hpp"

#include <boost/asio/buffers_iterator.hpp>

#include <condition_variable>

using std::condition_variable;

extern mutex subscribedMapMutex;

extern list<tuple<string,string>> stringList;
extern map<string, bool> subscribedMap;
// extern map<string, bool> requestedMap;
extern map<string, string>	stringMap;

extern 	map<string, vector<GeneralDataEntry>*>	anyPtrMap;
#include "binaryStore.hpp"
extern list<string> msgQueue;
extern list<string> outQueue;
extern mutex outMutex;
extern std::condition_variable cond;


// Echoes back all received WebSocket messages
class websocketsession : public std::enable_shared_from_this<websocketsession>
{
	websocket::stream<beast::tcp_stream> ws_;
	beast::flat_buffer buffer_;

	beast::multi_buffer			sendBuffer;
public:
	// Take ownership of the socket
	explicit
	websocketsession(tcp::socket&& socket)
		: ws_(std::move(socket))
	{
	}

	// Get on the correct executor
	void run()
	{
		// We need to be executing within a strand to perform async operations
		// on the I/O objects in this session. Although not strictly necessary
		// for single-threaded contexts, this example code is written to be
		// thread-safe by default.
		net::dispatch(ws_.get_executor(), beast::bind_front_handler(&websocketsession::on_run, shared_from_this()));
	}

	// Start the asynchronous operation
	void on_run()
	{
		// Set suggested timeout settings for the websocket
		ws_.set_option(websocket::stream_base::timeout::suggested(beast::role_type::server));

		// Set a decorator to change the Server of the handshake
		ws_.set_option(websocket::stream_base::decorator(
			[](websocket::response_type& res)
			{
				res.set(http::field::server, string(BOOST_BEAST_VERSION_STRING) + " websocket-server-async");
			}));
		
		// Accept the websocket handshake
		ws_.async_accept(beast::bind_front_handler(&websocketsession::on_accept,	shared_from_this()));
	}

	void on_accept(beast::error_code ec)
	{
		if (ec)
			return fail(ec, "accept");

// 	boost::beast::ostream(sendBuffer) << "Hello, world!\n";

// 	request_stream << "web socket message from ginan";
		
// 	ws_.async_write(sendBuffer.data(), beast::bind_front_handler(&websocketsession::on_write, shared_from_this()));
		
		// Read a message
// 		do_read();
		doSend();
	}

	void on_write(
		beast::error_code	ec,
		size_t				bytes_transferred);
	
	void do_read();

	void on_read(
		beast::error_code	ec,
		size_t				bytes_transferred);

	
	void doSend();
};

extern bool sendFlag;

void websocketsession::on_read(
		beast::error_code	ec,
		size_t				bytes_transferred)
{
	// This indicates that the session was closed
	if (ec == websocket::error::closed)
		return;

	if (ec)
		fail(ec, "read");

	// Echo the message
// 		ws_.text(ws_.got_text());
	string text = beast::buffers_to_string(buffer_.data());

	buffer_.consume(buffer_.size());
	
	sendFlag = true;
	
	if (text.empty() == false)
	{	
		lock_guard<mutex> lock(subscribedMapMutex);
		
		string name = text.substr(1);
		
		if (text[0] == '+')		
			subscribedMap[name] = true;
		if (text[0] == '-')		
			subscribedMap[name] = false;
		if (text[0] == '%')	
		{
			int pos = name.find("%");
			string id	= name.substr(0,pos);
			string val	= name.substr(pos + 1);
			
			if	( ANode::anodeIdMap[id].get()
				&&ANode::anodeIdMap[id]->numeric)
			{
				double num	= stod(val);
		
				*ANode::anodeIdMap[id]->numeric = num;
			}
			if	( ANode::anodeIdMap[id].get()
				&&ANode::anodeIdMap[id]->textual)
			{
				double num	= stod(val);
		
				*ANode::anodeIdMap[id]->textual = val;
			}
		}
		if (text[0] == '?')
		{
			outQueue.push_back(stringMap[name]);
		}
	}
	
	doSend();
	
	return;

	
	
	std::stringstream ssx;
	std::stringstream ssy;
	
// 		for (auto& [time, val] : *(anyPtrMap[text]))
// 		{
// 			ssx << "\"" << time << "\"" << ",";
// 			ssy << val  << ",";
// 		}
// 		ssx.seekp(-1,ssx.cur);
// 		ssy.seekp(-1,ssy.cur);
// 		
// 		ssx << " ";
// 		ssy << " ";
// 		
// 		boost::beast::ostream(sendBuffer) << "{ \"name\": \"" << text << "\", \"x\": [" << ssx.str() << "], \"y\": [" << ssy.str() << "], \"type\": \"scatter\" }";
// 		std::cout << "sending " << sendBuffer.size() << " bytes" << std::endl;
// 		
// 		ws_.async_write(sendBuffer.data(), beast::bind_front_handler(&websocketsession::on_write, shared_from_this()));
}


void websocketsession::do_read()
{
// 	std::cout << "trying to read\n";
	// Read a message into our buffer
	ws_.async_read(buffer_, beast::bind_front_handler(&websocketsession::on_read, shared_from_this()));
}

void websocketsession::on_write(
		beast::error_code	ec,
		size_t				bytes_transferred)
{
	if (ec)
		return fail(ec, "write");

// 	std::cout << "clearing" << std::endl;
	// Clear the buffer
	sendBuffer.consume(sendBuffer.size());

	// Do another write
	doSend();
}


void websocketsession::doSend()
{
	string value;
	for (auto once : {1})
	{
		std::unique_lock<mutex> lock(outMutex);

		if (msgQueue.empty() == false)
		{
			value = msgQueue.front();
// 			std::cout << value << std::endl;
			msgQueue.pop_front();
			break;
		}
		if (outQueue.empty() == false)
		{
			value = outQueue.front();
// 			std::cout << value << std::endl;
			outQueue.pop_front();
			break;
// 			std::cout << "\nwaiting for data\n";
			//prepare a timeout because the read_until call doesnt seem to return on bad requests.
// 			sendTimer.expires_from_now(boost::posix_time::seconds(5));
// 			sendTimer.async_wait(boost::bind(&websocketsession::timeout_handler, this, bp::error));    
		}
		
		do_read();
		return;
	}
	
	boost::beast::ostream(sendBuffer) << value;
// 	std::cout << "sending " << sendBuffer.size() << " bytes : \n" << value << std::endl;
	
	ws_.async_write(sendBuffer.data(), beast::bind_front_handler(&websocketsession::on_write, shared_from_this()));
}

	
// Accepts incoming connections and launches the sessions
class websocketlistener : public std::enable_shared_from_this<websocketlistener>
{
	net::io_context&	ioc_;
	tcp::acceptor		acceptor_;

public:
	websocketlistener(
		net::io_context&	ioc,
		tcp::endpoint		endpoint)
		: ioc_				(ioc)
		, acceptor_			(ioc)
	{
		beast::error_code ec;

		// Open the acceptor
		acceptor_.open(endpoint.protocol(), ec);
		if (ec)
		{
			fail(ec, "open");
			return;
		}

		// Allow address reuse
		acceptor_.set_option(net::socket_base::reuse_address(true), ec);
		if (ec)
		{
			fail(ec, "set_option");
			return;
		}

		// Bind to the server address
		acceptor_.bind(endpoint, ec);
		if (ec)
		{
			fail(ec, "bind");
			return;
		}

		// Start listening for connections
		acceptor_.listen(net::socket_base::max_listen_connections, ec);
		if (ec)
		{
			fail(ec, "listen");
			return;
		}
	}

	// Start accepting incoming connections
	void runListener()
	{
		// The new connection gets its own strand
		acceptor_.async_accept(net::make_strand(ioc_), beast::bind_front_handler(&websocketlistener::on_accept, shared_from_this()));
	}

	void on_accept(beast::error_code ec, tcp::socket socket)
	{
		if (ec)
		{
			fail(ec, "accept");
		}
		else
		{
			// Create the session and run it
			std::make_shared<websocketsession>(std::move(socket))->run();
		}

		// Accept another connection
		runListener();
	}
};

vector<std::thread> v;

net::io_context beastioc(5);	//threads

void Beasty()
{
	auto const threads = std::max<int>(5, 1);

	// The io_context is required for all I/O
	
// 	if (argc != 5)
// 	{
// 		std::cerr <<
// 				  "Usage: http-server-async-ssl <address> <port> <doc_root> <threads>\n" <<
// 				  "Example:\n" <<
// 				  "    http-server-async-ssl 0.0.0.0 8080 . 1\n";
// 		return EXIT_FAILURE;
// 	}
// 	auto const address	= net::ip::make_address(argv[1]);
// 	auto const port		= static_cast<unsigned short>(std::atoi(argv[2]));
// 	auto const doc_root	= std::make_shared<std::string>(argv[3]);
// 	auto const threads	= std::max<int>(1, std::atoi(argv[4]));
	auto const doc_root	= std::make_shared<string>(".");
	// Create and launch a listening port
	std::make_shared<httplistener>		(beastioc, tcp::endpoint{net::ip::make_address("0.0.0.0"), 8081}, doc_root	)->runListener();
	std::make_shared<websocketlistener>	(beastioc, tcp::endpoint{net::ip::make_address("0.0.0.0"), 8082}				)->runListener();
	// Run the I/O service on the requested number of threads
	
	v.reserve(threads);
	for (int i = 0; i < threads; i++)
		v.emplace_back([] {	beastioc.run();	});
// 	ioc.run();
}
