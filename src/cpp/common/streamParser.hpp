
#pragma once

#include <iostream>
#include <sstream>
#include <utility>
#include <vector>
#include <string>
#include <thread>
#include <map>


using std::multimap;
using std::string;
using std::tuple;
using std::pair;
using std::map;


#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/iostreams/device/array.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/trivial.hpp>
#include <boost/date_time.hpp>
#include <boost/format.hpp>
#include <boost/regex.hpp>
#include <boost/asio.hpp>

namespace B_io		= boost::iostreams;
namespace B_asio	= boost::asio;

using boost::asio::ip::tcp;





long int streamPos(
	std::istream& stream);










#include <memory>
#include <istream>

using std::unique_ptr;
using std::make_unique;



struct Stream
{
	string sourceString;

	virtual unique_ptr<std::istream> getIStream_ptr() = 0;

	/** Check to see if this stream has run out of data
	*/
	virtual bool isDead()
	{
		return false;
	}

	virtual ~Stream() = default;
};

struct Parser
{
	virtual void parse(std::istream& inputStream) = 0;

	virtual string parserType() = 0;

	virtual ~Parser() = default;
};


struct SerialStream;

struct StreamParser
{
private:
	unique_ptr<Stream>	stream_ptr;
	unique_ptr<Parser>	parser_ptr;

	//for debugging access only
	SerialStream*		serialStream_ptr;

public:
	Stream&	stream;
	Parser&	parser;

	StreamParser(
		unique_ptr<Stream> stream_ptr,
		unique_ptr<Parser> parser_ptr)
	:	stream_ptr		(std::move(stream_ptr)),
		parser_ptr		(std::move(parser_ptr)),
		stream			(*this->stream_ptr),
		parser			(*this->parser_ptr)
	{
		serialStream_ptr = (SerialStream*) &stream;
	}

	operator Stream&()
	{
		return stream;
	}

	operator Parser&()
	{
		return parser;
	}

	void parse()
	{
		auto iStream_ptr = stream.getIStream_ptr();
		parser.parse(*iStream_ptr);
	}

	virtual ~StreamParser() = default;
};

typedef std::shared_ptr<StreamParser>		StreamParserPtr;

extern	multimap<string, StreamParserPtr>					streamParserMultimap;
extern	map		<string, bool>								streamDOAMap;
