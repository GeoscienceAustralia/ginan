
// #pragma GCC optimize ("O0")

#include <map>

#include "streamParser.hpp"


multimap<string, StreamParserPtr>					streamParserMultimap;
map		<string, bool>								streamDOAMap;





long int streamPos(
	std::istream& stream)
{
// 			std::cout << "Closed" << "\n";
	if (stream)
	{
		long int filePos = stream.tellg();

		if (!stream)
		{
			BOOST_LOG_TRIVIAL(error) << "Error telling in file at " << filePos << "\n" << " - " << strerror(errno);

			return -1;
		}

		if (filePos < 0)
		{
			BOOST_LOG_TRIVIAL(error) << "Error: Negative file pos in file at " << filePos << "\n" << " - " << strerror(errno);

			return -1;
		}

		return filePos;
	}
	else
	{
// 		BOOST_LOG_TRIVIAL(error) << "InputStream is dead before destruction ";

		if (stream.eof())
		{
// 			BOOST_LOG_TRIVIAL(error) << "InputStream has end of file ";
		}
		return -1;
	}
}

