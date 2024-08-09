
#pragma once


#include <istream>
#include <memory>
#include <string>

using std::make_unique;
using std::unique_ptr;
using std::string;

#include "streamParser.hpp"


struct FileState : std::ifstream
{
	long int&		filePos;

	FileState(
		string					path,
		long int&				filePos,
		std::ifstream::openmode	mode = std::ifstream::in)
			: filePos {filePos}
	{
		if (filePos < 0)
		{
// 			BOOST_LOG_TRIVIAL(error) << "Error seeking to negative position in file at " << path << " to " << filePos;
			close();
			return;
		}

		open(path, mode);

		if (!*this)
		{
			BOOST_LOG_TRIVIAL(error) << "Error opening file at " << path
			<< "\n" << " - " << strerror(errno);
			filePos = -1;
			return;
		}

		seekg(filePos);

		if (!*this)
		{
			BOOST_LOG_TRIVIAL(error) << "Error seeking in file at " << filePos << " in " << path
			<< "\n" << " - " << strerror(errno);

			filePos = -1;
			return;
		}
	}

	~FileState()
	{
		filePos = streamPos(*this);
	}
};

struct FileStream : Stream
{
	string			path;
	long int		filePos = 0;

	FileStream(
		string	path)
	:	path	(path)
	{

	}

	unique_ptr<std::istream> getIStream_ptr() override
	{
// 		std::cout << "Getting FileStream" << "\n";

		return make_unique<FileState>(path, filePos);
	}

	bool isDead() override
	{
		if (filePos < 0)
		{
			return true;
		}

		auto iStream_ptr = this->getIStream_ptr();

		if	(*iStream_ptr)
		{
			return false;
		}
		else
		{
			return true;
		}
	}
};

