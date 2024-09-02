
#pragma once

#include <unistd.h>
#include <fcntl.h>

#include <string>
#include <vector>

using std::string;
using std::vector;


#include <boost/iostreams/device/array.hpp>
#include <boost/iostreams/stream.hpp>

namespace B_io		= boost::iostreams;


#include "streamParser.hpp"



struct SerialStateMembers
{
	vector<char>&					inputVector;
	B_io::basic_array_source<char>	input_source;

	SerialStateMembers(
		vector<char>&									inputVector)
	:	inputVector										(inputVector),
		input_source (B_io::basic_array_source<char>	(inputVector.data(), inputVector.size()))
	{

	}
};

struct SerialState : SerialStateMembers, B_io::stream<B_io::basic_array_source<char>>
{
	SerialState(
		vector<char>&									inputSource)
	:	SerialStateMembers								(inputSource),
		B_io::stream<B_io::basic_array_source<char>>	(input_source)
	{
// 		std::cout << "Serial State created, has length " << inputVector.size() << "\n";
	}

	~SerialState()
	{
		long int pos = streamPos(*this);

		if		(pos ==	0)
		{
			return;
		}
		else if (pos >	0)
		{
			inputVector.erase(inputVector.begin(), inputVector.begin() + pos);
		}
		else
		{
			inputVector.clear();
		}

// 		std::cout << "Serial State destroyed, has length " << inputVector.size() << "\n";
	}
};

struct SerialStream : Stream
{
	string	path;

	int		fileDescriptor = -1;

	vector<char>	receivedData;

	SerialStream()
	{

	}

	SerialStream(
		string path)
	:	path (path)
	{
		openStream();
	}

	void openStream();

	virtual void getData()
	{
		if (fileDescriptor < 0)
		{
			return;
		}

		while (1)
		{
			const int reserve = 0x4000;
			int oldSize = receivedData.size();

			receivedData.resize(receivedData.size() + reserve);


			int n = read(fileDescriptor, &receivedData[oldSize], reserve);

			receivedData.resize(oldSize + n);

			if (n == 0)
			{
				break;
			}
		}
	}

	unique_ptr<std::istream> getIStream_ptr() override
	{
		getData();

		return make_unique<SerialState>(receivedData);
	}

	virtual ~SerialStream()
	{
		if (fileDescriptor < 0)
		{
			return;
		}

		close(fileDescriptor);
	};
};



