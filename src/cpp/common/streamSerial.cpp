

#include <iostream>
#include <unistd.h>
#include <fcntl.h>

#include "streamSerial.hpp"

void SerialStream::openStream()
{
	fileDescriptor = open(path.c_str(), O_RDWR | O_NONBLOCK);
	
	if (fileDescriptor < 0)
	{
		std::cout << "\n" << "Error opening " << path << " as SerialStream";
	}
}
