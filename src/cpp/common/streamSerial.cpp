#include "common/streamSerial.hpp"
#include <iostream>
#include "common/platformCompat.hpp"

void SerialStream::openStream()
{
#ifdef _WIN32
    fileDescriptor = CreateFileA(
        path.c_str(),
        GENERIC_READ | GENERIC_WRITE,
        0,
        NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        NULL
    );

    if (fileDescriptor == INVALID_HANDLE_VALUE)
    {
        std::cout << "\n"
                  << "Error opening " << path << " as SerialStream";
    }
#else
    fileDescriptor = open(path.c_str(), O_RDWR | O_NONBLOCK);

    if (fileDescriptor < 0)
    {
        std::cout << "\n"
                  << "Error opening " << path << " as SerialStream";
    }
#endif
}
