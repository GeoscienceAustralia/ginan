#pragma once

#include <fstream>
#include <istream>
#include <memory>
#include <string>
#include "common/streamParser.hpp"

using std::make_unique;
using std::string;
using std::unique_ptr;

struct FileState : std::istream
{
    std::ifstream& persistentStream;
    long int&      filePos;

    FileState(
        std::ifstream&          persistentStream,
        const string&           path,
        long int&               filePos,
        std::ifstream::openmode mode = std::ifstream::in | std::ios::binary
    )
        : std::istream(nullptr), persistentStream{persistentStream}, filePos{filePos}
    {
        this->persistentStream.clear();

        if (filePos < 0)
        {
            this->persistentStream.setstate(std::ios::failbit);
            setstate(std::ios::failbit);
            return;
        }

        if (this->persistentStream.is_open() == false)
        {
            this->persistentStream.open(path, mode);

            if (!this->persistentStream)
            {
                BOOST_LOG_TRIVIAL(error) << "Error opening file at " << path;
                filePos = -1;
                setstate(std::ios::failbit);
                return;
            }
        }

        this->persistentStream.seekg(filePos);

        if (!this->persistentStream)
        {
            BOOST_LOG_TRIVIAL(error) << "Error seeking in file at " << filePos << " in " << path;

            filePos = -1;
            setstate(std::ios::failbit);
            return;
        }

        rdbuf(this->persistentStream.rdbuf());
        clear();
    }

    ~FileState() { filePos = streamPos(*this); }
};

struct FileStream : Stream
{
    string        path;
    long int      filePos = 0;
    std::ifstream persistentStream;

    FileStream(const string& path) : path(path) {}

    unique_ptr<std::istream> getIStream_ptr() override
    {
        return make_unique<FileState>(persistentStream, path, filePos);
    }

    bool isDead() override { return filePos < 0; }

    bool isAvailable() override
    {
        if (persistentStream.is_open())
        {
            return true;
        }

        return std::filesystem::exists(path) && std::filesystem::is_regular_file(path);
    }
};
