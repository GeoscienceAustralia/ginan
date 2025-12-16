#include "pea/peaCommitStrings.hpp"
#include <fstream>
#include "pea/peaCommitVersion.h"
#include "pea/peaLibVersion.h"

string ginanCommitHash()
{
    return GINAN_COMMIT_HASH;
}

string ginanCommitVersion()
{
    return GINAN_COMMIT_VERSION;
}

string ginanBranchName()
{
    return GINAN_BRANCH_NAME;
}

string ginanCommitDate()
{
    return GINAN_COMMIT_DATE;
}

string ginanEigenVersion()
{
    return GINAN_EIGEN;
}

string ginanMongoVersion()
{
    return GINAN_MONGOCXX;
}

string ginanCompilerVersion()
{
    return GINAN_COMPILER;
}

string ginanBoostVersion()
{
    return GINAN_BOOST;
}

string ginanOsName()
{
#if defined(_WIN32) || defined(_WIN64)
    // Under MSVC/MinGW, _WIN32 is defined for both 32-bit and 64-bit.
    // Use pointer size to determine bitness robustly (works under Wine too).
    if (sizeof(void*) == 8)
    {
        return "Windows 64-bit";
    }
    else
    {
        return "Windows 32-bit";
    }

#elif __APPLE__ || __MACH__

    std::ifstream fileStream("/System/Library/CoreServices/SystemVersion.plist");
    if (!fileStream)
    {
        return "Mac OSX";
    }

    string productName;
    string productVersion;

    string line;
    while (getline(fileStream, line))
    {
        if (line.find("ProductName") != string::npos)
        {
            productName = "next";
            continue;
        }
        if (line.find("ProductVersion") != string::npos)
        {
            productVersion = "next";
            continue;
        }

        for (auto str_ptr : {&productName, &productVersion})
        {
            auto& str = *str_ptr;

            if (str != "next")
            {
                continue;
            }

            auto startStr = line.find("<string>");
            auto stopStr  = line.find("</string>");

            if (startStr == string::npos || stopStr == string::npos)
            {
                str = "";
                continue;
            }

            str = line.substr(startStr + 8, stopStr - startStr - 8);
        }
    }

    if (productName.empty() || productVersion.empty())
    {
        return "Mac OSX";
    }

    return productName + " " + productVersion;

#elif defined(__linux__)

    std::ifstream fileStream("/etc/os-release");
    if (!fileStream)
    {
        return "Linux";
    }

    string line;
    while (getline(fileStream, line))
    {
        const string prefix = "PRETTY_NAME=";

        if (line.substr(0, prefix.size()) == prefix)
        {
            return line.substr(prefix.size());
        }
    }

    return "Linux";

#else

    return "Other";

#endif
}