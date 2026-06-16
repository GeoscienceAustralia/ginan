#pragma once

#include <string>

struct ACSConfig;

struct ISanityChecker
{
    virtual ~ISanityChecker() = default;

    virtual bool        check(ACSConfig& config) = 0;
    virtual std::string name() const             = 0;
};
