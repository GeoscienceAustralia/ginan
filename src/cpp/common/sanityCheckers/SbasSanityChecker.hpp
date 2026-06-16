#pragma once

#include "common/sanityCheckers/ISanityChecker.hpp"

struct SbasSanityChecker : ISanityChecker
{
    bool        check(ACSConfig& config) override;
    std::string name() const override;
};
