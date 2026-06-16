#pragma once

#include "common/sanityCheckers/ISanityChecker.hpp"

struct EpochToleranceChecker : ISanityChecker
{
    bool        check(ACSConfig& config) override;
    std::string name() const override;
};
