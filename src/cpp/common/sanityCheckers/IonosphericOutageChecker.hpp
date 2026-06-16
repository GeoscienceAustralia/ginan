#pragma once

#include "common/sanityCheckers/ISanityChecker.hpp"

struct IonosphericOutageChecker : ISanityChecker
{
    bool        check(ACSConfig& config) override;
    std::string name() const override;
};
