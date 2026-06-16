#pragma once

#include <memory>
#include <string>
#include <vector>
#include "common/sanityCheckers/ISanityChecker.hpp"

struct ConfigSanityManager
{
    void addChecker(std::unique_ptr<ISanityChecker> checker);
    bool runAllChecks(ACSConfig& config) const;

    size_t                   checkerCount() const;
    std::vector<std::string> checkerNames() const;

    static ConfigSanityManager defaultManager();

   private:
    std::vector<std::unique_ptr<ISanityChecker>> checkers;
};
