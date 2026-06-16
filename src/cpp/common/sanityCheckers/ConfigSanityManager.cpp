#include "common/sanityCheckers/ConfigSanityManager.hpp"
#include <boost/log/trivial.hpp>
#include "common/sanityCheckers/EphemerisTimeDelayChecker.hpp"
#include "common/sanityCheckers/EpochToleranceChecker.hpp"
#include "common/sanityCheckers/IonosphericFreeComboChecker.hpp"
#include "common/sanityCheckers/IonosphericOutageChecker.hpp"
#include "common/sanityCheckers/RequiredSiteEccentricityChecker.hpp"
#include "common/sanityCheckers/SbasSanityChecker.hpp"

void ConfigSanityManager::addChecker(std::unique_ptr<ISanityChecker> checker)
{
    if (checker)
    {
        checkers.push_back(std::move(checker));
    }
}

bool ConfigSanityManager::runAllChecks(ACSConfig& config) const
{
    bool allPassed = true;

    for (auto& checker : checkers)
    {
        try
        {
            allPassed &= checker->check(config);
        }
        catch (const std::exception& e)
        {
            allPassed = false;
            BOOST_LOG_TRIVIAL(error) << "Exception in configuration sanity checker "
                                     << checker->name() << ": " << e.what();
        }
    }

    return allPassed;
}

size_t ConfigSanityManager::checkerCount() const
{
    return checkers.size();
}

std::vector<std::string> ConfigSanityManager::checkerNames() const
{
    std::vector<std::string> names;
    names.reserve(checkers.size());

    for (auto& checker : checkers)
    {
        names.push_back(checker->name());
    }

    return names;
}

ConfigSanityManager ConfigSanityManager::defaultManager()
{
    ConfigSanityManager manager;

    manager.addChecker(std::make_unique<EpochToleranceChecker>());
    manager.addChecker(std::make_unique<RequiredSiteEccentricityChecker>());
    manager.addChecker(std::make_unique<IonosphericOutageChecker>());
    manager.addChecker(std::make_unique<EphemerisTimeDelayChecker>());
    manager.addChecker(std::make_unique<IonosphericFreeComboChecker>());
    manager.addChecker(std::make_unique<SbasSanityChecker>());

    return manager;
}
