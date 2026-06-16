#include "common/sanityCheckers/EpochToleranceChecker.hpp"
#include <boost/log/trivial.hpp>
#include "common/acsConfig.hpp"

bool EpochToleranceChecker::check(ACSConfig& config)
{
    if (config.epoch_tolerance <= config.epoch_interval / 2)
    {
        return true;
    }

    BOOST_LOG_TRIVIAL(warning) << "`epoch_tolerance` should not exceed half of "
                                  "`epoch_interval`, setting it to `epoch_interval / 2`";
    config.epoch_tolerance = config.epoch_interval / 2;

    return false;
}

std::string EpochToleranceChecker::name() const
{
    return "EpochToleranceChecker";
}
