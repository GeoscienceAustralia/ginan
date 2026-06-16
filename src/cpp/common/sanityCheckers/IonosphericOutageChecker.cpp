#include "common/sanityCheckers/IonosphericOutageChecker.hpp"
#include <boost/log/trivial.hpp>
#include "common/acsConfig.hpp"

bool IonosphericOutageChecker::check(ACSConfig& config)
{
    if (config.ionErrors.outage_reset_limit >= config.epoch_interval)
    {
        return true;
    }

    BOOST_LOG_TRIVIAL(warning) << "ionospheric_components:outage_reset_limit < "
                                  "epoch_interval, but it probably shouldnt be";
    return false;
}

std::string IonosphericOutageChecker::name() const
{
    return "IonosphericOutageChecker";
}
