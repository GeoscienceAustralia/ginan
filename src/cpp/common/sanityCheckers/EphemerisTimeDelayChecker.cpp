#include "common/sanityCheckers/EphemerisTimeDelayChecker.hpp"
#include "common/acsConfig.hpp"

bool EphemerisTimeDelayChecker::check(ACSConfig& config)
{
    if (config.simulate_real_time)
    {
        return true;
    }

    for (E_Sys sys : magic_enum::enum_values<E_Sys>())
    {
        config.eph_time_delay[sys] = config.default_eph_time_delay[sys];
    }

    return true;
}

std::string EphemerisTimeDelayChecker::name() const
{
    return "EphemerisTimeDelayChecker";
}
