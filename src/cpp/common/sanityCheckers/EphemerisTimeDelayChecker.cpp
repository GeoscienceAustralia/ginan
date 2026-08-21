#include "common/sanityCheckers/EphemerisTimeDelayChecker.hpp"
#include "common/acsConfig.hpp"

bool EphemerisTimeDelayChecker::check(ACSConfig& config)
{
    if (config.netOpts.uploadingStreamData.empty())
    {
        return true;
    }

    bool pass = true;

    for (auto [sys, proc] : config.process_sys)
    {
        if (proc == false)
            continue;

        double time_delay = config.eph_time_delay[sys];

        if (time_delay < 30)
        {
            BOOST_LOG_TRIVIAL(warning)
                << "`sys_options:" << enum_to_string(sys) << ":eph_time_delay` is set to "
                << time_delay
                << ". A value of at least 30 seconds is recommended for uploading SSR streams";

            pass = false;
        }
    }

    return pass;
}

std::string EphemerisTimeDelayChecker::name() const
{
    return "EphemerisTimeDelayChecker";
}
