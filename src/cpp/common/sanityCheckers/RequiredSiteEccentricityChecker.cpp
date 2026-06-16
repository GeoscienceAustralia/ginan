#include "common/sanityCheckers/RequiredSiteEccentricityChecker.hpp"
#include <boost/log/trivial.hpp>
#include "common/acsConfig.hpp"

bool RequiredSiteEccentricityChecker::check(ACSConfig& config)
{
    if (config.require_site_eccentricity == false)
    {
        return true;
    }

    bool valid = true;

    for (auto& [id, recOpts] : config.recOptsMap)
    {
        if (recOpts.eccentricityModel.enable)
        {
            continue;
        }

        valid = false;
        setOption(recOpts, recOpts.eccentricityModel.enable, true);
        BOOST_LOG_TRIVIAL(warning) << "Site eccentricity is required but `" << id
                                   << ": models: eccentricity` is not enabled, setting it to true";
    }

    return valid;
}

std::string RequiredSiteEccentricityChecker::name() const
{
    return "RequiredSiteEccentricityChecker";
}
