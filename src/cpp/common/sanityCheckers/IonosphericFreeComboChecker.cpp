#include "common/sanityCheckers/IonosphericFreeComboChecker.hpp"
#include <boost/log/trivial.hpp>
#include "common/acsConfig.hpp"

bool IonosphericFreeComboChecker::check(ACSConfig& config)
{
    if (config.pppOpts.ionoOpts.use_if_combo == false)
    {
        return true;
    }

    bool valid = true;

    for (auto& [id, recOpts] : config.recOptsMap)
    {
        if (recOpts.ionospheric_component2)
        {
            valid = false;
            setOption(recOpts, recOpts.ionospheric_component2, false);
            BOOST_LOG_TRIVIAL(warning)
                << "Higher-order ionospheric corrections are not supported when "
                   "use_if_combo is enabled, "
                   "setting ionospheric_components:use_2nd_order to false";
        }

        if (recOpts.ionospheric_component3)
        {
            valid = false;
            setOption(recOpts, recOpts.ionospheric_component3, false);
            BOOST_LOG_TRIVIAL(warning)
                << "Higher-order ionospheric corrections are not supported when "
                   "use_if_combo is enabled, "
                   "setting ionospheric_components:use_3rd_order to false";
        }
    }

    return valid;
}

std::string IonosphericFreeComboChecker::name() const
{
    return "IonosphericFreeComboChecker";
}
