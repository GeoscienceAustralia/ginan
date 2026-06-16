#include "common/sanityCheckers/SbasSanityChecker.hpp"
#include <boost/log/trivial.hpp>
#include "common/acsConfig.hpp"

bool SbasSanityChecker::check(ACSConfig& config)
{
    bool valid = true;

    if (config.sbsInOpts.freq == 1)
    {
        if (config.sbsInOpts.use_do259)
        {
            valid                      = false;
            config.sbsInOpts.use_do259 = false;
            BOOST_LOG_TRIVIAL(warning)
                << "DO-259 is not to be used with L1 SBAS. Setting use_do259 to false";
        }

        if (config.sbsInOpts.pvs_on_dfmc)
        {
            valid                        = false;
            config.sbsInOpts.pvs_on_dfmc = false;
            BOOST_LOG_TRIVIAL(warning)
                << "PVS on DFMC is not to be used with L1 SBAS. Setting pvs_on_dfmc to false";
        }
    }

    if (config.process_sbas == false)
    {
        return valid;
    }

    config.process_preprocessor = true;
    config.process_spp          = true;

    config.used_nav_types = config.sbsOpts.sbas_nav_types;

    for (auto& [id, satOpts] : config.satOptsMap)
    {
        vector<E_Source> sources = {E_Source::SBAS};
        setOption((CommonOptions&)satOpts, satOpts.posModel.enable, true);
        setOption((CommonOptions&)satOpts, satOpts.posModel.sources, sources);
        setOption((CommonOptions&)satOpts, satOpts.clockModel.enable, true);
        setOption((CommonOptions&)satOpts, satOpts.clockModel.sources, sources);
    }

    switch (config.sbsOpts.mode)
    {
        case E_SbasMode::L1:
        {
            BOOST_LOG_TRIVIAL(info)
                << "L1 SBAS processing mode is selected, make sure that:\n"
                   "   - You have inputs containing SBAS messages (sisnet, ems, sbf, etc.)\n"
                   "   - Parameter `sbas_inputs: prec_approach` is set appropriately";

            config.sbsInOpts.freq = 1;

            for (auto& [sys, process] : config.process_sys)
            {
                if (sys != E_Sys::GPS && sys != E_Sys::GLO && sys != E_Sys::SBS)
                {
                    process = false;
                }
                else
                {
                    config.code_priorities[sys] = {E_ObsCode::L1C};
                }
            }

            config.sppOpts.trop_models = {E_TropModel::SBAS};
            config.sppOpts.iono_mode   = E_IonoMode::SBAS;

            if (config.sppOpts.smooth_window != 100)
            {
                valid                        = false;
                config.sppOpts.smooth_window = 100;
                BOOST_LOG_TRIVIAL(warning)
                    << "It is recommended that a 100 second smoothing window be used for L1 "
                       "SBAS. Changing configuration";
            }

            if (config.sppOpts.use_smooth_only == false)
            {
                valid                          = false;
                config.sppOpts.use_smooth_only = true;
                BOOST_LOG_TRIVIAL(warning)
                    << "It is NOT recommended that measurements be used for SBAS before "
                       "smoothing. Changing configuration";
            }

            if (config.sbsOpts.use_sbas_rec_var == false)
            {
                valid                           = false;
                config.sbsOpts.use_sbas_rec_var = true;
                BOOST_LOG_TRIVIAL(warning)
                    << "It is recommended that measurement variance specific for SBAS are "
                       "used. Changing configuration";
            }

            if (config.sbsInOpts.use_do259)
            {
                valid                      = false;
                config.sbsInOpts.use_do259 = false;
                BOOST_LOG_TRIVIAL(warning)
                    << "DO-259 is not to be use with L1 SBAS. Setting use_do259 to false";
            }

            if (config.sbsInOpts.pvs_on_dfmc)
            {
                valid                        = false;
                config.sbsInOpts.pvs_on_dfmc = false;
                BOOST_LOG_TRIVIAL(warning)
                    << "PVS on DFMC is not to be use with L1 SBAS. Setting pvs_on_dfmc to false";
            }

            break;
        }

        case E_SbasMode::DFMC:
        {
            BOOST_LOG_TRIVIAL(info)
                << "DFMC processing mode is selected, make sure that:\n"
                   "   - You have inputs containing SBAS messages (sisnet, ems, sbf, etc.)\n"
                   "   - If using a service follwing DO-259 (instead of DO-259A), set "
                   "`sbas_inputs: use_do259: true`\n"
                   "   - If using measurements from GLO or BDS, set the `code_priorities` and "
                   "`used_nav_type` properly\n";

            config.sbsInOpts.freq        = 5;
            config.sbsInOpts.pvs_on_dfmc = false;

            for (auto& [sys, process] : config.process_sys)
            {
                if (sys == E_Sys::GLO || sys == E_Sys::LEO)
                {
                    process = false;
                }
                else if (sys != E_Sys::BDS)
                {
                    config.code_priorities[sys] = config.sbsOpts.sbas_code_priorities_map[sys];
                }
            }

            config.sppOpts.trop_models = {E_TropModel::SBAS};
            config.sppOpts.iono_mode   = E_IonoMode::SBAS;

            if (config.sppOpts.smooth_window < 0)
            {
                BOOST_LOG_TRIVIAL(warning)
                    << "It is recommended that a 100 second smoothing window be used for DFMC. "
                       "Please check your configuration";
            }

            break;
        }

        case E_SbasMode::PVS:
        {
            BOOST_LOG_TRIVIAL(info)
                << "PVS-via-DFMC processing mode is selected, make sure that:\n"
                   "   - You have inputs containing SBAS messages (sisnet, ems, sbf, etc.)\n"
                   "   - The SBAS messages come from SouthPAN's DFMC services\n"
                   "   The following processing options will be used:\n"
                   "       - GPS and/or GAL constellations will be used (with GPS as refeence "
                   "system)\n"
                   "       - Saastamoinen model wil be used for troposphere delay "
                   "mapping/estimation\n"
                   "       - If using solid earth tide models, ocean tide loading will be applied, "
                   "while atmospheric tide loading and pole tide loadings will not\n";

            config.process_ppp = true;

            config.sbsInOpts.freq        = 5;
            config.sbsInOpts.pvs_on_dfmc = true;

            for (auto& [sys, process] : config.process_sys)
            {
                if (sys == E_Sys::GPS || sys == E_Sys::GAL)
                {
                    process                     = true;
                    config.code_priorities[sys] = config.sbsOpts.sbas_code_priorities_map[sys];
                }
                else
                {
                    process = false;
                }
            }

            for (auto& [id, recOpts] : config.recOptsMap)
            {
                vector<E_TropModel> tropModels = {E_TropModel::STANDARD};
                setOption(recOpts, recOpts.receiver_reference_system, E_Sys::GPS);
                setOption(recOpts, recOpts.tropModel.enable, true);
                setOption(recOpts, recOpts.tropModel.models, tropModels);
                if (recOpts.tideModels.solid)
                {
                    setOption(recOpts, recOpts.tideModels.otl, true);
                    setOption(recOpts, recOpts.tideModels.atl, false);
                    setOption(recOpts, recOpts.tideModels.spole, false);
                    setOption(recOpts, recOpts.tideModels.opole, false);
                }
            }

            config.sppOpts.always_reinitialise   = true;
            config.pppOpts.use_primary_signals   = true;
            config.errorAccumulation.enable      = true;
            config.ambErrors.phase_reject_limit  = 2;
            config.ambErrors.resetOnSlip.LLI     = true;
            config.ambErrors.resetOnSlip.retrack = true;

            break;
        }
    }

    return valid;
}

std::string SbasSanityChecker::name() const
{
    return "SbasSanityChecker";
}
