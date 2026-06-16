// #pragma GCC optimize ("O0")

#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>
#include "common/acsConfig.hpp"
#include "common/algebra.hpp"
#include "common/eigenIncluder.hpp"
#include "common/gTime.hpp"
#include "common/navigation.hpp"
#include "common/receiver.hpp"
#include "common/sinex.hpp"
#include "pea/inputsOutputs.hpp"
#include "pea/peaCommitStrings.hpp"
#include "slr/slr.hpp"

using boost::algorithm::to_lower_copy;

void getStationsFromSinex(map<string, Receiver>& receiverMap, KFState& kfState) {}

static string resolvedReceiverType(const Receiver& rec)
{
    return rec.metadata.receiverType.valid ? rec.metadata.receiverType.value : rec.receiverType;
}

static string resolvedAntennaType(const Receiver& rec)
{
    return rec.metadata.antennaDescriptor.valid ? rec.metadata.antennaDescriptor.value
                                                : rec.antennaType;
}

void sinexPostProcessing(GTime time, map<string, Receiver>& receiverMap, KFState& netKFState)
{
    theSinex.inputFiles.clear();
    theSinex.acknowledgements.clear();
    theSinex.inputHistory.clear();

    sinexCheckAddGaReference("PPP Solution", ginanCommitVersion(), false);

    // add in the files used to create the solution
    for (auto& [id, ubxinput] : acsConfig.ubx_inputs)
    {
        sinexAddFiles(acsConfig.analysis_agency, time, ubxinput, "UBX");
    }
    for (auto& [id, sbfinput] : acsConfig.sbf_inputs)
    {
        sinexAddFiles(acsConfig.analysis_agency, time, sbfinput, "SBF");
    }
    for (auto& [id, rnxinput] : acsConfig.rnx_inputs)
    {
        sinexAddFiles(acsConfig.analysis_agency, time, rnxinput, "RINEX v3.x");
    }
    {
        sinexAddFiles(acsConfig.analysis_agency, time, acsConfig.sp3_files, "SP3");
    }
    {
        sinexAddFiles(acsConfig.analysis_agency, time, acsConfig.snx_files, "SINEX");
    }

    // Add other statistics as they become available...
    sinexAddStatistic("SAMPLING INTERVAL (SECONDS)", acsConfig.epoch_interval);

    char obsCode   = 'P';  // GNSS measurements  // Eugene: SLR?
    char constCode = ' ';

    string solcont = "ST";
    // uncomment next bit once integrated
    // if (acsConfig.orbit_output) solcont += 'O';

    string data_agc = "";

    PTime startTime;
    startTime.bigTime = boost::posix_time::to_time_t(
        acsConfig.start_epoch
    );  // todo? make these constructors for ptime.

    KFState sinexSubstate = mergeFilters({&netKFState}, {KF::ONE, KF::REC_POS, KF::REC_POS_RATE});

    updateSinexHeader(
        acsConfig.analysis_agency,
        data_agc,
        (GTime)startTime,
        time,
        obsCode,
        constCode,
        solcont,
        sinexSubstate.x.rows() - 1,
        2.02
    );  // Change this if the sinex format gets updated

    string filename = acsConfig.sinex_filename;

    replaceTimes(filename, acsConfig.start_epoch);

    writeSinex(filename, sinexSubstate, receiverMap, (GTime)startTime, time);
}

void updateReceiverMetadata(GTime time, Receiver& rec)
{
    if (rec.id.empty())
    {
        return;
    }

    rec.failureEccentricity = true;

    // Try config first
    auto& recOpts = acsConfig.getRecOpts(rec.id);
    {
        rec.metadata.ingestConfig(recOpts);
        syncReceiverMetadata(rec);

        rec.failureEccentricity =
            recOpts.eccentricityModel.enable && rec.metadata.antennaDelta.valid == false;
        rec.failureAprioriPos = rec.metadata.stationPosition.valid == false;
    }

    // Try sinex if anything not found from config
    if (rec.failureEccentricity || resolvedReceiverType(rec).empty() ||
        resolvedAntennaType(rec).empty() || rec.failureAprioriPos)
    {
        if ((GTime)rec.snx.stop < time || rec.snx.stop == UYds(0, 0, 0))
        {
            string snxId = rec.id;
            if (cdpIdMap.find(rec.id) != cdpIdMap.end())
            {
                // need to use CDP ID for SLR stations if possible
                int cdpId = cdpIdMap.at(rec.id);
                assert(cdpId >= 1000);  // if fails, need to consider zero-padding in sinex files
                snxId = std::to_string(cdpId);
            }

            auto result = getRecSnx(
                snxId,
                time,
                rec.snx
            );
            rec.failureSinex = result.failureSiteId;
        }

        if (rec.failureSinex == false)
        {
            rec.metadata.ingestSinex(rec.snx);
            syncReceiverMetadata(rec);

            rec.failureEccentricity =
                recOpts.eccentricityModel.enable && rec.metadata.antennaDelta.valid == false;
            rec.failureAprioriPos = rec.metadata.stationPosition.valid == false;
        }
    }
}

void sinexPerEpochPerStation(Trace& trace, GTime time, Receiver& rec)
{
    if (rec.id.empty())
    {
        return;
    }

    {
        // Eugene: Delete this?
        auto& solEpoch = theSinex.solEpochMap[rec.id];

        solEpoch.sitecode = rec.id;
        solEpoch.ptcode   = "A";
        solEpoch.solnnum  = "1";
        solEpoch.typecode = 'P';  // GPS by default  // Eugene: SLR?
        if ((GTime)solEpoch.start == GTime::noTime())
            solEpoch.start = time;
        solEpoch.end = time;
        solEpoch.mean =
            (GTime)solEpoch.start + ((GTime)solEpoch.end - (GTime)solEpoch.start).to_double() / 2;
    }

    updateReceiverMetadata(time, rec);

    // Update receiver options
    string receiverType = resolvedReceiverType(rec);
    if (receiverType.empty() == false)
    {
        receiverType = to_lower_copy(receiverType);
        receiverType = receiverType.substr(0, receiverType.find(" "));

        auto [it, inserted] = acsConfig.customAliasesMap[rec.id].insert(receiverType);
        if (inserted)
        {
            auto& baseRecOpts = acsConfig.getRecOpts((string) "_" + rec.id);

            for (auto& [id, inheritor] : baseRecOpts.inheritors)
            {
                inheritor->_initialised = false;
            }
        }
    }

    // Initialise the antenna information
    {
        string antennaType = resolvedAntennaType(rec);
        string tmpant      = antennaType;

        if (tmpant.empty())
        {
            BOOST_LOG_TRIVIAL(warning) << "Antenna name not specified for " << rec.id;
            trace << "Antenna name not specified for " << rec.id << "\n";

            return;
        }

        bool found;
        found = findAntenna(tmpant, E_Sys::GPS, time, nav, F1);
        if (found)
        {
            // all good, carry on
            rec.antennaId = tmpant;
            return;
        }

        // Try searching under the antenna type with DOME => NONE
        radome2none(tmpant);

        found = findAntenna(tmpant, E_Sys::GPS, time, nav, F1);
        if (found)
        {
            BOOST_LOG_TRIVIAL(warning) << "Using '" << tmpant << "' instead of: '" << antennaType
                                       << "' for radome of " << rec.id;
            trace << "Using '" << tmpant << "' instead of: '" << antennaType << "' for radome of "
                  << rec.id << "\n";

            rec.antennaId = tmpant;
            return;
        }

        BOOST_LOG_TRIVIAL(warning) << "No information for antenna " << antennaType;
        trace << "No information for antenna " << antennaType << "\n";
    }
}
