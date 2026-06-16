#include <boost/algorithm/string.hpp>
#include "common/acsConfig.hpp"
#include "common/receiver.hpp"
#include "common/rtcmDecoder.hpp"
#include "common/sinex.hpp"

SinexSiteId   dummySiteid;
SinexReceiver dummyReceiver;
SinexAntenna  dummyAntenna;
SinexSiteEcc  dummySiteEcc;

SinexSatIdentity dummySinexSatIdentity;
SinexSatEcc      dummySinexSatEcc;

static string trimmedCopy(const string& value)
{
    return boost::algorithm::trim_copy(value);
}

static void ingestTrimmedReceiverMetaField(
    ReceiverMetaField<string>&          field,
    const string&                       candidate,
    E_ReceiverMetaSource                source,
    const vector<E_ReceiverMetaSource>& priorityOrder
)
{
    string trimmed = trimmedCopy(candidate);

    ingestReceiverMetaField(field, trimmed, trimmed.empty() == false, source, priorityOrder);
}

static bool hasRtcmStationPosition(const RtcmStationInfo& rtcmInfo)
{
    if (rtcmInfo.physicalStationId >= 0)
    {
        return rtcmInfo.physEcefX != 0 || rtcmInfo.physEcefY != 0 || rtcmInfo.physEcefZ != 0;
    }

    return rtcmInfo.ecefX != 0 || rtcmInfo.ecefY != 0 || rtcmInfo.ecefZ != 0;
}

static bool hasConfigAntennaDelta(const ReceiverOptions& recOpts)
{
    return recOpts.eccentricityModel.enable &&
           (isInited(recOpts, recOpts.eccentricityModel.eccentricity) ||
            recOpts.eccentricityModel.eccentricity.isZero() == false);
}

static bool sinexAntennaDelta(const SinexRecData& recSnx, Vector3d& antennaDelta)
{
    if (recSnx.ecc_ptr == &dummySiteEcc)
    {
        return false;
    }

    antennaDelta = Vector3d(recSnx.ecc_ptr->ecc);
    return true;
}

void ReceiverMetadata::reset()
{
    *this = ReceiverMetadata();
}

void ReceiverMetadata::setPriority(const vector<E_ReceiverMetaSource>& priorityOrder)
{
    if (priorityOrder.empty())
    {
        sourcePriority = defaultReceiverMetaSourcePriority();
        return;
    }

    sourcePriority = priorityOrder;
}

void ReceiverMetadata::ingestConfig(const ReceiverOptions& recOpts)
{
    setPriority(recOpts.meta_priority);

    ingestTrimmedReceiverMetaField(
        receiverType,
        recOpts.receiver_type,
        E_ReceiverMetaSource::CONFIG,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        antennaDescriptor,
        recOpts.antenna_type,
        E_ReceiverMetaSource::CONFIG,
        sourcePriority
    );
    ingestReceiverMetaField(
        antennaDelta,
        recOpts.eccentricityModel.eccentricity,
        hasConfigAntennaDelta(recOpts),
        E_ReceiverMetaSource::CONFIG,
        sourcePriority
    );
    ingestReceiverMetaField(
        stationPosition,
        recOpts.apriori_pos,
        recOpts.apriori_pos.isZero() == false,
        E_ReceiverMetaSource::CONFIG,
        sourcePriority
    );
}

void ReceiverMetadata::ingestSinex(const SinexRecData& recSnx)
{
    Vector3d antennaDeltaCandidate = Vector3d::Zero();
    bool     hasAntennaDelta       = sinexAntennaDelta(recSnx, antennaDeltaCandidate);

    ingestTrimmedReceiverMetaField(
        receiverType,
        recSnx.rec_ptr->type,
        E_ReceiverMetaSource::SINEX,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        receiverFirmware,
        recSnx.rec_ptr->firm,
        E_ReceiverMetaSource::SINEX,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        receiverSerial,
        recSnx.rec_ptr->sn,
        E_ReceiverMetaSource::SINEX,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        antennaDescriptor,
        recSnx.ant_ptr->type,
        E_ReceiverMetaSource::SINEX,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        antennaSerial,
        recSnx.ant_ptr->sn,
        E_ReceiverMetaSource::SINEX,
        sourcePriority
    );
    ingestReceiverMetaField(
        antennaDelta,
        antennaDeltaCandidate,
        hasAntennaDelta,
        E_ReceiverMetaSource::SINEX,
        sourcePriority
    );
    ingestReceiverMetaField(
        stationPosition,
        Vector3d(recSnx.pos),
        recSnx.pos.isZero() == false,
        E_ReceiverMetaSource::SINEX,
        sourcePriority
    );
}

void ReceiverMetadata::ingestRinex(const RinexStation& rnxRec)
{
    ingestTrimmedReceiverMetaField(
        receiverType,
        rnxRec.recType,
        E_ReceiverMetaSource::RINEX,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        receiverFirmware,
        rnxRec.recFWVersion,
        E_ReceiverMetaSource::RINEX,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        receiverSerial,
        rnxRec.recSerial,
        E_ReceiverMetaSource::RINEX,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        antennaDescriptor,
        rnxRec.antDesc,
        E_ReceiverMetaSource::RINEX,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        antennaSerial,
        rnxRec.antSerial,
        E_ReceiverMetaSource::RINEX,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        markerName,
        rnxRec.id,
        E_ReceiverMetaSource::RINEX,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        markerNumber,
        rnxRec.marker,
        E_ReceiverMetaSource::RINEX,
        sourcePriority
    );
    ingestReceiverMetaField(
        antennaDelta,
        rnxRec.del,
        true,
        E_ReceiverMetaSource::RINEX,
        sourcePriority
    );
    ingestReceiverMetaField(
        stationPosition,
        rnxRec.pos,
        rnxRec.pos.isZero() == false,
        E_ReceiverMetaSource::RINEX,
        sourcePriority
    );
}

void ReceiverMetadata::ingestRtcm(const RtcmStationInfo& rtcmInfo)
{
    ingestTrimmedReceiverMetaField(
        receiverType,
        rtcmInfo.receiverType,
        E_ReceiverMetaSource::RTCM,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        receiverFirmware,
        rtcmInfo.receiverFirmware,
        E_ReceiverMetaSource::RTCM,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        receiverSerial,
        rtcmInfo.receiverSerial,
        E_ReceiverMetaSource::RTCM,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        antennaDescriptor,
        rtcmInfo.antennaDesc,
        E_ReceiverMetaSource::RTCM,
        sourcePriority
    );
    ingestTrimmedReceiverMetaField(
        antennaSerial,
        rtcmInfo.antennaSerial,
        E_ReceiverMetaSource::RTCM,
        sourcePriority
    );
    ingestReceiverMetaField(
        antennaDelta,
        Vector3d(0, 0, rtcmInfo.antennaHeight),
        rtcmInfo.hasAntennaHeight,
        E_ReceiverMetaSource::RTCM,
        sourcePriority
    );

    Vector3d rtcmPosition = {rtcmInfo.ecefX, rtcmInfo.ecefY, rtcmInfo.ecefZ};
    if (rtcmInfo.physicalStationId >= 0)
    {
        rtcmPosition = {rtcmInfo.physEcefX, rtcmInfo.physEcefY, rtcmInfo.physEcefZ};
    }

    ingestReceiverMetaField(
        stationPosition,
        rtcmPosition,
        hasRtcmStationPosition(rtcmInfo),
        E_ReceiverMetaSource::RTCM,
        sourcePriority
    );
}

void syncReceiverMetadata(Receiver& rec)
{
    rec.receiverType = rec.metadata.receiverType.valid ? rec.metadata.receiverType.value : "";
    rec.antennaType =
        rec.metadata.antennaDescriptor.valid ? rec.metadata.antennaDescriptor.value : "";
    rec.antDelta =
        rec.metadata.antennaDelta.valid ? rec.metadata.antennaDelta.value : VectorEnu::Zero();
}
