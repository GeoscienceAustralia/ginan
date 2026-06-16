#pragma once

#include <mutex>

#include "common/acsConfig.hpp"
#include "common/enums.h"
#include "common/gTime.hpp"
#include "common/observations.hpp"
#include "common/packetStatistics.hpp"
#include "common/rtcmEncoder.hpp"
#include "common/rtcmTrace.hpp"
#include "common/streamObs.hpp"

struct SignalInfo
{
    uint8_t   signal_id;
    E_FType   ftype;
    E_ObsCode obsCode;
};

struct RtcmStationInfo
{
    // From messages 1005/1006 (Stationary RTK Reference ARP)
    double ecefX            = 0;  ///< Reference station ECEF X (m)
    double ecefY            = 0;  ///< Reference station ECEF Y (m)
    double ecefZ            = 0;  ///< Reference station ECEF Z (m)
    double antennaHeight    = 0;  ///< Antenna height above marker (m), populated by 1006
    bool   hasAntennaHeight = false;
    bool   gpsSys           = false;
    bool   gloSys           = false;
    bool   galSys           = false;
    bool   refStation       = false;
    bool   singleOsc        = false;
    int    quarterCycle     = 0;

    // From messages 1007/1008 (Antenna Descriptors)
    string antennaDesc;
    int    antennaSetupId = 0;
    string antennaSerial;  ///< populated by 1008

    // From message 1033 (Antenna and Receiver Descriptor)
    string receiverType;
    string receiverFirmware;
    string receiverSerial;

    // From message 1032 (Physical Reference Station Position)
    int    physicalStationId = -1;  ///< -1 if not yet received
    double physEcefX         = 0;
    double physEcefY         = 0;
    double physEcefZ         = 0;
};

inline const RtcmStationInfo* selectRtcmStationInfoForMetadata(
    const map<int, RtcmStationInfo>& stationInfoMap,
    int                              lastReferenceStationId
)
{
    if (lastReferenceStationId >= 0)
    {
        auto it = stationInfoMap.find(lastReferenceStationId);
        if (it != stationInfoMap.end())
        {
            return &it->second;
        }

        return nullptr;
    }

    if (stationInfoMap.size() == 1)
    {
        return &stationInfoMap.begin()->second;
    }

    return nullptr;
}

struct RtcmDecoder : RtcmTrace, ObsLister, PacketStatistics
{
    static double rtcmDeltaTime;  ///< Common time used among all rtcmDecoders for delaying decoding
                                  ///< when realtime is enabled
    static map<GTime, int> receivedTimeMap;

    map<int, RtcmStationInfo> stationInfoMap;  ///< Station metadata keyed by RTCM station ID
    int                       lastReferenceStationId = -1;

    GTime lastTimeStamp;

    GTime receivedTime;    ///< Recent internal time from decoded rtcm messages

    ObsList superObsList;  ///< List to accumulate observations from smaller lists which share a
                           ///< common time

    static uint16_t message_length(char header[2]);

    static RtcmMessageType message_type(const uint8_t message[]);

    int adjGpsWeek(int week);

    int adjGstWeek(int week);

    int adjBdtWeek(int week);

    void traceLatency(GTime gpsTime);

    constexpr static int updateInterval[16] =
        {1, 2, 5, 10, 15, 30, 60, 120, 240, 300, 600, 900, 1800, 3600, 7200, 10800};

    GTime rtcmTimestampTime;
    GWeek rtcmWeek = -1;

    E_ObsCode signal_to_code(E_Sys sys, uint8_t signal);

    GTime rtcmTime();

    void decodeEphemeris(vector<unsigned char>& message);

    void decodeSSR(vector<unsigned char>& message);

    GTime decodeCustomTimestamp(vector<unsigned char>& message);

    E_RTCMSubmessage decodeCustomId(vector<unsigned char>& message);

    ObsList decodeMSM(vector<unsigned char>& message);

    string        recordFilename;
    std::ofstream recordFile;
    std::mutex    recordFileMutex;

    void setRecordFilename(const string& filename)
    {
        std::lock_guard<std::mutex> lock(recordFileMutex);

        if (filename == recordFilename)
            return;

        if (recordFile.is_open())
        {
            recordFile.flush();
            recordFile.close();
        }

        recordFilename = filename;
    }

    void recordFrame(vector<unsigned char>& data, unsigned int crcRead)
    {
        if (recordFilename.empty())
        {
            return;
        }

        std::lock_guard<std::mutex> lock(recordFileMutex);

        if (recordFile.is_open() == false)
        {
            recordFile.open(recordFilename, std::ios::app | std::ios::binary);
        }

        if (!recordFile)
        {
            return;
        }

        // Write the custom time stamp message.
        RtcmEncoder encoder;
        setTraceFilename(rtcmTraceFilename);

        auto buffer = encoder.encodeTimeStampRTCM();
        bool write  = encoder.encodeWriteMessageToBuffer(buffer);

        if (write)
        {
            encoder.encodeWriteMessages(recordFile);
        }

        // copy the message to the output file too
        recordFile.write((char*)data.data(), data.size());
        recordFile.write((char*)&crcRead, 3);
    }

    ~RtcmDecoder()
    {
        std::lock_guard<std::mutex> lock(recordFileMutex);
        if (recordFile.is_open())
        {
            recordFile.flush();
            recordFile.close();
        }
    }

    E_ReturnType decodeCustom(vector<unsigned char>& message)
    {
        E_RTCMSubmessage submessage = decodeCustomId(message);

        switch (submessage)
        {
            case (E_RTCMSubmessage::TIMESTAMP):
            {
                GTime timeStamp = decodeCustomTimestamp(message);

                rtcmTimestampTime = timeStamp;

                if (acsConfig.simulate_real_time)
                {
                    // get the current time and compare it with the timestamp in the message
                    GTime now = timeGet();

                    // find the delay between creation of the timestamp, and now
                    double thisDeltaTime = (now - timeStamp).to_double();

                    // initialise the global rtcm delay if needed
                    if (rtcmDeltaTime == 0)
                    {
                        rtcmDeltaTime = thisDeltaTime;
                    }

                    // if the delay is shorter than the global, go back and wait until it is longer
                    if (thisDeltaTime < rtcmDeltaTime)
                    {
                        return E_ReturnType::WAIT;
                    }

                    if (acsConfig.output_decoded_rtcm_json)
                        traceTimestamp(timeStamp);

                    break;
                }

                if (0)
                {
                    int& waitingStreams = receivedTimeMap[timeStamp];

                    if (lastTimeStamp == GTime::noTime())
                    {
                        lastTimeStamp = timeStamp;
                        waitingStreams++;

                        return E_ReturnType::WAIT;
                    }

                    if (timeStamp != lastTimeStamp)
                    {
                        lastTimeStamp = timeStamp;
                        waitingStreams++;
                    }

                    auto& [firstTime, count] = *receivedTimeMap.begin();

                    if (timeStamp > firstTime)
                    {
                        return E_ReturnType::WAIT;
                    }

                    if (timeStamp < firstTime)
                    {
                        std::cout << "unexpected time here" << "\n";
                        exit(1);
                    }

                    // we are the head of the pack, decrement/remove, and process
                    waitingStreams--;

                    if (waitingStreams <= 0)
                    {
                        receivedTimeMap.erase(timeStamp);
                    }
                }

                if (acsConfig.output_decoded_rtcm_json)
                    traceTimestamp(timeStamp);

                break;
            }
            default:
            {
                if (acsConfig.output_decoded_rtcm_json)
                    traceUnknown();

                break;
            }
        }

        return E_ReturnType::OK;
    }

    E_ReturnType decode(vector<unsigned char>& message);
};

uint32_t getbitu(const unsigned char* buff, int pos, int len);
int32_t  getbits(const unsigned char* buff, int pos, int len, bool* failure_ptr = nullptr);
uint32_t getbituInc(const unsigned char* buff, int& pos, int len);
int32_t  getbitsInc(const unsigned char* buff, int& pos, int len, bool* failure_ptr = nullptr);

/** Bounds-checked variant of getbituInc().
 * Returns false instead of reading when the requested bit range is outside the
 * supplied buffer bounds. Intended for future decoder hardening paths that need
 * to reject truncated or malformed RTCM payloads safely.
 */
[[maybe_unused]] bool
getbituIncChecked(const unsigned char* buff, int buffBits, int& pos, int len, uint32_t& out);

/** Bounds-checked variant of getbitsInc().
 * Returns false instead of reading when the requested bit range is outside the
 * supplied buffer bounds. The optional failure pointer is forwarded to the
 * signed extractor when the read is valid.
 */
[[maybe_unused]] bool getbitsIncChecked(
    const unsigned char* buff,
    int                  buffBits,
    int&                 pos,
    int                  len,
    int32_t&             out,
    bool*                failure_ptr = nullptr
);

int32_t getbitg(const unsigned char* buff, int pos, int len);
int32_t getbitgInc(const unsigned char* buff, int& pos, int len);

int32_t getbitgInc(vector<unsigned char>& buff, int& pos, int len);

uint32_t getbitu(vector<unsigned char>& buff, int pos, int len);

uint32_t getbituInc(vector<unsigned char>& buff, int& pos, int len);

int32_t getbitsInc(vector<unsigned char>& buff, int& pos, int len, bool* failure_ptr = nullptr);

/** Vector overload of the bounds-checked unsigned incremental bit reader.
 * Uses the vector size to derive the valid bit range before delegating to the
 * raw-buffer implementation.
 */
[[maybe_unused]] bool
getbituIncChecked(vector<unsigned char>& buff, int& pos, int len, uint32_t& out);

/** Vector overload of the bounds-checked signed incremental bit reader.
 * Uses the vector size to derive the valid bit range before delegating to the
 * raw-buffer implementation.
 */
[[maybe_unused]] bool getbitsIncChecked(
    vector<unsigned char>& buff,
    int&                   pos,
    int                    len,
    int32_t&               out,
    bool*                  failure_ptr = nullptr
);

double getbituIncScale(vector<unsigned char>& buff, int& pos, int len, double scale);

double getbitsIncScale(
    vector<unsigned char>& buff,
    int&                   pos,
    int                    len,
    double                 scale,
    bool*                  failure_ptr = nullptr
);
