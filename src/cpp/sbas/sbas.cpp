
#include "sbas/sbas.hpp"
#include "common/acsConfig.hpp"
#include "common/common.hpp"
#include "common/navigation.hpp"
#include "common/receiver.hpp"
#include "orbprop/coordinates.hpp"
#include "pea/inputsOutputs.hpp"

#define MAX_SBAS_CORR_AGE 300

using std::lock_guard;

GTime  lastEMSLoaded;
GTime  lastEMSWritten;
string lastEMSFile;

struct SBASSmoothControl
{
    GTime  lastUpdate;
    int    numMea = 0;
    double ambEst = 0;
    double ambVar = -1;
};

map<SatSys, map<string, SBASSmoothControl>> smoothedMeasMap;

mutex                   sbasMessagesMutex;
map<GTime, SBASMessage> sbasMessages;

GTime adjustDay(double tod1, GTime nearTime)
{
    GTow   tow0  = nearTime;
    double tod0  = fmod(tow0, secondsInDay);
    double delta = tod1 - tod0;
    while (delta > +secondsInDay / 2)
        delta -= secondsInDay;
    while (delta < -secondsInDay / 2)
        delta += secondsInDay;
    GTime teph = nearTime + delta;
    return teph;
}

void writeEMSline(
    GTime        time,         ///< Time of bias to write
    SBASMessage& sbasMessage,  ///< SBAS message data
    Trace&       trace         ///< Stream to output to
)
{
    GEpoch epoch = time;

    tracepdeex(
        0,
        trace,
        " %3d %02d %02d %02d %02d %02d %02d %2d %s\r\n",
        sbasMessage.prn,
        ((int)epoch.year) % 100,
        (int)epoch.month,
        (int)epoch.day,
        (int)epoch.hour,
        (int)epoch.min,
        (int)epoch.sec,
        sbasMessage.type,
        sbasMessage.message
    );
}

/** Write received SBAS messages into EMS files
 */
void writeEMSdata(
    Trace& trace,    ///< Trace to output to
    string filename  ///< File to write
)
{
    string checkFile = acsConfig.ems_filename;

    lock_guard<mutex> guard(sbasMessagesMutex);
    for (auto [frameTime, sbasData] : sbasMessages)
    {
        if (frameTime > lastEMSWritten)
        {
            // todo aaron, use the standard file rotations
            PTime                    pTime = frameTime;
            boost::posix_time::ptime otherPTime =
                boost::posix_time::from_time_t((time_t)pTime.bigTime);

            replaceTimes(checkFile, otherPTime);

            if (checkFile != lastEMSFile)
            {
                lastEMSFile = checkFile;
                tracepdeex(3, trace, "\nStarting new EMS file: %s\n", lastEMSFile.c_str());
            }

            std::ofstream outputStream(lastEMSFile, std::fstream::app);
            if (!outputStream)
            {
                BOOST_LOG_TRIVIAL(error) << "Cannot open EMS file:" << lastEMSFile;
                trace << "ERROR: cannot open EMS file:" << lastEMSFile;

                break;
            }

            tracepdeex(4, trace, "\nWriting EMS file line: %s\n", frameTime.to_string().c_str());
            writeEMSline(frameTime, sbasData, outputStream);

            lastEMSWritten = frameTime;
        }
    }
}

bool checkType(int frq, int type)
{
    if (type == 0)
        return true;
    if (type > 61)
        return true;
    if (frq == 1 && type < 31)
        return true;
    if (frq == 5 && type > 30)
        return true;
    return false;
}

void readEMSdata(  ///< Trace to output to
    string filename
)                  ///< File to write
{
    std::ifstream fileStream(filename);
    if (!fileStream)
    {
        BOOST_LOG_TRIVIAL(info) << "ems file open error " << filename;
        return;
    }

    int prn_ = acsConfig.sbsInOpts.prn;
    int freq = acsConfig.sbsInOpts.freq;

    string line;
    while (fileStream)
    {
        getline(fileStream, line);
        if (line.size() < 88)
            continue;

        int prn = std::stoi(line.substr(0, 3), nullptr, 10);
        int typ = std::stoi(line.substr(22, 2), nullptr, 10);

        if (prn != prn_)
            continue;
        if (!checkType(freq, typ))
            continue;

        double ep[6];
        ep[0] = (double)std::stoi(line.substr(4, 2), nullptr, 10);
        ep[1] = (double)std::stoi(line.substr(7, 2), nullptr, 10);
        ep[2] = (double)std::stoi(line.substr(10, 2), nullptr, 10);
        ep[3] = (double)std::stoi(line.substr(13, 2), nullptr, 10);
        ep[4] = (double)std::stoi(line.substr(16, 2), nullptr, 10);
        ep[5] = (double)std::stoi(line.substr(19, 2), nullptr, 10);
        ep[0] += 100 * round((acsConfig.sbsInOpts.ems_year - ep[0]) / 100);
        GTime messTime = epoch2time(ep);

        SBASMessage sbas;
        sbas.prn     = prn_;
        sbas.freq    = freq;
        sbas.type    = typ;
        sbas.message = line.substr(25, 64);

        for (int i = 0; i < 32; i++)
        {
            unsigned char byte =
                (unsigned char)std::stoi(sbas.message.substr(2 * i, 2), nullptr, 16);
            sbas.data[i] = byte;
        }

        {
            lock_guard<mutex> guard(sbasMessagesMutex);
            sbasMessages[messTime] = sbas;
        }
    }
}

void loadSBASdata(Trace& trace, GTime time, Navigation& nav)
{
    tracepdeex(
        5,
        trace,
        "\nSBASMESS Loading SBAS messages up to %s, %d",
        time.to_string().c_str(),
        sbasMessages.size()
    );

    for (auto& [sat, satDat] : nav.satNavMap)
    {
        auto& sbs = satDat.currentSBAS;
        for (auto it = sbs.corrUpdt.begin(); it != sbs.corrUpdt.end();)
        {
            auto teph = it->first;
            if ((time - teph).to_double() > MAX_SBAS_CORR_AGE)
                it = sbs.corrUpdt.erase(it);
            else
                it++;
        }
        for (auto it = sbs.slowCorr.begin(); it != sbs.slowCorr.end();)
        {
            auto teph = it->second.trec;
            if ((time - teph).to_double() > MAX_SBAS_CORR_AGE)
                it = sbs.slowCorr.erase(it);
            else
                it++;
        }
    }

    lock_guard<mutex> guard(sbasMessagesMutex);
    for (auto it = sbasMessages.begin(); it != sbasMessages.end();)
    {
        auto [frameTime, mess] = *it;
        if ((time - frameTime).to_double() > MAX_SBAS_CORR_AGE)
            it = sbasMessages.erase(it);
        else
            break;
    }

    for (auto& [frameTime, mess] : sbasMessages)
    {
        if ((lastEMSLoaded - frameTime).to_double() >= 0)
            continue;
        if ((time - frameTime).to_double() < 0)
            break;

        if (acsConfig.sbsInOpts.freq == 1)
            decodeSBASMessage(trace, frameTime, mess, nav);
        if (acsConfig.sbsInOpts.freq == 5)
            decodeDFMCMessage(trace, frameTime, mess, nav);
        lastEMSLoaded = frameTime;
    }
}

void estimateSBASProtLvl(Vector3d& staPos, Matrix3d& ecefP, double& horPL, double& verPL)
{
    horPL = -1;
    verPL = -1;

    switch (acsConfig.sbsInOpts.freq)
    {
        case 5:
            estimateDFMCPL(staPos, ecefP, horPL, verPL);
            return;
        default:
            return;
    }
}

double sbasSmoothedPsudo(
    Trace&  trace,
    GTime   time,
    SatSys  Sat,
    string  staId,
    double  measP,
    double  measL,
    double  variP,
    double  variL,
    double& varSmooth,
    bool    update
)
{
    varSmooth = -1;
    if (variP < 0 || variL < 0)
        return measP;

    auto&  smCtrl = smoothedMeasMap[Sat][staId];
    double fact   = 1.0 / smCtrl.numMea;
    if (update)
    {
        bool slip = false;
        if ((time - smCtrl.lastUpdate).to_double() > acsConfig.sbsInOpts.smth_out)
            slip = true;
        if (smCtrl.ambVar < 0)
            slip = true;

        double ambMea = measP - measL;
        if (fabs(ambMea - smCtrl.ambEst) > 4 * sqrt(smCtrl.ambVar + variP + variL))
            slip = true;  // Try replacing this with a outputs from preprocessor

        smCtrl.lastUpdate = time;
        if (slip)
        {
            smCtrl.numMea = 0;
            smCtrl.ambEst = 0;
            smCtrl.ambVar = 0;
        }

        smCtrl.numMea++;
        if (smCtrl.numMea > acsConfig.sbsInOpts.smth_win)
            smCtrl.numMea = acsConfig.sbsInOpts.smth_win;
        fact = 1.0 / smCtrl.numMea;

        smCtrl.ambEst += fact * (ambMea - smCtrl.ambEst);
        smCtrl.ambVar = SQR(fact) * (variP + variL) + SQR(1 - fact) * smCtrl.ambVar;
    }
    varSmooth = (1 - 2 * fact) * variL + smCtrl.ambVar;
    return smCtrl.ambEst + measL;
}

void writeSPP(string filename, Receiver& rec)
{
    std::ofstream output(filename, std::fstream::out | std::fstream::app);
    if (!output.is_open())
    {
        BOOST_LOG_TRIVIAL(warning) << "Warning: Error opening POS file '" << filename;
        return;
    }

    output.seekp(0, output.end);  // seek to end of file

    if (output.tellp() == 0)
        tracepdeex(
            2,
            output,
            "\n*YYYY-MM-DDTHH:MM:SS.SSS YYYY.YYYYYYYYY        X             Y              Z       "
            "HDOP   VDOP   GDOP   - - -       NLat            Elong        Height         dN       "
            "   dE          dU          dH         HPL        VPL     - - - -"
        );

    auto&     apriori = rec.aprioriPos;
    auto&     sppPos  = rec.sol.sppPos;
    auto&     sppTime = rec.sol.sppTime;
    VectorPos pos     = ecef2pos(apriori);
    VectorEnu dpos    = ecef2enu(pos, sppPos - apriori);

    string tstr  = sppTime.to_ISOstring(3);
    double tyear = sppTime.to_decYear();
    tracepdeex(
        2,
        output,
        "\n %s %14.9f %11.5f %11.5f %11.5f %11.4f %11.4f %11.4f - - - %15.10f %15.10f %11.5f "
        "%11.5f %11.5f %11.5f %11.5f %11.5f %11.5f - - -",
        tstr,
        tyear,
        sppPos[0],
        sppPos[1],
        sppPos[2],
        rec.sol.dops.hdop,
        rec.sol.dops.vdop,
        rec.sol.dops.gdop,
        pos[0] * R2D,
        pos[1] * R2D,
        pos[2],
        dpos[0],
        dpos[1],
        dpos[2],
        sqrt(dpos[0] * dpos[0] + dpos[1] * dpos[1]),
        rec.sol.horzPL,
        rec.sol.vertPL
    );
}
