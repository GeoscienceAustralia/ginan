
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
GTime  lastMessType0;
bool   sbasAlertNoSoL = false;

map<int, map<SatSys, SBASCova>> sbasUdreCov;
map<SatSys, usedSBASIODs>       usedSBASIODMap;

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
        for (auto it = sbs.slowUpdt.begin(); it != sbs.slowUpdt.end();)
        {
            auto slowUpdt = it->second;
            for (auto it2 = slowUpdt.begin(); it2 != slowUpdt.end();)
            {
                auto teph = it2->first;
                if ((time - teph).to_double() > MAX_SBAS_CORR_AGE)
                    it2 = slowUpdt.erase(it2);
                else
                    it2++;
            }

            if (slowUpdt.empty())
                it = sbs.slowUpdt.erase(it);
            else
                it++;
        }

        for (auto it = sbs.fastUpdt.begin(); it != sbs.fastUpdt.end();)
        {
            auto fastUpdt = it->second;
            for (auto it2 = fastUpdt.begin(); it2 != fastUpdt.end();)
            {
                auto teph = it2->first;
                if ((time - teph).to_double() > MAX_SBAS_CORR_AGE)
                    it2 = fastUpdt.erase(it2);
                else
                    it2++;
            }

            if (fastUpdt.empty())
                it = sbs.fastUpdt.erase(it);
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
    horPL = 8000;
    verPL = 100;

    if (sbasAlertNoSoL)
        return;

    VectorPos pos = ecef2pos(staPos);
    Matrix3d  E;
    pos2enu(pos, E.data());
    Matrix3d EP   = E * ecefP;
    Matrix3d enuP = EP * E.transpose();

    double scaleH = acsConfig.sbsInOpts.prec_aproach ? 6.00 : 6.18;
    double scaleV = 5.33;
    double aveEN  = (enuP(0, 0) + enuP(1, 1)) / 2;
    double difEN  = (enuP(0, 0) - enuP(1, 1)) / 2;
    double covEN  = enuP(0, 1);
    horPL         = scaleH * sqrt(aveEN + sqrt(difEN * difEN + covEN * covEN));
    verPL         = scaleV * sqrt(enuP(2, 2));
}

void writeSPP(string filename, Receiver& rec)
{
    auto& sppPos = rec.sol.sppPos;
    if (sppPos.norm() < 1000)
        return;

    std::ofstream output(filename, std::fstream::out | std::fstream::app);
    if (!output.is_open())
    {
        BOOST_LOG_TRIVIAL(warning) << "Warning: Error opening POS file '" << filename;
        return;
    }

    output.seekp(0, output.end);  // seek to end of file

    auto&     apriori = rec.aprioriPos;
    auto&     sppTime = rec.sol.sppTime;
    VectorPos pos     = ecef2pos(sppPos);

    string tstr  = sppTime.to_ISOstring(3);
    double tyear = sppTime.to_decYear();

    if (apriori.norm() > 1000)
    {
        if (output.tellp() == 0)
            tracepdeex(
                0,
                output,
                "\n*YYYY-MM-DDTHH:MM:SS.SSS YYYY.YYYYYYYYY        X             Y              Z   "
                "        HDOP       VDOP       GDOP      - - -       N ref.          E ref.       "
                "H ref.         dN          dE          dU          dH         HPL        VPL     "
                "- - -"
            );

        VectorEnu dpos = ecef2enu(pos, sppPos - apriori);
        tracepdeex(
            0,
            output,
            "\n %s %14.9f %14.5f %14.5f %14.5f %11.4f %11.4f %11.4f - - - %15.10f %15.10f %11.5f "
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
        return;
    }
    if (output.tellp() == 0)
        tracepdeex(
            0,
            output,
            "\n*YYYY-MM-DDTHH:MM:SS.SSS YYYY.YYYYYYYYY        X             Y              Z       "
            "    HDOP       VDOP       GDOP      - - -       N recv          E recv       H recv   "
            "- - - -     HPL        VPL      - - -"
        );

    tracepdeex(
        0,
        output,
        "\n %s %14.9f %14.5f %14.5f %14.5f %11.4f %11.4f %11.4f - - - %15.10f %15.10f %11.5f "
        "- - - - %11.5f %11.5f - - -",
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
        rec.sol.horzPL,
        rec.sol.vertPL
    );
}

double rangeErrFromCov(
    Trace&    trace,
    GTime     time,
    int       iodp,
    SatSys    sat,
    Vector3d& rRec,
    Vector3d& rSat,
    double    eCov
)
{
    if (sbasUdreCov.find(iodp) == sbasUdreCov.end())
        return -1;

    if (sbasUdreCov[iodp].find(sat) == sbasUdreCov[iodp].end())
        return -1;

    if (sbasUdreCov[iodp][sat].REScale < 0)
        return -1;

    if ((time - sbasUdreCov[iodp][sat].toe).to_double() > sbasUdreCov[iodp][sat].Ivalid)
        return -1;

    if (rSat.norm() <= RE_WGS84 * 0.9)
        return -1;
    Vector3d ePos = rSat - rRec;
    ePos.normalize();

    VectorXd ePosClk = VectorXd::Zero(4);
    for (int i = 0; i < 3; i++)
        ePosClk[i] = ePos[i];
    ePosClk[3]  = 1;
    VectorXd Ce = sbasUdreCov[iodp][sat].covr * ePosClk;
    double   x  = ePosClk.dot(Ce);

    return sqrt(x) + eCov * sbasUdreCov[iodp][sat].REScale;
}

double checkSBASVar(
    Trace&    trace,
    GTime     time,
    SatSys    sat,
    Vector3d& rRec,
    Vector3d& rSat,
    SBASMaps& sbsMaps
)
{
    if (usedSBASIODMap.find(sat) == usedSBASIODMap.end())
        return -1;

    if (fabs((time - usedSBASIODMap[sat].tUsed).to_double()) >= 0.5)
        return -1;

    int iodp = usedSBASIODMap[sat].iodp;
    int iodf = usedSBASIODMap[sat].iodf;
    int iode = usedSBASIODMap[sat].iode;

    if (sbsMaps.fastUpdt.find(iodp) == sbsMaps.fastUpdt.end())
        return -2;

    if (sbsMaps.fastCorr.find(iodf) == sbsMaps.fastCorr.end())
        return -2;

    auto& sbsFast = sbsMaps.fastCorr[iodf];
    if (sbsFast.iodp != iodp)
        return -2;

    switch (acsConfig.sbsInOpts.freq)
    {
        case 1:
            if (sbsMaps.slowCorr.find(iode) == sbsMaps.slowCorr.end())
                return -2;
            return estimateSBASVar(trace, time, sat, rRec, rSat, sbsFast, sbsMaps.slowCorr[iode]);
        case 5:
            return estimateDFMCVar(trace, time, sat, rRec, rSat, sbsFast);
    }

    return -2;
}

void checkForType0(GTime time, int type)
{
    if (type < 0)
    {
        lastMessType0  = time;
        sbasAlertNoSoL = true;
        for (auto& [sat, satDat] : nav.satNavMap)
        {
            auto& sbs = satDat.currentSBAS;
            sbs.fastUpdt.clear();
            sbs.slowUpdt.clear();
            sbs.fastCorr.clear();
            sbs.slowCorr.clear();
        }
    }
    else if ((time - lastMessType0).to_double() > 10)
        sbasAlertNoSoL = false;
}