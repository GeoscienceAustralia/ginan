// #pragma GCC optimize ("O0")

#include "common/sbfDecoder.hpp"

#include "architectureDocs.hpp"
#include "common/constants.hpp"
#include "common/enums.h"
#include "common/gTime.hpp"
#include "common/icdDecoder.hpp"
#include "common/navigation.hpp"
#include "common/observations.hpp"
#include "common/streamSbf.hpp"
#include "sbas/sbas.hpp"

using std::lock_guard;
using std::mutex;

GTime lastObstime;

SatSys sbsID2Sat(unsigned int id)
{
    SatSys sat;
    if (id > 0 && id < 38)
    {
        sat.sys = E_Sys::GPS;
        sat.prn = id;
        return sat;
    }
    if (id > 37 && id < 62)
    {
        sat.sys = E_Sys::GLO;
        sat.prn = id - 37;
        return sat;
    }
    if (id == 62)
    {
        sat.sys = E_Sys::GLO;
        sat.prn = 0;
        return sat;
    }
    if (id > 62 && id < 69)
    {
        sat.sys = E_Sys::GLO;
        sat.prn = id - 38;
        return sat;
    }
    if (id > 70 && id < 107)
    {
        sat.sys = E_Sys::GAL;
        sat.prn = id - 70;
        return sat;
    }
    if (id > 119 && id < 141)
    {
        sat.sys = E_Sys::SBS;
        sat.prn = id - 100;
        return sat;
    }
    if (id > 140 && id < 181)
    {
        sat.sys = E_Sys::BDS;
        sat.prn = id - 140;
        return sat;
    }
    if (id > 180 && id < 191)
    {
        sat.sys = E_Sys::QZS;
        sat.prn = id - 180;
        return sat;
    }
    if (id > 190 && id < 198)
    {
        sat.sys = E_Sys::IRN;
        sat.prn = id - 190;
        return sat;
    }
    if (id > 197 && id < 216)
    {
        sat.sys = E_Sys::SBS;
        sat.prn = id - 157;
        return sat;
    }
    if (id > 215 && id < 223)
    {
        sat.sys = E_Sys::IRN;
        sat.prn = id - 208;
        return sat;
    }
    if (id > 222 && id < 246)
    {
        sat.sys = E_Sys::BDS;
        sat.prn = id - 182;
        return sat;
    }
    return sat;
}

E_ObsCode sbdObsCode[40] = {
    E_ObsCode::L1C,   // GPS
    E_ObsCode::L1W,   // GPS
    E_ObsCode::L2W,   // GPS
    E_ObsCode::L2L,   // GPS
    E_ObsCode::L5Q,   // GPS
    E_ObsCode::L1L,   // GPS
    E_ObsCode::L1C,   // QZS
    E_ObsCode::L2L,   // QZS
    E_ObsCode::L1C,   // GLO
    E_ObsCode::L1P,   // GLO
    E_ObsCode::L2P,   // GLO
    E_ObsCode::L2C,   // GLO
    E_ObsCode::L3Q,   // GLO
    E_ObsCode::L1P,   // BDS
    E_ObsCode::L5P,   // BDS
    E_ObsCode::L5A,   // IRN
    E_ObsCode::NONE,  // -
    E_ObsCode::L1C,   // GAL
    E_ObsCode::NONE,  // -
    E_ObsCode::L6C,   // GAL
    E_ObsCode::L5Q,   // GAL
    E_ObsCode::L7Q,   // GAL
    E_ObsCode::L8Q,   // GAL
    E_ObsCode::NONE,  // -
    E_ObsCode::L1C,   // SBS
    E_ObsCode::L5I,   // SBS
    E_ObsCode::L5Q,   // QZS
    E_ObsCode::L6C,   // QZS
    E_ObsCode::L5D,   // BDS
    E_ObsCode::L7I,   // BDS
    E_ObsCode::L6I,   // BDS
    E_ObsCode::L1L,   // QZS
    E_ObsCode::L1Z,   // QZS
    E_ObsCode::L7D,   // BDS
    E_ObsCode::NONE,  // -
    E_ObsCode::NONE,  // -
    E_ObsCode::L1P,   // IRN
    E_ObsCode::L1E,   // QZS
    E_ObsCode::L5P    // QZS
};

unsigned int bin2unsigned(vector<unsigned char>& data, int init, int size)
{
    uint64_t value = 0;
    for (int i = 0; i < size; i++)
        value += data[init + i] << (8 * i);
    return value;
}

int bin2signed(vector<unsigned char>& data, int init, int size)
{
    unsigned int value  = bin2unsigned(data, init, size);
    unsigned int thres  = 1 << (8 * size - 1);
    int          result = value;
    if (value > thres)
        result -= 2 * thres;
    return result;
}

int bin2float(vector<unsigned char>& data, int init)
{
    float value;
    std::memcpy(&value, &data[init], 4);
    return value;
}

int bin2double(vector<unsigned char>& data, int init)
{
    double value;
    std::memcpy(&value, &data[init], 8);
    return value;
}

void SbfDecoder::decodeMeasEpoch(GTime time, vector<unsigned char>& data)
{
    int    numMeas   = data[6];
    int    mess1size = data[7];
    int    mess2size = data[8];
    int    flags     = data[9];
    double clock     = data[10] * 0.001;

    map<SatSys, GObs> obsMap;
    int               ind = 12;
    for (int i = 0; i < numMeas; i++)
    {
        int ind0 = ind;
        ind++;

        int          codeID   = data[ind++] & 0x1F;
        int          satID    = data[ind++];
        unsigned int pseudMSB = data[ind++] & 0x07;
        unsigned int pseudLSB = bin2unsigned(data, ind, 4);
        ind += 4;
        int doppInt = bin2signed(data, ind, 4);
        ind += 4;
        unsigned int carriLSB = bin2unsigned(data, ind, 2);
        ind += 2;
        int carriMSB = bin2signed(data, ind, 1);
        ind++;
        unsigned int CN0Int   = data[ind++];
        unsigned int lockTime = bin2unsigned(data, ind, 2);
        ind += 2;
        int          codeID2  = data[ind++] >> 3;
        unsigned int numMeas2 = data[ind++];

        SatSys sat = sbsID2Sat(satID);
        if (codeID == 31)
            codeID = codeID2 + 32;
        E_ObsCode code    = sbdObsCode[codeID];
        auto      ft      = code2Freq[sat.sys][code];
        auto      waveLen = genericWavelength[ft];
        double    Pr1     = 4294967.296 * pseudMSB + 0.001 * pseudLSB;
        double Ph1 = Pr1 / waveLen + 65.536 * carriMSB + (carriMSB > 0 ? 0.001 : -0.001) * carriLSB;
        double Dp1 = doppInt * 0.0001;

        auto& obs = obsMap[sat];
        obs.Sat   = sat;
        obs.time  = time;
        obs.mount = recId;

        Sig sig;
        sig.code = code;
        sig.P    = Pr1;
        sig.L    = Ph1;
        sig.D    = Dp1;
        obs.sigsLists[ft].push_back(sig);

        ind = ind0 + mess1size;
        for (int j = 0; j < numMeas2; j++)
        {
            int ind1              = ind;
            codeID                = data[ind++] & 0x1F;
            lockTime              = data[ind++];
            CN0Int                = data[ind++];
            unsigned int offstMSB = data[ind++];
            carriMSB              = bin2signed(data, ind, 1);
            ind++;
            codeID2               = data[ind++] >> 3;
            unsigned int pseudLSB = bin2unsigned(data, ind, 2);
            ind += 2;
            carriLSB = bin2unsigned(data, ind, 2);
            ind += 2;
            unsigned int dopplLSB = bin2unsigned(data, ind, 2);
            ind += 2;

            if (codeID == 31)
                codeID = codeID2 + 32;
            E_ObsCode code2    = sbdObsCode[codeID];
            auto      ft2      = code2Freq[sat.sys][code2];
            auto      waveLen2 = genericWavelength[ft2];
            int       pseudMSB = offstMSB & 0x07;
            if (pseudMSB > 4)
                pseudMSB -= 7;
            int dopplMSB = (offstMSB >> 3) & 0x07;
            if (dopplMSB > 4)
                dopplMSB -= 7;

            Sig sig2;
            sig2.code = code2;
            sig2.P    = Pr1 + 65.536 * pseudMSB + 0.001 * pseudLSB;
            sig2.L    = sig2.P / waveLen2 + 65.536 * carriMSB + 0.001 * carriLSB;
            sig2.D    = Dp1 * waveLen2 / waveLen + 6.5536 * dopplMSB + 0.0001 * dopplLSB;
            obs.sigsLists[ft2].push_back(sig2);

            ind = ind1 + mess2size;
        }
    }

    for (auto& [Sat, obs] : obsMap)
    {
        if (Sat.sys == E_Sys::GLO)
            continue;  // GLONASS not supported for now
        sbfObsList.push_back((shared_ptr<GObs>)obs);
    }
    obsListList.push_back(sbfObsList);
    sbfObsList.clear();
    return;
}

void SbfDecoder::decodeEndOfMeas(GTime time)
{
    obsListList.push_back(sbfObsList);
    sbfObsList.clear();
    lastObstime = time;
}

std::string val2Hex(unsigned char val)
{
    std::stringstream ss;
    ss << std::hex << std::uppercase << std::setw(2) << std::setfill('0') << val;
    return ss.str();
}

void decodeGEORawL1(GTime time, vector<unsigned char>& data)
{
    SBASMessage sbs;
    sbs.prn = data[6];
    if (sbs.prn > 197)
        sbs.prn -= 57;
    if (data[7] == 0)  // SBAS CRC failed
        return;
    // to do: reject frame is data[8] (Viterbi error count) exceds a threshold
    unsigned char navSource = data[9] & 0x1F;
    if (navSource != 24)
    {
        return;
    }
    sbs.freq = 1;
    sbs.type = data[13] >> 2;

    // data[10] (FreqNr) is not relevant for this message
    // data[11] (RxChannel) is not supported by Ginan
    for (int i = 0; i < 32; i++)
    {
        sbs.data[i] = data[i + 12];
        sbs.message += val2Hex(sbs.data[i]);
    }

    {
        lock_guard<mutex> guard(sbasMessagesMutex);

        for (auto it = sbasMessages.begin(); it != sbasMessages.end();)
        {
            auto& [tof, sbasData] = *it;
            if ((time - tof).to_double() > MAX_SBAS_MESS_AGE)
            {
                it = sbasMessages.erase(it);
            }
            else
            {
                it++;
            }
        }

        if (sbs.prn == acsConfig.sbsInOpts.prn && sbs.freq == acsConfig.sbsInOpts.freq)
            sbasMessages[time] = sbs;
    }
}

void decodeGEORawL5(GTime time, vector<unsigned char>& data)
{
    SBASMessage sbs;
    sbs.prn = data[6];
    if (sbs.prn > 197)
        sbs.prn -= 57;
    if (data[7] == 0)  // SBAS CRC failed
        return;
    // to do: reject frame if data[8] (Viterbi error count) exceds a threshold
    unsigned char navSource = data[9] & 0x1F;
    if (navSource != 25)
    {
        return;
    }
    sbs.freq = 5;
    sbs.type = (data[12] & 0x0F) * 4 + (data[13] >> 6);

    // data[10] (FreqNr) is not relevant for this message
    // data[11] (RxChannel) is not supported by Ginan
    for (int i = 0; i < 32; i++)
    {
        sbs.data[i] = data[i + 12];
        sbs.message += val2Hex(sbs.data[i]);
    }

    {
        lock_guard<mutex> guard(sbasMessagesMutex);

        for (auto it = sbasMessages.begin(); it != sbasMessages.end();)
        {
            auto& [tof, sbasData] = *it;
            if ((time - tof).to_double() > MAX_SBAS_MESS_AGE)
            {
                it = sbasMessages.erase(it);
            }
            else
            {
                it++;
            }
        }

        if (sbs.prn == acsConfig.sbsInOpts.prn && sbs.freq == acsConfig.sbsInOpts.freq)
            sbasMessages[time] = sbs;
    }
}

void decodeGPSNav(GTime time, vector<unsigned char>& data)
{
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::GPS)
        return;
    Eph eph;
    eph.type = E_NavMsgType::LNAV;
    eph.Sat  = sat;
    // data[7] : Reserved
    int ind  = 8;
    eph.week = bin2unsigned(data, ind, 2);
    ind += 2;
    eph.code = data[ind++];
    eph.sva  = data[ind++];
    int svh  = data[ind++];
    eph.flag = data[ind++];
    eph.iodc = bin2unsigned(data, ind, 2);
    ind += 2;
    eph.iode = data[ind++];
    ind++;  // data[17] : IODE in subframe 3
    eph.fit = data[ind++] ? 0 : 4;
    ind++;  // data[19] : Reserved
    eph.tgd[0] = bin2float(data, ind);
    ind += 4;
    double toc = bin2unsigned(data, ind, 4);
    ind += 4;
    eph.f2 = bin2float(data, ind);
    ind += 4;
    eph.f1 = bin2float(data, ind);
    ind += 4;
    eph.f0 = bin2float(data, ind);
    ind += 4;
    eph.crs = bin2float(data, ind);
    ind += 4;
    eph.deln = bin2float(data, ind) * SC2RAD;
    ind += 4;
    eph.M0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.cuc = bin2float(data, ind);
    ind += 4;
    eph.e = bin2double(data, ind);
    ind += 8;
    eph.cus = bin2float(data, ind);
    ind += 4;
    eph.sqrtA = bin2double(data, ind);
    ind += 8;
    double toe = bin2unsigned(data, ind, 4);
    ind += 4;
    eph.cic = bin2float(data, ind);
    ind += 4;
    eph.OMG0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.cis = bin2float(data, ind);
    ind += 4;
    eph.i0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.crc = bin2float(data, ind);
    ind += 4;
    eph.omg = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.OMGd = bin2float(data, ind) * SC2RAD;
    ind += 4;
    eph.idot = bin2float(data, ind);
    ind += 4;
    int wn_toc = bin2unsigned(data, ind, 2);
    ind += 2;
    int wn_toe = bin2unsigned(data, ind, 2);
    ind += 2;

    eph.ura[0] = svaToUra(eph.sva);
    eph.svh    = (E_Svh)svh;
    wn_toc += 1024 * floor((eph.week - wn_toc + 512) / 1024);
    wn_toe += 1024 * floor((eph.week - wn_toe + 512) / 1024);
    eph.toc                                = gpst2time(wn_toc, toc);
    eph.toe                                = gpst2time(wn_toe, toe);
    nav.ephMap[eph.Sat][eph.type][eph.toe] = eph;
}

void decodeGPSIon(GTime time, vector<unsigned char>& data)
{
    ION    ion;
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::GPS)
        return;
    ion.Sat  = sat;
    ion.type = E_NavMsgType::LNAV;
    ion.ttm  = time;
    int ind  = 8;
    ion.a0   = bin2float(data, ind);
    ind += 4;
    ion.a1 = bin2float(data, ind);
    ind += 4;
    ion.a2 = bin2float(data, ind);
    ind += 4;
    ion.a3 = bin2float(data, ind);
    ind += 4;
    ion.b0 = bin2float(data, ind);
    ind += 4;
    ion.b1 = bin2float(data, ind);
    ind += 4;
    ion.b2 = bin2float(data, ind);
    ind += 4;
    ion.b3 = bin2float(data, ind);
    ind += 4;

    nav.ionMap[ion.Sat.sys][ion.type][ion.ttm] = ion;
}

void decodeGPSUtc(GTime time, vector<unsigned char>& data)
{
    STO    sto;
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::GPS)
        return;
    sto.Sat  = sat;
    sto.type = E_NavMsgType::LNAV;
    sto.code = E_StoCode::GPUT;
    sto.ttm  = time;
    int ind  = 8;
    sto.A1   = bin2float(data, ind);
    ind += 4;
    sto.A0 = bin2double(data, ind);
    ind += 8;
    double tot = bin2unsigned(data, ind, 4);
    ind += 4;
    double tot_ = tot + 86400;  // GPS vs UTC week
    if (tot_ > 604800)
        tot_ -= 604800;         // GPS v UTC week
    sto.tot                                 = GTime((GTow)tot_, time);
    nav.stoMap[sto.code][sto.type][sto.tot] = sto;

    // to do: input leap seconds
}

void decodeGALNav(GTime time, vector<unsigned char>& data)
{
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::GAL)
        return;
    Eph eph;
    eph.Sat       = sat;
    int navSource = data[7];
    switch (navSource)
    {
        case 2:
            eph.type = E_NavMsgType::INAV;
            break;
        case 16:
            eph.type = E_NavMsgType::FNAV;
            break;
        default:
            return;
    }

    int ind   = 8;
    eph.sqrtA = bin2double(data, ind);
    ind += 8;
    eph.M0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.e = bin2double(data, ind);
    ind += 8;
    eph.i0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.omg = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.OMG0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.OMGd = bin2float(data, ind) * SC2RAD;
    ind += 4;
    eph.idot = bin2float(data, ind) * SC2RAD;
    ind += 4;
    eph.deln = bin2float(data, ind) * SC2RAD;
    ind += 4;
    eph.cuc = bin2float(data, ind);
    ind += 4;
    eph.cus = bin2float(data, ind);
    ind += 4;
    eph.crc = bin2float(data, ind);
    ind += 4;
    eph.crs = bin2float(data, ind);
    ind += 4;
    eph.cic = bin2float(data, ind);
    ind += 4;
    eph.cis = bin2float(data, ind);
    ind += 4;
    double toe = bin2unsigned(data, ind, 4);
    ind += 4;
    double toc = bin2unsigned(data, ind, 4);
    ind += 4;
    eph.f2 = bin2float(data, ind);
    ind += 4;
    eph.f1 = bin2float(data, ind);
    ind += 4;
    eph.f0 = bin2float(data, ind);
    ind += 4;
    int wn_toc = bin2unsigned(data, ind, 2);
    ind += 2;
    int wn_toe = bin2unsigned(data, ind, 2);
    ind += 2;
    eph.iodc = bin2unsigned(data, ind, 2);
    ind += 2;
    int svh = bin2unsigned(data, ind, 2);
    ind += 2;
    ind++;
    eph.ura[0] = svaToSisa(data[ind++]);  // SISA for L1L5
    eph.ura[1] = svaToSisa(data[ind++]);  // SISA for L1L7
    eph.ura[2] = svaToSisa(data[ind++]);  // SISA for L1L6
    eph.tgd[0] = bin2float(data, ind);
    ind += 4;                             // BGD for L1L5
    eph.tgd[1] = bin2float(data, ind);
    ind += 4;                             // BGD for L1L7
    eph.tgd[2] = bin2float(data, ind);
    ind += 4;                             // BGD for L1L6

    eph.iode = eph.iodc;

    wn_toc += 1024 * floor((eph.week - wn_toc + 512) / 1024);
    wn_toe += 1024 * floor((eph.week - wn_toe + 512) / 1024);
    eph.toc = gpst2time(wn_toc, toc);
    eph.toe = gpst2time(wn_toe, toe);

    int l1svh = svh & 0x0F;
    int l5svh = svh >> 4 & 0x0F;
    int l7svh = svh >> 8 & 0x0F;
    if (l1svh & 1)
    {
        eph.e1_dvs = l1svh >> 1 & 1;
        eph.e1_hs  = l1svh >> 2 & 3;
    }
    if (l5svh & 1)
    {
        eph.e5a_dvs = l5svh >> 1 & 1;
        eph.e5a_hs  = l5svh >> 2 & 3;
    }
    if (l7svh & 1)
    {
        eph.e5b_dvs = l7svh >> 1 & 1;
        eph.e5b_hs  = l7svh >> 2 & 3;
    }
    nav.ephMap[eph.Sat][eph.type][eph.toe] = eph;
}

void decodeGALIon(GTime time, vector<unsigned char>& data)
{
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::GAL)
        return;
    ION ion;
    ion.Sat       = sat;
    int navSource = data[7];
    switch (navSource)
    {
        case 2:
            ion.type = E_NavMsgType::INAV;
            break;
        case 16:
            ion.type = E_NavMsgType::FNAV;
            break;
        default:
            return;
    }
    ion.ttm = time;
    int ind = 8;
    ion.ai0 = bin2float(data, ind);
    ind += 4;
    ion.ai1 = bin2float(data, ind);
    ind += 4;
    ion.ai2 = bin2float(data, ind);
    ind += 4;
    ion.flag                                   = data[ind];
    nav.ionMap[ion.Sat.sys][ion.type][ion.ttm] = ion;
}

void decodeGALUtc(GTime time, vector<unsigned char>& data)
{
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::GAL)
        return;
    STO sto;
    sto.Sat       = sat;
    int navSource = data[7];
    switch (navSource)
    {
        case 2:
            sto.type = E_NavMsgType::INAV;
            break;
        case 16:
            sto.type = E_NavMsgType::FNAV;
            break;
        default:
            return;
    }
    sto.code = E_StoCode::GLUT;
    sto.ttm  = time;
    int ind  = 8;
    sto.A1   = bin2float(data, ind);
    ind += 4;
    sto.A0 = bin2double(data, ind);
    ind += 8;
    double tot = bin2unsigned(data, ind, 4);
    ind += 4;
    double tot_ = tot + 86400;  // GPS vs UTC week
    if (tot_ > 604800)
        tot_ -= 604800;         // GPS v UTC week
    sto.tot                                 = GTime((GTow)tot_, time);
    nav.stoMap[sto.code][sto.type][sto.tot] = sto;

    // to do: input leap seconds
}

void decodeGALGstGps(GTime time, vector<unsigned char>& data)
{
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::GAL)
        return;
    STO sto;
    sto.Sat       = sat;
    int navSource = data[7];
    switch (navSource)
    {
        case 2:
            sto.type = E_NavMsgType::INAV;
            break;
        case 16:
            sto.type = E_NavMsgType::FNAV;
            break;
        default:
            return;
    }
    sto.code = E_StoCode::GAGP;
    sto.ttm  = time;
    int ind  = 8;
    sto.A1   = bin2float(data, ind) * 1e9;
    ind += 4;
    sto.A0 = bin2double(data, ind) * 1e9;
    ind += 8;
    double tot = bin2unsigned(data, ind, 4);
    ind += 4;
    double tot_ = tot + 86400;  // GPS vs UTC week
    if (tot_ > 604800)
        tot_ -= 604800;         // GPS v UTC week
    sto.tot                                 = GTime((GTow)tot_, time);
    nav.stoMap[sto.code][sto.type][sto.tot] = sto;

    // to do: input leap seconds
}

void decodeBDSNav(GTime time, vector<unsigned char>& data)
{
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::BDS)
        return;
    Eph eph;
    eph.Sat  = sat;
    eph.type = E_NavMsgType::D1;
    int ind  = 8;
    eph.week = bin2unsigned(data, ind, 2);
    ind += 2;
    eph.sva  = data[ind++];
    int svh  = data[ind++];
    eph.iodc = data[ind++];
    eph.iode = data[ind++];
    ind += 2;
    eph.tgd[0] = bin2float(data, ind);
    ind += 4;
    eph.tgd[1] = bin2float(data, ind);
    ind += 4;
    double toc = bin2unsigned(data, ind, 4);
    ind += 4;
    eph.f2 = bin2float(data, ind);
    ind += 4;
    eph.f1 = bin2float(data, ind);
    ind += 4;
    eph.f0 = bin2float(data, ind);
    ind += 4;
    eph.crs = bin2float(data, ind);
    ind += 4;
    eph.deln = bin2float(data, ind) * SC2RAD;
    ind += 4;
    eph.M0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.cuc = bin2float(data, ind);
    ind += 4;
    eph.e = bin2double(data, ind);
    ind += 8;
    eph.cus = bin2float(data, ind);
    ind += 4;
    eph.sqrtA = bin2double(data, ind);
    ind += 8;
    double toe = bin2unsigned(data, ind, 4);
    ind += 4;
    eph.cic = bin2float(data, ind);
    ind += 4;
    eph.OMG0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.cis = bin2float(data, ind);
    ind += 4;
    eph.i0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.crc = bin2float(data, ind);
    ind += 4;
    eph.omg = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.OMGd = bin2float(data, ind) * SC2RAD;
    ind += 4;
    eph.idot = bin2float(data, ind) * SC2RAD;
    ind += 4;
    int wn_toc = bin2unsigned(data, ind, 2);
    ind += 2;
    int wn_toe = bin2unsigned(data, ind, 2);
    ind += 2;

    eph.ura[0] = svaToUra(eph.sva);
    wn_toc += 8192 * floor((eph.week - wn_toc + 4096) / 8192);
    wn_toe += 8192 * floor((eph.week - wn_toe + 4096) / 8192);
    eph.toc = GTime(BWeek(wn_toc), BTow(toc));
    eph.toe = GTime(BWeek(wn_toe), BTow(toe));

    nav.ephMap[eph.Sat][eph.type][eph.toe] = eph;
}

void decodeBDSIon(GTime time, vector<unsigned char>& data)
{
    ION    ion;
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::BDS)
        return;
    ion.Sat  = sat;
    ion.type = E_NavMsgType::D1;
    ion.ttm  = time;
    int ind  = 8;
    ion.a0   = bin2float(data, ind);
    ind += 4;
    ion.a1 = bin2float(data, ind);
    ind += 4;
    ion.a2 = bin2float(data, ind);
    ind += 4;
    ion.a3 = bin2float(data, ind);
    ind += 4;
    ion.b0 = bin2float(data, ind);
    ind += 4;
    ion.b1 = bin2float(data, ind);
    ind += 4;
    ion.b2 = bin2float(data, ind);
    ind += 4;
    ion.b3 = bin2float(data, ind);
    ind += 4;

    nav.ionMap[ion.Sat.sys][ion.type][ion.ttm] = ion;
}

void decodeBDSUtc(GTime time, vector<unsigned char>& data)
{
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::BDS)
        return;
    STO sto;
    sto.Sat  = sat;
    sto.type = E_NavMsgType::D1;
    sto.code = E_StoCode::BDUT;
    sto.ttm  = time;
    int ind  = 8;
    sto.A1   = bin2float(data, ind);
    ind += 4;
    sto.A0 = bin2double(data, ind);
    ind += 8;
    sto.tot                                 = time;
    nav.stoMap[sto.code][sto.type][sto.tot] = sto;

    // to do: input leap seconds
}

void decodeQZSNav(GTime time, vector<unsigned char>& data)
{
    SatSys sat = sbsID2Sat(data[6]);
    if (sat.sys != E_Sys::QZS)
        return;
    Eph eph;
    eph.type = E_NavMsgType::LNAV;
    eph.Sat  = sat;
    // data[7] : Reserved
    int ind  = 8;
    eph.week = bin2unsigned(data, ind, 2);
    ind += 2;
    eph.code = data[ind++];
    eph.sva  = data[ind++];
    int svh  = data[ind++];
    eph.flag = data[ind++];
    eph.iodc = bin2unsigned(data, ind, 2);
    ind += 2;
    eph.iode = data[ind++];
    ind++;  // data[17] : IODE in subframe 3
    eph.fit = data[ind++] ? 0 : 4;
    ind++;  // data[19] : Reserved
    eph.tgd[0] = bin2float(data, ind);
    ind += 4;
    double toc = bin2unsigned(data, ind, 4);
    ind += 4;
    eph.f2 = bin2float(data, ind);
    ind += 4;
    eph.f1 = bin2float(data, ind);
    ind += 4;
    eph.f0 = bin2float(data, ind);
    ind += 4;
    eph.crs = bin2float(data, ind);
    ind += 4;
    eph.deln = bin2float(data, ind) * SC2RAD;
    ind += 4;
    eph.M0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.cuc = bin2float(data, ind);
    ind += 4;
    eph.e = bin2double(data, ind);
    ind += 8;
    eph.cus = bin2float(data, ind);
    ind += 4;
    eph.sqrtA = bin2double(data, ind);
    ind += 8;
    double toe = bin2unsigned(data, ind, 4);
    ind += 4;
    eph.cic = bin2float(data, ind);
    ind += 4;
    eph.OMG0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.cis = bin2float(data, ind);
    ind += 4;
    eph.i0 = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.crc = bin2float(data, ind);
    ind += 4;
    eph.omg = bin2double(data, ind) * SC2RAD;
    ind += 8;
    eph.OMGd = bin2float(data, ind) * SC2RAD;
    ind += 4;
    eph.idot = bin2float(data, ind);
    ind += 4;
    int wn_toc = bin2unsigned(data, ind, 2);
    ind += 2;
    int wn_toe = bin2unsigned(data, ind, 2);
    ind += 2;

    eph.ura[0] = svaToUra(eph.sva);
    eph.svh    = (E_Svh)svh;
    wn_toc += 1024 * floor((eph.week - wn_toc + 512) / 1024);
    wn_toe += 1024 * floor((eph.week - wn_toe + 512) / 1024);
    eph.toc                                = gpst2time(wn_toc, toc);
    eph.toe                                = gpst2time(wn_toe, toe);
    nav.ephMap[eph.Sat][eph.type][eph.toe] = eph;
}

void SbfDecoder::decode(unsigned short int id, vector<unsigned char>& data)
{
    if (data.size() < 8)
        return;
    double tow  = bin2unsigned(data, 0, 4) * 0.001;
    int    week = bin2unsigned(data, 4, 2);

    GTime time = gpst2time(week, tow);
    // std::cout << "\nSBF message type " << id << ", " << time.to_string(3);

    switch (id)
    {
        case 4002:
            decodeGALNav(time, data);
            return;
        case 4004:   /*decodeGLONav(time,data);   */
            return;  // GLONASS not supported
        case 4020:
            decodeGEORawL1(time, data);
            return;
        case 4021:
            decodeGEORawL5(time, data);
            return;
        case 4027:
            decodeMeasEpoch(time, data);
            return;
        case 4030:
            decodeGALIon(time, data);
            return;
        case 4031:
            decodeGALUtc(time, data);
            return;
        case 4032:
            decodeGALGstGps(time, data);
            return;
        case 4036:   /*decodeGLOTime(time,data);   */
            return;  // GLONASS not supported
        case 4081:
            decodeBDSNav(time, data);
            return;
        case 4095:
            decodeQZSNav(time, data);
            return;
        case 4120:
            decodeBDSIon(time, data);
            return;
        case 4121:
            decodeBDSUtc(time, data);
            return;
        case 5891:
            decodeGPSNav(time, data);
            return;
        case 5893:
            decodeGPSIon(time, data);
            return;
        case 5894:
            decodeGPSUtc(time, data);
            return;
        case 5922:
            decodeEndOfMeas(time);
            return;
        // default: std::cout << " ...  not supported, yet";              return;
    }
}
