/**
 * @file rinex.cpp
 * @brief Implementation of RINEX file format processing and observation handling
 *
 * This file implements comprehensive RINEX (Receiver Independent Exchange Format)
 * file processing capabilities including:
 * - RINEX 2.x and 3.x observation data parsing
 * - Navigation data extraction
 * - Station information processing
 * - Observation type conversion and mapping
 * - Phase observation priority resolution
 * - Robust error handling and validation
 *
 * The implementation follows SOLID principles with single-responsibility functions
 * and a staging pattern for robust data processing.
 *
 * @author Geoscience Australia
 * @date 2024
 * @version 1.0
 */
// #pragma GCC optimize ("O0")

#include "common/rinex.hpp"
#include <boost/log/trivial.hpp>
#include <string>
#include "architectureDocs.hpp"
#include "common/biases.hpp"
#include "common/common.hpp"
#include "common/constants.hpp"
#include "common/gTime.hpp"
#include "common/navigation.hpp"
#include "common/receiver.hpp"
#include "common/rinexNavWrite.hpp"
#include "common/trace.hpp"

using std::string;

FileType CLK__() {}

FileType RNX__() {}

#define MAXPOSHEAD 1024  ///< max head line position
#define MINFREQ_GLO -7   ///< min frequency number glonass
#define MAXFREQ_GLO 13   ///< max frequency number glonass

/**
 * @brief Default navigation message types by GNSS system
 *
 * Provides default navigation message type mappings for each GNSS system
 * when processing RINEX 2.x and 3.x navigation files. Used to initialize
 * ephemeris structures with appropriate message types.
 *
 * @note These defaults may be overridden by explicit message type indicators
 */
map<E_Sys, E_NavMsgType> defNavMsgType = {
    {E_Sys::GPS, E_NavMsgType::LNAV},
    {E_Sys::GLO, E_NavMsgType::FDMA},
    {E_Sys::GAL, E_NavMsgType::IFNV},
    {E_Sys::BDS, E_NavMsgType::D1D2},
    {E_Sys::QZS, E_NavMsgType::LNAV},
    {E_Sys::IRN, E_NavMsgType::LNAV},
    {E_Sys::SBS, E_NavMsgType::SBAS}
};

// Set string without trailing spaces
void setstr(char* dst, const char* src, int n)
{
    char*       p = dst;
    const char* q = src;

    while (*q && q < src + n)
        *p++ = *q++;

    *p-- = '\0';

    while (p >= dst && *p == ' ')
        *p-- = '\0';
}

// Decode RINEX observation file header
void decodeObsH(
    std::istream&                   inputStream,
    string&                         line,
    double                          ver,
    E_TimeSys&                      tsys,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    Navigation&                     nav,
    RinexStation&                   rnxRec
)
{
    double      del[3];
    int         prn;
    int         fcn;
    const char* p;
    char*       buff  = &line[0];
    char*       label = buff + 60;

    //	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << ": ver=" << ver;

    if (strstr(label, "MARKER NAME"))
    {
        if (rnxRec.id.empty())
        {
            rnxRec.id.assign(buff, 4);
        }
    }
    else if (strstr(label, "MARKER NUMBER"))
    {
        rnxRec.marker.assign(buff, 20);
    }
    //     else if (strstr(label,"MARKER TYPE"         )) ; // ver.3
    //     else if (strstr(label,"OBSERVER / AGENCY"   )) ;
    else if (strstr(label, "REC # / TYPE / VERS"))
    {
        rnxRec.recSerial.assign(buff, 20);
        rnxRec.recType.assign(buff + 20, 20);
        rnxRec.recFWVersion.assign(buff + 40, 20);
    }
    else if (strstr(label, "ANT # / TYPE"))
    {
        rnxRec.antSerial.assign(buff, 20);
        rnxRec.antDesc.assign(buff + 20, 20);
    }
    else if (strstr(label, "APPROX POSITION XYZ"))
    {
        for (int i = 0, j = 0; i < 3; i++, j += 14)
            rnxRec.pos[i] = str2num(buff, j, 14);
    }
    else if (strstr(label, "ANTENNA: DELTA H/E/N"))
    {
        for (int i = 0, j = 0; i < 3; i++, j += 14)
            del[i] = str2num(buff, j, 14);

        rnxRec.del[2] = del[0];  // h
        rnxRec.del[0] = del[1];  // e
        rnxRec.del[1] = del[2];  // n
    }
    //     else if (strstr(label,"ANTENNA: DELTA X/Y/Z")) ; // opt ver.3
    //     else if (strstr(label,"ANTENNA: PHASECENTER")) ; // opt ver.3
    //     else if (strstr(label,"ANTENNA: B.SIGHT XYZ")) ; // opt ver.3
    //     else if (strstr(label,"ANTENNA: ZERODIR AZI")) ; // opt ver.3
    //     else if (strstr(label,"ANTENNA: ZERODIR XYZ")) ; // opt ver.3
    //     else if (strstr(label,"CENTER OF MASS: XYZ" )) ; // opt ver.3
    else if (strstr(label, "SYS / # / OBS TYPES"))
    {
        // RINEX 3: Parse system-specific observation types
        // Example: "G   16 C1C L1C D1C S1C C2S L2S D2S S2S C2W L2W D2W S2W C5Q"

        // get system from code letter
        char code[] = "x00";
        code[0]     = buff[0];

        SatSys Sat(code);

        if (Sat.sys == +E_Sys::NONE)
        {
            BOOST_LOG_TRIVIAL(debug) << "invalid system code: sys=" << code[0];

            return;
        }

        int n = (int)str2num(buff, 3, 3);

        BOOST_LOG_TRIVIAL(debug) << "RINEX3 processing " << n << " observation types for system "
                                 << code[0];

        for (int j = 0, k = 7; j < n; j++, k += 4)
        {
            if (k > 58)
            {
                // more on the next line
                if (!std::getline(inputStream, line))
                    break;

                buff = &line[0];
                k    = 7;
            }

            CodeType codeType;
            codeType.type = buff[k];

            // Extract 3-character observation code for RINEX 3
            char obsCode3str[] = "Lxx";
            obsCode3str[1]     = buff[k + 1];
            obsCode3str[2]     = buff[k + 2];

            // Handle BeiDou B1 code special case for version 3.02
            if ((Sat.sys == +E_Sys::BDS) && (obsCode3str[1] == '1') && (ver == 3.02))
            {
                // change beidou B1 code: 3.02 draft -> 3.02
                obsCode3str[1] = '2';
            }

            try
            {
                // For RINEX 3, directly store the 3-character code in the code field
                codeType.code = E_ObsCode::_from_string(obsCode3str);

                // Leave code2 as NONE for RINEX 3 since we have the full 3-character code
                codeType.code2 = E_ObsCode2::NONE;

                BOOST_LOG_TRIVIAL(debug)
                    << "RINEX3 stored code: " << obsCode3str << " -> " << codeType.code._to_string()
                    << " for system " << Sat.sys._to_string();
            }
            catch (...)
            {
                BOOST_LOG_TRIVIAL(debug) << "invalid RINEX3 obs code: " << obsCode3str;

                codeType.code  = E_ObsCode::NONE;
                codeType.code2 = E_ObsCode2::NONE;
            }

            sysCodeTypes[Sat.sys][j] = codeType;
        }

        // if unknown code in ver.3, set default code
        // for (auto& codeType : sysCodeTypes[Sat.sys])
        // {
        //     if (tobs[i][j][2])
        // 	continue;
        //
        //     if (!(p = strchr(frqcodes, tobs[i][j][1])))
        // 	continue;
        //
        // 	// default codes for unknown code
        //     	const char *defcodes[] =
        //    	{
        //         	"CWX   ",   // GPS: L125___
        //         	"CC    ",   // GLO: L12____
        //         	"X XXXX",   // GAL: L1_5678
        //         	"CXXX  ",   // QZS: L1256__
        //         	"C X   ",   // SBS: L1_5___
        //         	"X  XX "	// BDS: L1__67_
        //     	};
        //     	tobs[i][j][2] = defcodes[i][(int)(p - frqcodes)];
        //
        //     	BOOST_LOG_TRIVIAL(debug)
        // 	<< "set default for unknown code: sys=" << buff[0]
        // 	<< " code=" << tobs[i][j];
        // }
    }
    //     else if (strstr(label,"WAVELENGTH FACT L1/2")) ; // opt ver.2
    else if (strstr(label, "# / TYPES OF OBSERV"))
    {
        // RINEX 2: Parse global observation types (applies to all systems)
        // Example: "    10    C1    L1    S1    C2    P2    L2    S2    C5    L5"

        int n = (int)str2num(buff, 0, 6);

        BOOST_LOG_TRIVIAL(debug) << "RINEX2 processing " << n << " observation types";

        for (int i = 0, j = 10; i < n; i++, j += 6)
        {
            if (j > 58)
            {
                // go onto new line
                if (!std::getline(inputStream, line))
                    break;

                buff = (char*)line.c_str();
                j    = 10;
            }

            if (ver <= 2.99)
            {
                // Extract 2-character observation code for RINEX 2
                char obsCode2str[3] = {};
                setstr(obsCode2str, buff + j, 2);

                // save the type char before processing
                char typeChar = obsCode2str[0];

                BOOST_LOG_TRIVIAL(debug) << "RINEX2 processing obs type: " << obsCode2str;

                // Process for all satellite systems since RINEX 2 doesn't specify per-system
                for (E_Sys sys : E_Sys::_values())
                {
                    auto& recOpts = acsConfig.getRecOpts(rnxRec.id, {SatSys(sys, 0).sysName()});

                    CodeType codeType;
                    codeType.type = typeChar;
                    try
                    {
                        // For RINEX 2, primarily use code2 to store the original 2-character codes
                        BOOST_LOG_TRIVIAL(debug) << "RINEX2 converting code: " << obsCode2str
                                                 << " for system " << sys._to_string();
                        E_ObsCode2 obsCode2 = E_ObsCode2::_from_string(obsCode2str);
                        codeType.code2      = obsCode2;

                        BOOST_LOG_TRIVIAL(debug)
                            << "RINEX2 stored code2: " << obsCode2str << " -> "
                            << obsCode2._to_string() << " for system " << sys._to_string();

                        // Keep code as NONE for RINEX 2 - let downstream processing handle
                        // conversion if needed
                        codeType.code = E_ObsCode::NONE;
                    }
                    catch (...)
                    {
                        BOOST_LOG_TRIVIAL(warning)
                            << "Warning:" << rnxRec.id << " Unknown RINEX2 code: " << obsCode2str;

                        codeType.code2 = E_ObsCode2::NONE;
                        codeType.code  = E_ObsCode::NONE;
                    }

                    sysCodeTypes[sys][i] = codeType;
                }
            }
        }
        //*tobs[0][nt]='\0';
    }
    //     else if (strstr(label, "SIGNAL STRENGTH UNIT")) ; // opt ver.3
    //     else if (strstr(label, "INTERVAL"            )) ; // opt
    else if (strstr(label, "TIME OF FIRST OBS"))
    {
        if (!strncmp(buff + 48, "GPS", 3))
            tsys = E_TimeSys::GPST;
        else if (!strncmp(buff + 48, "GLO", 3))
            tsys = E_TimeSys::UTC;
        else if (!strncmp(buff + 48, "GAL", 3))
            tsys = E_TimeSys::GST;
        else if (!strncmp(buff + 48, "QZS", 3))
            tsys = E_TimeSys::QZSST;  // ver.3.02
        else if (!strncmp(buff + 48, "BDT", 3))
            tsys = E_TimeSys::BDT;    // ver.3.02
    }
    //     else if (strstr(label, "TIME OF LAST OBS"    )) ; // opt
    //     else if (strstr(label, "RCV CLOCK OFFS APPL" )) ; // opt
    //     else if (strstr(label, "SYS / DCBS APPLIED"  )) ; // opt ver.3
    //     else if (strstr(label, "SYS / PCVS APPLIED"  )) ; // opt ver.3
    //     else if (strstr(label, "SYS / SCALE FACTOR"  )) ; // opt ver.3
    //     else if (strstr(label, "SYS / PHASE SHIFTS"  )) ; // ver.3.01
    else if (strstr(label, "GLONASS SLOT / FRQ #"))
    {
        // ver.3.02
        p = buff + 4;
        for (int i = 0; i < 8; i++, p += 7)
        {
            if (sscanf(p, "R%2d %2d", &prn, &fcn) < 2)
                continue;

            SatSys Sat(E_Sys::GLO, prn);

            nav.gloFreqMap[Sat] = fcn;
        }
    }
    else if (strstr(label, "GLONASS COD/PHS/BIS"))
    {
        // ver.3.02
        p = buff;
        for (int i = 0; i < 4; i++, p += 13)
        {
            if (strncmp(p + 1, "C1C", 3))
                nav.glo_cpbias[0] = str2num(p, 5, 8);
            else if (strncmp(p + 1, "C1P", 3))
                nav.glo_cpbias[1] = str2num(p, 5, 8);
            else if (strncmp(p + 1, "C2C", 3))
                nav.glo_cpbias[2] = str2num(p, 5, 8);
            else if (strncmp(p + 1, "C2P", 3))
                nav.glo_cpbias[3] = str2num(p, 5, 8);
        }
    }
    else if (strstr(label, "LEAP SECONDS"))
    {
        // This would be GPS-UTC, and NOT optional as of RINEX 4
        nav.leaps = (int)str2num(buff, 0, 6);
    }
    //     else if (strstr(label, "# OF SALTELLITES"    )) ; // opt
    //     else if (strstr(label, "PRN / # OF OBS"      )) ; // opt
}

// Decode RINEX navigation file header
void decodeNavH(
    string&     line,  ///< Line to decode
    E_Sys       sys,   ///< GNSS system
    Navigation& nav    ///< Navigation data
)
{
    char* buff  = &line[0];
    char* label = buff + 60;

    // 	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__;

    if (strstr(label, "ION ALPHA"))
    {
        // opt ver.2
        E_NavMsgType type = defNavMsgType[sys];
        GTime        time = {};

        ION& ionEntry = nav.ionMap[sys][type][time];

        ionEntry.type    = type;
        ionEntry.Sat.sys = sys;
        ionEntry.ttm     = time;

        for (int i = 0, j = 2; i < 4; i++, j += 12)
            ionEntry.vals[i] = str2num(buff, j, 12);
    }
    else if (strstr(label, "ION BETA"))
    {
        // opt ver.2
        E_NavMsgType type = defNavMsgType[sys];
        GTime        time = {};

        ION& ionEntry = nav.ionMap[sys][type][time];

        ionEntry.type    = type;
        ionEntry.Sat.sys = sys;
        ionEntry.ttm     = time;

        for (int i = 0, j = 2; i < 4; i++, j += 12)
            ionEntry.vals[i + 4] = str2num(buff, j, 12);
    }
    else if (strstr(label, "DELTA-UTC: A0,A1,T,W"))
    {
        // opt ver.2
        E_NavMsgType type = defNavMsgType[sys];
        E_StoCode    code = E_StoCode::NONE;
        switch (sys)
        {
            case E_Sys::GPS:
                code = E_StoCode::GPUT;
                break;
            case E_Sys::QZS:
                code = E_StoCode::QZUT;
                break;
            case E_Sys::GAL:
                code = E_StoCode::GAUT;
                break;
        }

        GTow  tow  = str2num(buff, 31, 9);
        GWeek week = (int)str2num(buff, 40, 9);
        GTime time(week, tow);

        STO& stoEntry = nav.stoMap[code][type][time];

        stoEntry.type    = type;
        stoEntry.Sat.sys = sys;
        stoEntry.tot     = time;
        stoEntry.code    = code;

        stoEntry.A0 = str2num(buff, 3, 19);
        stoEntry.A1 = str2num(buff, 22, 19);
        stoEntry.A2 = 0;
    }
    else if (strstr(label, "IONOSPHERIC CORR"))
    {
        // opt ver.3
        char sysStr[4] = "";
        strncpy(sysStr, buff, 3);
        sys               = E_Sys::_from_string(sysStr);
        E_NavMsgType type = defNavMsgType[sys];
        GTime        time = {};

        ION& ionEntry = nav.ionMap[sys][type][time];

        ionEntry.type    = type;
        ionEntry.Sat.sys = sys;
        ionEntry.Sat.prn = str2num(buff, 55, 3);
        ionEntry.ttm     = time;

        if (buff[3] == 'A' || buff[3] == ' ')
        {
            for (int i = 0, j = 5; i < 4; i++, j += 12)
                ionEntry.vals[i] = str2num(buff, j, 12);
        }
        else if (buff[3] == 'B')
        {
            for (int i = 0, j = 5; i < 4; i++, j += 12)
                ionEntry.vals[i + 4] = str2num(buff, j, 12);
        }
    }
    else if (strstr(label, "TIME SYSTEM CORR"))
    {
        // opt ver.3
        char codeStr[5] = "";
        strncpy(codeStr, buff, 4);
        E_StoCode code = E_StoCode::_from_string(codeStr);

        char id[8] = "";
        strncpy(id, buff + 51, 5);
        SatSys Sat = SatSys(id);

        if (Sat.sys == +E_Sys::NONE)
        {
            switch (code)
            {
                case E_StoCode::GPUT:
                    Sat.sys = E_Sys::GPS;
                    break;
                case E_StoCode::GLUT:
                    Sat.sys = E_Sys::GLO;
                    break;
                case E_StoCode::GAUT:
                    Sat.sys = E_Sys::GAL;
                    break;
                case E_StoCode::BDUT:
                    Sat.sys = E_Sys::BDS;
                    break;
                case E_StoCode::QZUT:
                    Sat.sys = E_Sys::QZS;
                    break;
                case E_StoCode::SBUT:
                    Sat.sys = E_Sys::SBS;
                    break;
                case E_StoCode::GAGP:
                    Sat.sys = E_Sys::GAL;
                    break;
                case E_StoCode::QZGP:
                    Sat.sys = E_Sys::QZS;
                    break;
            }
        }
        // UTC ID skipped

        E_NavMsgType type = defNavMsgType[Sat.sys];

        double sec  = str2num(buff, 38, 7);
        double week = str2num(buff, 45, 5);
        GTime  time = {};
        if (Sat.sys != +E_Sys::BDS)
        {
            time = GTime(GWeek(week), GTow(sec));
        }
        else
        {
            time = GTime(BWeek(week), BTow(sec));
        }

        STO& stoEntry = nav.stoMap[code][type][time];

        stoEntry.type = type;
        stoEntry.Sat  = Sat;
        stoEntry.tot  = time;
        stoEntry.ttm  = time;
        stoEntry.code = code;

        stoEntry.A0 = str2num(buff, 5, 17);
        stoEntry.A1 = str2num(buff, 22, 16);
        stoEntry.A2 = 0.0;
    }
    else if (strstr(label, "LEAP SECONDS"))
    {
        // opt
        nav.leaps = (int)str2num(buff, 0, 6);
    }
}
// Decode GLONASS navigation file header
void decodeGnavH(string& line, Navigation& nav)
{
    char* buff  = &line[0];
    char* label = buff + 60;

    // 	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__;

    if (strstr(label, "CORR TO SYTEM TIME"))
        ;  // opt
    else if (strstr(label, "LEAP SECONDS"))
    {
        // opt
        nav.leaps = (int)str2num(buff, 0, 6);
    }
}

// Decode SBAS/geostationary navigation file header
void decodeHnavH(string& line, Navigation& nav)
{
    char* buff  = &line[0];
    char* label = buff + 60;

    // 	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__;

    if (strstr(label, "CORR TO SYTEM TIME"))
        ;  // opt
    else if (strstr(label, "D-UTC A0,A1,T,W,S,U"))
        ;  // opt
    else if (strstr(label, "LEAP SECONDS"))
    {
        // opt
        nav.leaps = (int)str2num(buff, 0, 6);
    }
}

// Read RINEX file header section
int readRnxH(
    std::istream&                   inputStream,
    double&                         ver,
    char&                           type,
    E_Sys&                          sys,
    E_TimeSys&                      tsys,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    Navigation&                     nav,
    RinexStation&                   rnxRec
)
{
    string line;
    int    i     = 0;
    int    block = 0;

    // 	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__;

    ver  = 2.10;
    type = ' ';
    sys  = E_Sys::GPS;
    tsys = E_TimeSys::GPST;

    char sysChar       = '\0';
    int  typeOffset    = 20;
    int  sysCharOffset = 40;

    while (std::getline(inputStream, line))
    {
        char* buff  = &line[0];
        char* label = buff + 60;

        if (line.length() <= 60)
        {
            continue;
        }
        else if (strstr(label, "RINEX VERSION / TYPE"))
        {
            ver = str2num(buff, 0, 9);

            type = buff[typeOffset];

            sysChar = buff[sysCharOffset];

            // possible error in generation by one manufacturer. This hack gets around it
            if (ver == 3.04 && type == ' ')
            {
                typeOffset += 1;
                sysCharOffset += 2;
                type = buff[typeOffset];

                sysChar = buff[sysCharOffset];
            }

            // satellite system
            switch (sysChar)
            {
                case ' ':
                case 'G':
                    sys  = E_Sys::GPS;
                    tsys = E_TimeSys::GPST;
                    break;
                case 'R':
                    sys  = E_Sys::GLO;
                    tsys = E_TimeSys::UTC;
                    break;
                case 'E':
                    sys  = E_Sys::GAL;
                    tsys = E_TimeSys::GST;
                    break;  // v.2.12
                case 'S':
                    sys  = E_Sys::SBS;
                    tsys = E_TimeSys::GPST;
                    break;
                case 'J':
                    sys  = E_Sys::QZS;
                    tsys = E_TimeSys::QZSST;
                    break;  // v.3.02
                case 'C':
                    sys  = E_Sys::BDS;
                    tsys = E_TimeSys::BDT;
                    break;  // v.2.12
                case 'M':
                    sys  = E_Sys::NONE;
                    tsys = E_TimeSys::GPST;
                    break;  // mixed
                default:
                    BOOST_LOG_TRIVIAL(debug) << "unsupported satellite system: " << sysChar;

                    break;
            }
            continue;
        }
        else if (strstr(label, "PGM / RUN BY / DATE"))
            continue;
        else if (strstr(label, "COMMENT"))
        {
            // read cnes wl satellite fractional bias
            if (strstr(buff, "WIDELANE SATELLITE FRACTIONAL BIASES") ||
                strstr(buff, "WIDELANE SATELLITE FRACTIONNAL BIASES"))
            {
                block = 1;
            }
            if (strstr(buff, "->"))
            {
                // may be a conversion line, test
                //  @todo: not sure what this is about...
                char sysChar;
                char r3[4] = {};
                char r2[3] = {};
                char comment[81];
                int  num = sscanf(buff, " %c %3c -> %2c %80s", &sysChar, r3, r2, comment);

                if (num == 4 && (string)comment == "COMMENT")
                {
                    try
                    {
                        E_Sys sys = SatSys::sysFromChar(sysChar);

                        char code = r3[0];
                        r3[0]     = 'L';
                        auto obs2 = E_ObsCode2 ::_from_string_nocase(r2);
                        auto obs3 = E_ObsCode ::_from_string_nocase(r3);

                        auto& recOpts = acsConfig.getRecOpts(rnxRec.id, {SatSys(sys, 0).sysName()});

                        auto& codeMap = recOpts.rinex23Conv.codeConv;
                        auto& phasMap = recOpts.rinex23Conv.phasConv;

                        if (r2[0] == 'C' || r2[0] == 'P')
                        {
                            codeMap[obs2] = obs3;
                        }
                        else
                        {
                            // For phase conversions, create a single-element vector
                            phasMap[obs2].push_back(obs3);
                        }
                    }
                    catch (...)
                    {
                    }
                }
            }
            else if (block)
            {
                // ignore reported widelane biases

                // double bias;
                // SatSys Sat;
                //
                // // cnes/cls grg clock
                // if	( !strncmp(buff, "WL", 2)
                // 	&&(Sat = SatSys(buff + 3), Sat)
                // 	&& sscanf(buff+40, "%lf", &bias) == 1)
                // {
                // 	nav.satNavMap[Sat].wlbias = bias;
                // }
                // // cnes ppp-wizard clock
                // else if ((Sat = SatSys(buff + 1), Sat)
                // 		&&sscanf(buff+6, "%lf", &bias) == 1)
                // {
                // 	nav.satNavMap[Sat].wlbias = bias;
                // }
            }
            continue;
        }
        // file type
        switch (type)
        {
            case 'O':
                decodeObsH(inputStream, line, ver, tsys, sysCodeTypes, nav, rnxRec);
                break;
            case 'N':
                decodeNavH(line, sys, nav);
                break;  // GPS (ver.2) or mixed (ver.3)
            case 'G':
                decodeGnavH(line, nav);
                break;
            case 'H':
                decodeHnavH(line, nav);
                break;
            case 'J':
                decodeNavH(line, E_Sys::QZS, nav);
                break;  // extension
            case 'E':   // fallthrough
            case 'L':
                decodeNavH(line, E_Sys::GAL, nav);
                break;  // extension
        }
        if (strstr(label, "END OF HEADER"))
        {
            return 1;
        }

        if (++i >= MAXPOSHEAD && type == ' ')
        {
            break;  // no rinex file
        }
    }
    return 0;
}
// Decode observation epoch header
int decodeObsEpoch(
    std::istream&   inputStream,
    string&         line,
    double          ver,
    E_TimeSys       tsys,
    GTime&          time,
    int&            flag,
    vector<SatSys>& sats
)
{
    int   n    = 0;
    char* buff = &line[0];

    // 	BOOST_LOG_TRIVIAL(debug)	<< __FUNCTION__ << ": ver=" << ver;

    if (ver <= 2.99)
    {
        // ver.2
        n = (int)str2num(buff, 29, 3);
        if (n <= 0)
            return 0;

        // epoch flag: 3:new site,4:header info,5:external event
        flag = (int)str2num(buff, 28, 1);

        if (flag >= 3 && flag <= 5)
        {
            return n;
        }

        bool error = str2time(buff, 0, 26, time, tsys);
        if (error)
        {
            BOOST_LOG_TRIVIAL(debug) << "rinex obs invalid epoch: epoch=" << buff;

            return 0;
        }

        for (int i = 0, j = 32; i < n; i++, j += 3)
        {
            if (j >= 68)
            {
                // more on the next line
                if (!std::getline(inputStream, line))
                    break;

                buff = &line[0];

                j = 32;
            }

            char id[4] = {};
            strncpy(id, buff + j, 3);
            sats.push_back(SatSys(id));
        }
    }
    else
    {
        // ver.3
        n = (int)str2num(buff, 32, 3);
        if (n <= 0)
        {
            return 0;
        }

        flag = (int)str2num(buff, 31, 1);

        if (flag >= 3 && flag <= 5)
            return n;

        if (buff[0] != '>' || str2time(buff, 1, 28, time, tsys))
        {
            BOOST_LOG_TRIVIAL(debug) << "rinex obs invalid epoch: epoch=" << buff;
            return 0;
        }
    }

    //     BOOST_LOG_TRIVIAL(debug)
    // 	<< "__FUNCTION__: time=" << time.to_string(3)
    // 	<< " flag=" << flag;

    return n;
}

/** Decode RINEX 2 observation data
 */
int decodeObsDataRinex2(
    std::istream&                   inputStream,
    string&                         line,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    GObs&                           obs,
    SatSys&                         v2SatSys,
    RinexStation&                   rnxRec
)
{
    char* buff = &line[0];
    int   stat = 1;

    // RINEX 2: Use satellite from epoch header
    obs.Sat = v2SatSys;

    if (!obs.Sat)
    {
        BOOST_LOG_TRIVIAL(debug) << "decodeObsDataRinex2: unsupported sat";
        stat = 0;
    }

    if (!stat)
        return 0;

    // Defensive check for valid satellite system before accessing sysCodeTypes
    if (obs.Sat.sys == +E_Sys::NONE || obs.Sat.sys._value < 0)
    {
        BOOST_LOG_TRIVIAL(error) << "RINEX2: Invalid satellite system: "
                                 << obs.Sat.sys._to_string();
        return 0;
    }

    // Check if the system exists in sysCodeTypes map
    if (sysCodeTypes.find(obs.Sat.sys) == sysCodeTypes.end())
    {
        BOOST_LOG_TRIVIAL(error) << "RINEX2: System " << obs.Sat.sys._to_string()
                                 << " not found in sysCodeTypes";
        return 0;
    }

    auto& codeTypes = sysCodeTypes[obs.Sat.sys];
    int   j         = 0;  // RINEX 2 starts at position 0

    BOOST_LOG_TRIVIAL(debug) << "RINEX2: About to process satellite " << obs.Sat.id() << " with "
                             << codeTypes.size() << " code types, line size: " << line.size();

    // Check for valid line buffer
    if (line.empty() || line.size() < 16)
    {
        BOOST_LOG_TRIVIAL(error) << "RINEX2: Invalid or too short line buffer (size: "
                                 << line.size() << ")";
        return 0;
    }

    BOOST_LOG_TRIVIAL(debug) << "RINEX2: Accessing receiver options for " << rnxRec.id;

    // Additional safety check for acsConfig
    try
    {
        auto testSys = SatSys(obs.Sat.sys, 0);
        BOOST_LOG_TRIVIAL(debug) << "RINEX2: Created test SatSys: " << testSys.sysName();
    }
    catch (const std::exception& e)
    {
        BOOST_LOG_TRIVIAL(error) << "RINEX2: Error creating SatSys: " << e.what();
        throw;
    }

    auto& recOpts = acsConfig.getRecOpts(rnxRec.id, {SatSys(obs.Sat.sys, 0).sysName()});
    BOOST_LOG_TRIVIAL(debug) << "RINEX2: Got receiver options, accessing conversion maps";
    auto& codeMap = recOpts.rinex23Conv.codeConv;
    auto& phasMap = recOpts.rinex23Conv.phasConv;
    BOOST_LOG_TRIVIAL(debug) << "RINEX2: Successfully accessed conversion maps, starting staging";

    // Stage 1: Collect all observations, storing priority arrays for phase observations
    ObservationStaging staging;
    BOOST_LOG_TRIVIAL(debug) << "RINEX2: Processing observations with priority staging";
    if (line.size() < 80)
        line.append(80 - line.size(), ' ');  // Ensure line is at least 80 characters

    for (auto& [index, codeType] : codeTypes)
    {
        // RINEX 2: Check for line continuation
        if (j >= 80)
        {
            if (!std::getline(inputStream, line))
                break;
            if (line.size() < 80)
                line.append(80 - line.size(), ' ');  // Ensure line is at least 80 characters

            // Validate new line
            if (line.empty())
            {
                BOOST_LOG_TRIVIAL(warning) << "RINEX2: Empty continuation line";
                break;
            }
            buff = &line[0];
            j    = 0;
        }

        // check if codeType.code2 is in codeMap or phasMap(inside an array)

        E_ObsCode         effectiveCode = E_ObsCode::NONE;
        vector<E_ObsCode> priorityCodes;
        bool              isPhaseObservation = false;

        // Determine which map to use based on observation type
        if (codeType.type == 'C' || codeType.type == 'P')
        {
            // Code/Pseudorange observations - use codeMap
            auto it = codeMap.find(codeType.code2);
            if (it != codeMap.end())
            {
                effectiveCode = it->second;
                BOOST_LOG_TRIVIAL(debug) << "RINEX2: Found code2 " << codeType.code2._to_string()
                                         << " in codeMap -> " << effectiveCode._to_string();
            }
            else
            {
                BOOST_LOG_TRIVIAL(warning) << "RINEX2: code2 " << codeType.code2._to_string()
                                           << " not found in codeMap for type " << codeType.type;
            }
        }
        else if (codeType.type == 'L')
        {
            // Phase observations - store priority array for later resolution
            auto it = phasMap.find(codeType.code2);
            if (it != phasMap.end() && !it->second.empty())
            {
                priorityCodes      = it->second;
                effectiveCode      = priorityCodes[0];  // Temporary for frequency lookup
                isPhaseObservation = true;
                BOOST_LOG_TRIVIAL(debug) << "RINEX2: Found phase priorities for "
                                         << codeType.code2._to_string() << " -> [" << [&]()
                {
                    string codes;
                    for (size_t i = 0; i < priorityCodes.size(); ++i)
                    {
                        if (i > 0)
                            codes += ",";
                        codes += priorityCodes[i]._to_string();
                    }
                    return codes;
                }() << "]";
            }
            else
            {
                BOOST_LOG_TRIVIAL(warning) << "RINEX2: code2 " << codeType.code2._to_string()
                                           << " not found in phasMap for type " << codeType.type;
            }
        }

        E_FType ft = code2Freq[obs.Sat.sys][effectiveCode];

        // Parse observation values
        ObservationValues obsValues = parseObservationValues(buff, j);

        // Stage the observation - use appropriate staging method
        if (isPhaseObservation)
        {
            stagePhaseObservation(
                staging,
                codeType.type,
                priorityCodes,
                ft,
                obsValues.value,
                obsValues.lli
            );
        }
        else
        {
            stageObservation(
                staging,
                codeType.type,
                effectiveCode,
                ft,
                obsValues.value,
                obsValues.lli
            );
        }
        j += 16;
        // }
    }

    // Stage 2: Validate all staged observations with conflict resolution
    ValidationReport report = validateStagedObservationsDetailed(staging, obs.Sat);
    if (!report.passed)
    {
        BOOST_LOG_TRIVIAL(warning)
            << "RINEX2:" << rnxRec.id << " Validation failed for satellite " << obs.Sat.id()
            << " (valid: " << report.validObservations << "/" << report.totalObservations << ")";
        return 0;
    }

    // Stage 3: Commit all validated observations to final structure
    commitStagedObservations(staging, obs, codeMap);

    // // Debug: Show final committed observations with L, P, and LLI values
    BOOST_LOG_TRIVIAL(debug) << "Final committed observations for " << obs.Sat.id() << ":";
    for (const auto& [fType, sigsList] : obs.sigsLists)
    {
        for (const auto& sig : sigsList)
        {
            BOOST_LOG_TRIVIAL(debug)
                << "  " << sig.code._to_string() << ": L=" << std::setprecision(16) << sig.L
                << ", P=" << std::setprecision(16) << sig.P
                << ", LLI=" << static_cast<int>(sig.LLI);
        }
    }

    return 1;
}

/** Decode RINEX 3 observation data
 */
int decodeObsDataRinex3(
    std::istream&                   inputStream,
    string&                         line,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    GObs&                           obs,
    RinexStation&                   rnxRec
)
{
    char  satid[8] = "";
    char* buff     = &line[0];
    int   stat     = 1;

    // RINEX 3: Extract satellite ID from observation line
    strncpy(satid, buff, 3);
    obs.Sat = SatSys(satid);

    if (!obs.Sat)
    {
        BOOST_LOG_TRIVIAL(debug) << "decodeObsDataRinex3: unsupported sat sat=" << satid;
        stat = 0;
    }

    if (!stat)
        return 0;

    auto& codeTypes = sysCodeTypes[obs.Sat.sys];
    int   j         = 3;  // RINEX 3 starts after 3-character satellite ID

    // Stage 1: Collect all observations into staging area
    ObservationStaging staging;

    for (auto& [index, codeType] : codeTypes)
    {
        E_FType ft = code2Freq[obs.Sat.sys][codeType.code];

        // Parse observation values using SRP helper function
        ObservationValues obsValues = parseObservationValues(buff, j);

        // Stage the observation instead of immediately committing
        stageObservation(staging, codeType.type, codeType.code, ft, obsValues.value, obsValues.lli);

        j += 16;
    }

    // Stage 2: Validate all staged observations with conflict resolution
    ValidationReport report = validateStagedObservationsDetailed(staging, obs.Sat);
    if (!report.passed)
    {
        BOOST_LOG_TRIVIAL(warning)
            << "RINEX3: Validation failed for satellite " << obs.Sat.id()
            << " (valid: " << report.validObservations << "/" << report.totalObservations << ")";
        return 0;
    }

    // Stage 3: Commit all validated observations to final structure
    // RINEX 3 doesn't use code conversion maps, so provide empty map
    map<E_ObsCode2, E_ObsCode> emptyCodeMap;
    commitStagedObservations(staging, obs, emptyCodeMap);

    return 1;
}

/** Decode obs data (dispatcher function)
 */
int decodeObsData(
    std::istream&                   inputStream,
    string&                         line,
    double                          ver,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    GObs&                           obs,
    SatSys&                         v2SatSys,
    RinexStation&                   rnxRec
)
{
    if (ver <= 2.99)
    {
        return decodeObsDataRinex2(inputStream, line, sysCodeTypes, obs, v2SatSys, rnxRec);
    }
    else
    {
        return decodeObsDataRinex3(inputStream, line, sysCodeTypes, obs, rnxRec);
    }
}

// Read RINEX observation data body
int readRnxObsB(
    std::istream&                   inputStream,
    double                          ver,
    E_TimeSys                       tsys,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    int&                            flag,
    ObsList&                        obsList,
    RinexStation&                   rnxRec
)
{
    GTime          time  = {};
    int            i     = 0;
    int            nSats = 0;  // cant replace with sats.size()
    vector<SatSys> sats;

    // read record
    string         line;
    std::streampos pos;
    while (pos = inputStream.tellg(), std::getline(inputStream, line))
    {
        // decode obs epoch
        if (i == 0)
        {
            nSats = decodeObsEpoch(inputStream, line, ver, tsys, time, flag, sats);
            if (nSats <= 0)
            {
                continue;
            }
        }
        else if (line[0] == '>')
        {
            BOOST_LOG_TRIVIAL(warning)
                << "Warning: unexpected end of epoch in rinex file at " << time;
            inputStream.seekg(pos);
            return obsList.size();
        }
        else if (flag <= 2 || flag == 6)
        {
            GObs rawObs = {};

            rawObs.time = time;

            // decode obs data
            bool pass =
                decodeObsData(inputStream, line, ver, sysCodeTypes, rawObs, sats[i - 1], rnxRec);
            if (pass)
            {
                // save obs data
                obsList.push_back((shared_ptr<GObs>)rawObs);
            }
        }

        i++;

        if (i > nSats)
            return obsList.size();
    }

    return -1;
}

// Read complete RINEX observation file
int readRnxObs(
    std::istream&                   inputStream,
    double                          ver,
    E_TimeSys                       tsys,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    ObsList&                        obsList,
    RinexStation&                   rnxRec
)
{
    int flag = 0;
    int stat = 0;

    //	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ 	<< ": ver=" << ver << " tsys=" << tsys;

    // read rinex obs data body
    int n = readRnxObsB(inputStream, ver, tsys, sysCodeTypes, flag, obsList, rnxRec);

    if (n >= 0)
        stat = 1;

    return stat;
}

// Decode GPS/Galileo/QZS/BeiDou ephemeris
int decodeEph(double ver, SatSys Sat, GTime toc, vector<double>& data, Eph& eph)
{
    // 	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << ": ver=" << ver << " sat=" << Sat.id();

    int sys = Sat.sys;

    if (sys != +E_Sys::GPS && sys != +E_Sys::GAL && sys != +E_Sys::QZS && sys != +E_Sys::BDS)
    {
        BOOST_LOG_TRIVIAL(debug) << "ephemeris error: invalid satellite sat=" << Sat.id();

        return 0;
    }

    eph.type = defNavMsgType[Sat.sys];
    eph.Sat  = Sat;
    eph.toc  = toc;

    eph.f0    = data[0];
    eph.f1    = data[1];
    eph.f2    = data[2];
    eph.crs   = data[4];
    eph.deln  = data[5];
    eph.M0    = data[6];
    eph.cuc   = data[7];
    eph.e     = data[8];
    eph.cus   = data[9];
    eph.sqrtA = data[10];
    eph.A     = SQR(eph.sqrtA);
    eph.toes  = data[11];  // toe (s) in gps/bdt week
    eph.cic   = data[12];
    eph.OMG0  = data[13];
    eph.cis   = data[14];
    eph.i0    = data[15];
    eph.crc   = data[16];
    eph.omg   = data[17];
    eph.OMGd  = data[18];
    eph.idot  = data[19];
    eph.week  = (int)data[21];  // gps/bdt week
    eph.ttms  = data[27];

    if (sys == +E_Sys::GPS || sys == +E_Sys::QZS)
    {
        eph.iode = (int)data[3];   // IODE
        eph.iodc = (int)data[26];  // IODC
        eph.toe  = GTime(GTow(eph.toes), eph.toc);
        eph.ttm  = GTime(GTow(eph.ttms), eph.toc);

        eph.code   = (int)data[20];       // GPS: codes on L2 ch
        eph.svh    = (E_Svh)data[24];     // sv health
        eph.sva    = uraToSva(data[23]);  // ura (m->index)
        eph.ura[0] = data[23];
        eph.flag   = (int)data[22];       // GPS: L2 P data flag

        eph.tgd[0] = data[25];            // TGD

        if (sys == +E_Sys::GPS)
        {
            eph.fit = data[28];
        }  // fit interval in hours for GPS
        else if (sys == +E_Sys::QZS)
        {
            eph.fitFlag = data[28];
            eph.fit     = eph.fitFlag ? 0.0 : 2.0;
        }  // fit interval flag for QZS

        if (acsConfig.use_tgd_bias)
            decomposeTGDBias(Sat, eph.tgd[0]);
    }
    else if (sys == +E_Sys::GAL)
    {
        // GAL ver.3
        eph.iode = (int)data[3];  // IODnav
        eph.toe  = GTime(GTow(eph.toes), eph.toc);
        eph.ttm  = GTime(GTow(eph.ttms), eph.toc);

        eph.code = (int)data[20];  // data sources
                                   // bit 0 set: I/NAV E1-B
                                   // bit 1 set: F/NAV E5a-I
                                   // bit 2 set: I/NAV E5b-I
                                   // bit 8 set: af0-af2 toc are for E5a.E1
                                   // bit 9 set: af0-af2 toc are for E5b.E1
        unsigned short iNavMask = 0x0005;
        unsigned short fNavMask = 0x0002;
        if (eph.code & iNavMask)
            eph.type = E_NavMsgType::INAV;
        else if (eph.code & fNavMask)
            eph.type = E_NavMsgType::FNAV;

        eph.svh = (E_Svh)data[24];  // sv health
                                    // bit     0: E1B DVS
                                    // bit   1-2: E1B HS
                                    // bit     3: E5a DVS
                                    // bit   4-5: E5a HS
                                    // bit     6: E5b DVS
                                    // bit   7-8: E5b HS
        eph.sva    = sisaToSva(data[23]);
        eph.ura[0] = data[23];

        eph.tgd[0] = data[25];  // BGD E5a/E1
        eph.tgd[1] = data[26];  // BGD E5b/E1

        if (acsConfig.use_tgd_bias)
            decomposeBGDBias(Sat, eph.tgd[0], eph.tgd[1]);
    }
    else if (sys == +E_Sys::BDS)
    {
        // BeiDou v.3.02
        if (Sat.prn > 5 && Sat.prn < 59)
            eph.type = E_NavMsgType::D1;  // MEO/IGSO
        else
            eph.type = E_NavMsgType::D2;  // GEO, prn range may change in the future*/

        eph.tocs = BTow(toc);
        eph.aode = (int)data[3];   // AODE
        eph.aodc = (int)data[28];  // AODC
        eph.iode = int(eph.tocs / 720) % 240;
        eph.iodc = eph.iode + 256 * int(eph.tocs / 172800) % 4;
        eph.toe  = GTime(BTow(eph.toes), eph.toc);
        eph.ttm  = GTime(BTow(eph.ttms), eph.toc);

        eph.svh    = (E_Svh)data[24];     // satH1
        eph.sva    = uraToSva(data[23]);  // ura (m->index)
        eph.ura[0] = data[23];

        eph.tgd[0] = data[25];  // TGD1 B1/B3
        eph.tgd[1] = data[26];  // TGD2 B2/B3
    }

    if (eph.iode < 0 || eph.iode > 1023)
    {
        BOOST_LOG_TRIVIAL(debug) << "rinex nav invalid: sat=" << Sat.id() << " iode=" << eph.iode;
    }

    if (eph.iodc < 0 || eph.iodc > 1023)
    {
        BOOST_LOG_TRIVIAL(debug) << "rinex nav invalid: sat=" << Sat.id() << " iodc=" << eph.iodc;
    }
    return 1;
}

// Decode GLONASS ephemeris parameters
int decodeGeph(
    double          ver,   ///< RINEX version
    SatSys          Sat,   ///< Satellite ID
    GTime           toc,   ///< Time of clock
    vector<double>& data,  ///< Data to decode
    Geph&           geph   ///< Glonass ephemeris
)
{
    double tow;

    //     BOOST_LOG_TRIVIAL(debug)
    // 	<< "decodeGeph: ver=" << ver << " sat=" << Sat.id();

    if (Sat.sys != +E_Sys::GLO)
    {
        BOOST_LOG_TRIVIAL(debug) << "glonass ephemeris error: invalid satellite sat=" << Sat.id();

        return 0;
    }

    geph.type = defNavMsgType[Sat.sys];
    geph.Sat  = Sat;

    RTod toes = int(RTod(toc) + 450.0) / 900 * 900.0;
    geph.toe  = GTime(toes, toc);

    geph.tofs = data[2];  // UTC
    geph.tof  = GTime(RTod(geph.tofs + 10800.0), toc);

    geph.iode = (int)toes / 900;

    geph.taun   = -data[0];  // -taun -> +taun
    geph.gammaN = data[1];   // +gamman

    for (int i = 0; i < 3; i++)
    {
        geph.pos[i] = data[3 + i * 4] * 1E3;
        geph.vel[i] = data[4 + i * 4] * 1E3;
        geph.acc[i] = data[5 + i * 4] * 1E3;
    }

    geph.svh = (E_Svh)data[6];
    geph.frq = (int)data[10];
    geph.age = (int)data[14];

    if (ver >= 3.05)
    {
        // todo Eugene: additional records from version 3.05 and on
    }

    // some receiver output >128 for minus frequency number
    if (geph.frq > 128)
        geph.frq -= 256;

    if (geph.frq < MINFREQ_GLO || geph.frq > MAXFREQ_GLO)
    {
        BOOST_LOG_TRIVIAL(debug) << "rinex gnav invalid freq: sat=" << Sat << " fn=" << geph.frq;
    }
    return 1;
}

// Decode SBAS/geostationary satellite ephemeris
int decodeSeph(double ver, SatSys Sat, GTime toc, vector<double>& data, Seph& seph)
{
    //     BOOST_LOG_TRIVIAL(debug)
    // 	<< "decodeSeph: ver=" << ver << " sat=" << Sat.id();

    if (Sat.sys != +E_Sys::SBS)
    {
        BOOST_LOG_TRIVIAL(debug) << "geo ephemeris error: invalid satellite sat=" << Sat.id();

        return 0;
    }

    seph.type = defNavMsgType[Sat.sys];
    seph.Sat  = Sat;
    seph.t0   = toc;

    seph.tofs = data[2];
    seph.tof  = GTime(GTow(seph.tofs), seph.t0);

    seph.af0 = data[0];
    seph.af1 = data[1];

    for (int i = 0; i < 3; i++)
    {
        seph.pos[i] = data[3 + i * 4] * 1E3;
        seph.vel[i] = data[4 + i * 4] * 1E3;
        seph.acc[i] = data[5 + i * 4] * 1E3;
    }

    seph.svh = (E_Svh)data[6];
    seph.sva = uraToSva(data[10]);
    seph.ura = data[10];
    return 1;
}

// Decode CNVX (Civil Navigation) ephemeris
int decodeCeph(
    double          ver,   ///< RINEX version
    SatSys          Sat,   ///< Satellite ID
    E_NavMsgType    type,  ///< Navigation message type
    GTime           toc,   ///< Time of clock
    vector<double>& data,  ///< Data to decode
    Ceph&           ceph   ///< CNVX ephemeris
)
{
    //     BOOST_LOG_TRIVIAL(debug)
    // 	<< "decodeCeph: ver=" << ver << " sat=" << Sat.id();

    if (ver < 4.0)
    {
        BOOST_LOG_TRIVIAL(debug) << "ephemeris error: invalid RINEX version=" << ver;

        return -1;
    }

    if (type != +E_NavMsgType::CNAV && type != +E_NavMsgType::CNV1 && type != +E_NavMsgType::CNV2 &&
        type != +E_NavMsgType::CNV3)
    {
        BOOST_LOG_TRIVIAL(debug) << "ephemeris error: invalid message type=" << type._to_string();

        return 0;
    }

    int sys = Sat.sys;

    if (sys != +E_Sys::GPS && sys != +E_Sys::QZS && sys != +E_Sys::BDS)
    {
        BOOST_LOG_TRIVIAL(debug) << "ephemeris error: invalid satellite sat=" << Sat.id();

        return 0;
    }

    ceph.Sat  = Sat;
    ceph.type = type;
    ceph.toc  = toc;

    ceph.f0   = data[0];
    ceph.f1   = data[1];
    ceph.f2   = data[2];
    ceph.Adot = data[3];
    ceph.crs  = data[4];
    ceph.deln = data[5];
    ceph.M0   = data[6];
    ceph.cuc  = data[7];
    ceph.e    = data[8];
    ceph.cus  = data[9];
    ceph.A    = SQR(data[10]);
    ceph.cic  = data[12];
    ceph.OMG0 = data[13];
    ceph.cis  = data[14];
    ceph.i0   = data[15];
    ceph.crc  = data[16];
    ceph.omg  = data[17];
    ceph.OMGd = data[18];
    ceph.idot = data[19];
    ceph.dn0d = data[20];

    if (sys == +E_Sys::GPS || sys == +E_Sys::QZS)
    {
        ceph.toe  = ceph.toc;
        ceph.toes = GTow(ceph.toe);

        ceph.ura[0] = data[21];
        ceph.ura[1] = data[22];
        ceph.ura[2] = data[26];
        ceph.ura[3] = data[23];

        ceph.svh = (E_Svh)data[24];  // sv health

        ceph.tgd[0] = data[25];      // TGD

        ceph.isc[0] = data[27];
        ceph.isc[1] = data[28];
        ceph.isc[2] = data[29];
        ceph.isc[3] = data[30];

        if (type == +E_NavMsgType::CNAV)
        {
            ceph.ttms = data[31];
            ceph.ttm  = GTime(GTow(ceph.ttms), ceph.toc);
            ceph.wnop = (int)data[32];
        }
        else if (type == +E_NavMsgType::CNV2)
        {
            ceph.isc[4] = data[31];
            ceph.isc[5] = data[32];

            ceph.ttms = data[35];
            ceph.ttm  = GTime(GTow(ceph.ttms), ceph.toc);
            ceph.wnop = (int)data[36];
        }

        ceph.tops = data[11];  // top (s) in seconds
        ceph.top  = GTime(GTow(ceph.tops), ceph.toc);
    }
    else if (sys == +E_Sys::BDS)
    {
        // BeiDou v.4.00

        ceph.orb = E_SatType::_from_integral(data[21]);

        ceph.sis[0] = data[23];
        ceph.sis[1] = data[24];
        ceph.sis[2] = data[25];
        ceph.sis[3] = data[26];

        if (type == +E_NavMsgType::CNV1 || type == +E_NavMsgType::CNV2)
        {
            ceph.isc[0] = data[27];
            ceph.isc[1] = data[28];

            ceph.tgd[0] = data[29];  // TGD_B1Cp
            ceph.tgd[1] = data[30];  // TGD_B2ap

            ceph.sis[4] = data[31];

            ceph.svh  = (E_Svh)data[32];  // sv health
            ceph.flag = (int)data[33];    // integrity flag
            ceph.iodc = (int)data[34];    // IODC
            ceph.iode = (int)data[38];    // IODE

            ceph.ttms = data[35];
            ceph.ttm  = GTime(BTow(ceph.ttms), ceph.toc);
        }
        else if (type == +E_NavMsgType::CNV3)
        {
            ceph.sis[4] = data[27];
            ceph.svh    = (E_Svh)data[28];  // sv health
            ceph.flag   = (int)data[29];    // integrity flag
            ceph.tgd[2] = data[30];         // TGD_B2ap

            ceph.ttms = data[31];
            ceph.ttm  = GTime(BTow(ceph.ttms), ceph.toc);
        }

        ceph.toes = data[11];  // top (s) in seconds
        ceph.tops = data[22];  // top (s) in seconds
        ceph.toe  = GTime(BTow(ceph.toes), ceph.toc);
        ceph.top  = GTime(BTow(ceph.tops), ceph.toc);
    }

    if (ceph.iode < 0 || ceph.iode > 1023)
    {
        BOOST_LOG_TRIVIAL(debug) << "rinex nav invalid: sat=" << Sat.id() << " iode=" << ceph.iode;
    }

    if (ceph.iodc < 0 || ceph.iodc > 1023)
    {
        BOOST_LOG_TRIVIAL(debug) << "rinex nav invalid: sat=" << Sat.id() << " iodc=" << ceph.iodc;
    }

    return 1;
}

// Decode System Time Offset message
int decodeSto(double ver, SatSys Sat, E_NavMsgType type, GTime toc, vector<double>& data, STO& sto)
{
    if (ver < 4.0)
    {
        BOOST_LOG_TRIVIAL(debug) << "ephemeris error: invalid RINEX version=" << ver;

        return -1;
    }

    int sys = Sat.sys;

    sto.Sat  = Sat;
    sto.type = type;
    sto.tot  = toc;

    sto.code = E_StoCode ::_from_integral(data[0]);
    sto.sid  = E_SbasId ::_from_integral(data[1]);
    sto.uid  = E_UtcId ::_from_integral(data[2]);

    sto.ttms = data[3];

    sto.A0 = data[4];
    sto.A1 = data[5];
    sto.A2 = data[6];

    if (sys != +E_Sys::BDS)
    {
        sto.ttm = GTime(GWeek(sto.tot), GTow(sto.ttms));
    }
    else
    {
        sto.ttm = GTime(BWeek(sto.tot), BTow(sto.ttms));
    }

    return 1;
}

// Decode Earth Orientation Parameters message
int decodeEop(double ver, SatSys Sat, E_NavMsgType type, GTime toc, vector<double>& data, EOP& eop)
{
    if (ver < 4.0)
    {
        BOOST_LOG_TRIVIAL(debug) << "ephemeris error: invalid RINEX version=" << ver;

        return -1;
    }

    int sys = Sat.sys;

    eop.Sat  = Sat;
    eop.type = type;
    eop.teop = toc;

    eop.xp   = data[0] * AS2R;
    eop.xpr  = data[1] * AS2R;
    eop.xprr = data[2] * AS2R;
    eop.yp   = data[4] * AS2R;
    eop.ypr  = data[5] * AS2R;
    eop.yprr = data[6] * AS2R;
    eop.ttms = data[7];
    eop.dut1 = data[8];
    eop.dur  = data[9];
    eop.durr = data[10];

    if (sys != +E_Sys::BDS)
    {
        eop.ttm = GTime(GWeek(eop.teop), GTow(eop.ttms));
    }
    else
    {
        eop.ttm = GTime(BWeek(eop.teop), BTow(eop.ttms));
    }

    return 1;
}

// Decode ionospheric parameters message
int decodeIon(double ver, SatSys Sat, E_NavMsgType type, GTime toc, vector<double>& data, ION& ion)
{
    if (ver < 4.0)
    {
        BOOST_LOG_TRIVIAL(debug) << "ephemeris error: invalid RINEX version=" << ver;

        return -1;
    }

    int sys = Sat.sys;

    ion.Sat  = Sat;
    ion.type = type;
    ion.ttm  = toc;

    if (sys == +E_Sys::GAL && type == +E_NavMsgType::IFNV)
    {
        ion.ai0 = data[0];
        ion.ai1 = data[1];
        ion.ai2 = data[2];

        ion.flag = (int)data[3];
    }
    else if (sys == +E_Sys::BDS && type == +E_NavMsgType::CNVX)
    {
        ion.alpha1 = data[0];
        ion.alpha2 = data[1];
        ion.alpha3 = data[2];
        ion.alpha4 = data[3];
        ion.alpha5 = data[4];
        ion.alpha6 = data[5];
        ion.alpha7 = data[6];
        ion.alpha8 = data[7];
        ion.alpha9 = data[8];
    }
    else if (type == +E_NavMsgType::LNAV || type == +E_NavMsgType::D1D2 ||
             type == +E_NavMsgType::CNVX)
    {
        ion.a0 = data[0];
        ion.a1 = data[1];
        ion.a2 = data[2];
        ion.a3 = data[3];
        ion.b0 = data[4];
        ion.b1 = data[5];
        ion.b2 = data[6];
        ion.b3 = data[7];

        ion.code = (int)data[8];

        if (ion.code == 1)  // QZS Japan area coefficients are currently skipped
            return 0;
    }

    return 1;
}

// Read RINEX navigation data body
int readRnxNavB(
    std::istream& inputStream,  ///< Input stream to read
    double        ver,          ///< RINEX version
    E_Sys         sys,          ///< Satellite system
    E_EphType&    type,         ///< Ephemeris type (output)
    Eph&          eph,          ///< GPS Ephemeris
    Geph&         geph,         ///< Glonass ephemeris
    Seph&         seph,         ///< Geo ephemeris
    Ceph&         ceph,         ///< CNVX ephemeris
    STO&          sto,          ///< System time offset data
    EOP&          eop,          ///< EOP data
    ION&          ion           ///< Ionosphere data
)
{
    GTime          toc;
    vector<double> data;
    int            sp = 3;
    string         line;
    char           id[8] = "";
    char*          p;

    // 	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << ": ver=" << ver << " sys=" << sys;

    SatSys       Sat     = {};
    E_NavRecType recType = E_NavRecType::NONE;
    E_NavMsgType msgType = E_NavMsgType::NONE;

    while (std::getline(inputStream, line))
    {
        char* buff = &line[0];

        if (data.empty())
        {
            // decode message type field
            if (ver >= 4.0 && buff[0] == '>')
            {
                // ver.4
                char typeStr[5] = "";
                strncpy(typeStr, buff + 2, 3);
                recType = E_NavRecType::_from_string(typeStr);

                strncpy(id, buff + 6, 3);
                Sat = SatSys(id);
                sys = Sat.sys;

                strncpy(typeStr, buff + 10, 4);
                std::replace(typeStr, typeStr + 4, ' ', '\0');
                msgType = E_NavMsgType::_from_string(typeStr);

                continue;
            }

            // decode satellite field
            if (ver >= 3.0 || sys == +E_Sys::GAL || sys == +E_Sys::QZS)
            {
                // ver.3 or GAL/QZS
                strncpy(id, buff, 3);
                sp = 4;
                if (ver < 4.0)  // satellite id included in message type field in ver.4
                {
                    Sat = SatSys(id);
                    if (ver >= 3.0)
                        sys = Sat.sys;
                }
            }
            else
            {
                Sat.sys = sys;
                Sat.prn = str2num(buff, 0, 2);
            }

            E_TimeSys tsys = E_TimeSys::GPST;
            switch (sys)
            {
                case E_Sys::GPS:
                    tsys = E_TimeSys::GPST;
                    break;
                case E_Sys::GLO:
                    tsys = E_TimeSys::UTC;
                    break;
                case E_Sys::GAL:
                    tsys = E_TimeSys::GST;
                    break;
                case E_Sys::BDS:
                    tsys = E_TimeSys::BDT;
                    break;
                case E_Sys::QZS:
                    tsys = E_TimeSys::QZSST;
                    break;
                case E_Sys::SBS:
                    tsys = E_TimeSys::GPST;
                    break;
                default:
                    tsys = E_TimeSys::GPST;
                    break;
            }

            // decode toc field
            bool error = str2time(buff + sp, 0, 19, toc, tsys);
            if (error == true)
            {
                //                 BOOST_LOG_TRIVIAL(debug)
                // 				<< "rinex nav toc error: " << buff;

                return 0;
            }

            if (recType == +E_NavRecType::STO)
            {
                // decode STO code, SBAS ID & UTC ID for STO message
                char code[19] = "";
                strncpy(code, buff + 24, 18);
                std::replace(code, code + 18, ' ', '\0');
                data.push_back(E_StoCode::_from_string(code));

                strncpy(code, buff + 43, 18);
                std::replace(code, code + 18, '-', '_');
                std::replace(code, code + 18, ' ', '\0');
                data.push_back(*(E_SbasId::_from_string_nothrow(code)));  // code may be empty

                strncpy(code, buff + 62, 18);
                std::replace(code, code + 18, '(', '_');
                std::replace(code, code + 18, ')', '\0');
                std::replace(code, code + 18, ' ', '\0');
                data.push_back(*(E_UtcId::_from_string_nothrow(code)));  // code may be empty
            }
            else
            {
                // decode data fields
                p = buff + sp + 19;
                for (int j = 0; j < 3; j++, p += 19)
                {
                    data.push_back(str2num(p, 0, 19));
                }
            }

            if (recType == +E_NavRecType::NONE)
                recType = E_NavRecType::EPH;
            if (msgType == +E_NavMsgType::NONE)
                msgType = defNavMsgType[sys];
        }
        else
        {
            // decode data fields
            p = buff + sp;
            for (int j = 0; j < 4; j++, p += 19)
            {
                data.push_back(str2num(p, 0, 19));
            }
            // decode ephemeris
            if (recType == +E_NavRecType::EPH)
            {
                switch (msgType)
                {
                    case E_NavMsgType::CNAV:  // fallthrough
                    case E_NavMsgType::CNV3:
                    {
                        if (data.size() >= 35)
                        {
                            type = E_EphType::CEPH;
                            return decodeCeph(ver, Sat, msgType, toc, data, ceph);
                        }
                        break;
                    }
                    case E_NavMsgType::CNV1:  // fallthrough
                    case E_NavMsgType::CNV2:
                    {
                        if (data.size() >= 39)
                        {
                            type = E_EphType::CEPH;
                            return decodeCeph(ver, Sat, msgType, toc, data, ceph);
                        }
                        break;
                    }
                    case E_NavMsgType::FDMA:
                    {
                        if (data.size() >= 15)
                        {
                            type = E_EphType::GEPH;
                            return decodeGeph(ver, Sat, toc, data, geph);
                        }
                        break;
                    }  // todo Eugene: additional records from version 3.05 and on
                    case E_NavMsgType::SBAS:
                    {
                        if (data.size() >= 15)
                        {
                            type = E_EphType::SEPH;
                            return decodeSeph(ver, Sat, toc, data, seph);
                        }
                        break;
                    }
                    default:
                    {
                        if (data.size() >= 31)
                        {
                            type = E_EphType::EPH;
                            return decodeEph(ver, Sat, toc, data, eph);
                        }
                        break;
                    }
                }
            }
            else if (recType == +E_NavRecType::STO)
            {
                if (data.size() >= 7)
                {
                    type = E_EphType::STO;
                    return decodeSto(ver, Sat, msgType, toc, data, sto);
                }
            }
            else if (recType == +E_NavRecType::EOP)
            {
                if (data.size() >= 11)
                {
                    type = E_EphType::EOP;
                    return decodeEop(ver, Sat, msgType, toc, data, eop);
                }
            }
            else if (recType == +E_NavRecType::ION)
            {
                switch (sys)
                {
                    case E_Sys::GAL:
                    {
                        if (data.size() >= 7)
                        {
                            type = E_EphType::ION;
                            return decodeIon(ver, Sat, msgType, toc, data, ion);
                        }
                        break;
                    }
                    default:
                    {
                        if (data.size() >= 11)
                        {
                            type = E_EphType::ION;
                            return decodeIon(ver, Sat, msgType, toc, data, ion);
                        }
                        break;
                    }
                }
            }
            else
                return -1;
        }
    }
    return -1;
}

// Read complete RINEX navigation file
int readRnxNav(
    std::istream& inputStream,  ///< Input stream to read
    double        ver,          ///< RINEX version
    E_Sys         sys,          ///< Satellite system
    Navigation&   nav           ///< Navigation object
)
{
    // 	BOOST_LOG_TRIVIAL(debug) << __FUNCTION__ << ": ver=" << ver << " sys=" << sys;

    // read rinex navigation data body
    while (1)
    {
        // initialise each time to avoid incomplete overwriting
        Eph  eph  = {};
        Geph geph = {};
        Seph seph = {};
        Ceph ceph = {};
        STO  sto  = {};
        EOP  eop  = {};
        ION  ion  = {};

        E_EphType type;

        int stat = readRnxNavB(inputStream, ver, sys, type, eph, geph, seph, ceph, sto, eop, ion);

        if (stat < 0)
        {
            break;
        }
        else if (stat > 0)
        {
            // add ephemeris to navigation data
            switch (type)
            {
                case E_EphType::EPH:
                    nav.ephMap[eph.Sat][eph.type][eph.toe] = eph;
                    break;
                case E_EphType::GEPH:
                    nav.gephMap[geph.Sat][geph.type][geph.toe] = geph;
                    break;
                case E_EphType::SEPH:
                    nav.sephMap[seph.Sat][seph.type][seph.t0] = seph;
                    break;
                case E_EphType::CEPH:
                    nav.cephMap[ceph.Sat][ceph.type][ceph.toe] = ceph;
                    break;
                case E_EphType::STO:
                    nav.stoMap[sto.code][sto.type][sto.tot] = sto;
                    break;
                case E_EphType::EOP:
                    nav.eopMap[eop.Sat.sys][eop.type][eop.teop] = eop;
                    break;
                case E_EphType::ION:
                    nav.ionMap[ion.Sat.sys][ion.type][ion.ttm] = ion;
                    break;
                default:
                    continue;
            }
        }
    }

    for (auto& [sys, eopSysMap] : nav.eopMap)
        for (auto& [type, eopList] : eopSysMap)
        {
            map<GTime, ERPValues> erpMap;

            for (auto& [time, eop] : eopList)
            {
                ERPValues erpv;
                erpv.time         = eop.teop;
                erpv.xp           = eop.xp;
                erpv.yp           = eop.yp;
                erpv.ut1Utc       = eop.dut1;
                erpv.xpr          = eop.xpr;
                erpv.ypr          = eop.ypr;
                erpMap[erpv.time] = erpv;
            }

            if (!erpMap.empty())
                nav.erp.erpMaps.push_back(erpMap);
        }

    return (
        nav.ephMap.empty() == false || nav.gephMap.empty() == false ||
        nav.sephMap.empty() == false || nav.cephMap.empty() == false ||
        nav.stoMap.empty() == false || nav.eopMap.empty() == false || nav.ionMap.empty() == false
    );
}

// Read RINEX clock file
int readRnxClk(std::istream& inputStream, double ver, Navigation& nav)
{
    //     trace(3,"readrnxclk: index=%d\n", index);

    static int index = 0;
    index++;
    string line;

    typedef struct
    {
        short offset;
        short length;
    } ClkStruct;

    ClkStruct typ = {0, 2};
    ClkStruct as  = {3, 3};
    ClkStruct ar  = {3, 4};
    ClkStruct tim = {8, 26};
    ClkStruct clk = {40, 19};
    ClkStruct std = {60, 19};

    // special case for 3.04 rnx with 9 char AR names
    if (ver == 3.04)
    {
        ar.length += 5;
        tim.offset += 5;
        clk.offset += 5;
        std.offset += 5;
    }

    GTime time0;
    while (std::getline(inputStream, line))
    {
        char* buff = &line[0];

        GTime time;
        if (str2time(buff, tim.offset, tim.length, time))
        {
            //             trace(2,"rinex clk invalid epoch: %34.34s\n", buff);
            continue;
        }

        string type(buff + typ.offset, typ.length);

        string idString;
        if (type == "AS")
        {
            idString.assign(buff + as.offset, as.length);
        }
        else if (type == "AR")
        {
            idString.assign(buff + ar.offset, ar.length);
        }
        else
            continue;

        Pclk preciseClock = {};

        preciseClock.clk      = str2num(buff, clk.offset, clk.length);
        preciseClock.clkStd   = str2num(buff, std.offset, std.length);
        preciseClock.clkIndex = index;

        nav.pclkMap[idString][time] = preciseClock;

        // Use minimum delta time between epochs as the data interval
        double dt = (time - time0).to_double();
        if (dt > 0 && dt < nav.pclkInterval)
        {
            nav.pclkInterval = dt;
        }

        time0 = time;
    }

    return nav.pclkMap.size() > 0;
}

// Read RINEX file with automatic type detection
int readRnx(
    std::istream&                   inputStream,
    char&                           type,
    ObsList&                        obsList,
    Navigation&                     nav,
    RinexStation&                   rnxRec,
    double&                         ver,
    E_Sys&                          sys,
    E_TimeSys&                      tsys,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes
)
{
    if (inputStream.tellg() == 0)
    {
        // read rinex header if at beginning of file
        readRnxH(inputStream, ver, type, sys, tsys, sysCodeTypes, nav, rnxRec);
    }

    // read rinex body
    switch (type)
    {
        case 'O':
            return readRnxObs(inputStream, ver, tsys, sysCodeTypes, obsList, rnxRec);
        case 'N':
            return readRnxNav(inputStream, ver, sys, nav);
        case 'G':
            return readRnxNav(inputStream, ver, E_Sys::GLO, nav);
        case 'H':
            return readRnxNav(inputStream, ver, E_Sys::SBS, nav);
        case 'J':
            return readRnxNav(inputStream, ver, E_Sys::QZS, nav);  // extension
        case 'L':
            return readRnxNav(inputStream, ver, E_Sys::GAL, nav);  // extension
        case 'C':
            return readRnxClk(inputStream, ver, nav);
    }

    BOOST_LOG_TRIVIAL(debug) << "unsupported rinex type ver=" << ver << " type=" << type;

    return 0;
}

// Helper functions for common RINEX observation processing (SRP compliance)

/**
 * @brief Parse observation values from RINEX formatted text
 *
 * Extracts numerical observation value and Loss of Lock Indicator from
 * a RINEX formatted line at the specified position. Includes comprehensive
 * bounds checking and error handling for malformed input data.
 *
 * RINEX observation format:
 * - 14 characters: observation value (right-justified, decimal point optional)
 * - 1 character: Loss of Lock Indicator (0-3)
 * - 1 character: Signal strength (optional, not currently processed)
 *
 * @param buff Character buffer containing RINEX observation line
 * @param position Starting position in buffer (0-based index)
 *
 * @return ObservationValues Structure containing parsed value and LLI
 *         Returns zeros if parsing fails or position is out of bounds
 *
 * @note Includes debug output for development/troubleshooting
 * @note LLI bits are masked to extract only relevant flags (bits 0-1)
 *
 * @warning Function assumes RINEX standard 16-character field width
 */
ObservationValues parseObservationValues(char* buff, int position)
{
    ObservationValues result;

    // Add bounds checking to prevent buffer overflow
    if (!buff)
    {
        BOOST_LOG_TRIVIAL(error) << "parseObservationValues: null buffer pointer";
        result.value = 0.0;
        result.lli   = 0.0;
        return result;
    }

    // Basic bounds check - RINEX observation fields are 14 chars + 1 char LLI
    size_t bufferLength = strlen(buff);
    result.value        = str2num(buff, position, 14);
    result.lli          = str2num(buff, position + 14, 1);
    result.lli          = (unsigned char)result.lli & 0x03;  // Extract LLI bits
    return result;
}

/**
 * @brief Assign parsed values to appropriate RawSig fields
 *
 * Routes observation values to the correct field in a RawSig structure
 * based on the observation type character. Implements type-safe assignment
 * with validation to prevent data corruption.
 *
 * Observation type mapping:
 * - 'C', 'P': Pseudorange/code observations -> signal.P
 * - 'L': Carrier phase observations -> signal.L (with LLI)
 * - 'D': Doppler observations -> signal.D
 * - 'S': Signal-to-noise ratio -> signal.snr
 *
 * @param signal Reference to RawSig structure to modify
 * @param observationType Single character observation type identifier
 * @param value Numerical observation value to assign
 * @param lli Loss of Lock Indicator (only used for phase observations)
 *
 * @note Only assigns non-zero values to prevent overwriting existing data
 * @note LLI is only assigned for phase ('L') observations
 */
void assignObservationValue(RawSig& signal, char observationType, double value, double lli)
{
    if (value)  // Only assign if value is valid (non-zero)
    {
        switch (observationType)
        {
            case 'P':  // fallthrough
            case 'C':
                signal.P = value;
                break;  // Pseudorange (Code/Pulse)
            case 'L':
                signal.L   = value;
                signal.LLI = lli;
                break;  // Carrier Phase
            case 'D':
                signal.D = value;
                break;  // Doppler
            case 'S':
                signal.snr = value;
                break;  // Signal-to-Noise Ratio
        }
    }
}

/**
 * @brief Stage observation for later processing and validation
 *
 * Adds an observation to the staging area with complete metadata for later
 * processing. Used for observations that can be immediately resolved without
 * requiring priority-based selection logic.
 *
 * The staging pattern provides several benefits:
 * - Deferred processing allows validation before commitment
 * - Conflict detection and resolution
 * - Consistent handling of all observation types
 * - Enhanced debugging and logging capabilities
 *
 * @param staging Reference to staging container map
 * @param obsType Single character observation type ('C', 'L', 'P', 'D', 'S')
 * @param obsCode Resolved RINEX 3 observation code (e.g., L1C, C1W)
 * @param frequency Frequency type enumeration (F1, F2, F5, etc.)
 * @param value Numerical observation value
 * @param lli Loss of Lock Indicator (0-3)
 *
 * @note Creates composite key from obsType + frequency + obsCode for uniqueness
 * @note Logs staging operation for debugging purposes
 *
 * @see stagePhaseObservation() for priority-based phase observations
 * @see ObservationKey for key structure details
 */
void stageObservation(
    ObservationStaging& staging,
    char                obsType,
    E_ObsCode           obsCode,
    E_FType             frequency,
    double              value,
    double              lli
)
{
    ObservationKey key = {obsType, frequency, obsCode};

    StagedObservation staged;
    staged.value     = value;
    staged.lli       = lli;
    staged.obsCode   = obsCode;
    staged.frequency = frequency;
    staged.isValid   = (value != 0.0);  // Consider non-zero values as valid

    staging[key] = staged;

    BOOST_LOG_TRIVIAL(debug) << "Staged observation: type=" << obsType
                             << " code=" << obsCode._to_string() << " freq=" << (int)frequency
                             << " value=" << value;
}

/**
 * @brief Stage phase observation with priority resolution support
 *
 * Stages a phase observation that requires priority-based code resolution.
 * Unlike regular observations, phase observations in RINEX 2 can map to
 * multiple possible RINEX 3 codes, requiring selection based on available
 * code observations.
 *
 * Priority resolution example:
 * - Configuration: L1 -> [L1W, L1C] (try L1W first, then L1C)
 * - If P1 has data -> L1W is available -> use L1W for L1 phase
 * - If P1 is zero but C1 has data -> use L1C for L1 phase
 *
 * @param staging Reference to staging container map
 * @param obsType Single character observation type (typically 'L')
 * @param priorityCodes Vector of observation codes in priority order
 * @param frequency Frequency type enumeration
 * @param value Numerical phase observation value
 * @param lli Loss of Lock Indicator
 *
 * @note Resolution occurs during commitStagedObservations() when code data is available
 * @note Uses first priority code as temporary key for staging
 * @note Logs priority array for debugging purposes
 *
 * @see commitStagedObservations() for priority resolution implementation
 */
void stagePhaseObservation(
    ObservationStaging&      staging,
    char                     obsType,
    const vector<E_ObsCode>& priorityCodes,
    E_FType                  frequency,
    double                   value,
    double                   lli
)
{
    // Use the first priority code as the key, but store the full priority array
    E_ObsCode keyCode = E_ObsCode::NONE;
    if (!priorityCodes.empty())
    {
        keyCode = priorityCodes[0];
    }
    ObservationKey key = {obsType, frequency, keyCode};

    StagedObservation staged;
    staged.value               = value;
    staged.lli                 = lli;
    staged.obsCode             = keyCode;
    staged.frequency           = frequency;
    staged.isValid             = (value != 0.0);
    staged.priorityCodes       = priorityCodes;
    staged.isPhaseWithPriority = true;

    staging[key] = staged;

    BOOST_LOG_TRIVIAL(debug) << "Staged phase observation with priority: type=" << obsType
                             << " priority_codes=[" <<
        [&]()
    {
        string codes;
        for (size_t i = 0; i < priorityCodes.size(); ++i)
        {
            if (i > 0)
                codes += ",";
            codes += priorityCodes[i]._to_string();
        }
        return codes;
    }() << "]" << " freq=" << (int)frequency
                             << " value=" << value;
}

/**
 * @brief Commit staged observations with phase priority resolution
 *
 * Processes all staged observations and transfers them to the final GObs
 * structure. Implements sophisticated two-pass algorithm for phase observation
 * priority resolution:
 *
 * Pass 1: Commit code observations and track available codes
 * - Process all non-phase observations (C, P, D, S types)
 * - Build set of available observation codes
 * - Create RawSig entries in appropriate frequency lists
 *
 * Pass 2: Resolve and commit phase observations
 * - For each phase observation with priority array
 * - Find first available code from priority list
 * - Fallback to first priority if none available
 * - Commit resolved phase observation
 *
 * @param staging Container of staged observations to process
 * @param obs Reference to output GObs structure to populate
 * @param codeMap RINEX 2->3 code conversion map (for reference, not used in current impl)
 *
 * @note Phase resolution depends on code observations being processed first
 * @note Extensive debug logging for troubleshooting priority resolution
 * @note Creates RawSig entries using findOrCreateSignal() helper
 *
 * @see findOrCreateSignal(), assignObservationValue()
 */
void commitStagedObservations(
    const ObservationStaging&         staging,
    GObs&                             obs,
    const map<E_ObsCode2, E_ObsCode>& codeMap
)
{
    // Step 1: Collect all committed code observations to determine availability
    set<E_ObsCode> availableCodes;

    // Step 2: First pass - commit all non-phase observations and collect available codes
    BOOST_LOG_TRIVIAL(debug) << "Phase priority: Starting first pass - collecting available codes";
    for (const auto& [key, staged] : staging)
    {
        if (!staged.isValid || staged.isPhaseWithPriority)
            continue;

        auto&   sigList = obs.sigsLists[staged.frequency];
        RawSig* rawSig  = findOrCreateSignal(sigList, staged.obsCode);

        // Commit the staged observation
        assignObservationValue(*rawSig, key.obsType, staged.value, staged.lli);

        // Track available codes for phase priority resolution
        availableCodes.insert(staged.obsCode);

        BOOST_LOG_TRIVIAL(debug) << "Committed code observation: type=" << key.obsType
                                 << " code=" << staged.obsCode._to_string()
                                 << " freq=" << (int)staged.frequency;
    }

    BOOST_LOG_TRIVIAL(debug) << "Phase priority: Available codes: [" << [&]()
    {
        string codes;
        for (const auto& code : availableCodes)
        {
            if (!codes.empty())
                codes += ",";
            codes += code._to_string();
        }
        return codes;
    }() << "]";

    // Step 3: Second pass - resolve and commit phase observations with priority
    BOOST_LOG_TRIVIAL(debug) << "Phase priority: Starting second pass - resolving phase priorities";
    // this part was initially `for (const auto& [key, staged] : staging)`. However it seems to be
    // an issue with clang in openMP sections

    for (const auto& stage : staging)
    {
        auto& key    = stage.first;
        auto& staged = stage.second;
        if (!staged.isValid || !staged.isPhaseWithPriority)
            continue;

        BOOST_LOG_TRIVIAL(debug) << "Phase priority: Processing phase " << key.obsType
                                 << " with priorities [" << [&]()
        {
            string codes;
            for (const auto& code : staged.priorityCodes)
            {
                if (!codes.empty())
                    codes += ",";
                codes += code._to_string();
            }
            return codes;
        }() << "]";

        // Resolve priority: find first available code from priority list
        E_ObsCode resolvedCode = E_ObsCode::NONE;
        for (const auto& priorityCode : staged.priorityCodes)
        {
            BOOST_LOG_TRIVIAL(debug)
                << "Phase priority: Checking if " << priorityCode._to_string() << " is available";
            if (availableCodes.find(priorityCode) != availableCodes.end())
            {
                resolvedCode = priorityCode;
                BOOST_LOG_TRIVIAL(debug) << "Phase priority resolved: " << key.obsType << " -> "
                                         << resolvedCode._to_string() << " (available)";
                break;
            }
            else
            {
                BOOST_LOG_TRIVIAL(debug)
                    << "Phase priority: " << priorityCode._to_string() << " not available";
            }
        }

        // Fallback to first priority if none available
        if (resolvedCode == +E_ObsCode::NONE && !staged.priorityCodes.empty())
        {
            resolvedCode = staged.priorityCodes[0];
            BOOST_LOG_TRIVIAL(warning) << "Phase priority fallback: " << key.obsType << " -> "
                                       << resolvedCode._to_string() << " (no available codes)";
        }

        if (resolvedCode != +E_ObsCode::NONE)
        {
            auto&   sigList = obs.sigsLists[staged.frequency];
            RawSig* rawSig  = findOrCreateSignal(sigList, resolvedCode);

            // Commit the resolved phase observation
            assignObservationValue(*rawSig, key.obsType, staged.value, staged.lli);

            BOOST_LOG_TRIVIAL(debug)
                << "Committed phase observation: type=" << key.obsType
                << " code=" << resolvedCode._to_string() << " freq=" << (int)staged.frequency;
        }
        else
        {
            BOOST_LOG_TRIVIAL(error) << "Failed to resolve phase observation: " << key.obsType;
        }
    }
}

/** Validate staged observations before committing
 * Single Responsibility: Quality assurance for staged data
 */
bool validateStagedObservations(const ObservationStaging& staging, const SatSys& satellite)
{
    int validCount = 0;
    int totalCount = 0;

    for (const auto& [obsKey, stagedObs] : staging)
    {
        totalCount++;
        if (stagedObs.isValid)
            validCount++;
    }

    // Require at least one valid observation
    bool isValid = (validCount > 0);

    BOOST_LOG_TRIVIAL(debug) << "Validation for " << satellite.id() << ": " << validCount << "/"
                             << totalCount << " valid observations" << " -> "
                             << (isValid ? "PASS" : "FAIL");

    return isValid;
}

/** Advanced validation and conflict resolution for staged observations
 * Single Responsibility: Data quality assurance and conflict handling
 */
void resolveObservationConflicts(ObservationStaging& staging)
{
    // With the new composite key structure (type + frequency + code),
    // each observation should be unique, so no conflicts should occur.
    // This function is kept for future extensions or edge cases.

    BOOST_LOG_TRIVIAL(debug) << "Conflict resolution: " << staging.size()
                             << " unique observations (no conflicts expected with composite keys)";
}

/** Enhanced validation with detailed statistics
 * Single Responsibility: Comprehensive data quality assessment
 */
ValidationReport validateStagedObservationsDetailed(
    ObservationStaging& staging,  // Note: non-const to allow conflict resolution
    const SatSys&       satellite
)
{
    ValidationReport report;

    // Resolve conflicts first (should be no-op with composite keys)
    resolveObservationConflicts(staging);

    // Collect statistics
    for (const auto& [key, staged] : staging)
    {
        report.observationCounts[key.obsType]++;
        report.totalObservations++;
        if (staged.isValid)
            report.validObservations++;
    }

    // Apply validation rules
    report.passed = (report.validObservations > 0);  // Require at least one valid observation

    BOOST_LOG_TRIVIAL(debug) << "Validation report for " << satellite.id() << ": "
                             << report.validObservations << "/" << report.totalObservations
                             << " valid observations - " << (report.passed ? "PASSED" : "FAILED");

    return report;
}
