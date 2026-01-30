#include "iono/ionoSBAS.hpp"
#include <map>
#include "common/common.hpp"
#include "common/constants.hpp"
#include "common/trace.hpp"
#include "sbas/sbas.hpp"

using std::map;

#define SBAS_GIV_OUTAGE 600
#define SBAS_IGP_OUTAGE 1200
#define IONO_DEBUG_TRACE_LEVEL 4

map<int, vector<int>> latiVects = {
    {0, {               -55, -50, -45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55}},
    {1, {     -75, -65, -55, -50, -45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 65, 75}},
    {2, {-85, -75, -65, -55, -50, -45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 65, 75}},
    {3, {     -75, -65, -55, -50, -45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 65, 75, 85}}
};
map<int, map<int, vector<int>>> Iono_Bands = {
    {0,
     {{0, {  1,  28, -180, 3}},
      {1, { 29,  51, -175, 0}},
      {2, { 52,  78, -170, 1}},
      {3, { 79, 101, -165, 0}},
      {4, {102, 128, -160, 1}},
      {5, {129, 151, -155, 0}},
      {6, {152, 178, -150, 1}},
      {7, {179, 201, -145, 0}}}},
    {1,
     {{0, {  1,  28, -140, 2}},
      {1, { 29,  51, -135, 0}},
      {2, { 52,  78, -130, 1}},
      {3, { 79, 101, -125, 0}},
      {4, {102, 128, -120, 1}},
      {5, {129, 151, -115, 0}},
      {6, {152, 178, -110, 1}},
      {7, {179, 201, -105, 0}}}},
    {2,
     {{0, {  1,  27, -100, 1}},
      {1, { 28,  50,  -95, 0}},
      {2, { 51,  78,  -90, 3}},
      {3, { 79, 101,  -85, 0}},
      {4, {102, 128,  -80, 1}},
      {5, {129, 151,  -75, 0}},
      {6, {152, 178,  -70, 1}},
      {7, {179, 201,  -65, 0}}}},
    {3,
     {{0, {  1,  27, -60, 1}},
      {1, { 28,  50, -55, 0}},
      {2, { 51,  78, -50, 2}},
      {3, { 79, 101, -45, 0}},
      {4, {102, 128, -40, 1}},
      {5, {129, 151, -35, 0}},
      {6, {152, 178, -30, 1}},
      {7, {179, 201, -25, 0}}}},
    {4,
     {{0, {  1,  27, -20, 1}},
      {1, { 28,  50, -15, 0}},
      {2, { 51,  77, -10, 1}},
      {3, { 78, 100,  -5, 0}},
      {4, {101, 128,   0, 3}},
      {5, {129, 151,   5, 0}},
      {6, {152, 178,  10, 1}},
      {7, {179, 201,  15, 0}}}},
    {5,
     {{0, {  1,  27, 20, 1}},
      {1, { 28,  50, 25, 0}},
      {2, { 51,  77, 30, 1}},
      {3, { 78, 100, 35, 0}},
      {4, {101, 128, 40, 2}},
      {5, {129, 151, 45, 0}},
      {6, {152, 178, 50, 1}},
      {7, {179, 201, 55, 0}}}},
    {6,
     {{0, {  1,  27, 60, 1}},
      {1, { 28,  50, 65, 0}},
      {2, { 51,  77, 70, 1}},
      {3, { 78, 100, 75, 0}},
      {4, {101, 127, 80, 1}},
      {5, {128, 150, 85, 0}},
      {6, {151, 178, 90, 3}},
      {7, {179, 201, 95, 0}}}},
    {7,
     {{0, {  1,  27, 100, 1}},
      {1, { 28,  50, 105, 0}},
      {2, { 51,  77, 110, 1}},
      {3, { 78, 100, 115, 0}},
      {4, {101, 127, 120, 1}},
      {5, {128, 150, 125, 0}},
      {6, {151, 178, 130, 2}},
      {7, {179, 201, 135, 0}}}},
    {8,
     {{0, {  1,  27, 140, 1}},
      {1, { 28,  50, 145, 0}},
      {2, { 51,  77, 150, 1}},
      {3, { 78, 100, 155, 0}},
      {4, {101, 127, 160, 1}},
      {5, {128, 150, 165, 0}},
      {6, {151, 177, 170, 1}},
      {7, {178, 200, 175, 0}}}},
    {9,
     {{0, {  1,  72,  60, -180,  5, 170}},
      {1, { 73, 108,  65, -180, 10, 170}},
      {2, {143, 144,  70, -180, 10, 170}},
      {3, {179, 180,  75, -180, 10, 170}},
      {4, {181, 192,  85, -180, 30, 150}}}},
    {10,
     {{0, {  1,  72, -60, -180,  5, 170}},
      {1, { 73, 108, -65, -180, 10, 170}},
      {2, {143, 144, -70, -180, 10, 170}},
      {3, {179, 180, -75, -180, 10, 170}},
      {4, {181, 192, -85, -170, 30, 160}}}}
};

double giveTable[16] = {
    0.0084,
    0.0333,
    0.0749,
    0.1331,
    0.2079,
    0.2994,
    0.4075,
    0.5322,
    0.6735,
    0.8315,
    1.1974,
    1.8709,
    3.3260,
    20.7870,
    187.0826,
    -1
};

struct ionoGridPoint
{
    int    IODI    = -1;
    GTime  IGPTime = GTime::noTime();
    GTime  GIVTime = GTime::noTime();
    int    lat;
    int    lon;
    double GIVD = 0;
    double GIVE = 9999999;
};

struct GridMap
{
    map<int, map<int, int>> IonoGridLati;  // IGP Latitude indexed by Band and entry number
    map<int, map<int, int>> IonoGridLong;
    bool                    complete = false;
};

map<int, GridMap>                 ionoGridMap;
map<int, map<int, ionoGridPoint>> IonoGridData;  // IGP data indexed by latitude, longitude
bool                              incBand9_10 = false;

bool addSBASIGP(Trace& trace, int IODI, int band, int ID, int entry, GTime tof, int nband)
{
    if (ID < 1)
        return false;
    if (band < 0 || band > 10)
        return false;
    auto& bandData = Iono_Bands[band];
    int   subBand  = -1;
    for (auto& [sub, Data] : bandData)
        if (Data[1] >= ID)
        {
            subBand = sub;
            break;
        }
    if (subBand < 0)
        return false;

    auto& blockData = bandData[subBand];
    if (band == 9 || band == 10)
    {
        incBand9_10 = true;
        int lat     = blockData[2];
        int lon     = blockData[3] + blockData[4] * (ID - blockData[0]);
        ionoGridMap[IODI].IonoGridLati[band][entry] = lat;
        ionoGridMap[IODI].IonoGridLong[band][entry] = lon;
        IonoGridData[lat][lon].IODI                 = IODI;
        IonoGridData[lat][lon].IGPTime              = tof;
        if (ionoGridMap[IODI].IonoGridLati.size() == nband)
            ionoGridMap[IODI].complete = true;
        tracepdeex(
            IONO_DEBUG_TRACE_LEVEL,
            trace,
            "%s IGP data for IGP[%2d][%3d]: lat=%3d; lon=%4d; ID=%3d; nband=%d\n",
            tof.to_string(),
            band,
            entry,
            lat,
            lon,
            ID,
            nband
        );
        return true;
    }

    int   lon                                   = blockData[2];
    auto& lasVec                                = latiVects[blockData[3]];
    int   lat                                   = lasVec[ID - blockData[0]];
    ionoGridMap[IODI].IonoGridLati[band][entry] = lat;
    ionoGridMap[IODI].IonoGridLong[band][entry] = lon;
    IonoGridData[lat][lon].IGPTime              = tof;
    IonoGridData[lat][lon].IODI                 = IODI;
    if (ionoGridMap[IODI].IonoGridLati.size() == nband)
    {
        ionoGridMap[IODI].complete = true;
        tracepdeex(IONO_DEBUG_TRACE_LEVEL, trace, "IGP map (%d) is comprete\n", IODI);
    }

    tracepdeex(
        IONO_DEBUG_TRACE_LEVEL,
        trace,
        "%s IGP data for IGP[%1d][%3d]: lat=%3d; lon=%4d; ID=%3d; nband=%d\n",
        tof.to_string(),
        band,
        entry,
        lat,
        lon,
        ID,
        nband
    );
    return true;
}

bool writeIonoData(Trace& trace, int IODI, int band, int entry, GTime tof, int GIVDI, int GIVEI)
{
    if (ionoGridMap.find(IODI) == ionoGridMap.end())
        return false;

    if (ionoGridMap[IODI].IonoGridLati.find(band) == ionoGridMap[IODI].IonoGridLati.end())
        return false;

    if (ionoGridMap[IODI].IonoGridLati[band].find(entry) ==
        ionoGridMap[IODI].IonoGridLati[band].end())
        return false;

    int lat = ionoGridMap[IODI].IonoGridLati[band][entry];
    int lon = ionoGridMap[IODI].IonoGridLong[band][entry];

    IonoGridData[lat][lon].IODI    = IODI;
    IonoGridData[lat][lon].GIVTime = tof;
    IonoGridData[lat][lon].lat     = lat;
    IonoGridData[lat][lon].lon     = lon;
    IonoGridData[lat][lon].GIVD    = GIVDI * 0.125;
    if (GIVEI == 15)
        IonoGridData[lat][lon].GIVE = -1;
    else
        IonoGridData[lat][lon].GIVE = sqrt(giveTable[GIVEI]);

    tracepdeex(
        IONO_DEBUG_TRACE_LEVEL,
        trace,
        "%s VTEC data for IGP[%2d][%3d]: lat=%3d; lon=%4d; GIVD=%6.3f; GIVE=%7.5f;\n",
        tof.to_string(),
        band,
        entry,
        lat,
        lon,
        IonoGridData[lat][lon].GIVD,
        IonoGridData[lat][lon].GIVE
    );
    return true;
}

double ionppp(const VectorPos& pos, const AzEl& azel, double& ippLat, double& ippLon)
{
    double rp    = 0.94797965 * cos(azel.el);
    double ap    = PI / 2 - azel.el - asin(rp);
    double sinap = sin(ap);
    double tanap = tan(ap);
    double cosaz = cos(azel.az);
    double sinaz = sin(azel.az);
    ippLat       = asin(sin(pos.lat()) * cos(ap) + cos(pos.lat()) * sinap * cosaz);

    if ((pos.latDeg() > +70 && +tanap * cosaz > tan(PI / 2 - pos.lat())) ||
        (pos.latDeg() < -70 && -tanap * cosaz > tan(PI / 2 + pos.lat())))
    {
        ippLon = pos.lon() + PI - asin(sinap * sinaz / cos(ippLat));
    }
    else
    {
        ippLon = pos.lon() + asin(sinap * sinaz / cos(ippLat));
    }

    if ((ippLon) > 180 * D2R)
        ippLon -= 360 * D2R;

    return 1 / sqrt(1 - SQR(rp));
}

int selectIGP85(GTime t, double ippLat, vector<ionoGridPoint>& selIGPs)
{
    selIGPs.clear();
    int lat    = -85;
    int lon[4] = {-140, -50, 40, 130};
    if (ippLat > 0)
    {
        lat    = 85;
        lon[0] = -180;
        lon[1] = -90;
        lon[2] = 0;
        lon[3] = 90;
    }

    if (IonoGridData.find(lat) == IonoGridData.end())
        return 0;
    int iodi = -1;
    for (int i = 0; i < 4; i++)
    {
        if (IonoGridData[lat].find(lon[i]) == IonoGridData[lat].end())
            return 0;
        auto ionData = IonoGridData[lat][lon[i]];
        if (fabs((t - ionData.IGPTime).to_double()) > SBAS_IGP_OUTAGE)
            return 0;
        if (!ionoGridMap[ionData.IODI].complete)
            return 0;
        if (iodi < 0)
            iodi = ionData.IODI;
        if (ionData.IODI != iodi)
            return 0;
        selIGPs.push_back(ionData);
    }

    return selIGPs.size() == 4 ? 4 : 0;
}

int selectIGP75(GTime t, double ippLat, double ippLon, vector<ionoGridPoint>& selIGPs)
{
    selIGPs.clear();
    int lat[4] = {-75, -75, -85, -85};
    int spa    = 90;
    int off    = 40;
    if (incBand9_10)
        spa = 30;
    if (ippLat > 0)
    {
        off = 0;
        for (int i = 0; i < 4; i++)
            lat[i] *= -1;
    }

    double lonDeg = ippLon * R2D;
    int    lon[4];
    lon[0] = 10 * floor(lonDeg / 10);
    lon[1] = lon[0] + 10;
    lon[2] = spa * floor((lonDeg - off) / spa) + off;
    if (lon[2] < -180)
        lon[2] += 360;
    lon[3] = lon[2] + spa;
    if (lon[3] > 180)
        lon[3] -= 360;
    int                   iodi = -1;
    vector<ionoGridPoint> candIGPs;
    for (int i = 0; i < 4; i++)
    {
        if (IonoGridData.find(lat[i]) == IonoGridData.end())
            return 0;
        if (IonoGridData[lat[i]].find(lon[i]) == IonoGridData[lat[i]].end())
            return 0;
        auto ionData = IonoGridData[lat[i]][lon[i]];
        if (ionData.GIVE < 0)
            return 0;
        if (fabs((t - ionData.IGPTime).to_double()) > SBAS_IGP_OUTAGE)
            return 0;
        if (!ionoGridMap[ionData.IODI].complete)
            return 0;
        if (iodi < 0)
            iodi = ionData.IODI;
        if (ionData.IODI != iodi)
            return 0;
        candIGPs[i] = ionData;
    }

    selIGPs.push_back(candIGPs[0]);
    selIGPs.push_back(candIGPs[1]);

    ionoGridPoint intrpIGP = candIGPs[2];
    double        dLon     = lon[0] - lon[2];
    if (dLon < 0)
        dLon += 360;
    dLon /= spa;
    intrpIGP.lon  = lon[0];
    intrpIGP.GIVD = dLon * candIGPs[3].GIVD + (1 - dLon) * candIGPs[2].GIVD;
    intrpIGP.GIVE = dLon * candIGPs[3].GIVE + (1 - dLon) * candIGPs[2].GIVE;
    selIGPs.push_back(intrpIGP);

    intrpIGP = candIGPs[3];
    dLon     = lon[1] - lon[3];
    if (dLon < 0)
        dLon += 360;
    dLon /= spa;
    intrpIGP.lon  = lon[1];
    intrpIGP.GIVD = dLon * candIGPs[2].GIVD + (1 - dLon) * candIGPs[3].GIVD;
    intrpIGP.GIVE = dLon * candIGPs[2].GIVE + (1 - dLon) * candIGPs[3].GIVE;
    selIGPs.push_back(intrpIGP);

    return 4;
}

bool checkTriangular(double ippLat, double ippLon, vector<ionoGridPoint> selIGPs)
{
    double lat0 = selIGPs[0].lat;
    double dlat;
    if (selIGPs[0].lat == selIGPs[1].lat)
        dlat = selIGPs[2].lat - lat0;
    else if (selIGPs[0].lat == selIGPs[2].lat)
        dlat = selIGPs[1].lat - lat0;
    else
    {
        lat0 = selIGPs[1].lat;
        dlat = selIGPs[0].lat - lat0;
    }

    double lon0 = selIGPs[0].lon;
    double dlon;
    if (selIGPs[0].lon == selIGPs[1].lon)
        dlon = selIGPs[2].lon - lon0;
    else if (selIGPs[0].lon == selIGPs[2].lon)
        dlon = selIGPs[1].lon - lon0;
    else
    {
        lon0 = selIGPs[1].lon;
        dlon = selIGPs[0].lon - lon0;
    }

    if (dlon > 180)
        dlon -= 360;
    if (dlon < -180)
        dlon += 360;

    double dlati = ippLat * R2D - lat0;
    double dloni = ippLon * R2D - lon0;

    if (dloni > 180)
        dloni -= 360;
    if (dloni < -180)
        dloni += 360;
    return (dlati / dlat + dloni / dlon) <= 1;
}

int selectIGP60(GTime t, double ippLat, double ippLon, vector<ionoGridPoint>& selIGPs)
{
    selIGPs.clear();
    int lat0 = 5 * floor(ippLat * R2D / 5);
    int lon0 = 10 * floor(ippLon * R2D / 10);

    int iodi = -1;
    for (int i = 0; i < 2; i++)
    {
        int lat = lat0 + 5 * i;
        if (IonoGridData.find(lat) == IonoGridData.end())
            continue;
        for (int j = 0; j < 2; j++)
        {
            int lon = lon0 + 10 * j;
            if (lon >= 180)
                lon -= 360;
            if (IonoGridData[lat].find(lon) == IonoGridData[lat].end())
                continue;

            auto ionData = IonoGridData[lat][lon];
            if (fabs((t - ionData.IGPTime).to_double()) > SBAS_IGP_OUTAGE)
                continue;
            if (!ionoGridMap[ionData.IODI].complete)
                continue;
            if (iodi < 0)
                iodi = ionData.IODI;
            if (ionData.IODI != iodi)
                continue;
            selIGPs.push_back(ionData);
        }
    }

    if (selIGPs.empty())
        return 0;

    if (selIGPs.size() == 4)
        return 4;
    if (selIGPs.size() == 3 && checkTriangular(ippLat, ippLon, selIGPs))
        return 3;

    vector<ionoGridPoint> selIGPcopy;
    for (auto& igp : selIGPs)
        selIGPcopy.push_back(igp);

    for (auto& igp : selIGPcopy)
    {
        int lat1 = igp.lat == lat0 ? lat0 : lat0 - 5;
        int lon1 = igp.lon == lon0 ? lon0 : lon0 - 10;
        if (lon1 < -180)
            lon1 += 360;
        iodi = -1;
        vector<ionoGridPoint> candIGPs;
        for (int i = 0; i < 2; i++)
        {
            int lat = lat1 + 10 * i;
            if (IonoGridData.find(lat) == IonoGridData.end())
                continue;
            for (int j = 0; j < 2; j++)
            {
                int lon = lon1 + 10 * j;
                if (lon >= 180)
                    lon -= 360;
                if (IonoGridData[lat].find(lon) == IonoGridData[lat].end())
                    continue;

                auto ionData = IonoGridData[lat][lon];
                if (fabs((t - ionData.IGPTime).to_double()) > SBAS_IGP_OUTAGE)
                    continue;
                if (!ionoGridMap[ionData.IODI].complete)
                    continue;
                if (iodi < 0)
                    iodi = ionData.IODI;
                if (ionData.IODI != iodi)
                    continue;
                candIGPs.push_back(ionData);
            }
        }

        if (candIGPs.size() == 4)
        {
            selIGPs.clear();
            for (auto& cand : candIGPs)
                selIGPs.push_back(cand);
            return 4;
        }

        if (candIGPs.size() == 3 && checkTriangular(ippLat, ippLon, candIGPs))
        {
            selIGPs.clear();
            for (auto& cand : candIGPs)
                selIGPs.push_back(cand);
            return 3;
        }
    }

    return selIGPs.size();
}

int selectIGP00(GTime t, double ippLat, double ippLon, vector<ionoGridPoint>& selIGPs)
{
    selIGPs.clear();
    int lat0 = 5 * floor(ippLat * R2D / 5);
    int lon0 = 5 * floor(ippLon * R2D / 5);

    int iodi = -1;
    for (int i = 0; i < 2; i++)
    {
        int lat = lat0 + 5 * i;
        if (IonoGridData.find(lat) == IonoGridData.end())
            continue;
        for (int j = 0; j < 2; j++)
        {
            int lon = lon0 + 5 * j;
            if (lon >= 180)
                lon -= 360;
            if (IonoGridData[lat].find(lon) == IonoGridData[lat].end())
                continue;

            auto ionData = IonoGridData[lat][lon];
            if (fabs((t - ionData.IGPTime).to_double()) > SBAS_IGP_OUTAGE)
                continue;
            if (!ionoGridMap[ionData.IODI].complete)
                continue;
            if (iodi < 0)
                iodi = ionData.IODI;
            if (ionData.IODI != iodi)
                continue;
            selIGPs.push_back(ionData);
        }
    }

    if (selIGPs.empty())
        return 0;

    if (selIGPs.size() == 4)
        return 4;

    if (selIGPs.size() == 3 && checkTriangular(ippLat, ippLon, selIGPs))
        return 3;

    vector<ionoGridPoint> selIGPcopy;
    for (auto& igp : selIGPs)
        selIGPcopy.push_back(igp);

    for (auto& igp : selIGPcopy)
    {
        int lat1 = igp.lat == lat0 ? lat0 : lat0 - 5;
        int lon1 = igp.lon == lon0 ? lon0 : lon0 - 5;
        if (lon1 < -180)
            lon1 += 360;
        iodi = -1;
        vector<ionoGridPoint> candIGPs;
        for (int i = 0; i < 2; i++)
        {
            int lat = lat1 + 10 * i;
            if (IonoGridData.find(lat) == IonoGridData.end())
                continue;
            for (int j = 0; j < 2; j++)
            {
                int lon = lon1 + 10 * j;
                if (lon >= 180)
                    lon -= 360;
                if (IonoGridData[lat].find(lon) == IonoGridData[lat].end())
                    continue;

                auto ionData = IonoGridData[lat][lon];
                if (fabs((t - ionData.IGPTime).to_double()) > SBAS_IGP_OUTAGE)
                    continue;
                if (!ionoGridMap[ionData.IODI].complete)
                    continue;
                if (iodi < 0)
                    iodi = ionData.IODI;
                if (ionData.IODI != iodi)
                    continue;
                candIGPs.push_back(ionData);
            }
        }

        if (candIGPs.size() == 4)
        {
            selIGPs.clear();
            for (auto& cand : candIGPs)
                selIGPs.push_back(cand);
            return 4;
        }

        if (candIGPs.size() == 3 && checkTriangular(ippLat, ippLon, candIGPs))
        {
            selIGPs.clear();
            for (auto& cand : candIGPs)
                selIGPs.push_back(cand);
            return 3;
        }
    }

    return selIGPs.size();
}

int selectIGPs(GTime t, double ippLat, double ippLon, vector<ionoGridPoint>& selIGPs)
{
    if (abs(ippLat) > 85 * D2R)
        return selectIGP85(t, ippLat, selIGPs);
    else if (abs(ippLat) > 75 * D2R)
        return selectIGP75(t, ippLat, ippLon, selIGPs);
    else if (abs(ippLat) > 60 * D2R)
        return selectIGP60(t, ippLat, ippLon, selIGPs);

    return selectIGP00(t, ippLat, ippLon, selIGPs);
}

double
iono3IGP(GTime t, double ippLat, double ippLon, vector<ionoGridPoint> selIGPs, double& ionVar)
{
    ionVar = -1;
    if (!checkTriangular(ippLat, ippLon, selIGPs))
        return 0.0;
    if (fabs(ippLat * R2D) > 75)
        return 0.0;

    int remap[3];
    if (selIGPs[0].lat == selIGPs[1].lat && selIGPs[0].lon == selIGPs[2].lon)
    {
        remap[0] = 1;
        remap[1] = 0;
        remap[2] = 2;
    }
    if (selIGPs[0].lat == selIGPs[2].lat && selIGPs[0].lon == selIGPs[1].lon)
    {
        remap[0] = 2;
        remap[1] = 0;
        remap[2] = 1;
    }
    if (selIGPs[1].lat == selIGPs[0].lat && selIGPs[1].lon == selIGPs[2].lon)
    {
        remap[0] = 0;
        remap[1] = 1;
        remap[2] = 2;
    }
    if (selIGPs[1].lat == selIGPs[2].lat && selIGPs[1].lon == selIGPs[0].lon)
    {
        remap[0] = 2;
        remap[1] = 1;
        remap[2] = 0;
    }
    if (selIGPs[2].lat == selIGPs[0].lat && selIGPs[2].lon == selIGPs[1].lon)
    {
        remap[0] = 0;
        remap[1] = 2;
        remap[2] = 1;
    }
    if (selIGPs[2].lat == selIGPs[1].lat && selIGPs[2].lon == selIGPs[0].lon)
    {
        remap[0] = 1;
        remap[1] = 2;
        remap[2] = 0;
    }

    double latRng = selIGPs[remap[1]].lat - selIGPs[remap[2]].lat;
    double dLat   = fabs((ippLat * R2D - selIGPs[1].lat) / latRng);

    double lonRng = fabs(selIGPs[remap[0]].lon - selIGPs[remap[1]].lon);
    if (lonRng > 180)
        lonRng = 360 - lonRng;
    double dLon = fabs((ippLon * R2D - selIGPs[1].lon));
    if (dLon > 180)
        dLon = 360 - dLon;
    dLon /= lonRng;

    double Wi[3];
    Wi[remap[0]] = dLat;
    Wi[remap[1]] = 1 - dLat - dLon;
    Wi[remap[2]] = dLon;

    double iono = 0;
    ionVar      = 0;
    for (int i = 0; i < 3; i++)
    {
        double varIono = estimateIonoVar(t, selIGPs[i].GIVTime, selIGPs[i].GIVE);
        if (varIono < 0)
        {
            ionVar = -1;
            return 0.0;
        }

        iono += Wi[i] * selIGPs[i].GIVD;
        ionVar += Wi[i] * varIono;
    }

    return iono;
}

double
iono4IGP(GTime t, double ippLat, double ippLon, vector<ionoGridPoint> selIGPs, double& ionVar)
{
    ionVar = -1;
    if (fabs(ippLat * R2D) > 85)
    {
        double        dlat = (fabs(ippLat * R2D) - 85) / 10;
        double        dlon = 0;
        map<int, int> remap;
        for (int i = 0; i < 4; i++)
        {
            double dif = ippLon * R2D - selIGPs[i].lon;
            if (dif < -180)
                dif += 360;
            if (dif >= 90)
                remap[i] = 2;
            else if (dif >= 0)
            {
                remap[i] = 3;
                dlon     = dif * (1 - 2 * dlat) / 90 + dlat;
            }
            else if (dif >= -90)
                remap[i] = 4;
            else
                remap[i] = 1;
        }
        double iono = 0;
        ionVar      = 0;
        for (int i = 0; i < 4; i++)
        {
            double Wi      = 0;
            double varIono = estimateIonoVar(t, selIGPs[i].GIVTime, selIGPs[i].GIVE);
            if (varIono < 0)
            {
                ionVar = -1;
                return 0.0;
            }
            switch (remap[i])
            {
                case 1:
                    Wi = dlat * dlon;
                    break;
                case 2:
                    Wi = (1 - dlat) * dlon;
                    break;
                case 3:
                    Wi = (1 - dlat) * (1 - dlon);
                    break;
                case 4:
                    Wi = dlat * (1 - dlon);
                    break;
            }
            iono += Wi * selIGPs[i].GIVD;

            ionVar += Wi * varIono;
        }

        return iono;
    }

    double latRng = fabs(selIGPs[3].lat - selIGPs[0].lat);
    if (latRng == 0)
        latRng = fabs(selIGPs[1].lat - selIGPs[0].lat);

    double lonRng = fabs(selIGPs[3].lon - selIGPs[0].lon);
    if (lonRng == 0)
        lonRng = fabs(selIGPs[1].lon - selIGPs[0].lon);

    if (lonRng > 180)
        lonRng = 360 - lonRng;

    double iono = 0;
    ionVar      = 0;
    for (int i = 0; i < 4; i++)
    {
        double varIono = estimateIonoVar(t, selIGPs[i].GIVTime, selIGPs[i].GIVE);
        if (varIono < 0)
        {
            selIGPs.erase(selIGPs.begin() + i);
            ionVar = -1;
            return iono3IGP(t, ippLat, ippLon, selIGPs, ionVar);
        }
        double dlat = fabs(ippLat * R2D - selIGPs[i].lat) / latRng;
        double dlon = fabs(ippLon * R2D - selIGPs[i].lon);
        if (dlon > 180)
            dlon = 360 - dlon;
        dlon /= lonRng;
        iono += dlat * dlon * selIGPs[i].GIVD;
        ionVar += dlat * dlon * varIono;
    }

    return iono;
}

double ionmodelSBAS(GTime t, const VectorPos& pos, const AzEl& azel, double& ionVar)
{
    double ippLat;
    double ippLon;
    double mapf = ionppp(pos, azel, ippLat, ippLon);

    ionVar = -1;
    vector<ionoGridPoint> selIGPs;
    int                   nIGP = selectIGPs(t, ippLat, ippLon, selIGPs);

    if (nIGP < 3)
        return 0.0;

    vector<ionoGridPoint> goodIGPs;
    for (auto ionData : selIGPs)
    {
        if (fabs((t - ionData.GIVTime).to_double()) > SBAS_GIV_OUTAGE)
            continue;
        if (ionData.GIVE < 0)
            continue;
        goodIGPs.push_back(ionData);
    }

    double iono = 0;
    switch (goodIGPs.size())
    {
        case 4:
            iono = iono4IGP(t, ippLat, ippLon, goodIGPs, ionVar);
            break;
        case 3:
            iono = iono3IGP(t, ippLat, ippLon, goodIGPs, ionVar);
            break;
        default:
            return 0.0;
    }

    if (ionVar < 0)
        return 0.0;

    ionVar *= mapf * mapf;
    return iono * mapf;
}