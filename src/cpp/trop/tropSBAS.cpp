#include "tropModels.hpp"

/** get meterological parameters
 */
void getmet(double lat, double* met)
{
    const double metprm[][10] = /* lat=15,30,45,60,75 */
        {{1013.25, 299.65, 26.31, 6.30E-3, 2.77, 0.00, 0.00, 0.00, 0.00E-3, 0.00},
         {1017.25, 294.15, 21.79, 6.05E-3, 3.15, -3.75, 7.00, 8.85, 0.25E-3, 0.33},
         {1015.75, 283.15, 11.66, 5.58E-3, 2.57, -2.25, 11.00, 7.24, 0.32E-3, 0.46},
         {1011.75, 272.15, 6.78, 5.39E-3, 1.81, -1.75, 15.00, 5.36, 0.81E-3, 0.74},
         {1013.00, 263.65, 4.11, 4.53E-3, 1.55, -0.50, 14.50, 3.39, 0.62E-3, 0.30}};

    lat = fabs(lat);

    if (lat <= 15)
        for (int i = 0; i < 10; i++)
            met[i] = metprm[0][i];
    else if (lat >= 75)
        for (int i = 0; i < 10; i++)
            met[i] = metprm[4][i];
    else
    {
        int    j = (int)(lat / 15);
        double a = (lat - j * 15) / 15.0;

        for (int i = 0; i < 10; i++)
        {
            met[i] = (1 - a) * metprm[j - 1][i] + a * metprm[j][i];
        }
    }
}

/* tropospheric delay correction -----------------------------------------------
 * compute sbas tropospheric delay correction (mops model)
 *-----------------------------------------------------------------------------*/
double tropSBAS(
    Trace&     trace,
    GTime      time,
    VectorPos& pos,
    double     elev,
    double&    dryZTD,
    double&    dryMap,
    double&    wetZTD,
    double&    wetMap,
    double&    var
)
{
    const double k1 = 77.604;
    const double k2 = 382000;
    const double rd = 287.054;
    const double gm = 9.784;
    const double g  = 9.80665;

    if (pos.hgt() < -100 || pos.hgt() > +10000 || elev <= 0)
    {
        dryMap = 0;
        wetMap = 0;
        dryMap = 0;
        wetMap = 0;
        var    = 0;
        return 0;
    }

    double met[10];
    getmet(pos.latDeg(), met);

    UYds   yds = time;
    double c   = cos(2 * PI * (yds.doy - (pos.lat() >= 0 ? 28 : 211)) / 365.25);
    for (int i = 0; i < 5; i++)
    {
        met[i] -= met[i + 5] * c;
    }
    dryZTD = 1E-6 * k1 * rd * met[0] / gm;
    wetZTD = 1E-6 * k2 * rd * met[2] / (gm * (met[4] + 1) - met[3] * rd) / met[1];

    double h = pos.hgt();
    dryZTD *= pow(1 - met[3] * h / met[1], g / (rd * met[3]));
    wetZTD *= pow(1 - met[3] * h / met[1], (met[4] + 1) * g / (rd * met[3]) - 1);

    double sinel   = sin(elev);
    dryMap         = 1.001 / sqrt(0.002001 + sinel * sinel);
    double elevDeg = elev * R2D;
    if (elevDeg < 4)
        dryMap *= 1.0 + 0.015 * SQR(4 - elevDeg);
    wetMap = dryMap;
    var    = SQR(0.12 * dryMap);
    return (dryZTD + wetZTD) * dryMap;
}
